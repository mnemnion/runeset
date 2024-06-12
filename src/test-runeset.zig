//! RuneSet test suite
//!
//! The intention is to provide 100% code coverage to the machine-instruction
//! level.

const std = @import("std");
const testing = std.testing;
const Allocator = std.mem.Allocator;

pub const elements = @import("elements.zig");
pub const runeset = @import("runeset.zig");

pub const data = @import("test-data.zig");

const RuneSet = runeset.RuneSet;
const codeunit = elements.codeunit;

const expect = std.testing.expect;
const expectEqual = testing.expectEqual;

//| Test data type(s)

/// LRstrings
///
/// A string split into canonical left and right portions.
///
/// To be well-formed, `str` must have all runes in `l` and `r`, which
/// must not themselves share any runes in common.
const LRstrings = data.LRstrings;

//| Test Functions

/// This confirms that none of the runes in `str` match in `set`.
///
/// Allows invalid UTF-8.
fn testMatchNone(set: RuneSet, str: []const u8) !void {
    var idx: usize = 0;
    while (idx < str.len) {
        const slice = str[idx..];
        const nB = codeunit(slice[0]).nBytes() orelse 1;
        try expectEqual(0, set.matchOne(slice));
        idx += nB;
    }
    // second pass to assure invalid follow bytes are handled safely
    while (idx < str.len) {
        const slice = str[idx..];
        const match = set.matchOne(slice);
        if (match) |m| {
            try expectEqual(0, m);
        }
        idx += 1;
    }
}

/// Build a RuneSet from `str`, verifying the following properties:
///
/// - The set matches all codepoints in str
/// - The count of codeunits in str matches that in set
///     - NOTE: codeunits in str must be unique!
/// - The union of set with itself is identical to original set
/// - The difference of set with itself is empty
/// - TODO the intersection of set with itself is identical
///
/// Invalid UTF-8 is safe, but the test will fail.
fn withStringVerifySetProperties(str: []const u8, alloc: Allocator) !void {
    const set = try RuneSet.createFromConstString(str, alloc);
    defer set.deinit(alloc);
    try verifySetProperties(str, set, alloc);
}

/// Verify basic set properties of a set created from all strings in a slice thereof.
fn withSliceVerifySetProperties(strs: []const []const u8, alloc: Allocator) !void {
    const set = try RuneSet.createFromConstStringSlice(strs, alloc);
    defer set.deinit(alloc);
    const str = try std.mem.concat(alloc, u8, strs);
    defer alloc.free(str);
    try verifySetProperties(str, set, alloc);
}

fn verifySetProperties(str: []const u8, set: RuneSet, alloc: Allocator) !void {
    const matched = set.matchMany(str);
    if (matched) |m| {
        try expectEqual(str.len, m);
        try expectEqual(str.len, set.codeunitCount());
    } else try expect(false);
    const matched_v = set.matchManyAssumeValid(str);
    try expectEqual(str.len, matched_v);
    const setU = try set.setUnion(set, alloc);
    defer setU.deinit(alloc);
    try expect(setU.equalTo(set));
    const setD = try set.setDifference(set, alloc);
    defer setD.deinit(alloc);
    try expectEqual(0, setD.codeunitCount());
    const setI = try set.setIntersection(set, alloc);
    defer setI.deinit(alloc);
    try expect(setI.equalTo(set));
    try expect(setU.equalTo(setI));
}

/// Verify basic set properties of an LRstrings data sample.
/// This also confirms that the data itself has the required properties
/// to test union, difference, and intersection.
fn withLRstringsVerifySetProperties(s: LRstrings, alloc: Allocator) !void {
    const setL = try RuneSet.createFromConstString(s.l, alloc);
    defer setL.deinit(alloc);
    const setR = try RuneSet.createFromConstString(s.r, alloc);
    defer setR.deinit(alloc);
    const setAll = try RuneSet.createFromConstString(s.str, alloc);
    defer setAll.deinit(alloc);
    try verifySetProperties(s.l, setL, alloc);
    try verifySetProperties(s.r, setR, alloc);
    try verifySetProperties(s.str, setAll, alloc);
    try expectEqual(s.l.len, setAll.matchManyAssumeValid(s.l));
    try expectEqual(s.r.len, setAll.matchManyAssumeValid(s.r));
    try testMatchNone(setL, s.r);
    try testMatchNone(setR, s.l);
}

/// Validate union properties of an LRstring set:
///
/// - The union of the `l` set and the `r` set matches both `l` and `r`
/// - The union of sets matches `str`
/// - A set of `str` is equal to the union of `l` and `r`
///
fn verifyLRSetUnion(s: LRstrings, alloc: Allocator) !void {
    const str = s.str;
    const l = s.l;
    const r = s.r;
    try verifySetUnion(str, l, r, alloc);
}

fn verifySetUnion(str: []const u8, l: []const u8, r: []const u8, alloc: Allocator) !void {
    const setL = try RuneSet.createFromConstString(l, alloc);
    defer setL.deinit(alloc);
    const setR = try RuneSet.createFromConstString(r, alloc);
    defer setR.deinit(alloc);
    const setU = try setL.setUnion(setR, alloc);
    defer setU.deinit(alloc);
    const setAll = try RuneSet.createFromConstString(str, alloc);
    defer setAll.deinit(alloc);
    try expect(setAll.expectEqualTo(setU));
    try expectEqual(setAll.codeunitCount(), setU.codeunitCount());
    const matchL = setU.matchMany(l);
    if (matchL) |m| {
        try expectEqual(l.len, m);
    } else try expect(false);
    const matchR = setU.matchMany(r);
    if (matchR) |m| {
        try expectEqual(r.len, m);
    } else try expect(false);
    const matchAll = setU.matchMany(str);
    if (matchAll) |m| {
        try expectEqual(str.len, m);
    } else try expect(false);
}

fn verifyLRSetDifference(LR: LRstrings, alloc: Allocator) !void {
    const r = LR.r;
    const l = LR.l;
    const str = LR.str;
    try verifySetDifference(str, l, r, alloc);
}

/// Verify correct set difference of LR string:
///
/// - The diff of set of `str`:
///     - With set of `l` matches all of `r`
///     - With set of `r` matches all of `l`
///     - With set of `l` matches none of `l`
///     - With set of `r` matches none of `r`
/// - The diff of set of `str` with itself is the empty set ∅
///
fn verifySetDifference(str: []const u8, l: []const u8, r: []const u8, alloc: Allocator) !void {
    const setAll = try RuneSet.createFromConstString(str, alloc);
    defer setAll.deinit(alloc);
    const setR = try RuneSet.createFromConstString(r, alloc);
    defer setR.deinit(alloc);
    const setL = try RuneSet.createFromConstString(l, alloc);
    defer setL.deinit(alloc);
    const setAdiffR = try setAll.setDifference(setR, alloc);
    defer setAdiffR.deinit(alloc);
    const setAdiffL = try setAll.setDifference(setL, alloc);
    defer setAdiffL.deinit(alloc);
    try expect(setR.expectEqualTo(setAdiffL));
    try expect(setL.expectEqualTo(setAdiffR));
    const matchL = setAdiffR.matchMany(l);
    if (matchL) |nMatch| {
        try expectEqual(l.len, nMatch);
    } else try expect(false);
    const matchR = setAdiffL.matchMany(r);
    if (matchR) |nMatch| {
        try expectEqual(r.len, nMatch);
    } else try expect(false);
    try testMatchNone(setAdiffL, l);
    try testMatchNone(setAdiffR, r);
    const setNone = try setAll.setDifference(setAll, alloc);
    defer setNone.deinit(alloc);
    try expectEqual(0, setNone.codeunitCount());
    try expectEqual(4, setNone.body.len);
}

fn verifyLRSetIntersection(LR: LRstrings, alloc: Allocator) !void {
    const str = LR.str;
    const l = LR.l;
    const r = LR.r;
    try verifySetIntersection(str, l, r, alloc);
}

fn verifySetIntersection(str: []const u8, l: []const u8, r: []const u8, alloc: Allocator) !void {
    const setAll = try RuneSet.createFromConstString(str, alloc);
    defer setAll.deinit(alloc);
    const setR = try RuneSet.createFromConstString(r, alloc);
    defer setR.deinit(alloc);
    const setL = try RuneSet.createFromConstString(l, alloc);
    defer setL.deinit(alloc);
    const setAllandR = try setAll.setIntersection(setR, alloc);
    defer setAllandR.deinit(alloc);
    try expect(setAllandR.expectEqualTo(setR));
    const matchR = setAllandR.matchMany(r);
    if (matchR) |nMatch| {
        try expectEqual(r.len, nMatch);
    } else try expect(false);
    const setAllandL = try setAll.setIntersection(setL, alloc);
    defer setAllandL.deinit(alloc);
    try expect(setAllandL.expectEqualTo(setL));
    const matchL = setAllandL.matchMany(l);
    if (matchL) |nMatch2| {
        try expectEqual(l.len, nMatch2);
    } else try expect(false);
    const setNoneL = try setAllandL.setIntersection(setAllandR, alloc);
    defer setNoneL.deinit(alloc);
    try expectEqual(0, setNoneL.codeunitCount());
    try expectEqual(4, setNoneL.body.len);
    const setNoneR = try setAllandR.setIntersection(setAllandL, alloc);
    defer setNoneR.deinit(alloc);
    try expectEqual(0, setNoneR.codeunitCount());
    try expectEqual(4, setNoneR.body.len);
    try expect(setNoneL.equalTo(setNoneR));
}

fn verifySetsOfTwoLRstrings(L: LRstrings, R: LRstrings, alloc: Allocator) !void {
    const str = try std.mem.concat(alloc, u8, &.{ L.str, R.str });
    defer alloc.free(str);
    const l = try std.mem.concat(alloc, u8, &.{ L.l, R.l });
    defer alloc.free(l);
    const r = try std.mem.concat(alloc, u8, &.{ L.r, R.r });
    defer alloc.free(r);
    // try as combined structure
    try verifySetUnion(str, l, r, alloc);
    try verifySetDifference(str, l, r, alloc);
    try verifySetDifference(str, r, l, alloc);
    try verifySetIntersection(str, l, r, alloc);
    // try as separated structure
    try verifySetUnion(str, L.str, R.str, alloc);
    try verifySetDifference(str, L.str, R.str, alloc);
    try verifySetDifference(str, R.str, L.str, alloc);
    try verifySetIntersection(str, L.str, R.str, alloc);
}

//| Test Suite

test "set properties" {
    const allocator = std.testing.allocator;
    try withStringVerifySetProperties(ascii.str, allocator);
    try withStringVerifySetProperties(greek.str, allocator);
    try withStringVerifySetProperties(math.str, allocator);
    try withStringVerifySetProperties(linear_B.str, allocator);
    try withStringVerifySetProperties(han_sample.str, allocator);
    try withStringVerifySetProperties(deseret.str, allocator);
    try withStringVerifySetProperties(two_byte_feather.str, allocator);
    try withStringVerifySetProperties(cjk_feather.str, allocator);
    try withStringVerifySetProperties(cjk_chunk.str, allocator);
    try withStringVerifySetProperties(pua_A_chunk.str, allocator);
    try withStringVerifySetProperties(pua_A_feather.str, allocator);
    try withStringVerifySetProperties(smp_chunk.str, allocator);
    try withStringVerifySetProperties(smp_scatter.str, allocator);
}

test "LRstring set properties" {
    const allocator = std.testing.allocator;
    try withLRstringsVerifySetProperties(ascii, allocator);
    try withLRstringsVerifySetProperties(greek, allocator);
    try withLRstringsVerifySetProperties(math, allocator);
    try withLRstringsVerifySetProperties(linear_B, allocator);
    try withLRstringsVerifySetProperties(han_sample, allocator);
    try withLRstringsVerifySetProperties(deseret, allocator);
    try withLRstringsVerifySetProperties(two_byte_feather, allocator);
    try withLRstringsVerifySetProperties(cjk_feather, allocator);
    try withLRstringsVerifySetProperties(cjk_chunk, allocator);
    try withLRstringsVerifySetProperties(cjk_chunk4k, allocator);
    try withLRstringsVerifySetProperties(cjk_scatter, allocator);
    try withLRstringsVerifySetProperties(pua_A_chunk, allocator);
    try withLRstringsVerifySetProperties(pua_A_feather, allocator);
    try withLRstringsVerifySetProperties(smp_chunk, allocator);
    try withLRstringsVerifySetProperties(smp_scatter, allocator);
    try withLRstringsVerifySetProperties(tangut_chunk, allocator);
    try withLRstringsVerifySetProperties(tangut_widechunk, allocator);
    try withLRstringsVerifySetProperties(tangut_scatter, allocator);
    try withLRstringsVerifySetProperties(khitan_widechunk, allocator);
}

test "set from slice properties" {
    const allocator = std.testing.allocator;
    const strs = .{ ascii.str, greek.str, math.str, linear_B.str, deseret.str, cjk_chunk.str };
    try withSliceVerifySetProperties(&strs, allocator);
}

test "set properties of combined sets" {
    const allocator = std.testing.allocator;
    try verifySetsOfTwoLRstrings(greek, math, allocator);
    try verifySetsOfTwoLRstrings(deseret, greek, allocator);
    try verifySetsOfTwoLRstrings(deseret, khitan_widechunk, allocator);
    try verifySetsOfTwoLRstrings(greek, deseret, allocator);
    try verifySetsOfTwoLRstrings(greek, cjk_scatter, allocator);
    try verifySetsOfTwoLRstrings(two_byte_chunk, khitan_widechunk, allocator);
    try verifySetsOfTwoLRstrings(cjk_feather, khitan_widechunk, allocator);
    try verifySetsOfTwoLRstrings(two_byte_feather, tangut_widechunk, allocator);
    try verifySetsOfTwoLRstrings(ascii, deseret, allocator);
    try verifySetsOfTwoLRstrings(cjk_scatter, math, allocator);
    try verifySetsOfTwoLRstrings(math, cjk_chunk4k, allocator);
    try verifySetsOfTwoLRstrings(khitan_widechunk, tangut_widechunk, allocator);
    try verifySetsOfTwoLRstrings(smp_chunk, pua_A_feather, allocator);
    try verifySetsOfTwoLRstrings(pua_A_chunk, smp_chunk, allocator);
}

test "set union tests" {
    const allocator = std.testing.allocator;
    try verifyLRSetUnion(ascii, allocator);
    try verifyLRSetUnion(greek, allocator);
    try verifyLRSetUnion(math, allocator);
    try verifyLRSetUnion(linear_B, allocator);
    try verifyLRSetUnion(han_sample, allocator);
    try verifyLRSetUnion(deseret, allocator);
    try verifyLRSetUnion(two_byte_feather, allocator);
    try verifyLRSetUnion(two_byte_chunk, allocator);
    try verifyLRSetUnion(cjk_feather, allocator);
    try verifyLRSetUnion(cjk_chunk, allocator);
    try verifyLRSetUnion(cjk_chunk4k, allocator);
    try verifyLRSetUnion(cjk_scatter, allocator);
    try verifyLRSetUnion(pua_A_feather, allocator);
    try verifyLRSetUnion(smp_chunk, allocator);
    try verifyLRSetUnion(tangut_chunk, allocator);
    try verifyLRSetUnion(tangut_widechunk, allocator);
    try verifyLRSetUnion(tangut_scatter, allocator);
    try verifyLRSetUnion(khitan_widechunk, allocator);
    try verifyLRSetUnion(smp_scatter, allocator);
    try verifyLRSetUnion(pua_A_chunk, allocator);
}

test "set difference tests" {
    const allocator = std.testing.allocator;
    try verifyLRSetDifference(ascii, allocator);
    try verifyLRSetDifference(greek, allocator);
    try verifyLRSetDifference(math, allocator);
    try verifyLRSetDifference(linear_B, allocator);
    try verifyLRSetDifference(han_sample, allocator);
    try verifyLRSetDifference(deseret, allocator);
    try verifyLRSetDifference(two_byte_feather, allocator);
    try verifyLRSetDifference(two_byte_chunk, allocator);
    try verifyLRSetDifference(cjk_feather, allocator);
    try verifyLRSetDifference(cjk_chunk, allocator);
    try verifyLRSetDifference(cjk_chunk4k, allocator);
    try verifyLRSetDifference(cjk_scatter, allocator);
    try verifyLRSetDifference(pua_A_feather, allocator);
    try verifyLRSetDifference(pua_A_chunk, allocator);
    try verifyLRSetDifference(smp_chunk, allocator);
    try verifyLRSetDifference(smp_scatter, allocator);
    try verifyLRSetDifference(tangut_chunk, allocator);
    try verifyLRSetDifference(tangut_scatter, allocator);
    try verifyLRSetDifference(tangut_widechunk, allocator);
    try verifyLRSetDifference(khitan_widechunk, allocator);
}

test "set intersection tests" {
    const allocator = std.testing.allocator;
    try verifyLRSetIntersection(ascii, allocator);
    try verifyLRSetIntersection(greek, allocator);
    try verifyLRSetIntersection(math, allocator);
    try verifyLRSetIntersection(linear_B, allocator);
    try verifyLRSetIntersection(han_sample, allocator);
    try verifyLRSetIntersection(deseret, allocator);
    try verifyLRSetIntersection(two_byte_feather, allocator);
    try verifyLRSetIntersection(two_byte_chunk, allocator);
    try verifyLRSetIntersection(cjk_feather, allocator);
    try verifyLRSetIntersection(cjk_chunk, allocator);
    try verifyLRSetIntersection(cjk_chunk4k, allocator);
    try verifyLRSetIntersection(cjk_scatter, allocator);
    try verifyLRSetIntersection(pua_A_feather, allocator);
    try verifyLRSetIntersection(pua_A_chunk, allocator);
    try verifyLRSetIntersection(smp_chunk, allocator);
    try verifyLRSetIntersection(smp_scatter, allocator);
    try verifyLRSetIntersection(tangut_chunk, allocator);
    try verifyLRSetIntersection(tangut_scatter, allocator);
    try verifyLRSetIntersection(tangut_widechunk, allocator);
    try verifyLRSetIntersection(khitan_widechunk, allocator);
}

// Inline tests of runeset.zig and all tests of element.zig
test {
    std.testing.refAllDecls(@This());
}

//| Test Data
//|
//| An extensive collection of string data, meant to fully exercise the
//| functionality of the RuneSet type.

//| LRstrings

const ascii = data.ascii;
const greek = data.greek;
const math = data.math;
const linear_B = data.linear_B;
const deseret = data.deseret;
const han_sample = data.han_sample;
const two_byte_feather = data.two_byte_feather;
const two_byte_chunk = data.two_byte_chunk;
const cjk_feather = data.cjk_feather;
const cjk_chunk = data.cjk_chunk;
const cjk_chunk4k = data.cjk_chunk4k;
const cjk_scatter = data.cjk_scatter;
const pua_A_chunk = data.pua_A_chunk;
const pua_A_feather = data.pua_A_feather;
const smp_chunk = data.smp_chunk;
const smp_scatter = data.smp_scatter;
const tangut_chunk = data.tangut_chunk;
const tangut_scatter = data.tangut_scatter;
const tangut_widechunk = data.tangut_widechunk;
const khitan_widechunk = data.khitan_widechunk;

test "data integrity" {
    try std.testing.expectEqualStrings(pua_A_chunk.str, pua_A_feather.str);
}
