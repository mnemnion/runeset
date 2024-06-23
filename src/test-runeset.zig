//! RuneSet test suite
//!
//! The intention is to provide 100% code coverage to the machine-instruction
//! level.

const std = @import("std");
const config = @import("config");
const testing = std.testing;
const Allocator = std.mem.Allocator;

pub const elements = @import("elements.zig");
pub const runeset = @import("runeset.zig");

pub const data = @import("test-data.zig");

const RuneSet = runeset.RuneSet;
const Rune = runeset.Rune;
const codeunit = elements.codeunit;

const expect = std.testing.expect;
const expectEqual = testing.expectEqual;
const expectError = testing.expectError;

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
    idx = 0;
    while (idx < str.len) {
        const slice = str[idx..];
        const match = set.matchOne(slice);
        if (match) |m| {
            try expectEqual(0, m);
        }
        try expectEqual(0, set.matchOneAssumeValid(slice));
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
    const asString = try set.toString(alloc);
    defer alloc.free(asString);
    const matchedNew = set.matchMany(asString);
    if (matchedNew) |nB| {
        try expectEqual(asString.len, nB);
    } else try expect(false);
    // XXX try verifySetIteration(set);
}

fn verifySetIteration(set: RuneSet) !void {
    var setIter = set.iterateRunes();
    var lastRune = Rune.fromCodepoint('\u{0}') catch unreachable;
    while (setIter.next()) |rune| {
        if (lastRune.rawInt() == rune.rawInt()) {
            std.debug.print(
                "Saw {u} and {u}\n",
                .{ lastRune.toCodepoint() catch unreachable, rune.toCodepoint() catch unreachable },
            );
            try expect(false);
        } else {
            lastRune = rune;
        }
        const runeArray = rune.toByteArray();
        if (rune.byteCount() != set.matchOne(&runeArray).?) {
            const rune_point = rune.toCodepoint();
            if (rune_point) |r| {
                std.debug.print("rune {u} not a member of set\n", .{r});
                std.debug.print(
                    "rune bytes: {x} {x} {x} {x}\n",
                    .{ rune.a, rune.b, rune.c, rune.d },
                );
                std.debug.print(
                    "rune array bytes: {x} {x} {x} {x}\n",
                    .{
                        runeArray[0], runeArray[1], runeArray[2], runeArray[3],
                    },
                );
                std.debug.print("set length {d}, idx {d}\n", .{ set.body.len, setIter.idx });
                set.debugMaskAt(setIter.idx);
            } else |_| {
                std.debug.print(
                    "rune is invalid! {x} {x} {x} {x}\n",
                    .{ rune.a, rune.b, rune.c, rune.d },
                );
            }
            try expect(false);
        }
        // try expectEqual(rune.byteCount(), set.matchOne(&runeArray).?);
    }
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

fn verifyLRstringsData(s: LRstrings, alloc: Allocator) !void {
    const setL = try RuneSet.createFromConstString(s.l, alloc);
    defer setL.deinit(alloc);
    const setR = try RuneSet.createFromConstString(s.r, alloc);
    defer setR.deinit(alloc);
    const setAll = try RuneSet.createFromConstString(s.str, alloc);
    defer setAll.deinit(alloc);
    try expectEqual(s.l.len, setAll.matchMany(s.l).?);
    try expectEqual(s.r.len, setAll.matchMany(s.r).?);
    try testMatchNone(setL, s.r);
    try testMatchNone(setR, s.l);
    try expect(setL.subsetOf(setAll));
    try expect(setR.subsetOf(setAll));
    try expect(!setAll.subsetOf(setL));
    try expect(!setAll.subsetOf(setR));
}

fn verifyLRSets(s: LRstrings, alloc: Allocator) !void {
    try verifyLRstringsData(s, alloc);
    const str = s.str;
    const l = s.l;
    const r = s.r;
    try verifySetOperations(str, l, r, alloc);
}

fn verifySetOperations(str: []const u8, l: []const u8, r: []const u8, alloc: Allocator) !void {
    try withStringVerifySetProperties(str, alloc);
    try withStringVerifySetProperties(l, alloc);
    try withStringVerifySetProperties(r, alloc);
    try verifySetUnion(str, l, r, alloc);
    try verifySetDifference(str, l, r, alloc);
    try verifySetIntersection(str, l, r, alloc);
}

/// Validate union properties of an LRstring set:
///
/// - The union of the `l` set and the `r` set matches both `l` and `r`
/// - The union of sets matches `str`
/// - A set of `str` is equal to the union of `l` and `r`
///
fn verifySetUnion(str: []const u8, l: []const u8, r: []const u8, alloc: Allocator) !void {
    const setL = try RuneSet.createFromConstString(l, alloc);
    defer setL.deinit(alloc);
    const setR = try RuneSet.createFromConstString(r, alloc);
    defer setR.deinit(alloc);
    const setU = try setL.setUnion(setR, alloc);
    defer setU.deinit(alloc);
    const setU2 = try setR.setUnion(setL, alloc);
    defer setU2.deinit(alloc);
    const setAll = try RuneSet.createFromConstString(str, alloc);
    defer setAll.deinit(alloc);
    try expect(setAll.equalTo(setU));
    try expect(setU2.equalTo(setU));
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
    try expect(setR.equalTo(setAdiffL));
    try expect(setL.equalTo(setAdiffR));
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
    const setLdiffR = try setL.setDifference(setR, alloc);
    defer setLdiffR.deinit(alloc);
    try expect(setLdiffR.equalTo(setL));
    const setRdiffL = try setR.setDifference(setL, alloc);
    defer setRdiffL.deinit(alloc);
    try expect(setRdiffL.equalTo(setR));
    const setNone = try setAll.setDifference(setAll, alloc);
    defer setNone.deinit(alloc);
    try expectEqual(0, setNone.codeunitCount());
    try expectEqual(4, setNone.body.len);
    const setLdiffAll = try setL.setDifference(setAll, alloc);
    defer setLdiffAll.deinit(alloc);
    try expectEqual(0, setLdiffAll.codeunitCount());
    try expectEqual(4, setLdiffAll.body.len);
    try expect(setNone.equalTo(setLdiffAll));
    const setRdiffAll = try setR.setDifference(setAll, alloc);
    defer setRdiffAll.deinit(alloc);
    try expectEqual(0, setRdiffAll.codeunitCount());
    try expectEqual(4, setRdiffAll.body.len);
    try expect(setNone.equalTo(setRdiffAll));
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
    try expect(setAllandR.equalTo(setR));
    const matchR = setAllandR.matchMany(r);
    if (matchR) |nMatch| {
        try expectEqual(r.len, nMatch);
    } else try expect(false);
    const setAllandL = try setAll.setIntersection(setL, alloc);
    defer setAllandL.deinit(alloc);
    try expect(setAllandL.equalTo(setL));
    const matchL = setAllandL.matchMany(l);
    if (matchL) |nMatch2| {
        try expectEqual(l.len, nMatch2);
    } else try expect(false);
    const setNoneL = try setL.setIntersection(setR, alloc);
    defer setNoneL.deinit(alloc);
    try expectEqual(0, setNoneL.codeunitCount());
    try expectEqual(4, setNoneL.body.len);
    const setNoneR = try setR.setIntersection(setL, alloc);
    defer setNoneR.deinit(alloc);
    try expectEqual(0, setNoneR.codeunitCount());
    try expectEqual(4, setNoneR.body.len);
    try expect(setNoneL.equalTo(setNoneR));
    const setLandAll = try setL.setIntersection(setAll, alloc);
    defer setLandAll.deinit(alloc);
    try expect(setAllandL.equalTo(setLandAll));
    const setRandAll = try setR.setIntersection(setAll, alloc);
    defer setRandAll.deinit(alloc);
    try expect(setRandAll.equalTo(setAllandR));
}

fn verifySetsOfTwoLRstrings(L: LRstrings, R: LRstrings, alloc: Allocator) !void {
    const str = try std.mem.concat(alloc, u8, &.{ L.str, R.str });
    defer alloc.free(str);
    const l = try std.mem.concat(alloc, u8, &.{ L.l, R.l });
    defer alloc.free(l);
    const r = try std.mem.concat(alloc, u8, &.{ L.r, R.r });
    defer alloc.free(r);
    // try as combined structure
    const combined_a = LRstrings{
        .str = str,
        .l = l,
        .r = r,
    };
    try verifyLRstringsData(combined_a, alloc);
    try verifySetUnion(str, l, r, alloc);
    try verifySetDifference(str, l, r, alloc);
    try verifySetIntersection(str, l, r, alloc);
    // try as separated structure
    const combined_b = LRstrings{
        .str = str,
        .l = L.str,
        .r = R.str,
    };
    try verifyLRstringsData(combined_b, alloc);
    try verifySetUnion(str, L.str, R.str, alloc);
    try verifySetDifference(str, L.str, R.str, alloc);
    try verifySetIntersection(str, L.str, R.str, alloc);
}

//| Test Suite

test "workshop" {
    const allocator = std.testing.allocator;
    const d_set = try RuneSet.createFromConstString(deseret.str, allocator);
    defer d_set.deinit(allocator);
    // try verifySetIteration(d_set);
}

test "verify sets of LRstrings data" {
    const allocator = std.testing.allocator;
    try verifyLRSets(ascii, allocator);
    try verifyLRSets(greek, allocator);
    try verifyLRSets(math, allocator);
    try verifyLRSets(linear_B, allocator);
    try verifyLRSets(deseret, allocator);
    try verifyLRSets(two_byte_feather, allocator);
    try verifyLRSets(two_byte_chunk, allocator);
    try verifyLRSets(cjk_feather, allocator);
    try verifyLRSets(cjk_chunk, allocator);
    try verifyLRSets(cjk_chunk4k, allocator);
    try verifyLRSets(cjk_scatter, allocator);
    try verifyLRSets(pua_A_chunk, allocator);
    try verifyLRSets(pua_A_feather, allocator);
    try verifyLRSets(smp_chunk, allocator);
    try verifyLRSets(smp_scatter, allocator);
    try verifyLRSets(tangut_chunk, allocator);
    try verifyLRSets(tangut_widechunk, allocator);
    try verifyLRSets(tangut_scatter, allocator);
    try verifyLRSets(khitan_widechunk, allocator);
    try verifyLRSets(rand1, allocator);
    try verifyLRSets(rand2, allocator);
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
    try verifySetsOfTwoLRstrings(cjk_chunk4k, greek, allocator);
    try verifySetsOfTwoLRstrings(two_byte_chunk, khitan_widechunk, allocator);
    try verifySetsOfTwoLRstrings(cjk_feather, khitan_widechunk, allocator);
    try verifySetsOfTwoLRstrings(two_byte_feather, tangut_widechunk, allocator);
    try verifySetsOfTwoLRstrings(ascii, deseret, allocator);
    try verifySetsOfTwoLRstrings(cjk_scatter, math, allocator);
    try verifySetsOfTwoLRstrings(math, cjk_chunk4k, allocator);
    try verifySetsOfTwoLRstrings(khitan_widechunk, tangut_widechunk, allocator);
    try verifySetsOfTwoLRstrings(smp_chunk, pua_A_feather, allocator);
    try verifySetsOfTwoLRstrings(pua_A_chunk, smp_chunk, allocator);
    try verifySetsOfTwoLRstrings(smp_chunk, cjk_chunk4k, allocator);
}

test "partial set matches" {
    const allocator = std.testing.allocator;
    const abcSet = try RuneSet.createFromConstString("abcdefghij", allocator);
    defer abcSet.deinit(allocator);
    try expectEqual(4, abcSet.matchMany("abcd123").?);
    try expectEqual(5, abcSet.matchManyAssumeValid("acegi12"));
}

test "subsetting" {
    const allocator = std.testing.allocator;
    const nums = "01234567890";
    const numSuper = try RuneSet.createFromConstString(nums, allocator);
    defer numSuper.deinit(allocator);
    const numSub = try RuneSet.createFromConstString(nums[0..5], allocator);
    defer numSub.deinit(allocator);
    try expect(numSub.subsetOf(numSuper));
    try expectEqual(false, numSuper.subsetOf(numSub));
    const alpha = "ABCDEFGHIJKL";
    const alphaSuper = try RuneSet.createFromConstString(alpha, allocator);
    defer alphaSuper.deinit(allocator);
    const alphaSub = try RuneSet.createFromConstString(alpha[0..8], allocator);
    defer alphaSub.deinit(allocator);
    try expect(alphaSub.subsetOf(alphaSuper));
    try expect(!alphaSuper.subsetOf(alphaSub));
    const greek_s = greek.str;
    const greekSuper = try RuneSet.createFromConstString(greek_s, allocator);
    defer greekSuper.deinit(allocator);
    const greekSub = try RuneSet.createFromConstString(greek_s[0 .. greek_s.len - 8], allocator);
    defer greekSub.deinit(allocator);
    try expect(greekSub.subsetOf(greekSuper));
    try expect(!greekSuper.subsetOf(greekSub));
    const han_s = cjk_chunk4k.str;
    const hanSuper = try RuneSet.createFromConstString(han_s, allocator);
    defer hanSuper.deinit(allocator);
    const hanSub = try RuneSet.createFromConstString(han_s[0 .. han_s.len - 72], allocator);
    defer hanSub.deinit(allocator);
    try expect(hanSub.subsetOf(hanSuper));
    try expect(!hanSuper.subsetOf(hanSub));
    const deseret_s = deseret.str;
    const deseretSuper = try RuneSet.createFromConstString(deseret_s, allocator);
    defer deseretSuper.deinit(allocator);
    const deseretSub = try RuneSet.createFromConstString(deseret_s[0 .. deseret_s.len - 24], allocator);
    defer deseretSub.deinit(allocator);
    try expect(deseretSub.subsetOf(deseretSuper));
    try expect(!deseretSuper.subsetOf(deseretSub));
    const greek_deseret = greek_s ++ deseret_s;
    const greekDeseret = try RuneSet.createFromConstString(greek_deseret, allocator);
    defer greekDeseret.deinit(allocator);
    try expect(greekSuper.subsetOf(greekDeseret));
    try expect(!greekDeseret.subsetOf(greekSuper));
}

test "coverage cases" {
    const allocator = std.testing.allocator;
    const setGreek = try RuneSet.createFromConstString(greek.str, allocator);
    defer setGreek.deinit(allocator);
    try expectEqual(null, setGreek.t4slice());
    try expectEqual(null, setGreek.t3slice());
    // invalid follow byte
    try expectEqual(null, setGreek.matchOne("\x9f"));
    try expectEqual(0, setGreek.matchOneAssumeValid("\x9f"));
    const setABC = try RuneSet.createFromConstString("abc", allocator);
    defer setABC.deinit(allocator);
    try expectEqual(0, setABC.matchOneAssumeValid("d"));
    // invalid high byte
    try expectError(error.InvalidUnicode, RuneSet.createFromConstString("\xff\xff", allocator));
    // invalid follow byte
    try expectError(error.InvalidUnicode, RuneSet.createFromConstString("a\xb8", allocator));
    // truncated multibyte
    try expectError(error.InvalidUnicode, RuneSet.createFromConstString("abc\xf0\x9f", allocator));
    // incomplete multibyte
    try expectError(error.InvalidUnicode, RuneSet.createFromConstString("λθ⌘\xf0abcde", allocator));
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
const rand1 = data.rand1;
const rand2 = data.rand2;

test "data integrity" {
    try std.testing.expectEqualStrings(pua_A_chunk.str, pua_A_feather.str);
}

const t_ext = @import("test-data-ext.zig");
const fuzz = @import("fuzz.zig");

test "extended random test data" {
    if (config.test_more) {
        const allocator = std.testing.allocator;
        const testudo = t_ext.mucho_testo[0..];
        var i: usize = 0;
        for (testudo) |t| {
            i += 1;
            try verifyLRSets(t, allocator);
        }
    }
}

test "fuzz set creation" {
    if (config.test_more) {
        const allocator = std.testing.allocator;
        try fuzz.bruteFuzzAndIgnorance(allocator);
    }
}
