//! RuneSet test suite
//!
//! The intention is to provide 100% code coverage to the machine-instruction
//! level.

const std = @import("std");
const config = @import("config");
const testing = std.testing;
const Allocator = std.mem.Allocator;
const builtin = @import("builtin");

const safemode = builtin.mode == .Debug or builtin.mode == .ReleaseSafe;

pub const elements = @import("elements.zig");
pub const runeset = @import("runeset.zig");

pub const data = @import("test-data.zig");

const RuneSet = runeset.RuneSet;
const RuneSetMemo = runeset.RuneSetMemo;
const RuneMap = runeset.RuneMap;
const OptKind = runeset.OptKind;
const codeunit = elements.codeunit;

const expect = std.testing.expect;
const expectEqual = testing.expectEqual;
const expectError = testing.expectError;
const expectEqualStrings = testing.expectEqualStrings;

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
        try expectEqual(null, set.ordinalMatch(slice));
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

fn verifyMemoMatchesSet(str: []const u8, set: RuneSet, alloc: Allocator) !void {
    const memo = try RuneSetMemo.createFromRuneSet(set, alloc);
    defer memo.deinit(alloc);
    try expectEqual(set.body.len, memo.body.len);
    try expectEqual(expectedMemoOffsetCount(memo), memo.offsets.len);
    try verifyMemoOffsetsAreBodyRelative(memo);
    try expectEqual(str.len, memo.matchMany(str).?);
    try expectEqual(str.len, memo.matchManyAssumeValid(str));
    try expectEqual(set.matchManyAllowInvalid("\x9fabc"), memo.matchManyAllowInvalid("\x9fabc"));

    var idx: usize = 0;
    while (idx < str.len) {
        const slice = str[idx..];
        const expected = set.matchOne(slice).?;
        try expectEqual(expected, memo.matchOne(slice).?);
        try expectEqual(set.matchOneAssumeValid(slice), memo.matchOneAssumeValid(slice));
        idx += expected;
    }
}

fn verifyMemoMatchesLR(s: LRstrings, alloc: Allocator) !void {
    const memoL = try RuneSetMemo.createFromConstString(s.l, alloc);
    defer memoL.deinit(alloc);
    const memoR = try RuneSetMemo.createFromConstString(s.r, alloc);
    defer memoR.deinit(alloc);

    try expectEqual(s.l.len, memoL.matchMany(s.l).?);
    try expectEqual(s.l.len, memoL.matchManyAssumeValid(s.l));
    try expectEqual(s.r.len, memoR.matchMany(s.r).?);
    try expectEqual(s.r.len, memoR.matchManyAssumeValid(s.r));
    try testMemoMatchNone(memoL, s.r);
    try testMemoMatchNone(memoR, s.l);
}

fn verifyRuneMapMatchesOrdinal(comptime opt: OptKind, str: []const u8, allocator: Allocator) !void {
    const memo = try RuneSetMemo.createFromConstString(str, allocator);
    errdefer memo.deinit(allocator);

    const vals = try allocator.alloc(usize, memo.asRuneSet().runeCount());
    errdefer allocator.free(vals);
    for (vals, 0..) |*val, idx| {
        val.* = idx;
    }

    const map = try RuneMap(usize, opt).initWithRuneSetMemo(allocator, memo, vals, null);
    defer map.deinit(allocator);

    var iter = map.set.asRuneSet().iterateRunes();
    var expected: usize = 0;
    while (iter.next()) |rune| {
        try expectEqual(map.set.asRuneSet().ordinalMatch(rune).?, map.indexOf(rune).?);
        try expectEqual(@as(?usize, expected), map.get(rune));
        expected += 1;
    }
    try expectEqual(vals.len, expected);
}

fn testMemoMatchNone(memo: RuneSetMemo, str: []const u8) !void {
    var idx: usize = 0;
    while (idx < str.len) {
        const slice = str[idx..];
        const nB = codeunit(slice[0]).nBytes() orelse 1;
        try expectEqual(0, memo.matchOne(slice));
        try expectEqual(0, memo.matchOneAssumeValid(slice));
        idx += nB;
    }
}

fn verifyMemoMatchesTwoLR(L: LRstrings, R: LRstrings, alloc: Allocator) !void {
    const str = try std.mem.concat(alloc, u8, &.{ L.str, R.str });
    defer alloc.free(str);
    const l = try std.mem.concat(alloc, u8, &.{ L.l, R.l });
    defer alloc.free(l);
    const r = try std.mem.concat(alloc, u8, &.{ L.r, R.r });
    defer alloc.free(r);

    try verifyMemoMatchesLR(.{
        .str = str,
        .l = l,
        .r = r,
    }, alloc);
    try verifyMemoMatchesLR(.{
        .str = str,
        .l = L.str,
        .r = R.str,
    }, alloc);
}

fn verifyMemoOffsetsAreBodyRelative(memo: RuneSetMemo) !void {
    for (memo.offsets[1..]) |offset| {
        try expect(offset < memo.body.len);
    }
}

fn expectMemoStepMatch(memo: *const RuneSetMemo, str: []const u8) !void {
    var st: RuneSetMemo.Step = .none;
    for (str, 0..) |b, idx| {
        st = memo.step(st, b);
        if (idx + 1 == str.len) {
            try expectEqual(RuneSetMemo.Step.match, st);
        } else {
            try expect(st != .none);
            try expect(st != .match);
        }
    }
}

fn expectedMemoOffsetCount(memo: RuneSetMemo) usize {
    const t3_start = 4 + @popCount(memo.body[2]);
    const t2_memo_start = 4 + @popCount(memo.body[2] & codeunit(32).hiMask());
    const t2_4b_start = 4 + @popCount(memo.body[2] & codeunit(48).hiMask());
    const t3_memo_end = t3_start + popCountWords(memo.body[t2_4b_start..t3_start]);
    return 1 + t3_memo_end - t2_memo_start;
}

fn popCountWords(words: []const u64) usize {
    var count: usize = 0;
    for (words) |word| {
        count += @popCount(word);
    }
    return count;
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
    try verifySetIteration(set);
}

fn verifySetIteration(set: RuneSet) !void {
    var setIter = set.iterateRunes();
    var lastRune: [4]u8 = undefined;
    var lastRuneLen: usize = 0;
    // Count total bytes seen
    var codeunits: usize = 0;
    // Count total runes seen
    var rune_count: usize = 0;
    while (setIter.next()) |rune| {
        // skipping the first rune, verify that codeunit value is increasing
        if (codeunits > 0) {
            try expect(std.mem.order(u8, lastRune[0..lastRuneLen], rune) == .lt);
        }
        codeunits += rune.len;
        @memcpy(lastRune[0..rune.len], rune);
        lastRuneLen = rune.len;
        const matchedBytes = set.matchOne(rune).?;
        try expectEqual(rune.len, matchedBytes);
        const order = set.ordinalMatch(rune).?;
        try expectEqual(rune_count, order);
        rune_count += 1;
    }
    try expectEqual(set.codeunitCount(), codeunits);
    try expectEqual(set.runeCount(), rune_count);
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
    try expectEqual(setL.runeCount(), setL.countMatches(s.l));
    try expectEqual(setR.runeCount(), setR.countMatches(s.r));
    try expectEqual(0, setR.countMatches(s.l));
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
    try verifyLRSets(combined_a, alloc);
    // try as separated structure
    const combined_b = LRstrings{
        .str = str,
        .l = L.str,
        .r = R.str,
    };
    try verifyLRSets(combined_b, alloc);
}

//| Test Suite

test "iterator edge cases" {
    const allocator = std.testing.allocator;
    try withStringVerifySetProperties("abcdefghijklmnopqrstuvwxyz", allocator);
    try withStringVerifySetProperties("012345" ++ greek.str, allocator);
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
    const setMath = try RuneSet.createFromConstString(math.str, allocator);
    defer setMath.deinit(allocator);
    var out_array: std.ArrayList(u8) = .empty;
    defer out_array.deinit(allocator);
    var writer: std.Io.Writer.Allocating = .fromArrayList(allocator, &out_array);
    try setGreek.serialize(&writer.writer, .private, "greek");
    const priv_str = try writer.toOwnedSlice();
    defer allocator.free(priv_str);
    const greek_str = "const greek = RuneSet{ .body = &.{ 0x0, 0x0, 0xc000, 0x0, 0xfffffffbfffe0000, 0x3ff } };\n";
    try expectEqualStrings(greek_str, priv_str);
    try setGreek.serialize(&writer.writer, .public, "greek");
    const pub_str = try writer.toOwnedSlice();
    defer allocator.free(pub_str);
    const pub_greek_str = "pub " ++ greek_str;
    try expectEqualStrings(pub_greek_str, pub_str);
    // Bit of an eyeball test...
    const greek2 = RuneSet{ .body = &.{ 0x0, 0x0, 0xc000, 0x0, 0xfffffffbfffe0000, 0x3ff } };
    try expect(setGreek.equalTo(greek2));
    try expectEqual(null, setGreek.t4slice());
    try expectEqual(null, setGreek.t3slice());
    // invalid follow byte
    try expectEqual(null, setGreek.matchOne("\x9f"));
    try expectEqual(null, setGreek.matchMany("\x9f"));
    try expectEqual(null, setGreek.ordinalMatch("\x9f"));
    try expectEqual(0, setGreek.matchOneAssumeValid("\x9f"));
    // Invalid non-follow byte
    try expectEqual(null, setGreek.ordinalMatch("\xceB"));
    try expectEqual(null, setMath.ordinalMatch("\xe2\x88Q"));
    // No slice
    try expectEqual(null, setGreek.t3_3c_slice());
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

test "RuneSetMemo matches RuneSet" {
    const allocator = testing.allocator;
    for (mini_samples) |sample| {
        const set = try RuneSet.createFromConstString(sample.str, allocator);
        defer set.deinit(allocator);
        try verifyMemoMatchesSet(sample.str, set, allocator);
    }
}

test "RuneSetMemo creates from strings" {
    const allocator = testing.allocator;
    const const_memo = try RuneSetMemo.createFromConstString(greek.str, allocator);
    defer const_memo.deinit(allocator);
    try expectEqual(greek.str.len, const_memo.matchMany(greek.str).?);

    const mutable = try allocator.alloc(u8, deseret.str.len);
    defer allocator.free(mutable);
    @memcpy(mutable, deseret.str);
    const mutable_memo = try RuneSetMemo.createFromMutableString(mutable, allocator);
    defer mutable_memo.deinit(allocator);
    try expectEqual(deseret.str.len, mutable_memo.matchMany(deseret.str).?);

    try expectError(error.InvalidUnicode, RuneSetMemo.createFromConstString("\xff\xff", allocator));
}

test "RuneSetMemo steps one byte at a time" {
    const allocator = testing.allocator;
    for (mini_samples) |sample| {
        const memo = try RuneSetMemo.createFromConstString(sample.str, allocator);
        defer memo.deinit(allocator);

        var idx: usize = 0;
        while (idx < sample.str.len) {
            const slice = sample.str[idx..];
            const n_bytes = codeunit(slice[0]).nBytes().?;
            try expectMemoStepMatch(&memo, slice[0..n_bytes]);
            idx += n_bytes;
        }
    }
}

test "RuneSetMemo matches LR sides independently" {
    const allocator = testing.allocator;
    const samples = [_]LRstrings{
        ascii,
        greek,
        math,
        linear_B,
        deseret,
        two_byte_feather,
        two_byte_chunk,
        cjk_feather,
        cjk_chunk,
        cjk_chunk4k,
        cjk_scatter,
        pua_A_chunk,
        pua_A_feather,
        smp_chunk,
        smp_scatter,
        tangut_chunk,
        tangut_widechunk,
        tangut_scatter,
        khitan_widechunk,
        rand1,
        rand2,
    };
    for (samples) |sample| {
        try verifyMemoMatchesLR(sample, allocator);
    }

    try verifyMemoMatchesTwoLR(greek, math, allocator);
    try verifyMemoMatchesTwoLR(deseret, greek, allocator);
    try verifyMemoMatchesTwoLR(deseret, khitan_widechunk, allocator);
    try verifyMemoMatchesTwoLR(greek, deseret, allocator);
    try verifyMemoMatchesTwoLR(greek, cjk_scatter, allocator);
    try verifyMemoMatchesTwoLR(cjk_chunk4k, greek, allocator);
    try verifyMemoMatchesTwoLR(two_byte_chunk, khitan_widechunk, allocator);
    try verifyMemoMatchesTwoLR(cjk_feather, khitan_widechunk, allocator);
    try verifyMemoMatchesTwoLR(two_byte_feather, tangut_widechunk, allocator);
    try verifyMemoMatchesTwoLR(ascii, deseret, allocator);
    try verifyMemoMatchesTwoLR(cjk_scatter, math, allocator);
    try verifyMemoMatchesTwoLR(math, cjk_chunk4k, allocator);
    try verifyMemoMatchesTwoLR(khitan_widechunk, tangut_widechunk, allocator);
    try verifyMemoMatchesTwoLR(smp_chunk, pua_A_feather, allocator);
    try verifyMemoMatchesTwoLR(pua_A_chunk, smp_chunk, allocator);
    try verifyMemoMatchesTwoLR(smp_chunk, cjk_chunk4k, allocator);
}

test "RuneSetMemo set operations match RuneSet" {
    const allocator = testing.allocator;
    const setL = try RuneSet.createFromConstString(math.l, allocator);
    defer setL.deinit(allocator);
    const setR = try RuneSet.createFromConstString(math.r, allocator);
    defer setR.deinit(allocator);
    const memoL = try RuneSetMemo.createFromRuneSet(setL, allocator);
    defer memoL.deinit(allocator);
    const memoR = try RuneSetMemo.createFromRuneSet(setR, allocator);
    defer memoR.deinit(allocator);

    const union_set = try setL.setUnion(setR, allocator);
    defer union_set.deinit(allocator);
    const union_memo = try memoL.setUnion(memoR, allocator);
    defer union_memo.deinit(allocator);
    try expect(union_set.equalTo(union_memo.asRuneSet()));
    try expectEqual(math.str.len, union_memo.matchMany(math.str).?);

    const diff_set = try union_set.setDifference(setR, allocator);
    defer diff_set.deinit(allocator);
    const diff_memo = try union_memo.setDifference(memoR, allocator);
    defer diff_memo.deinit(allocator);
    try expect(diff_set.equalTo(diff_memo.asRuneSet()));
    try expectEqual(math.l.len, diff_memo.matchMany(math.l).?);

    const intersect_set = try union_set.setIntersection(setL, allocator);
    defer intersect_set.deinit(allocator);
    const intersect_memo = try union_memo.setIntersection(setL, allocator);
    defer intersect_memo.deinit(allocator);
    try expect(intersect_set.equalTo(intersect_memo.asRuneSet()));
    try expectEqual(math.l.len, intersect_memo.matchMany(math.l).?);

    const disjoint_set = try union_set.setDisjunction(setL, allocator);
    defer disjoint_set.deinit(allocator);
    const disjoint_memo = try union_memo.setDisjunction(memoL, allocator);
    defer disjoint_memo.deinit(allocator);
    try expect(disjoint_set.equalTo(disjoint_memo.asRuneSet()));
    try expectEqual(math.r.len, disjoint_memo.matchMany(math.r).?);
}

test "RuneSetMemo serializes declaration and body" {
    const allocator = testing.allocator;
    const set = try RuneSet.createFromConstString(greek.str, allocator);
    defer set.deinit(allocator);
    const memo = try RuneSetMemo.createFromRuneSet(set, allocator);
    defer memo.deinit(allocator);

    var body_array: std.ArrayList(u8) = .empty;
    defer body_array.deinit(allocator);
    var body_writer: std.Io.Writer.Allocating = .fromArrayList(allocator, &body_array);
    try memo.serializeBody(&body_writer.writer);
    const body = try body_writer.toOwnedSlice();
    defer allocator.free(body);

    try expect(std.mem.startsWith(u8, body, ".{ .body = &.{ 0x0, 0x0, 0xc000"));
    try expect(std.mem.indexOf(u8, body, ".offsets = &.{") != null);
    try expect(std.mem.endsWith(u8, body, " } }"));

    var serialized_array: std.ArrayList(u8) = .empty;
    defer serialized_array.deinit(allocator);
    var serialized_writer: std.Io.Writer.Allocating = .fromArrayList(allocator, &serialized_array);
    try memo.serialize(&serialized_writer.writer, .public, "greek_memo");
    const serialized = try serialized_writer.toOwnedSlice();
    defer allocator.free(serialized);

    const expected = try std.mem.concat(allocator, u8, &.{
        "pub const greek_memo: RuneSetMemo = ",
        body,
        ";\n",
    });
    defer allocator.free(expected);
    try expectEqualStrings(expected, serialized);
}

test "RuneMap gets dense values by matched rune" {
    const allocator = testing.allocator;
    const vals = try allocator.dupe(u16, &.{ 10, 20, 30, 40, 50 });
    const map = try RuneMap(u16, .none).init(allocator, "Azλ⌘𐐀", vals, 999);
    defer map.deinit(allocator);

    try expectEqual(@as(usize, map.set.body.len - 4), map.offsets.len);
    try expectEqual(@as(?u16, 10), map.get("A"));
    try expectEqual(@as(?u16, 20), map.get("z"));
    try expectEqual(@as(?u16, 30), map.get("λ"));
    try expectEqual(@as(?u16, 40), map.get("⌘"));
    try expectEqual(@as(?u16, 50), map.get("𐐀"));
    try expectEqual(@as(?u16, 999), map.get("B"));
    try expectEqual(@as(?u16, 999), map.get("\x9f"));
}

test "RuneMap can return null for misses" {
    const allocator = testing.allocator;
    const vals = try allocator.dupe(u8, &.{ 1, 2, 3 });
    const map = try RuneMap(u8, .none).init(allocator, "abc", vals, null);
    defer map.deinit(allocator);

    try expectEqual(@as(usize, 0), map.offsets.len);
    try expectEqual(@as(?u8, 1), map.get("a"));
    try expectEqual(@as(?u8, null), map.get("d"));
}

test "RuneMap indices match RuneSet ordinals" {
    const allocator = testing.allocator;
    for (mini_samples) |sample| {
        try verifyRuneMapMatchesOrdinal(.none, sample.str, allocator);
    }
}

test "RuneMap dense offsets skip non-final masks" {
    const allocator = testing.allocator;
    const vals = try allocator.dupe(u16, &.{ 10, 20, 30, 40, 50 });
    const map = try RuneMap(u16, .dense).init(allocator, "Azλ⌘𐐀", vals, 999);
    defer map.deinit(allocator);

    try expect(map.offsets.len < map.set.body.len - 4);
    try expectEqual(@as(?u16, 10), map.get("A"));
    try expectEqual(@as(?u16, 20), map.get("z"));
    try expectEqual(@as(?u16, 30), map.get("λ"));
    try expectEqual(@as(?u16, 40), map.get("⌘"));
    try expectEqual(@as(?u16, 50), map.get("𐐀"));
    try expectEqual(@as(?u16, 999), map.get("B"));
    try expectEqual(@as(?u16, 999), map.get("\x9f"));
}

test "RuneMap dense steps assign matched values" {
    const allocator = testing.allocator;
    const vals = try allocator.dupe(u16, &.{ 10, 20, 30, 40, 50 });
    const map = try RuneMap(u16, .dense).init(allocator, "Azλ⌘𐐀", vals, 999);
    defer map.deinit(allocator);

    const samples = [_]struct {
        str: []const u8,
        val: u16,
    }{
        .{ .str = "A", .val = 10 },
        .{ .str = "z", .val = 20 },
        .{ .str = "λ", .val = 30 },
        .{ .str = "⌘", .val = 40 },
        .{ .str = "𐐀", .val = 50 },
    };

    for (samples) |sample| {
        var st: RuneMap(u16, .dense).Step = .none;
        var out: u16 = 0;
        for (sample.str, 0..) |b, idx| {
            st = map.step(st, b, &out);
            if (idx + 1 == sample.str.len) {
                try expectEqual(RuneMap(u16, .dense).Step.match, st);
                try expectEqual(sample.val, out);
            } else {
                try expect(st != .none);
                try expect(st != .match);
                try expectEqual(@as(u16, 0), out);
            }
        }
    }
}

test "RuneMap dense step assigns default only when available" {
    const allocator = testing.allocator;

    const default_vals = try allocator.dupe(u8, &.{ 1, 2, 3 });
    const default_map = try RuneMap(u8, .dense).init(allocator, "abc", default_vals, 99);
    defer default_map.deinit(allocator);

    var default_out: u8 = 7;
    try expectEqual(RuneMap(u8, .dense).Step.none, default_map.step(.none, 'd', &default_out));
    try expectEqual(@as(u8, 99), default_out);

    const null_vals = try allocator.dupe(u8, &.{ 1, 2, 3 });
    const null_map = try RuneMap(u8, .dense).init(allocator, "abc", null_vals, null);
    defer null_map.deinit(allocator);

    var null_out: u8 = 7;
    try expectEqual(RuneMap(u8, .dense).Step.none, null_map.step(.none, 'd', &null_out));
    try expectEqual(@as(u8, 7), null_out);
}

test "RuneMap dense indices match RuneSet ordinals" {
    const allocator = testing.allocator;
    for (mini_samples) |sample| {
        try verifyRuneMapMatchesOrdinal(.dense, sample.str, allocator);
    }
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

const mini_samples = [_]LRstrings{
    ascii,
    greek,
    math,
    linear_B,
    deseret,
    two_byte_feather,
    cjk_chunk,
    smp_scatter,
    tangut_widechunk,
    rand1,
};

test "data integrity" {
    try std.testing.expectEqualStrings(pua_A_chunk.str, pua_A_feather.str);
}

const more_tests = struct {
    const t_ext = @import("test-data-ext.zig");

    test "extended random test data" {
        const allocator = std.testing.allocator;
        const testudo = t_ext.mucho_testo[0..];
        var i: usize = 0;
        for (testudo) |t| {
            i += 1;
            try verifyLRSets(t, allocator);
        }
    }
};

const fuzz_tests = struct {
    const fuzz = @import("fuzz.zig");
    test "fuzz set creation" {
        const allocator = std.testing.allocator;
        try fuzz.bruteFuzzAndIgnorance(allocator);
    }
};

comptime {
    if (config.test_more) {
        _ = more_tests;
        if (!config.no_fuzz) {
            _ = fuzz_tests;
        }
    }
}

// Inline tests of runeset.zig and all tests of element.zig
comptime {
    std.testing.refAllDecls(@This());
}
