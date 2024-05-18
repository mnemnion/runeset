//! libruneset: fast utf8 codepoint sets for Zig
//!
//! We begin with the canonical charsets, which can represent arbitrary
//! classes of characters and support the big three set operations.
//!
//! This will be enhanced with ranged charsets, which can take a small
//! number of ranges (three seems right) for a more compact memory
//! layout, and similar speed for testing membership.  These will operate
//! against each other in union, intersection, and difference, as well as
//! against canonical sets.  This will be represented as a tagged union,
//! which also includes inverted (anything not in set) variants on both.
//!
//! All set operations will be nondestructive, returning a new set.  Some
//! cases of binary operations on two ranged sets will return a canonical
//! set, any operation involving a canonical set will return a canonical
//! set.  All ASCII sets will be canonical, since there's no advantage in
//! using a ranged representation there.
//!
//! The body of a canonical set is a `[]const u64`, with an internal structure
//! defined by the algorithms which use them.

const std = @import("std");
const ArrayList = std.ArrayList;
const Allocator = std.mem.Allocator;
const assert = std.debug.assert;
// "pub" might not be appropriate here, these types
// will most likely be fully encapsulated by the library.
const elements = @import("elements.zig");

const Mask = elements.Mask;
const toMask = Mask.toMask;
const CodeUnit = elements.CodeUnit;
const split = elements.split;

const RuneSet = []const u64;

const InvalidUnicode = error.InvalidUnicode;
/// Creates the body of a RuneSet from a mutable string, allocating
/// a (still mutable) []u64 from the allocator and returning it.
///
/// This operation turns the string into garbage, the caller is
/// responsible for freeing that memory, and owns the memory returned.
///
/// The string must have only valid utf-8, or this function will return
/// an error.
fn createBodyFromString(str: []u8, allocator: Allocator) ![]u64 {
    // The header handles all ASCII and lead bytes.  The fourth word
    // defines the offset into T4, which is zero unless the set contains
    // four-byte characters.
    var header: [4]u64 = .{0} ** 4;
    // We 'sieve' the string, taking characters in order of length,
    // and remove them from the string by moving all other characters
    // back.
    var back: usize = 0; // amount to move unused characters back.
    var sieve = str;
    // Lead pass: lead byte of everything added, ASCII characters
    // removed.
    var idx: usize = 0;
    var low = toMask(0);
    var hi = toMask(0);
    var lead = toMask(0);
    while (idx < sieve.len) {
        const cu = split(sieve[idx]);
        switch (cu.kind) {
            .low => {
                low.add(cu);
                back += 1;
                idx += 1;
            },
            .hi => {
                hi.add(cu);
                back += 1;
                idx += 1;
            },
            .lead => {
                lead.add(cu);
                const nBytes = cu.nBytes();
                if (nBytes) |nB| {
                    std.debug.assert(nB <= 4);
                    if (idx + nB > sieve.len) {
                        return InvalidUnicode;
                    }
                    if (nB >= 2) {
                        sieve[idx - back] = sieve[idx];
                        sieve[idx - back + 1] = sieve[idx + 1];
                    }
                    if (nB >= 3) {
                        sieve[idx - back + 2] = sieve[idx + 2];
                    }
                    if (nB == 4) {
                        sieve[idx - back + 3] = sieve[idx + 3];
                    }
                    idx += nB;
                } else {
                    return InvalidUnicode;
                }
            },
            .follow => return InvalidUnicode,
        }
    }
    // ASCII is now complete, copy over the masks to the header.
    header[0] = low.m;
    header[1] = hi.m;
    if (lead.count() == 0) { // set was ASCII-only
        assert(sieve.len == back);
        const memHeader = try allocator.alloc(u64, 4);
        @memcpy(memHeader, &header);
        return memHeader;
    }
    std.debug.print("str after sieve:\n{s}\n", .{sieve});
    sieve = sieve[0 .. sieve.len - back];
    std.debug.print("str after pass one:\n{s}\n", .{sieve});
    var T2: [56]u64 = .{0} ** 56; // Masks for all second bytes.
    idx = 0;
    back = 0;
    while (idx < sieve.len) {
        const cu = split(sieve[idx]);
        switch (cu.kind) {
            .low, .hi, .follow => return InvalidUnicode,
            .lead => {
                const nBytes = cu.nBytes();
                if (nBytes) |nB| {
                    if (idx + nB > sieve.len) return InvalidUnicode;
                    assert(lead.isIn(cu));
                    // add all second bytes
                    const b = split(sieve[idx + 1]);
                    // guaranteed in-range because nBytes validates
                    var bMask = toMask(T2[cu.body]);
                    bMask.add(b);
                    T2[cu.body] = bMask.m;
                    if (nB == 2) {
                        back += 2;
                    }
                    if (nB >= 3) {
                        sieve[idx - back] = sieve[idx];
                        sieve[idx - back + 1] = sieve[idx + 1];
                        sieve[idx - back + 2] = sieve[idx + 2];
                        if (nB == 4) {
                            sieve[idx - back + 3] = sieve[idx + 3];
                        }
                    }
                    idx += nB;
                } else return InvalidUnicode;
            },
        }
    }
    if (sieve.len == back) {
        // Only two-byte masks
        header[2] = lead.m;
        const T2c = compactSlice(&T2);
        const setBody = try allocator.alloc(u64, 4 + T2c.len);
        @memcpy(setBody[0..4], &header);
        @memcpy(setBody[4..], T2c);
        return setBody;
    }

    // header[2] = lead.m;

    return &header; // TODO obviously this data becomes garbage and must be copied
}

// remove all zero elements from a slice, returning the now-compaced slice.
fn compactSlice(slice: []u64) []u64 {
    var write: usize = 0;
    for (slice) |x| {
        if (x != 0) {
            slice[write] = x;
            write += 1;
        }
    }
    return slice[0..write];
}

const testing = std.testing;
const expect = testing.expect;
const expectEqual = testing.expectEqual;

fn makeMutable(s: []const u8, a: Allocator) ![]u8 {
    const mut = try a.alloc(u8, s.len);
    @memcpy(mut, s);
    return mut;
}
test "ASCII createBodyFromString" {
    const allocator = std.testing.allocator;
    const ALstr = try makeMutable("ABCDEFGHIJKL", allocator);
    defer allocator.free(ALstr);
    const asciiset = try createBodyFromString(ALstr, allocator);
    defer allocator.free(asciiset);
    var low = toMask(asciiset[0]);
    try expectEqual(low.m, 0);
    const hi = toMask(asciiset[1]);
    try expect(hi.isIn(split('C')));
    try expect(!hi.isIn(split('a')));
    const some_nums = try makeMutable("02468", allocator);
    defer allocator.free(some_nums);
    const numset = try createBodyFromString(some_nums, allocator);
    defer allocator.free(numset);
    low = toMask(numset[0]);
    try expectEqual(numset[1], 0);
    try expect(low.isIn(split('2')));
    try expect(!low.isIn(split('3')));
}

test "two-byte createBodyFromString" {
    const allocator = std.testing.allocator;
    const greekstr = try makeMutable("ABCDαβγδεζηθικλμνξοπρςστυφχψωEFG", allocator);
    defer allocator.free(greekstr);
    const greekset = try createBodyFromString(greekstr, allocator);
    defer allocator.free(greekset);
    const hi = toMask(greekset[1]);
    try expect(hi.isIn(split('G')));
    const lead = toMask(greekset[2]);
    const alfabeta = "αβ";
    std.debug.print("alfabeta {d}\n", .{split(alfabeta[0]).body});
    try expect(lead.isIn(split(alfabeta[0])));
}

test compactSlice {
    var arr: [6]u64 = .{ 0, 1, 2, 0, 1, 0 };
    const smol = compactSlice(arr[0..]);
    try expectEqual(3, smol.len);
    try expectEqual(1, smol[0]);
    try expectEqual(2, smol[1]);
    try expectEqual(1, smol[2]);
}

// Run elements tests as well
test {
    testing.refAllDecls(@This());
}
