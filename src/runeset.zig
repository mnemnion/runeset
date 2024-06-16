//! libruneset: fast utf8 codepoint sets for Zig
//!
//! Provides tools for working with sets of Unicode codepoints,
//! also known as characters or runes.
//!
//! The primary type is RuneSets, which may be constructed in
//! several ways.  These support very fast membership tests
//! through bitmasks and popcount, and are capable of encoding
//! arbitrary sets of characters.
//!
//! Additionally, these may be combined using the basic set operations:
//! union, difference, and intersection, as well as tested for equality.
//!
//! TODO iteration of codeunits

const std = @import("std");
const builtin = @import("builtin");
const Allocator = std.mem.Allocator;
const assert = std.debug.assert;
const dbgPrint = std.debug.print;

const elements = @import("elements.zig");

const Mask = elements.Mask;
const toMask = Mask.toMask;
/// A wrapper for a single byte of utf8
pub const CodeUnit = elements.CodeUnit;
/// codeunit(:u8) creates a CodeUnit
pub const codeunit = elements.codeunit;

/// Error indicating that a string for construction a RuneSet contains
/// invalid Unicode.
const InvalidUnicode = error.InvalidUnicode;

// Meaningful names for the T1 slots
const LOW = 0;
const HI = 1;
const LEAD = 2;
const T4_OFF = 3;

// Important values for correct use of RuneSets
// NOTE: These are offset/length values, e.g. two-byte range is 0..32,
//       meaning the highest legal body value is 31.
const TWO_MAX = 32; // maximum cu.body for two byte lead
const THREE_MAX = 48; // maximum for three byte lead
const FOUR_MAX = 56; // maximum for four-byte lead

// Masks for info about set[LEAD]
const MASK_OUT_TWO: u64 = codeunit(TWO_MAX - 1).lowMask();
const MASK_IN_TWO: u64 = codeunit(TWO_MAX).hiMask();
const MASK_IN_FOUR: u64 = codeunit(47).lowMask();
const MASK_OUT_FOUR: u64 = codeunit(THREE_MAX).hiMask();
// 32..56
const MASK_IN_THREE: u64 = MASK_OUT_TWO & MASK_OUT_FOUR;

/// RuneSet: a fast character set for UTF-8.
///
/// Create with:
///
/// - RuneSet.createFromMutableString
/// - RuneSet.createFromConstString
/// - TODO create from range or ranges (?)
///
/// Free with `set.deinit(allocator)`.
pub const RuneSet = struct {
    body: []const u64,

    // some names for meaningful operations
    inline fn leadMask(self: RuneSet) Mask {
        return toMask(self.body[LEAD]);
    }

    inline fn t2len(self: RuneSet) usize {
        return @popCount(self.body[LEAD]);
    }

    inline fn t2start(_: RuneSet) usize {
        return 4;
    }

    //| The following functions assume that the set has the
    //| sort of runes implied by asking for the region in
    //| question

    /// end of T2, exclusive
    pub inline fn t2end(self: RuneSet) usize {
        return self.t2start() + self.t2len();
    }

    /// final element of T2
    pub inline fn t2final(self: RuneSet) usize {
        return self.t2end() - 1;
    }

    /// Start of region of T2 containing 3 byte b codeunits
    inline fn t2_3b_start(self: RuneSet) usize {
        return 4 + @popCount(self.body[LEAD] & MASK_IN_TWO);
    }

    /// Start of region of T2 containing 4 byte b codeunits
    inline fn t2_4b_start(self: RuneSet) usize {
        return 4 + @popCount(self.body[LEAD] & MASK_OUT_FOUR);
    }

    /// Start of region of T3 containing 3 byte c codeunits
    inline fn t3_3c_start(self: RuneSet) usize {
        const c4b_off = popCountSlice(self.body[self.t2_4b_start()..self.t2end()]);
        return self.t3start() + c4b_off;
    }

    /// Beginning (backwardsly speaking!) of d byte region of T3
    inline fn t3_3d_begin(self: RuneSet) usize {
        return self.t3_3c_start() - 1;
    }

    /// Start of T3 region
    inline fn t3start(self: RuneSet) usize {
        return self.t2end();
    }

    // End of T3 region
    inline fn t3end(self: RuneSet) usize {
        if (self.body[T4_OFF] == 0)
            return self.body.len
        else
            return self.body[T4_OFF];
    }

    /// Start of T4 region, *or* zero for no T4
    inline fn t4offset(self: RuneSet) usize {
        return self.body[T4_OFF];
    }

    pub inline fn t2slice(self: RuneSet) ?[]const u64 {
        if (self.body[LEAD] == 0)
            return null
        else
            return self.body[4..self.t2end()];
    }

    pub inline fn t3slice(self: RuneSet) ?[]const u64 {
        if (self.noThreeBytes())
            return null
        else
            return self.body[self.t3start()..self.t3end()];
    }

    pub inline fn t4slice(self: RuneSet) ?[]const u64 {
        if (self.noFourBytes())
            return null
        else
            return self.body[self.t4offset()..];
    }

    inline fn maskAt(self: RuneSet, off: usize) Mask {
        return toMask(self.body[off]);
    }

    //| Debugging

    pub fn debugMaskAt(self: RuneSet, off: usize) void {
        std.debug.print("0x{x:0>16}\n", .{self.body[off]});
    }

    pub fn debugT1(self: RuneSet) void {
        std.debug.print("\nheader:\n", .{});
        for (self.body[0..4]) |m| {
            std.debug.print("0x{x:0>16} ", .{m});
        }
        std.debug.print("\n", .{});
    }

    pub fn debugT2(self: RuneSet) void {
        std.debug.print("\nT2:", .{});
        const T2 = self.t2slice();
        if (T2) |t2| {
            std.debug.print(" {d}\n", .{t2.len});
            for (t2) |m| {
                std.debug.print("0x{x:0>16} ", .{m});
            }
        } else {
            std.debug.print(" null\n", .{});
        }
        std.debug.print("\n", .{});
    }

    pub fn debugT3(self: RuneSet) void {
        std.debug.print("\nT3:", .{});
        const T3 = self.t3slice();
        if (T3) |t3| {
            std.debug.print(" {d}\n", .{t3.len});
            for (t3) |m| {
                std.debug.print("0x{x:0>16} ", .{m});
            }
        } else {
            std.debug.print(" null\n", .{});
        }
        std.debug.print("\n", .{});
    }

    pub fn debugT4(self: RuneSet) void {
        std.debug.print("\nT4:", .{});
        const T4 = self.t4slice();
        if (T4) |t4| {
            std.debug.print(" {d}\n", .{t4.len});
            for (t4) |m| {
                std.debug.print("0x{x:0>16} ", .{m});
            }
        } else {
            std.debug.print(" null\n", .{});
        }
        std.debug.print("\n", .{});
    }

    pub fn debugPrint(self: RuneSet) void {
        std.debug.print("\n", .{});
        self.debugT1();
        self.debugT2();
        self.debugT3();
        self.debugT4();
    }

    // No need for noTwoBytes because LEAD is present
    // in all cases, so testing a lead byte is always
    // safe

    // This counts any third byte, including one in
    // a four-byte char
    inline fn noThreeBytes(self: RuneSet) bool {
        return self.body[LEAD] & MASK_OUT_TWO == 0;
    }

    inline fn noFourBytes(self: RuneSet) bool {
        return self.body[T4_OFF] == 0;
    }

    fn spreadT2(self: RuneSet, T2: []u64) void {
        var iter = self.maskAt(LEAD).iterElements();
        const body = self.body;
        var off: usize = 4;
        while (iter.next()) |e| {
            T2[e] = body[off];
            off += 1;
        }
    }

    /// Free the memory allocated for a RuneSet
    pub fn deinit(self: RuneSet, alloc: Allocator) void {
        alloc.free(self.body);
    }

    /// Create a RuneSet from a mutable `[]u8`, destroying it in the
    /// process.  Caller is responsible for freeing the argument and
    /// return value; delete the latter with `set.deinit(allocator)`.
    pub fn createFromMutableString(str: []u8, allocator: Allocator) !RuneSet {
        return RuneSet{ .body = try createBodyFromString(str, allocator) };
    }

    /// Create a RuneSet from a `[]const u8`.
    ///
    /// Delete with `set.deinit(allocator)`.
    pub fn createFromConstString(str: []const u8, allocator: Allocator) !RuneSet {
        const s_mut = try makeMutable(str, allocator);
        defer allocator.free(s_mut);
        return try RuneSet.createFromMutableString(s_mut, allocator);
    }

    /// Create a RuneSet from a slice of 'string' slices: `[]const []const u8`.
    ///
    /// Delete with `set.deinit(allocator)`.
    pub fn createFromConstStringSlice(strs: []const []const u8, allocator: Allocator) !RuneSet {
        const s_concat = try std.mem.concat(allocator, u8, strs);
        defer allocator.free(s_concat);
        return try RuneSet.createFromMutableString(s_concat, allocator);
    }

    /// Write the codepoints of the RuneSet to a buffer.
    /// Caller guarantees that the buffer has room for all codepoints,
    /// this value can be obtained by calling runeset.codeunitCount().
    pub fn writeToBuffer(self: RuneSet, buf: []u8) void {
        const bod = self.body;
        var idx: usize = 0;
        var lowIter = toMask(bod[LOW]).iterCodeUnits(.low);
        while (lowIter.next()) |a| {
            buf[idx] = a.byte();
            idx += 1;
        }
        var hiIter = toMask(bod[HI]).iterCodeUnits(.hi);
        while (hiIter.next()) |a| {
            buf[idx] = a.byte();
            idx += 1;
        }
        if (bod[LEAD] == 0) return;
        var T1b_iter = toMask(bod[LEAD] & MASK_IN_TWO).iterCodeUnits(.lead);
        var T2i = self.t2start();
        while (T1b_iter.next()) |a| {
            var T2iter = toMask(bod[T2i]).iterCodeUnits(.follow);
            T2i += 1;
            while (T2iter.next()) |b| {
                buf[idx] = a.byte();
                idx += 1;
                buf[idx] = b.byte();
                idx += 1;
            }
        }
        if (self.noThreeBytes()) {
            assert(T2i == bod.len);
            return;
        }
        var T1c_iter = toMask(bod[LEAD] & MASK_IN_THREE).iterCodeUnits(.lead);
        var T3i = self.t3end() - 1;
        while (T1c_iter.next()) |a| {
            var T2iter = toMask(bod[T2i]).iterCodeUnits(.follow);
            T2i += 1;
            while (T2iter.next()) |b| {
                var T3iter = toMask(bod[T3i]).iterCodeUnitsBack(.follow);
                T3i -= 1;
                while (T3iter.next()) |c| {
                    buf[idx] = a.byte();
                    idx += 1;
                    buf[idx] = b.byte();
                    idx += 1;
                    buf[idx] = c.byte();
                    idx += 1;
                }
            }
        }
        if (self.noFourBytes()) {
            assert(T2i == self.t2end());
            assert(T3i == self.t3start() - 1);
            return;
        } else {
            assert(T3i == self.t3_3d_begin());
        }
        var T1d_iter = toMask(bod[LEAD] & MASK_IN_FOUR).iterCodeUnits(.lead);
        var T4i = bod.len - 1;
        while (T1d_iter.next()) |a| {
            var T2iter = toMask(bod[T2i]).iterCodeUnits(.follow);
            T2i += 1;
            while (T2iter.next()) |b| {
                var T3iter = toMask(bod[T3i]).iterCodeUnitsBack(.follow);
                T3i -= 1;
                while (T3iter.next()) |c| {
                    var T4iter = toMask(bod[T4i]).iterCodeUnitsBack(.follow);
                    T4i -= 1;
                    while (T4iter.next()) |d| {
                        buf[idx] = a.byte();
                        idx += 1;
                        buf[idx] = b.byte();
                        idx += 1;
                        buf[idx] = c.byte();
                        idx += 1;
                        buf[idx] = d.byte();
                        idx += 1;
                    }
                }
            }
        }
        assert(T2i == self.t2end());
        assert(T3i == self.t3start() - 1);
        assert(T4i == self.t4offset() - 1);
        return;
    }

    /// Serialize a RuneSet to a newly created string. Caller must free
    /// memory.
    pub fn toString(self: RuneSet, alloc: Allocator) error{OutOfMemory}![]u8 {
        const buf = try alloc.alloc(u8, self.codeunitCount());
        self.writeToBuffer(buf);
        return buf;
    }

    /// Match one rune at the beginning of the slice.
    /// This is safe to use with invalid UTF-8, and will return null if
    /// such is encountered.
    /// The normal return value is the number of bytes matched.
    /// Zero means that the rune beginning the slice was not a match.
    pub fn matchOne(self: RuneSet, slice: []const u8) ?usize {
        return matchOneDirectly(self.body, slice);
    }

    /// Match one rune at the beginning of the slice.
    /// Invalid UTF-8 is allowed, and will return 0.  A match will
    /// return the number of bytes matched.
    pub fn matchOneAllowInvalid(self: RuneSet, slice: []const u8) usize {
        return matchOneDirectly(self.body, slice) orelse 0;
    }

    /// Match one rune at the beginning of the slice.  Assumes the slice is
    /// valid UTF-8.  It is legal to call this function if the byte at [0]
    /// is not the lead byte of a sequence.  Returns number of bytes matched.
    ///
    /// Note: in debug mode, this will panic if it encounters invalid UTF-8.
    /// In release mode, it can return nonsense results, or read beyond the
    /// buffer if called on a truncated codepoitn.
    pub fn matchOneAssumeValid(self: RuneSet, slice: []const u8) usize {
        return matchOneDirectAssumeValid(self.body, slice);
    }

    /// Match as many runes as possible starting from the beginning of
    /// the slice.  Returns the number of bytes matched.
    /// Safe to use on invalid UTF-8, returning null if any is found.
    pub fn matchMany(self: RuneSet, slice: []const u8) ?usize {
        var idx: usize = 0;
        while (idx < slice.len) {
            const nBytes = self.matchOne(slice[idx..]);
            if (nBytes) |nB| {
                if (nB == 0)
                    return idx;
                idx += nB;
            } else {
                return null;
            }
        }
        return idx;
    }

    /// Match as many runes as possible starting from the beginning of
    /// the slice.  Invalid UTF-8 is treated as a failure to match.
    /// Safe to use on invalid UTF-8, including truncated multi-byte
    /// runes at the end of the slice.
    pub fn matchManyAllowInvalid(self: RuneSet, slice: []const u8) usize {
        var idx: usize = 0;
        while (idx < slice.len) {
            const nB = self.matchOne(slice[idx..]) orelse 0;
            if (nB == 0)
                return idx;
            idx += nB;
        }
        return idx;
    }

    /// Matches as many bytes as it can in `slice`, returning the number
    /// of bytes matched.  `slice` must be valid UTF-8.  For a discussion
    /// of the consequences of violating this assumption, see
    /// `RuneSet.matchOneAssumeValid`.
    pub fn matchManyAssumeValid(self: RuneSet, slice: []const u8) usize {
        var idx: usize = 0;
        while (idx < slice.len) {
            const nBytes = self.matchOneAssumeValid(slice[idx..]);
            if (nBytes == 0)
                return idx;
            idx += nBytes;
        }
        return idx;
    }

    /// Test if two RuneSets are equal.
    pub fn equalTo(self: RuneSet, other: RuneSet) bool {
        if (self.body.len != other.body.len) return false;
        for (self.body, other.body) |l, r| {
            if (l != r) return false;
        }
        return true;
    }

    /// A logging equality test, for testing purposes
    pub fn expectEqualTo(self: RuneSet, other: RuneSet) bool {
        if (self.body.len != other.body.len) {
            std.debug.print("L.len {d} != R.len {d}\n", .{ self.body.len, other.body.len });
            return false;
        }
        const LT3 = self.t3start();
        const RT3 = other.t3start();
        const LT4 = self.t4offset();
        const RT4 = other.t4offset();
        var match = true;
        for (self.body, other.body, 0..) |l, r, i| {
            if (l != r) {
                std.debug.print("at {d}, L != R:\n", .{i});
                if (i >= LT4 and LT4 != 0)
                    std.debug.print("LT4 + {d}, ", .{i - LT4})
                else if (i >= LT3)
                    std.debug.print("LT3 + {d}, ", .{i - LT3});
                if (i >= RT4 and RT4 != 0)
                    std.debug.print("RT4 + {d}\n", .{i - RT4})
                else if (i >= RT3)
                    std.debug.print("RT3 + {d}\n", .{i - RT3});
                std.debug.print("L: 0x{x:0>16}\n", .{l});
                std.debug.print("R: 0x{x:0>16}\n", .{r});
                match = false;
            }
        }
        return match;
    }

    // Return a tuple counting number of one, two, three, and four
    // byte codepoints
    fn counts(self: RuneSet) struct { usize, usize, usize, usize } {
        var c: [4]u64 = .{0} ** 4;
        c[0] = popCountSlice(self.body[LOW..LEAD]);
        if (self.body[LEAD] == 0) return .{ c[0], c[1], c[2], c[3] };
        const twosCount: usize = @popCount(self.body[LEAD] & MASK_IN_TWO);
        c[1] = popCountSlice(self.body[4 .. 4 + twosCount]);
        if (self.noThreeBytes()) return .{ c[0], c[1], c[2], c[3] };
        c[2] = popCountSlice(self.body[self.t3_3c_start()..self.t3end()]);
        if (self.noFourBytes()) return .{ c[0], c[1], c[2], c[3] };
        c[3] = popCountSlice(self.body[self.t4offset()..]);
        return .{ c[0], c[1], c[2], c[3] };
    }

    /// Return a count of runes (Unicode codepoints) in set.
    pub fn runeCount(self: RuneSet) usize {
        const a, const b, const c, const d = self.counts();
        return a + b + c + d;
    }

    /// Return a count of codunits (u8) in set.
    ///
    /// This is the required length of buffer for a slice passed to
    /// `runeset.toString(buf)`.
    pub fn codeunitCount(self: RuneSet) usize {
        const a, const b, const c, const d = self.counts();
        return a + (2 * b) + (3 * c) + (4 * d);
    }

    /// Test if the receiver is a subset of the argument.
    pub fn subsetOf(L: RuneSet, R: RuneSet) bool {
        const Lbod = L.body;
        const Rbod = R.body;
        if (Lbod[LOW] & ~Rbod[LOW] != 0) return false;
        if (Lbod[HI] & ~Rbod[HI] != 0) return false;
        if (Lbod[LEAD] & ~Rbod[LEAD] != 0) return false;
        if (Lbod[LEAD] == 0) return true;
        // We now know Rbod[LEAD] is a superset.
        // We iterate T2 separately, rather than doing one long
        // run, because iteration of T2 is always cheap, and this
        // gives early returns for some cases where the two sets share
        // a large number of three and four byte characters, costing
        // only a few cycles extra.
        const L1m = toMask(Lbod[LEAD]);
        {
            var LT2i = L.t2start();
            var RT2i = R.t2start();
            var R1iter = toMask(Rbod[LEAD]).iterElements();
            while (R1iter.next()) |e2| {
                if (L1m.isElem(e2)) {
                    if (Lbod[LT2i] & ~Rbod[RT2i] != 0) {
                        return false;
                    }
                    LT2i += 1;
                }
                RT2i += 1;
            }
            assert(LT2i == L.t2end());
            assert(RT2i == R.t2end());
        }
        // It also lets us use these two short circuits:
        if (L.noThreeBytes()) return true;
        if (R.noThreeBytes()) return false;
        // We can handle T3 and T4 in one iteration, backward
        // across T2, and forward across T3 and T4 (if it exists)
        var LT2i = L.t2final();
        var RT2i = R.t2final();
        var LT3i = L.t3start();
        var RT3i = R.t3start();
        // These can be zero, if so, they won't be used,
        // because the sets would have no d byte leads
        var LT4i = L.t4offset();
        var RT4i = R.t4offset();
        var RT1c_iter = blk: {
            const R1c = Rbod[LEAD] & MASK_OUT_TWO;
            break :blk toMask(R1c).iterElemBack();
        };
        while (RT1c_iter.next()) |e2| {
            if (L1m.isElem(e2)) {
                // We know this:
                assert(Lbod[LT2i] & ~Rbod[RT2i] == 0);
                const LT2m = toMask(Lbod[LT2i]);
                var RT2iter = toMask(Rbod[RT2i]).iterElemBack();
                while (RT2iter.next()) |e3| {
                    if (LT2m.isElem(e3)) {
                        if (Lbod[LT3i] & ~Rbod[RT3i] != 0) {
                            return false;
                        }
                        if (e2 >= THREE_MAX) {
                            // Means we know this:
                            assert(!R.noFourBytes());
                            const LT3m = toMask(Lbod[LT3i]);
                            var RT3iter = toMask(Rbod[RT3i]).iterElements();
                            while (RT3iter.next()) |e4| {
                                if (LT3m.isElem(e4)) {
                                    // Therefore:
                                    assert(!L.noFourBytes());
                                    if (Lbod[LT4i] & ~Rbod[RT4i] != 0) {
                                        return false;
                                    }
                                    LT4i += 1;
                                }
                                RT4i += 1;
                            }
                        }
                        LT3i += 1;
                    } else {
                        if (e2 >= THREE_MAX) {
                            RT4i += @popCount(Rbod[RT3i]);
                        }
                    }
                    RT3i += 1;
                }
                LT2i -= 1;
            } else {
                if (e2 >= THREE_MAX) {
                    const RT3count = @popCount(Rbod[RT2i]);
                    for (0..RT3count) |_| {
                        RT4i += @popCount(Rbod[RT3i]);
                        RT3i += 1;
                    }
                } else {
                    RT3i += @popCount(Rbod[RT2i]);
                }
            }
            RT2i -= 1;
        } // postconditions
        assert(LT2i == L.t2_3b_start() - 1);
        assert(RT2i == R.t2_3b_start() - 1);
        assert(LT3i == L.t3end());
        assert(RT3i == R.t3end());
        assert(LT4i == Lbod.len or LT4i == 0);
        assert(RT4i == Rbod.len or RT4i == 0);
        return true;
    }

    // TODO add RuneSet iterator

    //| # Set Operations
    //|
    //| Provides union, difference, and intersection operations.
    //|
    //| All such operations are non-destructive, allocating a new set.
    //|
    //| ## Vocabulary
    //|
    //| The variables used here are terse, but follow a fairly consistent syntax.
    //|
    //| The receiver is L, the comparator is R, and the new set is N.
    //|
    //| We refer to the tiers as header (T1), T2, T3, and T4.  These contain
    //| bitmasks for any byte of that order in a codepoint's sequence.
    //|
    //| We call the codepoints a (not used), b, c, and d, depending on how many
    //| bytes they have in total.  Thus, a d byte of the T2 tier is the second byte
    //| of a four-byte codeunit.
    //|
    //| Abbreviations: i for index, m for mask, e for element, len for length,
    //| iter for an iterator. c at the end of a tier means that tier is compacted.
    //|
    //| Examples: NT3i is an index into the T3 tier of N, our new set.
    //| e2 is an element of T2.  NT3_d_len is the length of the d region of
    //| NT3.  eRT3m is a mask of an element of R's T3.

    /// Union of two RuneSets.
    ///
    pub fn setUnion(L: RuneSet, R: RuneSet, allocator: Allocator) error{OutOfMemory}!RuneSet {
        var header: [4]u64 = .{0} ** 4;
        const Lbod = L.body;
        const Rbod = R.body;
        header[LOW] = Lbod[LOW] | Rbod[LOW];
        header[HI] = Lbod[HI] | Rbod[HI];
        header[LEAD] = Lbod[LEAD] | Rbod[LEAD];
        if (header[LEAD] == 0) { // ASCII sets
            const Nbod = try allocator.alloc(u64, 4);
            @memcpy(Nbod, &header);
            return RuneSet{ .body = Nbod };
        }
        var NT2: [FOUR_MAX]u64 = .{0} ** FOUR_MAX;
        L.spreadT2(&NT2);
        {
            var RT2i: usize = R.t2start();
            var rIter = toMask(Rbod[LEAD]).iterElements();
            while (rIter.next()) |r_e2| {
                NT2[r_e2] |= Rbod[RT2i];
                RT2i += 1;
            }
        }
        if (L.noThreeBytes() and R.noThreeBytes()) {
            const T2c = compactSlice(&NT2);
            const Nbod = try allocator.alloc(u64, 4 + T2c.len);
            @memcpy(Nbod[0..4], &header);
            @memcpy(Nbod[4..], T2c);
            return RuneSet{ .body = Nbod };
        }
        const NT3 = try allocator.alloc(u64, popCountSlice(NT2[TWO_MAX..]));
        defer allocator.free(NT3);
        // T3 rank
        {
            // These masks tell us which words in NT2 belong
            // to which sets.
            const LT1m = toMask(Lbod[LEAD]);
            const RT1m = toMask(Rbod[LEAD]);
            const both_T1m = toMask(Lbod[LEAD] & Rbod[LEAD]);
            assert(both_T1m.m == LT1m.intersection(RT1m).m);
            // Track progressive offsets into T3s
            var NT3i: usize = 0;
            var LT3i = L.t3start();
            var RT3i = R.t3start();
            // Reverse iterate lead mask elements of three and four bytes
            var LT2i = L.t2final();
            var RT2i = R.t2final();
            var unionT2iter = toMask(header[LEAD] & MASK_OUT_TWO).iterElemBack();
            assert(unionT2iter.mask.m == (Lbod[LEAD] & MASK_OUT_TWO) | (Rbod[LEAD] & MASK_OUT_TWO));
            while (unionT2iter.next()) |e2| {
                if (both_T1m.isElem(e2)) {
                    // Reverse-iterate the mask and test membership
                    const LT2m = toMask(Lbod[LT2i]);
                    const RT2m = toMask(Rbod[RT2i]);
                    const bothT2m = toMask(Lbod[LT2i] & Rbod[RT2i]);
                    LT2i -= 1;
                    RT2i -= 1;
                    var elemIter = toMask(NT2[e2]).iterElemBack();
                    while (elemIter.next()) |e3| {
                        if (bothT2m.isElem(e3)) {
                            NT3[NT3i] = Lbod[LT3i] | Rbod[RT3i];
                            LT3i += 1;
                            RT3i += 1;
                        } else if (LT2m.isElem(e3)) {
                            NT3[NT3i] = Lbod[LT3i];
                            LT3i += 1;
                        } else {
                            assert(RT2m.isElem(e3));
                            NT3[NT3i] = Rbod[RT3i];
                            RT3i += 1;
                        }
                        NT3i += 1;
                    }
                } else if (LT1m.isElem(e2)) {
                    const T3count = @popCount(NT2[e2]);
                    assert(NT2[e2] == Lbod[LT2i]);
                    assert(T3count > 0);
                    for (0..T3count) |_| {
                        NT3[NT3i] = Lbod[LT3i];
                        NT3i += 1;
                        LT3i += 1;
                    }
                    LT2i -= 1;
                } else {
                    assert(RT1m.isElem(e2));
                    const T3count = @popCount(NT2[e2]);
                    assert(NT2[e2] == Rbod[RT2i]);
                    assert(T3count > 0);
                    for (0..T3count) |_| {
                        NT3[NT3i] = Rbod[RT3i];
                        NT3i += 1;
                        RT3i += 1;
                    }
                    RT2i -= 1;
                }
            } // Sanity checks:
            assert(LT2i == T4_OFF + @popCount(Lbod[LEAD] & MASK_IN_TWO));
            assert(RT2i == T4_OFF + @popCount(Rbod[LEAD] & MASK_IN_TWO));
            assert(NT3i == NT3.len);
            assert(LT3i == L.t3end());
            assert(RT3i == R.t3end());
        } // end T3 rank block
        if (L.noFourBytes() and R.noFourBytes()) {
            const T2c = compactSlice(&NT2);
            const T2end = 4 + T2c.len;
            const setLen = T2end + NT3.len;
            const Nbod = try allocator.alloc(u64, setLen);
            @memcpy(Nbod[0..4], &header);
            @memcpy(Nbod[4..T2end], T2c);
            @memcpy(Nbod[T2end..setLen], NT3);
            return RuneSet{ .body = Nbod };
        }
        // T4 setup
        // - popcount of four-byte range of NT3
        const NT2b4len = popCountSlice(NT2[THREE_MAX..]);
        const NT4len = popCountSlice(NT3[0..NT2b4len]);
        // - allocate NT4
        const NT4 = try allocator.alloc(u64, NT4len);
        defer allocator.free(NT4);
        // T4 rank
        {
            // We iterate backward through the header mask, and backward
            // again through the T2 words.  This lets us start at 0 for
            // T3 and T4, going forwards (in reverse *lexical* order) to
            // fill in the union.
            const NT1d_m = toMask(header[LEAD] & MASK_IN_FOUR);
            const LT1d_m = toMask(Lbod[LEAD] & MASK_IN_FOUR);
            const RT1d_m = toMask(Rbod[LEAD] & MASK_IN_FOUR);
            assert(NT1d_m.m == LT1d_m.setunion(RT1d_m).m);
            // T2 offsets are tracked backward
            var LT2i = L.t2final();
            var RT2i = R.t2final();
            // T3s are tracked forward: highest lead byte to lowest
            var LT3i = L.t3start();
            var RT3i = R.t3start();
            // Debug tracking NT3, should be elided in release
            var NT3i: usize = 0;
            // T4 offsets forward: also highest lead to lowest
            var NT4i: usize = 0;
            var LT4i = L.t4offset();
            var RT4i = R.t4offset();
            // If one set lacks four byte runes, this is easy:
            if (LT4i == 0) {
                @memcpy(NT4, Rbod[RT4i..]);
            } else if (RT4i == 0) {
                @memcpy(NT4, Lbod[LT4i..]);
            } else {
                var NT2iter = NT1d_m.iterElemBack();
                while (NT2iter.next()) |e2| {
                    assert(NT2[e2] != 0);
                    if (LT1d_m.isElem(e2) and RT1d_m.isElem(e2)) {
                        // Shared T2 region
                        const LT2m = toMask(Lbod[LT2i]);
                        const RT2m = toMask(Rbod[RT2i]);
                        assert(NT2[e2] == Rbod[RT2i] | Lbod[LT2i]);
                        LT2i -= 1;
                        RT2i -= 1;
                        const unionT2m = toMask(NT2[e2]);
                        const bothT2m = LT2m.intersection(RT2m);
                        assert(unionT2m.m == LT2m.m | RT2m.m);
                        var unionT2iter = unionT2m.iterElemBack();
                        while (unionT2iter.next()) |e3| {
                            if (bothT2m.isElem(e3)) {
                                // back iterate both T3 masks, forward
                                const LT3m = toMask(Lbod[LT3i]);
                                const RT3m = toMask(Rbod[RT3i]);
                                const bothT3m = LT3m.intersection(RT3m);
                                var unionT3iter = blk: {
                                    break :blk LT3m.setunion(RT3m).iterElements();
                                };
                                LT3i += 1;
                                RT3i += 1;
                                if (builtin.mode == .Debug) {
                                    assert(unionT3iter.mask.m == NT3[NT3i]);
                                    NT3i += 1;
                                }
                                while (unionT3iter.next()) |e4| {
                                    if (bothT3m.isElem(e4)) {
                                        NT4[NT4i] = Lbod[LT4i] | Rbod[RT4i];
                                        LT4i += 1;
                                        RT4i += 1;
                                    } else if (LT3m.isElem(e4)) {
                                        NT4[NT4i] = Lbod[LT4i];
                                        LT4i += 1;
                                    } else {
                                        assert(RT3m.isElem(e4));
                                        NT4[NT4i] = Rbod[RT4i];
                                        RT4i += 1;
                                    }
                                    NT4i += 1;
                                }
                            } else if (LT2m.isElem(e3)) {
                                const T4count = @popCount(Lbod[LT3i]);
                                assert(T4count > 0);
                                assert(Lbod[LT3i] == NT3[NT3i]);
                                LT3i += 1;
                                if (builtin.mode == .Debug) {
                                    NT3i += 1;
                                }
                                for (0..T4count) |_| {
                                    NT4[NT4i] = Lbod[LT4i];
                                    NT4i += 1;
                                    LT4i += 1;
                                }
                            } else {
                                assert(RT2m.isElem(e3));
                                const T4count = @popCount(Rbod[RT3i]);
                                assert(T4count > 0);
                                assert(Rbod[RT3i] == NT3[NT3i]);
                                RT3i += 1;
                                if (builtin.mode == .Debug) {
                                    NT3i += 1;
                                }
                                for (0..T4count) |_| {
                                    NT4[NT4i] = Rbod[RT4i];
                                    NT4i += 1;
                                    RT4i += 1;
                                }
                            } // End T3 iteration
                        } // End T2 iteration
                    } else if (LT1d_m.isElem(e2)) {
                        // popcount T2 mask
                        assert(NT2[e2] == Lbod[LT2i]);
                        const T3count = @popCount(Lbod[LT2i]);
                        LT2i -= 1;
                        assert(T3count > 0);
                        for (0..T3count) |_| {
                            const T4count = @popCount(Lbod[LT3i]);
                            assert(T4count > 0);
                            assert(Lbod[LT3i] == NT3[NT3i]);
                            LT3i += 1;
                            if (builtin.mode == .Debug) {
                                NT3i += 1;
                            }
                            for (0..T4count) |_| {
                                NT4[NT4i] = Lbod[LT4i];
                                LT4i += 1;
                                NT4i += 1;
                            }
                        }
                    } else {
                        assert(RT1d_m.isElem(e2));
                        assert(NT2[e2] == Rbod[RT2i]);
                        const T3count = @popCount(Rbod[RT2i]);
                        RT2i -= 1;
                        assert(T3count > 0);
                        for (0..T3count) |_| {
                            const T4count = @popCount(Rbod[RT3i]);
                            assert(Rbod[RT3i] == NT3[NT3i]);
                            RT3i += 1;
                            if (builtin.mode == .Debug) {
                                NT3i += 1;
                            }
                            assert(T4count > 0);
                            for (0..T4count) |_| {
                                NT4[NT4i] = Rbod[RT4i];
                                RT4i += 1;
                                NT4i += 1;
                            }
                        }
                    }
                }
                // postconditions
                assert(NT4i == NT4.len);
                assert(LT2i == L.t2_4b_start() - 1);
                assert(RT2i == R.t2_4b_start() - 1);
                assert(RT3i == R.t3_3c_start());
                assert(LT3i == L.t3_3c_start());
                assert(LT4i == Lbod.len);
                assert(RT4i == Rbod.len);
            }
        } // end T4 rank
        const T2c = compactSlice(&NT2);
        const T2end = 4 + T2c.len;
        const T3end = T2end + NT3.len;
        header[T4_OFF] = T3end;
        const setLen = T3end + NT4.len;
        const Nbod = try allocator.alloc(u64, setLen);
        @memcpy(Nbod[0..4], &header);
        @memcpy(Nbod[4..T2end], T2c);
        @memcpy(Nbod[T2end..T3end], NT3);
        @memcpy(Nbod[T3end..], NT4);
        return RuneSet{ .body = Nbod };
    }

    /// Return difference of receiver and argument as new set.
    /// Calling context owns the memory.
    pub fn setDifference(L: RuneSet, R: RuneSet, allocator: Allocator) error{OutOfMemory}!RuneSet {
        var header: [4]u64 = undefined;
        const Lbod = L.body;
        const Rbod = R.body;
        header[LOW] = Lbod[LOW] & ~Rbod[LOW];
        header[HI] = Lbod[HI] & ~Rbod[HI];
        // We always assign this to header[LEAD] before returning
        var LLeadMask = toMask(Lbod[LEAD]);
        header[T4_OFF] = 0;
        // ASCII check:
        if (Lbod[LEAD] == 0) {
            header[LEAD] = 0;
            const Nbod = try allocator.alloc(u64, 4);
            @memcpy(Nbod, &header);
            return RuneSet{ .body = Nbod };
        }
        // Tier 2
        // We blow up LT2:
        var NT2: [FOUR_MAX]u64 = .{0} ** FOUR_MAX;
        L.spreadT2(&NT2);
        // Take a union of LEADS
        {
            const unionLead = toMask(Rbod[LEAD] | Lbod[LEAD]);
            const L1m = toMask(Lbod[LEAD]);
            const R1m = toMask(Rbod[LEAD]);
            // Iterate and diff
            var RT2i = R.t2start();
            var T2iter = unionLead.iterElements();
            while (T2iter.next()) |e| {
                if (e >= TWO_MAX) break;
                // assert(NT2[e] != 0);
                if (R1m.isElem(e)) {
                    NT2[e] &= ~Rbod[RT2i];
                    RT2i += 1;
                    // If this clears the mask, remove the LEAD bit
                    if (NT2[e] == 0 and L1m.isElem(e))
                        LLeadMask.remove(codeunit(e));
                }
            }
        } // Only L matters here: can't remove what you don't have
        if (L.noThreeBytes()) {
            header[LEAD] = LLeadMask.m;
            const T2c = compactSlice(&NT2);
            const Nbod = try allocator.alloc(u64, 4 + T2c.len);
            @memcpy(Nbod[0..4], &header);
            @memcpy(Nbod[4..], T2c);
            return RuneSet{ .body = Nbod };
        }
        // Tier 3
        //
        // Once again, we take difference on final bytes,
        // then, for any empty three-byte-final mask, we
        // remove the corresponding bit in T2. If that clears
        // a T2 word, we remove that bit in LLeadMask.
        //
        const LT3: []const u64 = L.t3slice().?; // we know this has contents
        const NT3 = try allocator.alloc(u64, LT3.len);
        defer allocator.free(NT3);
        @memcpy(NT3, LT3);
        if (!R.noThreeBytes()) {
            // Track ownership of T2 bits on each side:
            const LT2m = toMask(Lbod[LEAD]);
            const RT2m = toMask(Rbod[LEAD]);
            // Track progress forward through T3 of both sides
            var NT3i: usize = 0; // we've copied over all of LT3 to NT3.
            var RT3i = R.t3start(); // We know there are three-byte seqs in R
            // c-byte region of NT2 is all from L2, but RT2 offset needs tracking
            var RT2i = R.t3start() - 1;
            // We go *back* through the c-byte region of NT2, and *forward*
            // through both T3s. To track the latter, we iterate NT2 using
            // the union of both LEADs.
            var unionLeadIter = toMask(Lbod[LEAD] | Rbod[LEAD]).iterElemBack();
            while (unionLeadIter.next()) |e2| {
                if (e2 >= THREE_MAX) {
                    // d byte; Fast-forward all T3 for each side
                    if (LT2m.isElem(e2)) {
                        assert(NT2[e2] != 0);
                        NT3i += @popCount(NT2[e2]);
                    }
                    if (RT2m.isElem(e2)) {
                        assert(Rbod[RT2i] != 0);
                        RT3i += @popCount(Rbod[RT2i]);
                        RT2i -= 1;
                    }
                    continue;
                } else if (e2 < TWO_MAX) { // done
                    break;
                }
                // In c region of T2s.  T3 indices are past d byte region
                const inLT2 = LT2m.isElem(e2);
                if (!inLT2) {
                    // must be in R, we skip
                    assert(RT2m.isElem(e2));
                    assert(Rbod[RT2i] != 0);
                    RT3i += @popCount(Rbod[RT2i]);
                    RT2i -= 1;
                    continue;
                }
                const inRT2 = RT2m.isElem(e2);
                if (inLT2 and inRT2) {
                    // Find T3 intersections and take difference
                    assert(NT2[e2] != 0);
                    const eLT2m = toMask(NT2[e2]);
                    assert(Rbod[RT2i] != 0);
                    const eRT2m = toMask(Rbod[RT2i]);
                    // We're done with RT2i, decrement
                    RT2i -= 1;
                    // iterate the union of this T2 word backward
                    var unionT2iter = toMask(eLT2m.m | eRT2m.m).iterElemBack();
                    while (unionT2iter.next()) |e3| {
                        if (eLT2m.isElem(e3) and eRT2m.isElem(e3)) {
                            NT3[NT3i] &= ~Rbod[RT3i];
                            // Check for T3 null and mask out T2 bit if so
                            if (NT3[NT3i] == 0) {
                                var eNT2 = toMask(NT2[e2]);
                                eNT2.remove(codeunit(e3));
                                NT2[e2] = eNT2.m;
                                // We check if NT2[e2] is clear after iteration
                            }
                            NT3i += 1;
                            RT3i += 1;
                        } else if (eRT2m.isElem(e3)) {
                            RT3i += 1;
                        } else {
                            assert(eLT2m.isElem(e3));
                            NT3i += 1;
                        }
                    }
                    // Check for emptied NT2 mask
                    if (NT2[e2] == 0) {
                        LLeadMask.remove(codeunit(e2));
                    }
                } else { // Must be L2, advance NT3i
                    assert(inLT2);
                    NT3i += @popCount(NT2[e2]);
                }
            }
        } else { // else R is only two byte or less, no action needed
            assert(R.t3slice() == null);
        }
        // Once again, R 4-byte is irrelevant if L has none
        if (L.noFourBytes()) {
            header[LEAD] = LLeadMask.m;
            const T2c = compactSlice(&NT2);
            const T2end = 4 + T2c.len;
            const T3c = compactSlice(NT3);
            const setLen = T2end + T3c.len;
            const Nbod = try allocator.alloc(u64, setLen);
            @memcpy(Nbod[0..4], &header);
            @memcpy(Nbod[4..T2end], T2c);
            @memcpy(Nbod[T2end..setLen], T3c);
            return RuneSet{ .body = Nbod };
        } else if (R.noFourBytes()) {
            // we can just copy LT4 and return
            header[LEAD] = LLeadMask.m;
            const T2c = compactSlice(&NT2);
            const T2end = 4 + T2c.len;
            const T3c = compactSlice(NT3);
            const T3end = T2end + T3c.len;
            // which is the T4 offset:
            header[T4_OFF] = T3end;
            const T4 = L.t4slice().?; // checked in prior if statement
            const setLen = T3end + T4.len;
            const Nbod = try allocator.alloc(u64, setLen);
            @memcpy(Nbod[0..4], &header);
            @memcpy(Nbod[4..T2end], T2c);
            @memcpy(Nbod[T2end..T3end], T3c);
            @memcpy(Nbod[T3end..setLen], T4);
            return RuneSet{ .body = Nbod };
        }
        // Tier 4.
        const LT4 = L.t4slice().?; // checked above
        const NT4 = try allocator.alloc(u64, LT4.len);
        defer allocator.free(NT4);
        @memcpy(NT4, LT4);
        // We iterate backward through NT2, and each mask thereof,
        // allowing forward iteration through T3 and T4.
        // Track ownership of T2 bits on each side:
        const LT1m = toMask(Lbod[LEAD]);
        const RT1m = toMask(Rbod[LEAD]);
        // Track progress forward through T3
        // The d region of NT3 is still identical to LT3
        var NT3i: usize = 0;
        var RT3i = R.t3start();
        // Similarly, d region of NT2 is still identical to LT2
        var RT2i = R.t2final();
        // track T4 progress forward
        var NT4i: usize = 0;
        var RT4i = Rbod[T4_OFF];
        // Backward iterate T2, d region only
        var unionLeadIter = blk: {
            const LT1d = Lbod[LEAD] & MASK_IN_FOUR;
            const RT1d = Rbod[LEAD] & MASK_IN_FOUR;
            break :blk toMask(LT1d | RT1d).iterElemBack();
        };
        t2iter: while (unionLeadIter.next()) |e2| {
            const inLT2 = LT1m.isElem(e2);
            if (!inLT2) {
                // must be in R, we skip
                assert(RT1m.isElem(e2));
                assert(Rbod[RT2i] != 0);
                assert(NT2[e2] == 0);
                // move T3 and T4 index forward and T2 index back
                const T3count = @popCount(Rbod[RT2i]);
                RT2i -= 1;
                assert(T3count > 0);
                for (0..T3count) |_| {
                    const T4count = @popCount(Rbod[RT3i]);
                    RT3i += 1;
                    assert(T4count > 0);
                    RT4i += T4count;
                }
                continue :t2iter;
            }
            const inRT2 = RT1m.isElem(e2);
            if (inLT2 and inRT2) {
                // Find T3 overlaps and apply T4 accordingly
                assert(NT2[e2] != 0);
                const LT2m = toMask(NT2[e2]);
                assert(Rbod[RT2i] != 0);
                const RT2m = toMask(Rbod[RT2i]);
                // We're done with RT2i, decrement
                RT2i -= 1;
                // iterate the union of this T2 word backward
                var unionT2iter = toMask(LT2m.m | RT2m.m).iterElemBack();
                while (unionT2iter.next()) |e3| {
                    if (LT2m.isElem(e3) and RT2m.isElem(e3)) {
                        // We iterate forward through T3 and T4.
                        // Intersections are diffed,
                        // zeroed words remove that NT3 bit.
                        assert(NT3[NT3i] != 0); // may not be true later!
                        const eLT3m = toMask(NT3[NT3i]);
                        assert(Rbod[RT3i] != 0);
                        const eRT3m = toMask(Rbod[RT3i]);
                        var unionT3iter = toMask(eLT3m.m | eRT3m.m).iterElements();
                        while (unionT3iter.next()) |e4| {
                            if (eLT3m.isElem(e4) and eRT3m.isElem(e4)) {
                                NT4[NT4i] &= ~Rbod[RT4i];
                                // null check for T3 mask out
                                if (NT4[NT4i] == 0) {
                                    var NT3m = toMask(NT3[NT3i]);
                                    NT3m.remove(codeunit(e4));
                                    NT3[NT3i] = NT3m.m;
                                } // NT3[NT3i] checked after unionT3iter while loop
                                NT4i += 1;
                                RT4i += 1;
                            } else if (eRT3m.isElem(e4)) {
                                RT4i += 1;
                            } else {
                                assert(eLT3m.isElem(e4));
                                NT4i += 1;
                            }
                        }
                        if (NT3[NT3i] == 0) {
                            var N3m = toMask(NT2[e2]);
                            N3m.remove(codeunit(e3));
                            NT2[e2] = N3m.m;
                        } // NT2[e2] checked after unionT2iter while loop
                        // finished iterating T3 mask
                        NT3i += 1;
                        RT3i += 1;
                    } else if (RT2m.isElem(e3)) {
                        RT4i += @popCount(Rbod[RT3i]);
                        RT3i += 1;
                    } else {
                        assert(LT2m.isElem(e3));
                        NT4i += @popCount(NT3[NT3i]);
                        NT3i += 1;
                    }
                }
                // Check for emptied NT2 mask
                if (NT2[e2] == 0) {
                    LLeadMask.remove(codeunit(e2));
                }
            } else { // Must be L2, subtract mask from LT3i
                assert(inLT2);
                const NT3count = @popCount(NT2[e2]);
                for (0..NT3count) |_| {
                    const NT4count = @popCount(NT3[NT3i]);
                    NT4i += NT4count;
                    NT3i += 1;
                }
            }
        } // Postconditions
        assert(NT4i == NT4.len);
        // No post for NT3i, we may have removed bits from the popcount
        assert(RT2i == R.t2_4b_start() - 1);
        assert(RT3i == R.t3_3c_start());
        assert(RT4i == Rbod.len);
        // assemble
        header[LEAD] = LLeadMask.m;
        const T2c = compactSlice(&NT2);
        const T2end = 4 + T2c.len;
        const T3c = compactSlice(NT3);
        const T3end = T2end + T3c.len;
        const T4c = compactSlice(NT4);
        if (T4c.len != 0)
            header[T4_OFF] = T3end
        else
            header[T4_OFF] = 0;
        const setLen = T3end + T4c.len;
        const Nbod = try allocator.alloc(u64, setLen);
        @memcpy(Nbod[0..4], &header);
        @memcpy(Nbod[4..T2end], T2c);
        @memcpy(Nbod[T2end..T3end], T3c);
        @memcpy(Nbod[T3end..setLen], T4c);
        return RuneSet{ .body = Nbod };
    }

    /// Return intersection of receiver with first argument.
    /// Calling context owns memory.
    pub fn setIntersection(L: RuneSet, R: RuneSet, allocator: Allocator) error{OutOfMemory}!RuneSet {
        const Lbod = L.body;
        const Rbod = R.body;
        var header: [4]u64 = undefined;
        header[LOW] = Lbod[LOW] & Rbod[LOW];
        header[HI] = Lbod[HI] & Rbod[HI];
        header[LEAD] = Lbod[LEAD] & Rbod[LEAD];
        header[T4_OFF] = 0;
        if (header[LEAD] == 0) {
            const Nbod = try allocator.alloc(u64, 4);
            @memcpy(Nbod, &header);
            return RuneSet{ .body = Nbod };
        }
        // No spreading L2 into NT2 this time, it would just create extra work
        var NT2: [FOUR_MAX]u64 = .{0} ** FOUR_MAX;
        // Must be reassigned to header[LEAD] before returning
        var NLeadMask = toMask(header[LEAD]);
        const LLeadMask = L.maskAt(LEAD);
        const RLeadMask = R.maskAt(LEAD);
        {
            var T2iter = LLeadMask.setunion(RLeadMask).iterElements();
            var LT2i: usize = L.t2start();
            var RT2i: usize = L.t2start();
            while (T2iter.next()) |e2| {
                if (NLeadMask.isElem(e2)) {
                    NT2[e2] = Lbod[LT2i] & Rbod[RT2i];
                    if (NT2[e2] == 0) {
                        NLeadMask.remove(codeunit(e2));
                    }
                    LT2i += 1;
                    RT2i += 1;
                } else if (LLeadMask.isElem(e2)) {
                    LT2i += 1;
                } else {
                    assert(RLeadMask.isElem(e2));
                    RT2i += 1;
                }
            }
        } // we can check LEAD to see if there are surviving c bytes
        if (NLeadMask.m & MASK_OUT_TWO == 0) {
            header[LEAD] = NLeadMask.m;
            const T2c = compactSlice(&NT2);
            const Nbod = try allocator.alloc(u64, 4 + T2c.len);
            @memcpy(Nbod[0..4], &header);
            @memcpy(Nbod[4..], T2c);
            return RuneSet{ .body = Nbod };
        }
        // Tier 3
        const NT3 = try allocator.alloc(u64, popCountSlice(NT2[TWO_MAX..]));
        defer allocator.free(NT3);
        // We take a measure of NT4 now because we might remove
        // NT2 bits and need to leave room for empty masks in NT3
        // NT4 is, at most, as many words as the d block of NT3
        const NT3_d_len = popCountSlice(NT2[THREE_MAX..]);
        {
            // We iterate back through both T2s to find surviving T3s
            var unionT2iter = toMask((Rbod[LEAD] & MASK_OUT_TWO) | (Lbod[LEAD] & MASK_OUT_TWO)).iterElemBack();
            const bothLeadMask = toMask(Lbod[LEAD] & Rbod[LEAD]);
            var RT2i = R.t2final();
            var LT2i = L.t2final();
            var LT3i = L.t3start();
            var RT3i = R.t3start();
            var NT3i: usize = 0;
            while (unionT2iter.next()) |e2| {
                if (bothLeadMask.isElem(e2)) {
                    assert(NT2[e2] != 0);
                    const LT2m = toMask(Lbod[LT2i]);
                    const RT2m = toMask(Rbod[RT2i]);
                    LT2i -= 1;
                    RT2i -= 1;
                    var NT2m = toMask(NT2[e2]);
                    var unionT3Iter = LT2m.setunion(RT2m).iterElemBack();
                    while (unionT3Iter.next()) |e3| {
                        if (LT2m.isElem(e3) and RT2m.isElem(e3)) {
                            NT3[NT3i] = Rbod[RT3i] & Lbod[LT3i];
                            RT3i += 1;
                            LT3i += 1;
                            if (NT3[NT3i] == 0) {
                                NT2m.remove(codeunit(e3));
                            }
                            NT3i += 1;
                        } else if (LT2m.isElem(e3)) {
                            LT3i += 1;
                        } else {
                            assert(RT2m.isElem(e3));
                            RT3i += 1;
                        }
                    }
                    assert(@popCount(NT2[e2]) >= @popCount(NT2m.m));
                    NT2[e2] = NT2m.m;
                    if (NT2[e2] == 0)
                        NLeadMask.remove(codeunit(e2));
                } else if (RLeadMask.isElem(e2)) {
                    RT3i += @popCount(Rbod[RT2i]);
                    RT2i -= 1;
                } else {
                    assert(LLeadMask.isElem(e2));
                    LT3i += @popCount(Lbod[LT2i]);
                    LT2i -= 1;
                }
            } // sanity checks:
            assert(LT3i == L.t3end());
            assert(RT3i == R.t3end());
            assert(LT2i == 3 + @popCount(Lbod[LEAD] & MASK_IN_TWO));
            assert(RT2i == 3 + @popCount(Rbod[LEAD] & MASK_IN_TWO));
            // we should only remove bits from NT2:
            assert(NT3.len >= popCountSlice(NT2[TWO_MAX..]));
        }
        if (NLeadMask.m & MASK_IN_FOUR == 0) {
            header[LEAD] = NLeadMask.m;
            const T2c = compactSlice(&NT2);
            const T2end = 4 + T2c.len;
            const T3c = compactSlice(NT3);
            assert(T3c.len == popCountSlice(NT2[TWO_MAX..]));
            const setLen = T2end + T3c.len;
            const Nbod = try allocator.alloc(u64, setLen);
            @memcpy(Nbod[0..4], &header);
            @memcpy(Nbod[4..T2end], T2c);
            @memcpy(Nbod[T2end..setLen], T3c);
            return RuneSet{ .body = Nbod };
        }
        // Tier 4
        const NT4len = popCountSlice(NT3[0..NT3_d_len]);
        const NT4 = try allocator.alloc(u64, NT4len);
        defer allocator.free(NT4);
        // We follow the now familiar strategy of iterating across
        // unions of the set masks, and following through on the
        // intersections.
        { // In which we track Everything:
            // backward through T2s
            var RT2i = R.t2final();
            var LT2i = L.t2final();
            // forward through d bytes of T3
            var NT3i: usize = 0;
            var LT3i = L.t3start();
            var RT3i = R.t3start();
            // forward through T4
            var NT4i: usize = 0;
            var LT4i = L.t4offset();
            var RT4i = R.t4offset();
            assert(LT4i != 0);
            assert(RT4i != 0);
            var unionT2iter = blk: {
                const LT2d = Lbod[LEAD] & MASK_IN_FOUR;
                const RT2d = Rbod[LEAD] & MASK_IN_FOUR;
                break :blk toMask(LT2d | RT2d).iterElemBack();
            };
            while (unionT2iter.next()) |e2| {
                if (LLeadMask.isElem(e2) and RLeadMask.isElem(e2)) {
                    var NT2m = toMask(NT2[e2]);
                    // This block could already be empty:
                    if (NT2m.m != 0) {
                        const LT2m = toMask(Lbod[LT2i]);
                        const RT2m = toMask(Rbod[RT2i]);
                        LT2i -= 1;
                        RT2i -= 1;
                        var unionT3iter = LT2m.setunion(RT2m).iterElemBack();
                        while (unionT3iter.next()) |e3| {
                            if (LT2m.isElem(e3) and RT2m.isElem(e3)) {
                                var NT3m = toMask(NT3[NT3i]);
                                const LT3m = toMask(Lbod[LT3i]);
                                const RT3m = toMask(Rbod[RT3i]);
                                LT3i += 1;
                                RT3i += 1;
                                // NT3i is handle after replacing NT3m mask
                                var unionT4iter = blk: {
                                    break :blk LT3m.setunion(RT3m)
                                        .iterElements();
                                };
                                while (unionT4iter.next()) |e4| {
                                    if (NT3m.isElem(e4)) {
                                        NT4[NT4i] = Lbod[LT4i] & Rbod[RT4i];
                                        if (NT4[NT4i] == 0) {
                                            NT3m.remove(codeunit(e4));
                                        }
                                        NT4i += 1;
                                        LT4i += 1;
                                        RT4i += 1;
                                    } else if (LT3m.isElem(e4)) {
                                        LT4i += 1;
                                    } else {
                                        assert(RT3m.isElem(e4));
                                        RT4i += 1;
                                    }
                                }
                                NT3[NT3i] = NT3m.m;
                                if (NT3m.m == 0 and NT2m.isElem(e3)) {
                                    NT2m.remove(codeunit(e3));
                                }
                                NT3i += 1;
                            } else if (LT2m.isElem(e3)) {
                                LT4i += @popCount(Lbod[LT3i]);
                                LT3i += 1;
                            } else {
                                assert(RT2m.isElem(e3));
                                RT4i += @popCount(Rbod[RT3i]);
                                RT3i += 1;
                            }
                        } // end T3 union iteration
                    } else { // NT2 is zero
                        assert(NT2[e2] == 0);
                        {
                            const T3count = @popCount(Lbod[LT2i]);
                            LT2i -= 1;
                            assert(T3count > 0);
                            for (0..T3count) |_| {
                                const T4count = @popCount(Lbod[LT3i]);
                                LT3i += 1;
                                assert(T4count > 0);
                                LT4i += T4count;
                            }
                        }
                        {
                            const T3count = @popCount(Rbod[RT2i]);
                            RT2i -= 1;
                            assert(T3count > 0);
                            for (0..T3count) |_| {
                                const T4count = @popCount(Rbod[RT3i]);
                                RT3i += 1;
                                assert(T4count > 0);
                                RT4i += T4count;
                            }
                        }
                    }
                    assert(@popCount(NT2[e2]) >= @popCount(NT2m.m));
                    NT2[e2] = NT2m.m;
                    if (NT2[e2] == 0 and NLeadMask.isElem(e2))
                        NLeadMask.remove(codeunit(e2));
                } else if (LLeadMask.isElem(e2)) {
                    assert(NT2[e2] == 0);
                    const T3count = @popCount(Lbod[LT2i]);
                    LT2i -= 1;
                    assert(T3count > 0);
                    for (0..T3count) |_| {
                        const T4count = @popCount(Lbod[LT3i]);
                        LT3i += 1;
                        assert(T4count > 0);
                        LT4i += T4count;
                    }
                } else {
                    assert(RLeadMask.isElem(e2));
                    assert(NT2[e2] == 0);
                    const T3count = @popCount(Rbod[RT2i]);
                    RT2i -= 1;
                    assert(T3count > 0);
                    for (0..T3count) |_| {
                        const T4count = @popCount(Rbod[RT3i]);
                        RT3i += 1;
                        assert(T4count > 0);
                        RT4i += T4count;
                    }
                }
            } // end T2 union iteration
            // postconditions
            assert(NT4i == NT4.len);
            assert(LT3i == L.t3_3c_start());
            assert(RT3i == R.t3_3c_start());
            assert(LT4i == Lbod.len);
            assert(RT4i == Rbod.len);
            assert(RT2i == R.t2start() + @popCount(Rbod[LEAD] & MASK_OUT_FOUR) - 1);
            assert(LT2i == L.t2start() + @popCount(Lbod[LEAD] & MASK_OUT_FOUR) - 1);
            assert(NT3.len >= popCountSlice(NT2[TWO_MAX..]));
        } // end T4 block
        header[LEAD] = NLeadMask.m;
        // these two are only used in debug mode, hopefully elided in release
        const popT3 = popCountSlice(NT2[TWO_MAX..]);
        const popT4 = popCountSlice(NT2[THREE_MAX..]);
        const T2c = compactSlice(&NT2);
        const T2end = 4 + T2c.len;
        const T3c = compactSlice(NT3);
        assert(T3c.len == popT3);
        const T3end = T2end + T3c.len;
        const T4c = compactSlice(NT4);
        if (T4c.len != 0)
            header[T4_OFF] = T3end
        else
            header[T4_OFF] = 0;
        assert(T4c.len == popCountSlice(T3c[0..popT4]));
        const setLen = T3end + T4c.len;
        const Nbod = try allocator.alloc(u64, setLen);
        @memcpy(Nbod[0..4], &header);
        @memcpy(Nbod[4..T2end], T2c);
        @memcpy(Nbod[T2end..T3end], T3c);
        @memcpy(Nbod[T3end..setLen], T4c);
        return RuneSet{ .body = Nbod };
    }
};

/// Creates the body of a RuneSet from a mutable string, allocating
/// a (still mutable) []u64 from the allocator and returning it.
///
/// This operation turns the string into garbage, the caller is
/// responsible for freeing that memory, and owns the memory returned.
///
/// The string must have only valid utf-8, or this function will return
/// an error.
fn createBodyFromString(str: []u8, allocator: Allocator) error{ InvalidUnicode, OutOfMemory }![]u64 {
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
        const cu = codeunit(sieve[idx]);
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
                const nBytes = cu.nMultiBytes();
                if (nBytes) |nB| {
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
    header[LOW] = low.m;
    header[HI] = hi.m;
    if (lead.count() == 0) { // set was ASCII-only
        assert(sieve.len == back);
        const memHeader = try allocator.alloc(u64, 4);
        @memcpy(memHeader, &header);
        return memHeader;
    }
    sieve = sieve[0 .. sieve.len - back];
    // sieve for second bytes
    var T2: [FOUR_MAX]u64 = .{0} ** FOUR_MAX; // Masks for all second bytes.
    idx = 0;
    back = 0;
    while (idx < sieve.len) {
        const cu = codeunit(sieve[idx]);
        switch (cu.kind) {
            // This is now impossible by construction, we
            // filtered all leading .low and .hi, and would
            // have bailed on a .follow
            .low, .hi, .follow => unreachable,
            .lead => {
                const nBytes = cu.nMultiBytes();
                if (nBytes) |nB| {
                    if (idx + nB > sieve.len) return InvalidUnicode;
                    assert(lead.isIn(cu));
                    // add all second bytes
                    const b = codeunit(sieve[idx + 1]);
                    if (b.kind != .follow) return InvalidUnicode;
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
    } // All second bytes accounted for
    header[LEAD] = lead.m;
    if (sieve.len == back) {
        // High region of T2 must be empty (sanity check)
        assert(popCountSlice(T2[TWO_MAX..]) == 0);
        const T2c = compactSlice(&T2);
        // Should be length of popcount
        assert(T2c.len == lead.count());
        const setBody = try allocator.alloc(u64, 4 + T2c.len);
        @memcpy(setBody[0..4], &header);
        @memcpy(setBody[4..], T2c);
        return setBody;
    }
    sieve = sieve[0 .. sieve.len - back];
    // sieve for third bytes
    // number of elements in the high region of T2
    // is the number of masks in T3
    const T3: []u64 = try allocator.alloc(u64, popCountSlice(T2[TWO_MAX..]));
    defer allocator.free(T3);
    @memset(T3, 0);
    idx = 0;
    back = 0;
    // T3 is laid out 'backward': highest lead byte to lowest.
    while (idx < sieve.len) {
        const one = codeunit(sieve[idx]);
        switch (one.kind) {
            .low, .hi, .follow => unreachable,
            .lead => {
                assert(lead.isIn(one));
                const nB = one.nMultiBytes().?;
                assert(nB >= 3);
                assert(one.body >= TWO_MAX);
                const two = codeunit(sieve[idx + 1]);
                assert(two.kind == .follow); // already validated in sieve two
                // T2 is one-to-one
                const twoMask = toMask(T2[one.body]);
                assert(twoMask.isIn(two));
                // count all higher elements
                const hiThree = twoMask.higherThan(two).?;
                // ward off the impractical case where our lead
                // byte is the maximum value allowed by Unicode
                const threeOff = if (one.body < FOUR_MAX - 1)
                    hiThree + popCountSlice(T2[one.body + 1 ..])
                else
                    hiThree;
                const three = codeunit(sieve[idx + 2]);
                if (three.kind != .follow) return InvalidUnicode;
                var threeMask = toMask(T3[threeOff]);
                threeMask.add(three);
                T3[threeOff] = threeMask.m;
                if (nB == 3)
                    back += 3
                else {
                    assert(nB == 4);
                    sieve[idx - back] = sieve[idx];
                    sieve[idx - back + 1] = sieve[idx + 1];
                    sieve[idx - back + 2] = sieve[idx + 2];
                    sieve[idx - back + 3] = sieve[idx + 3];
                }
                idx += nB;
            },
        }
    }
    if (sieve.len == back) {
        // No four-byte characters
        // "very high" region of T2 must be empty
        assert(popCountSlice(T2[THREE_MAX..]) == 0);
        // T3 must have values in it
        assert(popCountSlice(T3) != 0);
        const T2c = compactSlice(&T2);
        // Must be length of popcount
        assert(T2c.len == lead.count());
        const T3off = 4 + T2c.len;
        const setLen = T3off + T3.len;
        const setBody = try allocator.alloc(u64, setLen);
        @memcpy(setBody[0..4], &header);
        @memcpy(setBody[4 .. 4 + T2c.len], T2c);
        @memcpy(setBody[T3off..], T3);
        return setBody;
    }
    sieve = sieve[0 .. sieve.len - back];
    // We now know the T4 header offset:
    // Length of header (4) + compacted T2 + popcount hi T2
    const T4Head = 4 + nonZeroCount(&T2) + popCountSlice(T2[TWO_MAX..]);
    header[T4_OFF] = T4Head;
    // Allocate T4: this is counted by a popcount of all
    // elements of T2 which have a fourth byte, then
    // popcounting that many elements of T3.
    const elem4 = popCountSlice(T2[THREE_MAX..]);
    const T4len = popCountSlice(T3[0..elem4]);
    const T4 = try allocator.alloc(u64, T4len);
    defer allocator.free(T4);
    @memset(T4, 0);
    // we no longer need back
    idx = 0;
    // sieve for fourth byte
    // T4 is a zig, not a zag: laid out forward, such that the
    // highest Unicode values of a codepoint are found at the
    // end of the T4 region.
    while (idx < sieve.len) {
        const one = codeunit(sieve[idx]);
        switch (one.kind) {
            .low, .hi, .follow => unreachable,
            .lead => {
                assert(lead.isIn(one));
                const nB = one.nMultiBytes().?;
                assert(nB == 4);
                assert(one.body >= THREE_MAX);
                const two = codeunit(sieve[idx + 1]);
                assert(two.kind == .follow);
                // Same procedure to find our path into
                // the maze...
                const twoMask = toMask(T2[one.body]);
                assert(twoMask.isIn(two));
                // count all higher elements
                const hiThree = twoMask.higherThan(two).?;
                const threeOff = if (one.body < FOUR_MAX - 1)
                    hiThree + popCountSlice(T2[one.body + 1 ..])
                else
                    hiThree;
                const three = codeunit(sieve[idx + 2]);
                const threeMask = toMask(T3[threeOff]);
                assert(threeMask.isIn(three));
                const fourLow = threeMask.lowerThan(three).?;
                const fourOff = if (threeOff == 0)
                    fourLow
                else
                    fourLow + popCountSlice(T3[0..threeOff]);
                if (idx + 3 >= sieve.len) return InvalidUnicode;
                const four = codeunit(sieve[idx + 3]);
                if (four.kind != .follow) return InvalidUnicode;
                var fourMask = toMask(T4[fourOff]);
                fourMask.add(four);
                T4[fourOff] = fourMask.m;
                idx += nB;
            },
        }
    }
    // given that the above is correct, we have validated UTF-8,
    // and added all runes in the string.
    const T2c = compactSlice(&T2);
    // Must be length of lead popcount
    assert(T2c.len == lead.count());
    const T3off = 4 + T2c.len;
    const T4off = header[T4_OFF];
    assert(T4off == T3off + T3.len);
    const setLen = T4off + T4.len;
    assert(header.len + T2c.len + T3.len + T4.len == setLen);
    const setBody = try allocator.alloc(u64, setLen);
    @memcpy(setBody[0..4], &header);
    @memcpy(setBody[4..T3off], T2c);
    @memcpy(setBody[T3off..T4off], T3);
    @memcpy(setBody[T4off..], T4);
    return setBody;
}

/// matchOneDirectly: use a 'raw' RuneSet to match against the
/// beginning of a slice.
///
/// This returns the number of bytes matched, or nothing if invalid
/// Unicode is encountered.  This is opportunistic: if a byte is
/// discovered to not be a member of the set, any further bytes will
/// not be decoded.
///
/// This is safe to use on any []u8, including truncated UTF-8.
///
fn matchOneDirectly(set: []const u64, str: []const u8) ?usize {
    const a = codeunit(str[0]);
    switch (a.kind) {
        .follow => return null,
        .low => {
            const mask = toMask(set[LOW]);
            if (mask.isIn(a))
                return 1
            else
                return 0;
        },
        .hi => {
            const mask = toMask(set[HI]);
            if (mask.isIn(a))
                return 1
            else
                return 0;
        },
        .lead => {
            const nB = a.nMultiBytes() orelse return null;
            assert(nB > 1);
            if (nB > str.len) return null;
            const a_mask = toMask(set[LEAD]);
            if (!a_mask.isIn(a)) return 0;
            const b = codeunit(str[1]);
            if (b.kind != .follow) return null;
            const b_loc = 4 + a_mask.lowerThan(a).?;
            const b_mask = toMask(set[b_loc]);
            if (!b_mask.isIn(b)) return 0;
            if (nB == 2) return 2;
            const t3_off = 4 + @popCount(set[LEAD]);
            const c = codeunit(str[2]);
            if (c.kind != .follow) return null;
            // Slice is safe because we know the T2 span has at least one word
            const c_off = b_mask.higherThan(b).? + popCountSlice(set[b_loc + 1 .. t3_off]);
            const c_loc = t3_off + c_off;
            const c_mask = toMask(set[c_loc]);
            if (!c_mask.isIn(c)) return 0;
            if (nB == 3) return 3;
            const d_off = c_mask.lowerThan(c).? + popCountSlice(set[t3_off..c_loc]);
            const d_loc = set[T4_OFF] + d_off;
            const d = codeunit(str[3]);
            if (d.kind != .follow) return null;
            const d_mask = toMask(set[d_loc]);
            if (d_mask.isIn(d)) return 4 else return 0;
        },
    }
}

/// Match one codepoint against the set, returning the number of bytes
/// matched.  This performs no validation, meaning that invalid unicode
/// can return bogus results.  Truncated UTF-8 at the end of a buffer
/// *will* read beyond valid memory in fast release modes.  This
/// function is, however safe to call with a follow byte at [0].
fn matchOneDirectAssumeValid(set: []const u64, str: []const u8) usize {
    const a = codeunit(str[0]);
    switch (a.kind) {
        .follow => return 0,
        .low => {
            const mask = toMask(set[LOW]);
            if (mask.isIn(a))
                return 1
            else
                return 0;
        },
        .hi => {
            const mask = toMask(set[HI]);
            if (mask.isIn(a))
                return 1
            else
                return 0;
        },
        .lead => {
            const nB = a.nMultiBytes().?;
            assert(nB > 1);
            assert(nB <= str.len);
            const a_mask = toMask(set[LEAD]);
            if (!a_mask.isIn(a)) return 0;
            const b = codeunit(str[1]);
            assert(b.kind == .follow);
            const b_loc = 4 + a_mask.lowerThan(a).?;
            const b_mask = toMask(set[b_loc]);
            if (!b_mask.isIn(b)) return 0;
            if (nB == 2) return 2;
            const t3_off = 4 + @popCount(set[LEAD]);
            const c = codeunit(str[2]);
            assert(c.kind == .follow);
            // Slice is safe because we know the T2 span has at least one word
            const c_off = b_mask.higherThan(b).? + popCountSlice(set[b_loc + 1 .. t3_off]);
            const c_loc = t3_off + c_off;
            const c_mask = toMask(set[c_loc]);
            if (!c_mask.isIn(c)) return 0;
            if (nB == 3) return 3;
            const d_off = c_mask.lowerThan(c).? + popCountSlice(set[t3_off..c_loc]);
            const d_loc = set[T4_OFF] + d_off;
            const d = codeunit(str[3]);
            assert(d.kind == .follow);
            const d_mask = toMask(set[d_loc]);
            if (d_mask.isIn(d)) return 4 else return 0;
        },
    }
}

/// Count non-zero members of word slice
inline fn nonZeroCount(words: []const u64) usize {
    var w: usize = 0;
    for (words) |word| {
        if (word != 0)
            w += 1;
    }
    return w;
}

/// sum of @popCount of all words in region.
fn popCountSlice(region: []const u64) usize {
    var ct: usize = 0;
    for (region) |w| ct += @popCount(w);
    return ct;
}

test popCountSlice {
    const region: [4]u64 = .{ 0, 0, 1, 3 };
    // check that empty slice returns 0
    try expectEqual(0, popCountSlice(region[2..2]));
    try expectEqual(3, popCountSlice(&region));
}

/// Remove all zero elements from a slice, returning the now-compacted slice.
/// The pointer of the new slice will be the same.  Caller remains responsible
/// for the original slice's full memory region.
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

/// Make a mutable copy of a constant string.
fn makeMutable(s: []const u8, a: Allocator) ![]u8 {
    const mut = try a.alloc(u8, s.len);
    @memcpy(mut, s);
    return mut;
}

test compactSlice {
    var arr: [6]u64 = .{ 0, 1, 2, 0, 1, 0 };
    const smol = compactSlice(arr[0..]);
    try expectEqual(3, smol.len);
    try expectEqual(1, smol[0]);
    try expectEqual(2, smol[1]);
    try expectEqual(1, smol[2]);
}

const testing = std.testing;
const expect = testing.expect;
const expectEqual = testing.expectEqual;
