//! libruneset: fast utf8 codepoint sets for Zig
//!
//! Provides tools for working with sets of Unicode codepoints,
//! also known as characters or runes.
//!
//! The primary type is RuneSets, which may be constructed in
//! several ways.  These support very fast membership tests
//! through bitmasks and popcount, and are capable to encoding
//! arbitrary sets of characters.
//!
//! They (will) also allow nondestructive set operations: union, set,
//! and difference.  Probably subset, equality, and iteration.

const std = @import("std");
const Allocator = std.mem.Allocator;
const DynamicBitSetUnmanaged = std.bit_set.DynamicBitSetUnmanaged;
const assert = std.debug.assert;
const elements = @import("elements.zig");

const Mask = elements.Mask;
const toMask = Mask.toMask;
/// A wrapper for a single byte of utf8
pub const CodeUnit = elements.CodeUnit;
/// codeunit(:u8) creates a CodeUnit
pub const codeunit = elements.codeunit;

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
// const MASK_IN_FOUR: u64 = codeunit(47).lowMask();
const MASK_OUT_FOUR: u64 = codeunit(THREE_MAX).hiMask();
// 32..56
const MASK_IN_THREE: u64 = MASK_OUT_TWO | MASK_OUT_FOUR;

/// RuneSet: a fast character set for UTF-8.
///
/// Create with:
///
/// - RuneSet.createFromMutableString
/// - RuneSet.createFromConstString
/// - ...
///
/// Free with `set.deinit(allocator)`.
pub const RuneSet = struct {
    body: []const u64,

    // some names for meaningful operations
    inline fn leadMask(self: *const RuneSet) Mask {
        return toMask(self.body[LEAD]);
    }

    inline fn t2Len(self: *const RuneSet) usize {
        return @popCount(self.body[LEAD]);
    }

    inline fn t2start(_: *const RuneSet) usize {
        return 4;
    }

    inline fn t2end(self: *const RuneSet) usize {
        return self.t2start() + self.t2Len();
    }

    // Start of region of T2 containing 3 byte b codeunits
    // inline fn t2_3b_start(self: *const RuneSet) usize {
    //     return 4 + @popCount(self.body[LEAD] & MASK_IN_TWO);
    // }

    // Start of region of T2 containing 4 byte b codeunits
    inline fn t2_4b_start(self: *const RuneSet) usize {
        return 4 + @popCount(self.body[LEAD] & MASK_OUT_FOUR);
    }

    // Start of region of T3 containing 3 byte c codeunits
    inline fn t3_3c_start(self: *const RuneSet) usize {
        const c4b_off = popCountSlice(self.body[self.t2_4b_start()..self.t2end()]);
        return self.t3start() + c4b_off;
    }

    // Start of T3 region
    inline fn t3start(self: *const RuneSet) usize {
        return self.t2end();
    }

    // End of T3 region
    inline fn t3end(self: *const RuneSet) usize {
        if (self.body[T4_OFF] == 0)
            return self.body.len
        else
            return self.body[T4_OFF];
    }

    inline fn t4offset(self: *const RuneSet) usize {
        return self.body[T4_OFF];
    }

    inline fn maskAt(self: *const RuneSet, off: usize) Mask {
        return toMask(self.body[off]);
    }

    // Debugging
    fn debugMaskAt(self: *const RuneSet, off: usize) void {
        std.debug.print("0x{x:0>16}\n", .{self.body[off]});
    }

    // No need for noTwoBytes because LEAD is present
    // in all cases, so testing a lead byte is always
    // safe

    inline fn noThreeBytes(self: *const RuneSet) bool {
        return self.body[LEAD] & MASK_OUT_TWO == 0;
    }

    inline fn noFourBytes(self: *const RuneSet) bool {
        return self.body[T4_OFF] == 0;
    }

    fn spreadT2(self: *const RuneSet, T2: []u64) void {
        var iter = self.maskAt(LEAD).iterElements();
        const body = self.body;
        while (iter.next()) |e| {
            T2[e] = body[4 + e];
        }
    }

    pub fn deinit(self: *const RuneSet, alloc: Allocator) void {
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
    /// Delete by passing the same allocator to `set.deinit(allocator)`.
    pub fn createFromConstString(str: []const u8, allocator: Allocator) !RuneSet {
        const s_mut = try makeMutable(str, allocator);
        defer allocator.free(s_mut);
        return try RuneSet.createFromMutableString(s_mut, allocator);
    }

    /// Match one rune at the beginning of the slice.
    ///
    /// This is safe to use with invalid UTF-8, and will return null if
    /// such is encountered.
    ///
    /// The normal return value is the number of bytes matched.  Zero
    /// means that the rune beginning the slice was not a match.
    pub fn matchOne(self: *const RuneSet, slice: []const u8) ?usize {
        return matchOneDirectly(self.body, slice);
    }

    /// Match as many runes as possible starting from the beginning of
    /// the slice.  Returns the number of bytes matched.
    ///
    /// Safe to use on invalid UTF-8, returning null if any is found.
    pub fn matchMany(self: *const RuneSet, slice: []const u8) ?usize {
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
    ///
    /// Safe to use on invalid UTF-8, including truncated multi-byte
    /// runes at the end of the slice.
    pub fn matchManyAllowInvalid(self: *RuneSet, slice: []const u8) usize {
        var idx: usize = 0;
        while (idx < slice.len) {
            const nB = self.matchOne(slice[idx..]) orelse 0;
            if (nB == 0)
                return idx;
            idx += nB;
        }
        return idx;
    }

    /// Test if two RuneSets are equal.
    pub fn equalTo(self: *const RuneSet, other: *const RuneSet) bool {
        if (self.body.len != other.body.len) return false;
        var match = true;
        for (self.body, other.body) |l, r| {
            match = match and l == r;
            if (!match) break;
        }
        return match;
    }

    // Return a tuple counting number of one, two, three, and four
    // byte codepoints
    fn counts(self: *const RuneSet) struct { usize, usize, usize, usize } {
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
    pub fn runeCount(self: *const RuneSet) usize {
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

    //| Set Operations
    //|
    //| The receiver is L, the comparator is R, and the new set is N

    /// Union of two RuneSets.
    ///
    pub fn setUnion(L: *const RuneSet, R: *const RuneSet, allocator: Allocator) !RuneSet {
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
            var t2i: usize = 4;
            var rIter = toMask(Rbod[LEAD]).iterElements();
            while (rIter.next()) |rOff| {
                NT2[rOff] |= Rbod[t2i];
                t2i += 1;
            }
        }
        if (L.noThreeBytes() and R.noThreeBytes()) {
            const T2c = compactSlice(&NT2);
            const Nbod = try allocator.alloc(u64, 4 + T2c.len);
            @memcpy(Nbod[0..4], &header);
            @memcpy(Nbod[4..], T2c);
            return RuneSet{ .body = Nbod };
        }
        const NT3 = try allocator.alloc(u64, popCountSlice(NT2[0..TWO_MAX]));
        defer allocator.free(NT3);
        // TODO we may not need to memset 0 NT3
        // In fact we probably do not. Remove after
        // test coverage anyway.
        @memset(NT3, 0);
        // We need to track ownership of the four-byte NT3 regions.
        // In principle, these could be 8 * 64 in width.  We *could*
        // use a comptime-known value of 512 bits, but this is seriously
        // wasteful for almost all actual RuneSets, we'll get better data
        // locality by heap-allocating a DynamicBitSet.  99% of the time it
        // will be word-backed and live in a register while we're using it.
        //
        // Find four-byte range of NT3 by popcounting NT2
        const NT2b4 = popCountSlice(NT2[THREE_MAX..]);
        var LT3_own = try DynamicBitSetUnmanaged.initEmpty(allocator, NT2b4);
        defer LT3_own.deinit(allocator);
        var RT3_own = try DynamicBitSetUnmanaged.initEmpty(allocator, NT2b4);
        defer RT3_own.deinit(allocator);
        // T3 rank
        {
            // These masks tell us which words in NT2 belong
            // to which sets.
            const L_T2 = toMask(Lbod[LEAD] & ~Rbod[LEAD]);
            const R_T2 = toMask(Rbod[LEAD] & ~Lbod[LEAD]);
            const both_T2 = toMask(Lbod[LEAD] & Rbod[LEAD]);
            // Track progressive offsets into T3s
            var N3i: usize = 0;
            var L3i = L.t3start();
            var R3i = R.t3start();
            var L2off = L.t2end();
            var R2off = R.t2end();
            // Reverse iterate lead mask elements of three and four bytes
            var nT2iter = toMask(header[LEAD] & MASK_OUT_TWO).iterElemBack();
            while (nT2iter.next()) |e| {
                // We only set ownership for four-byte chars
                const set_owned = if (e >= THREE_MAX) true else false;
                if (both_T2.isElem(e)) {
                    // Reverse-iterate the mask and test membership
                    const L_tE = toMask(Lbod[L2off] & ~Rbod[R2off]);
                    const R_tE = toMask(Rbod[R2off] & ~Lbod[R2off]);
                    L2off -= 1;
                    R2off -= 1;
                    const both_tE = toMask(Lbod[L2off] & Rbod[R2off]);
                    var elemIter = toMask(NT2[e]).iterElemBack();
                    while (elemIter.next()) |ee| {
                        if (both_tE.isElem(ee)) {
                            NT3[N3i] = Lbod[L3i] | Rbod[R3i];
                            L3i += 1;
                            R3i += 1;
                            if (set_owned) {
                                LT3_own.set(N3i);
                                RT3_own.set(N3i);
                            }
                            N3i += 1;
                        } else if (L_tE.isElem(ee)) {
                            NT3[N3i] = Lbod[L3i];
                            L3i += 1;
                            if (set_owned) {
                                LT3_own.set(N3i);
                            }
                            N3i += 1;
                        } else if (R_tE.isElem(ee)) {
                            NT3[N3i] = Rbod[R3i];
                            R3i += 1;
                            if (set_owned) {
                                RT3_own.set(N3i);
                            }
                            N3i += 1;
                        } else unreachable;
                    }
                } else if (L_T2.isElem(e)) {
                    L2off -= 1;
                    const pc = @popCount(NT2[e]);
                    assert(pc > 0);
                    for (0..pc) |_| {
                        NT3[N3i] = Lbod[L3i];
                        if (set_owned) {
                            LT3_own.set(N3i);
                        }
                        N3i += 1;
                        L3i += 1;
                    }
                } else if (R_T2.isElem(e)) {
                    R2off -= 1;
                    const pc = @popCount(NT2[e]);
                    assert(pc > 0);
                    for (0..pc) |_| {
                        NT3[N3i] = Rbod[L3i];
                        if (set_owned) {
                            RT3_own.set(N3i);
                        }
                        N3i += 1;
                        R3i += 1;
                    }
                } else unreachable;
            } // Sanity checks:
            assert(L2off == T4_OFF + @popCount(Lbod[LEAD] & MASK_IN_TWO));
            assert(R2off == T4_OFF + @popCount(Rbod[LEAD] & MASK_IN_TWO));
            assert(L3i == L.t3end());
            assert(R3i == R.t3end());
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
        const NT4len = popCountSlice(NT3[0..NT2b4]);
        // - allocate NT4
        const NT4 = try allocator.alloc(u64, NT4len);
        defer allocator.free(NT4);
        // TODO again, shouldn't need this
        @memset(NT4, 0);
        // T4 rank
        {
            // We've tracked ownership of NT3, now we need the offset
            // where c bytes of four-byte runes live
            // The +1 is so we don't go under zero when reverse iterating
            var NT3i = popCountSlice(NT2[THREE_MAX..]) + 1;
            // calculate intersection of ownership
            var bothT3_own = try LT3_own.clone(allocator);
            defer bothT3_own.deinit(allocator);
            bothT3_own.setIntersection(RT3_own);
            const LT3off = L.t3end();
            const RT3off = R.t3end();
            var NT4i: usize = 0;
            var LT4i = L.t4offset();
            var RT4i = R.t4offset();
            while (NT3i > 0) : (NT3i -= 1) {
                const i = NT3i - 1;
                if (bothT3_own.isSet(i)) {
                    const LT3m = toMask(Lbod[LT3off + i]);
                    const RT3m = toMask(Rbod[RT3off + i]);
                    var NT3mIter = toMask(NT3[i]).iterElemBack();
                    while (NT3mIter.next()) |e| {
                        if (LT3m.isElem(e) and RT3m.isElem(e)) {
                            NT4[NT4i] = Lbod[LT4i] | Rbod[RT4i];
                            LT4i += 1;
                            RT4i += 1;
                        } else if (LT3m.isElem(e)) {
                            NT4[NT4i] = Lbod[LT4i];
                            LT4i += 1;
                        } else if (RT3m.isElem(e)) {
                            NT4[NT4i] = Rbod[RT4i];
                        } else unreachable;
                        NT4i += 1;
                    } // TODO These are cheaper to do with a popcount
                    // we don't actually need or use the elements here
                } else if (LT3_own.isSet(i)) {
                    var NT3mIter = toMask(NT3[i]).iterElemBack();
                    while (NT3mIter.next()) |_| {
                        NT4[NT4i] = Lbod[LT4i];
                        LT4i += 1;
                        NT4i += 1;
                    }
                } else if (RT3_own.isSet(i)) {
                    var NT3mIter = toMask(NT3[i]).iterElemBack();
                    while (NT3mIter.next()) |_| {
                        NT4[NT4i] = Rbod[RT4i];
                        NT4i += 1;
                        RT4i += 1;
                    }
                }
            }
        } // end T4 rank
        // Copy and return!
        // Same stub different fn
        return RuneSet{ .body = &header };
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
    header[LOW] = low.m;
    header[HI] = hi.m;
    if (lead.count() == 0) { // set was ASCII-only
        assert(sieve.len == back);
        const memHeader = try allocator.alloc(u64, 4);
        @memcpy(memHeader, &header);
        return memHeader;
    }
    // std.debug.print("str after sieve:\n{s}\n", .{sieve});
    sieve = sieve[0 .. sieve.len - back];
    // std.debug.print("str after pass one:\n{s}\n", .{sieve});
    // sieve for second bytes
    var T2: [FOUR_MAX]u64 = .{0} ** FOUR_MAX; // Masks for all second bytes.
    idx = 0;
    back = 0;
    while (idx < sieve.len) {
        const cu = codeunit(sieve[idx]);
        switch (cu.kind) {
            .low, .hi, .follow => return InvalidUnicode,
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
            // Set may not contain any three bytes:
            if (nB == 3 and MASK_OUT_TWO & set[LEAD] == 0) return 0;
            // Or any four bytes:
            if (nB == 4 and set[T4_OFF] == 0) return 0;
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

fn makeMutable(s: []const u8, a: Allocator) ![]u8 {
    const mut = try a.alloc(u8, s.len);
    @memcpy(mut, s);
    return mut;
}

/// Build a "raw" set out of a known-good string, then
/// test membership of every character in the set
///
/// "known good" means the string must be valid utf-8.
fn buildAndTestString(s: []const u8, alloc: Allocator) !void {
    const str = try makeMutable(s, alloc);
    defer alloc.free(str);
    const set = try createBodyFromString(str, alloc);
    defer alloc.free(set);
    var idx: usize = 0;
    while (idx < s.len) {
        const slice = s[idx..];
        const nB = codeunit(s[idx]).nBytes() orelse unreachable;
        const nMatch = matchOneDirectly(set, slice) orelse unreachable;
        expectEqual(nMatch, nB) catch |err| {
            std.log.err("not a valid match at index {d}", .{idx});
            return err;
        };
        idx += nB;
    }
}

// All runes in str must be unique for this test to pass.
fn buildAndTestRuneSet(str: []const u8, alloc: Allocator) !void {
    const set = try RuneSet.createFromConstString(str, alloc);
    defer set.deinit(alloc);
    const matched = set.matchMany(str);
    if (matched) |m| {
        try expectEqual(str.len, m);
        try expectEqual(str.len, set.codeunitCount());
    } else try expect(false);
}

fn buildAndTestUnion(a: []const u8, b: []const u8, alloc: Allocator) !void {
    const setA = try RuneSet.createFromConstString(a, alloc);
    defer setA.deinit(alloc);
    const setB = try RuneSet.createFromConstString(b, alloc);
    defer setB.deinit(alloc);
    const setU = try setA.setUnion(&setB, alloc);
    defer setU.deinit(alloc);
    try expect(true);
}

// Test strings

const alphabet = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz";
const greek = "ΑΒΓΔΕΖΗΘΙΚΛΜΝΞΟΠΡΣΤΥΦΧΨΩαβγδεζηθικλμνξοπρςστυφχψω";
const alfagreek = alphabet ++ greek;

const math = "∀∁∂∃∄∅∆∇∈∉∊∋∌∍∎∏∐∑−∓∔∕∖∗∘∙√∛∜∝∞∟∠∡∢∣∤∥∦∧∨∩∪∫∬∭∮∯∰∱∲∳∴∵∶∷∸∹∺∻∼∽∾∿≀≁≂≃≄≅≆≇≈≉≊≋≌≍≎≏≐≑≒≓≔≕≖≗≘≙≚≛≜≝≞≟≠≡≢≣≤≥≦≧≨≩≪≫≬≭≮≯≰≱≲≳≴≵≶≷≸≹≺≻≼≽≾≿⊀⊁⊂⊃⊄⊅⊆⊇⊈⊉⊊⊋⊌⊍⊎⊏⊐⊑⊒⊓⊔⊕⊖⊗⊘⊙⊚⊛⊜⊝⊞⊟⊠⊡⊢⊣⊤⊥⊦⊧⊨⊩⊪⊫⊬⊭⊮⊯⊰⊱⊲⊳⊴⊵⊶⊷⊸⊹⊺⊻⊼⊽⊾⊿⋀⋁⋂⋃⋄⋅⋆⋇⋈⋉⋊⋋⋌⋍⋎⋏⋐⋑⋒⋓⋔⋕⋖⋗⋘⋙⋚⋛⋜⋝⋞⋟⋠⋡⋢⋣⋤⋥⋦⋧⋨⋩⋪⋫⋬⋭⋮⋯⋰⋱⋲⋳⋴⋵⋶⋷⋸⋹⋺⋻⋼⋽⋾⋿";

const deseret = "𐐀𐐁𐐂𐐃𐐄𐐅𐐆𐐇𐐈𐐉𐐊𐐋𐐌𐐍𐐎𐐏𐐐𐐑𐐒𐐓𐐔𐐕𐐖𐐗𐐘𐐙𐐚𐐛𐐜𐐝𐐞𐐟𐐠𐐡𐐢𐐣𐐤𐐥𐐦𐐧𐐨𐐩𐐪𐐫𐐬𐐭𐐮𐐯𐐰𐐱𐐲𐐳𐐴𐐵𐐶𐐷𐐸𐐹𐐺𐐻𐐼𐐽𐐾𐐿𐑀𐑁𐑂𐑃𐑄𐑅𐑆𐑇𐑈𐑉𐑊𐑋𐑌𐑍𐑎𐑏";

const matheret = math ++ deseret;
const maxsyma = alfagreek ++ math ++ deseret;

test "create body and match strings" {
    const allocator = std.testing.allocator;
    try buildAndTestString(alphabet, allocator);
    try buildAndTestString(greek, allocator);
    try buildAndTestString(alfagreek, allocator);
    try buildAndTestString(math, allocator);
    try buildAndTestString(deseret, allocator);
    try buildAndTestString(maxsyma, allocator);
}

test "create set and match strings" {
    const allocator = std.testing.allocator;
    try buildAndTestRuneSet(alphabet, allocator);
    try buildAndTestRuneSet(greek, allocator);
    try buildAndTestRuneSet(alfagreek, allocator);
    try buildAndTestRuneSet(math, allocator);
    try buildAndTestRuneSet(matheret, allocator);
    try buildAndTestRuneSet(deseret, allocator);
    try buildAndTestRuneSet(maxsyma, allocator);
}

test "create set unions" {
    const allocator = std.testing.allocator;
    try buildAndTestUnion(alphabet, alfagreek, allocator);
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
    try expect(hi.isIn(codeunit('C')));
    try expect(!hi.isIn(codeunit('a')));
    const some_nums = try makeMutable("02468", allocator);
    defer allocator.free(some_nums);
    const numset = try createBodyFromString(some_nums, allocator);
    defer allocator.free(numset);
    low = toMask(numset[0]);
    try expectEqual(numset[1], 0);
    try expect(low.isIn(codeunit('2')));
    try expect(!low.isIn(codeunit('3')));
}

test "two-byte createBodyFromString" {
    const allocator = std.testing.allocator;
    const greekstr = try makeMutable("ABCDαβγδεζηθικλμνξοπρςστυφχψωEFG", allocator);
    defer allocator.free(greekstr);
    const greekset = try createBodyFromString(greekstr, allocator);
    defer allocator.free(greekset);
    const hi = toMask(greekset[1]);
    try expect(hi.isIn(codeunit('G')));
    const lead = toMask(greekset[2]);
    try expectEqual(greekset.len, 4 + lead.count());
    const alfa = "αψ";
    try expect(lead.isIn(codeunit(alfa[0])));
    try expect(lead.isIn(codeunit(alfa[2])));
}

// Run elements tests as well
test {
    testing.refAllDecls(@This());
}
