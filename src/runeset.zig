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
//! TODO iteration of codeunits, serializing to string, and subset operation.

const std = @import("std");
const Allocator = std.mem.Allocator;
const DynamicBitSetUnmanaged = std.bit_set.DynamicBitSetUnmanaged;
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
const MASK_IN_THREE: u64 = MASK_OUT_TWO | MASK_OUT_FOUR;

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

    pub inline fn t2end(self: RuneSet) usize {
        return self.t2start() + self.t2len();
    }

    // Start of region of T2 containing 3 byte b codeunits
    inline fn t2_3b_start(self: RuneSet) usize {
        return 4 + @popCount(self.body[LEAD] & MASK_IN_TWO);
    }

    // Start of region of T2 containing 4 byte b codeunits
    inline fn t2_4b_start(self: RuneSet) usize {
        return 4 + @popCount(self.body[LEAD] & MASK_OUT_FOUR);
    }

    // Start of region of T3 containing 3 byte c codeunits
    inline fn t3_3c_start(self: RuneSet) usize {
        const c4b_off = popCountSlice(self.body[self.t2_4b_start()..self.t2end()]);
        return self.t3start() + c4b_off;
    }

    // Start of T3 region (also t3_3d_start)
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

    // Start of T4 region, *or* zero for no T4
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

    /// Test if two RuneSets are equal.
    pub fn equalTo(self: RuneSet, other: RuneSet) bool {
        if (self.body.len != other.body.len) return false;
        for (self.body, other.body) |l, r| {
            if (l != r) return false;
        }
        return true;
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

    // TODO add RuneSet iterator and runeset.toString(buf)

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
    pub fn setUnion(L: RuneSet, R: RuneSet, allocator: Allocator) !RuneSet {
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
        const NT3 = try allocator.alloc(u64, popCountSlice(NT2[TWO_MAX..]));
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

        const NT2b4len = popCountSlice(NT2[THREE_MAX..]);
        var LT3_own = try DynamicBitSetUnmanaged.initEmpty(allocator, NT2b4len * 64);
        defer LT3_own.deinit(allocator);
        var RT3_own = try DynamicBitSetUnmanaged.initEmpty(allocator, NT2b4len * 64);
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
            var L2off = L.t2end() - 1;
            var R2off = R.t2end() - 1;
            // Reverse iterate lead mask elements of three and four bytes
            var nT2iter = toMask(header[LEAD] & MASK_OUT_TWO).iterElemBack();
            while (nT2iter.next()) |e2| {
                // We only set ownership for four-byte chars
                const set_owned = if (e2 >= THREE_MAX) true else false;
                if (both_T2.isElem(e2)) {
                    // Reverse-iterate the mask and test membership
                    const L_tE = toMask(Lbod[L2off] & ~Rbod[R2off]);
                    const R_tE = toMask(Rbod[R2off] & ~Lbod[R2off]);
                    const both_tE = toMask(Lbod[L2off] & Rbod[R2off]);
                    L2off -= 1;
                    R2off -= 1;
                    var elemIter = toMask(NT2[e2]).iterElemBack();
                    while (elemIter.next()) |e3| {
                        if (both_tE.isElem(e3)) {
                            NT3[N3i] = Lbod[L3i] | Rbod[R3i];
                            L3i += 1;
                            R3i += 1;
                            if (set_owned) {
                                LT3_own.set(N3i);
                                RT3_own.set(N3i);
                            }
                        } else if (L_tE.isElem(e3)) {
                            NT3[N3i] = Lbod[L3i];
                            L3i += 1;
                            if (set_owned) {
                                LT3_own.set(N3i);
                            }
                        } else if (R_tE.isElem(e3)) {
                            NT3[N3i] = Rbod[R3i];
                            R3i += 1;
                            if (set_owned) {
                                RT3_own.set(N3i);
                            }
                        } else unreachable;
                        N3i += 1;
                    }
                } else if (L_T2.isElem(e2)) {
                    L2off -= 1;
                    const pc = @popCount(NT2[e2]);
                    assert(pc > 0);
                    for (0..pc) |_| {
                        NT3[N3i] = Lbod[L3i];
                        if (set_owned) {
                            LT3_own.set(N3i);
                        }
                        N3i += 1;
                        L3i += 1;
                    }
                } else if (R_T2.isElem(e2)) {
                    R2off -= 1;
                    const pc = @popCount(NT2[e2]);
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
        const NT4len = popCountSlice(NT3[0..NT2b4len]);
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
            const LT3off = L.t3start();
            const RT3off = R.t3start();
            var NT4i: usize = 0;
            var LT4i = L.t4offset();
            var RT4i = R.t4offset();
            while (NT3i > 0) : (NT3i -= 1) {
                const i = NT3i - 1;
                if (LT3_own.isSet(i) and RT3_own.isSet(i)) {
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
                            RT4i += 1;
                        } else unreachable;
                        NT4i += 1;
                    }
                } else if (LT3_own.isSet(i)) {
                    const NT3mIter = @popCount(NT3[i]);
                    for (0..NT3mIter) |_| {
                        NT4[NT4i] = Lbod[LT4i];
                        LT4i += 1;
                        NT4i += 1;
                    }
                } else if (RT3_own.isSet(i)) {
                    const NT3mIter = @popCount(NT3[i]);
                    for (0..NT3mIter) |_| {
                        NT4[NT4i] = Rbod[RT4i];
                        NT4i += 1;
                        RT4i += 1;
                    }
                }
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
    pub fn setDifference(L: RuneSet, R: RuneSet, allocator: Allocator) !RuneSet {
        var header: [4]u64 = undefined;
        const Lbod = L.body;
        const Rbod = R.body;
        header[LOW] = Lbod[LOW] & ~Rbod[LOW];
        header[HI] = Lbod[HI] & ~Rbod[HI];
        // We always assign this to header[LEAD] before returning
        var LLeadMask = toMask(Lbod[LEAD]);
        header[T4_OFF] = 0;
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
        // Take an intersection of LEADS
        const commonLead = toMask(Rbod[LEAD] & Lbod[LEAD]);
        {
            // Iterate and diff
            var RT2i = R.t2start();
            var T2iter = commonLead.iterElements();
            while (T2iter.next()) |e| {
                if (e >= TWO_MAX) break;
                assert(NT2[e] != 0);
                NT2[e] &= ~Rbod[RT2i];
                RT2i += 1;
                // If this clears the mask, remove the LEAD bit
                if (NT2[e] == 0)
                    LLeadMask.remove(codeunit(e));
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
        // Once again, R 4 byte is irrelevant if L has none
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
        // Same deal: Iterate d byte region of T2,
        // find corresponding regions of LT3 and RT3,
        // diff overlaps, remove nulls all the way up to LLeadMask
        //
        // While this may read much like a clone of the Tier 3 logic,
        // it is not.  Tier 3 did T2 backward, and T3 forward.  Tier 4
        // does T2 *and* T3 backward, and T4 forward.
        const LT4 = L.t4slice().?; // checked above
        const NT4 = try allocator.alloc(u64, LT4.len);
        defer allocator.free(NT4);
        @memcpy(NT4, LT4);
        if (!R.noFourBytes()) {
            // Backward iterate T2, d region only
            // TODO use a mask here and drop the conditional in the while loop
            var unionLeadIter = toMask(Lbod[LEAD] | Rbod[LEAD]).iterElemBack();
            // Track ownership of T2 bits on each side:
            const LT2m = toMask(Lbod[LEAD]);
            const RT2m = toMask(Rbod[LEAD]);
            // Track progress backward through T3 of both sides
            var NT3i = popCountSlice(NT2[THREE_MAX..]) - 1;
            var RT3i = R.t3_3c_start() - 1;
            // c-byte region of NT2 is all from L2, but RT2 offset needs tracking
            var RT2i = R.t3start() - 1;
            // track T4 progress
            var NT4i: usize = 0;
            var RT4i = Rbod[T4_OFF];
            while (unionLeadIter.next()) |e2| {
                if (e2 < THREE_MAX)
                    break;
                // in d region of T2
                const inLT2 = LT2m.isElem(e2);
                if (!inLT2) {
                    // must be in R, we skip
                    assert(RT2m.isElem(e2));
                    assert(Rbod[RT2i] != 0);
                    // both iterations are backward!
                    RT3i -= @popCount(Rbod[RT2i]);
                    RT2i -= 1;
                    continue;
                }
                const inRT2 = RT2m.isElem(e2);
                if (inLT2 and inRT2) {
                    // Find T3 overlaps and apply T4 accordingly
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
                            // we iterate the union mask backward,
                            // advancing T4 cursors. Intersections are diffed,
                            // zeroed words remove that NT3 bit.
                            assert(NT3[NT3i] != 0);
                            const eLT3m = toMask(NT3[NT3i]);
                            assert(Rbod[RT3i] != 0);
                            const eRT3m = toMask(Rbod[RT3i]);
                            var unionT3iter = toMask(eLT3m.m | eRT3m.m).iterElemBack();
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
                            // finished iterating T3 mask, subtract indices
                            if (NT3i > 0)
                                // if it is zero, we won't have further matches
                                NT3i -= 1;
                            RT3i -= 1;
                        } else if (eRT2m.isElem(e3)) {
                            RT3i -= 1;
                        } else {
                            assert(eLT2m.isElem(e3));
                            NT3i -= 1;
                        }
                    }
                    // Check for emptied NT2 mask
                    if (NT2[e2] == 0) {
                        LLeadMask.remove(codeunit(e2));
                    }
                } else { // Must be L2, subtract mask from LT3i
                    assert(inLT2);
                    NT3i -= @popCount(NT2[e2]);
                }
            }
        } else {
            assert(R.t4slice() == null);
        }
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
    pub fn setIntersection(L: RuneSet, R: RuneSet, allocator: Allocator) !RuneSet {
        // Similar to setDifference.  A bit simpler because we can
        // remove bits as we find them (but still have to go back
        // and clear more bits when we do each tier)
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
                } else if (RLeadMask.isElem(e2)) {
                    RT2i += 1;
                } else unreachable;
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
        @memset(NT3, 0); // TODO again, shouldn't don't need this
        defer allocator.free(NT3);
        {
            // We iterate back through both T2s to find surviving T3s
            var unionT2iter = toMask((Rbod[LEAD] & MASK_OUT_TWO) | (Lbod[LEAD] & MASK_OUT_TWO)).iterElemBack();
            var RT2i = R.t2end() - 1;
            var LT2i = L.t2end() - 1;
            var LT3i = L.t3start();
            var RT3i = R.t3start();
            var NT3i: usize = 0;
            while (unionT2iter.next()) |e2| {
                if (NT2[e2] != 0) {
                    const eLT2m = toMask(Lbod[LT2i]);
                    const eRT2m = toMask(Rbod[RT2i]);
                    var eNT2m = toMask(NT2[e2]);
                    var unionT3Iter = eLT2m.setunion(eRT2m).iterElemBack();
                    while (unionT3Iter.next()) |e3| {
                        if (eLT2m.isElem(e3) and eRT2m.isElem(e3)) {
                            NT3[NT3i] = Rbod[RT3i] & Lbod[LT3i];
                            RT3i += 1;
                            LT3i += 1;
                            if (NT3[NT3i] == 0) {
                                eNT2m.remove(codeunit(e3));
                            }
                            NT3i += 1;
                        } else if (eLT2m.isElem(e3)) {
                            LT3i += 1;
                        } else {
                            assert(eRT2m.isElem(e3));
                            RT3i += 1;
                        }
                    }
                    NT2[e2] = eNT2m.m;
                    if (NT2[e2] == 0)
                        NLeadMask.remove(codeunit(e2));
                }
                // subtract T2 pointers
                if (RLeadMask.isElem(e2))
                    RT2i -= 1;
                if (LLeadMask.isElem(e2))
                    LT2i -= 1;
            } // sanity checks:
            assert(LT3i == L.t3end());
            assert(RT3i == R.t3end());
            assert(LT2i == 3 + @popCount(Lbod[LEAD] & MASK_IN_TWO));
            assert(RT2i == 3 + @popCount(Rbod[LEAD] & MASK_IN_TWO));
        }
        if (NLeadMask.m & MASK_IN_FOUR == 0) {
            header[LEAD] = NLeadMask.m;
            const T2c = compactSlice(&NT2);
            const T2end = 4 + T2c.len;
            const T3c = compactSlice(NT3);
            const setLen = T2end + T3c.len;
            const Nbod = try allocator.alloc(u64, setLen);
            @memcpy(Nbod[0..4], &header);
            @memcpy(Nbod[4..T2end], T2c);
            @memcpy(Nbod[T2end..setLen], T3c);
            return RuneSet{ .body = Nbod };
        }
        // Tier 4
        // NT4 is, at most, as many words as the d block of NT3
        const NT3_d_len = popCountSlice(NT2[THREE_MAX..]);
        const NT4len = popCountSlice(NT3[0..NT3_d_len]);
        const NT4 = try allocator.alloc(u64, NT4len);
        defer allocator.free(NT4);
        // TODO defensive:
        @memset(NT4, 0);
        // We follow the now familiar strategy of iterating across
        // unions of the set masks, and following through on the
        // intersections.
        { // In which we track Everything:
            // backward through T2s
            var RT2i = R.t2end() - 1;
            var LT2i = L.t2end() - 1;
            // backward through d bytes of T3
            var NT3i = NT3_d_len - 1;
            var LT3i = L.t3_3c_start() - 1;
            var RT3i = R.t3_3c_start() - 1;
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
                if (NT2[e2] != 0) {
                    var eNT2m = toMask(NT2[e2]);
                    const eLT2m = toMask(Lbod[LT2i]);
                    const eRT2m = toMask(Rbod[RT2i]);
                    var unionT3iter = eLT2m.setunion(eRT2m).iterElemBack();
                    tier3: while (unionT3iter.next()) |e3| {
                        if (eLT2m.isElem(e3) and eRT2m.isElem(e3)) {
                            // We may have knocked out some of this region already:
                            var eNT3m = toMask(NT3[NT3i]);
                            const eLT3m = toMask(Lbod[LT3i]);
                            const eRT3m = toMask(Rbod[RT3i]);
                            LT3i -= 1;
                            RT3i -= 1;
                            if (eNT3m.m == 0) {
                                // corollary: NT2m already removed
                                assert(!eNT2m.isElem(e3));
                                continue :tier3;
                            }
                            var unionT4iter = blk: {
                                break :blk eLT3m.setunion(eRT3m)
                                    .iterElemBack();
                            };
                            while (unionT4iter.next()) |e4| {
                                if (eLT3m.isElem(e4) and eRT3m.isElem(e4)) {
                                    NT4[NT4i] = Lbod[LT4i] & Rbod[RT4i];
                                    if (NT4[NT4i] == 0) {
                                        eNT3m.remove(codeunit(e4));
                                    }
                                    NT4i += 1;
                                    LT4i += 1;
                                    RT4i += 1;
                                } else if (eLT3m.isElem(e4)) {
                                    LT4i += 1;
                                } else {
                                    assert(eRT3m.isElem(e4));
                                    RT4i += 1;
                                }
                            }
                            NT3[NT3i] = eNT3m.m;
                            if (eNT3m.m == 0) {
                                eNT2m.remove(codeunit(e3));
                            }
                            if (NT3i > 0)
                                NT3i -= 1;
                        } else if (eLT2m.isElem(e3)) {
                            LT3i -= 1;
                        } else {
                            assert(eRT2m.isElem(e3));
                            RT3i -= 1;
                        }
                    } // end T3 union iteration
                    NT2[e2] = eNT2m.m;
                    if (NT2[e2] == 0)
                        NLeadMask.remove(codeunit(e2));
                } // end T2 intersection, subtract T2 indices
                if (RLeadMask.isElem(e2))
                    RT2i -= 1;
                if (LLeadMask.isElem(e2))
                    LT2i -= 1;
            } // end T2 union iteration
        } // end T4 block
        header[LEAD] = NLeadMask.m;
        const T2c = compactSlice(&NT2);
        const T2end = 4 + T2c.len;
        const T3c = compactSlice(NT3);
        const T3end = T2end + T3c.len;
        const T4 = compactSlice(NT4);
        if (T4.len != 0)
            header[T4_OFF] = T3end
        else
            header[T4_OFF] = 0;
        const setLen = T3end + T4.len;
        const Nbod = try allocator.alloc(u64, setLen);
        @memcpy(Nbod[0..4], &header);
        @memcpy(Nbod[4..T2end], T2c);
        @memcpy(Nbod[T2end..T3end], T3c);
        @memcpy(Nbod[T3end..setLen], T4);
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
            if (nB >= 3 and MASK_OUT_TWO & set[LEAD] == 0) return 0;
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
