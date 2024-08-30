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
//! Also provided are the CodeUnit, a convenient packed-struct form of
//! `u8` for representing UTF-8 code units, and Rune, a faster and more
//! flexible way to work with encoded codepoints.

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
// meaning the highest legal body value is 31.
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

/// RuneSet: fast character sets for UTF-8.
///
/// Create with:
///
/// - `RuneSet.createFromMutableString`
/// - `RuneSet.createFromConstString`
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
    //| sort of runes implied by asking about the region in
    //| question.  Some will give useful answers when this is
    //| not the case, but in general, they will not.

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

    /// Start of T3 region.
    inline fn t3start(self: RuneSet) usize {
        return self.t2end();
    }

    /// End of T3 region.
    inline fn t3end(self: RuneSet) usize {
        if (self.body[T4_OFF] == 0)
            return self.body.len
        else
            return self.body[T4_OFF];
    }

    /// Last word (first in lexical order) of
    /// T3 region.
    inline fn t3final(self: RuneSet) usize {
        return self.t3end() - 1;
    }

    /// Start of T4 region, *or* zero for no T4
    inline fn t4offset(self: RuneSet) usize {
        return self.body[T4_OFF];
    }

    /// The slice containing second-byte masks.
    pub inline fn t2slice(self: RuneSet) ?[]const u64 {
        if (self.body[LEAD] == 0)
            return null
        else
            return self.body[4..self.t2end()];
    }

    /// The slice containing third-byte masks.
    pub inline fn t3slice(self: RuneSet) ?[]const u64 {
        if (self.noThreeBytes())
            return null
        else
            return self.body[self.t3start()..self.t3end()];
    }

    /// The slice of T3 containing 3 byte final masks.
    pub inline fn t3_3c_slice(self: RuneSet) ?[]const u64 {
        if (self.noThreeBytes())
            return null
        else
            return self.body[self.t3_3c_start()..self.t3end()];
    }

    /// The slice containing four-byte masks.
    pub inline fn t4slice(self: RuneSet) ?[]const u64 {
        if (self.noFourBytes())
            return null
        else
            return self.body[self.t4offset()..];
    }

    pub inline fn t4bounds_for_c(self: RuneSet, offset: usize) struct { usize, usize } {
        const t4start = self.t4offset() + popCountSlice(self.body[self.t3start()..offset]);
        return .{ t4start, t4start + @popCount(self.body[offset]) };
    }

    /// Return `u4` at offset as a `Mask`.
    inline fn maskAt(self: RuneSet, off: usize) Mask {
        return toMask(self.body[off]);
    }

    // No need for noTwoBytes because LEAD is present
    // in all cases, so testing a lead byte is always
    // safe.

    /// Test whether the `RuneSet` has any three- or four-byte
    /// codepoints.
    inline fn noThreeBytes(self: RuneSet) bool {
        return self.body[LEAD] & MASK_OUT_TWO == 0;
    }

    /// Test whether the `RuneSet` has any four-byte codepoints.
    inline fn noFourBytes(self: RuneSet) bool {
        return self.body[T4_OFF] == 0;
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

    /// Add all mask words of the set's T2 region to a new T2,
    /// which will contain an empty (0) mask for every possible
    /// T2 value.
    fn spreadT2(self: RuneSet, T2: []u64) void {
        var iter = self.maskAt(LEAD).iterElements();
        const body = self.body;
        var off: usize = 4;
        while (iter.next()) |e| {
            T2[e] = body[off];
            off += 1;
        }
    }

    /// Return T2 offset corresponding to the `a` CodeUnit.
    /// `a` must belong to the LEAD mask of the set.
    pub fn t2offsetFor(self: RuneSet, a: CodeUnit) usize {
        assert(a.kind == .lead);
        const a_mask = toMask(self.body[LEAD]);
        assert(a_mask.isIn(a));
        return self.t2start() + a_mask.lowerThan(a).?;
    }

    /// Return T3 offset corresponding to the `b` CodeUnit,
    /// given its T2 offset.  `b` must be in the relevant word,
    /// and must be a component of a multibyte sequence of three or
    /// four bytes in total.
    pub fn t3offsetFor(self: RuneSet, t2off: usize, b: CodeUnit) usize {
        assert(self.t2start() + @popCount(self.body[LEAD] & MASK_IN_TWO) <= t2off);
        assert(t2off < self.t2end());
        assert(b.kind == .follow);
        const b_mask = self.maskAt(t2off);
        assert(b_mask.isIn(b));
        const t3off = self.t3start();
        assert(t3off < self.body.len);
        const c_off = b_mask.higherThan(b).? + popCountSlice(self.body[t2off + 1 .. t3off]);
        return t3off + c_off;
    }

    /// Return T4 offset corresponding to the `c` CodeUnit, given
    /// its T3 offset.  `c.body` must belong to the T3 word.
    pub fn t4offsetFor(self: RuneSet, t3off: usize, c: CodeUnit) usize {
        assert(self.t4offset() != 0);
        assert(self.t3start() <= t3off);
        assert(t3off < self.t3end());
        assert(c.kind == .follow);
        const c_mask = self.maskAt(t3off);
        assert(c_mask.isIn(c));
        const d_off = c_mask.lowerThan(c).? + popCountSlice(self.body[self.t3start()..t3off]);
        assert(self.t4offset() + d_off < self.body.len);
        return self.t4offset() + d_off;
    }

    /// Free the memory allocated for a RuneSet.
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
    /// this value can be obtained by calling `runeset.codeunitCount()`.
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

    /// Serialize a RuneSet to a newly created string.  Caller will own
    /// the allocated memory.
    pub fn toString(self: RuneSet, alloc: Allocator) error{OutOfMemory}![]u8 {
        const buf = try alloc.alloc(u8, self.codeunitCount());
        self.writeToBuffer(buf);
        return buf;
    }

    /// Match one rune at the beginning of the slice.
    /// This is safe to use with invalid UTF-8, and will return `null` if
    /// such is encountered.  The normal return value is the number of bytes
    /// matched.  Zero means that the rune beginning the slice was not a match.
    ///
    /// Note: this function doesn't double as a UTF-8 validator, because it
    /// will return 0 for overlong encodings, and code unit sequences which
    /// represent codepoints which aren't scalar values (surrogates).  It's
    /// more about what it *won't* do, namely, spuriously detect invalid data
    /// as a match, or read past the buffer for a truncated multibyte sequence.
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
    /// valid UTF-8.  It is legal to call this function if the byte at `[0]`
    /// is not the lead byte of a sequence.  Returns number of bytes matched.
    ///
    /// Note: in debug mode, this will panic if it encounters invalid UTF-8.
    /// In release mode, it can return nonsense results, or read beyond the
    /// buffer if called on a truncated codepoint.
    pub fn matchOneAssumeValid(self: RuneSet, slice: []const u8) usize {
        return matchOneDirectAssumeValid(self.body, slice);
    }

    /// Match as many runes as possible starting from the beginning of
    /// the slice.  Returns the number of bytes matched.
    /// Safe to use on invalid UTF-8, returning `null` if any is found.
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

    /// Match a codepoint at slice, returning its lexicographical order
    /// within the set, starting from 0, when a match is made.  Returns
    /// `null` for no match, including ill-formed sequences.
    pub fn ordinalMatch(self: RuneSet, slice: []const u8) ?usize {
        const set = self.body;
        if (slice.len == 0) return null;
        const a = codeunit(slice[0]);
        switch (a.kind) {
            .low => {
                const low_m = self.maskAt(LOW);
                if (low_m.isIn(a)) {
                    return low_m.lowerThan(a);
                } else return null;
            },
            .hi => {
                const hi_m = self.maskAt(HI);
                if (hi_m.isIn(a)) {
                    return @popCount(set[LOW]) + hi_m.lowerThan(a).?;
                } else return null;
            },
            .follow => return null,
            .lead => {
                const nB = a.nMultiBytes() orelse return null;
                if (nB > slice.len) return null;
                const a_mask = self.leadMask();
                if (!a_mask.isIn(a)) return 0;
                const b = codeunit(slice[1]);
                if (b.kind != .follow) return null;
                const t2off = 4 + a_mask.lowerThan(a).?;
                const b_mask = toMask(set[t2off]);
                if (b_mask.isIn(b)) {
                    if (nB == 2) {
                        const a_count = self.countA();
                        return a_count + popCountSlice(set[4..t2off]) + b_mask.lowerThan(b).?;
                    }
                } else return null;
                const c = codeunit(slice[2]);
                const t3off = self.t3offsetFor(t2off, b);
                const c_mask = self.maskAt(t3off);
                if (c_mask.isIn(c)) {
                    if (nB == 3) {
                        const ab_count = self.countA() + self.countB();
                        // This one is a big tricky, we need the three-byte slice:
                        const t3c_slice = self.t3_3c_slice().?;
                        // And the offset one past that:
                        const t3c_off = t3off + 1 - self.t3_3c_start();
                        // Count of bytes /lower/ than c in the mask:
                        const c_off_low = c_mask.lowerThan(c).?;
                        // And the popcount of the rest of the slice (might be zero)
                        const c_after = popCountSlice(t3c_slice[t3c_off..]);
                        return ab_count + c_off_low + c_after;
                    }
                } else return null;
                const d = codeunit(slice[3]);
                const t4off = self.t4offsetFor(t3off, c);
                const d_mask = self.maskAt(t4off);
                if (d_mask.isIn(d)) {
                    const abc_count = self.countA() + self.countB() + self.countC();
                    // This one is a bit tricky because of the zaz-and-zig layout of T4.
                    // First we get the bounds for the `d` region of `c`:
                    const t4c_start, const t4c_end = self.t4bounds_for_c(t3off);
                    // Slice with it:
                    const t4c_slice = set[t4c_start..t4c_end];
                    // Count every d mask of the c bytes lower than our c.
                    // This works because our bounds are based on @popcount(c_mask) and
                    // lowerThan is always a subset of that.
                    const t4_low_count = popCountSlice(t4c_slice[0..c_mask.lowerThan(c).?]);
                    // Everything 'higher' than the end of that region is lower:
                    const t4_hi_count = popCountSlice(set[t4c_end..]);
                    // Append the low count of d itself to our total.
                    return abc_count + t4_low_count + t4_hi_count + d_mask.lowerThan(d).?;
                } else return null;
            },
        }
    }

    /// Test if two RuneSets are equal.
    pub fn equalTo(self: RuneSet, other: RuneSet) bool {
        if (self.body.len != other.body.len) return false;
        for (self.body, other.body) |l, r| {
            if (l != r) return false;
        }
        return true;
    }

    /// A logging equality test, for debugging purposes.
    pub fn expectEqualTo(self: RuneSet, other: RuneSet) bool {
        if (self.body.len != other.body.len) {
            std.debug.print("L.len {d} != R.len {d}\n", .{
                self.body.len,
                other.body.len,
            });
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
    // byte codepoints.
    fn counts(self: RuneSet) struct { usize, usize, usize, usize } {
        var c: [4]u64 = .{0} ** 4;
        c[0] = self.countA();
        if (self.body[LEAD] == 0) return .{ c[0], c[1], c[2], c[3] };
        c[1] = self.countB();
        if (self.noThreeBytes()) return .{ c[0], c[1], c[2], c[3] };
        c[2] = self.countC();
        if (self.noFourBytes()) return .{ c[0], c[1], c[2], c[3] };
        c[3] = self.countD();
        return .{ c[0], c[1], c[2], c[3] };
    }

    inline fn countA(self: RuneSet) usize {
        return popCountSlice(self.body[LOW..LEAD]);
    }

    inline fn countB(self: RuneSet) usize {
        const twosCount: usize = @popCount(self.body[LEAD] & MASK_IN_TWO);
        return popCountSlice(self.body[4 .. 4 + twosCount]);
    }

    inline fn countC(self: RuneSet) usize {
        return popCountSlice(self.body[self.t3_3c_start()..self.t3end()]);
    }

    inline fn countD(self: RuneSet) usize {
        return popCountSlice(self.body[self.t4offset()..]);
    }

    /// Return a count of runes (Unicode codepoints) in set.
    pub fn runeCount(self: RuneSet) usize {
        const a, const b, const c, const d = self.counts();
        return a + b + c + d;
    }

    /// Return a count of codunits (`u8`) in set.
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

    pub fn iterateRunes(self: RuneSet) RuneSetIterator {
        return RuneSetIterator.init(self);
    }

    //| # Set Operations
    //|
    //| Provides union, difference, and intersection operations.
    //|
    //| All such operations are non-destructive, allocating a new set.
    //|
    //| ## Vocabulary
    //|
    //| The variables used here are terse, but follow a consistent syntax.
    //|
    //| The receiver is L, the comparator is R, and the new set is N.
    //|
    //| We refer to the tiers as header (T1), T2, T3, and T4.  These contain
    //| bitmasks for any byte of that order in a codepoint's sequence.  The
    //| first four words of any RuneSet, the header, are LOW, HI, LEAD, and
    //| T4_OFF, so some variables related to a LEAD word will have Lead in
    //| their name.
    //|
    //| We call the codepoints a (not used), b, c, and d, depending on how many
    //| bytes they have in total.  Thus, a d byte of the T2 tier is the second byte
    //| of a four-byte codeunit.  So e.g. 2 is the 2nd byte of any sequence, and b
    //| means that sequence only has two bytes, no matter where we are in said
    //| sequence.
    //|
    //| Abbreviations: i for index, m for mask, e for element, len for length,
    //| iter for an iterator. c at the end of a tier means that tier is compacted.
    //|
    //| Examples: NT3i is an index into the T3 tier of N, our new set.
    //| e2 is an element of some T2, meaning, that T2 will have a mask for
    //| b bytes with e2 as the lead.  NT3_d_len is the length of the d region of
    //| NT3.  RT3m is a mask for element of R's T3, this is created from the
    //| *T2* region of R.
    //|
    //| Note that the violation of usual Zig style here, in which leading capitals
    //| are used only for types, is deliberate.  A capital letter denotes some part
    //| of one of the three sets, while lowercase indicates the aspect of the
    //| algorithm to which it belongs.  This aids legibility in some fiendishly
    //| complex operations, with many moving parts.
    //|
    //| On a similar note, these functions are much longer than typical good style
    //| would indicate.  This is inherent to what they do, it's a lot of code with
    //| an unavoidably repetitive character.  Breaking apart the tiers into sub-
    //| functions would just push that complexity around; all three of these
    //| operations work on each tier, one at a time, with early exits where possible,
    //| the flow being strictly top-to-bottom.  Instead of artificially separating
    //| these tiers into sub-functions, I've chosen to scope them instead.

    /// Union of two `RuneSet`s.
    /// Free returned memory with `set.deinit(allocator)`.
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
        // Tier 2
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
        // Tier 3
        // Each bit of the c and d region of T2 gets a mask:
        const NT3 = try allocator.alloc(u64, popCountSlice(NT2[TWO_MAX..]));
        defer allocator.free(NT3);
        {
            // Track ownership of T2 bitmasks.
            const LT1m = toMask(Lbod[LEAD]);
            const RT1m = toMask(Rbod[LEAD]);
            const bothT1m = toMask(Lbod[LEAD] & Rbod[LEAD]);
            assert(bothT1m.m == LT1m.intersection(RT1m).m);
            // Track T3 offsets forward.
            var NT3i: usize = 0;
            var LT3i = L.t3start();
            var RT3i = R.t3start();
            // Reverse iterate lead mask elements of three and four bytes.
            // We make all masks for T3, so we can use a popcount to
            // allocate T4, if there is one.
            var LT2i = L.t2final();
            var RT2i = R.t2final();
            var unionT2iter = toMask(header[LEAD] & MASK_OUT_TWO).iterElemBack();
            assert(unionT2iter.mask.m == (Lbod[LEAD] & MASK_OUT_TWO) | (Rbod[LEAD] & MASK_OUT_TWO));
            while (unionT2iter.next()) |e2| {
                if (bothT1m.isElem(e2)) {
                    // Reverse-iterate the mask and test membership.
                    const LT2m = toMask(Lbod[LT2i]);
                    const RT2m = toMask(Rbod[RT2i]);
                    const bothT2m = toMask(Lbod[LT2i] & Rbod[RT2i]);
                    LT2i -= 1;
                    RT2i -= 1;
                    var elemIter = toMask(NT2[e2]).iterElemBack();
                    assert(elemIter.mask.m == LT2m.m | RT2m.m);
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
            } // Postconditions:
            assert(LT2i == T4_OFF + @popCount(Lbod[LEAD] & MASK_IN_TWO));
            assert(RT2i == T4_OFF + @popCount(Rbod[LEAD] & MASK_IN_TWO));
            assert(NT3i == NT3.len);
            assert(LT3i == L.t3end());
            assert(RT3i == R.t3end());
        } // end T3 rank
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
        // Popcount of four-byte range of NT3.
        const NT2b4len = popCountSlice(NT2[THREE_MAX..]);
        const NT4len = popCountSlice(NT3[0..NT2b4len]);
        // Is word width of NT4.
        const NT4 = try allocator.alloc(u64, NT4len);
        defer allocator.free(NT4);
        // Tier 4
        {
            // We iterate backward through the header mask, and backward
            // again through the T2 words.  This lets us start at 0 for
            // T3 and T4, going forwards (in reverse *lexical* order) to
            // fill in the union.
            const NT1d_m = toMask(header[LEAD] & MASK_IN_FOUR);
            const LT1d_m = toMask(Lbod[LEAD] & MASK_IN_FOUR);
            const RT1d_m = toMask(Rbod[LEAD] & MASK_IN_FOUR);
            assert(NT1d_m.m == LT1d_m.setunion(RT1d_m).m);
            // T2 offsets are tracked backward.
            var LT2i = L.t2final();
            var RT2i = R.t2final();
            // T3s are tracked forward: highest lead byte to lowest.
            var LT3i = L.t3start();
            var RT3i = R.t3start();
            // Debug tracking NT3, should be elided in release.
            var NT3i: usize = 0;
            // T4 offsets forward: also highest lead to lowest.
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
                        // Shared T2 region.
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
                                // Back iterate both T3 masks, forward.
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
                            } // end T3 iteration
                        } // end T2 iteration
                    } else if (LT1d_m.isElem(e2)) {
                        // Popcount T2 mask.
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
                // Postconditions:
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

    /// Set difference: the receiver, minus the members of the argument.
    /// Free returned memory with `set.deinit(allocator)`.
    pub fn setDifference(L: RuneSet, R: RuneSet, allocator: Allocator) error{OutOfMemory}!RuneSet {
        var header: [4]u64 = undefined;
        const Lbod = L.body;
        const Rbod = R.body;
        header[LOW] = Lbod[LOW] & ~Rbod[LOW];
        header[HI] = Lbod[HI] & ~Rbod[HI];
        // We always assign this to header[LEAD] before returning.
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
        { // Take a union of LEADS.
            const unionLead = toMask(Rbod[LEAD] | Lbod[LEAD]);
            const L1m = toMask(Lbod[LEAD]);
            const R1m = toMask(Rbod[LEAD]);
            // Iterate and diff.
            var RT2i = R.t2start();
            var T2iter = unionLead.iterElements();
            while (T2iter.next()) |e| {
                if (e >= TWO_MAX) break;
                if (R1m.isElem(e)) {
                    NT2[e] &= ~Rbod[RT2i];
                    RT2i += 1;
                    // If this clears the mask, remove the LEAD bit.
                    if (NT2[e] == 0 and L1m.isElem(e))
                        LLeadMask.remove(codeunit(e));
                }
            }
        } // Only L matters here: can't remove what you don't have.
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
            // Track progress forward through T3 of both sides.
            var NT3i: usize = 0; // We've copied over all of LT3 to NT3.
            var RT3i = R.t3start(); // We know there are three-byte seqs in R.
            // c-byte region of NT2 is all from L2, but RT2 offset needs tracking:
            var RT2i = R.t3start() - 1;
            // We go *back* through the c-byte region of NT2, and *forward*
            // through both T3s. To track the latter, we iterate NT2 using
            // the union of both LEADs.
            var unionLeadIter = iter: {
                const L1m = Lbod[LEAD] & MASK_OUT_TWO;
                const R1m = Rbod[LEAD] & MASK_OUT_TWO;
                break :iter toMask(L1m | R1m).iterElemBack();
            };
            while (unionLeadIter.next()) |e2| {
                if (e2 >= THREE_MAX) {
                    // d byte, fast-forward all T3 for each side.
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
                }
                // In c region of T2s.  T3 indices are past d byte region.
                const inLT2 = LT2m.isElem(e2);
                if (!inLT2) {
                    // Must be in R, we skip.
                    assert(RT2m.isElem(e2));
                    assert(Rbod[RT2i] != 0);
                    // While keeping our indices accurate:
                    RT3i += @popCount(Rbod[RT2i]);
                    RT2i -= 1;
                    continue;
                }
                const inRT2 = RT2m.isElem(e2);
                if (inLT2 and inRT2) {
                    // Find T3 intersections and take difference.
                    assert(NT2[e2] != 0);
                    const eLT2m = toMask(NT2[e2]);
                    assert(Rbod[RT2i] != 0);
                    const eRT2m = toMask(Rbod[RT2i]);
                    // We're done with RT2i, decrement.
                    RT2i -= 1;
                    // Tterate the union of this T2 word backward.
                    var unionT2iter = toMask(eLT2m.m | eRT2m.m).iterElemBack();
                    while (unionT2iter.next()) |e3| {
                        if (eLT2m.isElem(e3) and eRT2m.isElem(e3)) {
                            NT3[NT3i] &= ~Rbod[RT3i];
                            // Check for T3 null and mask out T2 bit if so.
                            if (NT3[NT3i] == 0) {
                                var eNT2 = toMask(NT2[e2]);
                                eNT2.remove(codeunit(e3));
                                NT2[e2] = eNT2.m;
                                // We check if NT2[e2] is clear after iteration.
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
                    // Check for emptied NT2 mask.
                    if (NT2[e2] == 0) {
                        LLeadMask.remove(codeunit(e2));
                    }
                } else { // Must be L2, advance NT3i.
                    assert(inLT2);
                    NT3i += @popCount(NT2[e2]);
                }
            }
        } else { // Else R is only two byte or less, no action needed.
            assert(R.t3slice() == null);
        }
        // Once again, R 4-byte is irrelevant if L has none.
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
            // We can just copy LT4 and return.
            header[LEAD] = LLeadMask.m;
            const T2c = compactSlice(&NT2);
            const T2end = 4 + T2c.len;
            const T3c = compactSlice(NT3);
            const T3end = T2end + T3c.len;
            // Which is the T4 offset:
            header[T4_OFF] = T3end;
            const T4 = L.t4slice().?; // Checked in prior if statement.
            const setLen = T3end + T4.len;
            const Nbod = try allocator.alloc(u64, setLen);
            @memcpy(Nbod[0..4], &header);
            @memcpy(Nbod[4..T2end], T2c);
            @memcpy(Nbod[T2end..T3end], T3c);
            @memcpy(Nbod[T3end..setLen], T4);
            return RuneSet{ .body = Nbod };
        }
        // Tier 4.
        const LT4 = L.t4slice().?; // Checked above.
        const NT4 = try allocator.alloc(u64, LT4.len);
        defer allocator.free(NT4);
        @memcpy(NT4, LT4);
        // We iterate backward through NT2, and each mask thereof,
        // allowing forward iteration through T3 and T4.
        // Track ownership of T2 bits on each side:
        const LT1m = toMask(Lbod[LEAD]);
        const RT1m = toMask(Rbod[LEAD]);
        // Track progress forward through T3,
        // the d region of NT3 is still identical to LT3.
        var NT3i: usize = 0;
        var RT3i = R.t3start();
        // Similarly, d region of NT2 is still identical to LT2.
        var RT2i = R.t2final();
        // Track T4 progress forward.
        var NT4i: usize = 0;
        var RT4i = Rbod[T4_OFF];
        // Backward iterate T2, d region only.
        var unionLeadIter = blk: {
            const LT1d = Lbod[LEAD] & MASK_IN_FOUR;
            const RT1d = Rbod[LEAD] & MASK_IN_FOUR;
            break :blk toMask(LT1d | RT1d).iterElemBack();
        };
        t2iter: while (unionLeadIter.next()) |e2| {
            const inLT2 = LT1m.isElem(e2);
            if (!inLT2) {
                // Must be in R, we skip.
                assert(RT1m.isElem(e2));
                assert(Rbod[RT2i] != 0);
                assert(NT2[e2] == 0);
                // Move T3 and T4 index forward and T2 index back.
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
                // Find T3 overlaps and apply T4 accordingly.
                assert(NT2[e2] != 0);
                const LT2m = toMask(NT2[e2]);
                assert(Rbod[RT2i] != 0);
                const RT2m = toMask(Rbod[RT2i]);
                // We're done with RT2i, decrement.
                RT2i -= 1;
                const bothT2m = LT2m.intersection(RT2m);
                // Iterate the union of this T2 word backward.
                var unionT2iter = toMask(LT2m.m | RT2m.m).iterElemBack();
                while (unionT2iter.next()) |e3| {
                    if (bothT2m.isElem(e3)) {
                        // We iterate forward through T3 and T4.
                        // Intersections are diffed,
                        // zeroed words remove that NT3 bit.
                        assert(NT3[NT3i] != 0); // May not be true later!
                        const eLT3m = toMask(NT3[NT3i]);
                        assert(Rbod[RT3i] != 0);
                        const eRT3m = toMask(Rbod[RT3i]);
                        var unionT3iter = toMask(eLT3m.m | eRT3m.m).iterElements();
                        while (unionT3iter.next()) |e4| {
                            if (eLT3m.isElem(e4) and eRT3m.isElem(e4)) {
                                NT4[NT4i] &= ~Rbod[RT4i];
                                // Null check for T3 mask.
                                if (NT4[NT4i] == 0) {
                                    var NT3m = toMask(NT3[NT3i]);
                                    NT3m.remove(codeunit(e4));
                                    NT3[NT3i] = NT3m.m;
                                } // NT3[NT3i] checked after unionT3iter while loop.
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
                        } // NT2[e2] checked after unionT2iter while loop.
                        // Finished iterating T3 mask.
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
                // Check for empty NT2 mask.
                if (NT2[e2] == 0) {
                    LLeadMask.remove(codeunit(e2));
                }
            } else { // Must be L2, we skip forward.
                assert(inLT2);
                const NT3count = @popCount(NT2[e2]);
                for (0..NT3count) |_| {
                    const NT4count = @popCount(NT3[NT3i]);
                    NT4i += NT4count;
                    NT3i += 1;
                }
            }
        } // Postconditions:
        assert(NT4i == NT4.len);
        // No post for NT3i, we may have removed bits from the popcount.
        assert(RT2i == R.t2_4b_start() - 1);
        assert(RT3i == R.t3_3c_start());
        assert(RT4i == Rbod.len);
        // Assemble set.
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

    /// Set intersection: a new `RuneSet` containing the common members
    /// of both sets provided.
    /// Free returned memory with `set.deinit(allocator)`.
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
        // No spreading L2 into NT2 this time, it would just create extra work.
        var NT2: [FOUR_MAX]u64 = .{0} ** FOUR_MAX;
        // Must be reassigned to header[LEAD] before returning.
        var NLeadMask = toMask(header[LEAD]);
        const LT1m = L.maskAt(LEAD);
        const RT1m = R.maskAt(LEAD);
        {
            var T2iter = LT1m.setunion(RT1m).iterElements();
            var LT2i: usize = L.t2start();
            var RT2i: usize = L.t2start();
            while (T2iter.next()) |e2| {
                // Only time we can test with NLeadMask:
                if (NLeadMask.isElem(e2)) {
                    NT2[e2] = Lbod[LT2i] & Rbod[RT2i];
                    if (NT2[e2] == 0) {
                        NLeadMask.remove(codeunit(e2));
                    }
                    LT2i += 1;
                    RT2i += 1;
                } else if (LT1m.isElem(e2)) {
                    LT2i += 1;
                } else {
                    assert(RT1m.isElem(e2));
                    RT2i += 1;
                }
            }
        } // We can check LEAD to see if there are surviving c bytes.
        if (NLeadMask.m & MASK_OUT_TWO == 0) {
            header[LEAD] = NLeadMask.m;
            const T2c = compactSlice(&NT2);
            const Nbod = try allocator.alloc(u64, 4 + T2c.len);
            @memcpy(Nbod[0..4], &header);
            @memcpy(Nbod[4..], T2c);
            return RuneSet{ .body = Nbod };
        }
        // Tier 3
        const bothT1m = toMask(Rbod[LEAD] & Lbod[LEAD]);
        // In this and Tier 4, we use L as our reference point for N.
        const NT3 = try allocator.alloc(u64, L.t3slice().?.len);
        defer allocator.free(NT3);
        @memset(NT3, 0);
        {
            // We iterate back through both T2s to find surviving T3s.
            var unionT2iter = blk: {
                const RT1c_m = Rbod[LEAD] & MASK_OUT_TWO;
                const LT1c_m = Lbod[LEAD] & MASK_OUT_TWO;
                break :blk toMask(RT1c_m | LT1c_m).iterElemBack();
            };
            var RT2i = R.t2final();
            var LT2i = L.t2final();
            var LT3i = L.t3start();
            var RT3i = R.t3start();
            var NT3i: usize = 0;
            while (unionT2iter.next()) |e2| {
                if (bothT1m.isElem(e2)) {
                    // This can be empty, we still have to track LT3 and RT3.
                    var NT2m = toMask(NT2[e2]);
                    const LT2m = toMask(Lbod[LT2i]);
                    const RT2m = toMask(Rbod[RT2i]);
                    const bothT2m = LT2m.intersection(RT2m);
                    LT2i -= 1;
                    RT2i -= 1;
                    var unionT3iter = LT2m.setunion(RT2m).iterElemBack();
                    while (unionT3iter.next()) |e3| {
                        if (bothT2m.isElem(e3)) {
                            if (NT2m.isElem(e3)) {
                                NT3[NT3i] = Rbod[RT3i] & Lbod[LT3i];
                                if (NT3[NT3i] == 0) {
                                    NT2m.remove(codeunit(e3));
                                }
                            }
                            RT3i += 1;
                            LT3i += 1;
                            NT3i += 1;
                        } else if (LT2m.isElem(e3)) {
                            LT3i += 1;
                            NT3i += 1; // hence, zero
                        } else {
                            assert(RT2m.isElem(e3));
                            RT3i += 1;
                        }
                    }
                    NT2[e2] = NT2m.m;
                    // Might have started zero, but if not:
                    if (NT2[e2] == 0 and NLeadMask.isElem(e2)) {
                        NLeadMask.remove(codeunit(e2));
                    }
                } else if (LT1m.isElem(e2)) {
                    LT3i += @popCount(Lbod[LT2i]);
                    NT3i += @popCount(Lbod[LT2i]);
                    LT2i -= 1;
                } else {
                    assert(RT1m.isElem(e2));
                    RT3i += @popCount(Rbod[RT2i]);
                    RT2i -= 1;
                }
            }
            assert(LT3i == L.t3end());
            assert(RT3i == R.t3end());
            assert(NT3i == NT3.len);
            assert(LT2i == L.t2start() - 1 + @popCount(Lbod[LEAD] & MASK_IN_TWO));
            assert(RT2i == R.t2start() - 1 + @popCount(Rbod[LEAD] & MASK_IN_TWO));
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
        // Once again, reference on L.
        const NT4 = try allocator.alloc(u64, L.t4slice().?.len);
        defer allocator.free(NT4);
        @memset(NT4, 0);
        {
            // Backward through T2s.
            var RT2i = R.t2final();
            var LT2i = L.t2final();
            // Forward through d bytes of T3.
            var NT3i: usize = 0;
            var LT3i = L.t3start();
            var RT3i = R.t3start();
            // Forward through T4.
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
                if (bothT1m.isElem(e2)) {
                    var NT2m = toMask(NT2[e2]);
                    const LT2m = toMask(Lbod[LT2i]);
                    const RT2m = toMask(Rbod[RT2i]);
                    const bothT2m = LT2m.intersection(RT2m);
                    LT2i -= 1;
                    RT2i -= 1;
                    var unionT3iter = LT2m.setunion(RT2m).iterElemBack();
                    while (unionT3iter.next()) |e3| {
                        if (bothT2m.isElem(e3)) {
                            var NT3m = toMask(NT3[NT3i]);
                            const LT3m = toMask(Lbod[LT3i]);
                            const RT3m = toMask(Rbod[RT3i]);
                            // NT3[NT3i] was created as this intersection:
                            assert(NT3m.m == LT3m.intersection(RT3m).m);
                            var unionT4iter = LT3m.setunion(RT3m).iterElements();
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
                                    NT4i += 1;
                                    LT4i += 1;
                                } else {
                                    assert(RT3m.isElem(e4));
                                    RT4i += 1;
                                }
                            }
                            NT3[NT3i] = NT3m.m;
                            if (NT3[NT3i] == 0 and NT2m.isElem(e3)) {
                                NT2m.remove(codeunit(e3));
                            }
                            RT3i += 1;
                            LT3i += 1;
                            NT3i += 1;
                        } else if (LT2m.isElem(e3)) {
                            assert(NT3[NT3i] == 0);
                            const T4count = @popCount(Lbod[LT3i]);
                            assert(T4count > 0);
                            LT3i += 1;
                            NT3i += 1;
                            LT4i += T4count;
                            NT4i += T4count;
                        } else {
                            assert(RT2m.isElem(e3));
                            const T4count = @popCount(Rbod[RT3i]);
                            assert(T4count > 0);
                            RT4i += T4count;
                            RT3i += 1;
                        }
                    }
                    NT2[e2] = NT2m.m;
                    if (NT2[e2] == 0 and NLeadMask.isElem(e2)) {
                        NLeadMask.remove(codeunit(e2));
                    }
                } else if (LT1m.isElem(e2)) {
                    assert(NT2[e2] == 0);
                    assert(!NLeadMask.isElem(e2));
                    const T3count = @popCount(Lbod[LT2i]);
                    LT2i -= 1;
                    assert(T3count > 0);
                    for (0..T3count) |_| {
                        const T4count = @popCount(Lbod[LT3i]);
                        assert(NT3[NT3i] == 0);
                        assert(T4count > 0);
                        LT3i += 1;
                        NT3i += 1;
                        LT4i += T4count;
                        NT4i += T4count;
                    }
                } else {
                    assert(RT1m.isElem(e2));
                    assert(NT2[e2] == 0);
                    assert(!NLeadMask.isElem(e2));
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
            } // end T2 iter
            // Postconditions:
            assert(NT4i == NT4.len);
            assert(LT3i == L.t3_3c_start());
            assert(RT3i == R.t3_3c_start());
            assert(NT3i == L.t3_3c_start() - L.t3start());
            assert(LT4i == Lbod.len);
            assert(RT4i == Rbod.len);
            assert(RT2i == R.t2start() + @popCount(Rbod[LEAD] & MASK_OUT_FOUR) - 1);
            assert(LT2i == L.t2start() + @popCount(Lbod[LEAD] & MASK_OUT_FOUR) - 1);
            assert(NT3.len >= popCountSlice(NT2[TWO_MAX..]));
        } // end T4
        // These two are only used in debug mode, should be elided otherwise.
        const popT3 = popCountSlice(NT2[TWO_MAX..]);
        const popT4 = popCountSlice(NT2[THREE_MAX..]);
        // Final assembly.
        header[LEAD] = NLeadMask.m;
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

// An invalid Rune to signal a non-iterated RuneSetIterator.
const RUNE_START: u32 = 0xffff_ffff;

/// Iterate a set's members, one Rune at a time,
/// with `runeSetIterator.next()`.
pub const RuneSetIterator = struct {
    last: Rune,
    idx: usize,
    set: RuneSet,

    /// Create a RuneIterator.
    pub fn init(set: RuneSet) RuneSetIterator {
        return RuneSetIterator{
            // sentinel value (never valid)
            .last = @bitCast(RUNE_START),
            .idx = 0,
            .set = set,
        };
    }

    //| The challenge here is to maintain the state of the
    //| iterator, such that transitions between masks are
    //| efficient.  I've implemented this as a state machine,
    //| with each transition as its own function.  The unique
    //| transitions will be inlined.  I've opted to only store
    //| the index into the last byte, and recalculate when needed.
    //| Checking test membership needs to construct the same state
    //| for every successful match, and is very fast.  The most
    //| frequent changes are the cheapest.

    /// Yield the next Rune, or `null` when the
    /// set completes.
    pub fn next(iter: *RuneSetIterator) ?Rune {
        if (iter.last.rawInt() == RUNE_START) {
            const rune = iter.setup();
            if (rune) |r| {
                iter.last = r;
                return iter.last;
            } else {
                // If this is null, no need to reset.
                return null;
            }
        }
        // Note: the resulting rune may be of up to four bytes,
        // regardless of which switch prong is taken.
        const rune = switch (iter.last.byteCount()) {
            1 => iter.ascii(),
            2 => iter.bRune(),
            3 => iter.cRune(),
            4 => iter.dRune(),
            else => unreachable,
        };
        if (rune) |r| {
            return r;
        } else {
            // Reset as safety measure.
            iter.reset();
            return null;
        }
    }

    /// Reset the iterator to start with the first Rune.
    pub fn reset(iter: *RuneSetIterator) void {
        iter.last = @bitCast(RUNE_START);
        iter.idx = 0;
    }

    /// Set up a fresh RuneSetIterator.
    fn setup(iter: *RuneSetIterator) ?Rune {
        // Set up the RuneSetIterator, and return when possible.
        assert(iter.idx == LOW);
        var maybe_a = iter.set.maskAt(LOW).first(.low);
        if (maybe_a) |a| {
            // Low ASCII
            iter.last = Rune{
                .a = a.byte(),
                .b = 0,
                .c = 0,
                .d = 0,
            };
            iter.idx = LOW;
            return iter.last;
        } else {
            assert(iter.set.body[LOW] == 0);
            maybe_a = iter.set.maskAt(HI).first(.hi);
            if (maybe_a) |a| {
                // High ASCII
                iter.last = Rune{
                    .a = a.byte(),
                    .b = 0,
                    .c = 0,
                    .d = 0,
                };
                iter.idx = HI;
                return iter.last;
            } else {
                // Something multi-byte (maybe).
                return iter.setupHi();
            }
        }
    }

    /// Set up multi-byte iteration (whether fresh,
    /// or after ASCII completes).
    fn setupHi(iter: *RuneSetIterator) ?Rune {
        assert(iter.idx <= LEAD);
        const maybe_a = iter.set.maskAt(LEAD).first(.lead);
        if (maybe_a) |a| {
            const nB = a.nMultiBytes().?;
            const T2off = iter.set.t2offsetFor(a);
            const b = iter.set.maskAt(T2off).first(.follow).?;
            if (nB == 2) {
                iter.last = Rune{
                    .a = a.byte(),
                    .b = b.byte(),
                    .c = 0,
                    .d = 0,
                };
                iter.idx = T2off;
                return iter.last;
            }
            assert(iter.set.body[LEAD] & MASK_OUT_TWO != 0);
            // Since we're setting up, c is at the end of T3:
            const T3off = iter.set.t3final();
            assert(T3off == iter.set.t3offsetFor(T2off, b));
            const c = iter.set.maskAt(T3off).first(.follow).?;
            if (nB == 3) {
                iter.last = Rune{
                    .a = a.byte(),
                    .b = b.byte(),
                    .c = c.byte(),
                    .d = 0,
                };
                iter.idx = T3off;
                return iter.last;
            }
            assert(nB == 4);
            assert(iter.set.body[LEAD] & MASK_IN_FOUR != 0);
            assert(iter.set.body[T4_OFF] != 0);
            // To obtain T4:
            // Herein lies a mystery:
            const T4off = iter.set.body.len - @popCount(iter.set.body[T3off]);
            // Understand why this is true, and you understand the RuneSet.
            assert(T4off == iter.set.t4offsetFor(T3off, c));
            const d = iter.set.maskAt(T4off).first(.follow).?;
            iter.last = Rune{
                .a = a.byte(),
                .b = b.byte(),
                .c = c.byte(),
                .d = d.byte(),
            };
            iter.idx = T4off;
            return iter.last;
        } else {
            // Empty set.
            assert(iter.set.body[LEAD] == 0);
            assert(iter.set.body[T4_OFF] == 0);
            assert(iter.last.rawInt() == RUNE_START or iter.last.byteCount() == 1);
            assert(iter.idx <= LEAD);
            iter.reset();
            return null;
        }
    }

    /// Try to return an ASCII value, or advance to high
    /// characters, if any.
    fn ascii(iter: *RuneSetIterator) ?Rune {
        assert(iter.last.byteCount() == 1);
        const a_cu = codeunit(iter.last.a);
        const a_next = iter.set.maskAt(iter.idx).after(a_cu);
        if (a_next) |a| {
            iter.last = Rune{
                .a = a.byte(),
                .b = 0,
                .c = 0,
                .d = 0,
            };
            return iter.last;
        }
        iter.idx += 1;
        if (iter.idx == HI) {
            const maybe_a = iter.set.maskAt(HI).first(.hi);
            if (maybe_a) |a| {
                iter.last = Rune{
                    .a = a.byte(),
                    .b = 0,
                    .c = 0,
                    .d = 0,
                };
                return iter.last;
            } else {
                return iter.setupHi();
            }
        } else {
            assert(iter.idx == LEAD);
            return iter.setupHi();
        }
    }

    /// Try to return a two-byte character, advancing if
    /// we hit the last bit in a given T2b mask.
    fn bRune(iter: *RuneSetIterator) ?Rune {
        // a is set, we need the next b:
        const b_mask = iter.set.maskAt(iter.idx);
        const maybe_b = b_mask.after(codeunit(iter.last.b));
        if (maybe_b) |b| {
            iter.last = Rune{
                .a = iter.last.a,
                .b = b.byte(),
                .c = 0,
                .d = 0,
            };
            return iter.last;
        } else {
            return iter.afterBword();
        }
    }

    /// Advance the iterator after the last Rune in a
    /// two-byte mask.
    fn afterBword(iter: *RuneSetIterator) ?Rune {
        assert(iter.last.byteCount() == 2);
        // next a, if any
        const maybe_a = toMask(iter.set.body[LEAD]).after(codeunit(iter.last.a));
        if (maybe_a) |a| {
            iter.idx += 1;
            // Same as this:
            assert(iter.idx == iter.set.t2offsetFor(a));
            // Must be in T2 range:
            assert(iter.set.t2start() <= iter.idx);
            assert(iter.idx < iter.set.t2end());
            const b = iter.set.maskAt(iter.idx).first(.follow).?;
            switch (a.nMultiBytes().?) {
                2 => {
                    iter.last = Rune{
                        .a = a.byte(),
                        .b = b.byte(),
                        .c = 0,
                        .d = 0,
                    }; // idx is still valid
                    return iter.last;
                },
                3, 4 => |nB| {
                    // T3 starts at the end:
                    const T3off = iter.set.t3final();
                    const c = iter.set.maskAt(T3off).first(.follow).?;
                    if (nB == 3) {
                        iter.last = Rune{
                            .a = a.byte(),
                            .b = b.byte(),
                            .c = c.byte(),
                            .d = 0,
                        };
                        iter.idx = T3off;
                        return iter.last;
                    } else {
                        // Find T4
                        const T4off = iter.set.body.len - @popCount(iter.set.body[T3off]);
                        assert(T4off == iter.set.t4offsetFor(T3off, c));
                        const d = iter.set.maskAt(T4off).first(.follow).?;
                        iter.last = Rune{
                            .a = a.byte(),
                            .b = b.byte(),
                            .c = c.byte(),
                            .d = d.byte(),
                        };
                        iter.idx = T4off;
                        return iter.last;
                    }
                },
                else => unreachable,
            }
        } else {
            iter.reset();
            return null;
        }
    }

    /// Try to return a three-byte Rune, advancing if we
    /// reach the end of the T3 mask.
    fn cRune(iter: *RuneSetIterator) ?Rune {
        // a and b are set, we need the next c:
        const c_mask = iter.set.maskAt(iter.idx);
        const maybe_c = c_mask.after(codeunit(iter.last.c));
        if (maybe_c) |c| {
            iter.last = Rune{
                .a = iter.last.a,
                .b = iter.last.b,
                .c = c.byte(),
                .d = 0,
            };
            return iter.last;
        } else {
            return iter.afterCword();
        }
    }

    /// Advance to the next rune upon returning the last byte
    /// of a given T3 mask.
    fn afterCword(iter: *RuneSetIterator) ?Rune {
        assert(iter.last.byteCount() == 3);
        // a and b are known, we need the next b.
        var T2off = iter.set.t2offsetFor(codeunit(iter.last.a));
        const b_next = iter.set.maskAt(T2off).after(codeunit(iter.last.b));
        if (b_next) |b| {
            // Since a hasn't changed, we know it's still a three-byte
            // sequence:
            const T3off = iter.idx - 1;
            assert(T3off == iter.set.t3offsetFor(T2off, b));
            const c = iter.set.maskAt(T3off).first(.follow).?;
            iter.last = Rune{
                .a = iter.last.a,
                .b = b.byte(),
                .c = c.byte(),
                .d = 0,
            };
            iter.idx = T3off;
            return iter.last;
        } // Otherwise, next a:
        const a_next = iter.set.maskAt(LEAD).after(codeunit(iter.last.a));
        if (a_next) |a| {
            T2off += 1;
            assert(T2off == iter.set.t2offsetFor(a));
            const b = iter.set.maskAt(T2off).first(.follow).?;
            switch (a.nMultiBytes().?) {
                3, 4 => |nB| {
                    // iter.idx is still in T3.
                    const T3off = iter.idx - 1;
                    assert(T3off == iter.set.t3offsetFor(T2off, b));
                    const c = iter.set.maskAt(T3off).first(.follow).?;
                    if (nB == 3) {
                        assert(iter.set.t3_3c_start() <= T3off);
                        assert(T3off < iter.set.t3end());
                        iter.last = Rune{
                            .a = a.byte(),
                            .b = b.byte(),
                            .c = c.byte(),
                            .d = 0,
                        };
                        iter.idx = T3off;
                        return iter.last;
                    } else {
                        // Since we're in afterCword, there are three-byte
                        // sequences, so this test is valid:
                        assert(iter.set.t3start() <= T3off);
                        assert(T3off < iter.set.t3_3c_start());
                        // Since we're in afterCword, this is the first d.
                        iter.idx = iter.set.body.len - @popCount(iter.set.body[T3off]);
                        assert(iter.idx == iter.set.t4offsetFor(T3off, c));
                        const d = iter.set.maskAt(iter.idx).first(.follow).?;
                        iter.last = Rune{
                            .a = a.byte(),
                            .b = b.byte(),
                            .c = c.byte(),
                            .d = d.byte(),
                        };
                        return iter.last;
                    }
                },
                else => unreachable,
            }
        } else { // We're done
            iter.reset();
            return null;
        }
    }

    /// Return a four-byte character from the current word,
    /// or advance to the next word.
    fn dRune(iter: *RuneSetIterator) ?Rune {
        // a, b, and c, are set, we need the next d:
        const maybe_d = iter.set.maskAt(iter.idx).after(codeunit(iter.last.d));
        if (maybe_d) |d| {
            iter.last = Rune{
                .a = iter.last.a,
                .b = iter.last.b,
                .c = iter.last.c,
                .d = d.byte(),
            };
            return iter.last;
        } else {
            return iter.resetToD();
        }
    }

    /// Yield the next Rune of four bytes, should one
    /// happen to exist.
    fn resetToD(iter: *RuneSetIterator) ?Rune {
        assert(iter.last.byteCount() == 4);
        // Try for next c.
        var T2off = iter.set.t2offsetFor(codeunit(iter.last.a));
        var T3off = iter.set.t3offsetFor(T2off, codeunit(iter.last.b));
        const maybe_c = iter.set.maskAt(T3off).after(codeunit(iter.last.c));
        if (maybe_c) |c| {
            // Since we're still in the same c mask, increment is valid.
            iter.idx += 1;
            assert(iter.idx == iter.set.t4offsetFor(T3off, c));
            const d = iter.set.maskAt(iter.idx).first(.follow).?;
            iter.last = Rune{
                .a = iter.last.a,
                .b = iter.last.b,
                .c = c.byte(),
                .d = d.byte(),
            };
            return iter.last;
        } else { // New b needed.
            const maybe_b = iter.set.maskAt(T2off).after(codeunit(iter.last.b));
            if (maybe_b) |b| {
                // T3off can be decremented.
                T3off -= 1; // Equal to this more expensive calculation:
                assert(T3off == iter.set.t3offsetFor(T2off, b));
                const c = iter.set.maskAt(T3off).first(.follow).?;
                // Generally, this is the fastest way to obtain the T4 offset.
                // We might expect this to be a tad slower for the penultimate and
                // ultimate T3 masks in a set, a bit quicker otherwise.
                iter.idx = iter.idx - @popCount(iter.set.body[T3off]) - @popCount(iter.set.body[T3off + 1]) + 1;
                assert(iter.idx == iter.set.t4offsetFor(T3off, c));
                assert(iter.set.t4offset() <= iter.idx);
                const d = iter.set.maskAt(iter.idx).first(.follow).?;
                iter.last = Rune{
                    .a = iter.last.a,
                    .b = b.byte(),
                    .c = c.byte(),
                    .d = d.byte(),
                };
                return iter.last;
            } else { // New a needed.
                const maybe_a = iter.set.maskAt(LEAD).after(codeunit(iter.last.a));
                if (maybe_a) |a| {
                    // Increment in T2.
                    T2off += 1;
                    // Was already in d byte region, so now, must be greater
                    // than the first offset:
                    assert(iter.set.t2_4b_start() < T2off);
                    assert(T2off < iter.set.t2end());
                    const b = iter.set.maskAt(T2off).first(.follow).?;
                    T3off -= 1;
                    assert(T3off == iter.set.t3offsetFor(T2off, b));
                    const c = iter.set.maskAt(T3off).first(.follow).?;
                    iter.idx = iter.idx - @popCount(iter.set.body[T3off]) - @popCount(iter.set.body[T3off + 1]) + 1;
                    assert(iter.idx == iter.set.t4offsetFor(T3off, c));
                    assert(iter.set.t4offset() <= iter.idx);
                    const d = iter.set.maskAt(iter.idx).first(.follow).?;
                    iter.last = Rune{
                        .a = a.byte(),
                        .b = b.byte(),
                        .c = c.byte(),
                        .d = d.byte(),
                    };
                    return iter.last;
                } else { // No a means we're done.
                    iter.reset();
                    return null;
                }
            } // end maybe_b
        } // end maybe_c
        unreachable;
    }
}; // end RuneIterator

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
    if (lead.count() == 0) { // Set was ASCII-only.
        assert(sieve.len == back);
        const memHeader = try allocator.alloc(u64, 4);
        @memcpy(memHeader, &header);
        return memHeader;
    }
    sieve = sieve[0 .. sieve.len - back];
    // Sieve for second bytes.
    // Here we add an extra empty word to prevent a conditional later.
    var T2: [FOUR_MAX + 1]u64 = .{0} ** (FOUR_MAX + 1); // Masks for all second bytes.
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
                    // Add all second bytes.
                    const b = codeunit(sieve[idx + 1]);
                    if (b.kind != .follow) return InvalidUnicode;
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
    } // All second bytes accounted for.
    header[LEAD] = lead.m;
    if (sieve.len == back) {
        // High region of T2 must be empty:
        assert(popCountSlice(T2[TWO_MAX..]) == 0);
        const T2c = compactSlice(&T2);
        // Should be length of popcount:
        assert(T2c.len == lead.count());
        const setBody = try allocator.alloc(u64, 4 + T2c.len);
        @memcpy(setBody[0..4], &header);
        @memcpy(setBody[4..], T2c);
        return setBody;
    }
    sieve = sieve[0 .. sieve.len - back];
    // Sieve for third bytes.
    // Number of elements in the high region of T2
    // is the number of masks in T3.
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
                assert(two.kind == .follow); // Already validated in sieve two.
                // T2 is one-to-one.
                const twoMask = toMask(T2[one.body]);
                assert(twoMask.isIn(two));
                // Count all higher elements.
                const hiThree = twoMask.higherThan(two).?;
                // Unconditional due to the extra mask in T2.
                const threeOff = hiThree + popCountSlice(T2[one.body + 1 ..]);
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
        // No four-byte characters.
        // "very high" region of T2 must be empty:
        assert(popCountSlice(T2[THREE_MAX..]) == 0);
        // T3 must have values in it:
        assert(popCountSlice(T3) != 0);
        const T2c = compactSlice(&T2);
        // Must be length of popcount:
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
    // Length of header (4) + compacted T2 + popcount hi T2.
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
    // We no longer need back.
    idx = 0;
    // Sieve for fourth byte.
    // T4 is 'striped': the lexical order runs back to front, but
    // forward with respect to any one T3 word mask.  This allows
    // for the optimally-minimal amount of offset calculation.
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
                // Count all higher elements.
                const hiThree = twoMask.higherThan(two).?;
                const threeOff = hiThree + popCountSlice(T2[one.body + 1 ..]);
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
    // Given that the above is correct, we have validated UTF-8,
    // at least for our purposes, and added all runes in the string.
    const T2c = compactSlice(&T2);
    // Must be length of lead popcount.
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
            // Slice is safe because we know the T2 span has at least one word.
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
            if (d_mask.isIn(d)) return 4 else {
                return 0;
            }
        },
    }
}

/// Match one codepoint against the set, returning the number of bytes
/// matched.  This performs no validation, meaning that invalid unicode
/// can return bogus results.  Truncated UTF-8 at the end of a buffer
/// *will* read beyond valid memory in fast release modes.  This
/// function is, however, safe to call with a follow byte at [0].
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
            // Slice is safe because we know the T2 span has at least one word.
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

/// Count non-zero members of word slice.
inline fn nonZeroCount(words: []const u64) usize {
    var w: usize = 0;
    for (words) |word| {
        if (word != 0)
            w += 1;
    }
    return w;
}

/// Sum of @popCount of all words in region.
fn popCountSlice(region: []const u64) usize {
    var ct: usize = 0;
    for (region) |w| ct += @popCount(w);
    return ct;
}

test popCountSlice {
    const region: [4]u64 = .{ 0, 0, 1, 3 };
    // Check that empty slice returns 0.
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
fn makeMutable(s: []const u8, a: Allocator) error{OutOfMemory}![]u8 {
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

//| Includes and namespacing.

const std = @import("std");
const builtin = @import("builtin");
const Allocator = std.mem.Allocator;
const assert = std.debug.assert;

const elements = @import("elements.zig");

const Mask = elements.Mask;
const toMask = Mask.toMask;
/// A wrapper for a single byte of utf8
pub const CodeUnit = elements.CodeUnit;
/// codeunit(:u8) creates a CodeUnit
pub const codeunit = elements.codeunit;
pub const Rune = elements.Rune;

const testing = std.testing;
const expect = testing.expect;
const expectEqual = testing.expectEqual;
