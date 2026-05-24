//! libruneset: a Zig library for fast UTF-8 charsets
//!
//! The elements namespace provides data structures for working
//! with UTF-8 encoded data.

const std = @import("std");
const builtin = @import("builtin");
const testing = std.testing;
const assert = std.debug.assert;
const safeMode = builtin.mode == .Debug or builtin.mode == .ReleaseSafe;

/// Kinds of most significant bits in UTF-8
pub const RuneKind = enum(u2) {
    low,
    hi,
    follow,
    lead,
};

/// Packed `u8` struct representing one codeunit of UTF-8.
pub const CodeUnit = packed struct(u8) {
    body: u6,
    kind: RuneKind,

    /// Mask to check presence
    pub inline fn inMask(self: *const CodeUnit) u64 {
        return @as(u64, 1) << self.body;
    }

    // TODO consider an nMultiBytesFast, for the cases where we
    // know that invalid lead bytes are never present (such as in set)
    // operations, where we may assume that (and will assert that) the
    // LEAD mask contains no such bytes.

    /// Number of bytes in known multi-byte rune.
    ///
    /// Caller guarantees that the CodeUnit is a lead byte
    /// of a multi-byte rune: `cu.kind == .lead`.
    ///
    /// Invalid lead bytes will return null.
    pub inline fn nMultiBytes(self: *const CodeUnit) ?u8 {
        assert(self.kind == .lead);
        return switch (self.body) {
            // 0 and 1 are invalid for overlong reasons,
            // but RuneSet supports overlong encodings
            0...31 => 2,
            32...47 => 3,
            48...55 => 4,
            // Wasted space 56...61 is due entirely to Microsoft's
            // lack of vision and insistence on a substandard
            // and utterly inadequate encoding for Unicode
            // "64k should be enough for anyone" <spits>
            56...63 => null,
        };
    }

    /// Given a valid lead byte, return the number of bytes that should
    /// make up the code unit sequence.  Will return `null` if the lead
    /// byte is invalid.
    pub inline fn nBytes(self: *const CodeUnit) ?u8 {
        switch (self.kind) {
            .low, .hi => return 1,
            .lead => return self.nMultiBytes(),
            .follow => return null,
        }
    }

    /// Mask off all bits >= cu.body
    pub inline fn hiMask(self: *const CodeUnit) u64 {
        return (@as(u64, 1) << self.body) - 1;
    }

    /// Mask off all bits <= cu.body
    pub inline fn lowMask(self: *const CodeUnit) u64 {
        if (self.body == 63)
            return 0
        else
            return ~((@as(u64, 1) << (self.body + 1)) - 1);
    }

    /// Cast the `CodeUnit` to its backing `u8`.
    pub inline fn byte(self: *const CodeUnit) u8 {
        return @bitCast(self.*);
    }
};

/// Cast raw byte to CodeUnit
pub inline fn codeunit(b: u8) CodeUnit {
    return @bitCast(b);
}

/// Bitmask for runesets
///
/// We define our own bitset, because the operations we need to
/// perform only overlap with IntegerBitSet for trivial one-liners,
/// and furthermore, we need nondestructive versions of the basic
/// operations, which aren't a part of the IntegerBitSet interface.
///
/// Note that Masks do not track which kind of byte they apply to,
/// since they will be stored as ordinary u64s.  User code must
/// ensure that CodeUnits tested against a Mask are of the appropriate
/// type, and otherwise valid for the test performed.
///
pub const Mask = struct {
    m: u64,

    pub fn toMask(w: u64) Mask {
        return Mask{ .m = w };
    }

    /// Add one CodeUnit to a Mask.
    pub fn add(self: *Mask, cu: CodeUnit) void {
        self.m |= cu.inMask();
    }

    pub fn remove(self: *Mask, cu: CodeUnit) void {
        assert(self.isIn(cu));
        self.m &= ~cu.inMask();
    }

    // NOTE: This is not used anywhere, at least not yet (it is tested).

    /// Add a range of CodeUnits to a Mask.
    /// Caller guarantees that the range is ordered, and
    /// that the bytes are of the same `.kind`.
    pub fn addRange(self: *Mask, c1: CodeUnit, c2: CodeUnit) void {
        assert(c1.kind == c2.kind);
        assert(c1.body < c2.body);
        const mask = std.math.pow(u64, 2, (c2.body - c1.body) + 1) - 1;
        self.m |= mask << c1.body;
    }

    /// Test if a CodeUnit's low bytes are present in mask
    pub inline fn isIn(self: Mask, cu: CodeUnit) bool {
        return self.m | cu.inMask() == self.m;
    }

    /// Test if a u6 element is present in mask
    pub inline fn isElem(self: Mask, u: u6) bool {
        return (self.m & (@as(u64, 1) << u)) != 0;
    }

    /// Return number of bytes lower than cu.body in mask,
    /// if cu inhabits the mask.  Otherwise return null.
    pub inline fn lowerThan(self: Mask, cu: CodeUnit) ?u7 {
        if (self.isIn(cu)) {
            const m = cu.hiMask();
            return @popCount(self.m & m);
        } else {
            return null;
        }
    }

    /// Return number of bytes higher than cu.body in mask,
    /// if cu inhabits the mask.  Otherwise return null.
    pub inline fn higherThan(self: Mask, cu: CodeUnit) ?u7 {
        if (self.isIn(cu)) {
            const m = cu.lowMask();
            return @popCount(self.m & m);
        } else {
            return null;
        }
    }

    /// Return the next element of the Mask.
    /// It is illegal to pass this function a nonexistent element.
    pub inline fn after(self: Mask, cu: CodeUnit) ?CodeUnit {
        assert(self.isIn(cu));
        if (cu.body == 63) return null;
        const kind = cu.kind;
        var next: u6 = cu.body + 1;
        while (true) {
            if (self.isElem(next)) {
                return CodeUnit{ .kind = kind, .body = next };
            }
            if (next == 63) break;
            next += 1;
        }
        return null;
    }

    /// Return the first codeunit in the mask, if any.
    pub inline fn first(self: Mask, kind: RuneKind) ?CodeUnit {
        const c1 = @ctz(self.m);
        if (c1 == 64) {
            return null;
        } else {
            return CodeUnit{ .kind = kind, .body = @intCast(c1) };
        }
    }

    /// Return a forward iterator of elements (u6) in the Mask.
    pub fn iterElements(self: Mask) MaskElements {
        return MaskElements{ .mask = self };
    }

    /// Return a backward iterator of elements (u6) in the Mask.
    pub fn iterElemBack(self: Mask) MaskElemBack {
        return MaskElemBack{ .mask = self };
    }

    /// Given a CodeUnit kind, return a forward iterator of the elements
    /// of Mask as CodeUnits of that kind.
    pub fn iterCodeUnits(self: Mask, kind: RuneKind) MaskCodeUnits {
        return MaskCodeUnits{ .mIter = self.iterElements(), .kind = kind };
    }

    /// Given a CodeUnit kind, return a backward iterator of the elements
    /// of Mask as CodeUnits of that kind.
    pub fn iterCodeUnitsBack(self: Mask, kind: RuneKind) MaskCodeUnitsBack {
        return MaskCodeUnitsBack{ .mBack = self.iterElemBack(), .kind = kind };
    }

    /// Return count of all members in set.
    ///
    /// Most popcounts are done on words we don't need to have
    /// as Masks, this is a convenience for the LEAD word in
    /// particular, which will already be in Mask form.
    pub inline fn count(self: Mask) usize {
        return @popCount(self.m);
    }

    /// Return union of two Masks as a new Mask
    pub inline fn setunion(self: Mask, other: Mask) Mask {
        return Mask{ .m = self.m | other.m };
    }

    /// Return intersection of two Masks as a new Mask
    pub inline fn intersection(self: Mask, other: Mask) Mask {
        return Mask{ .m = self.m & other.m };
    }

    /// Return difference of two Masks as a new Mask
    pub inline fn difference(self: Mask, other: Mask) Mask {
        return Mask{ .m = self.m & ~other.m };
    }
};

//| Iterators

/// Mask Iterator.  maskElements.next() will provide all
/// u6 elements of the Mask.
pub const MaskElements = struct {
    mask: Mask,

    pub fn next(itr: *MaskElements) ?u6 {
        if (itr.mask.m == 0) return null;

        const e: u6 = @intCast(@ctz(itr.mask.m));
        itr.mask.m &= itr.mask.m - 1;
        return e;
    }
};

/// Reverse Mask iterator, of u6
pub const MaskElemBack = struct {
    mask: Mask,

    pub fn next(itr: *MaskElemBack) ?u6 {
        if (itr.mask.m == 0) return null;

        const e: u6 = @intCast(63 - @clz(itr.mask.m));
        itr.mask.m &= ~(@as(u64, 1) << e);
        return e;
    }
};

/// Iterate all the CodeUnits of a mask, as provided
/// with the correct RuneKind
pub const MaskCodeUnits = struct {
    mIter: MaskElements,
    kind: RuneKind,
    pub fn next(itr: *MaskCodeUnits) ?CodeUnit {
        const elem = itr.mIter.next();
        if (elem) |e| {
            return CodeUnit{ .kind = itr.kind, .body = e };
        } else {
            return null;
        }
    }
};

/// Iterate all CodeUnits of a Mask, backward, given a
/// correct RuneKind.
pub const MaskCodeUnitsBack = struct {
    mBack: MaskElemBack,
    kind: RuneKind,
    pub fn next(itr: *MaskCodeUnitsBack) ?CodeUnit {
        const elem = itr.mBack.next();
        if (elem) |e| {
            return CodeUnit{ .kind = itr.kind, .body = e };
        } else {
            return null;
        }
    }
};

//| Tests

const expect = std.testing.expect;
const expectEqual = std.testing.expectEqual;
const expectError = std.testing.expectError;
const expectEqualDeep = std.testing.expectEqualDeep;

test codeunit {
    const A = codeunit('A');
    try expectEqual(A.kind, .hi);
    try expectEqual('A', A.byte());
    const zero = codeunit('0');
    try expectEqual(zero.kind, .low);
    try expectEqual('0', zero.byte());
    const lambda = "λ";
    const lead = codeunit(lambda[0]);
    try expectEqual(lead.kind, .lead);
    try expectEqual(lead.nMultiBytes(), 2);
    const follow = codeunit(lambda[1]);
    try expectEqual(follow.kind, .follow);
    try expectEqual(null, follow.nBytes());
    // mask property check
    for (0..256) |i| {
        const cu = codeunit(@truncate(i));
        try expectEqual(cu.lowMask() & cu.hiMask(), 0);
        // For 63, masks are u64 min and max
        if (cu.body != 63) {
            try expect(cu.lowMask() > cu.hiMask());
            // For 0, masks are u64 max and min
            if (cu.body != 0)
                try expectEqual(~(cu.lowMask() | cu.hiMask()), cu.inMask());
        }
    }
}

test "mask tests" {
    var mask = Mask.toMask(0);
    const B = codeunit('B');
    mask.add(B);
    try expect(mask.isIn(B));
    const D = codeunit('D');
    mask.add(D);
    const Z = codeunit('Z');
    mask.add(Z);
    try expectEqual(mask.higherThan(B).?, 2);
    try expectEqual(mask.lowerThan(D), 1);
    try expectEqual(mask.lowerThan(codeunit('?')), null);
    try expectEqual(D, mask.after(B).?);
    try expectEqual(Z, mask.after(D).?);
    try expectEqual(null, mask.after(Z));
    try expectEqualDeep(B, mask.first(B.kind));
    var mIter = mask.iterElements();
    try expectEqual(B.body, mIter.next().?);
    try expectEqual(D.body, mIter.next().?);
    try expectEqual(Z.body, mIter.next().?);
    try expectEqual(null, mIter.next());
    var cuIter = mask.iterCodeUnits(.hi);
    try expectEqual(B, cuIter.next().?);
    try expectEqual(D, cuIter.next().?);
    try expectEqual(Z, cuIter.next().?);
    try expectEqual(null, cuIter.next());
    var bIter = mask.iterElemBack();
    try expectEqual(Z.body, bIter.next().?);
    try expectEqual(D.body, bIter.next().?);
    try expectEqual(B.body, bIter.next().?);
    try expectEqual(null, bIter.next());
    var m2 = Mask.toMask(0);
    m2.addRange(codeunit('A'), codeunit('Z'));
    try expect(m2.isIn(D));
    try expect(m2.isIn(codeunit('A')));
    try expect(m2.isIn(codeunit('Z')));
    try expect(!m2.isIn(codeunit('@')));
    try expect(!m2.isIn(codeunit('[')));
    try expectEqual(26, m2.count());
}

test "mask removal" {
    var m0 = Mask.toMask(0);
    const c0 = codeunit('\x00');
    m0.add(c0);
    try expect(m0.isIn(c0));
    m0.remove(c0);
    try expectEqual(false, m0.isIn(c0));
}

test "back iter" {
    var mask = Mask.toMask(0);
    const At = codeunit('@');
    const A = codeunit('A');
    try expectEqual(0, At.body);
    mask.add(At);
    mask.add(codeunit('A'));
    var iterB = mask.iterElemBack();
    try expectEqual(A.body, iterB.next().?);
    try expectEqual(At.body, iterB.next().?);
    try expectEqual(null, iterB.next());
    var alliter = Mask.toMask(std.math.maxInt(u64)).iterElemBack();
    var count: usize = 0;
    while (alliter.next()) |_| {
        count += 1;
    }
    try expectEqual(64, count);
}

test "forward iter" {
    var alliter = Mask.toMask(std.math.maxInt(u64)).iterElements();
    var count: usize = 0;
    while (alliter.next()) |_| {
        count += 1;
    }
    try expectEqual(64, count);
}

test "invalid states" {
    var zeroMask = Mask.toMask(0);
    try expectEqual(null, zeroMask.higherThan(codeunit('1')));
    try expectEqual(null, zeroMask.lowerThan(codeunit('a')));
}
