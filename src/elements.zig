//!  libruneset: a Zig library for fast utf-8 charsets
//!
//!

const std = @import("std");
const testing = std.testing;
const bit_set = std.bit_set;
const IntegerBitSet = bit_set.IntegerBitSet;

/// Kinds of most significant bits in UTF-8
pub const RuneKind = enum(u2) {
    low,
    hi,
    follow,
    lead,
};

/// Useful repr of one CodeUnit of a Rune
pub const CodeUnit = packed struct(u8) {
    body: u6,
    kind: RuneKind,

    /// Mask to check presence
    pub inline fn inMask(self: *const CodeUnit) u64 {
        return @as(u64, 1) << self.body;
    }

    /// Number of bytes in known multi-byte rune.
    ///
    /// Caller guarantees that the CodeUnit is a lead byte
    /// of a multi-byte rune: `cu.kind == .lead`.
    ///
    /// Invalid lead bytes will return null.
    pub inline fn nMultiBytes(self: *const CodeUnit) ?u8 {
        std.debug.assert(self.kind == .lead);
        return switch (self.body) {
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

    /// number of bytes in *any* valid lead rune
    /// Will return null for invalid leads.
    pub inline fn nBytes(self: *const CodeUnit) ?u8 {
        switch (self.kind) {
            .low, .hi => return 1,
            .follow => return null,
            .lead => return self.nMultiBytes(),
        }
    }

    /// Mask off all bits >= cu.body
    pub inline fn hiMask(self: *const CodeUnit) u64 {
        if (self.body == 63) {
            return std.math.maxInt(u64);
        } else {
            return (@as(u64, 1) << self.body) - 1;
        }
    }

    /// Mask off all bits <= cu.body
    pub inline fn lowMask(self: *const CodeUnit) u64 {
        if (self.body == 0)
            return std.math.maxInt(u64);
        if (self.body == 63)
            return 0
        else
            return ~((@as(u64, 1) << (self.body + 1)) - 1);
    }

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
/// We define our own bitset because the operations we need to
/// perform only overlap with IntegerBitSet for trivial one-liners,
/// and furthermore, we need nondestructive versions of the basic
/// operations, which aren't a part of the IntegerBitSet interface.
///
/// Note that Masks do not track which kind of byte they apply to,
/// since they will be stored as ordinary u64s.  User code must
/// ensure that CodeUnits tested against a Mask are of the appropriate
/// type, and order in sequence.
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

    /// Add a range of CodeUnits to a Mask.
    /// Caller guarantees that the range is ordered, and
    /// that the bytes are of the same `.kind`.
    pub fn addRange(self: *Mask, c1: CodeUnit, c2: CodeUnit) void {
        std.debug.assert(c1.kind == c2.kind);
        std.debug.assert(c1.body < c2.body);
        const mask = std.math.pow(u64, 2, (c2.body - c1.body) + 1) - 1;
        self.m |= mask << c1.body;
    }

    /// Test if a CodeUnit's low bytes are present in mask
    pub inline fn isIn(self: *const Mask, cu: CodeUnit) bool {
        return self.m | cu.inMask() == self.m;
    }

    /// Test if a u6 element is present in mask
    pub inline fn isElem(self: *const Mask, u: u6) bool {
        return self.m | @as(u64, 1) << u == self.m;
    }

    /// Return number of bytes lower than cu.body in mask,
    /// if cu inhabits the mask.  Otherwise return null.
    pub inline fn lowerThan(self: *const Mask, cu: CodeUnit) ?u64 {
        if (self.isIn(cu)) {
            const m = cu.hiMask();
            return @popCount(self.m & m);
        } else {
            return null;
        }
    }

    /// Return number of bytes higher than cu.body in mask,
    /// if cu inhabits the mask.  Otherwise return null.
    pub inline fn higherThan(self: *const Mask, cu: CodeUnit) ?u64 {
        if (self.isIn(cu)) {
            const m = cu.lowMask();
            return @popCount(self.m & m);
        } else {
            return null;
        }
    }

    /// Check u6 element for membership. Used to iterate, and
    /// in set operations on RuneSets.
    pub inline fn bitSet(self: *const Mask, member: u6) bool {
        return self.m | @as(u64, 1) << member == self.m;
    }

    pub fn iterElements(self: *const Mask) MaskElements {
        return MaskElements{ .mask = self.* };
    }

    pub fn iterElemBack(self: *const Mask) MaskElemBack {
        return MaskElemBack{ .mask = self.* };
    }

    pub fn iterCodeUnits(self: *const Mask, kind: RuneKind) MaskCodeUnits {
        return MaskCodeUnits{ .mIter = self.iterElements(), .kind = kind };
    }

    /// Return count of all members in set.
    ///
    /// Most popcounts are done on words we don't need to have
    /// as Masks, this is a convenience for the LEAD word in
    /// particular, which will already be in Mask form.
    pub inline fn count(self: *const Mask) usize {
        return @popCount(self.m);
    }

    /// Return union of two Masks as a new Mask
    pub inline fn setunion(self: *const Mask, other: *const Mask) Mask {
        return Mask{ .m = self.m | other.m };
    }

    /// Return intersection of two Masks as a new Mask
    pub inline fn intersection(self: *const Mask, other: *const Mask) Mask {
        return Mask{ .m = self.m & other.m };
    }

    /// Return difference of two Masks as a new Mask
    pub inline fn difference(self: *const Mask, other: *const Mask) Mask {
        return Mask{ .m = self.m & ~other.m };
    }
};

/// Mask Iterator. maskElements.next() will provide all
/// u6 elements of the Mask.
pub const MaskElements = struct {
    mask: Mask,
    i: u6 = 0,
    pub fn next(itr: *MaskElements) ?u6 {
        var result: ?u6 = null;
        while (itr.i < 63) {
            if (itr.mask.bitSet(itr.i)) {
                result = itr.i;
                itr.i += 1;
                break;
            } else {
                itr.i += 1;
            }
        }
        if (result) |r| return r;
        if (itr.i == 63 and itr.mask.bitSet(itr.i))
            return itr.i
        else
            return null;
    }
};

pub const MaskElemBack = struct {
    mask: Mask,
    i: u6 = 63,

    pub fn next(itr: *MaskElemBack) ?u6 {
        var result: ?u6 = null;
        while (itr.i > 0) {
            if (itr.mask.bitSet(itr.i)) {
                result = itr.i;
                itr.i -= 1;
                break;
            } else {
                itr.i -= 1;
            }
        }
        if (result) |r| return r;
        if (itr.i == 0 and itr.mask.bitSet(itr.i))
            return itr.i
        else
            return null;
    }
};

/// Iterate all the CodeUnits of a mask, as provided
/// with the correct RuneKind
pub const MaskCodeUnits = struct {
    mIter: MaskElements,
    kind: RuneKind,
    pub fn next(itr: *MaskCodeUnits) ?CodeUnit {
        const elem = MaskElements.next(&itr.mIter);
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

// test "bleh" {
//     const C = split('C');
//     std.debug.print("body: {d} hiMask: 0b{b:0>64}\n", .{ C.body, C.hiMask() });
//     std.debug.print("       lowMask: 0b{b:0>64}\n", .{C.lowMask()});
//     std.debug.print("        inMask: 0b{b:0>64}\n", .{C.inMask()});
//     const Qmark = split('?');
//     std.debug.print("body: {d} hiMask: 0b{b:0>64}\n", .{ Qmark.body, Qmark.hiMask() });D
//     std.debug.print("        lowMask: 0b{b:0>64}\n", .{Qmark.lowMask()});
//     const six_three = split('O');
//     std.debug.print("body: {d} hiMask: 0b{b:0>64}\n", .{ six_three.body, six_three.hiMask() });
//     std.debug.print("        lowMask: 0b{b:0>64}\n", .{six_three.lowMask()});
//     const at = split('@');
//     std.debug.print("body: {d} lowMask: 0b{b:0>64}\n", .{ at.body, at.lowMask() });
//     std.debug.print("         hiMask: 0b{b:0>64}\n", .{at.hiMask()});
//     try expect(3 == 3);
// }
