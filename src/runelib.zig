//!  librunelib: a Zig library for fast utf-8 charsets
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
    pub fn inMask(self: *const CodeUnit) u64 {
        return @as(u64, 1) << self.body;
    }

    /// Mask off all bits >= cu.body
    pub fn hiMask(self: *const CodeUnit) u64 {
        if (self.body == 63) {
            return std.math.maxInt(u64);
        } else {
            return (@as(u64, 1) << self.body) - 1;
        }
    }

    /// Mask off all bits <= cu.body
    pub fn lowMask(self: *const CodeUnit) u64 {
        if (self.body == 63)
            return 0
        else
            return ~((@as(u64, 1) << (self.body + 1)) - 1);
    }
};

/// Cast raw byte to CodeUnit
pub inline fn split(b: u8) CodeUnit {
    return @bitCast(b);
}

/// Bitmask for runesets
///
/// We define our own bitset because the operations we need to
/// perform only overlap with IntegerBitSet for trivial one-liners,
/// and furthermore, we need nondestructive versions of the basic
/// operations, which aren't a part of the IntegerBitSet interface.
///
pub const Mask = struct {
    m: u64,

    pub fn toMask(w: u64) Mask {
        return Mask{ .m = w };
    }

    pub fn add(self: *Mask, cu: CodeUnit) void {
        self.m |= cu.inMask();
    }

    pub fn isIn(self: *const Mask, cu: CodeUnit) bool {
        const m = cu.inMask();
        return self.m | m == self.m;
    }

    /// Return number of bytes lower than cu.body in mask,
    /// if cu inhabits the mask.  Otherwise return null.
    pub fn lowerThan(self: *const Mask, cu: CodeUnit) ?u64 {
        if (self.isIn(cu)) {
            const m = cu.hiMask();
            return @popCount(self.m & m);
        } else {
            return null;
        }
    }

    /// Return number of bytes higher than cu.body in mask,
    /// if cu inhabits the mask.  Otherwise return null.
    pub fn higherThan(self: *const Mask, cu: CodeUnit) ?u64 {
        if (self.isIn(cu)) {
            const m = cu.lowMask();
            return @popCount(self.m & m);
        } else {
            return null;
        }
    }
};

//| Tests

const expect = std.testing.expect;
const expectEqual = std.testing.expectEqual;

test split {
    const A = split('A');
    try expectEqual(A.kind, .hi);
    const zero = split('0');
    try expectEqual(zero.kind, .low);
    const lambda = "λ";
    const lead = split(lambda[0]);
    try expectEqual(lead.kind, .lead);
    const follow = split(lambda[1]);
    try expectEqual(follow.kind, .follow);
    // mask property check
    for (0..255) |i| {
        const cu = split(@truncate(i));
        try expectEqual(cu.lowMask() & cu.hiMask(), 0);
        if (cu.body % 63 != 0) // low mask for 63 is all zeros
            try expect(cu.lowMask() > cu.hiMask());
    }
}

test "mask tests" {
    var mask = Mask.toMask(0);
    const B = split('B');
    mask.add(B);
    try expect(mask.isIn(B));
    const D = split('D');
    mask.add(D);
    mask.add(split('Z'));
    try expectEqual(mask.higherThan(B).?, 2);
    try expectEqual(mask.lowerThan(D), 1);
    try expectEqual(mask.lowerThan(split('?')), null);
}

//
//test "bleh" {
//    const C = split('C');
//    std.debug.print("body: {d} hiMask: 0b{b:0>64}\n", .{ C.body, C.hiMask() });
//    std.debug.print("       lowMask: 0b{b:0>64}\n", .{C.lowMask()});
//    std.debug.print("        inMask: 0b{b:0>64}\n", .{C.inMask()});
//    const Qmark = split('?');
//    std.debug.print("body: {d} hiMask: 0b{b:0>64}\n", .{ Qmark.body, Qmark.hiMask() });
//    std.debug.print("        lowMask: 0b{b:0>64}\n", .{Qmark.lowMask()});
//    const six_three = split('O');
//    std.debug.print("body: {d} hiMask: 0b{b:0>64}\n", .{ six_three.body, six_three.hiMask() });
//    std.debug.print("        lowMask: 0b{b:0>64}\n", .{six_three.lowMask()});
//    const at = split('@');
//    std.debug.print("body: {d} lowMask: 0b{b:0>64}\n", .{ at.body, at.lowMask() });
//    std.debug.print("         hiMask: 0b{b:0>64}\n", .{at.hiMask()});
//    try expect(3 == 3);
//}
//
