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
};

/// Cast raw byte to CodeUnit
pub inline fn split(b: u8) CodeUnit {
    return @bitCast(b);
}

/// Cast raw u64 to IntegerBitSet(64)
pub inline fn wordmask(w: u64) IntegerBitSet(64) {
    return @bitCast(w);
}

/// Downcast IntegerBitSet(64) to underlying word
pub inline fn unwordmask(m: IntegerBitSet(64)) u64 {
    return @bitCast(m);
}

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
}
