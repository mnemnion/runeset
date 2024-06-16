//!  libruneset: a Zig library for fast utf-8 charsets
//!
//! The elements namespace provides data structures for working
//! with UTF-8 encoded data.

const std = @import("std");
const testing = std.testing;

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
        return (@as(u64, 1) << self.body) - 1;
    }

    /// Mask off all bits <= cu.body
    pub inline fn lowMask(self: *const CodeUnit) u64 {
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

/// Rune is a data structure which might be a UTF-8 scalar value.
/// Working with UTF-8 encoded values is its purpose, but it can
/// also have surrogate codepoints, overlong encodings, and garbage
/// data.  Creating an invalid Rune must be done deliberately, the
/// main constructor will return null for bad data.
///
/// A Rune is a packed u32 with four u8 fields: a, b, c, and d.
/// If all four are 0, this represents NUL or U+0.  If a is greater
/// than 0, and any of b, c, or d are 0, they are not a part of the
/// value which the Rune encodes.  On little endian hardware, this is
/// the same data layout as the same UTF-8 data in a string.
///
/// Two Runes which both encode valid codepoints, compared with @bitCast,
/// will sort according to their codepoint order.  While they can be
/// converted to scalar codepoints, they maintain their UTF-8 encoding
/// internally, such that adding them to strings, formatting them, and
/// so on, is straightforward and very fast.
pub const Rune = packed struct(u32) {
    a: u8,
    b: u8,
    c: u8,
    d: u8,

    // Make a Rune from a slice of u8, provided that this slice is
    // generalized UTF-8: encoding a codepoint, whether or not it is
    // an allowed scalar value.
    pub fn fromSlice(slice: []const u8) ?Rune {
        const a = codeunit(slice[0]);
        const nBytes = a.nBytes();
        if (nBytes) |nB| {
            if (nBytes > slice.len) return null;
            switch (a) {
                .low, .hi => return Rune{
                    .a = slice[0],
                    .b = 0,
                    .c = 0,
                    .d = 0,
                },
                .follow => null,
                .lead => {
                    // const nBytes = a.nBytes();
                    switch (nB) {
                        2 => {
                            if (codeunit(slice[1]).kind == .follow)
                                return Rune{
                                    .a = slice[0],
                                    .b = slice[1],
                                    .c = 0,
                                    .d = 0,
                                }
                            else
                                return null;
                        },
                        3 => if (codeunit(slice[1]).kind == .follow and codeunit(slice[2]).kind == .follow)
                            return Rune{
                                .a = slice[0],
                                .b = slice[1],
                                .c = slice[2],
                                .d = 0,
                            }
                        else
                            return null,
                        4 => if (codeunit(slice[1]).kind == .follow and codeunit(slice[2]).kind == .follow and codeunit(slice[3]).kind == .follow)
                            return Rune{
                                .a = slice[0],
                                .b = slice[1],
                                .c = slice[2],
                                .d = slice[3],
                            }
                        else
                            return null,
                        else => unreachable,
                    }
                },
            }
        } else return null;
    }

    /// Return a Rune from a slice of u8. Caller guarantees that [0] is addressable.
    /// If the sequence is not valid generalized UTF-8, the Rune will be the first
    /// byte of the slice.
    pub fn fromSliceAllowInvalid(slice: []const u8) Rune {
        const maybeRune = Rune.fromSlice(slice);
        if (maybeRune) |rune|
            return rune
        else
            return Rune{ .a = slice[0], .b = 0, .c = 0, .d = 0 };
    }

    /// Return a tuple containing the bytes of a Rune.
    pub fn toBytes(self: Rune) struct { u8, u8, u8, u8 } {
        return .{ self.a, self.b, self.c, self.d };
    }

    /// Copy the bytes of a Rune to the start of the provided slice.
    /// Caller is responsible for assuring the slice has sufficient
    /// room.  Returns the number of bytes copied.
    pub fn copyToSlice(self: Rune, slice: []u8) usize {
        slice[0] = self.a;
        if (self.b == 0) return 1;
        slice[1] = self.b;
        if (self.c == 0) return 2;
        slice[2] = self.c;
        if (self.d == 0) return 3;
        slice[3] = self.d;
        return 4;
    }

    // TODO fromCodepoint, writeToWriter, isScalarValue, isCodepoint,
    //
};

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
        std.debug.assert(self.isIn(cu));
        self.m &= ~cu.inMask();
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
    pub inline fn isIn(self: Mask, cu: CodeUnit) bool {
        return self.m | cu.inMask() == self.m;
    }

    /// Test if a u6 element is present in mask
    pub inline fn isElem(self: Mask, u: u6) bool {
        return self.m | @as(u64, 1) << u == self.m;
    }

    /// Return number of bytes lower than cu.body in mask,
    /// if cu inhabits the mask.  Otherwise return null.
    pub inline fn lowerThan(self: Mask, cu: CodeUnit) ?u64 {
        if (self.isIn(cu)) {
            const m = cu.hiMask();
            return @popCount(self.m & m);
        } else {
            return null;
        }
    }

    /// Return number of bytes higher than cu.body in mask,
    /// if cu inhabits the mask.  Otherwise return null.
    pub inline fn higherThan(self: Mask, cu: CodeUnit) ?u64 {
        if (self.isIn(cu)) {
            const m = cu.lowMask();
            return @popCount(self.m & m);
        } else {
            return null;
        }
    }

    /// Check u6 element for membership. Used to iterate, and
    /// in set operations on RuneSets.
    pub inline fn bitSet(self: Mask, member: u6) bool {
        return self.m | @as(u64, 1) << member == self.m;
    }

    pub fn iterElements(self: Mask) MaskElements {
        return MaskElements{ .mask = self };
    }

    pub fn iterElemBack(self: Mask) MaskElemBack {
        return MaskElemBack{ .mask = self };
    }

    pub fn iterCodeUnits(self: Mask, kind: RuneKind) MaskCodeUnits {
        return MaskCodeUnits{ .mIter = self.iterElements(), .kind = kind };
    }

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

/// Mask Iterator. maskElements.next() will provide all
/// u6 elements of the Mask.
pub const MaskElements = struct {
    mask: Mask,
    i: u8 = 0,
    pub fn next(itr: *MaskElements) ?u6 {
        var result: ?u6 = null;
        while (itr.i < 64) {
            const e: u6 = @intCast(itr.i);
            if (itr.mask.bitSet(e)) {
                result = e;
                itr.i += 1;
                break;
            } else {
                itr.i += 1;
            }
        }
        return result;
    }
};

pub const MaskElemBack = struct {
    mask: Mask,
    i: i8 = 63,

    pub fn next(itr: *MaskElemBack) ?u6 {
        var result: ?u6 = null;
        while (itr.i >= 0) {
            const e: u6 = @intCast(itr.i);
            if (itr.mask.bitSet(e)) {
                result = e;
                itr.i -= 1;
                break;
            } else {
                itr.i -= 1;
            }
        }
        return result;
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

/// Iterate all CodeUnits of a Mask, backward
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

// test "low mask" {
//     const c0 = codeunit('\x00');
//     std.debug.print("body: {d}\n   lowMask: 0b{b:0>64}\n", .{ c0.body, c0.lowMask() });
//     std.debug.print("    hiMask: 0b{b:0>64}\n", .{c0.hiMask()});
//     std.debug.print("    inMask: 0b{b:0>64}\n", .{c0.inMask()});
//     const c1 = codeunit('\x01');
//     std.debug.print("body: {d}\n   lowMask: 0b{b:0>64}\n", .{ c1.body, c1.lowMask() });
//     std.debug.print("    hiMask: 0b{b:0>64}\n", .{c1.hiMask()});
//     std.debug.print("    inMask: 0b{b:0>64}\n", .{c1.inMask()});
//     const c27 = codeunit('\x1b');
//     std.debug.print("body: {d}\n   lowMask: 0b{b:0>64}\n", .{ c27.body, c27.lowMask() });
//     std.debug.print("    hiMask: 0b{b:0>64}\n", .{c27.hiMask()});
//     std.debug.print("    inMask: 0b{b:0>64}\n", .{c27.inMask()});
//     const c63 = codeunit('?');
//     std.debug.print("body: {d}\n   lowMask: 0b{b:0>64}\n", .{ c63.body, c63.lowMask() });
//     std.debug.print("    hiMask: 0b{b:0>64}\n", .{c63.hiMask()});
//     std.debug.print("    inMask: 0b{b:0>64}\n", .{c63.inMask()});
// }

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
