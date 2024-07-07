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

/// A Rune is a data structure which may or may not hold a UTF-8 scalar
/// value.  While working with UTF-8 encoded values is the intention of
/// the struct, it can also hold surrogate codepoints and garbage data.
/// Creating an invalid Rune must be done deliberately, as the provided
/// API will return `null` for bad data by default.
///
/// A Rune is a packed u32 with four u8 fields: a, b, c, and d.  If all
/// four are 0, this represents NUL or U+0.  If a is greater than 0 and
/// any of b, c, or d are 0, they are not a part of the value which the
/// Rune encodes.
///
/// These fields are defined in the backing `u32` such that `a` is most
/// significant, and `d` is least.
///
/// Two Runes which both encode valid codepoints, compared as raw data,
/// will sort according to their codepoint order.  These maintain UTF-8
/// encoding internally, such that adding them to a string or streaming
/// their values, is straightforward and very fast.
///
/// An important category of Rune is a *conformant* Rune.  This will be
/// either a single byte of invalid data at `.a`, or is a complete byte
/// sequenced codepoint, whether or not that specific sequence is valid
/// UTF-8: what the WTF-8 standard calls "generalized" UTF-8.  Overlong
/// encodings are deliberately excluded from conformance.
///
/// The Rune API functions will return only conforming Runes as we have
/// defined above.  Functions which assume conformance will assert that
/// assumption in safety-checked build modes.
///
/// Most functions operating on a Rune assume that it conforms as we've
/// previously described.  Some functions come in an `AnyRune` variant,
/// those will give correct results, no matter what the backing integer
/// happens to contain.
pub const Rune = packed struct(u32) {
    d: u8,
    c: u8,
    b: u8,
    a: u8,

    // TODO pack malformed data into `d`, not `a`.

    /// Make a Rune from a slice of `u8`, provided that this slice is
    /// generalized UTF-8: encoding a codepoint, whether or not it is
    /// an allowed scalar value.  This is safe to call on any invalid
    /// data, given that `[0]` is addressable.  It will return `null`
    /// if the data is invalid as defined above.
    pub fn fromSlice(slice: []const u8) ?Rune {
        const a = codeunit(slice[0]);
        const nBytes = a.nBytes();
        if (nBytes) |nB| {
            if (nB > slice.len) return null;
            switch (a.kind) {
                .low, .hi => return Rune{
                    .a = slice[0],
                    .b = 0,
                    .c = 0,
                    .d = 0,
                }, // nBytes is null for this case
                .follow => unreachable,
                .lead => {
                    switch (slice[0]) {
                        // 0xc0 and 0xc1 are invalid (overlong)
                        0xc2...0xdf => {
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
                        0xe0 => {
                            // Omit overlong encoding.
                            if (slice[1] >= 0xa0 and slice[1] <= 0xbf and codeunit(slice[2]).kind == .follow)
                                return Rune{
                                    .a = slice[0],
                                    .b = slice[1],
                                    .c = slice[2],
                                    .d = 0,
                                }
                            else
                                return null;
                        }, // Surrogates are allowed.
                        0xe1...0xef => {
                            if (codeunit(slice[1]).kind == .follow and codeunit(slice[2]).kind == .follow)
                                return Rune{
                                    .a = slice[0],
                                    .b = slice[1],
                                    .c = slice[2],
                                    .d = 0,
                                }
                            else
                                return null;
                        },
                        0xf0 => {
                            if (codeunit(slice[1]).kind == .follow and codeunit(slice[2]).kind == .follow and codeunit(slice[3]).kind == .follow) {
                                // Omit overlong encoding.
                                if (slice[1] < 0x90) {
                                    return null;
                                }
                                return Rune{
                                    .a = slice[0],
                                    .b = slice[1],
                                    .c = slice[2],
                                    .d = slice[3],
                                };
                            } else return null;
                        },
                        0xf1...0xf3 => {
                            if (codeunit(slice[1]).kind == .follow and codeunit(slice[2]).kind == .follow and codeunit(slice[3]).kind == .follow) {
                                return Rune{
                                    .a = slice[0],
                                    .b = slice[1],
                                    .c = slice[2],
                                    .d = slice[3],
                                };
                            } else return null;
                        },
                        0xf4 => {
                            if (codeunit(slice[1]).kind == .follow and codeunit(slice[2]).kind == .follow and codeunit(slice[3]).kind == .follow) {
                                // Omit values without an equivalent codepoint.
                                if (slice[1] > 0x8f) {
                                    return null;
                                }
                                return Rune{
                                    .a = slice[0],
                                    .b = slice[1],
                                    .c = slice[2],
                                    .d = slice[3],
                                };
                            } else return null;
                        }, // nBytes handles overhigh lead bytes
                        else => unreachable,
                    }
                },
            }
        } else return null;
    }

    /// Return a Rune from a `[]const u8`.  Caller guarantees that [0] is
    /// addressable.  If the sequence is not valid generalized UTF-8, the
    /// Rune will be the first byte of the slice.  Thus the returned Rune
    /// will always be conformant.
    pub fn fromSliceAllowInvalid(slice: []const u8) Rune {
        const maybeRune = Rune.fromSlice(slice);
        if (maybeRune) |rune|
            return rune
        else
            return Rune{
                .a = slice[0],
                .b = 0,
                .c = 0,
                .d = 0,
            };
    }

    /// Return a tuple containing the bytes of a Rune.  Use this if you
    /// want to destructure some bytes into variables, or `toByteArray`
    /// otherwise.
    pub fn toByteTuple(rune: Rune) struct { u8, u8, u8, u8 } {
        return .{ rune.a, rune.b, rune.c, rune.d };
    }

    /// Return a `[4]u8` of the bytes in the Rune.
    pub fn toByteArray(rune: Rune) [4]u8 {
        return .{ rune.a, rune.b, rune.c, rune.d };
    }

    /// Return the number of bytes which contain data.  This produces a
    /// valid result for any `Rune`, conformant or not.
    pub fn byteCount(rune: Rune) usize {
        // `.a` always contains data, which may include any `u8`.
        if (rune.b == 0)
            return 1
        else if (rune.c == 0)
            return 2
        else if (rune.d == 0)
            return 3
        else
            return 4;
    }

    const A_MAX = 0x80;
    const B_MAX = 0x800;
    const C_MAX = 0x10000;
    const D_MAX = 0x110000;
    const MASK_B: u32 = 0xc0;
    const MASK_C: u32 = 0xe0;
    const MASK_D: u32 = 0xf0;
    const M_FOLLOW: u32 = 0x80;
    const M_LOW: u32 = 0x3f;

    /// Return a `Rune` which encodes a UTF-8 code point sequence which
    /// corresponds to the integer value provided.  This will produce a
    /// `Rune` for invalid surrogate codepoints.  If the integer is out
    /// of range, this function will return an error.
    pub fn fromCodepoint(cp: u21) !Rune {
        if (cp < A_MAX) {
            return Rune{
                .a = @intCast(cp),
                .b = 0,
                .c = 0,
                .d = 0,
            };
        } else if (cp < B_MAX) {
            const a: u8 = @intCast(MASK_B | (cp >> 6));
            const b: u8 = @intCast(M_FOLLOW | (cp & M_LOW));
            return Rune{
                .a = a,
                .b = b,
                .c = 0,
                .d = 0,
            };
        } else if (cp < C_MAX) {
            const a: u8 = @intCast(MASK_C | (cp >> 12));
            const b: u8 = @intCast(M_FOLLOW | ((cp >> 6) & M_LOW));
            const c: u8 = @intCast(M_FOLLOW | (cp & M_LOW));
            return Rune{
                .a = a,
                .b = b,
                .c = c,
                .d = 0,
            };
        } else if (cp < D_MAX) {
            const a: u8 = @intCast(MASK_D | (cp >> 18));
            const b: u8 = @intCast(M_FOLLOW | ((cp >> 12) & M_LOW));
            const c: u8 = @intCast(M_FOLLOW | ((cp >> 6) & M_LOW));
            const d: u8 = @intCast(M_FOLLOW | (cp & M_LOW));
            return Rune{
                .a = a,
                .b = b,
                .c = c,
                .d = d,
            };
        } else {
            return error.CodepointTooHigh;
        }
    }

    /// Convert a `Rune` to the codepoint it represents.  This function
    /// assumes a conformant `Rune`.  An error is returned If called on
    /// malformed data.  This will return a codepoint for a `Rune` that
    /// represents a surrogate.  Conformance is asserted in safe modes.
    /// Note that by their nature, Runes which contain an overlong code
    /// unit sequence will decode in fast modes to a spurious value.
    pub fn toCodepoint(rune: Rune) !u21 {
        if (rune.a < A_MAX) return @intCast(rune.a);
        if (rune.b == 0) return error.InvalidUnicode;
        if (rune.a & 0xe0 == 0xc0) {
            assert(rune.b & 0xc0 == 0x80);
            const a_wide = @as(u32, rune.a);
            return @intCast(((a_wide & 0x1f) << 6) | (rune.b & 0x3f));
        } else if (rune.a & 0xf0 == 0xe0) {
            assert(rune.b & 0xc0 == 0x80);
            assert(rune.c & 0xc0 == 0x80);
            const a_wide = @as(u32, rune.a);
            const b_wide = @as(u32, rune.b);
            return @intCast(((a_wide & 0x0f) << 12) | ((b_wide & 0x3f) << 6) | (rune.c & 0x3f));
        } else if (rune.a & 0xf8 == 0xf0) {
            assert(rune.b & 0xc0 == 0x80);
            assert(rune.c & 0xc0 == 0x80);
            assert(rune.d & 0xc0 == 0x80);
            assert(!(rune.a == 0xf4 and rune.b > 0x8f));
            const a_wide = @as(u32, rune.a);
            const b_wide = @as(u32, rune.b);
            const c_wide = @as(u32, rune.c);
            return @intCast(((a_wide & 0x07) << 18) | ((b_wide & 0x3f) << 12) | ((c_wide & 0x3f) << 6) | (rune.d & 0x3f));
        } else return error.InvalidUnicode;
    }

    /// Convert a `Rune` to the codepoint it represents.  Calling this
    /// function on a `Rune` which isn't a codepoint is safety-checked
    ///     illegal behavior.
    pub fn toCodepointAssumeValid(rune: Rune) u21 {
        return rune.toCodepoint() catch unreachable;
    }

    /// Return the backing `u32` of the Rune.
    ///
    /// This may be used for lexical comparison, but isn't equal to the
    /// Unicode codeunit value of the `Rune`, except for ASCII one-byte
    /// codepoints.  If the `Rune` is conformant, any malformed data is
    /// going to cluster in an identifiable region of a lexicographical
    /// sort, greater than any ASCII `Rune`, and less than any two-byte
    /// `Rune`.
    pub fn rawInt(rune: Rune) u32 {
        return @bitCast(rune);
    }

    /// Copy every byte of a `Rune` to the start of the slice.  This is
    /// legal to call on any `Rune`.  Returns the total amount of bytes
    /// copied.  Caller assures that the slice has sufficient room.
    pub fn copyToSlice(rune: Rune, slice: []u8) usize {
        slice[0] = rune.a;
        if (rune.b == 0) return 1;
        slice[1] = rune.b;
        if (rune.c == 0) return 2;
        slice[2] = rune.c;
        if (rune.d == 0) return 3;
        slice[3] = rune.d;
        return 4;
    }

    /// Test whether the `Rune` encodes the codepoint argument, whether
    /// the Rune is conformant or not.
    pub fn equalToCodepoint(rune: Rune, cp: u21) bool {
        const r_code = rune.toCodepoint() catch return false;
        return (r_code == cp);
    }

    /// Tests whether the `Rune` represents a valid (generalized) UTF-8
    /// sequence, or malformed data.  Assumes a conformant `Rune`.
    pub fn isCodepoint(rune: Rune) bool {
        if (rune.a > 0x7f and rune.b == 0) return false;
        if (safeMode) {
            const nBytes = codeunit(rune.a).nBytes();
            if (nBytes) |nB| {
                switch (nB) {
                    1 => {
                        assert(rune.b == 0);
                        assert(rune.c == 0);
                        assert(rune.d == 0);
                    },
                    2 => {
                        assert(codeunit(rune.b).kind == .follow);
                        assert(rune.c == 0);
                        assert(rune.d == 0);
                    },
                    3 => {
                        assert(codeunit(rune.b).kind == .follow);
                        assert(codeunit(rune.c).kind == .follow);
                        assert(rune.d == 0);
                    },
                    4 => {
                        assert(codeunit(rune.b).kind == .follow);
                        assert(codeunit(rune.c).kind == .follow);
                        assert(codeunit(rune.d).kind == .follow);
                    },
                    else => unreachable,
                }
            } else unreachable;
        }
        return true;
    }

    /// Test if the `Rune` encodes a valid generalized UTF-8 codepoint.
    /// This may be called on a non-conforming `Rune`.
    pub fn isCodepointAnyRune(rune: Rune) bool {
        if (rune.a <= 0x7f) {
            if (rune.b != 0 or rune.c != 0 or rune.d != 0) {
                return false;
            } else {
                return true;
            }
        }
        const b = rune.b;
        const c = rune.c;
        const d = rune.d;
        switch (rune.a) {
            0xc2...0xdf => {
                if (codeunit(b).kind == .follow and c == 0 and d == 0)
                    return true
                else
                    return false;
            },
            0xe0 => {
                // Omit overlong encoding.
                if (b >= 0xa0 and b <= 0xbf and codeunit(c).kind == .follow and d == 0)
                    return true
                else
                    return false;
            },
            0xe1...0xef => {
                if (codeunit(b).kind == .follow and codeunit(c).kind == .follow and d == 0)
                    return true
                else
                    return false;
            },
            0xf0 => {
                if (codeunit(b).kind == .follow and codeunit(c).kind == .follow and codeunit(d).kind == .follow) {
                    // Omit overlong encoding.
                    if (b < 0x90) {
                        return false;
                    } else {
                        return true;
                    }
                } else {
                    return false;
                }
            },
            0xf1...0xf3 => {
                if (codeunit(b).kind == .follow and codeunit(c).kind == .follow and codeunit(d).kind == .follow) {
                    return true;
                } else {
                    return false;
                }
            },
            0xf4 => {
                if (codeunit(b).kind == .follow and codeunit(c).kind == .follow and codeunit(d).kind == .follow) {
                    // Omit values without an equivalent codepoint.
                    if (b > 0x8f) {
                        return false;
                    } else {
                        return true;
                    }
                } else {
                    return false;
                }
            },
            else => return false,
        }
    }

    /// Test whether a `Rune` encodes a valid Unicode scalar value.  It
    /// makes no assumptions about the contents of the `Rune`, as such,
    /// it validates the byte sequence fully.
    pub fn isScalarValueAnyRune(rune: Rune) bool {
        // Reference: Table 3-7 of The Unicode Standard 15.0
        // https://www.unicode.org/versions/Unicode15.0.0/UnicodeStandard-15.0.pdf
        if (rune.a <= 0x7f) {
            // 1-byte sequence: 00..7F
            if (rune.b == 0 and rune.c == 0 and rune.d == 0) {
                return true;
            } else {
                return false;
            }
        } else if (rune.b == 0) {
            return false;
        } else if (rune.a >= 0xc2 and rune.a <= 0xdf) {
            // 2-byte sequence: C2..DF 80..BF
            if (rune.c == 0 and rune.d == 0) {
                return (rune.b & 0xc0) == 0x80;
            } else {
                return false;
            }
        } else if (rune.a == 0xe0) {
            // 3-byte sequence: E0 A0..BF 80..BF
            if (rune.d == 0) {
                return (rune.b >= 0xa0 and rune.b <= 0xbf) and (rune.c & 0xc0) == 0x80;
            } else {
                return false;
            }
        } else if (rune.a >= 0xe1 and rune.a <= 0xec) {
            // 3-byte sequence: E1..EC 80..BF 80..BF
            if (rune.d == 0) {
                return (rune.b & 0xc0) == 0x80 and (rune.c & 0xc0) == 0x80;
            } else {
                return false;
            }
        } else if (rune.a == 0xed) {
            // 3-byte sequence: ED 80..9F 80..BF
            if (rune.d == 0) {
                return (rune.b >= 0x80 and rune.b <= 0x9f) and (rune.c & 0xc0) == 0x80;
            } else {
                return false;
            }
        } else if (rune.a >= 0xee and rune.a <= 0xef) {
            // 3-byte sequence: EE..EF 80..BF 80..BF
            if (rune.d == 0) {
                return (rune.b & 0xc0) == 0x80 and (rune.c & 0xc0) == 0x80;
            } else {
                return false;
            }
        } else if (rune.a == 0xf0) {
            // 4-byte sequence: F0 90..BF 80..BF 80..BF
            return (rune.b >= 0x90 and rune.b <= 0xbf) and (rune.c & 0xc0) == 0x80 and (rune.d & 0xc0) == 0x80;
        } else if (rune.a >= 0xf1 and rune.a <= 0xf3) {
            // 4-byte sequence: F1..F3 80..BF 80..BF 80..BF
            return (rune.b & 0xc0) == 0x80 and (rune.c & 0xc0) == 0x80 and (rune.d & 0xc0) == 0x80;
        } else if (rune.a == 0xf4) {
            // 4-byte sequence: F4 80..8F 80..BF 80..BF
            return (rune.b >= 0x80 and rune.b <= 0x8f) and (rune.c & 0xc0) == 0x80 and (rune.d & 0xc0) == 0x80;
        } else {
            return false;
        }
    }

    /// Test whether the `Rune` encodes a UTF-8 scalar value.  This may
    /// only be called on conformant `Runes`, and this property will be
    /// asserted in safety build modes.  For `Rune`s which are possibly
    /// non-conforming, use `isScalarValueAnyRune`.
    pub fn isScalarValue(rune: Rune) bool {
        // Reference: Table 3-7 of The Unicode Standard 15.0
        // https://www.unicode.org/versions/Unicode15.0.0/UnicodeStandard-15.0.pdf
        if (rune.a <= 0x7f) {
            // 1-byte sequence: 00..7F
            return true;
        } else if (rune.b == 0) {
            return false;
        } else if (rune.a >= 0xc2 and rune.a <= 0xdf) {
            assert(rune.b & 0xc0 == 0x80);
            return true;
        } else if (rune.a >= 0xe0 and rune.a <= 0xec) {
            assert(!(rune.a == 0xe0 and rune.b < 0xa0));
            assert(rune.b & 0xc0 == 0x80);
            assert(rune.c & 0xc0 == 0x80);
            return true;
        } else if (rune.a == 0xed) {
            // Avoid surrogates
            return (rune.b >= 0x80 and rune.b <= 0x9f) and (rune.c & 0xc0) == 0x80;
        } else if (rune.a == 0xee or rune.a == 0xef) {
            assert(rune.b & 0xc0 == 0x80);
            assert(rune.c & 0xc0 == 0x80);
            return true;
        } else if (rune.a >= 0xf0 and rune.a <= 0xf4) {
            assert(!(rune.a == 0xf0 and rune.b < 0x90));
            assert(rune.b & 0xc0 == 0x80);
            assert(rune.c & 0xc0 == 0x80);
            assert(rune.d & 0xc0 == 0x80);
            assert(!(rune.a == 0xf4 and rune.b > 0x8f));
            return true;
        } else { // conformant runes will not have over-high leads unless b == 0
            unreachable;
        }
    }
};

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
    i: u8 = 0,
    pub fn next(itr: *MaskElements) ?u6 {
        var result: ?u6 = null;
        while (itr.i < 64) {
            const e: u6 = @intCast(itr.i);
            if (itr.mask.isElem(e)) {
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

/// Reverse Mask iterator, of u6
pub const MaskElemBack = struct {
    mask: Mask,
    i: i8 = 63,

    pub fn next(itr: *MaskElemBack) ?u6 {
        var result: ?u6 = null;
        while (itr.i >= 0) {
            const e: u6 = @intCast(itr.i);
            if (itr.mask.isElem(e)) {
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

test "Rune tests" {
    const rA = Rune.fromCodepoint(0x41) catch unreachable;
    try expectEqual(0x41, rA.a);
    try expect(rA.b == 0 and rA.c == 0 and rA.d == 0);
    try expectEqual(0x41, rA.toCodepoint());
    const strA = "A";
    try expectEqual(strA[0], rA.a);
    const rA2 = Rune.fromSlice(strA).?;
    try expectEqualDeep(rA, rA2);
    try expect(rA.equalToCodepoint('A'));
    try expect(rA.isCodepoint());
    try expect(rA.isCodepointAnyRune());
    try expect(rA.isScalarValueAnyRune());
    try expect(rA.isScalarValue());
    // greek Ω, U+3a9
    const rB = Rune.fromCodepoint(0x3a9) catch unreachable;
    try expectEqual(0xce, rB.a);
    try expectEqual(0xa9, rB.b);
    try expect(rB.c == 0 and rB.d == 0);
    try expectEqual(0x3a9, rB.toCodepoint());
    const strB = "Ω";
    try expectEqual(strB[0], rB.a);
    try expectEqual(strB[1], rB.b);
    const rB2 = Rune.fromSlice(strB).?;
    try expectEqualDeep(rB, rB2);
    try expect(rB.equalToCodepoint('Ω'));
    try expect(rB.isCodepoint());
    try expect(rB.isCodepointAnyRune());
    try expect(rB.isScalarValueAnyRune());
    try expect(rB.isScalarValue());
    // empty set ∅, U+2205
    const rC = Rune.fromCodepoint(0x2205) catch unreachable;
    try expectEqual(0xe2, rC.a);
    try expectEqual(0x88, rC.b);
    try expectEqual(0x85, rC.c);
    try expectEqual(0, rC.d);
    try expectEqual(0x2205, rC.toCodepoint());
    const strC = "∅";
    try expectEqual(strC[0], rC.a);
    try expectEqual(strC[1], rC.b);
    try expectEqual(strC[2], rC.c);
    const rCclone = Rune.fromSlice(strC).?;
    try expectEqualDeep(rC, rCclone);
    try expect(rC.equalToCodepoint('∅'));
    try expect(rC.isCodepoint());
    try expect(rC.isCodepointAnyRune());
    try expect(rC.isScalarValueAnyRune());
    try expect(rC.isScalarValue());
    const rC2 = Rune.fromCodepoint('ࠐ') catch unreachable;
    try expect(rC2.equalToCodepoint('ࠐ'));
    try expect(rC2.isCodepoint());
    try expect(rC2.isCodepointAnyRune());
    try expect(rC2.isScalarValueAnyRune());
    try expect(rC2.isScalarValue());
    const rC2a = rC2.toByteArray();
    try expectEqualDeep(rC2, Rune.fromSlice(&rC2a));
    const rC3 = Rune.fromCodepoint('\u{efd0}') catch unreachable;
    try expect(rC3.equalToCodepoint('\u{efd0}'));
    try expect(rC3.isCodepoint());
    try expect(rC3.isCodepointAnyRune());
    try expect(rC3.isScalarValueAnyRune());
    try expect(rC3.isScalarValue());
    const rC3a = rC3.toByteArray();
    try expectEqualDeep(rC3, Rune.fromSlice(&rC3a));
    // thinking emoji 🤔, U+1f914
    const rD = Rune.fromCodepoint(0x1f914) catch unreachable;
    try expectEqual(rD.toCodepoint(), rD.toCodepointAssumeValid());
    try expectEqual(0xf0, rD.a);
    try expectEqual(0x9f, rD.b);
    try expectEqual(0xa4, rD.c);
    try expectEqual(0x94, rD.d);
    try expectEqual(0x1f914, rD.toCodepoint());
    const strD = "🤔";
    try expectEqual(strD[0], rD.a);
    try expectEqual(strD[1], rD.b);
    try expectEqual(strD[2], rD.c);
    try expectEqual(strD[3], rD.d);
    const rD2 = Rune.fromSlice(strD).?;
    try expectEqualDeep(rD, rD2);
    const rDs = rD.toByteArray();
    try std.testing.expectEqualStrings(strD, &rDs);
    try expect(rD.equalToCodepoint('🤔'));
    try expect(rD.isCodepoint());
    try expect(rD.isCodepointAnyRune());
    try expect(rD.isScalarValueAnyRune());
    try expect(rD.isScalarValue());
    // Random high codepoint with useful lead byte
    const rDh1 = Rune.fromCodepoint(0xa1350) catch unreachable;
    try expect(rDh1.equalToCodepoint('\u{a1350}'));
    try expect(rDh1.isCodepoint());
    try expect(rDh1.isCodepointAnyRune());
    try expect(rDh1.isScalarValueAnyRune());
    try expect(rDh1.isScalarValue());
    const rDh1a = rDh1.toByteArray();
    try expectEqualDeep(rDh1, Rune.fromSlice(&rDh1a));
    // Same but higher
    const rDh2 = Rune.fromCodepoint(0x10ff00) catch unreachable;
    const rDh2a = rDh2.toByteArray();
    try expectEqualDeep(rDh2, Rune.fromSlice(&rDh2a));
    try expect(rDh2.equalToCodepoint('\u{10ff00}'));
    try expect(rDh2.isCodepoint());
    try expect(rDh2.isCodepointAnyRune());
    try expect(rDh2.isScalarValueAnyRune());
    try expect(rDh2.isScalarValue());
    // Too high
    try expectError(error.CodepointTooHigh, Rune.fromCodepoint(0x110000));
    try expectError(error.CodepointTooHigh, Rune.fromCodepoint(0x111000));
    try expectError(error.CodepointTooHigh, Rune.fromCodepoint(std.math.maxInt(u21)));
}

test "invalid Rune tests" {
    try expectEqual(null, Rune.fromSlice("\x81abc"));
    try expectEqual(null, Rune.fromSlice("\x9f0"));
    try expectEqual(null, Rune.fromSlice("\xcer"));
    try expectEqual(null, Rune.fromSlice("\xff\xff\xff"));
    try expectEqual(null, Rune.fromSlice("\xe2✓"));
    try expectEqual(null, Rune.fromSlice("\xf0\x9f\x98q"));
    try expectEqual(null, Rune.fromSlice("\xf1\x9f\x98q"));
    // overlong
    try expectEqual(null, Rune.fromSlice("\xf0\x80\x98\xa0"));
    try expectEqual(null, Rune.fromSlice("\xe0\x80\x98"));
    // too high
    try expectEqual(null, Rune.fromSlice("\xf4\x90\x82\x83"));
    try expectEqual(null, Rune.fromSlice("\xfa\x90\x82\x83"));
    try expectError(
        error.InvalidUnicode,
        (Rune{
            .a = 0xff,
            .b = 0xff,
            .c = 0,
            .d = 0,
        }).toCodepoint(),
    );
    const badA = Rune{ .a = 0x32, .b = 0xff, .c = 0, .d = 0 };
    try expectEqual(false, badA.isCodepointAnyRune());
    try expectEqual(false, badA.isScalarValueAnyRune());
    const badA2 = Rune{ .a = 0xe0, .b = 0, .c = 0, .d = 0 };
    try expectEqual(false, badA2.isCodepointAnyRune());
    try expectEqual(false, badA2.isScalarValueAnyRune());
    // This is conforming malformed data TODO may change storage to d, which would
    // fail this test!
    try expectEqual(false, badA2.isScalarValue());
    const badB = Rune{ .a = 0xc3, .b = 0xff, .c = 0, .d = 0 };
    try expectEqual(false, badB.isCodepointAnyRune());
    try expectEqual(false, badB.isScalarValueAnyRune());
    const badB2 = Rune{ .a = 0xc3, .b = 0x81, .c = 0xff, .d = 0 };
    try expectEqual(false, badB2.isCodepointAnyRune());
    try expectEqual(false, badB2.isScalarValueAnyRune());
    const badC = Rune{ .a = 0xe0, .b = 0x81, .c = 0x90, .d = 0 };
    try expectEqual(false, badC.isCodepointAnyRune());
    try expectEqual(false, badC.isScalarValueAnyRune());
    const badC2 = Rune{ .a = 0xe0, .b = 0x80, .c = 0x81, .d = 0xff };
    try expectEqual(false, badC2.isCodepointAnyRune());
    try expectEqual(false, badC2.isScalarValueAnyRune());
    const badC3 = Rune{ .a = 0xeD, .b = 0xff, .c = 0, .d = 0xff };
    try expectEqual(false, badC3.isCodepointAnyRune());
    try expectEqual(false, badC3.isScalarValueAnyRune());
    const badC4 = Rune{ .a = 0xee, .b = 0xff, .c = 0, .d = 0xff };
    try expectEqual(false, badC4.isCodepointAnyRune());
    try expectEqual(false, badC4.isScalarValueAnyRune());
    const badC5 = Rune{ .a = 0xe2, .b = 0xff, .c = 0, .d = 0xff };
    try expectEqual(false, badC5.isCodepointAnyRune());
    try expectEqual(false, badC5.isScalarValueAnyRune());
    const badD = Rune{ .a = 0xf0, .b = 0xff, .c = 0, .d = 0 };
    try expectEqual(false, badD.isCodepointAnyRune());
    try expectEqual(false, badD.isScalarValueAnyRune());
    const badD2 = Rune{ .a = 0xf2, .b = 0xff, .c = 0, .d = 0 };
    try expectEqual(false, badD2.isCodepointAnyRune());
    try expectEqual(false, badD2.isScalarValueAnyRune());
    const badTooHigh = Rune{ .a = 0xff, .b = 0xff, .c = 0, .d = 0 };
    try expectEqual(false, badTooHigh.isCodepointAnyRune());
    try expectEqual(false, badTooHigh.isScalarValueAnyRune());
    const badTooHigh2 = Rune{ .a = 0xf4, .b = 0x90, .c = 0xa0, .d = 0xa0 };
    try expectEqual(false, badTooHigh2.isCodepointAnyRune());
    try expectEqual(false, badTooHigh2.isScalarValueAnyRune());
    const badOverLong1 = Rune{ .a = 0xe0, .b = 0x80, .c = 0x90, .d = 0 };
    try expectEqual(false, badOverLong1.isCodepointAnyRune());
    try expectEqual(false, badOverLong1.isScalarValueAnyRune());
    const badOverLong2 = Rune{ .a = 0xf0, .b = 0x80, .c = 0x90, .d = 0x90 };
    try expectEqual(false, badOverLong2.isCodepointAnyRune());
    try expectEqual(false, badOverLong2.isScalarValueAnyRune());
}

test "Rune scalar tests" {
    const rSurrogate = try Rune.fromCodepoint(0xd850);
    try expect(rSurrogate.isCodepoint());
    try expect(rSurrogate.isCodepointAnyRune());
    try expectEqual(false, rSurrogate.isScalarValue());
    try expectEqual(false, rSurrogate.isScalarValueAnyRune());
    const rMalformed = Rune{ .a = 0xff, .b = 0, .c = 0, .d = 0 };
    try expectEqual(false, rMalformed.isScalarValue());
    try expectEqual(false, rMalformed.isScalarValueAnyRune());
}
