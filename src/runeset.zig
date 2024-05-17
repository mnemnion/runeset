//! libruneset: fast utf8 codepoint sets for Zig
//!
//! We begin with the canonical charsets, which can represent arbitrary
//! classes of characters and support the big three set operations.
//!
//! This will be enhanced with ranged charsets, which can take a small
//! number of ranges (three seems right) for a more compact memory
//! layout, and similar speed for testing membership.  These will operate
//! against each other in union, intersection, and difference, as well as
//! against canonical sets.  This will be represented as a tagged union,
//! which also includes inverted (anything not in set) variants on both.
//!
//! All set operations will be nondestructive, returning a new set.  Some
//! cases of binary operations on two ranged sets will return a canonical
//! set, any operation involving a canonical set will return a canonical
//! set.  All ASCII sets will be canonical, since there's no advantage in
//! using a ranged representation there.
//!
//! The body of a canonical set is a `[]const u64`, with an internal structure
//! defined by the algorithms which use them.

const std = @import("std");
const ArrayList = std.ArrayList;
// "pub" might not be appropriate here, these types
// will most likely be fully encapsulated by the library.
pub usingnamespace @import("elements.zig");

const testing = std.testing;

const RuneSet = []const u64;

/// Creates the body of a RuneSet from a mutable string, allocating
/// a (still mutable) []u64 from the allocator and returning it.
///
/// This operation turns the string into garbage, the caller is
/// responsible for freeing that memory, and owns the memory returned.
///
/// The string must have only valid utf-8, or this function will return
/// an error.
pub fn createBodyFromString(str: []u8, alloc: std.Allocator) ![]u64 {
    // The header handles all ASCII and lead bytes.  The fourth word
    // defines the offset into T4, which is zero unless the set contains
    // four-byte characters.
    var header: [4]u64 = 0 ** 4;
    // var T2: [56]u64 = 0 ** 56; // Masks for all second bytes.
    var hasT2 = false; // tells us if we used T2.
    // We 'sieve' the string, taking characters in order of length,
    // and remove them from the string by moving all other characters
    // back.
    var back: usize = 0; // amount to move unused characters back.
    var sieve = str;
    // ASCII pass: we process all ASCII and move anything else back.
    var idx: usize = 0;
    var low = Mask.toMask(0);
    var hi = Mask.toMask(0);
    while (idx < sieve.len) {
        const cu = split(sieve[idx]);
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
                hasT2 = true;
                nBytes = cu.nBytes();
                if (nBytes) |nB| {
                    std.debug.assert(nb <= 4);
                    if (idx + nB > sieve.len) {
                        return error.InvalidUnicode;
                    }
                    if (nB >= 2) {
                        sieve[idx - back] = sieve[i];
                        sieve[idx - back + 1] = sieve[i + 1];
                    }
                    if (nB >= 3) {
                        sieve[idx - back + 2] = sieve[i + 2];
                    }
                    if (nb == 4) {
                        sieve[idx - back + 3] = sieve[i + 3];
                    }
                } else {
                    return error.InvalidUnicode;
                }
                idx += nB + 1;
            },
            .follow => return error.InvalidUnicode,
        }
    }
    // TODO test hasT2, if ascii-only, construct header and return it
    sieve = sieve[0 .. sieve.len - back];
}

// Run elements tests as well
test {
    testing.refAllDecls(@This());
}
