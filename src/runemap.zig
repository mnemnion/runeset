//! RuneMap: Maps from Runeset matches to some value.
//!

/// A `RuneMap` provides the capacity to map a match to a value of type `T`.  Performance
/// of this mapping is like that of `RuneSet` itself: efficient by design for many
/// real sets of interest, but degrading for very large sets, or those of intermediate
/// size which are sparse in the Unicode codepoint space.
///
/// The slice of `[]T` provided on initialization is asserted to have a length greater
/// than or equal to the number of codepoints (runes) in the set.  A default value can
/// be provided for when there is no match in the `RuneSet`.
///
/// Set operations on `RuneMap`s are also provided.  For difference and intersection, the
/// second argument is a `RuneSet`, not a `RuneMap`, as these always result in an improper
/// subset of the original set or map.  For a union, the second argument is a `RuneMap`,
/// to provide missing values.  In safe modes, the value mapped to overlapping values is
/// asserted to be identical during set union.  In other modes, it will be the value
/// in the receiver map.
pub fn RuneMap(T: type) type {
    return struct {
        set: RuneSet,
        vals: []T,
        default: ?T,

        const RMap = @This();

        /// Initialize a RuneMap from a string and a slice of the mapped values.
        /// You may provide a default value, to return when there is no match,
        /// or `null` to return null.  The value slice is assumed to be owned by
        /// the RuneMap for `.deinit`, when this is
        /// not the case, deinitialize the set separately: `rune_map.set.deinit(allocator)`,
        pub fn init(allocator: Allocator, str: []const u8, vals: []T, default: ?T) Allocator.Error!RMap {
            const set = try RuneSet.createFromConstString(str, allocator);
            return initWithRuneSet(set, vals, default);
        }

        /// Initialize a RuneMap with a RuneSet, and a slice of the mapped
        /// values.  You may provide a default value, to return when there is no
        /// match, or `null` to return null.  The value slice, and the set, are
        /// assumed to be owned by the RuneMap for `.deinit`.
        pub fn initWithRuneSet(set: RuneSet, vals: []T, default: ?T) RMap {
            assert(set.runeCount() <= vals.len);
            return .{ .set = set, .vals = vals, .default = default };
        }

        pub fn deinit(map: RMap, allocator: Allocator) void {
            map.set.deinit(allocator);
            allocator.free(map.vals);
        }

        /// Get the matching value for the codepoint encoded starting with slice[0].
        /// If initialized with a default value, this function will always return a T.
        pub fn get(map: *const RMap, slice: []const u8) ?T {
            const idx = map.set.matchOne(slice);
            if (idx) |i| {
                return map.vals[i];
            } else {
                return map.default;
            }
        }
    };
}

const std = @import("std");
const Allocator = std.mem.Allocator;
const assert = std.debug.assert;
const runeset = @import("runeset.zig");
const RuneSet = runeset.RuneSet;
