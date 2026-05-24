//! RuneMap: map RuneSetMemo matches to caller-provided values.

const LOW = 0;
const HI = 1;
const LEAD = 2;
const T4_OFF = 3;

const TWO_MAX = 32;
const THREE_MAX = 48;

const MASK_IN_TWO: u64 = codeunit(TWO_MAX).hiMask();
const MASK_OUT_FOUR: u64 = codeunit(THREE_MAX).hiMask();

/// Degree of index optimization to perform
pub const OptKind = enum {
    /// The index cache will have 'holes' for non-final bytes
    none,
    /// The index cache is dense, no attempt is made to make it smaller
    dense,
    /// Reasonable effort will be made to compress the final values array,
    /// and the index cache will be dense
    high,
};

/// A `RuneMap` provides the capacity to map a matched rune to a value of
/// type `T`. The map owns its memoized set, its dense value slice, and its
/// final-mask offset cache.
pub fn RuneMap(T: type, opt: OptKind) type {
    if (opt == .high) {
        @compileError("RuneMap .high optimization is not implemented yet");
    }

    return struct {
        set: RuneSetMemo,
        vals: []T,
        offsets: []const u32,
        default: ?T,

        const RMap = @This();

        /// Initialize a RuneMap from a string and a dense slice of mapped
        /// values. Values are expected to be in RuneSet iteration order,
        /// which is the same as generalized UTF-8 lexicographical order.
        pub fn init(allocator: Allocator, str: []const u8, vals: []T, default: ?T) !RMap {
            const set = try RuneSetMemo.createFromConstString(str, allocator);
            errdefer set.deinit(allocator);
            return initWithRuneSetMemo(allocator, set, vals, default);
        }

        /// Initialize a RuneMap with an owned RuneSetMemo and owned values.
        pub fn initWithRuneSetMemo(allocator: Allocator, set: RuneSetMemo, vals: []T, default: ?T) OOM!RMap {
            assert(set.asRuneSet().runeCount() <= vals.len);
            const offsets = try buildOffsets(set, opt, allocator);
            return .{
                .set = set,
                .vals = vals,
                .offsets = offsets,
                .default = default,
            };
        }

        pub fn deinit(map: RMap, allocator: Allocator) void {
            map.set.deinit(allocator);
            allocator.free(map.vals);
            allocator.free(map.offsets);
        }

        /// Get the value mapped to the codepoint encoded at `slice[0..]`.
        /// Invalid input and non-members return `default`.
        pub fn get(map: *const RMap, slice: []const u8) ?T {
            const idx = map.indexOf(slice) orelse return map.default;
            return map.vals[idx];
        }

        pub fn indexOf(map: *const RMap, slice: []const u8) ?usize {
            if (slice.len == 0) return null;

            const body = map.set.body;
            const memo_offsets = map.set.offsets;
            const a = codeunit(slice[0]);

            switch (a.kind) {
                .follow => return null,
                .low => {
                    const mask = toMask(body[LOW]);
                    return uintOrNull(mask.lowerThan(a));
                },
                .hi => {
                    const mask = toMask(body[HI]);
                    const base = @popCount(body[LOW]);
                    return base + (uintOrNull(mask.lowerThan(a)) orelse return null);
                },
                .lead => {
                    const n_bytes = a.nMultiBytes() orelse return null;
                    if (n_bytes > slice.len) return null;

                    const a_mask = toMask(body[LEAD]);
                    if (!a_mask.isIn(a)) return null;

                    const b = codeunit(slice[1]);
                    if (b.kind != .follow) return null;
                    const b_loc = 4 + a_mask.lowerThan(a).?;
                    const b_mask = toMask(body[b_loc]);
                    if (!b_mask.isIn(b)) return null;
                    if (n_bytes == 2) {
                        return map.finalIndex(b_loc, b_mask.lowerThan(b).?);
                    }

                    const c = codeunit(slice[2]);
                    if (c.kind != .follow) return null;
                    const t3_start = t3start(body);
                    const c_off = b_mask.higherThan(b).? + t2Memo(memo_offsets, b_loc);
                    const c_loc = t3_start + c_off;
                    const c_mask = toMask(body[c_loc]);
                    if (!c_mask.isIn(c)) return null;
                    if (n_bytes == 3) {
                        return map.finalIndex(c_loc, c_mask.lowerThan(c).?);
                    }

                    const d = codeunit(slice[3]);
                    if (d.kind != .follow) return null;
                    const d_off = c_mask.lowerThan(c).? + t3Memo(memo_offsets, c_loc);
                    const d_loc = t4offset(body) + d_off;
                    const d_mask = toMask(body[d_loc]);
                    if (!d_mask.isIn(d)) return null;
                    return map.finalIndex(d_loc, d_mask.lowerThan(d).?);
                },
            }
        }

        inline fn finalIndex(map: *const RMap, final_offset: usize, in_mask: u7) usize {
            const base: usize = map.offsets[map.offsetIndex(final_offset)];
            return base + in_mask;
        }

        inline fn offsetIndex(map: *const RMap, final_offset: usize) usize {
            return switch (opt) {
                .none => final_offset - 4,
                .dense => map.denseOffsetIndex(final_offset),
                .high => unreachable,
            };
        }

        inline fn denseOffsetIndex(map: *const RMap, final_offset: usize) usize {
            const body = map.set.body;
            if (final_offset < t2_3b_start(body)) {
                return 2 + final_offset - 4;
            }
            if (final_offset < t3end(body)) {
                return final_offset - @as(usize, map.offsets[0]);
            }
            return final_offset - @as(usize, map.offsets[1]);
        }
    };
}

fn buildOffsets(set: RuneSetMemo, comptime opt: OptKind, allocator: Allocator) OOM![]const u32 {
    return switch (opt) {
        .none => buildSparseOffsets(set, allocator),
        .dense => buildDenseOffsets(set, allocator),
        .high => unreachable,
    };
}

fn buildSparseOffsets(set: RuneSetMemo, allocator: Allocator) OOM![]const u32 {
    const body = set.body;
    const offsets = try allocator.alloc(u32, body.len - 4);
    errdefer allocator.free(offsets);
    @memset(offsets, 0);

    const a_count: usize = @popCount(body[LOW]) + @popCount(body[HI]);
    const b_count = popCountSlice(body[4..t2_3b_start(body)]);
    const c_count = popCountSlice(body[t3_3c_start(body)..t3end(body)]);

    var base: usize = a_count;
    for (4..t2_3b_start(body)) |off| {
        offsets[off - 4] = @intCast(base);
        base += @popCount(body[off]);
    }

    base = a_count + b_count;
    var t3c_off = t3end(body);
    while (t3c_off > t3_3c_start(body)) {
        t3c_off -= 1;
        offsets[t3c_off - 4] = @intCast(base);
        base += @popCount(body[t3c_off]);
    }

    if (t4offset(body) != 0) {
        base = a_count + b_count + c_count;
        const t3_start = t3start(body);
        var t3d_off = t3_3c_start(body);
        while (t3d_off > t3_start) {
            t3d_off -= 1;
            const t4_start = t4offset(body) + popCountSlice(body[t3_start..t3d_off]);
            const t4_end = t4_start + @popCount(body[t3d_off]);
            for (t4_start..t4_end) |off| {
                offsets[off - 4] = @intCast(base);
                base += @popCount(body[off]);
            }
        }
    }

    return offsets;
}

fn buildDenseOffsets(set: RuneSetMemo, allocator: Allocator) OOM![]const u32 {
    const body = set.body;
    const t2_start = 4;
    const t2_final_count = t2_3b_start(body) - t2_start;
    const t3_start = t3_3c_start(body);
    const t3_final_count = t3end(body) - t3_start;
    const t4_start = t4offset(body);
    const t4_final_count = if (t4_start == 0) 0 else body.len - t4_start;
    const t3_cache_start = 2 + t2_final_count;
    const t4_cache_start = t3_cache_start + t3_final_count;

    const offsets = try allocator.alloc(u32, t4_cache_start + t4_final_count);
    errdefer allocator.free(offsets);

    assert(t3_start >= t3_cache_start);
    offsets[0] = @intCast(t3_start - t3_cache_start);
    offsets[1] = if (t4_start == 0) 0 else blk: {
        assert(t4_start >= t4_cache_start);
        break :blk @intCast(t4_start - t4_cache_start);
    };

    const a_count: usize = @popCount(body[LOW]) + @popCount(body[HI]);
    const b_count = popCountSlice(body[4..t2_3b_start(body)]);
    const c_count = popCountSlice(body[t3_3c_start(body)..t3end(body)]);

    var base: usize = a_count;
    for (4..t2_3b_start(body)) |off| {
        offsets[2 + off - 4] = @intCast(base);
        base += @popCount(body[off]);
    }

    base = a_count + b_count;
    var t3c_off = t3end(body);
    while (t3c_off > t3_3c_start(body)) {
        t3c_off -= 1;
        offsets[t3c_off - @as(usize, offsets[0])] = @intCast(base);
        base += @popCount(body[t3c_off]);
    }

    if (t4_start != 0) {
        base = a_count + b_count + c_count;
        const t3d_start = t3start(body);
        var t3d_off = t3_3c_start(body);
        while (t3d_off > t3d_start) {
            t3d_off -= 1;
            const final_start = t4_start + popCountSlice(body[t3d_start..t3d_off]);
            const final_end = final_start + @popCount(body[t3d_off]);
            for (final_start..final_end) |off| {
                offsets[off - @as(usize, offsets[1])] = @intCast(base);
                base += @popCount(body[off]);
            }
        }
    }

    return offsets;
}

inline fn t2_3b_start(body: []const u64) usize {
    return 4 + @popCount(body[LEAD] & MASK_IN_TWO);
}

inline fn t2_4b_start(body: []const u64) usize {
    return 4 + @popCount(body[LEAD] & MASK_OUT_FOUR);
}

inline fn t3start(body: []const u64) usize {
    return 4 + @popCount(body[LEAD]);
}

inline fn t3_3c_start(body: []const u64) usize {
    return t3start(body) + popCountSlice(body[t2_4b_start(body)..t3start(body)]);
}

inline fn t3end(body: []const u64) usize {
    return if (body[T4_OFF] == 0) body.len else @intCast(body[T4_OFF]);
}

inline fn t4offset(body: []const u64) usize {
    return @intCast(body[T4_OFF]);
}

inline fn t2Memo(offsets: []const u16, t2off: usize) usize {
    return offsets[t2off - offsets[0]];
}

inline fn t3Memo(offsets: []const u16, t3off: usize) usize {
    return offsets[t3off - offsets[0]];
}

fn popCountSlice(region: []const u64) usize {
    var count: usize = 0;
    for (region) |word| {
        count += @popCount(word);
    }
    return count;
}

inline fn uintOrNull(n: ?u7) ?usize {
    return if (n) |u| u else null;
}

const std = @import("std");
const Allocator = std.mem.Allocator;
const OOM = Allocator.Error;
const assert = std.debug.assert;

const elements = @import("elements.zig");
const Mask = elements.Mask;
const codeunit = elements.codeunit;
const toMask = Mask.toMask;
const RuneSetMemo = @import("runesetmemo.zig").RuneSetMemo;
