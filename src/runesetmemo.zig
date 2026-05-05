//! Memoized RuneSet matching.

const std = @import("std");
const Allocator = std.mem.Allocator;
const assert = std.debug.assert;

const elements = @import("elements.zig");
const Mask = elements.Mask;
const codeunit = elements.codeunit;
const toMask = Mask.toMask;
const runeset = @import("runeset.zig");
const RuneSet = runeset.RuneSet;

const LOW = 0;
const HI = 1;
const LEAD = 2;
const T4_OFF = 3;

const TWO_MAX = 32;
const THREE_MAX = 48;

const MASK_IN_TWO: u64 = codeunit(TWO_MAX).hiMask();
const MASK_OUT_FOUR: u64 = codeunit(THREE_MAX).hiMask();

/// A RuneSet which memorizes the offset calculations needed for matching
/// three- and four-byted codepoints.  This makes time-to-match strictly
/// independent of the size of the set: instead, it is O(n) where `n` is
/// the number of codeunits in the codepoint tested.  As close to constant
/// as any operation on UTF-8 ever gets.
pub const RuneSetMemo = struct {
    body: []const u64,
    offsets: []const u16,

    /// Create a RuneSetMemo from a RuneSet, copying the body data to a new slice.
    pub fn createFromRuneSet(set: anytype, allocator: Allocator) !RuneSetMemo {
        return createFromBody(set.body, allocator);
    }

    /// Create a RuneSetMemo from a mutable `[]u8`, destroying it in the process.
    pub fn createFromMutableString(str: []u8, allocator: Allocator) !RuneSetMemo {
        const set = try RuneSet.createFromMutableString(str, allocator);
        errdefer set.deinit(allocator);
        return createTakingBody(set.body, allocator);
    }

    /// Create a RuneSetMemo from a `[]const u8`.
    pub fn createFromConstString(str: []const u8, allocator: Allocator) !RuneSetMemo {
        const set = try RuneSet.createFromConstString(str, allocator);
        errdefer set.deinit(allocator);
        return createTakingBody(set.body, allocator);
    }

    /// Create a RuneSetMemo from a runeset.body, copying the data to a new slice.
    pub fn createFromBody(body: []const u64, allocator: Allocator) !RuneSetMemo {
        const owned_body = try allocator.alloc(u64, body.len);
        errdefer allocator.free(owned_body);
        @memcpy(owned_body, body);
        return createTakingBody(owned_body, allocator);
    }

    /// Take ownership of a runeset.body, enhancing it with memorized offset data.
    /// The original RuneSet must not be deinitialized after this operation.
    pub fn createTakingBody(body: []const u64, allocator: Allocator) !RuneSetMemo {
        const owned_offsets = try buildOffsets(body, allocator);
        return .{
            .body = body,
            .offsets = owned_offsets,
        };
    }

    pub fn deinit(memo: RuneSetMemo, allocator: Allocator) void {
        allocator.free(memo.body);
        allocator.free(memo.offsets);
    }

    /// Borrow the body data of the RuneSetMemo as an ordinary RuneSet.
    pub fn asRuneSet(memo: RuneSetMemo) RuneSet {
        return .{ .body = memo.body };
    }

    /// Serialize a RuneSetMemo as a constant Zig variable named `name`.  The `public`
    /// enum makes it uhh, `pub`, unless it's `.private`.  Of course.
    pub fn serialize(memo: RuneSetMemo, writer: anytype, public: RuneSet.Privacy, name: []const u8) !void {
        if (public == .public) {
            try writer.writeAll("pub ");
        }
        try writer.print("const {s}: RuneSetMemo = ", .{name});
        try memo.serializeBody(writer);
        try writer.writeAll(";\n");
    }

    /// Serialize just the body of a RuneSetMemo, in anonymous struct format.
    pub fn serializeBody(memo: RuneSetMemo, writer: anytype) !void {
        try writer.print(".{{ .body = &.{{ 0x{x}", .{memo.body[0]});
        for (memo.body[1..]) |word| {
            try writer.print(", 0x{x}", .{word});
        }
        try writer.print(" }}, .offsets = &.{{ {d}", .{memo.offsets[0]});
        for (memo.offsets[1..]) |offset| {
            try writer.print(", {d}", .{offset});
        }
        try writer.writeAll(" } }");
    }

    pub fn setUnion(L: RuneSetMemo, R: anytype, allocator: Allocator) error{OutOfMemory}!RuneSetMemo {
        var result = try L.asRuneSet().setUnion(normalizeRuneSet(R), allocator);
        errdefer result.deinit(allocator);
        return createTakingBody(result.body, allocator);
    }

    pub fn setDifference(L: RuneSetMemo, R: anytype, allocator: Allocator) error{OutOfMemory}!RuneSetMemo {
        var result = try L.asRuneSet().setDifference(normalizeRuneSet(R), allocator);
        errdefer result.deinit(allocator);
        return createTakingBody(result.body, allocator);
    }

    pub fn setIntersection(L: RuneSetMemo, R: anytype, allocator: Allocator) error{OutOfMemory}!RuneSetMemo {
        var result = try L.asRuneSet().setIntersection(normalizeRuneSet(R), allocator);
        errdefer result.deinit(allocator);
        return createTakingBody(result.body, allocator);
    }

    pub fn setDisjunction(L: RuneSetMemo, R: anytype, allocator: Allocator) error{OutOfMemory}!RuneSetMemo {
        var result = try L.asRuneSet().setDisjunction(normalizeRuneSet(R), allocator);
        errdefer result.deinit(allocator);
        return createTakingBody(result.body, allocator);
    }

    pub fn matchOne(memo: RuneSetMemo, slice: []const u8) ?usize {
        return matchOneDirectly(memo.body, memo.offsets, slice);
    }

    pub fn matchOneAllowInvalid(memo: RuneSetMemo, slice: []const u8) usize {
        return memo.matchOne(slice) orelse 0;
    }

    pub fn matchOneAssumeValid(memo: RuneSetMemo, slice: []const u8) usize {
        return matchOneDirectAssumeValid(memo.body, memo.offsets, slice);
    }

    pub fn matchMany(memo: RuneSetMemo, slice: []const u8) ?usize {
        var idx: usize = 0;
        while (idx < slice.len) {
            const n_bytes = memo.matchOne(slice[idx..]);
            if (n_bytes) |n| {
                if (n == 0) break;
                idx += n;
            } else return null;
        }
        return idx;
    }

    pub fn matchManyAllowInvalid(memo: RuneSetMemo, slice: []const u8) usize {
        var idx: usize = 0;
        while (idx < slice.len) {
            const n_bytes = memo.matchOne(slice[idx..]) orelse 0;
            if (n_bytes == 0)
                return idx;
            idx += n_bytes;
        }
        return idx;
    }

    pub fn matchManyAssumeValid(memo: RuneSetMemo, slice: []const u8) usize {
        var idx: usize = 0;
        while (idx < slice.len) {
            const n_bytes = memo.matchOneAssumeValid(slice[idx..]);
            if (n_bytes == 0) break;
            idx += n_bytes;
        }
        return idx;
    }
};

fn normalizeRuneSet(set: anytype) RuneSet {
    const Set = @TypeOf(set);
    if (Set == RuneSet) {
        return set;
    } else if (Set == RuneSetMemo) {
        return set.asRuneSet();
    } else {
        @compileError("expected RuneSet or RuneSetMemo");
    }
}

fn buildOffsets(body: []const u64, allocator: Allocator) ![]u16 {
    const t3_start = t3start(body);
    const t3_memo_end = t3_3c_start(body);
    const t2_memo_start = t2_3b_start(body);
    const t2_memo_len = t3_start - t2_memo_start;
    const t3_memo_len = t3_memo_end - t3_start;
    const offsets = try allocator.alloc(u16, 1 + t2_memo_len + t3_memo_len);
    errdefer allocator.free(offsets);

    offsets[0] = @intCast(t2_memo_start - 1);
    var idx: usize = 1;
    for (t2_memo_start..t3_start) |t2off| {
        offsets[idx] = @intCast(popCountSlice(body[t2off + 1 .. t3_start]));
        idx += 1;
    }

    for (t3_start..t3_memo_end) |t3off| {
        offsets[idx] = @intCast(popCountSlice(body[t3_start..t3off]));
        idx += 1;
    }

    return offsets;
}

inline fn t3start(body: []const u64) usize {
    return 4 + @popCount(body[LEAD]);
}

inline fn t2_3b_start(body: []const u64) usize {
    return 4 + @popCount(body[LEAD] & MASK_IN_TWO);
}

inline fn t2_4b_start(body: []const u64) usize {
    return 4 + @popCount(body[LEAD] & MASK_OUT_FOUR);
}

inline fn t3_3c_start(body: []const u64) usize {
    return t3start(body) + popCountSlice(body[t2_4b_start(body)..t3start(body)]);
}

inline fn t4offset(body: []const u64) usize {
    return @intCast(body[T4_OFF]);
}

inline fn t2Memo(offsets: []const u16, t2off: usize) u16 {
    return offsets[t2off - offsets[0]];
}

inline fn t3Memo(offsets: []const u16, t3off: usize) u16 {
    return offsets[t3off - offsets[0]];
}

fn matchOneDirectly(set: []const u64, offsets: []const u16, str: []const u8) ?usize {
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
            const c = codeunit(str[2]);
            if (c.kind != .follow) return null;
            const t3_start = t3start(set);
            const c_off = b_mask.higherThan(b).? + t2Memo(offsets, b_loc);
            const c_loc = t3_start + c_off;
            const c_mask = toMask(set[c_loc]);
            if (!c_mask.isIn(c)) return 0;
            if (nB == 3) return 3;
            const d_off = c_mask.lowerThan(c).? + t3Memo(offsets, c_loc);
            const d_loc = t4offset(set) + d_off;
            const d = codeunit(str[3]);
            if (d.kind != .follow) return null;
            const d_mask = toMask(set[d_loc]);
            if (d_mask.isIn(d)) return 4 else return 0;
        },
    }
}

fn matchOneDirectAssumeValid(set: []const u64, offsets: []const u16, str: []const u8) usize {
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
            const c = codeunit(str[2]);
            assert(c.kind == .follow);
            const t3_start = t3start(set);
            const c_off = b_mask.higherThan(b).? + t2Memo(offsets, b_loc);
            const c_loc = t3_start + c_off;
            const c_mask = toMask(set[c_loc]);
            if (!c_mask.isIn(c)) return 0;
            if (nB == 3) return 3;
            const d_off = c_mask.lowerThan(c).? + t3Memo(offsets, c_loc);
            const d_loc = t4offset(set) + d_off;
            const d = codeunit(str[3]);
            assert(d.kind == .follow);
            const d_mask = toMask(set[d_loc]);
            if (d_mask.isIn(d)) return 4 else return 0;
        },
    }
}

fn popCountSlice(region: []const u64) usize {
    var count: usize = 0;
    for (region) |word| {
        count += @popCount(word);
    }
    return count;
}
