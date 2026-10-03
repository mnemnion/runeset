//! Contiguous value stripes with an anchored ASCII prefix.

/// A stripe's original location, index-cache slot, and packed location.
pub const Stripe = struct {
    start: u32,
    len: u32,
    slot: u32 = 0,
    offset: u32 = 0,
};

/// Allocate packed values, searching from the last placement and wrapping.
/// Accept the first complete match or tail prefix; wrap only if neither fits.
/// Stripe zero is the fixed ASCII prefix. Input values remain caller-owned.
pub fn firstFit(comptime T: type, allocator: Allocator, vals: []const T, stripes: []Stripe) OOM![]T {
    var output: std.ArrayList(T) = .empty;
    defer output.deinit(allocator);
    try output.ensureTotalCapacity(allocator, vals.len);
    output.appendSliceAssumeCapacity(vals[0..stripes[0].len]);
    stripes[0].offset = 0;
    var cursor: u32 = 0;
    for (stripes[1..]) |*stripe| {
        const pattern = vals[stripe.start..][0..stripe.len];
        if (pattern.len == 0) {
            stripe.offset = cursor;
            continue;
        }
        assert(pattern.len <= 64);
        var borders: [64]u32 = undefined;
        borders[0] = 0;
        var matched: u32 = 0;
        for (pattern[1..], 1..) |value, i| {
            while (matched != 0 and !valueEql(value, pattern[matched])) matched = borders[matched - 1];
            if (valueEql(value, pattern[matched])) matched += 1;
            borders[i] = matched;
        }
        const output_len: u32 = @intCast(output.items.len);
        const stripe_borders = borders[0..pattern.len];
        var fit = findFirstFit(T, output.items, pattern, stripe_borders, cursor, output_len);
        if (fit == null and cursor != 0) {
            const original_cursor = cursor;
            cursor = 0;
            fit = findFirstFit(T, output.items, pattern, stripe_borders, cursor, original_cursor);
        }

        if (fit) |placement| {
            cursor = placement.start;
            output.appendSliceAssumeCapacity(pattern[placement.matched..]);
        } else {
            cursor = output_len;
            output.appendSliceAssumeCapacity(pattern);
        }
        stripe.offset = cursor;
    }
    return output.toOwnedSlice(allocator);
}

/// Allocate a greedy superstring using an Aho-Corasick trie and chain endpoints.
/// Stripe zero stays anchored; input values remain caller-owned.
pub fn goodFit(comptime T: type, allocator: Allocator, vals: []const T, stripes: []Stripe) OOM![]T {
    var trie: Trie(T) = .{ .vals = vals };
    defer trie.deinit(allocator);
    try trie.nodes.append(allocator, .{});
    const work = try allocator.alloc(Work, stripes.len);
    defer allocator.free(work);
    for (stripes, work, 0..) |stripe, *item, id| {
        item.* = .{ .head = id, .tail = id };
        var node: usize = 0;
        trie.witness(node, id, 0, stripes);
        for (stripe.start..stripe.start + stripe.len, 1..) |source, end| {
            node = try trie.insert(allocator, node, source);
            trie.witness(node, id, end, stripes);
        }
        item.node = node;
        if (trie.nodes.items[node].terminal == NONE) trie.nodes.items[node].terminal = id;
    }
    try trie.buildFailures(allocator, stripes);
    for (work, 0..) |*item, id| {
        const node = trie.nodes.items[item.node];
        item.container = if (id == 0) 0 else node.witness;
        item.relative = if (id == 0) 0 else node.end - stripes[id].len;
    }

    var ranks: std.ArrayList(usize) = .empty;
    defer ranks.deinit(allocator);
    try trie.rank(allocator, 0, work, &ranks);
    const available = try allocator.alloc(usize, ranks.items.len + 1);
    defer allocator.free(available);
    for (available, 0..) |*next, i| next.* = i;
    available[work[0].rank] = nextAvailable(available, work[0].rank + 1);

    // Failure links enumerate suffix-prefix overlaps in descending length.
    // Trie ranges and a deletion-only successor index avoid enumerating pairs.
    var buckets: [129]usize = @splat(NONE);
    for (work, 0..) |*item, id| {
        if (item.container != id) continue;
        const depth = trie.nodes.items[item.node].depth;
        item.pending = buckets[depth];
        buckets[depth] = id;
    }
    var total: usize = 0;
    for (work, stripes, 0..) |item, stripe, id| {
        if (item.container == id) total += stripe.len;
    }
    var depth: usize = buckets.len;
    while (depth != 0) {
        depth -= 1;
        while (buckets[depth] != NONE) {
            const id = buckets[depth];
            const item = &work[id];
            buckets[depth] = item.pending;
            const node = trie.nodes.items[item.node];
            var candidate = nextAvailable(available, node.lo);
            if (candidate < node.hi and ranks.items[candidate] == item.head) {
                candidate = nextAvailable(available, candidate + 1);
            }
            if (candidate < node.hi) {
                const right = ranks.items[candidate];
                item.successor = right;
                item.overlap = depth;
                work[work[right].tail].head = item.head;
                work[item.head].tail = work[right].tail;
                available[candidate] = nextAvailable(available, candidate + 1);
                total -= depth;
            } else if (item.node != 0) {
                item.node = node.fail;
                const next_depth = trie.nodes.items[node.fail].depth;
                item.pending = buckets[next_depth];
                buckets[next_depth] = id;
            }
        }
    }

    const output = try allocator.alloc(T, total);
    var id: usize = 0;
    var used: usize = 0;
    var overlap: usize = 0;
    while (id != NONE) {
        const stripe = stripes[id];
        work[id].base = used - overlap;
        const rest = vals[stripe.start + overlap .. stripe.start + stripe.len];
        @memcpy(output[used..][0..rest.len], rest);
        used += rest.len;
        overlap = work[id].overlap;
        id = work[id].successor;
    }
    assert(used == total);
    for (stripes, 0..) |*stripe, i| stripe.offset = @intCast(resolveBase(work, i));
    return output;
}

const NONE = std.math.maxInt(usize);

const Fit = struct {
    start: u32,
    matched: u32,
};

fn findFirstFit(comptime T: type, output: []const T, pattern: []const T, borders: []const u32, start: u32, limit: u32) ?Fit {
    if (start == limit) return null;
    // Candidate starts are below limit, but their matches can extend past it.
    const scan_limit = @min(output.len, @as(usize, limit) + pattern.len - 1);
    var scan: u32 = start;
    var matched: u32 = 0;
    while (scan < scan_limit) {
        while (matched != 0 and !valueEql(output[scan], pattern[matched])) matched = borders[matched - 1];
        if (valueEql(output[scan], pattern[matched])) matched += 1;
        scan += 1;
        if (matched == pattern.len) return .{ .start = scan - matched, .matched = matched };
    }
    if (scan == output.len and matched != 0 and scan - matched < limit) {
        return .{ .start = scan - matched, .matched = matched };
    }
    return null;
}

const Work = struct {
    node: usize = 0,
    container: usize = NONE,
    relative: usize = 0,
    rank: usize = NONE,
    head: usize,
    tail: usize,
    successor: usize = NONE,
    overlap: usize = 0,
    pending: usize = NONE,
    base: ?usize = null,
};

fn nextAvailable(available: []usize, index: usize) usize {
    var root = index;
    while (available[root] != root) root = available[root];
    var at = index;
    while (available[at] != at) {
        const next = available[at];
        available[at] = root;
        at = next;
    }
    return root;
}

fn resolveBase(work: []Work, id: usize) usize {
    if (work[id].base) |base| return base;
    const base = resolveBase(work, work[id].container) + work[id].relative;
    work[id].base = base;
    return base;
}

fn Trie(comptime T: type) type {
    return struct {
        vals: []const T,
        nodes: std.ArrayList(Node) = .empty,
        edges: std.HashMapUnmanaged(Key, usize, Context, std.hash_map.default_max_load_percentage) = .empty,

        const Tree = @This();
        const Key = struct { parent: usize, source: usize };
        const Context = struct {
            vals: []const T,

            pub fn hash(context: Context, key: Key) u64 {
                var hasher = std.hash.Wyhash.init(0);
                std.hash.autoHash(&hasher, key.parent);
                valueHash(&hasher, context.vals[key.source]);
                return hasher.final();
            }

            pub fn eql(context: Context, a: Key, b: Key) bool {
                return a.parent == b.parent and valueEql(context.vals[a.source], context.vals[b.source]);
            }
        };
        const Node = struct {
            child: usize = NONE,
            sibling: usize = NONE,
            source: usize = 0,
            fail: usize = 0,
            depth: usize = 0,
            terminal: usize = NONE,
            witness: usize = NONE,
            end: usize = 0,
            lo: usize = 0,
            hi: usize = 0,
        };

        fn deinit(tree: *Tree, allocator: Allocator) void {
            tree.nodes.deinit(allocator);
            tree.edges.deinit(allocator);
        }

        fn transition(tree: *const Tree, parent: usize, source: usize) ?usize {
            return tree.edges.getContext(.{ .parent = parent, .source = source }, .{ .vals = tree.vals });
        }

        fn insert(tree: *Tree, allocator: Allocator, parent: usize, source: usize) OOM!usize {
            const entry = try tree.edges.getOrPutContext(allocator, .{ .parent = parent, .source = source }, .{ .vals = tree.vals });
            if (entry.found_existing) return entry.value_ptr.*;
            const id = tree.nodes.items.len;
            entry.value_ptr.* = id;
            try tree.nodes.append(allocator, .{
                .source = source,
                .sibling = tree.nodes.items[parent].child,
                .depth = tree.nodes.items[parent].depth + 1,
            });
            tree.nodes.items[parent].child = id;
            return id;
        }

        fn witness(tree: *Tree, id: usize, stripe: usize, end: usize, stripes: []const Stripe) void {
            const node = &tree.nodes.items[id];
            const old = node.witness;
            if (old == NONE or (stripe == 0 and old != 0) or
                (old != 0 and (stripes[stripe].len > stripes[old].len or
                    (stripes[stripe].len == stripes[old].len and stripe < old))))
            {
                node.witness = stripe;
                node.end = end;
            }
        }

        fn buildFailures(tree: *Tree, allocator: Allocator, stripes: []const Stripe) OOM!void {
            var queue: std.ArrayList(usize) = .empty;
            defer queue.deinit(allocator);
            try queue.append(allocator, 0);
            var front: usize = 0;
            while (front < queue.items.len) : (front += 1) {
                const parent = queue.items[front];
                var child = tree.nodes.items[parent].child;
                while (child != NONE) : (child = tree.nodes.items[child].sibling) {
                    const source = tree.nodes.items[child].source;
                    if (parent != 0) {
                        var fail = tree.nodes.items[parent].fail;
                        while (tree.transition(fail, source) == null and fail != 0) fail = tree.nodes.items[fail].fail;
                        tree.nodes.items[child].fail = tree.transition(fail, source) orelse 0;
                    }
                    try queue.append(allocator, child);
                }
            }
            var back = queue.items.len;
            while (back > 1) {
                back -= 1;
                const node = tree.nodes.items[queue.items[back]];
                tree.witness(node.fail, node.witness, node.end, stripes);
            }
        }

        fn rank(tree: *Tree, allocator: Allocator, id: usize, work: []Work, ranks: *std.ArrayList(usize)) OOM!void {
            tree.nodes.items[id].lo = ranks.items.len;
            const terminal = tree.nodes.items[id].terminal;
            if (terminal != NONE and work[terminal].container == terminal) {
                work[terminal].rank = ranks.items.len;
                try ranks.append(allocator, terminal);
            }
            var child = tree.nodes.items[id].child;
            while (child != NONE) : (child = tree.nodes.items[child].sibling) {
                try tree.rank(allocator, child, work, ranks);
            }
            tree.nodes.items[id].hi = ranks.items.len;
        }
    };
}

fn valueEql(a: anytype, b: @TypeOf(a)) bool {
    const T = @TypeOf(a);
    return switch (@typeInfo(T)) {
        .float => @as(@Int(.unsigned, @bitSizeOf(T)), @bitCast(a)) == @as(@Int(.unsigned, @bitSizeOf(T)), @bitCast(b)),
        .@"struct" => |info| blk: {
            inline for (info.fields) |field| {
                if (!valueEql(@field(a, field.name), @field(b, field.name))) break :blk false;
            }
            break :blk true;
        },
        .vector => |info| valueEql(@as([info.len]info.child, a), @as([info.len]info.child, b)),
        .array => |info| blk: {
            for (0..info.len) |i| {
                if (!valueEql(a[i], b[i])) break :blk false;
            }
            break :blk true;
        },
        .optional => if (a) |payload| (if (b) |other| valueEql(payload, other) else false) else b == null,
        .error_union => if (a) |payload| (if (b) |other| valueEql(payload, other) else |_| false) else |err| (if (b) |_| false else |other| err == other),
        .@"union" => |info| blk: {
            if (info.tag_type == null) @compileError("value compression requires tagged unions");
            if (std.meta.activeTag(a) != std.meta.activeTag(b)) break :blk false;
            break :blk switch (a) {
                inline else => |payload, tag| valueEql(payload, @field(b, @tagName(tag))),
            };
        },
        else => std.meta.eql(a, b),
    };
}

fn valueHash(hasher: *std.hash.Wyhash, value: anytype) void {
    const T = @TypeOf(value);
    switch (@typeInfo(T)) {
        .float => std.hash.autoHash(hasher, @as(@Int(.unsigned, @bitSizeOf(T)), @bitCast(value))),
        .@"struct" => |info| inline for (info.fields) |field| {
            valueHash(hasher, @field(value, field.name));
        },
        .vector => |info| valueHash(hasher, @as([info.len]info.child, value)),
        .array => |info| for (0..info.len) |i| {
            valueHash(hasher, value[i]);
        },
        .optional => {
            std.hash.autoHash(hasher, value != null);
            if (value) |payload| valueHash(hasher, payload);
        },
        .error_union => if (value) |payload| {
            std.hash.autoHash(hasher, false);
            valueHash(hasher, payload);
        } else |err| {
            std.hash.autoHash(hasher, true);
            valueHash(hasher, err);
        },
        .@"union" => |info| {
            if (info.tag_type == null) @compileError("value compression requires tagged unions");
            std.hash.autoHash(hasher, std.meta.activeTag(value));
            switch (value) {
                inline else => |payload| valueHash(hasher, payload),
            }
        },
        else => std.hash.autoHashStrat(hasher, value, .Shallow),
    }
}

//| Tests

const testing = std.testing;
const expect = testing.expect;
const expectEqual = testing.expectEqual;

fn checkPacking(comptime good: bool, parts: []const []const u8, expected_len: usize) !void {
    var vals: std.ArrayList(u8) = .empty;
    defer vals.deinit(testing.allocator);
    const stripes = try testing.allocator.alloc(Stripe, parts.len);
    defer testing.allocator.free(stripes);
    for (parts, stripes) |part, *stripe| {
        stripe.* = .{ .start = @intCast(vals.items.len), .len = @intCast(part.len) };
        try vals.appendSlice(testing.allocator, part);
    }
    const output = if (good)
        try goodFit(u8, testing.allocator, vals.items, stripes)
    else
        try firstFit(u8, testing.allocator, vals.items, stripes);
    defer testing.allocator.free(output);
    try expectEqual(expected_len, output.len);
    try expectEqual(@as(usize, 0), stripes[0].offset);
    for (stripes, parts) |stripe, part| {
        try testing.expectEqualSlices(u8, part, output[stripe.offset..][0..stripe.len]);
    }
}

test "RuneMap first fit accepts forward fits and wraps only after a miss" {
    try checkPacking(false, &.{ "ABC", "BCD", "CDE", "DEF" }, 6);
    try checkPacking(false, &.{ "ABCD", "CD", "AB" }, 4);
    try checkPacking(false, &.{ "ABCDE", "DE", "BCD" }, 5);
    try checkPacking(false, &.{ "ABCAB", "CAB", "ABC" }, 6);
    try checkPacking(false, &.{ "ABCDE", "DE", "BCDEF" }, 6);
    try checkPacking(false, &.{ "ABC", "BC", "CAB" }, 5);
    try checkPacking(false, &.{ "ABABAB", "BAB", "ABABABC" }, 9);
    try checkPacking(false, &.{ "ABABAB", "ABABC" }, 7);
    try checkPacking(false, &.{ "", "AAAA", "AA", "AAAAAA", "AAA" }, 6);
    try checkPacking(false, &.{ "", "", "" }, 0);
    try checkPacking(false, &.{"AAAA"}, 4);
}

test "RuneMap first fit prefers matches at or after the cursor" {
    var stripes = [_]Stripe{
        .{ .start = 0, .len = 6 },
        .{ .start = 6, .len = 2 },
        .{ .start = 8, .len = 2 },
        .{ .start = 10, .len = 2 },
    };
    const output = try firstFit(u8, testing.allocator, "ABxxABxxABxx", &stripes);
    defer testing.allocator.free(output);
    try testing.expectEqualStrings("ABxxAB", output);
    for (stripes, [_]u32{ 0, 2, 4, 2 }) |stripe, offset| try expectEqual(offset, stripe.offset);
}

test "RuneMap first fit agrees with circular candidate search" {
    var prng = std.Random.DefaultPrng.init(0x317f17);
    const random = prng.random();
    var vals: [256]u8 = undefined;
    var expected: [256]u8 = undefined;
    var stripes: [16]Stripe = undefined;
    for (0..200) |_| {
        var used: u32 = 0;
        for (&stripes) |*stripe| {
            const len = random.intRangeAtMost(u32, 0, 16);
            stripe.* = .{ .start = used, .len = len };
            for (vals[used..][0..len]) |*value| value.* = random.uintLessThan(u8, 3);
            used += len;
        }
        const output = try firstFit(u8, testing.allocator, vals[0..used], &stripes);
        defer testing.allocator.free(output);
        var expected_len: usize = stripes[0].len;
        @memcpy(expected[0..expected_len], vals[0..expected_len]);
        var cursor: usize = 0;
        for (stripes[1..]) |stripe| {
            const pattern = vals[stripe.start..][0..stripe.len];
            if (pattern.len == 0) {
                try expectEqual(cursor, stripe.offset);
                continue;
            }
            var placement = expected_len;
            var matched: usize = 0;
            for (0..expected_len) |step| {
                const candidate = (cursor + step) % expected_len;
                const count = @min(pattern.len, expected_len - candidate);
                if (std.mem.eql(u8, pattern[0..count], expected[candidate..][0..count])) {
                    placement = candidate;
                    matched = count;
                    break;
                }
            }
            const rest = pattern[matched..];
            @memcpy(expected[expected_len..][0..rest.len], rest);
            expected_len += rest.len;
            cursor = placement;
            try expectEqual(cursor, stripe.offset);
        }
        try testing.expectEqualSlices(u8, expected[0..expected_len], output);
    }
}

test "RuneMap good fit containment overlaps cycles and anchored ASCII" {
    try checkPacking(true, &.{ "", "ABC", "BCA", "CAB" }, 5);
    try checkPacking(true, &.{ "", "CD", "BCD", "ABCDE", "BC", "ABCDE" }, 5);
    try checkPacking(true, &.{ "ABC", "BCD", "CDE", "DEF" }, 6);
    try checkPacking(true, &.{ "ABCD", "CD", "AB" }, 4);
    try checkPacking(true, &.{ "ABC", "XABC" }, 7);
    try checkPacking(true, &.{ "ABC", "ABCDE", "BCD" }, 5);
    try checkPacking(true, &.{ "ABC", "ABC", "ABC", "" }, 3);
    try checkPacking(true, &.{ "", "AAAA", "AA", "AAAAAA", "AAA" }, 6);
    try checkPacking(true, &.{ "", "XY", "AB", "QR" }, 6);
    try checkPacking(true, &.{ "", "", "" }, 0);
    try checkPacking(true, &.{"AAAA"}, 4);
    const ascii = "x" ** 62 ++ "ABC" ++ "y" ** 60 ++ "DEF";
    try checkPacking(true, &.{ ascii, "ABC", "DEFG" }, 129);
}

test "RuneMap good fit finds the longest overlap before shorter joins" {
    // Joining ABC -> CDE first loses the longer ABC -> BCX overlap.
    try checkPacking(true, &.{ "", "ABC", "CDE", "BCX" }, 7);
    try checkPacking(true, &.{ "", "GATT", "ATTAC", "TACA", "ACAG" }, 8);
}

test "RuneMap packing preserves randomized stripes and never grows input" {
    var prng = std.Random.DefaultPrng.init(0x71aef19);
    const random = prng.random();
    var vals: [4096]u8 = undefined;
    var stripes: [48]Stripe = undefined;
    for (0..1000) |_| {
        var used: usize = 0;
        const count = random.intRangeAtMost(usize, 1, stripes.len);
        const alphabet = random.intRangeAtMost(u8, 1, 32);
        for (stripes[0..count], 0..) |*stripe, i| {
            const len = random.intRangeAtMost(usize, 0, if (i == 0) 128 else 64);
            stripe.* = .{ .start = @intCast(used), .len = @intCast(len) };
            for (vals[used..][0..len]) |*value| value.* = random.uintLessThan(u8, alphabet);
            used += len;
        }
        inline for (.{ false, true }) |good| {
            const output = if (good)
                try goodFit(u8, testing.allocator, vals[0..used], stripes[0..count])
            else
                try firstFit(u8, testing.allocator, vals[0..used], stripes[0..count]);
            defer testing.allocator.free(output);
            try expect(output.len <= used);
            try expectEqual(@as(usize, 0), stripes[0].offset);
            for (stripes[0..count]) |stripe| {
                try testing.expectEqualSlices(u8, vals[stripe.start..][0..stripe.len], output[stripe.offset..][0..stripe.len]);
            }
        }
    }
}

test "RuneMap compression preserves structural pointer slice and float identity" {
    const Value = struct { tag: u8, float: f64, slice: []const u8, ptr: *const u8 };
    var left = [_]u8{ 1, 2 };
    var right = left;
    const nan: f64 = @bitCast(@as(u64, 0x7ff8000000000123));
    const vals = [_]Value{
        .{ .tag = 7, .float = 0.0, .slice = &left, .ptr = &left[0] },
        .{ .tag = 7, .float = -0.0, .slice = &left, .ptr = &left[0] },
        .{ .tag = 7, .float = nan, .slice = &left, .ptr = &left[0] },
        .{ .tag = 7, .float = nan, .slice = &left, .ptr = &left[0] },
        .{ .tag = 7, .float = 0.0, .slice = &right, .ptr = &left[0] },
        .{ .tag = 7, .float = 0.0, .slice = &left, .ptr = &right[0] },
    };
    inline for (.{ false, true }) |good| {
        var stripes: [vals.len + 1]Stripe = undefined;
        stripes[0] = .{ .start = 0, .len = 0 };
        for (stripes[1..], 0..) |*stripe, i| stripe.* = .{ .start = @intCast(i), .len = 1 };
        const output = if (good)
            try goodFit(Value, testing.allocator, &vals, &stripes)
        else
            try firstFit(Value, testing.allocator, &vals, &stripes);
        defer testing.allocator.free(output);
        try expectEqual(@as(usize, 5), output.len);
        for (vals, stripes[1..]) |value, stripe| try expect(valueEql(value, output[stripe.offset]));
    }
}

test "RuneMap good fit groups thousands of interchangeable overlap candidates" {
    const pairs = 2048;
    const vals = try testing.allocator.alloc(u32, pairs * 128);
    defer testing.allocator.free(vals);
    @memset(vals, 0);
    const stripes = try testing.allocator.alloc(Stripe, 1 + pairs * 2);
    defer testing.allocator.free(stripes);
    stripes[0] = .{ .start = 0, .len = 0 };
    for (0..pairs) |i| {
        vals[i * 128] = @intCast(i + 1);
        vals[i * 128 + 127] = @intCast(i + 1);
        stripes[1 + i * 2] = .{ .start = @intCast(i * 128), .len = 64 };
        stripes[2 + i * 2] = .{ .start = @intCast(i * 128 + 64), .len = 64 };
    }
    const output = try goodFit(u32, testing.allocator, vals, stripes);
    defer testing.allocator.free(output);
    try expect(output.len <= vals.len - pairs * 63);
    for (stripes) |stripe| {
        try testing.expectEqualSlices(u32, vals[stripe.start..][0..stripe.len], output[stripe.offset..][0..stripe.len]);
    }
}

test "RuneMap compression supports nested tagged values and zero-sized values" {
    const Value = union(enum) { nothing, scalar: ?f32, vector: @Vector(2, f32), result: error{Bad}![2]u16 };
    const vals = [_]Value{
        .nothing,                      .{ .scalar = null },      .{ .scalar = -0.0 },      .{ .scalar = 0.0 },
        .{ .vector = .{ 1.0, -0.0 } }, .{ .result = error.Bad }, .{ .result = .{ 1, 2 } },
    };
    var stripes: [vals.len + 1]Stripe = undefined;
    stripes[0] = .{ .start = 0, .len = 0 };
    for (stripes[1..], 0..) |*stripe, i| stripe.* = .{ .start = @intCast(i), .len = 1 };
    const output = try goodFit(Value, testing.allocator, &vals, &stripes);
    defer testing.allocator.free(output);
    try expectEqual(vals.len, output.len);
    for (vals, stripes[1..]) |value, stripe| try expect(valueEql(value, output[stripe.offset]));

    var empty_stripes = [_]Stripe{ .{ .start = 0, .len = 1 }, .{ .start = 1, .len = 2 } };
    const empty = try goodFit(void, testing.allocator, &.{ {}, {}, {} }, &empty_stripes);
    defer testing.allocator.free(empty);
    try expectEqual(@as(usize, 2), empty.len);
}

const std = @import("std");
const Allocator = std.mem.Allocator;
const OOM = Allocator.Error;
const assert = std.debug.assert;
