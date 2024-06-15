//! Quick and Dirty Fuzz Checker for creating RuneSets
//!
//! There are three unreachables in createFromConstString.
//!
//! This little test program tries to reach them, with brute force and
//! ignorance.
//!

const std = @import("std");
const runeset = @import("runeset.zig");
const RuneSet = runeset.RuneSet;

// your options for FUZZ TIME are (comment in what you want)

// a mere sip, all over in a jiffy
const FUZZ_TIME = std.math.maxInt(u16);

// this takes a respectable amount of time, about four minutes
// on my laptop
// const FUZZ_TIME = std.math.pow(u64, 2, 24);

// for overkill, we offer the following:
// const FUZZ_TIME = std.math.pow(u64, 2, 26);

// this will not complete in an amount of time worth dedicating
// to the task.  expensive space heater
// const FUZZ_TIME = std.math.maxInt(u32);

fn bruteFuzzAndIgnorance(alloc: std.mem.Allocator) !void {
    var count: usize = 0;
    var buf: [384]u8 = undefined;
    var prng = std.Random.DefaultPrng.init(blk: {
        var seed: u64 = undefined;
        try std.posix.getrandom(std.mem.asBytes(&seed));
        break :blk seed;
    });
    const rand = prng.random();
    while (count < FUZZ_TIME) : (count += 1) {
        rand.bytes(&buf);
        // this will not fill often, but when it does, we
        // do need to empty it:
        const set: ?RuneSet = RuneSet.createFromConstString(&buf, alloc) catch |e| blk: {
            switch (e) {
                error.InvalidUnicode => break :blk null,
                else => unreachable,
            }
        };
        if (set) |s| s.deinit(alloc);
    }
}

test "fuzz the puppy" {
    const allocator = std.testing.allocator;
    try bruteFuzzAndIgnorance(allocator);
    try std.testing.expect(true);
    std.debug.print("\nfuzzed\n", .{});
}
