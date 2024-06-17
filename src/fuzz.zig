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
// const FUZZ_TIME = std.math.maxInt(u16);

// this option takes about 30 seconds on my laptop
const FUZZ_TIME = std.math.pow(u64, 2, 24);

// for overkill, we offer the following, six minutes of crunch:
// const FUZZ_TIME = std.math.pow(u64, 2, 28);

// this will not complete in an amount of time worth dedicating
// to the task: estimated 25 hours.  expensive space heater
// const FUZZ_TIME = std.math.maxInt(u32);

/// bruteFuzzAndIgnorance
///
/// Design: we repeatedly feed a six byte random buffer to the creation
/// interface.  Because half of all bytes are valid UTF-8, six bytes is long
/// enough to trigger every code pathway, including the critical section
/// which could read past the end of the buffer.  About one in ten thousand
/// runs, for example, should have a valid four-byte sequence starting from [0],
/// which is the rarest outcome, so the amount of iterations provided is manifestly
/// adequate: even 2^16 runs should have this outcome 64 times on average.
///
/// We swallow any InvalidUnicode errors, and repeat.
pub fn bruteFuzzAndIgnorance(alloc: std.mem.Allocator) !void {
    var count: usize = 0;
    var buf: [6]u8 = undefined;
    var prng = std.Random.DefaultPrng.init(blk: {
        var seed: u64 = undefined;
        try std.posix.getrandom(std.mem.asBytes(&seed));
        break :blk seed;
    });
    const rand = prng.random();
    var count_completed: usize = 0;
    while (count < FUZZ_TIME) : (count += 1) {
        rand.bytes(&buf);
        // The buffer is short enough that this will succeed sometimes:
        const set: ?RuneSet = RuneSet.createFromMutableString(&buf, alloc) catch |e| blk: {
            switch (e) {
                error.InvalidUnicode => break :blk null,
                error.OutOfMemory => return e,
                else => unreachable,
            }
        };
        // In which case, we deallocate it
        if (set) |s| {
            count_completed += 1;
            s.deinit(alloc);
        }
    }
    const percent: f64 = @as(f64, @floatFromInt(count_completed)) / @as(f64, @floatFromInt(FUZZ_TIME));
    std.debug.print("\nfuzzed\n", .{});
    std.debug.print("completed {d} sets in {d} attempts, {d:<.2}%\n", .{ count_completed, FUZZ_TIME, percent * 100 });
}
