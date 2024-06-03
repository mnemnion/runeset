const std = @import("std");

pub const elements = @import("elements.zig");
pub const runeset = @import("runeset.zig");

const expect = std.testing.expect;

test "is this thing on?" {
    std.debug.print("\nthis thing is, in fact, on\n", .{});
    try expect(true);
}

test {
    std.testing.refAllDecls(@This());
}
