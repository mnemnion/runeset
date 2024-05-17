//! libruneset: fast utf8 codepoint sets for Zig
//!
//!

const std = @import("std");
// "pub" might not be appropriate here, these types
// will most likely be fully encapsulated by the library.
pub usingnamespace @import("elements.zig");

const testing = std.testing;

test {
    std.testing.refAllDecls(@This());
}
