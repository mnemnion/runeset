const std = @import("std");

pub fn build(b: *std.Build) void {
    const target = b.standardTargetOptions(.{});

    const optimize = b.standardOptimizeOption(.{});

    // Export as module to be available for @import("runeset") on user site
    const runeset_mod = b.addModule("runeset", .{
        .root_source_file = b.path("src/runeset.zig"),
        .target = target,
        .optimize = optimize,
    });

    _ = runeset_mod;

    const options = b.addOptions();
    if (b.option(bool, "test-more", "run more extensive tests") orelse false) {
        options.addOption(bool, "test_more", true);
    } else {
        options.addOption(bool, "test_more", false);
    }

    options.addOption(
        bool,
        "no_fuzz",
        b.option(bool, "no-fuzz", "don't fuzz for test_more") orelse false,
    );

    const test_filters: []const []const u8 = b.option(
        []const []const u8,
        "test-filter",
        "Skip tests that do not match any of the specified filters",
    ) orelse &.{};

    const runeset_test_mod = b.createModule(.{
        .root_source_file = b.path("src/test-runeset.zig"),
        .target = target,
        .optimize = optimize,
    });

    // Creates a step for unit testing. This only builds the test executable
    // but does not run it.
    const lib_unit_tests = b.addTest(.{
        .root_module = runeset_test_mod,
        .filters = test_filters,
    });

    const run_lib_unit_tests = b.addRunArtifact(lib_unit_tests);
    lib_unit_tests.root_module.addOptions("config", options);
    run_lib_unit_tests.has_side_effects = true;

    // ZTap test runner step.
    const ztap_dep = b.dependency("ztap", .{
        .target = target,
        .optimize = optimize,
        .timed = true,
        .threaded = true,
    });

    const ztap_unit_tests = b.addTest(.{
        .name = "ztap-run",
        .root_module = runeset_test_mod,
        .filters = test_filters,
        .test_runner = .{ .path = ztap_dep.namedLazyPath("runner"), .mode = .simple },
    });

    ztap_unit_tests.root_module.addOptions("config", options);

    const run_ztap_tests = b.addRunArtifact(ztap_unit_tests);
    run_ztap_tests.has_side_effects = true;
    b.installArtifact(ztap_unit_tests);

    ztap_unit_tests.root_module.addImport("ztap", ztap_dep.module("ztap"));

    const ztap_step = b.step("ztap", "Run ZTap unit tests");
    ztap_step.dependOn(&run_ztap_tests.step);

    // Similar to creating the run step earlier, this exposes a `test` step to
    // the `zig build --help` menu, providing a way for the user to request
    // running the unit tests.
    const test_step = b.step("test", "Run unit tests");
    test_step.dependOn(&run_lib_unit_tests.step);

    // Adds a step to generate code coverage
    const cov_step = b.step("cov", "Generate coverage (kcov must be installed)");

    const cov_run = b.addSystemCommand(&.{
        "kcov",
        "--clean",
        "--include-pattern=src/",
        "--exclude-line=unreachable,expect(false)",
        "kcov-output",
    });
    cov_run.addArtifactArg(lib_unit_tests);
    cov_step.dependOn(&cov_run.step);
}
