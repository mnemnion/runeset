const std = @import("std");

pub fn build(b: *std.Build) void {
    const target = b.standardTargetOptions(.{});

    const optimize = b.standardOptimizeOption(.{});

    const lib = b.addStaticLibrary(.{
        .name = "runeset",
        .root_source_file = b.path("src/runeset.zig"),
        .target = target,
        .optimize = optimize,
    });

    b.installArtifact(lib);

    // Export as module to be available for @import("runeset") on user site
    _ = b.addModule("runeset", .{
        .root_source_file = b.path("src/runeset.zig"),
        .target = target,
        .optimize = optimize,
    });

    const options = b.addOptions();
    if (b.option(bool, "test-more", "run more extensive tests") orelse false) {
        options.addOption(bool, "test_more", true);
    } else {
        options.addOption(bool, "test_more", false);
    }

    // Creates a step for unit testing. This only builds the test executable
    // but does not run it.
    const lib_unit_tests = b.addTest(.{
        .root_source_file = b.path("src/test-runeset.zig"),
        .target = target,
        .optimize = optimize,
    });

    const run_lib_unit_tests = b.addRunArtifact(lib_unit_tests);
    lib_unit_tests.root_module.addOptions("config", options);
    run_lib_unit_tests.has_side_effects = true;

    b.installDirectory(.{
        .source_dir = lib.getEmittedDocs(),
        .install_dir = .prefix,
        .install_subdir = "../docs",
    });

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
    _ = cov_run.captureStdOut();
    _ = cov_run.captureStdErr();
}
