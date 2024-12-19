const std = @import("std");

pub fn build(b: *std.Build) void {
    const target = b.standardTargetOptions(.{});
    const optimize = b.standardOptimizeOption(.{});
    const lib = b.addStaticLibrary(.{
        .name = "zyp",
        .root_source_file = b.path("src/root.zig"),
        .target = target,
        .optimize = optimize,
    });

    b.installArtifact(lib);

    const zyp = b.addModule("zyp", .{ .root_source_file = b.path("src/root.zig") });

    const unicode_id = b.dependency("unicode_id", .{});
    zyp.addImport("unicode-id", unicode_id.module("unicode-id"));

    const exe = b.addExecutable(.{
        .name = "zyp",
        .root_source_file = b.path("src/main.zig"),
        .target = target,
        .optimize = optimize,
    });
    exe.root_module.addImport("zyp", zyp);
    // This is only needed to get ZLS autocompletion.
    exe.root_module.addImport("unicode-id", unicode_id.module("unicode-id"));

    {
        const test_suite = b.addExecutable(.{
            .name = "test_suite",
            .root_source_file = b.path("tools/test_suite.zig"),
            .target = target,
            .optimize = optimize,
        });

        test_suite.root_module.addImport("zyp", zyp);
        const test_suite_cmd = b.addRunArtifact(test_suite);

        const test_runner_step = b.step("test_suite", "Run the Python test suite");
        test_runner_step.dependOn(&test_suite_cmd.step);
        if (b.args) |args| {
            test_suite_cmd.addArgs(args);
        }
    }

    b.installArtifact(exe);

    const run_cmd = b.addRunArtifact(exe);
    run_cmd.step.dependOn(b.getInstallStep());

    if (b.args) |args| {
        run_cmd.addArgs(args);
    }

    const run_step = b.step("run", "Run the app");
    run_step.dependOn(&run_cmd.step);

    const lib_unit_tests = b.addTest(.{
        .root_source_file = b.path("src/root.zig"),
        .target = target,
        .optimize = optimize,
    });
    lib_unit_tests.root_module.addImport("zyp", zyp);
    lib_unit_tests.root_module.addImport("unicode-id", unicode_id.module("unicode-id"));

    const run_lib_unit_tests = b.addRunArtifact(lib_unit_tests);

    const exe_unit_tests = b.addTest(.{
        .root_source_file = b.path("src/main.zig"),
        .target = target,
        .optimize = optimize,
    });
    exe_unit_tests.root_module.addImport("zyp", zyp);
    exe_unit_tests.root_module.addImport("unicode-id", unicode_id.module("unicode-id"));

    const run_exe_unit_tests = b.addRunArtifact(exe_unit_tests);

    // Similar to creating the run step earlier, this exposes a `test` step to
    // the `zig build --help` menu, providing a way for the user to request
    // running the unit tests.
    const test_step = b.step("test", "Run unit tests");
    test_step.dependOn(&run_lib_unit_tests.step);
    test_step.dependOn(&run_exe_unit_tests.step);
}
