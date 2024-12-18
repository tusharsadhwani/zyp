const std = @import("std");

const tokenizer = @import("zyp").tokenizer;

const PyToken = struct {
    type: []const u8,
    start: struct { u32, u32 },
    end: struct { u32, u32 },
};

fn to_py_token(al: std.mem.Allocator, token: tokenizer.Token) !PyToken {
    const py_token_type = try if (token.type.is_operator())
        al.dupe(u8, "OP")
    else
        std.ascii.allocUpperString(al, @tagName(token.type));

    return PyToken{
        .type = py_token_type,
        .start = .{ token.start_line, token.start_col },
        .end = .{ token.end_line, token.end_col },
    };
}

fn check(allocator: std.mem.Allocator, file_path: []const u8, source: []const u8, debug: bool) !void {
    var tokens = tokenizer.TokenIterator.init(allocator, source);
    defer tokens.deinit();

    var py_tokens = std.ArrayList(PyToken).init(allocator);
    defer {
        for (py_tokens.items) |py_token| {
            allocator.free(py_token.type);
        }
        py_tokens.deinit();
    }

    while (true) {
        const token = try tokens.next();
        if (token.type == .whitespace) continue;
        try py_tokens.append(try to_py_token(allocator, token));
        if (token.type == .endmarker) break;
    }

    var env_map = try std.process.getEnvMap(allocator);
    defer env_map.deinit();

    const result = try std.process.Child.run(.{
        .allocator = allocator,
        .argv = &.{
            "python3",
            "./simpler_tokenizer.py",
            file_path,
        },
        .env_map = &env_map,
        .max_output_bytes = 1024 * 1024, // 1MB buffer
    });
    defer allocator.free(result.stdout);
    defer allocator.free(result.stderr);

    if (result.term.Exited != 0) return error.SubprocessFailed;
    const expected_py_tokens_ = try std.json.parseFromSlice([]PyToken, allocator, result.stdout, .{});
    defer expected_py_tokens_.deinit();
    var expected_py_tokens = std.ArrayList(PyToken).init(allocator);
    defer expected_py_tokens.deinit();

    try expected_py_tokens.append(expected_py_tokens_.value[0]);
    for (expected_py_tokens_.value[1..], 1..) |token, index| {
        const last_token = expected_py_tokens.getLast();
        // Merge consecutive FSTRING_MIDDLE tokens. it's weird cpython has it like that.
        if (std.mem.eql(u8, token.type, "FSTRING_MIDDLE") and
            std.mem.eql(u8, last_token.type, "FSTRING_MIDDLE"))
        {
            _ = expected_py_tokens.pop();
            try expected_py_tokens.append(PyToken{
                .type = token.type,
                .start = last_token.start,
                .end = token.end,
            });
            continue;
        }
        if (index + 1 < expected_py_tokens_.value.len) {
            // When an FSTRING_MIDDLE ends with a `{{{` like f'x{{{1}', Python eats
            // the last { char as well as its end index, so we get a `x{` token
            // instead of the expected `x{{` token. This fixes that case. Pretty
            // much always there should be no gap between an fstring-middle ending
            // and the { op after it.
            // Same deal for `}}}"`
            const next_token = expected_py_tokens_.value[index + 1];
            if ((std.mem.eql(u8, token.type, "FSTRING_MIDDLE") and std.mem.eql(u8, next_token.type, "OP")) or
                (std.mem.eql(u8, token.type, "FSTRING_MIDDLE") and std.mem.eql(u8, next_token.type, "FSTRING_END")) and
                token.end[0] == next_token.start[0] and
                next_token.start[1] > token.end[1])
            {
                try expected_py_tokens.append(PyToken{
                    .type = token.type,
                    .start = token.start,
                    .end = next_token.start,
                });
                continue;
            }
        }
        try expected_py_tokens.append(token);
    }

    if (expected_py_tokens.items.len != py_tokens.items.len) {
        if (debug) {
            std.debug.print("--- expected:\n", .{});
            for (expected_py_tokens.items) |expected_py_token|
                std.debug.print("{s} {any} {any}\n", .{
                    expected_py_token.type,
                    expected_py_token.start,
                    expected_py_token.end,
                });
            std.debug.print("--- got:\n", .{});
            for (py_tokens.items) |py_token|
                std.debug.print("{s} {any} {any}\n", .{
                    py_token.type,
                    py_token.start,
                    py_token.end,
                });
        }
        return error.DifferentLengths;
    }
    for (py_tokens.items, expected_py_tokens.items) |py_token, expected_py_token| {
        if (debug)
            std.debug.print("EXPECTED {s} {any} {any} - GOT {s} {any} {any}\n", .{
                expected_py_token.type,
                expected_py_token.start,
                expected_py_token.end,
                py_token.type,
                py_token.start,
                py_token.end,
            });
        if (!std.mem.eql(u8, expected_py_token.type, py_token.type)) return error.NotEqual;
        if (expected_py_token.start[0] != py_token.start[0]) return error.NotEqual;
        if (expected_py_token.start[1] != py_token.start[1]) return error.NotEqual;
        if (expected_py_token.end[0] != py_token.end[0]) return error.NotEqual;
        if (expected_py_token.end[1] != py_token.end[1]) return error.NotEqual;
    }
}

fn all_digits(string: []const u8) bool {
    for (string) |ch| {
        if (!std.ascii.isDigit(ch))
            return false;
    }
    return true;
}

fn compare_strings(_: void, lhs: []const u8, rhs: []const u8) bool {
    return std.mem.order(u8, lhs, rhs).compare(std.math.CompareOperator.lt);
}

/// Resolve file and directory paths recursively, and return a list of Python files
/// present in the given paths.
pub fn get_python_files(al: std.mem.Allocator, path: []u8, debug: bool) !std.ArrayList([]u8) {
    var files = std.ArrayList([]u8).init(al);
    errdefer files.deinit();
    try _get_python_files(al, &files, path, debug);
    return files;
}
fn _get_python_files(al: std.mem.Allocator, files: *std.ArrayList([]u8), path: []u8, debug: bool) !void {
    // openFile fails on symlinks that point to paths that don't exist, skip those
    const file = std.fs.cwd().openFile(path, .{}) catch |err| {
        if (err == std.fs.File.OpenError.ProcessFdQuotaExceeded) return err;
        if (debug) std.debug.print("Failed to open {s}: {s}\n", .{ path, @errorName(err) });
        return;
    };
    defer file.close();

    const stat = try file.stat();
    if (debug) std.debug.print("Path {s} is a {s}\n", .{ path, @tagName(stat.kind) });
    switch (stat.kind) {
        .file => {
            if (std.mem.eql(u8, std.fs.path.extension(path), ".py")) {
                if (debug) std.debug.print("Storing python file {s}\n", .{path});
                try files.append(try al.dupe(u8, path));
            }
        },
        .directory => {
            // openDir fails on symlinks when .no_follow is given, skip those
            var dir = std.fs.cwd().openDir(
                path,
                .{ .iterate = true, .no_follow = true },
            ) catch |err| {
                if (debug) std.debug.print("Failed to open {s}: {s}\n", .{ path, @errorName(err) });
                return;
            };
            defer dir.close();

            var entries = dir.iterate();
            while (try entries.next()) |entry| {
                // Ignore dotted files / folders
                if (entry.name[0] == '.') {
                    if (debug) std.debug.print("Skipping hidden path {s}\n", .{entry.name});
                    continue;
                }
                const child_path = try std.fs.path.join(al, &.{ path, entry.name });
                defer al.free(child_path);
                try _get_python_files(al, files, child_path, debug);
            }
        },
        else => {},
    }
}

pub fn main() !u8 {
    var args = std.process.args();
    _ = args.skip(); // proc name

    const filename_arg = args.next();
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer std.debug.assert(gpa.deinit() == .ok);
    const allocator = gpa.allocator();

    var test_file_names = std.ArrayList([]const u8).init(allocator);
    defer {
        for (test_file_names.items) |item| allocator.free(item);
        test_file_names.deinit();
    }

    const dir_path = "./tools/tokenize_tests";
    const test_suite_dir = try std.fs.cwd().openDir(
        dir_path,
        .{ .iterate = true },
    );
    if (filename_arg == null) {
        var dir_iterator = test_suite_dir.iterate();
        while (try dir_iterator.next()) |entry| {
            if (entry.kind != .file) continue;
            const test_number = entry.name[entry.name.len - 7 .. entry.name.len - 3];
            if (!all_digits(test_number))
                continue;

            try test_file_names.append(try allocator.dupe(u8, entry.name));
        }

        std.sort.heap([]const u8, test_file_names.items, {}, compare_strings);
    } else {
        const file_name = if (all_digits(filename_arg.?))
            try std.fmt.allocPrint(allocator, "test_{s}.py", .{filename_arg.?})
        else
            try allocator.dupe(u8, filename_arg.?);

        // Check if directory
        if (!all_digits(filename_arg.?)) {
            const stat = try std.fs.cwd().statFile(file_name);
            if (stat.kind == .directory) {
                const files = try get_python_files(allocator, file_name, false);
                defer files.deinit();
                for (files.items) |file|
                    try test_file_names.append(file);

                allocator.free(file_name);
            } else {
                try test_file_names.append(file_name);
            }
        } else {
            try test_file_names.append(file_name);
        }
    }

    var test_results = std.json.ObjectMap.init(allocator);
    defer test_results.deinit();

    const single_run = test_file_names.items.len == 1;
    var failed = false;
    for (test_file_names.items) |file_name| {
        const file_path = if (std.mem.startsWith(u8, file_name, "test_"))
            try std.fs.path.join(allocator, &.{ dir_path, file_name })
        else
            try allocator.dupe(u8, file_name);
        defer allocator.free(file_path);

        const source = std.fs.cwd().readFileAlloc(
            allocator,
            file_path,
            std.math.maxInt(u32),
        ) catch |err| {
            switch (err) {
                std.fs.File.OpenError.FileNotFound => {
                    std.debug.print("Error: Test file {s} not found\n", .{file_name});
                    return 2;
                },
                else => return err,
            }
        };
        defer allocator.free(source);

        check(allocator, file_path, source, single_run) catch |err| {
            if (single_run) return err;
            std.debug.print("\x1b[1;31mF\x1b[m", .{});
            // std.debug.print(" - {s}\n", .{file_name});
            try test_results.put(file_name, .{ .bool = false });
            failed = true;
            continue;
        };
        try test_results.put(file_name, .{ .bool = true });
        std.debug.print("\x1b[1;32m.\x1b[m", .{});
        // std.debug.print(" - {s}\n", .{file_name});
    }
    std.debug.print("\n", .{});

    // Check for regressions if the regular test suite is being run
    if (filename_arg == null) {
        // Read last results json file
        const last_result_filename = "test_suite_last_result.json";
        const regressions = blk: {
            var _regressions = std.ArrayList([]const u8).init(allocator);

            // no previous run file, can't calculate regressions.
            _ = test_suite_dir.statFile(last_result_filename) catch break :blk null;

            const last_result_file = try test_suite_dir.openFile(last_result_filename, .{});
            defer last_result_file.close();
            const contents = try last_result_file.readToEndAlloc(allocator, std.math.maxInt(u32));
            defer allocator.free(contents);
            var last_test_results = try std.json.parseFromSlice(std.json.Value, allocator, contents, .{});
            defer last_test_results.deinit();

            var result_iterator = test_results.iterator();
            while (result_iterator.next()) |result| {
                const test_name = result.key_ptr.*;
                const last_result = last_test_results.value.object.get(test_name) orelse {
                    std.debug.print("Test case {s} didn't exist in last results\n", .{test_name});
                    continue;
                };
                if (last_result.bool == true and result.value_ptr.bool == false) {
                    failed = true;
                    try _regressions.append(test_name);
                }
            }

            break :blk _regressions;
        };
        defer if (regressions) |reg| reg.deinit();

        // Save if no regressions found
        if (regressions == null or regressions.?.items.len == 0) {
            const result_file = try test_suite_dir.createFile(last_result_filename, .{});
            defer result_file.close();
            try std.json.stringify(
                std.json.Value{ .object = test_results },
                .{ .whitespace = .indent_2 },
                result_file.writer(),
            );
        }

        // Print stats
        var failed_tests = std.ArrayList([]const u8).init(allocator);
        defer failed_tests.deinit();
        var result_iterator = test_results.iterator();
        while (result_iterator.next()) |result| {
            const passed = result.value_ptr.bool;
            if (!passed) {
                const test_name = result.key_ptr.*;
                try failed_tests.append(test_name);
            }
        }
        const failed_tests_str = try std.mem.join(allocator, "\n", failed_tests.items);
        defer allocator.free(failed_tests_str);
        if (failed)
            std.debug.print("\x1b[1;31mFailed tests:\x1b[m\n{s}\n\n", .{failed_tests_str});
        if (regressions) |reg|
            if (reg.items.len > 0) {
                const regressed_tests_str = try std.mem.join(allocator, "\n", reg.items);
                defer allocator.free(regressed_tests_str);
                std.debug.print("\x1b[1;31mRegressed tests:\x1b[m\n{s}\n", .{regressed_tests_str});
            };

        std.debug.print(
            "\x1b[1;32mSummary: {d} Passed, {d} Failed, {any} Regressed\x1b[m\n",
            .{ test_results.count() - failed_tests.items.len, failed_tests.items.len, if (regressions) |reg| reg.items.len else null },
        );
        if (regressions) |reg| if (reg.items.len > 0) {
            std.debug.print("\x1b[1;31mREGRESSIONS FOUND\x1b[m\n", .{});
        };
    }

    return if (failed) 1 else 0;
}
