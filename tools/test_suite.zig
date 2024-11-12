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
    });
    defer allocator.free(result.stdout);
    defer allocator.free(result.stderr);

    if (result.term.Exited != 0) return error.SubprocessFailed;
    const expected_py_tokens = try std.json.parseFromSlice([]PyToken, allocator, result.stdout, .{});
    defer expected_py_tokens.deinit();

    if (expected_py_tokens.value.len != py_tokens.items.len) {
        if (debug) {
            std.debug.print("--- expected:\n", .{});
            for (expected_py_tokens.value) |expected_py_token|
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
    for (py_tokens.items, expected_py_tokens.value) |py_token, expected_py_token| {
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

pub fn main() !u8 {
    var args = std.process.args();
    _ = args.skip(); // proc name

    const test_number_arg = blk: {
        if (args.next()) |arg| {
            if (all_digits(arg)) {
                break :blk arg;
            }
        }
        break :blk null;
    };
    // Print debug info when a specific test is run
    const single_run = test_number_arg != null;

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
    if (test_number_arg == null) {
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
        const file_name = try std.fmt.allocPrint(allocator, "test_{s}.py", .{test_number_arg.?});
        _ = test_suite_dir.statFile(file_name) catch |err| {
            defer allocator.free(file_name);
            switch (err) {
                std.fs.File.OpenError.FileNotFound => {
                    std.debug.print("Error: Test file {s} not found", .{file_name});
                    return 2;
                },
                else => return err,
            }
        };
        // Stat succeeded, file exists, run it
        try test_file_names.append(file_name);
    }

    var test_results = std.json.ObjectMap.init(allocator);
    defer test_results.deinit();

    var failed = false;
    for (test_file_names.items) |file_name| {
        const source = try test_suite_dir.readFileAlloc(
            allocator,
            file_name,
            std.math.maxInt(u32),
        );
        defer allocator.free(source);

        const file_path = try std.fs.path.join(allocator, &.{ dir_path, file_name });
        defer allocator.free(file_path);
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

    if (!single_run) {
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
                    std.debug.print("Test case {s} didn't exist in last results", .{test_name});
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
