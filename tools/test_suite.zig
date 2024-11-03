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
        .start = .{ token.line_number, token.byte_offset },
        .end = .{ token.line_number, token.byte_offset + (token.end_index - token.start_index) },
    };
}

fn check(allocator: std.mem.Allocator, file_path: []const u8, source: []const u8, debug: bool) !void {
    var tokens = tokenizer.tokenize(source);
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

    if (expected_py_tokens.value.len != py_tokens.items.len) return error.DifferentLengths;
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
    const print_debug_info = test_number_arg != null;

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
        check(allocator, file_path, source, print_debug_info) catch {
            std.debug.print("\x1b[1;31mF\x1b[m", .{});
            failed = true;
            continue;
        };
        std.debug.print("\x1b[1;32m.\x1b[m", .{});
    }
    std.debug.print("\n", .{});
    return if (failed) 1 else 0;
}
