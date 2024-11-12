const std = @import("std");

pub const tokenizer = @import("tokenizer.zig");

fn read_file(allocator: std.mem.Allocator, filepath: []const u8) ![]u8 {
    return try std.fs.cwd().readFileAlloc(allocator, filepath, std.math.maxInt(u32));
}

pub fn main() !u8 {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer std.debug.assert(gpa.deinit() == .ok);
    const allocator = gpa.allocator();

    var args = std.process.args();
    _ = args.skip();

    const filepath_arg = args.next();
    if (filepath_arg) |filepath| {
        const source = try read_file(allocator, filepath);
        defer allocator.free(source);

        var token_iterator = tokenizer.TokenIterator.init(allocator, source);
        defer token_iterator.deinit();
        while (true) {
            const token = try token_iterator.next();
            if (token.type != .whitespace) std.debug.print(
                "{s} {any}\n",
                .{ token.to_byte_slice(source), token },
            );
            if (token.type == .endmarker) break;
        }
    } else {
        std.debug.print("Usage: zyp <filepath.py>\n", .{});
        return 1;
    }
    return 0;
}
