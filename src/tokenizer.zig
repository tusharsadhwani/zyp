const std = @import("std");

pub const TokenType = enum(u8) {
    whitespace,
    newline,
    name,

    _op_start,
    lparen,
    rparen,
    op,
    _op_end,

    number,
    endmarker,

    const Self = @This();

    pub fn is_operator(self: Self) bool {
        const token_int: u8 = @intFromEnum(self);
        return token_int > @intFromEnum(Self._op_start) and token_int < @intFromEnum(Self._op_end);
    }
};

pub const Token = struct {
    type: TokenType,
    /// Byte offsets in the file
    start_index: u32,
    end_index: u32,
    line_number: u32,
    // 0-indexed offset from start of line
    byte_offset: u32,

    const Self = @This();

    fn to_byte_slice(self: *const Self, source: []const u8) []const u8 {
        return source[self.start_index..self.end_index];
    }
};

fn is_whitespace(char: u8) bool {
    return char == ' ' or char == '\r' or char == '\t' or char == '\x0b' or char == '\x0c';
}

const TokenIterator = struct {
    source: []const u8,
    current_index: u32 = 0,
    line_number: u32 = 1,
    byte_offset: u32 = 0,

    const Self = @This();

    fn advance(self: *Self) void {
        self.current_index += 1;
        self.byte_offset += 1;
    }

    fn make_token(self: *Self, tok_type: TokenType, start_index: u32) Token {
        // std.debug.print("{any} {d} {d} {d} {d} {d}\n", .{
        //     tok_type,
        //     start_index,
        //     self.current_index,
        //     self.line_number,
        //     self.byte_offset,
        //     (self.current_index - start_index),
        // });
        return Token{
            .type = tok_type,
            .start_index = start_index,
            .end_index = self.current_index,
            .line_number = self.line_number,
            .byte_offset = self.byte_offset - (self.current_index - start_index),
        };
    }

    pub fn next(self: *Self) ?Token {
        if (self.current_index > self.source.len) return null;
        if (self.current_index == self.source.len) {
            const token = self.make_token(.endmarker, self.current_index);
            self.advance();
            return token;
        }

        const current_char = self.source[self.current_index];
        switch (current_char) {
            '\n' => {
                self.advance();
                const token = self.make_token(.newline, self.current_index - 1);
                self.line_number += 1;
                self.byte_offset = 0;
                return token;
            },
            // TODO: handle indentation
            ' ', '\r', '\t', '\x0b', '\x0c' => {
                const start_index = self.current_index;
                while (is_whitespace(self.source[self.current_index])) : (self.advance()) {}
                return self.make_token(
                    .whitespace,
                    start_index,
                );
            },
            '=' => {
                self.advance();
                return self.make_token(
                    .op,
                    self.current_index - 1,
                );
            },
            '(' => {
                self.advance();
                return self.make_token(
                    .lparen,
                    self.current_index - 1,
                );
            },
            ')' => {
                self.advance();
                return self.make_token(
                    .rparen,
                    self.current_index - 1,
                );
            },
            '0'...'9' => {
                const start_index = self.current_index;
                while (std.ascii.isDigit(self.source[self.current_index])) : (self.advance()) {}
                return self.make_token(
                    .number,
                    start_index,
                );
            },
            // TODO: unicode identifier
            'A'...'Z', 'a'...'z', '_' => {
                const start_index = self.current_index;
                while (std.ascii.isAlphanumeric(self.source[self.current_index]) or
                    self.source[self.current_index] == '_') : (self.advance())
                {}

                return self.make_token(
                    .name,
                    start_index,
                );
            },
            else => unreachable,
        }
    }
};

pub fn tokenize(source: []const u8) TokenIterator {
    return TokenIterator{ .source = source };
}

test tokenize {
    const source =
        \\x = 1
        \\print(x)
    ;
    var token_iterator = tokenize(source);

    var token_list = std.ArrayList(Token).init(std.testing.allocator);
    defer token_list.deinit();

    while (token_iterator.next()) |token| {
        try token_list.append(token);
    }

    const expected_tokens = [_]struct { TokenType, []const u8 }{
        .{ .name, "x" },
        .{ .whitespace, " " },
        .{ .op, "=" },
        .{ .whitespace, " " },
        .{ .number, "1" },
        .{ .newline, "\n" },
        .{ .name, "print" },
        .{ .lparen, "(" },
        .{ .name, "x" },
        .{ .rparen, ")" },
        .{ .endmarker, "" },
    };
    for (token_list.items, expected_tokens) |token, expected| {
        const expected_type, const expected_value = expected;
        try std.testing.expectEqual(expected_type, token.type);
        try std.testing.expectEqualStrings(expected_value, token.to_byte_slice(source));
    }
}

const PyToken = struct {
    type: []const u8,
    start: struct { u32, u32 },
    end: struct { u32, u32 },
};

fn to_py_token(al: std.mem.Allocator, token: Token) !PyToken {
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

test "tokenize but subprocess" {
    const source = @embedFile("./test_files/tokenize_test.py");
    var tokens = tokenize(source);
    var py_tokens = std.ArrayList(PyToken).init(std.testing.allocator);
    defer {
        for (py_tokens.items) |py_token| {
            std.testing.allocator.free(py_token.type);
        }
        py_tokens.deinit();
    }

    while (tokens.next()) |token| {
        if (token.type == .whitespace) continue;
        try py_tokens.append(try to_py_token(std.testing.allocator, token));
    }

    var env_map = try std.process.getEnvMap(std.testing.allocator);
    defer env_map.deinit();

    const result = try std.process.Child.run(.{
        .allocator = std.testing.allocator,
        .argv = &.{
            "python3",
            "./simpler_tokenizer.py",
            "./src/test_files/tokenize_test.py",
        },
        .env_map = &env_map,
    });
    defer std.testing.allocator.free(result.stdout);
    defer std.testing.allocator.free(result.stderr);

    try std.testing.expectEqual(0, result.term.Exited);
    const expected_py_tokens = try std.json.parseFromSlice([]PyToken, std.testing.allocator, result.stdout, .{});
    defer expected_py_tokens.deinit();

    for (py_tokens.items, expected_py_tokens.value) |py_token, expected_py_token| {
        try std.testing.expectEqualStrings(expected_py_token.type, py_token.type);
        try std.testing.expectEqualDeep(expected_py_token, py_token);
    }
}
