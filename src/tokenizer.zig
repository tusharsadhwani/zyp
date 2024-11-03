const std = @import("std");

pub const TokenType = enum(u8) {
    whitespace,
    newline, // semantically meaningful newline
    nl, // non meaningful newline
    comment,

    _op_start,
    semicolon,
    lparen,
    rparen,
    lbracket,
    rbracket,
    lbrace,
    rbrace,
    op,
    _op_end,

    name,
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
        // Newline at end of file may not exist in the file
        if (self.type == .newline and self.start_index == source.len and self.end_index == source.len + 1)
            return "\n";
        // Endmarkers are out of bound too
        if (self.type == .endmarker)
            return "";
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

    bracket_level: u32 = 0,

    const Self = @This();

    fn is_in_bounds(self: *Self) bool {
        return self.current_index < self.source.len;
    }

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

    fn newline(self: *Self) Token {
        const last_token_was_newline = self.current_index >= 1 and
            self.source[self.current_index - 1] == '\n';
        self.advance();
        const in_brackets = self.bracket_level > 0;
        const token_type: TokenType = if (in_brackets or last_token_was_newline) .nl else .newline;
        const token = self.make_token(token_type, self.current_index - 1);
        self.line_number += 1;
        self.byte_offset = 0;
        return token;
    }

    fn endmarker(self: *Self) Token {
        const token = self.make_token(.endmarker, self.current_index);
        return token;
    }

    fn decimal(self: *Self) Token {
        const start_index = self.current_index;
        while (self.is_in_bounds() and std.ascii.isDigit(self.source[self.current_index])) : (self.advance()) {}
        return self.make_token(
            .number,
            start_index,
        );
    }

    fn binary(self: *Self) Token {
        const start_index = self.current_index;
        // jump over `0b`
        self.advance();
        self.advance();
        while (self.is_in_bounds() and
            self.source[self.current_index] == '0' or self.source[self.current_index] == '1') : (self.advance())
        {}
        return self.make_token(
            .number,
            start_index,
        );
    }

    fn octal(self: *Self) Token {
        const start_index = self.current_index;
        // jump over `0o`
        self.advance();
        self.advance();
        while (self.is_in_bounds() and
            self.source[self.current_index] >= '0' and self.source[self.current_index] <= '7') : (self.advance())
        {}
        return self.make_token(
            .number,
            start_index,
        );
    }

    fn hexadecimal(self: *Self) Token {
        const start_index = self.current_index;
        // jump over `0o`
        self.advance();
        self.advance();
        while (self.is_in_bounds() and
            std.ascii.isHex(self.source[self.current_index])) : (self.advance())
        {}
        return self.make_token(
            .number,
            start_index,
        );
    }

    pub fn next(self: *Self) !Token {
        if (self.current_index == self.source.len) {
            if (self.source[self.current_index - 1] != '\n') {
                return self.newline();
            } else {
                return self.endmarker();
            }
        }
        if (self.current_index > self.source.len) {
            return self.endmarker();
        }

        const current_char = self.source[self.current_index];
        switch (current_char) {
            '\n' => {
                return self.newline();
            },
            '#' => {
                const start_index = self.current_index;
                while (self.is_in_bounds() and self.source[self.current_index] != '\n') : (self.advance()) {}
                return self.make_token(
                    .comment,
                    start_index,
                );
            },
            // TODO: handle indentation
            ' ', '\r', '\t', '\x0b', '\x0c' => {
                const start_index = self.current_index;
                while (self.is_in_bounds() and is_whitespace(self.source[self.current_index])) : (self.advance()) {}
                return self.make_token(
                    .whitespace,
                    start_index,
                );
            },
            '+', '-', '&', '|', '<', '>', '=', '!', '~' => {
                const start_index = self.current_index;
                self.advance();
                if (self.is_in_bounds() and self.source[self.current_index] == '=') self.advance();
                return self.make_token(.op, start_index);
            },
            '/' => {
                const start_index = self.current_index;
                self.advance();
                if (self.is_in_bounds() and self.source[self.current_index] == '/') self.advance();
                if (self.is_in_bounds() and self.source[self.current_index] == '=') self.advance();
                return self.make_token(.op, start_index);
            },
            '*' => {
                const start_index = self.current_index;
                self.advance();
                if (self.is_in_bounds() and self.source[self.current_index] == '*') self.advance();
                if (self.is_in_bounds() and self.source[self.current_index] == '=') self.advance();
                return self.make_token(.op, start_index);
            },
            ';' => {
                self.advance();
                return self.make_token(
                    .semicolon,
                    self.current_index - 1,
                );
            },
            '(' => {
                self.advance();
                self.bracket_level += 1;
                return self.make_token(
                    .lparen,
                    self.current_index - 1,
                );
            },
            ')' => {
                self.advance();
                self.bracket_level -|= 1;
                return self.make_token(
                    .rparen,
                    self.current_index - 1,
                );
            },
            '[' => {
                self.advance();
                self.bracket_level += 1;
                return self.make_token(
                    .lbracket,
                    self.current_index - 1,
                );
            },
            ']' => {
                self.advance();
                self.bracket_level -|= 1;
                return self.make_token(
                    .rbracket,
                    self.current_index - 1,
                );
            },
            '{' => {
                self.advance();
                self.bracket_level += 1;
                return self.make_token(
                    .lbrace,
                    self.current_index - 1,
                );
            },
            '}' => {
                self.advance();
                self.bracket_level -|= 1;
                return self.make_token(
                    .rbrace,
                    self.current_index - 1,
                );
            },
            '0'...'9' => {
                if (self.current_index + 2 < self.source.len and
                    std.ascii.eqlIgnoreCase(self.source[self.current_index .. self.current_index + 2], "0b"))
                {
                    return self.binary();
                } else if (self.current_index + 2 < self.source.len and
                    std.ascii.eqlIgnoreCase(self.source[self.current_index .. self.current_index + 2], "0o"))
                {
                    return self.octal();
                } else if (self.current_index + 2 < self.source.len and
                    std.ascii.eqlIgnoreCase(self.source[self.current_index .. self.current_index + 2], "0x"))
                {
                    return self.hexadecimal();
                } else {
                    return self.decimal();
                }
            },
            // TODO: unicode identifier
            'A'...'Z', 'a'...'z', '_' => {
                const start_index = self.current_index;
                while (self.is_in_bounds() and
                    (std.ascii.isAlphanumeric(self.source[self.current_index]) or
                    self.source[self.current_index] == '_')) : (self.advance())
                {}

                return self.make_token(
                    .name,
                    start_index,
                );
            },
            else => return error.NotImplemented,
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

    while (true) {
        const token = try token_iterator.next();
        try token_list.append(token);
        if (token.type == .endmarker) break;
    }
    // for (token_list.items) |token| {
    //     if (token.type != .whitespace) std.debug.print("{s} {any}\n", .{ token.to_byte_slice(source), token });
    // }

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
        .{ .newline, "\n" },
        .{ .endmarker, "" },
    };
    for (token_list.items, expected_tokens) |token, expected| {
        const expected_type, const expected_value = expected;
        try std.testing.expectEqual(expected_type, token.type);
        try std.testing.expectEqualStrings(expected_value, token.to_byte_slice(source));
    }

    // TODO: test tokenizing empty source, pretty sure it fails right now
}
