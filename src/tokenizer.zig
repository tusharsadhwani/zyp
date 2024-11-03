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
    string,
    fstring_start,
    fstring_middle,
    fstring_end,

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
    start_line: u32,
    // 0-indexed offset from start of line
    start_col: u32,
    end_line: u32,
    end_col: u32,

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
    prev_index: u32 = 0,
    line_number: u32 = 1,
    prev_line_number: u32 = 1,
    byte_offset: u32 = 0,
    prev_byte_offset: u32 = 0,

    bracket_level: u32 = 0,
    prev_token: ?Token = null,

    // f-string state
    is_parsing_fstring: bool = false,

    const Self = @This();

    fn is_in_bounds(self: *Self) bool {
        return self.current_index < self.source.len;
    }

    fn peek(self: *Self) ?u8 {
        if (!self.is_in_bounds()) return null;
        return self.source[self.current_index];
    }

    fn advance(self: *Self) void {
        self.current_index += 1;
        self.byte_offset += 1;
    }

    fn advance_check_newline(self: *Self) void {
        if (self.source[self.current_index] == '\n') {
            self.current_index += 1;
            self.line_number += 1;
            self.byte_offset = 0;
        } else {
            self.advance();
        }
    }

    fn match(self: *Self, options: []const []const u8, config: struct { ignore_case: bool = false }) bool {
        for (options) |option| {
            if (self.current_index + option.len > self.source.len) continue;
            if (config.ignore_case) {
                if (std.ascii.eqlIgnoreCase(option, self.source[self.current_index .. self.current_index + option.len])) return true;
            } else {
                if (std.mem.eql(u8, option, self.source[self.current_index .. self.current_index + option.len])) return true;
            }
        }
        return false;
    }

    fn make_token(self: *Self, tok_type: TokenType) Token {
        const token = Token{
            .type = tok_type,
            .start_index = self.prev_index,
            .end_index = self.current_index,
            .start_line = self.prev_line_number,
            .start_col = self.prev_byte_offset,
            .end_line = self.line_number,
            .end_col = self.byte_offset,
        };
        if (tok_type == .newline or tok_type == .nl) {
            self.line_number += 1;
            self.byte_offset = 0;
        }
        self.prev_token = token;
        self.prev_index = self.current_index;
        self.prev_line_number = self.line_number;
        self.prev_byte_offset = self.byte_offset;
        return token;
    }

    fn newline(self: *Self) Token {
        const prev_token_type = if (self.prev_token) |tok| tok.type else null;
        self.advance();
        const in_brackets = self.bracket_level > 0;
        const token_type: TokenType =
            if (in_brackets or prev_token_type == .newline or prev_token_type == .comment)
            .nl
        else
            .newline;

        const token = self.make_token(token_type);
        return token;
    }

    fn endmarker(self: *Self) Token {
        const token = self.make_token(.endmarker);
        return token;
    }

    fn decimal(self: *Self) Token {
        while (self.is_in_bounds() and std.ascii.isDigit(self.source[self.current_index])) self.advance();
        return self.make_token(.number);
    }

    fn binary(self: *Self) Token {
        // jump over `0b`
        self.advance();
        self.advance();
        while (self.is_in_bounds() and
            self.source[self.current_index] == '0' or self.source[self.current_index] == '1') self.advance();
        return self.make_token(.number);
    }

    fn octal(self: *Self) Token {
        // jump over `0o`
        self.advance();
        self.advance();
        while (self.is_in_bounds() and
            self.source[self.current_index] >= '0' and self.source[self.current_index] <= '7') self.advance();
        return self.make_token(.number);
    }

    fn hexadecimal(self: *Self) Token {
        // jump over `0x`
        self.advance();
        self.advance();
        while (self.is_in_bounds() and
            std.ascii.isHex(self.source[self.current_index])) self.advance();
        return self.make_token(.number);
    }

    fn string_prefix_and_quotes(self: *Self) !struct { []const u8, []const u8 } {
        const rest_source = self.source[self.current_index..];
        const quote_index = self.current_index +
            if (std.mem.indexOf(u8, rest_source, "'")) |idx|
            idx
        else
            std.mem.indexOf(u8, rest_source, "\"") orelse
                @panic("No quote found somehow??");

        const prefix = self.source[self.current_index..quote_index];
        const quote_char = self.source[quote_index];

        // Check for triple quotes
        const quote = if (quote_index + 3 <= self.source.len and
            self.source[quote_index + 1] == quote_char and
            self.source[quote_index + 2] == quote_char)
            self.source[quote_index .. quote_index + 3]
        else
            self.source[quote_index .. quote_index + 1];

        return .{ prefix, quote };
    }

    fn fstring(self: *Self) !Token {
        const prefix, const quote = try self.string_prefix_and_quotes();
        if (!self.is_parsing_fstring) {
            self.is_parsing_fstring = true;
            for (0..prefix.len) |_| self.advance();
            for (0..quote.len) |_| self.advance();
            return self.make_token(.fstring_start);
        }

        if (self.match(&.{quote}, .{})) {
            for (0..quote.len) |_| self.advance();
            self.is_parsing_fstring = false;
            return self.make_token(.fstring_end);
        }

        while (self.is_in_bounds()) {
            const char = self.source[self.current_index];
            // Handle escapes
            if (char == '\\') {
                self.advance();
                self.advance_check_newline();
                continue;
            }

            // Find closing quote
            if (self.match(&.{quote}, .{})) {
                return self.make_token(.fstring_middle);
            }
            self.advance_check_newline();
        }
        return error.UnexpectedEOF;
    }

    fn string(self: *Self) !Token {
        const prefix, const quote = try self.string_prefix_and_quotes();
        for (prefix) |char| if (char == 'f' or char == 'F') return self.fstring();

        for (0..prefix.len) |_| self.advance();
        for (0..quote.len) |_| self.advance();

        while (self.is_in_bounds()) {
            const char = self.source[self.current_index];
            // Handle escapes
            if (char == '\\') {
                self.advance();
                self.advance_check_newline();
                continue;
            }

            // Find closing quote
            if (self.match(&.{quote}, .{})) {
                for (0..quote.len) |_| self.advance();
                return self.make_token(.string);
            }
            self.advance_check_newline();
        }

        return error.UnexpectedEOF;
    }

    pub fn next(self: *Self) !Token {
        if (self.current_index == self.source.len) {
            const prev_token = self.prev_token orelse return self.newline();
            if (prev_token.type == .newline or prev_token.type == .nl) {
                return self.endmarker();
            } else {
                return self.newline();
            }
        }
        if (self.current_index > self.source.len) {
            return self.endmarker();
        }

        if (self.is_parsing_fstring) return self.fstring();

        const current_char = self.source[self.current_index];
        switch (current_char) {
            '\n' => {
                return self.newline();
            },
            '#' => {
                while (self.peek() != '\n') self.advance();
                return self.make_token(.comment);
            },
            // TODO: handle indentation
            ' ', '\r', '\t', '\x0b', '\x0c' => {
                while (self.is_in_bounds() and is_whitespace(self.source[self.current_index])) self.advance();
                return self.make_token(.whitespace);
            },
            '+', '&', '|', '^', '<', '>', '=', '!', '~' => {
                self.advance();
                if (self.peek() == '=') self.advance();
                return self.make_token(.op);
            },
            '/' => {
                self.advance();
                if (self.peek() == '/') self.advance();
                if (self.peek() == '=') self.advance();
                return self.make_token(.op);
            },
            '*' => {
                self.advance();
                if (self.peek() == '*') self.advance();
                if (self.peek() == '=') self.advance();
                return self.make_token(.op);
            },
            '-' => {
                self.advance();
                if (self.peek() == '>') self.advance();
                return self.make_token(.op);
            },
            ',', ':', ';' => {
                self.advance();
                return self.make_token(.op);
            },
            '.' => {
                self.advance();
                if (self.current_index + 2 <= self.source.len and
                    std.ascii.eqlIgnoreCase(self.source[self.current_index .. self.current_index + 2], "0b"))
                {
                    self.advance();
                    self.advance();
                }
                return self.make_token(.op);
            },
            '(' => {
                self.advance();
                self.bracket_level += 1;
                return self.make_token(.lparen);
            },
            ')' => {
                self.advance();
                self.bracket_level -|= 1;
                return self.make_token(.rparen);
            },
            '[' => {
                self.advance();
                self.bracket_level += 1;
                return self.make_token(.lbracket);
            },
            ']' => {
                self.advance();
                self.bracket_level -|= 1;
                return self.make_token(.rbracket);
            },
            '{' => {
                self.advance();
                self.bracket_level += 1;
                return self.make_token(.lbrace);
            },
            '}' => {
                self.advance();
                self.bracket_level -|= 1;
                return self.make_token(.rbrace);
            },
            '0'...'9' => {
                if (self.current_index + 2 <= self.source.len and
                    std.ascii.eqlIgnoreCase(self.source[self.current_index .. self.current_index + 2], "0b"))
                {
                    return self.binary();
                } else if (self.current_index + 2 <= self.source.len and
                    std.ascii.eqlIgnoreCase(self.source[self.current_index .. self.current_index + 2], "0o"))
                {
                    return self.octal();
                } else if (self.current_index + 2 <= self.source.len and
                    std.ascii.eqlIgnoreCase(self.source[self.current_index .. self.current_index + 2], "0x"))
                {
                    return self.hexadecimal();
                } else {
                    return self.decimal();
                }
            },
            // TODO: unicode identifier
            '"', '\'', 'A'...'Z', 'a'...'z', '_' => {
                if ((self.current_index + 1 <= self.source.len and self.match(&.{ "\"", "'" }, .{})) or
                    (self.current_index + 2 <= self.source.len and
                    self.match(&.{ "b\"", "b'", "r\"", "r'", "f\"", "f'", "u\"", "u'" }, .{ .ignore_case = true })) or
                    (self.current_index + 3 <= self.source.len and
                    self.match(&.{ "br\"", "br'", "rb\"", "rb'", "fr\"", "fr'", "rf\"", "rf'" }, .{ .ignore_case = true })))
                    return try self.string();

                while (self.is_in_bounds() and
                    (std.ascii.isAlphanumeric(self.source[self.current_index]) or
                    self.source[self.current_index] == '_')) self.advance();

                return self.make_token(.name);
            },
            else => {
                // std.debug.print("Unsupported token at {d}:{d}: {c}\n", .{
                //     self.line_number,
                //     self.byte_offset,
                //     self.source[self.current_index],
                // });
                return error.NotImplemented;
            },
        }
    }
};

pub fn tokenize(source: []const u8) TokenIterator {
    return TokenIterator{ .source = source };
}

const TokenTuple = struct { TokenType, []const u8 };
fn validate_tokens(source: []const u8, expected_tokens: []TokenTuple) !void {
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
    for (token_list.items, expected_tokens) |token, expected| {
        const expected_type, const expected_value = expected;
        try std.testing.expectEqual(expected_type, token.type);
        try std.testing.expectEqualStrings(expected_value, token.to_byte_slice(source));
    }
}
test tokenize {
    const source: []const u8 =
        \\x = 1
        \\print(x)
    ;
    var expected_tokens = [_]TokenTuple{
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
    try validate_tokens(source, &expected_tokens);
}

test "blank source" {
    const source = "";
    var expected_tokens = [_]TokenTuple{
        .{ .newline, "\n" },
        .{ .endmarker, "" },
    };
    try validate_tokens(source, &expected_tokens);
}
