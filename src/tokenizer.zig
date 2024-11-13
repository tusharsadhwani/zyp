const std = @import("std");

pub const TokenType = enum(u8) {
    whitespace,
    indent,
    dedent,
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

    pub fn to_byte_slice(self: *const Self, source: []const u8) []const u8 {
        // Newline at end of file may not exist in the file
        if (self.type == .newline and self.start_index == source.len and self.end_index == source.len + 1)
            return "\n";

        // Dedents at end of file also may not exist in the file
        if (self.type == .dedent and self.start_index == source.len + 1 and self.end_index == source.len + 1)
            return "";

        // Endmarkers are out of bound too
        if (self.type == .endmarker)
            return "";
        return source[self.start_index..self.end_index];
    }
};

fn is_whitespace(char: u8) bool {
    return char == ' ' or char == '\r' or char == '\t' or char == '\x0b' or char == '\x0c';
}

pub const TokenIterator = struct {
    source: []const u8,
    current_index: u32 = 0,
    prev_index: u32 = 0,
    line_number: u32 = 1,
    prev_line_number: u32 = 1,
    byte_offset: u32 = 0,
    prev_byte_offset: u32 = 0,
    all_whitespace_on_this_line: bool = true,

    bracket_level: u32 = 0,
    prev_token: ?Token = null,

    indent_stack: std.ArrayList([]const u8),
    dedent_counter: u32 = 0,

    // f-string state
    is_parsing_fstring: bool = false,
    fstring_quote: ?[]const u8 = null,

    const Self = @This();

    pub fn init(allocator: std.mem.Allocator, source: []const u8) Self {
        return Self{
            .indent_stack = std.ArrayList([]const u8).init(allocator),
            .source = source,
        };
    }

    pub fn deinit(self: *Self) void {
        self.indent_stack.deinit();
    }

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

    fn next_line(self: *Self) void {
        self.line_number += 1;
        self.byte_offset = 0;
        self.all_whitespace_on_this_line = true;
    }

    fn advance_check_newline(self: *Self) void {
        if (self.source[self.current_index] == '\n') {
            self.current_index += 1;
            self.next_line();
        } else {
            self.advance();
        }
    }

    fn match(self: *Self, options: []const []const u8, config: struct { ignore_case: bool = false }) bool {
        for (options) |option| {
            if (self.current_index + option.len > self.source.len) continue;
            const slice = self.source[self.current_index .. self.current_index + option.len];
            if (config.ignore_case) {
                if (std.ascii.eqlIgnoreCase(option, slice)) return true;
            } else {
                if (std.mem.eql(u8, option, slice)) return true;
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
            self.next_line();
        } else if (tok_type == .whitespace or tok_type == .comment) {} else {
            self.all_whitespace_on_this_line = false;
        }
        self.prev_token = token;
        self.prev_index = self.current_index;
        self.prev_line_number = self.line_number;
        self.prev_byte_offset = self.byte_offset;
        return token;
    }

    fn newline(self: *Self) Token {
        self.advance();
        const in_brackets = self.bracket_level > 0;
        const token_type: TokenType =
            if (in_brackets or self.all_whitespace_on_this_line)
            .nl
        else
            .newline;

        const token = self.make_token(token_type);
        return token;
    }

    fn endmarker(self: *Self) Token {
        if (self.indent_stack.items.len > 0) {
            _ = self.indent_stack.pop();
            return self.make_token(.dedent);
        }
        return self.make_token(.endmarker);
    }

    fn decimal(self: *Self) Token {
        while (self.is_in_bounds() and std.ascii.isDigit(self.source[self.current_index])) self.advance();
        if (self.is_in_bounds() and self.source[self.current_index] == '.') self.advance();
        while (self.is_in_bounds() and std.ascii.isDigit(self.source[self.current_index])) self.advance();
        if (self.is_in_bounds() and (self.source[self.current_index] == 'e' or self.source[self.current_index] == 'E')) {
            self.advance();
            if (self.is_in_bounds() and self.source[self.current_index] == '-') self.advance();
        }
        while (self.is_in_bounds() and std.ascii.isDigit(self.source[self.current_index])) self.advance();
        // If all of this resulted in just a dot, return an operator
        if (self.current_index - self.prev_index == 1 and self.source[self.current_index - 1] == '.') {
            // Ellipsis check
            if (self.current_index + 2 <= self.source.len and
                std.mem.eql(u8, self.source[self.current_index .. self.current_index + 2], ".."))
            {
                self.advance();
                self.advance();
            }
            return self.make_token(.op);
        }

        return self.make_token(.number);
    }

    fn binary(self: *Self) Token {
        // jump over `0b`
        self.advance();
        self.advance();
        while (self.is_in_bounds() and
            self.source[self.current_index] == '0' or self.source[self.current_index] == '1') self.advance();
        if (self.is_in_bounds() and (self.source[self.current_index] == 'e' or self.source[self.current_index] == 'E')) {
            self.advance();
            if (self.is_in_bounds() and self.source[self.current_index] == '-') self.advance();
        }
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
        if (self.is_in_bounds() and (self.source[self.current_index] == 'e' or self.source[self.current_index] == 'E')) {
            self.advance();
            if (self.is_in_bounds() and self.source[self.current_index] == '-') self.advance();
        }
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
        if (self.is_in_bounds() and (self.source[self.current_index] == 'e' or self.source[self.current_index] == 'E')) {
            self.advance();
            if (self.is_in_bounds() and self.source[self.current_index] == '-') self.advance();
        }
        while (self.is_in_bounds() and
            std.ascii.isHex(self.source[self.current_index])) self.advance();
        return self.make_token(.number);
    }

    fn find_opening_quote(self: *Self) usize {
        // Quotes should always be within 3 chars of the beginning of the string token
        for (0..3) |offset| {
            const char = self.source[self.current_index + offset];
            if (char == '"' or char == '\'') return self.current_index + offset;
        }
        @panic("Quote not found somehow");
    }

    fn string_prefix_and_quotes(self: *Self) !struct { []const u8, []const u8 } {
        const quote_index = self.find_opening_quote();
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
        if (!self.is_parsing_fstring) {
            const prefix, const quote = try self.string_prefix_and_quotes();
            self.is_parsing_fstring = true;
            self.fstring_quote = quote;
            for (0..prefix.len) |_| self.advance();
            for (0..quote.len) |_| self.advance();
            return self.make_token(.fstring_start);
        }

        if (self.match(&.{self.fstring_quote.?}, .{})) {
            for (0..self.fstring_quote.?.len) |_| self.advance();
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
            if (self.match(&.{self.fstring_quote.?}, .{})) {
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

    fn indent(self: *Self) !Token {
        const start_index = self.current_index;
        while (self.is_in_bounds() and (self.source[self.current_index] == ' ' or self.source[self.current_index] == '\t')) self.advance();
        if (!self.is_in_bounds()) {
            if (self.current_index == start_index)
                return error.NotAnIndent;
            return self.make_token(.whitespace);
        }
        // For lines that are just leading whitespace and a comment, don't return indents
        if (self.current_index > start_index) {
            const next_char = self.peek();
            if (next_char == '#' or next_char == '\n') return self.make_token(.whitespace);
        }

        const new_indent = self.source[start_index..self.current_index];
        const current_indent = self.indent_stack.getLastOrNull() orelse "";

        if (new_indent.len == current_indent.len) {
            if (new_indent.len == 0) return error.NotAnIndent;

            if (!std.mem.eql(u8, new_indent, current_indent))
                return error.InconsistentUseOfTabsAndSpaces;
            return self.make_token(.whitespace);
        } else if (new_indent.len > current_indent.len) {
            if (current_indent.len > 0 and !std.mem.containsAtLeast(u8, new_indent, 1, current_indent))
                return error.InconsistentUseOfTabsAndSpaces;
            try self.indent_stack.append(new_indent);
            return self.make_token(.indent);
        } else {
            while (self.indent_stack.items.len > 0) {
                const top_indent = self.indent_stack.getLast();
                if (top_indent.len < new_indent.len)
                    return error.DedentDoesNotMatchAnyOuterIndent;

                if (top_indent.len == new_indent.len)
                    break;

                _ = self.indent_stack.pop();
                self.dedent_counter += 1;
            }
            // Let the dedent counter make the dedents. They must be length zero
            return self.make_token(.whitespace);
        }
    }

    fn is_newline(self: *Self) bool {
        if (self.source[self.current_index] == '\n') return true;
        if (self.source[self.current_index] == '\r' and self.current_index + 1 < self.source.len and self.source[self.current_index + 1] == '\n') {
            self.advance();
            return true;
        }
        return false;
    }

    pub fn next(self: *Self) !Token {
        // Always empty the dedent counter first
        if (self.dedent_counter > 0) {
            self.dedent_counter -= 1;
            return self.make_token(.dedent);
        }

        // EOF checks
        if (self.current_index == self.source.len) {
            const prev_token = self.prev_token orelse return self.newline();
            if (prev_token.type == .newline or prev_token.type == .nl or prev_token.type == .dedent) {
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
        // Newline check
        if (self.is_newline()) {
            return self.newline();
        }
        // \<newline> check
        if (current_char == '\\') {
            self.advance();
            if (!self.is_in_bounds()) return error.UnexpectedEOF;
            if (self.is_newline()) {
                self.advance();
                self.next_line();
                return self.make_token(.whitespace);
            }
            return error.UnexpectedCharacterAfterBackslash;
        }

        // Indent / dedent checks
        if (self.byte_offset == 0 and self.bracket_level == 0 and !self.is_parsing_fstring) {
            const indent_token = self.indent() catch |err| switch (err) {
                error.NotAnIndent => null,
                else => return err,
            };
            if (indent_token) |_token| return _token;
        }

        switch (current_char) {
            '#' => {
                while (self.peek() != '\n' and self.peek() != '\r') self.advance();
                return self.make_token(.comment);
            },
            ' ', '\r', '\t', '\x0b', '\x0c' => {
                while (self.is_in_bounds() and is_whitespace(self.source[self.current_index])) self.advance();
                return self.make_token(.whitespace);
            },
            '+', '&', '|', '^', '@', '%', '=', '!', '~' => {
                self.advance();
                if (self.peek() == '=') self.advance();
                return self.make_token(.op);
            },
            '<' => {
                self.advance();
                if (self.peek() == '<') self.advance();
                if (self.peek() == '=') self.advance();
                return self.make_token(.op);
            },
            '>' => {
                self.advance();
                if (self.peek() == '>') self.advance();
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
            '.', '0'...'9' => {
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

const TokenTuple = struct { TokenType, []const u8 };
fn validate_tokens(allocator: std.mem.Allocator, source: []const u8, expected_tokens: []TokenTuple) !void {
    var token_iterator = TokenIterator.init(allocator, source);
    defer token_iterator.deinit();
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
test TokenIterator {
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
    try validate_tokens(std.testing.allocator, source, &expected_tokens);
}

test "blank source" {
    const source = "";
    var expected_tokens = [_]TokenTuple{
        .{ .newline, "\n" },
        .{ .endmarker, "" },
    };
    try validate_tokens(std.testing.allocator, source, &expected_tokens);
}
