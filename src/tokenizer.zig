const std = @import("std");
const unicode_id = @import("unicode-id");

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
    colon,
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
        if ((self.type == .newline or self.type == .nl) and self.start_index == source.len and self.end_index == source.len + 1)
            return "";

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

pub const FStringState = struct {
    const State = enum(u8) {
        not_fstring,
        at_fstring_middle,
        at_fstring_lbrace,
        in_fstring_expr,
        in_fstring_expr_modifier,
        at_fstring_end,
    };

    const Self = @This();

    state: State,
    stack: std.ArrayList(State),

    pub fn init(allocator: std.mem.Allocator) Self {
        return Self{
            .state = .not_fstring,
            .stack = std.ArrayList(State).init(allocator),
        };
    }

    pub fn deinit(self: *Self) void {
        // std.debug.assert(self.state == .not_fstring);
        // std.debug.assert(self.stack.items.len == 0);
        self.stack.deinit();
    }

    pub fn enter_fstring(self: *Self) !void {
        try self.stack.append(self.state);
        self.state = .at_fstring_middle;
    }

    pub fn leave_fstring(self: *Self) void {
        std.debug.assert(self.state == .at_fstring_end);
        self.state = self.stack.pop();
    }

    pub fn consume_fstring_middle_for_lbrace(self: *Self) !void {
        if (self.state == .in_fstring_expr_modifier)
            try self.stack.append(self.state);
        self.state = .at_fstring_lbrace;
    }
    pub fn consume_fstring_middle_for_end(self: *Self) void {
        self.state = .at_fstring_end;
    }

    pub fn consume_lbrace(self: *Self) !void {
        self.state = .in_fstring_expr;
    }

    pub fn consume_rbrace(self: *Self) void {
        std.debug.assert(self.state == .in_fstring_expr or self.state == .in_fstring_expr_modifier);

        if (self.stack.items.len > 0 and self.stack.getLast() == .in_fstring_expr_modifier) {
            self.state = self.stack.pop();
        } else {
            self.state = .at_fstring_middle;
        }
    }

    pub fn consume_colon(self: *Self) void {
        std.debug.assert(self.state == .in_fstring_expr);
        self.state = .in_fstring_expr_modifier;
    }
};

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
    bracket_level_stack: std.ArrayList(u32),
    prev_token: ?Token = null,

    indent_stack: std.ArrayList([]const u8),
    dedent_counter: u32 = 0,

    // f-string state
    fstring_state: FStringState,
    fstring_quote_stack: std.ArrayList([]const u8),
    fstring_quote: ?[]const u8 = null,

    const Self = @This();

    pub fn init(allocator: std.mem.Allocator, source: []const u8) Self {
        return Self{
            .source = source,
            .indent_stack = std.ArrayList([]const u8).init(allocator),
            .bracket_level_stack = std.ArrayList(u32).init(allocator),
            .fstring_state = FStringState.init(allocator),
            .fstring_quote_stack = std.ArrayList([]const u8).init(allocator),
        };
    }

    pub fn deinit(self: *Self) void {
        // std.debug.assert(self.indent_stack.items.len == 0);
        self.indent_stack.deinit();
        // std.debug.assert(self.bracket_level_stack.items.len == 0);
        self.bracket_level_stack.deinit();
        self.fstring_state.deinit();
        // std.debug.assert(self.fstring_quote == null);
        self.fstring_quote_stack.deinit();
    }

    fn is_in_bounds(self: *Self) bool {
        return self.current_index < self.source.len;
    }

    fn peek(self: *Self) u8 {
        if (!self.is_in_bounds()) @panic("peeked out of bounds");
        return self.source[self.current_index];
    }

    fn peek_next(self: *Self) u8 {
        if (self.current_index + 1 >= self.source.len) @panic("peeked out of bounds");
        return self.source[self.current_index + 1];
    }

    fn advance(self: *Self) void {
        self.current_index += 1;
        self.byte_offset += 1;
    }

    fn advance_by(self: *Self, count: u32) void {
        self.current_index += count;
        self.byte_offset += count;
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

    fn push_fstring_quote(self: *Self, quote: []const u8) !void {
        if (self.fstring_quote) |fstring_quote| {
            try self.fstring_quote_stack.append(fstring_quote);
        }
        self.fstring_quote = quote;
    }

    fn pop_fstring_quote(self: *Self) !void {
        if (self.fstring_quote == null)
            return error.Underflow;
        self.fstring_quote = self.fstring_quote_stack.popOrNull();
    }

    fn newline(self: *Self) Token {
        self.advance();
        const in_brackets = self.bracket_level > 0;
        const token_type: TokenType =
            if (in_brackets or self.fstring_state.state == .in_fstring_expr or self.all_whitespace_on_this_line)
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
        var digit_before_decimal = false;
        if (std.ascii.isDigit(self.source[self.current_index])) {
            digit_before_decimal = true;
            self.advance();
        }

        // TODO: this is too lax; 1__2 tokenizes successfully
        while (self.is_in_bounds() and (std.ascii.isDigit(self.source[self.current_index]) or self.source[self.current_index] == '_'))
            self.advance();

        if (self.is_in_bounds() and self.source[self.current_index] == '.')
            self.advance();

        while (self.is_in_bounds() and (std.ascii.isDigit(self.source[self.current_index]) or (self.source[self.current_index] == '_' and std.ascii.isDigit(self.source[self.current_index - 1]))))
            self.advance();
        // Before advancing over the 'e', ensure that there has been at least 1 digit before the 'e'
        if (self.current_index + 1 < self.source.len and ((digit_before_decimal or std.ascii.isDigit(self.source[self.current_index - 1])) and (self.source[self.current_index] == 'e' or self.source[self.current_index] == 'E') and (std.ascii.isDigit(self.source[self.current_index + 1]) or (self.current_index + 2 < self.source.len and (self.source[self.current_index + 1] == '+' or self.source[self.current_index + 1] == '-') and std.ascii.isDigit(self.source[self.current_index + 2]))))) {
            self.advance();
            self.advance();
            // optional third advance not necessary as itll get advanced just below
        }
        // TODO: this is too lax; 1__2 tokenizes successfully
        while (self.is_in_bounds() and (std.ascii.isDigit(self.source[self.current_index]) or ((digit_before_decimal or std.ascii.isDigit(self.source[self.current_index - 1])) and self.source[self.current_index] == '_')))
            self.advance();

        // Complex numbers end in a `j`. But ensure at least 1 digit before it
        if (self.is_in_bounds() and ((digit_before_decimal or std.ascii.isDigit(self.source[self.current_index - 1])) and (self.source[self.current_index] == 'j' or self.source[self.current_index] == 'J')))
            self.advance();

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
        switch (self.fstring_state.state) {
            .not_fstring, .in_fstring_expr => {
                const prefix, const quote = try self.string_prefix_and_quotes();
                try self.push_fstring_quote(quote);
                for (0..prefix.len) |_| self.advance();
                for (0..quote.len) |_| self.advance();
                try self.fstring_state.enter_fstring();
                return self.make_token(.fstring_start);
            },
            .at_fstring_middle => {
                const is_single_quote = self.fstring_quote.?.len == 1;
                const start_index = self.current_index;
                while (self.is_in_bounds()) {
                    const char = self.peek();
                    // For single quotes, bail on newlines
                    if (char == '\n' and is_single_quote) return error.UnterminatedString;

                    // Handle escapes
                    if (char == '\\') {
                        self.advance();
                        // But don't escape a `\{` or `\}` in f-strings
                        // but DO escape `\N{` in f-strings, that's for unicode characters
                        if (self.current_index + 1 < self.source.len and
                            self.peek() == 'N' and self.peek_next() == '{')
                        {
                            self.advance();
                            self.advance();
                        }
                        if (self.is_in_bounds() and !(self.peek() == '{' or self.peek() == '}')) {
                            self.advance_check_newline();
                        }
                        continue;
                    }

                    // Find opening / closing quote
                    if (char == '{') {
                        if (self.current_index + 1 < self.source.len and self.peek_next() == '{') {
                            self.advance();
                            self.advance();
                            continue;
                        } else {
                            try self.fstring_state.consume_fstring_middle_for_lbrace();
                            // If fstring-middle is empty, skip it by returning the next step token
                            if (self.current_index == start_index) {
                                return self.fstring();
                            }
                            return self.make_token(.fstring_middle);
                        }
                    }
                    if (self.match(&.{self.fstring_quote.?}, .{})) {
                        self.fstring_state.consume_fstring_middle_for_end();
                        // If fstring-middle is empty, skip it by returning the next step token
                        if (self.current_index == start_index) {
                            return self.fstring();
                        }
                        return self.make_token(.fstring_middle);
                    }
                    self.advance_check_newline();
                }
                return error.UnexpectedEOF;
            },
            .at_fstring_lbrace => {
                self.advance();
                try self.bracket_level_stack.append(self.bracket_level);
                self.bracket_level = 0;
                try self.fstring_state.consume_lbrace();
                return self.make_token(.lbrace);
            },
            .at_fstring_end => {
                for (0..self.fstring_quote.?.len) |_| self.advance();
                try self.pop_fstring_quote();
                self.fstring_state.leave_fstring();
                return self.make_token(.fstring_end);
            },
            .in_fstring_expr_modifier => {
                const start_index = self.current_index;
                while (self.is_in_bounds()) {
                    const char = self.source[self.current_index];
                    if ((char == '\n' or char == '{') and self.fstring_quote.?.len == 1) {
                        if (char == '{') {
                            try self.fstring_state.consume_fstring_middle_for_lbrace();
                        } else {
                            // TODO: why?
                            self.fstring_state.state = .in_fstring_expr;
                        }
                        // If fstring-middle is empty, skip it by returning the next step token
                        if (self.current_index == start_index) {
                            return self.fstring();
                        }
                        return self.make_token(.fstring_middle);
                    } else if (char == '}') {
                        self.fstring_state.state = .in_fstring_expr;
                        return self.make_token(.fstring_middle);
                    }
                    self.advance_check_newline();
                }
                return error.UnexpectedEOF;
            },
        }
    }

    fn string(self: *Self) !Token {
        const prefix, const quote = try self.string_prefix_and_quotes();
        for (prefix) |char| if (char == 'f' or char == 'F') return self.fstring();

        for (0..prefix.len) |_| self.advance();
        for (0..quote.len) |_| self.advance();

        const is_single_quote = quote.len == 1;

        while (self.is_in_bounds()) {
            const char = self.source[self.current_index];
            // For single quotes, bail on newlines
            if (char == '\n' and is_single_quote) return error.UnterminatedString;

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
        var start_index = self.current_index;
        var saw_whitespace = false;
        var saw_tab_or_space = false;
        while (self.is_in_bounds()) {
            const char = self.source[self.current_index];
            if (is_whitespace(char)) {
                self.advance();
                saw_whitespace = true;
                if (char == ' ' or char == '\t') saw_tab_or_space = true;
            } else break;
        }
        if (!self.is_in_bounds()) {
            // File ends with no whitespace after newline, don't return indent
            if (self.current_index == start_index)
                return error.NotAnIndent;
            // If reached the end of the file, don't return an indent
            return self.make_token(.whitespace);
        }
        // If the line is preceded by just linefeeds/CR/etc.,
        // ignore that leading whitespace entirely.
        if (saw_whitespace and !saw_tab_or_space)
            start_index = self.current_index;

        // For lines that are just leading whitespace and a slash or a comment,
        // don't return indents
        const next_char = self.peek();
        if (next_char == '#' or next_char == '\\' or next_char == '\n') return self.make_token(.whitespace);

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

    fn name(self: *Self) !Token {
        const view = std.unicode.Utf8View.initUnchecked(self.source[self.current_index..]);
        var iter = view.iterator();

        const first_code_point = iter.nextCodepoint() orelse return error.UnexpectedEOF;
        if (!unicode_id.canStartId(first_code_point)) return error.UnexpectedCharacter;

        var len = iter.i;
        while (iter.nextCodepoint()) |codepoint| {
            // If this doesn't match, we want to return the length until the
            // previous codepoint
            if (!unicode_id.canContinueId(codepoint)) {
                break;
            }
            len = iter.i;
        }

        self.advance_by(@intCast(len));
        return self.make_token(.name);
    }

    pub fn next(self: *Self) !Token {
        // EOF checks
        if (self.current_index == self.source.len) {
            const prev_token = self.prev_token orelse return self.endmarker();
            if (prev_token.type == .newline or prev_token.type == .nl or prev_token.type == .dedent) {
                return self.endmarker();
            } else {
                return self.newline();
            }
        }
        if (self.current_index > self.source.len) {
            return self.endmarker();
        }

        // f-string check
        if (self.fstring_state.state != .not_fstring and self.fstring_state.state != .in_fstring_expr)
            return self.fstring();

        const current_char = self.source[self.current_index];

        // Comment check
        if (current_char == '#') {
            while (self.is_in_bounds() and self.peek() != '\n' and self.peek() != '\r') self.advance();
            return self.make_token(.comment);
        }

        // Empty the dedent counter
        if (self.dedent_counter > 0) {
            self.dedent_counter -= 1;
            return self.make_token(.dedent);
        }

        // Newline check
        if (self.is_newline()) {
            return self.newline();
        }
        // \<newline> check
        if (current_char == '\\') {
            self.advance();
            if (!self.is_in_bounds()) return error.UnexpectedEOF;

            // Consume all whitespace on this line and the next.
            var found_whitespace = false;
            while (self.is_in_bounds()) {
                const char = self.source[self.current_index];
                if (is_whitespace(char)) {
                    self.advance();
                    found_whitespace = true;
                } else if (char == '\n') {
                    self.advance();
                    found_whitespace = true;
                    // Move to next line without creating a newline token. But,
                    // if the previous line was all whitespace, whitespace on
                    // the next line is still valid indentation. Avoid consuming
                    if (self.all_whitespace_on_this_line) {
                        self.next_line();
                        break;
                    } else {
                        self.next_line();
                        // Preserve this boolean, we're on the same line semantically
                        self.all_whitespace_on_this_line = false;
                    }
                } else break;
            }
            if (!found_whitespace) {
                return error.UnexpectedCharacterAfterBackslash;
            }
            return self.make_token(.whitespace);
        }

        // Indent / dedent checks
        if (self.byte_offset == 0 and self.bracket_level == 0 and self.fstring_state.state == .not_fstring) {
            const indent_token = self.indent() catch |err| switch (err) {
                error.NotAnIndent => null,
                else => return err,
            };
            if (indent_token) |_token| return _token;
        }

        switch (current_char) {
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
                if (self.peek() == '>') {
                    // Barry as FLUFL easter egg
                    self.advance();
                    return self.make_token(.op);
                }
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
                // -> operator
                if (self.peek() == '>') {
                    self.advance();
                    return self.make_token(.op);
                }
                // -= operator
                if (self.peek() == '=') self.advance();
                return self.make_token(.op);
            },
            ',', ';' => {
                self.advance();
                return self.make_token(.op);
            },
            // This guy is not used in Python3, but still exists
            // for backwards compatibility i guess.
            '`' => {
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
                if (self.bracket_level == 0 and self.fstring_state.state == .in_fstring_expr) {
                    self.fstring_state.consume_rbrace();
                    self.bracket_level = self.bracket_level_stack.pop();
                } else {
                    self.bracket_level -|= 1;
                }
                return self.make_token(.rbrace);
            },
            ':' => {
                self.advance();
                if (self.bracket_level == 0 and self.fstring_state.state == .in_fstring_expr) {
                    self.fstring_state.state = .in_fstring_expr_modifier;
                    return self.make_token(.op);
                } else {
                    if (self.peek() == '=') self.advance();
                    return self.make_token(.op);
                }
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
            else => {
                if ((self.current_index + 1 <= self.source.len and self.match(&.{ "\"", "'" }, .{})) or
                    (self.current_index + 2 <= self.source.len and
                    self.match(&.{ "b\"", "b'", "r\"", "r'", "f\"", "f'", "u\"", "u'" }, .{ .ignore_case = true })) or
                    (self.current_index + 3 <= self.source.len and
                    self.match(&.{ "br\"", "br'", "rb\"", "rb'", "fr\"", "fr'", "rf\"", "rf'" }, .{ .ignore_case = true })))
                    return try self.string();

                return try self.name();
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
        \\
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

    // To ensure the tokenizer doesn't think `.e` is a number
    const dot_e_source =
        \\x = []
        \\x.extend([1])
    ;
    var dot_e_expected_tokens = [_]TokenTuple{
        .{ .name, "x" },
        .{ .whitespace, " " },
        .{ .op, "=" },
        .{ .whitespace, " " },
        .{ .lbracket, "[" },
        .{ .rbracket, "]" },
        .{ .newline, "\n" },
        .{ .name, "x" },
        .{ .op, "." },
        .{ .name, "extend" },
        .{ .lparen, "(" },
        .{ .lbracket, "[" },
        .{ .number, "1" },
        .{ .rbracket, "]" },
        .{ .rparen, ")" },
        .{ .newline, "" },
        .{ .endmarker, "" },
    };
    try validate_tokens(std.testing.allocator, dot_e_source, &dot_e_expected_tokens);

    // To ensure the tokenizer doesn't think `.j` is a complex number
    const dot_j_source =
        \\''.join([1, 2])
    ;
    var dot_j_expected_tokens = [_]TokenTuple{
        .{ .string, "''" },
        .{ .op, "." },
        .{ .name, "join" },
        .{ .lparen, "(" },
        .{ .lbracket, "[" },
        .{ .number, "1" },
        .{ .op, "," },
        .{ .whitespace, " " },
        .{ .number, "2" },
        .{ .rbracket, "]" },
        .{ .rparen, ")" },
        .{ .newline, "" },
        .{ .endmarker, "" },
    };
    try validate_tokens(std.testing.allocator, dot_j_source, &dot_j_expected_tokens);
}

test "blank source" {
    const source = "";
    var expected_tokens = [_]TokenTuple{
        .{ .endmarker, "" },
    };
    try validate_tokens(std.testing.allocator, source, &expected_tokens);

    const with_newline = "\n";
    var with_newline_tokens = [_]TokenTuple{
        .{ .nl, "\n" },
        .{ .endmarker, "" },
    };
    try validate_tokens(std.testing.allocator, with_newline, &with_newline_tokens);
}

test "source with no newline at the end" {
    const source = "x = 1";
    var expected_tokens = [_]TokenTuple{
        .{ .name, "x" },
        .{ .whitespace, " " },
        .{ .op, "=" },
        .{ .whitespace, " " },
        .{ .number, "1" },
        .{ .newline, "" },
        .{ .endmarker, "" },
    };
    try validate_tokens(std.testing.allocator, source, &expected_tokens);
}
