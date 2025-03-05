const std = @import("std");

const tokenizer = @import("tokenizer.zig");
const Token = tokenizer.Token;
const TokenType = tokenizer.TokenType;
const TokenIterator = tokenizer.TokenIterator;
const node = @import("nodes.zig");

pub const ParseResult = struct {
    arena: *std.heap.ArenaAllocator,
    tree: *node.Module,

    pub fn deinit(self: @This()) void {
        const allocator = self.arena.child_allocator;
        self.arena.deinit();
        allocator.destroy(self.arena);
    }
};

fn new(al: std.mem.Allocator, obj: anytype) !*@TypeOf(obj) {
    const heap_obj = try al.create(@TypeOf(obj));
    heap_obj.* = obj;
    return heap_obj;
}

pub const Parser = struct {
    al: std.mem.Allocator,

    source: []const u8,
    token_iterator: *TokenIterator,
    current_token: Token,

    const Self = @This();

    pub fn init(al: std.mem.Allocator, source: []const u8, token_iterator: *TokenIterator) !ParseResult {
        var arena = try al.create(std.heap.ArenaAllocator);
        arena.* = std.heap.ArenaAllocator.init(al);

        const first_token = token_iterator.next() catch unreachable;
        const arena_al = arena.allocator();
        var self = Self{
            .al = arena_al,
            .source = source,
            .token_iterator = token_iterator,
            .current_token = first_token,
        };
        const module = try self.parse();
        return ParseResult{ .arena = arena, .tree = module };
    }

    fn advance(self: *Self) !void {
        self.current_token = try self.token_iterator.next();
    }

    fn peek(self: *Self) Token {
        return self.current_token;
    }

    fn next(self: *Self) !Token {
        const current_token = self.current_token;
        try self.advance();
        return current_token;
    }

    fn consume(self: *Self, token_type: TokenType) !void {
        if (self.peek().type != token_type) return error.UnexpectedToken;
        try self.advance();
    }

    pub fn parse(self: *Self) !*node.Module {
        var tree = try self.al.create(node.Module);

        var body = std.ArrayList(node.Statement).init(self.al);
        while (true) {
            const token = self.peek();
            if (token.type == .endmarker) break;
            if (token.type == .newline or token.type == .nl) {
                try self.advance();
                continue;
            }
            try body.append(try self.parse_statement());
        }

        tree.body = try body.toOwnedSlice();
        return tree;
    }

    fn parse_statement(self: *Self) !node.Statement {
        switch (self.peek().type) {
            // TODO: if, for, etc. statements
            else => return try self.parse_assign_or_expr_stmt(),
        }
    }

    fn parse_assign_or_expr_stmt(self: *Self) !node.Statement {
        // TODO: assign statements
        // TODO: logical, operator statements etc.

        return node.Statement{
            .expr_stmt = .{
                .value = try self.parse_call(),
            },
        };
    }

    fn parse_call(self: *Self) !node.Expression {
        const value = try self.parse_expression();
        if (self.peek().type != .lparen)
            return value;

        try self.advance();
        // Parse arguments
        // Edge case: no args
        if (self.peek().type == .rparen) {
            return node.Expression{ .call = try new(self.al, node.Call{ .value = value, .arguments = &.{} }) };
        }

        var args = std.ArrayList(node.Expression).init(self.al);
        while (true) {
            const arg = try self.parse_expression();
            try args.append(arg);
            if (self.peek().type == .comma) {
                try self.advance();
                if (self.peek().type == .rparen) {
                    // Trailing comma case
                    try self.advance();
                    break;
                }
            } else {
                try self.consume(.rparen);
                break;
            }
        }

        return node.Expression{ .call = try new(self.al, node.Call{
            .value = value,
            .arguments = try args.toOwnedSlice(),
        }) };
    }

    fn parse_expression(self: *Self) !node.Expression {
        const token = try self.next();
        switch (token.type) {
            .name => {
                return node.Expression{ .name = token.to_byte_slice(self.source) };
            },
            // .string => return node.Expression{ .constant = .{ .value = self.parse_string(token) } },
            else => {
                if (token.type.is_number()) {
                    return node.Expression{ .constant = try self.parse_number(token) };
                }
                return error.NotImplemented;
            },
        }
    }

    fn parse_number(self: *Self, token: Token) !node.Constant { // TODO: remove '!'
        switch (token.type) {
            .decimal => return node.Constant{ .integer = std.fmt.parseInt(u64, token.to_byte_slice(self.source), 10) catch unreachable },
            .binary => return node.Constant{ .integer = std.fmt.parseInt(u64, token.to_byte_slice(self.source), 2) catch unreachable },
            .octal => return node.Constant{ .integer = std.fmt.parseInt(u64, token.to_byte_slice(self.source), 8) catch unreachable },
            .hexadecimal => return node.Constant{ .integer = std.fmt.parseInt(u64, token.to_byte_slice(self.source), 16) catch unreachable },
            .float => return node.Constant{ .float = std.fmt.parseFloat(f64, token.to_byte_slice(self.source)) catch unreachable },
            else => return error.NotImplemented, // TODO: complex
        }
    }
};

test Parser {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();

    const al = arena.allocator();

    const source =
        \\print(x)
        \\print(42)
    ;

    var token_iterator = TokenIterator.init(al, source);
    const result = try Parser.init(al, source, &token_iterator);
    defer result.deinit();
    try std.testing.expectEqualDeep(node.Module{
        .body = &.{
            node.Statement{
                .expr_stmt = .{
                    .value = node.Expression{
                        .call = try new(al, node.Call{
                            .value = node.Expression{ .name = "print" },
                            .arguments = &.{
                                node.Expression{ .name = "x" },
                            },
                        }),
                    },
                },
            },
            node.Statement{
                .expr_stmt = .{
                    .value = node.Expression{
                        .call = try new(al, node.Call{
                            .value = node.Expression{ .name = "print" },
                            .arguments = &.{
                                node.Expression{ .constant = node.Constant{ .integer = 42 } },
                            },
                        }),
                    },
                },
            },
        },
    }, result.tree.*);
}
