const std = @import("std");

const tokenizer = @import("tokenizer.zig");
const Token = tokenizer.Token;
const TokenType = tokenizer.TokenType;
const TokenIterator = tokenizer.TokenIterator;
const node = @import("nodes.zig");

fn new(al: std.mem.Allocator, obj: anytype) !*@TypeOf(obj) {
    const heap_obj = try al.create(@TypeOf(obj));
    heap_obj.* = obj;
    return heap_obj;
}

pub const Parser = struct {
    arena: *std.heap.ArenaAllocator,
    al: std.mem.Allocator,

    source: []const u8,
    token_iterator: *TokenIterator,
    current_token: Token,

    const Self = @This();

    pub fn init(al: std.mem.Allocator, source: []const u8, token_iterator: *TokenIterator) std.mem.Allocator.Error!Self {
        var arena = try al.create(std.heap.ArenaAllocator);
        // TODO: don't drop the arena when the parser deinits, put the arena
        // on the ParseResult that we return from Parser.parse().
        arena.* = std.heap.ArenaAllocator.init(al);

        const first_token = token_iterator.next() catch unreachable;
        const arena_al = arena.allocator();
        const self = Self{
            .arena = arena,
            .al = arena_al,
            .source = source,
            .token_iterator = token_iterator,
            .current_token = first_token,
        };
        return self;
    }

    fn deinit(self: *Self) void {
        self.arena.deinit();
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

    fn parse_call(self: *Self) !*node.Expression {
        const value = try self.parse_expression();
        if (self.peek().type != .lparen)
            return value;

        try self.advance();
        // Parse arguments
        // Edge case: no args
        if (self.peek().type == .rparen) {
            return try new(self.al, node.Expression{ .call = .{ .value = value, .arguments = &.{} } });
        }

        var args = std.ArrayList(*node.Expression).init(self.al);
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
        return try new(self.al, node.Expression{ .call = .{
            .value = value,
            .arguments = try args.toOwnedSlice(),
        } });
    }

    fn parse_expression(self: *Self) !*node.Expression {
        const token = try self.next();
        switch (token.type) {
            .name => {
                return try new(self.al, node.Expression{ .name = token.to_byte_slice(self.source) });
            },
            else => return error.NotImplemented,
        }
    }
};

pub fn parse(al: std.mem.Allocator, source: []const u8, token_iterator: *TokenIterator) !*node.Module {
    var parser = try Parser.init(al, source, token_iterator);
    // defer parser.deinit();

    const tree = try parser.parse();
    return tree;
}

test parse {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();

    const al = arena.allocator();

    const source =
        \\print(x)
    ;

    var token_iterator = TokenIterator.init(al, source);
    const tree = try parse(al, source, &token_iterator);
    try std.testing.expectEqualDeep(node.Module{
        .body = &.{
            node.Statement{
                .expr_stmt = .{
                    .value = try new(al, node.Expression{
                        .call = .{
                            .value = try new(al, node.Expression{
                                .name = "print",
                            }),
                            .arguments = &.{
                                try new(al, node.Expression{
                                    .name = "x",
                                }),
                            },
                        },
                    }),
                },
            },
        },
    }, tree.*);
}
