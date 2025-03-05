const std = @import("std");

pub const Module = struct { body: []const Statement };

pub const Statement = union(enum) {
    expr_stmt: struct {
        value: Expression,
    },
};

pub const Expression = union(enum) {
    // TODO: store a Span type instead, which will be the start and end index
    // Also change the tokens to return a Span too.
    // TODO: make Spans such that we can make the AST lossless, add trivia nodes
    // if needed.
    name: []const u8,
    constant: Constant,
    call: *Call,
};

pub const Constant = union(enum) {
    integer: u64, // TODO: python has bigints
    float: f64,
    string: []const u8,
};

pub const Call = struct {
    value: Expression,
    arguments: []const Expression,
};
