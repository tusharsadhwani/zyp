const std = @import("std");

pub const Module = struct { body: []const Statement };

pub const Statement = union(enum) {
    // TODO: do this instead
    // expr_stmt: *struct {
    //     value: Expression,
    // },
    expr_stmt: struct {
        value: *Expression,
    },
};

pub const Expression = union(enum) {
    // TODO: store a Span type instead, which will be the start and end index
    // Also change the tokens to return a Span too.
    name: []const u8,
    // TODO: do this instead
    // call: *struct {
    //     value: Expression,
    //     arguments: []const Expression,
    // },
    call: struct {
        value: *Expression,
        arguments: []const *Expression,
    },
};
