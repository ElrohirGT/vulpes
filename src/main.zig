const std = @import("std");

const TokenType = enum {
    FuncArgument,
    IntLiteral,
    Assignment,
    TopLevelName,
};

const Token = struct {
    type: TokenType,
    value: ?[]const u8 = null,

    pub fn deinit(s: Token, alloc: std.mem.Allocator) void {
        if (s.value) |innerStr| {
            alloc.free(innerStr);
        }
    }
};

const State = enum {
    None,
    TopLevelDeclaration,
    DeclarationBody,
};

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    const alloc = gpa.allocator();

    var args = std.process.argsWithAllocator(alloc) catch unreachable;
    defer args.deinit();

    _ = args.next();
    const filePath = args.next() orelse unreachable;

    var file = std.fs.cwd().openFile(filePath, .{}) catch unreachable;
    defer file.close();

    _ = try parse(alloc, file.reader().any());
}

fn parse(alloc: std.mem.Allocator, reader: std.io.AnyReader) !std.ArrayList(Token) {
    var char_buffer: [1]u8 = undefined;
    var parser_state = State.None;
    var tokens = std.ArrayList(Token).init(alloc);

    while (try reader.read(&char_buffer) != 0) {
        const char = char_buffer[0];
        std.log.debug("Consumed: `{c}`. On State: {}", .{ char, parser_state });
        std.log.debug("Tokens: {any}", .{tokens.items});

        switch (parser_state) {
            .None => {
                switch (char) {
                    ' ', '\t', '\r', '\n' => continue,
                    '0'...'9' => return error.ExpectedTopLevelDeclarationButGotNumber,
                    else => {
                        var topLevelNameBuffer = std.ArrayList(u8).init(alloc);
                        defer topLevelNameBuffer.deinit();
                        try topLevelNameBuffer.append(char);

                        try reader.streamUntilDelimiter(topLevelNameBuffer.writer(), ' ', null);
                        const topLevelName = try topLevelNameBuffer.toOwnedSlice();

                        try tokens.append(.{ .value = topLevelName, .type = .TopLevelName });
                        parser_state = .TopLevelDeclaration;
                    },
                }
            },
            .TopLevelDeclaration => {
                switch (char) {
                    ' ', '\t', '\r', '\n' => continue,
                    '0'...'9' => return error.ExpectedArgumentOrEqualsButGotNumber,
                    '=' => {
                        try tokens.append(.{ .type = .Assignment });
                        parser_state = .DeclarationBody;
                    },
                    else => {
                        var argNameBuffer = std.ArrayList(u8).init(alloc);
                        defer argNameBuffer.deinit();
                        try argNameBuffer.append(char);

                        try reader.streamUntilDelimiter(argNameBuffer.writer(), ' ', null);
                        const argName = try argNameBuffer.toOwnedSlice();

                        try tokens.append(.{ .value = argName, .type = .FuncArgument });
                        // parser_state = .TopLevelDeclaration;
                    },
                }
            },
            .DeclarationBody => {
                switch (char) {
                    ' ', '\t', '\r', '\n' => continue,
                    '0'...'9' => {
                        var buffer = std.ArrayList(u8).init(alloc);
                        defer buffer.deinit();
                        try buffer.append(char);

                        reader.streamUntilDelimiter(buffer.writer(), ' ', null) catch |err| switch (err) {
                            error.EndOfStream => {},
                            else => {
                                return err;
                            },
                        };
                        const argName = try buffer.toOwnedSlice();

                        try tokens.append(.{ .value = argName, .type = .IntLiteral });
                    },
                    else => unreachable,
                }
            },
        }
    }

    return tokens;
}

test "Can tokenize simple body" {
    const alloc = std.testing.allocator;
    const fileContents =
        \\ main env flags =
        \\ 5
    ;
    var stream = std.io.fixedBufferStream(fileContents);

    var expectedTokens = try std.ArrayList(Token).initCapacity(alloc, 5);
    defer expectedTokens.deinit();

    try expectedTokens.append(.{ .type = .TopLevelName, .value = "main" });
    try expectedTokens.append(.{ .type = .FuncArgument, .value = "env" });
    try expectedTokens.append(.{ .type = .FuncArgument, .value = "flags" });
    try expectedTokens.append(.{ .type = .Assignment });
    try expectedTokens.append(.{ .type = .IntLiteral, .value = "5" });

    var tokens = try parse(alloc, stream.reader().any());
    defer tokens.deinit();
    defer for (tokens.items) |value| {
        value.deinit(alloc);
    };

    for (tokens.items, 0..) |token, i| {
        const expected = expectedTokens.items[i];
        try std.testing.expectEqual(expected.type, token.type);
        if (expected.value != null and token.value != null) {
            try std.testing.expectEqualStrings(expected.value.?, token.value.?);
        } else {
            try std.testing.expectEqual(expected.value, token.value);
        }
    }
}

test "simple test" {
    var list = std.ArrayList(i32).init(std.testing.allocator);
    defer list.deinit(); // try commenting this out and see if zig detects the memory leak!
    try list.append(42);
    try std.testing.expectEqual(@as(i32, 42), list.pop());
}
