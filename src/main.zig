const std = @import("std");

const TokenType = enum {
    FuncArgument,
    IntLiteral,
    Assignment,
    TopLevelName,
    Variable,

    // Operation tokens
    SumOperator,
    MinusOperator,
    MultOperator,
    DivisionOperator,
};

const Token = struct {
    type: TokenType,
    value: ?[]const u8 = null,

    pub fn deinit(s: Token, alloc: std.mem.Allocator) void {
        if (s.value) |innerStr| {
            alloc.free(innerStr);
        }
    }

    pub fn toString(s: Token, alloc: std.mem.Allocator) ![]const u8 {
        var buffer = std.ArrayList(u8).init(alloc);
        defer buffer.deinit();

        try std.fmt.format(
            buffer.writer(),
            "[type: {}, value: {?s}]",
            .{ s.type, s.value },
        );
        return try buffer.toOwnedSlice();
    }

    pub fn arrayToString(s: std.ArrayList(Token), alloc: std.mem.Allocator) ![]const u8 {
        var buffer = std.ArrayList(u8).init(alloc);
        defer buffer.deinit();

        try std.fmt.format(buffer.writer(), "[", .{});
        for (s.items) |value| {
            const val = try value.toString(alloc);
            defer alloc.free(val);

            try std.fmt.format(buffer.writer(), "{s}\n", .{val});
        }
        try std.fmt.format(buffer.writer(), "]", .{});

        return try buffer.toOwnedSlice();
    }
};

/// Represents a state of the parser.
/// Normally indicates what the parser will search for!
const State = enum {
    None,
    TopLevelDeclaration,
    DeclarationBody,
    DeclarationBodyEnd1,
    DeclarationBodyEnd2,
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

    _ = try tokenize(alloc, file.reader().any());
}

fn tokenize(alloc: std.mem.Allocator, reader: std.io.AnyReader) !std.ArrayList(Token) {
    var char_buffer: [1]u8 = undefined;
    var parser_state = State.None;
    var tokens = std.ArrayList(Token).init(alloc);

    while (try reader.read(&char_buffer) != 0) {
        const char = char_buffer[0];
        std.log.debug("Consumed: `{c}`. On State: {}", .{ char, parser_state });

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
            .DeclarationBodyEnd1 => {
                switch (char) {
                    ' ', '\t', '\r' => continue,
                    '\n' => {
                        parser_state = .DeclarationBodyEnd2;
                        continue;
                    },
                    else => {
                        try declaration_body(alloc, &reader, char, &parser_state, &tokens);
                    },
                }
            },
            .DeclarationBodyEnd2 => {
                switch (char) {
                    ' ', '\t', '\r' => continue,
                    '\n' => {
                        parser_state = .None;
                    },
                    else => {
                        try declaration_body(alloc, &reader, char, &parser_state, &tokens);
                    },
                }
            },
            .DeclarationBody => {
                try declaration_body(alloc, &reader, char, &parser_state, &tokens);
            },
        }

        const tokens_str = try Token.arrayToString(tokens, alloc);
        defer alloc.free(tokens_str);

        std.log.debug("AFTER STATE: {}", .{parser_state});
        std.log.debug("Tokens: {s}\n", .{tokens_str});
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

    var tokens = try tokenize(alloc, stream.reader().any());
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

fn declaration_body(
    alloc: std.mem.Allocator,
    reader: *const std.io.AnyReader,
    char: u8,
    parser_state: *State,
    tokens: *std.ArrayList(Token),
) !void {
    switch (char) {
        ' ', '\t', '\r' => {},
        '\n' => {
            parser_state.* = .DeclarationBodyEnd1;
            return;
        },
        '0'...'9' => {
            const value: []const u8 = get_value: {
                var buffer = std.ArrayList(u8).init(alloc);
                defer buffer.deinit();
                try buffer.append(char);

                while (true) {
                    const byte = reader.readByte() catch |err| switch (err) {
                        error.EndOfStream => break,
                        else => return err,
                    };
                    switch (byte) {
                        ' ', '\t', '\r' => break,
                        '\n' => {
                            parser_state.* = .DeclarationBodyEnd1;
                            break;
                        },
                        else => try buffer.append(byte),
                    }
                }

                break :get_value try buffer.toOwnedSlice();
            };

            try tokens.append(.{ .value = value, .type = .IntLiteral });
        },
        else => {
            const value: []const u8 = get_value: {
                var buffer = std.ArrayList(u8).init(alloc);
                defer buffer.deinit();
                try buffer.append(char);

                while (true) {
                    const byte = reader.readByte() catch |err| switch (err) {
                        error.EndOfStream => break,
                        else => return err,
                    };

                    switch (byte) {
                        ' ', '\t', '\r' => break,
                        '\n' => {
                            parser_state.* = .DeclarationBodyEnd1;
                            break;
                        },
                        else => try buffer.append(byte),
                    }
                }

                break :get_value try buffer.toOwnedSlice();
            };

            try tokens.append(.{ .value = value, .type = .Variable });
        },
    }
}

test "Test files" {
    // var originDir = std.fs.cwd();
    // try std.testing.expectEqualStrings("", try originDir.realpathAlloc(std.testing.allocator, "."));
    var dir = try std.fs.cwd().openDir("src/testFiles", .{ .iterate = true });
    defer dir.close();

    var walker = try dir.walk(std.testing.allocator);
    defer walker.deinit();

    var no_failures = true;
    while (try walker.next()) |entry| {
        if (entry.kind != .file) {
            continue;
        }

        const log = std.log.scoped(.file_scope);

        // try std.testing.expectEqualStrings("", entry.path);

        const file = try dir.openFile(entry.path, .{ .mode = .read_only });
        // const file = try std.fs.openFileAbsolute(entry.path, .{ .mode = .read_only });
        defer file.close();

        var contentBuffer = std.ArrayList(u8).init(std.testing.allocator);
        defer contentBuffer.deinit();

        try file.reader().readAllArrayList(&contentBuffer, 10_000);
        const fileContents = try contentBuffer.toOwnedSlice();
        defer std.testing.allocator.free(fileContents);

        var iter = std.mem.splitSequence(u8, fileContents, "---");
        const contents = iter.next() orelse {
            log.err("File `{s}` no language content supplied! Please write some vulpes code and then write `---` to separate the expected content!", .{entry.path});
            return error.NoLanguageContentSupplied;
        };
        var stream = std.io.fixedBufferStream(contents);

        const expectedContents = iter.next() orelse {
            log.err("File `{s}` no expected tokens supplied! Please write some vulpes code and then write `---` to separate the expected content!", .{entry.path});
            return error.NoExpectedTestTokensSupplied;
        };
        var expectedTokens = std.ArrayList(Token).init(std.testing.allocator);
        defer expectedTokens.deinit();
        // defer for (expectedTokens.items) |value| {
        //     value.deinit(std.testing.allocator);
        // };
        { // Generating expected contents...

            var expectedIter = std.mem.splitScalar(u8, expectedContents, '\n');
            var i: i32 = 0;
            while (expectedIter.next()) |line| : (i += 1) {
                if (line.len == 0) {
                    continue;
                }

                var lineIter = std.mem.splitScalar(u8, line, ' ');
                const type_str = lineIter.next() orelse {
                    log.err("File `{s}`, {} lines after `---`. No token supplied!", .{ entry.path, i + 1 });
                    return error.NoTokenTypeSupplied;
                };
                const value = lineIter.next();
                const token_type = std.meta.stringToEnum(TokenType, type_str) orelse {
                    log.err("File `{s}`, {} lines after `---`. Unknown token supplied `{s}`!", .{ entry.path, i + 1, type_str });
                    return error.InvalidTokenType;
                };

                try expectedTokens.append(.{ .value = value, .type = token_type });
            }
        }

        var tokens = try tokenize(std.testing.allocator, stream.reader().any());
        defer tokens.deinit();
        defer for (tokens.items) |value| {
            value.deinit(std.testing.allocator);
        };

        // try std.testing.expectEqualDeep(expectedTokens, tokens);

        const file_test_result = file_test_result: {
            for (tokens.items, 0..) |token, i| {
                const expected = expectedTokens.items[i];
                std.testing.expectEqual(expected.type, token.type) catch |err| break :file_test_result err;
                if (expected.value != null and token.value != null) {
                    std.testing.expectEqualStrings(expected.value.?, token.value.?) catch |err| break :file_test_result err;
                } else {
                    std.testing.expectEqual(expected.value, token.value) catch |err| break :file_test_result err;
                }
            }
        };

        file_test_result catch |err| {
            const tokens_str = try Token.arrayToString(tokens, std.testing.allocator);
            defer std.testing.allocator.free(tokens_str);

            const expected_str = try Token.arrayToString(expectedTokens, std.testing.allocator);
            defer std.testing.allocator.free(expected_str);

            log.err("Failed test for file: `{s}`.\nERROR: {}\nACTUAL: {s}\nEXPECTED: {s}", .{
                entry.path,
                err,
                tokens_str,
                expected_str,
            });
            no_failures = false;
        };
    }

    try std.testing.expect(no_failures);
}

test "simple test" {
    var list = std.ArrayList(i32).init(std.testing.allocator);
    defer list.deinit(); // try commenting this out and see if zig detects the memory leak!
    try list.append(42);
    try std.testing.expectEqual(@as(i32, 42), list.pop());
}
