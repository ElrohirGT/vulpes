const std = @import("std");

pub const PeekableReader = struct {
    _peek_cache: ?u8 = null,
    _inner: std.io.AnyReader,

    pub fn fromReader(reader: std.io.AnyReader) PeekableReader {
        return .{ ._inner = reader };
    }

    /// Tries to peek at the next byte in the stream.
    /// Fails if reached EOF.
    pub fn peekByte(s: *PeekableReader) ?u8 {
        if (s._peek_cache == null) {
            s._peek_cache = s._inner.readByte() catch return null;
        }

        return s._peek_cache.?;
    }

    /// Reads a byte from the stream.
    pub fn readByte(s: *PeekableReader) !u8 {
        if (s._peek_cache) |byte| {
            s._peek_cache = null;
            return byte;
        }

        return s._inner.readByte();
    }

    /// Reads bytes from the stream until:
    /// * A non whitespace character appears (whitespace characters are: ' ', '\t', '\r').
    /// * A new line character appears (\n).
    /// * Reached EOF.
    pub fn readWhileWhitespaceOrNotEOL(s: *PeekableReader) void {
        while (true) {
            const nextByte = s.peekByte() orelse return;
            switch (nextByte) {
                ' ', '\t', '\r' => _ = s.readByte() catch unreachable,
                else => return,
            }
        }
    }

    /// Writes to the writer until it encounters a whitespace.
    /// It doesn't consume the whitespace.
    pub fn streamUntilWhitespace(s: *PeekableReader, writer: anytype) !void {
        while (true) {
            const nextByte = s.peekByte() orelse return;
            switch (nextByte) {
                ' ', '\t', '\r' => return,
                else => {
                    const byte = s.readByte() catch unreachable;
                    try writer.writeByte(byte);
                },
            }
        }
    }

    /// Streams bytes to the writer until it reaches a non number byte or EOF.
    /// It doesn't consumes the non number byte!
    pub fn streamUntilNotNumber(s: *PeekableReader, writer: anytype) !void {
        while (true) {
            const nextByte = s.peekByte() orelse return;
            switch (nextByte) {
                '0'...'9' => {
                    const byte = s.readByte() catch unreachable;
                    try writer.writeByte(byte);
                },
                else => return,
            }
        }
    }

    /// Reads all the remaining bytes from the stream until the end.
    /// It can fail because of the writer!
    pub fn streamUntilEnd(s: *PeekableReader, writer: anytype) !void {
        while (true) {
            const byte = s.readByte() catch return;
            try writer.writeByte(byte);
        }
    }
};

test "Can peek" {
    const str =
        \\hello World
        \\ABCD
    ;

    var stream = std.io.fixedBufferStream(str);
    var reader = PeekableReader.fromReader(stream.reader().any());

    try std.testing.expectEqual('h', try reader.readByte());
    try std.testing.expectEqual('e', reader.peekByte() orelse return error.ReachedEOF);
    try std.testing.expectEqual('e', try reader.readByte());
    try std.testing.expectEqual('l', try reader.readByte());
}
