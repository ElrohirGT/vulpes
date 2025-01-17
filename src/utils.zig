/// Checks if a byte is contained inside de variants array!
pub fn isByteIn(byte: u8, comptime variants: []const u8) bool {
    inline for (variants) |variant| {
        if (variant == byte) {
            return true;
        }
    } else return false;
}
