const std = @import("std");
const assert = std.debug.assert;

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();

    const allocator = gpa.allocator();
    // const allocator = std.heap.page_allocator;

    // Parse arguments
    const args = try std.process.argsAlloc(allocator);
    defer std.process.argsFree(allocator, args);

    if (args.len < 2) {
        std.debug.print("error: missing ISO filename\n", .{});
        return error.MissingArgument;
    }

    const iso_filename = args[1];

    // FIXME What the idiomatic way of writing this?
    var file = if (std.fs.cwd().openFile(iso_filename, .{})) |f| f else |err| {
        std.debug.print("error: couldn't open ISO file: '{s}'\n", .{iso_filename});
        return err;
    };
    defer file.close();
}
