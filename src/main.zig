const std = @import("std");
const assert = std.debug.assert;

const psx_state = @import("psx/state.zig");
const cpu_execution = @import("psx/cpu/execution.zig");

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

    const bios_filename = args[1];

    // FIXME What the idiomatic way of writing this?
    var file = if (std.fs.cwd().openFile(bios_filename, .{})) |f| f else |err| {
        std.debug.print("error: couldn't open BIOS file: '{s}'\n", .{bios_filename});
        return err;
    };
    defer file.close();

    var bios_buffer: [psx_state.BIOS_SizeBytes]u8 = undefined;
    const bios_bytes_read = try file.read(&bios_buffer);

    assert(bios_bytes_read == bios_buffer.len);

    var psx = try psx_state.create_state(bios_buffer, allocator);
    defer psx_state.destroy_state(&psx, allocator);

    while (true) {
        cpu_execution.step(&psx);
    }
}
