const std = @import("std");

const cpu = @import("cpu/state.zig");
const gpu = @import("gpu/state.zig");
const MMIO = @import("mmio.zig").MMIO;

pub const PSXState = struct {
    registers: cpu.Registers = .{},
    branch: bool = false,
    delay_slot: bool = false,
    gpu: gpu.State = .{},
    mmio: MMIO = .{},
    ram: []u8,
    bios: [BIOS_SizeBytes]u8,
};

pub fn create_state(bios: [BIOS_SizeBytes]u8, allocator: std.mem.Allocator) !PSXState {
    const ram = try allocator.alloc(u8, RAM_SizeBytes);
    errdefer allocator.free(ram);

    return PSXState{
        .ram = ram,
        .bios = bios,
    };
}

pub fn destroy_state(psx: *PSXState, allocator: std.mem.Allocator) void {
    allocator.free(psx.ram);
}

pub const RAM_SizeBytes = 2 * 1024 * 1024;
pub const BIOS_SizeBytes = 512 * 1024;
