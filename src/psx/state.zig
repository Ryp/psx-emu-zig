const std = @import("std");

const mmio = @import("mmio.zig");
const cpu = @import("cpu/state.zig");
const gpu = @import("gpu/state.zig");

pub const PSXState = struct {
    registers: cpu.Registers = .{},
    branch: bool = false,
    delay_slot: bool = false,
    bios: [BIOS_SizeBytes]u8,
    ram: []u8,
    mmio: mmio.MMIO = .{},
    gpu: gpu.State = .{},
};

pub fn create_psx_state(bios: [BIOS_SizeBytes]u8, allocator: std.mem.Allocator) !PSXState {
    const ram = try allocator.alloc(u8, RAM_SizeBytes);
    errdefer allocator.free(ram);

    return PSXState{
        .bios = bios,
        .ram = ram,
    };
}

pub fn destroy_psx_state(psx: *PSXState, allocator: std.mem.Allocator) void {
    allocator.free(psx.ram);
}

pub const RAM_SizeBytes = 2 * 1024 * 1024;
pub const BIOS_SizeBytes = 512 * 1024;
