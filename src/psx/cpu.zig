const std = @import("std");

const debug = @import("cpu_debug.zig");
const execution = @import("cpu_execution.zig");
const instructions = @import("cpu_instructions.zig");

// KUSEG       KSEG0     KSEG1 Length Description
// 0x00000000 0x80000000 0xa0000000 2048K Main RAM
const RAM_SizeBytes = 2 * 1024 * 1024;
const RAM_Offset = 0x00000000;
const RAM_OffsetEnd = RAM_Offset + RAM_SizeBytes;

// 0x1f000000 0x9f000000 0xbf000000 8192K Expansion Region 1
const Expansion_SizeBytes = 8 * 1024 * 1024;
const Expansion_Offset = 0x1f000000;
const Expansion_OffsetEnd = Expansion_Offset + Expansion_SizeBytes;

// 0x1f800000 0x9f800000 0xbf800000 1K Scratchpad
const Scratchpad_SizeBytes = 1024;
const Scratchpad_Offset = 0x1f800000;
const Scratchpad_OffsetEnd = Scratchpad_Offset + Scratchpad_SizeBytes;

// 0x1f801000 0x9f801000 0xbf801000 8K Hardware registers
const HWRegs_SizeBytes = 8 * 1024;
const HWRegs_Offset = 0x1f801000;
const HWRegs_OffsetEnd = HWRegs_Offset + HWRegs_SizeBytes;

const HWRegs_SPU_SizeBytes = 640;
const HWRegs_SPU_Offset = 0x1f801c00;
const HWRegs_SPU_OffsetEnd = HWRegs_SPU_Offset + HWRegs_SPU_SizeBytes;

const HWRegs_UnknownDebug_Offset = 0x1f802041;

// 0x1fc00000 0x9fc00000 0xbfc00000 512K BIOS ROM
pub const BIOS_SizeBytes = 512 * 1024;
const BIOS_Offset = 0x1fc00000;
const BIOS_OffsetEnd = BIOS_Offset + BIOS_SizeBytes;

pub const PSXState = struct {
    registers: Registers = .{},
    bios: [BIOS_SizeBytes]u8,
    ram: []u8,
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

// Register Name Conventional use
pub const RegisterName = enum(u5) {
    zero = 0, // Always zero
    at = 1, // Assembler temporary
    v0 = 2, // Function return values
    v1 = 3,
    a0 = 4, // Function arguments
    a1,
    a2,
    a3,
    t0 = 8, // Temporary registers
    t1,
    t2,
    t3,
    t4,
    t5,
    t6,
    t7,
    s0 = 16, // Saved registers
    s1,
    s2,
    s3,
    s4,
    s5,
    s6,
    s7,
    t8 = 24, // Temporary registers
    t9 = 25,
    k0 = 26, // Kernel reserved registers
    k1 = 27,
    gp = 28, // Global pointer
    sp = 29, // Stack pointer
    fp = 30, // Frame pointer
    ra = 31, // Function return address
};

pub const Registers = struct {
    pc: u32 = 0xbfc00000, // Program Counter
    r_in: [32]u32 = undefined, // FIXME does it have an initial value?
    r_out: [32]u32 = undefined, // FIXME does it have an initial value?
    pending_load: ?struct { register: RegisterName, value: u32 } = null,
    sr: u32 = undefined,
};

const PSXAddress = packed struct {
    offset: u29,
    mapping: enum(u3) {
        Useg = 0b000,
        Seg0 = 0b100,
        Seg1 = 0b101,
        Seg2 = 0b111,
    },
};

pub fn load_mem_u8(psx: *PSXState, address_u32: u32) u8 {
    std.debug.print("load addr: 0x{x:0>8}\n", .{address_u32});

    const address: PSXAddress = @bitCast(address_u32);

    switch (address.mapping) {
        .Useg, .Seg0, .Seg1 => {
            switch (address.offset) {
                RAM_Offset...RAM_OffsetEnd - 1 => |offset| {
                    const local_offset = offset - RAM_Offset;
                    return psx.ram[local_offset];
                },
                BIOS_Offset...BIOS_OffsetEnd - 1 => |offset| {
                    const local_offset = offset - BIOS_Offset;
                    return psx.bios[local_offset];
                },
                else => unreachable,
            }
        },
        .Seg2 => unreachable,
    }
}

pub fn load_mem_u32(psx: *PSXState, address_u32: u32) u32 {
    std.debug.print("load addr: 0x{x:0>8}\n", .{address_u32});

    const address: PSXAddress = @bitCast(address_u32);

    std.debug.assert(address.offset % 4 == 0); // FIXME implement bus errors

    switch (address.mapping) {
        .Useg, .Seg0, .Seg1 => {
            switch (address.offset) {
                RAM_Offset...RAM_OffsetEnd - 1 => |offset| {
                    const local_offset = offset - RAM_Offset;
                    const u32_slice = psx.ram[local_offset..];
                    return std.mem.readInt(u32, u32_slice[0..4], .little);
                },
                BIOS_Offset...BIOS_OffsetEnd - 1 => |offset| {
                    const local_offset = offset - BIOS_Offset;
                    const u32_slice = psx.bios[local_offset..];
                    return std.mem.readInt(u32, u32_slice[0..4], .little);
                },
                else => unreachable,
            }
        },
        .Seg2 => unreachable,
    }
}

pub fn store_mem_u8(psx: *PSXState, address_u32: u32, value: u8) void {
    std.debug.print("store addr: 0x{x:0>8}\n", .{address_u32});
    std.debug.print("store value: 0x{x:0>2}\n", .{value});

    if (psx.registers.sr & 0x00010000 != 0) {
        std.debug.print("FIXME store ignored because of cache isolation\n", .{});
        return;
    }

    const address: PSXAddress = @bitCast(address_u32);

    switch (address.mapping) {
        .Useg, .Seg0, .Seg1 => {
            switch (address.offset) {
                HWRegs_Offset...HWRegs_OffsetEnd - 1 => |offset| {
                    switch (offset) {
                        HWRegs_UnknownDebug_Offset => {
                            std.debug.print("FIXME Debug store ignored\n", .{});
                        },
                        HWRegs_SPU_Offset...HWRegs_SPU_OffsetEnd - 1 => {
                            std.debug.print("FIXME SPU store ignored\n", .{});
                        },
                        else => unreachable,
                    }
                },
                else => unreachable,
            }
        },
        .Seg2 => {
            switch (address.offset) {
                0x1ffe0130 => {
                    std.debug.print("FIXME store ignored at offset 0x{x:0>8}\n", .{address_u32});
                },
                else => unreachable,
            }
        },
    }
}

pub fn store_mem_u16(psx: *PSXState, address_u32: u32, value: u16) void {
    std.debug.print("store addr: 0x{x:0>8}\n", .{address_u32});
    std.debug.print("store value: 0x{x:0>4}\n", .{value});

    if (psx.registers.sr & 0x00010000 != 0) {
        std.debug.print("FIXME store ignored because of cache isolation\n", .{});
        return;
    }

    const address: PSXAddress = @bitCast(address_u32);

    std.debug.assert(address.offset % 2 == 0); // FIXME implement bus errors

    switch (address.mapping) {
        .Useg, .Seg0, .Seg1 => {
            switch (address.offset) {
                HWRegs_Offset...HWRegs_OffsetEnd - 1 => |offset| {
                    switch (offset) {
                        HWRegs_SPU_Offset...HWRegs_SPU_OffsetEnd - 1 => {
                            std.debug.print("FIXME SPU store ignored\n", .{});
                        },
                        else => unreachable,
                    }
                },
                else => unreachable,
            }
        },
        .Seg2 => {
            switch (address.offset) {
                0x1ffe0130 => {
                    std.debug.print("FIXME store ignored at offset 0x{x:0>8}\n", .{address_u32});
                },
                else => unreachable,
            }
        },
    }
}

pub fn store_mem_u32(psx: *PSXState, address_u32: u32, value: u32) void {
    std.debug.print("store addr: 0x{x:0>8}\n", .{address_u32});
    std.debug.print("store value: 0x{x:0>8}\n", .{value});

    if (psx.registers.sr & 0x00010000 != 0) {
        std.debug.print("FIXME store ignored because of cache isolation\n", .{});
        return;
    }

    const address: PSXAddress = @bitCast(address_u32);

    std.debug.assert(address.offset % 4 == 0); // FIXME implement bus errors

    switch (address.mapping) {
        .Useg, .Seg0, .Seg1 => {
            switch (address.offset) {
                RAM_Offset...RAM_OffsetEnd - 1 => |offset| {
                    const local_offset = offset - RAM_Offset;
                    const u32_slice = psx.ram[local_offset..];
                    return std.mem.writeInt(u32, u32_slice[0..4], value, .little);
                },
                HWRegs_Offset...HWRegs_OffsetEnd - 1 => |offset| {
                    const local_offset: u13 = @intCast(offset - HWRegs_Offset);
                    const local_offset_typed: HWRegOffsets = @enumFromInt(local_offset);
                    switch (local_offset_typed) {
                        .Expansion1BaseAddress, .Expansion2BaseAddress => {
                            std.debug.print("FIXME store ignored to expansion register 0x{x:0>8}\n", .{local_offset});
                        },
                        else => {
                            std.debug.print("FIXME store ignored at local offset 0x{x:0>8}\n", .{local_offset});
                        },
                    }
                },
                BIOS_Offset...BIOS_OffsetEnd - 1 => unreachable, // This should be read-only
                else => unreachable,
            }
        },
        .Seg2 => {
            switch (address.offset) {
                0x1ffe0130 => {
                    std.debug.print("FIXME store ignored at offset 0x{x:0>8}\n", .{address_u32});
                },
                else => unreachable,
            }
        },
    }
}

pub fn execute(psx: *PSXState) void {
    const nop_op_code = 0;
    var next_op_code: u32 = nop_op_code;

    while (true) {
        const op_code = next_op_code;

        // Execute any pending memory loads
        if (psx.registers.pending_load) |pending_load| {
            execution.store_reg(&psx.registers, pending_load.register, pending_load.value);
            psx.registers.pending_load = null;
        }

        next_op_code = load_mem_u32(psx, psx.registers.pc);

        // Execution is pipelined so we increment the PC before even starting to execute the current instruction
        psx.registers.pc +%= 4;

        const instruction = instructions.decode_instruction(op_code);

        debug.print_instruction(op_code, instruction);

        execution.execute_instruction(psx, instruction);

        std.mem.copyForwards(u32, &psx.registers.r_in, &psx.registers.r_out);
    }
}

const HWRegOffsets = enum(u13) {
    Expansion1BaseAddress = 0,
    Expansion2BaseAddress = 4,
    _,
};
