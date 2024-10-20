const std = @import("std");

const cpu = @import("cpu.zig");
const PSXState = cpu.PSXState;

pub fn load_mem_u8(psx: *PSXState, address: u32) u8 {
    return load_mem_generic(u8, psx, address);
}

pub fn load_mem_u16(psx: *PSXState, address: u32) u16 {
    return load_mem_generic(u16, psx, address);
}

pub fn load_mem_u32(psx: *PSXState, address: u32) u32 {
    return load_mem_generic(u32, psx, address);
}

pub fn store_mem_u8(psx: *PSXState, address: u32, value: u8) void {
    store_mem_generic(u8, psx, address, value);
}

pub fn store_mem_u16(psx: *PSXState, address: u32, value: u16) void {
    store_mem_generic(u16, psx, address, value);
}

pub fn store_mem_u32(psx: *PSXState, address: u32, value: u32) void {
    store_mem_generic(u32, psx, address, value);
}

const PSXAddress = packed struct {
    offset: u29,
    mapping: enum(u3) {
        Useg = 0b000,
        Seg0 = 0b100,
        Seg1 = 0b101,
        Seg2 = 0b111,
    },
};

// FIXME does cache isolation has any impact here?
fn load_mem_generic(comptime T: type, psx: *PSXState, address_u32: u32) T {
    const type_info = @typeInfo(T);
    const type_bits = type_info.Int.bits;
    const type_bytes = type_bits / 8;

    std.debug.assert(type_info.Int.signedness == .unsigned);
    std.debug.assert(type_bits % 8 == 0);

    std.debug.print("load addr: 0x{x:0>8}\n", .{address_u32});

    // {{ and }} are escaped curly brackets
    // const type_format_string = std.fmt.comptimePrint("0x{{x:0>{}}}", .{type_bytes * 2});
    // std.debug.print("store value: " ++ type_format_string ++ "\n", .{value});

    const address: PSXAddress = @bitCast(address_u32);

    std.debug.assert(address.offset % type_bytes == 0);

    switch (address.mapping) {
        .Useg, .Seg0, .Seg1 => {
            switch (address.offset) {
                RAM_Offset...RAM_OffsetEnd - 1 => |offset| {
                    const local_offset = offset - RAM_Offset;
                    const type_slice = psx.ram[local_offset..];
                    return std.mem.readInt(T, type_slice[0..type_bytes], .little);
                },
                HWRegs_Offset...HWRegs_OffsetEnd - 1 => |offset| {
                    switch (offset) {
                        HWRegs_InterruptMask_Offset, HWRegs_InterruptStatus_Offset => {
                            std.debug.print("FIXME Interrupt load ignored\n", .{});
                            return 0;
                        },
                        HWRegs_DMA_Offset...HWRegs_DMA_OffsetEnd - 1 => {
                            std.debug.print("FIXME DMA load ignored\n", .{});
                            return 0;
                        },
                        HWRegs_SPU_Offset...HWRegs_SPU_OffsetEnd - 1 => {
                            std.debug.print("FIXME SPU load ignored\n", .{});
                            return 0;
                        },
                        else => unreachable,
                    }
                },
                BIOS_Offset...BIOS_OffsetEnd - 1 => |offset| {
                    const local_offset = offset - BIOS_Offset;
                    const type_slice = psx.bios[local_offset..];
                    return std.mem.readInt(T, type_slice[0..type_bytes], .little);
                },
                Expansion_Offset...Expansion_OffsetEnd - 1 => |offset| switch (offset) {
                    Expansion_ParallelPortOffset => return 0xff,
                    else => unreachable,
                },
                else => unreachable,
            }
        },
        .Seg2 => {
            switch (address.offset) {
                CacheControl_Offset => {
                    std.debug.print("FIXME load ignored at cache control offset\n", .{});
                    return 0;
                },
                else => unreachable,
            }
        },
    }
}

fn store_mem_generic(comptime T: type, psx: *PSXState, address_u32: u32, value: T) void {
    const type_info = @typeInfo(T);
    const type_bits = type_info.Int.bits;
    const type_bytes = type_bits / 8;

    std.debug.assert(type_info.Int.signedness == .unsigned);
    std.debug.assert(type_bits % 8 == 0);

    std.debug.print("store addr: 0x{x:0>8}\n", .{address_u32});

    // {{ and }} are escaped curly brackets
    const type_format_string = std.fmt.comptimePrint("0x{{x:0>{}}}", .{type_bytes * 2});
    std.debug.print("store value: " ++ type_format_string ++ "\n", .{value});

    if (psx.registers.sr.isolate_cache == 1) {
        std.debug.print("FIXME store ignored because of cache isolation\n", .{});
        return;
    }

    const address: PSXAddress = @bitCast(address_u32);

    std.debug.assert(address.offset % type_bytes == 0);

    switch (address.mapping) {
        .Useg, .Seg0, .Seg1 => {
            switch (address.offset) {
                RAM_Offset...RAM_OffsetEnd - 1 => |offset| {
                    const local_offset = offset - RAM_Offset;
                    const type_slice = psx.ram[local_offset..];
                    std.mem.writeInt(T, type_slice[0..type_bytes], value, .little);
                },
                HWRegs_Offset...HWRegs_OffsetEnd - 1 => |offset| {
                    switch (offset) {
                        HWRegs_Expansion1BaseAddress_Offset, HWRegs_Expansion2BaseAddress_Offset => {
                            std.debug.print("FIXME store ignored to expansion register\n", .{});
                        },
                        HWRegs_0x1f801008_Offset, HWRegs_0x1f801010_Offset, HWRegs_0x1f80100c_Offset, HWRegs_0x1f801014_Offset, HWRegs_0x1f801018_Offset, HWRegs_0x1f80101c_Offset, HWRegs_0x1f801020_Offset, HWRegs_0x1f801060_Offset => {
                            std.debug.print("FIXME store ignored to unknown register\n", .{});
                        },
                        HWRegs_InterruptMask_Offset, HWRegs_InterruptStatus_Offset => {
                            std.debug.print("FIXME store ignored to IRQ register\n", .{});
                        },
                        HWRegs_DMA_Offset...HWRegs_DMA_OffsetEnd - 1 => {
                            std.debug.print("FIXME DMA store ignored\n", .{});
                        },
                        HWRegs_Timers_Offset...HWRegs_Timers_OffsetEnd - 1 => {
                            std.debug.print("FIXME Timer store ignored\n", .{});
                        },
                        HWRegs_SPU_Offset...HWRegs_SPU_OffsetEnd - 1 => {
                            std.debug.print("FIXME SPU store ignored\n", .{});
                        },
                        HWRegs_UnknownDebug_Offset => {
                            std.debug.print("FIXME debug store ignored\n", .{});
                        },
                        else => {
                            std.debug.print("FIXME debug store ignored\n", .{});
                        },
                    }
                },
                BIOS_Offset...BIOS_OffsetEnd - 1 => unreachable, // This should be read-only
                else => unreachable,
            }
        },
        .Seg2 => {
            switch (address.offset) {
                CacheControl_Offset => {
                    std.debug.print("FIXME store ignored at offset\n", .{});
                },
                else => unreachable,
            }
        },
    }
}

// KUSEG       KSEG0     KSEG1 Length Description
// 0x00000000 0x80000000 0xa0000000 2048K Main RAM
const RAM_Offset = 0x00000000;
const RAM_OffsetEnd = RAM_Offset + cpu.RAM_SizeBytes;

// 0x1f000000 0x9f000000 0xbf000000 8192K Expansion Region 1
const Expansion_SizeBytes = 8 * 1024 * 1024;
const Expansion_Offset = 0x1f000000;
const Expansion_OffsetEnd = Expansion_Offset + Expansion_SizeBytes;

const Expansion_ParallelPortOffset = 0x1f00_0084;

// 0x1f800000 0x9f800000 0xbf800000 1K Scratchpad
const Scratchpad_SizeBytes = 1024;
const Scratchpad_Offset = 0x1f800000;
const Scratchpad_OffsetEnd = Scratchpad_Offset + Scratchpad_SizeBytes;

// 0x1f801000 0x9f801000 0xbf801000 8K Hardware registers
const HWRegs_SizeBytes = 8 * 1024;
const HWRegs_Offset = 0x1f801000;
const HWRegs_OffsetEnd = HWRegs_Offset + HWRegs_SizeBytes;

const HWRegs_Expansion1BaseAddress_Offset = 0x1f801000;
const HWRegs_Expansion2BaseAddress_Offset = 0x1f801004;

const HWRegs_0x1f801008_Offset = 0x1f801008;
const HWRegs_0x1f80100c_Offset = 0x1f80100c;
const HWRegs_0x1f801010_Offset = 0x1f801010;
const HWRegs_0x1f801014_Offset = 0x1f801014;
const HWRegs_0x1f801018_Offset = 0x1f801018;
const HWRegs_0x1f80101c_Offset = 0x1f80101c;
const HWRegs_0x1f801020_Offset = 0x1f801020;
const HWRegs_0x1f801060_Offset = 0x1f801060;

const HWRegs_InterruptStatus_Offset = 0x1f801070;
const HWRegs_InterruptMask_Offset = 0x1f801074;

const HWRegs_DMA_SizeBytes = 0x80;
const HWRegs_DMA_Offset = 0x1f801080;
const HWRegs_DMA_OffsetEnd = HWRegs_DMA_Offset + HWRegs_DMA_SizeBytes;

// FIXME unclear where actually this lives
const HWRegs_Timers_SizeBytes = 3 * 16;
const HWRegs_Timers_Offset = 0x1f801100;
const HWRegs_Timers_OffsetEnd = HWRegs_Timers_Offset + HWRegs_Timers_SizeBytes;

const HWRegs_SPU_SizeBytes = 640;
const HWRegs_SPU_Offset = 0x1f801c00;
const HWRegs_SPU_OffsetEnd = HWRegs_SPU_Offset + HWRegs_SPU_SizeBytes;

const HWRegs_UnknownDebug_Offset = 0x1f802041;

// 0x1fc00000 0x9fc00000 0xbfc00000 512K BIOS ROM
const BIOS_Offset = 0x1fc00000;
const BIOS_OffsetEnd = BIOS_Offset + cpu.BIOS_SizeBytes;

const CacheControl_Offset = 0x1ffe0130;
