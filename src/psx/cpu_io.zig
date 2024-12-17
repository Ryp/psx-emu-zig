const std = @import("std");

const cpu = @import("cpu.zig");
const PSXState = cpu.PSXState;

const dma = @import("dma.zig");

pub fn load_mem_u8(psx: *PSXState, address: u32) u8 {
    return load_mem_generic(u8, psx, @bitCast(address));
}

pub fn load_mem_u16(psx: *PSXState, address: u32) u16 {
    return load_mem_generic(u16, psx, @bitCast(address));
}

pub fn load_mem_u32(psx: *PSXState, address: u32) u32 {
    return load_mem_generic(u32, psx, @bitCast(address));
}

pub fn store_mem_u8(psx: *PSXState, address: u32, value: u8) void {
    store_mem_generic(u8, psx, @bitCast(address), value);
}

pub fn store_mem_u16(psx: *PSXState, address: u32, value: u16) void {
    store_mem_generic(u16, psx, @bitCast(address), value);
}

pub fn store_mem_u32(psx: *PSXState, address: u32, value: u32) void {
    store_mem_generic(u32, psx, @bitCast(address), value);
}

pub const PSXAddress = packed struct {
    offset: u29,
    mapping: enum(u3) {
        Useg = 0b000,
        Seg0 = 0b100,
        Seg1 = 0b101,
        Seg2 = 0b111,
    },
};

// FIXME does cache isolation has any impact here?
fn load_mem_generic(comptime T: type, psx: *PSXState, address: PSXAddress) T {
    const type_info = @typeInfo(T);
    const type_bits = type_info.Int.bits;
    const type_bytes = type_bits / 8;

    std.debug.assert(type_info.Int.signedness == .unsigned);
    std.debug.assert(type_bits % 8 == 0);

    if (cpu.enable_debug_print) {
        std.debug.print("load addr: 0x{x:0>8}\n", .{@as(u32, @bitCast(address))});
    }

    std.debug.assert(address.offset % type_bytes == 0);

    switch (address.mapping) {
        .Useg, .Seg0, .Seg1 => {
            switch (address.offset) {
                RAM_Offset...RAM_OffsetEnd - 1 => |offset| {
                    const local_offset = offset - RAM_Offset;
                    const type_slice = psx.ram[local_offset..];
                    return std.mem.readInt(T, type_slice[0..type_bytes], .little);
                },
                MMIO_Offset...MMIO_OffsetEnd - 1 => |offset| {
                    const local_offset = offset - MMIO_Offset;
                    const mmio_bytes = std.mem.asBytes(&psx.mmio);
                    const type_slice = mmio_bytes[local_offset..][0..type_bytes];

                    switch (offset) {
                        MMIO_InterruptMask_Offset,
                        MMIO_InterruptStatus_Offset,
                        MMIO_GPUREAD_G0_Offset,
                        MMIO_GPUSTAT_G1_Offset,
                        MMIO_SPU_Offset...MMIO_SPU_OffsetEnd - 1,
                        => {
                            if (cpu.enable_debug_print) {
                                std.debug.print("FIXME load ignored\n", .{});
                            }
                            return 0;
                        },
                        dma.MMIO.Offset...dma.MMIO.OffsetEnd - 1 => {
                            return dma.load_mmio_generic(T, psx, offset);
                        },
                        else => {
                            const value = std.mem.readInt(T, type_slice[0..type_bytes], .little);
                            std.debug.print("address {x} = {}\n", .{ offset, value });
                            unreachable;
                        },
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
                    if (cpu.enable_debug_print) {
                        std.debug.print("FIXME load ignored at cache control offset\n", .{});
                    }
                    return 0;
                },
                else => unreachable,
            }
        },
    }
}

fn store_mem_generic(comptime T: type, psx: *PSXState, address: PSXAddress, value: T) void {
    const type_info = @typeInfo(T);
    const type_bits = type_info.Int.bits;
    const type_bytes = type_bits / 8;

    std.debug.assert(type_info.Int.signedness == .unsigned);
    std.debug.assert(type_bits % 8 == 0);

    if (cpu.enable_debug_print) {
        std.debug.print("store addr: 0x{x:0>8}\n", .{@as(u32, @bitCast(address))});

        // {{ and }} are escaped curly brackets
        const type_format_string = std.fmt.comptimePrint("0x{{x:0>{}}}", .{type_bytes * 2});
        std.debug.print("store value: " ++ type_format_string ++ "\n", .{value});
    }

    if (psx.registers.sr.isolate_cache == 1) {
        if (cpu.enable_debug_print) {
            std.debug.print("FIXME store ignored because of cache isolation\n", .{});
        }
        return;
    }

    std.debug.assert(address.offset % type_bytes == 0);

    switch (address.mapping) {
        .Useg, .Seg0, .Seg1 => {
            switch (address.offset) {
                RAM_Offset...RAM_OffsetEnd - 1 => |offset| {
                    const local_offset = offset - RAM_Offset;
                    const type_slice = psx.ram[local_offset..];
                    std.mem.writeInt(T, type_slice[0..type_bytes], value, .little);
                },
                MMIO_Offset...MMIO_OffsetEnd - 1 => |offset| {
                    switch (offset) {
                        MMIO_Expansion1BaseAddress_Offset,
                        MMIO_Expansion2BaseAddress_Offset,
                        MMIO_0x1f801008_Offset,
                        MMIO_0x1f801010_Offset,
                        MMIO_0x1f80100c_Offset,
                        MMIO_0x1f801014_Offset,
                        MMIO_0x1f801018_Offset,
                        MMIO_0x1f80101c_Offset,
                        MMIO_0x1f801020_Offset,
                        MMIO_0x1f801060_Offset,
                        MMIO_InterruptMask_Offset,
                        MMIO_InterruptStatus_Offset,
                        MMIO_Timers_Offset...MMIO_Timers_OffsetEnd - 1,
                        MMIO_GPUREAD_G0_Offset,
                        MMIO_SPU_Offset...MMIO_SPU_OffsetEnd - 1,
                        MMIO_UnknownDebug_Offset,
                        => {
                            if (cpu.enable_debug_print) {
                                std.debug.print("FIXME store ignored\n", .{});
                            }
                        },
                        MMIO_GPUSTAT_G1_Offset => unreachable,
                        dma.MMIO.Offset...dma.MMIO.OffsetEnd - 1 => {
                            dma.store_mmio_generic(T, psx, offset, value);
                        },
                        else => {
                            std.debug.print("address = {x}\n", .{offset});
                            unreachable;
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
                    if (cpu.enable_debug_print) {
                        std.debug.print("FIXME store ignored at offset\n", .{});
                    }
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

// 0x1fc00000 0x9fc00000 0xbfc00000 512K BIOS ROM
const BIOS_Offset = 0x1fc00000;
const BIOS_OffsetEnd = BIOS_Offset + cpu.BIOS_SizeBytes;

// 0x1ffe0130constant
const CacheControl_Offset = 0x1ffe0130;

// 0x1f801000 0x9f801000 0xbf801000 8K Hardware registers
const MMIO_SizeBytes = 8 * 1024;
pub const MMIO_Offset = 0x1f801000;
const MMIO_OffsetEnd = MMIO_Offset + MMIO_SizeBytes;
pub const MMIO = packed struct {
    memory_control1: MMIO_MemoryControl1 = .{},
    io_ports: MMIO_IOPorts = .{},
    memory_control2: MMIO_MemoryControl2 = .{},
    interrupt_control: MMIO_IRQControl = .{},
    dma: dma.MMIO.Packed = .{},
    timers: MMIO_Timers = .{},
    cdrom: MMIO_CDROM = .{},
    gpu: MMIO_GPU = .{},
    mdec: MMIO_MDEC = .{},
    spu: MMIO_SPU = .{},
    expansion2: MMIO_Expansion2 = .{},
};

const MMIO_MemoryControl1_Offset = 0x1f801000;
const MMIO_MemoryControl1_SizeBytes = MMIO_IOPorts_Offset - MMIO_MemoryControl1_Offset;
const MMIO_MemoryControl1 = packed struct {
    _unused: u512 = undefined,
};

const MMIO_IOPorts_Offset = 0x1f801040;
const MMIO_IOPorts_SizeBytes = MMIO_MemoryControl2_Offset - MMIO_IOPorts_Offset;
const MMIO_IOPorts = packed struct {
    _unused: u256 = undefined,
};

const MMIO_MemoryControl2_Offset = 0x1f801060;
const MMIO_MemoryControl2_SizeBytes = MMIO_IRQControl_Offset - MMIO_MemoryControl2_Offset;
const MMIO_MemoryControl2 = packed struct {
    ram_size: u32 = 0x0B88,
    _unused: u96 = undefined,
};

const MMIO_IRQControl_Offset = 0x1f801070;
const MMIO_IRQControl_SizeBytes = dma.MMIO.Offset - MMIO_IRQControl_Offset;
const MMIO_IRQControl = packed struct {
    control: u32 = undefined,
    mask: u32 = undefined,
    _unused: u64 = undefined,
};

pub const MMIO_Timers_Offset = 0x1f801100;
const MMIO_Timers_SizeBytes = MMIO_CDROM_Offset - MMIO_Timers_Offset;
const MMIO_Timers_OffsetEnd = MMIO_Timers_Offset + MMIO_Timers_SizeBytes;
const MMIO_Timers = packed struct {
    _unused: u14336 = undefined,
};

const MMIO_CDROM_Offset = 0x1f801800;
const MMIO_CDROM_SizeBytes = MMIO_GPU_Offset - MMIO_CDROM_Offset;
const MMIO_CDROM = packed struct {
    _unused: u128 = undefined,
};

const MMIO_GPU_Offset = 0x1f801810;
const MMIO_GPU_SizeBytes = MMIO_MDEC_Offset - MMIO_GPU_Offset;
const MMIO_GPU = packed struct {
    // 1F801810h.Read  4   GPUREAD Read responses to GP0(C0h) and GP1(10h) commands
    // 1F801810h.Write 4   GP0 Send GP0 Commands/Packets (Rendering and VRAM Access)
    GPUREAD: packed struct {
        todo: u32 = undefined,
    } = undefined,
    // 1F801814h.Read  4   GPUSTAT Read GPU Status Register
    // 1F801814h.Write 4   GP1 Send GP1 Commands (Display Control)
    GPUSTAT: packed struct { // Read-only
        texture_x_base: u4, // 0-3   Texture page X Base   (N*64)                              ;GP0(E1h).0-3
        texture_y_base: u1, // 4     Texture page Y Base   (N*256) (ie. 0 or 256)              ;GP0(E1h).4
        semi_transparency: u2, // 5-6   Semi Transparency     (0=B/2+F/2, 1=B+F, 2=B-F, 3=B+F/4)  ;GP0(E1h).5-6
        texture_page_colors: u2, // Texture page colors   (0=4bit, 1=8bit, 2=15bit, 3=Reserved)GP0(E1h).7-8
        dither_mode: u1, // Dither 24bit to 15bit (0=Off/strip LSBs, 1=Dither Enabled);GP0(E1h).9
        draw_to_display_area: u1, //   10    Drawing to display area (0=Prohibited, 1=Allowed)         ;GP0(E1h).10
        set_mask_bit_when_drawing: u1, //   11    Set Mask-bit when drawing pixels (0=No, 1=Yes/Mask)       ;GP0(E6h).0
        draw_pixels: u1, //   12    Draw Pixels           (0=Always, 1=Not to Masked areas)   ;GP0(E6h).1
        interlace_field: u1, //   13    Interlace Field       (or, always 1 when GP1(08h).5=0)
        reverse_flag: u1, //   14    "Reverseflag"         (0=Normal, 1=Distorted)             ;GP1(08h).7
        texture_disable: u1, //   15    Texture Disable       (0=Normal, 1=Disable Textures)      ;GP0(E1h).11
        horizontal_resolution1: u1, //   16    Horizontal Resolution 2     (0=256/320/512/640, 1=368)    ;GP1(08h).6
        horizontal_resolution2: u2, //   17-18 Horizontal Resolution 1     (0=256, 1=320, 2=512, 3=640)  ;GP1(08h).0-1
        vertical_resolution: u1, //   19    Vertical Resolution         (0=240, 1=480, when Bit22=1)  ;GP1(08h).2
        video_mode: u1, //   20    Video Mode                  (0=NTSC/60Hz, 1=PAL/50Hz)     ;GP1(08h).3
        display_area_color_depth: u1, //   21    Display Area Color Depth    (0=15bit, 1=24bit)            ;GP1(08h).4
        vertical_interlace: u1, //   22    Vertical Interlace          (0=Off, 1=On)                 ;GP1(08h).5
        display_enable: u1, //   23    Display Enable              (0=Enabled, 1=Disabled)       ;GP1(03h).0
        interrupt_request: u1, //   24    Interrupt Request (IRQ1)    (0=Off, 1=IRQ)       ;GP0(1Fh)/GP1(02h)
        dma_data_request_mode: u1, //   25    DMA / Data Request, meaning depends on GP1(04h) DMA Direction:
        // When GP1(04h)=0 ---> Always zero (0)
        // When GP1(04h)=1 ---> FIFO State  (0=Full, 1=Not Full)
        // When GP1(04h)=2 ---> Same as GPUSTAT.28
        // When GP1(04h)=3 ---> Same as GPUSTAT.27
        ready_to_send_cmd: u1, //   26    Ready to receive Cmd Word   (0=No, 1=Ready)  ;GP0(...) ;via GP0
        ready_to_send_vram_to_cpu: u1, //   27    Ready to send VRAM to CPU   (0=No, 1=Ready)  ;GP0(C0h) ;via GPUREAD
        ready_to_recv_dma_block: u1, //   28    Ready to receive DMA Block  (0=No, 1=Ready)  ;GP0(...) ;via GP0
        dma_direction: u2, //   29-30 DMA Direction (0=Off, 1=?, 2=CPUtoGP0, 3=GPUREADtoCPU)    ;GP1(04h).0-1
        drawing_even_odd_line_in_interlace_mode: u1, //   31    Drawing even/odd lines in interlace mode (0=Even or Vblank, 1=Odd)
        //
        // In 480-lines mode, bit31 changes per frame. And in 240-lines mode, the bit changes per scanline.
        // The bit is always zero during Vblank (vertical retrace and upper/lower screen border).
        //
        // Note
        // Further GPU status information can be retrieved via GP1(10h) and GP0(C0h).
        //
        // Ready Bits
        // Bit28: Normally, this bit gets cleared when the command execution is busy (ie. once when the command and all of its parameters are received),
        // however, for Polygon and Line Rendering commands, the bit gets cleared immediately after receiving the command word (ie. before receiving the vertex parameters).
        // The bit is used as DMA request in DMA Mode 2, accordingly, the DMA would probably hang if the Polygon/Line parameters are transferred in a separate DMA block (ie. the DMA probably starts ONLY on command words).
        // Bit27: Gets set after sending GP0(C0h) and its parameters, and stays set until all data words are received; used as DMA request in DMA Mode 3.
        // Bit26: Gets set when the GPU wants to receive a command. If the bit is cleared, then the GPU does either want to receive data,
        // or it is busy with a command execution (and doesn't want to receive anything).
        // Bit25: This is the DMA Request bit, however, the bit is also useful for non-DMA transfers, especially in the FIFO State mode.
    } = undefined,
    _unused: u64 = undefined,
};

const MMIO_MDEC_Offset = 0x1f801820;
const MMIO_MDEC_SizeBytes = MMIO_SPU_Offset - MMIO_MDEC_Offset;
const MMIO_MDEC_OffsetEnd = MMIO_MDEC_Offset + MMIO_MDEC_SizeBytes;
const MMIO_MDEC = packed struct {
    _unused: u7936 = undefined,
};

const MMIO_SPU_Offset = 0x1f801c00;
const MMIO_SPU_SizeBytes = MMIO_Expansion2_Offset - MMIO_SPU_Offset;
const MMIO_SPU_OffsetEnd = MMIO_SPU_Offset + MMIO_SPU_SizeBytes;
const MMIO_SPU = packed struct {
    _unused: u8192 = undefined,
};

const MMIO_Expansion2_Offset = 0x1f802000;
const MMIO_Expansion2_SizeBytes = MMIO_OffsetEnd - MMIO_Expansion2_Offset;
const MMIO_Expansion2 = packed struct {
    // FIXME Abusing the compiler for 1 bit here, one more and we hit the limit.
    // Really it should be 32768.
    _unused: u32767 = undefined,
};

// Known offsets, see https://psx-spx.consoledev.net/iomap/
// for more details.
const MMIO_Expansion1BaseAddress_Offset = 0x1f801000;
const MMIO_Expansion2BaseAddress_Offset = 0x1f801004;

const MMIO_0x1f801008_Offset = 0x1f801008;
const MMIO_0x1f80100c_Offset = 0x1f80100c;
const MMIO_0x1f801010_Offset = 0x1f801010;
const MMIO_0x1f801014_Offset = 0x1f801014;
const MMIO_0x1f801018_Offset = 0x1f801018;
const MMIO_0x1f80101c_Offset = 0x1f80101c;
const MMIO_0x1f801020_Offset = 0x1f801020;
const MMIO_0x1f801060_Offset = 0x1f801060;

const MMIO_InterruptStatus_Offset = 0x1f801070;
const MMIO_InterruptMask_Offset = 0x1f801074;

const MMIO_GPUREAD_G0_Offset = 0x1f801810;
const MMIO_GPUSTAT_G1_Offset = 0x1f801814;

const MMIO_MainVolume_Left = 0x1f801d80;
const MMIO_MainVolume_Right = 0x1f801d82;

const MMIO_UnknownDebug_Offset = 0x1f802041;

comptime {
    // Assert that the layout of the MMIO struct is correct, otherwise all hell breaks loose
    std.debug.assert(@offsetOf(MMIO, "memory_control1") == MMIO_MemoryControl1_Offset - MMIO_Offset);
    std.debug.assert(@offsetOf(MMIO, "io_ports") == MMIO_IOPorts_Offset - MMIO_Offset);
    std.debug.assert(@offsetOf(MMIO, "memory_control2") == MMIO_MemoryControl2_Offset - MMIO_Offset);
    std.debug.assert(@offsetOf(MMIO, "interrupt_control") == MMIO_IRQControl_Offset - MMIO_Offset);
    std.debug.assert(@offsetOf(MMIO, "dma") == dma.MMIO.Offset - MMIO_Offset);
    std.debug.assert(@offsetOf(MMIO, "timers") == MMIO_Timers_Offset - MMIO_Offset);
    std.debug.assert(@offsetOf(MMIO, "cdrom") == MMIO_CDROM_Offset - MMIO_Offset);
    std.debug.assert(@offsetOf(MMIO, "gpu") == MMIO_GPU_Offset - MMIO_Offset);
    std.debug.assert(@offsetOf(MMIO, "mdec") == MMIO_MDEC_Offset - MMIO_Offset);
    std.debug.assert(@offsetOf(MMIO, "spu") == MMIO_SPU_Offset - MMIO_Offset);
    std.debug.assert(@offsetOf(MMIO, "expansion2") == MMIO_Expansion2_Offset - MMIO_Offset);

    std.debug.assert(@sizeOf(MMIO_MemoryControl1) == MMIO_MemoryControl1_SizeBytes);
    std.debug.assert(@sizeOf(MMIO_IOPorts) == MMIO_IOPorts_SizeBytes);
    std.debug.assert(@sizeOf(MMIO_MemoryControl2) == MMIO_MemoryControl2_SizeBytes);
    std.debug.assert(@sizeOf(MMIO_IRQControl) == MMIO_IRQControl_SizeBytes);
    std.debug.assert(@sizeOf(MMIO_Timers) == MMIO_Timers_SizeBytes);
    std.debug.assert(@sizeOf(MMIO_CDROM) == MMIO_CDROM_SizeBytes);
    std.debug.assert(@sizeOf(MMIO_GPU) == MMIO_GPU_SizeBytes);
    std.debug.assert(@sizeOf(MMIO_MDEC) == MMIO_MDEC_SizeBytes);
    std.debug.assert(@sizeOf(MMIO_SPU) == MMIO_SPU_SizeBytes);
    std.debug.assert(@sizeOf(MMIO_Expansion2) == MMIO_Expansion2_SizeBytes);

    std.debug.assert(@sizeOf(MMIO) == MMIO_SizeBytes);
}
