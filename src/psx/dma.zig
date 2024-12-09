const std = @import("std");

const cpu = @import("cpu.zig");
const io = @import("cpu_io.zig");

// FIXME this might break if the type is not u32
pub fn load_mmio_generic(comptime T: type, psx: *cpu.PSXState, offset: u29) T {
    const type_bytes = @typeInfo(T).Int.bits / 8;

    const local_offset = offset - io.MMIO_Offset;
    const mmio_bytes = std.mem.asBytes(&psx.mmio);
    const type_slice = mmio_bytes[local_offset..][0..type_bytes];

    switch (offset) {
        MMIO_Offset...MMIO_OffsetEnd - 1 => {
            switch (offset) {
                MMIO_DMA_Control_Offset, MMIO_DMA_Interrupt_Offset => {
                    return std.mem.readInt(T, type_slice, .little);
                },
                else => {
                    if (cpu.enable_debug_print) {
                        std.debug.print("FIXME load ignored\n", .{});
                    }
                    return 0;
                },
            }
        },
        else => unreachable, // Should already be handled upstream
    }
}

pub fn store_mmio_generic(comptime T: type, psx: *cpu.PSXState, offset: u29, value: T) void {
    const type_bytes = @typeInfo(T).Int.bits / 8;

    const local_offset = offset - io.MMIO_Offset;
    const mmio_bytes = std.mem.asBytes(&psx.mmio);
    const type_slice = mmio_bytes[local_offset..][0..type_bytes];

    switch (offset) {
        MMIO_Offset...MMIO_OffsetEnd - 1 => {
            switch (offset) {
                MMIO_DMA_Control_Offset => {
                    std.mem.writeInt(T, type_slice, value, .little);
                },
                MMIO_DMA_Interrupt_Offset => {
                    const reset_irq_save = psx.mmio.dma.interrupt.reset_irq.raw;

                    // FIXME this might break if the type is not u32
                    std.mem.writeInt(T, type_slice, value, .little);

                    psx.mmio.dma.interrupt.zero_b6_14 = 0;
                    psx.mmio.dma.interrupt.reset_irq.raw = reset_irq_save & ~psx.mmio.dma.interrupt.reset_irq.raw;
                },
                else => if (cpu.enable_debug_print) {
                    std.debug.print("FIXME store ignored\n", .{});
                },
            }
        },
        else => unreachable, // Should already be handled upstream
    }
}

pub const MMIO_Offset = 0x1f801080;
pub const MMIO_SizeBytes = io.MMIO_Timers_Offset - MMIO_Offset;
pub const MMIO_OffsetEnd = MMIO_Offset + MMIO_SizeBytes;
pub const MMIO_DMA = packed struct {
    channel0: Channel = .{}, // MDECin  (RAM to MDEC)
    channel1: Channel = .{}, // MDECout (MDEC to RAM)
    channel2: Channel = .{}, // GPU (lists + image data)
    channel3: Channel = .{}, // SPU
    channel4: Channel = .{}, // CDROM (CDROM to RAM)
    channel5: Channel = .{}, // PIO (Expansion Port)
    channel6: Channel = .{}, // OTC (reverse clear OT) (GPU related)
    control: packed union { // DPCR - DMA Control register
        raw: u32,
        bits: packed struct {
            channel0_priority: u3, // 0-2   DMA0, MDECin  Priority      (0..7; 0=Highest, 7=Lowest)
            channel0_enable: u1, // 3     DMA0, MDECin  Master Enable (0=Disable, 1=Enable)
            channel1_priority: u3, // 4-6   DMA1, MDECout Priority      (0..7; 0=Highest, 7=Lowest)
            channel1_enable: u1, // 7     DMA1, MDECout Master Enable (0=Disable, 1=Enable)
            channel2_priority: u3, // 8-10  DMA2, GPU     Priority      (0..7; 0=Highest, 7=Lowest)
            channel2_enable: u1, // 11    DMA2, GPU     Master Enable (0=Disable, 1=Enable)
            channel3_priority: u3, // 12-14 DMA3, CDROM   Priority      (0..7; 0=Highest, 7=Lowest)
            channel3_enable: u1, // 15    DMA3, CDROM   Master Enable (0=Disable, 1=Enable)
            channel4_priority: u3, // 16-18 DMA4, SPU     Priority      (0..7; 0=Highest, 7=Lowest)
            channel4_enable: u1, // 19    DMA4, SPU     Master Enable (0=Disable, 1=Enable)
            channel5_priority: u3, // 20-22 DMA5, PIO     Priority      (0..7; 0=Highest, 7=Lowest)
            channel5_enable: u1, // 23    DMA5, PIO     Master Enable (0=Disable, 1=Enable)
            channel6_priority: u3, // 24-26 DMA6, OTC     Priority      (0..7; 0=Highest, 7=Lowest)
            channel6_enable: u1, // 27    DMA6, OTC     Master Enable (0=Disable, 1=Enable)
            _unknown1: u3, // 28-30 Unknown, Priority Offset or so? (R/W)
            _unknown2: u1, // 31    Unknown, no effect? (R/W)
        },
    } = .{ .raw = 0x07654321 },
    interrupt: packed struct { // DICR - DMA Interrupt register FIXME
        b0_5_unknown_rw: u6 = 0, // 0-5   Unknown  (read/write-able)
        zero_b6_14: u9 = 0, // 6-14  Not used (always zero)
        force_irq: u1 = 0, // 15    Force IRQ (sets bit31)                        (0=None, 1=Force Bit31=1)
        enable_irq: ChannelFlagBits = .{ .raw = 0 }, // 16-22 IRQ Enable setting bit24-30 upon DMA0..DMA6    (0=None, 1=Enable)
        enable_irq_master: u1 = 0, // 23    IRQ Enable setting bit31 when bit24-30=nonzero (0=None, 1=Enable)
        reset_irq: ChannelFlagBits = .{ .raw = 0 }, // 24-30 IRQ Flags for DMA0..DMA6    (Write 1 to reset) (0=None, 1=IRQ)
        is_irq_active: u1 = 0, // 31    IRQ Signal (0-to-1 triggers 1F801070h.bit3)    (0=None, 1=IRQ) (R)
    } = .{},
    _unused: u64 = undefined,

    const ChannelFlagBits = packed union {
        raw: u7,
        bits: packed struct {
            channel0: u1,
            channel1: u1,
            channel2: u1,
            channel3: u1,
            channel4: u1,
            channel5: u1,
            channel6: u1,
        },
    };

    const Channel = packed struct {
        // 1F801080h+N*10h - D#_MADR - DMA base address (Channel 0..6) (R/W)
        //
        // In SyncMode=0, the hardware doesn't update the MADR registers (it will contain the start address even during and after the transfer) (unless Chopping is enabled, in that case it does update MADR, same does probably also happen when getting interrupted by a higher priority DMA channel).
        // In SyncMode=1 and SyncMode=2, the hardware does update MADR (it will contain the start address of the currently transferred block; at transfer end, it'll hold the end-address in SyncMode=1, or the 00FFFFFFh end-code in SyncMode=2)
        // Note: Address bit0-1 are writeable, but any updated current/end addresses are word-aligned with bit0-1 forced to zero.
        base_address: packed struct {
            offset: u24 = undefined, // FIXME  0-23  Memory Address where the DMA will start reading from/writing to
            zero_b24_31: u8 = 0, //   24-31 Not used (always zero)
        } = .{},
        // 1F801084h+N*10h - D#_BCR - DMA Block Control (Channel 0..6) (R/W)
        //
        // BC/BS/BA can be in range 0001h..FFFFh (or 0=10000h). For BS, take care not to set the blocksize larger than the buffer of the corresponding unit can hold. (GPU and SPU both have a 16-word buffer). A larger blocksize means faster transfer.
        // SyncMode=1 decrements BA to zero, SyncMode=0 with chopping enabled decrements BC to zero (aside from that two cases, D#_BCR isn't changed during/after transfer).
        block_control: packed union {
            raw: u32, // FIXME
            mode0: packed struct { // For SyncMode=0 (ie. for OTC and CDROM):
                word_count: u16, //   0-15  BC    Number of words (0001h..FFFFh) (or 0=10000h words)
                _unused: u16, //   16-31 0     Not used (usually 0 for OTC, or 1 ("one block") for CDROM)
            },
            mode1: packed struct { // For SyncMode=1 (ie. for MDEC, SPU, and GPU-vram-data):
                block_size: u16, //   0-15  BS    Blocksize (words) ;for GPU/SPU max 10h, for MDEC max 20h
                block_count: u16, //   16-31 BA    Amount of blocks  ;ie. total length = BS*BA words
            },
            mode2: packed struct { // For SyncMode=2 (ie. for GPU-command-lists):
                _unused: u32, //   0-31  0     Not used (should be zero) (transfer ends at END-CODE in list)
            },
        } = .{ .raw = undefined }, // FIXME
        // 1F801088h+N*10h - D#_CHCR - DMA Channel Control (Channel 0..6) (R/W)
        //
        // The Start/Trigger bit is automatically cleared upon BEGIN of the transfer, this bit needs to be set only in SyncMode=0 (setting it in other SyncModes would force the first block to be transferred instantly without DRQ, which isn't desired).
        // The Start/Busy bit is automatically cleared upon COMPLETION of the transfer, this bit must be always set for all SyncModes when starting a transfer.
        // For DMA6/OTC there are some restrictions, D6_CHCR has only three read/write-able bits: Bit24,28,30. All other bits are read-only: Bit1 is always 1 (step=backward), and the other bits are always 0.
        channel_control: packed struct {
            transfer_direction: enum(u1) {
                ToRAM = 0,
                FromRAM = 1,
            },
            adress_step: enum(u1) {
                Inc4 = 0,
                Dec4 = 1,
            },
            zero_b2_7: u6 = 0,
            chopping_enable: u1, // 8 Chopping Enable       (0=Normal, 1=Chopping; run CPU during DMA gaps)
            sync_mode: enum(u2) { // 9-10 SyncMode, Transfer Synchronisation/Mode (0-3):
                StartImmediately = 0, //        0 Start immediately and transfer all at once (used for CDROM, OTC)
                SyncBlocksToDMARequests = 1, // 1 Sync blocks to DMA requests   (used for MDEC, SPU, and GPU-data)
                LinkedList = 2, //              2 Linked-List mode              (used for GPU-command-lists)
                Reserved = 3, //                3 Reserved                      (not used)
            },
            zero_b11_15: u5 = 0,
            chopping_dma_window: u3, // 16-18   Chopping DMA Window Size (1 SHL N words)
            zero_b19: u1 = 0,
            chopping_cpu_window: u3, // 20-22   Chopping CPU Window Size (1 SHL N clks)
            zero_b23: u1 = 0,
            status: enum(u1) { // 24 Start/Busy            (0=Stopped/Completed, 1=Start/Enable/Busy)
                StoppedOrCompleted = 0,
                StartOrEnableOrBusy = 1,
            },
            zero_b25_27: u3 = 0,
            start_or_trigger: u1, //   28      Start/Trigger         (0=Normal, 1=Manual Start; use for SyncMode=0)
            unknown_rw_b29: u1, //   29      Unknown (R/W) Pause?  (0=No, 1=Pause?)     (For SyncMode=0 only?)
            unknown_rw_b30: u1, //   30      Unknown (R/W)
            zero_b31: u1 = 0, //   31      Not used              (always zero)
        } = undefined, // FIXME
        _unused: u32 = undefined,
    };
};

const MMIO_DMA_Control_Offset = 0x1f8010f0;
const MMIO_DMA_Interrupt_Offset = 0x1f8010f4;
