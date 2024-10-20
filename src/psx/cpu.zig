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

const Expansion_ParallelPortOffset = 0x1f00_0084;

// 0x1f800000 0x9f800000 0xbf800000 1K Scratchpad
const Scratchpad_SizeBytes = 1024;
const Scratchpad_Offset = 0x1f800000;
const Scratchpad_OffsetEnd = Scratchpad_Offset + Scratchpad_SizeBytes;

// 0x1f801000 0x9f801000 0xbf801000 8K Hardware registers
const HWRegs_SizeBytes = 8 * 1024;
const HWRegs_Offset = 0x1f801000;
const HWRegs_OffsetEnd = HWRegs_Offset + HWRegs_SizeBytes;

// FIXME unclear where actually this lives
const HWRegs_Timers_SizeBytes = 3 * 16;
const HWRegs_Timers_Offset = 0x1f801100;
const HWRegs_Timers_OffsetEnd = HWRegs_Timers_Offset + HWRegs_Timers_SizeBytes;

const HWRegs_SPU_SizeBytes = 640;
const HWRegs_SPU_Offset = 0x1f801c00;
const HWRegs_SPU_OffsetEnd = HWRegs_SPU_Offset + HWRegs_SPU_SizeBytes;

const HWRegs_InterruptStatus_Offset = 0x1f801070;
const HWRegs_InterruptMask_Offset = 0x1f801074;
const HWRegs_UnknownDebug_Offset = 0x1f802041;

// 0x1fc00000 0x9fc00000 0xbfc00000 512K BIOS ROM
pub const BIOS_SizeBytes = 512 * 1024;
const BIOS_Offset = 0x1fc00000;
const BIOS_OffsetEnd = BIOS_Offset + BIOS_SizeBytes;

const CacheControl_Offset = 0x1ffe0130;

pub const PSXState = struct {
    registers: Registers = .{},
    branch: bool = false,
    delay_slot: bool = false,
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
    next_pc: u32 = 0xbfc00000 + 4, // Pipelined Program Counter
    current_instruction_pc: u32 = undefined,
    epc: u32 = undefined, // Exception Program Counter

    r_in: [32]u32 = undefined, // FIXME does it have an initial value?
    r_out: [32]u32 = undefined, // FIXME does it have an initial value?
    hi: u32 = undefined, // FIXME does it have an initial value?
    lo: u32 = undefined, // FIXME does it have an initial value?
    sr: SystemRegister = undefined, // FIXME does it have an initial value?
    cause: CauseRegister = undefined, // FIXME does it have an initial value?
    pending_load: ?struct { register: RegisterName, value: u32 } = null,
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

const SystemRegister = packed struct {
    // 0     IEc Current Interrupt Enable  (0=Disable, 1=Enable) ;rfe pops IEp here
    // 1     KUc Current Kernel/User Mode  (0=Kernel, 1=User)    ;rfe pops KUp here
    // 2     IEp Previous Interrupt Disable                      ;rfe pops IEo here
    // 3     KUp Previous Kernel/User Mode                       ;rfe pops KUo here
    // 4     IEo Old Interrupt Disable                       ;left unchanged by rfe
    // 5     KUo Old Kernel/User Mode                        ;left unchanged by rfe
    interrupt_stack: u6,
    // 6-7   -   Not used (zero)
    _unused_b6_7: u2,
    // 8-15  Im  8 bit interrupt mask fields. When set the corresponding
    //           interrupts are allowed to cause an exception.
    interrupt_mask: u8, // Im
    // 16    Isc Isolate Cache (0=No, 1=Isolate)
    //             When isolated, all load and store operations are targetted
    //             to the Data cache, and never the main memory.
    //             (Used by PSX Kernel, in combination with Port FFFE0130h)
    isolate_cache: u1, // Isc
    // 17    Swc Swapped cache mode (0=Normal, 1=Swapped)
    //             Instruction cache will act as Data cache and vice versa.
    //             Use only with Isc to access & invalidate Instr. cache entries.
    //             (Not used by PSX Kernel)
    swapped_cache: u1, // Swc
    // 18    PZ  When set cache parity bits are written as 0.
    cache_parity: u1,
    // 19    CM  Shows the result of the last load operation with the D-cache
    //           isolated. It gets set if the cache really contained data
    //           for the addressed memory location.
    cm: u1,
    // 20    PE  Cache parity error (Does not cause exception)
    cache_parity_error: u1,
    // 21    TS  TLB shutdown. Gets set if a programm address simultaneously
    //           matches 2 TLB entries.
    //           (initial value on reset allows to detect extended CPU version?)
    ts: u1,
    // 22    BEV Boot exception vectors in RAM/ROM (0=RAM/KSEG0, 1=ROM/KSEG1)
    bev: u1,
    // 23-24 -   Not used (zero)
    _unused_b23_24: u2,
    // 25    RE  Reverse endianness   (0=Normal endianness, 1=Reverse endianness)
    //             Reverses the byte order in which data is stored in
    //             memory. (lo-hi -> hi-lo)
    //             (Has affect only to User mode, not to Kernal mode) (?)
    //             (The bit doesn't exist in PSX ?)
    reverse_endianness: u1,
    // 26-27 -   Not used (zero)
    // 28    CU0 COP0 Enable (0=Enable only in Kernal Mode, 1=Kernal and User Mode)
    // 29    CU1 COP1 Enable (0=Disable, 1=Enable) (none such in PSX)
    // 30    CU2 COP2 Enable (0=Disable, 1=Enable) (GTE in PSX)
    // 31    CU3 COP3 Enable (0=Disable, 1=Enable) (none such in PSX)
    _unused_b26_31: u6,
};

const CauseRegister = packed struct {
    // 0-1   -      Not used (zero)
    _unused_b0_1: u2,
    // 2-6   Excode Describes what kind of exception occured:
    cause: ExceptionCause,
    // 7     -      Not used (zero)
    _unused_b7: u1,
    // 8-15  Ip     Interrupt pending field. Bit 8 and 9 are R/W, and
    //              contain the last value written to them. As long
    //              as any of the bits are set they will cause an
    //              interrupt if the corresponding bit is set in IM.
    interrupt_pending: u8,
    // 16-27 -      Not used (zero)
    _unused_b16_27: u12,
    // 28-29 CE     Opcode Bit26-27 (aka coprocessor number in case of COP opcodes)
    opcode: u2,
    // 30    -      Not used (zero) / Undoc: When BD=1, Branch condition (0=False)
    _unused_b30: u1,
    // 31    BD     Branch Delay (set when last exception points to the branch
    //              instruction instead of the instruction in the branch delay
    //              slot, where the exception occurred)
    branch_delay: u1,
};

pub const ExceptionCause = enum(u5) {
    INT = 0x00, // Interrupt
    MOD = 0x01, // Tlb modification (none such in PSX)
    TLBL = 0x02, // Tlb load         (none such in PSX)
    TLBS = 0x03, // Tlb store        (none such in PSX)
    AdEL = 0x04, // Address error, Data load or Instruction fetch
    // Address error, Data store
    // The address errors occur when attempting to read
    // outside of KUseg in user mode and when the address
    // is misaligned. (See also: BadVaddr register)
    AdES = 0x05,
    IBE = 0x06, // Bus error on Instruction fetch
    DBE = 0x07, // Bus error on Data load/store
    SysCall = 0x08, // Generated unconditionally by syscall instruction
    BP = 0x09, // Breakpoint - break instruction
    RI = 0x0A, // Reserved instruction
    CpU = 0x0B, // Coprocessor unusable
    Ov = 0x0C, // Arithmetic overflow
    _,
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
                Expansion_Offset...Expansion_OffsetEnd - 1 => |offset| switch (offset) {
                    Expansion_ParallelPortOffset => return 0xff,
                    else => unreachable,
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

    std.debug.assert(address.offset % 4 == 0);

    switch (address.mapping) {
        .Useg, .Seg0, .Seg1 => {
            switch (address.offset) {
                RAM_Offset...RAM_OffsetEnd - 1 => |offset| {
                    const local_offset = offset - RAM_Offset;
                    const u32_slice = psx.ram[local_offset..];
                    return std.mem.readInt(u32, u32_slice[0..4], .little);
                },
                HWRegs_Offset...HWRegs_OffsetEnd - 1 => |offset| {
                    switch (offset) {
                        HWRegs_InterruptMask_Offset, HWRegs_InterruptStatus_Offset => {
                            std.debug.print("FIXME Interrupt load ignored\n", .{});
                            return 0;
                        },
                        else => unreachable,
                    }
                },
                BIOS_Offset...BIOS_OffsetEnd - 1 => |offset| {
                    const local_offset = offset - BIOS_Offset;
                    const u32_slice = psx.bios[local_offset..];
                    return std.mem.readInt(u32, u32_slice[0..4], .little);
                },
                else => unreachable,
            }
        },
        .Seg2 => {
            switch (address.offset) {
                CacheControl_Offset => {
                    std.debug.print("FIXME load ignored at cache control offset 0x{x:0>8}\n", .{address_u32});
                    return 0;
                },
                else => unreachable,
            }
        },
    }
}

pub fn store_mem_u8(psx: *PSXState, address_u32: u32, value: u8) void {
    std.debug.print("store addr: 0x{x:0>8}\n", .{address_u32});
    std.debug.print("store value: 0x{x:0>2}\n", .{value});

    if (psx.registers.sr.isolate_cache == 1) {
        std.debug.print("FIXME store ignored because of cache isolation\n", .{});
        return;
    }

    const address: PSXAddress = @bitCast(address_u32);

    switch (address.mapping) {
        .Useg, .Seg0, .Seg1 => {
            switch (address.offset) {
                RAM_Offset...RAM_OffsetEnd - 1 => |offset| {
                    const local_offset = offset - RAM_Offset;
                    psx.ram[local_offset] = value;
                },
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
                CacheControl_Offset => {
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

    if (psx.registers.sr.isolate_cache == 1) {
        std.debug.print("FIXME store ignored because of cache isolation\n", .{});
        return;
    }

    const address: PSXAddress = @bitCast(address_u32);

    std.debug.assert(address.offset % 2 == 0);

    switch (address.mapping) {
        .Useg, .Seg0, .Seg1 => {
            switch (address.offset) {
                RAM_Offset...RAM_OffsetEnd - 1 => |offset| {
                    const local_offset = offset - RAM_Offset;
                    const u16_slice = psx.ram[local_offset..];
                    std.mem.writeInt(u16, u16_slice[0..2], value, .little);
                },
                HWRegs_Offset...HWRegs_OffsetEnd - 1 => |offset| {
                    switch (offset) {
                        HWRegs_Timers_Offset...HWRegs_Timers_OffsetEnd - 1 => {
                            std.debug.print("FIXME Timer store ignored\n", .{});
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
                CacheControl_Offset => {
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

    if (psx.registers.sr.isolate_cache == 1) {
        std.debug.print("FIXME store ignored because of cache isolation\n", .{});
        return;
    }

    const address: PSXAddress = @bitCast(address_u32);

    std.debug.assert(address.offset % 4 == 0);

    switch (address.mapping) {
        .Useg, .Seg0, .Seg1 => {
            switch (address.offset) {
                RAM_Offset...RAM_OffsetEnd - 1 => |offset| {
                    const local_offset = offset - RAM_Offset;
                    const u32_slice = psx.ram[local_offset..];
                    std.mem.writeInt(u32, u32_slice[0..4], value, .little);
                },
                HWRegs_Offset...HWRegs_OffsetEnd - 1 => |offset| {
                    const local_offset: u13 = @intCast(offset - HWRegs_Offset);
                    const local_offset_typed: HWRegOffsets = @enumFromInt(local_offset);
                    switch (local_offset_typed) {
                        .Expansion1BaseAddress, .Expansion2BaseAddress => {
                            std.debug.print("FIXME store ignored to expansion register 0x{x:0>8}\n", .{local_offset});
                        },
                        .InterruptStatus, .InterruptMask => {
                            std.debug.print("FIXME store ignored to IRQ register 0x{x:0>8}\n", .{local_offset});
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
                CacheControl_Offset => {
                    std.debug.print("FIXME store ignored at offset 0x{x:0>8}\n", .{address_u32});
                },
                else => unreachable,
            }
        },
    }
}

pub fn execute(psx: *PSXState) void {
    while (true) {
        psx.registers.current_instruction_pc = psx.registers.pc;

        if (psx.registers.pc % 4 != 0) {
            execution.execute_exception(psx, .AdEL);
            continue;
        }

        const op_code = load_mem_u32(psx, psx.registers.pc);

        psx.registers.pc = psx.registers.next_pc;
        psx.registers.next_pc +%= 4;

        // Execute any pending memory loads
        if (psx.registers.pending_load) |pending_load| {
            execution.store_reg(&psx.registers, pending_load.register, pending_load.value);
            psx.registers.pending_load = null;
        }

        const instruction = instructions.decode_instruction(op_code);

        debug.print_instruction(op_code, instruction);

        psx.delay_slot = psx.branch;
        psx.branch = false;

        execution.execute_instruction(psx, instruction);

        std.mem.copyForwards(u32, &psx.registers.r_in, &psx.registers.r_out);
    }
}

const HWRegOffsets = enum(u13) {
    Expansion1BaseAddress = 0,
    Expansion2BaseAddress = 4,
    InterruptStatus = HWRegs_InterruptStatus_Offset - HWRegs_Offset,
    InterruptMask = HWRegs_InterruptMask_Offset - HWRegs_Offset,
    _,
};
