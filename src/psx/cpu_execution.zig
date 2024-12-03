const std = @import("std");

const cpu = @import("cpu.zig");
const PSXState = cpu.PSXState;
const Registers = cpu.Registers;

const instructions = @import("cpu_instructions.zig");
const debug = @import("cpu_debug.zig");
const io = @import("cpu_io.zig");

pub fn step(psx: *PSXState) void {
    psx.registers.current_instruction_pc = psx.registers.pc;

    if (psx.registers.pc % 4 != 0) {
        execute_exception(psx, .AdEL);
        return;
    }

    const op_code = io.load_mem_u32(psx, psx.registers.pc);

    psx.registers.pc = psx.registers.next_pc;
    psx.registers.next_pc +%= 4;

    // Execute any pending memory loads
    if (psx.registers.pending_load) |pending_load| {
        store_reg(&psx.registers, pending_load.register, pending_load.value);
        psx.registers.pending_load = null;
    }

    const instruction = instructions.decode_instruction(op_code);

    if (cpu.enable_debug_print) {
        debug.print_instruction(instruction);
    }

    psx.delay_slot = psx.branch;
    psx.branch = false;

    execute_instruction(psx, instruction);

    std.mem.copyForwards(u32, &psx.registers.r_in, &psx.registers.r_out);
}

fn execute_instruction(psx: *PSXState, instruction: instructions.Instruction) void {
    switch (instruction) {
        .sll => |i| execute_sll(psx, i),
        .srl => |i| execute_srl(psx, i),
        .sra => |i| execute_sra(psx, i),
        .sllv => |i| execute_sllv(psx, i),
        .srlv => |i| execute_srlv(psx, i),
        .srav => |i| execute_srav(psx, i),
        .jr => |i| execute_jr(psx, i),
        .jalr => |i| execute_jalr(psx, i),
        .syscall => execute_syscall(psx),
        .break_ => execute_break(psx),
        .mfhi => |i| execute_mfhi(psx, i),
        .mthi => |i| execute_mthi(psx, i),
        .mflo => |i| execute_mflo(psx, i),
        .mtlo => |i| execute_mtlo(psx, i),
        .rfe => execute_rfe(psx),
        .cop1 => execute_cop1(psx),
        .cop2 => execute_cop2(psx),
        .cop3 => execute_cop3(psx),
        .mult => |i| execute_mult(psx, i),
        .multu => |i| execute_multu(psx, i),
        .div => |i| execute_div(psx, i),
        .divu => |i| execute_divu(psx, i),
        .add => |i| execute_add(psx, i),
        .addu => |i| execute_addu(psx, i),
        .sub => |i| execute_sub(psx, i),
        .subu => |i| execute_subu(psx, i),
        .and_ => |i| execute_and(psx, i),
        .or_ => |i| execute_or(psx, i),
        .xor => |i| execute_xor(psx, i),
        .nor => |i| execute_nor(psx, i),
        .slt => |i| execute_slt(psx, i),
        .sltu => |i| execute_sltu(psx, i),
        .b_cond_z => |i| execute_b_cond_z(psx, i),
        .j => |i| execute_j(psx, i),
        .jal => |i| execute_jal(psx, i),
        .beq => |i| execute_beq(psx, i),
        .bne => |i| execute_bne(psx, i),
        .blez => |i| execute_blez(psx, i),
        .bgtz => |i| execute_bgtz(psx, i),
        .mfc => |i| execute_mfc(psx, i),
        .cfc => |i| execute_cfc(psx, i),
        .mtc => |i| execute_mtc(psx, i),
        .ctc => |i| execute_ctc(psx, i),
        .bcn => |i| execute_bcn(psx, i),
        .addi => |i| execute_addi(psx, i),
        .addiu => |i| execute_addiu(psx, i),
        .slti => |i| execute_slti(psx, i),
        .sltiu => |i| execute_sltiu(psx, i),
        .andi => |i| execute_andi(psx, i),
        .ori => |i| execute_ori(psx, i),
        .xori => |i| execute_xori(psx, i),
        .lui => |i| execute_lui(psx, i),

        .lb => |i| execute_lb(psx, i),
        .lh => |i| execute_lh(psx, i),
        .lwl => |i| execute_lwl(psx, i),
        .lw => |i| execute_lw(psx, i),
        .lbu => |i| execute_lbu(psx, i),
        .lhu => |i| execute_lhu(psx, i),
        .lwr => |i| execute_lwr(psx, i),
        .sb => |i| execute_sb(psx, i),
        .sh => |i| execute_sh(psx, i),
        .swl => |i| execute_swl(psx, i),
        .sw => |i| execute_sw(psx, i),
        .swr => |i| execute_swr(psx, i),

        .lwc => |i| execute_lwc(psx, i),
        .swc => |i| execute_swc(psx, i),

        .invalid => execute_reserved_instruction(psx),
    }
}

fn load_reg(registers: Registers, register_name: cpu.RegisterName) u32 {
    const value = switch (register_name) {
        .zero => 0,
        else => registers.r_in[@intFromEnum(register_name)],
    };

    if (cpu.enable_debug_print) {
        std.debug.print("reg load 0x{x:0>8} from {}\n", .{ value, register_name });
    }

    return value;
}

fn store_reg(registers: *Registers, register_name: cpu.RegisterName, value: u32) void {
    if (cpu.enable_debug_print) {
        std.debug.print("reg store 0x{x:0>8} in {}\n", .{ value, register_name });
    }

    switch (register_name) {
        .zero => {},
        else => registers.r_out[@intFromEnum(register_name)] = value,
    }
}

fn execute_sll(psx: *PSXState, instruction: instructions.sll) void {
    const value = load_reg(psx.registers, instruction.rt);

    const result = value << instruction.shift_imm;

    store_reg(&psx.registers, instruction.rd, result);
}

fn execute_srl(psx: *PSXState, instruction: instructions.srl) void {
    const value = load_reg(psx.registers, instruction.rt);

    const result = value >> instruction.shift_imm;

    store_reg(&psx.registers, instruction.rd, result);
}

fn execute_sra(psx: *PSXState, instruction: instructions.sra) void {
    const value: i32 = @bitCast(load_reg(psx.registers, instruction.rt));

    // Sign-extending
    const result = value >> instruction.shift_imm;

    store_reg(&psx.registers, instruction.rd, @bitCast(result));
}

fn execute_sllv(psx: *PSXState, instruction: instructions.sllv) void {
    const value = load_reg(psx.registers, instruction.rt);
    const shift: u5 = @truncate(load_reg(psx.registers, instruction.rs));

    const result = value << shift;

    store_reg(&psx.registers, instruction.rd, result);
}

fn execute_srlv(psx: *PSXState, instruction: instructions.srlv) void {
    const value = load_reg(psx.registers, instruction.rt);
    const shift: u5 = @truncate(load_reg(psx.registers, instruction.rs));

    const result = value >> shift;

    store_reg(&psx.registers, instruction.rd, result);
}

fn execute_srav(psx: *PSXState, instruction: instructions.srav) void {
    const value: i32 = @bitCast(load_reg(psx.registers, instruction.rt));
    const shift: u5 = @truncate(load_reg(psx.registers, instruction.rs));

    // Sign-extending
    const result = value >> shift;

    store_reg(&psx.registers, instruction.rd, @bitCast(result));
}

fn execute_jr(psx: *PSXState, instruction: instructions.jr) void {
    const address = load_reg(psx.registers, instruction.rs);
    execute_generic_jump(psx, address);
}

fn execute_jalr(psx: *PSXState, instruction: instructions.jalr) void {
    store_reg(&psx.registers, instruction.rd, psx.registers.next_pc);

    const address = load_reg(psx.registers, instruction.rs);
    execute_generic_jump(psx, address);
}

fn execute_syscall(psx: *PSXState) void {
    execute_exception(psx, .SysCall);
}

fn execute_break(psx: *PSXState) void {
    execute_exception(psx, .BP);
}

fn execute_mult(psx: *PSXState, instruction: instructions.mult) void {
    const a: i64 = @as(i32, @bitCast(load_reg(psx.registers, instruction.rs)));
    const b: i64 = @as(i32, @bitCast(load_reg(psx.registers, instruction.rt)));

    const result: u64 = @bitCast(a * b);

    psx.registers.hi = @truncate(result >> 32);
    psx.registers.lo = @truncate(result);
}

fn execute_multu(psx: *PSXState, instruction: instructions.multu) void {
    const a: u64 = load_reg(psx.registers, instruction.rs);
    const b: u64 = load_reg(psx.registers, instruction.rt);

    const result = a * b;

    psx.registers.hi = @truncate(result >> 32);
    psx.registers.lo = @truncate(result);
}

fn execute_div(psx: *PSXState, instruction: instructions.div) void {
    const numerator_u32 = load_reg(psx.registers, instruction.rs);
    const numerator: i32 = @bitCast(numerator_u32);
    const divisor: i32 = @bitCast(load_reg(psx.registers, instruction.rt));

    if (divisor == 0) {
        // Division by zero
        psx.registers.hi = @bitCast(numerator);
        psx.registers.lo = if (numerator < 0) 1 else 0xff_ff_ff_ff;
    } else if (numerator_u32 == 0x80_00_00_00 and divisor == -1) {
        // Result can't be represented
        psx.registers.hi = 0;
        psx.registers.lo = numerator_u32;
    } else {
        psx.registers.hi = @bitCast(@rem(numerator, divisor));
        psx.registers.lo = @bitCast(@divTrunc(numerator, divisor));
    }
}

fn execute_divu(psx: *PSXState, instruction: instructions.divu) void {
    const numerator = load_reg(psx.registers, instruction.rs);
    const divisor = load_reg(psx.registers, instruction.rt);

    if (divisor == 0) {
        // Division by zero
        psx.registers.hi = numerator;
        psx.registers.lo = 0xff_ff_ff_ff;
    } else {
        psx.registers.hi = @rem(numerator, divisor);
        psx.registers.lo = @divTrunc(numerator, divisor);
    }
}

fn execute_add(psx: *PSXState, instruction: instructions.add) void {
    const value_s = load_reg(psx.registers, instruction.rs);
    const value_t = load_reg(psx.registers, instruction.rt);

    const result, const overflow = execute_generic_add_with_overflow(value_s, @bitCast(value_t));

    if (overflow) {
        execute_exception(psx, .Ov);
    } else {
        store_reg(&psx.registers, instruction.rd, result);
    }
}

fn execute_addu(psx: *PSXState, instruction: instructions.addu) void {
    const value_s = load_reg(psx.registers, instruction.rs);
    const value_t = load_reg(psx.registers, instruction.rt);

    const result = value_s +% value_t;

    store_reg(&psx.registers, instruction.rd, result);
}

fn execute_sub(psx: *PSXState, instruction: instructions.sub) void {
    const value_s = load_reg(psx.registers, instruction.rs);
    const value_t = load_reg(psx.registers, instruction.rt);

    _ = value_s;
    _ = value_t;
    unreachable;
    // store_reg(&psx.registers, instruction.rd, value_s | value_t);
}

fn execute_subu(psx: *PSXState, instruction: instructions.subu) void {
    const value_s = load_reg(psx.registers, instruction.rs);
    const value_t = load_reg(psx.registers, instruction.rt);

    const result = value_s -% value_t;

    store_reg(&psx.registers, instruction.rd, result);
}

fn execute_and(psx: *PSXState, instruction: instructions.and_) void {
    const value_s = load_reg(psx.registers, instruction.rs);
    const value_t = load_reg(psx.registers, instruction.rt);

    store_reg(&psx.registers, instruction.rd, value_s & value_t);
}

fn execute_or(psx: *PSXState, instruction: instructions.or_) void {
    const value_s = load_reg(psx.registers, instruction.rs);
    const value_t = load_reg(psx.registers, instruction.rt);

    store_reg(&psx.registers, instruction.rd, value_s | value_t);
}

fn execute_xor(psx: *PSXState, instruction: instructions.xor) void {
    const value_s = load_reg(psx.registers, instruction.rs);
    const value_t = load_reg(psx.registers, instruction.rt);

    store_reg(&psx.registers, instruction.rd, value_s ^ value_t);
}

fn execute_nor(psx: *PSXState, instruction: instructions.nor) void {
    const value_s = load_reg(psx.registers, instruction.rs);
    const value_t = load_reg(psx.registers, instruction.rt);

    store_reg(&psx.registers, instruction.rd, ~(value_s | value_t));
}

fn execute_slt(psx: *PSXState, instruction: instructions.slt) void {
    const value_s: i32 = @bitCast(load_reg(psx.registers, instruction.rs));
    const value_t: i32 = @bitCast(load_reg(psx.registers, instruction.rt));

    const result: u32 = if (value_s < value_t) 1 else 0;

    store_reg(&psx.registers, instruction.rd, result);
}

fn execute_sltu(psx: *PSXState, instruction: instructions.sltu) void {
    const value_s = load_reg(psx.registers, instruction.rs);
    const value_t = load_reg(psx.registers, instruction.rt);

    const result: u32 = if (value_s < value_t) 1 else 0;

    store_reg(&psx.registers, instruction.rd, result);
}

fn execute_b_cond_z(psx: *PSXState, instruction: instructions.b_cond_z) void {
    const value_s: i32 = @bitCast(load_reg(psx.registers, instruction.rs));

    var test_value = value_s < 0;

    // Flip test if needed
    test_value = test_value != instruction.test_greater;

    if (test_value) {
        if (instruction.link) {
            store_reg(&psx.registers, cpu.RegisterName.ra, psx.registers.next_pc);
        }

        execute_generic_branch(psx, instruction.rel_offset);
    }
}

fn execute_j(psx: *PSXState, instruction: instructions.j) void {
    const address = (psx.registers.next_pc & 0xf0_00_00_00) | instruction.offset;
    execute_generic_jump(psx, address);
}

fn execute_jal(psx: *PSXState, instruction: instructions.jal) void {
    store_reg(&psx.registers, cpu.RegisterName.ra, psx.registers.next_pc);

    const address = (psx.registers.next_pc & 0xf0_00_00_00) | instruction.offset;
    execute_generic_jump(psx, address);
}

fn execute_beq(psx: *PSXState, instruction: instructions.beq) void {
    const value_s = load_reg(psx.registers, instruction.rs);
    const value_t = load_reg(psx.registers, instruction.rt);

    if (value_s == value_t) {
        execute_generic_branch(psx, instruction.rel_offset);
    }
}

fn execute_bne(psx: *PSXState, instruction: instructions.bne) void {
    const value_s = load_reg(psx.registers, instruction.rs);
    const value_t = load_reg(psx.registers, instruction.rt);

    if (value_s != value_t) {
        execute_generic_branch(psx, instruction.rel_offset);
    }
}

fn execute_blez(psx: *PSXState, instruction: instructions.blez) void {
    const value_s: i32 = @bitCast(load_reg(psx.registers, instruction.rs));

    if (value_s <= 0) {
        execute_generic_branch(psx, instruction.rel_offset);
    }
}

fn execute_bgtz(psx: *PSXState, instruction: instructions.bgtz) void {
    const value_s: i32 = @bitCast(load_reg(psx.registers, instruction.rs));

    if (value_s > 0) {
        execute_generic_branch(psx, instruction.rel_offset);
    }
}

fn execute_mfc(psx: *PSXState, instruction: instructions.mtc) void {
    std.debug.assert(instruction.cop_index == 0);

    const value: u32 = switch (instruction.target) {
        .BPC, .BDA, .JUMPDEST, .DCIC, .BadVaddr, .BDAM, .BPCM, .PRID => unreachable,
        .SR => @bitCast(psx.registers.sr),
        .CAUSE => @bitCast(psx.registers.cause),
        .EPC => psx.registers.epc,
        _ => unreachable,
    };

    psx.registers.pending_load = .{ .register = instruction.cpu_rs, .value = @bitCast(value) };
}

fn execute_cfc(psx: *PSXState, instruction: instructions.cfc) void {
    _ = psx;
    _ = instruction;
    unreachable;
}

fn execute_mtc(psx: *PSXState, instruction: instructions.mtc) void {
    std.debug.assert(instruction.cop_index == 0);

    const value = load_reg(psx.registers, instruction.cpu_rs);

    switch (instruction.target) {
        .BPC, .BDA, .JUMPDEST, .DCIC, .BadVaddr, .BDAM, .BPCM, .PRID => {
            if (cpu.enable_debug_print) {
                std.debug.print("FIXME mtc0 target write ignored\n", .{});
            }
        },
        .SR => psx.registers.sr = @bitCast(value),
        .CAUSE => switch (value) {
            0 => {
                if (cpu.enable_debug_print) {
                    std.debug.print("FIXME mtc0 target write ignored\n", .{});
                }
            },
            else => unreachable,
        },
        .EPC => unreachable,
        _ => unreachable,
    }
}

fn execute_ctc(psx: *PSXState, instruction: instructions.ctc) void {
    _ = psx;
    _ = instruction;
    unreachable;
}

fn execute_bcn(psx: *PSXState, instruction: instructions.bcn) void {
    _ = psx;
    _ = instruction;
    unreachable;
}

fn execute_addi(psx: *PSXState, instruction: instructions.addi) void {
    const value_s = load_reg(psx.registers, instruction.rs);

    const result, const overflow = execute_generic_add_with_overflow(value_s, instruction.imm_i16);

    if (overflow) {
        execute_exception(psx, .Ov);
    } else {
        store_reg(&psx.registers, instruction.rt, result);
    }
}

fn execute_addiu(psx: *PSXState, instruction: instructions.addiu) void {
    const value_s = load_reg(psx.registers, instruction.rs);

    const result = wrapping_add_u32_i32(value_s, instruction.imm_i16);

    store_reg(&psx.registers, instruction.rt, result);
}

fn execute_slti(psx: *PSXState, instruction: instructions.slti) void {
    const value_s: i32 = @bitCast(load_reg(psx.registers, instruction.rs));

    const result: u32 = if (value_s < instruction.imm_i16) 1 else 0;

    store_reg(&psx.registers, instruction.rt, result);
}

fn execute_sltiu(psx: *PSXState, instruction: instructions.sltiu) void {
    const value_s = load_reg(psx.registers, instruction.rs);

    const result: u32 = if (value_s < instruction.imm_i16) 1 else 0;

    store_reg(&psx.registers, instruction.rt, result);
}

fn execute_andi(psx: *PSXState, instruction: instructions.andi) void {
    const value = load_reg(psx.registers, instruction.rs);

    store_reg(&psx.registers, instruction.rt, value & instruction.imm_u16);
}

fn execute_ori(psx: *PSXState, instruction: instructions.ori) void {
    const value = load_reg(psx.registers, instruction.rs);

    store_reg(&psx.registers, instruction.rt, value | instruction.imm_u16);
}

fn execute_xori(psx: *PSXState, instruction: instructions.xori) void {
    const value = load_reg(psx.registers, instruction.rs);

    store_reg(&psx.registers, instruction.rt, value ^ instruction.imm_u16);
}

fn execute_lui(psx: *PSXState, instruction: instructions.lui) void {
    const value = @as(u32, instruction.imm_u16) << 16;

    store_reg(&psx.registers, instruction.rt, value);
}

fn execute_lb(psx: *PSXState, instruction: instructions.lb) void {
    const address_base = load_reg(psx.registers, instruction.rs);
    const address = wrapping_add_u32_i32(address_base, instruction.imm_i16);

    const value: i8 = @bitCast(io.load_mem_u8(psx, address));
    const value_sign_extended: i32 = value;

    psx.registers.pending_load = .{ .register = instruction.rt, .value = @bitCast(value_sign_extended) };
}

fn execute_lbu(psx: *PSXState, instruction: instructions.lbu) void {
    const address_base = load_reg(psx.registers, instruction.rs);
    const address = wrapping_add_u32_i32(address_base, instruction.imm_i16);

    const value: u8 = io.load_mem_u8(psx, address);

    psx.registers.pending_load = .{ .register = instruction.rt, .value = value };
}

fn execute_lh(psx: *PSXState, instruction: instructions.lh) void {
    const address_base = load_reg(psx.registers, instruction.rs);
    const address = wrapping_add_u32_i32(address_base, instruction.imm_i16);

    if (address % 2 == 0) {
        const value: i16 = @bitCast(io.load_mem_u16(psx, address));
        const value_sign_extended: i32 = value;

        psx.registers.pending_load = .{ .register = instruction.rt, .value = @bitCast(value_sign_extended) };
    } else {
        execute_exception(psx, .AdEL);
    }
}

fn execute_lhu(psx: *PSXState, instruction: instructions.lhu) void {
    const address_base = load_reg(psx.registers, instruction.rs);
    const address = wrapping_add_u32_i32(address_base, instruction.imm_i16);

    if (address % 2 == 0) {
        const value = io.load_mem_u16(psx, address);

        psx.registers.pending_load = .{ .register = instruction.rt, .value = value };
    } else {
        execute_exception(psx, .AdEL);
    }
}

fn execute_lw(psx: *PSXState, instruction: instructions.lw) void {
    const address_base = load_reg(psx.registers, instruction.rs);
    const address = wrapping_add_u32_i32(address_base, instruction.imm_i16);

    if (address % 4 == 0) {
        const value = io.load_mem_u32(psx, address);

        psx.registers.pending_load = .{ .register = instruction.rt, .value = value };
    } else {
        execute_exception(psx, .AdEL);
    }
}

fn execute_lwl(psx: *PSXState, instruction: instructions.lwl) void {
    const address_base = load_reg(psx.registers, instruction.rs);
    const address = wrapping_add_u32_i32(address_base, instruction.imm_i16);
    const address_aligned = address & ~@as(u32, 0b11);

    const load_value = io.load_mem_u32(psx, address_aligned);

    const previous_value = if (psx.registers.pending_load) |pending_load|
        if (pending_load.is_unaligned) pending_load.value else 0
    else
        0;

    const result = switch (address % 4) {
        0 => previous_value & 0x00_ff_ff_ff | load_value << 24,
        1 => previous_value & 0x00_00_ff_ff | load_value << 16,
        2 => previous_value & 0x00_00_00_ff | load_value << 8,
        3 => previous_value & 0x00_00_00_00 | load_value << 0,
        else => unreachable,
    };

    psx.registers.pending_load = .{ .register = instruction.rt, .value = result };
}

fn execute_lwr(psx: *PSXState, instruction: instructions.lwr) void {
    const address_base = load_reg(psx.registers, instruction.rs);
    const address = wrapping_add_u32_i32(address_base, instruction.imm_i16);
    const address_aligned = address & ~@as(u32, 0b11);

    const load_value = io.load_mem_u32(psx, address_aligned);

    const previous_value = if (psx.registers.pending_load) |pending_load|
        if (pending_load.is_unaligned) pending_load.value else 0
    else
        0;

    const result = switch (address % 4) {
        0 => previous_value & 0x00_00_00_00 | load_value >> 0,
        1 => previous_value & 0xff_00_00_00 | load_value >> 8,
        2 => previous_value & 0xff_ff_00_00 | load_value >> 16,
        3 => previous_value & 0xff_ff_ff_00 | load_value >> 24,
        else => unreachable,
    };

    psx.registers.pending_load = .{ .register = instruction.rt, .value = result };
}

fn execute_sb(psx: *PSXState, instruction: instructions.sb) void {
    const value = load_reg(psx.registers, instruction.rt);

    const address_base = load_reg(psx.registers, instruction.rs);
    const address = wrapping_add_u32_i32(address_base, instruction.imm_i16);

    io.store_mem_u8(psx, address, @truncate(value));
}

fn execute_sh(psx: *PSXState, instruction: instructions.sh) void {
    const value = load_reg(psx.registers, instruction.rt);

    const address_base = load_reg(psx.registers, instruction.rs);
    const address = wrapping_add_u32_i32(address_base, instruction.imm_i16);

    if (address % 2 == 0) {
        io.store_mem_u16(psx, address, @truncate(value));
    } else {
        execute_exception(psx, .AdES);
    }
}

fn execute_sw(psx: *PSXState, instruction: instructions.sw) void {
    const value = load_reg(psx.registers, instruction.rt);

    const address_base = load_reg(psx.registers, instruction.rs);
    const address = wrapping_add_u32_i32(address_base, instruction.imm_i16);

    if (address % 4 == 0) {
        io.store_mem_u32(psx, address, value);
    } else {
        execute_exception(psx, .AdES);
    }
}

fn execute_swl(psx: *PSXState, instruction: instructions.swl) void {
    const value = load_reg(psx.registers, instruction.rt);

    const address_base = load_reg(psx.registers, instruction.rs);
    const address = wrapping_add_u32_i32(address_base, instruction.imm_i16);
    const address_aligned = address & ~@as(u32, 0b11);

    const previous_value = io.load_mem_u32(psx, address_aligned);

    const result = switch (address % 4) {
        0 => previous_value & 0xff_ff_ff_00 | value >> 24,
        1 => previous_value & 0xff_ff_00_00 | value >> 16,
        2 => previous_value & 0xff_00_00_00 | value >> 8,
        3 => previous_value & 0x00_00_00_00 | value >> 0,
        else => unreachable,
    };

    io.store_mem_u32(psx, address_aligned, result);
}

fn execute_swr(psx: *PSXState, instruction: instructions.swr) void {
    const value = load_reg(psx.registers, instruction.rt);

    const address_base = load_reg(psx.registers, instruction.rs);
    const address = wrapping_add_u32_i32(address_base, instruction.imm_i16);
    const address_aligned = address & ~@as(u32, 0b11);

    const previous_value = io.load_mem_u32(psx, address_aligned);

    const result = switch (address % 4) {
        0 => previous_value & 0x00_00_00_00 | value << 0,
        1 => previous_value & 0x00_00_00_ff | value << 8,
        2 => previous_value & 0x00_00_ff_ff | value << 16,
        3 => previous_value & 0x00_ff_ff_ff | value << 24,
        else => unreachable,
    };

    io.store_mem_u32(psx, address_aligned, result);
}

fn execute_mfhi(psx: *PSXState, instruction: instructions.mfhi) void {
    store_reg(&psx.registers, instruction.rd, psx.registers.hi);
}

fn execute_mthi(psx: *PSXState, instruction: instructions.mthi) void {
    psx.registers.hi = load_reg(psx.registers, instruction.rs);
}

fn execute_mflo(psx: *PSXState, instruction: instructions.mflo) void {
    store_reg(&psx.registers, instruction.rd, psx.registers.lo);
}

fn execute_mtlo(psx: *PSXState, instruction: instructions.mtlo) void {
    psx.registers.lo = load_reg(psx.registers, instruction.rs);
}

fn execute_rfe(psx: *PSXState) void {
    psx.registers.sr.interrupt_stack >>= 2;
}

fn execute_cop1(psx: *PSXState) void {
    execute_exception(psx, .CpU);
}

fn execute_cop2(psx: *PSXState) void {
    _ = psx;
    unreachable;
}

fn execute_cop3(psx: *PSXState) void {
    execute_exception(psx, .CpU);
}

fn execute_lwc(psx: *PSXState, instruction: instructions.lwc) void {
    if (instruction.cop_index == 2) {
        unreachable;
    } else {
        execute_exception(psx, .CpU);
    }
}

fn execute_swc(psx: *PSXState, instruction: instructions.swc) void {
    if (instruction.cop_index == 2) {
        unreachable;
    } else {
        execute_exception(psx, .CpU);
    }
}

fn execute_generic_add_with_overflow(lhs: u32, rhs: i32) struct { u32, bool } {
    var result: u32 = undefined;
    var overflow: u1 = undefined;

    if (rhs >= 0) {
        // NOTE: using two's-complement to ignore signedness
        result, overflow = @addWithOverflow(lhs, @as(u32, @intCast(rhs)));
    } else {
        result, overflow = @subWithOverflow(lhs, @as(u32, @intCast(-rhs)));
    }

    return .{ result, overflow != 0 };
}

fn execute_reserved_instruction(psx: *PSXState) void {
    execute_exception(psx, .RI);
}

fn execute_generic_jump(psx: *PSXState, address: u32) void {
    psx.branch = true;
    psx.registers.next_pc = address;
}

fn execute_generic_branch(psx: *PSXState, offset: i32) void {
    psx.branch = true;
    psx.registers.next_pc = wrapping_add_u32_i32(psx.registers.pc, offset);
}

fn execute_exception(psx: *PSXState, cause: cpu.ExceptionCause) void {
    psx.registers.cause = @bitCast(@as(u32, 0));
    psx.registers.cause.cause = cause;
    psx.registers.cause.branch_delay = if (psx.delay_slot) 1 else 0;

    psx.registers.epc = psx.registers.current_instruction_pc;

    if (psx.delay_slot) {
        psx.registers.epc -%= 4;
    }

    psx.registers.sr.interrupt_stack <<= 2;

    psx.registers.pc = if (psx.registers.sr.bev == 1) 0xbfc00180 else 0x80000080;
    psx.registers.next_pc = psx.registers.pc +% 4;
}

fn wrapping_add_u32_i32(lhs: u32, rhs: i32) u32 {
    // NOTE: using two's-complement to ignore signedness
    return lhs +% @as(u32, @bitCast(rhs));
}
