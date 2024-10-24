const std = @import("std");

const instructions = @import("cpu_instructions.zig");
const cpu = @import("cpu.zig");

pub fn print_instruction(op_code: u32, instruction: instructions.Instruction) void {
    std.debug.print("0b{b:0>32} 0x{x:0>8} ", .{ op_code, op_code });

    switch (instruction) {
        .sll => |i| print_shift_instruction("sll", i),
        .srl => |i| print_shift_instruction("srl", i),
        .sra => |i| print_shift_instruction("sra", i),
        .sllv => |i| print_ralu_instruction("sllv", i),
        .srlv => |i| print_ralu_instruction("srlv", i),
        .srav => |i| print_ralu_instruction("srav", i),

        .jr => |i| print_jr_instruction(i),
        .jalr => |i| print_jalr_instruction(i),
        .syscall => std.debug.print("syscall\n", .{}),
        .break_ => std.debug.print("break\n", .{}),

        .mfhi => |i| print_generic_rd("mfhi", i),
        .mthi => |i| print_generic_rd("mthi", i),
        .mflo => |i| print_generic_rd("mflo", i),
        .mtlo => |i| print_generic_rd("mtlo", i),
        .rfe => std.debug.print("rfe\n", .{}),
        .cop1 => std.debug.print("cop1\n", .{}),
        .cop2 => std.debug.print("cop2\n", .{}),
        .cop3 => std.debug.print("cop3\n", .{}),

        .mult => |i| print_generic_rs_rt("mult", i),
        .multu => |i| print_generic_rs_rt("multu", i),
        .div => |i| print_generic_rs_rt("div", i),
        .divu => |i| print_generic_rs_rt("divu", i),
        .add => |i| print_ralu_instruction("add", i),
        .addu => |i| print_ralu_instruction("addu", i),
        .sub => |i| print_ralu_instruction("sub", i),
        .subu => |i| print_ralu_instruction("subu", i),
        .and_ => |i| print_ralu_instruction("and", i),
        .or_ => |i| print_ralu_instruction("or", i),
        .xor => |i| print_ralu_instruction("xor", i),
        .nor => |i| print_ralu_instruction("nor", i),
        .slt => |i| print_ralu_instruction("slt", i),
        .sltu => |i| print_ralu_instruction("sltu", i),

        .b_cond_z => |i| print_b_cond_z_instruction(i),
        .j => |i| print_j_instruction("j", i),
        .jal => |i| print_j_instruction("jal", i),
        .beq => |i| print_branch_instruction("beq", i),
        .bne => |i| print_branch_instruction("bne", i),
        .blez => |i| print_branch_2_instruction("blez", i),
        .bgtz => |i| print_branch_2_instruction("bgtz", i),

        .mfc0 => |i| print_mfc0_instruction(i),
        .mtc0 => |i| print_mtc0_instruction(i),
        .addi => |i| print_i_signed_instruction("addi", i),
        .addiu => |i| print_i_signed_instruction("addiu", i),
        .slti => |i| print_i_signed_instruction("slti", i),
        .sltiu => |i| print_i_signed_instruction("sltiu", i),
        .andi => |i| print_i_instruction("andi", i),
        .ori => |i| print_i_instruction("ori", i),
        .xori => |i| print_i_instruction("xori", i),
        .lui => |i| print_r_instruction("lui", i),
        .lb => |i| print_i_mem_instruction("lb", i),
        .lh => |i| print_i_mem_instruction("lh", i),
        .lwl => |i| print_i_mem_instruction("lwl", i),
        .lw => |i| print_i_mem_instruction("lw", i),
        .lbu => |i| print_i_mem_instruction("lbu", i),
        .lhu => |i| print_i_mem_instruction("lhu", i),
        .lwr => |i| print_i_mem_instruction("lwr", i),
        .sb => |i| print_i_mem_instruction("sb", i),
        .sh => |i| print_i_mem_instruction("sh", i),
        .swl => |i| print_i_mem_instruction("swl", i),
        .sw => |i| print_i_mem_instruction("sw", i),
        .swr => |i| print_i_mem_instruction("swr", i),

        .lwc0 => std.debug.print("lwc0\n", .{}),
        .lwc1 => std.debug.print("lwc1\n", .{}),
        .lwc2 => std.debug.print("lwc2\n", .{}),
        .lwc3 => std.debug.print("lwc3\n", .{}),
        .swc0 => std.debug.print("swc0\n", .{}),
        .swc1 => std.debug.print("swc1\n", .{}),
        .swc2 => std.debug.print("swc2\n", .{}),
        .swc3 => std.debug.print("swc3\n", .{}),

        .invalid => std.debug.print("ill\n", .{}),
    }
}

pub fn print_bios_disasm(bios: []u8) void {
    const instruction_count = bios.len / 4;

    for (0..instruction_count) |instruction_index| {
        const op_offset = instruction_index * 4;
        const op_slice = bios[op_offset..];
        const op_code = std.mem.readInt(u32, op_slice[0..4], .little);

        std.debug.print("0x{x:0>8} | ", .{op_offset});

        const instruction = instructions.decode_instruction(op_code);

        print_instruction(op_code, instruction);
    }
}

fn print_generic_rd(op_name: [:0]const u8, instruction: instructions.generic_rd) void {
    std.debug.print("{s} ${}\n", .{ op_name, instruction.rd });
}

fn print_generic_rs_rt(op_name: [:0]const u8, instruction: instructions.generic_rs_rt) void {
    std.debug.print("{s} ${}, ${}\n", .{ op_name, instruction.rt, instruction.rs });
}

fn print_i_instruction(op_name: [:0]const u8, instruction: instructions.generic_rs_rt_imm_u16) void {
    std.debug.print("{s} ${}, ${}, 0x{x:0>4}\n", .{ op_name, instruction.rt, instruction.rs, instruction.imm_u16 });
}

fn print_i_signed_instruction(op_name: [:0]const u8, instruction: instructions.generic_rs_rt_imm_i16) void {
    std.debug.print("{s} ${}, ${}, {}\n", .{ op_name, instruction.rt, instruction.rs, instruction.imm_i16 });
}

fn print_i_mem_instruction(op_name: [:0]const u8, instruction: instructions.generic_rs_rt_imm_i16) void {
    std.debug.print("{s} ${}, {}(${})\n", .{ op_name, instruction.rt, instruction.imm_i16, instruction.rs });
}

fn print_r_instruction(op_name: [:0]const u8, instruction: instructions.generic_rt_imm_u16) void {
    std.debug.print("{s} ${}, 0x{x:0>4}\n", .{ op_name, instruction.rt, instruction.imm_u16 });
}

fn print_jr_instruction(instruction: instructions.jr) void {
    std.debug.print("jr ${}\n", .{instruction.rs});
}

fn print_jalr_instruction(instruction: instructions.jalr) void {
    std.debug.print("jalr ${} ${}\n", .{ instruction.rs, instruction.rd });
}

fn print_b_cond_z_instruction(instruction: instructions.b_cond_z) void {
    std.debug.print("b{s}z{s} ${}, {}\n", .{ if (instruction.test_greater) "ge" else "lt", if (instruction.link) "al" else "", instruction.rs, instruction.rel_offset });
}

fn print_j_instruction(op_name: [:0]const u8, instruction: instructions.generic_j) void {
    std.debug.print("{s} 0x{x:0>8}\n", .{ op_name, instruction.offset });
}

fn print_branch_instruction(op_name: [:0]const u8, instruction: instructions.generic_branch) void {
    std.debug.print("{s} ${}, ${}, {}\n", .{ op_name, instruction.rt, instruction.rs, instruction.rel_offset });
}

fn print_branch_2_instruction(op_name: [:0]const u8, instruction: instructions.generic_branch_2) void {
    std.debug.print("{s} ${}, {}\n", .{ op_name, instruction.rs, instruction.rel_offset });
}

fn print_shift_instruction(op_name: [:0]const u8, instruction: instructions.generic_shift_imm) void {
    if (instruction.rd == .zero and instruction.rt == .zero and instruction.shift_imm == 0) {
        std.debug.print("nop\n", .{});
    } else {
        std.debug.print("{s} ${}, ${}, {}\n", .{ op_name, instruction.rd, instruction.rt, instruction.shift_imm });
    }
}

fn print_ralu_instruction(op_name: [:0]const u8, instruction: instructions.generic_rs_rt_rd) void {
    std.debug.print("{s} ${}, ${}, ${}\n", .{ op_name, instruction.rd, instruction.rt, instruction.rs });
}

fn print_mfc0_instruction(instruction: instructions.mfc0) void {
    switch (instruction.target) {
        else => {
            std.debug.print("mfc0 ${}, {}\n", .{ instruction.cpu_rs, instruction.target });
        },
    }
}

fn print_mtc0_instruction(instruction: instructions.mtc0) void {
    switch (instruction.target) {
        else => {
            std.debug.print("mtc0 ${}, {}\n", .{ instruction.cpu_rs, instruction.target });
        },
    }
}
