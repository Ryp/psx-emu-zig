const std = @import("std");

const instructions = @import("cpu_instructions.zig");
const cpu = @import("cpu.zig");

pub fn print_instruction(op_code: u32, instruction: instructions.Instruction) void {
    std.debug.print("0b{b:0>32} 0x{x:0>8} ", .{ op_code, op_code });

    switch (instruction) {
        .sll => |i| print_sll_instruction(i),
        .jr => |i| print_jr_instruction(i),
        .jalr => |i| print_jalr_instruction(i),

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
        .addi => |i| print_i_add_instruction("addi", i),
        .addiu => |i| print_i_add_instruction("addiu", i),
        .andi => |i| print_i_instruction("andi", i),
        .ori => |i| print_i_instruction("ori", i),
        .xori => |i| print_i_instruction("xori", i),
        .lui => |i| print_r_instruction("lui", i),
        .lb => |i| print_i_mem_instruction("lb", i),
        .lh => |i| print_i_mem_instruction("lh", i),
        .lwl => |i| print_i_mem_instruction("lwl", i),
        .lw => |i| print_i_mem_instruction("lw", i),
        .lbu => |i| print_i_mem_instruction("lbu", i),
        .sb => |i| print_i_mem_instruction("sb", i),
        .sh => |i| print_i_mem_instruction("sh", i),
        .swl => |i| print_i_mem_instruction("swl", i),
        .sw => |i| print_i_mem_instruction("sw", i),

        .invalid => unreachable,
    }
}

fn print_i_instruction(op_name: [:0]const u8, instruction: instructions.generic_rs_rt_imm_u16) void {
    std.debug.print("{s} ${}, ${}, 0x{x:0>4}\n", .{ op_name, instruction.rt, instruction.rs, instruction.imm_u16 });
}

fn print_i_add_instruction(op_name: [:0]const u8, instruction: instructions.generic_rs_rt_imm_i16) void {
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

fn print_sll_instruction(instruction: instructions.sll) void {
    if (instruction.rd == .zero and instruction.rt == .zero and instruction.shift_imm == 0) {
        std.debug.print("nop\n", .{});
    } else {
        std.debug.print("sll ${}, ${}, 0x{x:0>4}\n", .{ instruction.rd, instruction.rt, instruction.shift_imm });
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
