const std = @import("std");

const cpu = @import("cpu.zig");
const mmio_gpu = @import("mmio_gpu.zig");

pub const State = struct {
    rectangle_texture_x_flip: u1 = 0,
    rectangle_texture_y_flip: u1 = 0,
};

// FIXME can we make the op_code the selector of the payload field?
const G0Command = packed struct {
    payload: packed union {
        draw_mode: packed struct {
            texture_x_base: u4,
            texture_y_base: u1,
            semi_transparency: u2,
            texture_page_colors: mmio_gpu.MMIO.Packed.TexturePageColors,
            dither_mode: u1,
            draw_to_display_area: u1,
            texture_disable: u1,
            rectangle_texture_x_flip: u1,
            rectangle_texture_y_flip: u1,
            zero_b14_23: u10,
        },
    },
    op_code: enum(u8) {
        DrawMode = 0xe1,
        _,
    },
};

pub fn execute_gp0_write(psx: *cpu.PSXState, command: G0Command) void {
    std.debug.print("GP0 COMMAND value: 0x{x:0>8}\n", .{@as(u32, @bitCast(command))});

    switch (command.op_code) {
        .DrawMode => {
            const draw_mode_payload = command.payload.draw_mode;

            psx.mmio.gpu.GPUSTAT.texture_x_base = draw_mode_payload.texture_x_base;
            psx.mmio.gpu.GPUSTAT.texture_y_base = draw_mode_payload.texture_y_base;
            psx.mmio.gpu.GPUSTAT.semi_transparency = draw_mode_payload.semi_transparency;
            psx.mmio.gpu.GPUSTAT.texture_page_colors = draw_mode_payload.texture_page_colors;
            psx.mmio.gpu.GPUSTAT.dither_mode = draw_mode_payload.dither_mode;
            psx.mmio.gpu.GPUSTAT.draw_to_display_area = draw_mode_payload.draw_to_display_area;
            psx.mmio.gpu.GPUSTAT.texture_disable = draw_mode_payload.texture_disable;

            psx.gpu.rectangle_texture_x_flip = draw_mode_payload.rectangle_texture_x_flip;
            psx.gpu.rectangle_texture_y_flip = draw_mode_payload.rectangle_texture_y_flip;

            std.debug.assert(draw_mode_payload.texture_page_colors != .Reserved);
            std.debug.assert(draw_mode_payload.zero_b14_23 == 0);
        },
        _ => unreachable, // FIXME
    }
}
