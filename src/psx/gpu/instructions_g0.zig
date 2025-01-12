const mmio = @import("mmio.zig");

pub const OpCode = enum(u8) {
    Noop = 0x00,
    ClearTextureCache = 0x01,
    DrawTriangleMonochromeOpaque = 0x20,
    DrawTriangleMonochromeTransparent = 0x22,
    DrawQuadMonochromeOpaque = 0x28,
    DrawQuadMonochromeTransparent = 0x2A,
    SetDrawMode = 0xe1,
    SetTextureWindow = 0xe2,
    SetDrawingAreaTopLeft = 0xe3,
    SetDrawingAreaBottomRight = 0xe4,
    SetDrawingOffset = 0xe5,
    SetMaskBitSetting = 0xe6,
    _,
};

pub const CommandRaw = packed struct {
    payload: u24,
    op_code: OpCode,
};

pub const Command = union(OpCode) {
    Noop,
    ClearTextureCache,
    DrawTriangleMonochromeOpaque: DrawMonochromeTriangle,
    DrawTriangleMonochromeTransparent: DrawMonochromeTriangle,
    DrawQuadMonochromeOpaque: DrawMonochromeQuad,
    DrawQuadMonochromeTransparent: DrawMonochromeQuad,
    SetDrawMode: SetDrawMode,
    SetTextureWindow: packed struct {
        mask_x: u5,
        mask_y: u5,
        offset_x: u5,
        offset_y: u5,
        zero_b20_23: u4,
    },
    SetDrawingAreaTopLeft: packed struct {
        left: u10,
        top: u10,
        zero_b20_23: u4,
    },
    SetDrawingAreaBottomRight: packed struct {
        right: u10,
        bottom: u10,
        zero_b20_23: u4,
    },
    SetDrawingOffset: packed struct {
        x: i11,
        y: i11,
        zero_b22_23: u2,
    },
    SetMaskBitSetting: packed struct {
        set_mask_when_drawing: u1,
        check_mask_before_drawing: u1,
        zero_b2_23: u22,
    },
};

const DrawMonochromeTriangle = packed struct {
    color: PackedColor,
    command: u8, // FIXME add opaque flag
    v1_pos: PackedVertexPos,
    v2_pos: PackedVertexPos,
    v3_pos: PackedVertexPos,
};

const DrawMonochromeQuad = packed struct {
    color: PackedColor,
    command: u8, // FIXME add opaque flag
    v1_pos: PackedVertexPos,
    v2_pos: PackedVertexPos,
    v3_pos: PackedVertexPos,
    v4_pos: PackedVertexPos,
};

const PackedVertexPos = packed struct {
    x: u16,
    y: u16,
};

const PackedColor = packed struct {
    r: u8,
    g: u8,
    b: u8,
};

const SetDrawMode = packed struct {
    texture_x_base: u4,
    texture_y_base: u1,
    semi_transparency: u2,
    texture_page_colors: mmio.MMIO.Packed.TexturePageColors,
    dither_mode: u1,
    draw_to_display_area: u1,
    texture_disable: u1,
    rectangle_texture_x_flip: u1,
    rectangle_texture_y_flip: u1,
    zero_b14_23: u10,
};

pub fn make_command(raw: CommandRaw) Command {
    return switch (raw.op_code) {
        .Noop => .{ .Noop = undefined },
        .ClearTextureCache => .{ .ClearTextureCache = undefined },
        .DrawTriangleMonochromeOpaque, .DrawTriangleMonochromeTransparent => unreachable,
        .DrawQuadMonochromeOpaque, .DrawQuadMonochromeTransparent => unreachable,
        .SetDrawMode => .{ .SetDrawMode = @bitCast(raw.payload) },
        .SetTextureWindow => .{ .SetTextureWindow = @bitCast(raw.payload) },
        .SetDrawingAreaTopLeft => .{ .SetDrawingAreaTopLeft = @bitCast(raw.payload) },
        .SetDrawingAreaBottomRight => .{ .SetDrawingAreaBottomRight = @bitCast(raw.payload) },
        .SetDrawingOffset => .{ .SetDrawingOffset = @bitCast(raw.payload) },
        .SetMaskBitSetting => .{ .SetMaskBitSetting = @bitCast(raw.payload) },
        _ => unreachable,
    };
}
