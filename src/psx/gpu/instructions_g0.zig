const std = @import("std");

const mmio = @import("mmio.zig");

pub fn command_size_bytes(op_code: OpCode) usize {
    const size_bytes: usize = switch (op_code) {
        .Noop => @sizeOf(Noop),
        .ClearTextureCache => @sizeOf(ClearTextureCache),

        .DrawTriangleMonochromeOpaque, .DrawTriangleMonochromeTransparent => @sizeOf(DrawTriangleMonochrome),
        .DrawQuadMonochromeOpaque, .DrawQuadMonochromeTransparent => 5 * 4, // FIXME https://github.com/ziglang/zig/issues/20647#issuecomment-2241509633

        .DrawTriangleTexturedOpaqueBlended, .DrawTriangleTexturedOpaque, .DrawTriangleTexturedTransparentBlended, .DrawTriangleTexturedTransparent => 7 * 4,
        .DrawQuadTexturedOpaqueBlended, .DrawQuadTexturedOpaque, .DrawQuadTexturedTransparentBlended, .DrawQuadTexturedTransparent => 9 * 4,

        .DrawTriangleShadedOpaque, .DrawTriangleShadedTransparent => 6 * 4,
        .DrawQuadShadedOpaque, .DrawQuadShadedTransparent => @sizeOf(DrawQuadShaded),

        .CopyRectangleVRAMtoCPU => 12,
        .CopyRectangleCPUtoVRAM => 12,

        .SetDrawMode => @sizeOf(SetDrawMode),
        .SetTextureWindow => @sizeOf(SetTextureWindow),
        .SetDrawingAreaTopLeft => @sizeOf(SetDrawingAreaTopLeft),
        .SetDrawingAreaBottomRight => @sizeOf(SetDrawingAreaBottomRight),
        .SetDrawingOffset => @sizeOf(SetDrawingOffset),
        .SetMaskBitSetting => @sizeOf(SetMaskBitSetting),
        _ => unreachable,
    };

    std.debug.assert(size_bytes % 4 == 0);

    return size_bytes;
}

pub const OpCode = enum(u8) {
    Noop = 0x00,
    ClearTextureCache = 0x01,

    DrawTriangleMonochromeOpaque = 0x20,
    DrawTriangleMonochromeTransparent = 0x22,
    DrawQuadMonochromeOpaque = 0x28,
    DrawQuadMonochromeTransparent = 0x2A,

    DrawTriangleTexturedOpaqueBlended = 0x24,
    DrawTriangleTexturedOpaque = 0x25,
    DrawTriangleTexturedTransparentBlended = 0x26,
    DrawTriangleTexturedTransparent = 0x27,
    DrawQuadTexturedOpaqueBlended = 0x2C,
    DrawQuadTexturedOpaque = 0x2D,
    DrawQuadTexturedTransparentBlended = 0x2E,
    DrawQuadTexturedTransparent = 0x2F,

    DrawTriangleShadedOpaque = 0x30,
    DrawTriangleShadedTransparent = 0x32,
    DrawQuadShadedOpaque = 0x38,
    DrawQuadShadedTransparent = 0x3A,

    CopyRectangleVRAMtoCPU = 0xc0,
    CopyRectangleCPUtoVRAM = 0xa0,

    SetDrawMode = 0xe1,
    SetTextureWindow = 0xe2,
    SetDrawingAreaTopLeft = 0xe3,
    SetDrawingAreaBottomRight = 0xe4,
    SetDrawingOffset = 0xe5,
    SetMaskBitSetting = 0xe6,
    _,
};

pub const Noop = packed struct {
    unknown_b0_23: u24,
    command: u8,
};

pub const ClearTextureCache = packed struct {
    zero_b0_23: u24,
    command: u8,
};

pub const DrawTriangleMonochrome = packed struct {
    color: PackedColor,
    command: u8, // FIXME add opaque flag
    v1_pos: PackedVertexPos,
    v2_pos: PackedVertexPos,
    v3_pos: PackedVertexPos,
};

pub const DrawQuadMonochrome = packed struct {
    color: PackedColor,
    command: u8, // FIXME add opaque flag
    v1_pos: PackedVertexPos,
    v2_pos: PackedVertexPos,
    v3_pos: PackedVertexPos,
    v4_pos: PackedVertexPos,
};

pub const DrawTriangleTextured = packed struct {
    color: PackedColor,
    command: u8, // FIXME add opaque flag
    v1_pos: PackedVertexPos,
    v1_texcoord: PackedTexCoord,
    palette: u16,
    v2_pos: PackedVertexPos,
    v2_texcoord: PackedTexCoord,
    tex_page: u16,
    v3_pos: PackedVertexPos,
    v3_texcoord: PackedTexCoord,
    zero_v3_b16_31: u16,
};

pub const DrawQuadTextured = packed struct {
    color: PackedColor,
    command: u8, // FIXME add opaque flag
    v1_pos: PackedVertexPos,
    v1_texcoord: PackedTexCoord,
    palette: u16,
    v2_pos: PackedVertexPos,
    v2_texcoord: PackedTexCoord,
    tex_page: u16,
    v3_pos: PackedVertexPos,
    v3_texcoord: PackedTexCoord,
    zero_v3_b16_31: u16,
    v4_pos: PackedVertexPos,
    v4_texcoord: PackedTexCoord,
    zero_v4_b16_31: u16,
};

pub const DrawTriangleShaded = packed struct {
    v1_color: PackedColor,
    command: u8, // FIXME add opaque flag
    v1_pos: PackedVertexPos,
    v2_color: PackedColor,
    v2_unused: u8,
    v2_pos: PackedVertexPos,
    v3_color: PackedColor,
    v3_unused: u8,
    v3_pos: PackedVertexPos,
};

pub const DrawQuadShaded = packed struct {
    v1_color: PackedColor,
    command: u8, // FIXME add opaque flag
    v1_pos: PackedVertexPos,
    v2_color: PackedColor,
    v2_unused: u8,
    v2_pos: PackedVertexPos,
    v3_color: PackedColor,
    v3_unused: u8,
    v3_pos: PackedVertexPos,
    v4_color: PackedColor,
    v4_unused: u8,
    v4_pos: PackedVertexPos,
};

pub const CopyRectangleAcrossCPU = packed struct {
    zero_b0_23: u24,
    command: u8,
    offset_x: u16,
    offset_y: u16,
    extent_x: u16,
    extent_y: u16,
};

pub const SetDrawMode = packed struct {
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
    command: u8,
};

pub const SetTextureWindow = packed struct {
    mask_x: u5,
    mask_y: u5,
    offset_x: u5,
    offset_y: u5,
    zero_b20_23: u4,
    command: u8,
};

pub const SetDrawingAreaTopLeft = packed struct {
    left: u10,
    top: u10,
    zero_b20_23: u4,
    command: u8,
};

pub const SetDrawingAreaBottomRight = packed struct {
    right: u10,
    bottom: u10,
    zero_b20_23: u4,
    command: u8,
};

pub const SetDrawingOffset = packed struct {
    x: i11,
    y: i11,
    zero_b22_23: u2,
    command: u8,
};

pub const SetMaskBitSetting = packed struct {
    set_mask_when_drawing: u1,
    check_mask_before_drawing: u1,
    zero_b2_23: u22,
    command: u8,
};

comptime {
    std.debug.assert(4 == @sizeOf(Noop));
    std.debug.assert(4 == @sizeOf(ClearTextureCache));
    std.debug.assert(16 == @sizeOf(DrawTriangleMonochrome));
    // FIXME https://github.com/ziglang/zig/issues/20647#issuecomment-2241509633
    // std.debug.assert(20 == @sizeOf(DrawQuadMonochrome));
    // std.debug.assert(24 == @sizeOf(DrawTriangleShaded));
    std.debug.assert(32 == @sizeOf(DrawQuadShaded));
    // std.debug.assert(12 == @sizeOf(CopyRectangleAcrossCPU));
    std.debug.assert(4 == @sizeOf(SetDrawMode));
    std.debug.assert(4 == @sizeOf(SetTextureWindow));
    std.debug.assert(4 == @sizeOf(SetDrawingAreaTopLeft));
    std.debug.assert(4 == @sizeOf(SetDrawingAreaBottomRight));
    std.debug.assert(4 == @sizeOf(SetDrawingOffset));
    std.debug.assert(4 == @sizeOf(SetMaskBitSetting));
}

pub const MaxCommandSizeBytes = 12 * 4; // FIXME use comptime fn?

const PackedVertexPos = packed struct {
    x: u16,
    y: u16,
};

pub const PackedColor = packed struct {
    r: u8,
    g: u8,
    b: u8,
};

const PackedTexCoord = packed struct {
    x: u8,
    y: u8,
};
