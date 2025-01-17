struct VS_INPUT
{
    uint vertex_id : SV_VertexID;
};

struct VS_OUTPUT
{
    float4 position_cs : SV_Position;
    float3 color : TEXCOORD0;
};

static const float2 VertexPos[3] = {
  float2(0, -1),
  float2(1, 1),
  float2(-1, 1),
};

static const float3 VertexColor[3] = {
  float3(1, 0, 0),
  float3(0, 1, 0),
  float3(0, 0, 1),
};

void main(in VS_INPUT input, out VS_OUTPUT output)
{
    output.position_cs = float4(VertexPos[input.vertex_id], 0.0, 1.0);
    output.color = VertexColor[input.vertex_id];
}
