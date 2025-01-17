struct VS_INPUT
{
    float2 position : TEXCOORD0;
    float3 color : TEXCOORD1;
};

struct VS_OUTPUT
{
    float4 position_cs : SV_Position;
    float2 color : TEXCOORD0;
};

void main(in VS_INPUT input, out VS_OUTPUT output)
{
    output.position_cs = float4(input.position, 0.0, 1.0);
    output.color = input.color;
}
