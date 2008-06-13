//----------------------------------------------------------------------------
uniform mat4 WVPMatrix;

void v_Texture ()
// (
//     in float4        kModelPosition  : POSITION,
//     in float2        kInBaseTCoord : TEXCOORD0,
//     out float4       kClipPosition : POSITION,
//     out float2       kOutBaseTCoord : TEXCOORD0,
//     uniform float4x4 WVPMatrix)
{
    // Transform the position from model space to clip space.
    gl_Position = WVPMatrix * gl_Vertex;

    // Pass through the texture coordinate.
    gl_TexCoord[0] = gl_MultiTexCoord0;
}

//----------------------------------------------------------------------------
uniform sampler2D BaseSampler;
void p_Texture ()
// (
//     in float2         kBaseTCoord : TEXCOORD0,
//     out float4        kPixelColor : COLOR,
//     uniform sampler2D BaseSampler)
{
    // Sample the texture image.
    gl_FragColor = texture2D(BaseSampler,gl_TexCoord[0].xy);
}
//----------------------------------------------------------------------------
