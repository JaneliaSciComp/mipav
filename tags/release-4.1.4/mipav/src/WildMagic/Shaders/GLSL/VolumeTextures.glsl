//----------------------------------------------------------------------------
uniform mat4 WVPMatrix;
void v_VolumeTextures ()
// (
//     in float4        kModelPosition  : POSITION,
//     in float3        kInTCoord : TEXCOORD0,
//     out float4       kClipPosition : POSITION,
//     out float3       kOutTCoord : TEXCOORD0,
//     uniform float4x4 WVPMatrix)
{
    // Transform the position from model space to clip space.
    gl_Position = WVPMatrix * gl_Vertex;

    // Pass through the texture coordinate.
    gl_TexCoord[0] = gl_MultiTexCoord0;
}

//----------------------------------------------------------------------------
uniform float     CommonAlpha;
uniform float     Threshold;
uniform sampler3D BaseSampler;
void p_VolumeTextures ()
// (
//     in float3         kTCoord : TEXCOORD0,
//     out float4        kPixelColor : COLOR,
//     uniform float     CommonAlpha,
//     uniform float     Threshold,
//     uniform sampler3D BaseSampler)
{
    // Sample the texture image.
    gl_FragColor.rgb = texture3D(BaseSampler,gl_TexCoord[0].xyz).rgb;
    if ( (gl_FragColor.r <= Threshold) && (gl_FragColor.g <= Threshold) && (gl_FragColor.b <= Threshold))
    {
        gl_FragColor.a = 0.0;
    }
    else
    {
        gl_FragColor.a = CommonAlpha;
    }
}
//----------------------------------------------------------------------------
