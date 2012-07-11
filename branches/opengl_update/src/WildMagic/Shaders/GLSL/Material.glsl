uniform mat4 WVPMatrix;
uniform vec4   MaterialDiffuse;
//----------------------------------------------------------------------------
void v_Material ()
//     in float4        kModelPosition  : POSITION,
//     out float4       kClipPosition : POSITION,
//     out float4       kDiffuseColor : COLOR,
//     uniform float4x4 WVPMatrix,
//     uniform float4   MaterialDiffuse)
{
    // Transform the position from model space to clip space.
    gl_Position = WVPMatrix * gl_Vertex;

    // Use the material diffuse color as the vertex color.
    gl_FrontColor = MaterialDiffuse;
}
//----------------------------------------------------------------------------
