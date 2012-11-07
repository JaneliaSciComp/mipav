uniform mat4 WVPMatrix;
uniform float Blend;
//----------------------------------------------------------------------------
void v_VolumePreRenderColor()
{
    // Transform the position from model space to clip space.
    gl_Position = WVPMatrix * gl_Vertex;

    // Pass through the vertex color.
    gl_FrontColor.xyz = gl_Color.xyz;
    gl_FrontColor.a = Blend;
}
