uniform mat4 WVPMatrix;
uniform float Blend;
in vec3 inPosition;
in vec4 inColor0;
out vec4 varColor;
//----------------------------------------------------------------------------
void v_VolumePreRenderColor()
{
    // Transform the position from model space to clip space.
    gl_Position = WVPMatrix * vec4(inPosition, 1.0);

    // Pass through the vertex color.
    varColor.xyz = inColor0.xyz;
    varColor.a = Blend;
}
