//----------------------------------------------------------------------------
uniform mat4 WVPMatrix;
in vec3 inPosition;
in vec3 inTexcoord0;
out vec3 varTexCoord;
void v_VolumeTexturesV ()
{
    // Transform the position from model space to clip space.
    gl_Position = WVPMatrix * vec4(inPosition, 1.0);

    // Pass through the texture coordinate.
    varTexCoord = inTexcoord0;
}
//----------------------------------------------------------------------------
