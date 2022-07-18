uniform mat4 WVPMatrix;
uniform float Blend;
in vec3 inPosition;
in vec3 inTexcoord0;
out vec4 varColor;
void v_VolumePreRender()
{
    // Transform the position from model space to clip space.
    gl_Position = WVPMatrix * vec4(inPosition, 1.0);
    varColor.rgb = inTexcoord0;
    varColor.a = Blend;
}
