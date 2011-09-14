uniform mat4 WVPMatrix;
uniform float Blend;
void v_VolumePreRender()
{
    // Transform the position from model space to clip space.
    gl_Position = WVPMatrix * gl_Vertex;
    gl_FrontColor.rgb = gl_MultiTexCoord0.xyz;
    gl_FrontColor.a = Blend;
}
