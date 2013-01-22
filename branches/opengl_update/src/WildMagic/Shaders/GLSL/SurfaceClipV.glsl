uniform vec4 SurfaceScale;
uniform mat4 WVPMatrix;
in vec3 inPosition;
in vec3 inTexcoord0;
in vec4 inColor0;
out vec3 varTexCoord;
out vec4 varColor;

void v_SurfaceClipV()
{
    // Transform the position from model space to clip space.
    gl_Position = WVPMatrix * vec4(inPosition, 1.0);
    // Pass through the texture coordinate.
    varTexCoord = inTexcoord0;
    // Pass through the color.
    varColor = inColor0;
}
