//----------------------------------------------------------------------------
uniform mat4 WVPMatrix;
in vec3 inPosition;
in vec4 inColor0;
in vec3 inTexcoord0;
out vec3 varTexCoord;
out vec4 varColor;
out vec4 position;
void v_BoundingBoxGridV ()
{
    // Transform the position from model space to clip space.
    gl_Position = WVPMatrix * vec4(inPosition, 1.0);
    
    // Pass through the vertex color.
    varColor = inColor0;

    // Pass through the texture coordinate.
    varTexCoord = inTexcoord0;

    position.xyz = inPosition.xyz;
    position.w = gl_Position.w;
}
//----------------------------------------------------------------------------
