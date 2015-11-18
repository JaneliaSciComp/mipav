uniform mat4 WVPMatrix;

in vec3 inPosition;
in vec4 inColor0;
out vec4 varColor;
void  v_VertexColor3()
{
    // Transform the position from model space to clip space.
    gl_Position = WVPMatrix * vec4(inPosition, 1.0);
    
    // Pass through the vertex color.
    varColor.xyz = inColor0.xyz;
    varColor.a = 1.0;
} 

//----------------------------------------------------------------------------
void  v_VertexColor4()
{
    // Transform the position from model space to clip space.
    gl_Position = WVPMatrix * vec4(inPosition, 1.0);
    
    // Pass through the vertex color.
    varColor = inColor0;
} 

