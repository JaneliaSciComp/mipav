uniform mat4 WVPMatrix;

void  v_VertexColor3()
{
    // Transform the position from model space to clip space.
    gl_Position = WVPMatrix * gl_Vertex;
    
    // Pass through the vertex color.
    gl_FrontColor.xyz = gl_Color.xyz;
} 

//----------------------------------------------------------------------------
void  v_VertexColor4()
{
    // Transform the position from model space to clip space.
    gl_Position = WVPMatrix * gl_Vertex;
    
    // Pass through the vertex color.
    gl_FrontColor = gl_Color;
} 

