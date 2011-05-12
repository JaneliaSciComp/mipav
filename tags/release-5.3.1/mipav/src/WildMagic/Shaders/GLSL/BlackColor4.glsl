uniform mat4 WVPMatrix;


//----------------------------------------------------------------------------
void  v_BlackColor4()
{
    // Transform the position from model space to clip space.
    gl_Position = WVPMatrix * gl_Vertex;
    
    // Black.
    gl_FrontColor = vec4(0.0, 0.0, 0.0, 1.0);
} 

