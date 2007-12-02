//----------------------------------------------------------------------------
uniform vec3 ConstantColor;
uniform float UseConstantColor;
uniform mat4 WVPMatrix;
void v_ConstantColor()
{
    // Transform the position from model space to clip space.
    gl_Position = WVPMatrix*gl_Vertex;
    
    // Set the vertex color.
    gl_FrontColor = gl_Color;
    if ( UseConstantColor == 1.0 )
    {
        gl_FrontColor.rgb = ConstantColor;
    }   
}
