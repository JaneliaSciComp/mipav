//----------------------------------------------------------------------------
uniform vec3 ConstantColor;
uniform float UseConstantColor;
uniform mat4 WVPMatrix;
void v_ConstantColor()
{
    // Transform the position from model space to clip space.
    gl_Position = WVPMatrix*gl_Vertex;
    
    // Set the vertex color.
    gl_FrontColor = gl_SecondaryColor;
    if ( UseConstantColor == 1.0 )
    {
        gl_FrontColor.rgb = ConstantColor;
    }   
    else if ( UseConstantColor == 2.0 )
    {
        gl_FrontColor.rgb = gl_Color.rgb;
    }   
    gl_FrontColor.a = 1.0;
}
