//----------------------------------------------------------------------------
uniform vec3 ConstantColor;
uniform float UseConstantColor;
uniform mat4 WVPMatrix;
in vec3 inPosition;
in vec4 inColor0;
in vec4 inColor1;
out vec4 varColor;
void v_ConstantColor()
{
    // Transform the position from model space to clip space.
    gl_Position = WVPMatrix*vec4(inPosition, 1.0);
    
    // Set the vertex color.
    varColor = inColor1;
    if ( UseConstantColor == 1.0 )
    {
        gl_FrontColor.rgb = ConstantColor;
    }   
    else if ( UseConstantColor == 2.0 )
    {
        varColor.rgb = inColor0.rgb;
    }   
    varColor.a = 1.0;
}
