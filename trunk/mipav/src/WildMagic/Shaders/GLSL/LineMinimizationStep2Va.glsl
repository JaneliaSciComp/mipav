//----------------------------------------------------------------------------
uniform sampler2D bracketNewImage;
void v_LineMinimizationStep2Va()
{
    vec2 texCoord = gl_Vertex.xy;
    gl_FrontColor = texture2D(bracketNewImage, texCoord );
    gl_Position = gl_Vertex;
    gl_Position.z = 0;
}
//----------------------------------------------------------------------------
