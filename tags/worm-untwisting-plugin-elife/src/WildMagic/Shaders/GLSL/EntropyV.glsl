//----------------------------------------------------------------------------
void v_EntropyV()
{
    vec2 texCoord = gl_Vertex.xy;
    texCoord += 1.0;
    texCoord /= 2.0;
    gl_TexCoord[0].z = 0.0;
    gl_TexCoord[0].w = 0.0;
    gl_TexCoord[0].xy = texCoord.xy;

    gl_Position = gl_Vertex;
    gl_Position.x = 0;
    gl_Position.y = 0;
}
//----------------------------------------------------------------------------
