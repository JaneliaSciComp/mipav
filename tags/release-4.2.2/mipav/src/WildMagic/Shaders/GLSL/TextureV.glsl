//----------------------------------------------------------------------------
uniform mat4 WVPMatrix;
void v_TextureV ()
{
    // Transform the position from model space to clip space.
    gl_Position = WVPMatrix * gl_Vertex;

    // Pass through the texture coordinate.
    gl_TexCoord[0] = gl_MultiTexCoord0;
}
//----------------------------------------------------------------------------
