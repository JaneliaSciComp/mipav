//----------------------------------------------------------------------------
uniform mat4 WVPMatrix;
void v_T0d2T1d2PassThrough()
{
    // Transform the position from model space to clip space.
    gl_Position = WVPMatrix*gl_Vertex;

    // Pass through the texture coordinates.
    gl_TexCoord[0] = gl_MultiTexCoord0;
    gl_TexCoord[1] = gl_MultiTexCoord1;
}
//----------------------------------------------------------------------------
