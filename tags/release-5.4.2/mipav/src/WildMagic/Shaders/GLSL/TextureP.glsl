//----------------------------------------------------------------------------
//uniform sampler2DRect BaseSampler;
uniform sampler2D BaseSampler;
void p_TextureP ()
{
    // Sample the texture image.
    //gl_FragColor = texture2DRect(BaseSampler,gl_TexCoord[0].xy);
    gl_FragColor = texture2D(BaseSampler,gl_TexCoord[0].xy);
}
//----------------------------------------------------------------------------
