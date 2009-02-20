//----------------------------------------------------------------------------
uniform sampler2D BaseSampler;
void p_TextureP ()
{
    // Sample the texture image.
    gl_FragColor = texture2D(BaseSampler,gl_TexCoord[0].xy);
}
//----------------------------------------------------------------------------
