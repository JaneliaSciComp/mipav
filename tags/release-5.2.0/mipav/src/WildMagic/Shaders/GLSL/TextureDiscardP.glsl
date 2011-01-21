//----------------------------------------------------------------------------
//uniform sampler2DRect BaseSampler;
uniform sampler2D BaseSampler;
uniform vec3 Color;
uniform float Blend;
void p_TextureDiscardP ()
{
    // Sample the texture image.
    //gl_FragColor = texture2DRect(BaseSampler,gl_TexCoord[0].xy);
    gl_FragColor = texture2D(BaseSampler,gl_TexCoord[0].xy);
    gl_FragColor.r *= Color.r;
    gl_FragColor.g *= Color.g;
    gl_FragColor.b *= Color.b;
    gl_FragColor.a *= Blend;
    if ( gl_FragColor.a == 0.0 )
    {
        discard;
    }
}
//----------------------------------------------------------------------------
