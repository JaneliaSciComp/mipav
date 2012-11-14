//----------------------------------------------------------------------------
uniform sampler2D BaseSampler;
uniform float Scale;
void p_ScaledTextureP ()
{
    vec4 data = texture2D(BaseSampler,gl_TexCoord[0].xy);
    data.r = log(data.r + 1) * Scale;
    gl_FragColor.r = data.r;
    gl_FragColor.g = data.r;
    gl_FragColor.b = data.r;
    gl_FragColor.a = 1.0;
}
//----------------------------------------------------------------------------
