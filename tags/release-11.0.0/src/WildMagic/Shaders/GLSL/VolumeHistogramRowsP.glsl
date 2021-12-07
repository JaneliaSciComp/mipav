//----------------------------------------------------------------------------
uniform sampler2D BaseSampler;
void p_VolumeHistogramRowsP ()
{
    vec4 color = texture2D(BaseSampler,gl_TexCoord[0].xy);
    gl_FragColor = vec4(0.0);
    gl_FragColor.b = color.r;
}
//----------------------------------------------------------------------------
