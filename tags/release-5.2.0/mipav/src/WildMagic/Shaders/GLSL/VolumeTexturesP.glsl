//----------------------------------------------------------------------------
uniform float     CommonAlpha;
uniform float     Threshold;
uniform sampler3D BaseSampler;
void p_VolumeTexturesP ()
{
    // Sample the texture image.
    gl_FragColor.rgb = texture3D(BaseSampler,gl_TexCoord[0].xyz).rgb;
    if ( (gl_FragColor.r <= Threshold) && (gl_FragColor.g <= Threshold) && (gl_FragColor.b <= Threshold))
    {
        gl_FragColor.a = 0.0;
    }
    else
    {
        gl_FragColor.a = CommonAlpha;
    }
}
//----------------------------------------------------------------------------
