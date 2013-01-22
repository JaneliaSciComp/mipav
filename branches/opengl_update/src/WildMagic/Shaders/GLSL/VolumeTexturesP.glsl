//----------------------------------------------------------------------------
uniform float     CommonAlpha;
uniform float     Threshold;
uniform sampler3D BaseSampler;
in vec3 varTexCoord;
out vec4 fragColor;
void p_VolumeTexturesP ()
{
    // Sample the texture image.
    fragColor.rgb = texture(BaseSampler,varTexCoord, 0.0).rgb;
    if ( (fragColor.r <= Threshold) && (fragColor.g <= Threshold) && (fragColor.b <= Threshold))
    {
        fragColor.a = 0.0;
    }
    else
    {
        fragColor.a = CommonAlpha;
    }
}
//----------------------------------------------------------------------------
