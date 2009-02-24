//----------------------------------------------------------------------------
uniform sampler3D bVolumeImageA_TEXUNIT1; 
uniform sampler1D cColorMapA_TEXUNIT2; 
uniform sampler3D iSurfaceImage_TEXUNIT8;
uniform sampler3D jVolumeImageB_TEXUNIT9; 
uniform sampler1D kColorMapB_TEXUNIT10; 

uniform float IsColorA;
uniform float IsColorB;
uniform float Blend;
uniform float ABBlend;
uniform float ShowSurface;

void p_Color_Opacity_TextureP()
{
    vec4 color = texture3D(bVolumeImageA_TEXUNIT1,gl_TexCoord[0].xyz);
    if ( IsColorA != 0.0 )
    {
        gl_FragColor.r = texture1D(cColorMapA_TEXUNIT2, color.r).r;
        gl_FragColor.g = texture1D(cColorMapA_TEXUNIT2, color.g).r;
        gl_FragColor.b = texture1D(cColorMapA_TEXUNIT2, color.b).r;
    }
    else
    {
        gl_FragColor.rgb = texture1D(cColorMapA_TEXUNIT2, color.r).rgb;
    }
    if ( ABBlend != 1.0 )
    {
        color = texture3D(jVolumeImageB_TEXUNIT9,gl_TexCoord[0].xyz);
        if ( IsColorB != 0.0 )
        {
            gl_FragColor.r = ABBlend * gl_FragColor.r + (1.0 - ABBlend) * texture1D(kColorMapB_TEXUNIT10, color.r).r;
            gl_FragColor.g = ABBlend * gl_FragColor.g + (1.0 - ABBlend) * texture1D(kColorMapB_TEXUNIT10, color.g).r;
            gl_FragColor.b = ABBlend * gl_FragColor.b + (1.0 - ABBlend) * texture1D(kColorMapB_TEXUNIT10, color.b).r;
        }
        else
        {
            gl_FragColor.rgb = ABBlend * gl_FragColor.rgb + (1.0 - ABBlend) * texture1D(kColorMapB_TEXUNIT10, color.r).rgb;
        }
    }

    if ( ShowSurface != 0.0 )
    {
        vec4 surfaceColor = texture3D(iSurfaceImage_TEXUNIT8,gl_TexCoord[0].xyz);
        if ( (surfaceColor.r != 0) || (surfaceColor.g != 0) || (surfaceColor.b != 0))
        {
            gl_FragColor.rgb = surfaceColor.rgb;
        }
    }
    
    gl_FragColor.a = Blend;
}
//----------------------------------------------------------------------------
