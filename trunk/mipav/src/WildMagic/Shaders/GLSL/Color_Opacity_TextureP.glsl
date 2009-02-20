//----------------------------------------------------------------------------
uniform sampler3D bVolumeImageA_TEXUNIT1; 
uniform sampler1D cColorMapA_TEXUNIT2; 
uniform sampler3D iSurfaceImage_TEXUNIT8;

uniform float IsColor;
uniform float blend;
uniform float ShowSurface;

void p_Color_Opacity_TextureP()
{
    vec4 color = texture3D(bVolumeImageA_TEXUNIT1,gl_TexCoord[0].xyz);
    if ( IsColor != 0.0 )
    {
        gl_FragColor.r = texture1D(cColorMapA_TEXUNIT2, color.r).r;
        gl_FragColor.g = texture1D(cColorMapA_TEXUNIT2, color.g).r;
        gl_FragColor.b = texture1D(cColorMapA_TEXUNIT2, color.b).r;
    }
    else
    {
        gl_FragColor.rgb = texture1D(cColorMapA_TEXUNIT2, color.r).rgb;
    }

    if ( ShowSurface != 0 )
    {
        vec4 surfaceColor = texture3D(iSurfaceImage_TEXUNIT8,gl_TexCoord[0].xyz);
        if ( (surfaceColor.r != 0) || (surfaceColor.g != 0) || (surfaceColor.b != 0))
        {
            gl_FragColor.rgb = surfaceColor.rgb;
        }
    }
    
    gl_FragColor.a = blend;
}
//----------------------------------------------------------------------------
