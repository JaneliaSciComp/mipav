//----------------------------------------------------------------------------
uniform sampler3D bVolumeImageA_TEXUNIT1; 
uniform sampler1D cColorMapA_TEXUNIT2; 
uniform sampler3D iSurfaceImage_TEXUNIT8;
uniform sampler3D jVolumeImageB_TEXUNIT9; 
uniform sampler1D kColorMapB_TEXUNIT10; 

uniform vec3 ColorLUTOnA;
uniform vec3 ColorLUTOnB;

uniform float IsColorA;
uniform float IsColorB;
uniform float Blend;
uniform float ABBlend;
uniform float ShowSurface;
uniform float ZSlice;
uniform float UseZSlice;

void p_Color_Opacity_TextureP()
{
    vec3 texCoord = gl_TexCoord[0].xyz;
    if ( UseZSlice != 0.0 )
    {
        texCoord.z = ZSlice;
    }
    gl_FragColor = vec4(0.0);
    //vec4 color = texture3D(bVolumeImageA_TEXUNIT1, gl_TexCoord[0].xyz);
    vec4 color = texture3D(bVolumeImageA_TEXUNIT1, texCoord );
    if ( IsColorA != 0.0 )
    {
        if ( ColorLUTOnA.x != 0.0 )
        {
            gl_FragColor.r = texture1D(cColorMapA_TEXUNIT2,color.r).r;
        }
        if ( ColorLUTOnA.y != 0.0 )
        {
            gl_FragColor.g = texture1D(cColorMapA_TEXUNIT2,color.g).g;
        }
        if ( ColorLUTOnA.z != 0.0 )
        {
            gl_FragColor.b = texture1D(cColorMapA_TEXUNIT2,color.b).b;
        }
    }
    else
    {
        gl_FragColor.rgb = texture1D(cColorMapA_TEXUNIT2, color.r).rgb;
    }
    if ( ABBlend != 1.0 )
    {
        //color = texture3D(jVolumeImageB_TEXUNIT9,gl_TexCoord[0].xyz);
        color = texture3D(jVolumeImageB_TEXUNIT9, texCoord );
        if ( IsColorB != 0.0 )
        {
            if ( ColorLUTOnB.x != 0.0 )
            {
                gl_FragColor.r = ABBlend * gl_FragColor.r + (1.0 - ABBlend) * texture1D(kColorMapB_TEXUNIT10, color.r).r;
            }
            else
            {
                gl_FragColor.r = ABBlend * gl_FragColor.r;
            }
            if ( ColorLUTOnB.y != 0.0 )
            {
                gl_FragColor.g = ABBlend * gl_FragColor.g + (1.0 - ABBlend) * texture1D(kColorMapB_TEXUNIT10, color.g).g;
            }
            else
            {
                gl_FragColor.g = ABBlend * gl_FragColor.g;
            }
            if ( ColorLUTOnB.z != 0.0 )
            {
                gl_FragColor.b = ABBlend * gl_FragColor.b + (1.0 - ABBlend) * texture1D(kColorMapB_TEXUNIT10, color.b).b;
            }
            else
            {
                gl_FragColor.b = ABBlend * gl_FragColor.b;
            }
        }
        else
        {
            gl_FragColor.rgb = ABBlend * gl_FragColor.rgb + (1.0 - ABBlend) * texture1D(kColorMapB_TEXUNIT10, color.r).rgb;
        }
    }

    if ( ShowSurface != 0.0 )
    {
        //vec4 surfaceColor = texture3D(iSurfaceImage_TEXUNIT8,gl_TexCoord[0].xyz);
        vec4 surfaceColor = texture3D(iSurfaceImage_TEXUNIT8, texCoord);
        if ( (surfaceColor.r != 0) || (surfaceColor.g != 0) || (surfaceColor.b != 0))
        {
            gl_FragColor.rgb = surfaceColor.rgb;
        }
    }
    
    gl_FragColor.a = Blend;
}
//----------------------------------------------------------------------------
