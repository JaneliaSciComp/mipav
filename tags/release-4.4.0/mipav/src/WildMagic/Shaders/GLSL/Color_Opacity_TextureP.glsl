//----------------------------------------------------------------------------
uniform sampler3D bVolumeImageA; 
uniform sampler1D cColorMapA; 
uniform sampler3D iSurfaceImage;
uniform sampler3D jVolumeImageB; 
uniform sampler1D kColorMapB; 

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
    vec4 color = texture3D(bVolumeImageA, texCoord );
    if ( IsColorA != 0.0 )
    {
        if ( ColorLUTOnA.x != 0.0 )
        {
            gl_FragColor.r = texture1D(cColorMapA,color.r).r;
        }
        if ( ColorLUTOnA.y != 0.0 )
        {
            gl_FragColor.g = texture1D(cColorMapA,color.g).g;
        }
        if ( ColorLUTOnA.z != 0.0 )
        {
            gl_FragColor.b = texture1D(cColorMapA,color.b).b;
        }
    }
    else
    {
        gl_FragColor.rgb = texture1D(cColorMapA, color.r).rgb;
    }
    if ( ABBlend != 1.0 )
    {
        color = texture3D(jVolumeImageB, texCoord );
        if ( IsColorB != 0.0 )
        {
            if ( ColorLUTOnB.x != 0.0 )
            {
                gl_FragColor.r = ABBlend * gl_FragColor.r + (1.0 - ABBlend) * texture1D(kColorMapB, color.r).r;
            }
            else
            {
                gl_FragColor.r = ABBlend * gl_FragColor.r;
            }
            if ( ColorLUTOnB.y != 0.0 )
            {
                gl_FragColor.g = ABBlend * gl_FragColor.g + (1.0 - ABBlend) * texture1D(kColorMapB, color.g).g;
            }
            else
            {
                gl_FragColor.g = ABBlend * gl_FragColor.g;
            }
            if ( ColorLUTOnB.z != 0.0 )
            {
                gl_FragColor.b = ABBlend * gl_FragColor.b + (1.0 - ABBlend) * texture1D(kColorMapB, color.b).b;
            }
            else
            {
                gl_FragColor.b = ABBlend * gl_FragColor.b;
            }
        }
        else
        {
            gl_FragColor.rgb = ABBlend * gl_FragColor.rgb + (1.0 - ABBlend) * texture1D(kColorMapB, color.r).rgb;
        }
    }

    if ( ShowSurface != 0.0 )
    {
        vec4 surfaceColor = texture3D(iSurfaceImage, texCoord);
        if ( (surfaceColor.r != 0) || (surfaceColor.g != 0) || (surfaceColor.b != 0))
        {
            gl_FragColor.rgb = surfaceColor.rgb;
        }
    }
    
    gl_FragColor.a = Blend;
}
//----------------------------------------------------------------------------
