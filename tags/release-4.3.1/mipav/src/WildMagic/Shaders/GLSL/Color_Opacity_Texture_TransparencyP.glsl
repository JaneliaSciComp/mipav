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

void p_Color_Opacity_Texture_TransparencyP()
{
    vec3 texCoord = gl_TexCoord[0].xyz;
    if ( UseZSlice != 0.0 )
    {
        texCoord.z = ZSlice;
    }
    vec4 kOutputColor = vec4(0.0);
    //vec4 color = texture3D(bVolumeImageA_TEXUNIT1, gl_TexCoord[0].xyz);
    vec4 color = texture3D(bVolumeImageA_TEXUNIT1, texCoord );
    if ( IsColorA != 0.0 )
    {
        if ( ColorLUTOnA.x != 0.0 )
        {
            kOutputColor.r = texture1D(cColorMapA_TEXUNIT2,color.r).r;
        }
        if ( ColorLUTOnA.y != 0.0 )
        {
            kOutputColor.g = texture1D(cColorMapA_TEXUNIT2,color.g).g;
        }
        if ( ColorLUTOnA.z != 0.0 )
        {
            kOutputColor.b = texture1D(cColorMapA_TEXUNIT2,color.b).b;
        }
    }
    else
    {
        kOutputColor.rgb = texture1D(cColorMapA_TEXUNIT2, color.r).rgb;
    }
    if ( ABBlend != 1.0 )
    {
        //color = texture3D(jVolumeImageB_TEXUNIT9,gl_TexCoord[0].xyz);
        color = texture3D(jVolumeImageB_TEXUNIT9, texCoord );
        if ( IsColorB != 0.0 )
        {
            if ( ColorLUTOnB.x != 0.0 )
            {
                kOutputColor.r = ABBlend * kOutputColor.r + (1.0 - ABBlend) * texture1D(kColorMapB_TEXUNIT10, color.r).r;
            }
            else
            {
                kOutputColor.r = ABBlend * kOutputColor.r;
            }
            if ( ColorLUTOnB.y != 0.0 )
            {
                kOutputColor.g = ABBlend * kOutputColor.g + (1.0 - ABBlend) * texture1D(kColorMapB_TEXUNIT10, color.g).g;
            }
            else
            {
                kOutputColor.g = ABBlend * kOutputColor.g;
            }
            if ( ColorLUTOnB.z != 0.0 )
            {
                kOutputColor.b = ABBlend * kOutputColor.b + (1.0 - ABBlend) * texture1D(kColorMapB_TEXUNIT10, color.b).b;
            }
            else
            {
                kOutputColor.b = ABBlend * kOutputColor.b;
            }
        }
        else
        {
            kOutputColor.rgb = ABBlend * kOutputColor.rgb + (1.0 - ABBlend) * texture1D(kColorMapB_TEXUNIT10, color.r).rgb;
        }
    }

    if ( ShowSurface != 0.0 )
    {
        //vec4 surfaceColor = texture3D(iSurfaceImage_TEXUNIT8,gl_TexCoord[0].xyz);
        vec4 surfaceColor = texture3D(iSurfaceImage_TEXUNIT8, texCoord);
        if ( (surfaceColor.r != 0) || (surfaceColor.g != 0) || (surfaceColor.b != 0))
        {
            kOutputColor.rgb = surfaceColor.rgb;
        }
    }
    
    kOutputColor.a = Blend;

    gl_FragData[0] = vec4(kOutputColor.rgb * kOutputColor.a, kOutputColor.a);
    gl_FragData[1] = vec4(1.0);
}
//----------------------------------------------------------------------------
