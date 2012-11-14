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

void p_Color_Opacity_Texture_TransparencyP()
{
    vec3 texCoord = gl_TexCoord[0].xyz;
    if ( UseZSlice != 0.0 )
    {
        texCoord.z = ZSlice;
    }
    vec4 kOutputColor = vec4(0.0);
    vec4 color = texture3D(bVolumeImageA, texCoord );
    if ( IsColorA != 0.0 )
    {
        if ( ColorLUTOnA.x != 0.0 )
        {
            kOutputColor.r = texture1D(cColorMapA,color.r).r;
        }
        if ( ColorLUTOnA.y != 0.0 )
        {
            kOutputColor.g = texture1D(cColorMapA,color.g).g;
        }
        if ( ColorLUTOnA.z != 0.0 )
        {
            kOutputColor.b = texture1D(cColorMapA,color.b).b;
        }
    }
    else
    {
        kOutputColor.rgb = texture1D(cColorMapA, color.r).rgb;
    }
    if ( ABBlend != 1.0 )
    {
        color = texture3D(jVolumeImageB, texCoord );
        if ( IsColorB != 0.0 )
        {
            if ( ColorLUTOnB.x != 0.0 )
            {
                kOutputColor.r = ABBlend * kOutputColor.r + (1.0 - ABBlend) * texture1D(kColorMapB, color.r).r;
            }
            else
            {
                kOutputColor.r = ABBlend * kOutputColor.r;
            }
            if ( ColorLUTOnB.y != 0.0 )
            {
                kOutputColor.g = ABBlend * kOutputColor.g + (1.0 - ABBlend) * texture1D(kColorMapB, color.g).g;
            }
            else
            {
                kOutputColor.g = ABBlend * kOutputColor.g;
            }
            if ( ColorLUTOnB.z != 0.0 )
            {
                kOutputColor.b = ABBlend * kOutputColor.b + (1.0 - ABBlend) * texture1D(kColorMapB, color.b).b;
            }
            else
            {
                kOutputColor.b = ABBlend * kOutputColor.b;
            }
        }
        else
        {
            kOutputColor.rgb = ABBlend * kOutputColor.rgb + (1.0 - ABBlend) * texture1D(kColorMapB, color.r).rgb;
        }
    }

    if ( ShowSurface != 0.0 )
    {
        vec4 surfaceColor = texture3D(iSurfaceImage, texCoord);
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
