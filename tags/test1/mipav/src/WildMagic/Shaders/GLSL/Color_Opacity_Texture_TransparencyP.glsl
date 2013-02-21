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

in vec3 varTexCoord;
#if __VERSION__ > 150
layout(location = 0) out vec4     outFragData0;
layout(location = 1) out vec4     outFragData1;
#else
out vec4     outFragData0;
out vec4     outFragData1;
#endif
void p_Color_Opacity_Texture_TransparencyP()
{
    vec3 texCoord = varTexCoord.xyz;
    if ( UseZSlice != 0.0 )
    {
        texCoord.z = ZSlice;
    }
    vec4 kOutputColor = vec4(0.0);
    vec4 color = texture(bVolumeImageA, texCoord, 0.0 );
    if ( IsColorA != 0.0 )
    {
        if ( ColorLUTOnA.x != 0.0 )
        {
            kOutputColor.r = texture(cColorMapA,color.r, 0.0).r;
        }
        if ( ColorLUTOnA.y != 0.0 )
        {
            kOutputColor.g = texture(cColorMapA,color.g, 0.0).g;
        }
        if ( ColorLUTOnA.z != 0.0 )
        {
            kOutputColor.b = texture(cColorMapA,color.b, 0.0).b;
        }
    }
    else
    {
        kOutputColor.rgb = texture(cColorMapA, color.r, 0.0).rgb;
    }
    if ( ABBlend != 1.0 )
    {
        color = texture(jVolumeImageB, texCoord, 0.0 );
        if ( IsColorB != 0.0 )
        {
            if ( ColorLUTOnB.x != 0.0 )
            {
                kOutputColor.r = ABBlend * kOutputColor.r + (1.0 - ABBlend) * texture(kColorMapB, color.r, 0.0).r;
            }
            else
            {
                kOutputColor.r = ABBlend * kOutputColor.r;
            }
            if ( ColorLUTOnB.y != 0.0 )
            {
                kOutputColor.g = ABBlend * kOutputColor.g + (1.0 - ABBlend) * texture(kColorMapB, color.g, 0.0).g;
            }
            else
            {
                kOutputColor.g = ABBlend * kOutputColor.g;
            }
            if ( ColorLUTOnB.z != 0.0 )
            {
                kOutputColor.b = ABBlend * kOutputColor.b + (1.0 - ABBlend) * texture(kColorMapB, color.b, 0.0).b;
            }
            else
            {
                kOutputColor.b = ABBlend * kOutputColor.b;
            }
        }
        else
        {
            kOutputColor.rgb = ABBlend * kOutputColor.rgb + (1.0 - ABBlend) * texture(kColorMapB, color.r, 0.0).rgb;
        }
    }

    if ( ShowSurface != 0.0 )
    {
        vec4 surfaceColor = texture(iSurfaceImage, texCoord, 0.0);
        if ( (surfaceColor.r != 0) || (surfaceColor.g != 0) || (surfaceColor.b != 0))
        {
            kOutputColor.rgb = surfaceColor.rgb;
        }
    }
    
    kOutputColor.a = Blend;

    outFragData0 = vec4(kOutputColor.rgb * kOutputColor.a, kOutputColor.a);
    outFragData1 = vec4(1.0);
}
//----------------------------------------------------------------------------
