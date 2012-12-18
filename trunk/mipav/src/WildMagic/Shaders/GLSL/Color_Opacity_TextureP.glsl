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
out vec4 fragColor;
void p_Color_Opacity_TextureP()
{
    vec3 texCoord = varTexCoord.xyz;
    if ( UseZSlice != 0.0 )
    {
        texCoord.z = ZSlice;
    }
    fragColor = vec4(0.0);
    vec4 color = texture(bVolumeImageA, texCoord, 0.0 );
    if ( IsColorA != 0.0 )
    {
        if ( ColorLUTOnA.x != 0.0 )
        {
            fragColor.r = texture(cColorMapA,color.r, 0.0).r;
        }
        if ( ColorLUTOnA.y != 0.0 )
        {
            fragColor.g = texture(cColorMapA,color.g, 0.0).g;
        }
        if ( ColorLUTOnA.z != 0.0 )
        {
            fragColor.b = texture(cColorMapA,color.b, 0.0).b;
        }
    }
    else
    {
        fragColor.rgb = texture(cColorMapA, color.r, 0.0).rgb;
    }
    if ( ABBlend != 1.0 )
    {
        color = texture(jVolumeImageB, texCoord, 0.0 );
        if ( IsColorB != 0.0 )
        {
            if ( ColorLUTOnB.x != 0.0 )
            {
                fragColor.r = ABBlend * fragColor.r + (1.0 - ABBlend) * texture(kColorMapB, color.r, 0.0).r;
            }
            else
            {
                fragColor.r = ABBlend * fragColor.r;
            }
            if ( ColorLUTOnB.y != 0.0 )
            {
                fragColor.g = ABBlend * fragColor.g + (1.0 - ABBlend) * texture(kColorMapB, color.g, 0.0).g;
            }
            else
            {
                fragColor.g = ABBlend * fragColor.g;
            }
            if ( ColorLUTOnB.z != 0.0 )
            {
                fragColor.b = ABBlend * fragColor.b + (1.0 - ABBlend) * texture(kColorMapB, color.b, 0.0).b;
            }
            else
            {
                fragColor.b = ABBlend * fragColor.b;
            }
        }
        else
        {
            fragColor.rgb = ABBlend * fragColor.rgb + (1.0 - ABBlend) * texture(kColorMapB, color.r, 0.0).rgb;
        }
    }

    if ( ShowSurface != 0.0 )
    {
        vec4 surfaceColor = texture(iSurfaceImage, texCoord, 0.0);
        if ( (surfaceColor.r != 0) || (surfaceColor.g != 0) || (surfaceColor.b != 0))
        {
            fragColor.rgb = surfaceColor.rgb;
        }
    }
    
    fragColor.a = Blend;
}
//----------------------------------------------------------------------------
