//----------------------------------------------------------------------------
uniform sampler3D bVolumeImageA; 
uniform sampler1D cColorMapA; 
uniform sampler3D iSurfaceImage;

uniform vec3 ColorLUTOnA;

uniform float IsColorA;
uniform float Blend;
uniform float ShowSurface;

in vec3 varTexCoord;
out vec4 fragColor;
void p_Color_Opacity_TextureP()
{
    //fragColor = vec4(1);
    //return;
    
    vec3 texCoord = varTexCoord.xyz;

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
