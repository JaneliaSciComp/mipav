//----------------------------------------------------------------------------
uniform mat4 WVPMatrix;
void v_Color_Opacity_Texture()
{
    // Transform the position from model space to clip space.
    gl_Position = WVPMatrix*gl_Vertex;

    // Pass through the texture coordinate.
    gl_TexCoord[0] = gl_MultiTexCoord0;
}
//----------------------------------------------------------------------------
uniform sampler3D bVolumeImageA_TEXUNIT1; 
uniform sampler1D cColorMapA_TEXUNIT2; 
uniform float IsColor;
uniform float blend;
// uniform sampler1D dOpacityMapA; 
// uniform sampler3D bVolumeImageA : TEXUNIT1; 
// uniform sampler1D cColorMapA_TEXUNIT2 : TEXUNIT2; 
// uniform sampler1D dOpacityMapA : TEXUNIT3; 
void p_Color_Opacity_Texture()
{
    vec4 color = texture3D(bVolumeImageA_TEXUNIT1,gl_TexCoord[0]);
//     float opacity = texture1D(dOpacityMapA,color.r).r;

//     if ( opacity == 0.0 )
//     {
//         gl_FragColor.rgb = 0.0;
//     }
//     else
//     {
//         gl_FragColor.rgb = texture1D(cColorMapA_TEXUNIT2,color.r).rgb;
//     }
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
    gl_FragColor.a = blend;
}
//----------------------------------------------------------------------------
