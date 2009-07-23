uniform sampler2D BaseSampler;
uniform float MyAlpha;
void p_MaterialTextureP()
{
    // Add the material and texture colors.
    vec4 kBaseColor = texture2D(BaseSampler,gl_TexCoord[0].xy);
    gl_FragData[0].rgb = clamp(kBaseColor.rgb + gl_Color.rgb, 0.0, 1.0);
    
    // Multiply the material and texture alphas.
    //gl_FragColor.a = gl_Color.a*gl_Color.a;
    gl_FragData[0].a = 1.0;
    //gl_FragData[1] = vec4(0.0);
    //gl_FragData[2] = vec4(0.0);
}
//----------------------------------------------------------------------------
