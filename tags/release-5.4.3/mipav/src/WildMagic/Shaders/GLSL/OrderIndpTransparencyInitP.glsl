uniform sampler2D BaseSampler;
uniform float MyAlpha;
void p_OrderIndpTransparencyInitP()
{
    // Add the material and texture colors.
    vec4 kBaseColor = texture2D(BaseSampler,gl_TexCoord[0].xy);
    vec4 color;
    color.rgb = clamp(kBaseColor.rgb + gl_Color.rgb, 0.0, 1.0);
    color.a = MyAlpha;

//     gl_FragData[0] = vec4(color.rgb * color.a, color.a);
//     gl_FragData[1] = vec4(1.0);
//     gl_FragData[0] = vec4(0.0);
//     gl_FragData[1] = vec4(color.rgb * color.a, color.a);
//     gl_FragData[2] = vec4(1.0);
    gl_FragData[0] = vec4(color.rgb * color.a, color.a);
    gl_FragData[1] = vec4(1.0);
    //gl_FragData[0] = vec4(0.0);
}
