uniform sampler2D ColorTex0;
uniform sampler2D ColorTex1;
uniform sampler2D ColorTex2;

void p_OrderIndpTransparencyFinalP()
{
    vec4 BackgroundColor = texture2D(ColorTex0, gl_TexCoord[0].xy);
    vec4 SumColor = texture2D(ColorTex1, gl_TexCoord[0].xy);
    float n = texture2D(ColorTex2, gl_TexCoord[0].xy).r;
    
    if (n == 0.0) {
        gl_FragColor.rgb = BackgroundColor;
        return;
    }
    
    vec3 AvgColor = SumColor.rgb / SumColor.a;
    float AvgAlpha = SumColor.a / n;
    
    float T = pow(1.0-AvgAlpha, n);
    gl_FragColor.rgb = AvgColor * (1 - T) + BackgroundColor.rgb * T;
}
