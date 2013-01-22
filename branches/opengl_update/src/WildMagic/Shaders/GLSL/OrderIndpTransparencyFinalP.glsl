uniform sampler2D ColorTex0;
uniform sampler2D ColorTex1;
uniform sampler2D ColorTex2;

in vec2 varTexCoord;
out vec4 fragColor;

void p_OrderIndpTransparencyFinalP()
{
    vec4 BackgroundColor = texture(ColorTex0, varTexCoord.xy, 0.0);
    vec4 SumColor = texture(ColorTex1, varTexCoord.xy, 0.0);
    float n = texture(ColorTex2, varTexCoord.xy, 0.0).r;
    
    if (n == 0.0) {
        fragColor.rgb = BackgroundColor.rgb;
        return;
    }
    
    vec3 AvgColor = SumColor.rgb / SumColor.a;
    float AvgAlpha = SumColor.a / n;
    
    float T = pow(1.0-AvgAlpha, n);
    fragColor.rgb = AvgColor * (1 - T) + BackgroundColor.rgb * T;
}
