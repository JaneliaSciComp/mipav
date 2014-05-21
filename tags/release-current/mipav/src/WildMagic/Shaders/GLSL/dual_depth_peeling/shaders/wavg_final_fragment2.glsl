//--------------------------------------------------------------------------------------
// Order Independent Transparency with Average Color
//
// Author: Louis Bavoil
// Email: sdkfeedback@nvidia.com
//
// Copyright (c) NVIDIA Corporation. All rights reserved.
//--------------------------------------------------------------------------------------

uniform sampler2D ColorTex0;
uniform sampler2D ColorTex1;
uniform sampler2D ColorTex2;

in vec2 varTexCoord;
out vec4 fragColor;
void main(void)
{
    vec4 BackgroundColor = texture(ColorTex0, varTexCoord, 0.0);
    vec4 SumColor = texture(ColorTex1, varTexCoord, 0.0);
    float n = texture(ColorTex2, varTexCoord, 0.0).r;
    
    if (n == 0.0) {
        fragColor = BackgroundColor;
        return;
    }
    vec3 AvgColor = SumColor.rgb / SumColor.a;
    float AvgAlpha = SumColor.a / n;
    
    float T = pow(1.0-AvgAlpha, n);
    fragColor = vec4(AvgColor * (1 - T) + BackgroundColor.rgb * T, 1.0);
}
