uniform sampler2D BaseSampler;
uniform float MyAlpha;
in vec2 varTexCoord;
in vec4 varColor;
layout(location = 0) out vec4     outFragData0;
layout(location = 1) out vec4     outFragData1;
void p_OrderIndpTransparencyInitP()
{
    // Add the material and texture colors.
    vec4 kBaseColor = texture(BaseSampler,varTexCoord.xy, 0.0);
    vec4 color;
    color.rgb = clamp(kBaseColor.rgb + varColor.rgb, 0.0, 1.0);
    color.a = MyAlpha;

    outFragData0 = vec4(color.rgb * color.a, color.a);
    outFragData1 = vec4(1.0);
}
