uniform sampler2D BaseSampler;
uniform float MyAlpha;
in vec3 varTexCoord;
in vec4 varColor;
layout(location = 0) out vec4     outFragData0;
void p_MaterialTextureP()
{
    // Add the material and texture colors.
    vec4 kBaseColor = texture2D(BaseSampler,varTexCoord.xy);
    outFragData0.rgb = clamp(kBaseColor.rgb + varColor.rgb, 0.0, 1.0);
    
    // Multiply the material and texture alphas.
    outFragData0.a = 1.0;
}
//----------------------------------------------------------------------------
