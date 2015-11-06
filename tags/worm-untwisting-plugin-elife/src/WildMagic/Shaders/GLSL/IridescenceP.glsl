//----------------------------------------------------------------------------
vec3 MapFromUnit (vec3 kVector)
{
    // map [0,1] to [-1,1]
    vec3 kV = vec3(0.0,0.0,0.0);
    kV.x = 2.0*kVector.x - 1.0;
    kV.y = 2.0*kVector.y - 1.0;
    kV.z = 2.0*kVector.z - 1.0;
    return kV;
}

uniform sampler2D BaseSampler;
uniform sampler1D GradientSampler;
in vec2 varTexCoord0;
in vec2 varTexCoord1;
in vec3 varTexCoord2;
in vec3 varTexCoord3;
out vec4 fragColor;
void p_IridescenceP ()
{
     //  Map the vectors to [-1,1]^3.
    vec3 kWorldNormal = MapFromUnit(varTexCoord2.xyz);
    vec3 kEyeDirection = MapFromUnit(varTexCoord3.xyz);
    
    // Calculate a Fresnel factor for a view-dependent lookup into a gradient
    // texture.  A different color/saturation occurs depending on what angle
    // you view at.
    float fFresnel = 1.0 + dot(kWorldNormal,kEyeDirection);
    fFresnel = fFresnel*fFresnel;

    vec3 kBaseColor = texture(BaseSampler,varTexCoord0.xy, 0.0).xyz;

    // The small perturbation of the Fresnel factor eliminates some spotting
    // where values are nearly zero. saturate in cg is clamp in glsl
    float fGradientTCoord = clamp(fFresnel+1.0/256.0, 0.0, 1.0);
    vec3 kGradientColor = texture(GradientSampler,fGradientTCoord, 0.0).xyz;

    // Blend the colors for the pixel color. lerp in cg is mix in glsl
    fragColor.rgb = mix(kBaseColor,kGradientColor,varTexCoord1.x);
    fragColor.a = 1.0;
}
