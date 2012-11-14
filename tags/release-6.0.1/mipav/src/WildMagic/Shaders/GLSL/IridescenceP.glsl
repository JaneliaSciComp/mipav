//----------------------------------------------------------------------------
vec3 MapFromUnit (vec3 kVector)
{
    // map [0,1] to [-1,1]
    vec3 kV = vec3(0.0,0.0,0.0);
    kV.x = 2.0*kVector.x - 1.0;
    kV.y = 2.0*kVector.y - 1.0;
    kV.z = 2.0*kVector.z - 1.0;
    //return 2.0*kVector - (1.0,1.0,1.0);
    return kV;
}

uniform sampler2D BaseSampler;
uniform sampler1D GradientSampler;

void p_IridescenceP ()
{
     //  Map the vectors to [-1,1]^3.
    vec3 kWorldNormal = MapFromUnit(gl_TexCoord[2].xyz);
    vec3 kEyeDirection = MapFromUnit(gl_TexCoord[3].xyz);
    
    // Calculate a Fresnel factor for a view-dependent lookup into a gradient
    // texture.  A different color/saturation occurs depending on what angle
    // you view at.
    float fFresnel = 1.0 + dot(kWorldNormal,kEyeDirection);
    fFresnel = fFresnel*fFresnel;

    vec3 kBaseColor = texture2D(BaseSampler,gl_TexCoord[0].xy).xyz;

    // The small perturbation of the Fresnel factor eliminates some spotting
    // where values are nearly zero. saturate in cg is clamp in glsl
    float fGradientTCoord = clamp(fFresnel+1.0/256.0, 0.0, 1.0);
    vec3 kGradientColor = texture1D(GradientSampler,fGradientTCoord).xyz;

    // Blend the colors for the pixel color. lerp in cg is mix in glsl
    gl_FragColor.rgb = mix(kBaseColor,kGradientColor,gl_TexCoord[1].x);
    //gl_FragColor.rgb = kBaseColor * gl_TexCoord[1].x + kGradientColor * ( 1.0 - gl_TexCoord[1].x);
    //gl_FragColor.rgb = kGradientColor;
    //gl_FragColor.rgb = kBaseColor;
    //gl_FragColor.a = 1.0;
}
