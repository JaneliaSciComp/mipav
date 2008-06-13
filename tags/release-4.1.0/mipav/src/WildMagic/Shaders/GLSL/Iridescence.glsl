
//----------------------------------------------------------------------------
// DirectX wants texture coordinates with components in [0,1].
//----------------------------------------------------------------------------
vec3 MapToUnit (vec3 kVector)
{
    // map [-1,1] to [0,1]
    return 0.5*kVector + 0.5;
}
//----------------------------------------------------------------------------
vec3 MapFromUnit (vec3 kVector)
{
    // map [0,1] to [-1,1]
    return 2.0*kVector - 1.0;
}

uniform mat4 WVPMatrix;
uniform mat4 WMatrix;
uniform vec3 CameraWorldPosition;
uniform float InterpolateFactor;

//----------------------------------------------------------------------------
void  v_Iridescence()
{

    // Transform the position from model space to clip space.
    gl_Position = WVPMatrix * gl_Vertex;
    
    // Transform the position from model space to world space.
    vec3 kWorldPosition = (WMatrix * gl_Vertex).xyz;

    // Transform the normal from model space to world space.  In case the
    // model-to-world matrix has nonunit scales, the resulting vector must
    // be normalized.  Map the vector to [0,1]^3.
    vec3 kWorldNormal = MapToUnit(normalize( (WMatrix * vec4(gl_Normal, 0.0) ).xyz));

    // Calculate the eye direction.  Map the vector to [0,1]^3.
    vec3 kEyeDirection = MapToUnit(normalize(kWorldPosition-CameraWorldPosition));

    // Pass through the base texture coordinate.
    gl_TexCoord[0].xy = gl_MultiTexCoord0.xy;

    // Pass through the interpolation factor.
    gl_TexCoord[1].x = InterpolateFactor;

    
    gl_Position = WVPMatrix * gl_Vertex;

    gl_TexCoord[3].xyz = kEyeDirection;
    gl_TexCoord[2].xyz = kWorldNormal;

}

//float saturate(float x)
//{
//    return max(0, min(1, x));
//}

uniform sampler2D BaseSampler;
uniform sampler1D GradientSampler;

void p_Iridescence ()
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
