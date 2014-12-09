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
//----------------------------------------------------------------------------
uniform sampler2D BaseSampler;
uniform sampler2D NormalSampler;
in vec2 varTexCoord0;
in vec2 varTexCoord1;
in vec4 varColor0;
out vec4 fragColor;
void p_SimpleBumpMapP()
{
    vec3 kLDir = MapFromUnit(varColor0.rgb);
    vec3 kNDir = MapFromUnit(texture(NormalSampler,varTexCoord1, 0.0).rgb);
    float fDot = clamp(dot(kLDir,kNDir), 0.0, 1.0);
    vec3 kBaseColor = texture(BaseSampler,varTexCoord0, 0.0).rgb;
    fragColor.rgb = fDot*kBaseColor;
    fragColor.a = 1.0;
}
//----------------------------------------------------------------------------
