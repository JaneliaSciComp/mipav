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
void p_SimpleBumpMapP()
{
    vec3 kLDir = MapFromUnit(gl_Color.rgb);
    vec3 kNDir = MapFromUnit(texture2D(NormalSampler,gl_TexCoord[1].xy).rgb);
    float fDot = clamp(dot(kLDir,kNDir), 0.0, 1.0);
    vec3 kBaseColor = texture2D(BaseSampler,gl_TexCoord[0].xy).rgb;
    gl_FragColor.rgb = fDot*kBaseColor;
    gl_FragColor.a = 1.0;
}
//----------------------------------------------------------------------------
