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
//----------------------------------------------------------------------------
uniform mat4 WVPMatrix;
void v_SimpleBumpMap()
{
    // Transform the position from model space to clip space.
    gl_Position = WVPMatrix*gl_Vertex;

    // Pass through the parameters.
    gl_TexCoord[0] = gl_MultiTexCoord0;
    gl_TexCoord[1] = gl_MultiTexCoord1;
    gl_FrontColor.rgb = MapToUnit(gl_Color);
}
//----------------------------------------------------------------------------
uniform sampler2D BaseSampler;
uniform sampler2D NormalSampler;
void p_SimpleBumpMap()
{
    vec3 kLDir = MapFromUnit(gl_Color);
    vec3 kNDir = MapFromUnit(texture2D(NormalSampler,gl_TexCoord[1]).rgb);
    float fDot = clamp(dot(kLDir,kNDir), 0.0, 1.0);
    vec3 kBaseColor = texture2D(BaseSampler,gl_TexCoord[0]).rgb;
    gl_FragColor.rgb = fDot*kBaseColor;
    gl_FragColor.a = 1.0;
}
//----------------------------------------------------------------------------
