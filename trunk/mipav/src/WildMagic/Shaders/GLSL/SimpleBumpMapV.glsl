//----------------------------------------------------------------------------
vec3 MapToUnit (vec3 kVector)
{
    // map [-1,1] to [0,1]
    vec3 kV = vec3(0.0,0.0,0.0);
    kV.x = 0.5*kVector.x + 0.5;
    kV.y = 0.5*kVector.y + 0.5;
    kV.z = 0.5*kVector.z + 0.5;
    //return 0.5*kVector + 0.5;
    return kV;
}
//----------------------------------------------------------------------------
uniform mat4 WVPMatrix;
in vec3 inPosition;
in vec2 inTexCoord0;
in vec2 inTexCoord1;
in vec4 inColor0;
out vec2 varTexCoord0;
out vec2 varTexCoord1;
out vec4 varColor0;
void v_SimpleBumpMapV()
{
    // Transform the position from model space to clip space.
    gl_Position = WVPMatrix*vec4(inPosition, 1.0);

    // Pass through the parameters.
    varTexCoord0 = inTexCoord0;
    varTexCoord1 = inTexCoord1;
    varColor0.rgb = MapToUnit(inColor0.rgb);
    varColor0.a = 1.0;
}
//----------------------------------------------------------------------------
