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
void v_SimpleBumpMapV()
{
    // Transform the position from model space to clip space.
    gl_Position = WVPMatrix*gl_Vertex;

    // Pass through the parameters.
    gl_TexCoord[0] = gl_MultiTexCoord0;
    gl_TexCoord[1] = gl_MultiTexCoord1;
    gl_FrontColor.rgb = MapToUnit(gl_Color.rgb);
}
//----------------------------------------------------------------------------
