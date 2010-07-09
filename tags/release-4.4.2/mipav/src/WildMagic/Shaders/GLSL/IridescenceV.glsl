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
uniform mat4 WMatrix;
uniform vec3 CameraWorldPosition;
uniform float InterpolateFactor;
//----------------------------------------------------------------------------
void  v_IridescenceV()
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
