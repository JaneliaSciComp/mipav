//----------------------------------------------------------------------------
// Shared functions.
//----------------------------------------------------------------------------
void GetDirectionalLightFactors
(
    vec3    kModelPosition,
    vec3    kModelNormal,
    vec3    kCameraPosition,
    vec3    kLightDirection,
    float     fSpecularExponent,
    out float fDiffuseFactor,
    out float fSpecularFactor)
{
    float fDiff, fSpec;

    fDiff = -dot(kModelNormal,kLightDirection);
    if (fDiff > 0.0)
    {
        vec3 kViewVector = normalize(kCameraPosition - kModelPosition);
        vec3 kHalfVector = normalize(kViewVector - kLightDirection);
        fSpec = dot(kModelNormal,kHalfVector);
        if (fSpec > 0.0)
        {
            fSpec = pow(fSpec,fSpecularExponent);
        }
        else
        {
            fSpec = 0.0;
        }
    }
    else
    {
        fDiff = 0.0;
        fSpec = 0.0;
    }
    
    fDiffuseFactor = fDiff;
    fSpecularFactor = fSpec;
}
//----------------------------------------------------------------------------
void GetPointLightFactors
(
    vec3    kModelPosition,
    vec3    kModelNormal,
    vec3    kCameraPosition,
    vec3    kLightPosition,
    float     fSpecularExponent,
    out float fDiffuseFactor,
    out float fSpecularFactor)
{
    float fDiff, fSpec;

    vec3 kVertexDirection = normalize(kModelPosition - kLightPosition);
    fDiff = -dot(kModelNormal,kVertexDirection);
    if (fDiff > 0.0)
    {
        vec3 kViewVector = normalize(kCameraPosition - kModelPosition);
        vec3 kHalfVector = normalize(kViewVector - kVertexDirection);
        fSpec = dot(kModelNormal,kHalfVector);
        if (fSpec > 0.0)
        {
            fSpec = pow(fSpec,fSpecularExponent);
        }
        else
        {
            fSpec = 0.0;
        }
    }
    else
    {
        fDiff = 0.0;
        fSpec = 0.0;
    }

    fDiffuseFactor = fDiff;
    fSpecularFactor = fSpec;
}
//----------------------------------------------------------------------------
void GetSpotLightFactors
(
    vec3    kModelPosition,
    vec3    kModelNormal,
    vec3    kCameraPosition,
    vec3    kLightPosition,
    float     fSpecularExponent,
    vec3    kSpotAxis,
    float     fSpotCosAngle,
    float     fSpotExponent,
    out float fDiffuseFactor,
    out float fSpecularFactor,
    out float fSpotFactor)
{
    float fDiff, fSpec, fSpot;

    vec3 kVertexDirection = normalize(kModelPosition - kLightPosition);
    float fVertexCosAngle = dot(kSpotAxis,kVertexDirection);
    if (fVertexCosAngle >= fSpotCosAngle)
    {
        fDiff = -dot(kModelNormal,kVertexDirection);
        if (fDiff > 0.0)
        {
            vec3 kViewVector = normalize(kCameraPosition - kModelPosition);
            vec3 kHalfVector = normalize(kViewVector - kVertexDirection);
            fSpec = dot(kModelNormal,kHalfVector);
            if (fSpec > 0.0)
            {
                fSpec = pow(fSpec,fSpecularExponent);
            }
            else
            {
                fSpec = 0.0;
            }
            fSpot = pow(fVertexCosAngle,fSpotExponent);
        }
        else
        {
            fDiff = 0.0;
            fSpec = 0.0;
            fSpot = 0.0;
        }
    }
    else
    {
        fDiff = 0.0;
        fSpec = 0.0;
        fSpot = 0.0;
    }

    fDiffuseFactor = fDiff;
    fSpecularFactor = fSpec;
    fSpotFactor = fSpot;
}
//----------------------------------------------------------------------------
float GetAttenuation
(
    mat4   kWMatrix,
    vec3   kModelPos,
    vec3   kLightPos, 
    vec4   kAttenuation)
{
    // Attenuate the color (x=constant, y=linear, z=quadratic, w=intensity).
    // Attenuation is not active when the x component is zero.  The distance
    // must be computed in *world* coordinates.  The distance in camera
    // coordinates is not correct when the MVP matrix has nonunit scaling
    // factors.
    
    vec3 kVertexModelDir = kModelPos - kLightPos;
    vec3 kVertexWorldDir = (kWMatrix * vec4(kVertexModelDir, 0.0) ).xyz;
    float fDistance = sqrt(
        kVertexWorldDir.x*kVertexWorldDir.x +
        kVertexWorldDir.y*kVertexWorldDir.y +
        kVertexWorldDir.z*kVertexWorldDir.z);

    float fAttn = kAttenuation.w/(kAttenuation.x + fDistance*(kAttenuation.y
        + fDistance*kAttenuation.z));
    
    return fAttn;
}
//----------------------------------------------------------------------------

//----------------------------------------------------------------------------
// Vertex shaders for lighting.  The function names start with "L", for
// lighting, and end with "VProgram" to support the program cataloging system
// in the engine.  After "L" is the number of lights (currently 1 through 8).
// After the number is a list of the light types.  Ambient lighting uses "a",
// directional lighting uses "d", point lighting uses "p", and spot lighting
// uses "s".  For example, the function L3apdVProgram represents three lights,
// one of them ambient, one of them point, and one of them directional.
//----------------------------------------------------------------------------

uniform mat4 WVPMatrix;
uniform vec3   MaterialEmissive;
uniform vec3   MaterialAmbient;
uniform vec3   Light0Ambient;
uniform vec4   Light0Attenuation;

//----------------------------------------------------------------------------
// L1
//----------------------------------------------------------------------------
void v_L1a ()
{
    // Transform the position from model space to clip space.
    gl_Position = WVPMatrix * gl_Vertex;

    vec3 kLAmb = Light0Attenuation.w*Light0Ambient;
    gl_FrontColor.rgb = MaterialEmissive + MaterialAmbient*kLAmb;
    gl_FrontColor.a = 1.0;
}
//----------------------------------------------------------------------------

//uniform mat4 WVPMatrix;
uniform vec3   CameraModelPosition;
//uniform vec3   MaterialEmissive;
//uniform vec3   MaterialAmbient;
uniform vec4   MaterialDiffuse;
uniform vec4   MaterialSpecular;
uniform vec3   Light0ModelDirection;
//uniform vec3   Light0Ambient;
uniform vec3   Light0Diffuse;
uniform vec3   Light0Specular;
//uniform vec4   Light0Attenuation;
void v_L1d ()
{
    // Transform the position from model space to clip space.
    gl_Position = WVPMatrix * gl_Vertex;

    float fDiff, fSpec;
    GetDirectionalLightFactors(gl_Vertex.xyz,gl_Normal,
        CameraModelPosition,Light0ModelDirection,MaterialSpecular.a,
        fDiff,fSpec);

    vec3 kColor = MaterialAmbient*Light0Ambient;
    if (fDiff > 0.0)
    {
        kColor += fDiff*MaterialDiffuse.rgb*Light0Diffuse;
        if (fSpec > 0.0)
        {
            kColor += fSpec*MaterialSpecular.rgb*Light0Specular;
        }
    }

    gl_FrontColor.rgb = MaterialEmissive + Light0Attenuation.w*kColor;
    gl_FrontColor.a = MaterialDiffuse.a;
}
//----------------------------------------------------------------------------
// uniform mat4 WVPMatrix;
uniform mat4 WMatrix;
// uniform vec3   CameraModelPosition;
// uniform vec3   MaterialEmissive;
// uniform vec3   MaterialAmbient;
// uniform vec4   MaterialDiffuse;
// uniform vec4   MaterialSpecular;
uniform vec3   Light0ModelPosition;
// uniform vec3   Light0Ambient;
// uniform vec3   Light0Diffuse;
// uniform vec3   Light0Specular;
// uniform vec4   Light0Attenuation;
void v_L1p ()
{
    // Transform the position from model space to clip space.
    gl_Position = WVPMatrix * gl_Vertex;

    float fDiff, fSpec;
    GetPointLightFactors(gl_Vertex.xyz,gl_Normal,
        CameraModelPosition,Light0ModelPosition,MaterialSpecular.a,
        fDiff,fSpec);

    float fAttn = GetAttenuation(WMatrix,gl_Vertex.xyz,
        Light0ModelPosition,Light0Attenuation);

    vec3 kColor = MaterialAmbient*Light0Ambient;
    if (fDiff > 0.0)
    {
        kColor += fDiff*MaterialDiffuse.xyz*Light0Diffuse;
        if (fSpec > 0.0)
        {
            kColor += fSpec*MaterialSpecular.xyz*Light0Specular;
        }
    }

    gl_FrontColor.rgb = MaterialEmissive + fAttn*kColor;
    gl_FrontColor.a = MaterialDiffuse.a;
}
//----------------------------------------------------------------------------
// uniform mat4 WVPMatrix;
// uniform mat4 WMatrix;
// uniform vec3 CameraModelPosition;
// uniform vec3 MaterialEmissive;
// uniform vec3 MaterialAmbient;
// uniform vec4 MaterialDiffuse;
// uniform vec4 MaterialSpecular;
// uniform vec3 Light0ModelPosition;
// uniform vec3 Light0ModelDirection;
// uniform vec3 Light0Ambient;
// uniform vec3 Light0Diffuse;
// uniform vec3 Light0Specular;
uniform vec4 Light0SpotCutoff;
// uniform vec4 Light0Attenuation;
void v_L1s ()
{
    // Transform the position from model space to clip space.
    gl_Position = WVPMatrix * gl_Vertex;

    float fDiff, fSpec, fSpot;
    GetSpotLightFactors(gl_Vertex.xyz,gl_Normal,
        CameraModelPosition,Light0ModelPosition,MaterialSpecular.a,
        Light0ModelDirection,Light0SpotCutoff.y,Light0SpotCutoff.w,fDiff,
        fSpec,fSpot);

    float fAttn = GetAttenuation(WMatrix,gl_Vertex.xyz,
        Light0ModelPosition,Light0Attenuation);

    vec3 kColor = MaterialAmbient*Light0Ambient;
    if (fSpot > 0.0)
    {
        if (fDiff > 0.0)
        {
            kColor += (fSpot*fDiff)*MaterialDiffuse.rgb*Light0Diffuse;
            if (fSpec > 0.0)
            {
                kColor += (fSpot*fSpec)*MaterialSpecular.rgb*Light0Specular;
            }
        }
    }
    
    gl_FrontColor.rgb = MaterialEmissive + fAttn*kColor;
    gl_FrontColor.a = MaterialDiffuse.a;
}
//----------------------------------------------------------------------------

//----------------------------------------------------------------------------
// L2
//----------------------------------------------------------------------------
// uniform mat4 WVPMatrix;
// uniform vec3   MaterialEmissive;
// uniform vec3   MaterialAmbient;
// uniform vec3   Light0Ambient;
// uniform vec4   Light0Attenuation;
uniform vec3   Light1Ambient;
uniform vec4   Light1Attenuation;
void v_L2aa ()
{
    // Transform the position from model space to clip space.
    gl_Position = WVPMatrix * gl_Vertex;

    vec3 kLAmb =
        Light0Attenuation.w*Light0Ambient +
        Light1Attenuation.w*Light1Ambient;

    gl_FrontColor.xyz = MaterialEmissive + MaterialAmbient*kLAmb;
    gl_FrontColor.a = 1.0;
}
//----------------------------------------------------------------------------
// uniform mat4 WVPMatrix;
// uniform vec3   CameraModelPosition;
// uniform vec3   MaterialEmissive;
// uniform vec3   MaterialAmbient;
// uniform vec4   MaterialDiffuse;
// uniform vec4   MaterialSpecular;
// uniform vec3   Light0Ambient;
// uniform vec4   Light0Attenuation;
uniform vec3   Light1ModelDirection;
// uniform vec3   Light1Ambient;
uniform vec3   Light1Diffuse;
uniform vec3   Light1Specular;
// uniform vec4   Light1Attenuation;
void v_L2ad ()
{
    // Transform the position from model space to clip space.
    gl_Position = WVPMatrix * gl_Vertex;

    float fDiff, fSpec;
    GetDirectionalLightFactors(gl_Vertex.xyz,gl_Normal,
        CameraModelPosition,Light1ModelDirection,MaterialSpecular.a,
        fDiff,fSpec);

    vec3 kLAmb =
        Light0Attenuation.w*Light0Ambient +
        Light1Attenuation.w*Light1Ambient;

    vec3 kColor = MaterialAmbient*kLAmb;
    if (fDiff > 0.0)
    {
        float fProd = Light0Attenuation.w*fDiff;
        kColor += fProd*MaterialDiffuse.rgb*Light1Diffuse;
        if (fSpec > 0.0)
        {
            fProd = Light0Attenuation.w*fSpec;
            kColor += fProd*MaterialSpecular.rgb*Light1Specular;
        }
    }

    gl_FrontColor.xyz = MaterialEmissive + kColor;
    gl_FrontColor.a = MaterialDiffuse.a;
}
//----------------------------------------------------------------------------
