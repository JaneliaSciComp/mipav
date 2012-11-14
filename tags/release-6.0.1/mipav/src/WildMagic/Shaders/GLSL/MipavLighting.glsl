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
    float fDiff = 0.0;
    float fSpec = 0.0;

//     fDiff = -dot(kModelNormal,kLightDirection);
//     if (fDiff > 0.0)
//     {
//         vec3 kViewVector = normalize(kCameraPosition - kModelPosition);
//         vec3 kHalfVector = normalize(kViewVector - kLightDirection);
//         fSpec = dot(kModelNormal,kHalfVector);
//         if (fSpec > 0.0)
//         {
//             fSpec = pow(fSpec,fSpecularExponent);
//         }
//         else
//         {
//             fSpec = 0.0;
//         }
//     }
//     else
//     {
//         fDiff = 0.0;
//         fSpec = 0.0;
//     }
    
//     fDiffuseFactor = fDiff;
//     fSpecularFactor = fSpec;

    vec3 V = normalize(kCameraPosition - kModelPosition);
    vec3 N = normalize(kModelNormal);
    
    vec3 L = vec3(kLightDirection.x, kLightDirection.y, kLightDirection.z);
    L = -L;
    L = normalize(L);
    
    vec3 R = vec3(N.x, N.y, N.z);
    R = R * ( 2 * dot(L, N));
    R = R - L;
    R = normalize(R);
    
    float cos_theta = dot(L, N);
    float cos_alpha = dot(R, V);
    
    if ( cos_theta > 0 ) {
       fDiff = 1.0f * cos_theta;
    }
    if ( cos_alpha > 0 ) {
       fSpec = pow(cos_alpha, fSpecularExponent);
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
//     float fDiff, fSpec;

//     vec3 kVertexDirection = normalize(kModelPosition - kLightPosition);
//     fDiff = -dot(kModelNormal,kVertexDirection);
//     if (fDiff > 0.0)
//     {
//         vec3 kViewVector = normalize(kCameraPosition - kModelPosition);
//         vec3 kHalfVector = normalize(kViewVector - kVertexDirection);
//         fSpec = dot(kModelNormal,kHalfVector);
//         if (fSpec > 0.0)
//         {
//             fSpec = pow(fSpec,fSpecularExponent);
//         }
//         else
//         {
//             fSpec = 0.0;
//         }
//     }
//     else
//     {
//         fDiff = 0.0;
//         fSpec = 0.0;
//     }

//     fDiffuseFactor = fDiff;
//     fSpecularFactor = fSpec;

    float fDiff = 0;
    float fSpec = 0;
    float cos_theta = 0;
    float cos_alpha = 0;

    
    vec3 kVertexDirection = normalize(kModelPosition - kLightPosition);
    fDiff = -dot(kModelNormal,kVertexDirection);
    if (fDiff > 0.0f)
    {
        vec3 kViewVector = normalize(kCameraPosition - kModelPosition);
        vec3 kHalfVector = normalize(kViewVector - kVertexDirection);
        fSpec = dot(kModelNormal,kHalfVector);
        if (fSpec > 0.0f)
        {
            fSpec = pow(fSpec,fSpecularExponent);
        }
        else
        {
            fSpec = 0.0f;
        }
    }
    else
    {
        fDiff = 0.0f;
        fSpec = 0.0f;
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

float GetAttenuation
(
 vec3   kModelPos,
 vec3   kLightPos, 
 vec4   kAttenuation)
{
    // Attenuate the color (x=constant, y=linear, z=quadratic, w=intensity).
    // Attenuation is not active when the x component is zero.  The distance
    // must be computed in *world* coordinates.  The distance in camera
    // coordinates is not correct when the MVP matrix has nonunit scaling
    // factors.
    
    vec3 kVertexWorldDir = kModelPos - kLightPos;
    //vec3 kVertexModelDir = kModelPos - kLightPos;
    //vec3 kVertexWorldDir = mul(kVertexModelDir,kWMatrix);
    float fDistance = sqrt(
                           kVertexWorldDir.x*kVertexWorldDir.x +
                           kVertexWorldDir.y*kVertexWorldDir.y +
                           kVertexWorldDir.z*kVertexWorldDir.z);

    float fAttn = kAttenuation.w/(kAttenuation.x + fDistance*(kAttenuation.y
                                                              + fDistance*kAttenuation.z));
    
    return fAttn;
}

vec4 AmbientLight(  vec3   MaterialEmissive,
                    vec3   MaterialAmbient,
                    vec3   LightAmbient,
                    vec4   LightAttenuation )
{
    vec3 kLAmb = LightAttenuation.w*LightAmbient;
    vec4 kVertexColor = vec4(0.0);
    kVertexColor.rgb = MaterialEmissive + MaterialAmbient*kLAmb;
    kVertexColor.a = 1.0;
    return kVertexColor;
}

vec4 DirectionalLight(  vec3 kModelPosition,
                        vec3 kModelNormal,
                        vec3   CameraWorldPosition,
                        vec3   MaterialEmissive,
                        vec3   MaterialAmbient,
                        vec4   MaterialDiffuse,
                        vec4   MaterialSpecular,
                        vec3   LightDirection,
                        vec3   LightAmbient,
                        vec3   LightDiffuse,
                        vec3   LightSpecular,
                        vec4   LightAttenuation )
    
{
    vec4 kVertexColor = vec4(0.0);
    float fDiff, fSpec;
    GetDirectionalLightFactors(kModelPosition,kModelNormal,
                               CameraWorldPosition,LightDirection,MaterialSpecular.a,
                               fDiff,fSpec);
    vec3 kColor = MaterialAmbient*LightAmbient;
    if (fDiff > 0.0)
    {
        kColor += fDiff*MaterialDiffuse.rgb*LightDiffuse;
        if (fSpec > 0.0)
        {
            kColor += fSpec*MaterialSpecular.rgb*LightSpecular;
        }
    }

    kVertexColor.rgb = MaterialEmissive + LightAttenuation.w*kColor;
    kVertexColor.a = 1.0;
    return kVertexColor;
}

vec4 PointLight(     vec3 kModelPosition,
                     vec3 kModelNormal,
                     vec3   CameraWorldPosition,
                     vec3   MaterialEmissive,
                     vec3   MaterialAmbient,
                     vec4   MaterialDiffuse,
                     vec4   MaterialSpecular,
                     vec3   LightWorldPosition,
                     vec3   LightAmbient,
                     vec3   LightDiffuse,
                     vec3   LightSpecular,
                     vec4   LightAttenuation )
{
    vec4 kVertexColor = vec4(0.0);
    float fDiff, fSpec;
    GetPointLightFactors(kModelPosition.xyz,kModelNormal,
                         CameraWorldPosition,LightWorldPosition,MaterialSpecular.a,
                         fDiff,fSpec);

    float fAttn = GetAttenuation(kModelPosition.xyz,
                                 LightWorldPosition,LightAttenuation);

    vec3 kColor = MaterialAmbient*LightAmbient;
    if (fDiff > 0.0)
    {
        kColor += fDiff*MaterialDiffuse.xyz*LightDiffuse;
        if (fSpec > 0.0)
        {
            kColor += fSpec*MaterialSpecular.xyz*LightSpecular;
        }
    }

    //kVertexColor.rgb = MaterialEmissive + fAttn*kColor;
    kVertexColor.rgb = MaterialEmissive + kColor;
    kVertexColor.a = MaterialDiffuse.a;
    return kVertexColor;
}

vec4 SpotLight(     vec3 kModelPosition,
                    vec3 kModelNormal,
                    vec3 CameraWorldPosition,
                    vec3 MaterialEmissive,
                    vec3 MaterialAmbient,
                    vec4 MaterialDiffuse,
                    vec4 MaterialSpecular,
                    vec3 LightWorldPosition,
                    vec3 LightWorldDirection,
                    vec3 LightAmbient,
                    vec3 LightDiffuse,
                    vec3 LightSpecular,
                    vec4 LightSpotCutoff,
                    vec4 LightAttenuation )
{
    vec4 kVertexColor = vec4(0.0);
    float fDiff, fSpec, fSpot;
    GetSpotLightFactors(kModelPosition.xyz,kModelNormal,
                        CameraWorldPosition,LightWorldPosition,MaterialSpecular.a,
                        LightWorldDirection,LightSpotCutoff.y,LightSpotCutoff.w,fDiff,
                        fSpec,fSpot);

    float fAttn = GetAttenuation(kModelPosition.xyz,
                                 LightWorldPosition,LightAttenuation);

    vec3 kColor = MaterialAmbient*LightAmbient;
    if (fSpot > 0.0)
    {
        if (fDiff > 0.0)
        {
            kColor += (fSpot*fDiff)*MaterialDiffuse.rgb*LightDiffuse;
            if (fSpec > 0.0)
            {
                kColor += (fSpot*fSpec)*MaterialSpecular.rgb*LightSpecular;
            }
        }
    }
    
    kVertexColor.rgb = MaterialEmissive + fAttn*kColor;
    kVertexColor.a = MaterialDiffuse.a;
    return kVertexColor;
}


/**
 * Compute the surface color based on lighting.
 */
vec4 computeColor( vec3 kModelPosition, vec3 kModelNormal, vec3 CameraWorldPosition, 
                   vec3 MaterialEmissive, vec3 MaterialAmbient, vec4 MaterialDiffuse, vec4 MaterialSpecular,
                   vec4 LightAmbient, vec4 LightDiffuse, vec4 LightSpecular,
                   vec4 LightWorldPosition, vec4 LightWorldDirection,
                   vec4 LightSpotCutoff, vec4 LightAttenuation,
                   float LightType )
{
    vec4 kReturn = vec4(0.0);
    
    if ( LightType == 0.0 )
    {
        kReturn = AmbientLight( MaterialEmissive.xyz,
                                MaterialAmbient.xyz,
                                LightAmbient.xyz,
                                LightAttenuation );
    }
    else if ( LightType == 1.0 )
    {
        kReturn = DirectionalLight(  kModelPosition.xyz,
                                     kModelNormal.xyz,
                                     CameraWorldPosition.xyz,
                                     MaterialEmissive.xyz,
                                     MaterialAmbient.xyz,
                                     MaterialDiffuse,
                                     MaterialSpecular,
                                     LightWorldDirection.xyz,
                                     LightAmbient.xyz,
                                     LightDiffuse.xyz,
                                     LightSpecular.xyz,
                                     LightAttenuation);
        
    }
    else if ( LightType == 2.0 )
    {
        kReturn = PointLight( kModelPosition.xyz,
                              kModelNormal.xyz,
                              CameraWorldPosition.xyz,
                              MaterialEmissive.xyz,
                              MaterialAmbient.xyz,
                              MaterialDiffuse,
                              MaterialSpecular,
                              LightWorldPosition.xyz,
                              LightAmbient.xyz,
                              LightDiffuse.xyz,
                              LightSpecular.xyz,
                              LightAttenuation);
    }
    else
    {
        kReturn = SpotLight( kModelPosition.xyz,
                             kModelNormal.xyz,
                             CameraWorldPosition.xyz,
                             MaterialEmissive.xyz,
                             MaterialAmbient.xyz,
                             MaterialDiffuse,
                             MaterialSpecular,
                             LightWorldPosition.xyz,
                             LightWorldDirection.xyz,
                             LightAmbient.xyz,
                             LightDiffuse.xyz,
                             LightSpecular.xyz,
                             LightSpotCutoff,
                             LightAttenuation);
    }
    return kReturn;
}

uniform vec3 MaterialEmissive;
uniform vec3 MaterialAmbient;
uniform vec4 MaterialDiffuse;
uniform vec4 MaterialSpecular;
                     
uniform float  Light0Type;
uniform vec4 Light0Ambient;
uniform vec4 Light0Diffuse;
uniform vec4 Light0Specular;
uniform vec4 Light0SpotCutoff;
uniform vec4 Light0Attenuation;
uniform vec4 Light0ModelPosition;
uniform vec4 Light0ModelDirection;
                     
uniform vec4 Light1Ambient;
uniform vec4 Light1Attenuation;
                     
uniform float  Light2Type;
uniform vec4 Light2Ambient;
uniform vec4 Light2Diffuse;
uniform vec4 Light2Specular;
uniform vec4 Light2SpotCutoff;
uniform vec4 Light2Attenuation;
uniform vec4 Light2WorldPosition;
uniform vec4 Light2WorldDirection;
                     
uniform float  Light3Type;
uniform vec4 Light3Ambient;
uniform vec4 Light3Diffuse;
uniform vec4 Light3Specular;
uniform vec4 Light3SpotCutoff;
uniform vec4 Light3Attenuation;
uniform vec4 Light3WorldPosition;
uniform vec4 Light3WorldDirection;
                     
uniform vec3 CameraModelPosition;
uniform vec3 CameraWorldPosition;
uniform mat4 WMatrix;
uniform mat4 WVPMatrix;
uniform float ReverseFace;

void v_MipavLighting()

{
    vec3 kModelNormal = gl_Normal.xyz;
    if ( ReverseFace == 1 )
    {
        kModelNormal.x *= -1;
        kModelNormal.y *= -1;
        kModelNormal.z *= -1;
    }

    // Transform the position from model space to clip space.
    gl_Position = WVPMatrix*gl_Vertex;

    // First light is static light:
    vec4 color0 = computeColor( gl_Vertex.xyz, kModelNormal.xyz, CameraModelPosition.xyz,
                                MaterialEmissive.xyz,  MaterialAmbient.xyz, MaterialDiffuse, MaterialSpecular,
                                Light0Ambient, Light0Diffuse, Light0Specular,
                                Light0ModelPosition, Light0ModelDirection,
                                Light0SpotCutoff, Light0Attenuation,
                                Light0Type );

    // Assume second light is alwasy an ambient light:
    vec4 color1 = AmbientLight( MaterialEmissive.xyz,  MaterialAmbient.xyz, 
                                Light1Ambient.xyz,
                                Light1Attenuation );
    
    // Remaining lights:
    vec4 color2 = computeColor( gl_Vertex.xyz, kModelNormal.xyz, CameraModelPosition.xyz,
                                MaterialEmissive.xyz,  MaterialAmbient.xyz, MaterialDiffuse, MaterialSpecular,
                                Light2Ambient, Light2Diffuse, Light2Specular,
                                Light2WorldPosition, Light2WorldDirection,
                                Light2SpotCutoff, Light2Attenuation,
                                Light2Type );
    
    vec4 color3 = computeColor( gl_Vertex.xyz, kModelNormal.xyz, CameraModelPosition.xyz,
                                MaterialEmissive.xyz,  MaterialAmbient.xyz, MaterialDiffuse, MaterialSpecular,
                                Light3Ambient, Light3Diffuse, Light3Specular,
                                Light3WorldPosition, Light3WorldDirection,
                                Light3SpotCutoff, Light3Attenuation,
                                Light3Type );
    
    gl_FrontColor = color0 + color1 + color2 + color3;
    gl_FrontColor.a = 1.0;
}
