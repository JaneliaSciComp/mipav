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
    
    float cos_theta = 0.0;
    float cos_alpha = 0.0;
    
    vec3 V = normalize(kCameraPosition - kModelPosition);
    vec3 N = normalize(kModelNormal);
    
    vec3 L = vec3(kLightDirection.x, kLightDirection.y, kLightDirection.z);
    L = -L;
    L = normalize(L);
    
    vec3 R = vec3(N.x, N.y, N.z);
    R = R * ( 2.0 * dot(L, N));
    R = R - L;
    R = normalize(R);
    
    cos_theta = dot(L, N);
    cos_alpha = dot(R, V);
    
    if ( cos_theta > 0.0 ) {
       fDiff = 1.0 * cos_theta;
    }
    if ( cos_alpha > 0.0 ) {
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
    float fDiff = 0.0;
    float fSpec = 0.0;
    float cos_theta = 0.0;
    float cos_alpha = 0.0;

    
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
                    vec4   LightAttenuation)

{
    vec4 kResult = vec4(0.0,0.0,0.0,0.0);
    vec3 kLAmb = LightAttenuation.w*LightAmbient;
    kResult.rgb = MaterialEmissive + MaterialAmbient*kLAmb;
    kResult.a = 1.0;
    return kResult;
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
                        vec4   LightAttenuation)
    
{
    vec4 kResult = vec4(0.0,0.0,0.0,0.0);
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

    kResult.rgb = MaterialEmissive + LightAttenuation.w*kColor;
    kResult.a = 1.0;
    return kResult;
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
                     vec4   LightAttenuation)
{
    vec4 kResult = vec4(0.0,0.0,0.0,0.0);
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

    kResult.rgb = MaterialEmissive + fAttn*kColor;
    kResult.a = MaterialDiffuse.a;
    return kResult;
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
                    vec4 LightAttenuation)
{
    vec4 kResult = vec4(0.0,0.0,0.0,0.0);

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
    
    kResult.rgb = MaterialEmissive + fAttn*kColor;
    kResult.a = MaterialDiffuse.a;
    return kResult;
}


/**
 * Compute the surface color based on lighting.
 */
vec4 computeColor( vec3 kModelPosition, vec3 kModelNormal, vec3 CameraWorldPosition, 
                   vec3 MaterialEmissive, vec3 MaterialAmbient, vec4 MaterialDiffuse, vec4 MaterialSpecular,
                   vec3 LightAmbient, vec3 LightDiffuse, vec3 LightSpecular,
                   vec3 LightWorldPosition, vec3 LightWorldDirection,
                   vec4 LightSpotCutoff, vec4 LightAttenuation,
                   float LightType)
{
    vec4 kResult = vec4(0.0,0.0,0.0,0.0);

    if ( LightType == -1.0 )
    {
        kResult.r = 0.0;
        kResult.g = 0.0;
        kResult.b = 0.0;
    }
    else
    {
        if ( LightType == 0.0 )
        {
            kResult = AmbientLight( MaterialEmissive,
                                    MaterialAmbient,
                                    LightAmbient,
                                    LightAttenuation );
        }
        else if ( LightType == 1.0 )
        {
            kResult = DirectionalLight(  kModelPosition,
                                         kModelNormal,
                                         CameraWorldPosition,
                                         MaterialEmissive,
                                         MaterialAmbient,
                                         MaterialDiffuse,
                                         MaterialSpecular,
                                         LightWorldDirection,
                                         LightAmbient,
                                         LightDiffuse,
                                         LightSpecular,
                                         LightAttenuation);

        }
        else if ( LightType == 2.0 )
        {
            kResult = PointLight( kModelPosition,
                                  kModelNormal,
                                  CameraWorldPosition,
                                  MaterialEmissive,
                                  MaterialAmbient,
                                  MaterialDiffuse,
                                  MaterialSpecular,
                                  LightWorldPosition,
                                  LightAmbient,
                                  LightDiffuse,
                                  LightSpecular,
                                  LightAttenuation);
        }
        else
        {
            kResult = SpotLight( kModelPosition,
                                 kModelNormal,
                                 CameraWorldPosition,
                                 MaterialEmissive,
                                 MaterialAmbient,
                                 MaterialDiffuse,
                                 MaterialSpecular,
                                 LightWorldPosition,
                                 LightWorldDirection,
                                 LightAmbient,
                                 LightDiffuse,
                                 LightSpecular,
                                 LightSpotCutoff,
                                 LightAttenuation);
        }
    }
    return kResult;
}

/**
 * Clip the volume based on the x,y,z axes.
 * returns 1 when the volume is clipped, 0 when not clipped.
 */
bool myClip ( vec3 myVec,
              float clipX,
              float clipXInv,
              float clipY,
              float clipYInv,
              float clipZ,
              float clipZInv )
{
    if ( myVec.x > clipX )
    {
        return true;
    }
    if ( myVec.x < clipXInv )
    {
        return true;
    }
    if ( myVec.y > clipY )
    {
        return true;
    }
    if ( myVec.y < clipYInv )
    {
        return true;
    }
    if ( myVec.z > clipZ )
    {
        return true;
    }
    if ( myVec.z < clipZInv )
    {
        return true;
    }
    return false;
}

varying vec4        kInPos;
varying vec3        kInNormal;
uniform sampler3D bVolumeImageA; 
uniform sampler1D cColorMapA; 
uniform sampler3D fVolumeImageNew;
uniform sampler1D gColorMapNew;

uniform float IsColor;
uniform float IsColorNew;
uniform float UseTexture;
uniform float UseImageNew;
uniform float UseLUTNew;
uniform float Blend;
uniform float ClipEnabled;
uniform float DoClip;
uniform float clipX;
uniform float clipXInv;
uniform float clipY;
uniform float clipYInv;
uniform float clipZ;
uniform float clipZInv;
uniform vec4 clipArb;
uniform vec4 clipEye;
uniform vec4 clipEyeInv;

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
uniform mat4 WVPMatrix;


void p_MipavLightingFragmentP()
{
    bool bClipped = false;

    if ( !bClipped && (ClipEnabled == 1.0) )
    {
        // axis-aligned clipping:
        if ( (DoClip != 0.0) && myClip( gl_TexCoord[0].xyz, clipX, clipXInv, clipY, clipYInv, clipZ, clipZInv ) )
        {
            bClipped = true;
        }
        else
        {
            bClipped = false;
            if ( DoClip != 0.0 )
            {
                // eye clipping and arbitrary clipping:
                vec4 aPosition = vec4(0,0,0,0);
                aPosition.xyz = gl_TexCoord[0].xyz - (.5,.5,.5);
                aPosition = WVPMatrix*aPosition;
                aPosition.xyz = aPosition.xyz + (.5,.5,.5);
                float fDot = dot( aPosition.xyz, clipEye.xyz );
                float fDotInv = dot( aPosition.xyz, clipEyeInv.xyz );
                float fDotArb = dot( gl_TexCoord[0].xyz, clipArb.xyz );
                if ( (fDot < clipEye.w) || (fDotInv > clipEyeInv.w) || (fDotArb > clipArb.w) )
                {
                    bClipped = true;
                }
            }
        }
    }
    if ( bClipped == true )
    {
        discard;
    }

    vec4 LocalMaterialDiffuse = MaterialDiffuse;
    vec3 LocalMaterialAmbient = MaterialAmbient;
    vec4 color;

    if ( bClipped == false )
    {
        if ( UseTexture != 0.0 )
        {
            if ( UseImageNew != 0.0 )
            {
                color = texture3D(fVolumeImageNew,gl_TexCoord[0].xyz);
            }
            else
            {
                color = texture3D(bVolumeImageA,gl_TexCoord[0].xyz);
            }
            if ( UseLUTNew != 0.0 )
            {
                if ( IsColorNew != 0.0 )
                {
                    LocalMaterialDiffuse.r = texture1D(gColorMapNew,color.r).r;
                    LocalMaterialDiffuse.g = texture1D(gColorMapNew,color.g).g;
                    LocalMaterialDiffuse.b = texture1D(gColorMapNew,color.b).b;
                }
                else
                {
                    LocalMaterialDiffuse.rgb = texture1D(gColorMapNew,color.r).rgb;
                }
            }
            else
            {
                if ( IsColor != 0.0 )
                {
                    LocalMaterialDiffuse.r = texture1D(cColorMapA,color.r).r;
                    LocalMaterialDiffuse.g = texture1D(cColorMapA,color.g).g;
                    LocalMaterialDiffuse.b = texture1D(cColorMapA,color.b).b;
                }
                else
                {
                    LocalMaterialDiffuse.rgb = texture1D(cColorMapA,color.r).rgb;
                }
            }
            LocalMaterialDiffuse.a = gl_Color.a;
            LocalMaterialAmbient = LocalMaterialDiffuse.xyz;
        }
        else
        {
            LocalMaterialDiffuse = gl_Color;
        }

        // First light is static light:
        vec4 color0 = vec4(0.0,0.0,0.0,0.0);
        vec4 color1 = vec4(0.0,0.0,0.0,0.0);
        vec4 color2 = vec4(0.0,0.0,0.0,0.0);
        vec4 color3 = vec4(0.0,0.0,0.0,0.0);
        color0 = computeColor( kInPos.xyz, kInNormal.xyz, CameraModelPosition.xyz,
                               MaterialEmissive.xyz,  LocalMaterialAmbient.xyz, LocalMaterialDiffuse.xyzw, MaterialSpecular.xyzw,
                               Light0Ambient.xyz, Light0Diffuse.xyz, Light0Specular.xyz,
                               Light0ModelPosition.xyz, Light0ModelDirection.xyz,
                               Light0SpotCutoff.xyzw, Light0Attenuation.xyzw,
                               Light0Type );
        
        // Assume second light is alwasy an ambient light:
        color1 = AmbientLight( MaterialEmissive.xyz,  LocalMaterialAmbient.xyz, 
                               Light1Ambient.xyz,
                               Light1Attenuation.xyzw );
        
        // Remaining lights:
        color2 = computeColor( kInPos.xyz, kInNormal.xyz, CameraModelPosition.xyz,
                               MaterialEmissive.xyz,  LocalMaterialAmbient.xyz, LocalMaterialDiffuse.xyzw, MaterialSpecular.xyzw,
                               Light2Ambient.xyz, Light2Diffuse.xyz, Light2Specular.xyz,
                               Light2WorldPosition.xyz, Light2WorldDirection.xyz,
                               Light2SpotCutoff.xyzw, Light2Attenuation.xyzw,
                               Light2Type );
        
        color3 = computeColor( kInPos.xyz, kInNormal.xyz, CameraModelPosition.xyz,
                               MaterialEmissive.xyz,  LocalMaterialAmbient.xyz, LocalMaterialDiffuse.xyzw, MaterialSpecular.xyzw,
                               Light3Ambient.xyz, Light3Diffuse.xyz, Light3Specular.xyz,
                               Light3WorldPosition.xyz, Light3WorldDirection.xyz,
                               Light3SpotCutoff.xyzw, Light3Attenuation.xyzw,
                               Light3Type );
        
        color = color0 + color1 + color2 + color3;
        color.a = Blend;  

        // Test normals:
//         color -= (color0 + color1 + color2 + color3);
//         color.r += (kInNormal.x + 1.0)/2.0;
//         color.g += (kInNormal.y + 1.0)/2.0;
//         color.b += (kInNormal.z + 1.0)/2.0;
//         color.a = 1.0;

        if ( color.a == 0.0 )
        {
            discard;
        }
    }
    gl_FragColor = color;
}
