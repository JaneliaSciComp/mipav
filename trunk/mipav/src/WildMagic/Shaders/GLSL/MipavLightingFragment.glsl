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
    float fDiff = 0;
    float fSpec = 0;
    
    float cos_theta = 0;
    float cos_alpha = 0;
    
    vec3 V = normalize(kCameraPosition - kModelPosition);
    vec3 N = normalize(kModelNormal);
    
    vec3 L = vec3(kLightDirection.x, kLightDirection.y, kLightDirection.z);
    L = -L;
    L = normalize(L);
    
    vec3 R = vec3(N.x, N.y, N.z);
    R = R * ( 2 * dot(L, N));
    R = R - L;
    R = normalize(R);
    
    cos_theta = dot(L, N);
    cos_alpha = dot(R, V);
    
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
            fSpot = pow(fVertexCosAngle,fSpotExponent);
        }
        else
        {
            fDiff = 0.0f;
            fSpec = 0.0f;
            fSpot = 0.0f;
        }
    }
    else
    {
        fDiff = 0.0f;
        fSpec = 0.0f;
        fSpot = 0.0f;
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

void AmbientLight( uniform vec3   MaterialEmissive,
                   uniform vec3   MaterialAmbient,
                   uniform vec3   LightAmbient,
                   uniform vec4   LightAttenuation,
                   out     vec4 kVertexColor)

{
    vec3 kLAmb = LightAttenuation.w*LightAmbient;
    kVertexColor.rgb = MaterialEmissive + MaterialAmbient*kLAmb;
    kVertexColor.a = 1.0;
}

void DirectionalLight( uniform vec3 kModelPosition,
                       uniform vec3 kModelNormal,
                       uniform vec3   CameraWorldPosition,
                       uniform vec3   MaterialEmissive,
                       uniform vec3   MaterialAmbient,
                       uniform vec4   MaterialDiffuse,
                       uniform vec4   MaterialSpecular,
                       uniform vec3   LightDirection,
                       uniform vec3   LightAmbient,
                       uniform vec3   LightDiffuse,
                       uniform vec3   LightSpecular,
                       uniform vec4   LightAttenuation,
                       out     vec4 kVertexColor)
    
{
    float fDiff, fSpec;
    GetDirectionalLightFactors(kModelPosition,kModelNormal,
                               CameraWorldPosition,LightDirection,MaterialSpecular.a,
                               fDiff,fSpec);
    vec3 kColor = MaterialAmbient*LightAmbient;
    if (fDiff > 0.0f)
    {
        kColor += fDiff*MaterialDiffuse.rgb*LightDiffuse;
        if (fSpec > 0.0f)
        {
            kColor += fSpec*MaterialSpecular.rgb*LightSpecular;
        }
    }

    kVertexColor.rgb = MaterialEmissive + LightAttenuation.w*kColor;
    kVertexColor.a = 1.0;
}

void PointLight(    uniform vec3 kModelPosition,
                    uniform vec3 kModelNormal,
                    uniform vec3   CameraWorldPosition,
                    uniform vec3   MaterialEmissive,
                    uniform vec3   MaterialAmbient,
                    uniform vec4   MaterialDiffuse,
                    uniform vec4   MaterialSpecular,
                    uniform vec3   LightWorldPosition,
                    uniform vec3   LightAmbient,
                    uniform vec3   LightDiffuse,
                    uniform vec3   LightSpecular,
                    uniform vec4   LightAttenuation,
                    out     vec4 kVertexColor)
{
    float fDiff, fSpec;
    GetPointLightFactors(kModelPosition.xyz,kModelNormal,
                         CameraWorldPosition,LightWorldPosition,MaterialSpecular.a,
                         fDiff,fSpec);

    float fAttn = GetAttenuation(kModelPosition.xyz,
                                 LightWorldPosition,LightAttenuation);

    vec3 kColor = MaterialAmbient*LightAmbient;
    if (fDiff > 0.0f)
    {
        kColor += fDiff*MaterialDiffuse.xyz*LightDiffuse;
        if (fSpec > 0.0f)
        {
            kColor += fSpec*MaterialSpecular.xyz*LightSpecular;
        }
    }

    kVertexColor.rgb = MaterialEmissive + fAttn*kColor;
    kVertexColor.a = MaterialDiffuse.a;
}

void SpotLight(    uniform vec3 kModelPosition,
                   uniform vec3 kModelNormal,
                   uniform vec3 CameraWorldPosition,
                   uniform vec3 MaterialEmissive,
                   uniform vec3 MaterialAmbient,
                   uniform vec4 MaterialDiffuse,
                   uniform vec4 MaterialSpecular,
                   uniform vec3 LightWorldPosition,
                   uniform vec3 LightWorldDirection,
                   uniform vec3 LightAmbient,
                   uniform vec3 LightDiffuse,
                   uniform vec3 LightSpecular,
                   uniform vec4 LightSpotCutoff,
                   uniform vec4 LightAttenuation,
                   out     vec4 kVertexColor)
{
    float fDiff, fSpec, fSpot;
    GetSpotLightFactors(kModelPosition.xyz,kModelNormal,
                        CameraWorldPosition,LightWorldPosition,MaterialSpecular.a,
                        LightWorldDirection,LightSpotCutoff.y,LightSpotCutoff.w,fDiff,
                        fSpec,fSpot);

    float fAttn = GetAttenuation(kModelPosition.xyz,
                                 LightWorldPosition,LightAttenuation);

    vec3 kColor = MaterialAmbient*LightAmbient;
    if (fSpot > 0.0f)
    {
        if (fDiff > 0.0f)
        {
            kColor += (fSpot*fDiff)*MaterialDiffuse.rgb*LightDiffuse;
            if (fSpec > 0.0f)
            {
                kColor += (fSpot*fSpec)*MaterialSpecular.rgb*LightSpecular;
            }
        }
    }
    
    kVertexColor.rgb = MaterialEmissive + fAttn*kColor;
    kVertexColor.a = MaterialDiffuse.a;
}


/**
 * Compute the surface color based on lighting.
 */
void computeColor( vec3 kModelPosition, vec3 kModelNormal, vec3 CameraWorldPosition, 
                   vec3 MaterialEmissive, vec3 MaterialAmbient, vec4 MaterialDiffuse, vec4 MaterialSpecular,
                   vec4 LightAmbient, vec4 LightDiffuse, vec4 LightSpecular,
                   vec4 LightWorldPosition, vec4 LightWorldDirection,
                   vec4 LightSpotCutoff, vec4 LightAttenuation,
                   float LightType,
                   out vec4 color_sample)
{
    if ( LightType == -1 )
    {
        color_sample.r = 0;
        color_sample.g = 0;
        color_sample.b = 0;
    }
    else
    {
        if ( LightType == 0 )
        {
            AmbientLight( MaterialEmissive,
                          MaterialAmbient,
                          LightAmbient,
                          LightAttenuation,
                          color_sample );
        }
        else if ( LightType == 1 )
        {
            DirectionalLight(  kModelPosition,
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
                               LightAttenuation,
                               color_sample);

        }
        else if ( LightType == 2 )
        {
            PointLight( kModelPosition,
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
                        LightAttenuation,
                        color_sample);
        }
        else
        {
            SpotLight( kModelPosition,
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
                       LightAttenuation,
                       color_sample);
        }
    }
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

void v_MipavLightingFragment(
                     in vec4        kModelPosition  : POSITION,
                     in vec4        kModelNormal : NORMAL,
                     in vec4        kModelColor0 : COLOR0,
                     in vec3        kInBaseTCoord : TEXCOORD0,
                     out vec4       kClipPosition : POSITION,
                     out vec3       kInPos,
                     out vec3       kInVolumeClipPos,
                     out vec3       kInNormal,
                     out vec4       kInColor0,
                     out vec3       kOutBaseTCoord : TEXCOORD0,
                     uniform float ReverseFace,
                     uniform vec4x4 WVPMatrix)
{
    // Transform the position from model space to clip space.
    kClipPosition = mul(kModelPosition,WVPMatrix);
    kInPos = kModelPosition;
    kInNormal = kModelNormal;
    if ( ReverseFace == 1 )
    {
        kInNormal.x *= -1;
        kInNormal.y *= -1;
        kInNormal.z *= -1;
    }
    
    kInVolumeClipPos = kInBaseTCoord;
    kInColor0 = kModelColor0;

    // Pass through the texture coordinate.
    kOutBaseTCoord = kInBaseTCoord;
}

void p_MipavLightingFragment(
                     out vec4       kVertexColor : COLOR,
                     in vec3         kBaseTCoord : TEXCOORD0,
                     in vec3        kInPos,
                     in vec3        kInVolumeClipPos,
                     in vec3        kInNormal,
                     in vec4       kInColor0,
                     uniform sampler3D VolumeImageA : TEXUNIT1, 
                     uniform sampler1D ColorMapA : TEXUNIT2,
                     uniform sampler3D VolumeImageNew : TEXUNIT16, 
                     uniform sampler1D ColorMapNew : TEXUNIT17,
                     uniform float UseTexture,
                     uniform float UseImageNew,
                     uniform float UseLUTNew,
                     uniform float Blend,
                     uniform float ClipEnabled,
                     uniform float DoClip,
                     uniform float clipX,
                     uniform float clipXInv,
                     uniform float clipY,
                     uniform float clipYInv,
                     uniform float clipZ,
                     uniform float clipZInv,
                     uniform vec4 clipArb,
                     uniform vec4 clipEye,
                     uniform vec4 clipEyeInv,

                     uniform vec3 MaterialEmissive,
                     uniform vec3 MaterialAmbient,
                     uniform vec4 MaterialDiffuse,
                     uniform vec4 MaterialSpecular,
                     
                     uniform float  Light0Type,
                     uniform vec4 Light0Ambient,
                     uniform vec4 Light0Diffuse,
                     uniform vec4 Light0Specular,
                     uniform vec4 Light0SpotCutoff,
                     uniform vec4 Light0Attenuation,
                     uniform vec4 Light0ModelPosition,
                     uniform vec4 Light0ModelDirection,
                     
                     uniform vec4 Light1Ambient,
                     uniform vec4 Light1Attenuation,
                     
                     uniform float  Light2Type,
                     uniform vec4 Light2Ambient,
                     uniform vec4 Light2Diffuse,
                     uniform vec4 Light2Specular,
                     uniform vec4 Light2SpotCutoff,
                     uniform vec4 Light2Attenuation,
                     uniform vec4 Light2WorldPosition,
                     uniform vec4 Light2WorldDirection,
                     
                     uniform float  Light3Type,
                     uniform vec4 Light3Ambient,
                     uniform vec4 Light3Diffuse,
                     uniform vec4 Light3Specular,
                     uniform vec4 Light3SpotCutoff,
                     uniform vec4 Light3Attenuation,
                     uniform vec4 Light3WorldPosition,
                     uniform vec4 Light3WorldDirection,
                     
                     uniform vec3 CameraModelPosition,
                     uniform vec3 CameraWorldPosition,
                     uniform vec4x4 WMatrix,
                     uniform vec4x4 WVPMatrix)

{
    bool bClipped = false;

    if ( !bClipped && (ClipEnabled == 1.0) )
    {
        // axis-aligned clipping:
        if ( (DoClip != 0) && myClip( kInVolumeClipPos, clipX, clipXInv, clipY, clipYInv, clipZ, clipZInv ) )
        {
            bClipped = true;
        }
        else
        {
            bClipped = false;
            if ( DoClip != 0 )
            {
                // eye clipping and arbitrary clipping:
                vec4 aPosition = vec4(0,0,0,0);
                aPosition.xyz = kInVolumeClipPos.xyz - (.5,.5,.5);
                aPosition = mul( aPosition, WVPMatrix );
                aPosition.xyz = aPosition.xyz + (.5,.5,.5);
                float fDot = dot( aPosition.xyz, clipEye.xyz );
                float fDotInv = dot( aPosition.xyz, clipEyeInv.xyz );
                float fDotArb = dot( kInVolumeClipPos.xyz, clipArb.xyz );
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

    if ( bClipped == false )
    {
        if ( UseTexture == 0 )
        {
            MaterialDiffuse = kInColor0;
        }
        else
        {
            vec4 color;
            if ( UseImageNew == 0 )
            {
                color = tex3D(VolumeImageA,kBaseTCoord);
            }
            else
            {
                color = tex3D(VolumeImageNew,kBaseTCoord);
            }
            if ( UseLUTNew == 0 )
            {
                MaterialDiffuse.rgb = tex1D(ColorMapA,color.r).rgb;
            }
            else
            {
                MaterialDiffuse.rgb = tex1D(ColorMapNew,color.r).rgb;
            }
            MaterialDiffuse.a = kInColor0.a;
        }

        // First light is static light:
        vec4 color0 = (0,0,0,0);
        vec4 color1 = (0,0,0,0);
        vec4 color2 = (0,0,0,0);
        vec4 color3 = (0,0,0,0);
        computeColor( kInPos, kInNormal, CameraModelPosition,
                      MaterialEmissive,  MaterialAmbient, MaterialDiffuse, MaterialSpecular,
                      Light0Ambient, Light0Diffuse, Light0Specular,
                      Light0ModelPosition, Light0ModelDirection,
                      Light0SpotCutoff, Light0Attenuation,
                      Light0Type, color0 );
        
        // Assume second light is alwasy an ambient light:
        AmbientLight( MaterialEmissive,  MaterialAmbient, 
                      Light1Ambient,
                      Light1Attenuation,
                      color1 );
        
        // Remaining lights:
        computeColor( kInPos, kInNormal, CameraModelPosition,
                      MaterialEmissive,  MaterialAmbient, MaterialDiffuse, MaterialSpecular,
                      Light2Ambient, Light2Diffuse, Light2Specular,
                      Light2WorldPosition, Light2WorldDirection,
                      Light2SpotCutoff, Light2Attenuation,
                      Light2Type, color2 );
        
        computeColor( kInPos, kInNormal, CameraModelPosition,
                      MaterialEmissive,  MaterialAmbient, MaterialDiffuse, MaterialSpecular,
                      Light3Ambient, Light3Diffuse, Light3Specular,
                      Light3WorldPosition, Light3WorldDirection,
                      Light3SpotCutoff, Light3Attenuation,
                      Light3Type, color3 );
        
        kVertexColor = color0 + color1 + color2 + color3;
        kVertexColor.a = Blend * kInColor0.a;

        // Test normals:
        //        kVertexColor -= (color0 + color1 + color2 + color3);
        //        kVertexColor.r += (kInNormal.x + 1.0)/2.0;
        //        kVertexColor.g += (kInNormal.y + 1.0)/2.0;
        //        kVertexColor.b += (kInNormal.z + 1.0)/2.0;

        if ( kVertexColor.a == 0 )
        {
            discard;
        }
    }
}
