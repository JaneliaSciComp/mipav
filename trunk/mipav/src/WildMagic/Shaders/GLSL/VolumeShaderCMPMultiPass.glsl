float computeX( 
               float         fY,
               vec4    LevLine )
{
    float x0 = LevLine.x;
    float y0 = LevLine.y;
    float x1 = LevLine.z;
    float y1 = LevLine.w;
    float t = (y0 - fY)/(y0 - y1);
    float x = x0 - x0 * t + x1 * t;
    return x;
}
//----------------------------------------------------------------------------
float computeAlpha( 
                   float         fX,
                   float         fY,
                   vec4    LevMidLine,
                   vec4    LevLeftLine,
                   vec4    LevRightLine )
{
    if ( (fY < LevLeftLine.y) || fY > LevLeftLine.w )
    {
        return 0.0;
    }
    float xMid = LevMidLine.x;
    float fShiftL = 0.0;
    float fShiftR = 0.0;
    float fShiftX = 0.0;
    float fIncr = 0.0;
    if ( LevMidLine.y != LevMidLine.w )
    {
        xMid = computeX( fY, LevMidLine );
    }
    else
    {
        fIncr = (LevMidLine.y - LevLeftLine.y)/(LevLeftLine.w-LevLeftLine.y);
        fIncr = fIncr * (LevRightLine.x - LevLeftLine.x);
        fShiftX = (LevMidLine.x - LevLeftLine.x)/(LevRightLine.x-LevLeftLine.x);
        fShiftL = (fShiftX)*fIncr;
        fShiftR = (1.0-fShiftX)*fIncr;
    }
    float xLeft = computeX( fY, LevLeftLine );
    float xRight = computeX( fY, LevRightLine );
    
    float fAlpha = 0.0;
    if ( (fX > (xMid - fShiftL)) && (fX < (xMid + fShiftR)) )
    {
        fAlpha = 1.0;
    }
    if ( (fX <= (xMid-fShiftL)) && (fX >= xLeft) )
    {
        fAlpha = (fX - xLeft) / ((xMid-fShiftL) - xLeft);
    }
    else if ( (fX >= (xMid+fShiftR)) && (fX <= xRight) )
    {
        fAlpha = (fX - xRight) / ((xMid+fShiftR) - xRight);
    }
    return fAlpha;
}
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
                        vec3   MaterialDiffuse,
                        vec4   MaterialSpecular,
                        vec3   LightDirection,
                        vec3   LightAmbient,
                        vec3   LightDiffuse,
                        vec3   LightSpecular,
                        vec4   LightAttenuation )
    
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


vec4 computeColor( vec3 kModelPosition, vec3 kModelNormal, vec3 CameraWorldPosition, 
                   vec3 MaterialEmissive, vec3 MaterialAmbient, vec4 MaterialDiffuse, vec4 MaterialSpecular,
                   vec4 LightAmbient, vec4 LightDiffuse, vec4 LightSpecular,
                   vec4 LightWorldPosition, vec4 LightWorldDirection,
                   vec4 LightSpotCutoff, vec4 LightAttenuation,
                   float LightType,
                   float Composite,
                   vec4 color)
{
    vec4 kResult = vec4(0.0,0.0,0.0,0.0);
    if ( LightType == -1.0 )
    {
//         kResult.r = kModelNormal.x;
//         kResult.g = kModelNormal.y;
//         kResult.b = kModelNormal.z;
        kResult.r = 0.0;
        kResult.g = 0.0;
        kResult.b = 0.0;
    }
    else
    {
        //vec3 local_normal = kModelNormal.xyz - (0.5, 0.5, 0.5);
        vec3 local_normal = ((2.0,2.0,2.0)* kModelNormal.xyz) - (1.0, 1.0, 1.0);
        local_normal = normalize( local_normal );

        vec3 LocalMaterialAmbient = MaterialAmbient;
        vec4 LocalMaterialDiffuse = MaterialDiffuse;
        vec3 LocalMaterialEmissive = MaterialEmissive;
        vec4 LocalMaterialSpecular = MaterialSpecular;

        if ( Composite != 0.0 )
        {
            LocalMaterialAmbient = color.xyz * MaterialAmbient;
            LocalMaterialDiffuse = color * MaterialDiffuse;
            LocalMaterialEmissive = color.xyz * MaterialEmissive;
            LocalMaterialSpecular = color * MaterialSpecular;
        }

        if ( LightType == 0.0 )
        {
            kResult = AmbientLight( LocalMaterialEmissive.xyz,
                                         LocalMaterialAmbient.xyz,
                                         LightAmbient.xyz,
                                         LightAttenuation.xyzw );
        }
        else if ( LightType == 1.0 )
        {
            kResult = DirectionalLight(  kModelPosition.xyz,
                                              local_normal.xyz,
                                              CameraWorldPosition.xyz,
                                              LocalMaterialEmissive.xyz,
                                              LocalMaterialAmbient.xyz,
                                              LocalMaterialDiffuse.xyz,
                                              LocalMaterialSpecular.xyzw,
                                              LightWorldDirection.xyz,
                                              LightAmbient.xyz,
                                              LightDiffuse.xyz,
                                              LightSpecular.xyz,
                                              LightAttenuation.xyzw);

        }
        else if ( LightType == 2.0 )
        {
            kResult = PointLight( kModelPosition.xyz,
                                       local_normal.xyz,
                                       CameraWorldPosition,
                                       LocalMaterialEmissive.xyz,
                                       LocalMaterialAmbient.xyz,
                                       LocalMaterialDiffuse.xyzw,
                                       LocalMaterialSpecular.xyzw,
                                       LightWorldPosition.xyz,
                                       LightAmbient.xyz,
                                       LightDiffuse.xyz,
                                       LightSpecular.xyz,
                                       LightAttenuation.xyzw);
        }
        else
        {
            kResult = SpotLight( kModelPosition.xyz,
                                      local_normal.xyz,
                                      CameraWorldPosition.xyz,
                                      LocalMaterialEmissive.xyz,
                                      LocalMaterialAmbient.xyz,
                                      LocalMaterialDiffuse.xyzw,
                                      LocalMaterialSpecular.xyzw,
                                      LightWorldPosition.xyz,
                                      LightWorldDirection.xyz,
                                      LightAmbient.xyz,
                                      LightDiffuse.xyz,
                                      LightSpecular.xyz,
                                      LightSpotCutoff.xyzw,
                                      LightAttenuation.xyzw);
        }
    }
    return kResult;
}

/**
 * Clip the volume based on the x,y,z axes.
 * returns 1 when the volume is clipped, 0 when not clipped.
 */
bool myClip(const vec3 myvec,
            float clipX,
            float clipXInv,
            float clipY,
            float clipYInv,
            float clipZ,
            float clipZInv )
{
    if ( myvec.x > clipX )
    {
        return true;
    }
    if ( myvec.x < clipXInv )
    {
        return true;
    }
    if ( myvec.y > clipY )
    {
        return true;
    }
    if ( myvec.y < clipYInv )
    {
        return true;
    }
    if ( myvec.z > clipZ )
    {
        return true;
    }
    if ( myvec.z < clipZInv )
    {
        return true;
    } else {
        return false;
    }
}

varying vec4 outPos;
uniform mat4 WVPMatrix;
uniform sampler2D aSceneImage_TEXUNIT0; 
uniform sampler3D bVolumeImageA_TEXUNIT1; 
uniform sampler1D cColorMapA_TEXUNIT2; 
uniform sampler1D dOpacityMapA_TEXUNIT3; 
uniform sampler3D eNormalMapA_TEXUNIT4; 
uniform sampler3D fVolumeImageA_GM_TEXUNIT5; 
uniform sampler1D gOpacityMapA_GM_TEXUNIT6; 
uniform sampler3D hVolumeImageA_2nd_TEXUNIT7; 
uniform float IsColorA;
uniform float DoClip;
uniform float GradientMagnitude;
uniform float clipX;
uniform float clipXInv;
uniform float clipY;
uniform float clipYInv;
uniform float clipZ;
uniform float clipZInv;
uniform float iPass;
uniform float iPassQuantity;
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
uniform vec3 CameraWorldPosition;

uniform float Blend;

uniform float MIP;
uniform float DRR;
uniform float Composite;
uniform float Surface;

uniform float MULTIHISTO;
uniform float   UseWidget0;
uniform vec4    LevColor0;
uniform vec4    LevMidLine0;
uniform vec4    LevLeftLine0;
uniform vec4    LevRightLine0;

uniform float    UseWidget1;
uniform vec4    LevColor1;
uniform vec4    LevMidLine1;
uniform vec4    LevLeftLine1;
uniform vec4    LevRightLine1;

uniform float    UseWidget2;
uniform vec4    LevColor2;
uniform vec4    LevMidLine2;
uniform vec4    LevLeftLine2;
uniform vec4    LevRightLine2;

uniform float    UseWidget3;
uniform vec4    LevColor3;
uniform vec4    LevMidLine3;
uniform vec4    LevLeftLine3;
uniform vec4    LevRightLine3;

uniform float    UseWidget4;
uniform vec4    LevColor4;
uniform vec4    LevMidLine4;
uniform vec4    LevLeftLine4;
uniform vec4    LevRightLine4;

uniform float    UseWidget5;
uniform vec4    LevColor5;
uniform vec4    LevMidLine5;
uniform vec4    LevLeftLine5;
uniform vec4    LevRightLine5;


/** Raycasting fragment program implementation */
void p_VolumeShaderCMPMultiPass()
{
    // find the right place to lookup in the backside buffer
    vec2 texc = ((outPos.xy / outPos.w) + 1.0) / 2.0;
    vec4 back_position  = texture2D(aSceneImage_TEXUNIT0, texc);

    // the start position of the ray is stored in the texturecoordinate
    vec3 start = gl_TexCoord[0].xyz; 

    // the ray direction
    vec3 dir = back_position.xyz - start;

    // The color at the current position along the ray:
    vec4 color = vec4(0.0);

    // The opacity at the current position along the ray:
    float opacity = 1.0;

    // Gradient magnitude values along the ray:
    vec4 colorGM = vec4(0.0);
    float opacityGM = 0.0;

    bool bClipped = false;

    // current position along the ray: 
    float fPos = iPass;
    vec3 position = vec3(0.0,0.0,0.0);
    position.x = start.x + fPos * dir.x;
    position.y = start.y + fPos * dir.y;
    position.z = start.z + fPos * dir.z;


    vec3 LocalMaterialAmbient = MaterialAmbient;
    vec3 LocalMaterialEmissive = MaterialEmissive;

    float fMapX, fMapY;

    // axis-aligned clipping:
    if ( (DoClip != 0.0) && myClip( position, clipX, clipXInv, clipY, clipYInv, clipZ, clipZInv ) )
    {
        color = vec4(0.0);
        opacity = 0.0;
    }
    else
    {
        bClipped = false;
        if ( DoClip != 0.0 )
        {
            // eye clipping and arbitrary clipping:
            vec4 aPosition = vec4(0.0);
            aPosition.xyz = position.xyz - (.5,.5,.5);
            aPosition = WVPMatrix*aPosition;
            aPosition.xyz = aPosition.xyz + (.5,.5,.5);
            float fDot = dot( aPosition.xyz, clipEye.xyz );
            float fDotInv = dot( aPosition.xyz, clipEyeInv.xyz );
            float fDotArb = dot( position.xyz, clipArb.xyz );
            if ( (fDot < clipEye.w) || (fDotInv > clipEyeInv.w) || (fDotArb > clipArb.w) )
            {
                color = vec4(0.0);
                opacity = 0.0;
                bClipped = true;
            }
        }
        // The value is not clipped, compute the color:
        if ( !bClipped )
        {
            color = texture3D(bVolumeImageA_TEXUNIT1,position);

            if ( MULTIHISTO != 0.0 )
            {
                colorGM = texture3D(fVolumeImageA_GM_TEXUNIT5,position);

                fMapX = color.r;
                fMapY = colorGM.r;

                if ( UseWidget0 != 0.0 )
                {
                    float opacity0 =
                        computeAlpha( fMapX, fMapY, LevMidLine0, LevLeftLine0, LevRightLine0 );
                    vec4 color0 = LevColor0 * opacity0;
                    color = color0;
                    opacity = opacity0 * LevColor0.a;
                }
                if ( UseWidget1 != 0.0 )
                {
                    float opacity1 =
                        computeAlpha( fMapX, fMapY, LevMidLine1, LevLeftLine1, LevRightLine1 );
                    vec4 color1 = LevColor1 * opacity1;
                    color += color1;
                    opacity += (opacity1 * LevColor1.a);
                }
                if ( UseWidget2 != 0.0 )
                {
                    float opacity2 =
                        computeAlpha( fMapX, fMapY, LevMidLine2, LevLeftLine2, LevRightLine2 );
                    vec4 color2 = LevColor2 * opacity2;
                    color += color2;
                    opacity += (opacity2 * LevColor2.a);
                }
                if ( UseWidget3 != 0.0 )
                {
                    float opacity3 =
                        computeAlpha( fMapX, fMapY, LevMidLine3, LevLeftLine3, LevRightLine3 );
                    vec4 color3 = LevColor3 * opacity3;
                    color += color3;
                    opacity += (opacity3 * LevColor3.a);
                }
                if ( UseWidget4 != 0.0 )
                {
                    float opacity4 =
                        computeAlpha( fMapX, fMapY, LevMidLine4, LevLeftLine4, LevRightLine4 );
                    vec4 color4 = LevColor4 * opacity4;
                    color += color4;
                    opacity += (opacity4 * LevColor4.a);
                }
                if ( UseWidget5 != 0.0 )
                {
                    float opacity5 =
                        computeAlpha( fMapX, fMapY, LevMidLine5, LevLeftLine5, LevRightLine5 );
                    vec4 color5 = LevColor5 * opacity5;
                    color += color5;
                    opacity += (opacity5 * LevColor5.a);
                }
            }
            else
            {
                opacity = color.r;
                opacity = texture1D(dOpacityMapA_TEXUNIT3,opacity).r;
            
                if ( IsColorA != 0.0 )
                {
                    color.r = texture1D(cColorMapA_TEXUNIT2,color.r).r;
                    color.g = texture1D(cColorMapA_TEXUNIT2,color.g).g;
                    color.b = texture1D(cColorMapA_TEXUNIT2,color.b).b;
                }
                else
                {
                    color = texture1D(cColorMapA_TEXUNIT2,color.r);
                }

                if ( GradientMagnitude != 0.0 )
                {
                    colorGM = texture3D(fVolumeImageA_GM_TEXUNIT5,position);
                    opacityGM = texture1D(gOpacityMapA_GM_TEXUNIT6,colorGM.r).r;
                    opacity = opacity * opacityGM;
                }
            }

            if ( Surface != 0.0 )
            {
                // Surface and Composite surface display:
                vec4 normal = texture3D(eNormalMapA_TEXUNIT4,position);
                normal.w = 0.0;
            
                // First light is static light:
                vec4 color0 = color;
                color0 = computeColor( position.xyz, normal.xyz, CameraModelPosition.xyz,
                                       MaterialEmissive.xyz,  MaterialAmbient.xyz, MaterialDiffuse.xyzw, MaterialSpecular.xyzw,
                                       Light0Ambient.xyzw, Light0Diffuse.xyzw, Light0Specular.xyzw,
                                       Light0ModelPosition.xyzw, Light0ModelDirection.xyzw,
                                       Light0SpotCutoff.xyzw, Light0Attenuation.xyzw,
                                       Light0Type,
                                       Composite, color0.xyzw );
            
                // Assume second light is alwasy an ambient light:
                vec4 color1 = color;
                if ( Composite != 0.0 )
                {
                    LocalMaterialAmbient = color1.xyz * MaterialAmbient.xyz;
                    LocalMaterialEmissive = color1.xyz * MaterialEmissive.xyz;
                }
                color1 = AmbientLight( LocalMaterialEmissive.xyz,
                                       LocalMaterialAmbient.xyz,
                                       Light1Ambient.xyz,
                                       Light1Attenuation.xyzw );
            
                // Remaining lights:
                vec4 color2 = color;
                color2 = computeColor( position.xyz, normal.xyz, CameraModelPosition.xyz,
                                       MaterialEmissive.xyz,  MaterialAmbient.xyz, MaterialDiffuse.xyzw, MaterialSpecular.xyzw,
                                       Light2Ambient.xyzw, Light2Diffuse.xyzw, Light2Specular.xyzw,
                                       Light2WorldPosition.xyzw, Light2WorldDirection.xyzw,
                                       Light2SpotCutoff.xyzw, Light2Attenuation.xyzw,
                                       Light2Type,
                                       Composite, color2.xyzw );
            
                vec4 color3 = color;
                color3 = computeColor( position.xyz, normal.xyz, CameraModelPosition.xyz,
                                       MaterialEmissive.xyz,  MaterialAmbient.xyz, MaterialDiffuse.xyzw, MaterialSpecular.xyzw,
                                       Light3Ambient.xyzw, Light3Diffuse.xyzw, Light3Specular.xyzw,
                                       Light3WorldPosition.xyzw, Light3WorldDirection.xyzw,
                                       Light3SpotCutoff.xyzw, Light3Attenuation.xyzw,
                                       Light3Type,
                                       Composite, color3.xyzw );
            
                color = color0 + color1 + color2 + color3;
            }
        }
    }

    if ( DRR != 0.0 )
    {
        color.rgb *= iPassQuantity;
        opacity /= iPassQuantity;
    }
    if ( MIP != 0.0 )
    {
        color.rgb *= opacity;
    }
    opacity *= Blend;
    gl_FragColor.rgb = color.rgb;
    gl_FragColor.a = opacity;
}
