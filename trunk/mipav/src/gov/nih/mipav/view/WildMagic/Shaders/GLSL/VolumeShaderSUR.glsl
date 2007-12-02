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

void AmbientLight(  vec3   MaterialEmissive,
                    vec3   MaterialAmbient,
                    vec3   LightAmbient,
                    vec4   LightAttenuation,
                   out     vec4 kVertexColor)

{
    vec3 kLAmb = LightAttenuation.w*LightAmbient;
    kVertexColor.rgb = MaterialEmissive + MaterialAmbient*kLAmb;
    kVertexColor.a = 1.0;
}

void DirectionalLight(  vec3 kModelPosition,
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
                        vec4   LightAttenuation,
                       out     vec4 kVertexColor)
    
{
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
}

void PointLight(     vec3 kModelPosition,
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
                     vec4   LightAttenuation,
                    out     vec4 kVertexColor)
{
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

    kVertexColor.rgb = MaterialEmissive + fAttn*kColor;
    kVertexColor.a = MaterialDiffuse.a;
}

void SpotLight(     vec3 kModelPosition,
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
                    vec4 LightAttenuation,
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
}


void computeColor( vec3 kModelPosition, vec3 kModelNormal, vec3 CameraWorldPosition, 
                   vec3 MaterialEmissive, vec3 MaterialAmbient, vec4 MaterialDiffuse, vec4 MaterialSpecular,
                   vec4 LightAmbient, vec4 LightDiffuse, vec4 LightSpecular,
                   vec4 LightWorldPosition, vec4 LightWorldDirection,
                   vec4 LightSpotCutoff, vec4 LightAttenuation,
                   float LightType,
                   float Composite,
                   inout vec4 color_sample)
{
    if ( LightType == -1.0 )
    {
        color_sample.r = 0.0;
        color_sample.g = 0.0;
        color_sample.b = 0.0;
    }
    else
    {
        vec3 local_normal = kModelNormal.xyz - (0.5, 0.5, 0.5);
        local_normal = normalize( local_normal );

        vec3 LocalMaterialAmbient = MaterialAmbient;
        vec4 LocalMaterialDiffuse = MaterialDiffuse;
        vec3 LocalMaterialEmissive = MaterialEmissive;
        vec4 LocalMaterialSpecular = MaterialSpecular;

        if ( Composite != 0.0 )
        {
            LocalMaterialAmbient = color_sample.xyz * MaterialAmbient;
            LocalMaterialDiffuse = color_sample * MaterialDiffuse;
            LocalMaterialEmissive = color_sample.xyz * MaterialEmissive;
            LocalMaterialSpecular = color_sample * MaterialSpecular;
        }

        if ( LightType == 0.0 )
        {
            AmbientLight( LocalMaterialEmissive,
                          LocalMaterialAmbient,
                          LightAmbient,
                          LightAttenuation,
                          color_sample );
        }
        else if ( LightType == 1.0 )
        {
            DirectionalLight(  kModelPosition,
                               local_normal,
                               CameraWorldPosition,
                               LocalMaterialEmissive,
                               LocalMaterialAmbient,
                               LocalMaterialDiffuse,
                               LocalMaterialSpecular,
                               LightWorldDirection,
                               LightAmbient,
                               LightDiffuse,
                               LightSpecular,
                               LightAttenuation,
                               color_sample);

        }
        else if ( LightType == 2.0 )
        {
            PointLight( kModelPosition,
                        local_normal,
                        CameraWorldPosition,
                        LocalMaterialEmissive,
                        LocalMaterialAmbient,
                        LocalMaterialDiffuse,
                        LocalMaterialSpecular,
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
                       local_normal,
                       CameraWorldPosition,
                       LocalMaterialEmissive,
                       LocalMaterialAmbient,
                       LocalMaterialDiffuse,
                       LocalMaterialSpecular,
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
uniform mat4 WMatrix;
uniform sampler2D aSceneImage; 
uniform sampler3D bVolumeImageA; 
uniform sampler1D cColorMapA; 
uniform sampler1D dOpacityMapA; 
uniform sampler3D eNormalMapA; 
uniform sampler3D fVolumeImageA_GM; 
uniform sampler1D gOpacityMapA_GM; 
uniform float stepsize;
uniform vec4  steps;
uniform float IsColor;
uniform float DoClip;
uniform float SelfShadow;
uniform float GradientMagnitude;
uniform float Composite;
uniform vec4 BackgroundColor;
uniform vec3 Translation;
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
uniform vec3 CameraWorldPosition;


/** Raycasting fragment program implementation */
void p_VolumeShaderSUR()
{
    // find the right place to lookup in the backside buffer
    vec2 texc = ((outPos.xy / outPos.w) + 1.0) / 2.0;
    vec4 back_position  = texture2D(aSceneImage, texc);

    // the start position of the ray is stored in the texturecoordinate
    vec3 start = gl_TexCoord[0].xyz; 

    // the ray direction
    vec3 dir = back_position.xyz - start;

    // the length from front to back is calculated and used to terminate the ray
    float len = length(dir.xyz); 

    // normalized direction vector:
    vec3 norm_dir = normalize(dir);

    // current position along the ray: 
    vec3 position = start.xyz;

    // The accumulated color and alpha values:
    vec4 color_acc = vec4(0.0);
    float alpha_acc = 0.0;

    // output color:
    gl_FragColor = BackgroundColor;
    if ( (back_position.x == 0.0) && (back_position.y == 0.0) && (back_position.z == 0.0) )
    {
        return;
    }

    // limit the number of iterations to STEPS, make sure that the stepsize will
    // cover the entire ray:
    float delta = stepsize;
    if ( (len/stepsize) > steps[0] )
    {
        delta = len/steps[0];
    }

    // The color at the current position along the ray:
    vec4 color = vec4(0.0);

    // The opacity at the current position along the ray:
    float opacity = 0.0;
    // Gradient magnitude values along the ray:
    vec4 colorGM = vec4(0.0);
    float opacityGM = 0.0;

    // The normal at the current position along the ray:
    vec4 normal = vec4(0.0);

    float alpha_sample = delta;
    vec3 delta_dir = norm_dir * delta;
    float delta_dir_len = length(delta_dir);
    float length_acc = 0.0;
    bool bClipped = false;

    vec4 worldNormal;
    vec4 color0 = vec4(0.0);
    vec4 color1 = vec4(0.0);
    vec4 color2 = vec4(0.0);
    vec4 color3 = vec4(0.0);


    vec3 LocalMaterialAmbient = MaterialAmbient;
    vec3 LocalMaterialEmissive = MaterialEmissive;

    // For some profiles the number of loop iterations must be determined at
    // compile time:
    for( int i = 0; i < 450; i++ )
    {
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
                color = texture3D(bVolumeImageA,position);
                opacity = texture1D(dOpacityMapA,color.r).r;
                if ( GradientMagnitude != 0.0 )
                {
                    colorGM = texture3D(fVolumeImageA_GM,position);
                    opacityGM = texture1D(gOpacityMapA_GM,colorGM.r).r;
                    opacity = opacity * opacityGM;
                }

                if ( IsColor != 0.0 )
                {
                    color.r = texture1D(cColorMapA,color.r).r;
                    color.g = texture1D(cColorMapA,color.g).g;
                    color.b = texture1D(cColorMapA,color.b).b;
                }
                else
                {
                    color = texture1D(cColorMapA,color.r);
                }
            }
        }
        // If the opacity is not zero:
        if ( opacity > 0.0 )
        {
            // Surface and Composite surface display:
            normal = texture3D(eNormalMapA,position);
            normal.w = 0.0;

            // First light is static light:
            color0 = color;
            computeColor( position, normal, CameraModelPosition,
                          MaterialEmissive,  MaterialAmbient, MaterialDiffuse, MaterialSpecular,
                          Light0Ambient, Light0Diffuse, Light0Specular,
                          Light0ModelPosition, Light0ModelDirection,
                          Light0SpotCutoff, Light0Attenuation,
                          Light0Type,
                          Composite, color0 );

            // Assume second light is alwasy an ambient light:
            color1 = color;
            if ( Composite != 0.0 )
            {
                LocalMaterialAmbient = color1.xyz * MaterialAmbient.xyz;
                LocalMaterialEmissive = color1.xyz * MaterialEmissive.xyz;
            }
            AmbientLight( LocalMaterialEmissive,
                          LocalMaterialAmbient,
                          Light1Ambient,
                          Light1Attenuation,
                          color1 );

            // Remaining lights:
            color2 = color;
            computeColor( position, normal, CameraModelPosition,
                          MaterialEmissive,  MaterialAmbient, MaterialDiffuse, MaterialSpecular,
                          Light2Ambient, Light2Diffuse, Light2Specular,
                          Light2WorldPosition, Light2WorldDirection,
                          Light2SpotCutoff, Light2Attenuation,
                          Light2Type,
                          Composite, color2 );

            color3 = color;
            computeColor( position, normal, CameraModelPosition,
                          MaterialEmissive,  MaterialAmbient, MaterialDiffuse, MaterialSpecular,
                          Light3Ambient, Light3Diffuse, Light3Specular,
                          Light3WorldPosition, Light3WorldDirection,
                          Light3SpotCutoff, Light3Attenuation,
                          Light3Type,
                          Composite, color3 );

            color = color0 + color1 + color2 + color3;

            // Composite surface:
            if ( Composite != 0.0 )
            {
                color_acc  = (1.0 - alpha_acc) * color * opacity + color_acc;
                alpha_acc += (1.0 - alpha_acc) * opacity;
            }
            // Surface:
            else if ( (color.x != 0.0) || (color.y != 0.0) || (color.z != 0.0) )
            {
                color_acc = color;
                alpha_acc = 1.0;
                break;
            }
        }
        // Break early if the accumulate alpha value for composite or composite surface is >= 1
        if ( (Composite != 0.0) && (alpha_acc >= 1.0) )
        {
            break;
        }
        // Increment position along the ray:
        position += delta_dir;
        length_acc += delta_dir_len;
        // Break when the end of the ray is reached, or if alpha >= 1;
        if ( (length_acc >= len) || (alpha_acc >= 1.0) )
        {
            break;
        }
    } 


    // Self-shadowing is possible for Surface mode:
    if ( (SelfShadow != 0.0) && (Composite == 0.0) && (alpha_acc == 1.0) ) {
        // Use the normal at the start position:
        vec3 local_normal = normal.xyz - (0.5, 0.5, 0.5);
        local_normal = normalize( local_normal );

        // normal at occluding position:
        vec4 normal2;
        vec3 local_normal2;

        // for each light, determine whether it is blocked by an opaque surface.
        // start at position + incr, step to light position

        // save start position, reuse for the next light
        start = position;

        // for each light that can cause shadows:
        // Light0:
        vec3 dir1 = Light0ModelPosition.xyz - start;
        vec3 norm_dir1 = normalize(dir1);
        vec3 delta_dir1 = norm_dir1 * delta;
        vec3 position1 = start;
        // Light2:
        vec3 dir2 = Light2WorldPosition.xyz - start;
        vec3 norm_dir2 = normalize(dir2);
        vec3 delta_dir2 = norm_dir2 * delta;
        vec3 position2 = start;
        // Light3:
        vec3 dir3 = Light3WorldPosition.xyz - start;
        vec3 norm_dir3 = normalize(dir3);
        vec3 delta_dir3 = norm_dir3 * delta;
        vec3 position3 = start;

        bool break1 = false;
        bool break2 = false;
        bool break3 = false;

        if ( Light0Type == -1.0 )
        {
            break1 = true;
        }
        if ( Light2Type == -1.0 )
        {
            break2 = true;
        }
        if ( Light3Type == -1.0 )
        {
            break3 = true;
        }

        for( int i = 0; i < 450; i++ )
        {
            // Light0:
            if ( !break1 )
            {
                // If we find a non-zero opacity, remove the color contribution
                color = texture3D(bVolumeImageA,position1);
                opacity = texture1D(dOpacityMapA,color.r).r;
                if (opacity > 0.0)
                {
                    normal2 = texture3D(eNormalMapA,position1);
                    local_normal2 = normal2.xyz - (0.5, 0.5, 0.5);
                    local_normal2 = normalize( local_normal2 );
                    if ( dot(local_normal.xyz, local_normal2.xyz) < 0.0 )
                    {
                        color_acc -= color0;
                        break1 = true;
                    }
                }
                // Increment position along the ray:
                if ( Light0Type == 1.0 )
                {
                    position1 -= delta_dir1;
                }
                else
                {
                    position1 += delta_dir1;
                }
                if ( (position1.x < 0.0) || (position1.x > 1.0) ||
                     (position1.y < 0.0) || (position1.y > 1.0) ||
                     (position1.z < 0.0) || (position1.z > 1.0)    )
                {
                    break1 = true;
                }
            }

            // Light2:
            if ( !break2 )
            {
                // If we find a non-zero opacity, remove the color contribution
                color = texture3D(bVolumeImageA,position2);
                opacity = texture1D(dOpacityMapA,color.r).r;
                if (opacity > 0.0)
                {
                    normal2 = texture3D(eNormalMapA,position2);
                    local_normal2 = normal2.xyz - (0.5, 0.5, 0.5);
                    local_normal2 = normalize( local_normal2 );
                    if ( dot(local_normal.xyz, local_normal2.xyz) < 0.0 )
                    {
                        color_acc -= color2;
                        break2 = true;
                    }
                }
                // Increment position along the ray:
                if ( Light2Type == 1.0 )
                {
                    position2 -= delta_dir2;
                }
                else
                {
                    position2 += delta_dir2;
                }
                if ( (position2.x < 0.0) || (position2.x > 1.0) ||
                     (position2.y < 0.0) || (position2.y > 1.0) ||
                     (position2.z < 0.0) || (position2.z > 1.0)    )
                {
                    break2 = true;
                }
            }            
            // Light3:
            if ( !break3 )
            {
                // If we find a non-zero opacity, remove the color contribution
                color = texture3D(bVolumeImageA,position3);
                opacity = texture1D(dOpacityMapA,color.r).r;
                if (opacity > 0.0)
                {
                    normal2 = texture3D(eNormalMapA,position3);
                    local_normal2 = normal2.xyz - (0.5, 0.5, 0.5);
                    local_normal2 = normalize( local_normal2 );
                    if ( dot(local_normal.xyz, local_normal2.xyz) < 0.0 )
                    {
                        color_acc -= color3;
                        break3 = true;
                    }
                }
                // Increment position along the ray:
                if ( Light3Type == 1.0 )
                {
                    position3 -= delta_dir3;
                }
                else
                {
                    position3 += delta_dir3;
                }
                if ( (position3.x < 0.0) || (position3.x > 1.0) ||
                     (position3.y < 0.0) || (position3.y > 1.0) ||
                     (position3.z < 0.0) || (position3.z > 1.0)    )
                {
                    break3 = true;
                }
            }            

            if ( break1 && break2 && break3 )
            {
                break;
            }
        }
    }

    gl_FragColor.rgb = alpha_acc * color_acc.rgb + (1.0 - alpha_acc)*BackgroundColor.rgb;
    gl_FragColor.a = 1.0;
}
