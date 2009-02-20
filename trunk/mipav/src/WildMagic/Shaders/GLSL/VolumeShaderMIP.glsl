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
uniform sampler3D eVolumeImageA_GM_TEXUNIT4; 
uniform sampler1D fOpacityMapA_GM_TEXUNIT5; 
uniform float stepsize;
uniform vec4  steps;
uniform float IsColor;
uniform float DoClip;
uniform float GradientMagnitude;
uniform vec4 BackgroundColor;
uniform float clipX;
uniform float clipXInv;
uniform float clipY;
uniform float clipYInv;
uniform float clipZ;
uniform float clipZInv;
uniform vec4 clipArb;
uniform vec4 clipEye;
uniform vec4 clipEyeInv;

/** Raycasting fragment program implementation */
void main()
{
    // find the right place to lookup in the backside buffer
    vec2 texc = ((outPos.xy / outPos.w) + 1.0) / 2.0;
    vec4 back_position  = texture2D(aSceneImage_TEXUNIT0, texc);

    // the start position of the ray is stored in the texture coordinate
    vec3 start = gl_TexCoord[0].xyz; 

    // the ray direction
    vec3 dir = back_position.xyz - start;

    // the length from front to back is calculated and used to terminate the ray
    float len = length(dir.xyz); 

    // normalized direction vector:
    vec3 norm_dir = normalize(dir);

    // current position along the ray: 
    vec3 position = start.xyz;

    // output color:
    gl_FragColor = BackgroundColor;
    if ( (back_position.x == 0.0) && (back_position.y == 0.0) && (back_position.z == 0.0) )
    {
        return;
    }
    float delta = stepsize;
    // limit the number of iterations to STEPS, make sure that the stepsize will
    // cover the entire ray:
    if ( (len/delta) > steps[0] )
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

    // Maximum value along the ray for MIP
    float fMax = 0.0;
    // Intensity value of color:
    float intensity_color = 0.0;
    // maximum color value along the ray
    vec3 color_max = vec3(0.0);

    vec3 delta_dir = norm_dir * delta;
    float delta_dir_len = length(delta_dir);
    float length_acc = 0.0;
    bool bClipped = false;


    // For some profiles the number of loop iterations must be determined at
    // compile time:
    //for ( int i = 0; i < 450; i++ )
    for( int i = 0; i < 1; i++ )
    {
        // axis-aligned clipping:
        if ( (DoClip != 0.0) &&
             myClip( position, clipX, clipXInv, clipY, clipYInv, clipZ, clipZInv ) )
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
                aPosition = WVPMatrix * aPosition;
                aPosition.xyz = aPosition.xyz + (.5,.5,.5);
                // eye clip:
                float fDot = dot( aPosition.xyz, clipEye.xyz );
                // eey inverse clip:
                float fDotInv = dot( aPosition.xyz, clipEyeInv.xyz );
                // arbitrary clip:
                float fDotArb = dot( position.xyz, clipArb.xyz );
                if ( (fDot < clipEye.w) ||
                     (fDotInv > clipEyeInv.w) ||
                     (fDotArb > clipArb.w) )
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
                opacity = texture1D(dOpacityMapA_TEXUNIT3,color.r).r;

                if ( GradientMagnitude != 0.0 )
                {
                    colorGM = texture3D(eVolumeImageA_GM_TEXUNIT4,position);
                    opacityGM = texture1D(fOpacityMapA_GM_TEXUNIT5,colorGM.r).r;
                    opacity = opacity * opacityGM;
                }
            }
        }
        // If the opacity is not zero:
        if ( opacity > 0.0 )
        {
            // MIP: Store the maximum value:
            intensity_color = dot(color.rgb, vec3(.3,.3,.3));
            if ( intensity_color > fMax ) 
            {
                fMax = intensity_color;
                color_max.rgb = color.rgb;
            }
        }
//         // Break early if the max is >= 1
//         if ( fMax >= 1.0)
//         {
//             break;
//         }
//         // Increment position along the ray:
//         position += delta_dir;
//         length_acc += delta_dir_len;
//         // Break when the end of the ray is reached
//         if ( length_acc >= len )
//         {
//             break;
//         }
    } 

    if ( fMax >= 1.0)
    {
        fMax = 1.0;
    }
    if ( IsColor != 0.0 )
    {
        gl_FragColor.r = texture1D(cColorMapA_TEXUNIT2,color_max.r).r;
        gl_FragColor.g = texture1D(cColorMapA_TEXUNIT2,color_max.g).g;
        gl_FragColor.b = texture1D(cColorMapA_TEXUNIT2,color_max.b).b;
    }
    else
    {
        gl_FragColor.rgb = texture1D(cColorMapA_TEXUNIT2,fMax).rgb;
    }
    // Blend with the background color:
    gl_FragColor = fMax * gl_FragColor + (1.0 - fMax) * BackgroundColor;
    gl_FragColor.a = 1.0;
}
