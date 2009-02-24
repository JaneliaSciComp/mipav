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

uniform sampler3D bVolumeImageA_TEXUNIT1; 
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
uniform mat4 WVPMatrix;
void p_CropClippedA()
{
    // current position along the ray: 
    vec4 position = gl_TexCoord[0];

    bool bClipped = false;

    // axis-aligned clipping:
    if ( (DoClip != 0.0) && myClip( position.xyz, clipX, clipXInv, clipY, clipYInv, clipZ, clipZInv ) )
    {
        bClipped = true;
    }
    else
    {
        bClipped = false;
        if ( DoClip != 0.0 )
        {
            // eye clipping and arbitrary clipping:
            vec4 aPosition = vec4(0.0);
            aPosition.xyz = position.xyz - (0.5,0.5,0.5);
            aPosition = WVPMatrix*aPosition;
            aPosition.xyz = aPosition.xyz + (0.5,0.5,0.5);
            float fDot = dot( aPosition.xyz, clipEye.xyz );
            float fDotInv = dot( aPosition.xyz, clipEyeInv.xyz );
            float fDotArb = dot( position.xyz, clipArb.xyz );
            if ( (fDot < clipEye.w) || (fDotInv > clipEyeInv.w) || (fDotArb > clipArb.w) )
            {
                bClipped = true;
            }
        }
    }
    if ( bClipped )
    {
        gl_FragColor.r = 0.0;
        gl_FragColor.g = 0.0;
        gl_FragColor.b = 0.0;
    }
    else
    {
        gl_FragColor = texture3D( bVolumeImageA_TEXUNIT1,  gl_TexCoord[0].xyz );
    }
    gl_FragColor.a = 1;
}
