
uniform sampler3D jVolumeImageB; 
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
void main()
{
    // current position along the ray: 
    vec4 position = gl_TexCoord[0];
    
    float bClipped = 0.0;
    
    // axis-aligned clipping:
    if ( DoClip != 0.0 )
    {
        if ( position.x > clipX )
        {
            bClipped = 1.0;
        }
        else if ( position.x < clipXInv )
        {
            bClipped = 1.0;
        }
        else if ( position.y > clipY )
        {
            bClipped = 1.0;
        }
        else if ( position.y < clipYInv )
        {
            bClipped = 1.0;
        }
        else if ( position.z > clipZ )
        {
            bClipped = 1.0;
        }
        else if ( position.z < clipZInv )
        {
            bClipped = 1.0;
        } 
        else 
        {
            bClipped = 0.0;
        }

        if ( bClipped != 1.0 )
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
                bClipped = 1.0;
            }
        }
    }
    if ( bClipped == 1.0 )
    {
        gl_FragColor = vec4(0.0);
    }
    else
    {
        gl_FragColor = texture3D( jVolumeImageB,  gl_TexCoord[0].xyz );
    }
    gl_FragColor.a = 1;
}
