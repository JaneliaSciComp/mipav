
uniform sampler3D volImage; 
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
uniform mat4 volumeMatrix;

in vec3 varTexCoord;
out vec4 fragColor;
void main()
{
    // current position along the ray: 
    vec3 position = varTexCoord;
    
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
            aPosition = volumeMatrix*aPosition;
            aPosition.xyz = aPosition.xyz + (0.5,0.5,0.5);
            float fDot = aPosition.z;
            float fDotInv = aPosition.z;
            float fDotArb = dot( position.xyz, clipArb.xyz );
            if ( (fDot < clipEye.w) || (fDotInv > clipEyeInv.w) || (fDotArb > clipArb.w) )
            {
                bClipped = 1.0;
            }
        }
    }
    if ( bClipped == 1.0 )
    {
        fragColor = vec4(0.0);
    }
    else
    {
        fragColor = texture( volImage,  varTexCoord );
    }
}
