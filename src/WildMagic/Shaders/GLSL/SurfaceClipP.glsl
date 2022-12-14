/**
 * Clip the volume based on the x,y,z axes.
 * returns 1 when the volume is clipped, 0 when not clipped.
 */
bool myClip(vec3 myvec,
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

uniform float clipX;
uniform float clipXInv;
uniform float clipY;
uniform float clipYInv;
uniform float clipZ;
uniform float clipZInv;

in vec3 varTexCoord;
in vec4 varColor;
out vec4 fragColor;

void p_SurfaceClipP()
{
    // axis-aligned clipping:
    if ( myClip( varTexCoord.xyz, clipX, clipXInv, clipY, clipYInv, clipZ, clipZInv ) )
    {
        discard;
    }
    fragColor = varColor;
}
