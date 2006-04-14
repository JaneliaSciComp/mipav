package gov.nih.mipav.view.renderer.surfaceview.brainflattenerview;

import javax.vecmath.*;

/**
 * Add methods which make the Vector2f more robust and flexible to use.
 */
public class MjVector2f
        extends Vector2f
{
    // Special vectors.
    public static final MjVector2f ZERO = new MjVector2f(0.0f,0.0f);

    // Constructors
    public MjVector2f()
    {
    }
    public MjVector2f (float fX, float fY)
    {
        super(fX,fY);
    }
    public MjVector2f (Tuple2f kTuple)
    {
        super(kTuple);
    }

    // Normalize this vector in place.  If the vector is very close to
    // zero length, then this vector is stored as the zero vector.
    void normalizeSafe()
    {
        float fLengthSquared = lengthSquared();
        if ( 0.0f == fLengthSquared )
        {
            set(ZERO);
        }
        else
        {
            scale(1.0f/(float)Math.sqrt(fLengthSquared));
        }
    }

    // Sets the value of this vector to the normalization of the specified
    // vector.  If the vector is very close to zero length, then this
    // vector is stored as the zero vector.
    void normalizeSafe (Vector2f kVector)
    {
        set(kVector);
        normalizeSafe();
    }

    // Set this vector to the 'perp' of the specified vector.
    void perp (Vector2f kVector)
    {
        set(kVector.y,-kVector.x);
    }

    // Returns Cross((x,y,0),(V.x,V.y,0)) = x*V.y - y*V.x
    float kross (Vector2f kV)
    {
        return x*kV.y - y*kV.x;
    }
}
