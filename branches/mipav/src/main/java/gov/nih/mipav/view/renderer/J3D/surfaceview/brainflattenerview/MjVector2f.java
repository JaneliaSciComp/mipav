package gov.nih.mipav.view.renderer.J3D.surfaceview.brainflattenerview;


import javax.vecmath.*;


/**
 * Add methods which make the Vector2f more robust and flexible to use.
 */
public class MjVector2f extends Vector2f {

    //~ Static fields/initializers -------------------------------------------------------------------------------------

    /** Use serialVersionUID for interoperability. */
    private static final long serialVersionUID = -1076716465909115350L;

    /** Special vectors. */
    public static final MjVector2f ZERO = new MjVector2f(0.0f, 0.0f);

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Constructors.
     */
    public MjVector2f() { }

    /**
     * Creates a new MjVector2f object.
     *
     * @param  kTuple  DOCUMENT ME!
     */
    public MjVector2f(Tuple2f kTuple) {
        super(kTuple);
    }

    /**
     * Creates a new MjVector2f object.
     *
     * @param  fX  DOCUMENT ME!
     * @param  fY  DOCUMENT ME!
     */
    public MjVector2f(float fX, float fY) {
        super(fX, fY);
    }

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * Returns Cross((x,y,0),(V.x,V.y,0)) = x*V.y - y*V.x.
     *
     * @param   kV  DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    float kross(Vector2f kV) {
        return (x * kV.y) - (y * kV.x);
    }

    /**
     * Normalize this vector in place. If the vector is very close to zero length, then this vector is stored as the
     * zero vector.
     */
    void normalizeSafe() {
        float fLengthSquared = lengthSquared();

        if (0.0f == fLengthSquared) {
            set(ZERO);
        } else {
            scale(1.0f / (float) Math.sqrt(fLengthSquared));
        }
    }

    /**
     * Sets the value of this vector to the normalization of the specified vector. If the vector is very close to zero
     * length, then this vector is stored as the zero vector.
     *
     * @param  kVector  DOCUMENT ME!
     */
    void normalizeSafe(Vector2f kVector) {
        set(kVector);
        normalizeSafe();
    }

    /**
     * Set this vector to the 'perp' of the specified vector.
     *
     * @param  kVector  DOCUMENT ME!
     */
    void perp(Vector2f kVector) {
        set(kVector.y, -kVector.x);
    }
}
