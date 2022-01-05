package gov.nih.mipav.view.renderer.J3D.surfaceview.brainflattenerview;


import javax.vecmath.*;


/**
 * Add methods which make the Vector3f more robust and flexible to use.
 */
public class MjVector3f extends Vector3f {

    //~ Static fields/initializers -------------------------------------------------------------------------------------

    /** Use serialVersionUID for interoperability. */
    private static final long serialVersionUID = -3573463090318350522L;

    /** Special vectors. */
    public static final MjVector3f ZERO = new MjVector3f(0.0f, 0.0f, 0.0f);

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Constructors.
     */
    public MjVector3f() { }

    /**
     * Creates a new MjVector3f object.
     *
     * @param  kTuple  DOCUMENT ME!
     */
    public MjVector3f(Tuple3f kTuple) {
        super(kTuple);
    }

    /**
     * Creates a new MjVector3f object.
     *
     * @param  fX  DOCUMENT ME!
     * @param  fY  DOCUMENT ME!
     * @param  fZ  DOCUMENT ME!
     */
    public MjVector3f(float fX, float fY, float fZ) {
        super(fX, fY, fZ);
    }

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * Input W must be initialized to a nonzero vector, output is {U,V,W}, an orthonormal basis. A hint is provided
     * about whether or not W is already unit length.
     *
     * @param  kU            DOCUMENT ME!
     * @param  kV            DOCUMENT ME!
     * @param  kW            DOCUMENT ME!
     * @param  bUnitLengthW  DOCUMENT ME!
     */
    static void generateOrthonormalBasis(MjVector3f kU, MjVector3f kV, MjVector3f kW, boolean bUnitLengthW) {

        if (!bUnitLengthW) {
            kW.normalizeSafe();
        }

        float fInvLength;

        if (Math.abs(kW.x) >= Math.abs(kW.y)) {

            // W.x or W.z is the largest magnitude component, swap them
            fInvLength = 1.0f / (float) Math.sqrt((kW.x * kW.x) + (kW.z * kW.z));
            kU.x = -kW.z * fInvLength;
            kU.y = 0.0f;
            kU.z = +kW.x * fInvLength;
        } else {

            // W.y or W.z is the largest magnitude component, swap them
            fInvLength = 1.0f / (float) Math.sqrt((kW.y * kW.y) + (kW.z * kW.z));
            kU.x = 0.0f;
            kU.y = +kW.z * fInvLength;
            kU.z = -kW.y * fInvLength;
        }

        kV.cross(kW, kU);
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
    void normalizeSafe(Vector3f kVector) {
        set(kVector);
        normalizeSafe();
    }
}
