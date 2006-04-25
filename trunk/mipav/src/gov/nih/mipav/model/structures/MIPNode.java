package gov.nih.mipav.model.structures;


/**
 * Structure to store the maximum intensity projection value for a 3d point.
 */
public class MIPNode {

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** DOCUMENT ME! */
    public float intensity;

    /** DOCUMENT ME! */
    public float x, y, z;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Creates a new MIPNode object.
     */
    public MIPNode() { } // end MIPNode()

    /**
     * Creates a new MIPNode object.
     *
     * @param  _x   DOCUMENT ME!
     * @param  _y   DOCUMENT ME!
     * @param  _z   DOCUMENT ME!
     * @param  val  DOCUMENT ME!
     */
    public MIPNode(int _x, int _y, int _z, float val) {
        x = _x;
        y = _y;
        z = _z;
        intensity = val;
    } // end MIPNode(...)

} // end class MIPNode
