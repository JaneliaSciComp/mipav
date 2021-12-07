package gov.nih.mipav.view.renderer.J3D.surfaceview.brainflattenerview;


/**
 * DOCUMENT ME!
 */
public class MjEdgeKey {

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** DOCUMENT ME! */
    public final int[] V = new int[2];

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Creates a new MjEdgeKey object.
     */
    public MjEdgeKey() {
        V[0] = -1;
        V[1] = -1;
    }

    /**
     * Creates a new MjEdgeKey object.
     *
     * @param  iV0  DOCUMENT ME!
     * @param  iV1  DOCUMENT ME!
     */
    public MjEdgeKey(int iV0, int iV1) {

        if (iV0 < iV1) {

            // v0 is minimum
            V[0] = iV0;
            V[1] = iV1;
        } else {

            // v1 is minimum
            V[0] = iV1;
            V[1] = iV0;
        }
    }

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * Support for hashing into a map of edges.
     *
     * @param   kObject  an edge for comparison to the current one
     *
     * @return  true iff the edges are identical. Because the class stores ordered indices, it is not necessary to use
     *          the more expensive test (V0 == other.V0 && V1 == other.V1) || (V0 == other.V1 && V1 == other.V0).
     */
    public boolean equals(Object kObject) {
        MjEdgeKey kKey = (MjEdgeKey) kObject;

        return (V[0] == kKey.V[0]) && (V[1] == kKey.V[1]);
    }

    /**
     * Support for hashing into a map of edges.
     *
     * @return  the hash key for the edge
     */
    public int hashCode() {
        return (V[0] << 16) | V[1];
    }
}
