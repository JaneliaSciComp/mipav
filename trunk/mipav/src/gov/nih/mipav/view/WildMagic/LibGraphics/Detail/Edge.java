package gov.nih.mipav.view.WildMagic.LibGraphics.Detail;
import java.util.*;

/**
 * A representation of an edge for the vertex-edge-triangle table. This class stores the pair of vertex indices for
 * the end points of the edge. The edges <V0,V1> and <V1,V0> are considered to be identical. To simplify
 * comparisons, the class stores the ordered indices. The class extends Object to obtain support for hashing into a
 * map of edges.
 */
public class Edge extends Object {

    /** DOCUMENT ME! */
    public int m_iV0, m_iV1;

    /**
     * Constructs an edge in the table.
     *
     * @param  iV0  a vertex index for an end point
     * @param  iV1  a vertex index for an end point
     */
    public Edge(int iV0, int iV1) {

        if (iV0 < iV1) {

            // V0 is minimum
            m_iV0 = iV0;
            m_iV1 = iV1;
        } else {

            // V1 is minimum
            m_iV0 = iV1;
            m_iV1 = iV0;
        }
    }

    /**
     * Support for hashing into a map of edges.
     *
     * @param   kObject  an edge for comparison to the current one
     *
     * @return  true iff the edges are identical. Because the class stores ordered indices, it is not necessary to
     *          use the more expensive test (V0 == other.V0 && V1 == other.V1) || (V0 == other.V1 && V1 ==
     *          other.V0).
     */
    public boolean equals(Object kObject) {
        Edge kE = (Edge) kObject;

        return (m_iV0 == kE.m_iV0) && (m_iV1 == kE.m_iV1);
    }

    /**
     * Support for hashing into a map of edges.
     *
     * @return  the hash key for the edge
     */
    public int hashCode() {
        return (m_iV0 << 16) | m_iV1;
    }
}
