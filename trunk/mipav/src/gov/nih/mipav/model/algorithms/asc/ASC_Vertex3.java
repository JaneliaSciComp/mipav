package gov.nih.mipav.model.algorithms.asc;

    import java.util.*;

/**
 * A small class for storing the vertices of the vertex-triangle mesh that
 * represents isosurfaces.  The main support is for hash tables of vertices
 * that are used to eliminate duplicate vertices from a mesh.
 */

public class ASC_Vertex3 extends Object implements Comparable
{
    /**
     * Create a vertex with the specified coordinates.
     *
     * @param fX  the x component of the vertex coordinate
     * @param fY  the y component of the vertex coordinate
     * @param fZ  the z component of the vertex coordinate
     */
    public ASC_Vertex3 (float fX, float fY, float fZ)
    {
        m_fX = fX;
        m_fY = fY;
        m_fZ = fZ;
    }

    /**
     * Support for hashing into a map of vertices.
     *
     * @return the hash key for the vertex
     */
    public int hashCode ()
    {
        return (int)(m_fX + 256.0f*m_fY + 65536.0f*m_fZ);
    }

    /**
     * Support for hashing into a map of vertices and for interface
     * Comparator for ordered sets.
     *
     * @param kObject an edge for comparison to the current one
     * @return true iff the vertices are identical.
     */
    public boolean equals (Object kObject)
    {
        ASC_Vertex3 kV = (ASC_Vertex3) kObject;
        return m_fX == kV.m_fX && m_fY == kV.m_fY && m_fZ == kV.m_fZ;
    }

    /**
     * Support for interface Comparable for ordered sets.
     *
     * @param kObj  the vertex to compare this one to
     * @return -1 for kObj0 < kObj1, 0 for kObj0 = kObj1, or +1 for
     *    kObj0 > kObj1
     */
    public int compareTo (Object kObj)
    {
        ASC_Vertex3 kV = (ASC_Vertex3)kObj;

        switch ( ms_iSortOn )
        {
        case 0:  // x, y, z
            if ( m_fX < kV.m_fX ) return -1;
            if ( m_fX > kV.m_fX ) return +1;
            if ( m_fY < kV.m_fY ) return -1;
            if ( m_fY > kV.m_fY ) return +1;
            if ( m_fZ < kV.m_fZ ) return -1;
            if ( m_fZ > kV.m_fZ ) return +1;
            break;

        case 1:  // z, x, y
            if ( m_fZ < kV.m_fZ ) return -1;
            if ( m_fZ > kV.m_fZ ) return +1;
            if ( m_fX < kV.m_fX ) return -1;
            if ( m_fX > kV.m_fX ) return +1;
            if ( m_fY < kV.m_fY ) return -1;
            if ( m_fY > kV.m_fY ) return +1;
            break;

        case 2:  // y, z, x
            if ( m_fY < kV.m_fY ) return -1;
            if ( m_fY > kV.m_fY ) return +1;
            if ( m_fZ < kV.m_fZ ) return -1;
            if ( m_fZ > kV.m_fZ ) return +1;
            if ( m_fX < kV.m_fX ) return -1;
            if ( m_fX > kV.m_fX ) return +1;
            break;
        }

        return 0;
    }

    // the vertex coordinates
    public float m_fX, m_fY, m_fZ;

    // switch for which coordinate to sort on
    static public int ms_iSortOn = 0;
}

