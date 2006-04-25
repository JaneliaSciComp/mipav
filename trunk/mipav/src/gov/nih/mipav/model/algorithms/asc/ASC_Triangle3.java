package gov.nih.mipav.model.algorithms.asc;


/**
 * A small class for storing the triangles of the vertex-triangle mesh that represents isosurfaces. The main support is
 * for hash tables of triangles that are used to eliminate duplicate triangles from a mesh.
 */
public class ASC_Triangle3 {

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** the vertex indices for the vertices of the triangle. */
    public int m_i0, m_i1, m_i2;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Constructs a triangle in the table.
     *
     * @param  i0  a vertex index for a triangle vertex
     * @param  i1  a vertex index for a triangle vertex
     * @param  i2  a vertex index for a triangle vertex
     */
    public ASC_Triangle3(int i0, int i1, int i2) {

        if (i0 < i1) {

            if (i0 < i2) {

                // i0 is minimum
                m_i0 = i0;
                m_i1 = i1;
                m_i2 = i2;
            } else {

                // i2 is minimum
                m_i0 = i2;
                m_i1 = i0;
                m_i2 = i1;
            }
        } else {

            if (i1 < i2) {

                // i1 is minimum
                m_i0 = i1;
                m_i1 = i2;
                m_i2 = i0;
            } else {

                // i2 is minimum
                m_i0 = i2;
                m_i1 = i0;
                m_i2 = i1;
            }
        }
    }

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * Support for hashing into a map of triangles.
     *
     * @param   kObject  a triangle for comparison to the current one
     *
     * @return  true iff the triangles are identical
     */
    public boolean equals(Object kObject) {
        ASC_Triangle3 kT = (ASC_Triangle3) kObject;

        return (m_i0 == kT.m_i0) && (m_i1 == kT.m_i1) && (m_i2 == kT.m_i2);
    }

    /**
     * Support for hashing into a map of triangles.
     *
     * @return  the hash key for the triangle
     */
    public int hashCode() {
        int iCmp;

        if (m_i1 < m_i2) {
            iCmp = (m_i1 << 8) ^ ((m_i0 << 16) | m_i2);
        } else {
            iCmp = (m_i2 << 8) ^ ((m_i0 << 16) | m_i1);
        }

        return iCmp;
    }
}
