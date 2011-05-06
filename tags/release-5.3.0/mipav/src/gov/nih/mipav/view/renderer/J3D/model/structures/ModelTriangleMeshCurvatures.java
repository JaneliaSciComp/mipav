package gov.nih.mipav.view.renderer.J3D.model.structures;


import javax.vecmath.*;


/**
 * Storage representation for a triangle mesh. Each vertex stores the coordinates and the normal vector. Each type of
 * vertex value is stored in a separate array, however, the vertex values correspond based on index into the array. The
 * connectivity of the vertices which form each triangle are stored in the array of indices.
 */
public class ModelTriangleMeshCurvatures {

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** Array of mean curvature values at each vertex. */
    private float[] m_afMeanCurvatures = null;

    /** Array of triangle vertex connectivity. */
    private final int[] m_aiIndices;

    /** Array of vertex coordinates. */
    private final Point3f[] m_akCoordinates;

    /** Array of vertex normals. */
    private final Vector3f[] m_akNormals;

    /** Array of triangle normals. */
    private final Vector3f[] m_akTriangleNormals;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Create a set of the min/max curvatures computed for each vertex in the triangle mesh. The triangle mesh is
     * specified by the input ModelTrianglMesh instance.
     *
     * @param  kMesh  ModelTriangleMesh Input triangle mesh which contains the vertex coordinates and vertex triangle
     *                connectivity information.
     */
    public ModelTriangleMeshCurvatures(ModelTriangleMesh kMesh) {

        // Create array to store the vertex coordinates.
        // Retrieve these coordinates from the mesh.
        m_akCoordinates = new Point3f[kMesh.getVertexCount()];

        for (int iVertex = 0; iVertex < m_akCoordinates.length; iVertex++) {
            m_akCoordinates[iVertex] = new Point3f();
        }

        kMesh.getCoordinates(0, m_akCoordinates);

        // Create array to store the connectivity information.
        m_aiIndices = new int[kMesh.getIndexCount()];
        kMesh.getCoordinateIndices(0, m_aiIndices);

        // Allocate array for normals (to be computed later).
        m_akNormals = new Vector3f[m_akCoordinates.length];

        for (int iNormal = 0; iNormal < m_akNormals.length; ++iNormal) {
            m_akNormals[iNormal] = new Vector3f();
        }

        // Create array for the normal vector of each triangle.
        m_akTriangleNormals = new Vector3f[m_aiIndices.length / 3];

        for (int iTriangle = 0; iTriangle < m_akTriangleNormals.length; ++iTriangle) {
            m_akTriangleNormals[iTriangle] = new Vector3f();
        }

        computeTriangleNormals();
        computeVertexNormals();
        computeCurvatures();
    }

    /**
     * Create a set of the min/max curvatures computed for each vertex in the triangle mesh. The triangle mesh is
     * specified by the coordinates of the vertices and the vertex array indices which define the triangle connectivity.
     * From this information, the normal vectors of each triangle can be computed and from that the normal vectors at
     * each vertex can be computed.
     *
     * @param  akCoordinates  array of vertex coordinates
     * @param  aiIndices      array of vertex indices given triangle connectivity
     */
    public ModelTriangleMeshCurvatures(Point3f[] akCoordinates, int[] aiIndices) {

        // Keep track of the coordinates, indices, and normals.
        m_akCoordinates = akCoordinates;
        m_aiIndices = aiIndices;

        // Allocate array for normals (to be computed later).
        m_akNormals = new Vector3f[m_akCoordinates.length];

        for (int iNormal = 0; iNormal < m_akNormals.length; ++iNormal) {
            m_akNormals[iNormal] = new Vector3f();
        }

        // Create array for the normal vector of each triangle.
        m_akTriangleNormals = new Vector3f[m_aiIndices.length / 3];

        for (int iTriangle = 0; iTriangle < m_akTriangleNormals.length; ++iTriangle) {
            m_akTriangleNormals[iTriangle] = new Vector3f();
        }

        computeTriangleNormals();
        computeVertexNormals();
        computeCurvatures();
    }

    /**
     * Create a set of the min/max curvatures computed for each vertex in the triangle mesh. The triangle mesh is
     * specified by the coordinates of the vertices, the normal vectors at each vertex, and the vertex array indices
     * which define the triangle connectivity.
     *
     * @param  akCoordinates  array of vertex coordinates
     * @param  akNormals      array of vertex normals; Must have at leat the same number of elements as does the
     *                        akCoordinates array where each element in this normal array corresponds to the vertex
     *                        coordinate array element at the same index.
     * @param  aiIndices      array of vertex indices given triangle connectivity
     */
    public ModelTriangleMeshCurvatures(Point3f[] akCoordinates, Vector3f[] akNormals, int[] aiIndices) {

        // Keep track of the coordinates, indices, and normals.
        m_akCoordinates = akCoordinates;
        m_aiIndices = aiIndices;
        m_akNormals = akNormals;

        // Create array for the normal vector of each triangle.
        m_akTriangleNormals = new Vector3f[m_aiIndices.length / 3];

        for (int iTriangle = 0; iTriangle < m_akTriangleNormals.length; ++iTriangle) {
            m_akTriangleNormals[iTriangle] = new Vector3f();
        }

        computeTriangleNormals();
        computeCurvatures();
    }

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * Get the array of connectivity among the vertices which define the triangles of the mesh. Each group of three
     * values in the array contains an index into the vertex array for the vertcies which define a single triangle in
     * the mesh.
     *
     * @return  int[] Array of vertex indices. Each index in the array should be a non-negative value less than the
     *          value returned by getNumVertices(). The dimension of this array should equal three times the value
     *          returned by getNumTriangles().
     */
    public int[] getCoordinateIndices() {
        return m_aiIndices;
    }

    /**
     * Get the array of coordinates at each vertex.
     *
     * @return  Point3f[] Array of 3D vertex coordinates. The dimension of this array should equal the value returned by
     *          getNumVertices().
     */
    public Point3f[] getCoordinates() {
        return m_akCoordinates;
    }

    /**
     * Get the array of mean curvature values at each vertex.
     *
     * @return  array of vertex mean curvature values where individual values may be signed; the dimension of this array
     *          should equal the value returned by getNumVertices().
     */
    public float[] getMeanCurvatures() {
        return m_afMeanCurvatures;
    }

    /**
     * Get the number of triangles used to defined the mesh.
     *
     * @return  number of triangles
     */
    public int getNumTriangles() {
        return m_akTriangleNormals.length;
    }

    /**
     * Get the number of vertices used to define the mesh.
     *
     * @return  number of vertices
     */
    public int getNumVertices() {
        return m_akCoordinates.length;
    }

    /**
     * Get the array of normal vectors for each triangle.
     *
     * @return  Vector3f[] Array of 3D normal vectors. The dimension of this array should equal the value returned by
     *          getNumTriangles().
     */
    public Vector3f[] getTriangleNormals() {
        return m_akTriangleNormals;
    }

    /**
     * Get the array of normal vectors at each vertex.
     *
     * @return  Vector3f[] Array of 3D normal vectors. The dimension of this array should equal the value returned by
     *          getNumVertices().
     */
    public Vector3f[] getVertexNormals() {
        return m_akNormals;
    }

    /**
     * Compute the curvatures at each vertex. Description of the algorithm is found in the embedded comments.
     */
    private void computeCurvatures() {
        final int iVQuantity = getNumVertices();
        final int iTQuantity = getNumTriangles();

        // Setup the arrays of matrices, one per vertex.
        float[][][] aaaWWTrn = new float[iVQuantity][3][3];
        float[][][] aaaDWTrn = new float[iVQuantity][3][3];

        for (int iVertex = 0; iVertex < iVQuantity; ++iVertex) {

            for (int iRow = 0; iRow < 3; ++iRow) {

                for (int iCol = 0; iCol < 3; ++iCol) {
                    aaaWWTrn[iVertex][iRow][iCol] = 0.0f;
                    aaaDWTrn[iVertex][iRow][iCol] = 0.0f;
                }
            }
        }

        // compute the matrix of normal derivatives
        int[] aiV = new int[3];
        Vector3f kE = new Vector3f();
        Vector3f kW = new Vector3f();
        Vector3f kD = new Vector3f();
        float[] aW = new float[3];
        float[] aD = new float[3];

        for (int iTriangle = 0; iTriangle < iTQuantity; ++iTriangle) {

            // get vertex indices
            aiV[0] = m_aiIndices[(3 * iTriangle) + 0];
            aiV[1] = m_aiIndices[(3 * iTriangle) + 1];
            aiV[2] = m_aiIndices[(3 * iTriangle) + 2];

            for (int j = 0; j < 3; j++) {
                int iV0 = aiV[j];
                int iV1 = aiV[(j + 1) % 3];
                int iV2 = aiV[(j + 2) % 3];

                // Compute edge from V0 to V1, project to tangent plane of
                // vertex, and compute difference of adjacent normals.
                kE.sub(m_akCoordinates[iV1], m_akCoordinates[iV0]);
                kD.sub(m_akNormals[iV1], m_akNormals[iV0]);
                kW.scaleAdd(-kE.dot(m_akNormals[iV0]), m_akNormals[iV0], kE);
                kD.get(aD);
                kW.get(aW);

                for (int iRow = 0; iRow < 3; iRow++) {

                    for (int iCol = 0; iCol < 3; iCol++) {
                        aaaWWTrn[iV0][iRow][iCol] += aW[iRow] * aW[iCol];
                        aaaDWTrn[iV0][iRow][iCol] += aD[iRow] * aW[iCol];
                    }
                }

                // Compute edge from V0 to V2, project to tangent plane of
                // vertex, and compute difference of adjacent normals.
                kE.sub(m_akCoordinates[iV2], m_akCoordinates[iV0]);
                kD.sub(m_akNormals[iV2], m_akNormals[iV0]);
                kW.scaleAdd(-kE.dot(m_akNormals[iV0]), m_akNormals[iV0], kE);
                kD.get(aD);
                kW.get(aW);

                for (int iRow = 0; iRow < 3; iRow++) {

                    for (int iCol = 0; iCol < 3; iCol++) {
                        aaaWWTrn[iV0][iRow][iCol] += aW[iRow] * aW[iCol];
                        aaaDWTrn[iV0][iRow][iCol] += aD[iRow] * aW[iCol];
                    }
                }
            }
        }

        // Add in N*N^T to W*W^T for numerical stability.  In theory
        // 0*0^T gets added to D*W^T, but of course no update needed in the
        // implementation.  Compute the matrix of normal derivatives.
        Matrix3f[] akDNormal = new Matrix3f[iVQuantity];
        float[] aNormal = new float[3];
        Matrix3f kWWTrn = new Matrix3f();
        Matrix3f kDWTrn = new Matrix3f();
        Matrix3f kWWTrnInv = new Matrix3f();

        for (int iVertex = 0; iVertex < iVQuantity; iVertex++) {
            akDNormal[iVertex] = new Matrix3f();

            // Skip degenerate vertices, i.e., those having
            // zero-length normal vectors.
            if (0.0 == m_akNormals[iVertex].lengthSquared()) {
                akDNormal[iVertex].setIdentity();

                continue;
            }

            m_akNormals[iVertex].get(aNormal);

            for (int iRow = 0; iRow < 3; iRow++) {

                for (int iCol = 0; iCol < 3; iCol++) {
                    aaaWWTrn[iVertex][iRow][iCol] *= 0.5f;
                    aaaDWTrn[iVertex][iRow][iCol] *= 0.5f;

                    aaaWWTrn[iVertex][iRow][iCol] += aNormal[iRow] * aNormal[iCol];

                    kWWTrn.setElement(iRow, iCol, aaaWWTrn[iVertex][iRow][iCol]);
                    kDWTrn.setElement(iRow, iCol, aaaDWTrn[iVertex][iRow][iCol]);
                }
            }

            akDNormal[iVertex] = new Matrix3f();
            kWWTrnInv.invert(kWWTrn);
            akDNormal[iVertex].mul(kDWTrn, kWWTrnInv);
        }

        aaaWWTrn = null;
        aaaDWTrn = null;

        // If N is a unit-length normal at a vertex, let U and V be
        // unit-length tangents so that {U, V, N} is an orthonormal set.
        // Define the matrix J = [U | V], a 3-by-2 matrix whose columns are
        // U and V.  Define J^T to be the transpose of J, a 2-by-3 matrix.
        // Let dN/dX denote the matrix of first-order derivatives of the
        // normal vector field.  The shape matrix is
        // S = (J^T * J)^{-1} * J^T * dN/dX * J = J^T * dN/dX * J
        // where the superscript of -1 denotes the inverse.  (The formula
        // allows for J built from non-perpendicular vectors.) The matrix S
        // is 2-by-2.  The principal curvatures are the eigenvalues of S.
        // If k is a principal curvature and W is the 2-by-1 eigenvector
        // corresponding to it, then S*W = k*W (by definition).  The
        // corresponding 3-by-1 tangent vector at
        // the vertex is called the principal direction for k, and is J*W.
        m_afMeanCurvatures = new float[iVQuantity];

        Vector3f kU = new Vector3f();
        Vector3f kV = new Vector3f();
        Vector3f kU2 = new Vector3f();
        Vector3f kV2 = new Vector3f();

        for (int iVertex = 0; iVertex < iVQuantity; iVertex++) {

            // Skip degenerate vertices, i.e., those having
            // zero-length normal vectors.
            if (0.0 == m_akNormals[iVertex].lengthSquared()) {
                m_afMeanCurvatures[iVertex] = 0.0f;

                continue;
            }

            // compute U and V given N
            generateOrthonormalBasis(kU, kV, m_akNormals[iVertex]);

            // Compute S = J^T * dN/dX * J.  In theory S is symmetric, but
            // because we have estimated dN/dX, we must slightly adjust our
            // calculations to make sure S is symmetric.
            akDNormal[iVertex].transform(kU, kU2);
            akDNormal[iVertex].transform(kV, kV2);

            float fSAvr = 0.5f * (kU.dot(kV2) + kV.dot(kU2));
            float fS00 = kU.dot(kU2);
            float fS01 = fSAvr;
            float fS10 = fSAvr;
            float fS11 = kV.dot(kV2);

            // compute the eigenvalues of S (min and max curvatures)
            float fTrace = fS00 + fS11;
            float fDet = (fS00 * fS11) - (fS01 * fS10);
            float fDiscr = (fTrace * fTrace) - (4.0f * fDet);
            float fRootDiscr = (float) Math.sqrt((double) Math.abs(fDiscr));
            float fMinCurvature = 0.5f * (fTrace - fRootDiscr);
            float fMaxCurvature = 0.5f * (fTrace + fRootDiscr);
            m_afMeanCurvatures[iVertex] = 0.5f * (fMinCurvature + fMaxCurvature);
        }
    }

    /**
     * Compute the normal vector for each triangle as the normal vector to the plane containing the vertices associated
     * with the triangle and considering the vertices to be counterclockwise ordered for a RH rule oriented normal
     * vector. This sets the elements in the m_akTriangleNormals array.
     */
    private void computeTriangleNormals() {

        // Allocate once instead of each time through the loop.
        Vector3f kV0 = new Vector3f();
        Vector3f kV1 = new Vector3f();

        // Compute the normal vector for each triangle.
        for (int iTriangle = 0; iTriangle < m_akTriangleNormals.length; ++iTriangle) {

            // Index into coordinate and normal array for triangle vertices.
            int iIndex = iTriangle * 3;
            int i0 = m_aiIndices[iIndex];
            int i1 = m_aiIndices[iIndex + 1];
            int i2 = m_aiIndices[iIndex + 2];

            // Triangle vertex coordinates.
            Point3f kP0 = m_akCoordinates[i0];
            Point3f kP1 = m_akCoordinates[i1];
            Point3f kP2 = m_akCoordinates[i2];

            // Compute vectors of plane containing triangle vertices.
            kV0.sub(kP1, kP0);
            kV1.sub(kP2, kP0);
            m_akTriangleNormals[iTriangle].cross(kV0, kV1);

            // Normalized plane normal is stored for each triangle.
            // Be careful not to normalize a zero-length vector.
            float fLength = m_akTriangleNormals[iTriangle].length();

            if (fLength > 0.0f) {
                m_akTriangleNormals[iTriangle].scale(1.0f / fLength);
            } else {
                m_akTriangleNormals[iTriangle].set(0.0f, 0.0f, 0.0f);
            }
        }
    }

    /**
     * Compute the normal vector for each vertex as the normalized average normal vector of all the triangles of which
     * the vertex belongs. Do not call this method unless the m_akTriangleNormals array contains valid normalized normal
     * vectors. This sets the elements in the m_akNormals array.
     */
    private void computeVertexNormals() {

        // Constants we need.
        final int iNumNormals = m_akNormals.length;
        final int iNumTriangles = m_akTriangleNormals.length;

        // Compute the normal for each vertex.
        for (int iNormal = 0; iNormal < iNumNormals; ++iNormal) {
            m_akNormals[iNormal].set(0.0f, 0.0f, 0.0f);
        }

        for (int iTriangle = 0; iTriangle < iNumTriangles; ++iTriangle) {
            int i0 = m_aiIndices[iTriangle * 3];
            int i1 = m_aiIndices[(iTriangle * 3) + 1];
            int i2 = m_aiIndices[(iTriangle * 3) + 2];

            Vector3f kNormal = m_akTriangleNormals[iTriangle];
            m_akNormals[i0].add(kNormal);
            m_akNormals[i1].add(kNormal);
            m_akNormals[i2].add(kNormal);
        }

        for (int iNormal = 0; iNormal < iNumNormals; ++iNormal) {

            // Be careful not to normalize a zero-length vector.
            float fLength = m_akNormals[iNormal].length();

            if (fLength > 0.0f) {
                m_akNormals[iNormal].scale(1.0f / fLength);
            } else {
                m_akNormals[iNormal].set(0.0f, 0.0f, 0.0f);
            }
        }
    }

    /**
     * Generate a set (any set) of orthonormal basis vectors.
     *
     * @param  kU  output vector in orthonormal basis (perpendicular to kW)
     * @param  kV  output vector in orthonormal basis (equal to kW cross kU)
     * @param  kW  input non-zero vector
     */
    private void generateOrthonormalBasis(Vector3f kU, Vector3f kV, Vector3f kW) {

        if (Math.abs(kW.x) >= Math.abs(kW.y)) {

            // W.x or W.z is the largest magnitude component, swap them
            float fInvLength = 1.0f / (float) Math.sqrt((kW.x * kW.x) + (kW.z * kW.z));
            kU.x = -kW.z * fInvLength;
            kU.y = 0.0f;
            kU.z = +kW.x * fInvLength;
        } else {

            // W.y or W.z is the largest magnitude component, swap them
            float fInvLength = 1.0f / (float) Math.sqrt((kW.y * kW.y) + (kW.z * kW.z));
            kU.x = 0.0f;
            kU.y = +kW.z * fInvLength;
            kU.z = -kW.y * fInvLength;
        }

        kV.cross(kW, kU);
    }

}
