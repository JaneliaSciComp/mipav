package gov.nih.mipav.view.renderer.J3D.surfaceview.brainflattenerview;


import javax.vecmath.*;


/**
 * DOCUMENT ME!
 */
public class MjMeshCurvature {

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** DOCUMENT ME! */
    protected float[] m_afMaxCurvature;

    /** DOCUMENT ME! */
    protected float[] m_afMinCurvature;

    /** DOCUMENT ME! */
    protected int[] m_aiConnect;

    /** DOCUMENT ME! */
    protected MjVector3f[] m_akMaxDirection;

    /** DOCUMENT ME! */
    protected MjVector3f[] m_akMinDirection;

    /** DOCUMENT ME! */
    protected MjVector3f[] m_akNormal;

    /** DOCUMENT ME! */
    protected Point3f[] m_akVertex;

    /** DOCUMENT ME! */
    protected int m_iTQuantity;

    /** DOCUMENT ME! */
    protected int m_iVQuantity;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * The caller is responsible for deleting the input arrays.
     *
     * @param  iVQuantity  DOCUMENT ME!
     * @param  akVertex    DOCUMENT ME!
     * @param  iTQuantity  DOCUMENT ME!
     * @param  aiConnect   DOCUMENT ME!
     */
    public MjMeshCurvature(int iVQuantity, Point3f[] akVertex, int iTQuantity, int[] aiConnect) {
        m_iVQuantity = iVQuantity;
        m_akVertex = akVertex;
        m_iTQuantity = iTQuantity;
        m_aiConnect = aiConnect;

        // compute normal vectors
        m_akNormal = new MjVector3f[m_iVQuantity];

        for (int i = 0; i < m_akNormal.length; i++) {
            m_akNormal[i] = new MjVector3f(0.0f, 0.0f, 0.0f);
        }

        MjVector3f kEdge1 = new MjVector3f();
        MjVector3f kEdge2 = new MjVector3f();
        MjVector3f kNormal = new MjVector3f();

        for (int i = 0; i < m_iTQuantity; i++) {

            // get vertex indices
            int iV0 = aiConnect[(3 * i) + 0];
            int iV1 = aiConnect[(3 * i) + 1];
            int iV2 = aiConnect[(3 * i) + 2];

            // compute the normal (length provides a weighted sum)
            kEdge1.sub(m_akVertex[iV1], m_akVertex[iV0]);
            kEdge2.sub(m_akVertex[iV2], m_akVertex[iV0]);
            kNormal.cross(kEdge1, kEdge2);

            m_akNormal[iV0].add(kNormal);
            m_akNormal[iV1].add(kNormal);
            m_akNormal[iV2].add(kNormal);
        }

        for (int i = 0; i < m_iVQuantity; i++) {
            m_akNormal[i].normalizeSafe();
        }

        // compute the matrix of normal derivatives
        Matrix3f[] akDNormal = new Matrix3f[m_iVQuantity];
        Matrix3f[] akWWTrn = new Matrix3f[m_iVQuantity];
        Matrix3f[] akDWTrn = new Matrix3f[m_iVQuantity];

        for (int i = 0; i < m_iVQuantity; i++) {

            // construct and initialize to all zeroes
            akDNormal[i] = new Matrix3f();
            akWWTrn[i] = new Matrix3f();
            akDWTrn[i] = new Matrix3f();
        }

        int[] aiV = new int[3];

        for (int i = 0; i < m_iTQuantity; i++) {

            // get vertex indices
            aiV[0] = aiConnect[(3 * i) + 0];
            aiV[1] = aiConnect[(3 * i) + 1];
            aiV[2] = aiConnect[(3 * i) + 2];

            MjVector3f kE = new MjVector3f();
            MjVector3f kW = new MjVector3f();
            MjVector3f kD = new MjVector3f();

            for (int j = 0; j < 3; j++) {
                int iV0 = aiV[j];
                int iV1 = aiV[(j + 1) % 3];
                int iV2 = aiV[(j + 2) % 3];

                //System.err.println(m_akVertex[iV0].x + " " + m_akVertex[iV0].y + " " + m_akVertex[iV0].z );
                //System.err.println(m_akVertex[iV1].x + " " + m_akVertex[iV1].y + " " + m_akVertex[iV1].z );
                
                // Compute edge from V0 to V1, project to tangent plane of vertex,
                // and compute difference of adjacent normals.
                kE.sub(m_akVertex[iV1], m_akVertex[iV0]);
                kW.scaleAdd(-kE.dot(m_akNormal[iV0]), m_akNormal[iV0], kE);
                kD.sub(m_akNormal[iV1], m_akNormal[iV0]);
                akWWTrn[iV0].m00 += kW.x * kW.x;
                akWWTrn[iV0].m01 += kW.x * kW.y;
                akWWTrn[iV0].m02 += kW.x * kW.z;
                akWWTrn[iV0].m10 += kW.y * kW.x;
                akWWTrn[iV0].m11 += kW.y * kW.y;
                akWWTrn[iV0].m12 += kW.y * kW.z;
                akWWTrn[iV0].m20 += kW.z * kW.x;
                akWWTrn[iV0].m21 += kW.z * kW.y;
                akWWTrn[iV0].m22 += kW.z * kW.z;
                akDWTrn[iV0].m00 += kD.x * kW.x;
                akDWTrn[iV0].m01 += kD.x * kW.y;
                akDWTrn[iV0].m02 += kD.x * kW.z;
                akDWTrn[iV0].m10 += kD.y * kW.x;
                akDWTrn[iV0].m11 += kD.y * kW.y;
                akDWTrn[iV0].m12 += kD.y * kW.z;
                akDWTrn[iV0].m20 += kD.z * kW.x;
                akDWTrn[iV0].m21 += kD.z * kW.y;
                akDWTrn[iV0].m22 += kD.z * kW.z;

                // Compute edge from V0 to V2, project to tangent plane of vertex,
                // and compute difference of adjacent normals.
                kE.sub(m_akVertex[iV2], m_akVertex[iV0]);
                kW.scaleAdd(-kE.dot(m_akNormal[iV0]), m_akNormal[iV0], kE);
                kD.sub(m_akNormal[iV2], m_akNormal[iV0]);
                akWWTrn[iV0].m00 += kW.x * kW.x;
                akWWTrn[iV0].m01 += kW.x * kW.y;
                akWWTrn[iV0].m02 += kW.x * kW.z;
                akWWTrn[iV0].m10 += kW.y * kW.x;
                akWWTrn[iV0].m11 += kW.y * kW.y;
                akWWTrn[iV0].m12 += kW.y * kW.z;
                akWWTrn[iV0].m20 += kW.z * kW.x;
                akWWTrn[iV0].m21 += kW.z * kW.y;
                akWWTrn[iV0].m22 += kW.z * kW.z;
                akDWTrn[iV0].m00 += kD.x * kW.x;
                akDWTrn[iV0].m01 += kD.x * kW.y;
                akDWTrn[iV0].m02 += kD.x * kW.z;
                akDWTrn[iV0].m10 += kD.y * kW.x;
                akDWTrn[iV0].m11 += kD.y * kW.y;
                akDWTrn[iV0].m12 += kD.y * kW.z;
                akDWTrn[iV0].m20 += kD.z * kW.x;
                akDWTrn[iV0].m21 += kD.z * kW.y;
                akDWTrn[iV0].m22 += kD.z * kW.z;
            }
        }

        // Add in N*N^T to W*W^T for numerical stability.  In theory 0*0^T gets
        // added to D*W^T, but of course no update needed in the implementation.
        // Compute the matrix of normal derivatives.
        Matrix3f kInverse = new Matrix3f();

        for (int i = 0; i < m_iVQuantity; i++) {
            akWWTrn[i].mul(0.5f);
            akWWTrn[i].m00 += m_akNormal[i].x * m_akNormal[i].x;
            akWWTrn[i].m01 += m_akNormal[i].x * m_akNormal[i].y;
            akWWTrn[i].m02 += m_akNormal[i].x * m_akNormal[i].z;
            akWWTrn[i].m10 += m_akNormal[i].y * m_akNormal[i].x;
            akWWTrn[i].m11 += m_akNormal[i].y * m_akNormal[i].y;
            akWWTrn[i].m12 += m_akNormal[i].y * m_akNormal[i].z;
            akWWTrn[i].m20 += m_akNormal[i].z * m_akNormal[i].x;
            akWWTrn[i].m21 += m_akNormal[i].z * m_akNormal[i].y;
            akWWTrn[i].m22 += m_akNormal[i].z * m_akNormal[i].z;
            akDWTrn[i].mul(0.5f);

            kInverse.invert(akWWTrn[i]);
            akDNormal[i].mul(akDWTrn[i], kInverse);
        }

        // If N is a unit-length normal at a vertex, let U and V be unit-length
        // tangents so that {U, V, N} is an orthonormal set.  Define the matrix
        // J = [U | V], a 3-by-2 matrix whose columns are U and V.  Define J^T
        // to be the transpose of J, a 2-by-3 matrix.  Let dN/dX denote the
        // matrix of first-order derivatives of the normal vector field.  The
        // shape matrix is
        // S = (J^T * J)^{-1} * J^T * dN/dX * J = J^T * dN/dX * J
        // where the superscript of -1 denotes the inverse.  (The formula allows
        // for J built from non-perpendicular vectors.) The matrix S is 2-by-2.
        // The principal curvatures are the eigenvalues of S.  If k is a principal
        // curvature and W is the 2-by-1 eigenvector corresponding to it, then
        // S*W = k*W (by definition).  The corresponding 3-by-1 tangent vector at
        // the vertex is called the principal direction for k, and is J*W.
        m_afMinCurvature = new float[m_iVQuantity];
        m_afMaxCurvature = new float[m_iVQuantity];
        m_akMinDirection = new MjVector3f[m_iVQuantity];
        m_akMaxDirection = new MjVector3f[m_iVQuantity];

        MjVector3f kU = new MjVector3f();
        MjVector3f kV = new MjVector3f();
        float[][] aS = new float[2][2];
        MjVector3f kTransformedU = new MjVector3f();
        MjVector3f kTransformedV = new MjVector3f();

        for (int i = 0; i < m_iVQuantity; i++) {

            // compute U and V given N
            MjVector3f.generateOrthonormalBasis(kU, kV, m_akNormal[i], true);

            // Compute S = J^T * dN/dX * J.  In theory S is symmetric, but
            // because we have estimated dN/dX, we must slightly adjust our
            // calculations to make sure S is symmetric.
            akDNormal[i].transform(kU, kTransformedU);
            akDNormal[i].transform(kV, kTransformedV);

            float fS01 = kU.dot(kTransformedV);
            float fS10 = kV.dot(kTransformedU);
            float fSAvr = 0.5f * (fS01 + fS10);
            aS[0][0] = kU.dot(kTransformedU);
            aS[0][1] = fSAvr;
            aS[1][0] = fSAvr;
            aS[1][1] = kV.dot(kTransformedV);

            // compute the eigenvalues of S (min and max curvatures)
            float fTrace = aS[0][0] + aS[1][1];
            float fDet = (aS[0][0] * aS[1][1]) - (aS[0][1] * aS[1][0]);
            float fDiscr = (fTrace * fTrace) - (4.0f * fDet);
            float fRootDiscr = (float) Math.sqrt(Math.abs(fDiscr));
            m_afMinCurvature[i] = 0.5f * (fTrace - fRootDiscr);
            m_afMaxCurvature[i] = 0.5f * (fTrace + fRootDiscr);

            // compute the eigenvectors of S
            MjVector2f kW0 = new MjVector2f(aS[0][1], m_afMinCurvature[i] - aS[0][0]);
            MjVector2f kW1 = new MjVector2f(m_afMinCurvature[i] - aS[1][1], aS[1][0]);

            if (kW0.lengthSquared() >= kW1.lengthSquared()) {
                kW0.normalizeSafe();
                m_akMinDirection[i] = new MjVector3f();
                m_akMinDirection[i].scaleAdd(kW0.x, kU);
                m_akMinDirection[i].scaleAdd(kW0.y, kV);
            } else {
                kW1.normalizeSafe();
                m_akMinDirection[i] = new MjVector3f();
                m_akMinDirection[i].scaleAdd(kW1.x, kU);
                m_akMinDirection[i].scaleAdd(kW1.y, kV);
            }

            kW0 = new MjVector2f(aS[0][1], m_afMaxCurvature[i] - aS[0][0]);
            kW1 = new MjVector2f(m_afMaxCurvature[i] - aS[1][1], aS[1][0]);

            if (kW0.lengthSquared() >= kW1.lengthSquared()) {
                kW0.normalizeSafe();
                m_akMaxDirection[i] = new MjVector3f();
                m_akMaxDirection[i].scaleAdd(kW0.x, kU);
                m_akMaxDirection[i].scaleAdd(kW0.y, kV);
            } else {
                kW1.normalizeSafe();
                m_akMaxDirection[i] = new MjVector3f();
                m_akMaxDirection[i].scaleAdd(kW1.x, kU);
                m_akMaxDirection[i].scaleAdd(kW1.y, kV);
            }
        }
    }

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    public int[] getConnect() {
        return m_aiConnect;
    }

    /**
     * DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    public float[] getMaxCurvatures() {
        return m_afMaxCurvature;
    }

    /**
     * DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    public MjVector3f[] getMaxDirections() {
        return m_akMaxDirection;
    }

    /**
     * DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    public float[] getMinCurvatures() {
        return m_afMinCurvature;
    }

    /**
     * DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    public MjVector3f[] getMinDirections() {
        return m_akMinDirection;
    }

    /**
     * derived quantites from the input mesh.
     *
     * @return  DOCUMENT ME!
     */
    public MjVector3f[] getNormals() {
        return m_akNormal;
    }

    /**
     * DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    public int getTQuantity() {
        return m_iTQuantity;
    }

    /**
     * DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    public Point3f[] getVertices() {
        return m_akVertex;
    }

    /**
     * input values from the constructor.
     *
     * @return  DOCUMENT ME!
     */
    public int getVQuantity() {
        return m_iVQuantity;
    }
}
