package gov.nih.mipav.view.WildMagic.LibFoundation.Meshes;

import gov.nih.mipav.view.WildMagic.LibFoundation.Mathematics.*;
import gov.nih.mipav.view.WildMagic.LibGraphics.SceneGraph.*;

/**
 * DOCUMENT ME!
 */
public class MeshCurvature {

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** DOCUMENT ME! */
    protected float[] m_afMaxCurvature;

    /** DOCUMENT ME! */
    protected float[] m_afMinCurvature;

    /** DOCUMENT ME! */
    protected Vector3f[] m_akMaxDirection;

    /** DOCUMENT ME! */
    protected Vector3f[] m_akMinDirection;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * The caller is responsible for deleting the input arrays.
     *
     */
    public MeshCurvature( TriMesh kMesh )
    {
        int iVQuantity = kMesh.VBuffer.GetVertexQuantity();

        // compute the matrix of normal derivatives
        Matrix3f[] akDNormal = new Matrix3f[iVQuantity];
        Matrix3f[] akWWTrn = new Matrix3f[iVQuantity];
        Matrix3f[] akDWTrn = new Matrix3f[iVQuantity];

        for (int i = 0; i < iVQuantity; i++) {

            // construct and initialize to all zeroes
            akDNormal[i] = new Matrix3f();
            akWWTrn[i] = new Matrix3f();
            akDWTrn[i] = new Matrix3f();
        }

        int[] aiV = new int[3];

        int[] aiConnect = kMesh.IBuffer.GetData();
        int iTQuantity = kMesh.GetTriangleQuantity();
        Vector3f kPos0 = new Vector3f();
        Vector3f kPos1 = new Vector3f();
        Vector3f kPos2= new Vector3f();
        Vector3f kNormal0 = new Vector3f();
        Vector3f kNormal1 = new Vector3f();
        Vector3f kNormal2 = new Vector3f();
        for (int i = 0; i < iTQuantity; i++) {

            // get vertex indices
            aiV[0] = aiConnect[(3 * i) + 0];
            aiV[1] = aiConnect[(3 * i) + 1];
            aiV[2] = aiConnect[(3 * i) + 2];

            Vector3f kE = new Vector3f();
            Vector3f kW = new Vector3f();
            Vector3f kD = new Vector3f();

            for (int j = 0; j < 3; j++) {
                int iV0 = aiV[j];
                int iV1 = aiV[(j + 1) % 3];
                int iV2 = aiV[(j + 2) % 3];

                // Compute edge from V0 to V1, project to tangent plane of vertex,
                // and compute difference of adjacent normals.
                kMesh.VBuffer.GetPosition3( iV0, kPos0 );
                kMesh.VBuffer.GetPosition3( iV1, kPos1 );
                kMesh.VBuffer.GetPosition3( iV2, kPos2 );
                kMesh.VBuffer.GetNormal3( iV0, kNormal0 );
                kMesh.VBuffer.GetNormal3( iV1, kNormal1 );
                kMesh.VBuffer.GetNormal3( iV2, kNormal2 );

                kPos1.sub( kPos0, kE);
                kW = kNormal0.scale(-kE.Dot(kNormal0));
                kW.addEquals(kE);
                kNormal1.sub(kNormal0, kD);
                akWWTrn[iV0].SetData( 0, 0, akWWTrn[iV0].GetData(0, 0) + kW.X() * kW.X());
                akWWTrn[iV0].SetData( 0, 1, akWWTrn[iV0].GetData(0, 1) + kW.X() * kW.Y());
                akWWTrn[iV0].SetData( 0, 2, akWWTrn[iV0].GetData(0, 2) + kW.X() * kW.Z());
                akWWTrn[iV0].SetData( 1, 0, akWWTrn[iV0].GetData(1, 0) + kW.Y() * kW.X());
                akWWTrn[iV0].SetData( 1, 1, akWWTrn[iV0].GetData(1, 1) + kW.Y() * kW.Y());
                akWWTrn[iV0].SetData( 1, 2, akWWTrn[iV0].GetData(1, 2) + kW.Y() * kW.Z());
                akWWTrn[iV0].SetData( 2, 0, akWWTrn[iV0].GetData(2, 0) + kW.Z() * kW.X());
                akWWTrn[iV0].SetData( 2, 1, akWWTrn[iV0].GetData(2, 1) + kW.Z() * kW.Y());
                akWWTrn[iV0].SetData( 2, 2, akWWTrn[iV0].GetData(2, 2) + kW.Z() * kW.Z());
                akDWTrn[iV0].SetData( 0, 0, akDWTrn[iV0].GetData(0, 0) + kD.X() * kW.X());
                akDWTrn[iV0].SetData( 0, 1, akDWTrn[iV0].GetData(0, 1) + kD.X() * kW.Y());
                akDWTrn[iV0].SetData( 0, 2, akDWTrn[iV0].GetData(0, 2) + kD.X() * kW.Z());
                akDWTrn[iV0].SetData( 1, 0, akDWTrn[iV0].GetData(1, 0) + kD.Y() * kW.X());
                akDWTrn[iV0].SetData( 1, 1, akDWTrn[iV0].GetData(1, 1) + kD.Y() * kW.Y());
                akDWTrn[iV0].SetData( 1, 2, akDWTrn[iV0].GetData(1, 2) + kD.Y() * kW.Z());
                akDWTrn[iV0].SetData( 2, 0, akDWTrn[iV0].GetData(2, 0) + kD.Z() * kW.X());
                akDWTrn[iV0].SetData( 2, 1, akDWTrn[iV0].GetData(2, 1) + kD.Z() * kW.Y());
                akDWTrn[iV0].SetData( 2, 2, akDWTrn[iV0].GetData(2, 2) + kD.Z() * kW.Z());

                // Compute edge from V0 to V2, project to tangent plane of vertex,
                // and compute difference of adjacent normals.
                kPos2.sub(kPos0, kE);
                kW = kNormal0.scale(-kE.Dot(kNormal0));
                kW.addEquals(kE);
                kNormal2.sub(kNormal0, kD);
                akWWTrn[iV0].SetData( 0, 0, akWWTrn[iV0].GetData(0, 0) + kW.X() * kW.X());
                akWWTrn[iV0].SetData( 0, 1, akWWTrn[iV0].GetData(0, 1) + kW.X() * kW.Y());
                akWWTrn[iV0].SetData( 0, 2, akWWTrn[iV0].GetData(0, 2) + kW.X() * kW.Z());
                akWWTrn[iV0].SetData( 1, 0, akWWTrn[iV0].GetData(1, 0) + kW.Y() * kW.X());
                akWWTrn[iV0].SetData( 1, 1, akWWTrn[iV0].GetData(1, 1) + kW.Y() * kW.Y());
                akWWTrn[iV0].SetData( 1, 2, akWWTrn[iV0].GetData(1, 2) + kW.Y() * kW.Z());
                akWWTrn[iV0].SetData( 2, 0, akWWTrn[iV0].GetData(2, 0) + kW.Z() * kW.X());
                akWWTrn[iV0].SetData( 2, 1, akWWTrn[iV0].GetData(2, 1) + kW.Z() * kW.Y());
                akWWTrn[iV0].SetData( 2, 2, akWWTrn[iV0].GetData(2, 2) + kW.Z() * kW.Z());
                akDWTrn[iV0].SetData( 0, 0, akDWTrn[iV0].GetData(0, 0) + kD.X() * kW.X());
                akDWTrn[iV0].SetData( 0, 1, akDWTrn[iV0].GetData(0, 1) + kD.X() * kW.Y());
                akDWTrn[iV0].SetData( 0, 2, akDWTrn[iV0].GetData(0, 2) + kD.X() * kW.Z());
                akDWTrn[iV0].SetData( 1, 0, akDWTrn[iV0].GetData(1, 0) + kD.Y() * kW.X());
                akDWTrn[iV0].SetData( 1, 1, akDWTrn[iV0].GetData(1, 1) + kD.Y() * kW.Y());
                akDWTrn[iV0].SetData( 1, 2, akDWTrn[iV0].GetData(1, 2) + kD.Y() * kW.Z());
                akDWTrn[iV0].SetData( 2, 0, akDWTrn[iV0].GetData(2, 0) + kD.Z() * kW.X());
                akDWTrn[iV0].SetData( 2, 1, akDWTrn[iV0].GetData(2, 1) + kD.Z() * kW.Y());
                akDWTrn[iV0].SetData( 2, 2, akDWTrn[iV0].GetData(2, 2) + kD.Z() * kW.Z());
            }
        }

        // Add in N*N^T to W*W^T for numerical stability.  In theory 0*0^T gets
        // added to D*W^T, but of course no update needed in the implementation.
        // Compute the matrix of normal derivatives.
        Matrix3f kInverse = new Matrix3f();

        for (int i = 0; i < iVQuantity; i++) {
            kMesh.VBuffer.GetNormal3( i, kNormal0 );

            akWWTrn[i].scale(0.5f);
            akWWTrn[i].SetData( 0, 0, akWWTrn[i].GetData(0, 0) + kNormal0.X() * kNormal0.X());
            akWWTrn[i].SetData( 0, 1, akWWTrn[i].GetData(0, 1) + kNormal0.X() * kNormal0.Y());
            akWWTrn[i].SetData( 0, 2, akWWTrn[i].GetData(0, 2) + kNormal0.X() * kNormal0.Z());
            akWWTrn[i].SetData( 1, 0, akWWTrn[i].GetData(1, 0) + kNormal0.Y() * kNormal0.X());
            akWWTrn[i].SetData( 1, 1, akWWTrn[i].GetData(1, 1) + kNormal0.Y() * kNormal0.Y());
            akWWTrn[i].SetData( 1, 2, akWWTrn[i].GetData(1, 2) + kNormal0.Y() * kNormal0.Z());
            akWWTrn[i].SetData( 2, 0, akWWTrn[i].GetData(2, 0) + kNormal0.Z() * kNormal0.X());
            akWWTrn[i].SetData( 2, 1, akWWTrn[i].GetData(2, 1) + kNormal0.Z() * kNormal0.Y());
            akWWTrn[i].SetData( 2, 2, akWWTrn[i].GetData(2, 2) + kNormal0.Z() * kNormal0.Z());
            akDWTrn[i].scale(0.5f);

            kInverse = akWWTrn[i].Inverse();
            akDNormal[i] = akDWTrn[i].mult(kInverse);
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
        m_afMinCurvature = new float[iVQuantity];
        m_afMaxCurvature = new float[iVQuantity];
        m_akMinDirection = new Vector3f[iVQuantity];
        m_akMaxDirection = new Vector3f[iVQuantity];

        Vector3f kU = new Vector3f();
        Vector3f kV = new Vector3f();
        float[][] aS = new float[2][2];
        Vector3f kTransformedU = new Vector3f();
        Vector3f kTransformedV = new Vector3f();

        
        for (int i = 0; i < iVQuantity; i++) {

            kMesh.VBuffer.GetNormal3( i, kNormal0 );

            // compute U and V given N
            Vector3f.GenerateOrthonormalBasis(kU, kV, kNormal0);

            // Compute S = J^T * dN/dX * J.  In theory S is symmetric, but
            // because we have estimated dN/dX, we must slightly adjust our
            // calculations to make sure S is symmetric.
            akDNormal[i].mult(kU, kTransformedU);
            akDNormal[i].mult(kV, kTransformedV);

            float fS01 = kU.Dot(kTransformedV);
            float fS10 = kV.Dot(kTransformedU);
            float fSAvr = 0.5f * (fS01 + fS10);
            aS[0][0] = kU.Dot(kTransformedU);
            aS[0][1] = fSAvr;
            aS[1][0] = fSAvr;
            aS[1][1] = kV.Dot(kTransformedV);

            // compute the eigenvalues of S (min and max curvatures)
            float fTrace = aS[0][0] + aS[1][1];
            float fDet = (aS[0][0] * aS[1][1]) - (aS[0][1] * aS[1][0]);
            float fDiscr = (fTrace * fTrace) - (4.0f * fDet);
            float fRootDiscr = (float) Math.sqrt(Math.abs(fDiscr));
            m_afMinCurvature[i] = 0.5f * (fTrace - fRootDiscr);
            m_afMaxCurvature[i] = 0.5f * (fTrace + fRootDiscr);

            // compute the eigenvectors of S
            Vector2f kW0 = new Vector2f(aS[0][1], m_afMinCurvature[i] - aS[0][0]);
            Vector2f kW1 = new Vector2f(m_afMinCurvature[i] - aS[1][1], aS[1][0]);

            if (kW0.SquaredLength() >= kW1.SquaredLength()) {
                kW0.Normalize();
                m_akMinDirection[i] = new Vector3f();
                m_akMinDirection[i].scaleEquals(kW0.X());
                m_akMinDirection[i].addEquals(kU);
                m_akMinDirection[i].scaleEquals(kW0.Y());
                m_akMinDirection[i].addEquals(kV);
            } else {
                kW1.Normalize();
                m_akMinDirection[i] = new Vector3f();
                m_akMinDirection[i].scaleEquals(kW1.X());
                m_akMinDirection[i].addEquals(kU);
                m_akMinDirection[i].scaleEquals(kW1.Y());
                m_akMinDirection[i].addEquals(kV);
            }

            kW0 = new Vector2f(aS[0][1], m_afMaxCurvature[i] - aS[0][0]);
            kW1 = new Vector2f(m_afMaxCurvature[i] - aS[1][1], aS[1][0]);

            if (kW0.SquaredLength() >= kW1.SquaredLength()) {
                kW0.Normalize();
                m_akMaxDirection[i] = new Vector3f();
                m_akMaxDirection[i].scaleEquals(kW0.X());
                m_akMaxDirection[i].addEquals(kU);
                m_akMaxDirection[i].scaleEquals(kW0.Y());
                m_akMaxDirection[i].addEquals(kV);
            } else {
                kW1.Normalize();
                m_akMaxDirection[i] = new Vector3f();
                m_akMaxDirection[i].scaleEquals(kW1.X());
                m_akMaxDirection[i].addEquals(kU);
                m_akMaxDirection[i].scaleEquals(kW1.Y());
                m_akMaxDirection[i].addEquals(kV);
            }
        }
    }

    //~ Methods --------------------------------------------------------------------------------------------------------


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
    public Vector3f[] getMaxDirections() {
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
    public Vector3f[] getMinDirections() {
        return m_akMinDirection;
    }
}
