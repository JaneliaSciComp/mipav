package gov.nih.mipav.view.renderer.WildMagic.brainflattenerview_WM;


import gov.nih.mipav.view.renderer.surfaceview.brainflattenerview.*;
import java.util.*;
import javax.vecmath.Point3f;
import gov.nih.mipav.view.WildMagic.LibFoundation.Mathematics.*;
import gov.nih.mipav.view.WildMagic.LibFoundation.Meshes.*;
import gov.nih.mipav.view.WildMagic.LibFoundation.Intersection.*;
import gov.nih.mipav.view.WildMagic.LibGraphics.SceneGraph.*;

/**
 * DOCUMENT ME!
 */
public class MjCorticalMesh_WM {

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** DOCUMENT ME! */
    private float[] m_afAvrConvexity;

    /** DOCUMENT ME! */
    private float[] m_afMeanCurvature;

    /** projection of sphere onto cylinder. */
    private Vector3f[] m_akCylinder = null;

    /** DOCUMENT ME! */
    private Edge[] m_akEdge = null;

    /** Conformal mapping to a plane. The (u,v) points correspond to the (x,y,z) mesh points. */
    private Vector2f[] m_akPlane = null;

    /** Conformal mapping to a sphere. rho is the radius of the stereographic sphere. */
    private Vector3f[] m_akSphere = null;

    /** DOCUMENT ME! */
    private Triangle[] m_akTriangle = null;

    /** DOCUMENT ME! */
    private Vertex[] m_akVertex = null;

    /** DOCUMENT ME! */
    private float m_fMaxAvrConvexity;

    /** DOCUMENT ME! */
    private float m_fMaxDistance;

    /** DOCUMENT ME! */
    private float m_fMaxMeanCurvature;

    /** DOCUMENT ME! */
    private float m_fMinAvrConvexity;

    /** DOCUMENT ME! */
    private float m_fMinDistance;

    /** DOCUMENT ME! */
    private float m_fMinMeanCurvature;

    /** DOCUMENT ME! */
    private float m_fRho;

    /** DOCUMENT ME! */
    private float m_fSurfaceArea;

    /** DOCUMENT ME! */
    private int m_iEQuantity;

    /** The index of the puncture triangle for computing the conformal mapping:. */
    private int m_iPunctureTri = 0;

    /** vertex-vertex distances measured along edge paths. */
    private HashMap<EdgeKey, Float> m_kDistance = new HashMap<EdgeKey, Float>(); /* map MjEdgeKey to Float */

    /** surface inflation. */
    private HashMap<EdgeKey, Float> m_kInitDistance = new HashMap<EdgeKey, Float>(); /* map MjEdgeKey to Float */

    /** DOCUMENT ME! */
    private Vector2f m_kPlaneMax;

    /** DOCUMENT ME! */
    private Vector2f m_kPlaneMin;

    private TriMesh m_kMesh;
    private TriMesh m_kSphereMesh = null;
    private TriMesh m_kCylinderMesh = null;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Creates a new MjCorticalMesh object.
     */
    public MjCorticalMesh_WM( TriMesh kMesh)
    {
        m_kMesh = kMesh;
        int iVQuantity = kMesh.VBuffer.GetVertexQuantity();
        int iTQuantity = kMesh.GetTriangleQuantity();

        m_iEQuantity = 0;
        m_fMinMeanCurvature = 0.0f;
        m_fMaxMeanCurvature = 0.0f;
        m_afMeanCurvature = null;
        m_akPlane = null;
        m_akSphere = null;
        m_akCylinder = null;
        m_fMinAvrConvexity = 0.0f;
        m_fMaxAvrConvexity = 0.0f;
        m_afAvrConvexity = null;

        /* dynamically construct triangle mesh from input */
        m_akVertex = new Vertex[iVQuantity];

        for (int i = 0; i < m_akVertex.length; i++) {
            m_akVertex[i] = new Vertex();
        }

        m_akEdge = new Edge[3 * iTQuantity];
        for (int i = 0; i < m_akEdge.length; i++) {
            m_akEdge[i] = new Edge();
        }

        m_akTriangle = new Triangle[iTQuantity];
        for (int i = 0; i < m_akTriangle.length; i++) {
            m_akTriangle[i] = new Triangle();
        }

        HashMap<EdgeKey, Integer> kEMap = new HashMap<EdgeKey, Integer>(); /* map MjEdgeKey -> int */

        int[] aiConnect = kMesh.IBuffer.GetData();
        for (int iT = 0; iT < iTQuantity; iT++) {

            /* update triangle */
            Triangle rkT = m_akTriangle[iT];
            rkT.V[0] = aiConnect[(3 * iT) + 0];
            rkT.V[1] = aiConnect[(3 * iT) + 1];
            rkT.V[2] = aiConnect[(3 * iT) + 2];

            /* add edges to mesh */
            for (int i0 = 2, i1 = 0; i1 < 3; i0 = i1++) {

                /* update vertices */
                m_akVertex[rkT.V[i1]].InsertTriangle(iT);

                EdgeKey kKey = new EdgeKey(rkT.V[i0], rkT.V[i1]);

                if (!kEMap.containsKey(kKey)) {

                    /* first time edge encountered */
                    kEMap.put(kKey, new Integer(m_iEQuantity));

                    /* update edge */
                    Edge rkE = m_akEdge[m_iEQuantity];
                    rkE.V[0] = rkT.V[i0];
                    rkE.V[1] = rkT.V[i1];
                    rkE.T[0] = iT;

                    /* update vertices */
                    m_akVertex[rkE.V[0]].InsertEdge(rkE.V[1], m_iEQuantity);
                    m_akVertex[rkE.V[1]].InsertEdge(rkE.V[0], m_iEQuantity);

                    /* update triangle */
                    rkT.E[i0] = m_iEQuantity;

                    m_iEQuantity++;
                } else {

                    /* second time edge encountered */
                    int iE = ((Integer) (kEMap.get(kKey))).intValue();
                    Edge rkE = m_akEdge[iE];

                    /* update edge */
                    assert (rkE.T[1] == -1); /* mesh must be manifold */
                    rkE.T[1] = iT;

                    /* update triangles */
                    int iAdj = rkE.T[0];
                    Triangle rkAdj = m_akTriangle[iAdj];

                    for (int j = 0; j < 3; j++) {

                        if (rkAdj.E[j] == iE) {
                            rkAdj.T[j] = iT;

                            break;
                        }
                    }

                    rkT.E[i0] = iE;
                    rkT.T[i0] = iAdj;
                }
            }
        }
    }

    //~ Methods --------------------------------------------------------------------------------------------------------

    public int getVQuantity()
    {
        return m_kMesh.VBuffer.GetVertexQuantity();
    }
    
    /**
     * conformal mapping of mesh to plane and sphere, projection to cylinder.
     */
    public void computeConformalMapping()
    {
        int iVQuantity = m_kMesh.VBuffer.GetVertexQuantity();
        m_akPlane = new Vector2f[iVQuantity];
        m_akSphere = new Vector3f[iVQuantity];
        m_akCylinder = new Vector3f[iVQuantity];

        for (int i = 0; i < iVQuantity; i++) {
            m_akPlane[i] = new Vector2f();
            m_akSphere[i] = new Vector3f();
            m_akCylinder[i] = new Vector3f();
        }

        Vector3f kE0 = new Vector3f();
        Vector3f kE1 = new Vector3f();
        Vector3f kNormal = new Vector3f();
        Vector3f kE10 = new Vector3f();
        Vector3f kE20 = new Vector3f();
        Vector3f kE12 = new Vector3f();
        Vector3f kCross = new Vector3f();

        int iV0, iV1, iV2;
        float fValue = 0.0f;

        /* construct sparse matrix A nondiagonal entries */
        MjSparseMatrix_WM kAMat = new MjSparseMatrix_WM();

        Vector3f kPos0 = new Vector3f();
        Vector3f kPos1 = new Vector3f();
        Vector3f kPos2= new Vector3f();
        for (int iE = 0; iE < m_iEQuantity; iE++) {
            Edge kE = m_akEdge[iE];
            iV0 = kE.V[0];
            iV1 = kE.V[1];
            
            m_kMesh.VBuffer.GetPosition3( iV0, kPos0 );
            m_kMesh.VBuffer.GetPosition3( iV1, kPos1 );

            Triangle kT0 = m_akTriangle[kE.T[0]];

            for (int i = 0; i < 3; i++) {
                iV2 = kT0.V[i];

                if ((iV2 != iV0) && (iV2 != iV1)) {
                    m_kMesh.VBuffer.GetPosition3( iV2, kPos2 );

                    kPos0.sub(kPos2, kE0);
                    kPos1.sub(kPos2, kE1);
                    kE0.Cross(kE1, kNormal);
                    fValue = kE0.Dot(kE1) / kNormal.Length();
                }
            }

            Triangle kT1 = m_akTriangle[kE.T[1]];

            for (int i = 0; i < 3; i++) {
                iV2 = kT1.V[i];

                if ((iV2 != iV0) && (iV2 != iV1)) {
                    m_kMesh.VBuffer.GetPosition3( iV2, kPos2 );

                    kPos0.sub(kPos2, kE0);
                    kPos1.sub(kPos2, kE1);
                    kE0.Cross(kE1, kNormal);
                    fValue += kE0.Dot(kE1) / kNormal.Length();
                }
            }

            fValue *= -0.5f;
            kAMat.setElement(iV0, iV1, fValue);
        }

        /* construct sparse matrix A diagonal entries */
        float[] afTmp = new float[iVQuantity];

        for (int i = 0; i < iVQuantity; i++) {
            afTmp[i] = 0.0f;
        }

        Iterator kIter = kAMat.iterator();

        while (kIter.hasNext()) {
            Map.Entry kEntry = (Map.Entry) kIter.next();
            MjSparseMatrix_WM.Index kIndex = (MjSparseMatrix_WM.Index) kEntry.getKey();
            Float kValue = (Float) kEntry.getValue();
            iV0 = kIndex.m_iRow;
            iV1 = kIndex.m_iCol;
            fValue = kValue.floatValue();
            assert (iV0 != iV1);
            afTmp[iV0] -= fValue;
            afTmp[iV1] -= fValue;
        }

        for (int iV = 0; iV < iVQuantity; iV++) {
            kAMat.setElement(iV, iV, afTmp[iV]);
        }

        assert (kAMat.size() == (iVQuantity + m_iEQuantity));

        /* Construct column vector B (happens to be sparse).  Triangle 0 is
         * the default for the puncture, but may also be set by the user */
        Triangle kT = m_akTriangle[m_iPunctureTri];
        iV0 = kT.V[0];
        iV1 = kT.V[1];
        iV2 = kT.V[2];

        Vector3f kV0 = m_kMesh.VBuffer.GetPosition3( iV0 );
        Vector3f kV1 = m_kMesh.VBuffer.GetPosition3( iV1 );
        Vector3f kV2 = m_kMesh.VBuffer.GetPosition3( iV2 );
        kV1.sub(kV0,kE10);
        kV2.sub(kV0,kE20);
        kV1.sub(kV2,kE12);
        kE20.Cross(kE10,kCross);

        float fLen10 = kE10.Length();
        float fInvLen10 = 1.0f / fLen10;
        float fTwoArea = kCross.Length();
        float fInvLenCross = 1.0f / fTwoArea;
        float fInvProd = fInvLen10 * fInvLenCross;
        float fRe0 = -fInvLen10;
        float fIm0 = fInvProd * kE12.Dot(kE10);
        float fRe1 = fInvLen10;
        float fIm1 = fInvProd * kE20.Dot(kE10);
        float fRe2 = 0.0f;
        float fIm2 = -fLen10 * fInvLenCross;

        /* solve sparse system for real parts */
        for (int i = 0; i < iVQuantity; i++) {
            afTmp[i] = 0.0f;
        }

        afTmp[iV0] = fRe0;
        afTmp[iV1] = fRe1;
        afTmp[iV2] = fRe2;

        float[] afResult = new float[iVQuantity];
        boolean bSolved = kAMat.solveSymmetricCG(iVQuantity, afTmp, afResult);
        assert (bSolved);

        for (int i = 0; i < iVQuantity; i++) {
            m_akPlane[i].X(afResult[i]);
        }

        /* solve sparse system for imaginary parts */
        for (int i = 0; i < iVQuantity; i++) {
            afTmp[i] = 0.0f;
        }

        afTmp[iV0] = -fIm0;
        afTmp[iV1] = -fIm1;
        afTmp[iV2] = -fIm2;
        bSolved = kAMat.solveSymmetricCG(iVQuantity, afTmp, afResult);
        assert (bSolved);

        for (int i = 0; i < iVQuantity; i++) {
            m_akPlane[i].Y(afResult[i]);
        }

        /* scale to [-1,1]^2 for numerical conditioning in later steps */
        float fMin = m_akPlane[0].X();
        float fMax = fMin;

        for (int i = 0; i < iVQuantity; i++) {

            if (m_akPlane[i].X() < fMin) {
                fMin = m_akPlane[i].X();
            } else if (m_akPlane[i].X() > fMax) {
                fMax = m_akPlane[i].X();
            }

            if (m_akPlane[i].Y() < fMin) {
                fMin = m_akPlane[i].Y();
            } else if (m_akPlane[i].Y() > fMax) {
                fMax = m_akPlane[i].Y();
            }
        }

        float fHalfRange = 0.5f * (fMax - fMin);
        float fInvHalfRange = 1.0f / fHalfRange;

        for (int i = 0; i < iVQuantity; i++) {
            m_akPlane[i].X( -1.0f + (fInvHalfRange * (m_akPlane[i].X() - fMin)) );
            m_akPlane[i].Y( -1.0f + (fInvHalfRange * (m_akPlane[i].Y() - fMin)) );
        }

        /* Map plane points to sphere using inverse stereographic projection. */
        /* The main issue is selecting a translation in (x,y) and a radius of */
        /* the projection sphere.  Both factors strongly influence the final */
        /* result. */

        /* Use the average as the south pole.  The points tend to be clustered */
        /* approximately in the middle of the conformally mapped punctured */
        /* triangle, so the average is a good choice to place the pole. */
        Vector2f kOrigin = new Vector2f(0.0f, 0.0f);

        for (int i = 0; i < iVQuantity; i++) {
            kOrigin.addEquals(m_akPlane[i]);
        }

        kOrigin.scaleEquals(1.0f / (float) iVQuantity);

        for (int i = 0; i < iVQuantity; i++) {
            m_akPlane[i].subEquals(kOrigin);
        }

        m_kPlaneMin = new Vector2f(m_akPlane[0]);
        m_kPlaneMax = new Vector2f(m_akPlane[0]);

        for (int i = 1; i < iVQuantity; i++) {

            if (m_akPlane[i].X() < m_kPlaneMin.X()) {
                m_kPlaneMin.X( m_akPlane[i].X() );
            } else if (m_akPlane[i].X() > m_kPlaneMax.X()) {
                m_kPlaneMax.X( m_akPlane[i].X() );
            }

            if (m_akPlane[i].Y() < m_kPlaneMin.Y()) {
                m_kPlaneMin.Y( m_akPlane[i].Y() );
            } else if (m_akPlane[i].Y() > m_kPlaneMax.Y()) {
                m_kPlaneMax.Y( m_akPlane[i].Y() );
            }
        }

        /* Select the radius of the sphere so that the projected punctured */
        /* triangle has an area whose fraction of total spherical area is the */
        /* same fraction as the area of the punctured triangle to the total area */
        /* of the original triangle mesh. */
        float fTwoTotalArea = 0.0f;
        int iTQuantity = m_kMesh.GetTriangleQuantity();
        for (int iT = 0; iT < iTQuantity; iT++) {
            Triangle kT0 = m_akTriangle[iT];
            m_kMesh.VBuffer.GetPosition3( kT0.V[0], kV0 );
            m_kMesh.VBuffer.GetPosition3( kT0.V[1], kV1 );
            m_kMesh.VBuffer.GetPosition3( kT0.V[2], kV2 );
            kV1.sub(kV0, kE0);
            kV2.sub(kV0, kE1);
            kE0.Cross(kE1, kCross);
            fTwoTotalArea += kCross.Length();
        }

        m_fRho = computeRadius(new Vector2f(m_akPlane[iV0]), new Vector2f(m_akPlane[iV1]),
                               new Vector2f(m_akPlane[iV2]), fTwoArea / fTwoTotalArea);

        float fRhoSqr = m_fRho * m_fRho;

        /* Inverse stereographic projection to obtain sphere coordinates.  The */
        /* sphere is centered at the origin and has radius 1. */
        Vector2f kPlaneVector = new Vector2f();

        for (int i = 0; i < iVQuantity; i++) {
            kPlaneVector.SetData(m_akPlane[i]);

            float fRSqr = kPlaneVector.SquaredLength();
            float fMult = 1.0f / (fRSqr + fRhoSqr);
            float fX = 2.0f * fMult * fRhoSqr * m_akPlane[i].X();
            float fY = 2.0f * fMult * fRhoSqr * m_akPlane[i].Y();
            float fZ = fMult * m_fRho * (fRSqr - fRhoSqr);
            m_akSphere[i].SetData(fX, fY, fZ);
            m_akSphere[i].scaleEquals(1.0f / m_fRho);
        }

        /* Project the sphere onto a cylinder.  The cylinder is centered at the */
        /* origin and has axis direction (0,0,1).  The radius of the cylinder is */
        /* the radius of the sphere, 1.  The height of the cylinder is 2 since z */
        /* varies from -1 to 1. */
        for (int i = 0; i < iVQuantity; i++) {
            m_akCylinder[i].X( -(float) Math.atan2(m_akSphere[i].Y(), m_akSphere[i].X()) );
            m_akCylinder[i].Y( m_akSphere[i].Z() );
            m_akCylinder[i].Z( 0.0f );
        }
    }

    /**
     * Computes the distances of vertex pairs <V0,V1> for which an edge-path exists with at most iSize edges.
     *
     * @param  iSize  DOCUMENT ME!
     */
    public void computeDistances(int iSize) {
        assert (iSize >= 1);

        if (iSize < 1) {
            return;
        }

        m_kDistance.clear();
        int iVQuantity = m_kMesh.VBuffer.GetVertexQuantity();
        for (int iSource = 0; iSource < iVQuantity; iSource++) {
            computeDistance(iSize, iSource, false);
        }

        computeExtremes();
    }

    /**
     * mean curvature.
     */
    public void computeMeanCurvature( Vector3f kCenter ) {
        int iVQuantity = m_kMesh.VBuffer.GetVertexQuantity();
        m_afMeanCurvature = new float[iVQuantity];


        Point3f[] akPoint = new Point3f[iVQuantity]; 
        for ( int i = 0; i < iVQuantity; i++ )
        {
            akPoint[i] = new Point3f( m_kMesh.VBuffer.GetPosition3fX(i),
                    m_kMesh.VBuffer.GetPosition3fY(i),
                    m_kMesh.VBuffer.GetPosition3fZ(i) );
                    
        }
        int iTQuantity = m_kMesh.GetTriangleQuantity(); 
        int[] aiConnect = m_kMesh.IBuffer.GetData();
        

        MjMeshCurvature kMG = new MjMeshCurvature(iVQuantity, akPoint, iTQuantity, aiConnect);

        //MeshCurvature kMG = new MeshCurvature(m_kMesh, kCenter);
        float[] afMinCurv = kMG.getMinCurvatures();
        float[] afMaxCurv = kMG.getMaxCurvatures();
        m_fMinMeanCurvature = afMinCurv[0] + afMaxCurv[0];
        m_fMaxMeanCurvature = m_fMinMeanCurvature;

        for (int i = 0; i < iVQuantity; i++) {
            m_afMeanCurvature[i] = afMinCurv[i] + afMaxCurv[i];

            if (m_afMeanCurvature[i] < m_fMinMeanCurvature) {
                m_fMinMeanCurvature = m_afMeanCurvature[i];
            } else if (m_afMeanCurvature[i] > m_fMaxMeanCurvature) {
                m_fMaxMeanCurvature = m_afMeanCurvature[i];
            }
            //System.err.println( m_afMeanCurvature[i] + " " + afMinCurv[i] + " " + afMaxCurv[i] );
        }
    }

    /**
     * compute vertex normals as averages of triangle normals.
     */
    public void computeNormals() {
        m_kMesh.UpdateMS();
    }

    /**
     * surface area of the mesh (input mesh is closed, manifold).
     */
    public void computeSurfaceArea() {
        m_fSurfaceArea = 0.0f;

        Vector3f kE1 = new Vector3f();
        Vector3f kE2 = new Vector3f();
        Vector3f kNormal = new Vector3f();

        int[] aiConnect = m_kMesh.IBuffer.GetData();
        int iTQuantity = m_kMesh.GetTriangleQuantity();
        for (int i = 0; i < iTQuantity; i++) {

            Vector3f kV0 = m_kMesh.VBuffer.GetPosition3( aiConnect[(3 * i) + 0] );
            Vector3f kV1 = m_kMesh.VBuffer.GetPosition3( aiConnect[(3 * i) + 1] );
            Vector3f kV2 = m_kMesh.VBuffer.GetPosition3( aiConnect[(3 * i) + 2] );
            kV1.sub(kV0, kE1);
            kV2.sub(kV0, kE2);
            kE1.Cross(kE2, kNormal);
            m_fSurfaceArea += 0.5f * kNormal.Length();
        }
    }

    /**
     * DOCUMENT ME!
     */
    public void disposeLocal() {
        int iVQuantity = m_kMesh.VBuffer.GetVertexQuantity();
        for (int i = 0; i < iVQuantity; i++) {

            if (m_akPlane != null) {
                m_akPlane[i] = null;
            }

            if (m_akSphere != null) {
                m_akSphere[i] = null;
            }

            if (m_akCylinder != null) {
                m_akCylinder[i] = null;
            }
        }
        if (m_akPlane != null) {
            m_akPlane = null;
        }

        if (m_akSphere != null) {
            m_akSphere = null;
        }

        if (m_akCylinder != null) {
            m_akCylinder = null;
        }

        if (m_akVertex != null) {

            for (int i = 0; i < m_akVertex.length; i++) {
                m_akVertex[i] = null;
            }

            m_akVertex = null;
        }

        if (m_akEdge != null) {

            for (int i = 0; i < m_akEdge.length; i++) {
                m_akEdge[i] = null;
            }

            m_akEdge = null;
        }

        if (m_akTriangle != null) {

            for (int i = 0; i < m_akTriangle.length; i++) {
                m_akTriangle[i] = null;
            }

            m_akTriangle = null;
        }
    }

    /**
     * surface inflation operation 0 = initialize 1 = inflate 2 = terminate.
     *
     * @param   iOperation  DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    public float doInflation(int iOperation) {
        float fError = 0.0f;
        int iVQuantity = m_kMesh.VBuffer.GetVertexQuantity();
        switch (iOperation) {
            
            case 0:
                m_afAvrConvexity = new float[iVQuantity];
                for (int i = 0; i < iVQuantity; i++) {
                    m_afAvrConvexity[i] = 0.0f;
                }

                computeDistances(1);
                m_kInitDistance = m_kDistance;
                computeNormals();
                fError = doInflationStep();
                break;

            case 1:
                fError = doInflationStep();
                break;

            case 2:
                if (null != m_afAvrConvexity) {
                    m_fMinAvrConvexity = m_afAvrConvexity[0];
                    m_fMaxAvrConvexity = m_fMinAvrConvexity;

                    for (int i = 1; i < iVQuantity; i++) {

                        if (m_afAvrConvexity[i] < m_fMinAvrConvexity) {
                            m_fMinAvrConvexity = m_afAvrConvexity[i];
                        } else if (m_afAvrConvexity[i] > m_fMaxAvrConvexity) {
                            m_fMaxAvrConvexity = m_afAvrConvexity[i];
                        }
                    }
                }

                fError = 0.0f;
                break;

            default:
                assert (false);
                break;
        }

        return fError;
    }

    /**
     * DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    public float[] getAvrConvexity() {
        return m_afAvrConvexity;
    }

    /**
     * DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    public TriMesh getCylinder() {
        if ( m_kCylinderMesh == null )
        {
            Attributes kAttr = new Attributes();
            kAttr.SetPChannels( 3 );
            kAttr.SetCChannels( 0, 4 );
            kAttr.SetNChannels( 3 );
            int iVQuantity = m_akCylinder.length;
            VertexBuffer kVBuffer = new VertexBuffer( kAttr, iVQuantity );
            for ( int i = 0; i < iVQuantity; i++ )
            {
                kVBuffer.SetPosition3( i, m_akCylinder[i] );
                kVBuffer.SetColor4( 0, i, m_kMesh.VBuffer.GetColor4(0, i) );
            }
            IndexBuffer kIBuffer = new IndexBuffer( m_kMesh.IBuffer );
            m_kCylinderMesh = new TriMesh( kVBuffer, kIBuffer );
            m_kCylinderMesh.SetName( m_kMesh.GetName() + "Cylinder" );
        }
        return m_kCylinderMesh;
    }
    

    /**
     * DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    public Vector3f[] getCylinderCoordinates() {
        return m_akCylinder;
    }

    /**
     * Vertex-vertex distance measured along the shortest edge-path connecting the two vertices.
     *
     * @param   iV0  DOCUMENT ME!
     * @param   iV1  DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    public float getDistance(int iV0, int iV1) {
        Float kDistance = (Float) m_kDistance.get(new EdgeKey(iV0, iV1));

        if (null != kDistance) {
            return kDistance.floatValue();
        }

        /* vertices not within each others neighborhood; */
        /* return invalid distance */
        return -1.0f;
    }

    /**
     * access to the distance map - stored in map from MjEdgeKey to Float.
     *
     * @return  DOCUMENT ME!
     */
    public HashMap getDistanceMap() {
        return m_kDistance;
    }

    /**
     * DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    public Edge[] getEdges() {
        return m_akEdge;
    }

    /**
     * DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    public int getEQuantity() {
        return m_iEQuantity;
    }

    /**
     * Produce polylines that are superimposed on the input cortical mesh, sphere, and plane. The znormal value
     * indicates the z-slice on the unit sphere that is mapped to the polyline on the mesh. This value is in (-1,1). The
     * angle value indicates the theta-slice on the unit sphere, theta measured as a positive angle counterclockwise
     * from the x-axis towards the y-axis. This value is in [0,2*pi]. The bias value adds a small amount to the polyline
     * to raise it above the mesh surface to avoid z-buffer fighting.
     *
     * @param   fZNormal  DOCUMENT ME!
     * @param   fMBias    DOCUMENT ME!
     * @param   fSBias    DOCUMENT ME!
     * @param   fPBias    DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    public Polylines getLatitude(float fZNormal, float fMBias, float fSBias, float fPBias) {

        /* Latitude circle in complex plane.  The radius is chosen so that the */
        /* latitude circles are uniformly spaced on the stereographic sphere in */
        /* the z-direction. */
        Circle2f kLatitude = new Circle2f();
        kLatitude.Center.SetData(Vector2f.ZERO);
        kLatitude.Radius = (float) Math.sqrt((1.0f + fZNormal) / (1.0f - fZNormal)) * getStereographicRadius();

        /* sorted t-values for intersections of circle */
        /* with triangle mesh edges */
        TreeMap<Float, Vector3f> kIntrMesh = new TreeMap<Float, Vector3f>(); /* map<float,Point3f> */
        TreeMap<Float, Vector3f> kIntrSphere = new TreeMap<Float, Vector3f>(); /* map<float,Point3f> */

        Segment2f kEdge = new Segment2f();
        Vector3f kU1mU0 = new Vector3f();
        Vector3f kU2mU0 = new Vector3f();
        Vector3f kN0 = new Vector3f();
        Vector3f kN1 = new Vector3f();
        Vector3f kNAvr = new Vector3f();
        Vector3f kNormal = new Vector3f();
        Vector2f kDiff = new Vector2f();

        int[] aiConnect = m_kMesh.IBuffer.GetData();
        for (int i = 0; i < m_iEQuantity; i++) {

            /* line segment representing the edge */
            int iV0 = m_akEdge[i].V[0];
            int iV1 = m_akEdge[i].V[1];
            Vector2f kV0 = m_akPlane[iV0];
            Vector2f kV1 = m_akPlane[iV1];
            kEdge.Origin.SetData(kV0);
            kV1.sub(kV0, kEdge.Direction);

            /* compute intersection of ray and segment */
            Vector2f[] akP = new Vector2f[2];
            int iCount = IntrSegment2Circle2f.FindIntersection(kEdge, kLatitude, akP);

            if (iCount > 0) {

                /* get normal for sharing triangle */
                int iTriangle = m_akEdge[i].T[0];
                Vector3f kU0 = m_kMesh.VBuffer.GetPosition3(aiConnect[(3 * iTriangle) + 0]);
                Vector3f kU1 = m_kMesh.VBuffer.GetPosition3(aiConnect[(3 * iTriangle) + 1]);
                Vector3f kU2 = m_kMesh.VBuffer.GetPosition3(aiConnect[(3 * iTriangle) + 2]);
                kU1.sub(kU0, kU1mU0);
                kU2.sub(kU0, kU2mU0);
                kU1mU0.Cross(kU2mU0, kN0);
                kN0.Normalize();

                /* get normal for sharing triangle */
                iTriangle = m_akEdge[i].T[1];
                kU0 = m_kMesh.VBuffer.GetPosition3(aiConnect[(3 * iTriangle) + 0]);
                kU1 = m_kMesh.VBuffer.GetPosition3(aiConnect[(3 * iTriangle) + 1]);
                kU2 = m_kMesh.VBuffer.GetPosition3(aiConnect[(3 * iTriangle) + 2]);
                kU1.sub(kU0, kU1mU0);
                kU2.sub(kU0, kU2mU0);
                kU1mU0.Cross(kU2mU0, kN1);
                kN1.Normalize();

                /* average normal */
                kN0.add(kN1, kNAvr);
                kNAvr.Normalize();


                Vector3f kPos0 = new Vector3f();
                Vector3f kPos1 = new Vector3f();
                Vector3f kPos2= new Vector3f();
                for (int j = 0; j < iCount; j++) {

                    /* Use the angle formed by the intersection point with the */
                    /* positive x-axis as the map key for sorting. */
                    float fAngle = (float) Math.atan2(akP[j].Y(), akP[j].X());

                    /* determine the edge parameter at the intersection */
                    akP[j].sub(kV0, kDiff);

                    float fNumer = kDiff.Dot(kEdge.Direction);
                    float fDenom = kEdge.Direction.SquaredLength();
                    float fS = fNumer / fDenom;

                    m_kMesh.VBuffer.GetPosition3( iV0, kPos0 );
                    m_kMesh.VBuffer.GetPosition3( iV1, kPos1 );

                    /* construct the edge point on the original mesh */
                    Vector3f kQ = new Vector3f();
                    kPos1.sub(kPos0, kQ);
                    kQ.scaleEquals(fS);
                    kQ.addEquals(kPos0);

                    /* To avoid z-buffer biasing problems, lift the point in the */
                    /* direction of the average of the normals for the triangles */
                    /* sharing the edge. */
                    kQ.addEquals(kNAvr.scale(fMBias));

                    /* save the point in an ordered map */
                    kIntrMesh.put(new Float(fAngle), kQ);

                    /* construct the edge point on the sphere */
                    kQ = new Vector3f();
                    m_akSphere[iV1].sub(m_akSphere[iV0], kQ);
                    kQ.scaleEquals(fS);
                    kQ.addEquals(m_akSphere[iV0]);

                    /* To avoid z-buffer biasing problems, lift the point in the */
                    /* direction of sphere normal. */
                    kNormal.SetData(kQ);
                    kNormal.Normalize();
                    kQ.SetData(kNormal.scale(fSBias));
                    kQ.addEquals(kNormal);

                    /* save the point in an ordered map */
                    kIntrSphere.put(new Float(fAngle), kQ);
                }
            }
        }

        Polylines kPolylines = new Polylines();
        assert (kIntrMesh.size() > 0);
        kPolylines.akMVertex = new Vector3f[kIntrMesh.size()];
        kPolylines.akSVertex = new Vector3f[kIntrSphere.size()];

        Iterator kIM = kIntrMesh.entrySet().iterator();
        Iterator kIS = kIntrSphere.entrySet().iterator();

        for (int i = 0; kIM.hasNext(); i++) {
            kPolylines.akMVertex[i] = (Vector3f) (((Map.Entry) kIM.next()).getValue());
            kPolylines.akSVertex[i] = (Vector3f) (((Map.Entry) kIS.next()).getValue());
        }

        kPolylines.akPVertex = new Vector3f[2];
        kPolylines.akPVertex[0] = new Vector3f(+(float) Math.PI, fZNormal, -fPBias);
        kPolylines.akPVertex[1] = new Vector3f(-(float) Math.PI, fZNormal, -fPBias);

        return kPolylines;
    }

    /**
     * DOCUMENT ME!
     *
     * @param   fAngle  DOCUMENT ME!
     * @param   fMBias  DOCUMENT ME!
     * @param   fSBias  DOCUMENT ME!
     * @param   fPBias  DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    public Polylines getLongitude(float fAngle, float fMBias, float fSBias, float fPBias) {
        int[] aiConnect = m_kMesh.IBuffer.GetData();
        
        final Vector2f kPoint2fZero = new Vector2f(0.0f, 0.0f);

        /* longitude ray in the complex plane */
        Ray2f kLongitude = new Ray2f();
        kLongitude.Origin.SetData(kPoint2fZero);
        kLongitude.Direction.X( (float) Math.cos(fAngle) );
        kLongitude.Direction.Y( (float) Math.sin(fAngle) );

        /* sorted t-values for intersections of ray with edges */
        TreeMap<Float, Vector3f> kIntrMesh = new TreeMap<Float, Vector3f>(); /* map<float,Point3f> */
        TreeMap<Float, Vector3f> kIntrSphere = new TreeMap<Float, Vector3f>(); /* map<float,Point3f> */

        /* ***** add the ray origin to the polyline ***** */
        int iContain = getContainingTriangle(kPoint2fZero, 0);
        assert (iContain > 0);

        Vector3f kBary = getBarycentric(kPoint2fZero, iContain);
        int iV0 = aiConnect[(3 * iContain) + 0];
        int iV1 = aiConnect[(3 * iContain) + 1];
        int iV2 = aiConnect[(3 * iContain) + 2];
        Vector3f kU1mU0 = new Vector3f();
        Vector3f kU2mU0 = new Vector3f();
        Vector3f kNormal = new Vector3f();

        /* get normal for containing triangle */
        Vector3f kU0 = m_kMesh.VBuffer.GetPosition3(iV0);
        Vector3f kU1 = m_kMesh.VBuffer.GetPosition3(iV1);
        Vector3f kU2 = m_kMesh.VBuffer.GetPosition3(iV2);
        kU1.sub(kU0, kU1mU0);
        kU2.sub(kU0, kU2mU0);
        kU1mU0.Cross(kU2mU0, kNormal);
        kNormal.Normalize();

        /* compute point on original mesh */
        Vector3f kQ = new Vector3f();
        kQ.addEquals( kU0.scale(kBary.X()));
        kQ.addEquals( kU1.scale(kBary.Y()));
        kQ.addEquals( kU2.scale(kBary.Z()));

        /* lift slightly off the surface */
        kQ.addEquals( kNormal.scale(fMBias));
        kIntrMesh.put(new Float(0.0f), kQ);

        /* repeat calculations for sphere */
        kU0 = m_akSphere[iV0];
        kU1 = m_akSphere[iV1];
        kU2 = m_akSphere[iV2];
        kQ = new Vector3f();
        kQ.addEquals( kU0.scale(kBary.X()));
        kQ.addEquals( kU1.scale(kBary.Y()));
        kQ.addEquals( kU2.scale(kBary.Z()));
        kNormal.SetData(kQ);
        kNormal.Normalize();
        kQ.addEquals( kNormal.scale(fMBias));
        kIntrSphere.put(new Float(0.0f), kQ);
        /* ***** end add ray origin ***** */

        /* ***** add the ray infinity to the polyline ***** */
        iContain = 0; /* TO DO: if puncture triangle is changed, change this */

        /* get normal for containing triangle */
        m_kMesh.VBuffer.GetPosition3( iV0, kU0 );
        m_kMesh.VBuffer.GetPosition3( iV1, kU1 );
        m_kMesh.VBuffer.GetPosition3( iV2, kU2 );
        kU1.sub(kU0, kU1mU0);
        kU2.sub(kU0, kU2mU0);
        kU1mU0.Cross(kU2mU0, kNormal);
        kNormal.Normalize();

        /* compute point on original mesh */
        kQ = new Vector3f();
        kQ.addEquals(kU0);
        kQ.addEquals(kU1);
        kQ.addEquals(kU2);
        kQ.scaleEquals(1.0f / 3.0f);

        /* lift slightly off the surface */
        kQ.addEquals( kNormal.scale(fMBias));
        kIntrMesh.put(new Float(Float.MAX_VALUE), kQ);

        /* repeat calculations for sphere */
        kU0 = m_akSphere[iV0];
        kU1 = m_akSphere[iV1];
        kU2 = m_akSphere[iV2];
        kQ = new Vector3f();
        kQ.addEquals(kU0);
        kQ.addEquals(kU1);
        kQ.addEquals(kU2);
        kQ.scaleEquals(1.0f / 3.0f);
        kNormal.SetData(kQ);
        kNormal.Normalize();
        kQ.addEquals( kNormal.scale(fSBias));
        kIntrSphere.put(new Float(Float.MAX_VALUE), kQ);
        /* ***** end add ray origin ***** */

        float[] afT = new float[2];
        Vector3f kNormal0 = new Vector3f();
        Vector3f kNormal1 = new Vector3f();
        Vector2f kDiff = new Vector2f();

        for (int i = 0; i < m_iEQuantity; i++) {

            /* line segment representing the edge */
            iV0 = m_akEdge[i].V[0];
            iV1 = m_akEdge[i].V[1];

            Vector2f kV0 = m_akPlane[iV0];
            Vector2f kV1 = m_akPlane[iV1];
            Segment2f kEdge = new Segment2f();
            kEdge.Origin.SetData(kV0);
            kV1.sub(kV0, kEdge.Direction);

            /* compute intersection of ray and segment */
            int iCount = IntrRay2Segment2f.FindIntersection(kLongitude, kEdge, afT);

            if (iCount > 0) {

                /* get normal for sharing triangle */
                int iTriangle = m_akEdge[i].T[0];
                m_kMesh.VBuffer.GetPosition3( aiConnect[(3 * iTriangle) + 0], kU0 );
                m_kMesh.VBuffer.GetPosition3( aiConnect[(3 * iTriangle) + 1], kU1 );
                m_kMesh.VBuffer.GetPosition3( aiConnect[(3 * iTriangle) + 2], kU2 );
                kU1.sub(kU0, kU1mU0);
                kU2.sub(kU0, kU2mU0);
                kU1mU0.Cross(kU2mU0, kNormal0);
                kNormal0.Normalize();

                /* get normal for sharing triangle */
                iTriangle = m_akEdge[i].T[1];
                m_kMesh.VBuffer.GetPosition3( aiConnect[(3 * iTriangle) + 0], kU0 );
                m_kMesh.VBuffer.GetPosition3( aiConnect[(3 * iTriangle) + 1], kU1 );
                m_kMesh.VBuffer.GetPosition3( aiConnect[(3 * iTriangle) + 2], kU2 );
                kU1.sub(kU0, kU1mU0);
                kU2.sub(kU0, kU2mU0);
                kU1mU0.Cross(kU2mU0, kNormal1);
                kNormal1.Normalize();

                kNormal0.add(kNormal1, kNormal);
                kNormal.Normalize();

                Vector3f kPos0 = new Vector3f();
                Vector3f kPos1 = new Vector3f();
                for (int j = 0; j < iCount; j++) {

                    /* afT[j] is the ray parameter, need to compute the edge */
                    /* parameter */
                    kDiff.SetData(kLongitude.Direction);
                    kDiff.scaleEquals(afT[j]);
                    kDiff.subEquals(kV0);

                    float fNumer = kDiff.Dot(kEdge.Direction);
                    float fDenom = kEdge.Direction.SquaredLength();
                    float fS = fNumer / fDenom;

                    m_kMesh.VBuffer.GetPosition3( iV0, kPos0 );
                    m_kMesh.VBuffer.GetPosition3( iV1, kPos1 );

                    /* construct the edge point on the original mesh */
                    kQ = new Vector3f();
                    kPos1.sub(kPos0, kQ);
                    kQ.scaleEquals(fS);
                    kQ.addEquals(kPos0);

                    /* To avoid z-buffer biasing problems, lift the point in the */
                    /* direction of the average of the normals for the triangles */
                    /* sharing the edge. */
                    kQ.addEquals( kNormal.scale(fMBias));

                    kIntrMesh.put(new Float(afT[j]), kQ);

                    /* repeat the construction for sphere */
                    kQ = new Vector3f();
                    m_akSphere[iV1].sub(m_akSphere[iV0], kQ);
                    kQ.scaleEquals(fS);
                    kQ.addEquals(m_akSphere[iV0]);
                    kNormal.SetData(kQ);
                    kNormal.Normalize();
                    kQ.addEquals( kNormal.scale(fSBias));
                    kIntrSphere.put(new Float(afT[j]), kQ);
                }
            }
        }

        Polylines kPolylines = new Polylines();
        assert (kIntrMesh.size() > 0);
        kPolylines.akMVertex = new Vector3f[kIntrMesh.size()];
        kPolylines.akSVertex = new Vector3f[kIntrSphere.size()];

        Iterator kIM = kIntrMesh.entrySet().iterator();
        Iterator kIS = kIntrSphere.entrySet().iterator();

        for (int i = 0; kIM.hasNext(); i++) {
            kPolylines.akMVertex[i] = (Vector3f) (((Map.Entry) kIM.next()).getValue());
            kPolylines.akSVertex[i] = (Vector3f) (((Map.Entry) kIS.next()).getValue());
        }

        fAngle -= Math.PI;
        kPolylines.akPVertex = new Vector3f[2];
        kPolylines.akPVertex[0] = new Vector3f(fAngle, -1.0f, -fPBias);
        kPolylines.akPVertex[1] = new Vector3f(fAngle, +1.0f, -fPBias);

        return kPolylines;
    }

    /**
     * DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    public float getMaxAvrConvexity() {
        return m_fMaxAvrConvexity;
    }

    /**
     * DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    public float getMaxDistance() {
        return m_fMaxDistance;
    }

    /**
     * DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    public float getMaxMeanCurvature() {
        return m_fMaxMeanCurvature;
    }

    /**
     * DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    public float[] getMeanCurvature() {
        return m_afMeanCurvature;
    }

    /**
     * DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    public float getMinAvrConvexity() {
        return m_fMinAvrConvexity;
    }

    /**
     * DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    public float getMinDistance() {
        return m_fMinDistance;
    }

    /**
     * DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    public float getMinMeanCurvature() {
        return m_fMinMeanCurvature;
    }

    /**
     * DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    public Vector2f[] getPlaneCoordinates() {
        return m_akPlane;
    }

    /**
     * DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    public Vector2f getPlaneMax() {
        return m_kPlaneMax;
    }

    /**
     * DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    public Vector2f getPlaneMin() {
        return m_kPlaneMin;
    }

    /**
     * DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    public TriMesh getSphere() {
        if ( m_kSphereMesh == null )
        {
            Attributes kAttr = new Attributes();
            kAttr.SetPChannels( 3 );
            kAttr.SetCChannels( 0, 4 );
            kAttr.SetNChannels( 3 );
            int iVQuantity = m_akSphere.length;
            VertexBuffer kVBuffer = new VertexBuffer( kAttr, iVQuantity );
            for ( int i = 0; i < iVQuantity; i++ )
            {
                kVBuffer.SetPosition3( i, m_akSphere[i] );
                kVBuffer.SetColor4( 0, i, m_kMesh.VBuffer.GetColor4(0, i) );
            }
            IndexBuffer kIBuffer = new IndexBuffer( m_kMesh.IBuffer );
            m_kSphereMesh = new TriMesh( kVBuffer, kIBuffer );
            m_kSphereMesh.SetName( m_kMesh.GetName() + "Sphere" );
        }
        return m_kSphereMesh;
    }

    /**
     * DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    public float getSurfaceArea() {
        return m_fSurfaceArea;
    }

    /**
     * DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    public Triangle[] getTriangles() {
        return m_akTriangle;
    }

    /**
     * DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    public Vertex[] getVertices() {
        return m_akVertex;
    }


    public void setColor( int i, float fRed, float fGreen, float fBlue )
    {
        m_kMesh.VBuffer.SetColor4( 0, i, fRed, fGreen, fBlue, 1.0f );
    }

    /**
     * uniformly scale points to [-1,1]^3, originally in [min,max]^3 returns 2D vector with (min,max) as the elements
     * public Vector2f scaleToCube ().
     *
     * @return  DOCUMENT ME!
     */
    public float scaleToCube() {
        float fMin = m_kMesh.VBuffer.GetPosition3fX( 0 );
        float fMax = fMin;
        int iVQuantity = m_kMesh.VBuffer.GetVertexQuantity();
        for (int i = 0; i < iVQuantity; i++) {

            if ( m_kMesh.VBuffer.GetPosition3fX( i ) < fMin) {
                fMin = m_kMesh.VBuffer.GetPosition3fX( i );
            } else if (m_kMesh.VBuffer.GetPosition3fX( i ) > fMax) {
                fMax = m_kMesh.VBuffer.GetPosition3fX( i );
            }

            if (m_kMesh.VBuffer.GetPosition3fY( i ) < fMin) {
                fMin = m_kMesh.VBuffer.GetPosition3fY( i );
            } else if (m_kMesh.VBuffer.GetPosition3fY( i ) > fMax) {
                fMax = m_kMesh.VBuffer.GetPosition3fY( i );
            }

            if (m_kMesh.VBuffer.GetPosition3fZ( i ) < fMin) {
                fMin = m_kMesh.VBuffer.GetPosition3fZ( i );
            } else if (m_kMesh.VBuffer.GetPosition3fZ( i ) > fMax) {
                fMax = m_kMesh.VBuffer.GetPosition3fZ( i );
            }
        }

        float fHalfRange = 0.5f * (fMax - fMin);
        float fInvHalfRange = 1.0f / fHalfRange;
        Vector3f kOne = new Vector3f(1.0f, 1.0f, 1.0f);

        Vector3f kPos = new Vector3f();
        for (int i = 0; i < iVQuantity; i++) {
            m_kMesh.VBuffer.GetPosition3( i, kPos );
            kPos.addEquals( kOne.scale(-fMin));
            kPos.scaleEquals(fInvHalfRange);
            kPos.subEquals(kOne);
            m_kMesh.VBuffer.SetPosition3( i, kPos );
        }

        /*        return new Vector2f(fMin,fMax); */
        return (float) (1.0 / fInvHalfRange);
    }

    /**
     * Set the index of the triangle used to "puncture" the mesh before the conformal mapping is computed. The triangle
     * is found based on the three index values that describe the triangle:
     *
     * @param  aiIndex  DOCUMENT ME!
     */
    public void setPunctureTriangle(int[] aiIndex) {
        m_iPunctureTri = 0;

        /* If aiIndex is not set, use the default value of 0 for the puncture
         * triangle */
        if (aiIndex == null) {
            return;
        }

        /* Search through the triangle list, find the triangle that matches
         * the aiIndex input: */
        int iTQuantity = m_kMesh.GetTriangleQuantity();
        for (int i = 0; i < iTQuantity; i++) {
            boolean bfound = true;

            for (int j = 0; j < aiIndex.length; j++) {

                if (m_akTriangle[i].V[j] != aiIndex[j]) {
                    bfound = false;

                    break;
                }
            }

            if (bfound) {
                m_iPunctureTri = i;
            }
        }
    }

    public void updateMesh()
    {
        m_kMesh.VBuffer.Release();
    }
    
    /**
     * DOCUMENT ME!
     *
     * @throws  Throwable  DOCUMENT ME!
     */
    protected void finalize() throws Throwable {
        disposeLocal();
    }

    /**
     * support for point-in-triangle tests; The akVertex array must have length 3.
     *
     * @param   akVertex  DOCUMENT ME!
     * @param   kP        DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    private static boolean contains(Vector2f[] akVertex, Vector2f kP) {
        /* assert:  <V0,V1,V2> is clockwise ordered */

        final float fEpsilon = 1e-05f;
        Vector2f kV1mV0 = new Vector2f();
        Vector2f kInnerNormal = new Vector2f();
        Vector2f kDiff = new Vector2f();

        for (int i0 = 2, i1 = 0; i1 < 3; i0 = i1++) {
            akVertex[i1].sub(akVertex[i0], kV1mV0);
            kInnerNormal.Perp(kV1mV0);
            kP.sub(akVertex[i0], kDiff);
            kInnerNormal.Normalize();
            kDiff.Normalize();

            float fCos = kInnerNormal.Dot(kDiff);

            if (fCos < -fEpsilon) {
                return false;
            }
        }

        return true;
    }

    /**
     * compute distances within specified neighborhood size (size >= 1).
     *
     * @param  iSize       DOCUMENT ME!
     * @param  iSource     DOCUMENT ME!
     * @param  bInitiator  DOCUMENT ME!
     */
    private void computeDistance(int iSize, int iSource, boolean bInitiator) {

        if (bInitiator) {
            m_fMinDistance = -1.0f;
            m_fMaxDistance = -1.0f;
            m_kDistance.clear();
        }

        /* start distance calculations from source vertex */
        Set<Integer> kInterior = new HashSet<Integer>(); /* set of int */
        kInterior.add(new Integer(iSource));

        /* compute distances to all 1-neighbors */
        Vertex kSource = m_akVertex[iSource];
        int i, iNbr;
        Set<Integer> kBoundary = new HashSet<Integer>(); /* set of int */

        for (i = 0; i < kSource.VQuantity; i++) {
            iNbr = kSource.V[i];
            kBoundary.add(new Integer(iNbr));

            EdgeKey kKey = new EdgeKey(iSource, iNbr);

            if (!m_kDistance.containsKey(kKey)) {
                float fDist = m_kMesh.VBuffer.GetPosition3(iNbr).sub(m_kMesh.VBuffer.GetPosition3(iSource)).Length();
                m_kDistance.put(kKey, new Float(fDist));
            }
        }

        /* Compute distances to farther neighbors using Dijkstra's algorithm. */
        /* TO DO.  A set of boundary points is used to support the breadth-first */
        /* search.  If the neighborhoods are very large and performance is an */
        /* issue, the sets should be replaced by a priority queue. */

        for (int j = 2; j <= iSize; j++) {
            Set<Integer> kExterior = new HashSet<Integer>(); /* set of int */

            Iterator kIter = kBoundary.iterator();

            while (kIter.hasNext()) {

                /* current boundary point to process */
                int iCenter = ((Integer) (kIter.next())).intValue();
                Vertex kCenter = m_akVertex[iCenter];

                /* distance of path <source,center> */
                float fPathLength = ((Float) (m_kDistance.get(new EdgeKey(iSource, iCenter)))).floatValue();

                /* search immediate, exterior neighbors */
                for (i = 0; i < kCenter.VQuantity; i++) {
                    iNbr = kCenter.V[i];

                    Integer kNbr = new Integer(iNbr);

                    if (!kInterior.contains(kNbr) && !kBoundary.contains(kNbr)) {

                        /* get distance of edge <center,nbr> */
                        Float kDist = (Float) m_kDistance.get(new EdgeKey(iCenter, iNbr));
                        float fDist = (null != kDist) ? kDist.floatValue()
                                                      : m_kMesh.VBuffer.GetPosition3(iNbr).sub(m_kMesh.VBuffer.GetPosition3(iCenter)).Length();

                        /* get distance of <source,nbr> along current path */
                        float fTotalLength = fPathLength + fDist;

                        /* update current distance to shorter one */
                        /* (if necessary) */
                        EdgeKey kKey = new EdgeKey(iSource, iNbr);
                        kDist = (Float) m_kDistance.get(kKey);

                        if (null != kDist) {

                            if (fTotalLength < kDist.floatValue()) {
                                m_kDistance.put(kKey, new Float(fTotalLength));
                            }
                        } else {
                            m_kDistance.put(kKey, new Float(fTotalLength));
                        }

                        kExterior.add(new Integer(iNbr));
                    }
                }
            }

            /* boundary points processed, move to interior */
            kInterior.addAll(kBoundary);

            /* exterior points are next in line to be processed */
            kBoundary = kExterior;
        }

        if (bInitiator) {
            computeExtremes();
        }
    }

    /**
     * DOCUMENT ME!
     */
    private void computeExtremes() {
        m_fMinDistance = Float.MAX_VALUE;
        m_fMaxDistance = 0.0f;

        Iterator kIter = m_kDistance.entrySet().iterator();

        while (kIter.hasNext()) {
            Map.Entry kMapEntry = (Map.Entry) (kIter.next());
            float fValue = ((Float) kMapEntry.getValue()).floatValue();

            if (fValue < m_fMinDistance) {
                m_fMinDistance = fValue;
            }

            if (fValue > m_fMaxDistance) {
                m_fMaxDistance = fValue;
            }
        }
    }

    /**
     * DOCUMENT ME!
     *
     * @param   kV0            DOCUMENT ME!
     * @param   kV1            DOCUMENT ME!
     * @param   kV2            DOCUMENT ME!
     * @param   fAreaFraction  DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    private float computeRadius(Vector2f kV0, Vector2f kV1, Vector2f kV2, float fAreaFraction) {
        float fR0Sqr = kV0.SquaredLength();
        float fR1Sqr = kV1.SquaredLength();
        float fR2Sqr = kV2.SquaredLength();
        float fDR10 = fR1Sqr - fR0Sqr;
        float fDR20 = fR2Sqr - fR0Sqr;
        float fDX10 = kV1.X() - kV0.X();
        float fDY10 = kV1.Y() - kV0.Y();
        float fDX20 = kV2.X() - kV0.X();
        float fDY20 = kV2.Y() - kV0.Y();
        float fDRX10 = (kV1.X() * fR0Sqr) - (kV0.X() * fR1Sqr);
        float fDRY10 = (kV1.Y() * fR0Sqr) - (kV0.Y() * fR1Sqr);
        float fDRX20 = (kV2.X() * fR0Sqr) - (kV0.X() * fR2Sqr);
        float fDRY20 = (kV2.Y() * fR0Sqr) - (kV0.Y() * fR2Sqr);

        float fC0 = (fDR20 * fDRY10) - (fDR10 * fDRY20);
        float fC1 = (fDR20 * fDY10) - (fDR10 * fDY20);
        float fD0 = (fDR10 * fDRX20) - (fDR20 * fDRX10);
        float fD1 = (fDR10 * fDX20) - (fDR20 * fDX10);
        float fE0 = (fDRX10 * fDRY20) - (fDRX20 * fDRY10);
        float fE1 = (fDRX10 * fDY20) - (fDRX20 * fDY10);
        float fE2 = (fDX10 * fDY20) - (fDX20 * fDY10);

        Polynomial1f kP0 = new Polynomial1f(6);
        kP0.setCoeff(0, 0.0f);
        kP0.setCoeff(1, 0.0f);
        kP0.setCoeff(2, fE0 * fE0);
        kP0.setCoeff(3, (fC0 * fC0) + (fD0 * fD0) + (2.0f * fE0 * fE1));
        kP0.setCoeff(4, (2.0f * ((fC0 * fC1) + (fD0 * fD1) + (fE0 * fE1))) + (fE1 * fE1));
        kP0.setCoeff(5, (fC1 * fC1) + (fD1 * fD1) + (2.0f * fE1 * fE2));
        kP0.setCoeff(6, fE2 * fE2);

        Polynomial1f kQ0 = new Polynomial1f(1);
        kQ0.setCoeff(0, fR0Sqr);
        kQ0.setCoeff(1, 1.0f);

        Polynomial1f kQ1 = new Polynomial1f(1);
        kQ1.setCoeff(0, fR1Sqr);
        kQ1.setCoeff(1, 1.0f);

        Polynomial1f kQ2 = new Polynomial1f(1);
        kQ2.setCoeff(0, fR2Sqr);
        kQ2.setCoeff(1, 1.0f);

        float fTmp = fAreaFraction * (float) Math.PI;
        float fAmp = fTmp * fTmp;
        Polynomial1f kP1 = new Polynomial1f();
        kP1.set(kQ0);
        kP1.scale(fAmp);
        kP1.mul(kP1, kQ0);
        kP1.mul(kP1, kQ0);
        kP1.mul(kP1, kQ0);
        kP1.mul(kP1, kQ1);
        kP1.mul(kP1, kQ1);
        kP1.mul(kP1, kQ2);
        kP1.mul(kP1, kQ2);

        Polynomial1f kFinal = new Polynomial1f();
        kFinal.sub(kP1, kP0);
        assert (kFinal.getDegree() <= 8);

        return kFinal.getRootBisection();
    }

    /**
     * surface inflation.
     *
     * @return  DOCUMENT ME!
     */
    private float doInflationStep() {
        int iVQuantity = m_kMesh.VBuffer.GetVertexQuantity();
        
        Vector3f kVDiff = new Vector3f();
        Vector3f kDDiff = new Vector3f();

        float fWeight = 0.1f;

        /* compute the gradient of the energy function */
        Vector3f[] akDJDX = new Vector3f[iVQuantity];
        Vector3f kSumSqr = new Vector3f(Vector3f.ZERO);
        float fError = 0.0f;

        Vector3f kPos0 = new Vector3f();
        Vector3f kPos1 = new Vector3f();
        for (int i0 = 0; i0 < iVQuantity; i0++) {
            akDJDX[i0] = new Vector3f(Vector3f.ZERO);

            Vertex kVertex = m_akVertex[i0];

            for (int j = 0; j < kVertex.VQuantity; j++) {
                int i1 = kVertex.V[j];

                m_kMesh.VBuffer.GetPosition3( i0, kPos0 );
                m_kMesh.VBuffer.GetPosition3( i1, kPos1 );
                kPos0.sub(kPos1, kVDiff);

                float fC = kVDiff.SquaredLength();

                EdgeKey kKey = new EdgeKey(i0, i1);
                assert (m_kInitDistance.containsKey(kKey));

                Float kValue = (Float) m_kInitDistance.get(kKey);
                assert (null != kValue);

                float fValue = kValue.floatValue();
                float fTmp0 = fC - (fValue * fValue);
                float fTmp1 = 1.0f + (fWeight * fTmp0);

                fError += (2.0f * fC) + (fWeight * fTmp0 * fTmp0);
                akDJDX[i0].addEquals( kVDiff.scale(fTmp1));
            }

            kSumSqr.addEquals(akDJDX[i0]);

            /* update average convexity */
            m_afAvrConvexity[i0] += m_kMesh.VBuffer.GetNormal3(i0).Dot(akDJDX[i0]);
        }

        float fInvQuantity = 1.0f / iVQuantity;
        fError *= fInvQuantity;

        float fGradLength = kSumSqr.Length();

        if (fGradLength < Mathf.EPSILON) {
            return fError;
        }

        if (fGradLength > 1.0f) {
            float fInvGradLength = 1.0f / fGradLength;

            for (int i = 0; i < iVQuantity; i++) {
                akDJDX[i].scaleEquals(fInvGradLength);
            }
        }

        /* Compute the energy function along the line X+t*D where X is the */
        /* total vector of vertex positions and D is the normalized gradient */
        /* vector. */
        Polynomial1f kPoly = new Polynomial1f(4);
        kPoly.setCoeff(0, fError);

        for (int j = 1; j <= 4; j++) {
            kPoly.setCoeff(j, 0.0f);
        }

        for (int i0 = 0; i0 < iVQuantity; i0++) {
            Vertex kVertex = m_akVertex[i0];

            for (int j = 0; j < kVertex.VQuantity; j++) {
                int i1 = kVertex.V[j];

                m_kMesh.VBuffer.GetPosition3( i0, kPos0 );
                m_kMesh.VBuffer.GetPosition3( i1, kPos1 );
                kPos0.sub(kPos1, kVDiff);
                akDJDX[i0].sub(akDJDX[i1], kDDiff);

                float fA = kDDiff.SquaredLength();
                float fB = kDDiff.Dot(kVDiff);
                float fC = kVDiff.SquaredLength();

                EdgeKey kKey = new EdgeKey(i0, i1);
                assert (m_kInitDistance.containsKey(kKey));

                Float kValue = (Float) m_kInitDistance.get(kKey);
                assert (null != kValue);

                float fValue = kValue.floatValue();
                float fTmp0 = fC - (fValue * fValue);
                float fTmp1 = 1.0f + (fWeight * fTmp0);

                kPoly.setCoeff(1, kPoly.getCoeff(1) + (4.0f * fB * fTmp1));
                kPoly.setCoeff(2, kPoly.getCoeff(2) + (4.0f * fWeight * fB * fB) + (2.0f * fA * fTmp1));
                kPoly.setCoeff(3, kPoly.getCoeff(3) + (4.0f * fWeight * fA * fB));
                kPoly.setCoeff(4, kPoly.getCoeff(4) + (fWeight * fA * fA));
            }
        }

        for (int j = 1; j <= 4; j++) {
            kPoly.setCoeff(j, kPoly.getCoeff(j) * fInvQuantity);
        }

        Polynomial1f kDPoly = kPoly.getDerivative();

        float[] afRoot = new float[3];
        int iCount = kDPoly.rootsDegree3(afRoot);
        assert (iCount > 0);

        int iMin = -1;

        for (int i = 0; i < iCount; i++) {
            float fTest = kDPoly.eval(afRoot[i]);
            float fValue = kPoly.eval(afRoot[i]);
            assert (fValue >= 0.0f);

            if (fValue < fError) {
                fError = fValue;
                iMin = i;
            }
        }

        if (iMin >= 0) {
            float fH = afRoot[iMin];

            Vector3f kPos = new Vector3f();
            for (int i = 0; i < iVQuantity; i++) {
                m_kMesh.VBuffer.GetPosition3( i, kPos );
                kPos.addEquals( akDJDX[i].scale(fH));
                m_kMesh.VBuffer.SetPosition3( i, kPos );
            }
        }

        return fError;
    }

    /**
     * Support for conformal mapping to planar triangle mesh. Once the containing triangle i is found, compute the
     * barycentric coordinates of P relative to the triangle vertices V0, V1, and V2.
     *
     * @param   kP  DOCUMENT ME!
     * @param   i   DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    private Vector3f getBarycentric(Vector2f kP, int i) {

        /* get barycentric coordinates */
        Triangle kT = m_akTriangle[i];
        Vector2f kV0 = m_akPlane[kT.V[0]];
        Vector2f kV1 = m_akPlane[kT.V[1]];
        Vector2f kV2 = m_akPlane[kT.V[2]];
        Vector2f kE10 = new Vector2f();
        kV1.sub(kV0, kE10);

        Vector2f kE20 = new Vector2f();
        kV2.sub(kV0, kE20);

        Vector2f kDiff = new Vector2f();
        kP.sub(kV0, kDiff);

        float fA00 = kE10.Dot(kE10);
        float fA01 = kE10.Dot(kE20);
        float fA11 = kE20.Dot(kE20);
        float fInvDet = 1.0f / ((fA00 * fA11) - (fA01 * fA01));
        float fB0 = kE10.Dot(kDiff);
        float fB1 = kE20.Dot(kDiff);

        Vector3f kBary = new Vector3f();
        kBary.Y( ((fA11 * fB0) - (fA01 * fB1)) * fInvDet );
        kBary.Z( ((fA00 * fB1) - (fA01 * fB0)) * fInvDet );
        kBary.X( 1.0f - kBary.Y() - kBary.Z() );

        return kBary;
    }

    /**
     * Support for conformal mapping to planar triangle mesh. Use a linear walk to locate a triangle containing P. The
     * index of such a triangle is returned, unless there is no containing triangle in which case the return value is
     * -1. The "start" index is the suggested starting triangle. When testing a lot of points that are ordered in some
     * spatial manner, the chance that the triangle containing the previous test point will contain the next test point
     * (or a neighbor might contain it). Taking advantage of spatial coherence should speed up the search in "batch
     * mode".
     *
     * @param   kP      DOCUMENT ME!
     * @param   iStart  DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    private int getContainingTriangle(Vector2f kP, int iStart) {

        /* The search should never iterate over more than all the triangles... */
        Ray2f kRay = new Ray2f();
        Segment2f kSeg = new Segment2f();
        Vector2f[] akVertex = new Vector2f[3];

        int iTQuantity = m_kMesh.GetTriangleQuantity();
        for (int i = 0; i < iTQuantity; i++) {

            /* get vertices of current triangle */
            Triangle kT = m_akTriangle[iStart];
            akVertex[0] = m_akPlane[kT.V[0]];
            akVertex[1] = m_akPlane[kT.V[1]];
            akVertex[2] = m_akPlane[kT.V[2]];

            /* test if P is inside the triangle */
            if (contains(akVertex, kP)) {
                return iStart;
            }

            /* P is outside the triangle.  Search an adjacent triangle through a */
            /* shared edge that is intersected by C+t*D where C is the current */
            /* triangle center and D is the direction (P-C)/|P-C|. */
            kRay.Origin.SetData(0.0f, 0.0f);
            kRay.Origin.addEquals(akVertex[0]);
            kRay.Origin.addEquals(akVertex[1]);
            kRay.Origin.addEquals(akVertex[2]);
            kRay.Origin.scaleEquals(1.0f / 3.0f);
            kP.sub(kRay.Origin, kRay.Direction);
            kRay.Direction.Normalize();

            int i0, i1;

            for (i0 = 2, i1 = 0; i1 < 3; i0 = i1++) {
                kSeg.Origin.SetData(akVertex[i0]);
                akVertex[i1].sub(akVertex[i0], kSeg.Direction);

                float[] afT = new float[2]; /* T[0] = ray, T[1] = segment */
                int iQuantity = IntrRay2Segment2f.FindIntersection(kRay, kSeg, afT);

                /* intersects? */
                if (0 != iQuantity) {
                    Edge kE = m_akEdge[kT.E[i0]];
                    iStart = ((kE.T[0] == iStart) ? kE.T[1] : kE.T[0]);

                    break;
                }
            }

            assert (i1 < 3);
        }

        /* point not located in triangle mesh */
        return -1;
    }

    /**
     * support for unfolding.
     *
     * @return  DOCUMENT ME!
     */
    private float getStereographicRadius() {
        return m_fRho;
    }

    //~ Inner Classes --------------------------------------------------------------------------------------------------

    /**
     * DOCUMENT ME!
     */
    public static class Edge {

        /** DOCUMENT ME! */
        public final int[] T = new int[2];

        /** DOCUMENT ME! */
        public final int[] V = new int[2];

        /**
         * Creates a new Edge object.
         */
        public Edge() {

            for (int i = 0; i < 2; i++) {
                V[i] = -1;
                T[i] = -1;
            }
        }
    }

    /**
     * DOCUMENT ME!
     */
    public static class Polylines {

        /** Array of mesh points. */
        Vector3f[] akMVertex;

        /** Array of plane points. */
        Vector3f[] akPVertex;

        /** Array of sphere points. */
        Vector3f[] akSVertex;
    }

    /**
     * DOCUMENT ME!
     */
    public static class Triangle {

        /** DOCUMENT ME! */
        public final int[] E = new int[3];

        /** DOCUMENT ME! */
        public final int[] T = new int[3];

        /** DOCUMENT ME! */
        public final int[] V = new int[3];

        /**
         * Creates a new Triangle object.
         */
        public Triangle() {

            for (int i = 0; i < 3; i++) {
                V[i] = -1;
                E[i] = -1;
                T[i] = -1;
            }
        }
    }

    /**
     * DOCUMENT ME!
     */
    public static class Vertex {

        /** DOCUMENT ME! */
        private static final int MV_CHUNK = 8;

        /** DOCUMENT ME! */
        public int[] E;

        /** DOCUMENT ME! */
        public int[] T;

        /** DOCUMENT ME! */
        public int TQuantity;

        /** DOCUMENT ME! */
        public int[] V;

        /** DOCUMENT ME! */
        public int VQuantity;

        /**
         * Creates a new Vertex object.
         */
        public Vertex() {
            VQuantity = 0;
            V = new int[0];
            E = new int[0];
            TQuantity = 0;
            T = new int[0];
        }

        /**
         * DOCUMENT ME!
         *
         * @param  iV  DOCUMENT ME!
         * @param  iE  DOCUMENT ME!
         */
        public void InsertEdge(int iV, int iE) {

            /* check if vertex/edge in adjacency array */
            /* nothing to do if in array) */
            for (int i = 0; i < VQuantity; i++) {

                if (iV == V[i]) {
                    return;
                }
            }

            if ((VQuantity % MV_CHUNK) == 0) {
                int[] aiSave = V;
                V = new int[VQuantity + MV_CHUNK];
                System.arraycopy(aiSave, 0, V, 0, VQuantity);

                aiSave = E;
                E = new int[VQuantity + MV_CHUNK];
                System.arraycopy(aiSave, 0, E, 0, VQuantity);
            }

            V[VQuantity] = iV;
            E[VQuantity] = iE;
            VQuantity++;
        }

        /**
         * DOCUMENT ME!
         *
         * @param  iT  DOCUMENT ME!
         */
        public void InsertTriangle(int iT) {

            /* check if triangle in adjacency array */
            /* (nothing to do if in array) */
            for (int i = 0; i < TQuantity; i++) {

                if (iT == T[i]) {
                    return;
                }
            }

            if ((TQuantity % MV_CHUNK) == 0) {
                int[] aiSave = T;
                T = new int[TQuantity + MV_CHUNK];
                System.arraycopy(aiSave, 0, T, 0, TQuantity);
            }

            T[TQuantity] = iT;
            TQuantity++;
        }
    }
}
