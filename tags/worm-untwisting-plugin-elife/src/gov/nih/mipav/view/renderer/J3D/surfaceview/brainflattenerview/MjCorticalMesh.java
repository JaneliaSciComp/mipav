package gov.nih.mipav.view.renderer.J3D.surfaceview.brainflattenerview;


import java.util.*;

import javax.vecmath.*;


/**
 * DOCUMENT ME!
 */
public class MjCorticalMesh {

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** DOCUMENT ME! */
    private float[] m_afAvrConvexity;

    /** DOCUMENT ME! */
    private float[] m_afMeanCurvature;

    /** DOCUMENT ME! */
    private int[] m_aiConnect = null;

    /** DOCUMENT ME! */
    private Color4f[] m_akColor = null;

    /** projection of sphere onto cylinder. */
    private Point3f[] m_akCylinder = null;

    /** DOCUMENT ME! */
    private Edge[] m_akEdge = null;

    /** DOCUMENT ME! */
    private MjVector3f[] m_akNormal = null;

    /** Conformal mapping to a plane. The (u,v) points correspond to the (x,y,z) mesh points. */
    private Point2f[] m_akPlane = null;

    /** mesh attributes. */
    private Point3f[] m_akPoint = null;

    /** Conformal mapping to a sphere. rho is the radius of the stereographic sphere. */
    private Point3f[] m_akSphere = null;

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

    /** DOCUMENT ME! */
    private int m_iTQuantity;

    /** mesh topology. */
    private int m_iVQuantity;

    /** vertex-vertex distances measured along edge paths. */
    private HashMap m_kDistance = new HashMap(); /* map MjEdgeKey to Float */

    /** surface inflation. */
    private HashMap m_kInitDistance = new HashMap(); /* map MjEdgeKey to Float */

    /** DOCUMENT ME! */
    private Point2f m_kPlaneMax;

    /** DOCUMENT ME! */
    private Point2f m_kPlaneMin;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Creates a new MjCorticalMesh object.
     *
     * @param  iVQuantity  DOCUMENT ME!
     * @param  akPoint     DOCUMENT ME!
     * @param  iTQuantity  DOCUMENT ME!
     * @param  aiConnect   DOCUMENT ME!
     */
    public MjCorticalMesh(int iVQuantity, Point3f[] akPoint, int iTQuantity, int[] aiConnect) {
        m_iVQuantity = iVQuantity;
        m_akPoint = akPoint;
        m_iTQuantity = iTQuantity;
        m_aiConnect = aiConnect;
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

        /* dynamically allocate storage for each vertex normal and color */
        m_akNormal = new MjVector3f[iVQuantity];
        m_akColor = new Color4f[iVQuantity];

        for (int i = 0; i < m_iVQuantity; i++) {
            m_akNormal[i] = new MjVector3f();
            m_akColor[i] = new Color4f();
        }

        /* dynamically construct triangle mesh from input */
        m_akVertex = new Vertex[m_iVQuantity];

        for (int i = 0; i < m_akVertex.length; i++) {
            m_akVertex[i] = new Vertex();
        }

        m_akEdge = new Edge[3 * m_iTQuantity];

        for (int i = 0; i < m_akEdge.length; i++) {
            m_akEdge[i] = new Edge();
        }

        m_akTriangle = new Triangle[m_iTQuantity];

        for (int i = 0; i < m_akTriangle.length; i++) {
            m_akTriangle[i] = new Triangle();
        }

        HashMap kEMap = new HashMap(); /* map MjEdgeKey -> int */

        for (int iT = 0; iT < m_iTQuantity; iT++) {

            /* update triangle */
            Triangle rkT = m_akTriangle[iT];
            rkT.V[0] = aiConnect[(3 * iT) + 0];
            rkT.V[1] = aiConnect[(3 * iT) + 1];
            rkT.V[2] = aiConnect[(3 * iT) + 2];

            /* add edges to mesh */
            for (int i0 = 2, i1 = 0; i1 < 3; i0 = i1++) {

                /* update vertices */
                m_akVertex[rkT.V[i1]].InsertTriangle(iT);

                MjEdgeKey kKey = new MjEdgeKey(rkT.V[i0], rkT.V[i1]);

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

    /**
     * conformal mapping of mesh to plane and sphere, projection to cylinder.
     */
    public void computeConformalMapping() {
        m_akPlane = new Point2f[m_iVQuantity];
        m_akSphere = new Point3f[m_iVQuantity];
        m_akCylinder = new Point3f[m_iVQuantity];

        for (int i = 0; i < m_iVQuantity; i++) {
            m_akPlane[i] = new Point2f();
            m_akSphere[i] = new Point3f();
            m_akCylinder[i] = new Point3f();
        }

        MjVector3f kE0 = new MjVector3f();
        MjVector3f kE1 = new MjVector3f();
        MjVector3f kNormal = new MjVector3f();
        MjVector3f kE10 = new MjVector3f();
        MjVector3f kE20 = new MjVector3f();
        MjVector3f kE12 = new MjVector3f();
        MjVector3f kCross = new MjVector3f();

        int iV0, iV1, iV2;
        float fValue = 0.0f;

        /* construct sparse matrix A nondiagonal entries */
        MjSparseMatrix kAMat = new MjSparseMatrix();

        for (int iE = 0; iE < m_iEQuantity; iE++) {
            Edge kE = m_akEdge[iE];
            iV0 = kE.V[0];
            iV1 = kE.V[1];

            Triangle kT0 = m_akTriangle[kE.T[0]];

            for (int i = 0; i < 3; i++) {
                iV2 = kT0.V[i];

                if ((iV2 != iV0) && (iV2 != iV1)) {
                    kE0.sub(m_akPoint[iV0], m_akPoint[iV2]);
                    kE1.sub(m_akPoint[iV1], m_akPoint[iV2]);
                    kNormal.cross(kE0, kE1);
                    fValue = kE0.dot(kE1) / kNormal.length();
                }
            }

            Triangle kT1 = m_akTriangle[kE.T[1]];

            for (int i = 0; i < 3; i++) {
                iV2 = kT1.V[i];

                if ((iV2 != iV0) && (iV2 != iV1)) {
                    kE0.sub(m_akPoint[iV0], m_akPoint[iV2]);
                    kE1.sub(m_akPoint[iV1], m_akPoint[iV2]);
                    kNormal.cross(kE0, kE1);
                    fValue += kE0.dot(kE1) / kNormal.length();
                }
            }

            fValue *= -0.5f;
            kAMat.setElement(iV0, iV1, fValue);
        }

        /* construct sparse matrix A diagonal entries */
        float[] afTmp = new float[m_iVQuantity];

        for (int i = 0; i < m_iVQuantity; i++) {
            afTmp[i] = 0.0f;
        }

        Iterator kIter = kAMat.iterator();

        while (kIter.hasNext()) {
            Map.Entry kEntry = (Map.Entry) kIter.next();
            MjSparseMatrix.Index kIndex = (MjSparseMatrix.Index) kEntry.getKey();
            Float kValue = (Float) kEntry.getValue();
            iV0 = kIndex.m_iRow;
            iV1 = kIndex.m_iCol;
            fValue = kValue.floatValue();
            assert (iV0 != iV1);
            afTmp[iV0] -= fValue;
            afTmp[iV1] -= fValue;
        }

        for (int iV = 0; iV < m_iVQuantity; iV++) {
            kAMat.setElement(iV, iV, afTmp[iV]);
        }

        assert (kAMat.size() == (m_iVQuantity + m_iEQuantity));

        /* Construct column vector B (happens to be sparse).  Triangle 0 is
         * the default for the puncture, but may also be set by the user */
        Triangle kT = m_akTriangle[m_iPunctureTri];
        iV0 = kT.V[0];
        iV1 = kT.V[1];
        iV2 = kT.V[2];

        Point3f kV0 = m_akPoint[iV0];
        Point3f kV1 = m_akPoint[iV1];
        Point3f kV2 = m_akPoint[iV2];
        kE10.sub(kV1, kV0);
        kE20.sub(kV2, kV0);
        kE12.sub(kV1, kV2);
        kCross.cross(kE20, kE10);

        float fLen10 = kE10.length();
        float fInvLen10 = 1.0f / fLen10;
        float fTwoArea = kCross.length();
        float fInvLenCross = 1.0f / fTwoArea;
        float fInvProd = fInvLen10 * fInvLenCross;
        float fRe0 = -fInvLen10;
        float fIm0 = fInvProd * kE12.dot(kE10);
        float fRe1 = fInvLen10;
        float fIm1 = fInvProd * kE20.dot(kE10);
        float fRe2 = 0.0f;
        float fIm2 = -fLen10 * fInvLenCross;

        /* solve sparse system for real parts */
        for (int i = 0; i < m_iVQuantity; i++) {
            afTmp[i] = 0.0f;
        }

        afTmp[iV0] = fRe0;
        afTmp[iV1] = fRe1;
        afTmp[iV2] = fRe2;

        float[] afResult = new float[m_iVQuantity];
        boolean bSolved = kAMat.solveSymmetricCG(m_iVQuantity, afTmp, afResult);
        assert (bSolved);

        for (int i = 0; i < m_iVQuantity; i++) {
            m_akPlane[i].x = afResult[i];
        }

        /* solve sparse system for imaginary parts */
        for (int i = 0; i < m_iVQuantity; i++) {
            afTmp[i] = 0.0f;
        }

        afTmp[iV0] = -fIm0;
        afTmp[iV1] = -fIm1;
        afTmp[iV2] = -fIm2;
        bSolved = kAMat.solveSymmetricCG(m_iVQuantity, afTmp, afResult);
        assert (bSolved);

        for (int i = 0; i < m_iVQuantity; i++) {
            m_akPlane[i].y = afResult[i];
        }

        /* scale to [-1,1]^2 for numerical conditioning in later steps */
        float fMin = m_akPlane[0].x;
        float fMax = fMin;

        for (int i = 0; i < m_iVQuantity; i++) {

            if (m_akPlane[i].x < fMin) {
                fMin = m_akPlane[i].x;
            } else if (m_akPlane[i].x > fMax) {
                fMax = m_akPlane[i].x;
            }

            if (m_akPlane[i].y < fMin) {
                fMin = m_akPlane[i].y;
            } else if (m_akPlane[i].y > fMax) {
                fMax = m_akPlane[i].y;
            }
        }

        float fHalfRange = 0.5f * (fMax - fMin);
        float fInvHalfRange = 1.0f / fHalfRange;

        for (int i = 0; i < m_iVQuantity; i++) {
            m_akPlane[i].x = -1.0f + (fInvHalfRange * (m_akPlane[i].x - fMin));
            m_akPlane[i].y = -1.0f + (fInvHalfRange * (m_akPlane[i].y - fMin));
        }

        /* Map plane points to sphere using inverse stereographic projection. */
        /* The main issue is selecting a translation in (x,y) and a radius of */
        /* the projection sphere.  Both factors strongly influence the final */
        /* result. */

        /* Use the average as the south pole.  The points tend to be clustered */
        /* approximately in the middle of the conformally mapped punctured */
        /* triangle, so the average is a good choice to place the pole. */
        Point2f kOrigin = new Point2f(0.0f, 0.0f);

        for (int i = 0; i < m_iVQuantity; i++) {
            kOrigin.add(m_akPlane[i]);
        }

        kOrigin.scale(1.0f / (float) m_iVQuantity);

        for (int i = 0; i < m_iVQuantity; i++) {
            m_akPlane[i].sub(kOrigin);
        }

        m_kPlaneMin = new Point2f(m_akPlane[0]);
        m_kPlaneMax = new Point2f(m_akPlane[0]);

        for (int i = 1; i < m_iVQuantity; i++) {

            if (m_akPlane[i].x < m_kPlaneMin.x) {
                m_kPlaneMin.x = m_akPlane[i].x;
            } else if (m_akPlane[i].x > m_kPlaneMax.x) {
                m_kPlaneMax.x = m_akPlane[i].x;
            }

            if (m_akPlane[i].y < m_kPlaneMin.y) {
                m_kPlaneMin.y = m_akPlane[i].y;
            } else if (m_akPlane[i].y > m_kPlaneMax.y) {
                m_kPlaneMax.y = m_akPlane[i].y;
            }
        }

        /* Select the radius of the sphere so that the projected punctured */
        /* triangle has an area whose fraction of total spherical area is the */
        /* same fraction as the area of the punctured triangle to the total area */
        /* of the original triangle mesh. */
        float fTwoTotalArea = 0.0f;

        for (int iT = 0; iT < m_iTQuantity; iT++) {
            Triangle kT0 = m_akTriangle[iT];
            kV0 = m_akPoint[kT0.V[0]];
            kV1 = m_akPoint[kT0.V[1]];
            kV2 = m_akPoint[kT0.V[2]];
            kE0.sub(kV1, kV0);
            kE1.sub(kV2, kV0);
            kCross.cross(kE0, kE1);
            fTwoTotalArea += kCross.length();
        }

        m_fRho = computeRadius(new MjVector2f(m_akPlane[iV0]), new MjVector2f(m_akPlane[iV1]),
                               new MjVector2f(m_akPlane[iV2]), fTwoArea / fTwoTotalArea);

        float fRhoSqr = m_fRho * m_fRho;

        /* Inverse stereographic projection to obtain sphere coordinates.  The */
        /* sphere is centered at the origin and has radius 1. */
        MjVector2f kPlaneVector = new MjVector2f();

        for (int i = 0; i < m_iVQuantity; i++) {
            kPlaneVector.set(m_akPlane[i]);

            float fRSqr = kPlaneVector.lengthSquared();
            float fMult = 1.0f / (fRSqr + fRhoSqr);
            float fX = 2.0f * fMult * fRhoSqr * m_akPlane[i].x;
            float fY = 2.0f * fMult * fRhoSqr * m_akPlane[i].y;
            float fZ = fMult * m_fRho * (fRSqr - fRhoSqr);
            m_akSphere[i].set(fX, fY, fZ);
            m_akSphere[i].scale(1.0f / m_fRho);
        }

        /* Project the sphere onto a cylinder.  The cylinder is centered at the */
        /* origin and has axis direction (0,0,1).  The radius of the cylinder is */
        /* the radius of the sphere, 1.  The height of the cylinder is 2 since z */
        /* varies from -1 to 1. */
        for (int i = 0; i < m_iVQuantity; i++) {
            m_akCylinder[i].x = -(float) Math.atan2(m_akSphere[i].y, m_akSphere[i].x);
            m_akCylinder[i].y = m_akSphere[i].z;
            m_akCylinder[i].z = 0.0f;
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

        for (int iSource = 0; iSource < m_iVQuantity; iSource++) {
            computeDistance(iSize, iSource, false);
        }

        computeExtremes();
    }

    /**
     * mean curvature.
     */
    public void computeMeanCurvature() {
        m_afMeanCurvature = new float[m_iVQuantity];

        MjMeshCurvature kMG = new MjMeshCurvature(m_iVQuantity, m_akPoint, m_iTQuantity, m_aiConnect);
        float[] afMinCurv = kMG.getMinCurvatures();
        float[] afMaxCurv = kMG.getMaxCurvatures();
        m_fMinMeanCurvature = afMinCurv[0] + afMaxCurv[0];
        m_fMaxMeanCurvature = m_fMinMeanCurvature;

        for (int i = 0; i < m_iVQuantity; i++) {
            m_afMeanCurvature[i] = afMinCurv[i] + afMaxCurv[i];

            if (m_afMeanCurvature[i] < m_fMinMeanCurvature) {
                m_fMinMeanCurvature = m_afMeanCurvature[i];
            } else if (m_afMeanCurvature[i] > m_fMaxMeanCurvature) {
                m_fMaxMeanCurvature = m_afMeanCurvature[i];
            }
        }
    }

    /**
     * compute vertex normals as averages of triangle normals.
     */
    public void computeNormals() {

        /* Initialize each normal vector to zero before summing. */
        for (int i = 0; i < m_iVQuantity; i++) {
            m_akNormal[i].set(MjVector3f.ZERO);
        }

        MjVector3f kEdge1 = new MjVector3f();
        MjVector3f kEdge2 = new MjVector3f();
        MjVector3f kNormal = new MjVector3f();

        for (int i = 0; i < m_iTQuantity; i++) {

            /* get vertex indices */
            int iV0 = m_aiConnect[(3 * i) + 0];
            int iV1 = m_aiConnect[(3 * i) + 1];
            int iV2 = m_aiConnect[(3 * i) + 2];

            /* get vertices */
            Point3f kV0 = m_akPoint[iV0];
            Point3f kV1 = m_akPoint[iV1];
            Point3f kV2 = m_akPoint[iV2];

            /* compute the normal (length provides the weighted sum) */
            kEdge1.sub(kV1, kV0);
            kEdge2.sub(kV2, kV0);
            kNormal.cross(kEdge1, kEdge2);

            m_akNormal[iV0].add(kNormal);
            m_akNormal[iV1].add(kNormal);
            m_akNormal[iV2].add(kNormal);
        }

        for (int i = 0; i < m_iVQuantity; i++) {
            m_akNormal[i].normalizeSafe();
        }
    }

    /**
     * surface area of the mesh (input mesh is closed, manifold).
     */
    public void computeSurfaceArea() {
        m_fSurfaceArea = 0.0f;

        MjVector3f kE1 = new MjVector3f();
        MjVector3f kE2 = new MjVector3f();
        MjVector3f kNormal = new MjVector3f();

        for (int i = 0; i < m_iTQuantity; i++) {
            Point3f kV0 = m_akPoint[m_aiConnect[(3 * i) + 0]];
            Point3f kV1 = m_akPoint[m_aiConnect[(3 * i) + 1]];
            Point3f kV2 = m_akPoint[m_aiConnect[(3 * i) + 2]];
            kE1.sub(kV1, kV0);
            kE2.sub(kV2, kV0);
            kNormal.cross(kE1, kE2);
            m_fSurfaceArea += 0.5f * kNormal.length();
        }
    }

    /**
     * DOCUMENT ME!
     */
    public void disposeLocal() {

        for (int i = 0; i < m_iVQuantity; i++) {

            if (m_akNormal != null) {
                m_akNormal[i] = null;
            }

            if (m_akColor != null) {
                m_akColor[i] = null;
            }

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

        if (m_akNormal != null) {
            m_akNormal = null;
        }

        if (m_akColor != null) {
            m_akColor = null;
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

        switch (iOperation) {

            case 0:
                m_afAvrConvexity = new float[m_iVQuantity];
                for (int i = 0; i < m_iVQuantity; i++) {
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

                    for (int i = 1; i < m_iVQuantity; i++) {

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
    public Color4f[] getColorCopy() {
        Color4f[] akColors = new Color4f[m_iVQuantity];

        for (int i = 0; i < m_iVQuantity; i++) {
            akColors[i] = new Color4f(m_akColor[i].x, m_akColor[i].y, m_akColor[i].z, 1f);
        }

        return akColors;
    }

    /**
     * DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    public Color4f[] getColors() {
        return m_akColor;
    }

    /**
     * DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    public int[] getConnectivity() {
        return m_aiConnect;
    }

    /**
     * DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    public int[] getConnectivityCopy() {
        int[] aiConnect = new int[m_iTQuantity * 3];

        for (int i = 0; i < m_iTQuantity; i++) {
            aiConnect[(3 * i) + 0] = m_aiConnect[(3 * i) + 0];
            aiConnect[(3 * i) + 1] = m_aiConnect[(3 * i) + 1];
            aiConnect[(3 * i) + 2] = m_aiConnect[(3 * i) + 2];
        }

        return aiConnect;
    }

    /**
     * DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    public Point3f[] getCylinderCoordinates() {
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
        Float kDistance = (Float) m_kDistance.get(new MjEdgeKey(iV0, iV1));

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
        MjPrimitive.Circle2f kLatitude = new MjPrimitive.Circle2f();
        kLatitude.m_kCenter.set(MjVector2f.ZERO);
        kLatitude.m_fRadius = (float) Math.sqrt((1.0f + fZNormal) / (1.0f - fZNormal)) * getStereographicRadius();

        /* sorted t-values for intersections of circle */
        /* with triangle mesh edges */
        TreeMap kIntrMesh = new TreeMap(); /* map<float,Point3f> */
        TreeMap kIntrSphere = new TreeMap(); /* map<float,Point3f> */

        MjPrimitive.Segment2f kEdge = new MjPrimitive.Segment2f();
        MjVector3f kU1mU0 = new MjVector3f();
        MjVector3f kU2mU0 = new MjVector3f();
        MjVector3f kN0 = new MjVector3f();
        MjVector3f kN1 = new MjVector3f();
        MjVector3f kNAvr = new MjVector3f();
        MjVector3f kNormal = new MjVector3f();
        MjVector2f kDiff = new MjVector2f();

        for (int i = 0; i < m_iEQuantity; i++) {

            /* line segment representing the edge */
            int iV0 = m_akEdge[i].V[0];
            int iV1 = m_akEdge[i].V[1];
            Point2f kV0 = m_akPlane[iV0];
            Point2f kV1 = m_akPlane[iV1];
            kEdge.m_kOrigin.set(kV0);
            kEdge.m_kDirection.sub(kV1, kV0);

            /* compute intersection of ray and segment */
            Point2f[] akP = new Point2f[2];
            int iCount = MjPrimitive.findIntersection(kEdge, kLatitude, akP);

            if (iCount > 0) {

                /* get normal for sharing triangle */
                int iTriangle = m_akEdge[i].T[0];
                Point3f kU0 = m_akPoint[m_aiConnect[(3 * iTriangle) + 0]];
                Point3f kU1 = m_akPoint[m_aiConnect[(3 * iTriangle) + 1]];
                Point3f kU2 = m_akPoint[m_aiConnect[(3 * iTriangle) + 2]];
                kU1mU0.sub(kU1, kU0);
                kU2mU0.sub(kU2, kU0);
                kN0.cross(kU1mU0, kU2mU0);
                kN0.normalizeSafe();

                /* get normal for sharing triangle */
                iTriangle = m_akEdge[i].T[1];
                kU0 = m_akPoint[m_aiConnect[(3 * iTriangle) + 0]];
                kU1 = m_akPoint[m_aiConnect[(3 * iTriangle) + 1]];
                kU2 = m_akPoint[m_aiConnect[(3 * iTriangle) + 2]];
                kU1mU0.sub(kU1, kU0);
                kU2mU0.sub(kU2, kU0);
                kN1.cross(kU1mU0, kU2mU0);
                kN1.normalizeSafe();

                /* average normal */
                kNAvr.add(kN0, kN1);
                kNAvr.normalizeSafe();

                for (int j = 0; j < iCount; j++) {

                    /* Use the angle formed by the intersection point with the */
                    /* positive x-axis as the map key for sorting. */
                    float fAngle = (float) Math.atan2(akP[j].y, akP[j].x);

                    /* determine the edge parameter at the intersection */
                    kDiff.sub(akP[j], kV0);

                    float fNumer = kDiff.dot(kEdge.m_kDirection);
                    float fDenom = kEdge.m_kDirection.lengthSquared();
                    float fS = fNumer / fDenom;

                    /* construct the edge point on the original mesh */
                    Point3f kQ = new Point3f();
                    kQ.sub(m_akPoint[iV1], m_akPoint[iV0]);
                    kQ.scale(fS);
                    kQ.add(m_akPoint[iV0]);

                    /* To avoid z-buffer biasing problems, lift the point in the */
                    /* direction of the average of the normals for the triangles */
                    /* sharing the edge. */
                    kQ.scaleAdd(fMBias, kNAvr, kQ);

                    /* save the point in an ordered map */
                    kIntrMesh.put(new Float(fAngle), kQ);

                    /* construct the edge point on the sphere */
                    kQ = new Point3f();
                    kQ.sub(m_akSphere[iV1], m_akSphere[iV0]);
                    kQ.scale(fS);
                    kQ.add(m_akSphere[iV0]);

                    /* To avoid z-buffer biasing problems, lift the point in the */
                    /* direction of sphere normal. */
                    kNormal.set(kQ);
                    kNormal.normalizeSafe();
                    kQ.scaleAdd(fSBias, kNormal, kNormal);

                    /* save the point in an ordered map */
                    kIntrSphere.put(new Float(fAngle), kQ);
                }
            }
        }

        Polylines kPolylines = new Polylines();
        assert (kIntrMesh.size() > 0);
        kPolylines.akMVertex = new Point3f[kIntrMesh.size()];
        kPolylines.akSVertex = new Point3f[kIntrSphere.size()];

        Iterator kIM = kIntrMesh.entrySet().iterator();
        Iterator kIS = kIntrSphere.entrySet().iterator();

        for (int i = 0; kIM.hasNext(); i++) {
            kPolylines.akMVertex[i] = (Point3f) (((Map.Entry) kIM.next()).getValue());
            kPolylines.akSVertex[i] = (Point3f) (((Map.Entry) kIS.next()).getValue());
        }

        kPolylines.akPVertex = new Point3f[2];
        kPolylines.akPVertex[0] = new Point3f(+(float) Math.PI, fZNormal, -fPBias);
        kPolylines.akPVertex[1] = new Point3f(-(float) Math.PI, fZNormal, -fPBias);

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
        final Point2f kPoint2fZero = new Point2f(0.0f, 0.0f);

        /* longitude ray in the complex plane */
        MjPrimitive.Ray2f kLongitude = new MjPrimitive.Ray2f();
        kLongitude.m_kOrigin.set(kPoint2fZero);
        kLongitude.m_kDirection.x = (float) Math.cos(fAngle);
        kLongitude.m_kDirection.y = (float) Math.sin(fAngle);

        /* sorted t-values for intersections of ray with edges */
        TreeMap kIntrMesh = new TreeMap(); /* map<float,Point3f> */
        TreeMap kIntrSphere = new TreeMap(); /* map<float,Point3f> */

        /* ***** add the ray origin to the polyline ***** */
        int iContain = getContainingTriangle(kPoint2fZero, 0);
        assert (iContain > 0);

        Point3f kBary = getBarycentric(kPoint2fZero, iContain);
        int iV0 = m_aiConnect[(3 * iContain) + 0];
        int iV1 = m_aiConnect[(3 * iContain) + 1];
        int iV2 = m_aiConnect[(3 * iContain) + 2];
        MjVector3f kU1mU0 = new MjVector3f();
        MjVector3f kU2mU0 = new MjVector3f();
        MjVector3f kNormal = new MjVector3f();

        /* get normal for containing triangle */
        Point3f kU0 = m_akPoint[iV0];
        Point3f kU1 = m_akPoint[iV1];
        Point3f kU2 = m_akPoint[iV2];
        kU1mU0.sub(kU1, kU0);
        kU2mU0.sub(kU2, kU0);
        kNormal.cross(kU1mU0, kU2mU0);
        kNormal.normalizeSafe();

        /* compute point on original mesh */
        Point3f kQ = new Point3f();
        kQ.scaleAdd(kBary.x, kU0, kQ);
        kQ.scaleAdd(kBary.y, kU1, kQ);
        kQ.scaleAdd(kBary.z, kU2, kQ);

        /* lift slightly off the surface */
        kQ.scaleAdd(fMBias, kNormal, kQ);
        kIntrMesh.put(new Float(0.0f), kQ);

        /* repeat calculations for sphere */
        kU0 = m_akSphere[iV0];
        kU1 = m_akSphere[iV1];
        kU2 = m_akSphere[iV2];
        kQ = new Point3f();
        kQ.scaleAdd(kBary.x, kU0, kQ);
        kQ.scaleAdd(kBary.y, kU1, kQ);
        kQ.scaleAdd(kBary.z, kU2, kQ);
        kNormal.set(kQ);
        kNormal.normalizeSafe();
        kQ.scaleAdd(fMBias, kNormal, kQ);
        kIntrSphere.put(new Float(0.0f), kQ);
        /* ***** end add ray origin ***** */

        /* ***** add the ray infinity to the polyline ***** */
        iContain = 0; /* TO DO: if puncture triangle is changed, change this */

        /* get normal for containing triangle */
        kU0 = m_akPoint[iV0];
        kU1 = m_akPoint[iV1];
        kU2 = m_akPoint[iV2];
        kU1mU0.sub(kU1, kU0);
        kU2mU0.sub(kU2, kU0);
        kNormal.cross(kU1mU0, kU2mU0);
        kNormal.normalizeSafe();

        /* compute point on original mesh */
        kQ = new Point3f();
        kQ.add(kU0);
        kQ.add(kU1);
        kQ.add(kU2);
        kQ.scale(1.0f / 3.0f);

        /* lift slightly off the surface */
        kQ.scaleAdd(fMBias, kNormal, kQ);
        kIntrMesh.put(new Float(Float.MAX_VALUE), kQ);

        /* repeat calculations for sphere */
        kU0 = m_akSphere[iV0];
        kU1 = m_akSphere[iV1];
        kU2 = m_akSphere[iV2];
        kQ = new Point3f();
        kQ.add(kU0);
        kQ.add(kU1);
        kQ.add(kU2);
        kQ.scale(1.0f / 3.0f);
        kNormal.set(kQ);
        kNormal.normalizeSafe();
        kQ.scaleAdd(fSBias, kNormal, kQ);
        kIntrSphere.put(new Float(Float.MAX_VALUE), kQ);
        /* ***** end add ray origin ***** */

        float[] afT = new float[2];
        MjVector3f kNormal0 = new MjVector3f();
        MjVector3f kNormal1 = new MjVector3f();
        MjVector2f kDiff = new MjVector2f();

        for (int i = 0; i < m_iEQuantity; i++) {

            /* line segment representing the edge */
            iV0 = m_akEdge[i].V[0];
            iV1 = m_akEdge[i].V[1];

            Point2f kV0 = m_akPlane[iV0];
            Point2f kV1 = m_akPlane[iV1];
            MjPrimitive.Segment2f kEdge = new MjPrimitive.Segment2f();
            kEdge.m_kOrigin.set(kV0);
            kEdge.m_kDirection.sub(kV1, kV0);

            /* compute intersection of ray and segment */
            int iCount = MjPrimitive.findIntersection(kLongitude, kEdge, afT);

            if (iCount > 0) {

                /* get normal for sharing triangle */
                int iTriangle = m_akEdge[i].T[0];
                kU0 = m_akPoint[m_aiConnect[(3 * iTriangle) + 0]];
                kU1 = m_akPoint[m_aiConnect[(3 * iTriangle) + 1]];
                kU2 = m_akPoint[m_aiConnect[(3 * iTriangle) + 2]];
                kU1mU0.sub(kU1, kU0);
                kU2mU0.sub(kU2, kU0);
                kNormal0.cross(kU1mU0, kU2mU0);
                kNormal0.normalizeSafe();

                /* get normal for sharing triangle */
                iTriangle = m_akEdge[i].T[1];
                kU0 = m_akPoint[m_aiConnect[(3 * iTriangle) + 0]];
                kU1 = m_akPoint[m_aiConnect[(3 * iTriangle) + 1]];
                kU2 = m_akPoint[m_aiConnect[(3 * iTriangle) + 2]];
                kU1mU0.sub(kU1, kU0);
                kU2mU0.sub(kU2, kU0);
                kNormal1.cross(kU1mU0, kU2mU0);
                kNormal1.normalizeSafe();

                kNormal.add(kNormal0, kNormal1);
                kNormal.normalizeSafe();

                for (int j = 0; j < iCount; j++) {

                    /* afT[j] is the ray parameter, need to compute the edge */
                    /* parameter */
                    kDiff.set(kLongitude.m_kDirection);
                    kDiff.scale(afT[j]);
                    kDiff.sub(kV0);

                    float fNumer = kDiff.dot(kEdge.m_kDirection);
                    float fDenom = kEdge.m_kDirection.lengthSquared();
                    float fS = fNumer / fDenom;

                    /* construct the edge point on the original mesh */
                    kQ = new Point3f();
                    kQ.sub(m_akPoint[iV1], m_akPoint[iV0]);
                    kQ.scale(fS);
                    kQ.add(m_akPoint[iV0]);

                    /* To avoid z-buffer biasing problems, lift the point in the */
                    /* direction of the average of the normals for the triangles */
                    /* sharing the edge. */
                    kQ.scaleAdd(fMBias, kNormal, kQ);

                    kIntrMesh.put(new Float(afT[j]), kQ);

                    /* repeat the construction for sphere */
                    kQ = new Point3f();
                    kQ.sub(m_akSphere[iV1], m_akSphere[iV0]);
                    kQ.scale(fS);
                    kQ.add(m_akSphere[iV0]);
                    kNormal.set(kQ);
                    kNormal.normalizeSafe();
                    kQ.scaleAdd(fSBias, kNormal, kQ);
                    kIntrSphere.put(new Float(afT[j]), kQ);
                }
            }
        }

        Polylines kPolylines = new Polylines();
        assert (kIntrMesh.size() > 0);
        kPolylines.akMVertex = new Point3f[kIntrMesh.size()];
        kPolylines.akSVertex = new Point3f[kIntrSphere.size()];

        Iterator kIM = kIntrMesh.entrySet().iterator();
        Iterator kIS = kIntrSphere.entrySet().iterator();

        for (int i = 0; kIM.hasNext(); i++) {
            kPolylines.akMVertex[i] = (Point3f) (((Map.Entry) kIM.next()).getValue());
            kPolylines.akSVertex[i] = (Point3f) (((Map.Entry) kIS.next()).getValue());
        }

        fAngle -= Math.PI;
        kPolylines.akPVertex = new Point3f[2];
        kPolylines.akPVertex[0] = new Point3f(fAngle, -1.0f, -fPBias);
        kPolylines.akPVertex[1] = new Point3f(fAngle, +1.0f, -fPBias);

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
    public Vector3f[] getNormalCopy() {
        Vector3f[] akNormals = new Vector3f[m_iVQuantity];

        for (int i = 0; i < m_iVQuantity; i++) {
            akNormals[i] = new Vector3f(m_akNormal[i].x, m_akNormal[i].y, m_akNormal[i].z);
        }

        return akNormals;
    }

    /**
     * DOCUMENT ME!
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
    public Point2f[] getPlaneCoordinates() {
        return m_akPlane;
    }

    /**
     * DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    public Point2f getPlaneMax() {
        return m_kPlaneMax;
    }

    /**
     * DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    public Point2f getPlaneMin() {
        return m_kPlaneMin;
    }

    /**
     * DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    public Point3f[] getPointCopy() {
        Point3f[] akVertices = new Point3f[m_iVQuantity];

        for (int i = 0; i < m_iVQuantity; i++) {
            akVertices[i] = new Point3f(m_akPoint[i].x, m_akPoint[i].y, m_akPoint[i].z);
        }

        return akVertices;
    }

    /**
     * attribute member access.
     *
     * @return  DOCUMENT ME!
     */
    public Point3f[] getPoints() {
        return m_akPoint;
    }

    /**
     * DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    public Point3f[] getSphereCoordinates() {
        return m_akSphere;
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
    public int getTQuantity() {
        return m_iTQuantity;
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

    /**
     * topology member access.
     *
     * @return  DOCUMENT ME!
     */
    public int getVQuantity() {
        return m_iVQuantity;
    }

    /**
     * uniformly scale points to [-1,1]^3, originally in [min,max]^3 returns 2D vector with (min,max) as the elements
     * public Vector2f scaleToCube ().
     *
     * @return  DOCUMENT ME!
     */
    public float scaleToCube() {
        float fMin = m_akPoint[0].x;
        float fMax = fMin;

        for (int i = 0; i < m_iVQuantity; i++) {

            if (m_akPoint[i].x < fMin) {
                fMin = m_akPoint[i].x;
            } else if (m_akPoint[i].x > fMax) {
                fMax = m_akPoint[i].x;
            }

            if (m_akPoint[i].y < fMin) {
                fMin = m_akPoint[i].y;
            } else if (m_akPoint[i].y > fMax) {
                fMax = m_akPoint[i].y;
            }

            if (m_akPoint[i].z < fMin) {
                fMin = m_akPoint[i].z;
            } else if (m_akPoint[i].z > fMax) {
                fMax = m_akPoint[i].z;
            }
        }

        float fHalfRange = 0.5f * (fMax - fMin);
        float fInvHalfRange = 1.0f / fHalfRange;
        MjVector3f kOne = new MjVector3f(1.0f, 1.0f, 1.0f);

        for (int i = 0; i < m_iVQuantity; i++) {
            m_akPoint[i].scaleAdd(-fMin, kOne, m_akPoint[i]);
            m_akPoint[i].scale(fInvHalfRange);
            m_akPoint[i].sub(kOne);
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
        for (int i = 0; i < m_iTQuantity; i++) {
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
    private static boolean contains(Point2f[] akVertex, Point2f kP) {
        /* assert:  <V0,V1,V2> is clockwise ordered */

        final float fEpsilon = 1e-05f;
        MjVector2f kV1mV0 = new MjVector2f();
        MjVector2f kInnerNormal = new MjVector2f();
        MjVector2f kDiff = new MjVector2f();

        for (int i0 = 2, i1 = 0; i1 < 3; i0 = i1++) {
            kV1mV0.sub(akVertex[i1], akVertex[i0]);
            kInnerNormal.perp(kV1mV0);
            kDiff.sub(kP, akVertex[i0]);
            kInnerNormal.normalizeSafe();
            kDiff.normalizeSafe();

            float fCos = kInnerNormal.dot(kDiff);

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
        Set kInterior = new HashSet(); /* set of int */
        kInterior.add(new Integer(iSource));

        /* compute distances to all 1-neighbors */
        Vertex kSource = m_akVertex[iSource];
        int i, iNbr;
        Set kBoundary = new HashSet(); /* set of int */

        for (i = 0; i < kSource.VQuantity; i++) {
            iNbr = kSource.V[i];
            kBoundary.add(new Integer(iNbr));

            MjEdgeKey kKey = new MjEdgeKey(iSource, iNbr);

            if (!m_kDistance.containsKey(kKey)) {
                float fDist = m_akPoint[iNbr].distance(m_akPoint[iSource]);
                m_kDistance.put(kKey, new Float(fDist));
            }
        }

        /* Compute distances to farther neighbors using Dijkstra's algorithm. */
        /* TO DO.  A set of boundary points is used to support the breadth-first */
        /* search.  If the neighborhoods are very large and performance is an */
        /* issue, the sets should be replaced by a priority queue. */

        for (int j = 2; j <= iSize; j++) {
            Set kExterior = new HashSet(); /* set of int */

            Iterator kIter = kBoundary.iterator();

            while (kIter.hasNext()) {

                /* current boundary point to process */
                int iCenter = ((Integer) (kIter.next())).intValue();
                Vertex kCenter = m_akVertex[iCenter];

                /* distance of path <source,center> */
                float fPathLength = ((Float) (m_kDistance.get(new MjEdgeKey(iSource, iCenter)))).floatValue();

                /* search immediate, exterior neighbors */
                for (i = 0; i < kCenter.VQuantity; i++) {
                    iNbr = kCenter.V[i];

                    Integer kNbr = new Integer(iNbr);

                    if (!kInterior.contains(kNbr) && !kBoundary.contains(kNbr)) {

                        /* get distance of edge <center,nbr> */
                        Float kDist = (Float) m_kDistance.get(new MjEdgeKey(iCenter, iNbr));
                        float fDist = (null != kDist) ? kDist.floatValue()
                                                      : m_akPoint[iNbr].distance(m_akPoint[iCenter]);

                        /* get distance of <source,nbr> along current path */
                        float fTotalLength = fPathLength + fDist;

                        /* update current distance to shorter one */
                        /* (if necessary) */
                        MjEdgeKey kKey = new MjEdgeKey(iSource, iNbr);
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
    private float computeRadius(MjVector2f kV0, MjVector2f kV1, MjVector2f kV2, float fAreaFraction) {
        float fR0Sqr = kV0.lengthSquared();
        float fR1Sqr = kV1.lengthSquared();
        float fR2Sqr = kV2.lengthSquared();
        float fDR10 = fR1Sqr - fR0Sqr;
        float fDR20 = fR2Sqr - fR0Sqr;
        float fDX10 = kV1.x - kV0.x;
        float fDY10 = kV1.y - kV0.y;
        float fDX20 = kV2.x - kV0.x;
        float fDY20 = kV2.y - kV0.y;
        float fDRX10 = (kV1.x * fR0Sqr) - (kV0.x * fR1Sqr);
        float fDRY10 = (kV1.y * fR0Sqr) - (kV0.y * fR1Sqr);
        float fDRX20 = (kV2.x * fR0Sqr) - (kV0.x * fR2Sqr);
        float fDRY20 = (kV2.y * fR0Sqr) - (kV0.y * fR2Sqr);

        float fC0 = (fDR20 * fDRY10) - (fDR10 * fDRY20);
        float fC1 = (fDR20 * fDY10) - (fDR10 * fDY20);
        float fD0 = (fDR10 * fDRX20) - (fDR20 * fDRX10);
        float fD1 = (fDR10 * fDX20) - (fDR20 * fDX10);
        float fE0 = (fDRX10 * fDRY20) - (fDRX20 * fDRY10);
        float fE1 = (fDRX10 * fDY20) - (fDRX20 * fDY10);
        float fE2 = (fDX10 * fDY20) - (fDX20 * fDY10);

        MjPolynomial1f kP0 = new MjPolynomial1f(6);
        kP0.setCoeff(0, 0.0f);
        kP0.setCoeff(1, 0.0f);
        kP0.setCoeff(2, fE0 * fE0);
        kP0.setCoeff(3, (fC0 * fC0) + (fD0 * fD0) + (2.0f * fE0 * fE1));
        kP0.setCoeff(4, (2.0f * ((fC0 * fC1) + (fD0 * fD1) + (fE0 * fE1))) + (fE1 * fE1));
        kP0.setCoeff(5, (fC1 * fC1) + (fD1 * fD1) + (2.0f * fE1 * fE2));
        kP0.setCoeff(6, fE2 * fE2);

        MjPolynomial1f kQ0 = new MjPolynomial1f(1);
        kQ0.setCoeff(0, fR0Sqr);
        kQ0.setCoeff(1, 1.0f);

        MjPolynomial1f kQ1 = new MjPolynomial1f(1);
        kQ1.setCoeff(0, fR1Sqr);
        kQ1.setCoeff(1, 1.0f);

        MjPolynomial1f kQ2 = new MjPolynomial1f(1);
        kQ2.setCoeff(0, fR2Sqr);
        kQ2.setCoeff(1, 1.0f);

        float fTmp = fAreaFraction * (float) Math.PI;
        float fAmp = fTmp * fTmp;
        MjPolynomial1f kP1 = new MjPolynomial1f();
        kP1.set(kQ0);
        kP1.scale(fAmp);
        kP1.mul(kP1, kQ0);
        kP1.mul(kP1, kQ0);
        kP1.mul(kP1, kQ0);
        kP1.mul(kP1, kQ1);
        kP1.mul(kP1, kQ1);
        kP1.mul(kP1, kQ2);
        kP1.mul(kP1, kQ2);

        MjPolynomial1f kFinal = new MjPolynomial1f();
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
        MjVector3f kVDiff = new MjVector3f();
        MjVector3f kDDiff = new MjVector3f();

        float fWeight = 0.1f;

        /* compute the gradient of the energy function */
        MjVector3f[] akDJDX = new MjVector3f[m_iVQuantity];
        MjVector3f kSumSqr = new MjVector3f(MjVector3f.ZERO);
        float fError = 0.0f;

        for (int i0 = 0; i0 < m_iVQuantity; i0++) {
            akDJDX[i0] = new MjVector3f(MjVector3f.ZERO);

            Vertex kVertex = m_akVertex[i0];

            for (int j = 0; j < kVertex.VQuantity; j++) {
                int i1 = kVertex.V[j];

                kVDiff.sub(m_akPoint[i0], m_akPoint[i1]);

                float fC = kVDiff.lengthSquared();

                MjEdgeKey kKey = new MjEdgeKey(i0, i1);
                assert (m_kInitDistance.containsKey(kKey));

                Float kValue = (Float) m_kInitDistance.get(kKey);
                assert (null != kValue);

                float fValue = kValue.floatValue();
                float fTmp0 = fC - (fValue * fValue);
                float fTmp1 = 1.0f + (fWeight * fTmp0);

                fError += (2.0f * fC) + (fWeight * fTmp0 * fTmp0);
                akDJDX[i0].scaleAdd(fTmp1, kVDiff, akDJDX[i0]);
            }

            kSumSqr.add(akDJDX[i0]);

            /* update average convexity */
            m_afAvrConvexity[i0] += m_akNormal[i0].dot(akDJDX[i0]);
        }

        float fInvQuantity = 1.0f / m_iVQuantity;
        fError *= fInvQuantity;

        float fGradLength = kSumSqr.length();

        if (fGradLength < MjMathf.EPSILON) {
            return fError;
        }

        if (fGradLength > 1.0f) {
            float fInvGradLength = 1.0f / fGradLength;

            for (int i = 0; i < m_iVQuantity; i++) {
                akDJDX[i].scale(fInvGradLength);
            }
        }

        /* Compute the energy function along the line X+t*D where X is the */
        /* total vector of vertex positions and D is the normalized gradient */
        /* vector. */
        MjPolynomial1f kPoly = new MjPolynomial1f(4);
        kPoly.setCoeff(0, fError);

        for (int j = 1; j <= 4; j++) {
            kPoly.setCoeff(j, 0.0f);
        }

        for (int i0 = 0; i0 < m_iVQuantity; i0++) {
            Vertex kVertex = m_akVertex[i0];

            for (int j = 0; j < kVertex.VQuantity; j++) {
                int i1 = kVertex.V[j];

                kVDiff.sub(m_akPoint[i0], m_akPoint[i1]);
                kDDiff.sub(akDJDX[i0], akDJDX[i1]);

                float fA = kDDiff.lengthSquared();
                float fB = kDDiff.dot(kVDiff);
                float fC = kVDiff.lengthSquared();

                MjEdgeKey kKey = new MjEdgeKey(i0, i1);
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

        MjPolynomial1f kDPoly = kPoly.getDerivative();

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

            for (int i = 0; i < m_iVQuantity; i++) {
                m_akPoint[i].scaleAdd(fH, akDJDX[i], m_akPoint[i]);
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
    private Point3f getBarycentric(Point2f kP, int i) {

        /* get barycentric coordinates */
        Triangle kT = m_akTriangle[i];
        Point2f kV0 = m_akPlane[kT.V[0]];
        Point2f kV1 = m_akPlane[kT.V[1]];
        Point2f kV2 = m_akPlane[kT.V[2]];
        MjVector2f kE10 = new MjVector2f();
        kE10.sub(kV1, kV0);

        MjVector2f kE20 = new MjVector2f();
        kE20.sub(kV2, kV0);

        MjVector2f kDiff = new MjVector2f();
        kDiff.sub(kP, kV0);

        float fA00 = kE10.dot(kE10);
        float fA01 = kE10.dot(kE20);
        float fA11 = kE20.dot(kE20);
        float fInvDet = 1.0f / ((fA00 * fA11) - (fA01 * fA01));
        float fB0 = kE10.dot(kDiff);
        float fB1 = kE20.dot(kDiff);

        Point3f kBary = new Point3f();
        kBary.y = ((fA11 * fB0) - (fA01 * fB1)) * fInvDet;
        kBary.z = ((fA00 * fB1) - (fA01 * fB0)) * fInvDet;
        kBary.x = 1.0f - kBary.y - kBary.z;

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
    private int getContainingTriangle(Point2f kP, int iStart) {

        /* The search should never iterate over more than all the triangles... */
        MjPrimitive.Ray2f kRay = new MjPrimitive.Ray2f();
        MjPrimitive.Segment2f kSeg = new MjPrimitive.Segment2f();
        Point2f[] akVertex = new Point2f[3];

        for (int i = 0; i < m_iTQuantity; i++) {

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
            kRay.m_kOrigin.set(0.0f, 0.0f);
            kRay.m_kOrigin.add(akVertex[0]);
            kRay.m_kOrigin.add(akVertex[1]);
            kRay.m_kOrigin.add(akVertex[2]);
            kRay.m_kOrigin.scale(1.0f / 3.0f);
            kRay.m_kDirection.sub(kP, kRay.m_kOrigin);
            kRay.m_kDirection.normalizeSafe();

            int i0, i1;

            for (i0 = 2, i1 = 0; i1 < 3; i0 = i1++) {
                kSeg.m_kOrigin.set(akVertex[i0]);
                kSeg.m_kDirection.sub(akVertex[i1], akVertex[i0]);

                float[] afT = new float[2]; /* T[0] = ray, T[1] = segment */
                int iQuantity = MjPrimitive.findIntersection(kRay, kSeg, afT);

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
        Point3f[] akMVertex;

        /** Array of plane points. */
        Point3f[] akPVertex;

        /** Array of sphere points. */
        Point3f[] akSVertex;
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
