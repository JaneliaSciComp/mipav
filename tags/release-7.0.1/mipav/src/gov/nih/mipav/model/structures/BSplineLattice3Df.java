package gov.nih.mipav.model.structures;


import WildMagic.LibFoundation.Curves.*;
import WildMagic.LibFoundation.Mathematics.*;


/**
 * This class manages a 3D lattice of control points given a discretized B-Spline basis for each axis.
 */
public class BSplineLattice3Df {

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** DOCUMENT ME! */
    protected Vector3f[][][] m_aaakControlPoint; // [iControlZ][iControlY][iControlX]

    /** DOCUMENT ME! */
    protected BSplineBasisDiscretef m_kBasisX; // X axis B-Spline basis.

    /** DOCUMENT ME! */
    protected BSplineBasisDiscretef m_kBasisY; // Y axis B-Spline basis.

    /** DOCUMENT ME! */
    protected BSplineBasisDiscretef m_kBasisZ; // Z axis B-Spline basis.

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Create 3D lattice of control points given a discretized B-Spline basis for each axis. Initially, all of the
     * control points are equally spaced.
     *
     * @param  kBasisX  BSplineBasisDiscretef B-Spline basis for the X axis.
     * @param  kBasisY  BSplineBasisDiscretef B-Spline basis for the Y axis.
     * @param  kBasisZ  BSplineBasisDiscretef B-Spline basis for the Z axis.
     */
    public BSplineLattice3Df(BSplineBasisDiscretef kBasisX, BSplineBasisDiscretef kBasisY,
                             BSplineBasisDiscretef kBasisZ) {

        m_kBasisX = kBasisX;
        m_kBasisY = kBasisY;
        m_kBasisZ = kBasisZ;

        int iNumControlX = kBasisX.GetNumCtrlPoints();
        int iNumControlY = kBasisY.GetNumCtrlPoints();
        int iNumControlZ = kBasisZ.GetNumCtrlPoints();

        // Allocate the equally spaced control points.
        m_aaakControlPoint = new Vector3f[iNumControlZ][iNumControlY][iNumControlX];

        for (int iControlX = 0; iControlX < iNumControlX; iControlX++) {
            float fX = iControlX / (iNumControlX - 1.0f);

            for (int iControlY = 0; iControlY < iNumControlY; iControlY++) {
                float fY = iControlY / (iNumControlY - 1.0f);

                for (int iControlZ = 0; iControlZ < iNumControlZ; iControlZ++) {
                    float fZ = iControlZ / (iNumControlZ - 1.0f);
                    m_aaakControlPoint[iControlZ][iControlY][iControlX] = new Vector3f(fX, fY, fZ);
                }
            }
        }
    }

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * Create an image of the specified dimensions and fill it to contain a mapping of the coordinates based on the 3D
     * lattice of B-spline control points.
     *
     * @param   iSizeX  int Number of samples along x axis of image.
     * @param   iSizeY  int Number of samples along y axis of image.
     * @param   iSizeZ  int Number of samples along z axis of image.
     *
     * @return  ModelSimpleImage[] Array of three ModelSimpleImage instances. The first one contains the x coordinate,
     *          the second contains the y coordinate for the map, and the third contains the z coordinate for the map.
     *          Each image has default resolutions of 1 for each axis.
     */
    public ModelSimpleImage[] createImageMap(int iSizeX, int iSizeY, int iSizeZ) {

        int[] aiExtents = new int[] { iSizeX, iSizeY, iSizeZ };
        ModelSimpleImage[] akImageMap = new ModelSimpleImage[] {
                                            new ModelSimpleImage(aiExtents), new ModelSimpleImage(aiExtents),
                                            new ModelSimpleImage(aiExtents)
                                        };
        int iNumSamplesX = iSizeX;
        int iNumSamplesXY = iSizeX * iSizeY;

        Vector3f kPos = new Vector3f();

        for (int iZ = 0; iZ < iSizeZ; iZ++) {

            for (int iY = 0; iY < iSizeY; iY++) {

                for (int iX = 0; iX < iSizeX; iX++) {

                    getPosition(iX, iY, iZ, kPos);

                    int iIndex = iX + (iNumSamplesX * iY) + (iNumSamplesXY * iZ);
                    akImageMap[0].data[iIndex] = kPos.X;
                    akImageMap[1].data[iIndex] = kPos.Y;
                    akImageMap[2].data[iIndex] = kPos.Z;
                }
            }
        }

        return akImageMap;
    }

    /**
     * Cleanup memory.
     *
     * @throws  Throwable  DOCUMENT ME!
     */
    public void finalize() throws Throwable {

        if (null != m_aaakControlPoint) {

            for (int i = 0; i < m_aaakControlPoint.length; i++) {

                for (int j = 0; j < m_aaakControlPoint[i].length; j++) {
                    m_aaakControlPoint[i][j] = null;
                }

                m_aaakControlPoint[i] = null;
            }

            m_aaakControlPoint = null;
        }

        m_kBasisX = null;
        m_kBasisY = null;
        m_kBasisZ = null;

        super.finalize();
    }

    /**
     * Return the discretized B-Spline basis for the X axis.
     *
     * @return  BSplineBasisDiscretef B-Spline basis.
     */
    public BSplineBasisDiscretef getBasisX() {
        return m_kBasisX;
    }

    /**
     * Return the discretized B-Spline basis for the Y axis.
     *
     * @return  BSplineBasisDiscretef B-Spline basis.
     */
    public BSplineBasisDiscretef getBasisY() {
        return m_kBasisY;
    }

    /**
     * Return the discretized B-Spline basis for the Z axis.
     *
     * @return  BSplineBasisDiscretef B-Spline basis.
     */
    public BSplineBasisDiscretef getBasisZ() {
        return m_kBasisZ;
    }

    /**
     * Retrieve the position of the specified control point.
     *
     * @param  iControlX  int Identifies control point in 3D lattice.
     * @param  iControlY  int Identifies control point in 3D lattice.
     * @param  iControlZ  int Identifies control point in 3D lattice.
     * @param  kPoint     Point3f Where the current coordinates of the control point will be stored upon return.
     */
    public void getControlPoint(int iControlX, int iControlY, int iControlZ, Vector3f kPoint) {

        if ((0 <= iControlX) && (iControlX < m_kBasisX.GetNumCtrlPoints()) && (0 <= iControlY) &&
                (iControlY < m_kBasisY.GetNumCtrlPoints()) && (0 <= iControlZ) &&
                (iControlZ < m_kBasisZ.GetNumCtrlPoints())) {

            kPoint.copy(m_aaakControlPoint[iControlZ][iControlY][iControlX]);
        }
    }

    /**
     * Return the B-Spline evaluation for the given input values. The evaluation is based on the 3D evaluation of the
     * B-Spline basis (one for each axis) and the 3D lattice of control points.
     *
     * @param  iSampleX  int Sample index along X axis which determines an input value in the [0,1] for that axis'
     *                   B-Spline basis evaluation.
     * @param  iSampleY  int Sample index along Y axis which determines an input value in the [0,1] for that axis'
     *                   B-Spline basis evaluation.
     * @param  iSampleZ  int Sample index along Z axis which determines an input value in the [0,1] for that axis'
     *                   B-Spline basis evaluation.
     * @param  kPos      Point3f 3D coordinates resulting from evaluation.
     */
    public void getPosition(int iSampleX, int iSampleY, int iSampleZ, Vector3f kPos) {

        float fX = (float) iSampleX / (float) (m_kBasisX.GetNumSamples() - 1);
        int iControlMaxX = m_kBasisX.GetKnotIndex(fX);
        int iControlMinX = iControlMaxX - m_kBasisX.GetDegree();

        float fY = (float) iSampleY / (float) (m_kBasisY.GetNumSamples() - 1);
        int iControlMaxY = m_kBasisY.GetKnotIndex(fY);
        int iControlMinY = iControlMaxY - m_kBasisY.GetDegree();

        float fZ = (float) iSampleZ / (float) (m_kBasisZ.GetNumSamples() - 1);
        int iControlMaxZ = m_kBasisZ.GetKnotIndex(fZ);
        int iControlMinZ = iControlMaxZ - m_kBasisZ.GetDegree();

        kPos.set(0.0f, 0.0f, 0.0f);
        for (int iControlX = iControlMinX; iControlX <= iControlMaxX; iControlX++) {

            for (int iControlY = iControlMinY; iControlY <= iControlMaxY; iControlY++) {

                for (int iControlZ = iControlMinZ; iControlZ <= iControlMaxZ; iControlZ++) {
                    float fTmp = m_kBasisX.GetD0(iControlX, iSampleX) * m_kBasisY.GetD0(iControlY, iSampleY) *
                                     m_kBasisZ.GetD0(iControlZ, iSampleZ);
                    kPos.scaleAdd( fTmp, m_aaakControlPoint[iControlZ][iControlY][iControlX], kPos );
                }
            }
        }
    }

    /**
     * Return the B-Spline evaluation for the given input values. The evaluation is based on the 3D evaluation of the
     * B-Spline basis (one for each axis) and the 3D lattice of control points. This method is used for interpolating
     * between the discrete samples.
     *
     * @param  fX    float Sample in [0,1] range for the X axis evaluation of the B-Spline basis.
     * @param  fY    float Sample in [0,1] range for the Y axis evaluation of the B-Spline basis.
     * @param  fZ    float Sample in [0,1] range for the Z axis evaluation of the B-Spline basis.
     * @param  kPos  Point3f 3D coordinates resulting from evaluation.
     */
    public void getPosition(float fX, float fY, float fZ, Vector3f kPos) {

        int iControlMaxX = m_kBasisX.GetKnotIndex(fX);
        int iControlMinX = iControlMaxX - m_kBasisX.GetDegree();

        int iControlMaxY = m_kBasisY.GetKnotIndex(fY);
        int iControlMinY = iControlMaxY - m_kBasisY.GetDegree();

        int iControlMaxZ = m_kBasisZ.GetKnotIndex(fZ);
        int iControlMinZ = iControlMaxZ - m_kBasisZ.GetDegree();

        float[] afD0X = new float[m_kBasisX.GetNumCtrlPoints()];
        float[] afD0Y = new float[m_kBasisY.GetNumCtrlPoints()];
        float[] afD0Z = new float[m_kBasisZ.GetNumCtrlPoints()];

        m_kBasisX.Compute(fX, afD0X, null, null);
        m_kBasisY.Compute(fY, afD0Y, null, null);
        m_kBasisZ.Compute(fZ, afD0Z, null, null);

        kPos.set(0.0f, 0.0f, 0.0f);
        for (int iControlX = iControlMinX; iControlX <= iControlMaxX; iControlX++) {

            for (int iControlY = iControlMinY; iControlY <= iControlMaxY; iControlY++) {

                for (int iControlZ = iControlMinZ; iControlZ <= iControlMaxZ; iControlZ++) {
                    float fTmp = afD0X[iControlX] * afD0Y[iControlY] * afD0Z[iControlZ];
                    kPos.scaleAdd( fTmp, m_aaakControlPoint[iControlZ][iControlY][iControlX], kPos );
                }
            }
        }

        afD0X = null;
        afD0Y = null;
        afD0Z = null;
    }

    /**
     * Set the position of the specified control point. No checks are made on the input coordinates of the control
     * point.
     *
     * @param  iControlX  int Identifies control point in 3D lattice.
     * @param  iControlY  int Identifies control point in 3D lattice.
     * @param  iControlZ  int Identifies control point in 3D lattice.
     * @param  kPoint     Point3f New coordinates of the control point.
     */
    public void setControlPoint(int iControlX, int iControlY, int iControlZ, Vector3f kPoint) {

        if ((0 <= iControlX) && (iControlX < m_kBasisX.GetNumCtrlPoints()) && (0 <= iControlY) &&
                (iControlY < m_kBasisY.GetNumCtrlPoints()) && (0 <= iControlZ) &&
                (iControlZ < m_kBasisZ.GetNumCtrlPoints())) {

            m_aaakControlPoint[iControlZ][iControlY][iControlX].copy(kPoint);
        }
    }
}
