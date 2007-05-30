package gov.nih.mipav.model.structures;


import javax.vecmath.*;


/**
 * This class manages a 2D lattice of control points given a discretized B-Spline basis for each axis.
 */
public class BSplineLattice2Df {

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** DOCUMENT ME! */
    protected Point2f[][] m_aakControlPoint; // [iNumCtrlPointsY][iNumCtrlPointsX]

    /** DOCUMENT ME! */
    protected BSplineBasisDiscretef m_kBasisX; // X axis B-Spline basis

    /** DOCUMENT ME! */
    protected BSplineBasisDiscretef m_kBasisY; // Y axis B-Spline basis

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Create 2D lattice of control points given a discretized B-Spline basis for each axis. Initially, all of the
     * control points are equally spaced.
     *
     * @param  kBasisX  BSplineBasisDiscretef B-Spline basis for the X axis.
     * @param  kBasisY  BSplineBasisDiscretef B-Spline basis for the Y axis.
     */
    public BSplineLattice2Df(BSplineBasisDiscretef kBasisX, BSplineBasisDiscretef kBasisY) {
        m_kBasisX = kBasisX;
        m_kBasisY = kBasisY;

        int iNumControlX = kBasisX.getNumControlPoints();
        int iNumControlY = kBasisY.getNumControlPoints();

        // Allocate the equally spaced control points.
        m_aakControlPoint = new Point2f[iNumControlY][iNumControlX];

        for (int iControlX = 0; iControlX < iNumControlX; iControlX++) {
            float fX = iControlX / (iNumControlX - 1.0f);

            for (int iControlY = 0; iControlY < iNumControlY; iControlY++) {
                float fY = iControlY / (iNumControlY - 1.0f);
                m_aakControlPoint[iControlY][iControlX] = new Point2f(fX, fY);
            }
        }
    }

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * Create an image of the specified dimensions and fill it to contain a mapping of the coordinates based on the 2D
     * lattice of B-spline control points.
     *
     * @param   iSizeX  int Number of samples along x axis of image.
     * @param   iSizeY  int Number of samples along y axis of image.
     *
     * @return  ModelSimpleImage[] Array of two ModelSimpleImage instances. The first one contains the x coordinate and
     *          the second contains the y coordinate for the map. Each image has default resolutions of 1 for each axis.
     */
    public ModelSimpleImage[] createImageMap(int iSizeX, int iSizeY) {

        int[] aiExtents = new int[] { iSizeX, iSizeY };
        ModelSimpleImage[] akImageMap = new ModelSimpleImage[] {
                                            new ModelSimpleImage(aiExtents), new ModelSimpleImage(aiExtents)
                                        };
        int iNumSamplesX = iSizeX;

        Point2f kPos = new Point2f();

        for (int iY = 0; iY < iSizeY; iY++) {

            for (int iX = 0; iX < iSizeX; iX++) {

                getPosition(iX, iY, kPos);

                int iIndex = iX + (iY * iNumSamplesX);
                akImageMap[0].data[iIndex] = kPos.x;
                akImageMap[1].data[iIndex] = kPos.y;
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

        if (null != m_aakControlPoint) {

            for (int i = 0; i < m_aakControlPoint.length; i++) {
                m_aakControlPoint[i] = null;
            }

            m_aakControlPoint = null;
        }

        m_kBasisX = null;
        m_kBasisY = null;

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
     * Retrieve the position of the specified control point.
     *
     * @param  iControlX  int Identifies control point in 2D lattice.
     * @param  iControlY  int Identifies control point in 2D lattice.
     * @param  kPoint     Point2f Where the current coordinates of the control point will be stored upon return.
     */
    public void getControlPoint(int iControlX, int iControlY, Point2f kPoint) {

        if ((0 <= iControlX) && (iControlX < m_kBasisX.getNumControlPoints()) && (0 <= iControlY) &&
                (iControlY < m_kBasisY.getNumControlPoints())) {

            kPoint.set(m_aakControlPoint[iControlY][iControlX]);
        }
    }

    /**
     * Return the B-Spline evaluation for the given input values. The evaluation is based on the 2D evaluation of the
     * B-Spline basis (one for each axis) and the 2D lattice of control points.
     *
     * @param  iSampleX  int Sample index along X axis which determines an input value in the [0,1] for that axis'
     *                   B-Spline basis evaluation.
     * @param  iSampleY  int Sample index along Y axis which determines an input value in the [0,1] for that axis'
     *                   B-Spline basis evaluation.
     * @param  kPos      Point2f 2D coordinates resulting from evaluation.
     */
    public void getPosition(int iSampleX, int iSampleY, Point2f kPos) {
        float fX = (float) iSampleX / (float) (m_kBasisX.getNumSamples() - 1);
        int iControlMaxX = m_kBasisX.getKnotIndex(fX);
        int iControlMinX = iControlMaxX - m_kBasisX.getDegree();

        float fY = (float) iSampleY / (float) (m_kBasisY.getNumSamples() - 1);
        int iControlMaxY = m_kBasisY.getKnotIndex(fY);
        int iControlMinY = iControlMaxY - m_kBasisY.getDegree();

        kPos.set(0.0f, 0.0f);

        for (int iControlX = iControlMinX; iControlX <= iControlMaxX; iControlX++) {

            for (int iControlY = iControlMinY; iControlY <= iControlMaxY; iControlY++) {
                float fTmp = m_kBasisX.getD0(iControlX, iSampleX) * m_kBasisY.getD0(iControlY, iSampleY);
                kPos.scaleAdd(fTmp, m_aakControlPoint[iControlY][iControlX], kPos);
            }
        }
    }

    /**
     * Return the B-Spline evaluation for the given input values. The evaluation is based on the 2D evaluation of the
     * B-Spline basis (one for each axis) and the 2D lattice of control points. This method is used for interpolating
     * between the discrete samples.
     *
     * @param  fX    float Sample in [0,1] range for the X axis evaluation of the B-Spline basis.
     * @param  fY    float Sample in [0,1] range for the Y axis evaluation of the B-Spline basis.
     * @param  kPos  Point2f 2D coordinates resulting from evaluation.
     */
    public void getPosition(float fX, float fY, Point2f kPos) {

        int iControlMaxX = m_kBasisX.getKnotIndex(fX);
        int iControlMinX = iControlMaxX - m_kBasisX.getDegree();

        int iControlMaxY = m_kBasisY.getKnotIndex(fY);
        int iControlMinY = iControlMaxY - m_kBasisY.getDegree();

        float[] afD0X = new float[m_kBasisX.getNumControlPoints()];
        float[] afD0Y = new float[m_kBasisY.getNumControlPoints()];

        m_kBasisX.compute(fX, afD0X, null, null);
        m_kBasisY.compute(fY, afD0Y, null, null);

        kPos.set(0.0f, 0.0f);

        for (int iControlX = iControlMinX; iControlX <= iControlMaxX; iControlX++) {

            for (int iControlY = iControlMinY; iControlY <= iControlMaxY; iControlY++) {
                float fTmp = afD0X[iControlX] * afD0Y[iControlY];
                kPos.scaleAdd(fTmp, m_aakControlPoint[iControlY][iControlX], kPos);
            }
        }

        afD0X = null;
        afD0Y = null;
    }

    /**
     * Set the position of the specified control point. No checks are made on the input coordinates of the control
     * point.
     *
     * @param  iControlX  int Identifies control point in 2D lattice.
     * @param  iControlY  int Identifies control point in 2D lattice.
     * @param  kPoint     Point2f New coordinates of the control point.
     */
    public void setControlPoint(int iControlX, int iControlY, Point2f kPoint) {

        if ((0 <= iControlX) && (iControlX < m_kBasisX.getNumControlPoints()) && (0 <= iControlY) &&
                (iControlY < m_kBasisY.getNumControlPoints())) {

            m_aakControlPoint[iControlY][iControlX].set(kPoint);
        }
    }
}
