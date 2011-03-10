package gov.nih.mipav.view.renderer.J3D.surfaceview.flythruview;

import WildMagic.LibFoundation.Curves.*;
import gov.nih.mipav.view.renderer.flythroughview.*;
import gov.nih.mipav.view.renderer.J3D.*;
import gov.nih.mipav.view.renderer.J3D.model.structures.*;

import javax.swing.*;

import javax.vecmath.*;


/**
 * DOCUMENT ME!
 */
public class ModelTriangleMeshCurveSegments {

    //~ Static fields/initializers -------------------------------------------------------------------------------------

    /** DOCUMENT ME! */
    public static final float fCURVE_CUTOFF = (float) (Math.PI / 4.0);

    /** DOCUMENT ME! */
    public static final float f10_DEGREES = (float) (Math.PI / 18.0);

    /** DOCUMENT ME! */
    public static final float fDISTANCE_SCALE_FACTOR = (float) 1.5;

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** The array of indexed triangle array coordinate indices. One per curve segment. */
    private int[][][] m_aaaiConnectivity;

    /** The aray of points (defined in time) where the curvature of the spline is highest. */
    private float[][] m_aafBreakPoints;

    /** Approximate "width" of the tube at the sample points on each curve. */
    private float[][] m_aafMaxDist;

    /**
     * Values that are used in multiple functions, are stored so that they are calculated only once. Relative position
     * on the curve
     */
    private float[][] m_aafRelativeLengths;

    /** The number of triangles in each sub-mesh. */
    private int[][] m_aaiNumTris;

    /** Positions on the sample curve. */
    private Point3f[][] m_aakCurvePos;

    /** Tangent vectors for each position on the sample curve. */
    private Vector3f[][] m_aakTangent;

    /** The number of break points on the spline. */
    private int[] m_aiNumBreakPoints;

    /** The array of possible curves to use for segmenting the mesh. */
    private Curve3f[] m_akCurvePosition;

    /** The number of curves. */
    private int m_iNumCurves;

    /** DOCUMENT ME! */
    private FlyPathGraphCurve m_kGraphCurve;

    /** DOCUMENT ME! */
    private FlyPathGraphSamples m_kGraphSamples;

    /** The input triangle mesh to be segmented. */
    private ModelTriangleMesh m_kMesh;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Creates a new ModelTriangleMeshCurveSegments object.
     *
     * @param  kMesh                        ModelTriangleMesh
     * @param  kGraphSamples                FlyPathGraphSamples
     * @param  kGraphCurve                  FlyPathGraphCurve
     * @param  iPathSamplesReductionFactor  int
     *
     *                                      <p>The ModelTriangleMeshCurveSegments constructor takes a ModelTriangleMesh,
     *                                      a list of Curves, and the point samples for those curves and produces a
     *                                      collection of sub-meshes smaller than the original mesh. The sub-meshes are
     *                                      constructed such that when the viewpoint is inside one of the sub-meshes
     *                                      only the triangles in that sum-mesh are visible. Allowing the program to
     *                                      send a smaller number of triangles through the graphics piplines will
     *                                      increase performance for large triangle meshes.</p>
     *
     *                                      <p>The Algorthim involves three steps:</p>
     *
     *                                      <p>1). First each curve is divided into segments based on curvature and the
     *                                      length of the segments. Breaking the curves at places of high curvature is a
     *                                      heuristic that works because the TriangleMesh is assumed to be a tube, for
     *                                      example the colon or brachia in the lung. If the tube bends, then triangles
     *                                      on the far side of the curve are not visible.</p>
     *
     *                                      <p>2). Second the radial distance from the curve through the TriangleMesh
     *                                      tube is calculated for each position on the curve. This distance is used to
     *                                      determine the maximum width of the tube for each position on the FlyPath
     *                                      curve.</p>
     *
     *                                      <p>3). Third, the triangles are distributed into sub-meshes. Each sub-mesh
     *                                      represents the visible triangles along each of the curve segments. Because
     *                                      triangles associated with the adjacent curve segments are visible at the
     *                                      start and end of each curve segment, each sub-mesh contains the triangles
     *                                      for the previous curve segment, the current curve segment, and the next
     *                                      curve segment. If a branch occurs in the current curve segment, the
     *                                      triangles associated with the first segment on that branch curve are also
     *                                      included in the sub-mesh.</p>
     */
    public ModelTriangleMeshCurveSegments(ModelTriangleMesh kMesh, FlyPathGraphSamples kGraphSamples,
                                          FlyPathGraphCurve kGraphCurve, int iPathSamplesReductionFactor) {

        /* Remember these inputs.
         */
        m_kMesh = kMesh;
        m_kGraphCurve = kGraphCurve;
        m_kGraphSamples = kGraphSamples;
        m_akCurvePosition = kGraphCurve.getArrayCurvePosition();
        m_iNumCurves = m_akCurvePosition.length;

        /* Segment the FlyPath Curve based on curvature To maintain accuracy
         * of the curve segments, the FlyPath is not down-sampled.
         */
        segmentCurvesByTangent();

        JProgressBar rendererProgressBar = ViewJFrameVolumeView.getRendererProgressBar();

        rendererProgressBar.setValue(60);
        rendererProgressBar.update(rendererProgressBar.getGraphics());

        /* Find the maximum radial tube width at each sampled point on the
         * FlyPath.
         */
        findMaxMeshDistance(iPathSamplesReductionFactor);

        rendererProgressBar.setValue(70);
        rendererProgressBar.update(rendererProgressBar.getGraphics());

        /* Segment the triangle mesh into smaller meshes based on the
         * curvature of the FlyPath using the maximum tube widths to create bounding spherical volumes for the
         * segmentation.
         */
        segmentMesh(iPathSamplesReductionFactor);

        rendererProgressBar.setValue(75);
        rendererProgressBar.update(rendererProgressBar.getGraphics());

        /* Delete all member variables not necessary for returning the correct
         * sub-mesh. */
        cleanUp();
    }

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * Delete all member variables needed for the mesh segmentation, but not needed for getTriangleArrayIndices.
     */
    public void cleanUp() {
        m_aaiNumTris = null;
        m_aafRelativeLengths = null;
        m_aafMaxDist = null;
        ;
        m_aakCurvePos = null;
        m_aakTangent = null;
    }

    /**
     * DOCUMENT ME!
     */
    public void dispose() {
        m_kMesh = null;
        m_akCurvePosition = null;
        m_aaaiConnectivity = null;


        m_kGraphSamples = null;
        m_kGraphCurve = null;

        m_aafBreakPoints = null;
        m_aiNumBreakPoints = null;

        m_aaiNumTris = null;

        m_aafRelativeLengths = null;

    }

    /**
     * DOCUMENT ME!
     */
    public void finalize() {
        dispose();
    }

    /**
     * DOCUMENT ME!
     *
     * @param   iCurve  int
     *
     * @return  Curve3
     */
    public Curve3f getCurve(int iCurve) {
        return m_akCurvePosition[iCurve];
    }

    /**
     * DOCUMENT ME!
     *
     * @param   iCurve  int
     * @param   fT      float
     *
     * @return  int[] getTriangleArrayIndices, returns the submesh associated with the Curve = iCurve and the relative
     *          position on that curve equal to fT. If
     */
    public int[] getTriangleArrayIndices(int iCurve, float fT) {

        /* Check that the Curve = iCurve exists:
         */
        if ((iCurve >= 0) && (iCurve < m_iNumCurves)) {

            /* Find and return the sub-mesh associated with iCurve and the
             * relative position, fT, on the curve:
             */
            int iSegment = findCurveSegment(iCurve, fT);

            return m_aaaiConnectivity[iCurve][iSegment];
        }

        /* No curve to return - return null;
         */
        return null;
    }

    /**
     * DOCUMENT ME!
     *
     * @return  ModelTriangleMesh
     */
    public ModelTriangleMesh getTriangleMesh() {
        return m_kMesh;
    }

    /**
     * DOCUMENT ME!
     *
     * @param   iCurve             DOCUMENT ME!
     * @param   fRelativeDistance  DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    protected int findCurveSegment(int iCurve, float fRelativeDistance) {

        /* Bound check for less than or equal to 0.0
         */
        if (fRelativeDistance <= 0.0) {
            return 0;
        }
        /* Bound check for greater than or equal to 1.0
         */
        else if (fRelativeDistance >= 1.0) {
            return (m_aiNumBreakPoints[iCurve] - 2);
        }

        /* Loop over the number of segments in this curve
         * until the interval containing the value of fRelativeDistance is found. Return the index of that interval.
         */
        for (int iBreakPt = 0; iBreakPt < (m_aiNumBreakPoints[iCurve] - 1); iBreakPt++) {

            if ((fRelativeDistance >= m_aafBreakPoints[iCurve][iBreakPt]) &&
                    (fRelativeDistance < m_aafBreakPoints[iCurve][iBreakPt + 1])) {
                return iBreakPt;
            }
        }

        return 0;
    }

    /**
     * DOCUMENT ME!
     *
     * @param  iDownSample  DOCUMENT ME!
     */
    protected void findMaxMeshDistance(int iDownSample) {

        /* All Temporary variables are initialized outside the loops for
         * performance
         */
        WildMagic.LibFoundation.Mathematics.Vector3f[] akSamplePos;

        float fDist;
        float fRelativeLength;

        /*
         * The four minimum distance values in each of the four directions perpendicular to the tangent (viewing)
         * direction.
         */
        float fMinN = Float.MAX_VALUE;
        float fMinNI = Float.MAX_VALUE;
        float fMinS = Float.MAX_VALUE;
        float fMinSI = Float.MAX_VALUE;

        /*
         * The four vectors perpendicular to the tangent (viewing) direction along the FlyPath curve.
         */
        Vector3f kNormal = new Vector3f(0, 0, 0);
        Vector3f kNormalInv = new Vector3f(0, 0, 0);
        Vector3f kSide = new Vector3f(0, 0, 0);
        Vector3f kSideInv = new Vector3f(0, 0, 0);
        Vector3f kRay = new Vector3f(0, 0, 0);

        float fAngle;

        int iVertexCount = m_kMesh.getVertexCount();
        Point3f kVertex0 = new Point3f((float) 0, (float) 0, (float) 0);

        /* Temporary aagMaxDist array. The final array of maximum distances is
         * taken as the average of the three distances i-1, i, i+1
         */
        float[][] aafMaxDist = new float[m_iNumCurves][];
        m_aafMaxDist = new float[m_iNumCurves][];

        float fMaxTemp1 = 0;
        float fMaxTemp2 = 0;

        /*
         * Loop over every curve in the FlyPath
         */
        for (int iCurve = 0; iCurve < m_iNumCurves; iCurve++) {

            /* Store the sample path positions for this Curve and the maximum
             * boundary distances from those path positions to the TriangleMesh data.
             */
            akSamplePos = m_kGraphSamples.getArrayPointPosition(iCurve);
            aafMaxDist[iCurve] = new float[akSamplePos.length];
            m_aafMaxDist[iCurve] = new float[akSamplePos.length];

            /*
             * ViewJSimpleProgressBar kProgress =  new ViewJSimpleProgressBar("Segmenting Triangle Mesh",
             *              "Finding Distance Metrics ..."); kProgress.setRange((float)0, (float)akSamplePos.length);
             */
            /* Loop over every sample point on the Curve, downsampling is an
             * option based on user-input.
             */
            for (int iSample = 1; iSample < akSamplePos.length; iSample += iDownSample) {

                /* Find the relative position on the curve
                 */
                fRelativeLength = m_aafRelativeLengths[iCurve][iSample];

                /* The Normal direction perpendicular to the tangent at the
                 * position on the curve.
                 */
                WildMagic.LibFoundation.Mathematics.Vector3f kVec = m_akCurvePosition[iCurve].GetSecondDerivative(fRelativeLength);
                kNormal = new Vector3f(kVec.X, kVec.Y, kVec.Z);

                if (kNormal.lengthSquared() > 0.0f) {
                    kNormal.normalize();
                }

                /* Inverse Normal
                 */
                kNormalInv.x = -kNormal.x;
                kNormalInv.y = -kNormal.y;
                kNormalInv.z = -kNormal.z;

                /* The directions to the "side" are calculated as the
                 * crossproduct between the tangent direction and the normal direction.
                 */
                kSide.cross(kNormal, m_aakTangent[iCurve][iSample]);
                kSideInv.x = -kSide.x;
                kSideInv.y = -kSide.y;
                kSideInv.z = -kSide.z;

                /* The minimum distance values in each of the four
                 * perpendicular direction are initialized to the maximum float value.
                 */
                fMinN = Float.MAX_VALUE;
                fMinNI = Float.MAX_VALUE;
                fMinS = Float.MAX_VALUE;
                fMinSI = Float.MAX_VALUE;

                /* Loop over all vertices in the TriangleMesh, find the
                 * minimum distance between the point on the FlyPath curve to each vertex in the four perpendicular
                 * directions.
                 */
                for (int iVertex = 0; iVertex < iVertexCount; iVertex++) {
                    m_kMesh.getCoordinate(iVertex, kVertex0);

                    /* kRay is the vector from the current point on the
                     * FlyPath curve to the current vertex.
                     */
                    kRay.x = kVertex0.x - m_aakCurvePos[iCurve][iSample].x;
                    kRay.y = kVertex0.y - m_aakCurvePos[iCurve][iSample].y;
                    kRay.z = kVertex0.z - m_aakCurvePos[iCurve][iSample].z;

                    if (kRay.lengthSquared() > 0.0f) {
                        kRay.normalize();
                    }

                    /* The distance to the vertex, distanceSquared is used for
                     * increased performance.
                     */
                    fDist = m_aakCurvePos[iCurve][iSample].distanceSquared(kVertex0);

                    /* Determine which quadrant the ray vector falls in and
                     * set the minimum value.
                     */
                    if (fDist < fMinN) {
                        fAngle = kNormal.angle(kRay);

                        if (fAngle < f10_DEGREES) {
                            fMinN = fDist;
                        }
                    }

                    if (fDist < fMinNI) {
                        fAngle = kNormalInv.angle(kRay);

                        if (fAngle < f10_DEGREES) {
                            fMinNI = fDist;
                        }
                    }

                    if (fDist < fMinS) {
                        fAngle = kSide.angle(kRay);

                        if (fAngle < f10_DEGREES) {
                            fMinS = fDist;
                        }
                    }

                    if (fDist < fMinSI) {
                        fAngle = kSideInv.angle(kRay);

                        if (fAngle < f10_DEGREES) {
                            fMinSI = fDist;
                        }
                    }
                }

                /* Check that the four minimum values are valid, if not set to
                 * zero.
                 */
                if (fMinN == Float.MAX_VALUE) {
                    fMinN = 0;
                }

                if (fMinNI == Float.MAX_VALUE) {
                    fMinNI = 0;
                }

                if (fMinS == Float.MAX_VALUE) {
                    fMinS = 0;
                }

                if (fMinSI == Float.MAX_VALUE) {
                    fMinSI = 0;
                }

                /* Take the maximum of the four minimums as the approximate
                 * "width" of the tube at this point on the FlyPath curve.
                 */
                fDist = Math.max(fMinN, fMinNI);
                fDist = Math.max(fDist, fMinS);
                fDist = Math.max(fDist, fMinSI);
                aafMaxDist[iCurve][iSample] = fDist;
            }

            /* Once all the samples on this curve have been evaluated, the
             * final boundary distance used is the maximum of the three values in a window around the current position.
             */
            for (int iSample = 0; iSample < akSamplePos.length; iSample++) {
                fMaxTemp1 = 0;
                fMaxTemp2 = 0;

                if ((iSample - iDownSample) >= 0) {
                    fMaxTemp1 = Math.max(aafMaxDist[iCurve][iSample - iDownSample], aafMaxDist[iCurve][iSample]);
                }

                if ((iSample + iDownSample) < akSamplePos.length) {
                    fMaxTemp2 = Math.max(aafMaxDist[iCurve][iSample], aafMaxDist[iCurve][iSample + iDownSample]);
                }

                m_aafMaxDist[iCurve][iSample] = Math.max(fMaxTemp1, fMaxTemp2);
            }
        }

        /* Free temporary variables allocated in this function.
         */
        aafMaxDist = null;
        akSamplePos = null;
    }

    /**
     * segmentCurvesByTangent produces a segmentation of the curves stored in the m_akCurvePosition data member. The
     * segmentation is based on how much the tangent of the curve changes from the last segment endpoint to the current
     * position on the curve. If the angle between the tangent at the current position and the vector between the
     * current position and the last segment endpoint is greater than 45 degrees, and the distance along the path
     * between the current point and the last segment end point is greater than a 5% of the overall curve length, then
     * the curve is split, and a new curve segment is started. If the new segment length is greater than 10% of the
     * overall curve length, it is divided into two new segments each with half the length. The Curve segment endpoints,
     * including the first start point at 0.0 and the last endpoint at 1.0, are stored in the data member
     * m_aafBreakPoints as relative distances along the curve. The numbers of break points for each curve are stored in
     * the data member m_aiNumBreakPoints. There will be m_aiNumBreakPoints - 1 mesh segments. The Curve is sampled
     * based on the original voxel path sample points. These points are also used in the findMaxMeshDistance function,
     * so they are stored in the data member m_aakCurvePos, the tangents at the sample points are also stored for later
     * use in the data member m_aakTangent. The relative distances of each segment endpoint are stored in the data
     * member m_aafReleativeLengths.
     */
    protected void segmentCurvesByTangent() {

        /* All intermediate variables are initialized outside the loops, in
         * increase perfomance speed of the calculations.
         */
        /* A temporary array -- with the maximum size -- for the segment break
         * points, since it is not known how many will be found.
         */
        float[][] aafCurveBreakPoints = new float[m_iNumCurves][];

        /* The data member for the number of break points along each curve.
         */
        m_aiNumBreakPoints = new int[m_iNumCurves];

        /* The original voxel sample points along the FlyPath curve
         */
        WildMagic.LibFoundation.Mathematics.Vector3f[] akSamplePos;

        /* Temporary variables for calculating the change in curvature of the
         * curve.
         */
        Point3f kPrevCurvePos = new Point3f(0, 0, 0);
        Vector3f kDifference = new Vector3f(0, 0, 0);
        float fAngle = 0;
        float fTime = 0;
        int iPrevIndex = 0;
        float fHalf = 0;
        float fPreviousTime = 0;
        //float fTotalCurveLength = 0;
        float fCurveSegmentPathLength = 0;

        /* Temporary variables for calculating the relative path lengths for
         * each sample point along the curve.
         */
        float[][] aafLengths = new float[m_iNumCurves][];
        float[] afTotalLength = new float[m_iNumCurves];

        /* The data member to keep track of the relative path lengths for each
         * sample point on each curve.
         */
        m_aafRelativeLengths = new float[m_iNumCurves][];
        ;

        /*
         * Loop over the number of curves in the kGraphCurve. Divide each curve into segments based on curvature, and on
         * the length of the curve segment.
         */
        m_aakCurvePos = new Point3f[m_iNumCurves][];
        m_aakTangent = new Vector3f[m_iNumCurves][];

        for (int iCurve = 0; iCurve < m_iNumCurves; iCurve++) {

            /* Retrieve the path sample points from m_kGraphSamples the sample
             * points were used to create the BSplines and are used calculate the relative distance of points along the
             * BSpline.  First the segment path lengths and the total path length are calculated, so that the relative
             * path can be stored for use later
             */
            akSamplePos = m_kGraphSamples.getArrayPointPosition(iCurve);

            /* Allocate memory to store the path length data
             */
            aafLengths[iCurve] = new float[akSamplePos.length];
            m_aafRelativeLengths[iCurve] = new float[akSamplePos.length];
            m_aakCurvePos[iCurve] = new Point3f[akSamplePos.length];
            m_aakTangent[iCurve] = new Vector3f[akSamplePos.length];

            /* Loop over the points in the sample path, calculate the path
             * lengths for each point in the path.
             */
            for (int iSample = 0; iSample < akSamplePos.length; iSample++) {

                if (iSample == 0) {
                    afTotalLength[iSample] = 0;
                    aafLengths[iCurve][iSample] = 0;
                    m_aafRelativeLengths[iCurve][iSample] = 0;
                } else {

                    /* Add the distance between the current sample point and
                     * the previous sample point to the total length
                     */
                    afTotalLength[iCurve] += akSamplePos[iSample].Distance(akSamplePos[iSample - 1]);

                    /* The length from the first sample point to the current
                     * sample point
                     */
                    aafLengths[iCurve][iSample] = afTotalLength[iCurve];
                }
            }

            /* Create a Progress bar to inform the user of the progress
             */
            /*
             * ViewJSimpleProgressBar kProgress = new ViewJSimpleProgressBar("Segmenting Triangle Mesh",
             *            "Processing Curve Points ...");
             */
            /* The range is the total length of this curve
             */
            // kProgress.setRange((float)0, afTotalLength[iCurve]);

            /* Temporarly store the Curve segment points
             */
            aafCurveBreakPoints[iCurve] = new float[akSamplePos.length];

            /* Initialize th number of segment points
             */
            m_aiNumBreakPoints[iCurve] = 0;
            //fTotalCurveLength = m_akCurvePosition[iCurve].GetTotalLength();


            /* Loop over the sample points and determine where to segment the
             * Curve based on curvature.
             */
            for (int iSample = 0; iSample < akSamplePos.length; iSample++) {

                /* Calculate and store the relative path lengths for the sample
                 * points.
                 */
                m_aafRelativeLengths[iCurve][iSample] = (aafLengths[iCurve][iSample] / afTotalLength[iCurve]);

                /* Time is the relative path length of the sample point. It is
                 * used to get the corresponding point on the Curve.
                 */
                fTime = m_aafRelativeLengths[iCurve][iSample];

                /* The position on the curve at the time fTime is stored for
                 * use in later functions.
                 */
                WildMagic.LibFoundation.Mathematics.Vector3f kVec = m_akCurvePosition[iCurve].GetPosition(fTime);
                m_aakCurvePos[iCurve][iSample] = new Point3f(kVec.X, kVec.Y, kVec.Z);

                /* If this is the first time through the loop, save the first
                 * point and index, also set the first CurveBreakPoint to this fTime.
                 */
                if (iSample == 0) {
                    kPrevCurvePos.x = m_aakCurvePos[iCurve][iSample].x;
                    kPrevCurvePos.y = m_aakCurvePos[iCurve][iSample].y;
                    kPrevCurvePos.z = m_aakCurvePos[iCurve][iSample].z;

                    aafCurveBreakPoints[iCurve][m_aiNumBreakPoints[iCurve]++] = fTime;

                    iPrevIndex = iSample;
                }

                /* If this is not the first time through the loop determine if
                 * the curvature from the current point on the Curve to the last point where the Curve was segmented.
                 */
                else if (iSample != 0) {

                    /* Use the tangent of the Curve at the current fTime to
                     * determine curvature. It is stored in a data member for use in later functions.
                     */
                	kVec = m_akCurvePosition[iCurve].GetTangent(fTime);
                    m_aakTangent[iCurve][iSample] = new Vector3f(kVec.X, kVec.Y, kVec.Z);

                    /* Calculate the difference vector between the current
                     *  point and the last point where the curve was segmented.
                     */
                    kDifference.x = m_aakCurvePos[iCurve][iSample].x - kPrevCurvePos.x;
                    kDifference.y = m_aakCurvePos[iCurve][iSample].y - kPrevCurvePos.y;
                    kDifference.z = m_aakCurvePos[iCurve][iSample].z - kPrevCurvePos.z;

                    /* Normalize the difference vector
                     */
                    if (kDifference.lengthSquared() > 0.0f) {
                        kDifference.normalize();
                    }

                    /* Calculate the angle between the difference vector and
                     * the tangent vector at the current point.
                     */
                    fAngle = m_aakTangent[iCurve][iSample].angle(kDifference);

                    /* Calculate the relative distance traveled since the last
                     * break point */
                    fCurveSegmentPathLength = fTime - m_aafRelativeLengths[iCurve][iPrevIndex];


                    /* If the angle is greater than or equal to 45 degrees,
                     * and the segment length is greater than a5% of the overall curve length, then segment the curve,
                     * and save the current point.
                     */
                    if ((fAngle >= fCURVE_CUTOFF) // 45 degrees
                            && (fCurveSegmentPathLength > 0.05)) {
                        kPrevCurvePos.x = m_aakCurvePos[iCurve][iSample].x;
                        kPrevCurvePos.y = m_aakCurvePos[iCurve][iSample].y;
                        kPrevCurvePos.z = m_aakCurvePos[iCurve][iSample].z;

                        /* If the new segment is longer than 10% of the curve
                         * break it into two segments. */
                        fPreviousTime = aafCurveBreakPoints[iCurve][m_aiNumBreakPoints[iCurve] - 1];

                        if ((fTime - fPreviousTime) > 0.10) {
                            fHalf = (float) ((fTime + fPreviousTime) / 2.0);
                            aafCurveBreakPoints[iCurve][m_aiNumBreakPoints[iCurve]++] = fHalf;
                        }

                        aafCurveBreakPoints[iCurve][m_aiNumBreakPoints[iCurve]++] = fTime;

                        iPrevIndex = iSample;
                    }
                }
            }

            // kProgress.dispose();
            aafCurveBreakPoints[iCurve][m_aiNumBreakPoints[iCurve]++] = (float) (1.0);
        }

        /* Allocate the exact size of the m_aafBreakPoints array and copy into
         * it.
         */
        m_aafBreakPoints = new float[m_iNumCurves][];

        for (int iCurve = 0; iCurve < m_iNumCurves; iCurve++) {
            m_aafBreakPoints[iCurve] = new float[m_aiNumBreakPoints[iCurve]];

            for (int iBreakPoint = 0; iBreakPoint < m_aiNumBreakPoints[iCurve]; iBreakPoint++) {
                m_aafBreakPoints[iCurve][iBreakPoint] = aafCurveBreakPoints[iCurve][iBreakPoint];
            }
        }

        /* Done with the tempory variables, so delete them.
         */
        aafCurveBreakPoints = null;
        aafLengths = null;
        afTotalLength = null;
    }

    /**
     * DOCUMENT ME!
     *
     * @param  iDownSample  DOCUMENT ME!
     */
    protected void segmentMesh(int iDownSample) {
        /* Local and temporary variables are allocated and initialized outside
         * the loops to increase performance.
         */

        /* Temporary and final connectivity arrays.
         */
        int[][][] aaaiConn = new int[m_iNumCurves][][];
        m_aaaiConnectivity = new int[m_iNumCurves][][];

        /* the number of triangles in each submesh
         */
        m_aaiNumTris = new int[m_iNumCurves][];

        /* Sample points along the FlyPath Curve. Down-sampling is an option
         * the user can choose.
         */
        WildMagic.LibFoundation.Mathematics.Vector3f[] akSamplePos;

        Point3f kVertex0 = new Point3f((float) 0, (float) 0, (float) 0);
        WildMagic.LibFoundation.Mathematics.Vector3f kVertex0WM = new WildMagic.LibFoundation.Mathematics.Vector3f(0,0,0);
        int iVIndex0 = 0, iVIndex1 = 0, iVIndex2 = 0;
        ;

        int iMinPt = 0;

        /* Variables for adding branch triangles to the parent branch */
        int iParent = 0;
        int iMinParentPt = 0;
        float fParentT = (float) 0.0;

        /* Variables for adding parent triangles to any branches */
        int iMinChildPt = 0;
        int[] iChildren;
        float[] fChildren;

        float fDist;
        float fRelativeLength;
        int iTriIndex;
        int iTriIndexDiv3;

        /* The number of triangles in the mesh. */
        int iIndexCount = m_kMesh.getIndexCount();

        /* Triangls already assigned to a sub-mesh are not duplicated in that
         * mesh. Initialize all positions to false.
         */
        boolean[][][] aaabFound = new boolean[m_iNumCurves][][];

        for (int iCurve = 0; iCurve < m_iNumCurves; iCurve++) {

            /* Allocate connectivit arrays */
            m_aaiNumTris[iCurve] = new int[(m_aiNumBreakPoints[iCurve] - 1)];
            aaaiConn[iCurve] = new int[(m_aiNumBreakPoints[iCurve] - 1)][];
            m_aaaiConnectivity[iCurve] = new int[(m_aiNumBreakPoints[iCurve] - 1)][];
            aaabFound[iCurve] = new boolean[(m_aiNumBreakPoints[iCurve] - 1)][];

            for (int iBreak = 0; iBreak < (m_aiNumBreakPoints[iCurve] - 1); iBreak++) {
                aaaiConn[iCurve][iBreak] = new int[iIndexCount];
                aaabFound[iCurve][iBreak] = new boolean[iIndexCount / 3];

                for (int iIndex = 0; iIndex < (iIndexCount / 3); iIndex++) {
                    aaabFound[iCurve][iBreak][iIndex] = false;
                }
            }
        }

        /* Loop over each curve, building sub-meshes. */
        for (int iCurve = 0; iCurve < m_iNumCurves; iCurve++) {

            /* Store the sample path positions for this Curve.
             */
            akSamplePos = m_kGraphSamples.getArrayPointPosition(iCurve);
            /*
             * ViewJSimpleProgressBar kProgress = new ViewJSimpleProgressBar("Segmenting Triangle Mesh",
             *            "Processing Triangles ..."); kProgress.setRange((float)0, (float)akSamplePos.length);
             */

            /* Loop over every sample in the FlyPath curve, down-sample if so
             * directed.
             */
            for (int iSample = 1; iSample < akSamplePos.length; iSample += iDownSample) {

                /* Find the sub-mesh for the current sample position, based on
                 * the curvature and the breakpoints calculated in segmentCurvesByTangent.
                 */
                fRelativeLength = m_aafRelativeLengths[iCurve][iSample];
                iMinPt = findCurveSegment(iCurve, fRelativeLength);

                /* Loop over the TriangleMesh index array. Increment by three
                 * each time through the loop -- as there are three Vertices for each triangle. Only one vertex in the
                 * triangles is boundary tested.
                 */
                for (int iIndex = 0; iIndex < iIndexCount; iIndex += 3) {

                    /* There are three times the number of triangle
                     * indices are there are triangles. */
                    iTriIndexDiv3 = iIndex / 3;

                    /* Find the three index values for this triangle and
                     * one vertex */
                    iVIndex0 = m_kMesh.getCoordinateIndex(iIndex);
                    m_kMesh.getCoordinate(iVIndex0, kVertex0);

                    /* The distance from the current position on the
                     * curve, both in the BSpline representation and the original voxel sample position representation
                     * are evaluated. The minimum distance is taken.
                     */
                    kVertex0WM.Set(kVertex0.x, kVertex0.y, kVertex0.z);
                    fDist = Math.min(m_aakCurvePos[iCurve][iSample].distanceSquared(kVertex0),
                                     akSamplePos[iSample].SquaredDistance(kVertex0WM));

                    /* If the distance is within the maximim "width" of
                     * the tube at this point on the path is the triangle added to the sub-mesh.
                     */
                    if (fDist < (fDISTANCE_SCALE_FACTOR * m_aafMaxDist[iCurve][iSample])) {
                        iVIndex1 = m_kMesh.getCoordinateIndex(iIndex + 1);
                        iVIndex2 = m_kMesh.getCoordinateIndex(iIndex + 2);

                        /* Add the vertex indices to the submesh */
                        if (!aaabFound[iCurve][iMinPt][iTriIndexDiv3]) {

                            /* Label this triangle as found for this submesh */
                            aaabFound[iCurve][iMinPt][iTriIndexDiv3] = true;

                            iTriIndex = m_aaiNumTris[iCurve][iMinPt];
                            aaaiConn[iCurve][iMinPt][iTriIndex] = iVIndex0;
                            aaaiConn[iCurve][iMinPt][iTriIndex + 1] = iVIndex1;
                            aaaiConn[iCurve][iMinPt][iTriIndex + 2] = iVIndex2;
                            m_aaiNumTris[iCurve][iMinPt] += 3;
                        }

                        /* Add the vertex indices to the sub-meshes on
                         * either side of this mesh. This takes care of the fact that when the position on the FlyPath
                         * approaches the next submesh, it's vertices
                         * become visible. */
                        if ((iMinPt - 1) >= 0) {

                            if (!aaabFound[iCurve][iMinPt - 1][iTriIndexDiv3]) {
                                aaabFound[iCurve][iMinPt - 1][iTriIndexDiv3] = true;

                                iTriIndex = m_aaiNumTris[iCurve][iMinPt - 1];
                                aaaiConn[iCurve][iMinPt - 1][iTriIndex] = iVIndex0;
                                aaaiConn[iCurve][iMinPt - 1][iTriIndex + 1] = iVIndex1;
                                aaaiConn[iCurve][iMinPt - 1][iTriIndex + 2] = iVIndex2;
                                m_aaiNumTris[iCurve][iMinPt - 1] += 3;
                            }
                        }

                        if ((iMinPt + 1) < (m_aiNumBreakPoints[iCurve] - 1)) {

                            if (!aaabFound[iCurve][iMinPt + 1][iTriIndexDiv3]) {
                                aaabFound[iCurve][iMinPt + 1][iTriIndexDiv3] = true;

                                iTriIndex = m_aaiNumTris[iCurve][iMinPt + 1];
                                aaaiConn[iCurve][iMinPt + 1][iTriIndex] = iVIndex0;
                                aaaiConn[iCurve][iMinPt + 1][iTriIndex + 1] = iVIndex1;
                                aaaiConn[iCurve][iMinPt + 1][iTriIndex + 2] = iVIndex2;
                                m_aaiNumTris[iCurve][iMinPt + 1] += 3;
                            }
                        }

                        /* If this is the first segment on the curve,
                         * check if it is a branch point and add the
                         * triangle to the parent. */
                        if (iMinPt == 0) {
                            iParent = m_kGraphCurve.getBranchParentIndex(iCurve);

                            if (iParent != -1) {
                                fParentT = m_kGraphCurve.getBranchParentNormalizedDist(iCurve);
                                iMinParentPt = findCurveSegment(iParent, fParentT);

                                if (!aaabFound[iParent][iMinParentPt][iTriIndexDiv3]) {
                                    aaabFound[iParent][iMinParentPt][iTriIndexDiv3] = true;

                                    iTriIndex = m_aaiNumTris[iParent][iMinParentPt];
                                    aaaiConn[iParent][iMinParentPt][iTriIndex] = iVIndex0;
                                    aaaiConn[iParent][iMinParentPt][iTriIndex + 1] = iVIndex1;
                                    aaaiConn[iParent][iMinParentPt][iTriIndex + 2] = iVIndex2;
                                    m_aaiNumTris[iParent][iMinParentPt] += 3;
                                }
                            }
                        }
                        /* Check if there is a branch off the segment this
                         * triangle belongs to, if so, add it to the branch's
                         * first segment. */

                        /* fChildren is the relative distance along this
                         * iCurve where the child branch, if any occurs. */
                        fChildren = m_kGraphCurve.getBranchPoints(iCurve);

                        /* Loop over the fChildren relative distances */
                        for (int iChild = 0; iChild < fChildren.length; iChild++) {

                            /* If the child's relative distance falls inside
                             * this branch: */
                            iMinChildPt = findCurveSegment(iCurve, fChildren[iChild]);

                            if (iMinChildPt == iMinPt) {

                                /* Get the curve index of the children */
                                iChildren = m_kGraphCurve.getBranchPointBranches(iCurve, iChild);

                                /* Loop over children */
                                for (int iChildIndex = 0; iChildIndex < iChildren.length; iChildIndex++) {

                                    /* Add this triangle to the child's first
                                     * segment ([0]), if it is not already
                                     * added: */
                                    if (!aaabFound[iChildren[iChildIndex]][0][iTriIndexDiv3]) {
                                        aaabFound[iChildren[iChildIndex]][0][iTriIndexDiv3] = true;

                                        iTriIndex = m_aaiNumTris[iChildren[iChildIndex]][0];

                                        aaaiConn[iChildren[iChildIndex]][0][iTriIndex] = iVIndex0;
                                        aaaiConn[iChildren[iChildIndex]][0][iTriIndex + 1] = iVIndex1;
                                        aaaiConn[iChildren[iChildIndex]][0][iTriIndex + 2] = iVIndex2;

                                        m_aaiNumTris[iChildren[iChildIndex]][0] += 3;
                                    }
                                }
                            }
                        }
                    }
                }
            }
        }

        /* The next step in the mesh segmentation is to loop over the
         * TriangleMesh index array, and add any Triangles that were not found to every mesh segment.
         */
        /*
         * ViewJSimpleProgressBar kProgress = new ViewJSimpleProgressBar("Segmenting Triangle Mesh",
         *        "Checking Triangles ..."); kProgress.setRange((float)0, (float)iIndexCount);
         */
        int iNotFoundCount = 0;
        boolean bTriFound = false;

        for (int iIndex = 0; iIndex < iIndexCount; iIndex += 3) {
            bTriFound = false;
            iTriIndexDiv3 = iIndex / 3;

            /* Determine if this triangle was found in any sub-mesh. If so,
             * break and don't add it. */
            for (int iCurve = 0; iCurve < m_iNumCurves; iCurve++) {

                for (int iBreak = 0; iBreak < (m_aiNumBreakPoints[iCurve] - 1); iBreak++) {

                    if (aaabFound[iCurve][iBreak][iTriIndexDiv3] == true) {
                        bTriFound = true;

                        break;
                    }
                }

                if (bTriFound) {
                    break;
                }
            }

            /* The triangle was not in any submesh. This is a conservative
             * approach to occlusion culling -- the triangle is added to each mesh. In testing typically > 1% of
             * triangles are not found at
             * this point in the algorithm. */
            if (!bTriFound) {
                iNotFoundCount++;
                iVIndex0 = m_kMesh.getCoordinateIndex(iIndex);
                iVIndex1 = m_kMesh.getCoordinateIndex(iIndex + 1);
                iVIndex2 = m_kMesh.getCoordinateIndex(iIndex + 2);

                for (int iCurve = 0; iCurve < m_iNumCurves; iCurve++) {

                    for (int iBreak = 0; iBreak < (m_aiNumBreakPoints[iCurve] - 1); iBreak++) {
                        iTriIndex = m_aaiNumTris[iCurve][iBreak];
                        aaaiConn[iCurve][iBreak][iTriIndex] = iVIndex0;
                        aaaiConn[iCurve][iBreak][iTriIndex + 1] = iVIndex1;
                        aaaiConn[iCurve][iBreak][iTriIndex + 2] = iVIndex2;
                        m_aaiNumTris[iCurve][iBreak] += 3;
                    }
                }
            }
        }


        int iNumTotalTris = 0;

        for (int iCurve = 0; iCurve < m_iNumCurves; iCurve++) {

            for (int iBreakPt = 0; iBreakPt < (m_aiNumBreakPoints[iCurve] - 1); iBreakPt++) {
                m_aaaiConnectivity[iCurve][iBreakPt] = new int[m_aaiNumTris[iCurve][iBreakPt]];
                iNumTotalTris = 0;

                for (iTriIndex = 0; iTriIndex < m_aaiNumTris[iCurve][iBreakPt]; iTriIndex++) {
                    m_aaaiConnectivity[iCurve][iBreakPt][iNumTotalTris++] = aaaiConn[iCurve][iBreakPt][iTriIndex];
                }
            }
        }

        /* Delete temporay and local variables */
        aaaiConn = null;
        aaabFound = null;
        akSamplePos = null;
    }
}
