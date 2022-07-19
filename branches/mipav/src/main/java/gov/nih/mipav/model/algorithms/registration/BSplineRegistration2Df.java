package gov.nih.mipav.model.algorithms.registration;


import WildMagic.LibFoundation.Curves.*;
import WildMagic.LibFoundation.Mathematics.*;
import gov.nih.mipav.model.structures.*;


/**
 * This class is used to register a 2D source image to a 2D target image. The images do not have to have the same
 * dimensions, but they must both contain single-channel data. The resulting registered source image has the same
 * dimensions as the input target image. A 2D B-Spline is used to map the coordinates of the registered image to
 * coordinates of the input source image. A separate B-Spline basis is setup for each axis given the number of control
 * points and the degree of the basis functions to use. Each B-Spline is uniform open with control points spaced equally
 * for each dimension. Bilinear interpolation is used to determine which input source image value(s) to assign to the
 * resulting registered source image. The "error" measure between the input target image and the current registered
 * source image is the sum of squared differences, where the root mean squared error is also computed. Control points
 * are moved one at a time by means of gradient descent minimization in order to minimize the error. The gradient is
 * approximated at each control point by means of finite differences. Control points are restricted from moving outside
 * the polygon formed by its 8 neighboring control points.
 */
public class BSplineRegistration2Df extends BSplineRegistrationBasef {

    //~ Static fields/initializers -------------------------------------------------------------------------------------

    /**
     * Defines the relative offset in control point indices to the 8 neighboring control points which form a bounding
     * polygon. The ordering is counterclockwise.
     */
    static final int[] m_scControlPointPolygonX = { -1, -1, -1, 0, +1, +1, +1, 0, -1 };

    /** DOCUMENT ME! */
    static final int[] m_scControlPointPolygonY = { +1, 0, -1, -1, -1, 0, +1, +1, +1 };

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** DOCUMENT ME! */
    protected int m_iNumSamplesSrcX;

    /** DOCUMENT ME! */
    protected int m_iNumSamplesSrcY;

    /** Number of samples in source and target images. */
    protected int m_iNumSamplesTrgX;

    /** DOCUMENT ME! */
    protected BSplineLattice2Df m_kBSpline2D;

    /** 2D B-Spline basis definitions. */
    protected BSplineBasisDiscretef m_kBSplineBasisX;

    /** DOCUMENT ME! */
    protected BSplineBasisDiscretef m_kBSplineBasisY;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Create instance to be used for registration. Upon construction, the initial state of the registered source image
     * is computed along with the error in comparing it to the reference target image.
     *
     * @param  kImageSrc    ModelSimpleImage Input source image which contains properties and data values. Must have
     *                      either 2 or 3 dimensions where the number of dimensions matches that of the target image.
     * @param  kImageTrg    ModelSimpleImage Input target image which contains properties and data values. Must have
     *                      either 2 or 3 dimensions where the number of dimensions matches that of the source image.
     * @param  kBasisX      BSplineBasisf B-spline basis for X axis which contains the B-spline degree and number of
     *                      control points.
     * @param  kBasisY      BSplineBasisf B-spline basis for Y axis which contains the B-spline degree and number of
     *                      control points.
     * @param  kRegMeasure  RegistrationMeasure Defines the cost measure for comparing the target image with the
     *                      registered source image.
     */
    public BSplineRegistration2Df(ModelSimpleImage kImageSrc, ModelSimpleImage kImageTrg, BSplineBasisf kBasisX,
                                  BSplineBasisf kBasisY, RegistrationMeasure kRegMeasure) {

        super(kImageSrc, kImageTrg, kRegMeasure);

        // Setup values used for mapping 2D array to linear array
        // allocate registration and error images.
        int iNumSamplesSrcX = kImageSrc.extents[0];
        int iNumSamplesSrcY = kImageSrc.extents[1];
        m_iNumSamplesTrgX = kImageTrg.extents[0];
        m_iNumSamplesSrcX = iNumSamplesSrcX;
        m_iNumSamplesSrcY = iNumSamplesSrcY;

        // create B-spline lattice
        m_kBSplineBasisX = new BSplineBasisDiscretef(kBasisX.GetNumCtrlPoints(), kBasisX.GetDegree(),
                                                     kImageTrg.extents[0]);
        m_kBSplineBasisY = new BSplineBasisDiscretef(kBasisY.GetNumCtrlPoints(), kBasisY.GetDegree(),
                                                     kImageTrg.extents[1]);
        m_kBSpline2D = new BSplineLattice2Df(m_kBSplineBasisX, m_kBSplineBasisY);

        // Create optimal placement of control points for each dimension
        // which yields the identity mapping of the source image, i.e.,
        // identity in that the output value to the Bspline function is
        // the same as the input value.  Then place these control points.
        float[] afControlPointX = createIdentityMapControlPoints(m_kBSplineBasisX);
        float[] afControlPointY = createIdentityMapControlPoints(m_kBSplineBasisY);
        Vector2f kPoint = new Vector2f();

        for (int iControlX = 0; iControlX < m_kBSplineBasisX.GetNumCtrlPoints(); iControlX++) {

            for (int iControlY = 0; iControlY < m_kBSplineBasisY.GetNumCtrlPoints(); iControlY++) {
                kPoint.X = afControlPointX[iControlX];
                kPoint.Y = afControlPointY[iControlY];
                m_kBSpline2D.setControlPoint(iControlX, iControlY, kPoint);
            }
        }

        // Update the initial registered source and error images.
        updateSamples(0, kImageTrg.extents[0] - 1, 0, kImageTrg.extents[1] - 1);
    }

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * Generate an image which contains the deformation value at each sample in the target image as it relates to how
     * the source image is transformed as a result of the registration. The image of interpolated source image
     * coordinates for each target image sample is generated and from that the Jacobian is generated at each sample
     * using finite differences. The deformation value is then the determinant of the Jacobian.
     *
     * @return  ModelSimpleImage New instance which contains the properties and data for the deformation image. This
     *          image has the same dimensions as and corresponds to the sample in the target image.
     */
    public ModelSimpleImage createImageDeformation() {

        float fDX = 1.0f / (m_kBSplineBasisX.GetNumSamples() - 1);
        float fDY = 1.0f / (m_kBSplineBasisY.GetNumSamples() - 1);

        ModelSimpleImage kDeformation = new ModelSimpleImage(m_kImageTrg.extents, m_kImageTrg.resolutions);

        ModelSimpleImage[] akSourceMap = createImageSourceMap();
        Vector2f kDiffX = new Vector2f();
        Vector2f kDiffY = new Vector2f();

        for (int iY = 0; iY < m_kBSplineBasisY.GetNumSamples(); iY++) {

            // Determine the indexes to use for the finite differences.
            // Use a central difference everywhere except for the first
            // and last index where a forward/backward difference is used.
            int iY0 = iY - 1;
            int iY1 = iY + 1;

            if (iY0 < 0) {
                iY0 = 0;
            } else if (iY1 >= m_kBSplineBasisY.GetNumSamples()) {
                iY1 = m_kBSplineBasisY.GetNumSamples() - 1;
            }

            for (int iX = 0; iX < m_kBSplineBasisX.GetNumSamples(); iX++) {

                // Determine the indexes to use for the finite differences.
                // Use a central difference everywhere except for the first
                // and last index where a forward/backward difference is used.
                int iX0 = iX - 1;
                int iX1 = iX + 1;

                if (iX0 < 0) {
                    iX0 = 0;
                } else if (iX1 >= m_kBSplineBasisX.GetNumSamples()) {
                    iX1 = m_kBSplineBasisX.GetNumSamples() - 1;
                }


                // Use finite differences to build the Jacobian as follows:
                //
                // | d(V.x)/dx  d(V.x)/dy |  = | a b |
                // | d(V.y)/dx  d(V.y)/dy |    | c d |
                //
                // where the determinant is ad-bc.
                // Note that the determinant is the same if the
                // matrix is transposed.
                int iDX0 = iX0 + (iY * m_iNumSamplesTrgX);
                int iDX1 = iX1 + (iY * m_iNumSamplesTrgX);
                int iDY0 = iX + (iY0 * m_iNumSamplesTrgX);
                int iDY1 = iX + (iY1 * m_iNumSamplesTrgX);
                kDiffX.X = akSourceMap[0].data[iDX1] - akSourceMap[0].data[iDX0];
                kDiffX.Y = akSourceMap[1].data[iDX1] - akSourceMap[1].data[iDX0];
                kDiffY.X = akSourceMap[0].data[iDY1] - akSourceMap[0].data[iDY0];
                kDiffY.Y = akSourceMap[1].data[iDY1] - akSourceMap[1].data[iDY0];
                kDiffX.scale(1.0f / ((iX1 - iX0) * fDX));
                kDiffY.scale(1.0f / ((iY1 - iY0) * fDY));

                float a = kDiffX.X;
                float b = kDiffY.X;
                float c = kDiffX.Y;
                float d = kDiffY.Y;

                int iIndex = iX + (iY * m_iNumSamplesTrgX);
                kDeformation.data[iIndex] = (a * d) - (b * c);
            }
        }

        akSourceMap = null;

        return kDeformation;
    }

    /**
     * Generate an image which contains the magnitude of the displacement vector at each sample in the target image as
     * it relates to how the source image is transformed as a result of the registration. The image of interpolated
     * source image coordinates for each target image sample is generated and the displacement is computed from the
     * source image coordinates for the identity registration transformation.
     *
     * @return  ModelSimpleImage New instance which contains the properties and data for the deformation image. This
     *          image has the same dimensions as and corresponds to the sample in the target image.
     */
    public ModelSimpleImage createImageDisplacement() {

        // What is the spacing of samples in the source/target images.
        float fSrcDX = 1.0f / (m_kImageSrc.extents[0] - 1);
        float fSrcDY = 1.0f / (m_kImageSrc.extents[1] - 1);
        float fTrgDX = 1.0f / (m_kImageTrg.extents[0] - 1);
        float fTrgDY = 1.0f / (m_kImageTrg.extents[1] - 1);

        ModelSimpleImage kImageDisplacement = new ModelSimpleImage(m_kImageTrg.extents, m_kImageTrg.resolutions);

        ModelSimpleImage[] akSourceMap = createImageSourceMap();

        for (int iY = 0; iY < m_kBSplineBasisY.GetNumSamples(); iY++) {
            float fSrcY = iY * fTrgDY;

            for (int iX = 0; iX < m_kBSplineBasisX.GetNumSamples(); iX++) {
                float fSrcX = iX * fTrgDX;

                // Sample in 2D array.
                int iIndex = iX + (iY * m_iNumSamplesTrgX);

                // Compute displacement vector in terms of samples.
                float fDX = (akSourceMap[0].data[iIndex] - fSrcX) / fSrcDX;
                float fDY = (akSourceMap[1].data[iIndex] - fSrcY) / fSrcDY;

                // Store the length of this displacement vector.
                kImageDisplacement.data[iIndex] = (float) Math.sqrt((fDX * fDX) + (fDY * fDY));
            }
        }

        akSourceMap = null;

        return kImageDisplacement;
    }

    /**
     * Generate an image the size of the target image in which each sample contains the 2D interpolated point
     * coordinates in the source image which "maps" or is registered with the corresponding point in the target image.
     *
     * @return  ModelSimpleImage[] Array of two ModelSimpleImage instances. The first one contains the x coordinate and
     *          the second contains the y coordinate for the map. Each image has the same dimensions which are that of
     *          the target image.
     */
    public ModelSimpleImage[] createImageSourceMap() {

        ModelSimpleImage[] akSourceMap = m_kBSpline2D.createImageMap(m_kImageTrg.extents[0], m_kImageTrg.extents[1]);

        // The images were created with resolutions of 1 in all axes,
        // so we need to reset them to correspond to those for the
        // the target image.
        akSourceMap[0].resolutions = (float[]) m_kImageTrg.resolutions.clone();
        akSourceMap[1].resolutions = (float[]) m_kImageTrg.resolutions.clone();

        return akSourceMap;
    }

    /**
     * Create a new registration with the same mapping as this one and which uses the same source image and the same
     * BSpline basis degree and number of control points. The only thing that changes is that the discrete [0,1]
     * interval is resampled.
     *
     * @param   kImageTrg  ModelSimpleImage Resampled target image.
     *
     * @return  BSplineRegistration2Df New registration of the same source image referenced to the resampled target
     *          image.
     */
    public BSplineRegistration2Df createSameMapping(ModelSimpleImage kImageTrg) {

        // Use the BSpline discrete bases for this instance to create
        // new one based on the resampling.
        BSplineBasisf kBasisX = new BSplineBasisf(this.m_kBSplineBasisX.GetNumCtrlPoints(),
                                                  this.m_kBSplineBasisX.GetDegree());
        BSplineBasisf kBasisY = new BSplineBasisf(this.m_kBSplineBasisY.GetNumCtrlPoints(),
                                                  this.m_kBSplineBasisY.GetDegree());

        // Create a new registration instance using the resampling.
        BSplineRegistration2Df kReg = new BSplineRegistration2Df(this.m_kImageSrc, kImageTrg, kBasisX, kBasisY,
                                                                 this.m_kRegMeasure.createNew());

        // Use the same control points since the number of control
        // points and the degree of the BSpline basis did not change.
        // This works because we are just resampling the [0,1] interval.
        Vector2f kPoint = new Vector2f();

        for (int iControlX = 0; iControlX < m_kBSplineBasisX.GetNumCtrlPoints(); iControlX++) {

            for (int iControlY = 0; iControlY < m_kBSplineBasisY.GetNumCtrlPoints(); iControlY++) {
                this.m_kBSpline2D.getControlPoint(iControlX, iControlY, kPoint);
                kReg.m_kBSpline2D.setControlPoint(iControlX, iControlY, kPoint);
            }
        }

        // Update the initial registered source and error images.
        kReg.updateSamples(0, kImageTrg.extents[0] - 1, 0, kImageTrg.extents[1] - 1);

        return kReg;
    }

    /**
     * Create a new registration with the same mapping as this one and which uses the same source image and a possibly
     * resampled target image, however, this time the BSpline basis degree and/or the number of control points may be
     * different.
     *
     * @param   kImageTrg  ModelSimpleImage Possibly resampled target image.
     * @param   kBasisX    BSplineBasisf B-spline basis for X axis which contains the B-spline degree and number of
     *                     control points.
     * @param   kBasisY    BSplineBasisf B-spline basis for Y axis which contains the B-spline degree and number of
     *                     control points.
     *
     * @return  BSplineRegistration2Df New registration of the same source image referenced to the resampled target
     *          image.
     */
    public BSplineRegistration2Df createSameMapping(ModelSimpleImage kImageTrg, BSplineBasisf kBasisX,
                                                    BSplineBasisf kBasisY) {

        // If the input BSpline basis is the same as that for the
        // basis used for this instance, then just address the issue
        // of the resampled target image.
        if (kBasisX.IsSameAs(this.m_kBSplineBasisX) && kBasisY.IsSameAs(this.m_kBSplineBasisY)) {
            return createSameMapping(kImageTrg);
        }

        // Create a new registration instance using the input BSpline basis.
        BSplineRegistration2Df kReg = new BSplineRegistration2Df(this.m_kImageSrc, kImageTrg, kBasisX, kBasisY,
                                                                 this.m_kRegMeasure.createNew());

        // We iterate through each control point in this new registration.
        // Note that this new registration instance by default provides the
        // "identity" map from the target to the source image properly
        // adjusted based on the BSpline basis degree and number of
        // control points.  The coordinates for each control point
        // are within the [0,1] range and so with that we can interpolate
        // the source map image to determine where that new registration
        // should reposition the control point to no longer provide the
        // "identity" map but to provide the registration map to the
        // original source image.
        Vector2f kPoint = new Vector2f();

        for (int iControlX = 0; iControlX < kBasisX.GetNumCtrlPoints(); iControlX++) {

            for (int iControlY = 0; iControlY < kBasisY.GetNumCtrlPoints(); iControlY++) {

                kReg.m_kBSpline2D.getControlPoint(iControlX, iControlY, kPoint);
                this.m_kBSpline2D.getPosition(kPoint.X, kPoint.Y, kPoint);
                kReg.m_kBSpline2D.setControlPoint(iControlX, iControlY, kPoint);
            }
        }

        // Update the initial registered source and error images.
        kReg.updateSamples(0, kImageTrg.extents[0] - 1, 0, kImageTrg.extents[1] - 1);

        return kReg;
    }

    /**
     * Cleanup memory.
     *
     * @throws  Throwable  DOCUMENT ME!
     */
    public void finalize() throws Throwable {
        m_kBSplineBasisX = null;
        m_kBSplineBasisY = null;
        m_kBSpline2D = null;

        super.finalize();
    }

    /**
     * Return access to the 2D discretized B-Spline internally created given the size of tbe input target image, and the
     * specified number of control points and degree of basis function for each B-Spline basis axis.
     *
     * @return  BSplineLattice2Df Access to 2D discretized B-Spline.
     */
    public BSplineLattice2Df getLattice() {
        return m_kBSpline2D;
    }

    /**
     * Move the specified control point using a gradient descent approach to minimize the error. The gradient descent
     * minimization is performed by sampling the error function at specified intervals. A maximum number of samples is
     * specified, but sampling always terminates when the bounding polygon formed by the 8 neighboring control points is
     * reached.
     *
     * @param  iControlX  int Identifies the control point in the 2D lattice.
     * @param  iControlY  int Identifies the control point in the 2D lattice.
     * @param  iMaxSteps  int Maximum number of samples of the error function by moving the control point in the
     *                    direction of the negative gradient.
     * @param  fStepSize  float Spacing between the samples of the error function by moving the control point in the
     *                    direction of the negative gradient.
     */
    public void minimizeControlPoint(int iControlX, int iControlY, int iMaxSteps, float fStepSize) {

        // Only allowed to move control points which are not anchored
        // to the boundary.
        if ((iControlX <= 0) || (iControlX >= (m_kBSplineBasisX.GetNumCtrlPoints() - 1))) {
            return;
        }

        if ((iControlY <= 0) || (iControlY >= (m_kBSplineBasisY.GetNumCtrlPoints() - 1))) {
            return;
        }

        // How far to search?
        float fMaxDist = (float) iMaxSteps * fStepSize;

        // Compute the error derivative at the specified control point.
        Vector2f kDirection = new Vector2f();
        getErrorDeriv(iControlX, iControlY, kDirection);
        kDirection.neg();

        if (0.0f == kDirection.length()) {
            return;
        }

        kDirection.normalize();

        // Compute how far the current control point is from the
        // boundary formed by its neighbor control points in the
        // direction of decreasing gradient.
        fMaxDist = getControlPointMaxMoveDist(iControlX, iControlY, kDirection, fMaxDist);

        // Compute the error by moving the control point along
        // the decreasing gradient direction.  Move the point by
        // small steps up to the boundary formed by the neighboring
        // control points and then keep track of where the minimum
        // was found.
        Vector2f kOrigin = new Vector2f();
        m_kBSpline2D.getControlPoint(iControlX, iControlY, kOrigin);

        double dMinError = getError();
        float fMinErrorT = 0.0f;
        Vector2f kNewPoint = new Vector2f();

        for (float fT = fStepSize; fT <= fMaxDist; fT += fStepSize) {
            kNewPoint.scaleAdd(fT, kDirection, kOrigin);
            m_kBSpline2D.setControlPoint(iControlX, iControlY, kNewPoint);
            updateControlPointSamples(iControlX, iControlY);

            double dError = getError();

            if (dError < dMinError) {
                dMinError = dError;
                fMinErrorT = fT;
            }
        }

        // Set the control point to the point along the ray where
        // the minimum was found.
        kNewPoint.scaleAdd(fMinErrorT, kDirection, kOrigin);
        m_kBSpline2D.setControlPoint(iControlX, iControlY, kNewPoint);
        updateControlPointSamples(iControlX, iControlY);
    }

    /**
     * Move the control point from its current location to the requested location. The requested location must be inside
     * the polygon formed by its 8 neighboring control points. If not, then a line segment is created between the
     * current and requested locations and the point where this line segment intersects the bounding polygon is found.
     * This point is offset a little along the line segment so that it is just inside the bounding polygon, and this
     * point is taken as the new location for the specified control point.
     *
     * @param   iControlX  int Identifies the control point in the 2D lattice.
     * @param   iControlY  int Identifies the control point in the 2D lattice.
     * @param   kPoint     Point2f Requested new coordinates for the specified control point.
     *
     * @return  float The distance between the updated control point position and its position before being modified.
     */
    public float moveControlPoint(int iControlX, int iControlY, Vector2f kPoint) {

        // Create vector from current control point coordinates to the
        // newly specified one.  Clip the new control point coordinates
        // to a point along the ray inside the neighboring control point
        // polygon.
    	Vector2f kOrigin = new Vector2f();
        m_kBSpline2D.getControlPoint(iControlX, iControlY, kOrigin);

        Vector2f kDirection = Vector2f.sub(kPoint, kOrigin);

        float fDist = kDirection.length();

        if (0.0f == fDist) {
            return fDist;
        }

        kDirection.normalize();
        fDist = getControlPointMaxMoveDist(iControlX, iControlY, kDirection, fDist);

        Vector2f kNewPoint = new Vector2f();
        kNewPoint.scaleAdd(fDist, kDirection, kOrigin);
        m_kBSpline2D.setControlPoint(iControlX, iControlY, kNewPoint);
        updateControlPointSamples(iControlX, iControlY);

        return fDist;
    }

    /**
     * Compute the maximum distance the control point can be moved in the specified direction while keeping the control
     * point relative to its 8 neighboring control points such that "folding" does not occur.
     *
     * @param   iControlX        int Identifies the control point in the 2D lattice.
     * @param   iControlY        int Identifies the control point in the 2D lattice.
     * @param   kRayDirection    Vector2f Direction from the current control point location for finding the distance to
     *                           the bounding polygon.
     * @param   fMaxDesiredDist  float Maximum distance along the ray direction that should be considered. At most, the
     *                           code would consider moving the control point.
     *
     * @return  float Computed distance.
     */
    protected float getControlPointMaxMoveDist(int iControlX, int iControlY, Vector2f kRayDirection,
                                               float fMaxDesiredDist) {

        // Get the origin for the specified control point.
    	Vector2f kP = new Vector2f();
        m_kBSpline2D.getControlPoint(iControlX, iControlY, kP);

        // Compute vector perpendicular to the direction vector.
        Vector2f kRayDirectionPerp = new Vector2f();
        kRayDirectionPerp.X = +kRayDirection.Y;
        kRayDirectionPerp.Y = -kRayDirection.X;

        // Loop through polygon of control point neighbors determining
        // which one intersects the ray.  Counterclockwise order.
        boolean bRayIntersect = false;
        float fMaxMoveDist = 0.0f;
        Vector2f kP0 = new Vector2f();
        Vector2f kP1 = new Vector2f();
        Vector2f kPT = new Vector2f();
        Vector2f kIntersect = new Vector2f();

        for (int iPolyPoint = 0; iPolyPoint < 8; iPolyPoint++) {

            // Create line segment of next neighbor control point pair
            m_kBSpline2D.getControlPoint(iControlX + m_scControlPointPolygonX[iPolyPoint],
                                         iControlY + m_scControlPointPolygonY[iPolyPoint], kP0);
            m_kBSpline2D.getControlPoint(iControlX + m_scControlPointPolygonX[iPolyPoint + 1],
                                         iControlY + m_scControlPointPolygonY[iPolyPoint + 1], kP1);
            Vector2f kSegmentDirection = Vector2f.sub(kP1, kP0);

            // Find point of intersection between ray and line segment.
            float fDenom = kSegmentDirection.dot(kRayDirectionPerp);

            if (0.0f != fDenom) {
                Vector2f kV = Vector2f.sub(kP, kP0);

                float fSegmentT = kV.dot(kRayDirectionPerp) / fDenom;

                // Does the ray intersect within the line segment bounds?
                if ((0.0f <= fSegmentT) && (fSegmentT <= 1.0f)) {
                    kIntersect.scaleAdd(fSegmentT, kSegmentDirection, kP0);

                    // Is the segment on the "direction" side of the ray?
                    kV = Vector2f.sub(kIntersect, kP);

                    float fT = kV.dot(kRayDirection);

                    if (fT > 0.0f) {

                        if (!bRayIntersect || (fT < fMaxMoveDist)) {
                            bRayIntersect = true;
                            fMaxMoveDist = fT;
                        }
                    }
                }
            }
        }

        // Scale back the max move distance by 1%.
        fMaxMoveDist = Math.min(fMaxMoveDist * 0.99f, fMaxDesiredDist);

        // Determine where the "folding" point occurs.  We already know that
        // the location of the control points before the specified control
        // point is moved does not cause "folding".  If the new location
        // of the control point does not cause "folding" then it is acceptable.
        // If the new location does cause "folding", then use bisection
        // method to find the location (along the ray) where just where
        // the "folding" occurs.  Stop when the bisection interval is
        // less than 1% of the maximum desired distance.
        // The first "iteration" is special because it just checks the
        // fMaxMoveDist to see if folding occurs there.
        float fIterMoveDistMin = 0.0f;
        float fIterMoveDistMax = fMaxMoveDist;
        float fIterMoveDist = fMaxMoveDist;
        int iIteration = 0;

        while ((fMaxDesiredDist * 0.01) < (fIterMoveDistMax - fIterMoveDistMin)) {

            // Where will the control point be moved to?
            kPT.scaleAdd(fIterMoveDist, kRayDirection, kP);

            // Look at each triangle formed by the center control point
            // and each pair of neighboring line segments on the perimeter
            // of the polygon formed by the 8 neighboring control points.
            // Use the determinant formulation which leads the computation
            // of the signed area of each triangle.  Because the line
            // segments are consistently counterclockwise ordered,
            // the signed determinant which results should be positive.  If
            // the center control point was moved such that the "folding" of
            // the Bspline transformation can occur, then a negative determinant
            // will be the result.
            boolean bFolding = false;

            for (int iPolyPoint = 0; iPolyPoint < 8; iPolyPoint++) {

                // Create line segment of next neighbor control point pair
                m_kBSpline2D.getControlPoint(iControlX + m_scControlPointPolygonX[iPolyPoint],
                                             iControlY + m_scControlPointPolygonY[iPolyPoint], kP0);
                m_kBSpline2D.getControlPoint(iControlX + m_scControlPointPolygonX[iPolyPoint + 1],
                                             iControlY + m_scControlPointPolygonY[iPolyPoint + 1], kP1);

                // Area of triangle is 1/2 times the (absolute value of the)
                // determinant of the following matrix:
                //
                // | P0.x P0.y  1 |
                // | P1.x P1.y  1 |
                // | PT.x PT.y  1 |
                //
                // If the determinant is negative, then the control points
                // are ordered such that "folding" occurs.
                float a0 = kP0.X, a1 = kP0.Y;
                float b0 = kP1.X, b1 = kP1.Y;
                float c0 = kPT.X, c1 = kPT.Y;
                float fDet = (a0 * b1) - (a1 * b0) + (a1 * c0) - (a0 * c1) + (b0 * c1) - (b1 * c0);

                if (fDet < 0.0f) {
                    bFolding = true;

                    break;
                }
            }

            // Special first pass?
            if (0 == iIteration++) {

                if (!bFolding) {
                    break;
                }
            }
            // Bisection iteration?
            else {

                if (bFolding) {
                    fIterMoveDistMax = fIterMoveDist;
                } else {
                    fIterMoveDistMin = fIterMoveDist;
                }
            }

            // Perform bisection.
            fIterMoveDist = (fIterMoveDistMin + fIterMoveDistMax) * 0.5f;

            // Always choose a distance which does not cause "folding"
            fMaxMoveDist = fIterMoveDistMin;
        }

        return fMaxMoveDist;
    }

    /**
     * Computes the derivative of the error function at a control point by approximating with finite differences where
     * the control point is moved small amounts in each orthgonal direction.
     *
     * @param  iControlX  int Identifies the control point in the 2D lattice.
     * @param  iControlY  int Identifies the control point in the 2D lattice.
     * @param  kDeriv     Vector2f Vector to be filled with the derivative computed along each axis.
     */
    protected void getErrorDeriv(int iControlX, int iControlY, Vector2f kDeriv) {

        // Compute the step of half a sample in each direction.
        float fSmallStepX = 0.5f / (float) (m_kBSplineBasisX.GetNumSamples() - 1);
        float fSmallStepY = 0.5f / (float) (m_kBSplineBasisY.GetNumSamples() - 1);

        // Get the coordinates of the current control point.
        Vector2f kOrigin = new Vector2f();
        m_kBSpline2D.getControlPoint(iControlX, iControlY, kOrigin);

        Vector2f kNewPoint = new Vector2f();

        // Compute the error in the +X direction.
        kNewPoint.set(+fSmallStepX, 0.0f).add(kOrigin);

        float fStepXPos = moveControlPoint(iControlX, iControlY, kNewPoint);
        double dErrorXPos = m_kRegMeasure.getError();
        m_kBSpline2D.setControlPoint(iControlX, iControlY, kOrigin);

        // Compute the error in the -X direction.
        kNewPoint.set(-fSmallStepX, 0.0f).add(kOrigin);

        float fStepXNeg = moveControlPoint(iControlX, iControlY, kNewPoint);
        double dErrorXNeg = m_kRegMeasure.getError();
        m_kBSpline2D.setControlPoint(iControlX, iControlY, kOrigin);

        // Compute the error in the +Y direction.
        kNewPoint.set(0.0f, +fSmallStepY).add(kOrigin);

        float fStepYPos = moveControlPoint(iControlX, iControlY, kNewPoint);
        double dErrorYPos = m_kRegMeasure.getError();
        m_kBSpline2D.setControlPoint(iControlX, iControlY, kOrigin);

        // Compute the error in the -Y direction.
        kNewPoint.set(0.0f, -fSmallStepY).add(kOrigin);

        float fStepYNeg = moveControlPoint(iControlX, iControlY, kNewPoint);
        double dErrorYNeg = m_kRegMeasure.getError();
        m_kBSpline2D.setControlPoint(iControlX, iControlY, kOrigin);

        // After moving the control point back to the origin, we need
        // to update the samples affected by this move.
        updateControlPointSamples(iControlX, iControlY);

        // Compute gradient terms.
        float fStepX = fStepXPos + fStepXNeg;
        float fStepY = fStepYPos + fStepYNeg;
        kDeriv.X = (fStepX > 0.0f) ? ((float) (dErrorXPos - dErrorXNeg) / fStepX) : 0.0f;
        kDeriv.Y = (fStepY > 0.0f) ? ((float) (dErrorYPos - dErrorYNeg) / fStepY) : 0.0f;
    }

    /**
     * Update the registered source image and the computed error as a result of making changes to a single control
     * point. This is the local control aspect of the B-Spline which means only those image samples which are affected
     * by the control point need to be updated.
     *
     * @param  iControlX  int Identifies the control point in the 2D lattice.
     * @param  iControlY  int Identifies the control point in the 2D lattice.
     */
    protected void updateControlPointSamples(int iControlX, int iControlY) {
        updateSamples(m_kBSplineBasisX.GetControlPointSampleIndexMin(iControlX),
                      m_kBSplineBasisX.GetControlPointSampleIndexMax(iControlX),
                      m_kBSplineBasisY.GetControlPointSampleIndexMin(iControlY),
                      m_kBSplineBasisY.GetControlPointSampleIndexMax(iControlY));
    }

    /**
     * Update the registered source image and the computed error but only for the specified range of samples.
     *
     * @param  iMinX  int Minimum X axis sample index.
     * @param  iMaxX  int Maximum X axis sample index.
     * @param  iMinY  int Minimum Y axis sample index.
     * @param  iMaxY  int Maximum Y axis sample index.
     */
    protected void updateSamples(int iMinX, int iMaxX, int iMinY, int iMaxY) {

        int iLimitX = m_iNumSamplesSrcX - 1;
        int iLimitY = m_iNumSamplesSrcY - 1;

        // compute local change to registered source
        Vector2f kPos = new Vector2f();

        for (int iY = iMinY; iY <= iMaxY; iY++) {

            for (int iX = iMinX; iX <= iMaxX; iX++) {

                // evaulate spline and setup for bilinear interpolation
                m_kBSpline2D.getPosition(iX, iY, kPos);

                float fX = iLimitX * kPos.X;
                float fY = iLimitY * kPos.Y;
                int iX0 = (int) fX;
                int iY0 = (int) fY;

                if (iX0 >= iLimitX) {
                    iX0 = iLimitX - 1;
                    fX = (float) iLimitX;
                }

                if (iY0 >= iLimitY) {
                    iY0 = iLimitY - 1;
                    fY = (float) iLimitY;
                }

                float fX1 = fX - iX0;
                float fY1 = fY - iY0;
                float fX0 = 1.0f - fX1;
                float fY0 = 1.0f - fY1;

                int iX0Y0 = iX0 + (iY0 * m_iNumSamplesSrcX);
                int iX1Y0 = iX0Y0 + 1;
                int iX0Y1 = iX0Y0 + m_iNumSamplesSrcX;
                int iX1Y1 = iX0Y1 + 1;

                // interpolate across X
                float fRegSrcY0 = (m_kImageSrc.data[iX0Y0] * fX0) + (m_kImageSrc.data[iX1Y0] * fX1);
                float fRegSrcY1 = (m_kImageSrc.data[iX0Y1] * fX0) + (m_kImageSrc.data[iX1Y1] * fX1);

                // interpolate across Y
                float fRegSrcValue = (fRegSrcY0 * fY0) + (fRegSrcY1 * fY1);

                int iIndex = iX + (iY * m_iNumSamplesTrgX);
                m_kRegMeasure.updateRegistration(iIndex, fRegSrcValue);
            }
        }
    }
}
