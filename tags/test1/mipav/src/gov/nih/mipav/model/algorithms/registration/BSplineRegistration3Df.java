package gov.nih.mipav.model.algorithms.registration;


import WildMagic.LibFoundation.Curves.*;
import WildMagic.LibFoundation.Mathematics.*;
import gov.nih.mipav.model.structures.BSplineLattice3Df;
import gov.nih.mipav.model.structures.ModelSimpleImage;
import gov.nih.mipav.util.ThreadUtil;
import gov.nih.mipav.view.Preferences;

import java.util.concurrent.CountDownLatch;


/**
 * This class is used to register a 3D source image to a 3D target image. The images do not have to have the same
 * dimensions, but they must both contain single-channel data. The resulting registered source image has the same
 * dimensions as the input target image. A 3D B-Spline is used to map the coordinates of the registered image to
 * coordinates of the input source image. A separate B-Spline basis is setup for each axis given the number of control
 * points and the degree of the basis functions to use. Each B-Spline is uniform open with control points spaced equally
 * for each dimension. Trilinear interpolation is used to determine which input source image value(s) to assign to the
 * resulting registered source image. The "error" measure between the input target image and the current registered
 * source image is the sum of squared differences, where the root mean squared error is also computed. Control points
 * are moved one at a time by means of gradient descent minimization in order to minimize the error. The gradient is
 * approximated at each control point by means of finite differences. Control points are restricted from moving outside
 * the polyhedron formed by its 26 neighboring control points.
 */
public class BSplineRegistration3Df extends BSplineRegistrationBasef {

    //~ Static fields/initializers -------------------------------------------------------------------------------------

    /**
     * Defines the relative offset in control point indices for the triangle mesh associated with the bounding
     * polyhedron formed by the 26 neighboring control points. Triangle vertices are consistently counterclockwise
     * ordered. First index: 0 for X axis, 1 for Y axis, 2 for Z axis; Second index: index of vertex within triangle;
     * Third index: index for triangle (there are 48 -- 8 per face; see initPolyhedronTrimeshConnectivity method for
     * description)
     */
    protected static int[][][] ms_aaaiPolyhedronTriangleControlPointOffset = new int[3][3][48];
    /**
     * This is a static method called to initialize the class. This method is used to initialize other static members.
     * Create the (relative) trimesh connectivity for the polyhedron of neighboring control points. There are six faces
     * each having 8 triangles. The ordering of the triangles is important and must be counterclockwise. This table is
     * used where we initially know each point is inside its neighboring control point polyhedron and we will not move
     * the center control point any distance further than the distance to the triangle in the direction of movement.
     */
    static {
        initPolyhedronTrimeshConnectivity(0, 0, 1, 2, +1); // +z index of ctrl points
        initPolyhedronTrimeshConnectivity(8, 0, 1, 2, -1); // -z index of ctrl points
        initPolyhedronTrimeshConnectivity(16, 1, 2, 0, +1); // +x index of ctrl points
        initPolyhedronTrimeshConnectivity(24, 1, 2, 0, -1); // -x index of ctrl points
        initPolyhedronTrimeshConnectivity(32, 2, 0, 1, +1); // +y index of ctrl points
        initPolyhedronTrimeshConnectivity(40, 2, 0, 1, -1); // -y index of ctrl points
    }

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** DOCUMENT ME! */
    protected int m_iNumSamplesSrcX;

    /** DOCUMENT ME! */
    protected int m_iNumSamplesSrcXY;

    /** DOCUMENT ME! */
    protected int m_iNumSamplesSrcY;

    /** DOCUMENT ME! */
    protected int m_iNumSamplesSrcZ;

    /** Number of samples in source and target images. */
    protected int m_iNumSamplesTrgX;

    /** DOCUMENT ME! */
    protected int m_iNumSamplesTrgXY;

    /** DOCUMENT ME! */
    protected BSplineLattice3Df m_kBSpline3D;

    /** 3D B-Spline basis definitions. */
    protected BSplineBasisDiscretef m_kBSplineBasisX;

    /** DOCUMENT ME! */
    protected BSplineBasisDiscretef m_kBSplineBasisY;

    /** DOCUMENT ME! */
    protected BSplineBasisDiscretef m_kBSplineBasisZ;

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
     * @param  kBasisZ      BSplineBasisf B-spline basis for Z axis which contains the B-spline degree and number of
     *                      control points.
     * @param  kRegMeasure  RegistrationMeasure Defines the cost measure for comparing the target image with the
     *                      registered source image.
     */
    public BSplineRegistration3Df(ModelSimpleImage kImageSrc, ModelSimpleImage kImageTrg, BSplineBasisf kBasisX,
                                  BSplineBasisf kBasisY, BSplineBasisf kBasisZ, RegistrationMeasure kRegMeasure) {

        super(kImageSrc, kImageTrg, kRegMeasure);

        // Compute values used for mapping 3D array to linear array
        // allocate registration and error images
        int iNumSamplesSrcX = kImageSrc.extents[0];
        int iNumSamplesSrcY = kImageSrc.extents[1];
        int iNumSamplesSrcZ = kImageSrc.extents[2];
        m_iNumSamplesTrgX = kImageTrg.extents[0];
        m_iNumSamplesTrgXY = m_iNumSamplesTrgX * kImageTrg.extents[1];
        m_iNumSamplesSrcX = iNumSamplesSrcX;
        m_iNumSamplesSrcY = iNumSamplesSrcY;
        m_iNumSamplesSrcZ = iNumSamplesSrcZ;
        m_iNumSamplesSrcXY = m_iNumSamplesSrcX * m_iNumSamplesSrcY;

        // create B-spline lattice
        m_kBSplineBasisX = new BSplineBasisDiscretef(kBasisX.GetNumCtrlPoints(), kBasisX.GetDegree(),
                                                     kImageTrg.extents[0]);
        m_kBSplineBasisY = new BSplineBasisDiscretef(kBasisY.GetNumCtrlPoints(), kBasisY.GetDegree(),
                                                     kImageTrg.extents[1]);
        m_kBSplineBasisZ = new BSplineBasisDiscretef(kBasisZ.GetNumCtrlPoints(), kBasisZ.GetDegree(),
                                                     kImageTrg.extents[2]);
        m_kBSpline3D = new BSplineLattice3Df(m_kBSplineBasisX, m_kBSplineBasisY, m_kBSplineBasisZ);


        // Create optimal placement of control points for each dimension
        // which yields the identity mapping of the source image, i.e.,
        // identity in that the output value to the Bspline function is
        // the same as the input value.  Then place these control points.
        float[] afControlPointX = createIdentityMapControlPoints(m_kBSplineBasisX);
        float[] afControlPointY = createIdentityMapControlPoints(m_kBSplineBasisY);
        float[] afControlPointZ = createIdentityMapControlPoints(m_kBSplineBasisZ);
        Vector3f kPoint = new Vector3f();

        for (int iControlX = 0; iControlX < m_kBSplineBasisX.GetNumCtrlPoints(); iControlX++) {

            for (int iControlY = 0; iControlY < m_kBSplineBasisY.GetNumCtrlPoints(); iControlY++) {

                for (int iControlZ = 0; iControlZ < m_kBSplineBasisZ.GetNumCtrlPoints(); iControlZ++) {
                    kPoint.X = afControlPointX[iControlX];
                    kPoint.Y = afControlPointY[iControlY];
                    kPoint.Z = afControlPointZ[iControlZ];
                    m_kBSpline3D.setControlPoint(iControlX, iControlY, iControlZ, kPoint);
                }
            }
        }

        // Update the initial registered source and error images.
        if(!Preferences.isMultiThreadingEnabled() ){
        	updateSamplesST(0, kImageTrg.extents[0] - 1, 0, kImageTrg.extents[1] - 1, 0, kImageTrg.extents[2] - 1);
        }else{
            updateSamplesMT(0, kImageTrg.extents[0] - 1, 0, kImageTrg.extents[1] - 1, 0, kImageTrg.extents[2] - 1);        	
        }
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
        float fDZ = 1.0f / (m_kBSplineBasisZ.GetNumSamples() - 1);

        ModelSimpleImage kDeformation = new ModelSimpleImage(m_kImageTrg.extents, m_kImageTrg.resolutions);

        ModelSimpleImage[] akSourceMap = createImageSourceMap();
        Vector3f kDiffX = new Vector3f();
        Vector3f kDiffY = new Vector3f();
        Vector3f kDiffZ = new Vector3f();

        for (int iZ = 0; iZ < m_kBSplineBasisZ.GetNumSamples(); iZ++) {

            // Determine the indexes to use for the finite differences.
            // Use a central difference everywhere except for the first
            // and last index where a forward/backward difference is used.
            int iZ0 = iZ - 1;
            int iZ1 = iZ + 1;

            if (iZ0 < 0) {
                iZ0 = 0;
            } else if (iZ1 >= m_kBSplineBasisZ.GetNumSamples()) {
                iZ1 = m_kBSplineBasisZ.GetNumSamples() - 1;
            }

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
                    // | d(V.X)/dx  d(V.X)/dy   d(V.X)/dz |    | a b c |
                    // | d(V.Y)/dx  d(V.Y)/dy   d(V.Y)/dz |  = | d e f |
                    // | d(V.Z)/dx  d(V.Z)/dy   d(V.Z)/dz |    | g h i |
                    //
                    // where the determinant is
                    // Note that the determinant is the same if the
                    // matrix is transposed.
                    int iDX0 = iX0 + (iY * m_iNumSamplesTrgX) + (iZ * m_iNumSamplesTrgXY);
                    int iDX1 = iX1 + (iY * m_iNumSamplesTrgX) + (iZ * m_iNumSamplesTrgXY);
                    int iDY0 = iX + (iY0 * m_iNumSamplesTrgX) + (iZ * m_iNumSamplesTrgXY);
                    int iDY1 = iX + (iY1 * m_iNumSamplesTrgX) + (iZ * m_iNumSamplesTrgXY);
                    int iDZ0 = iX + (iY * m_iNumSamplesTrgX) + (iZ0 * m_iNumSamplesTrgXY);
                    int iDZ1 = iX + (iY * m_iNumSamplesTrgX) + (iZ1 * m_iNumSamplesTrgXY);
                    kDiffX.X = akSourceMap[0].data[iDX1] - akSourceMap[0].data[iDX0];
                    kDiffX.Y = akSourceMap[1].data[iDX1] - akSourceMap[1].data[iDX0];
                    kDiffX.Z = akSourceMap[2].data[iDX1] - akSourceMap[2].data[iDX0];
                    kDiffY.X = akSourceMap[0].data[iDY1] - akSourceMap[0].data[iDY0];
                    kDiffY.Y = akSourceMap[1].data[iDY1] - akSourceMap[1].data[iDY0];
                    kDiffY.Z = akSourceMap[2].data[iDY1] - akSourceMap[2].data[iDY0];
                    kDiffZ.X = akSourceMap[0].data[iDZ1] - akSourceMap[0].data[iDZ0];
                    kDiffZ.Y = akSourceMap[1].data[iDZ1] - akSourceMap[1].data[iDZ0];
                    kDiffZ.Z = akSourceMap[2].data[iDZ1] - akSourceMap[2].data[iDZ0];
                    kDiffX.scale(1.0f / ((iX1 - iX0) * fDX));
                    kDiffY.scale(1.0f / ((iY1 - iY0) * fDY));
                    kDiffZ.scale(1.0f / ((iZ1 - iZ0) * fDZ));

                    float a = kDiffX.X;
                    float b = kDiffY.X;
                    float c = kDiffZ.X;
                    float d = kDiffX.Y;
                    float e = kDiffY.Y;
                    float f = kDiffZ.Y;
                    float g = kDiffX.Z;
                    float h = kDiffY.Z;
                    float i = kDiffZ.Z;

                    int iIndex = iX + (iY * m_iNumSamplesTrgX) + (iZ * m_iNumSamplesTrgXY);
                    kDeformation.data[iIndex] = (a * ((e * i) - (f * h))) + (b * ((f * g) - (d * i))) +
                                                (c * ((d * h) - (e * g)));
                }
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
        float fSrcDZ = 1.0f / (m_kImageSrc.extents[2] - 1);
        float fTrgDX = 1.0f / (m_kImageTrg.extents[0] - 1);
        float fTrgDY = 1.0f / (m_kImageTrg.extents[1] - 1);
        float fTrgDZ = 1.0f / (m_kImageTrg.extents[2] - 1);

        ModelSimpleImage kImageDisplacement = new ModelSimpleImage(m_kImageTrg.extents, m_kImageTrg.resolutions);

        ModelSimpleImage[] akSourceMap = createImageSourceMap();

        for (int iZ = 0; iZ < m_kBSplineBasisZ.GetNumSamples(); iZ++) {
            float fSrcZ = iZ * fTrgDZ;

            for (int iY = 0; iY < m_kBSplineBasisY.GetNumSamples(); iY++) {
                float fSrcY = iY * fTrgDY;

                for (int iX = 0; iX < m_kBSplineBasisX.GetNumSamples(); iX++) {
                    float fSrcX = iX * fTrgDX;

                    // Sample in 3D array.
                    int iIndex = iX + (iY * m_iNumSamplesTrgX) + (iZ * m_iNumSamplesTrgXY);

                    // Compute displacement vector in terms of samples.
                    float fDX = (akSourceMap[0].data[iIndex] - fSrcX) / fSrcDX;
                    float fDY = (akSourceMap[1].data[iIndex] - fSrcY) / fSrcDY;
                    float fDZ = (akSourceMap[2].data[iIndex] - fSrcZ) / fSrcDZ;

                    // Store the length of this displacement vector.
                    kImageDisplacement.data[iIndex] = (float) Math.sqrt((fDX * fDX) + (fDY * fDY) + (fDZ * fDZ));
                }
            }
        }

        akSourceMap = null;

        return kImageDisplacement;
    }

    /**
     * Generate an image the size of the target image in which each sample contains the 3D interpolated point
     * coordinates in the source image which "maps" or is registered with the corresponding point in the target image.
     *
     * @return  ModelSimpleImage[] Array of three ModelSimpleImage instances. The first one contains the x coordinate,
     *          the second contains the y coordinate for the map, and the third contains the z coordinate for the map.
     *          Each image has the same dimensions which are that of the target image.
     */
    public ModelSimpleImage[] createImageSourceMap() {

        ModelSimpleImage[] akSourceMap = m_kBSpline3D.createImageMap(m_kImageTrg.extents[0], m_kImageTrg.extents[1],
                                                                     m_kImageTrg.extents[2]);

        // The images were created with resolutions of 1 in all axes,
        // so we need to reset them to correspond to those for the
        // the target image.
        akSourceMap[0].resolutions = (float[]) m_kImageTrg.resolutions.clone();
        akSourceMap[1].resolutions = (float[]) m_kImageTrg.resolutions.clone();
        akSourceMap[2].resolutions = (float[]) m_kImageTrg.resolutions.clone();

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
    public BSplineRegistration3Df createSameMapping(ModelSimpleImage kImageTrg) {

        // Use the BSpline discrete bases for this instance to create
        // new one based on the resampling.
        BSplineBasisf kBasisX = new BSplineBasisf(this.m_kBSplineBasisX.GetNumCtrlPoints(),
                                                  this.m_kBSplineBasisX.GetDegree());
        BSplineBasisf kBasisY = new BSplineBasisf(this.m_kBSplineBasisY.GetNumCtrlPoints(),
                                                  this.m_kBSplineBasisY.GetDegree());
        BSplineBasisf kBasisZ = new BSplineBasisf(this.m_kBSplineBasisZ.GetNumCtrlPoints(),
                                                  this.m_kBSplineBasisZ.GetDegree());

        // Create a new registration instance using the resampling.
        BSplineRegistration3Df kReg = new BSplineRegistration3Df(this.m_kImageSrc, kImageTrg, kBasisX, kBasisY, kBasisZ,
                                                                 this.m_kRegMeasure.createNew());

        // Use the same control points since the number of control
        // points and the degree of the BSpline basis did not change.
        // This works because we are just resampling the [0,1] interval.
        Vector3f kPoint = new Vector3f();

        for (int iControlX = 0; iControlX < m_kBSplineBasisX.GetNumCtrlPoints(); iControlX++) {

            for (int iControlY = 0; iControlY < m_kBSplineBasisY.GetNumCtrlPoints(); iControlY++) {

                for (int iControlZ = 0; iControlZ < m_kBSplineBasisZ.GetNumCtrlPoints(); iControlZ++) {
                    this.m_kBSpline3D.getControlPoint(iControlX, iControlY, iControlZ, kPoint);
                    kReg.m_kBSpline3D.setControlPoint(iControlX, iControlY, iControlZ, kPoint);
                }
            }
        }

        // Update the initial registered source and error images.
        if(!Preferences.isMultiThreadingEnabled()){
        	kReg.updateSamplesST(0, kImageTrg.extents[0] - 1, 0, kImageTrg.extents[1] - 1, 0, kImageTrg.extents[2] - 1);
        }else{
        	kReg.updateSamplesMT(0, kImageTrg.extents[0] - 1, 0, kImageTrg.extents[1] - 1, 0, kImageTrg.extents[2] - 1);
        }

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
     * @param   kBasisZ    BSplineBasisf B-spline basis for Z axis which contains the B-spline degree and number of
     *                     control points.
     *
     * @return  BSplineRegistration2Df New registration of the same source image referenced to the resampled target
     *          image.
     */
    public BSplineRegistration3Df createSameMapping(ModelSimpleImage kImageTrg, BSplineBasisf kBasisX,
                                                    BSplineBasisf kBasisY, BSplineBasisf kBasisZ) {

        // If the input BSpline basis is the same as that for the
        // basis used for this instance, then just address the issue
        // of the resampled target image.
        if (kBasisX.IsSameAs(this.m_kBSplineBasisX) && kBasisY.IsSameAs(this.m_kBSplineBasisY) &&
                kBasisZ.IsSameAs(this.m_kBSplineBasisZ)) {
            return createSameMapping(kImageTrg);
        }

        // Create a new registration instance using the input BSpline basis.
        BSplineRegistration3Df kReg = new BSplineRegistration3Df(this.m_kImageSrc, kImageTrg, kBasisX, kBasisY, kBasisZ,
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
        Vector3f kPoint = new Vector3f();

        for (int iControlX = 0; iControlX < kBasisX.GetNumCtrlPoints(); iControlX++) {

            for (int iControlY = 0; iControlY < kBasisY.GetNumCtrlPoints(); iControlY++) {

                for (int iControlZ = 0; iControlZ < kBasisZ.GetNumCtrlPoints(); iControlZ++) {

                    kReg.m_kBSpline3D.getControlPoint(iControlX, iControlY, iControlZ, kPoint);
                    this.m_kBSpline3D.getPosition(kPoint.X, kPoint.Y, kPoint.Z, kPoint);
                    kReg.m_kBSpline3D.setControlPoint(iControlX, iControlY, iControlZ, kPoint);
                }
            }
        }

        // Update the initial registered source and error images.
        if(!Preferences.isMultiThreadingEnabled()){
        	kReg.updateSamplesST(0, kImageTrg.extents[0] - 1, 0, kImageTrg.extents[1] - 1, 0, kImageTrg.extents[2] - 1);
        }else{
        	kReg.updateSamplesMT(0, kImageTrg.extents[0] - 1, 0, kImageTrg.extents[1] - 1, 0, kImageTrg.extents[2] - 1);        	
        }

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
        m_kBSplineBasisZ = null;
        m_kBSpline3D = null;

        super.finalize();
    }

    /**
     * Return access to the 3D discretized B-Spline internally created given the size of tbe input target image, and the
     * specified number of control points and degree of basis function for each B-Spline basis axis.
     *
     * @return  BSplineLattice3Df Access to 3D discretized B-Spline.
     */
    public BSplineLattice3Df getLattice() {
        return m_kBSpline3D;
    }

    /**
     * Move the specified control point using a gradient descent approach to minimize the error. The gradient descent
     * minimization is performed by sampling the error function at specified intervals. A maximum number of samples is
     * specified, but sampling always terminates when the bounding polyhedron formed by the 26 neighboring control
     * points is reached.
     *
     * @param  iControlX  int Identifies the control point in the 3D lattice.
     * @param  iControlY  int Identifies the control point in the 3D lattice.
     * @param  iControlZ  int Identifies the control point in the 3D lattice.
     * @param  iMaxSteps  int Maximum number of samples of the error function by moving the control point in the
     *                    direction of the negative gradient.
     * @param  fStepSize  float Spacing between the samples of the error function by moving the control point in the
     *                    direction of the negative gradient.
     */
    public void minimizeControlPoint(int iControlX, int iControlY, int iControlZ, int iMaxSteps, float fStepSize) {
//    	long startTime = System.currentTimeMillis();
        // Only allowed to move control points which are not anchored
        // to the boundary.
        if ((iControlX <= 0) || (iControlX >= (m_kBSplineBasisX.GetNumCtrlPoints() - 1))) {
            return;
        }

        if ((iControlY <= 0) || (iControlY >= (m_kBSplineBasisY.GetNumCtrlPoints() - 1))) {
            return;
        }

        if ((iControlZ <= 0) || (iControlZ >= (m_kBSplineBasisZ.GetNumCtrlPoints() - 1))) {
            return;
        }

        // How far to search?
        float fMaxDist = (float) iMaxSteps * fStepSize;

        // Compute the error derivative at the specified control point.
        Vector3f kDirection = new Vector3f();
//        long currentTime = System.currentTimeMillis();
        getErrorDeriv(iControlX, iControlY, iControlZ, kDirection);
//        System.out.println("Time consumed by getErrorDeriv() is " + (System.currentTimeMillis()-currentTime));
        
        kDirection.neg();

        if (0.0f == kDirection.length()) {
            return;
        }

        kDirection.normalize();

        // Compute how far the current control point is from the
        // boundary formed by its neighbor control points in the
        // direction of decreasing gradient.
//        currentTime = System.currentTimeMillis();
        fMaxDist = getControlPointMaxMoveDist(iControlX, iControlY, iControlZ, kDirection, fMaxDist);
//        System.out.println("Time consumed by getControlPointMaxMoveDist() is " + (System.currentTimeMillis()-currentTime));
        // Compute the error by moving the control point along
        // the decreasing gradient direction.  Move the point by
        // small steps up to the boundary formed by the neighboring
        // control points and then keep track of where the minimum
        // was found.
        Vector3f kOrigin = new Vector3f();
        m_kBSpline3D.getControlPoint(iControlX, iControlY, iControlZ, kOrigin);

        double dMinError = getError();


        float fMinErrorT = 0.0f;
        Vector3f kNewPoint = new Vector3f();
        
//        currentTime = System.currentTimeMillis();
        for (float fT = fStepSize; fT <= fMaxDist; fT += fStepSize) {
            kNewPoint.scaleAdd(fT, kDirection, kOrigin);
            m_kBSpline3D.setControlPoint(iControlX, iControlY, iControlZ, kNewPoint);
            updateControlPointSamples(iControlX, iControlY, iControlZ);

            double dError = getError();

            if (dError < dMinError) {
                dMinError = dError;
                fMinErrorT = fT;
            }
        }
//        System.out.println("Time consumed by loop is " + (System.currentTimeMillis()-currentTime));
       
        // Set the control point to the point along the ray where
        // the minimum was found.
        kNewPoint.scaleAdd(fMinErrorT, kDirection, kOrigin);
        m_kBSpline3D.setControlPoint(iControlX, iControlY, iControlZ, kNewPoint);
//        currentTime = System.currentTimeMillis();
        updateControlPointSamples(iControlX, iControlY, iControlZ);
//        System.out.println("Time consumed by updateControlPointSamples() is " + (System.currentTimeMillis()-currentTime));
    }

    /**
     * Move the control point from its current location to the requested location. The requested location must be inside
     * the polyhedron formed by its 26 neighboring control points. If not, then a line segment is created between the
     * current and requested locations and the point where this line segment intersects the bounding polyhedron is
     * found. This point is offset a little along the line segment so that it is just inside the bounding polyhedron,
     * and this point is taken as the new location for the specified control point.
     *
     * @param   iControlX  int Identifies the control point in the 3D lattice.
     * @param   iControlY  int Identifies the control point in the 3D lattice.
     * @param   iControlZ  int Identifies the control point in the 3D lattice.
     * @param   kPoint     Point3f Requested new coordinates for the specified control point.
     *
     * @return  float The distance between the updated control point position and its position before being modified.
     */
    public float moveControlPoint(int iControlX, int iControlY, int iControlZ, Vector3f kPoint) {

        // Create vector from current control point coordinates to the
        // newly specified one.  Clip the new control point coordinates
        // to a point along the ray inside the neighboring control point
        // polygon.
    	Vector3f kOrigin = new Vector3f();
        m_kBSpline3D.getControlPoint(iControlX, iControlY, iControlZ, kOrigin);

        Vector3f kDirection = Vector3f.sub(kPoint, kOrigin);

        float fDist = kDirection.length();

        if (0.0f == fDist) {
            return fDist;
        }

        kDirection.normalize();
        fDist = getControlPointMaxMoveDist(iControlX, iControlY, iControlZ, kDirection, fDist);

        Vector3f kNewPoint = new Vector3f();
        kNewPoint.scaleAdd(fDist, kDirection, kOrigin);
        m_kBSpline3D.setControlPoint(iControlX, iControlY, iControlZ, kNewPoint);
        updateControlPointSamples(iControlX, iControlY, iControlZ);

        return fDist;
    }

    /**
     * Compute the maximum distance the control point can be moved in the specified direction while keeping the control
     * point relative to its 26 neighboring control points such that "folding" does not occur.
     *
     * @param   iControlX        int Identifies the control point in the 3D lattice.
     * @param   iControlY        int Identifies the control point in the 3D lattice.
     * @param   iControlZ        int Identifies the control point in the 3D lattice.
     * @param   kRayDirection    Vector3f Direction from the current control point location for finding the distance to
     *                           the bounding polyhedron.
     * @param   fMaxDesiredDist  float Maximum distance along the ray direction that should be considered. At most, the
     *                           code would consider moving the control point.
     *
     * @return  float Computed distance.
     */
    protected float getControlPointMaxMoveDist(int iControlX, int iControlY, int iControlZ, Vector3f kRayDirection,
                                               float fMaxDesiredDist) {

        // Get the origin for the specified control point.
    	Vector3f kP = new Vector3f();
        m_kBSpline3D.getControlPoint(iControlX, iControlY, iControlZ, kP);

        // Loop through each triangle in the neighboring control point polyhedron.
        // Look for the intersection of the ray with each triangle and track
        // the closest one.
        boolean bRayIntersect = false;
        float fMaxMoveDist = 0.0f;
        Vector3f kP0 = new Vector3f();
        Vector3f kP1 = new Vector3f();
        Vector3f kP2 = new Vector3f();
        Vector3f kPT = new Vector3f();
        Vector3f kV1 = new Vector3f();
        Vector3f kV2 = new Vector3f();
        Vector3f kN = new Vector3f();
        Vector3f kV = new Vector3f();
        Vector2f kPP0 = new Vector2f();
        Vector2f kPP1 = new Vector2f();
        Vector2f kPP2 = new Vector2f();
        Vector2f kPP = new Vector2f();
        Vector2f kPV = new Vector2f();
        Vector2f kPVPerp = new Vector2f();

        for (int iTriangle = 0; iTriangle < 48; iTriangle++) {

            // Get the coordinates for the three vertices of the triangle.
            m_kBSpline3D.getControlPoint(ms_aaaiPolyhedronTriangleControlPointOffset[0][0][iTriangle] + iControlX,
                                         ms_aaaiPolyhedronTriangleControlPointOffset[1][0][iTriangle] + iControlY,
                                         ms_aaaiPolyhedronTriangleControlPointOffset[2][0][iTriangle] + iControlZ, kP0);
            m_kBSpline3D.getControlPoint(ms_aaaiPolyhedronTriangleControlPointOffset[0][1][iTriangle] + iControlX,
                                         ms_aaaiPolyhedronTriangleControlPointOffset[1][1][iTriangle] + iControlY,
                                         ms_aaaiPolyhedronTriangleControlPointOffset[2][1][iTriangle] + iControlZ, kP1);
            m_kBSpline3D.getControlPoint(ms_aaaiPolyhedronTriangleControlPointOffset[0][2][iTriangle] + iControlX,
                                         ms_aaaiPolyhedronTriangleControlPointOffset[1][2][iTriangle] + iControlY,
                                         ms_aaaiPolyhedronTriangleControlPointOffset[2][2][iTriangle] + iControlZ, kP2);

            // Create vectors which define plane containing the triangle vertices.
            kV1.copy(kP1).sub(kP0);
            kV2.copy(kP2).sub(kP0);
            kV1.normalize();
            kV2.normalize();
            
            // Create the normal vector of the triangle plane.
            kN.copy(kV1).cross(kV2);
            kN.normalize();

            // Find the intersection of the ray with the plane.
            //
            // A + uB + wC = D + tE
            //
            // where A=kP0, B=kV1, C=kV2, BxC=kN, D=kRayOrigin, E=kRayDirection
            // This is solved by:
            //
            // ((BxC)*A - (BxC)*D)
            // t = -------------------
            // ((BxC)*E)
            float fNdD = kN.dot(kRayDirection);

            if (0.0f == fNdD) {

                // ray and plane are parallel
                continue;
            }

            kV.copy(kP0).sub(kP);

            float fT = kN.dot(kV) / fNdD;

            if (fT <= 0.0f) {

                // Plane passes through point (should not happen)
                // or plane is "behind" direction in which ray is pointing.
                continue;
            }

            kPT.scaleAdd(fT, kRayDirection, kP);

            // Project each triangle vertex and ray-plane intersection vertex
            // onto the orthogonal axis plane which is most parallel to the
            // plane defined by the three vertices.  This axis is identified
            // by the axis of the plane normal vector with the largest magnitude.
            if ((Math.abs(kN.X) > Math.abs(kN.Y)) && (Math.abs(kN.X) > Math.abs(kN.Z))) {
                kPP0.set(kP0.Y, kP0.Z);
                kPP1.set(kP1.Y, kP1.Z);
                kPP2.set(kP2.Y, kP2.Z);
                kPP.set(kPT.Y, kPT.Z);
            } else if (Math.abs(kN.Y) > Math.abs(kN.Z)) {
                kPP0.set(kP0.X, kP0.Z);
                kPP1.set(kP1.X, kP1.Z);
                kPP2.set(kP2.X, kP2.Z);
                kPP.set(kPT.X, kPT.Z);
            } else {
                kPP0.set(kP0.X, kP0.Y);
                kPP1.set(kP1.X, kP1.Y);
                kPP2.set(kP2.X, kP2.Y);
                kPP.set(kPT.X, kPT.Y);
            }

            // Determine if this point is inside the triangle.
            kPV = Vector2f.sub(kPP1, kPP0);
            kPVPerp.X = +kPV.Y;
            kPVPerp.Y = -kPV.X;
            kPV = Vector2f.sub(kPP, kPP0);

            float fC0 = kPVPerp.dot(kPV);
            kPV = Vector2f.sub(kPP2, kPP1);
            kPVPerp.X = +kPV.Y;
            kPVPerp.Y = -kPV.X;
            kPV = Vector2f.sub(kPP, kPP1);

            float fC1 = kPVPerp.dot(kPV);
            kPV = Vector2f.sub(kPP0, kPP2);
            kPVPerp.X = +kPV.Y;
            kPVPerp.Y = -kPV.X;
            kPV = Vector2f.sub(kPP, kPP2);

            float fC2 = kPVPerp.dot(kPV);

            if (((fC0 * fC1) >= 0.0f) && ((fC0 * fC2) >= 0.0f)) {

                if (!bRayIntersect || (fT < fMaxMoveDist)) {
                    bRayIntersect = true;
                    fMaxMoveDist = fT;
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

            // Look at each tetrahedron formed by the center control point
            // and the triangles on the face of the polyhedron formed by
            // the 26 neighboring control points.  Use the determinant formulation
            // which leads the computation of the signed volume of each tetrahedron.
            // Because the triangles are consistently counterclockwise ordered,
            // the signed determinant which results should be positive.  If
            // the center control point was moved such that the "folding" of
            // the Bspline transformation can occur, then a negative determinant
            // will be the result.
            boolean bFolding = false;

            for (int iTriangle = 0; iTriangle < 48; iTriangle++) {

                // Get the coordinates for the three vertices of the triangle.
                m_kBSpline3D.getControlPoint(ms_aaaiPolyhedronTriangleControlPointOffset[0][0][iTriangle] + iControlX,
                                             ms_aaaiPolyhedronTriangleControlPointOffset[1][0][iTriangle] + iControlY,
                                             ms_aaaiPolyhedronTriangleControlPointOffset[2][0][iTriangle] + iControlZ,
                                             kP0);
                m_kBSpline3D.getControlPoint(ms_aaaiPolyhedronTriangleControlPointOffset[0][1][iTriangle] + iControlX,
                                             ms_aaaiPolyhedronTriangleControlPointOffset[1][1][iTriangle] + iControlY,
                                             ms_aaaiPolyhedronTriangleControlPointOffset[2][1][iTriangle] + iControlZ,
                                             kP1);
                m_kBSpline3D.getControlPoint(ms_aaaiPolyhedronTriangleControlPointOffset[0][2][iTriangle] + iControlX,
                                             ms_aaaiPolyhedronTriangleControlPointOffset[1][2][iTriangle] + iControlY,
                                             ms_aaaiPolyhedronTriangleControlPointOffset[2][2][iTriangle] + iControlZ,
                                             kP2);

                // Volume of tetrahedron is 1/6 times the (absolute value of the)
                // determinant of the following matrix:
                //
                // | P0.X P0.Y P0.Z  1 |
                // | P1.X P1.Y P1.Z  1 |
                // | P2.X P2.Y P2.Z  1 |
                // | PT.X PT.Y PT.Z  1 |
                //
                // If the determinant is negative, then the control points
                // are ordered such that "folding" occurs.
                float a0 = kP0.X, a1 = kP0.Y, a2 = kP0.Z;
                float b0 = kP1.X, b1 = kP1.Y, b2 = kP1.Z;
                float c0 = kP2.X, c1 = kP2.Y, c2 = kP2.Z;
                float d0 = kPT.X, d1 = kPT.Y, d2 = kPT.Z;
                float fDet = (-b0 * c1 * d2) + (a0 * c1 * d2) + (b1 * c0 * d2) - (a1 * c0 * d2) - (a0 * b1 * d2) +
                             (a1 * b0 * d2) + (b0 * c2 * d1) - (a0 * c2 * d1) - (b2 * c0 * d1) + (a2 * c0 * d1) +
                             (a0 * b2 * d1) - (a2 * b0 * d1) - (b1 * c2 * d0) + (a1 * c2 * d0) + (b2 * c1 * d0) -
                             (a2 * c1 * d0) - (a1 * b2 * d0) + (a2 * b1 * d0) + (a0 * b1 * c2) - (a1 * b0 * c2) -
                             (a0 * b2 * c1) + (a2 * b0 * c1) + (a1 * b2 * c0) - (a2 * b1 * c0);

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
     * @param  iControlX  int Identifies the control point in the 3D lattice.
     * @param  iControlY  int Identifies the control point in the 3D lattice.
     * @param  iControlZ  int Identifies the control point in the 3D lattice.
     * @param  kDeriv     Vector3f Vector to be filled with the derivative computed along each axis.
     */
    protected void getErrorDeriv(int iControlX, int iControlY, int iControlZ, Vector3f kDeriv) {
        // Compute the step of half a sample in each direction.
        float fSmallStepX = 0.5f / (float) (m_kBSplineBasisX.GetNumSamples() - 1);
        float fSmallStepY = 0.5f / (float) (m_kBSplineBasisY.GetNumSamples() - 1);
        float fSmallStepZ = 0.5f / (float) (m_kBSplineBasisZ.GetNumSamples() - 1);

        // Get the coordinates of the current control point.
        Vector3f kOrigin = new Vector3f();
        m_kBSpline3D.getControlPoint(iControlX, iControlY, iControlZ, kOrigin);

        Vector3f kNewPoint = new Vector3f();

        // Compute the error in the +X direction.
        kNewPoint.set(+fSmallStepX, 0.0f, 0.0f);
        kNewPoint.add(kOrigin);

        float fStepXPos = moveControlPoint(iControlX, iControlY, iControlZ, kNewPoint);
        
        double dErrorXPos = m_kRegMeasure.getError();
        m_kBSpline3D.setControlPoint(iControlX, iControlY, iControlZ, kOrigin);

        // Compute the error in the -X direction.
        kNewPoint.set(-fSmallStepX, 0.0f, 0.0f);
        kNewPoint.add(kOrigin);

        float fStepXNeg = moveControlPoint(iControlX, iControlY, iControlZ, kNewPoint);
        double dErrorXNeg = m_kRegMeasure.getError();
        m_kBSpline3D.setControlPoint(iControlX, iControlY, iControlZ, kOrigin);

        // Compute the error in the +Y direction.
        kNewPoint.set(0.0f, +fSmallStepY, 0.0f);
        kNewPoint.add(kOrigin);

        float fStepYPos = moveControlPoint(iControlX, iControlY, iControlZ, kNewPoint);
        double dErrorYPos = m_kRegMeasure.getError();
        m_kBSpline3D.setControlPoint(iControlX, iControlY, iControlZ, kOrigin);

        // Compute the error in the -Y direction.
        kNewPoint.set(0.0f, -fSmallStepY, 0.0f);
        kNewPoint.add(kOrigin);

        float fStepYNeg = moveControlPoint(iControlX, iControlY, iControlZ, kNewPoint);
        double dErrorYNeg = m_kRegMeasure.getError();
        m_kBSpline3D.setControlPoint(iControlX, iControlY, iControlZ, kOrigin);

        // Compute the error in the +Z direction.
        kNewPoint.set(0.0f, 0.0f, +fSmallStepZ).add(kOrigin);

        float fStepZPos = moveControlPoint(iControlX, iControlY, iControlZ, kNewPoint);
        double dErrorZPos = m_kRegMeasure.getError();
        m_kBSpline3D.setControlPoint(iControlX, iControlY, iControlZ, kOrigin);

        // Compute the error in the -Z direction.
        kNewPoint.set(0.0f, 0.0f, -fSmallStepZ).add(kOrigin);

        float fStepZNeg = moveControlPoint(iControlX, iControlY, iControlZ, kNewPoint);
        double dErrorZNeg = m_kRegMeasure.getError();
        m_kBSpline3D.setControlPoint(iControlX, iControlY, iControlZ, kOrigin);

        // After moving the control point back to the origin, we need
        // to update the samples affected by this move.
        updateControlPointSamples(iControlX, iControlY, iControlZ);
        // Compute gradient terms.
        float fStepX = fStepXPos + fStepXNeg;
        float fStepY = fStepYPos + fStepYNeg;
        float fStepZ = fStepZPos + fStepZNeg;
        kDeriv.X = (fStepX > 0.0f) ? ((float) (dErrorXPos - dErrorXNeg) / fStepX) : 0.0f;
        kDeriv.Y = (fStepY > 0.0f) ? ((float) (dErrorYPos - dErrorYNeg) / fStepY) : 0.0f;
        kDeriv.Z = (fStepZ > 0.0f) ? ((float) (dErrorZPos - dErrorZNeg) / fStepZ) : 0.0f;
    } 

    /**
     * Update the registered source image and the computed error as a result of making changes to a single control
     * point. This is the local control aspect of the B-Spline which means only those image samples which are affected
     * by the control point need to be updated.
     *
     * @param  iControlX  int Identifies the control point in the 3D lattice.
     * @param  iControlY  int Identifies the control point in the 3D lattice.
     * @param  iControlZ  int Identifies the control point in the 3D lattice.
     */
    protected void updateControlPointSamples(int iControlX, int iControlY, int iControlZ) {
    	if(!Preferences.isMultiThreadingEnabled()){
    		updateSamplesST(m_kBSplineBasisX.GetControlPointSampleIndexMin(iControlX),
                      m_kBSplineBasisX.GetControlPointSampleIndexMax(iControlX),
                      m_kBSplineBasisY.GetControlPointSampleIndexMin(iControlY),
                      m_kBSplineBasisY.GetControlPointSampleIndexMax(iControlY),
                      m_kBSplineBasisZ.GetControlPointSampleIndexMin(iControlZ),
                      m_kBSplineBasisZ.GetControlPointSampleIndexMax(iControlZ));
    	}else{
            updateSamplesMT(m_kBSplineBasisX.GetControlPointSampleIndexMin(iControlX),
                    m_kBSplineBasisX.GetControlPointSampleIndexMax(iControlX),
                    m_kBSplineBasisY.GetControlPointSampleIndexMin(iControlY),
                    m_kBSplineBasisY.GetControlPointSampleIndexMax(iControlY),
                    m_kBSplineBasisZ.GetControlPointSampleIndexMin(iControlZ),
                    m_kBSplineBasisZ.GetControlPointSampleIndexMax(iControlZ));
    	}
    }

    /**
     * Update the registered source image and the computed error but only for the specified range of samples.
     *
     * @param  iMinX  int Minimum X axis sample index.
     * @param  iMaxX  int Maximum X axis sample index.
     * @param  iMinY  int Minimum Y axis sample index.
     * @param  iMaxY  int Maximum Y axis sample index.
     * @param  iMinZ  int Minimum Z axis sample index.
     * @param  iMaxZ  int Maximum Z axis sample index.
     */
    protected void updateSamplesST(int iMinX, int iMaxX, int iMinY, int iMaxY, int iMinZ, int iMaxZ) {
        int iLimitX = m_iNumSamplesSrcX - 1;
        int iLimitY = m_iNumSamplesSrcY - 1;
        int iLimitZ = m_iNumSamplesSrcZ - 1;

        // compute local change to registered source
        Vector3f kPos = new Vector3f();

        for (int iZ = iMinZ; iZ <= iMaxZ; iZ++) {

            for (int iY = iMinY; iY <= iMaxY; iY++) {

                for (int iX = iMinX; iX <= iMaxX; iX++) {

                    // evaulate spline and setup for trilinear interpolation
                    m_kBSpline3D.getPosition(iX, iY, iZ, kPos);

                    float fX = iLimitX * kPos.X;
                    float fY = iLimitY * kPos.Y;
                    float fZ = iLimitZ * kPos.Z;
                    int iX0 = (int) fX;
                    int iY0 = (int) fY;
                    int iZ0 = (int) fZ;

                    if (iX0 >= iLimitX) {
                        iX0 = iLimitX - 1;
                        fX = (float) iLimitX;
                    }

                    if (iY0 >= iLimitY) {
                        iY0 = iLimitY - 1;
                        fY = (float) iLimitY;
                    }

                    if (iZ0 >= iLimitZ) {
                        iZ0 = iLimitZ - 1; 
                        fZ = (float) iLimitZ;
                    }

                    float fX1 = fX - iX0;
                    float fY1 = fY - iY0;
                    float fZ1 = fZ - iZ0;
                    float fX0 = 1.0f - fX1;
                    float fY0 = 1.0f - fY1;
                    float fZ0 = 1.0f - fZ1;

                    int iX0Y0Z0 = iX0 + (m_iNumSamplesSrcX * iY0) + (m_iNumSamplesSrcXY * iZ0);
                    int iX0Y1Z0 = iX0Y0Z0 + m_iNumSamplesSrcX;
                    int iX1Y0Z0 = iX0Y0Z0 + 1;
                    int iX1Y1Z0 = iX0Y1Z0 + 1;
                    int iX0Y0Z1 = iX0Y0Z0 + m_iNumSamplesSrcXY;
                    int iX0Y1Z1 = iX0Y0Z1 + m_iNumSamplesSrcX;
                    int iX1Y0Z1 = iX0Y0Z1 + 1;
                    int iX1Y1Z1 = iX0Y1Z1 + 1;

                    // interpolate across X
                    float fRegSrcY0Z0 = (m_kImageSrc.data[iX0Y0Z0] * fX0) + (m_kImageSrc.data[iX1Y0Z0] * fX1);
                    float fRegSrcY1Z0 = (m_kImageSrc.data[iX0Y1Z0] * fX0) + (m_kImageSrc.data[iX1Y1Z0] * fX1);
                    float fRegSrcY0Z1 = (m_kImageSrc.data[iX0Y0Z1] * fX0) + (m_kImageSrc.data[iX1Y0Z1] * fX1);
                    float fRegSrcY1Z1 = (m_kImageSrc.data[iX0Y1Z1] * fX0) + (m_kImageSrc.data[iX1Y1Z1] * fX1);

                    // interpolate across Y and the across Z
                    float fRegSrcZ0 = (fRegSrcY0Z0 * fY0) + (fRegSrcY1Z0 * fY1);
                    float fRegSrcZ1 = (fRegSrcY0Z1 * fY0) + (fRegSrcY1Z1 * fY1);
                    float fRegSrcValue = (fRegSrcZ0 * fZ0) + (fRegSrcZ1 * fZ1);

                    int iIndex = iX + (m_iNumSamplesTrgX * iY) + (m_iNumSamplesTrgXY * iZ);
                    m_kRegMeasure.updateRegistration(iIndex, fRegSrcValue);
                }
            }
        }
        //System.out.println("Time consumed by updateSamplesST(): " + (System.nanoTime()-startTime) + "," + ((iMaxX-iMinX)*(iMaxY-iMinY)*(iMaxZ-iMinZ)));
    }

    protected void updateSamplesMT(int iMinX, int iMaxX, int iMinY, int iMaxY, int iMinZ, int iMaxZ) {
    	int nx = 0;
    	int ny = 0;
    	int nz = 0;
    	if(nthreads == 16){
    		nx = 4;
    		ny = 2;
    		nz = 2;
    	}else if(nthreads == 8){
    		nx = 2;
    		ny = 2;
    		nz = 2;
    	}else if(nthreads == 4){
    		nx = 1;
    		ny = 2;
    		nz = 2;
    	}else{
    		nx = 1;
    		ny = 1;
    		nz = 2;
    	}
    	float stepX = (float)(iMaxX-iMinX+1)/nx;
    	float stepY = (float)(iMaxY-iMinY+1)/ny;
    	float stepZ = (float)(iMaxZ-iMinZ+1)/nz;
        final CountDownLatch doneSignal = new CountDownLatch(nthreads);
        for(int i = 0; i < nz; i++){
        	final int startZ = iMinZ + (int)(i*stepZ);
        	final int endZ = iMinZ + (int)((i+1)*stepZ);
        	for(int j = 0; j < ny; j++){
            	final int startY = iMinY + (int)(j*stepY);
            	final int endY = iMinY + (int)((j+1)*stepY);
        		for(int k = 0; k < nx; k++){
                	final int startX = iMinX + (int)(k*stepX);
                	final int endX = iMinX + (int)((k+1)*stepX);
                    Runnable task = new Runnable() {
                        public void run() {
                    		update(startX, endX, startY, endY, startZ, endZ);
                            doneSignal.countDown();
                        }
                    };
                    ThreadUtil.mipavThreadPool.execute(task);
        		}
        	}
    	}
        try {
            doneSignal.await();
        } catch (InterruptedException e) {
            e.printStackTrace();
        }
    }

    protected void update(int iMinX, int iMaxX, int iMinY, int iMaxY, int iMinZ, int iMaxZ) {
//    	long startTime = System.nanoTime();
        int iLimitX = m_iNumSamplesSrcX - 1;
        int iLimitY = m_iNumSamplesSrcY - 1;
        int iLimitZ = m_iNumSamplesSrcZ - 1;

        // compute local change to registered source
        Vector3f kPos = new Vector3f();

        for (int iZ = iMinZ; iZ < iMaxZ; iZ++) {

            for (int iY = iMinY; iY < iMaxY; iY++) {

                for (int iX = iMinX; iX < iMaxX; iX++) {

                    // evaulate spline and setup for trilinear interpolation
                    m_kBSpline3D.getPosition(iX, iY, iZ, kPos);

                    float fX = iLimitX * kPos.X;
                    float fY = iLimitY * kPos.Y;
                    float fZ = iLimitZ * kPos.Z;
                    int iX0 = (int) fX;
                    int iY0 = (int) fY;
                    int iZ0 = (int) fZ;

                    if (iX0 >= iLimitX) {
                        iX0 = iLimitX - 1;
                        fX = (float) iLimitX;
                    }

                    if (iY0 >= iLimitY) {
                        iY0 = iLimitY - 1;
                        fY = (float) iLimitY;
                    }

                    if (iZ0 >= iLimitZ) {
                        iZ0 = iLimitZ - 1;
                        fZ = (float) iLimitZ;
                    }

                    float fX1 = fX - iX0;
                    float fY1 = fY - iY0;
                    float fZ1 = fZ - iZ0;
                    float fX0 = 1.0f - fX1;
                    float fY0 = 1.0f - fY1;
                    float fZ0 = 1.0f - fZ1;

                    int iX0Y0Z0 = iX0 + (m_iNumSamplesSrcX * iY0) + (m_iNumSamplesSrcXY * iZ0);
                    int iX0Y1Z0 = iX0Y0Z0 + m_iNumSamplesSrcX;
                    int iX1Y0Z0 = iX0Y0Z0 + 1;
                    int iX1Y1Z0 = iX0Y1Z0 + 1;
                    int iX0Y0Z1 = iX0Y0Z0 + m_iNumSamplesSrcXY;
                    int iX0Y1Z1 = iX0Y0Z1 + m_iNumSamplesSrcX;
                    int iX1Y0Z1 = iX0Y0Z1 + 1;
                    int iX1Y1Z1 = iX0Y1Z1 + 1;

                    // interpolate across X
                    float fRegSrcY0Z0 = (m_kImageSrc.data[iX0Y0Z0] * fX0) + (m_kImageSrc.data[iX1Y0Z0] * fX1);
                    float fRegSrcY1Z0 = (m_kImageSrc.data[iX0Y1Z0] * fX0) + (m_kImageSrc.data[iX1Y1Z0] * fX1);
                    float fRegSrcY0Z1 = (m_kImageSrc.data[iX0Y0Z1] * fX0) + (m_kImageSrc.data[iX1Y0Z1] * fX1);
                    float fRegSrcY1Z1 = (m_kImageSrc.data[iX0Y1Z1] * fX0) + (m_kImageSrc.data[iX1Y1Z1] * fX1);

                    // interpolate across Y and the across Z
                    float fRegSrcZ0 = (fRegSrcY0Z0 * fY0) + (fRegSrcY1Z0 * fY1);
                    float fRegSrcZ1 = (fRegSrcY0Z1 * fY0) + (fRegSrcY1Z1 * fY1);
                    float fRegSrcValue = (fRegSrcZ0 * fZ0) + (fRegSrcZ1 * fZ1);

                    int iIndex = iX + (m_iNumSamplesTrgX * iY) + (m_iNumSamplesTrgXY * iZ);
                    m_kRegMeasure.updateRegistration(iIndex, fRegSrcValue);
                }
            }
        }
//        System.out.println("Time consumed by Update(): " + (System.nanoTime()-startTime) + "," + ((iMaxX-iMinX)*(iMaxY-iMinY)*(iMaxZ-iMinZ)));
    }

    /**
     * Called by the constructor to add the relative control point offset indices for triangle mesh representation of
     * the bounding polyhedron formed by the neighboring 26 control points. This method is only called for the 8
     * triangles on a "face" which result from holding one of the axes at a fixed offset of -1 or +1. Hence this method
     * would need to be called once for each of the six "faces". All of the triangles go into a single table where order
     * does not matter.
     *
     * @param  iTriangleOffset  int Offset into triangle array for current "face". Should be one the following values:
     *                          {0,8,16,24,32,40}.
     * @param  iAxisA           int Index {0,1,2} for {X,Y,Z} axis to be assigned to "face" relative A (X) axis.
     * @param  iAxisB           int Index {0,1,2} for {X,Y,Z} axis to be assigned to "face" relative B (Y) axis.
     * @param  iAxisC           int Index {0,1,2} for {X,Y,Z} axis to be assigned to "face" relative C (Z) axis.
     * @param  iOffsetAxisC     int Fixed control point index relative offset value -1 or +1 to be assigned to "face"
     *                          relative C (Z) axis.
     */
    private static void initPolyhedronTrimeshConnectivity(int iTriangleOffset, int iAxisA, int iAxisB, int iAxisC,
                                                          int iOffsetAxisC) {

        // The connectivity is defined for the 8 triangles connecting
        // the control points by not varying those at index iAxisC;
        // i.e., only those on iAxisA and iAxisB are selected.
        // The following is a sketch of the neighboring control points
        // and how their triangle connectivity is selected.
        //
        // +-+-+
        // |/|\|
        // +-+-+
        // |\|-|
        // +-+-+
        //
        // Note that the ordering of the vertices in the triangles
        // *does matter* so that we get consistent counterclockwise
        // ordering.

        int[][] aaiTriangleAxisA = new int[3][];
        int[][] aaiTriangleAxisB = new int[3][];
        int[][] aaiTriangleAxisC = new int[3][];
        aaiTriangleAxisA[0] = ms_aaaiPolyhedronTriangleControlPointOffset[iAxisA][0];
        aaiTriangleAxisB[0] = ms_aaaiPolyhedronTriangleControlPointOffset[iAxisB][0];
        aaiTriangleAxisC[0] = ms_aaaiPolyhedronTriangleControlPointOffset[iAxisC][0];

        if (iOffsetAxisC < 0) {

            // for negative face indexes, reverse the ordering of the
            // second and third points to maintain consistent
            // counterclockwise ordering
            aaiTriangleAxisA[1] = ms_aaaiPolyhedronTriangleControlPointOffset[iAxisA][2];
            aaiTriangleAxisB[1] = ms_aaaiPolyhedronTriangleControlPointOffset[iAxisB][2];
            aaiTriangleAxisC[1] = ms_aaaiPolyhedronTriangleControlPointOffset[iAxisC][2];

            aaiTriangleAxisA[2] = ms_aaaiPolyhedronTriangleControlPointOffset[iAxisA][1];
            aaiTriangleAxisB[2] = ms_aaaiPolyhedronTriangleControlPointOffset[iAxisB][1];
            aaiTriangleAxisC[2] = ms_aaaiPolyhedronTriangleControlPointOffset[iAxisC][1];
        } else {

            // for positive face indexes, this ordering of the second
            // and third points defines consistent counterclockwise ordering
            aaiTriangleAxisA[1] = ms_aaaiPolyhedronTriangleControlPointOffset[iAxisA][1];
            aaiTriangleAxisB[1] = ms_aaaiPolyhedronTriangleControlPointOffset[iAxisB][1];
            aaiTriangleAxisC[1] = ms_aaaiPolyhedronTriangleControlPointOffset[iAxisC][1];

            aaiTriangleAxisA[2] = ms_aaaiPolyhedronTriangleControlPointOffset[iAxisA][2];
            aaiTriangleAxisB[2] = ms_aaaiPolyhedronTriangleControlPointOffset[iAxisB][2];
            aaiTriangleAxisC[2] = ms_aaaiPolyhedronTriangleControlPointOffset[iAxisC][2];
        }

        // Triangle 0: (0,0) (1,0) (0,1)
        aaiTriangleAxisA[0][0 + iTriangleOffset] = 0;
        aaiTriangleAxisB[0][0 + iTriangleOffset] = 0;
        aaiTriangleAxisC[0][0 + iTriangleOffset] = iOffsetAxisC;
        aaiTriangleAxisA[1][0 + iTriangleOffset] = 1;
        aaiTriangleAxisB[1][0 + iTriangleOffset] = 0;
        aaiTriangleAxisC[1][0 + iTriangleOffset] = iOffsetAxisC;
        aaiTriangleAxisA[2][0 + iTriangleOffset] = 0;
        aaiTriangleAxisB[2][0 + iTriangleOffset] = 1;
        aaiTriangleAxisC[2][0 + iTriangleOffset] = iOffsetAxisC;

        // Triangle 1: (0,0) (0,1) (-1,0)
        aaiTriangleAxisA[0][1 + iTriangleOffset] = 0;
        aaiTriangleAxisB[0][1 + iTriangleOffset] = 0;
        aaiTriangleAxisC[0][1 + iTriangleOffset] = iOffsetAxisC;
        aaiTriangleAxisA[1][1 + iTriangleOffset] = 0;
        aaiTriangleAxisB[1][1 + iTriangleOffset] = 1;
        aaiTriangleAxisC[1][1 + iTriangleOffset] = iOffsetAxisC;
        aaiTriangleAxisA[2][1 + iTriangleOffset] = -1;
        aaiTriangleAxisB[2][1 + iTriangleOffset] = 0;
        aaiTriangleAxisC[2][1 + iTriangleOffset] = iOffsetAxisC;

        // Triangle 2: (0,0) (-1,0) (0,-1)
        aaiTriangleAxisA[0][2 + iTriangleOffset] = 0;
        aaiTriangleAxisB[0][2 + iTriangleOffset] = 0;
        aaiTriangleAxisC[0][2 + iTriangleOffset] = iOffsetAxisC;
        aaiTriangleAxisA[1][2 + iTriangleOffset] = -1;
        aaiTriangleAxisB[1][2 + iTriangleOffset] = 0;
        aaiTriangleAxisC[1][2 + iTriangleOffset] = iOffsetAxisC;
        aaiTriangleAxisA[2][2 + iTriangleOffset] = 0;
        aaiTriangleAxisB[2][2 + iTriangleOffset] = -1;
        aaiTriangleAxisC[2][2 + iTriangleOffset] = iOffsetAxisC;

        // Triangle 3: (0,0) (0,-1) (1,0)
        aaiTriangleAxisA[0][3 + iTriangleOffset] = 0;
        aaiTriangleAxisB[0][3 + iTriangleOffset] = 0;
        aaiTriangleAxisC[0][3 + iTriangleOffset] = iOffsetAxisC;
        aaiTriangleAxisA[1][3 + iTriangleOffset] = 0;
        aaiTriangleAxisB[1][3 + iTriangleOffset] = -1;
        aaiTriangleAxisC[1][3 + iTriangleOffset] = iOffsetAxisC;
        aaiTriangleAxisA[2][3 + iTriangleOffset] = 1;
        aaiTriangleAxisB[2][3 + iTriangleOffset] = 0;
        aaiTriangleAxisC[2][3 + iTriangleOffset] = iOffsetAxisC;

        // Triangle 4: (1,1) (0,1) (1,0)
        aaiTriangleAxisA[0][4 + iTriangleOffset] = 1;
        aaiTriangleAxisB[0][4 + iTriangleOffset] = 1;
        aaiTriangleAxisC[0][4 + iTriangleOffset] = iOffsetAxisC;
        aaiTriangleAxisA[1][4 + iTriangleOffset] = 0;
        aaiTriangleAxisB[1][4 + iTriangleOffset] = 1;
        aaiTriangleAxisC[1][4 + iTriangleOffset] = iOffsetAxisC;
        aaiTriangleAxisA[2][4 + iTriangleOffset] = 1;
        aaiTriangleAxisB[2][4 + iTriangleOffset] = 0;
        aaiTriangleAxisC[2][4 + iTriangleOffset] = iOffsetAxisC;

        // Triangle 5: (-1,1) (-1,0) (0,1)
        aaiTriangleAxisA[0][5 + iTriangleOffset] = -1;
        aaiTriangleAxisB[0][5 + iTriangleOffset] = 1;
        aaiTriangleAxisC[0][5 + iTriangleOffset] = iOffsetAxisC;
        aaiTriangleAxisA[1][5 + iTriangleOffset] = -1;
        aaiTriangleAxisB[1][5 + iTriangleOffset] = 0;
        aaiTriangleAxisC[1][5 + iTriangleOffset] = iOffsetAxisC;
        aaiTriangleAxisA[2][5 + iTriangleOffset] = 0;
        aaiTriangleAxisB[2][5 + iTriangleOffset] = 1;
        aaiTriangleAxisC[2][5 + iTriangleOffset] = iOffsetAxisC;

        // Triangle 6: (-1,-1) (0,-1) (-1,0)
        aaiTriangleAxisA[0][6 + iTriangleOffset] = -1;
        aaiTriangleAxisB[0][6 + iTriangleOffset] = -1;
        aaiTriangleAxisC[0][6 + iTriangleOffset] = iOffsetAxisC;
        aaiTriangleAxisA[1][6 + iTriangleOffset] = 0;
        aaiTriangleAxisB[1][6 + iTriangleOffset] = -1;
        aaiTriangleAxisC[1][6 + iTriangleOffset] = iOffsetAxisC;
        aaiTriangleAxisA[2][6 + iTriangleOffset] = -1;
        aaiTriangleAxisB[2][6 + iTriangleOffset] = 0;
        aaiTriangleAxisC[2][6 + iTriangleOffset] = iOffsetAxisC;

        // Triangle 7: (1,-1) (1,0) (0,-1)
        aaiTriangleAxisA[0][7 + iTriangleOffset] = 1;
        aaiTriangleAxisB[0][7 + iTriangleOffset] = -1;
        aaiTriangleAxisC[0][7 + iTriangleOffset] = iOffsetAxisC;
        aaiTriangleAxisA[1][7 + iTriangleOffset] = 1;
        aaiTriangleAxisB[1][7 + iTriangleOffset] = 0;
        aaiTriangleAxisC[1][7 + iTriangleOffset] = iOffsetAxisC;
        aaiTriangleAxisA[2][7 + iTriangleOffset] = 0;
        aaiTriangleAxisB[2][7 + iTriangleOffset] = -1;
        aaiTriangleAxisC[2][7 + iTriangleOffset] = iOffsetAxisC;
    }
}
