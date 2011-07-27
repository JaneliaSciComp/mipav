package gov.nih.mipav.model.algorithms;

import WildMagic.LibFoundation.Curves.*;
import WildMagic.LibFoundation.Mathematics.*;
import gov.nih.mipav.model.structures.*;

import gov.nih.mipav.view.*;

import java.io.*;



/**
 * TransformBSpline algorithm This algorithm takes a source image and uses information read in from a .nlt file to
 * perform a nonlinear B-Spline transformation on the image. The dimensions of the transformed image, the degree of the
 * B-Spline(ranging from 1 to 4), the number of control points, and the values of the control points are obtained from
 * the .nlt file. For the .nlt file to be usable, the dimensions of the source image used in the .nlt file must be the
 * same as the dimensions of the source image used in the algorithm.
 */

public class AlgorithmTransformBSpline extends AlgorithmBase {

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** DOCUMENT ME! */
    private float[][] controlMat;

    /** DOCUMENT ME! */
    private float[][][] controlMat25D;

    /** DOCUMENT ME! */
    private int[] destExtents;

    /** DOCUMENT ME! */
    private boolean have25D = false;

    /** DOCUMENT ME! */
    private int m_iBSplineDegree;

    /** DOCUMENT ME! */
    private int m_iBSplineNumControlPoints;

    /** DOCUMENT ME! */
    private BSplineLattice2Df m_kBSpline2D;

    /** DOCUMENT ME! */
    private BSplineLattice3Df m_kBSpline3D;

    /** 2D and 3D B-Spline basis definitions. */
    private BSplineBasisDiscretef m_kBSplineBasisX;

    /** DOCUMENT ME! */
    private BSplineBasisDiscretef m_kBSplineBasisY;

    /** DOCUMENT ME! */
    private BSplineBasisDiscretef m_kBSplineBasisZ;

    /** DOCUMENT ME! */
    private ModelImage m_kImageResult;

    /** DOCUMENT ME! */
    private ModelImage m_kImageSource;

    //private ModelSimpleImage m_kSimpleImageResult;

    /** DOCUMENT ME! */
    private ModelSimpleImage m_kSimpleImageSource;

    /** DOCUMENT ME! */
    private float[] outResolutions;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * AlgorithmTransformBSpline - Constructor.
     *
     * @param  kImageSource              image that the B-Spline transformation is applied to
     * @param  afResolutions             sample resolutions for output image
     * @param  iBSplineDegree            degree of the B-Spline (same in all dimensions)
     * @param  iBSplineNumControlPoints  number of control points (same in all dimensions)
     * @param  controlMat25D             matrix containing the control point values The first index is the slice number
     *                                   The second index is used to select the point with controlY varying the fastest
     *                                   and controlX varying the slowest The second index is used to select the x or y
     *                                   value
     */

    public AlgorithmTransformBSpline(ModelImage kImageSource, float[] afResolutions, int iBSplineDegree,
                                     int iBSplineNumControlPoints, float[][][] controlMat25D) {

        super();

        // Save a copy of all the input parameters.
        m_kImageSource = kImageSource;
        m_iBSplineDegree = iBSplineDegree;
        m_iBSplineNumControlPoints = iBSplineNumControlPoints;
        this.outResolutions = afResolutions;
        this.controlMat25D = controlMat25D;
        m_kSimpleImageSource = new ModelSimpleImage(kImageSource);

        // Allocate linear arrays to store the image values.
        have25D = true;
    }

    /**
     * AlgorithmTransformBSpline - Constructor.
     *
     * @param  kImageSource              image that the B-Spline transformation is applied to
     * @param  afResolutions             sample resolutions for output image
     * @param  destExtents               dimensions of the transformed image
     * @param  iBSplineDegree            degree of the B-Spline (same in all dimensions)
     * @param  iBSplineNumControlPoints  number of control points (same in all dimensions)
     * @param  controlMat                matrix containing the control point values The first index is used to select
     *                                   the point with controlZ varying the fastest and controlX varying the slowest
     *                                   The second index is used to select the x, y, or z value
     */

    public AlgorithmTransformBSpline(ModelImage kImageSource, float[] afResolutions, int[] destExtents,
                                     int iBSplineDegree, int iBSplineNumControlPoints, float[][] controlMat) {

        super();

        // Save a copy of all the input parameters.
        m_kImageSource = kImageSource;
        this.destExtents = destExtents;
        this.outResolutions = afResolutions;
        m_iBSplineDegree = iBSplineDegree;
        m_iBSplineNumControlPoints = iBSplineNumControlPoints;
        this.controlMat = controlMat;
        m_kSimpleImageSource = new ModelSimpleImage(kImageSource);
    }

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * Prepares this class for destruction.
     */
    public void disposeLocal() {
        int i;

        m_kImageSource = null;
        m_kSimpleImageSource = null;
        //m_kSimpleImageResult = null;
        destExtents = null;

        if (controlMat != null) {

            for (i = 0; i < controlMat.length; i++) {
                controlMat[i] = null;
            }

            controlMat = null;
            m_kBSplineBasisX = null;
            m_kBSplineBasisY = null;
            m_kBSplineBasisZ = null;
            m_kBSpline2D = null;
            m_kBSpline3D = null;
        }

        System.gc();
    }

    /**
     * DOCUMENT ME!
     */
    public void finalize() {
        disposeLocal();
        super.finalize();
    }

    /**
     * DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    public ModelImage getTransformedImage() {
        return m_kImageResult;
    }

    /**
     * starts the program.
     */
    public void runAlgorithm() {
        

        try {

            if (have25D) {
                int[] aiSliceExtents = new int[] { m_kSimpleImageSource.extents[0], m_kSimpleImageSource.extents[1] };
                int numberSlices = m_kSimpleImageSource.extents[2];

                // Setup to use the progress bar.
                fireProgressStateChanged(m_kImageSource.getImageName(), "Transforming ...");


                // Create result image.
                try {

                    if (m_kImageSource.getType() == ModelStorageBase.ARGB) {
                        m_kImageResult = new ModelImage(ModelStorageBase.ARGB, m_kSimpleImageSource.extents,
                                                        m_kImageSource.getImageName() + "_registered");
                    } else if (m_kImageSource.getType() == ModelStorageBase.ARGB_USHORT) {
                        m_kImageResult = new ModelImage(ModelStorageBase.ARGB_USHORT, m_kSimpleImageSource.extents,
                                                        m_kImageSource.getImageName() + "_registered");
                    } else {
                        m_kImageResult = new ModelImage(ModelStorageBase.FLOAT, m_kSimpleImageSource.extents,
                                                        m_kImageSource.getImageName() + "_registered");
                    }
                } catch (OutOfMemoryError x) {
                    MipavUtil.displayError("AlgorithmTransformBSpline: unable to allocate enough memory");

                    if (m_kImageResult != null) {
                        m_kImageResult.disposeLocal(); // Clean up memory of result image
                        m_kImageResult = null;
                    }

                    setCompleted(false);
                    finalize();

                    return;
                }

                for (int slice = 0; slice < numberSlices; slice++) {
                    m_kImageResult.getFileInfo(slice).setResolutions(outResolutions);
                }

                //m_kSimpleImageResult = new ModelSimpleImage(m_kImageResult.getExtents(), outResolutions,
                                                            //m_kImageResult.isColorImage());

                // create B-spline lattice
                m_kBSplineBasisX = new BSplineBasisDiscretef(m_iBSplineNumControlPoints, m_iBSplineDegree,
                                                             m_kSimpleImageSource.extents[0]);
                m_kBSplineBasisY = new BSplineBasisDiscretef(m_iBSplineNumControlPoints, m_iBSplineDegree,
                                                             m_kSimpleImageSource.extents[1]);
                m_kBSpline2D = new BSplineLattice2Df(m_kBSplineBasisX, m_kBSplineBasisY);

                Vector2f kPoint = new Vector2f();

                for (int slice = 0; slice < numberSlices; slice++) {
                    fireProgressStateChanged(100 * slice / numberSlices);

                    ModelSimpleImage kSimpleImageSlice = new ModelSimpleImage(aiSliceExtents, outResolutions,
                                                                              m_kImageSource, slice);

                    int index = 0;

                    for (int iControlX = 0; iControlX < m_iBSplineNumControlPoints; iControlX++) {

                        for (int iControlY = 0; iControlY < m_iBSplineNumControlPoints; iControlY++) {
                            kPoint.X = controlMat25D[slice][index][0];
                            kPoint.Y = controlMat25D[slice][index++][1];
                            m_kBSpline2D.setControlPoint(iControlX, iControlY, kPoint);
                        }
                    }

                    ModelSimpleImage[] akSimpleImageSourceMap = m_kBSpline2D.createImageMap(aiSliceExtents[0],
                                                                                            aiSliceExtents[1]);
                    ModelSimpleImage kSimpleImageResult = kSimpleImageSlice.createMappedImage2d(akSimpleImageSourceMap[0],
                                                                                                akSimpleImageSourceMap[1]);
                    akSimpleImageSourceMap = null;

                    try {
                        m_kImageResult.importData(slice * kSimpleImageResult.data.length, kSimpleImageResult.data,
                                                  true); // compute min/max
                    } catch (IOException e) {
                        throw new RuntimeException("IOException on m_kImageResult.importData: " + e.getMessage());
                    }

                    kSimpleImageResult = null;
                }

            } // if (have25D)
            else { // !have25D

                // Setup to use the progress bar.
                fireProgressStateChanged(m_kImageSource.getImageName(), "Transforming ...");


                // Create result image.
                try {

                    if (m_kImageSource.getType() == ModelStorageBase.ARGB) {
                        m_kImageResult = new ModelImage(ModelStorageBase.ARGB, destExtents,
                                                        m_kImageSource.getImageName() + "_registered");
                    } else if (m_kImageSource.getType() == ModelStorageBase.ARGB_USHORT) {
                        m_kImageResult = new ModelImage(ModelStorageBase.ARGB_USHORT, destExtents,
                                                        m_kImageSource.getImageName() + "_registered");
                    } else {
                        m_kImageResult = new ModelImage(ModelStorageBase.FLOAT, destExtents,
                                                        m_kImageSource.getImageName() + "_registered");
                    }
                } catch (OutOfMemoryError x) {
                    MipavUtil.displayError("AlgorithmTransformBSpline: unable to allocate enough memory");

                    if (m_kImageResult != null) {
                        m_kImageResult.disposeLocal(); // Clean up memory of result image
                        m_kImageResult = null;
                    }

                    setCompleted(false);
                    finalize();

                    return;
                }

                // 2D
                if (m_kImageSource.getNDims() == 2) {

                    // Setup resolutions.
                    m_kImageResult.getFileInfo(0).setResolutions(outResolutions);

                    // Setup access to image data.
                    //m_kSimpleImageResult = new ModelSimpleImage(m_kImageResult.getExtents(), outResolutions,
                                                                //m_kImageResult.isColorImage());

                    // create B-spline lattice
                    m_kBSplineBasisX = new BSplineBasisDiscretef(m_iBSplineNumControlPoints, m_iBSplineDegree,
                                                                 destExtents[0]);
                    m_kBSplineBasisY = new BSplineBasisDiscretef(m_iBSplineNumControlPoints, m_iBSplineDegree,
                                                                 destExtents[1]);
                    m_kBSpline2D = new BSplineLattice2Df(m_kBSplineBasisX, m_kBSplineBasisY);

                    Vector2f kPoint = new Vector2f();

                    int index = 0;

                    for (int iControlX = 0; iControlX < m_iBSplineNumControlPoints; iControlX++) {

                        for (int iControlY = 0; iControlY < m_iBSplineNumControlPoints; iControlY++) {
                            kPoint.X = controlMat[index][0];
                            kPoint.Y = controlMat[index++][1];
                            m_kBSpline2D.setControlPoint(iControlX, iControlY, kPoint);
                        }
                    }

                    ModelSimpleImage[] akSimpleImageSourceMap = m_kBSpline2D.createImageMap(destExtents[0],
                                                                                            destExtents[1]);
                    ModelSimpleImage kSimpleImageResult = m_kSimpleImageSource.createMappedImage2d(akSimpleImageSourceMap[0],
                                                                                                   akSimpleImageSourceMap[1]);
                    akSimpleImageSourceMap = null;

                    try {
                        m_kImageResult.importData(0, kSimpleImageResult.data, true); // compute min/max
                    } catch (IOException e) {
                        throw new RuntimeException("IOException on m_kImageResult.importData: " + e.getMessage());
                    }

                    kSimpleImageResult = null;

                } // 3D
                else if (m_kImageSource.getNDims() == 3) {

                    // Setup resolutions.
                    for (int slice = 0; slice < m_kImageResult.getExtents()[2]; slice++) {
                        m_kImageResult.getFileInfo(slice).setResolutions(outResolutions);
                    }

                    // Setup access to image data.
                    //m_kSimpleImageResult = new ModelSimpleImage(m_kImageResult.getExtents(), outResolutions,
                                                                //m_kImageResult.isColorImage());

                    // create B-spline lattice
                    m_kBSplineBasisX = new BSplineBasisDiscretef(m_iBSplineNumControlPoints, m_iBSplineDegree,
                                                                 destExtents[0]);
                    m_kBSplineBasisY = new BSplineBasisDiscretef(m_iBSplineNumControlPoints, m_iBSplineDegree,
                                                                 destExtents[1]);
                    m_kBSplineBasisZ = new BSplineBasisDiscretef(m_iBSplineNumControlPoints, m_iBSplineDegree,
                                                                 destExtents[2]);
                    m_kBSpline3D = new BSplineLattice3Df(m_kBSplineBasisX, m_kBSplineBasisY, m_kBSplineBasisZ);

                    Vector3f kPoint = new Vector3f();

                    int index = 0;

                    for (int iControlX = 0; iControlX < m_iBSplineNumControlPoints; iControlX++) {

                        for (int iControlY = 0; iControlY < m_iBSplineNumControlPoints; iControlY++) {

                            for (int iControlZ = 0; iControlZ < m_iBSplineNumControlPoints; iControlZ++) {
                                kPoint.X = controlMat[index][0];
                                kPoint.Y = controlMat[index][1];
                                kPoint.Z = controlMat[index++][2];
                                m_kBSpline3D.setControlPoint(iControlX, iControlY, iControlZ, kPoint);
                            }
                        }
                    }

                    ModelSimpleImage[] akSimpleImageSourceMap = m_kBSpline3D.createImageMap(destExtents[0],
                                                                                            destExtents[1],
                                                                                            destExtents[2]);
                    ModelSimpleImage kSimpleImageResult = m_kSimpleImageSource.createMappedImage3d(akSimpleImageSourceMap[0],
                                                                                                   akSimpleImageSourceMap[1],
                                                                                                   akSimpleImageSourceMap[2]);
                    akSimpleImageSourceMap = null;

                    try {
                        m_kImageResult.importData(0, kSimpleImageResult.data, true); // compute min/max
                    } catch (IOException e) {
                        throw new RuntimeException("IOException on m_kImageResult.importData: " + e.getMessage());
                    }

                    kSimpleImageResult = null;

                } // Unsupported.
                else {
                    throw new RuntimeException("AlgorithmTransformBSpline: only 2D, 3D, and 2.5D supported");
                }
            } // !have25D

            // If we get here, we successfully completed registration given
            // the input specifications, but only if we were not terminated.
            if (!isThreadStopped()) {
                setCompleted(true);
            }
        } catch (RuntimeException e) {
            errorCleanUp("AlgorithmTransformBSpline: " + e.getMessage(), true);
        }

        disposeLocal();

    }
}
