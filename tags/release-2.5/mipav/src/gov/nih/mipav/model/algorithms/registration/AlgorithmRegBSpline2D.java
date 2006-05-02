package gov.nih.mipav.model.algorithms.registration;


import gov.nih.mipav.model.structures.*;

import gov.nih.mipav.view.*;

import java.io.*;

import java.text.*;

import javax.vecmath.*;


/**
 * BSpline based registration of 2D images.
 */
public class AlgorithmRegBSpline2D extends AlgorithmRegBSpline {

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Constructor.
     *
     * @param  kImageResult       ModelImage Reference to an image to store the registered source image. This image must
     *                            have the same dimensions as the registration target image.
     * @param  kImageSource       ModelImage Reference to the original input image to use as the registration source.
     *                            This image does not have to have the same dimensions as the image to be used as the
     *                            registration target.
     * @param  kImageTarget       ModelImage Reference to the original input image to use as the registration target.
     * @param  kImageDeformation  ModelImage Reference to an image to store the computed deformation. This may be a null
     *                            reference to indicate that the deformation is not to be computed. If this reference is
     *                            not null, then the image must have the same dimensions as the target image.
     * @param  kRegMeasure        RegistrationMeasure Reference to the particular cost measure to use for this
     *                            registration. The cost measure is the same for all passes.
     * @param  kOptionsPass1      Options Options to use for a first pass of registration. This may *not* be a null
     *                            reference.
     * @param  kOptionsPass2      Options Options to use for a second pass of registration. This may be null to indicate
     *                            that only a single pass of registration is to be performed.
     */
    public AlgorithmRegBSpline2D(ModelImage kImageResult, ModelImage kImageSource, ModelImage kImageTarget,
                                 ModelImage kImageDeformation, RegistrationMeasure kRegMeasure, Options kOptionsPass1,
                                 Options kOptionsPass2) {

        super(kImageResult, kImageSource, kImageTarget, kImageDeformation, kRegMeasure, kOptionsPass1, kOptionsPass2);
    }

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * Prepares this class for destruction.
     */
    public void disposeLocal() { }

    /**
     * DOCUMENT ME!
     */
    public void finalize() {
        disposeLocal();
        super.finalize();
    }

    /**
     * AlgorithmBase abstract method implementation which performs the registration.
     *
     * @throws  RuntimeException  DOCUMENT ME!
     */
    public void runAlgorithm() {

        // Create ModelSimpleImage instances of the input ModelImage instances.
        // ModelSimpleImage creates the data array needed to access the
        // samples in the image in a simple manner.
        ModelSimpleImage kSimpleImageSource = new ModelSimpleImage(m_kImageSource);
        ModelSimpleImage kSimpleImageTarget = new ModelSimpleImage(m_kImageTarget);

        // Mark the original source image and then create intensity-only
        // versions of the source and target to be used for registration.
        ModelSimpleImage kSimpleImageSourceOrig = kSimpleImageSource;
        final float fOneThird = 1.0f / 3.0f;

        if (kSimpleImageSource.isColor) {
            kSimpleImageSource = kSimpleImageSource.createIntensityImage(fOneThird, fOneThird, fOneThird);
        }

        if (kSimpleImageTarget.isColor) {
            kSimpleImageTarget = kSimpleImageTarget.createIntensityImage(fOneThird, fOneThird, fOneThird);
        }

        // Setup
        buildProgressBar(m_kImageSource.getImageName(), "Registering ...", 0, 100);

        boolean bMultiPass = (null != m_kOptionsPass2);

        // Pass 1
        BSplineRegistration2Df kReg = runPass(kSimpleImageSource, kSimpleImageTarget, m_kOptionsPass1, null,
                                              bMultiPass ? "Pass 1: " : "");

        // Optional Pass 2
        if ((null != m_kOptionsPass2) && !isThreadStopped()) {
            kReg = runPass(kSimpleImageSource, kSimpleImageTarget, m_kOptionsPass2, kReg, "Pass 2: ");
        }

        // If we get here, we successfully completed registration given
        // the input specifications, but only if we were not terminated.
        if (!isThreadStopped()) {
            setCompleted(true);

            // Make sure registration is to original target without subsampling.
            kReg = kReg.createSameMapping(kSimpleImageTarget);

            try {

                // Get source image map from the registration and use that
                // to resample the original source image to create the
                // registered source image.
                ModelSimpleImage[] akSimpleImageSourceMap = kReg.createImageSourceMap();
                ModelSimpleImage kSimpleImageResult = kSimpleImageSourceOrig.createMappedImage2d(akSimpleImageSourceMap[0],
                                                                                                 akSimpleImageSourceMap[1]);
                akSimpleImageSourceMap = null;
                m_kImageResult.importData(0, kSimpleImageResult.data, true);
                kSimpleImageResult = null;

                // Access the deformation image if specified.
                if (null != m_kImageDeformation) {
                    ModelSimpleImage kImageDataDeformation = kReg.createImageDeformation();
                    m_kImageDeformation.importData(0, kImageDataDeformation.data, true);
                    kImageDataDeformation = null;
                }
            } catch (IOException e) {
                throw new RuntimeException("IOException on m_kImageResult.importData: " + e.getMessage());
            }
        }

        kSimpleImageSource = null;
        kSimpleImageTarget = null;

        if (!isThreadStopped()) {
            writeResults(kReg);
        }

        kReg = null;
        disposeLocal();
        disposeProgressBar();
    }

    /**
     * Perform a "pass" of BSpline based registration. A "pass" is defined to be one or more iterations using the
     * BSpline basis parameters, gradient descent parameters, and convergence parameters.
     *
     * @param   kSource                ModelSimpleImage Reference to the original input image to use as the registration
     *                                 source. This image does not have to have the same dimensions as the image to be
     *                                 used as the registration target.
     * @param   kTarget                ModelSimpleImage Reference to the original input image to use as the registration
     *                                 target.
     * @param   kOptions               Options Options to use for a this registration pass. This may *not* be a null
     *                                 reference.
     * @param   kRegPrev               BSplineRegistration2Df Reference to a previous registration which can be used to
     *                                 initialize this registration. This may be a null reference meaning that the this
     *                                 registration uses the default "identity" initialization. If this it not a null
     *                                 reference, then this registration is initialized to provide the a close
     *                                 approximation to the same BSpline mapping as determined by the input
     *                                 registration. The main property of a registration is the position of its control
     *                                 points.
     * @param   kProgressPrefixString  String Text to add to the beginning of the progress bar message.
     *
     * @return  BSplineRegistration2Df Registration instance resulting from this pass. This may be needed to provide for
     *          the initialization of subsequent passes or to compute the deformation image.
     */
    protected BSplineRegistration2Df runPass(ModelSimpleImage kSource, ModelSimpleImage kTarget, Options kOptions,
                                             BSplineRegistration2Df kRegPrev, String kProgressPrefixString) {

        // For display error to a fixed resolution.
        DecimalFormat kDecimalFormat = new DecimalFormat();
        kDecimalFormat.setMinimumFractionDigits(2);
        kDecimalFormat.setMaximumFractionDigits(6);

        // Subsample?  If so, the speedup is achieved by subsampling the target.
        ModelSimpleImage kTargetUse = kTarget;

        if (kOptions.bSubsample) {
            kTargetUse = kTarget.subSample2dBy2();
        }

        BSplineRegistration2Df kReg = null;

        try {

            // Setup to use the progress bar.
            initProgressBar();

            // The control points along the edge do not move.
            int iNumIterationControlPoints = (kOptions.iBSplineNumControlPoints - 2) *
                                                 (kOptions.iBSplineNumControlPoints - 2);

            // Create class which performs the registration.
            BSplineBasisf kBasisX = new BSplineBasisf(kOptions.iBSplineNumControlPoints, kOptions.iBSplineDegree);
            BSplineBasisf kBasisY = new BSplineBasisf(kOptions.iBSplineNumControlPoints, kOptions.iBSplineDegree);

            if (null != kRegPrev) {
                kReg = kRegPrev.createSameMapping(kTargetUse, kBasisX, kBasisY);
            } else {
                kReg = new BSplineRegistration2Df(kSource, kTargetUse, kBasisX, kBasisY, m_kRegMeasure);
            }

            double dErrorPrev = kReg.getError();

            if (dErrorPrev > 0.0) {

                for (int iIteration = 0; iIteration < kOptions.iMaxIterations; iIteration++) {
                    int iIterationControlPoint = 0;

                    // Skip the control points on the boundary.
                    for (int iControlX = 1; iControlX < (kOptions.iBSplineNumControlPoints - 1); iControlX++) {

                        for (int iControlY = 1; iControlY < (kOptions.iBSplineNumControlPoints - 1); iControlY++) {

                            // Did use request early termination?
                            if (isThreadStopped()) {
                                return null;
                            }

                            // Update the progress bar.
                            double dConvergence = (dErrorPrev - kReg.getError()) / dErrorPrev;
                            progressBar.setMessage(kProgressPrefixString + "Iteration: " +
                                                   Integer.toString(iIteration + 1) + "/" + kOptions.iMaxIterations +
                                                   "  Convergence: " + kDecimalFormat.format(dConvergence));
                            progressBar.updateValue((++iIterationControlPoint) * 100 / iNumIterationControlPoints,
                                                    activeImage);

                            // Minimize single control point.
                            kReg.minimizeControlPoint(iControlX, iControlY, kOptions.iGradientDescentMinimizeMaxSteps,
                                                      kOptions.fGradientDescentMinimizeStepSize);
                        }
                    }

                    // Check how much error has changed after each control
                    // point is moved once per iteration.
                    double dError = kReg.getError();

                    if (((dErrorPrev - dError) / dErrorPrev) <= kOptions.fConvergenceLimit) {
                        break;
                    }

                    dErrorPrev = dError;
                }
            }
        } catch (RuntimeException e) {
            errorCleanUp("AlgorithmRegBSpline: " + e.getMessage(), true);
        }


        // If we applied a subsampling before performing the registration,
        // then create a new registration based on the subsample registration
        // results that relates to the orginal sampling of the target.
        if (kOptions.bSubsample) {
            kReg = kReg.createSameMapping(kTarget);
        }

        return kReg;
    }

    /**
     * Save the registration information to a file. This information includes the BSpline parameters; in particular, the
     * coordinates for the lattice of control points.
     *
     * @param  kReg  BSplineRegistration2Df Contains the parameters which define the BSpline for registering the source
     *               image to the targe image.
     */
    private void writeResults(BSplineRegistration2Df kReg) {

        try {
            BSplineBasisf kBasisX = kReg.getLattice().getBasisX();
            BSplineBasisf kBasisY = kReg.getLattice().getBasisY();

            ViewUserInterface UI = m_kImageSource.getUserInterface();
            String fileName = m_kImageSource.getImageName() + ".nlt";
            File file = new File(UI.getDefaultDirectory() + fileName);
            RandomAccessFile raFile = new RandomAccessFile(file, "rw");

            // Necessary so that if this is an overwritten file there isn't any
            // junk at the end
            raFile.setLength(0);

            String lineString;
            byte[] line;

            lineString = new String("# B-spline registration of " + m_kImageSource.getImageName() + " to " +
                                    m_kImageTarget.getImageName() + "\n");
            line = lineString.getBytes();
            raFile.write(line);

            lineString = new String("# Number of image dimensions\n");
            line = lineString.getBytes();
            raFile.write(line);

            lineString = new String(String.valueOf(m_kImageSource.getNDims()) + "\n");
            line = lineString.getBytes();
            raFile.write(line);

            lineString = new String("# Target image resolutions\n");
            line = lineString.getBytes();
            raFile.write(line);

            lineString = new String(String.valueOf(m_kImageTarget.getFileInfo(0).getResolutions()[0]) + " " +
                                    String.valueOf(m_kImageTarget.getFileInfo(0).getResolutions()[1]));
            lineString = lineString + "\n";
            line = lineString.getBytes();
            raFile.write(line);

            lineString = new String("# Target image dimensions\n");
            line = lineString.getBytes();
            raFile.write(line);

            lineString = new String(String.valueOf(m_kImageTarget.getExtents()[0]) + " " +
                                    String.valueOf(m_kImageTarget.getExtents()[1]));
            lineString = lineString + "\n";
            line = lineString.getBytes();
            raFile.write(line);

            lineString = new String("# B-Spline Degree (same for all axes)\n");
            line = lineString.getBytes();
            raFile.write(line);

            lineString = new String(String.valueOf(kBasisX.getDegree()) + " " + String.valueOf(kBasisY.getDegree()) +
                                    "\n");
            line = lineString.getBytes();
            raFile.write(line);

            lineString = new String("# B-Spline Control Points (same for all axes)\n");
            line = lineString.getBytes();
            raFile.write(line);

            lineString = new String(String.valueOf(kBasisX.getNumControlPoints()) + " " +
                                    String.valueOf(kBasisY.getNumControlPoints()) + "\n");
            line = lineString.getBytes();
            raFile.write(line);

            lineString = new String("# Final values of control points\n");
            line = lineString.getBytes();
            raFile.write(line);

            BSplineLattice2Df kBSpline2D = kReg.getLattice();
            Point2f control2D = new Point2f();

            for (int iControlX = 0; iControlX < kBasisX.getNumControlPoints(); iControlX++) {

                for (int iControlY = 0; iControlY < kBasisY.getNumControlPoints(); iControlY++) {
                    lineString = new String("# iControlX = " + String.valueOf(iControlX) + " iControlY = " +
                                            String.valueOf(iControlY) + "\n");
                    line = lineString.getBytes();
                    raFile.write(line);
                    kBSpline2D.getControlPoint(iControlX, iControlY, control2D);
                    lineString = new String(Float.toString(control2D.x) + "  " + Float.toString(control2D.y) + "\n");
                    line = lineString.getBytes();
                    raFile.write(line);
                }
            }

            kBSpline2D = null;
            control2D = null;
            raFile.close();
        } catch (IOException error) {
            MipavUtil.displayError("Error in writeResults is " + error.getMessage());
        }

    }
}
