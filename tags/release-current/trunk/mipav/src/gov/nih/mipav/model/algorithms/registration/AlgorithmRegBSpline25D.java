package gov.nih.mipav.model.algorithms.registration;


import WildMagic.LibFoundation.Curves.*;
import WildMagic.LibFoundation.Mathematics.*;
import gov.nih.mipav.model.algorithms.AlgorithmVOIExtraction;
import gov.nih.mipav.model.structures.*;

import gov.nih.mipav.view.*;

import java.io.*;

import java.text.*;
import java.util.BitSet;


/**
 * BSpline based registration of 2.5D images.
 */
public class AlgorithmRegBSpline25D extends AlgorithmRegBSpline {

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /**
     * Index of the slice in the input source image to use as the target. If the value is -1, then each slice is
     * registered to its adjacent one.
     */
    protected final int m_iSliceTarget;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Constructor.
     *
     * @param  kImageResult       ModelImage Reference to an image to store the registered source image. This image must
     *                            have the same dimensions as the input source image.
     * @param  kImageSource       ModelImage Reference to the original input image to use as the registration source.
     * @param  iSliceTarget       int Index of the slice in the input source image that is to be used as the reference.
     *                            If this index is -1, then each slice is registered to the one before it
     * @param  kImageDeformation  ModelImage Reference to an image to store the computed deformation. This may be a null
     *                            reference to indicate that the deformation is not to be computed. If this reference is
     *                            not null, then the image must have the same dimensions as the input source image.
     * @param  kRegMeasure        RegistrationMeasure Reference to the particular cost measure to use for this
     *                            registration. The cost measure is the same for all passes.
     * @param  kOptionsPass1      Options Options to use for a first pass of registration. This may *not* be a null
     *                            reference.
     * @param  kOptionsPass2      Options Options to use for a second pass of registration. This may be null to indicate
     *                            that only a single pass of registration is to be performed.
     */
    public AlgorithmRegBSpline25D(ModelImage kImageResult, ModelImage kImageSource, int iSliceTarget,
                                  ModelImage kImageDeformation, RegistrationMeasure kRegMeasure, Options kOptionsPass1,
                                  Options kOptionsPass2) {

        super(kImageResult, kImageSource, kImageSource, kImageDeformation, kRegMeasure, kOptionsPass1, kOptionsPass2);
        m_iSliceTarget = iSliceTarget;
    }

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * Prepares this class for destruction.
     */
    public void disposeLocal() { }

    /**
     * Override of Object method called by garbage collection.
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

        // Setup
        fireProgressStateChanged(m_kImageSource.getImageName(), "Registering ...");

        boolean bMultiPass = (null != m_kOptionsPass2);

        // Compute the dimensions/resolutions for each slice.
        int[] aiSliceExtents = new int[2];
        aiSliceExtents[0] = m_kImageSource.getExtents()[0];
        aiSliceExtents[1] = m_kImageSource.getExtents()[1];

        float[] afSliceResolutions = new float[2];
        afSliceResolutions[0] = m_kImageSource.getFileInfo(0).getResolutions()[0];
        afSliceResolutions[1] = m_kImageSource.getFileInfo(0).getResolutions()[1];

        // Save registration results for each slice.
        int iNumSlices = m_kImageSource.getExtents()[2];
        BSplineRegistration2Df[] akReg = new BSplineRegistration2Df[iNumSlices];

        int iStartImageResult = 0;
        int iStartImageDeformation = 0;

iteration:
        for (int iSlice = 0; iSlice < iNumSlices; iSlice++) {

            // Show the slice number in the progress.
            String kStringSlice = new String("Slice: " + String.valueOf(iSlice) + "/" + String.valueOf(iNumSlices-1) +
                                             "  ");

            // Update the progress bar.
            fireProgressStateChanged((iSlice + 1) * 100 / iNumSlices);

            // This is either the specified reference slice or the previous
            // slice (previous slice is set to last slice when the
            // current slice is the first slice).
            ModelSimpleImage kSimpleImageTarget = new ModelSimpleImage(aiSliceExtents, afSliceResolutions,
                                                                       m_kImageSource,
                                                                       (-1 == m_iSliceTarget)
                                                                       ? ((iNumSlices + iSlice - 1) % iNumSlices)
                                                                       : m_iSliceTarget);

            // Extract source slice.  This is always the current slice.
            ModelSimpleImage kSimpleImageSource = new ModelSimpleImage(aiSliceExtents, afSliceResolutions,
                                                                       m_kImageSource, iSlice);

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

            // Pass 1
            
            fireProgressStateChanged(kStringSlice + (bMultiPass ? "Pass 1" : ""));
            
            akReg[iSlice] = runPass(kSimpleImageSource, kSimpleImageTarget, m_kOptionsPass1, null);

            // Optional Pass 2
            if ((null != m_kOptionsPass2) && !isThreadStopped()) {
                fireProgressStateChanged(kStringSlice + "Pass 2");
                akReg[iSlice] = runPass(kSimpleImageSource, kSimpleImageTarget, m_kOptionsPass2, akReg[iSlice]);
            }

            // Did use request early termination?
            if (isThreadStopped()) {
                break iteration;
            }

            // Make sure registration is to original target without subsampling.
            akReg[iSlice] = akReg[iSlice].createSameMapping(kSimpleImageTarget);

            try {

                // Get source image map from the registration and use that
                // to resample the original source image to create the
                // registered source image.
                ModelSimpleImage[] akSimpleImageSourceMap = akReg[iSlice].createImageSourceMap();
                ModelSimpleImage kSimpleImageResult = kSimpleImageSourceOrig.createMappedImage2d(akSimpleImageSourceMap[0],
                                                                                                 akSimpleImageSourceMap[1]);
                m_kImageResult.importData(iStartImageResult, kSimpleImageResult.data, true); // compute min/max
                iStartImageResult += kSimpleImageResult.data.length;
                kSimpleImageResult = null;
                
                if (m_kImageSource.getVOIs().size() != 0) {
                	int sourceXDim = m_kImageSource.getExtents()[0];
                	int sourceYDim = m_kImageSource.getExtents()[1];
                	int srcLength = sourceXDim * sourceYDim;
                	int targetXDim = m_kImageTarget.getExtents()[0];
                	int targetYDim = m_kImageTarget.getExtents()[1];
                	int targetLength = targetXDim * targetYDim;
                	float kImageMapX[] = new float[targetLength];
                	akSimpleImageSourceMap[0].exportData(kImageMapX, 0, targetLength);
                	float kImageMapY[] = new float[targetLength];
                	akSimpleImageSourceMap[1].exportData(kImageMapY, 0, targetLength);
                	float imgBuffer[] = new float[srcLength];
                	transform2DVOI(m_kImageSource, m_kImageResult, imgBuffer, kImageMapX, kImageMapY, iSlice, iNumSlices);
                }
                akSimpleImageSourceMap = null;

                // Access the deformation image if specified.
                if (null != m_kImageDeformation) {
                    ModelSimpleImage kImageDataDeformation = akReg[iSlice].createImageDeformation();
                    m_kImageDeformation.importData(iStartImageDeformation, kImageDataDeformation.data, true); // compute min/max
                    iStartImageDeformation += kImageDataDeformation.data.length;
                    kImageDataDeformation = null;
                }
            } catch (IOException e) {
                throw new RuntimeException("IOException on m_kImageResult.importData: " + e.getMessage());
            }

            kSimpleImageSource = null;
            kSimpleImageTarget = null;
        }
        
        // If we get here, we successfully completed registration given
        // the input specifications, but only if we were not terminated.
        setCompleted(true);

        if (!isThreadStopped()) {
            writeResults(akReg);
        }

        akReg = null;
        disposeLocal();
        
    }
    
    private void transform2DVOI(ModelImage image, ModelImage destImage, final float[] imgBuffer, float[] kImageMapX, float[] kImageMapY,
    		                    int iSlice, int iNumSlices) {

        int i, j;
        int X0pos, Y0pos;
        float value;
        int roundX, roundY;
        int index;
        int index2;
        int indexC;
        int iXdim = image.getExtents()[0];
        int iYdim = image.getExtents()[1];
        int length = iXdim * iYdim;
        int index2Size;
        ModelImage maskImage;
        float fillValue = 0.0f;

        ModelImage tmpMask;
        VOIVector voiVector;

        
        
        voiVector = image.getVOIs();
        
        if (voiVector.size() == 0) {
            return;
        }

        indexC = -1;
        int iNumSamplesTrgX = destImage.getExtents()[0];
        int iNumSamplesTrgY = destImage.getExtents()[1];
        int iLimitSrcX = iXdim - 1;
        int iLimitSrcY = iYdim - 1;
        int tmpExtents[] = new int[3];
        tmpExtents[0] = destImage.getExtents()[0];
        tmpExtents[1] = destImage.getExtents()[1];
        tmpExtents[2] = iNumSlices;
        try {
            maskImage = new ModelImage(ModelStorageBase.SHORT, image.getExtents(), "Short Image");
            tmpMask = new ModelImage(ModelStorageBase.SHORT, tmpExtents, null);
        } catch (final OutOfMemoryError error) {
            throw error;
        }
        for (index = 0; index < voiVector.size(); index++) {
        	VOI presentVOI = voiVector.elementAt(index);
        	if (presentVOI.getCurveType() == VOI.CONTOUR) {
        		VOIBaseVector curves = presentVOI.getCurves();	
        		index2Size = curves.size();
        	}
        	else {
        		index2Size = 1;
        	}
        	for (index2 = 0; index2 < index2Size; index2++) {
        		indexC++;
		        
		        maskImage.clearMask();
		        
		        (voiVector.elementAt(index)).createOneElementBinaryMask3D(maskImage.getMask(), iXdim, iYdim, false, false, index2);

				BitSet mask = maskImage.getMask();

				
				for (i = 0; i < length; i++) {

					if (mask.get(i)) {
						maskImage.set(i, indexC + 1);
					}
					else {
						maskImage.set(i, 0);
					}
				}
		
		        try {
		            maskImage.exportData(0, length, imgBuffer); // locks and releases lock
		        } catch (final IOException error) {
		            displayError("Algorithm VOI transform: Image(s) locked");
		            setCompleted(false);
		
		            return;
		        }
		        
		        

		        for (int iY = 0; iY < iNumSamplesTrgY; iY++) {

		            for (int iX = 0; iX < iNumSamplesTrgX; iX++) {

		                int iIndexTrg = iX + (iY * iNumSamplesTrgX);

		                float fX = iLimitSrcX * kImageMapX[iIndexTrg];
		                float fY = iLimitSrcY * kImageMapY[iIndexTrg];
		                value = fillValue;
		                roundX = (int)(fX + 0.5f);
		                if ( (fX >= -0.5f) && (fX < iXdim)) {
		                	roundY = (int) (fY + 0.5f);
		            		
		                    if ( (fY >= -0.5f) && (fY < iYdim)) {
		                        X0pos = Math.min(roundX, iXdim - 1);
		                        Y0pos = Math.min(roundY, iYdim - 1) * iXdim;
		                        value = imgBuffer[Y0pos + X0pos];
		                    } // end if Y in bounds	
		                }
		                tmpMask.set(iX, iY, iSlice, value); 
		            }
		        }
		
		        
		
		        if (threadStopped) {
		            return;
		        }
		
		        // ******* Make algorithm for VOI extraction.
		        tmpMask.calcMinMax();
		
		        final AlgorithmVOIExtraction VOIExtAlgo = new AlgorithmVOIExtraction(tmpMask);
		
		        VOIExtAlgo.setRunningInSeparateThread(runningInSeparateThread);
		        VOIExtAlgo.run();
		        destImage.addVOIs(tmpMask.getVOIs());
		        tmpMask.resetVOIs();
		        for (j = 0; j < iNumSamplesTrgY; j++) {
	        		for (i = 0; i < iNumSamplesTrgX; i++) {
	        			tmpMask.set(i, j, iSlice, fillValue);
	        		}
	        	}
        	} // for (index2 = 0; index2 < curves.size(); index2++)
        } // for (index = 0; index < voiVector.size(); index++)
        tmpMask.disposeLocal();
        tmpMask = null;
        maskImage.disposeLocal();
        maskImage = null;
    }

    /**
     * Perform a "pass" of BSpline based registration. A "pass" is defined to be one or more iterations using the
     * BSpline basis parameters, gradient descent parameters, and convergence parameters.
     *
     * @param   kSource   ModelSimpleImage Reference to the original input image to use as the registration source. This
     *                    image does not have to have the same dimensions as the image to be used as the registration
     *                    target.
     * @param   kTarget   ModelSimpleImage Reference to the original input image to use as the registration target.
     * @param   kOptions  Options Options to use for a this registration pass. This may *not* be a null reference.
     * @param   kRegPrev  BSplineRegistration2Df Reference to a previous registration which can be used to initialize
     *                    this registration. This may be a null reference meaning that the this registration uses the
     *                    default "identity" initialization. If this it not a null reference, then this registration is
     *                    initialized to provide the a close approximation to the same BSpline mapping as determined by
     *                    the input registration. The main property of a registration is the position of its control
     *                    points.
     *
     * @return  BSplineRegistration2Df Registration instance resulting from this pass. This may be needed to provide for
     *          the initialization of subsequent passes or to compute the deformation image.
     */
    protected BSplineRegistration2Df runPass(ModelSimpleImage kSource, ModelSimpleImage kTarget, Options kOptions,
                                             BSplineRegistration2Df kRegPrev) {

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

                    // Skip the control points on the boundary.
                    for (int iControlX = 1; iControlX < (kOptions.iBSplineNumControlPoints - 1); iControlX++) {

                        for (int iControlY = 1; iControlY < (kOptions.iBSplineNumControlPoints - 1); iControlY++) {

                            // Did use request early termination?
                            if (isThreadStopped()) {
                                return null;
                            }

                            // Minimize single control point.
                            kReg.minimizeControlPoint(iControlX, iControlY, kOptions.iGradientDescentMinimizeMaxSteps,
                                                      kOptions.fGradientDescentMinimizeStepSize);
                        }
                    }

                    // Check how much error has changed after each control
                    // point is moved once per iteration.
                    double dError = kReg.getError();

                    if ((Math.abs(dErrorPrev - dError) / dErrorPrev) <= kOptions.fConvergenceLimit) {
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
     * @param  akReg  BSplineRegistration2Df[] Contains the parameters which define the BSpline for registering each
     *                slice of the source image to the particular reference slice.
     */
    private void writeResults(BSplineRegistration2Df[] akReg) {

        if ((null == akReg) || (0 == akReg.length)) {
            return;
        }

        try {

            // B-spline basis is same for all slices
            BSplineBasisf kBasisX = akReg[0].getLattice().getBasisX();
            BSplineBasisf kBasisY = akReg[0].getLattice().getBasisY();

            String fileName = m_kImageSource.getImageName() + ".nlt";
            File file = new File(ViewUserInterface.getReference().getDefaultDirectory() + fileName);
            RandomAccessFile raFile = new RandomAccessFile(file, "rw");

            // Necessary so that if this is an overwritten file there isn't any
            // junk at the end
            raFile.setLength(0);

            String lineString;
            byte[] line;

            lineString = new String("# B-spline registration of " + m_kImageSource.getImageName() + " slices to " +
                                    ((-1 == m_iSliceTarget) ? "previous slices"
                                                            : ("slice " + String.valueOf(m_iSliceTarget))) + "\n");
            line = lineString.getBytes();
            raFile.write(line);

            lineString = new String("# Number of image dimensions\n2.5\n");
            line = lineString.getBytes();
            raFile.write(line);

            lineString = new String("# Target image resolutions\n");
            line = lineString.getBytes();
            raFile.write(line);

            lineString = new String(String.valueOf(m_kImageSource.getFileInfo(0).getResolutions()[0]) + " " +
                                    String.valueOf(m_kImageSource.getFileInfo(0).getResolutions()[1]) + " " +
                                    String.valueOf(m_kImageSource.getFileInfo(0).getResolutions()[2]));
            lineString = lineString + "\n";
            line = lineString.getBytes();
            raFile.write(line);

            lineString = new String("# B-Spline Degree (same for all axes)\n");
            line = lineString.getBytes();
            raFile.write(line);

            lineString = new String(String.valueOf(kBasisX.GetDegree()) + " " + String.valueOf(kBasisY.GetDegree()) +
                                    "\n");
            line = lineString.getBytes();
            raFile.write(line);

            lineString = new String("# B-Spline Control Points (same for all axes)\n");
            line = lineString.getBytes();
            raFile.write(line);

            lineString = new String(String.valueOf(kBasisX.GetNumCtrlPoints()) + " " +
                                    String.valueOf(kBasisY.GetNumCtrlPoints()) + "\n");
            line = lineString.getBytes();
            raFile.write(line);

            Vector2f control2D = new Vector2f();

            for (int iSlice = 0; iSlice < m_kImageSource.getExtents()[2]; iSlice++) {
                lineString = new String("# Final values of control points for slice " + String.valueOf(iSlice) + "\n");
                line = lineString.getBytes();
                raFile.write(line);

                BSplineLattice2Df kBSpline2D = akReg[iSlice].getLattice();

                for (int iControlX = 0; iControlX < kBasisX.GetNumCtrlPoints(); iControlX++) {

                    for (int iControlY = 0; iControlY < kBasisY.GetNumCtrlPoints(); iControlY++) {
                        lineString = new String("# iControlX = " + String.valueOf(iControlX) + " iControlY = " +
                                                String.valueOf(iControlY) + "\n");
                        line = lineString.getBytes();
                        raFile.write(line);
                        kBSpline2D.getControlPoint(iControlX, iControlY, control2D);
                        lineString = new String(Float.toString(control2D.X) + "  " + Float.toString(control2D.Y) +
                                                "\n");
                        line = lineString.getBytes();
                        raFile.write(line);
                    }
                }

                kBSpline2D = null;
            }

            control2D = null;
            raFile.close();
        } catch (IOException error) {
            MipavUtil.displayError("Error in writeResults is " + error.getMessage());
        }

    }
}
