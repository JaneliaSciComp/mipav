package gov.nih.mipav.model.algorithms.filters;

import gov.nih.mipav.model.algorithms.AlgorithmBase;
import gov.nih.mipav.model.file.FileIO;
import gov.nih.mipav.model.file.FileUtility;
import gov.nih.mipav.model.file.FileWriteOptions;
import gov.nih.mipav.model.structures.BSplineKernelFunction;
import gov.nih.mipav.model.structures.CoxDeBoorBSplineKernelFunction;
import gov.nih.mipav.model.structures.ModelImage;
import gov.nih.mipav.model.structures.ModelStorageBase;

import java.io.File;
import java.io.IOException;
import java.util.Vector;

import WildMagic.LibFoundation.Mathematics.Vector3d;
import WildMagic.LibFoundation.Mathematics.Vector4d;
import gov.nih.mipav.view.MipavUtil;
import gov.nih.mipav.view.Preferences;
import gov.nih.mipav.view.ViewJProgressBar;

/**
 * 
 * 
 * @version 0.1 April 24, 2014
 * @author William Gandler
 * 
 *         This a a port of itkN4MRIBiasFieldCorrectionImageFilter.txx and 
 *         itkCoxDeBoorBSplineKernelFunctionTest2.cxx from the
 *         itk package. Here is the original itk header: Program: Advanced
 *         Normalization Tools Module: $RCSfile:
 *         itkN4MRIBiasFieldCorrectionImageFilter.txx,v $ Language: C++ Date:
 *         $Date: 2009/06/09 16:22:05 $ Version: $Revision: 1.6 $
 * 
 *         Copyright (c) ConsortiumOfANTS. All rights reserved. See accompanying
 *         COPYING.txt or
 *         http://sourceforge.net/projects/advants/files/ANTS/ANTSCopyright.txt
 *         for details.
 * 
 *         This software is distributed WITHOUT ANY WARRANTY; without even the
 *         implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
 *         PURPOSE. See the above copyright notices for more information.
 */
public class AlgorithmN4MRIBiasFieldCorrectionFilter extends AlgorithmBase {

	// ~ Instance fields
	// ------------------------------------------------------------------------------------------------
	
	private ModelImage fieldImage = null;

	/**
	 * Flag, if true, indicates that the whole image should be processed. If
	 * false either: 1.) Process where the mask is set true 2.) Make sure the
	 * mask is set false and process where the confidence image is > 0.0
	 */
	private boolean entireImage;

	private ModelImage confidenceImage = null;

	private double confidence[] = null;

	private int nDims;

	private int numberOfHistogramBins = 200;

	private ModelImage logBiasFieldControlPointLattice = null;

	private double WienerFilterNoise = 0.01;

	private double biasFieldFullWidthAtHalfMaximum = 0.15;

	private int splineOrder[];
	
	private int fittingLevels = 1;

	private int originalNumberOfFittingLevels[];
	
	private int controlPoints = 4;

	private int originalNumberOfControlPoints[];

	private double sigmoidNormalizedAlpha = 0.0;

	private double sigmoidNormalizedBeta = 0.5;
	
	private int maximumIterations = 50;

	private int maximumNumberOfIterations[];

	private double convergenceThreshold = 0.001;

	private int length;

	private float origin[];

	private float resolutions[];

	private int xDim;

	private int sliceSize;
	
	private int xyzSize;

	private float maskOrigin[];

	private int maskIndex[];

	private int maskLargest[];

	private int maskExtents[];
	
	private boolean selfTest = false;
	
	private boolean CoxDeBoorBSplineKernelFunctionTest = false;
	
	private boolean CoxDeBoorBSplineKernelFunctionTest2 = false;
	
	private boolean BSplineScatteredDataPointSetToImageFilterTest = false;
	
	private boolean BSplineScatteredDataPointSetToImageFilterTest2 = false;
	
	private boolean BSplineControlPointImageFilterTest = false;
	
	private boolean BSplineKernelFunctionTest = false;

	// ~ Constructors
	// ---------------------------------------------------------------------------------------------------

	/**
	 * Constructor which sets the source and destination images
	 * 
	 * @param destImg
	 *            the destination image
	 * @param fieldImage
	 * @param srcImg
	 *            the source image
	 * @maximumIterations
	 * @convergenceThreshold
	 * @biasFieldFullWidthAtHalfMaximum
	 * @WienerFilterNoise
	 * @fittingLevels
	 * @controlPoints
	 * @param confidenceImage
	 * @param maskFlag
	 *            the mask flag
	 */
	public AlgorithmN4MRIBiasFieldCorrectionFilter(ModelImage destImg, ModelImage fieldImage,
			ModelImage srcImg, int maximumIterations, double convergenceThreshold, double biasFieldFullWidthAtHalfMaximum,
			double WienerFilterNoise, int fittingLevels, int controlPoints, ModelImage confidenceImage, boolean maskFlag) {
		super(destImg, srcImg);
		this.fieldImage = fieldImage;
		this.maximumIterations = maximumIterations;
		this.convergenceThreshold = convergenceThreshold;
		this.biasFieldFullWidthAtHalfMaximum = biasFieldFullWidthAtHalfMaximum;
		this.WienerFilterNoise = WienerFilterNoise;
		this.fittingLevels = fittingLevels;
		this.controlPoints = controlPoints;
		this.confidenceImage = confidenceImage;
		entireImage = maskFlag;
	}

	// ~ Methods
	// --------------------------------------------------------------------------------------------------------

	/**
	 * Prepares this class for destruction.
	 */
	public void finalize() {
		destImage = null;
		srcImage = null;
		confidenceImage = null;
		confidence = null;
		splineOrder = null;
		originalNumberOfFittingLevels = null;
		originalNumberOfControlPoints = null;
		maximumNumberOfIterations = null;
		origin = null;
		resolutions = null;
		maskOrigin = null;
		maskIndex = null;
		maskLargest = null;
		maskExtents = null;
		super.finalize();
	}

	/**
	 * Starts the program.
	 */
	public void runAlgorithm() {
		int i;
		int j;
		int x;
		int y;
		int z;
		int t;
		int d;
		double buffer[];
		double logFilter[];
		double logUncorrected[];
		double logBiasField[];
		double logSharpened[];
		double subtracter1[];
		double newLogBiasField[];
		int currentLevel;
		int elapsedIterations = 0;
		int smallestX;
		int smallestY;
		int smallestZ;
		int smallestT;
		int largestX;
		int largestY;
		int largestZ;
		int largestT;
		double currentConvergenceMeasurement = Double.MAX_VALUE;
		double dirLength;
		double direction[][];

		if (srcImage == null) {
			displayError("Source Image is null");
			finalize();

			return;
		}
		
		if (CoxDeBoorBSplineKernelFunctionTest) {
			CoxDeBoorBSplineKernelFunction CoxDeBoor = new CoxDeBoorBSplineKernelFunction();
			CoxDeBoor.selfTest();
			setCompleted(false);
			return;
		}
		
		if (CoxDeBoorBSplineKernelFunctionTest2) {
			// All 317 CoxDeBoorBSplineKernelFunction tests passed
			// In this test we check to see that the derivative is
			// calculated correctly for spline orders 2 through 10.
			int testsPassed = 0;
			int testsFailed = 0;
			int testsTotal;
			for (int order = 2; order <= 10; order++) {
				CoxDeBoorBSplineKernelFunction kernel = new CoxDeBoorBSplineKernelFunction(order);
				CoxDeBoorBSplineKernelFunction kernelOrderMinus1 = new CoxDeBoorBSplineKernelFunction(order-1);
				for (double tt = 0.0; tt < (0.5*(order+1)); tt += 0.1) {
				    double derivative = kernel.evaluateDerivative(tt);
				    if (Math.abs(derivative - (kernelOrderMinus1.evaluate(tt+0.5)
				        - kernelOrderMinus1.evaluate(tt - 0.5))) > 1.0E-10) {
				        Preferences.debug("CoxDeBoorBSplineKernelFunction failed for order = " + order + 
				        		" tt = " + tt + "\n", Preferences.DEBUG_ALGORITHM);	
				        testsFailed++;
				    }
				    else {
				    	testsPassed++;
				    }
				}
				kernel.finalize();
				kernelOrderMinus1.finalize();
			}
			testsTotal = testsPassed + testsFailed;
			if (testsFailed == 0) {
				Preferences.debug("All " + testsPassed + " CoxDeBoorBSplineKernelFunction tests passed\n",
						Preferences.DEBUG_ALGORITHM);
			}
			else {
				Preferences.debug(testsPassed + " out of " + testsTotal + " CoxDeBoorBSplineKernelFunction tests passed\n",
						Preferences.DEBUG_ALGORITHM);
			}
			setCompleted(false);
			return;
		}
		
		if (BSplineControlPointImageFilterTest) {
			// BSplineOutput.xml and BSplineOutput2.xml as expected
			// This indicates the filter is working properly.
			FileIO fileIO;
	        boolean multiFile = false;
	        ModelImage testImage;
	        String directory = new String("C:" + File.separatorChar + "N4" + File.separatorChar);
	        String fileName = new String("N4ControlPoints_2D.nii.gz");
	        //String fileName = new String("N4ControlPoints14by14_2D.xml");
	        try {
	            fileIO = new FileIO();

	            testImage =  fileIO.readImage(fileName, directory, multiFile, null);
	        } catch (OutOfMemoryError e) {
	            MipavUtil.displayError("Out of memory!");

	            return;
	        }
	        
	        nDims = 2;
	        origin = new float[nDims];
	        int extents[] = new int[nDims];
	        extents[0] = 100;
	        extents[1] = 100;
	        resolutions = new float[nDims];
	        resolutions[0] = 1.0f;
	        resolutions[1] = 1.0f;
	        direction = new double[nDims][nDims];
	        direction[0][0] = 1.0;
	        direction[1][1] = 1.0;
	        //int numberOfLevels[] = new int[nDims];
	        //numberOfLevels[0] = 3;
	        //numberOfLevels[1] = 3;
	        
	        AlgorithmBSplineControlPointImageFilter bspliner = new AlgorithmBSplineControlPointImageFilter(nDims);
	        bspliner.setInput(testImage);
	        bspliner.setExtents(extents);
	        bspliner.setOrigin(origin);
	        bspliner.setResolutions(resolutions);
	        bspliner.setDirection(direction);
	        //bspliner.setNumberOfLevels(numberOfLevels);
	        
	        bspliner.generateData();
	        double outputBuffer[] = bspliner.getOutputBuffer();
	        ModelImage outputImage = new ModelImage(ModelStorageBase.DOUBLE, extents, "outputImage");
	        try {
	        	outputImage.importData(0, outputBuffer, true);
	        }
	        catch (IOException e) {
	        	MipavUtil.displayError("IOException " + e + " on outputImage.importData(0, outputBuffer, true)");
	        	setCompleted(false);
	        	return;	
	        }
	        FileWriteOptions opts = new FileWriteOptions(true);
	        opts.setFileType(FileUtility.XML);
	        opts.setFileDirectory(directory);
	        opts.setFileName("BSplineOutput.xml");
	        opts.setOptionsSet(true);
			fileIO.writeImage(outputImage, opts);
			outputImage.disposeLocal();
			outputImage = null;
			
			// Test out additional functionality by refining the control point lattice
			// and seeing if the output is the same.  In this example we double the
			/// resolution twice as the refinement is doubled at every level.
			
			int numberOfRefinementLevels[] = new int[nDims];
			numberOfRefinementLevels[0] = 3;
			numberOfRefinementLevels[1] = 3;
			ModelImage refinedControlPointLattice = bspliner.refineControlPointLattice(numberOfRefinementLevels);
			bspliner.finalize();
			bspliner = null;
			
			AlgorithmBSplineControlPointImageFilter bspliner2 = new AlgorithmBSplineControlPointImageFilter(nDims);
	        bspliner2.setInput(refinedControlPointLattice);
	        bspliner2.setExtents(extents);
	        bspliner2.setOrigin(origin);		
	        bspliner2.setResolutions(resolutions);
	        bspliner2.setDirection(direction);
	        
	        bspliner2.generateData();
	        outputBuffer = bspliner2.getOutputBuffer();
	        bspliner2.finalize();
	        bspliner2 = null;
	        ModelImage outputImage2 = new ModelImage(ModelStorageBase.DOUBLE, extents, "outputImage2");
	        try {
	        	outputImage2.importData(0, outputBuffer, true);
	        }
	        catch (IOException e) {
	        	MipavUtil.displayError("IOException " + e + " on outputImage2.importData(0, outputBuffer, true)");
	        	setCompleted(false);
	        	return;	
	        }
	        opts = new FileWriteOptions(true);
	        opts.setFileType(FileUtility.XML);
	        opts.setFileDirectory(directory);
	        opts.setFileName("BSplineOutput2.xml");
	        opts.setOptionsSet(true);
			fileIO.writeImage(outputImage2, opts);
			outputImage2.disposeLocal();
			outputImage2 = null;
		    setCompleted(false);
		    return;
		}
		
		if (BSplineScatteredDataPointSetToImageFilterTest) {
			// filter.setNumberOfLevels(10) gives result indicating
			// the filter is working properly
			FileIO fileIO;
	        boolean multiFile = false;
	        ModelImage testImage;
	        String directory = new String("C:" + File.separatorChar + "N4" + File.separatorChar + "Source" +
	                                      File.separatorChar + "Input" + File.separatorChar);
	        String fileName = new String("t81slice_threshold.xml");
	        try {
	            fileIO = new FileIO();

	            testImage =  fileIO.readImage(fileName, directory, multiFile, null);
	        } catch (OutOfMemoryError e) {
	            MipavUtil.displayError("Out of memory!");

	            return;
	        }
	        
	        // Iterate through the input image which consists of multivalued
	        // foreground pixels (=nonzero) and background values (=zero).
	        // The foreground pixels comprise the input point set.
	        // Extract both the 2-D locations and the pixel values of the points
	        xDim = testImage.getExtents()[0];
	        int yDim = testImage.getExtents()[1];
	        length = xDim * yDim;
	        buffer = new double[length];
	        try {
	        	testImage.exportData(0, length, buffer);
	        }
	        catch (IOException e) {
	        	MipavUtil.displayError("IOException " + e + " on testImage.exportData(0, length, buffer)");
	        	setCompleted(false);
	        	return;
	        }
	        Vector<Vector4d> pointLocation = new Vector<Vector4d>();
	    	Vector<Double> pointData = new Vector<Double>();
	        resolutions = testImage.getResolutions(0);
	        origin = testImage.getOrigin();
	        int[] extents = testImage.getExtents();
	        Vector4d point;
	        int index = 0;
	        for (y = 0; y < yDim; y++) {
	        	for (x = 0; x < xDim; x++) {
	        	    i = x + y * xDim;
	        	    if (buffer[i] > 0) {
	        	    	point = new Vector4d(x*resolutions[0] + origin[0], y*resolutions[1] + origin[1], 0.0, 0.0);
	        			pointData.add(index, buffer[i]);
	        			pointLocation.add(index, point);
	        			index++;
	        	    }
	        	}
	        }
	        
	        // Instantiate the B-spline filter and set the desired parameters
	        nDims = 2;
	        AlgorithmBSplineScatteredDataPointSetToImageFilter filter;
            filter = new AlgorithmBSplineScatteredDataPointSetToImageFilter(nDims);
            filter.setSplineOrder(3);
            int ncps[] = new int[nDims];
			for (i = 0; i < nDims; i++) {
				ncps[i] = 4;
			}
			filter.setNumberOfControlPoints(ncps);
			//filter.setNumberOfLevels(3);
			filter.setNumberOfLevels(10);
			filter.setOrigin(origin);
			filter.setResolutions(resolutions);
			filter.setExtents(extents);
			filter.setPointData(pointData);
			filter.setPointLocation(pointLocation);
			filter.generateData();
			
			ModelImage outputImage = filter.getOutputImage();
			FileWriteOptions opts = new FileWriteOptions(true);
	        opts.setFileType(FileUtility.XML);
	        opts.setFileDirectory(directory);
	        opts.setFileName("t81slice_threshold_corrected.xml");
	        opts.setOptionsSet(true);
			fileIO.writeImage(outputImage, opts);
		    setCompleted(false);
		    return;
		}
		
		if (BSplineScatteredDataPointSetToImageFilterTest2) {
			// Sample the helix
			// Cannot implement test with my code
			// parametric dimension = 1 and data dimension = 3
			// so would need nDims = 1 but my code can only do nDims = 2 to nDims = 4
			// Also would need a Vector<Vector3d> for pointData but my code can only do 
			// Vector<Double> for pointData
			double tt;
			Vector<Vector4d> pointLocation = new Vector<Vector4d>();
	    	Vector<Vector3d> pointData = new Vector<Vector3d>();
	    	Vector4d v;
	    	Vector3d dd;
	    	nDims = 1;
	    	float res[] = new float[nDims];
			for (i = 0; i < nDims; i++) {
				res[i] = 0.01f;
			}
			int extents[] = new int[nDims];
			for (i = 0; i < nDims; i++) {
				extents[i] = 101;
			}
			float org[] = new float[nDims];
			for (i = 0; i < nDims; i++) {
				org[i] = 0.0f;
			}
	    	AlgorithmBSplineScatteredDataPointSetToImageFilter filter;
			for (tt = 0.0, i = 0; tt <= 1.0 + 1.0E-10; tt += 0.05, i++) {
			    v = new Vector4d(tt, 0.0, 0.0, 0.0);
			    pointLocation.add(i, v);
			    dd = new Vector3d(0.25*Math.cos(tt*6.0*Math.PI), 0.25*Math.sin(tt* 6.0 * Math.PI), 4.00*tt);
			    //pointData.add(i, dd);
			}
			
			// Instantiate the filter and set the parameters
			filter = new AlgorithmBSplineScatteredDataPointSetToImageFilter(nDims);
			
			filter.setExtents(extents);
			filter.setOrigin(org);
			filter.setResolutions(res);
			//filter.setPointData(pointData);
			filter.setPointLocation(pointLocation);
			int ncps[] = new int[nDims];
			for (i = 0; i < nDims; i++) {
				ncps[i] = 4;
			}
			filter.setNumberOfControlPoints(ncps);
			filter.setNumberOfLevels(5);
			filter.setGenerateOutputImage(false);
			filter.generateData();
			setCompleted(false);
			return;
		}
		
		if (BSplineKernelFunctionTest) {
			BSplineKernelFunction BSpline = new BSplineKernelFunction();
			BSpline.selfTest();
			setCompleted(false);
			return;
		}

		fireProgressStateChanged(0, srcImage.getImageName(),
				"N4 MRI Bias Field Correction Filter on image ...");
		
		mask = null;

        if (entireImage == false) {
            mask = srcImage.generateVOIMask();
        }


		maximumNumberOfIterations = new int[fittingLevels];
		for (i = 0; i < maximumNumberOfIterations.length; i++) {
			maximumNumberOfIterations[i] = maximumIterations;
		}

		srcImage.calcMinMax();
		origin = srcImage.getOrigin();
		resolutions = srcImage.getFileInfo()[0].getResolutions();
		nDims = srcImage.getNDims();
		originalNumberOfFittingLevels = new int[nDims];
		for (i = 0; i < nDims; i++) {
			originalNumberOfFittingLevels[i] = fittingLevels;
		}
		originalNumberOfControlPoints = new int[nDims];
		for (i = 0; i < nDims; i++) {
			originalNumberOfControlPoints[i] = controlPoints;
		}
		splineOrder = new int[nDims];
		for (i = 0; i < nDims; i++) {
			splineOrder[i] = 3;
		}
		xDim = srcImage.getExtents()[0];
		length = srcImage.getExtents()[0];
		for (i = 1; i < nDims; i++) {
			length *= srcImage.getExtents()[i];
		}
		sliceSize = srcImage.getExtents()[0] * srcImage.getExtents()[1];
		xyzSize = sliceSize;
		if (nDims > 2) {
			xyzSize *= srcImage.getExtents()[2];
		}
		buffer = new double[length];
		logFilter = new double[length];
		logUncorrected = new double[length];

		try {
			srcImage.exportData(0, length, buffer); // locks and releases lock
		} catch (IOException error) {
			displayError("Algorithm N4 MRI Bias Field Correction Filter: Image(s) locked");
			setCompleted(false);
			fireProgressStateChanged(ViewJProgressBar.PROGRESS_WINDOW_CLOSING);
			srcImage.releaseLock();

			return;
		}
		
		if (selfTest) {
			selfTest();
			setCompleted(true);
			return;
		}

		if (confidenceImage != null) {
			confidence = new double[length];
			try {
				confidenceImage.exportData(0, length, confidence); // locks and
																	// releases
																	// lock
			} catch (IOException error) {
				displayError("Algorithm N4 MRI Bias Field Correction Filter: confidenceImage locked");
				setCompleted(false);
				fireProgressStateChanged(ViewJProgressBar.PROGRESS_WINDOW_CLOSING);
				confidenceImage.releaseLock();

				return;
			}
		}

		smallestX = Integer.MAX_VALUE;
		smallestY = Integer.MAX_VALUE;
		smallestZ = Integer.MAX_VALUE;
		smallestT = Integer.MAX_VALUE;
		largestX = Integer.MIN_VALUE;
		largestY = Integer.MIN_VALUE;
		largestZ = Integer.MIN_VALUE;
		largestT = Integer.MIN_VALUE;
		maskIndex = new int[nDims];
		maskLargest = new int[nDims];
		maskExtents = new int[nDims];
		maskOrigin = new float[nDims];
		for (i = 0; i < length; i++) {
			if (entireImage || mask.get(i) || ((confidence != null) && (confidence[i] > 0.0))) {
				x = i % xDim;
				y = (i % sliceSize) / xDim;
				z = (i % xyzSize)/ sliceSize;
				t = i / xyzSize;
				if (x < smallestX) {
					smallestX = x;
				}
				if (x > largestX) {
					largestX = x;
				}
				if (y < smallestY) {
					smallestY = y;
				}
				if (y > largestY) {
					largestY = y;
				}
				if (z < smallestZ) {
					smallestZ = z;
				}
				if (z > largestZ) {
					largestZ = z;
				}
				if (t < smallestT) {
					smallestT = t;
				}
				if (t > largestT) {
					largestT = t;
				}
			}
		}
		maskIndex[0] = smallestX;
		maskIndex[1] = smallestY;
		if (nDims > 2) {
			maskIndex[2] = smallestZ;
			if (nDims > 3) {
				maskIndex[3] = smallestT;
			}
		}
		maskLargest[0] = largestX;
		maskLargest[1] = largestY;
		if (nDims > 2) {
			maskLargest[2] = largestZ;
			if (nDims > 3) {
				maskLargest[3] = largestT;
			}
		}
		for (i = 0; i < nDims; i++) {
			maskExtents[i] = maskLargest[i] - maskIndex[i] + 1;
		}
		for (i = 0; i < nDims; i++) {
			maskOrigin[i] = origin[i] + resolutions[i] * maskIndex[i];
		}
		
		direction = new double[Math.min(nDims,3)][Math.min(nDims,3)];
		for (i = 0; i < Math.min(nDims,3); i++) {
        	dirLength = 0;
            for (j = 0; j < Math.min(nDims,3); j++) {
                direction[i][j] = srcImage.getMatrix().get(i, j);
                dirLength += (direction[i][j] * direction[i][j]);
            }
            dirLength = Math.sqrt(dirLength);
            for (j = 0; j < Math.min(nDims,3); j++) {
            	direction[i][j] = direction[i][j]/dirLength;
            }
        } // for (i = 0; i < Math.min(nDims,3); i++)

		// Calculate the log of the input image
		// Set NaNs, infinities, and negatives to zero
		for (i = 0; i < length; i++) {
			if (entireImage || (mask.get(i) && (confidence == null))
					|| ((confidence != null) && (confidence[i] > 0.0))) {
				logFilter[i] = Math.log(buffer[i]);
				if ((Double.isNaN(logFilter[i]))
						|| (Double.isInfinite(logFilter[i]))
						|| (logFilter[i] < 0.0)) {
					logFilter[i] = 0.0;
				}
				logUncorrected[i] = logFilter[i];
			}
		} // for (i = 0; i < length; i++)

		// Provide an initial log bias field of zeros.
		logBiasField = new double[length];

		subtracter1 = new double[length];
		// Iterate until convergence or iterative exhaustion
		int maximumNumberOfLevels = 1;
		for (d = 0; d < originalNumberOfFittingLevels.length; d++) {
			if (originalNumberOfFittingLevels[d] > maximumNumberOfLevels) {
				maximumNumberOfLevels = originalNumberOfFittingLevels[d];
			}
		}
		if (maximumNumberOfIterations.length != maximumNumberOfLevels) {
			MipavUtil
					.displayError("Number of iteration levels is not equal to the max number of levels");
			setCompleted(false);
			return;
		}
		for (currentLevel = 0; currentLevel < maximumNumberOfLevels; currentLevel++) {
			elapsedIterations = 0;
			currentConvergenceMeasurement = Double.MAX_VALUE;
			while ((elapsedIterations++ < maximumNumberOfIterations[currentLevel])
					&& (currentConvergenceMeasurement > convergenceThreshold)) {
				// Sharpen the current estimate of the uncorrected image
				logSharpened = sharpen(logUncorrected);
				for (i = 0; i < length; i++) {
					if (entireImage || (mask.get(i) && (confidence == null))
							|| ((confidence != null) && (confidence[i] > 0.0))) {
						subtracter1[i] = logUncorrected[i] - logSharpened[i];
					}
				}

				// Smooth the residual bias field estimate and add the resulting
				// control point grid to get the new total bias field estimate.
				newLogBiasField = updateBiasFieldEstimate(subtracter1);
				currentConvergenceMeasurement = calculateConvergenceMeasurement(
						logBiasField, newLogBiasField);
				Preferences.debug("currentConvergenceMeasurement = " + currentConvergenceMeasurement + "\n",
						Preferences.DEBUG_ALGORITHM);
				logBiasField = newLogBiasField;
				for (i = 0; i < length; i++) {
					if (entireImage || (mask.get(i) && (confidence == null))
							|| ((confidence != null) && (confidence[i] > 0.0))) {
						logUncorrected[i] = logFilter[i] - logBiasField[i];
					}
				}
			} // while ((elapsedIterations++ <
				// maximumNumberOfIterationsArray[currentLevel]) &&
		    AlgorithmBSplineControlPointImageFilter reconstructer = new AlgorithmBSplineControlPointImageFilter(nDims);
		    reconstructer.setInput(logBiasFieldControlPointLattice);
		    reconstructer.setOrigin(origin);
		    reconstructer.setResolutions(resolutions);
		    reconstructer.setDirection(direction);
		    reconstructer.setExtents(srcImage.getExtents());
		    reconstructer.generateData();
		    int numberOfLevels[] = new int[nDims];
		    for (i = 0; i < nDims; i++) {
		    	numberOfLevels[i] = 1;
		    }
		    for (d = 0; d < nDims; d++) {
		    	if ((originalNumberOfFittingLevels[d] + 1 >= currentLevel) && (currentLevel != maximumNumberOfLevels - 1)) {
		    		numberOfLevels[d] = 2;
		    	}
		    } //for (d = 0; d < nDims; d++)
		    logBiasFieldControlPointLattice = reconstructer.refineControlPointLattice(numberOfLevels);
		    reconstructer.finalize();
		    reconstructer = null;
		} // for (currentLevel = 0; currentLevel < maximumNumberOfLevels;
			// currentLevel++)
		
		double expFilter[] = new double[logBiasField.length];
		for (i = 0; i < expFilter.length; i++) {
			if (entireImage || (mask.get(i) && (confidence == null))
					|| ((confidence != null) && (confidence[i] > 0.0))) {
			    expFilter[i] = Math.exp(logBiasField[i]);
			}
		}
		
		// Divide the input image by the bias field to get the final image
		double outputBuffer[] = new double[buffer.length];
		for (i = 0; i < buffer.length; i++) {
			if (entireImage || (mask.get(i) && (confidence == null))
					|| ((confidence != null) && (confidence[i] > 0.0))) {
			outputBuffer[i] = buffer[i]/expFilter[i];
			}
		}
		
		if (fieldImage != null) {
			try {
				fieldImage.importData(0, expFilter, true);
			}
			catch (IOException e) {
				MipavUtil.displayError("IOException on fieldImage.importData(0, expFilter, true)");
				setCompleted(false);
				return;
			}	
		}
		
		if (destImage != null) {
			try {
				destImage.importData(0, outputBuffer, true);
			}
			catch (IOException e) {
				MipavUtil.displayError("IOException on destImage.importData(0, outputBuffer, true)");
				setCompleted(false);
				return;
			}
		}
		else {
			try {
			    srcImage.importData(0, outputBuffer, true);
			}
			catch (IOException e) {
				MipavUtil.displayError("IOException on srcImage.importData(0, outputBuffer, true)");
				setCompleted(false);
				return;
			}
		}

		setCompleted(true);
		return;
	}

	private double[] sharpen(double[] unsharpened) {
		double sharpened[] = new double[unsharpened.length];
		// Build the histogram for the uncorrected image.
		// Note that variables in real space are denoted by a single
		// upper case letter whereas their frequency counterparts are
		// indicated by a trailing lower case 'f'.
		double binMaximum = -Double.MAX_VALUE;
		double binMinimum = Double.MAX_VALUE;
		double pixel;
		int i;
		double histogramSlope;
		double H[] = new double[numberOfHistogramBins];
		double cidx;
		int idx;
		double offset;
		double exponent;
		int paddedHistogramSize;
		int histogramOffset;
		int n;
		FFTUtility fft;
		double VfR[];
		double VfI[];
		double scaledFWHM;
		double expFactor;
		double scaleFactor;
		int halfSize;
		double FfR[];
		double FfI[];
		double GfR[];
		double GfI[];
		double cR;
		double cI;
		double denom1;
		double denom2;
		double denom;
		double UfR[];
		double UfI[];
		double UR[];
		double UI[];
		double numeratorR[];
		double numeratorI[];
		double temp;
		double denominatorR[];
		double denominatorI[];
		double E[];
		double correctedPixel;

		for (i = 0; i < length; i++) {
			if (entireImage || (mask.get(i) && (confidence == null))
					|| ((confidence != null) && (confidence[i] > 0.0))) {
				pixel = unsharpened[i];
				if (pixel > binMaximum) {
					binMaximum = pixel;
				}
				if (pixel < binMinimum) {
					binMinimum = pixel;
				}
			} // if (entireImage || mask.get(i) || ((confidence != null) &&
				// (confidence[i] > 0.0))) {
		} // for (i = 0; i < length; i++)

		histogramSlope = (binMaximum - binMinimum)
				/ (double) (numberOfHistogramBins - 1);
		// Create the intensity profile (within the masked region, if
		// applicable)
		// using a triangular parzen windowing scheme
		for (i = 0; i < length; i++) {
			if (entireImage || (mask.get(i) && (confidence == null))
					|| ((confidence != null) && (confidence[i] > 0.0))) {
				pixel = unsharpened[i];
				cidx = (pixel - binMinimum) / histogramSlope;
				idx = (int) Math.floor(cidx);
				offset = cidx - idx;

				if (offset == 0.0) {
					H[idx] += 1.0;
				} else if (idx < numberOfHistogramBins - 1) {
					H[idx] += 1.0 - offset;
					H[idx + 1] += offset;
				}
			} // if (entireImage || mask.get(i) || ((confidence != null) &&
				// (confidence[i] > 0.0)))
		} // for (i = 0; i < length; i++)

		// Determine information about the intensity histogram and zero-pad
		// histogram to a power of 2.

		exponent = Math.ceil(Math.log(numberOfHistogramBins) / Math.log(2.0)) + 1.0;
		paddedHistogramSize = (int) (Math.pow(2.0, exponent) + 0.5);
		histogramOffset = (int) (0.5 * (paddedHistogramSize - numberOfHistogramBins));
		VfR = new double[paddedHistogramSize];
		VfI = new double[paddedHistogramSize];
		for (n = 0; n < numberOfHistogramBins; n++) {
			VfR[n + histogramOffset] = H[n];
		}

		// Instantiate the 1d FFT routine
		// -1 for forward transform
		fft = new FFTUtility(VfR, VfI, 1, paddedHistogramSize, 1, -1,
				FFTUtility.FFT);
		fft.run();
		fft.finalize();
		fft = null;

		// Create the Gaussian filter
		scaledFWHM = biasFieldFullWidthAtHalfMaximum / histogramSlope;
		expFactor = 4.0 * Math.log(2.0) / (scaledFWHM * scaledFWHM);
		scaleFactor = 2.0 * Math.sqrt(Math.log(2.0) / Math.PI) / scaledFWHM;

		FfR = new double[paddedHistogramSize];
		FfI = new double[paddedHistogramSize];
		FfR[0] = scaleFactor;
		halfSize = (int) (0.5 * paddedHistogramSize);
		for (n = 1; n <= halfSize; n++) {
			FfR[n] = FfR[paddedHistogramSize - n] = scaleFactor
					* Math.exp(-n * n * expFactor);
		}
		if (paddedHistogramSize % 2 == 0) {
			FfR[halfSize] = scaleFactor
					* Math.exp(-0.25 * paddedHistogramSize
							* paddedHistogramSize * expFactor);
		}

		// -1 for forward transform
		fft = new FFTUtility(FfR, FfI, 1, paddedHistogramSize, 1, -1,
				FFTUtility.FFT);
		fft.run();
		fft.finalize();
		fft = null;

		// Create the Wiener deconvolution filter
		GfR = new double[paddedHistogramSize];
		GfI = new double[paddedHistogramSize];
		for (n = 0; n < paddedHistogramSize; n++) {
			cR = FfR[n];
			cI = -FfI[n];
			// (cR + jcI)/(cR*FfR[n] - cI*FfI[n] + WienerFilterNoise +
			// j(cR*FfI[n] + cI*FfR[n])
			// Multiply numerator and denominator by complex conjugate of
			// denominator
			denom1 = cR * FfR[n] - cI * FfI[n] + WienerFilterNoise;
			denom2 = cR * FfI[n] + cI * FfR[n];
			denom = denom1 * denom1 + denom2 * denom2;
			GfR[n] = (cR * cR * FfR[n] + cR * WienerFilterNoise + cI * cI
					* FfR[n])
					/ denom;
			GfI[n] = (-cR * cR * FfI[n] - cI * cI * FfI[n] + cI * WienerFilterNoise)
					/ denom;
		}
		UfR = new double[paddedHistogramSize];
		UfI = new double[paddedHistogramSize];
		UR = new double[paddedHistogramSize];
		UI = new double[paddedHistogramSize];
		for (n = 0; n < paddedHistogramSize; n++) {
			UfR[n] = VfR[n] * GfR[n];
			UR[n] = UfR[n];
			UfI[n] = VfI[n] * GfR[n];
			UI[n] = UfI[n];
		}
		// +1 for backward transform
		fft = new FFTUtility(UR, UI, 1, paddedHistogramSize, 1, 1,
				FFTUtility.FFT);
		fft.run();
		fft.finalize();
		fft = null;
		denominatorR = new double[paddedHistogramSize];
		denominatorI = new double[paddedHistogramSize];
		for (n = 0; n < paddedHistogramSize; n++) {
			UR[n] = Math.max(UR[n], 0.0);
			denominatorR[n] = UR[n];
			UI[n] = 0.0;
		}

		// Compute mapping E(u|v)
		numeratorR = new double[paddedHistogramSize];
		numeratorI = new double[paddedHistogramSize];
		for (n = 0; n < paddedHistogramSize; n++) {
			numeratorR[n] = (binMinimum + (n - histogramOffset)
					* histogramSlope)
					* UR[n];
		}
		// -1 for forward transform
		fft = new FFTUtility(numeratorR, numeratorI, 1, paddedHistogramSize, 1,
				-1, FFTUtility.FFT);
		fft.run();
		fft.finalize();
		fft = null;
		for (n = 0; n < paddedHistogramSize; n++) {
			temp = numeratorR[n];
			numeratorR[n] = numeratorR[n] * FfR[n] - numeratorI[n] * FfI[n];
			numeratorI[n] = temp * FfI[n] + numeratorI[n] * FfR[n];
		}
		// +1 for backward transform
		fft = new FFTUtility(numeratorR, numeratorI, 1, paddedHistogramSize, 1,
				1, FFTUtility.FFT);
		fft.run();
		fft.finalize();
		fft = null;
		// -1 for forward transform
		fft = new FFTUtility(denominatorR, denominatorI, 1,
				paddedHistogramSize, 1, -1, FFTUtility.FFT);
		fft.run();
		fft.finalize();
		fft = null;
		for (n = 0; n < paddedHistogramSize; n++) {
			temp = denominatorR[n];
			denominatorR[n] = denominatorR[n] * FfR[n] - denominatorI[n]
					* FfI[n];
			denominatorI[n] = temp * FfI[n] + denominatorI[n] * FfR[n];
		}
		// +1 for backward transform
		fft = new FFTUtility(denominatorR, denominatorI, 1,
				paddedHistogramSize, 1, 1, FFTUtility.FFT);
		fft.run();
		fft.finalize();
		fft = null;
		// Remove the zero-padding from the mapping
		E = new double[numberOfHistogramBins];
		for (n = 0; n < numberOfHistogramBins; n++) {
			E[n] = numeratorR[n + histogramOffset]
					/ denominatorR[n + histogramOffset];
			if (Double.isNaN(E[n]) || Double.isInfinite(E[n])) {
				E[n] = 0.0;
			}
		}

		// Sharpen the image with the new mapping, E(u|v)
		for (i = 0; i < length; i++) {
			if (entireImage || (mask.get(i) && (confidence == null))
					|| ((confidence != null) && (confidence[i] > 0.0))) {
				cidx = (unsharpened[i] - binMinimum) / histogramSlope;
				idx = (int) Math.floor(cidx);

				if (idx < E.length - 1) {
					correctedPixel = E[idx] + (E[idx + 1] - E[idx])
							* (cidx - idx);
				} else {
					correctedPixel = E[E.length - 1];
				}
				sharpened[i] = correctedPixel;
			}
		}
		return sharpened;
	}

	private double[] updateBiasFieldEstimate(double[] fieldEstimate) {
    	double smoothField[] = new double[fieldEstimate.length];
    	double maxAbsValue;
    	double minAbsValue;
    	int i;
    	int j;
    	int d;
    	double pixel;
    	double direction[][] = new double[nDims][nDims];
    	double dirLength;
    	Vector<Vector4d> pointLocation = new Vector<Vector4d>();
    	Vector<Double> pointData = new Vector<Double>();
    	int index;
    	int x;
    	int y;
    	int z;
    	int t;
    	Vector4d point;
    	double confidenceWeight;
    	double sigmoidWeight;
    	double alpha;
    	double beta;
    	Vector<Double> weights = new Vector<Double>();
    	AlgorithmBSplineScatteredDataPointSetToImageFilter bspliner;
    	// Use maskOrigin for parametricOrigin
    	// Calculate min/max for sigmoid weighting.  Calculate mean for offsetting
    	// bias field calculations since B-spline algorithm biases the result to zero.
    	maxAbsValue = -Double.MAX_VALUE;
    	minAbsValue = Double.MAX_VALUE;
    	if (sigmoidNormalizedAlpha > 0.0) {
	    	for (i = 0; i < length; i++) {
	    		if (entireImage || (mask.get(i) && (confidence == null))
	    				||  ((confidence != null) && (confidence[i] > 0.0))) {
	    			pixel = Math.abs(fieldEstimate[i]);
	    			if (pixel > maxAbsValue) {
	    				maxAbsValue = pixel;
	    			}
	    			if (pixel < minAbsValue) {
	    				minAbsValue = pixel;
	    			}
	    		} // if (entireImage || mask.get(i) || ((confidence != null) && (confidence[i] > 0.0)))	
	        } // for (i = 0; i < length; i++)
    	} // if (sigmoidNormalizedAlpha > 0.0)
    	
    	// Get original direction and change to identity temporarily for the
    	// b-spline fitting
    	// In ITK:
    	// row direction = direction cosines of first axis = [m_Direction[0][0], m_Direction[0][1], m_Direction[0][2]]
    	// column direction = direction cosines of second axis = [m_Direction[1][0], m_Direction[1][1], m_Direction[1][2]]
    	// slice direction = direction cosines of third axis = [m_Direction[2][0], m_Direction[2][1], m_Direction[2][2]]
    	for (i = 0; i < Math.min(nDims,3); i++) {
    		direction[i][i] = 1;
    	}
    	index = 0;
    	for (i = 0; i < length; i++) {
    		if (entireImage || (mask.get(i) && (confidence == null)) || 
    				((confidence != null) && (confidence[i] > 0.0))) {
    			x = i  % xDim;
    			y = (i % sliceSize) / xDim;
    			z = (i % xyzSize)/sliceSize;
    			t = i / xyzSize;
    			// Remember direction cosine matrix has been set to identity
    			float zComp = 0.0f;
    			float tComp = 0.0f;
    			if (nDims > 2) {
    				zComp = z*resolutions[2] + origin[2];
    				if (nDims > 3) {
    					tComp = t*resolutions[3] + origin[3];
    				}
    			}
    			point = new Vector4d(x*resolutions[0] + origin[0], y*resolutions[1] + origin[1], 
    					zComp, tComp);
    			pointData.add(index, fieldEstimate[i]);
    			pointLocation.add(index, point);
    			confidenceWeight = 1.0;
    			if (confidence != null) {
    				confidenceWeight = confidence[i];
    			}
    			sigmoidWeight = 1.0;
    			if (sigmoidNormalizedAlpha > 0.0) {
    			    alpha = (maxAbsValue - minAbsValue) / (12.0 * sigmoidNormalizedAlpha);
    			    beta = minAbsValue + (maxAbsValue - minAbsValue) * sigmoidNormalizedBeta;
    			    sigmoidWeight = 1.0 / (1.0 + Math.exp(-(fieldEstimate[i] - beta)/alpha));
    			} // if (sigmoidNormalizedAlpha > 0.0)
    			weights.insertElementAt(sigmoidWeight * confidenceWeight, index);
    			index++;
    		}
    	}
    	//if ( (srcImage.getMatrixHolder().containsType(TransMatrix.TRANSFORM_SCANNER_ANATOMICAL))
               // || (srcImage.getFileInfo()[0].getFileFormat() == FileUtility.DICOM)) {
            for (i = 0; i < Math.min(nDims,3); i++) {
            	dirLength = 0;
                for (j = 0; j < Math.min(nDims,3); j++) {
                    direction[i][j] = srcImage.getMatrix().get(i, j);
                    dirLength += (direction[i][j] * direction[i][j]);
                }
                dirLength = Math.sqrt(dirLength);
                for (j = 0; j < Math.min(nDims,3); j++) {
                	direction[i][j] = direction[i][j]/dirLength;
                }
            } // for (i = 0; i < Math.min(nDims,3); i++)
    	//}
    	bspliner = new AlgorithmBSplineScatteredDataPointSetToImageFilter(nDims);
    	int numberOfFittingLevels[] = new int[nDims];
    	int numberOfControlPoints[] = new int[nDims];
        for (i = 0; i < nDims; i++) {
        	numberOfFittingLevels[i] = 1;
        }
        for (d = 0; d < nDims; d++) {
            if (logBiasFieldControlPointLattice == null) {
            	numberOfControlPoints[d] = originalNumberOfControlPoints[d];
            }
            else {
            	numberOfControlPoints[d] = logBiasFieldControlPointLattice.getExtents()[d];
            }
        }
        bspliner.setOrigin(maskOrigin);
        bspliner.setResolutions(resolutions);
        bspliner.setExtents(srcImage.getExtents());
        bspliner.setDirection(direction);
        bspliner.setGenerateOutputImage(false);
        bspliner.setNumberOfLevels(numberOfFittingLevels);
        bspliner.setSplineOrder(splineOrder);
        bspliner.setNumberOfControlPoints(numberOfControlPoints);
        bspliner.setPointData(pointData);
        bspliner.setPointLocation(pointLocation);
        bspliner.setPointWeights(weights);
        bspliner.generateData();
        
        // Add the bias field control points to the current estimate
        int latticeExtents[] = null;
        int latticeExtentsLength = 1;
        if (logBiasFieldControlPointLattice == null) {
        	logBiasFieldControlPointLattice = bspliner.getPhiLattice();
        	latticeExtents = logBiasFieldControlPointLattice.getExtents();
        	for (i = 0; i < latticeExtents.length; i++) {
        		latticeExtentsLength *= latticeExtents[i];
        	}
        }
        else {
        	for (i = 0; i < latticeExtentsLength; i++) {
        		logBiasFieldControlPointLattice.set(i, logBiasFieldControlPointLattice.getDouble(i) + 
        				                               bspliner.getPhiLattice().getDouble(i));
        	}
        }
        bspliner.finalize();
        bspliner = null;
        
        AlgorithmBSplineControlPointImageFilter reconstructer = new AlgorithmBSplineControlPointImageFilter(nDims);
        reconstructer.setInput(logBiasFieldControlPointLattice);
        reconstructer.setOrigin(origin);
        reconstructer.setResolutions(resolutions);
        reconstructer.setDirection(direction);
        reconstructer.setExtents(srcImage.getExtents());
        reconstructer.generateData();
        
        smoothField = reconstructer.getOutputBuffer();
        reconstructer.finalize();
        reconstructer = null;
        
    	return smoothField;
    }

	private double calculateConvergenceMeasurement(double[] fieldEstimate1,
			double[] fieldEstimate2) {
		int i;
		double subtracter[] = new double[fieldEstimate1.length];
		for (i = 0; i < length; i++) {
			if (entireImage || (mask.get(i) && (confidence == null))
					|| ((confidence != null) && (confidence[i] > 0.0))) {
				subtracter[i] = fieldEstimate1[i] - fieldEstimate2[i];
			}
		}

		// Calculate statistics over the mask region
		double mu = 0.0;
		double sigma = 0.0;
		double N = 0.0;

		for (i = 0; i < length; i++) {
			if (entireImage || (mask.get(i) && (confidence == null))
					|| ((confidence != null) && (confidence[i] > 0.0))) {
				double pixel = Math.exp(subtracter[i]);
				N += 1.0;

				if (N > 1.0) {
					double diff = pixel - mu;
					sigma = sigma + diff * diff * (N - 1.0) / N;
				}
				mu = mu * (1.0 - 1.0 / N) + pixel / N;
			}
		}
		sigma = Math.sqrt(sigma / (N - 1.0));
		return (sigma / mu);
	}
	
	private void selfTest() {
	  // Create an image where the illumination doubles across x
	  double outputBuffer[] = new double[sliceSize];
	  for (int y = 0; y < destImage.getExtents()[1]; y++) {
		  for (int x = 0; x < destImage.getExtents()[0]; x++) {
			  int i = x + y * destImage.getExtents()[0];
			  outputBuffer[i] = 100 * (1.0 + (double)x/(double)(destImage.getExtents()[0]));  
		  }
	  }
	  
	  try {
		  destImage.importData(0, outputBuffer, true);
	  }
	  catch (IOException e) {
		  MipavUtil.displayError("IOException on destImage.importData(0, outputBuffer, true");
	  }
	  return;
	}
}
