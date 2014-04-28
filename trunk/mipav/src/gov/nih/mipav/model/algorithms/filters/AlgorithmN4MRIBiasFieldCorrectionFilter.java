package gov.nih.mipav.model.algorithms.filters;

import gov.nih.mipav.model.algorithms.AlgorithmBase;
import gov.nih.mipav.model.structures.ModelImage;

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
 *         This a a port of itkN4MRIBiasFieldCorrectionImageFilter.txx from the
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

	private int originalNumberOfFittingLevels[];

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
	 * @param confidenceImage
	 * @param maskFlag
	 *            the mask flag
	 */
	public AlgorithmN4MRIBiasFieldCorrectionFilter(ModelImage destImg, ModelImage fieldImage,
			ModelImage srcImg, int maximumIterations, double convergenceThreshold, double biasFieldFullWidthAtHalfMaximum,
			double WienerFilterNoise, ModelImage confidenceImage, boolean maskFlag) {
		super(destImg, srcImg);
		this.fieldImage = fieldImage;
		this.maximumIterations = maximumIterations;
		this.convergenceThreshold = convergenceThreshold;
		this.biasFieldFullWidthAtHalfMaximum = biasFieldFullWidthAtHalfMaximum;
		this.WienerFilterNoise = WienerFilterNoise;
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

		fireProgressStateChanged(0, srcImage.getImageName(),
				"N4 MRI Bias Field Correction Filter on image ...");
		
		mask = null;

        if (entireImage == false) {
            mask = srcImage.generateVOIMask();
        }


		maximumNumberOfIterations = new int[1];
		for (i = 0; i < maximumNumberOfIterations.length; i++) {
			maximumNumberOfIterations[i] = maximumIterations;
		}

		srcImage.calcMinMax();
		origin = srcImage.getOrigin();
		resolutions = srcImage.getFileInfo()[0].getResolutions();
		nDims = srcImage.getNDims();
		originalNumberOfFittingLevels = new int[nDims];
		for (i = 0; i < nDims; i++) {
			originalNumberOfFittingLevels[i] = 1;
		}
		originalNumberOfControlPoints = new int[nDims];
		for (i = 0; i < nDims; i++) {
			originalNumberOfControlPoints[i] = 4;
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
				for (i = 0; i < length; i++) {
					if (entireImage || (mask.get(i) && (confidence == null))
							|| ((confidence != null) && (confidence[i] > 0.0))) {
						logBiasField[i] = newLogBiasField[i];
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
    			point = new Vector4d(x + origin[0], y + origin[1], z + origin[2], t + origin[3]);
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
        if (logBiasFieldControlPointLattice == null) {
        	logBiasFieldControlPointLattice = bspliner.getPhiLattice();
        }
        else {
        	for (i = 0; i < length; i++) {
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
}
