package gov.nih.mipav.model.algorithms;

import gov.nih.mipav.model.algorithms.filters.AlgorithmMedian;
import gov.nih.mipav.model.algorithms.filters.FFTUtility;
import gov.nih.mipav.model.algorithms.utilities.AlgorithmChangeType;
import gov.nih.mipav.model.algorithms.utilities.AlgorithmRGBtoGray;
import gov.nih.mipav.model.file.FileBase;
import gov.nih.mipav.model.file.FileInfoBase;
import gov.nih.mipav.model.structures.ModelImage;
import gov.nih.mipav.model.structures.ModelStorageBase;
import gov.nih.mipav.model.structures.TransMatrix;
import gov.nih.mipav.view.MipavUtil;
import gov.nih.mipav.view.ViewJFrameImage;

import java.io.IOException;

import WildMagic.LibFoundation.Mathematics.Vector3f;

/**
 * 
 Overview
 * 
 * This code contains the texture analysis functions for the paper `Texture
 * Analysis and Segmentation Using Modulation Features, Generative Models, and
 * Weighted Curve Evolution', by I. Kokkinos, G. Evangelopoulos and P. Maragos,
 * appearing in IEEE Transactions on Pattern Analysis and Machine Intelligence,
 * Volume 31, Issue 1, Jan. 2009 Page(s):142 - 157.
 * 
 * This toolbox was originally developed in and written for MATLAB, with
 * emphasis on efficient algorithm implementations for multiband image
 * filtering, demodulation in amplitude (AM) and frequency (FM) signals via the
 * regularized 2D discrete energy separation algorithm and probabilistic
 * localization of texture, edge, smooth image regions.
 * 
 * Author of the original MATLAB toolbox is Iasonas Kokkinos, currently
 * Assistant Professor at Ecole Centrale Paris, with partial contributions from
 * Georgios Evangelopoulos. The original MATLAB toolbox has been ported from
 * MATLAB to Java by William Gandler. The original MATLAB toolbox can be found
 * at http://cvsp.cs.ntua.gr/software/texture/. Permission to port the original
 * code was generously granted by Iasonas Kokkinos.
 * 
 * The provided functions include:
 * 
 * multi-scale & orientation filterbanks for gabors and edges projection on the
 * basis elements of the underlying generative models demodulation with
 * regularized/complex esa channel selection based on the amplitude/teager/mdl
 * criterion texture/edge/smooth classification based on mdl criterion
 * 
 * Parameters are primarily related to the filterbank construction and the final
 * classification stage.
 * 
 * 
 * References:
 * 
 * 1.) I. Kokkinos, G. Evangelopoulos and P. Maragos, Texture Analysis and
 * Segmentation using Modulation Features, Generative Models and Weighted Curve
 * Evolution, IEEE Transactions on Pattern Analysis & Machine Intelligence, vol.
 * 31, no. 1, pp. 142-157, Jan. 2009.
 * 
 * 2.) G. Evangelopoulos, I. Kokkinos and P. Maragos, Advances in Variational
 * Image Segmentation using AM-FM Models: Regularized Demodulation and
 * Probabilistic Cue Integration, Proc. Int' l Workshop on Variational and Level
 * Set Methods (VLSM-05), Beijing, China, Oct. 2005, Springer LNCS, vol. 3275,
 * pp. 121-136.
 * 
 * 3.) I. Kokkinos, G. Evangelopoulos and P. Maragos, Advances in Texture
 * Analysis: Energy Dominant Component and Multiple Hypothesis Testing, Proc.
 * IEEE Int' l Conf. on Image Processing (ICIP-04), Singapore, Oct. 2004, vol.
 * 3, pp. 1509-1512.
 * 
 * 4.) I. Kokkinos, G. Evangelopoulos and P. Maragos, Modulation-Feature Based
 * Textured Image Segmentation Using Curve Evolution, Proc. IEEE Int' l Conf. on
 * Image Processing (ICIP-04), Singapore, Oct. 2004, vol. 2, pp. 1204-1207.
 * 
 */

public class AlgorithmTextureAnalysis extends AlgorithmBase {
	
	private ModelImage[] destImage = null;

	private final boolean scaleImage;

	// epsilon = D1MACH(4)
	// Machine epsilon is the smallest positive epsilon such that
	// (1.0 + epsilon) != 1.0.
	// epsilon = 2**(1 - doubleDigits) = 2**(1 - 53) = 2**(-52)
	// epsilon = 2.2204460e-16
	// epsilon is called the largest relative spacing
	private final double epsilon = Math.pow(2.0, -52);

	private boolean displayFilters = false;

	private ModelImage texttd2Image;

	private ModelImage texttd3Image;

	private ModelImage edgetd2Image;

	private ModelImage edgetd3Image;

	private ModelImage gImage;

	// ~ Constructors
	// ---------------------------------------------------------------------------------------------------
	public AlgorithmTextureAnalysis(ModelImage[] destImage, ModelImage srcImage,
			boolean scaleImage) {
		super(null, srcImage);
		this.destImage = destImage;
		this.scaleImage = scaleImage;
	}

	/**
	 * Starts the program.
	 */
	@Override
	public void runAlgorithm() {
		AlgorithmChangeType changeTypeAlgo;
		final boolean image25D = true;
		AlgorithmTransform algoTrans;
		AlgorithmRGBtoGray gAlgo;
		ModelImage grayImage = null;
		ModelImage inputImage = null;
		int k;
		int x;
		int y;
		int inputXDim;
		int inputYDim;
		final int ndirs = 10;
		final int nscales = 4;
		// Proportional to number of oscillations within Gaussian
		final double sig2omega = 1.0;
		int minSize;
		double largestPeriod;
		// Range of radians per pixel for sinusoid of Gabor filter
		final double radianStart = 0.7; // Highest frequency
		double radianEnd; // Lowest frequency
		double textinvDes[][][][][] = new double[ndirs * nscales][][][][];
		double texttd1[][][] = new double[ndirs * nscales][][];
		double texttd2[][][] = new double[ndirs * nscales][][];
		double texttd3[][][] = new double[ndirs * nscales][][];
		double texttd22[][][] = new double[ndirs * nscales][][];
		double texttd23[][][] = new double[ndirs * nscales][][];
		double texttd33[][][] = new double[ndirs * nscales][][];
		double textfd1[][][] = new double[ndirs * nscales][][];
		double textfd1Imag[][][] = new double[ndirs * nscales][][];
		double textfd2[][][] = new double[ndirs * nscales][][];
		double textfd2Imag[][][] = new double[ndirs * nscales][][];
		double textfd3[][][] = new double[ndirs * nscales][][];
		double textfd3Imag[][][] = new double[ndirs * nscales][][];
		double textfd22[][][] = new double[ndirs * nscales][][];
		double textfd22Imag[][][] = new double[ndirs * nscales][][];
		double textfd23[][][] = new double[ndirs * nscales][][];
		double textfd23Imag[][][] = new double[ndirs * nscales][][];
		double textfd33[][][] = new double[ndirs * nscales][][];
		double textfd33Imag[][][] = new double[ndirs * nscales][][];
		double textsigmas[] = new double[ndirs * nscales];
		int textps[] = new int[ndirs * nscales];
		String textdomain[] = new String[ndirs * nscales];
		double textomegas[][] = new double[ndirs * nscales][];
		double textamplitudes[][] = new double[ndirs * nscales][];
		double textfilterAngle[] = new double[ndirs * nscales];
		double textsigmaX[] = new double[ndirs * nscales];
		double textA[][];
		double textph[][];
		double textFx[][];
		double textFy[][];
		int textidx[][];
		double texten[][];
		double textcritDCA[][];
		double edgeinvDes[][][][][] = new double[ndirs * nscales][][][][];
		double edgetd1[][][] = new double[ndirs * nscales][][];
		double edgetd2[][][] = new double[ndirs * nscales][][];
		double edgetd3[][][] = new double[ndirs * nscales][][];
		double edgetd22[][][] = new double[ndirs * nscales][][];
		double edgetd23[][][] = new double[ndirs * nscales][][];
		double edgetd33[][][] = new double[ndirs * nscales][][];
		double edgefd1[][][] = new double[ndirs * nscales][][];
		double edgefd1Imag[][][] = new double[ndirs * nscales][][];
		double edgefd2[][][] = new double[ndirs * nscales][][];
		double edgefd2Imag[][][] = new double[ndirs * nscales][][];
		double edgefd3[][][] = new double[ndirs * nscales][][];
		double edgefd3Imag[][][] = new double[ndirs * nscales][][];
		double edgefd22[][][] = new double[ndirs * nscales][][];
		double edgefd22Imag[][][] = new double[ndirs * nscales][][];
		double edgefd23[][][] = new double[ndirs * nscales][][];
		double edgefd23Imag[][][] = new double[ndirs * nscales][][];
		double edgefd33[][][] = new double[ndirs * nscales][][];
		double edgefd33Imag[][][] = new double[ndirs * nscales][][];
		double edgesigmas[] = new double[ndirs * nscales];
		int edgeps[] = new int[ndirs * nscales];
		String edgedomain[] = new String[ndirs * nscales];
		double edgeomegas[][] = new double[ndirs * nscales][];
		double edgeamplitudes[][] = new double[ndirs * nscales][];
		double edgefilterAngle[] = new double[ndirs * nscales];
		double edgesigmaX[] = new double[ndirs * nscales];
		double edgeA[][];
		double edgeph[][];
		double edgeFx[][];
		double edgeFy[][];
		int edgeidx[][];
		double edgeen[][];
		double edgecritDCA[][];
		double sgx[] = new double[ndirs * nscales];
		double invVariance2;
		double factorSharpness;
		double factorMdl;
		int scaleswt[] = new int[nscales];
		String DCAmethod = null;
		String esameth = null;
		int offset;
		double sg;
		double mdlCostScale;
		double critEdge;
		double critText;
		double expCritEdge;
		double expCritText;
		double denom;
		double ped[];
		double ptx[];
		double psm[];
		double synthText[];
		double freqText[];
		double synthEdge[];
		double freqEdge[];
		double buf[];
		double buf2[];
		ModelImage medianImage;
		int outputExtents[] = new int[2];
		AlgorithmMedian medianAlgo;
		int iters;
		int ksize;
		int kernelShape;
		float stdDev;
		int filterType;
		int maximumSize;
		boolean entireImage;
		inputXDim = srcImage.getExtents()[0];
		inputYDim = srcImage.getExtents()[1];
		FileInfoBase[] fileInfo;
		
		boolean testfreqz2 = false;
		if (testfreqz2) {
			testfreqz2();
			setCompleted(false);
			return;
		}
		
		boolean testconv2 = false;
		if (testconv2) {
			testconv2();
			setCompleted(false);
			return;
		}
		
		boolean testInvert = false;
		if (testInvert) {
			test_T2z0a_invert_design();
			setCompleted(false);
			return;
		}
		if (srcImage.isColorImage()) {
			final boolean thresholdAverage = false;
			final float threshold = 0.0f;
			final boolean intensityAverage = false;
			final boolean equalRange = true;
			final float minR = 0.0f;
			final float minG = 0.0f;
			final float minB = 0.0f;
			float redValue;
			float greenValue;
			float blueValue;
			float maxR;
			float maxG;
			float maxB;
			if (srcImage.getMinR() == srcImage.getMaxR()) {
				redValue = 0.0f;
				greenValue = 0.5f;
				blueValue = 0.5f;
			} else if (srcImage.getMinG() == srcImage.getMaxG()) {
				redValue = 0.5f;
				greenValue = 0.0f;
				blueValue = 0.5f;
			} else if (srcImage.getMinB() == srcImage.getMaxB()) {
				redValue = 0.5f;
				greenValue = 0.5f;
				blueValue = 0.0f;
			} else {
				redValue = (float) (1.0 / 3.0);
				greenValue = redValue;
				blueValue = redValue;

			}
			maxR = (float) srcImage.getMaxR();
			maxG = (float) srcImage.getMaxG();
			maxB = (float) srcImage.getMaxB();
			grayImage = new ModelImage(ModelStorageBase.FLOAT,
					srcImage.getExtents(), "grayImage");
			gAlgo = new AlgorithmRGBtoGray(grayImage, srcImage,
					redValue, greenValue, blueValue, thresholdAverage,
					threshold, intensityAverage, equalRange, minR, maxR,
					minG, maxG, minB, maxB);
			gAlgo.run();
			gAlgo.finalize();
		} // if (srcImage.isColorImage())
		
		inputImage = new ModelImage(ModelStorageBase.DOUBLE,
				srcImage.getExtents(), "changeTypeImage");
		inputImage.getFileInfo(0).setEndianess(FileBase.LITTLE_ENDIAN);
		if (srcImage.isColorImage()) {
			changeTypeAlgo = new AlgorithmChangeType(inputImage,
					grayImage, grayImage.getMin(), grayImage.getMax(),
					0.0, 1.0, image25D);
		} else {
			changeTypeAlgo = new AlgorithmChangeType(inputImage,
					srcImage, srcImage.getMin(),
					srcImage.getMax(), 0.0, 1.0, image25D);
		}
		changeTypeAlgo.run();
		changeTypeAlgo.finalize();
		changeTypeAlgo = null;
		if (grayImage != null) {
			grayImage.disposeLocal();
			grayImage = null;
		}
		fileInfo = inputImage.getFileInfo();
        fileInfo[0].setModality(srcImage.getFileInfo()[0].getModality());
        fileInfo[0].setFileDirectory(srcImage.getFileInfo()[0].getFileDirectory());
        fileInfo[0].setUnitsOfMeasure(srcImage.getFileInfo()[0].getUnitsOfMeasure());
        fileInfo[0].setResolutions(srcImage.getFileInfo()[0].getResolutions());
        fileInfo[0].setExtents(inputImage.getExtents());
        fileInfo[0].setMax(inputImage.getMax());
        fileInfo[0].setMin(inputImage.getMin());
        fileInfo[0].setImageOrientation(srcImage.getImageOrientation());
        fileInfo[0].setAxisOrientation(srcImage.getFileInfo()[0].getAxisOrientation());
        fileInfo[0].setOrigin(srcImage.getFileInfo()[0].getOrigin());
        if (scaleImage) {

			final boolean doPad = false;
			final TransMatrix xfrm = new TransMatrix(3);
			xfrm.identity();
			final int interp = AlgorithmTransform.BILINEAR;
			final int oXdim = 219;
			final int oYdim = 146;
			inputXDim = oXdim;
			inputYDim = oYdim;
			final float oXres = srcImage.getFileInfo()[0]
					.getResolutions()[0]
					* srcImage.getExtents()[0]
					/ oXdim;
			final float oYres = srcImage.getFileInfo()[0]
					.getResolutions()[1]
					* srcImage.getExtents()[1]
					/ oYdim;
			final int units[] = srcImage.getUnitsOfMeasure();
			final boolean doClip = true;
			final boolean doVOI = false;
			final boolean doRotateCenter = false;
			final Vector3f center = new Vector3f();
			final float fillValue = 0.0f;
			final boolean doUpdateOrigin = false;
			final boolean isSATransform = false;
			algoTrans = new AlgorithmTransform(inputImage, xfrm, interp,
					oXres, oYres, oXdim, oYdim, units, doVOI, doClip,
					doPad, doRotateCenter, center);
			algoTrans.setFillValue(fillValue);
			algoTrans.setUpdateOriginFlag(doUpdateOrigin);
			algoTrans.setUseScannerAnatomical(isSATransform);
			algoTrans.setSuppressProgressBar(true);

			algoTrans.run();
			inputImage.disposeLocal();

			inputImage = algoTrans.getTransformedImage();
			algoTrans.disposeLocal();
			algoTrans = null;
			inputImage.calcMinMax();
		} // if (scaleImage)

		minSize = Math.min(inputXDim, inputYDim);
		largestPeriod = minSize / 4.0;
		radianEnd = 2.0 * Math.PI / largestPeriod;
		
		for (y = 0; y < nscales; y++) {
			for (x = 0; x < ndirs; x++) {
				if (y < 3) {
					textdomain[x + y * ndirs] = "time";
					edgedomain[x + y * ndirs] = "time";
				}
				else {
					textdomain[x + y * ndirs] = "freq";
					edgedomain[x + y * ndirs] = "freq";	
				}
			}
		}

		// Construct filters and their time or frequency response
		// For efficiency use the time domain for small
		// filters and the frequency domain for large ones
		T1_responses(textomegas, textamplitudes,
				textfilterAngle, textsigmaX, texttd1, texttd2, texttd3,
				texttd22, texttd23, texttd33, textfd1, textfd1Imag,
				textfd2, textfd2Imag, textfd3, textfd3Imag, textfd22,
				textfd22Imag, textfd23, textfd23Imag, textfd33,
				textfd33Imag, textsigmas, textps, nscales, ndirs,
				sig2omega, radianStart, radianEnd, inputXDim,
				inputYDim, "texture", textdomain);
		T1_responses(edgeomegas, edgeamplitudes,
				edgefilterAngle, edgesigmaX, edgetd1, edgetd2, edgetd3,
				edgetd22, edgetd23, edgetd33, edgefd1, edgefd1Imag,
				edgefd2, edgefd2Imag, edgefd3, edgefd3Imag, edgefd22,
				edgefd22Imag, edgefd23, edgefd23Imag, edgefd33,
				edgefd33Imag, edgesigmas, edgeps, nscales, ndirs,
				sig2omega, radianStart, radianEnd, inputXDim,
				inputYDim, "edge", edgedomain);
	
			// Terms computed off-line for weighted projection on basis
	
			// These account for boundary conditions & for a
			// non-zero mean value of the even filter
			T2z0_projection_terms(textinvDes, texttd1, texttd2, texttd3,
					texttd22, texttd23, texttd33, textfd1, textfd1Imag,
					textfd2, textfd2Imag, textfd3, textfd3Imag, textfd22,
					textfd22Imag, textfd23, textfd23Imag, textfd33,
					textfd33Imag, textps, nscales, ndirs, textdomain,
					inputImage);
	
			T2z0_projection_terms(edgeinvDes, edgetd1, edgetd2, edgetd3,
					edgetd22, edgetd23, edgetd33, edgefd1, edgefd1Imag,
					edgefd2, edgefd2Imag, edgefd3, edgefd3Imag, edgefd22,
					edgefd22Imag, edgefd23, edgefd23Imag, edgefd33,
					edgefd33Imag, edgeps, nscales, ndirs, edgedomain,
					inputImage);
	        if (displayFilters) {
				double texttimetd1[][][] = new double[ndirs * nscales][][];
				double texttimetd2[][][] = new double[ndirs * nscales][][];
				double texttimetd3[][][] = new double[ndirs * nscales][][];
				double texttimetd22[][][] = new double[ndirs * nscales][][];
				double texttimetd23[][][] = new double[ndirs * nscales][][];
				double texttimetd33[][][] = new double[ndirs * nscales][][];
				double texttimefd1[][][] = new double[ndirs * nscales][][];
				double texttimefd1Imag[][][] = new double[ndirs * nscales][][];
				double texttimefd2[][][] = new double[ndirs * nscales][][];
				double texttimefd2Imag[][][] = new double[ndirs * nscales][][];
				double texttimefd3[][][] = new double[ndirs * nscales][][];
				double texttimefd3Imag[][][] = new double[ndirs * nscales][][];
				double texttimefd22[][][] = new double[ndirs * nscales][][];
				double texttimefd22Imag[][][] = new double[ndirs * nscales][][];
				double texttimefd23[][][] = new double[ndirs * nscales][][];
				double texttimefd23Imag[][][] = new double[ndirs * nscales][][];
				double texttimefd33[][][] = new double[ndirs * nscales][][];
				double texttimefd33Imag[][][] = new double[ndirs * nscales][][];
				double texttimesigmas[] = new double[ndirs * nscales];
				int texttimeps[] = new int[ndirs * nscales];
				String texttimedomain[] = new String[ndirs * nscales];
				double texttimeomegas[][] = new double[ndirs * nscales][];
				double texttimeamplitudes[][] = new double[ndirs * nscales][];
				double texttimefilterAngle[] = new double[ndirs * nscales];
				double texttimesigmaX[] = new double[ndirs * nscales];
				double edgetimetd1[][][] = new double[ndirs * nscales][][];
				double edgetimetd2[][][] = new double[ndirs * nscales][][];
				double edgetimetd3[][][] = new double[ndirs * nscales][][];
				double edgetimetd22[][][] = new double[ndirs * nscales][][];
				double edgetimetd23[][][] = new double[ndirs * nscales][][];
				double edgetimetd33[][][] = new double[ndirs * nscales][][];
				double edgetimefd1[][][] = new double[ndirs * nscales][][];
				double edgetimefd1Imag[][][] = new double[ndirs * nscales][][];
				double edgetimefd2[][][] = new double[ndirs * nscales][][];
				double edgetimefd2Imag[][][] = new double[ndirs * nscales][][];
				double edgetimefd3[][][] = new double[ndirs * nscales][][];
				double edgetimefd3Imag[][][] = new double[ndirs * nscales][][];
				double edgetimefd22[][][] = new double[ndirs * nscales][][];
				double edgetimefd22Imag[][][] = new double[ndirs * nscales][][];
				double edgetimefd23[][][] = new double[ndirs * nscales][][];
				double edgetimefd23Imag[][][] = new double[ndirs * nscales][][];
				double edgetimefd33[][][] = new double[ndirs * nscales][][];
				double edgetimefd33Imag[][][] = new double[ndirs * nscales][][];
				double edgetimesigmas[] = new double[ndirs * nscales];
				int edgetimeps[] = new int[ndirs * nscales];
				String edgetimedomain[] = new String[ndirs * nscales];
				double edgetimeomegas[][] = new double[ndirs * nscales][];
				double edgetimeamplitudes[][] = new double[ndirs * nscales][];
				double edgetimefilterAngle[] = new double[ndirs * nscales];
				double edgetimesigmaX[] = new double[ndirs * nscales];
				double dhout[][] = null;
				double dhoutImag[][] = null;
				double mx[] = null;
				double mag;
				
				for (y = 0; y < nscales; y++) {
					for (x = 0; x < ndirs; x++) {
						texttimedomain[x + y * ndirs] = "time";
						edgetimedomain[x + y * ndirs] = "time";
						
					}
				}
	
				T1_responses(texttimeomegas, texttimeamplitudes,
						texttimefilterAngle,
						texttimesigmaX, texttimetd1, texttimetd2,
						texttimetd3, texttimetd22, texttimetd23,
						texttimetd33, texttimefd1, texttimefd1Imag,
						texttimefd2, texttimefd2Imag, texttimefd3,
						texttimefd3Imag, texttimefd22, texttimefd22Imag,
						texttimefd23, texttimefd23Imag, texttimefd33,
						texttimefd33Imag, texttimesigmas, texttimeps,
						nscales, ndirs, sig2omega, radianStart, radianEnd,
						inputXDim, inputYDim, "texture", texttimedomain);
				T1_responses(edgetimeomegas, edgetimeamplitudes,
						edgetimefilterAngle,
						edgetimesigmaX, edgetimetd1, edgetimetd2,
						edgetimetd3, edgetimetd22, edgetimetd23,
						edgetimetd33, edgetimefd1, edgetimefd1Imag,
						edgetimefd2, edgetimefd2Imag, edgetimefd3,
						edgetimefd3Imag, edgetimefd22, edgetimefd22Imag,
						edgetimefd23, edgetimefd23Imag, edgetimefd33,
						edgetimefd33Imag, edgetimesigmas, edgetimeps,
						nscales, ndirs, sig2omega, radianStart, radianEnd,
						inputXDim, inputYDim, "edge", edgetimedomain);
				// Max of gabor filter response, frequency domain
				mx = new double[200 * 200];
				dhout = new double[200][200];
				dhoutImag = new double[200][200];
				for (k = 0; k < 40; k++) {
					freqz2(dhout, dhoutImag, texttimetd2[k], 200,
							200);
					for (y = 0; y < 200; y++) {
						for (x = 0; x < 200; x++) {
							mag = Math.sqrt(dhout[y][x] * dhout[y][x]
									+ dhoutImag[y][x] * dhoutImag[y][x]);
							if (mag > mx[x + 200 * y]) {
								mx[x + 200 * y] = mag;
							}
						}
					}
				}
				for (x = 0; x < 200; x++) {
					dhout[x] = null;
					dhoutImag[x] = null;
				}
				dhout = null;
				dhoutImag = null;
				int dExtents[] = new int[2];
				dExtents[0] = 200;
				dExtents[1] = 200;
				gImage = new ModelImage(ModelStorageBase.DOUBLE, dExtents,
						"max_gabor_response");
				try {
					gImage.importData(0, mx, true);
				} catch (IOException e) {
					MipavUtil.displayError("IOException " + e
							+ " on gImage.importData(0, mx, true)");
					setCompleted(false);
					return;
				}
				new ViewJFrameImage(gImage);
				// Show texture and edge filters at a single scale and
				// orientation
				int filId = 10;
				int texttimeLength = texttimetd2[0].length
						* texttimetd2[0][0].length;
				double buffer[] = new double[texttimeLength];
	
				for (y = 0; y < texttimetd2[0].length; y++) {
					for (x = 0; x < texttimetd2[0][0].length; x++) {
						buffer[x + y * texttimetd2[0][0].length] = texttimetd2[filId][y][x];
					}
				}
	
				dExtents[0] = texttimetd2[0][0].length;
				dExtents[1] = texttimetd2[0].length;
				texttd2Image = new ModelImage(ModelStorageBase.DOUBLE,
						dExtents, "text_td2_Image");
				try {
					texttd2Image.importData(0, buffer, true);
				} catch (IOException e) {
					MipavUtil
							.displayError("IOException "
									+ e
									+ " on texttd2Image.importData(0, buffer, true)");
					setCompleted(false);
					return;
				}
				new ViewJFrameImage(texttd2Image);
	
				for (y = 0; y < texttimetd2[0].length; y++) {
					for (x = 0; x < texttimetd2[0][0].length; x++) {
						buffer[x + y * texttimetd2[0][0].length] = texttimetd3[filId][y][x];
					}
				}
				texttd3Image = new ModelImage(ModelStorageBase.DOUBLE,
						dExtents, "text_td3_Image");
				try {
					texttd3Image.importData(0, buffer, true);
				} catch (IOException e) {
					MipavUtil
							.displayError("IOException "
									+ e
									+ " on texttd3Image.importData(0, buffer, true)");
					setCompleted(false);
					return;
				}
				new ViewJFrameImage(texttd3Image);
	
				int edgetimeLength = edgetimetd2[0].length
						* edgetimetd2[0][0].length;
				buffer = new double[edgetimeLength];
	
				for (y = 0; y < edgetimetd2[0].length; y++) {
					for (x = 0; x < edgetimetd2[0][0].length; x++) {
						buffer[x + y * edgetimetd2[0][0].length] = edgetimetd2[filId][y][x];
					}
				}
				dExtents[0] = edgetimetd2[0][0].length;
				dExtents[1] = edgetimetd2[0].length;
				edgetd2Image = new ModelImage(ModelStorageBase.DOUBLE,
						dExtents, "edge_td2_Image");
				try {
					edgetd2Image.importData(0, buffer, true);
				} catch (IOException e) {
					MipavUtil
							.displayError("IOException "
									+ e
									+ " on edgetd2Image.importData(0, buffer, true)");
					setCompleted(false);
					return;
				}
				new ViewJFrameImage(edgetd2Image);
	
				for (y = 0; y < edgetimetd2[0].length; y++) {
					for (x = 0; x < edgetimetd2[0][0].length; x++) {
						buffer[x + y * edgetimetd2[0][0].length] = edgetimetd3[filId][y][x];
					}
				}
				edgetd3Image = new ModelImage(ModelStorageBase.DOUBLE,
						dExtents, "edge_td3_Image");
				try {
					edgetd3Image.importData(0, buffer, true);
				} catch (IOException e) {
					MipavUtil
							.displayError("IOException "
									+ e
									+ " on edgetd3Image.importData(0, buffer, true)");
					setCompleted(false);
					return;
				}
				new ViewJFrameImage(edgetd3Image);
			} // if (displayFilters)
	
		// mdl criterion terms (refer to paper)
		// G: gaussian function with maximum at (x == 0) equal to 1
		// sum G = (filter_scales.^2) * [2*PI]
		for (k = 0; k < ndirs * nscales; k++) {
			sgx[k] = (2.0 * Math.PI) * textsigmas[k] * textsigmas[k];
		}
		invVariance2 = 1.0 / (2.0 * 0.03 * 0.03);
		factorSharpness = 1.0;
		factorMdl = 1.0;
	
		// Determine scales over which the decision is taken
		for (k = 0; k < nscales; k++) {
			scaleswt[k] = k;
		}
	
		// For the whole process to run at a single scale modify the code
		// above
		// by an outer for loop
		// for (scaleInd = 0; scaleInd < nscales; scaleInd++) {
		// scaleswt = scaleInd;
		// }
	
		// Main part:
		// Multi-scale and orientation filtering for texture/edge signals
	
		// Note: Filtering for texture is bundled with demodulation
		// This 'inflates' the filter responses where they
		// are decreased due to a mismatch between
		// the signal's and the Gabor's central frequencies.
	
		// channel selection criterion
		// 'mdl' : mdl -like criterion (current)
		// 'teag' : teager energy (teager-based DCA)
		// 'ampl' : amplitude
	    DCAmethod = "mdl";
	
		// Choose demodulation algorithm
		// 'gesa' : Gabor - ESA
		// 'cesa' : Complex-ESA
		// '' : no demodulation (use Gabor filter's amplitude/frequency)
		esameth = "gesa";
		textA = new double[inputYDim][inputXDim];
		textph = new double[inputYDim][inputXDim];
		textFx = new double[inputYDim][inputXDim];
		textFy = new double[inputYDim][inputXDim];
		textidx = new int[inputYDim][inputXDim];
		texten = new double[inputYDim][inputXDim];
		textcritDCA = new double[inputYDim][inputXDim];
		
		T2z1_filter(textA, textph, textFx, textFy,
				textidx, texten, textcritDCA, 
				textomegas, textamplitudes,
				textfilterAngle, textsigmaX, texttd1, texttd2, texttd3,
				textfd1, textfd1Imag, textfd2, textfd2Imag, textfd3,
				textfd3Imag, textfd22, textfd22Imag, textfd23,
				textfd23Imag, textfd33, textfd33Imag, textps, textdomain,
				scaleswt, ndirs, inputImage, textinvDes, DCAmethod,
				esameth, sgx, invVariance2, factorSharpness, factorMdl);
		
		// Filtering for edges
		DCAmethod = "mdl";
		esameth = ""; // no demodulation edges
		edgeA = new double[inputYDim][inputXDim];
		edgeph = new double[inputYDim][inputXDim];
		edgeFx = new double[inputYDim][inputXDim];
		edgeFy = new double[inputYDim][inputXDim];
		edgeidx = new int[inputYDim][inputXDim];
		edgeen = new double[inputYDim][inputXDim];
		edgecritDCA = new double[inputYDim][inputXDim];
		T2z1_filter(edgeA, edgeph, edgeFx, edgeFy,
				edgeidx, edgeen, edgecritDCA, 
				edgeomegas, edgeamplitudes,
				edgefilterAngle, edgesigmaX, edgetd1, edgetd2, edgetd3,
				edgefd1, edgefd1Imag, edgefd2, edgefd2Imag, edgefd3,
				edgefd3Imag, edgefd22, edgefd22Imag, edgefd23,
				edgefd23Imag, edgefd33, edgefd33Imag, edgeps, edgedomain,
				scaleswt, ndirs, inputImage, edgeinvDes, DCAmethod,
				esameth, sgx, invVariance2, factorSharpness, factorMdl);
		
		offset = 1; // A design parameter that allows 'smooth' to become stronger
		
		// Different filters at different pixels
		ped = new double[inputXDim * inputYDim];
		ptx = new double[inputXDim * inputYDim];
		psm = new double[inputXDim * inputYDim];
		for (y = 0; y < inputYDim; y++) {
			for (x = 0; x < inputXDim; x++) {
			    sg = sgx[edgeidx[y][x]];
			    mdlCostScale = -Math.log(sg)/sg;
			    critEdge = factorSharpness*(factorMdl*mdlCostScale + edgeen[y][x]*invVariance2) - offset;
			    sg = sgx[textidx[y][x]];
			    mdlCostScale = -Math.log(sg)/sg;
			    critText = factorSharpness*(factorMdl*mdlCostScale + texten[y][x]*invVariance2) - offset;
			    expCritEdge = Math.exp(critEdge);
			    expCritText = Math.exp(critText);
			    denom = expCritEdge + expCritText + 1.0;
			    ped[x + y * inputXDim] = expCritEdge/denom;
			    ptx[x + y * inputXDim] = expCritText/denom;
			    psm[x + y * inputXDim] = 1.0 - ped[x + y * inputXDim] - ptx[x + y * inputXDim];
			}
		}
		
		// Visualization of results
	    synthText = new double[inputXDim * inputYDim];
	    synthEdge = new double[inputXDim * inputYDim];
	    for (y = 0; y < inputYDim; y++) {
	    	for (x = 0; x < inputXDim; x++) {
	    		synthText[x + y * inputXDim] = textA[y][x] * Math.cos(textph[y][x]);
	    		synthEdge[x + y * inputXDim] = edgeA[y][x] * Math.cos(edgeph[y][x]);
	    	}
	    }
	    
	    outputExtents[0] = inputXDim;
	    outputExtents[1] = inputYDim;
	    buf = new double[inputXDim * inputYDim];
	    for (y = 0; y < inputYDim; y++) {
	    	for (x = 0; x < inputXDim; x++) {
	    	    buf[x + y * inputXDim] = textFx[y][x];	
	    	}
	    }
	    medianImage = new ModelImage(ModelStorageBase.DOUBLE, outputExtents, "medianImage");
	    try {
	    	medianImage.importData(0, buf, true);
	    }
	    catch (IOException e) {
	    	MipavUtil.displayError("IOException " + e + " on medianImage.importData(0, buf, true)");
	    	setCompleted(false);
	    	return;
	    }
	    iters = 1;
	    ksize = 3;
	    kernelShape = AlgorithmMedian.SQUARE_KERNEL;
	    stdDev = 0.0f;
	    filterType = AlgorithmMedian.STANDARD;
	    maximumSize = 3;
	    entireImage = true;
	    medianAlgo = new AlgorithmMedian(medianImage, iters, ksize, kernelShape, stdDev, filterType, maximumSize, entireImage);
	    medianAlgo.run();
	    medianAlgo.finalize();
	    medianAlgo = null;
	    try {
	    	medianImage.exportData(0, inputXDim * inputYDim, buf);
	    }
	    catch (IOException e) {
	    	MipavUtil.displayError("IOException " + e + " on medianImage.exportData(0, inputXDim * inputYDim, buf)");
	    	setCompleted(false);
	    	return;	
	    }
	    
	    buf2 = new double[inputXDim * inputYDim];
	    for (y = 0; y < inputYDim; y++) {
	    	for (x = 0; x < inputXDim; x++) {
	    	    buf2[x + y * inputXDim] = textFy[y][x];
	    	}
	    }
	    try {
	    	medianImage.importData(0, buf2, true);
	    }
	    catch (IOException e) {
	    	MipavUtil.displayError("IOException " + e + " on medianImage.importData(0, buf2, true)");
	    	setCompleted(false);
	    	return;
	    }
	    medianAlgo = new AlgorithmMedian(medianImage, iters, ksize, kernelShape, stdDev, filterType, maximumSize, entireImage);
	    medianAlgo.run();
	    medianAlgo.finalize();
	    medianAlgo = null;
	    try {
	    	medianImage.exportData(0, inputXDim * inputYDim, buf2);
	    }
	    catch (IOException e) {
	    	MipavUtil.displayError("IOException " + e + " on medianImage.exportData(0, inputXDim * inputYDim, buf2)");
	    	setCompleted(false);
	    	return;	
	    }
	    freqText = new double[inputXDim * inputYDim];
	    for (x = 0; x < inputXDim * inputYDim; x++) {
	    	freqText[x] = Math.sqrt(buf[x]*buf[x] + buf2[x]*buf2[x]);
	    }
	    
	    for (y = 0; y < inputYDim; y++) {
	    	for (x = 0; x < inputXDim; x++) {
	    	    buf[x + y * inputXDim] = edgeFx[y][x];	
	    	}
	    }
	    try {
	    	medianImage.importData(0, buf, true);
	    }
	    catch (IOException e) {
	    	MipavUtil.displayError("IOException " + e + " on medianImage.importData(0, buf, true)");
	    	setCompleted(false);
	    	return;
	    }
	    medianAlgo = new AlgorithmMedian(medianImage, iters, ksize, kernelShape, stdDev, filterType, maximumSize, entireImage);
	    medianAlgo.run();
	    medianAlgo.finalize();
	    medianAlgo = null;
	    try {
	    	medianImage.exportData(0, inputXDim * inputYDim, buf);
	    }
	    catch (IOException e) {
	    	MipavUtil.displayError("IOException " + e + " on medianImage.exportData(0, inputXDim * inputYDim, buf)");
	    	setCompleted(false);
	    	return;	
	    }
	    for (y = 0; y < inputYDim; y++) {
	    	for (x = 0; x < inputXDim; x++) {
	    	    buf2[x + y * inputXDim] = edgeFy[y][x];
	    	}
	    }
	    try {
	    	medianImage.importData(0, buf2, true);
	    }
	    catch (IOException e) {
	    	MipavUtil.displayError("IOException " + e + " on medianImage.importData(0, buf2, true)");
	    	setCompleted(false);
	    	return;
	    }
	    medianAlgo = new AlgorithmMedian(medianImage, iters, ksize, kernelShape, stdDev, filterType, maximumSize, entireImage);
	    medianAlgo.run();
	    medianAlgo.finalize();
	    medianAlgo = null;
	    try {
	    	medianImage.exportData(0, inputXDim * inputYDim, buf2);
	    }
	    catch (IOException e) {
	    	MipavUtil.displayError("IOException " + e + " on medianImage.exportData(0, inputXDim * inputYDim, buf2)");
	    	setCompleted(false);
	    	return;	
	    }
	    medianImage.disposeLocal();
	    medianImage = null;
	    freqEdge = new double[inputXDim * inputYDim];
	    for (x = 0; x < inputXDim * inputYDim; x++) {
	    	freqEdge[x] = Math.sqrt(buf[x]*buf[x] + buf2[x]*buf2[x]);
	    }
	    
	    try {
	    	destImage[0].importData(0, synthText, true);
	    }
	    catch (IOException e) {
	    	MipavUtil.displayError("IOException " + e + " on destImage[0].importData(0, synthText, true)");
	    	setCompleted(false);
	    	return;
	    }
	    
	    for (y = 0; y < inputYDim; y++) {
	    	for (x = 0; x < inputXDim; x++) {
	    	    buf[x + y * inputXDim] = textA[y][x];	
	    	}
	    }
	    try {
	    	destImage[1].importData(0, buf, true);
	    }
	    catch (IOException e) {
	    	MipavUtil.displayError("IOException " + e + " on destImage[1].importData(0, buf, true)");
	    	setCompleted(false);
	    	return;
	    }
	    
	    try {
	    	destImage[2].importData(0, freqText, true);
	    }
	    catch (IOException e) {
	    	MipavUtil.displayError("IOException " + e + " on destImage[2].importData(0, freqText, true)");
	    	setCompleted(false);
	    	return;
	    }
	    
	    try {
	    	destImage[3].importData(0, synthEdge, true);
	    }
	    catch (IOException e) {
	    	MipavUtil.displayError("IOException " + e + " on destImage[3].importData(0, synthEdge, true)");
	    	setCompleted(false);
	    	return;
	    }
	    
	    for (y = 0; y < inputYDim; y++) {
	    	for (x = 0; x < inputXDim; x++) {
	    	    buf[x + y * inputXDim] = edgeA[y][x];	
	    	}
	    }
	    try {
	    	destImage[4].importData(0, buf, true);
	    }
	    catch (IOException e) {
	    	MipavUtil.displayError("IOException " + e + " on destImage[4].importData(0, buf, true)");
	    	setCompleted(false);
	    	return;
	    }
	    
	    try {
	    	destImage[5].importData(0, freqEdge, true);
	    }
	    catch (IOException e) {
	    	MipavUtil.displayError("IOException " + e + " on destImage[5].importData(0, freqEdge, true)");
	    	setCompleted(false);
	    	return;
	    }
	    

	    try {
	    	destImage[6].importData(0, ptx, true);
	    }
	    catch (IOException e) {
	    	MipavUtil.displayError("IOException " + e + " on destImage[6].importData(0, ptx, true)");
	    	setCompleted(false);
	    	return;
	    }
	    
	    try {
	    	destImage[7].importData(0, ped, true);
	    }
	    catch (IOException e) {
	    	MipavUtil.displayError("IOException " + e + " on destImage[7].importData(0, ped, true)");
	    	setCompleted(false);
	    	return;
	    }
	    
	    try {
	    	destImage[8].importData(0, psm, true);
	    }
	    catch (IOException e) {
	    	MipavUtil.displayError("IOException " + e + " on destImage[8].importData(0, psm, true)");
	    	setCompleted(false);
	    	return;
	    }
		
		setCompleted(true);
		
		return;

	}

	private void T2z1_filter(double A[][], double ph[][], double Fx[][], double Fy[][],
			int idx[][], double en[][], double critDCA[][],
			double omegas[][], double amplitudes[][],
			double filterAngle[], double sigmaX[],
			double td1[][][], double td2[][][], double td3[][][],
			double fd1[][][], double fd1Imag[][][], double fd2[][][],
			double fd2Imag[][][], double fd3[][][], double fd3Imag[][][],
			double fd22[][][], double fd22Imag[][][], double fd23[][][],
			double fd23Imag[][][], double fd33[][][], double fd33Imag[][][],
			int ps[], String domain[], int scaleswt[], int ndirs,
			ModelImage inputImage, double invDes[][][][][], String DCAmethod,
			String esameth, double sgx[], double invVariance2,
			double factorSharpness, double factorMdl) {
		// fields that are being accumulated
		// A: amplitude, ph: phase, idx: filter index
		// en: model-based decrease in reconstruction error (sum of squares)
		// Fx, Fy; frequency components
		int xDim = inputImage.getExtents()[0];
		int yDim = inputImage.getExtents()[1];
		int length = xDim * yDim;
		double Anew[][] = new double[yDim][xDim];
		double phnew[][] = new double[yDim][xDim];
		double Fxnew[][] = new double[yDim][xDim];
		double Fynew[][] = new double[yDim][xDim];
		int idxnew;
		double ennew[][] = new double[yDim][xDim];
		double critDCAnew[][] = new double[yDim][xDim];
		double inputImageFFT[] = new double[length];
		double inputImageFFTImag[] = new double[length];
		double inputImFFT[][] = new double[yDim][xDim];
		double inputImFFTImag[][] = new double[yDim][xDim];
		double Am[][] = new double[yDim][xDim];
		double Wx[][] = new double[yDim][xDim];
		double Wy[][] = new double[yDim][xDim];
		double ener3DBasis[][] = new double[yDim][xDim];
		double endc[][] = new double[yDim][xDim];
		double ratio[][] = new double[yDim][xDim];
		double Araw[][] = new double[yDim][xDim];
		double TeagEn[][] = new double[yDim][xDim];
		double enerAmpl[][] = new double[yDim][xDim];
		double enerTeag[][] = new double[yDim][xDim];
		double critMDL[][] = new double[yDim][xDim];
		double preComputedIc1[][] = new double[yDim][xDim];
		double preComputedIc2[][] = new double[yDim][xDim];
		double preComputedIc3[][] = new double[yDim][xDim];
		double preComputedSc1[][] = new double[yDim][xDim];
		double preComputedsm[][] = new double[yDim][xDim];
		double Fn[][][] = new double[3][yDim][xDim];
		double fl[][][] = new double[3][yDim][xDim];
		boolean larger;
		FFTUtility fft;
		int k;
		int i;
		int scaleInd;
		int filStart;
		double inputIm[][] = new double[yDim][xDim];
		int y;
		int x;
		int dirInd;
		int filInd;
		int modnd;
		int sgn;
		double minresp;
		double Teag1[][];
		double Teag2[][];
		double sg;
		double mdlCostScale;
		int patchSize;
		int padXDim;
		int padYDim;
		
		for (y = 0; y < yDim; y++) {
	        for (x = 0; x < xDim; x++) {
	        	critDCA[y][x] = -1.0E11;
	        }
		}
		
		try {
			inputImage.exportData(0, length, inputImageFFT);
		} catch (IOException e) {
			MipavUtil.displayError("IOException " + e
					+ " on inputImage.exportData(0, length, inputImageFFT");
			return;
		}

		for (y = 0; y < yDim; y++) {
			for (x = 0; x < xDim; x++) {
				inputIm[y][x] = inputImageFFT[x + y * xDim];
			}
		}

		fft = new FFTUtility(inputImageFFT, inputImageFFTImag, yDim, xDim, 1,
				-1, FFTUtility.FFT);
		fft.setShowProgress(false);
		fft.run();
		fft.finalize();
		fft = null;
		fft = new FFTUtility(inputImageFFT, inputImageFFTImag, 1, yDim, xDim,
				-1, FFTUtility.FFT);
		fft.setShowProgress(false);
		fft.run();
		fft.finalize();
		fft = null;

		for (y = 0; y < yDim; y++) {
			for (x = 0; x < xDim; x++) {
				inputImFFT[y][x] = inputImageFFT[x + y * xDim];
				inputImFFTImag[y][x] = inputImageFFTImag[x + y * xDim];
			}
		}

		for (k = 0; k < scaleswt.length; k++) {
			scaleInd = scaleswt[k];
			filStart = scaleInd * ndirs;
			patchSize = ps[filStart];
			padXDim = inputIm[0].length + 2 * patchSize;
			padYDim = inputIm.length + 2 * patchSize;
			double fftImagePatch[] = new double[padXDim * padYDim];
			double fftImagePatchImag[] = new double[padXDim * padYDim];
			double fftSupportPatch[] = new double[padXDim * padYDim];
			double fftSupportPatchImag[] = new double[padXDim * padYDim];
			// Once for every scale, construct dc model for background
			if (domain[filStart].equals("time")) {
				T2z1b_get_responses_time(preComputedIc1, null, null,
						preComputedSc1, preComputedsm, null, null, null, null,
						null, null, td1[filStart], null, null, null, null,
						null, inputIm, 1);
			} // if (domain.equals("time"))
			else if (domain[filStart].equals("freq")) {
				
				T2z1a_make_image_structure(fftImagePatch, fftImagePatchImag,
						fftSupportPatch, fftSupportPatchImag, inputIm,
						ps[filStart]);
				T2z1b_get_responses_freq(preComputedIc1, null, null,
						preComputedSc1, preComputedsm, null, null, null, null,
						null, null, fftSupportPatch, fftSupportPatchImag,
						ps[filStart], fftImagePatch, fftImagePatchImag,
						fd1[filStart], fd1Imag[filStart], fd2[filStart],
						fd2Imag[filStart], fd3[filStart], fd3Imag[filStart],
						fd22[filStart], fd22Imag[filStart], fd23[filStart],
						fd23Imag[filStart], fd33[filStart], fd33Imag[filStart],
						1);
			} // else if (domain.equals("freq"))
			for (y = 0; y < yDim; y++) {
				for (x = 0; x < xDim; x++) {
					endc[y][x] = preComputedIc1[y][x] * preComputedsm[y][x];
				}
			}

			// And now loop over orientations
			for (dirInd = 0; dirInd < ndirs; dirInd++) {
				filInd = filStart + dirInd;

				// Construct fields involved in projection
				if (domain[filStart].equals("time")) {
					T2z1b_get_responses_time(null, preComputedIc2,
							preComputedIc3, null, null, null, null, null, null,
							null, null, null, td2[filInd], td3[filInd], null,
							null, null, inputIm, 2);
				} else if (domain[filStart].equals("freq")) {
					T2z1b_get_responses_freq(null, preComputedIc2,
							preComputedIc3, null, null, null, null, null, null,
							null, null, null, null, ps[filInd], fftImagePatch,
							fftImagePatchImag, null, null, fd2[filInd],
							fd2Imag[filInd], fd3[filInd], fd3Imag[filInd],
							null, null, null, null, null, null, 2);
				}

				// Estimate projection onto basis elements
				for (i = 0; i < 3; i++) {
					for (y = 0; y < yDim; y++) {
						for (x = 0; x < xDim; x++) {
							fl[0][y][x] = preComputedIc1[y][x];
							fl[1][y][x] = preComputedIc2[y][x];
							fl[2][y][x] = preComputedIc3[y][x];
							Fn[i][y][x] = (invDes[filInd][i][0][y][x]
									* preComputedIc1[y][x]
									+ invDes[filInd][i][1][y][x]
									* preComputedIc2[y][x] + invDes[filInd][i][2][y][x]
									* preComputedIc3[y][x]);
						}
					}
				} // for (i = 0; i < 3; i++)

				// ESA demodulation (Gabor only! - for edges: esa_meth = '')
				// Recover frequency vector and use to compensate for amplitude
				// losses
				// due to front-end filtering

				// Decide whether to flip frequency sign based on filter index
				modnd = (filInd % ndirs) + 1;
				if (modnd > ndirs / 2) {
					sgn = -1;
				} else {
					sgn = 1;
				}

				// minresp: determines the maximum scaling of responses (=
				// 1/minresp)
				// The frequency domain is 'tesselated' by Gabor filters
				// (see visualization in 1.53-64 of T00_batch_script)
				// The minimum value on the 'borders' of the compartments is
				// the minimum resp considered

				minresp = 0.35;
				if (esameth.equals("gesa")) {
					// gabor esa
					
					T2z1d_esa2D(Am, Wx, Wy, ratio, Araw, TeagEn,
							 inputImFFT, inputImFFTImag, sgn,
							omegas[filInd], amplitudes[filInd],
							filterAngle[filInd],
							sigmaX[filInd], minresp, 0);
				} else if (esameth.equals("cesa")) {
					// complex esa
					T2z1d_esa2D(Am, Wx, Wy, ratio, Araw, TeagEn,
							Fn[1], Fn[2], sgn, omegas[filInd],
							amplitudes[filInd],
							filterAngle[filInd], sigmaX[filInd], minresp, 1);
				} else if (esameth.equals("aesa")) {
					// plain esa
					T2z1d_esa2D(Am, Wx, Wy, ratio, Araw, TeagEn,
							Fn[1], null, sgn, omegas[filInd],
							amplitudes[filInd],
							filterAngle[filInd], sigmaX[filInd], minresp, 1);
				} else if (esameth.equals("")) {
					Wx = deriv_x_right(ph);
					Wy = deriv_y_up(ph);
					for (y = 0; y < yDim; y++) {
						for (x = 0; x < xDim; x++) {
							ratio[y][x] = 1.0;
						}
					}
					for (y = 0; y < yDim; y++) {
						for (x = 0; x < xDim; x++) {
							Araw[y][x] = Math.sqrt(Fn[2][y][x] * Fn[2][y][x]
									+ Fn[1][y][x] * Fn[1][y][x]);
							Am[y][x] = Araw[y][x];
						}
					}
					Teag1 = enop2D(Fn[1]);
					Teag2 = enop2D(Fn[2]);
					for (y = 0; y < yDim; y++) {
						for (x = 0; x < xDim; x++) {
							TeagEn[y][x] = Teag1[y][x] + Teag2[y][x];
						}
					}
				} // else if (esameth.equals(""))
				for (y = 0; y < yDim; y++) {
					for (x = 0; x < xDim; x++) {
						Fn[1][y][x] = Fn[1][y][x] / ratio[y][x];
						Fn[2][y][x] = Fn[2][y][x] / ratio[y][x];
						fl[1][y][x] = fl[1][y][x] / ratio[y][x];
						fl[2][y][x] = fl[2][y][x] / ratio[y][x];
					}
				}

				// The features extracted by DCA
				for (y = 0; y < yDim; y++) {
					for (x = 0; x < xDim; x++) {
						ener3DBasis[y][x] = fl[0][y][x] * Fn[0][y][x]
								+ fl[1][y][x] * Fn[1][y][x] + fl[2][y][x]
								* Fn[2][y][x];
					}
				}
				for (y = 0; y < yDim; y++) {
					for (x = 0; x < xDim; x++) {
						ennew[y][x] = ener3DBasis[y][x] - endc[y][x];
					}
				}
				for (y = 0; y < yDim; y++) {
					for (x = 0; x < xDim; x++) {
						Anew[y][x] = Am[y][x];
					}
				}
				for (y = 0; y < yDim; y++) {
					for (x = 0; x < xDim; x++) {
						phnew[y][x] = Math.atan2(Fn[2][y][x], Fn[1][y][x]);
					}
				}
				idxnew = filInd;
				for (y = 0; y < yDim; y++) {
					for (x = 0; x < xDim; x++) {
						Fxnew[y][x] = Wx[y][x];
					}
				}
				for (y = 0; y < yDim; y++) {
					for (x = 0; x < xDim; x++) {
						Fynew[y][x] = Wy[y][x];
					}
				}

				// Different possible terms used for channel selection
				for (y = 0; y < yDim; y++) {
					for (x = 0; x < xDim; x++) {
						enerAmpl[y][x] = Anew[y][x];
					}
				}
				for (y = 0; y < yDim; y++) {
					for (x = 0; x < xDim; x++) {
						enerTeag[y][x] = TeagEn[y][x];
					}
				}

				sg = sgx[filInd];
				mdlCostScale = -Math.log(sg) / sg;
				for (y = 0; y < yDim; y++) {
					for (x = 0; x < xDim; x++) {
						critMDL[y][x] = factorSharpness
								* (factorMdl * mdlCostScale + ennew[y][x]
										* invVariance2);
					}
				}
				if (DCAmethod.equals("ampl")) {
					for (y = 0; y < yDim; y++) {
						for (x = 0; x < xDim; x++) {
							critDCAnew[y][x] = enerAmpl[y][x];
						}
					}
				} else if (DCAmethod.equals("teag")) {
					for (y = 0; y < yDim; y++) {
						for (x = 0; x < xDim; x++) {
							critDCAnew[y][x] = enerTeag[y][x];
						}
					}
				} else if (DCAmethod.equals("mdl")) {
					for (y = 0; y < yDim; y++) {
						for (x = 0; x < xDim; x++) {
							critDCAnew[y][x] = critMDL[y][x];
						}
					}
				}

				// Update descriptor fields for each pixel where the
				// current selection criterion is larger than the previous
				for (y = 0; y < yDim; y++) {
					for (x = 0; x < xDim; x++) {
						larger = critDCAnew[y][x] > critDCA[y][x];
						if (larger) {
							A[y][x] = Anew[y][x];
							ph[y][x] = phnew[y][x];
							Fx[y][x] = Fxnew[y][x];
							Fy[y][x] = Fynew[y][x];
							idx[y][x] = idxnew;
							en[y][x] = ennew[y][x];
							critDCA[y][x] = critDCAnew[y][x];
						}
					}
				}
			} // for (dirInd = 0; dirInd < ndirs; dirInd++)
		} // for (k = 0; k < scaleswt.length; k++)
        
	}

	

	private void T2z1d_esa2D(double A[][], double Wx[][], double Wy[][],
			double freqr[][], double Anonorm[][],
			double Eo[][],
			double imInSrc[][], double imInSrcImag[][],
			int signChange, double omegas[], double amplitudes[],
			double filterAngle, double sigmaX,
			double ct, int diffType) {
		int yDim = imInSrc.length;
		int xDim = imInSrc[0].length;
        double Fx[][] = new double[yDim][xDim];
        double FxImag[][] = new double[yDim][xDim];
        double Fy[][] = new double[yDim][xDim];
        double FyImag[][] = new double[yDim][xDim];
		double Fxx[][] = new double[yDim][xDim];
		double FxxImag[][] = new double[yDim][xDim];
		double Fyy[][] = new double[yDim][xDim];
		double FyyImag[][] = new double[yDim][xDim];
		double Fxy[][] = new double[yDim][xDim];
		double FxyImag[][] = new double[yDim][xDim];
		double V2Fx[][] = new double[yDim][xDim];
		double V2FxImag[][] = new double[yDim][xDim];
		double V2Fy[][] = new double[yDim][xDim];
		double V2FyImag[][] = new double[yDim][xDim];
		double Eox[][] = new double[yDim][xDim];
		double Eoy[][] = new double[yDim][xDim];
		double im[][];
		double Ix[][] = null;
		double Iy[][] = null;
		double Ixx[][] = null;
		double Iyy[][] = null;
		double Ixy[][] = null;
		double EoxTerm[][] = null;
		double EoyTerm[][] = null;
		double Eon[][];
		double Eoxn[][];
		double Eoyn[][];
		double denom[][];
		double omegax[][];
		double omegay[][];
		double maxDenom;
		int ch;
		int y;
		int x;
		int nc;
		
		double imIn[][] = new double[imInSrc.length][imInSrc[0].length];
		double imInImag[][] = null;
		if (imInSrcImag != null) {
		    imInImag = new double[imInSrc.length][imInSrc[0].length];
		}
		for (y = 0; y < imInSrc.length; y++) {
			for (x = 0; x < imInSrc[0].length; x++) {
				imIn[y][x] = imInSrc[y][x];
				if (imInImag != null) {
				    imInImag[y][x] = imInSrcImag[y][x];
				}
			}
		}
		for (y = 0; y < yDim; y++) {
			for (x = 0; x < xDim; x++) {
				Eo[y][x] = 0.0;
			}
		}
		if (imInImag == null) {
			nc = 1;
		} else {
			nc = 2;
		}
		if (diffType == 0) {
			T2z1dI_get_responses_freq_gabor(Fx, FxImag, Fy, FyImag,
					Fxx, FxxImag, Fyy, FyyImag, Fxy, FxyImag, V2Fx, V2FxImag,
					V2Fy, V2FyImag, imIn, imInImag,
					omegas, amplitudes,
					filterAngle, sigmaX);
		} // if (diffType == 0)
		for (ch = 0; ch < nc; ch++) {
			if (ch == 0) {
				im = imIn;
			} else {
				im = imInImag;
			}
			switch (diffType) {
			case 0:
				if (ch == 0) {
					Ix = Fx;
					Iy = Fy;
					Ixx = Fxx;
					Iyy = Fyy;
					Ixy = Fxy;
					EoxTerm = V2Fx;
					EoyTerm = V2Fy;
				} // if (ch == 0)
				else {
					Ix = FxImag;
					Iy = FyImag;
					Ixx = FxxImag;
					Iyy = FyyImag;
					Ixy = FxyImag;
					EoxTerm = V2FxImag;
					EoyTerm = V2FyImag;
				} // else
				break;
			case 1:
				Ix = deriv_x_right(im);
				Iy = deriv_y_up(im);
				break;
			case 2:
				Ix = deriv_x_left(im);
				Iy = deriv_y_down(im);
			} // switch (diffType)

			if (diffType == 0) {
				Eon = new double[yDim][xDim];
				Eoxn = new double[yDim][xDim];
				Eoyn = new double[yDim][xDim];
				for (y = 0; y < yDim; y++) {
					for (x = 0; x < xDim; x++) {
						Eon[y][x] = Ix[y][x] * Ix[y][x] + Iy[y][x] * Iy[y][x]
								- im[y][x] * (Ixx[y][x] + Iyy[y][x]);
						Eoxn[y][x] = Ixx[y][x] * Ixx[y][x] + Ixy[y][x]
								* Ixy[y][x] - Ix[y][x] * EoxTerm[y][x];
						Eoyn[y][x] = Ixy[y][x] * Ixy[y][x] + Iyy[y][x]
								* Iyy[y][x] - Iy[y][x] * EoyTerm[y][x];
					}
				}
			} // if (diffType == 0)
			else {
				Eon = enop2D(im);
				Eoxn = enop2D(Ix);
				Eoyn = enop2D(Iy);
			}
			for (y = 0; y < yDim; y++) {
				for (x = 0; x < xDim; x++) {
					Eo[y][x] += Eon[y][x] / 2.0;
					Eox[y][x] += Eoxn[y][x] / 2.0;
					Eoy[y][x] += Eoyn[y][x] / 2.0;
				}
			}
		} // for (ch = 0; ch < nc; ch++)

		if (nc > 1) {
			// gabor-based amplitude estimate (no division)
			for (y = 0; y < yDim; y++) {
				for (x = 0; x < xDim; x++) {
					A[y][x] = Math.sqrt(imIn[y][x] * imIn[y][x]
							+ imInImag[y][x] * imInImag[y][x]);
				}
			}
		} // if (nc > 1)
		else {
			// esa-based amplitude estimate (needs some treatment of 0s in
			// divisor)
			denom = new double[yDim][xDim];
			maxDenom = 0.0;
			
			for (y = 0; y < yDim; y++) {
				for (x = 0; x < xDim; x++) {
					denom[y][x] = Math.sqrt(Math
							.max(Eox[y][x] + Eoy[y][x], 0.0));
					if (denom[y][x] > maxDenom) {
						maxDenom = denom[y][x];
					}
				}
			}
			maxDenom = Math.max(maxDenom, 1.0E-3);
			for (y = 0; y < yDim; y++) {
				for (x = 0; x < xDim; x++) {
					A[y][x] = Eo[y][x]
							/ Math.max(denom[y][x], 1.0E-2 * maxDenom);
				}
			}
		} // else

		for (y = 0; y < yDim; y++) {
			for (x = 0; x < xDim; x++) {
				if (Eo[y][x] <= 0.0) {
					Wx[y][x] = 0.0;
					Wy[y][x] = 0.0;
				}
				else {
					Wx[y][x] = Math.sqrt(Math.max(Eox[y][x] / Eo[y][x], 0.0));
					Wy[y][x] = Math.sqrt(Math.max(Eoy[y][x] / Eo[y][x], 0.0));
				}
			}
		}

		if (signChange == -1) {
			for (y = 0; y < yDim; y++) {
				for (x = 0; x < xDim; x++) {
					Wx[y][x] = -Wx[y][x];
				}
			}
		}

		for (y = 0; y < yDim; y++) {
			for (x = 0; x < xDim; x++) {
				Anonorm[y][x] = A[y][x];
			}
		}

		omegax = new double[Wx.length][Wx[0].length];
		omegay = new double[Wy.length][Wy[0].length];
		for (y = 0; y < Wx.length; y++) {
			for (x = 0; x < Wx[0].length; x++) {
				omegax[y][x] = Wx[y][x] / Math.PI;
			}
		}

		for (y = 0; y < Wy.length; y++) {
			for (x = 0; x < Wy[0].length; x++) {
				omegay[y][x] = Wy[y][x] / Math.PI;
			}
		}

		Tzz_freq_resp(freqr, omegax, omegay, sigmaX, filterAngle,
				omegas, amplitudes);
		for (y = 0; y < freqr.length; y++) {
			for (x = 0; x < freqr[0].length; x++) {
				freqr[y][x] = 2.0 * Math.max(freqr[y][x], ct);
			}
		}

		for (y = 0; y < A.length; y++) {
			for (x = 0; x < A[0].length; x++) {
				A[y][x] = A[y][x] / freqr[y][x];
			}
		}
	}

	private double[][] enop2D(double im[][]) {
		int yDim = im.length;
		int xDim = im[0].length;
		double Ix1[][] = new double[yDim][xDim];
		double Ix_1[][] = new double[yDim][xDim];
		double Iy1[][] = new double[yDim][xDim];
		double Iy_1[][] = new double[yDim][xDim];
		double energy[][] = new double[yDim][xDim];
		int y;
		int x;

		for (y = 0; y < yDim; y++) {
			for (x = 0; x < xDim - 1; x++) {
				Ix1[y][x] = im[y][x + 1];
			}
			Ix1[y][xDim - 1] = im[y][xDim - 1];
		}

		for (y = 0; y < yDim; y++) {
			Ix_1[y][0] = im[y][0];
			for (x = 1; x < xDim; x++) {
				Ix_1[y][x] = im[y][x - 1];
			}
		}

		for (x = 0; x < xDim; x++) {
			for (y = 0; y < yDim - 1; y++) {
				Iy1[y][x] = im[y + 1][x];
			}
			Iy1[yDim - 1][x] = im[yDim - 1][x];
		}

		for (x = 0; x < xDim; x++) {
			Iy_1[0][x] = im[0][x];
			for (y = 1; y < yDim; y++) {
				Iy_1[y][x] = im[y - 1][x];
			}
		}

		for (y = 0; y < yDim; y++) {
			for (x = 0; x < xDim; x++) {
				energy[y][x] = 2.0 * im[y][x] * im[y][x] - Ix1[y][x]
						* Ix_1[y][x] - Iy1[y][x] * Iy_1[y][x];
			}
		}

		return energy;
	}

	private double[][] deriv_x_right(double buf[][]) {
		int yDim = buf.length;
		int xDim = buf[0].length;
		double result[][] = new double[yDim][xDim];
		int y;
		int x;
		for (y = 0; y < yDim; y++) {
			for (x = 0; x < xDim - 1; x++) {
				result[y][x] = buf[y][x + 1] - buf[y][x];
			}
		}
		return result;
	}

	private double[][] deriv_x_left(double buf[][]) {
		int yDim = buf.length;
		int xDim = buf[0].length;
		double result[][] = new double[yDim][xDim];
		int y;
		int x;
		for (y = 0; y < yDim; y++) {
			for (x = 1; x < xDim; x++) {
				result[y][x] = buf[y][x] - buf[y][x - 1];
			}
		}
		return result;
	}

	private double[][] deriv_y_up(double buf[][]) {
		int yDim = buf.length;
		int xDim = buf[0].length;
		double result[][] = new double[yDim][xDim];
		int y;
		int x;
		for (y = 1; y < yDim; y++) {
			for (x = 0; x < xDim; x++) {
				result[y][x] = buf[y][x] - buf[y - 1][x];
			}
		}
		return result;
	}

	private double[][] deriv_y_down(double buf[][]) {
		int yDim = buf.length;
		int xDim = buf[0].length;
		double result[][] = new double[yDim][xDim];
		int y;
		int x;
		for (y = 0; y < yDim - 1; y++) {
			for (x = 0; x < xDim; x++) {
				result[y][x] = buf[y + 1][x] - buf[y][x];
			}
		}
		return result;
	}

	private void T2z1dI_get_responses_freq_gabor(double Fx[][], double FxImag[][], double Fy[][],
			double FyImag[][], double Fxx[][], double FxxImag[][],
			double Fyy[][], double FyyImag[][], double Fxy[][],
			double FxyImag[][], double V2Fx[][], double V2FxImag[][],
			double V2Fy[][], double V2FyImag[][], double imIn[][],
			double imInImag[][],
			double omegas[], double amplitudes[],
			double filterAngle, double sigmaX) {
		int sizem = imIn.length;
		int sizen = imIn[0].length;
		double F[][] = new double[sizem][sizen];
		double FImag[][] = new double[sizem][sizen];
		double fm[] = new double[sizem];
		double fn[] = new double[sizen];
		int i;
		double omegasn[][];
		double omegasm[][];
		int y;
		int x;
		double dirc[][] = null;

		double omegasn2[][] = new double[sizem][sizen];
		double omegasm2[][] = new double[sizem][sizen];
		double nmterm;
		double sumterm;
		double prodterm;
		if ((sizem % 2) == 1) {
			for (i = 0; i < sizem; i++) {
				fm[i] = -1.0 + 1.0 / sizem + (2.0 * i) / sizem;
			}
		} else {
			for (i = 0; i < sizem; i++) {
				fm[i] = -1.0 + (2.0 * i) / sizem;
			}
		}
		if ((sizen % 2) == 1) {
			for (i = 0; i < sizen; i++) {
				fn[i] = -1.0 + 1.0 / sizen + (2.0 * i) / sizen;
			}
		} else {
			for (i = 0; i < sizen; i++) {
				fn[i] = -1.0 + (2.0 * i) / sizen;
			}
		}
		omegasn = new double[sizem][sizen];
		omegasm = new double[sizem][sizen];
		dirc = new double[sizem][sizen];
		for (y = 0; y < sizem; y++) {
			for (x = 0; x < sizen; x++) {
				omegasn[y][x] = fn[x];
			}
		}
		for (x = 0; x < sizen; x++) {
			for (y = 0; y < sizem; y++) {
				omegasm[y][x] = fm[y];
			}
		}

		ifftshift(omegasn);
		ifftshift(omegasm);

		Tzz_freq_resp(dirc, omegasn, omegasm, sigmaX, filterAngle,
				omegas, amplitudes);
		for (y = 0; y < sizem; y++) {
			for (x = 0; x < sizen; x++) {
				omegasn[y][x] = Math.PI * omegasn[y][x];
				omegasm[y][x] = Math.PI * omegasm[y][x];
			}
		}
		for (y = 0; y < dirc.length; y++) {
			for (x = 0; x < dirc[0].length; x++) {
				dirc[y][x] *= 4.0;
			}
		}

		dirc[0][0] = 0.0;
		for (y = 0; y < sizem; y++) {
			for (x = 0; x < sizen; x++) {
				F[y][x] = dirc[y][x] * imIn[y][x];
				FImag[y][x] = dirc[y][x] * imInImag[y][x];
			}
		}
		for (y = 0; y < sizem; y++) {
			for (x = 0; x < sizen; x++) {
				Fx[y][x] = -omegasn[y][x] * FImag[y][x];
				FxImag[y][x] = omegasn[y][x] * F[y][x];
			}
		}
		ifft2(Fx, FxImag);

		for (y = 0; y < sizem; y++) {
			for (x = 0; x < sizen; x++) {
				Fy[y][x] = -omegasm[y][x] * FImag[y][x];
				FyImag[y][x] = omegasm[y][x] * F[y][x];
			}
		}
		ifft2(Fy, FyImag);

		for (y = 0; y < sizem; y++) {
			for (x = 0; x < sizen; x++) {
				omegasn2[y][x] = omegasn[y][x] * omegasn[y][x];
				Fxx[y][x] = -omegasn2[y][x] * F[y][x];
				FxxImag[y][x] = -omegasn2[y][x] * FImag[y][x];
				omegasm2[y][x] = omegasm[y][x] * omegasm[y][x];
				Fyy[y][x] = -omegasm2[y][x] * F[y][x];
				FyyImag[y][x] = -omegasm2[y][x] * FImag[y][x];
			}
		}
		ifft2(Fxx, FxxImag);
		ifft2(Fyy, FyyImag);

		for (y = 0; y < sizem; y++) {
			for (x = 0; x < sizen; x++) {
				nmterm = omegasn[y][x] * omegasm[y][x];
				Fxy[y][x] = -nmterm * F[y][x];
				FxyImag[y][x] = -nmterm * FImag[y][x];
			}
		}
		ifft2(Fxy, FxyImag);

		for (y = 0; y < sizem; y++) {
			for (x = 0; x < sizen; x++) {
				sumterm = omegasn2[y][x] + omegasm2[y][x];
				prodterm = omegasn[y][x] * sumterm;
				V2Fx[y][x] = prodterm * FImag[y][x];
				V2FxImag[y][x] = -prodterm * F[y][x];
				prodterm = omegasm[y][x] * sumterm;
				V2Fy[y][x] = prodterm * FImag[y][x];
				V2FyImag[y][x] = -prodterm * F[y][x];
			}
		}
		ifft2(V2Fx, V2FxImag);
		ifft2(V2Fy, V2FyImag);
		
		ifft2(F, FImag);

		for (y = 0; y < sizem; y++) {
			for (x = 0; x < sizen; x++) {
				imIn[y][x] = F[y][x];
				imInImag[y][x] = FImag[y][x];
			}
		}
	}

	private void ifft2(double buf[][], double bufI[][]) {
		int yDim = buf.length;
		int xDim = buf[0].length;
		int length = xDim * yDim;
		double buffer[] = new double[length];
		double bufferImag[] = new double[length];
		int y;
		int x;
		FFTUtility fft;
		for (y = 0; y < yDim; y++) {
			for (x = 0; x < xDim; x++) {
				buffer[x + y * xDim] = buf[y][x];
				bufferImag[x + y * xDim] = bufI[y][x];
			}
		}
		fft = new FFTUtility(buffer, bufferImag, yDim, xDim, 1, 1,
				FFTUtility.FFT);
		fft.setShowProgress(false);
		fft.run();
		fft.finalize();
		fft = null;
		fft = new FFTUtility(buffer, bufferImag, 1, yDim, xDim, 1,
				FFTUtility.FFT);
		fft.setShowProgress(false);
		fft.run();
		fft.finalize();
		fft = null;
		for (y = 0; y < yDim; y++) {
			for (x = 0; x < xDim; x++) {
				buf[y][x] = buffer[x + y * xDim];
				bufI[y][x] = bufferImag[x + y * xDim];
			}
		}
		buffer = null;
		bufferImag = null;
		return;
	}

	private void T2z0_projection_terms(double invDes[][][][][],
			double td1[][][], double td2[][][], double td3[][][],
			double td22[][][], double td23[][][], double td33[][][],
			double fd1[][][], double fd1Imag[][][], double fd2[][][],
			double fd2Imag[][][], double fd3[][][], double fd3Imag[][][],
			double fd22[][][], double fd22Imag[][][], double fd23[][][],
			double fd23Imag[][][], double fd33[][][], double fd33Imag[][][],
			int ps[], int nscales, int ndirs, String filtDomain[],
			ModelImage inputImage) {
		// Precompute terms required for weighted projection on basis
		int sizen0 = inputImage.getExtents()[0];
		int sizem0 = inputImage.getExtents()[1];
		double domain[][] = new double[sizem0][sizen0];
		double Sc1[][] = new double[sizem0][sizen0];
		double Sc2[][] = new double[sizem0][sizen0];
		double Sc3[][] = new double[sizem0][sizen0];
		double Sc22[][] = new double[sizem0][sizen0];
		double Sc23[][] = new double[sizem0][sizen0];
		double Sc33[][] = new double[sizem0][sizen0];
		int patchSize;
		int y;
		int x;
		int sc;
		int offsetsc;
		int dirInd;
		int filInd;
		int padXDim;
		int padYDim;
		for (y = 0; y < sizem0; y++) {
			for (x = 0; x < sizen0; x++) {
				domain[y][x] = 1.0;
			}
		}

		for (sc = 1; sc <= nscales; sc++) {
			offsetsc = (sc - 1) * ndirs;
			patchSize = ps[offsetsc];
			padXDim = sizen0 + 2 * patchSize;
			padYDim = sizem0 + 2 * patchSize;
			double fftImagePatch[] = new double[padXDim * padYDim];
			double fftImagePatchImag[] = new double[padXDim * padYDim];
			double fftSupportPatch[] = new double[padXDim * padYDim];
			double fftSupportPatchImag[] = new double[padXDim * padYDim];
			if (filtDomain[offsetsc].equals("freq")) {
				T2z1a_make_image_structure(fftImagePatch, fftImagePatchImag,
						fftSupportPatch, fftSupportPatchImag, domain, patchSize);
			}

			for (dirInd = 1; dirInd <= ndirs; dirInd++) {
				filInd = offsetsc + dirInd;
				if (filtDomain[offsetsc].equals("freq")) {
					T2z1b_get_responses_freq(null, null, null, null, null, Sc1,
							Sc2, Sc3, Sc22, Sc23, Sc33, fftSupportPatch,
							fftSupportPatchImag, patchSize, null, null,
							fd1[filInd - 1], fd1Imag[filInd - 1],
							fd2[filInd - 1], fd2Imag[filInd - 1],
							fd3[filInd - 1], fd3Imag[filInd - 1],
							fd22[filInd - 1], fd22Imag[filInd - 1],
							fd23[filInd - 1], fd23Imag[filInd - 1],
							fd33[filInd - 1], fd33Imag[filInd - 1], 0);
				} // if (filtDomain[offsetsc].equals("freq"))
				else if (filtDomain[offsetsc].equals("time")) {
					T2z1b_get_responses_time(null, null, null, null, null, Sc1,
							Sc2, Sc3, Sc22, Sc23, Sc33, td1[filInd - 1],
							td2[filInd - 1], td3[filInd - 1], td22[filInd - 1],
							td23[filInd - 1], td33[filInd - 1], domain, 0);
				} // else if (filtDomain[offsetsc].equals("time"))
				invDes[filInd - 1] = T2z0a_invert_design(Sc1, Sc2, Sc3, Sc22,
						Sc23, Sc33);
			} // for (dirInd = 1; dirInd <= ndirs; dirInd++)
		} // for (sc = 1; sc <= nscales; sc++)
	}
	
	private void test_T2z0a_invert_design() {
		// Test inversion of symmetrical 3 by 3 matrices
		double Sc1[][] = new double[1][1];
		double Sc2[][] = new double[1][1];
		double Sc3[][] = new double[1][1];
		double Sc22[][] = new double[1][1];
		double Sc23[][] = new double[1][1];
		double Sc33[][] = new double[1][1];
		double ans[][] = new double[3][3];
		double invMat[][][][];
		int x;
		int y;
		double diff;
		int testNum = 2;
		switch(testNum) {
		case 1:
		 //  2   -1    0
		 // -1    2   -1
		 //  0    -1   2
	     // has as its inverse
		 // 3/4   2/4   1/4
		 // 2/4   4/4   2/4
		 // 1/4   2/4   3/4
		 Sc1[0][0] = 2;
		 Sc2[0][0] = -1;
		 Sc3[0][0] = 0;
		 Sc22[0][0] = 2;
		 Sc23[0][0] = -1;
		 Sc33[0][0] = 2;
		 ans[0][0] = 0.75;
		 ans[0][1] = 0.5;
		 ans[0][2] = 0.25;
		 ans[1][0] = 0.5;
		 ans[1][1] = 1.0;
		 ans[1][2] = 0.5;
		 ans[2][0] = 0.25;
		 ans[2][1] = 0.5;
		 ans[2][2] = 0.75;
		 break;
		 case 2:
		 // 1    1/2   1/3
	     // 1/2  1/3   1/4
	     // 1/3  1/4   1/5
	     // has as its inverse
	     //  9    -36   30
	     // -36   192  -180
		 // 30   -180   180
	     // Small generated errors:
	     // y = 0 x = 0 diff = -1.1901590823981678E-13
		 // y = 0 x = 1 diff = 5.044853423896711E-13
	     // y = 0 x = 2 diff = -4.192202140984591E-13
		 // y = 1 x = 0 diff = 5.044853423896711E-13
		 // y = 1 x = 1 diff = -2.6147972675971687E-12
		 // y = 1 x = 2 diff = 2.4442670110147446E-12
	     // y = 2 x = 0 diff = -4.192202140984591E-13
	     // y = 2 x = 1 diff = 2.4442670110147446E-12
		 // y = 2 x = 2 diff = -2.3874235921539366E-12
		 Sc1[0][0] = 1.0;
		 Sc2[0][0] = 0.5;
		 Sc3[0][0] = 1.0/3.0;
		 Sc22[0][0] = 1.0/3.0;
		 Sc23[0][0] = 0.25;
		 Sc33[0][0] = 0.2;
		 ans[0][0] = 9.0;
		 ans[0][1] = -36.0;
		 ans[0][2] = 30.0;
		 ans[1][0] = -36.0;
		 ans[1][1] = 192.0;
		 ans[1][2] = -180.0;
		 ans[2][0] = 30.0;
		 ans[2][1] = -180.0;
		 ans[2][2] = 180.0;
		 break;
		}
		 invMat = T2z0a_invert_design(Sc1, Sc2, Sc3, Sc22, Sc23, Sc33);
		 for (y = 0; y < 3; y++) {
			 for (x = 0; x < 3; x++) {
				 diff = ans[y][x] - invMat[y][x][0][0];
				 System.out.println("y = " + y + " x = " + x + " diff = " + diff);
			 }
		 }
	}

	private double[][][][] T2z0a_invert_design(double Sc1[][], double Sc2[][],
			double Sc3[][], double Sc22[][], double Sc23[][], double Sc33[][]) {
		int y;
		int x;
		double invDes[][][][] = new double[3][3][Sc1.length][Sc1[0].length];
		double convqpinb[][][][] = new double[3][3][Sc1.length][Sc1[0].length];
		double det[][] = new double[Sc1.length][Sc1[0].length];
		double invDet[][] = new double[Sc1.length][Sc1[0].length];
		int k1;
		int k2;
		// dc * dc
		for (y = 0; y < Sc1.length; y++) {
			for (x = 0; x < Sc1[0].length; x++) {
				convqpinb[0][0][y][x] = Sc1[y][x];
			}
		}
		// sin * dc / cos * dc
		for (y = 0; y < Sc1.length; y++) {
			for (x = 0; x < Sc1[0].length; x++) {
				convqpinb[0][1][y][x] = Sc2[y][x];
				convqpinb[1][0][y][x] = Sc2[y][x];
				convqpinb[0][2][y][x] = Sc3[y][x];
				convqpinb[2][0][y][x] = Sc3[y][x];
			}
		}

		// cos * cos / sin * cos / sin * sin
		for (y = 0; y < Sc1.length; y++) {
			for (x = 0; x < Sc1[0].length; x++) {
				convqpinb[1][1][y][x] = Sc22[y][x];
				convqpinb[1][2][y][x] = Sc23[y][x];
				convqpinb[2][1][y][x] = Sc23[y][x];
				convqpinb[2][2][y][x] = Sc33[y][x];
			}
		}

		for (y = 0; y < Sc1.length; y++) {
			for (x = 0; x < Sc1[0].length; x++) {
				det[y][x] = convqpinb[0][0][y][x] * convqpinb[1][1][y][x]
						* convqpinb[2][2][y][x] - convqpinb[0][0][y][x]
						* convqpinb[1][2][y][x] * convqpinb[2][1][y][x]
						- convqpinb[1][0][y][x] * convqpinb[0][1][y][x]
						* convqpinb[2][2][y][x] + convqpinb[1][0][y][x]
						* convqpinb[0][2][y][x] * convqpinb[2][1][y][x]
						+ convqpinb[2][0][y][x] * convqpinb[0][1][y][x]
						* convqpinb[1][2][y][x] - convqpinb[2][0][y][x]
						* convqpinb[0][2][y][x] * convqpinb[1][1][y][x];
			}
		}

		for (y = 0; y < Sc1.length; y++) {
			for (x = 0; x < Sc1[0].length; x++) {
				invDes[0][0][y][x] = (convqpinb[1][1][y][x]
						* convqpinb[2][2][y][x] - convqpinb[1][2][y][x]
						* convqpinb[2][1][y][x]);
				invDes[0][1][y][x] = -(convqpinb[0][1][y][x]
						* convqpinb[2][2][y][x] - convqpinb[0][2][y][x]
						* convqpinb[2][1][y][x]);
				invDes[0][2][y][x] = (convqpinb[0][1][y][x]
						* convqpinb[1][2][y][x] - convqpinb[0][2][y][x]
						* convqpinb[1][1][y][x]);
				invDes[1][0][y][x] = -(convqpinb[1][0][y][x]
						* convqpinb[2][2][y][x] - convqpinb[1][2][y][x]
						* convqpinb[2][0][y][x]);
				invDes[1][1][y][x] = (convqpinb[0][0][y][x]
						* convqpinb[2][2][y][x] - convqpinb[0][2][y][x]
						* convqpinb[2][0][y][x]);
				invDes[1][2][y][x] = -(convqpinb[0][0][y][x]
						* convqpinb[1][2][y][x] - convqpinb[0][2][y][x]
						* convqpinb[1][0][y][x]);
				invDes[2][0][y][x] = (convqpinb[1][0][y][x]
						* convqpinb[2][1][y][x] - convqpinb[1][1][y][x]
						* convqpinb[2][0][y][x]);
				invDes[2][1][y][x] = -(convqpinb[0][0][y][x]
						* convqpinb[2][1][y][x] - convqpinb[0][1][y][x]
						* convqpinb[2][0][y][x]);
				invDes[2][2][y][x] = (convqpinb[0][0][y][x]
						* convqpinb[1][1][y][x] - convqpinb[0][1][y][x]
						* convqpinb[1][0][y][x]);
				if (det[y][x] >= 0.0) {
					invDet[y][x] = 1.0 / Math.max(det[y][x], 1.0E-7);
				} else {
					invDet[y][x] = -1.0 / Math.max(Math.abs(det[y][x]), 1.0E-7);
				}
			}
		}

		for (k1 = 0; k1 < 3; k1++) {
			for (k2 = 0; k2 < 3; k2++) {
				for (y = 0; y < Sc1.length; y++) {
					for (x = 0; x < Sc1[0].length; x++) {
						invDes[k1][k2][y][x] = invDes[k1][k2][y][x]
								* invDet[y][x];
					}
				}
			}
		}

		return invDes;
	}

	private void T2z1b_get_responses_time(double preComputedIc1[][],
			double preComputedIc2[][], double preComputedIc3[][],
			double preComputedSc1[][], double preComputedsm[][],
			double Sc1[][], double Sc2[][], double Sc3[][], double Sc22[][],
			double Sc23[][], double Sc33[][], double td1[][], double td2[][],
			double td3[][], double td22[][], double td23[][], double td33[][],
			double inputIm[][], int findNorm) {
		double onesm[][];
		int y;
		int x;
		switch (findNorm) {
		case 0:
			filter2(td1, inputIm, Sc1);
			filter2(td2, inputIm, Sc2);
			filter2(td3, inputIm, Sc3);
			filter2(td22, inputIm, Sc22);
			filter2(td23, inputIm, Sc23);
			filter2(td33, inputIm, Sc33);
			break;
		case 1:
			filter2(td1, inputIm, preComputedIc1);
			onesm = new double[inputIm.length][inputIm[0].length];
			for (y = 0; y < inputIm.length; y++) {
				for (x = 0; x < inputIm[0].length; x++) {
					onesm[y][x] = 1.0;
				}
			}
			filter2(td1, onesm, preComputedSc1);
			for (y = 0; y < preComputedIc1.length; y++) {
				for (x = 0; x < preComputedIc1[0].length; x++) {
					preComputedsm[y][x] = preComputedIc1[y][x]
							/ preComputedSc1[y][x];
				}
			}
			break;
		case 2:
			filter2(td2, inputIm, preComputedIc2);
		    filter2(td3, inputIm, preComputedIc3);
		}

	}

	private void filter2(double A[][], double B[][], double C[][]) {
		double A2[][] = new double[A.length][A[0].length];
		int y;
		int x;
		for (y = 0; y < A.length; y++) {
			for (x = 0; x < A[0].length; x++) {
				A2[y][x] = A[y][x];
			}
		}
		rot180(A2);
		conv2(B, A2, C);
		return;
	}
	
	private void testconv2() {
		int x;
		int y;
		double diff;
		int testNum = 3;
		double h[][];
		double I[][];
		double ans[][];
		double Cout[][];
		double f[][];
		double g[][];
		switch(testNum) {
			case 1:
			h = new double[3][3];
			h[0][0] = 1.0;
			h[0][1] = 2.0;
			h[0][2] = 3.0;
			h[1][0] = 0.0;
			h[1][1] = 0.0;
			h[1][2] = 0.0;
			h[2][0] = 6.0;
			h[2][1] = 5.0;
			h[2][2] = 4.0;
			I = new double[3][4];
			I[0][0] = 1.0;
			I[0][1] = 5.0;
			I[0][2] = 2.0;
			I[0][3] = 3.0;
			I[1][0] = 8.0;
			I[1][1] = 7.0;
			I[1][2] = 3.0;
			I[1][3] = 6.0;
			I[2][0] = 3.0;
			I[2][1] = 3.0;
			I[2][2] = 9.0;
			I[2][3] = 1.0;
			ans = new double[3][4];
			ans[0][0] = 23.0;
			ans[0][1] = 41.0;
			ans[0][2] = 33.0;
			ans[0][3] = 21.0;
			ans[1][0] = 44.0;
			ans[1][1] = 65.0;
			ans[1][2] = 76.0;
			ans[1][3] = 52.0;
			ans[2][0] = 82.0;
			ans[2][1] = 85.0;
			ans[2][2] = 79.0;
			ans[2][3] = 42.0;
			Cout = new double[I.length][I[0].length];
			conv2(I, h, Cout);
			for (y = 0; y < I.length; y++) {
				for (x = 0; x < I[0].length; x++) {
					diff = Cout[y][x] - ans[y][x];
					System.out.println("Difference at y = " + y + " x = " + x + " is " + diff);
				}
			}
			break;
		case 2:
			f = new double[2][3];
			f[0][0] = 2;
			f[0][1] = 3;
			f[0][2] = 4;
			f[1][0] = 1;
			f[1][1] = 6;
			f[1][2] = 7;
			g = new double[3][3];
			g[0][0] = 9;
			g[0][1] = 1;
			g[0][2] = 0;
			g[1][0] = 2;
			g[1][1] = 5;
			g[1][2] = 8;
			g[2][0] = 1;
			g[2][1] = 3;
			g[2][2] = 3;
			ans = new double[2][3];
			ans[0][0] = 71;
			ans[0][1] = 108;
			ans[0][2] = 51;
			ans[1][0] = 26;
			ans[1][1] = 71;
			ans[1][2] = 104;
			Cout = new double[f.length][f[0].length];
			conv2(f, g, Cout);
			for (y = 0; y < f.length; y++) {
				for (x = 0; x < f[0].length; x++) {
					diff = Cout[y][x] - ans[y][x];
					System.out.println("Difference at y = " + y + " x = " + x + " is " + diff);
				}
			}
		    break;
		case 3:
			f = new double[2][3];
			f[0][0] = 2;
			f[0][1] = 3;
			f[0][2] = 4;
			f[1][0] = 1;
			f[1][1] = 6;
			f[1][2] = 7;
			g = new double[3][3];
			g[0][0] = 9;
			g[0][1] = 1;
			g[0][2] = 0;
			g[1][0] = 2;
			g[1][1] = 5;
			g[1][2] = 8;
			g[2][0] = 1;
			g[2][1] = 3;
			g[2][2] = 3;
			ans = new double[3][3];
			ans[0][0] = 71;
			ans[0][1] = 108;
			ans[0][2] = 51;
			ans[1][0] = 26;
			ans[1][1] = 71;
			ans[1][2] = 104;
			ans[2][0] = 9;
			ans[2][1] = 28;
			ans[2][2] = 39;
			Cout = new double[g.length][g[0].length];
			conv2(g, f, Cout);
			for (y = 0; y < g.length; y++) {
				for (x = 0; x < g[0].length; x++) {
					diff = Cout[y][x] - ans[y][x];
					System.out.println("Difference at y = " + y + " x = " + x + " is " + diff);
				}
			}
		    break;
		}
	}

	private void conv2(double A[][], double B[][], double Cout[][]) {
		double L[][];
		double S[][];
		int ml;
		int nl;
		int ms;
		int ns;
		int y;
		int x;
		int y2;
		int x2;
		double C[][];
		double small;
		int yoff;
		int xoff;
		if (A.length * A[0].length >= B.length * B[0].length) {
			ml = A.length;
			nl = A[0].length;
			L = A;
			ms = B.length;
			ns = B[0].length;
			S = B;
		} else {
			ml = B.length;
			nl = B[0].length;
			L = B;
			ms = A.length;
			ns = A[0].length;
			S = A;
		}
		C = new double[ml + ms - 1][nl + ns - 1];
		for (y = 0; y < ms; y++) {
			for (x = 0; x < ns; x++) {
				small = S[y][x];
				if (small != 0.0) {
					for (y2 = 0; y2 < ml; y2++) {
						for (x2 = 0; x2 < nl; x2++) {
							C[y + y2][x + x2] += L[y2][x2] * small;
						}
					}
				}
			}
		}
		yoff = (int) Math.floor(B.length / 2.0);
		xoff = (int) Math.floor(B[0].length / 2.0);
		for (y = 0; y < A.length; y++) {
			for (x = 0; x < A[0].length; x++) {
				Cout[y][x] = C[y + yoff][x + xoff];
			}
		}
		return;
	}

	private double[] conv(double A[], double B[], double Cout[]) {
		double L[];
		double S[];
		int ml;
		int ms;
		int y;
		int y2;
		double C[];
		double small;
		int yoff;
		if (A.length >= B.length) {
			ml = A.length;
			L = A;
			ms = B.length;
			S = B;
		} else {
			ml = B.length;
			L = B;
			ms = A.length;
			S = A;
		}
		C = new double[ml + ms - 1];
		for (y = 0; y < ms; y++) {
			small = S[y];
			if (small != 0.0) {
				for (y2 = 0; y2 < ml; y2++) {
					C[y + y2] += L[y2] * small;
				}
			}
		}
		yoff = (int) Math.floor(B.length / 2.0);
		for (y = 0; y < A.length; y++) {
			Cout[y] = C[y + yoff];
		}
		return Cout;
	}

	private void T2z1b_get_responses_freq(double preComputedIc1[][],
			double preComputedIc2[][], double preComputedIc3[][],
			double preComputedSc1[][], double preComputedsm[][],
			double Sc1[][], double Sc2[][], double Sc3[][], double Sc22[][],
			double Sc23[][], double Sc33[][], double fftSupportPatch[],
			double fftSupportPatchImag[], int patchSize,
			double fftImagePatch[], double fftImagePatchImag[], double fd1[][],
			double fd1Imag[][], double fd2[][], double fd2Imag[][],
			double fd3[][], double fd3Imag[][], double fd22[][],
			double fd22Imag[][], double fd23[][], double fd23Imag[][],
			double fd33[][], double fd33Imag[][], int findNorm) {
		int sizem;
		int sizen;
		int y;
		int x;
		double fd1Copy[][] = null;
		double fd1ImagCopy[][] = null;
		double fd2Copy[][] = null;
		double fd2ImagCopy[][] = null;
		double fd3Copy[][] = null;
		double fd3ImagCopy[][] = null;
		double fd22Copy[][] = null;
		double fd22ImagCopy[][] = null;
		double fd23Copy[][] = null;
		double fd23ImagCopy[][] = null;
		double fd33Copy[][] = null;
		double fd33ImagCopy[][] = null;
	
		if (fd1 != null) {
			sizem = fd1.length;
			sizen = fd1[0].length;
		}
		else {
			sizem = fd2.length;
			sizen = fd2[0].length;	
		}
		int yDim = sizem - 2 * patchSize;
		int xDim = sizen - 2 * patchSize;
		FFTUtility fft;
		double ScR[] = new double[sizem * sizen];
		double ScI[] = new double[sizem * sizen];
		
		// Inverse fftshifts
		if (fd1 != null) {
			fd1Copy = new double[sizem][sizen];
			for (y = 0; y < sizem; y++) {
				for (x = 0; x < sizen; x++) {
					fd1Copy[y][x] = fd1[y][x];
				}
			}
		    ifftshift(fd1Copy);
		}
		if (fd1Imag != null) {
			fd1ImagCopy = new double[sizem][sizen];
			for (y = 0; y < sizem; y++) {
				for (x = 0; x < sizen; x++) {
					fd1ImagCopy[y][x] = fd1Imag[y][x];
				}
			}
		    ifftshift(fd1ImagCopy);
		}
		if (fd2 != null) {
			fd2Copy = new double[sizem][sizen];
			for (y = 0; y < sizem; y++) {
				for (x = 0; x < sizen; x++) {
					fd2Copy[y][x] = fd2[y][x];
				}
			}
		    ifftshift(fd2Copy);
		}
		if (fd2Imag != null) {
			fd2ImagCopy = new double[sizem][sizen];
			for (y = 0; y < sizem; y++) {
				for (x = 0; x < sizen; x++) {
					fd2ImagCopy[y][x] = fd2Imag[y][x];
				}
			}
		    ifftshift(fd2ImagCopy);
		}
		if (fd3 != null) {
			fd3Copy = new double[sizem][sizen];
			for (y = 0; y < sizem; y++) {
				for (x = 0; x < sizen; x++) {
					fd3Copy[y][x] = fd3[y][x];
				}
			}
		    ifftshift(fd3Copy);
		}
		if (fd3Imag != null) {
			fd3ImagCopy = new double[sizem][sizen];
			for (y = 0; y < sizem; y++) {
				for (x = 0; x < sizen; x++) {
					fd3ImagCopy[y][x] = fd3Imag[y][x];
				}
			}
		    ifftshift(fd3ImagCopy);
		}
		if (fd22 != null) {
			fd22Copy = new double[sizem][sizen];
			for (y = 0; y < sizem; y++) {
				for (x = 0; x < sizen; x++) {
					fd22Copy[y][x] = fd22[y][x];
				}
			}
		    ifftshift(fd22Copy);
		}
		if (fd22Imag != null) {
			fd22ImagCopy = new double[sizem][sizen];
			for (y = 0; y < sizem; y++) {
				for (x = 0; x < sizen; x++) {
					fd22ImagCopy[y][x] = fd22Imag[y][x];
				}
			}
		    ifftshift(fd22ImagCopy);
		}
		if (fd23 != null) {
			fd23Copy = new double[sizem][sizen];
			for (y = 0; y < sizem; y++) {
				for (x = 0; x < sizen; x++) {
					fd23Copy[y][x] = fd23[y][x];
				}
			}
		    ifftshift(fd23Copy);
		}
		if (fd23Imag != null) {
			fd23ImagCopy = new double[sizem][sizen];
			for (y = 0; y < sizem; y++) {
				for (x = 0; x < sizen; x++) {
					fd23ImagCopy[y][x] = fd23Imag[y][x];
				}
			}
		    ifftshift(fd23ImagCopy);
		}
		if (fd33 != null) {
			fd33Copy = new double[sizem][sizen];
			for (y = 0; y < sizem; y++) {
				for (x = 0; x < sizen; x++) {
					fd33Copy[y][x] = fd33[y][x];
				}
			}
		    ifftshift(fd33Copy);
		}
		if (fd33Imag != null) {
			fd33ImagCopy = new double[sizem][sizen];
			for (y = 0; y < sizem; y++) {
				for (x = 0; x < sizen; x++) {
					fd33ImagCopy[y][x] = fd33Imag[y][x];
				}
			}
		    ifftshift(fd33ImagCopy);
		}

		switch (findNorm) {
		case 0:
			for (y = 0; y < sizem; y++) {
				for (x = 0; x < sizen; x++) {
					ScR[x + y * sizen] = fd1Copy[y][x]
							* fftSupportPatch[x + y * sizen] - fd1ImagCopy[y][x]
							* fftSupportPatchImag[x + y * sizen];
					ScI[x + y * sizen] = fd1ImagCopy[y][x]
							* fftSupportPatch[x + y * sizen] + fd1Copy[y][x]
							* fftSupportPatchImag[x + y * sizen];
				}
			}

			fft = new FFTUtility(ScR, ScI, sizem, sizen, 1, 1, FFTUtility.FFT);
			fft.setShowProgress(false);
			fft.run();
			fft.finalize();
			fft = null;
			fft = new FFTUtility(ScR, ScI, 1, sizem, sizen, 1, FFTUtility.FFT);
			fft.setShowProgress(false);
			fft.run();
			fft.finalize();
			fft = null;

			for (y = 0; y < yDim; y++) {
				for (x = 0; x < xDim; x++) {
					Sc1[y][x] = ScR[x + patchSize + sizen * (y + patchSize)];
				}
			}

			for (y = 0; y < sizem; y++) {
				for (x = 0; x < sizen; x++) {
					ScR[x + y * sizen] = fd2Copy[y][x]
							* fftSupportPatch[x + y * sizen] - fd2ImagCopy[y][x]
							* fftSupportPatchImag[x + y * sizen];
					ScI[x + y * sizen] = fd2ImagCopy[y][x]
							* fftSupportPatch[x + y * sizen] + fd2Copy[y][x]
							* fftSupportPatchImag[x + y * sizen];
				}
			}

			fft = new FFTUtility(ScR, ScI, sizem, sizen, 1, 1, FFTUtility.FFT);
			fft.setShowProgress(false);
			fft.run();
			fft.finalize();
			fft = null;
			fft = new FFTUtility(ScR, ScI, 1, sizem, sizen, 1, FFTUtility.FFT);
			fft.setShowProgress(false);
			fft.run();
			fft.finalize();
			fft = null;

			for (y = 0; y < yDim; y++) {
				for (x = 0; x < xDim; x++) {
					Sc2[y][x] = ScR[x + patchSize + sizen * (y + patchSize)];
				}
			}

			for (y = 0; y < sizem; y++) {
				for (x = 0; x < sizen; x++) {
					ScR[x + y * sizen] = fd3Copy[y][x]
							* fftSupportPatch[x + y * sizen] - fd3ImagCopy[y][x]
							* fftSupportPatchImag[x + y * sizen];
					ScI[x + y * sizen] = fd3ImagCopy[y][x]
							* fftSupportPatch[x + y * sizen] + fd3Copy[y][x]
							* fftSupportPatchImag[x + y * sizen];
				}
			}

			fft = new FFTUtility(ScR, ScI, sizem, sizen, 1, 1, FFTUtility.FFT);
			fft.setShowProgress(false);
			fft.run();
			fft.finalize();
			fft = null;
			fft = new FFTUtility(ScR, ScI, 1, sizem, sizen, 1, FFTUtility.FFT);
			fft.setShowProgress(false);
			fft.run();
			fft.finalize();
			fft = null;

			for (y = 0; y < yDim; y++) {
				for (x = 0; x < xDim; x++) {
					Sc3[y][x] = ScR[x + patchSize + sizen * (y + patchSize)];
				}
			}

			for (y = 0; y < sizem; y++) {
				for (x = 0; x < sizen; x++) {
					ScR[x + y * sizen] = fd22Copy[y][x]
							* fftSupportPatch[x + y * sizen] - fd22ImagCopy[y][x]
							* fftSupportPatchImag[x + y * sizen];
					ScI[x + y * sizen] = fd22ImagCopy[y][x]
							* fftSupportPatch[x + y * sizen] + fd22Copy[y][x]
							* fftSupportPatchImag[x + y * sizen];
				}
			}

			fft = new FFTUtility(ScR, ScI, sizem, sizen, 1, 1, FFTUtility.FFT);
			fft.setShowProgress(false);
			fft.run();
			fft.finalize();
			fft = null;
			fft = new FFTUtility(ScR, ScI, 1, sizem, sizen, 1, FFTUtility.FFT);
			fft.setShowProgress(false);
			fft.run();
			fft.finalize();
			fft = null;

			for (y = 0; y < yDim; y++) {
				for (x = 0; x < xDim; x++) {
					Sc22[y][x] = ScR[x + patchSize + sizen * (y + patchSize)];
				}
			}

			for (y = 0; y < sizem; y++) {
				for (x = 0; x < sizen; x++) {
					ScR[x + y * sizen] = fd23Copy[y][x]
							* fftSupportPatch[x + y * sizen] - fd23ImagCopy[y][x]
							* fftSupportPatchImag[x + y * sizen];
					ScI[x + y * sizen] = fd23ImagCopy[y][x]
							* fftSupportPatch[x + y * sizen] + fd23Copy[y][x]
							* fftSupportPatchImag[x + y * sizen];
				}
			}

			fft = new FFTUtility(ScR, ScI, sizem, sizen, 1, 1, FFTUtility.FFT);
			fft.setShowProgress(false);
			fft.run();
			fft.finalize();
			fft = null;
			fft = new FFTUtility(ScR, ScI, 1, sizem, sizen, 1, FFTUtility.FFT);
			fft.setShowProgress(false);
			fft.run();
			fft.finalize();
			fft = null;
			
			for (y = 0; y < yDim; y++) {
				for (x = 0; x < xDim; x++) {
					Sc23[y][x] = ScR[x + patchSize + sizen * (y + patchSize)];
				}
			}

			for (y = 0; y < sizem; y++) {
				for (x = 0; x < sizen; x++) {
					ScR[x + y * sizen] = fd33Copy[y][x]
							* fftSupportPatch[x + y * sizen] - fd33ImagCopy[y][x]
							* fftSupportPatchImag[x + y * sizen];
					ScI[x + y * sizen] = fd33ImagCopy[y][x]
							* fftSupportPatch[x + y * sizen] + fd33Copy[y][x]
							* fftSupportPatchImag[x + y * sizen];
				}
			}

			fft = new FFTUtility(ScR, ScI, sizem, sizen, 1, 1, FFTUtility.FFT);
			fft.setShowProgress(false);
			fft.run();
			fft.finalize();
			fft = null;
			fft = new FFTUtility(ScR, ScI, 1, sizem, sizen, 1, FFTUtility.FFT);
			fft.setShowProgress(false);
			fft.run();
			fft.finalize();
			fft = null;

			for (y = 0; y < yDim; y++) {
				for (x = 0; x < xDim; x++) {
					Sc33[y][x] = ScR[x + patchSize + sizen * (y + patchSize)];
				}
			}

			break;
		case 1:
			for (y = 0; y < sizem; y++) {
				for (x = 0; x < sizen; x++) {
					ScR[x + y * sizen] = fd1Copy[y][x]
							* fftSupportPatch[x + y * sizen] - fd1ImagCopy[y][x]
							* fftSupportPatchImag[x + y * sizen];
					ScI[x + y * sizen] = fd1ImagCopy[y][x]
							* fftSupportPatch[x + y * sizen] + fd1Copy[y][x]
							* fftSupportPatchImag[x + y * sizen];
				}
			}

			fft = new FFTUtility(ScR, ScI, sizem, sizen, 1, 1, FFTUtility.FFT);
			fft.setShowProgress(false);
			fft.run();
			fft.finalize();
			fft = null;
			fft = new FFTUtility(ScR, ScI, 1, sizem, sizen, 1, FFTUtility.FFT);
			fft.setShowProgress(false);
			fft.run();
			fft.finalize();
			fft = null;

			for (y = 0; y < yDim; y++) {
				for (x = 0; x < xDim; x++) {
					preComputedSc1[y][x] = ScR[x + patchSize + sizen
							* (y + patchSize)];
				}
			}

			for (y = 0; y < sizem; y++) {
				for (x = 0; x < sizen; x++) {
					ScR[x + y * sizen] = fd1Copy[y][x]
							* fftImagePatch[x + y * sizen] - fd1ImagCopy[y][x]
							* fftImagePatchImag[x + y * sizen];
					ScI[x + y * sizen] = fd1ImagCopy[y][x]
							* fftImagePatch[x + y * sizen] + fd1Copy[y][x]
							* fftImagePatchImag[x + y * sizen];
				}
			}

			fft = new FFTUtility(ScR, ScI, sizem, sizen, 1, 1, FFTUtility.FFT);
			fft.setShowProgress(false);
			fft.run();
			fft.finalize();
			fft = null;
			fft = new FFTUtility(ScR, ScI, 1, sizem, sizen, 1, FFTUtility.FFT);
			fft.setShowProgress(false);
			fft.run();
			fft.finalize();
			fft = null;

			for (y = 0; y < yDim; y++) {
				for (x = 0; x < xDim; x++) {
					preComputedIc1[y][x] = ScR[x + patchSize + sizen
							* (y + patchSize)];
				}
			}

			for (y = 0; y < yDim; y++) {
				for (x = 0; x < xDim; x++) {
					preComputedsm[y][x] = preComputedIc1[y][x]
							/ preComputedSc1[y][x];
				}
			}

			break;
		case 2:
			for (y = 0; y < sizem; y++) {
				for (x = 0; x < sizen; x++) {
					ScR[x + y * sizen] = fd2Copy[y][x]
							* fftImagePatch[x + y * sizen] - fd2ImagCopy[y][x]
							* fftImagePatchImag[x + y * sizen];
					ScI[x + y * sizen] = fd2ImagCopy[y][x]
							* fftImagePatch[x + y * sizen] + fd2Copy[y][x]
							* fftImagePatchImag[x + y * sizen];
				}
			}

			fft = new FFTUtility(ScR, ScI, sizem, sizen, 1, 1, FFTUtility.FFT);
			fft.setShowProgress(false);
			fft.run();
			fft.finalize();
			fft = null;
			fft = new FFTUtility(ScR, ScI, 1, sizem, sizen, 1, FFTUtility.FFT);
			fft.setShowProgress(false);
			fft.run();
			fft.finalize();
			fft = null;

			for (y = 0; y < yDim; y++) {
				for (x = 0; x < xDim; x++) {
					preComputedIc2[y][x] = ScR[x + patchSize + sizen
							* (y + patchSize)];
				}
			}

			for (y = 0; y < sizem; y++) {
				for (x = 0; x < sizen; x++) {
					ScR[x + y * sizen] = fd3Copy[y][x]
							* fftImagePatch[x + y * sizen] - fd3ImagCopy[y][x]
							* fftImagePatchImag[x + y * sizen];
					ScI[x + y * sizen] = fd3ImagCopy[y][x]
							* fftImagePatch[x + y * sizen] + fd3Copy[y][x]
							* fftImagePatchImag[x + y * sizen];
				}
			}

			fft = new FFTUtility(ScR, ScI, sizem, sizen, 1, 1, FFTUtility.FFT);
			fft.setShowProgress(false);
			fft.run();
			fft.finalize();
			fft = null;
			fft = new FFTUtility(ScR, ScI, 1, sizem, sizen, 1, FFTUtility.FFT);
			fft.setShowProgress(false);
			fft.run();
			fft.finalize();
			fft = null;

			for (y = 0; y < yDim; y++) {
				for (x = 0; x < xDim; x++) {
					preComputedIc3[y][x] = ScR[x + patchSize + sizen
							* (y + patchSize)];
				}
			}
		} // switch (fnorm)
	}

	private void fftshift(double mtx[][]) {
		int sizem = mtx.length;
		int sizen = mtx[0].length;
		int y;
		int x;
		int DCY = (int) Math.ceil(sizem / 2.0);
		int DCX = (int) Math.ceil(sizen / 2.0);
		double temp[][] = new double[sizem][sizen];
		for (y = 0; y < sizem - DCY; y++) {
			for (x = 0; x < sizen - DCX; x++) {
				temp[y][x] = mtx[y + DCY][x + DCX];
			}
			for (x = sizen - DCX; x < sizen; x++) {
				temp[y][x] = mtx[y + DCY][x - (sizen - DCX)];
			}
		}
		for (y = sizem - DCY; y < sizem; y++) {
			for (x = 0; x < sizen - DCX; x++) {
				temp[y][x] = mtx[y - (sizem - DCY)][x + DCX];
			}
			for (x = sizen - DCX; x < sizen; x++) {
				temp[y][x] = mtx[y - (sizem - DCY)][x - (sizen - DCX)];
			}
		}
		for (y = 0; y < sizem; y++) {
			for (x = 0; x < sizen; x++) {
				mtx[y][x] = temp[y][x];
			}
		}
	}
	
	private void ifftshift(double mtx[][]) {
		int sizem = mtx.length;
		int sizen = mtx[0].length;
		int y;
		int x;
		int DCY = (int) Math.floor(sizem / 2.0);
		int DCX = (int) Math.floor(sizen / 2.0);
		double temp[][] = new double[sizem][sizen];
		for (y = 0; y < sizem - DCY; y++) {
			for (x = 0; x < sizen - DCX; x++) {
				temp[y][x] = mtx[y + DCY][x + DCX];
			}
			for (x = sizen - DCX; x < sizen; x++) {
				temp[y][x] = mtx[y + DCY][x - (sizen - DCX)];
			}
		}
		for (y = sizem - DCY; y < sizem; y++) {
			for (x = 0; x < sizen - DCX; x++) {
				temp[y][x] = mtx[y - (sizem - DCY)][x + DCX];
			}
			for (x = sizen - DCX; x < sizen; x++) {
				temp[y][x] = mtx[y - (sizem - DCY)][x - (sizen - DCX)];
			}
		}
		for (y = 0; y < sizem; y++) {
			for (x = 0; x < sizen; x++) {
				mtx[y][x] = temp[y][x];
			}
		}
	}

	private void T2z1a_make_image_structure(double fftImagePatch[],
			double fftImagePatchImag[], double fftSupportPatch[],
			double fftSupportPatchImag[], double input[][], int patchSize) {
		int sizem0 = input.length;
		int sizen0 = input[0].length;
		int padXDim = sizen0 + 2 * patchSize;
		int padYDim = sizem0 + 2 * patchSize;
		int y;
		int x;
		double timeImagePatch[] = new double[padXDim * padYDim];
		double timeSupportPatch[] = new double[padXDim * padYDim];
		FFTUtility fft;
		for (y = 0; y < sizem0; y++) {
			for (x = 0; x < sizen0; x++) {
				timeImagePatch[x + patchSize + padXDim * (y + patchSize)] = input[y][x];
				timeSupportPatch[x + patchSize + padXDim * (y + patchSize)] = 1.0;
				fftImagePatch[x + patchSize + padXDim * (y + patchSize)] = input[y][x];
				fftSupportPatch[x + patchSize + padXDim * (y + patchSize)] = 1.0;
			}
		}

		fft = new FFTUtility(fftImagePatch, fftImagePatchImag, padYDim,
				padXDim, 1, -1, FFTUtility.FFT);
		fft.setShowProgress(false);
		fft.run();
		fft.finalize();
		fft = null;
		fft = new FFTUtility(fftImagePatch, fftImagePatchImag, 1, padYDim,
				padXDim, -1, FFTUtility.FFT);
		fft.setShowProgress(false);
		fft.run();
		fft.finalize();
		fft = null;

		fft = new FFTUtility(fftSupportPatch, fftSupportPatchImag, padYDim,
				padXDim, 1, -1, FFTUtility.FFT);
		fft.setShowProgress(false);
		fft.run();
		fft.finalize();
		fft = null;
		fft = new FFTUtility(fftSupportPatch, fftSupportPatchImag, 1, padYDim,
				padXDim, -1, FFTUtility.FFT);
		fft.setShowProgress(false);
		fft.run();
		fft.finalize();
		fft = null;

		return;
	}

	private void T1_responses(double omegas[][], double amplitudes[][],
			double filterAngle[], double sigmaX[],
			double td1[][][], double td2[][][], double td3[][][],
			double td22[][][], double td23[][][], double td33[][][],
			double fd1[][][], double fd1Imag[][][], double fd2[][][],
			double fd2Imag[][][], double fd3[][][], double fd3Imag[][][],
			double fd22[][][], double fd22Imag[][][], double fd23[][][],
			double fd23Imag[][][], double fd33[][][], double fd33Imag[][][],
			double sigmas[], int ps[], final int nscales, final int ndirs,
			final double sig2omega, final double radianStart,
			final double radianEnd, final int inputXDim, final int inputYDim,
			final String filterType, String domain[]) {
		int filInd;
		int scInd;
		int drInd;
		int szfm;
		int szfn;
		int i;
		double fm[];
		double fn[];
		double xfreq[][];
		double yfreq[][];
		int y;
		int x;

		filterbank_DCA_2D(omegas, amplitudes, filterAngle,
				sigmaX, nscales, filterType, ndirs, sig2omega, radianStart,
				radianEnd);

		filInd = 0;
		for (scInd = 1; scInd <= nscales; scInd++) {

			for (drInd = 1; drInd <= ndirs; drInd++) {
				filInd++;
				sigmas[filInd - 1] = sigmaX[filInd - 1];

				if (domain[filInd-1].equals("freq")) {
					ps[filInd - 1] = 3 * (int) Math.ceil(sigmas[filInd - 1]);
					szfm = inputYDim + 2 * ps[filInd - 1];
					szfn = inputXDim + 2 * ps[filInd - 1];
					fm = new double[szfm];
					fn = new double[szfn];
					if ((szfm % 2) == 1) {
						for (i = 0; i < szfm; i++) {
							fm[i] = -1.0 + 1.0 / szfm + (2.0 * i) / szfm;
						}
					} else {
						for (i = 0; i < szfm; i++) {
							fm[i] = -1.0 + (2.0 * i) / szfm;
						}
					}
					if ((szfn % 2) == 1) {
						for (i = 0; i < szfn; i++) {
							fn[i] = -1.0 + 1.0 / szfn + (2.0 * i) / szfn;
						}
					} else {
						for (i = 0; i < szfn; i++) {
							fn[i] = -1.0 + (2.0 * i) / szfn;
						}
					}
					xfreq = new double[szfm][szfn];
					yfreq = new double[szfm][szfn];
					for (y = 0; y < szfm; y++) {
						for (x = 0; x < szfn; x++) {
							xfreq[y][x] = fn[x];
						}
					}
					for (x = 0; x < szfn; x++) {
						for (y = 0; y < szfm; y++) {
							yfreq[y][x] = fm[y];
						}
					}
				} // if (domain[filInd-1].equals("freq"))
				else {
					xfreq = null;
					yfreq = null;
					ps[filInd - 1] = -1;
				}
				T1z2_get_filter_struct(td1, td2,
						td3, td22, td23,
						td33, fd1, fd1Imag,
						fd2, fd2Imag, fd3,
						fd3Imag, fd22,
						fd22Imag, fd23,
						fd23Imag, fd33,
						fd33Imag, omegas[filInd - 1],
						amplitudes[filInd - 1], filterAngle[filInd - 1],
						sigmaX[filInd - 1], domain[filInd-1], xfreq, yfreq, filInd-1);	
			} // for (drInd = 1; drInd <= ndirs; drInd++)
		} // for (scInd = 1; scInd <= nscales; scInd++)

	}

	private void T1z2_get_filter_struct(double td1[][][], double td2[][][],
			double td3[][][], double td22[][][], double td23[][][], double td33[][][],
			double fd1[][][], double fd1Imag[][][], double fd2[][][],
			double fd2Imag[][][], double fd3[][][], double fd3Imag[][][],
			double fd22[][][], double fd22Imag[][][], double fd23[][][],
			double fd23Imag[][][], double fd33[][][], double fd33Imag[][][],
			final double omegas[], final double amplitudes[],
			final double filterAngle, final double sigmaX, final String domain,
			final double xfreq[][], final double yfreq[][], int index) {
		
		int sz[] = new int[2];
		// gaussian (constant basis)
		double []amplitudesgb0 = new double[]{2};
		double []amplitudesgb0Imag = new double[]{0};
		double []omegasgb0 = new double[] { 0 };
		//T1z2a_convert_filter(omegasgb0, amplitudesgb0, amplitudesgb0Imag,
				//omegas, amplitudes, filterAngle, sigmaX, 0);
        
		// even part of gabor complex
		double []amplitudesgbe = new double[4*amplitudes.length-3];
		double []amplitudesgbeImag = new double[4*amplitudes.length-3];
		double []omegasgbe = new double[4*omegas.length-3];
		T1z2a_convert_filter(omegasgbe, amplitudesgbe, amplitudesgbeImag,
				omegas, amplitudes, filterAngle, sigmaX, 1);

		// odd part of gabor complex
		double []amplitudesgbo = new double[4*amplitudes.length-3];
		double []amplitudesgboImag = new double[4*amplitudes.length-3];
		double []omegasgbo = new double[4*omegas.length-3];
		T1z2a_convert_filter(omegasgbo, amplitudesgbo, amplitudesgboImag,
				omegas, amplitudes, filterAngle, sigmaX, 2);

		// even basis squared * gaussian
		double []amplitudesgbee = new double[4*amplitudes.length-3];
		double []amplitudesgbeeImag = new double[4*amplitudes.length-3];
		double []omegasgbee = new double[4*omegas.length-3];
		T1z2a_convert_filter(omegasgbee, amplitudesgbee, amplitudesgbeeImag,
				omegas, amplitudes, filterAngle, sigmaX, 11);

		// even basis * odd basis * gaussian
		double []amplitudesgbeo = new double[4*amplitudes.length-3];
		double []amplitudesgbeoImag = new double[4*amplitudes.length-3];
		double []omegasgbeo = new double[4*omegas.length-3];
		T1z2a_convert_filter(omegasgbeo, amplitudesgbeo, amplitudesgbeoImag,
				omegas, amplitudes, filterAngle, sigmaX, 12);

		// odd basis squared * gaussian
		double []amplitudesgboo = new double[4*amplitudes.length-3];
		double []amplitudesgbooImag = new double[4*amplitudes.length-3];
		double []omegasgboo = new double[4*omegas.length-3];
		T1z2a_convert_filter(omegasgboo, amplitudesgboo, amplitudesgbooImag,
				omegas, amplitudes, filterAngle, sigmaX, 22);

		if (domain.equals("time")) {
			td1[index] = T1z2b_time_resp(omegasgb0, amplitudesgb0, amplitudesgb0Imag,
					filterAngle, sigmaX);
			td2[index] = T1z2b_time_resp(omegasgbe, amplitudesgbe, amplitudesgbeImag,
					filterAngle, sigmaX);
			td3[index] = T1z2b_time_resp(omegasgbo, amplitudesgbo, amplitudesgboImag,
					filterAngle, sigmaX);
			td22[index] = T1z2b_time_resp(omegasgbee, amplitudesgbee,
					amplitudesgbeeImag, filterAngle, sigmaX);
			td23[index] = T1z2b_time_resp(omegasgbeo, amplitudesgbeo,
					amplitudesgbeoImag, filterAngle, sigmaX);
			td33[index] = T1z2b_time_resp(omegasgboo, amplitudesgboo,
					amplitudesgbooImag, filterAngle, sigmaX);
		}
		else if (domain.equals("freq")) {
			double td1temp[][];
			double td2temp[][];
			double td3temp[][];
			double td22temp[][];
			double td23temp[][];
			double td33temp[][];
			td1temp = T1z2b_time_resp(omegasgb0, amplitudesgb0, amplitudesgb0Imag,
					filterAngle, sigmaX);
			td2temp = T1z2b_time_resp(omegasgbe, amplitudesgbe, amplitudesgbeImag,
					filterAngle, sigmaX);
			td3temp = T1z2b_time_resp(omegasgbo, amplitudesgbo, amplitudesgboImag,
					filterAngle, sigmaX);
			td22temp = T1z2b_time_resp(omegasgbee, amplitudesgbee,
					amplitudesgbeeImag, filterAngle, sigmaX);
			td23temp = T1z2b_time_resp(omegasgbeo, amplitudesgbeo,
					amplitudesgbeoImag, filterAngle, sigmaX);
			td33temp = T1z2b_time_resp(omegasgboo, amplitudesgboo,
					amplitudesgbooImag, filterAngle, sigmaX);
			sz[0] = xfreq.length;
			sz[1] = xfreq[0].length;
			fd1[index] = new double[xfreq.length][xfreq[0].length];
			fd1Imag[index] = new double[xfreq.length][xfreq[0].length];
			fd2[index] = new double[xfreq.length][xfreq[0].length];
			fd2Imag[index] = new double[xfreq.length][xfreq[0].length];
			fd3[index] = new double[xfreq.length][xfreq[0].length];
			fd3Imag[index] = new double[xfreq.length][xfreq[0].length];
			fd22[index] = new double[xfreq.length][xfreq[0].length];
			fd22Imag[index] = new double[xfreq.length][xfreq[0].length];
			fd23[index] = new double[xfreq.length][xfreq[0].length];
			fd23Imag[index] = new double[xfreq.length][xfreq[0].length];
			fd33[index] = new double[xfreq.length][xfreq[0].length];
			fd33Imag[index] = new double[xfreq.length][xfreq[0].length];
			freqz2(fd1[index], fd1Imag[index], td1temp, sz[1], sz[0]);
			freqz2(fd2[index], fd2Imag[index], td2temp, sz[1], sz[0]);
			freqz2(fd3[index], fd3Imag[index], td3temp, sz[1], sz[0]);
			freqz2(fd22[index], fd22Imag[index], td22temp, sz[1], sz[0]);
			freqz2(fd23[index], fd23Imag[index], td23temp, sz[1], sz[0]);
			freqz2(fd33[index], fd33Imag[index], td33temp, sz[1], sz[0]);
			} // else if (domain.equals("freq"))
        // domain is never equal to freq_pure
	}

	private void Tzz_freq_resp(double hout[][],
			double omegax[][], double omegay[][], double sigmaX,
			double filterAngle, double omegas[], double amplitudes[]) {
		int y;
		int x;
		double invSigmaX;
		double cosang;
		double sinang;
		int sizem;
		int sizen;
		int i;
		double rotatedx[][][];
		double rotatedy[][][];
		int cnt;
		int sig1[] = new int[] { 0, -1, 1 };
		int sig2[] = new int[] { 0, -1, 1 };
		int sig1Index;
		int sig2Index;
		double absAmp[];
		double maxAbsAmp;
		double threshMin;
		boolean omegaswt[];
		double carrier;
		double cenx;
		double ceny;
		double offx;
		double offy;
		double distCen[][];
		double gaussianTerm;
		int j;
		double diffx;
		double diffy;
		double denom;
		double omegax2[][] = new double[omegax.length][omegax[0].length];
		double omegay2[][] = new double[omegay.length][omegay[0].length];

		for (y = 0; y < omegax.length; y++) {
			for (x = 0; x < omegax[0].length; x++) {
				omegax2[y][x] = -omegax[y][x];
			}
		}
		for (y = 0; y < omegay.length; y++) {
			for (x = 0; x < omegay[0].length; x++) {
				omegay2[y][x] = -omegay[y][x];
			}
		}

		invSigmaX = 1.0 / (Math.PI * sigmaX);
		for (y = 0; y < hout.length; y++) {
			for (x = 0; x < hout[0].length; x++) {
				hout[y][x] = 0.0;
			}
		}
		cosang = Math.cos(filterAngle);
		sinang = Math.sin(filterAngle);

		sizem = omegax.length;
		sizen = omegax[0].length;
		
		
		//if ((maxAbsOmegas + 3.0 * invSigmaX) > 1.0) {
			rotatedx = new double[sizem][sizen][9];
			rotatedy = new double[sizem][sizen][9];
		//} else {
			//rotatedx = new double[sizem][sizen][1];
			//rotatedy = new double[sizem][sizen][1];
		//}

		for (y = 0; y < sizem; y++) {
			for (x = 0; x < sizen; x++) {
				rotatedx[y][x][0] = omegax2[y][x] * cosang + omegay2[y][x]
						* sinang;
				rotatedy[y][x][0] = -omegax2[y][x] * sinang + omegay2[y][x]
						* cosang;
			}
		}

		cnt = 0;
		for (sig1Index = 0; sig1Index <= 2; sig1Index++) {
			for (sig2Index = 0; sig2Index <= 2; sig2Index++) {
				for (y = 0; y < sizem; y++) {
					for (x = 0; x < sizen; x++) {
						rotatedx[y][x][cnt] = rotatedx[y][x][0] + 2.0
								* sig1[sig1Index] * cosang + 2.0
								* sig2[sig2Index] * sinang;
						rotatedy[y][x][cnt] = rotatedy[y][x][0] - 2.0
								* sig1[sig1Index] * sinang + 2.0
								* sig2[sig2Index] * cosang;
					}
				} // for (y = 0; y < sizem; y++)
				cnt++;
			}
		}

		absAmp = new double[amplitudes.length];
		maxAbsAmp = 0.0;
		for (i = 0; i < amplitudes.length; i++) {
			absAmp[i] = Math.abs(amplitudes[i]);
			if (absAmp[i] > maxAbsAmp) {
				maxAbsAmp = absAmp[i];
			}
		}
		threshMin = 0.01 * maxAbsAmp;
		omegaswt = new boolean[amplitudes.length];
		for (i = 0; i < amplitudes.length; i++) {
			if (absAmp[i] > threshMin) {
				omegaswt[i] = true;
			}
		}

		distCen = new double[sizem][sizen];
		denom = 2.0 * invSigmaX * invSigmaX;
		for (i = 0; i < amplitudes.length; i++) {
			if (omegaswt[i]) {
				carrier = omegas[i];
				cenx = carrier * cosang;
				ceny = carrier * sinang;
				offx = (-cenx * cosang) - ceny * sinang;
				offy = (cenx * sinang) - ceny * cosang;
				if ((Math.abs(carrier) + 3.0 * invSigmaX) > 1.0) {
					for (y = 0; y < sizem; y++) {
						for (x = 0; x < sizen; x++) {
							distCen[y][x] = Double.MAX_VALUE;
							for (j = 0; j < 9; j++) {
								diffx = rotatedx[y][x][j] - offx;
								diffy = rotatedy[y][x][j] - offy;
								if (diffx * diffx + diffy * diffy < distCen[y][x]) {
									distCen[y][x] = diffx * diffx + diffy
											* diffy;
								}
							}
						}
					}
				} // if ((Math.abs(carrier) + 3.0*invSigmaX) > 1.0)
				else {
					for (y = 0; y < sizem; y++) {
						for (x = 0; x < sizen; x++) {
							diffx = rotatedx[y][x][0] - offx;
							diffy = rotatedy[y][x][0] - offy;
							distCen[y][x] = diffx * diffx + diffy * diffy;
						}
					}
				} // else
				for (y = 0; y < sizem; y++) {
					for (x = 0; x < sizen; x++) {
						gaussianTerm = Math.exp(-distCen[y][x] / denom) / 2.0;
						hout[y][x] = hout[y][x] + amplitudes[i] * gaussianTerm;
					}
				}
			} // if (omegaswt[i])
		} // for (i = 0; i < amplitudes.length; i++)

		return;
	}
	
	private void testfreqz2() {
		int testNum = 8;
	    double a[][] = null;
	    int n1 = 64;
	    int n2 = 64;
	    Bessel bes;
	    double realArg;
    	double imaginaryArg = 0.0;
    	double initialOrder = 1.0;
    	int sequenceNumber = 1; // Number of sequential Bessel function orders calculated
    	double realResult[] = new double[1];
    	double imagResult[] = new double[1];
    	int[] nz = new int[1]; // number of components set to zero due to underflow
        int[] errorFlag = new int[1]; // zero if no error
        int y;
        int x;
        double root;
        double wc;
        double wc2;
        double wc1;
        double part2;
		switch (testNum) {
		case 1:
			// freqz2 has a zero imaginary component.
			// The real component is circular with zero at the center and -5.33333 at the periphery.
			// This corresponds to the frequency magnitude plot given by MATLAB
			a = new double[3][3];
			a[0][0] = 1.0/6.0;
			a[0][1] = 2.0/3.0;
			a[0][2] = 1.0/6.0;
			a[1][0] = 2.0/3.0;
			a[1][1] = -10.0/3.0;
			a[1][2] = 2.0/3.0;
			a[2][0] = 1.0/6.0;
			a[2][1] = 2.0/3.0;
			a[2][2] = 1.0/6.0;
			break;
		case 2:
			// 1/4(1 + cosw1)(1 + cosw2)
			// LPF with hout(0,0) = 1
			a = new double[3][3];
			a[0][0] = 1.0/16.0;
			a[0][1] = 2.0/16.0;
			a[0][2] = 1.0/16.0;
			a[1][0] = 2.0/16.0;
			a[1][1] = 4.0/16.0;
			a[1][2] = 2.0/16.0;
			a[2][0] = 1.0/16.0;
			a[2][1] = 2.0/16.0;
			a[2][2] = 1.0/16.0;	
			break;
		case 3:
			// 1/4(1 - cosw1)(1 - cosw2)
			// HPF with hout(0,0) = 0, 1 at 4 diagonal corners
			a = new double[3][3];
			a[0][0] = 1.0/16.0;
			a[0][1] = -2.0/16.0;
			a[0][2] = 1.0/16.0;
			a[1][0] = -2.0/16.0;
			a[1][1] = 4.0/16.0;
			a[1][2] = -2.0/16.0;
			a[2][0] = 1.0/16.0;
			a[2][1] = -2.0/16.0;
			a[2][2] = 1.0/16.0;	
			break;	
		case 4:
			// 1/3(1 + cosw1 + cosw2)
			// LPF with hout(0,0) = 1
			a = new double[3][3];
			a[0][0] = 0.0;
			a[0][1] = 1.0/6.0;
			a[0][2] = 0.0;
			a[1][0] = 1.0/6.0;
			a[1][1] = 1.0/3.0;
			a[1][2] = 1.0/6.0;
			a[2][0] = 0.0;
			a[2][1] = 1.0/6.0;
			a[2][2] = 0.0;	
			break;
		case 5:
			// hlp(n1,n2) = (wc/(2.0 * PI * sqrt(n1^2 + n2^2))*J1(wc*sqrt(n1^2 + n2^2)) lowpass
			// limit as x -> 0 of J1(x)/x = 1/2.
			// Sharp circular cutoff with values going from -0.088569 to 1.1030
			a = new double[101][101];
			wc = 0.5 * Math.PI;
			for (y = -50; y <= 50; y++) {
		    	for (x = -50; x <= 50; x++) {
		    		if ((x == 0) && (y == 0)) {
		    		    a[50][50] = (wc * wc)/(4.0 * Math.PI);
		    		}
		    		else {
			    		root = Math.sqrt(x*x + y*y);
			    		realArg = wc * root;
						bes = new Bessel(Bessel.BESSEL_J, realArg, imaginaryArg, initialOrder, Bessel.UNSCALED_FUNCTION,
			                    sequenceNumber, realResult, imagResult, nz, errorFlag);
			        	bes.run();
			        	if (errorFlag[0] != 0) {
			        	    displayError("Bessel_J error for realArg = " + realArg);
			        	    return;
			        	}
			        	a[y + 50][x + 50] = (wc/(2.0 * Math.PI * root))*realResult[0];
		    		}
		    	}
			}
			break;
		case 6:
			// hhp(n1, n2) = delta(n1,n2) - hlp(n1, n2) highpass
			// Sharp circular cutoff with values going from -0.1030 to 1.0885
			a = new double[101][101];
			wc = 0.5 * Math.PI;
			for (y = -50; y <= 50; y++) {
		    	for (x = -50; x <= 50; x++) {
		    		if ((x == 0) && (y == 0)) {
		    		    a[50][50] = 1 - (wc * wc)/(4.0 * Math.PI);
		    		}
		    		else {
			    		root = Math.sqrt(x*x + y*y);
			    		realArg = wc * root;
						bes = new Bessel(Bessel.BESSEL_J, realArg, imaginaryArg, initialOrder, Bessel.UNSCALED_FUNCTION,
			                    sequenceNumber, realResult, imagResult, nz, errorFlag);
			        	bes.run();
			        	if (errorFlag[0] != 0) {
			        	    displayError("Bessel_J error for realArg = " + realArg);
			        	    return;
			        	}
			        	a[y + 50][x + 50] = (-wc/(2.0 * Math.PI * root))*realResult[0];
		    		}
		    	}
			}
			break;
		case 7:
			// hbp(n1, n2) = (wc2/(2.0 * PI * sqrt(n1^2 + n2^2))*J1(wc2*sqrt(n1^2 + n2^2)) 
			//                - (wc1/(2.0 * PI * sqrt(n1^2 + n2^2))*J1(wc1*sqrt(n1^2 + n2^2))
			// bandpass with wc2 - wc1
			// Sharp bandpass with values between -0.0892 and 1.0895
			a = new double[101][101];
			wc2 = 0.6 * Math.PI;
			wc1 = 0.4 * Math.PI;
			for (y = -50; y <= 50; y++) {
		    	for (x = -50; x <= 50; x++) {
		    		if ((x == 0) && (y == 0)) {
		    		    a[50][50] = ((wc2 * wc2) - (wc1 * wc1))/(4.0 * Math.PI) ;
		    		}
		    		else {
			    		root = Math.sqrt(x*x + y*y);
			    		realArg = wc2 * root;
						bes = new Bessel(Bessel.BESSEL_J, realArg, imaginaryArg, initialOrder, Bessel.UNSCALED_FUNCTION,
			                    sequenceNumber, realResult, imagResult, nz, errorFlag);
			        	bes.run();
			        	if (errorFlag[0] != 0) {
			        	    displayError("Bessel_J error for realArg = " + realArg);
			        	    return;
			        	}
			        	part2 = (wc2/(2.0 * Math.PI * root))*realResult[0];
			        	realArg = wc1 * root;
			        	bes = new Bessel(Bessel.BESSEL_J, realArg, imaginaryArg, initialOrder, Bessel.UNSCALED_FUNCTION,
			                    sequenceNumber, realResult, imagResult, nz, errorFlag);
			        	bes.run();
			        	if (errorFlag[0] != 0) {
			        	    displayError("Bessel_J error for realArg = " + realArg);
			        	    return;
			        	}
			        	a[y + 50][x + 50] = part2 - (wc1/(2.0 * Math.PI * root))*realResult[0];
		    		}
		    	}
			}
			break;
		case 8:
			// hbs(n1, n2) = delta(n1, n2) - hbp(n1, n2)
			// Sharp bandstop with values going from -0.0895 to 1.0892
			a = new double[101][101];
			wc2 = 0.6 * Math.PI;
			wc1 = 0.4 * Math.PI;
			for (y = -50; y <= 50; y++) {
		    	for (x = -50; x <= 50; x++) {
		    		if ((x == 0) && (y == 0)) {
		    		    a[50][50] = 1.0 - ((wc2 * wc2) - (wc1 * wc1))/(4.0 * Math.PI) ;
		    		}
		    		else {
			    		root = Math.sqrt(x*x + y*y);
			    		realArg = wc2 * root;
						bes = new Bessel(Bessel.BESSEL_J, realArg, imaginaryArg, initialOrder, Bessel.UNSCALED_FUNCTION,
			                    sequenceNumber, realResult, imagResult, nz, errorFlag);
			        	bes.run();
			        	if (errorFlag[0] != 0) {
			        	    displayError("Bessel_J error for realArg = " + realArg);
			        	    return;
			        	}
			        	part2 = (wc2/(2.0 * Math.PI * root))*realResult[0];
			        	realArg = wc1 * root;
			        	bes = new Bessel(Bessel.BESSEL_J, realArg, imaginaryArg, initialOrder, Bessel.UNSCALED_FUNCTION,
			                    sequenceNumber, realResult, imagResult, nz, errorFlag);
			        	bes.run();
			        	if (errorFlag[0] != 0) {
			        	    displayError("Bessel_J error for realArg = " + realArg);
			        	    return;
			        	}
			        	a[y + 50][x + 50] = -part2 + (wc1/(2.0 * Math.PI * root))*realResult[0];
		    		}
		    	}
			}
			break;
		}
	    double hout[][] = new double[n2][n1];
	    double houtImag[][] = new double[n2][n1];
	    freqz2(hout, houtImag, a, n1, n2);
	    double buffer[] = new double[n2 * n1];
	    for (y = 0; y < n2; y++) {
	    	for (x = 0; x < n1; x++) {
	    		buffer[x + y * n1] = hout[y][x];
	    	}
	    }
	    int extents[] = new int[2];
	    extents[0] = n1;
	    extents[1] = n2;
	    ModelImage FIRfilter = new ModelImage(ModelStorageBase.DOUBLE, extents, "FIRfilter");
	    try {
	    	FIRfilter.importData(0, buffer, true);
	    }
	    catch (IOException e) {
	    	MipavUtil.displayError("IOException " + e + "on FIRfilter.importData(0, buffer, true)");
	    	return;
	    }
	    new ViewJFrameImage(FIRfilter);
	    
	    for (y = 0; y < n2; y++) {
	    	for (x = 0; x < n1; x++) {
	    		buffer[x + y * n1] = houtImag[y][x];
	    	}
	    }
	    ModelImage FIRImagfilter = new ModelImage(ModelStorageBase.DOUBLE, extents, "FIRImagfilter");
	    try {
	    	FIRImagfilter.importData(0, buffer, true);
	    }
	    catch (IOException e) {
	    	MipavUtil.displayError("IOException " + e + "on FIRImagfilter.importData(0, buffer, true)");
	    	return;
	    }
	    new ViewJFrameImage(FIRImagfilter);
	    return;
	}

	private void freqz2(double hout[][], double houtImag[][],
			double a[][], int n1, int n2) {
		double apad[][] = null;
		int y;
		int x;
		int yoff;
		int xoff;
		int w1off;
		double w1scale;
		int w2off;
		double w2scale;
		boolean useMesh = false;
		double FFTR[];
		double FFTI[];
		FFTUtility fft;
		double t1[][];
		double t2[][];
		double w1g[][];
		double w2g[][];
		int yout;
		int xout;
		double maxAbsImag;
		double acopy[][] = new double[a.length][a[0].length];
		for (y = 0; y < a.length; y++) {
			for (x = 0; x < a[0].length; x++) {
				acopy[y][x] = a[y][x];
			}
		}

		
		rot180(acopy);

		if ((a.length > n2) || (a[0].length > n1)) {
			useMesh = true;
		} else if (a.length != n2 || a[0].length != n1) {
			apad = new double[n2][n1];
			yoff = (int) Math.floor(n2 / 2.0)
					- (int) Math.floor(a.length / 2.0);
			xoff = (int) Math.floor(n1 / 2.0)
					- (int) Math.floor(a[0].length / 2.0);

			for (y = 0; y < a.length; y++) {
				for (x = 0; x < a[0].length; x++) {
					apad[y + yoff][x + xoff] = acopy[y][x];
				}
			}
		} // if (a.length != n2 || a[0].length != n1)
		else {
			apad = acopy;
		}

		if (!useMesh) {
			// Inverse fftshift
			ifftshift(apad);
			FFTR = new double[n2 * n1];
			FFTI = new double[n2 * n1];
			for (y = 0; y < n2; y++) {
				for (x = 0; x < n1; x++) {
					FFTR[x + y * n1] = apad[y][x];
				}
			}
			// For FFTUtility calls are scaled by 1/n for the inverse transform
			// In MATLAB ifft has a scaling of 1/M
			fft = new FFTUtility(FFTR, FFTI, n2, n1, 1, -1, FFTUtility.FFT);
			fft.setShowProgress(false);
			fft.run();
			fft.finalize();
			fft = null;
			fft = new FFTUtility(FFTR, FFTI, 1, n2, n1, -1, FFTUtility.FFT);
			fft.setShowProgress(false);
			fft.run();
			fft.finalize();
			fft = null;
			for (y = 0; y < n2; y++) {
				for (x = 0; x < n1; x++) {
					hout[y][x] = FFTR[x + y * n1];
					houtImag[y][x] = FFTI[x + y * n1];
				}
			}
			fftshift(hout);
			fftshift(houtImag);
		} // if (!useMesh)
		else { // useMesh
			w1g = new double[n2][n1];
			w1off = (int) Math.floor(n1 / 2.0);
			w1scale = 2.0 / n1;
			for (y = 0; y < n2; y++) {
			    for (x = 0; x < n1; x++) {
				     w1g[y][x] = (x - w1off) * w1scale;
			    }
			}

			w2g = new double[n2][n1];
			w2off = (int) Math.floor(n2 / 2.0);
			w2scale = 2.0 / n2;
			for (x = 0; x < n1; x++) {
				for (y = 0; y < n2; y++) {
					w2g[y][x] = (y - w2off) * w2scale;
				}
			}

			t1 = new double[a.length][a[0].length];
			t2 = new double[a.length][a[0].length];
			if ((a[0].length % 2) == 1) {
				for (y = 0; y < a.length; y++) {
					for (x = 0; x < a[0].length; x++) {
						t1[y][x] = -acopy[0].length / 2.0 + 1.0 / 2.0 + x;
					}
				}
			} else {
				for (y = 0; y < a.length; y++) {
					for (x = 0; x < a[0].length; x++) {
						t1[y][x] = -acopy[0].length / 2.0 + x;
					}
				}
			}
			if ((a.length % 2) == 1) {
				for (x = 0; x < a[0].length; x++) {
					for (y = 0; y < a.length; y++) {
						t2[y][x] = -acopy.length / 2.0 + 1.0 / 2.0 + y;
					}
				}
			} else {
				for (x = 0; x < a[0].length; x++) {
					for (y = 0; y < a.length; y++) {
						t2[y][x] = -acopy.length / 2.0 + y;
					}
				}
			}

			for (yout = 0; yout < n2; yout++) {
				for (xout = 0; xout < n1; xout++) {
					hout[yout][xout] = 0.0;
					houtImag[yout][xout] = 0.0;
					for (y = 0; y < a.length; y++) {
						for (x = 0; x < a[0].length; x++) {
							hout[yout][xout] += Math
									.cos(Math.PI
											* (w1g[yout][xout] * t1[y][x] + w2g[yout][xout]
													* t2[y][x])) * acopy[y][x];
							houtImag[yout][xout] -= Math
									.sin(Math.PI
											* (w1g[yout][xout] * t1[y][x] + w2g[yout][xout]
													* t2[y][x])) * acopy[y][x];
						}
					}
				}
			} // for (yout = 0; yout < n2; yout++)
		}

		maxAbsImag = 0.0;
		for (y = 0; y < n2; y++) {
			for (x = 0; x < n1; x++) {
				if (Math.abs(houtImag[y][x]) > maxAbsImag) {
					maxAbsImag = Math.abs(houtImag[y][x]);
				}
			}
		}

		if (maxAbsImag < Math.sqrt(epsilon)) {
			for (y = 0; y < n2; y++) {
				for (x = 0; x < n1; x++) {
					houtImag[y][x] = 0.0;
				}
			}
		}

		return;

	}

	private void rot180(double mtx[][]) {
		int sizem = mtx.length;
		int sizen = mtx[0].length;
		int y;
		int x;
		double temp[][] = new double[sizem][sizen];
		for (y = 0; y < sizem; y++) {
			for (x = 0; x < sizen; x++) {
				temp[y][x] = mtx[sizem - 1 - y][sizen - 1 - x];
			}
		}
		for (y = 0; y < sizem; y++) {
			for (x = 0; x < sizen; x++) {
				mtx[y][x] = temp[y][x];
			}
		}
	}

	private double[][] T1z2b_time_resp(final double omegas[],
			final double amplitudes[], final double amplitudesImag[],
			final double filterAngle, final double sigmaX) {
		int mx;
		int grx[][];
		int gry[][];
		int y;
		int x;
		int sz;
		double cosa;
		double sina;
		double gaussian[][];
		double sum;
		int namp;
		int cen;
		double res[][];
		double rotx[][];
		int k;
		double ampPos;
		double ampPosImag;
		double ampPosAbs;
		double ampNeg;
		double ampNegImag;
		double omega;
		double ampDiff;
		double ampDiffImag;
		double ampDiffAbs;

		mx = (int) Math.ceil(Math.max(5 * sigmaX, 5));
		sz = 2 * mx + 1;
		grx = new int[sz][sz];
		gry = new int[sz][sz];
		for (y = 0; y < sz; y++) {
			for (x = 0; x < sz; x++) {
				grx[y][x] = x - mx;
			}
		}
		for (x = 0; x < sz; x++) {
			for (y = 0; y < sz; y++) {
				gry[y][x] = y - mx;
			}
		}

		cosa = Math.cos(filterAngle);
		sina = Math.sin(filterAngle);
		gaussian = new double[sz][sz];
		for (y = 0; y < sz; y++) {
			for (x = 0; x < sz; x++) {
				gaussian[y][x] = Math.exp(-(grx[y][x] * grx[y][x] + gry[y][x]
						* gry[y][x])
						/ (2.0 * sigmaX * sigmaX));
			}
		}
		sum = 0.0;
		for (y = 0; y < sz; y++) {
			for (x = 0; x < sz; x++) {
				sum += gaussian[y][x];
			}
		}
		for (y = 0; y < sz; y++) {
			for (x = 0; x < sz; x++) {
				gaussian[y][x] = gaussian[y][x] / sum;
			}
		}

		rotx = new double[sz][sz];
		for (y = 0; y < sz; y++) {
			for (x = 0; x < sz; x++) {
				rotx[y][x] = grx[y][x] * cosa + gry[y][x] * sina;
			}
		}

		namp = (amplitudes.length - 1) / 2;
		cen = (amplitudes.length + 1) / 2;
		res = new double[sz][sz];
		for (y = 0; y < sz; y++) {
			for (x = 0; x < sz; x++) {
				res[y][x] = gaussian[y][x] * amplitudes[cen - 1] / 2.0;
			}
		}

		for (k = 1; k <= namp; k++) {
			ampPos = amplitudes[cen + k - 1];
			ampPosImag = amplitudesImag[cen + k - 1];
			ampNeg = amplitudes[cen - k - 1];
			ampNegImag = amplitudesImag[cen - k - 1];
			omega = Math.PI * omegas[cen + k - 1];
			ampPosAbs = Math.sqrt(ampPos * ampPos + ampPosImag * ampPosImag);
			if (ampPosAbs > 1.0E-7) {
				ampDiff = ampPos - ampNeg;
				ampDiffImag = ampPosImag - ampNegImag;
				ampDiffAbs = Math.sqrt(ampDiff * ampDiff + ampDiffImag
						* ampDiffImag);
				if ((ampDiffAbs / ampPosAbs) < 0.001) {
					for (y = 0; y < sz; y++) {
						for (x = 0; x < sz; x++) {
							res[y][x] = res[y][x] + ampPos * gaussian[y][x]
									* Math.cos(omega * rotx[y][x]);
						}
					}
				} // if ((ampDiffAbs/ampPosAbs) < 0.001)
				else {
					for (y = 0; y < sz; y++) {
						for (x = 0; x < sz; x++) {
							res[y][x] = res[y][x] - ampPosImag * gaussian[y][x]
									* Math.sin(omega * rotx[y][x]);
						}
					}
				}
			} // if (ampPosAbs > 1.0E-7)
		} // for (k = 1; k <= namp; k++)

		return res;
	}

	private void T1z2a_convert_filter(double omegasOut[],
			double amplitudesOut[], double amplitudesOutImag[],
			final double omegas[], final double amplitudes[],
			final double filterAngle, final double sigmaX, final int conj) {
		int sz0;
		double amplitudesOriginal[];
		int i;
		double omegas2[];
		double amplitudesEven[];
		double amplitudesOddImag[];
		double amplitudesUns[] = null;
		double amplitudesUnsImag[] = null;
		double amplitudesMult[] = null;
		double amplitudesMultImag[] = null;
		double A[];
		double B[];
		double C[];
		int sz1;

		sz0 = amplitudes.length - 1;

		amplitudesOriginal = new double[amplitudes.length + sz0];
		for (i = 0; i < amplitudes.length; i++) {
			amplitudesOriginal[i] = amplitudes[i];
		}
		omegas2 = new double[2 * omegas.length - 1];
		for (i = 0; i < omegas.length; i++) {
			omegas2[i] = omegas[i];
		}
		for (i = 0; i < omegas.length - 1; i++) {
			omegas2[omegas.length + i] = omegas[i + 1]
					+ omegas[omegas.length - 1];
		}
		amplitudesEven = new double[2 * amplitudesOriginal.length - 1];
		for (i = 0; i < amplitudesOriginal.length - 1; i++) {
			amplitudesEven[i] = amplitudesOriginal[amplitudesOriginal.length
					- 1 - i];
		}
		for (i = 0; i < amplitudesOriginal.length; i++) {
			amplitudesEven[i + amplitudesOriginal.length - 1] = amplitudesOriginal[i];
		}
		amplitudesOddImag = new double[2 * amplitudesOriginal.length - 1];
		for (i = 0; i < amplitudesOriginal.length - 1; i++) {
			amplitudesOddImag[i] = -amplitudesOriginal[amplitudesOriginal.length
					- 1 - i];
		}
		for (i = 0; i < amplitudesOriginal.length; i++) {
			amplitudesOddImag[i + amplitudesOriginal.length - 1] = amplitudesOriginal[i];
		}
		for (i = 0; i < omegas2.length - 1; i++) {
			omegasOut[i] = -omegas2[omegas2.length - 1 - i];
		}
		for (i = 0; i < omegas2.length; i++) {
			omegasOut[i + omegas2.length - 1] = omegas2[i];
		}

		switch (conj) {
		case 0:
			//amplitudesUns = new double[amplitudesEven.length];
			//for (i = 0; i < amplitudesUns.length; i++) {
				//amplitudesUns[i] = amplitudesEven[i];
			//}
			//amplitudesMult = new double[amplitudesEven.length];
			//for (i = 0; i < amplitudesMult.length; i++) {
				//amplitudesMult[i] = amplitudesEven[i];
			//}
			//break;
		case 1:
			amplitudesUns = new double[amplitudesEven.length];
			for (i = 0; i < amplitudesUns.length; i++) {
				amplitudesUns[i] = amplitudesEven[i];
			}
			break;
		case 2:
			amplitudesUnsImag = new double[amplitudesOddImag.length];
			for (i = 0; i < amplitudesUnsImag.length; i++) {
				amplitudesUnsImag[i] = amplitudesOddImag[i];
			}
			break;
		case 11:
			amplitudesUns = new double[amplitudesEven.length];
			for (i = 0; i < amplitudesUns.length; i++) {
				amplitudesUns[i] = amplitudesEven[i];
			}
			amplitudesMult = new double[amplitudesEven.length];
			for (i = 0; i < amplitudesMult.length; i++) {
				amplitudesMult[i] = amplitudesEven[i];
			}
			break;
		case 12:
			amplitudesUnsImag = new double[amplitudesOddImag.length];
			for (i = 0; i < amplitudesUnsImag.length; i++) {
				amplitudesUnsImag[i] = amplitudesOddImag[i];
			}
			amplitudesMult = new double[amplitudesEven.length];
			for (i = 0; i < amplitudesMult.length; i++) {
				amplitudesMult[i] = amplitudesEven[i];
			}
			break;
		case 22:
			amplitudesUnsImag = new double[amplitudesOddImag.length];
			for (i = 0; i < amplitudesUnsImag.length; i++) {
				amplitudesUnsImag[i] = amplitudesOddImag[i];
			}
			amplitudesMultImag = new double[amplitudesOddImag.length];
			for (i = 0; i < amplitudesMultImag.length; i++) {
				amplitudesMultImag[i] = amplitudesOddImag[i];
			}
			break;
		} // switch (conj)

		switch (conj) {
		case 0:
			//amplitudesOut = new double[] { 2 };
			//amplitudesOutImag = new double[1];
			//omegasOut = new double[] { 0 };
			break;
		case 1:
		case 2:
			if (amplitudesUns != null) {
				for (i = 0; i < amplitudesOut.length; i++) {
				    amplitudesOut[i] = amplitudesUns[i];
				}
			} else {
				for (i = 0; i < amplitudesOutImag.length; i++) {
				    amplitudesOutImag[i] = amplitudesUnsImag[i];
				}
			}
			break;
		default:
			// Estimate the fourier series coefficients of filter^2 =
			// convolution of 1-d filters
			// Both A and B are the same length
			if (amplitudesUns != null) {
				sz1 = amplitudesUns.length;
				A = amplitudesUns;
			} else {
				sz1 = amplitudesUnsImag.length;
				A = amplitudesUnsImag;
			}
			if (amplitudesMult != null) {
				B = amplitudesMult;
			} else {
				B = amplitudesMultImag;
			}
			C = new double[sz1];
			conv(A, B, C);
			for (i = 0; i < sz1; i++) {
				C[i] = C[i] / 2.0;
			}
			if ((amplitudesUns != null) && (amplitudesMult != null)) {
				for (i = 0; i < amplitudesOut.length; i++) {
				    amplitudesOut[i] = C[i];
				}
			} else if ((amplitudesUnsImag != null)
					&& (amplitudesMultImag != null)) {
				for (i = 0; i < sz1; i++) {
					amplitudesOut[i] = -C[i];
				}
			} else {
				for (i = 0; i < amplitudesOutImag.length; i++) {
				    amplitudesOutImag[i] = C[i];
				}
			}
		} // switch (conj)
	}

	private void filterbank_DCA_2D(double omegas[][], double amplitudes[][],
			double filterAngle[], double sigmaX[],
			final int nscales, final String filterType, final int ndirs,
			final double sig2omega, final double radianStart,
			final double radianEnd) {
		// Put central frequencies on logarithmic scale
		double factr;
		final double radianPerPixel[] = new double[nscales];
		int i;
		int scale;
		double rpp;
		double sigmaXTemp;
		int angle;
		int counter;
		double omegasInit[];
		double amplitudesInit[];
		double temp[];
		int dim;
		int dim2;
		int j;
		double val;
		double sum;
		boolean exclude[];
		int excludeCount;

		factr = Math.exp(Math.log(radianEnd / radianStart) / (nscales - 1));
		for (i = 0; i < nscales; i++) {
			radianPerPixel[i] = radianStart * Math.pow(factr, i);
		}

		for (scale = 1; scale <= nscales; scale++) {
			rpp = radianPerPixel[scale - 1];
			sigmaXTemp = sig2omega / rpp;
			if (filterType.equals("edge")) {
				rpp = rpp / 4.0;
			}
			for (angle = 1; angle <= ndirs; angle++) {
				counter = (scale - 1) * ndirs + angle;
				sigmaX[counter - 1] = sigmaXTemp;
				filterAngle[counter - 1] = Math.PI * (angle - 1) / ndirs;

				// Fields used for every single gabor filter
				if (filterType.equals("texture")) {
					omegas[counter - 1] = new double[] { 0, rpp };
					amplitudes[counter - 1] = new double[] { 0.0, 1.0 };
				} else { // filterType.equals("edge"))
					dim = 0;
					for (val = 0; val <= 1.0; val += rpp) {
						dim++;
					}
					temp = new double[dim];
					for (val = 0, j = 0; val <= 1.0; val += rpp) {
						temp[j++] = val;
					}
					excludeCount = 0;
					exclude = new boolean[dim];
					for (j = 0; j < dim-1; j += 2) {
						exclude[j] = true;
						excludeCount++;
					}
					dim2 = dim - excludeCount;
					omegasInit  = new double[dim2];
					for (i = 0,j = 0; j < dim; j++) {
						if (!exclude[j]) {
							omegasInit[i++] = temp[j];
						}
					}
					amplitudesInit = new double[dim2];
					for (j = 0; j < dim2; j++) {
						amplitudesInit[j] = 1.0 / Math.max(omegasInit[j],
								epsilon);
					}
					omegas[counter - 1] = new double[dim2 + 1];
					amplitudes[counter - 1] = new double[dim2 + 1];
					sum = 0.0;
					for (j = 1; j < dim2 + 1; j++) {
						omegas[counter - 1][j] = omegasInit[j - 1];
						amplitudes[counter - 1][j] = amplitudesInit[j - 1];
						sum += amplitudes[counter - 1][j]
								* amplitudes[counter - 1][j];
					}
					sum = Math.sqrt(sum);
					for (j = 1; j < dim2 + 1; j++) {
						amplitudes[counter - 1][j] = amplitudes[counter - 1][j]
								/ sum;
					}
				} // else
			} // for (angle = 1; angle <= ndirs; angle++)
		} // for (scale = 1; scale <= nscales; scale++)
	}
}
