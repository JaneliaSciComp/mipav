package gov.nih.mipav.model.algorithms;

import gov.nih.mipav.model.algorithms.utilities.AlgorithmChangeType;
import gov.nih.mipav.model.algorithms.utilities.AlgorithmRGBtoGray;
import gov.nih.mipav.model.file.*;
import gov.nih.mipav.model.structures.*;
import gov.nih.mipav.view.*;

import java.awt.Color;
import java.io.*;

  /**
   * This is a port of the file AutoSeedWatershed.cpp which calls openCV written by Ravimal Bandara.  His web site is
   * titled Image Segmentation using Unsupervised Watershed Algorithm with an Over-segmentation Reduction Technique.
   * This code is written for 2D color images.  In response to the question:
   * Would it be possible to modify your code for use on black and white images? What changes would be necessary?
   * Ravimal Bandara responded:
   * Yes you can by creating a gray histogram instead of Hues Saturation histogram. But I am not sure about the accuracy
   * due to the grayscale histograms are less discriminative compared to the colour histogram.
   * His code is licensed under the Code Project Open License (CPOL).
   * @author ilb
   *
   */


public class AlgorithmAutoSeedWatershed extends AlgorithmBase {

    //~ Static fields/initializers -------------------------------------------------------------------------------------
	//~ Instance fields ------------------------------------------------------------------------------------------------
	private int segmentNumber;
	
	private ModelImage grayImage;
	
	private ModelImage thresholdImage;
	
	private ModelImage distTransformed;
	
	private float scaleX;
	
	private float scaleY;
	
	private boolean error = false;
	
	private ViewUserInterface UI = ViewUserInterface.getReference();

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Constructs new watershed algorithm.
     *
     * @param  destImg  Image model where result image is to stored
     * @param  srcImg   Source image model
     * @param  scaleX
     * @param  scaleY
     */
    public AlgorithmAutoSeedWatershed(ModelImage destImg, ModelImage srcImg,
    		float scaleX, float scaleY) {

        super(destImg, srcImg);
        this.scaleX = scaleX;
        this.scaleY = scaleY;
    }

    

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * Prepares this class for destruction.
     */
    public void finalize() {
        destImage = null;
        srcImage = null;
        super.finalize();
    }

    /**
     * Starts the program.
     */
    public void runAlgorithm() {

        if (srcImage == null) {
            displayError("Source Image is null");

            return;
        }

        if (destImage == null) {
            displayError("Destination Image is null");

            return;
        }

        fireProgressStateChanged("Watershed ...");
        
        watershedSegment();
        if (error) {
        	setCompleted(false);
        	return;
        }
        
        segmentNumber = (int)grayImage.getMax();
        if (error) {
        	setCompleted(false);
        }
        else {
            setCompleted(true);
        }
        return;
    }
    
    private void watershedSegment() {
    	// Convert the image to grayscale with the computer graphics standard
    	// Y = 0.299 * R + 0.587 * G + 0.114 * B
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
			  AlgorithmRGBtoGray gAlgo;
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
				  redValue = 0.2989f;
				  greenValue = 0.5870f;
				  blueValue = 0.1140f;
			  }
			  maxR = (float) srcImage.getMaxR();
			  maxG = (float) srcImage.getMaxG();
			  maxB = (float) srcImage.getMaxB();
			  if (srcImage.getType() == ModelStorageBase.ARGB) {
                  grayImage = new ModelImage(ModelImage.UBYTE, srcImage.getExtents(),
                                               (srcImage.getImageName() + "Gray"));
              } else if (srcImage.getType() == ModelStorageBase.ARGB_USHORT) {
                  grayImage = new ModelImage(ModelImage.USHORT, srcImage.getExtents(),
                                               (srcImage.getImageName() + "Gray"));
              } else if (srcImage.getType() == ModelStorageBase.ARGB_FLOAT) {
                  grayImage = new ModelImage(ModelImage.FLOAT, srcImage.getExtents(),
                                               (srcImage.getImageName() + "Gray"));
              }

              // get some important information from imageA and put it in
              // the result image
              if ((srcImage.getFileInfo()[0]).getFileFormat() == FileUtility.DICOM)  {
              	FileInfoDicom fileInfoBuffer; // buffer of type DICOM
              	for (int n = 0; n < srcImage.getFileInfo().length; n++) {
              		fileInfoBuffer = (FileInfoDicom) srcImage.getFileInfo(n).clone(); // copy into buffer
              		fileInfoBuffer.getTagTable().setValue("0028,0002", new Short((short) 1), 2); // samples per pixel
              		fileInfoBuffer.getTagTable().setValue("0028,0004", new String("MONOCHROME2"), 11); // photometric
              		fileInfoBuffer.setDataType(grayImage.getType());
              		grayImage.setFileInfo(fileInfoBuffer, n);
              	}
              }
              else {
	                for (int n = 0; n < srcImage.getFileInfo().length; n++) {
	                    FileInfoBase fInfoBase = (FileInfoBase) (srcImage.getFileInfo(n).clone());
	                    fInfoBase.setDataType(grayImage.getType());
	                    grayImage.setFileInfo(fInfoBase, n);
	                }
              }
			  gAlgo = new AlgorithmRGBtoGray(grayImage, srcImage,
					  redValue, greenValue, blueValue, thresholdAverage,
					  threshold, intensityAverage, equalRange, minR, maxR,
					  minG, maxG, minB, maxB);
			  gAlgo.run();
			  gAlgo.finalize();
			  //new ViewJFrameImage(grayImage, null, new Dimension(610, 200));
		  } // if (srcImage.isColorImage())
    	else {
    		grayImage = srcImage;
    	}
    	// Threshold the image binary inverted such that dst(x,y) = 0 if src(x,y) > threshold, maxVal otherwise
    	// Use Otsu's binarization to find the optimal threshold value.  Otsu's algorithm assumes a bimodal image
    	// and finds the optimal threshold value in the middle of 2 peaks by minimizing the weighted within-class
    	// variance.  Output image is boolean with values of 0 and 1.
    	// Calculate histogram
    	int[] dimExtents = new int[1];
    	dimExtents[0] = 256;
    	ModelHistogram histogram = new ModelHistogram(ModelStorageBase.INTEGER, dimExtents);

        AlgorithmHistogram histoAlgoA = new AlgorithmHistogram(histogram, grayImage, true);
        histoAlgoA.setRunningInSeparateThread(false);
        histoAlgoA.run();
        int maxBin = histogram.getOtsuThreshold();
        double dif = grayImage.getMax() - grayImage.getMin();
    	double factor = dif / histogram.getExtents()[0];
    	histoAlgoA.finalize();
    	histoAlgoA = null;
    	float thresVal = (float)((maxBin * factor) + grayImage.getMin());
    	float[] thresholds = new float[2];
    	thresholds[0] = thresVal;
    	thresholds[1] = (float) grayImage.getMax();
    	thresholdImage = new ModelImage(ModelImage.BOOLEAN, srcImage.getExtents(), srcImage.getImageName()+ "threshold");
    	(thresholdImage.getFileInfo(0)).setModality(FileInfoBase.OTHER);
    	// fillValue not used
    	float fillValue = 0.0f;
    	int outputType = AlgorithmThresholdDual.BINARY_TYPE;
    	boolean wholeImage = true;
    	boolean isInverse = true;
    	AlgorithmThresholdDual thresholdAlgo = new AlgorithmThresholdDual(thresholdImage, grayImage, thresholds, fillValue, outputType,
                wholeImage, isInverse);
    	thresholdAlgo.run();
    	FileInfoBase[] fileInfo = thresholdImage.getFileInfo();
        fileInfo[0].setModality(FileInfoBase.OTHER);
        fileInfo[0].setFileDirectory(grayImage.getFileInfo()[0].getFileDirectory());

        fileInfo[0].setEndianess(grayImage.getFileInfo()[0].getEndianess());
        fileInfo[0].setUnitsOfMeasure(grayImage.getFileInfo()[0].getUnitsOfMeasure());
        fileInfo[0].setResolutions(grayImage.getFileInfo()[0].getResolutions());
        fileInfo[0].setExtents(thresholdImage.getExtents());
        fileInfo[0].setMax(thresholdImage.getMax());
        fileInfo[0].setMin(thresholdImage.getMin());
        fileInfo[0].setImageOrientation(grayImage.getImageOrientation());
        fileInfo[0].setAxisOrientation(grayImage.getFileInfo()[0].getAxisOrientation());
        fileInfo[0].setOrigin(grayImage.getFileInfo()[0].getOrigin());
        fileInfo[0].setPixelPadValue(grayImage.getFileInfo()[0].getPixelPadValue());
        fileInfo[0].setPhotometric(grayImage.getFileInfo()[0].getPhotometric());
        //new ViewJFrameImage(thresholdImage, null, new Dimension(610, 200));
        thresholdAlgo.finalize();
        thresholdAlgo = null;
        
        // Perform morphological open which is an erode followed by a dilate
        // The opencv uses a 9 by 9 square of signed 8-bit ones one channel with the anchor point at
        // 4,4 which places it in the center of the kernel since the coordinates
        // go from 0 to 8.  2 iterations.
        // OpenCV default seems to be 8-connected rather than 4-connected.
        int kernel = AlgorithmMorphology2D.CONNECTED80;
        float circleDiameter = 0.0f;
        int itersD = 2;
        int itersE = 2;
        AlgorithmMorphology2D openAlgo2D = new AlgorithmMorphology2D(thresholdImage, kernel, circleDiameter, AlgorithmMorphology2D.OPEN,
                itersD, itersE, 0, 0, wholeImage);
        openAlgo2D.run();
        openAlgo2D.finalize();
        openAlgo2D = null;
        
        distTransformed = new ModelImage(ModelStorageBase.UBYTE, srcImage.getExtents(), srcImage.getImageName() + "_distTransformed");
        // The input is an image with feature pixels with value 0 and non-feature pixels with nonzero values.
        // The function labels every non-feature pixel in the output image with a distance to the closest feature pixel.
        int pix, i;
        int xDim = srcImage.getExtents()[0];
        int yDim = srcImage.getExtents()[1];
        int sliceSize = xDim * yDim;
        int x1, x2, y1, y2;
        float distance;
        float dist;

        float[] distBuffer;
        int[] pointArray;
        int pointIndex = 0;
        byte imgBuffer[] = new byte[sliceSize];
        try {
        	thresholdImage.exportData(0, sliceSize, imgBuffer);
        }
        catch(IOException e) {
        	displayError("IOException " + e + "on thresholdImage.exportData(0, sliceSize, imgBuffer)");
        	error = true;
        	return;
        }

        try {
            pointArray = new int[sliceSize / 2];
            distBuffer = new float[sliceSize];
        } catch (OutOfMemoryError e) {
            displayError("distanceMap: Out of memory");
            error = true;

            return;
        }

        
        int end = sliceSize - xDim - 1;
        for (pix = 0; (pix < sliceSize) && !threadStopped; pix++) {

            if ((pix < xDim) && (imgBuffer[pix] == 0)) {
                pointArray[pointIndex] = pix;
                pointIndex++;
            } else if (((pix % xDim) == (xDim - 1)) && (imgBuffer[pix] == 0)) {
                pointArray[pointIndex] = pix;
                pointIndex++;
            } else if (((pix % xDim) == 0) && (imgBuffer[pix] == 0)) {
                pointArray[pointIndex] = pix;
                pointIndex++;
            } else if ((pix > (sliceSize - xDim)) && (imgBuffer[pix] == 0)) {
                pointArray[pointIndex] = pix;
                pointIndex++;
            } else if ((pix > (xDim + 1)) && (pix < end) && (imgBuffer[pix] == 0) &&
                           ((imgBuffer[pix - xDim] != 0) || (imgBuffer[pix + 1] != 0) || (imgBuffer[pix + xDim] != 0) ||
                                (imgBuffer[pix - 1] != 0))) {
                pointArray[pointIndex] = pix;
                pointIndex++;
            }
        }

        float xRes = srcImage.getFileInfo(0).getResolutions()[0];
        float xResSquared = xRes * xRes;
        float yRes = srcImage.getFileInfo(0).getResolutions()[1];
        float yResSquared = yRes * yRes;
        float distMax = 0.0f;

        for (pix = 0; pix < sliceSize; pix++) {

            if (imgBuffer[pix] > 0) {
                distance = Float.MAX_VALUE;
                x1 = pix % xDim;
                y1 = pix / xDim;

                for (i = 0; i < pointIndex; i++) {
                    x2 = pointArray[i] % xDim;
                    y2 = pointArray[i] / xDim;
                    dist = ((x2 - x1) * (x2 - x1) * xResSquared) + ((y2 - y1) * (y2 - y1) * yResSquared);

                    if (dist < distance) {
                        distance = dist;
                    }
                }

                distBuffer[pix] = (float) Math.sqrt(distance);
                if (distBuffer[pix] > distMax) {
                	distMax = distBuffer[pix];
                }
            }
            
        }
        
        // Normalize to go between 0.0 and 1.0
        // Threshold the transformed image to obtain markers for the watershed
        // if less than or equal to 0.1, set to 0
        // Otherwise set to 255.0.
        for (pix = 0; pix  < sliceSize; pix++) {
        	distBuffer[pix] = distBuffer[pix]/distMax;
        	if (distBuffer[pix] <= 0.1) {
        		distBuffer[pix] = 0.0f;
        	}
        	else {
        		distBuffer[pix] = 255.0f;
        	}
        }
        

        // Thresholded distance transformation image
        try {
            distTransformed.importData(0, distBuffer, true);
        } catch (IOException e) {
            displayError("IOException " + e + " on distTransformed.importData(0, distBuffer, true)");
            error = true;

            return;
        } 
        //new ViewJFrameImage(distTransformed);
        
        // Calculate the contours of the markers
        AlgorithmMorphology2D idObjectsAlgo2D;
        int method = AlgorithmMorphology2D.ID_OBJECTS;
        
        idObjectsAlgo2D = new AlgorithmMorphology2D(distTransformed, 0, 0, method, 0, 0, 0, 0, wholeImage);
        idObjectsAlgo2D.setMinMax(1, Integer.MAX_VALUE);
        idObjectsAlgo2D.run();
        idObjectsAlgo2D.finalize();
        idObjectsAlgo2D = null;
    	
    	distTransformed.calcMinMax();
        final AlgorithmVOIExtraction VOIExtractionAlgo = new AlgorithmVOIExtraction(distTransformed);

        VOIExtractionAlgo.run();
        VOIVector kVOIs = distTransformed.getVOIs();
        for (i = 0; i < kVOIs.size(); i++ )
        {
            VOI kCurrentGroup = kVOIs.get(i);
            kCurrentGroup.setAllActive(true);
        }
        //distTransformed.groupVOIs();
        kVOIs = distTransformed.getVOIs();
        //if (voiFieldName != null) {
            //((VOI)kVOIs.get(0)).setName(voiFieldName);
        //}
        // MIPAV AlgorithmWatershed does not handle color
        grayImage.setVOIs(kVOIs);
        new ViewJFrameImage(grayImage);
        distTransformed.disposeLocal();
        distTransformed = null;
        ModelImage gmImage = null;
        float sigmas[] = new float[2];
        sigmas[0] = scaleX;
        sigmas[1] = scaleY;
        AlgorithmWatershed watershedAlgo = new AlgorithmWatershed(destImage, grayImage, gmImage, sigmas, null);
        watershedAlgo.run();
        watershedAlgo.finalize();
        watershedAlgo = null;
        FileInfoBase fileInfo2[] = destImage.getFileInfo();
        fileInfo2[0].setModality(grayImage.getFileInfo()[0].getModality());
        fileInfo2[0].setFileDirectory(grayImage.getFileInfo()[0].getFileDirectory());
        fileInfo2[0].setEndianess(grayImage.getFileInfo()[0].getEndianess());
        fileInfo2[0].setUnitsOfMeasure(grayImage.getFileInfo()[0].getUnitsOfMeasure());
        fileInfo2[0].setResolutions(grayImage.getFileInfo()[0].getResolutions());
        fileInfo2[0].setExtents(destImage.getExtents());
        fileInfo2[0].setMax(destImage.getMax());
        fileInfo2[0].setMin(destImage.getMin());
        fileInfo2[0].setImageOrientation(grayImage.getImageOrientation());
        fileInfo2[0].setAxisOrientation(grayImage.getFileInfo()[0].getAxisOrientation());
        fileInfo2[0].setOrigin(grayImage.getFileInfo()[0].getOrigin());
        fileInfo2[0].setPixelPadValue(grayImage.getFileInfo()[0].getPixelPadValue());
        fileInfo2[0].setPhotometric(grayImage.getFileInfo()[0].getPhotometric());
    } // watershedSegment
    
    private void mergeSegments() {
        if (srcImage.isColorImage()) {
        	ModelImage colorImage;
        	if ((srcImage.getFileInfo()[0].getDataType() == ModelStorageBase.ARGB_USHORT) ||
        		(srcImage.getFileInfo()[0].getDataType() == ModelStorageBase.ARGB_FLOAT)) {
        	    colorImage = new ModelImage(ModelStorageBase.ARGB, srcImage.getExtents(), "ColorImage");
        	    boolean image25D = true;
        	    AlgorithmChangeType changeTypeAlgo = new AlgorithmChangeType(colorImage, srcImage, srcImage.getMin(),
        	    		srcImage.getMax(), 0, 255, image25D);
        	    changeTypeAlgo.run();
          		changeTypeAlgo.finalize();
          		changeTypeAlgo = null;
          		FileInfoBase[] fileInfo = colorImage.getFileInfo();
                fileInfo[0].setModality(srcImage.getFileInfo()[0].getModality());
                fileInfo[0].setFileDirectory(srcImage.getFileInfo()[0].getFileDirectory());
                fileInfo[0].setUnitsOfMeasure(srcImage.getFileInfo()[0].getUnitsOfMeasure());
                fileInfo[0].setResolutions(srcImage.getFileInfo()[0].getResolutions());
                fileInfo[0].setExtents(colorImage.getExtents());
                fileInfo[0].setMax(colorImage.getMax());
                fileInfo[0].setMin(colorImage.getMin());
                fileInfo[0].setImageOrientation(srcImage.getImageOrientation());
                fileInfo[0].setAxisOrientation(srcImage.getFileInfo()[0].getAxisOrientation());
                fileInfo[0].setOrigin(srcImage.getFileInfo()[0].getOrigin());
        	} // if ((srcImage.getFileInfo()[0].getDataType() == ModelStorageBase.ARGB_USHORT) ||
        	else {
        		colorImage = srcImage;
        	}
        	// Hue, saturation, and brightness all go from 0.0 to 1.0
        	int hueBins = 35;
        	int saturationBins = 30;
        	int hist[][][] = new int[segmentNumber][hueBins][saturationBins];
        	float[] hsb = new float[3];
        	float hue;
        	float saturation;
        	int i;
        	int sliceSize = srcImage.getExtents()[0] * srcImage.getExtents()[1];
        	short red[] = new short[sliceSize];
        	short green[] = new short[sliceSize];
        	short blue[] = new short[sliceSize];
        	int segment[] = new int[sliceSize];
        	int hBin;
        	int sBin;
        	int segmentTotal[] = new int[segmentNumber];
        	int h;
        	int s;
        	double normalizedHist[][][] = new double[segmentNumber][hueBins][saturationBins];
        	int c;
        	int q;
        	int numberHistogramBins = hueBins * saturationBins;
        	double sumHue[] = new double[segmentNumber];
        	double sumSaturation[] = new double[segmentNumber];
        	double meanHue[] = new double[segmentNumber];
        	double meanSaturation[] = new double[segmentNumber];
        	double varianceHue[] = new double[segmentNumber];
        	double varianceSaturation[] = new double[segmentNumber];
        	double covarianceHueSaturation[] = new double[segmentNumber];
        	double diffHue;
        	double diffSaturation;
        	double CHue;
        	double CSat;
        	double CHS;
        	double a;
        	double b;
        	double d;
        	double ainv;
        	double binv;
        	double dinv;
        	double hueDiff;
        	double saturationDiff;
        	double dmu[] = new double[2];
        	double d1;
        	double C11;
        	double C12;
        	double C21;
        	double C22;
        	double det;
        	double delta;
        	double tracePlus;
        	//double traceMinus;
        	double C11Plus;
        	double C12Plus;
        	double C21Plus;
        	double C22Plus;
        	//double C11Minus;
        	//double C12Minus;
        	//double C21Minus;
        	//double C22Minus;
        	try {
        		colorImage.exportRGBData(1, 0, sliceSize, red);
        	}
        	catch (IOException e) {
        	    MipavUtil.displayError("IOException " + e + " on colorImage.exportRGBData(1, 0, sliceSize, red)");
        	    error = true;
        	    return;
        	}
        	try {
        		colorImage.exportRGBData(2, 0, sliceSize, green);
        	}
        	catch (IOException e) {
        	    MipavUtil.displayError("IOException " + e + " on colorImage.exportRGBData(2, 0, sliceSize, green)");
        	    error = true;
        	    return;
        	}
        	try {
        		colorImage.exportRGBData(3, 0, sliceSize, blue);
        	}
        	catch (IOException e) {
        	    MipavUtil.displayError("IOException " + e + " on colorImage.exportRGBData(3, 0, sliceSize, blue)");
        	    error = true;
        	    return;
        	}
        	try {
        		destImage.exportData(0, sliceSize, segment);
        	}
        	catch (IOException e) {
        		MipavUtil.displayError("IOException " + e + " on destImage.exportData(0, sliceSize, segment)");
        		error = true;
        		return;
        	}
        	for (i = 0; i < sliceSize; i++) {
        		hsb = Color.RGBtoHSB(red[i], green[i], blue[i], hsb);
        		hue = hsb[0];
        		saturation = hsb[1];
        		hBin = (int)(hue * hueBins);
        		if (hBin == hueBins) {
        			hBin = hueBins - 1;
        		}
        		sBin = (int)(saturation * saturationBins);
        		if (sBin == saturationBins) {
        			sBin = saturationBins - 1;
        		}
        		if (segment[i] > 0) {
        		    hist[segment[i]-1][hBin][sBin]++;
        		    segmentTotal[segment[i]-1]++;
        		    sumHue[segment[i]-1] += hBin;
        		    sumSaturation[segment[i]-1] += sBin;
        		}
        	} // for (i = 0; i < sliceSize; i++)
        	
        	for (i = 0; i < segmentNumber; i++) {
        		meanHue[i] = sumHue[i]/segmentTotal[i];
        		meanSaturation[i] = sumSaturation[i]/segmentTotal[i];
        		for (h = 0; h < hueBins; h++) {
        		    for (s = 0; s < saturationBins; s++) {
        		    	normalizedHist[i][h][s] = hist[i][h][s]/segmentTotal[i];
        		    	diffHue = h - meanHue[i];
        		    	diffSaturation = s - meanSaturation[i];
        		    	varianceHue[i] += (diffHue * diffHue);
        		    	varianceSaturation[i] += (diffSaturation * diffSaturation);
        		    	covarianceHueSaturation[i] += (diffHue * diffSaturation);
        		    }
        		}
        	}
        	
        	for (i = 0; i < segmentNumber; i++) {
        		varianceHue[i] = varianceHue[i]/(segmentTotal[i] - 1);
        		varianceSaturation[i] = varianceSaturation[i]/(segmentTotal[i] - 1);
        		covarianceHueSaturation[i] = covarianceHueSaturation[i]/(segmentTotal[i] - 1);
        	}
        	
        	// Calculate the similarity of the histograms of each pair of segments
        	boolean merged[] = new boolean[segmentNumber];
        	for (c = 0; c < segmentNumber; c++) {
        		for (q = c+1; q < segmentNumber; q++) {
        			// If the segment is not merged already
        		    if (!merged[q]) {
        		    	// Calculate the histogram similarity
        		    	// C = (cov(c) + cov(q))/2
        		    	CHue = (varianceHue[c] + varianceHue[q])/2.0;
        		    	CSat = (varianceSaturation[c] + varianceSaturation[q])/2.0;
        		    	CHS = (covarianceHueSaturation[c] + covarianceHueSaturation[q])/2.0;
        		    	// dmu = (mu1 - mu2)/chol(C)
        		    	// Cholesky factorization of [CHue  CHS]
        		    	//                           [CHS   CSat]
        		    	// [a   0]  * [a  b] = [a*a          a*b]
        		    	// [b   d]  * [0  d]   [a*b  (b*b + d*d)]
        		    	
        		    	// chol(C) = [a b]
        		    	//           [0 d]
        		    	a = Math.sqrt(CHue);
        		    	b = CHS/a;
        		    	d = Math.sqrt(CSat - b*b);
        		    	// /chol(C) means multiply by inverse
        		    	// inv (chol(C)) = [1/a    -b/(a*d)]
        		    	//                 [0           1/d]
        		    	ainv = 1.0/a;
        		    	binv = -b/(a*d);
        		    	dinv = 1.0/d;
        		    	hueDiff = meanHue[c] - meanHue[q];
        		    	saturationDiff = meanSaturation[c] - meanSaturation[q];
        		    	dmu[0] = hueDiff * ainv;
        		    	dmu[1] = hueDiff * binv + saturationDiff * dinv;
        		    	// C1 * C2 is almost always not symmetric so the Cholesky
        		    	// decomposition cannot be performed.
        		    	// d = 0.125*dmu*dmu' + 0.5*log(det(C/chol(C1*C2)))
        	            // Do
        		    	// d = 0.125*dmu*dmu' + 0.5*log(abs(det(C/sqrtm(C1*C2))))
        		    	d1 = 0.125*(dmu[0]*dmu[0] + dmu[1]*dmu[1]);
        		    	C11 = varianceHue[c]*varianceHue[q] + covarianceHueSaturation[c]*covarianceHueSaturation[q];
        		    	C12 = varianceHue[c]*covarianceHueSaturation[q] + covarianceHueSaturation[c]*varianceSaturation[q];
                        C21 = covarianceHueSaturation[c]*varianceHue[q] + varianceSaturation[c]*covarianceHueSaturation[q];
                        C22 = covarianceHueSaturation[c]*covarianceHueSaturation[q] + varianceSaturation[c]*varianceSaturation[q];
                        det = C11 * C22 - C12 * C21;
                        if (det < 0.0) {
                            UI.setDataText("For segement numbers " + c + " and " + q + " the determinant is negative\n");
                            continue;
                        }
                        delta = Math.sqrt(det);
                        tracePlus = Math.sqrt(C11 + C22 + 2.0 * delta);
                        C11Plus = (C11 + delta)/tracePlus;
                        C12Plus = C12/tracePlus;
                        C21Plus = C21/tracePlus;
                        C22Plus = (C22 + delta)/tracePlus;
                        //traceMinus = Math.sqrt(C11 + C22 - 2.0 * delta);
                        //C11Minus = (C11 - delta)/traceMinus;
                        //C12Minus = C12/traceMinus;
                        //C21Minus = C21/traceMinus;
                        //C22Minus = (C22 - delta)/traceMinus;
                        // Both CPlus and CMinus are square roots, but CPlus agrees with P*sqrt(D)**(PInverse)
                        // For A = [a b]
                        //         [c d]
                        // Ainverse = (1/(ad-bc)) * [d -b]
                        //                          [-c a]
                        det = C11Plus * C22Plus - C12Plus * C21Plus;
                        C11 = C22Plus/det;
                        C12 = -C12Plus/det;
                        C21 = -C21Plus/det;
                        C22 = C11Plus/det;
        		    }
        		}
        	}
        } // if (srcImage.isColorImage())
    } // mergeSegments

    

}
