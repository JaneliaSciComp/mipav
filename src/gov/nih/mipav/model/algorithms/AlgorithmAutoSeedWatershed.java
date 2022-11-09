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
   * 
   * The formula for the Bhattacharyya distance for multivariate gaussian distributions is taken from "The Divergence
   * and Bhattacharyya Distance Measures in Signal Selection" by Thomas Kailath, IEEE Transactions on Communication
   * Technology" Vol. COM-15, No. 1, February, 1967, pp. 52-60.
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
	
	private boolean mergeSimilar;
	
	private double maxDistance;
	
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
     * @param  mergeSimilar
     * @param  maxDistance
     */
    public AlgorithmAutoSeedWatershed(ModelImage destImg, ModelImage srcImg,
    		float scaleX, float scaleY, boolean mergeSimilar, double maxDistance) {

        super(destImg, srcImg);
        this.scaleX = scaleX;
        this.scaleY = scaleY;
        this.mergeSimilar = mergeSimilar;
        this.maxDistance = maxDistance;
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
        
        if (mergeSimilar) {
	        mergeSegments();
	        if (error) {
	        	setCompleted(false);
	        }
	        else {
	            setCompleted(true);
	        }
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
    	int objectsID = (int)distTransformed.getMax();
        UI.setDataText("Number of objects id = " + objectsID + "\n");
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
        if (srcImage.isColorImage()) {
            new ViewJFrameImage(grayImage);
        }
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
        segmentNumber = (int)destImage.getMax();
        UI.setDataText("Number of segments before merging = " + segmentNumber + "\n");
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
        	int c;
        	int q;
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
        	double hueDiff;
        	double saturationDiff;
        	double C11Inverse;
        	double C12Inverse;
        	double C21Inverse;
        	double C22Inverse;
        	double similarity;
        	double B1;
        	double detR;
        	double detR1;
        	double detR2;
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
        	
        	// Remove segments of size 1 since variance divides by (segmentTotal[i] - 1)
        	int numDeleted = 0;
        	for (i = 0; i < segmentNumber; i++) {
        		if (segmentTotal[i] <= 1) {
        			for (q = 0; q < sliceSize; q++) {
        				if (segment[q] == i+1) {
        					segment[q] = 0;
        				}
        				else if (segment[q] > i+1) {
        					segment[q] = segment[q] - 1;
        				}
        			}
        			for (q = i; q  < segmentNumber-1; q++) {
        			    segmentTotal[q] = segmentTotal[q+1];
        			    sumHue[q] = sumHue[q+1];
        			    sumSaturation[q] = sumSaturation[q+1];
        			    for (h = 0; h < hueBins; h++) {
                		    for (s = 0; s < saturationBins; s++) {
                		        hist[q][h][s] = hist[q+1][h][s];	
                		    }
        			    }
        			} // for (q = i; q  < segmentNumber-1; q++)
        			segmentNumber--;
        			numDeleted++;
        		} // if (segmentTotal[i] <= 1)
        	} // for (i = 0; i < segmentNumber; i++)
        	if (numDeleted == 1) {
        		UI.setDataText("One segment of size 1 deleted\n");
        	}
        	else if (numDeleted > 1) {
        		UI.setDataText(numDeleted + " segments of size 1 deleted\n");
        	}
        	
        	for (i = 0; i < segmentNumber; i++) {
        		meanHue[i] = sumHue[i]/segmentTotal[i];
        		meanSaturation[i] = sumSaturation[i]/segmentTotal[i];
        		for (h = 0; h < hueBins; h++) {
        			diffHue = h - meanHue[i];
        		    for (s = 0; s < saturationBins; s++) {
        		    	diffSaturation = s - meanSaturation[i];
        		    	varianceHue[i] += (diffHue * diffHue) * hist[i][h][s];
        		    	varianceSaturation[i] += (diffSaturation * diffSaturation) * hist[i][h][s];
        		    	covarianceHueSaturation[i] += (diffHue * diffSaturation) * hist[i][h][s];
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
        		    	// For multivariate gaussian distributions
        		    	// if pi(x) = N(mi,Ri), where Ri are covariance matrices
        		    	// B = (1/8)(m1 - m2)'RInverse(m1-m2) + (1/2)ln(detR/sqrt(detR1*detR2))
        		    	// where 2R = R1 + R2
        		    	// The first term gives the class separability due to the difference
        		    	// between class means, while the second term gives the class separability
        		    	// due to the difference between class covariance matrices.
        		    	// Calculate the histogram similarity
        		    	// C = (cov(c) + cov(q))/2
        		    	CHue = (varianceHue[c] + varianceHue[q])/2.0;
        		    	CSat = (varianceSaturation[c] + varianceSaturation[q])/2.0;
        		    	CHS = (covarianceHueSaturation[c] + covarianceHueSaturation[q])/2.0;
        		    	// C = [CHue  CHS]
        		    	//     [CHS  CSat]
        		    	// For A = [a b]
                        //         [c d]
                        // Ainverse = (1/(ad-bc)) * [d -b]
                        //                          [-c a]
        		    	// Therefore, RInverse is given by:
        		    	detR = CHue * CSat - CHS * CHS;
        		    	C11Inverse = CSat/detR;
        		    	C12Inverse = -CHS/detR;
        		    	C21Inverse = -CHS/detR;
        		    	C22Inverse = CHue/detR;
        		    	hueDiff = meanHue[c] - meanHue[q];
        		    	saturationDiff = meanSaturation[c] - meanSaturation[q];
        		    	B1 = 0.125*(hueDiff*hueDiff*C11Inverse + hueDiff*saturationDiff*(C12Inverse + C21Inverse) +
        		    			    saturationDiff*saturationDiff*C22Inverse);
        		    	detR1 = varianceHue[c] * varianceSaturation[c] - covarianceHueSaturation[c] * covarianceHueSaturation[c];
        		    	detR2 = varianceHue[q] * varianceSaturation[q] - covarianceHueSaturation[q] * covarianceHueSaturation[q];
        		    	similarity = B1 + 0.5*Math.log(detR/Math.sqrt(detR1*detR2));
                        Preferences.debug("Similarity = " + similarity + " for segments " + c + " and " + q + "\n", 
                        		Preferences.DEBUG_ALGORITHM);
                        if (similarity <= maxDistance) {
                        	merged[q] = true;
                        	// Reduce number of segments
                        	for (i = 0; i < sliceSize; i++) {
                        		if (segment[i] == q+1) {
                        			segment[i] = c+1;
                        		}
                        		else if (segment[i] > q+1) {
                        			segment[i] = segment[i] - 1;
                        		}
                        	} // for (i = 0; i < sliceSize; i++)
                        	segmentTotal[c] = segmentTotal[c] + segmentTotal[q];
                        	sumHue[c] = sumHue[c] + sumHue[q];
                        	sumSaturation[c] = sumSaturation[c] + sumSaturation[q];
                        	for (h = 0; h < hueBins; h++) {
                    		    for (s = 0; s < saturationBins; s++) {
                    		    	hist[c][h][s] = hist[c][h][s] + hist[q][h][s];
                    		    }
                    		 }
                        	meanHue[c] = sumHue[c]/segmentTotal[c];
                    		meanSaturation[c] = sumSaturation[c]/segmentTotal[c];
                    		varianceHue[c] = 0.0;
                    		varianceSaturation[c] = 0.0;
                    		covarianceHueSaturation[c] = 0.0;
                    		for (h = 0; h < hueBins; h++) {
                    			diffHue = h - meanHue[c];
                    		    for (s = 0; s < saturationBins; s++) {
                    		    	diffSaturation = s - meanSaturation[c];
                    		    	varianceHue[c] += (diffHue * diffHue) * hist[c][h][s];
                    		    	varianceSaturation[c] += (diffSaturation * diffSaturation) * hist[c][h][s];
                    		    	covarianceHueSaturation[c] += (diffHue * diffSaturation) * hist[c][h][s];
                    		    }
                    		}
                    		varianceHue[c] = varianceHue[c]/(segmentTotal[c] - 1);
                    		varianceSaturation[c] = varianceSaturation[c]/(segmentTotal[c] - 1);
                    		covarianceHueSaturation[c] = covarianceHueSaturation[c]/(segmentTotal[c] - 1);
                    		for (i = q; i < segmentNumber-1; i++) {
                    			segmentTotal[i] = segmentTotal[i+1];
                    			sumHue[i] = sumHue[i+1];
                    			sumSaturation[i] = sumSaturation[i+1];
                    			for (h = 0; h < hueBins; h++) {
                        		    for (s = 0; s < saturationBins; s++) {
                        		    	hist[i][h][s] = hist[i+1][h][s];
                        		    }
                        		 }
                    			meanHue[i] = meanHue[i+1];
                    			meanSaturation[i] = meanSaturation[i+1];
                    			varianceHue[i] = varianceHue[i+1];
                    			varianceSaturation[i] = varianceSaturation[i+1];
                    			covarianceHueSaturation[i] = covarianceHueSaturation[i+1];
                    			merged[i] = merged[i+1];
                    		} // for (i = q; i < segmentNumber-1; i++)
                        	segmentNumber--;
                        }
        		    }
        		}
        	}
        	UI.setDataText("Number of segments after merging = " + segmentNumber + "\n");
        	try {
        	    destImage.importData(0, segment, true);
        	}
        	catch(IOException e) {
        	    MipavUtil.displayError("IOException " + e + " on destImage.importData(0, segment, true)");
        	    error = true;
        	    return;
        	}
        } // if (srcImage.isColorImage())
    } // mergeSegments

    

}
