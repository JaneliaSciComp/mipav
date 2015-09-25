package gov.nih.mipav.model.algorithms;
import WildMagic.LibFoundation.Mathematics.Vector3f;


import gov.nih.mipav.model.algorithms.filters.*;
import gov.nih.mipav.model.algorithms.utilities.AlgorithmRGBtoGray;
import gov.nih.mipav.model.file.*;
import gov.nih.mipav.model.scripting.*;
import gov.nih.mipav.model.structures.*;
import gov.nih.mipav.view.*;

import java.awt.Dimension;
import java.io.*;
import java.util.*;

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
	
	private int presentSegmentNumber;
	
	private ModelImage grayImage;
	
	private ModelImage thresholdImage;
	
	private ModelImage distTransformed;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Constructs new watershed algorithm.
     *
     * @param  destImg  Image model where result image is to stored
     * @param  srcImg   Source image model
     * @param  segmentNumber
     */
    public AlgorithmAutoSeedWatershed(ModelImage destImg, ModelImage srcImg, int segmentNumber) {

        super(destImg, srcImg);
        this.segmentNumber = segmentNumber;
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

        presentSegmentNumber = 0;
        
        watershedSegment();
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
    	double factor = dif / histogram.getNDims();
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
        
        distTransformed = new ModelImage(ModelStorageBase.FLOAT, srcImage.getExtents(), srcImage.getImageName() + "_distTransformed");
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
        	setCompleted(false);
        	return;
        }

        try {
            pointArray = new int[sliceSize / 2];
            distBuffer = new float[sliceSize];
        } catch (OutOfMemoryError e) {
            displayError("distanceMap: Out of memory");
            setCompleted(false);

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
        for (pix = 0; pix  < sliceSize; pix++) {
        	distBuffer[pix] = distBuffer[pix]/distMax;
        }

        try {
            distTransformed.importData(0, distBuffer, true);
        } catch (IOException error) {
            displayError("IOException " + error + " on distTransformed.importData(0, distBuffer, true)");
            setCompleted(false);

            return;
        } 
    }

    

}
