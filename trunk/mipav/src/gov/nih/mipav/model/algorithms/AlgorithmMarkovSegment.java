package gov.nih.mipav.model.algorithms;

import gov.nih.mipav.model.algorithms.AlgorithmBase;
import gov.nih.mipav.model.structures.ModelImage;
import gov.nih.mipav.model.structures.ModelStorageBase;

import java.io.IOException;
import java.util.Arrays;
import java.util.Vector;

import gov.nih.mipav.view.ViewJProgressBar;

/** Copyright (c) 2011, lin
All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are
met:

    * Redistributions of source code must retain the above copyright
      notice, this list of conditions and the following disclaimer.
    * Redistributions in binary form must reproduce the above copyright
      notice, this list of conditions and the following disclaimer in
      the documentation and/or other materials provided with the distribution

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE
LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
POSSIBILITY OF SUCH DAMAGE.
*/

/**
 * 
 * @author ilb
 * This is a port of the Lin code from MATLAB to Java.
 * The code is located at:
 * https://www.mathworks.com/matlabcentral/fileexchange/33592-image-segmentation-based-on-markov-random-fields
 * where it simply says:
 * I have written codes for image segmentation based on Markov Random Fields.
 */

public class AlgorithmMarkovSegment extends AlgorithmBase {
	
	//~ Instance fields ------------------------------------------------------------------------------------------------
	
	private int class_number = 3;
	
	private double potential = 0.5;
	
	private int maxIter = 30;
	
	 //~ Constructors ---------------------------------------------------------------------------------------------------
	
	public AlgorithmMarkovSegment(ModelImage destImg, ModelImage srcImg, int class_number, 
			double potential, int maxIter) {
		super(destImg, srcImg);
		this.class_number = class_number;
		this.potential = potential;
		this.maxIter = maxIter;
	}
	
	public void runAlgorithm() {
		int xDim;
	    int yDim;
	    int sliceSize;
	    double src[];
	    double src2[];
	    float fsrc[];
	    double buffer[][];
	    int zDim = 1;
	    int tDim = 1;
	    int z;
	    int t;
	    int s;
	    int d;
	    ModelImage segmentedImage;
	    int extents[] = new int[2];
	    double centroidPos[][] = new double[1][class_number];
	    AlgorithmKMeans algoKMeans;
	    int algoSelection = AlgorithmKMeans.K_MEANS;
        int distanceMeasure = AlgorithmKMeans.EUCLIDEAN_SQUARED;
        String kMeansFileName = srcImage.getImageFileName() +  "_kmeans.txt";
        int initSelection = AlgorithmKMeans.BRADLEY_FAYYAD_INIT;
        boolean followBatchWithIncremental = false;
        boolean bwSegmentedImage = true;
        double scale[] = new double[]{1.0};
        double rounded[];
        int i;
        int nPoints;
        int groupNum[];
        double weight[];
        double pos[][];
        int nval;
        float redBuffer[] = null;
        float greenBuffer[] = null;
        float blueBuffer[] = null;
        // Scale factor used in RGB-CIELab conversions.  255 for ARGB, could be higher for ARGB_USHORT.
        double scaleMax = 255.0;
        boolean useColorHistogram = false;
        boolean scaleVariablesToUnitVariance = false;
        double axesRatio[] = null;
        boolean showKMeansSegmentedImage = false;
        int iter;
        int nDims;
        int c;
	    
	    if (srcImage == null) {
            displayError("Source Image is null");
            finalize();

            return;
        }
        
        fireProgressStateChanged(0, srcImage.getImageName(), "Markov segment on image ...");
        
        if (srcImage.getNDims() >= 3) {
            zDim = srcImage.getExtents()[2];
        }
        if (srcImage.getNDims() >= 4) {
            tDim = srcImage.getExtents()[3];
        }
        
        xDim = srcImage.getExtents()[0];
        yDim = srcImage.getExtents()[1];
        sliceSize = xDim * yDim;
        extents[0] = xDim;
        extents[1] = yDim;
        // Maintain two buffer images.
        // In alternate iterations, one will be the 
        // source buffer, the other will be the destination buffer.
        buffer = new double[2][sliceSize];
        src = new double[sliceSize];
        src2 = new double[sliceSize];
        fsrc = new float[3*sliceSize];
        rounded = new double[sliceSize];
        segmentedImage = new ModelImage(ModelStorageBase.BYTE, extents,
	            (srcImage.getImageFileName()+ "_kmeans"));
        segmentedImage.getFileInfo()[0].setResolutions(srcImage.getFileInfo()[0].getResolutions());
        // For black and white create a 2D array with xDim * yDim positions in the one dimension and
        // 1 value.
        // For color create a 2D array with xDim * yDim positions in one dimension and 2 or 3 values.
        int cf = 1;
        if (srcImage.isColorImage()) {
        	cf = 3;
        }
        
        for (t = 0; (t < tDim) && !threadStopped; t++) {
            for (z = 0; (z < zDim) && !threadStopped; z++) {
            	for (c = 0; c < cf; c++) {
            	s = 1;
            	d = 0;
            	if (cf == 1) {
	                try {
	                    srcImage.exportData((t * zDim + z) * sliceSize, sliceSize, src); // locks and releases lock
	                } catch (IOException error) {
	                    displayError("Algorithm Markov Segment: Image(s) locked");
	                    setCompleted(false);
	                    fireProgressStateChanged(ViewJProgressBar.PROGRESS_WINDOW_CLOSING);
	                    srcImage.releaseLock();
	
	                    return;
	                }
	                
	                for (i = 0; i < sliceSize; i++) {
	                	src2[i] = src[i];
	                }
	                Arrays.sort(src2);
	                nPoints = 1;
	                for (i = 1; i < sliceSize; i++) {
	                	if (src2[i] > src2[i-1]) {
	                		nPoints++;
	                	}
	                }
	                System.out.println("nPoints = " + nPoints);
	                groupNum = new int[nPoints];
	                weight = new double[nPoints];
	                pos = new double[1][nPoints];
	                nval = 0;
	                weight[nval] = 1.0;
	                pos[0][nval] = rounded[0];
	                for (i = 1; i < sliceSize; i++) {
	                    if (rounded[i] == rounded[i-1]) {
	                        weight[nval] += 1.0;
	                    }
	                    else {
	                        nval++;
	                        weight[nval] = 1.0;
	                        pos[0][nval] = rounded[i];
	                    }
	                }
	                for (i = 0; i < sliceSize; i++) {
	                    src2[i] = src[i];
	                }
	                algoKMeans = new AlgorithmKMeans(segmentedImage,algoSelection,distanceMeasure,pos,scale,groupNum,weight,centroidPos,kMeansFileName,
	                        initSelection,redBuffer, greenBuffer, blueBuffer, scaleMax,
	                        useColorHistogram, scaleVariablesToUnitVariance, axesRatio,
	                        bwSegmentedImage, src2, showKMeansSegmentedImage, followBatchWithIncremental);
	                algoKMeans.run();
	                iter = 0;
	                while (iter < maxIter) {
	                	
	                } // while (iter < maxIter)
            	}
            	else {
            	    try {
            		    srcImage.exportRGBData(c+1, 4*(t * zDim + z) * sliceSize, sliceSize, fsrc);
            	} catch (IOException error) {
                    displayError("Algorithm Markov Segment: Image(s) locked");
                    setCompleted(false);
                    fireProgressStateChanged(ViewJProgressBar.PROGRESS_WINDOW_CLOSING);
                    srcImage.releaseLock();

                    return;
                }
            	}
            	} // for (c = 0; c < cf; c++)   
                } // for (z = 0; (z < zDim) && !threadStopped; z++)
            } // for (t = 0; (t < tDim) && !threadStopped; t++)
                
                /*// black and white image undergoes one dimensional kmeans to produce segmented image
                // Without rounding in one image 256 unique values become 65536 unique values
                // This is too many for kmeans to handle in any reasonable so round to integers
                for (i = 0; i < sliceSize; i++) {
                    rounded[i] = Math.round(src[i]);
                }
                Arrays.sort(rounded);
                nPoints = 1;
                for (i = 1; i < sliceSize; i++) {
                    if (rounded[i] > rounded[i-1]) {
                        nPoints++;
                    }
                }
                System.out.println("nPoints = " + nPoints);
                groupNum = new int[nPoints];
                weight = new double[nPoints];
                pos = new double[1][nPoints];
                nval = 0;
                weight[nval] = 1.0;
                pos[0][nval] = rounded[0];
                for (i = 1; i < sliceSize; i++) {
                    if (rounded[i] == rounded[i-1]) {
                        weight[nval] += 1.0;
                    }
                    else {
                        nval++;
                        weight[nval] = 1.0;
                        pos[0][nval] = rounded[i];
                    }
                }
                for (i = 0; i < sliceSize; i++) {
                    rounded[i] = Math.round(src[i]);
                }
                algoKMeans = new AlgorithmKMeans(segmentedImage,algoSelection,distanceMeasure,pos,scale,groupNum,weight,centroidPos,kMeansFileName,
                        initSelection,redBuffer, greenBuffer, blueBuffer, scaleMax,
                        useColorHistogram, scaleVariablesToUnitVariance, axesRatio,
                        bwSegmentedImage, rounded, showKMeansSegmentedImage);
                algoKMeans.run();
                iter = 0;
                while (iter < maxIter) {
                	
                } // while (iter < maxIter)
                try {
                    destImage.importData((t * zDim + z) * sliceSize, buffer[d], false);
                } catch (IOException error) {
                    errorCleanUp("Algorithm Markov Segment: Image(s) locked", false);

                    return;
                } */
               
            	
        
        destImage.calcMinMax();
        setCompleted(true);
        return;
	}
}