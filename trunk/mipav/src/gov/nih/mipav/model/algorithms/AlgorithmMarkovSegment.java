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
	
	private int iterations = 30;
	
	 //~ Constructors ---------------------------------------------------------------------------------------------------
	
	public AlgorithmMarkovSegment(ModelImage destImg, ModelImage srcImg, int class_number, 
			double potential, int iterations) {
		super(destImg, srcImg);
		this.class_number = class_number;
		this.potential = potential;
		this.iterations = iterations;
	}
	
	public void runAlgorithm() {
		int xDim;
	    int yDim;
	    int sliceSize;
	    double src[];
	    double src2[];
	    int zDim = 1;
	    int tDim = 1;
	    int z;
	    int t;
	    ModelImage segmentedImage;
	    int extents[] = new int[2];
	    double centroidPos[][] = null;
	    AlgorithmKMeans algoKMeans;
	    int algoSelection = AlgorithmKMeans.K_MEANS;
        int distanceMeasure = AlgorithmKMeans.EUCLIDEAN_SQUARED;
        String kMeansFileName = srcImage.getImageFileName() +  "_kmeans.txt";
        int initSelection = AlgorithmKMeans.BRADLEY_FAYYAD_INIT;
        boolean followBatchWithIncremental = false;
        boolean bwSegmentedImage;
        double scale[] = null;
        int i;
        int nPoints;
        int groupNum[] = null;
        double weight[] = null;
        double pos[][] = null;
        int nval;
        float redBuffer[] = null;
        float greenBuffer[] = null;
        float blueBuffer[] = null;
        // Scale factor used in RGB-CIELab conversions.  255 for ARGB, could be higher for ARGB_USHORT.
        double scaleMax = 255.0;
        boolean useColorHistogram = true;
        boolean scaleVariablesToUnitVariance = false;
        double axesRatio[] = null;
        boolean showKMeansSegmentedImage = false;
        boolean colorSegmentInRGB = true;
        int cf;
        int iter;
        float redMin;
        float redMax;
        float greenMin;
        float greenMax;
        float blueMin;
        float blueMax;
        boolean useRed = false;
        boolean useGreen = false;
        boolean useBlue = false;
        int vLength = 0;
        double mu[][];
        double sigma[][][];
        byte segmented[];
        int v;
        int v2;
        Vector<Integer> classVector = new Vector<Integer>();
        int j;
        double sum = 0.0;
        double diff;
        double diffRed[];
        double diffGreen[];
        double diffBlue[];
        double Ef[][];
        double mu_i[];
        double sigma_i[][];
        double diff_i[][];
        double det = 0.0;
        double inv[][];
        double transpose[][];
        double diffInv[][];
        double diffInvDiff[][];
        double El[][];
        int y;
        int x;
        int dely;
        int delx;
        double E[][];
        double Emin;
        int index;
	    
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
     
        src = new double[sliceSize];
        src2 = new double[sliceSize];
        segmented = new byte[sliceSize];
        Ef = new double[sliceSize][class_number];
        El = new double[sliceSize][class_number];
        E = new double[sliceSize][class_number];
        segmentedImage = new ModelImage(ModelStorageBase.BYTE, extents,
	            (srcImage.getImageFileName()+ "_kmeans"));
        segmentedImage.getFileInfo()[0].setResolutions(srcImage.getFileInfo()[0].getResolutions());
        // For black and white create a 2D array with xDim * yDim positions in the one dimension and
        // 1 value.
        // For color create a 2D array with xDim * yDim positions in one dimension and 2 or 3 values.
        if (srcImage.isColorImage()) {
        	cf = 3;
        	redBuffer = new float[sliceSize];
        	greenBuffer = new float[sliceSize];
        	blueBuffer = new float[sliceSize];
        	scale = new double[3];
        	scale[0] = 1.0;
        	scale[1] = 1.0;
        	scale[2] = 1.0;
        	bwSegmentedImage = false;
        }
        else {
        	cf = 1;
        	scale = new double[1];
        	scale[0] = 1.0;
        	bwSegmentedImage = true;
        	vLength = 1;
        }
        
        for (t = 0; (t < tDim) && !threadStopped; t++) {
            for (z = 0; (z < zDim) && !threadStopped; z++) {
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
	                centroidPos = new double[1][class_number];
	                nval = 0;
	                weight[nval] = 1.0;
	                pos[0][nval] = src2[0];
	                for (i = 1; i < sliceSize; i++) {
	                    if (src2[i] == src2[i-1]) {
	                        weight[nval] += 1.0;
	                    }
	                    else {
	                        nval++;
	                        weight[nval] = 1.0;
	                        pos[0][nval] = src2[i];
	                    }
	                }
	                for (i = 0; i < sliceSize; i++) {
	                    src2[i] = src[i];
	                }
	               
            	}
            	else {
            	    try {
            		    srcImage.exportRGBData(1, 4*(t * zDim + z) * sliceSize, sliceSize, redBuffer);
            	    } catch (IOException error) {
                    displayError("Algorithm Markov Segment: Image(s) locked");
                    setCompleted(false);
                    fireProgressStateChanged(ViewJProgressBar.PROGRESS_WINDOW_CLOSING);
                    srcImage.releaseLock();

                    return;
                    }
            	    
            	    try {
            		    srcImage.exportRGBData(2, 4*(t * zDim + z) * sliceSize, sliceSize, greenBuffer);
            	    } catch (IOException error) {
                    displayError("Algorithm Markov Segment: Image(s) locked");
                    setCompleted(false);
                    fireProgressStateChanged(ViewJProgressBar.PROGRESS_WINDOW_CLOSING);
                    srcImage.releaseLock();

                    return;
                    }
            	    
            	    try {
            		    srcImage.exportRGBData(3, 4*(t * zDim + z) * sliceSize, sliceSize, blueBuffer);
            	    } catch (IOException error) {
                    displayError("Algorithm Markov Segment: Image(s) locked");
                    setCompleted(false);
                    fireProgressStateChanged(ViewJProgressBar.PROGRESS_WINDOW_CLOSING);
                    srcImage.releaseLock();

                    return;
                    }
            	    
            	    redMin = Float.MAX_VALUE;
            	    redMax = -Float.MAX_VALUE;
            	    greenMin = Float.MAX_VALUE;
            	    greenMax = -Float.MAX_VALUE;
            	    blueMin = Float.MAX_VALUE;
            	    blueMax = -Float.MAX_VALUE;
            	    vLength = 0;
            	    useRed = false;
            	    useGreen = false;
            	    useBlue = false;
            	    for (i = 0; i < sliceSize; i++) {
            	    	if (redBuffer != null) {
    	        	        if (redBuffer[i] < redMin) {
    	        	        	redMin = redBuffer[i];
    	        	        }
    	        	        if (redBuffer[i] > redMax) {
    	        	        	redMax = redBuffer[i];
    	        	        }
            	    	}
            	    	if (greenBuffer != null) {
    	        	        if (greenBuffer[i] < greenMin) {
    	        	        	greenMin = greenBuffer[i];
    	        	        }
    	        	        if (greenBuffer[i] > greenMax) {
    	        	        	greenMax = greenBuffer[i];
    	        	        }
            	    	}
            	    	if (blueBuffer != null) {
    	        	        if (blueBuffer[i] < blueMin) {
    	        	        	blueMin = blueBuffer[i];
    	        	        }
    	        	        if (blueBuffer[i] > blueMax) {
    	        	        	blueMax = blueBuffer[i];
    	        	        }
            	    	}
            	    } // for (i = 0; i < length; i++)
            	    vLength = 0;
            	    if (redMax > redMin) {
            	    	useRed = true;
            	    	vLength++;
            	    }
            	    if (greenMax > greenMin) {
            	    	useGreen = true;
            	    	vLength++;
            	    }
            	    if (blueMax > blueMin) {
            	    	useBlue = true;
            	    	vLength++;
            	    }
            	    
            	} // else
            	algoKMeans = new AlgorithmKMeans(segmentedImage,algoSelection,distanceMeasure,pos,scale,groupNum,weight,centroidPos,kMeansFileName,
	                        initSelection,redBuffer, greenBuffer, blueBuffer, scaleMax,
	                        useColorHistogram, scaleVariablesToUnitVariance, axesRatio,
	                        bwSegmentedImage, src2, showKMeansSegmentedImage, followBatchWithIncremental, colorSegmentInRGB);
                algoKMeans.run();
                try {
                	segmentedImage.exportData(0, sliceSize, segmented);
                }
                catch (IOException error) {
                    displayError("Algorithm Markov Segment: segmentedImage locked");
                    setCompleted(false);
                    fireProgressStateChanged(ViewJProgressBar.PROGRESS_WINDOW_CLOSING);

                    return;
                }
                iter = 0;
                mu = new double[class_number][vLength];
                sigma = new double[vLength][vLength][class_number];
                mu_i = new double[vLength];
                sigma_i = new double[vLength][vLength];
                diff_i = new double[sliceSize][vLength];
                transpose = new double[vLength][vLength];
                inv = new double[vLength][vLength];
                diffInv = new double[sliceSize][vLength];
                diffInvDiff = new double[sliceSize][vLength];
                while (iter < iterations) {
                    for (i = 0; i < class_number; i++) {
                    	classVector.clear();
                    	for (j = 0; j < sliceSize; j++) {
                    	    if (segmented[j] == i) {
                    	    	classVector.add(j);
                    	    }
                    	}
                        if (classVector.size() == 1) {
                            if (cf == 1) {
                            	mu[i][0] = src[classVector.get(0)];
                            	sigma[0][0][i] = 0.0;
                            } // if (cf == 1)
                            else if (vLength == 3) {
                            	mu[i][0] = redBuffer[classVector.get(0)];
                            	mu[i][1] = greenBuffer[classVector.get(0)];
                            	mu[i][2] = blueBuffer[classVector.get(0)];
                            	for (v = 0; v < 3; v++) {
                            		for (v2 = 0; v2 < 3; v2++) {
                            			sigma[v][v2][i] = 0;
                            		}
                            	}
                            } // else if (vLength == 3)
                            else if (vLength == 2) {
                            	if (useRed && useGreen) {
                            	    mu[i][0] = redBuffer[classVector.get(0)];
                            	    mu[i][1] = greenBuffer[classVector.get(0)];	
                            	}
                            	else if (useRed && useBlue) {
                            		mu[i][0] = redBuffer[classVector.get(0)];
                             	    mu[i][1] = blueBuffer[classVector.get(0)];		
                            	}
                            	else {
                            		mu[i][0] = greenBuffer[classVector.get(0)];
                             	    mu[i][1] = blueBuffer[classVector.get(0)];			
                            	}
                            	for (v = 0; v < 2; v++) {
                            		for (v2 = 0; v2 < 3; v2++) {
                            			sigma[v][v2][i] = 0;
                            		}
                            	}
                            } // else if (vLength == 2)
                            else if (vLength == 1) {
                                if (useRed) {
                                	mu[i][0] = redBuffer[classVector.get(0)];	
                                }
                                else if (useGreen) {
                                	mu[i][0] = greenBuffer[classVector.get(0)];	
                                }
                                else {
                                	mu[i][0] = blueBuffer[classVector.get(0)];
                                }
                                sigma[0][0][i] = 0;
                            } // else if (vLength == 1)
                        } // if (classVector.size() == 1)
                        else if (classVector.size() > 1) {
                        	if (cf == 1) {
	                            sum = 0.0;
	                            for (j = 0; j < classVector.size(); j++) {
	                            	sum += src[classVector.get(j)];
	                            }
	                            mu[i][0] = sum/classVector.size();
	                            sum = 0.0;
	                            for (j = 0; j < classVector.size(); j++) {
	                                diff = src[classVector.get(j)] - mu[i][0];
	                                sum += (diff * diff);
	                            }
	                            sigma[0][0][i] = sum/(classVector.size() - 1);
                        	} // if (cf == 1)
                        	else if (vLength == 3) {
                        		sum = 0.0;
 	                            for (j = 0; j < classVector.size(); j++) {
 	                            	sum += redBuffer[classVector.get(j)];
 	                            }
 	                            mu[i][0] = sum/classVector.size();
 	                            sum = 0.0;
	                            for (j = 0; j < classVector.size(); j++) {
	                            	sum += greenBuffer[classVector.get(j)];
	                            }
	                            mu[i][1] = sum/classVector.size();
	                            sum = 0.0;
 	                            for (j = 0; j < classVector.size(); j++) {
 	                            	sum += blueBuffer[classVector.get(j)];
 	                            }
 	                            mu[i][2] = sum/classVector.size();
 	                            diffRed = new double[classVector.size()];
 	                            diffGreen = new double[classVector.size()];
 	                            diffBlue = new double[classVector.size()];
 	                            for (j = 0; j < classVector.size(); j++) {
 	                            	diffRed[j] = redBuffer[classVector.get(j)] - mu[i][0];
 	                            	diffGreen[j] = greenBuffer[classVector.get(j)] - mu[i][1];
 	                            	diffBlue[j] = blueBuffer[classVector.get(j)] - mu[i][2];
 	                            }
 	                            sum = 0.0;
 	                            for (j = 0; j < classVector.size(); j++) {
 	                            	sum += diffRed[j] * diffRed[j];
 	                            }
 	                            sigma[0][0][i] = sum/(classVector.size() - 1);
 	                            sum = 0.0;
	                            for (j = 0; j < classVector.size(); j++) {
	                            	sum += diffGreen[j] * diffGreen[j];
	                            }
	                            sigma[1][1][i] = sum/(classVector.size() - 1);
	                            sum = 0.0;
	                            for (j = 0; j < classVector.size(); j++) {
	                            	sum += diffBlue[j] * diffBlue[j];
	                            }
	                            sigma[2][2][i] = sum/(classVector.size() - 1);
	                            sum = 0.0;
	                            for (j = 0; j < classVector.size(); j++) {
	                            	sum += diffRed[j] * diffGreen[j];
	                            }
	                            sigma[0][1][i] = sum/(classVector.size() - 1);
	                            sigma[1][0][i] = sigma[0][1][i];
	                            sum = 0.0;
	                            for (j = 0; j < classVector.size(); j++) {
	                            	sum += diffRed[j] * diffBlue[j];
	                            }
	                            sigma[0][2][i] = sum/(classVector.size() - 1);
	                            sigma[2][0][i] = sigma[0][2][i];
	                            sum = 0.0;
	                            for (j = 0; j < classVector.size(); j++) {
	                            	sum += diffGreen[j] * diffBlue[j];
	                            }
	                            sigma[1][2][i] = sum/(classVector.size() - 1);
	                            sigma[2][1][i] = sigma[1][2][i];
                        	} // else if (vLength == 3)
                        	else if (vLength == 2) {
                        	    if (useRed && useGreen) {
                        	    	sum = 0.0;
     	                            for (j = 0; j < classVector.size(); j++) {
     	                            	sum += redBuffer[classVector.get(j)];
     	                            }
     	                            mu[i][0] = sum/classVector.size();
     	                            sum = 0.0;
    	                            for (j = 0; j < classVector.size(); j++) {
    	                            	sum += greenBuffer[classVector.get(j)];
    	                            }
    	                            mu[i][1] = sum/classVector.size();	
    	                            diffRed = new double[classVector.size()];
     	                            diffGreen = new double[classVector.size()];
     	                            for (j = 0; j < classVector.size(); j++) {
    	                            	diffRed[j] = redBuffer[classVector.get(j)] - mu[i][0];
    	                            	diffGreen[j] = greenBuffer[classVector.get(j)] - mu[i][1];
    	                            }
    	                            sum = 0.0;
    	                            for (j = 0; j < classVector.size(); j++) {
    	                            	sum += diffRed[j] * diffRed[j];
    	                            }
    	                            sigma[0][0][i] = sum/(classVector.size() - 1);
    	                            sum = 0.0;
   	                                for (j = 0; j < classVector.size(); j++) {
   	                            	    sum += diffGreen[j] * diffGreen[j];
   	                                }
   	                                sigma[1][1][i] = sum/(classVector.size() - 1);
   	                                sum = 0.0;
 	                                for (j = 0; j < classVector.size(); j++) {
 	                            	    sum += diffRed[j] * diffGreen[j];
 	                                }
 	                                sigma[0][1][i] = sum/(classVector.size() - 1);
 	                                sigma[1][0][i] = sigma[0][1][i];
                        	    } // if (useRed && useGreen)
                        	    else if (useRed && useBlue) {
                        	    	sum = 0.0;
     	                            for (j = 0; j < classVector.size(); j++) {
     	                            	sum += redBuffer[classVector.get(j)];
     	                            }
     	                            mu[i][0] = sum/classVector.size();
     	                            sum = 0.0;
    	                            for (j = 0; j < classVector.size(); j++) {
    	                            	sum += blueBuffer[classVector.get(j)];
    	                            }
    	                            mu[i][1] = sum/classVector.size();	
    	                            diffRed = new double[classVector.size()];
     	                            diffBlue = new double[classVector.size()];
     	                            for (j = 0; j < classVector.size(); j++) {
    	                            	diffRed[j] = redBuffer[classVector.get(j)] - mu[i][0];
    	                            	diffBlue[j] = blueBuffer[classVector.get(j)] - mu[i][1];
    	                            }
    	                            sum = 0.0;
    	                            for (j = 0; j < classVector.size(); j++) {
    	                            	sum += diffRed[j] * diffRed[j];
    	                            }
    	                            sigma[0][0][i] = sum/(classVector.size() - 1);
    	                            sum = 0.0;
   	                                for (j = 0; j < classVector.size(); j++) {
   	                            	    sum += diffBlue[j] * diffBlue[j];
   	                                }
   	                                sigma[1][1][i] = sum/(classVector.size() - 1);
   	                                sum = 0.0;
 	                                for (j = 0; j < classVector.size(); j++) {
 	                            	    sum += diffRed[j] * diffBlue[j];
 	                                }
 	                                sigma[0][1][i] = sum/(classVector.size() - 1);
 	                                sigma[1][0][i] = sigma[0][1][i];	
                        	    } // else if (useRed && useBlue)
                        	    else {
                        	    	sum = 0.0;
     	                            for (j = 0; j < classVector.size(); j++) {
     	                            	sum += greenBuffer[classVector.get(j)];
     	                            }
     	                            mu[i][0] = sum/classVector.size();
     	                            sum = 0.0;
    	                            for (j = 0; j < classVector.size(); j++) {
    	                            	sum += blueBuffer[classVector.get(j)];
    	                            }
    	                            mu[i][1] = sum/classVector.size();	
    	                            diffGreen = new double[classVector.size()];
     	                            diffBlue = new double[classVector.size()];
     	                            for (j = 0; j < classVector.size(); j++) {
    	                            	diffGreen[j] = redBuffer[classVector.get(j)] - mu[i][0];
    	                            	diffBlue[j] = blueBuffer[classVector.get(j)] - mu[i][1];
    	                            }
    	                            sum = 0.0;
    	                            for (j = 0; j < classVector.size(); j++) {
    	                            	sum += diffGreen[j] * diffGreen[j];
    	                            }
    	                            sigma[0][0][i] = sum/(classVector.size() - 1);
    	                            sum = 0.0;
   	                                for (j = 0; j < classVector.size(); j++) {
   	                            	    sum += diffBlue[j] * diffBlue[j];
   	                                }
   	                                sigma[1][1][i] = sum/(classVector.size() - 1);
   	                                sum = 0.0;
 	                                for (j = 0; j < classVector.size(); j++) {
 	                            	    sum += diffGreen[j] * diffBlue[j];
 	                                }
 	                                sigma[0][1][i] = sum/(classVector.size() - 1);
 	                                sigma[1][0][i] = sigma[0][1][i];		
                        	    }
                        	} // else if (vLength == 2)
                        	else if (vLength == 1) {
                        		if (useRed) {
                        			sum = 0.0;
     	                            for (j = 0; j < classVector.size(); j++) {
     	                            	sum += redBuffer[classVector.get(j)];
     	                            }
     	                            mu[i][0] = sum/classVector.size();	
     	                            sum = 0.0;
    	                            for (j = 0; j < classVector.size(); j++) {
   	                            	    diff = redBuffer[classVector.get(j)] - mu[i][0];
   	                            	    sum += (diff * diff);
   	                                }
    	                            sigma[0][0][i] = sum/(classVector.size() - 1);
                        		} // if (useRed)
                        		else if (useGreen) {
                        			sum = 0.0;
     	                            for (j = 0; j < classVector.size(); j++) {
     	                            	sum += greenBuffer[classVector.get(j)];
     	                            }
     	                            mu[i][0] = sum/classVector.size();	
     	                            sum = 0.0;
    	                            for (j = 0; j < classVector.size(); j++) {
   	                            	    diff = greenBuffer[classVector.get(j)] - mu[i][0];
   	                            	    sum += (diff * diff);
   	                                }
    	                            sigma[0][0][i] = sum/(classVector.size() - 1);	
                        		} // else if (useGreen)
                        		else {
                        			sum = 0.0;
     	                            for (j = 0; j < classVector.size(); j++) {
     	                            	sum += blueBuffer[classVector.get(j)];
     	                            }
     	                            mu[i][0] = sum/classVector.size();	
     	                            sum = 0.0;
    	                            for (j = 0; j < classVector.size(); j++) {
   	                            	    diff = blueBuffer[classVector.get(j)] - mu[i][0];
   	                            	    sum += (diff * diff);
   	                                }
    	                            sigma[0][0][i] = sum/(classVector.size() - 1);		
                        		}
                        	} // else if (vLength == 1)
                        } // else if (classVector.size() > 1)
                    } // for (i = 0; i < class_number; i++)
                    
                    for (i = 0; i < class_number; i++) {
                        for (v = 0; v < vLength; v++) {
                        	mu_i[v] = mu[i][v];
                        	for (v2 = 0; v2 < vLength; v2++) {
                        		sigma_i[v][v2] = sigma[v][v2][i];
                        	}
                        }
                        if (cf == 1) {
                        	for (j = 0; j < sliceSize; j++) {
                        		diff_i[j][0] = src[j] - mu_i[0];
                        	}
                        } // if (cf == 1)
                        else if (vLength == 3) {
                        	for (j = 0; j < sliceSize; j++) {
                        		diff_i[j][0] = redBuffer[j] - mu_i[0];
                        		diff_i[j][1] = greenBuffer[j] - mu_i[1];
                        		diff_i[j][2] = blueBuffer[j] - mu_i[2];
                        	}
                        } // else if (vLength == 3)
                        else if (vLength == 2) {
                            if (useRed && useGreen) {
                            	for (j = 0; j < sliceSize; j++) {
                            		diff_i[j][0] = redBuffer[j] - mu_i[0];
                            		diff_i[j][1] = greenBuffer[j] - mu_i[1];
                            	}	
                            } // if (useRed && useGreen) 
                            else if (useRed && useBlue) {
                            	for (j = 0; j < sliceSize; j++) {
                            		diff_i[j][0] = redBuffer[j] - mu_i[0];
                            		diff_i[j][1] = blueBuffer[j] - mu_i[1];
                            	}
                            } // else if (useRed && useBlue)
                            else {
                            	for (j = 0; j < sliceSize; j++) {
                            		diff_i[j][0] = greenBuffer[j] - mu_i[0];
                            		diff_i[j][1] = blueBuffer[j] - mu_i[1];
                            	}	
                            }
                        } // else if (vLength == 2)
                        else if (vLength == 1) {
                        	if (useRed) {
	                        	for (j = 0; j < sliceSize; j++) {
	                        		diff_i[j][0] = redBuffer[j] - mu_i[0];
	                        	}
                        	} // if (useRed)
                        	else if (useGreen) {
                        		for (j = 0; j < sliceSize; j++) {
	                        		diff_i[j][0] = greenBuffer[j] - mu_i[0];
	                        	}	
                        	} // else if (useGreen)
                        	else {
                        		for (j = 0; j < sliceSize; j++) {
	                        		diff_i[j][0] = blueBuffer[j] - mu_i[0];
	                        	}		
                        	} // else 
                        } // else if (vLength == 1)
                        if (vLength == 3) {
                        	det = sigma_i[0][0]*(sigma_i[1][1]*sigma_i[2][2] - sigma_i[1][2]*sigma_i[2][1])
                        	    -sigma_i[0][1]*(sigma_i[1][0]*sigma_i[2][2] - sigma_i[1][2]*sigma_i[2][0])
                        	    +sigma_i[0][2]*(sigma_i[1][0]*sigma_i[2][1] - sigma_i[1][1]*sigma_i[2][0]);
                        	for (v = 0; v < 3; v++) {
                        		for (v2 = 0; v2 < 3; v2++) {
                        			transpose[v][v2] = sigma_i[v2][v];
                        		}
                        	}
                        	inv[0][0] = (transpose[1][1]* transpose[2][2] - transpose[2][1]*transpose[1][2])/det;
                        	inv[0][1] = -(transpose[1][0] * transpose[2][2] - transpose[2][0]*transpose[1][2])/det;
                            inv[0][2] = (transpose[1][0] * transpose[2][1] - transpose[2][0] * transpose[1][1])/det;
                            inv[1][0] = -(transpose[0][1] * transpose[2][2] - transpose[2][1] * transpose[0][2])/det;
                            inv[1][1] = (transpose[0][0] * transpose[2][2] - transpose[2][0] * transpose[0][2])/det;
                            inv[1][2] = -(transpose[0][0] * transpose[2][1] - transpose[2][0] * transpose[0][1])/det;
                            inv[2][0] = (transpose[0][1] * transpose[1][2] - transpose[1][1] * transpose[0][2])/det;
                            inv[2][1] = -(transpose[0][0]* transpose[1][2] - transpose[1][0] * transpose[0][2])/det;
                            inv[2][2] = (transpose[0][0] * transpose[1][1] - transpose[1][0] * transpose[0][1])/det;
                        }
                        else if (vLength == 2) {
                            det = sigma_i[0][0]* sigma_i[1][1] - sigma_i[0][1]*sigma_i[1][0];
                            inv[0][0] = sigma_i[1][1]/det;
                            inv[0][1] = -sigma_i[0][1]/det;
                            inv[1][0] = -sigma_i[1][0]/det;
                            inv[1][1] = sigma_i[0][0]/det;
                        }
                        else if (vLength == 1) {
                        	det = sigma_i[0][0];
                        	inv[0][0] = 1.0/det;
                        }
                        for (j = 0; j < sliceSize; j++) {
                        	for (v = 0; v < vLength; v++) {
                        		diffInv[j][v] = 0.0;
                        		for (v2 = 0; v2 < vLength; v2++) {
                        			diffInv[j][v] += (diff_i[j][v2] * inv[v2][v]);
                        		}
                        	} 
                        } // for (j = 0; j < sliceSize; j++)
                    }
                    for (j = 0; j < sliceSize; j++) {
                    	for (v = 0; v < vLength; v++) {
                    		diffInvDiff[j][v] = diffInv[j][v] * diff_i[j][v];
                    	} 
                    } // for (j = 0; j < sliceSize; j++)
                    for (j = 0; j < sliceSize; j++) {
                    	sum = 0.0;
                    	for (v = 0; v < vLength; v++) {
                    		sum += diffInvDiff[j][v];
                    	}
                    	Ef[j][i] = sum + Math.log(det);
                    } // for (i = 0; i < class_number; i++)
                    for (i = 0; i < class_number; i++) {
                        for (y = 0; y < yDim; y++) {
                        	for (x = 0; x < xDim; x++) {
                        		sum = 0;
                        		for (dely = -1; dely <= 1; dely++) {
                        			for (delx = -1; delx <= 1; delx++) {
                        				if ((delx == 0) && (dely == 0)) {
                        					continue;
                        				}
                        				if (((x + delx) < 0) || ((x + delx) > xDim-1) ||
                        				   ((y + dely) < 0) || ((y + dely) > yDim-1)) {
                        					// If out of bounds add to sum
                        					sum = sum + 1;
                        				}
                        				else if (segmented[(x+delx) + xDim*(y+dely)] != i) {
                        					sum = sum + 1;
                        				}
                        			}
                        		}
                        		El[x+xDim*y][i] = sum*potential;
                        	} // for (x = 0; x < xDim; x++)
                        } // for (y = 0; y < yDim; y++)
                    } // for (i = 0; i < class_number; i++)
                    
                    for (i = 0; i < sliceSize; i++) {
                    	for (j = 0; j < class_number; j++) {
                    		E[i][j] = Ef[i][j] + El[i][j];
                    	}
                    } // for (i = 0; i < sliceSize; i++)
                    
                    for (i = 0; i < sliceSize; i++) {
                        Emin = Double.MAX_VALUE;
                        index = -1;
                        for (j = 0; j < class_number; j++) {
                        	if (E[i][j] < Emin) {
                        		Emin = E[i][j];
                        		index = j;
                        	}
                        } // for (j = 0; j < class_number; j++)
                        segmented[i] = (byte)index;
                    } // for (i = 0; i < sliceSize; i++)
                    
                    iter = iter + 1;
                } // while (iter < iterations)
                
                try {
                    destImage.importData((t * zDim + z) * sliceSize, segmented, false);
                } catch (IOException error) {
                    errorCleanUp("Algorithm Markov Segment: Image(s) locked", false);

                    return;
                } 
                } // for (z = 0; (z < zDim) && !threadStopped; z++)
            } // for (t = 0; (t < tDim) && !threadStopped; t++)
                
            destImage.calcMinMax();
            setCompleted(true);
            return;
	}
}