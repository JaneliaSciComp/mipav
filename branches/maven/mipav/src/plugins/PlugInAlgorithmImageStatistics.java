import java.io.*;

import gov.nih.mipav.model.structures.*;
import gov.nih.mipav.model.algorithms.*;
import java.text.DecimalFormat;
import java.util.*;



public class PlugInAlgorithmImageStatistics extends AlgorithmBase {
    
    /** X dimension of the CT image */
    private int xDim;

    /** Y dimension of the CT image */
    private int yDim;

    /** Slice size for xDim*yDim */
    private int sliceSize;
    
    private int [] imageBuffer;
    
    private BitSet paintBitmap;
    
    /**
     * Constructor.
     *
     * @param  srcImg       Source image model.
     */
    public PlugInAlgorithmImageStatistics(ModelImage resImg, ModelImage srcImg) {
        super(resImg, srcImg);
        
        paintBitmap = srcImg.getMask();
    }
    
    /**
     * Starts the algorithm.
     */
    public void runAlgorithm() {

        if (srcImage == null) {
            displayError("Source Image is null");

            return;
        }

        constructLog();

        if (srcImage.getNDims() == 2) {
            calc2D();
        } else if (srcImage.getNDims() > 2) {
            calc3D();
        }
    } // end runAlgorithm()
    
//  ~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * Prepares this class for destruction.
     */
    public void finalize() {
        destImage = null;
        srcImage = null;
        super.finalize();
    }
    
    /**
     * Constructs a string of the contruction parameters and outputs the string to the messsage frame if the logging
     * procedure is turned on.
     */
    private void constructLog() {
//        historyString = new String("ImageStatistics(" + ")\n");
    }
    
    
    /**
     * calculate some statistics of the image that is under a paint mask
     *
     */
    private void calc2D() {
        DecimalFormat fltFmt = new DecimalFormat("0.00");

        xDim = srcImage.getExtents()[0];
        yDim = srcImage.getExtents()[1];
        sliceSize = xDim * yDim;
        
        float xRes = srcImage.getResolutions(0)[0];
        float yRes = srcImage.getResolutions(0)[1];
               
        System.out.println("PlugInAlgorithmImageStatistics::calc2D()");
        
        // export the image data to a buffer
        try {
            imageBuffer = new int[sliceSize];
        } catch (OutOfMemoryError e) {
        	imageBuffer = null;
            errorCleanUp("PlugInAlgorithmImageStatistics::calc2D()  Out of memory", true);
            return;
        }

        try {
        	srcImage.exportData(0, sliceSize, imageBuffer); // locks and releases lock
        } catch (IOException error) {
            errorCleanUp("PlugInAlgorithmImageStatistics::calc2D()  Error exporting slice", false);
            return;
        }

/**/
        // find the min and max intensity values under the paint mask
        boolean firstTime = true;
        int minVal = 0;
        int maxVal = 0;
        int pixelCount = 0;
        int sumVal = 0;
        for (int i = 0; i < sliceSize; i++) {
        	if (paintBitmap.get(i) == true) {
        		pixelCount++;
        		sumVal += imageBuffer[i];
        		if (firstTime == true) {
        			firstTime = false;
        			minVal = maxVal = imageBuffer[i];
        		}
        		if (imageBuffer[i] < minVal) {
        			minVal = imageBuffer[i];
        		}
        		if (imageBuffer[i] > maxVal) {
        			maxVal = imageBuffer[i];
        		}
        	} // end if(paintBitmap...)
        } // end for (int i = 0; ...)
        
        // pixelCount is zero if there is no paint mask
        boolean paintMaskFlag = true;
        if (pixelCount == 0) {
        	paintMaskFlag = false;
        }
/**/
/**/
        // compute the amount of fat and muscle within the image
        //int totalCount = 0;
        //int totalSum = 0;
        int fatCount = 0;
        //int fatSum = 0;

        // values with 1024 added to each pixel
//        int fatMin = 834;
//        int fatMax = 994;
//        int leanMin = 1025;
//        int leanMax = 1124;
        
        // values in Hounsfield Units
        int fatMin = -190;
        int fatMax = -30;
        int leanMin = 0;
        int leanMax = 100;
        
        int leanCount = 0;
        //int leanSum = 0;
        int val;
/**/
/*
        for (int i = 0; i < sliceSize; i++) {
        	val = imageBuffer[i];
        	if (val > 0) {
        		totalCount++;
        		totalSum += val;
        	}
        	if (val >= fatMin && val <= fatMax) {
        		fatCount++;
        		fatSum += val;
        	}
        	if (val >= leanMin && val <= leanMax) {
        		leanCount++;
        		leanSum += val;
        	}
        }
        System.out.println("Total Area: " + fltFmt.format(xRes * yRes * totalCount / 100) + " cm^2");
        System.out.println("Fat Area:   " + fltFmt.format(xRes * yRes * fatCount / 100) + " cm^2");
        System.out.println("Lean Area:  " + fltFmt.format(xRes * yRes * leanCount / 100) + " cm^2");

        
        float aveFatVal = fatSum / (float)fatCount;
//        System.out.println("Num FAT pixels: " + fatCount);
        System.out.println("Ave FAT intensity:   " + fltFmt.format(aveFatVal) + "\t" + fltFmt.format(aveFatVal - 1024.0f) + " HU");

        float aveLeanVal = leanSum / (float)leanCount;
//        System.out.println("\nNum LEAN pixels: " + leanCount);
        System.out.println("Ave LEAN intensity:  " + fltFmt.format(aveLeanVal) + "\t" + fltFmt.format(aveLeanVal - 1024.0f) + " HU");
        
        float totalAveVal = totalSum / (float)totalCount;
        System.out.println("Ave total intensity: " + fltFmt.format(totalAveVal) + "\t" + fltFmt.format(totalAveVal - 1024.0f) + " HU");
*/        

        
/*
        // compute image stats from the entire image if there is not paint mask
        pixelCount = 0;
        sumVal = 0;
        if (paintMaskFlag == false) {
        	firstTime = true;
            for (int i = 0; i < sliceSize; i++) {
            	if (imageBuffer[i] > 0) {
            		pixelCount++;
            		sumVal += imageBuffer[i];
            		if (firstTime == true) {
            			firstTime = false;
            			minVal = maxVal = imageBuffer[i];
            		}
            		if (imageBuffer[i] < minVal) {
            			minVal = imageBuffer[i];
            		}
            		if (imageBuffer[i] > maxVal) {
            			maxVal = imageBuffer[i];
            		}
            	} // end if(paintBitmap...)
            } // end for (int i = 0; ...)
        } else {         // end if (paintMaskFlag == false)
        	firstTime = true;
            for (int i = 0; i < sliceSize; i++) {
            	if (paintBitmap.get(i) == true) {
            		pixelCount++;
            		sumVal += imageBuffer[i];
            		if (firstTime == true) {
            			firstTime = false;
            			minVal = maxVal = imageBuffer[i];
            		}
            		if (imageBuffer[i] < minVal) {
            			minVal = imageBuffer[i];
            		}
            		if (imageBuffer[i] > maxVal) {
            			maxVal = imageBuffer[i];
            		}
            	} // end if(paintBitmap...)
            } // end for (int i = 0; ...)
        } // end else

        float aveVal = sumVal / (float)pixelCount;
        System.out.println("Num pixels: " + pixelCount);
        System.out.println("Area: " + fltFmt.format(xRes * yRes * pixelCount / 100) + " cm^2");
        System.out.println("Vals  min: " + minVal + "  max: " + maxVal);
        System.out.println("Ave intensity: " + fltFmt.format(aveVal));
        
        float sigmaSquared;
        float valSquared, sumValSquared = 0.0f;
        if (paintMaskFlag == false) {
           	for (int i = 0; i < sliceSize; i++) {
        		if (imageBuffer[i] > -1020) {
        			valSquared = imageBuffer[i];
        			valSquared *= valSquared;
        		
        			sumValSquared += valSquared;
        		} // end if(paintBitmap...)
        	} // end for (int i = 0; ...)
       	
        } else {
        	for (int i = 0; i < sliceSize; i++) {
        		if (paintBitmap.get(i) == true) {
        			valSquared = imageBuffer[i];
        			valSquared *= valSquared;
        		
        			sumValSquared += valSquared;
        		} // end if(paintBitmap...)
        	} // end for (int i = 0; ...)
        } // end else
        
        sigmaSquared = sumValSquared / pixelCount - aveVal * aveVal;
        System.out.println("std deviation: " + fltFmt.format(Math.sqrt(sigmaSquared)));
*/
        
/*
        // sort the imageBuffer to find the median value
        int [] values = new int [pixelCount];
        int numVals = 0;
        int value, j, medianVal;
    	for (int i = 0; i < sliceSize; i++) {
    		if (paintBitmap.get(i) == true) {
    			value = imageBuffer[i];
    			j = numVals - 1;
    			while (j >= 0 && values[j] > value) {
    				values[j + 1] = values[j];
    				j--;
    			}
   				values[j+1] = value;
   				numVals++;
    		} // end if
    	} // end for
    	medianVal = values[pixelCount / 2];
        System.out.println("median: " + medianVal);
        
        // add the median to all pixels
    	for (int i = 0; i < sliceSize; i++) {
    		imageBuffer[i] -= medianVal;
    	}
*/

/*
        // computer the intensity range for the histogram
        int intensityRange = maxVal - minVal + 1;
        System.out.println("intensity range: " + intensityRange);
        int [] histogram = new int [intensityRange];
        
        if (paintMaskFlag == false) {
           	for (int i = 0; i < sliceSize; i++) {
        		histogram[imageBuffer[i] - minVal]++;
        	} // end for (int i = 0; ...)
       	
        } else {
        	for (int i = 0; i < sliceSize; i++) {
        		if (paintBitmap.get(i) == true) {
            		histogram[imageBuffer[i] - minVal]++;
        		} // end if(paintBitmap...)
        	} // end for (int i = 0; ...)
        } // end else
        
        PrintWriter pw;
    	File outFile;
    	String dirName = Preferences.getImageDirectory();
    	String fileName = dirName + "histogram.txt";
    	outFile = new File(fileName);
    	try {
    		pw = new PrintWriter(new BufferedWriter(new FileWriter(outFile)));
            for (int i = 0; i < intensityRange; i++) {
            	pw.println(i + minVal + "  " + histogram[i]);
            }
    		pw.close();
    	}
    	catch(IOException error) {
            errorCleanUp("PlugInAlgorithmImageStatistics::calc2D()  file error", true);
    	}

        // threshold the image and store the results in the resultImage
        for (int i = 0; i < sliceSize; i++) {
        	 if (imageBuffer[i] > minVal && imageBuffer[i] < maxVal)
        		 imageBuffer[i] = imageBuffer[i];
        	 else 
        		 imageBuffer[i] = 0; //imageBuffer[i];
        }
*/
        
/**/
        // output a label image 
        int otherCount = 0;
        if (paintMaskFlag == false) {
        	for (int i = 0; i < sliceSize; i++) {
        		val = imageBuffer[i];
//        		if (val > 4)
//        			imageBuffer[i] = 4;
       			if (val >= fatMin && val <= fatMax) {
       				imageBuffer[i] = 3;
       				fatCount++;
       			} else if (val >= leanMin && val <= leanMax) {
       				imageBuffer[i] = 6;
       				leanCount++;
       			} else if (val > fatMax && val < leanMin) {
       				imageBuffer[i] = 7;
       				otherCount++;
       			} else {
       				imageBuffer[i] = 0;
       			}
        	} // end for (int i = 0; ...)
        } else {
        	for (int i = 0; i < sliceSize; i++) {
        		if (paintBitmap.get(i) == true) {
        			val = imageBuffer[i];
//        			if (val > 4)
//        				imageBuffer[i] = 4;
        			if (val >= fatMin && val <= fatMax) {
        				imageBuffer[i] = 3;
        				fatCount++;
        			} else if (val >= leanMin && val <= leanMax) {
        				imageBuffer[i] = 6;
        				leanCount++;
        			} else if (val > fatMax && val < leanMin) {
        				imageBuffer[i] = 7;
        				otherCount++;
        			} else {
        				imageBuffer[i] = 0;
        			}
        		} else {   // end if (paintBitmap.get(i) ...)
        			imageBuffer[i] = 0;
        		}
        	} // end for (int i = 0; ...)
        } // end if(paintMaskFlag == false) {} else {}
        
        System.out.println("Fat Area:   " + fltFmt.format(xRes * yRes * fatCount / 100) + " cm^2");
        System.out.println("Lean Area:  " + fltFmt.format(xRes * yRes * leanCount / 100) + " cm^2");
        System.out.println("Other Area: " + fltFmt.format(xRes * yRes * otherCount / 100) + " cm^2");

/**/
        
        // import 
        try {
            destImage.importData(0, imageBuffer, true);
        } catch (IOException error) {
            imageBuffer = null;
            errorCleanUp("AlgorithmImage Statistics: could NOT import dest image", true);
            return;
        } // end try{}-catch{}

        destImage.calcMinMax();
 /*        
        // Find the image min and max values
        int imageMinVal = imageBuffer[0];
        int imageMaxVal = imageBuffer[0];
        for (int i = 1; i < sliceSize; i++) {
        	if (imageBuffer[i] < imageMinVal) {
        		imageMinVal = imageBuffer[i];
            }
            if (imageBuffer[i] > imageMaxVal) {
            	imageMaxVal = imageBuffer[i];
            }
        }
*/        
/* 
    	int [] imageHistogram;
    	int [] paintHistogram;

        // make a histogram buffer
        int numBins = imageMaxVal - imageMinVal + 1;
        try {
        	
        	imageHistogram = new int[numBins];
        	paintHistogram = new int[numBins];
        	
            for (int i = 0; i < sliceSize; i++) {
            	if (paintBitmap.get(i) == false) {
            		imageHistogram[imageBuffer[i] - imageMinVal]++;
            	} else {
            		paintHistogram[imageBuffer[i] - imageMinVal]++;
            	}
            } // end for (int i = 0; ...)
            
        } catch (OutOfMemoryError e) {
        	imageHistogram = null;
            errorCleanUp("PlugInAlgorithmImageStatistics::calc2D()  Out of memory", true);
            return;
        }

        PrintWriter pw;
    	File outFile;
    	String dirName = Preferences.getImageDirectory();
    	String fileName = dirName + "output.text";
    	outFile = new File(fileName);
    	try {
    		pw = new PrintWriter(new BufferedWriter(new FileWriter(outFile)));
    		pw.println("Image file: " + dirName + srcImage.getImageName());
    	    pw.println();
            pw.println("Image min: " + imageMinVal + "  max: " + imageMaxVal);
            pw.println("Mask Pixel Count: " + pixelCount);
            pw.println("paint min: " + paintMinVal + "  max: " + paintMaxVal);
            pw.println("Number of bins: " + numBins);

            pw.println();
            pw.println("Image Histogram");
            for (int i = 0; i < numBins; i++) {
            	pw.println(i + imageMinVal + "  " + imageHistogram[i]);
            }

            for (int i = 0; i < numBins; i++) {
            	pw.println(i + imageMinVal + "  " + paintHistogram[i]);
            }
     
    		pw.close();
    	}
    	catch(IOException error) {
            errorCleanUp("PlugInAlgorithmImageStatistics::calc2D()  file error", true);
    	}
/*
    	// save the histogram of the paintMaskPixels
    	fileName = dirName + "regionHisto.txt";
    	File histoFile = new File(fileName);
    	try {
    		pw = new PrintWriter(new BufferedWriter(new FileWriter(histoFile)));
            for (int i = 0; i < numBins; i++) {
            	pw.println(i + imageMinVal + "  " + paintHistogram[i]);
            }
    		pw.close();
   		
    	}
       	catch(IOException error) {
            errorCleanUp("PlugInAlgorithmImageStatistics::calc2D()  file error", true);
    	}
*/    	
        // necessary for the result image to be displayed
        setCompleted(true);

    } // end calc2D()
    
    
    private void calc3D() {
        xDim = srcImage.getExtents()[0];
        yDim = srcImage.getExtents()[1];
        sliceSize = xDim * yDim;
    } // end calc3D()
    
    
} // end class PlugInAlgorithmImageStatistics
