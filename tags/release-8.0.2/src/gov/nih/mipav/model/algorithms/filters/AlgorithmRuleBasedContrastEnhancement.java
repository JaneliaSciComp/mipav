package gov.nih.mipav.model.algorithms.filters;


import gov.nih.mipav.model.algorithms.*;
import gov.nih.mipav.model.algorithms.utilities.AlgorithmChangeType;
import gov.nih.mipav.model.structures.*;

import java.io.*;

import java.util.*;


/**
   This algorithm uses an equation with 3 membership functions, udark, ugray, and ubright, to transform a gray level g
   to a new gray level g' for histogram enhancement.  Data type is promoted if necessary.
   g' = (udark(g)*gmin + ugray(g)*gmid + ubright(g)*gmax)/(udark(g) + ugray(g) + ubright(g))
   gmin and gmax are the new user selected minimum and maximum gray scale levels, with 
   gmin <= srcMin and gmax >= srcMax.  Must have gmin < srcMin or gmax > srcMax.
   If we were to have gmin == srcMin and gmax = srcMax, then the image would be unchanged after processing.
   By default gmid = (srcMin + srcMax)/2.0, but the user can select other values between srcMin and srcMax.
   Note that in reference 3 the authors used gmin = 0, gmid = 200, gmax = 255.
   udark(g) has 2 straight line segments.  The first stays at 1 from gmin to srcMin.  The second goes from goes from 1 at srcMin to 0 at gmid.
   ugray(g) has 2 line segments.  The first goes from 0 at srcMin to 1 at gmid.  The second goes from 1
   at gmid to 0 at srcMax.
   udark has 2 straight line segments.  The first goes from 0 at gmid to 1 at srcMax.  The second stays at 1 from srcMax to gmax.
   
   References: 
   1.) Digital Image Processing Third Edition by Rafael C. Gonzalez and Richard E. Woods, Section 3.8.4
   Using Fuzzy Sets for Intensity Transformations, Pearson Prentice Hall, 2008, pp. 186-189.
   2.) Tizhoosh, H. R., "Fuzzy Image Enhancement: An Overview," in Fuzzy Techniques in Image Processing,
   E. Kerre and M. Nachtegael, eds., Springer-Verlag, New York, 2000, pp. 147-148.
   3.) Tizhoosh HR, Krell G, and Michaelis B., "On Fuzzy Image Enhancement of Megavoltage Images in Radiation Therapy,"
   in Proceedings of FUZZ-IEEE'97 (Barcelona), pp. 1399-1404, 1997.
 */
public class AlgorithmRuleBasedContrastEnhancement extends AlgorithmBase {

    //~ Static fields/initializers -------------------------------------------------------------------------------------

    
    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** The new image minimum.  Must have gmin <= srcMin */
	private double gmin;
    /** Gray scale value at which ugray(g) = 1.0.
     * By default gmid = (srcMin + srcMmax)/2.0, but it can assume any value between srcMin and srcMax */
    private double gmid;
    /** The new image maximum.  Must have gmax >= srcMax */
    private double gmax;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Constructor for images in which changes are returned to the source image.
     *
     * @param  srcImg    Source image model.
     * @param  gmin      The new image minimum
     * @param  gmid      Gray scale value at which ugray(g) = 1.0
     * @param  gmax      The new image maximum
     */
    public AlgorithmRuleBasedContrastEnhancement(ModelImage srcImg, double gmin, double gmid, double gmax) {
        super(null, srcImg);

        this.gmin = gmin;
        this.gmid = gmid;
        this.gmax = gmax;
    }

    /**
     * Constructor for images in which changes are placed in a predetermined destination image.
     *
     * @param  destImg   Image model where result image is stored.
     * @param  srcImg    Source image model.
     * @param  gmin      The new image minimum
     * @param  gmid      Gray scale value at which ugray(g) = 1.0
     * @param  gmax      The new image maximum
     */
    public AlgorithmRuleBasedContrastEnhancement(ModelImage destImg, ModelImage srcImg, double gmin, double gmid, double gmax) {

        super(destImg, srcImg);

        this.gmin = gmin;
        this.gmid = gmid;
        this.gmax = gmax;
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
     * Starts the algorithm.
     */
    public void runAlgorithm() {

        if (srcImage == null) {
            displayError("Source Image is null");

            return;
        }

        

        fireProgressStateChanged(srcImage.getImageName(), " Rule based contrast enhancement ...");
        
        if (destImage != null) { // if there exists a destination image
            calcStoreInDest();
            
        } else { // there is no image but the original source.
            calcStoreInPlace();
            
        }
    }


    /**
     * Rule based contrast enhancement of the source image. Replaces the original image with the processed image.
     */
    private void calcStoreInPlace() {

        int length; // total number of data-elements (pixels) in image
        double[] buffer; // data-buffer (for pixel data) which is the "heart" of the image
        double g;
        double udark;
        double ugray;
        double ubright;
        double[] resultBuffer;
        int i;
        ArrayList <Double> srcList = new ArrayList<Double>();
        int uniqueValues;
        int index;
        double srcMin = srcImage.getMin();
        double srcMax = srcImage.getMax();
        int newType;

        try {

           
            length = srcImage.getExtents()[0] * srcImage.getExtents()[1];
            if (srcImage.getNDims() > 2) {
            	length *= srcImage.getExtents()[2];
            	if (srcImage.getNDims() > 3) {
            		length *= srcImage.getExtents()[3];
            	}
            }

            buffer = new double[length];
            srcImage.exportData(0, length, buffer); // locks and releases lock
        } catch (IOException error) {
            buffer = null;
            resultBuffer = null;
            errorCleanUp("Algorithm Rule Based Contrast Enhancement: source image locked", true);

            return;
        } catch (OutOfMemoryError e) {
            buffer = null;
            resultBuffer = null;
            errorCleanUp("Algorithm Rule Based Contrast Enhancement: Out of memory when creating image buffer", true);

            return;
        }
        
        for (i = 0; i < length; i++) {
        	if (!srcList.contains(buffer[i])) {
    	        srcList.add(buffer[i]);	
    	    }	
        } // for (i = 0; i < length; i++)
        Collections.sort(srcList);
        uniqueValues = srcList.size();
        resultBuffer = new double[uniqueValues];
        for (i = 0; i < uniqueValues; i++) {
           g = srcList.get(i);
           if (g <= gmid) {
        	   ugray = (g - srcMin)/(gmid - srcMin);
        	   udark = 1.0 - ugray;
        	   ubright = 0.0;
           }
           else {
        	   udark = 0.0;
        	   ubright = (g - gmid)/(srcMax - gmid);
        	   ugray = 1.0 - ubright;
           }
           resultBuffer[i] = (udark*gmin + ugray*gmid + ubright*gmax)/(udark + ugray + ubright);
        } // for (i = 0; i < uniqueValues; i++)
        
        for (i = 0; i < length; i++) {
        	index = Collections.binarySearch(srcList, buffer[i]);
    	    buffer[i] = resultBuffer[index];
        }
        srcList.clear();
        
        newType = srcImage.getType();
        if (newType == ModelStorageBase.DOUBLE) {
        	
        }
        else if (newType == ModelStorageBase.FLOAT) {
        	if ((gmin < -Float.MAX_VALUE) || (gmax > Float.MAX_VALUE)) {
        		newType = ModelStorageBase.DOUBLE;
        	}
        }
        else if ((gmin < Long.MIN_VALUE) || (gmax > Long.MAX_VALUE)) {
        	newType = ModelStorageBase.DOUBLE;
        }
        else if (newType == ModelStorageBase.LONG) {
        	
        }
        else if (newType == ModelStorageBase.UINTEGER) {
        	if ((gmin < 0) || (gmax > 4294967295L)) {
        	    newType = ModelStorageBase.LONG;
        	}
        }
        else if ((gmin < Integer.MIN_VALUE) || (gmax > Integer.MAX_VALUE)) {
        	newType = ModelStorageBase.LONG;
        }
        else if (newType == ModelStorageBase.INTEGER) {
        	
        }
        else if (newType == ModelStorageBase.USHORT) {
        	if ((gmin < 0) || (gmax > 65535)) {
        		newType = ModelStorageBase.INTEGER;
        	}
        }
        else if ((gmin < -32768) || (gmax > 32767)) {
        	newType = ModelStorageBase.INTEGER;
        }
        else if (newType == ModelStorageBase.SHORT) {
        	
        }
        else if (newType == ModelStorageBase.UBYTE) {
        	if ((gmin < 0) || (gmax > 255)) {
        		newType = ModelStorageBase.SHORT;
        	}
        }
        else if (newType == ModelStorageBase.BYTE) {
        	if ((gmin < -128) || (gmax > 127)) {
        		newType = ModelStorageBase.SHORT;
        	}
        }
        
        if (srcImage.getType() != newType) {
        	AlgorithmChangeType convertType = new AlgorithmChangeType(srcImage, newType, srcImage.getMin(),
                    srcImage.getMax(), gmin, gmax, false);
            convertType.run();
            convertType = null;
        }
        
        if (threadStopped) {
            finalize();

            return;
        }
        

        try { // place buffer data into the image
            srcImage.importData(0, buffer, true);
        } catch (IOException error) {
            buffer = null;
            resultBuffer = null;
            errorCleanUp("Algorithm Rule Based Contrast Enhancement: Source image locked", true);

            return;
        }

        setCompleted(true);
    }

    

    /**
     * This function produces a new image that has had rule based contrast enhancement
     */
    private void calcStoreInDest() {

        int length; // total number of data-elements (pixels) in image
        double[] buffer; // data-buffer (for pixel data) which is the "heart" of the image
        double g;
        double udark;
        double ugray;
        double ubright;
        double[] resultBuffer; 
        int i;
        ArrayList <Double> srcList = new ArrayList<Double>();
        int uniqueValues;
        int index;
        double srcMin = srcImage.getMin();
        double srcMax = srcImage.getMax();
        
        try {
            destImage.setLock(ModelStorageBase.RW_LOCKED);
        } catch (IOException error) {
            errorCleanUp("Algorithm Fuzzy Minimization reports: destination image locked" + error, false);

            return;
        }

        try {
        	
        	length = srcImage.getExtents()[0] * srcImage.getExtents()[1];
            if (srcImage.getNDims() > 2) {
            	length *= srcImage.getExtents()[2];
            	if (srcImage.getNDims() > 3) {
            		length *= srcImage.getExtents()[3];
            	}
            }

            buffer = new double[length];
            srcImage.exportData(0, length, buffer); // locks and releases lock
        } catch (IOException error) {
            buffer = null;
            resultBuffer = null;
            errorCleanUp("Algorithm Rule Based Contrast Enhancement reports: source image locked", true);

            return;
        } catch (OutOfMemoryError e) {
            buffer = null;
            resultBuffer = null;
            errorCleanUp("Algorithm Rule Based Contrast Enhancement reports: out of memory", true);

            return;
        }
        
        for (i = 0; i < length; i++) {
        	if (!srcList.contains(buffer[i])) {
    	        srcList.add(buffer[i]);	
    	    }	
        } // for (i = 0; i < length; i++)
        Collections.sort(srcList);
        uniqueValues = srcList.size();
        resultBuffer = new double[uniqueValues];
        for (i = 0; i < uniqueValues; i++) {
           g = srcList.get(i);
           if (g <= gmid) {
        	   ugray = (g - srcMin)/(gmid - srcMin);
        	   udark = 1.0 - ugray;
        	   ubright = 0.0;
           }
           else {
        	   udark = 0.0;
        	   ubright = (g - gmid)/(srcMax - gmid);
        	   ugray = 1.0 - ubright;
           }
           resultBuffer[i] = (udark*gmin + ugray*gmid + ubright*gmax)/(udark + ugray + ubright);
        } // for (i = 0; i < uniqueValues; i++)
        
        for (i = 0; i < length; i++) {
        	index = Collections.binarySearch(srcList, buffer[i]);
    	    buffer[i] = resultBuffer[index];
        }
        srcList.clear();
        destImage.releaseLock(); // we didn't want to allow the image to be adjusted by someone else
        
       
        if (threadStopped) {
            finalize();

            return;
        }

        try { // but now place buffer data into the image
            destImage.importData(0, buffer, true);
        } catch (IOException error) {
            buffer = null;
            resultBuffer = null;
            errorCleanUp("Algorithm Rule Based Contrast Enhancement reports: destination image still locked", true);

            return;
        }

        setCompleted(true);
    }

   
}
