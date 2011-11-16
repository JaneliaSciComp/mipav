package gov.nih.mipav.model.algorithms.filters;


import gov.nih.mipav.model.algorithms.*;
import gov.nih.mipav.model.structures.*;

import java.io.*;

import java.util.*;


/**
   This algorithm uses an equation with 3 membership functions, udark, ugray, and ubright, to transform a gray level g
   to a new gray level g' for histogram enhancement.
   g' = (udark(g)*gmin + ugray(g)*gmid + ubright(g)*gmax)/(udark(g) + ugray(g) + ubright(g))
   gmin and gmax are the minimum and maximum gray scale levels.
   By default gmid = (gmin + gmax)/2.0, but the user can select other values between gmin and gmax.
   Note that in reference 3 the authors used gmin = 0, gmid = 200, gmax = 255.
   udark(g) is a straight line segment going from 1 at gmin to 0 at gmid.
   ugray(g) has 2 line segments.  The first goes from 0 at gmin to 1 at gmid.  The second goes from 1
   at gmid to 0 at gmax.
   udark is a straight line segment going from 0 at gmid to 1 at gmax.
   
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

    
    /** Gray scale value at which ugray(g) = 1.0.
     * By default gmid = (gmin + gmax)/2.0, but it can assume any value between gmin and gmax */
    private double gmid;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Constructor for images in which changes are returned to the source image.
     *
     * @param  srcImg    Source image model.
     * @param  gmid      Gray scale value at which ugray(g) = 1.0
     */
    public AlgorithmRuleBasedContrastEnhancement(ModelImage srcImg, double gmid) {
        super(null, srcImg);

        this.gmid = gmid;
    }

    /**
     * Constructor for images in which changes are placed in a predetermined destination image.
     *
     * @param  destImg   Image model where result image is stored.
     * @param  srcImg    Source image model.
     * @param  gmid      Gray scale value at which ugray(g) = 1.0
     */
    public AlgorithmRuleBasedContrastEnhancement(ModelImage destImg, ModelImage srcImg, double gmid) {

        super(destImg, srcImg);

        this.gmid = gmid;
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
        double gmin;
        double gmax;
        double g;
        double udark;
        double ugray;
        double ubright;
        double[] resultBuffer;
        int i;
        ArrayList <Double> srcList = new ArrayList<Double>();
        int uniqueValues;
        int index;

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
        
        gmin = srcImage.getMin();
        gmax = srcImage.getMax();
        
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
        	   ugray = (g - gmin)/(gmid - gmin);
        	   udark = 1.0 - ugray;
        	   ubright = 0.0;
           }
           else {
        	   udark = 0.0;
        	   ubright = (g - gmid)/(gmax - gmid);
        	   ugray = 1.0 - ubright;
           }
           resultBuffer[i] = (udark*gmin + ugray*gmid + ubright*gmax)/(udark + ugray + ubright);
        } // for (i = 0; i < uniqueValues; i++)
        
        for (i = 0; i < length; i++) {
        	index = Collections.binarySearch(srcList, buffer[i]);
    	    buffer[i] = resultBuffer[index];
        }
        srcList.clear();
        
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
        double gmin;
        double gmax;
        double g;
        double udark;
        double ugray;
        double ubright;
        double[] resultBuffer; 
        int i;
        ArrayList <Double> srcList = new ArrayList<Double>();
        int uniqueValues;
        int index;

        try {
            destImage.setLock(ModelStorageBase.RW_LOCKED);
        } catch (IOException error) {
            errorCleanUp("Algorithm Rule Based Contrast Enhancement reports: destination image locked", false);

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
        
        gmin = srcImage.getMin();
        gmax = srcImage.getMax();
        
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
        	   ugray = (g - gmin)/(gmid - gmin);
        	   udark = 1.0 - ugray;
        	   ubright = 0.0;
           }
           else {
        	   udark = 0.0;
        	   ubright = (g - gmid)/(gmax - gmid);
        	   ugray = 1.0 - ubright;
           }
           resultBuffer[i] = (udark*gmin + ugray*gmid + ubright*gmax)/(udark + ugray + ubright);
        } // for (i = 0; i < uniqueValues; i++)
        
        for (i = 0; i < length; i++) {
        	index = Collections.binarySearch(srcList, buffer[i]);
    	    buffer[i] = resultBuffer[index];
        }
        srcList.clear();
        
       
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
