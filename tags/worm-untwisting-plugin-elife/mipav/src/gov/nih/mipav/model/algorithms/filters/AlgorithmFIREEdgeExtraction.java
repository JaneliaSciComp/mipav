package gov.nih.mipav.model.algorithms.filters;


import gov.nih.mipav.model.algorithms.*;
import gov.nih.mipav.model.structures.*;

import java.io.*;


/**
   
   References: 
   1.) Digital Image Processing Third Edition by Rafael C. Gonzalez and Richard E. Woods, Section 3.8.5
   Using Fuzzy Sets for Spatial Filtering, Pearson Prentice Hall, 2008, pp. 189-191.
   2.) Russo F. and Ramponi G., "Edge Extraction by FIRE Operators", FUZZ-IEEE '94, 1994, vol 1, pp. 249-253,
   IEEE Press, New York.
 */
public class AlgorithmFIREEdgeExtraction extends AlgorithmBase {

    //~ Static fields/initializers -------------------------------------------------------------------------------------

    
    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** Standard deviation of the gaussian used in determining the zero membership function from intensity
     * differences */
	private double gaussianStdDev;
    /** Gray scale value at which the white membership function begins the linear transition from 0 to 1
     *  at the highest intensity value*/
    private double whiteStart;
    /** gray scale value at which the black membership function ends the linear transition from 1 at the
     * lowest gray scale value to 0 */
    private double blackEnd;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Constructor for images in which changes are returned to the source image.
     *
     * @param  srcImg    Source image model.
     * @param  gaussianStdDev
     * @param  whiteStart
     * @param  blackEnd
     */
    public AlgorithmFIREEdgeExtraction(ModelImage srcImg, double gaussianStdDev, double whiteStart, double blackEnd) {
        super(null, srcImg);

        this.gaussianStdDev = gaussianStdDev;
        this.whiteStart = whiteStart;
        this.blackEnd = blackEnd;
    }

    /**
     * Constructor for images in which changes are placed in a predetermined destination image.
     *
     * @param  destImg   Image model where result image is stored.
     * @param  srcImg    Source image model.
     * @param  gaussianStdDev
     * @param  whiteStart
     * @param  blackEnd
     */
    public AlgorithmFIREEdgeExtraction(ModelImage destImg, ModelImage srcImg, double gaussianStdDev, 
    		                           double whiteStart, double blackEnd) {

        super(destImg, srcImg);

        this.gaussianStdDev = gaussianStdDev;
        this.whiteStart = whiteStart;
        this.blackEnd = blackEnd;
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

        

        fireProgressStateChanged(srcImage.getImageName(), " Edge Extraction by FIRE Operators ...");
        
        if (destImage != null) { // if there exists a destination image
            calcStoreInDest();
            
        } else { // there is no image but the original source.
            calcStoreInPlace();
            
        }
    }


    /**
     * Edge extraction by FIRE operators of the source image. Replaces the original image with the processed image.
     */
    private void calcStoreInPlace() {

    	int xDim = srcImage.getExtents()[0];
        int yDim = srcImage.getExtents()[1];
        int sliceSize = xDim * yDim;
        int volSize;
        int zDim;
        int tDim;
        int x, y, z, t;
        int nDims = srcImage.getNDims();
        double[] buffer; // data-buffer (for pixel data) which is the "heart" of the image
        int i;
        double srcMin = srcImage.getMin();
        double srcMax = srcImage.getMax();
        double d2, d4, d6, d8;
        double z2, z4, z6, z8;
        double expDenom = 2.0 * gaussianStdDev * gaussianStdDev;
        double zeroMembership;
        double zm;
        double notZeroMembership;
        double whiteDenom = srcMax - whiteStart;
        double blackDenom = blackEnd - srcMin;
        double whiteClampingIntensity;
        double clampedWhiteMoment;
        double clampedWhiteArea;
        double whiteIntensityCenterOfMass;
        double blackClampingIntensity;
        double clampedBlackMoment;
        double clampedBlackArea;
        double blackIntensityCenterOfMass;
        
        if (nDims > 2) {
        	zDim = srcImage.getExtents()[2];
        }
        else {
        	zDim = 1;
        }
        volSize = sliceSize * zDim;
        if (nDims > 3) {
        	tDim = srcImage.getExtents()[3];
        }
        else {
        	tDim = 1;
        }
        
        buffer = new double[sliceSize];
        
        for (t = 0; t < tDim; t++) {
        	for (z = 0; z < zDim; z++) {
		
		        try {
		        	
		            srcImage.exportData(t*volSize + z*sliceSize, sliceSize, buffer); // locks and releases lock
		        } catch (IOException error) {
		            buffer = null;
		            errorCleanUp("Algorithm FIRE Edge Extraction reports: source image locked", true);
		
		            return;
		        } catch (OutOfMemoryError e) {
		            buffer = null;
		            errorCleanUp("Algorithm FIRE Edge Extraction reports: out of memory", true);
		
		            return;
		        }
		        
		        for (y = 0; y < yDim; y++) {
		        	for (x = 0; x < xDim; x++) {
		        	    i = x + y*xDim;
		        	    z2 = Double.POSITIVE_INFINITY;
		        	    z4 = Double.POSITIVE_INFINITY;
		        	    z6 = Double.POSITIVE_INFINITY;
		        	    z8 = Double.POSITIVE_INFINITY;
		        	    zeroMembership = 1.0;
		        	    if (y > 0) {
		        	    	d2 = buffer[i - xDim] - buffer[i];
		        	    	z2 = Math.exp(-d2*d2/expDenom);
		        	    }
		        	    if (y < yDim - 1) {
		        	        d8 = buffer[i + xDim] - buffer[i];	
		        	        z8 = Math.exp(-d8*d8/expDenom);
		        	    }
		        	    if (x > 0) {
		        	    	d4 = buffer[i - 1] - buffer[i];
		        	    	z4 = Math.exp(-d4*d4/expDenom);
		        	    }
		        	    if (x < xDim-1) {
		        	    	d6 = buffer[i + 1] - buffer[i];
		        	    	z6 = Math.exp(-d6*d6/expDenom);
		        	    }
		        	    if ((!Double.isInfinite(z2)) &&(!Double.isInfinite(z6)) ) {
		        	        zeroMembership = Math.min(z2, z6);
		        	    }
		        	    if ((!Double.isInfinite(z6)) && (!Double.isInfinite(z8))) {
		        	        zm = Math.min(z6,z8);
		        	        zeroMembership = Math.min(zeroMembership, zm);
		        	    }
		        	    if ((!Double.isInfinite(z8)) && (!Double.isInfinite(z4))) {
		        	        zm = Math.min(z8,z4);
		        	        zeroMembership = Math.min(zeroMembership, zm);
		        	    }
		        	    if ((!Double.isInfinite(z4)) && (!Double.isInfinite(z2))) {
		        	        zm = Math.min(z4,z2);
		        	        zeroMembership = Math.min(zeroMembership, zm);
		        	    }
		        	    // zeroMembership = (whiteClampingIntensity - whiteStart)/whiteDenom
		        	    whiteClampingIntensity = whiteStart + zeroMembership*whiteDenom;
		        	    // clampedWhiteMoment
		        	    // Integral from whiteStart to whiteClampingIntensity of
		        	    // intensity*((intensity - whiteStart)/whiteDenom)dintensity
		        	    // + Integral from whiteClampingIntensity to srcMax of
		        	    // intensity * zeroMembership
		        	    clampedWhiteMoment = (whiteClampingIntensity*whiteClampingIntensity*whiteClampingIntensity)/(3.0*whiteDenom)
		        	    - (whiteStart*whiteStart*whiteStart)/(3.0*whiteDenom) 
		        	    - (whiteStart*whiteClampingIntensity*whiteClampingIntensity)/(2.0*whiteDenom)
		        	    + (whiteStart*whiteStart*whiteStart)/(2.0*whiteDenom)
		        	    + (srcMax*srcMax*zeroMembership)/2.0
		        	    - (whiteClampingIntensity*whiteClampingIntensity*zeroMembership)/2.0;
		        	    clampedWhiteArea = 0.5*(whiteClampingIntensity - whiteStart)*zeroMembership +
		        	                       (srcMax - whiteClampingIntensity)*zeroMembership;
		        	    whiteIntensityCenterOfMass = clampedWhiteMoment/clampedWhiteArea;
		        	    notZeroMembership = 1.0 - zeroMembership;
		        	    // notZeroMembership = 1.0 - (blackClampingIntensity - srcMin)/blackDenom;
		        	    blackClampingIntensity = srcMin + blackDenom*(1.0 - notZeroMembership);
		        	    // clampedBlackMoment
		        	    // Integral from srcMin to blackClampingIntensity of
		        	    // (intensity * notZeroMembership)dintensity
		        	    // + Integral from blackClampingIntensity to blackEnd of
		        	    // intensity*(1.0 - (intensity - srcMin)/blackDenom)dintensity
		        	    clampedBlackMoment = blackClampingIntensity*blackClampingIntensity*notZeroMembership/2.0
		        	    - srcMin*srcMin*notZeroMembership/2.0
		        	    + blackEnd*blackEnd/2.0
		        	    - blackClampingIntensity*blackClampingIntensity/2.0
		        	    - (blackEnd*blackEnd*blackEnd)/(3.0*blackDenom)
		        	    + (blackClampingIntensity*blackClampingIntensity*blackClampingIntensity)/(3.0*blackDenom)
		        	    + (srcMin*blackEnd*blackEnd)/(2.0*blackDenom)
		        	    - (srcMin*blackClampingIntensity*blackClampingIntensity)/(2.0*blackDenom);
		        	    clampedBlackArea = 0.5*(blackEnd - blackClampingIntensity)*notZeroMembership
	                      + (blackClampingIntensity - srcMin)*notZeroMembership;
		        	    blackIntensityCenterOfMass = clampedBlackMoment/clampedBlackArea;
		        	    buffer[i] = (whiteIntensityCenterOfMass*zeroMembership*clampedWhiteArea +
		        	    		     blackIntensityCenterOfMass*notZeroMembership*clampedBlackArea)/
		        	    		     (zeroMembership*clampedWhiteArea + notZeroMembership*clampedBlackArea);
		        	}
		        } // for (y = 0; y < yDim; y++)
		        
		        if (threadStopped) {
		            finalize();
		
		            return;
		        }
		
		        try { // but now place buffer data into the image
		            srcImage.importData(t*volSize + z*sliceSize, buffer, false);
		        } catch (IOException error) {
		            buffer = null;
		            errorCleanUp("Algorithm FIRE Edge Extraction: IOException on srcImage.importData", false);
		
		            return;
		        }
        	} // for (z = 0; z < zDim; z++)
        } // for (t = 0; t < tDim; t++)
        srcImage.calcMinMax();

        setCompleted(true);
    }

    

    /**
     * This function produces a new image that has had edge extraction by FIRE operators
     */
    private void calcStoreInDest() {

        int xDim = srcImage.getExtents()[0];
        int yDim = srcImage.getExtents()[1];
        int sliceSize = xDim * yDim;
        int volSize;
        int zDim;
        int tDim;
        int x, y, z, t;
        int nDims = srcImage.getNDims();
        double[] buffer; // data-buffer (for pixel data) which is the "heart" of the image
        int i;
        double srcMin = srcImage.getMin();
        double srcMax = srcImage.getMax();
        double d2, d4, d6, d8;
        double z2, z4, z6, z8;
        double expDenom = 2.0 * gaussianStdDev * gaussianStdDev;
        double zeroMembership;
        double zm;
        double notZeroMembership;
        double whiteDenom = srcMax - whiteStart;
        double blackDenom = blackEnd - srcMin;
        double whiteClampingIntensity;
        double clampedWhiteMoment;
        double clampedWhiteArea;
        double whiteIntensityCenterOfMass;
        double blackClampingIntensity;
        double clampedBlackMoment;
        double clampedBlackArea;
        double blackIntensityCenterOfMass;
        
        if (nDims > 2) {
        	zDim = srcImage.getExtents()[2];
        }
        else {
        	zDim = 1;
        }
        volSize = sliceSize * zDim;
        if (nDims > 3) {
        	tDim = srcImage.getExtents()[3];
        }
        else {
        	tDim = 1;
        }
        
        buffer = new double[sliceSize];
        
        for (t = 0; t < tDim; t++) {
        	for (z = 0; z < zDim; z++) {
		        try {
		            destImage.setLock(ModelStorageBase.RW_LOCKED);
		        } catch (IOException error) {
		            errorCleanUp("Algorithm FIRE Edge Extraction reports: destination image locked" + error, false);
		
		            return;
		        }
		
		        try {
		        	
		            srcImage.exportData(t*volSize + z*sliceSize, sliceSize, buffer); // locks and releases lock
		        } catch (IOException error) {
		            buffer = null;
		            errorCleanUp("Algorithm FIRE Edge Extraction reports: source image locked", true);
		
		            return;
		        } catch (OutOfMemoryError e) {
		            buffer = null;
		            errorCleanUp("Algorithm FIRE Edge Extraction reports: out of memory", true);
		
		            return;
		        }
		        
		        for (y = 0; y < yDim; y++) {
		        	for (x = 0; x < xDim; x++) {
		        	    i = x + y*xDim;
		        	    z2 = Double.POSITIVE_INFINITY;
		        	    z4 = Double.POSITIVE_INFINITY;
		        	    z6 = Double.POSITIVE_INFINITY;
		        	    z8 = Double.POSITIVE_INFINITY;
		        	    zeroMembership = 1.0;
		        	    if (y > 0) {
		        	    	d2 = buffer[i - xDim] - buffer[i];
		        	    	z2 = Math.exp(-d2*d2/expDenom);
		        	    }
		        	    if (y < yDim - 1) {
		        	        d8 = buffer[i + xDim] - buffer[i];	
		        	        z8 = Math.exp(-d8*d8/expDenom);
		        	    }
		        	    if (x > 0) {
		        	    	d4 = buffer[i - 1] - buffer[i];
		        	    	z4 = Math.exp(-d4*d4/expDenom);
		        	    }
		        	    if (x < xDim-1) {
		        	    	d6 = buffer[i + 1] - buffer[i];
		        	    	z6 = Math.exp(-d6*d6/expDenom);
		        	    }
		        	    if ((!Double.isInfinite(z2)) &&(!Double.isInfinite(z6)) ) {
		        	        zeroMembership = Math.min(z2, z6);
		        	    }
		        	    if ((!Double.isInfinite(z6)) && (!Double.isInfinite(z8))) {
		        	        zm = Math.min(z6,z8);
		        	        zeroMembership = Math.min(zeroMembership, zm);
		        	    }
		        	    if ((!Double.isInfinite(z8)) && (!Double.isInfinite(z4))) {
		        	        zm = Math.min(z8,z4);
		        	        zeroMembership = Math.min(zeroMembership, zm);
		        	    }
		        	    if ((!Double.isInfinite(z4)) && (!Double.isInfinite(z2))) {
		        	        zm = Math.min(z4,z2);
		        	        zeroMembership = Math.min(zeroMembership, zm);
		        	    }
		        	    // zeroMembership = (whiteClampingIntensity - whiteStart)/whiteDenom
		        	    whiteClampingIntensity = whiteStart + zeroMembership*whiteDenom;
		        	    // clampedWhiteMoment
		        	    // Integral from whiteStart to whiteClampingIntensity of
		        	    // intensity*((intensity - whiteStart)/whiteDenom)dintensity
		        	    // + Integral from whiteClampingIntensity to srcMax of
		        	    // intensity * zeroMembership
		        	    clampedWhiteMoment = (whiteClampingIntensity*whiteClampingIntensity*whiteClampingIntensity)/(3.0*whiteDenom)
		        	    - (whiteStart*whiteStart*whiteStart)/(3.0*whiteDenom) 
		        	    - (whiteStart*whiteClampingIntensity*whiteClampingIntensity)/(2.0*whiteDenom)
		        	    + (whiteStart*whiteStart*whiteStart)/(2.0*whiteDenom)
		        	    + (srcMax*srcMax*zeroMembership)/2.0
		        	    - (whiteClampingIntensity*whiteClampingIntensity*zeroMembership)/2.0;
		        	    clampedWhiteArea = 0.5*(whiteClampingIntensity - whiteStart)*zeroMembership +
		        	                       (srcMax - whiteClampingIntensity)*zeroMembership;
		        	    whiteIntensityCenterOfMass = clampedWhiteMoment/clampedWhiteArea;
		        	    notZeroMembership = 1.0 - zeroMembership;
		        	    // notZeroMembership = 1.0 - (blackClampingIntensity - srcMin)/blackDenom;
		        	    blackClampingIntensity = srcMin + blackDenom*(1.0 - notZeroMembership);
		        	    // clampedBlackMoment
		        	    // Integral from srcMin to blackClampingIntensity of
		        	    // (intensity * notZeroMembership)dintensity
		        	    // + Integral from blackClampingIntensity to blackEnd of
		        	    // intensity*(1.0 - (intensity - srcMin)/blackDenom)dintensity
		        	    clampedBlackMoment = blackClampingIntensity*blackClampingIntensity*notZeroMembership/2.0
		        	    - srcMin*srcMin*notZeroMembership/2.0
		        	    + blackEnd*blackEnd/2.0
		        	    - blackClampingIntensity*blackClampingIntensity/2.0
		        	    - (blackEnd*blackEnd*blackEnd)/(3.0*blackDenom)
		        	    + (blackClampingIntensity*blackClampingIntensity*blackClampingIntensity)/(3.0*blackDenom)
		        	    + (srcMin*blackEnd*blackEnd)/(2.0*blackDenom)
		        	    - (srcMin*blackClampingIntensity*blackClampingIntensity)/(2.0*blackDenom);
		        	    clampedBlackArea = 0.5*(blackEnd - blackClampingIntensity)*notZeroMembership
	                      + (blackClampingIntensity - srcMin)*notZeroMembership;
		        	    blackIntensityCenterOfMass = clampedBlackMoment/clampedBlackArea;
		        	    buffer[i] = (whiteIntensityCenterOfMass*zeroMembership*clampedWhiteArea +
		        	    		     blackIntensityCenterOfMass*notZeroMembership*clampedBlackArea)/
		        	    		     (zeroMembership*clampedWhiteArea + notZeroMembership*clampedBlackArea);
		        	}
		        } // for (y = 0; y < yDim; y++)
		        
		        
		        destImage.releaseLock(); // we didn't want to allow the image to be adjusted by someone else
		        
		       
		        if (threadStopped) {
		            finalize();
		
		            return;
		        }
		
		        try { // but now place buffer data into the image
		            destImage.importData(t*volSize + z*sliceSize, buffer, false);
		        } catch (IOException error) {
		            buffer = null;
		            errorCleanUp("Algorithm FIRE Edge Extraction reports: destination image still locked", true);
		
		            return;
		        }
        	} // for (z = 0; z < zDim; z++)
        } // for (t = 0; t < tDim; t++)
        destImage.calcMinMax();

        setCompleted(true);
    }

   
}
