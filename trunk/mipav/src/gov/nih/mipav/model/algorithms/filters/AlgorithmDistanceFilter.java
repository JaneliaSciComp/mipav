package gov.nih.mipav.model.algorithms.filters;


import gov.nih.mipav.model.algorithms.AlgorithmBase;
import gov.nih.mipav.model.file.FileInfoBase;
import gov.nih.mipav.model.structures.ModelImage;
import gov.nih.mipav.model.structures.ModelStorageBase;
import gov.nih.mipav.model.structures.VOIBase;
import gov.nih.mipav.model.structures.VOIContour;
import gov.nih.mipav.model.structures.VOIVector;
import gov.nih.mipav.view.MipavUtil;
import gov.nih.mipav.view.ViewJFrameImage;

import java.awt.Dimension;
import java.io.IOException;
import java.util.Arrays;
import java.util.Vector;



public class AlgorithmDistanceFilter extends AlgorithmBase {

    
    
    /** DOCUMENT ME! */
    private float[] finalData; // final data

    /** DOCUMENT ME! */
    private float[] iKernel; // ideal kernel data

    /** DOCUMENT ME! */
    private float[] imagData; // imaginary data

    /** DOCUMENT ME! */
    private float[] imagKernelData; // imaginary kernel data

    /** DOCUMENT ME! */
    private int kDim; // kernel diameter

    /** DOCUMENT ME! */
    private float minimum, maximum;

    /** DOCUMENT ME! */
    private int ndim; // number of dimensions

    /** DOCUMENT ME! */
    private int newArrayLength; // size of zero padded buffers

    /** DOCUMENT ME! */
    private int[] newDimLengths; // zero padded dimension sizes

    /** DOCUMENT ME! */
    private int newSliceSize;

    /** DOCUMENT ME! */
    private int originalArrayLength; // size of original buffers (realData and imagData)

    /** DOCUMENT ME! */
    private int[] originalDimLengths; // original dimension sizes

    /** DOCUMENT ME! */
    private float[] realData; // real data

    /** DOCUMENT ME! */
    private float[] realKernelData; // real kernel data
    //~ Constructors ---------------------------------------------------------------------------------------------------

    private float[] originalBuffer;
  
    private VOIVector VOIs;
    
    private boolean distanceFilter;
    
    public AlgorithmDistanceFilter(ModelImage destImg, ModelImage srcImg, boolean distanceFilter ) {
        super(destImg, srcImg);
       
        this.distanceFilter = distanceFilter;
        System.err.println("ruida");
    }

    
    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * Prepares this class for destruction.
     */
    public void finalize() {
        /*
        realData = null; // real data
        imagData = null; // imaginary data
        realKernelData = null; // real kernel data
        imagKernelData = null; // imaginary kernel data
        iKernel = null; // ideal kernel data
        destImage = null;
        srcImage = null;
        super.finalize();
        */
    }

    /**
     * Returns reference to imaginary data array.
     *
     * @return  the reference the the imaginary datat array
     */
    public float[] getImaginaryData() {
        return imagData;
    }

        /**
     * Returns reference to real data array.
     *
     * @return  the reference the the real datat array
     */
    public float[] getRealData() {
        return realData;
    }

    /**
     * Starts the program.
     */
    public void runAlgorithm() {

        if (srcImage == null) {
            displayError("Source Image is null");

            return;
        }

        calcDistance();
        
    }


    private void calcDistance() {
    	int xDim = srcImage.getExtents()[0];
        int yDim = srcImage.getExtents()[1];
        int sliceSize = xDim * yDim;
        int centerX, centerY;
        int pos = 0;
        if ( srcImage.getVOIs() != null ) {
        	this.VOIs = srcImage.getVOIs();
        }
        
    	try {
	        originalBuffer = new float[sliceSize];
	        srcImage.exportData(0, sliceSize, originalBuffer);
    	}  catch (IOException error) {
			MipavUtil
			.displayError("I/O error: unable to import original buffer");
    	}	
    	
    	int centerPos = sliceSize / 2;
    	int xPos, yPos;
    	centerX = centerPos%xDim+ xDim/2;
    	centerY = centerPos/xDim;
    	
    	
    	
    	for (int y = 0; (y <= yDim-1); y++) {

			for (int x = 0; x <= xDim-1; x++) {
				pos = x + (y * xDim);
				
				xPos = pos % xDim;
				yPos = pos / xDim;
				
				if ( distanceFilter ) {
					originalBuffer[pos] = (float)Math.sqrt((xPos-centerX)*(xPos-centerX) + (yPos-centerY)*(yPos-centerY));  // calc distance
				} 
				
				if ( VOIs.size() > 0 ) {
					
				    Vector<VOIBase>[] vArray = VOIs.VOIAt(0).getCurves();
				    
				    if ( vArray != null ) {
				    	if ( vArray[0] != null && vArray[0].size() > 0 ) {
							VOIBase v  = vArray[0].get(0);
							if ( v instanceof VOIContour ) {
								if ( ((VOIContour)v).contains(x, y, false) ) {
										originalBuffer[pos] = originalBuffer[pos] * 1.0f;
								} else {
									    // System.err.println("1. originalBuffer[pos] = " + originalBuffer[pos]);
										originalBuffer[pos] = originalBuffer[pos] * -1.0f;
										// System.err.println("2. originalBuffer[pos] = " + originalBuffer[pos]);
								}
							}
				    	}
				      
				    }
				}
				
				
			}
	
		}
    	
    	try {
	        	destImage.importData(0, originalBuffer, true);
	        	destImage.calcMinMax();
	        	destImage.notifyImageDisplayListeners(null, true);
	            // new ViewJFrameImage(destImage);
			} catch (IOException error) {
				MipavUtil
						.displayError("I/O error: unable to import original buffer");
			}	
			
	   setCompleted(true);
    	
    }
    

 
 
}
