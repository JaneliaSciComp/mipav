package gov.nih.mipav.model.algorithms;

import java.io.IOException;

import gov.nih.mipav.model.structures.ModelHistogram;
import gov.nih.mipav.model.structures.ModelImage;
import gov.nih.mipav.model.structures.ModelStorageBase;



/**
 * @author pandyan
 * 
 * This algorithm calculates the cumulative histogram for an image
 * Color images need to supply what channel they want the cumulative histogram for
 *
 */
public class AlgorithmCumulativeHistogram extends AlgorithmBase {
	
	/** Reference to the image. */
    private ModelImage image;
    
    /** Reference to the histogram storage object. */
    private ModelHistogram histogram;
    
    /** int array of histogram values */
    private int[] histoBuffer = null;
    
    /** Indicates which channel of the RGB image the histogram should be calculated. 1=Red   2=Green  3=Blue */
    private int RGBOffset;
    
    
    /** Constructor for grey scale image
     * 
     * 
     * @param  histogram  model of a histogram for a RGB component
     * @param  image      model of the source imag
     *
     *
	 */
    public AlgorithmCumulativeHistogram(ModelHistogram histogram, ModelImage image) {
    	this.histogram = histogram;
    	this.image = image;
    }
    
    /**
     * Constructor for RGB image.
     *
     * @param  histogram  model of a histogram for a RGB component
     * @param  RGBOffset  correct offset for RED = 1 , GREEN = 2, or BLUE = 3 component to be exported
     * @param  image      model of the source image
     * 
     */
    public AlgorithmCumulativeHistogram(ModelHistogram histogram, int RGBOffset, ModelImage image) {
        this.histogram = histogram;
        this.RGBOffset = RGBOffset;
        this.image = image;
        
    }
    
    
    
    
    
    

	/**
	 * 
	 * @overide 
	 * 
	 * 
	 * 
	 */
	public void runAlgorithm() {
		int i;
        int length = 1;
        int bins = 1;
        float[] imgBuffer = null;
        histoBuffer = null;
        
        if (histogram == null) {
            displayError("Histogram is null");

            return;
        }

        if (image == null) {
            displayError("Source Image is null");

            return;
        }
        
        try {
            bins = 1;
            for (i = 0; i < histogram.getNDims(); i++) {
                bins *= histogram.getExtents()[i];
            }
            

            histoBuffer = new int[bins];
            histogram.exportData(0, bins, histoBuffer); // locks and releases lock
            fireProgressStateChanged("Histogram", "Calculating cumulative histogram...");
        } catch (IOException error) {
            errorCleanUp("Algorithm Cumulative Histogram: Histogram locked", false);

            return;
        } catch (OutOfMemoryError e) {
            errorCleanUp("Algorithm Cumulative Histogram: Out of memory", false);

            return;
        }
        
        
        int z, zStop;
        int value;
        double imageMax, imageMin;
        double divisor;
        double factor;

        switch (image.getType()) {

            case ModelStorageBase.BYTE:
                imageMin = -128;
                imageMax = 127;
                break;

            case ModelStorageBase.UBYTE:
                imageMin = 0;
                imageMax = 255;
                break;

            case ModelStorageBase.ARGB:
                imageMin = 0;
                imageMax = 255;
                break;

            case ModelStorageBase.ARGB_USHORT:
                if (RGBOffset == 1) {
                    imageMin = image.getMinR();
                    imageMax = image.getMaxR();
                } else if (RGBOffset == 2) {
                    imageMin = image.getMinG();
                    imageMax = image.getMaxG();
                } else {
                    imageMin = image.getMinB();
                    imageMax = image.getMaxB();
                }

                break;

            case ModelStorageBase.SHORT:
                imageMin = (double) image.getMin();
                imageMax = (double) image.getMax();
                break;

            default: {
                imageMin = (double) image.getMin();
                imageMax = (double) image.getMax();
                break;
            }
        }
        
        length = image.getSliceSize();
        imgBuffer = new float[length];

        if (image.getNDims() > 2) {
            zStop = image.getExtents()[2];
        } else {
            zStop = 1;
        }
        

       
        
        try {
            image.setLock(ModelStorageBase.W_LOCKED);
        } catch (IOException error) {
            errorCleanUp("Algorithm Histogram: Image locked", false);

            return;
        }
        
        for (z = 0; z < zStop; z++) {

            try {

                if (image.getType() == ModelStorageBase.COMPLEX) {
                    image.exportMagData(2 * z * length, length, imgBuffer);
                } else if (image.isColorImage()) {
                    image.exportRGBDataNoLock(RGBOffset, 4 * z * length, length, imgBuffer);
                } else {
                    image.exportDataNoLock(z * length, length, imgBuffer);
                }
            } catch (IOException error) {
                errorCleanUp("Algorithm Histogram: image bounds exceeded", false);
                image.releaseLock();

                return;
            }

            if ((image.getType() == ModelStorageBase.COMPLEX) && (image.getLogMagDisplay() == true)) {

                for (i = 0; i < length; i++) {
                    imgBuffer[i] = (float) (0.4342944819 * java.lang.Math.log(1 + imgBuffer[i]));
                }
            }

            divisor = imageMax - imageMin;

            if (divisor == 0) {
                divisor = 1;
            }

            factor = (bins - 1) / divisor;

            // This is the part that actually calculates the histogram
            // Calculate for the entire image
            for (i = 0; i < length; i++) {
            	value = (int) (((imgBuffer[i] - imageMin) * factor) + 0.5f);
                histoBuffer[value]++;
            }
        }

        fireProgressStateChanged(Math.round((float) (z + 1) / zStop * 100));
        
        //make it cumulative
        for(int k=histoBuffer.length-1;k>0;k--) {
        	for(int m=k-1;m>=0;m--) {
        		histoBuffer[k] = histoBuffer[k] + histoBuffer[m];
         	}
        }

        image.releaseLock();
        
        try {
            histogram.importData(0, histoBuffer, true); // locks and releases lock
        } catch (IOException error) {
            errorCleanUp("Algorithm Histogram: histogram locked", false);

            return;
        }
        
        
        imgBuffer = null;
        histoBuffer = null;
        setCompleted(true);
        System.gc();
      
	}

	
	/**
	 * This method returns the histogram array values
	 * 
	 * @return int[]
	 */
	public int[] getHistoBuffer() {
		return histoBuffer;
	}
	
	

}
