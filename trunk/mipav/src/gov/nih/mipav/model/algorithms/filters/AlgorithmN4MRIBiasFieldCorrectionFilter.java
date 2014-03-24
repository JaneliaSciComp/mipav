	package gov.nih.mipav.model.algorithms.filters;
	
	
	import gov.nih.mipav.model.algorithms.AlgorithmBase;
	import gov.nih.mipav.model.algorithms.AlgorithmConvolver;
	import gov.nih.mipav.model.algorithms.AlgorithmInterface;
	import gov.nih.mipav.model.algorithms.GenerateGaussian;
	import gov.nih.mipav.model.structures.ModelImage;
	
	import java.io.IOException;
	import gov.nih.mipav.view.ViewJProgressBar;
	
	
	/**
	 
	 *
	 * @version  0.1 March 19, 2014
	 * @author   William Gandler
	 */
	public class AlgorithmN4MRIBiasFieldCorrectionFilter extends AlgorithmBase {
	
	    //~ Instance fields ------------------------------------------------------------------------------------------------
	
	    /**
	     * Flag, if true, indicates that the whole image should be processed. If false on process the image over the mask
	     * areas.
	     */
	    private boolean entireImage;
	   
	    /* Assigned to srcImage if replace image, assigned to destImage if new image */
	    private ModelImage targetImage = null;
	    
	    private double imageMax;
	    
	    private int maximumNumberOfLevels = 1;
	    
	    private int maximumNumberOfIterationsArray[] = new int[maximumNumberOfLevels];
	    
	    private int maximumNumberOfIterations = 50;
	    
	    private double convergenceThreshold = 0.001;
	    
	    private int length;
	
	    //~ Constructors ---------------------------------------------------------------------------------------------------
	
	    /**
	     * Creates a new AlgorithmN4MRIBiasFieldCorrectionFilter.
	     *
	     * @param  srcImg    DOCUMENT ME!
	     * @param  maskFlag  DOCUMENT ME!
	     */
	    public AlgorithmN4MRIBiasFieldCorrectionFilter(ModelImage srcImg, boolean maskFlag) {
        this(null, srcImg, maskFlag);
    }

    /**
     * Constructor which sets the source and destination images
     *
     * @param  destImg   the destination image
     * @param  srcImg    the source image
     * @param  maskFlag  the mask flag
     * @param  img25D    the 2.5D indicator
     */
    public AlgorithmN4MRIBiasFieldCorrectionFilter(ModelImage destImg, ModelImage srcImg, boolean maskFlag) {
        super(destImg, srcImg);
        entireImage = maskFlag;
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
        int nDims;
        int i;
        double buffer[];
        double logFilter[];
        double logUncorrected[];
        double logBiasField[];
        double logSharpened[];
        double subtracter1[];
        double newLogBiasField[];
        int currentLevel;
        int elapsedIterations = 0;
        double currentConvergenceMeasurement = Double.MAX_VALUE;

        if (srcImage == null) {
            displayError("Source Image is null");
            finalize();

            return;
        }
        

        
        long startTime = System.nanoTime();
        fireProgressStateChanged(0, srcImage.getImageName(), "N4 MRI Bias Field Correction Filter on image ...");
        
        for (i = 0; i < maximumNumberOfLevels; i++) {
        	maximumNumberOfIterationsArray[i] = maximumNumberOfIterations;
        }
        
        if (destImage == null) {
            targetImage = srcImage;
        }
        else {
            targetImage = destImage;
        }
        
        srcImage.calcMinMax();
        imageMax = (float)srcImage.getMax();
        nDims = srcImage.getNDims();
        length = srcImage.getExtents()[0];
        for (i = 1; i < nDims; i++) {
        	length *= srcImage.getExtents()[i];
        }
        buffer = new double[length];
        logFilter = new double[length];
        logUncorrected = new double[length];
        
        try {
            srcImage.exportData(0, length, buffer); // locks and releases lock
        } catch (IOException error) {
            displayError("Algorithm N4 MRI Bias Field Correction Filter: Image(s) locked");
            setCompleted(false);
            fireProgressStateChanged(ViewJProgressBar.PROGRESS_WINDOW_CLOSING);
            srcImage.releaseLock();

            return;
        }
        
        // Calculate the log of the input image
        // Set NaNs, infinities, and negatives to zero
        for (i = 0; i < length; i++) {
        	if (entireImage || mask.get(i)) {
                logFilter[i] = Math.log(buffer[i]);
                if ((Double.isNaN(logFilter[i])) || (Double.isInfinite(logFilter[i])) || (logFilter[i] < 0.0)) {
                	logFilter[i] = 0.0;
                }
                logUncorrected[i] = logFilter[i];
        	}
        } // for (i = 0; i < length; i++)
        
        // Provide an initial log bias field of zeros.
        logBiasField = new double[length];
        
        subtracter1 = new double[length];
        for (currentLevel = 0; currentLevel < maximumNumberOfLevels; currentLevel++) {
            elapsedIterations = 0;
            currentConvergenceMeasurement = Double.MAX_VALUE;
            while ((elapsedIterations++ < maximumNumberOfIterationsArray[currentLevel]) &&
            	   (currentConvergenceMeasurement > convergenceThreshold)) {
                // Sharpen the current estimate of the uncorrected image
            	logSharpened = sharpen(logUncorrected);
            	for (i = 0; i < length; i++) {
            		if (entireImage || mask.get(i)) {
            		    subtracter1[i] = logUncorrected[i] - logSharpened[i];    	
            		}
            	}
            	
            	// Smooth the residual bias field estimate and add the resulting
            	// control point grid to get the new total bias field estimate.
            	newLogBiasField = updateBiasFieldEstimate(subtracter1);
            	currentConvergenceMeasurement = calculateConvergenceMeasurement(logBiasField, newLogBiasField);
            	logBiasField = newLogBiasField;
            	for (i = 0; i < length; i++) {
            		if (entireImage || mask.get(i)) {
            			logUncorrected[i] = logFilter[i] - logBiasField[i];
            		}
            	}
            } // while ((elapsedIterations++ < maximumNumberOfIterationsArray[currentLevel]) &&
        } // for (currentLevel = 0; currentLevel < maximumNumberOfLevels; currentLevel++)
        
        setCompleted(true);
        //System.out.println("Time consumed GB: " + (System.nanoTime()-startTime));
    }
    
    private double[]sharpen(double[] unsharpened) {
    	double sharpened[] = new double[unsharpened.length];
    	return sharpened;
    }
    
    private double[] updateBiasFieldEstimate(double[] fieldEstimate) {
    	double smoothField[] = new double[fieldEstimate.length];
    	return smoothField;
    }
    
    private double calculateConvergenceMeasurement(double[] fieldEstimate1, double[] fieldEstimate2) {
    	int i;
    	double subtracter[] = new double[fieldEstimate1.length];
    	for (i = 0; i < length; i++) {
    		if (entireImage || mask.get(i)) {
    			subtracter[i] = fieldEstimate1[i] - fieldEstimate2[i];
    		}
    	}
    	
    	// Calculate statistics over the mask region
    	double mu = 0.0;
    	double sigma = 0.0;
    	double N = 0.0;
    	
    	for (i = 0; i < length; i++) {
    		if (entireImage || mask.get(i)) {
    			double pixel = Math.exp(subtracter[i]);
    			N += 1.0;
    			
    			if (N > 1.0) {
    				double diff = pixel - mu;
    				sigma = sigma + diff * diff * (N - 1.0) / N;
    			}
    			mu = mu * (1.0 - 1.0/N) + pixel/N;
    		}
    	}
    	sigma = Math.sqrt(sigma/(N - 1.0));
    	return (sigma/ mu);
    }
}

