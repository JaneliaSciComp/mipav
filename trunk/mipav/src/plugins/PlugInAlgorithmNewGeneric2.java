import gov.nih.mipav.model.algorithms.AlgorithmBase;
import gov.nih.mipav.model.structures.ModelImage;

/**
 * The algorithm
 * 
 * 
 * @author senseneyj
 *
 */
public class PlugInAlgorithmNewGeneric2 extends AlgorithmBase {
    
	  	/** X dimension of the image */
	    private int xDim;

	    /** Y dimension of the image */
	    private int yDim;

	    /** Slice size for xDim*yDim */
	    private int sliceSize;
	    
	    /**
	     * Constructor.
	     *
	     * @param  resultImage  Result image model
	     * @param  srcImg       Source image model.
	     */
	    public PlugInAlgorithmNewGeneric2(ModelImage resultImage, ModelImage srcImg) {
	        super(resultImage, srcImg);
	        init();
	    }
	    
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
	     * Starts the algorithm.  At the conclusion of this method, AlgorithmBase reports to any
	     * algorithm listeners that this algorithm has completed.
	     */
	    public void runAlgorithm() {
	    	if(srcImage.getNDims() < 3) {
	    		calc2D();
	    	} else {
	    		calc3D();
	    	}
	    	
	    	setCompleted(true); //indicating to listeners that the algorithm completed successfully
	    	
	    } // end runAlgorithm()
	    
	//  ~ Methods --------------------------------------------------------------------------------------------------------

	    private void calc2D() {
	    	fireProgressStateChanged("Message 2D: "+srcImage.getImageName());
	    	for(int i=1; i<100; i++) {
	    		fireProgressStateChanged(i);
	    	}
	    }
	    
	    private void calc3D() {
	    	fireProgressStateChanged("Message 3D: "+srcImage.getImageName());
	    	for(int i=1; i<100; i++) {
	    		fireProgressStateChanged(i);
	    	}
	    }

		private void init() {
			xDim = srcImage.getExtents()[0];
	        yDim = srcImage.getExtents()[1];
	        sliceSize = xDim * yDim;
		}
}
