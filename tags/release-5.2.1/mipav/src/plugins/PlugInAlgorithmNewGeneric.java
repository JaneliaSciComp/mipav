import gov.nih.mipav.model.algorithms.AlgorithmBase;
import gov.nih.mipav.model.algorithms.AlgorithmInterface;
import gov.nih.mipav.model.algorithms.AlgorithmVOIExtraction;
import gov.nih.mipav.model.file.FileIO;
import gov.nih.mipav.model.structures.ModelImage;
import gov.nih.mipav.view.MipavUtil;
import gov.nih.mipav.view.ViewJProgressBar;
import gov.nih.mipav.view.ViewUserInterface;



public class PlugInAlgorithmNewGeneric extends AlgorithmBase implements AlgorithmInterface {
    
    public void algorithmPerformed(AlgorithmBase algorithm) {
		System.out.println("Alhgorithm performed");
		
	}

	/** X dimension of the CT image */
    private int xDim;

    /** Y dimension of the CT image */
    private int yDim;

    /** Slice size for xDim*yDim */
    private int sliceSize;
    
    /**
     * Constructor.
     *
     * @param  resultImage  Result image model
     * @param  srcImg       Source image model.
     */
    public PlugInAlgorithmNewGeneric(ModelImage resultImage, ModelImage srcImg) {
        super(resultImage, srcImg);
    }
    
    /**
     * Starts the algorithm.
     */
    public void runAlgorithm() {
    	srcImage.getParentFrame().dispose();
    	Runtime inst = Runtime.getRuntime();
    	System.out.println("Total Memory = " + inst.totalMemory()
    						+ " Free Memory = " + inst.freeMemory());
    	
    	ViewUserInterface.getReference().openImageFrame();
    	inst = Runtime.getRuntime();
    	System.out.println("Total Memory = " + inst.totalMemory()
				+ " Free Memory = " + inst.freeMemory());
    	ModelImage image = ViewUserInterface.getReference().getActiveImageFrame().getImageA();
    	
    	inst = Runtime.getRuntime();
    	System.out.println("Total Memory = " + inst.totalMemory()
				+ " Free Memory = " + inst.freeMemory());
    	
    	image.getParentFrame().dispose();
    	
    	System.out.println("Total Memory = " + inst.totalMemory()
				+ " Free Memory = " + inst.freeMemory());

    	
    	
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
    
    private void calc2D() {
    	
    	AlgorithmVOIExtraction VOIExtractionAlgo = new AlgorithmVOIExtraction(srcImage);
    	   VOIExtractionAlgo.addListener(this);
    	   ViewJProgressBar progressBar = new ViewJProgressBar(srcImage.getImageName(), "Extracting VOI ...", 0, 100, true);
    	   
    	           
    	      progressBar.setSeparateThread(false);
    	      VOIExtractionAlgo.addProgressChangeListener(progressBar);
    	      VOIExtractionAlgo.setProgressValues(0, 100);
    	       
    	          if (VOIExtractionAlgo.startMethod(Thread.MIN_PRIORITY) == false) {
    	        MipavUtil.displayError("A thread is already running on this object");
    	   }
    	
    	  
    	
    }
    
    private void calc3D() {
    	String voiDir = "C:\\Hello\\"+PlugInMuscleImageDisplay.VOI_DIR+"\\";
    	
        System.out.println("Attempting to save VOIs: "+srcImage.getVOIs().size()+"\tTo: "+voiDir);
        //VOI created
        srcImage.getParentFrame().saveAllVOIsTo(voiDir);
        srcImage.unregisterAllVOIs();
        //Load VOIs, but first check done
        
        
        
    	xDim = srcImage.getExtents()[0];
        yDim = srcImage.getExtents()[1];
        sliceSize = xDim * yDim;
        System.out.println("Done");
        int zDim = srcImage.getExtents()[2];
    }
}
