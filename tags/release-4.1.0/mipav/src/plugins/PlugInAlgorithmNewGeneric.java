import gov.nih.mipav.model.algorithms.AlgorithmBase;
import gov.nih.mipav.model.structures.ModelImage;



public class PlugInAlgorithmNewGeneric extends AlgorithmBase {
    
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

        if (srcImage == null) {
            displayError("Source Image is null");

            return;
        }

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
    
    private void calc2D() {
        
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
