
import gov.nih.mipav.model.algorithms.AlgorithmBase;
import gov.nih.mipav.model.structures.*;
import gov.nih.mipav.view.*;

import java.awt.*;

/**
 * Creates an interface for working with Iceland CT images.
 * 
 * @author senseneyj
 *
 */
public class PlugInAlgorithmMuscleSegmentation extends AlgorithmBase {
    
    //~ Static fields --------------------------------------------------------------------------------------------------
    
    public static final Color[] colorPick = {Color.GREEN, Color.ORANGE, Color.CYAN, 
                                                Color.YELLOW, Color.MAGENTA};
    
    public static final String LUT_IMAGE = "lutImage.tif";
    public static final String VOI_IMAGE = "voiImage.tif";
    
    //~ Instance fields ------------------------------------------------------------------------------------------------    
    
    /** denotes the type of srcImg (see enum ImageType) */
    private PlugInMuscleImageDisplay.ImageType imageType; 
    
    /** denotes the symmetry of srcImage */
    private PlugInMuscleImageDisplay.Symmetry symmetry;
    
    /** the parent frame. */
    private Frame parentFrame;
       
	
    /**
     * Constructor.
     *
     * @param  resultImage  Result image model
     * @param  srcImg       Source image model.
     */
    public PlugInAlgorithmMuscleSegmentation(ModelImage srcImg, PlugInMuscleImageDisplay.ImageType imageType, Frame parentFrame) {
        super(null, srcImg);
        this.imageType = imageType;
        this.parentFrame = parentFrame;
    }
    
    //~ Methods --------------------------------------------------------------------------------------------------------
    
    /**
     * Starts the algorithm.
     */
    public void runAlgorithm() {
        
        if (srcImage == null) {
            displayError("Source Image is null");

            return;
        }

        switch (imageType) {
            
            case Abdomen:
                performAbdomenDialog();
                break;
                
            case Thigh:
                performThighDialog();
                break;
                
            default:
                displayError("Image type not supported");
                break;
               
        }
    } // end runAlgorithm()
    
    
    private void performAbdomenDialog() {
        
    	String[][] mirrorArr = new String[3][];
        mirrorArr[0] = new String[1];
        mirrorArr[0][0] = "fascial area";
        
        mirrorArr[1] = new String[3];
        mirrorArr[1][0] = "psoas";
        mirrorArr[1][1] = "lateral";
        mirrorArr[1][2] = "paraspinous";
        
        mirrorArr[2] = new String[2];
        mirrorArr[2][0] = "aortic calcium";
        mirrorArr[2][1] = "rectus abdominus";
        
        boolean[][] mirrorZ = new boolean[3][];
        mirrorZ[0] = new boolean[1];
        mirrorZ[0][0] = false;
        
        mirrorZ[1] = new boolean[3];
        mirrorZ[1][0] = false;
        mirrorZ[1][1] = false;
        mirrorZ[1][2] = false;
        
        mirrorZ[2] = new boolean[2];
        mirrorZ[2][0] = false;
        mirrorZ[2][1] = true;
        
        String[][] noMirrorArr = new String[3][];
        noMirrorArr[0] = new String[1];
        noMirrorArr[0][0] = "Phantom";
        
        noMirrorArr[1] = new String[1];
        noMirrorArr[1][0] = "Bone sample";
        
        noMirrorArr[2] = new String[1];
        noMirrorArr[2][0] = "Water sample";
        
        boolean[][] noMirrorZ = new boolean[3][];
        noMirrorZ[0] = new boolean[1];
        noMirrorZ[0][0] = false;
        
        noMirrorZ[1] = new boolean[1];
        noMirrorZ[1][0] = false;
        
        noMirrorZ[2] = new boolean[1];
        noMirrorZ[2][0] = false;
        
        String[] titles = new String[3];
        titles[0] = "Abdomen";
        titles[1] = "Samples";
        titles[2] = "Muscles"; 
        
        if (ViewUserInterface.getReference().isAppFrameVisible()) {
        	new PlugInMuscleImageDisplay(srcImage, titles, mirrorArr, mirrorZ, 
        			noMirrorArr, noMirrorZ, 
        			PlugInMuscleImageDisplay.ImageType.Abdomen, 
        			PlugInMuscleImageDisplay.Symmetry.LEFT_RIGHT);
        } else {
        	new PlugInMuscleImageDisplay(srcImage, titles, mirrorArr, mirrorZ, 
        			noMirrorArr, noMirrorZ, 
        			PlugInMuscleImageDisplay.ImageType.Abdomen, 
        			PlugInMuscleImageDisplay.Symmetry.LEFT_RIGHT, true);
        }
        
    }
    
    /**
	 *   Builds thigh dialogue.
	 */
	private void performThighDialog() {
	    
	    String[][] mirrorArr = new String[3][];
	    mirrorArr[0] = new String[1];
	    mirrorArr[0][0] = "Thigh";
	    
	    mirrorArr[1] = new String[2];
	    mirrorArr[1][0] = "Bone";
	    mirrorArr[1][1] = "Marrow";
	    
	    mirrorArr[2] = new String[5];
	    mirrorArr[2][0] = "Fascia";
	    mirrorArr[2][1] = "Quads";
	    mirrorArr[2][2] = "Hamstrings";
	    mirrorArr[2][3] = "Sartorius";
	    mirrorArr[2][4] = "Adductors";
	    
	    boolean[][] mirrorZ = new boolean[3][];
	    mirrorZ[0] = new boolean[1];
	    mirrorZ[0][0] = false;
	    
	    mirrorZ[1] = new boolean[2];
	    mirrorZ[1][0] = false;
	    mirrorZ[1][1] = false;
	    
	    mirrorZ[2] = new boolean[5];
	    mirrorZ[2][0] = false;
	    mirrorZ[2][1] = true;
	    mirrorZ[2][2] = true;
	    mirrorZ[2][3] = true;
	    mirrorZ[2][4] = true;
	    
	    String[][] noMirrorArr = new String[3][];
	    noMirrorArr[0] = new String[1];
	    noMirrorArr[0][0] = "Phantom";
	    
	    noMirrorArr[1] = new String[1];
	    noMirrorArr[1][0] = "Bone sample";
	    
	    noMirrorArr[2] = new String[1];
	    noMirrorArr[2][0] = "Water sample";
	    
	    boolean[][] noMirrorZ = new boolean[3][];
	    noMirrorZ[0] = new boolean[1];
	    noMirrorZ[0][0] = false;
	    
	    noMirrorZ[1] = new boolean[1];
	    noMirrorZ[1][0] = false;
	    
	    noMirrorZ[2] = new boolean[1];
	    noMirrorZ[2][0] = false;
	    
	    String[] titles = new String[3];
	    titles[0] = "Thigh";
	    titles[1] = "Bone";
	    titles[2] = "Muscles";
	    
	    this.symmetry = PlugInMuscleImageDisplay.Symmetry.LEFT_RIGHT;
	    
	    if (ViewUserInterface.getReference().isAppFrameVisible()) {
	    	new PlugInMuscleImageDisplay(srcImage, titles, mirrorArr, mirrorZ, 
	    			noMirrorArr, noMirrorZ, 
	    			PlugInMuscleImageDisplay.ImageType.Thigh, symmetry);
	    } else {
	    	new PlugInMuscleImageDisplay(srcImage, titles, mirrorArr, mirrorZ, 
	    			noMirrorArr, noMirrorZ, 
	    			PlugInMuscleImageDisplay.ImageType.Thigh, symmetry, true);
	    }
	}
	
}

