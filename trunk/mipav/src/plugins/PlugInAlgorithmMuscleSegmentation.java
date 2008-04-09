
import gov.nih.mipav.model.algorithms.AlgorithmBase;
import gov.nih.mipav.model.structures.*;
import gov.nih.mipav.view.*;

import java.awt.*;
import java.util.TreeMap;

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
    
    /**Whether multiple slices are contained in srcImg */
    private boolean multipleSlices;
       
	
    /**
     * Constructor.
     *
     * @param  resultImage  Result image model
     * @param  srcImg       Source image model.
     */
    public PlugInAlgorithmMuscleSegmentation(ModelImage srcImg, PlugInMuscleImageDisplay.ImageType imageType, 
    											Frame parentFrame, boolean multipleSlices) {
        super(null, srcImg);
        this.imageType = imageType;
        this.parentFrame = parentFrame;
        this.multipleSlices = multipleSlices;
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
        
    	TreeMap calcTree = new TreeMap();
    	
    	String[][] mirrorArr = new String[3][];
        mirrorArr[0] = new String[0];
        
        mirrorArr[1] = new String[0];
        
        mirrorArr[2] = new String[4];
        mirrorArr[2][0] = "psoas";
        mirrorArr[2][1] = "lat. obliques";
        mirrorArr[2][2] = "paraspinous";
        mirrorArr[2][3] = "rectus";

        for(int i=0; i<mirrorArr.length; i++)  {
        	if(mirrorArr[i] != null) {
        		for(int j=0; j<mirrorArr[i].length; j++)
        			calcTree.put(mirrorArr[i][j], true);
        	}
        }
        
        boolean[][] mirrorZ = new boolean[3][];
        mirrorZ[0] = new boolean[0];
        
        mirrorZ[1] = new boolean[0];
        
        mirrorZ[2] = new boolean[4];
        mirrorZ[2][0] = true;
        mirrorZ[2][1] = true;
        mirrorZ[2][2] = true;
        mirrorZ[2][3] = true;
        
        String[][] noMirrorArr = new String[3][];
        noMirrorArr[0] = new String[2];
        noMirrorArr[0][0] = "Abdomen";
        noMirrorArr[0][1] = "Phantom";
        
        noMirrorArr[1] = new String[3];
        noMirrorArr[1][0] = "Visceral cavity";
        noMirrorArr[1][1] = "Bone sample";
        noMirrorArr[1][2] = "Water sample";
        
        noMirrorArr[2] = new String[1];
        noMirrorArr[2][0] = "Aorta";
        
        for(int i=0; i<noMirrorArr.length; i++) {
        	for(int j=0; j<noMirrorArr[i].length; j++) {
        		if(noMirrorArr[i][j].equals("Phantom") || noMirrorArr[i][j].contains("sample"))
        			calcTree.put(noMirrorArr[i][j], false);
        		else
        			calcTree.put(noMirrorArr[i][j], true);
        	}
        }
        
        boolean[][] noMirrorZ = new boolean[3][];
        noMirrorZ[0] = new boolean[2];
        noMirrorZ[0][0] = false;
        noMirrorZ[0][1] = false;
        
        noMirrorZ[1] = new boolean[3];
        noMirrorZ[1][0] = false;
        noMirrorZ[1][1] = false;
        noMirrorZ[1][2] = false;
        
        noMirrorZ[2] = new boolean[1];
        noMirrorZ[2][0] = true;
        
        String[] titles = new String[3];
        titles[0] = "Abdomen";
        titles[1] = "Tissue";
        titles[2] = "Muscles"; 
        
        if (ViewUserInterface.getReference().isAppFrameVisible()) {
        	new PlugInMuscleImageDisplay(srcImage, titles, mirrorArr, mirrorZ, 
        			noMirrorArr, noMirrorZ, calcTree, 
        			PlugInMuscleImageDisplay.ImageType.Abdomen, 
        			PlugInMuscleImageDisplay.Symmetry.LEFT_RIGHT, multipleSlices);
        } else {
        	new PlugInMuscleImageDisplay(srcImage, titles, mirrorArr, mirrorZ, 
        			noMirrorArr, noMirrorZ, calcTree, 
        			PlugInMuscleImageDisplay.ImageType.Abdomen, 
        			PlugInMuscleImageDisplay.Symmetry.LEFT_RIGHT, true, multipleSlices);
        }
        
    }
    
    /**
	 *   Builds thigh dialogue.
	 */
	private void performThighDialog() {
	    
	    TreeMap calcTree = new TreeMap();
	    
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
	    
	    for(int i=0; i<mirrorArr.length; i++) 
	    	for(int j=0; j<mirrorArr[i].length; j++)
	    		calcTree.put(mirrorArr[i][j], true); //true
	    //calcTree.put("Sartorius", false);
	    
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
	    
	    for(int i=0; i<noMirrorArr.length; i++) 
	    	for(int j=0; j<noMirrorArr[i].length; j++)
	    		calcTree.put(noMirrorArr[i][j], false);  //false
	    //calcTree.put("Phantom", true);
	    
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
	    			noMirrorArr, noMirrorZ, calcTree, 
	    			PlugInMuscleImageDisplay.ImageType.Thigh, symmetry, multipleSlices);
	    } else {
	    	new PlugInMuscleImageDisplay(srcImage, titles, mirrorArr, mirrorZ, 
	    			noMirrorArr, noMirrorZ, calcTree, 
	    			PlugInMuscleImageDisplay.ImageType.Thigh, symmetry, true, multipleSlices);
	    }
	}
	
}

