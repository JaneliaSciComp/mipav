
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
       
    /**voiList created by various set up methods*/
    private PlugInSelectableVOI[][] voiList;
    
    /**list of titles used for each pane*/
    private String[] titles;
	
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
                buildAbdomenDialog();
                break;
                
            case Thigh:
                buildThighDialog();
                break;
                
            case Custom:
            	buildCustomDialog();
            	break;
                
            default:
                displayError("Image type not supported");
                return;         
        }
        performDialog();
    } // end runAlgorithm()
    
    
    private void buildAbdomenDialog() {
        
    	voiList = new PlugInSelectableVOI[3][];
    	int imageSize = 1;
    	if(srcImage.getNDims() > 2)
    		imageSize = srcImage.getExtents()[2];
    	//String name, boolean closed, int numCurves, int location, boolean fillable, doCalc
    	voiList[0] = new PlugInSelectableVOI[3];
    	voiList[0][0] = new PlugInSelectableVOI("Abdomen", true, 1, 0, false, true, imageSize, 0);
    	voiList[0][1] = new PlugInSelectableVOI("Subcutaneous area", true, 1, 0, false, true, imageSize, 1);
    	voiList[0][2] = new PlugInSelectableVOI("Phantom", true, 1, 0, false, false, imageSize);
    	
    	voiList[1] = new PlugInSelectableVOI[5];
    	voiList[1][0] = new PlugInSelectableVOI("Visceral cavity", true, 1, 1, false, true, imageSize, 2);
    	voiList[1][1] = new PlugInSelectableVOI("Liver", true, 1, 1, false, true, imageSize, 3);
    	voiList[1][2] = new PlugInSelectableVOI("Liver cysts", true, 10, 1, true, true, imageSize, 4);
    	voiList[1][3] = new PlugInSelectableVOI("Bone sample", true, 1, 1, false, false, imageSize);
    	voiList[1][4] = new PlugInSelectableVOI("Water sample", true, 1, 1, false, false, imageSize);
    	
    	voiList[2] = new PlugInSelectableVOI[9];
    	voiList[2][0] = new PlugInSelectableVOI("Left Psoas", true, 1, 2, true, true, imageSize, 5);
    	voiList[2][1] = new PlugInSelectableVOI("Right Psoas", true, 1, 2, true, true, imageSize, 6);
    	voiList[2][2] = new PlugInSelectableVOI("Left Lat. obliques", true, 1, 2, true, true, imageSize, 7);
    	voiList[2][3] = new PlugInSelectableVOI("Right Lat. obliques", true, 1, 2, true, true, imageSize, 8);
    	voiList[2][4] = new PlugInSelectableVOI("Left Paraspinous", true, 1, 2, true, true, imageSize, 9);
    	voiList[2][5] = new PlugInSelectableVOI("Right Paraspinous", true, 1, 2, true, true, imageSize, 10);
    	voiList[2][6] = new PlugInSelectableVOI("Left Rectus", true, 1, 2, true, true, imageSize, 11);
    	voiList[2][7] = new PlugInSelectableVOI("Right Rectus", true, 1, 2, true, true, imageSize, 12);
    	voiList[2][8] = new PlugInSelectableVOI("Aortic Calcium", true, 5, 2, true, true, imageSize, 13);
        
        titles = new String[3];
        titles[0] = "Abdomen";
        titles[1] = "Tissue";
        titles[2] = "Muscles"; 
        
        symmetry = PlugInMuscleImageDisplay.Symmetry.LEFT_RIGHT;
	    imageType = PlugInMuscleImageDisplay.ImageType.Abdomen;
    }
    
    /**
	 *   Builds thigh dialogue.
	 */
	private void buildThighDialog() {
	    
	    //String name, boolean closed, int numCurves, int location, boolean fillable, doCalc
	    int imageSize = 1;
    	if(srcImage.getNDims() > 2)
    		imageSize = srcImage.getExtents()[2];
	    
	    voiList = new PlugInSelectableVOI[3][];
	    
	    voiList[0] = new PlugInSelectableVOI[3];
	    voiList[0][0] = new PlugInSelectableVOI("Left Thigh", true, 1, 0, false, true, imageSize, 0);
    	voiList[0][1] = new PlugInSelectableVOI("Right Thigh", true, 1, 0, false, true, imageSize, 1);
    	
    	voiList[0][2] = new PlugInSelectableVOI("Phantom", true, 1, 0, false, false, imageSize);
	    
    	voiList[1] = new PlugInSelectableVOI[5];
	    voiList[1][0] = new PlugInSelectableVOI("Left Bone", true, 1, 1, false, true, imageSize, 2);
    	voiList[1][1] = new PlugInSelectableVOI("Right Bone", true, 1, 1, false, true, imageSize, 3);
	    voiList[1][2] = new PlugInSelectableVOI("Left Marrow", true, 1, 1, true, true, imageSize, 4);
    	voiList[1][3] = new PlugInSelectableVOI("Right Marrow", true, 1, 1, true, true, imageSize, 5);
    	
    	voiList[1][4] = new PlugInSelectableVOI("Bone sample", true, 1, 1, false, false, imageSize);
	    
    	voiList[2] = new PlugInSelectableVOI[11];
	    voiList[2][0] = new PlugInSelectableVOI("Left Fascia", true, 1, 2, false, true, imageSize, 6);
    	voiList[2][1] = new PlugInSelectableVOI("Right Fascia", true, 1, 2, false, true, imageSize, 7);
	    voiList[2][2] = new PlugInSelectableVOI("Left Quads", true, 1, 2, true, true, imageSize, 8);
    	voiList[2][3] = new PlugInSelectableVOI("Right Quads", true, 1, 2, true, true, imageSize, 9);
	    voiList[2][4] = new PlugInSelectableVOI("Left Hamstrings", true, 1, 2, true, true, imageSize, 10);
    	voiList[2][5] = new PlugInSelectableVOI("Right Hamstrings", true, 1, 2, true, true, imageSize, 11);
	    voiList[2][6] = new PlugInSelectableVOI("Left Sartorius", true, 1, 2, true, true, imageSize, 12);
    	voiList[2][7] = new PlugInSelectableVOI("Right Sartorius", true, 1, 2, true, true, imageSize, 13);
	    voiList[2][8] = new PlugInSelectableVOI("Left Adductors", true, 1, 2, true, true, imageSize, 14);
    	voiList[2][9] = new PlugInSelectableVOI("Right Adductors", true, 1, 2, true, true, imageSize, 15);

    	voiList[2][10] = new PlugInSelectableVOI("Water sample", true, 1, 1, false, false, imageSize);
	    
	    titles = new String[3];
	    titles[0] = "Thigh";
	    titles[1] = "Bone";
	    titles[2] = "Muscles";
	     
	    symmetry = PlugInMuscleImageDisplay.Symmetry.LEFT_RIGHT;
	    imageType = PlugInMuscleImageDisplay.ImageType.Thigh;
	}
	
	private void buildCustomDialog() {
		MipavUtil.displayError("Custom dialog not yet built. Plugin will now end.");
	}
	
	private void performDialog() {
		if (ViewUserInterface.getReference().isAppFrameVisible()) {
        	new PlugInMuscleImageDisplay(srcImage, titles, voiList,  
        			imageType, symmetry, multipleSlices);
        } else {
        	new PlugInMuscleImageDisplay(srcImage, titles, voiList, 
        			imageType, symmetry, true, multipleSlices);
        }
	}
	
}

