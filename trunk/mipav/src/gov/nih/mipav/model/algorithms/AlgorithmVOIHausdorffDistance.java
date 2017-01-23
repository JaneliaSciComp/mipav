package gov.nih.mipav.model.algorithms;

import java.util.BitSet;
import gov.nih.mipav.model.structures.ModelImage;
import gov.nih.mipav.model.structures.ModelStorageBase;
import gov.nih.mipav.model.structures.VOI;
import gov.nih.mipav.model.structures.VOIContour;
import gov.nih.mipav.model.structures.VOIVector;
import gov.nih.mipav.view.ViewVOIVector;
import gov.nih.mipav.view.dialogs.JDialogBase;


/**
 * Algorithm class for finding Hausdorff distance on 2 VOIs
 * @author ilb
 *
 */
public class AlgorithmVOIHausdorffDistance extends AlgorithmBase {
	
	/** Vector of all VOIs that will have calculations performed. */
    protected ViewVOIVector selectedVOIset;
    
    /** Model Images **/
    protected ModelImage image;
    
    
    
    private AlgorithmMorphology2D alg2D;
    
    private AlgorithmMorphology3D alg3D;
    
    private int min = 0;
    
    private int max = 1000000000;
    
    private int kernel = 0;
    

     
    /**
     * constructor
     * @param img
     * @param clonedImage
     * @param selectedVOIset
     */
	public AlgorithmVOIHausdorffDistance(ModelImage image , ViewVOIVector selectedVOIset) {
		this.image = image;
		this.selectedVOIset = selectedVOIset;
	}
	
	
	/**
	 * run algorithm
	 */
	public void runAlgorithm() {
		
    System.out.println("I run Hausdorff Distance");

	}


	

}
