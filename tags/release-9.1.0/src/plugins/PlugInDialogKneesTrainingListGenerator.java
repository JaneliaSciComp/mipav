import gov.nih.mipav.model.algorithms.*;

import gov.nih.mipav.view.*;

import java.awt.*;


/**
 * Stub method to invoke the processing pipeline. 
 * 
 * @author Ruida Cheng
 *
 */
public class PlugInDialogKneesTrainingListGenerator implements AlgorithmInterface
{
    /** The main user interface. */
    private ViewUserInterface UI;

   
    /** image repository directory */ 
    private String inputDirImage;
    
   
    /** Algorithm to run training list generator. */
    private PlugInAlgorithmKneesTrainingListGenerator listAlgo;
    
    /**
     * Constructor of Training list generator.  Called from the MIPAV menu. 
     * @param theParentFrame  parent frame.
     */
    public PlugInDialogKneesTrainingListGenerator(Frame theParentFrame) {
        UI = ViewUserInterface.getReference();
    }

    /**
     * Constructor ofTraining list generator.  Called from Plugin dialog. 
     * @param theParentFrame
     * @param _inputDir    10-fold cross-validataion training fold.
     */
    public PlugInDialogKneesTrainingListGenerator(Frame theParentFrame, String _inputDirImage) {
    	inputDirImage = _inputDirImage;
    	UI = ViewUserInterface.getReference();
    
        callAlgorithm();
         
    }
    
   
	public void callAlgorithm() {

		listAlgo = new PlugInAlgorithmKneesTrainingListGenerator(inputDirImage, this);
		listAlgo.run();
	}
    
	public void algorithmPerformed(AlgorithmBase algorithm) {
		
	}

   
}

