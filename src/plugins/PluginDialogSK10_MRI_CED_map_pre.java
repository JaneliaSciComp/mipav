import gov.nih.mipav.model.algorithms.*;

import gov.nih.mipav.view.*;

import java.awt.*;
import java.awt.event.*;


/**
 * Stub method to invoke the processing pipeline. 
 * 
 * @author Ruida Cheng
 *
 */
public class PluginDialogSK10_MRI_CED_map_pre implements AlgorithmInterface
{
    
    /** The main user interface. */
    private ViewUserInterface UI;

   
    /** image repository directory */ 
    private String inputDirImage;
    
    /** map repository directory */ 
    private String inputDirMap;
    
    
    /** saved image and report directory. */
    private String outputDir;
    
    /** Algorithm to run brain subcotrical registration. */
    private PlugInAlgorithmSK10_MRI_CED_map_pre mappingAlgo;
    
    /**
     * Constructor of Brain Subcortical Dialog.  Called from the MIPAV menu. 
     * @param theParentFrame  parent frame.
     */
    public PluginDialogSK10_MRI_CED_map_pre(Frame theParentFrame) {
        UI = ViewUserInterface.getReference();
    }

    /**
     * Constructor of Brain subcortical dialog.  Called from Plugin dialog. 
     * @param theParentFrame
     * @param _inputDir    brain MRI image repository input directory.
     * @param _outputDir   output result directory. 
     * @param _regSection  registered sections in vector. 
     */
    public PluginDialogSK10_MRI_CED_map_pre(Frame theParentFrame, String _inputDirImage, String _inputDirMap, String _outputDir) {
    	inputDirImage = _inputDirImage;
    	inputDirMap = _inputDirMap;
    	outputDir = _outputDir;
    	UI = ViewUserInterface.getReference();
    
        callAlgorithm();
         
    }
      
   
    /**
     * The first instance is used as the reference.   We do comparison by using the reference image as the base, 
     * compare it with rest images.  
     */
	public void callAlgorithm() {

		mappingAlgo = new PlugInAlgorithmSK10_MRI_CED_map_pre(inputDirImage, inputDirMap, outputDir, this);
	    mappingAlgo.run();
	}
    
	public void algorithmPerformed(AlgorithmBase algorithm) {
		
	}
   
   
}

