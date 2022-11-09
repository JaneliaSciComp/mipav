import gov.nih.mipav.model.algorithms.*;
import gov.nih.mipav.view.dialogs.*;

import gov.nih.mipav.view.*;

import java.awt.*;
import java.awt.event.*;
import java.io.*;

import java.util.*;

import javax.swing.*;
import javax.swing.border.EtchedBorder;
import javax.swing.border.TitledBorder;


/**
 * Stub method to invoke the processing pipeline. 
 * 
 * @author Ruida Cheng
 *
 */
public class PlugInDialogKneesFemurSegmentationSKI10 implements AlgorithmInterface
{
    
    /** The main user interface. */
    private ViewUserInterface UI;

    /** image repository directory */ 
    private String inputDir;
    
    /** saved image and report directory. */
    private String outputDir;
    
    /** Algorithm to run brain subcotrical registration. */
    private PlugInAlgorithmKneesFemurSegmentationSKI10 subCorticalAlgo;
    
    /**
     * Constructor of Brain Subcortical Dialog.  Called from the MIPAV menu. 
     * @param theParentFrame  parent frame.
     */
    public PlugInDialogKneesFemurSegmentationSKI10(Frame theParentFrame) {
        UI = ViewUserInterface.getReference();
        //init();
    }

    /**
     * Constructor of Brain subcortical dialog.  Called from Plugin dialog. 
     * @param theParentFrame
     * @param _inputDir    brain MRI image repository input directory.
     * @param _outputDir   output result directory. 
     * @param _regSection  registered sections in vector. 
     */
    public PlugInDialogKneesFemurSegmentationSKI10(Frame theParentFrame, String _inputDir, String _outputDir) {
    	inputDir = _inputDir;
    	outputDir = _outputDir;
    	UI = ViewUserInterface.getReference();
        callAlgorithm();
         
    }
    
   
    /**
     * The first instance is used as the reference.   We do comparison by using the reference image as the base, 
     * compare it with rest images.  
     */
	public void callAlgorithm() {

		subCorticalAlgo = new PlugInAlgorithmKneesFemurSegmentationSKI10(inputDir, outputDir, this);
	    subCorticalAlgo.run();
	}
    
	public void algorithmPerformed(AlgorithmBase algorithm) {
		
	}

    
}

