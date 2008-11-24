import java.awt.Dimension;
import java.awt.event.ActionEvent;

import gov.nih.mipav.model.algorithms.AlgorithmBase;
import gov.nih.mipav.model.algorithms.AlgorithmInterface;
import gov.nih.mipav.model.structures.ModelImage;
import gov.nih.mipav.view.MipavUtil;
import gov.nih.mipav.view.dialogs.JDialogScriptableBase;
import javax.swing.*;
import java.io.File;

/**
 * @author joshim2
 *
 */
public class PlugInDialogAnonymizeDICOM extends JDialogScriptableBase implements AlgorithmInterface {

	// ~ Instance fields ------------------------------------------------------------------------
	
	/** File chooser object */
	private JFileChooser fileChooser;
	
	/** File selected by the user */
	private File selectedFile;
	
	/** Algorithm instance */
    private PlugInGenericAnonymizeDICOM algoAnonymizeDicom;
	
	
	
	//	~ Constructors --------------------------------------------------------------------------

    /**
     * Empty constructor needed for dynamic instantiation (used during scripting).
     */
    public PlugInDialogAnonymizeDICOM() { }
    
    /**
     * Sets up variables but does not show dialog.
     *
     * @param  theParentFrame  Parent frame.
     * @param  im              Source image.
     */
    public PlugInDialogAnonymizeDICOM(boolean openDialog) {
        
    	if(openDialog){
    	
    		fileChooser = new JFileChooser("null");
        	fileChooser.setFont(MipavUtil.defaultMenuFont);
        	fileChooser.setMultiSelectionEnabled(false);
        	
        	Dimension d = new Dimension(700, 400);
            fileChooser.setMinimumSize(d);
            fileChooser.setPreferredSize(d);
            
            int returnVal = fileChooser.showOpenDialog(null);
            
            if (returnVal == JFileChooser.APPROVE_OPTION) {
            	selectedFile = fileChooser.getSelectedFile();
            	callAlgorithm();
            }
            
            
            
    	}
    	
    	
        
    }
    
    // ~ Methods ----------------------------------------------------------------------------------
    
    /**
     * Once all the necessary variables are set, call the DICOM anonymizer algorithm.
     */
    
    protected void callAlgorithm() {
    
    	try{
    		System.gc();
    		
    		//Make algorithm.
    		algoAnonymizeDicom = new PlugInGenericAnonymizeDICOM(selectedFile);
    		
    		// This is very important. Adding this object as a listener allows the algorithm to
            // notify this object when it has completed of failed. See algorithm performed event.
            // This is made possible by implementing AlgorithmedPerformed interface
            algoAnonymizeDicom.addListener(this);
            
            createProgressBar(selectedFile.getName(), algoAnonymizeDicom);
            
            if (isRunInSeparateThread()) {

                // Start the thread as a low priority because we wish to still have user interface work fast.
                if (algoAnonymizeDicom.startMethod(Thread.MIN_PRIORITY) == false) {
                    MipavUtil.displayError("A thread is already running on this object");
                }
            } else {
                algoAnonymizeDicom.run();
            }
    		
    	} catch (OutOfMemoryError x) {
            System.gc();
            MipavUtil.displayError("Dialog Plugin Anonymize DICOM: unable to allocate enough memory");

            return;
    	}
    
    }
    
    /**
     * Closes dialog box when the OK button is pressed and calls the algorithm.
     *
     * @param  event  Event that triggers function.
     */
    public void actionPerformed(ActionEvent event) {
    	
    	
    }
    
    /**
     * This method is required if the AlgorithmPerformed interface is implemented. It is called by the algorithms when
     * it has completed or failed to complete, so that the dialog can display the result image and/or clean up.
     *
     * @param  algorithm  Algorithm that caused the event.
     */
    public void algorithmPerformed(AlgorithmBase algorithm) {
    	
    	algoAnonymizeDicom.finalize();
    	algoAnonymizeDicom = null;
    	
    }
    
    protected void setGUIFromParams() {
    	
    }
    protected void storeParamsFromGUI() {
    	
    }
    
}
