package gov.nih.mipav.view.dialogs;


import gov.nih.mipav.model.algorithms.*;
import gov.nih.mipav.model.scripting.*;
import gov.nih.mipav.model.scripting.parameters.ParameterFactory;
import gov.nih.mipav.model.structures.*;

import gov.nih.mipav.view.*;

import java.awt.*;
import java.awt.event.*;

import javax.swing.*;


/**
 * Dialog to get user input, then call the algorithm.
 */
public class JDialogHaarTransform extends JDialogScriptableBase implements AlgorithmInterface{

    //~ Static fields/initializers -------------------------------------------------------------------------------------

    /** Use serialVersionUID for interoperability. */
    //private static final long serialVersionUID;
		
	private ModelImage srcImage;
	
    private ModelImage transformImage;
    
    private ModelImage inverseImage;
    
    private HaarTransform htAlgo;
	
	/**
     * Empty constructor needed for dynamic instantiation.
     */
    public JDialogHaarTransform() { }

    /**
     * Construct the Haar Transform dialog.
     *
     * @param  theParentFrame  Parent frame.
     * @param  im              Source image.
     */
    public JDialogHaarTransform(Frame theParentFrame, ModelImage im) {
        super(theParentFrame, false);
        srcImage = im;
        init();
        setVisible(true);
    }

    //~ Methods --------------------------------------------------------------------------------------------------------
    
    /**
     * Closes dialog box when the OK button is pressed and calls the algorithm.
     *
     * @param  event  Event that triggers function.
     */
    public void actionPerformed(ActionEvent event) {
        String command = event.getActionCommand();

        if (command.equals("OK")) {

            if (setVariables()) {
                callAlgorithm();
            }
        } else if (command.equals("Cancel")) {
            dispose();
        } else if (command.equals("Help")) {
            //MipavUtil.showHelp("");
        } else {
            super.actionPerformed(event);
        }
    }
    
    /**
     * Sets up the GUI (panels, buttons, etc) and displays it on the screen.
     */
    private void init() {
        setForeground(Color.black);

        setTitle("Haar Transform");
        getContentPane().setLayout(new BorderLayout());
        JPanel paramsPanel = new JPanel(new GridBagLayout());
        paramsPanel.setBorder(buildTitledBorder("Transform parameters"));
        
        GridBagConstraints gbc = new GridBagConstraints();
        gbc.gridwidth = 1;
        gbc.anchor = GridBagConstraints.WEST;
        gbc.weightx = 1;
        
        getContentPane().add(paramsPanel, BorderLayout.CENTER);
        getContentPane().add(buildButtons(), BorderLayout.SOUTH);
        pack();
        setResizable(true);

        System.gc();
    }
    
    private boolean setVariables() {
    	
    	return true;
    }
	
	protected void callAlgorithm() {
		try {
			
		    transformImage = new ModelImage(ModelStorageBase.DOUBLE, srcImage.getExtents(), srcImage.getImageName() + "_transform");
			
		    inverseImage = new ModelImage(ModelStorageBase.DOUBLE, srcImage.getExtents(), srcImage.getImageName() + "_inverse");	
		    
		    
		    htAlgo = new HaarTransform(transformImage, inverseImage, srcImage);
		    
		    // This is very important. Adding this object as a listener allows the algorithm to
		    // notify this object when it has completed of failed. See algorithm performed event.
		    // This is made possible by implementing AlgorithmedPerformed interface
		    htAlgo.addListener(this);
		
		
		    createProgressBar(srcImage.getImageName(), htAlgo);
		
		    // Hide dialog
		    setVisible(false);
		
		    if (isRunInSeparateThread()) {
		
		        // Start the thread as a low priority because we wish to still have user interface work fast.
		        if (htAlgo.startMethod(Thread.MIN_PRIORITY) == false) {
		            MipavUtil.displayError("A thread is already running on this object");
		        }
		    } else {
		
		        htAlgo.run();
		    }
		   
	    } catch (OutOfMemoryError x) {
	    	
	    	if (transformImage != null) {
	    		transformImage.disposeLocal();
	    		transformImage = null;
	    	}
	    	
	        if (inverseImage != null) {
	    		inverseImage.disposeLocal();
	    		inverseImage = null;
	    	}
	
	        System.gc();
	        MipavUtil.displayError("Dialog Haar Transform: unable to allocate enough memory");
	        return;
	    }

       
    }
    
 // ************************************************************************
    // ************************** Algorithm Events ****************************
    // ************************************************************************

    /**
     * This method is required if the AlgorithmPerformed interface is implemented. It is called by the algorithms when
     * it has completed or failed to to complete, so that the dialog can be display the result image and/or clean up.
     *
     * @param  algorithm  Algorithm that caused the event.
     */
    public void algorithmPerformed(AlgorithmBase algorithm) {

        if (algorithm instanceof HaarTransform) {
        	
        	if (htAlgo.isCompleted()) {
        		if (transformImage != null) {
        			updateFileInfo(srcImage, transformImage);
        			
        			try {
                        new ViewJFrameImage(transformImage, null, new Dimension(610, 200));
                    } catch (OutOfMemoryError error) {
                        System.gc();
                        JOptionPane.showMessageDialog(null, "Out of memory: unable to open new transformImage frame",
                                                      "Error", JOptionPane.ERROR_MESSAGE);
                    }
        		} // if (transformImage != null)
        		
        		if (inverseImage != null) {
        			updateFileInfo(srcImage, inverseImage);
        			
        			try {
                        new ViewJFrameImage(inverseImage, null, new Dimension(610, 220));
                    } catch (OutOfMemoryError error) {
                        System.gc();
                        JOptionPane.showMessageDialog(null, "Out of memory: unable to open new inverseImage frame",
                                                      "Error", JOptionPane.ERROR_MESSAGE);
                    }
        		} // if (inverseImage != null)
        	} // if (htAlgo.isCompleted())
        	else {
        		if (transformImage != null) {
    	    		transformImage.disposeLocal();
    	    		transformImage = null;
    	    	}
    	    	
    	    	if (inverseImage != null) {
    	    		inverseImage.disposeLocal();
    	    		inverseImage = null;
    	    	}	

                System.gc();
        	}
        }
        

        if (algorithm.isCompleted()) {
            insertScriptLine();
        }

        dispose();
    }
	
	protected void setGUIFromParams() {
		srcImage = scriptParameters.retrieveInputImage();
	}
	
	protected void storeParamsFromGUI() throws ParserException {
		scriptParameters.storeInputImage(srcImage);	
		scriptParameters.storeImageInRecorder(transformImage);
		scriptParameters.storeImageInRecorder(inverseImage);
	}
}
