package gov.nih.mipav.view.dialogs;


import gov.nih.mipav.model.algorithms.*;
import gov.nih.mipav.model.scripting.*;
import gov.nih.mipav.model.scripting.parameters.*;
import gov.nih.mipav.model.structures.*;
import gov.nih.mipav.view.*;

import java.awt.*;
import java.awt.event.*;

import javax.swing.*;


/**
 * DOCUMENT ME!
 */
public class JDialogGlobalPb extends JDialogScriptableBase implements AlgorithmInterface, ScriptableActionInterface {

    //~ Static fields/initializers -------------------------------------------------------------------------------------

    /** Use serialVersionUID for interoperability. */
    //private static final long serialVersionUID;

    
    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** DOCUMENT ME! */
    private ModelImage image; // source image

    /** DOCUMENT ME! */
    private ModelImage gPbOrientImage = null;
    
    private ModelImage gPbThinImage = null;
	
	private ModelImage textonImage = null;
	
	// If not null or empty, automatic file saves occur with an outFile String base.
	private String outFile = null;
	
	// Resizing factor in (0, 1], to speed up eigenvector
	private double rsz = 1.0;
    
    private JTextField textOutFile;

    /** DOCUMENT ME! */
    private JTextField textResizing;

    /** DOCUMENT ME! */
    private AlgorithmGlobalPb globalPbAlgo;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Empty constructor needed for dynamic instantiation (used during scripting).
     */
    public JDialogGlobalPb() { }

    /**
     * Creates new dialog for entering parameters for globalPb edge detection.
     *
     * @param  theParentFrame  Parent frame
     * @param  im              Source image
     */
    public JDialogGlobalPb(Frame theParentFrame, ModelImage im) {
        super(theParentFrame, false);
        image = im;
        init();
    }

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * actionPerformed - Closes dialog box when the OK button is pressed and calls the algorithm.
     *
     * @param  event  event that triggers function
     */
    public void actionPerformed(ActionEvent event) {
        String command = event.getActionCommand();

        if (command.equals("OK")) {

            if (setVariables()) {
                callAlgorithm();
            }
        } else if (command.equals("Help")) {
            //MipavUtil.showHelp("");
        } else if (command.equals("Cancel")) {
            dispose();
        } else {
            super.actionPerformed(event);
        }
    }

    // ************************************************************************
    // ************************** Algorithm Events ****************************
    // ************************************************************************
    
    /**
     * This method is required if the AlgorithmPerformed interface is implemented. It is called by the algorithm when it
     * has completed or failed to to complete, so that the dialog can be display the result image and/or clean up.
     *
     * @param  algorithm  Algorithm that caused the event.
     */
    public void algorithmPerformed(AlgorithmBase algorithm) {

        if (algorithm instanceof AlgorithmGlobalPb) {
            System.err.println("Global Pb Edge Detection Elapsed: " + algorithm.getElapsedTime());
            image.clearMask();

            if (globalPbAlgo.isCompleted()) {
                // The algorithm has completed and produced a new image to be displayed.
            	if (gPbOrientImage != null) {

	                updateFileInfo(image, gPbOrientImage);
	                gPbOrientImage.clearMask();
	
	                try {
	
	                    new ViewJFrameImage(gPbOrientImage, null, new Dimension(610, 200));
	                } catch (OutOfMemoryError error) {
	                    System.gc();
	                    MipavUtil.displayError("Out of memory: unable to open new gPbOrientImage frame");
	                }
            	}
            	
            	if (gPbThinImage != null) {

	                updateFileInfo(image, gPbThinImage);
	                gPbThinImage.clearMask();
	
	                try {
	
	                    new ViewJFrameImage(gPbThinImage, null, new Dimension(610, 220));
	                } catch (OutOfMemoryError error) {
	                    System.gc();
	                    MipavUtil.displayError("Out of memory: unable to open new gPbThinImage frame");
	                }
            	}
            	
            	if (textonImage != null) {

	                updateFileInfo(image, textonImage);
	                textonImage.clearMask();
	
	                try {
	
	                    new ViewJFrameImage(textonImage, null, new Dimension(610, 240));
	                } catch (OutOfMemoryError error) {
	                    System.gc();
	                    MipavUtil.displayError("Out of memory: unable to open new textonImage frame");
	                }
            	}
            } else {
            	// algorithm failed but result image still has garbage
            	if (gPbOrientImage != null) {
	                gPbOrientImage.disposeLocal(); // clean up memory
	                gPbOrientImage = null;
            	}
            	
            	if (gPbThinImage != null) {
            		gPbThinImage.disposeLocal(); // clean up memory
	                gPbThinImage = null;	
            	}
            	
            	if (textonImage != null) {
            		textonImage.disposeLocal();
            		textonImage = null;
            	}
            	System.gc();
            }
        }

        if (algorithm.isCompleted()) {
            insertScriptLine();
        }
        // save the completion status for later
        setComplete(algorithm.isCompleted());

        globalPbAlgo.finalize();
        globalPbAlgo = null;
        dispose();
    }

    // *******************************************************************
    // ************************* Item Events ****************************
    // *******************************************************************

    /**
     * Method to handle item events.
     *
     * @param  event  event that cause the method to fire
     */
    public void itemStateChanged(ItemEvent event) {
        // Object source = event.getSource();
        // float tempNum;

    }

    
    /**
     * 
     * @param outFile
     */
    public void setOutFile(String outFile) {
        this.outFile = outFile;
    }

    /**
     * Accessor that sets resizing factor.
     *
     * @param  rsz  DOCUMENT ME!
     */
    public void setRsz(double rsz) {
        this.rsz = rsz;
    }
    
    /**
     * Once all the necessary variables are set, call the rule based contrast enhancement algorithm based on what type of image this is and
     * whether or not there is a separate destination image.
     */
    protected void callAlgorithm() {
    	String gPbOrientName;
    	if ((outFile != null) && (outFile.length() > 0)) {
    	    gPbOrientName = makeImageName(outFile, "_gPb_orient");	
    	}
    	else {
    		gPbOrientName = makeImageName(image.getImageName(), "_gPb_orient");
    	}
        
    	String gPbThinName;
    	if ((outFile != null) && (outFile.length() > 0)) {
    	    gPbThinName = makeImageName(outFile, "_gPb_thin");	
    	}
    	else {
    		gPbThinName = makeImageName(image.getImageName(), "_gPb_thin");
    	}
    	
    	String textonName;
    	if ((outFile != null) && (outFile.length() > 0)) {
    	    textonName = makeImageName(outFile, "_texton");	
    	}
    	else {
    		textonName = makeImageName(image.getImageName(), "_texton");
    	}

        try {
        	
        	int extents[] = new int[3];
            extents[0] = image.getExtents()[0];
            extents[1] = image.getExtents()[1];
            extents[2] = 8;
        	
            gPbOrientImage = new ModelImage(ModelStorageBase.DOUBLE, extents, gPbOrientName);
            gPbThinImage = new ModelImage(ModelStorageBase.DOUBLE, image.getExtents(), gPbThinName);
            textonImage = new ModelImage(ModelStorageBase.INTEGER, image.getExtents(), textonName);

            // Make algorithm
            globalPbAlgo = new AlgorithmGlobalPb(gPbOrientImage, gPbThinImage, textonImage,
            		                                       image, outFile, rsz);

            // This is very important. Adding this object as a listener allows the algorithm to
            // notify this object when it has completed or failed. See algorithm performed event.
            // This is made possible by implementing AlgorithmedPerformed interface
            globalPbAlgo.addListener(this);

            createProgressBar(image.getImageName(), globalPbAlgo);

            setVisible(false); // Hide dialog

            if (isRunInSeparateThread()) {

                // Start the thread as a low priority because we wish to still have user interface work fast.
                if (globalPbAlgo.startMethod(Thread.MIN_PRIORITY) == false) {
                    MipavUtil.displayError("A thread is already running on this object");
                }
            } else {

                globalPbAlgo.run();
            }
        } catch (OutOfMemoryError x) {
            MipavUtil.displayError("Dialog Global Pb Edge Detection: unable to allocate enough memory");

            if (gPbOrientImage != null) {
                gPbOrientImage.disposeLocal(); // Clean up memory of result image
                gPbOrientImage = null;
            }
            
            if (gPbThinImage != null) {
                gPbThinImage.disposeLocal(); // Clean up memory of result image
                gPbThinImage = null;
            }
            
            if (textonImage != null) {
                textonImage.disposeLocal(); // Clean up memory of result image
                textonImage = null;
            }

            return;
        }
           
    }

    

    /**
     * {@inheritDoc}
     */
    protected void doPostAlgorithmActions() {

        AlgorithmParameters.storeImageInRunner(gPbOrientImage);
        AlgorithmParameters.storeImageInRunner(gPbThinImage);
        AlgorithmParameters.storeImageInRunner(textonImage);
    }

    /**
     * {@inheritDoc}
     */
    protected void setGUIFromParams() {
        image = scriptParameters.retrieveInputImage();
        outFile = scriptParameters.getParams().getString("out_file");
        rsz = scriptParameters.getParams().getDouble("resizing");
    }

    /**
     * {@inheritDoc}
     */
    protected void storeParamsFromGUI() throws ParserException {
        scriptParameters.storeInputImage(image);
        scriptParameters.storeOutputImageParams(gPbOrientImage, true);
        scriptParameters.storeOutputImageParams(gPbThinImage, true);
        scriptParameters.storeOutputImageParams(textonImage, true);
        scriptParameters.getParams().put(ParameterFactory.newParameter("out_file", outFile));
        scriptParameters.getParams().put(ParameterFactory.newParameter("resizing", rsz));
    }

    /**
     * Sets up the GUI (panels, buttons, etc) and displays it on the screen.
     */
    private void init() {
        setTitle("Global Pb Edge Detection");

        GridBagConstraints gbc = new GridBagConstraints();
        gbc.weightx = 1;
        gbc.anchor = GridBagConstraints.WEST;
        gbc.fill = GridBagConstraints.HORIZONTAL;

        JPanel paramPanel = new JPanel(new GridBagLayout());
        paramPanel.setBorder(buildTitledBorder("Parameters"));
        
        JLabel labelOutFile = new JLabel("If present base for automatic saves");
        labelOutFile.setForeground(Color.black);
        labelOutFile.setFont(serif12);
        gbc.gridx = 0;
        gbc.gridy = 0;
        paramPanel.add(labelOutFile, gbc);

        textOutFile = new JTextField(20);
        textOutFile.setText("");
        textOutFile.setFont(serif12);
        gbc.gridx = 1;
        paramPanel.add(textOutFile, gbc);

        JLabel labelResizing = new JLabel("Resizing <= 1.0");
        labelResizing.setForeground(Color.black);
        labelResizing.setFont(serif12);
        gbc.gridx = 0;
        gbc.gridy = 1;
        paramPanel.add(labelResizing, gbc);

        textResizing = new JTextField(10);
        textResizing.setText("1.0");
        textResizing.setFont(serif12);
        gbc.gridx = 1;
        paramPanel.add(textResizing, gbc);

        JPanel buttonPanel = new JPanel();
        buildOKButton();
        buttonPanel.add(OKButton);
        buildCancelButton();
        buttonPanel.add(cancelButton);
        buildHelpButton();
        buttonPanel.add(helpButton);

        getContentPane().add(paramPanel);
        getContentPane().add(buttonPanel, BorderLayout.SOUTH);

        pack();
        //setResizable(false);
        setVisible(true);
    }

    /**
     * Use the GUI results to set up the variables needed to run the algorithm.
     *
     * @return  <code>true</code> if parameters set successfully, <code>false</code> otherwise.
     */
    private boolean setVariables() {

        System.gc();

        String tmpStr;
        
        outFile = textOutFile.getText();

        tmpStr = textResizing.getText();

        if (testParameter(tmpStr, 0.0001, 1.0)) {
            rsz = Double.valueOf(tmpStr).doubleValue();
        } else {
            textResizing.requestFocus();
            textResizing.selectAll();

            return false;
        }

        return true;

    }

    /**
     * Return meta-information about this discoverable action for categorization and labeling purposes.
     * 
     * @return Metadata for this action.
     */
    public ActionMetadata getActionMetadata() {
        return new MipavActionMetadata() {
            public String getCategory() {
                return new String("Algorithms.EdgeDetection (GlobalPb)");
            }

            public String getDescription() {
                return new String("Applies Global Pb Edge Detection.");
            }

            public String getDescriptionLong() {
                return new String("Applies Global Pb Edge Detection.");
            }

            public String getShortLabel() {
                return new String("GlobalPbEdgeDetection");
            }

            public String getLabel() {
                return new String("Global Pb Edge Detection");
            }

            public String getName() {
                return new String("Global Pb Edge Detection");
            }
        };
    }


    /**
     * Returns a table listing the input parameters of this algorithm (which should match up with the scripting
     * parameters used in {@link #setGUIFromParams()}).
     * 
     * @return A parameter table listing the inputs of this algorithm.
     */
    public ParameterTable createInputParameters() {
        final ParameterTable table = new ParameterTable();




        
        try {
            table.put(new ParameterExternalImage(AlgorithmParameters.getInputImageLabel(1)));
            table.put(new ParameterString("out_file", ""));
            table.put(new ParameterDouble("resizing", 1.0));
            } catch (final ParserException e) {
            // this shouldn't really happen since there isn't any real parsing going on...
            e.printStackTrace();
        }

        return table;
    }


    /**
     * Returns a table listing the output parameters of this algorithm (usually just labels used to obtain output image
     * names later).
     * 
     * @return A parameter table listing the outputs of this algorithm.
     */
    public ParameterTable createOutputParameters() {
        final ParameterTable table = new ParameterTable();

        try {
            table.put(new ParameterImage(AlgorithmParameters.RESULT_IMAGE));
        } catch (final ParserException e) {
            // this shouldn't really happen since there isn't any real parsing going on...
            e.printStackTrace();
        }

        return table;
    }


    

    /**
     * Returns whether the action has successfully completed its execution.
     * 
     * @return True, if the action is complete. False, if the action failed or is still running.
     */
    public boolean isActionComplete() {
        return isComplete();
    }


}
