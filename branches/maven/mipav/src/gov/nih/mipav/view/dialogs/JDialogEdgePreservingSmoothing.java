package gov.nih.mipav.view.dialogs;


import gov.nih.mipav.model.algorithms.AlgorithmBase;
import gov.nih.mipav.model.algorithms.AlgorithmInterface;
import gov.nih.mipav.model.algorithms.filters.AlgorithmEdgePreservingSmoothing;
import gov.nih.mipav.model.scripting.ParserException;
import gov.nih.mipav.model.scripting.parameters.ParameterBoolean;
import gov.nih.mipav.model.scripting.parameters.ParameterExternalImage;
import gov.nih.mipav.model.scripting.parameters.ParameterFactory;
import gov.nih.mipav.model.scripting.parameters.ParameterImage;
import gov.nih.mipav.model.scripting.parameters.ParameterInt;
import gov.nih.mipav.model.scripting.parameters.ParameterTable;
import gov.nih.mipav.model.structures.ModelImage;
import gov.nih.mipav.view.MipavUtil;
import gov.nih.mipav.view.Preferences;
import gov.nih.mipav.view.ViewImageUpdateInterface;
import gov.nih.mipav.view.ViewJFrameImage;
import gov.nih.mipav.view.ViewUserInterface;
import gov.nih.mipav.view.components.JPanelAlgorithmOutputOptions;
import gov.nih.mipav.view.components.PanelManager;
import gov.nih.mipav.view.components.WidgetFactory;

import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.Frame;
import java.awt.event.ActionEvent;
import java.util.Vector;

import javax.swing.ButtonGroup;
import javax.swing.JLabel;
import javax.swing.JRadioButton;
import javax.swing.JTextField;


/**
 * 
 */
public class JDialogEdgePreservingSmoothing extends JDialogScriptableBase implements AlgorithmInterface, ActionDiscovery {

    /** Use serialVersionUID for interoperability. */
    //private static final long serialVersionUID;

   
    /** Source image. */
    private ModelImage image;
    
    /** output to a new image or overwite the current image */
    private JPanelAlgorithmOutputOptions outputOptionsPanel;

    /** Result image. */
    private ModelImage resultImage = null;

    private JTextField textIterations;
    
    private int iterations;
    
    private ButtonGroup standardGroup;
    
    private JRadioButton standardButton;
    
    private JRadioButton extendedButton;
    
    private boolean standard;
    
    /** locks the frame title */
    private String[] titles;

    /** locking the image */
    private ViewUserInterface userInterface;

    private AlgorithmEdgePreservingSmoothing epsAlgo;
    /**
     * Empty constructor needed for dynamic instantiation.
     */
    public JDialogEdgePreservingSmoothing() {}

    /**
     * Construct the Edge Preserving Smoothing dialog.
     * 
     * @param theParentFrame Parent frame.
     * @param im Source image.
     */
    public JDialogEdgePreservingSmoothing(final Frame theParentFrame, final ModelImage im) {
        super(theParentFrame, false);
        image = im;
        userInterface = ViewUserInterface.getReference();
        init();
        setVisible(true);
    }

    /**
     * Closes dialog box when the OK button is pressed and calls the algorithm.
     * 
     * @param event Event that triggers function.
     */
    public void actionPerformed(final ActionEvent event) {
        final String command = event.getActionCommand();

        if (command.equals("OK")) {
        	if (setVariables()) {
        	    callAlgorithm();
        	}
        } else if (command.equals("Cancel")) {
            dispose();
        } else if (command.equals("Help")) {
            // TODO: testing wiki help
            //MipavUtil.showWebHelp("Filters_(Spatial):_Edge_Preserving_Smoothing");
        } else {
            super.actionPerformed(event);
        }
    }

    /**
     * This method is required if the AlgorithmPerformed interface is implemented. It is called by the algorithm when it
     * has completed or failed to to complete, so that the dialog can be display the result image and/or clean up.
     * 
     * @param algorithm Algorithm that caused the event.
     */
    public void algorithmPerformed(final AlgorithmBase algorithm) {

        if ( algorithm instanceof AlgorithmEdgePreservingSmoothing )
        {
        	if ( algorithm.isCompleted() )
        	{
        		if ( displayInNewFrame )
        		{
        			new ViewJFrameImage( algorithm.getDestImage() );
        		}
        		else
        		{
        			// These next lines set the titles in all frames where the source image is displayed to
        			// image name so as to indicate that the image is now unlocked!
        			// The image frames are enabled and then registered to the userinterface.
        			final Vector<ViewImageUpdateInterface> imageFrames = image.getImageFrameVector();

        			for (int i = 0; i < imageFrames.size(); i++) {
        				((Frame) (imageFrames.elementAt(i))).setTitle(titles[i]);
        				((Frame) (imageFrames.elementAt(i))).setEnabled(true);

        				if ( ((Frame) (imageFrames.elementAt(i))) != parentFrame) {
        					userInterface.registerFrame((Frame) (imageFrames.elementAt(i)));
        				}
        			}

        			if (parentFrame != null) {
        				userInterface.registerFrame(parentFrame);
        			}

        			image.notifyImageDisplayListeners(null, true);
        		}
        	}
        }
        // save the completion status for later
        setComplete(algorithm.isCompleted());

        dispose();
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
            table.put(new ParameterBoolean(AlgorithmParameters.DO_OUTPUT_NEW_IMAGE, true));
            table.put(new ParameterBoolean(AlgorithmParameters.DO_PROCESS_WHOLE_IMAGE, true));
            table.put(new ParameterBoolean("stan", true));
            table.put(new ParameterInt("iter", 1));
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
     * Return meta-information about this discoverable action for categorization and labeling purposes.
     * 
     * @return Metadata for this action.
     */
    public ActionMetadata getActionMetadata() {
        return new MipavActionMetadata() {
            public String getCategory() {
                return new String("Algorithms.Filters (spatial)");
            }

            public String getDescription() {
                return new String("Applies an edge preserving smoothing filter.");
            }

            public String getDescriptionLong() {
                return new String("Applies an edge preserving smoothing filter.");
            }

            public String getLabel() {
                return new String("Edge preserving smoothing");
            }

            public String getName() {
                return new String("Edge preserving smoothing");
            }

            public String getShortLabel() {
                return new String("Edge preserving smoothing");
            }
        };
    }

    /**
     * Returns the name of an image output by this algorithm, the image returned depends on the parameter label given
     * (which can be used to retrieve the image object from the image registry).
     * 
     * @param imageParamName The output image parameter label for which to get the image name.
     * @return The image name of the requested output image parameter label.
     */
    public String getOutputImageName(final String imageParamName) {
        if (imageParamName.equals(AlgorithmParameters.RESULT_IMAGE)) {
            if (getResultImage() != null) {
                // algo produced a new result image
                return getResultImage().getImageName();
            } else {
                // algo was done in place
                return image.getImageName();
            }
        }

        Preferences.debug("Unrecognized output image parameter: " + imageParamName + "\n", Preferences.DEBUG_SCRIPTING);

        return null;
    }
    
    /**
     * Accessor that returns the image.
     * 
     * @return The result image.
     */
    public ModelImage getResultImage()
    {
        return resultImage;
    }

    
    /**
     * Returns whether the action has successfully completed its execution.
     * 
     * @return True, if the action is complete. False, if the action failed or is still running.
     */
    public boolean isActionComplete() {
        return isComplete();
    }

    /**
     * Once all the necessary variables are set, call the Edge Preserving Smoothing algorithm based on what type of image this is
     * and whether or not there is a separate destination image.
     */
    protected void callAlgorithm() {
        final String name = JDialogBase.makeImageName(image.getImageName(), "_edge_preserving_smoothing");
        displayInNewFrame = outputOptionsPanel.isOutputNewImageSet();
        
        if ( displayInNewFrame )
        {
        	resultImage = new ModelImage( image.getType(), image.getExtents(), name );
        	JDialogBase.updateFileInfo( image, resultImage );
        	
    		epsAlgo = new AlgorithmEdgePreservingSmoothing(resultImage, image, standard, iterations,
    				outputOptionsPanel.isProcessWholeImageSet());
        }
        else
        {
        	final Vector<ViewImageUpdateInterface> imageFrames = image.getImageFrameVector();

        	titles = new String[imageFrames.size()];

        	for (int i = 0; i < imageFrames.size(); i++) {
        		titles[i] = ((Frame) (imageFrames.elementAt(i))).getTitle();
        		((Frame) (imageFrames.elementAt(i))).setTitle("Locked: " + titles[i]);
        		((Frame) (imageFrames.elementAt(i))).setEnabled(false);
        		userInterface.unregisterFrame((Frame) (imageFrames.elementAt(i)));
        	}
        	
            epsAlgo = new AlgorithmEdgePreservingSmoothing(null, image, standard, iterations,
        				outputOptionsPanel.isProcessWholeImageSet());
        }
        epsAlgo.addListener(this);

        // Hide the dialog since the algorithm is about to run.
        setVisible(false);
        epsAlgo.run();
    }

    /**
     * Perform any actions required after the running of the algorithm is complete.
     */
    protected void doPostAlgorithmActions() {

        if (outputOptionsPanel.isOutputNewImageSet()) {
            AlgorithmParameters.storeImageInRunner(getResultImage());
        }
    }
    
    /**
     * Accessor that sets the number of iterations.
     *
     * @param  num  Value to set iterations to (should be between 1 and 20).
     */
    public void setIters(int num) {
        iterations = num;
    }

	protected void setGUIFromParams() {
    	image = scriptParameters.retrieveInputImage();
        userInterface = ViewUserInterface.getReference();
        parentFrame = image.getParentFrame();

        if (image.getType() == ModelImage.BOOLEAN) {
            MipavUtil.displayError("Source Image must NOT be Boolean");
            dispose();

            return;
        }

        outputOptionsPanel = new JPanelAlgorithmOutputOptions(image);
        scriptParameters.setOutputOptionsGUI(outputOptionsPanel);
        standard = scriptParameters.getParams().getBoolean("stan");
        setIters(scriptParameters.getNumIterations());
		
	}

	protected void storeParamsFromGUI() throws ParserException {
    	scriptParameters.storeInputImage(image);

        scriptParameters.storeOutputImageParams(getResultImage(), outputOptionsPanel.isOutputNewImageSet());
        scriptParameters.storeProcessingOptions(outputOptionsPanel.isProcessWholeImageSet(), true);
        scriptParameters.getParams().put(ParameterFactory.newParameter("stan", standard));
        scriptParameters.storeNumIterations(iterations);
		
	}

	/**
     * Sets up the GUI (panels, buttons, etc) and displays it on the screen.
     */
    private void init() {
        setForeground(Color.black);

        setTitle("Edge Preserving Smoothing");
        getContentPane().setLayout(new BorderLayout());

        textIterations = WidgetFactory.buildTextField("1");
        textIterations.setColumns(5);
        standardGroup = new ButtonGroup();
        JLabel standardLabel = new JLabel("Type of Maximum Homogeneity Neighbor Filter:");
        standardLabel.setForeground(Color.black);
        standardLabel.setFont(serif12);
        standardButton = new JRadioButton("Standard (Nagao)", true);
        standardButton.setForeground(Color.black);
        standardButton.setFont(serif12);
        standardGroup.add(standardButton);
        extendedButton = new JRadioButton("Extended (Wang)", false);
        extendedButton.setForeground(Color.black);
        extendedButton.setFont(serif12);
        standardGroup.add(extendedButton);
        final PanelManager iterationsOptionsPanelManager = new PanelManager("Iterations");
        iterationsOptionsPanelManager.add( WidgetFactory.buildLabel("Iterations (1 - 20) ") );
        iterationsOptionsPanelManager.add(textIterations);
        final PanelManager filterTypePanelManager = new PanelManager("Filter Type");
        filterTypePanelManager.add(standardLabel);
        filterTypePanelManager.addOnNextLine(standardButton);
        filterTypePanelManager.addOnNextLine(extendedButton);

        outputOptionsPanel = new JPanelAlgorithmOutputOptions(image);

        final PanelManager paramPanelManager = new PanelManager();
        paramPanelManager.add( iterationsOptionsPanelManager.getPanel() );
        paramPanelManager.addOnNextLine(filterTypePanelManager.getPanel() );
        paramPanelManager.addOnNextLine(outputOptionsPanel);
        getContentPane().add(paramPanelManager.getPanel(), BorderLayout.CENTER);
        getContentPane().add(buildButtons(), BorderLayout.SOUTH);
        pack();
        setResizable(true);

        System.gc();
    }
    
    private boolean setVariables() {
        String tmpStr;

        // verify iteration is within bounds
        tmpStr = textIterations.getText();

        if (testParameter(tmpStr, 1, 20)) {
            iterations = Integer.valueOf(tmpStr).intValue();
        } else {
            textIterations.requestFocus();
            textIterations.selectAll();

            return false;
        }
        
        standard = standardButton.isSelected();
        return true;
    }

}
