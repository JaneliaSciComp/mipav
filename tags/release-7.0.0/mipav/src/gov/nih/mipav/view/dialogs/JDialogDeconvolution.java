package gov.nih.mipav.view.dialogs;


import gov.nih.mipav.model.algorithms.AlgorithmBase;
import gov.nih.mipav.model.algorithms.AlgorithmInterface;
import gov.nih.mipav.model.algorithms.OpenCLAlgorithmBase;
import gov.nih.mipav.model.algorithms.filters.OpenCL.filters.OpenCLAlgorithmDeconvolution;
import gov.nih.mipav.model.scripting.ParserException;
import gov.nih.mipav.model.scripting.parameters.Parameter;
import gov.nih.mipav.model.scripting.parameters.ParameterBoolean;
import gov.nih.mipav.model.scripting.parameters.ParameterExternalImage;
import gov.nih.mipav.model.scripting.parameters.ParameterImage;
import gov.nih.mipav.model.scripting.parameters.ParameterList;
import gov.nih.mipav.model.scripting.parameters.ParameterTable;
import gov.nih.mipav.model.structures.ModelImage;
import gov.nih.mipav.view.MipavUtil;
import gov.nih.mipav.view.Preferences;
import gov.nih.mipav.view.ViewImageUpdateInterface;
import gov.nih.mipav.view.ViewJFrameImage;
import gov.nih.mipav.view.ViewUserInterface;
import gov.nih.mipav.view.components.JPanelAlgorithmOutputOptions;
import gov.nih.mipav.view.components.JPanelColorChannels;
import gov.nih.mipav.view.components.PanelManager;
import gov.nih.mipav.view.components.WidgetFactory;

import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.Frame;
import java.awt.event.ActionEvent;
import java.util.Vector;

import javax.swing.JCheckBox;
import javax.swing.JPanel;
import javax.swing.JTextField;


/**
 * @see OpenCLAlgorithmDeconvolution
 */
public class JDialogDeconvolution extends JDialogScriptableBase implements AlgorithmInterface, ActionDiscovery {

    /** Use serialVersionUID for interoperability. */
    private static final long serialVersionUID = -5074546334694615886L;

    /** for processing color images */
    private JPanelColorChannels colorChannelPanel;

    /** Source image. */
    private ModelImage image;
    
    /** Source image. */
    private ModelImage imageB;
    
    /** output to a new image or overwite the current image */
    private JPanelAlgorithmOutputOptions outputOptionsPanel;

    /** Result image. */
    private ModelImage resultImage = null;

    private JTextField textIterations;
    
    /** locks the frame title */
    private String[] titles;

    /** locking the image */
    private ViewUserInterface userInterface;

    /** sigma conversion factor is by 1.0 / (2*Math.sqrt(2*Math.log(2))) */
    private JCheckBox conversionFactorCheckbox = null;
    
    /** gui, sigma value */
    private JTextField[] textGaussX = new JTextField[2];

    /** gui, sigma value */
    private JTextField[] textGaussY = new JTextField[2];

    /** gui, sigma value */
    private JTextField[] textGaussZ = new JTextField[2];


    /**
     * Empty constructor needed for dynamic instantiation.
     */
    public JDialogDeconvolution() {}

    /**
     * Construct the deconvolution blur dialog.
     * 
     * @param theParentFrame Parent frame.
     * @param im Source image.
     */
    public JDialogDeconvolution(final Frame theParentFrame, final ModelImage im, final ModelImage imB) {
        super(theParentFrame, false);
        if ( !OpenCLAlgorithmBase.isOCLAvailable() )
        {
        	MipavUtil.displayError( "OpenCL is not available on any platform" );
        }
        image = im;
        imageB = imB;
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

        if ( event.getSource() == textIterations )
        {
            if (!MipavUtil.testParameter(textIterations.getText(), 1, 50)) {
            	textIterations.requestFocus();
            	textIterations.selectAll();
            }
        }
        else if (command.equals("OK")) {
        	callAlgorithm();
        } else if (command.equals("Cancel")) {
            dispose();
        } else if (command.equals("Help")) {
            // TODO: testing wiki help
            //MipavUtil.showHelp("10009");
            MipavUtil.showWebHelp("Filters_(Spatial):_Gaussian_Blur#Applying_the_Gaussian_Blur_algorithm");
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

        if ( algorithm instanceof OpenCLAlgorithmDeconvolution )
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
            table.put(new ParameterBoolean(AlgorithmParameters.DO_PROCESS_SEPARABLE, true));
            table.put(new ParameterBoolean(AlgorithmParameters.DO_PROCESS_3D_AS_25D, false));
            table.put(new ParameterList(AlgorithmParameters.SIGMAS, Parameter.PARAM_FLOAT, "1.0,1.0,1.0"));
            table.put(new ParameterBoolean(AlgorithmParameters.SIGMA_DO_Z_RES_CORRECTION, true));
            table.put(new ParameterList(AlgorithmParameters.DO_PROCESS_RGB, Parameter.PARAM_BOOLEAN, "true,true,true"));
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
                return new String("Applies a simple deconvolution filter.");
            }

            public String getDescriptionLong() {
                return new String("Applies a simple deconvolution filter.");
            }

            public String getLabel() {
                return new String("Deconvolution");
            }

            public String getName() {
                return new String("Deconvolution");
            }

            public String getShortLabel() {
                return new String("Deconvolution");
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
     * Once all the necessary variables are set, call the Gaussian Blur algorithm based on what type of image this is
     * and whether or not there is a separate destination image.
     */
    protected void callAlgorithm() {
        final String name = JDialogBase.makeImageName(image.getImageName(), "_deconvolution");
        displayInNewFrame = outputOptionsPanel.isOutputNewImageSet();

        int iterations = Integer.parseInt(textIterations.getText());
        iterations = Math.max( 1, iterations );
        iterations = Math.min( 50, iterations);

        float[] sigmas = new float[] {
                Float.parseFloat(textGaussX[0].getText()), Float.parseFloat(textGaussY[0].getText()),
                Float.parseFloat(textGaussZ[0].getText())
            };
        float[] sigmasB = null;
        
        if ( imageB != null )
        {
        	sigmasB = new float[] {
                Float.parseFloat(textGaussX[1].getText()), Float.parseFloat(textGaussY[1].getText()),
                Float.parseFloat(textGaussZ[1].getText())
            };
        }
        OpenCLAlgorithmDeconvolution blurAlgo;
        if ( displayInNewFrame )
        {
        	resultImage = new ModelImage( image.getType(), image.getExtents(), name );
        	JDialogBase.updateFileInfo( image, resultImage );
        	if ( imageB != null )
        	{
        		blurAlgo = new OpenCLAlgorithmDeconvolution(resultImage, image, imageB, 
        				sigmas, sigmasB, outputOptionsPanel.isProcessWholeImageSet(), 
        				iterations, conversionFactorCheckbox.isSelected());        		
        	}
        	else
        	{
        		blurAlgo = new OpenCLAlgorithmDeconvolution(resultImage, image, 
        				sigmas, outputOptionsPanel.isProcessWholeImageSet(), 
        				iterations, conversionFactorCheckbox.isSelected());
        	}
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
        	if ( imageB != null )
        	{
        		blurAlgo = new OpenCLAlgorithmDeconvolution(image, imageB, sigmas, sigmasB,
        				outputOptionsPanel.isProcessWholeImageSet(), 
        				iterations, conversionFactorCheckbox.isSelected());        	
        	}
        	else
        	{
        		blurAlgo = new OpenCLAlgorithmDeconvolution(image, sigmas,
        				outputOptionsPanel.isProcessWholeImageSet(), 
        				iterations, conversionFactorCheckbox.isSelected());
        	}
        }
        blurAlgo.setRed(colorChannelPanel.isRedProcessingRequested());
        blurAlgo.setGreen(colorChannelPanel.isGreenProcessingRequested());
        blurAlgo.setBlue(colorChannelPanel.isBlueProcessingRequested());
        blurAlgo.addListener(this);

        // Hide the dialog since the algorithm is about to run.
        setVisible(false);
        blurAlgo.run();
    }

    /**
     * Perform any actions required after the running of the algorithm is complete.
     */
    protected void doPostAlgorithmActions() {

        if (outputOptionsPanel.isOutputNewImageSet()) {
            AlgorithmParameters.storeImageInRunner(getResultImage());
        }
    }

    @Override
	protected void setGUIFromParams() {
		// TODO Auto-generated method stub
		
	}

    @Override
	protected void storeParamsFromGUI() throws ParserException {
		// TODO Auto-generated method stub
		
	}

	/**
     * Sets up the GUI (panels, buttons, etc) and displays it on the screen.
     */
    private void init() {
        setForeground(Color.black);

        setTitle("Deconvolution");
        getContentPane().setLayout(new BorderLayout());

        textIterations = WidgetFactory.buildTextField("10");
        textIterations.setColumns(5);
        textIterations.addActionListener(this);
        final PanelManager iterationsOptionsPanelManager = new PanelManager("Iterations");
        iterationsOptionsPanelManager.add( WidgetFactory.buildLabel("Iterations (1 - 50.0) ") );
        iterationsOptionsPanelManager.add(textIterations);

        JPanel sigmaPanel;
        JPanel sigmaPanelB = null;
        if ( imageB != null )
        {
            sigmaPanel = initGUI(0, true, " Image A");
            sigmaPanelB = initGUI(1, true, " Image B");        	
        }
        else
        {
            sigmaPanel = initGUI(0, false, "");
        }

        colorChannelPanel = new JPanelColorChannels(image);
        outputOptionsPanel = new JPanelAlgorithmOutputOptions(image);

        final PanelManager paramPanelManager = new PanelManager();
        paramPanelManager.add( iterationsOptionsPanelManager.getPanel() );
        paramPanelManager.addOnNextLine(sigmaPanel);
        if ( sigmaPanelB != null )
        {
        	paramPanelManager.addOnNextLine(sigmaPanelB);
        }
        conversionFactorCheckbox = WidgetFactory.buildCheckBox("Use sigma conversion factor.", true);
        paramPanelManager.addOnNextLine(conversionFactorCheckbox);
        paramPanelManager.addOnNextLine(colorChannelPanel);
        paramPanelManager.addOnNextLine(outputOptionsPanel);

        getContentPane().add(paramPanelManager.getPanel(), BorderLayout.CENTER);
        getContentPane().add(buildButtons(), BorderLayout.SOUTH);
        pack();
        setResizable(true);

        System.gc();
    }

	/**
     * Initialize the Sigma Panel
     */
    private JPanel initGUI(int index, boolean dualImage, String postfix)
    {
        PanelManager scalePanelManager = new PanelManager();
        if ( dualImage )
        {
        	scalePanelManager.getPanel().setBorder(WidgetFactory.buildTitledBorder("Sigmas"+postfix));
        }
        else
        {
        	scalePanelManager.getPanel().setBorder(WidgetFactory.buildTitledBorder("Sigmas"));
        }
        

        textGaussX[index] = WidgetFactory.buildTextField("1.0");
        textGaussX[index].setColumns(5);
        textGaussY[index] = WidgetFactory.buildTextField("1.0");
        textGaussY[index].setColumns(5);
        textGaussZ[index] = WidgetFactory.buildTextField("1.0");
        textGaussZ[index].setColumns(5);
        
        scalePanelManager.add(WidgetFactory.buildLabel("X dimension (>= 0.0) "));
        scalePanelManager.add(textGaussX[index]);
        scalePanelManager.addOnNextLine(WidgetFactory.buildLabel("Y dimension (>= 0.0) "));
        scalePanelManager.add(textGaussY[index]);
        scalePanelManager.addOnNextLine(WidgetFactory.buildLabel("Z dimension (>= 0.0) "));
        scalePanelManager.add(textGaussZ[index]);
        return scalePanelManager.getPanel();
    }
}
