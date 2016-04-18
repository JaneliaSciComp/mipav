package gov.nih.mipav.view.dialogs;


import gov.nih.mipav.model.algorithms.*;
import gov.nih.mipav.model.scripting.*;
import gov.nih.mipav.model.scripting.parameters.*;
import gov.nih.mipav.model.structures.*;
import gov.nih.mipav.view.*;

import java.awt.*;
import java.awt.event.*;
import java.util.*;

import javax.swing.*;


/**
 * Dialog to get user input, then call the algorithm. The user has the option to generate a new image or replace the
 * source image. It should be noted that the algorithms are executed in their own threads.
 *
 * @version  1.0 April 15, 2016
 * @author   William Gandler
 * @see      AlgorithmFacetModel
 */
public class JDialogFacetModel extends JDialogScriptableBase implements AlgorithmInterface, ItemListener, ActionDiscovery, ScriptableActionInterface {

    //~ Static fields/initializers -------------------------------------------------------------------------------------

    /** Use serialVersionUID for interoperability. */
    //private static final long serialVersionUID;

	// Only FACET_BASED_PEAK_NOISE_REMOVAL, ITERATED_FACET_MODEL, and GRADIENT_BASED_FACET_EDGE_DETECTION
	// can use 3 by 3 masks.
	public static final int FACET_BASED_PEAK_NOISE_REMOVAL = 1;
	public static final int ITERATED_FACET_MODEL = 2;
	public static final int GRADIENT_BASED_FACET_EDGE_DETECTION = 3;
	public static final int ZERO_CROSSING_EDGE_DETECTOR = 4;
	// INTEGRATED_DIRECTIONAL_DERIVATIVE uses only blockSide = 5 or 7.
	public static final int INTEGRATED_DIRECTIONAL_DERIVATIVE_ANGLE = 5;
	public static final int INTEGRATED_DIRECTIONAL_DERIVATIVE_MAGNITUDE = 6;
	public static final int CORNER_DETECTOR = 7;
	
    //~ Instance fields ------------------------------------------------------------------------------------------------
		
	// Square blocks of blockSide by blockSide pixels used.  BlockSide must be an odd integer.
	private int blockSide;
	
	// Probability of rejecting a true hypothesis.  A reasonable range for alpha is .01 to .05
	// Used only in facetBasedPeakNoiseRemoval and gradientBasedFacetEdgeDetection
    private double alpha;
    
    // Tells which of the facet model routines to use
    private int routine;
    
    // Used only in cornerDetector
    private double gradientDirectionThreshold = 20.0;
    
    private JRadioButton gradientEdge;
    
    private JLabel labelAlpha;
    
    private JTextField textAlpha;
    
    private JRadioButton zeroDetector;

    /** DOCUMENT ME! */
    private JRadioButton integratedAngle;
    
    private JRadioButton integratedMagnitude;
    
    private JRadioButton cornerDetector;
    
    private JLabel labelThreshold;
    
    private JTextField textThreshold;

    /** DOCUMENT ME! */
    private ButtonGroup algoGroup;

    /** DOCUMENT ME! */
    private JPanel algoPanel;

    /** DOCUMENT ME! */
    private ButtonGroup destinationGroup;

    /** DOCUMENT ME! */
    private JPanel destinationPanel;

    /** DOCUMENT ME! */
    private int displayLoc; // Flag indicating if a new image is to be generated

    /** DOCUMENT ME! */
    private JPanel maskPanel;
    
    private JRadioButton mask3;
    
    private JRadioButton mask5;
    
    private JRadioButton mask7;
    
    private JRadioButton mask9;
    
    private JRadioButton mask11;
    
    private JRadioButton mask13;

    /** DOCUMENT ME! */
    private ButtonGroup maskGroup;

    /** DOCUMENT ME! */
    private AlgorithmFacetModel facetAlgo = null;

    /** DOCUMENT ME! */
    private JRadioButton iteratedModel;

    /** DOCUMENT ME! */
    private ModelImage image; // source image

    /** DOCUMENT ME! */
    private JPanel mainPanel;

    /** DOCUMENT ME! */
    private JRadioButton newImage;

    /** DOCUMENT ME! */
    private JRadioButton replaceImage;

    /** DOCUMENT ME! */
    private ModelImage resultImage = null; // result image

    /** DOCUMENT ME! */
    private String[] titles;

    /** DOCUMENT ME! */
    private ViewUserInterface userInterface;

    /** DOCUMENT ME! */
    private JRadioButton noiseRemoval;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Empty constructor needed for dynamic instantiation (used during scripting).
     */
    public JDialogFacetModel() { }

    /**
     * Used primarily for the script to store variables and run the algorithm. No actual dialog will appear but the set
     * up info and result image will be stored here.
     *
     * @param  im  Source image.
     */
    public JDialogFacetModel(ModelImage im) {
        super();
        userInterface = ViewUserInterface.getReference();
        image = im;
        parentFrame = image.getParentFrame();
    }

    // or if the source image is to be replaced
    /**
     * Creates a new JDialogFacetModel object.
     *
     * @param  theParentFrame  Parent frame.
     * @param  im              Source image.
     */
    public JDialogFacetModel(Frame theParentFrame, ModelImage im) {
        super(theParentFrame, false);
        image = im;
        userInterface = ViewUserInterface.getReference();
        init();
    }

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * Closes dialog box when the OK button is pressed and calls the algorithm.
     *
     * @param  event  Event that triggers function.
     */
    public void actionPerformed(ActionEvent event) {
        String command = event.getActionCommand();
        Object source = event.getSource();

        if (command.equals("OK")) {

            if (setVariables()) {
                callAlgorithm();
            }
        } else if ((source == noiseRemoval) || (source == gradientEdge) || (source == iteratedModel) || (source == zeroDetector) ||
                   (source == integratedAngle) || (source == integratedMagnitude) || (source == cornerDetector)) {
            if (noiseRemoval.isSelected() || (gradientEdge.isSelected())) {
            	labelAlpha.setEnabled(true);
            	textAlpha.setEnabled(true);
            	labelThreshold.setEnabled(false);
            	textThreshold.setEnabled(false);
            	mask3.setEnabled(true);
            	mask9.setEnabled(true);
            	mask11.setEnabled(true);
            	mask13.setEnabled(true);
            }
            else if (iteratedModel.isSelected()) {
            	labelAlpha.setEnabled(false);
            	textAlpha.setEnabled(false);
            	labelThreshold.setEnabled(false);
            	textThreshold.setEnabled(false);
            	mask3.setEnabled(true);
            	mask9.setEnabled(true);
            	mask11.setEnabled(true);
            	mask13.setEnabled(true);	
            }
            else if (zeroDetector.isSelected()) {
            	if (mask3.isSelected()) {
            		mask3.setSelected(false);
            		mask5.setSelected(true);
            	}
            	labelAlpha.setEnabled(false);
            	textAlpha.setEnabled(false);
            	labelThreshold.setEnabled(false);
            	textThreshold.setEnabled(false);
            	mask3.setEnabled(false);
            	mask9.setEnabled(true);
            	mask11.setEnabled(true);
            	mask13.setEnabled(true);		
            }
            else if (integratedAngle.isSelected() || integratedMagnitude.isSelected()) {
            	if (mask3.isSelected()) {
            		mask3.setSelected(false);
            		mask5.setSelected(true);
            	}
            	else if (mask9.isSelected()) {
            		mask9.setSelected(false);
            		mask5.setSelected(true);
            	}
            	else if (mask11.isSelected()) {
            		mask11.setSelected(false);
            		mask5.setSelected(true);
            	}
            	else if (mask13.isSelected()) {
            		mask13.setSelected(false);
            		mask5.setSelected(true);
            	}
            	labelAlpha.setEnabled(false);
            	textAlpha.setEnabled(false);
            	labelThreshold.setEnabled(false);
            	textThreshold.setEnabled(false);
            	mask3.setEnabled(false);
            	mask9.setEnabled(false);
            	mask11.setEnabled(false);
            	mask13.setEnabled(false);	
            }
            else if (cornerDetector.isSelected()) {
            	if (mask3.isSelected()) {
            		mask3.setSelected(false);
            		mask5.setSelected(true);
            	}
            	labelAlpha.setEnabled(false);
            	textAlpha.setEnabled(false);
            	labelThreshold.setEnabled(true);
            	textThreshold.setEnabled(true);
            	mask3.setEnabled(false);
            	mask9.setEnabled(true);
            	mask11.setEnabled(true);
            	mask13.setEnabled(true);			
            }
        
        } else if (command.equals("Cancel")) {
            dispose();
        } else if (source == helpButton) {
             
        } else {
            super.actionPerformed(event);
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

        if (algorithm instanceof AlgorithmFacetModel) {

            if ((algorithm.isCompleted() == true) && (resultImage != null)) {

            	JDialogBase.updateFileInfo( image, resultImage );

                try {
                    new ViewJFrameImage(resultImage, null, new Dimension(610, 200));
                } catch (OutOfMemoryError error) {
                    System.gc();
                    MipavUtil.displayError("Out of memory: unable to open new frame");
                }
            } else if (resultImage == null) {

                // These next lines set the titles in all frames where the source image is displayed to
                // image name so as to indicate that the image is now unlocked!
                // The image frames are enabled and then registed to the userinterface.
                Vector<ViewImageUpdateInterface> imageFrames = image.getImageFrameVector();

                for (int i = 0; i < imageFrames.size(); i++) {
                    ((ViewJFrameBase) (imageFrames.elementAt(i))).setTitle(titles[i]);
                    ((ViewJFrameBase) (imageFrames.elementAt(i))).setEnabled(true);

                    if (((Frame) (imageFrames.elementAt(i))) != parentFrame) {
                        ((ViewJFrameBase) parentFrame).getUserInterface().registerFrame((Frame)
                                                                                        (imageFrames.elementAt(i)));
                    }
                }

                if (parentFrame != null) {
                    ((ViewJFrameBase) parentFrame).getUserInterface().registerFrame(parentFrame);
                    ((ViewJFrameImage) parentFrame).getComponentImage().setLogMagDisplay(true);
                }

                updateFileTypeInfo(image, ModelStorageBase.DOUBLE);
                image.notifyImageDisplayListeners(null, true);
            } else if (resultImage != null) {

                // algorithm failed but result image still has garbage
                resultImage.disposeLocal(); // clean up memory
                resultImage = null;
            }

            if (algorithm.isCompleted()) {
                insertScriptLine();
            }
        }
        
       
     // save the completion status for later
        setComplete(algorithm.isCompleted());

        if (facetAlgo != null) {
            facetAlgo.finalize();
            facetAlgo = null;
        }
       
        dispose();
    }

    /**
     * Accessor that returns the image.
     *
     * @return  The result image.
     */
    public ModelImage getResultImage() {
        return resultImage;
    }

    /**
     * Accessor that sets the display loc variable to new, so that a new image is created once the algorithm completes.
     */
    public void setDisplayLocNew() {
        displayLoc = NEW;
    }

    /**
     * Accessor that sets the display loc variable to replace, so the current image is replaced once the algorithm
     * completes.
     */
    public void setDisplayLocReplace() {
        displayLoc = REPLACE;
    }

    
    /**
     * Once all the necessary variables are set, call the Frequency Filter algorithm based on what type of image this is
     * and whether or not there is a separate destination image.
     */
    protected void callAlgorithm() {

        String name = makeImageName(image.getImageName(), "_facetModel");

        if (displayLoc == NEW) {

            try {
            	resultImage = new ModelImage(ModelStorageBase.DOUBLE, image.getExtents(), name );
                resultImage.resetVOIs();

                // Make algorithm
                facetAlgo = new AlgorithmFacetModel(resultImage, image, routine, blockSide, alpha,
                		gradientDirectionThreshold);

                // This is very important. Adding this object as a listener allows the algorithm to
                // notify this object when it has completed or failed. See algorithm performed event.
                // This is made possible by implementing AlgorithmedPerformed interface
                facetAlgo.addListener(this);
                createProgressBar(image.getImageName(), facetAlgo);

                // Hide dialog since the algorithm is about to run
                setVisible(false);

                if (isRunInSeparateThread()) {

                    // Start the thread as a low priority because we wish to still have user interface work fast.
                    if (facetAlgo.startMethod(Thread.MIN_PRIORITY) == false) {
                        MipavUtil.displayError("A thread is already running on this object");
                    }
                } else {

                    facetAlgo.run();
                }
            } catch (OutOfMemoryError x) {
                MipavUtil.displayError("Dialog Facet Model: unable to allocate enough memory");

                if (resultImage != null) {
                    resultImage.disposeLocal(); // Clean up memory of result image
                    resultImage = null;
                }

                return;
            }
        } else {

            try {

                // No need to make new image space because the user has choosen to replace the source image
                // Make the algorithm class
                facetAlgo = new AlgorithmFacetModel(image, routine, blockSide, alpha,
                		gradientDirectionThreshold);

                // This is very important. Adding this object as a listener allows the algorithm to
                // notify this object when it has completed or failed. See algorithm performed event.
                // This is made possible by implementing AlgorithmedPerformed interface
                facetAlgo.addListener(this);

                createProgressBar(image.getImageName(), facetAlgo);

                // Hide the dialog since the algorithm is about to run.
                setVisible(false);

                // These next lines set the titles in all frames where the source image is displayed to
                // "locked - " image name so as to indicate that the image is now read/write locked!
                // The image frames are disabled and then unregisted from the userinterface until the
                // algorithm has completed.
                Vector<ViewImageUpdateInterface> imageFrames = image.getImageFrameVector();
                titles = new String[imageFrames.size()];

                for (int i = 0; i < imageFrames.size(); i++) {
                    titles[i] = ((ViewJFrameBase) (imageFrames.elementAt(i))).getTitle();
                    ((ViewJFrameBase) (imageFrames.elementAt(i))).setTitle("Locked: " + titles[i]);
                    ((ViewJFrameBase) (imageFrames.elementAt(i))).setEnabled(false);
                    userInterface.unregisterFrame((Frame) (imageFrames.elementAt(i)));
                }

                if (isRunInSeparateThread()) {

                    // Start the thread as a low priority because we wish to still have user interface work fast.
                    if (facetAlgo.startMethod(Thread.MIN_PRIORITY) == false) {
                        MipavUtil.displayError("A thread is already running on this object");
                    }
                } else {


                    facetAlgo.run();
                }
            } catch (OutOfMemoryError x) {
                MipavUtil.displayError("Dialog Facet Model: unable to allocate enough memory");

                return;
            }
        }
    }
    
    /**
     * 
     * @param routine
     */
    private void setRoutine(int routine) {
    	
    	this.routine = routine;
    }
    
    /**
     * 
     * @param blockSide
     */
    private void setBlockSide(int blockSide) {
    	this.blockSide = blockSide;
    }
    
    /**
     * 
     * @param alpha
     */
    private void setAlpha(double alpha) {
    	this.alpha = alpha;
    }
    
    /**
     * 
     * @param gradientDirectionThreshold
     */
    private void setGradientDirectionThreshold(double gradientDirectionThreshold) {
    	this.gradientDirectionThreshold = gradientDirectionThreshold;
    }

    /**
     * Store the result image in the script runner's image table now that the action execution is finished.
     */
    protected void doPostAlgorithmActions() {

        if (displayLoc == NEW) {
            AlgorithmParameters.storeImageInRunner(getResultImage());
        }
    }

    /**
     * {@inheritDoc}
     */
    protected void setGUIFromParams() {
        image = scriptParameters.retrieveInputImage();
        userInterface = ViewUserInterface.getReference();
        parentFrame = image.getParentFrame();

        if (scriptParameters.getParams().getBoolean(AlgorithmParameters.DO_OUTPUT_NEW_IMAGE)) {
            setDisplayLocNew();
        } else {
            setDisplayLocReplace();
        }

        setRoutine(scriptParameters.getParams().getInt("rout"));
        setBlockSide(scriptParameters.getParams().getInt("block_side"));
        setAlpha(scriptParameters.getParams().getDouble("alph"));
        setGradientDirectionThreshold(scriptParameters.getParams().getDouble("gradient_threshold"));
    }

    /**
     * {@inheritDoc}
     */
    protected void storeParamsFromGUI() throws ParserException {
        scriptParameters.storeInputImage(image);
        scriptParameters.storeOutputImageParams(getResultImage(), (displayLoc == NEW));

        scriptParameters.getParams().put(ParameterFactory.newParameter("rout", routine));
        scriptParameters.getParams().put(ParameterFactory.newParameter("block_side", blockSide));
        scriptParameters.getParams().put(ParameterFactory.newParameter("alph", alpha));
        scriptParameters.getParams().put(ParameterFactory.newParameter("gradient_threshold", gradientDirectionThreshold));
    }

    /**
     * Sets up the GUI (panels, buttons, etc) and displays it on the screen.
     */
    private void init() {
        setTitle("Facet Model");
        
        algoPanel = new JPanel(new GridBagLayout());
        algoPanel.setBorder(buildTitledBorder("Facet model algorithms"));

        algoGroup = new ButtonGroup();
        noiseRemoval = new JRadioButton("Facet based peak noise removal", true);
        noiseRemoval.setFont(serif12);
        noiseRemoval.setForeground(Color.black);
        noiseRemoval.addActionListener(this);
        algoGroup.add(noiseRemoval);

        gradientEdge = new JRadioButton("Gradient based facet edge detection", false);
        gradientEdge.setFont(serif12);
        gradientEdge.setForeground(Color.black);
        gradientEdge.addActionListener(this);
        algoGroup.add(gradientEdge);
        
        labelAlpha = new JLabel("True hypothesis rejection probability");
        labelAlpha.setForeground(Color.black);
        labelAlpha.setFont(serif12);
        labelAlpha.setEnabled(true);
        
        textAlpha = new JTextField(10);
        textAlpha.setText("0.01");
        textAlpha.setFont(serif12);
        textAlpha.setForeground(Color.black);
        textAlpha.setEnabled(true);    

        iteratedModel = new JRadioButton("Iterated facet model", false);
        iteratedModel.setFont(serif12);
        iteratedModel.setForeground(Color.black);
        iteratedModel.addActionListener(this);
        algoGroup.add(iteratedModel);
        
        zeroDetector = new JRadioButton("Zero crossing edge detector", false);
        zeroDetector.setFont(serif12);;
        zeroDetector.setForeground(Color.black);
        zeroDetector.addActionListener(this);
        algoGroup.add(zeroDetector);

        integratedAngle = new JRadioButton("Integrated directional derivative angle", false);
        integratedAngle.setFont(serif12);
        integratedAngle.setForeground(Color.black);
        integratedAngle.addActionListener(this);
        algoGroup.add(integratedAngle);
        
        integratedMagnitude = new JRadioButton("Integrated directional derivative magnitude", false);
        integratedMagnitude.setFont(serif12);
        integratedMagnitude.setForeground(Color.black);
        integratedMagnitude.addActionListener(this);
        algoGroup.add(integratedMagnitude);
        
        cornerDetector = new JRadioButton("Corner detector", false);
        cornerDetector.setFont(serif12);
        cornerDetector.setForeground(Color.black);
        cornerDetector.addActionListener(this);
        algoGroup.add(cornerDetector);
        
        labelThreshold = new JLabel("Gradient direction threshold");
        labelThreshold.setForeground(Color.black);
        labelThreshold.setFont(serif12);
        labelThreshold.setEnabled(false);
        
        textThreshold = new JTextField(10);
        textThreshold.setText("20.0");
        textThreshold.setFont(serif12);
        textThreshold.setForeground(Color.black);
        textThreshold.setEnabled(false);        

        GridBagConstraints gbc = new GridBagConstraints();
        gbc.gridwidth = 3;
        gbc.anchor = GridBagConstraints.WEST;
        gbc.weightx = 1;

        gbc.gridx = 0;
        gbc.gridy = 0;
        algoPanel.add(noiseRemoval, gbc);
        gbc.gridy = 1;
        gbc.gridwidth = 1;
        algoPanel.add(gradientEdge, gbc);
        gbc.gridx = 0;
        gbc.gridy = 2;
        algoPanel.add(labelAlpha, gbc);
        gbc.gridx = 1;
        algoPanel.add(textAlpha, gbc);
        gbc.gridx = 0;
        gbc.gridy = 3;
        gbc.gridwidth = 3;
        algoPanel.add(iteratedModel, gbc);
        gbc.gridy = 4;
        algoPanel.add(zeroDetector, gbc);
        gbc.gridy = 5;
        algoPanel.add(integratedAngle, gbc);
        gbc.gridy = 6;
        algoPanel.add(integratedMagnitude, gbc);
        gbc.gridy = 7;
        algoPanel.add(cornerDetector, gbc);
        gbc.gridx = 0;
        gbc.gridy = 8;
        gbc.gridwidth = 1;
        algoPanel.add(labelThreshold, gbc);
        gbc.gridx = 1;
        algoPanel.add(textThreshold, gbc);

        destinationPanel = new JPanel(new GridLayout(2, 1));
        destinationPanel.setForeground(Color.black);
        destinationPanel.setBorder(buildTitledBorder("Destination"));

        destinationGroup = new ButtonGroup();
        newImage = new JRadioButton("New image", true);
        newImage.setFont(serif12);
        newImage.setForeground(Color.black);
        destinationGroup.add(newImage);
        destinationPanel.add(newImage);

        replaceImage = new JRadioButton("Replace image", false);
        replaceImage.setFont(serif12);
        replaceImage.setForeground(Color.black);
        destinationGroup.add(replaceImage);
        destinationPanel.add(replaceImage);

        // Only if the image is unlocked can it be replaced.
        if (image.getLockStatus() == ModelStorageBase.UNLOCKED) {
            replaceImage.setEnabled(true);
        } else {
            replaceImage.setEnabled(false);
        }

        maskPanel = new JPanel(new GridBagLayout());
        maskPanel.setBorder(buildTitledBorder("Mask size"));

        maskGroup = new ButtonGroup();

        mask3 = new JRadioButton("3 x 3", true);
        mask3.setFont(serif12);
        mask3.setForeground(Color.black);
        maskGroup.add(mask3);
        
        mask5 = new JRadioButton("5 x 5", false);
        mask5.setFont(serif12);
        mask5.setForeground(Color.black);
        maskGroup.add(mask5);
        
        mask7 = new JRadioButton("7 x 7", false);
        mask7.setFont(serif12);
        mask7.setForeground(Color.black);
        maskGroup.add(mask7);
        
        mask9 = new JRadioButton("9 x 9", false);
        mask9.setFont(serif12);
        mask9.setForeground(Color.black);
        maskGroup.add(mask9);
        
        mask11 = new JRadioButton("11 x 11", false);
        mask11.setFont(serif12);
        mask11.setForeground(Color.black);
        maskGroup.add(mask11);
        
        mask13 = new JRadioButton("13 x 13", false);
        mask13.setFont(serif12);
        mask13.setForeground(Color.black);
        maskGroup.add(mask13);

        

        gbc.gridx = 0;
        gbc.gridy = 0;
        gbc.gridwidth = 1;
        gbc.weightx = 0;
        gbc.fill = GridBagConstraints.NONE;
        maskPanel.add(mask3, gbc);
        gbc.gridy = 1;
        maskPanel.add(mask5, gbc);
        gbc.gridy = 2;
        maskPanel.add(mask7, gbc);
        gbc.gridy = 3;
        maskPanel.add(mask9, gbc);
        gbc.gridy = 4;
        maskPanel.add(mask11, gbc);
        gbc.gridy = 5;
        maskPanel.add(mask13, gbc);

        mainPanel = new JPanel(new GridBagLayout());
        gbc.gridx = 0;
        gbc.gridy = 0;
        gbc.fill = GridBagConstraints.HORIZONTAL;
        mainPanel.add(algoPanel, gbc);
        gbc.gridy = 2;
        mainPanel.add(destinationPanel, gbc);
        gbc.gridy = 3;
        mainPanel.add(maskPanel, gbc);

        getContentPane().add(mainPanel);
        getContentPane().add(buildButtons(), BorderLayout.SOUTH);

        pack();
        setVisible(true);
    }

    /**
     * Use the GUI results to set up the variables needed to run the algorithm.
     *
     * @return  <code>true</code> if parameters set successfully, <code>false</code> otherwise.
     */
    private boolean setVariables() {
        String tmpStr;

        if (replaceImage.isSelected()) {
            displayLoc = REPLACE;
        } else if (newImage.isSelected()) {
            displayLoc = NEW;
        }
        
        if (noiseRemoval.isSelected()) {
        	routine = FACET_BASED_PEAK_NOISE_REMOVAL;
        }
        else if (gradientEdge.isSelected()) {
            routine = GRADIENT_BASED_FACET_EDGE_DETECTION;	
        }
        else if (iteratedModel.isSelected()) {
        	routine = ITERATED_FACET_MODEL;
        }
        else if (zeroDetector.isSelected()) {
        	routine = ZERO_CROSSING_EDGE_DETECTOR;
        }
        else if (integratedAngle.isSelected()) {
        	routine = INTEGRATED_DIRECTIONAL_DERIVATIVE_ANGLE;
        }
        else if (integratedMagnitude.isSelected()) {
        	routine = INTEGRATED_DIRECTIONAL_DERIVATIVE_MAGNITUDE;
        }
        else {
        	routine = CORNER_DETECTOR;
        }
        
        if ((routine == FACET_BASED_PEAK_NOISE_REMOVAL) || (routine == GRADIENT_BASED_FACET_EDGE_DETECTION)) {
        	tmpStr = textAlpha.getText();
            alpha = Double.parseDouble(tmpStr);
            
            if (alpha < 0.0) {
            	MipavUtil.displayError("alpha must be at least 0.0");
            	textAlpha.requestFocus();
            	textAlpha.selectAll();
            	return false;
            }
            
            if (alpha > 1.0) {
            	MipavUtil.displayError("alpha must be <= 1.0");
            	textAlpha.requestFocus();
            	textAlpha.selectAll();
            	return false;	
            }
        }
        
        if (routine == CORNER_DETECTOR) {
        	tmpStr = textThreshold.getText();
        	gradientDirectionThreshold = Double.parseDouble(tmpStr);
        	
        	if (gradientDirectionThreshold <= 0.0) {
        		MipavUtil.displayError("gradientDirectionThreshold must be greater than 0.0");
        		textThreshold.requestFocus();
        		textThreshold.selectAll();
        		return false;
        	}
        }

        if (mask3.isSelected()) {
        	blockSide = 3;
        }
        else if (mask5.isSelected()) {
        	blockSide = 5;
        }
        else if (mask7.isSelected()) {
        	blockSide = 7;
        }
        else if (mask9.isSelected()) {
        	blockSide = 9;
        }
        else if (mask11.isSelected()) {
        	blockSide = 11;
        }
        else {
        	blockSide = 13;
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
                return new String("Algorithms");
            }

            public String getDescription() {
                return new String("Facet model routines");
            }

            public String getDescriptionLong() {
                return new String("Facet model routines.");
            }

            public String getShortLabel() {
                return new String("FacetModel");
            }

            public String getLabel() {
                return new String("Facet Model");
            }

            public String getName() {
                return new String("Facet Model");
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
            table.put(new ParameterBoolean(AlgorithmParameters.DO_OUTPUT_NEW_IMAGE, true));
            table.put(new ParameterInt("rout", FACET_BASED_PEAK_NOISE_REMOVAL));
            table.put(new ParameterInt("blockSide", 3));
            table.put(new ParameterDouble("alph", 0.01));
            table.put(new ParameterDouble("gradient_threshold", 20.0));
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
     * Returns whether the action has successfully completed its execution.
     * 
     * @return True, if the action is complete. False, if the action failed or is still running.
     */
    public boolean isActionComplete() {
        return isComplete();
    }

}
