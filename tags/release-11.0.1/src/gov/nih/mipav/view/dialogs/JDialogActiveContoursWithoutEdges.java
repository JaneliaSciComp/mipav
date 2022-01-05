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

 */
public class JDialogActiveContoursWithoutEdges extends JDialogScriptableBase implements AlgorithmInterface, ItemListener, ScriptableActionInterface {

    //~ Static fields/initializers -------------------------------------------------------------------------------------

    /** Use serialVersionUID for interoperability. */
    //private static final long serialVersionUID;
	
	// General Chan-Vese method
	private static final int chan = 1;
	
	// Chan-Vese method for vector (RGB or multispectral) image
	private static final int vector = 2;
	
	// Chan-Vese method for twophase (2 phases applied here)
	// Two level set functions result in 4 segments
	private static final int twophase = 3;
	
	private int method = chan;

	// User supplied VOI
	private static final int user = 0;

	// Create a small circular mask
    private static final int small = 1;

    // Create a medium circular mask
 	private static final int medium = 2;

    // Create a large circular mask
 	private static final int large = 3;
 	
    // Create a mask with holes around
 	private static final int holes = 4;
 	
 	// Create a two layer mask with one layer small circular mask
 	// and the other layer with holes (only works for method twoPhase)
 	private static final int holes_small = 5;

    /** DOCUMENT ME! */
    public static final int WINDOW = 1;

    /** DOCUMENT ME! */
    public static final int GAUSSIAN = 2;

    /** DOCUMENT ME! */
    public static final int BUTTERWORTH = 3;

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** DOCUMENT ME! */
    private JRadioButton mediumCircularButton;

    /** DOCUMENT ME! */
    private JRadioButton largeCircularButton;
    
    private JRadioButton holesButton;
    
    private JRadioButton holesAndSmallCircularButton;

    /** DOCUMENT ME! */
    private JRadioButton twoPhaseButton;

    /** DOCUMENT ME! */
    private ButtonGroup methodGroup;

    /** DOCUMENT ME! */
    private JPanel methodPanel;

    /** DOCUMENT ME! */
    private JPanel parameterPanel;

    /** DOCUMENT ME! */
    private JPanel maskPanel;

    /** DOCUMENT ME! */
    private int maskType;

    /** DOCUMENT ME! */
    private ButtonGroup maskGroup;

    /** DOCUMENT ME! */
    private AlgorithmActiveContoursWithoutEdges activeContoursAlgo;

    /** DOCUMENT ME! */
    private JRadioButton vectorButton;

    /** DOCUMENT ME! */
    private JRadioButton smallCircularButton;

    /** DOCUMENT ME! */
    private ModelImage image; // source image

    /** DOCUMENT ME! */
    private JRadioButton userVOIButton;

    /** DOCUMENT ME! */
    private JPanel mainPanel;
    
    /** DOCUMENT ME! */
    private ModelImage resultImage = null; // result image

    /** DOCUMENT ME! */
    private String[] titles;

    /** DOCUMENT ME! */
    private JRadioButton channelButton;
    
    private JTextField textIterations;
    
    private int iterations = 400;
    
    private JTextField textMu;
    
    private double mu = 0.2;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Empty constructor needed for dynamic instantiation (used during scripting).
     */
    public JDialogActiveContoursWithoutEdges() { }

    /**
     * Used primarily for the script to store variables and run the algorithm. No actual dialog will appear but the set
     * up info and result image will be stored here.
     *
     * @param  im  Source image.
     */
    public JDialogActiveContoursWithoutEdges(ModelImage im) {
        super();
        image = im;
        parentFrame = image.getParentFrame();
    }

    // or if the source image is to be replaced
    /**
     * Creates a new JDialogActiveContoursWithoutEdges object.
     *
     * @param  theParentFrame  Parent frame.
     * @param  im              Source image.
     */
    public JDialogActiveContoursWithoutEdges(Frame theParentFrame, ModelImage im) {
        super(theParentFrame, false);
        image = im;
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
        } else if (source == channelButton) {
        	smallCircularButton.setEnabled(true);
        	mediumCircularButton.setEnabled(true);
        	largeCircularButton.setEnabled(true);
            holesAndSmallCircularButton.setEnabled(false);
        } else if (source == vectorButton) {
        	smallCircularButton.setEnabled(true);
        	mediumCircularButton.setEnabled(true);
        	largeCircularButton.setEnabled(true);
            holesAndSmallCircularButton.setEnabled(false);
        } else if (source == twoPhaseButton) {
        	smallCircularButton.setEnabled(false);
        	mediumCircularButton.setEnabled(false);
        	largeCircularButton.setEnabled(false);
            holesAndSmallCircularButton.setEnabled(true);
        } else if (command.equals("Cancel")) {
            dispose();
        } else if (source == helpButton) {
             //MipavUtil.showWebHelp("");
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

        if (algorithm instanceof AlgorithmActiveContoursWithoutEdges) {

            if ((algorithm.isCompleted() == true) && (resultImage != null)) {

            	updateFileTypeInfo(image, resultImage, ModelStorageBase.BYTE);

                // resultImage is the same or smaller than image.
                // The algorithm has completed and produced a new image to be displayed.
                try {

                    // resultImage.setImageName("Frequency Filtered image");
                    new ViewJFrameImage(resultImage, null, new Dimension(610, 200));
                } catch (OutOfMemoryError error) {
                    System.gc();
                    MipavUtil.displayError("Out of memory: unable to open new frame");
                }
            } else if (resultImage == null) {

                
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

        if (activeContoursAlgo != null) {
            activeContoursAlgo.finalize();
            activeContoursAlgo = null;
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
     * Accessor that sets the mask type
     *
     * @param  maskType  Value to set the mask type to
     */
    public void setMaskType(int maskType) {
        this.maskType = maskType;
    }

    /**
     * Accessor that sets the iterations variable.
     *
     * @param  iterations  Value to set iterations to.
     */
    public void setIterations(int iterations) {
        this.iterations = iterations;
    }

    /**
     * Accessor that sets the length parameter mu.
     *
     * @param  mu  Value to set mu to.
     */
    public void setMu(double mu) {
        this.mu = mu;
    }

    /**
     * Accessor that sets the method (chan, vector, twophase).
     *
     * @param  method  Value to set the method to.
     */
    public void setMethod(int method) {
        this.method = method;
    }

    /**
     * Once all the necessary variables are set, call the Frequency Filter algorithm based on what type of image this is
     * and whether or not there is a separate destination image.
     */
    protected void callAlgorithm() {
       

            try {
                String name = makeImageName(image.getImageName(), "_activeContours");
                resultImage = new ModelImage(ModelStorageBase.BYTE, image.getExtents(), name);

               
                // Make algorithm
                activeContoursAlgo = new AlgorithmActiveContoursWithoutEdges(resultImage, image, maskType, iterations, mu, method);

                // This is very important. Adding this object as a listener allows the algorithm to
                // notify this object when it has completed or failed. See algorithm performed event.
                // This is made possible by implementing AlgorithmedPerformed interface
                activeContoursAlgo.addListener(this);
                createProgressBar(image.getImageName(), activeContoursAlgo);

                // Hide dialog since the algorithm is about to run
                setVisible(false);

                if (isRunInSeparateThread()) {

                    // Start the thread as a low priority because we wish to still have user interface work fast.
                    if (activeContoursAlgo.startMethod(Thread.MIN_PRIORITY) == false) {
                        MipavUtil.displayError("A thread is already running on this object");
                    }
                } else {

                    activeContoursAlgo.run();
                }
            } catch (OutOfMemoryError x) {
                MipavUtil.displayError("Dialog Active Contours Without Edges: unable to allocate enough memory");

                return;
            }
        
    }

    /**
     * Store the result image in the script runner's image table now that the action execution is finished.
     */
    protected void doPostAlgorithmActions() {

        if (method == twophase) {
            AlgorithmParameters.storeImageInRunner(getResultImage());
        }
    }

    /**
     * {@inheritDoc}
     */
    protected void setGUIFromParams() {
        image = scriptParameters.retrieveInputImage();
        parentFrame = image.getParentFrame();
        setMethod(scriptParameters.getParams().getInt("method"));
        setMaskType(scriptParameters.getParams().getInt("mask_type"));
        setIterations(scriptParameters.getParams().getInt("iterations"));
        setMu(scriptParameters.getParams().getDouble("mu"));
    }

    /**
     * {@inheritDoc}
     */
    protected void storeParamsFromGUI() throws ParserException {
        scriptParameters.storeInputImage(image);
        scriptParameters.getParams().put(ParameterFactory.newParameter("method", method));
        scriptParameters.getParams().put(ParameterFactory.newParameter("mask_type", maskType));
        scriptParameters.getParams().put(ParameterFactory.newParameter("iterations", iterations));
        scriptParameters.getParams().put(ParameterFactory.newParameter("mu", mu));
    }

    /**
     * Sets up the GUI (panels, buttons, etc) and displays it on the screen.
     */
    private void init() {
        setTitle("Active Contours Without Edges");
        methodPanel = new JPanel(new GridBagLayout());
        methodPanel.setBorder(buildTitledBorder("Methods"));

        methodGroup = new ButtonGroup();
        channelButton = new JRadioButton("Single channel", true);
        channelButton.setFont(serif12);
        channelButton.setForeground(Color.black);
        channelButton.addActionListener(this);
        methodGroup.add(channelButton);

        vectorButton = new JRadioButton("Vector", false);
        vectorButton.setFont(serif12);
        vectorButton.setForeground(Color.black);
        vectorButton.addActionListener(this);
        methodGroup.add(vectorButton);

        if (image.isColorImage()) {
            vectorButton.setEnabled(true);
        } else {
            vectorButton.setEnabled(false);
        }

        twoPhaseButton = new JRadioButton("Two phase", false);
        twoPhaseButton.setFont(serif12);
        twoPhaseButton.setForeground(Color.black);
        twoPhaseButton.addActionListener(this);
        methodGroup.add(twoPhaseButton);

        GridBagConstraints gbc = new GridBagConstraints();
        gbc.gridwidth = 3;
        gbc.anchor = GridBagConstraints.WEST;
        gbc.weightx = 1;

        gbc.gridx = 0;
        gbc.gridy = 0;
        methodPanel.add(channelButton, gbc);
        gbc.gridy = 1;
        methodPanel.add(vectorButton, gbc);
        gbc.gridy = 2;
        methodPanel.add(twoPhaseButton, gbc);

        parameterPanel = new JPanel(new GridBagLayout());
        parameterPanel.setForeground(Color.black);
        
        parameterPanel.setBorder(buildTitledBorder("Parameters"));
        JLabel labelIterations = new JLabel("Iterations");
        labelIterations.setForeground(Color.black);
        labelIterations.setFont(serif12);
        gbc.gridwidth = 1;
        gbc.gridx = 0;
        gbc.gridy = 1;
        parameterPanel.add(labelIterations, gbc);
        
        textIterations = new JTextField(10);
        textIterations.setText("400");
        textIterations.setForeground(Color.black);
        textIterations.setFont(serif12);
        gbc.gridx = 1;
        parameterPanel.add(textIterations, gbc);
        
        JLabel labelMu = new JLabel("Length parameter mu");
        labelMu.setForeground(Color.black);
        labelMu.setFont(serif12);
        gbc.gridx = 0;
        gbc.gridy = 2;
        parameterPanel.add(labelMu, gbc);
        
        textMu = new JTextField(10);
        textMu.setText("0.2");
        textMu.setForeground(Color.black);
        textMu.setFont(serif12);
        gbc.gridx = 1;
        parameterPanel.add(textMu, gbc);

        maskPanel = new JPanel(new GridBagLayout());
        maskPanel.setBorder(buildTitledBorder("Mask"));

        maskGroup = new ButtonGroup();

        userVOIButton = new JRadioButton("User supplied VOI", true);
        userVOIButton.setFont(serif12);
        userVOIButton.setForeground(Color.black);
        maskGroup.add(userVOIButton);

        smallCircularButton = new JRadioButton("Small circular mask", false);
        smallCircularButton.setFont(serif12);
        smallCircularButton.setForeground(Color.black);
        maskGroup.add(smallCircularButton);

        mediumCircularButton = new JRadioButton("Medium circular mask", false);
        mediumCircularButton.setFont(serif12);
        mediumCircularButton.setForeground(Color.black);
        maskGroup.add(mediumCircularButton);

        largeCircularButton = new JRadioButton("Large circular mask", false);
        largeCircularButton.setForeground(Color.black);
        largeCircularButton.setFont(serif12);
        maskGroup.add(largeCircularButton);
        
        holesButton = new JRadioButton("Mask with holes around", false);
        holesButton.setForeground(Color.black);
        holesButton.setFont(serif12);
        maskGroup.add(holesButton);
        holesButton.setEnabled(true);
        
        holesAndSmallCircularButton = new JRadioButton("2 layers - holes and small circle", false);
        holesAndSmallCircularButton.setForeground(Color.black);
        holesAndSmallCircularButton.setFont(serif12);
        maskGroup.add(holesAndSmallCircularButton);
        holesAndSmallCircularButton.setEnabled(false);

        gbc.gridx = 0;
        gbc.gridy = 0;
        gbc.gridwidth = 1;
        gbc.weightx = 0;
        gbc.fill = GridBagConstraints.NONE;
        maskPanel.add(userVOIButton, gbc);
        gbc.gridy = 1;
        maskPanel.add(smallCircularButton, gbc);
        gbc.gridy = 2;
        maskPanel.add(mediumCircularButton, gbc);
        gbc.gridy = 3;
        maskPanel.add(largeCircularButton, gbc);
        gbc.gridy = 4;
        maskPanel.add(holesButton, gbc);
        gbc.gridy = 5;
        maskPanel.add(holesAndSmallCircularButton, gbc);

        mainPanel = new JPanel(new GridBagLayout());
        gbc.gridx = 0;
        gbc.gridy = 0;
        gbc.fill = GridBagConstraints.HORIZONTAL;
        mainPanel.add(methodPanel, gbc);
        gbc.gridy = 1;
        mainPanel.add(maskPanel, gbc);
        gbc.gridy = 2;
        mainPanel.add(parameterPanel, gbc);

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

        tmpStr = textIterations.getText();
        iterations = Integer.parseInt(tmpStr);

        if (iterations < 1) {
            MipavUtil.displayError("Iterations must be at least 1");
            textIterations.requestFocus();
            textIterations.selectAll();
            return false;
        } 
        
        tmpStr = textMu.getText();
        if (testParameter(tmpStr, 0.0, 1.0)) {
            mu = Double.valueOf(tmpStr).doubleValue();
        } else {
            MipavUtil.displayError("mu must be between 0.0 and 1.0");
            textMu.requestFocus();
            textMu.selectAll();

            return false;
        }

        if (channelButton.isSelected()) {
            method = chan;
        } else if (vectorButton.isSelected()) {
            method = vector;
        } else { // twoPhaseButton.isSelected()
            method = twophase;
        } // end of else twoPhaseButton.isSelected()

        if (userVOIButton.isSelected()) {
            maskType = user;
        } else if (smallCircularButton.isSelected()) {
            maskType = small;
        } else if (mediumCircularButton.isSelected()) {
            maskType = medium;
        } else if (largeCircularButton.isSelected()) {
            maskType = large;
        } else if (holesButton.isSelected()) {
        	maskType = holes;
        } else {
        	maskType = holes_small;
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
                return new String("Active contours without edges");
            }

            public String getDescriptionLong() {
                return new String("Active contours without edges.");
            }

            public String getShortLabel() {
                return new String("ActiveContoursWithoutEdges");
            }

            public String getLabel() {
                return new String("Active contours without edges");
            }

            public String getName() {
                return new String("Active contours without edges");
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
            table.put(new ParameterInt("method", chan));
            table.put(new ParameterInt("mask_type", user));
            table.put(new ParameterInt("iterations", 400));
            table.put(new ParameterDouble("mu", 0.2));
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
