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
 * Dialog to get user input, then call the algorithm.
 * The program does slice by slice Markov segmentation on black and white or color images.
 *Algorithms are executed in their own thread.
 *
 * @see  AlgorithmMarkovSegment
 */
public class JDialogMarkovSegment extends JDialogScriptableBase implements AlgorithmInterface, ActionDiscovery, ScriptableActionInterface {

    //~ Static fields/initializers -------------------------------------------------------------------------------------

    /** Use serialVersionUID for interoperability. */
    //private static final long serialVersionUID;

    //~ Instance fields ------------------------------------------------------------------------------------------------
    private int class_number = 3;
	
	private double potential = 0.5;
	
	private int iterations = 30;

    /** DOCUMENT ME! */
    private ModelImage image; // source image

    /** DOCUMENT ME! */
    private JLabel labelPotential;
    
    private JLabel labelIterations;

    /** DOCUMENT ME! */
    private JLabel labelClassNumber;

    /** DOCUMENT ME! */
    private AlgorithmMarkovSegment msAlgo;

    /** DOCUMENT ME! */
    private JPanel optionsPanel;

    /** DOCUMENT ME! */
    private ModelImage resultImage = null; // result image
    
    private JTextField textPotential;
    
    private JTextField textIterations;

    /** DOCUMENT ME! */
    private JTextField textClassNumber;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Empty constructor needed for dynamic instantiation (used during scripting).
     */
    public JDialogMarkovSegment() { }

    /**
     * Creates a new JDialogMarkovSegment object.
     *
     * @param  theParentFrame  Parent frame.
     * @param  im              Source image.
     */
    public JDialogMarkovSegment(Frame theParentFrame, ModelImage im) {
        super(theParentFrame, false);
        image = im;
        setDefaults();
        init();
    }

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * Closes dialog box when the OK button is pressed, sets variables and calls algorithm.
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
            MipavUtil.showWebHelp("Filters_(Spatial):_Markov_Segment#Applying_the_algorithm");
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

        if (algorithm instanceof AlgorithmMarkovSegment) {
            image.clearMask();

            if ((msAlgo.isCompleted() == true) && (resultImage != null)) {

                updateFileInfo(image, resultImage);
                resultImage.clearMask();

                // The algorithm has completed and produced a new image to be displayed.
                try {
                    new ViewJFrameImage(resultImage, null, new Dimension(610, 200));
                } catch (OutOfMemoryError error) {
                    MipavUtil.displayError("Out of memory: unable to open new frame");
                }
            
            } else if (resultImage != null) {

                // algorithm failed but result image still has garbage
                resultImage.disposeLocal(); // clean up memory
                resultImage = null;
            }
        }

        if (algorithm.isCompleted()) {
            insertScriptLine();
        }
     // save the completion status for later
        setComplete(algorithm.isCompleted());

        msAlgo.finalize();
        msAlgo = null;
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
     * Accessor that sets the class number.
     *
     * @param  class_number
     */
    public void setClassNumber(int class_number) {
        this.class_number = class_number;
    }
    /**
     * Accessor that sets the potential
     *
     * @param  potential
     */
    public void setPotential(double potential) {
        this.potential = potential;
    }
    
    /**
     * 
     * @param iterations
     */
    public void setIterations(int iterations) {
    	this.iterations = iterations;
    }

    /**
     * Once all the necessary variables are set, call the Markov Segment algorithm which will create
     * a new segmented image.
     */
    protected void callAlgorithm() {
        String name = makeImageName(image.getImageName(), "_MarkovSegment");

                try {

                    // Make result image of float BYTE
                    resultImage= new ModelImage(ModelImage.BYTE, image.getExtents(), name);

                    /* if ((resultImage.getFileInfo()[0]).getFileFormat() == FileUtility.DICOM){
                     *   ((FileInfoDicom)(resultImage.getFileInfo(0))).setSecondaryCaptureTags();
                     * }*/
                    // Make algorithm
                    msAlgo = new AlgorithmMarkovSegment(resultImage, image, class_number, potential, iterations);

                    // This is very important. Adding this object as a listener allows the algorithm to
                    // notify this object when it has completed of failed. See algorithm performed event.
                    // This is made possible by implementing AlgorithmedPerformed interface
                    msAlgo.addListener(this);

                    createProgressBar(image.getImageName(), msAlgo);

                    // Hide dialog
                    setVisible(false);

                    if (isRunInSeparateThread()) {

                        // Start the thread as a low priority because we wish to still have user interface work fast.
                        if (msAlgo.startMethod(Thread.MIN_PRIORITY) == false) {
                            MipavUtil.displayError("A thread is already running on this object");
                        }
                    } else {
                        msAlgo.run();
                    }

                } catch (OutOfMemoryError x) {
                    MipavUtil.displayError("Dialog MarkovSegment: unable to allocate enough memory");

                    if (resultImage != null) {
                        resultImage.disposeLocal(); // Clean up memory of result image
                        resultImage = null;
                    }

                    return;
                }
            

    }

    /**
     * Store the result image in the script runner's image table now that the action execution is finished.
     */
    protected void doPostAlgorithmActions() {
            AlgorithmParameters.storeImageInRunner(getResultImage());
    }

    /**
     * {@inheritDoc}
     */
    protected void setGUIFromParams() {
        image = scriptParameters.retrieveInputImage();
        parentFrame = image.getParentFrame();

        setDefaults();

        setClassNumber(scriptParameters.getParams().getInt("class_num"));
        setPotential(scriptParameters.getParams().getDouble("potent"));
        setIterations(scriptParameters.getParams().getInt("number_iterations"));
    }

    /**
     * {@inheritDoc}
     */
    protected void storeParamsFromGUI() throws ParserException {
        scriptParameters.storeInputImage(image);
        scriptParameters.storeOutputImageParams(getResultImage(), true);

        scriptParameters.getParams().put(ParameterFactory.newParameter("class_num", class_number));
        scriptParameters.getParams().put(ParameterFactory.newParameter("potent", potential));
        scriptParameters.getParams().put(ParameterFactory.newParameter("number_iterations", iterations));
    }

    /**
     * Initializes the GUI by creating the components, placing them in the dialog, and displaying them.
     */
    private void init() {

        setForeground(Color.black);

        getContentPane().setLayout(new BorderLayout());
        setTitle("Markov Segment");

        JPanel mainPanel;
        mainPanel = new JPanel();
        mainPanel.setBorder(BorderFactory.createEmptyBorder(3, 3, 3, 3));
        mainPanel.setLayout(new GridBagLayout());

        GridBagConstraints gbc = new GridBagConstraints();
        gbc.gridwidth = 1;
        gbc.gridheight = 1;
        gbc.anchor = GridBagConstraints.WEST;
        gbc.weightx = 1;
        gbc.insets = new Insets(3, 3, 3, 3);
        gbc.fill = GridBagConstraints.HORIZONTAL;
        gbc.gridx = 0;
        gbc.gridy = 0;

        optionsPanel = new JPanel(new GridBagLayout());
        optionsPanel.setForeground(Color.black);
        optionsPanel.setBorder(buildTitledBorder("Options"));
        getContentPane().add(optionsPanel);

        labelClassNumber = createLabel("Class number");

        optionsPanel.add(labelClassNumber, gbc);

        gbc.gridx = 1;
        gbc.gridy = 0;
        textClassNumber = new JTextField();
        textClassNumber.setText("3");
        textClassNumber.setFont(serif12);
        optionsPanel.add(textClassNumber, gbc);

        gbc.gridx = 0;
        gbc.gridy = 1;
        labelPotential = createLabel("Potential");
        optionsPanel.add(labelPotential, gbc);

        gbc.gridx = 1;
        gbc.gridy = 1;
        textPotential = new JTextField();
        textPotential.setText("0.5");
        textPotential.setFont(serif12);
        optionsPanel.add(textPotential, gbc);

        gbc.gridx = 0;
        gbc.gridy = 2;
        labelIterations = createLabel("Iterations");
        optionsPanel.add(labelIterations, gbc);

        gbc.gridx = 1;
        gbc.gridy = 2;
        textIterations = new JTextField();
        textIterations.setText("30");
        textIterations.setFont(serif12);
        optionsPanel.add(textIterations, gbc);

        gbc.gridx = 0;
        gbc.gridy = 0;
        mainPanel.add(optionsPanel, gbc);

        getContentPane().add(mainPanel, BorderLayout.CENTER);
        getContentPane().add(buildButtons(), BorderLayout.SOUTH);
        pack();
        setResizable(true);
        setVisible(true);

        System.gc();
    }

    /**
     * Set the default values for the parameters.
     */
    private void setDefaults() {

        class_number = 3;
    	potential = 0.5;
        iterations = 30;
    }

    /**
     * Use the GUI results to set up the variables needed to run the algorithm.
     *
     * @return  <code>true</code> if parameters set successfully, <code>false</code> otherwise.
     */
    private boolean setVariables() {
        String tmpStr;
        System.gc();

        tmpStr = textClassNumber.getText();

        if (testParameter(tmpStr, 0, 127)) {
            class_number = Integer.valueOf(tmpStr).intValue();
        } else {
            textClassNumber.requestFocus();
            textClassNumber.selectAll();

            return false;
        }

        tmpStr = textPotential.getText();

        if (testParameter(tmpStr, 0.0, Double.MAX_VALUE)) {
            potential = Double.valueOf(tmpStr).doubleValue();
        } else {
            textPotential.requestFocus();
            textPotential.selectAll();

            return false;
        }
        
        tmpStr = textIterations.getText();

        if (testParameter(tmpStr, 1, Integer.MAX_VALUE)) {
            iterations = Integer.valueOf(tmpStr).intValue();
        } else {
            textIterations.requestFocus();
            textIterations.selectAll();

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
                return new String("Algorithms");
            }

            public String getDescription() {
                return new String("Markov Segment");
            }

            public String getDescriptionLong() {
                return new String("Markov Segment");
            }

            public String getShortLabel() {
                return new String("Markov Segment");
            }

            public String getLabel() {
                return new String("Markov Segment");
            }

            public String getName() {
                return new String("Markov Segment");
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
            table.put(new ParameterInt("class_num", 3));
            table.put(new ParameterDouble("potent", 0.5));
            table.put(new ParameterInt("number_iterations",30));
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
