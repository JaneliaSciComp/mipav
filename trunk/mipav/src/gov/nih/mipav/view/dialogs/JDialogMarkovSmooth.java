package gov.nih.mipav.view.dialogs;


import gov.nih.mipav.model.algorithms.*;
import gov.nih.mipav.model.algorithms.filters.*;
import gov.nih.mipav.model.file.FileInfoBase.Unit;
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
 * source image. The program does slice by slice Markov smoothing with Iterated Conditional Modes on black and white images.
 *Algorithms are executed in their own thread.
 *
 * @see  AlgorithmMrkovSmooth
 */
public class JDialogMarkovSmooth extends JDialogScriptableBase implements AlgorithmInterface, ActionDiscovery, ScriptableActionInterface {

    //~ Static fields/initializers -------------------------------------------------------------------------------------

    /** Use serialVersionUID for interoperability. */
    //private static final long serialVersionUID;

    //~ Instance fields ------------------------------------------------------------------------------------------------
	/** The known covariance of the Gaussian noise */
    private double covariance;
    
    /** The maximum contribution to the potential of the difference between two neighboring pixel values. */
    private double max_diff;
    
    /** The weighting attached to the component of the potential due to the difference between two
        neighboring pixel values. */
    private double weight_diff;
    
    /** The number of iterations to perform */
    private int iterations;

    /** DOCUMENT ME! */
    private ButtonGroup destinationGroup;

    /** DOCUMENT ME! */
    private JPanel destinationPanel;

    /** DOCUMENT ME! */
    private int displayLoc; // Flag indicating if a new image is to be generated

    // or if the source image is to be replaced

    /** DOCUMENT ME! */
    private ModelImage image; // source image

    /** DOCUMENT ME! */
    private JLabel labelMaxDiff;
    
    private JLabel labelWeightDiff;
    
    private JLabel labelIterations;

    /** DOCUMENT ME! */
    private JLabel labelCovariance;

    /** DOCUMENT ME! */
    private JRadioButton newImage;

    /** DOCUMENT ME! */
    private AlgorithmMarkovSmooth msAlgo;

    /** DOCUMENT ME! */
    private JPanel optionsPanel;

    /** DOCUMENT ME! */
    private JRadioButton replaceImage;

    /** DOCUMENT ME! */
    private ModelImage resultImage = null; // result image

    /**
     * discriminate between noise and the underlying image. Ideally, the value should be set greater than the noise
     * level and less than the contrast of the underlying image. Edges of contrast smaller than this threshold will tend
     * to be blurred whereas those of greater contrast will not be.
     */
    private JTextField textMaxDiff;
    
    private JTextField textWeightDiff;
    
    private JTextField textIterations;

    /** DOCUMENT ME! */
    private JTextField textCovariance;

    /** DOCUMENT ME! */
    private String[] titles;

    /** DOCUMENT ME! */
    private ViewUserInterface userInterface;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Empty constructor needed for dynamic instantiation (used during scripting).
     */
    public JDialogMarkovSmooth() { }

    /**
     * Creates a new JDialogMarkovSmooth object.
     *
     * @param  theParentFrame  Parent frame.
     * @param  im              Source image.
     */
    public JDialogMarkovSmooth(Frame theParentFrame, ModelImage im) {
        super(theParentFrame, false);
        userInterface = ViewUserInterface.getReference();
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
            MipavUtil.showWebHelp("Filters_(Spatial):_Markov_Smoooth#Applying_the_algorithm");
        } else { // else if (source == thresholdCheckbox)
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

        if (algorithm instanceof AlgorithmMarkovSmooth) {
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
            } else if (resultImage == null) {

                // These next lines set the titles in all frames where the source image is displayed to
                // image name so as to indicate that the image is now unlocked!
                // The image frames are enabled and then registed to the userinterface.
                Vector<ViewImageUpdateInterface> imageFrames = image.getImageFrameVector();

                for (int i = 0; i < imageFrames.size(); i++) {
                    ((Frame) (imageFrames.elementAt(i))).setTitle(titles[i]);
                    ((Frame) (imageFrames.elementAt(i))).setEnabled(true);

                    if (((Frame) (imageFrames.elementAt(i))) != parentFrame) {
                        userInterface.registerFrame((Frame) (imageFrames.elementAt(i)));
                    }
                }

                if (parentFrame != null) {
                    userInterface.registerFrame(parentFrame);
                }

                image.notifyImageDisplayListeners(null, true);
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
     * Accessor that sets the Gaussian noise covariance.
     *
     * @param  covariance
     */
    public void setCovariance(double covariance) {
        this.covariance = covariance;
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
     * Accessor that sets the maximum contribution to the potential
     * of the difference between two neighboring pixel values.
     *
     * @param  max_diff
     */
    public void setMaxDiff(double max_diff) {
        this.max_diff = max_diff;
    }
    
    /**
     * Accessor that sets the weighting attached to the component of the potential
     * due to the difference between two neighboring pixel values.
     *
     * @param  weight_diff
     */
    public void setWeightDiff(double weight_diff) {
        this.weight_diff = weight_diff;
    }
    
    /**
     * 
     * @param iterations
     */
    public void setIterations(int iterations) {
    	this.iterations = iterations;
    }

    /**
     * Once all the necessary variables are set, call the Markov Smooth algorithm based on whether or not there
     * is a separate destination image.
     */
    protected void callAlgorithm() {
        String name = makeImageName(image.getImageName(), "_MarkovSmooth");

            if (displayLoc == NEW) {

                try {

                    // Make result image of float type
                    // resultImage= new ModelImage(ModelImage.FLOAT, destExtents, name, userInterface);
                    resultImage = (ModelImage) image.clone();
                    resultImage.setImageName(name);

                    /* if ((resultImage.getFileInfo()[0]).getFileFormat() == FileUtility.DICOM){
                     *   ((FileInfoDicom)(resultImage.getFileInfo(0))).setSecondaryCaptureTags();
                     * }*/
                    // Make algorithm
                    msAlgo = new AlgorithmMarkovSmooth(resultImage, image, covariance, max_diff, weight_diff, iterations);

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
                    MipavUtil.displayError("Dialog MarkovSmooth: unable to allocate enough memory");

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
                	msAlgo = new AlgorithmMarkovSmooth(image, covariance, max_diff, weight_diff, iterations);

                    // This is very important. Adding this object as a listener allows the algorithm to
                    // notify this object when it has completed of failed. See algorithm performed event.
                    // This is made possible by implementing AlgorithmedPerformed interface
                    msAlgo.addListener(this);

                    createProgressBar(image.getImageName(), msAlgo);

                    // Hide the dialog since the algorithm is about to run.
                    setVisible(false);

                    // These next lines set the titles in all frames where the source image is displayed to
                    // "locked - " image name so as to indicate that the image is now read/write locked!
                    // The image frames are disabled and then unregisted from the userinterface until the
                    // algorithm has completed.
                    Vector<ViewImageUpdateInterface> imageFrames = image.getImageFrameVector();
                    titles = new String[imageFrames.size()];

                    for (int i = 0; i < imageFrames.size(); i++) {
                        titles[i] = ((Frame) (imageFrames.elementAt(i))).getTitle();
                        ((Frame) (imageFrames.elementAt(i))).setTitle("Locked: " + titles[i]);
                        ((Frame) (imageFrames.elementAt(i))).setEnabled(false);
                        userInterface.unregisterFrame((Frame) (imageFrames.elementAt(i)));
                    }

                    if (isRunInSeparateThread()) {

                        // Start the thread as a low priority because we wish to still have user interface work fast.
                        if (msAlgo.startMethod(Thread.MIN_PRIORITY) == false) {
                            MipavUtil.displayError("A thread is already running on this object");
                        }
                    } else {
                        msAlgo.run();
                    }

                } catch (OutOfMemoryError x) {
                    MipavUtil.displayError("Dialog Markov Smooth: unable to allocate enough memory");

                    return;
                }
            }
        

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

        setDefaults();

        if (scriptParameters.doOutputNewImage()) {
            setDisplayLocNew();
        } else {
            setDisplayLocReplace();
        }

        setCovariance(scriptParameters.getParams().getDouble("noise_covariance"));
        setMaxDiff(scriptParameters.getParams().getDouble("maximum_difference"));
        setWeightDiff(scriptParameters.getParams().getDouble("weight_difference"));
        setIterations(scriptParameters.getParams().getInt("number_iterations"));
    }

    /**
     * {@inheritDoc}
     */
    protected void storeParamsFromGUI() throws ParserException {
        scriptParameters.storeInputImage(image);
        scriptParameters.storeOutputImageParams(getResultImage(), (displayLoc == NEW));

        scriptParameters.getParams().put(ParameterFactory.newParameter("noise_covariance", covariance));
        scriptParameters.getParams().put(ParameterFactory.newParameter("maximum_difference", max_diff));
        scriptParameters.getParams().put(ParameterFactory.newParameter("weight_difference", weight_diff));
        scriptParameters.getParams().put(ParameterFactory.newParameter("number_iterations", iterations));
    }

    /**
     * Initializes the GUI by creating the components, placing them in the dialog, and displaying them.
     */
    private void init() {

        setForeground(Color.black);

        getContentPane().setLayout(new BorderLayout());
        setTitle("Markov Smoothing");

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

        labelCovariance = createLabel("Gaussian noise covariance");

        optionsPanel.add(labelCovariance, gbc);

        gbc.gridx = 1;
        gbc.gridy = 0;
        textCovariance = new JTextField();
        textCovariance.setFont(serif12);
        optionsPanel.add(textCovariance, gbc);

        gbc.gridx = 0;
        gbc.gridy = 1;
        labelMaxDiff = createLabel("max_diff");
        optionsPanel.add(labelMaxDiff, gbc);

        gbc.gridx = 1;
        gbc.gridy = 1;
        textMaxDiff = new JTextField();
        textMaxDiff.setText("200.0");
        textMaxDiff.setFont(serif12);
        optionsPanel.add(textMaxDiff, gbc);

        gbc.gridx = 0;
        gbc.gridy = 2;
        labelWeightDiff = createLabel("weight_diff");
        optionsPanel.add(labelWeightDiff, gbc);

        gbc.gridx = 1;
        gbc.gridy = 2;
        textWeightDiff = new JTextField();
        textWeightDiff.setText("0.02");
        textWeightDiff.setFont(serif12);
        optionsPanel.add(textWeightDiff, gbc);

        gbc.gridx = 0;
        gbc.gridy = 3;
        labelIterations = createLabel("Iterations");
        optionsPanel.add(labelIterations, gbc);

        gbc.gridx = 1;
        gbc.gridy = 3;
        textIterations = new JTextField();
        textIterations.setText("10");
        textIterations.setFont(serif12);
        optionsPanel.add(textIterations, gbc);

        gbc.gridx = 0;
        gbc.gridy = 0;
        mainPanel.add(optionsPanel, gbc);

        JPanel outputOptPanel = new JPanel(new GridLayout(1, 1));
        destinationPanel = new JPanel(new BorderLayout());
        destinationPanel.setForeground(Color.black);
        destinationPanel.setBorder(buildTitledBorder("Destination"));
        outputOptPanel.add(destinationPanel);

        destinationGroup = new ButtonGroup();
        newImage = new JRadioButton("New image", true);
        newImage.setFont(serif12);
        destinationGroup.add(newImage);
        destinationPanel.add(newImage, BorderLayout.NORTH);

        replaceImage = new JRadioButton("Replace image", false);
        replaceImage.setFont(serif12);
        destinationGroup.add(replaceImage);
        destinationPanel.add(replaceImage, BorderLayout.CENTER);

        // Only if the image is unlocked can it be replaced.
        if (image.getLockStatus() == ModelStorageBase.UNLOCKED) {
            replaceImage.setEnabled(true);
        } else {
            replaceImage.setEnabled(false);
        }

        gbc.gridx = 0;
        gbc.gridy = 1;
        mainPanel.add(outputOptPanel, gbc);

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

        max_diff = 200.0;
        weight_diff = 0.02;
        iterations = 10;
        this.setDisplayLocNew(); // default
    }

    /**
     * Use the GUI results to set up the variables needed to run the algorithm.
     *
     * @return  <code>true</code> if parameters set successfully, <code>false</code> otherwise.
     */
    private boolean setVariables() {
        String tmpStr;
        System.gc();

        if (replaceImage.isSelected()) {
            displayLoc = REPLACE;
        } else if (newImage.isSelected()) {
            displayLoc = NEW;
        }

        tmpStr = textCovariance.getText();

        if (testParameter(tmpStr, 0.0, Double.MAX_VALUE)) {
            covariance = Double.valueOf(tmpStr).doubleValue();
        } else {
            textCovariance.requestFocus();
            textCovariance.selectAll();

            return false;
        }

        tmpStr = textMaxDiff.getText();

        if (testParameter(tmpStr, 0.0, Double.MAX_VALUE)) {
            max_diff = Double.valueOf(tmpStr).doubleValue();
        } else {
            textMaxDiff.requestFocus();
            textMaxDiff.selectAll();

            return false;
        }
        
        tmpStr = textWeightDiff.getText();

        if (testParameter(tmpStr, 0.0, Double.MAX_VALUE)) {
            weight_diff = Double.valueOf(tmpStr).doubleValue();
        } else {
            textWeightDiff.requestFocus();
            textWeightDiff.selectAll();

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
                return new String("Algorithms.Filters (spatial)");
            }

            public String getDescription() {
                return new String("Markov Smoothing using Iterated Conditional Modes");
            }

            public String getDescriptionLong() {
                return new String("Markov Smoothing using Iterated Conditional Modes");
            }

            public String getShortLabel() {
                return new String("Markov Smoothing");
            }

            public String getLabel() {
                return new String("Markov Smoothing");
            }

            public String getName() {
                return new String("Markov Smoothing");
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
            table.put(new ParameterDouble("noise_covariance"));
            table.put(new ParameterDouble("maximum_difference"));
            table.put(new ParameterDouble("weight_difference"));
            table.put(new ParameterInt("number_iterations",10));
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
