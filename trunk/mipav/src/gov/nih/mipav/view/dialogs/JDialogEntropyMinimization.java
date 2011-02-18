package gov.nih.mipav.view.dialogs;


import gov.nih.mipav.model.algorithms.*;
import gov.nih.mipav.model.scripting.ParserException;
import gov.nih.mipav.model.scripting.parameters.ParameterBoolean;
import gov.nih.mipav.model.scripting.parameters.ParameterExternalImage;
import gov.nih.mipav.model.scripting.parameters.ParameterFactory;
import gov.nih.mipav.model.scripting.parameters.ParameterFloat;
import gov.nih.mipav.model.scripting.parameters.ParameterImage;
import gov.nih.mipav.model.scripting.parameters.ParameterInt;
import gov.nih.mipav.model.scripting.parameters.ParameterTable;
import gov.nih.mipav.model.structures.*;

import gov.nih.mipav.view.*;

import java.awt.*;
import java.awt.event.*;

import java.util.*;

import javax.swing.*;


/**
 * DOCUMENT ME!
 */
public class JDialogEntropyMinimization extends JDialogScriptableBase 
	implements AlgorithmInterface, ActionDiscovery{

    //~ Static fields/initializers -------------------------------------------------------------------------------------

    /** Use serialVersionUID for interoperability. */
    private static final long serialVersionUID = -417774372547972740L;

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** DOCUMENT ME! */
    private int displayLoc; // Flag indicating if a new image is to be generated

    /** DOCUMENT ME! */
    private AlgorithmEntropyMinimization emAlgo;

    /** DOCUMENT ME! */
    private ModelImage image; // source image

    /** DOCUMENT ME! */
    private JRadioButton m2Button;

    /** DOCUMENT ME! */
    private JRadioButton m3Button;

    /** DOCUMENT ME! */
    private JRadioButton ma2Button;

    /** DOCUMENT ME! */
    private JRadioButton newImage;

    /** DOCUMENT ME! */
    private ButtonGroup noiseGroup;

    /** DOCUMENT ME! */
    private int noiseType = AlgorithmEntropyMinimization.NOISE_MA2;

    /** DOCUMENT ME! */
    private JRadioButton replaceImage;

    /** DOCUMENT ME! */
    private ModelImage resultImage = null; // result image

    /** DOCUMENT ME! */
    private boolean subsample = true;

    /** DOCUMENT ME! */
    private JCheckBox subsampleCheckbox;

    /** DOCUMENT ME! */
    private JCheckBox thresholdCheckbox;

    /** DOCUMENT ME! */
    private JLabel thresholdLabel;

    /** DOCUMENT ME! */
    private float thresholdLevel;

    /** DOCUMENT ME! */
    private boolean thresholdSelected = false;

    /** DOCUMENT ME! */
    private JTextField thresholdText;

    /** DOCUMENT ME! */
    private String[] titles;

    /** DOCUMENT ME! */
    private ViewUserInterface userInterface;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Empty constructor needed for dynamic instantiation (used during scripting).
     */
    public JDialogEntropyMinimization() { }

    // or if the source image is to be replaced

    /**
     * Creates new dialog for entering parameters for entropy minimization.
     *
     * @param  theParentFrame  Parent frame
     * @param  im              Source image
     */
    public JDialogEntropyMinimization(Frame theParentFrame, ModelImage im) {
        super(theParentFrame, false);
        image = im;
        userInterface =  ViewUserInterface.getReference();
        init();
    }

   

    //~ Methods --------------------------------------------------------------------------------------------------------
    
    /**
     * {@inheritDoc}
     */
    protected void storeParamsFromGUI() throws ParserException {
        scriptParameters.storeInputImage(image);
        scriptParameters.storeOutputImageParams(resultImage, (displayLoc == NEW));
        
        scriptParameters.getParams().put(ParameterFactory.newParameter("do_threshold", thresholdSelected));
        scriptParameters.getParams().put(ParameterFactory.newParameter("threshold", thresholdLevel));
        scriptParameters.getParams().put(ParameterFactory.newParameter("do_subsample", subsample));
        scriptParameters.getParams().put(ParameterFactory.newParameter("noise_type", noiseType));
    }

    /**
     * {@inheritDoc}
     */
    protected void setGUIFromParams() {
        image = scriptParameters.retrieveInputImage();
        userInterface = ViewUserInterface.getReference();
        parentFrame = image.getParentFrame();
        
        if (scriptParameters.doOutputNewImage()) {
            setDisplayLocNew();
        } else {
            setDisplayLocReplace();
        }
        
        thresholdSelected = scriptParameters.getParams().getBoolean("do_threshold");
        thresholdLevel = scriptParameters.getParams().getFloat("threshold");
        subsample = scriptParameters.getParams().getBoolean("do_subsample");
        noiseType = scriptParameters.getParams().getInt("noise_type");
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
     * actionPerformed - Closes dialog box when the OK button is pressed and calls the algorithm.
     *
     * @param  event  event that triggers function
     */
    public void actionPerformed(ActionEvent event) {
        String command = event.getActionCommand();
        Object source = event.getSource();

        if (command.equals("OK")) {

            if (setVariables()) {
                callAlgorithm();
            }
        } else if (command.equals("Cancel")) {
            dispose();
        } else if (source == thresholdCheckbox) {

            if (thresholdCheckbox.isSelected()) {
                thresholdLabel.setEnabled(true);
                thresholdText.setEnabled(true);
            } else {
                thresholdLabel.setEnabled(false);
                thresholdText.setEnabled(false);
            }
        } // else if (source == thresholdCheckbox)
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
        if (algorithm instanceof AlgorithmEntropyMinimization) {
            image.clearMask();

            if ((emAlgo.isCompleted() == true) && (resultImage != null)) {
                // The algorithm has completed and produced a new image to be displayed.

                updateFileInfo(image, resultImage);

                try {

                    // resultImage.setImageName("Unsharp mask");
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
        // Update frame
        // ((ViewJFrameBase)parentFrame).updateImages(true);

        if (algorithm.isCompleted()) {
            insertScriptLine();
        }

        emAlgo.finalize();
        emAlgo = null;
        dispose();
    }

    /**
     * Accessor that returns the image.
     *
     * @return  the result image
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
     * Accessor that sets noiseType variable.
     *
     * @param  noiseType  int
     */
    public void setNoiseType(int noiseType) {
        this.noiseType = noiseType;
    }

    /**
     * Accessor that sets the subsample variable.
     *
     * @param  subsample  boolean
     */
    public void setSubsample(boolean subsample) {
        this.subsample = subsample;
    }

    /**
     * Accessor that sets the thresholdLevel variable.
     *
     * @param  thresholdLevel  float
     */
    public void setThresholdLevel(float thresholdLevel) {
        this.thresholdLevel = thresholdLevel;
    }

    /**
     * Accessor that sets the thresholdSelected variable.
     *
     * @param  thresholdSelected  boolean
     */
    public void setThresholdSelected(boolean thresholdSelected) {
        this.thresholdSelected = thresholdSelected;
    }

    /**
     * Once all the necessary variables are set, call the Entropy Minimization algorithm based on what type of image
     * this is and whether or not there is a separate destination image.
     */
    protected void callAlgorithm() {
        String name = makeImageName(image.getImageName(), "_entropyMin");

        if (displayLoc == NEW) {

            try {

                // Make result image of float type
                // resultImage = new ModelImage(ModelImage.FLOAT, destExtents, name, userInterface);
                resultImage = (ModelImage) image.clone();
                resultImage.setImageName(name);
                resultImage.resetVOIs();

                /*if ((resultImage.getFileInfo()[0]).getFileFormat() == FileUtility.DICOM){
                 *  ((FileInfoDicom)(resultImage.getFileInfo(0))).setSecondaryCaptureTags();
                 *}
                 */
                
                // Make algorithm
                emAlgo = new AlgorithmEntropyMinimization(resultImage, image, thresholdSelected, thresholdLevel,
                                                          subsample, noiseType);

                // This is very important. Adding this object as a listener allows the algorithm to
                // notify this object when it has completed of failed. See algorithm performed event.
                // This is made possible by implementing AlgorithmedPerformed interface
                emAlgo.addListener(this);

                createProgressBar(image.getImageName(), emAlgo);
                
                // Hide dialog
                setVisible(false);

                if (isRunInSeparateThread()) {

                    // Start the thread as a low priority because we wish to still have user interface work fast.
                    if (emAlgo.startMethod(Thread.MIN_PRIORITY) == false) {
                        MipavUtil.displayError("A thread is already running on this object");
                    }
                } else {
                    emAlgo.run();
                }
            } catch (OutOfMemoryError x) {
                MipavUtil.displayError("Dialog Entropy Minimization: unable to allocate enough memory");

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
                emAlgo = new AlgorithmEntropyMinimization(image, thresholdSelected, thresholdLevel, subsample,
                                                          noiseType);

                // This is very important. Adding this object as a listener allows the algorithm to
                // notify this object when it has completed of failed. See algorithm performed event.
                // This is made possible by implementing AlgorithmedPerformed interface
                emAlgo.addListener(this);

                createProgressBar(image.getImageName(), emAlgo);
                
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

                    // Start the thread as a low priority because we wish to still have user interface.
                    if (emAlgo.startMethod(Thread.MIN_PRIORITY) == false) {
                        MipavUtil.displayError("A thread is already running on this object");
                    }
                } else {
                    emAlgo.run();
                }
            } catch (OutOfMemoryError x) {
                MipavUtil.displayError("Dialog Entropy Minimization: unable to allocate enough memory");

                return;
            }
        }

    }

    /**
     * Sets up the GUI (panels, buttons, etc) and displays it on the screen.
     */
    private void init() {
        setTitle("Entropy Minimization");

        GridBagConstraints gbc = new GridBagConstraints();
        gbc.weightx = 1;

        int yPos = 0;
        gbc.anchor = GridBagConstraints.WEST;
        gbc.fill = GridBagConstraints.HORIZONTAL;

        JPanel paramPanel = new JPanel(new GridBagLayout());
        paramPanel.setBorder(buildTitledBorder("Input parameters"));

        if (!image.isColorImage()) {
            thresholdCheckbox = new JCheckBox("Threshold to exclude periphery pixels");
            thresholdCheckbox.setFont(serif12);
            thresholdCheckbox.setForeground(Color.black);
            thresholdCheckbox.setEnabled(true);
            thresholdCheckbox.setSelected(false);
            thresholdCheckbox.addActionListener(this);
            gbc.gridx = 0;
            gbc.gridy = yPos++;
            gbc.gridwidth = 3;
            paramPanel.add(thresholdCheckbox, gbc);

            thresholdLabel = new JLabel("Threshold level");
            thresholdLabel.setForeground(Color.black);
            thresholdLabel.setFont(serif12);
            thresholdLabel.setEnabled(false);
            gbc.gridy = yPos++;
            gbc.gridwidth = 1;
            paramPanel.add(Box.createHorizontalStrut(5), gbc);
            gbc.gridx = 1;
            paramPanel.add(thresholdLabel, gbc);

            image.calcMinMax();
            thresholdLevel = (float) (image.getMin() + 1.0);
            thresholdText = new JTextField(10);
            thresholdText.setText(String.valueOf(thresholdLevel));
            thresholdText.setFont(serif12);
            thresholdText.setForeground(Color.black);
            thresholdText.setEnabled(false);
            gbc.gridx = 2;
            paramPanel.add(thresholdText, gbc);
        } // if (!image.isColorImage())

        subsampleCheckbox = new JCheckBox("Subsample image for speed");
        subsampleCheckbox.setFont(serif12);
        subsampleCheckbox.setForeground(Color.black);
        subsampleCheckbox.setEnabled(true);
        subsampleCheckbox.setSelected(true);
        gbc.gridx = 0;
        gbc.gridy = yPos++;
        gbc.gridwidth = 3;
        paramPanel.add(subsampleCheckbox, gbc);

        noiseGroup = new ButtonGroup();
        ma2Button = new JRadioButton("Multiplicative and additive quadratic noise", true);
        ma2Button.setFont(serif12);
        noiseGroup.add(ma2Button);
        gbc.gridy = yPos++;
        paramPanel.add(ma2Button, gbc);

        m2Button = new JRadioButton("Multiplicative quadratic noise", false);
        m2Button.setFont(serif12);
        noiseGroup.add(m2Button);
        gbc.gridy = yPos++;
        paramPanel.add(m2Button, gbc);

        m3Button = new JRadioButton("Multiplicative cubic noise", false);
        m3Button.setFont(serif12);
        noiseGroup.add(m3Button);
        gbc.gridy = yPos++;
        paramPanel.add(m3Button, gbc);

        JPanel destinationPanel = new JPanel(new GridLayout(2, 1));
        destinationPanel.setForeground(Color.black);
        destinationPanel.setBorder(buildTitledBorder("Destination"));

        ButtonGroup destinationGroup = new ButtonGroup();
        newImage = new JRadioButton("New image", true);
        newImage.setFont(serif12);
        destinationGroup.add(newImage);
        destinationPanel.add(newImage);

        replaceImage = new JRadioButton("Replace image", false);
        replaceImage.setFont(serif12);
        destinationGroup.add(replaceImage);
        destinationPanel.add(replaceImage);

        if (image.getLockStatus() == ModelStorageBase.UNLOCKED) {
            replaceImage.setEnabled(true);
        } else {
            replaceImage.setEnabled(false);
        }

        JPanel mainPanel = new JPanel(new GridBagLayout());
        gbc.gridx = 0;
        gbc.gridy = 0;
        gbc.gridwidth = 1;
        mainPanel.add(paramPanel, gbc);
        gbc.gridy = 1;
        mainPanel.add(destinationPanel, gbc);
        mainPanel.setBorder(BorderFactory.createEmptyBorder(5, 5, 5, 5));

        JPanel buttonPanel = new JPanel();
        buildOKButton();
        buttonPanel.add(OKButton);
        buildCancelButton();
        buttonPanel.add(cancelButton);

        getContentPane().add(mainPanel);
        getContentPane().add(buttonPanel, BorderLayout.SOUTH);

        pack();
        setResizable(false);
        setVisible(true);
    }

    /**
     * Use the GUI results to set up the variables needed to run the algorithm.
     *
     * @return  <code>true</code> if parameters set successfully, <code>false</code> otherwise.
     */
    private boolean setVariables() {
        String tmpStr;

        System.gc();

        if (thresholdCheckbox == null) {
            thresholdSelected = false;
        } else {
            thresholdSelected = thresholdCheckbox.isSelected();
        }

        if (thresholdSelected) {
            tmpStr = thresholdText.getText();

            if (testParameter(tmpStr, image.getMin(), image.getMax())) {
                thresholdLevel = Float.valueOf(tmpStr).floatValue();
            } else {
                MipavUtil.displayError("Threshold level must be between " + image.getMin() + " and " + image.getMax());
                thresholdText.requestFocus();
                thresholdText.selectAll();

                return false;
            }
        } // if (thresholdSelected)

        subsample = subsampleCheckbox.isSelected();

        if (ma2Button.isSelected()) {
            noiseType = AlgorithmEntropyMinimization.NOISE_MA2;
        } else if (m2Button.isSelected()) {
            noiseType = AlgorithmEntropyMinimization.NOISE_M2;
        } else if (m3Button.isSelected()) {
            noiseType = AlgorithmEntropyMinimization.NOISE_M3;
        }

        if (replaceImage.isSelected()) {
            displayLoc = REPLACE;
        } else if (newImage.isSelected()) {
            displayLoc = NEW;
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
                return new String("Algorithms.Shading Correction");
            }

            public String getDescription() {
                return new String("Performs retrospective shading correction based on entropy minimization." +
                		"Note that in the noise_type field, " +
                		"1 == Multiplicative and additive quadratic noise," +
                		"2 == Multiplicative quadratic noise, and" +
                		"3 == Multiplicative cubic noise.");
            }

            public String getDescriptionLong() {
                return new String("Performs retrospective shading correction based on entropy minimization." +
                		"Note that in the noise_type field, " +
                		"1 == Multiplicative and additive quadratic noise," +
                		"2 == Multiplicative quadratic noise, and" +
                		"3 == Multiplicative cubic noise.");
            }

            public String getShortLabel() {
                return new String("EntropyMinimization");
            }

            public String getLabel() {
                return new String("Entropy Minimization");
            }

            public String getName() {
                return new String("Entropy Minimization");
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
            table.put(new ParameterBoolean("do_threshold", false));
            table.put(new ParameterFloat("threshold", 1.0f));
            table.put(new ParameterBoolean("do_subsample", true));
            
            //1 == Multiplicative and additive quadratic noise
            //2 == Multiplicative quadratic noise
            //3 == Multiplicative cubic noise
            table.put(new ParameterInt("noise_type", 1));
            table.put(new ParameterBoolean(AlgorithmParameters.DO_OUTPUT_NEW_IMAGE, true));
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
