package gov.nih.mipav.view.dialogs;


import gov.nih.mipav.model.algorithms.*;
import gov.nih.mipav.model.algorithms.filters.*;
import gov.nih.mipav.model.algorithms.utilities.AlgorithmTiltCorrection;
import gov.nih.mipav.model.file.FileInfoBase.Unit;
import gov.nih.mipav.model.file.FileInfoDicom.*;
import gov.nih.mipav.model.file.FileInfoDicom;
import gov.nih.mipav.model.file.FileUtility;
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
 * source image. The program called performs tilt correction.
 *
 * @see  AlgorithmTiltCorrection
 */
public class JDialogTiltCorrection extends JDialogScriptableBase implements AlgorithmInterface, ActionDiscovery, ScriptableActionInterface {

    //~ Static fields/initializers -------------------------------------------------------------------------------------

    /** Use serialVersionUID for interoperability. */
    //private static final long serialVersionUID;

    //~ Instance fields ------------------------------------------------------------------------------------------------
    
    private boolean nonHelical = true;
    
    private double tiltAngle = 0.0;

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
    private JRadioButton newImage;

    /** DOCUMENT ME! */
    private AlgorithmTiltCorrection tiltAlgo;

    /** DOCUMENT ME! */
    private JPanel optionsPanel;

    /** DOCUMENT ME! */
    private JRadioButton replaceImage;

    /** DOCUMENT ME! */
    private ModelImage resultImage = null; // result image

    /** DOCUMENT ME! */
    private JTextField textAngle;
    
    private ButtonGroup typeGroup;
    
    private JRadioButton nonHelicalButton;
    
    private JRadioButton helicalButton;

    /** DOCUMENT ME! */
    private String[] titles;

    /** DOCUMENT ME! */
    private ViewUserInterface userInterface;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Empty constructor needed for dynamic instantiation (used during scripting).
     */
    public JDialogTiltCorrection() { }

    /**
     * Creates a new JDialogTiltCorrection object.
     *
     * @param  theParentFrame  Parent frame.
     * @param  im              Source image.
     */
    public JDialogTiltCorrection(Frame theParentFrame, ModelImage im) {
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

        if (algorithm instanceof AlgorithmTiltCorrection) {
            image.clearMask();

            if ((tiltAlgo.isCompleted() == true) && (resultImage != null)) {

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

        tiltAlgo.finalize();
        tiltAlgo = null;
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
    
    public void setNonHelical(boolean nonHelical) {
        this.nonHelical = nonHelical;
    }
    
    public void setTiltAngle(double tiltAngle) {
        this.tiltAngle = tiltAngle;
    }

    
    /**
     * Once all the necessary variables are set, call the tilt correction algorithm based on what type of image this is
     * and whether or not there is a separate destination image.
     */
    protected void callAlgorithm() {
        int i;
        String name = makeImageName(image.getImageName(), "_tiltCorection");
        for (i = 1; i <= 2; i++) {
            if (image.getFileInfo()[0].getUnitsOfMeasure()[i] != image.getFileInfo()[0].getUnitsOfMeasure()[0]) {
                image.make2UnitsOfMeasureIdentical(0, i);
            }
        }

        if (displayLoc == NEW) {

            try {

                // Make result image of float type
                // resultImage = new ModelImage(ModelImage.FLOAT, destExtents, name, userInterface);
                resultImage = (ModelImage) image.clone();
                resultImage.setImageName(name);

                if ((resultImage.getFileInfo()[0]).getFileFormat() == FileUtility.DICOM){
                    for (i = 0; i < resultImage.getExtents()[2]; i++) {
                        ((FileInfoDicom) (resultImage.getFileInfo(i))).setSecondaryCaptureTags();
                    }
                }
                // Make algorithm
                tiltAlgo = new AlgorithmTiltCorrection(resultImage, image, nonHelical, tiltAngle);

                // This is very important. Adding this object as a listener allows the algorithm to
                // notify this object when it has completed of failed. See algorithm performed event.
                // This is made possible by implementing AlgorithmedPerformed interface
                tiltAlgo.addListener(this);

                createProgressBar(image.getImageName(), tiltAlgo);

                // Hide dialog
                setVisible(false);

                if (isRunInSeparateThread()) {

                    // Start the thread as a low priority because we wish to still have user interface work fast.
                    if (tiltAlgo.startMethod(Thread.MIN_PRIORITY) == false) {
                        MipavUtil.displayError("A thread is already running on this object");
                    }
                } else {
                    tiltAlgo.run();
                }

            } catch (OutOfMemoryError x) {
                MipavUtil.displayError("Dialog NLNoiseReduction: unable to allocate enough memory");

                if (resultImage != null) {
                    resultImage.disposeLocal(); // Clean up image memory
                    resultImage = null;
                }

                return;
            }
        } else {

            try {

                // Make algorithm
                tiltAlgo = new AlgorithmTiltCorrection(null, image, nonHelical, tiltAngle);

                // This is very important. Adding this object as a listener allows the algorithm to
                // notify this object when it has completed of failed. See algorithm performed event.
                // This is made possible by implementing AlgorithmedPerformed interface
                tiltAlgo.addListener(this);

                createProgressBar(image.getImageName(), tiltAlgo);

                // Hide dialog
                setVisible(false);

                // These next lines set the titles in all frames where the source image is displayed to
                // "locked - " image name so as to indicate that the image is now read/write locked!
                // The image frames are disabled and then unregisted from the userinterface until the
                // algorithm has completed.
                Vector<ViewImageUpdateInterface> imageFrames = image.getImageFrameVector();
                titles = new String[imageFrames.size()];

                for (i = 0; i < imageFrames.size(); i++) {
                    titles[i] = ((Frame) (imageFrames.elementAt(i))).getTitle();
                    ((Frame) (imageFrames.elementAt(i))).setTitle("Locked: " + titles[i]);
                    ((Frame) (imageFrames.elementAt(i))).setEnabled(false);
                    userInterface.unregisterFrame((Frame) (imageFrames.elementAt(i)));
                }

                if (isRunInSeparateThread()) {

                    // Start the thread as a low priority because we wish to still have user interface work fast.
                    if (tiltAlgo.startMethod(Thread.MIN_PRIORITY) == false) {
                        MipavUtil.displayError("A thread is already running on this object");
                    }
                } else {
                    tiltAlgo.run();
                }

            } catch (OutOfMemoryError x) {
                MipavUtil.displayError("DialogTiltCorrection: unable to allocate enough memory");

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

        setNonHelical(scriptParameters.getParams().getBoolean("non_helical"));
        setTiltAngle(scriptParameters.getParams().getDouble("tilt_angle"));
    }

    /**
     * {@inheritDoc}
     */
    protected void storeParamsFromGUI() throws ParserException {
        scriptParameters.storeInputImage(image);
        scriptParameters.storeOutputImageParams(getResultImage(), (displayLoc == NEW));

        scriptParameters.getParams().put(ParameterFactory.newParameter("non_helical", nonHelical));
        scriptParameters.getParams().put(ParameterFactory.newParameter("tilt_angle", tiltAngle));
    }

    /**
     * Initializes the GUI by creating the components, placing them in the dialog, and displaying them.
     */
    private void init() {
        Object value;
        String s;
        double angle;
        JLabel labelAngle;

        setForeground(Color.black);

        getContentPane().setLayout(new BorderLayout());
        setTitle("Gantry/Detector Tilt Angle Correction");

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
        
        angle = 0.0;
        if ((image.getFileInfo()[0]).getFileFormat() == FileUtility.DICOM) {
            if (((FileInfoDicom) (image.getFileInfo(0))).getTagTable().getValue("0018,1120") != null) {
                value = ((FileInfoDicom) (image.getFileInfo(0))).getTagTable().getValue("0018,1120");
                s = ((String) value).trim();
                
                try {
                    angle = Double.valueOf(s).doubleValue();
                } catch (NumberFormatException e) {
                }
            } // if (((FileInfoDicom) (srcImage.getFileInfo(z))).getTagTable().getValue("0020,1041") != null)
        } // if ((image.getFileInfo()[0]).getFileFormat() == FileUtility.DICOM)
        
        labelAngle = createLabel("Gantry/Detector Tilt Angle in degrees");

        optionsPanel.add(labelAngle, gbc);

        gbc.gridx = 1;
        gbc.gridy = 0;
        textAngle = new JTextField();
        textAngle.setText(String.valueOf(angle));
        textAngle.setFont(serif12);
        optionsPanel.add(textAngle, gbc);
        
        typeGroup = new ButtonGroup();
        nonHelicalButton = new JRadioButton("NonHelical", true);
        nonHelicalButton.setEnabled(false);
        nonHelicalButton.setFont(serif12);
        nonHelicalButton.setForeground(Color.black);
        typeGroup.add(nonHelicalButton);
        gbc.gridx = 0;
        gbc.gridy = 1;
        optionsPanel.add(nonHelicalButton, gbc);
        
        helicalButton = new JRadioButton("Helical", false);
        helicalButton.setEnabled(false);
        helicalButton.setFont(serif12);
        helicalButton.setForeground(Color.black);
        typeGroup.add(helicalButton);
        gbc.gridx = 0;
        gbc.gridy = 2;
        optionsPanel.add(helicalButton, gbc);

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

        tiltAngle = 0.0;
        nonHelical = true;
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

        tmpStr = textAngle.getText();

        if (testParameter(tmpStr, -360.0, 360.0)) {
            tiltAngle = Double.valueOf(tmpStr).doubleValue();
        } else {
            textAngle.requestFocus();
            textAngle.selectAll();

            return false;
        }
        
        nonHelical = nonHelicalButton.isSelected();

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
                return new String("Algorithms.Utilities");
            }

            public String getDescription() {
                return new String("Performs tilt angle correction.");
            }

            public String getDescriptionLong() {
                return new String("Performs gantry/detector tilt angle correction.");
            }

            public String getShortLabel() {
                return new String("Tilt Angle Correction");
            }

            public String getLabel() {
                return new String("Tilt Angle Correction");
            }

            public String getName() {
                return new String("Tilt Angle Correction");
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
            table.put(new ParameterDouble("tilt_angle"));
            table.put(new ParameterBoolean("non_helical", true));
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
