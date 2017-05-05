package gov.nih.mipav.view.dialogs;


import gov.nih.mipav.model.algorithms.AlgorithmBase;
import gov.nih.mipav.model.algorithms.AlgorithmInterface;
import gov.nih.mipav.model.algorithms.AlgorithmSkullRemoval;
import gov.nih.mipav.model.file.FileInfoBase;
import gov.nih.mipav.model.scripting.ParserException;
import gov.nih.mipav.model.scripting.ScriptableActionInterface;
import gov.nih.mipav.model.scripting.parameters.*;
import gov.nih.mipav.model.structures.ModelImage;

import gov.nih.mipav.view.MipavUtil;
import gov.nih.mipav.view.Preferences;
import gov.nih.mipav.view.ViewJFrameImage;
import gov.nih.mipav.view.ViewUserInterface;

import java.awt.BorderLayout;
import java.awt.Frame;
import java.awt.GridLayout;
import java.awt.event.ActionEvent;

import javax.swing.*;


/**
 */
public class JDialogFaceAnonymize extends JDialogScriptableBase implements AlgorithmInterface, ActionDiscovery, ScriptableActionInterface {

    private static final long serialVersionUID = 4849596386051779471L;

    /** Face orientation not obtained from file information. */
    public static final int FACING_UNKNOWN = -1;

    /** Indicates sagittal image with x-axis oriented posterior to anterior. */
    public static final int FACING_RIGHT = 1;

    /** Indicates sagittal image with x-axis oriented anterior to posterior. */
    public static final int FACING_LEFT = 2;

    /** Indicates axial image with y-axis oriented posterior to anterior. */
    public static final int FACING_DOWN = 3;

    /** Indicates axial with y-axis oriented anterior to posterior. */
    public static final int FACING_UP = 4;

    /** Indicates coronal image with z-axis oriented posterior to anterior. */
    public static final int FACING_INTO_SCREEN = 5;

    /** Indicates coronal image with z-axis oriented anterior to posterior. */
    public static final int FACING_OUT_OF_SCREEN = 6;

    /** The face orientation parameter. */
    private static final String PARAM_FACE_ORIENTATION = "face_orientation";

    private static final String PARAM_ATLAS_IMG = "atlas_image";

    private static final String PARAM_DO_BLUR = "do_blur";

    private static final String PARAM_DO_REMOVE = "do_remove";

    private static final String PARAM_DO_FACE_ONLY = "do_face_only";

    private static final String PARAM_SHOW_FACE_SEG = "show_face_seg";

    private static final String PARAM_SHOW_SKULL_SEG = "show_skull_seg";

    private static final String PARAM_PAD_MM = "pad_in_mm";

    // ~ Instance fields
    // ------------------------------------------------------------------------------------------------

    /** The algorithm corresponding to this dialog box. */
    private AlgorithmSkullRemoval defaceAlgo;

    /** Indicates face orientation of the image. */
    private int faceOrientation;

    /** Button for facing down image. */
    private JRadioButton facingDownRadio;

    /** Button for facing into the screen image. */
    private JRadioButton facingIntoRadio;

    /** Button for facing left image. */
    private JRadioButton facingLeftRadio;

    /** Button for facing out image. */
    private JRadioButton facingOutRadio;

    /** Button for facing right image. */
    private JRadioButton facingRightRadio;

    /** Button for facing up image. */
    private JRadioButton facingUpRadio;

    /** Button for blurring just the face. */
    private JRadioButton blurFaceRadio;

    /** Button for removing just the face. */
    private JRadioButton removeFaceRadio;

    /** Button for blurring the face / skull. */
    private JRadioButton blurAllRadio;

    /** Button for removing the face / skull. */
    private JRadioButton removeAllRadio;

    /** Button for displaying the face segmentation. */
    private JRadioButton showFaceSegmentationRadio;

    /** Button for displaying the full segmentation. */
    private JRadioButton showAllSegmentationRadio;

    /** Text field corresponding to brain padding parameter. */
    private JTextField extraBrainPaddingField;

    /**
     * FaceAnonymizer parameter. Guarantees that the extracted brain will be avoided by the specified number of
     * millimeters. Initially set to 20.
     */
    private int extraMMsToPad = 0;

    private JTextField imageTextField;

    /** The image that face anonymization will be performed on. */
    private ModelImage srcImage;

    private boolean blur = false;

    private boolean remove = false;

    private boolean face = true;

    private boolean showFaceSegmentation = false;

    private boolean showSkullSegmentation = false;

    private ModelImage atlasImage = null;

    /**
     * Empty constructor needed for dynamic instantiation (used during scripting).
     */
    public JDialogFaceAnonymize() {}

    /**
     * Creates the face anonymizer dialog.
     * 
     * @param theParentFrame Parent frame.
     * @param im Source image.
     */
    public JDialogFaceAnonymize(final Frame theParentFrame, final ModelImage im) {
        super(theParentFrame, false);
        parentFrame = theParentFrame;
        srcImage = im;
        faceOrientation = getFaceOrientation(srcImage);
        if (faceOrientation == FACING_UNKNOWN) {
            initDirection();
        } else {
            init();
        }
    }

    // ~ Methods
    // --------------------------------------------------------------------------------------------------------

    /**
     * Presently only the script function calls this method. When the script sends this dialog the action command, this
     * method calls run.
     * 
     * @param event event that triggers function
     */
    @Override
    public void actionPerformed(final ActionEvent event) {
        final String command = event.getActionCommand();

        if (command.equalsIgnoreCase("atlasImageBrowse")) {
            ViewUserInterface.getReference().openImageFrame();
            final ViewJFrameImage imageFrame = ViewUserInterface.getReference().getActiveImageFrame();
            if (imageFrame != null) {
                if (atlasImage != null) {
                    atlasImage.disposeLocal();
                    atlasImage = null;
                }
                atlasImage = imageFrame.getActiveImage();
                if (atlasImage != null) {
                    imageTextField.setText(atlasImage.getImageFileName());
                }
            }
            // JFileChooser chooser = new JFileChooser();
            // if (ViewUserInterface.getReference().getDefaultDirectory() != null) {
            // chooser.setCurrentDirectory(new File(ViewUserInterface.getReference().getDefaultDirectory()));
            // } else {
            // chooser.setCurrentDirectory(new File(System.getProperties().getProperty("user.dir")));
            // }
            // chooser.setDialogTitle("Choose image");
            // chooser.setFileSelectionMode(JFileChooser.FILES_ONLY);
            // int returnValue = chooser.showOpenDialog(this);
            // if (returnValue == JFileChooser.APPROVE_OPTION) {
            // FileIO fileIO = new FileIO();
            // if(atlasImage != null) {
            // atlasImage.disposeLocal();
            // atlasImage = null;
            // }
            // atlasImage = fileIO.readImage(chooser.getSelectedFile().getName(), chooser.getCurrentDirectory() +
            // File.separator, false, null);
            //
            // imageTextField.setText(chooser.getSelectedFile().getAbsolutePath());
            // ViewUserInterface.getReference().setDefaultDirectory(chooser.getCurrentDirectory().toString() );
            // }
        }
        if (command.equals("OK")) {
            if (faceOrientation == FACING_UNKNOWN) {
                if (setVariablesDirection()) {
                    init();
                }
            } else if (setVariables()) {
                callAlgorithm();
            }
        } else if (command.equals("Cancel")) {
            dispose();
        } else if (command.equals("Help")) {
            // MipavUtil.showHelp("19014");
            MipavUtil.showWebHelp("Face_Anonymizer_(BET)#Applying_the_Face_Anonymizer_Algorithm");
        } else {
            super.actionPerformed(event);
        }
    }

    /**
     * This method is required if the AlgorithmPerformed interface is implemented. It is called by the algorithms when
     * it has completed or failed to to complete, so that the dialog can display the result image and/or clean up.
     * 
     * @param algorithm Algorithm that caused the event.
     */
    @Override
    public void algorithmPerformed(final AlgorithmBase algorithm) {

        if ( (algorithm instanceof AlgorithmSkullRemoval) && algorithm.isCompleted()) {
            insertScriptLine();
            setComplete(algorithm.isCompleted());

            defaceAlgo.finalize();
            defaceAlgo = null;
        }
    }

    /**
     * Construct a delimited string that contains the parameters to this algorithm.
     * 
     * @param delim the parameter delimiter (defaults to " " if empty)
     * 
     * @return the parameter string
     */
    public String getParameterString(String delim) {

        if (delim.equals("")) {
            delim = " ";
        }

        String str = new String();
        str += faceOrientation + delim;
        return str;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void setGUIFromParams() {
        srcImage = scriptParameters.retrieveInputImage();
        parentFrame = srcImage.getParentFrame();

        blur = scriptParameters.getParams().getBoolean(PARAM_DO_BLUR);
        remove = scriptParameters.getParams().getBoolean(PARAM_DO_REMOVE);
        face = scriptParameters.getParams().getBoolean(PARAM_DO_FACE_ONLY);
        showFaceSegmentation = scriptParameters.getParams().getBoolean(PARAM_SHOW_FACE_SEG);
        showSkullSegmentation = scriptParameters.getParams().getBoolean(PARAM_SHOW_SKULL_SEG);
        extraMMsToPad = scriptParameters.getParams().getInt(PARAM_PAD_MM);

        if (scriptParameters.getParams().containsParameter(PARAM_ATLAS_IMG)) {
            atlasImage = scriptParameters.getParams().getImage(PARAM_ATLAS_IMG);
        }
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void storeParamsFromGUI() throws ParserException {
        scriptParameters.storeInputImage(srcImage);

        if (atlasImage != null) {
            scriptParameters.storeImage(atlasImage, PARAM_ATLAS_IMG);
        }

        scriptParameters.getParams().put(ParameterFactory.newBoolean(PARAM_DO_BLUR, blur));
        scriptParameters.getParams().put(ParameterFactory.newBoolean(PARAM_DO_REMOVE, remove));
        scriptParameters.getParams().put(ParameterFactory.newBoolean(PARAM_DO_FACE_ONLY, face));
        scriptParameters.getParams().put(ParameterFactory.newBoolean(PARAM_SHOW_FACE_SEG, showFaceSegmentation));
        scriptParameters.getParams().put(ParameterFactory.newBoolean(PARAM_SHOW_SKULL_SEG, showSkullSegmentation));
        scriptParameters.getParams().put(ParameterFactory.newInt(PARAM_PAD_MM, extraMMsToPad));
    }

    /**
     * Perform any actions required after the running of the algorithm is complete.
     */
    @Override
    protected void doPostAlgorithmActions() {
        AlgorithmParameters.storeImageInRunner(defaceAlgo.getDestImage());

        if (showFaceSegmentation || showSkullSegmentation) {
            AlgorithmParameters.storeImageInRunner(defaceAlgo.getSegmentationImage());
        }

        if (defaceAlgo.getMaskImage() != null) {
            AlgorithmParameters.storeImageInRunner(defaceAlgo.getMaskImage());
        }
    }

    /**
     * Calls the algorithm.
     */
    @Override
    protected void callAlgorithm() {

        try {
            System.gc();
            if (atlasImage != null) {
                defaceAlgo = new AlgorithmSkullRemoval(srcImage, atlasImage);
            } else {
                defaceAlgo = new AlgorithmSkullRemoval(srcImage);
            }
            defaceAlgo.setOutputOption(blur, remove, face, showFaceSegmentation, showSkullSegmentation);
            defaceAlgo.setOffSet(extraMMsToPad);
            defaceAlgo.addListener(this);

            createProgressBar(srcImage.getImageName(), defaceAlgo);

            // Hide dialog
            setVisible(false);

            if (isRunInSeparateThread()) {

                // Start the thread as a low priority because we wish to still have user interface work fast.
                if (defaceAlgo.startMethod(Thread.MIN_PRIORITY) == false) {
                    MipavUtil.displayError("A thread is already running on this object");
                }
            } else {
                defaceAlgo.run();
            }
        } catch (final OutOfMemoryError x) {
            System.gc();
            MipavUtil.displayError("Dialog Extract Brain : unable to allocate enough memory");

            return;
        }
    }

    /**
     * Return the guess at the face's orientation based on the image orientation information.
     * 
     * @param img the image to guess from
     * 
     * @return the direction the face is likely pointing in (or FACING_UNKNOWN if a guess cannot be made)
     */
    private static int getFaceOrientation(final ModelImage img) {

        // See if the image orientation is known
        final int[] axisOrientation = img.getFileInfo(0).getAxisOrientation();

        if (axisOrientation[0] == FileInfoBase.ORI_P2A_TYPE) {
            return FACING_RIGHT;
        } else if (axisOrientation[0] == FileInfoBase.ORI_A2P_TYPE) {
            return FACING_LEFT;
        } else if (axisOrientation[1] == FileInfoBase.ORI_P2A_TYPE) {
            return FACING_DOWN;
        } else if (axisOrientation[1] == FileInfoBase.ORI_A2P_TYPE) {
            return FACING_UP;
        } else if (axisOrientation[2] == FileInfoBase.ORI_P2A_TYPE) {
            return FACING_INTO_SCREEN;
        } else if (axisOrientation[2] == FileInfoBase.ORI_A2P_TYPE) {
            return FACING_OUT_OF_SCREEN;
        } else {
            return FACING_UNKNOWN;
        }
    }

    /**
     * Makes the GUI elements of the dialog. Not called at present because it is not necessary.
     */
    private void initDirection() {
        setTitle("Anonymize Face");
        getContentPane().setLayout(new BorderLayout());

        final JPanel orientationPanel = new JPanel(new GridLayout(3, 2));
        orientationPanel.setBorder(MipavUtil.buildTitledBorder("Which way is the patient's face pointing?"));

        facingRightRadio = new JRadioButton("Right");
        facingRightRadio.setFont(MipavUtil.font12);

        if (faceOrientation == JDialogFaceAnonymize.FACING_RIGHT) {
            facingRightRadio.setSelected(true);
        }

        facingLeftRadio = new JRadioButton("Left");
        facingLeftRadio.setFont(MipavUtil.font12);

        if (faceOrientation == JDialogFaceAnonymize.FACING_LEFT) {
            facingLeftRadio.setSelected(true);
        }

        facingDownRadio = new JRadioButton("Down");
        facingDownRadio.setFont(MipavUtil.font12);

        if (faceOrientation == JDialogFaceAnonymize.FACING_DOWN) {
            facingDownRadio.setSelected(true);
        }

        facingUpRadio = new JRadioButton("Up");
        facingUpRadio.setFont(MipavUtil.font12);

        if (faceOrientation == JDialogFaceAnonymize.FACING_UP) {
            facingUpRadio.setSelected(true);
        }

        facingIntoRadio = new JRadioButton("Into the screen");
        facingIntoRadio.setFont(MipavUtil.font12);

        if (faceOrientation == JDialogFaceAnonymize.FACING_INTO_SCREEN) {
            facingIntoRadio.setSelected(true);
        }

        facingOutRadio = new JRadioButton("Out of the screen");
        facingOutRadio.setFont(MipavUtil.font12);

        if (faceOrientation == JDialogFaceAnonymize.FACING_OUT_OF_SCREEN) {
            facingOutRadio.setSelected(true);
        }

        final ButtonGroup group = new ButtonGroup();
        group.add(facingDownRadio);
        group.add(facingUpRadio);
        group.add(facingRightRadio);
        group.add(facingLeftRadio);
        group.add(facingIntoRadio);
        group.add(facingOutRadio);

        orientationPanel.add(facingRightRadio);
        orientationPanel.add(facingLeftRadio);
        orientationPanel.add(facingDownRadio);
        orientationPanel.add(facingUpRadio);
        orientationPanel.add(facingIntoRadio);
        orientationPanel.add(facingOutRadio);

        final JPanel mainPanel = new JPanel(new BorderLayout());
        mainPanel.add(orientationPanel, BorderLayout.NORTH);

        getContentPane().add(mainPanel, BorderLayout.NORTH);
        getContentPane().add(buildButtons(), BorderLayout.SOUTH);

        pack();
        // setResizable(false);
        setVisible(true);
    }

    /**
     * Makes the GUI elements of the dialog. Not called at present because it is not necessary.
     */
    private void init() {
        setTitle("Anonymize Face");
        getContentPane().setLayout(new BorderLayout());

        final JPanel orientationPanel = new JPanel(new GridLayout(3, 2));
        orientationPanel.setBorder(MipavUtil.buildTitledBorder("Options"));

        blurFaceRadio = new JRadioButton("Blur face.");
        blurFaceRadio.setFont(MipavUtil.font12);

        removeFaceRadio = new JRadioButton("Remove face.");
        removeFaceRadio.setFont(MipavUtil.font12);

        blurAllRadio = new JRadioButton("Blur skull.");
        blurAllRadio.setFont(MipavUtil.font12);

        removeAllRadio = new JRadioButton("Remove skull.");
        removeAllRadio.setFont(MipavUtil.font12);

        showFaceSegmentationRadio = new JRadioButton("Show face segmentation results.");
        showFaceSegmentationRadio.setFont(MipavUtil.font12);
        showFaceSegmentationRadio.setSelected(true);

        showAllSegmentationRadio = new JRadioButton("Show skull segmentation results.");
        showAllSegmentationRadio.setFont(MipavUtil.font12);
        showAllSegmentationRadio.setSelected(true);

        final ButtonGroup group = new ButtonGroup();
        group.add(blurFaceRadio);
        group.add(removeFaceRadio);
        group.add(blurAllRadio);
        group.add(removeAllRadio);
        group.add(showFaceSegmentationRadio);
        group.add(showAllSegmentationRadio);

        orientationPanel.add(blurFaceRadio);
        orientationPanel.add(blurAllRadio);
        orientationPanel.add(removeFaceRadio);
        orientationPanel.add(removeAllRadio);
        orientationPanel.add(showFaceSegmentationRadio);
        orientationPanel.add(showAllSegmentationRadio);

        final JPanel removalPanel = new JPanel(new GridLayout(1, 2));
        removalPanel.setBorder(MipavUtil.buildTitledBorder("Face removal options"));

        extraBrainPaddingField = new JTextField();
        extraBrainPaddingField.setText("" + extraMMsToPad);
        extraBrainPaddingField.setColumns(2);

        final JLabel extraPaddingLabel = new JLabel("Extracted brain is avoided by a buffer of this many mms");
        extraPaddingLabel.setFont(MipavUtil.font12);
        removalPanel.add(extraPaddingLabel);
        removalPanel.add(extraBrainPaddingField);

        final JPanel atlasPanel = new JPanel(new BorderLayout());
        atlasPanel.add(new JLabel("Optional Atlas Image:"), BorderLayout.WEST);
        imageTextField = new JTextField();
        atlasPanel.add(imageTextField, BorderLayout.CENTER);
        final JButton browseButton = new JButton("Browse");
        browseButton.setMinimumSize(MipavUtil.defaultButtonSize);
        browseButton.setPreferredSize(MipavUtil.defaultButtonSize);
        browseButton.setFont(serif12B);
        browseButton.addActionListener(this);
        browseButton.setActionCommand("atlasImageBrowse");
        atlasPanel.add(browseButton, BorderLayout.EAST);

        final JPanel mainPanel = new JPanel(new BorderLayout());
        mainPanel.add(orientationPanel, BorderLayout.NORTH);
        mainPanel.add(removalPanel, BorderLayout.CENTER);
        mainPanel.add(atlasPanel, BorderLayout.SOUTH);

        getContentPane().removeAll();
        getContentPane().add(mainPanel, BorderLayout.NORTH);
        getContentPane().add(buildButtons(), BorderLayout.SOUTH);

        pack();
        setVisible(true);
    }

    /**
     * Use the GUI results to set up the variables needed to run the algorithm.
     * 
     * @return <code>true</code> if parameters set successfully, <code>false</code> otherwise.
     */
    private boolean setVariablesDirection() {

        if (facingRightRadio.isSelected()) {
            faceOrientation = FACING_RIGHT;
        } else if (facingLeftRadio.isSelected()) {
            faceOrientation = FACING_LEFT;
        } else if (facingDownRadio.isSelected()) {
            faceOrientation = FACING_DOWN;
        } else if (facingUpRadio.isSelected()) {
            faceOrientation = FACING_UP;
        } else if (facingIntoRadio.isSelected()) {
            faceOrientation = FACING_INTO_SCREEN;
        } else if (facingOutRadio.isSelected()) {
            faceOrientation = FACING_OUT_OF_SCREEN;
        } else {
            MipavUtil.displayWarning("You must select the direction that the patient's face is pointing.");

            return false;
        }

        return true;
    }

    /**
     * Use the GUI results to set up the variables needed to run the algorithm.
     * 
     * @return <code>true</code> if parameters set successfully, <code>false</code> otherwise.
     */
    private boolean setVariables() {

        blur = blurFaceRadio.isSelected() || blurAllRadio.isSelected();
        remove = removeFaceRadio.isSelected() || removeAllRadio.isSelected();
        face = blurFaceRadio.isSelected() || removeFaceRadio.isSelected();
        showFaceSegmentation = showFaceSegmentationRadio.isSelected();
        showSkullSegmentation = showAllSegmentationRadio.isSelected();
        if (MipavUtil.testParameter(extraBrainPaddingField.getText(), 0, 500)) {
            extraMMsToPad = Integer.parseInt(extraBrainPaddingField.getText());
        } else {
            MipavUtil.displayError("Number of mms to pad around brain region should be between 0 and 500");

            return false;
        }

        return true;
    }

    /**
     * Return meta-information about this discoverable action for categorization and labeling purposes.
     * 
     * @return Metadata for this action.
     */
    @Override
    public ActionMetadata getActionMetadata() {
        return new MipavActionMetadata() {
            @Override
            public String getCategory() {
                return new String("Algorithms.Brain tools");
            }

            @Override
            public String getDescription() {
                return new String("Removes the face from 3d brain scans.");
            }

            @Override
            public String getDescriptionLong() {
                return new String("Removes the face from 3d brain scans.");
            }

            @Override
            public String getShortLabel() {
                return new String("AnonymizeFace");
            }

            @Override
            public String getLabel() {
                return new String("Anonymize Face");
            }

            @Override
            public String getName() {
                return new String("Anonymize Face");
            }
        };
    }

    /**
     * Returns a table listing the input parameters of this algorithm (which should match up with the scripting
     * parameters used in {@link #setGUIFromParams()}).
     * 
     * @return A parameter table listing the inputs of this algorithm.
     */
    @Override
    public ParameterTable createInputParameters() {
        final ParameterTable table = new ParameterTable();

        try {
            table.put(new ParameterExternalImage(AlgorithmParameters.getInputImageLabel(1)));
            table.put(new ParameterInt(PARAM_FACE_ORIENTATION, 5));
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
    @Override
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
    @Override
    public String getOutputImageName(final String imageParamName) {
        if (imageParamName.equals(AlgorithmParameters.RESULT_IMAGE)) {
            return srcImage.getImageName();
        }

        Preferences.debug("Unrecognized output image parameter: " + imageParamName + "\n", Preferences.DEBUG_SCRIPTING);

        return null;
    }

    /**
     * Returns whether the action has successfully completed its execution.
     * 
     * @return True, if the action is complete. False, if the action failed or is still running.
     */
    @Override
    public boolean isActionComplete() {
        return isComplete();
    }

}
