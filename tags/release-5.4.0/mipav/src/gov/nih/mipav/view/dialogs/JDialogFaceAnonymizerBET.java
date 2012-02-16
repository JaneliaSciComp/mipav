package gov.nih.mipav.view.dialogs;


import gov.nih.mipav.model.algorithms.*;
import gov.nih.mipav.model.file.*;
import gov.nih.mipav.model.scripting.*;
import gov.nih.mipav.model.scripting.parameters.*;
import gov.nih.mipav.model.structures.*;

import gov.nih.mipav.view.*;

import java.awt.*;
import java.awt.event.*;

import java.util.*;

import javax.swing.*;


/**
 * Dialog used to input parameters and start the BET-based face anonymizer.
 *
 * @author  mccreedy
 */
public class JDialogFaceAnonymizerBET extends JDialogScriptableBase
        implements AlgorithmInterface, DialogDefaultsInterface, ActionDiscovery, ScriptableActionInterface {

    //~ Static fields/initializers -------------------------------------------------------------------------------------

    /** Use serialVersionUID for interoperability. */
    private static final long serialVersionUID = -1027127665518969757L;

    /** Face orientation not obtained from file information. */
    private static final int FACING_UNKNOWN = -1;

    /** Indicates sagittal image with x-axis oriented posterior to anterior. */
    private static final int FACING_RIGHT = 1;

    /** Indicates sagittal image with x-axis oriented anterior to posterior. */
    private static final int FACING_LEFT = 2;

    /** Indicates axial image with y-axis oriented posterior to anterior. */
    private static final int FACING_DOWN = 3;

    /** Indicates axial with y-axis oriented anterior to posterior. */
    private static final int FACING_UP = 4;

    /** Indicates coronal image with z-axis oriented posterior to anterior. */
    private static final int FACING_INTO_SCREEN = 5;

    /** Indicates coronal image with z-axis oriented anterior to posterior. */
    private static final int FACING_OUT_OF_SCREEN = 6;

    /** The face orientation parameter. */
    private static final String PARAM_FACE_ORIENTATION = "face_orientation";

    /** Millimeters to add to brain mask. */
    private static final String PARAM_EXTRA_MMS_TO_PAD = "mms_to_delete_from_face";

    /** Flag indicates BET should approximate initial brain by sphere. */
    private static final String PARAM_BET_DO_SPHERE_ESTIMATION = "bet_do_estimate_with_sphere";

    /** BET based parameter for the depth to calculate in approximating brain maximum and minimum intensities. */
    private static final String PARAM_BET_IMG_INFLUENCE = "bet_image_influence";

    /** Controls the stiffness of the brain approximation mesh. */
    private static final String PARAM_BET_STIFFNESS = "bet_stiffness";

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** The algorithm corresponding to this dialog box. */
    private AlgorithmFaceAnonymizerBET defaceAlgo;

    /** Flag indicates BET should approximate initial brain by sphere. Initially set to false. */
    private boolean estimateWithSphereBET = false;

    /** Check box corresponding to <code>estimateSphereBET</code>. */
    private JCheckBox estimateWithSphereCheckbox;

    /** Text field corresponding to brain padding parameter. */
    private JTextField extraBrainPaddingField;

    /**
     * FaceAnonymizer parameter. Guarantees that the extracted brain will be avoided by the specified number of
     * millimeters. Initially set to 20.
     */
    private int extraMMsToPad = 20;

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

    /** the depth to calculate in approximating brain maximum and minimum intensities. Initially set to .1 */
    private float imageInfluenceBET = 0.1f;

    /** The text field corresponding to <code>imageInfluenceField</code>. */
    private JTextField imageInfluenceField;

    /** The image that face anonymization will be performed on. */
    private ModelImage srcImage;

    /** Controls the stiffness of the brain approximation mesh. Initially set to 0.15. */
    private float stiffnessBET = 0.15f;

    /** The text field corresponding to <code>stiffnessBET</code>. */
    private JTextField stiffnessField;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Empty constructor needed for dynamic instantiation (used during scripting).
     */
    public JDialogFaceAnonymizerBET() { }

    /**
     * Creates the face anonymizer dialog.
     *
     * @param  theParentFrame  Parent frame.
     * @param  im              Source image.
     */
    public JDialogFaceAnonymizerBET(Frame theParentFrame, ModelImage im) {
        super(theParentFrame, false);
        parentFrame = theParentFrame;
        srcImage = im;
        faceOrientation = getFaceOrientation(srcImage);
        loadDefaults();
        init();
    }

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * Presently only the script function calls this method. When the script sends this dialog the action command, this
     * method calls run.
     *
     * @param  event  event that triggers function
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
            MipavUtil.showHelp("19014");
        }
    }

    /**
     * This method is required if the AlgorithmPerformed interface is implemented. It is called by the algorithms when
     * it has completed or failed to to complete, so that the dialog can display the result image and/or clean up.
     *
     * @param  algorithm  Algorithm that caused the event.
     */
    public void algorithmPerformed(AlgorithmBase algorithm) {

        if (Preferences.is(Preferences.PREF_SAVE_DEFAULTS) && (this.getOwner() != null) && !isScriptRunning()) {
            saveDefaults();
        }

        if ((algorithm instanceof AlgorithmFaceAnonymizerBET) && algorithm.isCompleted()) {
            insertScriptLine();
            srcImage.calcMinMax();
            srcImage.notifyImageDisplayListeners(null, true);
         // save the completion status for later
            setComplete(algorithm.isCompleted());

            defaceAlgo.finalize();
            defaceAlgo = null;
        }
    }

    /**
     * Construct a delimited string that contains the parameters to this algorithm.
     *
     * @param   delim  the parameter delimiter (defaults to " " if empty)
     *
     * @return  the parameter string
     */
    public String getParameterString(String delim) {

        if (delim.equals("")) {
            delim = " ";
        }

        String str = new String();
        str += faceOrientation + delim;
        str += extraMMsToPad + delim;

        // str += verticalDeletionLimit + delim;
        str += estimateWithSphereBET + delim;
        str += imageInfluenceBET + delim;
        str += stiffnessBET;

        return str;
    }

    /**
     * Loads the default settings from Preferences to set up the dialog.
     */
    public void loadDefaults() {
        String defaultsString = Preferences.getDialogDefaults(getDialogName());

        if (defaultsString != null) {

            try {
                StringTokenizer st = new StringTokenizer(defaultsString, ",");
                faceOrientation = MipavUtil.getInt(st);
                extraMMsToPad = MipavUtil.getInt(st);
                estimateWithSphereBET = MipavUtil.getBoolean(st);
                imageInfluenceBET = MipavUtil.getFloat(st);
                stiffnessBET = MipavUtil.getFloat(st);
            } catch (Exception ex) {
                Preferences.debug("Resetting defaults for dialog: " + getDialogName());
                Preferences.removeProperty(getDialogName());
                ex.printStackTrace();
            }
        }
    }

    /**
     * Saves the default settings into the Preferences file.
     */
    public void saveDefaults() {
        String defaultsString = new String(getParameterString(","));
        Preferences.saveDialogDefaults(getDialogName(), defaultsString);
    }

    /**
     * {@inheritDoc}
     */
    public void setGUIFromParams() {
        srcImage = scriptParameters.retrieveInputImage();
        parentFrame = srcImage.getParentFrame();

        faceOrientation = getFaceOrientation(srcImage);

        int scriptFaceOrientation = scriptParameters.getParams().getInt(PARAM_FACE_ORIENTATION);

        if (faceOrientation == FACING_UNKNOWN) {
            faceOrientation = scriptFaceOrientation;
        }

        extraMMsToPad = scriptParameters.getParams().getInt(PARAM_EXTRA_MMS_TO_PAD);
        estimateWithSphereBET = scriptParameters.getParams().getBoolean(PARAM_BET_DO_SPHERE_ESTIMATION);
        imageInfluenceBET = scriptParameters.getParams().getFloat(PARAM_BET_IMG_INFLUENCE);
        stiffnessBET = scriptParameters.getParams().getFloat(PARAM_BET_STIFFNESS);
    }

    /**
     * {@inheritDoc}
     */
    public void storeParamsFromGUI() throws ParserException {
        scriptParameters.storeInputImage(srcImage);

        scriptParameters.getParams().put(ParameterFactory.newInt(PARAM_FACE_ORIENTATION, faceOrientation));
        scriptParameters.getParams().put(ParameterFactory.newInt(PARAM_EXTRA_MMS_TO_PAD, extraMMsToPad));
        scriptParameters.getParams().put(ParameterFactory.newBoolean(PARAM_BET_DO_SPHERE_ESTIMATION,
                                                                     estimateWithSphereBET));
        scriptParameters.getParams().put(ParameterFactory.newFloat(PARAM_BET_IMG_INFLUENCE, imageInfluenceBET));
        scriptParameters.getParams().put(ParameterFactory.newFloat(PARAM_BET_STIFFNESS, stiffnessBET));
    }

    /**
     * Calls the algorithm.
     */
    protected void callAlgorithm() {

        try {
            System.gc();
            defaceAlgo = new AlgorithmFaceAnonymizerBET(srcImage, faceOrientation, extraMMsToPad);

            /*verticalDeletionLimit*/
            defaceAlgo.setBETParameters(estimateWithSphereBET, imageInfluenceBET, stiffnessBET);
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
        } catch (OutOfMemoryError x) {
            System.gc();
            MipavUtil.displayError("Dialog Extract Brain : unable to allocate enough memory");

            return;
        }
    }

    /**
     * Return the guess at the face's orientation based on the image orientation information.
     *
     * @param   img  the image to guess from
     *
     * @return  the direction the face is likely pointing in (or FACING_UNKNOWN if a guess cannot be made)
     */
    private static int getFaceOrientation(ModelImage img) {

        // See if the image orientation is known
        int[] axisOrientation = img.getFileInfo(0).getAxisOrientation();

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
    private void init() {
        setTitle("Anonymize Face");
        getContentPane().setLayout(new BorderLayout());

        JPanel orientationPanel = new JPanel(new GridLayout(3, 2));
        orientationPanel.setBorder(MipavUtil.buildTitledBorder("Which way is the patient's face pointing?"));

        facingRightRadio = new JRadioButton("Right");
        facingRightRadio.setFont(MipavUtil.font12);

        if (faceOrientation == JDialogFaceAnonymizerBET.FACING_RIGHT) {
            facingRightRadio.setSelected(true);
        }

        facingLeftRadio = new JRadioButton("Left");
        facingLeftRadio.setFont(MipavUtil.font12);

        if (faceOrientation == JDialogFaceAnonymizerBET.FACING_LEFT) {
            facingLeftRadio.setSelected(true);
        }

        facingDownRadio = new JRadioButton("Down");
        facingDownRadio.setFont(MipavUtil.font12);

        if (faceOrientation == JDialogFaceAnonymizerBET.FACING_DOWN) {
            facingDownRadio.setSelected(true);
        }

        facingUpRadio = new JRadioButton("Up");
        facingUpRadio.setFont(MipavUtil.font12);

        if (faceOrientation == JDialogFaceAnonymizerBET.FACING_UP) {
            facingUpRadio.setSelected(true);
        }

        facingIntoRadio = new JRadioButton("Into the screen");
        facingIntoRadio.setFont(MipavUtil.font12);

        if (faceOrientation == JDialogFaceAnonymizerBET.FACING_INTO_SCREEN) {
            facingIntoRadio.setSelected(true);
        }

        facingOutRadio = new JRadioButton("Out of the screen");
        facingOutRadio.setFont(MipavUtil.font12);

        if (faceOrientation == JDialogFaceAnonymizerBET.FACING_OUT_OF_SCREEN) {
            facingOutRadio.setSelected(true);
        }

        ButtonGroup group = new ButtonGroup();
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

        JPanel removalPanel = new JPanel(new GridBagLayout());
        removalPanel.setBorder(MipavUtil.buildTitledBorder("Face removal options"));

        extraBrainPaddingField = new JTextField();
        extraBrainPaddingField.setText("" + extraMMsToPad);
        extraBrainPaddingField.setColumns(2);

        JLabel extraPaddingLabel = new JLabel("Extracted brain is avoided by a buffer of this many mms");
        extraPaddingLabel.setFont(MipavUtil.font12);

        GridBagConstraints gbc = new GridBagConstraints();
        gbc.anchor = GridBagConstraints.WEST;
        gbc.fill = GridBagConstraints.HORIZONTAL;
        gbc.gridx = 0;
        gbc.gridy = 0;
        gbc.ipadx = 5;
        removalPanel.add(extraPaddingLabel, gbc);
        gbc.anchor = GridBagConstraints.EAST;
        gbc.gridx++;
        removalPanel.add(extraBrainPaddingField, gbc);
        gbc.gridx = 0;

        JPanel betPanel = new JPanel(new GridBagLayout());
        betPanel.setBorder(MipavUtil.buildTitledBorder("Brain extraction options"));

        estimateWithSphereCheckbox = new JCheckBox("Estimate initial boundary using a sphere.");
        estimateWithSphereCheckbox.setFont(MipavUtil.font12);
        estimateWithSphereCheckbox.setSelected(estimateWithSphereBET);

        imageInfluenceField = new JTextField();
        imageInfluenceField.setText("" + imageInfluenceBET);
        imageInfluenceField.setColumns(2);

        JLabel imageInfluenceLabel = new JLabel("Image influence ratio (0.01 - 0.5)");
        imageInfluenceLabel.setFont(MipavUtil.font12);

        stiffnessField = new JTextField();
        stiffnessField.setText("" + stiffnessBET);
        stiffnessField.setColumns(2);

        JLabel stiffnessLabel = new JLabel("Mesh stiffness (0.01 - 0.5)");
        stiffnessLabel.setFont(MipavUtil.font12);

        gbc.gridx = 0;
        gbc.gridy = 0;
        betPanel.add(estimateWithSphereCheckbox, gbc);
        gbc.gridy++;
        betPanel.add(imageInfluenceLabel, gbc);
        gbc.anchor = GridBagConstraints.EAST;
        gbc.gridx++;
        betPanel.add(imageInfluenceField, gbc);
        gbc.anchor = GridBagConstraints.WEST;
        gbc.gridx = 0;
        gbc.gridy++;
        betPanel.add(stiffnessLabel, gbc);
        gbc.anchor = GridBagConstraints.EAST;
        gbc.gridx++;
        betPanel.add(stiffnessField, gbc);

        JPanel mainPanel = new JPanel(new BorderLayout());
        mainPanel.add(orientationPanel, BorderLayout.NORTH);
        mainPanel.add(removalPanel, BorderLayout.CENTER);
        mainPanel.add(betPanel, BorderLayout.SOUTH);

        getContentPane().add(mainPanel, BorderLayout.NORTH);
        getContentPane().add(buildButtons(), BorderLayout.SOUTH);

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

        if (MipavUtil.testParameter(extraBrainPaddingField.getText(), 0, 500)) {
            extraMMsToPad = Integer.parseInt(extraBrainPaddingField.getText());
        } else {
            MipavUtil.displayError("Number of mms to pad around brain region should be between 0 and 500");

            return false;
        }

        estimateWithSphereBET = estimateWithSphereCheckbox.isSelected();

        if (MipavUtil.testParameter(imageInfluenceField.getText(), 0.01, 0.5)) {
            imageInfluenceBET = Float.parseFloat(imageInfluenceField.getText());
        } else {
            MipavUtil.displayError("Image influence ratio should be between 0.01 and 0.5");

            return false;
        }

        if (MipavUtil.testParameter(stiffnessField.getText(), 0.01, 0.5)) {
            stiffnessBET = Float.parseFloat(stiffnessField.getText());
        } else {
            MipavUtil.displayError("Mesh stiffness should be between 0.01 and 0.5");

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
                return new String("Algorithms.Brain tools");
            }

            public String getDescription() {
                return new String("Removes the face from 3d brain scans.");
            }

            public String getDescriptionLong() {
                return new String("Removes the face from 3d brain scans.");
            }

            public String getShortLabel() {
                return new String("AnonymizeFace");
            }

            public String getLabel() {
                return new String("Anonymize Face");
            }

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
    public ParameterTable createInputParameters() {
        final ParameterTable table = new ParameterTable();
        
        try {
            table.put(new ParameterExternalImage(AlgorithmParameters.getInputImageLabel(1)));
            table.put(new ParameterInt(PARAM_FACE_ORIENTATION, 5));
            table.put(new ParameterInt(PARAM_EXTRA_MMS_TO_PAD, 20));
            table.put(new ParameterBoolean(PARAM_BET_DO_SPHERE_ESTIMATION, false));
            table.put(new ParameterFloat(PARAM_BET_IMG_INFLUENCE, .1f));
            table.put(new ParameterFloat(PARAM_BET_STIFFNESS, .15f));
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
    public boolean isActionComplete() {
        return isComplete();
    }

}
