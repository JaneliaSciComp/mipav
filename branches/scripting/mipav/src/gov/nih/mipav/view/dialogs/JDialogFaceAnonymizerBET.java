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
public class JDialogFaceAnonymizerBET extends JDialogBase
        implements AlgorithmInterface, DialogDefaultsInterface, ScriptableActionInterface {

    //~ Static fields/initializers -------------------------------------------------------------------------------------

    /** Use serialVersionUID for interoperability. */
    private static final long serialVersionUID = -1027127665518969757L;

    /** DOCUMENT ME! */
    private static final int FACING_UNKNOWN = -1;

    /** DOCUMENT ME! */
    private static final int FACING_RIGHT = 1;

    /** DOCUMENT ME! */
    private static final int FACING_LEFT = 2;

    /** DOCUMENT ME! */
    private static final int FACING_DOWN = 3;

    /** DOCUMENT ME! */
    private static final int FACING_UP = 4;

    /** DOCUMENT ME! */
    private static final int FACING_INTO_SCREEN = 5;

    /** DOCUMENT ME! */
    private static final int FACING_OUT_OF_SCREEN = 6;

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** DOCUMENT ME! */
    private AlgorithmFaceAnonymizerBET defaceAlgo;

    /** DOCUMENT ME! */
    private boolean estimateWithSphereBET = false;

    /** DOCUMENT ME! */
    private JCheckBox estimateWithSphereCheckbox;

    /** DOCUMENT ME! */
    private JTextField extraDeletionField;

    /** DOCUMENT ME! */
    private int extraMMsToDelete = 20;

    /** DOCUMENT ME! */
    private int faceOrientation;

    /** DOCUMENT ME! */
    private JRadioButton facingDownRadio;

    /** DOCUMENT ME! */
    private JRadioButton facingIntoRadio;

    /** DOCUMENT ME! */
    private JRadioButton facingLeftRadio;

    /** DOCUMENT ME! */
    private JRadioButton facingOutRadio;

    /** DOCUMENT ME! */
    private JRadioButton facingRightRadio;

    /** DOCUMENT ME! */
    private JRadioButton facingUpRadio;

    /** DOCUMENT ME! */
    private float imageInfluenceBET = 0.1f;

    /** DOCUMENT ME! */
    private JTextField imageInfluenceField;

    /** DOCUMENT ME! */
    private ModelImage srcImage;

    /** DOCUMENT ME! */
    private float stiffnessBET = 0.15f;

    /** DOCUMENT ME! */
    private JTextField stiffnessField;

    /** DOCUMENT ME! */
    private float verticalDeletionLimit = .33f;

    /** DOCUMENT ME! */
    private JTextField verticalLimitField;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Empty constructor needed for dynamic instantiation (used during scripting).
     */
    public JDialogFaceAnonymizerBET() { }

    /**
     * Sets the appropriate variables. Does not actually create a dialog that is visible because no user input is
     * necessary at present.
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

    /**
     * Sets the appropriate variables. Does not actually create a dialog that is visible because no user input is
     * necessary at present. This constructor is used by the script parser because it doesn't have the parent frame.
     *
     * @param  ui  User interface.
     * @param  im  Source image.
     */
    public JDialogFaceAnonymizerBET(ViewUserInterface ui, ModelImage im) {
        super();
        parentFrame = im.getParentFrame();
        srcImage = im;
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
            // MipavUtil.showHelp( "10045" );
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
            defaceAlgo.finalize();
            defaceAlgo = null;
        }
    }

    /**
     * Calls the algorithm.
     */
    public void callAlgorithm() {

        try {
            System.gc();
            defaceAlgo = new AlgorithmFaceAnonymizerBET(srcImage, faceOrientation, extraMMsToDelete,
                                                        verticalDeletionLimit);
            defaceAlgo.setBETParameters(estimateWithSphereBET, imageInfluenceBET, stiffnessBET);
            defaceAlgo.addListener(this);

            // Hide dialog
            setVisible(false);

            if (isRunInSeparateThread()) {

                // Start the thread as a low priority because we wish to still have user interface work fast.
                if (defaceAlgo.startMethod(Thread.MIN_PRIORITY) == false) {
                    MipavUtil.displayError("A thread is already running on this object");
                }
            } else {
                if (!ViewUserInterface.getReference().isAppFrameVisible()) {
                    defaceAlgo.setProgressBarVisible(false);
                }

                defaceAlgo.run();
            }
        } catch (OutOfMemoryError x) {
            System.gc();
            MipavUtil.displayError("Dialog Extract Brain : unable to allocate enough memory");

            return;
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
        str += extraMMsToDelete + delim;
        str += verticalDeletionLimit + delim;
        str += estimateWithSphereBET + delim;
        str += imageInfluenceBET + delim;
        str += stiffnessBET;

        return str;
    }

    /**
     * If a script is being recorded and the algorithm is done, add an entry for this algorithm.
     */
    public void insertScriptLine() {
        try {
            AlgorithmParameters algoParams = new DialogParameters();
            algoParams.storeParamsFromGUI();
            ScriptRecorder.getReference().addLine("FaceAnonymizerBET", algoParams.getParams());
        } catch (ParserException pe) {
            MipavUtil.displayError("Error encountered recording FaceAnonymizerBET script line:\n" + pe);
        }
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
                extraMMsToDelete = MipavUtil.getInt(st);
                verticalDeletionLimit = MipavUtil.getFloat(st);
                estimateWithSphereBET = MipavUtil.getBoolean(st);
                imageInfluenceBET = MipavUtil.getFloat(st);
                stiffnessBET = MipavUtil.getFloat(st);
            } catch (Exception ex) {

                // since there was a problem parsing the defaults string, start over with the original defaults
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
     * Run this algorithm from a script.
     *
     * @param  parameters  table of parameters for the script to use
     */
    public void scriptRun(ParameterTable parameters) {
        setScriptRunning(true);

        AlgorithmParameters algoParams = new DialogParameters(parameters);
        algoParams.setGUIFromParams();

        // setActiveImage(parser.isActiveImage());
        setSeparateThread(false);

        callAlgorithm();

        algoParams.doPostAlgorithmActions();
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

        // Create the radio buttons.
        facingRightRadio = new JRadioButton("Right");
        facingRightRadio.setFont(MipavUtil.font12);

        if (faceOrientation == JDialogFaceAnonymizer.FACING_RIGHT) {
            facingRightRadio.setSelected(true);
        }

        facingLeftRadio = new JRadioButton("Left");
        facingLeftRadio.setFont(MipavUtil.font12);

        if (faceOrientation == JDialogFaceAnonymizer.FACING_LEFT) {
            facingLeftRadio.setSelected(true);
        }

        facingDownRadio = new JRadioButton("Down");
        facingDownRadio.setFont(MipavUtil.font12);

        if (faceOrientation == JDialogFaceAnonymizer.FACING_DOWN) {
            facingDownRadio.setSelected(true);
        }

        facingUpRadio = new JRadioButton("Up");
        facingUpRadio.setFont(MipavUtil.font12);

        if (faceOrientation == JDialogFaceAnonymizer.FACING_UP) {
            facingUpRadio.setSelected(true);
        }

        facingIntoRadio = new JRadioButton("Into the screen");
        facingIntoRadio.setFont(MipavUtil.font12);

        if (faceOrientation == JDialogFaceAnonymizer.FACING_INTO_SCREEN) {
            facingIntoRadio.setSelected(true);
        }

        facingOutRadio = new JRadioButton("Out of the screen");
        facingOutRadio.setFont(MipavUtil.font12);

        if (faceOrientation == JDialogFaceAnonymizer.FACING_OUT_OF_SCREEN) {
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

        extraDeletionField = new JTextField();
        extraDeletionField.setText("" + extraMMsToDelete);
        extraDeletionField.setColumns(2);

        JLabel extraDeletionLabel = new JLabel("Additional mms to delete from facial area");
        extraDeletionLabel.setFont(MipavUtil.font12);

        verticalLimitField = new JTextField();
        verticalLimitField.setText("" + verticalDeletionLimit);
        verticalLimitField.setColumns(2);

        JLabel verticalLimitLabel = new JLabel("Vertial limit on the face deletion (0 = no removal, 1 = no limit)");
        verticalLimitLabel.setFont(MipavUtil.font12);

        GridBagConstraints gbc = new GridBagConstraints();
        gbc.anchor = GridBagConstraints.WEST;
        gbc.fill = GridBagConstraints.HORIZONTAL;
        gbc.gridx = 0;
        gbc.gridy = 0;
        gbc.ipadx = 5;
        removalPanel.add(extraDeletionLabel, gbc);
        gbc.anchor = GridBagConstraints.EAST;
        gbc.gridx++;
        removalPanel.add(extraDeletionField, gbc);
        gbc.gridx = 0;
        gbc.gridy++;
        removalPanel.add(verticalLimitLabel, gbc);
        gbc.gridx++;
        removalPanel.add(verticalLimitField, gbc);

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
        setResizable(false);
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

        if (MipavUtil.testParameter(extraDeletionField.getText(), 0, 500)) {
            extraMMsToDelete = Integer.parseInt(extraDeletionField.getText());
        } else {
            MipavUtil.displayError("Number of mms to delete from facial regions should be between 0 and 500");

            return false;
        }

        if (MipavUtil.testParameter(verticalLimitField.getText(), 0, 1)) {
            verticalDeletionLimit = Float.parseFloat(verticalLimitField.getText());
        } else {
            MipavUtil.displayError("Vertical face deletion limit must be between 0 and 1");

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
    
    //~ Inner Classes --------------------------------------------------------------------------------------------------

    /**
     * This class records parameters used in the algorithm's dialog.  It also sets up the dialog's GUI based on parameters parsed out while running it as part of a script.
     */
    private class DialogParameters extends AlgorithmParameters {
        private static final String FACE_ORIENTATION = "face_orientation";
        private static final String EXTRA_MMS_TO_DELETE = "mms_to_delete_from_face";
        private static final String VERT_DELETION_LIMIT = "vertical_deletion_limit_ratio";
        private static final String BET_DO_SPHERE_ESTIMATION = "bet_do_estimate_with_sphere";
        private static final String BET_IMG_INFLUENCE = "bet_image_influence";
        private static final String BET_STIFFNESS = "bet_stiffness";        
        
        /**
         * Creates a new DialogParameters object.  Called when recording the parameters for this algorithm.
         */
        public DialogParameters() {
            super();
        }

        /**
         * Creates a new DialogParameters object.  Called when setting up the dialog GUI when running a script.
         *
         * @param  parsedParams  The parsed table of parameters from the script being run.
         */
        public DialogParameters(ParameterTable parsedParams) {
            super(parsedParams);
        }

        /**
         * Perform any actions required after the running of the algorithm is complete.
         */
        public void doPostAlgorithmActions() {}

        /**
         * Set up the dialog GUI based on the parameters before running the algorithm as part of a script.
         */
        public void setGUIFromParams() {
            srcImage = retrieveInputImage();
            parentFrame = srcImage.getParentFrame();
            
            faceOrientation = getFaceOrientation(srcImage);
            int scriptFaceOrientation = params.getInt(FACE_ORIENTATION);
            if (faceOrientation == FACING_UNKNOWN) {
                faceOrientation = scriptFaceOrientation;
            }
            
            extraMMsToDelete = params.getInt(EXTRA_MMS_TO_DELETE);
            verticalDeletionLimit = params.getFloat(VERT_DELETION_LIMIT);
            estimateWithSphereBET = params.getBoolean(BET_DO_SPHERE_ESTIMATION);
            imageInfluenceBET = params.getFloat(BET_IMG_INFLUENCE);
            stiffnessBET = params.getFloat(BET_STIFFNESS);
        }

        /**
         * Store the parameters from the dialog to record the execution of this algorithm.
         * 
         * @throws  ParserException  If there is a problem creating one of the new parameters.
         */
        public void storeParamsFromGUI() throws ParserException {
            storeInputImage(srcImage);

            params.put(ParameterFactory.newInt(FACE_ORIENTATION, faceOrientation));
            params.put(ParameterFactory.newInt(EXTRA_MMS_TO_DELETE, extraMMsToDelete));
            params.put(ParameterFactory.newFloat(VERT_DELETION_LIMIT, verticalDeletionLimit));
            params.put(ParameterFactory.newBoolean(BET_DO_SPHERE_ESTIMATION, estimateWithSphereBET));
            params.put(ParameterFactory.newFloat(BET_IMG_INFLUENCE, imageInfluenceBET));
            params.put(ParameterFactory.newFloat(BET_STIFFNESS, stiffnessBET));
        }
    }
}
