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
 */
public class JDialogFaceAnonymize extends JDialogScriptableBase
        implements AlgorithmInterface, ActionDiscovery, ScriptableActionInterface {


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


    //~ Instance fields ------------------------------------------------------------------------------------------------

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

    /** Button for blurring the face / skull. */
    private JRadioButton blurFaceRadio;

    /** Button for removing the face / skull. */
    private JRadioButton removeFaceRadio;

    /** Button for displaying the segmentation. */
    private JRadioButton showSegmentationRadio;

    /** The image that face anonymization will be performed on. */
    private ModelImage srcImage;

    private boolean blurFace = false;
    private boolean removeFace = false;
    private boolean showSegmentation = false;

    /**
     * Empty constructor needed for dynamic instantiation (used during scripting).
     */
    public JDialogFaceAnonymize() { }

    /**
     * Creates the face anonymizer dialog.
     *
     * @param  theParentFrame  Parent frame.
     * @param  im              Source image.
     */
    public JDialogFaceAnonymize(Frame theParentFrame, ModelImage im) {
        super(theParentFrame, false);
        parentFrame = theParentFrame;
        srcImage = im;
        faceOrientation = getFaceOrientation(srcImage);
        if (faceOrientation == FACING_UNKNOWN )
        {
        	initDirection();
        }
        else
        {
        	init();
        }
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
        	if ( faceOrientation == FACING_UNKNOWN )
        	{
        		if ( setVariablesDirection() )
        		{
        			init();
        		}
        	}
        	else if (setVariables()) {
                callAlgorithm();
            }
        } else if (command.equals("Cancel")) {
            dispose();
        } else if (command.equals("Help")) {
            //MipavUtil.showHelp("19014");
            MipavUtil.showWebHelp("Face_Anonymizer_(BET)#Applying_the_Face_Anonymizer_Algorithm");
        } else {
            super.actionPerformed(event);
        }
    }

    /**
     * This method is required if the AlgorithmPerformed interface is implemented. It is called by the algorithms when
     * it has completed or failed to to complete, so that the dialog can display the result image and/or clean up.
     *
     * @param  algorithm  Algorithm that caused the event.
     */
    public void algorithmPerformed(AlgorithmBase algorithm) {

        if ((algorithm instanceof AlgorithmSkullRemoval) && algorithm.isCompleted()) {
            insertScriptLine();
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
        return str;
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
    }

    /**
     * {@inheritDoc}
     */
    public void storeParamsFromGUI() throws ParserException {
        scriptParameters.storeInputImage(srcImage);

        scriptParameters.getParams().put(ParameterFactory.newInt(PARAM_FACE_ORIENTATION, faceOrientation));
    }

    /**
     * Calls the algorithm.
     */
    protected void callAlgorithm() {

        try {
            System.gc();
            defaceAlgo = new AlgorithmSkullRemoval(srcImage, faceOrientation);
            defaceAlgo.setOutputOption( blurFace, removeFace, showSegmentation );
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
    private void initDirection() {
        setTitle("Anonymize Face");
        getContentPane().setLayout(new BorderLayout());

        JPanel orientationPanel = new JPanel(new GridLayout(3, 2));
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

        JPanel mainPanel = new JPanel(new BorderLayout());
        mainPanel.add(orientationPanel, BorderLayout.NORTH);

        getContentPane().add(mainPanel, BorderLayout.NORTH);
        getContentPane().add(buildButtons(), BorderLayout.SOUTH);

        pack();
        //setResizable(false);
        setVisible(true);
    }

    

    /**
     * Makes the GUI elements of the dialog. Not called at present because it is not necessary.
     */
    private void init() {
        setTitle("Anonymize Face");
        getContentPane().setLayout(new BorderLayout());

        JPanel orientationPanel = new JPanel(new GridLayout(3, 2));
        orientationPanel.setBorder(MipavUtil.buildTitledBorder("Options"));

        blurFaceRadio = new JRadioButton("Blur face and skull regions.");
        blurFaceRadio.setFont(MipavUtil.font12);

        removeFaceRadio = new JRadioButton("Remove face and skull regions.");
        removeFaceRadio.setFont(MipavUtil.font12);

        showSegmentationRadio = new JRadioButton("Show segmentation results.");
        showSegmentationRadio.setFont(MipavUtil.font12);
        showSegmentationRadio.setSelected(true);

        ButtonGroup group = new ButtonGroup();
        group.add(blurFaceRadio);
        group.add(removeFaceRadio);
        group.add(showSegmentationRadio);

        orientationPanel.add(blurFaceRadio);
        orientationPanel.add(removeFaceRadio);
        orientationPanel.add(showSegmentationRadio);

        JPanel mainPanel = new JPanel(new BorderLayout());
        mainPanel.add(orientationPanel, BorderLayout.NORTH);

        getContentPane().removeAll();
        getContentPane().add(mainPanel, BorderLayout.NORTH);
        getContentPane().add(buildButtons(), BorderLayout.SOUTH);

        pack();
        setVisible(true);
    }

    
    
    

    
    /**
     * Use the GUI results to set up the variables needed to run the algorithm.
     *
     * @return  <code>true</code> if parameters set successfully, <code>false</code> otherwise.
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
     * @return  <code>true</code> if parameters set successfully, <code>false</code> otherwise.
     */
    private boolean setVariables() {

        blurFace = blurFaceRadio.isSelected();
        removeFace = removeFaceRadio.isSelected();
        showSegmentation = showSegmentationRadio.isSelected();

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
