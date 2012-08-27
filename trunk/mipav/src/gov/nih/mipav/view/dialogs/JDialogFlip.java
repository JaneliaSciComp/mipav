package gov.nih.mipav.view.dialogs;


import gov.nih.mipav.model.algorithms.*;
import gov.nih.mipav.model.algorithms.utilities.*;
import gov.nih.mipav.model.scripting.*;
import gov.nih.mipav.model.scripting.parameters.*;
import gov.nih.mipav.model.structures.*;

import gov.nih.mipav.view.*;

import java.awt.*;
import java.awt.event.*;

import java.util.*;
import javax.swing.*;


/**
 * Dialog to call the image flip. This dialog will not be visible because it does not require user input at this time.
 * It was made a dialog object because it may in the future require user input and to be consistent with the
 * dialog/algorithm paradigm. In should be noted, that the algorithms are executed in their own thread.** replaces image
 *
 * @version  1.0 July 17, 2000
 * @author   Matthew J. McAuliffe, Ph.D.
 */
public class JDialogFlip extends JDialogScriptableBase implements AlgorithmInterface, ScriptableActionInterface, ActionDiscovery {

    //~ Static fields/initializers -------------------------------------------------------------------------------------

    /** Use serialVersionUID for interoperability. */
    private static final long serialVersionUID = -5672670158596197276L;

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** Algorithm used by the dialog. */
    private AlgorithmFlip flipAlgo;

    /** Axis to flip around. */
    private int flipAxis;
    
    /** The object to be flipped */
    private int flipObject;

    /** Source image. */
    private ModelImage image = null; // source image

    /** DOCUMENT ME! */
    private String[] titles;

    /** DOCUMENT ME! */
    private ViewUserInterface userInterface;
    
    /** When checked, VOIs are flipped when image is flipped. */
    private JCheckBox flipVoiCheckbox;
    
    /** When checked, change orientation and origin upon flipping.
     *  When unchecked, orienation and origin remain the same.
     */
    private JCheckBox orientationOriginCheckBox;
    
    private boolean changeOrientationOrigin = true;
    
    /** Image is flipped by depth when selected */
    private JRadioButton flipAxisZRadioButton;
    
    /** Image is flipped horizontally when selected */
    private JRadioButton flipAxisYRadioButton;
    
    /** Image is flipped vertically when selected */
    private JRadioButton flipAxisXRadioButton;
    
    private JPanel optionsPanel;
    
    private boolean loadAxisDefaults = true;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Empty constructor needed for dynamic instantiation (used during scripting).
     */
    public JDialogFlip() { }

    /**
     * Sets the appropriate variables. Creates a dialog if flipObject is equal to AlgorithmFlip.IMAGE.  User
     * is required to input whether all VOIs should be flipped.
     *
     * @param  theParentFrame  Parent frame.
     * @param  im              Source image.
     * @param  flipAxis        Axis which image is to be flipped.
     */
    public JDialogFlip(Frame theParentFrame, ModelImage im, int flipAxis, int flipObject) {
        super(theParentFrame, false);

        setForeground(Color.black);
        userInterface = ViewUserInterface.getReference();
        this.flipAxis = flipAxis;
        this.flipObject = flipObject;
        this.image =im;
        if(flipObject == AlgorithmFlip.IMAGE || flipObject == AlgorithmFlip.IMAGE_AND_VOI) {
            init();
            loadAxisDefaults = false;
            loadDefaults();
            setVariables();
        }
        setForeground(Color.black);
        
        
    }

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * Processes button events.
     *
     * @param  event  event that triggers function
     */
    public void actionPerformed(ActionEvent event) 
    {
        String command = event.getActionCommand();
             
        if (command.equals("OK") && setVariables()) {
            callAlgorithm();
        } else if (command.equals("Cancel")) {
            dispose();
        } else if (command.equals("Help")) {
            //MipavUtil.showHelp("U4027");
            MipavUtil.showWebHelp("Flipping_images");
        } else {
            super.actionPerformed(event);
        }
    }

    // ************************************************************************
    // ************************** Algorithm Events ****************************
    // ************************************************************************

    /**
     * This method is required if the AlgorithmPerformed interface is implemented. It is called by the algorithms when
     * it has completed or failed to complete, so that the dialog can display the result image and/or clean up.
     *
     * @param  algorithm  Algorithm that caused the event.
     */
    public void algorithmPerformed(AlgorithmBase algorithm) {

        if (algorithm instanceof AlgorithmFlip) {
            
            if (Preferences.is(Preferences.PREF_SAVE_DEFAULTS) && (this.getOwner() != null) && !isScriptRunning()) {
                saveDefaults();
            }

            // These next lines set the titles in all frames where the source image is displayed to
            // image name so as to indicate that the image is now unlocked!
            // The image frames are enabled and then registered to the userinterface.
            Vector<ViewImageUpdateInterface> imageFrames = image.getImageFrameVector();

            for (int i = 0; i < imageFrames.size(); i++) {
                ((Frame) (imageFrames.elementAt(i))).setTitle(titles[i]);
                ((Frame) (imageFrames.elementAt(i))).setEnabled(true);

                if ((((Frame) (imageFrames.elementAt(i))) != parentFrame) && (parentFrame != null)) {
                    userInterface.registerFrame((Frame) (imageFrames.elementAt(i)));
                }
            }

            if (parentFrame != null) {
                userInterface.registerFrame(parentFrame);
            }

            image.notifyImageDisplayListeners(null, true);

            if (algorithm.isCompleted()) {
                insertScriptLine();
            }
        }
        setComplete(algorithm.isCompleted());
        flipAlgo.finalize();
        flipAlgo = null;

    }

    /**
     * Calls the algorithm.
     */
    public void callAlgorithm() {

        try {
            System.gc();

            // Make algorithm
            flipAlgo = new AlgorithmFlip(image, flipAxis, flipObject, changeOrientationOrigin);

            // This is very important. Adding this object as a listener allows the algorithm to
            // notify this object when it has completed of failed. See algorithm performed event.
            // This is made possible by implementing AlgorithmedPerformed interface
            flipAlgo.addListener(this);
            
            createProgressBar(image.getImageName(), flipAlgo);
            
            // Hide dialog
            setVisible(false);

            // These next lines set the titles in all frames where the source image is displayed to
            // "locked - " image name so as to indicate that the image is now read/write locked!
            // The image frames are disabled and then unregisted from the userinterface until the
            // algorithm has completed.
            if (image.getImageFrameVector() != null){
	            Vector<ViewImageUpdateInterface> imageFrames = image.getImageFrameVector();
	            titles = new String[imageFrames.size()];
	
	            for (int i = 0; i < imageFrames.size(); i++) {
	                titles[i] = ((Frame) (imageFrames.elementAt(i))).getTitle();
	                ((Frame) (imageFrames.elementAt(i))).setTitle("Locked: " + titles[i]);
	                ((Frame) (imageFrames.elementAt(i))).setEnabled(false);
	                userInterface.unregisterFrame((Frame) (imageFrames.elementAt(i)));
	            }
            }

            if (isRunInSeparateThread()) {

                // Start the thread as a low priority because we wish to still have user interface work fast.
                if (flipAlgo.startMethod(Thread.MIN_PRIORITY) == false) {
                    MipavUtil.displayError("A thread is already running on this object");
                }
            } else {
                flipAlgo.run();
            }
        } catch (OutOfMemoryError x) {
            System.gc();
            MipavUtil.displayError("Dialog VOI Extraction: unable to allocate enough memory");

            return;
        }
    }
    
    /**
     * {@inheritDoc}
     */
    protected void storeParamsFromGUI() throws ParserException {
        scriptParameters.storeInputImage(image);
        
        if (flipAxis == AlgorithmFlip.X_AXIS) {
            scriptParameters.getParams().put(ParameterFactory.newParameter("flip_axis", "X"));
        } else if (flipAxis == AlgorithmFlip.Y_AXIS) {
            scriptParameters.getParams().put(ParameterFactory.newParameter("flip_axis", "Y"));
        } else if (flipAxis == AlgorithmFlip.Z_AXIS) {
        	scriptParameters.getParams().put(ParameterFactory.newParameter("flip_axis", "Z"));
        }
        
        if(flipObject == AlgorithmFlip.IMAGE) {
            scriptParameters.getParams().put(ParameterFactory.newParameter("flip_object", "image"));
        } else if(flipObject == AlgorithmFlip.VOI_TYPE) {
            scriptParameters.getParams().put(ParameterFactory.newParameter("flip_object", "voi"));
        } else if(flipObject == AlgorithmFlip.IMAGE_AND_VOI) {
        	scriptParameters.getParams().put(ParameterFactory.newParameter("flip_object", "image_and_voi"));
        }
        
        if (changeOrientationOrigin) {
            scriptParameters.getParams().put(ParameterFactory.newParameter("orientation_origin", "change"));
        }
        else {
            scriptParameters.getParams().put(ParameterFactory.newParameter("orientation_origin", "preserve"));
        }
    }
    
    /**
     * Saves the default settings into the Preferences file.
     */
    public void saveDefaults() {
        String delim = ",";
        String defaultsString = flipAxis + delim;
        defaultsString += flipObject + delim;
        defaultsString += changeOrientationOrigin;

        Preferences.saveDialogDefaults(getDialogName(), defaultsString);
    }
    
    /**
     * {@inheritDoc}
     */
    protected void setGUIFromParams() {
        image = scriptParameters.retrieveInputImage();
        userInterface = ViewUserInterface.getReference();
        parentFrame = image.getParentFrame();
        
        
        String axisn = scriptParameters.getParams().getString("flip_axis");
        if (axisn.toUpperCase().equals("X")) {
            flipAxis = AlgorithmFlip.X_AXIS;
        } else if (axisn.toUpperCase().equals("Y")) {
            flipAxis = AlgorithmFlip.Y_AXIS;
        } else if (axisn.toUpperCase().equals("Z")) {
        	flipAxis = AlgorithmFlip.Z_AXIS;
        } else {
            throw new ParameterException("flip_axis", "Illegal axis parameter: " + axisn);
        }
        axisn = scriptParameters.getParams().getString("flip_object");
        if(axisn.equals("image")) {
            flipObject = AlgorithmFlip.IMAGE;
        }
        if(axisn.equals("voi")) {
            flipObject = AlgorithmFlip.VOI_TYPE;
        }
        if(axisn.equals("image_and_voi")) {
        	flipObject = AlgorithmFlip.IMAGE_AND_VOI;
        }

        axisn = scriptParameters.getParams().getString("orientation_origin");
        if (axisn != null) {
            if (axisn.equals("change")) {
                changeOrientationOrigin = true;
            }
            if (axisn.equals("preserve")) {
                changeOrientationOrigin = false;
            }
        }
    }
    
    /**
     * Loads the default settings from Preferences to set up the dialog.
     */
    public void loadDefaults() {
        String defaultsString = Preferences.getDialogDefaults(getDialogName());

        if ((defaultsString != null) && (optionsPanel != null)) {

            try {
                StringTokenizer st = new StringTokenizer(defaultsString, ",");
                flipAxis = MipavUtil.getInt(st);
                if (loadAxisDefaults) {
                    if (flipAxis == AlgorithmFlip.X_AXIS) {
                        flipAxisXRadioButton.setSelected(true);
                    }  
                    else if (flipAxis == AlgorithmFlip.Y_AXIS) {
                        flipAxisYRadioButton.setSelected(true);
                    }
                    else if((image.getNDims() > 2)&&(flipAxis == AlgorithmFlip.Z_AXIS)) {
                        flipAxisZRadioButton.setSelected(true);
                    }
                } // if (loadAxisDefaults)
                
                flipObject = MipavUtil.getInt(st);
                VOIVector vec = image.getVOIs();
                if((flipObject == AlgorithmFlip.IMAGE_AND_VOI) && (vec.size() > 0)) {
                    flipVoiCheckbox.setSelected(true);
                }
                else {
                    flipVoiCheckbox.setSelected(false);
                }

                orientationOriginCheckBox.setSelected(MipavUtil.getBoolean(st));
                
            } catch (Exception ex) {

                // since there was a problem parsing the defaults string, start over with the original defaults
                Preferences.debug("Resetting defaults for dialog: " + getDialogName());
                Preferences.removeProperty(getDialogName());
            }
        }
    }
    
    private void init() {
        setTitle("Flip Image");
        getContentPane().setLayout(new BorderLayout());

        optionsPanel = new JPanel(new GridLayout(1, 2));

        optionsPanel.setForeground(Color.black);
        optionsPanel.setBorder(buildTitledBorder("Options"));

        flipVoiCheckbox = new JCheckBox("Flip all VOIs.");
        flipVoiCheckbox.setFont(serif12);
        VOIVector vec = image.getVOIs();
        if(vec.size() > 0) {
            flipVoiCheckbox.setSelected(true);
        }
        flipVoiCheckbox.addItemListener(this);
        optionsPanel.add(flipVoiCheckbox);
        
        orientationOriginCheckBox = new JCheckBox("Change orientation and origin");
        orientationOriginCheckBox.setFont(serif12);
        orientationOriginCheckBox.setSelected(changeOrientationOrigin);    
        optionsPanel.add(orientationOriginCheckBox);
        
        JPanel flipAxisPanel = new JPanel(new GridBagLayout());

        flipAxisPanel.setForeground(Color.black);
        flipAxisPanel.setBorder(buildTitledBorder("Flip Axis"));

        
        
        flipAxisPanel.setForeground(Color.black);
        flipAxisPanel.setBorder(buildTitledBorder("Flip Axis"));
        
        flipAxisXRadioButton = new JRadioButton("Vertical (X Axis)");
        flipAxisXRadioButton.setFont(serif12);
        if (flipAxis == AlgorithmFlip.X_AXIS) {
            flipAxisXRadioButton.setSelected(true);
        }
        else {
            flipAxisXRadioButton.setSelected(false);
        }
        
        flipAxisYRadioButton = new JRadioButton("Horizontal (Y Axis)");
        flipAxisYRadioButton.setFont(serif12);
        if (flipAxis == AlgorithmFlip.Y_AXIS) {
            flipAxisYRadioButton.setSelected(true);
        }
        else {
            flipAxisYRadioButton.setSelected(false);
        }
        
        if (image.getNDims() >= 3) {
            flipAxisZRadioButton = new JRadioButton("Depth (Z axis)");
            flipAxisZRadioButton.setFont(serif12);
            if (flipAxis == AlgorithmFlip.Z_AXIS) {
                flipAxisZRadioButton.setSelected(true);    
            }
            else {
                flipAxisZRadioButton.setSelected(false);
            }
        } // if (image.getNDims() >= 3)
        
        ButtonGroup axisGroup = new ButtonGroup();
        axisGroup.add(flipAxisXRadioButton);
        axisGroup.add(flipAxisYRadioButton);
        if (image.getNDims() >= 3) {
            axisGroup.add(flipAxisZRadioButton);
        }
        
        JLabel orientIconLabel = new JLabel("Note: Image origin is in the upper left hand corner. Righthand coordinate system.",
                MipavUtil.getIcon("orient.gif"), JLabel.LEFT);
        
        GridBagConstraints gbc = new GridBagConstraints();
        gbc.gridwidth = 1;
        gbc.gridheight = 1;
        gbc.anchor = GridBagConstraints.WEST;
        gbc.weightx = 1;
        gbc.insets = new Insets(3, 3, 3, 3);
        gbc.gridx = 0;
        gbc.gridy = 0;
        gbc.fill = GridBagConstraints.VERTICAL;
        
        
        
        flipAxisPanel.add(flipAxisXRadioButton, gbc);
        gbc.gridx = 1;
        flipAxisPanel.add(flipAxisYRadioButton, gbc);
        if (image.getNDims() >= 3) {
            gbc.gridx = 2;
            flipAxisPanel.add(flipAxisZRadioButton, gbc);
        }
        gbc.gridx = 0;
        gbc.gridy = 1;
        gbc.gridwidth = GridBagConstraints.REMAINDER;
        flipAxisPanel.add(orientIconLabel, gbc);
        
        
        getContentPane().add(optionsPanel, BorderLayout.NORTH);
        getContentPane().add(flipAxisPanel, BorderLayout.CENTER);
        getContentPane().add(buildButtons(), BorderLayout.SOUTH);
        pack();
        //setResizable(false);
        setVisible(true);
    }
    
    private boolean setVariables()
    {
        if(flipVoiCheckbox.isSelected() && image.getVOIs().size() != 0) {
            flipObject = AlgorithmFlip.IMAGE_AND_VOI;
        }
        else {
            flipObject = AlgorithmFlip.IMAGE;
        }
        
        changeOrientationOrigin = orientationOriginCheckBox.isSelected();
        
        if(flipAxisXRadioButton.isSelected()) {
            flipAxis = AlgorithmFlip.X_AXIS;
        }
        else if(flipAxisYRadioButton.isSelected()) {
            flipAxis = AlgorithmFlip.Y_AXIS;
        }
        else {
            flipAxis = AlgorithmFlip.Z_AXIS;
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
                return new String("Utilities");
            }

            public String getDescription() {
                return new String("Flips an image.");
            }

            public String getDescriptionLong() {
                return new String("Flips an image. For flip_axis, choose either X, Y or Z. For " +
                		"orientation choose either preserve or change.");
            }

            public String getShortLabel() {
                return new String("Flip");
            }

            public String getLabel() {
                return new String("Flip Image");
            }

            public String getName() {
                return new String("Flip Image");
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
            table.put(new ParameterString("flip_axis", "X"));
            table.put(new ParameterString("flip_object", "image"));
            table.put(new ParameterString("orientation_origin", "change"));
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
                return image.getImageName();
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
