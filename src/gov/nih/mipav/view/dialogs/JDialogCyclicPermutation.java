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
 * Creates the dialog to cyclically permute pixels around the image.
 *
 * <p>User selects:</p>
 *
 * <ol>
 *   <li>shiftX</li>
 *   <li>shiftY</li>
 *   <li>shiftZ for 3D and 4D</li>
 *   <li>shiftT for 4D</li>
 * </ol>
 *
 * <p>A new image or replacement of the old image may be selected.</p>
 */
public class JDialogCyclicPermutation extends JDialogScriptableBase implements AlgorithmInterface, ScriptableActionInterface, ActionDiscovery {

    //~ Static fields/initializers -------------------------------------------------------------------------------------

    /** Use serialVersionUID for interoperability. */
    //private static final long serialVersionUID;

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** DOCUMENT ME! */
    private JTextField shiftTInput;

    /** DOCUMENT ME! */
    private int shiftT = 0;

    /** DOCUMENT ME! */
    private AlgorithmCyclicPermutation cpAlgo;

    /** DOCUMENT ME! */
    private ButtonGroup destinationGroup;

    /** DOCUMENT ME! */
    private int displayLoc; // Flag indicating if a new image is to be generated

    // or if the source image is to be replaced

    /** DOCUMENT ME! */
    private ModelImage image; // source image

    /** DOCUMENT ME! */
    private int shiftX = 0;

    /** DOCUMENT ME! */
    private JTextField shiftXInput;

    /** DOCUMENT ME! */
    private JRadioButton newImage;

    /** DOCUMENT ME! */
    private JRadioButton replaceImage;

    /** DOCUMENT ME! */
    private ModelImage resultImage = null; // result image

    /** DOCUMENT ME! */
    private int shiftY = 0;

    /** DOCUMENT ME! */
    private JTextField shiftYInput;

    /** DOCUMENT ME! */
    private String[] titles; // title of the frame shown when image is NULL

    /** DOCUMENT ME! */
    private JTextField shiftZInput;

    /** DOCUMENT ME! */
    private int shiftZ = 0;

    /** DOCUMENT ME! */
    private ViewUserInterface userInterface;
    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Empty constructor needed for dynamic instantiation (used during scripting).
     */
    public JDialogCyclicPermutation() { }

    /**
     * Constructor that makes dialog and displays it.
     *
     * @param  theParentFrame  Parent frame.
     * @param  im              Source image.
     */
    public JDialogCyclicPermutation(Frame theParentFrame, ModelImage im) {
        super(theParentFrame, false);
        image = im;
        userInterface = ViewUserInterface.getReference();
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

        if (command.equals("OK")) {

            if (setVariables()) {
                callAlgorithm();
            }
        } else if (command.equals("Cancel")) {
            dispose();
        } else if (command.equals("Help")) {
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

        if (algorithm instanceof AlgorithmCyclicPermutation) {

            if ((cpAlgo.isCompleted() == true) && (resultImage != null)) { // in StoreInDest; "new Image"

                try {
                    new ViewJFrameImage(resultImage, null, new Dimension(610, 200));
                } catch (OutOfMemoryError error) {
                    MipavUtil.displayError("Out of memory: unable to open new frame");
                }

                insertScriptLine();

            } else if ((cpAlgo.isCompleted() == true) && (resultImage == null)) {

            	// These next lines set the titles in all frames where the source image is displayed to
                // image name so as to indicate that the image is now unlocked!
                // The image frames are enabled and then registered to the userinterface.
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

                insertScriptLine();

            } else if (cpAlgo.isCompleted() == false) {

                // algorithm failed but result image still has garbage
                if (resultImage != null) {
                    resultImage.disposeLocal(); // clean up memory
                }

                resultImage = null;

                Vector<ViewImageUpdateInterface> imageFrames = image.getImageFrameVector();

                for (int i = 0; i < imageFrames.size(); i++) {
                    ((Frame) (imageFrames.elementAt(i))).setTitle(titles[i]);
                    ((Frame) (imageFrames.elementAt(i))).setEnabled(true);

                    if ((((Frame) (imageFrames.elementAt(i))) != parentFrame) && (parentFrame != null)) {
                        userInterface.registerFrame((Frame) (imageFrames.elementAt(i)));
                    }
                }
            }
        }
        setComplete(cpAlgo.isCompleted());
        cpAlgo.finalize();
        cpAlgo = null;
        dispose();
    }

    /**
     * Accessor that returns the image after adding image margins.
     *
     * @return  The result image.
     */
    public ModelImage getResultImage() {
        return resultImage;
    }

    /**
     * Accessor that sets the shiftT value.
     *
     * @param  x  Value to set shiftT value to.
     */
    public void setShiftT(int x) {
        shiftT = x;
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
     * Accessor that sets the shiftX value.
     *
     * @param  x  Value to set shiftX value to.
     */
    public void setShiftX(int x) {
        shiftX = x;
    }

    /**
     * Accessor that sets the shiftY value.
     *
     * @param  x  Value to set shiftY value to.
     */
    public void setShiftY(int x) {
        shiftY = x;
    }

    /**
     * Accessor that sets the shiftZ value.
     *
     * @param  x  Value to set shiftZ value to.
     */
    public void setShiftZ(int x) {
        shiftZ = x;
    }

    /**
     * DOCUMENT ME!
     */
    protected void callAlgorithm() {

    	if (displayLoc == NEW) {
    		try {
                // Make result image
                resultImage = new ModelImage(image.getType(), image.getExtents(),
                                             makeImageName(image.getImageName(), "_cyclicPermutation"));

                
                cpAlgo = new AlgorithmCyclicPermutation(resultImage, image, shiftX, shiftY, shiftZ, shiftT);

                // This is very important. Adding this object as a listener allows the algorithm to
                // notify this object when it has completed of failed. See algorithm performed event.
                // This is made possible by implementing AlgorithmedPerformed interface
                cpAlgo.addListener(this);

                createProgressBar(image.getImageName(), cpAlgo);

                // Hide the dialog since the algorithm is about to run.
                setVisible(false);

                if (isRunInSeparateThread()) {

                    // Start the thread as a low priority because we wish to still have user interface work fast.
                    if (cpAlgo.startMethod(Thread.MIN_PRIORITY) == false) {
                        MipavUtil.displayError("A thread is already running on this object");
                    }
                } else {

                    cpAlgo.run();
                }
            } catch (OutOfMemoryError x) {
                MipavUtil.displayError("Dialog CyclicPermutation: unable to allocate enough memory");

                return;
            }
        } // if (displayLoc == NEW)
        else { // displayLoc == REPLACE

            try {
                
                cpAlgo = new AlgorithmCyclicPermutation(null, image, shiftX, shiftY, shiftZ, shiftT);
                cpAlgo.addListener(this);

                createProgressBar(image.getImageName(), cpAlgo);

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
                    if (cpAlgo.startMethod(Thread.MIN_PRIORITY) == false) {
                        MipavUtil.displayError("A thread is already running on this object");
                    }
                } else {

                    cpAlgo.run();
                }

            } catch (OutOfMemoryError x) {
                MipavUtil.displayError("Dialog CyclicPermutation: unable to allocate enough memory");

                return;
            }

        } // else displayLoc == REPLACE
    }

    /**
     * When one of the text inputs has been left blank, trying to convert them to ints results in throwing a null
     * pointer exception. This method determines which one of the JTextFields threw the null pointer Exception.
     *
     * @return  The text field that returned null.
     */
    protected JTextField determineNull() {
        String t;

        try {
            t = shiftXInput.getText();

            if (t.equals("")) {
                return shiftXInput;
            }

            t = shiftYInput.getText();

            if (t.equals("")) {
                return shiftYInput;
            }

            if (image.getNDims() > 2) {
	            t = shiftZInput.getText();
	
	            if (t.equals("")) {
	                return shiftZInput;
	            }
            }

            if (image.getNDims() > 3) {
	            t = shiftTInput.getText();
	
	            if (t.equals("")) {
	                return shiftTInput;
	            }
            }

            return shiftXInput;
        } catch (NullPointerException npe) {
            MipavUtil.displayError("JDialogCyclicPermutation reports: Unknown Error");

            return shiftXInput; // gotta have some thing returned
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

        if (scriptParameters.doOutputNewImage()) {
            setDisplayLocNew();
        } else {
            setDisplayLocReplace();
        }

        setShiftX(scriptParameters.getParams().getInt("x_shift"));
        setShiftY(scriptParameters.getParams().getInt("y_shift"));
        setShiftZ(scriptParameters.getParams().getInt("z_shift"));
        setShiftT(scriptParameters.getParams().getInt("t_shift"));

        
    }

    /**
     * {@inheritDoc}
     */
    protected void storeParamsFromGUI() throws ParserException {
        scriptParameters.storeInputImage(image);
        scriptParameters.storeOutputImageParams(resultImage, (displayLoc == NEW));

        scriptParameters.getParams().put(ParameterFactory.newParameter("x_shift", shiftX));
        scriptParameters.getParams().put(ParameterFactory.newParameter("y_shift", shiftY));
        scriptParameters.getParams().put(ParameterFactory.newParameter("z_shift", shiftZ));
        scriptParameters.getParams().put(ParameterFactory.newParameter("t_shift", shiftT));
    }

    /**
     * Initializes the GUI components and places them in the dialog.
     */
    private void init() {
        setTitle("Cyclic Permutation");
        setSize(350, 230);
        setForeground(Color.black);
        int nDims = image.getNDims();

        Box contentBox = new Box(BoxLayout.Y_AXIS);
        JPanel optionPanel = new JPanel();

        // make border
        optionPanel.setBorder(buildTitledBorder("Shifts Around Image"));
        contentBox.add(optionPanel);

        // set layout
        GridBagLayout gbl = new GridBagLayout();
        GridBagConstraints gbc = new GridBagConstraints();
        optionPanel.setLayout(gbl);

        gbc.anchor = GridBagConstraints.NORTHWEST;
        // make content, place into layout

        // left
        optionPanel.add(Box.createHorizontalStrut(10));

        JLabel shiftXLabel = new JLabel("X Shift:");
        shiftXLabel.setFont(serif12);
        shiftXLabel.setForeground(Color.black);
        shiftXLabel.setRequestFocusEnabled(false);
        gbc.gridwidth = 2;
        gbl.setConstraints(shiftXLabel, gbc);
        optionPanel.add(shiftXLabel);
        optionPanel.add(Box.createHorizontalStrut(10));

        shiftXInput = new JTextField("0", 4);
        shiftXInput.addActionListener(this);
        MipavUtil.makeNumericsOnly(shiftXInput, false);

        gbc.gridwidth = GridBagConstraints.REMAINDER;
        gbl.setConstraints(shiftXInput, gbc);
        optionPanel.add(shiftXInput);

        // right
        optionPanel.add(Box.createHorizontalStrut(10));

        JLabel shiftYLabel = new JLabel("Y Shift:");
        shiftYLabel.setFont(serif12);
        shiftYLabel.setForeground(Color.black);
        shiftYLabel.setRequestFocusEnabled(false);
        gbc.gridwidth = 2;
        gbl.setConstraints(shiftYLabel, gbc);
        optionPanel.add(shiftYLabel);
        optionPanel.add(Box.createHorizontalStrut(10));

        shiftYInput = new JTextField("0", 4);
        shiftYInput.addActionListener(this);
        MipavUtil.makeNumericsOnly(shiftYInput, false);

        gbc.gridwidth = GridBagConstraints.REMAINDER;
        gbl.setConstraints(shiftYInput, gbc);
        optionPanel.add(shiftYInput);

        // shiftZ
        optionPanel.add(Box.createHorizontalStrut(10));

        JLabel shiftZLabel = new JLabel("Z Shift:");
        shiftZLabel.setFont(serif12);
        shiftZLabel.setForeground(Color.black);
        shiftZLabel.setRequestFocusEnabled(false);
        if (nDims < 3) {
        	shiftZLabel.setEnabled(false);
        }
        gbc.gridwidth = 2;
        gbl.setConstraints(shiftZLabel, gbc);
        optionPanel.add(shiftZLabel);
        optionPanel.add(Box.createHorizontalStrut(10));
        shiftZInput = new JTextField("0", 4);
        if (nDims >= 3) {
            shiftZInput.addActionListener(this);
        }
        else {
        	shiftZInput.setEnabled(false);
        }
        MipavUtil.makeNumericsOnly(shiftZInput, false);
        gbc.gridwidth = GridBagConstraints.REMAINDER;
        gbl.setConstraints(shiftZInput, gbc);
        optionPanel.add(shiftZInput);

        // shiftT
        optionPanel.add(Box.createHorizontalStrut(10));

        JLabel shiftTLabel = new JLabel("T Shift:");
        shiftTLabel.setFont(serif12);
        shiftTLabel.setForeground(Color.black);
        shiftTLabel.setRequestFocusEnabled(false);
        if (nDims < 4) {
        	shiftTLabel.setEnabled(false);
        }
        gbc.gridwidth = 2;
        gbl.setConstraints(shiftTLabel, gbc);
        optionPanel.add(shiftTLabel);
        optionPanel.add(Box.createHorizontalStrut(10));
        shiftTInput = new JTextField("0", 4);
        if (nDims >= 4) {
            shiftTInput.addActionListener(this);
        }
        else {
        	shiftTInput.setEnabled(false);
        }
        MipavUtil.makeNumericsOnly(shiftTInput, false);
        gbc.gridwidth = GridBagConstraints.REMAINDER;
        gbl.setConstraints(shiftTInput, gbc);
        optionPanel.add(shiftTInput);

       

        // image destination select
        JPanel destPanel = new JPanel(); // panel carries no content but box & border
        destPanel.setBorder(buildTitledBorder("Select Destination"));

        Box destinationBox = new Box(BoxLayout.Y_AXIS);

        destinationGroup = new ButtonGroup();
        newImage = new JRadioButton("New Image", true);
        newImage.setFont(serif12);
        newImage.addActionListener(this);
        destinationGroup.add(newImage);
        destinationBox.add(newImage);
        newImage.setEnabled(true);

        replaceImage = new JRadioButton("Replace Image", false);
        replaceImage.setFont(serif12);
        replaceImage.addActionListener(this);
        destinationGroup.add(replaceImage);
        destinationBox.add(replaceImage);
        replaceImage.setEnabled(true);
        destPanel.add(destinationBox);
        contentBox.add(destPanel);

        contentBox.add(buildButtons());

        mainDialogPanel.add(contentBox);
        getContentPane().add(mainDialogPanel);

        pack();
        setVisible(true);
    }

    /**
     * Use the GUI results to set up the variables needed to run the algorithm.
     *
     * @return  <code>true</code> if parameters set successfully, <code>false</code> otherwise.
     */
    private boolean setVariables() {

        try {
            shiftX = Integer.parseInt(shiftXInput.getText()); // in pixels
            shiftY = Integer.parseInt(shiftYInput.getText()); // in pixels
            if (image.getNDims() > 2) {
                shiftZ = Integer.parseInt(shiftZInput.getText()); // in pixels
            }
            if (image.getNDims() > 3) {
                shiftT = Integer.parseInt(shiftTInput.getText()); // in pixels
            }
        } catch (NumberFormatException nfe) {

            // an empty text-field.  decide which one is empty, then alert the user to correct
            JTextField t = determineNull();
            MipavUtil.displayError("Improper number!");
            t.requestFocus();
            t.selectAll();

            return false;
        }

        if (newImage.isSelected()) {
            displayLoc = NEW;
        } else {
            displayLoc = REPLACE;
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
                return new String("Utilities.CyclicPermutation");
            }

            public String getDescription() {
                return new String("Cyclically permutes pixels based on shift inputs.");
            }

            public String getDescriptionLong() {
                return new String("Cyclically permutes pixels based on shift inputs.");            }

            public String getShortLabel() {
                return new String("CyclicPermutation");
            }

            public String getLabel() {
                return new String("Cyclic Permutation");
            }

            public String getName() {
                return new String("Cyclic Permutation");
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
            table.put(new ParameterInt("x_shift", 0));
            table.put(new ParameterInt("y_shift", 0));
            table.put(new ParameterInt("z_shift", 0));
            table.put(new ParameterInt("t_shift", 0));
            table.put(new ParameterBoolean(AlgorithmParameters.DO_OUTPUT_NEW_IMAGE, true));            } catch (final ParserException e) {
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
