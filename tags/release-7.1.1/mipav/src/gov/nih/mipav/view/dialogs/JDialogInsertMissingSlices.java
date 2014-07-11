package gov.nih.mipav.view.dialogs;


import gov.nih.mipav.model.algorithms.*;
import gov.nih.mipav.model.algorithms.utilities.*;
import gov.nih.mipav.model.scripting.*;
import gov.nih.mipav.model.scripting.parameters.*;
import gov.nih.mipav.model.structures.*;

import gov.nih.mipav.view.*;

import java.awt.*;
import java.awt.event.*;

import javax.swing.*;


/**
 * Creates the dialog to insert missing slices into an image. The dialog use the origin[2] for each slice to see if
 * there are any missing slices. If so, either an average or a blank can be inserted.
 */
public class JDialogInsertMissingSlices extends JDialogScriptableBase 
	implements AlgorithmInterface, ActionDiscovery {

    //~ Static fields/initializers -------------------------------------------------------------------------------------

    /** Use serialVersionUID for interoperability. */
    private static final long serialVersionUID = -5205189127854148892L;

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** true if no slices are missing. */
    private boolean allPresent;

    /** Radio button selected if inserted slices are a weighted average of surrounding slices. */
    private JRadioButton average;

    /** Radio button selected if inserted slices are blank. */
    private JRadioButton blank;

    /** Array of length totalSlices, false where slice is already present, true where slice must be inserted. */
    private boolean[] checkListInsert;

    /** DOCUMENT ME! */
    private boolean destFlag;

    /** DOCUMENT ME! */
    private ButtonGroup destinationGroup;

    /** Flag indicating if a new image is to be generated. */
    private int displayLoc;

    /** source image. */
    private ModelImage image;

    /** If true insert blank slices, if false insert weighted average slices. */
    private boolean insertBlank;

    /** DOCUMENT ME! */
    private int[] missingNumberArray;

    /** DOCUMENT ME! */
    private int missingPositions = 0;

    /** DOCUMENT ME! */
    private int[] missingSliceArray;

    /** DOCUMENT ME! */
    private int missingSlices;

    /** DOCUMENT ME! */
    private JRadioButton newImage;

    /** Number of slices in original 3D image. */
    private int nSlices;

    /** DOCUMENT ME! */
    private JRadioButton replaceImage;

    /** image create if new image button is selected. */
    private ModelImage resultImage = null;

    /** DOCUMENT ME! */
    private AlgorithmReplaceRemovedSlices rSliceAlgo;

    /** Number of slices that will be present in the 3D image after the missing slices have been inserted. */
    private int totalSlices;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Empty constructor needed for dynamic instantiation (used during scripting).
     */
    public JDialogInsertMissingSlices() { }

    /**
     * Creates new dialog for inserting a slice.
     *
     * @param  theParentFrame  Parent frame
     * @param  im              Source image
     */
    public JDialogInsertMissingSlices(Frame theParentFrame, ModelImage im) {
        super(theParentFrame, false);
        image = im; // set the image from the arguments to an image in this class
        init();
    }

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * Closes dialog box when the OK button is pressed and calls the algorithm.
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
        	//MipavUtil.showHelp("U4053");
            MipavUtil.showWebHelp("Slice_tools#Insert_Missing_Slices");
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

        if (algorithm instanceof AlgorithmReplaceRemovedSlices) {

            if (displayLoc == NEW) {

                if (rSliceAlgo.isCompleted() == true) {
                    resultImage = rSliceAlgo.getResultImage();

                    if (resultImage != null) {

                        try {

                            // put the new image into a new frame
                            new ViewJFrameImage(resultImage, null, new Dimension(25, 32));

                        } catch (OutOfMemoryError error) {
                            MipavUtil.displayError("Insert missing Slices reports: out of memory; " +
                                                   "unable to open a new frame");

                            return;
                        }
                    }
                }
            } else {

                if (rSliceAlgo.isCompleted() == true) {
                    image.notifyImageExtentsListeners();
                }
            }
        }

        if (algorithm.isCompleted() && (algorithm instanceof AlgorithmReplaceRemovedSlices)) {
            insertScriptLine();
        }

        algorithm.finalize();
        algorithm = null;
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
     * Accessor which lets you change the type of slice to be inserted.
     *
     * @param  insertBlank  the type of slice to be inserted (either AVERAGE_SLICE or BLANK_Slice)
     */
    public void setInsertBlank(boolean insertBlank) {
        this.insertBlank = insertBlank;
    }

    /**
     * Once all the necessary variables are set, call the Insert Slice algorithm based on what type of image this is and
     * whether or not there is a separate destination image.
     */
    protected void callAlgorithm() {

        if (allPresent) {
            MipavUtil.displayInfo(image.getImageName() + " has no missing slices");
            Preferences.debug(image.getImageName() + " has no missing slices\n");
            dispose();

            return;
        }

        try {


            if (displayLoc == NEW) {
                resultImage = (ModelImage) image.clone();
                resultImage.setImageName(image.getImageName() + "_insertedMisssingSlices");
            }

            // Make algorithm:
            if (displayLoc == REPLACE) {
                destFlag = false;
                rSliceAlgo = new AlgorithmReplaceRemovedSlices(image, checkListInsert, false, destFlag, insertBlank);
            } else {
                destFlag = true;
                rSliceAlgo = new AlgorithmReplaceRemovedSlices(resultImage, checkListInsert, false, destFlag,
                                                               insertBlank);
            }

            // This is very important. Adding this object as a listener allows the algorithm to
            // notify this object when it has completed or failed. See algorithm performed event.
            // This is made possible by implementing AlgorithmedPerformed interface
            rSliceAlgo.addListener(this);

            createProgressBar(image.getImageName(), rSliceAlgo);

            setVisible(false); // Hide dialog

            if (isRunInSeparateThread()) {

                // Start the thread as a low priority because we wish to still have user interface work fast.
                if (rSliceAlgo.startMethod(Thread.MIN_PRIORITY) == false) {
                    MipavUtil.displayError("A thread is already running on this object");
                }
            } else {
                rSliceAlgo.run();
            }
        } catch (OutOfMemoryError x) {

            if (resultImage != null) {
                resultImage.disposeLocal(); // Clean up image memory
                resultImage = null;
            }

            MipavUtil.displayError("Insert Missing Slices reports: unable to allocate enough memory");

            return;
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
        parentFrame = image.getParentFrame();

        findMissingSlices();

        if (!allPresent && scriptParameters.getParams().getBoolean(AlgorithmParameters.DO_OUTPUT_NEW_IMAGE)) {
            setDisplayLocNew();
        } else {
            setDisplayLocReplace();
        }

        setInsertBlank(scriptParameters.getParams().getBoolean("do_insert_blank_slices"));
    }

    /**
     * {@inheritDoc}
     */
    protected void storeParamsFromGUI() throws ParserException {
        scriptParameters.storeInputImage(image);
        scriptParameters.storeOutputImageParams(getResultImage(), (displayLoc == NEW));

        scriptParameters.getParams().put(ParameterFactory.newParameter("do_insert_blank_slices", insertBlank));
    }

    /**
     * DOCUMENT ME!
     */
    private void findMissingSlices() {
        int m;
        float averageSpacing;
        int z;
        float spacing;
        int numSlices;
        int i;
        int k;

        nSlices = image.getExtents()[2];

        float[] beginOrigin = image.getFileInfo(0).getOrigin();
        float[] endOrigin = image.getFileInfo(nSlices - 1).getOrigin();

        int sliceOriginIndex;

        if ((beginOrigin[0] != endOrigin[0]) && (beginOrigin[1] == endOrigin[1]) && (beginOrigin[2] == endOrigin[2])) {
            sliceOriginIndex = 0;
        } else if ((beginOrigin[0] == endOrigin[0]) && (beginOrigin[1] != endOrigin[1]) &&
                       (beginOrigin[2] == endOrigin[2])) {
            sliceOriginIndex = 1;
        } else {
            sliceOriginIndex = 2;
        }

        averageSpacing = (endOrigin[sliceOriginIndex] - beginOrigin[sliceOriginIndex]) / (nSlices - 1);

        // Count first slice
        totalSlices = 1;

        for (z = 0; z < (nSlices - 1); z++) {
            spacing = image.getFileInfo(z + 1).getOrigin()[sliceOriginIndex] -
                      image.getFileInfo(z).getOrigin()[sliceOriginIndex];
            numSlices = Math.max(1, Math.round(spacing / averageSpacing));

            if (numSlices >= 2) {
                missingPositions++;
            }

            if (numSlices == 2) {
                Preferences.debug("1 slice is missing between " + (z + 1) + " and " + (z + 2) + "\n");
            } else if (numSlices > 2) {
                Preferences.debug((numSlices - 1) + " are missing between " + (z + 1) + " and " + (z + 2) + "\n");
            }

            totalSlices += numSlices;
        }

        missingSlices = totalSlices - nSlices;
        missingSliceArray = new int[missingPositions];
        missingNumberArray = new int[missingPositions];

        allPresent = true;
        checkListInsert = new boolean[totalSlices];
        checkListInsert[0] = false;

        for (z = 0, m = 1, k = 0; z < (nSlices - 1); z++) {
            spacing = image.getFileInfo(z + 1).getOrigin()[sliceOriginIndex] -
                      image.getFileInfo(z).getOrigin()[sliceOriginIndex];
            numSlices = Math.max(1, Math.round(spacing / averageSpacing));

            if (numSlices >= 2) {
                missingSliceArray[k] = z;
                missingNumberArray[k++] = numSlices - 1;
            }

            for (i = 0; i < (numSlices - 1); i++) {
                checkListInsert[m++] = true;
                allPresent = false;
            }

            checkListInsert[m++] = false;
        }
    }

    /**
     * Sets up the GUI (panels, buttons, etc) and displays it on the screen.
     */
    private void init() {
        int i;
        int yPos = 0;
        JLabel[] statusLabel = new JLabel[10];
        int numStatusLabels = 0;

        for (i = 0; i < 10; i++) {
            statusLabel[i] = null;
        }

        setTitle("Insert missing slices");
        setForeground(Color.black);

        JPanel slicePanel = new JPanel(new GridBagLayout());
        slicePanel.setBorder(buildTitledBorder("Insert missing slices"));

        findMissingSlices();

        if (allPresent) {
            statusLabel[0] = new JLabel("No slices are missing");
            numStatusLabels = 1;
        } else if (missingPositions <= 10) {

            for (i = 0; i < missingPositions; i++) {

                if (missingNumberArray[i] == 1) {
                    statusLabel[i] = new JLabel("1 slice between " + (missingSliceArray[i] + 1) + " and " +
                                                (missingSliceArray[i] + 2) + " is missing");
                } else {
                    statusLabel[i] = new JLabel(missingNumberArray[i] + " slices between " +
                                                (missingSliceArray[i] + 1) + " and " + (missingSliceArray[i] + 2) +
                                                " are missing");
                }

            }

            numStatusLabels = missingPositions;
        } // else if (missingPositions <= 10)
        else {
            statusLabel[0] = new JLabel(missingSlices + " slices are missing");
            numStatusLabels = 1;
        }

        for (i = 0; i < numStatusLabels; i++) {
            statusLabel[i].setFont(serif12);
        }

        ButtonGroup sliceGroup = new ButtonGroup();
        average = new JRadioButton("Average", true);
        average.setFont(serif12);
        average.addActionListener(this);
        sliceGroup.add(average);

        blank = new JRadioButton("Blank", false);
        blank.setFont(serif12);
        blank.addActionListener(this);
        sliceGroup.add(blank);

        GridBagConstraints gbc = new GridBagConstraints();
        gbc.insets = new Insets(3, 3, 3, 3);

        gbc.gridx = 0;
        gbc.gridy = yPos++;
        gbc.weightx = 1;
        gbc.gridwidth = 1;
        gbc.anchor = GridBagConstraints.WEST;
        gbc.fill = GridBagConstraints.HORIZONTAL;

        for (i = 0; i < numStatusLabels; i++) {
            gbc.gridy = yPos++;
            slicePanel.add(statusLabel[i], gbc);
        }

        gbc.gridx = 0;
        gbc.gridy = yPos++;
        gbc.gridwidth = 1;
        slicePanel.add(average, gbc);

        gbc.gridx = 0;
        gbc.gridy = yPos++;
        slicePanel.add(blank, gbc);

        // destination goes in the left of the lower box
        JPanel destinationPanel = new JPanel();
        destinationPanel.setLayout(new BoxLayout(destinationPanel, BoxLayout.Y_AXIS));

        destinationPanel.setForeground(Color.black);
        destinationPanel.setBorder(buildTitledBorder("Destination"));

        destinationGroup = new ButtonGroup();
        newImage = new JRadioButton("New image", true);
        newImage.setFont(serif12);
        destinationGroup.add(newImage); // add the button to the grouping
        destinationPanel.add(newImage); // add the button to the component

        replaceImage = new JRadioButton("Replace image", false);
        replaceImage.setFont(serif12);
        destinationGroup.add(replaceImage); // add the button to the grouping
        destinationPanel.add(replaceImage); // add the button to the component

        gbc.gridy = yPos++;
        slicePanel.add(destinationPanel, gbc);

        // Only if the image is unlocked can it be replaced.
        if (image.getLockStatus() == ModelStorageBase.UNLOCKED) {
            replaceImage.setEnabled(true);
        } else {
            replaceImage.setEnabled(false);
        }

        JPanel buttonPanel = new JPanel();

        /*
         * buildOKButton(); buttonPanel.add(OKButton); buildCancelButton(); buttonPanel.add(cancelButton);
         */
        buttonPanel.add(buildButtons());

        getContentPane().add(slicePanel);
        getContentPane().add(buttonPanel, BorderLayout.SOUTH);
        pack();
        setVisible(true);

    }

    /**
     * Use the GUI results to set up the variables needed to run the algorithm.
     *
     * @return  <code>true</code> if parameters set successfully, <code>false</code> otherwise.
     */
    private boolean setVariables() {

        if (replaceImage.isSelected()) {
            displayLoc = REPLACE;
        } else if (newImage.isSelected()) {
            displayLoc = NEW;
        }

        if (average.isSelected()) {
            insertBlank = false;
        } else {
            insertBlank = true;
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
                return new String("Utilities.Slice tools");
            }

            public String getDescription() {
                return new String("Inserts averaged slices where slices have been removed." +
                		"Note: If do_insert_blank_slices is false, average surrounding slices to insert the missing slice. " +
                		"If true, insert a blank slice.");
            }

            public String getDescriptionLong() {
                return new String("Inserts averaged slices where slices have been removed." +
                		"Note: If do_insert_blank_slices is false, average surrounding slices to insert the missing slice. " +
                		"If true, insert a blank slice.");
            }

            public String getShortLabel() {
                return new String("InsertMissingSlices");
            }

            public String getLabel() {
                return new String("Insert Missing Slices");
            }

            public String getName() {
                return new String("Insert Missing Slices");
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
            
            //If false, average surrounding slices, if true, insert a blank slice
            table.put(new ParameterBoolean("do_insert_blank_slices", false));
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
