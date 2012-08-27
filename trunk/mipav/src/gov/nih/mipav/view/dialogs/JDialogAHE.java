package gov.nih.mipav.view.dialogs;


import gov.nih.mipav.model.algorithms.*;
import gov.nih.mipav.model.file.*;
import gov.nih.mipav.model.scripting.*;
import gov.nih.mipav.model.scripting.parameters.*;
import gov.nih.mipav.model.structures.*;

import gov.nih.mipav.view.*;
import gov.nih.mipav.view.components.*;

import java.awt.*;
import java.awt.event.*;

import java.util.*;

import javax.swing.*;


/**
 * Adaptive Histogram Equlization.
 *
 * <p>Dialog to get user input, then call the algorithm. The user has the option to generate a new image or replace the
 * source image.</p>
 *
 * <p>In should be noted, that the algorithms are executed in their own thread.</p>
 *
 * @version  1.0; 17 February 2000
 * @author   parsonsd
 * @author   Matthew J. McAuliffe, Ph.D.
 */
public class JDialogAHE extends JDialogScriptableBase implements AlgorithmInterface, ActionDiscovery, ScriptableActionInterface {

    //~ Static fields/initializers -------------------------------------------------------------------------------------

    /** Use serialVersionUID for interoperability. */
    private static final long serialVersionUID = -6408786765504411315L;

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** DOCUMENT ME! */
    private AlgorithmAHE aheAlgo = null;

    /** DOCUMENT ME! */
    private boolean clamp = false;

    /** DOCUMENT ME! */
    private JCheckBox clampImage;

    /** DOCUMENT ME! */
    private JTextField clampingValue;

    /** DOCUMENT ME! */
    private int clampValue = 75; // If clamping checked, defaults to 75

    /** DOCUMENT ME! */
    private JLabel clampValueLabel;

    /** DOCUMENT ME! */
    private JPanelColorChannels colorPanel;

    /** DOCUMENT ME! */
    private JComboBox comboBoxHeightDiv;

    /** DOCUMENT ME! */
    private JComboBox comboBoxWidthDiv;

    /** or if the source image is to be replaced. */
    private ButtonGroup destinationGroup;

    /** DOCUMENT ME! */
    private int displayLoc; // Flag indicating if a new image is to be generated

    /** DOCUMENT ME! */
    private int heightDivisions; // divide image into number of images (say, into threes)

    /** DOCUMENT ME! */
    private ModelImage image; // source image

    /** DOCUMENT ME! */
    private JRadioButton newImage;

    /** DOCUMENT ME! */
    private JRadioButton replaceImage;

    /** DOCUMENT ME! */
    private ModelImage resultImage = null; // result image

    /** DOCUMENT ME! */
    private String[] titles;

    /** DOCUMENT ME! */
    private ViewUserInterface userInterface;

    /** DOCUMENT ME! */
    private int widthDivisions;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Empty constructor needed for dynamic instantiation (used during scripting).
     */
    public JDialogAHE() { }

    /**
     * Creates a new JDialogAHE object.
     *
     * @param  theParentFrame  Parent frame.
     * @param  im              Source image.
     */
    public JDialogAHE(Frame theParentFrame, ModelImage im) {
        super(theParentFrame, false);

        if (im.getType() == ModelImage.BOOLEAN) {
            MipavUtil.displayError("Source Image must NOT be Boolean");
            dispose();

            return;
        }

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
        Object source = event.getSource();
        String command = event.getActionCommand();

        if (command.equals("OK")) {

            if (setVariables()) {
                callAlgorithm();
            }
        } else if (source == clampImage) {

            if (clampImage.isSelected()) {
                clampingValue.setEnabled(true);
                clampValueLabel.setForeground(Color.black);
                clampingValue.setText(Integer.toString(clampValue));
            } else {
                clampingValue.setEnabled(false);
                clampValueLabel.setForeground(Color.gray);

                try {
                    clampValue = Integer.parseInt(clampingValue.getText());
                } catch (NumberFormatException npe) {

                    // user emptied text, then clicked checkbox.
                    clampValue = 75;
                }

                clampingValue.setText("100");
            }
        } else if (source == comboBoxWidthDiv) {
            String s = (String) comboBoxWidthDiv.getSelectedItem();
            comboBoxHeightDiv.setSelectedItem(s);
        } else if (command.equals("Cancel")) {
            dispose();
        } else if (command.equals("Help")) {
            //MipavUtil.showHelp("10029");
            MipavUtil.showWebHelp("Histogram_Equalization:_Regional_Adaptive");
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

        if (algorithm instanceof AlgorithmAHE) {
            image.clearMask();

            if (aheAlgo.isCompleted() && (resultImage != null)) {
                // The algorithm has completed and produced a new image to be displayed.

                updateFileInfo(image, resultImage);
                resultImage.clearMask();

                try {
                    new ViewJFrameImage(resultImage, null, new Dimension(610, 200));
                } catch (OutOfMemoryError error) {
                    System.gc();
                    MipavUtil.displayError("Out of memory: unable to open new frame");
                }
            } else if (resultImage == null) {

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
            } else if (resultImage != null) {

                // algorithm failed but result image still has garbage
                resultImage.disposeLocal(); // clean up memory
                resultImage = null;
                System.gc();
            }
        }

        if (algorithm.isCompleted()) {
            insertScriptLine();
        }

        aheAlgo.finalize();
        aheAlgo = null;
        dispose();
        System.gc();
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
     * Accessor that sets the clamp flag.
     *
     * @param  flag  <code>true</code> indicates clamp, <code>false</code> otherwise.
     */
    public void setClampFlag(boolean flag) {
        clamp = flag;
    }

    /**
     * Accessor that sets the clamping value.
     *
     * @param  value  Value to set clamping to.
     */
    public void setClampingValue(int value) {
        clampValue = value;
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
     * Accessor that sets the height divisions.
     *
     * @param  value  Value to set height divisions to.
     */
    public void setHeightDivisions(int value) {
        heightDivisions = value;
    }

    /**
     * Accessor that sets the width divisions.
     *
     * @param  value  Value to set width divisions to.
     */
    public void setWidthDivisions(int value) {
        widthDivisions = value;
    }

    /**
     * Once all the necessary variables are set, call the algorithm based on what type of image this is and whether or
     * not there is a separate destination image.
     */
    protected void callAlgorithm() {

        String name = makeImageName(image.getImageName(), "_AHE");
        int[] destExtents = new int[image.getNDims()];

        for (int i = 0; i < image.getNDims(); i++) {
            destExtents[i] = image.getExtents()[i]; // E[i] dim
        }

        if (displayLoc == NEW) {

            try {
                resultImage = (ModelImage) image.clone();
                resultImage.setImageName(name);

                if (resultImage.getNDims() == 2) {

                    if ((resultImage.getFileInfo()[0]).getFileFormat() == FileUtility.DICOM) {
                        ((FileInfoDicom) (resultImage.getFileInfo(0))).setSecondaryCaptureTags();
                    }
                } else {

                    if ((resultImage.getFileInfo()[0]).getFileFormat() == FileUtility.DICOM) {

                        for (int i = 0; i < resultImage.getExtents()[2]; i++) {
                            ((FileInfoDicom) (resultImage.getFileInfo(i))).setSecondaryCaptureTags();
                        }
                    }
                }

                // Make algorithm
                aheAlgo = new AlgorithmAHE(resultImage, image, heightDivisions, widthDivisions);
                aheAlgo.setRGBChannelFilter(colorPanel.isRedProcessingRequested(),
                                            colorPanel.isGreenProcessingRequested(),
                                            colorPanel.isBlueProcessingRequested());

                if (clamp) {
                    aheAlgo.setContrastLimited(true);
                    aheAlgo.setClipLevel(clampValue);
                }

                // This is very important. Adding this object as a listener allows the algorithm to
                // notify this object when it has completed or failed. See algorithm performed event.
                // This is made possible by implementing AlgorithmedPerformed interface
                aheAlgo.addListener(this);
                createProgressBar(image.getImageName(), aheAlgo);

                setVisible(false); // Hide dialog

                if (isRunInSeparateThread()) {

                    // Start the thread as a low priority because we wish to still have user interface work fast.
                    if (aheAlgo.startMethod(Thread.MIN_PRIORITY) == false) {
                        MipavUtil.displayError("A thread is already running on this object");
                    }
                } else {

                    aheAlgo.run();
                }
            } catch (OutOfMemoryError x) {
                MipavUtil.displayError("Dialog Adaptive Histogram Equalization: unable to allocate enough memory");

                if (resultImage != null) {
                    resultImage.disposeLocal(); // Clean up memory of result image
                    resultImage = null;
                }

                return;
            }
        } else { // displayLoc == REPLACE

            try {

                // No need to make new image space because the user has choosen to replace the source image
                // Make the algorithm class
                aheAlgo = new AlgorithmAHE(image, heightDivisions, widthDivisions);
                aheAlgo.setRGBChannelFilter(colorPanel.isRedProcessingRequested(),
                                            colorPanel.isGreenProcessingRequested(),
                                            colorPanel.isBlueProcessingRequested());

                if (clamp) {
                    aheAlgo.setContrastLimited(true);
                    aheAlgo.setClipLevel(clampValue);
                }

                // This is very important. Adding this object as a listener allows the algorithm to
                // notify this object when it has completed or failed. See algorithm performed event.
                // This is made possible by implementing AlgorithmedPerformed interface
                aheAlgo.addListener(this);

                createProgressBar(image.getImageName(), aheAlgo);

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
                    if (aheAlgo.startMethod(Thread.MIN_PRIORITY) == false) {
                        MipavUtil.displayError("A thread is already running on this object");
                    }
                } else {

                    aheAlgo.run();
                }
            } catch (OutOfMemoryError x) {
                MipavUtil.displayError("Dialog AHE: unable to allocate enough memory");

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
     * Takes a text field, and forces the textfield to accept numbers, backspace and delete-key entries.
     *
     * @param  txt  Text field to modify.
     */
    protected void makeNumericsOnly(JTextField txt) {
        txt.addKeyListener(new KeyAdapter() { // make the field
                public void keyTyped(KeyEvent evt) { // not accept letters

                    char ch = evt.getKeyChar();

                    if (((ch < '0') || (ch > '9')) && ((ch != KeyEvent.VK_DELETE) && (ch != KeyEvent.VK_BACK_SPACE))) {

                        // if is the case that ch is outside the bounds of a number AND it is the case that ch is
                        // neither a BS or a DE, then... key is not a digit or a deletion char
                        evt.consume();
                    }
                }
            });
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

        colorPanel = new JPanelColorChannels(image);
        scriptParameters.setColorOptionsGUI(colorPanel);

        clamp = scriptParameters.getParams().getBoolean("do_clamping");
        clampValue = scriptParameters.getParams().getInt("clamp_value");
        heightDivisions = scriptParameters.getParams().getInt("height_divisions");
        widthDivisions = scriptParameters.getParams().getInt("width_divisions");
    }

    /**
     * {@inheritDoc}
     */
    protected void storeParamsFromGUI() throws ParserException {
        scriptParameters.storeInputImage(image);
        scriptParameters.storeOutputImageParams(resultImage, (displayLoc == NEW));

        scriptParameters.storeColorOptions(colorPanel);

        scriptParameters.getParams().put(ParameterFactory.newParameter("do_clamping", clamp));
        scriptParameters.getParams().put(ParameterFactory.newParameter("clamp_value", clampValue));
        scriptParameters.getParams().put(ParameterFactory.newParameter("height_divisions", heightDivisions));
        scriptParameters.getParams().put(ParameterFactory.newParameter("width_divisions", widthDivisions));
    }

    /**
     * Creates the combo-box that allows user to select the number of divisions in the image when building the
     * histogram.
     */
    private void buildDivisionComboBoxes() {
        JTextField fld;
        String[] items = new String[] { "1", "2", "3", "4", "5" };
        comboBoxHeightDiv = new JComboBox(items);
        comboBoxWidthDiv = new JComboBox(items);
        comboBoxHeightDiv.setEditable(true);
        fld = (JTextField) comboBoxHeightDiv.getEditor().getEditorComponent();
        makeNumericsOnly(fld);
        fld.setColumns(4);
        fld.setHorizontalAlignment(JTextField.RIGHT);
        comboBoxWidthDiv.setEditable(true);
        fld = (JTextField) comboBoxWidthDiv.getEditor().getEditorComponent();
        makeNumericsOnly(fld);
        fld.setColumns(4);
        fld.setHorizontalAlignment(JTextField.RIGHT);
    }


    /**
     * Sets up the GUI (panels, buttons, etc) and displays it on the screen.
     */
    private void init() {

        setForeground(Color.black);
        setTitle("Regional Adaptive Histogram Equalization");

        // place everything setting up the equalisation into the setupBox
        Box setupBox = new Box(BoxLayout.Y_AXIS);

        // paramPanel holds all the "Parameters"
        JPanel paramPanel = new JPanel();

        // panel gets a grid layout
        GridBagLayout gbl = new GridBagLayout();
        GridBagConstraints gbc = new GridBagConstraints();
        gbc.anchor = GridBagConstraints.WEST;
        gbc.insets = new Insets(2, 0, 2, 0); // component width = minwidth + (2ipadx)
        paramPanel.setLayout(gbl);

        // panel is titled & etched
        paramPanel.setForeground(Color.black);

        // set the border ... "Parameters"
        paramPanel.setBorder(buildTitledBorder("Parameters"));

        buildDivisionComboBoxes();

        JLabel labelWidthDivision = new JLabel("Divisions of width");
        labelWidthDivision.setFont(serif12);
        labelWidthDivision.setForeground(Color.black);
        gbc.gridwidth = 1;
        gbc.fill = GridBagConstraints.HORIZONTAL;
        gbc.anchor = GridBagConstraints.WEST;
        gbc.weightx = 1;
        gbl.setConstraints(labelWidthDivision, gbc);
        paramPanel.add(labelWidthDivision);

        paramPanel.add(Box.createHorizontalStrut(10));

        comboBoxWidthDiv.setMaximumSize(comboBoxWidthDiv.getPreferredSize()); // don't let it get any bigger than what
                                                                              // it prefers
        comboBoxWidthDiv.setFont(serif12);
        comboBoxWidthDiv.addActionListener(this);
        gbc.gridwidth = GridBagConstraints.REMAINDER;
        gbc.anchor = GridBagConstraints.EAST;
        gbc.fill = GridBagConstraints.NONE;
        gbl.setConstraints(comboBoxWidthDiv, gbc);
        paramPanel.add(comboBoxWidthDiv);

        JLabel labelHeightDivision = new JLabel("Divisions of height");
        labelHeightDivision.setFont(serif12);
        labelHeightDivision.setForeground(Color.black);
        gbc.weightx = 1;
        gbc.gridwidth = 1;
        gbc.fill = GridBagConstraints.HORIZONTAL;
        gbc.anchor = GridBagConstraints.WEST;
        gbl.setConstraints(labelHeightDivision, gbc);
        paramPanel.add(labelHeightDivision);

        paramPanel.add(Box.createHorizontalStrut(10));

        comboBoxHeightDiv.setFont(serif12);
        gbc.gridwidth = GridBagConstraints.REMAINDER;
        gbc.anchor = GridBagConstraints.EAST;
        gbc.fill = GridBagConstraints.NONE;
        gbl.setConstraints(comboBoxHeightDiv, gbc);
        paramPanel.add(comboBoxHeightDiv);

        clampImage = new JCheckBox("Activate clamping");
        clampImage.addActionListener(this);
        clampImage.setFont(serif12);
        clampImage.setSelected(false);
        gbc.gridwidth = GridBagConstraints.REMAINDER;
        gbl.setConstraints(clampImage, gbc);
        paramPanel.add(clampImage);

        JPanel clampingPanel = new JPanel();
        gbc.anchor = GridBagConstraints.WEST;
        clampingPanel.setLayout(gbl);

        // panel is titled & etched
        clampingPanel.setBorder(buildTitledBorder("Clamping value"));

        clampValueLabel = new JLabel("Fraction of most frequent pixel intensity (%)");
        clampValueLabel.setFont(serif12);
        clampValueLabel.setForeground(Color.gray);
        gbc.gridwidth = 3;
        gbc.fill = GridBagConstraints.HORIZONTAL;
        gbl.setConstraints(clampValueLabel, gbc);
        clampingPanel.add(clampValueLabel);

        clampingPanel.add(Box.createHorizontalStrut(10));

        clampingValue = new JTextField("100"); // preset to no clamping
        clampingValue.setHorizontalAlignment(JTextField.RIGHT);
        makeNumericsOnly(clampingValue);

        // don't let it get any bigger than what it prefers:
        clampingValue.setMaximumSize(clampingValue.getPreferredSize());
        clampingValue.setFont(serif12);
        clampingValue.setColumns(4);
        clampingValue.setEnabled(false);
        gbc.fill = GridBagConstraints.NONE;
        gbc.gridwidth = GridBagConstraints.REMAINDER;
        gbc.anchor = GridBagConstraints.EAST;
        gbl.setConstraints(clampingValue, gbc);
        clampingPanel.add(clampingValue);

        gbc.gridwidth = GridBagConstraints.REMAINDER;
        gbc.anchor = GridBagConstraints.WEST;
        gbc.fill = GridBagConstraints.HORIZONTAL;
        gbl.setConstraints(clampingPanel, gbc);
        paramPanel.add(clampingPanel);

        setupBox.add(paramPanel);

        // color channel selection goes here...
        colorPanel = new JPanelColorChannels(image);
        setupBox.add(colorPanel);

        Box lowerBox = new Box(BoxLayout.X_AXIS);

        // destination goes in the left of the lower box
        JPanel destinationPanel = new JPanel();
        Box destinationBox = new Box(BoxLayout.Y_AXIS);

        // destination panel setup
        destinationPanel.setForeground(Color.black);
        destinationPanel.setBorder(buildTitledBorder("Destination"));

        destinationGroup = new ButtonGroup();
        newImage = new JRadioButton("New image", true);
        newImage.setFont(serif12);
        newImage.setEnabled(true);
        destinationGroup.add(newImage); // add the button to the grouping
        destinationBox.add(newImage); // add the button to the component

        replaceImage = new JRadioButton("Replace image", false);
        replaceImage.setFont(serif12);
        replaceImage.setEnabled(true);
        destinationGroup.add(replaceImage); // add the button to the grouping
        destinationBox.add(replaceImage); // add the button to the component
        destinationPanel.add(destinationBox);

        lowerBox.add(destinationPanel);
        setupBox.add(lowerBox); // place lowerBox into the setupBox

        mainDialogPanel.add(setupBox, BorderLayout.CENTER); // put the setupBox into the dialog

        // Only if the image is unlocked can it be replaced.
        if (image.getLockStatus() == ModelStorageBase.UNLOCKED) {
            replaceImage.setEnabled(true);
        } else {
            replaceImage.setEnabled(false);
        }

        // Build the Panel that holds the OK and CANCEL Buttons
        JPanel OKCancelPanel = buildButtons();

        mainDialogPanel.add(OKCancelPanel, BorderLayout.SOUTH);

        getContentPane().add(mainDialogPanel);

        pack();
        setVisible(true);
        setResizable(false);
        System.gc();

    }

    /**
     * Use the GUI results to set up the variables needed to run the algorithm.
     *
     * @return  <code>true</code> if parameters set successfully, <code>false</code> otherwise.
     */
    private boolean setVariables() {
        JTextField fld;
        int maxH = image.getExtents()[1] / 2; // won't allow this side to be divided into finer
        int maxW = image.getExtents()[0] / 2; // groupings than these values along the X & Y axes

        if (replaceImage.isSelected()) {
            displayLoc = REPLACE;
        } else if (newImage.isSelected()) {
            displayLoc = NEW;
        }

        if (clampImage.isSelected()) {

            try {

                if ((Integer.parseInt(clampingValue.getText()) >= 100) ||
                        (Integer.parseInt(clampingValue.getText()) < 1)) {
                    MipavUtil.displayError("Value must be >= 1 && <= 99");
                    clampingValue.requestFocus();
                    clampingValue.selectAll();

                    return false;
                } else {
                    clamp = true;
                    clampValue = Integer.parseInt(clampingValue.getText());
                }
            } catch (NumberFormatException nfe) { // when asked to clamp but text field is left blank
                MipavUtil.displayError("Clipping value must be >= 1 && <= 99");
                clampingValue.requestFocus();
                clampingValue.selectAll();

                return false;
            }
        }

        try {
            heightDivisions = Integer.parseInt((String) comboBoxHeightDiv.getSelectedItem());
            widthDivisions = Integer.parseInt((String) comboBoxWidthDiv.getSelectedItem());

            if (heightDivisions >= maxH) {
                MipavUtil.displayError("Too many divisions.  Choose a number smaller than " + maxH);
                fld = (JTextField) (comboBoxHeightDiv.getEditor().getEditorComponent());
                fld.requestFocus();
                fld.selectAll();

                return false;
            }

            if (widthDivisions >= maxW) {
                MipavUtil.displayError("Too many divisions.  Choose a number smaller than " + maxW);
                fld = (JTextField) (comboBoxWidthDiv.getEditor().getEditorComponent());
                fld.requestFocus();
                fld.selectAll();

                return false;
            }

        } catch (NumberFormatException nfe) {
            MipavUtil.displayError("Image Division must be a number");

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
                return new String("Algorithms.Histogram tools");
            }

            public String getDescription() {
                return new String("Applies a Regional Adaptive Histogram Equalization filter.");
            }

            public String getDescriptionLong() {
                return new String("Applies a Regional Adaptive Histogram Equalization filter.");
            }

            public String getShortLabel() {
                return new String("AHE");
            }

            public String getLabel() {
                return new String("Regional Adaptive Histogram Equalization");
            }

            public String getName() {
                return new String("Regional Adaptive Histogram Equalization");
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
            table.put(new ParameterBoolean(AlgorithmParameters.DO_PROCESS_WHOLE_IMAGE, true));
            table.put(new ParameterList(AlgorithmParameters.DO_PROCESS_RGB, Parameter.PARAM_BOOLEAN, "true,true,true"));
            table.put(new ParameterBoolean("do_clamping", false));
            table.put(new ParameterInt("clamp_value", 75));
            table.put(new ParameterInt("height_divisions", 1));
            table.put(new ParameterInt("width_divisions", 1));
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
