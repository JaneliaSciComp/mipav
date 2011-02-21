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
 * Dialog which replaces all occurances of one value in an image with another value. The value replaced may be NaN,
 * -Inf, Inf, or a real number. The replacement value must be a real number.
 */
public class JDialogReplaceValue extends JDialogScriptableBase implements AlgorithmInterface, ItemListener, ActionDiscovery, ScriptableActionInterface {

    //~ Static fields/initializers -------------------------------------------------------------------------------------

    /** Use serialVersionUID for interoperability. */
    private static final long serialVersionUID = -4321343995269931526L;

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** DOCUMENT ME! */
    private AlgorithmReplaceValue algoReplace;

    /** DOCUMENT ME! */
    private int displayLoc;

    /** DOCUMENT ME! */
    private ModelImage image; // source image

    /** DOCUMENT ME! */
    private double inputVal = 0;

    /** DOCUMENT ME! */
    private JRadioButton newImage;

    /** DOCUMENT ME! */
    private JTextField outputField;

    /** DOCUMENT ME! */
    private double outputVal = 0;

    /** DOCUMENT ME! */
    private String rangeString;

    /** DOCUMENT ME! */
    private Vector<Values> rangesVector = new Vector<Values>();

    /** DOCUMENT ME! */
    private JRadioButton replaceImage;

    /** DOCUMENT ME! */
    private ModelImage resultImage = null; // result image

    /** DOCUMENT ME! */
    private JComboBox setChoiceBox;

    /** DOCUMENT ME! */
    private JRadioButton setChoiceButton;

    /** DOCUMENT ME! */
    private JRadioButton userDefinedButton;

    /** DOCUMENT ME! */
    private JTextField userDefinedField;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Empty constructor used for running scripts.
     */
    public JDialogReplaceValue() { }

    /**
     * Construct the replace value dialog window and wait for user action.
     *
     * @param  frame  the window this dialog is attached to
     * @param  img    the image to process
     */
    public JDialogReplaceValue(Frame frame, ModelImage img) {
        super(frame, false);

        this.image = img;
        setTitle("Replace Pixel/Voxel Value");
        init();
    }

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * Invoked when an action occurs.
     *
     * @param  e  ActionEvent
     *
     * @todo   Implement this java.awt.event.ActionListener method
     */
    public void actionPerformed(ActionEvent e) {
        String cmd = e.getActionCommand();

        if (cmd.equals("OK")) {

            if (setVariables()) {
                callAlgorithm();
            }
        } else if (cmd.equals("Cancel")) {
            dispose();
        } else if (cmd.equals("Help")) {
            MipavUtil.showHelp("U4045");
        }
    }

    /**
     * If the destination image is not null, put in frame.
     *
     * @param  algo  AlgorithmBase algorithm that was performed
     */
    public void algorithmPerformed(AlgorithmBase algo) {

        if (algo instanceof AlgorithmReplaceValue) {

            if (algo.isCompleted()) {

                if (displayLoc == NEW) {

                    try {
                        new ViewJFrameImage(resultImage, null, new Dimension(610, 200));
                    } catch (OutOfMemoryError error) {
                        MipavUtil.displayError("Out of memory: unable to open new frame");
                    }
                } else {
                    image.notifyImageDisplayListeners(null, true);
                }
             // save the completion status for later
                setComplete(algoReplace.isCompleted());
                insertScriptLine();
            }

        }

        this.dispose();
    }

    /**
     * Handle selection changes to the user defined value/preset radio buttons.
     *
     * @param  event  the event to handle
     */
    public void itemStateChanged(ItemEvent event) {

        if (userDefinedButton.isSelected()) {
            userDefinedField.setEnabled(true);
            setChoiceBox.setEnabled(false);
        } else {
            setChoiceBox.setEnabled(true);
            userDefinedField.setEnabled(false);
        }
    }

    /**
     * Calls the algorithm.
     */
    protected void callAlgorithm() {
        setVisible(false);

        if (displayLoc == NEW) {
            resultImage = (ModelImage) image.clone();
            resultImage.setImageName(image.getImageName() + "_replace_val");
            algoReplace = new AlgorithmReplaceValue(resultImage, image, rangesVector, outputVal);
        } else {
            algoReplace = new AlgorithmReplaceValue(null, image, rangesVector, outputVal);
        }

        algoReplace.addListener(this);

        createProgressBar(image.getImageName(), algoReplace);

        if (isRunInSeparateThread()) {

            // Start the thread as a low priority because we wish to still have user interface work fast.
            if (algoReplace.startMethod(Thread.MIN_PRIORITY) == false) {
                MipavUtil.displayError("A thread is already running on this object");
            }
        } else {
            algoReplace.run();
        }
    }

    /**
     * Store the result image in the script runner's image table now that the action execution is finished.
     */
    protected void doPostAlgorithmActions() {

        if (displayLoc == NEW) {
            AlgorithmParameters.storeImageInRunner(resultImage);
        }
    }

    /**
     * {@inheritDoc}
     */
    protected void setGUIFromParams() {
        image = scriptParameters.retrieveInputImage();
        parentFrame = image.getParentFrame();

        if (scriptParameters.doOutputNewImage()) {
            displayLoc = NEW;
        } else {
            displayLoc = REPLACE;
        }

        if (scriptParameters.getParams().containsParameter("replace_value")) {
            inputVal = scriptParameters.getParams().getDouble("replace_value");
            rangesVector.addElement(new Values(inputVal));
        } else {
            rangeString = scriptParameters.getParams().getString("replace_value_range");
            parseRanges();
        }

        outputVal = scriptParameters.getParams().getDouble("replace_with_value");
    }

    /**
     * {@inheritDoc}
     */
    protected void storeParamsFromGUI() throws ParserException {
        scriptParameters.storeInputImage(image);
        scriptParameters.storeOutputImageParams(resultImage, (displayLoc == NEW));

        if (setChoiceButton.isSelected()) {
            scriptParameters.getParams().put(ParameterFactory.newParameter("replace_value", inputVal));
        } else {
            scriptParameters.getParams().put(ParameterFactory.newParameter("replace_value_range", rangeString));
        }

        scriptParameters.getParams().put(ParameterFactory.newParameter("replace_with_value", outputVal));
    }

    /**
     * Set up the dialog GUI.
     */
    private void init() {

        JPanel mainPanel = new JPanel();

        mainPanel.setLayout(new GridBagLayout());

        GridBagConstraints gbc = new GridBagConstraints();
        mainPanel.setBorder(buildTitledBorder("Identify initial value to be changed"));

        ButtonGroup group = new ButtonGroup();

        userDefinedButton = new JRadioButton("User defined value(s) and/or range(s): ", false);
        userDefinedButton.setFont(MipavUtil.font12);
        group.add(userDefinedButton);

        setChoiceButton = new JRadioButton("Presets: ", true);
        setChoiceButton.setFont(MipavUtil.font12);
        group.add(setChoiceButton);

        userDefinedButton.addItemListener(this);
        // setChoiceButton.addItemListener(this);

        String[] choices = new String[] { "NaN", "\u221E", "-\u221E" };
        setChoiceBox = new JComboBox(choices);
        setChoiceBox.setFont(MipavUtil.font12);

        userDefinedField = new JTextField(9);
        userDefinedField.setToolTipText("e.g.: 5.23, (100.2)-(200.95), (-23.4)-(-4.59)");

        // MipavUtil.makeNumericsOnly(userDefinedField, true, true);
        userDefinedField.setEnabled(false);

        gbc.anchor = GridBagConstraints.WEST;
        gbc.weightx = 1;
        gbc.gridx = 0;
        gbc.gridy = 0;
        mainPanel.add(userDefinedButton, gbc);

        gbc.gridx = 1;
        mainPanel.add(userDefinedField, gbc);

        gbc.gridx = 0;
        gbc.gridy = 1;
        mainPanel.add(setChoiceButton, gbc);

        gbc.gridx = 1;
        mainPanel.add(setChoiceBox, gbc);

        JPanel outputPanel = new JPanel(new GridBagLayout());
        outputPanel.setBorder(buildTitledBorder("Replace with"));

        outputField = new JTextField(15);
        outputField.setFont(MipavUtil.font12);
        MipavUtil.makeNumericsOnly(outputField, true, true);
        outputField.setText(Double.toString(image.getMax()));

        gbc.gridx = 0;
        gbc.gridy = 0;
        outputPanel.add(outputField, gbc);

        // Destination panel of VOI
        JPanel destinationPanel = new JPanel(new GridLayout(2, 1));
        destinationPanel.setForeground(Color.gray);
        destinationPanel.setBorder(buildTitledBorder("Destination"));

        ButtonGroup destinationGroup = new ButtonGroup();
        newImage = new JRadioButton("New image", true);
        newImage.setFont(serif12);
        destinationGroup.add(newImage);
        destinationPanel.add(newImage);

        replaceImage = new JRadioButton("Replace image", false);
        replaceImage.setFont(serif12);
        destinationGroup.add(replaceImage);
        destinationPanel.add(replaceImage); // Only if the image is unlocked can it be replaced.

        if (image.getLockStatus() == ModelStorageBase.UNLOCKED) {
            replaceImage.setEnabled(true);
        } else {
            replaceImage.setEnabled(false);
        }

        JPanel centerPanel = new JPanel();
        centerPanel.setLayout(new BoxLayout(centerPanel, BoxLayout.Y_AXIS));

        centerPanel.add(mainPanel);
        centerPanel.add(outputPanel);
        centerPanel.add(destinationPanel);

        JPanel buttonPanel = new JPanel();
        buttonPanel.add(buildButtons());

        getContentPane().add(centerPanel, BorderLayout.CENTER);
        getContentPane().add(buttonPanel, BorderLayout.SOUTH);

        pack();
        setVisible(true);
    }

    /**
     * DOCUMENT ME!
     *
     * @return  boolean
     */
    private boolean parseRanges() {
        boolean okay = true;

        StringTokenizer tok = new StringTokenizer(rangeString, ",");

        String range = null;

        double firstVal;
        double secondVal;
        int idx = 0;

        String firstHalf;
        String secondHalf;

        while (tok.hasMoreTokens()) {
            range = tok.nextToken();

            // might just be a single value
            try {
                firstVal = Double.parseDouble(range);
                rangesVector.addElement(new Values(firstVal));

            } catch (Exception e) {

                try {
                    range = range.trim();

                    // now see if it is a range
                    idx = range.indexOf(")");

                    firstHalf = range.substring(1, idx);
                    // System.err.println("first half: " + firstHalf);

                    firstVal = Double.parseDouble(firstHalf);

                    secondHalf = range.substring(range.lastIndexOf("(") + 1, range.lastIndexOf(")"));

                    // System.err.println("second half: " + secondHalf);
                    secondVal = Double.parseDouble(secondHalf);

                    if (firstVal >= secondVal) {
                        return false;
                    }

                    rangesVector.addElement(new Values(firstVal, secondVal));

                } catch (Exception nex) {
                    MipavUtil.displayWarning("Incorrectly formatted value/range:\n" +
                                             "e.g.: 5.23, (100.2)-(200.95), (-23.4)-(-4.59)");
                    userDefinedField.requestFocus();

                    return false;
                }


            }

        }

        return okay;
    }

    /**
     * Sets up the algorithm's parameters.
     *
     * @return  DOCUMENT ME!
     */
    private boolean setVariables() {

        if (replaceImage.isSelected()) {
            displayLoc = REPLACE;
        } else if (newImage.isSelected()) {
            displayLoc = NEW;
        }

        outputVal = Double.parseDouble(outputField.getText());

        if (setChoiceButton.isSelected()) {

            switch (setChoiceBox.getSelectedIndex()) {

                case 0:
                    inputVal = Double.NaN;
                    break;

                case 1:
                    inputVal = Double.POSITIVE_INFINITY;
                    break;

                case 2:
                    inputVal = Double.NEGATIVE_INFINITY;
                    break;
            }

            rangesVector.addElement(new Values(inputVal));
        } else {
            rangeString = userDefinedField.getText();

            return parseRanges();
            // inputVal = Double.parseDouble(userDefinedField.getText());
        }


        return true;
    }

    //~ Inner Classes --------------------------------------------------------------------------------------------------

    /**
     * DOCUMENT ME!
     */
    public class Values {

        /** DOCUMENT ME! */
        public double firstVal;

        /** DOCUMENT ME! */
        public boolean isRange = false;

        /** DOCUMENT ME! */
        public double secondVal;

        /**
         * Creates a new Values object.
         *
         * @param  single  DOCUMENT ME!
         */
        public Values(double single) {
            this.firstVal = single;
        }

        /**
         * Creates a new Values object.
         *
         * @param  first   DOCUMENT ME!
         * @param  second  DOCUMENT ME!
         */
        public Values(double first, double second) {
            this.firstVal = first;
            this.secondVal = second;
            this.isRange = true;
        }
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
                return new String("Replaces pixels within a certain value range");
            }

            public String getDescriptionLong() {
                return new String("Replaces pixels within a certain value range");
            }

            public String getShortLabel() {
                return new String("ReplacePix");
            }

            public String getLabel() {
                return new String("Replace Pixel");
            }

            public String getName() {
                return new String("Replace Pixel");
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
            table.put(new ParameterDouble("replace_value", 0));
            table.put(new ParameterDouble("replace_with_value", 0));
            table.put(new ParameterString("replace_value_range", ""));
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
        if (displayLoc == 1) {
                // algo produced a new result image
                return resultImage.getImageName();
            } else {
                // algo was done in place
                return image.getImageName();
            }
        
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
