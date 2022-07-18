package gov.nih.mipav.view.dialogs;


import gov.nih.mipav.model.algorithms.AlgorithmBRISK;
import gov.nih.mipav.model.algorithms.AlgorithmBase;
import gov.nih.mipav.model.algorithms.AlgorithmInterface;
import gov.nih.mipav.model.scripting.ParserException;
import gov.nih.mipav.model.scripting.ScriptableActionInterface;
import gov.nih.mipav.model.scripting.parameters.ParameterExternalImage;
import gov.nih.mipav.model.scripting.parameters.ParameterImage;
import gov.nih.mipav.model.scripting.parameters.ParameterTable;
import gov.nih.mipav.model.structures.ModelImage;

import gov.nih.mipav.view.MipavUtil;
import gov.nih.mipav.view.Preferences;
import gov.nih.mipav.view.ViewUserInterface;

import java.awt.*;
import java.awt.event.ActionEvent;
import java.awt.event.ItemEvent;
import java.util.Enumeration;
import java.util.Vector;

import javax.swing.*;


// import javax.swing.*;

/**
 * DOCUMENT ME!
 */
@SuppressWarnings("serial")
public class JDialogBRISK extends JDialogScriptableBase implements AlgorithmInterface, ActionDiscovery, ScriptableActionInterface {

    // ~ Static fields/initializers
    // -------------------------------------------------------------------------------------

    /** Use serialVersionUID for interoperability. */
    // private static final long serialVersionUID;

    // ~ Instance fields
    // ------------------------------------------------------------------------------------------------

    /** DOCUMENT ME! */
    private ModelImage image; // source image

    /** DOCUMENT ME! */
    private ModelImage destImage = null; // match image array

    /** DOCUMENT ME! */
    private AlgorithmBRISK bAlgo;

    private int threshold = 60;

    private int HammingDistanceThreshold = 90;

    private int octaves = 4;

    private boolean rotationInvariant = true;

    private boolean scaleInvariant = true;

    private final double patternScale = 1.0;

    private final Vector<Double> radiusList = null;

    private final Vector<Integer> numberList = null;

    // Short pair maximum distance
    private final double dMax = 5.85;

    // Long pair maximum distance
    private final double dMin = 8.2;

    private final Vector<Integer> indexChange = new Vector<Integer>();

    private JPanel paramPanel;

    private JPanel imageVOIPanel;

    private ButtonGroup imageVOIGroup;

    private JRadioButton wholeImage;

    private JRadioButton VOIRegions;

    private ViewUserInterface userInterface;

    private JLabel labelThreshold;

    private JTextField textThreshold;

    private boolean wholeImageFlag = true;

    private JLabel labelHammingDistanceThreshold;

    private JTextField textHammingDistanceThreshold;

    private JLabel labelOctaves;

    private JTextField textOctaves;

    private JCheckBox rotationCheckBox;

    private JCheckBox scaleCheckBox;

    private JComboBox comboBoxImage;

    // ~ Constructors
    // ---------------------------------------------------------------------------------------------------

    /**
     * Empty constructor needed for dynamic instantiation (used during scripting).
     */
    public JDialogBRISK() {}

    /**
     * Creates new dialog for entering parameters for BRISK.
     * 
     * @param theParentFrame Parent frame
     * @param im Source image
     */
    public JDialogBRISK(final Frame theParentFrame, final ModelImage im) {
        super(theParentFrame, false);
        image = im;
        userInterface = ViewUserInterface.getReference();
        init();
    }

    // ~ Methods
    // --------------------------------------------------------------------------------------------------------

    /**
     * actionPerformed - Closes dialog box when the OK button is pressed and calls the algorithm.
     * 
     * @param event event that triggers function
     */
    @Override
    public void actionPerformed(final ActionEvent event) {
        final String command = event.getActionCommand();

        if (command.equals("OK")) {

            if (setVariables()) {
                callAlgorithm();
            }
        } else if (command.equals("Help")) {
            // MipavUtil.showHelp("");
        } else if (command.equals("Cancel")) {
            dispose();
        } else {
            super.actionPerformed(event);
        }
    }

    // ************************************************************************
    // ************************** Algorithm Events ****************************
    // ************************************************************************

    /**
     * This method is required if the AlgorithmPerformed interface is implemented. It is called by the algorithm when it
     * has completed or failed to to complete, so that the dialog can be display the result image and/or clean up.
     * 
     * @param algorithm Algorithm that caused the event.
     */
    @Override
    public void algorithmPerformed(final AlgorithmBase algorithm) {

        if (algorithm instanceof AlgorithmBRISK) {
            System.err.println("BRISK Elapsed: " + algorithm.getElapsedTime());
            image.clearMask();
        }

        if (algorithm.isCompleted()) {
            insertScriptLine();
        }
        // save the completion status for later
        setComplete(algorithm.isCompleted());

        bAlgo.finalize();
        bAlgo = null;
        // dispose();
    }

    // *******************************************************************
    // ************************* Item Events ****************************
    // *******************************************************************

    /**
     * Method to handle item events.
     * 
     * @param event event that cause the method to fire
     */
    @Override
    public void itemStateChanged(final ItemEvent event) {
        // Object source = event.getSource();
        // float tempNum;

    }

    /**
     * Once all the necessary variables are set, call the rule based contrast enhancement algorithm based on what type
     * of image this is and whether or not there is a separate destination image.
     */
    @Override
    protected void callAlgorithm() {

        try {

            // Make algorithm
            bAlgo = new AlgorithmBRISK(destImage, image, wholeImageFlag, threshold, HammingDistanceThreshold, octaves, rotationInvariant, scaleInvariant,
                    patternScale, radiusList, numberList, dMax, dMin, indexChange);

            // This is very important. Adding this object as a listener allows the algorithm to
            // notify this object when it has completed or failed. See algorithm performed event.
            // This is made possible by implementing AlgorithmedPerformed interface
            bAlgo.addListener(this);

            createProgressBar(image.getImageName(), bAlgo);

            setVisible(false); // Hide dialog

            if (isRunInSeparateThread()) {

                // Start the thread as a low priority because we wish to still have user interface work fast.
                if (bAlgo.startMethod(Thread.MIN_PRIORITY) == false) {
                    MipavUtil.displayError("A thread is already running on this object");
                }
            } else {

                bAlgo.run();
            }
        } catch (final OutOfMemoryError x) {
            MipavUtil.displayError("Dialog BRISK: unable to allocate enough memory");

            return;
        }

    }

    /**
     * {@inheritDoc}
     */
    @Override
    protected void doPostAlgorithmActions() {

    }

    /**
     * {@inheritDoc}
     */
    @Override
    protected void setGUIFromParams() {
        image = scriptParameters.retrieveInputImage();
    }

    /**
     * {@inheritDoc}
     */
    @Override
    protected void storeParamsFromGUI() throws ParserException {
        scriptParameters.storeInputImage(image);
    }

    /**
     * Sets up the GUI (panels, buttons, etc) and displays it on the screen.
     */
    private void init() {
        setForeground(Color.black);

        setTitle("BRISK");

        final JLabel labelImage = new JLabel("Base image [" + image.getImageName() + "] for:");
        labelImage.setForeground(Color.black);
        labelImage.setFont(serif12);
        comboBoxImage = buildImgComboBox(image);

        labelThreshold = new JLabel("Detection threshold");
        labelThreshold.setForeground(Color.black);
        labelThreshold.setFont(serif12);

        textThreshold = new JTextField(5);
        textThreshold.setText("60");
        textThreshold.setFont(serif12);

        labelHammingDistanceThreshold = new JLabel("Maximum possible descriptors distance");
        labelHammingDistanceThreshold.setForeground(Color.black);
        labelHammingDistanceThreshold.setFont(serif12);

        textHammingDistanceThreshold = new JTextField(5);
        textHammingDistanceThreshold.setText("90");
        textHammingDistanceThreshold.setFont(serif12);

        labelOctaves = new JLabel("Octaves for the detection");
        labelOctaves.setForeground(Color.black);
        labelOctaves.setFont(serif12);

        textOctaves = new JTextField(5);
        textOctaves.setText("4");
        textOctaves.setFont(serif12);

        rotationCheckBox = new JCheckBox("Rotation invariance");
        rotationCheckBox.setFont(serif12);
        rotationCheckBox.setSelected(true);

        scaleCheckBox = new JCheckBox("Scale invariance");
        scaleCheckBox.setFont(serif12);
        scaleCheckBox.setSelected(true);

        final JPanel upperPanel = new JPanel(new GridBagLayout());

        final GridBagConstraints gbc = new GridBagConstraints();
        gbc.gridwidth = 1;
        gbc.gridheight = 1;
        gbc.anchor = GridBagConstraints.WEST;
        gbc.insets = new Insets(5, 5, 5, 5);

        gbc.gridx = 0;
        gbc.gridy = 0;
        gbc.weightx = 0;
        gbc.fill = GridBagConstraints.NONE;
        upperPanel.add(labelImage, gbc);
        gbc.gridy = 1;
        upperPanel.add(comboBoxImage, gbc);
        gbc.gridx = 0;
        gbc.gridy = 2;
        gbc.weightx = 0;
        gbc.fill = GridBagConstraints.NONE;
        upperPanel.add(labelThreshold, gbc);
        gbc.gridx = 1;
        gbc.gridy = 2;
        gbc.weightx = 1;
        gbc.fill = GridBagConstraints.HORIZONTAL;
        upperPanel.add(textThreshold, gbc);
        gbc.gridx = 0;
        gbc.gridy = 3;
        gbc.weightx = 0;
        gbc.fill = GridBagConstraints.NONE;
        upperPanel.add(labelHammingDistanceThreshold, gbc);
        gbc.gridx = 1;
        gbc.gridy = 3;
        gbc.weightx = 1;
        gbc.fill = GridBagConstraints.HORIZONTAL;
        upperPanel.add(textHammingDistanceThreshold, gbc);
        gbc.gridx = 0;
        gbc.gridy = 4;
        gbc.weightx = 0;
        gbc.fill = GridBagConstraints.NONE;
        upperPanel.add(labelOctaves, gbc);
        gbc.gridx = 1;
        gbc.gridy = 4;
        gbc.weightx = 1;
        gbc.fill = GridBagConstraints.HORIZONTAL;
        upperPanel.add(textOctaves, gbc);
        gbc.gridx = 0;
        gbc.gridy = 5;
        gbc.gridwidth = 2;
        upperPanel.add(rotationCheckBox, gbc);
        gbc.gridx = 0;
        gbc.gridy = 6;
        gbc.gridwidth = 2;
        upperPanel.add(scaleCheckBox, gbc);

        imageVOIPanel = new JPanel(new GridBagLayout());
        imageVOIPanel.setForeground(Color.black);
        imageVOIPanel.setBorder(buildTitledBorder("Image region"));

        imageVOIGroup = new ButtonGroup();
        wholeImage = new JRadioButton("Whole image", true);
        wholeImage.setFont(serif12);
        imageVOIGroup.add(wholeImage);

        VOIRegions = new JRadioButton("VOI region(s)", false);
        VOIRegions.setFont(serif12);
        imageVOIGroup.add(VOIRegions);

        gbc.gridx = 0;
        gbc.gridy = 0;
        gbc.weightx = 1;
        gbc.fill = GridBagConstraints.BOTH;
        gbc.gridwidth = 1;
        gbc.insets = new Insets(0, 0, 0, 0);
        imageVOIPanel.add(wholeImage, gbc);
        gbc.gridy = 1;
        imageVOIPanel.add(VOIRegions, gbc);

        paramPanel = new JPanel(new GridBagLayout());
        paramPanel.setForeground(Color.black);
        paramPanel.setBorder(buildTitledBorder("Parameters"));

        gbc.gridx = 0;
        gbc.gridwidth = GridBagConstraints.REMAINDER;
        int ypos = 0;
        gbc.gridy = ypos++;
        gbc.fill = GridBagConstraints.BOTH;
        gbc.weightx = 1;
        paramPanel.add(upperPanel, gbc);
        gbc.gridy = ypos++;
        gbc.gridwidth = 1;
        paramPanel.add(imageVOIPanel, gbc);

        getContentPane().add(paramPanel);
        getContentPane().add(buildButtons(), BorderLayout.SOUTH);

        pack();
        setVisible(true);
    }

    /**
     * Builds a list of images. Returns combobox.
     * 
     * @param image DOCUMENT ME!
     * 
     * @return Newly created combo box.
     */
    private JComboBox buildImgComboBox(final ModelImage image) {
        final JComboBox comboBox = new JComboBox();
        comboBox.setFont(serif12);
        comboBox.setBackground(Color.white);

        final Enumeration<String> names = userInterface.getRegisteredImageNames();

        while (names.hasMoreElements()) {
            final String name = names.nextElement();

            if ( !name.equals(image.getImageName())) {
                final ModelImage img = userInterface.getRegisteredImageByName(name);

                if ( (img.getNDims() == 2) && ( !img.isColorImage()) && (userInterface.getFrameContainingImage(img) != null)) {
                    comboBox.addItem(name);
                }
            }
        }
        comboBox.addItemListener(this);

        return comboBox;
    }

    /**
     * Use the GUI results to set up the variables needed to run the algorithm.
     * 
     * @return <code>true</code> if parameters set successfully, <code>false</code> otherwise.
     */
    private boolean setVariables() {
        String tmpStr;

        // assign destImage to image selected in comboBox
        final String selectedName = (String) comboBoxImage.getSelectedItem();

        if (selectedName != null) {
            destImage = ViewUserInterface.getReference().getRegisteredImageByName(selectedName);
        }

        if (wholeImage.isSelected()) {
            wholeImageFlag = true;
        } else if (VOIRegions.isSelected()) {
            wholeImageFlag = false;
        }

        tmpStr = textThreshold.getText();

        if (testParameter(tmpStr, 1, 255)) {
            threshold = Integer.valueOf(tmpStr).intValue();
        } else {
            textThreshold.requestFocus();
            textThreshold.selectAll();

            return false;
        }

        tmpStr = textHammingDistanceThreshold.getText();

        if (testParameter(tmpStr, 0, 200)) {
            HammingDistanceThreshold = Integer.valueOf(tmpStr).intValue();
        } else {
            textHammingDistanceThreshold.requestFocus();
            textHammingDistanceThreshold.selectAll();

            return false;
        }

        tmpStr = textOctaves.getText();

        if (testParameter(tmpStr, 1, 8)) {
            octaves = Integer.valueOf(tmpStr).intValue();
        } else {
            textOctaves.requestFocus();
            textOctaves.selectAll();

            return false;
        }

        rotationInvariant = rotationCheckBox.isSelected();

        scaleInvariant = scaleCheckBox.isSelected();

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
                return new String("Algorithms BRISK");
            }

            @Override
            public String getDescription() {
                return new String("Applies Binary Robust Invariant Scalable Keypoints.");
            }

            @Override
            public String getDescriptionLong() {
                return new String("Applies Binary Robust Invariant Scalable Keypoints.");
            }

            @Override
            public String getShortLabel() {
                return new String("BRISK");
            }

            @Override
            public String getLabel() {
                return new String("BRISK");
            }

            @Override
            public String getName() {
                return new String("BRISK");
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
     * Returns whether the action has successfully completed its execution.
     * 
     * @return True, if the action is complete. False, if the action failed or is still running.
     */
    @Override
    public boolean isActionComplete() {
        return isComplete();
    }

    /**
     * Accessor that returns the image.
     * 
     * @return the result image
     */
    public ModelImage getResultImage() {
        return destImage;
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

}
