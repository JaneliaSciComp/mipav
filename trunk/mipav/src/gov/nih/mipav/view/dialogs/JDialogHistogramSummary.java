package gov.nih.mipav.view.dialogs;


import gov.nih.mipav.model.algorithms.*;
import gov.nih.mipav.model.scripting.*;
import gov.nih.mipav.model.scripting.parameters.*;
import gov.nih.mipav.model.structures.*;

import gov.nih.mipav.view.*;

import java.awt.*;
import java.awt.event.*;

import javax.swing.*;


/**
 * Dialog to choose images, then call the RGBConcat algorithm.
 *
 * @version  0.1 June 5, 2000
 * @author   Matthew J. McAuliffe, Ph.D.
 */
public class JDialogHistogramSummary extends JDialogScriptableBase 
	implements AlgorithmInterface, ActionDiscovery {

    //~ Static fields/initializers -------------------------------------------------------------------------------------

    /** Use serialVersionUID for interoperability. */
    private static final long serialVersionUID = -1570864449910121911L;

    /** Red channel. */
    private static final int RED_OFFSET = 1;

    /** Green channel. */
    private static final int GREEN_OFFSET = 2;

    /** Blue channel. */
    private static final int BLUE_OFFSET = 3;

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** DOCUMENT ME! */
    double imageMax;

    /** DOCUMENT ME! */
    double imageMin;

    /** DOCUMENT ME! */
    private int bins = 256;

    /** DOCUMENT ME! */
    private JTextField binText;

    /** DOCUMENT ME! */
    private JRadioButton blueButton;

    /** DOCUMENT ME! */
    private ButtonGroup colorGroup;

    /** DOCUMENT ME! */
    private JPanel colorPanel;

    /** DOCUMENT ME! */
    private String error = null;

    /** DOCUMENT ME! */
    private JRadioButton greenButton;

    /** DOCUMENT ME! */
    private AlgorithmHistogram histAlgo;

    /** DOCUMENT ME! */
    private ModelImage image; // source image

    /** DOCUMENT ME! */
    private JRadioButton radVOIs;

    /** DOCUMENT ME! */
    private JRadioButton radWholeImage;

    /** DOCUMENT ME! */
    private JRadioButton redButton;

    /** DOCUMENT ME! */
    private int RGBOffset = RED_OFFSET;
    
    private boolean displayGraph = true;
    
    private JCheckBox userLimitsCheckBox;
    
    private boolean userLimits = false;
    
    private JTextField userMinText;
    
    private JLabel userMinLabel;
    
    private float userMin = 0.0f;
    
    private JTextField userMaxText;
    
    private JLabel userMaxLabel;
    
    private float userMax = 0.0f;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Empty constructor needed for dynamic instantiation (used during scripting).
     */
    public JDialogHistogramSummary() { }

    /**
     * Creates new dialog to enter parameters for RGBConcat algorithm.
     *
     * @param  theParentFrame  Parent frame
     * @param  im              Source image
     */
    public JDialogHistogramSummary(Frame theParentFrame, ModelImage im) {
        super(theParentFrame, false);

        image = im;
        init();
    }

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * Closes dialog box when the OK button is pressed and calls the algorithm.
     *
     * @param  event  Event that triggers function
     */
    public void actionPerformed(ActionEvent event) {
        String command = event.getActionCommand();
        Object source = event.getSource();

        if (command.equals("OK")) {

            if (setVariables()) {
                callAlgorithm();
            }
        } else if (command.equals("Cancel")) {
            dispose();
        } else if (command.equals("Help")) {
        	//MipavUtil.showHelp("A3001");
        	MipavUtil.showWebHelp("Histogram_summary");
        } else if ((source == redButton) || (source == greenButton) || (source == blueButton)) {

            switch (image.getType()) {

                case ModelStorageBase.ARGB:
                case ModelStorageBase.ARGB_USHORT:
                case ModelStorageBase.ARGB_FLOAT:
                    if (redButton.isSelected()) {
                        imageMin = image.getMinR();
                        imageMax = image.getMaxR();
                    } else if (greenButton.isSelected()) {
                        imageMin = image.getMinG();
                        imageMax = image.getMaxG();
                    } else {
                        imageMin = image.getMinB();
                        imageMax = image.getMaxB();
                    }

                    break;

                default:
                    imageMin = (double) image.getMin();
                    imageMax = (double) image.getMax();
                    break;
            }

            if ((image.getType() != ModelStorageBase.FLOAT) && (image.getType() != ModelStorageBase.DOUBLE) &&
                    (image.getType() != ModelStorageBase.ARGB_FLOAT)) {
                bins = (int) Math.round(imageMax - imageMin + 1);
                bins = Math.min(bins, 4096);
            }

            binText.setText(String.valueOf(bins));
        }
    }


    // ************************************************************************
    // ************************** Algorithm Events ****************************
    // ************************************************************************

    /**
     * This method is required if the AlgorithmPerformed interface is implemented. It is called by the algorithm when it
     * has completed or failed to to complete, so that the dialog can be display the result image and/or clean up.
     *
     * @param  algorithm  Algorithm that caused the event.
     */
    public void algorithmPerformed(AlgorithmBase algorithm) {

        if (algorithm instanceof AlgorithmHistogram) {

            if (algorithm.isCompleted()) {
                insertScriptLine();
            }

            histAlgo.finalize();
            histAlgo = null;
            dispose();
        }
    }


    /**
     * Accessor that returns error String if an error has occured.
     *
     * @return  String describing error
     */
    public String getError() {
        return error;
    }

    /**
     * Accessor that sets the RGBOffset.
     *
     * @param  RGBoffset  DOCUMENT ME!
     */
    public void setRGBOffset(int RGBoffset) {
        this.RGBOffset = RGBoffset;
    }
    
    /**
     * If true, use userMin to userMax instead of image.getMin() to image.getMax() as histogram limits
     * @param userLimits
     */
    public void setUserLimits(boolean userLimits) {
        this.userLimits = userLimits;
    }
    
    public void setUserMin(float userMin) {
        this.userMin = userMin;
    }
    
    public void setUserMax(float userMax) {
        this.userMax = userMax;
    }
    
    /**
     * Watches the font descriptor checkboxes (bold/italic) and the font name (style) combo box, updates displayed font
     * with each change.
     *
     * @param  event  ItemEvent the item change event that occured
     */
    public void itemStateChanged(ItemEvent event) {
        if (event.getSource() == userLimitsCheckBox) {
            if (userLimitsCheckBox.isSelected()) {
                userMinLabel.setEnabled(true);
                userMinText.setEnabled(true);
                userMaxLabel.setEnabled(true);
                userMaxText.setEnabled(true);
            }
            else {
                userMinLabel.setEnabled(false);
                userMinText.setEnabled(false);
                userMaxLabel.setEnabled(false);
                userMaxText.setEnabled(false);    
            }
        }
    }

    /**
     * Once all the necessary variables are set, call the Histogram algorithm based on whehter the image is color or
     * not.
     */
    protected void callAlgorithm() {

        try {
            System.gc();

            // Make algorithm
            if (image.isColorImage()) {
                histAlgo = new AlgorithmHistogram(image, bins, RGBOffset, radWholeImage.isSelected(), displayGraph,
                                                  userLimits, userMin, userMax);
            } else {
                histAlgo = new AlgorithmHistogram(image, bins, radWholeImage.isSelected(), displayGraph,
                                                  userLimits, userMin, userMax);
            }

            // This is very important. Adding this object as a listener allows the algorithm to
            // notify this object when it has completed of failed. See algorithm performed event.
            // This is made possible by implementing AlgorithmedPerformed interface
            histAlgo.addListener(this);

            createProgressBar(image.getImageName(), histAlgo);

            // Hide dialog
            setVisible(false);

            if (isRunInSeparateThread()) {

                // Start the thread as a low priority because we wish to still have user interface work fast.
                if (histAlgo.startMethod(Thread.MIN_PRIORITY) == false) {
                    MipavUtil.displayError("A thread is already running on this object");
                }
            } else {
                histAlgo.run();
            }

        } catch (OutOfMemoryError x) {


            System.gc();
            MipavUtil.displayError("Dialog Histogram Summary: unable to allocate enough memory");

            return;
        }
    }

    /**
     * {@inheritDoc}
     */
    protected void setGUIFromParams() {
        image = scriptParameters.retrieveInputImage();

        bins = scriptParameters.getParams().getInt("number_of_bins");

        if (image.isColorImage() && scriptParameters.getParams().containsParameter("RGB_offset")) {
            RGBOffset = scriptParameters.getParams().getInt("RGB_offset");
        } else if (image.isColorImage()) {
            throw new ParameterException("RGB_offset",
                                         "This parameter (RGB_offset) is required for the processing of color images.  Please re-record this script using a color image.");
        }

        radWholeImage = new JRadioButton("Whole image");
        radWholeImage.setSelected(scriptParameters.doProcessWholeImage());
        userLimits = scriptParameters.getParams().getBoolean("user_limits");
        userMin = scriptParameters.getParams().getFloat("user_min");
        userMax = scriptParameters.getParams().getFloat("user_max");
    }

    /**
     * {@inheritDoc}
     */
    protected void storeParamsFromGUI() throws ParserException {
        scriptParameters.storeInputImage(image);

        scriptParameters.getParams().put(ParameterFactory.newParameter("number_of_bins", bins));

        if (image.isColorImage()) {
            scriptParameters.getParams().put(ParameterFactory.newParameter("RGB_offset", RGBOffset));
        }

        scriptParameters.storeProcessWholeImage(radWholeImage.isSelected());
        scriptParameters.getParams().put(ParameterFactory.newParameter("user_limits", userLimits));
        scriptParameters.getParams().put(ParameterFactory.newParameter("user_min", userMin));
        scriptParameters.getParams().put(ParameterFactory.newParameter("user_max", userMax));
    }

    /**
     * Sets up the GUI (panels, buttons, etc) and displays it on the screen.
     */
    private void init() {

        setForeground(Color.black);
        setTitle("Histogram Summary");

        if (image.isColorImage()) {
            colorPanel = new JPanel(new GridLayout(3, 1));
            colorPanel.setForeground(Color.black);
            colorPanel.setBorder(buildTitledBorder("Colors"));

            colorGroup = new ButtonGroup();
            redButton = new JRadioButton("Red", true);
            redButton.setFont(serif12);
            redButton.setForeground(Color.black);
            redButton.addActionListener(this);
            colorGroup.add(redButton);
            colorPanel.add(redButton);

            greenButton = new JRadioButton("Green", false);
            greenButton.setFont(serif12);
            greenButton.setForeground(Color.black);
            greenButton.addActionListener(this);
            colorGroup.add(greenButton);
            colorPanel.add(greenButton);

            blueButton = new JRadioButton("Blue", false);
            blueButton.setFont(serif12);
            blueButton.setForeground(Color.black);
            blueButton.addActionListener(this);
            colorGroup.add(blueButton);
            colorPanel.add(blueButton);
        } // if (image.isColorImage())

        switch (image.getType()) {

            case ModelStorageBase.ARGB:
            case ModelStorageBase.ARGB_USHORT:
            case ModelStorageBase.ARGB_FLOAT:
                if (redButton.isSelected()) {
                    imageMin = image.getMinR();
                    imageMax = image.getMaxR();
                } else if (greenButton.isSelected()) {
                    imageMin = image.getMinG();
                    imageMax = image.getMaxG();
                } else {
                    imageMin = image.getMinB();
                    imageMax = image.getMaxB();
                }

                break;

            default:
                imageMin = (double) image.getMin();
                imageMax = (double) image.getMax();
                break;
        }

        if ((image.getType() != ModelStorageBase.FLOAT) && (image.getType() != ModelStorageBase.DOUBLE) &&
                (image.getType() != ModelStorageBase.ARGB_FLOAT)) {
            bins = (int) Math.round(imageMax - imageMin + 1);
            bins = Math.min(bins, 4096);
        }


        JPanel binPanel = new JPanel(new GridLayout(6, 2));
        binPanel.setForeground(Color.black);
        binPanel.setBorder(buildTitledBorder("Bins"));

        JLabel binLabel = new JLabel("Number of bins: ");
        binLabel.setForeground(Color.black);
        binLabel.setFont(serif12);
        binPanel.add(binLabel);

        binText = new JTextField(String.valueOf(bins));
        binText.setForeground(Color.black);
        binText.setFont(serif12);
        binPanel.add(binText);

        ButtonGroup buttonGroup = new ButtonGroup();

        radWholeImage = new JRadioButton("Whole image");
        binPanel.add(radWholeImage);
        radWholeImage.setSelected(true);
        radWholeImage.setFont(serif12);
        buttonGroup.add(radWholeImage);

        binPanel.add(new JLabel(" "));

        radVOIs = new JRadioButton("VOI region(s)");
        binPanel.add(radVOIs);
        radVOIs.setFont(serif12);
        buttonGroup.add(radVOIs);
        
        binPanel.add(new JLabel(" "));
        
        userLimitsCheckBox = new JCheckBox("User specified minimum and maximum");
        userLimitsCheckBox.setSelected(false);
        userLimitsCheckBox.setFont(serif12);
        userLimitsCheckBox.setForeground(Color.black);
        userLimitsCheckBox.addItemListener(this);
        binPanel.add(userLimitsCheckBox);
        
        binPanel.add(new JLabel(" "));
        
        userMinLabel = new JLabel("User specified minimum");
        userMinLabel.setEnabled(false);
        userMinLabel.setFont(serif12);
        userMinLabel.setForeground(Color.black);
        binPanel.add(userMinLabel);

        userMinText = new JTextField(10);
        userMinText.setText(String.valueOf(image.getMin()));
        userMinText.setEnabled(false);
        userMinText.setFont(serif12);
        userMinText.setForeground(Color.black);
        binPanel.add(userMinText);
        
        userMaxLabel = new JLabel("User specified maximum");
        userMaxLabel.setEnabled(false);
        userMaxLabel.setFont(serif12);
        userMaxLabel.setForeground(Color.black);
        binPanel.add(userMaxLabel);

        userMaxText = new JTextField(10);
        userMaxText.setText(String.valueOf(image.getMax()));
        userMaxText.setEnabled(false);
        userMaxText.setFont(serif12);
        userMaxText.setForeground(Color.black);
        binPanel.add(userMaxText);
        
        
        JPanel buttonPanel = new JPanel();


        buttonPanel.add(buildButtons());

        JPanel mainPanel = new JPanel(new BorderLayout());

        if (image.isColorImage()) {
            mainPanel.add(colorPanel, BorderLayout.NORTH);
        }

        mainPanel.add(binPanel);
        mainPanel.setBorder(BorderFactory.createEmptyBorder(5, 5, 5, 5));

        getContentPane().add(mainPanel);
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
        String tmpStr;

        if (image.isColorImage()) {

            if (redButton.isSelected()) {
                RGBOffset = RED_OFFSET;
            } else if (greenButton.isSelected()) {
                RGBOffset = GREEN_OFFSET;
            } else {
                RGBOffset = BLUE_OFFSET;
            }
        } // if (image.isColorImage())

        tmpStr = binText.getText();

        if (testParameter(tmpStr, 1.0, 4096.0)) {
            bins = Integer.valueOf(tmpStr).intValue();
        } else {
            MipavUtil.displayError("bin number must be between 1 and 4096");
            binText.requestFocus();
            binText.selectAll();

            return false;
        }
        
        userLimits = userLimitsCheckBox.isSelected();
        
        if (userLimits) {
            tmpStr = userMinText.getText();
            if (testParameter(tmpStr, -Float.MAX_VALUE, Float.MAX_VALUE)) {
                userMin = Float.valueOf(tmpStr).floatValue();
            } else {
                userMinText.requestFocus();
                userMinText.selectAll();

                return false;
            }
            
            tmpStr = userMaxText.getText();
            if (testParameter(tmpStr, userMin, Float.MAX_VALUE)) {
                userMax = Float.valueOf(tmpStr).floatValue();
            } else {
                userMaxText.requestFocus();
                userMaxText.selectAll();

                return false;
            }    
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
                return new String("Algorithms.Histogram");
            }

            public String getDescription() {
                return new String("Calculates the histogram for an image. " +
                		"The number of bins is determined by the extents of the histogram model." +
                		"Note that for the rgb_offset parameter, red offset == 1," +
                		"green offset == 2 and blue offset == 3. " +
                		"If image is grayscale, ignore this parameter.");
            }

            public String getDescriptionLong() {
                return new String("Calculates the histogram for an image. " +
                		"The number of bins is determined by the extents of the histogram model." +
                		"Note that for the rgb_offset parameter, red offset == 1," +
                		"green offset == 2 and blue offset == 3. " +
                		"If image is grayscale, ignore this parameter.");
            }

            public String getShortLabel() {
                return new String("HistogramSummary");
            }

            public String getLabel() {
                return new String("Histogram Summary");
            }

            public String getName() {
                return new String("Histogram Summary");
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
            
            //Red offset == 1
            //Green offset == 2
            //Blue offset == 3
            table.put(new ParameterInt("RGB_offset", 1));
            table.put(new ParameterInt("number_of_bins",256));
            table.put(new ParameterBoolean(AlgorithmParameters.DO_PROCESS_WHOLE_IMAGE, true));
            table.put(new ParameterBoolean("user_limits", false));
            table.put(new ParameterFloat("user_min", 0.0f));
            table.put(new ParameterFloat("user_max", 0.0f));
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
        	return image.getImageName();
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
