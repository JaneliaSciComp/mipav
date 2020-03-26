


import gov.nih.mipav.model.algorithms.*;
import gov.nih.mipav.model.scripting.*;
import gov.nih.mipav.model.scripting.parameters.*;
import gov.nih.mipav.model.structures.*;

import gov.nih.mipav.view.*;
import gov.nih.mipav.view.dialogs.*;

import java.awt.*;
import java.awt.event.*;

import javax.swing.*;


/**
 * Dialog to choose image for outputting histogram text to file
 *
 * @version  0.1 November 17, 2008
 * @author   William Gandler
 */
public class PlugInDialogHistogram extends JDialogScriptableBase implements AlgorithmInterface {

    //~ Static fields/initializers -------------------------------------------------------------------------------------

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
    private PlugInAlgorithmHistogram histAlgo;

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
    public PlugInDialogHistogram() { }

    /**
     * Creates new dialog to enter parameters for PlugInAlgorithmHistogram.
     *
     * @param  theParentFrame  Parent frame
     * @param  im              Source image
     */
    public PlugInDialogHistogram(Frame theParentFrame, ModelImage im) {
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
     * @param  algorithm  Algorithm that caused the event.
     */
    public void algorithmPerformed(AlgorithmBase algorithm) {

        if (algorithm instanceof PlugInAlgorithmHistogram) {

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
                histAlgo = new PlugInAlgorithmHistogram(image, RGBOffset, radWholeImage.isSelected(),
                                                  userLimits, userMin, userMax);
            } else {
                histAlgo = new PlugInAlgorithmHistogram(image, radWholeImage.isSelected(),
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
            MipavUtil.displayError("Dialog Histogram: unable to allocate enough memory");

            return;
        }
    }

    /**
     * {@inheritDoc}
     */
    protected void setGUIFromParams() {
        image = scriptParameters.retrieveInputImage();

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
        setTitle("Histogram");

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


        JPanel binPanel = new JPanel(new GridLayout(5, 2));
        binPanel.setForeground(Color.black);
        binPanel.setBorder(buildTitledBorder("Histogram"));

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


        buttonPanel.add(buildOKCancelButtons());

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
}
