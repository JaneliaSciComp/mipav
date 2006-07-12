package gov.nih.mipav.view.dialogs;


import gov.nih.mipav.model.algorithms.*;
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
public class JDialogHistogramSummary extends JDialogBase implements AlgorithmInterface, ScriptableInterface {

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

    /** DOCUMENT ME! */
    private ViewUserInterface userInterface;

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
        userInterface = ((ViewJFrameBase) (parentFrame)).getUserInterface();
        init();
    }

    /**
     * Used primarily for the script to store variables and run the algorithm. No actual dialog will appear but the set
     * up info and result image will be stored here.
     *
     * @param  UI  The user interface, needed to create the image frame.
     * @param  im  Source image.
     */
    public JDialogHistogramSummary(ViewUserInterface UI, ModelImage im) {
        super(false);
        userInterface = UI;
        image = im;
        parentFrame = im.getParentFrame();
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
            MipavUtil.showHelp("");
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
        ViewJFrameImage imageFrame = null;

        if (algorithm instanceof AlgorithmHistogram) {

            insertScriptLine(algorithm);

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
     * If a script is being recorded and the algorithm is done, add an entry for this algorithm.
     *
     * @param  algo  the algorithm to make an entry for
     */
    public void insertScriptLine(AlgorithmBase algo) { }

    /**
     * Run this algorithm from a script.
     *
     * @param   parser  the script parser we get the state from
     *
     * @throws  IllegalArgumentException  if there is something wrong with the arguments in the script
     */
    public void scriptRun(AlgorithmScriptParser parser) throws IllegalArgumentException { }

    /**
     * Accessor that sets the RGBOffset.
     *
     * @param  RGBoffset  DOCUMENT ME!
     */
    public void setRGBOffset(int RGBoffset) {
        this.RGBOffset = RGBOffset;
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
                histAlgo = new AlgorithmHistogram(image, bins, RGBOffset, radWholeImage.isSelected());
            } else {
                histAlgo = new AlgorithmHistogram(image, bins, radWholeImage.isSelected());
            }

            // This is very important. Adding this object as a listener allows the algorithm to
            // notify this object when it has completed of failed. See algorithm performed event.
            // This is made possible by implementing AlgorithmedPerformed interface
            histAlgo.addListener(this);

            // Hide dialog
            setVisible(false);

            if (isRunInSeparateThread()) {

                // Start the thread as a low priority because we wish to still have user interface work fast.
                if (histAlgo.startMethod(Thread.MIN_PRIORITY) == false) {
                    MipavUtil.displayError("A thread is already running on this object");
                }
            } else {
                if (!userInterface.isAppFrameVisible()) {
                    histAlgo.setProgressBarVisible(false);
                }

                histAlgo.run();
            }

        } catch (OutOfMemoryError x) {


            System.gc();
            MipavUtil.displayError("Dialog Histogram Summary: unable to allocate enough memory");

            return;
        }
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


        JPanel binPanel = new JPanel(new GridLayout(3, 2));
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

        return true;
    }
}
