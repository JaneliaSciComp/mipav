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
 * @version  June 2, 2004
 * @see      JDialogBase
 * @see      AlgorithmInterface
 *
 *           <p>$Logfile: /mipav/src/plugins/PlugInDialogObjectDistanceKruhlak.java $ $Revision: 6 $ $Date: 1/25/06
 *           4:59p $</p>
 */
public class PlugInDialogObjectDistanceKruhlak extends JDialogScriptableBase implements AlgorithmInterface {

    //~ Static fields/initializers -------------------------------------------------------------------------------------

    /** Use serialVersionUID for interoperability. */
    private static final long serialVersionUID = -1456956765697025584L;

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** DOCUMENT ME! */
    private JCheckBox blueCheckBox;

    /** DOCUMENT ME! */
    private int colorsPresent = 0;

    /** DOCUMENT ME! */
    private JCheckBox greenCheckBox;

    /** DOCUMENT ME! */
    private ModelImage image; // source image

    /** DOCUMENT ME! */
    private JLabel labelImage;

    /** DOCUMENT ME! */
    private JLabel labelThreshold1;

    /** DOCUMENT ME! */
    private JLabel labelThreshold2;

    /** DOCUMENT ME! */
    private int maxR, maxG, maxB;

    /** DOCUMENT ME! */
    private int minR, minG, minB;

    /** DOCUMENT ME! */
    private PlugInAlgorithmObjectDistanceKruhlak objectDistAlgo = null;

    /** DOCUMENT ME! */
    private JCheckBox redCheckBox;

    /** DOCUMENT ME! */
    private JTextField textThreshold1;

    /** DOCUMENT ME! */
    private JTextField textThreshold2;

    /** DOCUMENT ME! */
    private int threshold1;

    /** DOCUMENT ME! */
    private int threshold2;

    /** DOCUMENT ME! */
    private boolean useBlue = false;

    /** DOCUMENT ME! */
    private boolean useGreen = false;

    /** DOCUMENT ME! */
    private boolean useRed = false;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Empty constructor needed for dynamic instantiation (used during scripting).
     */
    public PlugInDialogObjectDistanceKruhlak() { }

    /**
     * Creates new dialog for distances between 2 3D centers of mass using a plugin.
     *
     * @param  theParentFrame  Parent frame.
     * @param  im              Source image.
     */
    public PlugInDialogObjectDistanceKruhlak(Frame theParentFrame, ModelImage im) {
        super(theParentFrame, false);

        if (!im.isColorImage()) {
            MipavUtil.displayError("Source Image must be Color");
            dispose();

            return;
        }

        image = im;
        init();
    }

    //~ Methods --------------------------------------------------------------------------------------------------------

    // ************************************************************************
    // ************************** Event Processing ****************************
    // ************************************************************************

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
        } else if (command.equals("Script")) {
            callAlgorithm();
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
     * @param  algorithm  Algorithm that caused the event.
     */
    public void algorithmPerformed(AlgorithmBase algorithm) {

        if (algorithm instanceof PlugInAlgorithmObjectDistanceKruhlak) {
            image.clearMask();

            if (objectDistAlgo.isCompleted() == true) {
                insertScriptLine();
            }

            dispose();
        }
    }

    /**
     * itemStateChanged.
     *
     * @param  event  a checkbox-generated event
     */
    public void itemStateChanged(ItemEvent event) {
        Object source = event.getSource();

        if ((colorsPresent == 3) &&
                ((source == redCheckBox) || (source == greenCheckBox) || (source == blueCheckBox))) {

            // Only process if 2 checkBoxes are selected and 1 is not selected
            if (((redCheckBox.isSelected()) && (greenCheckBox.isSelected()) && (!blueCheckBox.isSelected())) ||
                    ((redCheckBox.isSelected()) && (!greenCheckBox.isSelected()) && (blueCheckBox.isSelected())) ||
                    ((!redCheckBox.isSelected()) && (greenCheckBox.isSelected()) && (blueCheckBox.isSelected()))) {
                useRed = redCheckBox.isSelected();
                useGreen = greenCheckBox.isSelected();
                useBlue = blueCheckBox.isSelected();

                if (useRed) {
                    labelThreshold1.setText("Red Threshold (" + String.valueOf(minR) + "-" + String.valueOf(maxR) +
                                            ")");
                    threshold1 = minR;
                } else {
                    labelThreshold1.setText("Green Threshold (" + String.valueOf(minG) + "-" + String.valueOf(maxG) +
                                            ")");
                    threshold1 = minG;
                }

                textThreshold1.setText(String.valueOf(threshold1));

                if (!useBlue) {
                    labelThreshold2.setText("Green Threshold (" + String.valueOf(minG) + "-" + String.valueOf(maxG) +
                                            ")");
                    threshold2 = minG;
                } else {
                    labelThreshold2.setText("Blue Threshold (" + String.valueOf(minB) + "-" + String.valueOf(maxB) +
                                            ")");
                    threshold2 = minB;
                }

                textThreshold2.setText(String.valueOf(threshold2));
            }
        }
    }

    /**
     * Once all the necessary variables are set, call the Gaussian Blur algorithm based on what type of image this is
     * and whether or not there is a separate destination image.
     */
    protected void callAlgorithm() {

        try {

            objectDistAlgo = new PlugInAlgorithmObjectDistanceKruhlak(image, useRed, useGreen, useBlue, threshold1,
                                                                      threshold2);

            // This is very important. Adding this object as a listener allows
            // the algorithm to
            // notify this object when it has completed or failed. See algorithm
            // performed event.
            // This is made possible by implementing AlgorithmedPerformed
            // interface
            objectDistAlgo.addListener(this);

            createProgressBar(image.getImageName(), " ...", objectDistAlgo);

            setVisible(false); // Hide dialog

            if (isRunInSeparateThread()) {

                // Start the thread as a low priority because we wish to still
                // have user interface work fast.
                if (objectDistAlgo.startMethod(Thread.MIN_PRIORITY) == false) {
                    MipavUtil.displayError("A thread is already running on this object");
                }
            } else {
                objectDistAlgo.run();
            }
        } catch (OutOfMemoryError x) {
            MipavUtil.displayError("Object Distance Kruhlak: unable to allocate enough memory");

            return;
        }

    } // end callAlgorithm()

    /**
     * {@inheritDoc}
     */
    protected void setGUIFromParams() {
        image = scriptParameters.retrieveInputImage();
        parentFrame = image.getParentFrame();

        if (!image.isColorImage()) {
            throw new ParameterException(AlgorithmParameters.getInputImageLabel(1), "Source Image must be Color");
        }

        boolean[] rgb = scriptParameters.doProcessRGB();
        useRed = rgb[0];
        useGreen = rgb[1];
        useBlue = rgb[2];

        threshold1 = scriptParameters.getParams().getInt("threshold_1");
        threshold2 = scriptParameters.getParams().getInt("threshold_2");
    }

    /**
     * {@inheritDoc}
     */
    protected void storeParamsFromGUI() throws ParserException {
        scriptParameters.storeInputImage(image);

        scriptParameters.storeColorOptions(useRed, useGreen, useBlue);
        scriptParameters.getParams().put(ParameterFactory.newParameter("threshold_1", threshold1));
        scriptParameters.getParams().put(ParameterFactory.newParameter("threshold_2", threshold2));
    }

    /**
     * Sets up the GUI (panels, buttons, etc) and displays it on the screen.
     */
    private void init() {
        boolean haveRed;
        boolean haveGreen;
        boolean haveBlue;
        JPanel imagePanel;

        setForeground(Color.black);
        setTitle("Center of Mass Distances");

        GridBagConstraints gbc = new GridBagConstraints();
        gbc.gridwidth = 1;
        gbc.gridheight = 1;
        gbc.anchor = GridBagConstraints.WEST;
        gbc.weightx = 1;
        gbc.insets = new Insets(3, 3, 3, 3);
        gbc.fill = GridBagConstraints.HORIZONTAL;

        int yPos = 0;
        gbc.gridx = 0;
        gbc.gridy = yPos++;

        JPanel mainPanel = new JPanel(new GridBagLayout());
        mainPanel.setForeground(Color.black);
        mainPanel.setBorder(buildTitledBorder("Input parameters"));

        JLabel labelVOI = new JLabel("Select a new VOI for each region");
        labelVOI.setForeground(Color.black);
        labelVOI.setFont(serif12);
        mainPanel.add(labelVOI, gbc);

        if (image.getNDims() > 2) {
            JLabel labelSlices = new JLabel("Propagate VOIs to desired slices");
            labelSlices.setForeground(Color.black);
            labelSlices.setFont(serif12);
            gbc.gridy = yPos++;
            mainPanel.add(labelSlices, gbc);
        } // if (image.getNDims() > 2)

        GridBagConstraints gbc2 = new GridBagConstraints();

        gbc2.gridwidth = 1;
        gbc2.gridheight = 1;
        gbc2.anchor = GridBagConstraints.WEST;
        gbc2.weightx = 1;
        gbc2.insets = new Insets(3, 3, 3, 3);
        gbc2.fill = GridBagConstraints.HORIZONTAL;
        gbc2.gridx = 0;

        int yPos2 = 0;
        gbc2.gridy = yPos2++;

        imagePanel = new JPanel(new GridBagLayout());
        imagePanel.setBorder(buildTitledBorder("Channel selection"));

        haveRed = false;
        haveGreen = false;
        haveBlue = false;
        minR = (int) Math.round(image.getMinR());
        maxR = (int) Math.round(image.getMaxR());

        if (minR != maxR) {
            haveRed = true;
        }

        minG = (int) Math.round(image.getMinG());
        maxG = (int) Math.round(image.getMaxG());

        if (minG != maxG) {
            haveGreen = true;
        }

        minB = (int) Math.round(image.getMinB());
        maxB = (int) Math.round(image.getMaxB());

        if (minB != maxB) {
            haveBlue = true;
        }

        colorsPresent = 0;

        if (haveRed) {
            colorsPresent++;
        }

        if (haveGreen) {
            colorsPresent++;
        }

        if (haveBlue) {
            colorsPresent++;
        }

        if (colorsPresent == 0) {
            MipavUtil.displayError("All channels in this color image are single valued");

            return;
        } else if (colorsPresent == 1) {

            if (haveRed) {
                MipavUtil.displayError("Only the red channel has more than 1 bin");
            } else if (haveGreen) {
                MipavUtil.displayError("Only the green channel has more than 1 bin");
            } else {
                MipavUtil.displayError("Only the blue channel has more than 1 bin");
            }

            return;
        } // else if (colorsPresent == 1)
        else if (colorsPresent == 2) {

            if (haveRed && haveGreen) {
                labelImage = new JLabel("Distance between red and green");
                useRed = true;
                useGreen = true;
            } else if (haveRed && haveBlue) {
                labelImage = new JLabel("Distance between red and blue");
                useRed = true;
                useBlue = true;
            } else {
                labelImage = new JLabel("Distance between green and blue");
                useGreen = true;
                useBlue = true;
            }

            labelImage.setForeground(Color.black);
            labelImage.setFont(serif12);
            gbc.gridy = yPos2++;
            imagePanel.add(labelImage, gbc2);
        } // else if (colorsPresent == 2)
        else { // colorsPresent == 3
            labelImage = new JLabel("Select 2 of the 3 colors");
            labelImage.setForeground(Color.black);
            labelImage.setFont(serif12);
            gbc2.gridy = yPos2++;
            imagePanel.add(labelImage, gbc2);

            redCheckBox = new JCheckBox("Red");
            redCheckBox.setFont(serif12);
            redCheckBox.setForeground(Color.black);
            redCheckBox.setSelected(true);
            redCheckBox.addItemListener(this);
            gbc2.gridy = yPos2++;
            imagePanel.add(redCheckBox, gbc2);

            greenCheckBox = new JCheckBox("Green");
            greenCheckBox.setFont(serif12);
            greenCheckBox.setForeground(Color.black);
            greenCheckBox.setSelected(true);
            greenCheckBox.addItemListener(this);
            gbc2.gridy = yPos2++;
            imagePanel.add(greenCheckBox, gbc2);

            blueCheckBox = new JCheckBox("Blue");
            blueCheckBox.setFont(serif12);
            blueCheckBox.setForeground(Color.black);
            blueCheckBox.setSelected(false);
            blueCheckBox.addItemListener(this);
            gbc2.gridy = yPos2++;
            imagePanel.add(blueCheckBox, gbc2);

            useRed = true;
            useGreen = true;
        } // else colorsPresent == 3

        if (useRed) {
            labelThreshold1 = new JLabel("Red Threshold (" + String.valueOf(minR) + "-" + String.valueOf(maxR) + ")");
        } else {
            labelThreshold1 = new JLabel("Green Threshold (" + String.valueOf(minG) + "-" + String.valueOf(maxG) + ")");
        }

        labelThreshold1.setForeground(Color.black);
        labelThreshold1.setFont(serif12);
        gbc2.gridy = yPos2;
        imagePanel.add(labelThreshold1, gbc2);

        gbc2.gridx = 1;
        textThreshold1 = new JTextField(5);

        if (useRed) {
            threshold1 = minR;
        } else {
            threshold1 = minG;
        }

        textThreshold1.setText(String.valueOf(threshold1));
        textThreshold1.setFont(serif12);
        yPos2++;
        imagePanel.add(textThreshold1, gbc2);

        if (!useBlue) {
            labelThreshold2 = new JLabel("Green Threshold (" + String.valueOf(minG) + "-" + String.valueOf(maxG) + ")");
        } else {
            labelThreshold2 = new JLabel("Blue Threshold (" + String.valueOf(minB) + "-" + String.valueOf(maxB) + ")");
        }

        labelThreshold2.setForeground(Color.black);
        labelThreshold2.setFont(serif12);
        gbc2.gridx = 0;
        gbc2.gridy = yPos2;
        imagePanel.add(labelThreshold2, gbc2);

        gbc2.gridx = 1;
        textThreshold2 = new JTextField(5);

        if (!useBlue) {
            threshold2 = minG;
        } else {
            threshold2 = minB;
        }

        textThreshold2.setText(String.valueOf(threshold2));
        textThreshold2.setFont(serif12);
        yPos2++;
        imagePanel.add(textThreshold2, gbc2);

        gbc.gridy = yPos++;
        mainPanel.add(imagePanel, gbc);
        getContentPane().add(mainPanel, BorderLayout.CENTER);

        // Build the Panel that holds the OK and CANCEL Buttons
        JPanel OKCancelPanel = new JPanel();

        // size and place the OK button
        buildOKButton();
        OKCancelPanel.add(OKButton, BorderLayout.WEST);

        // size and place the CANCEL button
        buildCancelButton();
        OKCancelPanel.add(cancelButton, BorderLayout.EAST);
        getContentPane().add(OKCancelPanel, BorderLayout.SOUTH);

        pack();
        setVisible(true);
        setResizable(false);
        System.gc();

    } // end init()

    /**
     * Use the GUI results to set up the variables needed to run the algorithm.
     *
     * @return  <code>true</code> if parameters set successfully, <code>false</code> otherwise.
     */
    private boolean setVariables() {
        ViewVOIVector VOIs;
        int nVOIs;
        int nBoundingVOIs = 0;
        int i;
        String tmpStr;

        VOIs = image.getVOIs();
        nVOIs = VOIs.size();

        if (nVOIs == 0) {
            MipavUtil.displayError("No VOIs were selected");

            return false;
        }

        for (i = 0; i < nVOIs; i++) {

            if (VOIs.VOIAt(i).getCurveType() == VOI.CONTOUR) {
                nBoundingVOIs++;
            }
        }

        if (nBoundingVOIs == 0) {
            MipavUtil.displayError("No countour VOIs were present");

            return false;
        }

        if (colorsPresent == 2) { }
        else if (((redCheckBox.isSelected()) && (greenCheckBox.isSelected()) && (!blueCheckBox.isSelected())) ||
                     ((redCheckBox.isSelected()) && (!greenCheckBox.isSelected()) && (blueCheckBox.isSelected())) ||
                     ((!redCheckBox.isSelected()) && (greenCheckBox.isSelected()) && (blueCheckBox.isSelected()))) {

            useRed = redCheckBox.isSelected();
            useGreen = greenCheckBox.isSelected();
            useBlue = blueCheckBox.isSelected();
        } else {
            MipavUtil.displayError("Exactly 2 color boxes must be checked");

            return false;
        }

        tmpStr = textThreshold1.getText();
        threshold1 = Integer.parseInt(tmpStr);

        if (useRed && (threshold1 < minR)) {
            MipavUtil.displayError("Red threshold cannot be less than " + minR);
            textThreshold1.requestFocus();
            textThreshold1.selectAll();

            return false;
        } else if (useRed && (threshold1 > maxR)) {
            MipavUtil.displayError("Red threshold cannot be greater than " + maxR);
            textThreshold1.requestFocus();
            textThreshold1.selectAll();

            return false;
        } else if (!useRed && (threshold1 < minG)) {
            MipavUtil.displayError("Green threshold cannot be less than " + minG);
            textThreshold1.requestFocus();
            textThreshold1.selectAll();

            return false;
        } else if (!useRed && (threshold1 > maxG)) {
            MipavUtil.displayError("Green threshold cannot be greater than " + maxG);
            textThreshold1.requestFocus();
            textThreshold1.selectAll();

            return false;
        }

        tmpStr = textThreshold2.getText();
        threshold2 = Integer.parseInt(tmpStr);

        if (!useBlue && (threshold2 < minG)) {
            MipavUtil.displayError("Green threshold cannot be less than " + minG);
            textThreshold2.requestFocus();
            textThreshold2.selectAll();

            return false;
        } else if (!useBlue && (threshold2 > maxG)) {
            MipavUtil.displayError("Green threshold cannot be greater than " + maxG);
            textThreshold2.requestFocus();
            textThreshold2.selectAll();

            return false;
        } else if (useBlue && (threshold2 < minB)) {
            MipavUtil.displayError("Blue threshold cannot be less than " + minB);
            textThreshold2.requestFocus();
            textThreshold2.selectAll();

            return false;
        } else if (useBlue && (threshold2 > maxB)) {
            MipavUtil.displayError("Blue threshold cannot be greater than " + maxB);
            textThreshold2.requestFocus();
            textThreshold2.selectAll();

            return false;
        }

        return true;
    } // end setVariables()

}
