package gov.nih.mipav.view.dialogs;


import gov.nih.mipav.model.algorithms.*;
import gov.nih.mipav.model.structures.*;

import gov.nih.mipav.view.*;

import java.awt.*;
import java.awt.event.*;

import javax.swing.*;


/**
 * Dialog is used by the ViewJFrameHistoLUT frame. This dialog allows the user to select parameters before invoking the
 * threshold algorithm.
 *
 * @version  1.0 Mar 9, 2000
 * @author   Matthew J. McAuliffe, Ph.D.
 */
public class JDialogThresholdLUT extends JDialogBase {

    //~ Static fields/initializers -------------------------------------------------------------------------------------

    /** Use serialVersionUID for interoperability. */
    private static final long serialVersionUID = -3705234971745674752L;

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** DOCUMENT ME! */
    private float fillValue = 0;

    /** DOCUMENT ME! */
    private ModelImage image; // source image

    /** DOCUMENT ME! */
    private JLabel labelMax;

    /** DOCUMENT ME! */
    private JLabel labelMin;

    /** DOCUMENT ME! */
    private float lowerThreshold, upperThreshold;

    /** DOCUMENT ME! */
    private int outputType;

    /** DOCUMENT ME! */
    private JRadioButton radioBinary;

    /** DOCUMENT ME! */
    private JRadioButton radioRange;

    /** DOCUMENT ME! */
    private JRadioButton radioThresh;

    /** DOCUMENT ME! */
    private ModelImage resultImage; // source image

    /** DOCUMENT ME! */
    private JTextField textFill;

    /** DOCUMENT ME! */
    private JTextField textMax;

    /** DOCUMENT ME! */
    private JTextField textMin;

    /** DOCUMENT ME! */
    private AlgorithmThresholdDual thresholdAlgo;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Creates new threshold LUT dialog.
     *
     * @param  theParentFrame  Parent frame
     * @param  im              Source image
     * @param  lowThreshold    Lower threshold bound
     * @param  upThreshold     Upper threshold bound
     */
    public JDialogThresholdLUT(Frame theParentFrame, ModelImage im, float lowThreshold, float upThreshold) {
        super(theParentFrame, false);
        image = im;
        lowerThreshold = lowThreshold;
        upperThreshold = upThreshold;
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
        String tmpStr;
        float[] thresholds = new float[2];
        float max = 0, min = 0;

        if (command.equals("Binary")) {
            textFill.setEnabled(false);
            textMin.setEnabled(false);
            textMax.setEnabled(false);
            labelMin.setEnabled(false);
            labelMax.setEnabled(false);
        } else if (command.equals("Thresh")) {
            textFill.setEnabled(true);
            textMin.setEnabled(false);
            textMax.setEnabled(false);
            labelMin.setEnabled(false);
            labelMax.setEnabled(false);
        } else if (command.equals("Range")) {
            textFill.setEnabled(false);
            textMin.setEnabled(true);
            textMax.setEnabled(true);
            labelMin.setEnabled(true);
            labelMax.setEnabled(true);
        } else if (command.equals("OK")) {

            if (radioThresh.isSelected() == true) {
                tmpStr = textFill.getText();

                if (testParameter(tmpStr, -100000, 1000000)) {
                    fillValue = Float.valueOf(tmpStr).floatValue();
                } else {
                    textFill.requestFocus();
                    textFill.selectAll();

                    return;
                }
            } else if (radioRange.isSelected() == true) {
                tmpStr = textMax.getText();

                if (testParameter(tmpStr, -100000, 1000000)) {
                    max = Float.valueOf(tmpStr).intValue();
                } else {
                    textMax.requestFocus();
                    textMax.selectAll();

                    return;
                }

                tmpStr = textMin.getText();

                if (testParameter(tmpStr, -100000, 1000000)) {
                    min = Float.valueOf(tmpStr).intValue();
                } else {
                    textMin.requestFocus();
                    textMin.selectAll();

                    return;
                }
            }

            thresholds[0] = lowerThreshold;
            thresholds[1] = upperThreshold;

            if ((radioBinary.isSelected() == true) || (radioThresh.isSelected() == true)) {

                if (image.getNDims() == 2) { // source image is 2D

                    try {

                        // No need to make new image space because the user has choosen to replace the source image
                        // Make the algorithm class
                        thresholdAlgo = new AlgorithmThresholdDual(image, thresholds, fillValue, outputType, true,
                                                                   false);
                        thresholdAlgo.run();
                    } catch (OutOfMemoryError x) {
                        MipavUtil.displayError("Dialog threshold: unable to allocate enough memory");

                        return;
                    }

                    image.notifyImageDisplayListeners(null, true);
                    dispose();
                } else if (image.getNDims() == 3) {

                    try {

                        // Make algorithm
                        thresholdAlgo = new AlgorithmThresholdDual(image, thresholds, fillValue, outputType, true,
                                                                   true);
                        thresholdAlgo.run();
                    } catch (OutOfMemoryError x) {
                        MipavUtil.displayError("Dialog threshold: unable to allocate enough memory");

                        return;
                    }

                    image.notifyImageDisplayListeners(null, true);
                    dispose();
                }
            } else { // radioRange.isSelected() == true

                if (image.getNDims() == 2) { // source image is 2D

                    int[] destExtents = new int[2];
                    destExtents[0] = image.getExtents()[0]; // X dim
                    destExtents[1] = image.getExtents()[1]; // Y dim

                    try {
                        resultImage = new ModelImage(ModelImage.BOOLEAN, destExtents, " Threshold");
                        thresholdAlgo = new AlgorithmThresholdDual(resultImage, image, thresholds, 0,
                                                                   AlgorithmThresholdDual.BINARY_TYPE, true, true);
                        thresholdAlgo.run();

                        AlgorithmMorphology2D morphAlgo2D = new AlgorithmMorphology2D(resultImage, 0, 0,
                                                                                      AlgorithmMorphology2D.DELETE_OBJECTS,
                                                                                      0, 0, 0, 0, true);
                        morphAlgo2D.setMinMax(Math.round(min), Math.round(max));
                        morphAlgo2D.run();

                        AlgorithmQuantify algoQuantify = new AlgorithmQuantify(image, resultImage);
                        algoQuantify.run();

                        // output
                        // object      # of voxels    total intensity  avg. intensity  volume/area
                        resultImage.clearMask();

                        new ViewJFrameImage(resultImage, null, new Dimension(610, 200));
                    } catch (OutOfMemoryError x) {
                        MipavUtil.displayError("Dialog threshold: unable to allocate enough memory");

                        return;
                    }

                    image.notifyImageDisplayListeners(null, true);
                    dispose();
                } else if (image.getNDims() == 3) {
                    int[] destExtents = new int[3];
                    destExtents[0] = image.getExtents()[0];
                    destExtents[1] = image.getExtents()[1];
                    destExtents[2] = image.getExtents()[2];

                    try {
                        resultImage = new ModelImage(ModelImage.BOOLEAN, destExtents, " Threshold");

                        // Make algorithm
                        thresholdAlgo = new AlgorithmThresholdDual(resultImage, image, thresholds, 0,
                                                                   AlgorithmThresholdDual.BINARY_TYPE, true, true);
                        thresholdAlgo.run();

                        AlgorithmMorphology3D morphAlgo3D = new AlgorithmMorphology3D(resultImage, 0, 0,
                                                                                      AlgorithmMorphology3D.DELETE_OBJECTS,
                                                                                      0, 0, 0, 0, true);
                        morphAlgo3D.setMinMax(Math.round(min), Math.round(max));
                        morphAlgo3D.run();

                        AlgorithmQuantify algoQuantify = new AlgorithmQuantify(image, resultImage);
                        algoQuantify.run();
                        resultImage.clearMask();

                        new ViewJFrameImage(resultImage, null, new Dimension(610, 200));
                    } catch (OutOfMemoryError x) {
                        MipavUtil.displayError("Dialog threshold: unable to allocate enough memory");

                        return;
                    }

                    image.notifyImageDisplayListeners(null, true);
                    dispose();
                }
            }
        } else if (command.equals("Cancel")) {
            cancelFlag = true;
            dispose();
        } else {
            super.actionPerformed(event);
        }
    }

    /**
     * Set up GUI parameters.
     */
    private void init() {
        setForeground(Color.black);
        setTitle("LUT Threshold");

        JPanel optionPanel = new JPanel();
        optionPanel.setLayout(new GridBagLayout());
        optionPanel.setForeground(Color.black);
        optionPanel.setBorder(buildTitledBorder("Threshold parameters"));

        ButtonGroup threshGroup = new ButtonGroup();
        radioBinary = new JRadioButton("Produce binary image");
        radioBinary.setFont(serif12);
        radioBinary.addActionListener(this);
        radioBinary.setActionCommand("Binary");
        radioBinary.setSelected(true);
        radioBinary.setAlignmentX(Component.LEFT_ALIGNMENT);
        threshGroup.add(radioBinary);

        radioThresh = new JRadioButton("Set values outside of thresholds to:");
        radioThresh.setFont(serif12);
        radioThresh.addActionListener(this);
        radioThresh.setActionCommand("Thresh");
        radioThresh.setAlignmentX(Component.LEFT_ALIGNMENT);
        threshGroup.add(radioThresh);

        radioRange = new JRadioButton("Delete objects outside range:");
        radioRange.setFont(serif12);
        radioRange.addActionListener(this);
        radioRange.setActionCommand("Range");
        radioRange.setAlignmentX(Component.LEFT_ALIGNMENT);
        threshGroup.add(radioRange);

        textFill = new JTextField(5);
        textFill.setText("0");
        textFill.setFont(serif12);
        textFill.setEnabled(false);
        textFill.addFocusListener(this);

        labelMax = new JLabel("Maximum");
        labelMax.setForeground(Color.black);
        labelMax.setFont(serif12);
        labelMax.setEnabled(false);

        textMax = new JTextField(5);
        textMax.setText("1000");
        textMax.setFont(serif12);
        textMax.setEnabled(false);

        labelMin = new JLabel("Minimum");
        labelMin.setForeground(Color.black);
        labelMin.setFont(serif12);
        labelMin.setEnabled(false);

        textMin = new JTextField(5);
        textMin.setText("1");
        textMin.setFont(serif12);
        textMin.setEnabled(false);

        JPanel binaryPanel = new JPanel(new FlowLayout(FlowLayout.LEFT));
        binaryPanel.add(radioBinary);

        JPanel threshPanel = new JPanel(new FlowLayout(FlowLayout.LEFT));
        threshPanel.add(radioThresh);
        threshPanel.add(textFill);

        JPanel rangePanel = new JPanel(new FlowLayout(FlowLayout.LEFT));
        rangePanel.add(radioRange);

        JPanel minPanel = new JPanel(new FlowLayout(FlowLayout.LEFT));
        minPanel.add(Box.createHorizontalStrut(5));
        minPanel.add(labelMin);
        minPanel.add(textMin);

        JPanel maxPanel = new JPanel(new FlowLayout(FlowLayout.LEFT));
        maxPanel.add(Box.createHorizontalStrut(5));
        maxPanel.add(labelMax);
        maxPanel.add(textMax);

        Insets insets = new Insets(0, 2, 0, 2);
        GridBagConstraints gbc = new GridBagConstraints();

        gbc.gridwidth = 1;
        gbc.gridheight = 1;
        gbc.insets = insets;
        gbc.anchor = GridBagConstraints.WEST;

        gbc.gridx = 0;
        gbc.gridy = 0;
        gbc.weightx = 1;
        gbc.fill = GridBagConstraints.HORIZONTAL;
        optionPanel.add(binaryPanel, gbc);
        gbc.gridx = 0;
        gbc.gridy = 1;
        gbc.weightx = 1;
        gbc.fill = GridBagConstraints.HORIZONTAL;
        optionPanel.add(threshPanel, gbc);
        gbc.gridx = 0;
        gbc.gridy = 2;
        gbc.weightx = 1;
        gbc.fill = GridBagConstraints.HORIZONTAL;
        optionPanel.add(rangePanel, gbc);
        gbc.gridx = 0;
        gbc.gridy = 3;
        gbc.weightx = 1;
        gbc.fill = GridBagConstraints.HORIZONTAL;
        optionPanel.add(minPanel, gbc);
        gbc.gridx = 0;
        gbc.gridy = 4;
        gbc.weightx = 1;
        gbc.fill = GridBagConstraints.HORIZONTAL;
        optionPanel.add(maxPanel, gbc);

        JPanel buttonPanel = new JPanel();
        buildOKButton();
        buildCancelButton();
        buttonPanel.add(OKButton);
        buttonPanel.add(cancelButton);

        getContentPane().add(optionPanel);
        getContentPane().add(buttonPanel, BorderLayout.SOUTH);
        pack();
        setVisible(true);
    }
}
