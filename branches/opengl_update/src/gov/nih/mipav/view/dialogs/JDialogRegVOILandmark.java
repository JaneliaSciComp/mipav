package gov.nih.mipav.view.dialogs;

import WildMagic.LibFoundation.Mathematics.Vector3f;

import gov.nih.mipav.model.algorithms.*;
import gov.nih.mipav.model.algorithms.registration.*;
import gov.nih.mipav.model.structures.*;

import gov.nih.mipav.view.*;

import java.awt.*;
import java.awt.event.*;

import java.io.*;

import java.util.*;

import javax.swing.*;


/**
 * Dialog to register Kidney images, same spacial location, over time. Calls AlgorithmRegVOILandmark. The user is able
 * to control the degree of blurring in all dimensions and indicate if a correction factor be applied to the z-dimension
 * to account for differing resolutions between the xy resolutions (intra-plane). The user has the option to generate a
 * new image or replace the source image. In addition the user can indicate if you wishes to have the algorithm applied
 * to whole image or to the VOI regions. Adapted from JDialogGradientMag
 *
 * @version  0.1 June, 2000
 * @author   Delia McGarry
 * @see      AlgorithmRegVOILandmark
 */
public class JDialogRegVOILandmark extends JDialogBase implements AlgorithmInterface {

    //~ Static fields/initializers -------------------------------------------------------------------------------------

    /** Use serialVersionUID for interoperability. */
    private static final long serialVersionUID = 1515612809506343793L;

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** DOCUMENT ME! */
    private AlgorithmRegVOILandmark algoRegVOILankmark;

    /** DOCUMENT ME! */
    private JComboBox comboBoxOpt;

    /** DOCUMENT ME! */
    private ModelImage image; // source image

    /** DOCUMENT ME! */
    private JLabel labelminTx, labelmaxTx, labelminTy, labelmaxTy, labelminRz, labelmaxRz;

    /** DOCUMENT ME! */
    private JLabel labelStep;

    /** DOCUMENT ME! */
    private VOIContour position = null;

    /** DOCUMENT ME! */
    private ModelImage resultImage = null; // result image
    
    private ModelImage clonedImage;

    /** DOCUMENT ME! */
    private float scaleX = 1.0f;

    /** DOCUMENT ME! */
    private float scaleY = 1.0f;

    /** DOCUMENT ME! */
    private JTextField textGaussX, textGaussY, textStep;

    /** DOCUMENT ME! */
    private JTextField textminTx, textmaxTx, textminTy, textmaxTy, textminRz, textmaxRz;

    /** DOCUMENT ME! */
    private float[] tmpIntensity = null;

    /** DOCUMENT ME! */
    private Vector3f[] tmpPosition = null;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Creates new dialog to register images.
     *
     * @param  theParentFrame  Parent frame
     * @param  im              Source image
     */
    public JDialogRegVOILandmark(Frame theParentFrame, ModelImage im) {
        super(theParentFrame, false);
        image = im;
        init();
    }

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * Closes dialog box when the OK button is pressed and calls the algorithm.
     *
     * @param  event  event that triggers function
     */
    public void actionPerformed(ActionEvent event) {
        Object source = event.getSource();
        String tmpStr;
        int i, j;
        int VOIindex = -1;
        int nVOIs = 0;
        double minTx, maxTx, minTy, maxTy, minRz, maxRz, step;
        int boxIndex = 0;
        int opt = 0;
        int costFunc = 0;

        if (source == OKButton) {

            if (image.getNDims() != 3) {
                MipavUtil.displayError("This algorithm only works for 2.5D kidney datasets.");

                return;
            }

            // get input from scalePanel
            tmpStr = textGaussX.getText();

            if (testParameter(tmpStr, 0.5, 5.0)) {
                scaleX = Float.valueOf(tmpStr).floatValue();
            } else {
                textGaussX.requestFocus();
                textGaussX.selectAll();

                return;
            }

            tmpStr = textGaussY.getText();

            if (testParameter(tmpStr, 0.5, 5.0)) {
                scaleY = Float.valueOf(tmpStr).floatValue();
            } else {
                textGaussY.requestFocus();
                textGaussY.selectAll();

                return;
            }

            int[] destExtents = new int[3];
            destExtents[0] = image.getExtents()[0];
            destExtents[1] = image.getExtents()[1];
            destExtents[2] = image.getExtents()[2];

            float[] sigmas = new float[2];
            sigmas[0] = scaleX;
            sigmas[1] = scaleY;
            setVisible(false); // Hide dialog

            // get input from optionPanel
            boxIndex = comboBoxOpt.getSelectedIndex();

            if (boxIndex == 0) {
                opt = AlgorithmRegVOILandmark.EXHAUSTIVEOPT;
            } else if (boxIndex == 1) {
                opt = AlgorithmRegVOILandmark.SIMPLEXOPT;
            }

            //if (minDiff.isSelected()) {
            //    costFunc = AlgorithmRegVOILandmark.MINDIFF;
            //} else {
            //    costFunc = AlgorithmRegVOILandmark.MAXSUM;
            //}

            tmpStr = textStep.getText();

            if (testParameter(tmpStr, -5, 5)) {
                step = Double.valueOf(tmpStr).doubleValue();
            } else {
                textStep.requestFocus();
                textStep.selectAll();

                return;
            }

            // get input from RangePanel
            tmpStr = textminTx.getText();

            if (testParameter(tmpStr, -2048, 2048)) {
                minTx = Double.valueOf(tmpStr).doubleValue();
            } else {
                textminTx.requestFocus();
                textminTx.selectAll();

                return;
            }

            tmpStr = textmaxTx.getText();

            if (testParameter(tmpStr, -2048, 2048)) {
                maxTx = Double.valueOf(tmpStr).doubleValue();
            } else {
                textmaxTx.requestFocus();
                textmaxTx.selectAll();

                return;
            }

            tmpStr = textminTy.getText();

            if (testParameter(tmpStr, -2048, 2048)) {
                minTy = Double.valueOf(tmpStr).doubleValue();
            } else {
                textminTy.requestFocus();
                textminTy.selectAll();

                return;
            }

            tmpStr = textmaxTy.getText();

            if (testParameter(tmpStr, -2048, 2048)) {
                maxTy = Double.valueOf(tmpStr).doubleValue();
            } else {
                textmaxTy.requestFocus();
                textmaxTy.selectAll();

                return;
            }

            tmpStr = textminRz.getText();

            if (testParameter(tmpStr, -360, 360)) {
                minRz = Double.valueOf(tmpStr).doubleValue();
            } else {
                textminRz.requestFocus();
                textminRz.selectAll();

                return;
            }

            tmpStr = textmaxRz.getText();

            if (testParameter(tmpStr, -360, 360)) {
                maxRz = Double.valueOf(tmpStr).doubleValue();
            } else {
                textmaxRz.requestFocus();
                textmaxRz.selectAll();

                return;
            }

            try {
                Preferences.debug(minTx + "," + maxTx + "," + minTy + "," + maxTy + "," + minRz + "," + maxRz + "," +
                                  step + "\n",Preferences.DEBUG_ALGORITHM);
                nVOIs = image.getVOIs().size();
                Preferences.debug("nVOIs = " + nVOIs + "\n",Preferences.DEBUG_ALGORITHM);

                ViewVOIVector VOIs = image.getVOIs();

                if (nVOIs == 0) {
                    MipavUtil.displayError("Trace contour using one polygon VOI on first slice.");

                    return;
                }

                for (i = 0; i < nVOIs; i++) {

                    if ((VOIs.VOIAt(i).isActive() == true) &&
                            ((VOIs.VOIAt(i).getCurveType() == VOI.CONTOUR) ||
                                 (VOIs.VOIAt(i).getCurveType() == VOI.POLYLINE))) {
                        VOIindex = i;

                        break;
                    }
                }

                if (i == nVOIs) {
                    MipavUtil.displayError("Please select VOI");

                    return;
                }

                Vector<VOIBase> curves = VOIs.VOIAt(VOIindex).getCurves();
                int nCurves = curves.size();

                if (nCurves == 0) {
                    MipavUtil.displayError("Please select VOI from the first image slice.");

                    return;
                }

                for (j = 0; j < nCurves; j++) {

                    if (((VOIContour) (curves.elementAt(j))).isActive()) {
                        break;
                    }
                }

                if (j == nCurves) {
                    MipavUtil.displayError("Please select VOI");

                    return;
                }

                int xdim = destExtents[0];
                int ydim = destExtents[1];
                int tdim = destExtents[2];
                float[] resolutions = new float[] { 1, 1, 1 };

                for (i = 0; i < tdim; i++) {
                    image.getFileInfo(i).setResolutions(resolutions);
                }

                float[] volBuffer = null;
                int sliceSize = xdim * ydim;
                int volLength = sliceSize * tdim;
                volBuffer = new float[volLength];
                image.exportData(0, volLength, volBuffer); // copy image into 1D array

                int length = (int)
                                 Math.round(((VOIContour) (curves.elementAt(j))).getLengthPtToPt(resolutions) /
                                                resolutions[0]);

                tmpPosition = new Vector3f[2 * length];
                tmpIntensity = new float[2 * length];

                Preferences.debug("VOI predicted array length = " + length + "\n",Preferences.DEBUG_ALGORITHM);

                length = image.getVOIs().VOIAt(VOIindex).getPositionAndIntensity(0, j, tmpPosition, tmpIntensity,
                                                                                 volBuffer, xdim);

                Preferences.debug("actual VOI length = " + length + "\n",Preferences.DEBUG_ALGORITHM);

                position = new VOIContour(false, true);

                for (i = 0; i < length; i++) {
                    position.add( new Vector3f(tmpPosition[i].X, tmpPosition[i].Y, tmpPosition[i].Z) );
                }

                // Hide dialog
                setVisible(false);

                String name = makeImageName(image.getImageName(), "_result");
                
                clonedImage = (ModelImage)image.clone(image.getImageName() + "_result");

                // Make NEW result image of float type
                resultImage = new ModelImage(ModelImage.FLOAT, destExtents, name);

                // call algoRegKidney here
                algoRegVOILankmark = new AlgorithmRegVOILandmark(clonedImage, resultImage, sigmas, true, position, minTx,
                                                                 maxTx, minTy, maxTy, minRz, maxRz, step, opt,
                                                                 costFunc);
                algoRegVOILankmark.addListener(this);

                createProgressBar(image.getImageName(), algoRegVOILankmark);

                // Start the thread as a low priority because we wish to still have user interface work fast
                if (algoRegVOILankmark.startMethod(Thread.MIN_PRIORITY) == false) {
                    MipavUtil.displayError("A thread is already running on this object");
                }
            } catch (IOException error) {
                MipavUtil.displayError("Registration Kidney: IO Exception");

                return;
            } catch (OutOfMemoryError x) {

                if (resultImage != null) {
                    resultImage.disposeLocal(); // Clean up image memory
                    resultImage = null;
                }

                MipavUtil.displayError("Dialog Register Kidney: unable to allocate enough memory");

                return;
            }
        } else if (source == cancelButton) {
            dispose();
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

       /* if (algorithm instanceof AlgorithmRegVOILandmark) {

            if (algoRegVOILankmark.isCompleted() == true) {

                // display registered image
                Vector imageFrames = image.getImageFrameVector();

                for (int i = 0; i < imageFrames.size(); i++) {
                    //((Frame) (imageFrames.elementAt(i))).setTitle(titles[i]);
                    ((Frame) (imageFrames.elementAt(i))).setEnabled(true);

                    if (((Frame) (imageFrames.elementAt(i))) != parentFrame) {
                        UI.registerFrame((Frame) (imageFrames.elementAt(i)));
                    }
                }

                if (parentFrame != null) {
                    UI.registerFrame(parentFrame);
                }

                image.notifyImageDisplayListeners(null, true);
            }
        }*/

        // Update frame
        // ((ViewJFrameBase)parentFrame).updateImages(true);
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


    // *******************************************************************
    // ************************* Item Events ****************************
    // *******************************************************************

    /**
     * Sets labels and text fields enabled or disabled depending on checkbox.
     *
     * @param  event  Event that caused method to fire.
     */
    public void itemStateChanged(ItemEvent event) {
    /*    Object source = event.getSource();

        if (source == comboBoxOpt) {

            if (comboBoxOpt.getSelectedIndex() == 0) {
                labelminTx.setEnabled(true);
                textminTx.setEnabled(true);
                labelmaxTx.setEnabled(true);
                textmaxTx.setEnabled(true);
                labelminRz.setEnabled(true);
                textminRz.setEnabled(true);
                labelmaxRz.setEnabled(true);
                textmaxRz.setEnabled(true);
                labelminTy.setEnabled(true);
                textminTy.setEnabled(true);
                labelmaxTy.setEnabled(true);
                textmaxTy.setEnabled(true);
                labelStep.setEnabled(true);
                textStep.setEnabled(true);
            } else {
                labelminTx.setEnabled(false);
                textminTx.setEnabled(false);
                labelmaxTx.setEnabled(false);
                textmaxTx.setEnabled(false);
                labelminRz.setEnabled(false);
                textminRz.setEnabled(false);
                labelmaxRz.setEnabled(false);
                textmaxRz.setEnabled(false);
                labelminTy.setEnabled(false);
                textminTy.setEnabled(false);
                labelmaxTy.setEnabled(false);
                textmaxTy.setEnabled(false);
                labelStep.setEnabled(false);
                textStep.setEnabled(false);
            }
        }
        */
    }

    /**
     * Sets up GUI components and displays dialog.
     */
    private void init() {
        setTitle("Register");

        JPanel scalePanel = new JPanel(new GridLayout(2, 2));
        scalePanel.setBorder(buildTitledBorder("Scale of the Gaussian"));

        JLabel labelGaussX = new JLabel("X Dimension (0.5 - 5.0) ");
        labelGaussX.setForeground(Color.black);
        labelGaussX.setFont(serif12);
        scalePanel.add(labelGaussX);
        textGaussX = new JTextField();
        textGaussX.setText("1.0");
        textGaussX.setFont(serif12);
        scalePanel.add(textGaussX);

        JLabel labelGaussY = new JLabel("Y Dimension (0.5 - 5.0) ");
        labelGaussY.setForeground(Color.black);
        labelGaussY.setFont(serif12);
        scalePanel.add(labelGaussY);
        textGaussY = new JTextField();
        textGaussY.setText("1.0");
        textGaussY.setFont(serif12);
        scalePanel.add(textGaussY);

        JPanel optionPanel = new JPanel(new GridBagLayout());
        optionPanel.setBorder(buildTitledBorder("Options:"));

        JLabel labelOpt = new JLabel("Optimization:");
        labelOpt.setForeground(Color.black);
        labelOpt.setFont(serif12);

        comboBoxOpt = new JComboBox();
        comboBoxOpt.setFont(serif12);
        comboBoxOpt.setBackground(Color.white);
        comboBoxOpt.addItem("Exhaustive");
        comboBoxOpt.addItem("Downhill Simplex");
        comboBoxOpt.addItemListener(this);

        labelStep = new JLabel("Step Size:");
        labelStep.setForeground(Color.black);
        labelStep.setFont(serif12);

        textStep = new JTextField(5);
        textStep.setText("0.5");
        textStep.setFont(serif12);

        //ButtonGroup costGroup = new ButtonGroup();
        //minDiff = new JRadioButton("Min. diff between Gradient Mag. sums under VOI", false);
        //minDiff.setFont(serif12);
        //minDiff.setEnabled(true);
        //costGroup.add(minDiff);
        //minDiff.addItemListener(this);

        //maxSum = new JRadioButton("Max. Gradient Mag. sum under VOI", true);
        //maxSum.setFont(serif12);
        //maxSum.setEnabled(true);
        //costGroup.add(maxSum);
        //maxSum.addItemListener(this);

        GridBagConstraints gbc = new GridBagConstraints();
        gbc.anchor = GridBagConstraints.WEST;
        gbc.insets = new Insets(0, 5, 0, 5);
        gbc.weightx = 1;
        optionPanel.add(labelOpt, gbc);
        gbc.gridx = 1;
        gbc.fill = GridBagConstraints.HORIZONTAL;
        optionPanel.add(comboBoxOpt, gbc);
        gbc.gridx = 0;
        gbc.gridy = 1;
        gbc.fill = GridBagConstraints.NONE;
        optionPanel.add(labelStep, gbc);
        gbc.gridx = 1;
        gbc.fill = GridBagConstraints.HORIZONTAL;
        optionPanel.add(textStep, gbc);
        gbc.gridx = 0;
        gbc.gridy = 2;
        gbc.gridwidth = 2;
        //optionPanel.add(minDiff, gbc);
        gbc.gridy = 3;
        //optionPanel.add(maxSum, gbc);


        JPanel rangePanel = new JPanel(new GridLayout(3, 4));
        rangePanel.setBorder(buildTitledBorder("Search Range:"));

        // min Tx
        labelminTx = new JLabel(" min Tx:");
        labelminTx.setForeground(Color.black);
        labelminTx.setFont(serif12);
        rangePanel.add(labelminTx);

        textminTx = new JTextField(5);
        textminTx.setText("-10");
        textminTx.setFont(serif12);
        rangePanel.add(textminTx);

        // max Tx
        labelmaxTx = new JLabel(" max Tx:");
        labelmaxTx.setForeground(Color.black);
        labelmaxTx.setFont(serif12);
        rangePanel.add(labelmaxTx);

        textmaxTx = new JTextField(5);
        textmaxTx.setText("10");
        textmaxTx.setFont(serif12);
        rangePanel.add(textmaxTx);

        // min Ty
        labelminTy = new JLabel(" min Ty:");
        labelminTy.setForeground(Color.black);
        labelminTy.setFont(serif12);
        rangePanel.add(labelminTy);

        textminTy = new JTextField(5);
        textminTy.setText("-10");
        textminTy.setFont(serif12);
        rangePanel.add(textminTy);

        // max Ty
        labelmaxTy = new JLabel(" max Ty:");
        labelmaxTy.setForeground(Color.black);
        labelmaxTy.setFont(serif12);
        rangePanel.add(labelmaxTy);

        textmaxTy = new JTextField(5);
        textmaxTy.setText("10");
        textmaxTy.setFont(serif12);
        rangePanel.add(textmaxTy);

        // minRz
        labelminRz = new JLabel(" min Rz:");
        labelminRz.setForeground(Color.black);
        labelminRz.setFont(serif12);
        rangePanel.add(labelminRz);

        textminRz = new JTextField(5);
        textminRz.setText("-10");
        textminRz.setFont(serif12);
        rangePanel.add(textminRz);

        // max Rz
        labelmaxRz = new JLabel(" max Rz:");
        labelmaxRz.setForeground(Color.black);
        labelmaxRz.setFont(serif12);
        rangePanel.add(labelmaxRz);

        textmaxRz = new JTextField(5);
        textmaxRz.setText("10");
        textmaxRz.setFont(serif12);
        rangePanel.add(textmaxRz);

        JPanel mainPanel = new JPanel(new GridBagLayout());
        gbc.gridx = 0;
        gbc.gridy = 0;
        gbc.gridwidth = 1;
        gbc.fill = GridBagConstraints.HORIZONTAL;
        mainPanel.add(scalePanel, gbc);
        gbc.gridy = 1;
        mainPanel.add(optionPanel, gbc);
        gbc.gridy = 2;
        mainPanel.add(rangePanel, gbc);
        mainPanel.setBorder(BorderFactory.createEmptyBorder(5, 5, 5, 5));

        JPanel buttonPanel = new JPanel();
        buildOKButton();
        buttonPanel.add(OKButton);
        buildCancelButton();
        buttonPanel.add(cancelButton);

        getContentPane().add(mainPanel);
        getContentPane().add(buttonPanel, BorderLayout.SOUTH);
        pack();
        setVisible(true);
    }

}
