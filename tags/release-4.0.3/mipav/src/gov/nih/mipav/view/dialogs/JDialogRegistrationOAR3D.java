package gov.nih.mipav.view.dialogs;


import gov.nih.mipav.model.algorithms.*;
import gov.nih.mipav.model.algorithms.registration.*;
import gov.nih.mipav.model.file.*;
import gov.nih.mipav.model.scripting.*;
import gov.nih.mipav.model.scripting.parameters.*;
import gov.nih.mipav.model.structures.*;

import gov.nih.mipav.view.*;

import java.awt.*;
import java.awt.event.*;

import java.io.*;

import java.util.*;

import javax.swing.*;


/**
 * Dialog to get user input, then call algorithm RegistrationOAR3D. The user must designate a reference image, a type of
 * cost function, the degrees of freedom used in the registration, and the range and rate of the coarse and fine
 * samples. These are set to defaults most likely to give a fast and accurate registration. The user may also select
 * weighted images to discount parts of the reference and input images in the registration. These must be the same size
 * as their respective originals - i.e., the reference weight image must be the same size as the reference image. The
 * user can select to display the registered image. Regardless of whether this is selected, the matrix will be stored in
 * a file in the user's working directory and also in the original image's transformation matrix.
 *
 * @author  Neva Cherniavsky
 * @see     AlgorithmCostFunctions
 * @see     AlgorithmRegOAR3D
 */
public class JDialogRegistrationOAR3D extends JDialogScriptableBase implements AlgorithmInterface {

    //~ Static fields/initializers -------------------------------------------------------------------------------------

    /** Use serialVersionUID for interoperability. */
    private static final long serialVersionUID = -1461819906844299206L;

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** Variables for Advanced Settings dialog. */
    private JDialog advancedDialog;

    /** DOCUMENT ME! */
    private JTextField bracketBoundText, maxIterationsText, numMinText;

    /** DOCUMENT ME! */
    private JButton buttonWeightInput;

    /** DOCUMENT ME! */
    private JButton buttonWeightRef;

    /** DOCUMENT ME! */
    private boolean calcCOG = true;

    /** DOCUMENT ME! */
    private JCheckBox calcCOGCheckbox;

    /** DOCUMENT ME! */
    private JCheckBox calcLSBox;

    /** DOCUMENT ME! */
    private JPanel coarsePanelX, coarsePanelY, coarsePanelZ;

    /** DOCUMENT ME! */
    private JTextField coarseRateTextX, coarseRateTextY, coarseRateTextZ;

    /** DOCUMENT ME! */
    private JComboBox comboBoxCostFunct;

    /** DOCUMENT ME! */
    private JComboBox comboBoxDOF;

    /** DOCUMENT ME! */
    private JComboBox comboBoxImage;

    /** DOCUMENT ME! */
    private JComboBox comboBoxInterp;

    /** DOCUMENT ME! */
    private JComboBox comboBoxInterp2;

    /** DOCUMENT ME! */
    private int cost, interp, interp2, DOF;

    /** DOCUMENT ME! */
    private String costName = null;

    /** DOCUMENT ME! */
    private boolean displayTransform;

    /** DOCUMENT ME! */
    private boolean doColor;

    /** DOCUMENT ME! */
    private boolean doLS = false;

    /** DOCUMENT ME! */
    private boolean doSubsample;

    /** DOCUMENT ME! */
    private boolean fastMode;

    /** DOCUMENT ME! */
    private JCheckBox fastModeCheckbox;

    /** DOCUMENT ME! */
    private String fileNameWRef, directoryWRef, fileNameWInput, directoryWInput;

    /** DOCUMENT ME! */
    private JPanel finePanelX, finePanelY, finePanelZ;

    /** DOCUMENT ME! */
    private JTextField fineRateTextX, fineRateTextY, fineRateTextZ;

    /** DOCUMENT ME! */
    private GridBagConstraints gbc;

    /** DOCUMENT ME! */
    private ModelImage inputWeightImage, refWeightImage;

    /** DOCUMENT ME! */
    private JLabel labelInterp2;

    /** DOCUMENT ME! */
    private boolean lsCompleted = false;

    /** DOCUMENT ME! */
    private ModelImage lsImage = null;

    /** DOCUMENT ME! */
    private TransMatrix lsMatrix = null;

    /** DOCUMENT ME! */
    private ModelImage matchImage; // register match image to reference Image

    /** DOCUMENT ME! */
    private int maxIterations_def = 2, bracketBound_def = 10, numMinima_def = 3;

    /** DOCUMENT ME! */
    private int maxIterations = maxIterations_def, bracketBound = bracketBound_def;

    /** DOCUMENT ME! */
    private boolean maxOfMinResol;

    /** DOCUMENT ME! */
    private JCheckBox minMaxCheckbox;

    /** DOCUMENT ME! */
    private JRadioButton noneRadio;

    /** DOCUMENT ME! */
    private int numMinima = numMinima_def;

    /** DOCUMENT ME! */
    private ModelImage refImage;

    /** DOCUMENT ME! */
    private AlgorithmRegOAR3D reg3 = null;

    /** DOCUMENT ME! */
    private ModelImage resultImage;

    /** DOCUMENT ME! */
    private JTextField rotateBeginTextX, rotateBeginTextY, rotateBeginTextZ;

    /** DOCUMENT ME! */
    private float rotateBeginX, rotateEndX, coarseRateX, fineRateX;

    /** DOCUMENT ME! */
    private float rotateBeginY, rotateEndY, coarseRateY, fineRateY;

    /** DOCUMENT ME! */
    private float rotateBeginZ, rotateEndZ, coarseRateZ, fineRateZ;

    /** DOCUMENT ME! */
    private JTextField rotateEndTextX, rotateEndTextY, rotateEndTextZ;

    /** DOCUMENT ME! */
    private JPanel rotatePanel;

    /** DOCUMENT ME! */
    private JPanel rotateRangePanelX, rotateRangePanelY, rotateRangePanelZ;

    /** DOCUMENT ME! */
    private JCheckBox sampleCheckbox;

    /** DOCUMENT ME! */
    private JTextField textInput;

    /** DOCUMENT ME! */
    private JTextField textRef;

    /** DOCUMENT ME! */
    private JCheckBox transformCheckbox;

    /** DOCUMENT ME! */
    private ViewUserInterface UI;

    /** DOCUMENT ME! */
    private JCheckBox universalCheckbox;

    /** DOCUMENT ME! */
    private JRadioButton voiRadio;

    /** DOCUMENT ME! */
    private boolean voisOnly;

    /** DOCUMENT ME! */
    private boolean weighted;

    /** DOCUMENT ME! */
    private JRadioButton weightRadio;

    /** DOCUMENT ME! */
    private JRadioButton xRadio;

    /** DOCUMENT ME! */
    private boolean xSelected = true;

    /** DOCUMENT ME! */
    private JRadioButton yRadio;

    /** DOCUMENT ME! */
    private boolean ySelected = false;

    /** DOCUMENT ME! */
    private JRadioButton zRadio;

    /** DOCUMENT ME! */
    private boolean zSelected = false;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Empty constructor needed for dynamic instantiation (used during scripting).
     */
    public JDialogRegistrationOAR3D() { }

    /**
     * Creates new dialog for user to choose type of linear image registration algorithm to run.
     *
     * @param  theParentFrame  Parent frame.
     * @param  im              Source image.
     */
    public JDialogRegistrationOAR3D(Frame theParentFrame, ModelImage im) {
        super(theParentFrame, false);

        matchImage = im;

        // System.out.println("image _______________________________" + matchImage);
        if (matchImage.isColorImage()) {
            doColor = true;
        } else {
            doColor = false;
        }

        UI = ViewUserInterface.getReference();
        init();
    }

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * Closes dialog box when the OK button is pressed, sets the variables, and calls the algorithm.
     *
     * @param  event  Event that triggers function.
     */
    public void actionPerformed(ActionEvent event) {
        String command = event.getActionCommand();
        String tmpStr;

        if (command.equals("OK")) {

            if (setVariables()) {
                callAlgorithm();
            }
        } else if (command.equals("Cancel")) {
            dispose();
        } else if (command.equals("Help")) {
        	MipavUtil.showHelp("OAR19076");
        } else if (command.equals("AdvancedSettings")) {
            bracketBound_def = bracketBound;
            maxIterations_def = maxIterations;
            numMinima_def = numMinima;
            advancedDialog = buildAdvancedDialog(bracketBound, maxIterations, numMinima);
        } else if (command.equals("Ref")) {

            try {

                JFileChooser chooser = new JFileChooser();

                if (UI.getDefaultDirectory() != null) {
                    File file = new File(UI.getDefaultDirectory());

                    if (file != null) {
                        chooser.setCurrentDirectory(file);
                    } else {
                        chooser.setCurrentDirectory(new File(System.getProperty("user.dir")));
                    }
                } else {
                    chooser.setCurrentDirectory(new File(System.getProperty("user.dir")));
                }

                chooser.addChoosableFileFilter(new ViewImageFileFilter(ViewImageFileFilter.GEN));
                chooser.addChoosableFileFilter(new ViewImageFileFilter(ViewImageFileFilter.TECH));
                chooser.addChoosableFileFilter(new ViewImageFileFilter(ViewImageFileFilter.MICROSCOPY));
                chooser.addChoosableFileFilter(new ViewImageFileFilter(ViewImageFileFilter.MISC));

                chooser.setDialogTitle("Open Reference weight file");
                directoryWRef = String.valueOf(chooser.getCurrentDirectory()) + File.separatorChar;

                int returnValue = chooser.showOpenDialog(UI.getMainFrame());

                if (returnValue == JFileChooser.APPROVE_OPTION) {
                    fileNameWRef = chooser.getSelectedFile().getName();
                    directoryWRef = String.valueOf(chooser.getCurrentDirectory()) + File.separatorChar;
                    UI.setDefaultDirectory(directoryWRef);
                } else {
                    fileNameWRef = null;

                    return;
                }

                if (fileNameWRef != null) {
                    textRef.setText(fileNameWRef);
                }
            } catch (OutOfMemoryError e) {
                MipavUtil.displayError("Out of memory in JDialogRegistrationOAR3D.");

                return;
            }
        } else if (command.equals("Input")) {

            try {
                JFileChooser chooser = new JFileChooser();

                if (UI.getDefaultDirectory() != null) {
                    File file = new File(UI.getDefaultDirectory());

                    if (file != null) {
                        chooser.setCurrentDirectory(file);
                    } else {
                        chooser.setCurrentDirectory(new File(System.getProperty("user.dir")));
                    }
                } else {
                    chooser.setCurrentDirectory(new File(System.getProperty("user.dir")));
                }

                chooser.addChoosableFileFilter(new ViewImageFileFilter(ViewImageFileFilter.GEN));
                chooser.addChoosableFileFilter(new ViewImageFileFilter(ViewImageFileFilter.TECH));
                chooser.addChoosableFileFilter(new ViewImageFileFilter(ViewImageFileFilter.MICROSCOPY));
                chooser.addChoosableFileFilter(new ViewImageFileFilter(ViewImageFileFilter.MISC));

                chooser.setDialogTitle("Open Input weight file");
                directoryWInput = String.valueOf(chooser.getCurrentDirectory()) + File.separatorChar;

                int returnValue = chooser.showOpenDialog(UI.getMainFrame());

                if (returnValue == JFileChooser.APPROVE_OPTION) {
                    fileNameWInput = chooser.getSelectedFile().getName();
                    directoryWInput = String.valueOf(chooser.getCurrentDirectory()) + File.separatorChar;
                    UI.setDefaultDirectory(directoryWInput);
                } else {
                    fileNameWInput = null;

                    return;
                }

                if (fileNameWInput != null) {
                    textInput.setText(fileNameWInput);
                }
            } catch (OutOfMemoryError e) {
                MipavUtil.displayError("Out of memory in JDialogRegistrationOAR3D.");

                return;
            }
        } else if (command.equals("AdvancedOkay")) {
            tmpStr = bracketBoundText.getText();

            if (testParameter(tmpStr, 1, 60)) {
                bracketBound = Integer.valueOf(tmpStr).intValue();
            } else {
                bracketBound = bracketBound_def;
            }

            tmpStr = maxIterationsText.getText();

            if (testParameter(tmpStr, 1, 100)) {
                maxIterations = Integer.valueOf(tmpStr).intValue();
            } else {
                maxIterations = maxIterations_def;
            }

            tmpStr = numMinText.getText();

            if (testParameter(tmpStr, 1, 25)) {
                numMinima = Integer.valueOf(tmpStr).intValue();
            } else {
                numMinima = numMinima_def;
            }

            advancedDialog.setVisible(false);
            advancedDialog.dispose();
        } else if (command.equals("AdvancedCancel")) {
            maxIterations = maxIterations_def;
            bracketBound = bracketBound_def;
            numMinima = numMinima_def;
            advancedDialog.setVisible(false);
            advancedDialog.dispose();
        } else if (command.equals("AdvancedHelp")) {
        	MipavUtil.showHelp("OAR19078");
        }
    }

    // ************************************************************************
    // ************************** Algorithm Events ****************************
    // ************************************************************************

    /**
     * This method is required if the AlgorithmPerformed interface is implemented. It is called by the algorithms when
     * it has completed or failed to complete.
     *
     * @param  algorithm  Algorithm that caused the event.
     */
    public void algorithmPerformed(AlgorithmBase algorithm) {
        AlgorithmTransform transform = null;

        if (algorithm instanceof AlgorithmRegOAR3D) {

            if (reg3.isCompleted()) {
                TransMatrix finalMatrix = reg3.getTransform();

                if (doLS) {
                    // System.err.println("OAR3D Matrix: " + finalMatrix);
                    // System.err.println("LS Matrix: "  + lsMatrix);

                    finalMatrix.timesEquals(lsMatrix);
                    // System.err.println("OAR3D x LS: " + finalMatrix);
                }

                if (displayTransform) {
                    int xdimA = refImage.getExtents()[0];
                    int ydimA = refImage.getExtents()[1];
                    int zdimA = refImage.getExtents()[2];
                    float xresA = refImage.getFileInfo(0).getResolutions()[0];
                    float yresA = refImage.getFileInfo(0).getResolutions()[1];
                    float zresA = refImage.getFileInfo(0).getResolutions()[2];

                    String name = makeImageName(matchImage.getImageName(), "_register");

                    transform = new AlgorithmTransform(matchImage, finalMatrix, interp2, xresA, yresA, zresA, xdimA,
                                                       ydimA, zdimA, true, false, false);

                    transform.setUpdateOriginFlag(true);
                    transform.run();
                    resultImage = transform.getTransformedImage();
                    transform.finalize();

                    resultImage.calcMinMax();
                    resultImage.setImageName(name);

                    if (resultImage != null) {

                        try {
                            new ViewJFrameImage(resultImage, null, new Dimension(610, 200));
                        } catch (OutOfMemoryError error) {
                            MipavUtil.displayError("Out of memory: unable to open new frame");
                        }
                    } else {
                        MipavUtil.displayError("Result Image is null");
                    }

                    if (transform != null) {
                        transform.disposeLocal();
                        transform = null;
                    }
                }

                finalMatrix.setTransformID(TransMatrix.TRANSFORM_ANOTHER_DATASET);
                matchImage.getMatrixHolder().addMatrix(finalMatrix);

                String message = "Using cost function, " + costName;
                message += ", the cost is " + Double.toString(reg3.getAnswer()) + ".\n";
                message += "Some registration settings: \n";
                message += "X Rotations from " + rotateBeginX + " to " + rotateEndX + ", ";
                message += "with a X coarse rate of " + coarseRateX + " and X fine rate of " + fineRateX + ".\n";
                message += "Y Rotations from " + rotateBeginY + " to " + rotateEndY + ", ";
                message += "with a Y coarse rate of " + coarseRateY + " and Y fine rate of " + fineRateY + ".\n";
                message += "Z Rotations from " + rotateBeginZ + " to " + rotateEndZ + ", ";
                message += "with a Z coarse rate of " + coarseRateZ + " and Z fine rate of " + fineRateZ + ".\n";
                finalMatrix.saveMatrix(UI.getDefaultDirectory() + matchImage.getImageName() + "_To_" +
                                       refImage.getImageName() + ".mtx", message);

                insertScriptLine();
            }

            if (reg3 != null) {
                reg3.disposeLocal();
                reg3 = null;
            }

            matchImage = null; // register match image to reference Image
            refImage = null;

            if (inputWeightImage != null) {
                inputWeightImage.disposeLocal();
                inputWeightImage = null;
            }

            if (refWeightImage != null) {
                refWeightImage.disposeLocal();
                refWeightImage = null;
            }

            dispose();
            System.gc();
        }
    }

    /**
     * Accessor to get the result image.
     *
     * @return  Result image.
     */
    public ModelImage getResultImage() {
        return resultImage;
    }

    /**
     * Changes the interpolation box to enabled or disabled depending on if the transform box is checked or not.
     *
     * @param  event  Event that triggered this function.
     */
    public void itemStateChanged(ItemEvent event) {

        if (event.getSource() == transformCheckbox) {
            comboBoxInterp2.setEnabled(transformCheckbox.isSelected());
            labelInterp2.setEnabled(transformCheckbox.isSelected());
        } else if (event.getSource() == fastModeCheckbox) {

            // enable or disable search variables
            fastMode = fastModeCheckbox.isSelected();
            rotateBeginTextX.setEnabled(!fastModeCheckbox.isSelected());
            rotateEndTextX.setEnabled(!fastModeCheckbox.isSelected());
            coarseRateTextX.setEnabled(!fastModeCheckbox.isSelected());
            fineRateTextX.setEnabled(!fastModeCheckbox.isSelected());
            rotateBeginTextY.setEnabled(!fastModeCheckbox.isSelected());
            rotateEndTextY.setEnabled(!fastModeCheckbox.isSelected());
            coarseRateTextY.setEnabled(!fastModeCheckbox.isSelected());
            fineRateTextY.setEnabled(!fastModeCheckbox.isSelected());
            rotateBeginTextZ.setEnabled(!fastModeCheckbox.isSelected());
            rotateEndTextZ.setEnabled(!fastModeCheckbox.isSelected());
            coarseRateTextZ.setEnabled(!fastModeCheckbox.isSelected());
            fineRateTextZ.setEnabled(!fastModeCheckbox.isSelected());
        } else if (event.getSource() == calcCOGCheckbox) {

            // enable or disable search variables
            calcCOG = calcCOGCheckbox.isSelected();
        } else if ((event.getSource() == weightRadio) || (event.getSource() == noneRadio) ||
                       (event.getSource() == voiRadio)) {
            buttonWeightRef.setEnabled(weightRadio.isSelected());
            buttonWeightInput.setEnabled(weightRadio.isSelected());

            if (weightRadio.isSelected()) {
                comboBoxDOF.setSelectedIndex(3);
            } // if (weightRadio.isSelected())
        } // else if ((event.getSource() == weightRadio) || (event.getSource() == noneRadio) ||

        // (event.getSource() == voiRadio))
        else if (event.getSource() == universalCheckbox) {

            if (universalCheckbox.isSelected()) {
                xRadio.setEnabled(false);
                yRadio.setEnabled(false);
                zRadio.setEnabled(false);
                xRadio.setSelected(true);
                yRadio.setSelected(false);
                zRadio.setSelected(false);

                if (xSelected) {
                    return;
                } else if (ySelected) {
                    rotatePanel.remove(rotateRangePanelY);
                    rotatePanel.remove(coarsePanelY);
                    rotatePanel.remove(finePanelY);
                    ySelected = false;
                } else { // if (zSelected)
                    rotatePanel.remove(rotateRangePanelZ);
                    rotatePanel.remove(coarsePanelZ);
                    rotatePanel.remove(finePanelZ);
                    zSelected = false;
                } // else if zSelected

                xSelected = true;
                gbc.gridx = 0;
                gbc.gridy = 2;
                gbc.gridwidth = 1;
                gbc.anchor = GridBagConstraints.WEST;
                rotatePanel.add(rotateRangePanelX, gbc);

                gbc.gridx = 0;
                gbc.gridy = 3;
                gbc.gridwidth = GridBagConstraints.REMAINDER;
                rotatePanel.add(coarsePanelX, gbc);

                gbc.gridx = 0;
                gbc.gridy = 4;
                gbc.gridwidth = GridBagConstraints.REMAINDER;
                rotatePanel.add(finePanelX, gbc);
            } else {
                xRadio.setEnabled(true);
                yRadio.setEnabled(true);
                zRadio.setEnabled(true);
            }
        } // else if (event.getSource() == universalCheckbox)
        else if ((event.getSource() == xRadio) || (event.getSource() == yRadio) || (event.getSource() == zRadio)) {

            if (xRadio.isSelected()) {

                if (xSelected) {
                    return;
                } else if (ySelected) {
                    rotatePanel.remove(rotateRangePanelY);
                    rotatePanel.remove(coarsePanelY);
                    rotatePanel.remove(finePanelY);
                    ySelected = false;
                } else { // if (zSelected)
                    rotatePanel.remove(rotateRangePanelZ);
                    rotatePanel.remove(coarsePanelZ);
                    rotatePanel.remove(finePanelZ);
                    zSelected = false;
                } // else if zSelected

                xSelected = true;
                gbc.gridx = 0;
                gbc.gridy = 2;
                gbc.gridwidth = 1;
                gbc.anchor = GridBagConstraints.WEST;
                rotatePanel.add(rotateRangePanelX, gbc);

                gbc.gridx = 0;
                gbc.gridy = 3;
                gbc.gridwidth = GridBagConstraints.REMAINDER;
                rotatePanel.add(coarsePanelX, gbc);

                gbc.gridx = 0;
                gbc.gridy = 4;
                gbc.gridwidth = GridBagConstraints.REMAINDER;
                rotatePanel.add(finePanelX, gbc);
            } // if (xRadio.isSelected)
            else if (yRadio.isSelected()) {

                if (xSelected) {
                    rotatePanel.remove(rotateRangePanelX);
                    rotatePanel.remove(coarsePanelX);
                    rotatePanel.remove(finePanelX);
                    xSelected = false;
                } // if (xSelected)
                else if (ySelected) {
                    return;
                } else { // zSelected
                    rotatePanel.remove(rotateRangePanelZ);
                    rotatePanel.remove(coarsePanelZ);
                    rotatePanel.remove(finePanelZ);
                    zSelected = false;
                } // else zSelected

                ySelected = true;
                gbc.gridx = 0;
                gbc.gridy = 2;
                gbc.gridwidth = 1;
                gbc.anchor = GridBagConstraints.WEST;
                rotatePanel.add(rotateRangePanelY, gbc);

                gbc.gridx = 0;
                gbc.gridy = 3;
                gbc.gridwidth = GridBagConstraints.REMAINDER;
                rotatePanel.add(coarsePanelY, gbc);

                gbc.gridx = 0;
                gbc.gridy = 4;
                gbc.gridwidth = GridBagConstraints.REMAINDER;
                rotatePanel.add(finePanelY, gbc);
            } // else if (yRadio.isSelected())
            else if (zRadio.isSelected()) {

                if (xSelected) {
                    rotatePanel.remove(rotateRangePanelX);
                    rotatePanel.remove(coarsePanelX);
                    rotatePanel.remove(finePanelX);
                    xSelected = false;
                } // if (xSelcted)
                else if (ySelected) {
                    rotatePanel.remove(rotateRangePanelY);
                    rotatePanel.remove(coarsePanelY);
                    rotatePanel.remove(finePanelY);
                    ySelected = false;
                } // else if (ySelected)
                else { // zSelected
                    return;
                } // else zSelected

                zSelected = true;
                gbc.gridx = 0;
                gbc.gridy = 2;
                gbc.gridwidth = 1;
                gbc.anchor = GridBagConstraints.WEST;
                rotatePanel.add(rotateRangePanelZ, gbc);

                gbc.gridx = 0;
                gbc.gridy = 3;
                gbc.gridwidth = GridBagConstraints.REMAINDER;
                rotatePanel.add(coarsePanelZ, gbc);

                gbc.gridx = 0;
                gbc.gridy = 4;
                gbc.gridwidth = GridBagConstraints.REMAINDER;
                rotatePanel.add(finePanelZ, gbc);
            } // else if (zRadio.isSelected())

            rotatePanel.validate();
            repaint();
        } // else if xRadio, yRadio, or zRadio
    }

    /**
     * Accessor to set the advanced settings.
     *
     * @param  bracketBound   DOCUMENT ME!
     * @param  maxIterations  DOCUMENT ME!
     * @param  numMinima      DOCUMENT ME!
     */
    public void setAdvancedSettings(int bracketBound, int maxIterations, int numMinima) {
        this.bracketBound = bracketBound;
        this.maxIterations = maxIterations;
        this.numMinima = numMinima;
    }

    /**
     * Accessor to set the whether or not to calculate the center of gravity (mass).
     *
     * @param  flag  <code>true</code> then calculate center of gravity (mass).
     */
    public void setCalcCOG(boolean flag) {
        calcCOG = flag;
    }

    /**
     * Accessor to set the coarse sample beginX.
     *
     * @param  x  Coarse beginX
     */
    public void setCoarseBeginX(float x) {
        rotateBeginX = x;
    }

    /**
     * Accessor to set the coarse sample beginY.
     *
     * @param  y  Coarse beginY
     */
    public void setCoarseBeginY(float y) {
        rotateBeginY = y;
    }

    /**
     * Accessor to set the coarse sample beginZ.
     *
     * @param  z  Coarse beginZ
     */
    public void setCoarseBeginZ(float z) {
        rotateBeginZ = z;
    }

    /**
     * Accessor to set the coarse sample endX.
     *
     * @param  x  Coarse endX
     */
    public void setCoarseEndX(float x) {
        rotateEndX = x;
    }

    /**
     * Accessor to set the coarse sample endY.
     *
     * @param  y  Coarse endY
     */
    public void setCoarseEndY(float y) {
        rotateEndY = y;
    }

    /**
     * Accessor to set the coarse sample endZ.
     *
     * @param  z  Coarse endZ
     */
    public void setCoarseEndZ(float z) {
        rotateEndZ = z;
    }

    /**
     * Accessor to set the coarse sample rateX.
     *
     * @param  x  Coarse rateX
     */
    public void setCoarseRateX(float x) {
        coarseRateX = x;
    }

    /**
     * Accessor to set the coarse sample rateY.
     *
     * @param  y  Coarse rateY
     */
    public void setCoarseRateY(float y) {
        coarseRateY = y;
    }

    /**
     * Accessor to set the coarse sample rateZ.
     *
     * @param  z  Coarse rateZ
     */
    public void setCoarseRateZ(float z) {
        coarseRateZ = z;
    }

    /**
     * Accessor to set the choice of cost function.
     *
     * @param  x  Cost function.
     */
    public void setCostChoice(int x) {
        cost = x;
    }

    /**
     * Accessor to set the display transform flag.
     *
     * @param  flag  <code>true</code> means display the transformed image.
     */
    public void setDisplayTransform(boolean flag) {
        displayTransform = flag;
    }

    /**
     * Accessor to set the degrees of freedom.
     *
     * @param  x  Degrees of freedom
     */
    public void setDOF(int x) {
        DOF = x;
    }

    /**
     * Accessor to set whether or not to execute the fast mode (skip sub sample and goto last final optimization).
     *
     * @param  flag  <code>true</code> then skip to level one (last ) optimization.
     */
    public void setFastMode(boolean flag) {
        fastMode = flag;
    }

    /**
     * Accessor to set the fine sample rateX.
     *
     * @param  x  Fine rateX
     */
    public void setFineRateX(float x) {
        fineRateX = x;
    }

    /**
     * Accessor to set the fine sample rateY.
     *
     * @param  y  Fine rateY
     */
    public void setFineRateY(float y) {
        fineRateY = y;
    }

    /**
     * Accessor to set the fine sample rateZ.
     *
     * @param  z  Fine rateZ
     */
    public void setFineRateZ(float z) {
        fineRateZ = z;
    }

    /**
     * Accessor to set the input weight image.
     *
     * @param  im  Input weight image.
     */
    public void setInputWeightImage(ModelImage im) {
        inputWeightImage = im;
    }

    /**
     * Accessor to set the initial interpolation.
     *
     * @param  x  Interpolation
     */
    public void setInterp(int x) {
        interp = x;
    }

    /**
     * Accessor to set the final interpolation.
     *
     * @param  x  Interpolation
     */
    public void setInterp2(int x) {
        interp2 = x;
    }

    /**
     * Accessor to set the maximum resolutions flag.
     *
     * @param  flag  <code>true</code> then use the maximum of minimums of the resolutions of the images.
     */
    public void setMaxOfMinResol(boolean flag) {
        maxOfMinResol = flag;
    }

    /**
     * Accessor to set the reference image.
     *
     * @param  im  Reference image.
     */
    public void setReferenceImage(ModelImage im) {
        refImage = im;
    }

    /**
     * Accessor to set the reference weight image.
     *
     * @param  im  Reference weight image.
     */
    public void setReferenceWeightImage(ModelImage im) {
        refWeightImage = im;
    }

    /**
     * Accessor to set whether or not subsampling occurs.
     *
     * @param  doSubsample  DOCUMENT ME!
     */
    public void setSubsample(boolean doSubsample) {
        this.doSubsample = doSubsample;
    }

    /**
     * Accessor to set the VOIs only flag.
     *
     * @param  flag  <code>true</code> then only register the parts of the images in the VOIs.
     */
    public void setVoisOnly(boolean flag) {
        voisOnly = flag;
    }

    /**
     * Accessor to set the weighted images flag.
     *
     * @param  flag  <code>true</code> means there are weighted images.
     */
    public void setWeighted(boolean flag) {
        weighted = flag;
    }

    /**
     * Calls the algorithm with the set-up parameters.
     */
    protected void callAlgorithm() {

        if (doLS) {
            JDialogRegistrationLeastSquares lsDialog = new JDialogRegistrationLeastSquares(parentFrame, matchImage,
                                                                                           refImage);
            lsCompleted = lsDialog.getLSCompleted();

            if (!lsCompleted) {
                lsDialog.dispose();

                return;
            }

            lsMatrix = matchImage.getMatrix();
            lsImage = lsDialog.getResultImage();
            lsDialog.dispose();
        }


        if (voisOnly && !doLS) {
            float[] refRes = new float[] {
                                 refImage.getFileInfo(0).getResolutions()[0],
                                 refImage.getFileInfo(0).getResolutions()[1],
                                 refImage.getFileInfo(0).getResolutions()[2]
                             };
            float[] matchRes = new float[] {
                                   matchImage.getFileInfo(0).getResolutions()[0],
                                   matchImage.getFileInfo(0).getResolutions()[1],
                                   matchImage.getFileInfo(0).getResolutions()[2]
                               };

            refWeightImage = new ModelImage(ModelStorageBase.BYTE, refImage.getExtents(), "VOI ref");
            inputWeightImage = new ModelImage(ModelStorageBase.BYTE, matchImage.getExtents(), "VOI match");

            refWeightImage.getFileInfo(0).setResolutions(refRes);
            inputWeightImage.getFileInfo(0).setResolutions(matchRes);

            // make new reference and input images based on the VOIs in them.
            // pass those new images to the registration algorithm
            BitSet mask = refImage.generateVOIMask();
            int imageSize = refImage.getSliceSize() * refImage.getExtents()[2];

            for (int i = 0; i < imageSize; i++) {

                if (!mask.get(i)) {
                    refWeightImage.set(i, 0);
                } else {
                    refWeightImage.set(i, 1);
                }
            }

            mask = matchImage.generateVOIMask();
            imageSize = matchImage.getSliceSize() * matchImage.getExtents()[2];

            for (int i = 0; i < imageSize; i++) {

                if (!mask.get(i)) {
                    inputWeightImage.set(i, 0);
                } else {
                    inputWeightImage.set(i, 1);
                }
            }

            weighted = true;
        } // if (voisOnly)

        if (weighted) {

            if (!doLS) {
                reg3 = new AlgorithmRegOAR3D(refImage, matchImage, refWeightImage, inputWeightImage, cost, DOF, interp,
                                             rotateBeginX, rotateEndX, coarseRateX, fineRateX, rotateBeginY, rotateEndY,
                                             coarseRateY, fineRateY, rotateBeginZ, rotateEndZ, coarseRateZ, fineRateZ,
                                             maxOfMinResol, doSubsample, fastMode, bracketBound, maxIterations,
                                             numMinima);
            } else {
                reg3 = new AlgorithmRegOAR3D(refImage, lsImage, refWeightImage, inputWeightImage, cost, DOF, interp,
                                             rotateBeginX, rotateEndX, coarseRateX, fineRateX, rotateBeginY, rotateEndY,
                                             coarseRateY, fineRateY, rotateBeginZ, rotateEndZ, coarseRateZ, fineRateZ,
                                             maxOfMinResol, doSubsample, fastMode, bracketBound, maxIterations,
                                             numMinima);
            }
        } else {
            // System.out.println("Reference image name is " +refImage.getImageName());
            // System.out.println("Moving image name is " +matchImage.getImageName());

            if (!doLS) {
                reg3 = new AlgorithmRegOAR3D(refImage, matchImage, cost, DOF, interp, rotateBeginX, rotateEndX,
                                             coarseRateX, fineRateX, rotateBeginY, rotateEndY, coarseRateY, fineRateY,
                                             rotateBeginZ, rotateEndZ, coarseRateZ, fineRateZ, maxOfMinResol,
                                             doSubsample, fastMode, bracketBound, maxIterations, numMinima);
            } else {
                System.err.println("Sending LS Image to OAR3D algorithm");
                reg3 = new AlgorithmRegOAR3D(refImage, lsImage, cost, DOF, interp, rotateBeginX, rotateEndX,
                                             coarseRateX, fineRateX, rotateBeginY, rotateEndY, coarseRateY, fineRateY,
                                             rotateBeginZ, rotateEndZ, coarseRateZ, fineRateZ, maxOfMinResol,
                                             doSubsample, fastMode, bracketBound, maxIterations, numMinima);

            }
        }

        reg3.addListener(this);


        createProgressBar(matchImage.getImageName(), reg3);

        // Hide dialog
        setVisible(false);

        if (isRunInSeparateThread()) {

            // Start the thread as a low priority because we wish to still have user interface work fast.
            if (reg3.startMethod(Thread.MIN_PRIORITY) == false) {
                MipavUtil.displayError("A thread is already running on this object");
            }
        } else {
            reg3.run();
        }

    }

    /**
     * Store the result image in the script runner's image table now that the action execution is finished.
     */
    protected void doPostAlgorithmActions() {
        AlgorithmParameters.storeImageInRunner(getResultImage());
    }

    /**
     * {@inheritDoc}
     */
    protected void setGUIFromParams() {
        matchImage = scriptParameters.retrieveInputImage();
        UI = ViewUserInterface.getReference();
        parentFrame = matchImage.getParentFrame();

        if (matchImage.isColorImage()) {
            doColor = true;
        } else {
            doColor = false;
        }

        setReferenceImage(scriptParameters.retrieveImage("reference_image"));

        setWeighted(scriptParameters.getParams().getBoolean("do_use_weight_images"));

        if (weighted) {
            setInputWeightImage(scriptParameters.retrieveImage("input_weight_image"));
            setReferenceWeightImage(scriptParameters.retrieveImage("reference_weight_image"));
        }

        setDOF(scriptParameters.getParams().getInt("degrees_of_freedom"));
        setInterp(scriptParameters.getParams().getInt("initial_interpolation_type"));
        setCostChoice(scriptParameters.getParams().getInt("cost_function_type"));

        float[] rotBegin = scriptParameters.getParams().getList("rotate_begin").getAsFloatArray();
        float[] rotEnd = scriptParameters.getParams().getList("rotate_end").getAsFloatArray();
        float[] coarseRates = scriptParameters.getParams().getList("coarse_rate").getAsFloatArray();
        float[] fineRates = scriptParameters.getParams().getList("fine_rate").getAsFloatArray();

        setCoarseBeginX(rotBegin[0]);
        setCoarseEndX(rotEnd[0]);
        setCoarseRateX(coarseRates[0]);
        setFineRateX(fineRates[0]);

        setCoarseBeginY(rotBegin[1]);
        setCoarseEndY(rotEnd[1]);
        setCoarseRateY(coarseRates[1]);
        setFineRateY(fineRates[1]);

        setCoarseBeginZ(rotBegin[2]);
        setCoarseEndZ(rotEnd[2]);
        setCoarseRateZ(coarseRates[2]);
        setFineRateZ(fineRates[2]);

        setDisplayTransform(scriptParameters.getParams().getBoolean("do_display_transform"));
        setInterp2(scriptParameters.getParams().getInt("final_interpolation_type"));

        setMaxOfMinResol(scriptParameters.getParams().getBoolean("do_use_max_of_min_resolutions"));
        setSubsample(scriptParameters.getParams().getBoolean("do_subsample"));
        setFastMode(scriptParameters.getParams().getBoolean("do_use_fast_mode"));
        setCalcCOG(scriptParameters.getParams().getBoolean("do_calc_COG"));

        setAdvancedSettings(scriptParameters.getParams().getInt("bracket_bound"),
                            scriptParameters.getParams().getInt("max_iterations"),
                            scriptParameters.getParams().getInt("num_minima"));
    }

    /**
     * {@inheritDoc}
     */
    protected void storeParamsFromGUI() throws ParserException {
        scriptParameters.storeInputImage(matchImage);
        scriptParameters.storeImage(refImage, "reference_image");

        scriptParameters.getParams().put(ParameterFactory.newParameter("do_use_weight_images", weighted));

        if (weighted) {
            scriptParameters.storeImage(inputWeightImage, "input_weight_image");
            scriptParameters.storeImage(refWeightImage, "reference_weight_image");
        }

        scriptParameters.storeImageInRecorder(getResultImage());

        scriptParameters.getParams().put(ParameterFactory.newParameter("degrees_of_freedom", DOF));
        scriptParameters.getParams().put(ParameterFactory.newParameter("initial_interpolation_type", interp));
        scriptParameters.getParams().put(ParameterFactory.newParameter("final_interpolation_type", interp2));
        scriptParameters.getParams().put(ParameterFactory.newParameter("cost_function_type", cost));
        scriptParameters.getParams().put(ParameterFactory.newParameter("rotate_begin",
                                                                       new float[] {
                                                                           rotateBeginX, rotateBeginY, rotateBeginZ
                                                                       }));
        scriptParameters.getParams().put(ParameterFactory.newParameter("rotate_end",
                                                                       new float[] {
                                                                           rotateEndX, rotateEndY, rotateEndZ
                                                                       }));
        scriptParameters.getParams().put(ParameterFactory.newParameter("coarse_rate",
                                                                       new float[] {
                                                                           coarseRateX, coarseRateY, coarseRateZ
                                                                       }));
        scriptParameters.getParams().put(ParameterFactory.newParameter("fine_rate",
                                                                       new float[] { fineRateX, fineRateY, fineRateZ }));
        scriptParameters.getParams().put(ParameterFactory.newParameter("do_display_transform", displayTransform));
        scriptParameters.getParams().put(ParameterFactory.newParameter("do_use_max_of_min_resolutions", maxOfMinResol));
        scriptParameters.getParams().put(ParameterFactory.newParameter("do_subsample", doSubsample));
        scriptParameters.getParams().put(ParameterFactory.newParameter("do_use_fast_mode", fastMode));
        scriptParameters.getParams().put(ParameterFactory.newParameter("do_calc_COG", calcCOG));
        scriptParameters.getParams().put(ParameterFactory.newParameter("bracket_bound", bracketBound));
        scriptParameters.getParams().put(ParameterFactory.newParameter("max_iterations", maxIterations));
        scriptParameters.getParams().put(ParameterFactory.newParameter("num_minima", numMinima));
    }

    /**
     * Build advanced settings dialog. Returns JDialog.
     *
     * @param   bracketBound  DOCUMENT ME!
     * @param   maxIter       DOCUMENT ME!
     * @param   numMinima     DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    private JDialog buildAdvancedDialog(int bracketBound, int maxIter, int numMinima) {
        serif12 = MipavUtil.font12;
        serif12B = MipavUtil.font12B;

        advancedDialog = new JDialog(this, "Advanced OAR settings", false);
        // Parent is the JDialogRegistrationOAR3D, title, modal
        // Changed to non-modal after adding Help button 12/17/07

        // Setting panel
        JPanel settingsPanel = new JPanel();
        settingsPanel.setBorder(BorderFactory.createTitledBorder("Optimization settings"));
        settingsPanel.setLayout(new BoxLayout(settingsPanel, BoxLayout.Y_AXIS));

        JPanel bracketPanel = new JPanel();
        bracketPanel.setLayout(new BorderLayout(1, 3)); // BorderLayout(int hgap, int vgap)
        bracketPanel.setBorder(BorderFactory.createEmptyBorder(0, 10, 0, 10));

        JLabel bracketBoundLabel = new JLabel("Multiple of tolerance to bracket the minimum: ", JLabel.LEFT);
        bracketPanel.add(bracketBoundLabel, BorderLayout.WEST);
        bracketPanel.setToolTipText("Used for translation, scale and skew.");
        bracketBoundText = new JTextField(String.valueOf(bracketBound), 5);
        bracketBoundText.addFocusListener(this);
        bracketPanel.add(bracketBoundText, BorderLayout.CENTER);

        JLabel bracketInstruct = new JLabel("Recommended values 10-60.", JLabel.RIGHT);
        bracketPanel.add(bracketInstruct, BorderLayout.SOUTH);

        JPanel maxIterPanel = new JPanel();
        maxIterPanel.setLayout(new BorderLayout(1, 3));
        maxIterPanel.setBorder(BorderFactory.createEmptyBorder(0, 10, 0, 10));

        JLabel maxIterationsLabel = new JLabel("Number of iterations: ", JLabel.LEFT);
        maxIterPanel.add(maxIterationsLabel, BorderLayout.WEST);
        maxIterPanel.setToolTipText("Used for levelOne. Other levels are multiples of this #.");
        maxIterationsText = new JTextField(String.valueOf(maxIter), 5);
        maxIterationsText.addFocusListener(this);
        maxIterPanel.add(maxIterationsText, BorderLayout.CENTER);

        JLabel maxIterInstruct = new JLabel("Recommended value 1-5.", JLabel.RIGHT);
        maxIterPanel.add(maxIterInstruct, BorderLayout.SOUTH);

        JPanel numMinPanel = new JPanel();
        numMinPanel.setLayout(new BorderLayout(1, 3));
        numMinPanel.setBorder(BorderFactory.createEmptyBorder(0, 10, 0, 10));

        JLabel numMinLabel = new JLabel("Number of minima from Level 8 to test at Level 4: ", JLabel.LEFT);
        numMinPanel.add(numMinLabel, BorderLayout.WEST);
        numMinPanel.setToolTipText("Increasing will significantly increase processing time.");
        numMinText = new JTextField(String.valueOf(numMinima), 5);
        numMinText.addFocusListener(this);
        numMinPanel.add(numMinText, BorderLayout.CENTER);

        settingsPanel.add(bracketPanel);
        settingsPanel.add(Box.createVerticalStrut(20));
        settingsPanel.add(maxIterPanel);
        settingsPanel.add(Box.createVerticalStrut(20));
        settingsPanel.add(numMinPanel);
        settingsPanel.add(Box.createVerticalStrut(15));
        settingsPanel.add(sampleCheckbox);
        settingsPanel.add(Box.createVerticalStrut(10));
        settingsPanel.add(fastModeCheckbox);
        // settingsPanel.add(Box.createVerticalStrut(10));
        // settingsPanel.add(calcCOGCheckbox, Component.LEFT_ALIGNMENT);

        advancedDialog.getContentPane().add(settingsPanel, BorderLayout.NORTH);

        // Okay-Cancel Panel
        JPanel okayCancelPanel = new JPanel(new FlowLayout());
        JButton advCancelButton = new JButton("Cancel");
        advCancelButton.setActionCommand("AdvancedCancel");
        advCancelButton.addActionListener(this);
        advCancelButton.setPreferredSize(new Dimension(120, 30));
        advCancelButton.setFont(serif12B);

        // okayCancelPanel.add(cancelButton);
        JButton okayButton = new JButton("OK");
        okayButton.setActionCommand("AdvancedOkay");
        okayButton.addActionListener(this);
        okayButton.setPreferredSize(new Dimension(120, 30));
        okayButton.setFont(serif12B);
        
        // Help Button
        JButton helpButton = new JButton("Help");
        helpButton.setActionCommand("AdvancedHelp");
        helpButton.addActionListener(this);
        helpButton.setPreferredSize(new Dimension(120,30));
        helpButton.setFont(serif12B);
        
        okayCancelPanel.add(okayButton);
        okayCancelPanel.add(advCancelButton);
        okayCancelPanel.add(helpButton);

        advancedDialog.getContentPane().add(okayCancelPanel, BorderLayout.SOUTH);

        Rectangle dialogBounds = this.getBounds();
        advancedDialog.setLocation((int) ((Toolkit.getDefaultToolkit().getScreenSize().width * 0.75) -
                                          (dialogBounds.width / 2)),
                                   (Toolkit.getDefaultToolkit().getScreenSize().height / 2) -
                                   (dialogBounds.height / 2));

        advancedDialog.pack();
        advancedDialog.setVisible(true);


        return advancedDialog;
    }

    /**
     * Builds a list of images. Returns combobox.
     *
     * @param   image  DOCUMENT ME!
     *
     * @return  Newly created combo box.
     */
    private JComboBox buildImgComboBox(ModelImage image) {
        JComboBox comboBox = new JComboBox();
        comboBox.setFont(serif12);
        comboBox.setBackground(Color.white);

        Enumeration names = UI.getRegisteredImageNames();

        while (names.hasMoreElements()) {
            String name = (String) names.nextElement();

            if (!name.equals(image.getImageName())) {
                ModelImage img = UI.getRegisteredImageByName(name);

                if ((image.getNDims() == img.getNDims()) && (image.isColorImage() == img.isColorImage()) &&
                        (UI.getFrameContainingImage(img) != null)) {
                    comboBox.addItem(name);
                }
            }
        }

        return comboBox;
    }

    /**
     * Initializes the GUI components and displays the dialog.
     */
    private void init() {
        setForeground(Color.black);
        setTitle("Optimized Automatic Image Registration 3D");

        JPanel optPanel = new JPanel();
        optPanel.setLayout(new GridBagLayout());
        optPanel.setBorder(buildTitledBorder("Input Options"));

        String matchName = matchImage.getImageName();
        JLabel labelImage = new JLabel("Register [" + matchName + "] to:");
        labelImage.setForeground(Color.black);
        labelImage.setFont(serif12);
        labelImage.setAlignmentX(Component.LEFT_ALIGNMENT);

        comboBoxImage = buildImgComboBox(matchImage);

        JLabel labelDOF = new JLabel("Degrees of freedom:");
        labelDOF.setForeground(Color.black);
        labelDOF.setFont(serif12);
        labelDOF.setAlignmentX(Component.LEFT_ALIGNMENT);

        comboBoxDOF = new JComboBox();
        comboBoxDOF.setFont(MipavUtil.font12);
        comboBoxDOF.setBackground(Color.white);
        comboBoxDOF.setToolTipText("Degrees of freedom");
        comboBoxDOF.addItem("Rigid - 6");
        comboBoxDOF.addItem("Global rescale - 7");
        comboBoxDOF.addItem("Specific rescale - 9");
        comboBoxDOF.addItem("Affine - 12");
        comboBoxDOF.setSelectedIndex(3);
        comboBoxDOF.addItemListener(this);

        JLabel labelCost = new JLabel("Cost function:");
        labelCost.setForeground(Color.black);
        labelCost.setFont(serif12);
        labelCost.setAlignmentX(Component.LEFT_ALIGNMENT);

        comboBoxCostFunct = new JComboBox();
        comboBoxCostFunct.setFont(MipavUtil.font12);
        comboBoxCostFunct.setBackground(Color.white);
        comboBoxCostFunct.setToolTipText("Cost function");

        if (!doColor) {
            comboBoxCostFunct.addItem("Correlation ratio");
        }

        // comboBoxCostFunct.addItem("Correlation ratio smoothed");
        comboBoxCostFunct.addItem("Least squares");

        // comboBoxCostFunct.addItem("Least squares smoothed");
        // comboBoxCostFunct.addItem("Mutual information");
        // comboBoxCostFunct.addItem("Mutual information smoothed");
        if (!doColor) {
            comboBoxCostFunct.addItem("Normalized cross correlation");
        }

        // comboBoxCostFunct.addItem("Normalized cross correlation smoothed");
        if (!doColor) {
            comboBoxCostFunct.addItem("Normalized mutual information");
        }

        // comboBoxCostFunct.addItem("Normalized mutual information smoothed");
        comboBoxCostFunct.setSelectedIndex(0);

        JLabel labelInterp = new JLabel("Interpolation:");
        labelInterp.setForeground(Color.black);
        labelInterp.setFont(serif12);
        labelInterp.setAlignmentX(Component.LEFT_ALIGNMENT);

        comboBoxInterp = new JComboBox();
        comboBoxInterp.setFont(serif12);
        comboBoxInterp.setBackground(Color.white);
        comboBoxInterp.setAlignmentX(Component.LEFT_ALIGNMENT);

        comboBoxInterp.addItem("Trilinear");
        comboBoxInterp.addItem("Bspline 3rd order");
        comboBoxInterp.addItem("Bspline 4th order");
        comboBoxInterp.addItem("Cubic Lagrangian");
        comboBoxInterp.addItem("Quintic Lagrangian");
        comboBoxInterp.addItem("Heptic Lagrangian");
        comboBoxInterp.addItem("Windowed sinc");
        // comboBoxInterp.addItem("Nearest Neighbor");

        minMaxCheckbox = new JCheckBox("Use the max of the min resolutions of the two datasets when resampling.");
        minMaxCheckbox.setFont(serif12);
        minMaxCheckbox.setForeground(Color.black);
        minMaxCheckbox.setSelected(true);
        minMaxCheckbox.addItemListener(this);

        // Note the next 3 checkboxes are initialized here, for cases when the user doesn't
        // choose to edit the Advanced Settings.  They will only be made visible in the
        // Advanced Settings dialog.
        sampleCheckbox = new JCheckBox("Subsample image for speed");
        sampleCheckbox.setFont(serif12);
        sampleCheckbox.setForeground(Color.black);
        sampleCheckbox.setSelected(true);
        sampleCheckbox.setEnabled(true);
        sampleCheckbox.setAlignmentX(Component.LEFT_ALIGNMENT);

        fastModeCheckbox = new JCheckBox("Skip multilevel search.  Assume images are close to alignment.");
        fastModeCheckbox.setFont(serif12);
        fastModeCheckbox.setForeground(Color.black);
        fastModeCheckbox.setSelected(false);
        fastModeCheckbox.setEnabled(true);
        fastModeCheckbox.addItemListener(this);
        fastModeCheckbox.setAlignmentX(Component.LEFT_ALIGNMENT);

        calcLSBox = new JCheckBox("Initialize registration process by applying Least Squares", false);
        calcLSBox.setFont(serif12);
        calcLSBox.setForeground(Color.black);

        Insets insets = new Insets(0, 2, 0, 2);
        gbc = new GridBagConstraints();
        gbc.gridx = 0;
        gbc.gridy = 0;
        gbc.gridwidth = 1;
        gbc.gridheight = 1;
        gbc.insets = insets;
        gbc.weightx = 0;
        gbc.fill = GridBagConstraints.NONE;
        gbc.anchor = GridBagConstraints.WEST;
        optPanel.add(labelImage, gbc);
        gbc.gridx = 1;
        gbc.weightx = 1;
        gbc.fill = GridBagConstraints.HORIZONTAL;
        optPanel.add(comboBoxImage, gbc);

        gbc.gridx = 0;
        gbc.gridy = 2;
        gbc.weightx = 0;
        gbc.fill = GridBagConstraints.NONE;
        optPanel.add(labelDOF, gbc);
        gbc.gridx = 1;
        gbc.gridy = 2;
        gbc.weightx = 1;
        gbc.fill = GridBagConstraints.HORIZONTAL;
        optPanel.add(comboBoxDOF, gbc);

        gbc.gridx = 0;
        gbc.gridy = 3;
        gbc.weightx = 0;
        gbc.fill = GridBagConstraints.NONE;
        optPanel.add(labelInterp, gbc);
        gbc.gridx = 1;
        gbc.gridy = 3;
        gbc.weightx = 1;
        gbc.fill = GridBagConstraints.HORIZONTAL;
        optPanel.add(comboBoxInterp, gbc);

        gbc.gridx = 0;
        gbc.gridy = 4;
        gbc.weightx = 0;
        gbc.fill = GridBagConstraints.NONE;
        optPanel.add(labelCost, gbc);
        gbc.gridx = 1;
        gbc.gridy = 4;
        gbc.weightx = 1;
        gbc.fill = GridBagConstraints.HORIZONTAL;
        optPanel.add(comboBoxCostFunct, gbc);

        gbc.gridx = 0;
        gbc.gridy = 5;
        gbc.weightx = 1;
        gbc.fill = GridBagConstraints.HORIZONTAL;
        gbc.gridwidth = 7;
        optPanel.add(minMaxCheckbox, gbc);


        gbc.gridx = 0;
        gbc.gridy = 6;
        gbc.weightx = 1;
        gbc.fill = GridBagConstraints.REMAINDER;
        optPanel.add(calcLSBox, gbc);

        universalCheckbox = new JCheckBox("Apply same rotations to all dimensions.");
        universalCheckbox.setFont(serif12);
        universalCheckbox.setForeground(Color.black);
        universalCheckbox.setSelected(true);
        universalCheckbox.addItemListener(this);

        ButtonGroup dimensionGroup = new ButtonGroup();

        xRadio = new JRadioButton("X");
        xRadio.setFont(serif12);
        xRadio.setForeground(Color.black);
        xRadio.setAlignmentX(Component.LEFT_ALIGNMENT);
        xRadio.setSelected(true);
        xRadio.setEnabled(false);
        xRadio.addItemListener(this);
        dimensionGroup.add(xRadio);

        yRadio = new JRadioButton("Y");
        yRadio.setFont(serif12);
        yRadio.setForeground(Color.black);
        yRadio.setSelected(false);
        yRadio.setEnabled(false);
        yRadio.addItemListener(this);
        dimensionGroup.add(yRadio);

        zRadio = new JRadioButton("Z");
        zRadio.setFont(serif12);
        zRadio.setForeground(Color.black);
        zRadio.setSelected(false);
        zRadio.setEnabled(false);
        zRadio.addItemListener(this);
        dimensionGroup.add(zRadio);

        JPanel xyzPanel = new JPanel(new FlowLayout(FlowLayout.LEFT));
        xyzPanel.setAlignmentX(Component.LEFT_ALIGNMENT);

        xyzPanel.add(xRadio);
        xyzPanel.add(yRadio);
        xyzPanel.add(zRadio);


        // Rotation Range Panel
        rotateRangePanelX = new JPanel(new FlowLayout(FlowLayout.LEFT));
        rotateRangePanelX.setAlignmentX(Component.LEFT_ALIGNMENT);

        JLabel labelRotateRangeX = new JLabel("Rotation angle sampling range:");
        labelRotateRangeX.setForeground(Color.black);
        labelRotateRangeX.setFont(serif12);

        JLabel labelRotateRangeToX = new JLabel("to");
        labelRotateRangeToX.setForeground(Color.black);
        labelRotateRangeToX.setFont(serif12);

        JLabel labelRotateDegreesX = new JLabel("degrees");
        labelRotateDegreesX.setFont(serif12);

        rotateBeginTextX = new JTextField("-30", 3);
        rotateEndTextX = new JTextField("30", 3);

        rotateRangePanelX.add(labelRotateRangeX);
        rotateRangePanelX.add(rotateBeginTextX);
        rotateRangePanelX.add(labelRotateRangeToX);
        rotateRangePanelX.add(rotateEndTextX);
        rotateRangePanelX.add(labelRotateDegreesX);

        // Coarse sampling rate panel
        coarsePanelX = new JPanel(new FlowLayout(FlowLayout.LEFT));
        coarsePanelX.setAlignmentX(Component.LEFT_ALIGNMENT);

        JLabel labelCoarseX = new JLabel("Coarse angle increment: ");
        labelCoarseX.setForeground(Color.black);
        labelCoarseX.setFont(serif12);
        labelCoarseX.setAlignmentX(Component.LEFT_ALIGNMENT);

        JLabel labelCoarseDegreesX = new JLabel("degrees");
        labelCoarseDegreesX.setFont(serif12);
        coarseRateTextX = new JTextField("15", 3);

        coarsePanelX.add(labelCoarseX);
        coarsePanelX.add(coarseRateTextX);
        coarsePanelX.add(labelCoarseDegreesX);
        coarsePanelX.setAlignmentX(Component.LEFT_ALIGNMENT);

        // Fine sampling rate panel
        finePanelX = new JPanel(new FlowLayout(FlowLayout.LEFT));

        JLabel labelFineX = new JLabel("Fine angle increment:");
        labelFineX.setForeground(Color.black);
        labelFineX.setFont(serif12);
        labelFineX.setAlignmentX(Component.LEFT_ALIGNMENT);

        JLabel labelFineDegreesX = new JLabel("degrees");
        labelFineDegreesX.setFont(serif12);
        fineRateTextX = new JTextField("6", 3);

        finePanelX.add(labelFineX);
        finePanelX.add(fineRateTextX);
        finePanelX.add(labelFineDegreesX);
        finePanelX.setAlignmentX(Component.LEFT_ALIGNMENT);

        rotatePanel = new JPanel();
        rotatePanel.setLayout(new GridBagLayout());
        rotatePanel.setBorder(buildTitledBorder("Rotations"));

        gbc.fill = GridBagConstraints.HORIZONTAL;
        gbc.weightx = 1;
        gbc.gridx = 0;
        gbc.gridy = 0;
        gbc.gridwidth = 1;
        rotatePanel.add(universalCheckbox, gbc);

        gbc.gridx = 0;
        gbc.gridy = 1;
        gbc.gridwidth = GridBagConstraints.REMAINDER;
        gbc.anchor = GridBagConstraints.WEST;
        rotatePanel.add(xyzPanel, gbc);

        gbc.gridx = 0;
        gbc.gridy = 2;
        gbc.gridwidth = 1;
        gbc.anchor = GridBagConstraints.WEST;
        rotatePanel.add(rotateRangePanelX, gbc);

        gbc.gridx = 0;
        gbc.gridy = 3;
        gbc.gridwidth = GridBagConstraints.REMAINDER;
        rotatePanel.add(coarsePanelX, gbc);

        gbc.gridx = 0;
        gbc.gridy = 4;
        gbc.gridwidth = GridBagConstraints.REMAINDER;
        rotatePanel.add(finePanelX, gbc);

        rotateRangePanelY = new JPanel(new FlowLayout(FlowLayout.LEFT));
        rotateRangePanelY.setAlignmentX(Component.LEFT_ALIGNMENT);

        JLabel labelRotateRangeY = new JLabel("Rotation angle sampling range:");
        labelRotateRangeY.setForeground(Color.black);
        labelRotateRangeY.setFont(serif12);

        JLabel labelRotateRangeToY = new JLabel("to");
        labelRotateRangeToY.setForeground(Color.black);
        labelRotateRangeToY.setFont(serif12);

        JLabel labelRotateDegreesY = new JLabel("degrees");
        labelRotateDegreesY.setFont(serif12);

        rotateBeginTextY = new JTextField("-30", 3);
        rotateEndTextY = new JTextField("30", 3);

        rotateRangePanelY.add(labelRotateRangeY);
        rotateRangePanelY.add(rotateBeginTextY);
        rotateRangePanelY.add(labelRotateRangeToY);
        rotateRangePanelY.add(rotateEndTextY);
        rotateRangePanelY.add(labelRotateDegreesY);

        // Coarse sampling rate panel
        coarsePanelY = new JPanel(new FlowLayout(FlowLayout.LEFT));
        coarsePanelY.setAlignmentX(Component.LEFT_ALIGNMENT);

        JLabel labelCoarseY = new JLabel("Coarse angle increment: ");
        labelCoarseY.setForeground(Color.black);
        labelCoarseY.setFont(serif12);
        labelCoarseY.setAlignmentX(Component.LEFT_ALIGNMENT);

        JLabel labelCoarseDegreesY = new JLabel("degrees");
        labelCoarseDegreesY.setFont(serif12);

        coarseRateTextY = new JTextField("15", 3);

        coarsePanelY.add(labelCoarseY);
        coarsePanelY.add(coarseRateTextY);
        coarsePanelY.add(labelCoarseDegreesY);
        coarsePanelY.setAlignmentX(Component.LEFT_ALIGNMENT);

        // Fine sampling rate panel
        finePanelY = new JPanel(new FlowLayout(FlowLayout.LEFT));

        JLabel labelFineY = new JLabel("Fine angle increment:");
        labelFineY.setForeground(Color.black);
        labelFineY.setFont(serif12);
        labelFineY.setAlignmentX(Component.LEFT_ALIGNMENT);

        JLabel labelFineDegreesY = new JLabel("degrees");
        labelFineDegreesY.setFont(serif12);

        fineRateTextY = new JTextField("6", 3);

        finePanelY.add(labelFineY);
        finePanelY.add(fineRateTextY);
        finePanelY.add(labelFineDegreesY);
        finePanelY.setAlignmentX(Component.LEFT_ALIGNMENT);

        rotateRangePanelZ = new JPanel(new FlowLayout(FlowLayout.LEFT));
        rotateRangePanelZ.setAlignmentX(Component.LEFT_ALIGNMENT);

        JLabel labelRotateRangeZ = new JLabel("Rotation angle sampling range:");
        labelRotateRangeZ.setForeground(Color.black);
        labelRotateRangeZ.setFont(serif12);

        JLabel labelRotateRangeToZ = new JLabel("to");
        labelRotateRangeToZ.setForeground(Color.black);
        labelRotateRangeToZ.setFont(serif12);

        JLabel labelRotateDegreesZ = new JLabel("degrees");
        labelRotateDegreesZ.setFont(serif12);

        rotateBeginTextZ = new JTextField("-30", 3);
        rotateEndTextZ = new JTextField("30", 3);

        rotateRangePanelZ.add(labelRotateRangeZ);
        rotateRangePanelZ.add(rotateBeginTextZ);
        rotateRangePanelZ.add(labelRotateRangeToZ);
        rotateRangePanelZ.add(rotateEndTextZ);
        rotateRangePanelZ.add(labelRotateDegreesZ);

        // Coarse sampling rate panel
        coarsePanelZ = new JPanel(new FlowLayout(FlowLayout.LEFT));
        coarsePanelZ.setAlignmentX(Component.LEFT_ALIGNMENT);

        JLabel labelCoarseZ = new JLabel("Coarse angle increment: ");
        labelCoarseZ.setForeground(Color.black);
        labelCoarseZ.setFont(serif12);
        labelCoarseZ.setAlignmentX(Component.LEFT_ALIGNMENT);

        JLabel labelCoarseDegreesZ = new JLabel("degrees");
        labelCoarseDegreesZ.setFont(serif12);

        coarseRateTextZ = new JTextField("15", 3);

        coarsePanelZ.add(labelCoarseZ);
        coarsePanelZ.add(coarseRateTextZ);
        coarsePanelZ.add(labelCoarseDegreesZ);
        coarsePanelZ.setAlignmentX(Component.LEFT_ALIGNMENT);

        // Fine sampling rate panel
        finePanelZ = new JPanel(new FlowLayout(FlowLayout.LEFT));

        JLabel labelFineZ = new JLabel("Fine angle increment:");
        labelFineZ.setForeground(Color.black);
        labelFineZ.setFont(serif12);
        labelFineZ.setAlignmentX(Component.LEFT_ALIGNMENT);

        JLabel labelFineDegreesZ = new JLabel("degrees");
        labelFineDegreesZ.setFont(serif12);

        fineRateTextZ = new JTextField("6", 3);

        finePanelZ.add(labelFineZ);
        finePanelZ.add(fineRateTextZ);
        finePanelZ.add(labelFineDegreesZ);
        finePanelZ.setAlignmentX(Component.LEFT_ALIGNMENT);

        ButtonGroup weightGroup = new ButtonGroup();

        noneRadio = new JRadioButton("No weight");
        noneRadio.setFont(serif12);
        noneRadio.setForeground(Color.black);
        noneRadio.setSelected(true);
        noneRadio.addItemListener(this);
        weightGroup.add(noneRadio);

        voiRadio = new JRadioButton("Register area delineated by VOIs only");
        voiRadio.setFont(serif12);
        voiRadio.setForeground(Color.black);
        voiRadio.setSelected(false);
        voiRadio.addItemListener(this);
        weightGroup.add(voiRadio);

        weightRadio = new JRadioButton("Weight registration");
        weightRadio.setFont(serif12);
        weightRadio.setForeground(Color.black);
        weightRadio.setSelected(false);
        weightRadio.addItemListener(this);
        weightGroup.add(weightRadio);

        buttonWeightRef = new JButton("Choose ref. weight");
        buttonWeightRef.setForeground(Color.black);
        buttonWeightRef.setFont(serif12B);
        buttonWeightRef.setEnabled(false);
        buttonWeightRef.addActionListener(this);
        buttonWeightRef.setActionCommand("Ref");
        buttonWeightRef.setPreferredSize(new Dimension(145, 30));

        textRef = new JTextField();
        textRef.setFont(serif12);
        textRef.setEnabled(false);

        buttonWeightInput = new JButton("Choose input weight");
        buttonWeightInput.setForeground(Color.black);
        buttonWeightInput.setFont(serif12B);
        buttonWeightInput.setEnabled(false);
        buttonWeightInput.addActionListener(this);
        buttonWeightInput.setActionCommand("Input");
        buttonWeightInput.setPreferredSize(buttonWeightRef.getPreferredSize());

        textInput = new JTextField();
        textInput.setFont(serif12);
        textInput.setEnabled(false);

        JPanel weightPanel = new JPanel(new GridBagLayout());
        weightPanel.setBorder(buildTitledBorder("Weighted images"));

        gbc.gridx = 0;
        gbc.gridy = 0;
        gbc.weightx = 0;
        gbc.fill = GridBagConstraints.NONE;
        gbc.anchor = GridBagConstraints.WEST;
        gbc.gridwidth = 2;
        weightPanel.add(noneRadio, gbc);
        gbc.gridx = 0;
        gbc.gridy = 1;
        gbc.weightx = 0;
        gbc.fill = GridBagConstraints.NONE;
        gbc.anchor = GridBagConstraints.WEST;
        weightPanel.add(voiRadio, gbc);
        gbc.gridx = 0;
        gbc.gridy = 2;
        gbc.weightx = 0;
        gbc.fill = GridBagConstraints.NONE;
        gbc.anchor = GridBagConstraints.WEST;
        weightPanel.add(weightRadio, gbc);
        gbc.gridy = 3;
        gbc.fill = GridBagConstraints.NONE;
        gbc.gridwidth = 1;
        weightPanel.add(buttonWeightRef, gbc);
        gbc.gridx = 1;
        gbc.weightx = 1;
        gbc.fill = GridBagConstraints.HORIZONTAL;
        weightPanel.add(textRef, gbc);
        gbc.gridx = 0;
        gbc.gridy = 4;
        gbc.weightx = 0;
        gbc.fill = GridBagConstraints.NONE;
        weightPanel.add(buttonWeightInput, gbc);
        gbc.gridx = 1;
        gbc.weightx = 1;
        gbc.fill = GridBagConstraints.HORIZONTAL;
        weightPanel.add(textInput, gbc);

        JPanel outPanel = new JPanel();
        outPanel.setLayout(new GridBagLayout());
        outPanel.setBorder(buildTitledBorder("Output Options"));

        transformCheckbox = new JCheckBox("Display transformed image");
        transformCheckbox.setFont(serif12);
        transformCheckbox.setForeground(Color.black);
        transformCheckbox.setSelected(true);
        transformCheckbox.addItemListener(this);

        labelInterp2 = new JLabel("Interpolation:");
        labelInterp2.setForeground(Color.black);
        labelInterp2.setFont(serif12);
        labelInterp2.setAlignmentX(Component.LEFT_ALIGNMENT);

        comboBoxInterp2 = new JComboBox();
        comboBoxInterp2.setFont(serif12);
        comboBoxInterp2.setBackground(Color.white);
        comboBoxInterp2.setAlignmentX(Component.LEFT_ALIGNMENT);

        comboBoxInterp2.addItem("Trilinear");
        comboBoxInterp2.addItem("Bspline 3rd order");
        comboBoxInterp2.addItem("Bspline 4th order");
        comboBoxInterp2.addItem("Cubic Lagrangian");
        comboBoxInterp2.addItem("Quintic Lagrangian");
        comboBoxInterp2.addItem("Heptic Lagrangian");
        comboBoxInterp2.addItem("Windowed sinc");
        comboBoxInterp2.addItem("Nearest Neighbor");

        gbc.gridx = 0;
        gbc.gridy = 0;
        gbc.weightx = 0;
        gbc.fill = GridBagConstraints.HORIZONTAL;
        outPanel.add(transformCheckbox, gbc);
        gbc.gridx = 0;
        gbc.gridy = 1;
        gbc.weightx = 0;
        gbc.fill = GridBagConstraints.NONE;
        outPanel.add(labelInterp2, gbc);
        gbc.gridx = 1;
        gbc.gridy = 1;
        gbc.weightx = 1;
        gbc.fill = GridBagConstraints.HORIZONTAL;
        outPanel.add(comboBoxInterp2, gbc);

        JPanel buttonPanel = new JPanel();
        buildOKButton();
        buttonPanel.add(OKButton);
        buildCancelButton();
        buttonPanel.add(cancelButton);
        buildHelpButton();
        buttonPanel.add(helpButton);

        JButton advancedButton = new JButton("Advanced settings");
        advancedButton.setActionCommand("AdvancedSettings");
        advancedButton.addActionListener(this);
        advancedButton.setPreferredSize(new Dimension(140, 30));
        advancedButton.setFont(serif12B);
        buttonPanel.add(advancedButton);

        JPanel mainPanel = new JPanel();
        mainPanel.setLayout(new BoxLayout(mainPanel, BoxLayout.Y_AXIS));
        mainPanel.setBorder(BorderFactory.createEmptyBorder(5, 5, 5, 5));
        optPanel.setAlignmentX(Component.LEFT_ALIGNMENT);
        rotatePanel.setAlignmentX(Component.LEFT_ALIGNMENT);
        weightPanel.setAlignmentX(Component.LEFT_ALIGNMENT);
        outPanel.setAlignmentX(Component.LEFT_ALIGNMENT);
        mainPanel.add(optPanel);
        mainPanel.add(rotatePanel);
        mainPanel.add(weightPanel);
        mainPanel.add(outPanel);

        getContentPane().add(mainPanel);
        getContentPane().add(buttonPanel, BorderLayout.SOUTH);
        pack();
        setVisible(true);
    }

    /**
     * Sets the variables needed to call the registration algorithm based on the values entered in the dialog.
     *
     * @return  <code>true</code> if the variables are properly set, <code>false</code> otherwise.
     */
    private boolean setVariables() {
        int i;

        refImage = UI.getRegisteredImageByName((String) comboBoxImage.getSelectedItem());
        weighted = weightRadio.isSelected();
        maxOfMinResol = minMaxCheckbox.isSelected();
        voisOnly = voiRadio.isSelected();

        doLS = calcLSBox.isSelected();

        if (weighted) {
            fileNameWRef = textRef.getText();
            fileNameWInput = textInput.getText();

            try {
                FileIO fileIO = new FileIO();
                refWeightImage = fileIO.readImage(fileNameWRef, directoryWRef, false, null);

                if (refWeightImage == null) {
                    MipavUtil.displayError("Reference weight image is not valid.");

                    return false;
                } else if (refWeightImage.getNDims() != refImage.getNDims()) {
                    MipavUtil.displayError("Dimensions of reference weight image must match the reference image.");

                    return false;
                }

                for (i = 0; i < refImage.getNDims(); i++) {

                    if (refImage.getExtents()[i] != refWeightImage.getExtents()[i]) {
                        MipavUtil.displayError("Dimensions of reference weight image must match the reference image.");

                        return false;
                    }
                }

                inputWeightImage = fileIO.readImage(fileNameWInput, directoryWInput, false, null);

                if (inputWeightImage == null) {
                    MipavUtil.displayError("Input weight image is not valid.");

                    return false;
                } else if (inputWeightImage.getNDims() != matchImage.getNDims()) {
                    MipavUtil.displayError("Dimensions of input weight image must match the input image.");

                    return false;
                }

                for (i = 0; i < matchImage.getNDims(); i++) {

                    if (matchImage.getExtents()[i] != inputWeightImage.getExtents()[i]) {
                        MipavUtil.displayError("Dimensions of input weight image must match the input image.");

                        return false;
                    }
                }
            } catch (OutOfMemoryError e) {
                MipavUtil.displayError("Out of memory in JDialogRegistrationOAR3D");

                return false;
            }
        }

        if (doColor) {

            if ((!weighted) && (!voisOnly)) {

                switch (comboBoxCostFunct.getSelectedIndex()) {

                    case 0:
                        cost = AlgorithmCostFunctions.LEAST_SQUARES_SMOOTHED_COLOR;
                        costName = "LEAST_SQUARES_SMOOTHED_COLOR";
                        break;
                }
            } else {

                switch (comboBoxCostFunct.getSelectedIndex()) {

                    case 0:
                        cost = AlgorithmCostFunctions.LEAST_SQUARES_SMOOTHED_WGT_COLOR;
                        costName = "LEAST_SQUARES_SMOOTHED_WGT_COLOR";
                        break;
                }
            }
        } // if (doColor)
        else { // black and white

            if ((!weighted) && (!voisOnly)) {

                switch (comboBoxCostFunct.getSelectedIndex()) {

                    case 0:
                        cost = AlgorithmCostFunctions.CORRELATION_RATIO_SMOOTHED;
                        costName = "CORRELATION_RATIO_SMOOTHED";
                        break;
                        // case 0:  cost = AlgorithmCostFunctions.CORRELATION_RATIO;                     break;

                    case 1:
                        cost = AlgorithmCostFunctions.LEAST_SQUARES_SMOOTHED;
                        costName = "LEAST_SQUARES_SMOOTHED";

                        // cost = AlgorithmCostFunctions.LEAST_SQUARES;
                        // costName = "LEAST_SQUARES_SMOOTHED";
                        break;
                        // case 2:  cost = AlgorithmCostFunctions.MUTUAL_INFORMATION_SMOOTHED;           break;

                    case 2:
                        cost = AlgorithmCostFunctions.NORMALIZED_XCORRELATION_SMOOTHED;
                        costName = "NORMALIZED_XCORRELATION_SMOOTHED";
                        break;
                        // case 3:  cost = AlgorithmCostFunctions.NORMALIZED_MUTUAL_INFORMATION;         break;

                    case 3:
                        cost = AlgorithmCostFunctions.NORMALIZED_MUTUAL_INFORMATION_SMOOTHED;
                        costName = "NORMALIZED_MUTUAL_INFORMATION_SMOOTHED";
                        break;

                    default:
                        cost = AlgorithmCostFunctions.CORRELATION_RATIO_SMOOTHED;
                        costName = "CORRELATION_RATIO_SMOOTHED";
                        break;
                }
            } else {

                switch (comboBoxCostFunct.getSelectedIndex()) {

                    case 0:
                        cost = AlgorithmCostFunctions.CORRELATION_RATIO_SMOOTHED_WGT;
                        costName = "CORRELATION_RATIO_SMOOTHED_WGT";
                        break;

                    case 1:
                        cost = AlgorithmCostFunctions.LEAST_SQUARES_SMOOTHED_WGT;
                        costName = "LEAST_SQUARES_SMOOTHED_WGT";
                        break;
                        // case 2:  cost = AlgorithmCostFunctions.MUTUAL_INFORMATION_SMOOTHED_WGT;           break;

                    case 2:
                        cost = AlgorithmCostFunctions.NORMALIZED_XCORRELATION_SMOOTHED_WGT;
                        costName = "NORMALIZED_XCORRELATION_SMOOTHED_WGT";
                        break;

                    case 3:
                        cost = AlgorithmCostFunctions.NORMALIZED_MUTUAL_INFORMATION_SMOOTHED_WGT;
                        costName = "NORMALIZED_MUTUAL_INFORMATION_SMOOTHED_WGT";
                        break;

                    default:
                        cost = AlgorithmCostFunctions.CORRELATION_RATIO_SMOOTHED_WGT;
                        costName = "CORRELATION_RATIO_SMOOTHED_WGT";
                        break;
                }
            }
        } // else black and white

        switch (comboBoxDOF.getSelectedIndex()) {

            case 0:
                DOF = 6;
                break;

            case 1:
                DOF = 7;
                break;

            case 2:
                DOF = 9;
                break;

            case 3:
                DOF = 12;
                break;

            default:
                DOF = 12;
                break;
        }

        switch (comboBoxInterp.getSelectedIndex()) {

            case 0:
                interp = AlgorithmTransform.TRILINEAR;
                break;

            case 1:
                interp = AlgorithmTransform.BSPLINE3;
                break;

            case 2:
                interp = AlgorithmTransform.BSPLINE4;
                break;

            case 3:
                interp = AlgorithmTransform.CUBIC_LAGRANGIAN;
                break;

            case 4:
                interp = AlgorithmTransform.QUINTIC_LAGRANGIAN;
                break;

            case 5:
                interp = AlgorithmTransform.HEPTIC_LAGRANGIAN;
                break;

            case 6:
                interp = AlgorithmTransform.WSINC;
                break;
                // case 7:  interp = AlgorithmTransform.NEAREST_NEIGHBOR;  break;

            default:
                interp = AlgorithmTransform.TRILINEAR;
                break;
        }


        switch (comboBoxInterp2.getSelectedIndex()) {

            case 0:
                interp2 = AlgorithmTransform.TRILINEAR;
                break;

            case 1:
                interp2 = AlgorithmTransform.BSPLINE3;
                break;

            case 2:
                interp2 = AlgorithmTransform.BSPLINE4;
                break;

            case 3:
                interp2 = AlgorithmTransform.CUBIC_LAGRANGIAN;
                break;

            case 4:
                interp2 = AlgorithmTransform.QUINTIC_LAGRANGIAN;
                break;

            case 5:
                interp2 = AlgorithmTransform.HEPTIC_LAGRANGIAN;
                break;

            case 6:
                interp2 = AlgorithmTransform.WSINC;
                break;

            case 7:
                interp2 = AlgorithmTransform.NEAREST_NEIGHBOR;
                break;

            default:
                interp2 = AlgorithmTransform.TRILINEAR;
                break;
        }

        displayTransform = transformCheckbox.isSelected();
        fastMode = fastModeCheckbox.isSelected();

        if (!testParameter(rotateBeginTextX.getText(), -360, 360)) {
            showX();
            rotateBeginTextX.requestFocus();
            rotateBeginTextX.selectAll();

            return false;
        } else {
            rotateBeginX = Float.valueOf(rotateBeginTextX.getText()).floatValue();
        }

        if (!testParameter(rotateEndTextX.getText(), -360, 360)) {
            showX();
            rotateEndTextX.requestFocus();
            rotateEndTextX.selectAll();

            return false;
        } else {
            rotateEndX = Float.valueOf(rotateEndTextX.getText()).floatValue();
        }

        if (!testParameter(coarseRateTextX.getText(), 0.01, 360)) {
            showX();
            coarseRateTextX.requestFocus();
            coarseRateTextX.selectAll();

            return false;
        } else {
            coarseRateX = Float.valueOf(coarseRateTextX.getText()).floatValue();
        }

        if (rotateBeginX > rotateEndX) {
            MipavUtil.displayError("Beginning of rangeX must be less than end of range.");
            showX();
            rotateBeginTextX.requestFocus();
            rotateBeginTextX.selectAll();

            return false;
        }

        if (((rotateEndX - rotateBeginX) / coarseRateX) < 1) {
            int response = JOptionPane.showConfirmDialog(this,
                                                         "Warning: with such a large rateX, there will only be 1 sampling.  Continue?",
                                                         "Sampling warning", JOptionPane.YES_NO_OPTION,
                                                         JOptionPane.WARNING_MESSAGE);

            if (response == JOptionPane.NO_OPTION) {
                showX();
                coarseRateTextX.requestFocus();
                coarseRateTextX.selectAll();

                return false;
            }
        }

        if (!testParameter(fineRateTextX.getText(), 0.01, 360)) {
            showX();
            fineRateTextX.requestFocus();
            fineRateTextX.selectAll();

            return false;
        } else {
            fineRateX = Float.valueOf(fineRateTextX.getText()).floatValue();
        }

        if (((rotateEndX - rotateBeginX) / fineRateX) < 1) {
            int response = JOptionPane.showConfirmDialog(this,
                                                         "Warning: with such a large rateX, there will only be 1 sampling.  Continue?",
                                                         "Sampling warning", JOptionPane.YES_NO_OPTION,
                                                         JOptionPane.WARNING_MESSAGE);

            if (response == JOptionPane.NO_OPTION) {
                showX();
                coarseRateTextX.requestFocus();
                coarseRateTextX.selectAll();

                return false;
            }
        }

        if (universalCheckbox.isSelected()) {
            rotateBeginY = rotateBeginX;
            rotateBeginZ = rotateBeginX;
            rotateEndY = rotateEndX;
            rotateEndZ = rotateEndX;
            coarseRateY = coarseRateX;
            coarseRateZ = coarseRateX;
            fineRateY = fineRateX;
            fineRateZ = fineRateX;
        } else { // universalCheckbox not selected

            if (!testParameter(rotateBeginTextY.getText(), -360, 360)) {
                showY();
                rotateBeginTextY.requestFocus();
                rotateBeginTextY.selectAll();

                return false;
            } else {
                rotateBeginY = Float.valueOf(rotateBeginTextY.getText()).floatValue();
            }

            if (!testParameter(rotateEndTextY.getText(), -360, 360)) {
                showY();
                rotateEndTextY.requestFocus();
                rotateEndTextY.selectAll();

                return false;
            } else {
                rotateEndY = Float.valueOf(rotateEndTextY.getText()).floatValue();
            }

            if (!testParameter(coarseRateTextY.getText(), 0.01, 360)) {
                showY();
                coarseRateTextY.requestFocus();
                coarseRateTextY.selectAll();

                return false;
            } else {
                coarseRateY = Float.valueOf(coarseRateTextY.getText()).floatValue();
            }

            if (rotateBeginY > rotateEndY) {
                MipavUtil.displayError("Beginning of rangeY must be less than end of range.");
                showY();
                rotateBeginTextY.requestFocus();
                rotateBeginTextY.selectAll();

                return false;
            }

            if (((rotateEndY - rotateBeginY) / coarseRateY) < 1) {
                int response = JOptionPane.showConfirmDialog(this,
                                                             "Warning: with such a large rateY, there will only be 1 sampling.  Continue?",
                                                             "Sampling warning", JOptionPane.YES_NO_OPTION,
                                                             JOptionPane.WARNING_MESSAGE);

                if (response == JOptionPane.NO_OPTION) {
                    showY();
                    coarseRateTextY.requestFocus();
                    coarseRateTextY.selectAll();

                    return false;
                }
            }

            if (!testParameter(fineRateTextY.getText(), 0.01, 360)) {
                showY();
                fineRateTextY.requestFocus();
                fineRateTextY.selectAll();

                return false;
            } else {
                fineRateY = Float.valueOf(fineRateTextY.getText()).floatValue();
            }

            if (((rotateEndY - rotateBeginY) / fineRateY) < 1) {
                int response = JOptionPane.showConfirmDialog(this,
                                                             "Warning: with such a large rateY, there will only be 1 sampling.  Continue?",
                                                             "Sampling warning", JOptionPane.YES_NO_OPTION,
                                                             JOptionPane.WARNING_MESSAGE);

                if (response == JOptionPane.NO_OPTION) {
                    showY();
                    coarseRateTextY.requestFocus();
                    coarseRateTextY.selectAll();

                    return false;
                }
            }

            if (!testParameter(rotateBeginTextZ.getText(), -360, 360)) {
                showZ();
                rotateBeginTextZ.requestFocus();
                rotateBeginTextZ.selectAll();

                return false;
            } else {
                rotateBeginZ = Float.valueOf(rotateBeginTextZ.getText()).floatValue();
            }

            if (!testParameter(rotateEndTextZ.getText(), -360, 360)) {
                showZ();
                rotateEndTextZ.requestFocus();
                rotateEndTextZ.selectAll();

                return false;
            } else {
                rotateEndZ = Float.valueOf(rotateEndTextZ.getText()).floatValue();
            }

            if (!testParameter(coarseRateTextZ.getText(), 0.01, 360)) {
                showZ();
                coarseRateTextZ.requestFocus();
                coarseRateTextZ.selectAll();

                return false;
            } else {
                coarseRateZ = Float.valueOf(coarseRateTextZ.getText()).floatValue();
            }

            if (rotateBeginZ > rotateEndZ) {
                MipavUtil.displayError("Beginning of rangeZ must be less than end of range.");
                showZ();
                rotateBeginTextZ.requestFocus();
                rotateBeginTextZ.selectAll();

                return false;
            }

            if (((rotateEndZ - rotateBeginZ) / coarseRateZ) < 1) {
                int response = JOptionPane.showConfirmDialog(this,
                                                             "Warning: with such a large rateZ, there will only be 1 sampling.  Continue?",
                                                             "Sampling warning", JOptionPane.YES_NO_OPTION,
                                                             JOptionPane.WARNING_MESSAGE);

                if (response == JOptionPane.NO_OPTION) {
                    showZ();
                    coarseRateTextZ.requestFocus();
                    coarseRateTextZ.selectAll();

                    return false;
                }
            }

            if (!testParameter(fineRateTextZ.getText(), 0.01, 360)) {
                showZ();
                fineRateTextZ.requestFocus();
                fineRateTextZ.selectAll();

                return false;
            } else {
                fineRateZ = Float.valueOf(fineRateTextZ.getText()).floatValue();
            }

            if (((rotateEndZ - rotateBeginZ) / fineRateZ) < 1) {
                int response = JOptionPane.showConfirmDialog(this,
                                                             "Warning: with such a large rateZ, there will only be 1 sampling.  Continue?",
                                                             "Sampling warning", JOptionPane.YES_NO_OPTION,
                                                             JOptionPane.WARNING_MESSAGE);

                if (response == JOptionPane.NO_OPTION) {
                    showZ();
                    coarseRateTextZ.requestFocus();
                    coarseRateTextZ.selectAll();

                    return false;
                }
            }
        } // else universalCheckbox not selected

        if (voisOnly) {

            // check that there actually are VOIs there
            // and propagate the VOIs to all slices
            ViewVOIVector VOIs = (ViewVOIVector) refImage.getVOIs();
            int nVOI = VOIs.size();

            if (nVOI < 1) {
                MipavUtil.displayError("There must be at least one VOI in " + refImage.getImageName() +
                                       " to register.");

                return false;
            }

            VOIs = (ViewVOIVector) matchImage.getVOIs();
            nVOI = VOIs.size();

            if (nVOI < 1) {
                MipavUtil.displayError("There must be at least one VOI in " + matchImage.getImageName() +
                                       " to register.");

                return false;
            }
        } // if (voisOnly)

        doSubsample = sampleCheckbox.isSelected();

        return true;
    }

    /**
     * DOCUMENT ME!
     */
    private void showX() {

        if (xSelected) {
            return;
        } else if (ySelected) {
            rotatePanel.remove(rotateRangePanelY);
            rotatePanel.remove(coarsePanelY);
            rotatePanel.remove(finePanelY);
            ySelected = false;
        } else { // if (zSelected)
            rotatePanel.remove(rotateRangePanelZ);
            rotatePanel.remove(coarsePanelZ);
            rotatePanel.remove(finePanelZ);
            zSelected = false;
        } // else if zSelected

        xSelected = true;
        gbc.gridx = 0;
        gbc.gridy = 2;
        gbc.gridwidth = 1;
        gbc.anchor = GridBagConstraints.WEST;
        rotatePanel.add(rotateRangePanelX, gbc);

        gbc.gridx = 0;
        gbc.gridy = 3;
        gbc.gridwidth = GridBagConstraints.REMAINDER;
        rotatePanel.add(coarsePanelX, gbc);

        gbc.gridx = 0;
        gbc.gridy = 4;
        gbc.gridwidth = GridBagConstraints.REMAINDER;
        rotatePanel.add(finePanelX, gbc);
        xRadio.setEnabled(false);
        yRadio.setEnabled(false);
        zRadio.setEnabled(false);
        xRadio.setSelected(true);
        yRadio.setSelected(false);
        zRadio.setSelected(false);
        xRadio.setEnabled(true);
        yRadio.setEnabled(true);
        zRadio.setEnabled(true);
    }

    /**
     * DOCUMENT ME!
     */
    private void showY() {

        if (xSelected) {
            rotatePanel.remove(rotateRangePanelX);
            rotatePanel.remove(coarsePanelX);
            rotatePanel.remove(finePanelX);
            xSelected = false;
        } // if (xSelected)
        else if (ySelected) {
            return;
        } else { // zSelected
            rotatePanel.remove(rotateRangePanelZ);
            rotatePanel.remove(coarsePanelZ);
            rotatePanel.remove(finePanelZ);
            zSelected = false;
        } // else zSelected

        ySelected = true;
        gbc.gridx = 0;
        gbc.gridy = 2;
        gbc.gridwidth = 1;
        gbc.anchor = GridBagConstraints.WEST;
        rotatePanel.add(rotateRangePanelY, gbc);

        gbc.gridx = 0;
        gbc.gridy = 3;
        gbc.gridwidth = GridBagConstraints.REMAINDER;
        rotatePanel.add(coarsePanelY, gbc);

        gbc.gridx = 0;
        gbc.gridy = 4;
        gbc.gridwidth = GridBagConstraints.REMAINDER;
        rotatePanel.add(finePanelY, gbc);
        xRadio.setEnabled(false);
        yRadio.setEnabled(false);
        zRadio.setEnabled(false);
        xRadio.setSelected(false);
        yRadio.setSelected(true);
        zRadio.setSelected(false);
        xRadio.setEnabled(true);
        yRadio.setEnabled(true);
        zRadio.setEnabled(true);
    }

    /**
     * DOCUMENT ME!
     */
    private void showZ() {

        if (xSelected) {
            rotatePanel.remove(rotateRangePanelX);
            rotatePanel.remove(coarsePanelX);
            rotatePanel.remove(finePanelX);
            xSelected = false;
        } // if (xSelcted)
        else if (ySelected) {
            rotatePanel.remove(rotateRangePanelY);
            rotatePanel.remove(coarsePanelY);
            rotatePanel.remove(finePanelY);
            ySelected = false;
        } // else if (ySelected)
        else { // zSelected
            return;
        } // else zSelected

        zSelected = true;
        gbc.gridx = 0;
        gbc.gridy = 2;
        gbc.gridwidth = 1;
        gbc.anchor = GridBagConstraints.WEST;
        rotatePanel.add(rotateRangePanelZ, gbc);

        gbc.gridx = 0;
        gbc.gridy = 3;
        gbc.gridwidth = GridBagConstraints.REMAINDER;
        rotatePanel.add(coarsePanelZ, gbc);

        gbc.gridx = 0;
        gbc.gridy = 4;
        gbc.gridwidth = GridBagConstraints.REMAINDER;
        rotatePanel.add(finePanelZ, gbc);
        xRadio.setEnabled(false);
        yRadio.setEnabled(false);
        zRadio.setEnabled(false);
        xRadio.setSelected(false);
        yRadio.setSelected(false);
        zRadio.setSelected(true);
        xRadio.setEnabled(true);
        yRadio.setEnabled(true);
        zRadio.setEnabled(true);
    }
}
