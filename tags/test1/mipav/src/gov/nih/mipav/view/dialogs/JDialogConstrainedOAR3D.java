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
 * @see     AlgorithmConstrainedOAR3D
 */
public class JDialogConstrainedOAR3D extends JDialogScriptableBase implements AlgorithmInterface {

    //~ Static fields/initializers -------------------------------------------------------------------------------------

    /** Use serialVersionUID for interoperability. */
    private static final long serialVersionUID = -5867077647353095805L;

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
    private JPanel coarsePanelX, coarsePanelY, coarsePanelZ;

    /** DOCUMENT ME! */
    private JComboBox comboBoxCostFunct;
    
    private JComboBox comboBoxSearchAlgo;

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
    private boolean doSubsample;

    /** DOCUMENT ME! */
    private boolean fastMode;

    /** DOCUMENT ME! */
    private JCheckBox fastModeCheckbox;

    /** DOCUMENT ME! */
    private String fileNameWRef, directoryWRef, fileNameWInput, directoryWInput;

    /** DOCUMENT ME! */
    private GridBagConstraints gbc;

    /** DOCUMENT ME! */
    private ModelImage inputWeightImage, refWeightImage;

    /** DOCUMENT ME! */
    private JLabel labelInterp2;

    /** DOCUMENT ME! */
    private JLabel labelTranslateRangeX = new JLabel("Limits on X translation (or all translations):");

    /** DOCUMENT ME! */
    private JLabel labelTranslateRangeY = new JLabel("Limits on Y translation:");

    /** DOCUMENT ME! */
    private JLabel labelTranslateRangeZ = new JLabel("Limits on Z translation :");

    /** DOCUMENT ME! */
    private boolean limitTranslation = true, oneTransLimit = true;

    /** DOCUMENT ME! */
    private JCheckBox limitTranslationCheckbox;

    /** DOCUMENT ME! */
    private ModelImage matchImage; // register match image to reference Image

    /** DOCUMENT ME! */
    private int maxIterations_def = 10, bracketBound_def = 10, numMinima_def = 5;

    /** DOCUMENT ME! */
    private int maxIterations = maxIterations_def, bracketBound = bracketBound_def;

    /** when done debugging: private boolean limitTranslation = false, oneTransLimit = false;. */
    private float[][] maxLimits;

    /** DOCUMENT ME! */
    private boolean maxOfMinResol;

    /** DOCUMENT ME! */
    private JTextField maxTransTextX, maxTransTextY, maxTransTextZ;

    /** DOCUMENT ME! */
    private JCheckBox minMaxCheckbox;

    /** DOCUMENT ME! */
    private JTextField minTransTextX, minTransTextY, minTransTextZ;

    /** DOCUMENT ME! */
    private JLabel mmLabelX = new JLabel("mm");

    /** DOCUMENT ME! */
    private JLabel mmLabelY = new JLabel("mm");

    /** DOCUMENT ME! */
    private JLabel mmLabelZ = new JLabel("mm");

    /** DOCUMENT ME! */
    private JRadioButton noneRadio;

    /** DOCUMENT ME! */
    private JTextField numberCoarseTextX, numberCoarseTextY, numberCoarseTextZ;

    /** DOCUMENT ME! */
    private int numCoarseX = 0, numCoarseY = 0, numCoarseZ = 0;

    /** DOCUMENT ME! */
    private int numMinima = numMinima_def;

    /** DOCUMENT ME! */
    private ModelImage refImage;

    /** DOCUMENT ME! */
    private AlgorithmConstrainedELSUNCOAR3D reg3E = null;
    
    private AlgorithmConstrainedOAR3D reg3 = null;

    /** DOCUMENT ME! */
    private ModelImage resultImage;

    /** DOCUMENT ME! */
    private JTextField rotateBeginTextX, rotateBeginTextY, rotateBeginTextZ;

    /** private JTextField fineRateTextX, fineRateTextY, fineRateTextZ;. */
    private float rotateBeginX, rotateEndX, rotateRangeX;

    /** DOCUMENT ME! */
    private float rotateBeginY, rotateEndY, rotateRangeY;

    /** DOCUMENT ME! */
    private float rotateBeginZ, rotateEndZ, rotateRangeZ;

    /** DOCUMENT ME! */
    private JTextField rotateEndTextX, rotateEndTextY, rotateEndTextZ;

    /** Variables for Rotation and Translation bounds. */
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
    private JLabel toLabelX = new JLabel("mm to");

    /** DOCUMENT ME! */
    private JLabel toLabelY = new JLabel("mm to");

    /** DOCUMENT ME! */
    private JLabel toLabelZ = new JLabel("mm to");

    /** DOCUMENT ME! */
    private JCheckBox transformCheckbox;

    /** DOCUMENT ME! */
    private JPanel translatePanel;

    /** private JPanel finePanelX, finePanelY, finePanelZ;. */
    private JPanel translateRangePanelX, translateRangePanelY, translateRangePanelZ;

    /** DOCUMENT ME! */
    private float[][] transLimits; // in mm, Algorithm will convert to pixels

    /** DOCUMENT ME! */
    private ViewUserInterface UI;

    /** DOCUMENT ME! */
    private JCheckBox universalRotationCheckbox;

    /** DOCUMENT ME! */
    private JCheckBox universalTranslationCheckbox;

    /** DOCUMENT ME! */
    private JRadioButton voiRadio;

    /** DOCUMENT ME! */
    private boolean voisOnly;

    /** DOCUMENT ME! */
    private boolean weighted;

    /** DOCUMENT ME! */
    private JRadioButton weightRadio;

    /** DOCUMENT ME! */
    private JRadioButton xRRadio, yRRadio, zRRadio;

    /** DOCUMENT ME! */
    private boolean xRSelected = true;

    /** DOCUMENT ME! */
    private boolean yRSelected = false;
    
    private JLabel outOfBoundsLabel;
    
    private JComboBox outOfBoundsComboBox;
    
    private JLabel valueLabel;
    
    private JTextField valueText;
    
    private double imageMin;
    
    private double imageMax;
    
    private int dataType;
    
    /**
     * Tells how to select fill value for out of bounds data
     * 0 for image minimum
     * 1 for NaN for float, zero otherwise.
     * 2 for user defined
     * 3 for image maximum
     */
    private int outOfBoundsIndex = 0;
    
    private float fillValue = 0.0f;
    
    private JLabel matrixLabel;
    
    private JComboBox matrixComboBox;
    
    private String matrixDirectory;
    
    private JLabel userDirectoryLabel;
    
    private JTextField userDirectoryText;
    
    private boolean useELSUNC = false;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Empty constructor needed for dynamic instantiation (used during scripting).
     */
    public JDialogConstrainedOAR3D() { }

    /**
     * Creates new dialog for user to choose type of linear image registration algorithm to run.
     *
     * @param  theParentFrame  Parent frame.
     * @param  im              Source image.
     */
    public JDialogConstrainedOAR3D(Frame theParentFrame, ModelImage im) {
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
        } else if (command.equals("AdvancedSettings")) {
            bracketBound_def = bracketBound;
            maxIterations_def = maxIterations;
            numMinima_def = numMinima;
            advancedDialog = buildAdvancedDialog(bracketBound, maxIterations, numMinima, limitTranslation,
                                                 oneTransLimit);
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
                MipavUtil.displayError("Out of memory in AlgorithmConstrainedESLUNCOAR3D.");

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
                MipavUtil.displayError("Out of memory in JDialogConstrainedOAR3D.");

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

            if (limitTranslation) {
                tmpStr = minTransTextX.getText();

                if (testParameter(tmpStr, -200, 200)) {
                    transLimits[0][0] = Float.valueOf(tmpStr).floatValue();
                }

                tmpStr = maxTransTextX.getText();

                if (testParameter(tmpStr, -200, 200)) {
                    transLimits[1][0] = Float.valueOf(tmpStr).floatValue();
                }

                if (universalTranslationCheckbox.isSelected()) {
                    transLimits[0][1] = transLimits[0][2] = transLimits[0][0];
                    transLimits[1][1] = transLimits[1][2] = transLimits[1][0];
                } else {
                    tmpStr = minTransTextY.getText();

                    if (testParameter(tmpStr, -200, 200)) {
                        transLimits[0][1] = Float.valueOf(tmpStr).floatValue();
                    }

                    tmpStr = maxTransTextY.getText();

                    if (testParameter(tmpStr, -200, 200)) {
                        transLimits[1][1] = Float.valueOf(tmpStr).floatValue();
                    }

                    tmpStr = minTransTextZ.getText();

                    if (testParameter(tmpStr, -200, 200)) {
                        transLimits[0][2] = Float.valueOf(tmpStr).floatValue();
                    }

                    tmpStr = maxTransTextZ.getText();

                    if (testParameter(tmpStr, -200, 200)) {
                        transLimits[1][2] = Float.valueOf(tmpStr).floatValue();
                    }
                }
            } else {

                for (int i = 0; i < 3; i++) {
                    transLimits[0][i] = maxLimits[0][i];
                    transLimits[1][i] = maxLimits[1][i];
                }
            }

            advancedDialog.setVisible(false);
            advancedDialog.dispose();
        } else if (command.equals("AdvancedCancel")) {
            maxIterations = maxIterations_def;
            bracketBound = bracketBound_def;
            numMinima = numMinima_def;
            advancedDialog.setVisible(false);
            advancedDialog.dispose();
        } else {
            super.actionPerformed(event);
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
        boolean pad = false;
        int xdimA = refImage.getExtents()[0];
        int ydimA = refImage.getExtents()[1];
        int zdimA = refImage.getExtents()[2];
        float xresA = refImage.getFileInfo(0).getResolutions()[0];
        float yresA = refImage.getFileInfo(0).getResolutions()[1];
        float zresA = refImage.getFileInfo(0).getResolutions()[2];

        if (algorithm instanceof AlgorithmConstrainedOAR3D) {

            if (reg3.isCompleted()) {
              
                if (displayTransform) {
                    String name = makeImageName(matchImage.getImageName(), "_register");
                    transform = new AlgorithmTransform(matchImage, reg3.getTransform(), interp2, xresA, yresA, zresA,
                                                       xdimA, ydimA, zdimA, true, false, pad);

                    transform.setUpdateOriginFlag(true);
                    transform.setFillValue(fillValue);

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
                    
                    resultImage.getMatrixHolder().replaceMatrices(refImage.getMatrixHolder().getMatrices());
                    
                    float scale = (refImage.getExtents()[2] - 1)/(resultImage.getExtents()[2] - 1);
                    for (int i = 0; i < resultImage.getExtents()[2]; i++) {
                        float interp = i * scale;
                        int lowerLimit = (int)interp;
                        int upperLimit;
                        float origin[] = new float[3];;
                        if (lowerLimit < refImage.getExtents()[2] - 1) {
                             upperLimit = lowerLimit + 1; 
                             for (int j = 0; j < 3; j++) {
                             origin[j] = (upperLimit - interp) * refImage.getFileInfo(lowerLimit).getOrigin()[j] +
                                      (interp - lowerLimit) * refImage.getFileInfo(upperLimit).getOrigin()[j];
                             }
                        }
                        else {
                            for (int j = 0; j < 3; j++) {
                                origin[j] = refImage.getFileInfo(lowerLimit).getOrigin()[j];
                                }    
                        }
                        
                        resultImage.getFileInfo(i).setOrigin(origin);
                    }
                }

                //BEN: Changed... add the matrix to the holder (and set as another dataset type)
                TransMatrix resultMatrix = reg3.getTransform();
                resultMatrix.setTransformID(TransMatrix.TRANSFORM_ANOTHER_DATASET);
                matchImage.getMatrixHolder().addMatrix(resultMatrix);


                String message = "Using cost function, " + costName;
                message += ", the cost is " + Double.toString(reg3.getAnswer()) + ".\n";
                message += "Some registration settings: \n";
                message += "X Rotations from " + rotateBeginX + " to " + rotateEndX + ", ";
                message += "with number or X angles in coarse sampling " + numCoarseX + ".\n";
                message += "Y Rotations from " + rotateBeginY + " to " + rotateEndY + ", ";
                message += "with number or Y angles in coarse sampling " + numCoarseY + ".\n";
                message += "Z Rotations from " + rotateBeginZ + " to " + rotateEndZ + ", ";
                message += "with number or Z angles in coarse sampling " + numCoarseZ + ".\n";
                reg3.getTransform().saveMatrix(matrixDirectory + File.separator + matchImage.getImageName() + "_To_" +
                                               refImage.getImageName() + ".mtx", interp2, xresA, yresA, zresA,
                                               xdimA, ydimA, zdimA, true, false, pad, message);
                Preferences.debug("Saved " + matrixDirectory + File.separator + matchImage.getImageName() + "_To_" +
                                               refImage.getImageName() + ".mtx\n",Preferences.DEBUG_FILEIO);

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
        
        if (algorithm instanceof AlgorithmConstrainedELSUNCOAR3D) {

            if (reg3E.isCompleted()) {

                if (displayTransform) {
                    

                    String name = makeImageName(matchImage.getImageName(), "_register");
                    transform = new AlgorithmTransform(matchImage, reg3E.getTransform(), interp2, xresA, yresA, zresA,
                                                       xdimA, ydimA, zdimA, true, false, pad);

                    transform.setUpdateOriginFlag(true);
                    transform.setFillValue(fillValue);

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
                    
                    resultImage.getMatrixHolder().replaceMatrices(refImage.getMatrixHolder().getMatrices());
                    
                    float scale = (refImage.getExtents()[2] - 1)/(resultImage.getExtents()[2] - 1);
                    for (int i = 0; i < resultImage.getExtents()[2]; i++) {
                        float interp = i * scale;
                        int lowerLimit = (int)interp;
                        int upperLimit;
                        float origin[] = new float[3];;
                        if (lowerLimit < refImage.getExtents()[2] - 1) {
                             upperLimit = lowerLimit + 1; 
                             for (int j = 0; j < 3; j++) {
                             origin[j] = (upperLimit - interp) * refImage.getFileInfo(lowerLimit).getOrigin()[j] +
                                      (interp - lowerLimit) * refImage.getFileInfo(upperLimit).getOrigin()[j];
                             }
                        }
                        else {
                            for (int j = 0; j < 3; j++) {
                                origin[j] = refImage.getFileInfo(lowerLimit).getOrigin()[j];
                                }    
                        }
                        
                        resultImage.getFileInfo(i).setOrigin(origin);
                    }
                }

                //BEN: Changed... add the matrix to the holder (and set as another dataset type)
                TransMatrix resultMatrix = reg3E.getTransform();
                resultMatrix.setTransformID(TransMatrix.TRANSFORM_ANOTHER_DATASET);
                matchImage.getMatrixHolder().addMatrix(resultMatrix);


                String message = "Using cost function, " + costName;
                message += ", the cost is " + Double.toString(reg3E.getAnswer()) + ".\n";
                message += "Some registration settings: \n";
                message += "X Rotations from " + rotateBeginX + " to " + rotateEndX + ", ";
                message += "with number or X angles in coarse sampling " + numCoarseX + ".\n";
                message += "Y Rotations from " + rotateBeginY + " to " + rotateEndY + ", ";
                message += "with number or Y angles in coarse sampling " + numCoarseY + ".\n";
                message += "Z Rotations from " + rotateBeginZ + " to " + rotateEndZ + ", ";
                message += "with number or Z angles in coarse sampling " + numCoarseZ + ".\n";
                reg3E.getTransform().saveMatrix(matrixDirectory + File.separator + matchImage.getImageName() + "_To_" +
                                               refImage.getImageName() + ".mtx", interp2, xresA, yresA, zresA,
                                               xdimA, ydimA, zdimA, true, false, pad, message);
                Preferences.debug("Saved " + matrixDirectory + File.separator + matchImage.getImageName() + "_To_" +
                                               refImage.getImageName() + ".mtx\n",Preferences.DEBUG_FILEIO);

                insertScriptLine();
            }

            if (reg3E != null) {
                reg3E.disposeLocal();
                reg3E = null;
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
            numberCoarseTextX.setEnabled(!fastModeCheckbox.isSelected());

            // fineRateTextX.setEnabled(!fastModeCheckbox.isSelected());
            rotateBeginTextY.setEnabled(!fastModeCheckbox.isSelected());
            rotateEndTextY.setEnabled(!fastModeCheckbox.isSelected());
            numberCoarseTextY.setEnabled(!fastModeCheckbox.isSelected());

            // fineRateTextY.setEnabled(!fastModeCheckbox.isSelected());
            rotateBeginTextZ.setEnabled(!fastModeCheckbox.isSelected());
            rotateEndTextZ.setEnabled(!fastModeCheckbox.isSelected());
            numberCoarseTextZ.setEnabled(!fastModeCheckbox.isSelected());
            // fineRateTextZ.setEnabled(!fastModeCheckbox.isSelected());
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
        else if (event.getSource() == universalRotationCheckbox) {

            if (universalRotationCheckbox.isSelected()) {
                xRRadio.setEnabled(false);
                yRRadio.setEnabled(false);
                zRRadio.setEnabled(false);
                xRRadio.setSelected(true);
                yRRadio.setSelected(false);
                zRRadio.setSelected(false);

                if (xRSelected) {
                    return;
                } else if (yRSelected) {
                    rotatePanel.remove(rotateRangePanelY);
                    rotatePanel.remove(coarsePanelY);

                    // rotatePanel.remove(finePanelY);
                    yRSelected = false;
                } else { // if (zSelected)
                    rotatePanel.remove(rotateRangePanelZ);
                    rotatePanel.remove(coarsePanelZ);

                    // rotatePanel.remove(finePanelZ);
                } // else if zRSelected

                xRSelected = true;
                gbc.gridx = 0;
                gbc.gridy = 2;
                gbc.gridwidth = 1;
                gbc.anchor = GridBagConstraints.WEST;
                rotatePanel.add(rotateRangePanelX, gbc);

                gbc.gridx = 0;
                gbc.gridy = 3;
                gbc.gridwidth = GridBagConstraints.REMAINDER;
                rotatePanel.add(coarsePanelX, gbc);

                // gbc.gridx = 0;
                // gbc.gridy = 4;
                // gbc.gridwidth = GridBagConstraints.REMAINDER;
                // rotatePanel.add(finePanelX, gbc);
            } else {
                xRRadio.setEnabled(true);
                yRRadio.setEnabled(true);
                zRRadio.setEnabled(true);
            }
        } // else if (event.getSource() == universalRotationCheckbox)
        else if ((event.getSource() == xRRadio) || (event.getSource() == yRRadio) || (event.getSource() == zRRadio)) {

            if (xRRadio.isSelected()) {

                if (xRSelected) {
                    return;
                } else if (yRSelected) {
                    rotatePanel.remove(rotateRangePanelY);
                    rotatePanel.remove(coarsePanelY);

                    // rotatePanel.remove(finePanelY);
                    yRSelected = false;
                } else { // if (zRSelected)
                    rotatePanel.remove(rotateRangePanelZ);
                    rotatePanel.remove(coarsePanelZ);

                    // rotatePanel.remove(finePanelZ);
                } // else if zRSelected

                xRSelected = true;
                gbc.gridx = 0;
                gbc.gridy = 2;
                gbc.gridwidth = 1;
                gbc.anchor = GridBagConstraints.WEST;
                rotatePanel.add(rotateRangePanelX, gbc);

                gbc.gridx = 0;
                gbc.gridy = 3;
                gbc.gridwidth = GridBagConstraints.REMAINDER;
                rotatePanel.add(coarsePanelX, gbc);

                // gbc.gridx = 0;
                // gbc.gridy = 4;
                // gbc.gridwidth = GridBagConstraints.REMAINDER;
                // rotatePanel.add(finePanelX, gbc);
            } // if (xRRadio.isSelected)
            else if (yRRadio.isSelected()) {

                if (xRSelected) {
                    rotatePanel.remove(rotateRangePanelX);
                    rotatePanel.remove(coarsePanelX);

                    // rotatePanel.remove(finePanelX);
                    xRSelected = false;
                } // if (xRSelected)
                else if (yRSelected) {
                    return;
                } else { // zRSelected
                    rotatePanel.remove(rotateRangePanelZ);
                    rotatePanel.remove(coarsePanelZ);

                    // rotatePanel.remove(finePanelZ);
                } // else zRSelected

                yRSelected = true;
                gbc.gridx = 0;
                gbc.gridy = 2;
                gbc.gridwidth = 1;
                gbc.anchor = GridBagConstraints.WEST;
                rotatePanel.add(rotateRangePanelY, gbc);

                gbc.gridx = 0;
                gbc.gridy = 3;
                gbc.gridwidth = GridBagConstraints.REMAINDER;
                rotatePanel.add(coarsePanelY, gbc);

                // gbc.gridx = 0;
                // gbc.gridy = 4;
                // gbc.gridwidth = GridBagConstraints.REMAINDER;
                // rotatePanel.add(finePanelY, gbc);
            } // else if (yRadio.isSelected())
            else if (zRRadio.isSelected()) {

                if (xRSelected) {
                    rotatePanel.remove(rotateRangePanelX);
                    rotatePanel.remove(coarsePanelX);

                    // rotatePanel.remove(finePanelX);
                    xRSelected = false;
                } // if (xSelected)
                else if (yRSelected) {
                    rotatePanel.remove(rotateRangePanelY);
                    rotatePanel.remove(coarsePanelY);

                    // rotatePanel.remove(finePanelY);
                    yRSelected = false;
                } // else if (yRSelected)
                else { // zRSelected
                    return;
                } // else zRSelected

                gbc.gridx = 0;
                gbc.gridy = 2;
                gbc.gridwidth = 1;
                gbc.anchor = GridBagConstraints.WEST;
                rotatePanel.add(rotateRangePanelZ, gbc);

                gbc.gridx = 0;
                gbc.gridy = 3;
                gbc.gridwidth = GridBagConstraints.REMAINDER;
                rotatePanel.add(coarsePanelZ, gbc);

                // gbc.gridx = 0;
                // gbc.gridy = 4;
                // gbc.gridwidth = GridBagConstraints.REMAINDER;
                // rotatePanel.add(finePanelZ, gbc);
            } // else if (zRadio.isSelected())

            rotatePanel.validate();
            repaint();
        } // else if xRadio, yRadio, or zRadio
        else if (event.getSource() == limitTranslationCheckbox) {

            if (limitTranslationCheckbox.isSelected()) {
                limitTranslation = true;
                universalTranslationCheckbox.setEnabled(true);
                enableTranslationX(true);

                if (universalTranslationCheckbox.isSelected()) {
                    enableTranslationsYZ(false);
                } else {
                    enableTranslationsYZ(true);
                }
            } else {
                limitTranslation = false;
                universalTranslationCheckbox.setEnabled(false);
                enableTranslationX(false);
                enableTranslationsYZ(false);
            }
        } else if (event.getSource() == universalTranslationCheckbox) {

            if (universalTranslationCheckbox.isSelected()) {
                oneTransLimit = true;
                enableTranslationsYZ(false);
            } else {
                oneTransLimit = false;
                enableTranslationsYZ(true);
            }
        } else if (event.getSource() == outOfBoundsComboBox) {
            switch (outOfBoundsComboBox.getSelectedIndex()) {
                case 0: // image minimum
                    valueText.setText(String.valueOf(imageMin));
                    valueText.setEnabled(false);
                    break;
                case 1: // If float NaN, else 0
                    if ((dataType == ModelStorageBase.FLOAT) || (dataType == ModelStorageBase.DOUBLE) ||
                        (dataType == ModelStorageBase.ARGB_FLOAT)) {
                        valueText.setText(String.valueOf(Float.NaN)); 
                    }
                    else {
                        valueText.setText(String.valueOf(0));
                    }
                    valueText.setEnabled(false);
                    break;
                case 2: // User defined;
                    valueText.setEnabled(true);
                    break;
                case 3: // Image maximum
                    valueText.setText(String.valueOf(imageMax));
                    valueText.setEnabled(false);
                    break;
            } // switch (outOfBoundsComboBox.getSelectedIndex())
        } // else if (event.getSource() == outOfBoundsComboBox)
        else if (event.getSource() == comboBoxImage) {
        	refImage = UI.getRegisteredImageByName((String) comboBoxImage.getSelectedItem());
        	
        	matrixComboBox.removeAllItems();
            
            if (refImage != null) {
                matrixComboBox.addItem(refImage.getImageDirectory());	
            }
            
            if ((matchImage.getImageDirectory() != null) && 
                	(!refImage.getImageDirectory().equals(matchImage.getImageDirectory()))){
                	matrixComboBox.addItem(matchImage.getImageDirectory());
            }
            
            if ((UI.getDefaultDirectory() != null) && 
            	(!UI.getDefaultDirectory().equals(refImage.getImageDirectory())) &&
            	(!UI.getDefaultDirectory().equals(matchImage.getImageDirectory()))) {
            	matrixComboBox.addItem(UI.getDefaultDirectory());
            }
            matrixComboBox.addItem("User specified matrix directory");
            matrixComboBox.setSelectedIndex(0);
        }
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
     * Accessor to set the maximum X translation.
     *
     * @param  maxX  maximum x
     */
    public void setMaxTx(float maxX) {
        transLimits[1][0] = maxX;
    }

    /**
     * Accessor to set the maximum Y translation.
     *
     * @param  maxY  maximum y
     */
    public void setMaxTy(float maxY) {
        transLimits[1][1] = maxY;
    }

    /**
     * Accessor to set the maximum Z translation.
     *
     * @param  maxZ  maximum z
     */
    public void setMaxTz(float maxZ) {
        transLimits[1][2] = maxZ;
    }

    /**
     * Accessor to set the minimum X translation.
     *
     * @param  minX  minimum x
     */
    public void setMinTx(float minX) {
        transLimits[0][0] = minX;
    }

    /**
     * Accessor to set the minimum Y translation.
     *
     * @param  minY  minimum y
     */
    public void setMinTy(float minY) {
        transLimits[0][1] = minY;
    }

    /**
     * Accessor to set the minimum Z translation.
     *
     * @param  minZ  minimum z
     */
    public void setMinTz(float minZ) {
        transLimits[0][2] = minZ;
    }

    /**
     * Accessor to set the coarse sample rateX.
     *
     * @param  x  Coarse rateX
     */
    public void setNumCoarseX(int x) {
        numCoarseX = x;
    }

    /**
     * Accessor to set the coarse sample rateY.
     *
     * @param  y  Coarse rateY
     */
    public void setNumCoarseY(int y) {
        numCoarseY = y;
    }

    /**
     * Accessor to set the coarse sample rateZ.
     *
     * @param  z  Coarse rateZ
     */
    public void setNumCoarseZ(int z) {
        numCoarseZ = z;
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
     * Accessor to set whether to use Powell's algorithm calling Brent's method or ELSUNC for search algorithm
     * @param useELSUNC
     */
    public void setUseELSUNC(boolean useELSUNC) {
    	this.useELSUNC = useELSUNC;
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
     * tells how to select fill value for out of bounds data
     * 0 for image minimum
     * 1 for NaN for float, zero otherwise.
     * 2 for user defined
     * 3 for image max 
     * @param outOfBoundsIndex
     */
    public void setOutOfBoundsIndex(int outOfBoundsIndex) {
        this.outOfBoundsIndex = outOfBoundsIndex;
    }
    
    /**
     * Accessor to set intensity value for out of bounds data
     * @param fillValue
     */
    public void setFillValue(float fillValue) {
        this.fillValue = fillValue;
    }
    
    /**
     * Accessor to set directory in which the matrix file is stored
     * @param matrixDirectory
     */
    public void setMatrixDirectory(String matrixDirectory) {
    	this.matrixDirectory = matrixDirectory;
    }

    /**
     * Calls the algorithm with the set-up parameters.
     */
    protected void callAlgorithm() {
        //System.out.println("Reference image name is " + refImage.getImageName());
        //System.out.println("Moving image name is " + matchImage.getImageName());

        if (voisOnly) {
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

        if (useELSUNC) {
	        if (weighted) {
	            reg3E = new AlgorithmConstrainedELSUNCOAR3D(refImage, matchImage, refWeightImage, inputWeightImage, cost, DOF,
	                                                 interp, rotateBeginX, rotateRangeX, rotateBeginY, rotateRangeY,
	                                                 rotateBeginZ, rotateRangeZ, numCoarseX, numCoarseY, numCoarseZ,
	                                                 transLimits, maxOfMinResol, doSubsample, fastMode, calcCOG,
	                                                 bracketBound, maxIterations, numMinima);
	        } else {
	           // System.out.println("Reference image name is " + refImage.getImageName());
	            //System.out.println("Moving image name is " + matchImage.getImageName());
	
	            reg3E = new AlgorithmConstrainedELSUNCOAR3D(refImage, matchImage, cost, DOF, interp, rotateBeginX, rotateRangeX,
	                                                 rotateBeginY, rotateRangeY, rotateBeginZ, rotateRangeZ, numCoarseX,
	                                                 numCoarseY, numCoarseZ, transLimits, maxOfMinResol, doSubsample,
	                                                 fastMode, calcCOG, bracketBound, maxIterations, numMinima);
	        }
	        reg3E.addListener(this);

	        createProgressBar(matchImage.getImageName(), reg3E);

	        // Hide dialog
	        setVisible(false);

	        if (isRunInSeparateThread()) {

	            // Start the thread as a low priority because we wish to still have user interface work fast.
	            if (reg3E.startMethod(Thread.MIN_PRIORITY) == false) {
	                MipavUtil.displayError("A thread is already running on this object");
	            }
	        } else {

	            reg3E.run();
	        }
        }
        else {
        	if (weighted) {
	            reg3 = new AlgorithmConstrainedOAR3D(refImage, matchImage, refWeightImage, inputWeightImage, cost, DOF,
	                                                 interp, rotateBeginX, rotateRangeX, rotateBeginY, rotateRangeY,
	                                                 rotateBeginZ, rotateRangeZ, numCoarseX, numCoarseY, numCoarseZ,
	                                                 transLimits, maxOfMinResol, doSubsample, fastMode, calcCOG,
	                                                 bracketBound, maxIterations, numMinima);
	        } else {
	           // System.out.println("Reference image name is " + refImage.getImageName());
	            //System.out.println("Moving image name is " + matchImage.getImageName());
	
	            reg3 = new AlgorithmConstrainedOAR3D(refImage, matchImage, cost, DOF, interp, rotateBeginX, rotateRangeX,
	                                                 rotateBeginY, rotateRangeY, rotateBeginZ, rotateRangeZ, numCoarseX,
	                                                 numCoarseY, numCoarseZ, transLimits, maxOfMinResol, doSubsample,
	                                                 fastMode, calcCOG, bracketBound, maxIterations, numMinima);
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
        matchImage.calcMinMax();
        imageMin = matchImage.getMin();
        imageMax = matchImage.getMax();
        dataType = matchImage.getFileInfo()[0].getDataType();
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
        setUseELSUNC(scriptParameters.getParams().getBoolean("use_elsunc"));

        float[] rotBegin = scriptParameters.getParams().getList("rotate_begin").getAsFloatArray();
        float[] rotEnd = scriptParameters.getParams().getList("rotate_end").getAsFloatArray();
        int[] coarseRates = scriptParameters.getParams().getList("coarse_rate").getAsIntArray();

        setCoarseBeginX(rotBegin[0]);
        setCoarseEndX(rotEnd[0]);
        setNumCoarseX(coarseRates[0]);

        setCoarseBeginY(rotBegin[1]);
        setCoarseEndY(rotEnd[1]);
        setNumCoarseY(coarseRates[1]);

        setCoarseBeginZ(rotBegin[2]);
        setCoarseEndZ(rotEnd[2]);
        setNumCoarseZ(coarseRates[2]);

        setDisplayTransform(scriptParameters.getParams().getBoolean("do_display_transform"));
        setInterp2(scriptParameters.getParams().getInt("final_interpolation_type"));

        setMaxOfMinResol(scriptParameters.getParams().getBoolean("do_use_max_of_min_resolutions"));
        setSubsample(scriptParameters.getParams().getBoolean("do_subsample"));
        setFastMode(scriptParameters.getParams().getBoolean("do_use_fast_mode"));
        setCalcCOG(scriptParameters.getParams().getBoolean("do_calc_COG"));
        setOutOfBoundsIndex(scriptParameters.getParams().getInt("out_of_bounds_index"));
        switch(outOfBoundsIndex) {
            case 0: 
                setFillValue((float)imageMin);
                break;
            case 1: 
                if ((dataType == ModelStorageBase.FLOAT) || (dataType == ModelStorageBase.DOUBLE) ||
                        (dataType == ModelStorageBase.ARGB_FLOAT)) {
                    setFillValue(Float.NaN);
                }
                else {
                    setFillValue(0.0f);
                }
                break;
            case 2:
                setFillValue(scriptParameters.getParams().getFloat("fill_value"));
                break;
            case 3:
                setFillValue((float)imageMax);
                break;
        }
        setMatrixDirectory(scriptParameters.getParams().getString("matrix_directory"));

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

        if (getResultImage() != null) {
            scriptParameters.storeImageInRecorder(getResultImage());
        }

        scriptParameters.getParams().put(ParameterFactory.newParameter("degrees_of_freedom", DOF));
        scriptParameters.getParams().put(ParameterFactory.newParameter("initial_interpolation_type", interp));
        scriptParameters.getParams().put(ParameterFactory.newParameter("final_interpolation_type", interp2));
        scriptParameters.getParams().put(ParameterFactory.newParameter("cost_function_type", cost));
        scriptParameters.getParams().put(ParameterFactory.newParameter("use_elsunc", useELSUNC));
        scriptParameters.getParams().put(ParameterFactory.newParameter("rotate_begin",
                                                                       new float[] {
                                                                           rotateBeginX, rotateBeginY, rotateBeginZ
                                                                       }));
        scriptParameters.getParams().put(ParameterFactory.newParameter("rotate_end",
                                                                       new float[] {
                                                                           rotateEndX, rotateEndY, rotateEndZ
                                                                       }));
        scriptParameters.getParams().put(ParameterFactory.newParameter("coarse_rate",
                                                                       new int[] { numCoarseX, numCoarseY, numCoarseZ }));
        scriptParameters.getParams().put(ParameterFactory.newParameter("do_display_transform", displayTransform));
        scriptParameters.getParams().put(ParameterFactory.newParameter("do_use_max_of_min_resolutions", maxOfMinResol));
        scriptParameters.getParams().put(ParameterFactory.newParameter("do_subsample", doSubsample));
        scriptParameters.getParams().put(ParameterFactory.newParameter("do_use_fast_mode", fastMode));
        scriptParameters.getParams().put(ParameterFactory.newParameter("do_calc_COG", calcCOG));
        scriptParameters.getParams().put(ParameterFactory.newParameter("out_of_bounds_index", outOfBoundsIndex));
        scriptParameters.getParams().put(ParameterFactory.newParameter("fill_value", fillValue));
        scriptParameters.getParams().put(ParameterFactory.newParameter("matrix_directory", matrixDirectory));
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
     * @param   limitTrans    DOCUMENT ME!
     * @param   oneTrans      DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    private JDialog buildAdvancedDialog(int bracketBound, int maxIter, int numMinima, boolean limitTrans,
                                        boolean oneTrans) {
        serif12 = MipavUtil.font12;
        serif12B = MipavUtil.font12B;

        advancedDialog = new JDialog(this, "Advanced OAR settings", true);
        // Parent is the JDialogConstrainedOAR3D

        // Setting panel
        JPanel settingsPanel = new JPanel();
        settingsPanel.setBorder(BorderFactory.createTitledBorder("Optimization settings"));
        settingsPanel.setLayout(new GridBagLayout());
        // settingsPanel.setAlignmentX(Component.LEFT_ALIGNMENT);
       
        JPanel bracketPanel = new JPanel();
        bracketPanel.setLayout(new BorderLayout(1, 3));

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

        JLabel maxIterationsLabel = new JLabel("Number of iterations: ", JLabel.LEFT);
        maxIterPanel.add(maxIterationsLabel, BorderLayout.WEST);
        maxIterPanel.setToolTipText("Used for levelOne. Other levels are multiples of this #.");
        maxIterationsText = new JTextField(String.valueOf(maxIter), 5);
        maxIterationsText.addFocusListener(this);
        maxIterPanel.add(maxIterationsText, BorderLayout.CENTER);

        JLabel maxIterInstruct = new JLabel("Recommended value 5-20.", JLabel.RIGHT);
        maxIterPanel.add(maxIterInstruct, BorderLayout.SOUTH);

        JPanel numMinPanel = new JPanel();
        numMinPanel.setLayout(new BorderLayout(1, 3));

        JLabel numMinLabel = new JLabel("Number of minima from Level 8 to test at Level 4: ", JLabel.LEFT);
        numMinPanel.add(numMinLabel, BorderLayout.WEST);
        numMinPanel.setToolTipText("Increasing will significantly increase processing time.");
        numMinText = new JTextField(String.valueOf(numMinima), 5);
        numMinText.addFocusListener(this);
        numMinPanel.add(numMinText, BorderLayout.CENTER);

        // Translation Search Range
        translatePanel = new JPanel();
        translatePanel.setLayout(new BoxLayout(translatePanel, BoxLayout.Y_AXIS));
        translatePanel.setBorder(buildTitledBorder("Translation Search Range"));
        translatePanel.setAlignmentX(Component.LEFT_ALIGNMENT);

        // Limit translations?
        limitTranslationCheckbox = new JCheckBox("Limit translation range.");
        limitTranslationCheckbox.setSelected(limitTrans);
        limitTranslationCheckbox.setFont(serif12);
        limitTranslationCheckbox.addItemListener(this);
        limitTranslationCheckbox.setAlignmentX(Component.LEFT_ALIGNMENT);
        translatePanel.add(limitTranslationCheckbox);

        // Universal translation?
        universalTranslationCheckbox = new JCheckBox("Apply same translations limits to all dimensions.");
        universalTranslationCheckbox.setEnabled(limitTrans);
        universalTranslationCheckbox.setSelected(oneTrans);
        universalTranslationCheckbox.setFont(serif12);
        universalTranslationCheckbox.addItemListener(this);
        translatePanel.add(universalTranslationCheckbox);

        // X Translation Range Panel
        translateRangePanelX = new JPanel(new FlowLayout(FlowLayout.LEFT));
        translateRangePanelX.setAlignmentX(Component.LEFT_ALIGNMENT);

        labelTranslateRangeX.setFont(serif12);
        toLabelX.setFont(serif12);
        mmLabelX.setFont(serif12);
        minTransTextX = new JTextField(String.valueOf(transLimits[0][0]), 4);
        maxTransTextX = new JTextField(String.valueOf(transLimits[1][0]), 4);

        translateRangePanelX.add(labelTranslateRangeX);
        translateRangePanelX.add(minTransTextX);
        translateRangePanelX.add(toLabelX);
        translateRangePanelX.add(maxTransTextX);
        translateRangePanelX.add(mmLabelX);

        // Y Translation Range Panel
        translateRangePanelY = new JPanel(new FlowLayout(FlowLayout.LEFT));
        translateRangePanelY.setAlignmentX(Component.LEFT_ALIGNMENT);

        labelTranslateRangeY.setFont(serif12);
        toLabelY.setFont(serif12);
        mmLabelY.setFont(serif12);
        minTransTextY = new JTextField(String.valueOf(transLimits[0][1]), 4);
        maxTransTextY = new JTextField(String.valueOf(transLimits[1][1]), 4);

        translateRangePanelY.add(labelTranslateRangeY);
        translateRangePanelY.add(minTransTextY);
        translateRangePanelY.add(toLabelY);
        translateRangePanelY.add(maxTransTextY);
        translateRangePanelY.add(mmLabelY);

        // Z Translation Range Panel
        translateRangePanelZ = new JPanel(new FlowLayout(FlowLayout.LEFT));
        translateRangePanelZ.setAlignmentX(Component.LEFT_ALIGNMENT);

        labelTranslateRangeZ.setFont(serif12);
        toLabelZ.setFont(serif12);
        mmLabelZ.setFont(serif12);
        minTransTextZ = new JTextField(String.valueOf(transLimits[0][2]), 4);
        maxTransTextZ = new JTextField(String.valueOf(transLimits[1][2]), 4);

        translateRangePanelZ.add(labelTranslateRangeZ);
        translateRangePanelZ.add(minTransTextZ);
        translateRangePanelZ.add(toLabelZ);
        translateRangePanelZ.add(maxTransTextZ);
        translateRangePanelZ.add(mmLabelZ);

        translatePanel.add(translateRangePanelX);
        translatePanel.add(translateRangePanelY);
        translatePanel.add(translateRangePanelZ);

        GridBagConstraints gbc = new GridBagConstraints();
        gbc.gridy = 0;
        gbc.gridx = 0;
        gbc.anchor = GridBagConstraints.WEST;
        settingsPanel.add(bracketPanel, gbc);
        //settingsPanel.add(Box.createVerticalStrut(20));
        gbc.gridy = 1;
        settingsPanel.add(maxIterPanel, gbc);
        //settingsPanel.add(Box.createVerticalStrut(20));
        gbc.gridy = 2;
        settingsPanel.add(numMinPanel, gbc);
        //settingsPanel.add(Box.createVerticalStrut(20));
        gbc.gridy = 3;
        settingsPanel.add(translatePanel, gbc);
        //settingsPanel.add(Box.createVerticalStrut(15));
        gbc.gridy = 4;
        settingsPanel.add(sampleCheckbox, gbc);
        //settingsPanel.add(Box.createVerticalStrut(10));
        gbc.gridy = 5;
        settingsPanel.add(fastModeCheckbox, gbc);
        //settingsPanel.add(Box.createVerticalStrut(10));
        gbc.gridy = 6;
        settingsPanel.add(calcCOGCheckbox, gbc);

        enableTranslationX(limitTrans);
        enableTranslationsYZ(false);
        advancedDialog.getContentPane().add(settingsPanel, BorderLayout.NORTH);

        // Okay-Cancel Panel
        JPanel okayCancelPanel = new JPanel(new FlowLayout());
        JButton advCancelButton = new JButton("Cancel");
        advCancelButton.setActionCommand("AdvancedCancel");
        advCancelButton.addActionListener(this);
        advCancelButton.setPreferredSize(new Dimension(120, 30));
        advCancelButton.setFont(serif12B);

        JButton okayButton = new JButton("OK");
        okayButton.setActionCommand("AdvancedOkay");
        okayButton.addActionListener(this);
        okayButton.setPreferredSize(new Dimension(120, 30));
        okayButton.setFont(serif12B);
        okayCancelPanel.add(okayButton);
        okayCancelPanel.add(advCancelButton);

        advancedDialog.getContentPane().add(okayCancelPanel, BorderLayout.SOUTH);

        Rectangle dialogBounds = this.getBounds();
        advancedDialog.setLocation((int) ((Toolkit.getDefaultToolkit().getScreenSize().width / 2) -
                                          (dialogBounds.width / 2)),
                                   (Toolkit.getDefaultToolkit().getScreenSize().height / 2) -
                                   (dialogBounds.height / 2));
        advancedDialog.setPreferredSize(new Dimension(470, 460));
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

        Enumeration<String> names = UI.getRegisteredImageNames();

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
        comboBox.addItemListener(this);

        return comboBox;
    }

    /**
     * DOCUMENT ME!
     *
     * @param  state  DOCUMENT ME!
     */
    private void enableTranslationsYZ(boolean state) {
        labelTranslateRangeY.setEnabled(state);
        labelTranslateRangeZ.setEnabled(state);
        minTransTextY.setEnabled(state);
        minTransTextZ.setEnabled(state);
        toLabelY.setEnabled(state);
        toLabelZ.setEnabled(state);
        maxTransTextY.setEnabled(state);
        maxTransTextZ.setEnabled(state);
        mmLabelY.setEnabled(state);
        mmLabelZ.setEnabled(state);
    }

    /**
     * DOCUMENT ME!
     *
     * @param  state  DOCUMENT ME!
     */
    private void enableTranslationX(boolean state) {
        labelTranslateRangeX.setEnabled(state);
        minTransTextX.setEnabled(state);
        toLabelX.setEnabled(state);
        maxTransTextX.setEnabled(state);
        mmLabelX.setEnabled(state);
    }

    /**
     * Initializes the GUI components and displays the dialog.
     */
    private void init() {

        // Initialize translation limit values
        // (Do this here, b/c we don't want it redone if user reselects Advanced Settings.)
        float[] resolution = null;
        float[] halfFov = { 0, 0, 0 };
        int[] extents = null;
        transLimits = new float[2][3];
        maxLimits = new float[2][3];
        extents = matchImage.getExtents();
        resolution = matchImage.getFileInfo(0).getResolutions();
        matchImage.calcMinMax();
        imageMin = matchImage.getMin();
        imageMax = matchImage.getMax();
        dataType = matchImage.getFileInfo()[0].getDataType();

        for (int i = 0; i < 3; i++) {
            halfFov[i] = extents[i] * resolution[i] / 2.0f;
            maxLimits[0][i] = -Math.round(halfFov[i]);
            maxLimits[1][i] = Math.round(halfFov[i]);
            transLimits[0][i] = maxLimits[0][i];
            transLimits[1][i] = maxLimits[1][i];
        }

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
        
        JLabel labelSearch = new JLabel("Search algorithm:");
        labelSearch.setForeground(Color.black);
        labelSearch.setFont(serif12);
        labelSearch.setAlignmentX(Component.LEFT_ALIGNMENT);
        
        comboBoxSearchAlgo = new JComboBox();
        comboBoxSearchAlgo.setFont(MipavUtil.font12);
        comboBoxSearchAlgo.setBackground(Color.white);
        comboBoxSearchAlgo.setToolTipText("Search algorithm");
        comboBoxSearchAlgo.addItem("Powell's calling Brent's");
        comboBoxSearchAlgo.addItem("ELSUNC");
        comboBoxSearchAlgo.setSelectedIndex(0);
        

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

        calcCOGCheckbox = new JCheckBox("Initialize registration process by aligning the COG's.");
        calcCOGCheckbox.setFont(serif12);
        calcCOGCheckbox.setForeground(Color.black);
        calcCOGCheckbox.setSelected(false); // ZAC: change back to true
        calcCOGCheckbox.setEnabled(true);
        calcCOGCheckbox.addItemListener(this);

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
        gbc.weightx = 0;
        gbc.fill = GridBagConstraints.NONE;
        optPanel.add(labelSearch, gbc);
        gbc.gridx = 1;
        gbc.gridy = 5;
        gbc.weightx = 1;
        gbc.fill = GridBagConstraints.HORIZONTAL;
        optPanel.add(comboBoxSearchAlgo, gbc);

        gbc.gridx = 0;
        gbc.gridy = 6;
        gbc.weightx = 1;
        gbc.fill = GridBagConstraints.HORIZONTAL;
        gbc.gridwidth = 7;
        optPanel.add(minMaxCheckbox, gbc);

        // Rotation Search Range Panel
        rotatePanel = new JPanel();
        rotatePanel.setLayout(new BoxLayout(rotatePanel, BoxLayout.Y_AXIS));
        rotatePanel.setBorder(buildTitledBorder("Rotation Search Range"));

        // Radio buttons
        universalRotationCheckbox = new JCheckBox("Apply same rotations to all dimensions.", true);
        universalRotationCheckbox.setFont(serif12);
        universalRotationCheckbox.addItemListener(this);

        rotatePanel.add(universalRotationCheckbox, gbc);

        xRRadio = new JRadioButton("X", true);
        xRRadio.setEnabled(false);
        xRRadio.addItemListener(this);

        yRRadio = new JRadioButton("Y", false);
        yRRadio.setEnabled(false);
        yRRadio.addItemListener(this);

        zRRadio = new JRadioButton("Z", false);
        zRRadio.setEnabled(false);
        zRRadio.addItemListener(this);

        ButtonGroup dimensionRGroup = new ButtonGroup();
        dimensionRGroup.add(xRRadio);
        dimensionRGroup.add(yRRadio);
        dimensionRGroup.add(zRRadio);

        JPanel xyzRotPanel = new JPanel(new FlowLayout(FlowLayout.LEFT));
        xyzRotPanel.setAlignmentX(Component.LEFT_ALIGNMENT);

        xyzRotPanel.add(xRRadio);
        xyzRotPanel.add(yRRadio);
        xyzRotPanel.add(zRRadio);

        rotatePanel.add(xyzRotPanel, gbc);

        // X Rotation Range Panel
        rotateRangePanelX = new JPanel(new FlowLayout(FlowLayout.LEFT));
        rotateRangePanelX.setAlignmentX(Component.LEFT_ALIGNMENT);

        // X - rotation range
        JLabel labelRotateRangeX = new JLabel("Rotation angle sampling range:");
        labelRotateRangeX.setFont(serif12);

        JLabel labelRotateRangeToX = new JLabel("to");
        labelRotateRangeToX.setFont(serif12);

        JLabel labelRotateDegreesX = new JLabel("degrees");
        labelRotateDegreesX.setFont(serif12);
        rotateBeginTextX = new JTextField("-10", 3);
        rotateEndTextX = new JTextField("10", 3);
        // when done debugging:
        // rotateBeginTextX = new JTextField("-30", 3);
        // rotateEndTextX = new JTextField("30", 3);

        rotateRangePanelX.add(labelRotateRangeX);
        rotateRangePanelX.add(rotateBeginTextX);
        rotateRangePanelX.add(labelRotateRangeToX);
        rotateRangePanelX.add(rotateEndTextX);
        rotateRangePanelX.add(labelRotateDegreesX);

        // X - Coarse sampling rate panel
        coarsePanelX = new JPanel(new FlowLayout(FlowLayout.LEFT));
        coarsePanelX.setAlignmentX(Component.LEFT_ALIGNMENT);

        JLabel labelCoarseX = new JLabel("Number of angles to sample in coarse grid: ");
        labelCoarseX.setFont(serif12);
        numberCoarseTextX = new JTextField("3", 3);
        coarsePanelX.add(labelCoarseX);
        coarsePanelX.add(numberCoarseTextX);

        /*
         *   // X - Fine sampling rate panel  finePanelX = new JPanel(new FlowLayout(FlowLayout.LEFT));  JLabel
         * labelFineX = new JLabel("Fine angle increment:");  labelFineX.setAlignmentX(Component.LEFT_ALIGNMENT);
         * labelFineX.setFont(serif12);  JLabel labelFineDegreesX = new JLabel("degrees");
         * labelFineDegreesX.setFont(serif12);  fineRateTextX = new JTextField("6", 3);
         *
         * finePanelX.add(labelFineX);  finePanelX.add(fineRateTextX);  finePanelX.add(labelFineDegreesX);
         * finePanelX.setAlignmentX(Component.LEFT_ALIGNMENT);
         */

        // Add X Rotation Panel
        rotatePanel.add(rotateRangePanelX);
        rotatePanel.add(coarsePanelX);
        // rotatePanel.add(finePanelX);

        // Y Panel
        rotateRangePanelY = new JPanel(new FlowLayout(FlowLayout.LEFT));
        rotateRangePanelY.setAlignmentX(Component.LEFT_ALIGNMENT);

        // Y rotation range
        JLabel labelRotateRangeY = new JLabel("Rotation angle sampling range:");
        labelRotateRangeY.setForeground(Color.black);
        labelRotateRangeY.setFont(serif12);

        JLabel labelRotateRangeToY = new JLabel("to");
        labelRotateRangeToY.setForeground(Color.black);
        labelRotateRangeToY.setFont(serif12);

        JLabel labelRotateDegreesY = new JLabel("degrees");
        labelRotateDegreesY.setFont(serif12);

        rotateBeginTextY = new JTextField("-10", 3);
        rotateEndTextY = new JTextField("10", 3);

        rotateRangePanelY.add(labelRotateRangeY);
        rotateRangePanelY.add(rotateBeginTextY);
        rotateRangePanelY.add(labelRotateRangeToY);
        rotateRangePanelY.add(rotateEndTextY);
        rotateRangePanelY.add(labelRotateDegreesY);

        // Y - Coarse sampling rate panel
        coarsePanelY = new JPanel(new FlowLayout(FlowLayout.LEFT));
        coarsePanelY.setAlignmentX(Component.LEFT_ALIGNMENT);

        JLabel labelCoarseY = new JLabel("Number of angles to sample in coarse grid: ");
        labelCoarseY.setForeground(Color.black);
        labelCoarseY.setFont(serif12);
        labelCoarseY.setAlignmentX(Component.LEFT_ALIGNMENT);
        numberCoarseTextY = new JTextField("3", 3);
        coarsePanelY.add(labelCoarseY);
        coarsePanelY.add(numberCoarseTextY);

        /*
         *   // Y - Fine sampling rate panel  finePanelY = new JPanel(new FlowLayout(FlowLayout.LEFT));
         *
         * JLabel labelFineY = new JLabel("Fine angle increment:");  labelFineY.setForeground(Color.black);
         * labelFineY.setFont(serif12);  labelFineY.setAlignmentX(Component.LEFT_ALIGNMENT);  JLabel labelFineDegreesY =
         * new JLabel("degrees");  labelFineDegreesY.setFont(serif12);
         *
         * fineRateTextY = new JTextField("6", 3);
         *
         * finePanelY.add(labelFineY);  finePanelY.add(fineRateTextY);  finePanelY.add(labelFineDegreesY);
         * finePanelY.setAlignmentX(Component.LEFT_ALIGNMENT);
         */

        // Z Panel
        rotateRangePanelZ = new JPanel(new FlowLayout(FlowLayout.LEFT));
        rotateRangePanelZ.setAlignmentX(Component.LEFT_ALIGNMENT);

        // Z rotation range
        JLabel labelRotateRangeZ = new JLabel("Rotation angle sampling range:");
        labelRotateRangeZ.setForeground(Color.black);
        labelRotateRangeZ.setFont(serif12);

        JLabel labelRotateRangeToZ = new JLabel("to");
        labelRotateRangeToZ.setForeground(Color.black);
        labelRotateRangeToZ.setFont(serif12);

        JLabel labelRotateDegreesZ = new JLabel("degrees");
        labelRotateDegreesZ.setFont(serif12);

        rotateBeginTextZ = new JTextField("-10", 3);
        rotateEndTextZ = new JTextField("10", 3);

        rotateRangePanelZ.add(labelRotateRangeZ);
        rotateRangePanelZ.add(rotateBeginTextZ);
        rotateRangePanelZ.add(labelRotateRangeToZ);
        rotateRangePanelZ.add(rotateEndTextZ);
        rotateRangePanelZ.add(labelRotateDegreesZ);

        // Z - Coarse sampling rate panel
        coarsePanelZ = new JPanel(new FlowLayout(FlowLayout.LEFT));
        coarsePanelZ.setAlignmentX(Component.LEFT_ALIGNMENT);

        JLabel labelCoarseZ = new JLabel("Number of angles to sample in coarse grid: ");
        labelCoarseZ.setForeground(Color.black);
        labelCoarseZ.setFont(serif12);
        labelCoarseZ.setAlignmentX(Component.LEFT_ALIGNMENT);
        numberCoarseTextZ = new JTextField("3", 3);

        coarsePanelZ.add(labelCoarseZ);
        coarsePanelZ.add(numberCoarseTextZ);

        /*
         *   // Z - Fine sampling rate panel  finePanelZ = new JPanel(new FlowLayout(FlowLayout.LEFT));
         *
         * JLabel labelFineZ = new JLabel("Fine angle increment:");  labelFineZ.setForeground(Color.black);
         * labelFineZ.setFont(serif12);  labelFineZ.setAlignmentX(Component.LEFT_ALIGNMENT);  JLabel labelFineDegreesZ =
         * new JLabel("degrees");  labelFineDegreesZ.setFont(serif12);
         *
         * fineRateTextZ = new JTextField("6", 3);
         *
         * finePanelZ.add(labelFineZ);  finePanelZ.add(fineRateTextZ);  finePanelZ.add(labelFineDegreesZ);
         * finePanelZ.setAlignmentX(Component.LEFT_ALIGNMENT);
         */

        // Set weighting for registration
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
        
        outOfBoundsLabel = new JLabel("Out of bounds data:");
        outOfBoundsLabel.setForeground(Color.black);
        outOfBoundsLabel.setFont(serif12);
        outOfBoundsLabel.setAlignmentX(Component.LEFT_ALIGNMENT);
        
        outOfBoundsComboBox = new JComboBox();
        outOfBoundsComboBox.setFont(serif12);
        outOfBoundsComboBox.setBackground(Color.white);
        outOfBoundsComboBox.setAlignmentX(Component.LEFT_ALIGNMENT);
        
        outOfBoundsComboBox.addItem("Image minimum");
        outOfBoundsComboBox.addItem("If float NaN, else 0");
        outOfBoundsComboBox.addItem("User defined");
        outOfBoundsComboBox.addItem("Image maximum");
        outOfBoundsComboBox.setSelectedIndex(0);
        outOfBoundsComboBox.addItemListener(this);
        
        valueLabel = new JLabel("Out of bounds intensity value:");
        valueLabel.setForeground(Color.black);
        valueLabel.setFont(serif12);
        valueLabel.setAlignmentX(Component.LEFT_ALIGNMENT);
        
        valueText = new JTextField(String.valueOf(imageMin));
        valueText.setFont(serif12);
        valueText.setEnabled(false);
        
        matrixLabel = new JLabel("Matrix file directory");
        matrixLabel.setForeground(Color.black);
        matrixLabel.setFont(serif12);
        matrixLabel.setAlignmentX(Component.LEFT_ALIGNMENT);
        
        matrixComboBox = new JComboBox();
        matrixComboBox.setFont(serif12);
        matrixComboBox.setBackground(Color.white);
        matrixComboBox.setAlignmentX(Component.LEFT_ALIGNMENT);
        
        refImage = UI.getRegisteredImageByName((String) comboBoxImage.getSelectedItem());
        
        if (refImage != null) {
            matrixComboBox.addItem(refImage.getImageDirectory());	
        }
        if ((matchImage.getImageDirectory() != null) && 
        	(!refImage.getImageDirectory().equals(matchImage.getImageDirectory()))){
        	matrixComboBox.addItem(matchImage.getImageDirectory());
        }
        if ((UI.getDefaultDirectory() != null) && 
        	(!UI.getDefaultDirectory().equals(refImage.getImageDirectory())) &&
        	(!UI.getDefaultDirectory().equals(matchImage.getImageDirectory()))) {
        	matrixComboBox.addItem(UI.getDefaultDirectory());
        }
        matrixComboBox.addItem("User specified matrix directory");
        matrixComboBox.setSelectedIndex(0);
        
        userDirectoryLabel = new JLabel("User specified matrix directory");
        userDirectoryLabel.setForeground(Color.black);
        userDirectoryLabel.setFont(serif12);
        userDirectoryLabel.setAlignmentX(Component.LEFT_ALIGNMENT);
        
        userDirectoryText = new JTextField();
        userDirectoryText.setFont(serif12);
        userDirectoryText.setEnabled(true);

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
        gbc.gridx = 0;
        gbc.gridy = 2;
        gbc.weightx = 0;
        gbc.fill = GridBagConstraints.NONE;
        outPanel.add(outOfBoundsLabel, gbc);
        gbc.gridx = 1;
        gbc.gridy = 2;
        gbc.weightx = 1;
        gbc.fill = GridBagConstraints.HORIZONTAL;
        outPanel.add(outOfBoundsComboBox, gbc);
        gbc.gridx = 0;
        gbc.gridy = 3;
        gbc.weightx = 0;
        gbc.fill = GridBagConstraints.NONE;
        outPanel.add(valueLabel, gbc);
        gbc.gridx = 1;
        gbc.gridy = 3;
        gbc.weightx = 1;
        gbc.weightx = 1;
        gbc.fill = GridBagConstraints.HORIZONTAL;
        outPanel.add(valueText, gbc);
        gbc.gridx = 0;
        gbc.gridy = 4;
        gbc.weightx = 0;
        gbc.fill = GridBagConstraints.NONE;
        outPanel.add(matrixLabel, gbc);
        gbc.gridx = 1;
        gbc.gridy = 4;
        gbc.weightx = 1;
        gbc.fill = GridBagConstraints.HORIZONTAL;
        outPanel.add(matrixComboBox, gbc);
        gbc.gridx = 0;
        gbc.gridy = 5;
        gbc.weightx = 0;
        gbc.fill = GridBagConstraints.NONE;
        outPanel.add(userDirectoryLabel, gbc);
        gbc.gridx = 1;
        gbc.gridy = 5;
        gbc.weightx = 1;
        gbc.weightx = 1;
        gbc.fill = GridBagConstraints.HORIZONTAL;
        outPanel.add(userDirectoryText, gbc);

        JPanel buttonPanel = new JPanel();
        buildOKButton();
        buttonPanel.add(OKButton);
        buildCancelButton();
        buttonPanel.add(cancelButton);

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
        Integer tempInteger = new Integer(1);

        refImage = UI.getRegisteredImageByName((String) comboBoxImage.getSelectedItem());
        weighted = weightRadio.isSelected();
        maxOfMinResol = minMaxCheckbox.isSelected();
        voisOnly = voiRadio.isSelected();

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
                MipavUtil.displayError("Out of memory in JDialogConstrainedOAR3D");

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
        
        switch(comboBoxSearchAlgo.getSelectedIndex()) {
        case 0:
        	useELSUNC = false;
        	break;
        case 1:
        	useELSUNC = true;
        	break;
        default:
        	useELSUNC = false;
        }

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
                // case 7:  interp = AlgorithmTransform.NEAREST_NEIGHBOR;   break;

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
            rotateRangeX = rotateEndX - rotateBeginX;
        }

        if (rotateRangeX < 0) {
            MipavUtil.displayError("Beginning of rangeX must be less than end of range.");
            showX();
            rotateBeginTextX.requestFocus();
            rotateBeginTextX.selectAll();

            return false;
        }

        if (!testParameter(numberCoarseTextX.getText(), 1, 100)) {
            showX();
            numberCoarseTextX.requestFocus();
            numberCoarseTextX.selectAll();

            return false;
        } else {
            tempInteger = new Integer(numberCoarseTextX.getText());
            numCoarseX = tempInteger.intValue();
        }

        if (numCoarseX == 1) {
            int response = JOptionPane.showConfirmDialog(this, "Warning: There will only be 1 sampling.  Continue?",
                                                         "Sampling warning", JOptionPane.YES_NO_OPTION,
                                                         JOptionPane.WARNING_MESSAGE);

            if (response == JOptionPane.NO_OPTION) {
                showX();
                numberCoarseTextX.requestFocus();
                numberCoarseTextX.selectAll();

                return false;
            }
        }

        if (universalRotationCheckbox.isSelected()) {
            rotateBeginY = rotateBeginX;
            rotateBeginZ = rotateBeginX;
            rotateEndY = rotateEndX;
            rotateEndZ = rotateEndX;
            rotateRangeY = rotateRangeX;
            rotateRangeZ = rotateRangeX;
            numCoarseY = numCoarseX;
            numCoarseZ = numCoarseX;
        } else { // universalRotationCheckbox not selected

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
                rotateRangeY = rotateEndY - rotateBeginY;
            }

            if (rotateRangeY < 0) {
                MipavUtil.displayError("Beginning of rangeY must be less than end of range.");
                showY();
                rotateBeginTextY.requestFocus();
                rotateBeginTextY.selectAll();

                return false;
            }

            if (!testParameter(numberCoarseTextY.getText(), 1, 100)) {
                showY();
                numberCoarseTextY.requestFocus();
                numberCoarseTextY.selectAll();

                return false;
            } else {
                tempInteger = new Integer(numberCoarseTextY.getText());
                numCoarseY = tempInteger.intValue();
            }

            if (numCoarseY == 1) {
                int response = JOptionPane.showConfirmDialog(this, "Warning: There will only be 1 Y sampling.  Continue?",
                                                             "Sampling warning", JOptionPane.YES_NO_OPTION,
                                                             JOptionPane.WARNING_MESSAGE);

                if (response == JOptionPane.NO_OPTION) {
                    showY();
                    numberCoarseTextY.requestFocus();
                    numberCoarseTextY.selectAll();

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
                rotateRangeZ = rotateEndZ - rotateBeginZ;
            }

            if (rotateRangeZ < 0) {
                MipavUtil.displayError("Beginning of rangeZ must be less than end of range.");
                showZ();
                rotateBeginTextZ.requestFocus();
                rotateBeginTextZ.selectAll();

                return false;
            }

            if (!testParameter(numberCoarseTextZ.getText(), 1, 100)) {
                showZ();
                numberCoarseTextZ.requestFocus();
                numberCoarseTextZ.selectAll();

                return false;
            } else {
                tempInteger = new Integer(numberCoarseTextZ.getText());
                numCoarseZ = tempInteger.intValue();
            }

            if (numCoarseZ == 1) {
                int response = JOptionPane.showConfirmDialog(this, "Warning: There will only be 1 Z sampling.  Continue?",
                                                             "Sampling warning", JOptionPane.YES_NO_OPTION,
                                                             JOptionPane.WARNING_MESSAGE);

                if (response == JOptionPane.NO_OPTION) {
                    showZ();
                    numberCoarseTextZ.requestFocus();
                    numberCoarseTextZ.selectAll();

                    return false;
                }
            }

        } // else universalRotationCheckbox not selected

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
        
        fillValue = Float.valueOf(valueText.getText()).floatValue();
        outOfBoundsIndex = outOfBoundsComboBox.getSelectedIndex();
        if (outOfBoundsIndex == 2) {
            // user defined value
            boolean success = testType(dataType, fillValue);
            if (!success) {
                MipavUtil.displayError("User defined value is out of the data type range");
                valueText.requestFocus();
                valueText.selectAll();
                return false;
            }
        }
        
        matrixDirectory = (String)matrixComboBox.getSelectedItem();
        if (matrixDirectory != null) {
	        if (matrixDirectory.equals("User specified matrix directory")) {
	            matrixDirectory = userDirectoryText.getText();	
	        }
        }

        return true;
    }
    
    /**
     * Determine if the value is in the image type range and
     * within the float range since AlgorithmTransform does
     * not use double buffers.
     *
     * @param   type    image type
     * @param   value   value tested
     *
     * @return  true if value is within acceptable range
     */
    private boolean testType(int type, float value) {

        if (type == ModelStorageBase.BOOLEAN) {

            if ((value < 0) || (value > 1)) {
                return false;
            } else {
                return true;
            }
        } else if (type == ModelStorageBase.BYTE) {

            if ((value < -128) || (value > 127)) {
                return false;
            } else {
                return true;
            }
        } else if (type == ModelStorageBase.UBYTE) {

            if ((value < 0) || (value > 255)) {
                return false;
            } else {
                return true;
            }
        } else if (type == ModelStorageBase.SHORT) {

            if ((value < -32768) || (value > 32767)) {
                return false;
            } else {
                return true;
            }
        } else if (type == ModelStorageBase.USHORT) {

            if ((value < 0) || (value > 65535)) {
                return false;
            } else {
                return true;
            }
        } else if (type == ModelStorageBase.INTEGER) {

            if ((value < Integer.MIN_VALUE) || (value > Integer.MAX_VALUE)) {
                return false;
            } else {
                return true;
            }
        } else if (type == ModelStorageBase.UINTEGER) {

            if ((value < 0) || (value > 4294967295L)) {
                return false;
            } else {
                return true;
            }
        } else if (type == ModelStorageBase.LONG) {

            if ((value < Long.MIN_VALUE) || (value > Long.MAX_VALUE)) {
                return false;
            } else {
                return true;
            }
        } else if (type == ModelStorageBase.FLOAT) {

            if ((value < -Float.MAX_VALUE) || (value > Float.MAX_VALUE)) {
                return false;
            } else {
                return true;
            }
        } else if (type == ModelStorageBase.DOUBLE) {
            // Float buffers are used in the AlgorithmTransform routines
            if ((value < -Float.MAX_VALUE) || (value > Float.MAX_VALUE)) {
                return false;
            } else {
                return true;
            }
        } else if (type == ModelStorageBase.ARGB) {

            if ((value < 0) || (value > 255)) {
                return false;
            } else {
                return true;
            }
        } else if (type == ModelStorageBase.ARGB_USHORT) {

            if ((value < 0) || (value > 65535)) {
                return false;
            } else {
                return true;
            }
        } else if (type == ModelStorageBase.ARGB_FLOAT) {

            if ((value < -Float.MAX_VALUE) || (value > Float.MAX_VALUE)) {
                return false;
            } else {
                return true;
            }
        } else {
            return false;
        }
    }

    /**
     * DOCUMENT ME!
     */
    private void showX() {

        if (xRSelected) {
            return;
        } else if (yRSelected) {
            rotatePanel.remove(rotateRangePanelY);
            rotatePanel.remove(coarsePanelY);

            // rotatePanel.remove(finePanelY);
            yRSelected = false;
        } else { // if (zRSelected)
            rotatePanel.remove(rotateRangePanelZ);
            rotatePanel.remove(coarsePanelZ);

            // rotatePanel.remove(finePanelZ);
        } // else if zRSelected

        xRSelected = true;
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

        // rotatePanel.add(finePanelX, gbc);
        xRRadio.setEnabled(false);
        yRRadio.setEnabled(false);
        zRRadio.setEnabled(false);
        xRRadio.setSelected(true);
        yRRadio.setSelected(false);
        zRRadio.setSelected(false);
        xRRadio.setEnabled(true);
        yRRadio.setEnabled(true);
        zRRadio.setEnabled(true);
    }

    /**
     * DOCUMENT ME!
     */
    private void showY() {

        if (xRSelected) {
            rotatePanel.remove(rotateRangePanelX);
            rotatePanel.remove(coarsePanelX);

            // rotatePanel.remove(finePanelX);
            xRSelected = false;
        } // if (xRSelected)
        else if (yRSelected) {
            return;
        } else { // zRSelected
            rotatePanel.remove(rotateRangePanelZ);
            rotatePanel.remove(coarsePanelZ);

            // rotatePanel.remove(finePanelZ);
        } // else zRSelected

        yRSelected = true;
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

        // rotatePanel.add(finePanelY, gbc);
        xRRadio.setEnabled(false);
        yRRadio.setEnabled(false);
        zRRadio.setEnabled(false);
        xRRadio.setSelected(false);
        yRRadio.setSelected(true);
        zRRadio.setSelected(false);
        xRRadio.setEnabled(true);
        yRRadio.setEnabled(true);
        zRRadio.setEnabled(true);
    }

    /**
     * DOCUMENT ME!
     */
    private void showZ() {

        if (xRSelected) {
            rotatePanel.remove(rotateRangePanelX);
            rotatePanel.remove(coarsePanelX);

            // rotatePanel.remove(finePanelX);
            xRSelected = false;
        } // if (xSelcted)
        else if (yRSelected) {
            rotatePanel.remove(rotateRangePanelY);
            rotatePanel.remove(coarsePanelY);

            // rotatePanel.remove(finePanelY);
            yRSelected = false;
        } // else if (yRSelected)
        else { // zRSelected
            return;
        } // else zRSelected

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

        // rotatePanel.add(finePanelZ, gbc);
        xRRadio.setEnabled(false);
        yRRadio.setEnabled(false);
        zRRadio.setEnabled(false);
        xRRadio.setSelected(false);
        yRRadio.setSelected(false);
        zRRadio.setSelected(true);
        xRRadio.setEnabled(true);
        yRRadio.setEnabled(true);
        zRRadio.setEnabled(true);
    }
}
