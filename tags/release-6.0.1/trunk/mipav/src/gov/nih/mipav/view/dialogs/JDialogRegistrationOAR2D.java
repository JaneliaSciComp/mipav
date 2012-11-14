package gov.nih.mipav.view.dialogs;


import gov.nih.mipav.model.algorithms.*;
import gov.nih.mipav.model.algorithms.registration.*;
import gov.nih.mipav.model.file.*;
import gov.nih.mipav.model.scripting.*;
import gov.nih.mipav.model.scripting.parameters.*;
import gov.nih.mipav.model.structures.*;

import gov.nih.mipav.util.ThreadUtil;
import gov.nih.mipav.view.*;

import java.awt.*;
import java.awt.event.*;

import java.io.*;

import java.text.DecimalFormat;
import java.text.DecimalFormatSymbols;
import java.util.*;

import javax.swing.*;


/**
 * Dialog to get user input, then call AlgorithmRegOAR2D.
 *
 * @author  Neva Cherniavsky
 * @see     AlgorithmCostFunctions
 * @see     AlgorithmRegOAR2D
 */
public class JDialogRegistrationOAR2D extends JDialogScriptableBase implements AlgorithmInterface, ActionDiscovery, ScriptableActionInterface {

    //~ Static fields/initializers -------------------------------------------------------------------------------------

    /** Use serialVersionUID for interoperability. */
    private static final long serialVersionUID = -688716905705879638L;

    //~ Instance fields ------------------------------------------------------------------------------------------------
    
    // Different search algorithms
    private final int POWELL = 0;
    
	private final int ELSUNC = 1;
	
	private final int LEVENBERG_MARQUARDT = 2;
	
	private final int NL2SOL = 3;
	
	private int searchAlgorithm = POWELL;

    /** Variables for Advanced Settings dialog. */
    private JDialog advancedDialog;

    /** DOCUMENT ME! */
    private JTextField maxIterationsText, numMinText;

    /** CheckBox to turn brute-force registration on or off:. */
    private JCheckBox bruteForceCheckBox = null;

    /** Dialog to set the brute-force registration parameters:. */
    private JDialog bruteForceDialog;

    /** DOCUMENT ME! */
    private JButton buttonWeightInput;

    /** DOCUMENT ME! */
    private JButton buttonWeightRef;

    /** DOCUMENT ME! */
    private JTextField coarseRateText;

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
    private boolean displayTransform;

    /** DOCUMENT ME! */
    private boolean doColor;

    /** CheckBox to turn color registration on or off:. */
    private JCheckBox doColorCheckBox = null;

    /** DOCUMENT ME! */
    private boolean doSubsample;
    private boolean doJTEM;
    private boolean doMultiThread;

    /** DOCUMENT ME! */
    private String fileNameWRef, directoryWRef, fileNameWInput, directoryWInput;

    /** DOCUMENT ME! */
    private JTextField fineRateText;

    /** DOCUMENT ME! */
    private ModelImage inputWeightImage, refWeightImage;

    /** DOCUMENT ME! */
    private JLabel labelCoarse;

    /** DOCUMENT ME! */
    private JLabel labelCoarseDegrees;

    /** DOCUMENT ME! */
    private JLabel labelFine;

    /** DOCUMENT ME! */
    private JLabel labelFineDegrees;

    /** DOCUMENT ME! */
    private JLabel labelInterp2;

    /** DOCUMENT ME! */
    private JLabel labelRotateDegrees;

    /** DOCUMENT ME! */
    private JLabel labelRotateRange;

    /** DOCUMENT ME! */
    private JLabel labelRotateRangeTo;

    /** If true the dialog for this instance of JDialogRegistrationOAR2D will not activate the AlgorithmRegOAR2D:. */
    private boolean m_bSubsetDialog = false;

    /** DOCUMENT ME! */
    private ModelImage matchImage; // register match image to reference Image

    /** DOCUMENT ME! */
    private int maxIterations_def = 2, numMinima_def = 3;

    /** DOCUMENT ME! */
    private int maxIterations = maxIterations_def;

    /** DOCUMENT ME! */
    private JRadioButton noneRadio;

    /** DOCUMENT ME! */
    private int numMinima = numMinima_def;

    /** DOCUMENT ME! */
    private ModelImage refImage;

    /** DOCUMENT ME! */
    private AlgorithmRegOAR2D reg2 = null;
    
    private AlgorithmRegELSUNCOAR2D reg2E = null;

    /** DOCUMENT ME! */
    private ModelImage resultImage = null;

    /** DOCUMENT ME! */
    private float rotateBegin, rotateEnd, coarseRate, fineRate;

    /** DOCUMENT ME! */
    private JTextField rotateBeginText;

    /** DOCUMENT ME! */
    private JTextField rotateEndText;

    /** DOCUMENT ME! */
    private float rotationBF = 0f, xscaleBF = 0f, yscaleBF = 0f;

    /** Brute-force registration parameters:. */
    private JTextField rotationText, translationText, xscaleText, yscaleText, scaleStepsText;

    /** DOCUMENT ME! */
    private JCheckBox sampleCheckBox;
    /** DOCUMENT ME! */
    private JCheckBox jtemCheckBox;
    
    private JCheckBox multiThreadCheckBox;

    /** DOCUMENT ME! */
    private int scaleStepsBF;

    /** DOCUMENT ME! */
    private JTextField textInput;

    /** DOCUMENT ME! */
    private JTextField textRef;

    /** DOCUMENT ME! */
    private JCheckBox transformCheckbox;

    /** DOCUMENT ME! */
    private int translationBF = 0;

    /** DOCUMENT ME! */
    private ViewUserInterface UI;

    /** DOCUMENT ME! */
    private JRadioButton voiRadio;

    /** DOCUMENT ME! */
    private boolean voisOnly;

    /** DOCUMENT ME! */
    private boolean weighted;

    /** DOCUMENT ME! */
    private JRadioButton weightRadio;
    
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

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Empty constructor needed for dynamic instantiation (used during scripting).
     */
    public JDialogRegistrationOAR2D() { }

    /**
     * Creates new dialog for user to choose type of 2D image registration algorithm to run.
     *
     * @param  theParentFrame  Parent frame.
     * @param  im              Source image.
     */
    public JDialogRegistrationOAR2D(Frame theParentFrame, ModelImage im) {
        super(theParentFrame, false);
        matchImage = im;

        if (matchImage.isColorImage()) {
            doColor = true;
        } else {
            doColor = false;
        }

        UI = ViewUserInterface.getReference();
        init();
    }

    /**
     * Creates new dialog for user to choose type of 2D image registration algorithm to run.
     *
     * @param  theParentFrame     Parent frame.
     * @param  ref                Reference image.
     * @param  match              Match image.
     * @param  iCost              default cost choice.
     * @param  iDOF               default degrees of freedom choice.
     * @param  iInterp            default image interpolation choice.
     * @param  fRotateBegin       default rotation range choice.
     * @param  fRotateEnd         default rotation range choice.
     * @param  fCoarseRate        default rotation coarse rate choice.
     * @param  fFineRate          default rotation fine rate choice.
     * @param  bDoSubsample       default subsampling choice.
     * @param  iMaxIterations     default max iterations choice.
     * @param  iNumMin            default number of minima choice.
     * @param  iInterp2           default display interpolation choice.
     * @param  bDisplayTransform  default display transformed image choice.
     * @param  fRotateBF          default rotation angle range for brute-force registration.
     * @param  fXScaleBF          default scale in x range for brute-force registration.
     * @param  fYScaleBF          default scale in x range for brute-force registration.
     * @param  iScaleStepsBF      default number of steps to divide scale range brute-force registration.
     * @param  iTranslationBF     default translation range for brute-force registration.
     */
    public JDialogRegistrationOAR2D(Frame theParentFrame, ModelImage ref, ModelImage match, int iCost, int iDOF,
                                    int iInterp, float fRotateBegin, float fRotateEnd, float fCoarseRate,
                                    float fFineRate, boolean bDoSubsample, int iMaxIterations,
                                    int iNumMin, int iInterp2, boolean bDisplayTransform, float fRotateBF,
                                    float fXScaleBF, float fYScaleBF, int iScaleStepsBF, int iTranslationBF) {
        super(theParentFrame, false);
        matchImage = match;
        refImage = ref;
        UI = ViewUserInterface.getReference();

        /* The dialog for this instance of JDialogRegistrationOAR2D will not
         * activate the AlgorithmRegOAR2D: */
        m_bSubsetDialog = true;

        cost = iCost;
        DOF = iDOF;
        interp = iInterp;
        rotateBegin = fRotateBegin;
        rotateEnd = fRotateEnd;
        coarseRate = fCoarseRate;
        fineRate = fFineRate;
        doSubsample = bDoSubsample;
        maxIterations = iMaxIterations;
        numMinima = iNumMin;

        interp2 = iInterp2;
        displayTransform = bDisplayTransform;

        rotationBF = fRotateBF;
        xscaleBF = fXScaleBF;
        yscaleBF = fYScaleBF;
        scaleStepsBF = iScaleStepsBF;
        translationBF = iTranslationBF;

        weighted = true;

        if (matchImage.isColorImage()) {
            doColor = true;
        } else {
            doColor = false;
        }

        initPresets();
    }

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * Closes dialog box when the OK button is pressed, sets the variables, and calls the algorithm.
     *
     * @param  event  Event that triggers function.
     */
    public void actionPerformed(ActionEvent event) {
        String tmpStr;
        String command = event.getActionCommand();

        if (command.equals("OK")) {

            if (setVariables()) {
                callAlgorithm();
            }
        } else if (command.equals("Cancel")) {
            dispose();
        } else if (command.equals("Help")) {
        	//MipavUtil.showHelp("OAR19076");
            MipavUtil.showWebHelp("Optimized_automatic_registration_3D#Optimized_Automatic_Registration_dialog_box_options");
        } else if (command.equals("AdvancedSettings")) {
            maxIterations_def = maxIterations;
            numMinima_def = numMinima;
            advancedDialog = buildAdvancedDialog(maxIterations, numMinima);
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
            numMinima = numMinima_def;
            advancedDialog.setVisible(false);
            advancedDialog.dispose();
        } else if (command.equals("AdvancedHelp")) {
        	//MipavUtil.showHelp("OAR19078");
            MipavUtil.showWebHelp("Optimized_automatic_registration_3D#Advanced_OAR_settings_for_Constrained_Optimized_Automatic_Registration_3D");
        }
        /* If the image is not processed as a color image, there are more
         * registration cost functions available for the user to choose: */
        else if (command.equals("CheckDoColor")) {
            doColor = doColorCheckBox.isSelected();
            comboBoxCostFunct.removeAllItems();

            if (!doColor) {
                comboBoxCostFunct.addItem("Correlation ratio");
            }

            comboBoxCostFunct.addItem("Least squares");

            if (!doColor) {
                comboBoxCostFunct.addItem("Normalized cross correlation");
                comboBoxCostFunct.addItem("Normalized mutual information");
            }

            //This is least squares if doColor, else correlation ratio
            comboBoxCostFunct.setSelectedIndex(0);
        } else if (command.equals("BruteForceCheck")) {

            if (bruteForceCheckBox.isSelected()) {
                comboBoxDOF.setEnabled(false);
                rotateBeginText.setEnabled(false);
                rotateEndText.setEnabled(false);
                coarseRateText.setEnabled(false);
                fineRateText.setEnabled(false);
            } else {
                comboBoxDOF.setEnabled(true);
                rotateBeginText.setEnabled(true);
                rotateEndText.setEnabled(true);
                coarseRateText.setEnabled(true);
                fineRateText.setEnabled(true);
            }
        }
        /* Sets the parameters in the JFrameRegistrationMosaic object: */
        else if (command.equals("setParams")) {

            if (setVariables()) {
                setVisible(false);
                ((JFrameRegistrationMosaic) parentFrame).getVariablesFromDialog(this, false);
            }
        }
        /* Sets the parameters in the JFrameRegistrationMosaic object and tells it
         * to start the registration: */
        else if (command.equals("setAndRegister")) {

            if (setVariables()) {
                setVisible(false);
                ((JFrameRegistrationMosaic) parentFrame).getVariablesFromDialog(this, true);
            }
        }
        /* Opens the brute force dialog box: */
        else if (command.equals("bruteForceOptions")) {
            bruteForceDialog = buildBruteForceDialog();
        }
        /* Sets the brute force parameters: */
        else if (command.equals("BruteForceOkay")) {
            tmpStr = rotationText.getText();

            if (testParameter(tmpStr, 0, 360)) {
                rotationBF = Float.valueOf(tmpStr).floatValue();
            }

            tmpStr = translationText.getText();

            if (testParameter(tmpStr, 0, Math.max(matchImage.getExtents()[0], matchImage.getExtents()[1]))) {
                translationBF = Integer.valueOf(tmpStr).intValue();
            }

            tmpStr = xscaleText.getText();

            if (testParameter(tmpStr, 0, 10)) {
                xscaleBF = Float.valueOf(tmpStr).floatValue();
            }

            tmpStr = yscaleText.getText();

            if (testParameter(tmpStr, 0, 10)) {
                yscaleBF = Float.valueOf(tmpStr).floatValue();
            }

            tmpStr = scaleStepsText.getText();

            if (testParameter(tmpStr, 0, 100)) {
                scaleStepsBF = Integer.valueOf(tmpStr).intValue();
            }

            bruteForceDialog.setVisible(false);
            bruteForceDialog.dispose();
        }
        /* Closes the brute force dialog: */
        else if (command.equals("BruteForceCancel")) {
            bruteForceDialog.setVisible(false);
            bruteForceDialog.dispose();
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
        double xOrig;
        double yOrig;
        double xCen;
        double yCen;
        double xCenNew;
        double yCenNew;
        float resX;
        float resY;
        TransMatrix xfrm = null;
        //double M[][];
        String comStr;
        DecimalFormat nf;
        ViewUserInterface UI = ViewUserInterface.getReference();
        int xdimA = refImage.getExtents()[0];
        int ydimA = refImage.getExtents()[1];
        float xresA = refImage.getFileInfo(0).getResolutions()[0];
        float yresA = refImage.getFileInfo(0).getResolutions()[1];
        
        nf = new DecimalFormat();
        nf.setMaximumFractionDigits(4);
        nf.setMinimumFractionDigits(0);
        nf.setGroupingUsed(false);

        DecimalFormatSymbols dfs = nf.getDecimalFormatSymbols();
        dfs.setDecimalSeparator('.');
        nf.setDecimalFormatSymbols(dfs);

        if (algorithm instanceof AlgorithmRegOAR2D) {

            if (reg2.isCompleted()) {

                if (displayTransform) {
                    

                    String name = makeImageName(matchImage.getImageName(), "_register");
                    xfrm  = reg2.getTransform();

                    //System.err.println( xfrm );
                    transform = new AlgorithmTransform(matchImage, xfrm, interp2, xresA, yresA, xdimA,
                                                       ydimA, true, false, pad);

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
                    }

                    transform = null;
                }
                else {
                    xfrm = reg2.getTransform();
                }
                
                xOrig = (matchImage.getExtents()[0] - 1.0)/2.0;
                yOrig = (matchImage.getExtents()[1] - 1.0)/2.0;
                resX = matchImage.getFileInfo()[0].getResolutions()[0];
                resY = matchImage.getFileInfo()[0].getResolutions()[1];
                xCen = xOrig * resX;
                yCen = yOrig * resY;
                xfrm.Inverse();
                xCenNew = xCen*xfrm.Get(0, 0) + yCen*xfrm.Get(0, 1) + xfrm.Get(0, 2);
                yCenNew = xCen*xfrm.Get(1, 0) + yCen*xfrm.Get(1, 1) + xfrm.Get(1, 2);
                Preferences.debug("The geometric center of " + matchImage.getImageName() + " at (" 
                                   + xCen + ", " + yCen + ")\n",Preferences.DEBUG_ALGORITHM);
                if (resultImage != null) {
                    comStr = "moves to (" + nf.format(xCenNew) + ", " + nf.format(yCenNew) + ") in " +
                                     resultImage.getImageName() + ".\n";
                }
                else {
                    comStr = "moves to (" + nf.format(xCenNew) + ", " + nf.format(yCenNew) + ").\n";    
                }
                Preferences.debug(comStr,Preferences.DEBUG_ALGORITHM);
                
                if (resultImage != null) {
                    resultImage.getMatrixHolder().replaceMatrices(refImage.getMatrixHolder().getMatrices());
    
                    resultImage.getFileInfo(0).setOrigin(refImage.getFileInfo(0).getOrigin());
                }

                TransMatrix resultMatrix = reg2.getTransform();
                resultMatrix.setTransformID(TransMatrix.TRANSFORM_ANOTHER_DATASET);
                
                matchImage.getMatrixHolder().addMatrix(resultMatrix);

                reg2.getTransform().saveMatrix(matrixDirectory + matchImage.getImageName() + "_To_" +
                                               refImage.getImageName() + ".mat", interp2, xresA, yresA, 0.0f, xdimA,
                                               ydimA, 0, true, false, pad, null);
                Preferences.debug("Saved " + matrixDirectory + matchImage.getImageName() + "_To_" +
                                               refImage.getImageName() + ".mat\n",Preferences.DEBUG_FILEIO);

                insertScriptLine();
            }
         // save the completion status for later
            setComplete(algorithm.isCompleted());

            if (reg2 != null) {
                reg2.disposeLocal();
                reg2 = null;
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
        
        if (algorithm instanceof AlgorithmRegELSUNCOAR2D) {

            if (reg2E.isCompleted()) {

                if (displayTransform) {
                    String name = makeImageName(matchImage.getImageName(), "_register");
                    xfrm  = reg2E.getTransform();

                    //System.err.println( xfrm );
                    transform = new AlgorithmTransform(matchImage, xfrm, interp2, xresA, yresA, xdimA,
                                                       ydimA, true, false, pad);

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
                    }

                    transform = null;
                }
                else {
                    xfrm = reg2E.getTransform();
                }
                
                xOrig = (matchImage.getExtents()[0] - 1.0)/2.0;
                yOrig = (matchImage.getExtents()[1] - 1.0)/2.0;
                resX = matchImage.getFileInfo()[0].getResolutions()[0];
                resY = matchImage.getFileInfo()[0].getResolutions()[1];
                xCen = xOrig * resX;
                yCen = yOrig * resY;
                xfrm.Inverse();
                xCenNew = xCen*xfrm.Get(0, 0) + yCen*xfrm.Get(0, 1) + xfrm.Get(0, 2);
                yCenNew = xCen*xfrm.Get(1, 0) + yCen*xfrm.Get(1, 1) + xfrm.Get(1, 2);
                Preferences.debug("The geometric center of " + matchImage.getImageName() + " at (" 
                                   + xCen + ", " + yCen + ")\n",Preferences.DEBUG_ALGORITHM);
                if (resultImage != null) {
                    comStr = "moves to (" + nf.format(xCenNew) + ", " + nf.format(yCenNew) + ") in " +
                                     resultImage.getImageName() + ".\n";
                }
                else {
                    comStr = "moves to (" + nf.format(xCenNew) + ", " + nf.format(yCenNew) + ").\n";    
                }
                Preferences.debug(comStr,Preferences.DEBUG_ALGORITHM);
                
                if (resultImage != null) {
                    resultImage.getMatrixHolder().replaceMatrices(refImage.getMatrixHolder().getMatrices());
    
                    resultImage.getFileInfo(0).setOrigin(refImage.getFileInfo(0).getOrigin());
                }

                TransMatrix resultMatrix = reg2E.getTransform();
                resultMatrix.setTransformID(TransMatrix.TRANSFORM_ANOTHER_DATASET);
                
                matchImage.getMatrixHolder().addMatrix(resultMatrix);

                reg2E.getTransform().saveMatrix(matrixDirectory + matchImage.getImageName() + "_To_" +
                                               refImage.getImageName() + ".mat", interp2, xresA, yresA, 0.0f, xdimA,
                                               ydimA, 0, true, false, pad, null);
                Preferences.debug("Saved " + matrixDirectory + matchImage.getImageName() + "_To_" +
                                               refImage.getImageName() + ".mat\n",Preferences.DEBUG_FILEIO);

                insertScriptLine();
            }
         // save the completion status for later
            setComplete(algorithm.isCompleted());

            if (reg2E != null) {
                reg2E.disposeLocal();
                reg2E = null;
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
     * Accessor to get whether or not to do the brute-force registration.
     *
     * @return  bruteForceCheckBox.isSelected()
     */
    public boolean getBruteForce() {

        if (bruteForceCheckBox != null) {
            return bruteForceCheckBox.isSelected();
        }

        return false;
    }

    /**
     * Accessor to get the coarse sample begin.
     *
     * @return  rotateBegon Coarse begin
     */
    public float getCoarseBegin() {
        return rotateBegin;
    }

    /**
     * Accessor to get the coarse sample end.
     *
     * @return  rotateEnd Coarse end
     */
    public float getCoarseEnd() {
        return rotateEnd;
    }

    /**
     * Accessor to get the coarse sample rate.
     *
     * @return  coarseRate, Coarse rate
     */
    public float getCoarseRate() {
        return coarseRate;
    }

    /**
     * Accessor to get the choice of cost function.
     *
     * @return  cost, Cost function.
     */
    public int getCostChoice() {
        return cost;
    }

    /**
     * Accessor to get the display transform flag.
     *
     * @return  displayTransform flag <code>true</code> means display the transformed image.
     */
    public boolean getDisplayTransform() {
        return displayTransform;
    }

    /**
     * Accessor to get the degrees of freedom.
     *
     * @return  DOF, Degrees of freedom
     */
    public int getDOF() {
        return DOF;
    }

    /**
     * Accessor to get the fine sample rate.
     *
     * @return  fineRate, Fine rate
     */
    public float getFineRate() {
        return fineRate;
    }

    /**
     * Accessor to get the initial interpolation.
     *
     * @return  interp, Interpolation
     */
    public int getInterp() {
        return interp;
    }

    /**
     * Accessor to get the final interpolation.
     *
     * @return  interp2, Interpolation
     */
    public int getInterp2() {
        return interp2;
    }

    /**
     * Accessor to get maxIterations.
     *
     * @return  maxIterations
     */
    public int getMaxIterations() {
        return maxIterations;
    }

    /**
     * Accessor to get numMinima.
     *
     * @return  numMinima
     */
    public int getNumMinima() {
        return numMinima;
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
     * Accessor to get the rotation range for brute force registration:
     *
     * @return  rotationBF, range of rotations
     */
    public float getRotationBruteForce() {
        return rotationBF;
    }

    /**
     * Accessor to get the y scale range for brute force registration:
     *
     * @return  scaleStepsBF, number of divisions for scale
     */
    public int getScaleStepsBruteForce() {
        return scaleStepsBF;
    }

    /**
     * Accessor to get whether or not subsampling occurs.
     *
     * @return  doSubsample
     */
    public boolean getSubsample() {
        return this.doSubsample;
    }

    /**
     * Accessor to get the translation range for brute force registration:
     *
     * @return  translationBF, range of x,y translations
     */
    public int getTranslationBruteForce() {
        return translationBF;
    }

    /**
     * Accessor to get the x scale range for brute force registration:
     *
     * @return  xscaleBF, range of scales in x
     */
    public float getXScaleBruteForce() {
        return xscaleBF;
    }

    /**
     * Accessor to get the y scale range for brute force registration:
     *
     * @return  yscaleBF, range of scales in y
     */
    public float getYScaleBruteForce() {
        return yscaleBF;
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
        } else if ((event.getSource() == weightRadio) || (event.getSource() == noneRadio) ||
                       (event.getSource() == voiRadio)) {
            buttonWeightRef.setEnabled(weightRadio.isSelected());
            buttonWeightInput.setEnabled(weightRadio.isSelected());

            if (weightRadio.isSelected()) {
                comboBoxDOF.setSelectedIndex(4);
            } // if (weightRadio.isSelected())
        } // else if ((event.getSource() == weightRadio) || (event.getSource() == noneRadio) ||

        // (event.getSource() == voiRadio))
        else if (event.getSource() == comboBoxDOF) {

        	if (comboBoxDOF.getSelectedIndex() == 0) { // rotation only
                rotateBeginText.setEnabled(true);
                rotateEndText.setEnabled(true);
                coarseRateText.setEnabled(true);
                fineRateText.setEnabled(false);
                labelRotateRange.setEnabled(true);
                labelRotateRangeTo.setEnabled(true);
                labelRotateDegrees.setEnabled(true);
                labelCoarse.setEnabled(true);
                labelCoarseDegrees.setEnabled(true);
                labelFine.setEnabled(false);
                labelFineDegrees.setEnabled(false);
                sampleCheckBox.setSelected(false);
                sampleCheckBox.setEnabled(false);
            } else if (comboBoxDOF.getSelectedIndex() == 1) {
                rotateBeginText.setEnabled(false);
                rotateEndText.setEnabled(false);
                coarseRateText.setEnabled(false);
                fineRateText.setEnabled(false);
                labelRotateRange.setEnabled(false);
                labelRotateRangeTo.setEnabled(false);
                labelRotateDegrees.setEnabled(false);
                labelCoarse.setEnabled(false);
                labelCoarseDegrees.setEnabled(false);
                labelFine.setEnabled(false);
                labelFineDegrees.setEnabled(false);
                sampleCheckBox.setEnabled(false);
            } else {
                rotateBeginText.setEnabled(true);
                rotateEndText.setEnabled(true);
                coarseRateText.setEnabled(true);
                fineRateText.setEnabled(true);
                labelRotateRange.setEnabled(true);
                labelRotateRangeTo.setEnabled(true);
                labelRotateDegrees.setEnabled(true);
                labelCoarse.setEnabled(true);
                labelCoarseDegrees.setEnabled(true);
                labelFine.setEnabled(true);
                labelFineDegrees.setEnabled(true);
                sampleCheckBox.setEnabled(true);
            }
        } else if (event.getSource() == comboBoxSearchAlgo) {
        	switch(comboBoxSearchAlgo.getSelectedIndex()) {
        	case 0: // Powell's calling Brent's
        		jtemCheckBox.setEnabled(true);
        		break;
        	case 1: // ELSUNC
        		jtemCheckBox.setEnabled(false);
        		break;
        	case 2: // NL2SOL
        		jtemCheckBox.setEnabled(false);
        		break;
        	case 3: // LEVENBERG_MARQUARDT
        		jtemCheckBox.setEnabled(false);
        		break;
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
     * Accessor to set the coarse sample begin.
     *
     * @param  x  Coarse begin
     */
    public void setCoarseBegin(float x) {
        rotateBegin = x;
    }

    /**
     * Accessor to set the coarse sample end.
     *
     * @param  x  Coarse end
     */
    public void setCoarseEnd(float x) {
        rotateEnd = x;
    }

    /**
     * Accessor to set the coarse sample rate.
     *
     * @param  x  Coarse rate
     */
    public void setCoarseRate(float x) {
        coarseRate = x;
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
     * Accessor to set the fine sample rate.
     *
     * @param  x  Fine rate
     */
    public void setFineRate(float x) {
        fineRate = x;
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
     * 
     * @param searchAlgorithm
     */
    public void setSearchAlgorithm(int searchAlgorithm) {
    	this.searchAlgorithm = searchAlgorithm;
    }
    
    /**
     * Accessor to set whether or not powell's algorithm uses multithreading
     * @param doMultiThread
     */
    public void setMultiThread(boolean doMultiThread) {
    	this.doMultiThread = doMultiThread;
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

        if (voisOnly) {
            float[] refRes = new float[] {
                                 refImage.getFileInfo(0).getResolutions()[0],
                                 refImage.getFileInfo(0).getResolutions()[1]
                             };
            float[] matchRes = new float[] {
                                   matchImage.getFileInfo(0).getResolutions()[0],
                                   matchImage.getFileInfo(0).getResolutions()[1]
                               };

            refWeightImage = new ModelImage(ModelStorageBase.BYTE, refImage.getExtents(), "VOI ref");
            inputWeightImage = new ModelImage(ModelStorageBase.BYTE, matchImage.getExtents(), "VOI match");

            refWeightImage.getFileInfo(0).setResolutions(refRes);
            inputWeightImage.getFileInfo(0).setResolutions(matchRes);

            // make new reference and input images based on the VOIs in them.
            // pass those new images to the registration algorithm
            BitSet mask = refImage.generateVOIMask();
            int imageSize = refImage.getSliceSize();

            for (int i = 0; i < imageSize; i++) {

                if (!mask.get(i)) {
                    refWeightImage.set(i, 0);
                } else {
                    refWeightImage.set(i, 1);
                }
            }

            mask = matchImage.generateVOIMask();
            imageSize = matchImage.getSliceSize();

            for (int i = 0; i < imageSize; i++) {

                if (!mask.get(i)) {
                    inputWeightImage.set(i, 0);
                } else {
                    inputWeightImage.set(i, 1);
                }
            }

            weighted = true;
        } // if (voisOnly)

        if (searchAlgorithm != POWELL) {
        	if (weighted) {
	            reg2E = new AlgorithmRegELSUNCOAR2D(refImage, matchImage, refWeightImage, inputWeightImage, cost, DOF, interp,
	                                         rotateBegin, rotateEnd, coarseRate, fineRate, doSubsample, doMultiThread,
	                                         maxIterations, numMinima, searchAlgorithm);
	        } else {
	            reg2E = new AlgorithmRegELSUNCOAR2D(refImage, matchImage, cost, DOF, interp, rotateBegin, rotateEnd, coarseRate,
	                                         fineRate, doSubsample, doMultiThread,
	                                         maxIterations, numMinima, searchAlgorithm);
	        }
	
	        // Hide dialog
	        setVisible(false);
	
	        // Start the thread as a low priority because we wish to still have user interface work fast.
	        reg2E.addListener(this);
	
	        createProgressBar(matchImage.getImageName(), reg2E);
	
	        if (isRunInSeparateThread()) {
	
	            // Start the thread as a low priority because we wish to still have user interface work fast.
	            if (reg2E.startMethod(Thread.MIN_PRIORITY) == false) {
	                MipavUtil.displayError("A thread is already running on this object");
	            }
	        } else {
	            reg2E.run();
	        }	
        }
        else {
	        if (weighted) {
	            reg2 = new AlgorithmRegOAR2D(refImage, matchImage, refWeightImage, inputWeightImage, cost, DOF, interp,
	                                         rotateBegin, rotateEnd, coarseRate, fineRate, doSubsample, doMultiThread,
	                                         maxIterations, numMinima);
	        } else {
	            reg2 = new AlgorithmRegOAR2D(refImage, matchImage, cost, DOF, interp, rotateBegin, rotateEnd, coarseRate,
	                                         fineRate, doSubsample, doMultiThread,
	                                         maxIterations, numMinima);
	            reg2.setJTEM(doJTEM);
	        }
	
	        // Hide dialog
	        setVisible(false);
	
	        // Start the thread as a low priority because we wish to still have user interface work fast.
	        reg2.addListener(this);
	
	        createProgressBar(matchImage.getImageName(), reg2);
	
	        if (isRunInSeparateThread()) {
	
	            // Start the thread as a low priority because we wish to still have user interface work fast.
	            if (reg2.startMethod(Thread.MIN_PRIORITY) == false) {
	                MipavUtil.displayError("A thread is already running on this object");
	            }
	        } else {
	            reg2.run();
	        }
        }
    }

    /**
     * Store the result image in the script runner's image table now that the action execution is finished.
     */
    protected void doPostAlgorithmActions() {
        if (getResultImage() != null) {
            AlgorithmParameters.storeImageInRunner(getResultImage());
        }
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

        setReferenceImage(scriptParameters.retrieveInputImage(2));

        setWeighted(scriptParameters.getParams().getBoolean("do_use_weight_images"));

        if (weighted) {
            setInputWeightImage(scriptParameters.retrieveImage("input_weight_image"));
            setReferenceWeightImage(scriptParameters.retrieveImage("reference_weight_image"));
        }

        setDOF(scriptParameters.getParams().getInt("degrees_of_freedom"));
        setInterp(scriptParameters.getParams().getInt("initial_interpolation_type"));
        setCostChoice(scriptParameters.getParams().getInt("cost_function_type"));
        setSearchAlgorithm(scriptParameters.getParams().getInt("search_algorithm"));

        setCoarseBegin(scriptParameters.getParams().getFloat("rotate_begin"));
        setCoarseEnd(scriptParameters.getParams().getFloat("rotate_end"));
        setCoarseRate(scriptParameters.getParams().getFloat("coarse_rate"));
        setFineRate(scriptParameters.getParams().getFloat("fine_rate"));

        setDisplayTransform(scriptParameters.getParams().getBoolean("do_display_transform"));
        setInterp2(scriptParameters.getParams().getInt("final_interpolation_type"));

        setSubsample(scriptParameters.getParams().getBoolean("do_subsample"));
        setMultiThread(scriptParameters.getParams().getBoolean("do_multi_thread"));
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
    }

    /**
     * {@inheritDoc}
     */
    protected void storeParamsFromGUI() throws ParserException {
        scriptParameters.storeInputImage(matchImage);
        scriptParameters.storeInputImage(refImage);

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
        scriptParameters.getParams().put(ParameterFactory.newParameter("search_algorithm", searchAlgorithm));
        scriptParameters.getParams().put(ParameterFactory.newParameter("rotate_begin", rotateBegin));
        scriptParameters.getParams().put(ParameterFactory.newParameter("rotate_end", rotateEnd));
        scriptParameters.getParams().put(ParameterFactory.newParameter("coarse_rate", coarseRate));
        scriptParameters.getParams().put(ParameterFactory.newParameter("fine_rate", fineRate));
        scriptParameters.getParams().put(ParameterFactory.newParameter("do_display_transform", displayTransform));
        scriptParameters.getParams().put(ParameterFactory.newParameter("do_subsample", doSubsample));
        scriptParameters.getParams().put(ParameterFactory.newParameter("do_multi_thread", doMultiThread));
        scriptParameters.getParams().put(ParameterFactory.newParameter("out_of_bounds_index", outOfBoundsIndex));
        scriptParameters.getParams().put(ParameterFactory.newParameter("fill_value", fillValue));
        scriptParameters.getParams().put(ParameterFactory.newParameter("matrix_directory", matrixDirectory));
    }

    /**
     * Build advanced settings dialog. Returns JDialog.
     *
     * @param   maxIter       DOCUMENT ME!
     * @param   numMinima     DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    private JDialog buildAdvancedDialog(int maxIter, int numMinima) {
        serif12 = MipavUtil.font12;
        serif12B = MipavUtil.font12B;

        advancedDialog = new JDialog(this, "Advanced OAR settings", false);
        // Parent is the JDialogRegistrationOAR3D, title, modal
        // Dialog changed to non-modal after adding Help button 12/17/07

        // Setting panel
        JPanel settingsPanel = new JPanel();
        settingsPanel.setBorder(BorderFactory.createTitledBorder("Optimization settings"));
        settingsPanel.setLayout(new BoxLayout(settingsPanel, BoxLayout.Y_AXIS));

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

        settingsPanel.add(maxIterPanel);
        settingsPanel.add(Box.createVerticalStrut(20));
        settingsPanel.add(numMinPanel);

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
     * Builds the bruteForceDialog so the user can set the brute-force registration parameters:
     *
     * @return  bruteForceDialog
     */
    private JDialog buildBruteForceDialog() {
        serif12 = MipavUtil.font12;
        serif12B = MipavUtil.font12B;

        bruteForceDialog = new JDialog(this, "Brute Force Settings", true);

        // Setting panel
        JPanel settingsPanel = new JPanel();
        settingsPanel.setBorder(BorderFactory.createTitledBorder("Registration settings"));
        settingsPanel.setLayout(new BoxLayout(settingsPanel, BoxLayout.Y_AXIS));

        JPanel rotationPanel = new JPanel();
        rotationPanel.setLayout(new BorderLayout(1, 3));
        rotationPanel.setBorder(BorderFactory.createEmptyBorder(0, 10, 0, 10));

        JLabel rotationBoundLabel = new JLabel("Rotation Range (+/-) angle: ", JLabel.LEFT);
        rotationPanel.add(rotationBoundLabel, BorderLayout.WEST);
        rotationText = new JTextField(String.valueOf(rotationBF), 5);
        rotationText.addFocusListener(this);
        rotationPanel.add(rotationText, BorderLayout.CENTER);

        JLabel rotationInstruct = new JLabel("Value may be zero for no rotation optimization.", JLabel.RIGHT);
        rotationPanel.add(rotationInstruct, BorderLayout.SOUTH);

        JPanel translationPanel = new JPanel();
        translationPanel.setLayout(new BorderLayout(1, 3));
        translationPanel.setBorder(BorderFactory.createEmptyBorder(0, 10, 0, 10));

        JLabel translationBoundLabel = new JLabel("Translation Range (+/-) pixels: ", JLabel.LEFT);
        translationPanel.add(translationBoundLabel, BorderLayout.WEST);
        translationText = new JTextField(String.valueOf(translationBF), 5);
        translationText.addFocusListener(this);
        translationPanel.add(translationText, BorderLayout.CENTER);

        JLabel translationInstruct = new JLabel("Value may be zero for no translation optimization.", JLabel.RIGHT);
        translationPanel.add(translationInstruct, BorderLayout.SOUTH);

        JPanel xscalePanel = new JPanel();
        xscalePanel.setLayout(new BorderLayout(1, 3));
        xscalePanel.setBorder(BorderFactory.createEmptyBorder(0, 10, 0, 10));

        JLabel xscaleBoundLabel = new JLabel("Scale in X (+/-) %: ", JLabel.LEFT);
        xscalePanel.add(xscaleBoundLabel, BorderLayout.WEST);
        xscaleText = new JTextField(String.valueOf(xscaleBF), 5);
        xscaleText.addFocusListener(this);
        xscalePanel.add(xscaleText, BorderLayout.CENTER);

        JLabel xscaleInstruct = new JLabel("Value may be zero for no scale in X optimization.", JLabel.RIGHT);
        xscalePanel.add(xscaleInstruct, BorderLayout.SOUTH);

        JPanel yscalePanel = new JPanel();
        yscalePanel.setLayout(new BorderLayout(1, 3));
        yscalePanel.setBorder(BorderFactory.createEmptyBorder(0, 10, 0, 10));

        JLabel yscaleBoundLabel = new JLabel("Scale in Y (+/-) %: ", JLabel.LEFT);
        yscalePanel.add(yscaleBoundLabel, BorderLayout.WEST);
        yscaleText = new JTextField(String.valueOf(yscaleBF), 5);
        yscaleText.addFocusListener(this);
        yscalePanel.add(yscaleText, BorderLayout.CENTER);

        JLabel yscaleInstruct = new JLabel("Value may be zero for no scale in Y optimization.", JLabel.RIGHT);
        yscalePanel.add(yscaleInstruct, BorderLayout.SOUTH);

        JPanel scaleStepsPanel = new JPanel();
        scaleStepsPanel.setLayout(new BorderLayout(1, 3));
        scaleStepsPanel.setBorder(BorderFactory.createEmptyBorder(0, 10, 0, 10));

        JLabel scaleStepsBoundLabel = new JLabel("Number of steps for testing scales: ", JLabel.LEFT);
        scaleStepsPanel.add(scaleStepsBoundLabel, BorderLayout.WEST);
        scaleStepsText = new JTextField(String.valueOf(scaleStepsBF), 5);
        scaleStepsText.addFocusListener(this);
        scaleStepsPanel.add(scaleStepsText, BorderLayout.CENTER);

        JLabel scaleStepsInstruct = new JLabel("Value may be zero for no scale in optimization.", JLabel.RIGHT);
        scaleStepsPanel.add(scaleStepsInstruct, BorderLayout.SOUTH);

        settingsPanel.add(rotationPanel);
        settingsPanel.add(Box.createVerticalStrut(20));
        settingsPanel.add(translationPanel);
        settingsPanel.add(Box.createVerticalStrut(20));
        settingsPanel.add(xscalePanel);
        settingsPanel.add(Box.createVerticalStrut(20));
        settingsPanel.add(yscalePanel);
        settingsPanel.add(Box.createVerticalStrut(20));
        settingsPanel.add(scaleStepsPanel);
        settingsPanel.add(Box.createVerticalStrut(20));

        bruteForceDialog.getContentPane().add(settingsPanel, BorderLayout.NORTH);

        // Okay-Cancel Panel
        JPanel okayCancelPanel = new JPanel(new FlowLayout());
        JButton bruteCancelButton = new JButton("Cancel");
        bruteCancelButton.setActionCommand("BruteForceCancel");
        bruteCancelButton.addActionListener(this);
        bruteCancelButton.setPreferredSize(new Dimension(120, 30));
        bruteCancelButton.setFont(serif12B);

        JButton okayButton = new JButton("OK");
        okayButton.setActionCommand("BruteForceOkay");
        okayButton.addActionListener(this);
        okayButton.setPreferredSize(new Dimension(120, 30));
        okayButton.setFont(serif12B);

        okayCancelPanel.add(okayButton);
        okayCancelPanel.add(bruteCancelButton);

        bruteForceDialog.getContentPane().add(okayCancelPanel, BorderLayout.SOUTH);

        Rectangle dialogBounds = this.getBounds();
        bruteForceDialog.setLocation((int) ((Toolkit.getDefaultToolkit().getScreenSize().width * 0.75) -
                                            (dialogBounds.width / 2)),
                                     (Toolkit.getDefaultToolkit().getScreenSize().height / 2) -
                                     (dialogBounds.height / 2));

        bruteForceDialog.pack();
        bruteForceDialog.setVisible(true);

        return bruteForceDialog;
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
            String name = names.nextElement();

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
     * Initializes the GUI components and displays the dialog.
     */
    private void init() {
        matchImage.calcMinMax();
        imageMin = matchImage.getMin();
        imageMax = matchImage.getMax();
        dataType = matchImage.getFileInfo()[0].getDataType();
        setForeground(Color.black);
        setTitle("Optimized Automatic Image Registration 2D");

        JPanel optPanel = new JPanel();
        optPanel.setLayout(new GridBagLayout());
        optPanel.setBorder(buildTitledBorder("Input Options"));

        JLabel labelImage;
        String matchName = matchImage.getImageName();
        labelImage = new JLabel("Register [" + matchName + "] to:");
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
        comboBoxDOF.addItem("Rotation only - 1");
        comboBoxDOF.addItem("Translations - 2");
        comboBoxDOF.addItem("Rigid - 3");
        comboBoxDOF.addItem("Global rescale - 4");
        comboBoxDOF.addItem("Specific rescale - 5");
        comboBoxDOF.addItem("Affine - 6");
        comboBoxDOF.setSelectedIndex(5);
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

        comboBoxCostFunct.addItem("Least squares");

        if (!doColor) {
            comboBoxCostFunct.addItem("Normalized cross correlation");
        }

        if (!doColor) {
            comboBoxCostFunct.addItem("Normalized mutual information");
            comboBoxCostFunct.addItem("Normalized mutual information - GPU");
            //comboBoxCostFunct.addItem("Normalized mutual information - GPU2");
        }

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
        comboBoxSearchAlgo.addItem("NL2SOL");
        comboBoxSearchAlgo.addItem("Levenberg-Marquardt");
        comboBoxSearchAlgo.addItemListener(this);
        comboBoxSearchAlgo.setSelectedIndex(0);

        JLabel labelInterp = new JLabel("Interpolation:");
        labelInterp.setForeground(Color.black);
        labelInterp.setFont(serif12);
        labelInterp.setAlignmentX(Component.LEFT_ALIGNMENT);

        comboBoxInterp = new JComboBox();
        comboBoxInterp.setFont(serif12);
        comboBoxInterp.setBackground(Color.white);
        comboBoxInterp.setAlignmentX(Component.LEFT_ALIGNMENT);

        comboBoxInterp.addItem("Bilinear");
        comboBoxInterp.addItem("Bspline 3rd order");
        comboBoxInterp.addItem("Bspline 4th order");
        comboBoxInterp.addItem("Cubic Lagrangian");
        comboBoxInterp.addItem("Quintic Lagrangian");
        comboBoxInterp.addItem("Heptic Lagrangian");
        comboBoxInterp.addItem("Windowed sinc");
        // comboBoxInterp.addItem("Nearest Neighbor");

        // Rotation Range Panel
        JPanel rotateRangePanel = new JPanel(new FlowLayout(FlowLayout.LEFT));

        labelRotateRange = new JLabel("Rotation angle sampling range:");
        labelRotateRange.setForeground(Color.black);
        labelRotateRange.setFont(serif12);
        labelRotateRangeTo = new JLabel("to");
        labelRotateRangeTo.setForeground(Color.black);
        labelRotateRangeTo.setFont(serif12);
        labelRotateDegrees = new JLabel("degrees");
        labelRotateDegrees.setFont(serif12);

        rotateBeginText = new JTextField("-30", 3);
        rotateEndText = new JTextField("30", 3);

        rotateRangePanel.add(labelRotateRange);
        rotateRangePanel.add(rotateBeginText);
        rotateRangePanel.add(labelRotateRangeTo);
        rotateRangePanel.add(rotateEndText);
        rotateRangePanel.add(labelRotateDegrees);

        // Coarse sampling rate panel
        JPanel coarsePanel = new JPanel(new FlowLayout(FlowLayout.LEFT));

        labelCoarse = new JLabel("Coarse angle increment: ");
        labelCoarse.setForeground(Color.black);
        labelCoarse.setFont(serif12);
        labelCoarse.setAlignmentX(Component.LEFT_ALIGNMENT);
        labelCoarseDegrees = new JLabel("degrees");
        labelCoarseDegrees.setFont(serif12);
        coarseRateText = new JTextField("15", 3);

        coarsePanel.add(labelCoarse);
        coarsePanel.add(coarseRateText);
        coarsePanel.add(labelCoarseDegrees);
        coarsePanel.setAlignmentX(Component.LEFT_ALIGNMENT);

        // Fine sampling rate panel
        JPanel finePanel = new JPanel(new FlowLayout(FlowLayout.LEFT));

        labelFine = new JLabel("Fine angle increment:");
        labelFine.setForeground(Color.black);
        labelFine.setFont(serif12);
        labelFine.setAlignmentX(Component.LEFT_ALIGNMENT);
        labelFineDegrees = new JLabel("degrees");
        labelFineDegrees.setFont(serif12);
        fineRateText = new JTextField("6", 3);

        finePanel.add(labelFine);
        finePanel.add(fineRateText);
        finePanel.add(labelFineDegrees);
        finePanel.setAlignmentX(Component.LEFT_ALIGNMENT);

        sampleCheckBox = new JCheckBox("Subsample image for speed");
        sampleCheckBox.setFont(serif12);
        sampleCheckBox.setForeground(Color.black);
        sampleCheckBox.setSelected(true);
        sampleCheckBox.setEnabled(true);

        jtemCheckBox = new JCheckBox("Full Powell's Method");
        jtemCheckBox.setFont(serif12);
        jtemCheckBox.setForeground(Color.black);
        jtemCheckBox.setSelected(false);
        jtemCheckBox.setEnabled(true);
        
        multiThreadCheckBox = new JCheckBox("Multi-threading enabled");
        multiThreadCheckBox.setFont(serif12);
        multiThreadCheckBox.setForeground(Color.black);
        multiThreadCheckBox.setSelected(Preferences.isMultiThreadingEnabled()  &&
        		(ThreadUtil.getAvailableCores() > 1));
        multiThreadCheckBox.setEnabled(ThreadUtil.getAvailableCores() > 1);

        Insets insets = new Insets(0, 2, 0, 2);
        GridBagConstraints gbc = new GridBagConstraints();
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
        gbc.gridwidth = GridBagConstraints.REMAINDER;
        optPanel.add(rotateRangePanel, gbc);

        gbc.gridx = 0;
        gbc.gridy = 7;
        gbc.gridwidth = GridBagConstraints.REMAINDER;
        optPanel.add(coarsePanel, gbc);

        gbc.gridx = 0;
        gbc.gridy = 8;
        gbc.gridwidth = GridBagConstraints.REMAINDER;
        optPanel.add(finePanel, gbc);

        gbc.gridx = 0;
        gbc.gridy = 9;
        gbc.weightx = 1;
        gbc.fill = GridBagConstraints.HORIZONTAL;
        optPanel.add(sampleCheckBox, gbc);
        
        gbc.gridx = 0;
        gbc.gridy = 10;
        gbc.weightx = 1;
        gbc.fill = GridBagConstraints.HORIZONTAL;
        optPanel.add(jtemCheckBox, gbc);
        
        gbc.gridx = 0;
        gbc.gridy = 11;
        gbc.weightx = 1;
        gbc.fill = GridBagConstraints.HORIZONTAL;
        optPanel.add(multiThreadCheckBox, gbc);

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

        comboBoxInterp2.addItem("Bilinear");
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
        weightPanel.setAlignmentX(Component.LEFT_ALIGNMENT);
        outPanel.setAlignmentX(Component.LEFT_ALIGNMENT);
        mainPanel.add(optPanel);
        mainPanel.add(weightPanel);
        mainPanel.add(outPanel);

        getContentPane().add(mainPanel);
        getContentPane().add(buttonPanel, BorderLayout.SOUTH);
        pack();
        setVisible(true);
    }


    /**
     * Initializes the GUI components, based on the preset values set by the class calling the constructor. Then
     * displays the dialog.
     */
    private void initPresets() {
        setForeground(Color.black);
        setTitle("Optimized Automatic Image Registration 2D");

        JPanel optPanel = new JPanel();
        optPanel.setLayout(new GridBagLayout());
        optPanel.setBorder(buildTitledBorder("Input Options"));

        JLabel labelDOF = new JLabel("Degrees of freedom:");
        labelDOF.setForeground(Color.black);
        labelDOF.setFont(serif12);
        labelDOF.setAlignmentX(Component.LEFT_ALIGNMENT);

        comboBoxDOF = new JComboBox();
        comboBoxDOF.setFont(MipavUtil.font12);
        comboBoxDOF.setBackground(Color.white);
        comboBoxDOF.setToolTipText("Degrees of freedom");
        comboBoxDOF.addItem("Rotation only - 1");
        comboBoxDOF.addItem("Translations - 2");
        comboBoxDOF.addItem("Rigid - 3");
        comboBoxDOF.addItem("Global rescale - 4");
        comboBoxDOF.addItem("Specific rescale - 5");
        comboBoxDOF.addItem("Affine - 6");
        comboBoxDOF.setSelectedIndex(DOF - 1);
        comboBoxDOF.addItemListener(this);

        JLabel labelCost = new JLabel("Cost function:");
        labelCost.setForeground(Color.black);
        labelCost.setFont(serif12);
        labelCost.setAlignmentX(Component.LEFT_ALIGNMENT);

        /* If the image is a color image then we can choose to process it as a
         * color image or not: */
        if (doColor) {
            doColorCheckBox = new JCheckBox("Process Color Image");
            doColorCheckBox.addActionListener(this);
            doColorCheckBox.setActionCommand("CheckDoColor");
            doColorCheckBox.setFont(serif12);
            doColorCheckBox.setForeground(Color.black);
            doColorCheckBox.setSelected(doColor);
            doColorCheckBox.setEnabled(true);
        }

        comboBoxCostFunct = new JComboBox();
        comboBoxCostFunct.setFont(MipavUtil.font12);
        comboBoxCostFunct.setBackground(Color.white);
        comboBoxCostFunct.setToolTipText("Cost function");

        if (!doColor) {
            comboBoxCostFunct.addItem("Correlation ratio");
        }

        comboBoxCostFunct.addItem("Least squares");

        if (!doColor) {
            comboBoxCostFunct.addItem("Normalized cross correlation");
            comboBoxCostFunct.addItem("Normalized mutual information");
        }

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
        comboBoxSearchAlgo.addItem("NL2SOL");
        comboBoxSearchAlgo.addItem("Levenberg-Marquardt");
        comboBoxSearchAlgo.setSelectedIndex(0);

        JLabel labelInterp = new JLabel("Interpolation:");
        labelInterp.setForeground(Color.black);
        labelInterp.setFont(serif12);
        labelInterp.setAlignmentX(Component.LEFT_ALIGNMENT);

        comboBoxInterp = new JComboBox();
        comboBoxInterp.setFont(serif12);
        comboBoxInterp.setBackground(Color.white);
        comboBoxInterp.setAlignmentX(Component.LEFT_ALIGNMENT);

        comboBoxInterp.addItem("Bilinear");
        comboBoxInterp.addItem("Nearest Neighbor");
        comboBoxInterp.addItem("Bspline 3rd order");
        comboBoxInterp.addItem("Bspline 4th order");
        comboBoxInterp.addItem("Cubic Lagrangian");
        comboBoxInterp.addItem("Quintic Lagrangian");
        comboBoxInterp.addItem("Heptic Lagrangian");
        comboBoxInterp.addItem("Windowed sinc");
        comboBoxInterp.setSelectedIndex(interp - 1);


        // Rotation Range Panel
        JPanel rotateRangePanel = new JPanel(new FlowLayout(FlowLayout.LEFT));

        labelRotateRange = new JLabel("Rotation angle sampling range:");
        labelRotateRange.setForeground(Color.black);
        labelRotateRange.setFont(serif12);
        labelRotateRangeTo = new JLabel("to");
        labelRotateRangeTo.setForeground(Color.black);
        labelRotateRangeTo.setFont(serif12);
        labelRotateDegrees = new JLabel("degrees");
        labelRotateDegrees.setFont(serif12);

        rotateBeginText = new JTextField(new String("" + rotateBegin), 3);
        rotateEndText = new JTextField(new String("" + rotateEnd), 3);

        rotateRangePanel.add(labelRotateRange);
        rotateRangePanel.add(rotateBeginText);
        rotateRangePanel.add(labelRotateRangeTo);
        rotateRangePanel.add(rotateEndText);
        rotateRangePanel.add(labelRotateDegrees);

        // Coarse sampling rate panel
        JPanel coarsePanel = new JPanel(new FlowLayout(FlowLayout.LEFT));

        labelCoarse = new JLabel("Coarse angle increment: ");
        labelCoarse.setForeground(Color.black);
        labelCoarse.setFont(serif12);
        labelCoarse.setAlignmentX(Component.LEFT_ALIGNMENT);
        labelCoarseDegrees = new JLabel("degrees");
        labelCoarseDegrees.setFont(serif12);
        coarseRateText = new JTextField(new String("" + coarseRate), 3);

        coarsePanel.add(labelCoarse);
        coarsePanel.add(coarseRateText);
        coarsePanel.add(labelCoarseDegrees);
        coarsePanel.setAlignmentX(Component.LEFT_ALIGNMENT);

        // Fine sampling rate panel
        JPanel finePanel = new JPanel(new FlowLayout(FlowLayout.LEFT));

        labelFine = new JLabel("Fine angle increment:");
        labelFine.setForeground(Color.black);
        labelFine.setFont(serif12);
        labelFine.setAlignmentX(Component.LEFT_ALIGNMENT);
        labelFineDegrees = new JLabel("degrees");
        labelFineDegrees.setFont(serif12);
        fineRateText = new JTextField(new String("" + fineRate), 3);

        finePanel.add(labelFine);
        finePanel.add(fineRateText);
        finePanel.add(labelFineDegrees);
        finePanel.setAlignmentX(Component.LEFT_ALIGNMENT);

        sampleCheckBox = new JCheckBox("Subsample image for speed");
        sampleCheckBox.setFont(serif12);
        sampleCheckBox.setForeground(Color.black);
        sampleCheckBox.setSelected(doSubsample);
        sampleCheckBox.setEnabled(true);

        bruteForceCheckBox = new JCheckBox("Brute Force Registration");
        bruteForceCheckBox.setActionCommand("BruteForceCheck");
        bruteForceCheckBox.addActionListener(this);
        bruteForceCheckBox.setFont(serif12);
        bruteForceCheckBox.setForeground(Color.black);
        bruteForceCheckBox.setSelected(false);
        bruteForceCheckBox.setEnabled(true);

        JButton bruteForceOptionsButton = new JButton("Brute Force Options");
        bruteForceOptionsButton.setFont(serif12B);
        bruteForceOptionsButton.setEnabled(true);
        bruteForceOptionsButton.setActionCommand("bruteForceOptions");
        bruteForceOptionsButton.addActionListener(this);

        GridBagConstraints gbc = new GridBagConstraints();

        gbc.gridx = 0;
        gbc.gridy = 0;
        gbc.weightx = 0;
        gbc.fill = GridBagConstraints.NONE;
        optPanel.add(labelDOF, gbc);
        gbc.gridx = 1;
        gbc.gridy = 0;
        gbc.weightx = 1;
        gbc.fill = GridBagConstraints.HORIZONTAL;
        optPanel.add(comboBoxDOF, gbc);

        gbc.gridx = 0;
        gbc.gridy++;
        gbc.weightx = 0;
        gbc.fill = GridBagConstraints.NONE;
        optPanel.add(labelInterp, gbc);
        gbc.gridx = 1;
        gbc.weightx = 1;
        gbc.fill = GridBagConstraints.HORIZONTAL;
        optPanel.add(comboBoxInterp, gbc);

        if (doColor) {
            gbc.gridx = 0;
            gbc.gridy++;
            gbc.weightx = 0;
            gbc.fill = GridBagConstraints.NONE;
            optPanel.add(doColorCheckBox, gbc);
        }

        gbc.gridx = 0;
        gbc.gridy++;
        gbc.weightx = 0;
        gbc.fill = GridBagConstraints.NONE;
        optPanel.add(labelCost, gbc);
        gbc.gridx = 1;
        gbc.weightx = 1;
        gbc.fill = GridBagConstraints.HORIZONTAL;
        optPanel.add(comboBoxCostFunct, gbc);
        
        gbc.gridx = 0;
        gbc.gridy++;
        gbc.weightx = 0;
        gbc.fill = GridBagConstraints.NONE;
        optPanel.add(labelSearch, gbc);
        gbc.gridx = 1;
        gbc.weightx = 1;
        gbc.fill = GridBagConstraints.HORIZONTAL;
        optPanel.add(comboBoxSearchAlgo, gbc);

        gbc.gridx = 0;
        gbc.gridy++;
        gbc.gridwidth = GridBagConstraints.REMAINDER;
        optPanel.add(rotateRangePanel, gbc);

        gbc.gridx = 0;
        gbc.gridy++;
        gbc.gridwidth = GridBagConstraints.REMAINDER;
        optPanel.add(coarsePanel, gbc);

        gbc.gridx = 0;
        gbc.gridy++;
        gbc.gridwidth = GridBagConstraints.REMAINDER;
        optPanel.add(finePanel, gbc);

        gbc.gridx = 0;
        gbc.gridy++;
        gbc.weightx = 1;
        gbc.fill = GridBagConstraints.HORIZONTAL;
        optPanel.add(sampleCheckBox, gbc);

        gbc.gridx = 0;
        gbc.gridy++;
        gbc.weightx = 0;
        gbc.anchor = GridBagConstraints.WEST;
        gbc.fill = GridBagConstraints.NONE;
        optPanel.add(bruteForceCheckBox, gbc);
        gbc.gridx = 1;
        gbc.weightx = 0;
        gbc.anchor = GridBagConstraints.EAST;
        gbc.fill = GridBagConstraints.NONE;
        optPanel.add(bruteForceOptionsButton, gbc);

        JPanel outPanel = new JPanel();
        outPanel.setLayout(new GridBagLayout());
        outPanel.setBorder(buildTitledBorder("Output Options"));

        transformCheckbox = new JCheckBox("Display transformed image");
        transformCheckbox.setFont(serif12);
        transformCheckbox.setForeground(Color.black);
        transformCheckbox.setSelected(displayTransform);
        transformCheckbox.addItemListener(this);

        labelInterp2 = new JLabel("Interpolation:");
        labelInterp2.setForeground(Color.black);
        labelInterp2.setFont(serif12);
        labelInterp2.setAlignmentX(Component.LEFT_ALIGNMENT);

        comboBoxInterp2 = new JComboBox();
        comboBoxInterp2.setFont(serif12);
        comboBoxInterp2.setBackground(Color.white);
        comboBoxInterp2.setAlignmentX(Component.LEFT_ALIGNMENT);

        comboBoxInterp2.addItem("Bilinear");
        comboBoxInterp2.addItem("Nearest Neighbor");
        comboBoxInterp2.addItem("Bspline 3rd order");
        comboBoxInterp2.addItem("Bspline 4th order");
        comboBoxInterp2.addItem("Cubic Lagrangian");
        comboBoxInterp2.addItem("Quintic Lagrangian");
        comboBoxInterp2.addItem("Heptic Lagrangian");
        comboBoxInterp2.addItem("Windowed sinc");
        comboBoxInterp2.setSelectedIndex(interp2 - 1);

        gbc.gridx = 0;
        gbc.gridy = 0;
        gbc.weightx = 0;
        gbc.fill = GridBagConstraints.HORIZONTAL;
        outPanel.add(transformCheckbox, gbc);
        gbc.gridx = 0;
        gbc.gridy = 1;
        gbc.weightx = 0;
        gbc.fill = GridBagConstraints.HORIZONTAL;
        outPanel.add(labelInterp2, gbc);
        gbc.gridx = 1;
        gbc.gridy = 2;
        gbc.weightx = 1;
        gbc.fill = GridBagConstraints.HORIZONTAL;
        outPanel.add(comboBoxInterp2, gbc);

        JPanel buttonPanel = new JPanel();
        JButton setParamsButton = new JButton("Set Parameters");
        setParamsButton.setActionCommand("setParams");
        setParamsButton.addActionListener(this);
        setParamsButton.setFont(serif12B);
        buttonPanel.add(setParamsButton);

        JButton setAndRegisterButton = new JButton("Set Parameters and Register Images");
        setAndRegisterButton.setActionCommand("setAndRegister");
        setAndRegisterButton.addActionListener(this);
        setAndRegisterButton.setFont(serif12B);
        buttonPanel.add(setAndRegisterButton);

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
        outPanel.setAlignmentX(Component.LEFT_ALIGNMENT);
        mainPanel.add(optPanel);
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

        if (m_bSubsetDialog == false) {
            refImage = UI.getRegisteredImageByName((String) comboBoxImage.getSelectedItem());
            weighted = weightRadio.isSelected();
            voisOnly = voiRadio.isSelected();
        }

        if (weighted && (m_bSubsetDialog == false)) {
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

                for (int i = 0; i < refImage.getNDims(); i++) {

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

                for (int i = 0; i < matchImage.getNDims(); i++) {

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
                        break;
                }
            } else {

                switch (comboBoxCostFunct.getSelectedIndex()) {

                    case 0:
                        cost = AlgorithmCostFunctions.LEAST_SQUARES_SMOOTHED_WGT_COLOR;
                        break;
                }
            }
        } // if (doColor)
        else { // black and white

            if ((!weighted) && (!voisOnly)) {

                switch (comboBoxCostFunct.getSelectedIndex()) {

                    case 0:
                        cost = AlgorithmCostFunctions.CORRELATION_RATIO_SMOOTHED;
                        break;

                    case 1:
                        cost = AlgorithmCostFunctions.LEAST_SQUARES_SMOOTHED;
                        break;
                        // case 2:  cost = AlgorithmCostFunctions.MUTUAL_INFORMATION_SMOOTHED;             break;

                    case 2:
                        cost = AlgorithmCostFunctions.NORMALIZED_XCORRELATION_SMOOTHED;
                        break;

                    case 3:
                        cost = AlgorithmCostFunctions.NORMALIZED_MUTUAL_INFORMATION_SMOOTHED;
                        break;

                    case 4:
                        cost = AlgorithmCostFunctions2D.NORMALIZED_MUTUAL_INFORMATION_GPU;
                        break;
                    case 5:
                        cost = AlgorithmCostFunctions2D.NORMALIZED_MUTUAL_INFORMATION_GPU_LM;
                        break;

                    default:
                        cost = AlgorithmCostFunctions.CORRELATION_RATIO_SMOOTHED;
                        break;
                }
            } else {

                switch (comboBoxCostFunct.getSelectedIndex()) {

                    case 0:
                        cost = AlgorithmCostFunctions.CORRELATION_RATIO_SMOOTHED_WGT;
                        break;

                    case 1:
                        cost = AlgorithmCostFunctions.LEAST_SQUARES_SMOOTHED_WGT;
                        break;
                        // case 2:  cost = AlgorithmCostFunctions.MUTUAL_INFORMATION_SMOOTHED_WGT;           break;

                    case 2:
                        cost = AlgorithmCostFunctions.NORMALIZED_XCORRELATION_SMOOTHED_WGT;
                        break;

                    case 3:
                        cost = AlgorithmCostFunctions.NORMALIZED_MUTUAL_INFORMATION_SMOOTHED_WGT;
                        break;

                    default:
                        cost = AlgorithmCostFunctions.CORRELATION_RATIO_SMOOTHED_WGT;
                        break;
                }
            }
        } // else black and white
        
        switch(comboBoxSearchAlgo.getSelectedIndex()) {
        case 0:
        	searchAlgorithm = POWELL;
        	break;
        case 1:
        	searchAlgorithm = ELSUNC;
        	break;
        case 2:
        	searchAlgorithm = NL2SOL;
        	break;
        case 3: 
        	searchAlgorithm = LEVENBERG_MARQUARDT;
        	break;
        default:
        	searchAlgorithm = POWELL;
        }

        switch (comboBoxDOF.getSelectedIndex()) {

            case 0:
                DOF = 1;
                break;

            case 1:
                DOF = 2;
                break;

            case 2:
                DOF = 3;
                break;

            case 3:
                DOF = 4;
                break;

            case 4:
                DOF = 5;
                break;
                
            case 5:
                DOF = 6;
                break;

            default:
                DOF = 6;
                break;
        }

        switch (comboBoxInterp.getSelectedIndex()) {

            case 0:
                interp = AlgorithmTransform.BILINEAR;
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

            default:
                interp = AlgorithmTransform.BILINEAR;
                break;
        }


        switch (comboBoxInterp2.getSelectedIndex()) {

            case 0:
                interp2 = AlgorithmTransform.BILINEAR;
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
                interp2 = AlgorithmTransform.BILINEAR;
                break;
        }

        displayTransform = transformCheckbox.isSelected();

        if (!testParameter(rotateBeginText.getText(), -360, 360)) {
            rotateBeginText.requestFocus();
            rotateBeginText.selectAll();

            return false;
        } else {
            rotateBegin = Float.valueOf(rotateBeginText.getText()).floatValue();
        }

        if (!testParameter(rotateEndText.getText(), -360, 360)) {
            rotateEndText.requestFocus();
            rotateEndText.selectAll();

            return false;
        } else {
            rotateEnd = Float.valueOf(rotateEndText.getText()).floatValue();
        }

        if (!testParameter(coarseRateText.getText(), 0.01, 360)) {
            coarseRateText.requestFocus();
            coarseRateText.selectAll();

            return false;
        } else {
            coarseRate = Float.valueOf(coarseRateText.getText()).floatValue();
        }

        if (rotateBegin > rotateEnd) {
            MipavUtil.displayError("Beginning of range must be less than end of range.");
            rotateBeginText.requestFocus();
            rotateBeginText.selectAll();

            return false;
        }

        if (((rotateEnd - rotateBegin) / coarseRate) < 1) {
            int response = JOptionPane.showConfirmDialog(this,
                                                         "Warning: with such a large rate, there will only be 1 sampling.  Continue?",
                                                         "Sampling warning", JOptionPane.YES_NO_OPTION,
                                                         JOptionPane.WARNING_MESSAGE);

            if (response == JOptionPane.NO_OPTION) {
                coarseRateText.requestFocus();
                coarseRateText.selectAll();

                return false;
            }
        }

        if (!testParameter(rotateBeginText.getText(), -360, 360)) {
            rotateBeginText.requestFocus();
            rotateBeginText.selectAll();

            return false;
        } else {
            rotateBegin = Float.valueOf(rotateBeginText.getText()).floatValue();
        }

        if (!testParameter(rotateEndText.getText(), -360, 360)) {
            rotateEndText.requestFocus();
            rotateEndText.selectAll();

            return false;
        } else {
            rotateEnd = Float.valueOf(rotateEndText.getText()).floatValue();
        }

        if (!testParameter(fineRateText.getText(), 0.01, 360)) {
            fineRateText.requestFocus();
            fineRateText.selectAll();

            return false;
        } else {
            fineRate = Float.valueOf(fineRateText.getText()).floatValue();
        }

        if (rotateBegin > rotateEnd) {
            MipavUtil.displayError("Beginning of range must be less than end of range.");
            rotateBeginText.requestFocus();
            rotateBeginText.selectAll();

            return false;
        }

        if (((rotateEnd - rotateBegin) / fineRate) < 1) {
            int response = JOptionPane.showConfirmDialog(this,
                                                         "Warning: with such a large rate, there will only be 1 sampling.  Continue?",
                                                         "Sampling warning", JOptionPane.YES_NO_OPTION,
                                                         JOptionPane.WARNING_MESSAGE);

            if (response == JOptionPane.NO_OPTION) {
                coarseRateText.requestFocus();
                coarseRateText.selectAll();

                return false;
            }
        }

        doSubsample = sampleCheckBox.isSelected();
        doJTEM = jtemCheckBox.isSelected();
        doMultiThread = multiThreadCheckBox.isSelected();
        
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
     * Return meta-information about this discoverable action for categorization and labeling purposes.
     * 
     * @return Metadata for this action.
     */
    public ActionMetadata getActionMetadata() {
        return new MipavActionMetadata() {
            public String getCategory() {
                return new String("Algorithms.Registration");
            }

            public String getDescription() {
                return new String("Performs a optimized automatic 2d image registration.");
            }

            public String getDescriptionLong() {
                return new String("Performs a optimized automatic 2d image registration.");
            }

            public String getShortLabel() {
                return new String("RegistrationOAR2d");
            }

            public String getLabel() {
                return new String("Optimized automatic 2d image registration");
            }

            public String getName() {
                return new String("Optimized automatic 2d image registration");
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
            table.put(new ParameterExternalImage(AlgorithmParameters.getInputImageLabel(2)));

            table.put(new ParameterBoolean("do_use_weight_images", false));
            final Parameter weight = table.getParameter("do_use_weight_images");
            
            Parameter tempParam = new ParameterExternalImage("input_weight_image");
            tempParam.setParentCondition(weight, "true");
            table.put(tempParam);
            
            tempParam = new ParameterExternalImage("reference_weight_image");
            tempParam.setParentCondition(weight, "true");
            table.put(tempParam);

            table.put(new ParameterInt("degrees_of_freedom", 6));
            table.put(new ParameterInt("initial_interpolation_type", 1));
            table.put(new ParameterInt("cost_function_type", 1));
            table.put(new ParameterBoolean("use_elsunc", false));

            table.put(new ParameterFloat("rotate_begin", -30));
            table.put(new ParameterFloat("rotate_end", 30));
            table.put(new ParameterFloat("coarse_rate", 15));
            table.put(new ParameterFloat("fine_rate", 6));

            table.put(new ParameterBoolean("do_display_transform", true));
            table.put(new ParameterInt("final_interpolation_type", 1));

            table.put(new ParameterBoolean("do_subsample", true));
            table.put(new ParameterInt("out_of_bounds_index", 0));
            table.put(new ParameterString("matrix_directory"));

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
                return getResultImage().getImageName();
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
