package gov.nih.mipav.view.dialogs;


import gov.nih.mipav.model.algorithms.*;
import gov.nih.mipav.model.algorithms.registration.*;
import gov.nih.mipav.model.file.*;
import gov.nih.mipav.model.file.FileInfoBase.Unit;
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
 * Dialog to get user input, then call AlgorithmRegOAR25D. Internal registration is performed within one 3D image. In
 * adjacent mode the first slice above the reference slice is registered to the reference slice, then the second slice
 * above the reference slice is registered to the first slice above the reference slice, and so on until the last slice
 * is registered the next to last slice. Then, the first slice below the reference slice is registered to the reference
 * slice, the second slice below the reference slice is registered to first slice below the reference slice, and so on
 * unitl the first slice is registered to the second slice. In reference mode every slice is simply registered to the
 * reference slice.
 *
 * @author  Neva Cherniavsky
 * @see     AlgorithmCostFunctions
 * @see     AlgorithmRegOAR25D2
 */
public class JDialogRegistrationOAR25D extends JDialogScriptableBase
        implements AlgorithmInterface, DialogDefaultsInterface {

    //~ Static fields/initializers -------------------------------------------------------------------------------------

    /** Use serialVersionUID for interoperability. */
    private static final long serialVersionUID = -8324270868997072706L;

    //~ Instance fields ------------------------------------------------------------------------------------------------
    
 // Different search algorithms
    private final int POWELL = 0;
    
	private final int ELSUNC = 1;
	
	private final int LEVENBERG_MARQUARDT = 2;
	
	private final int NL2SOL = 3;
	
	private int searchAlgorithm = POWELL;

    /** DOCUMENT ME! */
    boolean doGraph;

    /** DOCUMENT ME! */
    private JRadioButton adjacentImageRButton;

    /** Variables for Advanced Settings dialog. */
    private JDialog advancedDialog;

    /** DOCUMENT ME! */
    private JTextField maxIterationsText, numMinText;

    /** DOCUMENT ME! */
    private JButton buttonWeightInput;

    /** DOCUMENT ME! */
    private JTextField coarseRateText;

    /** DOCUMENT ME! */
    private JComboBox comboBoxCostFunct;
    
    private JComboBox comboBoxSearchAlgo;

    /** DOCUMENT ME! */
    private JComboBox comboBoxDOF;

    /** DOCUMENT ME! */
    private JComboBox comboBoxInterp;

    /** DOCUMENT ME! */
    private JComboBox comboBoxInterp2;

    /** DOCUMENT ME! */
    private int cost, interp, interp2, DOF;

    /** DOCUMENT ME! */
    private boolean doAdjacent;

    /** DOCUMENT ME! */
    private boolean doColor;

    /** DOCUMENT ME! */
    private boolean doSubsample;

    /** DOCUMENT ME! */
    private String fileNameWInput, directoryWInput;

    /** DOCUMENT ME! */
    private JTextField fineRateText;

    /** DOCUMENT ME! */
    private JCheckBox graphCheckBox;

    /** DOCUMENT ME! */
    private ModelImage inputWeightImage;

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

    /** DOCUMENT ME! */
    private ModelImage matchImage; // register slices within matchImage

    /** DOCUMENT ME! */
    private int maxIterations_def = 2, numMinima_def = 6;

    /** DOCUMENT ME! */
    private int maxIterations = maxIterations_def;

    /** DOCUMENT ME! */
    private JRadioButton noneRadio;

    /** DOCUMENT ME! */
    private int numMinima = numMinima_def;

    /** DOCUMENT ME! */
    private int refImageNum = 0;

    /** DOCUMENT ME! */
    private JTextField refImageNumText;

    /** DOCUMENT ME! */
    private JRadioButton refImageRButton;

    /** DOCUMENT ME! */
    private ModelImage refSlice = null; // for optional outside reference slice

    /** DOCUMENT ME! */
    private AlgorithmRegOAR25D2 reg25 = null;
    
    private AlgorithmRegELSUNCOAR25D reg25E = null;

    /** DOCUMENT ME! */
    private float rotateBegin, rotateEnd, coarseRate, fineRate;

    /** DOCUMENT ME! */
    private JTextField rotateBeginText;

    /** DOCUMENT ME! */
    private JTextField rotateEndText;

    /** DOCUMENT ME! */
    private JCheckBox sampleCheckBox;

    /** DOCUMENT ME! */
    private JTextField textInput;

    /** DOCUMENT ME! */
    private boolean transformVOIs = false;

    /** DOCUMENT ME! */
    private JCheckBox transformVOIsBox;

    /** DOCUMENT ME! */
    private ViewUserInterface UI;

    /** DOCUMENT ME! */
    private boolean useOutsideReferenceSlice = false;

    /** DOCUMENT ME! */
    private JRadioButton voiRadio;

    /** DOCUMENT ME! */
    private boolean voisOnly;

    /** DOCUMENT ME! */
    private boolean weighted;

    /** DOCUMENT ME! */
    private JRadioButton weightRadio;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Empty constructor needed for dynamic instantiation (used during scripting).
     */
    public JDialogRegistrationOAR25D() { }

    /**
     * Creates new dialog for user to choose variables for internal registration.
     *
     * @param  theParentFrame  Parent frame.
     * @param  im              Source image.
     */
    public JDialogRegistrationOAR25D(Frame theParentFrame, ModelImage im) {
        super(theParentFrame, false);
        matchImage = im;

        if (matchImage.isColorImage()) {
            doColor = true;
        } else {
            doColor = false;
        }

        UI = ViewUserInterface.getReference();
        init();
        loadDefaults();
        setVisible(true);
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
        	//MipavUtil.showHelp("OAR19076");
            MipavUtil.showWebHelp("Optimized_automatic_registration_3D#Optimized_Automatic_Registration_dialog_box_options");
        } else if (command.equals("AdvancedSettings")) {
            maxIterations_def = maxIterations;
            numMinima_def = numMinima;
            advancedDialog = buildAdvancedDialog(maxIterations, numMinima);
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
                MipavUtil.displayError("Out of memory in JDialogRegistrationOAR25D.");

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
        int i;
        float[] rot = null;
        float[] posR = null;
        float[][] trans = null;
        float[][] posT = null;

        if (algorithm instanceof AlgorithmRegOAR25D2) {

            if (Preferences.is(Preferences.PREF_SAVE_DEFAULTS) && !isScriptRunning()) {
                saveDefaults();
            }

            if (algorithm.isCompleted()) {
                matchImage.clearMask();


                if (doGraph) {

                    if (DOF == 3) {
                        rot = reg25.getRot();
                    }

                    posR = new float[matchImage.getExtents()[2]];

                    for (i = 0; i < matchImage.getExtents()[2]; i++) {
                        posR[i] = i + 1.0f;
                    }

                    if (DOF == 3) {
                        ViewJFrameGraph rotGraph = new ViewJFrameGraph(posR, rot, "Rotations", "Slice number",
                                                                       "Degrees");
                        rotGraph.makeRangeSymmetric();
                        rotGraph.setDefaultDirectory(UI.getDefaultDirectory());
                        rotGraph.setVisible(true);
                    }

                    trans = reg25.getTrans();
                    posT = new float[2][matchImage.getExtents()[2]];

                    for (i = 0; i < matchImage.getExtents()[2]; i++) {
                        posT[0][i] = i + 1.0f;
                        posT[1][i] = i + 1.0f;
                    }

                    ViewJFrameGraph transGraph = new ViewJFrameGraph(posT, trans, "Translations", "Slice number",
                                                                     "Translations in " +
                                                                     (Unit.getUnitFromLegacyNum(matchImage.getFileInfo(0).getUnitsOfMeasure(0))).getAbbrev());
                    transGraph.makeRangeSymmetric();
                    transGraph.showXYZLegends();
                    transGraph.setDefaultDirectory(UI.getDefaultDirectory());
                    transGraph.setVisible(true);
                } // if (doGraph)

                insertScriptLine();
            } // isCompleted

            if (reg25 != null) {
                reg25.disposeLocal();
                reg25.finalize();
            }

            reg25 = null;

            matchImage = null;

            if (inputWeightImage != null) {
                inputWeightImage.disposeLocal();
            }

            inputWeightImage = null;

            rot = null;
            posR = null;

            if (trans != null) {

                for (i = 0; i < trans.length; i++) {
                    trans[i] = null;
                }

                trans = null;
            }

            if (posT != null) {

                for (i = 0; i < posT.length; i++) {
                    posT[i] = null;
                }

                posT = null;
            }

            dispose();
            System.gc();
        }
        
        if (algorithm instanceof AlgorithmRegELSUNCOAR25D) {

            if (Preferences.is(Preferences.PREF_SAVE_DEFAULTS) && !isScriptRunning()) {
                saveDefaults();
            }

            if (algorithm.isCompleted()) {
                matchImage.clearMask();


                if (doGraph) {

                    if (DOF == 3) {
                        rot = reg25E.getRot();
                    }

                    posR = new float[matchImage.getExtents()[2]];

                    for (i = 0; i < matchImage.getExtents()[2]; i++) {
                        posR[i] = i + 1.0f;
                    }

                    if (DOF == 3) {
                        ViewJFrameGraph rotGraph = new ViewJFrameGraph(posR, rot, "Rotations", "Slice number",
                                                                       "Degrees");
                        rotGraph.makeRangeSymmetric();
                        rotGraph.setDefaultDirectory(UI.getDefaultDirectory());
                        rotGraph.setVisible(true);
                    }

                    trans = reg25E.getTrans();
                    posT = new float[2][matchImage.getExtents()[2]];

                    for (i = 0; i < matchImage.getExtents()[2]; i++) {
                        posT[0][i] = i + 1.0f;
                        posT[1][i] = i + 1.0f;
                    }

                    ViewJFrameGraph transGraph = new ViewJFrameGraph(posT, trans, "Translations", "Slice number",
                                                                     "Translations in " +
                                                                     (Unit.getUnitFromLegacyNum(matchImage.getFileInfo(0).getUnitsOfMeasure(0))).getAbbrev());
                    transGraph.makeRangeSymmetric();
                    transGraph.showXYZLegends();
                    transGraph.setDefaultDirectory(UI.getDefaultDirectory());
                    transGraph.setVisible(true);
                } // if (doGraph)

                insertScriptLine();
            } // isCompleted

            if (reg25E != null) {
                reg25E.disposeLocal();
                reg25E.finalize();
            }

            reg25E = null;

            matchImage = null;

            if (inputWeightImage != null) {
                inputWeightImage.disposeLocal();
            }

            inputWeightImage = null;

            rot = null;
            posR = null;

            if (trans != null) {

                for (i = 0; i < trans.length; i++) {
                    trans[i] = null;
                }

                trans = null;
            }

            if (posT != null) {

                for (i = 0; i < posT.length; i++) {
                    posT[i] = null;
                }

                posT = null;
            }

            dispose();
            System.gc();
        }
    }

    /**
     * Construct a delimited string that contains the parameters to this algorithm.
     *
     * @param   delim  the parameter delimiter (defaults to " " if empty)
     *
     * @return  the parameter string
     */
    public String getParameterString(String delim) {

        if (delim.equals("")) {
            delim = " ";
        }

        String str = new String();

        // str += weighted + delim;
        // if ( weighted ) {
        // str += UI.getScriptDialog().getVar( inputWeightImage.getImageName() ) + delim;
        // }
        str += cost + delim;
        str += DOF + delim;
        str += interp + delim;
        str += interp2 + delim;
        str += doAdjacent + delim;
        str += refImageNum + delim;
        str += rotateBegin + delim;
        str += rotateEnd + delim;
        str += coarseRate + delim;
        str += fineRate + delim;
        str += doGraph + delim;
        str += doSubsample + delim;
        str += transformVOIs + delim;
        str += maxIterations + delim;
        str += numMinima + delim;
        str += searchAlgorithm;

        return str;
    }

    /**
     * Changes the interpolation box to enabled or disabled depending on if the transform box is checked or not.
     *
     * @param  event  Event that triggered this function.
     */
    public void itemStateChanged(ItemEvent event) {

        if ((event.getSource() == weightRadio) || (event.getSource() == noneRadio) || (event.getSource() == voiRadio)) {
            buttonWeightInput.setEnabled(weightRadio.isSelected());

            if (weightRadio.isSelected()) {
                comboBoxDOF.setSelectedIndex(4);
            } // if (weightRadio.isSelected())
        } // if ((event.getSource() == weightRadio) || (event.getSource() == noneRadio) ||

        // (event.getSource() == voiRadio))
        else if (event.getSource() == comboBoxDOF) {

            if (comboBoxDOF.getSelectedIndex() == 0) {
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
                graphCheckBox.setText("Graph translations");
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
                graphCheckBox.setText("Graph rotations and translations");
                sampleCheckBox.setEnabled(true);
            }

            if ((comboBoxDOF.getSelectedIndex() <= 1) && (refImageRButton.isSelected())) {
                graphCheckBox.setEnabled(true);
            } else {
                graphCheckBox.setEnabled(false);
                graphCheckBox.setSelected(false);
            }
        } // else if (event.getSource() == comboBoxDOF)
        else if ((event.getSource() == adjacentImageRButton) || (event.getSource() == refImageRButton)) {

            if ((comboBoxDOF.getSelectedIndex() <= 1) && (refImageRButton.isSelected())) {
                graphCheckBox.setEnabled(true);
            } else {
                graphCheckBox.setEnabled(false);
                graphCheckBox.setSelected(false);
            }
        }
    }

    /**
     * Loads the default settings from Preferences to set up the dialog.
     */
    public void loadDefaults() {
        String defaultsString = Preferences.getDialogDefaults(getDialogName());

        if ((weightRadio != null) && (defaultsString != null)) {
            System.err.println("Defaults string: " + defaultsString);

            try {
                StringTokenizer st = new StringTokenizer(defaultsString, ",");
                weightRadio.setSelected(MipavUtil.getBoolean(st));

                int tempInt = MipavUtil.getInt(st);

                switch (tempInt) {

                    case AlgorithmCostFunctions.LEAST_SQUARES_SMOOTHED_COLOR:
                    case AlgorithmCostFunctions.LEAST_SQUARES_SMOOTHED_WGT_COLOR:
                    case AlgorithmCostFunctions.CORRELATION_RATIO_SMOOTHED:
                    case AlgorithmCostFunctions.CORRELATION_RATIO_SMOOTHED_WGT:
                        comboBoxCostFunct.setSelectedIndex(0);
                        break;

                    case AlgorithmCostFunctions.LEAST_SQUARES_SMOOTHED:
                    case AlgorithmCostFunctions.LEAST_SQUARES_SMOOTHED_WGT:
                        comboBoxCostFunct.setSelectedIndex(1);
                        break;

                    case AlgorithmCostFunctions.NORMALIZED_XCORRELATION_SMOOTHED:
                    case AlgorithmCostFunctions.NORMALIZED_XCORRELATION_SMOOTHED_WGT:
                        comboBoxCostFunct.setSelectedIndex(2);
                        break;

                    case AlgorithmCostFunctions.NORMALIZED_MUTUAL_INFORMATION_SMOOTHED:
                    case AlgorithmCostFunctions.NORMALIZED_MUTUAL_INFORMATION_SMOOTHED_WGT:
                        comboBoxCostFunct.setSelectedIndex(3);
                        break;

                    default:
                        comboBoxCostFunct.setSelectedIndex(0);
                }

                tempInt = MipavUtil.getInt(st);

                switch (tempInt) {

                    case 2:
                        comboBoxDOF.setSelectedIndex(0);
                        break;

                    case 3:
                        comboBoxDOF.setSelectedIndex(1);
                        break;

                    case 4:
                        comboBoxDOF.setSelectedIndex(2);
                        break;

                    case 5:
                        comboBoxDOF.setSelectedIndex(3);
                        break;

                    case 6:
                        comboBoxDOF.setSelectedIndex(4);
                        break;

                    default:
                        comboBoxDOF.setSelectedIndex(4);
                        break;
                }

                tempInt = MipavUtil.getInt(st);

                switch (comboBoxInterp.getSelectedIndex()) {

                    case AlgorithmTransform.BILINEAR:
                        comboBoxInterp.setSelectedIndex(0);
                        break;

                    case AlgorithmTransform.BSPLINE3:
                        comboBoxInterp.setSelectedIndex(1);
                        break;

                    case AlgorithmTransform.BSPLINE4:
                        comboBoxInterp.setSelectedIndex(2);
                        break;

                    case AlgorithmTransform.CUBIC_LAGRANGIAN:
                        comboBoxInterp.setSelectedIndex(3);
                        break;

                    case AlgorithmTransform.QUINTIC_LAGRANGIAN:
                        comboBoxInterp.setSelectedIndex(4);
                        break;

                    case AlgorithmTransform.HEPTIC_LAGRANGIAN:
                        comboBoxInterp.setSelectedIndex(5);
                        break;

                    case AlgorithmTransform.WSINC:
                        comboBoxInterp.setSelectedIndex(6);
                        break;

                    default:
                        comboBoxInterp.setSelectedIndex(0);
                        break;
                }

                tempInt = MipavUtil.getInt(st);

                switch (comboBoxInterp2.getSelectedIndex()) {

                    case AlgorithmTransform.BILINEAR:
                        comboBoxInterp2.setSelectedIndex(0);
                        break;

                    case AlgorithmTransform.BSPLINE3:
                        comboBoxInterp2.setSelectedIndex(1);
                        break;

                    case AlgorithmTransform.BSPLINE4:
                        comboBoxInterp2.setSelectedIndex(2);
                        break;

                    case AlgorithmTransform.CUBIC_LAGRANGIAN:
                        comboBoxInterp2.setSelectedIndex(3);
                        break;

                    case AlgorithmTransform.QUINTIC_LAGRANGIAN:
                        comboBoxInterp2.setSelectedIndex(4);
                        break;

                    case AlgorithmTransform.HEPTIC_LAGRANGIAN:
                        comboBoxInterp2.setSelectedIndex(5);
                        break;

                    case AlgorithmTransform.WSINC:
                        comboBoxInterp2.setSelectedIndex(6);
                        break;

                    default:
                        comboBoxInterp2.setSelectedIndex(0);
                        break;
                }

                adjacentImageRButton.setSelected(MipavUtil.getBoolean(st));
                refImageNumText.setText("" + MipavUtil.getInt(st));
                rotateBeginText.setText("" + MipavUtil.getFloat(st));
                rotateEndText.setText("" + MipavUtil.getFloat(st));
                coarseRateText.setText("" + MipavUtil.getFloat(st));
                fineRateText.setText("" + MipavUtil.getFloat(st));
                graphCheckBox.setSelected(MipavUtil.getBoolean(st));
                sampleCheckBox.setSelected(MipavUtil.getBoolean(st));

                boolean doTV = MipavUtil.getBoolean(st);

                if (transformVOIsBox.isEnabled()) {
                    transformVOIsBox.setSelected(doTV);
                }

                setMaxIterations(MipavUtil.getInt(st));
                setNumMinima(MipavUtil.getInt(st));
            } catch (Exception ex) {

                // since there was a problem parsing the defaults string, start over with the original defaults
                //System.out.println("Resetting defaults for dialog: " + getDialogName());
                Preferences.removeProperty(getDialogName());
                ex.printStackTrace();
            }
        }
    }

    /**
     * Saves the default settings into the Preferences file.
     */
    public void saveDefaults() {

        if (weightRadio != null) {
            String defaultsString = new String(weighted + "," + getParameterString(","));
            Preferences.saveDialogDefaults(getDialogName(), defaultsString);
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
     * Accessor to set doAdjacent.
     *
     * @param  doAdj  true means adjacent mode, false means reference mode
     */
    public void setDoAdjacent(boolean doAdj) {
        doAdjacent = doAdj;
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
     * Accessor to set if a graph should be brought up of rotations & translations.
     *
     * @param  doGr  boolean true = bring up graph
     */
    public void setDoGraph(boolean doGr) {
        doGraph = doGr;
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
     * Accessor to set graphCheckBox.
     *
     * @param  doGraph  if true output graphs of rotations and translations
     */
    public void setGraphCheckBox(boolean doGraph) {
        this.doGraph = doGraph;
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
     * Accessor to set maxIterations.
     *
     * @param  maxIterations  DOCUMENT ME!
     */
    public void setMaxIterations(int maxIterations) {
        this.maxIterations = maxIterations;
    }

    /**
     * Accessor to set numMinima.
     *
     * @param  numMinima  DOCUMENT ME!
     */
    public void setNumMinima(int numMinima) {
        this.numMinima = numMinima;
    }

    /**
     * allows user to use an outside reference slice for registering.
     *
     * @param  refSlice  (2-Dim reference slice)
     */
    public void setOutsideReferenceSlice(ModelImage refSlice) {
        this.refSlice = refSlice;
        this.useOutsideReferenceSlice = true;
    }

    /**
     * Accessor to set refImageNum.
     *
     * @param  refImageNumber  number of reference slice
     */
    public void setRefImageNum(int refImageNumber) {

        // if we are setting this up in a script where we don't know the middle slice, setting
        // refImageNumber to -1 will automatically set the reference slice to the middle
        if (refImageNumber == -1) {
            refImageNum = matchImage.getExtents()[2] / 2;
        } else {
            refImageNum = refImageNumber;
        }
    }
    
    /**
     * 
     * @param searchAlgorithm
     */
    public void setSearchAlgorithm(int searchAlgorithm) {
    	this.searchAlgorithm = searchAlgorithm;
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
     * Accessor to set whether or not VOIs should be transformed.
     *
     * @param  transformVOIs  DOCUMENT ME!
     */
    public void setTransformVOIs(boolean transformVOIs) {
        this.transformVOIs = transformVOIs;
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
        BitSet mask = null;

        if (voisOnly) {
            float[] matchRes = new float[] {
                                   matchImage.getFileInfo(0).getResolutions()[0],
                                   matchImage.getFileInfo(0).getResolutions()[1],
                                   matchImage.getFileInfo(0).getResolutions()[2]
                               };

            inputWeightImage = new ModelImage(ModelStorageBase.BYTE, matchImage.getExtents(), "VOI match");

            inputWeightImage.getFileInfo(0).setResolutions(matchRes);
            // make new input image based on the VOIs. pass those new image to the registration algorithm

            mask = matchImage.generateVOIMask();

            int matchImageSize = matchImage.getSliceSize() * matchImage.getExtents()[2];

            for (int i = 0; i < matchImageSize; i++) {

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
	            reg25E = new AlgorithmRegELSUNCOAR25D(matchImage, inputWeightImage, cost, DOF, interp, interp2, doAdjacent,
	                                            refImageNum, rotateBegin, rotateEnd, coarseRate, fineRate, doGraph,
	                                            doSubsample, transformVOIs, maxIterations, numMinima, searchAlgorithm);
	        } else {
	
	            reg25E = new AlgorithmRegELSUNCOAR25D(matchImage, cost, DOF, interp, interp2, doAdjacent, refImageNum,
	                                            rotateBegin, rotateEnd, coarseRate, fineRate, doGraph, doSubsample,
	                                            transformVOIs, maxIterations, numMinima, searchAlgorithm);
	
	            if (useOutsideReferenceSlice) {
	
	                if (!reg25E.setReferenceSlice(refSlice)) {
	                    MipavUtil.displayError("Reference slice does not have same extents as input image");
	                }
	            }
	
	        }
	
	        // Start the thread as a low priority because we wish to still have user interface work fast.
	        reg25E.addListener(this);
	
	        createProgressBar(matchImage.getImageName(), reg25E);
	
	        // Hide dialog
	        setVisible(false);
	
	        if(isRunInSeparateThread()) {
		        // Start the thread as a low priority because we wish to still have user interface work fast
		        if (reg25E.startMethod(Thread.MIN_PRIORITY) == false) {
		            MipavUtil.displayError("A thread is already running on this object");
		        }
	        } else {
	        	reg25E.run();
	        }	
        }
        else {
	        if (weighted) {
	            reg25 = new AlgorithmRegOAR25D2(matchImage, inputWeightImage, cost, DOF, interp, interp2, doAdjacent,
	                                            refImageNum, rotateBegin, rotateEnd, coarseRate, fineRate, doGraph,
	                                            doSubsample, transformVOIs, maxIterations, numMinima);
	        } else {
	
	            reg25 = new AlgorithmRegOAR25D2(matchImage, cost, DOF, interp, interp2, doAdjacent, refImageNum,
	                                            rotateBegin, rotateEnd, coarseRate, fineRate, doGraph, doSubsample,
	                                            transformVOIs, maxIterations, numMinima);
	
	            if (useOutsideReferenceSlice) {
	
	                if (!reg25.setReferenceSlice(refSlice)) {
	                    MipavUtil.displayError("Reference slice does not have same extents as input image");
	                }
	            }
	
	        }
	
	        // Start the thread as a low priority because we wish to still have user interface work fast.
	        reg25.addListener(this);
	
	        createProgressBar(matchImage.getImageName(), reg25);
	
	        // Hide dialog
	        setVisible(false);
	
	        if(isRunInSeparateThread()) {
		        // Start the thread as a low priority because we wish to still have user interface work fast
		        if (reg25.startMethod(Thread.MIN_PRIORITY) == false) {
		            MipavUtil.displayError("A thread is already running on this object");
		        }
	        } else {
	        	reg25.run();
	        }
        }
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

        setWeighted(scriptParameters.getParams().getBoolean("do_use_weight_images"));

        if (weighted) {
            setInputWeightImage(scriptParameters.retrieveImage("input_weight_image"));
        }

        setDOF(scriptParameters.getParams().getInt("degrees_of_freedom"));
        setInterp(scriptParameters.getParams().getInt("initial_interpolation_type"));
        setCostChoice(scriptParameters.getParams().getInt("cost_function_type"));
        setSearchAlgorithm(scriptParameters.getParams().getInt("search_algorithm"));

        setCoarseBegin(scriptParameters.getParams().getFloat("rotate_begin"));
        setCoarseEnd(scriptParameters.getParams().getFloat("rotate_end"));
        setCoarseRate(scriptParameters.getParams().getFloat("coarse_rate"));
        setFineRate(scriptParameters.getParams().getFloat("fine_rate"));

        setInterp2(scriptParameters.getParams().getInt("final_interpolation_type"));

        setSubsample(scriptParameters.getParams().getBoolean("do_subsample"));

        setMaxIterations(scriptParameters.getParams().getInt("max_iterations"));
        setNumMinima(scriptParameters.getParams().getInt("num_minima"));

        setDoAdjacent(scriptParameters.getParams().getBoolean("do_register_to_adjacent_slice"));
        setRefImageNum(scriptParameters.getParams().getInt("reference_slice_num"));
        setGraphCheckBox(scriptParameters.getParams().getBoolean("do_graph_transform"));
        setTransformVOIs(scriptParameters.getParams().getBoolean("do_transform_vois"));

        if (refImageNum == 0) {
            setRefImageNum((int) (matchImage.getExtents()[2] / 2) + 1);
        }
    }

    /**
     * {@inheritDoc}
     */
    protected void storeParamsFromGUI() throws ParserException {
        scriptParameters.storeInputImage(matchImage);

        scriptParameters.getParams().put(ParameterFactory.newParameter("do_use_weight_images", weighted));

        if (weighted) {
            scriptParameters.storeImage(inputWeightImage, "input_weight_image");
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
        scriptParameters.getParams().put(ParameterFactory.newParameter("do_subsample", doSubsample));
        scriptParameters.getParams().put(ParameterFactory.newParameter("max_iterations", maxIterations));
        scriptParameters.getParams().put(ParameterFactory.newParameter("num_minima", numMinima));

        scriptParameters.getParams().put(ParameterFactory.newParameter("do_register_to_adjacent_slice", doAdjacent));
        scriptParameters.getParams().put(ParameterFactory.newParameter("reference_slice_num", refImageNum));
        scriptParameters.getParams().put(ParameterFactory.newParameter("do_graph_transform", doGraph));
        scriptParameters.getParams().put(ParameterFactory.newParameter("do_transform_vois", transformVOIs));
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
        // Changed to non-modal after adding Help button 12/17/07

        // Setting panel
        JPanel settingsPanel = new JPanel();
        settingsPanel.setBorder(buildTitledBorder("Optimization settings"));
        settingsPanel.setLayout(new BoxLayout(settingsPanel, BoxLayout.Y_AXIS));

        JPanel maxIterPanel = new JPanel();
        maxIterPanel.setLayout(new BorderLayout(1, 3));
        maxIterPanel.setBorder(BorderFactory.createEmptyBorder(0, 10, 0, 10));

        JLabel maxIterationsLabel = new JLabel("Number of iterations: ", JLabel.LEFT);
        maxIterationsLabel.setFont(serif12);
        maxIterPanel.add(maxIterationsLabel, BorderLayout.WEST);
        maxIterPanel.setToolTipText("Used for levelOne. Other levels are multiples of this #.");
        maxIterationsText = new JTextField(String.valueOf(maxIter), 5);
        maxIterationsText.addFocusListener(this);
        maxIterPanel.add(maxIterationsText, BorderLayout.CENTER);

        JLabel maxIterInstruct = new JLabel("Recommended value 1-5.", JLabel.RIGHT);
        maxIterInstruct.setFont(serif12);
        maxIterPanel.add(maxIterInstruct, BorderLayout.SOUTH);

        JPanel numMinPanel = new JPanel();
        numMinPanel.setLayout(new BorderLayout(1, 3));
        numMinPanel.setBorder(BorderFactory.createEmptyBorder(0, 10, 0, 10));

        JLabel numMinLabel = new JLabel("Number of minima from Level 8 to test at Level 4: ", JLabel.LEFT);
        numMinLabel.setFont(serif12);
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
     * Initializes the GUI components and displays the dialog.
     */
    private void init() {
        setForeground(Color.black);
        setTitle("Optimized Automatic Image Registration 2.5D");

        JPanel optPanel = new JPanel();
        optPanel.setLayout(new GridBagLayout());
        optPanel.setBorder(buildTitledBorder("Input Options"));

        String matchName = matchImage.getImageName();
        ViewVOIVector VOIs = matchImage.getVOIs();
        JLabel labelImage = new JLabel("Internal Registration for " + matchName);
        labelImage.setForeground(Color.black);
        labelImage.setFont(serif12);
        labelImage.setAlignmentX(Component.LEFT_ALIGNMENT);

        JLabel labelDOF = new JLabel("Degrees of freedom:");
        labelDOF.setForeground(Color.black);
        labelDOF.setFont(serif12);
        labelDOF.setAlignmentX(Component.LEFT_ALIGNMENT);

        comboBoxDOF = new JComboBox();
        comboBoxDOF.setFont(MipavUtil.font12);
        comboBoxDOF.setBackground(Color.white);
        comboBoxDOF.setToolTipText("Degrees of freedom");
        comboBoxDOF.addItem("Translations - 2");
        comboBoxDOF.addItem("Rigid - 3");
        comboBoxDOF.addItem("Global rescale - 4");
        comboBoxDOF.addItem("Specific rescale - 5");
        comboBoxDOF.addItem("Affine - 6");
        comboBoxDOF.setSelectedIndex(1);
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
            comboBoxCostFunct.addItem("Normalized mutual information");
        }

        //This is least squares if doColor, else correlation ratio
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

        rotateBeginText = new JTextField("-3", 3);
        rotateEndText = new JTextField("3", 3);

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
        coarseRateText = new JTextField("3", 3);

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
        fineRateText = new JTextField("2", 3);

        finePanel.add(labelFine);
        finePanel.add(fineRateText);
        finePanel.add(labelFineDegrees);
        finePanel.setAlignmentX(Component.LEFT_ALIGNMENT);

        sampleCheckBox = new JCheckBox("Subsample image for speed");
        sampleCheckBox.setFont(serif12);
        sampleCheckBox.setForeground(Color.black);
        sampleCheckBox.setSelected(true);
        sampleCheckBox.setEnabled(true);

        // ignoreCOGBox = new JCheckBox("Ignore center of gravity");
        // ignoreCOGBox.setFont(serif12);
        // ignoreCOGBox.setForeground(Color.black);
        // ignoreCOGBox.setSelected(false);
        // ignoreCOGBox.setEnabled(true);

        transformVOIsBox = new JCheckBox("Transform VOIs");
        transformVOIsBox.setFont(serif12);
        transformVOIsBox.setForeground(Color.black);
        transformVOIsBox.setSelected(false);

        if ((VOIs != null) && (VOIs.size() > 0)) {
            transformVOIsBox.setEnabled(true);
        } else {
            transformVOIsBox.setEnabled(false);
        }

        Insets insets = new Insets(0, 2, 0, 2);
        GridBagConstraints gbc = new GridBagConstraints();

        gbc.insets = insets;
        gbc.fill = GridBagConstraints.NONE;
        gbc.anchor = GridBagConstraints.WEST;

        gbc.gridx = 0;
        gbc.gridy = 0;
        gbc.gridwidth = 2;
        gbc.gridheight = 1;
        optPanel.add(labelImage, gbc);

        gbc.gridx = 0;
        gbc.gridy = 1;
        gbc.weightx = 0;
        gbc.gridwidth = 1;
        optPanel.add(labelDOF, gbc);
        gbc.gridx = 1;
        gbc.gridy = 1;
        gbc.weightx = 1;
        gbc.gridwidth = GridBagConstraints.REMAINDER;
        optPanel.add(comboBoxDOF, gbc);

        gbc.gridx = 0;
        gbc.gridy = 2;
        gbc.weightx = 0;
        gbc.gridwidth = 1;
        optPanel.add(labelInterp, gbc);
        gbc.gridx = 1;
        gbc.gridy = 2;
        gbc.weightx = 1;
        gbc.gridwidth = GridBagConstraints.REMAINDER;
        optPanel.add(comboBoxInterp, gbc);

        gbc.gridx = 0;
        gbc.gridy = 3;
        gbc.weightx = 0;
        gbc.gridwidth = 1;
        optPanel.add(labelCost, gbc);
        gbc.gridx = 1;
        gbc.gridy = 3;
        gbc.weightx = 1;
        gbc.gridwidth = GridBagConstraints.REMAINDER;
        optPanel.add(comboBoxCostFunct, gbc);
        
        gbc.gridx = 0;
        gbc.gridy = 4;
        gbc.weightx = 0;
        gbc.gridwidth = 1;
        optPanel.add(labelSearch, gbc);
        gbc.gridx = 1;
        gbc.gridy = 4;
        gbc.weightx = 1;
        gbc.gridwidth = GridBagConstraints.REMAINDER;
        optPanel.add(comboBoxSearchAlgo, gbc);

        gbc.weightx = 0;
        gbc.gridwidth = 3;
        gbc.gridx = 0;
        gbc.gridy = 5;
        optPanel.add(rotateRangePanel, gbc);

        gbc.gridx = 0;
        gbc.gridy = 6;
        optPanel.add(coarsePanel, gbc);

        gbc.gridx = 0;
        gbc.gridy = 7;
        optPanel.add(finePanel, gbc);

        gbc.gridx = 0;
        gbc.gridy = 8;
        gbc.weightx = 1;
        gbc.gridwidth = 1;
        optPanel.add(sampleCheckBox, gbc);

        // gbc.gridx = 0;
        // gbc.gridy = 9;
        // gbc.weightx = 1;
        // gbc.gridwidth = 1;
        // optPanel.add(ignoreCOGBox, gbc);

        gbc.gridx = 0;
        gbc.gridy = 9;
        gbc.weightx = 1;
        gbc.gridwidth = 1;
        optPanel.add(transformVOIsBox, gbc);

        // Reference image panel
        JPanel refPanel = new JPanel(new GridBagLayout());
        refPanel.setBorder(buildTitledBorder("Reference image"));

        JLabel labelInternal = new JLabel("Reference slice (0-" + String.valueOf(matchImage.getExtents()[2]-1) + ")");
        labelInternal.setForeground(Color.black);
        labelInternal.setFont(serif12);
        gbc.gridx = 0;
        gbc.gridy = 0;
        gbc.gridwidth = 1;
        gbc.weightx = 0;
        refPanel.add(labelInternal, gbc);

        refImageNumText = new JTextField(String.valueOf((matchImage.getExtents()[2] / 2) - 1), 3);
        refImageNumText.setEnabled(true);
        gbc.gridx = 1;
        gbc.gridy = 0;
        gbc.gridwidth = GridBagConstraints.REMAINDER;
        gbc.weightx = 1;
        refPanel.add(refImageNumText, gbc);

        // Adjacent or reference radio buttons
        ButtonGroup group1 = new ButtonGroup();

        adjacentImageRButton = new JRadioButton("Register to adjacent slice", false);
        adjacentImageRButton.setFont(serif12);
        adjacentImageRButton.addItemListener(this);
        group1.add(adjacentImageRButton);

        refImageRButton = new JRadioButton("Register to reference slice", true);
        refImageRButton.setFont(serif12);
        refImageRButton.addItemListener(this);
        group1.add(refImageRButton);

        gbc.gridx = 0;
        gbc.gridy = 1;
        refPanel.add(adjacentImageRButton, gbc);
        gbc.gridx = 0;
        gbc.gridy = 2;
        refPanel.add(refImageRButton, gbc);

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
        voiRadio.setEnabled(false);

        if ((VOIs != null) && (VOIs.size() > 0)) {
            voiRadio.setEnabled(true);
        }

        weightGroup.add(voiRadio);

        weightRadio = new JRadioButton("Weight registration");
        weightRadio.setFont(serif12);
        weightRadio.setForeground(Color.black);
        weightRadio.setSelected(false);
        weightRadio.addItemListener(this);
        weightGroup.add(weightRadio);

        buttonWeightInput = new JButton("Choose input weight");
        buttonWeightInput.setForeground(Color.black);
        buttonWeightInput.setFont(serif12B);
        buttonWeightInput.setEnabled(false);
        buttonWeightInput.addActionListener(this);
        buttonWeightInput.setActionCommand("Input");
        buttonWeightInput.setPreferredSize(new Dimension(145, 30));

        textInput = new JTextField();
        textInput.setFont(serif12);
        textInput.setEnabled(false);

        JPanel weightPanel = new JPanel(new GridBagLayout());
        weightPanel.setBorder(buildTitledBorder("Weighted image"));

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
        weightPanel.add(buttonWeightInput, gbc);
        gbc.gridx = 1;
        gbc.weightx = 1;
        gbc.fill = GridBagConstraints.HORIZONTAL;
        weightPanel.add(textInput, gbc);

        JPanel outPanel = new JPanel();
        outPanel.setLayout(new GridBagLayout());
        outPanel.setBorder(buildTitledBorder("Output Options"));

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

        graphCheckBox = new JCheckBox("Graph rotations and translations");
        graphCheckBox.setFont(serif12);
        graphCheckBox.setForeground(Color.black);
        graphCheckBox.setSelected(false);
        graphCheckBox.setEnabled(true);

        gbc.gridx = 0;
        gbc.gridy = 0;
        gbc.weightx = 0;
        gbc.fill = GridBagConstraints.NONE;
        outPanel.add(labelInterp2, gbc);
        gbc.gridx = 1;
        gbc.gridy = 0;
        gbc.weightx = 1;
        gbc.fill = GridBagConstraints.HORIZONTAL;
        outPanel.add(comboBoxInterp2, gbc);
        gbc.gridx = 0;
        gbc.gridy = 1;
        gbc.weightx = 1;
        gbc.fill = GridBagConstraints.HORIZONTAL;
        outPanel.add(graphCheckBox, gbc);

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
        advancedButton.setMinimumSize(new Dimension(90, 30));
        advancedButton.setFont(serif12B);
        buttonPanel.add(advancedButton);

        JPanel mainPanel = new JPanel();
        mainPanel.setLayout(new BoxLayout(mainPanel, BoxLayout.Y_AXIS));
        mainPanel.setBorder(BorderFactory.createEmptyBorder(5, 5, 5, 5));
        optPanel.setAlignmentX(Component.LEFT_ALIGNMENT);
        refPanel.setAlignmentX(Component.LEFT_ALIGNMENT);
        weightPanel.setAlignmentX(Component.LEFT_ALIGNMENT);
        outPanel.setAlignmentX(Component.LEFT_ALIGNMENT);
        mainPanel.add(optPanel);
        mainPanel.add(refPanel);
        mainPanel.add(weightPanel);
        mainPanel.add(outPanel);

        getContentPane().add(mainPanel);
        getContentPane().add(buttonPanel, BorderLayout.SOUTH);
        pack();
        // setVisible(true);
    }

    /**
     * Sets the variables needed to call the registration algorithm based on the values entered in the dialog.
     *
     * @return  <code>true</code> if the variables are properly set, <code>false</code> otherwise.
     */
    private boolean setVariables() {
        weighted = weightRadio.isSelected();
        voisOnly = voiRadio.isSelected();

        if (weighted) {
            fileNameWInput = textInput.getText();

            try {
                FileIO fileIO = new FileIO();
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
                MipavUtil.displayError("Out of memory in JDialogRegistrationOAR25D");

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
                DOF = 2;
                break;

            case 1:
                DOF = 3;
                break;

            case 2:
                DOF = 4;
                break;

            case 3:
                DOF = 5;
                break;

            case 4:
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

        if (!testParameter(refImageNumText.getText(), 0, matchImage.getExtents()[2]-1)) {
            refImageNumText.requestFocus();
            refImageNumText.selectAll();

            return false;
        } else {
            refImageNum = Integer.valueOf(refImageNumText.getText()).intValue();
        }

        if (adjacentImageRButton.isSelected()) {
            doAdjacent = true;
        } else {
            doAdjacent = false;
        }

        doGraph = graphCheckBox.isSelected();

        doSubsample = sampleCheckBox.isSelected();

        // ignoreCOG = ignoreCOGBox.isSelected();

        transformVOIs = transformVOIsBox.isSelected();

        return true;
    }

}
