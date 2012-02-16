package gov.nih.mipav.view.renderer.WildMagic.DTI_FrameWork;


import gov.nih.mipav.model.algorithms.AlgorithmBase;
import gov.nih.mipav.model.algorithms.AlgorithmCostFunctions;
import gov.nih.mipav.model.algorithms.AlgorithmInterface;
import gov.nih.mipav.model.algorithms.AlgorithmTransform;
import gov.nih.mipav.model.algorithms.registration.AlgorithmRegOAR35D;
import gov.nih.mipav.model.file.DTIParameters;
import gov.nih.mipav.model.file.FileIO;
import gov.nih.mipav.model.file.FileInfoBase.Unit;
import gov.nih.mipav.model.scripting.ParserException;
import gov.nih.mipav.model.scripting.parameters.ParameterFactory;
import gov.nih.mipav.model.structures.ModelImage;
import gov.nih.mipav.model.structures.ModelStorageBase;

import gov.nih.mipav.view.MipavUtil;
import gov.nih.mipav.view.ViewImageFileFilter;
import gov.nih.mipav.view.ViewJFrameGraph;
import gov.nih.mipav.view.ViewJFrameImage;
import gov.nih.mipav.view.ViewUserInterface;
import gov.nih.mipav.view.ViewVOIVector;
import gov.nih.mipav.view.dialogs.JDialogScriptableBase;

import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.Component;
import java.awt.Dimension;
import java.awt.FlowLayout;
import java.awt.Font;
import java.awt.Frame;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.Insets;
import java.awt.Rectangle;
import java.awt.Toolkit;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.FocusListener;
import java.awt.event.ItemEvent;
import java.awt.event.ItemListener;
import java.io.File;
import java.util.BitSet;

import javax.swing.BorderFactory;
import javax.swing.Box;
import javax.swing.BoxLayout;
import javax.swing.ButtonGroup;
import javax.swing.JButton;
import javax.swing.JCheckBox;
import javax.swing.JComboBox;
import javax.swing.JDialog;
import javax.swing.JFileChooser;
import javax.swing.JLabel;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.JRadioButton;
import javax.swing.JTextField;
import javax.swing.border.EtchedBorder;
import javax.swing.border.TitledBorder;


public class JPanelDTIRegistrationEddyCurrent35D extends JPanel implements AlgorithmInterface, ActionListener,
        ItemListener {
    // ~ Static fields/initializers
    // -------------------------------------------------------------------------------------

    /** Use serialVersionUID for interoperability. */
    private static final long serialVersionUID = -4309868934393418962L;

    // ~ Instance fields
    // ------------------------------------------------------------------------------------------------

    /** DOCUMENT ME! */
    boolean doGraph;

    /** DOCUMENT ME! */
    private JRadioButton adjacentImageRButton;

    /** Variables for Advanced Settings dialog. */
    private JDialog advancedDialog;

    /** DOCUMENT ME! */
    private JRadioButton averageImageRButton;

    /** DOCUMENT ME! */
    private JTextField bracketBoundText, maxIterationsText, numMinText;

    /** DOCUMENT ME! */
    private JButton buttonWeightInput;

    /** DOCUMENT ME! */
    private JTextField coarseRateText;

    /** DOCUMENT ME! */
    private JComboBox comboBoxCostFunct;

    /** DOCUMENT ME! */
    private JComboBox comboBoxDOF;

    /** DOCUMENT ME! */
    private JComboBox comboBoxInterp;

    /** DOCUMENT ME! */
    private JComboBox comboBoxInterp2;

    /** DOCUMENT ME! */
    private int cost, interp, interp2, DOF;

    /** DOCUMENT ME! */
    private boolean doColor;

    /** DOCUMENT ME! */
    private boolean doSubsample;

    /** DOCUMENT ME! */
    private boolean fastMode;

    /** DOCUMENT ME! */
    private JCheckBox fastModeCheckbox;

    /** DOCUMENT ME! */
    private String fileNameWInput, directoryWInput;

    /** DOCUMENT ME! */
    private JTextField fineRateText;

    /** DOCUMENT ME! */
    private JCheckBox graphCheckBox;

    /** DOCUMENT ME! */
    private ModelImage inputWeightImage;

    /** DOCUMENT ME! */
    private JLabel labelInterp2;

    /** DOCUMENT ME! */
    private ModelImage matchImage; // register slices within matchImage

    private ModelImage resultImage;

    /** DOCUMENT ME! */
    private int maxIterations_def = 2, bracketBound_def = 10, numMinima_def = 3;

    /** DOCUMENT ME! */
    private int maxIterations = maxIterations_def, bracketBound = bracketBound_def;

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
    private ModelImage refVolume = null; // for optional outside reference volume

    /** DOCUMENT ME! */
    private AlgorithmRegOAR35D reg35 = null;

    /** DOCUMENT ME! */
    private int registerTo; // 1 for adjacent
                            // 2 for average
                            // 3 for reference

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
    private ViewUserInterface UI;

    /** DOCUMENT ME! */
    private boolean useOutsideReferenceVolume = false;

    /** DOCUMENT ME! */
    private JRadioButton voiRadio;

    /** DOCUMENT ME! */
    private boolean voisOnly;

    /** DOCUMENT ME! */
    private boolean weighted;

    /** DOCUMENT ME! */
    private JRadioButton weightRadio;

    private Font serif12;

    private Font serif12B;

    private JButton OKButton;

    private JButton cancelButton;

    private JButton helpButton;

    private JCheckBox advancedBox;
    
    private JCheckBox preProcessedBox;
    
    public JCheckBox epiCheckBox;

    private DTIPipeline pipeline;

    private AlgorithmTransform algTransform;

    public JPanel mainRegPanel;

    private JPanel settingsPanel;

    private JLabel bracketBoundLabel;

    private JLabel maxIterationsLabel;

    private JLabel numMinLabel;
    
    private JLabel t2FileLabel;
    
    private JButton t2Button;
    
    public ViewJFrameImage imageFrame;
    

    /** grid bag constraints * */
    private GridBagConstraints gbc;

    public JPanelDTIRegistrationEddyCurrent35D(DTIPipeline pipeline) {
        super();
        // super(theParentFrame, false);
        // super();
        // matchImage = im;

        this.pipeline = pipeline;

        UI = ViewUserInterface.getReference();
        init();
    }

    // ~ Methods
    // --------------------------------------------------------------------------------------------------------

    /**
     * Closes dialog box when the OK button is pressed, sets the variables, and calls the algorithm.
     * 
     * @param event Event that triggers function.
     */
    public void actionPerformed(ActionEvent event) {
        String command = event.getActionCommand();
        String tmpStr;

        if (command.equals("OK")) {
            System.out.println("okworking");
            if (pipeline.DWIImage != null) {
                if (setVariables()) {
                    callAlgorithm();
                }
            } else {
                MipavUtil.displayError("Please load a DWI dataset image");
            }
        } else if (command.equals("Cancel")) {

        } else if (command.equals("Help")) {
            MipavUtil.showHelp("OAR19076");

        } else if (command.equals("AdvancedSettings")) {
            if (advancedBox.isSelected()) {
                bracketBoundLabel.setForeground(Color.BLACK);
                maxIterationsLabel.setForeground(Color.BLACK);
                numMinLabel.setForeground(Color.BLACK);

                bracketBoundText.setEnabled(true);
                maxIterationsText.setEnabled(true);
                numMinText.setEnabled(true);
                fastModeCheckbox.setEnabled(true);

                bracketBound_def = bracketBound;
                maxIterations_def = maxIterations;
                numMinima_def = numMinima;
            } else {
                bracketBoundLabel.setForeground(Color.lightGray);
                maxIterationsLabel.setForeground(Color.lightGray);
                numMinLabel.setForeground(Color.lightGray);

                bracketBoundText.setEnabled(false);
                maxIterationsText.setEnabled(false);
                numMinText.setEnabled(false);
                fastModeCheckbox.setEnabled(false);
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
                MipavUtil.displayError("Out of memory in JDialogRegistrationOAR25D.");

                return;
            }
        } else if (command.equals("AdvancedOkay")) {
            tmpStr = bracketBoundText.getText();

            if (MipavUtil.testParameter(tmpStr, 1, 60)) {
                bracketBound = Integer.valueOf(tmpStr).intValue();
            } else {
                bracketBound = bracketBound_def;
            }

            tmpStr = maxIterationsText.getText();

            if (MipavUtil.testParameter(tmpStr, 1, 100)) {
                maxIterations = Integer.valueOf(tmpStr).intValue();
            } else {
                maxIterations = maxIterations_def;
            }

            tmpStr = numMinText.getText();

            if (MipavUtil.testParameter(tmpStr, 1, 25)) {
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
     * @param algorithm Algorithm that caused the event.
     */
    public void algorithmPerformed(AlgorithmBase algorithm) {
        int i;
        float[][] rot = null;
        float[][] posR = null;
        float[][] trans = null;
        float[][] posT = null;

        matchImage = pipeline.DWIImage;

        if (algorithm instanceof AlgorithmRegOAR35D) {

            if (reg35.isCompleted()) {

                // String name = makeImageName(matchImage.getImageName(), "_registered");

                resultImage = reg35.getTransformedImage();
                
                //resultImage.getFileInfo()[0].setFileName(name + ".avi");
               //resultImage.clearMask();
                
                //new ViewJFrameImage(resultImage);
                
                /*if (resultImage != null) { 
                    try { imageFrame = new ViewJFrameImage(resultImage, null,new Dimension(610, 200), UI); 
                    } 
                    catch (OutOfMemoryError error) {
                        MipavUtil.displayError("Out of memory:unable to open new frame"); 
                        } 
                    } else {
                        MipavUtil.displayError("Result Image is null"); 
                        }
                */
                if (resultImage == null) {
                    System.out.println("resultimage is null");
                }

                if (resultImage != null) {
                    System.out.println("resultimage not null");

                }
                /*
                 * resultImage.calcMinMax(); resultImage.setImageName(name);
                 * //resultImage.getFileInfo()[0].setFileName(name + ".avi"); resultImage.clearMask();
                 * //matchImage.clearMask();
                 * 
                 * if (resultImage != null) { try { imageFrame = new ViewJFrameImage(resultImage, null,new
                 * Dimension(610, 200), UI); } catch (OutOfMemoryError error) {
                 * MipavUtil.displayError("Out of memory:unable to open new frame"); } } else {
                 * MipavUtil.displayError("Result Image is null"); }
                 */

                if (doGraph) {
                    rot = reg35.getRot();
                    posR = new float[3][matchImage.getExtents()[3]];

                    for (i = 0; i < matchImage.getExtents()[3]; i++) {
                        posR[0][i] = i + 1.0f;
                        posR[1][i] = i + 1.0f;
                        posR[2][i] = i + 1.0f;
                    }

                    ViewJFrameGraph rotGraph = new ViewJFrameGraph(posR, rot, "Rotations", "Volume number", "Degrees");
                    rotGraph.makeRangeSymmetric();
                    rotGraph.showXYZLegends();
                    rotGraph.setDefaultDirectory(UI.getDefaultDirectory());
                    rotGraph.setVisible(true);
                    trans = reg35.getTrans();
                    posT = new float[3][matchImage.getExtents()[3]];

                    for (i = 0; i < matchImage.getExtents()[3]; i++) {
                        posT[0][i] = i + 1.0f;
                        posT[1][i] = i + 1.0f;
                        posT[2][i] = i + 1.0f;
                    }

                    ViewJFrameGraph transGraph = new ViewJFrameGraph(posT, trans, "Translations", "Volume number",
                            "Translations in "
                                    + (Unit.getUnitFromLegacyNum(matchImage.getFileInfo(0).getUnitsOfMeasure(0)))
                                            .getAbbrev());
                    transGraph.makeRangeSymmetric();
                    transGraph.showXYZLegends();
                    transGraph.setDefaultDirectory(UI.getDefaultDirectory());
                    transGraph.setVisible(true);
                } // if (doGraph)

            } // isCompleted

            if (reg35 != null) {
                reg35.disposeLocal();
            }

            reg35 = null;

            matchImage = null;

            if (inputWeightImage != null) {
                inputWeightImage.disposeLocal();
            }

            inputWeightImage = null;

            rot = null;

            if (posR != null) {

                for (i = 0; i < posR.length; i++) {
                    posR[i] = null;
                }

                posR = null;
            }

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

            System.gc();
        }
    }

    /**
     * Changes the interpolation box to enabled or disabled depending on if the transform box is checked or not.
     * 
     * @param event Event that triggered this function.
     */
    public void itemStateChanged(ItemEvent event) {

        if ( (event.getSource() == weightRadio) || (event.getSource() == noneRadio) || (event.getSource() == voiRadio)) {
            buttonWeightInput.setEnabled(weightRadio.isSelected());

        } // if ((event.getSource() == weightRadio) || (event.getSource() == noneRadio) ||

        // (event.getSource() == voiRadio))
        else if (event.getSource() == comboBoxDOF) {

            if ( (comboBoxDOF.getSelectedIndex() == 0)
                    && ( (refImageRButton.isSelected()) || (averageImageRButton.isSelected()))) {
                graphCheckBox.setEnabled(true);
            } else {
                graphCheckBox.setEnabled(false);
                graphCheckBox.setSelected(false);
            }
        } // else if (event.getSource() == comboBoxDOF)
        else if ( (event.getSource() == adjacentImageRButton) || (event.getSource() == averageImageRButton)
                || (event.getSource() == refImageRButton)) {

            if ( (comboBoxDOF.getSelectedIndex() == 0)
                    && ( (refImageRButton.isSelected()) || (averageImageRButton.isSelected()))) {
                graphCheckBox.setEnabled(true);
            } else {
                graphCheckBox.setEnabled(false);
                graphCheckBox.setSelected(false);
            }
        } else if (event.getSource() == fastModeCheckbox) {

            // enable or disable search variables
            fastMode = fastModeCheckbox.isSelected();
            rotateBeginText.setEnabled( !fastModeCheckbox.isSelected());
            ;
            rotateEndText.setEnabled( !fastModeCheckbox.isSelected());
            ;
            coarseRateText.setEnabled( !fastModeCheckbox.isSelected());
            ;
            fineRateText.setEnabled( !fastModeCheckbox.isSelected());
            ;
        }
    }

    /**
     * Accessor to set bracketBound.
     * 
     * @param bracketBound DOCUMENT ME!
     */
    public void setBracketBound(int bracketBound) {
        this.bracketBound = bracketBound;
    }

    /**
     * Accessor to set the coarse sample begin.
     * 
     * @param x Coarse begin
     */
    public void setCoarseBegin(float x) {
        rotateBegin = x;
    }

    /**
     * Accessor to set the coarse sample end.
     * 
     * @param x Coarse end
     */
    public void setCoarseEnd(float x) {
        rotateEnd = x;
    }

    /**
     * Accessor to set the coarse sample rate.
     * 
     * @param x Coarse rate
     */
    public void setCoarseRate(float x) {
        coarseRate = x;
    }

    /**
     * Accessor to set the choice of cost function.
     * 
     * @param x Cost function.
     */
    public void setCostChoice(int x) {
        cost = x;
    }

    /**
     * Accessor to set the degrees of freedom.
     * 
     * @param x Degrees of freedom
     */
    public void setDOF(int x) {
        DOF = x;
    }

    /**
     * Accessor to set whether or not to execute the fast mode (skip sub sample and goto last final optimization).
     * 
     * @param flag <code>true</code> then skip to level one (last ) optimization.
     */
    public void setFastMode(boolean flag) {
        fastMode = flag;
    }

    /**
     * Accessor to set the fine sample rate.
     * 
     * @param x Fine rate
     */
    public void setFineRate(float x) {
        fineRate = x;
    }

    /**
     * Accessor to set graphCheckBox.
     * 
     * @param doGraph if true output graphs of rotations and translations
     */
    public void setGraphCheckBox(boolean doGraph) {
        this.doGraph = doGraph;
    }

    /**
     * Accessor to set the input weight image.
     * 
     * @param im Input weight image.
     */
    public void setInputWeightImage(ModelImage im) {
        inputWeightImage = im;
    }

    /**
     * Accessor to set the initial interpolation.
     * 
     * @param x Interpolation
     */
    public void setInterp(int x) {
        interp = x;
    }

    /**
     * Accessor to set the final interpolation.
     * 
     * @param x Interpolation
     */
    public void setInterp2(int x) {
        interp2 = x;
    }

    /**
     * Accessor to set maxIterations.
     * 
     * @param maxIterations DOCUMENT ME!
     */
    public void setMaxIterations(int maxIterations) {
        this.maxIterations = maxIterations;
    }

    /**
     * Accessor to set numMinima.
     * 
     * @param numMinima DOCUMENT ME!
     */
    public void setNumMinima(int numMinima) {
        this.numMinima = numMinima;
    }

    /**
     * allows user to use an outside reference volume for registering.
     * 
     * @param refVolume (3-Dim reference volume)
     */
    public void setOutsideReferenceVolume(ModelImage refVolume) {
        this.refVolume = refVolume;
        this.useOutsideReferenceVolume = true;
    }

    /**
     * Accessor to set refImageNum.
     * 
     * @param refImageNumber number of reference slice
     */
    public void setRefImageNum(int refImageNumber) {
        refImageNum = refImageNumber;
    }

    /**
     * Accessor to set registerTo.
     * 
     * @param registerTo - 1 = adjacent, 2 = average, 3 = reference
     */
    public void setRegisterTo(int registerTo) {
        this.registerTo = registerTo;
    }

    /**
     * Accessor to set whether or not subsampling occurs.
     * 
     * @param doSubsample DOCUMENT ME!
     */
    public void setSubsample(boolean doSubsample) {
        this.doSubsample = doSubsample;
    }

    /**
     * Accessor to set the VOIs only flag.
     * 
     * @param flag <code>true</code> then only register the parts of the images in the VOIs.
     */
    public void setVoisOnly(boolean flag) {
        voisOnly = flag;
    }

    /**
     * Accessor to set the weighted images flag.
     * 
     * @param flag <code>true</code> means there are weighted images.
     */
    public void setWeighted(boolean flag) {
        weighted = flag;
    }

    /**
     * Calls the algorithm with the set-up parameters.
     */
    protected void callAlgorithm() {
        BitSet mask = null;
        matchImage = pipeline.DWIImage;

        if (voisOnly) {
            float[] matchRes = new float[] {matchImage.getFileInfo(0).getResolutions()[0],
                    matchImage.getFileInfo(0).getResolutions()[1], matchImage.getFileInfo(0).getResolutions()[2],
                    matchImage.getFileInfo(0).getResolutions()[3]};

            inputWeightImage = new ModelImage(ModelStorageBase.BYTE, matchImage.getExtents(), "VOI match");

            inputWeightImage.getFileInfo(0).setResolutions(matchRes);
            // make new input image based on the VOIs. pass those new image to the registration algorithm

            mask = matchImage.generateVOIMask();

            int matchImageSize = matchImage.getSliceSize() * matchImage.getExtents()[2];

            for (int j = 0; j < matchImage.getExtents()[3]; j++) {

                for (int i = 0; i < matchImageSize; i++) {

                    if ( !mask.get(i)) {
                        inputWeightImage.set( (j * matchImageSize) + i, 0);
                    } else {
                        inputWeightImage.set( (j * matchImageSize) + i, 1);
                    }
                }
            }

            weighted = true;

        } // if (voisOnly)

        if (weighted) {
            reg35 = new AlgorithmRegOAR35D(matchImage, inputWeightImage, cost, DOF, interp, interp2, registerTo,
                    refImageNum, rotateBegin, rotateEnd, coarseRate, fineRate, doGraph, doSubsample, fastMode,
                    bracketBound, maxIterations, numMinima);
        } else {

            reg35 = new AlgorithmRegOAR35D(matchImage, cost, DOF, interp, interp2, registerTo, refImageNum,
                    rotateBegin, rotateEnd, coarseRate, fineRate, doGraph, doSubsample, fastMode, bracketBound,
                    maxIterations, numMinima);

            if (useOutsideReferenceVolume) {

                if ( !reg35.setReferenceVolume(refVolume)) {
                    MipavUtil.displayError("Reference volume does not have same extents as input image");
                }
            }

        }

        // Start the thread as a low priority because we wish to still have user interface work fast.
        reg35.addListener(this);

        // Hide dialog
        setVisible(false);

        reg35.run();
    }

    private TitledBorder buildTitledBorder(String title) {
        return new TitledBorder(new EtchedBorder(), title, TitledBorder.LEFT, TitledBorder.CENTER, MipavUtil.font12B,
                Color.black);
    }

    /**
     * Initializes the GUI components and displays the dialog.
     */
    private void init() {
        setForeground(Color.black);

        JPanel optPanel = new JPanel();
        optPanel.setLayout(new GridBagLayout());
        optPanel.setBorder(buildTitledBorder("Input Options"));

        // String matchName = matchImage.getImageName();
        // JLabel labelImage = new JLabel("Internal Registration for " + matchName);
        

        
        JLabel labelImage = new JLabel("Internal Registration for ");
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
        //comboBoxDOF.addItem("Rigid - 6");
        comboBoxDOF.addItem("Motion Correction");
        comboBoxDOF.addItem("Global rescale - 7");
        comboBoxDOF.addItem("Specific rescale - 9");
        //comboBoxDOF.addItem("Affine - 12");
        comboBoxDOF.addItem("Motion Correction + Eddy Current");
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

        if ( !doColor) {
            comboBoxCostFunct.addItem("Correlation ratio");
        }

        comboBoxCostFunct.addItem("Least squares");

        if ( !doColor) {
            comboBoxCostFunct.addItem("Normalized cross correlation");
            comboBoxCostFunct.addItem("Normalized mutual information");
        }

        // This is least squares if doColor, else correlation ratio
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

        // Rotation Range Panel
        JPanel rotateRangePanel = new JPanel(new FlowLayout(FlowLayout.LEFT));

        JLabel labelRotateRange = new JLabel("Rotation angle sampling range:");
        labelRotateRange.setForeground(Color.black);
        labelRotateRange.setFont(serif12);

        JLabel labelRotateRangeTo = new JLabel("to");
        labelRotateRangeTo.setForeground(Color.black);
        labelRotateRangeTo.setFont(serif12);

        JLabel labelRotateDegrees = new JLabel("degrees");
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

        JLabel labelCoarse = new JLabel("Coarse angle increment: ");
        labelCoarse.setForeground(Color.black);
        labelCoarse.setFont(serif12);
        labelCoarse.setAlignmentX(Component.LEFT_ALIGNMENT);

        JLabel labelCoarseDegrees = new JLabel("degrees");
        labelCoarseDegrees.setFont(serif12);
        coarseRateText = new JTextField("15", 3);

        coarsePanel.add(labelCoarse);
        coarsePanel.add(coarseRateText);
        coarsePanel.add(labelCoarseDegrees);
        coarsePanel.setAlignmentX(Component.LEFT_ALIGNMENT);

        // Fine sampling rate panel
        JPanel finePanel = new JPanel(new FlowLayout(FlowLayout.LEFT));

        JLabel labelFine = new JLabel("Fine angle increment:");
        labelFine.setForeground(Color.black);
        labelFine.setFont(serif12);
        labelFine.setAlignmentX(Component.LEFT_ALIGNMENT);

        JLabel labelFineDegrees = new JLabel("degrees");
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

        gbc.weightx = 0;
        gbc.gridwidth = 3;
        gbc.gridx = 0;
        gbc.gridy = 4;
        optPanel.add(rotateRangePanel, gbc);

        gbc.gridx = 0;
        gbc.gridy = 5;
        optPanel.add(coarsePanel, gbc);

        gbc.gridx = 0;
        gbc.gridy = 6;
        optPanel.add(finePanel, gbc);

        gbc.gridx = 0;
        gbc.gridy = 7;
        gbc.weightx = 1;
        gbc.gridwidth = 1;
        optPanel.add(sampleCheckBox, gbc);

        // Reference image panel
        JPanel refPanel = new JPanel(new GridBagLayout());
        refPanel.setBorder(buildTitledBorder("Reference image"));
        
        /*t2FileLabel = new JLabel("Upload T2 Image: ");
        t2FileLabel.setEnabled(true);
        gbc.gridx = 0;
        gbc.gridy = 0;
        gbc.gridwidth = 1;
        gbc.weightx = 0;
        refPanel.add(t2FileLabel, gbc);
        
        
        t2Button = new JButton("Browse");
        t2Button.addActionListener(this);
        t2Button.setActionCommand("browseT2File");
        t2Button.setEnabled(true);
        gbc.gridx = 1;
        gbc.gridy = 0;
        refPanel.add(t2Button, gbc);*/
        


        // JLabel labelInternal = new JLabel("Reference volume (0-" + String.valueOf(matchImage.getExtents()[3]-1) +
        // ")");
        JLabel labelInternal = new JLabel("Reference Volume Number");
        labelInternal.setForeground(Color.black);
        labelInternal.setFont(serif12);
        gbc.gridx = 0;
        gbc.gridy = 0;
        gbc.gridwidth = 1;
        gbc.weightx = 0;
        refPanel.add(labelInternal, gbc);

        // refImageNumText = new JTextField(String.valueOf((matchImage.getExtents()[3] / 2)), 3);
        refImageNumText = new JTextField("0", 2);
        refImageNumText.setEnabled(true);
        gbc.gridx = 1;
        gbc.gridy = 0;
        gbc.gridwidth = GridBagConstraints.REMAINDER;
        gbc.weightx = 1;
        refPanel.add(refImageNumText, gbc);

        // Adjacent, average, or reference radio buttons
        ButtonGroup group1 = new ButtonGroup();

        /*adjacentImageRButton = new JRadioButton("Register to adjacent volume", true);
        adjacentImageRButton.setFont(serif12);
        adjacentImageRButton.addItemListener(this);
        group1.add(adjacentImageRButton);

        averageImageRButton = new JRadioButton("Register to average volume", false);
        averageImageRButton.setFont(serif12);
        averageImageRButton.addItemListener(this);
        group1.add(averageImageRButton);*/

        refImageRButton = new JRadioButton("Register to reference volume", true);
        refImageRButton.setFont(serif12);
        refImageRButton.addItemListener(this);
        group1.add(refImageRButton);

        /*gbc.gridx = 0;
        gbc.gridy = 1;
        refPanel.add(adjacentImageRButton, gbc);
        gbc.gridx = 0;
        refPanel.add(averageImageRButton, gbc);*/
        gbc.gridx = 0;
        gbc.gridy = 1;
        refPanel.add(refImageRButton, gbc);

        ButtonGroup weightGroup = new ButtonGroup();

        noneRadio = new JRadioButton("No weight");
        noneRadio.setFont(serif12);
        noneRadio.setForeground(Color.black);
        noneRadio.setSelected(true);
        // noneRadio.setActionCommand("NoneRadio");
        noneRadio.addItemListener(this);
        weightGroup.add(noneRadio);

        voiRadio = new JRadioButton("Register area delineated by VOIs only");
        voiRadio.setFont(serif12);
        voiRadio.setForeground(Color.black);
        voiRadio.setSelected(false);
        voiRadio.addItemListener(this);
        voiRadio.setEnabled(false);

        /*
         * ViewVOIVector VOIs = matchImage.getVOIs();
         * 
         * if ((VOIs != null) && (VOIs.size() > 0)) { voiRadio.setEnabled(true); }
         */

        weightGroup.add(voiRadio);

        weightRadio = new JRadioButton("Weight registration");
        weightRadio.setFont(serif12);
        weightRadio.setForeground(Color.black);
        weightRadio.setSelected(false);
        // weightRadio.setActionCommand("WeightRadio");
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

        comboBoxInterp2.addItem("Trilinear");
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
        graphCheckBox.setEnabled(false);
        
        epiCheckBox = new JCheckBox("Perform EPI Distortion Correction");
        epiCheckBox.setFont(serif12);
        epiCheckBox.setForeground(Color.black);
        epiCheckBox.setActionCommand("EPI");
        epiCheckBox.setSelected(false);
        epiCheckBox.setEnabled(false);
        epiCheckBox.addActionListener(this);
        



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
        
        gbc.gridx = 0;
        gbc.gridy = 2;
        gbc.weightx = 1;
        gbc.fill = GridBagConstraints.HORIZONTAL;
        outPanel.add(epiCheckBox, gbc);


        settingsPanel = new JPanel(new GridBagLayout());
        settingsPanel.setBorder(buildTitledBorder("Optimization settings"));

        advancedBox = new JCheckBox("Use Advanced Settings");
        advancedBox.setActionCommand("AdvancedSettings");
        advancedBox.setSelected(false);
        advancedBox.setEnabled(true);
        advancedBox.addActionListener(this);
        advancedBox.setFont(serif12B);

        JPanel bracketPanel = new JPanel(new GridBagLayout());
        bracketBoundLabel = new JLabel("Multiple of tolerance to bracket the minimum (10-60): ");
        bracketBoundLabel.setForeground(Color.lightGray);
        gbc.gridx = 0;
        gbc.gridy = 0;
        bracketPanel.add(bracketBoundLabel, gbc);
        bracketPanel.setToolTipText("Used for translation, scale and skew.");
        bracketBoundText = new JTextField(String.valueOf(bracketBound), 3);
        bracketBoundText.addActionListener(this);
        bracketBoundText.setEnabled(false);
        gbc.gridx = 1;
        gbc.gridy = 0;
        bracketPanel.add(bracketBoundText, gbc);

        JPanel maxIterPanel = new JPanel(new GridBagLayout());
        maxIterationsLabel = new JLabel("Number of iterations (1-5): ");
        maxIterationsLabel.setForeground(Color.lightGray);
        gbc.gridx = 0;
        gbc.gridy = 1;
        maxIterPanel.add(maxIterationsLabel, gbc);
        maxIterPanel.setToolTipText("Used for levelOne. Other levels are multiples of this #.");
        maxIterationsText = new JTextField(String.valueOf(10), 3);
        maxIterationsText.addActionListener(this);
        maxIterationsText.setEnabled(false);
        gbc.gridx = 1;
        gbc.gridy = 1;
        maxIterPanel.add(maxIterationsText, gbc);

        JPanel numMinPanel = new JPanel(new GridBagLayout());
        numMinLabel = new JLabel("Number of minima from Level 8 to test at Level 4: ");
        numMinLabel.setForeground(Color.lightGray);
        gbc.gridx = 0;
        gbc.gridy = 2;
        numMinPanel.add(numMinLabel, gbc);
        numMinPanel.setToolTipText("Increasing will significantly increase processing time.");
        numMinText = new JTextField(String.valueOf(numMinima), 3);
        numMinText.addActionListener(this);
        numMinText.setEnabled(false);
        gbc.gridx = 1;
        gbc.gridy = 2;
        numMinPanel.add(numMinText, gbc);

        fastModeCheckbox = new JCheckBox("Skip multilevel search.  Assume images are close to alignment.");
        fastModeCheckbox.setFont(serif12);
        fastModeCheckbox.setForeground(Color.black);
        fastModeCheckbox.setSelected(false);
        fastModeCheckbox.setEnabled(false);
        fastModeCheckbox.addActionListener(this);

        gbc.gridx = 0;
        gbc.gridy = 0;
        settingsPanel.add(advancedBox, gbc);
        gbc.gridx = 0;
        gbc.gridy = 1;
        settingsPanel.add(bracketPanel, gbc);
        gbc.gridx = 0;
        gbc.gridy = 2;
        settingsPanel.add(maxIterPanel, gbc);
        gbc.gridx = 0;
        gbc.gridy = 3;
        settingsPanel.add(numMinPanel, gbc);
        gbc.gridx = 0;
        gbc.gridy = 4;
        settingsPanel.add(fastModeCheckbox, gbc);

        JPanel buttonPanel = new JPanel();
        buildOKButton();
        buttonPanel.add(OKButton);
        buildCancelButton();
        buttonPanel.add(cancelButton);
        buildHelpButton();
        buttonPanel.add(helpButton);

        mainRegPanel = new JPanel();
        mainRegPanel.setLayout(new GridBagLayout());
        gbc.anchor = GridBagConstraints.NORTHWEST;
        gbc.gridx = 0;
        gbc.gridy = 0;
        gbc.gridwidth = 1;
        gbc.weightx = .5;
        gbc.weighty = 1;
        mainRegPanel.add(optPanel, gbc);
        gbc.gridx = 0;
        gbc.gridy = 1;
        gbc.gridwidth = 1;
        gbc.weightx = .5;
        gbc.weighty = 1;
        mainRegPanel.add(refPanel, gbc);
        gbc.gridx = 0;
        gbc.gridy = 2;
        gbc.gridwidth = 1;
        gbc.weightx = .5;
        gbc.weighty = 1;
        mainRegPanel.add(weightPanel, gbc);
        gbc.gridx = 0;
        gbc.gridy = 3;
        gbc.gridwidth = 1;
        gbc.weightx = .5;
        gbc.weighty = 1;
        mainRegPanel.add(outPanel, gbc);

        gbc.gridx = 0;
        gbc.gridy = 4;
        gbc.gridwidth = 1;
        gbc.weightx = .5;
        gbc.weighty = 1;
        mainRegPanel.add(buttonPanel, gbc);

        gbc.anchor = GridBagConstraints.NORTHWEST;
        gbc.gridx = 1;
        gbc.gridy = 0;
        gbc.weightx = .5;
        gbc.weighty = 1;
        mainRegPanel.add(settingsPanel, gbc);

        // this.add(mainPanel);
        /*
         * getContentPane().add(mainPanel); getContentPane().add(buttonPanel, BorderLayout.SOUTH); pack();
         */
        setVisible(true);
    }

    private JButton buildOKButton() {
        OKButton = new JButton("OK");
        OKButton.addActionListener(this);

        // OKButton.setToolTipText("Accept values and perform action.");
        OKButton.setMinimumSize(MipavUtil.defaultButtonSize);
        OKButton.setPreferredSize(MipavUtil.defaultButtonSize);
        OKButton.setFont(serif12B);

        return OKButton;
    }

    private JButton buildCancelButton() {
        cancelButton = new JButton("Cancel");
        cancelButton.addActionListener(this);

        // OKButton.setToolTipText("Accept values and perform action.");
        cancelButton.setMinimumSize(MipavUtil.defaultButtonSize);
        cancelButton.setPreferredSize(MipavUtil.defaultButtonSize);
        cancelButton.setFont(serif12B);

        return cancelButton;
    }

    private JButton buildHelpButton() {
        helpButton = new JButton("Help");
        helpButton.addActionListener(this);

        // OKButton.setToolTipText("Accept values and perform action.");
        helpButton.setMinimumSize(MipavUtil.defaultButtonSize);
        helpButton.setPreferredSize(MipavUtil.defaultButtonSize);
        helpButton.setFont(serif12B);

        return helpButton;
    }

    /**
     * Sets the variables needed to call the registration algorithm based on the values entered in the dialog.
     * 
     * @return <code>true</code> if the variables are properly set, <code>false</code> otherwise.
     */
    private boolean setVariables() {
        int i;
        int nVOI;
        weighted = weightRadio.isSelected();
        voisOnly = voiRadio.isSelected();
        matchImage = pipeline.DWIImage;

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

                for (i = 0; i < matchImage.getNDims(); i++) {

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

            if ( ( !weighted) && ( !voisOnly)) {

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

            if ( ( !weighted) && ( !voisOnly)) {

                switch (comboBoxCostFunct.getSelectedIndex()) {

                    case 0:
                        cost = AlgorithmCostFunctions.CORRELATION_RATIO_SMOOTHED;
                        break;

                    case 1:
                        cost = AlgorithmCostFunctions.LEAST_SQUARES_SMOOTHED;
                        break;
                    // case 2: cost = AlgorithmCostFunctions.MUTUAL_INFORMATION_SMOOTHED; break;

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
                    // case 2: cost = AlgorithmCostFunctions.MUTUAL_INFORMATION_SMOOTHED_WGT; break;

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

        if ( !MipavUtil.testParameter(rotateBeginText.getText(), -360, 360)) {
            rotateBeginText.requestFocus();
            rotateBeginText.selectAll();

            return false;
        } else {
            rotateBegin = Float.valueOf(rotateBeginText.getText()).floatValue();
        }

        if ( !MipavUtil.testParameter(rotateEndText.getText(), -360, 360)) {
            rotateEndText.requestFocus();
            rotateEndText.selectAll();

            return false;
        } else {
            rotateEnd = Float.valueOf(rotateEndText.getText()).floatValue();
        }

        if ( !MipavUtil.testParameter(coarseRateText.getText(), 0.01, 360)) {
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

        if ( ( (rotateEnd - rotateBegin) / coarseRate) < 1) {
            int response = JOptionPane.showConfirmDialog(this,
                    "Warning: with such a large rate, there will only be 1 sampling.  Continue?", "Sampling warning",
                    JOptionPane.YES_NO_OPTION, JOptionPane.WARNING_MESSAGE);

            if (response == JOptionPane.NO_OPTION) {
                coarseRateText.requestFocus();
                coarseRateText.selectAll();

                return false;
            }
        }

        if ( !MipavUtil.testParameter(rotateBeginText.getText(), -360, 360)) {
            rotateBeginText.requestFocus();
            rotateBeginText.selectAll();

            return false;
        } else {
            rotateBegin = Float.valueOf(rotateBeginText.getText()).floatValue();
        }

        if ( !MipavUtil.testParameter(rotateEndText.getText(), -360, 360)) {
            rotateEndText.requestFocus();
            rotateEndText.selectAll();

            return false;
        } else {
            rotateEnd = Float.valueOf(rotateEndText.getText()).floatValue();
        }

        if ( !MipavUtil.testParameter(fineRateText.getText(), 0.01, 360)) {
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

        if ( ( (rotateEnd - rotateBegin) / fineRate) < 1) {
            int response = JOptionPane.showConfirmDialog(this,
                    "Warning: with such a large rate, there will only be 1 sampling.  Continue?", "Sampling warning",
                    JOptionPane.YES_NO_OPTION, JOptionPane.WARNING_MESSAGE);

            if (response == JOptionPane.NO_OPTION) {
                coarseRateText.requestFocus();
                coarseRateText.selectAll();

                return false;
            }
        }

        if ( !MipavUtil.testParameter(refImageNumText.getText(), 0, matchImage.getExtents()[3] - 1)) {
            refImageNumText.requestFocus();
            refImageNumText.selectAll();

            return false;
        } else {
            refImageNum = Integer.valueOf(refImageNumText.getText()).intValue();
        }

        if (adjacentImageRButton.isSelected()) {
            registerTo = 1;
        } else if (averageImageRButton.isSelected()) {
            registerTo = 2;
        } else { // reference
            registerTo = 3;
        }

        doGraph = graphCheckBox.isSelected();

        if (voisOnly) {

            // check that there actually are VOIs there
            // and propagate the VOIs to all slices
            ViewVOIVector VOIs = (ViewVOIVector) matchImage.getVOIs();
            nVOI = VOIs.size();

            if (nVOI < 1) {
                MipavUtil.displayError("There must be at least one VOI in " + matchImage.getImageName()
                        + " to register.");

                return false;
            }
        } // if (voisOnly)

        doSubsample = sampleCheckBox.isSelected();

        return true;
    }

}
