package gov.nih.mipav.view.renderer.WildMagic.DTI_FrameWork;


import gov.nih.mipav.model.algorithms.AlgorithmBase;
import gov.nih.mipav.model.algorithms.AlgorithmCostFunctions;
import gov.nih.mipav.model.algorithms.AlgorithmInterface;
import gov.nih.mipav.model.algorithms.AlgorithmTransform;
import gov.nih.mipav.model.algorithms.registration.AlgorithmRegOAR35D;
import gov.nih.mipav.model.algorithms.registration.AlgorithmRegOAR3D;
import gov.nih.mipav.model.algorithms.utilities.AlgorithmInsertVolume;
import gov.nih.mipav.model.algorithms.utilities.AlgorithmRemoveTSlices;
import gov.nih.mipav.model.algorithms.utilities.AlgorithmSubset;
import gov.nih.mipav.model.file.DTIParameters;
import gov.nih.mipav.model.file.FileInfoBase;
import gov.nih.mipav.model.structures.ModelImage;
import gov.nih.mipav.model.structures.TransMatrix;

import gov.nih.mipav.view.MipavUtil;
import gov.nih.mipav.view.Preferences;
import gov.nih.mipav.view.ViewJFrameImage;
import gov.nih.mipav.view.ViewUserInterface;
import gov.nih.mipav.view.dialogs.JDialogBase;

import java.awt.Color;
import java.awt.Component;
import java.awt.Dimension;
import java.awt.Font;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.Insets;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.ItemEvent;
import java.awt.event.ItemListener;
import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.PrintStream;
import java.text.DecimalFormat;
import java.text.DecimalFormatSymbols;

import javax.swing.JButton;
import javax.swing.JCheckBox;
import javax.swing.JComboBox;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JTextField;
import javax.swing.border.EtchedBorder;
import javax.swing.border.TitledBorder;

import Jama.Matrix;


public class JPanelDTIPreprocessing extends JPanel implements AlgorithmInterface, ActionListener, ItemListener {

    // ~ Static fields/initializers
    // -------------------------------------------------------------------------------------

    /** Use serialVersionUID for interoperability. */
    private static final long serialVersionUID = -4309868934393418962L;

    // ~ Instance fields
    // ------------------------------------------------------------------------------------------------

    private DTIPipeline pipeline;

    /** DOCUMENT ME! */
    private ViewUserInterface UI;

    /** DOCUMENT ME! */
    private ModelImage refT2image;

    /** DOCUMENT ME! */
    private ModelImage resultB0toT2Image;

    /** DOCUMENT ME! */
    private ModelImage matchB0image; // register match image to reference Image

    /** DOCUMENT ME! */
    private ModelImage matchDWIImage;

    public ModelImage dwi35RegImage = null;

    public ModelImage result35RegImage;

    /** grid bag constraints * */
    private GridBagConstraints gbc;

    private Font serif12;

    private Font serif12B;

    /** DOCUMENT ME! */
    private JTextField refImageNumText;

    /** DOCUMENT ME! */
    private JComboBox comboBoxDOF;

    /** DOCUMENT ME! */
    private JComboBox comboBoxInterp;

    /** DOCUMENT ME! */
    private JComboBox comboBoxCostFunct;

    /** DOCUMENT ME! */
    private JCheckBox transformB0Checkbox;

    /** DOCUMENT ME! */
    private JCheckBox transformDWICheckbox;

    /** DOCUMENT ME! */
    public JComboBox matrixComboBox;

    /** DOCUMENT ME! */
    public JTextField matrixDirText;

    private JButton OKButton;

    private JButton cancelButton;

    private JButton helpButton;

    public JPanel mainPrePanel;

    /** DOCUMENT ME! */
    private AlgorithmRegOAR3D reg3 = null;

    /** DOCUMENT ME! */
    private AlgorithmRegOAR35D reg35 = null;

    /** DOCUMENT ME! */
    private int cost, interp, DOF;

    /** DOCUMENT ME! */
    private int costT2, interpT2, DOFT2;

    private int DOFReg35;

    private String matrixDirectory;

    private float rotateBeginX, rotateEndX, coarseRateX, fineRateX, rotateBeginY, rotateEndY, coarseRateY, fineRateY,
            rotateBeginZ, rotateEndZ, coarseRateZ, fineRateZ;

    private float rotateBegin, rotateEnd, coarseRate, fineRate;

    private boolean maxOfMinResol = true, doSubsample = true, doMultiThread = true, fastMode = false;

    private boolean doGraph = false;

    private int bracketBound = 10, maxIterations = 2, numMinima = 3;

    int registerTo = 3;

    /** DOCUMENT ME! */

    private int[] destB0Extents;

    private int sliceNum;

    private String resultB0String;

    private ModelImage resultB0Image = null;

    private ModelImage srcB0Image;

    private AlgorithmSubset subsetAlgo;

    private boolean[] tVolumeRemove;

    private AlgorithmRemoveTSlices removeTSlicesAlgo;

    private AlgorithmInsertVolume insertVolumeAlgo;

    /** DOCUMENT ME! */
    private ModelImage resultB0RemoveImage = null;

    /** DOCUMENT ME! */
    public ModelImage newB0DWIRegImage = null;

    private String resultB0RemoveString;

    private int[] destB0RemoveExtents;

    private String newB0DWIRegString;

    private int[] newB0DWIRegExtents;

    private int[] epiExtents;

    public TransMatrix[] arrayTransMatrix;

    public TransMatrix b0toStructMatrix;

    private ModelImage[] EPI4dto3dArray;

    private ModelImage EPI4dto3dVolume;

    private DTIParameters dtiRegParams;

    public JPanelDTIPreprocessing(DTIPipeline pipeline) {
        super();
        // super(theParentFrame, false);
        // super();
        // matchB0image = im;

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
            if (pipeline.T2Image != null) {
                System.out.println("t2image");
                callT2Algorithm();
                setVariables();
                callReg35Algorithm();
            } else {
                setVariables();
                callReg35Algorithm();
            }

            if (transformB0Checkbox.isSelected()) {
                if (resultB0toT2Image != null) {
                    try {
                        new ViewJFrameImage(resultB0toT2Image, null, new Dimension(610, 200));
                    } catch (final OutOfMemoryError error) {
                        MipavUtil.displayError("Out of memory: unable to open new frame");
                    }
                }
            }

            if (transformDWICheckbox.isSelected()) {
                if (result35RegImage != null) {
                    try {
                        new ViewJFrameImage(result35RegImage, null, new Dimension(610, 200));

                        pipeline.nextButton.setEnabled(true);
                        pipeline.nextButton.setActionCommand("next2");
                    } catch (final OutOfMemoryError error) {
                        MipavUtil.displayError("Out of memory: unable to open new frame");
                    }
                }

            }

        } else if (command.equals("Cancel")) {

        } else if (command.equals("Help")) {
            MipavUtil.showHelp("OAR19076");

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

        AlgorithmTransform transform = null;
        final boolean pad = false;
        double xOrig;
        double yOrig;
        double zOrig;
        double xCen;
        double yCen;
        double zCen;
        double xCenNew;
        double yCenNew;
        double zCenNew;
        float resX;
        float resY;
        float resZ;
        String comStr;
        DecimalFormat nf;
        final ViewUserInterface UI = ViewUserInterface.getReference();

        nf = new DecimalFormat();
        nf.setMaximumFractionDigits(4);
        nf.setMinimumFractionDigits(0);
        nf.setGroupingUsed(false);

        final DecimalFormatSymbols dfs = nf.getDecimalFormatSymbols();
        dfs.setDecimalSeparator('.');
        nf.setDecimalFormatSymbols(dfs);

        if (algorithm instanceof AlgorithmRegOAR3D) {
            if (reg3.isCompleted()) {
                //System.out.println("reg3 completed");

                matrixDirectory = (String) matrixComboBox.getSelectedItem();

                b0toStructMatrix = reg3.getTransform();
                //System.out.println("test: " + b0toStructMatrix);

                if (transformB0Checkbox.isSelected()) {
                    final int xdimA = refT2image.getExtents()[0];
                    final int ydimA = refT2image.getExtents()[1];
                    final int zdimA = refT2image.getExtents()[2];
                    final float xresA = refT2image.getFileInfo(0).getResolutions()[0];
                    final float yresA = refT2image.getFileInfo(0).getResolutions()[1];
                    final float zresA = refT2image.getFileInfo(0).getResolutions()[2];

                    final String name = JDialogBase.makeImageName(matchB0image.getImageName(), "_RegisteredB0toT2");

                    transform = new AlgorithmTransform(matchB0image, b0toStructMatrix, 0, xresA, yresA, zresA, xdimA,
                            ydimA, zdimA, true, false, pad);

                    transform.setUpdateOriginFlag(true);
                    transform.setFillValue(Float.valueOf("0.0"));
                    transform.run();
                    resultB0toT2Image = transform.getTransformedImage();
                    transform.finalize();

                    resultB0toT2Image.calcMinMax();
                    resultB0toT2Image.setImageName(name);

                    if (resultB0toT2Image != null) {
                        // Removes Old B0 from 4d DWI dataset
                        tVolumeRemove = new boolean[srcB0Image.getExtents()[3]];

                        for (int i = 0; i < srcB0Image.getExtents()[3] - 1; i++) {

                            if (i == sliceNum) {// Finds volume based on user specified to remove
                                tVolumeRemove[i] = true;
                            } else {
                                tVolumeRemove[i] = false;
                            }
                        }
                        destB0RemoveExtents = new int[4];
                        destB0RemoveExtents[0] = srcB0Image.getExtents()[0];
                        destB0RemoveExtents[1] = srcB0Image.getExtents()[1];
                        destB0RemoveExtents[2] = srcB0Image.getExtents()[2];
                        destB0RemoveExtents[3] = srcB0Image.getExtents()[3] - 1;
                        resultB0RemoveString = srcB0Image.getImageName() + "Remove T Voume ="
                                + refImageNumText.getText();
                        resultB0RemoveImage = new ModelImage(srcB0Image.getType(), destB0RemoveExtents,
                                resultB0RemoveString);

                        removeTSlicesAlgo = new AlgorithmRemoveTSlices(srcB0Image, resultB0RemoveImage, tVolumeRemove);
                        // createProgressBar(resultB0RemoveString, removeTSlicesAlgo);
                        removeTSlicesAlgo.run();

                        if ( (removeTSlicesAlgo.isCompleted() == true) && (resultB0RemoveImage != null)) {
                            // Inserts new registered B0 to T2 into 4D DWI dataset
                            newB0DWIRegExtents = new int[4];
                            newB0DWIRegExtents[0] = srcB0Image.getExtents()[0];
                            newB0DWIRegExtents[1] = srcB0Image.getExtents()[1];
                            newB0DWIRegExtents[2] = srcB0Image.getExtents()[2];
                            newB0DWIRegExtents[3] = srcB0Image.getExtents()[3];
                            newB0DWIRegString = srcB0Image.getImageName() + "NewB0&DWIDataset";
                            newB0DWIRegImage = new ModelImage(srcB0Image.getType(), newB0DWIRegExtents,
                                    newB0DWIRegString);
                            insertVolumeAlgo = new AlgorithmInsertVolume(resultB0RemoveImage, newB0DWIRegImage, 3,
                                    sliceNum, resultB0toT2Image);
                            insertVolumeAlgo.run();
                        }

                        /*
                         * if ( (insertVolumeAlgo.isCompleted() == true) && (newB0DWIRegImage != null)) { try { new
                         * ViewJFrameImage(newB0DWIRegImage, null, new Dimension(610, 200)); } catch (final
                         * OutOfMemoryError error) { MipavUtil.displayError("Out of memory: unable to open new frame");
                         * } }
                         */
                    } else {
                        MipavUtil.displayError("Result Image is null");
                    }

                    if (transform != null) {
                        transform.disposeLocal();
                        transform = null;
                    }
                }

                xOrig = (matchB0image.getExtents()[0] - 1.0) / 2.0;
                yOrig = (matchB0image.getExtents()[1] - 1.0) / 2.0;
                zOrig = (matchB0image.getExtents()[2] - 1.0) / 2.0;
                resX = matchB0image.getFileInfo()[0].getResolutions()[0];
                resY = matchB0image.getFileInfo()[0].getResolutions()[1];
                resZ = matchB0image.getFileInfo()[0].getResolutions()[2];
                xCen = xOrig * resX;
                yCen = yOrig * resY;
                zCen = zOrig * resZ;
                b0toStructMatrix.Inverse();
                xCenNew = xCen * b0toStructMatrix.Get(0, 0) + yCen * b0toStructMatrix.Get(0, 1) + zCen
                        * b0toStructMatrix.Get(0, 2) + b0toStructMatrix.Get(0, 3);
                yCenNew = xCen * b0toStructMatrix.Get(1, 0) + yCen * b0toStructMatrix.Get(1, 1) + zCen
                        * b0toStructMatrix.Get(1, 2) + b0toStructMatrix.Get(1, 3);
                zCenNew = xCen * b0toStructMatrix.Get(2, 0) + yCen * b0toStructMatrix.Get(2, 1) + zCen
                        * b0toStructMatrix.Get(2, 2) + b0toStructMatrix.Get(2, 3);
                Preferences.debug("The geometric center of " + matchB0image.getImageName() + " at (" + xCen + ", "
                        + yCen + ", " + zCen + ")\n", Preferences.DEBUG_ALGORITHM);
                if (resultB0toT2Image != null) {
                    comStr = "moves to (" + nf.format(xCenNew) + ", " + nf.format(yCenNew) + ", " + nf.format(zCenNew)
                            + ") in " + resultB0toT2Image.getImageName() + ".\n";
                } else {
                    comStr = "moves to (" + nf.format(xCenNew) + ", " + nf.format(yCenNew) + ", " + nf.format(zCenNew)
                            + ").\n";
                }
                Preferences.debug(comStr, Preferences.DEBUG_ALGORITHM);

                if (resultB0toT2Image != null) {
                    resultB0toT2Image.getMatrixHolder().replaceMatrices(refT2image.getMatrixHolder().getMatrices());

                    for (int i = 0; i < resultB0toT2Image.getExtents()[2]; i++) {
                        resultB0toT2Image.getFileInfo(i).setOrigin(refT2image.getFileInfo(i).getOrigin());
                    }
                }

                b0toStructMatrix.setTransformID(TransMatrix.TRANSFORM_ANOTHER_DATASET);
                matchB0image.getMatrixHolder().addMatrix(b0toStructMatrix);

                String message = "Using cost function, " + "Correlation ration";
                message += ", the cost is " + Double.toString(reg3.getAnswer()) + ".\n";
                message += "Some registration settings: \n";
                message += "X Rotations from " + rotateBeginX + " to " + rotateEndX + ", ";
                message += "with a X coarse rate of " + coarseRateX + " and X fine rate of " + fineRateX + ".\n";
                message += "Y Rotations from " + rotateBeginY + " to " + rotateEndY + ", ";
                message += "with a Y coarse rate of " + coarseRateY + " and Y fine rate of " + fineRateY + ".\n";
                message += "Z Rotations from " + rotateBeginZ + " to " + rotateEndZ + ", ";
                message += "with a Z coarse rate of " + coarseRateZ + " and Z fine rate of " + fineRateZ + ".\n";
                b0toStructMatrix.saveMatrix(matrixDirectory + File.separator + matchB0image.getImageName() + "_To_"
                        + refT2image.getImageName() + ".mtx", message);
                Preferences.debug("Saved " + matrixDirectory + File.separator + matchB0image.getImageName() + "_To_"
                        + refT2image.getImageName() + ".mtx\n", Preferences.DEBUG_FILEIO);

            }

            if (reg3 != null) {
                reg3.disposeLocal();
                reg3 = null;
            }

            matchB0image = null; // register match image to reference Image
            refT2image = null;
        }
        if (algorithm instanceof AlgorithmRegOAR35D) {
            matrixDirectory = (String) matrixComboBox.getSelectedItem();
            final TransMatrix finalMatrix = reg35.getTransform();
            arrayTransMatrix = reg35.getArrayTransMatrix();
            //Testing
            /*for (int i = 0; i < arrayTransMatrix.length; i++) {
                System.out.println("TestarrayTransMatrix: " + arrayTransMatrix[i]);
            }*/
            JTextField[][] textMatrix = new JTextField[4][4];

            result35RegImage = reg35.getTransformedImage();
            if (result35RegImage != null) {
                result35RegImage.calcMinMax();
                result35RegImage.setImageName(pipeline.DWIImage.getImageName() + comboBoxDOF.getSelectedItem());
                createArrayTransMatrixTXT();
                dtiRegParams = new DTIParameters(result35RegImage.getExtents()[3]);
                dtiRegParams = pipeline.DWIImage.getDTIParameters();
                //Testing
                /*System.out.println("dtiRegparamsvol: " + dtiRegParams.getNumVolumes());
                if (dtiRegParams.getNumVolumes() == result35RegImage.getExtents()[3]) {
                    System.out.println("dtiRegparamsvol1: " + dtiRegParams.getNumVolumes());
                    for (int i = 0; i < dtiRegParams.getNumVolumes(); i++) {
                        System.out.println("dtibvalsparamsvol1: " + dtiRegParams.getbValues()[i]);
                    }
                }*/
            }

        }

    }

    /**
     * Calls the algorithm with the set-up parameters.
     */
    protected void callReg35Algorithm() {
        cost = 1;
        float rotateBegin = (float) -30.0;
        float rotateEnd = (float) 30.0;
        float coarseRate = (float) 15.0;
        float fineRate = (float) 6.0;
        sliceNum = Integer.parseInt(refImageNumText.getText());

        if (newB0DWIRegImage != null) {
            matchDWIImage = newB0DWIRegImage;
        } else {
            matchDWIImage = pipeline.DWIImage;
        }

        dwi35RegImage = (ModelImage) matchDWIImage.clone(matchDWIImage.getImageName() + "3.5RegB0&DWIDataset");

        reg35 = new AlgorithmRegOAR35D(dwi35RegImage, cost, DOF, interp, interp, registerTo, sliceNum, rotateBegin,
                rotateEnd, coarseRate, fineRate, doGraph, doSubsample, fastMode, bracketBound, maxIterations, numMinima);
        //Testing
        /*System.out.println("costChoice" + cost);
        System.out.println("int _DOF" + DOF);
        System.out.println("_interp" + interp);
        System.out.println("_interp2" + interp);
        System.out.println("mode" + registerTo);
        System.out.println("refImageNum" + sliceNum);
        System.out.println(" _rotateBegin" + rotateBegin);
        System.out.println("_rotateEnd" + rotateEnd);
        System.out.println("_coarseRate" + coarseRate);
        System.out.println("_fineRate" + fineRate);
        System.out.println("doGraph" + doGraph);
        System.out.println("doSubsample" + doSubsample);
        System.out.println("fastMode" + fastMode);
        System.out.println("_bracketBound" + bracketBound);
        System.out.println("_baseNumIter" + maxIterations);
        System.out.println("_numMinima" + numMinima);*/
        // System.out.println("doJTEM" +doJTEM);
        // System.out.println("doMultiThread" +doMultiThread);

        // Start the thread as a low priority because we wish to still have user interface work fast.
        reg35.addListener(this);

        setVisible(true);

        reg35.run();

    }

    /**
     * Calls the algorithm with the set-up parameters.
     */
    protected void callT2Algorithm() {

        refT2image = pipeline.T2Image;
        srcB0Image = pipeline.DWIImage;
        destB0Extents = new int[3];
        destB0Extents[0] = srcB0Image.getExtents()[0];
        destB0Extents[1] = srcB0Image.getExtents()[1];
        destB0Extents[2] = srcB0Image.getExtents()[2];
        resultB0String = srcB0Image.getImageName() + "T=" + refImageNumText.getText();
        resultB0Image = new ModelImage(srcB0Image.getType(), destB0Extents, resultB0String);
        sliceNum = Integer.parseInt(refImageNumText.getText());

        if (resultB0Image != null) {
            subsetAlgo = new AlgorithmSubset(pipeline.DWIImage, resultB0Image, AlgorithmSubset.REMOVE_T, sliceNum);
            // createProgressBar(srcB0Image.getImageName(), subsetAlgo);
            subsetAlgo.run();
        }

        if ( (subsetAlgo.isCompleted() == true) && (resultB0Image != null)) {
            matchB0image = resultB0Image;

        }

        costT2 = 1;
        DOFT2 = 6;
        interpT2 = 0;
        float rotateBeginX = (float) -30.0;
        float rotateEndX = (float) 30.0;
        float coarseRateX = (float) 15.0;
        float fineRateX = (float) 6.0;
        float rotateBeginY = (float) -30.0;
        float rotateEndY = (float) 30.0;
        float coarseRateY = (float) 15.0;
        float fineRateY = (float) 6.0;
        float rotateBeginZ = (float) -30.0;
        float rotateEndZ = (float) 30.0;
        float coarseRateZ = (float) 15.0;
        float fineRateZ = (float) 6.0;

        reg3 = new AlgorithmRegOAR3D(refT2image, matchB0image, costT2, DOFT2, interpT2, rotateBeginX, rotateEndX,
                coarseRateX, fineRateX, rotateBeginY, rotateEndY, coarseRateY, fineRateY, rotateBeginZ, rotateEndZ,
                coarseRateZ, fineRateZ, maxOfMinResol, doSubsample, doMultiThread, fastMode, bracketBound,
                maxIterations, numMinima);

        reg3.addListener(this);
        reg3.run();

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

        JLabel labelInternal = new JLabel("Reference DWI Volume Number: ");
        labelInternal.setForeground(Color.black);
        labelInternal.setFont(serif12);

        refImageNumText = new JTextField("0", 2);
        refImageNumText.setEnabled(true);

        final JLabel labelDOF = new JLabel("Degrees of freedom:");
        labelDOF.setForeground(Color.black);
        labelDOF.setFont(serif12);
        labelDOF.setAlignmentX(Component.LEFT_ALIGNMENT);

        comboBoxDOF = new JComboBox();
        comboBoxDOF.setFont(MipavUtil.font12);
        comboBoxDOF.setBackground(Color.white);
        comboBoxDOF.setToolTipText("Degrees of freedom");
        comboBoxDOF.addItem("Motion Correction");
        comboBoxDOF.addItem("Motion Correction + Eddy Current");
        comboBoxDOF.setSelectedIndex(0);
        comboBoxDOF.addItemListener(this);

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
        comboBoxInterp.setSelectedIndex(0);
        comboBoxInterp.addItemListener(this);

        JLabel labelCost = new JLabel("Cost function:");
        labelCost.setForeground(Color.black);
        labelCost.setFont(serif12);
        labelCost.setAlignmentX(Component.LEFT_ALIGNMENT);

        comboBoxCostFunct = new JComboBox();
        comboBoxCostFunct.setFont(MipavUtil.font12);
        comboBoxCostFunct.setBackground(Color.white);
        comboBoxCostFunct.setAlignmentX(Component.LEFT_ALIGNMENT);
        comboBoxCostFunct.setToolTipText("Cost function");
        comboBoxCostFunct.addItem("Correlation ratio");
        comboBoxCostFunct.addItem("Least squares");
        comboBoxCostFunct.addItem("Normalized cross correlation");
        comboBoxCostFunct.addItem("Normalized mutual information");
        comboBoxCostFunct.setSelectedIndex(0);
        comboBoxCostFunct.addItemListener(this);

        Insets insets = new Insets(0, 2, 0, 2);
        GridBagConstraints gbc = new GridBagConstraints();

        gbc.insets = insets;
        gbc.fill = GridBagConstraints.NONE;
        gbc.anchor = GridBagConstraints.WEST;

        gbc.gridx = 0;
        gbc.gridy = 0;
        gbc.gridwidth = 1;
        gbc.gridheight = 1;
        optPanel.add(labelInternal, gbc);

        gbc.gridx = 1;
        gbc.gridy = 0;
        gbc.weightx = 1;
        gbc.gridwidth = GridBagConstraints.REMAINDER;
        optPanel.add(refImageNumText, gbc);

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

        final JPanel outPanel = new JPanel();
        outPanel.setLayout(new GridBagLayout());
        outPanel.setBorder(buildTitledBorder("Output Options"));

        transformB0Checkbox = new JCheckBox("Display Registered B0 to Structural Image");
        transformB0Checkbox.setFont(serif12);
        transformB0Checkbox.setAlignmentX(Component.LEFT_ALIGNMENT);
        transformB0Checkbox.setForeground(Color.black);
        transformB0Checkbox.setSelected(true);
        transformB0Checkbox.addItemListener(this);

        transformDWICheckbox = new JCheckBox("Display Transformed DWI Dataset Image");
        transformDWICheckbox.setFont(serif12);
        transformDWICheckbox.setAlignmentX(Component.LEFT_ALIGNMENT);
        transformDWICheckbox.setForeground(Color.black);
        transformDWICheckbox.setSelected(true);
        transformDWICheckbox.addItemListener(this);

        JLabel matrixLabel = new JLabel("Matrix file directory:");
        matrixLabel.setForeground(Color.black);
        matrixLabel.setFont(serif12);
        matrixLabel.setAlignmentX(Component.LEFT_ALIGNMENT);

        matrixComboBox = new JComboBox();
        matrixComboBox.setFont(serif12);
        matrixComboBox.setBackground(Color.white);
        matrixComboBox.setAlignmentX(Component.LEFT_ALIGNMENT);

        // refImage = UI.getRegisteredImageByName((String) comboBoxImage.getSelectedItem());

        if (pipeline.T2Image != null) {
            matrixComboBox.addItem(pipeline.T2Image.getImageDirectory());
        }

        // System.out.println("srcImageDir: " +srcB0Image.getImageDirectory());

        // refImage = UI.getRegisteredImageByName((String) comboBoxImage.getSelectedItem());

        gbc.gridx = 0;
        gbc.gridy = 0;
        gbc.weightx = 1;
        gbc.fill = GridBagConstraints.HORIZONTAL;
        outPanel.add(transformB0Checkbox, gbc);

        gbc.gridx = 0;
        gbc.gridy = 1;
        gbc.weightx = 1;
        gbc.fill = GridBagConstraints.HORIZONTAL;
        outPanel.add(transformDWICheckbox, gbc);

        gbc.gridx = 0;
        gbc.gridy = 2;
        gbc.gridwidth = 1;
        gbc.gridheight = 1;
        outPanel.add(matrixLabel, gbc);

        gbc.gridx = 1;
        gbc.gridy = 2;
        gbc.weightx = 1;
        gbc.gridwidth = GridBagConstraints.HORIZONTAL;
        ;
        outPanel.add(matrixComboBox, gbc);

        JPanel buttonPanel = new JPanel();
        buildOKButton();
        buttonPanel.add(OKButton);
        buildCancelButton();
        buttonPanel.add(cancelButton);
        buildHelpButton();
        buttonPanel.add(helpButton);

        mainPrePanel = new JPanel();
        mainPrePanel.setLayout(new GridBagLayout());
        // gbc.anchor = GridBagConstraints.NORTHWEST;
        gbc.gridx = 0;
        gbc.gridy = 0;
        gbc.weightx = .5;
        gbc.weighty = 0;
        gbc.gridwidth = 1;
        gbc.fill = GridBagConstraints.BOTH;
        mainPrePanel.add(optPanel, gbc);
        gbc.gridx = 0;
        gbc.gridy = 1;
        gbc.weightx = .5;
        gbc.weighty = 0;
        gbc.gridwidth = 1;
        gbc.fill = GridBagConstraints.BOTH;
        mainPrePanel.add(outPanel, gbc);
        gbc.gridx = 0;
        gbc.gridy = 2;
        gbc.gridwidth = 1;
        gbc.weightx = 1;
        gbc.weighty = 1;
        mainPrePanel.add(buttonPanel, gbc);

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
     * This method creates the B-Value/Gradient file for DTI Tab
     * 
     * @return
     */
    public void createArrayTransMatrixTXT() {

        try {
            StringBuffer sb;
            int padLength;
            File arrayMatFile = new File(matrixDirectory + pipeline.DWIImage.getImageName() + "TransMats" + ".mtx");
            FileOutputStream outputStream = new FileOutputStream(arrayMatFile);
            PrintStream printStream = new PrintStream(outputStream);
            String matrixString = "";
            String[] matrixArr = new String[pipeline.DWIImage.getExtents()[3]];

            for (int i = 0; i < pipeline.DWIImage.getExtents()[3] - 1; i++) {
                printStream.print("TransMatrix" + " " + i + ":");
                printStream.println();
                for (int j = 0; j < 4; j++) {
                    matrixString = "\t" + Float.toString(arrayTransMatrix[i].Get(j, 0)) + "    "
                            + Float.toString(arrayTransMatrix[i].Get(j, 1)) + "    "
                            + Float.toString(arrayTransMatrix[i].Get(j, 2)) + "    "
                            + Float.toString(arrayTransMatrix[i].Get(j, 3));
                    printStream.print(matrixString);
                    printStream.println();
                }
            }
            printStream.println();
            printStream.print("Using cost function, " + comboBoxCostFunct.getSelectedItem());
            printStream.print(", the cost is " + Double.toString(reg35.getAnswer()));
            printStream.println();
            printStream.print("Some registration settings: ");
            printStream.println();
            printStream.print("X Rotations from " + -30.0 + " to " + 30.0 + ", ");
            printStream.print("with a X coarse rate of " + 15.0 + " and X fine rate of " + 6.0);
            printStream.println();
            printStream.print("Y Rotations from " + -30.0 + " to " + 30.0 + ", ");
            printStream.print("with a Y coarse rate of " + 15.0 + " and Y fine rate of " + 6.0);
            printStream.println();
            printStream.print("Z Rotations from " + -30.0 + " to " + 30.0 + ", ");
            printStream.print("with a Z coarse rate of " + 15.0 + " and Z fine rate of " + 6.0);
            printStream.println();

        } catch (Exception e) {
            Preferences.debug("! ERROR: " + e.toString() + "\n", Preferences.DEBUG_ALGORITHM);
            Preferences.debug("! ERROR: Creation of arrarTrans<atrix file failed....exiting algorithm \n",
                    Preferences.DEBUG_ALGORITHM);

        }

    }

    /**
     * Sets the variables needed to call the registration algorithm based on the values entered in the dialog.
     * 
     * @return <code>true</code> if the variables are properly set, <code>false</code> otherwise.
     */
    private boolean setVariables() {

        switch (comboBoxDOF.getSelectedIndex()) {

            case 0:
                DOF = 6;
                break;

            case 1:
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

        return true;
    }

    @Override
    public void itemStateChanged(ItemEvent e) {
        // TODO Auto-generated method stub

    }

}
