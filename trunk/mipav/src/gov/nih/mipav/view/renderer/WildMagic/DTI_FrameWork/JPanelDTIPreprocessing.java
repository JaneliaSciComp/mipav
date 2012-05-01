package gov.nih.mipav.view.renderer.WildMagic.DTI_FrameWork;


import gov.nih.mipav.util.MipavCoordinateSystems;

import gov.nih.mipav.model.algorithms.AlgorithmBase;
import gov.nih.mipav.model.algorithms.AlgorithmCostFunctions;
import gov.nih.mipav.model.algorithms.AlgorithmInterface;
import gov.nih.mipav.model.algorithms.AlgorithmTransform;
import gov.nih.mipav.model.algorithms.registration.AlgorithmRegOAR35D;
import gov.nih.mipav.model.algorithms.registration.AlgorithmRegOAR3D;
import gov.nih.mipav.model.algorithms.utilities.AlgorithmAddMargins;
import gov.nih.mipav.model.algorithms.utilities.AlgorithmInsertVolume;
import gov.nih.mipav.model.algorithms.utilities.AlgorithmRemoveTSlices;
import gov.nih.mipav.model.algorithms.utilities.AlgorithmSubset;
import gov.nih.mipav.model.file.DTIParameters;
import gov.nih.mipav.model.file.FileInfoBase;
import gov.nih.mipav.model.file.FileInfoNIFTI;
import gov.nih.mipav.model.structures.ModelImage;
import gov.nih.mipav.model.structures.TransMatrix;

import gov.nih.mipav.view.MipavUtil;
import gov.nih.mipav.view.Preferences;
import gov.nih.mipav.view.ViewJFrameImage;
import gov.nih.mipav.view.ViewJProgressBar;
import gov.nih.mipav.view.ViewUserInterface;
import gov.nih.mipav.view.dialogs.JDialogBase;
import gov.nih.mipav.view.dialogs.JDialogScriptableTransform;

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
import javax.swing.border.LineBorder;
import javax.swing.border.TitledBorder;

import WildMagic.LibFoundation.Mathematics.Vector3f;

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

    private Font serif12;

    private Font serif12B;

    /** DOCUMENT ME! */
    public JTextField refImageNumText;

    /** DOCUMENT ME! */
    private JComboBox comboBoxDOF;
    
    /** Progress bar that will listen to a dialog's algorithm (and reflect current progress)*/
    protected ViewJProgressBar progressBar;

    /** DOCUMENT ME! */
    private JComboBox comboBoxInterp;

    /** DOCUMENT ME! */
    private JComboBox comboBoxCostFunct;


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

    private String matrixDirectory;

    private float rotateBeginX, rotateEndX, coarseRateX, fineRateX, rotateBeginY, rotateEndY, coarseRateY, fineRateY,
            rotateBeginZ, rotateEndZ, coarseRateZ, fineRateZ;

    private boolean maxOfMinResol = true, doSubsample = true, doMultiThread = true, fastMode = false;

    private boolean doGraph = false;

    private int maxIterations = 2, numMinima = 3;

    int registerTo = 3;

    /** DOCUMENT ME! */

    private int[] destB0Extents;

    public int refVolNum;
    
    private DTIGradTableCorrectionAfterTrans gradTableRegCorrect = null;

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

    public TransMatrix[] arrayTransMatrix;

    public TransMatrix b0toStructMatrix;

    private DTIParameters dtiRegParams;

    public JCheckBox transformMatDWICheckbox;

    public JLabel transformB0label;

    public JCheckBox transformB0MatCheckbox;

    public JCheckBox transformB0Checkbox;
    
    public JLabel blanklabel;

    public JCheckBox epiCheckbox;

    private JCheckBox skipPreCheckbox;
    
    public JCheckBox correctGradTransCheckbox;
   
    private JLabel labelDOF;

    private JLabel labelCost;

    private JLabel labelInternal;
    
    private DTIParameters dtiparams;

    private JLabel labelInterp;

    public JPanel highlightBorderPanel;

    public JPanel structOptPanel;

    public ModelImage inputPreTensorImage;

    private float[][] correctedGradients;

    private AlgorithmTransform algoTrans;

    private String resultT2String;

    private ModelImage matchT2image;
    
    private AlgorithmAddMargins imageMarginsAlgo;

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

        if (command.equals("RUN OAR 3.5D")) {
            if (pipeline.T2Image != null) {
                B0extraction();
                if(matchB0image!=null){
                    if(pipeline.T2Image.getExtents()!= matchB0image.getExtents()){
                        resampleT2();                
                        callT2Algorithm();
                        setVariablesForOAR35D();
                        callReg35Algorithm();   
                    }
                    else{
                        callT2Algorithm();
                        setVariablesForOAR35D();
                        callReg35Algorithm(); 
                    }
                }
                else{
                    MipavUtil.displayError("Error extracting B0 image");   
                }

            } else {
                setVariablesForOAR35D();
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


                    } catch (final OutOfMemoryError error) {
                        MipavUtil.displayError("Out of memory: unable to open new frame");
                    }
                }

            }
            if (transformMatDWICheckbox.isSelected()) {
                createArrayTransMatrixTXT();               
            }
            
            if (correctGradTransCheckbox.isSelected()) {
                gradTableRegCorrect = new DTIGradTableCorrectionAfterTrans(pipeline.gradients, pipeline.bvalues, arrayTransMatrix, refVolNum);
                setVisible(true);
                gradTableRegCorrect.run();
                correctedGradients = gradTableRegCorrect.getCorrectedGradients();                
                dtiparams = pipeline.DWIImage.getDTIParameters();
                for (int i = 0; i < pipeline.DWIImage.getExtents()[3]; i++) {
                    // Populate Gradient column
                    pipeline.srcBvalGradTable.setValueAt(String.valueOf(correctedGradients[i][0]), i, 2);
                    pipeline.srcBvalGradTable.setValueAt(String.valueOf(correctedGradients[i][1]), i, 3);
                    pipeline.srcBvalGradTable.setValueAt(String.valueOf(correctedGradients[i][2]), i, 4);
                   }
                
                dtiparams.setGradients(correctedGradients);

            }
            
            
  
            if (result35RegImage != null) {
                pipeline.nextButton.setEnabled(true);
                pipeline.nextButton.setActionCommand("next2");
                
            }




        } else if (command.equals("skipPre")) {
            if (skipPreCheckbox.isSelected()){
                transformMatDWICheckbox.setEnabled(false);
                transformDWICheckbox.setEnabled(false);
                labelInternal.setEnabled(false);
                refImageNumText.setEnabled(false);              
                labelDOF.setEnabled(false); 
                comboBoxDOF.setEnabled(false);
                labelInterp.setEnabled(false);
                comboBoxInterp.setEnabled(false);
                labelCost.setEnabled(false);
                comboBoxCostFunct.setEnabled(false);
                OKButton.setEnabled(false);
                if (pipeline.T2Image != null){
                    epiCheckbox.setSelected(false);
                    epiCheckbox.setEnabled(false);
                    transformB0label.setEnabled(false);
                    transformB0MatCheckbox.setEnabled(false);
                    blanklabel.setEnabled(false);
                    transformB0Checkbox.setEnabled(false);
                }



                inputPreTensorImage = pipeline.DWIImage;
                pipeline.nextButton.setEnabled(true);
                pipeline.nextButton.setActionCommand("next2");
                


            }
            else{
                transformMatDWICheckbox.setEnabled(true);
                transformB0label.setEnabled(true);
                transformB0MatCheckbox.setEnabled(true);
                labelInternal.setEnabled(true);
                refImageNumText.setEnabled(true);              
                labelDOF.setEnabled(true);
                comboBoxDOF.setEnabled(true);
                labelInterp.setEnabled(true);
                comboBoxInterp.setEnabled(true);
                labelCost.setEnabled(true);
                comboBoxCostFunct.setEnabled(true);
                OKButton.setEnabled(true);
                
                pipeline.nextButton.setEnabled(false);
                pipeline.nextButton.setActionCommand("next2");
            }




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

        nf = new DecimalFormat();
        nf.setMaximumFractionDigits(4);
        nf.setMinimumFractionDigits(0);
        nf.setGroupingUsed(false);

        final DecimalFormatSymbols dfs = nf.getDecimalFormatSymbols();
        dfs.setDecimalSeparator('.');
        nf.setDecimalFormatSymbols(dfs);

        if (algorithm instanceof AlgorithmRegOAR3D) {
            if (reg3.isCompleted()) {
                b0toStructMatrix = reg3.getTransform();
               
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

                            if (i == refVolNum) {// Finds volume based on user specified to remove
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
                        createProgressBar(resultB0RemoveString, removeTSlicesAlgo);
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
                                    refVolNum, resultB0toT2Image);
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
                



                if (resultB0toT2Image != null) {
                    resultB0toT2Image.getMatrixHolder().replaceMatrices(refT2image.getMatrixHolder().getMatrices());

                    for (int i = 0; i < resultB0toT2Image.getExtents()[2]; i++) {
                        resultB0toT2Image.getFileInfo(i).setOrigin(refT2image.getFileInfo(i).getOrigin());
                    }
                }

                b0toStructMatrix.setTransformID(TransMatrix.TRANSFORM_ANOTHER_DATASET);
                matchB0image.getMatrixHolder().addMatrix(b0toStructMatrix);
                if (transformB0MatCheckbox.isSelected()) {
                    String message = "Using cost function, " + "Correlation ration";
                    message += ", the cost is " + Double.toString(reg3.getAnswer()) + ".\n";
                    message += "Some registration settings: \n";
                    message += "X Rotations from " + rotateBeginX + " to " + rotateEndX + ", ";
                    message += "with a X coarse rate of " + coarseRateX + " and X fine rate of " + fineRateX + ".\n";
                    message += "Y Rotations from " + rotateBeginY + " to " + rotateEndY + ", ";
                    message += "with a Y coarse rate of " + coarseRateY + " and Y fine rate of " + fineRateY + ".\n";
                    message += "Z Rotations from " + rotateBeginZ + " to " + rotateEndZ + ", ";
                    message += "with a Z coarse rate of " + coarseRateZ + " and Z fine rate of " + fineRateZ + ".\n";
                    b0toStructMatrix.saveMatrix(pipeline.T2Image.getImageDirectory() + File.separator + matchB0image.getImageName() + "_To_"
                            + refT2image.getImageName() + ".mtx", message);
                    Preferences.debug("Saved " + pipeline.T2Image.getImageDirectory() + File.separator + matchB0image.getImageName() + "_To_"
                            + refT2image.getImageName() + ".mtx\n", Preferences.DEBUG_FILEIO);
                }

            }

            if (reg3 != null) {
                reg3.disposeLocal();
                reg3 = null;
            }

            matchB0image = null; // register match image to reference Image
            refT2image = null;
        }
        if (algorithm instanceof AlgorithmRegOAR35D) {
            matrixDirectory = pipeline.DWIImage.getImageDirectory();
            arrayTransMatrix = reg35.getArrayTransMatrix();

            result35RegImage = reg35.getTransformedImage();
            if (result35RegImage != null) {
                result35RegImage.calcMinMax();
                result35RegImage.setImageName(pipeline.DWIImage.getImageName() + comboBoxDOF.getSelectedItem());
                dtiRegParams = new DTIParameters(result35RegImage.getExtents()[3]);
                dtiRegParams = pipeline.DWIImage.getDTIParameters();
            }

        }
        

    }
    private void B0extraction(){
        srcB0Image = pipeline.DWIImage;
        destB0Extents = new int[3];
        destB0Extents[0] = srcB0Image.getExtents()[0];
        destB0Extents[1] = srcB0Image.getExtents()[1];
        destB0Extents[2] = srcB0Image.getExtents()[2];
        resultB0String = srcB0Image.getImageName() + "T=" + refImageNumText.getText();
        resultB0Image = new ModelImage(srcB0Image.getType(), destB0Extents, resultB0String);
        //B0 extraction from DWI dataset
        if (resultB0Image != null) {
            subsetAlgo = new AlgorithmSubset(pipeline.DWIImage, resultB0Image, AlgorithmSubset.REMOVE_T, refVolNum);
            createProgressBar(srcB0Image.getImageName(), subsetAlgo);
            subsetAlgo.run();
        }

        if ( (subsetAlgo.isCompleted() == true) && (resultB0Image != null)) {
            matchB0image = resultB0Image;
        }
    }
    private void resampleT2() {
        
        //Parameters for algoTrans
        TransMatrix xfrm = new TransMatrix(4);
        xfrm.MakeIdentity();
        int[] units = matchB0image.getUnitsOfMeasure();
        float fovX = pipeline.T2Image.getResolutions(0)[0] * (pipeline.T2Image.getExtents()[0] - 1);
        float fovY = pipeline.T2Image.getResolutions(0)[1]  * (pipeline.T2Image.getExtents()[1] - 1);
        float fovZ = pipeline.T2Image.getResolutions(0)[2]  * (pipeline.T2Image.getExtents()[2] - 1);
        int oXdim = Math.round(fovX / (matchB0image.getResolutions(0)[0]) + 1);
        int oYdim = Math.round(fovY / (matchB0image.getResolutions(0)[1]) + 1);
        int oZdim = Math.round(fovZ / (matchB0image.getResolutions(0)[2]) + 1);
        
        //Sets T2 resolutions the same as the B0 volume
        algoTrans = new AlgorithmTransform(pipeline.T2Image, xfrm, 0, matchB0image.getResolutions(0)[0], 
                matchB0image.getResolutions(0)[1], matchB0image.getResolutions(0)[2], oXdim, 
                oYdim, oZdim, units, false, false, false, false, null);
        algoTrans.addListener(this);
        
        algoTrans.run();
        
        /*if (algoTrans.isCompleted()){
            if(algoTrans.getTransformedImage()!=null){
                new ViewJFrameImage(algoTrans.getTransformedImage());
            }
        }*/
                
        int[] addCropXArr = new int[2];
        int[] addCropYArr = new int[2];
        int[] addCropZArr = new int[2];

        resultT2String = pipeline.T2Image.getImageName() + "Resample";
        matchT2image = new ModelImage(pipeline.T2Image.getType(), destB0Extents, resultT2String);
        //Add or remove padding from T2 image
        float addCropX = (matchB0image.getExtents()[0]-algoTrans.getTransformedImage().getExtents()[0])/2;
        float addCropY =(matchB0image.getExtents()[1]-algoTrans.getTransformedImage().getExtents()[1])/2;
        float addCropZ =(matchB0image.getExtents()[2]-algoTrans.getTransformedImage().getExtents()[2])/2;
        if (String.valueOf(addCropX/2).contains(".5")){
            addCropX = (float) (addCropX + 1.0);
            addCropXArr[0]= (int) (addCropX + .5); 
            addCropXArr[1]= (int) (addCropX - .5); 
        }
        else{
            addCropXArr[0]= (int) (addCropX); 
            addCropXArr[1]= (int) (addCropX); 
        }
        if (String.valueOf(addCropY/2).contains(".5")){
            addCropY = (float) (addCropY + 1.0);
            addCropYArr[0]= (int) (addCropY + .5); 
            addCropYArr[1]= (int) (addCropY - .5); 
        }
        else{
            addCropYArr[0]= (int) (addCropY); 
            addCropYArr[1]= (int) (addCropY); 
        }
        if (String.valueOf(addCropZ/2).contains(".5")){
            addCropZ = (float) (addCropZ + 1.0);
            addCropZArr[0]= (int) (addCropZ + .5); 
            addCropZArr[1]= (int) (addCropZ - .5); 
        }
        else{
            addCropZArr[0]= (int) (addCropZ); 
            addCropZArr[1]= (int) (addCropZ); 
        }
        
        imageMarginsAlgo = new AlgorithmAddMargins(algoTrans.getTransformedImage(), matchT2image,addCropXArr ,addCropYArr ,addCropZArr);
        imageMarginsAlgo.addListener(this);
        createProgressBar(pipeline.T2Image.getImageName(), imageMarginsAlgo);
        imageMarginsAlgo.run();
        if((imageMarginsAlgo.isCompleted() == true) && matchT2image!=null){
            new ViewJFrameImage(matchT2image);
        }
        algoTrans = null;

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
        refVolNum = Integer.parseInt(refImageNumText.getText());

        if (newB0DWIRegImage != null) {
            matchDWIImage = newB0DWIRegImage;
        } else {
            matchDWIImage = pipeline.DWIImage;
        }

        dwi35RegImage = (ModelImage) matchDWIImage.clone(matchDWIImage.getImageName() + "3.5RegB0&DWIDataset");

        reg35 = new AlgorithmRegOAR35D(dwi35RegImage, cost, DOF, interp, interp, registerTo, refVolNum, rotateBegin,
                rotateEnd, coarseRate, fineRate, doGraph, doSubsample, fastMode, maxIterations, numMinima);

        reg35.addListener(this);
        
        createProgressBar(dwi35RegImage.getImageName(), reg35);

        setVisible(true);

        reg35.run();

    }

    /**
     * Calls the algorithm with the set-up parameters.
     */
    protected void callT2Algorithm() {

        if (matchT2image!= null){
            refT2image = matchT2image;
        }
        else{
        refT2image = pipeline.T2Image;
        }
        refVolNum = Integer.parseInt(refImageNumText.getText());


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
                coarseRateZ, fineRateZ, maxOfMinResol, doSubsample, doMultiThread, fastMode,
                maxIterations, numMinima);

        reg3.addListener(this);
        createProgressBar(refT2image.getImageName(), reg3);
        reg3.run();

    }
    

    private TitledBorder buildTitledBorder(String title) {
        return new TitledBorder(new EtchedBorder(), title, TitledBorder.LEFT, TitledBorder.CENTER, MipavUtil.font12B,
                Color.black);
    }
    

    
    private TitledBorder highlightTitledBorder(String title){
        return new TitledBorder(new LineBorder( Color.black, 2), title, TitledBorder.LEFT, TitledBorder.CENTER, MipavUtil.font12B,
                Color.black);
    }

    /**
     * Initializes the GUI components and displays the dialog.
     */
    private void init() {
        setForeground(Color.black);
        

        
        structOptPanel = new JPanel();
        structOptPanel.setLayout(new GridBagLayout());
        structOptPanel.setBorder(buildTitledBorder("B0 to Structural Image OAR 3D Output Options"));
        transformB0Checkbox = new JCheckBox("Display Registered B0 to Structural Image");
        transformB0Checkbox.setFont(serif12);
        transformB0Checkbox.setAlignmentX(Component.LEFT_ALIGNMENT);
        transformB0Checkbox.setForeground(Color.black);
        transformB0Checkbox.setSelected(true);
        transformB0Checkbox.addActionListener(this);
        transformB0Checkbox.setEnabled(false);
        
        transformB0MatCheckbox = new JCheckBox("Save Registered B0 to Structural Image Trans Matrix to directory");
        transformB0MatCheckbox.setFont(serif12);
        transformB0MatCheckbox.setAlignmentX(Component.LEFT_ALIGNMENT);
        transformB0MatCheckbox.setForeground(Color.black);
        transformB0MatCheckbox.setSelected(true);
        transformB0MatCheckbox.addActionListener(this);
        transformB0MatCheckbox.setEnabled(false);
        
        epiCheckbox = new JCheckBox("Perform EPI Distortion Correction");
        epiCheckbox.setFont(serif12);
        epiCheckbox.setAlignmentX(Component.LEFT_ALIGNMENT);
        epiCheckbox.setForeground(Color.black);
        epiCheckbox.setActionCommand("doEPI");
        epiCheckbox.setSelected(false);
        epiCheckbox.addActionListener(this);
        epiCheckbox.setEnabled(false);
        
        blanklabel = new JLabel("-------------------------------------------------------------------------------" +
        		"----------------------------------------------------------------------------------------------------------" +
        		"----------------------------------------------------------");

        blanklabel.setForeground(Color.black);
        blanklabel.setFont(serif12);
        blanklabel.setEnabled(false);
        
        transformB0label = new JLabel("Note: B0 to Structural Image OAR 3D rigid registration is performed " +
        		"automatically when RUN OAR 3.5D button is selected");
        transformB0label.setForeground(Color.black);
        transformB0label.setFont(serif12);
        transformB0label.setEnabled(false);
        
        
        
        Insets insets = new Insets(0, 2, 0, 2);
        GridBagConstraints gbc = new GridBagConstraints();

        gbc.insets = insets;
        gbc.fill = GridBagConstraints.NONE;
        gbc.anchor = GridBagConstraints.WEST;
        
        gbc.gridx = 0;
        gbc.gridy = 0;
        gbc.weightx = 1;
        gbc.fill = GridBagConstraints.HORIZONTAL;
        structOptPanel.add(transformB0Checkbox, gbc);
        
        gbc.gridx = 0;
        gbc.gridy = 1;
        gbc.weightx = 1;
        gbc.fill = GridBagConstraints.HORIZONTAL;
        structOptPanel.add(transformB0MatCheckbox, gbc);
        
        gbc.gridx = 0;
        gbc.gridy = 2;
        gbc.weightx = 1;
        gbc.fill = GridBagConstraints.HORIZONTAL;
        structOptPanel.add(epiCheckbox, gbc);
        
        gbc.gridx = 0;
        gbc.gridy = 3;
        gbc.weightx = 1;
        //gbc.fill = GridBagConstraints.HORIZONTAL;
        structOptPanel.add(blanklabel, gbc);
        
        gbc.gridx = 0;
        gbc.gridy = 4;
        gbc.weightx = 1;
        //gbc.fill = GridBagConstraints.HORIZONTAL;
        structOptPanel.add(transformB0label, gbc);

        
        

        JPanel optPanel = new JPanel();
        optPanel.setLayout(new GridBagLayout());
        optPanel.setBorder(buildTitledBorder("OAR 3.5D Input Options"));
        
        skipPreCheckbox = new JCheckBox("Skip Pre-Processing");
        skipPreCheckbox.setFont(serif12);
        skipPreCheckbox.setAlignmentX(Component.LEFT_ALIGNMENT);
        skipPreCheckbox.setForeground(Color.black);
        skipPreCheckbox.setActionCommand("skipPre");
        skipPreCheckbox.addActionListener(this);
        skipPreCheckbox.setSelected(false);


        labelInternal = new JLabel("Reference DWI Volume Number: ");
        labelInternal.setForeground(Color.black);
        labelInternal.setFont(serif12);

        refImageNumText = new JTextField("0", 2);
        refImageNumText.setEnabled(true);

        labelDOF = new JLabel("Degrees of freedom:");
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

        labelInterp = new JLabel("Interpolation:");
        labelInterp.setForeground(Color.black);
        labelInterp.setFont(serif12);
        labelInterp.setAlignmentX(Component.LEFT_ALIGNMENT);

        comboBoxInterp = new JComboBox();
        comboBoxInterp.setFont(MipavUtil.font12);
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

        labelCost = new JLabel("Cost function:");
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

        gbc.insets = insets;
        gbc.fill = GridBagConstraints.NONE;
        gbc.anchor = GridBagConstraints.WEST;
        
        gbc.gridx = 0;
        gbc.gridy = 0;
        gbc.weightx = 0;
        gbc.gridwidth = 1;
        //gbc.anchor = GridBagConstraints.REMAINDER;
        gbc.insets = new Insets(0, 2, 0, 2);
        gbc.fill = GridBagConstraints.REMAINDER;
        optPanel.add(skipPreCheckbox, gbc);

        gbc.gridx = 0;
        gbc.gridy = 1;
        gbc.gridwidth = 1;
        gbc.gridheight = 1;
        gbc.weightx = 0;
        gbc.fill = GridBagConstraints.NONE;
        gbc.anchor = GridBagConstraints.WEST;
        gbc.insets = new Insets(0, 2, 0, 2);
        optPanel.add(labelInternal, gbc);

        gbc.gridx = 1;
        gbc.gridy = 1;
        gbc.weightx = 0.15;
        gbc.fill = GridBagConstraints.REMAINDER;
        optPanel.add(refImageNumText, gbc);

        gbc.gridx = 0;
        gbc.gridy = 2;
        gbc.weightx = 0;
        gbc.gridwidth = 1;
        optPanel.add(labelDOF, gbc);
        gbc.gridx = 1;
        gbc.gridy = 2;
        gbc.weightx = 1;
        gbc.gridwidth = GridBagConstraints.REMAINDER;
        optPanel.add(comboBoxDOF, gbc);

        gbc.gridx = 0;
        gbc.gridy = 3;
        gbc.weightx = 0;
        gbc.gridwidth = 1;
        optPanel.add(labelInterp, gbc);
        gbc.gridx = 1;
        gbc.gridy = 3;
        gbc.weightx = 1;
        gbc.gridwidth = GridBagConstraints.REMAINDER;
        optPanel.add(comboBoxInterp, gbc);

        gbc.gridx = 0;
        gbc.gridy = 4;
        gbc.weightx = 0;
        gbc.gridwidth = 1;
        optPanel.add(labelCost, gbc);
        gbc.gridx = 1;
        gbc.gridy = 4;
        gbc.weightx = 1;
        gbc.gridwidth = GridBagConstraints.REMAINDER;
        optPanel.add(comboBoxCostFunct, gbc);

        final JPanel outPanel = new JPanel();
        outPanel.setLayout(new GridBagLayout());
        outPanel.setBorder(buildTitledBorder("OAR 3.5D Output Options"));
        


        transformDWICheckbox = new JCheckBox("Display Transformed DWI Dataset Image");
        transformDWICheckbox.setFont(serif12);
        transformDWICheckbox.setAlignmentX(Component.LEFT_ALIGNMENT);
        transformDWICheckbox.setForeground(Color.black);
        transformDWICheckbox.setSelected(true);
        transformDWICheckbox.addActionListener(this);

        transformMatDWICheckbox = new JCheckBox("Save Trans Matrices from Transformed DWI dataset to directory");
        transformMatDWICheckbox.setForeground(Color.black);
        transformMatDWICheckbox.setFont(serif12);
        transformMatDWICheckbox.setAlignmentX(Component.LEFT_ALIGNMENT);
        transformMatDWICheckbox.setSelected(true);
        transformMatDWICheckbox.addActionListener(this);
        
        correctGradTransCheckbox = new JCheckBox("Correct Gradients after Transformation");
        correctGradTransCheckbox.setForeground(Color.black);
        correctGradTransCheckbox.setFont(serif12);
        correctGradTransCheckbox.setAlignmentX(Component.LEFT_ALIGNMENT);
        correctGradTransCheckbox.setSelected(true);
        correctGradTransCheckbox.addActionListener(this);

        matrixComboBox = new JComboBox();
        matrixComboBox.setFont(serif12);
        matrixComboBox.setBackground(Color.white);
        matrixComboBox.setAlignmentX(Component.LEFT_ALIGNMENT);


        if (pipeline.T2Image != null) {
            matrixComboBox.addItem(pipeline.T2Image.getImageDirectory());
        }


        gbc.fill = GridBagConstraints.NONE;
        gbc.anchor = GridBagConstraints.WEST;
        

        
        gbc.gridx = 0;
        gbc.gridy = 0;
        gbc.weightx = 1;
        gbc.fill = GridBagConstraints.HORIZONTAL;
        //gbc.fill = GridBagConstraints.HORIZONTAL;
        outPanel.add(transformDWICheckbox, gbc);

        gbc.gridx = 0;
        gbc.gridy = 1;
        gbc.weightx = 1;
        gbc.fill = GridBagConstraints.HORIZONTAL;
        outPanel.add(transformMatDWICheckbox, gbc);
        
        gbc.gridx = 0;
        gbc.gridy = 2;
        gbc.weightx = 1;
        gbc.fill = GridBagConstraints.HORIZONTAL;
        outPanel.add(correctGradTransCheckbox, gbc);


        JPanel buttonPanel = new JPanel();
        buildOKButton();
        buttonPanel.add(OKButton);
        buildCancelButton();
        buildHelpButton();
        
        highlightBorderPanel = new JPanel();
        highlightBorderPanel.setLayout(new GridBagLayout());
        highlightBorderPanel.setBorder(buildTitledBorder(""));
        highlightBorderPanel.setAlignmentX(Component.TOP_ALIGNMENT);
        highlightBorderPanel.setAlignmentY(Component.TOP_ALIGNMENT);

        mainPrePanel = new JPanel();
        mainPrePanel.setLayout(new GridBagLayout());
        gbc.anchor = GridBagConstraints.NORTHWEST;

        gbc.gridx = 0;
        gbc.gridy = 0;
        gbc.weightx = .5;
        gbc.weighty = 0;
        gbc.gridwidth = 1;
        gbc.anchor = GridBagConstraints.NORTHWEST;
        //gbc.fill = GridBagConstraints.BOTH;
        highlightBorderPanel.add(optPanel, gbc);
        gbc.gridx = 0;
        gbc.gridy = 1;
        gbc.weightx = .5;
        gbc.weighty = 0;
        gbc.gridwidth = 1;
        gbc.anchor = GridBagConstraints.NORTHWEST;
        //gbc.fill = GridBagConstraints.BOTH;
        highlightBorderPanel.add(outPanel, gbc);
        gbc.gridx = 0;
        gbc.gridy = 2;
        gbc.weightx = .5;
        gbc.weighty = 0;
        gbc.gridwidth = 1;
        gbc.anchor = GridBagConstraints.NORTHWEST;
        //gbc.fill = GridBagConstraints.BOTH;
        highlightBorderPanel.add(structOptPanel, gbc);

        
        gbc.gridx = 0;
        gbc.gridy = 0;
        gbc.anchor = GridBagConstraints.NORTHWEST;
        //gbc.fill = GridBagConstraints.BOTH;
        mainPrePanel.add(highlightBorderPanel, gbc);
        gbc.gridx = 0;
        gbc.gridy = 2;
        gbc.gridwidth = 1;
        gbc.weightx = 1;
        gbc.weighty = 1;
        gbc.anchor = GridBagConstraints.NORTHWEST;
        mainPrePanel.add(buttonPanel, gbc);

        setVisible(true);

    }

    private JButton buildOKButton() {
        OKButton = new JButton("RUN OAR 3.5D");
        OKButton.addActionListener(this);

        // OKButton.setToolTipText("Accept values and perform action.");
        //OKButton.setMinimumSize(new Dimension(10, 200));
        //OKButton.setPreferredSize(MipavUtil.defaultButtonSize);
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
            File arrayMatFile = new File(matrixDirectory + pipeline.DWIImage.getImageName() + "TransMats" + ".mtx");
            FileOutputStream outputStream = new FileOutputStream(arrayMatFile);
            PrintStream printStream = new PrintStream(outputStream);
            String matrixString = "";

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
    private boolean setVariablesForOAR35D() {

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
    
    /**
     * Creates the progress bar that will listen to an algorithm's progress changes
     * @param title progress bar's title
     * @param pListener algorithmbase that will notify progress updates to the pBar
     */
    protected void createProgressBar(String title, AlgorithmBase pListener) {
        createProgressBar(title, " ...", pListener);
    }
    
    /**
     * Creates the progress bar (should be created within JDialog's callAlgorithm method
     * @param title progress bar's title
     * @param msg the message to display on the progress bar (initial setting)
     * @param pListener the algorithm that will register the progress bar as a listener
     */
    protected void createProgressBar(String title, String msg, AlgorithmBase pListener) {
        progressBar = new ViewJProgressBar(title, msg, 0, 100, true);
        progressBar.setSeparateThread(false);
        pListener.addProgressChangeListener(progressBar);
        pListener.setProgressValues(0, 100);
    }

    @Override
    public void itemStateChanged(ItemEvent e) {
        // TODO Auto-generated method stub

    }

}
