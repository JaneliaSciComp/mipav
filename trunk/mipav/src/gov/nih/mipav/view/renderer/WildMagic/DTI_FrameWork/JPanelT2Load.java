package gov.nih.mipav.view.renderer.WildMagic.DTI_FrameWork;

import gov.nih.mipav.util.ThreadUtil;

import gov.nih.mipav.model.algorithms.AlgorithmBase;
import gov.nih.mipav.model.algorithms.AlgorithmCostFunctions;
import gov.nih.mipav.model.algorithms.AlgorithmInterface;
import gov.nih.mipav.model.algorithms.AlgorithmTransform;
import gov.nih.mipav.model.algorithms.registration.AlgorithmRegOAR3D;
import gov.nih.mipav.model.algorithms.utilities.AlgorithmRemoveTSlices;
import gov.nih.mipav.model.algorithms.utilities.AlgorithmSubset;
import gov.nih.mipav.model.file.FileIO;
import gov.nih.mipav.model.scripting.ParserException;
import gov.nih.mipav.model.scripting.parameters.Parameter;
import gov.nih.mipav.model.scripting.parameters.ParameterBoolean;
import gov.nih.mipav.model.scripting.parameters.ParameterExternalImage;
import gov.nih.mipav.model.scripting.parameters.ParameterFactory;
import gov.nih.mipav.model.scripting.parameters.ParameterFloat;
import gov.nih.mipav.model.scripting.parameters.ParameterImage;
import gov.nih.mipav.model.scripting.parameters.ParameterInt;
import gov.nih.mipav.model.scripting.parameters.ParameterList;
import gov.nih.mipav.model.scripting.parameters.ParameterString;
import gov.nih.mipav.model.scripting.parameters.ParameterTable;
import gov.nih.mipav.model.structures.ModelImage;
import gov.nih.mipav.model.structures.ModelStorageBase;
import gov.nih.mipav.model.structures.TransMatrix;

import gov.nih.mipav.view.MipavUtil;
import gov.nih.mipav.view.Preferences;
import gov.nih.mipav.view.ViewImageFileFilter;
import gov.nih.mipav.view.ViewJFrameImage;
import gov.nih.mipav.view.ViewOpenFileUI;
import gov.nih.mipav.view.ViewUserInterface;
import gov.nih.mipav.view.ViewVOIVector;
import gov.nih.mipav.view.dialogs.ActionMetadata;
import gov.nih.mipav.view.dialogs.AlgorithmParameters;
import gov.nih.mipav.view.dialogs.JDialogBase;
import gov.nih.mipav.view.dialogs.JDialogRegistrationLeastSquares;
import gov.nih.mipav.view.dialogs.MipavActionMetadata;
import gov.nih.mipav.view.dialogs.ActionMetadata.ImageRequirements;

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
import java.awt.event.ItemEvent;
import java.awt.event.ItemListener;
import java.io.File;
import java.text.DecimalFormat;
import java.text.DecimalFormatSymbols;
import java.util.ArrayList;
import java.util.BitSet;
import java.util.EnumSet;
import java.util.Enumeration;
import java.util.Set;
import java.util.Vector;

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
import javax.swing.SwingConstants;
import javax.swing.border.EtchedBorder;
import javax.swing.border.TitledBorder;

public class JPanelT2Load extends JPanel implements AlgorithmInterface, ActionListener,
ItemListener  {
    // ~ Static fields/initializers
    // -------------------------------------------------------------------------------------

    /** Use serialVersionUID for interoperability. */
    private static final long serialVersionUID = -1461819906844299206L;

    // ~ Instance fields
    // ------------------------------------------------------------------------------------------------

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
    
    private JCheckBox multiThreadCheckBox;

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
    
    private boolean doMultiThread = true;

    /** DOCUMENT ME! */
    private boolean doSubsample;
    /** When true, the full version of JTEM Powell search is used in the registration algorithm. */
    private boolean doJTEM;

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
    /** Turns on the JTEM full version of Powell's algorithm on or off */
    private JCheckBox jtemCheckbox;

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

    private JLabel outOfBoundsLabel;

    private JComboBox outOfBoundsComboBox;

    private JLabel valueLabel;

    private JTextField valueText;

    private double imageMin;

    private double imageMax;

    private int dataType;
    
    private Font serif12;
    
    private Font serif12B;

    /**
     * Tells how to select fill value for out of bounds data 0 for image minimum 1 for NaN for float, zero otherwise. 2
     * for user defined 3 for image maximum
     */
    private int outOfBoundsIndex = 0;

    private float fillValue = 0.0f;
    
    private JLabel matrixLabel;
    
    public JComboBox matrixComboBox;
    
    private String matrixDirectory;
    
    private JLabel userDirectoryLabel;
    
    private JTextField userDirectoryText;
    
    private DTIPipeline pipeline;
    
    private JButton OKButton;

    private JButton cancelButton;

    private JButton helpButton;
    
    private JTextField textB0image; 
    
    /** DOCUMENT ME! */
    private JButton openB0Button; 
    
    private boolean lastStackFlag = false;
    
    /** B0image. */
    public ModelImage m_kB0Image;
    
    public ViewJFrameImage b0frame;
    
    //private ViewUserInterface ui;
    
    public JPanel mainT2Panel;
    
    private JPanel settingsPanel;
    
    private JCheckBox advancedBox;
    
    private JLabel bracketBoundLabel;

    private JLabel maxIterationsLabel;

    private JLabel numMinLabel;
    
    private int[] destB0Extents;
    
    /** DOCUMENT ME! */
    private int sliceNum;
    
    private String resultB0String;
    
    private ModelImage resultB0Image = null;
    
    private ModelImage srcB0Image;
    
    private AlgorithmSubset subsetAlgo;
    
    private boolean[] tVolumeRemove;
    
    private AlgorithmRemoveTSlices removeTSlicesAlgo;
    
    /** DOCUMENT ME! */
    private ModelImage resultB0RemoveImage = null;
    
    private String resultB0RemoveString;
    
    private int[] destB0RemoveExtents;

    

    // ~ Constructors
    // ---------------------------------------------------------------------------------------------------

    public JPanelT2Load (DTIPipeline pipeline) {
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
    public void actionPerformed(final ActionEvent event) {
        final String command = event.getActionCommand();
        String tmpStr;

        if (command.equals("OK")) {

            if (setVariables()) {
                callAlgorithm();
            }
        } else if (command.equals("Cancel")) {
           // dispose();
        } else if (command.equals("Help")) {
            MipavUtil.showHelp("OAR19076");
        }
            else if (command.equals("AdvancedSettings")) {
                if (advancedBox.isSelected()) {
                    bracketBoundLabel.setForeground(Color.BLACK);
                    maxIterationsLabel.setForeground(Color.BLACK);
                    numMinLabel.setForeground(Color.BLACK);

                    bracketBoundText.setEnabled(true);
                    maxIterationsText.setEnabled(true);
                    numMinText.setEnabled(true);
                    sampleCheckbox.setEnabled(true);
                    jtemCheckbox.setEnabled(true);
                    fastModeCheckbox.setEnabled(true);

                    bracketBound_def = bracketBound;
                    maxIterations_def = maxIterations;
                    numMinima_def = numMinima;
;

                } else {
                    bracketBoundLabel.setForeground(Color.lightGray);
                    maxIterationsLabel.setForeground(Color.lightGray);
                    numMinLabel.setForeground(Color.lightGray);

                    bracketBoundText.setEnabled(false);
                    maxIterationsText.setEnabled(false);
                    numMinText.setEnabled(false);
                    sampleCheckbox.setEnabled(false);
                    jtemCheckbox.setEnabled(false);
                    fastModeCheckbox.setEnabled(false);
                }

            //advancedDialog = buildAdvancedDialog(bracketBound, maxIterations, numMinima);
        } else if (command.equals("Ref")) {

            try {

                final JFileChooser chooser = new JFileChooser();

                if (UI.getDefaultDirectory() != null) {
                    final File file = new File(UI.getDefaultDirectory());

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

                final int returnValue = chooser.showOpenDialog(UI.getMainFrame());

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
            } catch (final OutOfMemoryError e) {
                MipavUtil.displayError("Out of memory in JDialogRegistrationOAR3D.");

                return;
            }
        } else if (command.equals("Input")) {

            try {
                final JFileChooser chooser = new JFileChooser();

                if (UI.getDefaultDirectory() != null) {
                    final File file = new File(UI.getDefaultDirectory());

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

                final int returnValue = chooser.showOpenDialog(UI.getMainFrame());

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
            } catch (final OutOfMemoryError e) {
                MipavUtil.displayError("Out of memory in JDialogRegistrationOAR3D.");

                return;
            }
        } else if (command.equals("AdvancedOkay")) {
            tmpStr = bracketBoundText.getText();

            if (JDialogBase.testParameter(tmpStr, 1, 60)) {
                bracketBound = Integer.valueOf(tmpStr).intValue();
            } else {
                bracketBound = bracketBound_def;
            }

            tmpStr = maxIterationsText.getText();

            if (JDialogBase.testParameter(tmpStr, 1, 100)) {
                maxIterations = Integer.valueOf(tmpStr).intValue();
            } else {
                maxIterations = maxIterations_def;
            }

            tmpStr = numMinText.getText();

            if (JDialogBase.testParameter(tmpStr, 1, 25)) {
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
            
        } else if (command.equals("browseB0File")) {
            loadB0File();
            pipeline.repaint();
    }
     else if (command.equals("extractB0File")) {
         if ( pipeline.T2Image != null){
             srcB0Image = pipeline.DWIImage;
             System.out.println("extractworking");
             destB0Extents = new int[3];
             destB0Extents[0] = srcB0Image.getExtents()[0];
             destB0Extents[1] = srcB0Image.getExtents()[1];
             destB0Extents[2] = srcB0Image.getExtents()[2];             
             resultB0String = srcB0Image.getImageName() + "T=" + textB0image.getText();             
             resultB0Image = new ModelImage(srcB0Image.getType(), destB0Extents, resultB0String);
             sliceNum = Integer.parseInt(textB0image.getText());
             

             
             if (resultB0Image != null){
                 subsetAlgo = new AlgorithmSubset(pipeline.DWIImage, resultB0Image, AlgorithmSubset.REMOVE_T, sliceNum);
                 System.out.println("resultB0Image not null");
                 
                 //createProgressBar(srcB0Image.getImageName(), subsetAlgo);
                 
                 subsetAlgo.run();
                 
                 if ( (subsetAlgo.isCompleted() == true) && (resultB0Image != null)) {
                     System.out.println("subsetAlgo completed");
                     try {

                         // put the new image into a new frame
                         new ViewJFrameImage(resultB0Image, null, new Dimension(25, 32));
                     } catch (final OutOfMemoryError error) {
                         MipavUtil.displayError("JDialogSubset reports: out of memory; " + "unable to open a new frame");
                     }
                 }
                 
                 tVolumeRemove = new boolean[srcB0Image.getExtents()[3]];

                 for (int i = 0; i < srcB0Image.getExtents()[3]-1; i++) {
                     System.out.println("removeslicenum" +sliceNum);

                     if (i == sliceNum) {
                         System.out.println("tVolumeRemove = true");
                         tVolumeRemove[i] = true;
                     } else {
                         tVolumeRemove[i] = false;
                     }
                 }
                 System.out.println("tvolumeremove" +tVolumeRemove[1]);
                 
                 destB0RemoveExtents = new int[4];
                 destB0RemoveExtents[0] = srcB0Image.getExtents()[0];
                 destB0RemoveExtents[1] = srcB0Image.getExtents()[1];
                 destB0RemoveExtents[2] = srcB0Image.getExtents()[2];
                 destB0RemoveExtents[3] = srcB0Image.getExtents()[3] - 1;
                 resultB0RemoveString = srcB0Image.getImageName() + "Remove T Voume =" + textB0image.getText();             
                 resultB0RemoveImage = new ModelImage(srcB0Image.getType(), destB0RemoveExtents, resultB0RemoveString);
                 
                 if (resultB0RemoveImage != null){
                     System.out.println("resultreomvebo is not null");
                 removeTSlicesAlgo = new AlgorithmRemoveTSlices(srcB0Image, resultB0RemoveImage, tVolumeRemove);
                 removeTSlicesAlgo.run();
                 
                     if ( (removeTSlicesAlgo.isCompleted() == true) && (resultB0RemoveImage != null)) {
                         System.out.println("removeTSlicesAlgo completed");
                         try {
    
                             // put the new image into a new frame
                             new ViewJFrameImage(resultB0RemoveImage, null, new Dimension(25, 32));
                         } catch (final OutOfMemoryError error) {
                             MipavUtil.displayError("JDialogremovet reports: out of memory; " + "unable to open a new frame");
                         }
                     }
                 }
             }
             
             
             
             
         }
         

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
    public void algorithmPerformed(final AlgorithmBase algorithm) {
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

        // save the completion status for later
        //setComplete(algorithm.isCompleted());

        if (algorithm instanceof AlgorithmRegOAR3D) {

            if (reg3.isCompleted()) {
                final TransMatrix finalMatrix = reg3.getTransform();
                System.err.println(finalMatrix);

                if (doLS) {
                    // System.err.println("OAR3D Matrix: " + finalMatrix);
                    // System.err.println("LS Matrix: " + lsMatrix);

                    finalMatrix.Mult(lsMatrix);
                    // System.err.println("OAR3D x LS: " + finalMatrix);
                }

                if (displayTransform) {
                    final int xdimA = refImage.getExtents()[0];
                    final int ydimA = refImage.getExtents()[1];
                    final int zdimA = refImage.getExtents()[2];
                    final float xresA = refImage.getFileInfo(0).getResolutions()[0];
                    final float yresA = refImage.getFileInfo(0).getResolutions()[1];
                    final float zresA = refImage.getFileInfo(0).getResolutions()[2];

                    final String name = JDialogBase.makeImageName(matchImage.getImageName(), "_register");

                    transform = new AlgorithmTransform(matchImage, finalMatrix, interp2, xresA, yresA, zresA, xdimA,
                            ydimA, zdimA, true, false, pad);

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
                        } catch (final OutOfMemoryError error) {
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

                xOrig = (matchImage.getExtents()[0] - 1.0) / 2.0;
                yOrig = (matchImage.getExtents()[1] - 1.0) / 2.0;
                zOrig = (matchImage.getExtents()[2] - 1.0) / 2.0;
                resX = matchImage.getFileInfo()[0].getResolutions()[0];
                resY = matchImage.getFileInfo()[0].getResolutions()[1];
                resZ = matchImage.getFileInfo()[0].getResolutions()[2];
                xCen = xOrig * resX;
                yCen = yOrig * resY;
                zCen = zOrig * resZ;
                finalMatrix.Inverse();
                xCenNew = xCen * finalMatrix.Get(0, 0) + yCen * finalMatrix.Get(0, 1) + zCen * finalMatrix.Get(0, 2)
                        + finalMatrix.Get(0, 3);
                yCenNew = xCen * finalMatrix.Get(1, 0) + yCen * finalMatrix.Get(1, 1) + zCen * finalMatrix.Get(1, 2)
                        + finalMatrix.Get(1, 3);
                zCenNew = xCen * finalMatrix.Get(2, 0) + yCen * finalMatrix.Get(2, 1) + zCen * finalMatrix.Get(2, 2)
                        + finalMatrix.Get(2, 3);
                Preferences.debug("The geometric center of " + matchImage.getImageName() + " at (" + xCen + ", " + yCen
                        + ", " + zCen + ")\n",Preferences.DEBUG_ALGORITHM);
                if (resultImage != null) {
                    comStr = "moves to (" + nf.format(xCenNew) + ", " + nf.format(yCenNew) + ", " + nf.format(zCenNew)
                            + ") in " + resultImage.getImageName() + ".\n";
                } else {
                    comStr = "moves to (" + nf.format(xCenNew) + ", " + nf.format(yCenNew) + ", " + nf.format(zCenNew)
                            + ").\n";
                }
                Preferences.debug(comStr,Preferences.DEBUG_ALGORITHM);

                if (resultImage != null) {
                    resultImage.getMatrixHolder().replaceMatrices(refImage.getMatrixHolder().getMatrices());

                    for (int i = 0; i < resultImage.getExtents()[2]; i++) {
                        resultImage.getFileInfo(i).setOrigin(refImage.getFileInfo(i).getOrigin());
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
                finalMatrix.saveMatrix(matrixDirectory + File.separator + matchImage.getImageName() + "_To_"
                        + refImage.getImageName() + ".mtx", message);
                Preferences.debug("Saved " + matrixDirectory + File.separator + matchImage.getImageName() + "_To_"
                        + refImage.getImageName() + ".mtx\n",Preferences.DEBUG_FILEIO);


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

            //dispose();
            System.gc();
        }
    }

    /**
     * Accessor to get the result image.
     * 
     * @return Result image.
     */
    public ModelImage getResultImage() {
        return resultImage;
    }

    /**
     * Changes the interpolation box to enabled or disabled depending on if the transform box is checked or not.
     * 
     * @param event Event that triggered this function.
     */
    public void itemStateChanged(final ItemEvent event) {

        if (event.getSource() == transformCheckbox) {
            comboBoxInterp2.setEnabled(transformCheckbox.isSelected());
            labelInterp2.setEnabled(transformCheckbox.isSelected());
        } else if (event.getSource() == fastModeCheckbox) {

            // enable or disable search variables
            fastMode = fastModeCheckbox.isSelected();
            rotateBeginTextX.setEnabled( !fastModeCheckbox.isSelected());
            rotateEndTextX.setEnabled( !fastModeCheckbox.isSelected());
            coarseRateTextX.setEnabled( !fastModeCheckbox.isSelected());
            fineRateTextX.setEnabled( !fastModeCheckbox.isSelected());
            rotateBeginTextY.setEnabled( !fastModeCheckbox.isSelected());
            rotateEndTextY.setEnabled( !fastModeCheckbox.isSelected());
            coarseRateTextY.setEnabled( !fastModeCheckbox.isSelected());
            fineRateTextY.setEnabled( !fastModeCheckbox.isSelected());
            rotateBeginTextZ.setEnabled( !fastModeCheckbox.isSelected());
            rotateEndTextZ.setEnabled( !fastModeCheckbox.isSelected());
            coarseRateTextZ.setEnabled( !fastModeCheckbox.isSelected());
            fineRateTextZ.setEnabled( !fastModeCheckbox.isSelected());
        } else if (event.getSource() == calcCOGCheckbox) {

            // enable or disable search variables
            calcCOG = calcCOGCheckbox.isSelected();
        } else if ( (event.getSource() == weightRadio) || (event.getSource() == noneRadio)
                || (event.getSource() == voiRadio)) {
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
        else if ( (event.getSource() == xRadio) || (event.getSource() == yRadio) || (event.getSource() == zRadio)) {

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
        } else if (event.getSource() == outOfBoundsComboBox) {
            switch (outOfBoundsComboBox.getSelectedIndex()) {
                case 0: // image minimum
                    valueText.setText(String.valueOf(imageMin));
                    valueText.setEnabled(false);
                    break;
                case 1: // If float NaN, else 0
                    if ( (dataType == ModelStorageBase.FLOAT) || (dataType == ModelStorageBase.DOUBLE)
                            || (dataType == ModelStorageBase.ARGB_FLOAT)) {
                        valueText.setText(String.valueOf(Float.NaN));
                    } else {
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
            /*matrixComboBox.addItem("User specified matrix directory");
            matrixComboBox.setSelectedIndex(0);*/
        }
    }

    /**
     * Accessor to set the advanced settings.
     * 
     * @param bracketBound DOCUMENT ME!
     * @param maxIterations DOCUMENT ME!
     * @param numMinima DOCUMENT ME!
     */
    public void setAdvancedSettings(final int bracketBound, final int maxIterations, final int numMinima) {
        this.bracketBound = bracketBound;
        this.maxIterations = maxIterations;
        this.numMinima = numMinima;
    }

    /**
     * Accessor to set the whether or not to calculate the center of gravity (mass).
     * 
     * @param flag <code>true</code> then calculate center of gravity (mass).
     */
    public void setCalcCOG(final boolean flag) {
        calcCOG = flag;
    }

    /**
     * Accessor to set the coarse sample beginX.
     * 
     * @param x Coarse beginX
     */
    public void setCoarseBeginX(final float x) {
        rotateBeginX = x;
    }

    /**
     * Accessor to set the coarse sample beginY.
     * 
     * @param y Coarse beginY
     */
    public void setCoarseBeginY(final float y) {
        rotateBeginY = y;
    }

    /**
     * Accessor to set the coarse sample beginZ.
     * 
     * @param z Coarse beginZ
     */
    public void setCoarseBeginZ(final float z) {
        rotateBeginZ = z;
    }

    /**
     * Accessor to set the coarse sample endX.
     * 
     * @param x Coarse endX
     */
    public void setCoarseEndX(final float x) {
        rotateEndX = x;
    }

    /**
     * Accessor to set the coarse sample endY.
     * 
     * @param y Coarse endY
     */
    public void setCoarseEndY(final float y) {
        rotateEndY = y;
    }

    /**
     * Accessor to set the coarse sample endZ.
     * 
     * @param z Coarse endZ
     */
    public void setCoarseEndZ(final float z) {
        rotateEndZ = z;
    }

    /**
     * Accessor to set the coarse sample rateX.
     * 
     * @param x Coarse rateX
     */
    public void setCoarseRateX(final float x) {
        coarseRateX = x;
    }

    /**
     * Accessor to set the coarse sample rateY.
     * 
     * @param y Coarse rateY
     */
    public void setCoarseRateY(final float y) {
        coarseRateY = y;
    }

    /**
     * Accessor to set the coarse sample rateZ.
     * 
     * @param z Coarse rateZ
     */
    public void setCoarseRateZ(final float z) {
        coarseRateZ = z;
    }

    /**
     * Accessor to set the choice of cost function.
     * 
     * @param x Cost function.
     */
    public void setCostChoice(final int x) {
        cost = x;
    }

    /**
     * Accessor to set the display transform flag.
     * 
     * @param flag <code>true</code> means display the transformed image.
     */
    public void setDisplayTransform(final boolean flag) {
        displayTransform = flag;
    }

    /**
     * Accessor to set the degrees of freedom.
     * 
     * @param x Degrees of freedom
     */
    public void setDOF(final int x) {
        DOF = x;
    }

    /**
     * Accessor to set whether or not to execute the fast mode (skip sub sample and goto last final optimization).
     * 
     * @param flag <code>true</code> then skip to level one (last ) optimization.
     */
    public void setFastMode(final boolean flag) {
        fastMode = flag;
    }

    /**
     * Accessor to set the fine sample rateX.
     * 
     * @param x Fine rateX
     */
    public void setFineRateX(final float x) {
        fineRateX = x;
    }

    /**
     * Accessor to set the fine sample rateY.
     * 
     * @param y Fine rateY
     */
    public void setFineRateY(final float y) {
        fineRateY = y;
    }

    /**
     * Accessor to set the fine sample rateZ.
     * 
     * @param z Fine rateZ
     */
    public void setFineRateZ(final float z) {
        fineRateZ = z;
    }

    /**
     * Accessor to set the input weight image.
     * 
     * @param im Input weight image.
     */
    public void setInputWeightImage(final ModelImage im) {
        inputWeightImage = im;
    }

    /**
     * Accessor to set the initial interpolation.
     * 
     * @param x Interpolation
     */
    public void setInterp(final int x) {
        interp = x;
    }

    /**
     * Accessor to set the final interpolation.
     * 
     * @param x Interpolation
     */
    public void setInterp2(final int x) {
        interp2 = x;
    }

    /**
     * Accessor to set the maximum resolutions flag.
     * 
     * @param flag <code>true</code> then use the maximum of minimums of the resolutions of the images.
     */
    public void setMaxOfMinResol(final boolean flag) {
        maxOfMinResol = flag;
    }

    /**
     * Accessor to set the reference image.
     * 
     * @param im Reference image.
     */
    public void setReferenceImage(final ModelImage im) {
        refImage = im;
    }

    /**
     * Accessor to set the reference weight image.
     * 
     * @param im Reference weight image.
     */
    public void setReferenceWeightImage(final ModelImage im) {
        refWeightImage = im;
    }

    /**
     * Accessor to set whether or not subsampling occurs.
     * 
     * @param doSubsample DOCUMENT ME!
     */
    public void setSubsample(final boolean doSubsample) {
        this.doSubsample = doSubsample;
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
     * @param flag <code>true</code> then only register the parts of the images in the VOIs.
     */
    public void setVoisOnly(final boolean flag) {
        voisOnly = flag;
    }

    /**
     * Accessor to set the weighted images flag.
     * 
     * @param flag <code>true</code> means there are weighted images.
     */
    public void setWeighted(final boolean flag) {
        weighted = flag;
    }

    /**
     * tells how to select fill value for out of bounds data 0 for image minimum 1 for NaN for float, zero otherwise. 2
     * for user defined 3 for image max
     * 
     * @param outOfBoundsIndex
     */
    public void setOutOfBoundsIndex(final int outOfBoundsIndex) {
        this.outOfBoundsIndex = outOfBoundsIndex;
    }

    /**
     * Accessor to set intensity value for out of bounds data
     * 
     * @param fillValue
     */
    public void setFillValue(final float fillValue) {
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

        if (doLS) {
            final JDialogRegistrationLeastSquares lsDialog = new JDialogRegistrationLeastSquares(pipeline.T2frame,
                    matchImage, refImage);
            lsCompleted = lsDialog.getLSCompleted();

            if ( !lsCompleted) {
                lsDialog.dispose();

                return;
            }

            lsMatrix = lsDialog.getResultMatrix();
            lsImage = lsDialog.getResultImage();
            lsDialog.dispose();
        }

        if (voisOnly && !doLS) {
            final float[] refRes = new float[] {refImage.getFileInfo(0).getResolutions()[0],
                    refImage.getFileInfo(0).getResolutions()[1], refImage.getFileInfo(0).getResolutions()[2]};
            final float[] matchRes = new float[] {matchImage.getFileInfo(0).getResolutions()[0],
                    matchImage.getFileInfo(0).getResolutions()[1], matchImage.getFileInfo(0).getResolutions()[2]};

            refWeightImage = new ModelImage(ModelStorageBase.BYTE, refImage.getExtents(), "VOI ref");
            inputWeightImage = new ModelImage(ModelStorageBase.BYTE, matchImage.getExtents(), "VOI match");

            refWeightImage.getFileInfo(0).setResolutions(refRes);
            inputWeightImage.getFileInfo(0).setResolutions(matchRes);

            // make new reference and input images based on the VOIs in them.
            // pass those new images to the registration algorithm
            BitSet mask = refImage.generateVOIMask();
            int imageSize = refImage.getSliceSize() * refImage.getExtents()[2];

            for (int i = 0; i < imageSize; i++) {

                if ( !mask.get(i)) {
                    refWeightImage.set(i, 0);
                } else {
                    refWeightImage.set(i, 1);
                }
            }

            mask = matchImage.generateVOIMask();
            imageSize = matchImage.getSliceSize() * matchImage.getExtents()[2];

            for (int i = 0; i < imageSize; i++) {

                if ( !mask.get(i)) {
                    inputWeightImage.set(i, 0);
                } else {
                    inputWeightImage.set(i, 1);
                }
            }

            weighted = true;
        } // if (voisOnly)

        if (weighted) {

            if ( !doLS) {
                reg3 = new AlgorithmRegOAR3D(refImage, matchImage, refWeightImage, inputWeightImage, cost, DOF, interp,
                        rotateBeginX, rotateEndX, coarseRateX, fineRateX, rotateBeginY, rotateEndY, coarseRateY,
                        fineRateY, rotateBeginZ, rotateEndZ, coarseRateZ, fineRateZ, maxOfMinResol, doSubsample,
                        doMultiThread, fastMode, bracketBound, maxIterations, numMinima);
            } else {
                reg3 = new AlgorithmRegOAR3D(refImage, lsImage, refWeightImage, inputWeightImage, cost, DOF, interp,
                        rotateBeginX, rotateEndX, coarseRateX, fineRateX, rotateBeginY, rotateEndY, coarseRateY,
                        fineRateY, rotateBeginZ, rotateEndZ, coarseRateZ, fineRateZ, maxOfMinResol, doSubsample,
                        doMultiThread, fastMode, bracketBound, maxIterations, numMinima);
            }
        } else {
            // System.out.println("Reference image name is " +refImage.getImageName());
            // System.out.println("Moving image name is " +matchImage.getImageName());

            if ( !doLS) {
                reg3 = new AlgorithmRegOAR3D(refImage, matchImage, cost, DOF, interp, rotateBeginX, rotateEndX,
                        coarseRateX, fineRateX, rotateBeginY, rotateEndY, coarseRateY, fineRateY, rotateBeginZ,
                        rotateEndZ, coarseRateZ, fineRateZ, maxOfMinResol, doSubsample, doMultiThread, 
                        fastMode, bracketBound, maxIterations, numMinima);
                reg3.setJTEM(doJTEM);
            } else {
                System.err.println("Sending LS Image to OAR3D algorithm");
                reg3 = new AlgorithmRegOAR3D(refImage, lsImage, cost, DOF, interp, rotateBeginX, rotateEndX,
                        coarseRateX, fineRateX, rotateBeginY, rotateEndY, coarseRateY, fineRateY, rotateBeginZ,
                        rotateEndZ, coarseRateZ, fineRateZ, maxOfMinResol, doSubsample, doMultiThread, fastMode, bracketBound,
                        maxIterations, numMinima);

            }
        }

        reg3.addListener(this);


            reg3.run();


    }



  
    /**
     * Build advanced settings dialog. Returns JDialog.
     * 
     * @param bracketBound DOCUMENT ME!
     * @param maxIter DOCUMENT ME!
     * @param numMinima DOCUMENT ME!
     * 
     * @return DOCUMENT ME!
     */
  /*  private JDialog buildAdvancedDialog(final int bracketBound, final int maxIter, final int numMinima) {
        serif12 = MipavUtil.font12;
        serif12B = MipavUtil.font12B;

        advancedDialog = new JDialog(this, "Advanced OAR settings", false);
        // Parent is the JDialogRegistrationOAR3D, title, modal
        // Changed to non-modal after adding Help button 12/17/07

        // Setting panel
        final JPanel settingsPanel = new JPanel();
        settingsPanel.setBorder(BorderFactory.createTitledBorder("Optimization settings"));
        settingsPanel.setLayout(new BoxLayout(settingsPanel, BoxLayout.Y_AXIS));

        final JPanel bracketPanel = new JPanel();
        bracketPanel.setLayout(new BorderLayout(1, 3)); // BorderLayout(int hgap, int vgap)
        bracketPanel.setBorder(BorderFactory.createEmptyBorder(0, 10, 0, 10));

        final JLabel bracketBoundLabel = new JLabel("Multiple of tolerance to bracket the minimum: ",
                SwingConstants.LEFT);
        bracketPanel.add(bracketBoundLabel, BorderLayout.WEST);
        bracketPanel.setToolTipText("Used for translation, scale and skew.");
        bracketBoundText = new JTextField(String.valueOf(bracketBound), 5);
        bracketBoundText.addFocusListener(this);
        bracketPanel.add(bracketBoundText, BorderLayout.CENTER);

        final JLabel bracketInstruct = new JLabel("Recommended values 10-60.", SwingConstants.RIGHT);
        bracketPanel.add(bracketInstruct, BorderLayout.SOUTH);

        final JPanel maxIterPanel = new JPanel();
        maxIterPanel.setLayout(new BorderLayout(1, 3));
        maxIterPanel.setBorder(BorderFactory.createEmptyBorder(0, 10, 0, 10));

        final JLabel maxIterationsLabel = new JLabel("Number of iterations: ", SwingConstants.LEFT);
        maxIterPanel.add(maxIterationsLabel, BorderLayout.WEST);
        maxIterPanel.setToolTipText("Used for levelOne. Other levels are multiples of this #.");
        maxIterationsText = new JTextField(String.valueOf(maxIter), 5);
        maxIterationsText.addFocusListener(this);

        maxIterPanel.add(maxIterationsText, BorderLayout.CENTER);

        final JLabel maxIterInstruct = new JLabel("Recommended value 1-5.", SwingConstants.RIGHT);
        maxIterPanel.add(maxIterInstruct, BorderLayout.SOUTH);

        final JPanel numMinPanel = new JPanel();
        numMinPanel.setLayout(new BorderLayout(1, 3));
        numMinPanel.setBorder(BorderFactory.createEmptyBorder(0, 10, 0, 10));

        final JLabel numMinLabel = new JLabel("Number of minima from Level 8 to test at Level 4: ", SwingConstants.LEFT);
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
        settingsPanel.add(jtemCheckbox);
        settingsPanel.add(Box.createVerticalStrut(10));
        settingsPanel.add(fastModeCheckbox);
        // settingsPanel.add(Box.createVerticalStrut(10));
        // settingsPanel.add(calcCOGCheckbox, Component.LEFT_ALIGNMENT);

        advancedDialog.getContentPane().add(settingsPanel, BorderLayout.NORTH);

        // Okay-Cancel Panel
        final JPanel okayCancelPanel = new JPanel(new FlowLayout());
        final JButton advCancelButton = new JButton("Cancel");
        advCancelButton.setActionCommand("AdvancedCancel");
        advCancelButton.addActionListener(this);
        advCancelButton.setPreferredSize(new Dimension(120, 30));
        advCancelButton.setFont(serif12B);

        // okayCancelPanel.add(cancelButton);
        final JButton okayButton = new JButton("OK");
        okayButton.setActionCommand("AdvancedOkay");
        okayButton.addActionListener(this);
        okayButton.setPreferredSize(new Dimension(120, 30));
        okayButton.setFont(serif12B);

        // Help Button
        final JButton helpButton = new JButton("Help");
        helpButton.setActionCommand("AdvancedHelp");
        helpButton.addActionListener(this);
        helpButton.setPreferredSize(new Dimension(120, 30));
        helpButton.setFont(serif12B);

        okayCancelPanel.add(okayButton);
        okayCancelPanel.add(advCancelButton);
        okayCancelPanel.add(helpButton);

        advancedDialog.getContentPane().add(okayCancelPanel, BorderLayout.SOUTH);

        final Rectangle dialogBounds = this.getBounds();
        advancedDialog.setLocation(
                (int) ( (Toolkit.getDefaultToolkit().getScreenSize().width * 0.75) - (dialogBounds.width / 2)),
                (Toolkit.getDefaultToolkit().getScreenSize().height / 2) - (dialogBounds.height / 2));

        advancedDialog.pack();
        advancedDialog.setVisible(true);

        return advancedDialog;
    }*/

    /**
     * Builds a list of images. Returns combobox.
     * 
     * @param image DOCUMENT ME!
     * 
     * @return Newly created combo box.
     */
    private JComboBox buildImgComboBox(final ModelImage image) {
        final JComboBox comboBox = new JComboBox();
        comboBox.setFont(serif12);
        comboBox.setBackground(Color.white);

        final Enumeration<String> names = UI.getRegisteredImageNames();

        while (names.hasMoreElements()) {
            final String name = names.nextElement();

            if ( !name.equals(image.getImageName())) {
                final ModelImage img = UI.getRegisteredImageByName(name);

                if ( (image.getNDims() == img.getNDims()) && (image.isColorImage() == img.isColorImage())
                        && (UI.getFrameContainingImage(img) != null)) {
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
       /* matchImage.calcMinMax();
        imageMin = matchImage.getMin();
        imageMax = matchImage.getMax();
        dataType = matchImage.getFileInfo()[0].getDataType();*/
        setForeground(Color.black);

        final JPanel optPanel = new JPanel();
        optPanel.setLayout(new GridBagLayout());
        optPanel.setBorder(buildTitledBorder("Input Options"));

        //final String matchName = matchImage.getImageName();
        /*final JLabel labelImage = new JLabel("Upload B0 to Register to T2");
        labelImage.setForeground(Color.black);
        labelImage.setFont(serif12);
        labelImage.setAlignmentX(Component.LEFT_ALIGNMENT);
        
        textB0image = new JTextField();
        textB0image.setPreferredSize(new Dimension(275, 21));
        textB0image.setEditable(true);
        textB0image.setBackground(Color.white);
        textB0image.setFont(MipavUtil.font12);

        
        
        openB0Button = new JButton("Browse");
        openB0Button.addActionListener(this);
        openB0Button.setActionCommand("browseB0File");
        openB0Button.setEnabled(true);*/
        
        final JLabel labelImage = new JLabel("Reference Volume Number");
        labelImage.setForeground(Color.black);
        labelImage.setFont(serif12);
        labelImage.setAlignmentX(Component.LEFT_ALIGNMENT);
        
        textB0image = new JTextField("0" ,3);
        //textB0image.setPreferredSize(new Dimension(275, 21));
        textB0image.setEditable(true);
        textB0image.setBackground(Color.white);
        textB0image.setFont(MipavUtil.font12);
        
        openB0Button = new JButton("Extract");
        openB0Button.addActionListener(this);
        openB0Button.setActionCommand("extractB0File");
        openB0Button.setEnabled(true);



        final JLabel labelDOF = new JLabel("Degrees of freedom:");
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

        final JLabel labelCost = new JLabel("Cost function:");
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

        // comboBoxCostFunct.addItem("Correlation ratio smoothed");
        comboBoxCostFunct.addItem("Least squares");

        // comboBoxCostFunct.addItem("Least squares smoothed");
        // comboBoxCostFunct.addItem("Mutual information");
        // comboBoxCostFunct.addItem("Mutual information smoothed");
        if ( !doColor) {
            comboBoxCostFunct.addItem("Normalized cross correlation");
        }

        // comboBoxCostFunct.addItem("Normalized cross correlation smoothed");
        if ( !doColor) {
            comboBoxCostFunct.addItem("Normalized mutual information");
            comboBoxCostFunct.addItem("Normalized mutual information - GPU");
            // comboBoxCostFunct.addItem("Normalized mutual information - GPU2");
        }

        // comboBoxCostFunct.addItem("Normalized mutual information smoothed");
        //This is least squares if doColor, else correlation ratio
        comboBoxCostFunct.setSelectedIndex(0);

        final JLabel labelInterp = new JLabel("Interpolation:");
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



        calcLSBox = new JCheckBox("Initialize registration process by applying Least Squares", false);
        calcLSBox.setFont(serif12);
        calcLSBox.setForeground(Color.black);
        
        multiThreadCheckBox = new JCheckBox("Multi-threading enabled (not deterministic)");
        multiThreadCheckBox.setFont(serif12);
        multiThreadCheckBox.setForeground(Color.black);
        multiThreadCheckBox.setSelected(Preferences.isMultiThreadingEnabled()  &&
                (ThreadUtil.getAvailableCores() > 1));
        multiThreadCheckBox.setEnabled(ThreadUtil.getAvailableCores() > 1);

        final Insets insets = new Insets(0, 2, 0, 2);
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
        optPanel.add(textB0image, gbc);
        gbc.gridx = 2;
        gbc.weightx = 1;
        gbc.fill = GridBagConstraints.HORIZONTAL;
        optPanel.add(openB0Button, gbc);
    

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
        
        gbc.gridx = 0;
        gbc.gridy = 7;
        gbc.weightx = 1;
        gbc.fill = GridBagConstraints.REMAINDER;
        optPanel.add(multiThreadCheckBox, gbc);


        universalCheckbox = new JCheckBox("Apply same rotations to all dimensions.");
        universalCheckbox.setFont(serif12);
        universalCheckbox.setForeground(Color.black);
        universalCheckbox.setSelected(true);
        universalCheckbox.addItemListener(this);

        final ButtonGroup dimensionGroup = new ButtonGroup();

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

        final JPanel xyzPanel = new JPanel(new FlowLayout(FlowLayout.LEFT));
        xyzPanel.setAlignmentX(Component.LEFT_ALIGNMENT);

        xyzPanel.add(xRadio);
        xyzPanel.add(yRadio);
        xyzPanel.add(zRadio);

        // Rotation Range Panel
        rotateRangePanelX = new JPanel(new FlowLayout(FlowLayout.LEFT));
        rotateRangePanelX.setAlignmentX(Component.LEFT_ALIGNMENT);

        final JLabel labelRotateRangeX = new JLabel("Rotation angle sampling range:");
        labelRotateRangeX.setForeground(Color.black);
        labelRotateRangeX.setFont(serif12);

        final JLabel labelRotateRangeToX = new JLabel("to");
        labelRotateRangeToX.setForeground(Color.black);
        labelRotateRangeToX.setFont(serif12);

        final JLabel labelRotateDegreesX = new JLabel("degrees");
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

        final JLabel labelCoarseX = new JLabel("Coarse angle increment: ");
        labelCoarseX.setForeground(Color.black);
        labelCoarseX.setFont(serif12);
        labelCoarseX.setAlignmentX(Component.LEFT_ALIGNMENT);

        final JLabel labelCoarseDegreesX = new JLabel("degrees");
        labelCoarseDegreesX.setFont(serif12);
        coarseRateTextX = new JTextField("15", 3);

        coarsePanelX.add(labelCoarseX);
        coarsePanelX.add(coarseRateTextX);
        coarsePanelX.add(labelCoarseDegreesX);
        coarsePanelX.setAlignmentX(Component.LEFT_ALIGNMENT);

        // Fine sampling rate panel
        finePanelX = new JPanel(new FlowLayout(FlowLayout.LEFT));

        final JLabel labelFineX = new JLabel("Fine angle increment:");
        labelFineX.setForeground(Color.black);
        labelFineX.setFont(serif12);
        labelFineX.setAlignmentX(Component.LEFT_ALIGNMENT);

        final JLabel labelFineDegreesX = new JLabel("degrees");
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

        final JLabel labelRotateRangeY = new JLabel("Rotation angle sampling range:");
        labelRotateRangeY.setForeground(Color.black);
        labelRotateRangeY.setFont(serif12);

        final JLabel labelRotateRangeToY = new JLabel("to");
        labelRotateRangeToY.setForeground(Color.black);
        labelRotateRangeToY.setFont(serif12);

        final JLabel labelRotateDegreesY = new JLabel("degrees");
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

        final JLabel labelCoarseY = new JLabel("Coarse angle increment: ");
        labelCoarseY.setForeground(Color.black);
        labelCoarseY.setFont(serif12);
        labelCoarseY.setAlignmentX(Component.LEFT_ALIGNMENT);

        final JLabel labelCoarseDegreesY = new JLabel("degrees");
        labelCoarseDegreesY.setFont(serif12);

        coarseRateTextY = new JTextField("15", 3);

        coarsePanelY.add(labelCoarseY);
        coarsePanelY.add(coarseRateTextY);
        coarsePanelY.add(labelCoarseDegreesY);
        coarsePanelY.setAlignmentX(Component.LEFT_ALIGNMENT);

        // Fine sampling rate panel
        finePanelY = new JPanel(new FlowLayout(FlowLayout.LEFT));

        final JLabel labelFineY = new JLabel("Fine angle increment:");
        labelFineY.setForeground(Color.black);
        labelFineY.setFont(serif12);
        labelFineY.setAlignmentX(Component.LEFT_ALIGNMENT);

        final JLabel labelFineDegreesY = new JLabel("degrees");
        labelFineDegreesY.setFont(serif12);

        fineRateTextY = new JTextField("6", 3);

        finePanelY.add(labelFineY);
        finePanelY.add(fineRateTextY);
        finePanelY.add(labelFineDegreesY);
        finePanelY.setAlignmentX(Component.LEFT_ALIGNMENT);

        rotateRangePanelZ = new JPanel(new FlowLayout(FlowLayout.LEFT));
        rotateRangePanelZ.setAlignmentX(Component.LEFT_ALIGNMENT);

        final JLabel labelRotateRangeZ = new JLabel("Rotation angle sampling range:");
        labelRotateRangeZ.setForeground(Color.black);
        labelRotateRangeZ.setFont(serif12);

        final JLabel labelRotateRangeToZ = new JLabel("to");
        labelRotateRangeToZ.setForeground(Color.black);
        labelRotateRangeToZ.setFont(serif12);

        final JLabel labelRotateDegreesZ = new JLabel("degrees");
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

        final JLabel labelCoarseZ = new JLabel("Coarse angle increment: ");
        labelCoarseZ.setForeground(Color.black);
        labelCoarseZ.setFont(serif12);
        labelCoarseZ.setAlignmentX(Component.LEFT_ALIGNMENT);

        final JLabel labelCoarseDegreesZ = new JLabel("degrees");
        labelCoarseDegreesZ.setFont(serif12);

        coarseRateTextZ = new JTextField("15", 3);

        coarsePanelZ.add(labelCoarseZ);
        coarsePanelZ.add(coarseRateTextZ);
        coarsePanelZ.add(labelCoarseDegreesZ);
        coarsePanelZ.setAlignmentX(Component.LEFT_ALIGNMENT);

        // Fine sampling rate panel
        finePanelZ = new JPanel(new FlowLayout(FlowLayout.LEFT));

        final JLabel labelFineZ = new JLabel("Fine angle increment:");
        labelFineZ.setForeground(Color.black);
        labelFineZ.setFont(serif12);
        labelFineZ.setAlignmentX(Component.LEFT_ALIGNMENT);

        final JLabel labelFineDegreesZ = new JLabel("degrees");
        labelFineDegreesZ.setFont(serif12);

        fineRateTextZ = new JTextField("6", 3);

        finePanelZ.add(labelFineZ);
        finePanelZ.add(fineRateTextZ);
        finePanelZ.add(labelFineDegreesZ);
        finePanelZ.setAlignmentX(Component.LEFT_ALIGNMENT);

        final ButtonGroup weightGroup = new ButtonGroup();

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

        final JPanel weightPanel = new JPanel(new GridBagLayout());
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

        final JPanel outPanel = new JPanel();
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
        
        //refImage = UI.getRegisteredImageByName((String) comboBoxImage.getSelectedItem());
        
        if (pipeline.T2Image != null) {
            matrixComboBox.addItem(pipeline.T2Image.getImageDirectory());   
        }
       /*if ((matchImage.getImageDirectory() != null) && 
            (!refImage.getImageDirectory().equals(matchImage.getImageDirectory()))){
            matrixComboBox.addItem(matchImage.getImageDirectory());
        }
        if ((UI.getDefaultDirectory() != null) && 
            (!UI.getDefaultDirectory().equals(refImage.getImageDirectory())) &&
            (!UI.getDefaultDirectory().equals(matchImage.getImageDirectory()))) {
            matrixComboBox.addItem(UI.getDefaultDirectory());
        }*/
        /*matrixComboBox.addItem("User specified matrix directory");
        matrixComboBox.setSelectedIndex(0);*/
        
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

        // Note the next 3 checkboxes are initialized here, for cases when the user doesn't
        // choose to edit the Advanced Settings. They will only be made visible in the
        // Advanced Settings dialog.
        sampleCheckbox = new JCheckBox("Subsample image for speed");
        sampleCheckbox.setFont(serif12);
        sampleCheckbox.setForeground(Color.black);
        sampleCheckbox.setSelected(true);
        sampleCheckbox.setEnabled(false);
        sampleCheckbox.setAlignmentX(Component.LEFT_ALIGNMENT);
        
        

        jtemCheckbox = new JCheckBox("Full Powell's Method");
        jtemCheckbox.setFont(serif12);
        jtemCheckbox.setForeground(Color.black);
        jtemCheckbox.setSelected(false);
        jtemCheckbox.setEnabled(false);
        jtemCheckbox.setAlignmentX(Component.LEFT_ALIGNMENT);

        fastModeCheckbox = new JCheckBox("Skip multilevel search.  Assume images are close to alignment.");
        fastModeCheckbox.setFont(serif12);
        fastModeCheckbox.setForeground(Color.black);
        fastModeCheckbox.setSelected(false);
        fastModeCheckbox.setEnabled(false);
        fastModeCheckbox.addItemListener(this);
        fastModeCheckbox.setAlignmentX(Component.LEFT_ALIGNMENT);

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
        settingsPanel.add(sampleCheckbox, gbc);
        gbc.gridx = 0;
        gbc.gridy = 5;
        settingsPanel.add(jtemCheckbox, gbc);
        gbc.gridx = 0;
        gbc.gridy = 6;
        settingsPanel.add(fastModeCheckbox, gbc);

        final JPanel buttonPanel = new JPanel();
        buildOKButton();
        buttonPanel.add(OKButton);
        buildCancelButton();
        buttonPanel.add(cancelButton);
        buildHelpButton();
        buttonPanel.add(helpButton);

        /*final JButton advancedButton = new JButton("Advanced settings");
        advancedButton.setActionCommand("AdvancedSettings");
        advancedButton.addActionListener(this);
        advancedButton.setPreferredSize(new Dimension(140, 30));
        advancedButton.setFont(serif12B);
        buttonPanel.add(advancedButton);*/

        mainT2Panel = new JPanel();
        mainT2Panel.setLayout(new BoxLayout(mainT2Panel, BoxLayout.Y_AXIS));
        mainT2Panel.setBorder(BorderFactory.createEmptyBorder(5, 5, 5, 5));
        optPanel.setAlignmentX(Component.LEFT_ALIGNMENT);
        rotatePanel.setAlignmentX(Component.LEFT_ALIGNMENT);
        weightPanel.setAlignmentX(Component.LEFT_ALIGNMENT);
        outPanel.setAlignmentX(Component.LEFT_ALIGNMENT);
        mainT2Panel.add(optPanel);
        mainT2Panel.add(rotatePanel);
        mainT2Panel.add(weightPanel);
        mainT2Panel.add(outPanel);
        mainT2Panel.add(buttonPanel);
        
        mainT2Panel = new JPanel();
        mainT2Panel.setLayout(new GridBagLayout());
        gbc.gridx = 0;
        gbc.gridy = 0;
        gbc.gridwidth = 1;
        gbc.weightx = .5;
        gbc.weighty = 1;
        mainT2Panel.add(optPanel, gbc);
        gbc.gridx = 0;
        gbc.gridy = 1;
        gbc.gridwidth = 1;
        gbc.weightx = .5;
        gbc.weighty = 1;
        mainT2Panel.add(rotatePanel, gbc);
        gbc.gridx = 0;
        gbc.gridy = 2;
        gbc.gridwidth = 1;
        gbc.weightx = .5;
        gbc.weighty = 1;
        mainT2Panel.add(weightPanel, gbc);
        gbc.gridx = 0;
        gbc.gridy = 3;
        gbc.gridwidth = 1;
        gbc.weightx = .5;
        gbc.weighty = 1;
        mainT2Panel.add(outPanel, gbc);

        gbc.gridx = 0;
        gbc.gridy = 4;
        gbc.gridwidth = 1;
        gbc.weightx = .5;
        gbc.weighty = 1;
        mainT2Panel.add(buttonPanel, gbc);

        gbc.anchor = GridBagConstraints.NORTHWEST;
        gbc.gridx = 1;
        gbc.gridy = 0;
        gbc.weightx = .5;
        gbc.weighty = 1;
        mainT2Panel.add(settingsPanel, gbc);


        /*getContentPane().add(mainPanel);
        getContentPane().add(buttonPanel, BorderLayout.SOUTH);
        pack();*/
        setVisible(true);
    }
    
    public void loadB0File() {

        ViewOpenFileUI openFile = new ViewOpenFileUI(true);          
        final boolean stackFlag = getLastStackFlag();
        ArrayList<Vector<String>> openImagesArrayList = openFile.open(stackFlag);
        final FileIO fileIO = new FileIO();
        //m_kDWIImage = fileIO.readImage(openFile.getImagePath());
        textB0image.setText(openFile.getImagePath());
        m_kB0Image = openFile.getImage();

        Vector<Frame> imageFrameVector = UI.getImageFrameVector();
        for (int i = 0; i<imageFrameVector.size(); i++){
            String imageFrameName = imageFrameVector.get(i).getName();
            String openedFileName = openFile.getFileName();
            if (openedFileName.equals(imageFrameName)){
                b0frame = (ViewJFrameImage) imageFrameVector.get(i);
                break;
            }
        }
        

        
    }
    
    public boolean getLastStackFlag() {
        return this.lastStackFlag;
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
    
    private TitledBorder buildTitledBorder(String title) {
        return new TitledBorder(new EtchedBorder(), title, TitledBorder.LEFT, TitledBorder.CENTER, MipavUtil.font12B,
                Color.black);
    }

    /**
     * Sets the variables needed to call the registration algorithm based on the values entered in the dialog.
     * 
     * @return <code>true</code> if the variables are properly set, <code>false</code> otherwise.
     */
    private boolean setVariables() {
        int i;
        
        refImage = pipeline.T2Image;
        if ( resultB0Image != null){
            System.out.println("b0 image not null");
        matchImage = resultB0Image;
        }
        weighted = weightRadio.isSelected();
        maxOfMinResol = minMaxCheckbox.isSelected();
        voisOnly = voiRadio.isSelected();

        doLS = calcLSBox.isSelected();
        doMultiThread = multiThreadCheckBox.isSelected();

        if (weighted) {
            fileNameWRef = textRef.getText();
            fileNameWInput = textInput.getText();

            try {
                final FileIO fileIO = new FileIO();
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
            } catch (final OutOfMemoryError e) {
                MipavUtil.displayError("Out of memory in JDialogRegistrationOAR3D");

                return false;
            }
        }

        if (doColor) {

            if ( ( !weighted) && ( !voisOnly)) {

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

            if ( ( !weighted) && ( !voisOnly)) {

                switch (comboBoxCostFunct.getSelectedIndex()) {

                    case 0:
                        cost = AlgorithmCostFunctions.CORRELATION_RATIO_SMOOTHED;
                        costName = "CORRELATION_RATIO_SMOOTHED";
                        break;
                    // case 0: cost = AlgorithmCostFunctions.CORRELATION_RATIO; break;

                    case 1:
                        cost = AlgorithmCostFunctions.LEAST_SQUARES_SMOOTHED;
                        costName = "LEAST_SQUARES_SMOOTHED";

                        // cost = AlgorithmCostFunctions.LEAST_SQUARES;
                        // costName = "LEAST_SQUARES_SMOOTHED";
                        break;
                    // case 2: cost = AlgorithmCostFunctions.MUTUAL_INFORMATION_SMOOTHED; break;

                    case 2:
                        cost = AlgorithmCostFunctions.NORMALIZED_XCORRELATION_SMOOTHED;
                        costName = "NORMALIZED_XCORRELATION_SMOOTHED";
                        break;
                    // case 3: cost = AlgorithmCostFunctions.NORMALIZED_MUTUAL_INFORMATION; break;

                    case 3:
                        cost = AlgorithmCostFunctions.NORMALIZED_MUTUAL_INFORMATION_SMOOTHED;
                        costName = "NORMALIZED_MUTUAL_INFORMATION_SMOOTHED";
                        break;

                    case 4:
                        cost = AlgorithmCostFunctions.NORMALIZED_MUTUAL_INFORMATION_GPU;
                        costName = "NORMALIZED_MUTUAL_INFORMATION_GPU";
                        break;

                    case 5:
                        cost = AlgorithmCostFunctions.NORMALIZED_MUTUAL_INFORMATION_GPU_LM;
                        costName = "NORMALIZED_MUTUAL_INFORMATION_GPU_LM";
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
                    // case 2: cost = AlgorithmCostFunctions.MUTUAL_INFORMATION_SMOOTHED_WGT; break;

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
            // case 7: interp = AlgorithmTransform.NEAREST_NEIGHBOR; break;

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

        if ( !JDialogBase.testParameter(rotateBeginTextX.getText(), -360, 360)) {
            showX();
            rotateBeginTextX.requestFocus();
            rotateBeginTextX.selectAll();

            return false;
        } else {
            rotateBeginX = Float.valueOf(rotateBeginTextX.getText()).floatValue();
        }

        if ( !JDialogBase.testParameter(rotateEndTextX.getText(), -360, 360)) {
            showX();
            rotateEndTextX.requestFocus();
            rotateEndTextX.selectAll();

            return false;
        } else {
            rotateEndX = Float.valueOf(rotateEndTextX.getText()).floatValue();
        }

        if ( !JDialogBase.testParameter(coarseRateTextX.getText(), 0.01, 360)) {
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

        if ( ( (rotateEndX - rotateBeginX) / coarseRateX) < 1) {
            final int response = JOptionPane.showConfirmDialog(this,
                    "Warning: with such a large rateX, there will only be 1 sampling.  Continue?", "Sampling warning",
                    JOptionPane.YES_NO_OPTION, JOptionPane.WARNING_MESSAGE);

            if (response == JOptionPane.NO_OPTION) {
                showX();
                coarseRateTextX.requestFocus();
                coarseRateTextX.selectAll();

                return false;
            }
        }

        if ( !JDialogBase.testParameter(fineRateTextX.getText(), 0.01, 360)) {
            showX();
            fineRateTextX.requestFocus();
            fineRateTextX.selectAll();

            return false;
        } else {
            fineRateX = Float.valueOf(fineRateTextX.getText()).floatValue();
        }

        if ( ( (rotateEndX - rotateBeginX) / fineRateX) < 1) {
            final int response = JOptionPane.showConfirmDialog(this,
                    "Warning: with such a large rateX, there will only be 1 sampling.  Continue?", "Sampling warning",
                    JOptionPane.YES_NO_OPTION, JOptionPane.WARNING_MESSAGE);

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

            if ( !JDialogBase.testParameter(rotateBeginTextY.getText(), -360, 360)) {
                showY();
                rotateBeginTextY.requestFocus();
                rotateBeginTextY.selectAll();

                return false;
            } else {
                rotateBeginY = Float.valueOf(rotateBeginTextY.getText()).floatValue();
            }

            if ( !JDialogBase.testParameter(rotateEndTextY.getText(), -360, 360)) {
                showY();
                rotateEndTextY.requestFocus();
                rotateEndTextY.selectAll();

                return false;
            } else {
                rotateEndY = Float.valueOf(rotateEndTextY.getText()).floatValue();
            }

            if ( !JDialogBase.testParameter(coarseRateTextY.getText(), 0.01, 360)) {
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

            if ( ( (rotateEndY - rotateBeginY) / coarseRateY) < 1) {
                final int response = JOptionPane.showConfirmDialog(this,
                        "Warning: with such a large rateY, there will only be 1 sampling.  Continue?",
                        "Sampling warning", JOptionPane.YES_NO_OPTION, JOptionPane.WARNING_MESSAGE);

                if (response == JOptionPane.NO_OPTION) {
                    showY();
                    coarseRateTextY.requestFocus();
                    coarseRateTextY.selectAll();

                    return false;
                }
            }

            if ( !JDialogBase.testParameter(fineRateTextY.getText(), 0.01, 360)) {
                showY();
                fineRateTextY.requestFocus();
                fineRateTextY.selectAll();

                return false;
            } else {
                fineRateY = Float.valueOf(fineRateTextY.getText()).floatValue();
            }

            if ( ( (rotateEndY - rotateBeginY) / fineRateY) < 1) {
                final int response = JOptionPane.showConfirmDialog(this,
                        "Warning: with such a large rateY, there will only be 1 sampling.  Continue?",
                        "Sampling warning", JOptionPane.YES_NO_OPTION, JOptionPane.WARNING_MESSAGE);

                if (response == JOptionPane.NO_OPTION) {
                    showY();
                    coarseRateTextY.requestFocus();
                    coarseRateTextY.selectAll();

                    return false;
                }
            }

            if ( !JDialogBase.testParameter(rotateBeginTextZ.getText(), -360, 360)) {
                showZ();
                rotateBeginTextZ.requestFocus();
                rotateBeginTextZ.selectAll();

                return false;
            } else {
                rotateBeginZ = Float.valueOf(rotateBeginTextZ.getText()).floatValue();
            }

            if ( !JDialogBase.testParameter(rotateEndTextZ.getText(), -360, 360)) {
                showZ();
                rotateEndTextZ.requestFocus();
                rotateEndTextZ.selectAll();

                return false;
            } else {
                rotateEndZ = Float.valueOf(rotateEndTextZ.getText()).floatValue();
            }

            if ( !JDialogBase.testParameter(coarseRateTextZ.getText(), 0.01, 360)) {
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

            if ( ( (rotateEndZ - rotateBeginZ) / coarseRateZ) < 1) {
                final int response = JOptionPane.showConfirmDialog(this,
                        "Warning: with such a large rateZ, there will only be 1 sampling.  Continue?",
                        "Sampling warning", JOptionPane.YES_NO_OPTION, JOptionPane.WARNING_MESSAGE);

                if (response == JOptionPane.NO_OPTION) {
                    showZ();
                    coarseRateTextZ.requestFocus();
                    coarseRateTextZ.selectAll();

                    return false;
                }
            }

            if ( !JDialogBase.testParameter(fineRateTextZ.getText(), 0.01, 360)) {
                showZ();
                fineRateTextZ.requestFocus();
                fineRateTextZ.selectAll();

                return false;
            } else {
                fineRateZ = Float.valueOf(fineRateTextZ.getText()).floatValue();
            }

            if ( ( (rotateEndZ - rotateBeginZ) / fineRateZ) < 1) {
                final int response = JOptionPane.showConfirmDialog(this,
                        "Warning: with such a large rateZ, there will only be 1 sampling.  Continue?",
                        "Sampling warning", JOptionPane.YES_NO_OPTION, JOptionPane.WARNING_MESSAGE);

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
            ViewVOIVector VOIs = refImage.getVOIs();
            int nVOI = VOIs.size();

            if (nVOI < 1) {
                MipavUtil
                        .displayError("There must be at least one VOI in " + refImage.getImageName() + " to register.");

                return false;
            }

            VOIs = matchImage.getVOIs();
            nVOI = VOIs.size();

            if (nVOI < 1) {
                MipavUtil.displayError("There must be at least one VOI in " + matchImage.getImageName()
                        + " to register.");

                return false;
            }
        } // if (voisOnly)

        doSubsample = sampleCheckbox.isSelected();
        doJTEM = jtemCheckbox.isSelected();

        fillValue = Float.valueOf(valueText.getText()).floatValue();
        outOfBoundsIndex = outOfBoundsComboBox.getSelectedIndex();
        if (outOfBoundsIndex == 2) {
            // user defined value
            boolean success = testType(dataType, fillValue);
            if ( !success) {
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
     * Determine if the value is in the image type range and within the float range since AlgorithmTransform does not
     * use double buffers.
     * 
     * @param type image type
     * @param value value tested
     * 
     * @return true if value is within acceptable range
     */
    private boolean testType(final int type, final float value) {

        if (type == ModelStorageBase.BOOLEAN) {

            if ( (value < 0) || (value > 1)) {
                return false;
            } else {
                return true;
            }
        } else if (type == ModelStorageBase.BYTE) {

            if ( (value < -128) || (value > 127)) {
                return false;
            } else {
                return true;
            }
        } else if (type == ModelStorageBase.UBYTE) {

            if ( (value < 0) || (value > 255)) {
                return false;
            } else {
                return true;
            }
        } else if (type == ModelStorageBase.SHORT) {

            if ( (value < -32768) || (value > 32767)) {
                return false;
            } else {
                return true;
            }
        } else if (type == ModelStorageBase.USHORT) {

            if ( (value < 0) || (value > 65535)) {
                return false;
            } else {
                return true;
            }
        } else if (type == ModelStorageBase.INTEGER) {

            if ( (value < Integer.MIN_VALUE) || (value > Integer.MAX_VALUE)) {
                return false;
            } else {
                return true;
            }
        } else if (type == ModelStorageBase.UINTEGER) {

            if ( (value < 0) || (value > 4294967295L)) {
                return false;
            } else {
                return true;
            }
        } else if (type == ModelStorageBase.LONG) {

            if ( (value < Long.MIN_VALUE) || (value > Long.MAX_VALUE)) {
                return false;
            } else {
                return true;
            }
        } else if (type == ModelStorageBase.FLOAT) {

            if ( (value < -Float.MAX_VALUE) || (value > Float.MAX_VALUE)) {
                return false;
            } else {
                return true;
            }
        } else if (type == ModelStorageBase.DOUBLE) {
            // Float buffers are used in the AlgorithmTransform routines
            if ( (value < -Float.MAX_VALUE) || (value > Float.MAX_VALUE)) {
                return false;
            } else {
                return true;
            }
        } else if (type == ModelStorageBase.ARGB) {

            if ( (value < 0) || (value > 255)) {
                return false;
            } else {
                return true;
            }
        } else if (type == ModelStorageBase.ARGB_USHORT) {

            if ( (value < 0) || (value > 65535)) {
                return false;
            } else {
                return true;
            }
        } else if (type == ModelStorageBase.ARGB_FLOAT) {

            if ( (value < -Float.MAX_VALUE) || (value > Float.MAX_VALUE)) {
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
                return new String("Perform a linear registration of one 3D volume to a target 3D volume.");
            }

            public String getDescriptionLong() {
                return new String("Perform a linear registration of one 3D volume to a target 3D volume.");
            }

            public String getShortLabel() {
                return new String("OAR3D");
            }

            public String getLabel() {
                return new String("Optimized automatic registration 3D");
            }

            public String getName() {
                return new String("Optimized automatic registration 3D");
            }

            public Set<ImageRequirements> getInputImageRequirements() {
                return EnumSet.of(ImageRequirements.NDIM_3);
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
            // match image
            table.put(new ParameterExternalImage(AlgorithmParameters.getInputImageLabel(1)));

            table.put(new ParameterExternalImage("reference_image"));

            table.put(new ParameterBoolean("do_use_weight_images", false));
            Parameter p = new ParameterExternalImage("input_weight_image");
            p.setParentCondition(table.getParameter("do_use_weight_images"), "true");
            table.put(p);
            p = new ParameterExternalImage("reference_weight_image");
            p.setParentCondition(table.getParameter("do_use_weight_images"), "true");
            table.put(p);

            table.put(new ParameterInt("degrees_of_freedom", 12));
            table.put(new ParameterInt("initial_interpolation_type", 0));
            table.put(new ParameterInt("final_interpolation_type", 0));
            table.put(new ParameterInt("cost_function_type", 1));
            table.put(new ParameterList("rotate_begin", Parameter.PARAM_FLOAT, "-30,-30,-30"));
            table.put(new ParameterList("rotate_end", Parameter.PARAM_FLOAT, "30,30,30"));
            table.put(new ParameterList("coarse_rate", Parameter.PARAM_FLOAT, "15,15,15"));
            table.put(new ParameterList("fine_rate", Parameter.PARAM_FLOAT, "6,6,6"));
            table.put(new ParameterBoolean("do_display_transform", true));
            table.put(new ParameterBoolean("do_use_max_of_min_resolutions", true));
            table.put(new ParameterBoolean("do_subsample", true));
            table.put(new ParameterBoolean("do_use_fast_mode", true));
            table.put(new ParameterBoolean("do_calc_COG", true));

            table.put(new ParameterInt("out_of_bounds_index", 0));
            p = new ParameterFloat("fill_value", 0);
            p.setParentCondition(table.getParameter("out_of_bounds_index"), "2");
            table.put(p);
            table.put(new ParameterString("matrix_directory"));

            table.put(new ParameterInt("bracket_bound", 10));
            table.put(new ParameterInt("max_iterations", 2));
            table.put(new ParameterInt("num_minima", 3));
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
            if (getResultImage() != null) {
                // algo produced a new result image
                return getResultImage().getImageName();
            }
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
        return isActionComplete();
    }
    
    

}
