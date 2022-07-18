package nibib.spim;


// MIPAV is freely available from http://mipav.cit.nih.gov

// THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
// EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES
// OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
// NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
// HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY,
// WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
// FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE
// OR OTHER DEALINGS IN THE SOFTWARE.

/*****************************************************************
 ****************************************************************** 
 The MIPAV application is intended for research use only. This application has NOT been approved for ANY diagnostic
 * use by the Food and Drug Administration. There is currently no approval process pending.
 * 
 * This software may NOT be used for diagnostic purposes.
 * 
 ****************************************************************** 
 ******************************************************************/

import gov.nih.mipav.plugins.JDialogStandaloneScriptablePlugin;

import gov.nih.mipav.model.algorithms.AlgorithmBase;
import gov.nih.mipav.model.algorithms.AlgorithmInterface;
import gov.nih.mipav.model.algorithms.OpenCLAlgorithmBase;
import gov.nih.mipav.model.algorithms.filters.OpenCL.filters.OpenCLAlgorithmDeconvolution;
import gov.nih.mipav.model.algorithms.utilities.AlgorithmMaximumIntensityProjection;
import gov.nih.mipav.model.algorithms.utilities.AlgorithmRotate;
import gov.nih.mipav.model.scripting.ParserException;
import gov.nih.mipav.model.scripting.parameters.ParameterFactory;
import gov.nih.mipav.model.structures.ModelImage;

import gov.nih.mipav.view.MipavUtil;
import gov.nih.mipav.view.Preferences;
import gov.nih.mipav.view.ViewJFrameImage;
import gov.nih.mipav.view.ViewUserInterface;
import gov.nih.mipav.view.dialogs.GuiBuilder;
import gov.nih.mipav.view.dialogs.JDialogBase;

import java.awt.*;
import java.awt.event.*;
import java.io.File;
import java.io.FileNotFoundException;
import java.util.*;

import javax.swing.*;
import javax.swing.table.DefaultTableModel;

import nibib.spim.PlugInAlgorithmGenerateFusion.SampleMode;


/**
 * Class for performing image fusion based on reference image and transformation matrix. Option to output
 * geometric/arithmetic mean.
 * 
 * @version January 27, 2015
 * @see JDialogBase
 * @see AlgorithmInterface
 * 
 * @author Justin Senseney (SenseneyJ@mail.nih.gov)
 * @see http://mipav.cit.nih.gov
 */
public class PlugInDialogGenerateFusion extends JDialogStandaloneScriptablePlugin implements AlgorithmInterface {

    // ~ Static fields/initializers
    // -------------------------------------------------------------------------------------

    private static final long serialVersionUID = 7916311305902468003L;

    public static final String XPROJ = "XProj";

    public static final String YPROJ = "YProj";

    public static final String ZPROJ = "ZProj";

    final String initAriLoc = new File(Preferences.getImageDirectory()).getParent() + File.separator + "AriMean" + File.separator;

    final String initGeoLoc = new File(Preferences.getImageDirectory()).getParent() + File.separator + "GeoMean" + File.separator;

    final String initTransformPrefusionLoc = new File(Preferences.getImageDirectory()).getParent() + File.separator + "PrefusionTransform" + File.separator;

    final String initBasePrefusionLoc = new File(Preferences.getImageDirectory()).getParent() + File.separator + "PrefusionBase" + File.separator;

    final String initDeconvLoc = new File(Preferences.getImageDirectory()).getParent() + File.separator + "Deconvolution" + File.separator;

    final String initRegLoc = new File(Preferences.getImageDirectory()).getParent() + File.separator + "register2DAll" + File.separator;

    // ~ Instance fields
    // ------------------------------------------------------------------------------------------------

    private ModelImage resultImage = null;

    /** This source image is typically set by the constructor */
    private ModelImage image;

    /** This is your algorithm */
    private PlugInAlgorithmGenerateFusion generateFusionAlgo = null;

    private GridBagConstraints gbc;

    private JTextField mtxFileLocText, mtxFileDirectoryText, transformFileLocText, baseFileLocText, register2DFileDirectoryText;

    private JCheckBox geometricMeanShowBox, arithmeticMeanShowBox, interImagesBox;

    private JPanel okCancelPanel;

    private JTextField transformImageText, baseImageText;

    private JCheckBox doShowPrefusionBox;

    private String mtxFileLoc;

    private String mtxFileDirectory;

    private File[] baseImageAr, transformImageAr;

    private boolean doShowPreFusion, doInterImages;

    private boolean showGeoMean, showAriMean;

    private String transformFileDir, baseFileDir;

    private String register2DFileDirString;

    private File register2DFileDir;

    private String baseImage;

    private JTextField resXText, resYText, resZText;

    private JTextField concurrentNumText;

    private JCheckBox doThresholdBox;

    private JTextField thresholdIntensityText;

    private boolean doThreshold;

    private double resX, resY, resZ;

    private int concurrentNum;

    private double thresholdIntensity;

    private JTextField xMovementText, yMovementText, zMovementText;

    private Integer xMovement, yMovement, zMovement;

    private int xMove, yMove, zMove;

    private SampleMode mode;

    private int modeNum;

    private JComboBox modeOption;

    private JCheckBox doSmartMovementBox;

    private boolean doSmartMovement;

    private int stepSize;

    private int maxX, maxY, maxZ;

    private int minX, minY, minZ;

    private JTextField minXText, minYText, minZText;

    private JTextField maxXText, maxYText, maxZText;

    private JTextField stepSizeText;

    private JCheckBox arithmeticMeanSaveBox;

    private JCheckBox geometricMeanSaveBox;

    private JTextField arithmeticMeanFolderText, geometricMeanFolderText;

    private boolean saveGeoMean;

    private boolean saveAriMean;

    private File geoMeanDir;

    private String geoMeanDirString;

    private File ariMeanDir;

    private String ariMeanDirString;

    private JTextField savePrefusionTransformFolderText;

    private JTextField savePrefusionBaseFolderText;

    private JCheckBox doSavePrefusionBox;

    private boolean savePrefusion;

    private File prefusionBaseDir;

    private String prefusionBaseDirString;

    private File prefusionTransformDir;

    private String prefusionTransformDirString;

    private JTextField transformAriWeightText, baseAriWeightText, transformGeoWeightText, baseGeoWeightText;

    /** Weighting scheme for arithmetic and geometric weighting, unweighted by default. */
    private double baseAriWeight = 1, transformAriWeight = 1, baseGeoWeight = 1, transformGeoWeight = 1;

    private JTextField rangeFusionText;

    private JTable fusionConfirmTable;

    /** Whether to show/save the maximum intensity projections. */
    private boolean showMaxProj, saveMaxProj;

    /** Whether to save/show max projection of created images */
    private JCheckBox doShowMaxProjBox, doSaveMaxProjBox;

    /** Checkboxes for computing x, y, and z max projections */
    private JCheckBox doXMaxBox, doYMaxBox, doZMaxBox;

    private boolean xMaxBoxSelected;

    private boolean yMaxBoxSelected;

    private boolean zMaxBoxSelected;

    /** Max projection lower intensity threshold text field */
    private JTextField minThresholdMaxProjText;

    private float minThreshold;

    /** MIP algorithm for later processing */
    private AlgorithmMaximumIntensityProjection[] maxAlgo;

    private JTextField slidingWindowText;

    private JCheckBox doSlideWindowBox;

    private int slidingWindow;

    /** Combobox for selecting save result type */
    private JComboBox saveTypeText;

    /** File format for saving result images */
    private String saveType;

    /** Whether to perform deconvolution. */
    private boolean doDeconv;

    /** Checkbox to control whether to perform the deconvolution step. */
    private JCheckBox deconvPerformCheckbox;

    /** Checkbox to control whether to leave the deconvolution output image frames open. */
    private boolean deconvShowResults;

    /** Checkbox to control whether to leave the deconvolution output image frames open. */
    private JCheckBox deconvShowResultsCheckbox;

    private final static int JavaPlatform = 1;

    private final static int OpenCLPlatform = 2;

    private int deconvPlatform;

    private ButtonGroup platformGroup;

    private JRadioButton OpenCLButton;

    private JRadioButton JavaButton;

    /** Panel containing deconvolution parameters. */
    private JPanel deconvParamPanel;

    private ButtonGroup deconvolutionGroup;

    private JRadioButton jointButton;

    private JRadioButton arithmeticMeanButton;

    private JRadioButton geometricMeanButton;

    private int deconvolutionMethod = OpenCLAlgorithmDeconvolution.JOINT_DECON;

    /** The number of iterations to perform during deconvolution. */
    private int deconvIterations;

    /** Text field for the number of deconvolution iterations. */
    private JTextField deconvIterationsText;

    /** The sigmas to use for ImageA during deconvolution. */
    private float[] deconvSigmaA;

    /** Text fields for the Image A deconvolution sigmas in each dimension. */
    private JTextField deconvSigmaAXText, deconvSigmaAYText, deconvSigmaAZText;

    /** The sigmas to use for ImageB during deconvolution. */
    private float[] deconvSigmaB;

    /** Text fields for the Image B deconvolution sigmas in each dimension. */
    private JTextField deconvSigmaBXText, deconvSigmaBYText, deconvSigmaBZText;

    /** Whether to use the deconvolution sigma conversion factor. */
    private boolean useDeconvSigmaConversionFactor;

    /** Checkbox indicating whether to apply a deconvolution sigma conversion factor. */
    private JCheckBox deconvUseSigmaConversionFactor;

    /** The directory to save deconvolved files to. */
    private File deconvDir;

    private String deconvDirString;

    /** The text field for setting the directory to save deconvolved files to. */
    private JTextField saveDeconvFolderText;

    private ButtonGroup registrationGroup;

    private JRadioButton noRegisterButton;

    private JRadioButton registerOneButton;

    private JRadioButton registerAllButton;

    private JRadioButton noRegister2DButton;

    private JRadioButton register2DOneButton;

    private JRadioButton register2DAllButton;

    private boolean registerOne = true;

    private boolean registerAll = false;

    private boolean noRegister2D = false;

    private boolean register2DOne = false;

    private boolean register2DAll = false;

    private JLabel labelTimeExt;

    private JTextField textTimeExt;

    private int timeNum; // The extension number in the file name

    private int timeIndex; // The index of the file in the array with the desired timeNum

    private JPanel coarsePanelX, coarsePanelY, coarsePanelZ;

    /** DOCUMENT ME! */
    private JTextField coarseRateTextX, coarseRateTextY, coarseRateTextZ;

    private JPanel finePanelX, finePanelY, finePanelZ;

    /** DOCUMENT ME! */
    private JTextField fineRateTextX, fineRateTextY, fineRateTextZ;

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

    private JCheckBox universalCheckbox;

    private boolean xSelected = true;

    private boolean ySelected = false;

    private JRadioButton xRadio;

    private JRadioButton yRadio;

    private JRadioButton zRadio;

    private JTabbedPane tabbedPane;

    private ButtonGroup baseImageRotation;

    private JRadioButton noBaseRotationButton;

    private JRadioButton minusXBaseRotationButton;

    private JRadioButton plusXBaseRotationButton;

    private JRadioButton minusYBaseRotationButton;

    private JRadioButton plusYBaseRotationButton;

    private JRadioButton invertYBaseRotationButton;

    private ButtonGroup transformImageRotation;

    private JRadioButton noTransformRotationButton;

    private JRadioButton minusXTransformRotationButton;

    private JRadioButton plusXTransformRotationButton;

    private JRadioButton minusYTransformRotationButton;

    private JRadioButton plusYTransformRotationButton;

    private JRadioButton invertYTransformRotationButton;

    private int baseRotation = -1;

    private int transformRotation = AlgorithmRotate.Y_AXIS_MINUS;

    // ~ Constructors
    // ---------------------------------------------------------------------------------------------------

    /**
     * Constructor used for instantiation during script execution (required for dynamic loading).
     */
    public PlugInDialogGenerateFusion() {}

    /**
     * Creates new dialog for kidney segmentation from an abdominal cavity image using a plugin.
     * 
     * @param theParentFrame Parent frame.
     * @param im Source image.
     */
    public PlugInDialogGenerateFusion(final boolean modal) {
        super(modal);

        init();
    }

    // ~ Methods
    // --------------------------------------------------------------------------------------------------------

    // ************************************************************************
    // ************************** Event Processing ****************************
    // ************************************************************************

    /**
     * Closes dialog box when the OK button is pressed and calls the algorithm.
     * 
     * @param event Event that triggers function.
     */
    @Override
    public void actionPerformed(final ActionEvent event) {
        final String command = event.getActionCommand();

        if (command.equals("OK")) {
            if (setVariables()) {
                callAlgorithm();
            }
        } else if (command.equals("Script")) {
            callAlgorithm();
        } else if (command.equals("Cancel")) {
            // dispose();
            this.windowClosing(new WindowEvent(this, WindowEvent.WINDOW_CLOSING));
        } else {
            super.actionPerformed(event);
        }
        // System.out.print(this.getSize());
    } // end actionPerformed()

    /**
     * This method is required if the AlgorithmPerformed interface is implemented. It is called by the algorithm when it
     * has completed or failed to to complete, so that the dialog can be display the result image and/or clean up.
     * 
     * @param algorithm Algorithm that caused the event.
     */
    @Override
    public void algorithmPerformed(final AlgorithmBase algorithm) {
        if (algorithm instanceof PlugInAlgorithmGenerateFusion) {
            Preferences.debug("Elapsed: " + algorithm.getElapsedTime());

            if ( (generateFusionAlgo.isCompleted() == true)) {
                final Collection<ModelImage> list = generateFusionAlgo.getResultImageList();
                synchronized (list) {
                    final Iterator<ModelImage> itr = list.iterator();
                    while (itr.hasNext()) {
                        new ViewJFrameImage(itr.next());
                    }
                }
                insertScriptLine();
            }

            if (generateFusionAlgo != null) {
                generateFusionAlgo.finalize();
                generateFusionAlgo = null;
            }

            // dispose();
            if (super.isExitRequired()) {
                ViewUserInterface.getReference().windowClosing(new WindowEvent(this, WindowEvent.WINDOW_CLOSING));
            }
        }
    } // end algorithmPerformed()

    /**
     * Once all the necessary variables are set, call the kidney segmentation algorithm
     */
    @Override
    protected void callAlgorithm() {

        try {

            generateFusionAlgo = new PlugInAlgorithmGenerateFusion(registerOne, registerAll, noRegister2D, register2DOne, register2DAll, register2DFileDir,
                    rotateBeginX, rotateEndX, coarseRateX, fineRateX, rotateBeginY, rotateEndY, coarseRateY, fineRateY, rotateBeginZ, rotateEndZ, coarseRateZ,
                    fineRateZ, doShowPreFusion, doInterImages, showGeoMean, showAriMean, showMaxProj, doThreshold, resX, resY, resZ, concurrentNum,
                    thresholdIntensity, mtxFileLoc, mtxFileDirectory, timeIndex, baseImageAr, transformImageAr, xMovement, yMovement, zMovement, mode, minX,
                    minY, minZ, maxX, maxY, maxZ, stepSize, saveMaxProj, saveGeoMean, geoMeanDir, saveAriMean, ariMeanDir, savePrefusion, prefusionBaseDir,
                    prefusionTransformDir, baseAriWeight, transformAriWeight, baseGeoWeight, transformGeoWeight, maxAlgo, saveType, doDeconv, deconvPlatform,
                    deconvolutionMethod, deconvIterations, deconvSigmaA, deconvSigmaB, useDeconvSigmaConversionFactor, deconvDir, deconvShowResults,
                    baseRotation, transformRotation);

            // This is very important. Adding this object as a listener allows the algorithm to
            // notify this object when it has completed or failed. See algorithm performed event.
            // This is made possible by implementing AlgorithmedPerformed interface
            generateFusionAlgo.addListener(this);
            // createProgressBar("Creating plugin", " ...", generateFusionAlgo);

            setVisible(false); // Hide dialog

            if (isRunInSeparateThread()) {

                // Start the thread as a low priority because we wish to still
                // have user interface work fast.
                if (generateFusionAlgo.startMethod(Thread.MIN_PRIORITY) == false) {
                    MipavUtil.displayError("A thread is already running on this object");
                }
            } else {
                generateFusionAlgo.run();
            }
        } catch (final OutOfMemoryError x) {
            if (resultImage != null) {
                resultImage.disposeLocal(); // Clean up memory of result image
                resultImage = null;
            }

            MipavUtil.displayError("Generic algorithm: unable to allocate enough memory");

            return;
        }

    } // end callAlgorithm()

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
     * 
     * @param baseRotation
     */
    public void setBaseRotation(final int baseRotation) {
        this.baseRotation = baseRotation;
    }

    /**
     * 
     * @param transformRotation
     */
    public void setTransformRotation(final int transformRotation) {
        this.transformRotation = transformRotation;
    }

    /**
     * Used in turning your plugin into a script
     */
    @Override
    protected void setGUIFromParams() {

        registerOne = scriptParameters.getParams().getBoolean("reg_one");
        registerAll = scriptParameters.getParams().getBoolean("reg_all");
        noRegister2D = scriptParameters.getParams().getBoolean("no_reg_2D");
        register2DOne = scriptParameters.getParams().getBoolean("reg_2D_one");
        register2DAll = scriptParameters.getParams().getBoolean("reg_2D_all");
        final float[] rotBegin = scriptParameters.getParams().getList("rotate_begin").getAsFloatArray();
        final float[] rotEnd = scriptParameters.getParams().getList("rotate_end").getAsFloatArray();
        final float[] coarseRates = scriptParameters.getParams().getList("coarse_rate").getAsFloatArray();
        final float[] fineRates = scriptParameters.getParams().getList("fine_rate").getAsFloatArray();

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
        saveAriMean = scriptParameters.getParams().getBoolean("save_arithmetic");
        showAriMean = scriptParameters.getParams().getBoolean("show_arithmetic");
        saveGeoMean = scriptParameters.getParams().getBoolean("save_geometric");
        showGeoMean = scriptParameters.getParams().getBoolean("show_geometric");
        doInterImages = scriptParameters.getParams().getBoolean("do_interImages");
        savePrefusion = scriptParameters.getParams().getBoolean("save_prefusion");
        doShowPreFusion = scriptParameters.getParams().getBoolean("do_show_pre_fusion");
        doThreshold = scriptParameters.getParams().getBoolean("do_threshold");
        saveMaxProj = scriptParameters.getParams().getBoolean("save_max_proj");
        showMaxProj = scriptParameters.getParams().getBoolean("show_max_proj");
        xMaxBoxSelected = scriptParameters.getParams().getBoolean("x_max_box_selected");
        yMaxBoxSelected = scriptParameters.getParams().getBoolean("y_max_box_selected");
        zMaxBoxSelected = scriptParameters.getParams().getBoolean("z_max_box_selected");
        doSmartMovement = scriptParameters.getParams().getBoolean("do_smart_movement");
        thresholdIntensity = scriptParameters.getParams().getDouble("threshold_intensity");
        resX = scriptParameters.getParams().getDouble("res_x");
        resY = scriptParameters.getParams().getDouble("res_y");
        if ( ! (noRegister2D || register2DOne || register2DAll)) {
            resZ = scriptParameters.getParams().getDouble("res_z");
        }

        if (registerOne || registerAll || register2DOne || register2DAll) {
            mtxFileDirectory = scriptParameters.getParams().getString("mtxFileDirectory");
            if (registerOne || register2DOne) {
                timeNum = scriptParameters.getParams().getInt("time_num");
            }
        } else {
            mtxFileLoc = scriptParameters.getParams().getString("mtxFileLoc");
        }
        if (noRegister2D || register2DOne || register2DAll) {
            register2DFileDirString = scriptParameters.getParams().getString("register2DFileDirString");
        }
        transformFileDir = scriptParameters.getParams().getString("spimAFileDir");
        baseFileDir = scriptParameters.getParams().getString("spimBFileDir");

        baseImage = scriptParameters.getParams().getString("baseImage");

        baseRotation = scriptParameters.getParams().getInt("base_rotation");
        transformRotation = scriptParameters.getParams().getInt("transform_rotation");
        concurrentNum = scriptParameters.getParams().getInt("concurrent_num");
        modeNum = scriptParameters.getParams().getInt("mode_num");
        saveType = scriptParameters.getParams().getString("save_type");
        if ( ! (noRegister2D || register2DOne || register2DAll)) {

            if (showMaxProj || saveMaxProj) {
                minThreshold = scriptParameters.getParams().getFloat("min_threshold");
                slidingWindow = scriptParameters.getParams().getInt("sliding_window");
                setMaxProjVariables();
            }

            doDeconv = scriptParameters.getParams().getBoolean("do_deconv");
            if (doDeconv) {
                deconvPlatform = scriptParameters.getParams().getInt("deconv_platform");
                deconvDirString = scriptParameters.getParams().getString("deconvDirString");
                deconvShowResults = scriptParameters.getParams().getBoolean("deconv_show_results");
                deconvolutionMethod = scriptParameters.getParams().getInt("deconvolution_method");
                deconvIterations = scriptParameters.getParams().getInt("deconv_iterations");
                deconvSigmaA = scriptParameters.getParams().getList("deconv_sigmaA").getAsFloatArray();
                deconvSigmaB = scriptParameters.getParams().getList("deconv_sigmaB").getAsFloatArray();
                useDeconvSigmaConversionFactor = scriptParameters.getParams().getBoolean("use_deconv_sigma_conversion_factor");
            } // if (doDeconv)

            if (saveAriMean) {
                ariMeanDirString = scriptParameters.getParams().getString("ariMeanDirString");
            }

            if (saveGeoMean) {
                geoMeanDirString = scriptParameters.getParams().getString("geoMeanDirString");
            }

            if (savePrefusion) {
                prefusionBaseDirString = scriptParameters.getParams().getString("prefusionBaseDirString");
                prefusionTransformDirString = scriptParameters.getParams().getString("prefusionTransformDirString");
            }

            if (showAriMean || saveAriMean) {
                baseAriWeight = scriptParameters.getParams().getDouble("base_ari_weight");
                transformAriWeight = scriptParameters.getParams().getDouble("transform_ari_weight");
            }

            if (showGeoMean || saveGeoMean) {
                baseGeoWeight = scriptParameters.getParams().getDouble("base_geo_weight");
                transformGeoWeight = scriptParameters.getParams().getDouble("transform_geo_weight");
            }

            if ( !doSmartMovement) {
                xMove = scriptParameters.getParams().getInt("x_move");
                xMovement = Integer.valueOf(xMove);
                yMove = scriptParameters.getParams().getInt("y_move");
                yMovement = Integer.valueOf(yMove);
                zMove = scriptParameters.getParams().getInt("z_move");
                zMovement = Integer.valueOf(zMove);
            } else {
                xMovement = yMovement = zMovement = null;

                minX = scriptParameters.getParams().getInt("min_x");
                minY = scriptParameters.getParams().getInt("min_y");
                minZ = scriptParameters.getParams().getInt("min_z");

                maxX = scriptParameters.getParams().getInt("max_x");
                maxY = scriptParameters.getParams().getInt("max_y");
                maxZ = scriptParameters.getParams().getInt("max_z");

                stepSize = scriptParameters.getParams().getInt("step_size");
            }
        } // if (!(noRegister2D || register2DOne || register2DAll))

        HashSet<Integer> includeRange = new HashSet<Integer>();
        if (scriptParameters.getParams().containsParameter("fusion_range")) {
            final String rangeFusion = scriptParameters.getParams().getString("fusion_range");
            if (rangeFusion != null) {
                final String[] ranges = rangeFusion.split("[,;]");
                for (int i = 0; i < ranges.length; i++) {
                    final String[] subset = ranges[i].split("-");
                    int lowerBound = -1, bound = -1;
                    for (int j = 0; j < subset.length; j++) {
                        try {
                            bound = Integer.valueOf(subset[j].trim());
                            if (lowerBound == -1) {
                                lowerBound = bound;
                                includeRange.add(lowerBound);
                            }
                        } catch (final NumberFormatException e) {
                            Preferences.debug("Invalid range specified: " + bound, Preferences.DEBUG_ALGORITHM);
                        }
                    }

                    for (int k = lowerBound + 1; k <= bound; k++) {
                        includeRange.add(k);
                    }
                }
            }

            if (includeRange.size() == 0) {
                includeRange = null;
            }
        }

        populateFileLists(includeRange);

    } // end setGUIFromParams()

    /**
     * Used in turning your plugin into a script
     */
    @Override
    protected void storeParamsFromGUI() throws ParserException {

        scriptParameters.getParams().put(ParameterFactory.newParameter("reg_one", registerOne));
        scriptParameters.getParams().put(ParameterFactory.newParameter("reg_all", registerAll));
        scriptParameters.getParams().put(ParameterFactory.newParameter("no_reg_2D", noRegister2D));
        scriptParameters.getParams().put(ParameterFactory.newParameter("reg_2D_one", register2DOne));
        scriptParameters.getParams().put(ParameterFactory.newParameter("reg_2D_all", register2DAll));
        scriptParameters.getParams().put(ParameterFactory.newParameter("rotate_begin", new float[] {rotateBeginX, rotateBeginY, rotateBeginZ}));
        scriptParameters.getParams().put(ParameterFactory.newParameter("rotate_end", new float[] {rotateEndX, rotateEndY, rotateEndZ}));
        scriptParameters.getParams().put(ParameterFactory.newParameter("coarse_rate", new float[] {coarseRateX, coarseRateY, coarseRateZ}));
        scriptParameters.getParams().put(ParameterFactory.newParameter("fine_rate", new float[] {fineRateX, fineRateY, fineRateZ}));
        scriptParameters.getParams().put(ParameterFactory.newParameter("save_arithmetic", saveAriMean));
        scriptParameters.getParams().put(ParameterFactory.newParameter("show_arithmetic", showAriMean));
        scriptParameters.getParams().put(ParameterFactory.newParameter("save_geometric", saveGeoMean));
        scriptParameters.getParams().put(ParameterFactory.newParameter("show_geometric", showGeoMean));
        scriptParameters.getParams().put(ParameterFactory.newParameter("do_interImages", doInterImages));
        scriptParameters.getParams().put(ParameterFactory.newParameter("save_prefusion", savePrefusion));
        scriptParameters.getParams().put(ParameterFactory.newParameter("do_show_pre_fusion", doShowPreFusion));
        scriptParameters.getParams().put(ParameterFactory.newParameter("do_threshold", doThreshold));
        scriptParameters.getParams().put(ParameterFactory.newParameter("save_max_proj", saveMaxProj));
        scriptParameters.getParams().put(ParameterFactory.newParameter("show_max_proj", showMaxProj));
        scriptParameters.getParams().put(ParameterFactory.newParameter("x_max_box_selected", xMaxBoxSelected));
        scriptParameters.getParams().put(ParameterFactory.newParameter("y_max_box_selected", yMaxBoxSelected));
        scriptParameters.getParams().put(ParameterFactory.newParameter("z_max_box_selected", zMaxBoxSelected));
        scriptParameters.getParams().put(ParameterFactory.newParameter("do_smart_movement", doSmartMovement));
        scriptParameters.getParams().put(ParameterFactory.newParameter("threshold_intensity", thresholdIntensity));
        scriptParameters.getParams().put(ParameterFactory.newParameter("res_x", resX));
        scriptParameters.getParams().put(ParameterFactory.newParameter("res_y", resY));
        if ( ! (noRegister2D || register2DOne || register2DAll)) {
            scriptParameters.getParams().put(ParameterFactory.newParameter("res_z", resZ));
        }

        if (registerOne || registerAll || register2DOne || register2DAll) {
            scriptParameters.getParams().put(ParameterFactory.newParameter("mtxFileDirectory", mtxFileDirectory));
            if (registerOne || register2DOne) {
                scriptParameters.getParams().put(ParameterFactory.newParameter("time_num", timeNum));
            }
        } else {
            scriptParameters.getParams().put(ParameterFactory.newParameter("mtxFileLoc", mtxFileLoc));
        }
        if (noRegister2D || register2DOne || register2DAll) {
            scriptParameters.getParams().put(ParameterFactory.newParameter("register2DFileDirString", register2DFileDirString));
        }
        scriptParameters.getParams().put(ParameterFactory.newParameter("spimAFileDir", transformFileDir));
        scriptParameters.getParams().put(ParameterFactory.newParameter("spimBFileDir", baseFileDir));

        scriptParameters.getParams().put(ParameterFactory.newParameter("baseImage", baseImage));

        scriptParameters.getParams().put(ParameterFactory.newParameter("base_rotation", baseRotation));
        scriptParameters.getParams().put(ParameterFactory.newParameter("transform_rotation", transformRotation));
        scriptParameters.getParams().put(ParameterFactory.newParameter("concurrent_num", concurrentNum));
        scriptParameters.getParams().put(ParameterFactory.newParameter("mode_num", modeNum));
        scriptParameters.getParams().put(ParameterFactory.newParameter("save_type", saveType));
        scriptParameters.getParams().put(ParameterFactory.newParameter("do_deconv", doDeconv));
        if ( ! (noRegister2D || register2DOne || register2DAll)) {

            if (showMaxProj || saveMaxProj) {
                scriptParameters.getParams().put(ParameterFactory.newParameter("min_threshold", minThreshold));
                scriptParameters.getParams().put(ParameterFactory.newParameter("sliding_window", slidingWindow));
            }

            if (doDeconv) {
                scriptParameters.getParams().put(ParameterFactory.newParameter("deconv_platform", deconvPlatform));
                scriptParameters.getParams().put(ParameterFactory.newParameter("deconvDirString", deconvDirString));
                scriptParameters.getParams().put(ParameterFactory.newParameter("deconv_show_results", deconvShowResults));
                scriptParameters.getParams().put(ParameterFactory.newParameter("deconvolution_method", deconvolutionMethod));
                scriptParameters.getParams().put(ParameterFactory.newParameter("deconv_iterations", deconvIterations));
                scriptParameters.getParams().put(ParameterFactory.newParameter("deconv_sigmaA", deconvSigmaA));
                scriptParameters.getParams().put(ParameterFactory.newParameter("deconv_sigmaB", deconvSigmaB));
                scriptParameters.getParams().put(ParameterFactory.newParameter("use_deconv_sigma_conversion_factor", useDeconvSigmaConversionFactor));
            } // if (doDeconv)

            if (saveAriMean) {
                scriptParameters.getParams().put(ParameterFactory.newParameter("ariMeanDirString", ariMeanDirString));
            }

            if (saveGeoMean) {
                scriptParameters.getParams().put(ParameterFactory.newParameter("geoMeanDirString", geoMeanDirString));
            }

            if (savePrefusion) {
                scriptParameters.getParams().put(ParameterFactory.newParameter("prefusionBaseDirString", prefusionBaseDirString));
                scriptParameters.getParams().put(ParameterFactory.newParameter("prefusionTransformDirString", prefusionTransformDirString));
            }

            if (showAriMean || saveAriMean) {
                scriptParameters.getParams().put(ParameterFactory.newParameter("base_ari_weight", baseAriWeight));
                scriptParameters.getParams().put(ParameterFactory.newParameter("transform_ari_weight", transformAriWeight));
            }

            if (showGeoMean || saveGeoMean) {
                scriptParameters.getParams().put(ParameterFactory.newParameter("base_geo_weight", baseGeoWeight));
                scriptParameters.getParams().put(ParameterFactory.newParameter("transform_geo_weight", transformGeoWeight));
            }

            if ( !doSmartMovement) {
                scriptParameters.getParams().put(ParameterFactory.newParameter("x_move", xMove));
                scriptParameters.getParams().put(ParameterFactory.newParameter("y_move", yMove));
                scriptParameters.getParams().put(ParameterFactory.newParameter("z_move", zMove));
            } else {
                xMovement = yMovement = zMovement = null;

                scriptParameters.getParams().put(ParameterFactory.newParameter("min_x", minX));
                scriptParameters.getParams().put(ParameterFactory.newParameter("min_y", minY));
                scriptParameters.getParams().put(ParameterFactory.newParameter("min_z", minZ));

                scriptParameters.getParams().put(ParameterFactory.newParameter("max_x", maxX));
                scriptParameters.getParams().put(ParameterFactory.newParameter("max_y", maxY));
                scriptParameters.getParams().put(ParameterFactory.newParameter("max_z", maxZ));

                scriptParameters.getParams().put(ParameterFactory.newParameter("step_size", stepSize));
            }
        } // if (!(noRegister2D || register2DOne || register2DAll))

        scriptParameters.getParams().put(ParameterFactory.newParameter("fusion_range", rangeFusionText.getText()));
    } // end storeParamsFromGUI()

    private GridBagConstraints createGBC() {
        final GridBagConstraints gbc = new GridBagConstraints();

        gbc.gridwidth = 1;
        gbc.gridheight = 1;
        gbc.anchor = GridBagConstraints.WEST;
        gbc.weightx = 1;
        gbc.insets = new Insets(3, 3, 3, 3);
        gbc.fill = GridBagConstraints.HORIZONTAL;
        gbc.gridx = 0;
        gbc.gridy = 0;

        return gbc;
    }

    private void init() {
        setResizable(true);
        setForeground(Color.black);
        setTitle("Generate fusion 544l");
        try {
            setIconImage(MipavUtil.getIconImage("divinci.gif"));
        } catch (final FileNotFoundException e) {
            Preferences.debug("Failed to load default icon", Preferences.DEBUG_MINOR);
        }

        final GuiBuilder gui = new GuiBuilder(this);

        gbc = new GridBagConstraints();
        gbc.gridwidth = 1;
        gbc.gridheight = 1;
        gbc.anchor = GridBagConstraints.WEST;
        gbc.weightx = 1;
        gbc.insets = new Insets(3, 3, 3, 3);
        gbc.fill = GridBagConstraints.HORIZONTAL;
        gbc.gridx = 0;
        gbc.gridy = 0;

        final JPanel leftPanel = new JPanel(new GridBagLayout());
        leftPanel.setForeground(Color.black);

        final JPanel mtxPanel = new JPanel(new GridBagLayout());
        mtxPanel.setForeground(Color.black);
        mtxPanel.setBorder(buildTitledBorder("File information"));

        registrationGroup = new ButtonGroup();
        noRegisterButton = new JRadioButton("No registration", false);
        noRegisterButton.setFont(serif12);
        noRegisterButton.setForeground(Color.black);
        registrationGroup.add(noRegisterButton);
        mtxPanel.add(noRegisterButton, gbc);
        gbc.gridy++;

        registerOneButton = new JRadioButton("One time registration", true);
        registerOneButton.setFont(serif12);
        registerOneButton.setForeground(Color.black);
        registrationGroup.add(registerOneButton);
        mtxPanel.add(registerOneButton, gbc);
        gbc.gridy++;

        registerAllButton = new JRadioButton("All times registration", false);
        registerAllButton.setFont(serif12);
        registerAllButton.setForeground(Color.black);
        registrationGroup.add(registerAllButton);
        mtxPanel.add(registerAllButton, gbc);
        gbc.gridy++;

        noRegister2DButton = new JRadioButton("No 2D registration", false);
        noRegister2DButton.setFont(serif12);
        noRegister2DButton.setForeground(Color.black);
        registrationGroup.add(noRegister2DButton);
        mtxPanel.add(noRegister2DButton, gbc);
        gbc.gridy++;

        register2DOneButton = new JRadioButton("One time 2D registration", false);
        register2DOneButton.setFont(serif12);
        register2DOneButton.setForeground(Color.black);
        registrationGroup.add(register2DOneButton);
        mtxPanel.add(register2DOneButton, gbc);
        gbc.gridy++;

        register2DAllButton = new JRadioButton("All times 2D registration", false);
        register2DAllButton.setFont(serif12);
        register2DAllButton.setForeground(Color.black);
        registrationGroup.add(register2DAllButton);
        mtxPanel.add(register2DAllButton, gbc);
        gbc.gridy++;

        final GridBagConstraints gbc2 = new GridBagConstraints();
        gbc2.gridwidth = 1;
        gbc2.gridheight = 1;
        gbc2.anchor = GridBagConstraints.WEST;
        gbc2.weightx = 1;
        gbc2.fill = GridBagConstraints.HORIZONTAL;
        gbc2.gridx = 0;
        gbc2.gridy = 0;
        gbc2.insets = new Insets(3, 3, 3, 3);
        final JPanel timeNumberPanel = new JPanel(new GridBagLayout());
        timeNumberPanel.setForeground(Color.black);

        final GridBagConstraints gbc3 = new GridBagConstraints();
        gbc3.gridwidth = 1;
        gbc3.gridheight = 1;
        gbc3.anchor = GridBagConstraints.WEST;
        gbc3.weightx = 1;
        gbc3.fill = GridBagConstraints.HORIZONTAL;
        gbc3.gridx = 0;
        gbc3.gridy = 0;
        gbc3.insets = new Insets(3, 3, 3, 3);
        final JPanel oneTimePanel = new JPanel(new GridBagLayout());
        oneTimePanel.setForeground(Color.black);

        labelTimeExt = new JLabel("Image number to register :");
        labelTimeExt.setForeground(Color.black);
        labelTimeExt.setFont(serif12);
        oneTimePanel.add(labelTimeExt, gbc3);

        textTimeExt = new JTextField(20);
        textTimeExt.setForeground(Color.black);
        textTimeExt.setFont(serif12);
        gbc3.gridx = 1;
        oneTimePanel.add(textTimeExt, gbc3);
        gbc3.gridx = 0;
        gbc3.gridy++;
        timeNumberPanel.add(oneTimePanel, gbc2);
        gbc2.gridy++;

        gbc2.gridwidth = 2;
        mtxFileDirectoryText = gui.buildFileField("Directory containing matrix file: ", " ", false, JFileChooser.DIRECTORIES_ONLY);
        timeNumberPanel.add(mtxFileDirectoryText.getParent(), gbc2);
        gbc2.gridx = 0;
        gbc2.gridy++;

        final GridBagConstraints gbc5 = new GridBagConstraints();
        gbc5.gridwidth = 1;
        gbc5.gridheight = 1;
        gbc5.anchor = GridBagConstraints.WEST;
        gbc5.weightx = 1;
        gbc5.fill = GridBagConstraints.HORIZONTAL;
        gbc5.gridx = 0;
        gbc5.gridy = 0;
        final JPanel registerFilePanel = new JPanel(new GridBagLayout());
        registerFilePanel.setForeground(Color.black);

        register2DFileDirectoryText = gui.buildFileField("Directory containing registered 2D files: ", " ", false, JFileChooser.DIRECTORIES_ONLY);
        registerFilePanel.add(register2DFileDirectoryText.getParent(), gbc5);
        registerFilePanel.setVisible(false);

        timeNumberPanel.add(registerFilePanel, gbc2);
        gbc2.gridx = 0;
        gbc2.gridy++;

        universalCheckbox = new JCheckBox("Apply same rotation limits to all dimensions.");
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

        rotateBeginTextX = new JTextField("-10", 3);
        rotateEndTextX = new JTextField("10", 3);

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
        coarseRateTextX = new JTextField("3", 3);

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
        fineRateTextX = new JTextField("1", 3);

        finePanelX.add(labelFineX);
        finePanelX.add(fineRateTextX);
        finePanelX.add(labelFineDegreesX);
        finePanelX.setAlignmentX(Component.LEFT_ALIGNMENT);

        rotatePanel = new JPanel();
        rotatePanel.setLayout(new GridBagLayout());
        rotatePanel.setBorder(buildTitledBorder("Rotations"));
        final GridBagConstraints gbc6 = new GridBagConstraints();
        gbc6.gridheight = 1;
        gbc6.anchor = GridBagConstraints.WEST;
        gbc6.insets = new Insets(3, 3, 3, 3);

        gbc6.fill = GridBagConstraints.HORIZONTAL;
        gbc6.weightx = 1;
        gbc6.gridx = 0;
        gbc6.gridy = 0;
        gbc6.gridwidth = 1;
        rotatePanel.add(universalCheckbox, gbc6);

        gbc6.gridx = 0;
        gbc6.gridy = 1;
        gbc6.gridwidth = GridBagConstraints.REMAINDER;
        gbc6.anchor = GridBagConstraints.WEST;
        rotatePanel.add(xyzPanel, gbc6);

        gbc6.gridx = 0;
        gbc6.gridy = 2;
        gbc6.gridwidth = 1;
        gbc6.anchor = GridBagConstraints.WEST;
        rotatePanel.add(rotateRangePanelX, gbc6);

        gbc6.gridx = 0;
        gbc6.gridy = 3;
        gbc6.gridwidth = GridBagConstraints.REMAINDER;
        rotatePanel.add(coarsePanelX, gbc6);

        gbc6.gridx = 0;
        gbc6.gridy = 4;
        gbc6.gridwidth = GridBagConstraints.REMAINDER;
        rotatePanel.add(finePanelX, gbc6);

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

        rotateBeginTextY = new JTextField("-10", 3);
        rotateEndTextY = new JTextField("10", 3);

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

        coarseRateTextY = new JTextField("3", 3);

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

        fineRateTextY = new JTextField("1", 3);

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

        rotateBeginTextZ = new JTextField("-10", 3);
        rotateEndTextZ = new JTextField("10", 3);

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

        coarseRateTextZ = new JTextField("3", 3);

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

        fineRateTextZ = new JTextField("1", 3);

        finePanelZ.add(labelFineZ);
        finePanelZ.add(fineRateTextZ);
        finePanelZ.add(labelFineDegreesZ);
        finePanelZ.setAlignmentX(Component.LEFT_ALIGNMENT);

        timeNumberPanel.add(rotatePanel, gbc2);

        mtxPanel.add(timeNumberPanel, gbc);
        gbc.gridy++;

        final GridBagConstraints gbc4 = new GridBagConstraints();
        gbc4.gridwidth = 1;
        gbc4.gridheight = 1;
        gbc4.anchor = GridBagConstraints.WEST;
        gbc4.weightx = 1;
        gbc4.fill = GridBagConstraints.HORIZONTAL;
        gbc4.gridx = 0;
        gbc4.gridy = 0;
        final JPanel matrixFilePanel = new JPanel(new GridBagLayout());
        matrixFilePanel.setForeground(Color.black);

        mtxFileLocText = gui.buildFileField("Matrix file: ", "", false, JFileChooser.FILES_ONLY);
        matrixFilePanel.add(mtxFileLocText.getParent(), gbc3);
        matrixFilePanel.setVisible(false);

        mtxPanel.add(matrixFilePanel, gbc);
        gbc.gridy++;

        noRegisterButton.addActionListener(new ActionListener() {
            @Override
            public void actionPerformed(final ActionEvent arg0) {
                timeNumberPanel.setVisible(registerOneButton.isSelected() || registerAllButton.isSelected() || noRegister2DButton.isSelected()
                        || register2DOneButton.isSelected() || register2DAllButton.isSelected());
                oneTimePanel.setVisible(registerOneButton.isSelected() || register2DOneButton.isSelected());
                matrixFilePanel.setVisible( ! (registerOneButton.isSelected() || registerAllButton.isSelected() || register2DOneButton.isSelected() || register2DAllButton
                        .isSelected()));
                registerFilePanel.setVisible(noRegister2DButton.isSelected() || register2DOneButton.isSelected() || register2DAllButton.isSelected());
                rotatePanel.setVisible( ! (noRegisterButton.isSelected() || noRegister2DButton.isSelected()));
                mtxFileDirectoryText.getParent().setVisible( ! (noRegisterButton.isSelected() || noRegister2DButton.isSelected()));
            }
        });

        registerOneButton.addActionListener(new ActionListener() {
            @Override
            public void actionPerformed(final ActionEvent arg0) {
                timeNumberPanel.setVisible(registerOneButton.isSelected() || registerAllButton.isSelected() || noRegister2DButton.isSelected()
                        || register2DOneButton.isSelected() || register2DAllButton.isSelected());
                oneTimePanel.setVisible(registerOneButton.isSelected() || register2DOneButton.isSelected());
                matrixFilePanel.setVisible( ! (registerOneButton.isSelected() || registerAllButton.isSelected() || register2DOneButton.isSelected() || register2DAllButton
                        .isSelected()));
                registerFilePanel.setVisible(noRegister2DButton.isSelected() || register2DOneButton.isSelected() || register2DAllButton.isSelected());
                rotatePanel.setVisible( ! (noRegisterButton.isSelected() || noRegister2DButton.isSelected()));
                mtxFileDirectoryText.getParent().setVisible( ! (noRegisterButton.isSelected() || noRegister2DButton.isSelected()));
            }
        });

        registerAllButton.addActionListener(new ActionListener() {
            @Override
            public void actionPerformed(final ActionEvent arg0) {
                timeNumberPanel.setVisible(registerOneButton.isSelected() || registerAllButton.isSelected() || noRegister2DButton.isSelected()
                        || register2DOneButton.isSelected() || register2DAllButton.isSelected());
                oneTimePanel.setVisible(registerOneButton.isSelected() || register2DOneButton.isSelected());
                matrixFilePanel.setVisible( ! (registerOneButton.isSelected() || registerAllButton.isSelected() || register2DOneButton.isSelected() || register2DAllButton
                        .isSelected()));
                registerFilePanel.setVisible(noRegister2DButton.isSelected() || register2DOneButton.isSelected() || register2DAllButton.isSelected());
                rotatePanel.setVisible( ! (noRegisterButton.isSelected() || noRegister2DButton.isSelected()));
                mtxFileDirectoryText.getParent().setVisible( ! (noRegisterButton.isSelected() || noRegister2DButton.isSelected()));
            }
        });

        noRegister2DButton.addActionListener(new ActionListener() {
            @Override
            public void actionPerformed(final ActionEvent arg0) {
                timeNumberPanel.setVisible(registerOneButton.isSelected() || registerAllButton.isSelected() || noRegister2DButton.isSelected()
                        || register2DOneButton.isSelected() || register2DAllButton.isSelected());
                oneTimePanel.setVisible(registerOneButton.isSelected() || register2DOneButton.isSelected());
                matrixFilePanel.setVisible( ! (registerOneButton.isSelected() || registerAllButton.isSelected() || register2DOneButton.isSelected() || register2DAllButton
                        .isSelected()));
                registerFilePanel.setVisible(noRegister2DButton.isSelected() || register2DOneButton.isSelected() || register2DAllButton.isSelected());
                rotatePanel.setVisible( ! (noRegisterButton.isSelected() || noRegister2DButton.isSelected()));
                mtxFileDirectoryText.getParent().setVisible( ! (noRegisterButton.isSelected() || noRegister2DButton.isSelected()));
            }
        });

        register2DOneButton.addActionListener(new ActionListener() {
            @Override
            public void actionPerformed(final ActionEvent arg0) {
                timeNumberPanel.setVisible(registerOneButton.isSelected() || registerAllButton.isSelected() || noRegister2DButton.isSelected()
                        || register2DOneButton.isSelected() || register2DAllButton.isSelected());
                oneTimePanel.setVisible(registerOneButton.isSelected() || register2DOneButton.isSelected());
                matrixFilePanel.setVisible( ! (registerOneButton.isSelected() || registerAllButton.isSelected() || register2DOneButton.isSelected() || register2DAllButton
                        .isSelected()));
                registerFilePanel.setVisible(noRegister2DButton.isSelected() || register2DOneButton.isSelected() || register2DAllButton.isSelected());
                rotatePanel.setVisible( ! (noRegisterButton.isSelected() || noRegister2DButton.isSelected()));
                mtxFileDirectoryText.getParent().setVisible( ! (noRegisterButton.isSelected() || noRegister2DButton.isSelected()));
            }
        });

        register2DAllButton.addActionListener(new ActionListener() {
            @Override
            public void actionPerformed(final ActionEvent arg0) {
                timeNumberPanel.setVisible(registerOneButton.isSelected() || registerAllButton.isSelected() || noRegister2DButton.isSelected()
                        || register2DOneButton.isSelected() || register2DAllButton.isSelected());
                oneTimePanel.setVisible(registerOneButton.isSelected() || register2DOneButton.isSelected());
                matrixFilePanel.setVisible( ! (registerOneButton.isSelected() || registerAllButton.isSelected() || register2DOneButton.isSelected() || register2DAllButton
                        .isSelected()));
                registerFilePanel.setVisible(noRegister2DButton.isSelected() || register2DOneButton.isSelected() || register2DAllButton.isSelected());
                rotatePanel.setVisible( ! (noRegisterButton.isSelected() || noRegister2DButton.isSelected()));
                mtxFileDirectoryText.getParent().setVisible( ! (noRegisterButton.isSelected() || noRegister2DButton.isSelected()));
            }
        });

        final FolderSaveActionListener folderSave = new FolderSaveActionListener(this);

        transformFileLocText = gui.buildFileField("Directory containing transform image: ", "", false, JFileChooser.DIRECTORIES_ONLY, folderSave);
        mtxPanel.add(transformFileLocText.getParent(), gbc);
        gbc.gridy++;

        baseFileLocText = gui.buildFileField("Directory containing base image: ", "", false, JFileChooser.DIRECTORIES_ONLY);
        mtxPanel.add(baseFileLocText.getParent(), gbc);
        gbc.gridy++;

        final JPanel transformPanel = new JPanel();
        final FlowLayout transformFlow = new FlowLayout(FlowLayout.LEFT);
        transformPanel.setLayout(transformFlow);

        transformImageText = gui.buildField("Transform image ", "SPIMB");
        transformPanel.add(transformImageText.getParent());

        baseImageText = gui.buildField(" to image ", "SPIMA");
        transformPanel.add(baseImageText.getParent());

        mtxPanel.add(transformPanel, gbc);
        gbc.gridy++;

        rangeFusionText = gui.buildField("Range of images to fuse (ex. 3-7, 12, 18-21, etc.): ", " ");
        mtxPanel.add(rangeFusionText.getParent(), gbc);
        gbc.gridy++;

        saveTypeText = gui.buildComboBox("Save result images as type: ", new String[] {"Tiff", "Raw"}, 0);
        mtxPanel.add(saveTypeText.getParent(), gbc);
        gbc.gridy++;

        final JLabel dirMove = new JLabel("Enter translation to apply to transformed image (optional):");
        mtxPanel.add(dirMove, gbc);
        gbc.gridy++;

        final JPanel movementPanel = new JPanel();
        final FlowLayout movementFlow = new FlowLayout(FlowLayout.LEFT);
        movementPanel.setLayout(movementFlow);

        xMovementText = gui.buildIntegerField("X: ", 0);
        movementPanel.add(xMovementText.getParent());

        yMovementText = gui.buildIntegerField("Y: ", 0);
        movementPanel.add(yMovementText.getParent());

        zMovementText = gui.buildIntegerField("Z: ", 0);
        movementPanel.add(zMovementText.getParent());

        mtxPanel.add(movementPanel, gbc);
        gbc.gridy++;

        doSmartMovementBox = gui.buildCheckBox("Attempt to generate optimized translation", false);
        mtxPanel.add(doSmartMovementBox.getParent(), gbc);
        gbc.gridy++;

        final JPanel smartPanel = new JPanel();
        smartPanel.setLayout(new GridBagLayout());
        final GridBagConstraints smartGBC = new GridBagConstraints();
        smartGBC.gridx = 0;
        smartGBC.gridy = 0;

        minXText = gui.buildIntegerField("X range: ", -60);
        smartPanel.add(minXText.getParent(), smartGBC);
        smartGBC.gridx++;

        maxXText = gui.buildIntegerField(" to ", 60);
        smartPanel.add(maxXText.getParent(), smartGBC);
        smartGBC.gridy++;
        smartGBC.gridx = 0;

        minYText = gui.buildIntegerField("Y range: ", -60);
        smartPanel.add(minYText.getParent(), smartGBC);
        smartGBC.gridx++;

        maxYText = gui.buildIntegerField(" to ", 60);
        smartPanel.add(maxYText.getParent(), smartGBC);
        smartGBC.gridy++;
        smartGBC.gridx = 0;

        minZText = gui.buildIntegerField("Z range: ", -30);
        smartPanel.add(minZText.getParent(), smartGBC);
        smartGBC.gridx++;

        maxZText = gui.buildIntegerField(" to ", 30);
        smartPanel.add(maxZText.getParent(), smartGBC);
        smartGBC.gridy++;
        smartGBC.gridx = 0;
        smartGBC.gridwidth = 2;

        stepSizeText = gui.buildIntegerField("Initial step size: ", 5);
        smartPanel.add(stepSizeText.getParent(), smartGBC);

        mtxPanel.add(smartPanel, gbc);
        gbc.gridy++;

        smartPanel.setVisible(false);

        doSmartMovementBox.addActionListener(new ActionListener() {
            @Override
            public void actionPerformed(final ActionEvent arg0) {
                xMovementText.setEnabled( !doSmartMovementBox.isSelected());
                yMovementText.setEnabled( !doSmartMovementBox.isSelected());
                zMovementText.setEnabled( !doSmartMovementBox.isSelected());

                smartPanel.setVisible(doSmartMovementBox.isSelected());
            }
        });

        gbc.gridx = 0;
        gbc.gridy = 0;

        leftPanel.add(mtxPanel, gbc);

        final JPanel middlePanel = new JPanel(new GridBagLayout());
        middlePanel.setForeground(Color.black);

        final JPanel rightPanel = new JPanel(new GridBagLayout());
        rightPanel.setForeground(Color.black);

        final JPanel algOptionPanel = new JPanel(new GridBagLayout());
        algOptionPanel.setForeground(Color.black);
        algOptionPanel.setBorder(MipavUtil.buildTitledBorder("Algorithm options"));

        final JLabel resLabel = new JLabel("Initial resolutions (um): ");
        algOptionPanel.add(resLabel, gbc);
        gbc.gridy++;

        final JPanel resPanel = new JPanel(new GridBagLayout());
        resPanel.setForeground(Color.black);
        final FlowLayout flow = new FlowLayout(FlowLayout.LEFT);
        resPanel.setLayout(flow);

        resXText = gui.buildDecimalField("X: ", .1625);
        resPanel.add(resXText.getParent());

        resYText = gui.buildDecimalField("Y: ", .1625);
        resPanel.add(resYText.getParent());

        resZText = gui.buildDecimalField("Z: ", 1.0);
        resPanel.add(resZText.getParent());

        algOptionPanel.add(resPanel, gbc);
        gbc.gridy++;
        gbc.gridx = 0;

        modeOption = gui.buildComboBox("Sampling mode", SampleMode.values());
        algOptionPanel.add(modeOption.getParent(), gbc);
        gbc.gridy++;

        concurrentNumText = gui.buildIntegerField("Number of concurrent fusions: ", (Runtime.getRuntime().availableProcessors() - 2) > 1 ? Runtime.getRuntime()
                .availableProcessors() - 2 : 1);
        algOptionPanel.add(concurrentNumText.getParent(), gbc);
        gbc.gridy++;

        doThresholdBox = gui.buildCheckBox("Threshold noise", false);
        algOptionPanel.add(doThresholdBox.getParent(), gbc);
        gbc.gridy++;

        final JPanel thresholdPanel = new JPanel(new GridBagLayout());
        final GridBagConstraints gbcThreshold = new GridBagConstraints();
        thresholdPanel.setForeground(Color.black);

        gbcThreshold.gridx = 0;
        gbcThreshold.gridy = 0;

        thresholdIntensityText = gui.buildDecimalField("Threshold value", 10);
        thresholdPanel.add(thresholdIntensityText.getParent(), gbcThreshold);

        algOptionPanel.add(thresholdPanel, gbc);
        gbc.gridy++;

        doThresholdBox.addActionListener(new ActionListener() {
            @Override
            public void actionPerformed(final ActionEvent e) {
                thresholdPanel.setVisible(doThresholdBox.isSelected());
            }
        });

        thresholdPanel.setVisible(doThresholdBox.isSelected());

        gbc.gridy = 0;
        gbc.gridx = 0;
        middlePanel.add(algOptionPanel, gbc);

        final JPanel outputPanel = new JPanel(new GridBagLayout());
        outputPanel.setForeground(Color.black);
        outputPanel.setBorder(MipavUtil.buildTitledBorder("Output options"));

        final JPanel prefusionPanel = buildPrefusionPanel(gui, folderSave);

        final JPanel arithmeticPanel = buildArithmeticPanel(gui, folderSave);

        final JPanel geometricPanel = buildGeometricPanel(gui, folderSave);

        final JPanel maxProjPanel = buildMaxProjPanel(gui, folderSave);

        final JPanel deconvPanel = buildDeconvolutionPanel(gui, folderSave);

        gbc.gridy = 0;
        // gbc.gridwidth = 1;
        outputPanel.add(prefusionPanel, gbc);
        gbc.gridy++;
        outputPanel.add(arithmeticPanel, gbc);
        gbc.gridy++;
        outputPanel.add(geometricPanel, gbc);
        gbc.gridy++;
        outputPanel.add(maxProjPanel, gbc);
        gbc.gridy++;

        interImagesBox = gui.buildCheckBox("Show intermediate images", false);
        outputPanel.add(interImagesBox.getParent(), gbc);

        mtxFileLocText.addActionListener(new ActionListener() {
            @Override
            public void actionPerformed(final ActionEvent e) {
                try {
                    File f;
                    if ( (registerOneButton.isSelected() || registerAllButton.isSelected() || register2DOneButton.isSelected() || register2DAllButton
                            .isSelected())) {
                        f = new File(mtxFileLocText.getText()).getParentFile().getParentFile();
                    } else {
                        f = new File(mtxFileLocText.getText()).getParentFile().getParentFile().getParentFile();
                    }
                    Preferences.setImageDirectory(f);
                } catch (final Exception ex) {}
            }
        });

        register2DFileDirectoryText.addActionListener(new ActionListener() {
            @Override
            public void actionPerformed(final ActionEvent e) {
                try {
                    File f;
                    register2DFileDirString = register2DFileDirectoryText.getText();
                    f = new File(register2DFileDirString).getParentFile().getParentFile();
                    Preferences.setImageDirectory(f);
                } catch (final Exception ex) {}
            }
        });

        gbc.gridy = 1;
        middlePanel.add(outputPanel, gbc);

        gbc.gridy++;
        okCancelPanel = gui.buildOKCancelPanel();
        middlePanel.add(okCancelPanel, gbc);

        gbc.gridx = 0;
        gbc.gridy = 0;
        rightPanel.add(deconvPanel, gbc);
        gbc.gridy++;

        gbc = new GridBagConstraints();
        gbc.gridwidth = 1;
        gbc.gridheight = 1;
        gbc.anchor = GridBagConstraints.WEST;
        gbc.weightx = 1;
        gbc.insets = new Insets(3, 3, 3, 3);
        gbc.fill = GridBagConstraints.HORIZONTAL;
        gbc.gridx = 0;
        gbc.gridy = 0;

        final JPanel right2Panel = new JPanel(new GridBagLayout());
        right2Panel.setForeground(Color.black);

        final JPanel basePanel = new JPanel(new GridBagLayout());
        basePanel.setForeground(Color.black);
        basePanel.setBorder(buildTitledBorder("Base image rotations"));

        baseImageRotation = new ButtonGroup();
        noBaseRotationButton = new JRadioButton("No base image rotation", true);
        noBaseRotationButton.setFont(serif12);
        noBaseRotationButton.setForeground(Color.black);
        baseImageRotation.add(noBaseRotationButton);
        basePanel.add(noBaseRotationButton, gbc);
        gbc.gridy++;

        minusXBaseRotationButton = new JRadioButton("-90 degree X axis base image rotation", false);
        minusXBaseRotationButton.setFont(serif12);
        minusXBaseRotationButton.setForeground(Color.black);
        baseImageRotation.add(minusXBaseRotationButton);
        basePanel.add(minusXBaseRotationButton, gbc);
        gbc.gridy++;

        plusXBaseRotationButton = new JRadioButton("+90 degree X axis base image rotation", false);
        plusXBaseRotationButton.setFont(serif12);
        plusXBaseRotationButton.setForeground(Color.black);
        baseImageRotation.add(plusXBaseRotationButton);
        basePanel.add(plusXBaseRotationButton, gbc);
        gbc.gridy++;

        minusYBaseRotationButton = new JRadioButton("-90 degree Y axis base image rotation", false);
        minusYBaseRotationButton.setFont(serif12);
        minusYBaseRotationButton.setForeground(Color.black);
        baseImageRotation.add(minusYBaseRotationButton);
        basePanel.add(minusYBaseRotationButton, gbc);
        gbc.gridy++;

        plusYBaseRotationButton = new JRadioButton("+90 degree Y axis base image rotation", false);
        plusYBaseRotationButton.setFont(serif12);
        plusYBaseRotationButton.setForeground(Color.black);
        baseImageRotation.add(plusYBaseRotationButton);
        basePanel.add(plusYBaseRotationButton, gbc);
        gbc.gridy++;

        invertYBaseRotationButton = new JRadioButton("180 degree Y axis base image rotation", false);
        invertYBaseRotationButton.setFont(serif12);
        invertYBaseRotationButton.setForeground(Color.black);
        baseImageRotation.add(invertYBaseRotationButton);
        basePanel.add(invertYBaseRotationButton, gbc);
        gbc.gridy++;

        final JPanel transformImagePanel = new JPanel(new GridBagLayout());
        transformImagePanel.setForeground(Color.black);
        transformImagePanel.setBorder(buildTitledBorder("Transform image rotations"));

        gbc.gridx = 0;
        gbc.gridy = 0;

        transformImageRotation = new ButtonGroup();
        noTransformRotationButton = new JRadioButton("No transform image rotation", false);
        noTransformRotationButton.setFont(serif12);
        noTransformRotationButton.setForeground(Color.black);
        transformImageRotation.add(noTransformRotationButton);
        transformImagePanel.add(noTransformRotationButton, gbc);
        gbc.gridy++;

        minusXTransformRotationButton = new JRadioButton("-90 degree X axis transform image rotation", false);
        minusXTransformRotationButton.setFont(serif12);
        minusXTransformRotationButton.setForeground(Color.black);
        transformImageRotation.add(minusXTransformRotationButton);
        transformImagePanel.add(minusXTransformRotationButton, gbc);
        gbc.gridy++;

        plusXTransformRotationButton = new JRadioButton("+90 degree X axis transform image rotation", false);
        plusXTransformRotationButton.setFont(serif12);
        plusXTransformRotationButton.setForeground(Color.black);
        transformImageRotation.add(plusXTransformRotationButton);
        transformImagePanel.add(plusXTransformRotationButton, gbc);
        gbc.gridy++;

        minusYTransformRotationButton = new JRadioButton("-90 degree Y axis transform image rotation", true);
        minusYTransformRotationButton.setFont(serif12);
        minusYTransformRotationButton.setForeground(Color.black);
        transformImageRotation.add(minusYTransformRotationButton);
        transformImagePanel.add(minusYTransformRotationButton, gbc);
        gbc.gridy++;

        plusYTransformRotationButton = new JRadioButton("+90 degree Y axis transform image rotation", false);
        plusYTransformRotationButton.setFont(serif12);
        plusYTransformRotationButton.setForeground(Color.black);
        transformImageRotation.add(plusYTransformRotationButton);
        transformImagePanel.add(plusYTransformRotationButton, gbc);
        gbc.gridy++;

        invertYTransformRotationButton = new JRadioButton("180 degree Y axis transform image rotation", false);
        invertYTransformRotationButton.setFont(serif12);
        invertYTransformRotationButton.setForeground(Color.black);
        transformImageRotation.add(invertYTransformRotationButton);
        transformImagePanel.add(invertYTransformRotationButton, gbc);
        gbc.gridy++;

        gbc.gridx = 0;
        gbc.gridy = 0;
        right2Panel.add(basePanel, gbc);
        gbc.gridy++;
        right2Panel.add(transformImagePanel, gbc);

        tabbedPane = new JTabbedPane();
        tabbedPane.setFont(MipavUtil.font12B);
        tabbedPane.addTab("File", leftPanel);

        tabbedPane.addTab("Options", middlePanel);

        tabbedPane.addTab("Decon", rightPanel);

        tabbedPane.addTab("Image rotations", right2Panel);

        getContentPane().add(tabbedPane, BorderLayout.CENTER);

        final Dimension dim = Toolkit.getDefaultToolkit().getScreenSize();
        getContentPane().setMaximumSize(new Dimension(685, (int) dim.getHeight() - 120));
        getContentPane().setPreferredSize(new Dimension(685, (int) dim.getHeight() - 120));

        pack();
        setVisible(true);
        setResizable(true);

        System.gc();

    } // end init()

    @Override
    public void itemStateChanged(final ItemEvent event) {

        if (event.getSource() == universalCheckbox) {

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
        }
    }

    private JPanel buildMaxProjPanel(final GuiBuilder gui, final ActionListener folderSave) {
        final GridBagConstraints gbc = createGBC();
        final JPanel maxProjPanel = new JPanel(new GridBagLayout());
        maxProjPanel.setForeground(Color.black);
        maxProjPanel.setBorder(MipavUtil.buildTitledBorder("Maximum projection options"));
        doShowMaxProjBox = gui.buildCheckBox("Show max projection images", false);
        maxProjPanel.add(doShowMaxProjBox.getParent(), gbc);
        gbc.gridx++;

        doSaveMaxProjBox = gui.buildCheckBox("Save max projection images", true);
        doSaveMaxProjBox.addActionListener(folderSave);
        maxProjPanel.add(doSaveMaxProjBox.getParent(), gbc);

        gbc.gridx++;
        doSlideWindowBox = gui.buildCheckBox("Do sliding window", false);
        maxProjPanel.add(doSlideWindowBox.getParent(), gbc);

        gbc.gridx = 0;
        gbc.gridwidth = 1;
        gbc.gridy++;

        minThresholdMaxProjText = gui.buildDecimalField("Min threshold", 0.0);
        maxProjPanel.add(minThresholdMaxProjText.getParent(), gbc);

        gbc.gridx += 2;
        gbc.gridwidth = 1;
        slidingWindowText = gui.buildIntegerField("Sliding window", 1);
        slidingWindowText.setEnabled(false);
        maxProjPanel.add(slidingWindowText.getParent(), gbc);

        doSlideWindowBox.addActionListener(new ActionListener() {
            @Override
            public void actionPerformed(final ActionEvent e) {
                slidingWindowText.setEnabled(doSlideWindowBox.isSelected());
            }
        });

        gbc.gridx = 0;
        gbc.gridy++;
        gbc.gridwidth = 1;
        doXMaxBox = gui.buildCheckBox("Do max X", true);
        maxProjPanel.add(doXMaxBox.getParent(), gbc);

        gbc.gridx++;
        doYMaxBox = gui.buildCheckBox("Do max Y", true);
        maxProjPanel.add(doYMaxBox.getParent(), gbc);

        gbc.gridx++;
        doZMaxBox = gui.buildCheckBox("Do max Z", true);
        maxProjPanel.add(doZMaxBox.getParent(), gbc);

        gbc.gridwidth = 4;
        gbc.gridy++;
        gbc.gridx = 0;

        // saveMaxProjFolderText = gui.buildFileField("Maximum projection image location:", initMaxProjLoc, false,
        // JFileChooser.DIRECTORIES_ONLY);
        // maxProjPanel.add(saveMaxProjFolderText.getParent(), gbc);
        // gbc.gridy++;
        // saveMaxProjFolderText.getParent().setVisible(false);

        return maxProjPanel;
    }

    private JPanel buildPrefusionPanel(final GuiBuilder gui, final ActionListener folderSave) {
        final GridBagConstraints gbc = createGBC();
        final JPanel prefusionPanel = new JPanel(new GridBagLayout());
        prefusionPanel.setForeground(Color.black);
        prefusionPanel.setBorder(MipavUtil.buildTitledBorder("Prefusion options"));
        doShowPrefusionBox = gui.buildCheckBox("Show pre-fusion images", false);
        prefusionPanel.add(doShowPrefusionBox.getParent(), gbc);
        gbc.gridx++;

        doSavePrefusionBox = gui.buildCheckBox("Save pre-fusion images", false);
        doSavePrefusionBox.addActionListener(folderSave);
        prefusionPanel.add(doSavePrefusionBox.getParent(), gbc);
        gbc.gridwidth = 2;
        gbc.gridy++;
        gbc.gridx = 0;

        savePrefusionBaseFolderText = gui.buildFileField("Base image location:", initBasePrefusionLoc, false, JFileChooser.DIRECTORIES_ONLY);
        prefusionPanel.add(savePrefusionBaseFolderText.getParent(), gbc);
        gbc.gridy++;
        savePrefusionBaseFolderText.getParent().setVisible(false);

        savePrefusionTransformFolderText = gui.buildFileField("Transformed image location:", initTransformPrefusionLoc, false, JFileChooser.DIRECTORIES_ONLY);
        prefusionPanel.add(savePrefusionTransformFolderText.getParent(), gbc);
        savePrefusionTransformFolderText.getParent().setVisible(false);

        return prefusionPanel;
    }

    private JPanel buildArithmeticPanel(final GuiBuilder gui, final ActionListener folderSave) {
        final GridBagConstraints gbc = createGBC();
        final JPanel arithmeticPanel = new JPanel(new GridBagLayout());
        arithmeticPanel.setForeground(Color.black);
        arithmeticPanel.setBorder(MipavUtil.buildTitledBorder("Arithmetic options"));
        arithmeticMeanShowBox = gui.buildCheckBox("Show arithmetic mean", false);
        arithmeticPanel.add(arithmeticMeanShowBox.getParent(), gbc);
        gbc.gridx++;

        arithmeticMeanSaveBox = gui.buildCheckBox("Save arithmetic mean", false);
        arithmeticPanel.add(arithmeticMeanSaveBox.getParent(), gbc);
        gbc.gridy++;
        gbc.gridwidth = 2;
        gbc.gridx = 0;
        final JPanel ariWeightPanel = new JPanel(new GridBagLayout());
        ariWeightPanel.setForeground(Color.black);
        final FlowLayout flow = new FlowLayout(FlowLayout.LEFT);
        ariWeightPanel.setLayout(flow);

        transformAriWeightText = gui.buildDecimalField("Transformed image arithmetic weight: ", 1.0);
        ariWeightPanel.add(transformAriWeightText.getParent(), gbc);

        baseAriWeightText = gui.buildDecimalField("Base image arithmetic weight: ", 1.0);
        ariWeightPanel.add(baseAriWeightText.getParent(), gbc);

        arithmeticMeanShowBox.addActionListener(new ActionListener() {
            @Override
            public void actionPerformed(final ActionEvent arg0) {
                ariWeightPanel.setVisible(arithmeticMeanSaveBox.isSelected() || arithmeticMeanShowBox.isSelected());
            }
        });

        arithmeticPanel.add(ariWeightPanel, gbc);
        gbc.gridy++;
        gbc.gridx = 0;
        gbc.gridwidth = 2;

        arithmeticMeanFolderText = gui.buildFileField("Arithmetic mean folder location: ", initAriLoc, false, JFileChooser.DIRECTORIES_ONLY);
        arithmeticPanel.add(arithmeticMeanFolderText.getParent(), gbc);
        arithmeticMeanFolderText.getParent().setVisible(false);

        arithmeticMeanSaveBox.addActionListener(folderSave);

        return arithmeticPanel;
    }

    private JPanel buildGeometricPanel(final GuiBuilder gui, final ActionListener folderSave) {
        final GridBagConstraints gbc = createGBC();
        final JPanel geometricPanel = new JPanel(new GridBagLayout());
        geometricPanel.setForeground(Color.black);
        geometricPanel.setBorder(MipavUtil.buildTitledBorder("Geometric options"));
        geometricMeanShowBox = gui.buildCheckBox("Show geometric mean", false);
        geometricPanel.add(geometricMeanShowBox.getParent(), gbc);
        gbc.gridx++;

        geometricMeanSaveBox = gui.buildCheckBox("Save geometric mean", false);
        geometricPanel.add(geometricMeanSaveBox.getParent(), gbc);
        gbc.gridy++;
        gbc.gridwidth = 2;
        gbc.gridx = 0;
        final JPanel geoWeightPanel = new JPanel(new GridBagLayout());
        geoWeightPanel.setForeground(Color.black);
        final FlowLayout flow = new FlowLayout(FlowLayout.LEFT);
        geoWeightPanel.setLayout(flow);

        transformGeoWeightText = gui.buildDecimalField("Transformed image geometric weight: ", 1.0);
        geoWeightPanel.add(transformGeoWeightText.getParent(), gbc);

        baseGeoWeightText = gui.buildDecimalField("Base image geometric weight: ", 1.0);
        geoWeightPanel.add(baseGeoWeightText.getParent(), gbc);

        geometricMeanShowBox.addActionListener(new ActionListener() {
            @Override
            public void actionPerformed(final ActionEvent e) {
                geoWeightPanel.setVisible(geometricMeanSaveBox.isSelected() || geometricMeanShowBox.isSelected());
            }
        });
        geoWeightPanel.setVisible(false);

        geometricPanel.add(geoWeightPanel, gbc);
        gbc.gridy++;
        gbc.gridx = 0;
        gbc.gridwidth = 2;
        gbc.fill = GridBagConstraints.BOTH;

        geometricMeanFolderText = gui.buildFileField("Geometric mean folder location: ", initGeoLoc, false, JFileChooser.DIRECTORIES_ONLY);
        geometricPanel.add(geometricMeanFolderText.getParent(), gbc);
        geometricMeanFolderText.getParent().setVisible(false);

        geometricMeanSaveBox.addActionListener(folderSave);

        return geometricPanel;
    }

    private JPanel buildDeconvolutionPanel(final GuiBuilder gui, final ActionListener folderSave) {
        final GridBagConstraints gbc2 = createGBC();
        final JPanel deconvPanel = new JPanel(new GridBagLayout());
        deconvPanel.setForeground(Color.black);
        deconvPanel.setBorder(MipavUtil.buildTitledBorder("Deconvolution options"));

        deconvPerformCheckbox = gui.buildCheckBox("Perform deconvolution", true);

        deconvPerformCheckbox.addActionListener(new ActionListener() {
            @Override
            public void actionPerformed(final ActionEvent e) {
                deconvParamPanel.setVisible(deconvPerformCheckbox.isSelected());
            }
        });
        deconvPanel.add(deconvPerformCheckbox.getParent(), gbc2);
        gbc2.gridy++;

        final GridBagConstraints gbc = createGBC();
        deconvParamPanel = new JPanel(new GridBagLayout());
        deconvParamPanel.setForeground(Color.black);
        deconvParamPanel.setVisible(true);
        // deconvParamPanel.setBorder(MipavUtil.buildTitledBorder("Deconvolution options"));

        platformGroup = new ButtonGroup();
        JavaButton = new JRadioButton("Java deconvolution on CPU", true);
        if (OpenCLAlgorithmBase.isOCLAvailable()) {
            JavaButton.setEnabled(true);
        } else {
            JavaButton.setEnabled(false);
        }
        JavaButton.setFont(serif12);
        JavaButton.setForeground(Color.black);
        platformGroup.add(JavaButton);
        deconvParamPanel.add(JavaButton, gbc);
        gbc.gridy++;

        OpenCLButton = new JRadioButton("OpenCL deconvolution on graphics board", false);
        if (OpenCLAlgorithmBase.isOCLAvailable()) {
            OpenCLButton.setEnabled(true);
        } else {
            OpenCLButton.setEnabled(false);
        }
        OpenCLButton.setFont(serif12);
        OpenCLButton.setForeground(Color.black);
        platformGroup.add(OpenCLButton);
        deconvParamPanel.add(OpenCLButton, gbc);
        gbc.gridy++;

        deconvolutionGroup = new ButtonGroup();
        jointButton = new JRadioButton("Joint deconvolution", true);
        jointButton.setFont(serif12);
        jointButton.setForeground(Color.black);
        deconvolutionGroup.add(jointButton);
        deconvParamPanel.add(jointButton, gbc);
        gbc.gridy++;

        arithmeticMeanButton = new JRadioButton("Arithmetic mean deconvolution", false);
        arithmeticMeanButton.setFont(serif12);
        arithmeticMeanButton.setForeground(Color.black);
        deconvolutionGroup.add(arithmeticMeanButton);
        deconvParamPanel.add(arithmeticMeanButton, gbc);
        gbc.gridy++;

        geometricMeanButton = new JRadioButton("Geometric mean deconvolution", false);
        geometricMeanButton.setFont(serif12);
        geometricMeanButton.setForeground(Color.black);
        deconvolutionGroup.add(geometricMeanButton);
        deconvParamPanel.add(geometricMeanButton, gbc);
        gbc.gridy++;

        deconvShowResultsCheckbox = gui.buildCheckBox("Show deconvolution images", false);
        gbc.gridwidth = 2;
        deconvParamPanel.add(deconvShowResultsCheckbox, gbc);
        gbc.gridy++;

        saveDeconvFolderText = gui.buildFileField("Deconvolution output location:", initDeconvLoc, false, JFileChooser.DIRECTORIES_ONLY);
        deconvParamPanel.add(saveDeconvFolderText.getParent(), gbc);
        gbc.gridy++;
        gbc.gridwidth = 1;

        deconvIterationsText = gui.buildIntegerField("Iterations (1 - 50)", 10);
        deconvParamPanel.add(deconvIterationsText.getParent(), gbc);
        gbc.gridx++;

        deconvUseSigmaConversionFactor = gui.buildCheckBox("Use sigma conversion factor", true);
        deconvParamPanel.add(deconvUseSigmaConversionFactor.getParent(), gbc);
        gbc.gridx = 0;
        gbc.gridy++;

        final JPanel deconvSigmasAPanel = new JPanel(new GridLayout(0, 1));
        deconvSigmasAPanel.setForeground(Color.black);
        deconvSigmasAPanel.setBorder(MipavUtil.buildTitledBorder("Sigmas A (Pre-fusion base)"));

        deconvSigmaAXText = gui.buildDecimalField("X dimension (>= 0.0)", 3.5);
        deconvSigmasAPanel.add(deconvSigmaAXText.getParent());
        deconvSigmaAYText = gui.buildDecimalField("Y dimension (>= 0.0)", 3.5);
        deconvSigmasAPanel.add(deconvSigmaAYText.getParent());
        deconvSigmaAZText = gui.buildDecimalField("Z dimension (>= 0.0)", 9.6);
        deconvSigmasAPanel.add(deconvSigmaAZText.getParent());

        deconvParamPanel.add(deconvSigmasAPanel, gbc);
        gbc.gridx++;

        final JPanel deconvSigmasBPanel = new JPanel(new GridLayout(0, 1));
        deconvSigmasBPanel.setForeground(Color.black);
        deconvSigmasBPanel.setBorder(MipavUtil.buildTitledBorder("Sigmas B (Pre-fusion transform)"));

        deconvSigmaBXText = gui.buildDecimalField("X dimension (>= 0.0)", 9.6);
        deconvSigmasBPanel.add(deconvSigmaBXText.getParent());
        deconvSigmaBYText = gui.buildDecimalField("Y dimension (>= 0.0)", 3.5);
        deconvSigmasBPanel.add(deconvSigmaBYText.getParent());
        deconvSigmaBZText = gui.buildDecimalField("Z dimension (>= 0.0)", 3.5);
        deconvSigmasBPanel.add(deconvSigmaBZText.getParent());

        deconvParamPanel.add(deconvSigmasBPanel, gbc);

        deconvPanel.add(deconvParamPanel, gbc2);

        return deconvPanel;
    }

    private class FolderSaveActionListener implements ActionListener {

        private final JDialogBase parent;

        public FolderSaveActionListener(final JDialogBase parent) {
            this.parent = parent;
        }

        @Override
        public void actionPerformed(final ActionEvent e) {

            transformAriWeightText.getParent().getParent().setVisible(arithmeticMeanSaveBox.isSelected() || arithmeticMeanShowBox.isSelected());
            transformGeoWeightText.getParent().getParent().setVisible(geometricMeanSaveBox.isSelected() || geometricMeanShowBox.isSelected());

            geometricMeanFolderText.getParent().setVisible(geometricMeanSaveBox.isSelected());
            arithmeticMeanFolderText.getParent().setVisible(arithmeticMeanSaveBox.isSelected());

            savePrefusionTransformFolderText.getParent().setVisible(doSavePrefusionBox.isSelected());
            savePrefusionBaseFolderText.getParent().setVisible(doSavePrefusionBox.isSelected());

            // saveMaxProjFolderText.getParent().setVisible(doSaveMaxProjBox.isSelected() ||
            // doShowMaxProjBox.isSelected());

            if (transformFileLocText.getText() != null && transformFileLocText.getText().length() > 0) {

                File rootFolderLoc = new File(transformFileLocText.getText()).getParentFile();

                if (rootFolderLoc.getName().contains("SPIMA") || rootFolderLoc.getName().contains("SPIMB")) {
                    rootFolderLoc = rootFolderLoc.getParentFile();
                }

                final String rootFolderPath = rootFolderLoc.getAbsolutePath();

                try {
                    // only change the folders if they are the same as the initial path (no user change)
                    if (geometricMeanFolderText.getText().equals(initGeoLoc)) {
                        geometricMeanFolderText.setText(rootFolderPath + File.separator + "GeoMean" + File.separator);
                    }
                    if (arithmeticMeanFolderText.getText().equals(initAriLoc)) {
                        arithmeticMeanFolderText.setText(rootFolderPath + File.separator + "AriMean" + File.separator);
                    }
                    if (savePrefusionBaseFolderText.getText().equals(initBasePrefusionLoc)) {
                        savePrefusionBaseFolderText.setText(rootFolderPath + File.separator + "PrefusionBase" + File.separator);
                    }
                    if (savePrefusionTransformFolderText.getText().equals(initTransformPrefusionLoc)) {
                        savePrefusionTransformFolderText.setText(rootFolderPath + File.separator + "PrefusionTransform" + File.separator);
                    }
                    if (saveDeconvFolderText.getText().equals(initDeconvLoc)) {
                        saveDeconvFolderText.setText(rootFolderPath + File.separator + "Deconvolution" + File.separator);
                    }

                    // if(saveMaxProjFolderText.isVisible()) {
                    // if(saveMaxProjFolderText.getText().equals(initMaxProjLoc)) {
                    // saveMaxProjFolderText.setText(new File(transformFileLocText.getText()).getParent() +
                    // File.separator + "MaxProj" + File.separator);
                    // }
                    // }
                } catch (final Exception e1) {
                    e1.printStackTrace();
                }
            }
        }
    }

    private File createDirectory(final String location) {
        File f = null;
        try {
            f = new File(location);
            if ( !f.exists()) {
                f.mkdirs();
            }
        } catch (final Exception e) {
            MipavUtil.displayError("Error making directory " + location);
        }

        return f;
    }

    private boolean createMaxProjFolders(final String parentFolder) {
        if (xMaxBoxSelected) {
            if (createDirectory(parentFolder + File.separator + XPROJ) == null) {
                return false;
            }
        }

        if (yMaxBoxSelected) {
            if (createDirectory(parentFolder + File.separator + YPROJ) == null) {
                return false;
            }
        }

        if (zMaxBoxSelected) {
            if (createDirectory(parentFolder + File.separator + ZPROJ) == null) {
                return false;
            }
        }

        return true;
    }

    /**
     * This method could ensure everything in your dialog box has been set correctly
     * 
     * @return
     */
    private boolean setVariables() {
        registerOne = registerOneButton.isSelected();
        registerAll = registerAllButton.isSelected();
        noRegister2D = noRegister2DButton.isSelected();
        register2DOne = register2DOneButton.isSelected();
        register2DAll = register2DAllButton.isSelected();
        showGeoMean = geometricMeanShowBox.isSelected();
        showAriMean = arithmeticMeanShowBox.isSelected();
        saveGeoMean = geometricMeanSaveBox.isSelected();
        saveAriMean = arithmeticMeanSaveBox.isSelected();
        savePrefusion = doSavePrefusionBox.isSelected();
        doInterImages = interImagesBox.isSelected();
        doShowPreFusion = doShowPrefusionBox.isSelected();
        doThreshold = doThresholdBox.isSelected();
        saveMaxProj = doSaveMaxProjBox.isSelected();
        showMaxProj = doShowMaxProjBox.isSelected();
        xMaxBoxSelected = doXMaxBox.isSelected();
        yMaxBoxSelected = doYMaxBox.isSelected();
        zMaxBoxSelected = doZMaxBox.isSelected();

        doSmartMovement = doSmartMovementBox.isSelected();

        saveType = saveTypeText.getSelectedItem().toString();

        boolean maxProjCreate = true;
        if ( ! (noRegister2D || register2DOne || register2DAll)) {
            if (saveGeoMean) {
                geoMeanDirString = geometricMeanFolderText.getText();
                if ( (geoMeanDir = createDirectory(geoMeanDirString)) == null) {
                    return false;
                }
                if (saveMaxProj) {
                    maxProjCreate = createMaxProjFolders(geoMeanDirString);
                }
            }

            if (saveAriMean) {
                ariMeanDirString = arithmeticMeanFolderText.getText();
                if ( (ariMeanDir = createDirectory(ariMeanDirString)) == null) {
                    return false;
                }
                if (saveMaxProj) {
                    maxProjCreate = createMaxProjFolders(ariMeanDirString);
                }
            }

            if (savePrefusion) {
                prefusionBaseDirString = savePrefusionBaseFolderText.getText();
                prefusionTransformDirString = savePrefusionTransformFolderText.getText();
                if ( (prefusionBaseDir = createDirectory(prefusionBaseDirString)) == null) {
                    return false;
                }
                if ( (prefusionTransformDir = createDirectory(prefusionTransformDirString)) == null) {
                    return false;
                }
                if (saveMaxProj) {
                    maxProjCreate = createMaxProjFolders(prefusionBaseDirString);
                    maxProjCreate = createMaxProjFolders(prefusionTransformDirString);
                }
            }
        } // if (!(noRegister2D || register2DOne || register2DAll))

        if (noRegister2D || register2DOne || register2DAll) {
            register2DFileDirString = register2DFileDirectoryText.getText();
            if ( (register2DFileDir = createDirectory(register2DFileDirString)) == null) {
                return false;
            }
        }

        doDeconv = deconvPerformCheckbox.isSelected();
        if ( ! (noRegister2D || register2DOne || register2DAll)) {
            if (doDeconv) {
                deconvDirString = saveDeconvFolderText.getText();
                if ( (deconvDir = createDirectory(deconvDirString)) == null) {
                    return false;
                }
                if (saveMaxProj) {
                    maxProjCreate = createMaxProjFolders(deconvDirString);
                }
            }
        }

        if ( !maxProjCreate) {
            return false;
        }

        try {

            concurrentNum = Integer.valueOf(concurrentNumText.getText()).intValue();

            thresholdIntensity = Double.valueOf(thresholdIntensityText.getText()).doubleValue();

            resX = Double.valueOf(resXText.getText()).doubleValue();
            resY = Double.valueOf(resYText.getText()).doubleValue();
            if ( ! (noRegister2D || register2DOne || register2DAll)) {
                resZ = Double.valueOf(resZText.getText()).doubleValue();
            }

            if (registerOne || registerAll || register2DOne || register2DAll) {

                if (registerOne || register2DOne) {
                    timeNum = Integer.valueOf(textTimeExt.getText()).intValue();
                }

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
                    final int response = JOptionPane.showConfirmDialog(this, "Warning: with such a large rateX, there will only be 1 sampling.  Continue?",
                            "Sampling warning", JOptionPane.YES_NO_OPTION, JOptionPane.WARNING_MESSAGE);

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
                    final int response = JOptionPane.showConfirmDialog(this, "Warning: with such a large rateX, there will only be 1 sampling.  Continue?",
                            "Sampling warning", JOptionPane.YES_NO_OPTION, JOptionPane.WARNING_MESSAGE);

                    if (response == JOptionPane.NO_OPTION) {
                        showX();
                        coarseRateTextX.requestFocus();
                        coarseRateTextX.selectAll();

                        return false;
                    }
                }

                if ( ! (register2DOne || register2DAll)) {
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
                                    "Warning: with such a large rateY, there will only be 1 sampling.  Continue?", "Sampling warning",
                                    JOptionPane.YES_NO_OPTION, JOptionPane.WARNING_MESSAGE);

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
                                    "Warning: with such a large rateY, there will only be 1 sampling.  Continue?", "Sampling warning",
                                    JOptionPane.YES_NO_OPTION, JOptionPane.WARNING_MESSAGE);

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
                                    "Warning: with such a large rateZ, there will only be 1 sampling.  Continue?", "Sampling warning",
                                    JOptionPane.YES_NO_OPTION, JOptionPane.WARNING_MESSAGE);

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
                                    "Warning: with such a large rateZ, there will only be 1 sampling.  Continue?", "Sampling warning",
                                    JOptionPane.YES_NO_OPTION, JOptionPane.WARNING_MESSAGE);

                            if (response == JOptionPane.NO_OPTION) {
                                showZ();
                                coarseRateTextZ.requestFocus();
                                coarseRateTextZ.selectAll();

                                return false;
                            }
                        }
                    } // (if !(register2DOne || register2DAll))
                } // else universalCheckbox not selected
            } // if (registerOne || registerAll || register2DOne || register2DAll)

            if ( ! (noRegister2D || register2DOne || register2DAll)) {
                if (showAriMean || saveAriMean) {
                    baseAriWeight = Double.valueOf(baseAriWeightText.getText()).doubleValue();
                    transformAriWeight = Double.valueOf(transformAriWeightText.getText()).doubleValue();
                }

                if (showGeoMean || saveGeoMean) {
                    baseGeoWeight = Double.valueOf(baseGeoWeightText.getText()).doubleValue();
                    transformGeoWeight = Double.valueOf(transformGeoWeightText.getText()).doubleValue();
                }

                if ( !doSmartMovement) {
                    xMovement = Integer.valueOf(xMovementText.getText());
                    xMove = xMovement.intValue();
                    yMovement = Integer.valueOf(yMovementText.getText());
                    yMove = yMovement.intValue();
                    zMovement = Integer.valueOf(zMovementText.getText());
                    zMove = zMovement.intValue();
                } else {
                    xMovement = yMovement = zMovement = null;

                    minX = Integer.valueOf(minXText.getText());
                    minY = Integer.valueOf(minYText.getText());
                    minZ = Integer.valueOf(minZText.getText());

                    maxX = Integer.valueOf(maxXText.getText());
                    maxY = Integer.valueOf(maxYText.getText());
                    maxZ = Integer.valueOf(maxZText.getText());

                    if (minX >= maxX) {
                        MipavUtil.displayError("Input error, maxX < minX.");
                        return false;
                    }

                    if (minY >= maxY) {
                        MipavUtil.displayError("Input error, maxY < minY.");
                        return false;
                    }

                    if (minZ >= maxZ) {
                        MipavUtil.displayError("Input error, maxZ < minZ.");
                        return false;
                    }

                    stepSize = Integer.valueOf(stepSizeText.getText());
                }
            } // if (!(noRegister2D || register2DOne || register2DAll))
        } catch (final NumberFormatException nfe) {
            MipavUtil.displayError("Input error, enter numerical values only.");
            return false;
        }

        if ( ! (registerOne || registerAll || register2DOne || register2DAll)) {
            try {
                mtxFileLoc = mtxFileLocText.getText();
                final File f = new File(mtxFileLoc);
                if ( !f.exists()) {
                    MipavUtil.displayError("Matrix file could not be found.");
                    return false;
                }
            } catch (final Exception e) {

                MipavUtil.displayError("Invalid matrix file.");
                return false;
            }
            transformFileDir = transformFileLocText.getText();
            baseFileDir = baseFileLocText.getText();
        } // if (!(registerOne || registerAll || register2DOne || register2DAll))
        else {
            mtxFileDirectory = mtxFileDirectoryText.getText();
            transformFileDir = transformFileLocText.getText();
            if ( (transformFileDir == null) || (transformFileDir.length() == 0)) {
                transformFileDir = mtxFileDirectory + File.separatorChar + "SPIMB";
                transformFileLocText.setText(transformFileDir);
            }
            baseFileDir = baseFileLocText.getText();
            if ( (baseFileDir == null) || (baseFileDir.length() == 0)) {
                baseFileDir = mtxFileDirectory + File.separatorChar + "SPIMA";
                baseFileLocText.setText(baseFileDir);
            }
        }

        baseImage = baseImageText.getText();

        mode = (SampleMode) modeOption.getSelectedItem();
        modeNum = modeOption.getSelectedIndex();
        final String rangeFusion = rangeFusionText.getText();
        HashSet<Integer> includeRange = new HashSet<Integer>();
        if (rangeFusion != null) {
            final String[] ranges = rangeFusion.split("[,;]");
            for (int i = 0; i < ranges.length; i++) {
                final String[] subset = ranges[i].split("-");
                int lowerBound = -1, bound = -1;
                for (int j = 0; j < subset.length; j++) {
                    try {
                        bound = Integer.valueOf(subset[j].trim());
                        if (lowerBound == -1) {
                            lowerBound = bound;
                            includeRange.add(lowerBound);
                        }
                    } catch (final NumberFormatException e) {
                        Preferences.debug("Invalid range specified: " + bound, Preferences.DEBUG_ALGORITHM);
                    }
                }

                for (int k = lowerBound + 1; k <= bound; k++) {
                    includeRange.add(k);
                }
            }
        }

        if (includeRange.size() == 0) {
            includeRange = null;
        }

        if ( !populateFileLists(includeRange)) {
            return false;
        }

        final Object[][] transformMessage = new Object[baseImageAr.length][];
        for (int i = 0; i < baseImageAr.length; i++) {
            transformMessage[i] = new Object[1];
            transformMessage[i][0] = "Image " + transformImageAr[i].getName() + " transformed to " + baseImageAr[i].getName();
        }

        final String[] columns = {"Proceed with the following operations?"};

        final DefaultTableModel d = new DefaultTableModel() {
            @Override
            public boolean isCellEditable(final int row, final int column) {
                return false;
            }
        };
        d.setColumnCount(1);
        d.setColumnIdentifiers(columns);
        for (int i = 0; i < baseImageAr.length; i++) {
            d.addRow(transformMessage[i]);
        }

        fusionConfirmTable = new JTable();

        fusionConfirmTable.setModel(d);

        fusionConfirmTable.addKeyListener(new KeyListener() {

            @Override
            public void keyPressed(final KeyEvent key) {

                switch (key.getKeyCode()) {

                    case KeyEvent.VK_DELETE:
                    case KeyEvent.VK_BACK_SPACE:
                        doDeleteRows();
                }
            }

            @Override
            public void keyReleased(final KeyEvent key) {}

            @Override
            public void keyTyped(final KeyEvent key) {}

        });

        fusionConfirmTable.setRequestFocusEnabled(true);
        fusionConfirmTable.setFocusable(true);

        final JScrollPane scroll = new JScrollPane(fusionConfirmTable);
        scroll.setPreferredSize(new Dimension(400, 300));

        final int returnOption = JOptionPane.showConfirmDialog(this, scroll, "Algorithm run confirm", JOptionPane.YES_NO_OPTION);
        if (returnOption != JOptionPane.YES_OPTION) {
            return false;
        }

        final DefaultTableModel dReturn = (DefaultTableModel) fusionConfirmTable.getModel();
        if (dReturn.getRowCount() != baseImageAr.length) {
            final File[] baseImageArRevised = new File[dReturn.getRowCount()];
            final File[] transformImageArRevised = new File[dReturn.getRowCount()];

            int index = 0;
            for (int i = 0; i < dReturn.getRowCount(); i++) {
                while ( ! ((Vector) dReturn.getDataVector().elementAt(i)).elementAt(0).toString().contains(baseImageAr[index].getName())) {
                    index++;
                }
                baseImageArRevised[i] = baseImageAr[index];
                transformImageArRevised[i] = transformImageAr[index];
            }

            baseImageAr = baseImageArRevised;
            transformImageAr = transformImageArRevised;
        }

        if (showMaxProj || saveMaxProj) {
            minThreshold = 0.0f;
            slidingWindow = 1;

            try {
                minThreshold = Float.valueOf(minThresholdMaxProjText.getText());
                if (doSlideWindowBox.isSelected()) {
                    slidingWindow = Integer.valueOf(slidingWindowText.getText());
                } else {
                    slidingWindow = -1;
                }
            } catch (final NumberFormatException nfe) {
                MipavUtil.displayError("Bad algorithm input for maximum intensity projection.");
                return false;
            }
            setMaxProjVariables();
        }

        // deconvolution parameters
        if (doDeconv) {
            if (JavaButton.isSelected()) {
                deconvPlatform = JavaPlatform;
            } else {
                deconvPlatform = OpenCLPlatform;
            }
            deconvShowResults = deconvShowResultsCheckbox.isSelected();
            if (jointButton.isSelected()) {
                deconvolutionMethod = OpenCLAlgorithmDeconvolution.JOINT_DECON;
            } else if (arithmeticMeanButton.isSelected()) {
                deconvolutionMethod = OpenCLAlgorithmDeconvolution.AVERAGE_DECON;
            } else {
                deconvolutionMethod = OpenCLAlgorithmDeconvolution.MULTIPLICATION_DECON;
            }
            deconvIterations = Integer.valueOf(deconvIterationsText.getText());
            deconvSigmaA = new float[3];
            deconvSigmaA[0] = Float.valueOf(deconvSigmaAXText.getText());
            deconvSigmaA[1] = Float.valueOf(deconvSigmaAYText.getText());
            deconvSigmaA[2] = Float.valueOf(deconvSigmaAZText.getText());
            deconvSigmaB = new float[3];
            deconvSigmaB[0] = Float.valueOf(deconvSigmaBXText.getText());
            deconvSigmaB[1] = Float.valueOf(deconvSigmaBYText.getText());
            deconvSigmaB[2] = Float.valueOf(deconvSigmaBZText.getText());
            useDeconvSigmaConversionFactor = deconvUseSigmaConversionFactor.isSelected();
        }

        if (noBaseRotationButton.isSelected()) {
            baseRotation = -1;
        } else if (minusXBaseRotationButton.isSelected()) {
            baseRotation = AlgorithmRotate.X_AXIS_MINUS;
        } else if (plusXBaseRotationButton.isSelected()) {
            baseRotation = AlgorithmRotate.X_AXIS_PLUS;
        } else if (minusYBaseRotationButton.isSelected()) {
            baseRotation = AlgorithmRotate.Y_AXIS_MINUS;
        } else if (plusYBaseRotationButton.isSelected()) {
            baseRotation = AlgorithmRotate.Y_AXIS_PLUS;
        } else if (invertYBaseRotationButton.isSelected()) {
            baseRotation = AlgorithmRotate.Y_AXIS_180;
        }

        if (noTransformRotationButton.isSelected()) {
            transformRotation = -1;
        } else if (minusXTransformRotationButton.isSelected()) {
            transformRotation = AlgorithmRotate.X_AXIS_MINUS;
        } else if (plusXTransformRotationButton.isSelected()) {
            transformRotation = AlgorithmRotate.X_AXIS_PLUS;
        } else if (minusYTransformRotationButton.isSelected()) {
            transformRotation = AlgorithmRotate.Y_AXIS_MINUS;
        } else if (plusYTransformRotationButton.isSelected()) {
            transformRotation = AlgorithmRotate.Y_AXIS_PLUS;
        } else if (invertYTransformRotationButton.isSelected()) {
            transformRotation = AlgorithmRotate.Y_AXIS_180;
        }

        return true;
    } // end setVariables()

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

    private boolean setMaxProjVariables() {

        int numDim = 0;
        if (xMaxBoxSelected) {
            numDim++;
        }

        if (yMaxBoxSelected) {
            numDim++;
        }

        if (zMaxBoxSelected) {
            numDim++;
        }

        maxAlgo = new AlgorithmMaximumIntensityProjection[numDim];

        // if((maxProjDir = createDirectory(saveMaxProjFolderText)) == null) {
        // return false;
        // }

        int index = 0;
        if (xMaxBoxSelected) {
            maxAlgo[index] = new AlgorithmMaximumIntensityProjection(null, 0, 0, slidingWindow, minThreshold, 0, true, false,
                    AlgorithmMaximumIntensityProjection.X_PROJECTION);
            index++;
        }

        if (yMaxBoxSelected) {
            maxAlgo[index] = new AlgorithmMaximumIntensityProjection(null, 0, 0, slidingWindow, minThreshold, 0, true, false,
                    AlgorithmMaximumIntensityProjection.Y_PROJECTION);
            index++;
        }

        if (zMaxBoxSelected) {
            maxAlgo[index] = new AlgorithmMaximumIntensityProjection(null, 0, 0, slidingWindow, minThreshold, 0, true, false,
                    AlgorithmMaximumIntensityProjection.Z_PROJECTION);
        }

        return true;
    }

    private void doDeleteRows() {
        final int[] rows = fusionConfirmTable.getSelectedRows();
        for (int i = rows.length - 1; i >= 0; i--) {
            ((DefaultTableModel) fusionConfirmTable.getModel()).removeRow(rows[i]);
        }
    }

    private boolean populateFileLists(final HashSet<Integer> includeRange) {
        final ArrayList<File> baseImageList = new ArrayList<File>();
        final ArrayList<File> transformImageList = new ArrayList<File>();
        try {
            File fB = new File(transformFileDir);
            File fA = new File(baseFileDir);
            boolean containsFiles = false;
            for (final File fTry : fA.listFiles()) {
                if (fTry.getName().contains(baseImage)) {
                    containsFiles = true;
                    break;
                }
            }
            if ( !containsFiles) {
                fA = new File(transformFileDir);
                fB = new File(baseFileDir);
            }

            if ( !fA.exists() || !fA.isDirectory()) {
                MipavUtil.displayError("Spim file directories could not be found");
                return false;
            }
            search: for (final File fTry : fA.listFiles()) {
                final String s = fTry.getName();
                if ( !s.contains(".mtx")) {
                    if (s.contains(baseImage)) {
                        final String[] subSection = s.split(baseImage);
                        searchSub: for (final File fTrySub : fB.listFiles()) {
                            final String sSub = fTrySub.getName();
                            if ( !s.contains(".mtx")) {
                                for (final String subParts : subSection) {
                                    if ( !sSub.contains(subParts)) {
                                        continue searchSub;
                                    }
                                }
                                // matching subsections have been found
                                baseImageList.add(fTry);
                                transformImageList.add(fTrySub);
                                continue search;
                            }
                        }
                    }
                }
            }
        } catch (final Exception e) {
            MipavUtil.displayError("Invalid spim directories");
            return false;
        }

        if (includeRange != null) {
            final int originalSize = baseImageList.size();
            for (int i = originalSize; i > 0; i--) {
                final int index = getIndex(baseImageList.get(i - 1));
                if ( !includeRange.contains(index)) {
                    baseImageList.remove(i - 1);
                    transformImageList.remove(i - 1);
                }
            }
        }

        if (registerOne || register2DOne) {
            timeIndex = -1;
            for (int i = 0; i < baseImageList.size(); i++) {
                final int index = getIndex(baseImageList.get(i));
                if (index == timeNum) {
                    timeIndex = i;
                    break;
                }
            }
            if (timeIndex == -1) {
                MipavUtil.displayError("Error in image number to register");
                return false;
            }
        } // if (registerOne || register2DOne)

        if (noRegister2D || register2DOne || register2DAll) {
            if ( (register2DFileDir = createDirectory(register2DFileDirString)) == null) {
                return false;
            }
        }

        final FileCompare f = new FileCompare();
        Collections.sort(baseImageList, f);
        Collections.sort(transformImageList, f);

        baseImageAr = baseImageList.toArray(new File[baseImageList.size()]);
        transformImageAr = transformImageList.toArray(new File[transformImageList.size()]);

        mode = SampleMode.values()[modeNum];

        boolean maxProjCreate = true;
        if ( ! (noRegister2D || register2DOne || register2DAll)) {
            if (doDeconv) {
                if ( (deconvDir = createDirectory(deconvDirString)) == null) {
                    return false;
                }
                if (saveMaxProj) {
                    maxProjCreate = createMaxProjFolders(deconvDirString);
                }
            } // if (doDeconv)

            if (saveGeoMean) {
                if ( (geoMeanDir = createDirectory(geoMeanDirString)) == null) {
                    return false;
                }
                if (saveMaxProj) {
                    maxProjCreate = createMaxProjFolders(geoMeanDirString);
                }
            } // if (saveGeoMean)

            if (saveAriMean) {
                if ( (ariMeanDir = createDirectory(ariMeanDirString)) == null) {
                    return false;
                }
                if (saveMaxProj) {
                    maxProjCreate = createMaxProjFolders(ariMeanDirString);
                }
            }

            if (savePrefusion) {
                if ( (prefusionBaseDir = createDirectory(prefusionBaseDirString)) == null) {
                    return false;
                }
                if ( (prefusionTransformDir = createDirectory(prefusionTransformDirString)) == null) {
                    return false;
                }
                if (saveMaxProj) {
                    maxProjCreate = createMaxProjFolders(prefusionBaseDirString);
                    maxProjCreate = createMaxProjFolders(prefusionTransformDirString);
                }
            }
        } // if (!(noRegister2D || register2DOne || register2DAll))

        if ( !maxProjCreate) {
            return false;
        }

        return true;
    }

    private int getIndex(final File file) {
        final String name = file.getName();
        int upper = -1, lower = -1;
        boolean inRange = false;
        for (int i = name.length(); i > 0; i--) {
            if (Character.isDigit(name.charAt(i - 1))) {
                if ( !inRange) {
                    upper = i;
                    lower = i;
                    inRange = true;
                } else {
                    lower = i;
                }
            } else {
                inRange = false;
                if (upper != -1 && lower != -1) {
                    break;
                }
            }
        }

        try {
            return Integer.valueOf(name.substring(lower - 1, upper)).intValue();
        } catch (final Exception e) {
            return -1;
        }
    }

    private class FileCompare implements Comparator<File> {
        @Override
        public int compare(final File arg0, final File arg1) {
            if (arg0.getName().length() != arg1.getName().length()) {
                return arg0.getName().length() - arg1.getName().length();
            }

            return arg0.getName().compareTo(arg1.getName());
        }
    }
}
