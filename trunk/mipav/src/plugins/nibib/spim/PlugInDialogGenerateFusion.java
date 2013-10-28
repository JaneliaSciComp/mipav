package nibib.spim;
//MIPAV is freely available from http://mipav.cit.nih.gov

//THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, 
//EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES 
//OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND 
//NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT 
//HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, 
//WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING 
//FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE 
//OR OTHER DEALINGS IN THE SOFTWARE. 

/*****************************************************************
******************************************************************

The MIPAV application is intended for research use only.
This application has NOT been approved for ANY diagnostic use 
by the Food and Drug Administration. There is currently no 
approval process pending. 

This software may NOT be used for diagnostic purposes.

******************************************************************
******************************************************************/

import gov.nih.mipav.model.algorithms.*;
import gov.nih.mipav.model.algorithms.utilities.AlgorithmMaximumIntensityProjection;

import gov.nih.mipav.model.scripting.*;
import gov.nih.mipav.model.scripting.parameters.ParameterFactory;
import gov.nih.mipav.model.structures.*;
import gov.nih.mipav.plugins.JDialogStandaloneScriptablePlugin;

import gov.nih.mipav.view.*;
import gov.nih.mipav.view.dialogs.GuiBuilder;
import gov.nih.mipav.view.dialogs.JDialogBase;

import java.awt.*;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.ItemEvent;
import java.awt.event.KeyEvent;
import java.awt.event.KeyListener;
import java.awt.event.WindowEvent;
import java.io.File;
import java.io.FileNotFoundException;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.Comparator;
import java.util.HashSet;
import java.util.Iterator;
import java.util.Vector;

import javax.swing.*;
import javax.swing.event.ChangeListener;
import javax.swing.table.DefaultTableModel;

import nibib.spim.PlugInAlgorithmGenerateFusion.SampleMode;


/**
 * Class for performing image fusion based on reference image and transformation matrix.  Option to output geometric/arithmetic mean.
 * 
 * @version  October 29, 2013
 * @see      JDialogBase
 * @see      AlgorithmInterface
 *
 * @author Justin Senseney (SenseneyJ@mail.nih.gov)
 * @see http://mipav.cit.nih.gov
 */
public class PlugInDialogGenerateFusion extends JDialogStandaloneScriptablePlugin implements AlgorithmInterface {
    
    //~ Static fields/initializers -------------------------------------------------------------------------------------

	private static final long serialVersionUID = 7916311305902468003L;
    
    public static final String XPROJ = "XProj";
    public static final String YPROJ = "YProj";
    public static final String ZPROJ = "ZProj";
    
    final String initAriLoc = new File(Preferences.getImageDirectory()).getParent() + File.separator + "AriMean" + File.separator;
    final String initGeoLoc = new File(Preferences.getImageDirectory()).getParent() + File.separator + "GeoMean" + File.separator;
    final String initTransformPrefusionLoc = new File(Preferences.getImageDirectory()).getParent() + File.separator + "PrefusionTransform" + File.separator;
    final String initBasePrefusionLoc = new File(Preferences.getImageDirectory()).getParent() + File.separator + "PrefusionBase" + File.separator;
    final String initDeconvLoc = new File(Preferences.getImageDirectory()).getParent() + File.separator + "Deconvolution" + File.separator;
    
    //~ Instance fields ------------------------------------------------------------------------------------------------

    private ModelImage resultImage = null;

    /** This source image is typically set by the constructor */
    private ModelImage image;
    
    /** This is your algorithm */
    private PlugInAlgorithmGenerateFusion generateFusionAlgo = null;
    
    private GridBagConstraints gbc;

    private JTextField mtxFileLocText, mtxFileDirectoryText, transformFileLocText, baseFileLocText;

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

    private SampleMode mode;
    
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

    private File ariMeanDir;
    private JTextField savePrefusionTransformFolderText;
    private JTextField savePrefusionBaseFolderText;
    private JCheckBox doSavePrefusionBox;
    private boolean savePrefusion;
    private File prefusionBaseDir;
    private File prefusionTransformDir;
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
    /** Max projection lower intensity threshold text field */
    private JTextField minThresholdMaxProjText;
    /** MIP algorithm for later processing */
    private AlgorithmMaximumIntensityProjection[] maxAlgo;
    private JTextField slidingWindowText;
    private JCheckBox doSlideWindowBox;
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
	/** Panel containing deconvolution parameters. */
	private JPanel deconvParamPanel;
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
	/** The text field for setting the directory to save deconvolved files to. */
	private JTextField saveDeconvFolderText;
	private JCheckBox registrationCheckbox;
	private boolean register = true;
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
	
  //~ Constructors ---------------------------------------------------------------------------------------------------
    
    /**
     * Constructor used for instantiation during script execution (required for dynamic loading).
     */
    public PlugInDialogGenerateFusion() { }

    /**
     * Creates new dialog for kidney segmentation from an abdominal cavity image using a plugin.
     *
     * @param  theParentFrame  Parent frame.
     * @param  im              Source image.
     */
    public PlugInDialogGenerateFusion(boolean modal) {
        super(modal);
        
        init();
    }
    
    //  ~ Methods --------------------------------------------------------------------------------------------------------

    // ************************************************************************
    // ************************** Event Processing ****************************
    // ************************************************************************

    /**
     * Closes dialog box when the OK button is pressed and calls the algorithm.
     *
     * @param  event  Event that triggers function.
     */
    public void actionPerformed(ActionEvent event) {
        String command = event.getActionCommand();

        if (command.equals("OK")) {
            if (setVariables()) {
                callAlgorithm();
            }
        } else if (command.equals("Script")) {
            callAlgorithm();
        } else if (command.equals("Cancel")) {
            //dispose();
        	this.windowClosing(new WindowEvent(this, WindowEvent.WINDOW_CLOSING));
        } else {
            super.actionPerformed(event);
        }
        //System.out.print(this.getSize());
    } // end actionPerformed()

    /**
     * This method is required if the AlgorithmPerformed interface is implemented. It is called by the algorithm when it
     * has completed or failed to to complete, so that the dialog can be display the result image and/or clean up.
     *
     * @param  algorithm  Algorithm that caused the event.
     */
    public void algorithmPerformed(AlgorithmBase algorithm) {
        if (algorithm instanceof PlugInAlgorithmGenerateFusion) {
            Preferences.debug("Elapsed: " + algorithm.getElapsedTime());
            
            if ((generateFusionAlgo.isCompleted() == true)) {
                Collection<ModelImage> list = generateFusionAlgo.getResultImageList();
                synchronized(list) {
                    Iterator<ModelImage> itr = list.iterator();
                    while(itr.hasNext()) {
                        new ViewJFrameImage(itr.next());
                    }
                }
                insertScriptLine();
            } 

            if (generateFusionAlgo != null) {
                generateFusionAlgo.finalize();
                generateFusionAlgo = null;
            }

            //dispose();
            if (super.isExitRequired()) {
                ViewUserInterface.getReference().windowClosing(new WindowEvent(this, WindowEvent.WINDOW_CLOSING));
            }
        }
    } // end algorithmPerformed()
    
    /**
     * Once all the necessary variables are set, call the kidney segmentation algorithm
     */
    protected void callAlgorithm() {

        try {
            
            generateFusionAlgo = new PlugInAlgorithmGenerateFusion(register, rotateBeginX, rotateEndX, coarseRateX, fineRateX, rotateBeginY, 
                                                                   rotateEndY, coarseRateY, fineRateY, rotateBeginZ, rotateEndZ, coarseRateZ,
                                                                   fineRateZ,doShowPreFusion, doInterImages, showGeoMean, showAriMean, 
                                                                   showMaxProj, doThreshold, resX, resY, resZ, concurrentNum, thresholdIntensity,
                                                                                mtxFileLoc, mtxFileDirectory, timeIndex, baseImageAr, transformImageAr, 
                                                                                xMovement, yMovement, zMovement, mode, 
                                                                                minX, minY, minZ, maxX, maxY, maxZ, stepSize, 
                                                                                saveMaxProj, saveGeoMean, geoMeanDir, saveAriMean, ariMeanDir, 
                                                                                savePrefusion, prefusionBaseDir, prefusionTransformDir, 
                                                                                baseAriWeight, transformAriWeight, baseGeoWeight, transformGeoWeight, 
                                                                                maxAlgo, saveType, doDeconv, deconvIterations, deconvSigmaA,
                                                                                deconvSigmaB, useDeconvSigmaConversionFactor, deconvDir, deconvShowResults);

            // This is very important. Adding this object as a listener allows the algorithm to
            // notify this object when it has completed or failed. See algorithm performed event.
            // This is made possible by implementing AlgorithmedPerformed interface
            generateFusionAlgo.addListener(this);
            //createProgressBar("Creating plugin", " ...", generateFusionAlgo);

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
        } catch (OutOfMemoryError x) {
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
     * Used in turning your plugin into a script
     */
    protected void setGUIFromParams() {

    	register = scriptParameters.getParams().getBoolean("reg");
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
        showAriMean = scriptParameters.getParams().getBoolean("show_arithmetic");
    	showGeoMean = scriptParameters.getParams().getBoolean("show_geometric");
    	doInterImages = scriptParameters.getParams().getBoolean("do_interImages");
    	doShowPreFusion = scriptParameters.getParams().getBoolean("do_subsample");
    	doThreshold = scriptParameters.getParams().getBoolean("do_threshold");
    	
    	if (register) {
    	    mtxFileDirectory = scriptParameters.getParams().getFile("mtxFileDirectory");
    	    timeNum = scriptParameters.getParams().getInt("time_num");
    	}
    	else {
    	    mtxFileLoc = scriptParameters.getParams().getFile("mtxFileLoc");
    	}
    	transformFileDir = scriptParameters.getParams().getFile("spimAFileDir");
    	baseFileDir = scriptParameters.getParams().getFile("spimBFileDir");
    	
    	baseImage = scriptParameters.getParams().getString("baseImage");
    	
    	populateFileLists(null);
    	
    } //end setGUIFromParams()

    /**
     * Used in turning your plugin into a script
     */
    protected void storeParamsFromGUI() throws ParserException {
   
        scriptParameters.getParams().put(ParameterFactory.newParameter("reg", register));
        scriptParameters.getParams().put(
                ParameterFactory.newParameter("rotate_begin", new float[] {rotateBeginX, rotateBeginY, rotateBeginZ}));
        scriptParameters.getParams().put(
                ParameterFactory.newParameter("rotate_end", new float[] {rotateEndX, rotateEndY, rotateEndZ}));
        scriptParameters.getParams().put(
                ParameterFactory.newParameter("coarse_rate", new float[] {coarseRateX, coarseRateY, coarseRateZ}));
        scriptParameters.getParams().put(
                ParameterFactory.newParameter("fine_rate", new float[] {fineRateX, fineRateY, fineRateZ}));
        scriptParameters.getParams().put(ParameterFactory.newParameter("show_arithmetic", showAriMean));
        scriptParameters.getParams().put(ParameterFactory.newParameter("show_geometric", showGeoMean));
        scriptParameters.getParams().put(ParameterFactory.newParameter("do_interImages", doInterImages));
        scriptParameters.getParams().put(ParameterFactory.newParameter("do_subsample", doShowPreFusion));
        scriptParameters.getParams().put(ParameterFactory.newParameter("do_threshold", doThreshold));
       
        if (register) {
            scriptParameters.getParams().put(ParameterFactory.newParameter("mtxFileDirectory", mtxFileDirectory));
            scriptParameters.getParams().put(ParameterFactory.newParameter("time_num", timeNum));
        }
        else {
            scriptParameters.getParams().put(ParameterFactory.newParameter("mtxFileLoc", mtxFileLoc));
        }
        scriptParameters.getParams().put(ParameterFactory.newParameter("spimAFileDir", transformFileDir));
        scriptParameters.getParams().put(ParameterFactory.newParameter("spimBFileDir", baseFileDir));
        
        scriptParameters.getParams().put(ParameterFactory.newParameter("baseImageText", baseImageText));
    } //end storeParamsFromGUI()
   
    private GridBagConstraints createGBC() {
        GridBagConstraints gbc = new GridBagConstraints();
        
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
        setTitle("Generate fusion 544e");
        try {
            setIconImage(MipavUtil.getIconImage("divinci.gif"));
        } catch (FileNotFoundException e) {
            Preferences.debug("Failed to load default icon", Preferences.DEBUG_MINOR);
        }
        
        GuiBuilder gui = new GuiBuilder(this);

        gbc = new GridBagConstraints();
        gbc.gridwidth = 1;
        gbc.gridheight = 1;
        gbc.anchor = GridBagConstraints.WEST;
        gbc.weightx = 1;
        gbc.insets = new Insets(3, 3, 3, 3);
        gbc.fill = GridBagConstraints.HORIZONTAL;
        gbc.gridx = 0;
        gbc.gridy = 0;

        JPanel leftPanel = new JPanel(new GridBagLayout());
        leftPanel.setForeground(Color.black);

        JPanel mtxPanel = new JPanel(new GridBagLayout());
        mtxPanel.setForeground(Color.black);
        mtxPanel.setBorder(buildTitledBorder("File information"));
        
        registrationCheckbox = gui.buildCheckBox("Run registration", true);
        mtxPanel.add(registrationCheckbox.getParent(), gbc);
        gbc.gridy++;
        
        GridBagConstraints gbc2 = new GridBagConstraints();
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
        
        JLabel labelTimeExt = new JLabel("Image number to register :");
        labelTimeExt.setForeground(Color.black);
        labelTimeExt.setFont(serif12);
        timeNumberPanel.add(labelTimeExt, gbc2);
        
        textTimeExt = new JTextField(20);
        textTimeExt.setForeground(Color.black);
        textTimeExt.setFont(serif12);
        gbc2.gridx = 1;
        timeNumberPanel.add(textTimeExt, gbc2);
        gbc2.gridx = 0;
        gbc2.gridy++;
        
        gbc2.gridwidth = 2;
        mtxFileDirectoryText = gui.buildFileField("Directory containing matrix file: ", " ", false, JFileChooser.DIRECTORIES_ONLY);
        timeNumberPanel.add(mtxFileDirectoryText.getParent(), gbc2);
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
        
        timeNumberPanel.add(rotatePanel, gbc2);
        
        mtxPanel.add(timeNumberPanel, gbc);
        gbc.gridy++;
        
        GridBagConstraints gbc3 = new GridBagConstraints();
        gbc3.gridwidth = 1;
        gbc3.gridheight = 1;
        gbc3.anchor = GridBagConstraints.WEST;
        gbc3.weightx = 1;
        gbc3.fill = GridBagConstraints.HORIZONTAL;
        gbc3.gridx = 0;
        gbc3.gridy = 0; 
        final JPanel matrixFilePanel = new JPanel(new GridBagLayout());
        matrixFilePanel.setForeground(Color.black);
        
        mtxFileLocText = gui.buildFileField("Matrix file: ", "", false, JFileChooser.FILES_ONLY);
        matrixFilePanel.add(mtxFileLocText.getParent(), gbc3);
        matrixFilePanel.setVisible(false);
        
        mtxPanel.add(matrixFilePanel, gbc);
        gbc.gridy++;
        
        registrationCheckbox.addActionListener(new ActionListener() {
            public void actionPerformed(ActionEvent arg0) {
                timeNumberPanel.setVisible(registrationCheckbox.isSelected());  
                matrixFilePanel.setVisible(!registrationCheckbox.isSelected());
            }
        });
        
        FolderSaveActionListener folderSave = new FolderSaveActionListener(this);
        
        transformFileLocText = gui.buildFileField("Directory containing transform image: ", "", false, JFileChooser.DIRECTORIES_ONLY, folderSave);
        mtxPanel.add(transformFileLocText.getParent(), gbc);
        gbc.gridy++;
        
        baseFileLocText = gui.buildFileField("Directory containing base image: ", "", false, JFileChooser.DIRECTORIES_ONLY);
        mtxPanel.add(baseFileLocText.getParent(), gbc);
        gbc.gridy++;
        
        JPanel transformPanel = new JPanel();
        FlowLayout transformFlow = new FlowLayout(FlowLayout.LEFT);
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
        
        saveTypeText = gui.buildComboBox("Save result images as type: ", new String[]{"Tiff", "Raw"}, 0);
        mtxPanel.add(saveTypeText.getParent(), gbc);
        gbc.gridy++;
        
        JLabel dirMove = new JLabel("Enter translation to apply to transformed image (optional):");
        mtxPanel.add(dirMove, gbc);
        gbc.gridy++;
        
        final JPanel movementPanel = new JPanel();
        FlowLayout movementFlow = new FlowLayout(FlowLayout.LEFT);
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
        GridBagConstraints smartGBC = new GridBagConstraints();
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
        
        doSmartMovementBox.addActionListener(new ActionListener(){
            public void actionPerformed(ActionEvent arg0) {
                xMovementText.setEnabled(!doSmartMovementBox.isSelected());
                yMovementText.setEnabled(!doSmartMovementBox.isSelected());
                zMovementText.setEnabled(!doSmartMovementBox.isSelected());
                
                smartPanel.setVisible(doSmartMovementBox.isSelected());
            }
        });
        
        gbc.gridx = 0;
        gbc.gridy = 0;
        
        leftPanel.add(mtxPanel, gbc);
        
        JPanel rightPanel = new JPanel(new GridBagLayout());
        rightPanel.setForeground(Color.black);
        
        JPanel algOptionPanel = new JPanel(new GridBagLayout());
        algOptionPanel.setForeground(Color.black);
        algOptionPanel.setBorder(MipavUtil.buildTitledBorder("Algorithm options"));
        
        JLabel resLabel = new JLabel("Initial resolutions (um): ");
        algOptionPanel.add(resLabel, gbc);
        gbc.gridy++;
        
        JPanel resPanel = new JPanel(new GridBagLayout());
        resPanel.setForeground(Color.black);
        FlowLayout flow = new FlowLayout(FlowLayout.LEFT);
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
        
        modeOption =  gui.buildComboBox("Sampling mode", SampleMode.values());
        algOptionPanel.add(modeOption.getParent(), gbc);
        gbc.gridy++;
        
        concurrentNumText = gui.buildIntegerField("Number of concurrent fusions: ", 
                                                    (Runtime.getRuntime().availableProcessors() - 2) > 1 ? Runtime.getRuntime().availableProcessors()-2 : 1);
        algOptionPanel.add(concurrentNumText.getParent(), gbc);
        gbc.gridy++;
        
        doThresholdBox = gui.buildCheckBox("Threshold noise", false);
        algOptionPanel.add(doThresholdBox.getParent(), gbc);
        gbc.gridy++;
        
        final JPanel thresholdPanel = new JPanel(new GridBagLayout());
        GridBagConstraints gbcThreshold = new GridBagConstraints();
        thresholdPanel.setForeground(Color.black);
        
        gbcThreshold.gridx = 0;
        gbcThreshold.gridy = 0;
        
        thresholdIntensityText = gui.buildDecimalField("Threshold value", 10);
        thresholdPanel.add(thresholdIntensityText.getParent(), gbcThreshold);
        
        algOptionPanel.add(thresholdPanel, gbc);
        gbc.gridy++;
        
        doThresholdBox.addActionListener(new ActionListener() {
            public void actionPerformed(ActionEvent e) {
                thresholdPanel.setVisible(doThresholdBox.isSelected());
            }
        });
        
        thresholdPanel.setVisible(doThresholdBox.isSelected());
        
        gbc.gridy = 0;
        gbc.gridx = 0;
        rightPanel.add(algOptionPanel, gbc);
        
        
        
        JPanel outputPanel = new JPanel(new GridBagLayout());
        outputPanel.setForeground(Color.black);
        outputPanel.setBorder(MipavUtil.buildTitledBorder("Output options"));

        JPanel prefusionPanel = buildPrefusionPanel(gui, folderSave);
        
        JPanel arithmeticPanel = buildArithmeticPanel(gui, folderSave);
        
        JPanel geometricPanel = buildGeometricPanel(gui, folderSave);

        JPanel maxProjPanel = buildMaxProjPanel(gui, folderSave);
        
        JPanel deconvPanel = buildDeconvolutionPanel(gui, folderSave);
        
        gbc.gridy = 0;
        //gbc.gridwidth = 1;
        outputPanel.add(prefusionPanel, gbc);
        gbc.gridy++;
        outputPanel.add(arithmeticPanel, gbc);
        gbc.gridy++;
        outputPanel.add(geometricPanel, gbc);
        gbc.gridy++;
        outputPanel.add(maxProjPanel, gbc);
        gbc.gridy++;
        outputPanel.add(deconvPanel, gbc);
        gbc.gridy++;
        
        interImagesBox = gui.buildCheckBox("Show intermediate images", false);
        outputPanel.add(interImagesBox.getParent(), gbc);
        
        
        mtxFileLocText.addActionListener(new ActionListener() {
            public void actionPerformed(ActionEvent e) {
                try {
                    File f;
                    if (registrationCheckbox.isSelected()) {
                        f = new File(mtxFileLocText.getText()).getParentFile().getParentFile();
                    }
                    else {
                        f = new File(mtxFileLocText.getText()).getParentFile().getParentFile().getParentFile();    
                    }
                    Preferences.setImageDirectory(f);
                } catch(Exception ex) {}
            }
        });
        
        gbc.gridy = 1;
        rightPanel.add(outputPanel, gbc);
        
        gbc.gridy++;
        okCancelPanel = gui.buildOKCancelPanel();
        rightPanel.add(okCancelPanel, gbc);
        
        tabbedPane = new JTabbedPane();
        tabbedPane.setFont(MipavUtil.font12B);
        tabbedPane.addTab("File", leftPanel);

        tabbedPane.addTab("Options", rightPanel);

        getContentPane().add(tabbedPane, BorderLayout.CENTER);
        
        Dimension dim = Toolkit.getDefaultToolkit().getScreenSize();
        if(dim.getHeight() < 950) {
            getContentPane().setMaximumSize(new Dimension(685, 850));
            getContentPane().setPreferredSize(new Dimension(685, 850));
        }
        
        
        pack();
        setVisible(true);
        setResizable(true);
        
        
        System.gc();
        
    } // end init()
    
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
    
    
    private JPanel buildMaxProjPanel(GuiBuilder gui, ActionListener folderSave) {
        GridBagConstraints gbc = createGBC();
        JPanel maxProjPanel = new JPanel(new GridBagLayout());
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
        
        gbc.gridx+=2;
        gbc.gridwidth = 1;
        slidingWindowText = gui.buildIntegerField("Sliding window", 1);
        slidingWindowText.setEnabled(false);
        maxProjPanel.add(slidingWindowText.getParent(), gbc);
        
        doSlideWindowBox.addActionListener(new ActionListener() {
            public void actionPerformed(ActionEvent e) {
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
        
//        saveMaxProjFolderText = gui.buildFileField("Maximum projection image location:", initMaxProjLoc, false, JFileChooser.DIRECTORIES_ONLY);
//        maxProjPanel.add(saveMaxProjFolderText.getParent(), gbc);
//        gbc.gridy++;  
//        saveMaxProjFolderText.getParent().setVisible(false);
        
        return maxProjPanel;
    }

    private JPanel buildPrefusionPanel(GuiBuilder gui, ActionListener folderSave) {
        GridBagConstraints gbc = createGBC();
        JPanel prefusionPanel = new JPanel(new GridBagLayout());
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

    private JPanel buildArithmeticPanel(GuiBuilder gui, ActionListener folderSave) {
        GridBagConstraints gbc = createGBC();
        JPanel arithmeticPanel = new JPanel(new GridBagLayout());
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
        FlowLayout flow = new FlowLayout(FlowLayout.LEFT);
        ariWeightPanel.setLayout(flow);
        
        transformAriWeightText = gui.buildDecimalField("Transformed image arithmetic weight: ", 1.0);
        ariWeightPanel.add(transformAriWeightText.getParent(), gbc);
        
        baseAriWeightText = gui.buildDecimalField("Base image arithmetic weight: ", 1.0);
        ariWeightPanel.add(baseAriWeightText.getParent(), gbc);
        
        arithmeticMeanShowBox.addActionListener(new ActionListener() {
            public void actionPerformed(ActionEvent arg0) {
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

    private JPanel buildGeometricPanel(GuiBuilder gui, ActionListener folderSave) {
        GridBagConstraints gbc = createGBC();
        JPanel geometricPanel = new JPanel(new GridBagLayout());
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
        FlowLayout flow = new FlowLayout(FlowLayout.LEFT);
        geoWeightPanel.setLayout(flow);
        
        transformGeoWeightText = gui.buildDecimalField("Transformed image geometric weight: ", 1.0);
        geoWeightPanel.add(transformGeoWeightText.getParent(), gbc);
        
        baseGeoWeightText = gui.buildDecimalField("Base image geometric weight: ", 1.0);
        geoWeightPanel.add(baseGeoWeightText.getParent(), gbc);
        
        geometricMeanShowBox.addActionListener(new ActionListener() {
            public void actionPerformed(ActionEvent e) {
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
    
    private JPanel buildDeconvolutionPanel(GuiBuilder gui, ActionListener folderSave) {
    	GridBagConstraints gbc2 = createGBC();
    	JPanel deconvPanel = new JPanel(new GridBagLayout());
        deconvPanel.setForeground(Color.black);
        deconvPanel.setBorder(MipavUtil.buildTitledBorder("Deconvolution options"));
        
        if (OpenCLAlgorithmBase.isOCLAvailable() ) {
            deconvPerformCheckbox = gui.buildCheckBox("Perform deconvolution", true);
        }
        else {
            deconvPerformCheckbox = gui.buildCheckBox("Perform deconvolution", false);    
        }
        deconvPerformCheckbox.addActionListener(new ActionListener() {
            public void actionPerformed(ActionEvent e) {
            	deconvParamPanel.setVisible(deconvPerformCheckbox.isSelected());
            }
        });
        deconvPanel.add(deconvPerformCheckbox.getParent(), gbc2);
        gbc2.gridy++;
        
        if ( !OpenCLAlgorithmBase.isOCLAvailable() ) {
        	//MipavUtil.displayError( "OpenCL is not available on any platform" );
        	deconvPerformCheckbox.setEnabled(false);
        }
        
        GridBagConstraints gbc = createGBC();
        deconvParamPanel = new JPanel(new GridBagLayout());
        deconvParamPanel.setForeground(Color.black);
        deconvParamPanel.setVisible(false);
        //deconvParamPanel.setBorder(MipavUtil.buildTitledBorder("Deconvolution options"));
        
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
        
        JPanel deconvSigmasAPanel = new JPanel(new GridLayout(0,1));
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
        
        JPanel deconvSigmasBPanel = new JPanel(new GridLayout(0,1));
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
        
        private JDialogBase parent;

        public FolderSaveActionListener(JDialogBase parent) {
            this.parent = parent;
        }
        
        public void actionPerformed(ActionEvent e) {
            
            transformAriWeightText.getParent().getParent().setVisible(arithmeticMeanSaveBox.isSelected() || arithmeticMeanShowBox.isSelected());
            transformGeoWeightText.getParent().getParent().setVisible(geometricMeanSaveBox.isSelected() || geometricMeanShowBox.isSelected());
            
            geometricMeanFolderText.getParent().setVisible(geometricMeanSaveBox.isSelected());
            arithmeticMeanFolderText.getParent().setVisible(arithmeticMeanSaveBox.isSelected());
            
            savePrefusionTransformFolderText.getParent().setVisible(doSavePrefusionBox.isSelected());
            savePrefusionBaseFolderText.getParent().setVisible(doSavePrefusionBox.isSelected());
            
//            saveMaxProjFolderText.getParent().setVisible(doSaveMaxProjBox.isSelected() || doShowMaxProjBox.isSelected());
            
            if(transformFileLocText.getText() != null && 
                    transformFileLocText.getText().length() > 0) {
                
                File rootFolderLoc = new File(transformFileLocText.getText()).getParentFile();
                
                if(rootFolderLoc.getName().contains("SPIMA") || rootFolderLoc.getName().contains("SPIMB")) {
                    rootFolderLoc = rootFolderLoc.getParentFile();
                }
                
                String rootFolderPath = rootFolderLoc.getAbsolutePath();
                
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
                    
//                    if(saveMaxProjFolderText.isVisible()) {
//                        if(saveMaxProjFolderText.getText().equals(initMaxProjLoc)) {
//                            saveMaxProjFolderText.setText(new File(transformFileLocText.getText()).getParent() + File.separator + "MaxProj" + File.separator);
//                        }
//                    }
                } catch(Exception e1) {
                    e1.printStackTrace();
                }
            }
        }
    }
    
    private File createDirectory(String location) {
        File f = null;
        try {
            f = new File(location);
            if(!f.exists()) {
                f.mkdirs();
            }
        } catch(Exception e) {
            MipavUtil.displayError("Error making directory "+location);
        }
        
        return f;
    }

    private boolean createMaxProjFolders(String parentFolder) {
        if(doXMaxBox.isSelected()) {
            if(createDirectory(parentFolder+File.separator+XPROJ) == null) {
                return false;
            }
        }
        
        if(doYMaxBox.isSelected()) {
            if(createDirectory(parentFolder+File.separator+YPROJ) == null) {
                return false;
            }
        }
        
        if(doZMaxBox.isSelected()) {
            if(createDirectory(parentFolder+File.separator+ZPROJ) == null) {
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
	    register = registrationCheckbox.isSelected();
	    showGeoMean = geometricMeanShowBox.isSelected();
        showAriMean = arithmeticMeanShowBox.isSelected();
        saveGeoMean = geometricMeanSaveBox.isSelected();
        saveAriMean = arithmeticMeanSaveBox.isSelected();
        savePrefusion = doSavePrefusionBox.isSelected();
        doInterImages = interImagesBox.isSelected();
        doShowPreFusion = doShowPrefusionBox.isSelected();
        doThreshold = doThresholdBox.isSelected();
        doInterImages = interImagesBox.isSelected();
        saveMaxProj = doSaveMaxProjBox.isSelected();
        showMaxProj = doShowMaxProjBox.isSelected();
	    
        doSmartMovement = doSmartMovementBox.isSelected();
        
        saveType = saveTypeText.getSelectedItem().toString();
        
        boolean maxProjCreate = true;
        if(saveGeoMean) {
            if((geoMeanDir = createDirectory(geometricMeanFolderText.getText())) == null) {
                return false;
            }
            if(saveMaxProj) {
                maxProjCreate = createMaxProjFolders(geometricMeanFolderText.getText());
            }
        }
        
        if(saveAriMean) {
            if((ariMeanDir = createDirectory(arithmeticMeanFolderText.getText())) == null) {
                return false;
            }
            if(saveMaxProj) {
                maxProjCreate = createMaxProjFolders(arithmeticMeanFolderText.getText());
            }
        }
        
        if(savePrefusion) {
            if((prefusionBaseDir = createDirectory(savePrefusionBaseFolderText.getText())) == null) {
                return false;
            }
            if((prefusionTransformDir = createDirectory(savePrefusionTransformFolderText.getText())) == null) {
                return false;
            }
            if(saveMaxProj) {
                maxProjCreate = createMaxProjFolders(savePrefusionBaseFolderText.getText());
                maxProjCreate = createMaxProjFolders(savePrefusionTransformFolderText.getText());
            }
        }
        
        doDeconv = deconvPerformCheckbox.isSelected();
        if (doDeconv) {
        	if((deconvDir = createDirectory(saveDeconvFolderText.getText())) == null) {
                return false;
            }
        	if (saveMaxProj) {
        		maxProjCreate = createMaxProjFolders(saveDeconvFolderText.getText());
        	}
        }
        
        if(!maxProjCreate) {
            return false;
        }
                
	    try {
		    
		    concurrentNum = Integer.valueOf(concurrentNumText.getText()).intValue();
		    
		    thresholdIntensity = Double.valueOf(thresholdIntensityText.getText()).doubleValue();
		    
		    resX = Double.valueOf(resXText.getText()).doubleValue();
		    resY = Double.valueOf(resYText.getText()).doubleValue();
		    resZ = Double.valueOf(resZText.getText()).doubleValue();
		    
		    if (register) {
                timeNum = Integer.valueOf(textTimeExt.getText()).intValue();
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
            } // if (register)
		    
		    if(showAriMean || saveAriMean) {
    		    baseAriWeight = Double.valueOf(baseAriWeightText.getText()).doubleValue();
    		    transformAriWeight = Double.valueOf(transformAriWeightText.getText()).doubleValue();
		    }
		    
		    if(showGeoMean || saveGeoMean) {
    		    baseGeoWeight = Double.valueOf(baseGeoWeightText.getText()).doubleValue();
                transformGeoWeight = Double.valueOf(transformGeoWeightText.getText()).doubleValue();
		    }
		    
		    if(!doSmartMovement) {
    		    xMovement = Integer.valueOf(xMovementText.getText());
    		    yMovement = Integer.valueOf(yMovementText.getText());
    		    zMovement = Integer.valueOf(zMovementText.getText());
		    } else {
		        xMovement = yMovement = zMovement = null;
		        
		        minX = Integer.valueOf(minXText.getText());
                minY = Integer.valueOf(minYText.getText());
                minZ = Integer.valueOf(minZText.getText());
                
                maxX = Integer.valueOf(maxXText.getText());
                maxY = Integer.valueOf(maxYText.getText());
                maxZ = Integer.valueOf(maxZText.getText());
                
                if(minX >= maxX) {
                    MipavUtil.displayError("Input error, maxX < minX.");
                    return false;
                }
                
                if(minY >= maxY) {
                    MipavUtil.displayError("Input error, maxY < minY.");
                    return false;    
                }
                
                if(minZ >= maxZ) {
                    MipavUtil.displayError("Input error, maxZ < minZ.");
                    return false;
                }
                
                stepSize = Integer.valueOf(stepSizeText.getText());
		    }
		} catch(NumberFormatException nfe) {
            MipavUtil.displayError("Input error, enter numerical values only.");
            return false;
        }
	      
	    if (!register) {
    	    try {
    	        mtxFileLoc = mtxFileLocText.getText();
    	        File f = new File(mtxFileLoc);
    	        if(!f.exists()) {
    	            MipavUtil.displayError("Matrix file could not be found.");
    	            return false;
    	        }
    	    } catch(Exception e) {
    	        
    	        MipavUtil.displayError("Invalid matrix file.");
    	        return false;
    	    }
	    } // if (!register)
	    else {
	        mtxFileDirectory = mtxFileDirectoryText.getText();
	    }
	    
	    transformFileDir = transformFileLocText.getText();
	    baseFileDir = baseFileLocText.getText();
	    baseImage = baseImageText.getText();
	    
	    mode = (SampleMode) modeOption.getSelectedItem();
	    
	    String rangeFusion = rangeFusionText.getText();
	    HashSet<Integer> includeRange = new HashSet<Integer>();
	    if(rangeFusion != null) {  
	        String[] ranges = rangeFusion.split("[,;]");
	        for(int i=0; i<ranges.length; i++) {
	            String[] subset = ranges[i].split("-");
	            int lowerBound = -1, bound = -1;
	            for(int j=0; j<subset.length; j++) {
	                try {
	                    bound = Integer.valueOf(subset[j].trim());
	                    if(lowerBound == -1) {
	                        lowerBound = bound;
	                        includeRange.add(lowerBound);
	                    } 
	                } catch(NumberFormatException e) {
	                    Preferences.debug("Invalid range specified: "+bound, Preferences.DEBUG_ALGORITHM);
	                }
	            }
	            
	            for(int k=lowerBound+1; k<=bound; k++) {
                    includeRange.add(k);
                }
	        }
	    }
	    
	    if(includeRange.size() == 0) {
	        includeRange = null;
	    }
	   
	    if(!populateFileLists(includeRange)) {
	        return false;
	    }
		
	    Object[][] transformMessage = new Object[baseImageAr.length][];
	    for(int i=0; i<baseImageAr.length; i++) {
	        transformMessage[i] = new Object[1];
	        transformMessage[i][0] = "Image "+transformImageAr[i].getName()+" transformed to "+baseImageAr[i].getName();
	    }
	    
	    String[] columns = {"Proceed with the following operations?"};
	    
	    DefaultTableModel d = new DefaultTableModel() {
            public boolean isCellEditable(int row, int column) {
                return false;
            }
	    };
	    d.setColumnCount(1);
	    d.setColumnIdentifiers(columns);
	    for(int i=0; i<baseImageAr.length; i++) {
	        d.addRow(transformMessage[i]);
	    }
	    
	    fusionConfirmTable = new JTable();

	    fusionConfirmTable.setModel(d);

	    fusionConfirmTable.addKeyListener(new KeyListener() {

            public void keyPressed(KeyEvent key) {
              
                switch(key.getKeyCode()) {
               
                case KeyEvent.VK_DELETE:
                case KeyEvent.VK_BACK_SPACE:
                    doDeleteRows();
                }
            }

            

            public void keyReleased(KeyEvent key) {}

            public void keyTyped(KeyEvent key) {}
	        
	    });
	    
	    fusionConfirmTable.setRequestFocusEnabled(true);
	    fusionConfirmTable.setFocusable(true);
	    
	    JScrollPane scroll = new JScrollPane(fusionConfirmTable);
	    scroll.setPreferredSize(new Dimension(400, 300));
	    
	    int returnOption = JOptionPane.showConfirmDialog(this, scroll, "Algorithm run confirm", JOptionPane.YES_NO_OPTION);
	    if(returnOption != JOptionPane.YES_OPTION) {
	        return false;
	    }
	    
	    
	    
	    DefaultTableModel dReturn = (DefaultTableModel) fusionConfirmTable.getModel();
	    if(dReturn.getRowCount() != baseImageAr.length) {
	        File[] baseImageArRevised = new File[dReturn.getRowCount()];
	        File[] transformImageArRevised = new File[dReturn.getRowCount()];
	        
	        int index = 0;
	        for(int i=0; i<dReturn.getRowCount(); i++) {
	            while(!((Vector)dReturn.getDataVector().elementAt(i)).elementAt(0).toString().contains(baseImageAr[index].getName())) {
	                index++;
	            }
	            baseImageArRevised[i] = baseImageAr[index];
	            transformImageArRevised[i] = transformImageAr[index];
	        }
	        
	        baseImageAr = baseImageArRevised;
	        transformImageAr = transformImageArRevised;
	    }
	    
	    if(showMaxProj || saveMaxProj) {
	        setMaxProjVariables();
	    }
	    
	    // deconvolution parameters
	    if (doDeconv) {
	    	deconvShowResults = deconvShowResultsCheckbox.isSelected();
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
	    
		return true;
	} //end setVariables()
	
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
        if(doXMaxBox.isSelected()) {
            numDim++;
        }
        
        if(doYMaxBox.isSelected()) {
            numDim++;
        }
        
        if(doZMaxBox.isSelected()) {
            numDim++;
        }
        
        maxAlgo = new AlgorithmMaximumIntensityProjection[numDim];
        
        float minThreshold = 0.0f;
        int slidingWindow = 1;
        
        try {
            minThreshold = Float.valueOf(minThresholdMaxProjText.getText());
            if(doSlideWindowBox.isSelected()) {
                slidingWindow = Integer.valueOf(slidingWindowText.getText());
            } else {
                slidingWindow = -1;
            }
        } catch(NumberFormatException nfe) {
            MipavUtil.displayError("Bad algorithm input for maximum intensity projection.");
            return false;
        }
        
//        if((maxProjDir = createDirectory(saveMaxProjFolderText)) == null) {
//            return false;
//        }
        
        int index = 0;
        if(doXMaxBox.isSelected()) {
            maxAlgo[index] = new AlgorithmMaximumIntensityProjection(null, 0, 0, slidingWindow, minThreshold, 0, true, false, AlgorithmMaximumIntensityProjection.X_PROJECTION);
            index++;
        }
        
        if(doYMaxBox.isSelected()) {
            maxAlgo[index] = new AlgorithmMaximumIntensityProjection(null, 0, 0, slidingWindow, minThreshold, 0, true, false, AlgorithmMaximumIntensityProjection.Y_PROJECTION);
            index++;
        }
        
        if(doZMaxBox.isSelected()) {
            maxAlgo[index] = new AlgorithmMaximumIntensityProjection(null, 0, 0, slidingWindow, minThreshold, 0, true, false, AlgorithmMaximumIntensityProjection.Z_PROJECTION);
        }
        
        return true;
    }

    private void doDeleteRows() {
        int[] rows = fusionConfirmTable.getSelectedRows();
        for(int i=rows.length-1; i>=0; i--) {
            ((DefaultTableModel)fusionConfirmTable.getModel()).removeRow(rows[i]);
        }
    }
	
    private boolean populateFileLists(HashSet<Integer> includeRange) {
        ArrayList<File> baseImageList = new ArrayList<File>();
        ArrayList<File> transformImageList = new ArrayList<File>();
        try {
            File fB = new File(transformFileDir);
            File fA = new File(baseFileDir);
            boolean containsFiles = false;
            for(File fTry : fA.listFiles()) {
                if(fTry.getName().contains(baseImage)) {
                    containsFiles = true;
                    break;
                }
            }
            if(!containsFiles) {
                fA = new File(transformFileDir);
                fB = new File(baseFileDir);
            }
            
            if(!fA.exists() || !fA.isDirectory()) {
                MipavUtil.displayError("Spim file directories could not be found");
                return false;
            }
    search: for(File fTry : fA.listFiles()) {
                String s = fTry.getName(); 
                if(!s.contains(".mtx")) {
                    if(s.contains(baseImage)) {
                        String[] subSection = s.split(baseImage);
            searchSub:  for(File fTrySub : fB.listFiles()) {
                            String sSub = fTrySub.getName();
                            if(!s.contains(".mtx")) {
                                for(String subParts : subSection) {
                                    if(!sSub.contains(subParts)) {
                                        continue searchSub;
                                    }
                                }
                                //matching subsections have been found
                                baseImageList.add(fTry);
                                transformImageList.add(fTrySub);
                                continue search;
                            }
                        }
                    }
                }
            }
        } catch(Exception e) {
            MipavUtil.displayError("Invalid spim directories");
            return false;
        }
        
        if(includeRange != null) {
            int originalSize = baseImageList.size();
            for(int i=originalSize; i>0; i--) {
                int index = getIndex(baseImageList.get(i-1));
                if(!includeRange.contains(index)) {
                    baseImageList.remove(i-1);
                    transformImageList.remove(i-1);
                }
            }
        }
        
        if (register) {
           timeIndex = -1;
           for (int i = 0; i < baseImageList.size(); i++) {
               int index = getIndex(baseImageList.get(i));
               if (index == timeNum) {
                   timeIndex = i;
                   break;
               }
           }
           if (timeIndex == -1) {
               MipavUtil.displayError("Error in image number to register");
               return false;
           }
        } // if (register)
        
        FileCompare f = new FileCompare();
        Collections.sort(baseImageList, f);
        Collections.sort(transformImageList, f);
        
        baseImageAr = baseImageList.toArray(new File[baseImageList.size()]);
        transformImageAr = transformImageList.toArray(new File[transformImageList.size()]);
        
        return true;
    }
    
    private int getIndex(File file) {
        String name = file.getName();
        int upper = -1, lower = -1;
        boolean inRange = false;
        for(int i=name.length(); i > 0; i--) {
            if(Character.isDigit(name.charAt(i-1))) {
                if(!inRange) {
                    upper = i;
                    lower = i;
                    inRange = true;
                } else {
                    lower = i;
                }
            } else {
                inRange = false;
                if(upper != -1 && lower != -1) {
                    break;
                }
            }
        }
        
        try {
            return Integer.valueOf(name.substring(lower-1, upper)).intValue();
        } catch(Exception e) {
            return -1;
        }
    }


    private class FileCompare implements Comparator<File> {
        public int compare(File arg0, File arg1) {
            if(arg0.getName().length() != arg1.getName().length()) {
                return arg0.getName().length() - arg1.getName().length();
            }
            
            return arg0.getName().compareTo(arg1.getName());
        }
    }
}
