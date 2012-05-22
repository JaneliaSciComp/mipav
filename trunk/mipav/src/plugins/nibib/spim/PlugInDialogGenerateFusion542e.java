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

import gov.nih.mipav.model.scripting.*;
import gov.nih.mipav.model.scripting.parameters.ParameterFactory;
import gov.nih.mipav.model.structures.*;

import gov.nih.mipav.view.*;
import gov.nih.mipav.view.dialogs.GuiBuilder;
import gov.nih.mipav.view.dialogs.JDialogScriptableBase;

import java.awt.*;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.KeyEvent;
import java.awt.event.KeyListener;
import java.io.File;
import java.io.FileNotFoundException;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.Comparator;
import java.util.Enumeration;
import java.util.HashSet;
import java.util.Iterator;
import java.util.Vector;

import javax.swing.*;
import javax.swing.table.DefaultTableModel;
import javax.swing.text.JTextComponent;

import nibib.spim.PlugInAlgorithmGenerateFusion542e.SampleMode;


/**
 * Class for performing image fusion based on reference image and transformation matrix.  Option to output geometric/arithmetic mean.
 * 
 * @version  June 4, 2010
 * @see      JDialogBase
 * @see      AlgorithmInterface
 *
 * @author Justin Senseney (SenseneyJ@mail.nih.gov)
 * @see http://mipav.cit.nih.gov
 */
public class PlugInDialogGenerateFusion542e extends JDialogScriptableBase implements AlgorithmInterface {
    
    //~ Static fields/initializers -------------------------------------------------------------------------------------

    /**declare UID */
    
    final String initAriLoc = new File(Preferences.getImageDirectory()).getParent() + File.separator + "AriMean" + File.separator;
    final String initGeoLoc = new File(Preferences.getImageDirectory()).getParent() + File.separator + "GeoMean" + File.separator;
    final String initTransformPrefusionLoc = new File(Preferences.getImageDirectory()).getParent() + File.separator + "PrefusionTransform" + File.separator;
    final String initBasePrefusionLoc = new File(Preferences.getImageDirectory()).getParent() + File.separator + "PrefusionBase" + File.separator;
    
    //~ Instance fields ------------------------------------------------------------------------------------------------

    private ModelImage resultImage = null;

    /** This source image is typically set by the constructor */
    private ModelImage image; // 
    
    /** This is your algorithm */
    private PlugInAlgorithmGenerateFusion542e generateFusionAlgo = null;

    private JTextField mtxFileLocText, transformFileLocText, baseFileLocText;

    private JCheckBox geometricMeanShowBox, arithmeticMeanShowBox, interImagesBox;

    private JPanel okCancelPanel;

    private JTextField transformImageText, baseImageText;

    private JCheckBox doShowPrefusionBox;

    private String mtxFileLoc;

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
    
  //~ Constructors ---------------------------------------------------------------------------------------------------
    
    /**
     * Constructor used for instantiation during script execution (required for dynamic loading).
     */
    public PlugInDialogGenerateFusion542e() { }

    /**
     * Creates new dialog for kidney segmentation from an abdominal cavity image using a plugin.
     *
     * @param  theParentFrame  Parent frame.
     * @param  im              Source image.
     */
    public PlugInDialogGenerateFusion542e(boolean modal) {
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
            dispose();
        }
    } // end actionPerformed()

    /**
     * This method is required if the AlgorithmPerformed interface is implemented. It is called by the algorithm when it
     * has completed or failed to to complete, so that the dialog can be display the result image and/or clean up.
     *
     * @param  algorithm  Algorithm that caused the event.
     */
    public void algorithmPerformed(AlgorithmBase algorithm) {
        if (algorithm instanceof PlugInAlgorithmGenerateFusion542e) {
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

            dispose();
        }

    } // end algorithmPerformed()

    
    /**
     * Once all the necessary variables are set, call the kidney segmentation algorithm
     */
    protected void callAlgorithm() {

        try {
            
            generateFusionAlgo = new PlugInAlgorithmGenerateFusion542e(doShowPreFusion, doInterImages, showGeoMean, showAriMean, doThreshold, 
                                                                         resX, resY, resZ, concurrentNum, thresholdIntensity,
                                                                                mtxFileLoc, baseImageAr, transformImageAr, 
                                                                                xMovement, yMovement, zMovement, mode, 
                                                                                minX, minY, minZ, maxX, maxY, maxZ, stepSize, 
                                                                                saveGeoMean, geoMeanDir, saveAriMean, ariMeanDir, 
                                                                                savePrefusion, prefusionBaseDir, prefusionTransformDir, 
                                                                                baseAriWeight, transformAriWeight, baseGeoWeight, transformGeoWeight);

            // This is very important. Adding this object as a listener allows the algorithm to
            // notify this object when it has completed or failed. See algorithm performed event.
            // This is made possible by implementing AlgorithmedPerformed interface
            generateFusionAlgo.addListener(this);
            createProgressBar("Creating plugin", " ...", generateFusionAlgo);

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
     * Used in turning your plugin into a script
     */
    protected void setGUIFromParams() {

    	showAriMean = scriptParameters.getParams().getBoolean("do_arithmetic");
    	showGeoMean = scriptParameters.getParams().getBoolean("do_geometric");
    	doInterImages = scriptParameters.getParams().getBoolean("do_interImages");
    	doShowPreFusion = scriptParameters.getParams().getBoolean("do_subsample");
    	doThreshold = scriptParameters.getParams().getBoolean("do_threshold");
    	
    	mtxFileLoc = scriptParameters.getParams().getFile("mtxFileLoc");
    	transformFileDir = scriptParameters.getParams().getFile("spimAFileDir");
    	baseFileDir = scriptParameters.getParams().getFile("spimBFileDir");
    	
    	baseImage = scriptParameters.getParams().getString("baseImage");
    	
    	populateFileLists(null);
    	
    } //end setGUIFromParams()

    /**
     * Used in turning your plugin into a script
     */
    protected void storeParamsFromGUI() throws ParserException {
   
        scriptParameters.getParams().put(ParameterFactory.newParameter("do_arithmetic", showAriMean));
        scriptParameters.getParams().put(ParameterFactory.newParameter("do_geometric", showGeoMean));
        scriptParameters.getParams().put(ParameterFactory.newParameter("do_interImages", doInterImages));
        scriptParameters.getParams().put(ParameterFactory.newParameter("do_subsample", doShowPreFusion));
        scriptParameters.getParams().put(ParameterFactory.newParameter("do_threshold", doThreshold));
       
        scriptParameters.getParams().put(ParameterFactory.newParameter("mtxFileLoc", mtxFileLoc));
        scriptParameters.getParams().put(ParameterFactory.newParameter("spimAFileDir", transformFileDir));
        scriptParameters.getParams().put(ParameterFactory.newParameter("spimBFileDir", baseFileDir));
        
        scriptParameters.getParams().put(ParameterFactory.newParameter("baseImageText", baseImageText));
    } //end storeParamsFromGUI()
   
    private void init() {
        setResizable(true);
        setForeground(Color.black);
        setTitle("Generate fusion 542e");
        try {
            setIconImage(MipavUtil.getIconImage("divinci.gif"));
        } catch (FileNotFoundException e) {
            Preferences.debug("Failed to load default icon", Preferences.DEBUG_MINOR);
        }
        
        GuiBuilder gui = new GuiBuilder(this);

        GridBagConstraints gbc = new GridBagConstraints();
        gbc.gridwidth = 1;
        gbc.gridheight = 1;
        gbc.anchor = GridBagConstraints.WEST;
        gbc.weightx = 1;
        gbc.insets = new Insets(3, 3, 3, 3);
        gbc.fill = GridBagConstraints.HORIZONTAL;
        gbc.gridx = 0;
        gbc.gridy = 0;

        JPanel mainPanel = new JPanel(new GridBagLayout());
        mainPanel.setForeground(Color.black);

        JPanel mtxPanel = new JPanel(new GridBagLayout());
        mtxPanel.setForeground(Color.black);
        mtxPanel.setBorder(buildTitledBorder("File information"));
        
        mtxFileLocText = gui.buildFileField("Matrix file location: ", "", false, JFileChooser.FILES_ONLY);
        mtxPanel.add(mtxFileLocText.getParent(), gbc);
        gbc.gridy++;
        
        transformFileLocText = gui.buildFileField("Directory containing transform image: ", "", false, JFileChooser.DIRECTORIES_ONLY);
        mtxPanel.add(transformFileLocText.getParent(), gbc);
        gbc.gridy++;
        
        baseFileLocText = gui.buildFileField("Directory containing base image: ", "", false, JFileChooser.DIRECTORIES_ONLY);
        mtxPanel.add(baseFileLocText.getParent(), gbc);
        gbc.gridy++;
        
        JPanel transformPanel = new JPanel();
        FlowLayout transformFlow = new FlowLayout(FlowLayout.LEFT);
        transformPanel.setLayout(transformFlow);
        
        transformImageText = gui.buildField("Transform image ", "SPIMA");
        transformPanel.add(transformImageText.getParent());
        
        baseImageText = gui.buildField(" to image ", "SPIMB");
        transformPanel.add(baseImageText.getParent());
        
        mtxPanel.add(transformPanel, gbc);
        gbc.gridy++;
        
        rangeFusionText = gui.buildField("Range of images to fuse (ex. 3-7, 12, 18-21, etc.): ", " ");
        mtxPanel.add(rangeFusionText.getParent(), gbc);
        gbc.gridy++;
        
        JLabel dirMove = new JLabel("Enter translation to apply to transformed image (optional)");
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
        
        mainPanel.add(mtxPanel, gbc);
        
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
        
        resZText = gui.buildDecimalField("Z: ", .5);
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
        
        gbc.gridy = 1;
        gbc.gridx = 0;
        mainPanel.add(algOptionPanel, gbc);
        
        
        FolderSaveActionListener folderSave = new FolderSaveActionListener();
        JPanel outputPanel = new JPanel(new GridBagLayout());
        outputPanel.setForeground(Color.black);
        outputPanel.setBorder(MipavUtil.buildTitledBorder("Output options"));
        
        
        JPanel prefusionPanel = new JPanel(new GridBagLayout());
        prefusionPanel.setForeground(Color.black);
        prefusionPanel.setBorder(MipavUtil.buildTitledBorder("Prefusion options"));
        gbc.gridy = 0; 
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
        gbc.gridy = 0;
        gbc.gridwidth = 1;
        savePrefusionTransformFolderText.getParent().setVisible(false);
        
        JPanel arithmeticPanel = new JPanel(new GridBagLayout());
        arithmeticPanel.setForeground(Color.black);
        arithmeticPanel.setBorder(MipavUtil.buildTitledBorder("Arithmetic options"));
        arithmeticMeanShowBox = gui.buildCheckBox("Show arithmetic mean", true);
        arithmeticPanel.add(arithmeticMeanShowBox.getParent(), gbc);
        gbc.gridx++;
        
        arithmeticMeanSaveBox = gui.buildCheckBox("Save arithmetic mean", false);
        arithmeticPanel.add(arithmeticMeanSaveBox.getParent(), gbc);
        gbc.gridy++;
        gbc.gridwidth = 2;
        gbc.gridx = 0;
        final JPanel ariWeightPanel = new JPanel(new GridBagLayout());
        ariWeightPanel.setForeground(Color.black);
        flow = new FlowLayout(FlowLayout.LEFT);
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
        gbc.gridy = 0;
        gbc.gridwidth = 1;
        arithmeticMeanFolderText.getParent().setVisible(false);
        
        arithmeticMeanSaveBox.addActionListener(folderSave);
        
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
        flow = new FlowLayout(FlowLayout.LEFT);
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
        
        geometricMeanFolderText = gui.buildFileField("Geometric mean folder location: ", initGeoLoc, false, JFileChooser.DIRECTORIES_ONLY);
        geometricPanel.add(geometricMeanFolderText.getParent(), gbc);
        gbc.gridwidth = 1;
        gbc.gridy++;
        geometricMeanFolderText.getParent().setVisible(false);
        
        geometricMeanSaveBox.addActionListener(folderSave);
        
        
        gbc.gridy = 0;
        outputPanel.add(prefusionPanel, gbc);
        gbc.gridy++;
        outputPanel.add(arithmeticPanel, gbc);
        gbc.gridy++;
        outputPanel.add(geometricPanel, gbc);
        gbc.gridy++;
        
        interImagesBox = gui.buildCheckBox("Show intermediate images", false);
        outputPanel.add(interImagesBox.getParent(), gbc);
        
        
        mtxFileLocText.addActionListener(new ActionListener() {
            public void actionPerformed(ActionEvent e) {
                try {
                    File f = new File(mtxFileLocText.getText()).getParentFile().getParentFile().getParentFile();
                    Preferences.setImageDirectory(f);
                } catch(Exception ex) {}
            }
        });
        
        gbc.gridy = 2;
        mainPanel.add(outputPanel, gbc);
        
        gbc.gridy++;
        okCancelPanel = gui.buildOKCancelPanel();
        mainPanel.add(okCancelPanel, gbc);
        
        JScrollPane scroll = new JScrollPane(mainPanel);

        getContentPane().add(scroll, BorderLayout.CENTER);
        
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
    
    
    private class FolderSaveActionListener implements ActionListener {
        
        public void actionPerformed(ActionEvent e) {
            
            transformAriWeightText.getParent().getParent().setVisible(arithmeticMeanSaveBox.isSelected() || arithmeticMeanShowBox.isSelected());
            transformGeoWeightText.getParent().getParent().setVisible(geometricMeanSaveBox.isSelected() || geometricMeanShowBox.isSelected());
            
            geometricMeanFolderText.getParent().setVisible(geometricMeanSaveBox.isSelected());
            arithmeticMeanFolderText.getParent().setVisible(arithmeticMeanSaveBox.isSelected());
            
            savePrefusionTransformFolderText.getParent().setVisible(doSavePrefusionBox.isSelected());
            savePrefusionBaseFolderText.getParent().setVisible(doSavePrefusionBox.isSelected());
            
            if(transformFileLocText.getText() != null && 
                    transformFileLocText.getText().length() > 0) {
                try {
                    if(geometricMeanFolderText.isVisible()) {
                        if(geometricMeanFolderText.getText().equals(initGeoLoc)) {
                            geometricMeanFolderText.setText(new File(transformFileLocText.getText()).getParent() + File.separator + "GeoMean" + File.separator);
                        }
                    }
                    
                    if(arithmeticMeanFolderText.isVisible()) {
                        if(arithmeticMeanFolderText.getText().equals(initAriLoc)) {
                            arithmeticMeanFolderText.setText(new File(transformFileLocText.getText()).getParent() + File.separator + "AriMean" + File.separator);
                        }
                    }
                    
                    if(savePrefusionBaseFolderText.isVisible()) {
                        if(baseImageText.getText() != null && baseImageText.getText().length() > 0) {
                            if(savePrefusionBaseFolderText.getText().equals(initBasePrefusionLoc) || 
                                    !savePrefusionBaseFolderText.getText().contains(baseImageText.getText())) {
                                savePrefusionBaseFolderText.setText(new File(baseFileLocText.getText()).getParent() + File.separator + "Prefusion" +baseImageText.getText()+ File.separator);
                            }
                        }
                    }
                    
                    if(savePrefusionTransformFolderText.isVisible()) {
                        if(transformImageText.getText() != null && transformImageText.getText().length() > 0) {
                            if(savePrefusionTransformFolderText.getText().equals(initTransformPrefusionLoc) || 
                                    !savePrefusionTransformFolderText.getText().contains(transformImageText.getText())) {
                                savePrefusionTransformFolderText.setText(new File(transformFileLocText.getText()).getParent() + File.separator + "Prefusion" +transformImageText.getText()+ File.separator);
                            }
                        }
                    }
                    
                } catch(Exception e1) {
                    e1.printStackTrace();
                }
            }
            
            
            
        }
        
    }
    
    private File createDirectory(JTextField location) {
        File f = null;
        try {
            f = new File(location.getText());
            if(!f.exists()) {
                f.mkdirs();
            }
        } catch(Exception e) {
            MipavUtil.displayError("Error making directory "+location.getText());
        }
        
        return f;
    }

    /**
     * This method could ensure everything in your dialog box has been set correctly
     * 
     * @return
     */
	private boolean setVariables() {
	    showGeoMean = geometricMeanShowBox.isSelected();
        showAriMean = arithmeticMeanShowBox.isSelected();
        saveGeoMean = geometricMeanSaveBox.isSelected();
        saveAriMean = arithmeticMeanSaveBox.isSelected();
        savePrefusion = doSavePrefusionBox.isSelected();
        doInterImages = interImagesBox.isSelected();
        doShowPreFusion = doShowPrefusionBox.isSelected();
        doThreshold = doThresholdBox.isSelected();
        doInterImages = interImagesBox.isSelected();
	    
        doSmartMovement = doSmartMovementBox.isSelected();
        
        if(saveGeoMean) {
            if((geoMeanDir = createDirectory(geometricMeanFolderText)) == null) {
                return false;
            }
        }
        
        if(saveAriMean) {
            if((ariMeanDir = createDirectory(arithmeticMeanFolderText)) == null) {
                return false;
            }
        }
        
        if(savePrefusion) {
            prefusionBaseDir = createDirectory(savePrefusionBaseFolderText);
            prefusionTransformDir = createDirectory(savePrefusionTransformFolderText);
        }
                
	    try {
		    
		    concurrentNum = Integer.valueOf(concurrentNumText.getText()).intValue();
		    
		    thresholdIntensity = Double.valueOf(thresholdIntensityText.getText()).doubleValue();
		    
		    resX = Double.valueOf(resXText.getText()).doubleValue();
		    resY = Double.valueOf(resYText.getText()).doubleValue();
		    resZ = Double.valueOf(resZText.getText()).doubleValue();
		    
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
	    
		return true;
	} //end setVariables()

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
            File fA = new File(transformFileDir);
            File fB = new File(baseFileDir);
            boolean containsFiles = false;
            for(File fTry : fA.listFiles()) {
                if(fTry.getName().contains(baseImage)) {
                    containsFiles = true;
                    continue;
                }
            }
            if(!containsFiles) {
                fB = new File(transformFileDir);
                fA = new File(baseFileDir);
            }
            
            if(!fA.exists() || !fA.isDirectory()) {
                MipavUtil.displayError("Spim file directories could not be found");
                return false;
            }
    search:   for(File fTry : fA.listFiles()) {
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
