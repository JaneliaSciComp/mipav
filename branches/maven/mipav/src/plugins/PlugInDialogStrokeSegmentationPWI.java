import gov.nih.mipav.plugins.JDialogStandaloneScriptablePlugin;

import gov.nih.mipav.model.algorithms.*;
import gov.nih.mipav.model.file.*;
import gov.nih.mipav.model.scripting.*;
import gov.nih.mipav.model.scripting.parameters.ParameterFactory;
import gov.nih.mipav.model.structures.*;

import gov.nih.mipav.view.*;

import java.awt.*;
import java.awt.event.ActionEvent;
import java.awt.event.WindowEvent;
import java.io.File;

import javax.swing.*;


public class PlugInDialogStrokeSegmentationPWI extends JDialogStandaloneScriptablePlugin implements AlgorithmInterface {
    private static final long serialVersionUID = -8203006546174953787L;
    
    private JRadioButton dirMethodRadio;
    private JRadioButton fileMethodRadio;
    
    private JTextField dirFileField;
    
    private String dirFileString;
    
    private String adcPath = null;
    private String dwiPath = null;
    
    private String pwiPath = null;
    
    private boolean foundADC = false;
    private boolean foundDWI = false;
    private boolean foundPWI = false;
    
    private String outputDir;
    
    private JTextField adcImageFileField;
    private JTextField dwiImageFileField;
    private JTextField pwiImageFileField;
    
    private JTextField adcThresholdField;
    
    private JRadioButton symmetryRadio;
    private JTextField symmetryRemovalMaxSliceField;
    
    private JCheckBox filterCheckbox;
    
    private JRadioButton cerebellumRadio;
    private JTextField cerebellumSliceMaxField;
    
//    private JCheckBox cerebellumAggressiveCheckbox;
//    private JTextField cerebellumAggressiveSliceMaxField;
    
    private JCheckBox skullRemovalCheckbox;
    
    private JCheckBox selectAdditionalObjCheckbox;
    private JTextField selectAdditionalObjPctField;

    private boolean adcImageMultifile = false;
    private ModelImage adcImage;
    
    private boolean dwiImageMultifile = false;
    private ModelImage dwiImage;
    
    private boolean pwiImageMultifile = false;
    private ModelImage pwiImage = null;
    
    private int adcThreshold = 620;
    
    private boolean doSymmetryRemoval = true;
    private int symmetryRemovalMaxSlice = 10;
    
    private boolean doFilter = true;
    
    private boolean doCerebellumSkip = false;
    
    private int cerebellumSkipSliceMax = 10;
    
//    private boolean doCerebellumSkipAggressive = true;
    
//    private int cerebellumSkipAggressiveSliceMax = 15;
    
    private boolean doSkullRemoval = true;
    
    private boolean doSelectAdditionalObj = false;
    
    private int selectAdditionalObjPct = 30;
    
    private JTextField threshCloseIterField;
    
    private int threshCloseIter = 1;
    
    private JTextField threshCloseSizeField;
    
    private float threshCloseSize = 4f;
    
    private JCheckBox requireMinCoreSizeCheckbox;
    private JTextField minCoreSizeCCField;
    
    private boolean requireMinCoreSize = true;
    private float minCoreSizeCC = 0.2f;
    
    private JCheckBox pwiMultithreadCheckbox;
    private boolean doPwiMultithread = true;
    
    private JCheckBox pwiCalcCorrMapCheckbox;
    private boolean doPwiCalcCorrMap = true;
    
    private JTextField corrmapMaskThresholdField;
    private float corrmapMaskThreshold = 0.5f;
    
    private JCheckBox useTmaxInCorrmapMaskCheckbox;
    private boolean useTmaxInCorrmapMask = false;
    
    private JCheckBox pwiCalcCBFCBVMTTCheckbox;
    private boolean doPwiCalcCBFCBVMTT = false;
    
    private JCheckBox pwiSaveOutputFilesCheckbox;
    private boolean doPwiSaveOutputFiles = false;
    
    private JCheckBox spatialSmoothingCheckBox;
    private boolean spatialSmoothing = false;
    
    private JLabel sigmaXLabel;
    private JTextField sigmaXText;
    private float sigmax = 5.0f;
    
    private JLabel sigmaYLabel;
    private JTextField sigmaYText;
    private float sigmay = 5.0f;
    
    private JCheckBox artifactCleanupCheckbox;
    private boolean doArtifactCleanup = true;
    
    private JTextField meanThresholdField;
    private float meanThreshold = 0.5f;
    
    private JTextField artifactCloseIterField;
    private int artifactCloseIter = 1;
    
    private JTextField artifactCloseSizeField;
    private float artifactCloseSize = 6f;
    
    private JTextField ventricleMeanThreshField;
    private float ventricleMeanThresh = 0.4f;
    
    private JCheckBox perfusionSymmetryRemovalCheckbox;
    private boolean doPerfusionSymmetryRemoval = true;
    
    private JTextField minPerfusionObjectSizeField;
    private int minPerfusionObjectSize = 100;
    
    private PlugInAlgorithmStrokeSegmentationPWI segAlgo = null;
    
    private boolean isDicomListenerRun = false;
    
    private StrokeSegmentationDicomReceiverPWI listenerParent;
    
    private static final String svnVersion = "$Rev: 16059 $";

    private static final String svnLastUpdate = "$Date: 2019-06-07 10:56:25 -0400 (Fri, 07 Jun 2019) $";
    
    private static final String pluginVersion = MipavUtil.getSVNChangedDate(svnLastUpdate);
    
    private static final String adcFileStub = "ADC";
    private static final String dwiFileStub = "DWI";
    private static final String pwiFileStub = "PWI";
    
    private static final String PREF_PLUGIN_STROKE_SEG_LAST_DIR = "PlugInStrokeSegLastDir";
    private static final String PREF_PLUGIN_STROKE_SEG_LAST_ADC = "PlugInStrokeSegLastADC";
    private static final String PREF_PLUGIN_STROKE_SEG_LAST_DWI = "PlugInStrokeSegLastDWI";
    
    private String lastDir = "";
    private String lastDwi = "";
    private String lastAdc = "";
    
    private boolean allowNoPWI = false;

    /**
     * Constructor used for instantiation during script execution (required for dynamic loading).
     */
    public PlugInDialogStrokeSegmentationPWI() {
        super(false);
        
        if (Preferences.isPreferenceSet(PREF_PLUGIN_STROKE_SEG_LAST_DIR)) {
            lastDir = Preferences.getProperty(PREF_PLUGIN_STROKE_SEG_LAST_DIR);
            if (lastDir == null) {
                lastDir = "";
            }
        }
        if (Preferences.isPreferenceSet(PREF_PLUGIN_STROKE_SEG_LAST_ADC)) {
            lastAdc = Preferences.getProperty(PREF_PLUGIN_STROKE_SEG_LAST_ADC);
            if (lastAdc == null) {
                lastAdc = "";
            }
        }
        if (Preferences.isPreferenceSet(PREF_PLUGIN_STROKE_SEG_LAST_DWI)) {
            lastDwi = Preferences.getProperty(PREF_PLUGIN_STROKE_SEG_LAST_DWI);
            if (lastDwi == null) {
                lastDwi = "";
            }
        }
        
        init();
    }
    
    /**
     * Constructor for DICOM catcher.
     */
    public PlugInDialogStrokeSegmentationPWI(final StrokeSegmentationDicomReceiverPWI parent, final String dicomDir, boolean noPWI) {
        super(false);

        setVisible(false);
        
        isDicomListenerRun = true;
        
        listenerParent = parent;
        
        dirFileString = dicomDir;
        
        allowNoPWI = noPWI;
        
        // set dir and find files
        findVolumesInDir(dirFileString);
        
        if (adcPath != null && !adcPath.equals("")) {
            final File adcFile = new File(adcPath);
            adcImage = openImage(adcFile, adcImageMultifile);
            
            if (adcImage == null) {
                System.err.println("Error opening ADC volume from file: " + adcPath);
                return;
            }
        } else {
            System.err.println("No ADC volume selected.");
            return;
        }
        
        if (dwiPath != null && !dwiPath.equals("")) {
            final File dwiFile = new File(dwiPath);
            dwiImage = openImage(dwiFile, dwiImageMultifile);
            
            if (dwiImage == null) {
                System.err.println("Error opening DWI volume from file: " + dwiPath);
                return;
            }
        } else {
            System.err.println("No DWI volume selected.");
            return;
        }
        
        // TODO
        if ((pwiPath != null && !pwiPath.equals("")) && !allowNoPWI) {
            final File pwiFile = new File(pwiPath);
            pwiImage = openImage(pwiFile, pwiImageMultifile);
            
            if (pwiImage == null) {
                System.err.println("Error opening PWI volume from file: " + dwiPath);
                return;
            }
        } else if (allowNoPWI) {
        	// do nothing
        } else {
            System.err.println("No PWI volume selected.");
            return;
        }
        
        if (adcImage != null && dwiImage != null && (pwiImage != null || allowNoPWI)) {
            callAlgorithm();
        }
    }
    
    /**
     * Closes dialog box when the OK button is pressed and calls the algorithm.
     *
     * @param  event  Event that triggers function.
     */
    public void actionPerformed(ActionEvent event) {
        String command = event.getActionCommand();
        Object source = event.getSource();

        if (command.equals("OK")) {
            if (setVariables()) {
                callAlgorithm();
            }
        } else if (command.equals("BrowseDir")) {
            if (browseDir()) {
                dirMethodRadio.setSelected(true);
            }
        } else if (command.equals("BrowseDWI")) {
            if (browseDWIImage()) {
                fileMethodRadio.setSelected(true);
            }
        } else if (command.equals("BrowsePWI")) {
            if (browsePWIImage()) {
                fileMethodRadio.setSelected(true);
            }
        } else if (command.equals("BrowseADC")) {
            if (browseADCImage()) {
                fileMethodRadio.setSelected(true);
            }
        } else if (command.equals("DirMethod")) {
            // nothing to do
        } else if (command.equals("FileMethod")) {
            // nothing to do
        } else if (command.equals("Cancel")) {
            this.windowClosing(new WindowEvent(this, WindowEvent.WINDOW_CLOSING));
        } else if (source == spatialSmoothingCheckBox) {
            sigmaXLabel.setEnabled(spatialSmoothingCheckBox.isSelected());
            sigmaXText.setEnabled(spatialSmoothingCheckBox.isSelected());
            sigmaYLabel.setEnabled(spatialSmoothingCheckBox.isSelected());
            sigmaYText.setEnabled(spatialSmoothingCheckBox.isSelected());
        } else {
            super.actionPerformed(event);
        }
    } 

    /**
     * This method is required if the AlgorithmPerformed interface is implemented. It is called by the algorithm when it
     * has completed or failed to to complete, so that the dialog can be display the result image and/or clean up.
     *
     * @param  algorithm  Algorithm that caused the event.
     */
    public void algorithmPerformed(AlgorithmBase algorithm) {
        PlugInAlgorithmStrokeSegmentationPWI segAlgo = (PlugInAlgorithmStrokeSegmentationPWI) algorithm;
        
        if (segAlgo instanceof PlugInAlgorithmStrokeSegmentationPWI) {
            System.err.println("Stroke segmentation Elapsed: " + segAlgo.getElapsedTime());
            
            double resFactorCC = segAlgo.getAdcResolutionFactorCC();
            double resFactorCCPWI = segAlgo.getPwiResolutionFactorCC();
            
            if (listenerParent != null) {
                listenerParent.emailReport(adcImage, segAlgo.getLightboxFileList(), segAlgo.getCoreObjectSizeTable(), segAlgo.getTmaxTables(), segAlgo.getCorrmapSegObjectSizeTable(), resFactorCC, resFactorCCPWI);
            }

            if (segAlgo.isCompleted()) {
                insertScriptLine();
            }

            if (segAlgo != null) {
                segAlgo.finalize();
                segAlgo = null;
            }
            
            // if running from a script, the script runner manages the adc/dwi image cleanup
            if (!ScriptRunner.getReference().isRunning()) {
                if (adcImage != null) {
                    adcImage.disposeLocal();
                    adcImage = null;
                }
                if (dwiImage != null) {
                    dwiImage.disposeLocal();
                    dwiImage = null;
                }
                if (pwiImage != null) {
                    pwiImage.disposeLocal();
                    pwiImage = null;
                }
            }

            if (!isDicomListenerRun && isExitRequired()) {
                System.exit(0);
            } else {
                dispose();
            }
            
            log("Segmentation algorithm complete.");
        }

    }
    
    protected boolean setVariables() {
        boolean doDirMethod = dirMethodRadio.isSelected();
        
        if (doDirMethod) {
            dirFileString = dirFileField.getText();
            
             findVolumesInDir(dirFileString);
        } else {
            adcPath = adcImageFileField.getText();
            dwiPath = dwiImageFileField.getText();
            pwiPath = pwiImageFileField.getText();
            outputDir = new File(adcPath).getParentFile().getAbsolutePath() + File.separator;
        }
        
        if (adcPath != null && !adcPath.equals("")) {
            final File adcFile = new File(adcPath);
            adcImage = openImage(adcFile, adcImageMultifile);
            
            if (adcImage == null) {
                MipavUtil.displayError("Error opening ADC volume from file: " + adcPath);
                return false;
            }
        } else {
            MipavUtil.displayError("No ADC volume selected.");
            return false;
        }
        
        
        if (dwiPath != null && !dwiPath.equals("")) {
            final File dwiFile = new File(dwiPath);
            dwiImage = openImage(dwiFile, dwiImageMultifile);
            
            if (dwiImage == null) {
                MipavUtil.displayError("Error opening DWI volume from file: " + dwiPath);
                return false;
            }
        } else {
            MipavUtil.displayError("No DWI volume selected.");
            return false;
        }
        
        // TODO
        if (pwiPath != null && !pwiPath.equals("")) {
            final File pwiFile = new File(pwiPath);
            pwiImage = openImage(pwiFile, pwiImageMultifile);
            
            if (pwiImage == null) {
                MipavUtil.displayError("Error opening PWI volume from file: " + pwiPath);
                return false;
            }
        } else {
            MipavUtil.displayError("No PWI volume selected.");
            return false;
        }
        
        adcThreshold = Integer.parseInt(adcThresholdField.getText());
        
        if (dirMethodRadio.isSelected() && !outputDir.equals("")) {
            Preferences.setProperty(PREF_PLUGIN_STROKE_SEG_LAST_DIR, outputDir);
        } else if (fileMethodRadio.isSelected()) {
            if (!dwiPath.equals("")) {
                Preferences.setProperty(PREF_PLUGIN_STROKE_SEG_LAST_DWI, dwiPath);
            }
            if (!adcPath.equals("")) {
                Preferences.setProperty(PREF_PLUGIN_STROKE_SEG_LAST_ADC, adcPath);
            }
        }
        
        doFilter = filterCheckbox.isSelected();
        
        doSymmetryRemoval = symmetryRadio.isSelected();
        
        symmetryRemovalMaxSlice = Integer.parseInt(symmetryRemovalMaxSliceField.getText());
        
        doCerebellumSkip = cerebellumRadio.isSelected();
        cerebellumSkipSliceMax = Integer.parseInt(cerebellumSliceMaxField.getText());
        
//        doCerebellumSkipAggressive = cerebellumAggressiveCheckbox.isSelected();
//        cerebellumSkipAggressiveSliceMax = Integer.parseInt(cerebellumAggressiveSliceMaxField.getText());
        
        threshCloseIter = Integer.parseInt(threshCloseIterField.getText());
        threshCloseSize = Float.parseFloat(threshCloseSizeField.getText());
        
        requireMinCoreSize = requireMinCoreSizeCheckbox.isSelected();
        minCoreSizeCC = Float.parseFloat(minCoreSizeCCField.getText());
        
        doSkullRemoval = skullRemovalCheckbox.isSelected();
        
        doSelectAdditionalObj = selectAdditionalObjCheckbox.isSelected();
        selectAdditionalObjPct = Integer.parseInt(selectAdditionalObjPctField.getText());
        
        doPwiMultithread = pwiMultithreadCheckbox.isSelected();
        doPwiCalcCBFCBVMTT = pwiCalcCBFCBVMTTCheckbox.isSelected();
        doPwiSaveOutputFiles = pwiSaveOutputFilesCheckbox.isSelected();
        
        corrmapMaskThreshold = Float.parseFloat(corrmapMaskThresholdField.getText());
        
        useTmaxInCorrmapMask = useTmaxInCorrmapMaskCheckbox.isSelected();
        
        spatialSmoothing = spatialSmoothingCheckBox.isSelected();
        if (spatialSmoothing) {
            String tmpStr = sigmaXText.getText();
            try {
                sigmax = Float.valueOf(tmpStr).floatValue();
            }
            catch (NumberFormatException e) {
                MipavUtil.displayError("sigmax text does not have a proper float");
                sigmaXText.requestFocus();
                sigmaXText.selectAll();
                return false;
            }
            if (sigmax < 0) {
                MipavUtil.displayError("sigmax must be at least 0");
                sigmaXText.requestFocus();
                sigmaXText.selectAll();
                return false;
            }
            
            tmpStr = sigmaYText.getText();
            try {
                sigmay = Float.valueOf(tmpStr).floatValue();
            }
            catch (NumberFormatException e) {
                MipavUtil.displayError("sigmay text does not have a proper float");
                sigmaYText.requestFocus();
                sigmaYText.selectAll();
                return false;
            }
            if (sigmay < 0) {
                MipavUtil.displayError("sigmay must be at least 0");
                sigmaYText.requestFocus();
                sigmaYText.selectAll();
                return false;
            }

        } // if (spatialSmoothing)
        
        doArtifactCleanup = artifactCleanupCheckbox.isSelected();
        meanThreshold = Float.parseFloat(meanThresholdField.getText());
        artifactCloseIter = Integer.parseInt(artifactCloseIterField.getText());
        artifactCloseSize = Float.parseFloat(artifactCloseSizeField.getText());
        
        doPerfusionSymmetryRemoval = perfusionSymmetryRemovalCheckbox.isSelected();
        minPerfusionObjectSize = Integer.parseInt(minPerfusionObjectSizeField.getText());
        
        ventricleMeanThresh = Float.parseFloat(ventricleMeanThreshField.getText());
        
        return true;
    }

    
    /**
     * Once all the necessary variables are set, call the stroke segmentation algorithm
     */
    protected void callAlgorithm() {

        try {
            segAlgo = new PlugInAlgorithmStrokeSegmentationPWI(dwiImage, adcImage, pwiImage, adcThreshold, doFilter, doCerebellumSkip, cerebellumSkipSliceMax, 
            		doSymmetryRemoval, symmetryRemovalMaxSlice, doSkullRemoval, threshCloseIter, threshCloseSize, doSelectAdditionalObj, selectAdditionalObjPct, 
            		requireMinCoreSize, minCoreSizeCC, outputDir, doPwiMultithread, doPwiCalcCorrMap, doPwiCalcCBFCBVMTT, doPwiSaveOutputFiles, spatialSmoothing, 
            		sigmax, sigmay, doArtifactCleanup, meanThreshold, artifactCloseSize, artifactCloseIter, doPerfusionSymmetryRemoval, minPerfusionObjectSize,
            		ventricleMeanThresh, corrmapMaskThreshold, useTmaxInCorrmapMask);

            // This is very important. Adding this object as a listener allows the algorithm to
            // notify this object when it has completed or failed. See algorithm performed event.
            // This is made possible by implementing AlgorithmedPerformed interface
            segAlgo.addListener(this);
            createProgressBar(dwiImage.getImageName(), " ...", segAlgo);

            setVisible(false); // Hide dialog

            if (isRunInSeparateThread()) {

                // Start the thread as a low priority because we wish to still
                // have user interface work fast.
                if (segAlgo.startMethod(Thread.MIN_PRIORITY) == false) {
                    MipavUtil.displayError("A thread is already running on this object");
                }
            } else {
                segAlgo.run();
            }
        } catch (OutOfMemoryError x) {
            if (dwiImage != null) {
                dwiImage.disposeLocal();
                dwiImage = null;
            }
            
            if (adcImage != null) {
                adcImage.disposeLocal();
                adcImage = null;
            }
            if (pwiImage != null) {
                pwiImage.disposeLocal();
                pwiImage = null;
            }

            MipavUtil.displayError("Stroke segmentation: unable to allocate enough memory");

            return;
        }

    }

    protected void setGUIFromParams() {
        adcImage = scriptParameters.retrieveImage("adc_image");
        dwiImage = scriptParameters.retrieveImage("dwi_image");
        
        adcThreshold = scriptParameters.getParams().getInt("adc_threshold");
        
        doFilter = scriptParameters.getParams().getBoolean("do_filter");
        
        doSymmetryRemoval = scriptParameters.getParams().getBoolean("do_symmetry_removal");
        
        symmetryRemovalMaxSlice = scriptParameters.getParams().getInt("symmetry_removal_slice_max");
        
        doCerebellumSkip = scriptParameters.getParams().getBoolean("do_cerebellum_skip");
        cerebellumSkipSliceMax = scriptParameters.getParams().getInt("cerebellum_skip_slice_max");
        
//        doCerebellumSkipAggressive = scriptParameters.getParams().getBoolean("do_cerebellum_skip_aggressive");
//        cerebellumSkipAggressiveSliceMax = scriptParameters.getParams().getInt("cerebellum_skip_aggressive_slice_max");
        
        doSkullRemoval = scriptParameters.getParams().getBoolean("do_skull_removal");
        
        threshCloseIter = scriptParameters.getParams().getInt("threshold_close_iter_num");
        threshCloseSize = scriptParameters.getParams().getInt("threshold_close_kernel_size");
        
        doSelectAdditionalObj = scriptParameters.getParams().getBoolean("do_select_additional_objs");
        selectAdditionalObjPct = scriptParameters.getParams().getInt("select_additional_objs_percent");
        
        requireMinCoreSize = scriptParameters.getParams().getBoolean("do_require_min_core_size");
        minCoreSizeCC = scriptParameters.getParams().getFloat("min_core_size_cc");
        
        doPwiMultithread = scriptParameters.getParams().getBoolean("do_pwi_multithread");
        doPwiCalcCorrMap = scriptParameters.getParams().getBoolean("do_pwi_calc_corr_map");
        doPwiCalcCBFCBVMTT = scriptParameters.getParams().getBoolean("do_pwi_calc_cbf_cbv_mtt");
        doPwiSaveOutputFiles = scriptParameters.getParams().getBoolean("do_pwi_save_output_files");
        spatialSmoothing = scriptParameters.getParams().getBoolean("do_pwi_spatial_smoothing");
        
        doArtifactCleanup = scriptParameters.getParams().getBoolean("do_pwi_artifact_cleanup");
        meanThreshold = scriptParameters.getParams().getInt("pwi_mean_thresh_val");
        artifactCloseSize = scriptParameters.getParams().getFloat("pwi_artifact_close_kernel_size");
        artifactCloseIter = scriptParameters.getParams().getInt("pwi_artifact_close_iter_num");
        
        corrmapMaskThreshold = scriptParameters.getParams().getFloat("pwi_corrmap_mask_threshold");
        
        useTmaxInCorrmapMask = scriptParameters.getParams().getBoolean("pwi_corrmap_use_tmax_mask");
        
        outputDir = adcImage.getImageDirectory() + File.separator;
    }

    protected void storeParamsFromGUI() throws ParserException {
        scriptParameters.storeImage(adcImage, "adc_image");
        scriptParameters.storeImage(dwiImage, "dwi_image");
        scriptParameters.getParams().put(ParameterFactory.newParameter("adc_threshold", adcThreshold));
        // TODO DWI Filter
        scriptParameters.getParams().put(ParameterFactory.newParameter("do_symmetry_removal", doSymmetryRemoval));
        scriptParameters.getParams().put(ParameterFactory.newParameter("symmetry_removal_slice_max", symmetryRemovalMaxSlice));
        scriptParameters.getParams().put(ParameterFactory.newParameter("do_cerebellum_skip", doCerebellumSkip));
        scriptParameters.getParams().put(ParameterFactory.newParameter("cerebellum_skip_slice_max", cerebellumSkipSliceMax));
//        scriptParameters.getParams().put(ParameterFactory.newParameter("do_cerebellum_skip_aggressive", doCerebellumSkipAggressive));
//        scriptParameters.getParams().put(ParameterFactory.newParameter("cerebellum_skip_aggressive_slice_max", cerebellumSkipAggressiveSliceMax));
        scriptParameters.getParams().put(ParameterFactory.newParameter("do_skull_removal", doSkullRemoval));
        scriptParameters.getParams().put(ParameterFactory.newParameter("threshold_close_iter_num", threshCloseIter));
        scriptParameters.getParams().put(ParameterFactory.newParameter("threshold_close_kernel_size", threshCloseSize));
        scriptParameters.getParams().put(ParameterFactory.newParameter("do_select_additional_objs", doSelectAdditionalObj));
        scriptParameters.getParams().put(ParameterFactory.newParameter("select_additional_objs_percent", selectAdditionalObjPct));
        scriptParameters.getParams().put(ParameterFactory.newParameter("do_require_min_core_size", requireMinCoreSize));
        scriptParameters.getParams().put(ParameterFactory.newParameter("min_core_size_cc", minCoreSizeCC));
        
        scriptParameters.getParams().put(ParameterFactory.newParameter("do_pwi_multithread", doPwiMultithread));
        scriptParameters.getParams().put(ParameterFactory.newParameter("do_pwi_calc_corr_map", doPwiCalcCorrMap));
        scriptParameters.getParams().put(ParameterFactory.newParameter("do_pwi_calc_cbf_cbv_mtt", doPwiCalcCBFCBVMTT));
        scriptParameters.getParams().put(ParameterFactory.newParameter("do_pwi_save_output_files", doPwiSaveOutputFiles));
        scriptParameters.getParams().put(ParameterFactory.newParameter("do_pwi_spatial_smoothing", spatialSmoothing));
        
        scriptParameters.getParams().put(ParameterFactory.newParameter("do_pwi_artifact_cleanup", doArtifactCleanup));
        scriptParameters.getParams().put(ParameterFactory.newParameter("pwi_mean_thresh_val", meanThreshold));
        scriptParameters.getParams().put(ParameterFactory.newParameter("pwi_artifact_close_kernel_size", artifactCloseSize));
        scriptParameters.getParams().put(ParameterFactory.newParameter("pwi_artifact_close_iter_num", artifactCloseIter));
        
        scriptParameters.getParams().put(ParameterFactory.newParameter("pwi_corrmap_mask_threshold", corrmapMaskThreshold));
        scriptParameters.getParams().put(ParameterFactory.newParameter("pwi_corrmap_use_tmax_mask", useTmaxInCorrmapMask));
    }
    
    /**
     * Perform any actions required after the running of the algorithm is complete.
     */
    protected void doPostAlgorithmActions() {
        // nothing to do
    }
   
    private void init() {
        setForeground(Color.black);
        setTitle("Stroke Segmentation " + pluginVersion);

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
        mainPanel.setBorder(buildTitledBorder("Input parameters"));
        
        JLabel labelThreshold = new JLabel("ADC threshold value (if fractional ADC found, divided by 1000)");
        labelThreshold.setForeground(Color.black);
        labelThreshold.setFont(serif12);
        mainPanel.add(labelThreshold, gbc);
        
        adcThresholdField = new JTextField(10);
        adcThresholdField.setText("" + adcThreshold);
        gbc.fill = GridBagConstraints.NONE;
        gbc.gridx++;
        mainPanel.add(adcThresholdField, gbc);
        
        gbc.fill = GridBagConstraints.HORIZONTAL;
        
        gbc.gridy++;
        gbc.gridx = 0;
        
        gbc.gridwidth = 3;
        
        filterCheckbox = new JCheckBox("Apply anisotropic filter to ADC volume", doFilter);
        filterCheckbox.setForeground(Color.black);
        filterCheckbox.setFont(serif12);
        mainPanel.add(filterCheckbox, gbc);
        
        gbc.gridy++;
        gbc.gridx = 0;
        
        gbc.gridwidth = 1;
        
        symmetryRadio = new JRadioButton("Perform symmetry removal on ADC mask up to slice");
        symmetryRadio.setForeground(Color.black);
        symmetryRadio.setFont(serif12);
        mainPanel.add(symmetryRadio, gbc);
        
        symmetryRemovalMaxSliceField = new JTextField(10);
        symmetryRemovalMaxSliceField.setText("" + symmetryRemovalMaxSlice);
        gbc.fill = GridBagConstraints.NONE;
        gbc.gridx++;
        mainPanel.add(symmetryRemovalMaxSliceField, gbc);
        
        gbc.gridy++;
        gbc.gridx = 0;
        
        gbc.gridwidth = 1;
        
        cerebellumRadio = new JRadioButton("Ignore thresholded ADC values up to slice");
        cerebellumRadio.setForeground(Color.black);
        cerebellumRadio.setFont(serif12);
        mainPanel.add(cerebellumRadio, gbc);
        
        ButtonGroup cerebellumGroup = new ButtonGroup();
        cerebellumGroup.add(symmetryRadio);
        cerebellumGroup.add(cerebellumRadio);
        
        if (doSymmetryRemoval) {
            symmetryRadio.setSelected(true);
        } else {
            cerebellumRadio.setSelected(true);
        }
        
        gbc.gridwidth = 1;
        
        cerebellumSliceMaxField = new JTextField(10);
        cerebellumSliceMaxField.setText("" + cerebellumSkipSliceMax);
        gbc.fill = GridBagConstraints.NONE;
        gbc.gridx++;
        mainPanel.add(cerebellumSliceMaxField, gbc);
//        
//        gbc.gridy++;
//        gbc.gridx = 0;
//        
//        gbc.gridwidth = 1;
//        
//        cerebellumAggressiveCheckbox = new JCheckBox("Perform second removal of threholded ADC values up to slice", doCerebellumSkipAggressive);
//        cerebellumAggressiveCheckbox.setForeground(Color.black);
//        cerebellumAggressiveCheckbox.setFont(serif12);
//        mainPanel.add(cerebellumAggressiveCheckbox, gbc);
//        
//        gbc.gridwidth = 1;
//        
//        cerebellumAggressiveSliceMaxField = new JTextField(10);
//        cerebellumAggressiveSliceMaxField.setText("" + cerebellumSkipAggressiveSliceMax);
//        gbc.fill = GridBagConstraints.NONE;
//        gbc.gridx++;
//        mainPanel.add(cerebellumAggressiveSliceMaxField, gbc);
        
        gbc.gridy++;
        gbc.gridx = 0;
        
        gbc.gridwidth = 3;
        
        skullRemovalCheckbox = new JCheckBox("Use DWI volume to mask ADC skull artifacts", doSkullRemoval);
        skullRemovalCheckbox.setForeground(Color.black);
        skullRemovalCheckbox.setFont(serif12);
        mainPanel.add(skullRemovalCheckbox, gbc);
        
        gbc.gridy++;
        gbc.gridx = 0;
        
        gbc.gridwidth = 1;
        
        JLabel labelThreshCloseIter = new JLabel("ADC threshold mask close iterations");
        labelThreshCloseIter.setForeground(Color.black);
        labelThreshCloseIter.setFont(serif12);
        mainPanel.add(labelThreshCloseIter, gbc);
        
        threshCloseIterField = new JTextField(10);
        threshCloseIterField.setText("" + threshCloseIter);
        gbc.fill = GridBagConstraints.NONE;
        gbc.gridx++;
        mainPanel.add(threshCloseIterField, gbc);
        
        gbc.gridy++;
        gbc.gridx = 0;
        
        JLabel labelThreshCloseSize = new JLabel("ADC threshold mask close circle diameter (mm)");
        labelThreshCloseSize.setForeground(Color.black);
        labelThreshCloseSize.setFont(serif12);
        mainPanel.add(labelThreshCloseSize, gbc);
        
        threshCloseSizeField = new JTextField(10);
        threshCloseSizeField.setText("" + threshCloseSize);
        gbc.fill = GridBagConstraints.NONE;
        gbc.gridx++;
        mainPanel.add(threshCloseSizeField, gbc);
        
        gbc.gridy++;
        gbc.gridx = 0;
        
        gbc.gridwidth = 1;
        
        selectAdditionalObjCheckbox = new JCheckBox("Select additonal objects within X% size", doSelectAdditionalObj);
        selectAdditionalObjCheckbox.setForeground(Color.black);
        selectAdditionalObjCheckbox.setFont(serif12);
        mainPanel.add(selectAdditionalObjCheckbox, gbc);
        
        selectAdditionalObjPctField = new JTextField(10);
        selectAdditionalObjPctField.setText("" + selectAdditionalObjPct);
        gbc.fill = GridBagConstraints.NONE;
        gbc.gridx++;
        mainPanel.add(selectAdditionalObjPctField, gbc);
        
        gbc.gridy++;
        gbc.gridx = 0;
        
        gbc.gridwidth = 1;
        
        requireMinCoreSizeCheckbox = new JCheckBox("Require core objects to be above minimum size (CC)", requireMinCoreSize);
        requireMinCoreSizeCheckbox.setForeground(Color.black);
        requireMinCoreSizeCheckbox.setFont(serif12);
        mainPanel.add(requireMinCoreSizeCheckbox, gbc);
        
        minCoreSizeCCField = new JTextField(10);
        minCoreSizeCCField.setText("" + minCoreSizeCC);
        gbc.fill = GridBagConstraints.NONE;
        gbc.gridx++;
        mainPanel.add(minCoreSizeCCField, gbc);
        
        gbc.gridy++;
        gbc.gridx = 0;
        
        gbc.gridwidth = 3;
        
        pwiMultithreadCheckbox = new JCheckBox("Enable PWI multi-threaded TSP processing", doPwiMultithread);
        pwiMultithreadCheckbox.setForeground(Color.black);
        pwiMultithreadCheckbox.setFont(serif12);
        mainPanel.add(pwiMultithreadCheckbox, gbc);
        
        gbc.gridy++;
        gbc.gridx = 0;
        
        gbc.gridwidth = 1;
        
        JLabel corrmapTheshLabel = new JLabel("Maximum value for corrmap segmentation starting from core region");
        corrmapTheshLabel.setForeground(Color.black);
        corrmapTheshLabel.setFont(serif12);
        mainPanel.add(corrmapTheshLabel, gbc);
        
        corrmapMaskThresholdField = new JTextField(10);
        corrmapMaskThresholdField.setText("" + corrmapMaskThreshold);
        gbc.fill = GridBagConstraints.NONE;
        gbc.gridx++;
        mainPanel.add(corrmapMaskThresholdField, gbc);
        
        gbc.gridy++;
        gbc.gridx = 0;
        
        gbc.gridwidth = 3;
        
        useTmaxInCorrmapMaskCheckbox = new JCheckBox("Use Tmax values to limit Corrmap segementation", useTmaxInCorrmapMask);
        useTmaxInCorrmapMaskCheckbox.setForeground(Color.black);
        useTmaxInCorrmapMaskCheckbox.setFont(serif12);
        mainPanel.add(useTmaxInCorrmapMaskCheckbox, gbc);
        
        gbc.gridy++;
        gbc.gridx = 0;
        
        gbc.gridwidth = 3;
        
        pwiCalcCBFCBVMTTCheckbox = new JCheckBox("Calculate PWI CBF, CBV, and MTT", doPwiCalcCBFCBVMTT);
        pwiCalcCBFCBVMTTCheckbox.setForeground(Color.black);
        pwiCalcCBFCBVMTTCheckbox.setFont(serif12);
        mainPanel.add(pwiCalcCBFCBVMTTCheckbox, gbc);
        
        gbc.gridy++;
        gbc.gridx = 0;
        
        gbc.gridwidth = 3;
        
        pwiSaveOutputFilesCheckbox = new JCheckBox("Save all PWI TSP algorithm output volumes", doPwiSaveOutputFiles);
        pwiSaveOutputFilesCheckbox.setForeground(Color.black);
        pwiSaveOutputFilesCheckbox.setFont(serif12);
        mainPanel.add(pwiSaveOutputFilesCheckbox, gbc);
        
        gbc.gridx = 0;
        gbc.gridy++;
        spatialSmoothingCheckBox = new JCheckBox("Perform PWI spatial smoothing", spatialSmoothing);
        spatialSmoothingCheckBox.setFont(MipavUtil.font12);
        spatialSmoothingCheckBox.setForeground(Color.black);
        spatialSmoothingCheckBox.addActionListener(this);
        mainPanel.add(spatialSmoothingCheckBox, gbc);
        
        gbc.gridy++;
        sigmaXLabel = new JLabel("PWI Gaussian blur sigma X in millimeters");
        sigmaXLabel.setFont(serif12);
        sigmaXLabel.setForeground(Color.black);
        sigmaXLabel.setEnabled(spatialSmoothing);
        mainPanel.add(sigmaXLabel, gbc);
        
        gbc.gridx++;
        sigmaXText = new JTextField(10);
        sigmaXText.setText("" + sigmax);
        sigmaXText.setFont(serif12);
        sigmaXText.setForeground(Color.black);
        sigmaXText.setEnabled(spatialSmoothing);
        mainPanel.add(sigmaXText, gbc);
        
        gbc.gridx = 0;
        gbc.gridy++;
        sigmaYLabel = new JLabel("PWI Gaussian blur sigma Y in millimeters");
        sigmaYLabel.setFont(serif12);
        sigmaYLabel.setForeground(Color.black);
        sigmaYLabel.setEnabled(spatialSmoothing);
        mainPanel.add(sigmaYLabel, gbc);
        
        gbc.gridx = 1;
        sigmaYText = new JTextField(10);
        sigmaYText.setText("" + sigmay);
        sigmaYText.setFont(serif12);
        sigmaYText.setForeground(Color.black);
        sigmaYText.setEnabled(spatialSmoothing);
        mainPanel.add(sigmaYText, gbc);
        
        gbc.gridy++;
        gbc.gridx = 0;
        
        gbc.gridwidth = 3;
        
        artifactCleanupCheckbox = new JCheckBox("Clean up PWI artifacts using mean threshold", doArtifactCleanup);
        artifactCleanupCheckbox.setForeground(Color.black);
        artifactCleanupCheckbox.setFont(serif12);
        mainPanel.add(artifactCleanupCheckbox, gbc);
        
        gbc.gridy++;
        gbc.gridx = 0;
        
        gbc.gridwidth = 1;
        
        JLabel labelMeanThreshold = new JLabel("Artifact mean threshold ratio");
        labelMeanThreshold.setForeground(Color.black);
        labelMeanThreshold.setFont(serif12);
        mainPanel.add(labelMeanThreshold, gbc);
        
        meanThresholdField = new JTextField(10);
        meanThresholdField.setText("" + meanThreshold);
        gbc.fill = GridBagConstraints.NONE;
        gbc.gridx++;
        mainPanel.add(meanThresholdField, gbc);
        
        gbc.gridy++;
        gbc.gridx = 0;
        
        JLabel labelArtifactCloseSize = new JLabel("Artifact mask closing circle diameter (mm)");
        labelArtifactCloseSize.setForeground(Color.black);
        labelArtifactCloseSize.setFont(serif12);
        mainPanel.add(labelArtifactCloseSize, gbc);
        
        artifactCloseSizeField = new JTextField(10);
        artifactCloseSizeField.setText("" + artifactCloseSize);
        gbc.fill = GridBagConstraints.NONE;
        gbc.gridx++;
        mainPanel.add(artifactCloseSizeField, gbc);
        
        gbc.gridy++;
        gbc.gridx = 0;
        
        gbc.gridwidth = 1;
        
        JLabel labelArtifactCloseIter = new JLabel("Artifact mask close iterations");
        labelArtifactCloseIter.setForeground(Color.black);
        labelArtifactCloseIter.setFont(serif12);
        mainPanel.add(labelArtifactCloseIter, gbc);
        
        artifactCloseIterField = new JTextField(10);
        artifactCloseIterField.setText("" + artifactCloseIter);
        gbc.fill = GridBagConstraints.NONE;
        gbc.gridx++;
        mainPanel.add(artifactCloseIterField, gbc);
        
        gbc.gridy++;
        gbc.gridx = 0;
        
        gbc.gridwidth = 3;
        
        perfusionSymmetryRemovalCheckbox = new JCheckBox("Perform symmetry removal on perfusion mask", doPerfusionSymmetryRemoval);
        perfusionSymmetryRemovalCheckbox.setForeground(Color.black);
        perfusionSymmetryRemovalCheckbox.setFont(serif12);
        mainPanel.add(perfusionSymmetryRemovalCheckbox, gbc);
        
        gbc.gridy++;
        gbc.gridx = 0;
        
        gbc.gridwidth = 1;
        
        JLabel labelPerfSize = new JLabel("Minimum size of perfusion mask objects");
        labelPerfSize.setForeground(Color.black);
        labelPerfSize.setFont(serif12);
        mainPanel.add(labelPerfSize, gbc);
        
        minPerfusionObjectSizeField = new JTextField(10);
        minPerfusionObjectSizeField.setText("" + minPerfusionObjectSize);
        gbc.fill = GridBagConstraints.NONE;
        gbc.gridx++;
        mainPanel.add(minPerfusionObjectSizeField, gbc);
        
        gbc.gridy++;
        gbc.gridx = 0;
        
        gbc.gridwidth = 1;
        
        JLabel labelVentricleThreshold = new JLabel("Ventricle mask DWI intensity threshold");
        labelVentricleThreshold.setForeground(Color.black);
        labelVentricleThreshold.setFont(serif12);
        mainPanel.add(labelVentricleThreshold, gbc);
        
        ventricleMeanThreshField = new JTextField(10);
        ventricleMeanThreshField.setText("" + ventricleMeanThresh);
        gbc.fill = GridBagConstraints.NONE;
        gbc.gridx++;
        mainPanel.add(ventricleMeanThreshField, gbc);
        
        gbc.gridy++;
        gbc.gridx = 0;
        
        gbc.gridwidth = 3;
        
        dirMethodRadio = new JRadioButton("Select directory with ADC, DWI, and PWI files/subdirectories");
        dirMethodRadio.setForeground(Color.black);
        dirMethodRadio.setFont(serif12);
        dirMethodRadio.setActionCommand("DirMethod");
        dirMethodRadio.addActionListener(this);
        mainPanel.add(dirMethodRadio, gbc);
        
        gbc.gridwidth = 1;
        gbc.gridy++;
        gbc.gridx = 0;
        
        JLabel labelDir = new JLabel("Parent directory");
        labelDir.setForeground(Color.black);
        labelDir.setFont(serif12);
        mainPanel.add(labelDir, gbc);
        
        dirFileField = new JTextField(50);
        //dirFileField.setText(lastDir);
        gbc.gridx++;
        mainPanel.add(dirFileField, gbc);
        
        JButton dirFileButton = new JButton("Browse");
        dirFileButton.setActionCommand("BrowseDir");
        dirFileButton.addActionListener(this);
        dirFileButton.setForeground(Color.black);
        dirFileButton.setFont(serif12B);
        gbc.gridx++;
        mainPanel.add(dirFileButton, gbc);
        
        gbc.gridy++;
        gbc.gridx = 0;
        
        gbc.gridwidth = 3;
        
        fileMethodRadio = new JRadioButton("Select individual ADC and DWI files");
        fileMethodRadio.setForeground(Color.black);
        fileMethodRadio.setFont(serif12);
        fileMethodRadio.setActionCommand("FileMethod");
        fileMethodRadio.addActionListener(this);
        mainPanel.add(fileMethodRadio, gbc);
        
        ButtonGroup methodGroup = new ButtonGroup();
        methodGroup.add(dirMethodRadio);
        methodGroup.add(fileMethodRadio);
        
        // default to dir method, unless a previous value is set for the dwi/adc files but not the dir field
        if (lastDir.equals("") && (!lastDwi.equals("") || !lastAdc.equals(""))) {
            fileMethodRadio.setSelected(true);
        } else {
            dirMethodRadio.setSelected(true);
        }
        
        gbc.gridwidth = 1;
        
        gbc.gridy++;
        gbc.gridx = 0;

        JLabel labelDWI = new JLabel("DWI image");
        labelDWI.setForeground(Color.black);
        labelDWI.setFont(serif12);
        mainPanel.add(labelDWI, gbc);
        
        dwiImageFileField = new JTextField(50);
        //dwiImageFileField.setText(lastDwi);
        gbc.gridx++;
        mainPanel.add(dwiImageFileField, gbc);
        
        JButton dwiImageFileButton = new JButton("Browse");
        dwiImageFileButton.setActionCommand("BrowseDWI");
        dwiImageFileButton.addActionListener(this);
        dwiImageFileButton.setForeground(Color.black);
        dwiImageFileButton.setFont(serif12B);
        gbc.gridx++;
        mainPanel.add(dwiImageFileButton, gbc);
        
        gbc.gridy++;
        gbc.gridx = 0;
        
        JLabel labelADC = new JLabel("ADC image");
        labelADC.setForeground(Color.black);
        labelADC.setFont(serif12);
        mainPanel.add(labelADC, gbc);
        
        adcImageFileField = new JTextField(50);
        //adcImageFileField.setText(lastAdc);
        gbc.gridx++;
        mainPanel.add(adcImageFileField, gbc);
        
        JButton adcImageFileButton = new JButton("Browse");
        adcImageFileButton.setActionCommand("BrowseADC");
        adcImageFileButton.addActionListener(this);
        adcImageFileButton.setForeground(Color.black);
        adcImageFileButton.setFont(serif12B);
        gbc.gridx++;
        mainPanel.add(adcImageFileButton, gbc);
        
        gbc.gridy++;
        gbc.gridx = 0;
        
        JLabel labelPWI = new JLabel("PWI scan");
        labelPWI.setForeground(Color.black);
        labelPWI.setFont(serif12);
        mainPanel.add(labelPWI, gbc);
        
        pwiImageFileField = new JTextField(50);
        //pwiImageFileField.setText(lastAdc);
        gbc.gridx++;
        mainPanel.add(pwiImageFileField, gbc);
        
        JButton pwiImageFileButton = new JButton("Browse");
        pwiImageFileButton.setActionCommand("BrowsePWI");
        pwiImageFileButton.addActionListener(this);
        pwiImageFileButton.setForeground(Color.black);
        pwiImageFileButton.setFont(serif12B);
        gbc.gridx++;
        mainPanel.add(pwiImageFileButton, gbc);

        getContentPane().add(mainPanel, BorderLayout.CENTER);

        // Build the Panel that holds the OK and CANCEL Buttons
        JPanel OKCancelPanel = new JPanel();

        // size and place the OK button
        buildOKButton();
        OKCancelPanel.add(OKButton, BorderLayout.WEST);

        // size and place the CANCEL button
        buildCancelButton();
        OKCancelPanel.add(cancelButton, BorderLayout.EAST);
        getContentPane().add(OKCancelPanel, BorderLayout.SOUTH);

        pack();
        setVisible(true);
        setResizable(true);
        System.gc();

    } // end init()
    
    private boolean browseDir() {
        String initDir = dirFileField.getText();
        if (initDir.equals("") || !(new File(initDir).exists())) {
            initDir = ViewUserInterface.getReference().getDefaultDirectory();
        }
        
        final JFileChooser chooser = new JFileChooser(initDir);

        chooser.setFileSelectionMode(JFileChooser.DIRECTORIES_ONLY);
        chooser.setDialogTitle("Choose directory containing ADC and DWI volumes");
        final int returnValue = chooser.showOpenDialog(this);
        if (returnValue == JFileChooser.APPROVE_OPTION) {
            dirFileField.setText(chooser.getSelectedFile().getAbsolutePath() + File.separator);

            dirFileString = chooser.getSelectedFile().getAbsolutePath() + File.separator;
            
            return true;
        }
        
        return false;
    }
    
    private boolean browseADCImage() {
        final ViewFileChooserBase fileChooser = new ViewFileChooserBase(true, false);
        fileChooser.setMulti(ViewUserInterface.getReference().getLastStackFlag());

        final JFileChooser chooser = fileChooser.getFileChooser();
        chooser.setCurrentDirectory(new File(ViewUserInterface.getReference().getDefaultDirectory()));

        // default to TECH filter
        int filter = ViewImageFileFilter.TECH;

        try {
            filter = Integer.parseInt(Preferences.getProperty(Preferences.PREF_FILENAME_FILTER));
        } catch (final NumberFormatException nfe) {

            // an invalid value was set in preferences -- so don't
            // use it!
            filter = -1;
        }

        chooser.addChoosableFileFilter(new ViewImageFileFilter(ViewImageFileFilter.GEN));
        chooser.addChoosableFileFilter(new ViewImageFileFilter(ViewImageFileFilter.TECH));
        chooser.addChoosableFileFilter(new ViewImageFileFilter(ViewImageFileFilter.MICROSCOPY));
        chooser.addChoosableFileFilter(new ViewImageFileFilter(ViewImageFileFilter.MISC));

        if (filter != -1) {
            // it seems that the set command adds the filter
            // again...
            // chooser.addChoosableFileFilter(new
            // ViewImageFileFilter(filter));

            // if filter is something we already added, then remove
            // it before
            // setting it..... (kludgy, kludgy....)
            final javax.swing.filechooser.FileFilter found = ViewOpenFileUI.findFilter(chooser, filter);

            if (found != null) {
                chooser.removeChoosableFileFilter(found);
            }

            // initially set to the preferences
            chooser.setFileFilter(new ViewImageFileFilter(filter));
        }

        final int returnVal = chooser.showOpenDialog(this);

        if (returnVal == JFileChooser.APPROVE_OPTION) {
            boolean isMultiFile = fileChooser.isMulti();

            final File file = chooser.getSelectedFile();
            ViewUserInterface.getReference().setDefaultDirectory(file.getParent());
            
            adcImageFileField.setText(file.getAbsolutePath());
            adcImageMultifile = isMultiFile;

            return true;
        }
        
        return false;
    }
    
    private boolean browseDWIImage() {
        final ViewFileChooserBase fileChooser = new ViewFileChooserBase(true, false);
        fileChooser.setMulti(ViewUserInterface.getReference().getLastStackFlag());

        final JFileChooser chooser = fileChooser.getFileChooser();
        chooser.setCurrentDirectory(new File(ViewUserInterface.getReference().getDefaultDirectory()));

        // default to TECH filter
        int filter = ViewImageFileFilter.TECH;

        try {
            filter = Integer.parseInt(Preferences.getProperty(Preferences.PREF_FILENAME_FILTER));
        } catch (final NumberFormatException nfe) {

            // an invalid value was set in preferences -- so don't
            // use it!
            filter = -1;
        }

        chooser.addChoosableFileFilter(new ViewImageFileFilter(ViewImageFileFilter.GEN));
        chooser.addChoosableFileFilter(new ViewImageFileFilter(ViewImageFileFilter.TECH));
        chooser.addChoosableFileFilter(new ViewImageFileFilter(ViewImageFileFilter.MICROSCOPY));
        chooser.addChoosableFileFilter(new ViewImageFileFilter(ViewImageFileFilter.MISC));

        if (filter != -1) {
            // it seems that the set command adds the filter
            // again...
            // chooser.addChoosableFileFilter(new
            // ViewImageFileFilter(filter));

            // if filter is something we already added, then remove
            // it before
            // setting it..... (kludgy, kludgy....)
            final javax.swing.filechooser.FileFilter found = ViewOpenFileUI.findFilter(chooser, filter);

            if (found != null) {
                chooser.removeChoosableFileFilter(found);
            }

            // initially set to the preferences
            chooser.setFileFilter(new ViewImageFileFilter(filter));
        }

        final int returnVal = chooser.showOpenDialog(this);

        if (returnVal == JFileChooser.APPROVE_OPTION) {
            boolean isMultiFile = fileChooser.isMulti();

            final File file = chooser.getSelectedFile();
            ViewUserInterface.getReference().setDefaultDirectory(file.getParent());
            
            dwiImageFileField.setText(file.getAbsolutePath());
            dwiImageMultifile = isMultiFile;

            return true;
        }
        
        return false;
    }
    
    private boolean browsePWIImage() {
        final ViewFileChooserBase fileChooser = new ViewFileChooserBase(true, false);
        fileChooser.setMulti(ViewUserInterface.getReference().getLastStackFlag());

        final JFileChooser chooser = fileChooser.getFileChooser();
        chooser.setCurrentDirectory(new File(ViewUserInterface.getReference().getDefaultDirectory()));

        // default to TECH filter
        int filter = ViewImageFileFilter.TECH;

        try {
            filter = Integer.parseInt(Preferences.getProperty(Preferences.PREF_FILENAME_FILTER));
        } catch (final NumberFormatException nfe) {

            // an invalid value was set in preferences -- so don't
            // use it!
            filter = -1;
        }

        chooser.addChoosableFileFilter(new ViewImageFileFilter(ViewImageFileFilter.GEN));
        chooser.addChoosableFileFilter(new ViewImageFileFilter(ViewImageFileFilter.TECH));
        chooser.addChoosableFileFilter(new ViewImageFileFilter(ViewImageFileFilter.MICROSCOPY));
        chooser.addChoosableFileFilter(new ViewImageFileFilter(ViewImageFileFilter.MISC));

        if (filter != -1) {
            // it seems that the set command adds the filter
            // again...
            // chooser.addChoosableFileFilter(new
            // ViewImageFileFilter(filter));

            // if filter is something we already added, then remove
            // it before
            // setting it..... (kludgy, kludgy....)
            final javax.swing.filechooser.FileFilter found = ViewOpenFileUI.findFilter(chooser, filter);

            if (found != null) {
                chooser.removeChoosableFileFilter(found);
            }

            // initially set to the preferences
            chooser.setFileFilter(new ViewImageFileFilter(filter));
        }

        final int returnVal = chooser.showOpenDialog(this);

        if (returnVal == JFileChooser.APPROVE_OPTION) {
            boolean isMultiFile = fileChooser.isMulti();

            final File file = chooser.getSelectedFile();
            ViewUserInterface.getReference().setDefaultDirectory(file.getParent());
            
            pwiImageFileField.setText(file.getAbsolutePath());
            pwiImageMultifile = isMultiFile;

            return true;
        }
        
        return false;
    }
    
    private ModelImage openImage(final File file, final boolean isMultiFile) {
        final FileIO fileIO = new FileIO();
        fileIO.setQuiet(true);

        return fileIO.readImage(file.getName(), file.getParent() + File.separator, isMultiFile, null);
    }
    
    private boolean findVolumesInDir(final String dir) {
        final File dirFile = new File(dir);
        
        File[] dirContents = dirFile.listFiles();
        
        for (File file : dirContents) {
            String nameNoExt = FileUtility.stripExtension(file.getName());
            if (nameNoExt.equalsIgnoreCase(adcFileStub)) {
                if (file.isDirectory()) {
                    File[] subContents = file.listFiles();
                    if (subContents.length > 0) {
                        adcPath = subContents[0].getAbsolutePath();
                        adcImageMultifile = true;
                        foundADC = true;
                    }
                } else {
                    adcPath = file.getAbsolutePath();
                    foundADC = true;
                }
            } else if (nameNoExt.equalsIgnoreCase(dwiFileStub)) {
                if (file.isDirectory()) {
                    File[] subContents = file.listFiles();
                    if (subContents.length > 0) {
                        dwiPath = subContents[0].getAbsolutePath();
                        dwiImageMultifile = true;
                        foundDWI = true;
                    }
                } else {
                    dwiPath = file.getAbsolutePath();
                    foundDWI = true;
                }
            } else if (nameNoExt.equalsIgnoreCase(pwiFileStub)) {
                if (file.isDirectory()) {
                    File[] subContents = file.listFiles();
                    if (subContents.length > 0) {
                        pwiPath = subContents[0].getAbsolutePath();
                        pwiImageMultifile = true;
                        foundPWI = true;
                    }
                } else {
                    pwiPath = file.getAbsolutePath();
                    foundPWI = true;
                }
            }
        }

        if (!foundADC || !foundDWI || !foundPWI) {
            findDicomFilesRecursive(dirFile);
        }
        
        if (!foundADC) {
            MipavUtil.displayError("No ADC files found in directory: " + dir);
            return false;
        }
        
        if (!foundDWI) {
            MipavUtil.displayError("No DWI files found in directory: " + dir);
            return false;
        }
        
        if (!foundPWI && !allowNoPWI) {
            MipavUtil.displayError("No PWI files found in directory: " + dir);
            return false;
        } else if (!foundPWI && allowNoPWI) {
        	System.err.println("No PWI files found - running DWI/ADC coretool only: " + dir);
        }
        
        outputDir = dirFile.getAbsolutePath() + File.separator;
        
        return true;
    }
    
    private void findDicomFilesRecursive(final File dirFile) {
        
        File[] dirContents = dirFile.listFiles();
        
        boolean checkedFirstFile = false;
        for (File file : dirContents) {
            // skip previously generated files
            if (file.getName().equalsIgnoreCase("core_seg_report.html")) {
                continue;
            }
            
            if (file.isDirectory()) {
                findDicomFilesRecursive(file);
            } else {
                if (!checkedFirstFile && !file.getName().contains(PlugInAlgorithmStrokeSegmentationPWI.outputLabel)) {
                    checkedFirstFile = true;
                    ModelImage img = openImage(file, false);
                    if (img != null) {
                        if (img.getFileInfo(0) instanceof FileInfoDicom) {
                            String imageType = (String)((FileInfoDicom)img.getFileInfo(0)).getTagTable().getValue("0008,0008");
                            String seriesDesc = (String)((FileInfoDicom)img.getFileInfo(0)).getTagTable().getValue("0008,103E");
                            String protocolName = (String)((FileInfoDicom)img.getFileInfo(0)).getTagTable().getValue("0018,1030");
                            
                            for (String val : imageType.split("\\\\")) {
                                if (isADC(val)) {
                                    // some studies are transmitted with both 'reg' and unmodified versions of the data
                                    // choose the 'reg' version if we previously found a different ADC series 
                                    if (foundADC && isRegisteredVol(seriesDesc, protocolName)) {
                                        adcPath = file.getAbsolutePath();
                                        adcImageMultifile = true;
                                        foundADC = true;
                                        break;
                                    } else if (!foundADC) {
                                        // new ADC volume, and we hadn't encountered one before
                                        adcPath = file.getAbsolutePath();
                                        adcImageMultifile = true;
                                        foundADC = true;
                                        break;
                                    }
                                } else if (isDWI(val)) {
                                    // some studies are transmitted with both 'reg' and unmodified versions of the data
                                    // choose the 'reg' version if we previously found a different ADC series 
                                    if (foundDWI && isRegisteredVol(seriesDesc, protocolName)) {
                                        dwiPath = file.getAbsolutePath();
                                        dwiImageMultifile = true;
                                        foundDWI = true;
                                        break;
                                    } else if (!foundDWI) {
                                        // new ADC volume, and we hadn't encountered one before
                                        dwiPath = file.getAbsolutePath();
                                        dwiImageMultifile = true;
                                        foundDWI = true;
                                        break;
                                    }
                                } else if (isPWI(val)) {
                                    // some studies are transmitted with both 'reg' and unmodified versions of the data
                                    // choose the 'reg' version if we previously found a different PWI series 
                                    if (foundPWI && isRegisteredVol(seriesDesc, protocolName)) {
                                        pwiPath = file.getAbsolutePath();
                                        pwiImageMultifile = true;
                                        foundPWI = true;
                                        break;
                                    } else if (!foundPWI) {
                                        // new PWI volume, and we hadn't encountered one before
                                        pwiPath = file.getAbsolutePath();
                                        pwiImageMultifile = true;
                                        foundPWI = true;
                                        break;
                                    }
                                }
                            }
                        }
                        
                        img.disposeLocal();
                        img = null;
                    }
                }
            }
        }
    }
    
    /**
     * Append a line to the log output area in the DICOM listener Log tab.
     * 
     * @param line The line to append (do not include the trailing newline).
     */
    public void log(final String line) {
        if (listenerParent != null) {
            listenerParent.log(line);
        }
    }
    
    public static final boolean isRegisteredVol(final String seriesDesc, final String protocolName) {
        return ((seriesDesc != null && seriesDesc.toLowerCase().contains("reg")) || (protocolName != null && protocolName.toLowerCase().contains("reg")));
    }
    
    public static final boolean isADC(final String imgType) {
        return (imgType != null && (imgType.equalsIgnoreCase("Average DC") || imgType.equalsIgnoreCase("ADC") || imgType.equalsIgnoreCase("ADC_UNSPECIFIED")));
    }
    
    public static final boolean isDWI(final String imgType) {
        return (imgType != null && (imgType.equalsIgnoreCase("SE") || imgType.equalsIgnoreCase("M_SE") || imgType.equalsIgnoreCase("TRACEW")));
    }
    
    public static final boolean isPWI(final String imgType) {
        return (imgType != null && (imgType.equalsIgnoreCase("FFE") || imgType.equalsIgnoreCase("M_FFE") || imgType.equalsIgnoreCase("PERFUSION")));
    }
}
