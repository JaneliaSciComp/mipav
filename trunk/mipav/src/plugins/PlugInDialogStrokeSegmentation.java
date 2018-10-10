import gov.nih.mipav.plugins.JDialogStandaloneScriptablePlugin;

import gov.nih.mipav.model.algorithms.*;
import gov.nih.mipav.model.file.*;
import gov.nih.mipav.model.file.FileInfoBase.Unit;
import gov.nih.mipav.model.scripting.*;
import gov.nih.mipav.model.scripting.parameters.ParameterFactory;
import gov.nih.mipav.model.structures.*;

import gov.nih.mipav.view.*;

import java.awt.*;
import java.awt.event.ActionEvent;
import java.awt.event.WindowEvent;
import java.io.File;

import javax.swing.*;


public class PlugInDialogStrokeSegmentation extends JDialogStandaloneScriptablePlugin implements AlgorithmInterface {
    private static final long serialVersionUID = -8203006546174953787L;
    
    private JRadioButton dirMethodRadio;
    private JRadioButton fileMethodRadio;
    
    private JTextField dirFileField;
    
    private String dirFileString;
    
    private String adcPath = null;
    private String dwiPath = null;
    
    private boolean foundADC = false;
    private boolean foundDWI = false;
    
    private String outputDir;
    
    private JTextField adcImageFileField;
    private JTextField dwiImageFileField;
    
    private JTextField adcThresholdField;
    
    private JCheckBox symmetryCheckbox;
    
    private JCheckBox cerebellumCheckbox;
    private JTextField cerebellumSliceMaxField;
    
    private JCheckBox skullRemovalCheckbox;

    private boolean adcImageMultifile = false;
    private ModelImage adcImage;
    
    private boolean dwiImageMultifile = false;
    private ModelImage dwiImage;
    
    private int adcThreshold = 620;
    
    private boolean doSymmetryRemoval = true;
    
    private boolean doCerebellumSkip = true;
    
    private int cerebellumSkipSliceMax = 7;
    
    private boolean doSkullRemoval = true;
    
    private JTextField threshCloseIterField;
    
    private int threshCloseIter = 1;
    
    private JTextField threshCloseSizeField;
    
    private float threshCloseSize = 4f;
    
    private PlugInAlgorithmStrokeSegmentation segAlgo = null;
    
    private boolean isDicomListenerRun = false;
    
    private StrokeSegmentationDicomReceiver listenerParent;
    
    private static final String svnVersion = "$Rev$";

    private static final String svnLastUpdate = "$Date$";
    
    private static final String pluginVersion = MipavUtil.getSVNChangedDate(svnLastUpdate);
    
    private static final String adcFileStub = "Baseline_ADC";
    private static final String dwiFileStub = "Baseline_DWI";
    
    private static final String PREF_PLUGIN_STROKE_SEG_LAST_DIR = "PlugInStrokeSegLastDir";
    private static final String PREF_PLUGIN_STROKE_SEG_LAST_ADC = "PlugInStrokeSegLastADC";
    private static final String PREF_PLUGIN_STROKE_SEG_LAST_DWI = "PlugInStrokeSegLastDWI";
    
    private String lastDir = "";
    private String lastDwi = "";
    private String lastAdc = "";

    /**
     * Constructor used for instantiation during script execution (required for dynamic loading).
     */
    public PlugInDialogStrokeSegmentation() {
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
    public PlugInDialogStrokeSegmentation(final StrokeSegmentationDicomReceiver parent, final String dicomDir) {
        super(false);

        setVisible(false);
        
        isDicomListenerRun = true;
        
        listenerParent = parent;
        
        dirFileString = dicomDir;
        
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
        
        // default values
        adcThreshold = 620;
        doSymmetryRemoval = true;
        doCerebellumSkip = true;
        doSkullRemoval = true;
        cerebellumSkipSliceMax = 7;
        
        if (adcImage != null && dwiImage != null) {
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
        PlugInAlgorithmStrokeSegmentation segAlgo = (PlugInAlgorithmStrokeSegmentation) algorithm;
        
        if (segAlgo instanceof PlugInAlgorithmStrokeSegmentation) {
            Preferences.debug("Stroke segmentation Elapsed: " + segAlgo.getElapsedTime());
            
            float[] resol = adcImage.getResolutions(0);
            int[] units = adcImage.getUnitsOfMeasure();
            
            Unit unit = Unit.getUnitFromLegacyNum(units[0]);
            double[] resolCC = new double[resol.length];
            for (int i = 0; i < resol.length; i++) {
                resolCC[i] = unit.convertTo(resol[i], Unit.CENTIMETERS);
            }
            
            double coreVolCC = segAlgo.getCoreSize() * resolCC[0] * resolCC[1] * resolCC[2];
            
            if (listenerParent != null) {
                listenerParent.emailReport(adcImage, segAlgo.getThreshLightboxFile(), segAlgo.getCoreLightboxFile(), coreVolCC);
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
        
        doSymmetryRemoval = symmetryCheckbox.isSelected();
        
        doCerebellumSkip = cerebellumCheckbox.isSelected();
        cerebellumSkipSliceMax = Integer.parseInt(cerebellumSliceMaxField.getText());
        
        threshCloseIter = Integer.parseInt(threshCloseIterField.getText());
        threshCloseSize = Float.parseFloat(threshCloseSizeField.getText());
        
        doSkullRemoval = skullRemovalCheckbox.isSelected();
        
        return true;
    }

    
    /**
     * Once all the necessary variables are set, call the stroke segmentation algorithm
     */
    protected void callAlgorithm() {

        try {
            segAlgo = new PlugInAlgorithmStrokeSegmentation(dwiImage, adcImage, adcThreshold, doSymmetryRemoval, doCerebellumSkip, cerebellumSkipSliceMax, doSkullRemoval, threshCloseIter, threshCloseSize, outputDir);

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

            MipavUtil.displayError("Stroke segmentation: unable to allocate enough memory");

            return;
        }

    }

    protected void setGUIFromParams() {
        adcImage = scriptParameters.retrieveImage("adc_image");
        dwiImage = scriptParameters.retrieveImage("dwi_image");
        
        adcThreshold = scriptParameters.getParams().getInt("adc_threshold");
        
        doSymmetryRemoval = scriptParameters.getParams().getBoolean("do_symmetry_removal");
        
        doCerebellumSkip = scriptParameters.getParams().getBoolean("do_cerebellum_skip");
        cerebellumSkipSliceMax = scriptParameters.getParams().getInt("cerebellum_skip_slice_max");
        
        doSkullRemoval = scriptParameters.getParams().getBoolean("do_skull_removal");
        
        threshCloseIter = scriptParameters.getParams().getInt("threshold_close_iter_num");
        threshCloseSize = scriptParameters.getParams().getInt("threshold_close_kernel_size");
        
        outputDir = adcImage.getImageDirectory() + File.separator;
    }

    protected void storeParamsFromGUI() throws ParserException {
        scriptParameters.storeImage(adcImage, "adc_image");
        scriptParameters.storeImage(dwiImage, "dwi_image");
        scriptParameters.getParams().put(ParameterFactory.newParameter("adc_threshold", adcThreshold));
        scriptParameters.getParams().put(ParameterFactory.newParameter("do_symmetry_removal", doSymmetryRemoval));
        scriptParameters.getParams().put(ParameterFactory.newParameter("do_cerebellum_skip", doCerebellumSkip));
        scriptParameters.getParams().put(ParameterFactory.newParameter("cerebellum_skip_slice_max", cerebellumSkipSliceMax));
        scriptParameters.getParams().put(ParameterFactory.newParameter("do_skull_removal", doSkullRemoval));
        scriptParameters.getParams().put(ParameterFactory.newParameter("threshold_close_iter_num", threshCloseIter));
        scriptParameters.getParams().put(ParameterFactory.newParameter("threshold_close_kernel_size", threshCloseSize));
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
        
        JLabel labelThreshold = new JLabel("ADC threshold value");
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
        
        symmetryCheckbox = new JCheckBox("Perform symmetry removal on ADC mask", doSymmetryRemoval);
        symmetryCheckbox.setForeground(Color.black);
        symmetryCheckbox.setFont(serif12);
        mainPanel.add(symmetryCheckbox, gbc);
        
        gbc.gridy++;
        gbc.gridx = 0;
        
        gbc.gridwidth = 1;
        
        cerebellumCheckbox = new JCheckBox("Ignore thresholded ADC values up to slice", doCerebellumSkip);
        cerebellumCheckbox.setForeground(Color.black);
        cerebellumCheckbox.setFont(serif12);
        mainPanel.add(cerebellumCheckbox, gbc);
        
        gbc.gridwidth = 1;
        
        cerebellumSliceMaxField = new JTextField(10);
        cerebellumSliceMaxField.setText("" + cerebellumSkipSliceMax);
        gbc.fill = GridBagConstraints.NONE;
        gbc.gridx++;
        mainPanel.add(cerebellumSliceMaxField, gbc);
        
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
        
        gbc.gridwidth = 3;
        
        dirMethodRadio = new JRadioButton("Select directory with Baseline_ADC and Baseline_DWI files/subdirectories");
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
                } else if (file.getName().endsWith(".img")) {
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
                } else if (file.getName().endsWith(".img")) {
                    dwiPath = file.getAbsolutePath();
                    foundDWI = true;
                }
            }
        }

        if (!foundADC || !foundDWI) {
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
                if (!checkedFirstFile && !file.getName().contains(PlugInAlgorithmStrokeSegmentation.outputLabel)) {
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
        return (seriesDesc.toLowerCase().contains("reg") || protocolName.toLowerCase().contains("reg"));
    }
    
    public static final boolean isADC(final String imgType) {
        return (imgType.equalsIgnoreCase("ADC") || imgType.equalsIgnoreCase("ADC_UNSPECIFIED"));
    }
    
    public static final boolean isDWI(final String imgType) {
        return (imgType.equalsIgnoreCase("SE") || imgType.equalsIgnoreCase("M_SE") || imgType.equalsIgnoreCase("TRACEW"));
    }
}
