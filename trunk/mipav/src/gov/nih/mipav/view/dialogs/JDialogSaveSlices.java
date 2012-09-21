package gov.nih.mipav.view.dialogs;


import gov.nih.mipav.model.file.*;

import gov.nih.mipav.view.*;

import java.awt.*;
import java.awt.event.*;

import javax.swing.*;


/**
 * Simple dialog to indicate which slices should be saved. There are different constructors based on whether a 3D or 4D
 * image is to be saved, and also if it's a TIFF file. The information entered is saved as a FileWriteOptions object.
 * There is no provision for storing 4D TIFF images, so if TIFF is 4D always save as a set of 3D volumes.
 *
 * @version  1.0 Feburary 8, 1999
 * @author   Matthew McAuliffe
 * @author   Neva Cherniavsky
 * @see      FileIO
 * @see      FileWriteOptions
 */
public class JDialogSaveSlices extends JDialogBase {

    //~ Static fields/initializers -------------------------------------------------------------------------------------

    /** Use serialVersionUID for interoperability. */
    private static final long serialVersionUID = 6208822935229525687L;

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** DOCUMENT ME! */
    private boolean afniEnabled;

    /** DOCUMENT ME! */
    private boolean corEnabled;
    
    private boolean geGenesisEnabled;
    
    private boolean geSigna4XEnabled;

    /** DOCUMENT ME! */
    private boolean enablePackBitWrite;

    /** DOCUMENT ME! */
    private boolean fourDimEnabled;

    /** DOCUMENT ME! */
    private JLabel labelDigitNumber;

    /** DOCUMENT ME! */
    private JLabel labelFirstSlice;

    /** DOCUMENT ME! */
    private JLabel labelFirstTimePeriod;

    /** DOCUMENT ME! */
    private JLabel labelLastSlice;

    /** DOCUMENT ME! */
    private JLabel labelLastTimePeriod;

    /** DOCUMENT ME! */
    private JLabel labelStartNumber;

    /** DOCUMENT ME! */
    private int maxTimeValue;

    /** DOCUMENT ME! */
    private int maxValue;

    /** DOCUMENT ME! */
    private int minTimeValue;

    /** DOCUMENT ME! */
    private int minValue;

    /** DOCUMENT ME! */
    private JCheckBox multiFileCheckbox;

    /** DOCUMENT ME! */
    private FileWriteOptions options;

    /** DOCUMENT ME! */
    private JCheckBox packBitCheckbox;

    /** DOCUMENT ME! */
    private JPanel slicePanel;

    /** DOCUMENT ME! */
    private JTextField textDigitNumber;

    /** DOCUMENT ME! */
    private JTextField textFirstSlice;

    /** DOCUMENT ME! */
    private JTextField textFirstTimePeriod;

    /** DOCUMENT ME! */
    private JTextField textLastSlice;

    /** DOCUMENT ME! */
    private JTextField textLastTimePeriod;

    /** DOCUMENT ME! */
    private JTextField textStartNumber;

    /** DOCUMENT ME! */
    private boolean tiffEnabled;

    /** DOCUMENT ME! */
    private JPanel tiffPanel;

    /** DOCUMENT ME! */
    private boolean timeEnabled;

    /** DOCUMENT ME! */
    private JPanel timePanel;
    
    /** Dicom options panel */
    private JPanel dicomInfoPanel;
    
    /** Check box for specifying whether DICOM files should be saved as an encapsulated JPEG2000*/
    private JCheckBox encapJP2Checkbox;
    
    /** Check box for specifying whether DICOM files should be stamped with MIPAV information*/
    private JCheckBox stampSecondaryCheckbox;
    
    /** Check box for saving dicom files in enhanced format (concatenates all frames in one file */
    private JCheckBox saveEnhancedDicomCheckbox;
    
    /** Whether DICOM files should be saved as an encapsulated JPEG2000 */
    private boolean saveAsEncapJP2 = false;
    
    /** Whether to stamp DICOM files with the MIPAV secondary stamp */
    private boolean stampSecondary = true;

    /** Whether 3D or higher dimensionality dicom images should be saved as a single file */
    private boolean saveEnhancedDicom = false;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Constructs a save dialog meant for a 3D image.
     *
     * @param  theParentFrame  Parent frame.
     * @param  mnValue         Lowest slice number in range.
     * @param  mxValue         Highest slice number in range.
     * @param  options         Structure to store the write options chosen here.
     */
    public JDialogSaveSlices(Frame theParentFrame, int mnValue, int mxValue, FileWriteOptions options) {

        super(theParentFrame, true);
        minValue = mnValue;
        maxValue = mxValue;
        timeEnabled = false;
        fourDimEnabled = false;
        tiffEnabled = (options.getFileType() == FileUtility.TIFF);
        corEnabled = (options.getFileType() == FileUtility.COR);
        afniEnabled = (options.getFileType() == FileUtility.AFNI);
        geSigna4XEnabled = (options.getFileType() == FileUtility.GE_SIGNA4X);
        geGenesisEnabled = (options.getFileType() == FileUtility.GE_GENESIS);
        enablePackBitWrite = options.isPackBitEnabled();
        this.options = options;
        init();

    }

    /**
     * Constructs a save dialog meant for a 4D image.
     *
     * @param  theParentFrame  Parent frame.
     * @param  mnValue         Lowest slice number in range.
     * @param  mxValue         Highest slice number in range.
     * @param  mnTimeValue     Lowest time number in range.
     * @param  mxTimeValue     Highest time number in range.
     * @param  options         Structure to store the write options chosen here.
     */
    public JDialogSaveSlices(Frame theParentFrame, int mnValue, int mxValue, int mnTimeValue, int mxTimeValue,
                             FileWriteOptions options) {
        super(theParentFrame, true);
        minValue = mnValue;
        maxValue = mxValue;
        minTimeValue = mnTimeValue;
        maxTimeValue = mxTimeValue;
        timeEnabled = true;
        tiffEnabled = (options.getFileType() == FileUtility.TIFF);
        fourDimEnabled = ((options.getFileType() == FileUtility.ANALYZE) ||
                              (options.getFileType() == FileUtility.NIFTI) ||
                              (options.getFileType() == FileUtility.RAW) ||
                              (options.getFileType() == FileUtility.FITS) ||
                              (options.getFileType() == FileUtility.ICS) ||
                              (options.getFileType() == FileUtility.INTERFILE) ||
                              (options.getFileType() == FileUtility.AFNI) ||
                              (options.getFileType() == FileUtility.XML) ||
                              (options.getFileType() == FileUtility.PARREC) || // Modified to support PAR/REC
                              (options.getFileType() == FileUtility.NRRD)||
                              (options.getFileType() == FileUtility.MINC_HDF) ||
                              (options.getFileType() == FileUtility.DICOM) ||  //will only write 4D dicom in enhanced format 
                              (options.getFileType() == FileUtility.TIFF)); // TIFF must be stored as a set of 3D volumes
        enablePackBitWrite = options.isPackBitEnabled();
        this.options = options;
        init();
       
    }

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * Closes dialog box when the OK button is pressed and sets the information; when multi checkbox is selected or
     * deselected, enables or disables appropriate labels and text fields; and disposes on cancel.
     *
     * @param  event  Event that triggers this function.
     */
    public void actionPerformed(ActionEvent event) {
        String command = event.getActionCommand();
        int endNumber;
        int endDigits;

        if (command.equals("OK")) {
            String tmpStr;

            tmpStr = textFirstSlice.getText();

            if (testParameter(tmpStr, minValue, maxValue)) {
                options.setBeginSlice(Integer.parseInt(tmpStr));
            } else {
                textFirstSlice.requestFocus();
                textFirstSlice.selectAll();

                return;
            }

            tmpStr = textLastSlice.getText();

            if (testParameter(tmpStr, minValue, maxValue)) {
                options.setEndSlice(Integer.parseInt(tmpStr));
            } else {
                textLastSlice.requestFocus();
                textLastSlice.selectAll();

                return;
            }

            if (options.getBeginSlice() > options.getEndSlice()) {
                MipavUtil.displayError("First slice must be less than or equal to last slice");
                textLastSlice.requestFocus();
                textLastSlice.selectAll();

                return;
            }

            if (timeEnabled) {
                tmpStr = textFirstTimePeriod.getText();

                if (testParameter(tmpStr, minTimeValue, maxTimeValue)) {
                    options.setBeginTime(Integer.parseInt(tmpStr));

                    if (tiffEnabled) {
                        options.setTimeSlice(options.getBeginTime());
                    }
                } else {
                    textFirstTimePeriod.requestFocus();
                    textFirstTimePeriod.selectAll();

                    return;
                }

                // don't test end time period if TIFF or MINC image, because there is no value there.
                if (fourDimEnabled) {
                    tmpStr = textLastTimePeriod.getText();

                    if (testParameter(tmpStr, minTimeValue, maxTimeValue)) {
                        options.setEndTime(Integer.parseInt(tmpStr));
                    } else {
                        textLastTimePeriod.requestFocus();
                        textLastTimePeriod.selectAll();

                        return;
                    }

                    if (options.getBeginTime() > options.getEndTime()) {
                        MipavUtil.displayError("First slice must be less than or equal to last slice");
                        textLastTimePeriod.requestFocus();
                        textLastTimePeriod.selectAll();

                        return;
                    }
                }
            }

            if (corEnabled || geSigna4XEnabled || geGenesisEnabled || (tiffEnabled && timeEnabled) ||(multiFileCheckbox.isEnabled() &&
            	multiFileCheckbox.isSelected())) {
                options.setMultiFile(true);
                tmpStr = textStartNumber.getText();
                options.setStartNumber(Integer.parseInt(tmpStr));

                if (options.getStartNumber() < 0) {
                    MipavUtil.displayError("Start Number must not be less than 0");
                    textStartNumber.requestFocus();
                    textStartNumber.selectAll();

                    return;
                }

                tmpStr = textDigitNumber.getText();
                options.setDigitNumber(Integer.parseInt(tmpStr));

                if (options.getDigitNumber() < 1) {
                    MipavUtil.displayError("Must have at least 1 digit");
                    textDigitNumber.requestFocus();
                    textDigitNumber.selectAll();

                    return;
                }

                endNumber = options.getStartNumber() + options.getEndSlice() - options.getBeginSlice();

                if (endNumber > 0) {

                    // log10(x) = loge(x)/loge(10)
                    endDigits = 1 + (int) (0.4342944819 * java.lang.Math.log((double) endNumber));

                    if (endDigits > options.getDigitNumber()) {
                        MipavUtil.displayError("At least " + endDigits + " digits are required");
                        textDigitNumber.requestFocus();
                        textDigitNumber.selectAll();

                        return;
                    }
                }

                if (options.getDigitNumber() > 4) {
                    MipavUtil.displayError("No more than 4 digits are allowed");
                    textDigitNumber.requestFocus();
                    textDigitNumber.selectAll();

                    return;
                }
            } else {
                options.setMultiFile(false);
            }
            saveAsEncapJP2 = encapJP2Checkbox.isSelected();
            stampSecondary = stampSecondaryCheckbox.isSelected();
            if(saveEnhancedDicomCheckbox != null) {
                saveEnhancedDicom = saveEnhancedDicomCheckbox.isSelected();
            }
            options.setWritePackBit(packBitCheckbox.isSelected());

            dispose();
        } else if (command.equals("Cancel")) {
            cancelFlag = true;
            dispose();
        } else if (command.equals("Multi")) {

            if (multiFileCheckbox.isSelected()) {
                textStartNumber.setEnabled(true);
                textDigitNumber.setEnabled(true);
                labelStartNumber.setEnabled(true);
                labelDigitNumber.setEnabled(true);

                if (fourDimEnabled) {
                    textFirstSlice.setText(new Integer(minValue).toString());
                    textLastSlice.setText(new Integer(maxValue).toString());
                    textFirstSlice.setEnabled(false);
                    textLastSlice.setEnabled(false);
                }
            } else {
                textStartNumber.setEnabled(false);
                textDigitNumber.setEnabled(false);
                labelStartNumber.setEnabled(false);
                labelDigitNumber.setEnabled(false);

                if (fourDimEnabled) {
                    textFirstSlice.setEnabled(true);
                    textLastSlice.setEnabled(true);
                }
            }
        } else if (command.equalsIgnoreCase("help")) {
            //MipavUtil.showHelp("U4019");
            MipavUtil.showWebHelp("Converting_non-DICOM_image_files_to_DICOM_format#Save_Range_of_Slices_dialog_box");
        } else {
            super.actionPerformed(event);
        }
    }

    /**
     * Returns the necessary options that the user set in this dialog.
     *
     * @return  A structure holding the write options that the user set up.
     */
    public FileWriteOptions getWriteOptions() {
        return options;
    }
    
    /**
     * @return Whether dicom files should be stamped with MIPAV information.
     */
    public boolean doStampSecondary() {
    	return stampSecondary;
    }

    /**
     * @return Whether dicom should be saved in enhanced format.
     */
    public boolean doSaveEnhancedDicom() {
        return saveEnhancedDicom;
    }

    public boolean getSaveAsEncapJP2() {
		return saveAsEncapJP2;
	}

	/**
     * Sets up GUI and displays the dialog.
     */
    private void init() {

        if (timeEnabled)  {
            setTitle("Save range of volumes");
        }
        else {
            setTitle("Save range of slices");
        }
        setResizable(false);
        cancelFlag = false;

        JPanel mainPanel = new JPanel();
        mainPanel.setLayout(new BoxLayout(mainPanel, BoxLayout.Y_AXIS));

        GridBagConstraints gbc = new GridBagConstraints();
        Insets insets = new Insets(0, 2, 0, 2);

        gbc.gridx = 0;
        gbc.gridy = 0;
        gbc.gridwidth = 1;
        gbc.gridheight = 1;
        gbc.insets = insets;
        gbc.weightx = 0;
        gbc.fill = GridBagConstraints.NONE;
        gbc.anchor = GridBagConstraints.WEST;

        slicePanel = new JPanel();
        slicePanel.setLayout(new GridLayout(2, 2));
        slicePanel.setForeground(Color.black);
        slicePanel.setBorder(buildTitledBorder("Choose Range of Slices to Save"));

        labelFirstSlice = new JLabel("First Slice");
        labelFirstSlice.setFont(serif12);
        labelFirstSlice.setForeground(Color.black);
        if((maxValue == 0) || (tiffEnabled && timeEnabled)) {
        	labelFirstSlice.setEnabled(false);
        }
        slicePanel.add(labelFirstSlice);

        JPanel textFirstPanel = new JPanel();
        textFirstSlice = new JTextField(5);
        textFirstSlice.setText(String.valueOf(minValue));
        textFirstSlice.setFont(serif12);
        textFirstSlice.addFocusListener(this);
        if((maxValue == 0) || (tiffEnabled && timeEnabled)) {
        	textFirstSlice.setEnabled(false);
        }
        gbc.gridx = 1;
        gbc.anchor = GridBagConstraints.EAST;
        textFirstPanel.add(textFirstSlice);
        slicePanel.add(textFirstPanel);

        labelLastSlice = new JLabel("Last Slice");
        gbc.gridx = 0;
        gbc.gridy = 1;
        gbc.anchor = GridBagConstraints.WEST;
        labelLastSlice.setFont(serif12);
        labelLastSlice.setForeground(Color.black);
        if((maxValue == 0) || (tiffEnabled && timeEnabled)) {
        	labelLastSlice.setEnabled(false);
        }
        slicePanel.add(labelLastSlice);

        JPanel textLastPanel = new JPanel();
        textLastSlice = new JTextField(5);
        gbc.gridx = 1;
        gbc.anchor = GridBagConstraints.EAST;
        textLastSlice.setText(String.valueOf(maxValue));
        textLastSlice.setFont(serif12);
        textLastSlice.addFocusListener(this);
        if((maxValue == 0) || (tiffEnabled && timeEnabled)) {
        	textLastSlice.setEnabled(false);
        }
        textLastPanel.add(textLastSlice);
        slicePanel.add(textLastPanel);

        timePanel = new JPanel();
        timePanel.setLayout(new GridLayout(2, 2));
        timePanel.setForeground(Color.black);
        timePanel.setBorder(buildTitledBorder("Choose Range of Time Periods to Save"));

        labelFirstTimePeriod = new JLabel("First Time Period");
        labelFirstTimePeriod.setFont(serif12);
        labelFirstTimePeriod.setForeground(Color.black);
        labelFirstTimePeriod.setEnabled(timeEnabled);
        timePanel.add(labelFirstTimePeriod);

        JPanel textFirstTimePanel = new JPanel();
        textFirstTimePeriod = new JTextField(5);

        if (timeEnabled) {
            textFirstTimePeriod.setText(String.valueOf(minTimeValue));
        }

        textFirstTimePeriod.setFont(serif12);
        textFirstTimePeriod.addFocusListener(this);
        textFirstTimePeriod.setEnabled(timeEnabled);
        textFirstTimePanel.add(textFirstTimePeriod);
        timePanel.add(textFirstTimePanel);

        labelLastTimePeriod = new JLabel("Last time period");
        labelLastTimePeriod.setFont(serif12);
        labelLastTimePeriod.setForeground(Color.black);
        labelLastTimePeriod.setEnabled(timeEnabled && fourDimEnabled);
        timePanel.add(labelLastTimePeriod);

        JPanel textLastTimePanel = new JPanel();
        textLastTimePeriod = new JTextField(5);

        if (timeEnabled && fourDimEnabled) {
            textLastTimePeriod.setText(String.valueOf(maxTimeValue));
        }

        textLastTimePeriod.setFont(serif12);
        textLastTimePeriod.addFocusListener(this);
        textLastTimePeriod.setEnabled(timeEnabled && fourDimEnabled);
        textLastTimePanel.add(textLastTimePeriod);
        timePanel.add(textLastTimePanel);

        JPanel tiff2Panel = new JPanel();
        tiffPanel = new JPanel();
        tiffPanel.setLayout(new BorderLayout());
        tiffPanel.setForeground(Color.black);
        tiff2Panel.setBorder(buildTitledBorder("TIFF Options"));

        packBitCheckbox = new JCheckBox("Save with packed bits compression");
        packBitCheckbox.setFont(serif12);
        packBitCheckbox.setSelected(false);
        packBitCheckbox.setEnabled(enablePackBitWrite);
        packBitCheckbox.setAlignmentX(Component.LEFT_ALIGNMENT);
        tiffPanel.add(packBitCheckbox);
        
        //encapsulated jpeg2000 dicom panel
        JPanel encapJP2Panel2 = new JPanel();
        dicomInfoPanel = new JPanel();
        dicomInfoPanel.setLayout(new BorderLayout());
        dicomInfoPanel.setForeground(Color.black);
        dicomInfoPanel.setBorder(buildTitledBorder("DICOM Options"));
        
        encapJP2Checkbox = new JCheckBox("Save as Encapsulated JPEG2000");
        encapJP2Checkbox.setFont(serif12);
        encapJP2Checkbox.setSelected(false);
        encapJP2Checkbox.setAlignmentX(Component.LEFT_ALIGNMENT);
        dicomInfoPanel.add(encapJP2Checkbox, BorderLayout.NORTH);
        
        stampSecondaryCheckbox = new JCheckBox("Stamp dicom files with MIPAV information");
        stampSecondaryCheckbox.setFont(serif12);
        stampSecondaryCheckbox.setSelected(true);
        stampSecondaryCheckbox.setAlignmentX(Component.LEFT_ALIGNMENT);
        dicomInfoPanel.add(stampSecondaryCheckbox, BorderLayout.CENTER);
        
        if(maxValue != 0) { //image is 3D or greater in dimensions
            saveEnhancedDicomCheckbox = new JCheckBox("Save as enhanced dicom");
            saveEnhancedDicomCheckbox.setFont(serif12);
            saveEnhancedDicomCheckbox.setSelected(false);
            saveEnhancedDicomCheckbox.setAlignmentX(Component.LEFT_ALIGNMENT);
            dicomInfoPanel.add(saveEnhancedDicomCheckbox, BorderLayout.SOUTH);
        }

        JPanel generalPanel = new JPanel();
        generalPanel.setBorder(buildTitledBorder("General Options"));
        generalPanel.setLayout(new BorderLayout());

        if (timeEnabled) {
            multiFileCheckbox = new JCheckBox("Save image volumes to separate files");    
        }
        else {
            multiFileCheckbox = new JCheckBox("Save image slices to separate files");
        }
        multiFileCheckbox.setFont(serif12);
        
        

        if (corEnabled || geSigna4XEnabled || geGenesisEnabled || (tiffEnabled && timeEnabled)) {
            // No provision for storing 4D tiff files
            multiFileCheckbox.setSelected(true);
            multiFileCheckbox.setEnabled(false);
        } else {
            multiFileCheckbox.setSelected(false);
        }

        if (afniEnabled || (options.getFileType() == FileUtility.DICOM)) {
            multiFileCheckbox.setEnabled(false);
        }

        multiFileCheckbox.setActionCommand("Multi");
        multiFileCheckbox.addActionListener(this);
        generalPanel.add(multiFileCheckbox, BorderLayout.NORTH);

        JPanel filePanel = new JPanel();
        filePanel.setLayout(new GridLayout(2, 2));

        labelStartNumber = new JLabel("First File Starting Number");
        labelStartNumber.setFont(serif12);

        if (corEnabled || geSigna4XEnabled || geGenesisEnabled || (tiffEnabled && timeEnabled)) {
            labelStartNumber.setEnabled(true);
        } else {
            labelStartNumber.setEnabled(false);
        }

        labelStartNumber.setForeground(Color.black);
        filePanel.add(labelStartNumber);

        JPanel textStartPanel = new JPanel();
        textStartNumber = new JTextField(5);
        textStartNumber.setText(String.valueOf(1));
        textStartNumber.setFont(serif12);

        if (corEnabled || geSigna4XEnabled || geGenesisEnabled || (tiffEnabled && timeEnabled)) {
            textStartNumber.setEnabled(true);
        } else {
            textStartNumber.setEnabled(false);
        }

        textStartNumber.addFocusListener(this);
        textStartPanel.add(textStartNumber);
        filePanel.add(textStartPanel);

        labelDigitNumber = new JLabel("File Name Number of Digits");
        labelDigitNumber.setFont(serif12);

        if (corEnabled || geSigna4XEnabled || geGenesisEnabled || (tiffEnabled && timeEnabled)) {
            labelDigitNumber.setEnabled(true);
        } else {
            labelDigitNumber.setEnabled(false);
        }

        labelDigitNumber.setForeground(Color.black);
        filePanel.add(labelDigitNumber);

        JPanel textDigitPanel = new JPanel();
        textDigitNumber = new JTextField(5);
        textDigitNumber.setText(String.valueOf(3));
        textDigitNumber.setFont(serif12);

        if (corEnabled || geSigna4XEnabled || geGenesisEnabled || (tiffEnabled && timeEnabled)) {
            textDigitNumber.setEnabled(true);
        } else {
            textDigitNumber.setEnabled(false);
        }

        textDigitNumber.addFocusListener(this);
        textDigitPanel.add(textDigitNumber);
        filePanel.add(textDigitPanel);

        generalPanel.add(filePanel, BorderLayout.SOUTH);

        tiff2Panel.setLayout(new BorderLayout());
        tiff2Panel.add(tiffPanel, BorderLayout.CENTER);
        
        encapJP2Panel2.setLayout(new BorderLayout());
        encapJP2Panel2.add(dicomInfoPanel, BorderLayout.CENTER);

        mainPanel.add(generalPanel);
        mainPanel.add(slicePanel);
        mainPanel.add(timePanel);
        mainPanel.add(tiff2Panel);
        if(options.getFileType() == FileUtility.DICOM) {
        	mainPanel.add(encapJP2Panel2);
        }

        JPanel buttonPanel = new JPanel();
        buttonPanel.add(buildOKButton());
        buttonPanel.add(buildCancelButton());

        // buttonPanel.add(buildHelpButton());
        getContentPane().add(mainPanel);
        getContentPane().add(buttonPanel, BorderLayout.SOUTH);

        pack();
        OKButton.requestFocus();
        setVisible(true);
        // if(options.getFileType() == FileUtility.DICOM) {
        // actionPerformed(new ActionEvent(this,1,"OK"));
        // }

    }

}
