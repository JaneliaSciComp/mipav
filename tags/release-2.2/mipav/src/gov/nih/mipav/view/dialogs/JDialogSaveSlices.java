package gov.nih.mipav.view.dialogs;

import gov.nih.mipav.view.*;
import gov.nih.mipav.model.file.*;

import java.awt.event.*;
import java.awt.*;
import java.awt.Dialog.*;
import java.lang.Math.*;

import javax.swing.*;

/**
     *   Simple dialog to indicate which slices should be saved.  There are different
 *	constructors based on whether a 3D or 4D image is to be saved, and also
 *	if it's a TIFF file.  The information entered is saved as a FileWriteOptions object.
 *
 *
 *		@version    1.0 Feburary 8, 1999
 *		@author     Matthew McAuliffe
 *		@author		Neva Cherniavsky
 *       @see        FileIO
 *		@see		FileWriteOptions
 *
 */
public class JDialogSaveSlices
    extends JDialogBase {

  private int minValue;
  private int maxValue;
  private int minTimeValue;
  private int maxTimeValue;
  private boolean enablePackBitWrite;
  private boolean timeEnabled;
  private boolean tiffEnabled;
  private boolean fourDimEnabled;
  private boolean corEnabled;
  private boolean afniEnabled;

  private FileWriteOptions options;

  private JTextField textFirstSlice;
  private JTextField textLastSlice;
  private JTextField textStartNumber;
  private JTextField textDigitNumber;
  private JTextField textFirstTimePeriod;
  private JTextField textLastTimePeriod;
  private JLabel labelFirstTimePeriod;
  private JLabel labelLastTimePeriod;
  private JLabel labelFirstSlice;
  private JLabel labelLastSlice;
  private JLabel labelStartNumber;
  private JLabel labelDigitNumber;

  private JCheckBox multiFileCheckbox;
  private JCheckBox packBitCheckbox;

  private JPanel slicePanel;
  private JPanel timePanel;
  private JPanel tiffPanel;

  /**
   *  Constructs a save dialog meant for a 3D image.
   *  @param theParentFrame Parent frame.
   *  @param mnValue        Lowest slice number in range.
   *  @param mxvalue        Highest slice number in range.
   *  @param options	    Structure to store the write options chosen here.
   */
  public JDialogSaveSlices(Frame theParentFrame, int mnValue, int mxValue,
                           FileWriteOptions options) {

    super(theParentFrame, true);
    minValue = mnValue;
    maxValue = mxValue;
    timeEnabled = false;
    fourDimEnabled = false;
    tiffEnabled = (options.getFileType() == FileBase.TIFF);
    corEnabled = (options.getFileType() == FileBase.COR);
    afniEnabled = (options.getFileType() == FileBase.AFNI);
    enablePackBitWrite = options.isPackBitEnabled();
    this.options = options;
    init();

  }

  /**
   *  Constructs a save dialog meant for a 4D image.
   *  @param theParentFrame Parent frame.
   *  @param mnValue        Lowest slice number in range.
   *  @param mxvalue        Highest slice number in range.
   *  @param mnTimeValue    Lowest time number in range.
   *  @param mxTimeValue    Highest time number in range.
   *  @param options	    Structure to store the write options chosen here.
   */
  public JDialogSaveSlices(Frame theParentFrame, int mnValue, int mxValue,
                           int mnTimeValue, int mxTimeValue,
                           FileWriteOptions options) {
    super(theParentFrame, true);
    minValue = mnValue;
    maxValue = mxValue;
    minTimeValue = mnTimeValue;
    maxTimeValue = mxTimeValue;
    timeEnabled = true;
    tiffEnabled = (options.getFileType() == FileBase.TIFF);
    fourDimEnabled = (options.getFileType() == FileBase.ANALYZE ||
                      options.getFileType() == FileBase.NIFTI ||
                      options.getFileType() == FileBase.RAW ||
                      options.getFileType() == FileBase.FITS ||
                      options.getFileType() == FileBase.ICS ||
                      options.getFileType() == FileBase.INTERFILE ||
                      options.getFileType() == FileBase.AFNI ||
                      options.getFileType() == FileBase.XML);
    enablePackBitWrite = options.isPackBitEnabled();
    this.options = options;
    init();
  }

  /**
   *	Sets up GUI and displays the dialog.
   */
  private void init() {

    setTitle("Save range of slices");
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
    gbc.fill = gbc.NONE;
    gbc.anchor = gbc.WEST;

    slicePanel = new JPanel();
    slicePanel.setLayout(new GridLayout(2, 2));
    slicePanel.setForeground(Color.black);
    slicePanel.setBorder(buildTitledBorder("Choose Range of Slices to Save"));

    labelFirstSlice = new JLabel("First Slice");
    labelFirstSlice.setFont(serif12);
    labelFirstSlice.setForeground(Color.black);
    slicePanel.add(labelFirstSlice);

    JPanel textFirstPanel = new JPanel();
    textFirstSlice = new JTextField(5);
    textFirstSlice.setText(String.valueOf(minValue));
    textFirstSlice.setFont(serif12);
    textFirstSlice.addFocusListener(this);
    gbc.gridx = 1;
    gbc.anchor = gbc.EAST;
    textFirstPanel.add(textFirstSlice);
    slicePanel.add(textFirstPanel);

    labelLastSlice = new JLabel("Last Slice");
    gbc.gridx = 0;
    gbc.gridy = 1;
    gbc.anchor = gbc.WEST;
    labelLastSlice.setFont(serif12);
    labelLastSlice.setForeground(Color.black);
    slicePanel.add(labelLastSlice);

    JPanel textLastPanel = new JPanel();
    textLastSlice = new JTextField(5);
    gbc.gridx = 1;
    gbc.anchor = gbc.EAST;
    textLastSlice.setText(String.valueOf(maxValue));
    textLastSlice.setFont(serif12);
    textLastSlice.addFocusListener(this);
    textLastPanel.add(textLastSlice);
    slicePanel.add(textLastPanel);

    timePanel = new JPanel();
    timePanel.setLayout(new GridLayout(2, 2));
    timePanel.setForeground(Color.black);
    timePanel.setBorder(buildTitledBorder(
        "Choose Range of Time Periods to Save"));

    labelFirstTimePeriod = new JLabel("First Time Period");
    labelFirstTimePeriod.setFont(serif12);
    labelFirstTimePeriod.setForeground(Color.black);
    labelFirstTimePeriod.setEnabled(timeEnabled);
    timePanel.add(labelFirstTimePeriod);

    JPanel textFirstTimePanel = new JPanel();
    textFirstTimePeriod = new JTextField(5);
    if (timeEnabled)
      textFirstTimePeriod.setText(String.valueOf(minTimeValue));
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
    if (timeEnabled && fourDimEnabled)
      textLastTimePeriod.setText(String.valueOf(maxTimeValue));
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

    JPanel generalPanel = new JPanel();
    generalPanel.setBorder(buildTitledBorder("General Options"));
    generalPanel.setLayout(new BorderLayout());

    multiFileCheckbox = new JCheckBox("Save image slices to separate files");
    multiFileCheckbox.setFont(serif12);
    if (corEnabled) {
        multiFileCheckbox.setSelected(true);
        multiFileCheckbox.setEnabled(false);
    }
    else {
        multiFileCheckbox.setSelected(false);
    }
    if (afniEnabled || options.getFileType() == FileBase.DICOM) {
        multiFileCheckbox.setEnabled(false);
    }
    multiFileCheckbox.setActionCommand("Multi");
    multiFileCheckbox.addActionListener(this);
    generalPanel.add(multiFileCheckbox, BorderLayout.NORTH);

    JPanel filePanel = new JPanel();
    filePanel.setLayout(new GridLayout(2, 2));

    labelStartNumber = new JLabel("First File Starting Number");
    labelStartNumber.setFont(serif12);
    if (corEnabled) {
        labelStartNumber.setEnabled(true);
    }
    else {
        labelStartNumber.setEnabled(false);
    }
    labelStartNumber.setForeground(Color.black);
    filePanel.add(labelStartNumber);

    JPanel textStartPanel = new JPanel();
    textStartNumber = new JTextField(5);
    textStartNumber.setText(String.valueOf(1));
    textStartNumber.setFont(serif12);
    if (corEnabled) {
        textStartNumber.setEnabled(true);
    }
    else {
        textStartNumber.setEnabled(false);
    }
    textStartNumber.addFocusListener(this);
    textStartPanel.add(textStartNumber);
    filePanel.add(textStartPanel);

    labelDigitNumber = new JLabel("File Name Number of Digits");
    labelDigitNumber.setFont(serif12);
    if (corEnabled) {
        labelDigitNumber.setEnabled(true);
    }
    else {
        labelDigitNumber.setEnabled(false);
    }
    labelDigitNumber.setForeground(Color.black);
    filePanel.add(labelDigitNumber);

    JPanel textDigitPanel = new JPanel();
    textDigitNumber = new JTextField(5);
    textDigitNumber.setText(String.valueOf(3));
    textDigitNumber.setFont(serif12);
    if (corEnabled) {
        textDigitNumber.setEnabled(true);
    }
    else {
        textDigitNumber.setEnabled(false);
    }
    textDigitNumber.addFocusListener(this);
    textDigitPanel.add(textDigitNumber);
    filePanel.add(textDigitPanel);

    generalPanel.add(filePanel, BorderLayout.SOUTH);

    tiff2Panel.setLayout(new BorderLayout());
    tiff2Panel.add(tiffPanel, BorderLayout.CENTER);

    mainPanel.add(generalPanel);
    mainPanel.add(slicePanel);
    mainPanel.add(timePanel);
    mainPanel.add(tiff2Panel);

    JPanel buttonPanel = new JPanel();
    buttonPanel.add(buildOKButton());
    buttonPanel.add(buildCancelButton());

    getContentPane().add(mainPanel);
    getContentPane().add(buttonPanel, BorderLayout.SOUTH);

    pack();
    OKButton.requestFocus();
    setVisible(true);
    //if(options.getFileType() == FileBase.DICOM) {
    //    System.out.println("hhhhhadhhhahhhhahahh");
    //    actionPerformed(new ActionEvent(this,1,"OK"));
    //}

  }

  /**
   *	Returns the necessary options that the user set in this dialog.
   *	@return		A structure holding the write options that the user set up.
   */
  public FileWriteOptions getWriteOptions() {
    return options;
  }

  /**
   *    Closes dialog box when the OK button is pressed and
   *    sets the information; when multi checkbox is selected or deselected,
   *	enables or disables appropriate labels and text fields; and disposes on cancel.
   *    @param event      Event that triggers this function.
   */
  public void actionPerformed(ActionEvent event) {
    String command = event.getActionCommand();
    int endNumber;
    int endDigits;

    if (command.equals("OK")) {
      String tmpStr;

      tmpStr = textFirstSlice.getText();

      if (testParameter(tmpStr, minValue, maxValue)) {
        options.setBeginSlice(Integer.parseInt(tmpStr) - 1);
      }
      else {
        textFirstSlice.requestFocus();
        textFirstSlice.selectAll();
        return;
      }

      tmpStr = textLastSlice.getText();

      if (testParameter(tmpStr, minValue, maxValue)) {
        options.setEndSlice(Integer.parseInt(tmpStr) - 1);
      }
      else {
        textLastSlice.requestFocus();
        textLastSlice.selectAll();
        return;
      }

      if (options.getBeginSlice() > options.getEndSlice()) {
        MipavUtil.displayError(
            "First slice must be less than or equal to last slice");
        textLastSlice.requestFocus();
        textLastSlice.selectAll();
        return;
      }

      if (timeEnabled) {
        tmpStr = textFirstTimePeriod.getText();
        if (testParameter(tmpStr, minTimeValue, maxTimeValue)) {
          options.setBeginTime(Integer.parseInt(tmpStr) - 1);
          if (tiffEnabled)
            options.setTimeSlice(options.getBeginTime());
        }
        else {
          textFirstTimePeriod.requestFocus();
          textFirstTimePeriod.selectAll();
          return;
        }

        // don't test end time period if TIFF or MINC image, because there is no value there.
        if (fourDimEnabled) {
          tmpStr = textLastTimePeriod.getText();
          if (testParameter(tmpStr, minTimeValue, maxTimeValue)) {
            options.setEndTime(Integer.parseInt(tmpStr) - 1);
          }
          else {
            textLastTimePeriod.requestFocus();
            textLastTimePeriod.selectAll();
            return;
          }

          if (options.getBeginTime() > options.getEndTime()) {
            MipavUtil.displayError(
                "First slice must be less than or equal to last slice");
            textLastTimePeriod.requestFocus();
            textLastTimePeriod.selectAll();
            return;
          }
        }
      }

      if (corEnabled || (multiFileCheckbox.isEnabled() && multiFileCheckbox.isSelected())) {
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
        endNumber = options.getStartNumber() + options.getEndSlice() -
            options.getBeginSlice();
        if (endNumber > 0) {
          // log10(x) = loge(x)/loge(10)
          endDigits = 1 +
              (int) (0.4342944819 * java.lang.Math.log( (double) endNumber));
          if (endDigits > options.getDigitNumber()) {
            MipavUtil.displayError("At least " + endDigits +
                                   " digits are required");
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
      }
      else
        options.setMultiFile(false);
      options.setWritePackBit(packBitCheckbox.isSelected());

      dispose();
    }
    else if (command.equals("Cancel")) {
      cancelFlag = true;
      dispose();
    }
    else if (command.equals("Multi")) {
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
      }
      else {
        textStartNumber.setEnabled(false);
        textDigitNumber.setEnabled(false);
        labelStartNumber.setEnabled(false);
        labelDigitNumber.setEnabled(false);
        if (fourDimEnabled) {
          textFirstSlice.setEnabled(true);
          textLastSlice.setEnabled(true);
        }
      }
    }
  }

}
