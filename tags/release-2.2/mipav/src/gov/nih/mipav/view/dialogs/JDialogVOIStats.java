package gov.nih.mipav.view.dialogs;

import gov.nih.mipav.view.*;
import gov.nih.mipav.model.structures.*;
import gov.nih.mipav.model.file.*;
import gov.nih.mipav.model.algorithms.*;
import gov.nih.mipav.view.*;

import java.awt.event.*;
import java.awt.*;
import java.util.*;

import javax.swing.*;
import javax.swing.event.*;

/**
 *
 *		@version    1.1 June 15, 1999
 *		@author     Matthew J. McAuliffe, Ph.D.
 *
 *       $Logfile: /mipav/src/gov/nih/mipav/view/dialogs/JDialogVOIStats.java $
 *       $Revision: 56 $
 *       $Date: 2/17/06 6:20p $
 */
public class JDialogVOIStats
    extends JDialogBase
    implements ItemListener,
    ChangeListener,
    FocusListener,
    UpdateVOISelectionListener {

  private JButton calcButton;
  private JButton applyButton;
  private JButton colorButton;
  private JTextField VOIName;
  private JCheckBox checkboxBoundingBox;
  private JCheckBox checkboxAdditiveOrSubtractive;
  private JCheckBox checkboxIncludeForProcessing;
  private JCheckBox checkboxBoundary;
  private JScrollPane sPane;
  private JPanel statsPanel;

  private JCheckBox checkboxExclude;
  private JTextField textMin;
  private JTextField textMax;
  private JLabel labelMin;
  private JLabel labelMax;
  private JSlider opacitySlider;
  private JLabel current;

  private VOI voi;
  private ModelImage image;
  //private     JVOIChecklist       listPanel;
  private JPanelStatisticsList listPanel;
  private AlgorithmVOIProps algoVOI;

  private JTextField seedValueTF;
  private short seedValue;

  private ViewJColorChooser colorChooser;
  private Color colorVOI;

  /** Constructor for the JDialogVOIStats.
   *   <p>
   *   this class ought to listen for
   *   VOI updates, but we are having it
   *   implemented elsewhere.
   */
  public JDialogVOIStats(Frame theParentFrame, ModelImage img, VOI _voi) {
    super(theParentFrame, false);
    voi = _voi;
    image = img;
    init();
  }

  /**
   *   Sets up GUI components - buttons, checkboxes, sliders, etc.
   */
  private void init() {
    //setTitle("VOI Statistics");

    JLabel labelName = new JLabel("Name of VOI");
    labelName.setFont(serif12);
    labelName.setForeground(Color.black);

    colorButton = new JButton();
    colorButton.setPreferredSize(new Dimension(25, 25));
    colorButton.setToolTipText("Change VOI color");
    colorButton.addActionListener(this);

    VOIName = new JTextField(10);
    VOIName.setFont(serif12);

    JPanel namePanel = new JPanel(new GridBagLayout());
    GridBagConstraints gbc = new GridBagConstraints();
    gbc.anchor = gbc.WEST;
    gbc.weightx = 1;

    namePanel.add(labelName, gbc);
    gbc.gridx = 1;
    namePanel.add(colorButton, gbc);
    gbc.gridx = 2;
    gbc.weightx = 1; //gbc.fill = gbc.HORIZONTAL;
    namePanel.add(VOIName);

    checkboxBoundingBox = new JCheckBox("Show contour bounding box");
    checkboxBoundingBox.setFont(serif12);

    checkboxAdditiveOrSubtractive = new JCheckBox("Use additive polarity for VOI");
    checkboxAdditiveOrSubtractive.setFont(serif12);

    checkboxIncludeForProcessing = new JCheckBox("Include for processing");
    checkboxIncludeForProcessing.setFont(serif12);

    checkboxBoundary = new JCheckBox("Display VOI shading");
    checkboxBoundary.setFont(serif12);
    checkboxBoundary.addItemListener(this);

    JPanel checkboxPanel = new JPanel();
    checkboxPanel.setLayout(new BoxLayout(checkboxPanel, BoxLayout.Y_AXIS));
    checkboxPanel.add(checkboxBoundingBox);
    checkboxPanel.add(checkboxAdditiveOrSubtractive);
    checkboxPanel.add(checkboxIncludeForProcessing);
    checkboxPanel.add(checkboxBoundary);

    opacitySlider = new JSlider(JSlider.HORIZONTAL, 0, 100, 30);

    opacitySlider.setMajorTickSpacing(20);
    opacitySlider.setValue(30);
    opacitySlider.setPaintTicks(true);
    opacitySlider.setEnabled(true);
    opacitySlider.addChangeListener(this);

    JLabel maximum = new JLabel(String.valueOf(1));
    maximum.setForeground(Color.black);
    maximum.setFont(serif12);

    current = new JLabel(String.valueOf(opacitySlider.getValue() / 100.0f));
    current.setForeground(Color.black);
    current.setFont(serif12B);

    JLabel minimum = new JLabel(String.valueOf(0));
    minimum.setForeground(Color.black);
    minimum.setFont(serif12);

    JPanel sliderPanel = new JPanel(new GridBagLayout());

    gbc.gridx = 0;
    gbc.gridy = 0;
    gbc.gridwidth = 3;
    gbc.weightx = 1;
    gbc.gridheight = 1;
    gbc.fill = gbc.HORIZONTAL;

    sliderPanel.add(opacitySlider, gbc);

    gbc.gridx = 0;
    gbc.gridy = 1;
    gbc.gridwidth = 1;
    gbc.weightx = 0;
    gbc.anchor = gbc.WEST;
    gbc.fill = gbc.NONE;

    sliderPanel.add(minimum, gbc);

    gbc.gridx = 1;
    gbc.anchor = gbc.CENTER;
    gbc.weightx = .5;

    sliderPanel.add(current, gbc);

    gbc.gridx = 2;
    gbc.anchor = gbc.EAST;
    gbc.weightx = 0;

    sliderPanel.add(maximum, gbc);
    sliderPanel.setBorder(buildTitledBorder("Opacity"));

    JPanel panelVOIProps = new JPanel(new GridBagLayout());
    panelVOIProps.setBorder(buildTitledBorder("VOI properties"));
    gbc.gridx = 0;
    gbc.gridy = 0;
    gbc.gridwidth = 1;
    gbc.gridheight = 1;
    gbc.weightx = 1;
    gbc.anchor = gbc.WEST;
    gbc.fill = gbc.HORIZONTAL;
    panelVOIProps.add(namePanel, gbc);
    gbc.gridy = 1;
    panelVOIProps.add(checkboxPanel, gbc);
    gbc.gridy = 2;
    gbc.weighty = 1;
    gbc.anchor = gbc.NORTH; //gbc.fill = gbc.BOTH;
    panelVOIProps.add(sliderPanel, gbc);

    listPanel = new JPanelStatisticsList();
    try {
      listPanel.setSliceCount(image.getExtents()[2]);
    }
    catch (ArrayIndexOutOfBoundsException aioobe) {
      // otherwise, this must be a 2d image.
      listPanel.setSliceCount(1);
    }
    finally {
      listPanel.setCheckBoxesEnabled();
    }
    /*        if (voi != null) {
            }
            sPane = new JScrollPane( listPanel,
                        ScrollPaneConstants.VERTICAL_SCROLLBAR_AS_NEEDED,
                        ScrollPaneConstants.HORIZONTAL_SCROLLBAR_AS_NEEDED);
            sPane.setBackground(Color.lightGray);
            sPane.setPreferredSize(new Dimension(190, 130));
     */
    checkboxExclude = new JCheckBox("Exclude intensity range");
    checkboxExclude.setFont(serif12);
    checkboxExclude.addActionListener(this);

    labelMin = new JLabel("Range: ");
    labelMin.setFont(serif12);
    labelMin.setForeground(Color.black);
    labelMin.setEnabled(false);

    textMin = new JTextField(5);
    textMin.setEnabled(false);
    textMin.setFont(serif12);

    labelMax = new JLabel(" to ");
    labelMax.setFont(serif12);
    labelMax.setForeground(Color.black);
    labelMax.setEnabled(false);

    textMax = new JTextField(5);
    textMax.setEnabled(false);
    textMax.setFont(serif12);

    JPanel checkPanel = new JPanel(new FlowLayout(FlowLayout.LEFT));
    checkPanel.add(checkboxExclude);

    JPanel rangePanel = new JPanel(new FlowLayout(FlowLayout.LEFT));

    rangePanel.add(labelMin);
    rangePanel.add(textMin);
    rangePanel.add(labelMax);
    rangePanel.add(textMax);

    JPanel intensityPanel = new JPanel();
    intensityPanel.setLayout(new BoxLayout(intensityPanel, BoxLayout.Y_AXIS));
    intensityPanel.add(checkPanel);
    intensityPanel.add(rangePanel);

    statsPanel = new JPanel(new BorderLayout());
    statsPanel.add(listPanel);
    statsPanel.add(intensityPanel, BorderLayout.SOUTH);

    JLabel labelSeed = new JLabel("Seed value (0-32K)");
    labelSeed.setFont(serif12);
    labelSeed.setForeground(Color.black);

    seedValueTF = new JTextField(5);
    seedValueTF.setFont(serif12);
    seedValueTF.addFocusListener(this);

    JPanel seedValuePanel = new JPanel(new FlowLayout(FlowLayout.LEFT));
    seedValuePanel.setBorder(buildTitledBorder("Watershed seed value"));
    seedValuePanel.add(labelSeed);
    seedValuePanel.add(seedValueTF);

    JPanel calcPanel = new JPanel(new BorderLayout());
    calcPanel.add(statsPanel);
    calcPanel.add(seedValuePanel, BorderLayout.SOUTH);

    applyButton = new JButton("Apply");
    applyButton.setPreferredSize(MipavUtil.defaultButtonSize);
    applyButton.setFont(serif12B);
    applyButton.addActionListener(this);

    cancelButton = buildCancelButton();
    cancelButton.setPreferredSize(MipavUtil.defaultButtonSize);

    JPanel leftButton = new JPanel();
    leftButton.add(applyButton);
    leftButton.add(cancelButton);

    JPanel leftPanel = new JPanel(new BorderLayout());
    leftPanel.add(panelVOIProps);
    leftPanel.add(leftButton, BorderLayout.SOUTH);

    calcButton = new JButton("Calculate");
    calcButton.setPreferredSize(new Dimension(100, 30));
    calcButton.setFont(serif12B);
    calcButton.addActionListener(this);

    JPanel rightButton = new JPanel();
    rightButton.add(calcButton);

    JPanel rightPanel = new JPanel(new BorderLayout());
    rightPanel.add(calcPanel);
    rightPanel.add(rightButton, BorderLayout.SOUTH);

    mainDialogPanel.setLayout(new BorderLayout());
    mainDialogPanel.add(leftPanel, BorderLayout.WEST);
    mainDialogPanel.add(rightPanel);
    mainDialogPanel.setBorder(BorderFactory.createEmptyBorder(5, 5, 5, 5));
    getContentPane().add(mainDialogPanel);
    pack();
  }

  /**
   *   Updates the dialog based on the VOI passed in.
   *   @param _voi VOI whose properties we want to calculate.
   *   @param img  Image where voi is to be updated
   */
  public void updateVOI(VOI _voi, ModelImage img) {

    voi = _voi;
    image = img;

    if (voi.getBoundingBoxFlag() == true) {
      checkboxBoundingBox.setSelected(true);
    }
    else {
      checkboxBoundingBox.setSelected(false);
    }

    if (voi.getPolarity() == voi.ADDITIVE) {
      checkboxAdditiveOrSubtractive.setSelected(true);
    }
    else {
      checkboxAdditiveOrSubtractive.setSelected(false);
    }
    checkboxIncludeForProcessing.setSelected(voi.getProcess());

    if (voi.getDisplayMode() == VOI.BOUNDARY) {
      checkboxBoundary.setSelected(true);
      opacitySlider.setEnabled(false);
    }
    else {
      checkboxBoundary.setSelected(false);
      opacitySlider.setEnabled(true);
    }

    //VOIName.setBackground(voi.getColor());
    colorButton.setBackground(voi.getColor());
    colorVOI = voi.getColor();

    seedValueTF.setText(String.valueOf(voi.getWatershedID()));

    VOIName.setText(voi.getName());
    setTitle("VOI Statistics - " + voi.getUID());

    //turn things on/off depending on if a PolyLine is selected
    if (voi.getCurveType() == VOI.POLYLINE) {
        listPanel.setCheckBoxesDisabled();
        calcButton.setEnabled(false);
        if (checkboxExclude.isSelected()) {
            textMin.setEnabled(false);
            textMax.setEnabled(false);
        }
        checkboxExclude.setEnabled(false);
        seedValueTF.setEnabled(false);
    } else {
        checkboxExclude.setEnabled(true);
        if (checkboxExclude.isSelected()) {
            textMin.setEnabled(true);
            textMax.setEnabled(true);
        }
        seedValueTF.setEnabled(true);
        calcButton.setEnabled(true);
        // on the asssumption that the image is a 3d image, this will work
        try {
            listPanel.setSliceCount(img.getExtents()[2]);
        }
        catch (ArrayIndexOutOfBoundsException aioobe) {
            // otherwise, this must be a 2d image.
            listPanel.setSliceCount(1);
        }
        finally {

            listPanel.setCheckBoxesEnabled();
        }
    }
    validate();
  }

  /**
   *   Applies changes to VOI when "Apply" is pressed; closes when
   *   "Cancel" is pressed; and calculates statistics and outputs them
   *   to the message frame when "Calculate" is pressed.  Also brings
   *   up a color chooser when the color button is pressed.
   *   @param event    Event that triggers this function
   */
  public void actionPerformed(ActionEvent event) {
    Object source = event.getSource();
    String tmpStr;
    int xUnits = image.getFileInfo(0).getUnitsOfMeasure()[0];
    int yUnits = image.getFileInfo(0).getUnitsOfMeasure()[1];
    String unitsString = null;

    if (source == colorButton) {
      colorChooser = new ViewJColorChooser(new Frame(),
                                           "Pick VOI color",
                                           new OkColorListener(),
                                           new CancelListener());
    }
    else if (source == applyButton) {
      ViewVOIVector vectorVOI = image.getVOIs();
      ViewVOIVector newVOIVector;
      int temp[];
      int j = 0, location = -1;
      String name = "";
      boolean changedName = false;

      tmpStr = seedValueTF.getText();
      if (testParameter(tmpStr, 0, 32000)) {
        seedValue = Short.valueOf(tmpStr).shortValue();
        voi.setWatershedID(seedValue);
      }

      try {
        newVOIVector = new ViewVOIVector();
        temp = new int[100];
      }
      catch (OutOfMemoryError error) {
        MipavUtil.displayError("Out of memory: JDialogVOIStats.actionPerformed");
        return;
      }

      for (int i = 0; i < vectorVOI.size(); i++) {
        if ( ( (VOI) vectorVOI.elementAt(i)).getName().equals(VOIName.getText()) &&
            ! ( (VOI) vectorVOI.elementAt(i)).equals(voi)) {
          newVOIVector.addElement(vectorVOI.elementAt(i));
          temp[j++] = i;
          name = VOIName.getText();
          changedName = true;
        }
        if ( ( (VOI) vectorVOI.elementAt(i)).equals(voi)) {
          location = i;
        }
      }
      voi.setName(VOIName.getText());

      if (checkboxBoundingBox.isSelected() == true) {
        voi.setBoundingBoxFlag(true);
      }
      else {
        voi.setBoundingBoxFlag(false);
      }

      if (checkboxAdditiveOrSubtractive.isSelected() == true) {
        voi.setPolarity(VOI.ADDITIVE);
      }
      else {
        voi.setPolarity(VOI.SUBTRACTIVE);
      }

      if (checkboxIncludeForProcessing.isSelected() == true) {
        voi.setProcess(true);
      }
      else {
        voi.setProcess(false);
      }

      if (checkboxBoundary.isSelected() == true) {
        voi.setDisplayMode(VOI.BOUNDARY);
      }
      else {
        voi.setDisplayMode(VOI.SOLID);
      }

      voi.setColor(colorVOI);
      if (parentFrame != null && parentFrame instanceof ViewJFrameImage) {
        ((ViewJFrameImage)parentFrame).getControls().setVOIColor(colorVOI);
      }
      if (j != 0 && location != -1) {

        newVOIVector.addElement(voi);
        temp[j++] = location;

        int where[] = new int[j];
        for (int i = 0; i < j; i++)
          where[i] = temp[i];
        image.groupVOIs(newVOIVector, where, name);
        Vector VOIs = image.getVOIs();
        updateVOI( (VOI) (VOIs.elementAt(VOIs.size() - 1)), image);
      }
      else {
        updateVOI(voi, image);
      }
      image.notifyImageDisplayListeners(null, true);
    }
    else if (source == calcButton) {
      if (textMin.isEnabled()) {
        String tempStr = textMin.getText();
        if (testParameter(tempStr, -10000000, 10000000)) {
          voi.setMinimumIgnore(Float.valueOf(tempStr).floatValue());
        }
        else {
          textMin.requestFocus();
          textMin.selectAll();
          return;
        }
        tempStr = textMax.getText();
        if (testParameter(tempStr, -10000000, 10000000)) {
          voi.setMaximumIgnore(Float.valueOf(tempStr).floatValue());
        }
        else {
          textMax.requestFocus();
          textMax.selectAll();
          return;
        }
      }
      else {
        voi.setMinimumIgnore(Float.MAX_VALUE);
        voi.setMaximumIgnore(Float.MIN_VALUE);
      }

      voi.setStatisticList(listPanel.getViewList());
      // only loading the image works because we have been changing
      // the thing held BY the image.
      algoVOI = new AlgorithmVOIProps(image);
      algoVOI.setActiveImage(false);
      if (!( (ViewJFrameImage) (parentFrame)).getUserInterface().isAppFrameVisible()) {
        algoVOI.setProgressBarVisible(false);
      }
      algoVOI.run();
      if (!algoVOI.isCompleted()) {
        MipavUtil.displayError("Please make sure VOI is selected.");
        return;
      }
      ViewUserInterface UI = ( (ViewJFrameImage) (parentFrame)).getUserInterface();
      UI.setDataText("\n -----------------------------------------------------------------------------\n");
      //UI.setDataText("Image:     " + image.getFileInfo(0).getFileName() + "\n");
      UI.setDataText("Image:     " + image.getImageName() + "\n");
      UI.setDataText("VOI  :     " + voi.getName() + "\n");
      int measure;
      String str = new String();

      /*
                   if ( image.isColorImage() ) {
          if (algoVOI.getMaxIntensityRed() > algoVOI.getMinIntensityRed()) {
           UI.setDataText("  Red min: " + algoVOI.getMinIntensityRed() + " \t\tRed max: " +
           algoVOI.getMaxIntensityRed() + "\n");
          }
           if (algoVOI.getMaxIntensityGreen() > algoVOI.getMinIntensityGreen()) {
              UI.setDataText("  Green min: " + algoVOI.getMinIntensityGreen() + " \tGreen max: " +
           algoVOI.getMaxIntensityGreen() + "\n");
          }
          if (algoVOI.getMaxIntensityBlue() > algoVOI.getMinIntensityBlue()) {
              UI.setDataText("  Blue min: " + algoVOI.getMinIntensityBlue() + " \t\tBlue max: " + algoVOI.getMaxIntensityBlue() + "\n");
          }
                   }
                   else {
          if (algoVOI.getMaxIntensity() > algoVOI.getMinIntensity()) {
              UI.setDataText("  Min: " + algoVOI.getMinIntensity() + " \t\tMax: " + algoVOI.getMaxIntensity() + "\n");
          }
                   }
       */

      //Only if selected
      ViewList statsList[] = voi.getStatisticList();
      for (int i = 0; i < statsList.length; i++) {
        if (statsList[i].getState() == true) {
          if (statsList[i].getString().equals(algoVOI.
                                              makeStatisticListDescriptions()[0])) {
            UI.setDataText("  No. of Voxels                \t= " +
                           algoVOI.getNVoxels() + "\n");
          }
          else if (statsList[i].getString().equals(algoVOI.
              makeStatisticListDescriptions()[1])) {
            //might want to double check to ensure all three units are the same if not display a tag
            str = image.getFileInfo(0).getVolumeUnitsOfMeasureStr();
            UI.setDataText("  Volume                       \t= " +
                           algoVOI.getVolume() + "   " +
                           str + "\n");
          }
          else if (statsList[i].getString().equals(algoVOI.
              makeStatisticListDescriptions()[2])) {
            //might want to double check to ensure all three units are the same if not display a tag
            str = image.getFileInfo(0).getAreaUnitsOfMeasureStr();
            UI.setDataText("  Area                         \t= " +
                           algoVOI.getArea() + "   " +
                           str + "\n");
          }
          else if (statsList[i].getString().equals(algoVOI.
              makeStatisticListDescriptions()[3])) {
              str = FileInfoBase.getUnitsOfMeasureAbbrevStr(image.getFileInfo(0).getUnitsOfMeasure(0));
              UI.setDataText("  Perimeter                         \t= " +
                           algoVOI.getPerimeter() + "   " + str + "\n");
          }
          else if (statsList[i].getString().equals(algoVOI.
              makeStatisticListDescriptions()[4])) {
            if (image.isColorImage()) {
              UI.setDataText("  Red min:   \t\t= " + algoVOI.getMinIntensityRed() +
                             "\n");

              UI.setDataText("  Green min: \t\t= " +
                             algoVOI.getMinIntensityGreen() + "\n");

              UI.setDataText("  Blue min: \t\t= " + algoVOI.getMinIntensityBlue() +
                             "\n");
            }
            else {
              UI.setDataText("  Min: \t\t= " + algoVOI.getMinIntensity() + "\n");
            }
          }
          else if (statsList[i].getString().equals(algoVOI.
              makeStatisticListDescriptions()[5])) {
            if (image.isColorImage()) {
              UI.setDataText("  Red max:   \t\t= " + algoVOI.getMaxIntensityRed() +
                             "\n");

              UI.setDataText("  Green max: \t\t= " +
                             algoVOI.getMaxIntensityGreen() + "\n");

              UI.setDataText("  Blue max:  \t\t= " +
                             algoVOI.getMaxIntensityBlue() + "\n");
            }
            else {
              UI.setDataText("  Max: \t\t= " + algoVOI.getMaxIntensity() + "\n");
            }
          }

          else if (statsList[i].getString().equals(algoVOI.
              makeStatisticListDescriptions()[6])) {
            if (image.isColorImage()) {
              UI.setDataText("  Average voxel intensity      \t= " +
                             algoVOI.getAvgIntenR() + " R, " +
                             algoVOI.getAvgIntenG() + " G, " +
                             algoVOI.getAvgIntenB() + " B, " + "\n");
            }
            else {
              UI.setDataText("  Average voxel intensity      \t= " +
                             algoVOI.getAvgInten() + "\n");
            }
          }
          else if (statsList[i].getString().equalsIgnoreCase(algoVOI.
              makeStatisticListDescriptions()[7])) {
            if (image.isColorImage()) {
              UI.setDataText("  Std. dev. of voxel intensity \t= " +
                             algoVOI.getStdDevR() + " R, " +
                             algoVOI.getStdDevG() + " G, " +
                             algoVOI.getStdDevB() + " B, " + "\n");
            }
            else {
              UI.setDataText("  Std. dev. of voxel intensity \t= " +
                             algoVOI.getStdDev() + "\n");
            }
          }
          else if (statsList[i].getString().equalsIgnoreCase(algoVOI.
              makeStatisticListDescriptions()[8])) {
            UI.setDataText("  Center of Mass               \t= " +
                           algoVOI.getCenterOfMass() + "\n");
          }
          else if (statsList[i].getString().equals(algoVOI.
              makeStatisticListDescriptions()[9])) {
            UI.setDataText("  Principal axis (only 2D)     \t= " +
              algoVOI.getPrincipalAxis() + "  degrees\n");
          }
          else if (statsList[i].getString().equals(algoVOI.
              makeStatisticListDescriptions()[10])) {
            UI.setDataText("  Eccentricity (only 2D)       \t= " +
                           algoVOI.getEccentricity() + "\n");
          }
          else if (statsList[i].getString().equals(algoVOI.
              makeStatisticListDescriptions()[11])) {
            if ((xUnits == yUnits) && (xUnits != FileInfoBase.UNKNOWN_MEASURE)) {
              unitsString = FileInfoBase.getUnitsOfMeasureStr(xUnits);
            }
            if (unitsString != null) {
              UI.setDataText("  Major axis length (only 2D)   \t= " +
                             algoVOI.getMajorAxis() +
                             " " + unitsString +"\n");
            }
            else {
              UI.setDataText("  Major axis length (only 2D)     \t= " +
                       algoVOI.getMajorAxis() + "\n");
            }
          }
          else if (statsList[i].getString().equals(algoVOI.
              makeStatisticListDescriptions()[12])) {
            if ((xUnits == yUnits) && (xUnits != FileInfoBase.UNKNOWN_MEASURE)) {
              unitsString = FileInfoBase.getUnitsOfMeasureStr(xUnits);
            }
            if (unitsString != null) {
              UI.setDataText("  Minor axis length (only 2D)   \t= " +
                             algoVOI.getMinorAxis() +
                             " " + unitsString +"\n");
            }
            else {
              UI.setDataText("  Minor axis length (only 2D)     \t= " +
                       algoVOI.getMinorAxis() + "\n");
            }
          }
        }
      }
    }
    else if (source == checkboxExclude) {
      boolean flag = checkboxExclude.isSelected();
      labelMax.setEnabled(flag);
      labelMin.setEnabled(flag);
      textMin.setEnabled(flag);
      textMax.setEnabled(flag);
    }
    else if (source == cancelButton) {
      cancelFlag = true;
      setVisible(false);
    }
  }

  //*******************************************************************
   //************************* Item Events ****************************
    //*******************************************************************

     /**
      *   Sets opacity slider to enabled or disabled depending on
      *   boundary checkbox.
      *   @param event    Event that cause the method to fire
      */
     public void itemStateChanged(ItemEvent event) {
       Object source = event.getSource();

       if (source == checkboxBoundary) {
         if (checkboxBoundary.isSelected()) {
           opacitySlider.setEnabled(false);
         }
         else {
           opacitySlider.setEnabled(true);
         }
       }
     }

  public void showColorChooser() {
    colorChooser = new ViewJColorChooser(new Frame(),
                                           "Pick VOI color",
                                           new OkColorListener(),
                                           new CancelListener());
  }

  //*******************************************************************
   //************************* Change Events ****************************
    //*******************************************************************

     /**
      *    Sets values based on knob along slider
      *    @param e    Event that triggered this function
      */
     public void stateChanged(ChangeEvent e) {
       Object source = e.getSource();

       if (source == opacitySlider) {
         current.setText(String.valueOf(opacitySlider.getValue() / (float) 100));
         if (voi != null) {
           voi.setOpacity(opacitySlider.getValue() / (float) 100);
         }
       }
     }

  /** responds to the volume of interest
   *   (<code>VOI</code>) change events.
   *   <p>
   *   This method calls <code>updateVOI</code>
   *   using the <code>UpdateVOIEvent</code>
   *   changed <code>VOI</code>, and retrieves
   *   the activeImage out of the current image's
   *   frame.
   *
   *   @see UpdateVOIEvent
   *   @see #updateVOI
   *   @see ViewJFrameBase#getActiveImage
   */
  public void selectionChanged(UpdateVOIEvent newVOIselection) {
    updateVOI(newVOIselection.getChangedVolumeOfInterest(),
              image.getParentFrame().getComponentImage().getActiveImage());
  }

  /**
   *    Pick up the selected color and call method to change the VOI color
   *
   */
  class OkColorListener
      implements ActionListener {

    /**
     *   Get color from chooser and set button
     *   and VOI color.
     *   @param e    Event that triggered function.
     */
    public void actionPerformed(ActionEvent e) {
      Color color = colorChooser.getColor();
      colorButton.setBackground(color);
      colorVOI = color;
    }
  }

  /**
   *    Does nothing.
   */
  class CancelListener
      implements ActionListener {

    /**
     *   Does nothing.
     */
    public void actionPerformed(ActionEvent e) {
    }
  }

  //*******************************************************************
   //************************* Focus Events ****************************
    //*******************************************************************

     /**
      *   Test the seed value and if appropriate, sets it.
      *   @param event    Event that triggered function.
      */
     public void focusLost(FocusEvent event) {

       String tmpStr = seedValueTF.getText();
       if (testParameter(tmpStr, 0, 32000)) {
         seedValue = Short.valueOf(tmpStr).shortValue();
         voi.setWatershedID(seedValue);
       }

     }

}
