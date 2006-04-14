package gov.nih.mipav.view.dialogs;

import gov.nih.mipav.view.*;
import gov.nih.mipav.model.structures.*;
import gov.nih.mipav.model.file.*;
import gov.nih.mipav.model.algorithms.*;
import gov.nih.mipav.model.algorithms.registration.*;

import java.awt.event.*;
import java.awt.*;
import javax.swing.*;
import java.text.DecimalFormat;

public class JDialogRegistrationAIR
    extends JDialogBase
    implements AlgorithmInterface, ScriptableInterface {

  private ModelImage matchImage; //register source image to refImage
  private ModelImage refImage;
  private ModelImage resultImage = null; // result image

  private JComboBox comboBoxDOF, comboBoxInterp, comboBoxCostFunct,
      comboBoxPart;
  private JCheckBox cubicInterpolationCheckBox;
  private boolean cubicInterpolation = false;
  private JCheckBox interactionCheckBox;
  private boolean interaction = true;
  private JCheckBox sourceVOICheckBox;
  private JCheckBox targetVOICheckBox;
  private boolean entireSource = true;
  private boolean entireTarget = true;
  private JComboBox comboBoxImage;

  private ViewUserInterface UI;
  private AlgorithmRegAIR air = null;

  private int transformation;
  private int interpolation; // NEAREST_NEIGHBOR or CUBIC_SPLINE

  private JTextField textSourceThreshold;
  private JTextField textTargetThreshold;
  private JLabel labelTargetThreshold;
  private float sourceThreshold;
  private float targetThreshold;
  private JTextField textSourceGaussX;
  private JTextField textSourceGaussY;
  private JTextField textSourceGaussZ;
  private JTextField textTargetGaussX;
  private JTextField textTargetGaussY;
  private JTextField textTargetGaussZ;
  private String selectedName;
  private float sourceGaussX = 0.0f;
  private float sourceGaussY = 0.0f;
  private float sourceGaussZ = 0.0f;
  private float targetGaussX = 0.0f;
  private float targetGaussY = 0.0f;
  private float targetGaussZ = 0.0f;
  private String tmpStr;
  private JCheckBox crossModalityCheckBox;
  private int sourcePartitions;
  private int targetPartitions;
  private int costFxn;
  private int targetXDim, targetYDim, targetZDim;
  private float pixel_size_s;
  private float targetVoxelX, targetVoxelY, targetVoxelZ;
  private float xoom1, yoom1, zoom1;
  private int[] resultExtents;
  private int targetUnitsX, targetUnitsY, targetUnitsZ;
  private boolean doConvert = false;
  private JTextField textPrecision;
  private float precision;
  private JTextField textIterations;
  private int iterations;
  private DecimalFormat nf;

  /**
   *  Creates new registration dialog.
   *  @param theParentFrame Parent frame
   *  @param im             Source image
   */
  public JDialogRegistrationAIR(Frame theParentFrame, ModelImage im) {
    super(theParentFrame, true);
    matchImage = im;
    UI = ( (ViewJFrameBase) (parentFrame)).getUserInterface();
    nf = new DecimalFormat("####0.0####");
    init();
  }

  /**
   *	Used primarily for the script to store variables and run the algorithm.  No
   *	actual dialog will appear but the set up info and result image will be stored here.
   *	@param _UI   The user interface, needed to create the image frame.
   *	@param im	Source image.
   */
  public JDialogRegistrationAIR(ViewUserInterface _UI, ModelImage im) {
    super();
    UI = _UI;
    matchImage = im;
  }

  /**
   * Empty constructor needed for dynamic instantiation (used during scripting).
   */
  public JDialogRegistrationAIR() {}

  /**
   * Run this algorithm from a script.
   * @param parser the script parser we get the state from
   * @throws IllegalArgumentException if there is something wrong with the arguments in the script
   */
  public void scriptRun (AlgorithmScriptParser parser) throws IllegalArgumentException {
      String srcImageKey = null;
      String destImageKey = null;
      String image2Key = null;

      try {
          srcImageKey = parser.getNextString();
          image2Key = parser.getNextString();
      } catch (Exception e) {
          throw new IllegalArgumentException();
      }
      ModelImage im = parser.getImage(srcImageKey);
      ModelImage im2 = parser.getImage(image2Key);

      matchImage = im;
      UI = matchImage.getUserInterface();
      parentFrame = matchImage.getParentFrame();
      setRefImage(im2);

      // the result image
      try {
          destImageKey = parser.getNextString();
      } catch (Exception e) {
          throw new IllegalArgumentException();
      }

      try {
          setEntireSource(parser.getNextBoolean());
          setEntireTarget(parser.getNextBoolean());
          setTransformation(parser.getNextInteger());
          setInterpolation(parser.getNextInteger());
          setSourceThreshold(parser.getNextFloat());
          setTargetThreshold(parser.getNextFloat());
          setSourceGaussX(parser.getNextFloat());
          setSourceGaussY(parser.getNextFloat());
          setSourceGaussZ(parser.getNextFloat());
          setTargetGaussX(parser.getNextFloat());
          setTargetGaussY(parser.getNextFloat());
          setTargetGaussZ(parser.getNextFloat());
          setSourcePartitions(parser.getNextInteger());
          setTargetPartitions(parser.getNextInteger());
          setCostFunction(parser.getNextInteger());
          setCubic(parser.getNextBoolean());
          setInteraction(parser.getNextBoolean());
          setPrecision(parser.getNextFloat());
          setIterations(parser.getNextInteger());
      } catch (Exception e) {
          throw new IllegalArgumentException();
      }

      setActiveImage(parser.isActiveImage());
      setSeparateThread(false);
      callAlgorithm();
      parser.putVariable(destImageKey, getResultImage().getImageName());
  }

  /**
   * If a script is being recorded and the algorithm is done, add an entry for this algorithm.
   * @param algo the algorithm to make an entry for
   */
  public void insertScriptLine (AlgorithmBase algo) {
      if (algo.isCompleted()) {
          if (UI.isScriptRecording()) {
              //check to see if the match image is already in the ImgTable
              if (UI.getScriptDialog().getImgTableVar(matchImage.getImageName()) == null) {
                  if (UI.getScriptDialog().getActiveImgTableVar(matchImage.getImageName()) == null) {
                      UI.getScriptDialog().putActiveVar(matchImage.getImageName());
                  }
              }
              //check to see if the ref image is already in the ImgTable
              if (UI.getScriptDialog().getImgTableVar(refImage.getImageName()) == null) {
                  if (UI.getScriptDialog().getActiveImgTableVar(refImage.getImageName()) == null) {
                      UI.getScriptDialog().putActiveVar(refImage.getImageName());
                  }
              }

              UI.getScriptDialog().putVar(resultImage.getImageName());
              UI.getScriptDialog().append("RegistrationAIR ");
              UI.getScriptDialog().append(UI.getScriptDialog().getVar(matchImage.
                                                                      getImageName()) + " " +
                                          UI.getScriptDialog().getVar(refImage.
                                                                      getImageName()) + " " +
                                          UI.getScriptDialog().getVar(resultImage.
                                                                      getImageName()) + " " +
                                          entireSource + " " + entireTarget + " " +
                                          transformation + " " +
                                          interpolation + " " + sourceThreshold +
                                          " " + targetThreshold + " " +
                                          sourceGaussX + " " + sourceGaussY + " " +
                                          sourceGaussZ + " " +
                                          targetGaussX + " " + targetGaussY + " " +
                                          targetGaussZ + " " +
                                          sourcePartitions + " " +
                                          targetPartitions + " " + costFxn + " " +
                                          cubicInterpolation + " " + interaction +
                                          " " + precision + " " +
                                          iterations + "\n");
          }
      }
  }

  /**
   *	Initializes the GUI components and displays the dialog.
   */
  private void init() {
    setForeground(Color.black);
    setTitle("AIR Registration");

    String matchName = matchImage.getImageName();
    JLabel labelImage = new JLabel("Register [" + matchName + "] to:");
    labelImage.setForeground(Color.black);
    labelImage.setFont(serif12);
    labelImage.setAlignmentX(Component.LEFT_ALIGNMENT);

    comboBoxImage = buildImageComboBox(matchImage);
    comboBoxImage.addItemListener(this);

    JLabel labelDOF = new JLabel("Degrees of freedom:");
    labelDOF.setForeground(Color.black);
    labelDOF.setFont(serif12);
    labelDOF.setAlignmentX(Component.LEFT_ALIGNMENT);

    comboBoxDOF = new JComboBox();
    comboBoxDOF.setFont(MipavUtil.font12);
    comboBoxDOF.setBackground(Color.white);
    comboBoxDOF.setToolTipText("Degrees of freedom");
    if (matchImage.getNDims() == 2) {
      comboBoxDOF.addItem("Affine - 6");
      comboBoxDOF.addItem("Global rescale - 4");
      comboBoxDOF.addItem("Rigid - 3");
      comboBoxDOF.addItem("Specific rescale - 5");
    }
    else {
      comboBoxDOF.addItem("Affine - 12");
      comboBoxDOF.addItem("Global rescale - 7");
      comboBoxDOF.addItem("Rigid - 6");
      comboBoxDOF.addItem("Specific rescale - 9");
    }
    comboBoxDOF.setSelectedIndex(3);

    JLabel labelInterp = new JLabel("Interpolation:");
    labelInterp.setForeground(Color.black);
    labelInterp.setFont(serif12);
    labelInterp.setAlignmentX(Component.LEFT_ALIGNMENT);

    comboBoxInterp = new JComboBox();
    comboBoxInterp.setFont(serif12);
    comboBoxInterp.setBackground(Color.white);
    comboBoxInterp.setAlignmentX(Component.LEFT_ALIGNMENT);

    comboBoxInterp.addItem("Linear");
    comboBoxInterp.addItem("Windowed sinc");
    comboBoxInterp.addItem("Nearest Neighbor");

    if (matchImage.getNDims() == 2) {
      cubicInterpolationCheckBox = new JCheckBox(
          "Interpolation to square pixels");
    }
    else {
      cubicInterpolationCheckBox = new JCheckBox(
          "Interpolation to cubic voxels");
    }
    cubicInterpolationCheckBox.setFont(serif12);
    cubicInterpolationCheckBox.setEnabled(true);
    cubicInterpolationCheckBox.setSelected(false);
    cubicInterpolationCheckBox.addActionListener(this);

    interactionCheckBox = new JCheckBox(
        "Spatial parameter derivatives interact");
    interactionCheckBox.setFont(serif12);
    interactionCheckBox.setEnabled(true);
    interactionCheckBox.setSelected(true);
    interactionCheckBox.addActionListener(this);

    JPanel optPanel = new JPanel(new GridBagLayout());
    optPanel.setBorder(buildTitledBorder("Transformation Options"));

    Insets insets = new Insets(0, 2, 0, 2);
    GridBagConstraints gbc = new GridBagConstraints();

    gbc.gridx = 0;
    gbc.gridy = 0;
    gbc.gridwidth = 1;
    gbc.gridheight = 1;
    gbc.insets = insets;
    gbc.weightx = 0;
    gbc.fill = gbc.NONE;
    gbc.anchor = gbc.WEST;
    optPanel.add(labelImage, gbc);
    gbc.gridx = 1;
    gbc.weightx = 1;
    gbc.fill = gbc.HORIZONTAL;
    optPanel.add(comboBoxImage, gbc);

    gbc.gridx = 0;
    gbc.gridy = 2;
    gbc.weightx = 0;
    gbc.fill = gbc.NONE;
    optPanel.add(labelDOF, gbc);
    gbc.gridx = 1;
    gbc.gridy = 2;
    gbc.weightx = 1;
    gbc.fill = gbc.HORIZONTAL;
    optPanel.add(comboBoxDOF, gbc);

    gbc.gridx = 0;
    gbc.gridy = 3;
    gbc.weightx = 0;
    gbc.fill = gbc.NONE;
    optPanel.add(labelInterp, gbc);
    gbc.gridx = 1;
    gbc.gridy = 3;
    gbc.weightx = 1;
    gbc.fill = gbc.HORIZONTAL;
    optPanel.add(comboBoxInterp, gbc);

    gbc.gridx = 0;
    gbc.gridy = 4;
    gbc.weightx = 1;
    gbc.gridwidth = 2;
    optPanel.add(cubicInterpolationCheckBox, gbc);

    gbc.gridy = 5;
    optPanel.add(interactionCheckBox, gbc);

    crossModalityCheckBox = new JCheckBox("1 PET & 1 MRI");
    crossModalityCheckBox.setFont(serif12);
    crossModalityCheckBox.setEnabled(true);
    crossModalityCheckBox.setSelected(false);
    crossModalityCheckBox.addActionListener(this);

    JLabel labelPart = new JLabel("Type of partition:");
    labelPart.setForeground(Color.black);
    labelPart.setFont(serif12);
    labelPart.setAlignmentX(Component.LEFT_ALIGNMENT);

    comboBoxPart = new JComboBox();
    comboBoxPart.setFont(serif12);
    comboBoxPart.setBackground(Color.white);
    comboBoxPart.setAlignmentX(Component.LEFT_ALIGNMENT);

    comboBoxPart.addItem("Bidirectional");
    comboBoxPart.addItem("Forward fit");
    comboBoxPart.addItem("Reverse fit");
    comboBoxPart.addItemListener(this);

    JLabel labelCost = new JLabel("Cost function:");
    labelCost.setForeground(Color.black);
    labelCost.setFont(serif12);
    labelCost.setAlignmentX(Component.LEFT_ALIGNMENT);

    comboBoxCostFunct = new JComboBox();
    comboBoxCostFunct.setFont(MipavUtil.font12);
    comboBoxCostFunct.setBackground(Color.white);
    comboBoxCostFunct.setToolTipText("Cost function");
    comboBoxCostFunct.addItem("Std dev of ratio");
    comboBoxCostFunct.addItem("Least squares");
    comboBoxCostFunct.addItem("Scaled least squares");
    comboBoxCostFunct.addItem("Correlation ratio");
    comboBoxCostFunct.setSelectedIndex(0);

    JPanel partPanel = new JPanel(new GridBagLayout());
    partPanel.setBorder(buildTitledBorder("Setting partitions"));

    gbc.gridx = 0;
    gbc.gridy = 0;
    gbc.gridwidth = 2;
    gbc.gridheight = 1;
    gbc.insets = insets;
    gbc.weightx = 1;
    gbc.fill = gbc.HORIZONTAL;
    gbc.anchor = gbc.WEST;
    partPanel.add(crossModalityCheckBox, gbc);

    gbc.gridx = 0;
    gbc.gridy = 1;
    gbc.weightx = 0;
    gbc.fill = gbc.NONE;
    gbc.gridwidth = 1;
    partPanel.add(labelPart, gbc);
    gbc.gridx = 1;
    gbc.gridy = 1;
    gbc.weightx = 1;
    gbc.fill = gbc.HORIZONTAL;
    partPanel.add(comboBoxPart, gbc);

    gbc.gridx = 0;
    gbc.gridy = 2;
    gbc.weightx = 0;
    gbc.fill = gbc.NONE;
    gbc.gridwidth = 1;
    partPanel.add(labelCost, gbc);
    gbc.gridx = 1;
    gbc.gridy = 2;
    gbc.weightx = 1;
    gbc.fill = gbc.HORIZONTAL;
    partPanel.add(comboBoxCostFunct, gbc);

    sourceVOICheckBox = new JCheckBox("Apply only to VOI region in source");
    sourceVOICheckBox.setFont(serif12);
    sourceVOICheckBox.setEnabled(true);
    sourceVOICheckBox.setSelected(false);
    sourceVOICheckBox.addActionListener(this);

    targetVOICheckBox = new JCheckBox("Apply only to VOI region in destination");
    targetVOICheckBox.setFont(serif12);
    targetVOICheckBox.setEnabled(true);
    targetVOICheckBox.setSelected(false);
    targetVOICheckBox.addActionListener(this);

    JPanel VOIPanel = new JPanel(new GridBagLayout());
    VOIPanel.setBorder(buildTitledBorder("VOI region use"));

    gbc.gridx = 0;
    gbc.gridy = 0;
    gbc.gridwidth = 1;
    gbc.gridheight = 1;
    gbc.insets = insets;
    gbc.weightx = 1;
    gbc.fill = gbc.HORIZONTAL;
    gbc.anchor = gbc.WEST;
    VOIPanel.add(sourceVOICheckBox, gbc);
    gbc.gridy = 1;
    VOIPanel.add(targetVOICheckBox, gbc);

    JLabel labelPrecision = new JLabel("Convergence criteria(E-7-E-3)");
    labelPrecision.setForeground(Color.black);
    labelPrecision.setFont(serif12);

    textPrecision = new JTextField(10);
    textPrecision.setText("0.00001");
    textPrecision.setFont(serif12);

    JLabel labelIterations = new JLabel("Maximum Iterations");
    labelIterations.setForeground(Color.black);
    labelIterations.setFont(serif12);

    textIterations = new JTextField(10);
    textIterations.setText("25");
    textIterations.setFont(serif12);

    JPanel iterationsPanel = new JPanel(new GridBagLayout());
    iterationsPanel.setBorder(buildTitledBorder("Iteration parameters"));

    gbc.gridx = 0;
    gbc.gridy = 0;
    gbc.gridwidth = 1;
    gbc.gridheight = 1;
    gbc.insets = insets;
    gbc.weightx = 0;
    gbc.fill = gbc.NONE;
    gbc.anchor = gbc.WEST;
    iterationsPanel.add(labelPrecision, gbc);
    gbc.gridx = 1;
    gbc.weightx = 1;
    gbc.fill = gbc.HORIZONTAL;
    iterationsPanel.add(textPrecision, gbc);

    gbc.gridx = 0;
    gbc.gridy = 1;
    gbc.weightx = 0;
    gbc.fill = gbc.NONE;
    iterationsPanel.add(labelIterations, gbc);
    gbc.gridx = 1;
    gbc.gridy = 1;
    gbc.weightx = 1;
    gbc.fill = gbc.HORIZONTAL;
    iterationsPanel.add(textIterations, gbc);

    JLabel labelSourceThreshold = new JLabel("Source threshold ("
                                             + nf.format(matchImage.getMin()) +
                                             " - " +
                                             nf.format(matchImage.getMax()) +
                                             ")");
    labelSourceThreshold.setForeground(Color.black);
    labelSourceThreshold.setFont(serif12);

    textSourceThreshold = new JTextField(10);
    sourceThreshold = (float) (matchImage.getMin() +
                               0.2f * (matchImage.getMax() - matchImage.getMin()));
    textSourceThreshold.setText(nf.format(sourceThreshold));
    textSourceThreshold.setFont(serif12);

    selectedName = (String) comboBoxImage.getSelectedItem();
    refImage = UI.getRegisteredImageByName(selectedName);
    targetThreshold = (float) (refImage.getMin() +
                               0.2f * (refImage.getMax() - refImage.getMin()));

    labelTargetThreshold = new JLabel("Target threshold ("
                                      + nf.format(refImage.getMin()) + " - " +
                                      nf.format(refImage.getMax()) + ")");
    labelTargetThreshold.setForeground(Color.black);
    labelTargetThreshold.setFont(serif12);

    textTargetThreshold = new JTextField(10);
    textTargetThreshold.setText(nf.format(targetThreshold));
    textTargetThreshold.setFont(serif12);

    JPanel thresholdPanel = new JPanel(new GridBagLayout());
    thresholdPanel.setBorder(buildTitledBorder("Thresholds"));

    gbc.gridx = 0;
    gbc.gridy = 0;
    gbc.gridwidth = 1;
    gbc.gridheight = 1;
    gbc.insets = insets;
    gbc.weightx = 0;
    gbc.fill = gbc.NONE;
    gbc.anchor = gbc.WEST;
    thresholdPanel.add(labelSourceThreshold, gbc);
    gbc.gridx = 1;
    gbc.weightx = 1;
    gbc.fill = gbc.HORIZONTAL;
    thresholdPanel.add(textSourceThreshold, gbc);

    gbc.gridx = 0;
    gbc.gridy = 1;
    gbc.weightx = 1;
    gbc.fill = gbc.HORIZONTAL;
    thresholdPanel.add(labelTargetThreshold, gbc);
    gbc.gridx = 1;
    gbc.gridy = 1;
    gbc.weightx = 1;
    gbc.fill = gbc.HORIZONTAL;
    thresholdPanel.add(textTargetThreshold, gbc);

    JLabel labelSourceGaussX = new JLabel("Source X FWHM");
    labelSourceGaussX.setForeground(Color.black);
    labelSourceGaussX.setFont(serif12);

    textSourceGaussX = new JTextField(10);
    textSourceGaussX.setText("0.0");
    textSourceGaussX.setFont(serif12);

    JLabel labelSourceGaussY = new JLabel("Source Y FWHM");
    labelSourceGaussY.setForeground(Color.black);
    labelSourceGaussY.setFont(serif12);

    textSourceGaussY = new JTextField(10);
    textSourceGaussY.setText("0.0");
    textSourceGaussY.setFont(serif12);

    JLabel labelSourceGaussZ = new JLabel("Source Z FWHM");
    labelSourceGaussZ.setForeground(Color.black);
    labelSourceGaussZ.setFont(serif12);

    textSourceGaussZ = new JTextField(10);
    textSourceGaussZ.setText("0.0");
    textSourceGaussZ.setFont(serif12);

    JLabel labelTargetGaussX = new JLabel("Target X FWHM");
    labelTargetGaussX.setForeground(Color.black);
    labelTargetGaussX.setFont(serif12);

    textTargetGaussX = new JTextField(10);
    textTargetGaussX.setText("0.0");
    textTargetGaussX.setFont(serif12);

    JLabel labelTargetGaussY = new JLabel("Target Y FWHM");
    labelTargetGaussY.setForeground(Color.black);
    labelTargetGaussY.setFont(serif12);

    textTargetGaussY = new JTextField(10);
    textTargetGaussY.setText("0.0");
    textTargetGaussY.setFont(serif12);

    JLabel labelTargetGaussZ = new JLabel("Target Z FWHM");
    labelTargetGaussZ.setForeground(Color.black);
    labelTargetGaussZ.setFont(serif12);

    textTargetGaussZ = new JTextField(10);
    textTargetGaussZ.setText("0.0");
    textTargetGaussZ.setFont(serif12);

    JPanel gaussianPanel = new JPanel(new GridBagLayout());
    gaussianPanel.setBorder(buildTitledBorder("Gaussian FWHMs"));

    gbc.gridx = 0;
    gbc.gridy = 0;
    gbc.gridwidth = 1;
    gbc.gridheight = 1;
    gbc.insets = insets;
    gbc.weightx = 0;
    gbc.fill = gbc.NONE;
    gbc.anchor = gbc.WEST;
    gaussianPanel.add(labelSourceGaussX, gbc);
    gbc.gridx = 1;
    gbc.weightx = 1;
    gbc.fill = gbc.HORIZONTAL;
    gaussianPanel.add(textSourceGaussX, gbc);

    gbc.gridx = 0;
    gbc.gridy = 1;
    gbc.weightx = 0;
    gbc.fill = gbc.NONE;
    gaussianPanel.add(labelSourceGaussY, gbc);
    gbc.gridx = 1;
    gbc.gridy = 1;
    gbc.weightx = 1;
    gbc.fill = gbc.HORIZONTAL;
    gaussianPanel.add(textSourceGaussY, gbc);

    if (matchImage.getNDims() > 2) {
      gbc.gridx = 0;
      gbc.gridy = 2;
      gbc.weightx = 0;
      gbc.fill = gbc.NONE;
      gaussianPanel.add(labelSourceGaussZ, gbc);
      gbc.gridx = 1;
      gbc.gridy = 2;
      gbc.weightx = 1;
      gbc.fill = gbc.HORIZONTAL;
      gaussianPanel.add(textSourceGaussZ, gbc);
    }

    gbc.gridx = 0;
    gbc.gridy = 3;
    gbc.weightx = 0;
    gbc.fill = gbc.NONE;
    gaussianPanel.add(labelTargetGaussX, gbc);
    gbc.gridx = 1;
    gbc.gridy = 3;
    gbc.weightx = 1;
    gbc.fill = gbc.HORIZONTAL;
    gaussianPanel.add(textTargetGaussX, gbc);

    gbc.gridx = 0;
    gbc.gridy = 4;
    gbc.weightx = 0;
    gbc.fill = gbc.NONE;
    gaussianPanel.add(labelTargetGaussY, gbc);
    gbc.gridx = 1;
    gbc.gridy = 4;
    gbc.weightx = 1;
    gbc.fill = gbc.HORIZONTAL;
    gaussianPanel.add(textTargetGaussY, gbc);

    if (matchImage.getNDims() > 2) {
      gbc.gridx = 0;
      gbc.gridy = 5;
      gbc.weightx = 0;
      gbc.fill = gbc.NONE;
      gaussianPanel.add(labelTargetGaussZ, gbc);
      gbc.gridx = 1;
      gbc.gridy = 5;
      gbc.weightx = 1;
      gbc.fill = gbc.HORIZONTAL;
      gaussianPanel.add(textTargetGaussZ, gbc);
    }

    JPanel leftPanel = new JPanel(new GridBagLayout());
    gbc.gridx = 0;
    gbc.gridy = 0;
    gbc.gridwidth = 1;
    gbc.gridheight = 1;
    gbc.insets = insets;
    gbc.weightx = 1;
    gbc.fill = gbc.HORIZONTAL;
    gbc.anchor = gbc.WEST;

    leftPanel.add(optPanel, gbc);
    gbc.gridy = 1;
    leftPanel.add(partPanel, gbc);
    gbc.gridy = 2;
    leftPanel.add(VOIPanel, gbc);

    JPanel rightPanel = new JPanel(new GridBagLayout());
    gbc.gridx = 0;
    gbc.gridy = 0;
    gbc.gridwidth = 1;
    gbc.gridheight = 1;
    gbc.insets = insets;
    gbc.weightx = 1;
    gbc.weighty = 1;
    gbc.fill = gbc.BOTH;
    gbc.anchor = gbc.WEST;

    rightPanel.add(iterationsPanel, gbc);
    gbc.gridy = 1;
    rightPanel.add(thresholdPanel, gbc);
    gbc.gridy = 2;
    rightPanel.add(gaussianPanel, gbc);

    JPanel buttonPanel = new JPanel();
    buildOKButton();
    buildCancelButton();
    buttonPanel.add(OKButton);
    buttonPanel.add(cancelButton);
    getContentPane().add(leftPanel);
    getContentPane().add(rightPanel, BorderLayout.EAST);
    getContentPane().add(buttonPanel, BorderLayout.SOUTH);
    pack();
    setVisible(true);
  }

  /**
   *    Closes dialog box when the OK button is pressed, sets up
   *    the variables needed for running the algorithm, and calls
   *    the algorithm.
   *    @param event    Event that triggers function
   */
  public void actionPerformed(ActionEvent event) {
    int i;
    Object source = event.getSource();

    if (source == OKButton) {
      if (setVariables()) {
        setVisible(false);
        callAlgorithm();
      }
    }
    else if (source == crossModalityCheckBox) {
      if (crossModalityCheckBox.isSelected()) {
        for (i = comboBoxCostFunct.getItemCount() - 1; i >= 1; i--) {
          comboBoxCostFunct.removeItemAt(i);
        }
        comboBoxCostFunct.addItem("Correlation ratio");
        comboBoxCostFunct.setSelectedIndex(0);
      }
      else {
        for (i = comboBoxCostFunct.getItemCount() - 1; i >= 1; i--) {
          comboBoxCostFunct.removeItemAt(i);
        }
        comboBoxCostFunct.addItem("Least squares");
        comboBoxCostFunct.addItem("Scaled least squares");
        comboBoxCostFunct.addItem("Correlation ratio");
      }
    }
    else if (source == cancelButton) {
      dispose();
    }
  }

  /**
   *   Accessor to get the result image.
   *   @return     The result image.
   */
  public ModelImage getResultImage() {
    return resultImage;
  }

  /**
   *   Accessor to set the type of transformation for the
   *   registration.
   *   @param trans    Type of transformation (GLOBAL_RESCALING2D, etc)
   */
  public void setTransformation(int trans) {
    transformation = trans;
  }

  /**
   *   Accessor to set the type of interpolation for the
   *   registration.
   *   @param interp    Type of interpolation (WSINC, etc)
   */
  public void setInterpolation(int interp) {
    interpolation = interp;
  }

  /**
   *   Accessor to set the cubic interpolation flag.
   *   @param flag     <code>true</code> means resample to cubic
   *                   voxels.
   */
  public void setCubic(boolean flag) {
    cubicInterpolation = flag;
  }

  /**
   *   Accessor to set the precision for the registration.
   *   @param precise  Precision
   */
  public void setPrecision(float precise) {
    precision = precise;
  }

  /**
   *   Accessor to set the number of iterations.
   *   @param iter     Number of iterations.
   */
  public void setIterations(int iter) {
    iterations = iter;
  }

  /**
   *   Accessor to set the interaction flag.
   *   @param flag     <code>true</code> means interaction.
   */
  public void setInteraction(boolean flag) {
    interaction = flag;
  }

  /**
   *   Accessor to set the entire source flag.
   *   @param flag     <code>true</code> means use entire source image.
   */
  public void setEntireSource(boolean flag) {
    entireSource = flag;
  }

  /**
   *   Accessor to set the entire target flag.
   *   @param flag     <code>true</code> means use entire target image.
   */
  public void setEntireTarget(boolean flag) {
    entireTarget = flag;
  }

  /**
   *   Accessor to set the source threshold for the registration.
   *   @param thresh   Source threshold
   */
  public void setSourceThreshold(float thresh) {
    sourceThreshold = thresh;
  }

  /**
   *   Accessor to set the target threshold for the registration.
   *   @param thresh   Target threshold
   */
  public void setTargetThreshold(float thresh) {
    targetThreshold = thresh;
  }

  /**
   *   Accessor to set the source Gaussian x for the registration.
   *   @param x        Source Gaussian X parameter.
   */
  public void setSourceGaussX(float x) {
    sourceGaussX = x;
  }

  /**
   *   Accessor to set the source Gaussian y for the registration.
   *   @param x        Source Gaussian Y parameter.
   */
  public void setSourceGaussY(float x) {
    sourceGaussY = x;
  }

  /**
   *   Accessor to set the source Gaussian z for the registration.
   *   @param x        Source Gaussian Z parameter.
   */
  public void setSourceGaussZ(float x) {
    sourceGaussZ = x;
  }

  /**
   *   Accessor to set the target Gaussian x for the registration.
   *   @param x        Target Gaussian X parameter.
   */
  public void setTargetGaussX(float x) {
    targetGaussX = x;
  }

  /**
   *   Accessor to set the target Gaussian y for the registration.
   *   @param x        Target Gaussian Y parameter.
   */
  public void setTargetGaussY(float x) {
    targetGaussY = x;
  }

  /**
   *   Accessor to set the target Gaussian z for the registration.
   *   @param x        Target Gaussian Z parameter.
   */
  public void setTargetGaussZ(float x) {
    targetGaussZ = x;
  }

  /**
   *   Accessor to set the number of source partitions (0, 1, 256).
   *   @param part     Number of source partitions (0, 1, 256).
   */
  public void setSourcePartitions(int part) {
    sourcePartitions = part;
  }

  /**
   *   Accessor to set the number of target partitions (0, 1, 256).
   *   @param part     Number of target partitions (0, 1, 256).
   */
  public void setTargetPartitions(int part) {
    targetPartitions = part;
  }

  /**
   *   Accessor to set the cost function.
   *   @param cost Cost function.
   */
  public void setCostFunction(int cost) {
    costFxn = cost;
  }

  /**
   *   Accessor to set the target image.
   *   @param image    The target image.
   */
  public void setRefImage(ModelImage image) {
    refImage = image;
  }

  /**
   *   Sets up the variables needed for the algorithm from
   *   the GUI components.
   *   @return Flag indicating if the setup was successful.
   */
  private boolean setVariables() {
    // assign source Image to image selected in comboBox
    selectedName = (String) comboBoxImage.getSelectedItem();
    refImage = UI.getRegisteredImageByName(selectedName);

    switch (comboBoxDOF.getSelectedIndex()) {
      case 1:
        if (matchImage.getNDims() == 2) {
          transformation = AlgorithmRegAIR.GLOBAL_RESCALING2D;
        }
        else {
          transformation = AlgorithmRegAIR.GLOBAL_RESCALING3D;
        }
        break;
      case 2:
        if (matchImage.getNDims() == 2) {
          transformation = AlgorithmRegAIR.RIGID_BODY2D;
        }
        else {
          transformation = AlgorithmRegAIR.RIGID_BODY3D;
        }
        break;
      case 3:
        if (matchImage.getNDims() == 2) {
          transformation = AlgorithmRegAIR.FIXED_DETERMINANT2D;
        }
        else {
          transformation = AlgorithmRegAIR.TRADITIONAL3D;
        }
        break;
      case 0:
      default:
        if (matchImage.getNDims() == 2) {
          transformation = AlgorithmRegAIR.AFFINE2D;
        }
        else {
          transformation = AlgorithmRegAIR.AFFINE3D;
        }
        break;
    }
    switch (comboBoxInterp.getSelectedIndex()) {
      case 0:
        if (matchImage.getNDims() == 2) {
          interpolation = AlgorithmTransform.BILINEAR;
        }
        else {
          interpolation = AlgorithmTransform.TRILINEAR;
        }
        break;
      case 1:
        interpolation = AlgorithmTransform.WSINC;
        break;
      case 2:
        interpolation = AlgorithmTransform.NEAREST_NEIGHBOR;
        break;
      default:
        interpolation = AlgorithmTransform.TRILINEAR;
        break;
    }

    if (cubicInterpolationCheckBox.isSelected()) {
      cubicInterpolation = true;
    }
    else {
      cubicInterpolation = false;
    }

    if (interactionCheckBox.isSelected()) {
      interaction = true;
    }
    else {
      interaction = false;
    }

    tmpStr = textPrecision.getText();
    if (testParameter(tmpStr, 1.0e-7, 1.0e-3)) {
      precision = Float.valueOf(tmpStr).floatValue();
    }
    else {
      textPrecision.requestFocus();
      textPrecision.selectAll();
      return false;
    }

    tmpStr = textIterations.getText();
    if (testParameter(tmpStr, 1, 10000)) {
      iterations = Integer.valueOf(tmpStr).intValue();
    }
    else {
      textIterations.requestFocus();
      textIterations.selectAll();
      return false;
    }

    if (sourceVOICheckBox.isSelected()) {
      entireSource = false;
    }
    else {
      entireSource = true;
    }

    if (targetVOICheckBox.isSelected()) {
      entireTarget = false;
    }
    else {
      entireTarget = true;
    }

    tmpStr = textSourceThreshold.getText();
    if (testParameter(tmpStr, matchImage.getMin(), matchImage.getMax())) {
      sourceThreshold = Float.valueOf(tmpStr).floatValue();
    }
    else {
      textSourceThreshold.requestFocus();
      textSourceThreshold.selectAll();
      return false;
    }

    tmpStr = textTargetThreshold.getText();
    if (testParameter(tmpStr, refImage.getMin(), refImage.getMax())) {
      targetThreshold = Float.valueOf(tmpStr).floatValue();
    }
    else {
      textTargetThreshold.requestFocus();
      textTargetThreshold.selectAll();
      return false;
    }

    tmpStr = textSourceGaussX.getText();
    if (testParameter(tmpStr, 0.0, Float.MAX_VALUE)) {
      sourceGaussX = Float.valueOf(tmpStr).floatValue();
    }
    else {
      textSourceGaussX.requestFocus();
      textSourceGaussX.selectAll();
      return false;
    }

    tmpStr = textSourceGaussY.getText();
    if (testParameter(tmpStr, 0.0, Float.MAX_VALUE)) {
      sourceGaussY = Float.valueOf(tmpStr).floatValue();
    }
    else {
      textSourceGaussY.requestFocus();
      textSourceGaussY.selectAll();
      return false;
    }

    tmpStr = textSourceGaussZ.getText();
    if (testParameter(tmpStr, 0.0, Float.MAX_VALUE)) {
      sourceGaussZ = Float.valueOf(tmpStr).floatValue();
    }
    else {
      textSourceGaussZ.requestFocus();
      textSourceGaussZ.selectAll();
      return false;
    }

    tmpStr = textTargetGaussX.getText();
    if (testParameter(tmpStr, 0.0, Float.MAX_VALUE)) {
      targetGaussX = Float.valueOf(tmpStr).floatValue();
    }
    else {
      textTargetGaussX.requestFocus();
      textTargetGaussX.selectAll();
      return false;
    }

    tmpStr = textTargetGaussY.getText();
    if (testParameter(tmpStr, 0.0, Float.MAX_VALUE)) {
      targetGaussY = Float.valueOf(tmpStr).floatValue();
    }
    else {
      textTargetGaussY.requestFocus();
      textTargetGaussY.selectAll();
      return false;
    }

    tmpStr = textTargetGaussZ.getText();
    if (testParameter(tmpStr, 0.0, Float.MAX_VALUE)) {
      targetGaussZ = Float.valueOf(tmpStr).floatValue();
    }
    else {
      textTargetGaussZ.requestFocus();
      textTargetGaussZ.selectAll();
      return false;
    }

    if (comboBoxCostFunct.getItemCount() >= 3) {
      switch (comboBoxCostFunct.getSelectedIndex()) {
        case 0:
          costFxn = 1;
          break;
        case 1:
          costFxn = 2;
          break;
        case 2:
          costFxn = 3;
          break;
        case 3:
          costFxn = 4;
          break;
        default:
          costFxn = 3;
          break;
      }
    }
    else {
      switch (comboBoxCostFunct.getSelectedIndex()) {
        case 0:
          costFxn = 1;
          break;
        case 1:
          costFxn = 4;
          break;
        default:
          costFxn = 1;
          break;
      }
    }

    /* Use 1 partition for intramodality, PET to PET or MRI to MRI.
       Use 256 partitions for intermodality, MRI to PET or for
       correlation ratio.
       Bidirectional fits are generally recommended.  However, if you
       have edited the target image to remove areas of pathology that
       you think will interfere with registration, choose forward fit.
       Likewise, if you have edited the source image to remove pathology,
       choose reverse fit */
    if ( (crossModalityCheckBox.isSelected() == true) || (costFxn == 4)) {
      switch (comboBoxPart.getSelectedIndex()) {
        case 1: // forward fit
          sourcePartitions = 0;
          targetPartitions = 256;
          break;
        case 2: // reverse fit
          sourcePartitions = 256;
          targetPartitions = 0;
          break;
        case 0: // default bidirectional fit
        default:
          sourcePartitions = 256;
          targetPartitions = 256;
          break;
      }
    }
    else {
      switch (comboBoxPart.getSelectedIndex()) {
        case 1: // forward fit
          sourcePartitions = 0;
          targetPartitions = 1;
          break;
        case 2: // reverse fit
          sourcePartitions = 1;
          targetPartitions = 0;
          break;
        case 0: // default bidirectional fit
        default:
          sourcePartitions = 1;
          targetPartitions = 1;
          break;
      }
    }

    return true;
  }

  /**
   *   Runs the algorithm.
   */
  private void callAlgorithm() {
    targetXDim = refImage.getExtents()[0];
    targetYDim = refImage.getExtents()[1];
    if (refImage.getNDims() > 2) {
      targetZDim = refImage.getExtents()[2];
    }
    if (cubicInterpolation) {
      targetVoxelX = refImage.getFileInfo()[0].getResolutions()[0];
      targetVoxelY = refImage.getFileInfo()[0].getResolutions()[1];
      targetVoxelZ = refImage.getFileInfo()[0].getResolutions()[2];
      targetUnitsX = refImage.getFileInfo()[0].getUnitsOfMeasure()[0];
      targetUnitsY = refImage.getFileInfo()[0].getUnitsOfMeasure()[1];
      targetUnitsZ = refImage.getFileInfo()[0].getUnitsOfMeasure()[2];
      doConvert = false;
      if ( (refImage.getNDims() > 2) &&
          ( (targetUnitsX != targetUnitsY) || (targetUnitsX != targetUnitsZ))) {
        doConvert = true;
      }
      else if ( (refImage.getNDims() == 2) &&
               (targetUnitsX != targetUnitsY)) {
        doConvert = true;
      }
      if (doConvert) {
        // The units of measure were not identical.  Convert to millimeters
        if ( (targetUnitsX == FileInfoBase.INCHES) ||
            (targetUnitsX == FileInfoBase.CENTIMETERS) ||
            (targetUnitsX == FileInfoBase.ANGSTROMS) ||
            (targetUnitsX == FileInfoBase.MICROMETERS) ||
            (targetUnitsX == FileInfoBase.MILLIMETERS) ||
            (targetUnitsX == FileInfoBase.METERS) ||
            (targetUnitsX == FileInfoBase.KILOMETERS) ||
            (targetUnitsX == FileInfoBase.MILES) ||
            (targetUnitsX == FileInfoBase.NANOMETERS)) {
          if (targetVoxelX <= 0.0f) {
            MipavUtil.displayWarning("Target x resolution was recorded as " +
                                     targetVoxelX +
                                     ". It is being changed to 1.0");
            targetVoxelX = 1.0f;
          }
          // Be ready for conversions between different units.
          if (targetUnitsX == FileInfoBase.MILLIMETERS) {
            // leave unchanged
          }
          else if (targetUnitsX == FileInfoBase.INCHES) {
            targetVoxelX = 25.4f * targetVoxelX;
          }
          else if (targetUnitsX == FileInfoBase.CENTIMETERS) {
            targetVoxelX = 10.0f * targetVoxelX;
          }
          else if (targetUnitsX == FileInfoBase.ANGSTROMS) {
            targetVoxelX = 1.0e-7f * targetVoxelX;
          }
          else if (targetUnitsX == FileInfoBase.NANOMETERS) {
            targetVoxelX = 1.0e-6f * targetVoxelX;
          }
          else if (targetUnitsX == FileInfoBase.MICROMETERS) {
            targetVoxelX = 1.0e-3f * targetVoxelX;
          }
          else if (targetUnitsX == FileInfoBase.METERS) {
            targetVoxelX = 1.0e3f * targetVoxelX;
          }
          else if (targetUnitsX == FileInfoBase.KILOMETERS) {
            targetVoxelX = 1.0e6f * targetVoxelX;
          }
          else if (targetUnitsX == FileInfoBase.MILES) {
            targetVoxelX = 1.6093e6f * targetVoxelX;
          }
        }
        if ( (targetUnitsY == FileInfoBase.INCHES) ||
            (targetUnitsY == FileInfoBase.CENTIMETERS) ||
            (targetUnitsY == FileInfoBase.ANGSTROMS) ||
            (targetUnitsY == FileInfoBase.MICROMETERS) ||
            (targetUnitsY == FileInfoBase.MILLIMETERS) ||
            (targetUnitsY == FileInfoBase.METERS) ||
            (targetUnitsY == FileInfoBase.KILOMETERS) ||
            (targetUnitsY == FileInfoBase.MILES) ||
            (targetUnitsY == FileInfoBase.NANOMETERS)) {
          if (targetVoxelY <= 0.0f) {
            MipavUtil.displayWarning("Target y resolution was recorded as " +
                                     targetVoxelY +
                                     ". It is being changed to 1.0");
            targetVoxelY = 1.0f;
          }
          // Be ready for conversions between different units.
          if (targetUnitsY == FileInfoBase.MILLIMETERS) {
            // leave unchanged
          }
          else if (targetUnitsY == FileInfoBase.INCHES) {
            targetVoxelY = 25.4f * targetVoxelY;
          }
          else if (targetUnitsY == FileInfoBase.CENTIMETERS) {
            targetVoxelY = 10.0f * targetVoxelY;
          }
          else if (targetUnitsY == FileInfoBase.ANGSTROMS) {
            targetVoxelY = 1.0e-7f * targetVoxelY;
          }
          else if (targetUnitsY == FileInfoBase.NANOMETERS) {
            targetVoxelY = 1.0e-6f * targetVoxelY;
          }
          else if (targetUnitsY == FileInfoBase.MICROMETERS) {
            targetVoxelY = 1.0e-3f * targetVoxelY;
          }
          else if (targetUnitsY == FileInfoBase.METERS) {
            targetVoxelY = 1.0e3f * targetVoxelY;
          }
          else if (targetUnitsY == FileInfoBase.KILOMETERS) {
            targetVoxelY = 1.0e6f * targetVoxelY;
          }
          else if (targetUnitsY == FileInfoBase.MILES) {
            targetVoxelY = 1.6093e6f * targetVoxelY;
          }
        }
        if ( (targetUnitsZ == FileInfoBase.INCHES) ||
            (targetUnitsZ == FileInfoBase.CENTIMETERS) ||
            (targetUnitsZ == FileInfoBase.ANGSTROMS) ||
            (targetUnitsZ == FileInfoBase.MICROMETERS) ||
            (targetUnitsZ == FileInfoBase.MILLIMETERS) ||
            (targetUnitsZ == FileInfoBase.METERS) ||
            (targetUnitsZ == FileInfoBase.KILOMETERS) ||
            (targetUnitsZ == FileInfoBase.MILES) ||
            (targetUnitsZ == FileInfoBase.NANOMETERS)) {
          if (targetVoxelZ <= 0.0f) {
            MipavUtil.displayWarning("Target z resolution was recorded as " +
                                     targetVoxelZ +
                                     ". It is being changed to 1.0");
            targetVoxelZ = 1.0f;
          }
          // Be ready for conversions between different units.
          if (targetUnitsZ == FileInfoBase.MILLIMETERS) {
            // leave unchanged
          }
          else if (targetUnitsZ == FileInfoBase.INCHES) {
            targetVoxelZ = 25.4f * targetVoxelZ;
          }
          else if (targetUnitsZ == FileInfoBase.CENTIMETERS) {
            targetVoxelZ = 10.0f * targetVoxelZ;
          }
          else if (targetUnitsZ == FileInfoBase.ANGSTROMS) {
            targetVoxelZ = 1.0e-7f * targetVoxelZ;
          }
          else if (targetUnitsZ == FileInfoBase.NANOMETERS) {
            targetVoxelZ = 1.0e-6f * targetVoxelZ;
          }
          else if (targetUnitsZ == FileInfoBase.MICROMETERS) {
            targetVoxelZ = 1.0e-3f * targetVoxelZ;
          }
          else if (targetUnitsZ == FileInfoBase.METERS) {
            targetVoxelZ = 1.0e3f * targetVoxelZ;
          }
          else if (targetUnitsZ == FileInfoBase.KILOMETERS) {
            targetVoxelZ = 1.0e6f * targetVoxelZ;
          }
          else if (targetUnitsZ == FileInfoBase.MILES) {
            targetVoxelZ = 1.6093e6f * targetVoxelZ;
          }
        }
      } // if (doConvert)
      pixel_size_s = targetVoxelX;
      if (targetVoxelY < pixel_size_s) {
        pixel_size_s = targetVoxelY;
      }
      if (refImage.getNDims() > 2) {
        if (targetVoxelZ < pixel_size_s) {
          pixel_size_s = targetVoxelZ;
        }
      }

      xoom1 = (float) (targetVoxelX / pixel_size_s);
      targetXDim = (int) ( (targetXDim - 1) * xoom1 + 1);
      targetVoxelX = (float) pixel_size_s;

      yoom1 = (float) (targetVoxelY / pixel_size_s);
      targetYDim = (int) ( (targetYDim - 1) * yoom1 + 1);
      targetVoxelY = (float) pixel_size_s;

      if (refImage.getNDims() > 2) {
        zoom1 = (float) (targetVoxelZ / pixel_size_s);
        targetZDim = (int) ( (targetZDim - 1) * zoom1 + 1);
        targetVoxelZ = (float) pixel_size_s;
      }
    } // if (cubicInterpolation)
    if (refImage.getNDims() == 2) {
      resultExtents = new int[2];
      resultExtents[0] = targetXDim;
      resultExtents[1] = targetYDim;
    }
    else {
      resultExtents = new int[3];
      resultExtents[0] = targetXDim;
      resultExtents[1] = targetYDim;
      resultExtents[2] = targetZDim;
    }
    try {
      resultImage = new ModelImage(ModelStorageBase.FLOAT, resultExtents,
                                   makeImageName(matchImage.getImageName(),
                                                 "_registered"), UI);
      air = new AlgorithmRegAIR(resultImage, refImage, entireTarget, matchImage,
                                entireSource, transformation, interpolation,
                                sourceThreshold,
                                targetThreshold, sourceGaussX, sourceGaussY,
                                sourceGaussZ,
                                targetGaussX, targetGaussY, targetGaussZ,
                                sourcePartitions,
                                targetPartitions, costFxn, cubicInterpolation,
                                interaction,
                                precision, iterations);
    }
    catch (OutOfMemoryError x) {
      MipavUtil.displayError(
          "Dialog RegistrationAIR: unable to allocate enough memory");
      if (resultImage != null) {
        resultImage.disposeLocal(); // Clean up memory of result image
        resultImage = null;
      }
      return;
    }

    // This is very important. Adding this object as a listener allows
    // the algorithm to notify this object when it has completed of failed.
    // See algorithm performed event. This is made possible by implementing
    air.addListener(this);

    if (runInSeparateThread) {
      // Start the thread as a low priority because we wish to still have
      //user interface work fast
      if (air.startMethod(Thread.MIN_PRIORITY) == false) {
        MipavUtil.displayError("A thread is already running on this object");
        return;
      }
    }
    else {
      air.setActiveImage(isActiveImage);
      if (!UI.isAppFrameVisible()) {
        air.setProgressBarVisible(false);
      }
      air.run();
    }
  }

  //************************************************************************
   //************************** Algorithm Events ****************************
    //************************************************************************

     /**
          *	This method is required if the AlgorithmPerformed interface is implemented.
      *   It is called by the algorithm when it has completed or failed to to complete,
      *   so that the dialog can be display the result image and/or clean up.
      *   @param algorithm   Algorithm that caused the event.
      */
     public void algorithmPerformed(AlgorithmBase algorithm) {
       if (algorithm instanceof AlgorithmRegAIR) {
         matchImage.clearMask();

         if (air.isCompleted() == true && resultImage != null) {
           //The algorithm has completed and produced a new image to be displayed.
           updateFileInfo(matchImage, resultImage);
           resultImage.clearMask();

           try {
             new ViewJFrameImage(resultImage, null, new Dimension(610, 200));
           }
           catch (OutOfMemoryError error) {
             System.gc();
             MipavUtil.displayError("Out of memory: unable to open new frame");
           }
         }
         else if (resultImage != null) {
           //algorithm failed but result image still has garbage
           resultImage.disposeLocal(); // Clean up memory of result image
           resultImage = null;
           System.gc();
         }
       }
       matchImage.notifyImageDisplayListeners(null, true);

       insertScriptLine(algorithm);

       dispose();
     }

  /**
   *   Method to handle item events.  If image name was
   *   changed in the combo box, reset labels for target
   *   threshold.  If partition was changed, disable the
   *   necessary check boxes.
   *   @param event    Event that cause the method to fire
   */
  public void itemStateChanged(ItemEvent event) {
    Object source = event.getSource();
    if (source == comboBoxImage) {
      selectedName = (String) comboBoxImage.getSelectedItem();
      refImage = UI.getRegisteredImageByName(selectedName);
      targetThreshold = (float) (refImage.getMin() +
                                 0.2f * (refImage.getMax() - refImage.getMin()));
      if (labelTargetThreshold != null) {
        labelTargetThreshold.setText("Target threshold ("
                                     + nf.format(refImage.getMin()) + " - " +
                                     nf.format(refImage.getMax()) + ")");
      }
      if (textTargetThreshold != null) {
        textTargetThreshold.setText(String.valueOf(targetThreshold));
      }
    }
    else if (source == comboBoxPart) {
      switch (comboBoxPart.getSelectedIndex()) {
        case 1: // forward fit, 0 source partitions
          if (sourceVOICheckBox.isSelected()) {
            sourceVOICheckBox.setSelected(false);
            MipavUtil.displayWarning(
                "Masking of the source files is disabled because you have" +
                " set its number of partitions to zero");
          }
          sourceVOICheckBox.setEnabled(false);
          break;
        case 2: // reverse fit, 0 target partitions
          if (targetVOICheckBox.isSelected()) {
            targetVOICheckBox.setSelected(false);
            MipavUtil.displayWarning(
                "Masking of the target files is disabled because you have" +
                " set its number of partitions to zero");
          }
          targetVOICheckBox.setEnabled(false);
          break;
        case 0:
        default:
          targetVOICheckBox.setEnabled(true);
          sourceVOICheckBox.setEnabled(true);
          break;
      }
    }
  }
}
