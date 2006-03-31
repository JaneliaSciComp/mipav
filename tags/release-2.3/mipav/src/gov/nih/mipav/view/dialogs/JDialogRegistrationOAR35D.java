package gov.nih.mipav.view.dialogs;

import gov.nih.mipav.view.*;
import gov.nih.mipav.model.structures.*;
import gov.nih.mipav.model.file.*;
import gov.nih.mipav.model.algorithms.*;
import gov.nih.mipav.model.algorithms.registration.*;

import java.awt.event.*;
import java.awt.*;
import java.util.*;
import java.io.File;

import javax.swing.*;

/**
 *   Dialog to get user input, then call AlgorithmRegOAR35D.
 *   Internal registration is performed within one 4D image.
 *   In adjacent mode the first volume above the reference volume is
 *   registered to the reference volume, then the second volume above the
 *   reference volume is registered to the first volume above the
 *   reference volume, and so on until the last volume is registered
 *   the next to last volume.  Then, the first volume below the
 *   reference volume is registered to the reference volume, the second
 *   volume below the reference volume is registered to first volume below
 *   the reference volume, and so on unitl the first volume is registered
 *   to the second volume.  In reference mode every volume is simply
 *   registered to the reference volume.  In average mode every volume is
 *   registered to the average of all volumes.
 *
 *	@author     Neva Cherniavsky
 *	@see		AlgorithmCostFunctions
 *	@see		AlgorithmRegOAR35D
 *
 */
public class        JDialogRegistrationOAR35D
       extends      JDialogBase
       implements   AlgorithmInterface, ScriptableInterface {

  private ViewUserInterface UI;
  private AlgorithmRegOAR35D reg35 = null;

  private ModelImage matchImage; //register slices within matchImage
  private ModelImage resultImage = null;
  private ModelImage refVolume = null; //for optional outside reference volume
  private boolean useOutsideReferenceVolume = false;
  private JComboBox comboBoxDOF;
  private JComboBox comboBoxCostFunct;
  private JLabel labelInterp2;
  private JComboBox comboBoxInterp;
  private JComboBox comboBoxInterp2;
  private JTextField rotateBeginText;
  private JTextField rotateEndText;
  private JTextField coarseRateText;
  private JTextField fineRateText;
  private JCheckBox sampleCheckBox;
  private boolean doSubsample;
  private JCheckBox fastModeCheckbox;
  private boolean fastMode;
  private JRadioButton voiRadio;
  private JRadioButton weightRadio;
  private JRadioButton noneRadio;
  private JTextField textInput;
  private JButton buttonWeightInput;

  private JRadioButton adjacentImageRButton;
  private JRadioButton averageImageRButton;
  private JRadioButton refImageRButton;
  private JTextField refImageNumText;
  private int refImageNum = 0;
  private int registerTo; // 1 for adjacent
                          // 2 for average
                          // 3 for reference
  private JCheckBox graphCheckBox;

  private String fileNameWInput, directoryWInput;
  private ModelImage inputWeightImage;

  private float rotateBegin, rotateEnd, coarseRate, fineRate;
  private int cost, interp, interp2, DOF;
  private boolean weighted;
  private boolean voisOnly;
  private boolean doColor;
  boolean doGraph;

  private boolean isScript;

  // Variables for Advanced Settings dialog
  private JDialog advancedDialog;
  private JTextField bracketBoundText, maxIterationsText, numMinText;
  private int maxIterations_def=2, bracketBound_def=10, numMinima_def=3;
  private int maxIterations=maxIterations_def, bracketBound=bracketBound_def;
  private int numMinima=numMinima_def;

  /**
   *	Creates new dialog for user to choose variables for internal registration
   *	@param theParentFrame  Parent frame.
   *	@param im              Source image.
   */
  public JDialogRegistrationOAR35D(Frame theParentFrame, ModelImage im) {
    super(theParentFrame, true);
    matchImage = im;
    if (matchImage.isColorImage()) {
      doColor = true;
    }
    else {
      doColor = false;
    }
    UI = ( (ViewJFrameBase) parentFrame).getUserInterface();
    init();
  }

  /**
       *	Used primarily for the script to store variables and run the algorithm.  No
   *	actual dialog will appear but the set up info and result image will be stored here.
   *	@param _UI   The user interface, needed to create the image frame.
   *	@param im	Source image.
   */
  public JDialogRegistrationOAR35D(ViewUserInterface _UI, ModelImage im) {
    super();
    UI = _UI;
    matchImage = im;
    if (matchImage.isColorImage()) {
      doColor = true;
    }
    else {
      doColor = false;
    }
    parentFrame = im.getParentFrame();
  }

  /**
   * Empty constructor needed for dynamic instantiation (used during scripting).
   */
  public JDialogRegistrationOAR35D() {}

  /**
     * Run this algorithm from a script.
     * @param parser the script parser we get the state from
     * @throws IllegalArgumentException if there is something wrong with the arguments in the script
     */
    public void scriptRun(AlgorithmScriptParser parser) throws IllegalArgumentException {
        String srcImageKey = null;

        try {
            srcImageKey = parser.getNextString();
        }
        catch (Exception e) {
            throw new IllegalArgumentException();
        }
        ModelImage im = parser.getImage(srcImageKey);

        matchImage = im;
        UI = matchImage.getUserInterface();
        parentFrame = matchImage.getParentFrame();
        if (matchImage.isColorImage()) {
            doColor = true;
        }
        else {
            doColor = false;
        }

        try {
            boolean weighted = parser.getNextBoolean();
            setWeighted(weighted);
            if (weighted) {
                setInputWeightImage(parser.getImage(parser.getNextString()));
            }
            setCostChoice(parser.getNextInteger());
            setDOF(parser.getNextInteger());
            setInterp(parser.getNextInteger());
            setInterp2(parser.getNextInteger());
            setRegisterTo(parser.getNextInteger());
            setRefImageNum(parser.getNextInteger());
            setCoarseBegin(parser.getNextFloat());
            setCoarseEnd(parser.getNextFloat());
            setCoarseRate(parser.getNextFloat());
            setFineRate(parser.getNextFloat());
            setGraphCheckBox(parser.getNextBoolean());
            setSubsample(parser.getNextBoolean());
            setFastMode(parser.getNextBoolean());
            setBracketBound(parser.getNextInteger());
            setMaxIterations(parser.getNextInteger());
            setNumMinima(parser.getNextInteger());
        }
        catch (Exception e) {
            throw new IllegalArgumentException();
        }

        setActiveImage(parser.isActiveImage());
        setSeparateThread(false);
        // only set ref image number if it has not been set already (so user can designate which slice)
        if (refImageNum == 0)
            setRefImageNum( (int) (matchImage.getExtents()[3] / 2) + 1);
        setIsScript(true);
        callAlgorithm();
    }

    /**
     * If a script is being recorded and the algorithm is done, add an entry for this algorithm.
     * @param algo the algorithm to make an entry for
     */
    public void insertScriptLine(AlgorithmBase algo) {
        if (algo.isCompleted()) {
            if (UI.isScriptRecording()) {
                //check to see if the match image is already in the ImgTable
                if (UI.getScriptDialog().getImgTableVar(matchImage.getImageName()) == null) {
                    if (UI.getScriptDialog().getActiveImgTableVar(matchImage.getImageName()) == null) {
                        UI.getScriptDialog().putActiveVar(matchImage.getImageName());
                    }
                }
                //check to see if the match image is already in the ImgTable
                if (UI.getScriptDialog().getImgTableVar(matchImage.getImageName()) == null) {
                    if (UI.getScriptDialog().getActiveImgTableVar(matchImage.getImageName()) == null) {
                        UI.getScriptDialog().putActiveVar(matchImage.getImageName());
                    }
                }

                if (weighted) {
                    /*
                                    UI.getScriptDialog().putVar(inputWeightImage.getImageName());
                                    UI.getScriptDialog().append("LoadImage " +
                        UI.getScriptDialog().getVar(inputWeightImage.getImageName()) +
                        "\n");
                     */

                    //check to see if the match image is already in the ImgTable
                    if (UI.getScriptDialog().getImgTableVar(inputWeightImage.getImageName()) == null) {
                        if (UI.getScriptDialog().getActiveImgTableVar(inputWeightImage.getImageName()) == null) {
                            UI.getScriptDialog().putActiveVar(inputWeightImage.getImageName());
                        }
                    }

                    UI.getScriptDialog().append("RegistrationOAR35D " +
                                                UI.getScriptDialog().getVar(
                        matchImage.getImageName()) + " " + weighted + " " +
                                                UI.getScriptDialog().getVar(
                        inputWeightImage.getImageName()) + " " +
                                                cost + " " + DOF + " " + interp +
                                                " " + interp2 + " " + registerTo +
                                                " " + refImageNum +
                                                " " + rotateBegin + " " + rotateEnd + " " +
                                                coarseRate + " " + fineRate + " " +
                                                doGraph + " " + doSubsample +
                                                " " + fastMode + " " + bracketBound + " " +
                                                maxIterations + " " + numMinima + "\n");
                }

                else {
                    UI.getScriptDialog().append("RegistrationOAR35D " +
                                                UI.getScriptDialog().getVar(
                        matchImage.getImageName()) + " " + weighted + " " +
                                                cost + " " + DOF + " " + interp +
                                                " " + interp2 + " " + registerTo +
                                                " " + refImageNum +
                                                " " + rotateBegin + " " + rotateEnd + " " +
                                                coarseRate + " " + fineRate + " " +
                                                doGraph + " " + doSubsample + " " +
                                                fastMode + " " + bracketBound + " " +
                                                maxIterations + " " + numMinima + "\n");
                }
            }
        }
    }

  /**
   *   Accessor to get the result image.
   *   @return  Result image.
   */
  public ModelImage getResultImage() {
    return resultImage;
  }

  /**
   *   Accessor to set the input weight image.
   *   @param im  Input weight image.
   */
  public void setInputWeightImage(ModelImage im) {
    inputWeightImage = im;
  }

  /**
   * allows user to use an outside reference volume for registering
   * @param refVolume (3-Dim reference volume)
   */
  public void setOutsideReferenceVolume(ModelImage refVolume) {
    this.refVolume = refVolume;
    this.useOutsideReferenceVolume = true;
  }

  /**
   *   Accessor to set the choice of cost function
   *   @param x Cost function.
   */
  public void setCostChoice(int x) {
    cost = x;
  }

  /**
   *   Accessor to set the degrees of freedom
   *   @param x Degrees of freedom
   */
  public void setDOF(int x) {
    DOF = x;
  }

  /**
   *   Accessor to set the initial interpolation.
   *   @param x Interpolation
   */
  public void setInterp(int x) {
    interp = x;
  }

  /**
   *   Accessor to set the coarse sample begin.
   *   @param x Coarse begin
   */
  public void setCoarseBegin(float x) {
    rotateBegin = x;
  }

  /**
   *   Accessor to set the coarse sample end.
   *   @param x Coarse end
   */
  public void setCoarseEnd(float x) {
    rotateEnd = x;
  }

  /**
   *   Accessor to set the coarse sample rate.
   *   @param x Coarse rate
   */
  public void setCoarseRate(float x) {
    coarseRate = x;
  }

  /**
   *   Accessor to set the fine sample rate.
   *   @param x Fine rate
   */
  public void setFineRate(float x) {
    fineRate = x;
  }

  /**
   *   Accessor to set the weighted images flag.
   *   @param flag <code>true</code> means there are weighted images.
   */
  public void setWeighted(boolean flag) {
    weighted = flag;
  }

  /**
   *   Accessor to set the VOIs only flag.
   *   @param flag <code>true</code> then only register the parts of the images in the VOIs.
   */
  public void setVoisOnly(boolean flag) {
    voisOnly = flag;
  }

  /**
   *   Accessor to set the final interpolation.
   *   @param x Interpolation
   */
  public void setInterp2(int x) {
    interp2 = x;
  }

  /**
   *   Accessor to set registerTo
   *   @param registerTo - 1 = adjacent, 2 = average, 3 = reference
   */
  public void setRegisterTo(int registerTo) {
    this.registerTo = registerTo;
  }

  /**
   *   Accessor to set refImageNum
   *   @param refImageNumber number of reference slice
   */
  public void setRefImageNum(int refImageNumber) {
    refImageNum = refImageNumber;
  }

  /**
   *  Accessor to set graphCheckBox
   *  @param doGraph if true output graphs of rotations and translations
   */
  public void setGraphCheckBox(boolean doGraph) {
    this.doGraph = doGraph;
  }

  /**
   *   Accessor to set whether or not subsampling occurs
   *   @param doSubsample
   */
  public void setSubsample(boolean doSubsample) {
    this.doSubsample = doSubsample;
  }

  /**
   *   Accessor to set whether or not to execute the fast mode (skip sub sample and goto last final optimization).
   *   @param flag <code>true</code> then skip to level one (last ) optimization.
   */
  public void setFastMode(boolean flag) {
    fastMode = flag;
  }

  /**
  *  Accessor to set bracketBound
  *  @param bracketBound
  */
  public void setBracketBound(int bracketBound) {
    this.bracketBound = bracketBound;
  }

  /**
  *  Accessor to set maxIterations
  *  @param maxIterations
  */
  public void setMaxIterations(int maxIterations) {
    this.maxIterations = maxIterations;
  }

  /**
  *  Accessor to set numMinima
  *  @param numMinima
  */
  public void setNumMinima(int numMinima) {
    this.numMinima = numMinima;
  }

  /**
   *	Initializes the GUI components and displays the dialog.
   */
  private void init() {
    setForeground(Color.black);
    setTitle("Optimized Automatic Image Registration 3.5D");

    JPanel optPanel = new JPanel();
    optPanel.setLayout(new GridBagLayout());
    optPanel.setBorder(buildTitledBorder("Input Options"));

    String matchName = matchImage.getImageName();
    JLabel labelImage = new JLabel("Internal Registration for " + matchName);
    labelImage.setForeground(Color.black);
    labelImage.setFont(serif12);
    labelImage.setAlignmentX(Component.LEFT_ALIGNMENT);

    JLabel labelDOF = new JLabel("Degrees of freedom:");
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

    JLabel labelCost = new JLabel("Cost function:");
    labelCost.setForeground(Color.black);
    labelCost.setFont(serif12);
    labelCost.setAlignmentX(Component.LEFT_ALIGNMENT);

    comboBoxCostFunct = new JComboBox();
    comboBoxCostFunct.setFont(MipavUtil.font12);
    comboBoxCostFunct.setBackground(Color.white);
    comboBoxCostFunct.setToolTipText("Cost function");
    if (!doColor) {
      comboBoxCostFunct.addItem("Correlation ratio");
    }
    comboBoxCostFunct.addItem("Least squares");
    if (!doColor) {
      comboBoxCostFunct.addItem("Normalized cross correlation");
      comboBoxCostFunct.addItem("Normalized mutual information");
    }
    comboBoxCostFunct.setSelectedIndex(0);

    JLabel labelInterp = new JLabel("Interpolation:");
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

    // Rotation Range Panel
    JPanel rotateRangePanel = new JPanel(new FlowLayout(FlowLayout.LEFT));

    JLabel labelRotateRange = new JLabel("Rotation angle sampling range:");
    labelRotateRange.setForeground(Color.black);
    labelRotateRange.setFont(serif12);
    JLabel labelRotateRangeTo = new JLabel("to");
    labelRotateRangeTo.setForeground(Color.black);
    labelRotateRangeTo.setFont(serif12);
    JLabel labelRotateDegrees = new JLabel("degrees");
    labelRotateDegrees.setFont(serif12);

    rotateBeginText = new JTextField("-30", 3);
    rotateEndText = new JTextField("30", 3);

    rotateRangePanel.add(labelRotateRange);
    rotateRangePanel.add(rotateBeginText);
    rotateRangePanel.add(labelRotateRangeTo);
    rotateRangePanel.add(rotateEndText);
    rotateRangePanel.add(labelRotateDegrees);

    // Coarse sampling rate panel
    JPanel coarsePanel = new JPanel(new FlowLayout(FlowLayout.LEFT));

    JLabel labelCoarse = new JLabel("Coarse angle increment: ");
    labelCoarse.setForeground(Color.black);
    labelCoarse.setFont(serif12);
    labelCoarse.setAlignmentX(Component.LEFT_ALIGNMENT);
    JLabel labelCoarseDegrees = new JLabel("degrees");
    labelCoarseDegrees.setFont(serif12);
    coarseRateText = new JTextField("15", 3);

    coarsePanel.add(labelCoarse);
    coarsePanel.add(coarseRateText);
    coarsePanel.add(labelCoarseDegrees);
    coarsePanel.setAlignmentX(Component.LEFT_ALIGNMENT);

    // Fine sampling rate panel
    JPanel finePanel = new JPanel(new FlowLayout(FlowLayout.LEFT));

    JLabel labelFine = new JLabel("Fine angle increment:");
    labelFine.setForeground(Color.black);
    labelFine.setFont(serif12);
    labelFine.setAlignmentX(Component.LEFT_ALIGNMENT);
    JLabel labelFineDegrees = new JLabel("degrees");
    labelFineDegrees.setFont(serif12);
    fineRateText = new JTextField("6", 3);

    finePanel.add(labelFine);
    finePanel.add(fineRateText);
    finePanel.add(labelFineDegrees);
    finePanel.setAlignmentX(Component.LEFT_ALIGNMENT);

    sampleCheckBox = new JCheckBox("Subsample image for speed");
    sampleCheckBox.setFont(serif12);
    sampleCheckBox.setForeground(Color.black);
    sampleCheckBox.setSelected(true);
    sampleCheckBox.setEnabled(true);

    Insets insets = new Insets(0, 2, 0, 2);
    GridBagConstraints gbc = new GridBagConstraints();

    gbc.insets = insets;
    gbc.fill = GridBagConstraints.NONE;
    gbc.anchor = GridBagConstraints.WEST;

    gbc.gridx = 0; gbc.gridy = 0;
    gbc.gridwidth = 2; gbc.gridheight = 1;
    optPanel.add(labelImage, gbc);

    gbc.gridx = 0; gbc.gridy = 1;
    gbc.weightx = 0;
    gbc.gridwidth = 1;
    optPanel.add(labelDOF, gbc);
    gbc.gridx = 1; gbc.gridy = 1;
    gbc.weightx = 1;
    gbc.gridwidth = GridBagConstraints.REMAINDER;
    optPanel.add(comboBoxDOF, gbc);

    gbc.gridx = 0; gbc.gridy = 2;
    gbc.weightx = 0;
    gbc.gridwidth = 1;
    optPanel.add(labelInterp, gbc);
    gbc.gridx = 1; gbc.gridy = 2;
    gbc.weightx = 1;
    gbc.gridwidth = GridBagConstraints.REMAINDER;
    optPanel.add(comboBoxInterp, gbc);

    gbc.gridx = 0; gbc.gridy = 3;
    gbc.weightx = 0;
    gbc.gridwidth = 1;
    optPanel.add(labelCost, gbc);
    gbc.gridx = 1; gbc.gridy = 3;
    gbc.weightx = 1;
    gbc.gridwidth = GridBagConstraints.REMAINDER;
    optPanel.add(comboBoxCostFunct, gbc);

    gbc.weightx = 0;
    gbc.gridwidth = 3;
    gbc.gridx = 0; gbc.gridy = 4;
    optPanel.add(rotateRangePanel, gbc);

    gbc.gridx = 0; gbc.gridy = 5;
    optPanel.add(coarsePanel, gbc);

    gbc.gridx = 0; gbc.gridy = 6;
    optPanel.add(finePanel, gbc);

    gbc.gridx = 0; gbc.gridy = 7;
    gbc.weightx = 1; gbc.gridwidth = 1;
    optPanel.add(sampleCheckBox, gbc);

    // Reference image panel
    JPanel refPanel = new JPanel(new GridBagLayout());
    refPanel.setBorder(buildTitledBorder("Reference image"));

    JLabel labelInternal = new JLabel("Reference volume (1-" +
                                      String.valueOf(matchImage.getExtents()[3]) + ")");
    labelInternal.setForeground(Color.black);
    labelInternal.setFont(serif12);
    gbc.gridx = 0; gbc.gridy = 0;
    gbc.gridwidth = 1; gbc.weightx=0;
    refPanel.add(labelInternal, gbc);

    refImageNumText = new JTextField(String.valueOf( (matchImage.getExtents()[3] /
        2) + 1), 3);
    refImageNumText.setEnabled(true);
    gbc.gridx = 1;  gbc.gridy = 0;
    gbc.gridwidth = GridBagConstraints.REMAINDER; gbc.weightx=1;
    refPanel.add(refImageNumText, gbc);

    // Adjacent, average, or reference radio buttons
    ButtonGroup group1 = new ButtonGroup();

    adjacentImageRButton = new JRadioButton("Register to adjacent volume", true);
    adjacentImageRButton.setFont(serif12);
    adjacentImageRButton.addItemListener(this);
    group1.add(adjacentImageRButton);

    averageImageRButton = new JRadioButton("Register to average volume", false);
    averageImageRButton.setFont(serif12);
    averageImageRButton.addItemListener(this);
    group1.add(averageImageRButton);

    refImageRButton = new JRadioButton("Register to reference volume", false);
    refImageRButton.setFont(serif12);
    refImageRButton.addItemListener(this);
    group1.add(refImageRButton);

    gbc.gridx = 0; gbc.gridy = 1;
    refPanel.add(adjacentImageRButton, gbc);
    gbc.gridx = 0; gbc.gridy = 2;
    refPanel.add(averageImageRButton, gbc);
    gbc.gridx = 0; gbc.gridy = 3;
    refPanel.add(refImageRButton, gbc);

    ButtonGroup weightGroup = new ButtonGroup();

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
    voiRadio.setEnabled(false);
    ViewVOIVector VOIs = matchImage.getVOIs();
    if (VOIs != null && VOIs.size() > 0) {
      voiRadio.setEnabled(true);
    }
    weightGroup.add(voiRadio);

    weightRadio = new JRadioButton("Weight registration");
    weightRadio.setFont(serif12);
    weightRadio.setForeground(Color.black);
    weightRadio.setSelected(false);
    weightRadio.addItemListener(this);
    weightGroup.add(weightRadio);

    buttonWeightInput = new JButton("Choose input weight");
    buttonWeightInput.setForeground(Color.black);
    buttonWeightInput.setFont(serif12B);
    buttonWeightInput.setEnabled(false);
    buttonWeightInput.addActionListener(this);
    buttonWeightInput.setActionCommand("Input");
    buttonWeightInput.setPreferredSize(new Dimension(145, 30));

    textInput = new JTextField();
    textInput.setFont(serif12);
    textInput.setEnabled(false);

    JPanel weightPanel = new JPanel(new GridBagLayout());
    weightPanel.setBorder(buildTitledBorder("Weighted image"));

    gbc.gridx = 0;
    gbc.gridy = 0;
    gbc.weightx = 0;
    gbc.fill = gbc.NONE;
    gbc.anchor = gbc.WEST;
    gbc.gridwidth = 2;
    weightPanel.add(noneRadio, gbc);
    gbc.gridx = 0;
    gbc.gridy = 1;
    gbc.weightx = 0;
    gbc.fill = gbc.NONE;
    gbc.anchor = gbc.WEST;
    weightPanel.add(voiRadio, gbc);
    gbc.gridx = 0;
    gbc.gridy = 2;
    gbc.weightx = 0;
    gbc.fill = gbc.NONE;
    gbc.anchor = gbc.WEST;
    weightPanel.add(weightRadio, gbc);
    gbc.gridy = 3;
    gbc.fill = gbc.NONE;
    gbc.gridwidth = 1;
    weightPanel.add(buttonWeightInput, gbc);
    gbc.gridx = 1;
    gbc.weightx = 1;
    gbc.fill = gbc.HORIZONTAL;
    weightPanel.add(textInput, gbc);

    JPanel outPanel = new JPanel();
    outPanel.setLayout(new GridBagLayout());
    outPanel.setBorder(buildTitledBorder("Output Options"));

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

    graphCheckBox = new JCheckBox("Graph rotations and translations");
    graphCheckBox.setFont(serif12);
    graphCheckBox.setForeground(Color.black);
    graphCheckBox.setSelected(false);
    graphCheckBox.setEnabled(false);

    gbc.gridx = 0;
    gbc.gridy = 0;
    gbc.weightx = 0;
    gbc.fill = gbc.NONE;
    outPanel.add(labelInterp2, gbc);
    gbc.gridx = 1;
    gbc.gridy = 0;
    gbc.weightx = 1;
    gbc.fill = gbc.HORIZONTAL;
    outPanel.add(comboBoxInterp2, gbc);
    gbc.gridx = 0;
    gbc.gridy = 1;
    gbc.weightx = 1;
    gbc.fill = gbc.HORIZONTAL;
    outPanel.add(graphCheckBox, gbc);

    JPanel buttonPanel = new JPanel();
    buildOKButton();
    buttonPanel.add(OKButton);
    buildCancelButton();
    buttonPanel.add(cancelButton);
    JButton advancedButton = new JButton("Advanced settings");
    advancedButton.setActionCommand("AdvancedSettings");
    advancedButton.addActionListener(this);
    advancedButton.setMinimumSize(new Dimension(90, 30));
    advancedButton.setFont(serif12B);
    buttonPanel.add(advancedButton);

    JPanel mainPanel = new JPanel();
    mainPanel.setLayout(new BoxLayout(mainPanel, BoxLayout.Y_AXIS));
    mainPanel.setBorder(BorderFactory.createEmptyBorder(5, 5, 5, 5));
    optPanel.setAlignmentX(Component.LEFT_ALIGNMENT);
    refPanel.setAlignmentX(Component.LEFT_ALIGNMENT);
    weightPanel.setAlignmentX(Component.LEFT_ALIGNMENT);
    outPanel.setAlignmentX(Component.LEFT_ALIGNMENT);
    mainPanel.add(optPanel);
    mainPanel.add(refPanel);
    mainPanel.add(weightPanel);
    mainPanel.add(outPanel);

    getContentPane().add(mainPanel);
    getContentPane().add(buttonPanel, BorderLayout.SOUTH);
    pack();
    setVisible(true);
  }

  /**
   *	Closes dialog box when the OK button is pressed, sets the variables, and calls the algorithm.
   *	@param event       Event that triggers function.
   */
  public void actionPerformed(ActionEvent event) {
    String command = event.getActionCommand();
    String tmpStr;
    if (command.equals("OK")) {
      setIsScript(false);
      if (setVariables()) {
        callAlgorithm();
      }
    }
    else if (command.equals("Cancel")) {
      dispose();
    }
    else if (command.equals("AdvancedSettings")) {
      bracketBound_def=bracketBound;
      maxIterations_def=maxIterations;
      numMinima_def=numMinima;
      advancedDialog = buildAdvancedDialog(bracketBound,maxIterations,numMinima);
    }
    else if (command.equals("Input")) {
      try {
        JFileChooser chooser = new JFileChooser();
        if (UI.getDefaultDirectory() != null) {
          File file = new File(UI.getDefaultDirectory());
          if (file != null) {
            chooser.setCurrentDirectory(file);
          }
          else {
            chooser.setCurrentDirectory(new File(System.getProperty("user.dir")));
          }
        }
        else {
          chooser.setCurrentDirectory(new File(System.getProperty("user.dir")));
        }

        chooser.addChoosableFileFilter(new ViewImageFileFilter(
            ViewImageFileFilter.GEN));
        chooser.addChoosableFileFilter(new ViewImageFileFilter(
            ViewImageFileFilter.TECH));
        chooser.addChoosableFileFilter(new ViewImageFileFilter(
            ViewImageFileFilter.MICROSCOPY));
        chooser.addChoosableFileFilter(new ViewImageFileFilter(
            ViewImageFileFilter.MISC));

        chooser.setDialogTitle("Open Input weight file");
        directoryWInput = String.valueOf(chooser.getCurrentDirectory()) +
            File.separatorChar;

        int returnValue = chooser.showOpenDialog(UI.getMainFrame());

        if (returnValue == JFileChooser.APPROVE_OPTION) {
          fileNameWInput = chooser.getSelectedFile().getName();
          directoryWInput = String.valueOf(chooser.getCurrentDirectory()) +
              File.separatorChar;
          UI.setDefaultDirectory(directoryWInput);
        }
        else {
          fileNameWInput = null;
          return;
        }
        if (fileNameWInput != null)
          textInput.setText(fileNameWInput);
      }
      catch (OutOfMemoryError e) {
        MipavUtil.displayError("Out of memory in JDialogRegistrationOAR25D.");
        return;
      }
    }
    else if (command.equals("AdvancedOkay")) {
        tmpStr=bracketBoundText.getText();
        if (testParameter(tmpStr, 1, 50)) {
            bracketBound = Integer.valueOf(tmpStr).intValue();
        }
        else {
            bracketBound = bracketBound_def;
        }
        tmpStr=maxIterationsText.getText();
        if (testParameter(tmpStr, 1, 100)) {
            maxIterations = Integer.valueOf(tmpStr).intValue();
        }
        else {
            maxIterations = maxIterations_def;
        }
        tmpStr=numMinText.getText();
        if (testParameter(tmpStr, 1, 25)) {
            numMinima = Integer.valueOf(tmpStr).intValue();
        }
        else {
            numMinima = numMinima_def;
        }
        advancedDialog.setVisible(false);
        advancedDialog.dispose();
    }
    else if (command.equals("AdvancedCancel")) {
        maxIterations=maxIterations_def;
        bracketBound=bracketBound_def;
        numMinima = numMinima_def;
        advancedDialog.setVisible(false);
        advancedDialog.dispose();
    }
  }

  /**
   *	Changes the interpolation box to enabled or disabled depending on if
   *	the transform box is checked or not.
   *	@param event	Event that triggered this function.
   */
  public void itemStateChanged(ItemEvent event) {
    if ( (event.getSource() == weightRadio) || (event.getSource() == noneRadio) ||
        (event.getSource() == voiRadio)) {
      buttonWeightInput.setEnabled(weightRadio.isSelected());
      if (weightRadio.isSelected()) {
        comboBoxDOF.setSelectedIndex(3);
      } // if (weightRadio.isSelected())
    } // if ((event.getSource() == weightRadio) || (event.getSource() == noneRadio) ||
    //  (event.getSource() == voiRadio))
    else if (event.getSource() == comboBoxDOF) {
      if ( (comboBoxDOF.getSelectedIndex() == 0) &&
          ((refImageRButton.isSelected()) || (averageImageRButton.isSelected()))) {
        graphCheckBox.setEnabled(true);
      }
      else {
        graphCheckBox.setEnabled(false);
        graphCheckBox.setSelected(false);
      }
    } // else if (event.getSource() == comboBoxDOF)
    else if ( (event.getSource() == adjacentImageRButton) ||
             (event.getSource() == averageImageRButton) ||
             (event.getSource() == refImageRButton)) {
      if ( (comboBoxDOF.getSelectedIndex() == 0) &&
          ((refImageRButton.isSelected()) || (averageImageRButton.isSelected()))) {
        graphCheckBox.setEnabled(true);
      }
      else {
        graphCheckBox.setEnabled(false);
        graphCheckBox.setSelected(false);
      }
    }
    else if (event.getSource() == fastModeCheckbox) {
      //enable or disable search variables
      fastMode = fastModeCheckbox.isSelected();
      rotateBeginText.setEnabled(!fastModeCheckbox.isSelected()); ;
      rotateEndText.setEnabled(!fastModeCheckbox.isSelected()); ;
      coarseRateText.setEnabled(!fastModeCheckbox.isSelected()); ;
      fineRateText.setEnabled(!fastModeCheckbox.isSelected()); ;
    }
  }

  /**
   * Sets it to be /not be a script
   * @param isScript is this a script running?
   */
  private void setIsScript(boolean isScript) {
    this.isScript = isScript;
  }

  /**
   * Build advanced settings dialog.  Returns JDialog.
   */
   private JDialog buildAdvancedDialog(int bracketBound, int maxIter, int numMinima) {
     serif12  = MipavUtil.font12;
     serif12B = MipavUtil.font12B;

     advancedDialog = new JDialog(this, "Advanced OAR settings", true);
                // Parent is the JDialogRegistrationOAR3D, title, modal

     // Setting panel
     JPanel settingsPanel = new JPanel();
     settingsPanel.setBorder(BorderFactory.createTitledBorder("Optimization settings"));
     settingsPanel.setLayout(new BoxLayout(settingsPanel, BoxLayout.Y_AXIS));

     JPanel bracketPanel = new JPanel();
     bracketPanel.setLayout(new BorderLayout(1,3));  // BorderLayout(int hgap, int vgap)
     bracketPanel.setBorder(BorderFactory.createEmptyBorder(0,10,0,10));
     JLabel bracketBoundLabel = new JLabel("Multiple of tolerance to bracket the minimum: ", JLabel.LEFT);
     bracketPanel.add(bracketBoundLabel, BorderLayout.WEST);
     bracketPanel.setToolTipText("Used for translation, scale and skew.");
     bracketBoundText = new JTextField(String.valueOf(bracketBound), 5);
     bracketBoundText.addFocusListener(this);
     bracketPanel.add(bracketBoundText, BorderLayout.CENTER);
     JLabel bracketInstruct = new JLabel("Recommended values 10-60.", JLabel.RIGHT);
     bracketPanel.add(bracketInstruct, BorderLayout.SOUTH);

     JPanel maxIterPanel = new JPanel();
     maxIterPanel.setLayout(new BorderLayout(1,3));
     maxIterPanel.setBorder(BorderFactory.createEmptyBorder(0,10,0,10));
     JLabel maxIterationsLabel = new JLabel ("Number of iterations: ", JLabel.LEFT);
     maxIterPanel.add(maxIterationsLabel, BorderLayout.WEST);
     maxIterPanel.setToolTipText("Used for levelOne. Other levels are multiples of this #.");
     maxIterationsText = new JTextField(String.valueOf(maxIter), 5);
     maxIterationsText.addFocusListener(this);
     maxIterPanel.add(maxIterationsText, BorderLayout.CENTER);
      JLabel maxIterInstruct = new JLabel("Recommended value 1-5.", JLabel.RIGHT);
     maxIterPanel.add(maxIterInstruct, BorderLayout.SOUTH);

     JPanel numMinPanel = new JPanel();
     numMinPanel.setLayout(new BorderLayout(1,3));
     numMinPanel.setBorder(BorderFactory.createEmptyBorder(0,10,0,10));
     JLabel numMinLabel = new JLabel("Number of minima from Level 8 to test at Level 4: ", JLabel.LEFT);
     numMinPanel.add(numMinLabel, BorderLayout.WEST);
     numMinPanel.setToolTipText("Increasing will significantly increase processing time.");
     numMinText = new JTextField(String.valueOf(numMinima), 5);
     numMinText.addFocusListener(this);
     numMinPanel.add(numMinText, BorderLayout.CENTER);

     fastModeCheckbox = new JCheckBox(
        "Skip multilevel search.  Assume images are close to alignment.");
     fastModeCheckbox.setFont(serif12);
     fastModeCheckbox.setForeground(Color.black);
     fastModeCheckbox.setSelected(false);
     fastModeCheckbox.setEnabled(true);
     fastModeCheckbox.addItemListener(this);
     fastModeCheckbox.setAlignmentX(Component.LEFT_ALIGNMENT);


     settingsPanel.add(bracketPanel);
     settingsPanel.add(Box.createVerticalStrut(20));
     settingsPanel.add(maxIterPanel);
     settingsPanel.add(Box.createVerticalStrut(20));
     settingsPanel.add(numMinPanel);
     settingsPanel.add(Box.createVerticalStrut(15));
     settingsPanel.add(fastModeCheckbox);

     advancedDialog.getContentPane().add(settingsPanel, BorderLayout.NORTH);

     // Okay-Cancel Panel
     JPanel okayCancelPanel = new JPanel(new FlowLayout());
     JButton cancelButton = new JButton("Cancel");
     cancelButton.setActionCommand("AdvancedCancel");
     cancelButton.addActionListener(this);
     cancelButton.setPreferredSize(new Dimension(120,30));
     cancelButton.setFont(serif12B);
     //okayCancelPanel.add(cancelButton);
     JButton okayButton = new JButton("OK");
     okayButton.setActionCommand("AdvancedOkay");
     okayButton.addActionListener(this);
     okayButton.setPreferredSize(new Dimension(120,30));
     okayButton.setFont(serif12B);
     okayCancelPanel.add(okayButton);
     okayCancelPanel.add(cancelButton);

     advancedDialog.getContentPane().add(okayCancelPanel, BorderLayout.SOUTH);

     Rectangle dialogBounds = this.getBounds();
     advancedDialog.setLocation(
        (int)(Toolkit.getDefaultToolkit().getScreenSize().width*0.75-dialogBounds.width/2),
              Toolkit.getDefaultToolkit().getScreenSize().height/2-dialogBounds.height/2);

     advancedDialog.pack();
     advancedDialog.setVisible(true);

     return advancedDialog;
   }

  /**
   *	Sets the variables needed to call the registration algorithm based on
   *	the values entered in the dialog.
   *	@return	<code>true</code> if the variables are properly set, <code>false</code> otherwise.
   */
  private boolean setVariables() {
    int i;
    int nVOI;
    weighted = weightRadio.isSelected();
    voisOnly = voiRadio.isSelected();

    if (weighted) {
      fileNameWInput = textInput.getText();
      try {
        FileIO fileIO = new FileIO();
        inputWeightImage = fileIO.readImage(fileNameWInput, directoryWInput, false, null);
        if (inputWeightImage == null) {
          MipavUtil.displayError("Input weight image is not valid.");
          return false;
        }
        else if (inputWeightImage.getNDims() != matchImage.getNDims()) {
          MipavUtil.displayError(
              "Dimensions of input weight image must match the input image.");
          return false;
        }
        for (i = 0; i < matchImage.getNDims(); i++) {
          if (matchImage.getExtents()[i] != inputWeightImage.getExtents()[i]) {
            MipavUtil.displayError(
                "Dimensions of input weight image must match the input image.");
            return false;
          }
        }
      }
      catch (OutOfMemoryError e) {
        MipavUtil.displayError("Out of memory in JDialogRegistrationOAR25D");
        return false;
      }
    }

    if (doColor) {
      if ((!weighted) && (!voisOnly)) {
        switch (comboBoxCostFunct.getSelectedIndex()) {
          case 0:
            cost = AlgorithmCostFunctions.LEAST_SQUARES_SMOOTHED_COLOR;
            break;
        }
      }
      else {
        switch (comboBoxCostFunct.getSelectedIndex()) {
          case 0:
            cost = AlgorithmCostFunctions.LEAST_SQUARES_SMOOTHED_WGT_COLOR;
            break;
        }
      }
    } // if (doColor)
    else { // black and white
      if ((!weighted) && (!voisOnly)) {
        switch (comboBoxCostFunct.getSelectedIndex()) {
          case 0:
            cost = AlgorithmCostFunctions.CORRELATION_RATIO_SMOOTHED;
            break;
          case 1:
            cost = AlgorithmCostFunctions.LEAST_SQUARES_SMOOTHED;
            break;
            //case 2:  cost = AlgorithmCostFunctions.MUTUAL_INFORMATION_SMOOTHED;             break;
          case 2:
            cost = AlgorithmCostFunctions.NORMALIZED_XCORRELATION_SMOOTHED;
            break;
          case 3:
            cost = AlgorithmCostFunctions.
                NORMALIZED_MUTUAL_INFORMATION_SMOOTHED;
            break;
          default:
            cost = AlgorithmCostFunctions.CORRELATION_RATIO_SMOOTHED;
            break;
        }
      }
      else {
        switch (comboBoxCostFunct.getSelectedIndex()) {
          case 0:
            cost = AlgorithmCostFunctions.CORRELATION_RATIO_SMOOTHED_WGT;
            break;
          case 1:
            cost = AlgorithmCostFunctions.LEAST_SQUARES_SMOOTHED_WGT;
            break;
            //case 2:  cost = AlgorithmCostFunctions.MUTUAL_INFORMATION_SMOOTHED_WGT;           break;
          case 2:
            cost = AlgorithmCostFunctions.NORMALIZED_XCORRELATION_SMOOTHED_WGT;
            break;
          case 3:
            cost = AlgorithmCostFunctions.
                NORMALIZED_MUTUAL_INFORMATION_SMOOTHED_WGT;
            break;
          default:
            cost = AlgorithmCostFunctions.CORRELATION_RATIO_SMOOTHED_WGT;
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

    if (!testParameter(rotateBeginText.getText(), -360, 360)) {
      rotateBeginText.requestFocus();
      rotateBeginText.selectAll();
      return false;
    }
    else
      rotateBegin = Float.valueOf(rotateBeginText.getText()).floatValue();
    if (!testParameter(rotateEndText.getText(), -360, 360)) {
      rotateEndText.requestFocus();
      rotateEndText.selectAll();
      return false;
    }
    else
      rotateEnd = Float.valueOf(rotateEndText.getText()).floatValue();
    if (!testParameter(coarseRateText.getText(), 0.01, 360)) {
      coarseRateText.requestFocus();
      coarseRateText.selectAll();
      return false;
    }
    else
      coarseRate = Float.valueOf(coarseRateText.getText()).floatValue();
    if (rotateBegin > rotateEnd) {
      MipavUtil.displayError(
          "Beginning of range must be less than end of range.");
      rotateBeginText.requestFocus();
      rotateBeginText.selectAll();
      return false;
    }
    if ( (rotateEnd - rotateBegin) / coarseRate < 1) {
      int response = JOptionPane.showConfirmDialog(this,
          "Warning: with such a large rate, there will only be 1 sampling.  Continue?",
          "Sampling warning", JOptionPane.YES_NO_OPTION,
          JOptionPane.WARNING_MESSAGE);
      if (response == JOptionPane.NO_OPTION) {
        coarseRateText.requestFocus();
        coarseRateText.selectAll();
        return false;
      }
    }

    if (!testParameter(rotateBeginText.getText(), -360, 360)) {
      rotateBeginText.requestFocus();
      rotateBeginText.selectAll();
      return false;
    }
    else
      rotateBegin = Float.valueOf(rotateBeginText.getText()).floatValue();
    if (!testParameter(rotateEndText.getText(), -360, 360)) {
      rotateEndText.requestFocus();
      rotateEndText.selectAll();
      return false;
    }
    else
      rotateEnd = Float.valueOf(rotateEndText.getText()).floatValue();

    if (!testParameter(fineRateText.getText(), 0.01, 360)) {
      fineRateText.requestFocus();
      fineRateText.selectAll();
      return false;
    }
    else
      fineRate = Float.valueOf(fineRateText.getText()).floatValue();

    if (rotateBegin > rotateEnd) {
      MipavUtil.displayError(
          "Beginning of range must be less than end of range.");
      rotateBeginText.requestFocus();
      rotateBeginText.selectAll();
      return false;
    }

    if ( (rotateEnd - rotateBegin) / fineRate < 1) {
      int response = JOptionPane.showConfirmDialog(this,
          "Warning: with such a large rate, there will only be 1 sampling.  Continue?",
          "Sampling warning", JOptionPane.YES_NO_OPTION,
          JOptionPane.WARNING_MESSAGE);
      if (response == JOptionPane.NO_OPTION) {
        coarseRateText.requestFocus();
        coarseRateText.selectAll();
        return false;
      }
    }

    if (!testParameter(refImageNumText.getText(), 1, matchImage.getExtents()[3])) {
      refImageNumText.requestFocus();
      refImageNumText.selectAll();
      return false;
    }
    else
      refImageNum = Integer.valueOf(refImageNumText.getText()).intValue() - 1;

    if (adjacentImageRButton.isSelected()) {
      registerTo = 1;
    }
    else if (averageImageRButton.isSelected()) {
      registerTo = 2;
    }
    else {  // reference
      registerTo = 3;
    }

    doGraph = graphCheckBox.isSelected();

    if (voisOnly) {
      // check that there actually are VOIs there
      // and propagate the VOIs to all slices
      ViewVOIVector VOIs = (ViewVOIVector) matchImage.getVOIs();
      nVOI = VOIs.size();
      if (nVOI < 1) {
        MipavUtil.displayError("There must be at least one VOI in " +
                               matchImage.getImageName() + " to register.");
        return false;
      }
    } // if (voisOnly)


    doSubsample = sampleCheckBox.isSelected();

    return true;
  }

  /**
   *	Calls the algorithm with the set-up parameters.
   */
  private void callAlgorithm() {
    BitSet mask = null;
    if (voisOnly) {
      float[] matchRes = new float[] {
          matchImage.getFileInfo(0).getResolutions()[0],
          matchImage.getFileInfo(0).getResolutions()[1],
          matchImage.getFileInfo(0).getResolutions()[2],
          matchImage.getFileInfo(0).getResolutions()[3]};

      inputWeightImage = new ModelImage(ModelStorageBase.BYTE,
                                        matchImage.getExtents(), "VOI match",
                                        matchImage.getUserInterface());

      inputWeightImage.getFileInfo(0).setResolutions(matchRes);
      // make new input image based on the VOIs.
      // pass those new image to the registration algorithm

      mask = matchImage.generateVOIMask();
      int matchImageSize = matchImage.getSliceSize() * matchImage.getExtents()[2];
      for (int j = 0; j < matchImage.getExtents()[3]; j++) {
          for (int i = 0; i < matchImageSize; i++) {
              if (!mask.get(i)) {
                  inputWeightImage.set(j*matchImageSize + i, 0);
              } else {
                  inputWeightImage.set(j*matchImageSize + i, 1);
              }
          }
      }
      weighted = true;

    } // if (voisOnly)

    if (weighted) {
      reg35 = new AlgorithmRegOAR35D(matchImage, inputWeightImage, cost, DOF,
                                      interp, interp2,
                                      registerTo, refImageNum,
                                      rotateBegin, rotateEnd,
                                      coarseRate, fineRate,
                                      doGraph, doSubsample, fastMode,
                                      bracketBound, maxIterations, numMinima);
    }
    else {

      reg35 = new AlgorithmRegOAR35D(matchImage, cost, DOF, interp, interp2,
                                      registerTo, refImageNum,
                                      rotateBegin, rotateEnd,
                                      coarseRate, fineRate,
                                      doGraph, doSubsample, fastMode,
                                      bracketBound, maxIterations, numMinima);
      if (useOutsideReferenceVolume) {

        if (!reg35.setReferenceVolume(refVolume)) {
          MipavUtil.displayError("Reference volume does not have same extents as input image");
        }
      }

    }
    // Start the thread as a low priority because we wish to still have user interface work fast.
    reg35.addListener(this);
    // Hide dialog
    setVisible(false);
    if (isScript) {
      reg35.setActiveImage(isActiveImage);
      if (!UI.isAppFrameVisible()) {
        reg35.setProgressBarVisible(false);
      }
      reg35.run();
    }
    else {
      if (reg35.startMethod(Thread.MIN_PRIORITY) == false) {
        MipavUtil.displayError("A thread is already running on this object");
      }
    }
  }

  //************************************************************************
   //************************** Algorithm Events ****************************
    //************************************************************************

     /**
      *	This method is required if the AlgorithmPerformed interface is implemented. It is called by the
      *   algorithms when it has completed or failed to complete.
      *   @param algorithm   Algorithm that caused the event.
      */
     public void algorithmPerformed(AlgorithmBase algorithm) {
       int i;
       ViewJFrameImage imageFrame = null;
       float rot[][] = null;
       float posR[][] = null;
       float trans[][] = null;
       float posT[][] = null;
       if (algorithm instanceof AlgorithmRegOAR35D) {

         if (reg35.isCompleted()) {
           /*
           String name = makeImageName(matchImage.getImageName(), "_registered");

           resultImage = reg25.getTransformedImage();
           resultImage.calcMinMax();
           resultImage.setImageName(name);
           resultImage.getFileInfo()[0].setFileName(name + ".avi");
           resultImage.clearMask();
           matchImage.clearMask();

           if (resultImage != null) {
             try {
               imageFrame = new ViewJFrameImage(resultImage, null,
                                                new Dimension(610, 200), UI);
             }
             catch (OutOfMemoryError error) {
               MipavUtil.displayError("Out of memory: unable to open new frame");
             }
           }
           else {
             MipavUtil.displayError("Result Image is null");
           }
           */
           if (doGraph) {
             rot = reg35.getRot();
             posR = new float[3][matchImage.getExtents()[3]];
             for (i = 0; i < matchImage.getExtents()[3]; i++) {
               posR[0][i] = i + 1.0f;
               posR[1][i] = i + 1.0f;
               posR[2][i] = i + 1.0f;
             }
             ViewJFrameGraph rotGraph = new ViewJFrameGraph(posR, rot,
                 "Rotations","Volume number", "Degrees");
             rotGraph.makeRangeSymmetric();
             rotGraph.showXYZLegends();
             rotGraph.setDefaultDirectory(UI.getDefaultDirectory());
             rotGraph.setVisible(true);
             trans = reg35.getTrans();
             posT = new float[3][matchImage.getExtents()[3]];
             for (i = 0; i < matchImage.getExtents()[3]; i++) {
               posT[0][i] = i + 1.0f;
               posT[1][i] = i + 1.0f;
               posT[2][i] = i + 1.0f;
             }
             ViewJFrameGraph transGraph = new ViewJFrameGraph(posT, trans,
                 "Translations",
                 "Volume number", "Translations in " +
                 FileInfoBase.getUnitsOfMeasureAbbrevStr(matchImage.getFileInfo(
                 0).
                 getUnitsOfMeasure(0)));
             transGraph.makeRangeSymmetric();
             transGraph.showXYZLegends();
             transGraph.setDefaultDirectory(UI.getDefaultDirectory());
             transGraph.setVisible(true);
           } // if (doGraph)


           insertScriptLine(algorithm);
         } // isCompleted

         if (reg35 != null)
           reg35.disposeLocal();
         reg35 = null;

         matchImage = null;

         if (inputWeightImage != null)
           inputWeightImage.disposeLocal();
         inputWeightImage = null;

         rot = null;
         if (posR != null) {
           for (i = 0; i < posR.length; i++) {
             posR[i] = null;
           }
           posR = null;
         }
         if (trans != null) {
           for (i = 0; i < trans.length; i++) {
             trans[i] = null;
           }
           trans = null;
         }
         if (posT != null) {
           for (i = 0; i < posT.length; i++) {
             posT[i] = null;
           }
           posT = null;
         }
         dispose();
         System.gc();
       }
     }

}
