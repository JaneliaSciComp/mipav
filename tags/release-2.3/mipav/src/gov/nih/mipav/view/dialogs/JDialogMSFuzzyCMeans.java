package gov.nih.mipav.view.dialogs;

import gov.nih.mipav.view.*;
import gov.nih.mipav.model.structures.*;
import gov.nih.mipav.model.file.*;
import gov.nih.mipav.model.algorithms.*;

import java.awt.event.*;
import java.awt.*;
import java.io.File;

import javax.swing.*;
import javax.swing.event.*;

/**
 *   Dialog to get user input, then call the algorithm.
 *
 */
public class JDialogMSFuzzyCMeans
    extends JDialogBase
    implements AlgorithmInterface,
    ScriptableInterface,
    ListSelectionListener {

  private AlgorithmMSpectralFuzzyCMeans afcmAlgo;
  private ModelImage srcImage[] = null; // all source images
  private ModelImage resultImage[] = null; // result image
  private ModelImage resultImage0 = null;
  private ModelImage resultImage1 = null;
  private ModelImage resultImage2 = null;
  private ModelImage resultImage3 = null;
  private ModelImage resultImage4 = null;
  private ModelImage resultImage5 = null;
  private ModelImage resultImage6 = null;
  private ModelImage resultImage7 = null;
  private ModelImage resultImage8 = null;
  private ModelImage resultImage9 = null;
  private ModelImage resultImage10 = null;
  private ModelImage tempImage[] = null;
  private int resultNumber;

  private JPanel imagePanel;
  private JPanel paramPanel;
  private JTextField textNClasses;
  private JLabel labelNClasses;
  private int nClasses;

  private JTextField textNPyramid;
  private JLabel labelNPyramid;
  private int nPyramid = 4;

  private JTextField textExpo;
  private JLabel labelExpo;
  private float q;

  private JTextField textEndTol;
  private JLabel labelEndTol;
  private float endTol;

  private JTextField textMaxIter;
  private JLabel labelMaxIter;
  private int maxIter;

  private JTextField textOneSmooth;
  private JTextField textTwoSmooth;
  private JLabel labelSmooth;
  private float oneSmooth = 5e4f;
  private float twoSmooth = 5e5f;

  private JTextField textOneJacobiIter;
  private JTextField textTwoJacobiIter;
  private JLabel labelJacobi;
  private int oneJacobiIter = 2;
  private int twoJacobiIter = 3;

  private JCheckBox cropCheckbox;
  private boolean cropBackground;

  //private     JCheckBox    calcGainFieldCheckbox;
  private boolean outputGainField = false;

  private JPanel segmentationPanel;
  private ButtonGroup segmentationGroup;
  private JRadioButton hardOnly;
  private JRadioButton fuzzyOnly;
  private JRadioButton hardFuzzyBoth;
  private int segmentation;
  private int destExtents[];
  private int presentNumber;
  private int srcNumber = 1;

  private JButton chooserButton;
  private ViewUserInterface userInterface;
  private Dimension newFrameLocation;

  private JLabel labelLoaded;
  private JList imageList;
  private DefaultListModel model;
  private int removeIndex;
  private boolean changeRemoveIndex = true;
  private JButton removeButton;
  private boolean doColor;
  private JPanel colorPanel;
  private JCheckBox redCheckbox;
  private JCheckBox greenCheckbox;
  private JCheckBox blueCheckbox;
  private boolean doRed;
  private boolean doGreen;
  private boolean doBlue;
  private float[] centroids;
  private float[] threshold;

  private JPanel imageVOIPanel;
  private ButtonGroup imageVOIGroup;
  private JRadioButton wholeImage;
  private JRadioButton VOIRegions;

  private boolean regionFlag; // true = apply algorithm to the whole image
  // false = apply algorithm only to VOI regions

  /**
   *  @param parent          parent frame
   *  @param im              source image
   */
  public JDialogMSFuzzyCMeans(Frame theParentFrame, ModelImage im) {
    super(theParentFrame, true);
    srcImage = new ModelImage[1];
    srcImage[0] = im;
    userInterface = ( (ViewJFrameBase) (parentFrame)).getUserInterface();
    if (im.isColorImage()) {
      doColor = true;
      doRed = true;
      doBlue = true;
      doGreen = true;
    }
    else {
      doColor = false;
      doRed = false;
      doBlue = false;
      doGreen = false;
    }
    if (srcImage[0].getNDims() == 2) { // source image is 2D
      destExtents = new int[2];
      destExtents[0] = srcImage[0].getExtents()[0]; // X dim
      destExtents[1] = srcImage[0].getExtents()[1]; // Y dim
    }
    else { // srcImage[0].getNDims)() == 3
      destExtents = new int[3];
      destExtents[0] = srcImage[0].getExtents()[0];
      destExtents[1] = srcImage[0].getExtents()[1];
      destExtents[2] = srcImage[0].getExtents()[2];
    }

    init();
  }

  /**
       *	Used primarily for the script to store variables and run the algorithm.  No
   *	actual dialog will appear but the set up info and result image will be stored here.
   *	@param UI   The user interface, needed to create the image frame.
   *	@param im	Source image.
   */
  public JDialogMSFuzzyCMeans(ViewUserInterface UI, ModelImage im) {
    super();
    userInterface = UI;
    srcImage = new ModelImage[1];
    srcImage[0] = im;
    if (im.isColorImage()) {
      doColor = true;
      doRed = true;
      doBlue = true;
      doGreen = true;
    }
    else {
      doColor = false;
      doRed = false;
      doBlue = false;
      doGreen = false;
    }
    if (srcImage[0].getNDims() == 2) { // source image is 2D
      destExtents = new int[2];
      destExtents[0] = srcImage[0].getExtents()[0]; // X dim
      destExtents[1] = srcImage[0].getExtents()[1]; // Y dim
    }
    else { // srcImage[0].getNDims)() == 3
      destExtents = new int[3];
      destExtents[0] = srcImage[0].getExtents()[0];
      destExtents[1] = srcImage[0].getExtents()[1];
      destExtents[2] = srcImage[0].getExtents()[2];
    }
  }

  /**
   * Empty constructor needed for dynamic instantiation (used during scripting).
   */
  public JDialogMSFuzzyCMeans() {}

  /**
   * Run this algorithm from a script.
   * @param parser the script parser we get the state from
   * @throws IllegalArgumentException if there is something wrong with the arguments in the script
   */
  public void scriptRun (AlgorithmScriptParser parser) throws IllegalArgumentException {
      String srcImageKey = null;

      try {
          srcImageKey = parser.getNextString();
      } catch (Exception e) {
          throw new IllegalArgumentException();
      }
      ModelImage im = parser.getImage(srcImageKey);

      srcImage = new ModelImage[1];
      srcImage[0] = im;
      if (im.isColorImage()) {
          doColor = true;
          doRed = true;
          doBlue = true;
          doGreen = true;
      }
      else {
          doColor = false;
          doRed = false;
          doBlue = false;
          doGreen = false;
      }
      if (srcImage[0].getNDims() == 2) { // source image is 2D
          destExtents = new int[2];
          destExtents[0] = srcImage[0].getExtents()[0]; // X dim
          destExtents[1] = srcImage[0].getExtents()[1]; // Y dim
      }
      else { // srcImage[0].getNDims)() == 3
          destExtents = new int[3];
          destExtents[0] = srcImage[0].getExtents()[0];
          destExtents[1] = srcImage[0].getExtents()[1];
          destExtents[2] = srcImage[0].getExtents()[2];
      }
      userInterface = srcImage[0].getUserInterface();
      parentFrame = srcImage[0].getParentFrame();

      String[] results;
      ModelImage[] sources;
      try {
          int length = parser.getNextInteger();
          sources = new ModelImage[length];
          sources[0] = im;
          for (int i = 1; i < length; i++) {
              sources[i] = parser.getImage(parser.getNextString());
          }
          setSourceImage(sources);

          int numImages = parser.getNextInteger();
          results = new String[numImages];
          for (int i = 0; i < numImages; i++) {
              results[i] = parser.getNextString();
          }
      }
      catch (Exception e) {
          throw new IllegalArgumentException();
      }

      try {
          setRegionFlag(parser.getNextBoolean());
          int nClasses = parser.getNextInteger();
          setNClasses(nClasses);
          setQ(parser.getNextFloat());
          setCrop(parser.getNextBoolean());
          setEndTol(parser.getNextFloat());
          setMaxIter(parser.getNextInteger());
          setSegmentationType(parser.getNextInteger());
          boolean red = parser.getNextBoolean();
          boolean green = parser.getNextBoolean();
          boolean blue = parser.getNextBoolean();
          setRed(red);
          setBlue(blue);
          setGreen(green);
          int length2 = 0;
          for (int i = 0; i < sources.length; i++) {
              if (red)
                  length2++;
              if (green)
                  length2++;
              if (blue)
                  length2++;
              if (!red && !green && !blue)
                  length2++;
          }
          float[] threshold = new float[length2];
          float[] centroids = new float[length2 * nClasses];

          for (int i = 0; i < threshold.length; i++)
              threshold[i] = parser.getNextFloat();
          for (int i = 0; i < centroids.length; i++)
              centroids[i] = parser.getNextFloat();
          setThreshold(threshold);
          setCentroids(centroids);
      } catch (Exception e) {
          throw new IllegalArgumentException();
      }

      setActiveImage(parser.isActiveImage());
      setSeparateThread(false);

      for (int i = 0; i < srcImage.length; i++) {
        if (!checkImage(srcImage[i]))
          return;
      }

      callAlgorithm();
      for (int i = 0; i < results.length; i++) {
          parser.putVariable(results[i], getResultImage()[i].getImageName());
      }
  }

  /**
   * If a script is being recorded and the algorithm is done, add an entry for this algorithm.
   * @param algo the algorithm to make an entry for
   */
  public void insertScriptLine (AlgorithmBase algo) {
      if (algo.isCompleted()) {
          if (userInterface.isScriptRecording()) {
              //check to see if the  srcImage[0] is already in the ImgTable
              if (userInterface.getScriptDialog().getImgTableVar(srcImage[0].getImageName()) == null) {
                  if (userInterface.getScriptDialog().getActiveImgTableVar(srcImage[0].getImageName()) == null) {
                      userInterface.getScriptDialog().putActiveVar(srcImage[0].getImageName());
                  }
              }

              userInterface.getScriptDialog().append("MSFuzzyCMeans " +
                                                     userInterface.getScriptDialog().
                                                     getVar(srcImage[0].
                                                            getImageName()) + " " + srcImage.length + " ");
              for (int i = 1; i < srcImage.length; i++) {
                  //check to see if the  srcImage[i] is already in the ImgTable
                  if (userInterface.getScriptDialog().getImgTableVar(srcImage[i].getImageName()) == null) {
                      if (userInterface.getScriptDialog().getActiveImgTableVar(srcImage[i].getImageName()) == null) {
                          userInterface.getScriptDialog().putActiveVar(srcImage[i].getImageName());
                      }
                  }

                  userInterface.getScriptDialog().append(userInterface.
                                                         getScriptDialog().getVar(srcImage[i].getImageName()) + " ");
              }
              userInterface.getScriptDialog().append(resultNumber + " ");
              for (int i = 0; i < resultNumber; i++) {
                  userInterface.getScriptDialog().putVar(resultImage[i].getImageName());
                  userInterface.getScriptDialog().append(userInterface.
                                                         getScriptDialog().getVar(resultImage[i].getImageName()) +
                                                         " ");
              } // for (i = 0; i < resultNumber; i++)
              userInterface.getScriptDialog().append(regionFlag + " " + nClasses +
                                                     " " + q + " " +
                                                     cropBackground + " " +
                                                     endTol + " " + maxIter + " " +
                                                     segmentation + " ");
              String temp = "";
              for (int i = 0; i < threshold.length; i++) {
                  temp += threshold[i] + " ";
              }
              for (int i = 0; i < centroids.length; i++) {
                  temp += centroids[i] + " ";
              }
              userInterface.getScriptDialog().append(doRed + " " + doGreen + " " +
                                                     doBlue + " " + temp + "\n");
          }
      }
  }

  /**
   *	Sets up the GUI (panels, buttons, etc) and displays it on the screen.
   */
  private void init() {
    setForeground(Color.black);
    newFrameLocation = userInterface.getNewFrameYLocation();

    setTitle("Fuzzy C-means");

    labelNClasses = new JLabel("Number of desired classes");
    labelNClasses.setForeground(Color.black);
    labelNClasses.setFont(serif12);

    textNClasses = new JTextField(5);
    textNClasses.setText("3");
    textNClasses.setFont(serif12);

    /* labelNPyramid = new JLabel("Number of pyramid levels.");
          labelNPyramid.setForeground(Color.black);
       labelNPyramid.setFont(serif12);
       textNPyramid = new JTextField(5);
       textNPyramid.setText("4");
       textNPyramid.setFont(serif12);*/

    labelExpo = new JLabel("Desired exponent value");
    labelExpo.setForeground(Color.black);
    labelExpo.setFont(serif12);

    textExpo = new JTextField(5);
    textExpo.setText("2");
    textExpo.setFont(serif12);

    cropCheckbox = new JCheckBox("Background cropping");
    cropCheckbox.setFont(serif12);
    cropCheckbox.setSelected(false);
    cropCheckbox.addActionListener(this);
    cropCheckbox.setActionCommand("Crop");

    labelEndTol = new JLabel("End tolerance");
    labelEndTol.setForeground(Color.black);
    labelEndTol.setFont(serif12);

    textEndTol = new JTextField(5);
    textEndTol.setText("0.01");
    textEndTol.setFont(serif12);

    labelMaxIter = new JLabel("Maximum number of iterations");
    labelMaxIter.setForeground(Color.black);
    labelMaxIter.setFont(serif12);

    textMaxIter = new JTextField(5);
    textMaxIter.setText("200");
    textMaxIter.setFont(serif12);

    JPanel upperPanel = new JPanel(new GridBagLayout());

    GridBagConstraints gbc = new GridBagConstraints();
    gbc.gridwidth = 1;
    gbc.gridheight = 1;
    gbc.anchor = gbc.WEST;
    gbc.insets = new Insets(5, 5, 5, 5);

    gbc.gridx = 0;
    gbc.gridy = 0;
    gbc.weightx = 0;
    gbc.fill = gbc.NONE;
    upperPanel.add(labelNClasses, gbc);
    gbc.gridx = 1;
    gbc.gridy = 0;
    gbc.weightx = 1;
    gbc.fill = gbc.HORIZONTAL;
    upperPanel.add(textNClasses, gbc);
    gbc.gridx = 0;
    gbc.gridy = 1;
    gbc.weightx = 0;
    gbc.fill = gbc.NONE;
    upperPanel.add(labelExpo, gbc);
    gbc.gridx = 1;
    gbc.gridy = 1;
    gbc.weightx = 1;
    gbc.fill = gbc.HORIZONTAL;
    upperPanel.add(textExpo, gbc);
    gbc.gridx = 0;
    gbc.gridy = 2;
    gbc.weightx = 0;
    gbc.fill = gbc.NONE;
    upperPanel.add(labelEndTol, gbc);
    gbc.gridx = 1;
    gbc.gridy = 2;
    gbc.weightx = 1;
    gbc.fill = gbc.HORIZONTAL;
    upperPanel.add(textEndTol, gbc);
    gbc.gridx = 0;
    gbc.gridy = 3;
    gbc.weightx = 0;
    gbc.fill = gbc.NONE;
    upperPanel.add(labelMaxIter, gbc);
    gbc.gridx = 1;
    gbc.gridy = 3;
    gbc.weightx = 1;
    gbc.fill = gbc.HORIZONTAL;
    upperPanel.add(textMaxIter, gbc);
    gbc.gridx = 0;
    gbc.gridy = 4;
    gbc.gridwidth = 2;
    upperPanel.add(cropCheckbox, gbc);

    /* labelSmooth = new JLabel("1st and 2nd order smoothness.");
          labelSmooth.setForeground(Color.black);
       labelSmooth.setFont(serif12);
       textOneSmooth = new JTextField(5);
       textOneSmooth.setText("50000");
       textOneSmooth.setFont(serif12);
       textTwoSmooth = new JTextField(5);
       textTwoSmooth.setText("500000");
       textTwoSmooth.setFont(serif12);
       labelJacobi = new JLabel("Number of Jacobi iterations.");
          labelJacobi.setForeground(Color.black);
       labelJacobi.setFont(serif12);
       textOneJacobiIter = new JTextField(5);
       textOneJacobiIter.setText("2");
       textOneJacobiIter.setFont(serif12);
       textTwoJacobiIter = new JTextField(5);
       textTwoJacobiIter.setText("3");
       textTwoJacobiIter.setFont(serif12); */

    imageVOIPanel = new JPanel(new GridBagLayout());
    imageVOIPanel.setForeground(Color.black);
    imageVOIPanel.setBorder(buildTitledBorder("1st image region"));

    imageVOIGroup = new ButtonGroup();
    wholeImage = new JRadioButton("Whole image", true);
    wholeImage.setFont(serif12);
    imageVOIGroup.add(wholeImage);

    VOIRegions = new JRadioButton("VOI region(s)", false);
    VOIRegions.setFont(serif12);
    imageVOIGroup.add(VOIRegions);

    gbc.gridx = 0;
    gbc.gridy = 0;
    gbc.weightx = 1;
    gbc.fill = gbc.BOTH;
    gbc.gridwidth = 1;
    gbc.insets = new Insets(0, 0, 0, 0);
    imageVOIPanel.add(wholeImage, gbc);
    gbc.gridy = 1;
    imageVOIPanel.add(VOIRegions, gbc);

    segmentationGroup = new ButtonGroup();
    hardOnly = new JRadioButton("Hard only", false);
    hardOnly.setFont(serif12);
    segmentationGroup.add(hardOnly);

    fuzzyOnly = new JRadioButton("Fuzzy only", false);
    fuzzyOnly.setFont(serif12);
    segmentationGroup.add(fuzzyOnly);

    hardFuzzyBoth = new JRadioButton("Hard and fuzzy both", true);
    hardFuzzyBoth.setFont(serif12);
    segmentationGroup.add(hardFuzzyBoth);

    segmentationPanel = new JPanel(new GridBagLayout());
    segmentationPanel.setBorder(buildTitledBorder("Segmentation"));

    gbc.gridx = 0;
    gbc.gridy = 0;
    gbc.weightx = 0;
    gbc.fill = gbc.NONE;
    gbc.gridwidth = 1;
    segmentationPanel.add(hardOnly, gbc);
    gbc.gridy = 1;
    segmentationPanel.add(fuzzyOnly, gbc);
    gbc.gridy = 2;
    segmentationPanel.add(hardFuzzyBoth, gbc);

    paramPanel = new JPanel(new GridBagLayout());
    paramPanel.setForeground(Color.black);
    paramPanel.setBorder(buildTitledBorder("Parameters"));

    gbc.gridx = 0;
    gbc.gridwidth = gbc.REMAINDER;
    gbc.gridy = 0;
    gbc.fill = gbc.BOTH;
    gbc.weightx = 1;
    paramPanel.add(upperPanel, gbc);
    gbc.gridy = 1;
    gbc.gridwidth = 1;
    paramPanel.add(imageVOIPanel, gbc);
    gbc.gridx = 1;
    paramPanel.add(segmentationPanel, gbc);

    if (doColor) {
      colorPanel = new JPanel(new GridBagLayout());
      colorPanel.setBorder(buildTitledBorder("Channels"));

      redCheckbox = new JCheckBox("Red");
      redCheckbox.setFont(serif12);
      redCheckbox.setSelected(true);
      redCheckbox.addActionListener(this);
      redCheckbox.setActionCommand("Red");

      greenCheckbox = new JCheckBox("Green");
      greenCheckbox.setFont(serif12);
      greenCheckbox.setSelected(true);
      greenCheckbox.addActionListener(this);
      greenCheckbox.setActionCommand("Green");

      blueCheckbox = new JCheckBox("Blue");
      blueCheckbox.setFont(serif12);
      blueCheckbox.setSelected(true);
      blueCheckbox.addActionListener(this);
      blueCheckbox.setActionCommand("Blue");

      gbc.gridx = 0;
      gbc.gridy = 0;
      colorPanel.add(redCheckbox, gbc);
      gbc.gridy = 1;
      colorPanel.add(greenCheckbox, gbc);
      gbc.gridy = 2;
      colorPanel.add(blueCheckbox, gbc);
      gbc.gridx = 1;
      colorPanel.add(Box.createHorizontalStrut(5), gbc);

      gbc.gridx = 2;
      gbc.gridy = 1;
      gbc.gridwidth = 1;
      paramPanel.add(colorPanel, gbc);

    } // if (doColor)

    /*calcGainFieldCheckbox = new JCheckBox("Generate gain field.");
       calcGainFieldCheckbox.setBounds(10, 425, 240, 25);
       calcGainFieldCheckbox.setFont(serif12);
       paramPanel.add(calcGainFieldCheckbox);
       calcGainFieldCheckbox.setSelected(false); */

    imagePanel = new JPanel(new BorderLayout());
    imagePanel.setBorder(buildTitledBorder("Load Image(s)"));

    model = new DefaultListModel();
    model.addElement(srcImage[0].getImageName());
    imageList = new JList(model);
    imageList.setVisibleRowCount(6);
    imageList.setPreferredSize(new Dimension(300, 120));
    imageList.setSelectionMode(ListSelectionModel.SINGLE_SELECTION);
    imageList.addListSelectionListener(this);
    imagePanel.add(imageList);

    JPanel chooserPanel = new JPanel();
    chooserButton = new JButton("Load");
    chooserButton.setPreferredSize(MipavUtil.defaultButtonSize);
    chooserButton.setFont(serif12B);
    chooserPanel.add(chooserButton);
    chooserButton.addActionListener(this);
    chooserButton.setActionCommand("Choose");

    removeButton = new JButton("Remove");
    removeButton.setPreferredSize(MipavUtil.defaultButtonSize);
    removeButton.setFont(serif12B);
    removeButton.setEnabled(false);
    chooserPanel.add(removeButton);
    removeButton.addActionListener(this);
    removeButton.setActionCommand("Remove");

    imagePanel.add(chooserPanel, BorderLayout.SOUTH);

    gbc.gridx = 0;
    gbc.gridwidth = gbc.REMAINDER;
    gbc.gridy = 2;
    gbc.fill = gbc.HORIZONTAL;
    gbc.weightx = 1;
    paramPanel.add(imagePanel, gbc);

    getContentPane().add(paramPanel);
    getContentPane().add(buildButtons(), BorderLayout.SOUTH);

    pack();
    setVisible(true);
  }

  /**
   *  Accessor that returns the image.
   *  @return          The result image.
   */
  public ModelImage[] getResultImage() {
    return resultImage;
  }

  /**
   *  Accessor that sets the array of source images.
   *  @param	The new source images.
   */
  public void setSourceImage(ModelImage[] images) {
    srcImage = images;
  }

  /**
   *	Accessor that sets the region flag.
   *	@param flag		<code>true</code> indicates the whole image is blurred, <code>false</code> indicates a region.
   */
  public void setRegionFlag(boolean flag) {
    regionFlag = flag;
  }

  /**
   *	Accessor that sets the crop background flag.
   *	@param flag		<code>true</code> indicates crop the background, <code>false</code> otherwise.
   */
  public void setCrop(boolean flag) {
    cropBackground = flag;
  }

  /**
   *	Accessor that sets the number of classes.
   *	@param classes	The number of classes.
   */
  public void setNClasses(int classes) {
    nClasses = classes;
  }

  /**
   *	Accessor that sets the max iterations.
   *	@param max The max iterations
   */
  public void setMaxIter(int max) {
    maxIter = max;
  }

  /**
   *	Accessor that sets the segmentation type (BOTH_FUZZY_HARD, FUZZY, or HARD)
   *	@param type	The segmentation type.
   */
  public void setSegmentationType(int type) {
    segmentation = type;
  }

  /**
   *	Accessor that sets the q variable.
   *	@param scale	Value to set q variable to.
   */
  public void setQ(float scale) {
    q = scale;
  }

  /**
   *	Accessor that sets the end tol.
   *	@param scale	Value to set end tol to.
   */
  public void setEndTol(float scale) {
    endTol = scale;
  }

  /**
   *	Accessor that sets the color flag.
   *	@param flag		<code>true</code> indicates ARG image, red.
   */
  public void setRed(boolean flag) {
    if (doColor)
      doRed = flag;
  }

  /**
   *	Accessor that sets the color flag.
   *	@param flag		<code>true</code> indicates ARG image, green.
   */
  public void setGreen(boolean flag) {
    if (doColor)
      doGreen = flag;
  }

  /**
   *	Accessor that sets the color flag.
   *	@param flag		<code>true</code> indicates ARG image, blue.
   */
  public void setBlue(boolean flag) {
    if (doColor)
      doBlue = flag;
  }

  /**
   *	Accessor that sets the centroids.
   *	@param scale	Value to set centroids to.
   */
  public void setCentroids(float[] centroids) {
    this.centroids = centroids;
  }

  /**
   *	Accessor that sets the threshold.
   *	@param scale	Value to set threshold to.
   */
  public void setThreshold(float[] threshold) {
    this.threshold = threshold;
  }

  /**
   *	Closes dialog box when the OK button is pressed and calls the algorithm.
   *  @param event       Event that triggers function.
   */
  public void actionPerformed(ActionEvent event) {
    String command = event.getActionCommand();

    int i, j;

    if (command.equals("Choose")) {
      ModelImage newImage = open();
      if (!checkImage(newImage))
        return;
      srcNumber++;
      tempImage = new ModelImage[srcNumber - 1];
      for (i = 0; i < (srcNumber - 1); i++) {
        tempImage[i] = srcImage[i];
      }
      srcImage = null;
      srcImage = new ModelImage[srcNumber];
      for (i = 0; i < (srcNumber - 1); i++) {
        srcImage[i] = tempImage[i];
      }
      tempImage = null;
      srcImage[srcNumber - 1] = newImage;
      model.addElement(srcImage[srcNumber - 1].getImageName());

      newImage = null;
      removeButton.setEnabled(true);
    } // if (command.equals("Choose"))
    else if ( (command.equals("Remove")) && (removeIndex == 0)) {
      // Cannot remove original image
      MipavUtil.displayError("Cannot remove original loaded image");
    }
    else if ( (command.equals("Remove")) && (removeIndex >= 1) &&
             (removeIndex <= (srcNumber - 1))) {
      // changeRemoveIndex = false is needed because the model.removeElement
      // line causes valueChanged to execute.  Without changeRemoveIndex an
      // unselected element causes removeIndex to be changed to -1.
      changeRemoveIndex = false;
      model.removeElement(srcImage[removeIndex].getImageName());
      tempImage = new ModelImage[srcNumber - 1];
      for (i = 0, j = 0; i < srcNumber; i++) {
        if (i != removeIndex) {
          tempImage[j++] = srcImage[i];
        }
      } // for ( i = 0, j=0; i < srcNumber; i++)
      srcImage[removeIndex].disposeLocal();
      srcImage[removeIndex] = null;
      changeRemoveIndex = true;
      srcImage = null;
      srcImage = new ModelImage[srcNumber - 1];
      for (i = 0; i < (srcNumber - 1); i++) {
        srcImage[i] = tempImage[i];
      }
      tempImage = null;
      srcNumber--;
      if (srcNumber == 1) {
        removeButton.setEnabled(false);
      }
    } // else if ((command.equals("Remove"))  && (removeIndex >= 1) && (removeIndex <= (srcNumber - 1)))
    else if (command.equals("OK")) {
      if (setVariables()) {
        callAlgorithm();
      }
    }
    else if (command.equals("Cancel")) {
      dispose();
    }
    else if (command.equals("Crop")) {
      if (cropCheckbox.isSelected()) {
        wholeImage.setEnabled(false);
        VOIRegions.setEnabled(false);
        wholeImage.setSelected(true);
        VOIRegions.setSelected(false);
      }
      else {
        wholeImage.setEnabled(true);
        VOIRegions.setEnabled(true);
      }
    }
    else if (command.equals("Help")) {
      MipavUtil.showHelp("fuzzyc-means_fuzzy_c-means_multispectral_and_single_channel_algorithms_htm_toc_applying_the_fuzzy_c");
    }
  }

  /**
   *	Checks the color and dimensionality of the new image vs. the original source image.
   *	All new images should have the same color modality as the source and be of the same dimensions.
   *	@return	Flag indicating if the image checks out.
   */
  private boolean checkImage(ModelImage testImage) {
    if (testImage == null)
      return false;
    if ( (srcImage[0].isColorImage() == true) && (testImage.isColorImage() == false)) {
      if (userInterface.isScriptRecording())
        userInterface.getScriptDialog().removeLine();
      MipavUtil.displayError("Cannot load a color (" + testImage.getImageName() +
                             ") unless the original file is color.");
      return false;
    }
    if (srcImage[0].getNDims() != testImage.getNDims()) {
      if (userInterface.isScriptRecording())
        userInterface.getScriptDialog().removeLine();
      MipavUtil.displayError("Error! " + srcImage[0].getImageName() + " is " +
                             srcImage[0].getNDims() +
                             "D, while " + testImage.getImageName() + " is " +
                             testImage.getNDims() + "D");
      return false;
    }
    for (int i = 0; i < srcImage[0].getNDims(); i++) {
      if ( (testImage != null) && (destExtents[i] != testImage.getExtents()[i])) {
        if (userInterface.isScriptRecording())
          userInterface.getScriptDialog().removeLine();
        MipavUtil.displayError("Error! For dimension = " + i + " " +
                               srcImage[0].getImageName() +
                               " has length = " + destExtents[i] + " while " +
                               testImage.getImageName() +
                               " has length = " + testImage.getExtents()[i]);
        return false;
      }
    }
    return true;

  }

  /**
   *	Use the GUI results to set up the variables needed to run the algorithm.
   *	@return		<code>true</code> if parameters set successfully, <code>false</code> otherwise.
   */
  private boolean setVariables() {
    String tmpStr;

    if (wholeImage.isSelected())
      regionFlag = true;
    else if (VOIRegions.isSelected())
      regionFlag = false;

    tmpStr = textNClasses.getText();
    if (testParameter(tmpStr, 1.0, 6.0)) {
      nClasses = Integer.valueOf(tmpStr).intValue();
    }
    else {
      textNClasses.requestFocus();
      textNClasses.selectAll();
      return false;
    }

    /* tmpStr = textNPyramid.getText();
        if ( testParameter(tmpStr, 1.0, 6.0) ){
            nPyramid = Integer.valueOf(tmpStr).intValue();
        }
        else{
            textNPyramid.requestFocus();
            textNPyramid.selectAll();
            return false;
        } */

    tmpStr = textExpo.getText();
    if (testParameter(tmpStr, 1.1, 5.0)) {
      q = Float.valueOf(tmpStr).floatValue();
    }
    else {
      textExpo.requestFocus();
      textExpo.selectAll();
      return false;
    }

    if (cropCheckbox.isSelected()) {
      cropBackground = true;
    }
    else {
      cropBackground = false;
    }

    tmpStr = textEndTol.getText();
    if (testParameter(tmpStr, Float.MIN_VALUE, 1.0)) {
      endTol = Float.valueOf(tmpStr).floatValue();
    }
    else {
      textEndTol.requestFocus();
      textEndTol.selectAll();
      return false;
    }

    tmpStr = textMaxIter.getText();
    if (testParameter(tmpStr, 1.0, 10000.0)) {
      maxIter = Integer.valueOf(tmpStr).intValue();
    }
    else {
      textMaxIter.requestFocus();
      textMaxIter.selectAll();
      return false;
    }

    /*   tmpStr = textOneSmooth.getText();
        if (testParameter(tmpStr,2.0e2,2.0e6) ) {
            oneSmooth = Float.valueOf(tmpStr).floatValue();
        }
        else {
            textOneSmooth.requestFocus();
            textOneSmooth.selectAll();
            return false;
        }
        tmpStr = textTwoSmooth.getText();
        if (testParameter(tmpStr,2.0e3,2.0e7) ) {
            twoSmooth = Float.valueOf(tmpStr).floatValue();
        }
        else {
            textTwoSmooth.requestFocus();
            textTwoSmooth.selectAll();
            return false;
        }
        tmpStr = textOneJacobiIter.getText();
        if (testParameter(tmpStr,1.0,10.0) ) {
            oneJacobiIter = Integer.valueOf(tmpStr).intValue();
        }
        else {
            textOneJacobiIter.requestFocus();
            textOneJacobiIter.selectAll();
            return false;
        }
        tmpStr = textTwoJacobiIter.getText();
        if (testParameter(tmpStr,1.0,10.0) ) {
            twoJacobiIter = Integer.valueOf(tmpStr).intValue();
        }
        else {
            textTwoJacobiIter.requestFocus();
            textTwoJacobiIter.selectAll();
            return false;
        } */

    if (hardOnly.isSelected()) {
      segmentation = AlgorithmMSpectralFuzzyCMeans.HARD_ONLY;
    }
    else if (fuzzyOnly.isSelected()) {
      segmentation = AlgorithmMSpectralFuzzyCMeans.FUZZY_ONLY;
    }
    else {
      segmentation = AlgorithmMSpectralFuzzyCMeans.BOTH_FUZZY_HARD;
    }

    /* if (calcGainFieldCheckbox.isSelected()) {
        outputGainField = true;
          }
          else {
        outputGainField = false;
          } */

    if (doColor) {
      doRed = redCheckbox.isSelected();
      doGreen = greenCheckbox.isSelected();
      doBlue = blueCheckbox.isSelected();
    } // if (doColor)
    return true;
  }

  /**
   *	Once all the necessary variables are set, call the Fuzzy C Means
   *	algorithm based on what type of image this is and whether or not there
   *	is a separate destination image.
   */
  private void callAlgorithm() {
    int i;
    System.gc();
    // Calculate the number of result images.
    resultNumber = 0;
    if ( (segmentation == AlgorithmMSpectralFuzzyCMeans.HARD_ONLY) ||
        (segmentation == AlgorithmMSpectralFuzzyCMeans.BOTH_FUZZY_HARD)) {
      resultNumber++; // segmented image
    } // if ((segmentation == HARD_ONLY) || (segmentation == BOTH_FUZZY_HARD))

    if ( (segmentation == AlgorithmMSpectralFuzzyCMeans.FUZZY_ONLY) ||
        (segmentation == AlgorithmMSpectralFuzzyCMeans.BOTH_FUZZY_HARD)) {
      //if (outputGainField) {
      //resultNumber++;
      //}
      resultNumber += nClasses;
    } // if ((segmentation == FUZZY_ONLY) || (segmentation == BOTH_FUZZY_HARD))

    try {
      resultImage = new ModelImage[resultNumber];
      presentNumber = 0;

      if ( (segmentation == AlgorithmMSpectralFuzzyCMeans.FUZZY_ONLY) ||
          (segmentation == AlgorithmMSpectralFuzzyCMeans.BOTH_FUZZY_HARD)) {
        for (i = 0; i < nClasses; i++) {
          String name = makeImageName(srcImage[0].getImageName(),
                                      "_class" + (i + 1));
          if (srcImage[0].getType() == ModelStorageBase.ARGB) {
            resultImage[presentNumber++] = new ModelImage(ModelStorageBase.
                UBYTE, destExtents,
                name, userInterface);
          }
          else if (srcImage[0].getType() == ModelStorageBase.ARGB_USHORT) {
            resultImage[presentNumber++] = new ModelImage(ModelStorageBase.
                USHORT, destExtents,
                name, userInterface);
          }
          else if (srcImage[0].getType() == ModelStorageBase.ARGB_FLOAT) {
            resultImage[presentNumber++] = new ModelImage(ModelStorageBase.
                FLOAT, destExtents,
                name, userInterface);
          }
          else {
            resultImage[presentNumber++] = new ModelImage(srcImage[0].getType(),
                destExtents,
                name, userInterface);
          }
        }
        /* if (outputGainField) {
            resultImage[presentNumber++] = new ModelImage(ModelStorageBase.FLOAT, destExtents,
             makeImageName(srcImage[0].getImageName(), "_mult"), userInterface);
                   } */
      }
      if ( (segmentation == AlgorithmMSpectralFuzzyCMeans.HARD_ONLY) ||
          (segmentation == AlgorithmMSpectralFuzzyCMeans.BOTH_FUZZY_HARD)) {
        resultImage[presentNumber++] = new ModelImage(ModelStorageBase.UBYTE,
            destExtents,
            makeImageName(srcImage[0].getImageName(), "_seg"), userInterface);
      }

      // Make algorithm
      afcmAlgo = new AlgorithmMSpectralFuzzyCMeans(resultImage, srcImage,
          nClasses, nPyramid,
          oneJacobiIter, twoJacobiIter, q, oneSmooth, twoSmooth,
          outputGainField, segmentation, cropBackground,
          maxIter, endTol, doRed, doGreen, doBlue, regionFlag);
      // This is very important. Adding this object as a listener allows the algorithm to
      // notify this object when it has completed of failed. See algorithm performed event.
      // This is made possible by implementing AlgorithmedPerformed interface
      afcmAlgo.addListener(this);

      if (regionFlag == false)
        afcmAlgo.setMask(srcImage[0].generateVOIMask());

        // if not previously set by script file, call the dialog to set now
      if (centroids == null && threshold == null) {
        // if returned false, dialog was cancelled
        if (!getCentroidsThreshold())
          return;
      }

      afcmAlgo.setCentroids(centroids);
      afcmAlgo.setThreshold(threshold);
      // Hide dialog
      setVisible(false);

      if (runInSeparateThread) {
        // Start the thread as a low priority because we wish to still have user interface work fast.
        if (afcmAlgo.startMethod(Thread.MIN_PRIORITY) == false) {
          MipavUtil.displayError("A thread is already running on this object");
        }
      }
      else {
        afcmAlgo.setActiveImage(isActiveImage);
        if (!userInterface.isAppFrameVisible()) {
          afcmAlgo.setProgressBarVisible(false);
        }
        afcmAlgo.run();
      }
    }
    catch (OutOfMemoryError x) {

      if (resultImage != null) {
        for (i = 0; i < resultNumber; i++) {
          if (resultImage[i] != null) {
            resultImage[i].disposeLocal(); // Clean up memory of result image
            resultImage[i] = null;
          }
        }
        resultImage = null;
      }
      System.gc();
      MipavUtil.displayError(
          "Dialog MS Fuzzy CMeans: unable to allocate enough memory");
      return;
    }

  }

  /**
   *	Gets the minimum and maximum of each image and initializes the
   *	centroids dialog appropriately.
   *	@return	Flag indicating a successful get.
   */
  private boolean getCentroidsThreshold() {
    int imageNumber = srcImage.length;
    int spectraNumber = 0;
    int i, j, k, m, x, y, z, kVol, mVol, newXDim, newYDim, newZDim, kClass,
        index, newSliceSize, newVolSize;

    float[] minimum;
    float[] maximum;
    float[] tBuffer;
    float[] buffer;
    float[] buffer2;

    int xDim = srcImage[0].getExtents()[0];
    int yDim = srcImage[0].getExtents()[1];
    int zDim;
    if (srcImage[0].getNDims() > 2)
      zDim = srcImage[0].getExtents()[2];
    else
      zDim = 1;
      //int orgXDim     = xDim;
      //int orgYDim     = yDim;
    int sliceSize = xDim * yDim;
    int volSize = xDim * yDim * zDim;
    //int orgSlice    = sliceSize;
    //int orgVol      = volSize;

    for (i = 0; i < srcImage.length; i++) {
      if (srcImage[i].isColorImage()) {
        if (doRed)
          spectraNumber++;
        if (doGreen)
          spectraNumber++;
        if (doBlue)
          spectraNumber++;
      }
      else
        spectraNumber++;
    }

    try {

      minimum = new float[spectraNumber];
      maximum = new float[spectraNumber];
      threshold = new float[spectraNumber];
      tBuffer = new float[volSize];
      buffer = new float[spectraNumber * volSize];

      for (i = 0, j = 0; i < imageNumber; i++) {
        srcImage[i].calcMinMax();
        if (srcImage[i].isColorImage()) {
          if (doRed) {
            minimum[j] = (float) srcImage[i].getMinR();
            maximum[j] = (float) srcImage[i].getMaxR();
            j++;
          }
          if (doGreen) {
            minimum[j] = (float) srcImage[i].getMinG();
            maximum[j] = (float) srcImage[i].getMaxG();
            j++;
          }
          if (doBlue) {
            minimum[j] = (float) srcImage[i].getMinB();
            maximum[j] = (float) srcImage[i].getMaxB();
            j++;
          }
        }
        else {
          minimum[j] = (float) srcImage[i].getMin();
          maximum[j] = (float) srcImage[i].getMax();
          j++;
        }
      } // for (i = 0,j = 0; i < imageNumber;i++)

      for (i = 0, k = 0; i < imageNumber; i++) {
        if (srcImage[i].isColorImage()) {
          if (doRed) {
            srcImage[i].exportRGBData(1, 0, sliceSize, tBuffer);

            kVol = k * volSize;
            for (j = 0; j < sliceSize; j++) {
              buffer[kVol + j] = tBuffer[j];
            }
            k++;
          } // if (doRed)
          if (doGreen) {
            srcImage[i].exportRGBData(2, 0, sliceSize, tBuffer);

            kVol = k * volSize;
            for (j = 0; j < sliceSize; j++) {
              buffer[kVol + j] = tBuffer[j];
            }
            k++;
          } // if (doGreen)
          if (doBlue) {
            srcImage[i].exportRGBData(3, 0, sliceSize, tBuffer);

            kVol = k * volSize;
            for (j = 0; j < sliceSize; j++) {
              buffer[kVol + j] = tBuffer[j];
            }
            k++;
          } // if (doBlue)
        }
        else { // not color
          srcImage[i].exportData(0, volSize, tBuffer);

          kVol = k * volSize;
          for (j = 0; j < volSize; j++) {
            buffer[kVol + j] = tBuffer[j];
          }
          k++;
        } // else not color
      } // for (i = 0, k = 0; i < imageNumber; i++)

      if (!regionFlag) {
        for (k = 0; k < spectraNumber; k++) {
          maximum[k] = -Float.MAX_VALUE;
          minimum[k] = Float.MAX_VALUE;
          kVol = k * volSize;
          for (i = 0; i < volSize; i++) {
            if (afcmAlgo.getMask().get(i)) {
              if (buffer[i + kVol] > maximum[k]) {
                maximum[k] = buffer[i + kVol];
              }
              if (buffer[i + kVol] < minimum[k]) {
                minimum[k] = buffer[i + kVol];
              }
            }
          }
        } // for (k = 0; k < spectraNumber; k++)
      } // if (!wholeImage)
      tBuffer = null;
      System.gc();

      float[] tCentroids = new float[nClasses];
      centroids = new float[spectraNumber * nClasses];
      for (i = 0, k = 0; i < imageNumber; i++) {
        if (srcImage[i].isColorImage()) {
          if (doRed) {
            JDialogCentroidThreshold dialogCentroidThreshold = new
                JDialogCentroidThreshold(parentFrame,
                                         "RED " + srcImage[i].getImageName(),
                                         nClasses, minimum[k], maximum[k]);
            if (dialogCentroidThreshold.isCancelled())
              return false;
            else {
              tCentroids = dialogCentroidThreshold.getCentroids();
              threshold[k] = dialogCentroidThreshold.getThreshold();
              kClass = k * nClasses;
              for (j = 0; j < nClasses; j++) {
                centroids[kClass + j] = tCentroids[j];
              }
              k++;
            }
          } // if (doRed)
          if (doGreen) {
            JDialogCentroidThreshold dialogCentroidThreshold = new
                JDialogCentroidThreshold(parentFrame,
                                         "GREEN " + srcImage[i].getImageName(),
                                         nClasses, minimum[k], maximum[k]);
            if (dialogCentroidThreshold.isCancelled())
              return false;
            else {
              tCentroids = dialogCentroidThreshold.getCentroids();
              threshold[k] = dialogCentroidThreshold.getThreshold();
              kClass = k * nClasses;
              for (j = 0; j < nClasses; j++) {
                centroids[kClass + j] = tCentroids[j];
              }
              k++;
            }
          } // if (doGreen)
          if (doBlue) {
            JDialogCentroidThreshold dialogCentroidThreshold = new
                JDialogCentroidThreshold(parentFrame,
                                         "BLUE " + srcImage[i].getImageName(),
                                         nClasses, minimum[k], maximum[k]);
            if (dialogCentroidThreshold.isCancelled())
              return false;
            else {
              tCentroids = dialogCentroidThreshold.getCentroids();
              threshold[k] = dialogCentroidThreshold.getThreshold();
              kClass = k * nClasses;
              for (j = 0; j < nClasses; j++) {
                centroids[kClass + j] = tCentroids[j];
              }
              k++;
            }
          } // if (doBlue)
        }
        else { // not color
          JDialogCentroidThreshold dialogCentroidThreshold = new
              JDialogCentroidThreshold(parentFrame, srcImage[i].getImageName(),
                                       nClasses, minimum[k], maximum[k]);
          if (dialogCentroidThreshold.isCancelled())
            return false;
          else {
            tCentroids = dialogCentroidThreshold.getCentroids();
            threshold[k] = dialogCentroidThreshold.getThreshold();
            kClass = k * nClasses;
            for (j = 0; j < nClasses; j++) {
              centroids[kClass + j] = tCentroids[j];
            }
            k++;
          }
        } // else not color
      } // for (i = 0, k = 0; i < imageNumber; i++)
      tCentroids = null;

      int xLow = 0;
      int yLow = 0;
      int zLow = 0;
      int xHigh = xDim - 1;
      int yHigh = yDim - 1;
      int zHigh = zDim - 1;
      int zStepIn, zStepOut, yStepIn, yStepOut, mStepIn, mStepOut;
      if (cropBackground) {
        // Find the smallest bounding box for the 1st data set
        // If cropBackground is true wholeImage is constrained to be true
        xLow = xDim - 1;
        yLow = yDim - 1;
        zLow = zDim - 1;
        xHigh = 0;
        yHigh = 0;
        zHigh = 0;
        for (z = 0; z < zDim; z++) {
          zStepIn = z * sliceSize;
          for (y = 0; y < yDim; y++) {
            yStepIn = y * xDim + zStepIn;
            for (x = 0; x < xDim; x++) {
              index = x + yStepIn;
              if (buffer[index] >= threshold[0]) {
                if (x < xLow)
                  xLow = x;
                if (x > xHigh)
                  xHigh = x;
                if (y < yLow)
                  yLow = y;
                if (y > yHigh)
                  yHigh = y;
                if (z < zLow)
                  zLow = z;
                if (z > zHigh)
                  zHigh = z;
              } // if (buffer[index] > threshold[0])
            } // for (x = 0; x < xDim; x++)
          } // for (y = 0; y < yDim; y++)
        } // for (z = 0; z < zDim; z++)
        if ( (xLow > 0) || (xHigh < (xDim - 1)) || (yLow > 0) ||
            (yHigh < (yDim - 1)) ||
            (zLow > 0) || (zHigh < (zDim - 1))) {
          // A smaller bounding box has been found for the data
          // Recopy area to smaller data array to save space
          newXDim = xHigh - xLow + 1;
          newYDim = yHigh - yLow + 1;
          newZDim = zHigh - zLow + 1;
          newSliceSize = newXDim * newYDim;
          newVolSize = newSliceSize * newZDim;
          buffer2 = new float[spectraNumber * newVolSize];
          for (m = 0; m < spectraNumber; m++) {
            mStepOut = m * volSize;
            mStepIn = m * newVolSize - xLow - yLow * newXDim -
                zLow * newSliceSize;
            for (z = zLow; z <= zHigh; z++) {
              zStepOut = z * sliceSize + mStepOut;
              zStepIn = z * newSliceSize + mStepIn;
              for (y = yLow; y <= yHigh; y++) {
                yStepOut = y * xDim + zStepOut;
                yStepIn = y * newXDim + zStepIn;
                for (x = xLow; x <= xHigh; x++) {
                  buffer2[x + yStepIn] =
                      buffer[x + yStepOut];
                } // for (x = xLow; x <= xHigh; x++)
              } // for (y = yLow; y <= yHigh; y++)
            } // for (z = zLow; z <= zHigh; z++)
          } // for (m = 0; m < spectraNumber; m++)
          xDim = newXDim;
          yDim = newYDim;
          zDim = newZDim;
          sliceSize = xDim * yDim;
          volSize = sliceSize * zDim;
          int totalSize = spectraNumber * volSize;
          buffer = null;
          buffer = new float[totalSize];
          for (i = 0; i < totalSize; i++) {
            buffer[i] = buffer2[i];
          }
          buffer2 = null;
          // Find the new minimum
          for (m = 0; m < spectraNumber; m++) {
            minimum[m] = Float.MAX_VALUE;
            mVol = m * volSize;
            for (i = 0; i < volSize; i++) {
              if (buffer[mVol + i] < minimum[m]) {
                minimum[m] = buffer[mVol + i];
              } // if (buffer[mSlice + i] < minimum[m])
            } // for (i = 0; i < sliceSize; i++)
          } // for (m = 0; m < spectralNumber; m++) {
        } // if ((xLow > 0) || (xHigh < (xDim-1)) || (yLow > 0) || (yHigh < (yDim - 1)))
      } // if (cropBackground)
    }
    catch (java.io.IOException ioe) {
      tBuffer = null;
      buffer = null;
      buffer2 = null;
      System.gc();
      MipavUtil.displayError(
          "Dialog MSFuzzyCMeans: Error in trying to find centroids.");
      return false;
    }
    catch (OutOfMemoryError error) {
      tBuffer = null;
      buffer = null;
      buffer2 = null;
      System.gc();
      MipavUtil.displayError("Algorithm FuzzyCMeans reports:\n" +
                             error.toString());
      return false;
    }

    tBuffer = null;
    buffer = null;
    buffer2 = null;
    System.gc();
    return true;
  }

  //************************************************************************
   //************************** Algorithm Events ****************************
    //************************************************************************

     /**
      *	This method is required if the AlgorithmPerformed interface is implemented. It is called by the
      *   algorithms when it has completed or failed to to complete, so that the dialog can be display
      *   the result image and/or clean up.
      *   @param algorithm   Algorithm that caused the event.
      */
     public void algorithmPerformed(AlgorithmBase algorithm) {

       int i;
       ViewJFrameImage imageFrame[] = new ViewJFrameImage[resultNumber];
       if (algorithm instanceof AlgorithmMSpectralFuzzyCMeans) {
         srcImage[0].clearMask();

         if (afcmAlgo.isCompleted() == true && resultImage != null) {
           // The algorithm has completed and produced a new image to be displayed.
           // Take resultImage out of array form or null pointer errors can
           // result in one of the resultImages after another of the resultImages
           // has been deleted.
           for (i = 0; i < resultNumber; i++) {
             updateFileInfo(srcImage[0], resultImage[i]);
             resultImage[i].clearMask();
                 try {
                   imageFrame[i] =
                       new ViewJFrameImage(resultImage[i], null, new Dimension(610, 200 + i * 20));
                 }
                 catch (OutOfMemoryError error) {
                   System.gc();
                   JOptionPane.showMessageDialog(null,
                       "Out of memory: unable to open new resultImage0 frame",
                       "Error", JOptionPane.ERROR_MESSAGE);
                 }

           }
         }
         else if (resultImage != null) {
           //algorithm failed but result image still has garbage
           for (i = 0; i < resultNumber; i++) {
             if (resultImage[i] != null) {
               resultImage[i].disposeLocal(); // Clean up memory of result image
               resultImage[i] = null;
             }
           }
           resultImage = null;
           System.gc();
         }
       }

       insertScriptLine(algorithm);

       dispose();
     }

  /**
   *	Sets the remove index based on the selected index in the list.
   *	@param evt	Event that caused this method to fire.
   */
  public void valueChanged(ListSelectionEvent evt) {
    if (changeRemoveIndex) {
        JList source = (JList) evt.getSource();
        removeIndex = source.getSelectedIndex();
    }
  }

  /**
   *	Open an image based on the suffix of the file.
   *	@return	The image.
   */
  private ModelImage open() {
    JFileChooser chooser = null;
    FileIO fileIO = null;
    boolean multiFile = false;
    String fileName;
    String directory;

    try {

      chooser = new JFileChooser();
      if (userInterface.getDefaultDirectory() != null) {
        File file = new File(userInterface.getDefaultDirectory());
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

      chooser.setDialogTitle("Open Image");
      int returnValue = chooser.showOpenDialog(userInterface.getMainFrame());

      if (returnValue == JFileChooser.APPROVE_OPTION) {
        fileName = chooser.getSelectedFile().getName();
        directory = String.valueOf(chooser.getCurrentDirectory()) +
            File.separatorChar;
        userInterface.setDefaultDirectory(directory);
      }
      else
        return null;
    }
    catch (OutOfMemoryError e) {
      MipavUtil.displayError("Out of memory!");
      return null;
    }

    try {
      fileIO = new FileIO();
      return fileIO.readImage(fileName, directory, multiFile, null);
    }
    catch (OutOfMemoryError e) {
      MipavUtil.displayError("Out of memory!");
      return null;
    }
  }

}
