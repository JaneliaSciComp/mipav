package gov.nih.mipav.view.dialogs;

import gov.nih.mipav.view.*;
import gov.nih.mipav.model.structures.*;
import gov.nih.mipav.model.file.*;
import gov.nih.mipav.model.algorithms.*;
import gov.nih.mipav.view.dialogs.*;

import java.awt.event.*;
import java.awt.*;
import java.io.File;

import javax.swing.*;
import javax.swing.event.*;

/**
 *   Dialog to get user input, then call the algorithm.
 *
 */
public class JDialogFRETBleedThrough
    extends JDialogBase
    implements AlgorithmInterface,
    ScriptableInterface,
    ListSelectionListener {

  private AlgorithmFRETBleedThrough fbtAlgo;
  // The source image must be either an image with donor dye only taken with
  // a donor fluorescent peak filter or an image with acceptor dye only taken
  // with a acceptor fluorescent peak filter.
  private ModelImage srcImage;
  private ModelImage FRETImage = null;
  private ModelImage FP2Image = null;
  private ViewJComponentEditImage componentImage;
  private ButtonGroup VOIGroup;
  private JRadioButton activeButton;
  private JRadioButton backgroundButton;
  private ButtonGroup dyeGroup;
  private JRadioButton acceptorButton;
  private JRadioButton donorButton;
  private boolean acceptorRun = true;

  private JPanel paramPanel;



  private ViewUserInterface userInterface;

  private JPanel imagePanel;
  private JButton chooserButton;
  private JList imageList;
  private DefaultListModel model;
  private JButton removeButton;
  private JPanel imagePanel2;
  private JButton chooserButton2;
  private JList imageList2;
  private DefaultListModel model2;
  private JButton removeButton2;

  private boolean doColor;
  private JPanel colorPanel;
  private ButtonGroup colorGroup;
  private JRadioButton redButton;
  private JRadioButton greenButton;
  private JRadioButton blueButton;
  private boolean useRed = false;
  private boolean useGreen = false;
  private boolean useBlue = false;


  private ViewVOIVector VOIs;
  private int nBoundingVOIs;
  private int activeIndex;
  private int backgroundIndex;



  /**
   *  @param parent          parent frame
   *  @param im              source image
   */
  public JDialogFRETBleedThrough(Frame theParentFrame, ModelImage im) {
    super(theParentFrame, false);
    srcImage = im;
    componentImage = ((ViewJFrameImage)theParentFrame).getComponentImage();
    userInterface = ( (ViewJFrameBase) (parentFrame)).getUserInterface();
    if (im.isColorImage()) {
      doColor = true;
      useRed = true;
      useBlue = false;
      useGreen = false;
    }
    else {
      doColor = false;
      useRed = false;
      useBlue = false;
      useGreen = false;
    }

    init();
  }

  /**
   *	Used primarily for the script to store variables and run the algorithm.  No
   *	actual dialog will appear but the set up info and result image will be stored here.
   *	@param UI   The user interface, needed to create the image frame.
   *	@param im	Source image.
   */
  public JDialogFRETBleedThrough(ViewUserInterface UI, ModelImage im) {
    super();
    userInterface = UI;
    srcImage = im;
    parentFrame = srcImage.getParentFrame();
    componentImage = ((ViewJFrameImage)parentFrame).getComponentImage();
    if (im.isColorImage()) {
      doColor = true;
      useRed = true;
      useBlue = false;
      useGreen = false;
    }
    else {
      doColor = false;
      useRed = false;
      useBlue = false;
      useGreen = false;
    }
  }

  /**
   * Empty constructor needed for dynamic instantiation (used during scripting).
   */
  public JDialogFRETBleedThrough() {}

  /**
   * Run this algorithm from a script.
   * @param parser the script parser we get the state from
   * @throws IllegalArgumentException if there is something wrong with the arguments in the script
   */
  public void scriptRun (AlgorithmScriptParser parser) throws IllegalArgumentException {
      String srcImageKey = null;
      String FRETImageKey = null;
      String FP2ImageKey = null;

      try {
          srcImageKey = parser.getNextString();
      } catch (Exception e) {
          throw new IllegalArgumentException();
      }
      srcImage = parser.getImage(srcImageKey);

      if (srcImage.isColorImage()) {
          doColor = true;
          useRed = true;
          useBlue = false;
          useGreen = false;
      }
      else {
          doColor = false;
          useRed = false;
          useBlue = false;
          useGreen = false;
      }

      userInterface = srcImage.getUserInterface();
      parentFrame = srcImage.getParentFrame();

      try {
          FRETImageKey = parser.getNextString();
      } catch (Exception e) {
          throw new IllegalArgumentException();
      }
      FRETImage = parser.getImage(FRETImageKey);

      try {
          FP2ImageKey = parser.getNextString();
      } catch (Exception e) {
          throw new IllegalArgumentException();
      }
      FP2Image = parser.getImage(FP2ImageKey);

      try {

          boolean red = parser.getNextBoolean();
          boolean green = parser.getNextBoolean();
          boolean blue = parser.getNextBoolean();
          boolean acceptorRun = parser.getNextBoolean();
          setRed(red);
          setBlue(blue);
          setGreen(green);
          setAcceptorRun(acceptorRun);

      } catch (Exception e) {
          throw new IllegalArgumentException();
      }

      setActiveImage(parser.isActiveImage());
      setSeparateThread(false);

      if (!checkImage(FRETImage)) {
        return;
      }
      if (!checkImage(FP2Image)) {
        return;
      }

      callAlgorithm();
  }

  /**
   * If a script is being recorded and the algorithm is done, add an entry for this algorithm.
   * @param algo the algorithm to make an entry for
   */
  public void insertScriptLine (AlgorithmBase algo) {
      if (algo.isCompleted()) {
          if (userInterface.isScriptRecording()) {
              //check to see if the  srcImage is already in the ImgTable
              if (userInterface.getScriptDialog().getImgTableVar(srcImage.getImageName()) == null) {
                  if (userInterface.getScriptDialog().getActiveImgTableVar(srcImage.getImageName()) == null) {
                      userInterface.getScriptDialog().putActiveVar(srcImage.getImageName());
                  }
              }

              userInterface.getScriptDialog().append("FRETBleedThrough " +
                                                     userInterface.getScriptDialog().
                                                     getVar(srcImage.
                                                            getImageName()) + " ");
                  //check to see if the  srcImage is already in the ImgTable
                  if (userInterface.getScriptDialog().getImgTableVar(srcImage.getImageName()) == null) {
                      if (userInterface.getScriptDialog().getActiveImgTableVar(srcImage.getImageName()) == null) {
                          userInterface.getScriptDialog().putActiveVar(srcImage.getImageName());
                      }
                  }

                  userInterface.getScriptDialog().append(userInterface.
                                                         getScriptDialog().getVar(srcImage.getImageName()) + " ");
                  userInterface.getScriptDialog().putVar(FRETImage.getImageName());
                  userInterface.getScriptDialog().append(userInterface.
                                                         getScriptDialog().getVar(FRETImage.getImageName()) +
                                                         " ");
                  userInterface.getScriptDialog().putVar(FP2Image.getImageName());
                  userInterface.getScriptDialog().append(userInterface.
                                                         getScriptDialog().getVar(FP2Image.getImageName()) +
                                                         " ");


              userInterface.getScriptDialog().append(useRed + " " + useGreen + " " +
                                                     useBlue + " " +
                                                     acceptorRun + "\n");
          }
      }
  }

  /**
   *	Sets up the GUI (panels, buttons, etc) and displays it on the screen.
   */
  private void init() {
    int yPos = 0;

    JPanel namePanel;

    JPanel VOIPanel;
    JPanel dyePanel;
    setForeground(Color.black);

    setTitle("FRET Bleed Through");

    GridBagConstraints gbc4 = new GridBagConstraints();
    gbc4.gridwidth = 1; gbc4.gridheight = 1; gbc4.anchor = gbc4.WEST; gbc4.weightx = 1;
    gbc4.insets = new Insets(3,3,3,3);
    gbc4.fill = GridBagConstraints.HORIZONTAL;
    gbc4.gridx = 0; gbc4.gridy = 0;


    namePanel = new JPanel(new GridBagLayout());
    JLabel nameLabel = new JLabel("1FP image with 1FP filter: " + srcImage.getImageName());
    nameLabel.setFont(serif12);
    nameLabel.setForeground(Color.black);
    namePanel.add(nameLabel, gbc4);

    dyePanel = new JPanel(new GridBagLayout());
    dyePanel.setBorder(buildTitledBorder("Select dye type used"));
    dyeGroup = new ButtonGroup();
    acceptorButton = new JRadioButton("Acceptor dye only",true);
    acceptorButton.setForeground(Color.black);
    acceptorButton.setFont(serif12);
    dyeGroup.add(acceptorButton);
    gbc4.gridy = 0;
    dyePanel.add(acceptorButton,gbc4);

    donorButton = new JRadioButton("Donor dye only",false);
    donorButton.setForeground(Color.black);
    donorButton.setFont(serif12);
    dyeGroup.add(donorButton);
    gbc4.gridy = 1;
    dyePanel.add(donorButton, gbc4);

    VOIPanel = new JPanel(new GridBagLayout());
    VOIPanel.setBorder(buildTitledBorder("Select VOIs"));


    VOIGroup = new ButtonGroup();

    backgroundButton = new JRadioButton("Add background VOI",true);
    backgroundButton.setForeground(Color.blue);
    backgroundButton.setFont(serif12);
    backgroundButton.addActionListener(this);
    VOIGroup.add(backgroundButton);
    gbc4.gridy = 0;
    VOIPanel.add(backgroundButton, gbc4);
    componentImage.setMode(ViewJComponentEditImage.NEW_VOI);
    componentImage.setPresetHue(2.0f/3.0f); // blue

    activeButton = new JRadioButton("Add active VOI", false);
    activeButton.setForeground(Color.red);
    activeButton.setFont(serif12);
    activeButton.addActionListener(this);
    VOIGroup.add(activeButton);
    gbc4.gridy = 1;
    VOIPanel.add(activeButton, gbc4);


    paramPanel = new JPanel(new GridBagLayout());
    paramPanel.setForeground(Color.black);

    GridBagConstraints gbc = new GridBagConstraints();
    gbc.gridwidth = 1; gbc.gridheight = 1; gbc.anchor = gbc.WEST; gbc.weightx = 1;
    gbc.insets = new Insets(3,3,3,3);
    gbc.fill = GridBagConstraints.HORIZONTAL;
    gbc.gridx = 0; gbc.gridy = 0;


    gbc.gridx = 0;
    gbc.gridy = yPos++;
    gbc.fill = gbc.BOTH;
    gbc.weightx = 1;
    gbc.gridwidth = 1;
    paramPanel.add(namePanel, gbc);
    gbc.gridy = yPos++;
    paramPanel.add(dyePanel, gbc);
    gbc.gridy = yPos++;
    paramPanel.add(VOIPanel, gbc);

    if (doColor) {
      colorPanel = new JPanel(new GridBagLayout());
      colorPanel.setBorder(buildTitledBorder("Channels"));
      colorGroup = new ButtonGroup();
      redButton = new JRadioButton("Use red color",true);
      redButton.setForeground(Color.red);
      redButton.setFont(serif12);
      colorGroup.add(redButton);


      greenButton = new JRadioButton("Use green color",false);
      greenButton.setForeground(Color.green.darker());
      greenButton.setFont(serif12);
      colorGroup.add(greenButton);

      blueButton = new JRadioButton("Use blue color", false);
      blueButton.setForeground(Color.blue);
      blueButton.setFont(serif12);
      colorGroup.add(blueButton);

      gbc.gridx = 0;
      gbc.gridy = 0;
      colorPanel.add(redButton, gbc);
      gbc.gridy = 1;
      colorPanel.add(greenButton, gbc);
      gbc.gridy = 2;
      colorPanel.add(blueButton, gbc);
      gbc.gridx = 1;
      colorPanel.add(Box.createHorizontalStrut(5), gbc);

      gbc.gridx = 0;
      gbc.gridy = yPos++;
      gbc.gridwidth = 1;
      paramPanel.add(colorPanel, gbc);

    } // if (doColor)

    imagePanel = new JPanel(new BorderLayout());
    imagePanel.setBorder(buildTitledBorder("Load 1FP image taken with FRET filter"));

    model = new DefaultListModel();
    imageList = new JList(model);
    imageList.setVisibleRowCount(2);
    imageList.setPreferredSize(new Dimension(300, 60));
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

    imagePanel2 = new JPanel(new BorderLayout());
    imagePanel2.setBorder(buildTitledBorder("Load 1FP image taken with 2FP filter"));

    model2 = new DefaultListModel();
    imageList2 = new JList(model2);
    imageList2.setVisibleRowCount(2);
    imageList2.setPreferredSize(new Dimension(300, 60));
    imageList2.setSelectionMode(ListSelectionModel.SINGLE_SELECTION);
    imageList2.addListSelectionListener(this);
    imagePanel2.add(imageList2);

    JPanel chooserPanel2 = new JPanel();
    chooserButton2 = new JButton("Load");
    chooserButton2.setPreferredSize(MipavUtil.defaultButtonSize);
    chooserButton2.setFont(serif12B);
    chooserPanel2.add(chooserButton2);
    chooserButton2.addActionListener(this);
    chooserButton2.setActionCommand("Choose2");

    removeButton2 = new JButton("Remove");
    removeButton2.setPreferredSize(MipavUtil.defaultButtonSize);
    removeButton2.setFont(serif12B);
    removeButton2.setEnabled(false);
    chooserPanel2.add(removeButton2);
    removeButton2.addActionListener(this);
    removeButton2.setActionCommand("Remove2");

    imagePanel2.add(chooserPanel2, BorderLayout.SOUTH);


    gbc.gridx = 0;
    gbc.gridwidth = gbc.REMAINDER;
    gbc.gridy = yPos++;
    gbc.fill = gbc.HORIZONTAL;
    gbc.weightx = 1;
    paramPanel.add(imagePanel, gbc);
    gbc.gridy = yPos++;
    paramPanel.add(imagePanel2, gbc);

    getContentPane().add(paramPanel);
    getContentPane().add(buildButtons(), BorderLayout.SOUTH);

    pack();
    setVisible(true);
  }


  /**
   *  Accessor that sets the source image.
   *  @param	The new source image.
   */
  public void setSourceImage(ModelImage image) {
    srcImage = image;
  }

  public void setRed(boolean useRed) {
      this.useRed = useRed;
  }

  public void setGreen(boolean useGreen) {
      this.useGreen = useGreen;
  }

  public void setBlue(boolean useBlue) {
      this.useBlue = useBlue;
  }

  public void setAcceptorRun(boolean acceptorRun) {
    this.acceptorRun = acceptorRun;
  }



  /**
   *	Closes dialog box when the OK button is pressed and calls the algorithm.
   *  @param event       Event that triggers function.
   */
  public void actionPerformed(ActionEvent event) {
    String command = event.getActionCommand();
    Object source = event.getSource();

    int i, j;

    if (command.equals("Choose")) {
      FRETImage = open();
      if (FRETImage == null) {
        return;
      }
      if (!checkImage(FRETImage)) {
        FRETImage.disposeLocal();
        FRETImage = null;
        return;
      }
      model.addElement(FRETImage.getImageName());
      removeButton.setEnabled(true);
      chooserButton.setEnabled(false);
    } // if (command.equals("Choose"))
    else if (command.equals("Remove")) {
      model.removeElement(FRETImage.getImageName());
      FRETImage.disposeLocal();
      FRETImage = null;
      chooserButton.setEnabled(true);
      removeButton.setEnabled(false);
    } // else if (command.equals("Remove"))
    else if (command.equals("Choose2")) {
      FP2Image = open();
      if (FP2Image == null) {
        return;
      }
      if (!checkImage(FP2Image)) {
        FP2Image.disposeLocal();
        FP2Image = null;
        return;
      }
      model2.addElement(FP2Image.getImageName());
      removeButton2.setEnabled(true);
      chooserButton2.setEnabled(false);
    } // if (command.equals("Choose2"))
    else if (command.equals("Remove2")) {
      model2.removeElement(FP2Image.getImageName());
      FP2Image.disposeLocal();
      FP2Image = null;
      chooserButton2.setEnabled(true);
      removeButton2.setEnabled(false);
    } // else if (command.equals("Remove2"))
    else if (command.equals("OK")) {
      if (setVariables()) {
        callAlgorithm();
      }
    }
    else if (command.equals("Cancel")) {
      dispose();
    }
    else if ((source == activeButton) || (source == backgroundButton)) {
        if (activeButton.isSelected()) {
            componentImage.setMode(ViewJComponentEditImage.NEW_VOI);
            componentImage.setPresetHue(-1.0f); // no preset color
        }
        else if (backgroundButton.isSelected()) {
            componentImage.setMode(ViewJComponentEditImage.NEW_VOI);
            componentImage.setPresetHue(2.0f/3.0f); // blue
        }
    }

    else if (command.equals("Help")) {
      MipavUtil.showHelp(" ");
    }
  }

  /**
   *	Checks the color and dimensionality of the new image vs. the original source image.
   *    The new image cannot be color unless the source image is color.  However,
   *    then new image may be black and white when the source image is color.
   *	All new images should have the same dimensions as the source.
   *	@return	Flag indicating if the image checks out.
   */
  private boolean checkImage(ModelImage testImage) {
    if (testImage == null)
      return false;
    if (testImage.getImageName() == srcImage.getImageName()) {
      MipavUtil.displayError("Cannot add original FP1 filter image");
      return false;
    }
    if ((FP2Image != null) && (FRETImage != null) &&
        (FRETImage.getImageName() == FP2Image.getImageName())) {
     MipavUtil.displayError(
      "Cannot use same image for FRET and FP2 bleed throughs");
     return false;
   }
    if ( (srcImage.isColorImage() == false) && (testImage.isColorImage() == true)) {
      if (userInterface.isScriptRecording())
        userInterface.getScriptDialog().removeLine();
      MipavUtil.displayError("Cannot load a color (" + testImage.getImageName() +
                             ") unless the original file is color.");
      return false;
    }
    if (srcImage.getNDims() != testImage.getNDims()) {
      if (userInterface.isScriptRecording())
        userInterface.getScriptDialog().removeLine();
      MipavUtil.displayError("Error! " + srcImage.getImageName() + " is " +
                             srcImage.getNDims() +
                             "D, while " + testImage.getImageName() + " is " +
                             testImage.getNDims() + "D");
      return false;
    }
    for (int i = 0; i < srcImage.getNDims(); i++) {
      if ( (testImage != null) && (srcImage.getExtents()[i] != testImage.getExtents()[i])) {
        if (userInterface.isScriptRecording())
          userInterface.getScriptDialog().removeLine();
        MipavUtil.displayError("Error! For dimension = " + i + " " +
                               srcImage.getImageName() +
                               " has length = " + srcImage.getExtents()[i] + " while " +
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

    int i;
    int nVOIs;
    float [] hsb;
    float hue;
    VOIs = srcImage.getVOIs();
    nVOIs = VOIs.size();
    nBoundingVOIs = 0;
    activeIndex = -1;
    backgroundIndex = -1;


    for (i = 0; i < nVOIs; i++) {
        if ((VOIs.VOIAt(i).getCurveType() == VOI.CONTOUR) ||
            (VOIs.VOIAt(i).getCurveType() == VOI.POLYLINE)) {
            nBoundingVOIs++;
            hsb = Color.RGBtoHSB(VOIs.VOIAt(i).getColor().getRed(),
                           VOIs.VOIAt(i).getColor().getGreen(),
                           VOIs.VOIAt(i).getColor().getBlue(),null);
            hue = hsb[0];
            if ((Math.abs(hue - (2.0f/3.0f))) < 0.0001f) {
                if (backgroundIndex == -1) {
                    backgroundIndex = i;
                    VOIs.VOIAt(i).setName("Background");
                }
                else {
                    MipavUtil.displayError("Cannot have more than 1 background VOI");
                    return false;
                }
            }
            else  {
                  activeIndex = i;
                  VOIs.VOIAt(i).setName("Active" + i);
           }
        }
    } // for (i = 0; i < nVOIs; i++)

    if (activeIndex == -1) {
        MipavUtil.displayError("Must specify an active VOI");
        return false;
    }

    if (backgroundIndex == -1) {
        MipavUtil.displayError("Must specify a background VOI");
        return false;
    }




    if (doColor) {
      useRed = redButton.isSelected();
      useGreen = greenButton.isSelected();
      useBlue = blueButton.isSelected();
    } // if (doColor)

    acceptorRun  = acceptorButton.isSelected();

    return true;
  }

  /**
   *	Once all the necessary variables are set, call AlgorithmFRETBleedThrough
   */
  private void callAlgorithm() {
    int i;
    System.gc();
    try {

      // Make algorithm
      fbtAlgo = new AlgorithmFRETBleedThrough(srcImage,
          FRETImage, FP2Image, useRed, useGreen, useBlue, acceptorRun);
      // This is very important. Adding this object as a listener allows the algorithm to
      // notify this object when it has completed of failed. See algorithm performed event.
      // This is made possible by implementing AlgorithmedPerformed interface
      fbtAlgo.addListener(this);


      // Hide dialog
      setVisible(false);

      if (runInSeparateThread) {
        // Start the thread as a low priority because we wish to still have user interface work fast.
        if (fbtAlgo.startMethod(Thread.MIN_PRIORITY) == false) {
          MipavUtil.displayError("A thread is already running on this object");
        }
      }
      else {
        fbtAlgo.setActiveImage(isActiveImage);
        if (!userInterface.isAppFrameVisible()) {
          fbtAlgo.setProgressBarVisible(false);
        }
        fbtAlgo.run();
      }
    }
    catch (OutOfMemoryError x) {


      System.gc();
      MipavUtil.displayError(
          "Dialog FRET Bleed Through: unable to allocate enough memory");
      return;
    }

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

       if (algorithm instanceof AlgorithmFRETBleedThrough) {


         insertScriptLine(algorithm);

         dispose();
       }
     }

  /**
   *	Sets the remove index based on the selected index in the list.
   *	@param evt	Event that caused this method to fire.
   */
  public void valueChanged(ListSelectionEvent evt) {
    JList source = (JList) evt.getSource();
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
          ViewImageFileFilter.MISC));
      chooser.addChoosableFileFilter(new ViewImageFileFilter(
          ViewImageFileFilter.MICROSCOPY));

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
