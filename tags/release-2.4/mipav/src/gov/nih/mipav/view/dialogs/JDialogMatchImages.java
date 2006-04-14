package gov.nih.mipav.view.dialogs;

import gov.nih.mipav.view.*;
import gov.nih.mipav.model.structures.*;
import gov.nih.mipav.model.algorithms.*;
import gov.nih.mipav.model.algorithms.utilities.*;

import java.awt.event.*;
import java.awt.*;
import java.util.*;

import javax.swing.*;

/**
 *   Dialog to get user input, then call the algorithm...
 *
 *		@version    0.1 July 14, 2003
 *		@author     Zohara A Cohen, Ph.D.
 *
 */
public class JDialogMatchImages extends JDialogBase implements AlgorithmInterface, ScriptableInterface {

  private AlgorithmMatchImages matchAlgo;
  private ModelImage imageA; // first source image
  private ModelImage imageB; // second source image
  private ModelImage resultImageA = null; // result image
  private ModelImage resultImageB = null; // result image
  private ViewUserInterface userInterface;
  private JComboBox comboBoxImageA, comboBoxImageB;
  private JCheckBox checkOrigins, checkDimensions;
  private String selectedNameA, selectedNameB;
  private boolean doOrigins, doDimensions;
  private JTextField padValTxt;
  private int padValue=0;

  /**
   *  Creates new match image dialog and displays.
   *  @param parent          Parent frame.
   *  @param im              Source image.
   */
  public JDialogMatchImages(Frame theParentFrame, ModelImage im) {
    super(theParentFrame, false);
    imageA = im;
    userInterface = ( (ViewJFrameBase) (parentFrame)).getUserInterface();
    init();
  }

  /**
   *	Used primarily for the script to store variables and run the algorithm.  No
   *	actual dialog will appear but the set up info and result image will be stored here.
   *	@param UI   The user interface, needed to create the image frame.
   *	@param imA  Source image.
   *    @param imB
   *    @param doOr
   *    @param doRe
   *    @param doOrig
   *    @param doDim
   */
  public JDialogMatchImages(ViewUserInterface UI, ModelImage imA, ModelImage imB,
                        boolean doOr, boolean doRe, boolean doOrig, boolean doDim) {
    super(false);
    imageA = imA;
    imageB = imB;
    userInterface = UI;
    doOrigins = doOrig;
    doDimensions = doDim;
    parentFrame = imA.getParentFrame();
  }

  /**
   * Empty constructor needed for dynamic instantiation (used during scripting).
   */
  public JDialogMatchImages() {}

  /**
   * Run this algorithm from a script.
   * @param parser the script parser we get the state from
   * @throws IllegalArgumentException if there is something wrong with the arguments in the script
   */
  public void scriptRun (AlgorithmScriptParser parser) throws IllegalArgumentException {
      String imageAKey = null;
      String imageBKey = null;
      String destImageAKey = null;
      String destImageBKey = null;

      try {
          imageAKey = parser.getNextString();
          imageBKey = parser.getNextString();
      } catch (Exception e) {
          throw new IllegalArgumentException();
      }

      imageA = parser.getImage(imageAKey);
      imageB = parser.getImage(imageBKey);
      userInterface = imageA.getUserInterface();
      parentFrame = imageA.getParentFrame();

      // the result image
      try {
          destImageAKey = parser.getNextString();
          destImageBKey = parser.getNextString();
      } catch (Exception e) {
          throw new IllegalArgumentException();
      }

      try {
          doOrigins = parser.getNextBoolean();
          doDimensions = parser.getNextBoolean();
      } catch (Exception e) {
          throw new IllegalArgumentException();
      }

      setActiveImage(parser.isActiveImage());
      setSeparateThread(false);
      callAlgorithm();
      parser.putVariable(destImageAKey, getResultImageA().getImageName());
      parser.putVariable(destImageBKey, getResultImageB().getImageName());
  }

  /**
   * If a script is being recorded and the algorithm is done, add an entry for this algorithm.
   * @param algo the algorithm to make an entry for
   */
  public void insertScriptLine (AlgorithmBase algo) {
      if (algo.isCompleted()) {
          if (userInterface.isScriptRecording()) {
              //check to see if the  imageA is already in the ImgTable
              if (userInterface.getScriptDialog().getImgTableVar(imageA.getImageName()) == null) {
                  if (userInterface.getScriptDialog().getActiveImgTableVar(imageA.getImageName()) == null) {
                      userInterface.getScriptDialog().putActiveVar(imageA.getImageName());
                  }
              }
              //check to see if the  imageB is already in the ImgTable
              if (userInterface.getScriptDialog().getImgTableVar(imageB.getImageName()) == null) {
                  if (userInterface.getScriptDialog().getActiveImgTableVar(imageB.getImageName()) == null) {
                      userInterface.getScriptDialog().putActiveVar(imageB.getImageName());
                  }
              }

              userInterface.getScriptDialog().append("MatchImages " +
                                                     userInterface.getScriptDialog().getVar(imageA.getImageName()) +
                                                     " " +
                                                     userInterface.getScriptDialog().getVar(imageB.getImageName()) +
                                                     " ");
              userInterface.getScriptDialog().putVar(resultImageA.getImageName());
              userInterface.getScriptDialog().putVar(resultImageB.getImageName());
              userInterface.getScriptDialog().append(userInterface.getScriptDialog().getVar(resultImageA.getImageName()) +
                                                     " " +
                                                     userInterface.getScriptDialog().getVar(resultImageB.getImageName()) +
                                                     " ");
              userInterface.getScriptDialog().append(doOrigins + " " + doDimensions + "\n");
          }
      }
  }

  /**
   *	Sets up the GUI (panels, buttons, etc) and displays it on the screen.
   */
  private void init() {
    setTitle("Match Images");

    JPanel inputPanel = new JPanel();
    inputPanel.setBorder(buildTitledBorder("Match image features:"));

    inputPanel.setLayout(new GridBagLayout());
    GridBagConstraints gbc = new GridBagConstraints();
    gbc.anchor = gbc.WEST;
    gbc.gridy=1;
    gbc.gridwidth = 2;
    JLabel descript = new JLabel("This alorithm matches the orientations and "
                                    +"resolutions of two images.");
    inputPanel.add(descript,gbc);

    gbc.gridy = 2;
    gbc.gridwidth = 1;
    JLabel labelImageA = new JLabel("Image A:");
    inputPanel.add(labelImageA, gbc);

    gbc.gridwidth = GridBagConstraints.REMAINDER;
    comboBoxImageA = buildComboBoxImage();
    inputPanel.add(comboBoxImageA,gbc);
    inputPanel.add(Box.createHorizontalStrut(10),gbc);
    String name = (String)userInterface.getActiveImageFrame().getComponentImage().getActiveImage().getImageName();
    comboBoxImageA.setSelectedItem(name);

    gbc.gridy = 3;
    gbc.gridwidth = 1;
    JLabel labelImageB = new JLabel("Image B: ");
    inputPanel.add(labelImageB, gbc);

    gbc.gridwidth = GridBagConstraints.REMAINDER;
    comboBoxImageB = buildComboBoxImage();
    inputPanel.add(comboBoxImageB,gbc);
    inputPanel.add(Box.createHorizontalStrut(10),gbc);

    checkOrigins = new JCheckBox("Match image origins too.");
    checkDimensions = new JCheckBox("Match image dimensions too.");
    checkOrigins.setSelected(false);
    checkDimensions.setSelected(false);
    checkOrigins.setEnabled(true);
    checkDimensions.setEnabled(true);
    checkOrigins.setAlignmentX(Component.LEFT_ALIGNMENT);
    checkDimensions.setAlignmentX(Component.LEFT_ALIGNMENT);
    gbc.gridy=4; inputPanel.add(checkOrigins, gbc);
    gbc.gridy=5; inputPanel.add(checkDimensions, gbc);

    JLabel padLabel = new JLabel("Intensity value for padding ");
    padValTxt = new JTextField(String.valueOf(padValue),4);
    gbc.gridy=6;
    gbc.gridwidth=1;
    inputPanel.add(padLabel, gbc);
    gbc.gridwidth = GridBagConstraints.REMAINDER;
    inputPanel.add(padValTxt, gbc);

    JPanel buttonPanel = new JPanel();
    buttonPanel.add(buildButtons());

    JPanel mainPanel = new JPanel(new BorderLayout());
    mainPanel.add(inputPanel, BorderLayout.NORTH);
    mainPanel.add(buttonPanel, BorderLayout.SOUTH);

    mainPanel.setBorder(BorderFactory.createEmptyBorder(5, 5, 5, 5));
    getContentPane().add(mainPanel);
    pack();
    setVisible(true);
  }

  /**
   *   Builds a list of images to operate on from the template image.
   */
  private JComboBox buildComboBoxImage() {
    ViewUserInterface UI;

    JComboBox comboBoxImage = new JComboBox();
    comboBoxImage.setFont(serif12);
    comboBoxImage.setBackground(Color.white);

    UI = imageA.getUserInterface();
    Enumeration names = UI.getRegisteredImageNames();

    // Add images from user interface
    // Guaranteed to have at least one unique potential image B, because it's
    // tested for in ViewJFrameImage before this dialog is created.
    while (names.hasMoreElements()) {
      String name = (String) names.nextElement();
      ModelImage img = UI.getRegisteredImageByName(name);
      if (UI.getFrameContainingImage(img) != null) {
          comboBoxImage.addItem(name);
      }
    }
    return comboBoxImage;
  }

  /**
   *	Use the GUI results to set up the variables needed to run the algorithm.
   *	@return		<code>true</code> if parameters set successfully, <code>false</code> otherwise.
   */
  private boolean setVariables() {
    ViewUserInterface UI = imageA.getUserInterface();
    selectedNameA = (String) comboBoxImageA.getSelectedItem();
    selectedNameB = (String) comboBoxImageB.getSelectedItem();
    imageA = UI.getRegisteredImageByName(selectedNameA);
    imageB = UI.getRegisteredImageByName(selectedNameB);
    doOrigins = checkOrigins.isSelected();
    doDimensions = checkDimensions.isSelected();
    String tmpStr;
    tmpStr = padValTxt.getText();
    if (testParameter(tmpStr, 0, 255)) {
      padValue = Integer.valueOf(tmpStr).intValue();
    }
    else {
      MipavUtil.displayError("Padding intensity must be between 0 and 255.");
      padValue = 0;
    }
    return true;
  }

  /**
   *	Once all the necessary variables are set, call the Gaussian Blur
   *	algorithm based on what type of image this is and whether or not there
   *	is a separate destination image.
   */
  private void callAlgorithm() {
    try {
        // Make algorithm
        boolean resByRef=false;
        matchAlgo = new AlgorithmMatchImages(imageA, imageB, doOrigins, doDimensions,
                                             resByRef);
        matchAlgo.setPadValue(padValue);
        // This is very important. Adding this object as a listener allows the algorithm to
        // notify this object when it has completed or failed. See algorithm performed event.
        // This is made possible by implementing AlgorithmedPerformed interface
        matchAlgo.addListener(this);
        // Hide dialog
        setVisible(false);

        if (runInSeparateThread) {
        // Start the thread as a low priority because we wish to still have user interface work fast.
            if (matchAlgo.startMethod(Thread.MIN_PRIORITY) == false) {
                MipavUtil.displayError("A thread is already running on this object");
            }
        }
        else {
            matchAlgo.setActiveImage(isActiveImage);
                if (!userInterface.isAppFrameVisible()) {
                  matchAlgo.setProgressBarVisible(false);
                }
            matchAlgo.run();
        }
    }
    catch (OutOfMemoryError x) {
        System.gc();
        MipavUtil.displayError("Unable to allocate enough memory to run algorithms.");
        return;
    }
  }

  /**
   *  Closes dialog box when the OK button is pressed and
   *  calls the algorithm.
   *  @param event       event that triggers function
   */
  public void actionPerformed(ActionEvent event) {
    String command = event.getActionCommand();
    Object source = event.getSource();

    if (command.equals("OK")) {
      if (setVariables()) {
        callAlgorithm();
      }
    }
    else if (command.equals("Cancel")) {
      dispose();
    }
    else if ( command.equals("Help")) {
       MipavUtil.showHelp("10064");
    }
  }

  //************************************************************************
   //************************** Algorithm Events ****************************
    //************************************************************************

     /**
      *	This method is required if the AlgorithmPerformed interface is implemented. It is called by the
      *   algorithms when it has completed or failed to complete, so that the dialog can display
      *   the result image and/or clean up.
      *   @param algorithm   Algorithm that caused the event.
      */
     public void algorithmPerformed(AlgorithmBase algorithm) {
       ViewJFrameImage imageFrameA, imageFrameB;
       imageFrameA = null;
       imageFrameB = null;

       if (algorithm instanceof AlgorithmMatchImages) {
          boolean isNewA = matchAlgo.isNewA();
          boolean isNewB = matchAlgo.isNewB();

          if (isNewA) resultImageA = matchAlgo.getResultA();
          if (isNewB) resultImageB = matchAlgo.getResultB();

          if (matchAlgo.isCompleted() == true) {
             System.out.println("AlgorithmMatchImages completed.");

            // Display new images
            try {
              if (isNewA) {
                resultImageA.calcMinMax();
                imageFrameA = new ViewJFrameImage(resultImageA, null, new Dimension(25,55));
              }
              if (isNewB) {
                resultImageB.calcMinMax();
                imageFrameB = new ViewJFrameImage(resultImageB, null, new Dimension(35, 65));
              }
            }
            catch (OutOfMemoryError error) {
              System.gc();
              MipavUtil.displayError("Out of memory: unable to open new frames");
            }
          }
          else {
             if (resultImageA != null) resultImageA.disposeLocal(); // Clean up memory of result images
             if (resultImageB != null) resultImageB.disposeLocal(); // Clean up memory of result image
          }
       }

       // Update frames
       imageA.notifyImageDisplayListeners(null,true);
       imageB.notifyImageDisplayListeners(null,true);

       // Write to script.
       insertScriptLine(algorithm);

       matchAlgo.finalize();
       matchAlgo = null;

       dispose();
       System.gc();
     }

  /**
   *  Accessor that returns the image.
   *  @return          The result image.
   */
  public ModelImage getResultImageA() {
    return resultImageA;
  }

  /**
   *  Accessor that returns the image.
   *  @return          The result image.
   */
  public ModelImage getResultImageB() {
    return resultImageB;
  }

  /**
   *   Accessor that sets image A.
   *   @param im   Image A.
   */
  public void setImageA(ModelImage im) {
    imageA = im;
  }

  /**
   *   Accessor that sets image B.
   *   @param im   Image B.
   */
  public void setImageB(ModelImage im) {
    imageB = im;
  }

}
