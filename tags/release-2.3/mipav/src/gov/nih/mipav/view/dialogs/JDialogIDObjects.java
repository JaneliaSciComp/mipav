package gov.nih.mipav.view.dialogs;

import gov.nih.mipav.view.*;
import gov.nih.mipav.model.structures.*;
import gov.nih.mipav.model.algorithms.*;

import java.awt.event.*;
import java.awt.*;
import java.util.*;

import javax.swing.*;

/**
 *   Dialog to get user input, then call the algorithm. The user is able to control
 *   the degree of blurring in all dimensions and indicate if a correction factor be
 *   applied to the z-dimension to account for differing resolutions between the
 *   xy resolutions (intra-plane) and the z resolution (inter-plane). The user has the
 *   option to generate a new image or replace the source image. In addition the user
 *   can indicate if you wishes to have the algorithm applied to whole image or to the
 *   VOI regions. In should be noted, that the algorithms are executed in their own
 *   thread.
 *
 *		@version    0.1 Nov 17, 1998
 *		@author     Matthew J. McAuliffe, Ph.D.
 *       @see        AlgorithmMorphology2D
 *
 */
public class JDialogIDObjects
    extends JDialogBase
    implements AlgorithmInterface, ScriptableInterface {

  private AlgorithmMorphology2D idObjectsAlgo2D = null;
  private AlgorithmMorphology3D idObjectsAlgo3D = null;
  private ModelImage image; // source image
  private ModelImage resultImage = null; // result image
  private ViewUserInterface userInterface;

  private String titles[];

  private JPanel destinationPanel;
  private ButtonGroup destinationGroup;
  private JRadioButton replaceImage;
  private JRadioButton newImage;

  private JPanel imageVOIPanel;
  private ButtonGroup imageVOIGroup;
  private JRadioButton wholeImage;
  private JRadioButton VOIRegions;

  private int displayLoc; // Flag indicating if a new image is to be generated

  // or if the source image is to be replaced
  private boolean regionFlag = false;

  private JTextField textMax;
  private JLabel labelMax;
  private JTextField textMin;
  private JLabel labelMin;
  private int max, min;

  /**
   *  Creates new dialog.
   *  @param theParentFrame    Parent frame
   *  @param im                Source image
   */
  public JDialogIDObjects(Frame theParentFrame, ModelImage im) {
    super(theParentFrame, true);

    if (im.getType() != ModelImage.BOOLEAN &&
        im.getType() != ModelImage.UBYTE &&
        im.getType() != ModelImage.USHORT) {
      MipavUtil.displayError(
          "Source Image must be BOOLEAN, UNSIGNED BYTE or UNSIGNED SHORT");
      dispose();
      return;
    }
    image = im;
    userInterface = ( (ViewJFrameBase) (parentFrame)).getUserInterface();
    init();
  }

  /**
   *	Used primarily for the script to store variables and run the algorithm.  No
   *	actual dialog will appear but the set up info and result image will be stored here.
   *	@param UI   The user interface, needed to create the image frame.
   *	@param im	Source image.
   */
  public JDialogIDObjects(ViewUserInterface UI, ModelImage im) {
    super();
    if (im.getType() != ModelImage.BOOLEAN &&
        im.getType() != ModelImage.UBYTE &&
        im.getType() != ModelImage.USHORT) {
      MipavUtil.displayError("Source Image must be Boolean or UByte or UShort");
      dispose();
      return;
    }
    userInterface = UI;
    image = im;
    parentFrame = image.getParentFrame();
  }

  /**
   * Empty constructor needed for dynamic instantiation (used during scripting).
   */
  public JDialogIDObjects() {}

  /**
   * Run this algorithm from a script.
   * @param parser the script parser we get the state from
   * @throws IllegalArgumentException if there is something wrong with the arguments in the script
   */
  public void scriptRun(AlgorithmScriptParser parser) throws
      IllegalArgumentException {
    String srcImageKey = null;
    String destImageKey = null;

    try {
      srcImageKey = parser.getNextString();
    }
    catch (Exception e) {
      throw new IllegalArgumentException();
    }
    ModelImage im = parser.getImage(srcImageKey);

    if (im.getType() != ModelImage.BOOLEAN &&
        im.getType() != ModelImage.UBYTE &&
        im.getType() != ModelImage.USHORT) {
      MipavUtil.displayError("Source Image must be Boolean or UByte or UShort");
      dispose();
      return;
    }
    image = im;
    userInterface = image.getUserInterface();
    parentFrame = image.getParentFrame();

    // the result image
    try {
      destImageKey = parser.getNextString();
    }
    catch (Exception e) {
      throw new IllegalArgumentException();
    }

    if (srcImageKey.equals(destImageKey)) {
      this.setDisplayLocReplace();
    }
    else {
      this.setDisplayLocNew();
    }

    try {
      setRegionFlag(parser.getNextBoolean());
    }
    catch (Exception e) {
      throw new IllegalArgumentException();
    }

    setActiveImage(parser.isActiveImage());
    setSeparateThread(false);
    callAlgorithm();
    if (!srcImageKey.equals(destImageKey)) {
      parser.putVariable(destImageKey, getResultImage().getImageName());
    }
  }

  /**
   * If a script is being recorded and the algorithm is done, add an entry for this algorithm.
   * @param algo the algorithm to make an entry for
   */
  public void insertScriptLine(AlgorithmBase algo) {
    if (algo.isCompleted()) {
      if (userInterface.isScriptRecording()) {
        //check to see if the match image is already in the ImgTable
        if (userInterface.getScriptDialog().getImgTableVar(image.getImageName()) == null) {
          if (userInterface.getScriptDialog().getActiveImgTableVar(image.
              getImageName()) == null) {
            userInterface.getScriptDialog().putActiveVar(image.getImageName());
          }
        }

        userInterface.getScriptDialog().append("IDObjects " +
                                               userInterface.getScriptDialog().
                                               getVar(image.getImageName()) +
                                               " ");
        if (displayLoc == NEW) {
          userInterface.getScriptDialog().putVar(resultImage.getImageName());
          userInterface.getScriptDialog().append(userInterface.
                                                 getScriptDialog().getVar(
              resultImage.getImageName()) + " " +
                                                 regionFlag + "\n");
        }
        else {
          userInterface.getScriptDialog().append(userInterface.
                                                 getScriptDialog().getVar(image.
              getImageName()) + " " +
                                                 regionFlag + "\n");
        }
      }
    }
  }

  /**
   *	Sets up the GUI (panels, buttons, etc) and displays it on the screen.
   */
  private void init() {
    setForeground(Color.black);

    setTitle("Identify objects");

    destinationPanel = new JPanel(new GridBagLayout());
    destinationPanel.setForeground(Color.black);
    destinationPanel.setBorder(buildTitledBorder("Destination"));

    destinationGroup = new ButtonGroup();
    newImage = new JRadioButton("New image", true);
    newImage.setFont(serif12);
    destinationGroup.add(newImage);

    replaceImage = new JRadioButton("Replace image", false);
    replaceImage.setFont(serif12);
    destinationGroup.add(replaceImage);

    GridBagConstraints gbc = new GridBagConstraints();
    gbc.gridx = 0;
    gbc.gridy = 0;
    gbc.weightx = 1;
    gbc.anchor = gbc.WEST;
    destinationPanel.add(newImage, gbc);
    gbc.gridy = 1;
    destinationPanel.add(replaceImage, gbc);

    imageVOIPanel = new JPanel(new GridBagLayout());
    imageVOIPanel.setForeground(Color.black);
    imageVOIPanel.setBorder(buildTitledBorder("ID Objects"));

    imageVOIGroup = new ButtonGroup();
    wholeImage = new JRadioButton("Whole image", true);
    wholeImage.setFont(serif12);
    imageVOIGroup.add(wholeImage);

    VOIRegions = new JRadioButton("VOI region(s)", false);
    VOIRegions.setFont(serif12);
    imageVOIGroup.add(VOIRegions);

    gbc.gridx = 0;
    gbc.gridy = 0;
    imageVOIPanel.add(wholeImage, gbc);
    gbc.gridy = 1;
    imageVOIPanel.add(VOIRegions, gbc);
    // Only if the image is unlocked can it be replaced.
    if (image.getLockStatus() == ModelStorageBase.UNLOCKED) {
      replaceImage.setEnabled(true);
    }
    else {
      replaceImage.setEnabled(false);
    }

    JPanel mainPanel = new JPanel(new GridBagLayout());

    gbc.gridx = 0;
    gbc.gridy = 0;
    gbc.gridwidth = 1;
    gbc.fill = gbc.BOTH;
    gbc.insets = new Insets(5, 5, 5, 5);
    mainPanel.add(destinationPanel, gbc);
    gbc.gridx = 1;
    mainPanel.add(imageVOIPanel, gbc);

    JPanel buttonPanel = new JPanel();
    buildOKButton();
    buttonPanel.add(OKButton);
    buildCancelButton();
    buttonPanel.add(cancelButton);
    JPanel maskPanel = new JPanel(new GridBagLayout());
    maskPanel.setForeground(Color.black);
    maskPanel.setBorder(buildTitledBorder("Delete particles outside range"));

    labelMax = new JLabel("Maximum size");
    labelMax.setForeground(Color.black);
    labelMax.setFont(serif12);

    textMax = new JTextField(5);
    textMax.setText("5000");
    textMax.setFont(serif12);

    labelMin = new JLabel("Minimum size");
    labelMin.setForeground(Color.black);
    labelMin.setFont(serif12);

    textMin = new JTextField(5);
    textMin.setText("1");
    textMin.setFont(serif12);

    gbc = new GridBagConstraints();
    gbc.gridwidth = 1;
    gbc.gridheight = 1;
    gbc.anchor = gbc.WEST;
    gbc.insets = new Insets(5, 5, 5, 5);

    gbc.gridx = 0;
    gbc.gridy = 0;
    gbc.weightx = 0;
    gbc.fill = gbc.NONE;
    maskPanel.add(labelMax, gbc);
    gbc.gridx = 1;
    gbc.gridy = 0;
    gbc.weightx = 1;
    gbc.fill = gbc.HORIZONTAL;
    maskPanel.add(textMax, gbc);
    gbc.gridx = 0;
    gbc.gridy = 1;
    gbc.weightx = 0;
    gbc.fill = gbc.NONE;
    maskPanel.add(labelMin, gbc);
    gbc.gridx = 1;
    gbc.gridy = 1;
    gbc.weightx = 1;
    gbc.fill = gbc.HORIZONTAL;
    maskPanel.add(textMin, gbc);

    JPanel controlPanel = new JPanel();
    controlPanel.setLayout(new BoxLayout(controlPanel, BoxLayout.Y_AXIS));
    controlPanel.add(mainPanel);
    controlPanel.add(maskPanel);
    controlPanel.add(buttonPanel);
    getContentPane().add(controlPanel);
    pack();
    setVisible(true);
  }

  /**
   *   Accessor that returns the image
   *   @return          the result image
   */
  public ModelImage getResultImage() {
    return resultImage;
  }

  /**
   *    Closes dialog box when the OK button is pressed and
   *    calls the algorithm
   *    @param event       event that triggers function
   */
  public void actionPerformed(ActionEvent event) {
    String command = event.getActionCommand();

    if (command.equals("OK")) {
      if (setVariables()) {
        callAlgorithm();
      }
    }
    else if (command.equals("Cancel")) {
      dispose();
    }
  }

  /**
   *	Accessor that sets the display loc variable to replace, so the current image
   *	is replaced once the algorithm completes.
   */
  public void setDisplayLocReplace() {
    displayLoc = REPLACE;
  }

  /**
   *	Accessor that sets the display loc variable to new, so that a new image
   *	is created once the algorithm completes.
   */
  public void setDisplayLocNew() {
    displayLoc = NEW;
  }

  /**
   *	Accessor that sets the region flag.
   *	@param flag		<code>true</code> indicates the whole image is blurred, <code>false</code> indicates a region.
   */
  public void setRegionFlag(boolean flag) {
    regionFlag = flag;
  }

  /**
   *	Use the GUI results to set up the variables needed to run the algorithm.
   *	@return		<code>true</code> if parameters set successfully, <code>false</code> otherwise.
   */
  private boolean setVariables() {

    System.gc();
    String tmpStr;
    if (replaceImage.isSelected()) {
      displayLoc = REPLACE;
    }
    else if (newImage.isSelected()) {
      displayLoc = NEW;

    }
    if (wholeImage.isSelected()) {
      regionFlag = true;
    }
    else if (VOIRegions.isSelected()) {
      regionFlag = false;
    }
    tmpStr = textMax.getText();
    if (testParameter(tmpStr, 1, 1000000000)) {
      max = Integer.valueOf(tmpStr).intValue();
    }
    else {
      textMax.requestFocus();
      textMax.selectAll();
      return false;
    }

    tmpStr = textMin.getText();
    if (testParameter(tmpStr, 1, 1000000000)) {
      min = Integer.valueOf(tmpStr).intValue();
    }
    else {
      textMin.requestFocus();
      textMin.selectAll();
      return false;
    }
    return true;
  }

  /**
   *	Once all the necessary variables are set, call the Gaussian Blur
   *	algorithm based on what type of image this is and whether or not there
   *	is a separate destination image.
   */
  private void callAlgorithm() {
    int kernel = 0;
    String name = makeImageName(image.getImageName(), "_IDObjects");
    // identify objects
    if (image.getNDims() == 2) { // source image is 2D

      int destExtents[] = new int[2];
      destExtents[0] = image.getExtents()[0]; // X dim
      destExtents[1] = image.getExtents()[1]; // Y dim

      if (displayLoc == NEW) {
        try {
          // Make result image of float type
          resultImage = (ModelImage) image.clone();
          resultImage.setImageName(name);
          // Make algorithm
          idObjectsAlgo2D = new AlgorithmMorphology2D(resultImage, kernel, 0,
              AlgorithmMorphology2D.ID_OBJECTS, 0, 0, 0, 0, regionFlag);
          idObjectsAlgo2D.setMinMax(min, max);
          if (regionFlag == false) {
            idObjectsAlgo2D.setMask(image.generateVOIMask());
            // This is very important. Adding this object as a listener allows the algorithm to
            // notify this object when it has completed of failed. See algorithm performed event.
            // This is made possible by implementing AlgorithmedPerformed interface
          }
          idObjectsAlgo2D.addListener(this);
          // Hide dialog
          setVisible(false);

          if (runInSeparateThread) {
            // Start the thread as a low priority because we wish to still have user interface work fast.
            if (idObjectsAlgo2D.startMethod(Thread.MIN_PRIORITY) == false) {
              MipavUtil.displayError(
                  "A thread is already running on this object");
            }
          }
          else {
            idObjectsAlgo2D.setActiveImage(isActiveImage);
            if (!userInterface.isAppFrameVisible()) {
              idObjectsAlgo2D.setProgressBarVisible(false);
            }
            idObjectsAlgo2D.run();
          }
        }
        catch (OutOfMemoryError x) {
          MipavUtil.displayError(
              "Dialog ID objects: unable to allocate enough memory");
          if (resultImage != null) {
            resultImage.disposeLocal(); // Clean up memory of result image
            resultImage = null;
          }
          return;
        }
      }
      else {
        try {
          // No need to make new image space because the user has choosen to replace the source image
          // Make the algorithm class
          idObjectsAlgo2D = new AlgorithmMorphology2D(image, kernel, 0,
              AlgorithmMorphology2D.ID_OBJECTS, 0, 0, 0, 0, regionFlag);
          idObjectsAlgo2D.setMinMax(min, max);

          if (regionFlag == false) {
            idObjectsAlgo2D.setMask(image.generateVOIMask());
            // This is very important. Adding this object as a listener allows the algorithm to
            // notify this object when it has completed of failed. See algorithm performed event.
            // This is made possible by implementing AlgorithmedPerformed interface
          }
          idObjectsAlgo2D.addListener(this);
          // Hide the dialog since the algorithm is about to run.
          setVisible(false);

          // These next lines set the titles in all frames where the source image is displayed to
          // "locked - " image name so as to indicate that the image is now read/write locked!
          // The image frames are disabled and then unregisted from the userinterface until the
          // algorithm has completed.
          Vector imageFrames = image.getImageFrameVector();
          titles = new String[imageFrames.size()];
          for (int i = 0; i < imageFrames.size(); i++) {
            titles[i] = ( (ViewJFrameBase) (imageFrames.elementAt(i))).getTitle();
            ( (ViewJFrameBase) (imageFrames.elementAt(i))).setTitle("Locked: " +
                titles[i]);
            ( (ViewJFrameBase) (imageFrames.elementAt(i))).setEnabled(false);
            userInterface.unregisterFrame( (Frame) (imageFrames.elementAt(i)));
          }

          if (runInSeparateThread) {
            // Start the thread as a low priority because we wish to still have user interface.
            if (idObjectsAlgo2D.startMethod(Thread.MIN_PRIORITY) == false) {
              MipavUtil.displayError(
                  "A thread is already running on this object");
            }
          }
          else {
            idObjectsAlgo2D.setActiveImage(isActiveImage);
            if (!userInterface.isAppFrameVisible()) {
              idObjectsAlgo2D.setProgressBarVisible(false);
            }
            idObjectsAlgo2D.run();
          }
        }
        catch (OutOfMemoryError x) {
          MipavUtil.displayError(
              "Dialog ID objects: unable to allocate enough memory");
          return;
        }
      }
    }
    else if (image.getNDims() == 3) {
      int destExtents[] = new int[3];
      destExtents[0] = image.getExtents()[0];
      destExtents[1] = image.getExtents()[1];
      destExtents[2] = image.getExtents()[2];

      if (displayLoc == NEW) {
        try {
          // Make result image of float type
          resultImage = (ModelImage) image.clone();
          resultImage.setImageName(name);
          // Make algorithm
          idObjectsAlgo3D = new AlgorithmMorphology3D(resultImage, kernel, 0,
              AlgorithmMorphology3D.ID_OBJECTS, 0, 0, 0, 0, regionFlag);
          idObjectsAlgo3D.setMinMax(min, max);
          if (regionFlag == false) {
            idObjectsAlgo3D.setMask(image.generateVOIMask());
            // This is very important. Adding this object as a listener allows the algorithm to
            // notify this object when it has completed of failed. See algorithm performed event.
            // This is made possible by implementing AlgorithmedPerformed interface
          }
          idObjectsAlgo3D.addListener(this);
          // Hide dialog
          setVisible(false);

          if (runInSeparateThread) {
            // Start the thread as a low priority because we wish to still have user interface work fast
            if (idObjectsAlgo3D.startMethod(Thread.MIN_PRIORITY) == false) {
              MipavUtil.displayError(
                  "A thread is already running on this object");
            }
          }
          else {
            idObjectsAlgo3D.setActiveImage(isActiveImage);
            if (!userInterface.isAppFrameVisible()) {
              idObjectsAlgo3D.setProgressBarVisible(false);
            }
            idObjectsAlgo3D.run();
          }
        }
        catch (OutOfMemoryError x) {
          MipavUtil.displayError(
              "Dialog ID objects: unable to allocate enough memory");
          if (resultImage != null) {
            resultImage.disposeLocal(); // Clean up image memory
            resultImage = null;
          }
          return;
        }
      }
      else {
        try {
          // Make algorithm
          idObjectsAlgo3D = new AlgorithmMorphology3D(image, kernel, 0,
              AlgorithmMorphology3D.ID_OBJECTS, 0, 0, 0, 0, regionFlag);
          idObjectsAlgo3D.setMinMax(min, max);
          if (regionFlag == false) {
            idObjectsAlgo3D.setMask(image.generateVOIMask());
            // This is very important. Adding this object as a listener allows the algorithm to
            // notify this object when it has completed of failed. See algorithm performed event.
            // This is made possible by implementing AlgorithmedPerformed interface
          }
          idObjectsAlgo3D.addListener(this);
          // Hide dialog
          setVisible(false);

          // These next lines set the titles in all frames where the source image is displayed to
          // "locked - " image name so as to indicate that the image is now read/write locked!
          // The image frames are disabled and then unregisted from the userinterface until the
          // algorithm has completed.
          Vector imageFrames = image.getImageFrameVector();
          titles = new String[imageFrames.size()];
          for (int i = 0; i < imageFrames.size(); i++) {
            titles[i] = ( (ViewJFrameBase) (imageFrames.elementAt(i))).getTitle();
            ( (ViewJFrameBase) (imageFrames.elementAt(i))).setTitle("Locked: " +
                titles[i]);
            ( (ViewJFrameBase) (imageFrames.elementAt(i))).setEnabled(false);
            userInterface.unregisterFrame( (Frame) (imageFrames.elementAt(i)));
          }

          if (runInSeparateThread) {
            // Start the thread as a low priority because we wish to still have user interface work fast
            if (idObjectsAlgo3D.startMethod(Thread.MIN_PRIORITY) == false) {
              MipavUtil.displayError(
                  "A thread is already running on this object");
            }
          }
          else {
            idObjectsAlgo3D.setActiveImage(isActiveImage);
            if (!userInterface.isAppFrameVisible()) {
              idObjectsAlgo3D.setProgressBarVisible(false);
            }
            idObjectsAlgo3D.run();
          }
        }
        catch (OutOfMemoryError x) {
          MipavUtil.displayError(
              "Dialog ID objects: unable to allocate enough memory");
          return;
        }
      }
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

       ViewJFrameImage imageFrame = null;
       if (algorithm instanceof AlgorithmMorphology2D) {
         image.clearMask();
         if (idObjectsAlgo2D.isCompleted() == true && resultImage != null) {
           updateFileInfo(image, resultImage);
           resultImage.clearMask();
           //The algorithm has completed and produced a new image to be displayed.
           try {
             //resultImage.setImageName("ID image");
             imageFrame = new ViewJFrameImage(resultImage, null, new Dimension(610, 200));
           }
           catch (OutOfMemoryError error) {
             MipavUtil.displayError("Out of memory: unable to open new frame");
           }
         }
         else if (resultImage == null) {
           // These next lines set the titles in all frames where the source image is displayed to
           // image name so as to indicate that the image is now unlocked!
           // The image frames are enabled and then registed to the userinterface.
           Vector imageFrames = image.getImageFrameVector();
           for (int i = 0; i < imageFrames.size(); i++) {
             ( (ViewJFrameBase) (imageFrames.elementAt(i))).setTitle(titles[i]);
             ( (ViewJFrameBase) (imageFrames.elementAt(i))).setEnabled(true);
             if ( ( (Frame) (imageFrames.elementAt(i))) != parentFrame) {
               userInterface.registerFrame
                   ( (Frame) (imageFrames.elementAt(i)));
             }
           }
           if (userInterface != null) {
             userInterface.registerFrame(parentFrame);
           }
           image.notifyImageDisplayListeners(null, true);
         }
         else if (resultImage != null) {
           //algorithm failed but result image still has garbage
           resultImage.disposeLocal(); // clean up memory
           resultImage = null;
         }
       }
       else if (algorithm instanceof AlgorithmMorphology3D) {
         image.clearMask();
         if (idObjectsAlgo3D.isCompleted() == true && resultImage != null) {
           updateFileInfo(image, resultImage);
           resultImage.clearMask();
           //The algorithm has completed and produced a new image to be displayed.
           try {
             //resultImage.setImageName("ID image");
             imageFrame = new ViewJFrameImage(resultImage, null, new Dimension(610, 200));
           }
           catch (OutOfMemoryError error) {
             MipavUtil.displayError("Out of memory: unable to open new frame");
           }
         }
         else if (resultImage == null) {
           // These next lines set the titles in all frames where the source image is displayed to
           // image name so as to indicate that the image is now unlocked!
           // The image frames are enabled and then registed to the userinterface.
           Vector imageFrames = image.getImageFrameVector();
           for (int i = 0; i < imageFrames.size(); i++) {
             ( (ViewJFrameBase) (imageFrames.elementAt(i))).setTitle(titles[i]);
             ( (ViewJFrameBase) (imageFrames.elementAt(i))).setEnabled(true);
             if ( ( (Frame) (imageFrames.elementAt(i))) != parentFrame) {
               userInterface.registerFrame( (Frame) (imageFrames.elementAt(i)));
             }
           }
           if (parentFrame != null) {
             userInterface.registerFrame(parentFrame);
           }
           image.notifyImageDisplayListeners(null, true);
         }
         else if (resultImage != null) {
           //algorithm failed but result image still has garbage
           resultImage.disposeLocal(); // clean up memory
           resultImage = null;
         }
       }
       insertScriptLine(algorithm);
       if (idObjectsAlgo2D != null) {
         idObjectsAlgo2D.finalize();
         idObjectsAlgo2D = null;
       }
       if (idObjectsAlgo3D != null) {
         idObjectsAlgo3D.finalize();
         idObjectsAlgo3D = null;
       }
       dispose();
     }

}
