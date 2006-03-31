package gov.nih.mipav.view.dialogs;

import gov.nih.mipav.view.*;
import gov.nih.mipav.model.structures.*;
import gov.nih.mipav.model.algorithms.*;

import java.awt.event.*;
import java.awt.*;
import java.util.*;

import javax.swing.*;

/**
 *   Dialog to get user input, then call the algorithm.  The user has the
 *   option to generate a new image or replace the source image. It should be noted,
 *   that the algorithms are executed in their own thread. The mean or median
 *   value of a VOI is subtracted from the image.  For black and white either clipping
     *   to maintain the same image type or promotion to a new image type if the new
 *   minimum or maximum exceed the available range may be selected.  For color
 *   only clipping is currently available because MIPAV cannot handle negative
 *   color values.
 *
 */
public class JDialogSubtractVOI
    extends JDialogBase
    implements AlgorithmInterface {

  private AlgorithmSubtractVOI subVOIAlgo;
  private ModelImage image; // source image
  private ModelImage resultImage = null; // result image
  private ViewUserInterface userInterface;

  private String titles[];

  private JPanel inputPanel;
  private ButtonGroup group;
  private JRadioButton radioMean;
  private JRadioButton radioMedian;

  private JPanel clipPanel;
  private ButtonGroup clipGroup;
  private JRadioButton radioClip;
  private JRadioButton radioPromote;

  private JPanel destinationPanel;
  private ButtonGroup destinationGroup;
  private JRadioButton replaceImage;
  private JRadioButton newImage;

  private int averageMode = AlgorithmSubtractVOI.MEDIAN;
  private int clipMode = AlgorithmSubtractVOI.CLIP;
  private int displayLoc; // Flag indicating if a new image is to be generated
  // or if the source image is to be replaced

  /**
   *  Creates new dialog.
   *  @param theParentFrame    Parent frame
   *  @param im                Source image
   */
  public JDialogSubtractVOI(Frame theParentFrame, ModelImage im) {
    super(theParentFrame, true);
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
  public JDialogSubtractVOI(ViewUserInterface UI, ModelImage im) {
    super();
    userInterface = UI;
    image = im;
    parentFrame = image.getParentFrame();
  }

  /**
   *	Sets up the GUI (panels, buttons, etc) and displays it on the screen.
   */
  private void init() {
    setForeground(Color.black);
    setTitle("Subtract VOI Background");

    inputPanel = new JPanel(new GridBagLayout());
    inputPanel.setForeground(Color.black);
    inputPanel.setBorder(buildTitledBorder("Average Type"));

    group = new ButtonGroup();
    radioMean = new JRadioButton("Mean", false);
    radioMean.setFont(serif12);
    group.add(radioMean);

    radioMedian = new JRadioButton("Median", true);
    radioMedian.setFont(serif12);
    group.add(radioMedian);

    GridBagConstraints gbc = new GridBagConstraints();
    gbc.gridheight = 1;
    gbc.anchor = gbc.WEST;

    gbc.gridx = 0;
    gbc.gridy = 0;
    gbc.weightx = 100;
    gbc.fill = gbc.HORIZONTAL;
    gbc.gridwidth = 1;
    gbc.insets = new Insets(0, 0, 0, 0);
    inputPanel.add(radioMean, gbc);
    gbc.gridy = 1;
    inputPanel.add(radioMedian, gbc);

    clipPanel = new JPanel(new GridBagLayout());
    clipPanel.setForeground(Color.black);
    clipPanel.setBorder(buildTitledBorder("Output clipping"));

    clipGroup = new ButtonGroup();
    radioClip = new JRadioButton("Clip", true);
    radioClip.setFont(serif12);
    clipGroup.add(radioClip);

    radioPromote = new JRadioButton("Promote image type", false);
    radioPromote.setFont(serif12);
    // MIPAV cannot currently handle negative color values
    if (image.isColorImage()) {
      radioPromote.setEnabled(false);
    }
    clipGroup.add(radioPromote);

    gbc.gridheight = 1;
    gbc.anchor = gbc.WEST;

    gbc.gridx = 0;
    gbc.gridy = 0;
    gbc.weightx = 100;
    gbc.fill = gbc.HORIZONTAL;
    gbc.gridwidth = 1;
    gbc.insets = new Insets(0, 0, 0, 0);
    clipPanel.add(radioClip, gbc);
    gbc.gridy = 1;
    clipPanel.add(radioPromote, gbc);

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

    // Only if the image is unlocked can it be replaced.
    if (image.getLockStatus() == ModelStorageBase.UNLOCKED) {
      replaceImage.setEnabled(true);
    }
    else {
      replaceImage.setEnabled(false);
    }

    gbc.gridx = 0;
    gbc.gridy = 0;
    gbc.weightx = 100;
    gbc.fill = gbc.HORIZONTAL;
    destinationPanel.add(newImage, gbc);
    gbc.gridy = 1;
    destinationPanel.add(replaceImage, gbc);

    JPanel mainPanel = new JPanel(new GridBagLayout());
    gbc.gridx = 0;
    gbc.gridy = 0;
    gbc.gridwidth = 1;
    gbc.weightx = 1;
    gbc.fill = gbc.HORIZONTAL;
    mainPanel.add(inputPanel, gbc);
    gbc.gridy = 1;
    mainPanel.add(clipPanel, gbc);
    gbc.gridy = 2;
    gbc.fill = gbc.BOTH;
    mainPanel.add(destinationPanel, gbc);

    JPanel buttonPanel = new JPanel();
    buildOKButton();
    buttonPanel.add(OKButton);
    buildCancelButton();
    buttonPanel.add(cancelButton);

    getContentPane().add(mainPanel);
    getContentPane().add(buttonPanel, BorderLayout.SOUTH);
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

  /* Accessor that sets the average mode
   * @param n  the average mode to be used when performing the math algorithm
   */
  public void setAverageMode(int n) {
    averageMode = n;
  }

  /* Accessor that sets the clip mode
   * @param n  the clip mode to be used when performing the math algorithm
   */
  public void setClipMode(int n) {
    clipMode = n;
  }

  /**
   *	Use the GUI results to set up the variables needed to run the algorithm.
   *	@return		<code>true</code> if parameters set successfully, <code>false</code> otherwise.
   */
  private boolean setVariables() {

    String tmpStr;

    if (replaceImage.isSelected())
      displayLoc = REPLACE;
    else if (newImage.isSelected())
      displayLoc = NEW;

    if (radioMean.isSelected())
      averageMode = AlgorithmSubtractVOI.MEAN;
    else if (radioMedian.isSelected())
      averageMode = AlgorithmSubtractVOI.MEDIAN;

    if (radioClip.isSelected())
      clipMode = AlgorithmSubtractVOI.CLIP;
    else if (radioPromote.isSelected())
      clipMode = AlgorithmSubtractVOI.PROMOTE;

    return true;
  }

  /**
   *	Once all the necessary variables are set, call the Gaussian Blur
   *	algorithm based on what type of image this is and whether or not there
   *	is a separate destination image.
   */
  private void callAlgorithm() {

    if (displayLoc == NEW) {
      try {
        // make the new image name
        String name = makeImageName(image.getImageName(), "_subVOI");

        resultImage = new ModelImage(image.getType(), image.getExtents(), name,
                                     userInterface);

        // Make algorithm
        subVOIAlgo = new AlgorithmSubtractVOI(resultImage, image, averageMode,
                                              clipMode);
        // This is very important. Adding this object as a listener allows the algorithm to
        // notify this object when it has completed of failed. See algorithm performed event.
        // This is made possible by implementing AlgorithmedPerformed interface
        subVOIAlgo.addListener(this);
        // Hide dialog
        setVisible(false);

        if (runInSeparateThread) {
          // Start the thread as a low priority because we wish to still have user interface work fast.
          if (subVOIAlgo.startMethod(Thread.MIN_PRIORITY) == false) {
            MipavUtil.displayError("A thread is already running on this object");
          }
        }
        else {
          subVOIAlgo.setActiveImage(isActiveImage);
          if (!userInterface.isAppFrameVisible()) {
            subVOIAlgo.setProgressBarVisible(false);
          }
          subVOIAlgo.run();
        }
      }
      catch (OutOfMemoryError x) {
        if (resultImage != null) {
          resultImage.disposeLocal(); // Clean up memory of result image
          resultImage = null;
        }
        System.gc();
        MipavUtil.displayError(
            "Dialog Subtract VOI: unable to allocate enough memory");
        return;
      }
    }
    else {
      try {
        // No need to make new image space because the user has choosen to replace the source image
        // Make the algorithm class
        subVOIAlgo = new AlgorithmSubtractVOI(image, averageMode, clipMode);
        // This is very important. Adding this object as a listener allows the algorithm to
        // notify this object when it has completed of failed. See algorithm performed event.
        // This is made possible by implementing AlgorithmedPerformed interface
        subVOIAlgo.addListener(this);
        // Hide the dialog since the algorithm is about to run.
        setVisible(false);

        // These next lines set the titles in all frames where the source image is displayed to
        // "locked - " image name so as to indicate that the image is now read/write locked!
        // The image frames are disabled and then unregisted from the userinterface until the
        // algorithm has completed.
        Vector imageFrames = image.getImageFrameVector();
        titles = new String[imageFrames.size()];
        for (int i = 0; i < imageFrames.size(); i++) {
          titles[i] = ( (Frame) (imageFrames.elementAt(i))).getTitle();
          ( (Frame) (imageFrames.elementAt(i))).setTitle("Locked: " + titles[i]);
          ( (Frame) (imageFrames.elementAt(i))).setEnabled(false);
          userInterface.unregisterFrame( (Frame) (imageFrames.elementAt(i)));
        }

        if (runInSeparateThread) {
          // Start the thread as a low priority because we wish to still have user interface.
          if (subVOIAlgo.startMethod(Thread.MIN_PRIORITY) == false) {
            MipavUtil.displayError("A thread is already running on this object");
          }
        }
        else {
          subVOIAlgo.setActiveImage(isActiveImage);
          if (!userInterface.isAppFrameVisible()) {
           subVOIAlgo.setProgressBarVisible(false);
          }
          subVOIAlgo.run();
        }
      }
      catch (OutOfMemoryError x) {
        System.gc();
        MipavUtil.displayError(
            "Dialog Subtract VOI: unable to allocate enough memory");
        return;
      }
    }
  }

  /**
   *  Closes dialog box when the OK button is pressed and
   *  calls the algorithm
   *  @param event       event that triggers function
   */
  public void actionPerformed(ActionEvent event) {
    String command = event.getActionCommand();

    if (command.equals("OK")) {
      if (setVariables()) {
        callAlgorithm();
      }
    }
    else if (command.equals("Script")) {
      callAlgorithm();
    }
    else if (command.equals("Cancel")) {
      dispose();
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

       ViewJFrameImage imageFrame = null;
       if (algorithm instanceof AlgorithmSubtractVOI) {
         image.clearMask();
         if (subVOIAlgo.isCompleted() == true && resultImage != null) {
           //The algorithm has completed and produced a new image to be displayed.

           updateFileInfo(image, resultImage);
           resultImage.clearMask();

           try {
             imageFrame = new ViewJFrameImage(resultImage, null, new Dimension(610, 200));
           }
           catch (OutOfMemoryError error) {
             System.gc();
             MipavUtil.displayError("Out of memory: unable to open new frame");
           }
         }
         else if (resultImage == null) {
           // These next lines set the titles in all frames where the source image is displayed to
           // image name so as to indicate that the image is now unlocked!
           // The image frames are enabled and then registered to the userinterface.
           Vector imageFrames = image.getImageFrameVector();
           for (int i = 0; i < imageFrames.size(); i++) {
             ( (Frame) (imageFrames.elementAt(i))).setTitle(titles[i]);
             ( (Frame) (imageFrames.elementAt(i))).setEnabled(true);
             if ( ( (Frame) (imageFrames.elementAt(i))) != parentFrame) {
               userInterface.registerFrame( (Frame) (imageFrames.elementAt(i)));
             }
           }
           if (parentFrame != null)
             userInterface.registerFrame(parentFrame);
           image.notifyImageDisplayListeners(null, true);
         }
         else if (resultImage != null) {
           //algorithm failed but result image still has garbage
           resultImage.disposeLocal(); // clean up memory
           resultImage = null;
           System.gc();
         }
       }

       if (subVOIAlgo.isCompleted() == true) {
         if (userInterface.isScriptRecording()) {
             //check to see if the image is already in the ImgTable
            if (userInterface.getScriptDialog().getImgTableVar(image.getImageName()) == null) {
                if (userInterface.getScriptDialog().getActiveImgTableVar(image.getImageName()) == null) {
                    userInterface.getScriptDialog().putActiveVar(image.getImageName());
                }
            }

           userInterface.getScriptDialog().append("Subtract VOI " +
                                                  userInterface.getScriptDialog().
                                                  getVar(image.getImageName()) +
                                                  " ");
           if (displayLoc == NEW) {
             userInterface.getScriptDialog().putVar(resultImage.getImageName());
             userInterface.getScriptDialog().append(userInterface.
                 getScriptDialog().getVar(resultImage.getImageName()) + " " +
                 averageMode + " " + clipMode + "\n");
           }
           else {
             userInterface.getScriptDialog().append(userInterface.
                 getScriptDialog().getVar(image.getImageName()) + " " +
                 averageMode + " " + clipMode + "\n");

           }
         }
       }

       // Update frame
       if (parentFrame != null)
         ( (ViewJFrameBase) parentFrame).updateImages(true);
       subVOIAlgo.finalize();
       subVOIAlgo = null;
       dispose();
     }

  //*******************************************************************
   //************************* Item Events ****************************
    //*******************************************************************

     /**
      *  itemStateChanged - unchanged
      */
     public void itemStateChanged(ItemEvent event) {
       Object source = event.getSource();
     }

  /**
   *  focusLost    - when the user clicks the mouse out of a text field,
   *                 resets the neccessary variables.
   *  @param event   event that triggers this function
   */
  public void focusLost(FocusEvent event) {

  }

}
