package gov.nih.mipav.view.dialogs;

import gov.nih.mipav.view.*;
import gov.nih.mipav.model.structures.*;
import gov.nih.mipav.model.algorithms.*;
import gov.nih.mipav.model.algorithms.registration.*;

import java.awt.event.*;
import java.awt.*;
import javax.swing.*;

/**
 *   Dialog to get user input, then call algorithmRegistrationShear.
 *
 */
public class JDialogRegistrationShear
    extends JDialogBase
    implements AlgorithmInterface, ScriptableInterface {
  private ViewUserInterface UI;
  private AlgorithmRegistrationShear RegShear = null;
  private ModelImage imageB; //register imageB to imageA
  private ModelImage imageA;
  private ModelImage resultImage = null; // result image
  private JComboBox comboBoxImage;
  private JComboBox comboBoxInterp;
  private int interpolationMethod;

  /**
   *   Creates a new dialog for getting information to run
   *   the registration.
   *   @param theParentFrame  Parent frame
   *   @param im              Source image
   */
  public JDialogRegistrationShear(Frame theParentFrame, ModelImage im) {
    super(theParentFrame, true);
    imageB = im;
    UI = ( (ViewJFrameBase) parentFrame).getUserInterface();
    init();
  }

  /**
   *	Used primarily for the script to store variables and run the algorithm.  No
   *	actual dialog will appear but the set up info and result image will be stored here.
   *	@param _UI   The user interface, needed to create the image frame.
   *	@param im	Source image.
   */
  public JDialogRegistrationShear(ViewUserInterface _UI, ModelImage im) {
    super();
    UI = _UI;
    imageB = im;
    parentFrame = im.getParentFrame();
  }

  /**
   * Empty constructor needed for dynamic instantiation (used during scripting).
   */
  public JDialogRegistrationShear() {}

  /**
   * Run this algorithm from a script.
   * @param parser the script parser we get the state from
   * @throws IllegalArgumentException if there is something wrong with the arguments in the script
   */
  public void scriptRun (AlgorithmScriptParser parser) throws IllegalArgumentException {
      String image1Key = null;
      String image2Key = null;
      String destImageKey = null;

      try {
          image1Key = parser.getNextString();
          image2Key = parser.getNextString();
      } catch (Exception e) {
          throw new IllegalArgumentException();
      }
      ModelImage im = parser.getImage(image1Key);

      imageB = im;
      UI = imageB.getUserInterface();
      parentFrame = imageB.getParentFrame();
      setImageA(parser.getImage(image2Key));

      // the result image
      try {
          destImageKey = parser.getNextString();
      } catch (Exception e) {
          throw new IllegalArgumentException();
      }

      try {
          setInterpolationMethod(parser.getNextInteger());
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
              if (UI.getScriptDialog().getImgTableVar(imageB.getImageName()) == null) {
                  if (UI.getScriptDialog().getActiveImgTableVar(imageB.getImageName()) == null) {
                      UI.getScriptDialog().putActiveVar(imageB.getImageName());
                  }
              }

              //check to see if the match image is already in the ImgTable
              if (UI.getScriptDialog().getImgTableVar(imageA.getImageName()) == null) {
                  if (UI.getScriptDialog().getActiveImgTableVar(imageA.getImageName()) == null) {
                      UI.getScriptDialog().putActiveVar(imageA.getImageName());
                  }
              }

              UI.getScriptDialog().append("RegistrationShear " +
                                          UI.getScriptDialog().getVar(imageB.getImageName()) +
                                          " " +
                                          UI.getScriptDialog().getVar(imageA.getImageName()) +
                                          " ");
              UI.getScriptDialog().putVar(resultImage.getImageName());
              UI.getScriptDialog().append(UI.
                                          getScriptDialog().getVar(resultImage.getImageName()) + " " +
                                          interpolationMethod + "\n");
          }
      }
  }

  /**
  *   The base imageA
  *   @param imageA
  */
  public void setImageA(ModelImage imageA) {
      this.imageA = imageA;
  }

  /**
  *   The interpolation method - cubic, quintic, or heptic Lagrangian
  *   @param interpolationMethod
  */
  public void setInterpolationMethod(int interpolationMethod) {
      this.interpolationMethod = interpolationMethod;
  }

  /**
   *  Accessor that returns the image.
   *  @return          The result image.
   */
  public ModelImage getResultImage() {
      return resultImage;
  }

  /**
   *	Initializes GUI components and displays dialog.
   */
  private void init() {
    setForeground(Color.black);
    setTitle("Shear Registration");

    JPanel optPanel = new JPanel();
    optPanel.setLayout(new GridBagLayout());
    optPanel.setBorder(buildTitledBorder("Input Options"));

    String matchName = imageB.getImageName();
    JLabel labelImage = new JLabel("Register [" + matchName + "] to:");
    labelImage.setForeground(Color.black);
    labelImage.setFont(serif12);
    labelImage.setAlignmentX(Component.LEFT_ALIGNMENT);

    comboBoxImage = buildImageComboBox(imageB);

    JLabel labelInterp = new JLabel("Interpolation:");
    labelInterp.setForeground(Color.black);
    labelInterp.setFont(serif12);
    labelInterp.setAlignmentX(Component.LEFT_ALIGNMENT);

    comboBoxInterp = new JComboBox();
    comboBoxInterp.setFont(serif12);
    comboBoxInterp.setBackground(Color.white);
    comboBoxInterp.setAlignmentX(Component.LEFT_ALIGNMENT);

    comboBoxInterp.addItem("Cubic Lagrangian");
    comboBoxInterp.addItem("Quintic Lagrangian");
    comboBoxInterp.addItem("Heptic Lagrangian");

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
    gbc.gridy = 1;
    gbc.weightx = 0;
    gbc.fill = gbc.NONE;
    optPanel.add(labelInterp, gbc);
    gbc.gridx = 1;
    gbc.gridy = 1;
    gbc.weightx = 1;
    gbc.fill = gbc.HORIZONTAL;
    optPanel.add(comboBoxInterp, gbc);

    buildOKButton();
    buildCancelButton();
    JPanel buttonPanel = new JPanel();
    buttonPanel.add(OKButton);
    buttonPanel.add(cancelButton);
    getContentPane().add(optPanel);
    getContentPane().add(buttonPanel, BorderLayout.SOUTH);
    pack();
    setVisible(true);
  }

  /**
   *    Closes dialog box when the OK button is pressed and
   *    calls the algorithm
   *    @param event       Event that triggers function
   */
  public void actionPerformed(ActionEvent event) {
    Object source = event.getSource();
    String command = event.getActionCommand();

    if (source == OKButton) {
      if (setVariables()) {
        setVisible(false);
        callAlgorithm();
      }
    }
    else if (source == cancelButton) {
      dispose();
    }
  }

  /**
   *   Sets the variables based on the GUI.
   *   @return Flag indicating successful set.
   */
  private boolean setVariables() {
    String selectedName = (String) comboBoxImage.getSelectedItem();
    imageA = UI.getRegisteredImageByName(selectedName);

    if (imageA.getNDims() != 3) {
      MipavUtil.displayError("This algorithm only works for 3D datasets.");
      return false;
    }
    switch (comboBoxInterp.getSelectedIndex()) {
      case 1:
        interpolationMethod = AlgorithmTransform.QUINTIC_LAGRANGIAN;
        break;
      case 2:
        interpolationMethod = AlgorithmTransform.HEPTIC_LAGRANGIAN;
        break;
      case 0:
      default:
        interpolationMethod = AlgorithmTransform.CUBIC_LAGRANGIAN;
        break;
    }
    return true;
  }

  /**
   *   Calls the algorithm.
   */
  private void callAlgorithm() {
    try {
      int destExtents[] = new int[3];
      destExtents[0] = imageA.getExtents()[0];
      destExtents[1] = imageA.getExtents()[1];
      destExtents[2] = imageA.getExtents()[2];

      String name = makeImageName(imageA.getImageName(), "_regShear");

      // Make NEW result image of float type
      resultImage = new ModelImage(ModelImage.FLOAT, destExtents, name, UI);
      RegShear = new AlgorithmRegistrationShear(imageA, imageB, resultImage, UI,
                                                interpolationMethod);

    }
    catch (OutOfMemoryError x) {
      MipavUtil.displayError(
          "Dialog Register Shear: unable to allocate enough memory");
      return;
    }

    // This is very important. Adding this object as a listener allows
    // the algorithm to notify this object when it has completed of failed.
    // See algorithm performed event. This is made possible by implementing
    RegShear.addListener(this);

    if (runInSeparateThread) {
      // Start the thread as a low priority because we wish to still have
      // user interface work fast
      if (RegShear.startMethod(Thread.MIN_PRIORITY) == false) {
        MipavUtil.displayError("A thread is already running on this object");
      }
    }
    else {
      RegShear.setActiveImage(isActiveImage);
      if (!UI.isAppFrameVisible()) {
        RegShear.setProgressBarVisible(false);
      }
      RegShear.run();
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
       if (algorithm instanceof AlgorithmRegistrationShear) {
         if ( (RegShear.isCompleted() == true) && (resultImage != null)) {
           resultImage.calcMinMax();
           //The algorithm has completed and produced a new image to be displayed.
           try {
             //resultImage.setImageName("Registered imageB");
             imageFrame = new ViewJFrameImage(resultImage, null, new Dimension(610, 200));
           }
           catch (OutOfMemoryError error) {
             MipavUtil.displayError("Out of memory: unable to open new frame");
           }

           insertScriptLine(algorithm);
         }

         else if (resultImage != null) {
           //algorithm failed but result image still has garbage
           resultImage.disposeLocal(); // clean up memory
           resultImage = null;
         }
       }
       // Update frames
       imageA.notifyImageDisplayListeners(null, true);
       imageB.notifyImageDisplayListeners(null, true);
       RegShear.finalize();
       RegShear = null;
       dispose();
     }

  /**
   * constructLog - constructs a string of the construction parameters and
   *                outputs the string to the messsage frame if the logging
   *                procedure is turned on.
   */
  private void constructLog() {

    String logString = new String("Register " + imageB.getImageName() +
                                  " to " + imageA.getImageName() + "\n");
    //Preferences.log(imageB.getUserInterface(), logString);
  }

  /**
   *   closingLog  - constructs a string indicating if the whether or not
   *                 the algorithm completed sucessfully.
   *   overrides AlgorithmBase's nonfunctional closingLog function
   */
  protected void closingLog() {
    String logString;
    if (RegShear.isCompleted() == true) {
      logString = new String("Register " + imageB.getImageName() +
                             " to " + imageA.getImageName() +
                             " Completed successfully!" + "\n");
    }
    else {
      logString = new String("Register " + imageB.getImageName() +
                             " to " + imageA.getImageName() +
                             " Algorithm failed!" + "\n");
    }
    //Preferences.log(imageB.getUserInterface(), logString);
  }

}
