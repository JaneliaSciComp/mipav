package gov.nih.mipav.view.dialogs;

import gov.nih.mipav.view.*;
import gov.nih.mipav.model.structures.*;

import gov.nih.mipav.model.algorithms.*;
import gov.nih.mipav.model.algorithms.filters.*;

import java.awt.event.*;
import java.awt.*;
import javax.swing.*;
import java.text.DecimalFormat;


/**
 *   Dialog to get user input, then call a specified diffusion algorithm.
 *   It should be noted that the algorithms are executed in their own
 *   thread.
 */
public class JDialogRegularizedIsotropicDiffusion extends JDialogBase implements AlgorithmInterface, ScriptableInterface {

  private AlgorithmRegularizedIsotropicDiffusion regIsoDiffusionAlgo = null;

  private ModelImage srcImage;
  private ModelImage resultImage;
  private ViewUserInterface userInterface = null;
  private DecimalFormat fltFmt;
  private int numIterations = 1;
  private float stdDev = 1.0f;
  private float contrast = 0.15f;

  private JTextField textNumberIterations, textGaussian, textContrast;
  private JCheckBox checkBox25D;
  private boolean   do25D = true;


  public JDialogRegularizedIsotropicDiffusion(Frame frame, ModelImage im) {
    super(frame, false);

    srcImage = im;
    userInterface = ((ViewJFrameBase)(parentFrame)).getUserInterface();
    fltFmt = new DecimalFormat("0.00");
    init();
  } // end JDialogDiffusion(...)


  /**
   *	Used primarily for the script to store variables and run the algorithm.  No
   *	actual dialog will appear but the set up info and result image will be stored here.
   *	@param UI   The user interface, needed to create the image frame.
   *	@param im	Source image.
   */
  public JDialogRegularizedIsotropicDiffusion(ViewUserInterface UI, ModelImage im) {
    super();
    userInterface = UI;
    srcImage = im;
    parentFrame = im.getParentFrame();
  }

  /**
   * Empty constructor needed for dynamic instantiation (used during scripting).
   */
  public JDialogRegularizedIsotropicDiffusion() {}

  /**
   * Run this algorithm from a script.
   * @param parser the script parser we get the state from
   * @throws IllegalArgumentException if there is something wrong with the arguments in the script
   */
  public void scriptRun (AlgorithmScriptParser parser) throws IllegalArgumentException {
      String srcImageKey = null;
      String destImageKey = null;

      try {
          srcImageKey = parser.getNextString();
      } catch (Exception e) {
          throw new IllegalArgumentException();
      }
      ModelImage im = parser.getImage(srcImageKey);

      srcImage = im;
      userInterface = srcImage.getUserInterface();
      parentFrame = srcImage.getParentFrame();

      // the result image
      try {
          destImageKey = parser.getNextString();
      } catch (Exception e) {
          throw new IllegalArgumentException();
      }

      try {
          setNumIterations(parser.getNextInteger());
          setStdDev(parser.getNextFloat());
          setContrast(parser.getNextFloat());
          setDo25D(parser.getNextBoolean());
      } catch (Exception e) {
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
  public void insertScriptLine (AlgorithmBase algo) {
      if (algo.isCompleted()) {
          if (userInterface.isScriptRecording()) {
              //check to see if the image is already in the ImgTable
              if (userInterface.getScriptDialog().getImgTableVar(srcImage.getImageName()) == null) {
                  if (userInterface.getScriptDialog().getActiveImgTableVar(srcImage.getImageName()) == null) {
                      userInterface.getScriptDialog().putActiveVar(srcImage.getImageName());
                  }
              }

              userInterface.getScriptDialog().append("RegularizedIsotropicDiffusion " +
                                                     userInterface.getScriptDialog().
                                                     getVar(srcImage.getImageName()) +
                                                     " ");
              //if (displayLoc == NEW) {
              userInterface.getScriptDialog().putVar(resultImage.getImageName());
              userInterface.getScriptDialog().append(userInterface.
                                                     getScriptDialog().getVar(resultImage.getImageName()) + " "
                                                     + numIterations + " " + stdDev + " " + contrast +
                                                     " " + do25D + "\n");
              //}
              /*else {
                userInterface.getScriptDialog().append(userInterface.
                    getScriptDialog().getVar(image.getImageName()) + " " +
                    + numIterations + " " + stdDev + " " + contrast +
                    " " + do25D + "\n");
                       }*/
          }
      }
  }

  private void init() {
    setForeground(Color.black);
    setTitle("Regularized Isotropic Diffusion");
    JPanel paramPanel = buildParameterPanel();

    GridBagConstraints gbc = new GridBagConstraints();
    gbc.gridx = 0;
    gbc.weightx = 1.0;
    gbc.weighty = 1.0;
    gbc.fill = gbc.HORIZONTAL;

    JPanel mainPanel = new JPanel(new GridBagLayout());
    gbc.gridy = 0;
    mainPanel.add(paramPanel, gbc);

    getContentPane().add(mainPanel);
    getContentPane().add(buildButtons(), BorderLayout.SOUTH);

    pack();
    setVisible(true);
    setResizable(false);

  } // end init()


  private JPanel buildParameterPanel() {
    JPanel parameterPanel = new JPanel(new GridBagLayout());
    parameterPanel.setForeground(Color.black);
    parameterPanel.setBorder(buildTitledBorder("Parameters"));

    JLabel labelNumberIterations = new JLabel("Number of Iterations");
    labelNumberIterations.setFont(serif12);
    textNumberIterations = new JTextField();
    textNumberIterations.setText(String.valueOf(numIterations));
    textNumberIterations.setColumns(5);
    textNumberIterations.setMaximumSize(textNumberIterations.getPreferredSize());
    textNumberIterations.setHorizontalAlignment(JTextField.RIGHT);
    textNumberIterations.setFont(serif12);

    JLabel labelGaussian = new JLabel("Gaussian standard deviation");
    labelGaussian.setFont(serif12);
    textGaussian = new JTextField();
    textGaussian.setText(String.valueOf(stdDev));
    textGaussian.setColumns(5);
    textGaussian.setMaximumSize(textGaussian.getPreferredSize());
    textGaussian.setHorizontalAlignment(JTextField.RIGHT);
    textGaussian.setFont(serif12);

    JLabel labelContrast = new JLabel("Diffusion contrast parameter");
    labelContrast.setFont(serif12);
    textContrast = new JTextField();
    textContrast.setText(String.valueOf(contrast));
    textContrast.setColumns(5);
    textContrast.setMaximumSize(textContrast.getPreferredSize());
    textContrast.setHorizontalAlignment(JTextField.RIGHT);
    textContrast.setFont(serif12);

    if (srcImage.getNDims() >= 3) {
        checkBox25D = new JCheckBox("Process each slice separately",true);
        checkBox25D.setFont(serif12);
    }

    GridBagConstraints gbc = new GridBagConstraints();
    gbc.anchor = gbc.WEST;
    gbc.gridheight = 1;
    gbc.gridwidth  = 1;
    gbc.insets = new Insets(6, 6, 6, 6);

    gbc.gridy = 0;
    gbc.gridx = 0; parameterPanel.add(labelNumberIterations, gbc);
    gbc.gridx = 1; parameterPanel.add(textNumberIterations, gbc);

    gbc.gridy++;
    gbc.gridx = 0; parameterPanel.add(labelGaussian, gbc);
    gbc.gridx = 1; parameterPanel.add(textGaussian, gbc);

    gbc.gridy++;
    gbc.gridx = 0; parameterPanel.add(labelContrast, gbc);
    gbc.gridx = 1; parameterPanel.add(textContrast, gbc);

    if (srcImage.getNDims() >= 3) {
        gbc.gridy++;
        gbc.gridx = 0; parameterPanel.add(checkBox25D, gbc);
    }


    return parameterPanel;
  }  // end buildParameterPanel()

  /**
  *  Accessor that sets the number of iterations
  *  @param numIterations
  */
  public void setNumIterations(int numIterations) {
    this.numIterations = numIterations;
  }

  /**
  *  Accessor that sets the standard deviastion
  *  @param stdDev
  */
  public void setStdDev(float stdDev) {
    this.stdDev = stdDev;
  }

  /**
  *  Accessor that sets the contrast
  *  @param contrast
  */
  public void setContrast(float contrast) {
    this.contrast = contrast;
  }

  /**
  *  Accessor that sets if slice by slice processing occurs
  *  @param do25D
  */
  public void setDo25D(boolean do25D) {
    this.do25D = do25D;
  }

  /**
   *  Accessor that returns the image.
   *  @return          The result image.
   */
  public ModelImage getResultImage() {
    return resultImage;
  }

  private boolean setVariables() {
    String tmpStr;
    tmpStr = textNumberIterations.getText();
    numIterations = Integer.valueOf(tmpStr).intValue();

    tmpStr = textGaussian.getText();
    stdDev = Float.valueOf(tmpStr).floatValue();

    tmpStr = textContrast.getText();
    contrast = Float.valueOf(tmpStr).floatValue();

    if (srcImage.getNDims() >= 3) {
        do25D = checkBox25D.isSelected();
    }

    return true;
  } // end setVariables()



  /**
  *  Closes dialog box when the OK button is pressed and calls the algorithm.
  *  @param event       Event that triggers function.
  */
  public void actionPerformed(ActionEvent event) {
    String command = event.getActionCommand();
    if (command.equals("OK")) {
      if(setVariables()) { callAlgorithm(); }
    }
    else if (command.equals("Cancel")) {
      dispose();
    }
    else if (command.equals("Help")) {
      MipavUtil.showHelp("10088");
    }// end if()-else
  } // end actionPerformed(...)





  /**
  *	Once all the necessary variables are set, call the mean
  *	algorithm based on what type of image this is and whether or not there
  *	is a separate destination image.
  */
  private void callAlgorithm() {
    float[] sigmas = null;
    String name;

      name = makeImageName(srcImage.getImageName(), "_rid");

      try {
        if (srcImage.isColorImage()) {
            resultImage = new ModelImage(srcImage.getType(),
                                     srcImage.getExtents(), name, userInterface);
        }
        else {
            resultImage = new ModelImage(ModelStorageBase.FLOAT,
                                     srcImage.getExtents(), name, userInterface);
        }
        regIsoDiffusionAlgo = new AlgorithmRegularizedIsotropicDiffusion(resultImage,
                              srcImage, numIterations, stdDev, contrast, do25D);

        // This is very important. Adding this object as a listener allows the algorithm to
        // notify this object when it has completed or failed. See algorithm performed event.
        // This is made possible by implementing AlgorithmedPerformed interface
        regIsoDiffusionAlgo.addListener(this);

        if (runInSeparateThread) {
          // Start the thread as a low priority because we wish to still have user interface work fast.
          if (regIsoDiffusionAlgo.startMethod(Thread.MIN_PRIORITY) == false) {
            MipavUtil.displayError("A thread is already running on this object");
          }
        }
        else {
          regIsoDiffusionAlgo.setActiveImage(isActiveImage);
          if (!userInterface.isAppFrameVisible()) {
              regIsoDiffusionAlgo.setProgressBarVisible(false);
          }
          regIsoDiffusionAlgo.run();
        } // end if (runInSeparateThread)
      }
      catch (OutOfMemoryError x) {
        MipavUtil.displayError("JDialogRegIsoDiffusion: unable to allocate enough memory");
        if (resultImage != null) {
          resultImage.disposeLocal(); // Clean up memory of result image
          resultImage = null;
        }
        return;
      } // end try()=catch()
      dispose();
  } // end callAlgorithm()



  public void algorithmPerformed(AlgorithmBase algorithm) {
    ViewJFrameImage imageFrame = null;
    if ((algorithm instanceof AlgorithmRegularizedIsotropicDiffusion &&
         regIsoDiffusionAlgo.isCompleted() == true) && resultImage != null) {

      updateFileInfo(srcImage, resultImage);
      resultImage.clearMask();
      try {
        imageFrame = new ViewJFrameImage(resultImage, null, new Dimension(610, 200));
      } catch (OutOfMemoryError error) {
        System.gc();
        MipavUtil.displayError("Out of memory: unable to open new frame");
      }

      insertScriptLine(algorithm);

      regIsoDiffusionAlgo.finalize();
      regIsoDiffusionAlgo = null;
    }
  } // end algorithmPerformed(...)


} // end class JDialogRegIsoDiffusion
