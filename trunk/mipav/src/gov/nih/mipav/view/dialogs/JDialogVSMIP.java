package gov.nih.mipav.view.dialogs;

import gov.nih.mipav.view.*;
import gov.nih.mipav.model.structures.*;
import gov.nih.mipav.model.structures.MIPNode;

import gov.nih.mipav.model.algorithms.*;

import java.awt.event.*;
import java.awt.*;

import javax.swing.*;


/**
 *
 *
 */
public class JDialogVSMIP extends JDialogBase
  implements AlgorithmInterface {

  private ModelImage srcImage;
  private ModelImage mipImage = null;
  private ModelImage resultImage = null;
  private float[] resultBuffer = null;
  private ViewUserInterface userInterface;

  private AlgorithmVSMIP vsMIPAlgo = null;

  private JTextField textMipPlaneWidth, textMipPlaneHeight, textMipStepSize;
  private JTextField textAngleX, textAngleY;
  private JCheckBox iterateButton;
  private JRadioButton viewMipImage, viewAccumulatorImage;

  private int mipPlaneWidth, mipPlaneHeight;
  private float stepSize, angleX, angleY;

  private MIPNode[] mipInfo;

  ViewJFrameImage accumulatorFrame = null;
  ViewJFrameImage mipFrame = null;
  boolean viewMIP = true, viewAccumulator = false;

  boolean iterate = false;




  public JDialogVSMIP(Frame theParentFrame, ModelImage im) {
    super(theParentFrame, false);

    if (im.getNDims() != 3) {
      MipavUtil.displayError("Source Image must be 3D to generate a MIP");
      dispose();
      return;
    }
    srcImage = im;
    userInterface = ((ViewJFrameBase)(parentFrame)).getUserInterface();
    init();
  } // end JDialogVSMIP(...)




  private void init() {
    setForeground(Color.black);
    setTitle("VS MIP Method");

    Box setupBox = new Box(BoxLayout.Y_AXIS);
    JPanel parametersPanel = new JPanel();
    GridBagLayout gbl = new GridBagLayout();
    GridBagConstraints gbc = new GridBagConstraints();
    gbc.anchor = GridBagConstraints.WEST;
    gbc.fill = GridBagConstraints.HORIZONTAL;
    gbc.insets = new Insets(5, 10, 5, 10);
    parametersPanel.setLayout(gbl);

    parametersPanel.setForeground(Color.black);
    parametersPanel.setBorder(buildTitledBorder("Parameters"));


    JLabel mipPlaneWidthLabel = new JLabel("MIP plane width (pixels)");
    mipPlaneWidthLabel.setForeground(Color.black);
    mipPlaneWidthLabel.setFont(serif12);
    gbc.gridwidth = 2;
    gbc.anchor = GridBagConstraints.WEST;
    gbl.setConstraints(mipPlaneWidthLabel, gbc);
    parametersPanel.add(mipPlaneWidthLabel);

    textMipPlaneWidth = new JTextField();
    textMipPlaneWidth.setText(String.valueOf(srcImage.getExtents()[0]));
    textMipPlaneWidth.setColumns(5);
    textMipPlaneWidth.setMaximumSize(textMipPlaneWidth.getPreferredSize());
    textMipPlaneWidth.setHorizontalAlignment(JTextField.RIGHT);
    textMipPlaneWidth.setFont(serif12);
    gbc.gridwidth = GridBagConstraints.REMAINDER;
    gbc.anchor = GridBagConstraints.EAST;
    gbl.setConstraints(textMipPlaneWidth, gbc);
    parametersPanel.add(textMipPlaneWidth);


    JLabel mipPlaneHeightLabel = new JLabel("MIP plane height (pixels)");
    mipPlaneHeightLabel.setForeground(Color.black);
    mipPlaneHeightLabel.setFont(serif12);
    gbc.gridwidth = 2;
    gbc.anchor = GridBagConstraints.WEST;
    gbl.setConstraints(mipPlaneHeightLabel, gbc);
    parametersPanel.add(mipPlaneHeightLabel);

    textMipPlaneHeight = new JTextField();
    textMipPlaneHeight.setText(String.valueOf(srcImage.getExtents()[1]));
    textMipPlaneHeight.setColumns(5);
    textMipPlaneHeight.setMaximumSize(textMipPlaneWidth.getPreferredSize());
    textMipPlaneHeight.setHorizontalAlignment(JTextField.RIGHT);
    textMipPlaneHeight.setFont(serif12);
    gbc.gridwidth = GridBagConstraints.REMAINDER;
    gbc.anchor = GridBagConstraints.EAST;
    gbl.setConstraints(textMipPlaneHeight, gbc);
    parametersPanel.add(textMipPlaneHeight);


    JLabel mipPlaneStepSizeLabel = new JLabel("MIP step size");
    mipPlaneStepSizeLabel.setForeground(Color.black);
    mipPlaneStepSizeLabel.setFont(serif12);
    gbc.gridwidth = 2;
    gbc.anchor = GridBagConstraints.WEST;
    gbl.setConstraints(mipPlaneStepSizeLabel, gbc);
    parametersPanel.add(mipPlaneStepSizeLabel);

    textMipStepSize = new JTextField();
    textMipStepSize.setText(String.valueOf(1.0));
    textMipStepSize.setColumns(5);
    textMipStepSize.setMaximumSize(textMipStepSize.getPreferredSize());
    textMipStepSize.setHorizontalAlignment(JTextField.RIGHT);
    textMipStepSize.setFont(serif12);
    gbc.gridwidth = GridBagConstraints.REMAINDER;
    gbc.anchor = GridBagConstraints.EAST;
    gbl.setConstraints(textMipStepSize, gbc);
    parametersPanel.add(textMipStepSize);



    // add the parameters panel to the setupBox
    setupBox.add(parametersPanel);



    JPanel anglePanel = new JPanel();
    gbc.anchor = GridBagConstraints.WEST;
    gbc.fill = GridBagConstraints.HORIZONTAL;
    gbc.insets = new Insets(5, 10, 5, 10);
    anglePanel.setLayout(gbl);

    anglePanel.setForeground(Color.black);
    anglePanel.setBorder(buildTitledBorder("MIP Plane Control"));


    JLabel mipXAngleLabel = new JLabel("Rotation about X-axis (degrees)");
    mipXAngleLabel.setForeground(Color.black);
    mipXAngleLabel.setFont(serif12);
    gbc.gridwidth = 2;
    gbc.anchor = GridBagConstraints.WEST;
    gbl.setConstraints(mipXAngleLabel, gbc);
    anglePanel.add(mipXAngleLabel);

    textAngleX = new JTextField();
    textAngleX.setText(String.valueOf(0.0));
    textAngleX.setColumns(5);
    textAngleX.setMaximumSize(textAngleX.getPreferredSize());
    textAngleX.setHorizontalAlignment(JTextField.RIGHT);
    textAngleX.setFont(serif12);
    gbc.gridwidth = GridBagConstraints.REMAINDER;
    gbc.anchor = GridBagConstraints.EAST;
    gbl.setConstraints(textAngleX, gbc);
    anglePanel.add(textAngleX);


    JLabel mipYAngleLabel = new JLabel("Rotation about Y-axis (degrees)");
    mipYAngleLabel.setForeground(Color.black);
    mipYAngleLabel.setFont(serif12);
    gbc.gridwidth = 2;
    gbc.anchor = GridBagConstraints.WEST;
    gbl.setConstraints(mipYAngleLabel, gbc);
    anglePanel.add(mipYAngleLabel);

    textAngleY = new JTextField();
    textAngleY.setText(String.valueOf(0.0));
    textAngleY.setColumns(5);
    textAngleY.setMaximumSize(textAngleY.getPreferredSize());
    textAngleY.setHorizontalAlignment(JTextField.RIGHT);
    textAngleY.setFont(serif12);
    gbc.gridwidth = GridBagConstraints.REMAINDER;
    gbc.anchor = GridBagConstraints.EAST;
    gbl.setConstraints(textAngleY, gbc);
    anglePanel.add(textAngleY);

    iterateButton = new JCheckBox("Iterate", true);
    iterateButton.setFont(serif12);
    iterateButton.setSelected(false);
    anglePanel.add(iterateButton);


    setupBox.add(anglePanel);



    JPanel viewPanel = new JPanel();
    gbc.anchor = GridBagConstraints.NORTH;
    gbc.fill = GridBagConstraints.VERTICAL;
    gbc.insets = new Insets(5, 10, 5, 10);
    gbc.gridwidth = 1;
    gbc.gridheight = 2;
    viewPanel.setLayout(gbl);

    viewPanel.setForeground(Color.black);
    viewPanel.setBorder(buildTitledBorder("View Control"));

    viewMipImage = new JRadioButton("MIP image", true);
    viewMipImage.setFont(serif12);
    viewPanel.add(viewMipImage);


    viewAccumulatorImage = new JRadioButton("Accumluator image", false);
    viewAccumulatorImage.setFont(serif12);
    gbc.gridheight = GridBagConstraints.REMAINDER;
    gbc.anchor = GridBagConstraints.SOUTH;
    viewPanel.add(viewAccumulatorImage);

    setupBox.add(viewPanel);




    getContentPane().add(setupBox, BorderLayout.CENTER);
    getContentPane().add(buildButtons(), BorderLayout.SOUTH);
    pack();
    setVisible(true);
    setResizable(false);
    System.gc();
  } // end init()



  public void actionPerformed(ActionEvent event) {
    String command = event.getActionCommand();

    if (command.equals("OK")) {
      if (setVariables()) { callAlgorithm(); }
    } else if (command.equals("Cancel")) {
      dispose();
    } else if (command.equals("Help")) {
      MipavUtil.showHelp("10017");
    } // end if(command.equals("OK")-else
  } // end actionPerformed(...)



  private boolean setVariables() {

    // sanity check to insure we can call the MIP algorithm
    if(srcImage.getNDims() != 3) return false;

    if (resultImage == null) {
      resultImage = new ModelImage(ModelStorageBase.FLOAT, srcImage.getExtents(), "VesselVolume", userInterface);
    }

    // read strings from the text fields and convert to an integers
    String tmpStr = textMipPlaneWidth.getText();
    mipPlaneWidth = Integer.valueOf(tmpStr).intValue();

    tmpStr = textMipPlaneHeight.getText();
    mipPlaneHeight = Integer.valueOf(tmpStr).intValue();

    mipInfo = new MIPNode[mipPlaneWidth * mipPlaneHeight];
    for(int i = 0; i < mipInfo.length; i++) {
      mipInfo[i] = new MIPNode();
    }

    tmpStr = textMipStepSize.getText();
    stepSize = Float.valueOf(tmpStr).floatValue();

    tmpStr = textAngleX.getText();
    angleX = Float.valueOf(tmpStr).floatValue();

    tmpStr = textAngleY.getText();
    angleY = Float.valueOf(tmpStr).floatValue();

    int[] mipDims = new int[2];
    mipDims[0] = mipPlaneWidth;
    mipDims[1] = mipPlaneHeight;
    mipImage = new ModelImage(ModelStorageBase.FLOAT, mipDims, "MIP", userInterface);
    if (viewMipImage.isSelected()) {
      viewMIP = true;
    }
    if (viewAccumulatorImage.isSelected()) {
      viewAccumulator = true;
    }

    if (iterateButton.isSelected()) {
      iterate = true;
      viewAccumulator = true;
    } else {
      iterate = false;
    }

    return true;
  } // end setVariables()



  private void callAlgorithm() {
    String name = makeImageName(srcImage.getImageName(), "_vsMIP");

    try {
      vsMIPAlgo = new AlgorithmVSMIP(resultImage, mipImage, srcImage,
                                     mipPlaneWidth, mipPlaneHeight, stepSize,
                                     angleX, angleY, iterate);

      // This is very important. Adding this object as a listener allows the algorithm to
      // notify this object when it has completed or failed. See algorithm performed event.
      // This is made possible by implementing AlgorithmedPerformed interface
      vsMIPAlgo.addListener(this);

      if (runInSeparateThread) {
        if (vsMIPAlgo.startMethod(Thread.MIN_PRIORITY) == false) {
          MipavUtil.displayError("A thread is already running on this object");
        }
      } else {
        vsMIPAlgo.setActiveImage(isActiveImage);
        if (!userInterface.isAppFrameVisible()) {
          vsMIPAlgo.setProgressBarVisible(false);
        }
        vsMIPAlgo.run();
      } // end if (runInSeparateThread)

    } catch (OutOfMemoryError x) {
      MipavUtil.displayError("JDialogVSMIP: unable to allocate enough memory for MIP image");
      if (resultImage != null) { resultImage.disposeLocal(); resultImage = null; }
      return;
    } // end try()=catch()

  } // end callAlgorithm()



  public void algorithmPerformed(AlgorithmBase algorithm) {
    if (algorithm instanceof AlgorithmVSMIP) {
      if (vsMIPAlgo.isCompleted() == true) {

        if (viewMIP && mipImage != null) {
          try {
            mipFrame = new ViewJFrameImage(mipImage, null, new Dimension(610, 200));
          } catch (OutOfMemoryError error) {
            System.gc();
            MipavUtil.displayError("Out of memory: unable to open new frame");
          }
        } // end if(viewMIP)

        if (viewAccumulator && accumulatorFrame == null) {
          try {
            accumulatorFrame = new ViewJFrameImage(resultImage, null, new Dimension(610, 200));
          } catch (OutOfMemoryError error) {
            System.gc();
            MipavUtil.displayError("Out of memory: unable to open new frame");
          }
        } else if (viewAccumulator && accumulatorFrame != null) {
          accumulatorFrame.updateImages(true);
        } // end if (accumulatorFrame == null ...)

      } // end if (vsMIPAlgo.isCompleted() ...)
    } // end if (algorithm ...)
  } // end algorithmPerformed(...)


/*
  public void algorithmPerformed(AlgorithmBase algorithm) {
    if (algorithm instanceof AlgorithmVSMIP) {
      if (vsMIPAlgo.isCompleted() == true) {

        if (viewMIP) {
          float[] mipBuffer;
          int mipLength;
          try {
            mipLength = mipImage.getSliceSize();
            mipBuffer = new float[mipLength];
            mipImage.exportData(0, mipLength, mipBuffer);
          } catch (IOException error) {
            mipBuffer = null;
            MipavUtil.displayError("AlgorithmVSMIP::run()  could NOT export source image");
            return;
          } catch (OutOfMemoryError e){
            mipBuffer = null;
            MipavUtil.displayError("AlgorithmVSMIP::run()  Out of memory when creating image buffer");
            return;
          } // end try{}-catch{}-catch{}

          // make the mip image
          for(int i = 0; i < mipLength; i++) {
            mipBuffer[i] = mipInfo[i].intensity;
          }


          try { mipImage.importData(0, mipBuffer, true);  }
          catch (IOException error) {
            mipBuffer = null;
            MipavUtil.displayError("AlgorithmVSMIP::run()  Could NOT import destBuffer to the image");
            return;
          } // end try{}-catch{}


          try {
            mipFrame = new ViewJFrameImage(mipImage, null,
                                         new Dimension(610, 200),
                                           userInterface);
          } catch (OutOfMemoryError error) {
            System.gc();
            MipavUtil.displayError("Out of memory: unable to open new frame");
          }
        } // end if(viewMIP)
        if (accumulatorFrame == null && viewAccumulator) {
          try {
            accumulatorFrame = new ViewJFrameImage(resultImage, null,
                                            new Dimension(610, 200),
                                            userInterface);
          } catch (OutOfMemoryError error) {
            System.gc();
            MipavUtil.displayError("Out of memory: unable to open new frame");
          }
        } else if (accumulatorFrame != null) {
          accumulatorFrame.updateImages(true);
        }
      } // end if (vsMIPAlgo.isCompleted() ...)
    } // end if (algorithm ...)
  } // end algorithmPerformed(...)
*/

/*
  public void algorithmPerformed(AlgorithmBase algorithm) {
    ViewJFrameImage imageFrame = null;
    if (algorithm instanceof AlgorithmVSMIP) {
        image.clearMask();
        if (vsMIPAlgo.isCompleted() == true && resultImage != null) {
          //The algorithm has completed and produced a new image to be displayed.

          // updateFileInfo fails if the resultImage has a different number of dimensions than image
          if (image.getNDims() == resultImage.getNDims()) {
            updateFileInfo(image, resultImage);
          }
          resultImage.clearMask();
          try {
            //resultImage.setImageName("Median: "+image.getImageName());
            imageFrame = new ViewJFrameImage(resultImage, null,
                                            new Dimension(610, 200),
                                            userInterface);
          }
          catch (OutOfMemoryError error) {
            System.gc();
            MipavUtil.displayError("Out of memory: unable to open new frame");
          }
        } // end if (vsMIPAlgo.isCompleted() ...)
      } // end if (algorithm ...)
   } // end algorithmPerformed(...)
*/


} // end class JDialogVSMIP
