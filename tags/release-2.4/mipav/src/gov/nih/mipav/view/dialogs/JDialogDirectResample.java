package gov.nih.mipav.view.dialogs;

import gov.nih.mipav.view.*;
import gov.nih.mipav.model.structures.*;
import gov.nih.mipav.model.algorithms.*;
import gov.nih.mipav.view.*;


import java.awt.event.*;
import java.awt.*;
import javax.swing.*;

/**
*	Dialog to ask user to resample the images or not.
*	@author Ruida Cheng
*/
public class JDialogDirectResample extends JDialogBase{

   /** Left panel and right panels corresponding to original and expected extents. */
   JPanel leftPanel, rightPanel;

   /** Original X, Y, Z dimension extents values. */
   JTextField extXInput, extYInput, extZInput;

    /** Power of 2 X, Y, Z dimension extents values. */
   JTextField extXOutput, extYOutput, extZOutput;

   /** Original dimensions extents value array. */
   int extents[];

   /** Original resolutioin arrray */
   float res[];

   /** Model images A and B */
   ModelImage imageA, imageB;

   /** Parent ui. */
   ViewUserInterface userInterface;

   /** Component image. */
   ViewJComponentEditImage componentImage;

   /** Parent ViewJFrameImage. */
   ViewJFrameImage resampledImageFrame;

   /** Boolean flag to enable volume render button. */
   boolean enableVolRender = true;

  /** Number of available dimension */
   int dim;

   /**  Boolean flag to do resample images. */
   boolean forceResample = false;

   /** Resampled dimension value in Power of 2. */
   int volExtents[] = new int[3];

   /** Resample resolutioin corresponding to Power of 2. */
   float newRes[] = new float[3];

   /** Volume size X*Y*Z */
   int volSize = 1;

   /** Resampled model images A and B. */
   ModelImage resImageA, resImageB;

   /** Temp Model image. */
   ModelImage resampledImage     = null;

   /** Boolean flag to indicate the original image is in Power of 2. */
   boolean originalVolPowerOfTwo = true;

    /**
    *	Creates the dialog, using the input parameters to place
    *	it on the screen.
    *	@param _imageA         Model image A.
    *	@param _imageB         Model image B.
    *   @param _userInterface    Parent ui.
    *   @param _componentImage   Dicom converted to Java image.
    *   @param _resampledImageFrame  Parent image frame ViewJFrameImage.
    */
    public JDialogDirectResample(ModelImage _imageA, ModelImage _imageB,
                             ViewUserInterface _userInterface,
                             ViewJComponentEditImage _componentImage,
                             ViewJFrameImage _resampledImageFrame) {
            super(_userInterface.getMainFrame(), false);
            this.imageA = _imageA;
            this.imageB = _imageB;
            resImageA = _imageA;
            resImageB = _imageB;
            this.userInterface = _userInterface;
            this.componentImage = _componentImage;
            this.resampledImageFrame = _resampledImageFrame;
            extents = imageA.getExtents();
            res = imageA.getFileInfo(0).getResolutions();
            this.dim = extents.length;
            for(int i = 0; i < extents.length; i++ ) {
              volExtents[i] = dimPowerOfTwo(extents[i]);
              volSize *= volExtents[i];
              if (volExtents[i] != extents[i])
                originalVolPowerOfTwo = false;
                newRes[i] = (res[i] * (extents[i]-1)) / (volExtents[i]-1);
            }
            init();
    }

    public JDialogDirectResample(ModelImage _imageA, ModelImage _imageB,
                             ViewUserInterface _userInterface,
                             ViewJComponentEditImage _componentImage) {
            super(_userInterface.getMainFrame(), false);
            this.imageA = _imageA;
            this.imageB = _imageB;
            resImageA = _imageA;
            resImageB = _imageB;
            this.userInterface = _userInterface;
            this.componentImage = _componentImage;
            extents = imageA.getExtents();
            res = imageA.getFileInfo(0).getResolutions();
            this.dim = extents.length;
            for(int i = 0; i < extents.length; i++ ) {
              volExtents[i] = dimPowerOfTwo(extents[i]);
              volSize *= volExtents[i];
              if (volExtents[i] != extents[i])
                originalVolPowerOfTwo = false;
                newRes[i] = (res[i] * (extents[i]-1)) / (volExtents[i]-1);
            }
    }


    /**
    *	On "OK", sets the name variable to the text entered.  On "Cancel"
    *	disposes of this dialog and sets cancel flag.
    *	@param event	Event that triggered this method.
    */
    public void actionPerformed(ActionEvent event) {
        String command = event.getActionCommand();
        if (command.equals("Resample")) {
          volExtents[0] = dimPowerOfTwo(Integer.parseInt(extXOutput.getText()));
          newRes[0] =  (float)((extents[0]-1)*res[0])/(float)(volExtents[0]-1);
          volExtents[1] = dimPowerOfTwo(Integer.parseInt(extYOutput.getText()));
          newRes[1] =  (float)((extents[1]-1)*res[1])/(float)(volExtents[1]-1);
          if ( dim >= 3 ) {
             volExtents[2] = dimPowerOfTwo(Integer.parseInt(extZOutput.getText()));
             newRes[2] =  (float)((extents[2]-1)*res[2])/(float)(volExtents[2]-1);
          }

          if ( dim >= 3 ) {
             if ( extents[0] == volExtents[0] && extents[1] == volExtents[1] && extents[2] == volExtents[2]) {
                     forceResample = false;
             } else {
                     forceResample = true;
             }
          } else {
             if ( extents[0] == volExtents[0] && extents[1] == volExtents[1] ) {
                    forceResample = false;
             } else {
                    forceResample = true;
             }
          }
          doResample();
          dispose();
        }
        else if (command.equals("Cancel")) {
          dispose();
        } else if ( command.equals("xChanged") ) {
              int x = Integer.parseInt(extXOutput.getText());
              x = dimPowerOfTwo(x);
              extXOutput.setText(Integer.toString(x));
        } else if ( command.equals("yChanged") ) {
              int y = Integer.parseInt(extYOutput.getText());
              y = dimPowerOfTwo(y);
              extYOutput.setText(Integer.toString(y));
        } else if ( command.equals("zChanged") ) {
              int z = Integer.parseInt(extZOutput.getText());
              z = dimPowerOfTwo(z);
              extZOutput.setText(Integer.toString(z));
        }
    }

    /**
     *  Build the resample dialog.
     */
    public void init() {
      setTitle("Resample Dialog");
      Box mainBox = new Box(BoxLayout.Y_AXIS);
      /*
      JPanel msgPanel = new JPanel();
      msgPanel.setLayout(new BorderLayout());
      msgPanel.setBorder(buildTitledBorder(""));
      msgPanel.add(new JLabel("Do you want to resample the images?"), BorderLayout.NORTH);
      msgPanel.add(new JLabel("You can modify the extents to Power of 2."), BorderLayout.CENTER);
      */
      JPanel endPanel = new JPanel();
      endPanel.setLayout(new BorderLayout());
      //endPanel.add(new JLabel("Selecting Resample will resample the extents to Power of 2."), BorderLayout.NORTH);
      //endPanel.add(new JLabel("Cancel will exit the resample dialog."), BorderLayout.CENTER);

      // msgPanel.add(endPanel, BorderLayout.SOUTH);

      // mainBox.add(msgPanel);
      mainBox.add(endPanel);

      Box contentBox = new Box(BoxLayout.X_AXIS);
      JPanel leftPanel = new JPanel();
      JPanel rightPanel = new JPanel();
      // make border
      leftPanel.setBorder(buildTitledBorder("Original Extents"));
      contentBox.add(leftPanel);

      // set layout
      GridBagLayout gbl = new GridBagLayout();
      GridBagConstraints gbc = new GridBagConstraints();
      leftPanel.setLayout(gbl);

      // extent X
      leftPanel.add(Box.createHorizontalStrut(10));
      JLabel extXLabel = new JLabel("extent X:");
      extXLabel.setFont(serif12);
      extXLabel.setForeground(Color.black);
      extXLabel.setRequestFocusEnabled(false);
      gbc.gridwidth = 2;
      gbl.setConstraints(extXLabel, gbc);
      leftPanel.add(extXLabel);
      leftPanel.add(Box.createHorizontalStrut(10));

      extXInput = new JTextField(Integer.toString(extents[0]), 3);
      gbc.gridwidth = GridBagConstraints.REMAINDER;
      gbl.setConstraints(extXInput, gbc);
      extXInput.setEnabled(false);
      leftPanel.add(extXInput);

      // extent Y
      leftPanel.add(Box.createHorizontalStrut(10));
      JLabel extYLabel = new JLabel("extent Y:");
      extYLabel.setFont(serif12);
      extYLabel.setForeground(Color.black);
      extYLabel.setRequestFocusEnabled(false);
      gbc.gridwidth = 2;
      gbl.setConstraints(extYLabel, gbc);
      leftPanel.add(extYLabel);
      leftPanel.add(Box.createHorizontalStrut(10));

      extYInput = new JTextField(Integer.toString(extents[1]), 3);
      gbc.gridwidth = GridBagConstraints.REMAINDER;
      gbl.setConstraints(extYInput, gbc);
      extYInput.setEnabled(false);
      leftPanel.add(extYInput);
      if ( dim >= 3 ) {
        // extent Z
        leftPanel.add(Box.createHorizontalStrut(10));
        JLabel extZLabel = new JLabel("extent Z:");
        extZLabel.setFont(serif12);
        extZLabel.setForeground(Color.black);
        extZLabel.setRequestFocusEnabled(false);
        gbc.gridwidth = 2;
        gbl.setConstraints(extZLabel, gbc);
        leftPanel.add(extZLabel);
        leftPanel.add(Box.createHorizontalStrut(10));

        extZInput = new JTextField(Integer.toString(extents[2]), 3);
        gbc.gridwidth = GridBagConstraints.REMAINDER;
        gbl.setConstraints(extZInput, gbc);
        extZInput.setEnabled(false);
        leftPanel.add(extZInput);
      }
      // make border
     rightPanel.setBorder(buildTitledBorder("Expected Extents"));
     contentBox.add(rightPanel);

     // set layout
     gbl = new GridBagLayout();
     gbc = new GridBagConstraints();
     rightPanel.setLayout(gbl);

     // extent X expected
     rightPanel.add(Box.createHorizontalStrut(10));
     JLabel extXNewLabel = new JLabel("extent X:");
     extXNewLabel.setFont(serif12);
     extXNewLabel.setForeground(Color.black);
     extXNewLabel.setRequestFocusEnabled(false);
     gbc.gridwidth = 2;
     gbl.setConstraints(extXNewLabel, gbc);
     rightPanel.add(extXNewLabel);
     rightPanel.add(Box.createHorizontalStrut(10));

     extXOutput = new JTextField(Integer.toString(volExtents[0]), 3);
     extXOutput.addActionListener(this);
     extXOutput.setActionCommand("xChanged");
     gbc.gridwidth = GridBagConstraints.REMAINDER;
     gbl.setConstraints(extXOutput, gbc);
     MipavUtil.makeNumericsOnly(extXOutput,false);
     rightPanel.add(extXOutput);

     // extent Y expected
     rightPanel.add(Box.createHorizontalStrut(10));
     JLabel extYNewLabel = new JLabel("extent Y:");
     extYNewLabel.setFont(serif12);
     extYNewLabel.setForeground(Color.black);
     extYNewLabel.setRequestFocusEnabled(false);
     gbc.gridwidth = 2;
     gbl.setConstraints(extYNewLabel, gbc);
     rightPanel.add(extYNewLabel);
     rightPanel.add(Box.createHorizontalStrut(10));

     extYOutput = new JTextField(Integer.toString(volExtents[1]), 3);
     extYOutput.addActionListener(this);
     extYOutput.setActionCommand("yChanged");
     gbc.gridwidth = GridBagConstraints.REMAINDER;
     gbl.setConstraints(extYOutput, gbc);
     MipavUtil.makeNumericsOnly(extYOutput,false);
     rightPanel.add(extYOutput);
     if ( dim >= 3 ) {
       // extent Z expected
       rightPanel.add(Box.createHorizontalStrut(10));
       JLabel extZNewLabel = new JLabel("extent Z:");
       extZNewLabel.setFont(serif12);
       extZNewLabel.setForeground(Color.black);
       extZNewLabel.setRequestFocusEnabled(false);
       gbc.gridwidth = 2;
       gbl.setConstraints(extZNewLabel, gbc);
       rightPanel.add(extZNewLabel);
       rightPanel.add(Box.createHorizontalStrut(10));

       extZOutput = new JTextField(Integer.toString(volExtents[2]), 3);
       extZOutput.addActionListener(this);
       extZOutput.setActionCommand("zChanged");
       gbc.gridwidth = GridBagConstraints.REMAINDER;
       gbl.setConstraints(extZOutput, gbc);
       MipavUtil.makeNumericsOnly(extZOutput, false);
       rightPanel.add(extZOutput);
     }
     mainBox.add(contentBox);

     JPanel OKCancelPanel = new JPanel(new FlowLayout());

     // OKButton = buildOKButton();
     OKButton = buildResampleButton();
     OKCancelPanel.add(OKButton);

     cancelButton = buildCancelButton();
     OKCancelPanel.add(cancelButton);
     mainBox.add(OKCancelPanel);

     getContentPane().add(mainBox);

     pack();
     setVisible(true);

    }

    /**
     *  Builds the OK button.  Sets it internally as well
     *  return the just-built button.
     */
    private JButton buildResampleButton() {
      OKButton = new JButton("Resample");
      OKButton.addActionListener(this);
      OKButton.setMinimumSize(MipavUtil.defaultButtonSize);
      OKButton.setPreferredSize(MipavUtil.defaultButtonSize);
      OKButton.setFont(serif12B);
      return OKButton;
    }


    /**
     *   Resample images to power of 2.
     */
    public void doResample() {
        AlgorithmTransform transformImg;
        if ( forceResample ) {
          // resample imageA
          if (dim >= 3) {
            transformImg = new AlgorithmTransform(imageA, new TransMatrix(4),
                                                  AlgorithmTransform.TRILINEAR,
                                                  newRes[0], newRes[1], newRes[2],
                                                  volExtents[0], volExtents[1],
                                                  volExtents[2],
                                                  false, true, false);
          }
          else {
            transformImg = new AlgorithmTransform(imageA, new TransMatrix(4),
                                                  AlgorithmTransform.BILINEAR,
                                                  newRes[0], newRes[1],
                                                  volExtents[0], volExtents[1],
                                                  false, true, false);
          }
          transformImg.setActiveImage(false);
          if (!userInterface.isAppFrameVisible()) {
              transformImg.setProgressBarVisible(false);
          }
          transformImg.run();
          if (transformImg.isCompleted() == false) {
            // What to do
          }

          resampledImage = transformImg.getTransformedImage();
          resampledImage.calcMinMax();

          resampledImageFrame = new ViewJFrameImage(resampledImage, null, new Dimension(200, 200));
          resImageA = resampledImage;
          transformImg.disposeLocal();
          transformImg = null;
        }
        // resample imageB
        if ( imageB != null && forceResample ) {
            //Resample image into volume that is a power of two !
            Preferences.debug(
                "ViewJFrameSurfaceRenderer.buildTexture: Volume resampled.");
            if ( dim >= 3 ) {
                     transformImg = new AlgorithmTransform(imageB, new TransMatrix(4),
                                                    AlgorithmTransform.TRILINEAR,
                                                    newRes[0], newRes[1], newRes[2],
                                                    volExtents[0], volExtents[1],
                                                    volExtents[2],
                                                    false, true, false);
            } else {
                     transformImg = new AlgorithmTransform(imageA, new TransMatrix(4),
                                   //AlgorithmTransform.CUBIC_LAGRANGIAN,
                                   AlgorithmTransform.BILINEAR,
                                   newRes[0], newRes[1],
                                   volExtents[0], volExtents[1],
                                   false, true, false);

            }
            transformImg.setActiveImage(false);
            if (!userInterface.isAppFrameVisible()) {
            transformImg.setProgressBarVisible(false);
            }
            transformImg.run();
            if (transformImg.isCompleted() == false) {
                // What to do
            }

            resampledImage = transformImg.getTransformedImage();
            resampledImage.calcMinMax();

            resImageB = resampledImage;
            resampledImageFrame.setImageB(resImageB);
            transformImg.disposeLocal();
            transformImg = null;
        }

      }

      /**
       * Calculate the dimension value to power of 2.
       * @param dim     dimension value
       * @return  value  dimension value in power of 2.
       */
      private int dimPowerOfTwo(int dim) {
        // 128^3 x 4 is 8MB
        // 256^3 x 4 is 64MB
        if (dim <= 16) {
          return 16;
        }
        else if (dim <= 32) {
          return 32;
        }
        else if (dim <= 64) {
          if (dim > 40) {
            return 64;
          }
          else {
            return 32;
          }
        }
        else if (dim <= 128) {
          if (dim > 80) {
            return 128;
          }
          else {
            return 64;
          }
        }
        else if (dim <= 256) {
          if (dim > 160) {
            return 256;
          }
          else {
            return 128;
          }
        }
        else if (dim <= 512) {
          if (dim > 448) {
            return 512;
          }
          else {
            return 256;
          }
        }
        else
          return 512;
    }

    protected void finalize() throws Throwable {
        dispose(true);
        super.finalize();
    }

    /**
     * Dispose memory.
     * @param flag
     */
    public void dispose(boolean flag) {
        /*
        if (componentImage != null) {
            componentImage.setBuffers(null, null, null, null, null);
            componentImage.setImageA(null);
            componentImage.setImageB(null);
            componentImage.dispose(false);
            componentImage.disposeLocal();
            componentImage = null;
        }

        if (resampledImageFrame != null) {
            resampledImageFrame.dispose();
            resampledImageFrame = null;
        }

        if (resImageA != null) {
            resImageA.disposeLocal();
            resImageA = null;
        }

        if (resImageB != null) {
            resImageB.disposeLocal();
            resImageA = null;
        }

        if (resampledImage != null) {
            resampledImage.disposeLocal();
            resampledImage = null;
        }
        */
        extents = null;
        res = null;
        volExtents = null;
        newRes = null;

        super.dispose();
    }

}


