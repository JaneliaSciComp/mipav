package gov.nih.mipav.view.dialogs;


import gov.nih.mipav.model.algorithms.*;
import gov.nih.mipav.model.structures.*;

import gov.nih.mipav.view.*;

import java.awt.*;
import java.awt.event.*;

import javax.swing.*;


/**
 * DOCUMENT ME!
 */
public class JDialogVSMIP extends JDialogBase implements AlgorithmInterface {

    //~ Static fields/initializers -------------------------------------------------------------------------------------

    /** Use serialVersionUID for interoperability. */
    private static final long serialVersionUID = -229974440831184567L;

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** DOCUMENT ME! */
    ViewJFrameImage accumulatorFrame = null;

    /** DOCUMENT ME! */
    boolean iterate = false;

    /** DOCUMENT ME! */
    ViewJFrameImage mipFrame = null;

    /** DOCUMENT ME! */
    boolean viewMIP = true, viewAccumulator = false;

    /** DOCUMENT ME! */
    private JCheckBox iterateButton;

    /** DOCUMENT ME! */
    private ModelImage mipImage = null;

    /** DOCUMENT ME! */
    private MIPNode[] mipInfo;

    /** DOCUMENT ME! */
    private int mipPlaneWidth, mipPlaneHeight;

    /** DOCUMENT ME! */
    private float[] resultBuffer = null;

    /** DOCUMENT ME! */
    private ModelImage resultImage = null;

    /** DOCUMENT ME! */
    private ModelImage srcImage;

    /** DOCUMENT ME! */
    private float stepSize, angleX, angleY;

    /** DOCUMENT ME! */
    private JTextField textAngleX, textAngleY;

    /** DOCUMENT ME! */
    private JTextField textMipPlaneWidth, textMipPlaneHeight, textMipStepSize;

    /** DOCUMENT ME! */
    private JRadioButton viewMipImage, viewAccumulatorImage;

    /** DOCUMENT ME! */
    private AlgorithmVSMIP vsMIPAlgo = null;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Creates a new JDialogVSMIP object.
     *
     * @param  theParentFrame  DOCUMENT ME!
     * @param  im              DOCUMENT ME!
     */
    public JDialogVSMIP(Frame theParentFrame, ModelImage im) {
        super(theParentFrame, false);

        if (im.getNDims() != 3) {
            MipavUtil.displayError("Source Image must be 3D to generate a MIP");
            dispose();

            return;
        }

        srcImage = im;

        init();
    } // end JDialogVSMIP(...)

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * DOCUMENT ME!
     *
     * @param  event  DOCUMENT ME!
     */
    public void actionPerformed(ActionEvent event) {
        String command = event.getActionCommand();

        if (command.equals("OK")) {

            if (setVariables()) {
                callAlgorithm();
            }
        } else if (command.equals("Cancel")) {
            dispose();
        } else if (command.equals("Help")) {
            MipavUtil.showHelp("10017");
        } // end if(command.equals("OK")-else
    } // end actionPerformed(...)


    /**
     * DOCUMENT ME!
     *
     * @param  algorithm  DOCUMENT ME!
     */
    public void algorithmPerformed(AlgorithmBase algorithm) {

        if (algorithm instanceof AlgorithmVSMIP) {

            if (vsMIPAlgo.isCompleted() == true) {

                if (viewMIP && (mipImage != null)) {

                    try {
                        mipFrame = new ViewJFrameImage(mipImage, null, new Dimension(610, 200));
                    } catch (OutOfMemoryError error) {
                        System.gc();
                        MipavUtil.displayError("Out of memory: unable to open new frame");
                    }
                } // end if(viewMIP)

                if (viewAccumulator && (accumulatorFrame == null)) {

                    try {
                        accumulatorFrame = new ViewJFrameImage(resultImage, null, new Dimension(610, 200));
                    } catch (OutOfMemoryError error) {
                        System.gc();
                        MipavUtil.displayError("Out of memory: unable to open new frame");
                    }
                } else if (viewAccumulator && (accumulatorFrame != null)) {
                    accumulatorFrame.updateImages(true);
                } // end if (accumulatorFrame == null ...)

            } // end if (vsMIPAlgo.isCompleted() ...)
        } // end if (algorithm ...)
    } // end algorithmPerformed(...)


    /**
     * DOCUMENT ME!
     */
    protected void callAlgorithm() {
        String name = makeImageName(srcImage.getImageName(), "_vsMIP");

        try {
            vsMIPAlgo = new AlgorithmVSMIP(resultImage, mipImage, srcImage, mipPlaneWidth, mipPlaneHeight, stepSize,
                                           angleX, angleY, iterate);

            // This is very important. Adding this object as a listener allows the algorithm to
            // notify this object when it has completed or failed. See algorithm performed event.
            // This is made possible by implementing AlgorithmedPerformed interface
            vsMIPAlgo.addListener(this);

            createProgressBar(srcImage.getImageName(), vsMIPAlgo);

            if (isRunInSeparateThread()) {

                if (vsMIPAlgo.startMethod(Thread.MIN_PRIORITY) == false) {
                    MipavUtil.displayError("A thread is already running on this object");
                }
            } else {
                vsMIPAlgo.run();
            } // end if (isRunInSeparateThread())

        } catch (OutOfMemoryError x) {
            MipavUtil.displayError("JDialogVSMIP: unable to allocate enough memory for MIP image");

            if (resultImage != null) {
                resultImage.disposeLocal();
                resultImage = null;
            }

            return;
        } // end try()=catch()

    } // end callAlgorithm()


    /**
     * DOCUMENT ME!
     */
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


    /**
     * DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    private boolean setVariables() {

        // sanity check to insure we can call the MIP algorithm
        if (srcImage.getNDims() != 3) {
            return false;
        }

        if (resultImage == null) {
            resultImage = new ModelImage(ModelStorageBase.FLOAT, srcImage.getExtents(), "VesselVolume");
        }

        // read strings from the text fields and convert to an integers
        String tmpStr = textMipPlaneWidth.getText();
        mipPlaneWidth = Integer.valueOf(tmpStr).intValue();

        tmpStr = textMipPlaneHeight.getText();
        mipPlaneHeight = Integer.valueOf(tmpStr).intValue();

        mipInfo = new MIPNode[mipPlaneWidth * mipPlaneHeight];

        for (int i = 0; i < mipInfo.length; i++) {
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
        mipImage = new ModelImage(ModelStorageBase.FLOAT, mipDims, "MIP");

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


    /*
     * public void algorithmPerformed(AlgorithmBase algorithm) { if (algorithm instanceof AlgorithmVSMIP) {   if
     * (vsMIPAlgo.isCompleted() == true) {
     *
     * if (viewMIP) {       float[] mipBuffer;       int mipLength;       try {         mipLength =
     * mipImage.getSliceSize();         mipBuffer = new float[mipLength];         mipImage.exportData(0, mipLength,
     * mipBuffer);       } catch (IOException error) {         mipBuffer = null;
     * MipavUtil.displayError("AlgorithmVSMIP::run()  could NOT export source image");         return;       } catch
     * (OutOfMemoryError e){         mipBuffer = null;         MipavUtil.displayError("AlgorithmVSMIP::run()  Out of
     * memory when creating image buffer");         return;       } // end try{}-catch{}-catch{}
     *
     *  // make the mip image       for(int i = 0; i < mipLength; i++) {         mipBuffer[i] = mipInfo[i].intensity;
     *   }
     *
     *
     *  try { mipImage.importData(0, mipBuffer, true);  }       catch (IOException error) {         mipBuffer = null;
     *     MipavUtil.displayError("AlgorithmVSMIP::run()  Could NOT import destBuffer to the image"); return;       } //
     * end try{}-catch{}
     *
     *
     *  try {         mipFrame = new ViewJFrameImage(mipImage, null,                                      new
     * Dimension(610, 200),                                        userInterface);       } catch (OutOfMemoryError
     * error) {         System.gc();         MipavUtil.displayError("Out of memory: unable to open new frame");       }
     *  } // end if(viewMIP)     if (accumulatorFrame == null && viewAccumulator) {       try { accumulatorFrame = new
     * ViewJFrameImage(resultImage, null,                                         new Dimension(610, 200),
     *                           userInterface);       } catch (OutOfMemoryError error) {         System.gc();
     * MipavUtil.displayError("Out of memory: unable to open new frame");       }   } else if (accumulatorFrame != null)
     * {       accumulatorFrame.updateImages(true);     }   } // end if (vsMIPAlgo.isCompleted() ...) } // end if
     * (algorithm ...) } // end algorithmPerformed(...)
     */

    /*
     * public void algorithmPerformed(AlgorithmBase algorithm) { ViewJFrameImage imageFrame = null; if (algorithm
     * instanceof AlgorithmVSMIP) {     image.clearMask();     if (vsMIPAlgo.isCompleted() == true && resultImage !=
     * null) {       //The algorithm has completed and produced a new image to be displayed.
     *
     *  // updateFileInfo fails if the resultImage has a different number of dimensions than image       if
     * (image.getNDims() == resultImage.getNDims()) {         updateFileInfo(image, resultImage);       }
     * resultImage.clearMask();       try {         //resultImage.setImageName("Median: "+image.getImageName());
     * imageFrame = new ViewJFrameImage(resultImage, null,                                         new Dimension(610,
     * 200),                                         userInterface);       }       catch (OutOfMemoryError error) {
     * System.gc();         MipavUtil.displayError("Out of memory: unable to open new frame");       }     } // end if
     * (vsMIPAlgo.isCompleted() ...)   } // end if (algorithm ...) } // end algorithmPerformed(...)
     */


} // end class JDialogVSMIP
