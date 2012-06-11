package gov.nih.mipav.view.dialogs;


import gov.nih.mipav.model.algorithms.*;
import gov.nih.mipav.model.structures.*;

import gov.nih.mipav.view.*;

import java.awt.*;
import java.awt.event.*;

import java.util.*;

import javax.swing.*;


/**
 * Dialog to get user input, then call the snake-like algorithm.
 *
 * @see  AlgorithmAGVF
 */
public class JDialogAGVF extends JDialogBase implements AlgorithmInterface, ItemListener {

    //~ Static fields/initializers -------------------------------------------------------------------------------------

    /** Use serialVersionUID for interoperability. */
    private static final long serialVersionUID = -3328779751424483654L;

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** DOCUMENT ME! */
    private AlgorithmAGVF agvfAlgo;

    /** DOCUMENT ME! */
    private int boundaryIterations;

    /** DOCUMENT ME! */
    private JCheckBox checkboxDisplay;

    /** default dilation:. */
    private float dilation = 2.0f;

    /** DOCUMENT ME! */
    private boolean do25D = true;

    /** DOCUMENT ME! */
    private JCheckBox do25DCheckBox;

    /** Cell-Tracking addition:. */
    /** When true, do a Cell-tracking version of Active GVF Contours:. */
    private boolean doCellTracking = false;

    /** Checkbox to turn cell-tracking on/off:. */
    private JCheckBox doCellTrackingCheckBox;

    /** When true, dilate the cell radius before finding the next contour:. */
    private boolean doDilate = false;

    /** Checkbox to turn cell-tracking on/off:. */
    private JCheckBox doDilateCheckBox;

    /** DOCUMENT ME! */
    private int groupNum;

    /** DOCUMENT ME! */
    private int gvfIterations;

    /** DOCUMENT ME! */
    private ModelImage image; // source image

    /** DOCUMENT ME! */
    private float kValue;

    /** user-interface for setting the cell-radius dilation (multiple of cell expected radius). */
    private JLabel labelDilation;

    /** DOCUMENT ME! */
    private JLabel labelGaussZ;

    /** user-interface for setting the estimated cell-radius (pixels). */
    private JLabel labelRadius;

    /** user-interface for setting the uniform-resampling constraint contribution:. */
    private JLabel labelResampling;

    /** user-interface for setting the shape constraint contribution:. */
    private JLabel labelShape;

    /** user-interface for setting the size constraint contribution:. */
    private JLabel labelSize;

    /** user-interface for setting the initial cell velocity:. */
    private JLabel labelVelocity;

    /** DOCUMENT ME! */
    private JRadioButton propagate;

    /** DOCUMENT ME! */
    private boolean propagationFlag;

    /** Default values for cell-tracking:. */
    /** Default cell radius (pixels). */
    private float radiusConstraint = 100;

    /** DOCUMENT ME! */
    private boolean removeOriginal;

    /** DOCUMENT ME! */
    private JCheckBox removeOriginalCheckBox;

    /** default sampling constraint contribution:. */
    private float resamplingConstraint = 0.05f;

    /** DOCUMENT ME! */
    private ModelImage resultImage; // magnitude of GVF

    /** DOCUMENT ME! */
    private float scaleX;

    /** DOCUMENT ME! */
    private float scaleY;

    /** DOCUMENT ME! */
    private float scaleZ = 1.0f;

    /** default shape constraint contribution:. */
    private float shapeConstraint = 0.05f;

    /** DOCUMENT ME! */
    private float[] sigmas;

    /** DOCUMENT ME! */
    private JRadioButton singleSlice;

    /** default size constraint contribution:. */
    private float sizeConstraint = 0.05f;

    /** DOCUMENT ME! */
    private float smoothness;

    /** DOCUMENT ME! */
    private VOI srcVOI;

    /** DOCUMENT ME! */
    private JTextField textBoundaryIterations;

    /** DOCUMENT ME! */
    private JTextField textDilation;

    /** DOCUMENT ME! */
    private JTextField textDx;

    /** DOCUMENT ME! */
    private JTextField textDy;

    /** DOCUMENT ME! */
    private JTextField textGaussX;

    /** DOCUMENT ME! */
    private JTextField textGaussY;

    /** DOCUMENT ME! */
    private JTextField textGaussZ;

    /** DOCUMENT ME! */
    private JTextField textGVFIterations;

    /** DOCUMENT ME! */
    private JTextField textK;

    /** DOCUMENT ME! */
    private JTextField textRadius;

    /** DOCUMENT ME! */
    private JTextField textResampling;

    /** DOCUMENT ME! */
    private JTextField textShape;

    /** DOCUMENT ME! */
    private JTextField textSize;

    /** DOCUMENT ME! */
    private JTextField textSmoothness;

    /** DOCUMENT ME! */
    private String[] titles;

    /** default initial velocity (dx):. */
    private float velocityDx = 0;

    /** default initial velocity (dx):. */
    private float velocityDy = 0;

    /** DOCUMENT ME! */
    private Color voiColor;

    /** DOCUMENT ME! */
    private ViewVOIVector VOIs = null;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Creates new dialog for finding the GVF.
     *
     * @param  theParentFrame  Parent frame
     * @param  im              Source image
     */
    public JDialogAGVF(Frame theParentFrame, ModelImage im) {
        super(theParentFrame, false);

        VOIs = im.getVOIs();

        int nVOI;

        nVOI = VOIs.size();

        if (nVOI == 0) {
            MipavUtil.displayError("There are no VOIs to operate on.");
            dispose();

            return;
        }

        for (groupNum = 0; groupNum < nVOI; groupNum++) {

            if ((VOIs.VOIAt(groupNum).isActive() == true) && (VOIs.VOIAt(groupNum).getCurveType() == VOI.CONTOUR)) {
                break;
            }
        }

        if (groupNum == nVOI) {
            MipavUtil.displayError("VOI must be selected");
            dispose();

            return;
        }

        voiColor = VOIs.VOIAt(groupNum).getColor();
        srcVOI = VOIs.VOIAt(groupNum);
        image = im;
        init();
    }

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * When the OK button is pressed, sets variables and calls the algorithm. When the cancel button is pressed, closes
     * the dialog.
     *
     * @param  event  Event that triggers function
     */
    public void actionPerformed(ActionEvent event) {
        Object source = event.getSource();
        ;
        propagationFlag = false;

        String tmpStr;
        int i;

        if ((source == propagate) || (source == singleSlice)) {

            if (singleSlice.isSelected()) {
                do25DCheckBox.setEnabled(false);
                do25DCheckBox.setSelected(true);
                labelGaussZ.setEnabled(false);
                textGaussZ.setEnabled(false);
            } else { // propagate.isSelected()
                do25DCheckBox.setEnabled(true);
            }
        } // if ((source == propagate) || (source == singleSlice))
        else if (source == do25DCheckBox) {

            if (do25DCheckBox.isSelected()) {
                labelGaussZ.setEnabled(false);
                textGaussZ.setEnabled(false);
            } else {
                labelGaussZ.setEnabled(true);
                textGaussZ.setEnabled(true);
            }
        } // else if (source == do25DCheckBox)
        else if (source == doCellTrackingCheckBox) {

            /*  Turn cell-tracking interface on or off:  */
            if (doCellTrackingCheckBox.isSelected()) {
                labelRadius.setEnabled(true);
                textRadius.setEnabled(true);
                labelSize.setEnabled(true);
                textSize.setEnabled(true);
                labelShape.setEnabled(true);
                textShape.setEnabled(true);
                labelResampling.setEnabled(true);
                textResampling.setEnabled(true);
                doDilateCheckBox.setEnabled(true);
                labelVelocity.setEnabled(true);
                textDx.setEnabled(true);
                textDy.setEnabled(true);
            } else {
                labelRadius.setEnabled(false);
                textRadius.setEnabled(false);
                labelSize.setEnabled(false);
                textSize.setEnabled(false);
                labelShape.setEnabled(false);
                textShape.setEnabled(false);
                labelResampling.setEnabled(false);
                textResampling.setEnabled(false);
                doDilateCheckBox.setEnabled(false);
                labelVelocity.setEnabled(false);
                textDx.setEnabled(false);
                textDy.setEnabled(false);
            }
        } // else if (source == do25DCheckBox)
        else if (source == doDilateCheckBox) {

            if (doDilateCheckBox.isSelected()) {
                labelDilation.setEnabled(true);
                textDilation.setEnabled(true);
            } else {
                labelDilation.setEnabled(false);
                textDilation.setEnabled(false);
            }
        } else if (source == OKButton) {

            if (singleSlice.isSelected()) {
                propagationFlag = false;
            } else if (propagate.isSelected()) {
                propagationFlag = true;
            }

            removeOriginal = removeOriginalCheckBox.isSelected();

            do25D = do25DCheckBox.isSelected();

            tmpStr = textGaussX.getText();

            if (testParameter(tmpStr, 0.0, 5.0)) {
                scaleX = Float.valueOf(tmpStr).floatValue();
            } else {
                textGaussX.requestFocus();
                textGaussX.selectAll();

                return;
            }

            tmpStr = textGaussY.getText();

            if (testParameter(tmpStr, 0.0, 5.0)) {
                scaleY = Float.valueOf(tmpStr).floatValue();
            } else {
                textGaussY.requestFocus();
                textGaussY.selectAll();

                return;
            }

            if (!do25D) {
                tmpStr = textGaussZ.getText();

                if (testParameter(tmpStr, 0.0, 5.0)) {
                    scaleZ = Float.valueOf(tmpStr).floatValue();
                } else {
                    textGaussZ.requestFocus();
                    textGaussZ.selectAll();

                    return;
                }
            } // if (!do25D)

            tmpStr = textGVFIterations.getText();

            if (testParameter(tmpStr, 1.0, 1000000.0)) {
                gvfIterations = Integer.valueOf(tmpStr).intValue();
            } else {
                textGVFIterations.requestFocus();
                textGVFIterations.selectAll();

                return;
            }

            tmpStr = textBoundaryIterations.getText();

            if (testParameter(tmpStr, 1.0, 1000000.0)) {
                boundaryIterations = Integer.valueOf(tmpStr).intValue();
            } else {
                textBoundaryIterations.requestFocus();
                textBoundaryIterations.selectAll();

                return;
            }

            tmpStr = textK.getText();

            if (testParameter(tmpStr, 0.01, 0.5)) {
                kValue = Float.valueOf(tmpStr).floatValue();
            } else {
                textK.requestFocus();
                textK.selectAll();

                return;
            }

            tmpStr = textSmoothness.getText();

            if (testParameter(tmpStr, 0.5, 2.4)) {
                smoothness = Float.valueOf(tmpStr).floatValue();
            } else {
                textSmoothness.requestFocus();
                textSmoothness.selectAll();

                return;
            }

            /* Check if cell-tracking is enabled, if so then set the
             * variables: */
            doCellTracking = doCellTrackingCheckBox.isSelected();
            doDilate = doDilateCheckBox.isSelected();

            if (doCellTracking) {
                tmpStr = textRadius.getText();
                radiusConstraint = Float.valueOf(tmpStr).floatValue();
                tmpStr = textShape.getText();
                shapeConstraint = Float.valueOf(tmpStr).floatValue();
                tmpStr = textSize.getText();
                sizeConstraint = Float.valueOf(tmpStr).floatValue();
                tmpStr = textResampling.getText();
                resamplingConstraint = Float.valueOf(tmpStr).floatValue();
                tmpStr = textDilation.getText();
                dilation = Float.valueOf(tmpStr).floatValue();
                tmpStr = textDx.getText();
                velocityDx = Float.valueOf(tmpStr).floatValue();
                tmpStr = textDy.getText();
                velocityDy = Float.valueOf(tmpStr).floatValue();
            }

            if (image.getNDims() == 2) { // source image is 2D

                sigmas = new float[2];
                sigmas[0] = scaleX; // set standard deviations (sigma) in X and Y
                sigmas[1] = scaleY;

                try {

                    if (checkboxDisplay.isSelected() == true) {
                        resultImage = new ModelImage(ModelStorageBase.FLOAT, image.getExtents(),
                                                     image.getImageName() + "_gvf");

                        // Make the algorithm class
                        if (!doCellTracking) {
                            agvfAlgo = new AlgorithmAGVF(resultImage, image, sigmas, gvfIterations, boundaryIterations,
                                                         kValue, smoothness, srcVOI, do25D);
                        } else {
                            agvfAlgo = new AlgorithmCellTrackingAGVF(resultImage, image, sigmas, gvfIterations,
                                                                     boundaryIterations, kValue, smoothness, srcVOI,
                                                                     do25D, radiusConstraint, shapeConstraint,
                                                                     sizeConstraint, resamplingConstraint, doDilate,
                                                                     dilation, velocityDx, velocityDy);
                        }
                    } else {

                        // Make the algorithm class
                        if (!doCellTracking) {
                            agvfAlgo = new AlgorithmAGVF(null, image, sigmas, gvfIterations, boundaryIterations, kValue,
                                                         smoothness, srcVOI, do25D);
                        } else {
                            agvfAlgo = new AlgorithmCellTrackingAGVF(null, image, sigmas, gvfIterations,
                                                                     boundaryIterations, kValue, smoothness, srcVOI,
                                                                     do25D, radiusConstraint, shapeConstraint,
                                                                     sizeConstraint, resamplingConstraint, doDilate,
                                                                     dilation, velocityDx, velocityDy);
                        }

                    }

                    // This is very important. Adding this object as a listener allows the algorithm to
                    // notify this object when it has completed of failed. See algorithm performed event.
                    // This is made possible by implementing AlgorithmedPerformed interface
                    agvfAlgo.addListener(this);
                    createProgressBar(image.getImageName(), agvfAlgo);

                    // Hide the dialog since the algorithm is about to run.
                    setVisible(false);

                    // These next lines set the titles in all frames where the source image is displayed to
                    // "locked - " image name so as to indicate that the image is now read/write locked!
                    // The image frames are disabled and then unregisted from the userinterface until the
                    // algorithm has completed.
                    Vector<ViewImageUpdateInterface> imageFrames = image.getImageFrameVector();
                    titles = new String[imageFrames.size()];

                    for (i = 0; i < imageFrames.size(); i++) {
                        titles[i] = ((ViewJFrameBase) (imageFrames.elementAt(i))).getTitle();
                        ((ViewJFrameBase) (imageFrames.elementAt(i))).setTitle("Locked: " + titles[i]);
                        ((ViewJFrameBase) (imageFrames.elementAt(i))).setEnabled(false);
                        ((ViewJFrameBase) parentFrame).getUserInterface().unregisterFrame((Frame)
                                                                                          (imageFrames.elementAt(i)));
                    }

                    if (isRunInSeparateThread()) {

                        // Start the thread as a low priority because we wish to still have user interface.
                        if (agvfAlgo.startMethod(Thread.MIN_PRIORITY) == false) {
                            MipavUtil.displayError("A thread is already running on this object");
                        }
                    } else {

                        agvfAlgo.run();
                    }
                } catch (OutOfMemoryError x) {

                    if (resultImage != null) {
                        resultImage.disposeLocal();
                        resultImage = null;
                    }

                    MipavUtil.displayError("Dialog GVF: unable to allocate enough memory");

                    return;
                }
            } else if (image.getNDims() == 3) {
                int[] extents = null;

                if (do25D) {
                    sigmas = new float[2];
                    sigmas[0] = scaleX;
                    sigmas[1] = scaleY;
                } else {
                    sigmas = new float[3];
                    sigmas[0] = scaleX;
                    sigmas[1] = scaleY;
                    sigmas[2] = scaleZ;
                }

                ///if (propagationFlag == false) {
                // extents = new int[2];
                // }
                // else {
                extents = new int[3];

                extents[0] = image.getExtents()[0];
                extents[1] = image.getExtents()[1];
                extents[2] = image.getExtents()[2];

                try {

                    if (checkboxDisplay.isSelected() == true) {
                        resultImage = new ModelImage(ModelStorageBase.FLOAT, extents, image.getImageName() + "_gvf");

                        // Make algorithm
                        if (!doCellTracking) {
                            agvfAlgo = new AlgorithmAGVF(resultImage, image, sigmas, gvfIterations, boundaryIterations,
                                                         kValue, smoothness, srcVOI, do25D);

                        } else {
                            agvfAlgo = new AlgorithmCellTrackingAGVF(resultImage, image, sigmas, gvfIterations,
                                                                     boundaryIterations, kValue, smoothness, srcVOI,
                                                                     do25D, radiusConstraint, shapeConstraint,
                                                                     sizeConstraint, resamplingConstraint, doDilate,
                                                                     dilation, velocityDx, velocityDy);

                        }
                    } else {

                        // Make algorithm
                        if (!doCellTracking) {
                            agvfAlgo = new AlgorithmAGVF(null, image, sigmas, gvfIterations, boundaryIterations, kValue,
                                                         smoothness, srcVOI, do25D);
                        } else {
                            agvfAlgo = new AlgorithmCellTrackingAGVF(null, image, sigmas, gvfIterations,
                                                                     boundaryIterations, kValue, smoothness, srcVOI,
                                                                     do25D, radiusConstraint, shapeConstraint,
                                                                     sizeConstraint, resamplingConstraint, doDilate,
                                                                     dilation, velocityDx, velocityDy);
                        }
                    }

                    agvfAlgo.setPropagation(propagationFlag);

                    // This is very important. Adding this object as a listener allows the algorithm to
                    // notify this object when it has completed of failed. See algorithm performed event.
                    // This is made possible by implementing AlgorithmedPerformed interface
                    agvfAlgo.addListener(this);
                    createProgressBar(image.getImageName(), agvfAlgo);

                    // Hide dialog
                    setVisible(false);

                    // These next lines set the titles in all frames where the source image is displayed to
                    // "locked - " image name so as to indicate that the image is now read/write locked!
                    // The image frames are disabled and then unregisted from the userinterface until the
                    // algorithm has completed.
                    Vector<ViewImageUpdateInterface> imageFrames = image.getImageFrameVector();
                    titles = new String[imageFrames.size()];

                    for (i = 0; i < imageFrames.size(); i++) {
                        titles[i] = ((ViewJFrameBase) (imageFrames.elementAt(i))).getTitle();
                        ((ViewJFrameBase) (imageFrames.elementAt(i))).setTitle("Locked: " + titles[i]);
                        ((ViewJFrameBase) (imageFrames.elementAt(i))).setEnabled(false);
                        ((ViewJFrameBase) parentFrame).getUserInterface().unregisterFrame((Frame)
                                                                                          (imageFrames.elementAt(i)));
                    }

                    if (isRunInSeparateThread()) {

                        // Start the thread as a low priority because we wish to still have user interface work fast
                        if (agvfAlgo.startMethod(Thread.MIN_PRIORITY) == false) {
                            MipavUtil.displayError("A thread is already running on this object");
                        }
                    } else {

                        agvfAlgo.run();
                    }
                } catch (OutOfMemoryError x) {
                    MipavUtil.displayError("Dialog AGVF: unable to allocate enough memory");

                    return;
                }
            }
        } else if (source == cancelButton) {
            dispose();
        }
        else if (source == helpButton) {
            MipavUtil.showHelp("10506");
            
        }
    }

    // ************************************************************************
    // ************************** Algorithm Events ****************************
    // ************************************************************************

    /**
     * This method is required if the AlgorithmPerformed interface is implemented. It is called by the algorithm when it
     * has completed or failed to to complete, so that the dialog can be display the result image and/or clean up.
     *
     * @param  algorithm  Algorithm that caused the event.
     */
    public void algorithmPerformed(AlgorithmBase algorithm) {
        VOI resultVOI;

        if (algorithm instanceof AlgorithmAGVF) {

            if (agvfAlgo.isCompleted() == true) {

                // The algorithm has completed and produced a new image to be displayed.
                resultVOI = agvfAlgo.getResultVOI();

                if (removeOriginal) {
                    resultVOI.setColor(voiColor);
                    image.getVOIs().removeElementAt(groupNum);
                }

                image.registerVOI(resultVOI);

                if (resultImage != null) {
                    updateFileInfo(image, resultImage);
                    resultImage.clearMask();

                    try {
                        new ViewJFrameImage(resultImage, null, new Dimension(610, 200));
                    } catch (OutOfMemoryError error) {
                        System.gc();
                        JOptionPane.showMessageDialog(null, "Out of memory: unable to open new frame", "Error",
                                                      JOptionPane.ERROR_MESSAGE);
                    }
                } // if (resultImage != null)

                // Update frame
                ((ViewJFrameBase) parentFrame).updateImages(true);
                if ( voiManager != null )
                {
                    voiManager.algorithmPerformed();
                }
            } // if (agvfAlgo.isCompleted() == true)
            else if ((agvfAlgo.isCompleted() == false) && (resultImage != null)) {
                resultImage.disposeLocal();
                resultImage = null;
                System.gc();
            }

            // These next lines set the titles in all frames where the source image is displayed to
            // image name so as to indicate that the image is now unlocked!
            // The image frames are enabled and then registed to the userinterface.
            Vector<ViewImageUpdateInterface> imageFrames = image.getImageFrameVector();

            for (int i = 0; i < imageFrames.size(); i++) {
                ((ViewJFrameBase) (imageFrames.elementAt(i))).setTitle(titles[i]);
                ((ViewJFrameBase) (imageFrames.elementAt(i))).setEnabled(true);
                ((ViewJFrameBase) parentFrame).getUserInterface().registerFrame((Frame) (imageFrames.elementAt(i)));
            }
        }
        dispose();
    }

    /**
     * Initializes GUI variables and displays dialog.
     */
    private void init() {
        setForeground(Color.black);
        setTitle("Evolve Boundary");
        JPanel scalePanel = new JPanel(new GridLayout(3, 2));
        scalePanel.setForeground(Color.black);
        scalePanel.setBorder(buildTitledBorder("Scale of the Gaussian"));

        JLabel labelGaussX = new JLabel(" X Dimension (0.0 - 5.0) ");
        labelGaussX.setForeground(Color.black);
        labelGaussX.setFont(serif12);
        scalePanel.add(labelGaussX);

        textGaussX = new JTextField(5);
        textGaussX.setText("1.0");
        textGaussX.setFont(serif12);
        scalePanel.add(textGaussX);

        JLabel labelGaussY = new JLabel(" Y Dimension (0.0 - 5.0) ");
        labelGaussY.setForeground(Color.black);
        labelGaussY.setFont(serif12);
        scalePanel.add(labelGaussY);

        textGaussY = new JTextField(5);
        textGaussY.setText("1.0");
        textGaussY.setFont(serif12);
        scalePanel.add(textGaussY);

        labelGaussZ = new JLabel(" Z Dimension (0.0 - 5.0) ");
        labelGaussZ.setForeground(Color.black);
        labelGaussZ.setFont(serif12);
        labelGaussZ.setEnabled(false);
        scalePanel.add(labelGaussZ);

        textGaussZ = new JTextField(5);
        textGaussZ.setText("1.0");
        textGaussZ.setFont(serif12);
        textGaussZ.setEnabled(false);
        scalePanel.add(textGaussZ);

        JPanel imageVOIPanel = new JPanel(new GridLayout(3, 1));
        imageVOIPanel.setForeground(Color.black);
        imageVOIPanel.setBorder(buildTitledBorder("Evolve Boundary"));

        ButtonGroup imageVOIGroup = new ButtonGroup();
        singleSlice = new JRadioButton(" Single slice ", true);
        singleSlice.setFont(serif12);
        singleSlice.addActionListener(this);
        imageVOIGroup.add(singleSlice);

        propagate = new JRadioButton(" Propagate to adjacent slices ", false);
        propagate.setFont(serif12);
        propagate.addActionListener(this);
        imageVOIGroup.add(propagate);

        if (image.getNDims() == 2) {
            propagate.setEnabled(false);
        }

        removeOriginalCheckBox = new JCheckBox("Replace Original Contour");
        removeOriginalCheckBox.setFont(serif12);
        removeOriginalCheckBox.setForeground(Color.black);
        removeOriginalCheckBox.setSelected(false);

        imageVOIPanel.add(singleSlice);
        imageVOIPanel.add(propagate);
        imageVOIPanel.add(removeOriginalCheckBox);

        JPanel iterationsPanel = new JPanel(new GridLayout(3, 2));
        iterationsPanel.setForeground(Color.black);
        iterationsPanel.setBorder(buildTitledBorder("Maximum Iterations"));

        JLabel labelGVFIterations = new JLabel(" GVF field iterations ");
        labelGVFIterations.setForeground(Color.black);
        labelGVFIterations.setFont(serif12);

        textGVFIterations = new JTextField(5);
        textGVFIterations.setText("200");
        textGVFIterations.setFont(serif12);

        JLabel labelBoundaryIterations = new JLabel(" Evolve iterations ");
        labelBoundaryIterations.setForeground(Color.black);
        labelBoundaryIterations.setFont(serif12);

        textBoundaryIterations = new JTextField(5);
        textBoundaryIterations.setText("300");
        textBoundaryIterations.setFont(serif12);

        checkboxDisplay = new JCheckBox("Display GVF image");
        checkboxDisplay.setFont(serif12);
        checkboxDisplay.setForeground(Color.black);

        GridBagConstraints gbc = new GridBagConstraints();
        gbc.weightx = 1;
        gbc.anchor = GridBagConstraints.WEST;
        gbc.fill = GridBagConstraints.HORIZONTAL;
        iterationsPanel.add(labelGVFIterations);
        iterationsPanel.add(textGVFIterations);
        iterationsPanel.add(labelBoundaryIterations);
        iterationsPanel.add(textBoundaryIterations);
        iterationsPanel.add(checkboxDisplay);

        GridBagConstraints gbc2 = new GridBagConstraints();
        gbc2.weightx = 1;
        gbc2.anchor = GridBagConstraints.WEST;
        gbc2.fill = GridBagConstraints.HORIZONTAL;

        JPanel parametersPanel = new JPanel(new GridBagLayout());
        parametersPanel.setForeground(Color.black);
        parametersPanel.setBorder(buildTitledBorder("Parameters"));

        JLabel labelK = new JLabel(" GVF k (0.01-0.5) ");
        labelK.setForeground(Color.black);
        labelK.setFont(serif12);
        gbc2.gridx = 0;
        gbc2.gridy = 0;
        parametersPanel.add(labelK, gbc2);

        textK = new JTextField(5);
        textK.setText("0.15");
        textK.setFont(serif12);
        gbc2.gridx = 1;
        gbc2.gridy = 0;
        parametersPanel.add(textK, gbc2);

        JLabel labelSmoothness = new JLabel(" Smoothness (0.5 - 2.4) ");
        labelSmoothness.setForeground(Color.black);
        labelSmoothness.setFont(serif12);
        gbc2.gridx = 0;
        gbc2.gridy = 1;
        parametersPanel.add(labelSmoothness, gbc2);

        textSmoothness = new JTextField(5);
        textSmoothness.setText("1.7");
        textSmoothness.setFont(serif12);
        gbc2.gridx = 1;
        gbc2.gridy = 1;
        parametersPanel.add(textSmoothness, gbc2);

        do25DCheckBox = new JCheckBox(" Slice by slice processing");
        do25DCheckBox.setFont(serif12);
        do25DCheckBox.setForeground(Color.black);
        do25DCheckBox.setSelected(true);
        do25DCheckBox.setEnabled(false);
        do25DCheckBox.addActionListener(this);
        gbc2.gridx = 0;
        gbc2.gridy = 2;
        parametersPanel.add(do25DCheckBox, gbc2);

        JPanel cellTrackingPanel = new JPanel(new GridBagLayout());
        cellTrackingPanel.setForeground(Color.black);
        cellTrackingPanel.setBorder(buildTitledBorder("Cell Tracking"));
        gbc2.gridx = 0;
        gbc2.gridy = 0;

        /* Setup the cell-tracking user-interface: */
        doCellTrackingCheckBox = new JCheckBox(" Cell Tracking");
        doCellTrackingCheckBox.setFont(serif12);
        doCellTrackingCheckBox.setForeground(Color.black);
        doCellTrackingCheckBox.setSelected(false);
        doCellTrackingCheckBox.setEnabled(true);
        doCellTrackingCheckBox.addActionListener(this);
        cellTrackingPanel.add(doCellTrackingCheckBox, gbc2);

        gbc2.gridx = 0;
        gbc2.gridy++;
        labelRadius = new JLabel("Expected Cell Radius (pixels)");
        labelRadius.setEnabled(false);
        cellTrackingPanel.add(labelRadius, gbc2);
        gbc2.gridx = 1;

        float[] xBounds = new float[2], yBounds = new float[2], zBounds = new float[2];
        srcVOI.getBounds(xBounds, yBounds, zBounds);
        radiusConstraint = Math.max(xBounds[1] - xBounds[0], yBounds[1] - yBounds[0]);
        radiusConstraint /= 2.0f;
        textRadius = new JTextField(new String("" + radiusConstraint), 5);
        textRadius.setFont(serif12);
        textRadius.setEnabled(false);
        cellTrackingPanel.add(textRadius, gbc2);

        gbc2.gridx = 0;
        gbc2.gridy++;
        labelSize = new JLabel("Size Constraint factor (0-1):");
        labelSize.setEnabled(false);
        cellTrackingPanel.add(labelSize, gbc2);
        gbc2.gridx = 1;
        textSize = new JTextField(new String("" + sizeConstraint), 5);
        textSize.setFont(serif12);
        textSize.setEnabled(false);
        cellTrackingPanel.add(textSize, gbc2);

        gbc2.gridx = 0;
        gbc2.gridy++;
        labelShape = new JLabel("Shape Constraint factor (0-1):");
        labelShape.setEnabled(false);
        cellTrackingPanel.add(labelShape, gbc2);
        gbc2.gridx = 1;
        textShape = new JTextField(new String("" + shapeConstraint), 5);
        textShape.setEnabled(false);
        textShape.setFont(serif12);
        cellTrackingPanel.add(textShape, gbc2);

        gbc2.gridx = 0;
        gbc2.gridy++;
        labelResampling = new JLabel("Implicit Resampling Constraint factor (0-1):");
        labelResampling.setEnabled(false);
        cellTrackingPanel.add(labelResampling, gbc2);
        gbc2.gridx = 1;
        textResampling = new JTextField(new String("" + resamplingConstraint), 5);
        textResampling.setFont(serif12);
        textResampling.setEnabled(false);
        cellTrackingPanel.add(textResampling, gbc2);

        /* Setup the cell-dilation user-interface: */
        gbc2.gridx = 0;
        gbc2.gridy++;
        doDilateCheckBox = new JCheckBox(" Cell Dilation");
        doDilateCheckBox.setFont(serif12);
        doDilateCheckBox.setForeground(Color.black);
        doDilateCheckBox.setSelected(false);
        doDilateCheckBox.setEnabled(false);
        doDilateCheckBox.addActionListener(this);
        cellTrackingPanel.add(doDilateCheckBox, gbc2);

        gbc2.gridx = 0;
        gbc2.gridy++;
        labelDilation = new JLabel("Cell dilation factor:");
        labelDilation.setEnabled(false);
        cellTrackingPanel.add(labelDilation, gbc2);
        gbc2.gridx = 1;
        textDilation = new JTextField(new String("" + dilation), 5);
        textDilation.setFont(serif12);
        textDilation.setEnabled(false);
        cellTrackingPanel.add(textDilation, gbc2);

        gbc2.gridx = 0;
        gbc2.gridy++;
        labelVelocity = new JLabel("Initial Cell Velocity in pixels (X,Y) :");
        labelVelocity.setEnabled(false);
        cellTrackingPanel.add(labelVelocity, gbc2);
        gbc2.gridx = 1;
        textDx = new JTextField(new String("" + velocityDx), 5);
        textDx.setFont(serif12);
        textDx.setEnabled(false);
        cellTrackingPanel.add(textDx, gbc2);
        gbc2.gridx = 2;
        textDy = new JTextField(new String("" + velocityDy), 5);
        textDy.setFont(serif12);
        textDy.setEnabled(false);
        cellTrackingPanel.add(textDy, gbc2);

        JPanel mainPanel = new JPanel(new GridBagLayout());
        gbc.gridx = 0;
        gbc.gridy = 0;
        mainPanel.add(scalePanel, gbc);
        gbc.gridy = 1;
        mainPanel.add(imageVOIPanel, gbc);
        gbc.gridy = 2;
        mainPanel.add(iterationsPanel, gbc);
        gbc.gridy = 3;
        mainPanel.add(parametersPanel, gbc);
        gbc.gridy = 4;
        mainPanel.add(cellTrackingPanel, gbc);
        mainPanel.setBorder(BorderFactory.createEmptyBorder(5, 5, 5, 5));

        JPanel buttonPanel = new JPanel();
        buildOKButton();
        buttonPanel.add(OKButton);
        buildCancelButton();
        buttonPanel.add(cancelButton);
        buildHelpButton();
        buttonPanel.add(helpButton);
        
        mainDialogPanel.add(mainPanel);
        mainDialogPanel.add(buttonPanel, BorderLayout.SOUTH);

        getContentPane().add(mainDialogPanel);

        pack();
        setVisible(true);
    }
}
