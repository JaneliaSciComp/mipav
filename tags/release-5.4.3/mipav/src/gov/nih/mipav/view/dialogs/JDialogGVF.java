package gov.nih.mipav.view.dialogs;


import gov.nih.mipav.model.algorithms.*;
import gov.nih.mipav.model.structures.*;

import gov.nih.mipav.view.*;

import java.awt.*;
import java.awt.event.*;

import java.util.*;

import javax.swing.*;


/**
 * Dialog to get user input, then call the algorithm. The user is able to control the degree of blurring in x and y
 * dimensions. It should be noted, that the algorithms are executed in their own thread.
 *
 * @see  AlgorithmGVF
 */
public class JDialogGVF extends JDialogBase implements AlgorithmInterface, ItemListener {

    //~ Static fields/initializers -------------------------------------------------------------------------------------

    /** Use serialVersionUID for interoperability. */
    private static final long serialVersionUID = -7812609363025907144L;

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** DOCUMENT ME! */
    private int boundaryIterations;

    /** DOCUMENT ME! */
    private JCheckBox checkboxDisplay;

    /** DOCUMENT ME! */
    private boolean do25D = true;

    /** DOCUMENT ME! */
    private JCheckBox do25DCheckBox;

    /** DOCUMENT ME! */
    private int[] extents;

    /** DOCUMENT ME! */
    private int groupNum;

    /** DOCUMENT ME! */
    private AlgorithmGVF gvfAlgo;

    /** DOCUMENT ME! */
    private int gvfIterations;

    /** DOCUMENT ME! */
    private ModelImage image; // source image

    /** DOCUMENT ME! */
    private float kValue;

    /** DOCUMENT ME! */
    private JLabel labelGaussZ;

    /** DOCUMENT ME! */
    private JRadioButton propagate;

    /** DOCUMENT ME! */
    private boolean propagationFlag;

    /** DOCUMENT ME! */
    private boolean removeOriginal;

    /** DOCUMENT ME! */
    private JCheckBox removeOriginalCheckBox;

    /** DOCUMENT ME! */
    private ModelImage resultImage; // magnitude of GVF

    /** DOCUMENT ME! */
    private float scaleX;

    /** DOCUMENT ME! */
    private float scaleY;

    /** DOCUMENT ME! */
    private float scaleZ = 1.0f;

    /** DOCUMENT ME! */
    private JRadioButton singleSlice;

    /** DOCUMENT ME! */
    private VOI srcVOI;

    /** DOCUMENT ME! */
    private JTextField textBoundaryIterations;

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
    private String[] titles;

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
    public JDialogGVF(Frame theParentFrame, ModelImage im) {
        super(theParentFrame, false);

        VOIs = im.getVOIs();

        int nVOI;

        nVOI = VOIs.size();

        if (nVOI == 0) {
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
     * Closes dialog box when the OK button is pressed and calls the algorithm.
     *
     * @param  event  event that triggers function
     */
    public void actionPerformed(ActionEvent event) {
        Object source = event.getSource();
        String tmpStr;
        int i;

        propagationFlag = false;

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
        else if (source == OKButton) {

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

            if (image.getNDims() == 2) { // source image is 2D

                float[] sigmas = new float[2];
                sigmas[0] = scaleX; // set standard deviations (sigma) in X and Y
                sigmas[1] = scaleY;

                try {

                    if (checkboxDisplay.isSelected() == true) {
                        resultImage = new ModelImage(ModelStorageBase.FLOAT, image.getExtents(),
                                                     image.getImageName() + "_gvf");

                        // Make the algorithm class
                        gvfAlgo = new AlgorithmGVF(resultImage, image, sigmas, gvfIterations, boundaryIterations,
                                                   kValue, srcVOI, do25D);
                    } else {
                        gvfAlgo = new AlgorithmGVF(null, image, sigmas, gvfIterations, boundaryIterations, kValue,
                                                   srcVOI, do25D);
                    }

                    // This is very important. Adding this object as a listener allows the algorithm to
                    // notify this object when it has completed of failed. See algorithm performed event.
                    // This is made possible by implementing AlgorithmedPerformed interface
                    gvfAlgo.addListener(this);

                    createProgressBar(image.getImageName(), gvfAlgo);

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
                        if (gvfAlgo.startMethod(Thread.MIN_PRIORITY) == false) {
                            MipavUtil.displayError("A thread is already running on this object");
                        }
                    } else {
                        gvfAlgo.run();
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
                float[] sigmas;

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

                extents = new int[3];
                extents[0] = image.getExtents()[0];
                extents[1] = image.getExtents()[1];
                extents[2] = image.getExtents()[2];

                try {

                    if (checkboxDisplay.isSelected() == true) {
                        resultImage = new ModelImage(ModelStorageBase.FLOAT, extents, image.getImageName() + "_gvf");

                        // Make algorithm
                        gvfAlgo = new AlgorithmGVF(resultImage, image, sigmas, gvfIterations, boundaryIterations,
                                                   kValue, srcVOI, do25D);
                    } else {
                        gvfAlgo = new AlgorithmGVF(null, image, sigmas, gvfIterations, boundaryIterations, kValue,
                                                   srcVOI, do25D);
                    }

                    gvfAlgo.setPropagation(propagationFlag);

                    // This is very important. Adding this object as a listener allows the algorithm to
                    // notify this object when it has completed of failed. See algorithm performed event.
                    // This is made possible by implementing AlgorithmedPerformed interface
                    gvfAlgo.addListener(this);

                    createProgressBar(image.getImageName(), gvfAlgo);

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
                        if (gvfAlgo.startMethod(Thread.MIN_PRIORITY) == false) {
                            MipavUtil.displayError("A thread is already running on this object");
                        }
                    } else {
                        gvfAlgo.run();
                    }
                } catch (OutOfMemoryError x) {
                    MipavUtil.displayError("Dialog GVF: unable to allocate enough memory");

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

        if (algorithm instanceof AlgorithmGVF) {

            if (gvfAlgo.isCompleted() == true) {

                // The algorithm has completed and produced a new image to be displayed.
                resultVOI = gvfAlgo.getResultVOI();

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
            else if ((gvfAlgo.isCompleted() == false) && (resultImage != null)) {
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
        imageVOIGroup.add(propagate);
        propagate.addActionListener(this);

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

        do25DCheckBox = new JCheckBox(" Slice by slice processing");
        do25DCheckBox.setFont(serif12);
        do25DCheckBox.setForeground(Color.black);
        do25DCheckBox.setSelected(true);
        do25DCheckBox.setEnabled(false);
        do25DCheckBox.addActionListener(this);
        gbc2.gridx = 0;
        gbc2.gridy = 1;
        parametersPanel.add(do25DCheckBox, gbc2);

        GridBagConstraints gbc = new GridBagConstraints();
        gbc.weightx = 1;
        gbc.anchor = GridBagConstraints.WEST;
        gbc.fill = GridBagConstraints.HORIZONTAL;

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
        mainPanel.setBorder(BorderFactory.createEmptyBorder(5, 5, 5, 5));

        JPanel buttonPanel = new JPanel();
        buildOKButton();
        buttonPanel.add(OKButton);
        buildCancelButton();
        buttonPanel.add(cancelButton);
        buildHelpButton();
        buttonPanel.add(helpButton);

        getContentPane().add(mainPanel);
        getContentPane().add(buttonPanel, BorderLayout.SOUTH);
        pack();
        setVisible(true);
    }

}
