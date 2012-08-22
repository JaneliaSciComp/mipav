package gov.nih.mipav.view.dialogs;


import gov.nih.mipav.model.algorithms.*;
import gov.nih.mipav.model.structures.*;

import gov.nih.mipav.view.*;

import java.awt.*;
import java.awt.event.*;

import java.util.*;

import javax.swing.*;


/**
 * Dialog to get user input, then call the algorithm. The user is able to control the degree of blurring in all
 * dimensions and indicate if a correction factor be applied to the z-dimension to account for differing resolutions
 * between the xy resolutions (intra-plane) and the z resolution (inter-plane). The user has the option to generate a
 * new image or replace the source image. In addition the user can indicate if you wishes to have the algorithm applied
 * to whole image or to the VOI regions. In should be noted, that the algorithms are executed in their own thread.
 *
 * @version  0.1 Nov 17, 1998
 * @author   Matthew J. McAuliffe, Ph.D.
 * @see      AlgorithmSnake
 */
public class JDialogBSnake extends JDialogBase implements AlgorithmInterface {

    //~ Static fields/initializers -------------------------------------------------------------------------------------

    /** Use serialVersionUID for interoperability. */
    private static final long serialVersionUID = 6813632326437983422L;

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** DOCUMENT ME! */
    private int boundaryIterations;

    /** DOCUMENT ME! */
    private int groupNum;

    /** DOCUMENT ME! */
    private ModelImage image; // source image

    /** DOCUMENT ME! */
    private JLabel labelCorrected;

    /** DOCUMENT ME! */
    private float normFactor = 1; // normalization factor to adjust for resolution

    /** DOCUMENT ME! */
    private JRadioButton propagate;

    /** DOCUMENT ME! */
    private int propagationType;

    /** DOCUMENT ME! */
    private boolean removeOriginal;

    /** DOCUMENT ME! */
    private JCheckBox removeOriginalCheckBox;

    /** difference between x,y resolutions (in plane) and z resolution (between planes). */
    private JCheckBox resolutionCheckbox;

    /** DOCUMENT ME! */
    private float scaleX;

    /** DOCUMENT ME! */
    private float scaleY;

    /** DOCUMENT ME! */
    private float scaleZ;

    /** DOCUMENT ME! */
    private JRadioButton singleSlice;

    /** DOCUMENT ME! */
    private AlgorithmBSnake snakeAlgo;

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
    private String[] titles;

    /** DOCUMENT ME! */
    private Color voiColor;

    /** DOCUMENT ME! */
    private ViewVOIVector VOIs = null;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Creates new dialog for entering parameters for algorithm.
     *
     * @param  theParentFrame  Parent frame
     * @param  im              Source image
     */
    public JDialogBSnake(Frame theParentFrame, ModelImage im) {
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
        propagationType = AlgorithmBSnake.PROP_SINGLE;

        if (source == OKButton) {

            if (singleSlice.isSelected()) {
                propagationType = AlgorithmBSnake.PROP_SINGLE;
            } else if (propagate.isSelected()) {
                propagationType = AlgorithmBSnake.PROP_ALL;
            }

            removeOriginal = removeOriginalCheckBox.isSelected();

            tmpStr = textGaussX.getText();

            if (testParameter(tmpStr, 0.5, 5.0)) {
                scaleX = Float.valueOf(tmpStr).floatValue();
            } else {
                textGaussX.requestFocus();
                textGaussX.selectAll();

                return;
            }

            tmpStr = textGaussY.getText();

            if (testParameter(tmpStr, 0.5, 5.0)) {
                scaleY = Float.valueOf(tmpStr).floatValue();
            } else {
                textGaussY.requestFocus();
                textGaussY.selectAll();

                return;
            }

            tmpStr = textGaussZ.getText();

            if (testParameter(tmpStr, 0.0, 5.0)) {
                scaleZ = Float.valueOf(tmpStr).floatValue();
            } else {
                textGaussZ.requestFocus();
                textGaussZ.selectAll();

                return;
            }

            // Apply normalization if requested!
            if (resolutionCheckbox.isSelected()) {
                scaleZ = scaleZ * normFactor;
            }

            tmpStr = textBoundaryIterations.getText();

            if (testParameter(tmpStr, 1.0, 1000000.0)) {
                boundaryIterations = Integer.valueOf(tmpStr).intValue();
            } else {
                textBoundaryIterations.requestFocus();
                textBoundaryIterations.selectAll();

                return;
            }

            if (image.getNDims() == 2) { // source image is 2D

                float[] sigmas = new float[2];
                sigmas[0] = scaleX; // set standard deviations (sigma) in X and Y
                sigmas[1] = scaleY;

                try {

                    // No need to make new image space because the user has choosen to replace the source image
                    // Make the algorithm class
                    snakeAlgo = new AlgorithmBSnake(image, sigmas, boundaryIterations, srcVOI);

                    // This is very important. Adding this object as a listener allows the algorithm to
                    // notify this object when it has completed of failed. See algorithm performed event.
                    // This is made possible by implementing AlgorithmedPerformed interface
                    snakeAlgo.addListener(this);

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

                    // Start the thread as a low priority because we wish to still have user interface.
                    if (snakeAlgo.startMethod(Thread.MIN_PRIORITY) == false) {
                        MipavUtil.displayError("A thread is already running on this object");
                    }
                } catch (OutOfMemoryError x) {
                    MipavUtil.displayError("Dialog Snake: unable to allocate enough memory");

                    return;
                }
            } else if (image.getNDims() == 3) {
                float[] sigmas = new float[3];
                sigmas[0] = scaleX;
                sigmas[1] = scaleY;
                sigmas[2] = scaleZ;

                try {

                    // Make algorithm
                    snakeAlgo = new AlgorithmBSnake(image, sigmas, boundaryIterations, srcVOI);
                    snakeAlgo.setPropagation(propagationType);

                    // This is very important. Adding this object as a listener allows the algorithm to
                    // notify this object when it has completed of failed. See algorithm performed event.
                    // This is made possible by implementing AlgorithmedPerformed interface
                    snakeAlgo.addListener(this);

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

                    // Start the thread as a low priority because we wish to still have user interface work fast
                    if (snakeAlgo.startMethod(Thread.MIN_PRIORITY) == false) {
                        MipavUtil.displayError("A thread is already running on this object");
                    }
                } catch (OutOfMemoryError x) {
                    MipavUtil.displayError("Dialog BSnake: unable to allocate enough memory");

                    return;
                }
            }
        } else if (source == cancelButton) {
            dispose();
        }
        
        else if (source == helpButton) {
            //MipavUtil.showHelp("10506");
            MipavUtil.showWebHelp("Segmenting_Images_Using_Contours_and_Masks:_Using_contours_to_segment_a_VOI");
        }
    }

    // ************************************************************************
    // ************************** Algorithm Events ****************************
    // ************************************************************************

    /**
     * algorithmPerformed this method is required if the AlgorithmPerformed interface is implemented. It is called by
     * the algorithms when it has completed or failed to to complete, so that the dialog can be display the result image
     * and/or clean up.
     *
     * @param  algorithm  algorithm that caused the event.
     */
    public void algorithmPerformed(AlgorithmBase algorithm) {
        VOI resultVOI;

        // ViewJFrameImage imageFrame = null;
        if (algorithm instanceof AlgorithmBSnake) {

            if (snakeAlgo.isCompleted() == true) {

                // The algorithm has completed and produced a new image to be displayed.
                resultVOI = snakeAlgo.getResultVOI();

                if (removeOriginal) {
                    resultVOI.setColor(voiColor);
                    image.getVOIs().removeElementAt(groupNum);
                }

                image.registerVOI(resultVOI);

                // Update frame
                ((ViewJFrameBase) parentFrame).updateImages(true);
                if ( voiManager != null )
                {
                    voiManager.algorithmPerformed();
                }

            } // if(snakeAlgo.isCompleted() == true)

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
     * focusLost - when the user clicks the mouse out of a text field, resets the neccessary variables.
     *
     * @param  event  event that triggers this function
     */
    public void focusLost(FocusEvent event) {
        Object source = event.getSource();
        float tempNum;

        if (source == textGaussZ) {

            if (resolutionCheckbox.isSelected()) {
                tempNum = normFactor * Float.valueOf(textGaussZ.getText()).floatValue();
                labelCorrected.setText("      Corrected scale = " + makeString(tempNum, 3));
            } else {
                labelCorrected.setText(" ");
            }
        }
    }

    /**
     * itemStateChanged - method to handle item events.
     *
     * @param  event  event that cause the method to fire
     */
    public void itemStateChanged(ItemEvent event) {
        Object source = event.getSource();
        float tempNum;

        if (source == resolutionCheckbox) {

            if (resolutionCheckbox.isSelected()) {
                tempNum = normFactor * Float.valueOf(textGaussZ.getText()).floatValue();
                labelCorrected.setText("      Corrected scale = " + makeString(tempNum, 3));
            } else {
                labelCorrected.setText(" ");
            }
        }
    }

    /**
     * Sets up the d(Color.black);GUI (panels, buttons, etc) and displays it on the screen.
     */
    private void init() {
        setForeground(Color.black);
        setTitle("Evolve Boundary");
        JPanel scalePanel = new JPanel(new GridLayout(3, 2));
        scalePanel.setForeground(Color.black);
        scalePanel.setBorder(buildTitledBorder("Scale of the Gaussian"));

        JLabel labelGaussX = new JLabel(" X Dimension (0.5 - 5.0) ");
        labelGaussX.setForeground(Color.black);
        labelGaussX.setFont(serif12);
        scalePanel.add(labelGaussX);

        textGaussX = new JTextField();
        textGaussX.setText("1.0");
        textGaussX.setFont(serif12);
        scalePanel.add(textGaussX);

        JLabel labelGaussY = new JLabel(" Y Dimension (0.5 - 5.0) ");
        labelGaussY.setForeground(Color.black);
        labelGaussY.setFont(serif12);
        scalePanel.add(labelGaussY);

        textGaussY = new JTextField();
        textGaussY.setText("1.0");
        textGaussY.setFont(serif12);
        scalePanel.add(textGaussY);

        JLabel labelGaussZ = new JLabel(" Z Dimension (0.0 - 5.0) ");
        labelGaussZ.setForeground(Color.black);
        labelGaussZ.setFont(serif12);
        scalePanel.add(labelGaussZ);

        textGaussZ = new JTextField();
        textGaussZ.setText("1.0");
        textGaussZ.setFont(serif12);
        scalePanel.add(textGaussZ);

        JPanel resPanel = new JPanel(new GridLayout(2, 1));
        resPanel.setBorder(buildTitledBorder("Resolution options"));

        resolutionCheckbox = new JCheckBox("Use image resolutions to normalize Z scale.");
        resolutionCheckbox.setFont(serif12);
        resPanel.add(resolutionCheckbox);
        resolutionCheckbox.setSelected(true);

        if (image.getNDims() == 3) { // if the source image is 3D then allow
            resolutionCheckbox.setEnabled(true); // the user to indicate if it wishes to
            resolutionCheckbox.addItemListener(this); // use the correction factor
            textGaussZ.addFocusListener(this);
            textGaussZ.setEnabled(true);
        } else {
            resolutionCheckbox.setEnabled(false); // Image is only 2D, thus this checkbox
            labelGaussZ.setEnabled(false); // is not relevent
            textGaussZ.setEnabled(false);
        }

        if (image.getNDims() == 3) { // Source image is 3D, thus show correction factor

            int index = image.getExtents()[2] / 2;
            float xRes = image.getFileInfo(index).getResolutions()[0];
            float zRes = image.getFileInfo(index).getResolutions()[2];
            normFactor = xRes / zRes; // Calculate correction factor
            labelCorrected = new JLabel("      Corrected scale = " +
                                        String.valueOf(normFactor * Float.valueOf(textGaussZ.getText()).floatValue()));
            labelCorrected.setForeground(Color.black);
            labelCorrected.setFont(serif12);
            resPanel.add(labelCorrected);
        }

        JPanel imageVOIPanel = new JPanel(new GridLayout(3, 1));
        imageVOIPanel.setForeground(Color.black);
        imageVOIPanel.setBorder(buildTitledBorder("Evolve Boundary"));

        ButtonGroup imageVOIGroup = new ButtonGroup();
        singleSlice = new JRadioButton("Single slice", true);
        singleSlice.setFont(serif12);
        imageVOIGroup.add(singleSlice);
        imageVOIPanel.add(singleSlice);

        propagate = new JRadioButton("Propagate to adjacent slices", false);
        propagate.setFont(serif12);
        imageVOIGroup.add(propagate);
        imageVOIPanel.add(propagate);

        if (image.getNDims() == 2) {
            propagate.setEnabled(false);
        }

        removeOriginalCheckBox = new JCheckBox("Replace Original Contour");
        removeOriginalCheckBox.setFont(serif12);
        removeOriginalCheckBox.setForeground(Color.black);
        removeOriginalCheckBox.setSelected(false);
        imageVOIPanel.add(removeOriginalCheckBox);

        JPanel paramPanel = new JPanel(new GridLayout(1, 2));
        paramPanel.setForeground(Color.black);
        paramPanel.setBorder(buildTitledBorder("Algorithm parameters"));

        JLabel labelBoundaryIterations = new JLabel("Boundary iterations");
        labelBoundaryIterations.setForeground(Color.black);
        labelBoundaryIterations.setFont(serif12);
        paramPanel.add(labelBoundaryIterations);

        textBoundaryIterations = new JTextField();
        textBoundaryIterations.setText("20");
        textBoundaryIterations.setFont(serif12);
        paramPanel.add(textBoundaryIterations);

        JPanel mainPanel = new JPanel(new GridBagLayout());
        mainPanel.setBorder(BorderFactory.createEmptyBorder(3, 3, 3, 3));

        GridBagConstraints gbc = new GridBagConstraints();
        gbc.gridwidth = 1;
        gbc.gridheight = 1;
        gbc.anchor = GridBagConstraints.WEST;
        gbc.weightx = 1;
        gbc.insets = new Insets(3, 3, 3, 3);
        gbc.gridx = 0;
        gbc.gridy = 0;
        gbc.fill = GridBagConstraints.HORIZONTAL;
        mainPanel.add(scalePanel, gbc);
        gbc.gridy = 1;
        mainPanel.add(resPanel, gbc);
        gbc.gridy = 2;
        mainPanel.add(imageVOIPanel, gbc);
        gbc.gridy = 3;
        mainPanel.add(paramPanel, gbc);

        JPanel buttonPanel = new JPanel();
        buildOKButton();
        buttonPanel.add(OKButton);
        buildCancelButton();
        buttonPanel.add(cancelButton);
        buildHelpButton();
        buttonPanel.add(helpButton);

        mainDialogPanel.add(mainPanel, BorderLayout.CENTER);
        mainDialogPanel.add(buttonPanel, BorderLayout.SOUTH);

        getContentPane().add(mainDialogPanel);

        pack();
        setVisible(true);
    }
}
