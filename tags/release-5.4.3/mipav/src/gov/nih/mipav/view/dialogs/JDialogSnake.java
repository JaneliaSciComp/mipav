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
public class JDialogSnake extends JDialogBase implements AlgorithmInterface {

    //~ Static fields/initializers -------------------------------------------------------------------------------------

    /** Use serialVersionUID for interoperability. */
    private static final long serialVersionUID = -6160718529233426119L;

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** DOCUMENT ME! */
    private int boundaryDir = AlgorithmSnake.OUT_DIR;

    /** DOCUMENT ME! */
    private JComboBox boundaryDirBox;

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
                                  // difference between x,y resolutions (in plane)
                                  // and z resolution (between planes)

    /** DOCUMENT ME! */
    private JRadioButton propagate;

    /** DOCUMENT ME! */
    private int propagationType;

    /** DOCUMENT ME! */
    private boolean removeOriginal;

    /** DOCUMENT ME! */
    private JCheckBox removeOriginalCheckBox;

    /** DOCUMENT ME! */
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
    private float smoothness;

    /** DOCUMENT ME! */
    private AlgorithmSnake snakeAlgo;

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
    private JTextField textSmoothness;

    /** DOCUMENT ME! */
    private String[] titles;

    /** DOCUMENT ME! */
    private ViewUserInterface userInterface;

    /** DOCUMENT ME! */
    private Color voiColor;

    /** DOCUMENT ME! */
    private ViewVOIVector VOIs = null;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Creates new dialog for entering parameters for algorithm.
     *
     * @param  theParentFrame  Parent frame
     * @param  im              Source image.
     */
    public JDialogSnake(Frame theParentFrame, ModelImage im) {
        super(theParentFrame, false);
        userInterface = ViewUserInterface.getReference();
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
    

    /**
     * Creates new dialog for entering parameters for algorithm.
     *
     * @param  theParentFrame  Parent frame
     * @param  im              Source image.
     */
    public JDialogSnake(Frame theParentFrame, ModelImage im, boolean separateThread) {
    	this(theParentFrame, im);
        setSeparateThread(separateThread);   	
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

        propagationType = AlgorithmSnake.PROP_SINGLE;

        if (source == OKButton) {

            if (singleSlice.isSelected()) {
                propagationType = AlgorithmSnake.PROP_SINGLE;
            } else if (propagate.isSelected()) {
                propagationType = AlgorithmSnake.PROP_ALL;
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

            tmpStr = textBoundaryIterations.getText();

            if (testParameter(tmpStr, 1.0, 1000000.0)) {
                boundaryIterations = Integer.valueOf(tmpStr).intValue();
            } else {
                textBoundaryIterations.requestFocus();
                textBoundaryIterations.selectAll();

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

            boundaryDir = boundaryDirBox.getSelectedIndex();

            if (image.getNDims() == 2) { // source image is 2D

                float[] sigmas = new float[2];
                sigmas[0] = scaleX; // set standard deviations (sigma) in X and Y
                sigmas[1] = scaleY;

                try {

                    // No need to make new image space because the user has choosen to replace the source image
                    // Make the algorithm class
                    snakeAlgo = new AlgorithmSnake(image, sigmas, boundaryIterations, smoothness, srcVOI, boundaryDir);

                    // This is very important. Adding this object as a listener allows the algorithm to
                    // notify this object when it has completed of failed. See algorithm performed event.
                    // This is made possible by implementing AlgorithmedPerformed interface
                    snakeAlgo.addListener(this);
                   
                    createProgressBar(image.getImageName(), snakeAlgo);
                    
                    // Hide the dialog since the algorithm is about to run.
                    setVisible(false);

                    // These next lines set the titles in all frames where the source image is displayed to
                    // "locked - " image name so as to indicate that the image is now read/write locked!
                    // The image frames are disabled and then unregisted from the userinterface until the
                    // algorithm has completed.
                    Vector<ViewImageUpdateInterface> imageFrames = image.getImageFrameVector();
                    titles = new String[imageFrames.size()];

                    for (i = 0; i < imageFrames.size(); i++) {
                        titles[i] = ((Frame) (imageFrames.elementAt(i))).getTitle();
                        ((Frame) (imageFrames.elementAt(i))).setTitle("Locked: " + titles[i]);
                        ((Frame) (imageFrames.elementAt(i))).setEnabled(false);
                        userInterface.unregisterFrame((Frame) (imageFrames.elementAt(i)));
                    }

                    if (isRunInSeparateThread()) {
                        // Start the thread as a low priority because we wish to still have user interface work fast.
                        if (snakeAlgo.startMethod(Thread.MIN_PRIORITY) == false) {
                            MipavUtil.displayError("A thread is already running on this object");
                        }
                    } else {
                    	snakeAlgo.run();
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
                    snakeAlgo = new AlgorithmSnake(image, sigmas, boundaryIterations, smoothness, srcVOI, boundaryDir);
                    snakeAlgo.setPropagation(propagationType);

                    // This is very important. Adding this object as a listener allows the algorithm to
                    // notify this object when it has completed of failed. See algorithm performed event.
                    // This is made possible by implementing AlgorithmedPerformed interface
                    snakeAlgo.addListener(this);

                    createProgressBar(image.getImageName(), snakeAlgo);
                    
                    // Hide dialog
                    setVisible(false);

                    // These next lines set the titles in all frames where the source image is displayed to
                    // "locked - " image name so as to indicate that the image is now read/write locked!
                    // The image frames are disabled and then unregisted from the userinterface until the
                    // algorithm has completed.
                    Vector<ViewImageUpdateInterface> imageFrames = image.getImageFrameVector();
                    titles = new String[imageFrames.size()];

                    for (i = 0; i < imageFrames.size(); i++) {
                        titles[i] = ((Frame) (imageFrames.elementAt(i))).getTitle();
                        ((Frame) (imageFrames.elementAt(i))).setTitle("Locked: " + titles[i]);
                        ((Frame) (imageFrames.elementAt(i))).setEnabled(false);
                        userInterface.unregisterFrame((Frame) (imageFrames.elementAt(i)));
                    }

                    if (isRunInSeparateThread()) {
                        // Start the thread as a low priority because we wish to still have user interface work fast.
                        if (snakeAlgo.startMethod(Thread.MIN_PRIORITY) == false) {
                            MipavUtil.displayError("A thread is already running on this object");
                        }
                    } else {
                    	snakeAlgo.run();
                    }
                } catch (OutOfMemoryError x) {
                    MipavUtil.displayError("Dialog snake: unable to allocate enough memory");

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
     * @param  algorithm  algorithm that caused the event.
     */
    public void algorithmPerformed(AlgorithmBase algorithm) {
        VOI resultVOI;

        if (algorithm instanceof AlgorithmSnake) {

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
                ((Frame) (imageFrames.elementAt(i))).setTitle(titles[i]);
                ((Frame) (imageFrames.elementAt(i))).setEnabled(true);
                userInterface.registerFrame((Frame) (imageFrames.elementAt(i)));
            }
        }
        dispose();
    }

    /**
     * When the user clicks the mouse out of a text field, resets the necessary variables.
     *
     * @param  event  Event that triggers this function
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
     * Sets corrected label if resolution checkbox is checked.
     *
     * @param  event  Event that cause the method to fire
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
     * Sets up the GUI (panels, buttons, etc) and displays it on the screen.
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
        textGaussX.setText("2.0");
        textGaussX.setFont(serif12);
        scalePanel.add(textGaussX);

        JLabel labelGaussY = new JLabel(" Y Dimension (0.5 - 5.0) ");
        labelGaussY.setForeground(Color.black);
        labelGaussY.setFont(serif12);
        scalePanel.add(labelGaussY);

        textGaussY = new JTextField();
        textGaussY.setText("2.0");
        textGaussY.setFont(serif12);
        scalePanel.add(textGaussY);

        JLabel labelGaussZ = new JLabel(" Z Dimension (0.0 - 5.0) ");
        labelGaussZ.setForeground(Color.black);
        labelGaussZ.setFont(serif12);
        scalePanel.add(labelGaussZ);

        textGaussZ = new JTextField();
        textGaussZ.setText("2.0");
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


        JPanel paramPanel = new JPanel(new GridLayout(3, 2));
        paramPanel.setForeground(Color.black);
        paramPanel.setBorder(buildTitledBorder("Algorithm parameters"));

        JLabel labelBoundaryDir = new JLabel("Move boundary "); // make & set a label
        labelBoundaryDir.setForeground(Color.black);
        labelBoundaryDir.setFont(serif12);
        paramPanel.add(labelBoundaryDir); // add kernel label

        boundaryDirBox = new JComboBox();
        boundaryDirBox.setFont(serif12);
        boundaryDirBox.setBackground(Color.white);
        boundaryDirBox.addItem("Any direction");
        boundaryDirBox.addItem("Inward");
        boundaryDirBox.addItem("Outward");
        paramPanel.add(boundaryDirBox);

        JLabel labelBoundaryIterations = new JLabel("Boundary iterations");
        labelBoundaryIterations.setForeground(Color.black);
        labelBoundaryIterations.setFont(serif12);
        paramPanel.add(labelBoundaryIterations);

        textBoundaryIterations = new JTextField();
        textBoundaryIterations.setText("50");
        textBoundaryIterations.setFont(serif12);
        paramPanel.add(textBoundaryIterations);

        JLabel labelSmoothness = new JLabel("Smoothness (0.5 - 2.4)");
        labelSmoothness.setForeground(Color.black);
        labelSmoothness.setFont(serif12);
        paramPanel.add(labelSmoothness);

        textSmoothness = new JTextField();
        textSmoothness.setText("2.0");
        textSmoothness.setFont(serif12);
        paramPanel.add(textSmoothness);

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

        getContentPane().add(mainPanel, BorderLayout.CENTER);
        getContentPane().add(buttonPanel, BorderLayout.SOUTH);
        pack();
        setVisible(true);
    }
}
