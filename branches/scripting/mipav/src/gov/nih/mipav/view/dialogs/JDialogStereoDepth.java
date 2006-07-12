package gov.nih.mipav.view.dialogs;


import gov.nih.mipav.model.algorithms.*;
import gov.nih.mipav.model.structures.*;

import gov.nih.mipav.view.*;

import java.awt.*;
import java.awt.event.*;

import java.util.*;

import javax.swing.*;


/**
 * Dialog to get user input Dialog to input left and right stereo images for calculating stereo depth.
 */
public class JDialogStereoDepth extends JDialogBase implements AlgorithmInterface, ItemListener {

    //~ Static fields/initializers -------------------------------------------------------------------------------------

    /** Use serialVersionUID for interoperability. */
    private static final long serialVersionUID = -4416780919935465708L;

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** DOCUMENT ME! */
    private JComboBox comboBoxImage;

    /** DOCUMENT ME! */
    private ModelImage firstImage, secondImage;

    /** DOCUMENT ME! */
    private JLabel fourthLabel;

    /** DOCUMENT ME! */
    private JRadioButton leftButton;

    /** DOCUMENT ME! */
    private ModelImage leftImage, rightImage;

    /** DOCUMENT ME! */
    private ModelImage resultImage = null; // result image

    /** DOCUMENT ME! */
    private JRadioButton rightButton;

    /** DOCUMENT ME! */
    private AlgorithmStereoDepth stereoDepthAlgo = null;

    /** DOCUMENT ME! */
    private String[] titles;

    /** DOCUMENT ME! */
    private ViewUserInterface UI;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Creates new dialog.
     *
     * @param  theParentFrame  Parent frame
     * @param  im              Source image
     */
    public JDialogStereoDepth(Frame theParentFrame, ModelImage im) {
        super(theParentFrame, true);
        firstImage = im;
        init();
    }

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * Closes dialog box when the OK button is pressed and calls the algorithm.
     *
     * @param  event  Event that triggers function.
     */
    public void actionPerformed(ActionEvent event) {
        Object source = event.getSource();
        String command = event.getActionCommand();

        if (command.equals("OK")) {

            if (setVariables()) {
                callAlgorithm();
            }
        } else if (command.equals("Script")) {
            callAlgorithm();
        } else if (command.equals("Cancel")) {
            dispose();
        } else if ((source == leftButton) || (source == rightButton)) {

            if (leftButton.isSelected()) {
                fourthLabel.setText(" Image B:  Right");
            } else {
                fourthLabel.setText(" Image B:  Left");
            }
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

        ViewJFrameImage imageFrame = null;

        if (algorithm instanceof AlgorithmStereoDepth) {
            firstImage.clearMask();
            secondImage.clearMask();

            if ((stereoDepthAlgo.isCompleted() == true) && (resultImage != null)) {
                // The algorithm has completed and produced a new image to be displayed.

                updateFileInfo(firstImage, resultImage);
                resultImage.clearMask();

                try {
                    imageFrame = new ViewJFrameImage(resultImage, null, new Dimension(610, 200));
                } catch (OutOfMemoryError error) {
                    System.gc();
                    MipavUtil.displayError("Out of memory: unable to open new frame");
                }
            } else if (resultImage != null) {

                // algorithm failed but result image still has garbage
                resultImage.disposeLocal(); // clean up memory
                resultImage = null;
                System.gc();

            }
        }

        if (stereoDepthAlgo.isCompleted() == true) {

            if (UI.isScriptRecording()) {

                // check to see if the image is already in the ImgTable
                if (UI.getScriptDialog().getImgTableVar(leftImage.getImageName()) == null) {

                    if (UI.getScriptDialog().getActiveImgTableVar(leftImage.getImageName()) == null) {
                        UI.getScriptDialog().putActiveVar(leftImage.getImageName());
                    }
                }

                // check to see if the image is already in the ImgTable
                if (UI.getScriptDialog().getImgTableVar(rightImage.getImageName()) == null) {

                    if (UI.getScriptDialog().getActiveImgTableVar(rightImage.getImageName()) == null) {
                        UI.getScriptDialog().putActiveVar(rightImage.getImageName());
                    }
                }

                UI.getScriptDialog().append("Stereo Depth " + UI.getScriptDialog().getVar(leftImage.getImageName()) +
                                            " " + UI.getScriptDialog().getVar(rightImage.getImageName()) + " " +
                                            UI.getScriptDialog().getVar(resultImage.getImageName()) + "\n");

            }
        }

        dispose();
    }

    /**
     * Builds a list of images. Returns combobox. List must be all color or all black and white.
     *
     * @param   image  DOCUMENT ME!
     *
     * @return  Newly created combo box.
     */
    private JComboBox buildComboBox(ModelImage image) {
        ViewUserInterface UI;
        ModelImage nextImage;

        JComboBox comboBox = new JComboBox();
        comboBox.setFont(serif12);
        comboBox.setBackground(Color.white);

        UI = image.getUserInterface();

        Enumeration names = UI.getRegisteredImageNames();

        while (names.hasMoreElements()) {
            String name = (String) names.nextElement();

            if (!name.equals(image.getImageName())) {
                nextImage = UI.getRegisteredImageByName(name);

                if ((image.isColorImage() == nextImage.isColorImage()) &&
                        (UI.getFrameContainingImage(nextImage) != null) && (nextImage.getNDims() == 2) &&
                        (image.getExtents()[0] == nextImage.getExtents()[0]) &&
                        (image.getExtents()[1] == nextImage.getExtents()[1])) {
                    comboBox.addItem(name);
                }
            }
        }

        return comboBox;
    }

    /**
     * DOCUMENT ME!
     */
    protected void callAlgorithm() {
        String name = makeImageName(firstImage.getImageName(), "_sDepth");

        try {
            resultImage = new ModelImage(ModelImage.FLOAT, firstImage.getExtents(), name,
                                         firstImage.getUserInterface());

            // Make algorithm
            stereoDepthAlgo = new AlgorithmStereoDepth(resultImage, leftImage, rightImage);

            // This is very important. Adding this object as a listener allows the algorithm to
            // notify this object when it has completed of failed. See algorithm performed event.
            // This is made possible by implementing AlgorithmedPerformed interface
            stereoDepthAlgo.addListener(this);

            // Hide dialog
            setVisible(false);

            if (isRunInSeparateThread()) {

                // Start the thread as a low priority because we wish to still have user interface work fast.
                if (stereoDepthAlgo.startMethod(Thread.MIN_PRIORITY) == false) {
                    MipavUtil.displayError("A thread is already running on this object");
                }
            } else {
                if (!UI.isAppFrameVisible()) {
                    stereoDepthAlgo.setProgressBarVisible(false);
                }

                stereoDepthAlgo.run();
            }
        } catch (OutOfMemoryError x) {

            if (resultImage != null) {
                resultImage.disposeLocal(); // Clean up memory of result image
                resultImage = null;
            }

            System.gc();
            MipavUtil.displayError("Dialog Stereo Depth: unable to allocate enough memory");

            return;
        }

    }

    /**
     * Initializes GUI components and displays dialog.
     */
    private void init() {
        setForeground(Color.black);
        setTitle("Stereo Depth");

        String firstName = firstImage.getImageName();

        JPanel imagePanel = new JPanel(new GridBagLayout());
        imagePanel.setBorder(BorderFactory.createEmptyBorder(3, 3, 3, 3));
        imagePanel.setForeground(Color.black);
        imagePanel.setBorder(buildTitledBorder("Images"));

        GridBagConstraints gbc = new GridBagConstraints();
        gbc.gridwidth = 3;
        gbc.gridheight = 1;
        gbc.anchor = gbc.WEST;
        gbc.weightx = 1;
        gbc.insets = new Insets(3, 3, 3, 3);
        gbc.gridx = 0;
        gbc.gridy = 0;
        gbc.fill = GridBagConstraints.HORIZONTAL;

        JLabel firstLabel = new JLabel("Image A:  " + firstName);
        firstLabel.setForeground(Color.black);
        firstLabel.setFont(serif12);
        imagePanel.add(firstLabel, gbc);

        gbc.gridwidth = 1;
        gbc.gridy = 1;

        JLabel secondLabel = new JLabel("Image A:  ");
        secondLabel.setForeground(Color.black);
        secondLabel.setFont(serif12);
        imagePanel.add(secondLabel, gbc);

        gbc.gridx = 1;

        ButtonGroup LRGroup = new ButtonGroup();
        leftButton = new JRadioButton("Left", true);
        leftButton.setFont(serif12);
        leftButton.setEnabled(true);
        leftButton.addActionListener(this);
        LRGroup.add(leftButton); // add the button to the grouping
        imagePanel.add(leftButton, gbc); // add the button to the component

        gbc.gridx = 2;
        rightButton = new JRadioButton("Right", false);
        rightButton.setFont(serif12);
        rightButton.setEnabled(true);
        rightButton.addActionListener(this);
        LRGroup.add(rightButton); // add the button to the grouping
        imagePanel.add(rightButton, gbc); // add the button to the component

        gbc.gridx = 0;
        gbc.gridy = 2;

        JLabel thirdLabel = new JLabel("Image B:  ");
        thirdLabel.setForeground(Color.black);
        thirdLabel.setFont(serif12);
        imagePanel.add(thirdLabel, gbc);

        gbc.gridx = 1;
        gbc.gridwidth = 2;
        comboBoxImage = buildComboBox(firstImage);
        imagePanel.add(comboBoxImage, gbc);

        gbc.gridx = 0;
        gbc.gridy = 3;
        fourthLabel = new JLabel(" Image B:  Right");
        fourthLabel.setForeground(Color.black);
        fourthLabel.setFont(serif12);
        imagePanel.add(fourthLabel, gbc);

        buildOKButton();
        buildCancelButton();

        JPanel buttonPanel = new JPanel();
        buttonPanel.add(OKButton);
        buttonPanel.add(cancelButton);

        getContentPane().add(imagePanel, BorderLayout.NORTH);
        getContentPane().add(buttonPanel, BorderLayout.SOUTH);

        pack();
        setVisible(true);
    }

    /**
     * Use the GUI results to set up the variables needed to run the algorithm.
     *
     * @return  <code>true</code> if parameters set successfully, <code>false</code> otherwise.
     */
    private boolean setVariables() {

        UI = firstImage.getUserInterface();

        String selectedName = (String) comboBoxImage.getSelectedItem();
        secondImage = UI.getRegisteredImageByName(selectedName);

        if (secondImage == null) {
            return false;
        }

        if (leftButton.isSelected()) {
            leftImage = firstImage;
            rightImage = secondImage;
        } else {
            rightImage = firstImage;
            leftImage = secondImage;
        }

        return true;
    }

}
