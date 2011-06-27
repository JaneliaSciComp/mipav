package gov.nih.mipav.view.dialogs;


import gov.nih.mipav.model.algorithms.*;
import gov.nih.mipav.model.algorithms.registration.*;
import gov.nih.mipav.model.structures.*;

import gov.nih.mipav.view.*;

import java.awt.*;
import java.awt.event.*;

import java.util.*;

import javax.swing.*;


/**
 * Dialog to get user input, then call algorithmChamfer. Reference image does not change. Match image is the image that
 * gets transformed until it is registered to the base image. User selects base image using dialog. Algorithms are
 * executed in their own thread.
 *
 * @version  0.2 October 24, 2001
 * @author   Neva Cherniavsky
 * @see      AlgorithmRegChamfer
 */
public class JDialogRegistrationChamfer extends JDialogBase implements AlgorithmInterface {

    //~ Static fields/initializers -------------------------------------------------------------------------------------

    /** Use serialVersionUID for interoperability. */
    private static final long serialVersionUID = 8391544557885381489L;

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** Register match image to base image. */
    private ModelImage baseImage;

    /** Algorithm this dialog will call. */
    private AlgorithmRegChamfer chamfer = null;

    /** GUI components. */
    private JComboBox comboBox2or3D;

    /** DOCUMENT ME! */
    private JComboBox comboBoxEdge;

    /** DOCUMENT ME! */
    private JComboBox comboBoxOriginal;

    /** DOCUMENT ME! */
    private JLabel edge;

    /** <code>true</code> means do each slice individually, <code>false</code> means do them all together. */
    private boolean image25D = false;

    /** Register match image to base image. */
    private ModelImage matchImage;

    /** DOCUMENT ME! */
    private JLabel original;

    /** <code>true</code> means apply algorithm to whole image, <code>false</code> means to just the VOI. */
    private boolean regionFlag = true;

    /** DOCUMENT ME! */
    private JRadioButton register25D;

    /** DOCUMENT ME! */
    private JRadioButton register2Dor3D;

    /** Used for setting image to locked and unlocked. */
    private String[] titles;

    /** DOCUMENT ME! */
    private ViewUserInterface userInterface;

    /** DOCUMENT ME! */
    private JRadioButton voiRegions;

    /** DOCUMENT ME! */
    private JRadioButton wholeImage;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Creates dialog and calls init(), which sets up GUI.
     *
     * @param  theParentFrame  parent frame
     * @param  im              source image
     */
    public JDialogRegistrationChamfer(Frame theParentFrame, ModelImage im) {
        super(theParentFrame, false);
        matchImage = im;
        userInterface = ViewUserInterface.getReference();
        init();
    }

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * Closes dialog box when the OK button is pressed and calls the algorithm.
     *
     * @param  event  Event that triggers function.
     */
    public void actionPerformed(ActionEvent event) {
        String command = event.getActionCommand();

        if (command.equals("OK")) {

            if (setVariables()) {
                callAlgorithm();
            }
        } else if (command.equals("Cancel")) {
            dispose();
        } else if (command.equals("Series")) {

            if (register25D.isSelected()) {
                comboBox2or3D.setEnabled(false);
                comboBoxOriginal.setEnabled(true);
                comboBoxEdge.setEnabled(true);
                original.setEnabled(true);
                edge.setEnabled(true);
            }
        } else if (command.equals("TwoD")) {

            if (register2Dor3D.isSelected()) {
                comboBox2or3D.setEnabled(true);
                comboBoxOriginal.setEnabled(false);
                comboBoxEdge.setEnabled(false);
                original.setEnabled(false);
                edge.setEnabled(false);
            }
        }
    }

    // ************************************************************************
    // ************************** Algorithm Events ****************************
    // ************************************************************************

    /**
     * This method is required if the AlgorithmPerformed interface is implemented. It is called by the algorithm when it
     * has completed or failed to to complete.
     *
     * @param  algorithm  algorithm that caused the event.
     */
    public void algorithmPerformed(AlgorithmBase algorithm) {

        if (algorithm instanceof AlgorithmRegChamfer) {

            if (chamfer.isCompleted() == true) {
                Preferences.debug("chamfer completed = true\n",Preferences.DEBUG_ALGORITHM);

                if (!image25D) {
                    matchImage.setMatrix(chamfer.getTransformMatchtoBase());
                    Preferences.debug("matchor25DImage.getMatrix = ",Preferences.DEBUG_ALGORITHM);
                    System.out.println(matchImage.getMatrix());
                    Preferences.debug("chamfer.getTransformBtoA = ",Preferences.DEBUG_ALGORITHM);
                    System.out.println(chamfer.getTransformMatchtoBase());
                } else {
                    matchImage.calcMinMax();
                }

                Preferences.debug("Chamfer Done\n",Preferences.DEBUG_ALGORITHM);
            }

            // These next lines set the titles in all frames where the source image is displayed to
            // image name so as to indicate that the image is now unlocked!
            // The image frames are enabled and then registed to the userinterface.
            Vector<ViewImageUpdateInterface> imageFrames = matchImage.getImageFrameVector();

            for (int i = 0; i < imageFrames.size(); i++) {
                ((Frame) (imageFrames.elementAt(i))).setTitle(titles[i]);
                ((Frame) (imageFrames.elementAt(i))).setEnabled(true);

                if (((Frame) (imageFrames.elementAt(i))) != parentFrame) {
                    userInterface.registerFrame((Frame) (imageFrames.elementAt(i)));
                }

                ((ViewJFrameBase) parentFrame).getUserInterface().registerFrame(parentFrame);
                matchImage.notifyImageDisplayListeners(null, true);
            }
        }
        // Update frame

        ((ViewJFrameBase) parentFrame).updateImages(true);
        dispose();
    }

    /**
     * Calls the algorithm, assuming the variables are already set up. Runs algorithm in separate thread.
     */
    protected void callAlgorithm() {

        if (image25D) {
            chamfer = new AlgorithmRegChamfer(baseImage, matchImage, regionFlag, image25D);
        } else {
            chamfer = new AlgorithmRegChamfer(baseImage, matchImage, regionFlag);
        }

        // Hide dialog
        setVisible(false);

        // This is very important. Adding this object as a listener allows
        // the algorithm to notify this object when it has completed of failed.
        // See algorithm performed event. This is made possible by implementing
        chamfer.addListener(this);

        createProgressBar(baseImage.getImageName(), chamfer);

        // These next lines set the titles in all frames where the source image
        // is displayed to "locked - " image name so as to indicate that the image
        // is now read/write locked!  The image frames are disabled and then
        // unregisted from the userinterface until the algorithm has completed.
        Vector<ViewImageUpdateInterface> imageFrames = matchImage.getImageFrameVector();
        titles = new String[imageFrames.size()];

        for (int i = 0; i < imageFrames.size(); i++) {
            titles[i] = ((Frame) (imageFrames.elementAt(i))).getTitle();
            ((Frame) (imageFrames.elementAt(i))).setTitle("Locked: " + titles[i]);
            ((Frame) (imageFrames.elementAt(i))).setEnabled(false);
            userInterface.unregisterFrame((Frame) (imageFrames.elementAt(i)));
        }

        // Start the thread as a low priority because we wish to still have
        // user interface work fast
        // if (erodeAlgo3D.startMethod(Thread.MIN_PRIORITY) == false){
        // MipavUtil.displayError("A thread is already running on this object", "Error");
        if (chamfer.startMethod(Thread.MIN_PRIORITY) == false) {
            MipavUtil.displayError("A thread is already running on this object");
        }

    }

    /**
     * Sets up the GUI components.
     */
    private void init() {
        setForeground(Color.black);
        setTitle("Registration Chamfer");

        GridBagConstraints gbc = new GridBagConstraints();

        ButtonGroup registerGroup = new ButtonGroup();
        String matchName = matchImage.getImageName();
        register2Dor3D = new JRadioButton("Register [" + matchName + "] to:", false);
        register2Dor3D.setForeground(Color.black);
        register2Dor3D.setFont(serif12);
        registerGroup.add(register2Dor3D);
        register2Dor3D.addActionListener(this);
        register2Dor3D.setActionCommand("TwoD");
        register2Dor3D.setAlignmentX(Component.LEFT_ALIGNMENT);

        comboBox2or3D = buildImageComboBox(matchImage);
        comboBox2or3D.setEnabled(false);

        register25D = new JRadioButton("Register series (2.5D)", true);
        register25D.setFont(serif12);
        registerGroup.add(register25D);
        register25D.setSelected(false);
        register25D.addActionListener(this);
        register25D.setActionCommand("Series");
        register25D.setAlignmentX(Component.LEFT_ALIGNMENT);

        original = new JLabel("Original image:");
        original.setForeground(Color.black);
        original.setFont(serif12);
        original.setAlignmentX(Component.LEFT_ALIGNMENT);

        comboBoxOriginal = buildImageComboBox(matchImage);

        edge = new JLabel("Edge image:");
        edge.setForeground(Color.black);
        edge.setFont(serif12);
        edge.setAlignmentX(Component.LEFT_ALIGNMENT);

        comboBoxEdge = buildImageComboBox(matchImage);

        ButtonGroup imageVOIGroup = new ButtonGroup();
        wholeImage = new JRadioButton("Whole image", true);
        wholeImage.setFont(serif12);
        imageVOIGroup.add(wholeImage);
        wholeImage.setAlignmentX(Component.LEFT_ALIGNMENT);

        voiRegions = new JRadioButton("VOI region(s)", false);
        voiRegions.setFont(serif12);
        imageVOIGroup.add(voiRegions);
        voiRegions.setAlignmentX(Component.LEFT_ALIGNMENT);

        JPanel regPanel = new JPanel(new GridBagLayout());
        gbc.gridx = 0;
        gbc.gridy = 0;
        gbc.gridheight = 1;
        gbc.gridwidth = 1;
        gbc.anchor = GridBagConstraints.WEST;
        regPanel.add(register2Dor3D, gbc);
        gbc.gridx = 1;
        regPanel.add(comboBox2or3D, gbc);
        gbc.gridx = 0;
        gbc.gridy = 1;
        regPanel.add(register25D, gbc);
        gbc.gridx = 1;
        regPanel.add(original, gbc);
        gbc.gridx = 2;
        regPanel.add(comboBoxOriginal, gbc);
        gbc.gridx = 1;
        gbc.gridy = 2;
        regPanel.add(edge, gbc);
        gbc.gridx = 2;
        regPanel.add(comboBoxEdge, gbc);
        regPanel.setAlignmentX(Component.LEFT_ALIGNMENT);

        JPanel voiPanel = new JPanel();
        voiPanel.setLayout(new BoxLayout(voiPanel, BoxLayout.Y_AXIS));
        voiPanel.setBorder(buildTitledBorder("Register"));
        voiPanel.add(wholeImage);
        voiPanel.add(voiRegions);
        voiPanel.setAlignmentX(Component.LEFT_ALIGNMENT);

        JPanel mainPanel = new JPanel();
        mainPanel.setLayout(new BoxLayout(mainPanel, BoxLayout.Y_AXIS));
        mainPanel.setBorder(buildTitledBorder("Options"));
        mainPanel.add(regPanel);
        mainPanel.add(voiPanel);

        buildOKButton();
        buildCancelButton();

        JPanel buttonPanel = new JPanel();
        buttonPanel.add(OKButton);
        buttonPanel.add(cancelButton);

        if (matchImage.getNDims() == 2) {
            register25D.setEnabled(false);
            image25D = false;
            comboBoxOriginal.setEnabled(false);
            comboBoxEdge.setEnabled(false);
            original.setEnabled(false);
            edge.setEnabled(false);
        }

        getContentPane().add(mainPanel);
        getContentPane().add(buttonPanel, BorderLayout.SOUTH);
        pack();
        setVisible(true);
    }

    /**
     * Sets variables required to run the algorithm.
     *
     * @return  <code>true</code> if successful in setting variables, <code>false</code> if not and should cancel
     *          algorithm.
     */
    private boolean setVariables() {
        ViewUserInterface UI;

        try {

            if (wholeImage.isSelected()) {
                regionFlag = true;
            } else if (voiRegions.isSelected()) {
                regionFlag = false;
            }

            UI = ViewUserInterface.getReference();

            String imageBoxSelected = null;

            if (register25D.isSelected()) {
                image25D = true;
                imageBoxSelected = (String) comboBoxOriginal.getSelectedItem();
                baseImage = UI.getRegisteredImageByName(imageBoxSelected);
                imageBoxSelected = (String) comboBoxEdge.getSelectedItem();
                matchImage = UI.getRegisteredImageByName(imageBoxSelected);
            } else { // assign baseImage to image selected in comboBox
                imageBoxSelected = (String) comboBox2or3D.getSelectedItem();
                baseImage = UI.getRegisteredImageByName(imageBoxSelected);
            }
        } catch (OutOfMemoryError x) {
            MipavUtil.displayError("Dialog Register Chamfer: unable to allocate enough memory");

            return false;
        }

        return true;
    }

}
