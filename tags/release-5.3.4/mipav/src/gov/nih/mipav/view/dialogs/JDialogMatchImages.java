package gov.nih.mipav.view.dialogs;


import gov.nih.mipav.model.algorithms.*;
import gov.nih.mipav.model.algorithms.utilities.*;
import gov.nih.mipav.model.scripting.*;
import gov.nih.mipav.model.scripting.parameters.*;
import gov.nih.mipav.model.structures.*;

import gov.nih.mipav.view.*;

import java.awt.*;
import java.awt.event.*;

import java.util.*;

import javax.swing.*;


/**
 * Dialog to get user input, then call the algorithm...
 *
 * @version  0.1 July 14, 2003
 * @author   Zohara A Cohen, Ph.D.
 */
public class JDialogMatchImages extends JDialogScriptableBase implements AlgorithmInterface {

    //~ Static fields/initializers -------------------------------------------------------------------------------------

    /** Use serialVersionUID for interoperability. */
    private static final long serialVersionUID = -4508090288311270016L;

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** DOCUMENT ME! */
    private JCheckBox checkOrigins, checkDimensions;

    /** DOCUMENT ME! */
    private JComboBox comboBoxImageA, comboBoxImageB;

    /** DOCUMENT ME! */
    private boolean doOrigins, doDimensions;

    /** DOCUMENT ME! */
    private ModelImage imageA; // first source image

    /** DOCUMENT ME! */
    private ModelImage imageB; // second source image

    /** DOCUMENT ME! */
    private AlgorithmMatchImages matchAlgo;

    /** DOCUMENT ME! */
    private JTextField padValTxt;

    /** DOCUMENT ME! */
    private int padValue = 0;

    /** DOCUMENT ME! */
    private ModelImage resultImageA = null; // result image

    /** DOCUMENT ME! */
    private ModelImage resultImageB = null; // result image

    /** DOCUMENT ME! */
    private String selectedNameA, selectedNameB;

    /** DOCUMENT ME! */
    private ViewUserInterface userInterface;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Empty constructor needed for dynamic instantiation (used during scripting).
     */
    public JDialogMatchImages() { }

    /**
     * Creates new match image dialog and displays.
     *
     * @param  theParentFrame  Parent frame.
     * @param  im              Source image.
     */
    public JDialogMatchImages(Frame theParentFrame, ModelImage im) {
        super(theParentFrame, false);
        imageA = im;
        userInterface = ViewUserInterface.getReference();
        init();
    }

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * Closes dialog box when the OK button is pressed and calls the algorithm.
     *
     * @param  event  event that triggers function
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
            MipavUtil.showHelp("U4037");
        }
    }

    // ************************************************************************
    // ************************** Algorithm Events ****************************
    // ************************************************************************

    /**
     * This method is required if the AlgorithmPerformed interface is implemented. It is called by the algorithms when
     * it has completed or failed to complete, so that the dialog can display the result image and/or clean up.
     *
     * @param  algorithm  Algorithm that caused the event.
     */
    public void algorithmPerformed(AlgorithmBase algorithm) {
        if (algorithm instanceof AlgorithmMatchImages) {

            if (imageA != matchAlgo.getImageA()) {
                resultImageA = matchAlgo.getImageA();
            }

            if (imageB != matchAlgo.getImageB()) {
                resultImageB = matchAlgo.getImageB();
            }

            if (matchAlgo.isCompleted() == true) {
               // System.out.println("AlgorithmMatchImages completed.");

                // Display new images
                try {

                    if (resultImageA != null) {
                        resultImageA.calcMinMax();
                        new ViewJFrameImage(resultImageA, null, new Dimension(25, 55));
                    }

                    if (resultImageB != null) {
                        resultImageB.calcMinMax();
                        new ViewJFrameImage(resultImageB, null, new Dimension(35, 65));
                    }
                } catch (OutOfMemoryError error) {
                    System.gc();
                    MipavUtil.displayError("Out of memory: unable to open new frames");
                }
            } else {

                if (resultImageA != null) {
                    resultImageA.disposeLocal(); // Clean up memory of result images
                }

                if (resultImageB != null) {
                    resultImageB.disposeLocal(); // Clean up memory of result image
                }
            }
        }

        // Update frames
        imageA.notifyImageDisplayListeners(null, true);
        imageB.notifyImageDisplayListeners(null, true);

        // Write to script.
        if (algorithm.isCompleted()) {
            insertScriptLine();
        }

        matchAlgo.finalize();
        matchAlgo = null;

        dispose();
        System.gc();
    }

    /**
     * Accessor that returns the image.
     *
     * @return  The result image.
     */
    public ModelImage getResultImageA() {
        return resultImageA;
    }

    /**
     * Accessor that returns the image.
     *
     * @return  The result image.
     */
    public ModelImage getResultImageB() {
        return resultImageB;
    }

    /**
     * Accessor that sets image A.
     *
     * @param  im  Image A.
     */
    public void setImageA(ModelImage im) {
        imageA = im;
    }

    /**
     * Accessor that sets image B.
     *
     * @param  im  Image B.
     */
    public void setImageB(ModelImage im) {
        imageB = im;
    }

    /**
     * Once all the necessary variables are set, call the Gaussian Blur algorithm based on what type of image this is
     * and whether or not there is a separate destination image.
     */
    protected void callAlgorithm() {

        try {

            // Make algorithm
            matchAlgo = new AlgorithmMatchImages(imageA, imageB, doOrigins, doDimensions);
            //matchAlgo.setPadValue(padValue);

            // This is very important. Adding this object as a listener allows the algorithm to
            // notify this object when it has completed or failed. See algorithm performed event.
            // This is made possible by implementing AlgorithmedPerformed interface
            matchAlgo.addListener(this);

            createProgressBar(imageA.getImageName(), matchAlgo);

            // Hide dialog
            setVisible(false);

            if (isRunInSeparateThread()) {

                // Start the thread as a low priority because we wish to still have user interface work fast.
                if (matchAlgo.startMethod(Thread.MIN_PRIORITY) == false) {
                    MipavUtil.displayError("A thread is already running on this object");
                }
            } else {
                matchAlgo.run();
            }
        } catch (OutOfMemoryError x) {
            System.gc();
            MipavUtil.displayError("Unable to allocate enough memory to run algorithms.");

            return;
        }
    }

    /**
     * Store the result image in the script runner's image table now that the action execution is finished.
     */
    protected void doPostAlgorithmActions() {

        if (getResultImageA() != null) {
            AlgorithmParameters.storeImageInRunner(getResultImageA());
        }

        if (getResultImageB() != null) {
            AlgorithmParameters.storeImageInRunner(getResultImageB());
        }
    }

    /**
     * {@inheritDoc}
     */
    protected void setGUIFromParams() {
        imageA = scriptParameters.retrieveInputImage(1);
        imageB = scriptParameters.retrieveInputImage(2);

        userInterface = ViewUserInterface.getReference();
        parentFrame = imageA.getParentFrame();

        doOrigins = scriptParameters.getParams().getBoolean("do_match_origins");
        doDimensions = scriptParameters.getParams().getBoolean("do_match_dimensions");
    }

    /**
     * {@inheritDoc}
     */
    protected void storeParamsFromGUI() throws ParserException {
        scriptParameters.storeInputImage(imageA);
        scriptParameters.storeInputImage(imageB);

        scriptParameters.getParams().put(ParameterFactory.newParameter("do_match_origins", doOrigins));
        scriptParameters.getParams().put(ParameterFactory.newParameter("do_match_dimensions", doDimensions));

        if (getResultImageA() != null) {
            scriptParameters.storeImageInRecorder(getResultImageA());
        }

        if (getResultImageB() != null) {
            scriptParameters.storeImageInRecorder(getResultImageB());
        }
    }

    /**
     * Builds a list of images to operate on from the template image.
     *
     * @return  DOCUMENT ME!
     */
    private JComboBox buildComboBoxImage() {
        JComboBox comboBoxImage = new JComboBox();
        comboBoxImage.setFont(serif12);
        comboBoxImage.setBackground(Color.white);

        Enumeration<String> names = ViewUserInterface.getReference().getRegisteredImageNames();

        // Add images from user interface
        // Guaranteed to have at least one unique potential image B, because it's
        // tested for in ViewJFrameImage before this dialog is created.
        while (names.hasMoreElements()) {
            String name = names.nextElement();
            ModelImage img = ViewUserInterface.getReference().getRegisteredImageByName(name);

            if (ViewUserInterface.getReference().getFrameContainingImage(img) != null) {
                comboBoxImage.addItem(name);
            }
        }

        return comboBoxImage;
    }

    /**
     * Sets up the GUI (panels, buttons, etc) and displays it on the screen.
     */
    private void init() {
        setTitle("Match Images");

        JPanel inputPanel = new JPanel();
        inputPanel.setBorder(buildTitledBorder("Match image features:"));

        inputPanel.setLayout(new GridBagLayout());

        GridBagConstraints gbc = new GridBagConstraints();
        gbc.anchor = GridBagConstraints.WEST;
        gbc.gridy = 1;
        gbc.gridwidth = 2;

        JLabel descript = new JLabel("This alorithm matches the orientations and " + "resolutions of two images.");
        inputPanel.add(descript, gbc);

        gbc.gridy = 2;
        gbc.gridwidth = 1;

        JLabel labelImageA = new JLabel("Image A:");
        inputPanel.add(labelImageA, gbc);

        gbc.gridwidth = GridBagConstraints.REMAINDER;
        comboBoxImageA = buildComboBoxImage();
        inputPanel.add(comboBoxImageA, gbc);
        inputPanel.add(Box.createHorizontalStrut(10), gbc);

        String name = (String) userInterface.getActiveImageFrame().getComponentImage().getActiveImage().getImageName();
        comboBoxImageA.setSelectedItem(name);

        gbc.gridy = 3;
        gbc.gridwidth = 1;

        JLabel labelImageB = new JLabel("Image B: ");
        inputPanel.add(labelImageB, gbc);

        gbc.gridwidth = GridBagConstraints.REMAINDER;
        comboBoxImageB = buildComboBoxImage();
        inputPanel.add(comboBoxImageB, gbc);
        inputPanel.add(Box.createHorizontalStrut(10), gbc);

        checkOrigins = new JCheckBox("Match image origins too.");
        checkDimensions = new JCheckBox("Match image dimensions too.");
        checkOrigins.setSelected(false);
        checkDimensions.setSelected(false);
        checkOrigins.setEnabled(true);
        checkDimensions.setEnabled(true);
        checkOrigins.setAlignmentX(Component.LEFT_ALIGNMENT);
        checkDimensions.setAlignmentX(Component.LEFT_ALIGNMENT);
        gbc.gridy = 4;
        inputPanel.add(checkOrigins, gbc);
        gbc.gridy = 5;
        inputPanel.add(checkDimensions, gbc);

        JLabel padLabel = new JLabel("Intensity value for padding ");
        padValTxt = new JTextField(String.valueOf(padValue), 4);
        gbc.gridy = 6;
        gbc.gridwidth = 1;
        inputPanel.add(padLabel, gbc);
        gbc.gridwidth = GridBagConstraints.REMAINDER;
        inputPanel.add(padValTxt, gbc);

        JPanel buttonPanel = new JPanel();
        buttonPanel.add(buildButtons());

        JPanel mainPanel = new JPanel(new BorderLayout());
        mainPanel.add(inputPanel, BorderLayout.NORTH);
        mainPanel.add(buttonPanel, BorderLayout.SOUTH);

        mainPanel.setBorder(BorderFactory.createEmptyBorder(5, 5, 5, 5));
        getContentPane().add(mainPanel);
        pack();
        setVisible(true);
    }

    /**
     * Use the GUI results to set up the variables needed to run the algorithm.
     *
     * @return  <code>true</code> if parameters set successfully, <code>false</code> otherwise.
     */
    private boolean setVariables() {
        selectedNameA = (String) comboBoxImageA.getSelectedItem();
        selectedNameB = (String) comboBoxImageB.getSelectedItem();
        imageA = ViewUserInterface.getReference().getRegisteredImageByName(selectedNameA);
        imageB = ViewUserInterface.getReference().getRegisteredImageByName(selectedNameB);
        doOrigins = checkOrigins.isSelected();
        doDimensions = checkDimensions.isSelected();

        String tmpStr;
        tmpStr = padValTxt.getText();

        if (testParameter(tmpStr, 0, 255)) {
            padValue = Integer.valueOf(tmpStr).intValue();
        } else {
            MipavUtil.displayError("Padding intensity must be between 0 and 255.");
            padValue = 0;
        }

        return true;
    }

}
