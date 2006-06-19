package gov.nih.mipav.view.dialogs;


import gov.nih.mipav.model.algorithms.*;
import gov.nih.mipav.model.algorithms.filters.*;
import gov.nih.mipav.model.structures.*;

import gov.nih.mipav.view.*;

import java.awt.*;
import java.awt.event.*;

import java.util.*;

import javax.swing.*;


/**
 * Dialog to get user input, then call the algorithm. The user has the option to generate a new image or replace the
 * source image. It should be noted that the algorithms are executed in their own threads. Note that a filter and its
 * mirror image reflected across the u and v frequency axes produce identical frequency responses.
 *
 * @version  March 21, 2006
 * @see      AlgorithmFrequencyFilter
 */
public class JDialogGaborFilter extends JDialogBase implements AlgorithmInterface, ScriptableInterface, ItemListener {

    //~ Static fields/initializers -------------------------------------------------------------------------------------

    /** Use serialVersionUID for interoperability. */
    private static final long serialVersionUID = -7693130193527088045L;

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** DOCUMENT ME! */
    private boolean createGabor = true;

    /** DOCUMENT ME! */
    private JCheckBox createGaborCheckBox;

    /** DOCUMENT ME! */
    private ButtonGroup destinationGroup;

    /** DOCUMENT ME! */
    private JPanel destinationPanel;

    /** DOCUMENT ME! */
    private int displayLoc; // Flag indicating if a new image is to be generated

    /** DOCUMENT ME! */
    private JPanel filterPanel;

    /** DOCUMENT ME! */
    private float freqU;

    /** DOCUMENT ME! */
    private AlgorithmFrequencyFilter FrequencyFilterAlgo = null;

    /** DOCUMENT ME! */
    private float freqV;

    /** DOCUMENT ME! */
    private ModelImage image; // source image

    /** DOCUMENT ME! */
    private JLabel labelFU;

    /** DOCUMENT ME! */
    private JLabel labelFV;

    /** DOCUMENT ME! */
    private JLabel labelSU;

    /** DOCUMENT ME! */
    private JLabel labelSV;

    /** DOCUMENT ME! */
    private JLabel labelTheta;

    /** DOCUMENT ME! */
    private JPanel mainPanel;

    /** DOCUMENT ME! */
    private JRadioButton newImage;

    /** DOCUMENT ME! */
    private JRadioButton replaceImage;

    /** DOCUMENT ME! */
    private ModelImage resultImage = null; // result image

    /** DOCUMENT ME! */
    private float sigmaU;

    /** DOCUMENT ME! */
    private float sigmaV;

    /** DOCUMENT ME! */
    private JTextField textFU;

    /** DOCUMENT ME! */
    private JTextField textFV;

    /** DOCUMENT ME! */
    private JTextField textSU;

    /** DOCUMENT ME! */
    private JTextField textSV;

    /** DOCUMENT ME! */
    private JTextField textTheta;

    /** DOCUMENT ME! */
    private float theta;

    /** DOCUMENT ME! */
    private String[] titles;

    /** DOCUMENT ME! */
    private ViewUserInterface userInterface;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Empty constructor needed for dynamic instantiation (used during scripting).
     */
    public JDialogGaborFilter() { }

    // or if the source image is to be replaced
    /**
     * Creates a new JDialogGaborFilter object.
     *
     * @param  theParentFrame  Parent frame.
     * @param  im              Source image.
     */
    public JDialogGaborFilter(Frame theParentFrame, ModelImage im) {
        super(theParentFrame, false);
        image = im;
        userInterface = ((ViewJFrameBase) parentFrame).getUserInterface();
        init();
    }

    /**
     * Used primarily for the script to store variables and run the algorithm. No actual dialog will appear but the set
     * up info and result image will be stored here.
     *
     * @param  UI  The user interface, needed to create the image frame.
     * @param  im  Source image.
     */
    public JDialogGaborFilter(ViewUserInterface UI, ModelImage im) {
        super();
        userInterface = UI;
        image = im;
        parentFrame = image.getParentFrame();
    }

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * Closes dialog box when the OK button is pressed and calls the algorithm.
     *
     * @param  event  Event that triggers function.
     */
    public void actionPerformed(ActionEvent event) {
        String command = event.getActionCommand();
        Object source = event.getSource();

        if (command.equals("OK")) {

            if (setVariables()) {
                callAlgorithm();
            }
        } else if (command.equals("Cancel")) {
            dispose();
        } else if (source == helpButton) {
            MipavUtil.showHelp("");
        }
    }

    // ************************************************************************
    // ************************** Algorithm Events ****************************
    // ************************************************************************

    /**
     * This method is required if the AlgorithmPerformed interface is implemented. It is called by the algorithms when
     * it has completed or failed to to complete, so that the dialog can be display the result image and/or clean up.
     *
     * @param  algorithm  Algorithm that caused the event.
     */
    public void algorithmPerformed(AlgorithmBase algorithm) {

        ViewJFrameImage imageFrame = null;

        if (algorithm instanceof AlgorithmFrequencyFilter) {

            if ((algorithm.isCompleted() == true) && (resultImage != null)) {

                updateFileTypeInfo(image, resultImage, ModelStorageBase.FLOAT);

                // resultImage is the same or smaller than image.
                // The algorithm has completed and produced a new image to be displayed.
                try {

                    // resultImage.setImageName("Frequency Filtered image");
                    imageFrame = new ViewJFrameImage(resultImage, null, new Dimension(610, 200));
                } catch (OutOfMemoryError error) {
                    System.gc();
                    MipavUtil.displayError("Out of memory: unable to open new frame");
                }
            } else if (resultImage == null) {

                // These next lines set the titles in all frames where the source image is displayed to
                // image name so as to indicate that the image is now unlocked!
                // The image frames are enabled and then registed to the userinterface.
                Vector imageFrames = image.getImageFrameVector();

                for (int i = 0; i < imageFrames.size(); i++) {
                    ((ViewJFrameBase) (imageFrames.elementAt(i))).setTitle(titles[i]);
                    ((ViewJFrameBase) (imageFrames.elementAt(i))).setEnabled(true);

                    if (((Frame) (imageFrames.elementAt(i))) != parentFrame) {
                        ((ViewJFrameBase) parentFrame).getUserInterface().registerFrame((Frame) (imageFrames.elementAt(i)));
                    }
                }

                if (parentFrame != null) {
                    ((ViewJFrameBase) parentFrame).getUserInterface().registerFrame(parentFrame);
                    ((ViewJFrameImage) parentFrame).getComponentImage().setLogMagDisplay(true);
                }

                updateFileTypeInfo(image, ModelStorageBase.FLOAT);
                image.notifyImageDisplayListeners(null, true);
            } else if (resultImage != null) {

                // algorithm failed but result image still has garbage
                resultImage.disposeLocal(); // clean up memory
                resultImage = null;
            }

            insertScriptLine(algorithm);
        }

        FrequencyFilterAlgo.finalize();
        FrequencyFilterAlgo = null;
        dispose();
    }

    /**
     * Accessor that returns the image.
     *
     * @return  The result image.
     */
    public ModelImage getResultImage() {
        return resultImage;
    }

    /**
     * If a script is being recorded and the algorithm is done, add an entry for this algorithm.
     *
     * @param  algo  the algorithm to make an entry for
     */
    public void insertScriptLine(AlgorithmBase algo) {

        if (algo.isCompleted()) {

            if (userInterface.isScriptRecording()) {

                // check to see if the match image is already in the ImgTable
                if (userInterface.getScriptDialog().getImgTableVar(image.getImageName()) == null) {

                    if (userInterface.getScriptDialog().getActiveImgTableVar(image.getImageName()) == null) {
                        userInterface.getScriptDialog().putActiveVar(image.getImageName());
                    }
                }

                userInterface.getScriptDialog().append("GaborFilter " +
                                                       userInterface.getScriptDialog().getVar(image.getImageName()) +
                                                       " ");

                if (displayLoc == NEW) {
                    userInterface.getScriptDialog().putVar(resultImage.getImageName());
                    userInterface.getScriptDialog().append(userInterface.getScriptDialog().getVar(resultImage.getImageName()) +
                                                           " " + freqU + " " + freqV + " " + sigmaU + " " + sigmaV +
                                                           " " + theta + " " + createGabor + "\n");
                } else {
                    userInterface.getScriptDialog().append(userInterface.getScriptDialog().getVar(image.getImageName()) +
                                                           " " + freqU + " " + freqV + " " + sigmaU + " " + sigmaV +
                                                           " " + theta + " " + createGabor + "\n");
                }
            }
        }
    }

    /**
     * Run this algorithm from a script.
     *
     * @param   parser  the script parser we get the state from
     *
     * @throws  IllegalArgumentException  if there is something wrong with the arguments in the script
     */
    public void scriptRun(AlgorithmScriptParser parser) throws IllegalArgumentException {
        String srcImageKey = null;
        String destImageKey = null;

        try {
            srcImageKey = parser.getNextString();
        } catch (Exception e) {
            throw new IllegalArgumentException();
        }

        ModelImage im = parser.getImage(srcImageKey);

        image = im;
        userInterface = image.getUserInterface();
        parentFrame = image.getParentFrame();

        // the result image
        try {
            destImageKey = parser.getNextString();
        } catch (Exception e) {
            throw new IllegalArgumentException();
        }

        if (srcImageKey.equals(destImageKey)) {
            this.setDisplayLocReplace();
        } else {
            this.setDisplayLocNew();
        }

        try {
            setFreqU(parser.getNextFloat());
            setFreqV(parser.getNextFloat());
            setSigmaU(parser.getNextFloat());
            setSigmaV(parser.getNextFloat());
            setTheta(parser.getNextFloat());
            setCreateGabor(parser.getNextBoolean());
        } catch (Exception e) {
            throw new IllegalArgumentException();
        }

        setSeparateThread(false);
        callAlgorithm();

        if (!srcImageKey.equals(destImageKey)) {
            parser.putVariable(destImageKey, getResultImage().getImageName());
        }
    }

    /**
     * Accessor that determines whether or not a Gabor filter image is created.
     *
     * @param  createGabor  variable determining whether or not a Gabor filter image is created.
     */
    public void setCreateGabor(boolean createGabor) {
        this.createGabor = createGabor;
    }

    /**
     * Accessor that sets the display loc variable to new, so that a new image is created once the algorithm completes.
     */
    public void setDisplayLocNew() {
        displayLoc = NEW;
    }

    /**
     * Accessor that sets the display loc variable to replace, so the current image is replaced once the algorithm
     * completes.
     */
    public void setDisplayLocReplace() {
        displayLoc = REPLACE;
    }

    /**
     * Accessor that sets the prerotation horizontal frequency variable.
     *
     * @param  scale  Value to set prerotation horizontal frequency to.
     */
    public void setFreqU(float scale) {
        freqU = scale;
    }

    /**
     * Accessor that sets the prerotation vertical frequency variable.
     *
     * @param  scale  Value to set prerotation vertical frequency to.
     */
    public void setFreqV(float scale) {
        freqV = scale;
    }

    /**
     * Accessor that sets the prerotation horizontal standard deviation variable.
     *
     * @param  scale  Value to set prerotation horizontal standard deviation to.
     */
    public void setSigmaU(float scale) {
        sigmaU = scale;
    }

    /**
     * Accessor that sets the prerotation vertical standard deviation variable.
     *
     * @param  scale  Value to set prerotation vertical standard deviation to.
     */
    public void setSigmaV(float scale) {
        sigmaV = scale;
    }

    /**
     * Accessor that sets the rotation angle in degrees.
     *
     * @param  scale  Value to set rotation angle in degrees to.
     */
    public void setTheta(float scale) {
        theta = scale;
    }

    /**
     * Once all the necessary variables are set, call the Frequency Filter algorithm based on what type of image this is
     * and whether or not there is a separate destination image.
     */
    private void callAlgorithm() {
        String name = makeImageName(image.getImageName(), "_gaborFilter");

        if (displayLoc == NEW) {

            try {
                resultImage = (ModelImage) image.clone();
                resultImage.setImageName(name);
                resultImage.resetVOIs();

                // Make algorithm
                FrequencyFilterAlgo = new AlgorithmFrequencyFilter(resultImage, image, freqU, freqV, sigmaU, sigmaV,
                                                                   theta, createGabor);

                // This is very important. Adding this object as a listener allows the algorithm to
                // notify this object when it has completed or failed. See algorithm performed event.
                // This is made possible by implementing AlgorithmedPerformed interface
                FrequencyFilterAlgo.addListener(this);

                // Hide dialog since the algorithm is about to run
                setVisible(false);

                if (isRunInSeparateThread()) {

                    // Start the thread as a low priority because we wish to still have user interface work fast.
                    if (FrequencyFilterAlgo.startMethod(Thread.MIN_PRIORITY) == false) {
                        MipavUtil.displayError("A thread is already running on this object");
                    }
                } else {
                    if (!userInterface.isAppFrameVisible()) {
                        FrequencyFilterAlgo.setProgressBarVisible(false);
                    }

                    FrequencyFilterAlgo.run();
                }
            } catch (OutOfMemoryError x) {
                MipavUtil.displayError("Dialog GaborFilter: unable to allocate enough memory");

                if (resultImage != null) {
                    resultImage.disposeLocal(); // Clean up memory of result image
                    resultImage = null;
                }

                return;
            }
        } else {

            try {

                // No need to make new image space because the user has choosen to replace the source image
                // Make the algorithm class
                FrequencyFilterAlgo = new AlgorithmFrequencyFilter(image, freqU, freqV, sigmaU, sigmaV, theta,
                                                                   createGabor);

                // This is very important. Adding this object as a listener allows the algorithm to
                // notify this object when it has completed or failed. See algorithm performed event.
                // This is made possible by implementing AlgorithmedPerformed interface
                FrequencyFilterAlgo.addListener(this);

                // Hide the dialog since the algorithm is about to run.
                setVisible(false);

                // These next lines set the titles in all frames where the source image is displayed to
                // "locked - " image name so as to indicate that the image is now read/write locked!
                // The image frames are disabled and then unregisted from the userinterface until the
                // algorithm has completed.
                Vector imageFrames = image.getImageFrameVector();
                titles = new String[imageFrames.size()];

                for (int i = 0; i < imageFrames.size(); i++) {
                    titles[i] = ((ViewJFrameBase) (imageFrames.elementAt(i))).getTitle();
                    ((ViewJFrameBase) (imageFrames.elementAt(i))).setTitle("Locked: " + titles[i]);
                    ((ViewJFrameBase) (imageFrames.elementAt(i))).setEnabled(false);
                    userInterface.unregisterFrame((Frame) (imageFrames.elementAt(i)));
                }

                if (isRunInSeparateThread()) {

                    // Start the thread as a low priority because we wish to still have user interface work fast.
                    if (FrequencyFilterAlgo.startMethod(Thread.MIN_PRIORITY) == false) {
                        MipavUtil.displayError("A thread is already running on this object");
                    }
                } else {
                    if (!userInterface.isAppFrameVisible()) {
                        FrequencyFilterAlgo.setProgressBarVisible(false);
                    }

                    FrequencyFilterAlgo.run();
                }
            } catch (OutOfMemoryError x) {
                MipavUtil.displayError("Dialog GaborFilter: unable to allocate enough memory");

                return;
            }
        }
    }

    /**
     * Sets up the GUI (panels, buttons, etc) and displays it on the screen.
     */
    private void init() {
        setTitle("Gabor Filter");

        GridBagConstraints gbc = new GridBagConstraints();
        gbc.gridwidth = 3;
        gbc.anchor = gbc.WEST;
        gbc.weightx = 1;

        filterPanel = new JPanel(new GridBagLayout());
        filterPanel.setBorder(buildTitledBorder("Frequency domain filter specifications"));

        labelFU = new JLabel("Prerotation horizontal frequency -1.0 to 1.0 ");
        labelFU.setForeground(Color.black);
        labelFU.setFont(serif12);
        labelFU.setEnabled(true);

        textFU = new JTextField(10);
        freqU = image.getFreqU();
        textFU.setText(String.valueOf(freqU));
        textFU.setFont(serif12);
        textFU.setEnabled(true);

        labelFV = new JLabel("Prerotation vertical frequency -1.0 to 1.0 ");
        labelFV.setForeground(Color.black);
        labelFV.setFont(serif12);
        labelFV.setEnabled(true);

        textFV = new JTextField(10);
        freqV = image.getFreqV();
        textFV.setText(String.valueOf(freqV));
        textFV.setFont(serif12);
        textFV.setEnabled(true);

        labelSU = new JLabel("Prerotation horizontal standard deviation > 0.0 ");
        labelSU.setForeground(Color.black);
        labelSU.setFont(serif12);
        labelSU.setEnabled(true);

        textSU = new JTextField(10);
        sigmaU = image.getSigmaU();
        textSU.setText(String.valueOf(sigmaU));
        textSU.setFont(serif12);
        textSU.setEnabled(true);

        labelSV = new JLabel("Prerotation vertical standard deviation > 0.0 ");
        labelSV.setForeground(Color.black);
        labelSV.setFont(serif12);
        labelSV.setEnabled(true);

        textSV = new JTextField(10);
        sigmaV = image.getSigmaV();
        textSV.setText(String.valueOf(sigmaV));
        textSV.setFont(serif12);
        textSV.setEnabled(true);

        labelTheta = new JLabel("Rotation angle in degrees ");
        labelTheta.setForeground(Color.black);
        labelTheta.setFont(serif12);
        labelTheta.setEnabled(true);

        textTheta = new JTextField(10);
        theta = (float) (image.getTheta() * 180.0 / Math.PI);
        textTheta.setText(String.valueOf(theta));
        textTheta.setFont(serif12);
        textTheta.setEnabled(true);

        createGaborCheckBox = new JCheckBox("Create an image of the Gabor filter");
        createGaborCheckBox.setFont(serif12);
        createGaborCheckBox.setForeground(Color.black);
        createGaborCheckBox.setSelected(true);
        createGaborCheckBox.setEnabled(true);

        gbc.gridx = 0;
        gbc.gridy = 0;
        gbc.gridwidth = 1;
        gbc.weightx = .5;
        gbc.fill = gbc.HORIZONTAL;
        filterPanel.add(labelFU, gbc);
        gbc.gridx = 1;
        filterPanel.add(textFU, gbc);
        gbc.gridx = 0;
        gbc.gridy = 1;
        filterPanel.add(labelFV, gbc);
        gbc.gridx = 1;
        filterPanel.add(textFV, gbc);
        gbc.gridx = 0;
        gbc.gridy = 2;
        filterPanel.add(labelSU, gbc);
        gbc.gridx = 1;
        filterPanel.add(textSU, gbc);
        gbc.gridx = 0;
        gbc.gridy = 3;
        filterPanel.add(labelSV, gbc);
        gbc.gridx = 1;
        filterPanel.add(textSV, gbc);
        gbc.gridx = 0;
        gbc.gridy = 4;
        filterPanel.add(labelTheta, gbc);
        gbc.gridx = 1;
        filterPanel.add(textTheta, gbc);
        gbc.gridx = 0;
        gbc.gridy = 5;
        filterPanel.add(createGaborCheckBox, gbc);

        destinationPanel = new JPanel(new GridLayout(2, 1));
        destinationPanel.setForeground(Color.black);
        destinationPanel.setBorder(buildTitledBorder("Destination"));

        destinationGroup = new ButtonGroup();
        newImage = new JRadioButton("New image", true);
        newImage.setFont(serif12);
        newImage.setForeground(Color.black);
        destinationGroup.add(newImage);
        destinationPanel.add(newImage);

        replaceImage = new JRadioButton("Replace image", false);
        replaceImage.setFont(serif12);
        replaceImage.setForeground(Color.black);
        destinationGroup.add(replaceImage);
        destinationPanel.add(replaceImage);

        // Only if the image is unlocked can it be replaced.
        if (image.getLockStatus() == ModelStorageBase.UNLOCKED) {
            replaceImage.setEnabled(true);
        } else {
            replaceImage.setEnabled(false);
        }

        mainPanel = new JPanel(new GridBagLayout());
        gbc.gridx = 0;
        gbc.gridy = 0;
        gbc.fill = gbc.HORIZONTAL;
        mainPanel.add(filterPanel, gbc);
        gbc.gridy = 1;
        mainPanel.add(destinationPanel, gbc);

        getContentPane().add(mainPanel);
        getContentPane().add(buildButtons(), BorderLayout.SOUTH);
        pack();
        setVisible(true);
    }

    /**
     * Use the GUI results to set up the variables needed to run the algorithm.
     *
     * @return  <code>true</code> if parameters set successfully, <code>false</code> otherwise.
     */
    private boolean setVariables() {
        String tmpStr;

        if (replaceImage.isSelected()) {
            displayLoc = REPLACE;
        } else if (newImage.isSelected()) {
            displayLoc = NEW;
        }

        tmpStr = textFU.getText();

        if (testParameter(tmpStr, -1.0, 1.0)) {
            freqU = Float.valueOf(tmpStr).floatValue();
        } else {
            MipavUtil.displayError("Horizontal frequency must be between -1.0 and 1.0");
            textFU.requestFocus();
            textFU.selectAll();

            return false;
        }

        tmpStr = textFV.getText();

        if (testParameter(tmpStr, -1.0, 1.0)) {
            freqV = Float.valueOf(tmpStr).floatValue();
        } else {
            MipavUtil.displayError("Vertical frequency must be between -1.0 and 1.0");
            textFV.requestFocus();
            textFV.selectAll();

            return false;
        }

        tmpStr = textSU.getText();

        if (testParameter(tmpStr, 1.0E-6, 1.0E6)) {
            sigmaU = Float.valueOf(tmpStr).floatValue();
        } else {
            MipavUtil.displayError("Horizontal standard deviation must be between 1.0E-6 and 1.0e6");
            textSU.requestFocus();
            textSU.selectAll();

            return false;
        }

        tmpStr = textSV.getText();

        if (testParameter(tmpStr, 1.0E-6, 1.0E6)) {
            sigmaV = Float.valueOf(tmpStr).floatValue();
        } else {
            MipavUtil.displayError("Vertical standard deviation must be between 1.0E-6 and 1.0E6");
            textSV.requestFocus();
            textSV.selectAll();

            return false;
        }

        tmpStr = textTheta.getText();
        theta = (float) (Float.valueOf(tmpStr).floatValue() * Math.PI / 180.0);

        createGabor = createGaborCheckBox.isSelected();

        return true;
    }
}
