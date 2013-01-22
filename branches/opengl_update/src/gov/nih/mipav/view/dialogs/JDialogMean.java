package gov.nih.mipav.view.dialogs;


import gov.nih.mipav.model.algorithms.*;
import gov.nih.mipav.model.algorithms.filters.*;
import gov.nih.mipav.model.file.*;
import gov.nih.mipav.model.scripting.*;
import gov.nih.mipav.model.scripting.parameters.*;
import gov.nih.mipav.model.structures.*;

import gov.nih.mipav.view.*;
import gov.nih.mipav.view.components.*;

import java.awt.*;
import java.awt.event.*;

import java.util.*;

import javax.swing.*;


/**
 * Dialog to get user input, then call the algorithm. The user has the option to generate a new image or replace the
 * source image. In addition the user can select having the algorithm applied to whole image or to the VOI regions. It
 * should be noted that the algorithms are executed in their own thread.
 */
public class JDialogMean extends JDialogScriptableBase implements AlgorithmInterface, ActionDiscovery, ScriptableActionInterface {

    //~ Static fields/initializers -------------------------------------------------------------------------------------

    /** Use serialVersionUID for interoperability. */
    private static final long serialVersionUID = -5373013772234532691L;

    /** DOCUMENT ME! */
    public static final int SEPARATE_COMPONENT = 1;

    /** DOCUMENT ME! */
    public static final int ADAPTIVE_VECTOR = 2;

    /** DOCUMENT ME! */
    public static final int PARALLEL_VECTOR = 3;

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** DOCUMENT ME! */
    private JRadioButton adaptiveVectorButton;

    /** DOCUMENT ME! */
    private boolean blue;

    /** DOCUMENT ME! */
    private JCheckBox blueChannel;

    /** DOCUMENT ME! */
    private JLabel blueLabel;

    /** DOCUMENT ME! */
    private JTextField blueText;

    /** DOCUMENT ME! */
    private int blueVector;

    /** DOCUMENT ME! */
    private JRadioButton bySlice; // allows selection of processing each slice of a 3D image-set individually

    /** DOCUMENT ME! */
    private JComboBox comboBoxKernelSize;

    /** DOCUMENT ME! */
    private int filterType = SEPARATE_COMPONENT;

    /** DOCUMENT ME! */
    private boolean green;

    /** DOCUMENT ME! */
    private JCheckBox greenChannel;

    /** DOCUMENT ME! */
    private JLabel greenLabel;

    /** DOCUMENT ME! */
    private JTextField greenText;

    /** DOCUMENT ME! */
    private int greenVector;

    /** DOCUMENT ME! */
    private ModelImage image; // source image

    /** false = apply algorithm only to VOI regions. */
    private boolean image25D = false; // flag indicating if slices should be processed independently

    /** DOCUMENT ME! */
    private int kernelSize;

    /** DOCUMENT ME! */
    private AlgorithmMean meanAlgo = null;

    /** DOCUMENT ME! */
    private JPanelAlgorithmOutputOptions outputPanel;

    /** DOCUMENT ME! */
    private JRadioButton parallelVectorButton;

    /** DOCUMENT ME! */
    private boolean red;

    /** DOCUMENT ME! */
    private JCheckBox redChannel;

    /** DOCUMENT ME! */
    private JLabel redLabel;

    /** DOCUMENT ME! */
    private JTextField redText;

    /** DOCUMENT ME! */
    private int redVector;

    /** DOCUMENT ME! */
    private ModelImage resultImage = null; // result image

    /** DOCUMENT ME! */
    private JRadioButton separateButton;

    /** DOCUMENT ME! */
    private ButtonGroup separateVectorGroup;

    /** DOCUMENT ME! */
    private String[] titles;

    /** DOCUMENT ME! */
    private ViewUserInterface userInterface;

    /** DOCUMENT ME! */
    private JRadioButton wholeVolume;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Empty constructor needed for dynamic instantiation (used during scripting).
     */
    public JDialogMean() { }

    /**
     * Creates a new JDialogMean object.
     *
     * @param  theParentFrame  Parent frame.
     * @param  im              Source image.
     */
    public JDialogMean(Frame theParentFrame, ModelImage im) {
        super(theParentFrame, false);

        if (im.getType() == ModelImage.BOOLEAN) {
            MipavUtil.displayError("Source Image must NOT be Boolean");
            dispose();

            return;
        }

        image = im;
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
        Object source = event.getSource();

        if (command.equals("OK")) {

            if (setVariables()) {
                callAlgorithm();
            }
        } else if (command.equals("Volume")) {

            // kernel Size
            int indx = comboBoxKernelSize.getSelectedIndex(); // get the current combo-box selection
            comboBoxKernelSize.removeAllItems();
            buildKernelSizeComboBox(false);
            comboBoxKernelSize.setSelectedIndex(indx); // set the new combo-box to the old selection
            pack();
        } else if (command.equals("Slice")) {

            // kernel size
            int indx = comboBoxKernelSize.getSelectedIndex(); // get the current combo-box selection
            comboBoxKernelSize.removeAllItems();
            buildKernelSizeComboBox(true);
            comboBoxKernelSize.setSelectedIndex(indx); // set the new combo-box to the old selection
        } else if (command.equals("Help")) {
            //MipavUtil.showHelp("10015");
            MipavUtil.showWebHelp("Filters_(Spatial):_Mean#Applying_the_Mean_algorithm");
        } else if (command.equals("Cancel")) {
            dispose();
        } else if ((source == separateButton) || (source == adaptiveVectorButton) || (source == parallelVectorButton)) {

            if (separateButton.isSelected()) {
                redChannel.setEnabled(true);
                greenChannel.setEnabled(true);
                blueChannel.setEnabled(true);
                redLabel.setEnabled(false);
                redText.setEnabled(false);
                greenLabel.setEnabled(false);
                greenText.setEnabled(false);
                blueLabel.setEnabled(false);
                blueText.setEnabled(false);
            } else if (adaptiveVectorButton.isSelected()) {
                redChannel.setEnabled(false);
                greenChannel.setEnabled(false);
                blueChannel.setEnabled(false);
                redLabel.setEnabled(false);
                redText.setEnabled(false);
                greenLabel.setEnabled(false);
                greenText.setEnabled(false);
                blueLabel.setEnabled(false);
                blueText.setEnabled(false);
            } else { // parallelVectorButton.isSelected()
                redChannel.setEnabled(false);
                greenChannel.setEnabled(false);
                blueChannel.setEnabled(false);
                redLabel.setEnabled(true);
                redText.setEnabled(true);
                greenLabel.setEnabled(true);
                greenText.setEnabled(true);
                blueLabel.setEnabled(true);
                blueText.setEnabled(true);
            }
        } else {
            super.actionPerformed(event);
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

        if (algorithm instanceof AlgorithmMean) {
            image.clearMask();

            if ((meanAlgo.isCompleted() == true) && (resultImage != null)) {
                // The algorithm has completed and produced a new image to be displayed.

                updateFileInfo(image, resultImage);
                resultImage.clearMask();

                try {

                    // resultImage.setImageName("Mean: "+image.getImageName());
                    new ViewJFrameImage(resultImage, null, new Dimension(610, 200));
                } catch (OutOfMemoryError error) {
                    System.gc();
                    MipavUtil.displayError("Out of memory: unable to open new frame");
                }
            } else if (resultImage == null) {

                // These next lines set the titles in all frames where the source image is displayed to
                // image name so as to indicate that the image is now unlocked!
                // The image frames are enabled and then registered to the userinterface.
                Vector<ViewImageUpdateInterface> imageFrames = image.getImageFrameVector();

                for (int i = 0; i < imageFrames.size(); i++) {
                    ((Frame) (imageFrames.elementAt(i))).setTitle(titles[i]);
                    ((Frame) (imageFrames.elementAt(i))).setEnabled(true);

                    if (((Frame) (imageFrames.elementAt(i))) != parentFrame) {
                        userInterface.registerFrame((Frame) (imageFrames.elementAt(i)));
                    }
                }

                if (parentFrame != null) {
                    userInterface.registerFrame(parentFrame);
                }

                image.notifyImageDisplayListeners(null, true);
            } else if (resultImage != null) {

                // algorithm failed but result image still has garbage
                resultImage.disposeLocal(); // clean up memory
                resultImage = null;
                System.gc();
            }
        }

        if (algorithm.isCompleted()) {
            insertScriptLine();
        }
     // save the completion status for later
        setComplete(algorithm.isCompleted());

        meanAlgo.finalize();
        meanAlgo = null;
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
     * Accessor that sets the color flag.
     *
     * @param  flag  <code>true</code> indicates ARG image, blue.
     */
    public void setBlue(boolean flag) {
        blue = flag;
    }

    /**
     * DOCUMENT ME!
     *
     * @param  blueVector  DOCUMENT ME!
     */
    public void setBlueVector(int blueVector) {
        this.blueVector = blueVector;
    }

    /**
     * DOCUMENT ME!
     *
     * @param  filterType  SEPARATE_COMPONENT, ADAPTIVE_VECTOR, or PARALLEL_VECTOR
     */
    public void setFilterType(int filterType) {
        this.filterType = filterType;
    }

    /**
     * Accessor that sets the color flag.
     *
     * @param  flag  <code>true</code> indicates ARG image, green.
     */
    public void setGreen(boolean flag) {
        green = flag;
    }

    /**
     * DOCUMENT ME!
     *
     * @param  greenVector  DOCUMENT ME!
     */
    public void setGreenVector(int greenVector) {
        this.greenVector = greenVector;
    }

    /**
     * Accessor that sets the slicing flag.
     *
     * @param  flag  <code>true</code> indicates slices should be processed independently.
     */
    public void setImage25D(boolean flag) {
        image25D = flag;
    }

    /**
     * Accessor that sets the kernel size.
     *
     * @param  size  Value to set size to (3 == 3x3, 5 == 5x5, etc.)
     */
    public void setKernelSize(int size) {
        kernelSize = size;
    }

    /**
     * Accessor that sets the color flag.
     *
     * @param  flag  <code>true</code> indicates ARG image, red.
     */
    public void setRed(boolean flag) {
        red = flag;
    }

    /**
     * DOCUMENT ME!
     *
     * @param  redVector  DOCUMENT ME!
     */
    public void setRedVector(int redVector) {
        this.redVector = redVector;
    }

    /**
     * Once all the necessary variables are set, call the mean algorithm based on what type of image this is and whether
     * or not there is a separate destination image.
     */
    protected void callAlgorithm() {
        String name = makeImageName(image.getImageName(), "_mean");

        // stuff to do when working on 2-D images.
        if (image.getNDims() == 2) { // source image is 2D

            int[] destExtents = new int[2];
            destExtents[0] = image.getExtents()[0]; // X dim
            destExtents[1] = image.getExtents()[1]; // Y dim

            if (outputPanel.isOutputNewImageSet()) { // (2D)

                try {

                    // Make result image of float type
                    // resultImage     = new ModelImage(image.getType(), destExtents, name, userInterface);
                    resultImage = (ModelImage) image.clone();
                    resultImage.setImageName(name);

                    if ((resultImage.getFileInfo()[0]).getFileFormat() == FileUtility.DICOM) {
                        ((FileInfoDicom) (resultImage.getFileInfo(0))).setSecondaryCaptureTags();
                    }

                    // Make algorithm
                    meanAlgo = new AlgorithmMean(resultImage, image, kernelSize, outputPanel.isProcessWholeImageSet());

                    // only if the src image is colour will any channel checkboxes be enabled
                    meanAlgo.setRGBChannelFilter(filterType, red, green, blue, redVector, greenVector, blueVector);

                    // This is very important. Adding this object as a listener allows the algorithm to
                    // notify this object when it has completed or failed. See algorithm performed event.
                    // This is made possible by implementing AlgorithmedPerformed interface
                    meanAlgo.addListener(this);

                    createProgressBar(image.getImageName(), meanAlgo);

                    setVisible(false); // Hide dialog

                    if (isRunInSeparateThread()) {

                        // Start the thread as a low priority because we wish to still have user interface work fast.
                        if (meanAlgo.startMethod(Thread.MIN_PRIORITY) == false) {
                            MipavUtil.displayError("A thread is already running on this object");
                        }
                    } else {
                        meanAlgo.run();
                    }
                } catch (OutOfMemoryError x) {
                    MipavUtil.displayError("Dialog mean: unable to allocate enough memory");

                    if (resultImage != null) {
                        resultImage.disposeLocal(); // Clean up memory of result image
                        resultImage = null;
                    }

                    return;
                }
            } else { // displayLoc == REPLACE        (2D)

                try {

                    // No need to make new image space because the user has choosen to replace the source image
                    // Make the algorithm class
                    meanAlgo = new AlgorithmMean(image, kernelSize, outputPanel.isProcessWholeImageSet());

                    // only if the src image is colour will any channel checkboxes be enabled
                    meanAlgo.setRGBChannelFilter(filterType, red, green, blue, redVector, greenVector, blueVector);

                    // This is very important. Adding this object as a listener allows the algorithm to
                    // notify this object when it has completed or failed. See algorithm performed event.
                    // This is made possible by implementing AlgorithmedPerformed interface
                    meanAlgo.addListener(this);
                    createProgressBar(image.getImageName(), meanAlgo);

                    // Hide the dialog since the algorithm is about to run.
                    setVisible(false);

                    // These next lines set the titles in all frames where the source image is displayed to
                    // "locked - " image name so as to indicate that the image is now read/write locked!
                    // The image frames are disabled and then unregisted from the userinterface until the
                    // algorithm has completed.
                    Vector<ViewImageUpdateInterface> imageFrames = image.getImageFrameVector();
                    titles = new String[imageFrames.size()];

                    for (int i = 0; i < imageFrames.size(); i++) {
                        titles[i] = ((Frame) (imageFrames.elementAt(i))).getTitle();
                        ((Frame) (imageFrames.elementAt(i))).setTitle("Locked: " + titles[i]);
                        ((Frame) (imageFrames.elementAt(i))).setEnabled(false);
                        userInterface.unregisterFrame((Frame) (imageFrames.elementAt(i)));
                    }

                    if (isRunInSeparateThread()) {

                        // Start the thread as a low priority because we wish to still have user interface work fast.
                        if (meanAlgo.startMethod(Thread.MIN_PRIORITY) == false) {
                            MipavUtil.displayError("A thread is already running on this object");
                        }
                    } else {
                        meanAlgo.run();
                    }
                } catch (OutOfMemoryError x) {
                    MipavUtil.displayError("Dialog mean: unable to allocate enough memory");

                    return;
                }
            }
        } else if (image.getNDims() == 3) {
            int[] destExtents = new int[3];
            destExtents[0] = image.getExtents()[0];
            destExtents[1] = image.getExtents()[1];
            destExtents[2] = image.getExtents()[2];

            if (outputPanel.isOutputNewImageSet()) { // (3D)

                try {

                    // Make result image of float type
                    // resultImage     = new ModelImage(image.getType(), destExtents, name, userInterface);
                    resultImage = (ModelImage) image.clone();
                    resultImage.setImageName(name);

                    if ((resultImage.getFileInfo()[0]).getFileFormat() == FileUtility.DICOM) {

                        for (int i = 0; i < resultImage.getExtents()[2]; i++) {
                            ((FileInfoDicom) (resultImage.getFileInfo(i))).setSecondaryCaptureTags();
                        }
                    }

                    // Make algorithm
                    meanAlgo = new AlgorithmMean(resultImage, image, kernelSize, image25D,
                                                 outputPanel.isProcessWholeImageSet());

                    // only if the src image is colour will any channel checkboxes be enabled
                    meanAlgo.setRGBChannelFilter(filterType, red, green, blue, redVector, greenVector, blueVector);

                    // This is very important. Adding this object as a listener allows the algorithm to
                    // notify this object when it has completed or failed. See algorithm performed event.
                    // This is made possible by implementing AlgorithmedPerformed interface
                    meanAlgo.addListener(this);
                    createProgressBar(image.getImageName(), meanAlgo);

                    setVisible(false); // Hide dialog

                    if (isRunInSeparateThread()) {

                        // Start the thread as a low priority because we wish to still have user interface work fast.
                        if (meanAlgo.startMethod(Thread.MIN_PRIORITY) == false) {
                            MipavUtil.displayError("A thread is already running on this object");
                        }
                    } else {
                        meanAlgo.run();
                    }
                } catch (OutOfMemoryError x) {
                    MipavUtil.displayError("Dialog mean: unable to allocate enough memory");

                    if (resultImage != null) {
                        resultImage.disposeLocal(); // Clean up image memory
                        resultImage = null;
                    }

                    return;
                }
            } else { // displayLoc == REPLACE         (3D)

                try {

                    // Make algorithm
                    meanAlgo = new AlgorithmMean(image, kernelSize, image25D, outputPanel.isProcessWholeImageSet());

                    // only if the src image is colour will any channel checkboxes be enabled
                    meanAlgo.setRGBChannelFilter(filterType, red, green, blue, redVector, greenVector, blueVector);

                    // This is very important. Adding this object as a listener allows the algorithm to
                    // notify this object when it has completed or failed. See algorithm performed event.
                    // This is made possible by implementing AlgorithmedPerformed interface
                    meanAlgo.addListener(this);
                    createProgressBar(image.getImageName(), meanAlgo);

                    // Hide dialog
                    setVisible(false);

                    // These next lines set the titles in all frames where the source image is displayed to
                    // "locked - " image name so as to indicate that the image is now read/write locked!
                    // The image frames are disabled and then unregisted from the userinterface until the
                    // algorithm has completed.
                    Vector<ViewImageUpdateInterface> imageFrames = image.getImageFrameVector();
                    titles = new String[imageFrames.size()];

                    for (int i = 0; i < imageFrames.size(); i++) {
                        titles[i] = ((Frame) (imageFrames.elementAt(i))).getTitle();
                        ((Frame) (imageFrames.elementAt(i))).setTitle("Locked: " + titles[i]);
                        ((Frame) (imageFrames.elementAt(i))).setEnabled(false);
                        userInterface.unregisterFrame((Frame) (imageFrames.elementAt(i)));
                    }

                    if (isRunInSeparateThread()) {

                        // Start the thread as a low priority because we wish to still have user interface work fast.
                        if (meanAlgo.startMethod(Thread.MIN_PRIORITY) == false) {
                            MipavUtil.displayError("A thread is already running on this object");
                        }
                    } else {

                        meanAlgo.run();
                    }
                } catch (OutOfMemoryError x) {
                    MipavUtil.displayError("Dialog mean: unable to allocate enough memory");

                    return;
                }
            }
        }
    }

    /**
     * Store the result image in the script runner's image table now that the action execution is finished.
     */
    protected void doPostAlgorithmActions() {

        if (outputPanel.isOutputNewImageSet()) {
            AlgorithmParameters.storeImageInRunner(getResultImage());
        }
    }

    /**
     * {@inheritDoc}
     */
    protected void setGUIFromParams() {
        image = scriptParameters.retrieveInputImage();
        userInterface = ViewUserInterface.getReference();
        parentFrame = image.getParentFrame();

        if (image.getType() == ModelImage.BOOLEAN) {
            MipavUtil.displayError("Source Image must NOT be Boolean");
            dispose();

            return;
        }

        outputPanel = new JPanelAlgorithmOutputOptions(image);
        scriptParameters.setOutputOptionsGUI(outputPanel);

        setImage25D(scriptParameters.getParams().getBoolean(AlgorithmParameters.DO_PROCESS_3D_AS_25D));
        setKernelSize(scriptParameters.getParams().getInt("kernel_size"));
        setFilterType(scriptParameters.getParams().getInt("rgb_filter_type"));

        boolean[] rgb = scriptParameters.getParams().getList(AlgorithmParameters.DO_PROCESS_RGB).getAsBooleanArray();
        setRed(rgb[0]);
        setGreen(rgb[1]);
        setBlue(rgb[2]);

        int[] rgbVectors = scriptParameters.getParams().getList("rgb_vector").getAsIntArray();
        setRedVector(rgbVectors[0]);
        setGreenVector(rgbVectors[1]);
        setBlueVector(rgbVectors[2]);
    }

    /**
     * {@inheritDoc}
     */
    protected void storeParamsFromGUI() throws ParserException {
        scriptParameters.storeInputImage(image);

        scriptParameters.storeOutputImageParams(getResultImage(), outputPanel.isOutputNewImageSet());
        scriptParameters.storeProcessingOptions(outputPanel.isProcessWholeImageSet(), image25D);

        scriptParameters.getParams().put(ParameterFactory.newParameter("kernel_size", kernelSize));
        scriptParameters.getParams().put(ParameterFactory.newParameter("rgb_filter_type", filterType));
        scriptParameters.storeColorOptions(red, green, blue);
        scriptParameters.getParams().put(ParameterFactory.newParameter("rgb_vector",
                                                                       new int[] { redVector, greenVector, blueVector }));
    }

    /**
     * Creates the combo-box that allows user to select the size of the kernel (mask).
     *
     * @param  singleSlices  DOCUMENT ME!
     */
    private void buildKernelSizeComboBox(boolean singleSlices) {

        if (singleSlices) {
            comboBoxKernelSize.addItem("3x3");
            comboBoxKernelSize.addItem("5x5");
            comboBoxKernelSize.addItem("7x7");
            comboBoxKernelSize.addItem("9x9");
            comboBoxKernelSize.addItem("11x11");
        } else {
            comboBoxKernelSize.addItem("3x3x3");
            comboBoxKernelSize.addItem("5x5x5");
            comboBoxKernelSize.addItem("7x7x7");
            comboBoxKernelSize.addItem("9x9x9");
            comboBoxKernelSize.addItem("11x11x11");
        }
    }

    /**
     * Associate one side of the kernel size with selectBox choice.
     */
    private void determineKernelSize() {

        if (comboBoxKernelSize.getSelectedIndex() == 0) {
            kernelSize = 3;
        } else if (comboBoxKernelSize.getSelectedIndex() == 1) {
            kernelSize = 5;
        } else if (comboBoxKernelSize.getSelectedIndex() == 2) {
            kernelSize = 7;
        } else if (comboBoxKernelSize.getSelectedIndex() == 3) {
            kernelSize = 9;
        } else if (comboBoxKernelSize.getSelectedIndex() == 4) {
            kernelSize = 11;
        }
    }

    /**
     * Sets up the GUI (panels, buttons, etc) and displays it on the screen.
     */
    private void init() {
        setForeground(Color.black);
        setTitle("Mean (Lowpass) Filter");

        // place everything setting up the kernel & filtering into the setupBox
        Box setupBox = new Box(BoxLayout.Y_AXIS);

        // maskPanel holds all the "Parameters"
        JPanel maskPanel = new JPanel();

        // panel gets a grid layout
        GridBagLayout gbl = new GridBagLayout();
        GridBagConstraints gbc = new GridBagConstraints();
        gbc.anchor = GridBagConstraints.WEST;
        gbc.insets = new Insets(2, 0, 2, 0); // component width = minwidth + (2ipadx)
        maskPanel.setLayout(gbl);

        maskPanel.setForeground(Color.black);
        maskPanel.setBorder(buildTitledBorder("Parameters")); // set the border ... "Parameters"

        JLabel labelKernelSize = createLabel("Kernel size:"); // make & set a label
        gbc.gridwidth = 2;
        gbc.anchor = GridBagConstraints.WEST;
        gbl.setConstraints(labelKernelSize, gbc);
        maskPanel.add(labelKernelSize); // add kernel label

        comboBoxKernelSize = new JComboBox();
        comboBoxKernelSize.setFont(serif12);
        comboBoxKernelSize.setBackground(Color.white);
        gbc.gridwidth = GridBagConstraints.REMAINDER;
        gbc.anchor = GridBagConstraints.EAST;
        gbl.setConstraints(comboBoxKernelSize, gbc);

        if (image.getNDims() == 2) {
            this.buildKernelSizeComboBox(true); // 2D images MUST be slice filtered
        } else {
            this.buildKernelSizeComboBox(true); // default setting for 3D+ images is slice filtering
        }

        maskPanel.add(comboBoxKernelSize); // add the comboboxt to the panel

        setupBox.add(maskPanel); // the parameters-panel is at the top of the box

        JPanel colourPanel = new JPanel();
        colourPanel.setLayout(gbl);

        // gbc.gridwidth = GridBagConstraints.REMAINDER;
        gbc.anchor = GridBagConstraints.WEST;
        colourPanel.setForeground(Color.black);
        colourPanel.setBorder(buildTitledBorder("Color channel selection")); // set the border ... "Colour channel
                                                                             // Selection"

        separateVectorGroup = new ButtonGroup();
        separateButton = new JRadioButton("Separate components", true);
        separateButton.setFont(serif12);
        separateButton.setForeground(Color.black);
        separateButton.addActionListener(this);
        separateVectorGroup.add(separateButton);

        // gbl.setConstraints(separateButton, gbc);
        gbc.gridx = 0;
        gbc.gridy = 0;
        gbc.gridwidth = 1;
        colourPanel.add(separateButton, gbc);

        redChannel = new JCheckBox("Red channel", true);
        redChannel.setFont(serif12);

        // gbl.setConstraints(redChannel, gbc);
        gbc.gridy = 1;
        colourPanel.add(redChannel, gbc);

        greenChannel = new JCheckBox("Green channel", true);
        greenChannel.setFont(serif12);

        // gbl.setConstraints(greenChannel, gbc);
        gbc.gridy = 2;
        colourPanel.add(greenChannel, gbc);

        blueChannel = new JCheckBox("Blue channel", true);
        blueChannel.setFont(serif12);

        // gbl.setConstraints(blueChannel, gbc);
        gbc.gridy = 3;
        colourPanel.add(blueChannel, gbc);

        adaptiveVectorButton = new JRadioButton("Adaptive vector directional filter", false);
        adaptiveVectorButton.setFont(serif12);
        adaptiveVectorButton.setForeground(Color.black);
        adaptiveVectorButton.addActionListener(this);
        separateVectorGroup.add(adaptiveVectorButton);

        // gbl.setConstraints(adaptiveVectorButton, gbc);
        gbc.gridy = 4;
        colourPanel.add(adaptiveVectorButton, gbc);

        parallelVectorButton = new JRadioButton("Vector decomposition filter parallel only", false);
        parallelVectorButton.setFont(serif12);
        parallelVectorButton.setForeground(Color.black);
        parallelVectorButton.addActionListener(this);
        separateVectorGroup.add(parallelVectorButton);

        // gbl.setConstraints(parallelVectorButton, gbc);
        gbc.gridy = 5;
        colourPanel.add(parallelVectorButton, gbc);

        redLabel = new JLabel("Red");
        redLabel.setForeground(Color.black);
        redLabel.setFont(serif12);
        redLabel.setEnabled(false);
        gbc.gridy = 6;
        colourPanel.add(redLabel, gbc);

        redText = new JTextField(6);
        redText.setText("255");

        if (image.getType() == ModelStorageBase.ARGB_USHORT) {
            redText.setText("65535");
        }

        redText.setForeground(Color.BLACK);
        redText.setFont(serif12);
        redText.setEnabled(false);
        gbc.gridx = 1;
        colourPanel.add(redText, gbc);

        greenLabel = new JLabel("Green");
        greenLabel.setForeground(Color.black);
        greenLabel.setFont(serif12);
        greenLabel.setEnabled(false);
        gbc.gridx = 0;
        gbc.gridy = 7;
        colourPanel.add(greenLabel, gbc);

        greenText = new JTextField(6);
        greenText.setText("0");
        greenText.setForeground(Color.BLACK);
        greenText.setFont(serif12);
        greenText.setEnabled(false);
        gbc.gridx = 1;
        colourPanel.add(greenText, gbc);

        blueLabel = new JLabel("Blue");
        blueLabel.setForeground(Color.black);
        blueLabel.setFont(serif12);
        blueLabel.setEnabled(false);
        gbc.gridx = 0;
        gbc.gridy = 8;
        colourPanel.add(blueLabel, gbc);

        blueText = new JTextField(6);
        blueText.setText("0");
        blueText.setForeground(Color.BLACK);
        blueText.setFont(serif12);
        blueText.setEnabled(false);
        gbc.gridx = 1;
        colourPanel.add(blueText, gbc);

        // if not a colour image, block access to the channel switches
        // since they don't mean anything
        if (image.isColorImage() == false) {
            separateButton.setEnabled(false);
            redChannel.setEnabled(false);
            greenChannel.setEnabled(false);
            blueChannel.setEnabled(false);
            adaptiveVectorButton.setEnabled(false);
            parallelVectorButton.setEnabled(false);
        }

        colourPanel.setToolTipText("Colour images can be filtered over any combination of colour channels");
        setupBox.add(colourPanel);

        outputPanel = new JPanelAlgorithmOutputOptions(image);

        setupBox.add(outputPanel); // place lowerBox into the setupBox

        if (image.getNDims() > 2) { // only perform if the image has more than 1 slice

            JPanel kernelThicknessPanel = new JPanel();
            kernelThicknessPanel.setBorder(buildTitledBorder("Kernel thickness")); // give the panel a border

            Box kernelThicknessBox = new Box(BoxLayout.Y_AXIS);

            ButtonGroup kernelThicknessGroup = new ButtonGroup();
            bySlice = new JRadioButton("Apply slice kernel", true);
            bySlice.setFont(serif12);
            bySlice.addActionListener(this);
            bySlice.setActionCommand("Slice");
            kernelThicknessGroup.add(bySlice); // add the button to the grouping
            kernelThicknessBox.add(bySlice); // add the button to the component

            wholeVolume = new JRadioButton("Apply volume kernel", false);
            wholeVolume.setFont(serif12);
            wholeVolume.addActionListener(this);
            wholeVolume.setActionCommand("Volume");
            kernelThicknessGroup.add(wholeVolume); // add the button to the grouping
            kernelThicknessBox.add(wholeVolume); // add the button to the component

            kernelThicknessPanel.add(kernelThicknessBox);
            setupBox.add(kernelThicknessPanel);
        }

        getContentPane().add(setupBox, BorderLayout.CENTER); // put the setupBox into the dialog

        getContentPane().add(buildButtons(), BorderLayout.SOUTH);

        pack();
        setVisible(true);
        //setResizable(false);
        System.gc();
    }

    /**
     * Use the GUI results to set up the variables needed to run the algorithm.
     *
     * @return  <code>true</code> if parameters set successfully, <code>false</code> otherwise.
     */
    private boolean setVariables() {
        String tmpStr;
        int colorMax = 255;

        if (image.getType() == ModelStorageBase.ARGB_USHORT) {
            colorMax = 65535;
        }

        // associate kernel size with selectBox choice.
        this.determineKernelSize();

        if (separateButton.isSelected()) {
            filterType = SEPARATE_COMPONENT;
        } else if (adaptiveVectorButton.isSelected()) {
            filterType = ADAPTIVE_VECTOR;
        } else {
            filterType = PARALLEL_VECTOR;
        }

        red = redChannel.isSelected();
        green = greenChannel.isSelected();
        blue = blueChannel.isSelected();

        tmpStr = redText.getText();
        redVector = Integer.parseInt(tmpStr);

        if (redVector < 0) {
            MipavUtil.displayError("Red must be at least 0");
            redText.requestFocus();
            redText.selectAll();

            return false;
        } else if (redVector > colorMax) {
            MipavUtil.displayError("Red must not exceed " + colorMax);
            redText.requestFocus();
            redText.selectAll();

            return false;
        }

        tmpStr = greenText.getText();
        greenVector = Integer.parseInt(tmpStr);

        if (greenVector < 0) {
            MipavUtil.displayError("Green must be at least 0");
            greenText.requestFocus();
            greenText.selectAll();

            return false;
        } else if (greenVector > colorMax) {
            MipavUtil.displayError("Green must not exceed " + colorMax);
            greenText.requestFocus();
            greenText.selectAll();

            return false;
        }

        tmpStr = blueText.getText();
        blueVector = Integer.parseInt(tmpStr);

        if (blueVector < 0) {
            MipavUtil.displayError("Blue must be at least 0");
            blueText.requestFocus();
            blueText.selectAll();

            return false;
        } else if (blueVector > colorMax) {
            MipavUtil.displayError("Blue must not exceed " + colorMax);
            blueText.requestFocus();
            blueText.selectAll();

            return false;
        }

        if (bySlice != null) {
            image25D = bySlice.isSelected();
        }

        return true;
    }

    /**
     * Return meta-information about this discoverable action for categorization and labeling purposes.
     * 
     * @return Metadata for this action.
     */
    public ActionMetadata getActionMetadata() {
        return new MipavActionMetadata() {
            public String getCategory() {
                return new String("Algorithms.Filters (spatial)");
            }

            public String getDescription() {
                return new String("Applies a mean filter.");
            }

            public String getDescriptionLong() {
                return new String("Applies a mean filter.");
            }

            public String getShortLabel() {
                return new String("Mean");
            }

            public String getLabel() {
                return new String("Mean");
            }

            public String getName() {
                return new String("Mean");
            }
        };
    }


    /**
     * Returns a table listing the input parameters of this algorithm (which should match up with the scripting
     * parameters used in {@link #setGUIFromParams()}).
     * 
     * @return A parameter table listing the inputs of this algorithm.
     */
    public ParameterTable createInputParameters() {
        final ParameterTable table = new ParameterTable();

        try {
            table.put(new ParameterExternalImage(AlgorithmParameters.getInputImageLabel(1)));
            table.put(new ParameterBoolean(AlgorithmParameters.DO_OUTPUT_NEW_IMAGE, true));
            table.put(new ParameterBoolean(AlgorithmParameters.DO_PROCESS_WHOLE_IMAGE, true));
            table.put(new ParameterBoolean(AlgorithmParameters.DO_PROCESS_SEPARABLE, true));
            table.put(new ParameterBoolean(AlgorithmParameters.DO_PROCESS_3D_AS_25D, false));
            table.put(new ParameterInt("kernel_size", 3));
            table.put(new ParameterInt("rgb_filter_type", 1));
            table.put(new ParameterList(AlgorithmParameters.DO_PROCESS_RGB, Parameter.PARAM_BOOLEAN, "true,true,true"));
            table.put(new ParameterList("rgb_vector", Parameter.PARAM_INT, "255,0,0"));
        } catch (final ParserException e) {
            // this shouldn't really happen since there isn't any real parsing going on...
            e.printStackTrace();
        }

        return table;
    }


    /**
     * Returns a table listing the output parameters of this algorithm (usually just labels used to obtain output image
     * names later).
     * 
     * @return A parameter table listing the outputs of this algorithm.
     */
    public ParameterTable createOutputParameters() {
        final ParameterTable table = new ParameterTable();

        try {
            table.put(new ParameterImage(AlgorithmParameters.RESULT_IMAGE));
        } catch (final ParserException e) {
            // this shouldn't really happen since there isn't any real parsing going on...
            e.printStackTrace();
        }

        return table;
    }


    /**
     * Returns the name of an image output by this algorithm, the image returned depends on the parameter label given
     * (which can be used to retrieve the image object from the image registry).
     * 
     * @param imageParamName The output image parameter label for which to get the image name.
     * @return The image name of the requested output image parameter label.
     */
    public String getOutputImageName(final String imageParamName) {
        if (imageParamName.equals(AlgorithmParameters.RESULT_IMAGE)) {
            if (getResultImage() != null) {
                // algo produced a new result image
                return getResultImage().getImageName();
            } else {
                // algo was done in place
                return image.getImageName();
            }
        }

        Preferences.debug("Unrecognized output image parameter: " + imageParamName + "\n", Preferences.DEBUG_SCRIPTING);

        return null;
    }


    /**
     * Returns whether the action has successfully completed its execution.
     * 
     * @return True, if the action is complete. False, if the action failed or is still running.
     */
    public boolean isActionComplete() {
        return isComplete();
    }

}
