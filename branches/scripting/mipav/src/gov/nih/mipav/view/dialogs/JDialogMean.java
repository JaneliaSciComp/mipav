package gov.nih.mipav.view.dialogs;


import gov.nih.mipav.model.algorithms.*;
import gov.nih.mipav.model.algorithms.filters.*;
import gov.nih.mipav.model.file.*;
import gov.nih.mipav.model.structures.*;

import gov.nih.mipav.view.*;

import java.awt.*;
import java.awt.event.*;

import java.util.*;

import javax.swing.*;


/**
 * Dialog to get user input, then call the algorithm. The user has the option to generate a new image or replace the
 * source image. In addition the user can select having the algorithm applied to whole image or to the VOI regions. It
 * should be noted that the algorithms are executed in their own thread.
 */
public class JDialogMean extends JDialogBase implements AlgorithmInterface, ScriptableInterface {

    //~ Static fields/initializers -------------------------------------------------------------------------------------

    /** Use serialVersionUID for interoperability. */
    private static final long serialVersionUID = -5373013772234532691L;
    
    public static final int SEPARATE_COMPONENT = 1;
    
    public static final int ADAPTIVE_VECTOR = 2;
    
    public static final int PARALLEL_VECTOR = 3;

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** DOCUMENT ME! */
    private boolean blue;

    /** DOCUMENT ME! */
    private JCheckBox blueChannel;

    /** DOCUMENT ME! */
    private JRadioButton bySlice; // allows selection of processing each slice of a 3D image-set individually

    /** DOCUMENT ME! */
    private JComboBox comboBoxKernelSize;

    /** DOCUMENT ME! */
    private ButtonGroup destinationGroup;

    /** DOCUMENT ME! */
    private int displayLoc; // Flag indicating if a new image is to be generated

    /** or if the source image is to be replaced. */
    private boolean entireImageFlag; // true = apply algorithm to the whole image

    /** DOCUMENT ME! */
    private boolean green;

    /** DOCUMENT ME! */
    private JCheckBox greenChannel;
    
    private ButtonGroup separateVectorGroup;
    private JRadioButton separateButton;
    private int filterType = SEPARATE_COMPONENT;
    private JRadioButton parallelVectorButton;
    private JRadioButton adaptiveVectorButton;
    private JLabel redLabel;
    private JTextField redText;
    private JLabel greenLabel;
    private JTextField greenText;
    private JLabel blueLabel;
    private JTextField blueText;
    private int redVector;
    private int greenVector;
    private int blueVector;

    /** DOCUMENT ME! */
    private ModelImage image; // source image

    /** false = apply algorithm only to VOI regions. */
    private boolean image25D = false; // flag indicating if slices should be processed independently

    /** DOCUMENT ME! */
    private ButtonGroup imageVOIGroup;

    /** DOCUMENT ME! */
    private int kernelSize;

    /** DOCUMENT ME! */
    private AlgorithmMean meanAlgo = null;

    /** DOCUMENT ME! */
    private JRadioButton newImage;

    /** DOCUMENT ME! */
    private boolean red;

    /** DOCUMENT ME! */
    private JCheckBox redChannel;

    /** DOCUMENT ME! */
    private JRadioButton replaceImage;

    /** DOCUMENT ME! */
    private ModelImage resultImage = null; // result image

    /** DOCUMENT ME! */
    private String[] titles;

    /** DOCUMENT ME! */
    private ViewUserInterface userInterface;

    /** DOCUMENT ME! */
    private JRadioButton VOIRegions;

    /** DOCUMENT ME! */
    private JRadioButton wholeImage;

    /** DOCUMENT ME! */
    private JRadioButton wholeVolume; //

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
        userInterface = ((ViewJFrameBase) (parentFrame)).getUserInterface();
        init();
    }

    /**
     * Used primarily for the script to store variables and run the algorithm. No actual dialog will appear but the set
     * up info and result image will be stored here.
     *
     * @param  UI  The user interface, needed to create the image frame.
     * @param  im  Source image.
     */
    public JDialogMean(ViewUserInterface UI, ModelImage im) {
        super();
        userInterface = UI;

        if (im.getType() == ModelImage.BOOLEAN) {
            MipavUtil.displayError("Source Image must NOT be Boolean");
            dispose();

            return;
        }

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
            MipavUtil.showHelp("10015");
        } else if (command.equals("Cancel")) {
            dispose();
        }
        else if ((source == separateButton) || (source == adaptiveVectorButton) ||
                 (source == parallelVectorButton)) {
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
            }
            else if (adaptiveVectorButton.isSelected()) {
                redChannel.setEnabled(false);
                greenChannel.setEnabled(false);
                blueChannel.setEnabled(false);
                redLabel.setEnabled(false);
                redText.setEnabled(false);
                greenLabel.setEnabled(false);
                greenText.setEnabled(false);
                blueLabel.setEnabled(false);
                blueText.setEnabled(false);
            }
            else { // parallelVectorButton.isSelected()
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
                Vector imageFrames = image.getImageFrameVector();

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

        insertScriptLine(algorithm);

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

                userInterface.getScriptDialog().append("Mean " +
                                                       userInterface.getScriptDialog().getVar(image.getImageName()) +
                                                       " ");

                if (displayLoc == NEW) {
                    userInterface.getScriptDialog().putVar(resultImage.getImageName());
                    userInterface.getScriptDialog().append(userInterface.getScriptDialog().getVar(resultImage.getImageName()) +
                                                           " " + entireImageFlag + " " + image25D + " " + kernelSize +
                                                           " " + filterType +
                                                           " " + red + " " + green + " " + blue + 
                                                           " " + redVector + " " + greenVector + " " + blueVector + "\n");
                } else {
                    userInterface.getScriptDialog().append(userInterface.getScriptDialog().getVar(image.getImageName()) +
                                                           " " + entireImageFlag + " " + image25D + " " + kernelSize +
                                                           " " + filterType + 
                                                           " " + red + " " + green + " " + blue + 
                                                           " " + redVector + " " + greenVector + " " + blueVector + "\n");
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

        if (im.getType() == ModelImage.BOOLEAN) {
            MipavUtil.displayError("Source Image must NOT be Boolean");
            dispose();

            return;
        }

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
            setEntireImageFlag(parser.getNextBoolean());
            setImage25D(parser.getNextBoolean());
            setKernelSize(parser.getNextInteger());
            setFilterType(parser.getNextInteger());
            setRed(parser.getNextBoolean());
            setGreen(parser.getNextBoolean());
            setBlue(parser.getNextBoolean());
            setRedVector(parser.getNextInteger());
            setGreenVector(parser.getNextInteger());
            setBlueVector(parser.getNextInteger());
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
     * Accessor that sets the color flag.
     *
     * @param  flag  <code>true</code> indicates ARG image, blue.
     */
    public void setBlue(boolean flag) {
        blue = flag;
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
     * Accessor that sets the region flag.
     *
     * @param  flag  <code>true</code> indicates the whole image is processed, <code>false</code> indicates a region.
     */
    public void setEntireImageFlag(boolean flag) {
        entireImageFlag = flag;
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
     * 
     * @param filterType SEPARATE_COMPONENT, ADAPTIVE_VECTOR, or PARALLEL_VECTOR
     */
    public void setFilterType(int filterType) {
        this.filterType = filterType;
    }
    
    /**
     * 
     * @param redVector
     */
    public void setRedVector(int redVector) {
        this.redVector = redVector;
    }
    
    /**
     * 
     * @param greenVector
     */
    public void setGreenVector(int greenVector) {
        this.greenVector = greenVector;
    }
    
    public void setBlueVector(int blueVector) {
        this.blueVector = blueVector;
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

            if (displayLoc == NEW) { // (2D)

                try {

                    // Make result image of float type
                    // resultImage     = new ModelImage(image.getType(), destExtents, name, userInterface);
                    resultImage = (ModelImage) image.clone();
                    resultImage.setImageName(name);

                    if ((resultImage.getFileInfo()[0]).getFileFormat() == FileBase.DICOM) {
                        ((FileInfoDicom) (resultImage.getFileInfo(0))).setValue("0002,0002",
                                                                                "1.2.840.10008.5.1.4.1.1.7 ", 26); // Secondary Capture SOP UID
                        ((FileInfoDicom) (resultImage.getFileInfo(0))).setValue("0008,0016",
                                                                                "1.2.840.10008.5.1.4.1.1.7 ", 26);
                        ((FileInfoDicom) (resultImage.getFileInfo(0))).setValue("0002,0012", "1.2.840.34379.17", 16); // bogus Implementation UID made up by Matt
                        ((FileInfoDicom) (resultImage.getFileInfo(0))).setValue("0002,0013", "MIPAV--NIH", 10); //
                    }

                    // Make algorithm
                    meanAlgo = new AlgorithmMean(resultImage, image, kernelSize, entireImageFlag);

                    // only if the src image is colour will any channel checkboxes be enabled
                    meanAlgo.setRGBChannelFilter(filterType, red, green, blue,
                                                 redVector, greenVector, blueVector);

                    // This is very important. Adding this object as a listener allows the algorithm to
                    // notify this object when it has completed or failed. See algorithm performed event.
                    // This is made possible by implementing AlgorithmedPerformed interface
                    meanAlgo.addListener(this);
                    setVisible(false); // Hide dialog

                    if (isRunInSeparateThread()) {

                        // Start the thread as a low priority because we wish to still have user interface work fast.
                        if (meanAlgo.startMethod(Thread.MIN_PRIORITY) == false) {
                            MipavUtil.displayError("A thread is already running on this object");
                        }
                    } else {
                        if (!userInterface.isAppFrameVisible()) {
                            meanAlgo.setProgressBarVisible(false);
                        }

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
                    meanAlgo = new AlgorithmMean(image, kernelSize, entireImageFlag);

                    // only if the src image is colour will any channel checkboxes be enabled
                    meanAlgo.setRGBChannelFilter(filterType, red, green, blue,
                                                 redVector, greenVector, blueVector);

                    // This is very important. Adding this object as a listener allows the algorithm to
                    // notify this object when it has completed or failed. See algorithm performed event.
                    // This is made possible by implementing AlgorithmedPerformed interface
                    meanAlgo.addListener(this);

                    // Hide the dialog since the algorithm is about to run.
                    setVisible(false);

                    // These next lines set the titles in all frames where the source image is displayed to
                    // "locked - " image name so as to indicate that the image is now read/write locked!
                    // The image frames are disabled and then unregisted from the userinterface until the
                    // algorithm has completed.
                    Vector imageFrames = image.getImageFrameVector();
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
                        if (!userInterface.isAppFrameVisible()) {
                            meanAlgo.setProgressBarVisible(false);
                        }

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

            if (displayLoc == NEW) { // (3D)

                try {

                    // Make result image of float type
                    // resultImage     = new ModelImage(image.getType(), destExtents, name, userInterface);
                    resultImage = (ModelImage) image.clone();
                    resultImage.setImageName(name);

                    if ((resultImage.getFileInfo()[0]).getFileFormat() == FileBase.DICOM) {

                        for (int i = 0; i < resultImage.getExtents()[2]; i++) {
                            ((FileInfoDicom) (resultImage.getFileInfo(i))).setValue("0002,0002",
                                                                                    "1.2.840.10008.5.1.4.1.1.7 ", 26); // Secondary Capture SOP UID
                            ((FileInfoDicom) (resultImage.getFileInfo(i))).setValue("0008,0016",
                                                                                    "1.2.840.10008.5.1.4.1.1.7 ", 26);
                            ((FileInfoDicom) (resultImage.getFileInfo(i))).setValue("0002,0012", "1.2.840.34379.17",
                                                                                    16); // bogus Implementation UID
                                                                                         // made up by Matt
                            ((FileInfoDicom) (resultImage.getFileInfo(i))).setValue("0002,0013", "MIPAV--NIH", 10); //
                        }
                    }

                    // Make algorithm
                    meanAlgo = new AlgorithmMean(resultImage, image, kernelSize, image25D, entireImageFlag);

                    // only if the src image is colour will any channel checkboxes be enabled
                    meanAlgo.setRGBChannelFilter(filterType, red, green, blue,
                                                 redVector, greenVector, blueVector);

                    // This is very important. Adding this object as a listener allows the algorithm to
                    // notify this object when it has completed or failed. See algorithm performed event.
                    // This is made possible by implementing AlgorithmedPerformed interface
                    meanAlgo.addListener(this);
                    setVisible(false); // Hide dialog

                    if (isRunInSeparateThread()) {

                        // Start the thread as a low priority because we wish to still have user interface work fast.
                        if (meanAlgo.startMethod(Thread.MIN_PRIORITY) == false) {
                            MipavUtil.displayError("A thread is already running on this object");
                        }
                    } else {
                        if (!userInterface.isAppFrameVisible()) {
                            meanAlgo.setProgressBarVisible(false);
                        }

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
                    meanAlgo = new AlgorithmMean(image, kernelSize, image25D, entireImageFlag);

                    // only if the src image is colour will any channel checkboxes be enabled
                    meanAlgo.setRGBChannelFilter(filterType, red, green, blue,
                                                 redVector, greenVector, blueVector);

                    // This is very important. Adding this object as a listener allows the algorithm to
                    // notify this object when it has completed or failed. See algorithm performed event.
                    // This is made possible by implementing AlgorithmedPerformed interface
                    meanAlgo.addListener(this);

                    // Hide dialog
                    setVisible(false);

                    // These next lines set the titles in all frames where the source image is displayed to
                    // "locked - " image name so as to indicate that the image is now read/write locked!
                    // The image frames are disabled and then unregisted from the userinterface until the
                    // algorithm has completed.
                    Vector imageFrames = image.getImageFrameVector();
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
                        if (!userInterface.isAppFrameVisible()) {
                            meanAlgo.setProgressBarVisible(false);
                        }

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
        //gbc.gridwidth = GridBagConstraints.REMAINDER;
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
        //gbl.setConstraints(separateButton, gbc);
        gbc.gridx = 0;
        gbc.gridy = 0;
        gbc.gridwidth = 1;
        colourPanel.add(separateButton, gbc);
        
        redChannel = new JCheckBox("Red channel", true);
        redChannel.setFont(serif12);
        //gbl.setConstraints(redChannel, gbc);
        gbc.gridy = 1;
        colourPanel.add(redChannel, gbc);

        greenChannel = new JCheckBox("Green channel", true);
        greenChannel.setFont(serif12);
        //gbl.setConstraints(greenChannel, gbc);
        gbc.gridy = 2;
        colourPanel.add(greenChannel, gbc);

        blueChannel = new JCheckBox("Blue channel", true);
        blueChannel.setFont(serif12);
        //gbl.setConstraints(blueChannel, gbc);
        gbc.gridy = 3;
        colourPanel.add(blueChannel, gbc);
        
        adaptiveVectorButton = new JRadioButton("Adaptive vector directional filter", false);
        adaptiveVectorButton.setFont(serif12);
        adaptiveVectorButton.setForeground(Color.black);
        adaptiveVectorButton.addActionListener(this);
        separateVectorGroup.add(adaptiveVectorButton);
        //gbl.setConstraints(adaptiveVectorButton, gbc);
        gbc.gridy = 4;
        colourPanel.add(adaptiveVectorButton, gbc);
        
        parallelVectorButton = new JRadioButton("Vector decomposition filter parallel only", false);
        parallelVectorButton.setFont(serif12);
        parallelVectorButton.setForeground(Color.black);
        parallelVectorButton.addActionListener(this);
        separateVectorGroup.add(parallelVectorButton);
        //gbl.setConstraints(parallelVectorButton, gbc);
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

        Box lowerBox = new Box(BoxLayout.X_AXIS);

        // destination goes in the left of the lower box
        JPanel destinationPanel = new JPanel();
        Box destinationBox = new Box(BoxLayout.Y_AXIS);

        destinationPanel.setForeground(Color.black);
        destinationPanel.setBorder(buildTitledBorder("Destination"));

        destinationGroup = new ButtonGroup();
        newImage = new JRadioButton("New image", true);
        newImage.setFont(serif12);
        destinationGroup.add(newImage); // add the button to the grouping
        destinationBox.add(newImage); // add the button to the component

        replaceImage = new JRadioButton("Replace image", false);
        replaceImage.setFont(serif12);
        destinationGroup.add(replaceImage); // add the button to the grouping
        destinationBox.add(replaceImage); // add the button to the component
        destinationPanel.add(destinationBox);

        lowerBox.add(destinationPanel);

        // filter goes in the right of the lower box
        JPanel imageVOIPanel = new JPanel();
        imageVOIPanel.setForeground(Color.black);
        imageVOIPanel.setBorder(buildTitledBorder("Filter"));

        Box imageVOIBox = new Box(BoxLayout.Y_AXIS);
        imageVOIGroup = new ButtonGroup();
        wholeImage = new JRadioButton("Whole image", true);
        wholeImage.setFont(serif12);
        imageVOIGroup.add(wholeImage); // add the button to the grouping
        imageVOIBox.add(wholeImage); // add the button to the component

        VOIRegions = new JRadioButton("VOI region(s)", false);
        VOIRegions.setFont(serif12);
        imageVOIGroup.add(VOIRegions); // add the button to the grouping
        imageVOIBox.add(VOIRegions); // add the button to the component

        imageVOIPanel.add(imageVOIBox); // place the box onto a border
        lowerBox.add(imageVOIPanel); // place the bordered stuff into the lower box

        setupBox.add(lowerBox); // place lowerBox into the setupBox

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

        // Only if the image is unlocked can it be replaced.
        if (image.getLockStatus() == ModelStorageBase.UNLOCKED) {
            replaceImage.setEnabled(true);
        } else {
            replaceImage.setEnabled(false);
        }

        getContentPane().add(buildButtons(), BorderLayout.SOUTH);

        pack();
        setVisible(true);
        setResizable(false);
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

        if (replaceImage.isSelected()) {
            displayLoc = REPLACE;
        } else if (newImage.isSelected()) {
            displayLoc = NEW;
        }

        if (wholeImage.isSelected()) {
            entireImageFlag = true;
        } else if (VOIRegions.isSelected()) {
            entireImageFlag = false;
        }

        // associate kernel size with selectBox choice.
        this.determineKernelSize();

        if (separateButton.isSelected()) {
            filterType = SEPARATE_COMPONENT;
        }
        else if (adaptiveVectorButton.isSelected()) {
            filterType = ADAPTIVE_VECTOR;
        }
        else {
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
        }
        else if (redVector > colorMax) {
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
        }
        else if (greenVector > colorMax) {
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
        }
        else if (blueVector > colorMax) {
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
}
