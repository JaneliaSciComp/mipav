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
 *
 * @version  1.0; 17 February 2000
 */
public class JDialogMedian extends JDialogBase implements AlgorithmInterface, ScriptableInterface {

    //~ Static fields/initializers -------------------------------------------------------------------------------------

    /** Use serialVersionUID for interoperability. */
    private static final long serialVersionUID = -7749097890204237664L;

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** DOCUMENT ME! */
    public long start, end;

    /** DOCUMENT ME! */
    private boolean blue;

    /** DOCUMENT ME! */
    private JCheckBox blueChannel;

    /** DOCUMENT ME! */
    private JRadioButton bySlice; // allows selection of processing each slice of a 3D image-set individually

    /** DOCUMENT ME! */
    private JComboBox comboBoxKernelShape;

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

    /** DOCUMENT ME! */
    private ModelImage image; // source image

    /** false = apply algorithm only to VOI regions. */
    private boolean image25D = false; // flag indicating if slices should be processed independently

    /** DOCUMENT ME! */
    private ButtonGroup imageVOIGroup;

    /** DOCUMENT ME! */
    private int iters;

    /** DOCUMENT ME! */
    private int kernelShape;

    /** DOCUMENT ME! */
    private int kernelSize;

    /** DOCUMENT ME! */
    private AlgorithmMedian medianAlgo = null;

    /** DOCUMENT ME! */
    private JRadioButton newImage;

    /** DOCUMENT ME! */
    private boolean red;

    /** DOCUMENT ME! */
    private JCheckBox redChannel;
    
    private ButtonGroup vectorGroup;
    
    private JRadioButton vectorButton;
    
    private JRadioButton componentButton;
    
    private boolean vectorFilter = true;

    /** DOCUMENT ME! */
    private JRadioButton replaceImage;

    /** DOCUMENT ME! */
    private ModelImage resultImage = null; // result image

    /** DOCUMENT ME! */
    private float stdDev;

    /** DOCUMENT ME! */
    private JTextField textNIter;

    /** DOCUMENT ME! */
    private JTextField textSTDDeviation; // textfield to hold Standard Deviation Value.
    
    private JLabel labelSTDDeviation;

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
    public JDialogMedian() { }

    /**
     * Creates a new JDialogMedian object.
     *
     * @param  theParentFrame  Parent frame.
     * @param  im              Source image.
     */
    public JDialogMedian(Frame theParentFrame, ModelImage im) {
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
    public JDialogMedian(ViewUserInterface UI, ModelImage im) {
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
        } else if ((source == vectorButton) || (source == componentButton)) {
            if (vectorButton.isSelected()) {
                redChannel.setSelected(true);
                redChannel.setEnabled(false);
                greenChannel.setSelected(true);
                greenChannel.setEnabled(false);
                blueChannel.setSelected(true);
                blueChannel.setEnabled(false);
                labelSTDDeviation.setEnabled(false);
                textSTDDeviation.setEnabled(false);
                textSTDDeviation.setText("0.0");
            }
            else {
                redChannel.setEnabled(true);
                greenChannel.setEnabled(true);
                blueChannel.setEnabled(true);
                labelSTDDeviation.setEnabled(true);
                textSTDDeviation.setEnabled(true);
            }
        } else if (command.equals("Volume")) {

            // kernel Size
            int indx = comboBoxKernelSize.getSelectedIndex(); // get the current combo-box selection
            comboBoxKernelSize.removeAllItems();
            buildKernelSizeComboBox(false);
            comboBoxKernelSize.setSelectedIndex(indx); // set the new combo-box to the old selection

            // kernel Shape
            indx = comboBoxKernelShape.getSelectedIndex();
            comboBoxKernelShape.removeAllItems();
            buildKernelShapeComboBox(false);
            comboBoxKernelShape.setSelectedIndex(indx); // set the new combo-box to the old selection
            pack();
        } else if (command.equals("Slice")) {

            // kernel size
            int indx = comboBoxKernelSize.getSelectedIndex(); // get the current combo-box selection
            comboBoxKernelSize.removeAllItems();
            buildKernelSizeComboBox(true);
            comboBoxKernelSize.setSelectedIndex(indx); // set the new combo-box to the old selection

            // kernel shape
            // indx = comboBoxKernelShape.getSelectedIndex();
            indx = 0;
            comboBoxKernelShape.removeAllItems();
            buildKernelShapeComboBox(true);
            comboBoxKernelShape.setSelectedIndex(indx); // set the new combo-box to the old selection
        } else if (command.equals("Cancel")) {
            dispose();
        } else if (command.equals("Help")) {
            MipavUtil.showHelp("10017");
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

        if (algorithm instanceof AlgorithmMedian) {
            end = System.currentTimeMillis();
            System.err.println("Median Elapsed: " + (end - start));
            image.clearMask();

            if ((medianAlgo.isCompleted() == true) && (resultImage != null)) {
                // The algorithm has completed and produced a new image to be displayed.

                updateFileInfo(image, resultImage);
                resultImage.clearMask();

                try {

                    // resultImage.setImageName("Median: "+image.getImageName());
                    imageFrame = new ViewJFrameImage(resultImage, null, new Dimension(610, 200));
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

        medianAlgo.finalize();
        medianAlgo = null;
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

                userInterface.getScriptDialog().append("Median " +
                                                       userInterface.getScriptDialog().getVar(image.getImageName()) +
                                                       " ");

                if (displayLoc == NEW) {
                    userInterface.getScriptDialog().putVar(resultImage.getImageName());
                    userInterface.getScriptDialog().append(userInterface.getScriptDialog().getVar(resultImage.getImageName()) +
                                                           " " + entireImageFlag + " " + image25D + " " + iters + " " +
                                                           stdDev + " " + kernelSize + " " + kernelShape + " " + 
                                                           vectorFilter + " " + red +
                                                           " " + green + " " + blue + "\n");
                } else {
                    userInterface.getScriptDialog().append(userInterface.getScriptDialog().getVar(image.getImageName()) +
                                                           " " + entireImageFlag + " " + image25D + " " + iters + " " +
                                                           stdDev + " " + kernelSize + " " + kernelShape + " " +
                                                           vectorFilter + " " + red +
                                                           " " + green + " " + blue + "\n");
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
            setIters(parser.getNextInteger());
            setStdDev(parser.getNextFloat());
            setKernelSize(parser.getNextInteger());
            setKernelShape(parser.getNextInteger());
            setVectorFilter(parser.getNextBoolean());
            setRed(parser.getNextBoolean());
            setGreen(parser.getNextBoolean());
            setBlue(parser.getNextBoolean());
        } catch (Exception e) {
            throw new IllegalArgumentException();
        }

        setActiveImage(parser.isActiveImage());
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
     * Accessor that sets the number of iterations.
     *
     * @param  num  Value to set iterations to (should be between 1 and 20).
     */
    public void setIters(int num) {
        iters = num;
    }

    /**
     * Accessor that sets the kernel shape.
     *
     * @param  shape  Value to set size to (0 == square, 1 == cross).
     */
    public void setKernelShape(int shape) {
        kernelShape = shape;
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
     * Accessor that sets if all colors are vector filtered at once
     * @param vectorFilter
     */
    public void setVectorFilter(boolean vectorFilter) {
        this.vectorFilter = vectorFilter;
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
     * Accessor that sets the standard deviation.
     *
     * @param  dev  Value to set the standard deviation to (should be between 0.0 and 9.9).
     */
    public void setStdDev(float dev) {
        stdDev = dev;
    }

    /**
     * Creates the combo-box that allows user to select the shape of the kernel (mask).
     *
     * @param  singleSlices  DOCUMENT ME!
     */
    private void buildKernelShapeComboBox(boolean singleSlices) {

        if (singleSlices) {
            comboBoxKernelShape.addItem("Square");
            comboBoxKernelShape.addItem("Cross (+)");
            comboBoxKernelShape.addItem("Corner-to-corner (x)");
            comboBoxKernelShape.addItem("Horizontal");
            comboBoxKernelShape.addItem("Vertical");
        } else {
            comboBoxKernelShape.addItem("Cube");
            comboBoxKernelShape.addItem("Axis");
            comboBoxKernelShape.addItem("Corner-to-corner");
        }
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
     * Once all the necessary variables are set, call the median algorithm based on what type of image this is and
     * whether or not there is a separate destination image.
     */
    private void callAlgorithm() {
        start = System.currentTimeMillis();

        String name = makeImageName(image.getImageName(), "_median");

        // stuff to do when working on 2-D images.
        if (image.getNDims() == 2) { // source image is 2D

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
                    medianAlgo = new AlgorithmMedian(resultImage, image, iters, kernelSize, kernelShape, stdDev,
                                                     entireImageFlag);

                    // only if the src image is colour will any channel checkboxes be enabled
                    medianAlgo.setRGBChannelFilter(vectorFilter, red, green, blue);

                    // This is very important. Adding this object as a listener allows the algorithm to
                    // notify this object when it has completed or failed. See algorithm performed event.
                    // This is made possible by implementing AlgorithmedPerformed interface
                    medianAlgo.addListener(this);
                    setVisible(false); // Hide dialog

                    if (runInSeparateThread) {

                        // Start the thread as a low priority because we wish to still have user interface work fast.
                        if (medianAlgo.startMethod(Thread.MIN_PRIORITY) == false) {
                            MipavUtil.displayError("A thread is already running on this object");
                        }
                    } else {
                        medianAlgo.setActiveImage(isActiveImage);

                        if (!userInterface.isAppFrameVisible()) {
                            medianAlgo.setProgressBarVisible(false);
                        }

                        medianAlgo.run();
                    }
                } catch (OutOfMemoryError x) {
                    MipavUtil.displayError("Dialog median: unable to allocate enough memory");

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
                    medianAlgo = new AlgorithmMedian(image, iters, kernelSize, kernelShape, stdDev, entireImageFlag);

                    // only if the src image is colour will any channel checkboxes be enabled
                    medianAlgo.setRGBChannelFilter(vectorFilter, red, green, blue);

                    // This is very important. Adding this object as a listener allows the algorithm to
                    // notify this object when it has completed or failed. See algorithm performed event.
                    // This is made possible by implementing AlgorithmedPerformed interface
                    medianAlgo.addListener(this);

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

                    if (runInSeparateThread) {

                        // Start the thread as a low priority because we wish to still have user interface work fast.
                        if (medianAlgo.startMethod(Thread.MIN_PRIORITY) == false) {
                            MipavUtil.displayError("A thread is already running on this object");
                        }
                    } else {
                        medianAlgo.setActiveImage(isActiveImage);

                        if (!userInterface.isAppFrameVisible()) {
                            medianAlgo.setProgressBarVisible(false);
                        }

                        medianAlgo.run();
                    }
                } catch (OutOfMemoryError x) {
                    MipavUtil.displayError("Dialog median: unable to allocate enough memory");

                    return;
                }
            }
        } else if (image.getNDims() == 3) {

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
                    medianAlgo = new AlgorithmMedian(resultImage, image, iters, kernelSize, kernelShape, stdDev,
                                                     image25D, entireImageFlag);

                    // only if the src image is colour will any channel checkboxes be enabled
                    medianAlgo.setRGBChannelFilter(vectorFilter, red, green, blue);

                    // This is very important. Adding this object as a listener allows the algorithm to
                    // notify this object when it has completed or failed. See algorithm performed event.
                    // This is made possible by implementing AlgorithmedPerformed interface
                    medianAlgo.addListener(this);
                    setVisible(false); // Hide dialog

                    if (runInSeparateThread) {

                        // Start the thread as a low priority because we wish to still have user interface work fast.
                        if (medianAlgo.startMethod(Thread.MIN_PRIORITY) == false) {
                            MipavUtil.displayError("A thread is already running on this object");
                        }
                    } else {
                        medianAlgo.setActiveImage(isActiveImage);

                        if (!userInterface.isAppFrameVisible()) {
                            medianAlgo.setProgressBarVisible(false);
                        }

                        medianAlgo.run();
                    }
                } catch (OutOfMemoryError x) {
                    MipavUtil.displayError("Dialog median: unable to allocate enough memory");

                    if (resultImage != null) {
                        resultImage.disposeLocal(); // Clean up image memory
                        resultImage = null;
                    }

                    return;
                }
            } else { // displayLoc == REPLACE         (3D)

                try {

                    // Make algorithm
                    medianAlgo = new AlgorithmMedian(image, iters, kernelSize, kernelShape, stdDev, image25D,
                                                     entireImageFlag);

                    // only if the src image is colour will any channel checkboxes be enabled
                    medianAlgo.setRGBChannelFilter(vectorFilter, red, green, blue);

                    // This is very important. Adding this object as a listener allows the algorithm to
                    // notify this object when it has completed or failed. See algorithm performed event.
                    // This is made possible by implementing AlgorithmedPerformed interface
                    medianAlgo.addListener(this);

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

                    if (runInSeparateThread) {

                        // Start the thread as a low priority because we wish to still have user interface work fast.
                        if (medianAlgo.startMethod(Thread.MIN_PRIORITY) == false) {
                            MipavUtil.displayError("A thread is already running on this object");
                        }
                    } else {
                        medianAlgo.setActiveImage(isActiveImage);

                        if (!userInterface.isAppFrameVisible()) {
                            medianAlgo.setProgressBarVisible(false);
                        }

                        medianAlgo.run();
                    }
                } catch (OutOfMemoryError x) {
                    MipavUtil.displayError("Dialog median: unable to allocate enough memory");

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
        setTitle("Median Filter");

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

        // iterations
        JLabel labelNIter = createLabel("Number of iterations (1-20):"); // make and set the iteration instruction
        gbc.gridwidth = 2;
        gbc.anchor = GridBagConstraints.WEST;
        gbl.setConstraints(labelNIter, gbc);
        maskPanel.add(labelNIter);

        textNIter = createTextField("1"); // make and set the input field
        textNIter.setColumns(2);
        textNIter.setMaximumSize(textNIter.getPreferredSize()); // don't let it get any bigger than what it prefers
        textNIter.setHorizontalAlignment(JTextField.CENTER);
        textNIter.setFont(serif12);
        MipavUtil.makeNumericsOnly(textNIter, false);
        gbc.gridwidth = GridBagConstraints.REMAINDER;
        gbc.anchor = GridBagConstraints.EAST;
        gbl.setConstraints(textNIter, gbc);
        maskPanel.add(textNIter);

        // standard deviation info for not filtering certain pixels.
        labelSTDDeviation = createLabel("Maximum standard deviation:");
        gbc.gridwidth = 2;
        gbc.anchor = GridBagConstraints.WEST;
        gbl.setConstraints(labelSTDDeviation, gbc);
        maskPanel.add(labelSTDDeviation); // add the instructions
        if (image.isColorImage()) {
            labelSTDDeviation.setEnabled(false);
        }

        textSTDDeviation = createTextField("0.0"); // make & set input
        textSTDDeviation.setColumns(3);
        textSTDDeviation.setMaximumSize(textSTDDeviation.getPreferredSize()); // don't let it get any bigger
        if (image.isColorImage()) {
            textSTDDeviation.setEnabled(false);
        }

        // than what it prefers
        textSTDDeviation.setHorizontalAlignment(JTextField.CENTER);
        textSTDDeviation.setFont(serif12);
        MipavUtil.makeNumericsOnly(textSTDDeviation, true);
        gbc.gridwidth = GridBagConstraints.REMAINDER;
        gbc.anchor = GridBagConstraints.EAST;
        gbl.setConstraints(textSTDDeviation, gbc);
        maskPanel.add(textSTDDeviation); // add input

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

        JLabel labelKernelShape = createLabel("Kernel shape:"); // make & set a label
        gbc.gridwidth = 2;
        gbc.anchor = GridBagConstraints.WEST;
        gbl.setConstraints(labelKernelShape, gbc);
        maskPanel.add(labelKernelShape); // add kernel label

        comboBoxKernelShape = new JComboBox();
        comboBoxKernelShape.setFont(serif12);
        comboBoxKernelShape.setBackground(Color.white);
        gbc.gridwidth = GridBagConstraints.REMAINDER;
        gbc.anchor = GridBagConstraints.EAST;
        gbl.setConstraints(comboBoxKernelShape, gbc);

        if (image.getNDims() == 2) {
            this.buildKernelShapeComboBox(true); // 2D MUST be singleSlice filtered
        } else {
            this.buildKernelShapeComboBox(true); // default for 3D+ filtering is slice filtering
        }

        maskPanel.add(comboBoxKernelShape); // add the combo box to the panel
        setupBox.add(maskPanel); // the parameters-panel is at the top of the box

        JPanel colourPanel = new JPanel();
        colourPanel.setLayout(gbl);
        gbc.gridwidth = GridBagConstraints.REMAINDER;
        gbc.anchor = GridBagConstraints.WEST;
        colourPanel.setForeground(Color.black);
        colourPanel.setBorder(buildTitledBorder("Color channel selection")); // set the border ... "Colour channel
                                                                             // Selection"
        
        vectorGroup = new ButtonGroup();
        vectorButton = new JRadioButton("Vector filter on all colors together", true);
        vectorButton.setFont(serif12);
        vectorGroup.add(vectorButton);
        gbl.setConstraints(vectorButton, gbc);
        colourPanel.add(vectorButton);
        if (image.isColorImage()) {
            vectorButton.setEnabled(true);
        }
        else {
            vectorButton.setEnabled(false);
        }
        vectorButton.addActionListener(this);
        
        componentButton = new JRadioButton("Filter on each color separately", false);
        componentButton.setFont(serif12);
        vectorGroup.add(componentButton);
        gbl.setConstraints(componentButton, gbc);
        colourPanel.add(componentButton);
        if (image.isColorImage()) {
            componentButton.setEnabled(true);
        }
        else {
            componentButton.setEnabled(false);
        }
        componentButton.addActionListener(this);

        redChannel = new JCheckBox("Red Channel", true);
        redChannel.setEnabled(false);
        redChannel.setFont(serif12);
        gbl.setConstraints(redChannel, gbc);
        colourPanel.add(redChannel);

        greenChannel = new JCheckBox("Green Channel", true);
        greenChannel.setEnabled(false);
        greenChannel.setFont(serif12);
        gbl.setConstraints(greenChannel, gbc);
        colourPanel.add(greenChannel);

        blueChannel = new JCheckBox("Blue Channel", true);
        blueChannel.setEnabled(false);
        blueChannel.setFont(serif12);
        gbl.setConstraints(blueChannel, gbc);
        colourPanel.add(blueChannel);

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
            kernelThicknessPanel.setBorder(buildTitledBorder("Kernel Thickness")); // give the panel a border

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

        // get the index of the combo-box for kernelShape
        kernelShape = comboBoxKernelShape.getSelectedIndex();

        // verify iteration is within bounds
        tmpStr = textNIter.getText();

        if (testParameter(tmpStr, 1, 20)) {
            iters = Integer.valueOf(tmpStr).intValue();
        } else {
            textNIter.requestFocus();
            textNIter.selectAll();

            return false;
        }

        // verify Standard deviation is within bounds
        tmpStr = textSTDDeviation.getText();

        if (testParameter(tmpStr, 0, 9.9)) {
            stdDev = Float.valueOf(tmpStr).floatValue();
        } else {
            textSTDDeviation.requestFocus();
            textSTDDeviation.selectAll();

            return false;
        }

        vectorFilter = vectorButton.isSelected();
        red = redChannel.isSelected();
        green = greenChannel.isSelected();
        blue = blueChannel.isSelected();

        if (bySlice != null) {
            image25D = bySlice.isSelected();
        }

        return true;
    }
}
