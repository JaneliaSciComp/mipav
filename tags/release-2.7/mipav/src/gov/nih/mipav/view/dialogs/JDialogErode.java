package gov.nih.mipav.view.dialogs;


import gov.nih.mipav.model.algorithms.*;
import gov.nih.mipav.model.structures.*;

import gov.nih.mipav.view.*;

import java.awt.*;
import java.awt.event.*;

import java.util.*;

import javax.swing.*;


/**
 * Dialog to get user input, then call the algorithm. The user has the option to generate a new image or replace the
 * source image. In addition the user selects if the algorithm is applied to whole image or to the VOI regions. It
 * should be noted that the algorithms are executed in their own threads.
 *
 * @version  0.1 Nov 17, 1998
 * @author   Matthew J. McAuliffe, Ph.D.
 * @see      AlgorithmMorphology2D
 */
public class JDialogErode extends JDialogBase implements AlgorithmInterface, ScriptableInterface {

    //~ Static fields/initializers -------------------------------------------------------------------------------------

    /** Use serialVersionUID for interoperability. */
    private static final long serialVersionUID = -3069882376396094338L;

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** DOCUMENT ME! */
    private JComboBox comboBoxKernelErode;

    /** DOCUMENT ME! */
    private ButtonGroup destinationGroupErode;

    /** DOCUMENT ME! */
    private JPanel destinationPanelErode;

    /** DOCUMENT ME! */
    private int displayLoc; // Flag indicating if a new image is to be generated

    /** DOCUMENT ME! */
    private boolean do25D = false; // do a 2.5D erode on a 3D image

    /** DOCUMENT ME! */
    private AlgorithmMorphology25D erodeAlgo25D = null;

    /** DOCUMENT ME! */
    private AlgorithmMorphology2D erodeAlgo2D = null;

    /** DOCUMENT ME! */
    private AlgorithmMorphology3D erodeAlgo3D = null;

    /** DOCUMENT ME! */
    private ModelImage image; // source image

    /** DOCUMENT ME! */
    private JCheckBox image25D;

    /** DOCUMENT ME! */
    private ButtonGroup imageVOIGroup;

    /** DOCUMENT ME! */
    private JPanel imageVOIPanel;

    /** or if the source image is to be replaced. */
    private int iters;

    /** DOCUMENT ME! */
    private int kernelErode = 0;

    /** DOCUMENT ME! */
    private float kernelSizeErode;

    /** DOCUMENT ME! */
    private JLabel labelKernelErode;

    /** DOCUMENT ME! */
    private JLabel labelKernelSizeErode;

    /** DOCUMENT ME! */
    private JLabel labelNIterErode;

    /** DOCUMENT ME! */
    private JPanel maskPanelErode;

    /** DOCUMENT ME! */
    private JRadioButton newImage;

    /** DOCUMENT ME! */
    private boolean regionFlag = false;

    /** DOCUMENT ME! */
    private JRadioButton replaceImage;

    /** DOCUMENT ME! */
    private ModelImage resultImage = null; // result image

    /** DOCUMENT ME! */
    private JTextField textKernelSizeErode;

    /** DOCUMENT ME! */
    private JTextField textNIterErode;

    /** DOCUMENT ME! */
    private String[] titles;

    /** DOCUMENT ME! */
    private ViewUserInterface userInterface;

    /** DOCUMENT ME! */
    private JRadioButton VOIRegions;

    /** DOCUMENT ME! */
    private JRadioButton wholeImage;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Empty constructor needed for dynamic instantiation (used during scripting).
     */
    public JDialogErode() { }

    /**
     * Creates new erode dialog.
     *
     * @param  theParentFrame  Parent frame
     * @param  im              Source image
     */
    public JDialogErode(Frame theParentFrame, ModelImage im) {
        super(theParentFrame, true);

        if ((im.getType() != ModelImage.BOOLEAN) && (im.getType() != ModelImage.UBYTE) &&
                (im.getType() != ModelImage.USHORT)) {
            MipavUtil.displayError("Source Image must be Boolean or UByte or UShort");
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
    public JDialogErode(ViewUserInterface UI, ModelImage im) {
        super();

        if ((im.getType() != ModelImage.BOOLEAN) && (im.getType() != ModelImage.UBYTE) &&
                (im.getType() != ModelImage.USHORT)) {
            MipavUtil.displayError("Source Image must be Boolean or UByte or UShort");
            dispose();

            return;
        }

        userInterface = UI;
        image = im;
        parentFrame = image.getParentFrame();
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

        if (algorithm instanceof AlgorithmMorphology2D) {
            image.clearMask();

            if ((erodeAlgo2D.isCompleted() == true) && (resultImage != null)) {
                updateFileInfo(image, resultImage);
                resultImage.clearMask();

                // The algorithm has completed and produced a new image to be displayed.
                try {

                    // resultImage.setImageName("Eroded image");
                    imageFrame = new ViewJFrameImage(resultImage);
                } catch (OutOfMemoryError error) {
                    MipavUtil.displayError("Out of memory: unable to open new frame");
                }
            } else if (resultImage == null) {

                // These next lines set the titles in all frames where the source image is displayed to
                // image name so as to indicate that the image is now unlocked!
                // The image frames are enabled and then registed to the userinterface.
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
            }

        } else if (algorithm instanceof AlgorithmMorphology25D) {
            image.clearMask();

            if ((erodeAlgo25D.isCompleted() == true) && (resultImage != null)) {
                updateFileInfo(image, resultImage);
                resultImage.clearMask();

                // The algorithm has completed and produced a new image to be displayed.
                try {

                    // resultImage.setImageName("Eroded image");
                    imageFrame = new ViewJFrameImage(resultImage);
                } catch (OutOfMemoryError error) {
                    MipavUtil.displayError("Out of memory: unable to open new frame");
                }
            } else if (resultImage == null) {

                // These next lines set the titles in all frames where the source image is displayed to
                // image name so as to indicate that the image is now unlocked!
                // The image frames are enabled and then registed to the userinterface.
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
            }
        } else if (algorithm instanceof AlgorithmMorphology3D) {
            image.clearMask();

            if ((erodeAlgo3D.isCompleted() == true) && (resultImage != null)) {
                updateFileInfo(image, resultImage);
                resultImage.clearMask();

                // The algorithm has completed and produced a new image to be displayed.
                try {

                    // resultImage.setImageName("Eroded image");
                    imageFrame = new ViewJFrameImage(resultImage);
                } catch (OutOfMemoryError error) {
                    MipavUtil.displayError("Out of memory: unable to open new frame");
                }
            } else if (resultImage == null) {

                // These next lines set the titles in all frames where the source image is displayed to
                // image name so as to indicate that the image is now unlocked!
                // The image frames are enabled and then registed to the userinterface.
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
            }
        }

        insertScriptLine(algorithm);

        // Update frame
        // ((ViewJFrameBase)parentFrame).updateImages(true);

        if (erodeAlgo2D != null) {
            erodeAlgo2D.finalize();
            erodeAlgo2D = null;
        }

        if (erodeAlgo25D != null) {
            erodeAlgo25D.finalize();
            erodeAlgo25D = null;
        }

        if (erodeAlgo3D != null) {
            erodeAlgo3D.finalize();
            erodeAlgo3D = null;
        }

        dispose();
    }

    /**
     * Accessor that returns the image.
     *
     * @return  the result image
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

                userInterface.getScriptDialog().append("Erode " +
                                                       userInterface.getScriptDialog().getVar(image.getImageName()) +
                                                       " ");

                if (displayLoc == NEW) {
                    userInterface.getScriptDialog().putVar(resultImage.getImageName());
                    userInterface.getScriptDialog().append(userInterface.getScriptDialog().getVar(resultImage.getImageName()) +
                                                           " " + regionFlag + " " + iters + " " + kernelErode + " " +
                                                           kernelSizeErode + " " + do25D + "\n");
                } else {
                    userInterface.getScriptDialog().append(userInterface.getScriptDialog().getVar(image.getImageName()) +
                                                           " " + regionFlag + " " + iters + " " + kernelErode + " " +
                                                           kernelSizeErode + " " + do25D + "\n");

                }
            }
        }
    }

    // *******************************************************************
    // ************************* Item Events ****************************
    // *******************************************************************

    /**
     * Sets text fields enabled depending upon which item in the combo box is selected.
     *
     * @param  event  DOCUMENT ME!
     */
    public void itemStateChanged(ItemEvent event) {
        Object source = event.getSource();

        if (source == comboBoxKernelErode) {

            if (comboBoxKernelErode.getSelectedIndex() == 2) {
                textKernelSizeErode.setEnabled(true);
                labelKernelSizeErode.setEnabled(true);
            } else {
                textKernelSizeErode.setEnabled(false);
                labelKernelSizeErode.setEnabled(false);
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

        if ((im.getType() != ModelImage.BOOLEAN) && (im.getType() != ModelImage.UBYTE) &&
                (im.getType() != ModelImage.USHORT)) {
            MipavUtil.displayError("Source Image must be Boolean or UByte or UShort");
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
            setRegionFlag(parser.getNextBoolean());
            setNumErosions(parser.getNextInteger());
            setKernelType(parser.getNextInteger());
            setKernelSize(parser.getNextFloat());
            setImage25D(parser.getNextBoolean());
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
     * Process the image in 2.5D.
     *
     * @param  b  whether to do 2.5D morphology
     */
    public void setImage25D(boolean b) {
        do25D = b;
    }

    /**
     * Accessor that sets the kernel size.
     *
     * @param  s  the desired kernel size
     */
    public void setKernelSize(float s) {
        kernelSizeErode = s;
    }

    /**
     * Accessor that sets the kernel type to use.
     *
     * @param  krnl  the kernel type to use (either AlgorithmMorphology2D.CONNECTED4, AlgorithmMorphology2D.CONNECTED12,
     *               AlgorithmMorphology2D.SIZED_CIRCLE, AlgorithmMorphology3D.CONNECTED6,
     *               AlgorithmMorphology3D.CONNECTED24, AlgorithmMorphology3D.SIZED_SPHERE (or the
     *               AlgorithmMorphology25D ones))
     */
    public void setKernelType(int krnl) {
        kernelErode = krnl;
    }

    /**
     * Accessor that sets the number of erosions to perform.
     *
     * @param  n  The number of erosions to do.
     */
    public void setNumErosions(int n) {
        iters = n;
    }

    /**
     * Accessor that sets the region flag.
     *
     * @param  flag  <code>true</code> indicates the whole image is eroded, <code>false</code> indicates a region.
     */
    public void setRegionFlag(boolean flag) {
        regionFlag = flag;
    }

    /**
     * Builds kernel combo box.
     */
    private void buildComboBox() {

        comboBoxKernelErode = new JComboBox();
        comboBoxKernelErode.setFont(serif12);
        comboBoxKernelErode.setBackground(Color.white);

        if (image.getNDims() == 2) {
            comboBoxKernelErode.addItem("3x3 -  4 connected");
            comboBoxKernelErode.addItem("5x5 - 12 connected");
            comboBoxKernelErode.addItem("User sized circle.");
        } else if (image.getNDims() == 3) {
            comboBoxKernelErode.addItem("3x3x3 -  6 connected (2.5D: 4)");
            comboBoxKernelErode.addItem("5x5x5 - 24 connected (2.5D: 12)");
            comboBoxKernelErode.addItem("User sized sphere.");
        }
    }

    /**
     * Once all the necessary variables are set, call the Gaussian Blur algorithm based on what type of image this is
     * and whether or not there is a separate destination image.
     */
    private void callAlgorithm() {
        String name = makeImageName(image.getImageName(), "_eroded");

        if (image.getNDims() == 2) { // source image is 2D

            if (displayLoc == NEW) {

                try {
                    resultImage = (ModelImage) image.clone();
                    resultImage.setImageName(name);

                    // Make algorithm
                    erodeAlgo2D = new AlgorithmMorphology2D(resultImage, kernelErode, kernelSizeErode,
                                                            AlgorithmMorphology2D.ERODE, 0, iters, 0, 0, regionFlag);

                    if (regionFlag == false) {
                        erodeAlgo2D.setMask(image.generateVOIMask());
                    }

                    // This is very important. Adding this object as a listener allows the algorithm to
                    // notify this object when it has completed or failed. See algorithm performed event.
                    // This is made possible by implementing AlgorithmedPerformed interface
                    erodeAlgo2D.addListener(this);

                    // Hide dialog
                    setVisible(false);

                    if (isRunInSeparateThread()) {

                        // Start the thread as a low priority because we wish to still have user interface work fast.
                        if (erodeAlgo2D.startMethod(Thread.MIN_PRIORITY) == false) {
                            MipavUtil.displayError("A thread is already running on this object");
                        }
                    } else {
                        if (!userInterface.isAppFrameVisible()) {
                            erodeAlgo2D.setProgressBarVisible(false);
                        }

                        erodeAlgo2D.run();
                    }
                } catch (OutOfMemoryError x) {
                    MipavUtil.displayError("Dialog erode: unable to allocate enough memory");

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
                    erodeAlgo2D = new AlgorithmMorphology2D(image, kernelErode, kernelSizeErode,
                                                            AlgorithmMorphology2D.ERODE, 0, iters, 0, 0, regionFlag);

                    if (regionFlag == false) {
                        erodeAlgo2D.setMask(image.generateVOIMask());
                    }

                    // This is very important. Adding this object as a listener allows the algorithm to
                    // notify this object when it has completed or failed. See algorithm performed event.
                    // This is made possible by implementing AlgorithmedPerformed interface
                    erodeAlgo2D.addListener(this);

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

                        // Start the thread as a low priority because we wish to still have user interface.
                        if (erodeAlgo2D.startMethod(Thread.MIN_PRIORITY) == false) {
                            MipavUtil.displayError("A thread is already running on this object");
                        }
                    } else {
                        if (!userInterface.isAppFrameVisible()) {
                            erodeAlgo2D.setProgressBarVisible(false);
                        }

                        erodeAlgo2D.run();
                    }
                } catch (OutOfMemoryError x) {
                    MipavUtil.displayError("Dialog erode: unable to allocate enough memory");

                    return;
                }
            }
        } else if ((image.getNDims() == 3) && !do25D) {

            if (displayLoc == NEW) {

                try {
                    resultImage = (ModelImage) image.clone();
                    resultImage.setImageName(name);

                    // Make algorithm
                    erodeAlgo3D = new AlgorithmMorphology3D(resultImage, kernelErode, kernelSizeErode,
                                                            AlgorithmMorphology3D.ERODE, 0, iters, 0, 0, regionFlag);

                    if (regionFlag == false) {
                        erodeAlgo3D.setMask(image.generateVOIMask());
                    }

                    // This is very important. Adding this object as a listener allows the algorithm to
                    // notify this object when it has completed or failed. See algorithm performed event.
                    // This is made possible by implementing AlgorithmedPerformed interface
                    erodeAlgo3D.addListener(this);

                    // Hide dialog
                    setVisible(false);

                    if (isRunInSeparateThread()) {

                        // Start the thread as a low priority because we wish to still have user interface work fast
                        if (erodeAlgo3D.startMethod(Thread.MIN_PRIORITY) == false) {
                            MipavUtil.displayError("A thread is already running on this object");
                        }
                    } else {
                        if (!userInterface.isAppFrameVisible()) {
                            erodeAlgo3D.setProgressBarVisible(false);
                        }

                        erodeAlgo3D.run();
                    }
                } catch (OutOfMemoryError x) {
                    MipavUtil.displayError("Dialog erode: unable to allocate enough memory");

                    if (resultImage != null) {
                        resultImage.disposeLocal(); // Clean up image memory
                        resultImage = null;
                    }

                    return;
                }
            } else {

                try {

                    // Make algorithm
                    erodeAlgo3D = new AlgorithmMorphology3D(image, kernelErode, kernelSizeErode,
                                                            AlgorithmMorphology3D.ERODE, 0, iters, 0, 0, regionFlag);

                    if (regionFlag == false) {
                        erodeAlgo3D.setMask(image.generateVOIMask());
                    }

                    // This is very important. Adding this object as a listener allows the algorithm to
                    // notify this object when it has completed or failed. See algorithm performed event.
                    // This is made possible by implementing AlgorithmedPerformed interface
                    erodeAlgo3D.addListener(this);

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

                        // Start the thread as a low priority because we wish to still have user interface work fast
                        if (erodeAlgo3D.startMethod(Thread.MIN_PRIORITY) == false) {
                            MipavUtil.displayError("A thread is already running on this object");
                        }
                    } else {
                        if (!userInterface.isAppFrameVisible()) {
                            erodeAlgo3D.setProgressBarVisible(false);
                        }

                        erodeAlgo3D.run();
                    }
                } catch (OutOfMemoryError x) {
                    MipavUtil.displayError("Dialog erode: unable to allocate enough memory");

                    return;
                }
            }
        } else if (do25D) {

            // convert to 2.5d kernel type
            if (kernelErode == AlgorithmMorphology3D.CONNECTED6) {
                kernelErode = AlgorithmMorphology25D.CONNECTED4;
            } else if (kernelErode == AlgorithmMorphology3D.CONNECTED24) {
                kernelErode = AlgorithmMorphology25D.CONNECTED12;
            } else if (kernelErode == AlgorithmMorphology3D.SIZED_SPHERE) {
                kernelErode = AlgorithmMorphology25D.SIZED_CIRCLE;
            }

            if (displayLoc == NEW) {

                try {
                    resultImage = (ModelImage) image.clone();
                    resultImage.setImageName(name);

                    // Make algorithm
                    erodeAlgo25D = new AlgorithmMorphology25D(resultImage, kernelErode, kernelSizeErode,
                                                              AlgorithmMorphology25D.ERODE, 0, iters, 0, 0, regionFlag);

                    if (regionFlag == false) {
                        erodeAlgo25D.setMask(image.generateVOIMask());
                    }

                    // This is very important. Adding this object as a listener allows the algorithm to
                    // notify this object when it has completed or failed. See algorithm performed event.
                    // This is made possible by implementing AlgorithmedPerformed interface
                    erodeAlgo25D.addListener(this);

                    // Hide dialog
                    setVisible(false);

                    if (isRunInSeparateThread()) {

                        // Start the thread as a low priority because we wish to still have user interface work fast.
                        if (erodeAlgo25D.startMethod(Thread.MIN_PRIORITY) == false) {
                            MipavUtil.displayError("A thread is already running on this object");
                        }
                    } else {
                        if (!userInterface.isAppFrameVisible()) {
                            erodeAlgo25D.setProgressBarVisible(false);
                        }

                        erodeAlgo25D.run();
                    }
                } catch (OutOfMemoryError x) {
                    MipavUtil.displayError("Dialog erode: unable to allocate enough memory");

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
                    erodeAlgo25D = new AlgorithmMorphology25D(image, kernelErode, kernelSizeErode,
                                                              AlgorithmMorphology25D.ERODE, 0, iters, 0, 0, regionFlag);

                    if (regionFlag == false) {
                        erodeAlgo25D.setMask(image.generateVOIMask());
                    }

                    // This is very important. Adding this object as a listener allows the algorithm to
                    // notify this object when it has completed or failed. See algorithm performed event.
                    // This is made possible by implementing AlgorithmedPerformed interface
                    erodeAlgo25D.addListener(this);

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

                        // Start the thread as a low priority because we wish to still have user interface.
                        if (erodeAlgo25D.startMethod(Thread.MIN_PRIORITY) == false) {
                            MipavUtil.displayError("A thread is already running on this object");
                        }
                    } else {
                        if (!userInterface.isAppFrameVisible()) {
                            erodeAlgo25D.setProgressBarVisible(false);
                        }

                        erodeAlgo25D.run();
                    }
                } catch (OutOfMemoryError x) {
                    MipavUtil.displayError("Dialog erode: unable to allocate enough memory");

                    return;
                }
            }
        }
    }

    /**
     * Sets up the GUI (panels, buttons, etc) and displays it on the screen.
     */
    private void init() {
        setForeground(Color.black);
        setTitle("Erode");

        labelNIterErode = new JLabel("Number of erosions (1-20)");
        labelNIterErode.setForeground(Color.black);
        labelNIterErode.setFont(serif12);

        textNIterErode = new JTextField(5);
        textNIterErode.setText("1");
        textNIterErode.setFont(serif12);

        labelKernelErode = new JLabel("Kernel selection");
        labelKernelErode.setForeground(Color.black);
        labelKernelErode.setFont(serif12);

        buildComboBox();
        comboBoxKernelErode.addItemListener(this);

        String unitString = null;

        unitString = new String(image.getFileInfo()[0].sUnits[image.getFileInfo()[0].getUnitsOfMeasure(0)]);

        if (image.getNDims() == 2) {
            labelKernelSizeErode = new JLabel("Circle diameter - " + unitString);
        } else {
            labelKernelSizeErode = new JLabel("Sphere diameter - " + unitString);
        }

        labelKernelSizeErode.setForeground(Color.black);
        labelKernelSizeErode.setBounds(75, 120, 155, 25);
        labelKernelSizeErode.setFont(serif12);
        labelKernelSizeErode.setEnabled(false);

        textKernelSizeErode = new JTextField(5);
        textKernelSizeErode.setText("1");
        textKernelSizeErode.setFont(serif12);
        textKernelSizeErode.setEnabled(false);

        maskPanelErode = new JPanel(new GridBagLayout());
        maskPanelErode.setForeground(Color.black);
        maskPanelErode.setBorder(buildTitledBorder("Parameters"));

        GridBagConstraints gbc = new GridBagConstraints();

        gbc.gridwidth = 1;
        gbc.gridheight = 1;
        gbc.anchor = gbc.WEST;
        gbc.insets = new Insets(5, 5, 5, 5);

        gbc.gridx = 0;
        gbc.gridy = 0;
        gbc.weightx = 0;
        gbc.fill = gbc.NONE;
        maskPanelErode.add(labelNIterErode, gbc);
        gbc.gridx = 1;
        gbc.gridy = 0;
        gbc.weightx = 1;
        gbc.fill = gbc.HORIZONTAL;
        maskPanelErode.add(textNIterErode, gbc);
        gbc.gridx = 0;
        gbc.gridy = 1;
        gbc.weightx = 0;
        gbc.fill = gbc.NONE;
        maskPanelErode.add(labelKernelErode, gbc);
        gbc.gridx = 1;
        gbc.gridy = 1;
        gbc.weightx = 1;
        gbc.fill = gbc.HORIZONTAL;
        maskPanelErode.add(comboBoxKernelErode, gbc);
        gbc.gridx = 0;
        gbc.gridy = 2;
        gbc.weightx = 0;
        gbc.fill = gbc.NONE;
        maskPanelErode.add(labelKernelSizeErode, gbc);
        gbc.gridx = 1;
        gbc.gridy = 2;
        gbc.weightx = 1;
        gbc.fill = gbc.HORIZONTAL;
        maskPanelErode.add(textKernelSizeErode, gbc);

        destinationPanelErode = new JPanel(new GridBagLayout());
        destinationPanelErode.setForeground(Color.black);
        destinationPanelErode.setBorder(buildTitledBorder("Destination"));

        destinationGroupErode = new ButtonGroup();
        newImage = new JRadioButton("New image", true);
        newImage.setFont(serif12);
        destinationGroupErode.add(newImage);

        replaceImage = new JRadioButton("Replace image", false);
        replaceImage.setBounds(10, 42, 120, 25);
        replaceImage.setFont(serif12);
        destinationGroupErode.add(replaceImage);

        gbc.gridx = 0;
        gbc.gridy = 0;
        gbc.insets = new Insets(0, 0, 0, 0);
        destinationPanelErode.add(newImage, gbc);
        gbc.gridy = 1;
        destinationPanelErode.add(replaceImage, gbc);

        imageVOIPanel = new JPanel(new GridBagLayout());
        imageVOIPanel.setForeground(Color.black);
        imageVOIPanel.setBorder(buildTitledBorder("Erode"));

        imageVOIGroup = new ButtonGroup();
        wholeImage = new JRadioButton("Whole image", true);
        wholeImage.setFont(serif12);
        imageVOIGroup.add(wholeImage);

        VOIRegions = new JRadioButton("VOI region(s)", false);
        VOIRegions.setFont(serif12);
        imageVOIGroup.add(VOIRegions);

        image25D = new JCheckBox("Process image in 2.5D", false);
        image25D.setFont(serif12);

        gbc.gridx = 0;
        gbc.gridy = 0;
        imageVOIPanel.add(wholeImage, gbc);
        gbc.gridy = 1;
        imageVOIPanel.add(VOIRegions, gbc);

        // Only if the image is unlocked can it be replaced.
        if (image.getLockStatus() == ModelStorageBase.UNLOCKED) {
            replaceImage.setEnabled(true);
        } else {
            replaceImage.setEnabled(false);
        }

        gbc.gridy = 2;
        imageVOIPanel.add(image25D, gbc);

        if (image.getNDims() == 3) {
            image25D.setEnabled(true);
        } else {
            image25D.setEnabled(false);
        }

        JPanel mainPanel = new JPanel(new GridBagLayout());

        gbc.gridx = 0;
        gbc.gridy = 0;
        gbc.gridwidth = 2;
        gbc.weightx = 1;
        gbc.fill = gbc.HORIZONTAL;
        mainPanel.add(maskPanelErode, gbc);
        gbc.gridy = 1;
        gbc.gridwidth = 1;
        gbc.fill = gbc.BOTH;
        mainPanel.add(destinationPanelErode, gbc);
        gbc.gridx = 1;
        mainPanel.add(imageVOIPanel, gbc);

        JPanel buttonPanel = new JPanel();

        buildOKButton();
        buttonPanel.add(OKButton);
        buildCancelButton();
        buttonPanel.add(cancelButton);

        getContentPane().add(mainPanel);
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
        String tmpStr;

        System.gc();

        if (replaceImage.isSelected()) {
            displayLoc = REPLACE;
        } else if (newImage.isSelected()) {
            displayLoc = NEW;
        }

        if (wholeImage.isSelected()) {
            regionFlag = true;
        } else if (VOIRegions.isSelected()) {
            regionFlag = false;
        }

        do25D = image25D.isSelected();

        tmpStr = textNIterErode.getText();

        if (testParameter(tmpStr, 1, 20)) {
            iters = Integer.valueOf(tmpStr).intValue();
        } else {
            textNIterErode.requestFocus();
            textNIterErode.selectAll();

            return false;
        }

        tmpStr = textKernelSizeErode.getText();

        float max = (float) (image.getExtents()[0] * image.getFileInfo()[0].getResolutions()[0] / 5);

        // if ( max < 10 ) max = 10;
        if (textKernelSizeErode.isEnabled() == true) {

            if (testParameter(tmpStr, 0, max)) {
                kernelSizeErode = Float.valueOf(tmpStr).floatValue();
            } else {
                textKernelSizeErode.requestFocus();
                textKernelSizeErode.selectAll();

                return false;
            }
        }

        if (image.getNDims() == 2) {

            if (comboBoxKernelErode.getSelectedIndex() == 0) {
                kernelErode = AlgorithmMorphology2D.CONNECTED4;
            } else if (comboBoxKernelErode.getSelectedIndex() == 1) {
                kernelErode = AlgorithmMorphology2D.CONNECTED12;
            } else if (comboBoxKernelErode.getSelectedIndex() == 2) {
                kernelErode = AlgorithmMorphology2D.SIZED_CIRCLE;
            }
        } else if (image.getNDims() == 3) {

            if (comboBoxKernelErode.getSelectedIndex() == 0) {
                kernelErode = AlgorithmMorphology3D.CONNECTED6;
            } else if (comboBoxKernelErode.getSelectedIndex() == 1) {
                kernelErode = AlgorithmMorphology3D.CONNECTED24;
            } else if (comboBoxKernelErode.getSelectedIndex() == 2) {
                kernelErode = AlgorithmMorphology3D.SIZED_SPHERE;
            }
        }

        return true;
    }

}
