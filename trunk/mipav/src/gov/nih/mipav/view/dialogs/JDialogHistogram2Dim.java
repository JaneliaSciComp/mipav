package gov.nih.mipav.view.dialogs;


import gov.nih.mipav.model.algorithms.*;
import gov.nih.mipav.model.scripting.*;
import gov.nih.mipav.model.scripting.parameters.*;
import gov.nih.mipav.model.structures.*;

import gov.nih.mipav.view.*;

import java.awt.*;
import java.awt.event.*;

import java.util.*;

import javax.swing.*;


/**
 * Dialog to get user input Create 2D histogram of images with equal dimensions or 2D histograms with 2 colors inside a
 * RGB image. Algorithms are executed in their own thread.
 */
public class JDialogHistogram2Dim extends JDialogScriptableBase implements AlgorithmInterface {

    //~ Static fields/initializers -------------------------------------------------------------------------------------

    /** Use serialVersionUID for interoperability. */
    private static final long serialVersionUID = -2641827384253148080L;

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** Number of bins for first image. */
    private int bin1;

    /** Default number of bins for first image. Default is 256. */
    private boolean bin1Default;

    /** Label description for bin1. */
    private JLabel bin1Label;

    /** Textfield for bin1. */
    private JTextField bin1Text;

    /** Number of bins for second image. */
    private int bin2;

    /** Default number of bins for first image. Default is 256. */
    private boolean bin2Default;

    /** Label description for bin1. */
    private JLabel bin2Label;

    /** Textfield for bin2. */
    private JTextField bin2Text;

    /** Checkbox to select blue image. */
    private JCheckBox blueCheckBox;

    /** Number of colors present. */
    private int colorsPresent = 0;

    /** If true, the range of second image is same as the range of the first image. */
    private boolean doLinearRescale = true;

    /** If true, calculates the log of the result image for better visualization. */
    private boolean doLogResult = true;

    /** First image (Source image). */
    private ModelImage firstImage;

    /** Checkbox to select green image. */
    private JCheckBox greenCheckBox;

    /** Instance of AlgorithmHistogram2Dim. */
    private AlgorithmHistogram2Dim histogram2DimAlgo = null;

    /** Combobox to select the second image. */
    private JComboBox imageComboBox;

    /** Image label. */
    private JLabel labelImage;

    /** Checkbox to select linear rescaling of second image. */
    private JCheckBox linearCheckbox;

    /** Max intensity values in Red, Green and Blue images respectively. */
    private double maxR, maxG, maxB;

    /** Min. intensity values in Red, Green and Blue images respectively */
    private double minR, minG, minB;

    /** Possible intensity values for second image. */
    private double possibleInt2Values;

    /** Possible intensity values for first image. */
    private double possibleIntValues;

    /** Checkbox to select Red image. */
    private JCheckBox redCheckBox;

    /** Checkbox to display the log of result image. */
    private JCheckBox resultCheckbox;

    /** Image where the result is stored. */
    private ModelImage resultImage = null;

    /** Second image (Base image). */
    private ModelImage secondImage = null;

    /** String titles for the new windows. */
    private String[] titles;

    /** If true, blue image is selected as of the two images for calculating histogram. */
    private boolean useBlue = false;

    /** If true, green image is selected as of the two images for calculating histogram. */
    private boolean useGreen = false;

    /** If true, red image is selected as of the two images for calculating histogram. */
    private boolean useRed = false;

    /** Keeps record of the present structure of the application. */
    private ViewUserInterface userInterface;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Empty constructor needed for dynamic instantiation (used during scripting).
     */
    public JDialogHistogram2Dim() { }

    /**
     * Creates new dialog.
     *
     * @param  theParentFrame  Parent frame
     * @param  im              Source image
     */
    public JDialogHistogram2Dim(Frame theParentFrame, ModelImage im) {
        super(theParentFrame, false);
        firstImage = im;
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
        } else if (command.equals("Help")) {

            if (firstImage.isColorImage()) {
                MipavUtil.showHelp("19054");
            } else {
                MipavUtil.showHelp("19052");
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

        if (algorithm instanceof AlgorithmHistogram2Dim) {
            firstImage.clearMask();

            if ((histogram2DimAlgo.isCompleted() == true) && (resultImage != null)) {
                // The algorithm has completed and produced a new image to be displayed.

                updateFileInfo(firstImage, resultImage);
                resultImage.clearMask();

                try {
                    new ViewJFrameImage(resultImage, null, new Dimension(610, 200));
                } catch (OutOfMemoryError error) {
                    System.gc();
                    MipavUtil.displayError("Out of memory: unable to open new frame");
                }
            } else if (resultImage == null) {

                // These next lines set the titles in all frames where the source image is displayed to
                // image name so as to indicate that the image is now unlocked!
                // The image frames are enabled and then registered to the userinterface.
                Vector<ViewImageUpdateInterface> imageFrames = firstImage.getImageFrameVector();

                for (int i = 0; i < imageFrames.size(); i++) {
                    ((ViewJFrameBase) (imageFrames.elementAt(i))).setTitle(titles[i]);
                    ((ViewJFrameBase) (imageFrames.elementAt(i))).setEnabled(true);

                    if (((Frame) (imageFrames.elementAt(i))) != parentFrame) {
                        userInterface.registerFrame((Frame) (imageFrames.elementAt(i)));

                    }
                }

                if (parentFrame != null) {
                    userInterface.registerFrame(parentFrame);
                }

                firstImage.notifyImageDisplayListeners(null, true);
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

        histogram2DimAlgo.finalize();
        histogram2DimAlgo = null;
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

    // ************************* Item Events ****************************
    // *******************************************************************

    /**
     * itemStateChanged.
     *
     * @param  event  DOCUMENT ME!
     */
    public void itemStateChanged(ItemEvent event) {
        Object source = event.getSource();
        String tmpStr;

        if (source == imageComboBox) {
            userInterface = ViewUserInterface.getReference();

            String selectedName = (String) imageComboBox.getSelectedItem();
            secondImage = userInterface.getRegisteredImageByName(selectedName);

            possibleInt2Values = secondImage.getMax() - secondImage.getMin() + 1;

            if (secondImage.getMax() == secondImage.getMin()) {
                linearCheckbox.setSelected(false);
                linearCheckbox.setEnabled(false);
            } else {
                linearCheckbox.setEnabled(true);
            }

            doLinearRescale = linearCheckbox.isSelected();
            tmpStr = bin2Text.getText();
            bin2 = Integer.parseInt(tmpStr);

            if ((bin2 > Math.round(possibleInt2Values)) && (!doLinearRescale) &&
                    ((secondImage.getType() == ModelStorageBase.BYTE) ||
                         (secondImage.getType() == ModelStorageBase.UBYTE) ||
                         (secondImage.getType() == ModelStorageBase.SHORT) ||
                         (secondImage.getType() == ModelStorageBase.USHORT) ||
                         (secondImage.getType() == ModelStorageBase.INTEGER) ||
                         (secondImage.getType() == ModelStorageBase.UINTEGER) ||
                         (secondImage.getType() == ModelStorageBase.LONG))) {
                bin2 = (int) Math.round(possibleInt2Values);
            }

            bin2Text.setText(String.valueOf(bin2));

        } // if ( source == imageComboBox)
        else if (source == linearCheckbox) {
            doLinearRescale = linearCheckbox.isSelected();

            if (firstImage.isColorImage()) {

                if (useBlue) {
                    possibleInt2Values = maxB - minB + 1;
                } else {
                    possibleInt2Values = maxG - minG + 1;
                }

                tmpStr = bin2Text.getText();

                if ((!doLinearRescale) &&
                        ((firstImage.getType() == ModelStorageBase.ARGB) ||
                             (firstImage.getType() == ModelStorageBase.ARGB_USHORT))) {
                    bin2 = (int) Math.round(possibleInt2Values);
                } else if (doLinearRescale) {
                    bin2 = bin1;
                }
            } // if (firstImage.isColorImage())
            else { // not color image
                possibleInt2Values = secondImage.getMax() - secondImage.getMin() + 1;
                tmpStr = bin2Text.getText();
                bin2 = Integer.parseInt(tmpStr);

                if ((!doLinearRescale) &&
                        ((secondImage.getType() == ModelStorageBase.BYTE) ||
                             (secondImage.getType() == ModelStorageBase.UBYTE) ||
                             (secondImage.getType() == ModelStorageBase.SHORT) ||
                             (secondImage.getType() == ModelStorageBase.USHORT) ||
                             (secondImage.getType() == ModelStorageBase.INTEGER) ||
                             (secondImage.getType() == ModelStorageBase.UINTEGER) ||
                             (secondImage.getType() == ModelStorageBase.LONG))) {
                    bin2 = (int) Math.round(possibleInt2Values);
                } else if (doLinearRescale) {
                    bin2 = bin1;
                }
            } // else not color image

            bin2Text.setText(String.valueOf(bin2));
        } // else if (source == linearCheckbox)
        else if ((source == redCheckBox) || (source == greenCheckBox) || (source == blueCheckBox)) {

            // Only process if 2 checkBoxes are selected and 1 is not selected
            if (((redCheckBox.isSelected()) && (greenCheckBox.isSelected()) && (!blueCheckBox.isSelected())) ||
                    ((redCheckBox.isSelected()) && (!greenCheckBox.isSelected()) && (blueCheckBox.isSelected())) ||
                    ((!redCheckBox.isSelected()) && (greenCheckBox.isSelected()) && (blueCheckBox.isSelected()))) {
                useRed = redCheckBox.isSelected();
                useGreen = greenCheckBox.isSelected();
                useBlue = blueCheckBox.isSelected();
                bin1 = 256;

                if (useRed) {
                    possibleIntValues = firstImage.getMaxR() - firstImage.getMinR() + 1;
                } else {
                    possibleIntValues = firstImage.getMaxG() - firstImage.getMinG() + 1;
                }

                if (useBlue) {
                    possibleInt2Values = firstImage.getMaxB() - firstImage.getMinB() + 1;
                } else {
                    possibleInt2Values = firstImage.getMaxG() - firstImage.getMinG() + 1;
                }

                if (((firstImage.getType() == ModelStorageBase.ARGB) ||
                         (firstImage.getType() == ModelStorageBase.ARGB_USHORT)) && (possibleIntValues < 256)) {
                    bin1 = (int) Math.round(possibleIntValues);
                }

                if (doLinearRescale) {
                    bin2 = bin1;
                } else {
                    bin2 = 256;

                    if (((firstImage.getType() == ModelStorageBase.ARGB) ||
                             (firstImage.getType() == ModelStorageBase.ARGB_USHORT)) && (possibleInt2Values < 256)) {
                        bin2 = (int) Math.round(possibleInt2Values);
                    }
                }

                bin1Text.setText(String.valueOf(bin1));
                bin2Text.setText(String.valueOf(bin2));

                if (useRed) {
                    bin1Label.setText("Red bin number ");
                } else {
                    bin1Label.setText("Green bin number ");
                }

                if (useBlue) {
                    bin2Label.setText("Blue bin number ");
                    linearCheckbox.setText("Linearly rescale blue");
                } else {
                    bin2Label.setText("Green bin number ");
                    linearCheckbox.setText("Linearly rescale green");
                }
            }
        }
    }

    /**
     * Accessor that sets bin1.
     *
     * @param  bin1  DOCUMENT ME!
     */
    public void setBin1(int bin1) {
        this.bin1 = bin1;
    }

    /**
     * DOCUMENT ME!
     *
     * @param  bin1Default  DOCUMENT ME!
     */
    public void setBin1Default(boolean bin1Default) {
        this.bin1Default = bin1Default;
    }

    /**
     * Accessor that sets bin2.
     *
     * @param  bin2  DOCUMENT ME!
     */
    public void setBin2(int bin2) {
        this.bin2 = bin2;
    }

    /**
     * DOCUMENT ME!
     *
     * @param  bin2Default  DOCUMENT ME!
     */
    public void setBin2Default(boolean bin2Default) {
        this.bin2Default = bin2Default;
    }

    /**
     * Accessor that sets whether or not linear rescaling occurs.
     *
     * @param  doLinearRescale  DOCUMENT ME!
     */
    public void setDoLinearRescale(boolean doLinearRescale) {
        this.doLinearRescale = doLinearRescale;
    }


    /**
     * Accessor that sets whether or not log of result image is displayed.
     *
     * @param  doLogResult  If true, displays the log of result image for better visualization
     */
    public void setDoLogResult(boolean doLogResult) {
        this.doLogResult = doLogResult;
    }

    /**
     * Accessor to set the secondImage.
     *
     * @param  secondImage  DOCUMENT ME!
     */
    public void setSecondImage(ModelImage secondImage) {
        this.secondImage = secondImage;
    }

    /**
     * Accessor that setes if the blue channel is used.
     *
     * @param  useBlue  DOCUMENT ME!
     */
    public void setUseBlue(boolean useBlue) {
        this.useBlue = useBlue;
    }

    /**
     * Accessor that sets if the green channel is used.
     *
     * @param  useGreen  DOCUMENT ME!
     */
    public void setUseGreen(boolean useGreen) {
        this.useGreen = useGreen;
    }

    /**
     * Accessor that sets if the red channel is used.
     *
     * @param  useRed  DOCUMENT ME!
     */
    public void setUseRed(boolean useRed) {
        this.useRed = useRed;
    }

    /**
     * DOCUMENT ME!
     */
    protected void callAlgorithm() {
        String name = makeImageName(firstImage.getImageName(), "_hist2Dim");

        try {
            int[] extents = new int[2];
            extents[0] = bin1;
            extents[1] = bin2;
            resultImage = new ModelImage(ModelStorageBase.DOUBLE, extents, name);

            // Make algorithm
            if (firstImage.isColorImage()) {
                histogram2DimAlgo = new AlgorithmHistogram2Dim(resultImage, firstImage, doLinearRescale, doLogResult,
                                                               bin1, bin2, useRed, useGreen, useBlue);
            } else {
                histogram2DimAlgo = new AlgorithmHistogram2Dim(resultImage, firstImage, secondImage, doLinearRescale,
                                                               doLogResult, bin1, bin2);
            }

            // This is very important. Adding this object as a listener allows the algorithm to
            // notify this object when it has completed of failed. See algorithm performed event.
            // This is made possible by implementing AlgorithmedPerformed interface
            histogram2DimAlgo.addListener(this);

            createProgressBar(firstImage.getImageName(), histogram2DimAlgo);

            // Hide dialog
            setVisible(false);

            if (isRunInSeparateThread()) {

                // Start the thread as a low priority because we wish to still have user interface work fast.
                if (histogram2DimAlgo.startMethod(Thread.MIN_PRIORITY) == false) {
                    MipavUtil.displayError("A thread is already running on this object");
                }
            } else {
                histogram2DimAlgo.run();
            }
        } catch (OutOfMemoryError x) {

            if (resultImage != null) {
                resultImage.disposeLocal(); // Clean up memory of result image
                resultImage = null;
            }

            System.gc();
            MipavUtil.displayError("Dialog Histogram 2Dim: unable to allocate enough memory");

            return;
        }
    }

    /**
     * Store the result image in the script runner's image table now that the action execution is finished.
     */
    protected void doPostAlgorithmActions() {
        AlgorithmParameters.storeImageInRunner(getResultImage());
    }

    /**
     * {@inheritDoc}
     */
    protected void setGUIFromParams() {
        ParameterImage firstImageParameter = scriptParameters.getParams().getImageParameter(AlgorithmParameters.getInputImageLabel(1));
        ParameterImage secondImageParameter = scriptParameters.getParams().getImageParameter(AlgorithmParameters.getInputImageLabel(2));

        firstImage = firstImageParameter.getImage();
        userInterface = ViewUserInterface.getReference();
        parentFrame = firstImage.getParentFrame();

        // only set the second image if it isn't the same as the first one..
        if (!firstImageParameter.isSameImageAs(secondImageParameter)) {
            secondImage = secondImageParameter.getImage();
        }

        setDoLinearRescale(scriptParameters.getParams().getBoolean("do_linear_rescale"));
        setDoLogResult(scriptParameters.getParams().getBoolean("do_log_result"));
        setBin1(scriptParameters.getParams().getInt("bin_1"));
        setBin2(scriptParameters.getParams().getInt("bin_2"));
        setBin1Default(scriptParameters.getParams().getBoolean("do_use_bin_1_default"));
        setBin2Default(scriptParameters.getParams().getBoolean("do_use_bin_2_default"));

        boolean[] rgb = scriptParameters.getParams().getList(AlgorithmParameters.DO_PROCESS_RGB).getAsBooleanArray();
        setUseRed(rgb[0]);
        setUseGreen(rgb[1]);
        setUseBlue(rgb[2]);

        if (secondImage != null) {

            if (bin1Default) {
                possibleIntValues = firstImage.getMax() - firstImage.getMin() + 1;

                if ((firstImage.getType() == ModelStorageBase.BYTE) ||
                        (firstImage.getType() == ModelStorageBase.UBYTE) ||
                        (firstImage.getType() == ModelStorageBase.SHORT) ||
                        (firstImage.getType() == ModelStorageBase.USHORT) ||
                        (firstImage.getType() == ModelStorageBase.INTEGER) ||
                        (firstImage.getType() == ModelStorageBase.UINTEGER) ||
                        (firstImage.getType() == ModelStorageBase.LONG)) {
                    bin1 = (int) Math.round(possibleIntValues);
                }
            } // if (bin1Default)

            if (bin2Default) {
                possibleInt2Values = secondImage.getMax() - secondImage.getMin() + 1;

                if ((secondImage.getType() == ModelStorageBase.BYTE) ||
                        (secondImage.getType() == ModelStorageBase.UBYTE) ||
                        (secondImage.getType() == ModelStorageBase.SHORT) ||
                        (secondImage.getType() == ModelStorageBase.USHORT) ||
                        (secondImage.getType() == ModelStorageBase.INTEGER) ||
                        (secondImage.getType() == ModelStorageBase.UINTEGER) ||
                        (secondImage.getType() == ModelStorageBase.LONG)) {
                    bin2 = (int) Math.round(possibleInt2Values);
                }
            } // if (bin2Default)
        } else { // secondImage == null

            if (bin1Default) {

                if (useRed) {
                    possibleIntValues = firstImage.getMaxR() - firstImage.getMinR() + 1;
                } else {
                    possibleIntValues = firstImage.getMaxG() - firstImage.getMinG() + 1;
                }

                if ((firstImage.getType() == ModelStorageBase.ARGB) ||
                        (firstImage.getType() == ModelStorageBase.ARGB_USHORT)) {
                    bin1 = (int) Math.round(possibleIntValues);
                }
            } // if (bin1Default)

            if (bin2Default) {

                if (useBlue) {
                    possibleInt2Values = firstImage.getMaxB() - firstImage.getMinB() + 1;
                } else {
                    possibleInt2Values = firstImage.getMaxG() - firstImage.getMinG() + 1;
                }

                if ((firstImage.getType() == ModelStorageBase.ARGB) ||
                        (firstImage.getType() == ModelStorageBase.ARGB_USHORT)) {
                    bin2 = (int) Math.round(possibleInt2Values);
                }
            } // if (bin2Default)
        }
    }

    /**
     * {@inheritDoc}
     */
    protected void storeParamsFromGUI() throws ParserException {

        if (secondImage != null) {
            possibleIntValues = firstImage.getMax() - firstImage.getMin() + 1;

            if (((firstImage.getType() == ModelStorageBase.BYTE) || (firstImage.getType() == ModelStorageBase.UBYTE) ||
                     (firstImage.getType() == ModelStorageBase.SHORT) ||
                     (firstImage.getType() == ModelStorageBase.USHORT) ||
                     (firstImage.getType() == ModelStorageBase.INTEGER) ||
                     (firstImage.getType() == ModelStorageBase.UINTEGER) ||
                     (firstImage.getType() == ModelStorageBase.LONG)) &&
                    (bin1 == (int) Math.round(possibleIntValues))) {
                bin1Default = true;
            } else {
                bin1Default = false;
            }

            possibleInt2Values = secondImage.getMax() - secondImage.getMin() + 1;

            if (((secondImage.getType() == ModelStorageBase.BYTE) ||
                     (secondImage.getType() == ModelStorageBase.UBYTE) ||
                     (secondImage.getType() == ModelStorageBase.SHORT) ||
                     (secondImage.getType() == ModelStorageBase.USHORT) ||
                     (secondImage.getType() == ModelStorageBase.INTEGER) ||
                     (secondImage.getType() == ModelStorageBase.UINTEGER) ||
                     (secondImage.getType() == ModelStorageBase.LONG)) && (!doLinearRescale) &&
                    (bin2 == (int) Math.round(possibleInt2Values))) {
                bin2Default = true;
            } else {
                bin2Default = false;
            }

            scriptParameters.storeInputImage(firstImage);
            scriptParameters.storeInputImage(secondImage);
            scriptParameters.storeImageInRecorder(getResultImage());

            scriptParameters.getParams().put(ParameterFactory.newParameter("do_linear_rescale", doLinearRescale));
            scriptParameters.getParams().put(ParameterFactory.newParameter("do_log_result", doLogResult));
            scriptParameters.getParams().put(ParameterFactory.newParameter("bin_1", bin1));
            scriptParameters.getParams().put(ParameterFactory.newParameter("bin_2", bin2));
            scriptParameters.getParams().put(ParameterFactory.newParameter("do_use_bin_1_default", bin1Default));
            scriptParameters.getParams().put(ParameterFactory.newParameter("do_use_bin_2_default", bin2Default));
            scriptParameters.storeColorOptions(useRed, useGreen, useBlue);
        } else {
            // secondImage == null

            if (useRed) {
                possibleIntValues = firstImage.getMaxR() - firstImage.getMinR() + 1;
            } else {
                possibleIntValues = firstImage.getMaxG() - firstImage.getMinG() + 1;
            }

            if (useBlue) {
                possibleInt2Values = firstImage.getMaxB() - firstImage.getMinB() + 1;
            } else {
                possibleInt2Values = firstImage.getMaxG() - firstImage.getMinG() + 1;
            }

            if (((firstImage.getType() == ModelStorageBase.ARGB) ||
                     (firstImage.getType() == ModelStorageBase.ARGB_USHORT)) &&
                    (bin1 == (int) Math.round(possibleIntValues))) {
                bin1Default = true;
            } else {
                bin1Default = false;
            }

            if (((firstImage.getType() == ModelStorageBase.ARGB) ||
                     (firstImage.getType() == ModelStorageBase.ARGB_USHORT)) && (!doLinearRescale) &&
                    (bin2 == (int) Math.round(possibleInt2Values))) {
                bin2Default = true;
            } else {
                bin2Default = false;
            }

            // store the first image as both image1 and image2
            scriptParameters.storeInputImage(firstImage);
            scriptParameters.storeInputImage(firstImage);
            scriptParameters.storeImageInRecorder(getResultImage());

            scriptParameters.getParams().put(ParameterFactory.newParameter("do_linear_rescale", doLinearRescale));
            scriptParameters.getParams().put(ParameterFactory.newParameter("do_log_result", doLogResult));
            scriptParameters.getParams().put(ParameterFactory.newParameter("bin_1", bin1));
            scriptParameters.getParams().put(ParameterFactory.newParameter("bin_2", bin2));
            scriptParameters.getParams().put(ParameterFactory.newParameter("do_use_bin_1_default", bin1Default));
            scriptParameters.getParams().put(ParameterFactory.newParameter("do_use_bin_2_default", bin2Default));
            scriptParameters.storeColorOptions(useRed, useGreen, useBlue);
        }
    }

    /**
     * Builds a list of images. Returns combobox. List must be all color or all black and white.
     *
     * @param   image  DOCUMENT ME!
     *
     * @return  Newly created combo box.
     */
    private JComboBox buildComboBox(ModelImage image) {
        ModelImage nextImage;
        boolean doAdd;
        int i;

        JComboBox comboBox = new JComboBox();
        comboBox.setFont(serif12);
        comboBox.setBackground(Color.white);

        Enumeration<String> names = userInterface.getRegisteredImageNames();

        while (names.hasMoreElements()) {
            String name = names.nextElement();

            if (!name.equals(image.getImageName())) {
                nextImage = userInterface.getRegisteredImageByName(name);

                if (userInterface.getFrameContainingImage(nextImage) != null) {

                    if ((image.isColorImage() == nextImage.isColorImage()) &&
                            (image.getNDims() == nextImage.getNDims())) {
                        doAdd = true;

                        for (i = 0; i < image.getNDims(); i++) {

                            if (image.getExtents()[i] != nextImage.getExtents()[i]) {
                                doAdd = false;
                            }
                        }

                        if (doAdd) {
                            comboBox.addItem(name);
                        }
                    }
                }
            }
        }

        return comboBox;
    }

    /**
     * Initializes GUI components and displays dialog.
     */
    private void init() {
        boolean haveRed;
        boolean haveGreen;
        boolean haveBlue;
        JPanel imagePanel;

        setForeground(Color.black);
        setTitle("Histogram Two Dimensional");

        String matchName = firstImage.getImageName();

        if (firstImage.isColorImage()) {

            haveRed = false;
            haveGreen = false;
            haveBlue = false;
            minR = firstImage.getMinR();
            maxR = firstImage.getMaxR();

            if (minR != maxR) {
                haveRed = true;
            }

            minG = firstImage.getMinG();
            maxG = firstImage.getMaxG();

            if (minG != maxG) {
                haveGreen = true;
            }

            minB = firstImage.getMinB();
            maxB = firstImage.getMaxB();

            if (minB != maxB) {
                haveBlue = true;
            }

            colorsPresent = 0;

            if (haveRed) {
                colorsPresent++;
            }

            if (haveGreen) {
                colorsPresent++;
            }

            if (haveBlue) {
                colorsPresent++;
            }

            if (colorsPresent == 0) {
                MipavUtil.displayError("All channels in this color image are single valued");

                return;
            } else if (colorsPresent == 1) {

                if (haveRed) {
                    MipavUtil.displayError("Only the red channel has more than 1 bin");
                } else if (haveGreen) {
                    MipavUtil.displayError("Only the green channel has more than 1 bin");
                } else {
                    MipavUtil.displayError("Only the blue channel has more than 1 bin");
                }

                return;
            } // else if (colorsPresent == 1)
            else if (colorsPresent == 2) {

                if (haveRed && haveGreen) {
                    labelImage = new JLabel("Histogram 2D with red to green");
                    useRed = true;
                    useGreen = true;
                } else if (haveRed && haveBlue) {
                    labelImage = new JLabel("Histogram 2D with red to blue");
                    useRed = true;
                    useBlue = true;
                } else {
                    labelImage = new JLabel("Histogram 2D with green to blue");
                    useGreen = true;
                    useBlue = true;
                }

                labelImage.setForeground(Color.black);
                labelImage.setFont(serif12);
                imagePanel = new JPanel(new FlowLayout(FlowLayout.LEFT));
                imagePanel.add(labelImage);
            } // else if (colorsPresent == 2)
            else { // colorsPresent == 3
                labelImage = new JLabel("Select 2 of the 3 colors");
                labelImage.setForeground(Color.black);
                labelImage.setFont(serif12);

                GridBagConstraints gbc2 = new GridBagConstraints();
                gbc2.gridwidth = 1;
                gbc2.gridheight = 1;
                gbc2.anchor = GridBagConstraints.WEST;
                gbc2.weightx = 1;
                gbc2.insets = new Insets(3, 3, 3, 3);
                gbc2.fill = GridBagConstraints.HORIZONTAL;
                gbc2.gridx = 0;
                gbc2.gridy = 0;

                imagePanel = new JPanel(new GridBagLayout());
                imagePanel.add(labelImage, gbc2);

                gbc2.gridy = 1;
                redCheckBox = new JCheckBox("Red");
                redCheckBox.setFont(serif12);
                redCheckBox.setForeground(Color.black);
                redCheckBox.setSelected(true);
                redCheckBox.addItemListener(this);
                imagePanel.add(redCheckBox, gbc2);

                gbc2.gridy = 2;
                greenCheckBox = new JCheckBox("Green");
                greenCheckBox.setFont(serif12);
                greenCheckBox.setForeground(Color.black);
                greenCheckBox.setSelected(true);
                greenCheckBox.addItemListener(this);
                imagePanel.add(greenCheckBox, gbc2);

                gbc2.gridy = 3;
                blueCheckBox = new JCheckBox("Blue");
                blueCheckBox.setFont(serif12);
                blueCheckBox.setForeground(Color.black);
                blueCheckBox.setSelected(false);
                blueCheckBox.addItemListener(this);
                imagePanel.add(blueCheckBox, gbc2);

                useRed = true;
                useGreen = true;
            } // else colorsPresent == 3

            bin1 = 256;

            if (useRed) {
                possibleIntValues = firstImage.getMaxR() - firstImage.getMinR() + 1;
            } else {
                possibleIntValues = firstImage.getMaxG() - firstImage.getMinG() + 1;
            }

            if (((firstImage.getType() == ModelStorageBase.ARGB) ||
                     (firstImage.getType() == ModelStorageBase.ARGB_USHORT)) && (possibleIntValues < 256)) {
                bin1 = (int) Math.round(possibleIntValues);
            }

            bin2 = bin1;
        } // if (firstImage.isColorImage())
        else {
            labelImage = new JLabel("Histogram 2D with [" + matchName + "] to:");
            labelImage.setForeground(Color.black);
            labelImage.setFont(serif12);
            imageComboBox = buildComboBox(firstImage);
            imageComboBox.addItemListener(this);

            userInterface = ViewUserInterface.getReference();

            String selectedName = (String) imageComboBox.getSelectedItem();

            if (selectedName == null) {
                MipavUtil.displayError("No image found to form a 2D histogram with");

                return;
            }

            secondImage = userInterface.getRegisteredImageByName(selectedName);

            bin1 = 256;
            possibleIntValues = firstImage.getMax() - firstImage.getMin() + 1;

            if (((firstImage.getType() == ModelStorageBase.BYTE) || (firstImage.getType() == ModelStorageBase.UBYTE) ||
                     (firstImage.getType() == ModelStorageBase.SHORT) ||
                     (firstImage.getType() == ModelStorageBase.USHORT) ||
                     (firstImage.getType() == ModelStorageBase.INTEGER) ||
                     (firstImage.getType() == ModelStorageBase.UINTEGER) ||
                     (firstImage.getType() == ModelStorageBase.LONG)) && (possibleIntValues < 256)) {
                bin1 = (int) Math.round(possibleIntValues);
            }

            if (secondImage.getMax() == secondImage.getMin()) {
                bin2 = 1;
            } else {
                bin2 = bin1;
            }

            imagePanel = new JPanel(new FlowLayout(FlowLayout.LEFT));
            imagePanel.add(labelImage);
            imagePanel.add(imageComboBox);
        } // else !firstImage.isColorImage()

        GridBagConstraints gbc = new GridBagConstraints();
        gbc.gridwidth = 2;
        gbc.gridheight = 1;
        gbc.anchor = GridBagConstraints.WEST;
        gbc.weightx = 1;
        gbc.insets = new Insets(3, 3, 3, 3);
        gbc.fill = GridBagConstraints.HORIZONTAL;
        gbc.gridx = 0;
        gbc.gridy = 0;

        JPanel rescalePanel = new JPanel(new GridBagLayout());
        rescalePanel.setForeground(Color.black);
        rescalePanel.setBorder(buildTitledBorder("Options"));

        if (useBlue) {
            linearCheckbox = new JCheckBox("Linearly rescale blue");
        } else if (useGreen) {
            linearCheckbox = new JCheckBox("Linearly rescale green");
        } else {
            linearCheckbox = new JCheckBox("Linearly rescale 2nd image");
        }

        linearCheckbox.setFont(serif12);
        linearCheckbox.setForeground(Color.black);

        if (useBlue || useGreen) {
            linearCheckbox.setSelected(true);
            linearCheckbox.setEnabled(true);
        } else if (secondImage.getMax() == secondImage.getMin()) {
            linearCheckbox.setSelected(false);
            linearCheckbox.setEnabled(false);
        } else {
            linearCheckbox.setSelected(true);
            linearCheckbox.setEnabled(true);
        }

        linearCheckbox.addItemListener(this);
        rescalePanel.add(linearCheckbox, gbc);


        gbc.gridwidth = 1;
        gbc.gridx = 0;
        gbc.gridy = 1;

        if (useRed) {
            bin1Label = new JLabel("Red bin number ");
        } else if (useGreen) {
            bin1Label = new JLabel("Green bin number ");
        } else {
            bin1Label = new JLabel("Image 1 bin number ");
        }

        bin1Label.setForeground(Color.black);
        bin1Label.setFont(serif12);
        rescalePanel.add(bin1Label, gbc);

        gbc.gridx = 1;
        gbc.gridy = 1;
        bin1Text = new JTextField();
        bin1Text.setText(String.valueOf(bin1));
        bin1Text.setFont(serif12);
        rescalePanel.add(bin1Text, gbc);

        gbc.gridx = 0;
        gbc.gridy = 2;

        if (useBlue) {
            bin2Label = new JLabel("Blue bin number ");
        } else if (useGreen) {
            bin2Label = new JLabel("Green bin number ");
        } else {
            bin2Label = new JLabel("Image 2 bin number ");
        }

        bin2Label.setForeground(Color.black);
        bin2Label.setFont(serif12);
        rescalePanel.add(bin2Label, gbc);

        gbc.gridx = 1;
        gbc.gridy = 2;
        bin2Text = new JTextField();
        bin2Text.setText(String.valueOf(bin2));
        bin2Text.setFont(serif12);
        rescalePanel.add(bin2Text, gbc);

        gbc.gridx = 0;
        gbc.gridy = 3;
        resultCheckbox = new JCheckBox("Calculate log of the result image");
        resultCheckbox.setFont(serif12);
        resultCheckbox.setForeground(Color.black);
        resultCheckbox.setSelected(true);
        resultCheckbox.setEnabled(true);
        resultCheckbox.addItemListener(this);
        rescalePanel.add(resultCheckbox, gbc);

        buildOKButton();
        buildCancelButton();
        buildHelpButton();

        JPanel buttonPanel = new JPanel();
        buttonPanel.add(OKButton);
        buttonPanel.add(cancelButton);
        buttonPanel.add(helpButton);

        getContentPane().add(imagePanel, BorderLayout.NORTH);
        getContentPane().add(rescalePanel, BorderLayout.CENTER);
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

        doLinearRescale = linearCheckbox.isSelected();
        doLogResult = resultCheckbox.isSelected();

        if (firstImage.isColorImage()) {
            userInterface = ViewUserInterface.getReference();

            if (((redCheckBox.isSelected()) && (greenCheckBox.isSelected()) && (!blueCheckBox.isSelected())) ||
                    ((redCheckBox.isSelected()) && (!greenCheckBox.isSelected()) && (blueCheckBox.isSelected())) ||
                    ((!redCheckBox.isSelected()) && (greenCheckBox.isSelected()) && (blueCheckBox.isSelected()))) {
                useRed = redCheckBox.isSelected();
                useGreen = greenCheckBox.isSelected();
                useBlue = blueCheckBox.isSelected();

                if (useRed) {
                    possibleIntValues = firstImage.getMaxR() - firstImage.getMinR() + 1;
                } else {
                    possibleIntValues = firstImage.getMaxG() - firstImage.getMinG() + 1;
                }

                if (useBlue) {
                    possibleInt2Values = firstImage.getMaxB() - firstImage.getMinB() + 1;
                } else {
                    possibleInt2Values = firstImage.getMaxG() - firstImage.getMinG() + 1;
                }
            } else {
                MipavUtil.displayError("Exactly 2 color boxes must be checked");

                return false;
            }

            tmpStr = bin1Text.getText();
            bin1 = Integer.parseInt(tmpStr);

            if (bin1 < 1) {

                if (useRed) {
                    MipavUtil.displayError("Red must have at least 1 bin");
                } else {
                    MipavUtil.displayError("Green must have at least 1 bin");
                }

                bin1Text.requestFocus();
                bin1Text.selectAll();

                return false;
            } else if ((bin1 > Math.round(possibleIntValues)) &&
                           ((firstImage.getType() == ModelStorageBase.ARGB) ||
                                (firstImage.getType() == ModelStorageBase.ARGB_USHORT))) {

                if (useRed) {
                    MipavUtil.displayError("Red must not have more than " + Math.round(possibleIntValues) + " bins");
                } else {
                    MipavUtil.displayError("Green must not have more than " + Math.round(possibleIntValues) + " bins");
                }

                bin1Text.requestFocus();
                bin1Text.selectAll();

                return false;
            }

            tmpStr = bin2Text.getText();
            bin2 = Integer.parseInt(tmpStr);

            if (bin2 < 1) {

                if (useBlue) {
                    MipavUtil.displayError("Blue must have at least 1 bin");
                } else {
                    MipavUtil.displayError("Green must have at least 1 bin");
                }

                bin2Text.requestFocus();
                bin2Text.selectAll();

                return false;
            } else if ((bin2 > Math.round(possibleInt2Values)) && (!doLinearRescale) &&
                           ((firstImage.getType() == ModelStorageBase.ARGB) ||
                                (firstImage.getType() == ModelStorageBase.ARGB_USHORT))) {

                if (useBlue) {
                    MipavUtil.displayError("Blue must not have more than " + Math.round(possibleInt2Values) + " bins");
                } else {
                    MipavUtil.displayError("Green must not have more than " + Math.round(possibleInt2Values) + " bins");
                }

                bin2Text.requestFocus();
                bin2Text.selectAll();

                return false;
            }
        } // if (firstImage.isColorImage())
        else { // not color image
            userInterface = ViewUserInterface.getReference();

            String selectedName = (String) imageComboBox.getSelectedItem();
            secondImage = userInterface.getRegisteredImageByName(selectedName);

            if (secondImage == null) {
                return false;
            }

            tmpStr = bin1Text.getText();
            bin1 = Integer.parseInt(tmpStr);

            if (bin1 < 1) {
                MipavUtil.displayError("Image 1 must have at least 1 bin");
                bin1Text.requestFocus();
                bin1Text.selectAll();

                return false;
            } else if ((bin1 > Math.round(possibleIntValues)) &&
                           ((firstImage.getType() == ModelStorageBase.BYTE) ||
                                (firstImage.getType() == ModelStorageBase.UBYTE) ||
                                (firstImage.getType() == ModelStorageBase.SHORT) ||
                                (firstImage.getType() == ModelStorageBase.USHORT) ||
                                (firstImage.getType() == ModelStorageBase.INTEGER) ||
                                (firstImage.getType() == ModelStorageBase.UINTEGER) ||
                                (firstImage.getType() == ModelStorageBase.LONG))) {

                MipavUtil.displayError("Image 1 must not have more than " + Math.round(possibleIntValues) + " bins");
                bin1Text.requestFocus();
                bin1Text.selectAll();

                return false;
            }

            possibleInt2Values = secondImage.getMax() - secondImage.getMin() + 1;
            tmpStr = bin2Text.getText();
            bin2 = Integer.parseInt(tmpStr);

            if (bin2 < 1) {
                MipavUtil.displayError("Image 2 must have at least 1 bin");
                bin2Text.requestFocus();
                bin2Text.selectAll();

                return false;
            } else if ((bin2 > Math.round(possibleInt2Values)) && (!doLinearRescale) &&
                           ((secondImage.getType() == ModelStorageBase.BYTE) ||
                                (secondImage.getType() == ModelStorageBase.UBYTE) ||
                                (secondImage.getType() == ModelStorageBase.SHORT) ||
                                (secondImage.getType() == ModelStorageBase.USHORT) ||
                                (secondImage.getType() == ModelStorageBase.INTEGER) ||
                                (secondImage.getType() == ModelStorageBase.UINTEGER) ||
                                (secondImage.getType() == ModelStorageBase.LONG))) {

                MipavUtil.displayError("Image 2 must not have more than " + Math.round(possibleInt2Values) + " bins");
                bin2Text.requestFocus();
                bin2Text.selectAll();

                return false;
            }
        } // not color image

        return true;
    }

}
