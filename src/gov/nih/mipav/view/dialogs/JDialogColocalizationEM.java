package gov.nih.mipav.view.dialogs;


import gov.nih.mipav.model.algorithms.*;
import gov.nih.mipav.model.scripting.*;
import gov.nih.mipav.model.scripting.parameters.*;
import gov.nih.mipav.model.structures.*;

import gov.nih.mipav.view.*;

import java.awt.*;
import java.awt.event.*;

import java.io.*;

import java.util.*;

import javax.swing.*;


/**
 * Dialog to get user input Identify colocalized pixels Algorithms are executed in their own thread.
 */
public class JDialogColocalizationEM extends JDialogScriptableBase implements AlgorithmInterface {

    //~ Static fields/initializers -------------------------------------------------------------------------------------

    /** Use serialVersionUID for interoperability. */
    private static final long serialVersionUID = -2241466976779150747L;

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** DOCUMENT ME! */
    private JRadioButton andButton;

    /** DOCUMENT ME! */
    private int bin1;

    /** DOCUMENT ME! */
    private JLabel bin1Label;

    /** DOCUMENT ME! */
    private JTextField bin1Text;

    /** DOCUMENT ME! */
    private int bin2;

    /** DOCUMENT ME! */
    private JLabel bin2Label;

    /** DOCUMENT ME! */
    private JTextField bin2Text;

    /** DOCUMENT ME! */
    private JCheckBox blueCheckBox;

    /** DOCUMENT ME! */
    private int bottomPad = 60;

    /** DOCUMENT ME! */
    private float[] buffer = null;

    /** DOCUMENT ME! */
    private AlgorithmColocalizationEM colocalizationAlgo = null;

    /** DOCUMENT ME! */
    private int colorsPresent = 0;

    /** DOCUMENT ME! */
    private JComboBox comboBoxCostFunct;

    /** DOCUMENT ME! */
    private int cost;

    /** DOCUMENT ME! */
    private boolean doOr = true;

    /** DOCUMENT ME! */
    private boolean entireImage = true;

    /** DOCUMENT ME! */
    private ModelImage firstImage;

    /** DOCUMENT ME! */
    private JLabel gaussianLabel;

    /** DOCUMENT ME! */
    private int gaussians = 4;

    /** DOCUMENT ME! */
    private JTextField gaussianText;

    /** DOCUMENT ME! */
    private JCheckBox greenCheckBox;

    /** DOCUMENT ME! */
    private JComboBox imageComboBox;

    /** DOCUMENT ME! */
    private int imageLength;

    /** DOCUMENT ME! */
    private JLabel iterationLabel;

    /** DOCUMENT ME! */
    private int iterations = 20;

    /** DOCUMENT ME! */
    private JTextField iterationText;

    /** DOCUMENT ME! */
    private JLabel labelCost;

    /** DOCUMENT ME! */
    private JLabel labelImage;

    /** DOCUMENT ME! */
    private int leftPad = 80;

    /** DOCUMENT ME! */
    private BitSet mask = null;

    /** DOCUMENT ME! */
    private double maxR, maxG, maxB;

    /** DOCUMENT ME! */
    private double maxRV, maxGV, maxBV;

    /** DOCUMENT ME! */
    private double minR, minG, minB;

    /** DOCUMENT ME! */
    private double minRV, minGV, minBV;

    /** DOCUMENT ME! */
    private double minV, maxV;

    /** DOCUMENT ME! */
    private int nBoundingVOIs;

    /** DOCUMENT ME! */
    private JRadioButton orButton;

    /** DOCUMENT ME! */
    private double possibleInt2Values;

    /** DOCUMENT ME! */
    private double possibleIntValues;

    /** DOCUMENT ME! */
    private JCheckBox redCheckBox;

    /** DOCUMENT ME! */
    private JCheckBox regCheckBox;

    /** DOCUMENT ME! */
    private boolean register;

    /** DOCUMENT ME! */
    private ModelImage resultImage = null; // result image

    /** DOCUMENT ME! */
    private int rightPad = 40;

    /** DOCUMENT ME! */
    private ModelImage secondImage = null;

    /** DOCUMENT ME! */
    private double secondMinV, secondMaxV;

    /** DOCUMENT ME! */
    private String secondName = null;

    /** Class segmentation shown. */
    private ModelImage segImage = null;

    /** DOCUMENT ME! */
    private float threshold1 = 1.0f;

    /** DOCUMENT ME! */
    private JLabel threshold1Label;

    /** DOCUMENT ME! */
    private JTextField threshold1Text;

    /** DOCUMENT ME! */
    private float threshold2 = 1.0f;

    /** DOCUMENT ME! */
    private JLabel threshold2Label;

    /** DOCUMENT ME! */
    private JTextField threshold2Text;

    /** DOCUMENT ME! */
    private int topPad = 40;

    /** DOCUMENT ME! */
    private ViewUserInterface UI;

    /** DOCUMENT ME! */
    private boolean useBlue = false;

    /** DOCUMENT ME! */
    private boolean useGreen = false;

    /** DOCUMENT ME! */
    private boolean useRed = false;

    /** DOCUMENT ME! */
    private JRadioButton VOIRegions;

    /** DOCUMENT ME! */
    private JRadioButton wholeImage;

    /** DOCUMENT ME! */
    private int xDim;

    /** DOCUMENT ME! */
    private int yDim;

    /** DOCUMENT ME! */
    private int zDim;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Empty constructor needed for dynamic instantiation (used during scripting).
     */
    public JDialogColocalizationEM() { }

    /**
     * Creates a new JDialogColocalizationEM object.
     *
     * @param  firstImage  DOCUMENT ME!
     */
    public JDialogColocalizationEM(ModelImage firstImage) {
        super();
        this.UI = ViewUserInterface.getReference();
        this.firstImage = firstImage;
        parentFrame = firstImage.getParentFrame();
    }


    /**
     * Creates new dialog.
     *
     * @param  theParentFrame  Parent frame
     * @param  im              Source image
     */
    public JDialogColocalizationEM(Frame theParentFrame, ModelImage im) {
        super(theParentFrame, false);
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
        String command = event.getActionCommand();

        if (command.equals("OK")) {

            if (setVariables()) {
                callAlgorithm();
            }
        } else if (command.equals("Cancel")) {
            dispose();
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
        insertScriptLine();
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
     * Accessor that returns the image.
     *
     * @return  The segmented image.
     */
    public ModelImage getSegImage() {
        return segImage;
    }


    // ************************* Item Events ****************************
    // *******************************************************************

    /**
     * itemStateChanged.
     *
     * @param  event  DOCUMENT ME!
     */
    public void itemStateChanged(ItemEvent event) {
        int i;
        Object source = event.getSource();

        if (source == regCheckBox) {

            if (regCheckBox.isSelected()) {
                labelCost.setEnabled(true);
                comboBoxCostFunct.setEnabled(true);
            } else {
                labelCost.setEnabled(false);
                comboBoxCostFunct.setEnabled(false);
            }
        } else if (source == imageComboBox) {
            UI = ViewUserInterface.getReference();
            secondName = (String) imageComboBox.getSelectedItem();
            secondImage = UI.getRegisteredImageByName(secondName);

            if (entireImage) {
                possibleInt2Values = secondImage.getMax() - secondImage.getMin() + 1;
            } else {

                try {
                    secondImage.exportData(0, imageLength, buffer);
                } catch (IOException e) {
                    MipavUtil.displayError("IOException on secondImage.exportData");

                    return;
                }

                secondMinV = Double.MAX_VALUE;
                secondMaxV = -Double.MAX_VALUE;

                for (i = 0; i < imageLength; i++) {

                    if (mask.get(i)) {

                        if (buffer[i] < secondMinV) {
                            secondMinV = buffer[i];
                        }

                        if (buffer[i] > secondMaxV) {
                            secondMaxV = buffer[i];
                        }
                    }
                }

                possibleInt2Values = secondMaxV - secondMinV + 1;
            }

            bin2 = 256;

            if ((bin2 > Math.round(possibleInt2Values)) &&
                    ((secondImage.getType() == ModelStorageBase.BYTE) ||
                         (secondImage.getType() == ModelStorageBase.UBYTE) ||
                         (secondImage.getType() == ModelStorageBase.SHORT) ||
                         (secondImage.getType() == ModelStorageBase.USHORT) ||
                         (secondImage.getType() == ModelStorageBase.INTEGER) ||
                         (secondImage.getType() == ModelStorageBase.UINTEGER) ||
                         (secondImage.getType() == ModelStorageBase.LONG))) {
                bin2 = (int) Math.round(possibleInt2Values);
            }

            threshold2Label.setText(secondName + " data threshold ");
            bin2Label.setText(secondName + " bin number ");
            bin2Text.setText(String.valueOf(bin2));
        } // if ( source == imageComboBox)
        else if ((source == wholeImage) || (source == VOIRegions)) {
            entireImage = wholeImage.isSelected();

            if (firstImage.isColorImage()) {
                bin1 = 256;

                if (useRed) {

                    if (entireImage) {
                        possibleIntValues = firstImage.getMaxR() - firstImage.getMinR() + 1;
                    } else {
                        possibleIntValues = maxRV - minRV + 1;
                    }
                } else {

                    if (entireImage) {
                        possibleIntValues = firstImage.getMaxG() - firstImage.getMinG() + 1;
                    } else {
                        possibleIntValues = maxGV - minGV + 1;
                    }
                }

                if (((firstImage.getType() == ModelStorageBase.ARGB) ||
                         (firstImage.getType() == ModelStorageBase.ARGB_USHORT)) && (possibleIntValues < 256)) {
                    bin1 = (int) Math.round(possibleIntValues);
                }

                bin2 = 256;

                if (useBlue) {

                    if (entireImage) {
                        possibleInt2Values = firstImage.getMaxB() - firstImage.getMinB() + 1;
                    } else {
                        possibleInt2Values = maxBV - minBV + 1;
                    }
                } else {

                    if (entireImage) {
                        possibleInt2Values = firstImage.getMaxG() - firstImage.getMinG() + 1;
                    } else {
                        possibleInt2Values = maxGV - minGV + 1;
                    }
                }

                if (((firstImage.getType() == ModelStorageBase.ARGB) ||
                         (firstImage.getType() == ModelStorageBase.ARGB_USHORT)) && (possibleInt2Values < 256)) {
                    bin2 = (int) Math.round(possibleInt2Values);
                }

                bin1Text.setText(String.valueOf(bin1));
                bin2Text.setText(String.valueOf(bin2));
            } // if (firstImage.isColorImage())
            else { // black and white

                if (entireImage) {
                    possibleIntValues = firstImage.getMax() - firstImage.getMin() + 1;
                    possibleInt2Values = secondImage.getMax() - secondImage.getMin() + 1;
                } else {
                    possibleIntValues = maxV - minV + 1;
                    possibleInt2Values = secondMaxV - secondMinV + 1;
                }

                bin1 = 256;

                if (((firstImage.getType() == ModelStorageBase.BYTE) ||
                         (firstImage.getType() == ModelStorageBase.UBYTE) ||
                         (firstImage.getType() == ModelStorageBase.SHORT) ||
                         (firstImage.getType() == ModelStorageBase.USHORT) ||
                         (firstImage.getType() == ModelStorageBase.INTEGER) ||
                         (firstImage.getType() == ModelStorageBase.UINTEGER) ||
                         (firstImage.getType() == ModelStorageBase.LONG)) && (possibleIntValues < 256)) {
                    bin1 = (int) Math.round(possibleIntValues);
                }

                bin1Text.setText(String.valueOf(bin1));

                bin2 = 256;

                if ((bin2 > Math.round(possibleInt2Values)) &&
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
            } // else black and white
        } // else if ((source == wholeImage) || (source == VOIRegions))
        else if ((colorsPresent == 3) &&
                     ((source == redCheckBox) || (source == greenCheckBox) || (source == blueCheckBox))) {

            // Only process if 2 checkBoxes are selected and 1 is not selected
            if (((redCheckBox.isSelected()) && (greenCheckBox.isSelected()) && (!blueCheckBox.isSelected())) ||
                    ((redCheckBox.isSelected()) && (!greenCheckBox.isSelected()) && (blueCheckBox.isSelected())) ||
                    ((!redCheckBox.isSelected()) && (greenCheckBox.isSelected()) && (blueCheckBox.isSelected()))) {
                useRed = redCheckBox.isSelected();
                useGreen = greenCheckBox.isSelected();
                useBlue = blueCheckBox.isSelected();
                bin1 = 256;

                if (useRed) {

                    if (entireImage) {
                        possibleIntValues = firstImage.getMaxR() - firstImage.getMinR() + 1;
                    } else {
                        possibleIntValues = maxRV - minRV + 1;
                    }
                } else {

                    if (entireImage) {
                        possibleIntValues = firstImage.getMaxG() - firstImage.getMinG() + 1;
                    } else {
                        possibleIntValues = maxGV - minGV + 1;
                    }
                }

                if (((firstImage.getType() == ModelStorageBase.ARGB) ||
                         (firstImage.getType() == ModelStorageBase.ARGB_USHORT)) && (possibleIntValues < 256)) {
                    bin1 = (int) Math.round(possibleIntValues);
                }

                bin2 = 256;

                if (useBlue) {

                    if (entireImage) {
                        possibleInt2Values = firstImage.getMaxB() - firstImage.getMinB() + 1;
                    } else {
                        possibleInt2Values = maxBV - minBV + 1;
                    }
                } else {

                    if (entireImage) {
                        possibleInt2Values = firstImage.getMaxG() - firstImage.getMinG() + 1;
                    } else {
                        possibleInt2Values = maxGV - minGV + 1;
                    }
                }

                if (((firstImage.getType() == ModelStorageBase.ARGB) ||
                         (firstImage.getType() == ModelStorageBase.ARGB_USHORT)) && (possibleInt2Values < 256)) {
                    bin2 = (int) Math.round(possibleInt2Values);
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
                } else {
                    bin2Label.setText("Green bin number ");
                }
            }
        }
    }


    /**
     * DOCUMENT ME!
     *
     * @param  bin1  DOCUMENT ME!
     */
    public void setBin1(int bin1) {
        this.bin1 = bin1;
    }

    /**
     * DOCUMENT ME!
     *
     * @param  bin2  DOCUMENT ME!
     */
    public void setBin2(int bin2) {
        this.bin2 = bin2;
    }

    /**
     * DOCUMENT ME!
     *
     * @param  bottomPad  DOCUMENT ME!
     */
    public void setBottomPad(int bottomPad) {
        this.bottomPad = bottomPad;
    }

    /**
     * DOCUMENT ME!
     *
     * @param  cost  DOCUMENT ME!
     */
    public void setCost(int cost) {
        this.cost = cost;
    }

    /**
     * DOCUMENT ME!
     *
     * @param  doOr  DOCUMENT ME!
     */
    public void setDoOr(boolean doOr) {
        this.doOr = doOr;
    }

    /**
     * DOCUMENT ME!
     *
     * @param  entireImage  DOCUMENT ME!
     */
    public void setEntireImage(boolean entireImage) {
        this.entireImage = entireImage;
    }

    /**
     * DOCUMENT ME!
     *
     * @param  gaussians  DOCUMENT ME!
     */
    public void setGaussians(int gaussians) {
        this.gaussians = gaussians;
    }

    /**
     * DOCUMENT ME!
     *
     * @param  iterations  DOCUMENT ME!
     */
    public void setIterations(int iterations) {
        this.iterations = iterations;
    }

    /**
     * DOCUMENT ME!
     *
     * @param  leftPad  DOCUMENT ME!
     */
    public void setLeftPad(int leftPad) {
        this.leftPad = leftPad;
    }

    /**
     * DOCUMENT ME!
     *
     * @param  register  DOCUMENT ME!
     */
    public void setRegister(boolean register) {
        this.register = register;
    }

    /**
     * DOCUMENT ME!
     *
     * @param  rightPad  DOCUMENT ME!
     */
    public void setRightPad(int rightPad) {
        this.rightPad = rightPad;
    }

    /**
     * DOCUMENT ME!
     *
     * @param  secondImage  DOCUMENT ME!
     */
    public void setSecondImage(ModelImage secondImage) {
        this.secondImage = secondImage;
    }

    /**
     * DOCUMENT ME!
     *
     * @param  threshold1  DOCUMENT ME!
     */
    public void setThreshold1(float threshold1) {
        this.threshold1 = threshold1;
    }

    /**
     * DOCUMENT ME!
     *
     * @param  threshold2  DOCUMENT ME!
     */
    public void setThreshold2(float threshold2) {
        this.threshold2 = threshold2;
    }

    /**
     * DOCUMENT ME!
     *
     * @param  topPad  DOCUMENT ME!
     */
    public void setTopPad(int topPad) {
        this.topPad = topPad;
    }

    /**
     * DOCUMENT ME!
     *
     * @param  useBlue  DOCUMENT ME!
     */
    public void setUseBlue(boolean useBlue) {
        this.useBlue = useBlue;
    }

    /**
     * DOCUMENT ME!
     *
     * @param  useGreen  DOCUMENT ME!
     */
    public void setUseGreen(boolean useGreen) {
        this.useGreen = useGreen;
    }

    /**
     * DOCUMENT ME!
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
        String segName = makeImageName(firstImage.getImageName(), "_seg");

        try {
            int[] extents = new int[2];

            // Allow padding space at left and bottom
            extents[0] = bin1 + leftPad + rightPad;
            extents[1] = bin2 + bottomPad + topPad;

            // Allow log of 1 + counts to be displayed
            resultImage = new ModelImage(ModelStorageBase.DOUBLE, extents, name);
            segImage = new ModelImage(ModelStorageBase.UBYTE, firstImage.getExtents(), segName);


            // Make algorithm
            if (firstImage.isColorImage()) {
                colocalizationAlgo = new AlgorithmColocalizationEM(resultImage, segImage, firstImage, bin1, bin2,
                                                                   threshold1, threshold2, doOr, leftPad, rightPad,
                                                                   bottomPad, topPad, useRed, useGreen, useBlue,
                                                                   entireImage, register, cost, gaussians, iterations);
            } else {
                colocalizationAlgo = new AlgorithmColocalizationEM(resultImage, segImage, firstImage, secondImage, bin1,
                                                                   bin2, threshold1, threshold2, doOr, leftPad,
                                                                   rightPad, bottomPad, topPad, entireImage, register,
                                                                   cost, gaussians, iterations);
            }

            // This is very important. Adding this object as a listener allows the algorithm to
            // notify this object when it has completed of failed. See algorithm performed event.
            // This is made possible by implementing AlgorithmedPerformed interface
            colocalizationAlgo.addListener(this);

            createProgressBar(firstImage.getImageName(), colocalizationAlgo);

            // Hide dialog
            setVisible(false);

            if (isRunInSeparateThread()) {

                // Start the thread as a low priority because we wish to still have user interface work fast.
                if (colocalizationAlgo.startMethod(Thread.MIN_PRIORITY) == false) {
                    MipavUtil.displayError("A thread is already running on this object");
                }
            } else {

                colocalizationAlgo.run();
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
     * Used to perform actions after the execution of the algorithm is completed (e.g., put the result image in the
     * image table). Defaults to no action, override to actually have it do something.
     */
    protected void doPostAlgorithmActions() {
        AlgorithmParameters.storeImageInRunner(getResultImage());
    }

    /**
     * Set the dialog GUI using the script parameters while running this algorithm as part of a script.
     */
    protected void setGUIFromParams() {
        bin1 = scriptParameters.getParams().getInt("bin1");
        bin2 = scriptParameters.getParams().getInt("bin2");
        threshold1 = scriptParameters.getParams().getFloat("threshold1");
        threshold2 = scriptParameters.getParams().getFloat("threshold2");
        doOr = scriptParameters.getParams().getBoolean("doOr");
        leftPad = scriptParameters.getParams().getInt("leftPad");
        rightPad = scriptParameters.getParams().getInt("rightPad");
        bottomPad = scriptParameters.getParams().getInt("bottomPad");
        topPad = scriptParameters.getParams().getInt("topPad");
        useRed = scriptParameters.getParams().getBoolean("useRed");
        useGreen = scriptParameters.getParams().getBoolean("useGreen");
        useBlue = scriptParameters.getParams().getBoolean("useBlue");
        entireImage = scriptParameters.getParams().getBoolean("entireImage");
        register = scriptParameters.getParams().getBoolean("register");
        cost = scriptParameters.getParams().getInt("cost");
        gaussians = scriptParameters.getParams().getInt("gaussians");
        iterations = scriptParameters.getParams().getInt("iterations");
    }


    /**
     * Record the parameters just used to run this algorithm in a script.
     *
     * @throws  ParserException  If there is a problem creating/recording the new parameters.
     */
    protected void storeParamsFromGUI() throws ParserException {
        scriptParameters.storeInputImage(firstImage);

        scriptParameters.getParams().put(ParameterFactory.newParameter("resultImage", resultImage));
        scriptParameters.getParams().put(ParameterFactory.newParameter("segImage", segImage));
        scriptParameters.getParams().put(ParameterFactory.newParameter("firstImage", firstImage));
        scriptParameters.getParams().put(ParameterFactory.newParameter("bin1", bin1));
        scriptParameters.getParams().put(ParameterFactory.newParameter("bin2", bin2));
        scriptParameters.getParams().put(ParameterFactory.newParameter("threshold1", threshold1));
        scriptParameters.getParams().put(ParameterFactory.newParameter("threshold2", threshold2));
        scriptParameters.getParams().put(ParameterFactory.newParameter("doOr", doOr));
        scriptParameters.getParams().put(ParameterFactory.newParameter("leftPad", leftPad));
        scriptParameters.getParams().put(ParameterFactory.newParameter("rightPad", rightPad));
        scriptParameters.getParams().put(ParameterFactory.newParameter("bottomPad", bottomPad));
        scriptParameters.getParams().put(ParameterFactory.newParameter("topPad", topPad));
        scriptParameters.getParams().put(ParameterFactory.newParameter("useRed", useRed));
        scriptParameters.getParams().put(ParameterFactory.newParameter("useGreen", useGreen));
        scriptParameters.getParams().put(ParameterFactory.newParameter("useBlue", useBlue));
        scriptParameters.getParams().put(ParameterFactory.newParameter("entireImage", entireImage));
        scriptParameters.getParams().put(ParameterFactory.newParameter("register", register));
        scriptParameters.getParams().put(ParameterFactory.newParameter("cost", cost));
        scriptParameters.getParams().put(ParameterFactory.newParameter("gaussians", gaussians));
        scriptParameters.getParams().put(ParameterFactory.newParameter("iterations", iterations));
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
        boolean doAdd;
        int i;

        JComboBox comboBox = new JComboBox();
        comboBox.setFont(serif12);
        comboBox.setBackground(Color.white);

        UI = ViewUserInterface.getReference();

        Enumeration<String> names = UI.getRegisteredImageNames();

        while (names.hasMoreElements()) {
            String name = (String) names.nextElement();

            if (!name.equals(image.getImageName())) {
                nextImage = UI.getRegisteredImageByName(name);

                if (UI.getFrameContainingImage(nextImage) != null) {

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
        int i;
        ViewVOIVector VOIs;
        int nVOIs;

        setForeground(Color.black);
        setTitle("Expectation Maximization Colocalization");

        String firstName = firstImage.getImageName();

        VOIs = firstImage.getVOIs();
        nVOIs = VOIs.size();
        nBoundingVOIs = 0;

        for (i = 0; i < nVOIs; i++) {

            if ((VOIs.VOIAt(i).getCurveType() == VOI.CONTOUR) || (VOIs.VOIAt(i).getCurveType() == VOI.POLYLINE)) {
                nBoundingVOIs++;
            }
        }

        if (nBoundingVOIs > 1) {
            MipavUtil.displayError("Only 1 contour VOI is allowed");

            return;
        }

        xDim = firstImage.getExtents()[0];
        yDim = firstImage.getExtents()[1];
        imageLength = xDim * yDim;

        if (firstImage.getNDims() >= 3) {
            zDim = firstImage.getExtents()[2];
            imageLength = imageLength * zDim;
        }

        if (nBoundingVOIs == 1) {
            mask = firstImage.generateVOIMask();

            if (firstImage.isColorImage()) {
                buffer = new float[4 * imageLength];

                try {
                    firstImage.exportData(0, 4 * imageLength, buffer);
                } catch (IOException e) {
                    MipavUtil.displayError("IOException on firstImage.exportData");

                    return;
                }

                minRV = Double.MAX_VALUE;
                minGV = Double.MAX_VALUE;
                minBV = Double.MAX_VALUE;
                maxRV = -Double.MAX_VALUE;
                maxGV = -Double.MAX_VALUE;
                maxBV = -Double.MAX_VALUE;

                for (i = 0; i < imageLength; i++) {

                    if (mask.get(i)) {

                        if (buffer[(4 * i) + 1] < minRV) {
                            minRV = buffer[(4 * i) + 1];
                        }

                        if (buffer[(4 * i) + 2] < minGV) {
                            minGV = buffer[(4 * i) + 2];
                        }

                        if (buffer[(4 * i) + 3] < minBV) {
                            minBV = buffer[(4 * i) + 3];
                        }

                        if (buffer[(4 * i) + 1] > maxRV) {
                            maxRV = buffer[(4 * i) + 1];
                        }

                        if (buffer[(4 * i) + 2] > maxGV) {
                            maxGV = buffer[(4 * i) + 2];
                        }

                        if (buffer[(4 * i) + 3] > maxBV) {
                            maxBV = buffer[(4 * i) + 3];
                        }
                    } // if (mask.get(i))
                } // for (i = 0; i < imageLength; i++)
            } // if (firstImage.isColorImage())
            else { // firstImage is black and white
                buffer = new float[imageLength];

                try {
                    firstImage.exportData(0, imageLength, buffer);
                } catch (IOException e) {
                    MipavUtil.displayError("IOException on firstImage.exportData");

                    return;
                }

                minV = Double.MAX_VALUE;
                maxV = -Double.MAX_VALUE;

                for (i = 0; i < imageLength; i++) {

                    if (mask.get(i)) {

                        if (buffer[i] < minV) {
                            minV = buffer[i];
                        }

                        if (buffer[i] > maxV) {
                            maxV = buffer[i];
                        }
                    }
                }
            } // else firstImage is black and white
        } // if (nBoundingVOIs == 1)

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
                    labelImage = new JLabel("Colocalization with red to green");
                    useRed = true;
                    useGreen = true;
                } else if (haveRed && haveBlue) {
                    labelImage = new JLabel("Colocalization with red to blue");
                    useRed = true;
                    useBlue = true;
                } else {
                    labelImage = new JLabel("Colocalization with green to blue");
                    useGreen = true;
                    useBlue = true;
                }

                labelImage.setForeground(Color.black);
                labelImage.setFont(serif12);
                imagePanel = new JPanel(new FlowLayout(FlowLayout.LEFT));
                imagePanel.setBorder(buildTitledBorder("Channel selection"));
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
                imagePanel.setBorder(buildTitledBorder("Channel selection"));
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
            bin2 = 256;

            if (useRed) {
                possibleIntValues = firstImage.getMaxR() - firstImage.getMinR() + 1;
            } else {
                possibleIntValues = firstImage.getMaxG() - firstImage.getMinG() + 1;
            }

            if (((firstImage.getType() == ModelStorageBase.ARGB) ||
                     (firstImage.getType() == ModelStorageBase.ARGB_USHORT)) && (possibleIntValues < 256)) {
                bin1 = (int) Math.round(possibleIntValues);
            }

            if (useBlue) {
                possibleInt2Values = firstImage.getMaxB() - firstImage.getMinB() + 1;
            } else {
                possibleInt2Values = firstImage.getMaxG() - firstImage.getMinG() + 1;
            }

            if (((firstImage.getType() == ModelStorageBase.ARGB) ||
                     (firstImage.getType() == ModelStorageBase.ARGB_USHORT)) && (possibleInt2Values < 256)) {
                bin2 = (int) Math.round(possibleInt2Values);
            }
        } // if (firstImage.isColorImage())
        else { // !(firstImage.isColorImage())
            labelImage = new JLabel("Colocalization with [" + firstName + "] and ");
            labelImage.setForeground(Color.black);
            labelImage.setFont(serif12);
            imageComboBox = buildComboBox(firstImage);
            imageComboBox.addItemListener(this);

            UI = ViewUserInterface.getReference();
            secondName = (String) imageComboBox.getSelectedItem();

            if (secondName == null) {
                MipavUtil.displayError("No image found to colocalize with");

                return;
            }

            secondImage = UI.getRegisteredImageByName(secondName);

            if (nBoundingVOIs == 1) {

                try {
                    secondImage.exportData(0, imageLength, buffer);
                } catch (IOException e) {
                    MipavUtil.displayError("IOException on secondImage.exportData");

                    return;
                }

                secondMinV = Double.MAX_VALUE;
                secondMaxV = -Double.MAX_VALUE;

                for (i = 0; i < imageLength; i++) {

                    if (mask.get(i)) {

                        if (buffer[i] < secondMinV) {
                            secondMinV = buffer[i];
                        }

                        if (buffer[i] > secondMaxV) {
                            secondMaxV = buffer[i];
                        }
                    }
                }
            } // if (nBoundingVOIs == 1)

            bin1 = 256;
            bin2 = 256;
            possibleIntValues = firstImage.getMax() - firstImage.getMin() + 1;

            if (((firstImage.getType() == ModelStorageBase.BYTE) || (firstImage.getType() == ModelStorageBase.UBYTE) ||
                     (firstImage.getType() == ModelStorageBase.SHORT) ||
                     (firstImage.getType() == ModelStorageBase.USHORT) ||
                     (firstImage.getType() == ModelStorageBase.INTEGER) ||
                     (firstImage.getType() == ModelStorageBase.UINTEGER) ||
                     (firstImage.getType() == ModelStorageBase.LONG)) && (possibleIntValues < 256)) {
                bin1 = (int) Math.round(possibleIntValues);
            }

            possibleInt2Values = secondImage.getMax() - secondImage.getMin() + 1;

            if (((secondImage.getType() == ModelStorageBase.BYTE) ||
                     (secondImage.getType() == ModelStorageBase.UBYTE) ||
                     (secondImage.getType() == ModelStorageBase.SHORT) ||
                     (secondImage.getType() == ModelStorageBase.USHORT) ||
                     (secondImage.getType() == ModelStorageBase.INTEGER) ||
                     (secondImage.getType() == ModelStorageBase.UINTEGER) ||
                     (secondImage.getType() == ModelStorageBase.LONG)) && (possibleInt2Values < 256)) {
                bin2 = (int) Math.round(possibleInt2Values);
            }

            imagePanel = new JPanel(new FlowLayout(FlowLayout.LEFT));
            imagePanel.setBorder(buildTitledBorder("Channel selection"));
            imagePanel.add(labelImage);
            imagePanel.add(imageComboBox);
        } // else !firstImage.isColorImage()

        GridBagConstraints gbc = new GridBagConstraints();
        gbc.gridwidth = 1;
        gbc.gridheight = 1;
        gbc.anchor = GridBagConstraints.WEST;
        gbc.weightx = 1;
        gbc.insets = new Insets(3, 3, 3, 3);
        gbc.fill = GridBagConstraints.HORIZONTAL;
        gbc.gridx = 0;
        gbc.gridy = 0;

        JPanel registrationPanel = new JPanel(new GridBagLayout());
        registrationPanel.setForeground(Color.black);
        registrationPanel.setBorder(buildTitledBorder("Registration"));

        regCheckBox = new JCheckBox("Registration before Colocalization");
        regCheckBox.setFont(serif12);
        regCheckBox.setForeground(Color.black);
        regCheckBox.setSelected(false);
        regCheckBox.addItemListener(this);
        registrationPanel.add(regCheckBox, gbc);

        labelCost = new JLabel("Cost function:");
        labelCost.setForeground(Color.black);
        labelCost.setFont(serif12);
        labelCost.setAlignmentX(Component.LEFT_ALIGNMENT);
        labelCost.setEnabled(false);
        gbc.gridx = 0;
        gbc.gridy = 1;
        registrationPanel.add(labelCost, gbc);

        comboBoxCostFunct = new JComboBox();
        comboBoxCostFunct.setFont(MipavUtil.font12);
        comboBoxCostFunct.setBackground(Color.white);
        comboBoxCostFunct.setToolTipText("Cost function");
        comboBoxCostFunct.addItem("Correlation ratio");
        comboBoxCostFunct.addItem("Least squares");
        comboBoxCostFunct.addItem("Normalized cross correlation");
        comboBoxCostFunct.addItem("Normalized mutual information");
        comboBoxCostFunct.setSelectedIndex(0);
        comboBoxCostFunct.setEnabled(false);
        gbc.gridx = 1;
        gbc.gridy = 1;
        registrationPanel.add(comboBoxCostFunct, gbc);

        JPanel mainPanel = new JPanel(new GridBagLayout());
        gbc.gridx = 0;
        gbc.gridy = 0;
        mainPanel.add(registrationPanel, gbc);

        JPanel thresholdPanel = new JPanel(new GridBagLayout());
        thresholdPanel.setForeground(Color.black);
        thresholdPanel.setBorder(buildTitledBorder("Data thresholds"));

        if (useRed) {
            threshold1Label = new JLabel("Red data threshold ");
        } else if (useGreen) {
            threshold1Label = new JLabel("Green data threshold ");
        } else {
            threshold1Label = new JLabel(firstName + " data threshold ");
        }

        threshold1Label.setForeground(Color.black);
        threshold1Label.setFont(serif12);
        thresholdPanel.add(threshold1Label, gbc);

        gbc.gridx = 1;
        gbc.gridy = 0;
        threshold1Text = new JTextField();

        if ((firstImage.getType() == ModelStorageBase.FLOAT) || (firstImage.getType() == ModelStorageBase.DOUBLE)) {
            threshold1 = (float) firstImage.getMin();
            threshold1Text.setText(String.valueOf(threshold1));
        } else {
            threshold1Text.setText("1");
        }

        threshold1Text.setFont(serif12);
        thresholdPanel.add(threshold1Text, gbc);

        gbc.gridx = 0;
        gbc.gridy = 1;

        if (useBlue) {
            threshold2Label = new JLabel("Blue data threshold ");
        } else if (useGreen) {
            threshold2Label = new JLabel("Green data threshold ");
        } else {
            threshold2Label = new JLabel(secondName + " data threshold ");
        }

        threshold2Label.setForeground(Color.black);
        threshold2Label.setFont(serif12);
        thresholdPanel.add(threshold2Label, gbc);

        gbc.gridx = 1;
        gbc.gridy = 1;
        threshold2Text = new JTextField();

        if ((secondImage != null) &&
                ((secondImage.getType() == ModelStorageBase.FLOAT) ||
                     (secondImage.getType() == ModelStorageBase.DOUBLE))) {
            threshold2 = (float) secondImage.getMin();
            threshold2Text.setText(String.valueOf(threshold2));
        } else {
            threshold2Text.setText("1");
        }

        threshold2Text.setFont(serif12);
        thresholdPanel.add(threshold2Text, gbc);

        gbc.gridx = 0;
        gbc.gridy = 2;

        ButtonGroup thresholdGroup = new ButtonGroup();
        orButton = new JRadioButton("OR", true);
        orButton.setFont(serif12);
        thresholdGroup.add(orButton);
        thresholdPanel.add(orButton, gbc);

        gbc.gridx = 1;
        gbc.gridy = 2;
        andButton = new JRadioButton("AND", false);
        andButton.setFont(serif12);
        thresholdGroup.add(andButton);
        thresholdPanel.add(andButton, gbc);

        gbc.gridx = 0;
        gbc.gridy = 1;
        mainPanel.add(thresholdPanel, gbc);

        JPanel rescalePanel = new JPanel(new GridBagLayout());
        rescalePanel.setForeground(Color.black);
        rescalePanel.setBorder(buildTitledBorder("Bin numbers"));

        if (useRed) {
            bin1Label = new JLabel("Red bin number ");
        } else if (useGreen) {
            bin1Label = new JLabel("Green bin number ");
        } else {
            bin1Label = new JLabel(firstName + " bin number ");
        }

        bin1Label.setForeground(Color.black);
        bin1Label.setFont(serif12);
        gbc.gridx = 0;
        gbc.gridy = 0;
        rescalePanel.add(bin1Label, gbc);

        gbc.gridx = 1;
        gbc.gridy = 0;
        bin1Text = new JTextField();
        bin1Text.setText(String.valueOf(bin1));
        bin1Text.setFont(serif12);
        rescalePanel.add(bin1Text, gbc);

        gbc.gridx = 0;
        gbc.gridy = 1;

        if (useBlue) {
            bin2Label = new JLabel("Blue bin number ");
        } else if (useGreen) {
            bin2Label = new JLabel("Green bin number ");
        } else {
            bin2Label = new JLabel(secondName + " bin number ");
        }

        bin2Label.setForeground(Color.black);
        bin2Label.setFont(serif12);
        rescalePanel.add(bin2Label, gbc);

        gbc.gridx = 1;
        gbc.gridy = 1;
        bin2Text = new JTextField();
        bin2Text.setText(String.valueOf(bin2));
        bin2Text.setFont(serif12);
        rescalePanel.add(bin2Text, gbc);
        gbc.gridx = 0;
        gbc.gridy = 2;
        mainPanel.add(rescalePanel, gbc);

        JPanel optionsPanel = new JPanel();
        optionsPanel.setLayout(new GridBagLayout());
        optionsPanel.setForeground(Color.black);
        optionsPanel.setBorder(buildTitledBorder("Options"));

        gaussianLabel = new JLabel("Number of gaussians");
        gaussianLabel.setFont(serif12);
        gaussianLabel.setForeground(Color.black);
        gbc.gridx = 0;
        gbc.gridy = 0;
        optionsPanel.add(gaussianLabel, gbc);

        gaussianText = new JTextField("4");
        gaussianText.setFont(serif12);
        gbc.gridx = 1;
        gbc.gridy = 0;
        optionsPanel.add(gaussianText, gbc);

        iterationLabel = new JLabel("Number of iterations");
        iterationLabel.setFont(serif12);
        iterationLabel.setForeground(Color.black);
        gbc.gridx = 0;
        gbc.gridy = 1;
        optionsPanel.add(iterationLabel, gbc);

        iterationText = new JTextField("20");
        iterationText.setFont(serif12);
        gbc.gridx = 1;
        gbc.gridy = 1;
        optionsPanel.add(iterationText, gbc);


        gbc.gridx = 0;
        gbc.gridy = 3;
        mainPanel.add(optionsPanel, gbc);

        JPanel imageVOIPanel = new JPanel();
        imageVOIPanel.setLayout(new BorderLayout());
        imageVOIPanel.setForeground(Color.black);
        imageVOIPanel.setBorder(buildTitledBorder("Colocalization region"));

        ButtonGroup imageVOIGroup = new ButtonGroup();
        wholeImage = new JRadioButton("Whole image", true);
        wholeImage.setFont(serif12);
        wholeImage.addItemListener(this);
        imageVOIGroup.add(wholeImage);
        imageVOIPanel.add(wholeImage, BorderLayout.NORTH);

        VOIRegions = new JRadioButton("VOI region", false);
        VOIRegions.setFont(serif12);
        VOIRegions.addItemListener(this);
        imageVOIGroup.add(VOIRegions);
        imageVOIPanel.add(VOIRegions, BorderLayout.CENTER);

        if (nBoundingVOIs != 1) {
            VOIRegions.setEnabled(false);
        }

        gbc.gridx = 0;
        gbc.gridy = 4;
        mainPanel.add(imageVOIPanel, gbc);


        buildOKButton();
        buildCancelButton();

        JPanel buttonPanel = new JPanel();
        buttonPanel.add(OKButton);
        buttonPanel.add(cancelButton);

        getContentPane().add(imagePanel, BorderLayout.NORTH);
        getContentPane().add(mainPanel, BorderLayout.CENTER);
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

        entireImage = wholeImage.isSelected();

        if ((!entireImage) && (nBoundingVOIs > 1)) {
            MipavUtil.displayError("Only 1 contour VOI may be present");

            return false;
        }

        tmpStr = gaussianText.getText();
        gaussians = Integer.parseInt(tmpStr);

        tmpStr = iterationText.getText();
        iterations = Integer.parseInt(tmpStr);

        register = regCheckBox.isSelected();

        switch (comboBoxCostFunct.getSelectedIndex()) {

            case 0:
                cost = AlgorithmCostFunctions.CORRELATION_RATIO_SMOOTHED;
                break;

            case 1:
                cost = AlgorithmCostFunctions.LEAST_SQUARES_SMOOTHED;
                break;
                // case 2:  cost = AlgorithmCostFunctions.MUTUAL_INFORMATION_SMOOTHED;             break;

            case 2:
                cost = AlgorithmCostFunctions.NORMALIZED_XCORRELATION_SMOOTHED;
                break;

            case 3:
                cost = AlgorithmCostFunctions.NORMALIZED_MUTUAL_INFORMATION_SMOOTHED;
                break;

            default:
                cost = AlgorithmCostFunctions.CORRELATION_RATIO_SMOOTHED;
                break;
        }

        tmpStr = threshold1Text.getText();
        threshold1 = Float.parseFloat(tmpStr);

        tmpStr = threshold2Text.getText();
        threshold2 = Float.parseFloat(tmpStr);

        doOr = orButton.isSelected();

        if (firstImage.isColorImage()) {
            UI = ViewUserInterface.getReference();

            if (colorsPresent == 2) { }
            else if (((redCheckBox.isSelected()) && (greenCheckBox.isSelected()) && (!blueCheckBox.isSelected())) ||
                         ((redCheckBox.isSelected()) && (!greenCheckBox.isSelected()) && (blueCheckBox.isSelected())) ||
                         ((!redCheckBox.isSelected()) && (greenCheckBox.isSelected()) && (blueCheckBox.isSelected()))) {
                useRed = redCheckBox.isSelected();
                useGreen = greenCheckBox.isSelected();
                useBlue = blueCheckBox.isSelected();
            } else {
                MipavUtil.displayError("Exactly 2 color boxes must be checked");

                return false;
            }

            if (useRed) {

                if (entireImage) {
                    possibleIntValues = firstImage.getMaxR() - firstImage.getMinR() + 1;
                } else {
                    possibleIntValues = maxRV - minRV + 1;
                }
            } else {

                if (entireImage) {
                    possibleIntValues = firstImage.getMaxG() - firstImage.getMinG() + 1;
                } else {
                    possibleIntValues = maxGV - minGV + 1;
                }
            }

            if (useBlue) {

                if (entireImage) {
                    possibleInt2Values = firstImage.getMaxB() - firstImage.getMinB() + 1;
                } else {
                    possibleInt2Values = maxBV - minBV + 1;
                }
            } else {

                if (entireImage) {
                    possibleInt2Values = firstImage.getMaxG() - firstImage.getMinG() + 1;
                } else {
                    possibleInt2Values = maxGV - minGV + 1;
                }
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
            } else if ((bin2 > Math.round(possibleInt2Values)) &&
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
            UI = ViewUserInterface.getReference();

            String selectedName = (String) imageComboBox.getSelectedItem();
            secondImage = UI.getRegisteredImageByName(selectedName);

            if (secondImage == null) {
                return false;
            }

            if (entireImage) {
                possibleIntValues = firstImage.getMax() - firstImage.getMin() + 1;
            } else {
                possibleIntValues = maxV - minV + 1;
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

            if (entireImage) {
                possibleInt2Values = secondImage.getMax() - secondImage.getMin() + 1;
            } else {
                possibleInt2Values = secondMaxV - secondMinV + 1;
            }

            tmpStr = bin2Text.getText();
            bin2 = Integer.parseInt(tmpStr);

            if (bin2 < 1) {
                MipavUtil.displayError("Image 2 must have at least 1 bin");
                bin2Text.requestFocus();
                bin2Text.selectAll();

                return false;
            } else if ((bin2 > Math.round(possibleInt2Values)) &&
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
