package gov.nih.mipav.view.dialogs;


import gov.nih.mipav.model.algorithms.*;
import gov.nih.mipav.model.file.*;
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
 * Dialog to get user input Calculate Pearson's correlation coefficient Calculate P value for this correlation
 * coefficient Identify colocalized pixels Algorithms are executed in their own thread.
 */
public class JDialogColocalizationRegression extends JDialogScriptableBase implements AlgorithmInterface {

    //~ Static fields/initializers -------------------------------------------------------------------------------------

    /** Use serialVersionUID for interoperability. */
    private static final long serialVersionUID = 2982353729666791012L;

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** DOCUMENT ME! */
    float point1 = 0.0f;

    /** DOCUMENT ME! */
    float point2 = 0.0f;

    /** DOCUMENT ME! */
    private float background1 = 1.0f;

    /** DOCUMENT ME! */
    private float background2 = 1.0f;

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
    private int bottomPad = 40;

    /** DOCUMENT ME! */
    private float[] buffer = null;

    /** DOCUMENT ME! */
    private AlgorithmColocalizationRegression colocalizationAlgo = null;

    /** DOCUMENT ME! */
    private JCheckBox colocCheckBox;

    /** DOCUMENT ME! */
    private int colorsPresent = 0;

    /** DOCUMENT ME! */
    private JComboBox comboBoxCostFunct;

    /** DOCUMENT ME! */
    private ViewJComponentEditImage componentImage1;

    /** DOCUMENT ME! */
    private ViewJComponentEditImage componentImage2;

    /** DOCUMENT ME! */
    private int cost;

    /** DOCUMENT ME! */
    private boolean doColocWithThresholds = true;

    /** DOCUMENT ME! */
    private boolean doSecondIteration = false;

    /** DOCUMENT ME! */
    private boolean doVOISubtraction;

    /** DOCUMENT ME! */
    private boolean entireImage = true;

    /** DOCUMENT ME! */
    private ModelImage firstImage;

    /** DOCUMENT ME! */
    private String firstName = null;

    /** DOCUMENT ME! */
    private JCheckBox greenCheckBox;

    /** DOCUMENT ME! */
    private JComboBox imageComboBox;

    /** DOCUMENT ME! */
    private int imageLength;

    /** DOCUMENT ME! */
    private JLabel labelCost;

    /** DOCUMENT ME! */
    private JLabel labelImage;

    /** DOCUMENT ME! */
    private int leftPad = 40;

    /** DOCUMENT ME! */
    private BitSet mask = null;

    /** DOCUMENT ME! */
    private JButton maskButton;

    /** DOCUMENT ME! */
    private String maskFileDir;

    /** DOCUMENT ME! */
    private String maskFileName;

    /** DOCUMENT ME! */
    private ModelImage maskImage = null;

    /** DOCUMENT ME! */
    private JRadioButton maskRadio;

    /** DOCUMENT ME! */
    private JTextField maskText;

    /** DOCUMENT ME! */
    private double maxR, maxG, maxB;

    /** DOCUMENT ME! */
    private double minR, minG, minB;

    /** DOCUMENT ME! */
    private int nBoundingVOIs;

    /** DOCUMENT ME! */
    private JLabel point1Label;

    /** DOCUMENT ME! */
    private JTextField point1Text;

    /** DOCUMENT ME! */
    private JLabel point2Label;

    /** DOCUMENT ME! */
    private JTextField point2Text;

    /** DOCUMENT ME! */
    private boolean pointCalculation = false;

    /** DOCUMENT ME! */
    private JCheckBox pointCheckBox;

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
    private int rightPad = 20;

    /** DOCUMENT ME! */
    private ModelImage secondImage = null;

    /** DOCUMENT ME! */
    private JCheckBox secondIterationCheckBox;

    /** DOCUMENT ME! */
    private String secondName = null;

    /** DOCUMENT ME! */
    private int topPad = 20;

    /** DOCUMENT ME! */
    private ViewUserInterface UI;

    /** DOCUMENT ME! */
    private boolean useBlue = false;

    /** DOCUMENT ME! */
    private boolean useGreen = false;

    /** DOCUMENT ME! */
    private boolean useRed = false;

    /** DOCUMENT ME! */
    private JCheckBox VOICheckBox;

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
    public JDialogColocalizationRegression() { }

    /**
     * Creates a new JDialogColocalizationRegression object.
     *
     * @param  firstImage  DOCUMENT ME!
     */
    public JDialogColocalizationRegression(ModelImage firstImage) {
        super();
        this.UI = ViewUserInterface.getReference();
        this.firstImage = firstImage;
        parentFrame = firstImage.getParentFrame();
        componentImage1 = ((ViewJFrameImage) parentFrame).getComponentImage();
    }

    /**
     * Creates new dialog.
     *
     * @param  theParentFrame  Parent frame
     * @param  im              Source image
     */
    public JDialogColocalizationRegression(Frame theParentFrame, ModelImage im) {
        super(theParentFrame, false);
        firstImage = im;
        componentImage1 = ((ViewJFrameImage) theParentFrame).getComponentImage();
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
        } else if (command.equals("Help")) {
            MipavUtil.showHelp("10052");
        } else if (command.equals("Cancel")) {
            componentImage1.getVOIHandler().setPresetHue(-1.0f);

            if (componentImage2 != null) {
                componentImage2.getVOIHandler().setPresetHue(-1.0f);
            }

            dispose();
        } else if (command.equals("Mask")) {

            try {
                JFileChooser chooser = new JFileChooser();

                UI = ViewUserInterface.getReference();

                if (UI.getDefaultDirectory() != null) {
                    File file = new File(UI.getDefaultDirectory());

                    if (file != null) {
                        chooser.setCurrentDirectory(file);
                    } else {
                        chooser.setCurrentDirectory(new File(System.getProperty("user.dir")));
                    }
                } else {
                    chooser.setCurrentDirectory(new File(System.getProperty("user.dir")));
                }

                chooser.addChoosableFileFilter(new ViewImageFileFilter(ViewImageFileFilter.GEN));
                chooser.addChoosableFileFilter(new ViewImageFileFilter(ViewImageFileFilter.TECH));
                chooser.addChoosableFileFilter(new ViewImageFileFilter(ViewImageFileFilter.MICROSCOPY));
                chooser.addChoosableFileFilter(new ViewImageFileFilter(ViewImageFileFilter.MISC));

                chooser.setDialogTitle("Open mask file");
                maskFileDir = String.valueOf(chooser.getCurrentDirectory()) + File.separatorChar;

                int returnValue = chooser.showOpenDialog(UI.getMainFrame());

                if (returnValue == JFileChooser.APPROVE_OPTION) {
                    maskFileName = chooser.getSelectedFile().getName();
                    maskFileDir = String.valueOf(chooser.getCurrentDirectory()) + File.separatorChar;
                    UI.setDefaultDirectory(maskFileDir);
                } else {
                    maskFileName = null;
                }

                if (maskFileName != null) {
                    maskText.setText(maskFileName);
                } else {
                    wholeImage.setSelected(true);
                    maskText.setText("");
                }
            } catch (OutOfMemoryError e) {
                MipavUtil.displayError("Out of memory in JDialogColocalizationRegression.");

                return;
            }
        } // else if (command.equals("Mask"))
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

        if (algorithm instanceof AlgorithmColocalizationRegression) {
            firstImage.clearMask();

            insertScriptLine();

            // colocalizationAlgo.finalize();

            // colocalizationAlgo = null;
        } // if (algorithm instanceof AlgorithmColocalizationRegression)

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

        if (source == VOICheckBox) {

            if (VOICheckBox.isSelected()) {
                componentImage1.getVOIHandler().newVOI(2.0f / 3.0f); // blue
                //componentImage1.setCursorMode(ViewJComponentEditImage.NEW_VOI);
                //componentImage1.getVOIHandler().setPresetHue(2.0f / 3.0f); // blue

                if (componentImage2 != null) {
                    componentImage2.getVOIHandler().newVOI(2.0f / 3.0f); // blue
                    //componentImage2.setCursorMode(ViewJComponentEditImage.NEW_VOI);
                    //componentImage2.getVOIHandler().setPresetHue(2.0f / 3.0f); // blue
                }
            } // if (VOICheckBox.isSelected())
            else {
                componentImage1.getVOIHandler().setPresetHue(-1.0f);

                if (componentImage2 != null) {
                    componentImage2.getVOIHandler().setPresetHue(-1.0f);
                }
            }
        } // if (source == VOICheckBox)
        else if (source == regCheckBox) {

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
            componentImage2 = secondImage.getParentFrame().getComponentImage();

            possibleInt2Values = secondImage.getMax() - secondImage.getMin() + 1;

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

            bin2Label.setText(secondName + " bin number ");
            bin2Text.setText(String.valueOf(bin2));
        } // if ( source == imageComboBox)
        else if ((source == wholeImage) || (source == VOIRegions) || (source == maskRadio)) {
            entireImage = wholeImage.isSelected();

            if (maskRadio.isSelected()) {
                maskButton.setEnabled(true);
                maskText.setEnabled(true);
            } else {
                maskButton.setEnabled(false);
                maskText.setEnabled(false);
            }
        } // else if ((source == wholeImage) || (source == VOIRegions) ||

        // (source == maskRadio))
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
                    possibleIntValues = firstImage.getMaxR() - firstImage.getMinR() + 1;

                } else {
                    possibleIntValues = firstImage.getMaxG() - firstImage.getMinG() + 1;
                }

                if (((firstImage.getType() == ModelStorageBase.ARGB) ||
                         (firstImage.getType() == ModelStorageBase.ARGB_USHORT)) && (possibleIntValues < 256)) {
                    bin1 = (int) Math.round(possibleIntValues);
                }

                bin2 = 256;

                if (useBlue) {
                    possibleInt2Values = firstImage.getMaxB() - firstImage.getMinB() + 1;
                } else {
                    possibleInt2Values = firstImage.getMaxG() - firstImage.getMinG() + 1;
                }

                if (((firstImage.getType() == ModelStorageBase.ARGB) ||
                         (firstImage.getType() == ModelStorageBase.ARGB_USHORT)) && (possibleInt2Values < 256)) {
                    bin2 = (int) Math.round(possibleInt2Values);
                }

                bin1Text.setText(String.valueOf(bin1));
                bin2Text.setText(String.valueOf(bin2));

                if (useRed) {
                    bin1Label.setText("Red bin number ");
                    point1Label.setText("Red (" + minR + "-" + maxR + ")");
                    point1Text.setText(String.valueOf(minR));
                } else {
                    bin1Label.setText("Green bin number ");
                    point1Label.setText("Green (" + minG + "-" + maxG + ")");
                    point1Text.setText(String.valueOf(minG));
                }

                if (useBlue) {
                    bin2Label.setText("Blue bin number ");
                    point2Label.setText("Blue (" + minB + "-" + maxB + ")");
                    point2Text.setText(String.valueOf(minB));
                } else {
                    bin2Label.setText("Green bin number ");
                    point2Label.setText("Green (" + minG + "-" + maxG + ")");
                    point2Text.setText(String.valueOf(minG));
                }
            }
        } else if (source == pointCheckBox) {

            if (pointCheckBox.isSelected()) {
                point1Label.setEnabled(true);
                point2Label.setEnabled(true);
                point1Text.setEnabled(true);
                point2Text.setEnabled(true);
                secondIterationCheckBox.setSelected(false);
                secondIterationCheckBox.setEnabled(false);
            } else {
                point1Label.setEnabled(false);
                point2Label.setEnabled(false);
                point1Text.setEnabled(false);
                point2Text.setEnabled(false);
                secondIterationCheckBox.setEnabled(true);
            }
        }
    }


    /**
     * DOCUMENT ME!
     *
     * @param  background1  DOCUMENT ME!
     */
    public void setBackground1(float background1) {
        this.background1 = background1;
    }

    /**
     * DOCUMENT ME!
     *
     * @param  background2  DOCUMENT ME!
     */
    public void setBackground2(float background2) {
        this.background2 = background2;
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
     * @param  doColocWithThresholds  DOCUMENT ME!
     */
    public void setDoColocWithThresholds(boolean doColocWithThresholds) {
        this.doColocWithThresholds = doColocWithThresholds;
    }

    /**
     * DOCUMENT ME!
     *
     * @param  doSecondIteration  DOCUMENT ME!
     */
    public void setDoSecondIteration(boolean doSecondIteration) {
        this.doSecondIteration = doSecondIteration;
    }

    /**
     * DOCUMENT ME!
     *
     * @param  doVOISubtraction  DOCUMENT ME!
     */
    public void setDoVOISubtraction(boolean doVOISubtraction) {
        this.doVOISubtraction = doVOISubtraction;
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
     * @param  leftPad  DOCUMENT ME!
     */
    public void setLeftPad(int leftPad) {
        this.leftPad = leftPad;
    }

    /**
     * DOCUMENT ME!
     *
     * @param  point1  DOCUMENT ME!
     */
    public void setPoint1(float point1) {
        this.point1 = point1;
    }

    /**
     * DOCUMENT ME!
     *
     * @param  point2  DOCUMENT ME!
     */
    public void setPoint2(float point2) {
        this.point2 = point2;
    }

    /**
     * DOCUMENT ME!
     *
     * @param  pointCalculation  DOCUMENT ME!
     */
    public void setPointCalculation(boolean pointCalculation) {
        this.pointCalculation = pointCalculation;
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
     * @param  topPad  DOCUMENT ME!
     */
    public void setTopPad(int topPad) {
        this.topPad = topPad;
    }

    /**
     * DOCUMENT ME!
     *
     * @param   im  DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    public boolean setupMaskImage(ModelImage im) {
        this.maskImage = im;

        if (maskImage != null) {
            System.err.println("mask image aint null");

            if (maskImage.getNDims() != firstImage.getNDims()) {
                MipavUtil.displayError("Dimensions of mask image must match the source image.");

                return false;
            }

            for (int i = 0; i < firstImage.getNDims(); i++) {

                if (firstImage.getExtents()[i] != maskImage.getExtents()[i]) {
                    MipavUtil.displayError("Dimensions of mask image must match the source image.");

                    return false;
                }
            }

            xDim = firstImage.getExtents()[0];
            yDim = firstImage.getExtents()[1];
            imageLength = xDim * yDim;

            if (firstImage.getNDims() >= 3) {
                zDim = firstImage.getExtents()[2];
                imageLength = imageLength * zDim;
            }

            mask = new BitSet(imageLength);
            buffer = new float[imageLength];

            try {
                maskImage.exportData(0, imageLength, buffer);
            } catch (IOException e) {
                MipavUtil.displayError("IOException on maskImage.exportData");

                return false;
            }

            for (int i = 0; i < imageLength; i++) {

                if (buffer[i] > 0) {
                    mask.set(i);
                } else {
                    mask.clear(i);
                }
            }
        } else {
            return false;
        }

        return true;
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
     * Disposes of error dialog, then frame. Sets cancelled to <code>true</code>.
     *
     * @param  event  DOCUMENT ME!
     */
    public void windowClosing(WindowEvent event) {
        componentImage1.getVOIHandler().setPresetHue(-1.0f);

        if (componentImage2 != null) {
            componentImage2.getVOIHandler().setPresetHue(-1.0f);
        }

        cancelFlag = true;
        dispose();
    }

    /**
     * DOCUMENT ME!
     */
    protected void callAlgorithm() {
        componentImage1.getVOIHandler().setPresetHue(-1.0f);

        if (componentImage2 != null) {
            componentImage2.getVOIHandler().setPresetHue(-1.0f);
        }

        String name = makeImageName(firstImage.getImageName(), "_hist2Dim");

        try {
            int[] extents = new int[2];

            // Allow padding space at left and bottom
            extents[0] = bin1 + leftPad + rightPad;
            extents[1] = bin2 + bottomPad + topPad;

            // Allow log of 1 + counts to be displayed
            resultImage = new ModelImage(ModelStorageBase.DOUBLE, extents, name);

            // Make algorithm

            if (firstImage.isColorImage()) {
                // System.err.println("Buffer len is: " + buffer.length); System.err.println(bin1 + " " + bin2 + " " +
                // background1 + " " + background2 + " " + leftPad + " " + rightPad +                     " " +
                // bottomPad + " " + " " + topPad + " " + useRed + " " + useGreen + " " + useBlue + " " +
                // doColocWithThresholds + " "                     + entireImage + " " + register + " " + cost + " " +
                // doSecondIteration + " " + doVOISubtraction);

                colocalizationAlgo = new AlgorithmColocalizationRegression(resultImage, firstImage, mask, bin1, bin2,
                                                                           background1, background2, leftPad, rightPad,
                                                                           bottomPad, topPad, useRed, useGreen, useBlue,
                                                                           doColocWithThresholds, entireImage, register,
                                                                           cost, doSecondIteration, doVOISubtraction,
                                                                           pointCalculation, point1, point2);
            } else {
                colocalizationAlgo = new AlgorithmColocalizationRegression(resultImage, firstImage, secondImage, mask,
                                                                           bin1, bin2, background1, background2,
                                                                           leftPad, rightPad, bottomPad, topPad,
                                                                           doColocWithThresholds, entireImage, register,
                                                                           cost, doSecondIteration, doVOISubtraction,
                                                                           pointCalculation, point1, point2);
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

            System.err.println("Checking for resultimage == null");

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
        background1 = scriptParameters.getParams().getFloat("background1");
        background2 = scriptParameters.getParams().getFloat("background2");
        leftPad = scriptParameters.getParams().getInt("leftPad");
        rightPad = scriptParameters.getParams().getInt("rightPad");
        bottomPad = scriptParameters.getParams().getInt("bottomPad");
        topPad = scriptParameters.getParams().getInt("topPad");
        useRed = scriptParameters.getParams().getBoolean("useRed");
        useGreen = scriptParameters.getParams().getBoolean("useGreen");
        useBlue = scriptParameters.getParams().getBoolean("useBlue");
        doColocWithThresholds = scriptParameters.getParams().getBoolean("doColocWithThresholds");
        entireImage = scriptParameters.getParams().getBoolean("entireImage");
        register = scriptParameters.getParams().getBoolean("register");
        cost = scriptParameters.getParams().getInt("cost");
        doSecondIteration = scriptParameters.getParams().getBoolean("doSecondIteration");
        doVOISubtraction = scriptParameters.getParams().getBoolean("doVOISubtraction");
        pointCalculation = scriptParameters.getParams().getBoolean("pointCalculation");
        point1 = scriptParameters.getParams().getFloat("point1");
        point2 = scriptParameters.getParams().getFloat("point2");
    }

    /**
     * Record the parameters just used to run this algorithm in a script.
     *
     * @throws  ParserException  If there is a problem creating/recording the new parameters.
     */
    protected void storeParamsFromGUI() throws ParserException {

        scriptParameters.storeInputImage(firstImage);

        scriptParameters.getParams().put(ParameterFactory.newParameter("bin1", bin1));
        scriptParameters.getParams().put(ParameterFactory.newParameter("bin2", bin2));
        scriptParameters.getParams().put(ParameterFactory.newParameter("background1", background1));
        scriptParameters.getParams().put(ParameterFactory.newParameter("background2", background2));
        scriptParameters.getParams().put(ParameterFactory.newParameter("leftPad", leftPad));
        scriptParameters.getParams().put(ParameterFactory.newParameter("rightPad", rightPad));
        scriptParameters.getParams().put(ParameterFactory.newParameter("bottomPad", bottomPad));
        scriptParameters.getParams().put(ParameterFactory.newParameter("topPad", topPad));
        scriptParameters.getParams().put(ParameterFactory.newParameter("useRed", useRed));
        scriptParameters.getParams().put(ParameterFactory.newParameter("useGreen", useGreen));
        scriptParameters.getParams().put(ParameterFactory.newParameter("useBlue", useBlue));
        scriptParameters.getParams().put(ParameterFactory.newParameter("doColocWithThresholds", doColocWithThresholds));
        scriptParameters.getParams().put(ParameterFactory.newParameter("entireImage", entireImage));
        scriptParameters.getParams().put(ParameterFactory.newParameter("register", register));
        scriptParameters.getParams().put(ParameterFactory.newParameter("cost", cost));
        scriptParameters.getParams().put(ParameterFactory.newParameter("doSecondIteration", doSecondIteration));
        scriptParameters.getParams().put(ParameterFactory.newParameter("doVOISubtraction", doVOISubtraction));
        scriptParameters.getParams().put(ParameterFactory.newParameter("pointCalculation", pointCalculation));
        scriptParameters.getParams().put(ParameterFactory.newParameter("point1", point1));
        scriptParameters.getParams().put(ParameterFactory.newParameter("point2", point2));

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
        setTitle("Orthogonal Regression Colocalization");
        firstName = firstImage.getImageName();

        VOIs = firstImage.getVOIs();
        nVOIs = VOIs.size();
        nBoundingVOIs = 0;

        for (i = 0; i < nVOIs; i++) {

            if (VOIs.VOIAt(i).getCurveType() == VOI.CONTOUR) {
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

                gbc2.gridx = 1;
                greenCheckBox = new JCheckBox("Green");
                greenCheckBox.setFont(serif12);
                greenCheckBox.setForeground(Color.black);
                greenCheckBox.setSelected(true);
                greenCheckBox.addItemListener(this);
                imagePanel.add(greenCheckBox, gbc2);

                gbc2.gridx = 2;
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
            componentImage2 = secondImage.getParentFrame().getComponentImage();

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

        JPanel backgroundPanel = new JPanel(new GridBagLayout());

        backgroundPanel.setForeground(Color.black);
        backgroundPanel.setBorder(buildTitledBorder("VOI Background Subtraction"));

        if (firstImage.isColorImage()) {
            VOICheckBox = new JCheckBox("Subtract average VOI level");
        } else {
            VOICheckBox = new JCheckBox("Subtract average VOI levels");
        }

        VOICheckBox.setFont(serif12);
        VOICheckBox.setForeground(Color.blue);
        VOICheckBox.setSelected(false);
        VOICheckBox.addItemListener(this);
        backgroundPanel.add(VOICheckBox, gbc);

        if ((firstImage.getType() == ModelStorageBase.FLOAT) || (firstImage.getType() == ModelStorageBase.DOUBLE)) {
            background1 = (float) firstImage.getMin();
        }

        if ((secondImage != null) &&
                ((secondImage.getType() == ModelStorageBase.FLOAT) ||
                     (secondImage.getType() == ModelStorageBase.DOUBLE))) {
            background2 = (float) secondImage.getMin();
        }

        gbc.gridx = 0;
        gbc.gridy = 1;
        mainPanel.add(backgroundPanel, gbc);

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

        secondIterationCheckBox = new JCheckBox("Second iteration excluding subthresholded region");
        secondIterationCheckBox.setFont(serif12);
        secondIterationCheckBox.setForeground(Color.black);
        secondIterationCheckBox.setSelected(false);
        gbc.gridx = 0;
        gbc.gridy = 0;
        optionsPanel.add(secondIterationCheckBox, gbc);

        colocCheckBox = new JCheckBox("Limit colocalization to pixels >= threshold");
        colocCheckBox.setFont(serif12);
        colocCheckBox.setForeground(Color.black);
        colocCheckBox.setSelected(true);
        gbc.gridx = 0;
        gbc.gridy = 1;
        optionsPanel.add(colocCheckBox, gbc);

        pointCheckBox = new JCheckBox("Calculate linear coefficient at this point");
        pointCheckBox.setFont(serif12);
        pointCheckBox.setForeground(Color.black);
        pointCheckBox.setSelected(false);
        pointCheckBox.addItemListener(this);
        gbc.gridx = 0;
        gbc.gridy = 2;
        optionsPanel.add(pointCheckBox, gbc);

        if (useRed) {
            point1Label = new JLabel("Red (" + minR + "-" + maxR + ")");
        } else if (useGreen) {
            point1Label = new JLabel("Green (" + minG + "-" + maxG + ")");
        } else {
            point1Label = new JLabel(firstName + " (" + firstImage.getMin() + "-" + firstImage.getMax() + ")");
        }

        point1Label.setForeground(Color.black);
        point1Label.setFont(serif12);
        point1Label.setEnabled(false);
        gbc.gridx = 0;
        gbc.gridy = 3;
        optionsPanel.add(point1Label, gbc);

        point1Text = new JTextField();

        if (useRed) {
            point1Text.setText(String.valueOf(minR));
        } else if (useGreen) {
            point1Text.setText(String.valueOf(minG));
        } else {
            point1Text.setText(String.valueOf(firstImage.getMin()));
        }

        point1Text.setFont(serif12);
        point1Text.setColumns(6);
        point1Text.setEnabled(false);
        gbc.gridx = 1;
        gbc.gridy = 3;
        optionsPanel.add(point1Text, gbc);

        if (useBlue) {
            point2Label = new JLabel("Blue (" + minB + "-" + maxB + ")");
        } else if (useGreen) {
            point2Label = new JLabel("Green (" + minG + "-" + maxG + ")");
        } else {
            point2Label = new JLabel(secondName + " (" + secondImage.getMin() + "-" + secondImage.getMax() + ")");
        }

        point2Label.setForeground(Color.black);
        point2Label.setFont(serif12);
        point2Label.setEnabled(false);
        gbc.gridx = 0;
        gbc.gridy = 4;
        optionsPanel.add(point2Label, gbc);

        point2Text = new JTextField();

        if (useBlue) {
            point2Text.setText(String.valueOf(minB));
        } else if (useGreen) {
            point2Text.setText(String.valueOf(minG));
        } else {
            point2Text.setText(String.valueOf(secondImage.getMin()));
        }

        point2Text.setFont(serif12);
        point2Text.setColumns(6);
        point2Text.setEnabled(false);
        gbc.gridx = 1;
        gbc.gridy = 4;
        optionsPanel.add(point2Text, gbc);

        gbc.gridx = 0;
        gbc.gridy = 3;
        mainPanel.add(optionsPanel, gbc);

        JPanel imageVOIPanel = new JPanel();

        imageVOIPanel.setLayout(new GridBagLayout());
        imageVOIPanel.setForeground(Color.black);
        imageVOIPanel.setBorder(buildTitledBorder("Colocalization region"));

        ButtonGroup imageVOIGroup = new ButtonGroup();

        wholeImage = new JRadioButton("Whole image", true);
        wholeImage.setFont(serif12);
        wholeImage.addItemListener(this);
        gbc.gridx = 0;
        gbc.gridy = 0;
        imageVOIGroup.add(wholeImage);
        imageVOIPanel.add(wholeImage, gbc);

        VOIRegions = new JRadioButton("VOI region", false);
        VOIRegions.setFont(serif12);
        VOIRegions.addItemListener(this);
        imageVOIGroup.add(VOIRegions);
        gbc.gridy = 1;
        imageVOIPanel.add(VOIRegions, gbc);

        if (nBoundingVOIs != 1) {
            VOIRegions.setEnabled(false);
        }

        maskRadio = new JRadioButton("Mask file", false);
        maskRadio.setFont(serif12);
        maskRadio.setForeground(Color.black);
        maskRadio.addItemListener(this);
        imageVOIGroup.add(maskRadio);
        gbc.gridy = 2;
        imageVOIPanel.add(maskRadio, gbc);

        maskButton = new JButton("Choose mask file");
        maskButton.setForeground(Color.black);
        maskButton.setFont(serif12B);
        maskButton.setEnabled(false);
        maskButton.addActionListener(this);
        maskButton.setActionCommand("Mask");
        gbc.gridx = 0;
        gbc.gridy = 3;
        gbc.gridwidth = 1;
        imageVOIPanel.add(maskButton, gbc);

        maskText = new JTextField();
        maskText.setFont(serif12);
        maskText.setEnabled(false);
        gbc.gridx = 1;
        gbc.gridy = 3;
        gbc.gridwidth = 3;
        gbc.weightx = 10;
        imageVOIPanel.add(maskText, gbc);

        gbc.gridx = 0;
        gbc.gridy = 4;
        gbc.gridwidth = 1;
        gbc.weightx = 1;
        mainPanel.add(imageVOIPanel, gbc);

        getContentPane().add(imagePanel, BorderLayout.NORTH);
        getContentPane().add(mainPanel, BorderLayout.CENTER);
        getContentPane().add(buildButtons(), BorderLayout.SOUTH);

        pack();
        setVisible(true);
    }

    /**
     * DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    private boolean setupMask() {

        try {

            if ((maskFileName == null) || (maskFileDir == null)) {
                System.err.println("yo");

                return false;
            }

            FileIO fileIO = new FileIO();
            System.err.println("about to return setMaskImage");

            return setupMaskImage(fileIO.readImage(maskFileName, maskFileDir, false, null));

        } catch (OutOfMemoryError e) {
            MipavUtil.displayError("Out of memory in JDialogColocalizationRegression.");

            return false;
        }
    }

    /**
     * Use the GUI results to set up the variables needed to run the algorithm.
     *
     * @return  <code>true</code> if parameters set successfully, <code>false</code> otherwise.
     */
    private boolean setVariables() {
        String tmpStr;

        entireImage = wholeImage.isSelected();

        if ((!entireImage) && (nBoundingVOIs > 2)) {
            MipavUtil.displayError("Only 2 contour VOIs may be present");

            return false;
        }

        if (!maskRadio.isSelected()) {
            mask = null;
        } else {

            // System.err.println("about to try setting up mask");
            if (!setupMask()) {
                mask = null;

                return false;
            }

            if (maskImage != null) {
                maskImage.disposeLocal();
            }
        }

        doVOISubtraction = VOICheckBox.isSelected();

        doSecondIteration = secondIterationCheckBox.isSelected();

        doColocWithThresholds = colocCheckBox.isSelected();

        pointCalculation = pointCheckBox.isSelected();

        if (pointCalculation) {
            tmpStr = point1Text.getText();
            point1 = Float.parseFloat(tmpStr);

            if (useRed && (point1 < minR)) {
                MipavUtil.displayError("Red must be at least " + minR);
                point1Text.requestFocus();
                point1Text.selectAll();

                return false;
            } else if (useRed && (point1 > maxR)) {
                MipavUtil.displayError("Red must not exceed " + maxR);
                point1Text.requestFocus();
                point1Text.selectAll();

                return false;
            } else if (useGreen && (point1 < minG)) {
                MipavUtil.displayError("Green must be at least " + minG);
                point1Text.requestFocus();
                point1Text.selectAll();

                return false;
            } else if (useGreen && (point1 > maxG)) {
                MipavUtil.displayError("Green must not exceed " + maxG);
                point1Text.requestFocus();
                point1Text.selectAll();

                return false;
            } else if ((!useRed) && (!useGreen) && (point1 < firstImage.getMin())) {
                MipavUtil.displayError(firstName + " must be at least " + firstImage.getMin());
                point1Text.requestFocus();
                point1Text.selectAll();

                return false;
            } else if ((!useRed) && (!useGreen) && (point1 > firstImage.getMax())) {
                MipavUtil.displayError(firstName + " must not exceed " + firstImage.getMax());
                point1Text.requestFocus();
                point1Text.selectAll();

                return false;
            }

            tmpStr = point2Text.getText();
            point2 = Float.parseFloat(tmpStr);

            if (useBlue && (point2 < minB)) {
                MipavUtil.displayError("Blue must be at least " + minB);
                point2Text.requestFocus();
                point2Text.selectAll();

                return false;
            } else if (useBlue && (point2 > maxB)) {
                MipavUtil.displayError("Blue must not exceed " + maxB);
                point2Text.requestFocus();
                point2Text.selectAll();

                return false;
            } else if (useGreen && (point2 < minG)) {
                MipavUtil.displayError("Green must be at least " + minG);
                point2Text.requestFocus();
                point2Text.selectAll();

                return false;
            } else if (useGreen && (point2 > maxG)) {
                MipavUtil.displayError("Green must not exceed " + maxG);
                point2Text.requestFocus();
                point2Text.selectAll();

                return false;
            } else if ((!useRed) && (!useGreen) && (point2 < secondImage.getMin())) {
                MipavUtil.displayError(secondName + " must be at least " + secondImage.getMin());
                point2Text.requestFocus();
                point2Text.selectAll();

                return false;
            } else if ((!useRed) && (!useGreen) && (point2 > secondImage.getMax())) {
                MipavUtil.displayError(secondName + " must not exceed " + secondImage.getMax());
                point2Text.requestFocus();
                point2Text.selectAll();

                return false;
            }

        } // if (pointCalculation)

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
                possibleIntValues = firstImage.getMaxR() - firstImage.getMinR() + 1;
            } else {
                possibleIntValues = firstImage.getMaxG() - firstImage.getMinG() + 1;
            }

            if (useBlue) {
                possibleInt2Values = firstImage.getMaxB() - firstImage.getMinB() + 1;
            } else {
                possibleInt2Values = firstImage.getMaxG() - firstImage.getMinG() + 1;
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

            componentImage2 = secondImage.getParentFrame().getComponentImage();

            possibleIntValues = firstImage.getMax() - firstImage.getMin() + 1;
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
