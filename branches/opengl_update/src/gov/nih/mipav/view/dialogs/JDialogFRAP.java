package gov.nih.mipav.view.dialogs;


import gov.nih.mipav.model.algorithms.*;
import gov.nih.mipav.model.file.*;
import gov.nih.mipav.model.structures.*;

import gov.nih.mipav.view.*;

import java.awt.*;
import java.awt.event.*;

import java.util.*;

import javax.swing.*;


/**
 * Dialog to get user input Fluorescence recovery after photobleaching Algorithms are executed in their own thread.
 */
public class JDialogFRAP extends JDialogBase implements AlgorithmInterface, ItemListener, WindowListener {

    //~ Static fields/initializers -------------------------------------------------------------------------------------

    /** Use serialVersionUID for interoperability. */
    private static final long serialVersionUID = 6390017817188485094L;

    /** Bleached ROI shapes. */
    private static final int RECTANGLE = 18;

    /** DOCUMENT ME! */
    private static final int ELLIPSE = 19;

    /** DOCUMENT ME! */
    private static final int CLOSED_POLYLINE = 20;

    /** DOCUMENT ME! */
    private static final int CLOSED_BEZIER = 22;

    /** DOCUMENT ME! */
    private static final int CIRCLE = 24;

    /** Diffusion models. */
    private static final int NARROW_BAND_2D = 1;

    /** DOCUMENT ME! */
    private static final int CIRCLE_2D = 2;

    /** DOCUMENT ME! */
    private static final int PURE_1D = 3;

    /** DOCUMENT ME! */
    private static final int SINGLE_EXPONENTIAL = 4;

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** DOCUMENT ME! */
    private ButtonGroup analysisGroup;

    /** DOCUMENT ME! */
    private JRadioButton backgroundButton;

    /** DOCUMENT ME! */
    private int backgroundIndex = -1;

    /** DOCUMENT ME! */
    private JRadioButton bandButton;

    /** DOCUMENT ME! */
    private int bleachedROIShape = -1;

    /** DOCUMENT ME! */
    private JRadioButton blueButton;

    /** DOCUMENT ME! */
    private JRadioButton circleButton;

    /** DOCUMENT ME! */
    private ButtonGroup colorGroup;

    /** DOCUMENT ME! */
    private int colorsPresent = 0;

    /** DOCUMENT ME! */
    private JComboBox comboBoxCostFunct;

    /** DOCUMENT ME! */
    private ViewJComponentEditImage componentImage;

    /** DOCUMENT ME! */
    private int cost;

    /** DOCUMENT ME! */
    private JCheckBox createRegCheckBox;

    /** DOCUMENT ME! */
    private boolean createRegImage = false;

    /** DOCUMENT ME! */
    private float diffusion = 0.0f;

    /** DOCUMENT ME! */
    private FileInfoLSM fileInfo;

    /** DOCUMENT ME! */
    private FileInfoImageXML fileInfoImageXML;

    /** DOCUMENT ME! */
    private int firstSliceAfterBleach = -1;

    /** DOCUMENT ME! */
    private int firstSliceNum;

    /** DOCUMENT ME! */
    private JTextField firstSliceNumText;


    /** DOCUMENT ME! */
    private AlgorithmFRAP frapAlgo = null;

    /** DOCUMENT ME! */
    private JRadioButton greenButton;

    /** DOCUMENT ME! */
    private ModelImage image;

    /** DOCUMENT ME! */
    private double[] knotX = null;

    /** DOCUMENT ME! */
    private JLabel labelCost;

    /** DOCUMENT ME! */
    private JLabel labelDiffusion;

    /** DOCUMENT ME! */
    private JLabel labelFirstSlice;

    /** DOCUMENT ME! */
    private JLabel labelImage;

    /** DOCUMENT ME! */
    private JLabel labelRadius;

    /** DOCUMENT ME! */
    private JLabel labelRef;

    /** DOCUMENT ME! */
    private JLabel labelShape;

    /** DOCUMENT ME! */
    private double maxR, maxG, maxB;

    /** DOCUMENT ME! */
    private double minR, minG, minB;

    /** DOCUMENT ME! */
    private int model = 1;

    /** DOCUMENT ME! */
    private int nBoundingVOIs;

    /** DOCUMENT ME! */
    private JRadioButton oneDButton;

    /** DOCUMENT ME! */
    private boolean paramVary = false;

    /** DOCUMENT ME! */
    private JCheckBox paramVaryCheckBox;

    /** DOCUMENT ME! */
    private JRadioButton photoBleachedButton;

    /** DOCUMENT ME! */
    private int photoBleachedIndex = -1;

    /** DOCUMENT ME! */
    private float radius = 0.0f;

    /** DOCUMENT ME! */
    private JRadioButton redButton;


    /** DOCUMENT ME! */
    private JCheckBox regCheckBox;

    /** DOCUMENT ME! */
    private boolean register;

    /** DOCUMENT ME! */
    private JRadioButton singleExpButton;

    /** DOCUMENT ME! */
    private JTextField textDiffusion;

    /** DOCUMENT ME! */
    private JTextField textRadius;

    /** DOCUMENT ME! */
    private boolean useBlue = false;

    /** DOCUMENT ME! */
    private boolean useGreen = false;

    /** DOCUMENT ME! */
    private boolean useRed = false;

    /** DOCUMENT ME! */
    private ButtonGroup VOIGroup;

    /** DOCUMENT ME! */
    private ViewVOIVector VOIs;

    /** DOCUMENT ME! */
    private JRadioButton wholeOrganButton;

    /** DOCUMENT ME! */
    private JCheckBox wholeOrganCheckBox;

    /** DOCUMENT ME! */
    private int wholeOrganIndex = -1;

    /** DOCUMENT ME! */
    private boolean wholeOrganNormalize = false;

    /** DOCUMENT ME! */
    private int xPos, yPos;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Creates a new JDialogFRAP object.
     *
     * @param  image  DOCUMENT ME!
     */
    public JDialogFRAP(ModelImage image) {
        super();
        this.image = image;
        parentFrame = image.getParentFrame();
        componentImage = ((ViewJFrameImage) parentFrame).getComponentImage();
    }

    /**
     * Creates new dialog.
     *
     * @param  theParentFrame  Parent frame
     * @param  im              Source image
     */
    public JDialogFRAP(Frame theParentFrame, ModelImage im) {
        super(theParentFrame, false);
        image = im;
        componentImage = ((ViewJFrameImage) theParentFrame).getComponentImage();
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
        } else if (command.equals("Script")) {
            callAlgorithm();
        } else if (command.equals("Help")) {
            //MipavUtil.showHelp("10083");
            MipavUtil.showWebHelp("Microscopy:_FRAP_(Fluorescence_Recovery_After_Photobleaching)#Applying_the_FRAP_algorithm");
        } else if (command.equals("Cancel")) {
            componentImage.getVOIHandler().setPresetHue(-1.0f);
            dispose();
        } else if ((source == photoBleachedButton) || (source == wholeOrganButton) || (source == backgroundButton)) {

            if (photoBleachedButton.isSelected()) {
                componentImage.getVOIHandler().newVOI(0.0f); // red
                //componentImage.setCursorMode(ViewJComponentEditImage.NEW_VOI);
                //componentImage.getVOIHandler().setPresetHue(0.0f); // red
            } else if (wholeOrganButton.isSelected()) {
                componentImage.getVOIHandler().newVOI(1.0f / 3.0f); // green
                //componentImage.setCursorMode(ViewJComponentEditImage.NEW_VOI);
                //componentImage.getVOIHandler().setPresetHue(1.0f / 3.0f); // green
            } else if (backgroundButton.isSelected()) {
                componentImage.getVOIHandler().newVOI(2.0f / 3.0f); // blue
                //componentImage.setCursorMode(ViewJComponentEditImage.NEW_VOI);
                //componentImage.getVOIHandler().setPresetHue(2.0f / 3.0f); // blue
            }
        } else if ((source == bandButton) || (source == circleButton) || (source == oneDButton) ||
                       (source == singleExpButton)) {

            if ((bandButton.isSelected()) && (!circleButton.isSelected()) && (!oneDButton.isSelected()) &&
                    (!singleExpButton.isSelected())) {
                wholeOrganCheckBox.setSelected(true);
                wholeOrganCheckBox.setEnabled(false);
                labelRadius.setEnabled(false);
                textRadius.setEnabled(false);
                labelDiffusion.setEnabled(false);
                textDiffusion.setEnabled(false);
            } else if ((!bandButton.isSelected()) && (circleButton.isSelected()) && (!oneDButton.isSelected()) &&
                           (!singleExpButton.isSelected())) {
                wholeOrganCheckBox.setSelected(false);
                wholeOrganCheckBox.setEnabled(false);
                labelRadius.setEnabled(true);
                textRadius.setEnabled(true);
                labelDiffusion.setEnabled(true);
                textDiffusion.setEnabled(true);
            } else if ((!bandButton.isSelected()) && (!circleButton.isSelected()) && (oneDButton.isSelected()) &&
                           (!singleExpButton.isSelected())) {
                wholeOrganCheckBox.setEnabled(true);
                labelRadius.setEnabled(false);
                textRadius.setEnabled(false);
                labelDiffusion.setEnabled(false);
                textDiffusion.setEnabled(false);
            } else if ((!bandButton.isSelected()) && (!circleButton.isSelected()) && (!oneDButton.isSelected()) &&
                           (singleExpButton.isSelected())) {
                wholeOrganCheckBox.setEnabled(true);
                labelRadius.setEnabled(false);
                textRadius.setEnabled(false);
                labelDiffusion.setEnabled(false);
                textDiffusion.setEnabled(false);
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


        if (frapAlgo.isCompleted() == true) { }

        dispose();
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

        if (source == regCheckBox) {

            if (regCheckBox.isSelected()) {
                labelCost.setEnabled(true);
                comboBoxCostFunct.setEnabled(true);
                createRegCheckBox.setEnabled(true);
                createRegCheckBox.setSelected(true);
            } else {
                labelCost.setEnabled(false);
                comboBoxCostFunct.setEnabled(false);
                createRegCheckBox.setEnabled(false);
                createRegCheckBox.setSelected(false);
            }
        } else if (source == wholeOrganCheckBox) {

            if (wholeOrganCheckBox.isSelected()) {
                wholeOrganButton.setEnabled(true);
            } else {
                wholeOrganButton.setEnabled(false);

                if (wholeOrganButton.isSelected()) {
                    wholeOrganButton.setSelected(false);
                    photoBleachedButton.setSelected(true);
                    componentImage.getVOIHandler().newVOI(0.0f); // red
                    //componentImage.setCursorMode(ViewJComponentEditImage.NEW_VOI);
                    //componentImage.getVOIHandler().setPresetHue(0.0f); // red
                }
            }
        } // else if (source == wholeOrganCheckBox)

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
     * Accessor to set firstSliceNum.
     *
     * @param  firstSliceNumber  number of wholeOrgan slice
     */
    public void setfirstSliceNum(int firstSliceNumber) {
        firstSliceNum = firstSliceNumber;
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
        componentImage.getVOIHandler().setPresetHue(-1.0f);
        cancelFlag = true;
        dispose();
    }

    /**
     * DOCUMENT ME!
     */
    protected void callAlgorithm() {

        try {

            componentImage.getVOIHandler().setPresetHue(-1.0f);

            // Make algorithm
            frapAlgo = new AlgorithmFRAP(image, useRed, useGreen, useBlue, firstSliceNum, photoBleachedIndex,
                                         wholeOrganIndex, backgroundIndex, model, register, cost, createRegImage,
                                         paramVary, radius, diffusion);

            // This is very important. Adding this object as a listener allows the algorithm to
            // notify this object when it has completed of failed. See algorithm performed event.
            // This is made possible by implementing AlgorithmedPerformed interface
            frapAlgo.addListener(this);

            createProgressBar(image.getImageName(), frapAlgo);

            // Hide dialog
            setVisible(false);

            if (isRunInSeparateThread()) {

                // Start the thread as a low priority because we wish to still have user interface work fast.
                if (frapAlgo.startMethod(Thread.MIN_PRIORITY) == false) {
                    MipavUtil.displayError("A thread is already running on this object");
                }
            } else {
                frapAlgo.run();
            }
        } catch (OutOfMemoryError x) {

            System.gc();
            MipavUtil.displayError("Dialog FRAP: unable to allocate enough memory");

            return;
        }
    }


    /**
     * Initializes GUI components and displays dialog.
     */
    @SuppressWarnings("unchecked")
    private void init() {
        boolean haveRed = false;
        boolean haveGreen = false;
        boolean haveBlue = false;
        int yButton = 1;
        boolean buttonSet = true;
        JPanel imagePanel;
        JPanel VOIPanel;
        JPanel colorPanel;
        int i;

        setForeground(Color.black);
        setTitle("Fluorescence Recovery after Photobleaching");

        VOIPanel = new JPanel(new GridBagLayout());
        VOIPanel.setBorder(buildTitledBorder("Select VOIs"));

        GridBagConstraints gbc4 = new GridBagConstraints();
        gbc4.gridwidth = 1;
        gbc4.gridheight = 1;
        gbc4.anchor = GridBagConstraints.WEST;
        gbc4.weightx = 1;
        gbc4.insets = new Insets(3, 3, 3, 3);
        gbc4.fill = GridBagConstraints.HORIZONTAL;
        gbc4.gridx = 0;
        gbc4.gridy = 0;

        VOIGroup = new ButtonGroup();

        photoBleachedButton = new JRadioButton("Add required photobleached VOI", true);
        photoBleachedButton.setForeground(Color.red);
        photoBleachedButton.setFont(serif12);
        photoBleachedButton.addActionListener(this);
        VOIGroup.add(photoBleachedButton);
        VOIPanel.add(photoBleachedButton, gbc4);
        componentImage.getVOIHandler().newVOI(0.0f); // red
        //componentImage.setCursorMode(ViewJComponentEditImage.NEW_VOI);
        //componentImage.getVOIHandler().setPresetHue(0.0f); // red

        wholeOrganButton = new JRadioButton("Add required whole organ VOI", false);
        wholeOrganButton.setForeground(Color.green.darker());
        wholeOrganButton.setFont(serif12);
        wholeOrganButton.addActionListener(this);
        VOIGroup.add(wholeOrganButton);
        gbc4.gridy = 1;
        VOIPanel.add(wholeOrganButton, gbc4);

        backgroundButton = new JRadioButton("Add optional background VOI", false);
        backgroundButton.setForeground(Color.blue);
        backgroundButton.setFont(serif12);
        backgroundButton.addActionListener(this);
        VOIGroup.add(backgroundButton);
        gbc4.gridy = 2;
        VOIPanel.add(backgroundButton, gbc4);

        colorPanel = new JPanel(new GridBagLayout());

        if (image.isColorImage()) {
            minR = image.getMinR();
            maxR = image.getMaxR();

            if (minR != maxR) {
                haveRed = true;
            }

            minG = image.getMinG();
            maxG = image.getMaxG();

            if (minG != maxG) {
                haveGreen = true;
            }

            minB = image.getMinB();
            maxB = image.getMaxB();

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
                    useRed = true;
                    labelImage = new JLabel("Color red used");
                } else if (haveGreen) {
                    useGreen = true;
                    labelImage = new JLabel("Color green used");
                } else {
                    useBlue = true;
                    labelImage = new JLabel("Color blue used");
                }

                labelImage.setForeground(Color.black);
                labelImage.setFont(serif12);

                colorPanel.setBorder(buildTitledBorder("Color used"));
                colorPanel.add(labelImage);
            } // else if (colorsPresent == 1)
            else if (colorsPresent >= 2) {
                labelImage = new JLabel("Select 1 of the colors");
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

                colorPanel.setBorder(buildTitledBorder("Color selection"));
                colorPanel.add(labelImage, gbc2);
                colorGroup = new ButtonGroup();

                if (haveRed) {
                    gbc2.gridy = yButton++;
                    redButton = new JRadioButton("Red", buttonSet);
                    buttonSet = false;
                    redButton.setFont(serif12);
                    redButton.setForeground(Color.black);
                    colorGroup.add(redButton);
                    colorPanel.add(redButton, gbc2);
                } // if (haveRed)

                if (haveGreen) {
                    gbc2.gridy = yButton++;
                    greenButton = new JRadioButton("Green", buttonSet);
                    buttonSet = false;
                    greenButton.setFont(serif12);
                    greenButton.setForeground(Color.black);
                    colorGroup.add(greenButton);
                    colorPanel.add(greenButton, gbc2);
                } // if (haveGreen)

                if (haveBlue) {
                    gbc2.gridy = yButton++;
                    blueButton = new JRadioButton("Blue", buttonSet);
                    blueButton.setFont(serif12);
                    blueButton.setForeground(Color.black);
                    colorGroup.add(blueButton);
                    colorPanel.add(blueButton, gbc2);
                } // if (haveBlue)


            } // else if (colorsPresent >= 2)

        } // if (image.isColorImage())

        imagePanel = new JPanel(new GridBagLayout());

        GridBagConstraints gbc3 = new GridBagConstraints();
        gbc3.gridwidth = 1;
        gbc3.gridheight = 1;
        gbc3.anchor = GridBagConstraints.WEST;
        gbc3.weightx = 1;
        gbc3.insets = new Insets(3, 3, 3, 3);
        gbc3.fill = GridBagConstraints.HORIZONTAL;
        gbc3.gridx = 0;
        gbc3.gridy = 0;
        imagePanel.add(VOIPanel, gbc3);

        if (image.isColorImage()) {
            gbc3.gridy = 1;
            imagePanel.add(colorPanel, gbc3);
        }

        try { // In this case, the file must be LSM
            fileInfo = (FileInfoLSM) image.getFileInfo(0);

            if (fileInfo.getFirstSliceAfterBleach() >= 0) {
                firstSliceAfterBleach = fileInfo.getFirstSliceAfterBleach();
            }

            if (fileInfo.getBleachedROIShape() >= 0) {
                bleachedROIShape = fileInfo.getBleachedROIShape();
            }

            if (fileInfo.getKnotX() != null) {
                knotX = fileInfo.getKnotX();
            }
        } catch (ClassCastException e) { // If it isn't, catch the exception
        }

        try { // In this case, the file must be XML

            int knotXLength = 0;
            double[] knotXTemp = new double[200];
            fileInfoImageXML = (FileInfoImageXML) image.getFileInfo(0);

            /** go through the hashtable of parameter sets */
            Enumeration<String> setEnum = fileInfoImageXML.getPSetKeys();

            while (setEnum.hasMoreElements()) {
                String temp = setEnum.nextElement();
                Enumeration<String> paramEnum = fileInfoImageXML.getPSet(temp).getParameterKeys();

                while (paramEnum.hasMoreElements()) {
                    String paramName = paramEnum.nextElement();

                    if (paramName.equals("firstSliceAfterBleach")) {
                        String firstSliceAfterBleachStr = fileInfoImageXML.getPSet(temp).getParameter(paramName).getValue();
                        firstSliceAfterBleach = Integer.valueOf(firstSliceAfterBleachStr).intValue();
                    } else if (paramName.equals("bleachedROIShape")) {
                        String bleachedROIShapeStr = fileInfoImageXML.getPSet(temp).getParameter(paramName).getValue();
                        bleachedROIShape = Integer.valueOf(bleachedROIShapeStr).intValue();
                    } else if ((paramName.length() > 7) && (paramName.substring(0, 6).equals("knotX["))) {
                        int endIndex = paramName.indexOf(']');
                        String arrayIndexString = paramName.substring(6, endIndex);
                        int arrayIndex = Integer.valueOf(arrayIndexString).intValue();
                        String knotXString = fileInfoImageXML.getPSet(temp).getParameter(paramName).getValue();
                        knotXTemp[arrayIndex] = Double.valueOf(knotXString).doubleValue();
                        knotXLength++;
                    }
                } // while (paramEnum.hasMoreElements())
            } // while (setEnum.hasMoreElements())

            if (knotXLength > 0) {
                knotX = new double[knotXLength];

                for (i = 0; i < knotX.length; i++) {
                    knotX[i] = knotXTemp[i];
                }
            } // if (knotXLength > 0)
        } catch (ClassCastException e) { // If it isn't, catch the exception
        }

        GridBagConstraints gbc = new GridBagConstraints();
        gbc.gridwidth = 1;
        gbc.gridheight = 1;
        gbc.anchor = GridBagConstraints.WEST;
        gbc.weightx = 1;
        gbc.insets = new Insets(3, 3, 3, 3);
        gbc.fill = GridBagConstraints.HORIZONTAL;
        xPos = 0;
        yPos = 0;
        gbc.gridx = 0;
        gbc.gridy = 0;

        JPanel slicePanel = new JPanel(new GridBagLayout());
        slicePanel.setForeground(Color.black);
        slicePanel.setBorder(buildTitledBorder("Slice parameters"));

        if (firstSliceAfterBleach >= 0) {
            labelFirstSlice = new JLabel("First slice after bleach = " + String.valueOf(firstSliceAfterBleach + 1));
            labelFirstSlice.setForeground(Color.black);
            labelFirstSlice.setFont(serif12);
            labelFirstSlice.setAlignmentX(Component.LEFT_ALIGNMENT);
            slicePanel.add(labelFirstSlice, gbc);
            xPos = 1;
            yPos = 1;
        } // if (firstSliceAfterBleach >= 0)

        if (bleachedROIShape >= 0) {

            switch (bleachedROIShape) {

                case RECTANGLE:
                    labelShape = new JLabel("Bleached region is a rectangle");
                    break;

                case ELLIPSE:
                    labelShape = new JLabel("Bleached region is an ellipse");
                    break;

                case CLOSED_POLYLINE:
                    if (knotX == null) {
                        labelShape = new JLabel("Bleached region is a closed polyline");
                    } else {
                        labelShape = new JLabel("Bleached region is a closed polyline " + " with " + knotX.length +
                                                " verticies");
                    }

                    break;

                case CLOSED_BEZIER:
                    if (knotX == null) {
                        labelShape = new JLabel("Bleached region is a closed bezier " + "spline curve");
                    } else {
                        labelShape = new JLabel("Bleached region is a closed bezier " + "spline curve with " +
                                                knotX.length + " knots");
                    }

                    break;

                case CIRCLE:
                    labelShape = new JLabel("Bleached region is a circle");
                    break;

                default:
                    labelShape = new JLabel("Bleached region is an unknown shape");
            } // switch (bleachedROIShape)

            labelShape.setForeground(Color.black);
            labelShape.setFont(serif12);
            labelShape.setAlignmentX(Component.LEFT_ALIGNMENT);
            gbc.gridx = xPos;
            gbc.gridy = 0;
            slicePanel.add(labelShape, gbc);
            yPos = 1;
        } // if (bleachedROIShape >= 0)

        labelRef = new JLabel("First slice after photobleaching (1-" + String.valueOf(image.getExtents()[2]) + ")");
        labelRef.setForeground(Color.black);
        labelRef.setFont(serif12);
        labelRef.setAlignmentX(Component.LEFT_ALIGNMENT);
        gbc.gridx = 0;
        gbc.gridy = yPos;
        slicePanel.add(labelRef, gbc);

        if (firstSliceAfterBleach >= 0) {
            firstSliceNumText = new JTextField(String.valueOf(firstSliceAfterBleach + 1), 3);
        } else {
            firstSliceNumText = new JTextField(String.valueOf(4), 3);
        }

        gbc.gridx = 1;
        gbc.gridy = yPos;
        slicePanel.add(firstSliceNumText, gbc);

        JPanel analysisPanel = new JPanel(new GridBagLayout());
        analysisPanel.setForeground(Color.black);
        analysisPanel.setBorder(buildTitledBorder("Photobleached model assumed in analysis"));

        GridBagConstraints gbc5 = new GridBagConstraints();

        gbc5.gridwidth = 1;
        gbc5.gridheight = 1;
        gbc5.anchor = GridBagConstraints.WEST;
        gbc5.weightx = 1;
        gbc5.insets = new Insets(3, 3, 3, 3);
        gbc5.fill = GridBagConstraints.HORIZONTAL;
        gbc5.gridx = 0;
        gbc5.gridy = 0;

        analysisGroup = new ButtonGroup();

        if ((bleachedROIShape == CIRCLE) || (bleachedROIShape == -1)) {
            circleButton = new JRadioButton("2D Circle", true);
            model = CIRCLE_2D;
        } else {
            circleButton = new JRadioButton("2D Circle", false);
            model = NARROW_BAND_2D;
        }

        circleButton.setForeground(Color.black);
        circleButton.setFont(serif12);
        circleButton.addActionListener(this);
        analysisGroup.add(circleButton);
        analysisPanel.add(circleButton, gbc5);


        if ((bleachedROIShape != CIRCLE) && (bleachedROIShape != -1)) {
            bandButton = new JRadioButton("2D Narrow band", true);
        } else {
            bandButton = new JRadioButton("2D Narrow band", false);
        }

        bandButton.setForeground(Color.black);
        bandButton.setFont(serif12);
        bandButton.addActionListener(this);
        analysisGroup.add(bandButton);
        gbc5.gridy = 1;
        analysisPanel.add(bandButton, gbc5);

        oneDButton = new JRadioButton("1D", false);
        oneDButton.setForeground(Color.black);
        oneDButton.setFont(serif12);
        oneDButton.addActionListener(this);
        analysisGroup.add(oneDButton);
        gbc5.gridy = 2;
        analysisPanel.add(oneDButton, gbc5);

        singleExpButton = new JRadioButton("Single Exponential", false);
        singleExpButton.setForeground(Color.black);
        singleExpButton.setFont(serif12);
        singleExpButton.addActionListener(this);
        analysisGroup.add(singleExpButton);
        gbc5.gridy = 3;
        analysisPanel.add(singleExpButton, gbc5);

        wholeOrganCheckBox = new JCheckBox("Whole organ normalization");
        wholeOrganCheckBox.setFont(serif12);
        wholeOrganCheckBox.setForeground(Color.black);

        if (model == CIRCLE_2D) {
            wholeOrganCheckBox.setEnabled(false);
            wholeOrganCheckBox.setSelected(false);
            wholeOrganNormalize = false;
            wholeOrganButton.setEnabled(false);
        } else {
            wholeOrganCheckBox.setEnabled(false);
            wholeOrganCheckBox.setSelected(true);
            wholeOrganNormalize = true;
        }

        wholeOrganCheckBox.addItemListener(this);
        gbc5.gridy = 4;
        analysisPanel.add(wholeOrganCheckBox, gbc5);

        paramVaryCheckBox = new JCheckBox("Grid search with parameter variation");
        paramVaryCheckBox.setFont(serif12);
        paramVaryCheckBox.setForeground(Color.black);
        paramVaryCheckBox.setSelected(false);
        paramVaryCheckBox.addItemListener(this);
        gbc5.gridy = 5;
        analysisPanel.add(paramVaryCheckBox, gbc5);

        JPanel paramPanel = new JPanel(new GridBagLayout());
        paramPanel.setForeground(Color.black);
        paramPanel.setBorder(buildTitledBorder("Input parameters"));

        GridBagConstraints gbc6 = new GridBagConstraints();

        gbc6.gridwidth = 1;
        gbc6.gridheight = 1;
        gbc6.anchor = GridBagConstraints.WEST;
        gbc6.weightx = 1;
        gbc6.insets = new Insets(3, 3, 3, 3);
        gbc6.fill = GridBagConstraints.HORIZONTAL;
        gbc6.gridx = 0;
        gbc6.gridy = 0;

        labelRadius = new JLabel("Radius of bleach spot (um)");
        labelRadius.setForeground(Color.black);
        labelRadius.setFont(serif12);

        if (model == CIRCLE_2D) {
            labelRadius.setEnabled(true);
        } else {
            labelRadius.setEnabled(false);
        }

        paramPanel.add(labelRadius, gbc6);

        textRadius = new JTextField(10);
        textRadius.setText("0.0");
        textRadius.setFont(serif12);

        if (model == CIRCLE_2D) {
            textRadius.setEnabled(true);
        } else {
            textRadius.setEnabled(false);
        }

        gbc6.gridx = 1;
        paramPanel.add(textRadius, gbc6);

        labelDiffusion = new JLabel("Diffusion constant (um*um/sec)");
        labelDiffusion.setForeground(Color.black);
        labelDiffusion.setFont(serif12);

        if (model == CIRCLE_2D) {
            labelDiffusion.setEnabled(true);
        } else {
            labelDiffusion.setEnabled(false);
        }

        gbc6.gridx = 0;
        gbc6.gridy = 1;
        paramPanel.add(labelDiffusion, gbc6);

        textDiffusion = new JTextField(10);
        textDiffusion.setText("0.0");
        textDiffusion.setForeground(Color.black);
        textDiffusion.setFont(serif12);

        if (model == CIRCLE_2D) {
            textDiffusion.setEnabled(true);
        } else {
            textDiffusion.setEnabled(false);
        }

        gbc6.gridx = 1;
        paramPanel.add(textDiffusion, gbc6);

        gbc = new GridBagConstraints();
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

        regCheckBox = new JCheckBox("Registration before FRAP");
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

        createRegCheckBox = new JCheckBox("Create registration image");
        createRegCheckBox.setFont(serif12);
        createRegCheckBox.setForeground(Color.black);
        createRegCheckBox.setSelected(false);
        createRegCheckBox.setEnabled(false);
        createRegCheckBox.addItemListener(this);
        gbc.gridx = 0;
        gbc.gridy = 2;
        registrationPanel.add(createRegCheckBox, gbc);


        JPanel mainPanel = new JPanel(new GridBagLayout());
        gbc.gridx = 0;
        yPos = 0;
        gbc.gridy = yPos++;
        mainPanel.add(slicePanel, gbc);
        gbc.gridy = yPos++;
        mainPanel.add(analysisPanel, gbc);
        gbc.gridy = yPos++;
        mainPanel.add(paramPanel, gbc);
        gbc.gridy = yPos++;
        mainPanel.add(registrationPanel, gbc);

        getContentPane().add(imagePanel, BorderLayout.NORTH);
        getContentPane().add(mainPanel, BorderLayout.CENTER);
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
        int i;
        int nVOIs;
        float[] hsb;
        float hue;
        String tmpStr;
        VOIs = image.getVOIs();
        nVOIs = VOIs.size();
        nBoundingVOIs = 0;
        photoBleachedIndex = -1;
        wholeOrganIndex = -1;
        backgroundIndex = -1;

        if (!testParameter(firstSliceNumText.getText(), 1, image.getExtents()[2])) {
            firstSliceNumText.requestFocus();
            firstSliceNumText.selectAll();

            return false;
        } else {
            firstSliceNum = Integer.valueOf(firstSliceNumText.getText()).intValue() - 1;
        }

        if (bandButton.isSelected()) {
            model = NARROW_BAND_2D;
        } else if (circleButton.isSelected()) {
            model = CIRCLE_2D;
        } else if (oneDButton.isSelected()) {
            model = PURE_1D;
        } else if (singleExpButton.isSelected()) {
            model = SINGLE_EXPONENTIAL;
        }

        wholeOrganNormalize = wholeOrganCheckBox.isSelected();

        paramVary = paramVaryCheckBox.isSelected();

        for (i = 0; i < nVOIs; i++) {

            if ((VOIs.VOIAt(i).getCurveType() == VOI.CONTOUR) || (VOIs.VOIAt(i).getCurveType() == VOI.POLYLINE)) {
                nBoundingVOIs++;
                hsb = Color.RGBtoHSB(VOIs.VOIAt(i).getColor().getRed(), VOIs.VOIAt(i).getColor().getGreen(),
                                     VOIs.VOIAt(i).getColor().getBlue(), null);
                hue = hsb[0];

                if ((Math.abs(hue - 0.0f)) < 0.0001f) {

                    if (photoBleachedIndex == -1) {
                        photoBleachedIndex = i;
                        VOIs.VOIAt(i).setName("Photobleached");
                    } else {
                        MipavUtil.displayError("Cannot have more than 1 photobleached VOI");

                        return false;
                    }
                } else if ((Math.abs(hue - (1.0f / 3.0f))) < 0.0001f) {

                    if (wholeOrganIndex == -1) {
                        wholeOrganIndex = i;
                        VOIs.VOIAt(i).setName("WholeOrgan");
                    } else {
                        MipavUtil.displayError("Cannot have more than 1 whole organ VOI");

                        return false;
                    }
                } else if ((Math.abs(hue - (2.0f / 3.0f))) < 0.0001f) {

                    if (backgroundIndex == -1) {
                        backgroundIndex = i;
                        VOIs.VOIAt(i).setName("Background");
                    } else {
                        MipavUtil.displayError("Cannot have more than 1 background VOI");

                        return false;
                    }
                } else {
                    MipavUtil.displayError("VOI hue = " + hue + " Must be 0 for red, " +
                                           "1/3 for green, or 2/3 for blue");

                    return false;
                }
            }
        } // for (i = 0; i < nVOIs; i++)

        if (photoBleachedIndex == -1) {
            MipavUtil.displayError("Must specify a photobleached VOI");

            return false;
        }

        if (wholeOrganNormalize && (wholeOrganIndex == -1)) {
            MipavUtil.displayError("Must specify a whole organ VOI");

            return false;
        }

        if (model == CIRCLE_2D) {
            tmpStr = textRadius.getText();

            if (testParameter(tmpStr, 0.0, 1.0E6)) {
                radius = Float.valueOf(tmpStr).floatValue();
            } else {
                textRadius.requestFocus();
                textRadius.selectAll();

                return false;
            }

            tmpStr = textDiffusion.getText();

            if (testParameter(tmpStr, 0.0, 1.0E30)) {
                diffusion = Float.valueOf(tmpStr).floatValue();
            } else {
                textDiffusion.requestFocus();
                textDiffusion.selectAll();

                return false;
            }
        } // if (model == CIRCLE_2D)

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

        createRegImage = createRegCheckBox.isSelected();

        if (image.isColorImage()) {

            if (colorsPresent >= 2) {

                if ((redButton != null) && (redButton.isSelected())) {
                    useRed = true;
                } else if ((greenButton != null) && (greenButton.isSelected())) {
                    useGreen = true;
                } else {
                    useBlue = true;
                }
            }


        } // if (image.isColorImage())


        return true;
    }

}
