package gov.nih.mipav.view.dialogs;


import gov.nih.mipav.model.algorithms.*;
import gov.nih.mipav.model.algorithms.utilities.*;
import gov.nih.mipav.model.file.*;
import gov.nih.mipav.model.scripting.*;
import gov.nih.mipav.model.scripting.parameters.*;
import gov.nih.mipav.model.structures.*;

import gov.nih.mipav.view.*;

import java.awt.*;
import java.awt.event.*;

import javax.swing.*;


/**
 * DOCUMENT ME!
 *
 * @version  0.1 Nov 17, 1998
 * @author   Matthew J. McAuliffe, Ph.D.
 */
public class JDialogRGBtoGray extends JDialogScriptableBase implements AlgorithmInterface {

    //~ Static fields/initializers -------------------------------------------------------------------------------------

    /** Use serialVersionUID for interoperability. */
    private static final long serialVersionUID = 3780026821781916357L;

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** DOCUMENT ME! */
    private float blueValue = 1.0f / 3.0f;

    /** DOCUMENT ME! */
    private ButtonGroup destinationGroup;

    /** DOCUMENT ME! */
    private JPanel destinationPanel;

    /** DOCUMENT ME! */
    private int displayLoc; // Flag indicating if a new image is to be generated
                            // or if the source image is to be replaced

    /** DOCUMENT ME! */
    private JRadioButton equalButton;

    /** DOCUMENT ME! */
    private FileInfoBase fInfoBase;

    /** DOCUMENT ME! */
    private JRadioButton graphicsButton;

    /** DOCUMENT ME! */
    private float greenValue = 1.0f / 3.0f;

    /** DOCUMENT ME! */
    private ModelImage imageA = null; // source image

    /** DOCUMENT ME! */
    private boolean intensityAverage;

    /** DOCUMENT ME! */
    private JLabel labelR, labelG, labelB;

    /** DOCUMENT ME! */
    private JRadioButton newImage;

    /** set the values to the defaults -- based on a default of equal values. */
    private float redValue = 1.0f / 3.0f;

    /** DOCUMENT ME! */
    private JRadioButton replaceImage;

    /** DOCUMENT ME! */
    private ModelImage resultImage = null; // result image

    /** DOCUMENT ME! */
    private AlgorithmRGBtoGray RGBAlgo;

    /** DOCUMENT ME! */
    private JTextField textR, textG, textB;

    /** DOCUMENT ME! */
    private JTextField textThreshold;

    /** DOCUMENT ME! */
    private float threshold = 0.0f;

    /** DOCUMENT ME! */
    private boolean thresholdAverage;

    /** DOCUMENT ME! */
    private JCheckBox thresholdCheckBox;

    /** DOCUMENT ME! */
    private JRadioButton userButton;
    
    private JRadioButton equalRangeButton;
    
    private JRadioButton unequalRangeButton;
    
    private JLabel labelMinR, labelMinG, labelMinB;
    
    private JLabel labelMaxR, labelMaxG, labelMaxB;
    
    private JTextField textMinR, textMinG, textMinB;
    
    private JTextField textMaxR, textMaxG, textMaxB;
    
    private boolean equalRange;
    
    private float minR, maxR, minG, maxG, minB, maxB;


    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Empty constructor needed for dynamic instantiation (used during scripting).
     */
    public JDialogRGBtoGray() { }

    /**
     * Sets variables needed to call algorithm.
     *
     * @param  theParentFrame  Parent frame
     * @param  imA             Source image
     */
    public JDialogRGBtoGray(Frame theParentFrame, ModelImage imA) {
        super(theParentFrame, false);
        imageA = imA;
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

        if ((source == equalButton) || (source == graphicsButton) || (source == userButton)) {

            if (equalButton.isSelected()) {
                labelR.setEnabled(false);
                textR.setText("0.3333");
                textR.setEnabled(false);
                labelG.setEnabled(false);
                textG.setText("0.3333");
                textG.setEnabled(false);
                labelB.setEnabled(false);
                textB.setText("0.3333");
                textB.setEnabled(false);
                if (equalRangeButton.isSelected()) {
                    thresholdCheckBox.setEnabled(true);
                    textThreshold.setEnabled(true);
                }
                else {
                    thresholdCheckBox.setEnabled(false);
                    thresholdCheckBox.setSelected(false);
                    textThreshold.setEnabled(false);    
                }
            } else if (graphicsButton.isSelected()) {
                labelR.setEnabled(false);
                textR.setText("0.299");
                textR.setEnabled(false);
                labelG.setEnabled(false);
                textG.setText("0.587");
                textG.setEnabled(false);
                labelB.setEnabled(false);
                textB.setText("0.114");
                textB.setEnabled(false);
                thresholdCheckBox.setEnabled(false);
                thresholdCheckBox.setSelected(false);
                textThreshold.setEnabled(false);
            } else {
                labelR.setEnabled(true);
                textR.setText(" ");
                textR.setEnabled(true);
                labelG.setEnabled(true);
                textG.setText(" ");
                textG.setEnabled(true);
                labelB.setEnabled(true);
                textB.setText(" ");
                textB.setEnabled(true);
                thresholdCheckBox.setEnabled(false);
                thresholdCheckBox.setSelected(false);
                textThreshold.setEnabled(false);
            }
        } else if ((source == equalRangeButton) || (source == unequalRangeButton)) {
            if (equalRangeButton.isSelected()) {
                labelMinR.setEnabled(false);
                textMinR.setEnabled(false);
                labelMaxR.setEnabled(false);
                textMaxR.setEnabled(false);
                labelMinG.setEnabled(false);
                textMinG.setEnabled(false);
                labelMaxG.setEnabled(false);
                textMaxG.setEnabled(false);
                labelMinB.setEnabled(false);
                textMinB.setEnabled(false);
                labelMaxB.setEnabled(false);
                textMaxB.setEnabled(false);
                if (equalButton.isSelected()) {
                    thresholdCheckBox.setEnabled(true);
                    textThreshold.setEnabled(true);
                }
            } else {
                labelMinR.setEnabled(true);
                textMinR.setEnabled(true);
                labelMaxR.setEnabled(true);
                textMaxR.setEnabled(true);
                labelMinG.setEnabled(true);
                textMinG.setEnabled(true);
                labelMaxG.setEnabled(true);
                textMaxG.setEnabled(true);
                labelMinB.setEnabled(true);
                textMinB.setEnabled(true);
                labelMaxB.setEnabled(true);
                textMaxB.setEnabled(true); 
                thresholdCheckBox.setEnabled(false);
                thresholdCheckBox.setSelected(false);
                textThreshold.setEnabled(false);    
            }
        } else if (source == thresholdCheckBox) {

            if (thresholdCheckBox.isSelected()) {
                textThreshold.setEnabled(true);
            } else {
                textThreshold.setEnabled(false);
            }
        } else if (command.equals("OK")) {

            if (setVariables()) {
                callAlgorithm();
            }
        } else if (command.equals("Help")) {
            //MipavUtil.showHelp("U4017");
            MipavUtil.showWebHelp("Converting_image_datasets_to_different_data_types#Converting_RGB_datasets_to_grayscale_datasets");
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
     * This method is required if the AlgorithmPerformed interface is implemented. It is called by the algorithms when
     * it has completed or failed to to complete, so that the dialog can be display the result image and/or clean up.
     *
     * @param  algorithm  Algorithm that caused the event.
     */
    public void algorithmPerformed(AlgorithmBase algorithm) {

        if (algorithm instanceof AlgorithmRGBtoGray) {

            if ((RGBAlgo.isCompleted() == true) && (resultImage != null)) {
                // The algorithm has completed and produced a new image to be displayed.

                try {
                    new ViewJFrameImage(resultImage, null, new Dimension(610, 200));
                } catch (OutOfMemoryError error) {
                    System.gc();
                    MipavUtil.displayError("Out of memory: unable to open new frame");
                }
            } else if ((RGBAlgo.isCompleted() == true) && (resultImage == null)) {

                imageA = RGBAlgo.getSrcImage();

                try {
                    new ViewJFrameImage(imageA, null, new Dimension(610, 200));
                } catch (OutOfMemoryError error) {
                    System.gc();
                    MipavUtil.displayError("Out of memory: unable to open new frame");
                }

            } else if (resultImage != null) {

                // algorithm failed but result image still has garbage
                resultImage.disposeLocal(); // clean up memory
                resultImage = null;
                System.gc();

            }

            // Update frame
            if (parentFrame != null) {
                ((ViewJFrameBase) parentFrame).updateImages(true);
            }

            if (algorithm.isCompleted()) {
                insertScriptLine();
            }

            RGBAlgo.finalize();
            RGBAlgo = null;
        }
    }

    /**
     * Accessor that returns the result image.
     *
     * @return  The result image.
     */
    public ModelImage getResultImage() {
        return resultImage;
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
     * Sets whether intensity averaging is performed to determine which which channels should be used in calculating the
     * gray.
     *
     * @param  intensityAverage  DOCUMENT ME!
     */
    public void setIntensityAverage(boolean intensityAverage) {
        this.intensityAverage = intensityAverage;

        if (intensityAverage) {
            this.thresholdAverage = false;
        }
    }

    /**
     * Sets the red, green, and blue values to equal values. The values represent the proportion of one color to another
     * (i.e. they must add up to 1.0).
     */
    public void setRGBEqual() {
        redValue = greenValue = blueValue = 1.0f / 3.0f;
    }

    /**
     * Sets the red, green, and blue values to computer graphic values. The values represent the proportion of one color
     * to another (i.e. they must add up to 1.0).
     */
    public void setRGBGraphics() {
        redValue = 0.299f;
        greenValue = 0.587f;
        blueValue = 0.114f;
    }

    /**
     * Sets the red, green, and blue values to the given parameters. The values represent the proportion of one color to
     * another (i.e. they must add up to 1.0).
     *
     * @param  rVal  the red proportion
     * @param  gVal  the green proportion
     * @param  bVal  the blue proportion
     */
    public void setRGBValues(float rVal, float gVal, float bVal) {
        redValue = rVal;
        greenValue = gVal;
        blueValue = bVal;
    }

    /**
     * Sets the threshold for threshold average.
     *
     * @param  threshold  DOCUMENT ME!
     */
    public void setThreshold(float threshold) {
        this.threshold = threshold;
    }

    /**
     * Sets whether threshold averaging is performed - Only include those components greater than threshold.
     *
     * @param  thresholdAverage  DOCUMENT ME!
     */
    public void setThresholdAverage(boolean thresholdAverage) {
        this.thresholdAverage = thresholdAverage;

        if (thresholdAverage) {
            this.intensityAverage = false;
        }
    }
    
    public void setEqualRange(boolean equalRange) {
        this.equalRange = equalRange;
    }
    
    public void setMinR(float minR) {
        this.minR = minR;
    }
    
    public void setMaxR(float maxR) {
        this.maxR = maxR;
    }
    
    public void setMinG(float minG) {
        this.minG = minG;
    }
    
    public void setMaxG(float maxG) {
        this.maxG = maxG;
    }
    
    public void setMinB(float minB) {
        this.minB = minB;
    }
    
    public void setMaxB(float maxB) {
        this.maxB = maxB;
    }

    /**
     * Calls the algorithm.
     */
    protected void callAlgorithm() {

        if (displayLoc == NEW) {

            try {

                if (imageA.getType() == ModelStorageBase.ARGB) {
                    resultImage = new ModelImage(ModelImage.UBYTE, imageA.getExtents(),
                                                 (imageA.getImageName() + "Gray"));
                } else if (imageA.getType() == ModelStorageBase.ARGB_USHORT) {
                    resultImage = new ModelImage(ModelImage.USHORT, imageA.getExtents(),
                                                 (imageA.getImageName() + "Gray"));
                } else if (imageA.getType() == ModelStorageBase.ARGB_FLOAT) {
                    resultImage = new ModelImage(ModelImage.FLOAT, imageA.getExtents(),
                                                 (imageA.getImageName() + "Gray"));
                }

                // get some important information from imageA and put it in
                // the result image
                if ((imageA.getFileInfo()[0]).getFileFormat() == FileUtility.DICOM)  {
                	FileInfoDicom fileInfoBuffer; // buffer of type DICOM
                	for (int n = 0; n < imageA.getFileInfo().length; n++) {
                		fileInfoBuffer = (FileInfoDicom) imageA.getFileInfo(n).clone(); // copy into buffer
                		fileInfoBuffer.getTagTable().setValue("0028,0002", new Short((short) 1), 2); // samples per pixel
                		fileInfoBuffer.getTagTable().setValue("0028,0004", new String("MONOCHROME2"), 11); // photometric
                		fileInfoBuffer.setDataType(resultImage.getType());
                		resultImage.setFileInfo(fileInfoBuffer, n);
                	}
                }
                else {
	                for (int n = 0; n < imageA.getFileInfo().length; n++) {
	                    fInfoBase = (FileInfoBase) (imageA.getFileInfo(n).clone());
	                    fInfoBase.setDataType(resultImage.getType());
	                    resultImage.setFileInfo(fInfoBase, n);
	                }
                }

                // Make algorithm
                RGBAlgo = new AlgorithmRGBtoGray(resultImage, imageA, redValue, greenValue, blueValue, thresholdAverage,
                                                 threshold, intensityAverage, equalRange, minR, maxR, minG, maxG,
                                                 minB, maxB);

                // This is very important. Adding this object as a listener allows the algorithm to
                // notify this object when it has completed of failed. See algorithm performed event.
                // This is made possible by implementing AlgorithmedPerformed interface
                RGBAlgo.addListener(this);

                createProgressBar(imageA.getImageName(), RGBAlgo);

                // Hide dialog
                setVisible(false);

                if (isRunInSeparateThread()) {

                    // Start the thread as a low priority because we wish to still have user interface work fast.
                    if (RGBAlgo.startMethod(Thread.MIN_PRIORITY) == false) {
                        MipavUtil.displayError("A thread is already running on this object");
                    }
                } else {
                    RGBAlgo.run();
                }
            } catch (OutOfMemoryError x) {

                if (resultImage != null) {
                    resultImage.disposeLocal(); // Clean up memory of result image
                    resultImage = null;
                }

                System.gc();
                MipavUtil.displayError("Dialog RGB to Gray: unable to allocate enough memory");

                return;
            }
        } // if (displayLoc == NEW)
        else { // displayLoc == REPLACE

            try {

                // No need to make new image space because the user has choosen to replace the source image
                // Make the algorithm class
                // Make algorithm
                RGBAlgo = new AlgorithmRGBtoGray(imageA, redValue, greenValue, blueValue, thresholdAverage, threshold,
                                                 intensityAverage, equalRange, minR, maxR, minG, maxG,
                                                 minB, maxB);

                // This is very important. Adding this object as a listener allows the algorithm to
                // notify this object when it has completed of failed. See algorithm performed event.
                // This is made possible by implementing AlgorithmedPerformed interface
                RGBAlgo.addListener(this);

                // Hide the dialog since the algorithm is about to run.
                setVisible(false);

                // These next lines set the titles in all frames where the source image is displayed to
                // "locked - " image name so as to indicate that the image is now read/write locked!
                // The image frames are disabled and then unregisted from the userinterface until the
                // algorithm has completed.
                /*Vector imageFrames = imageA.getImageFrameVector();
                 *
                 * titles = new String[imageFrames.size()]; for ( int i = 0; i < imageFrames.size(); i++ ) { titles[i] = (
                 * (Frame) ( imageFrames.elementAt( i ) ) ).getTitle(); ( (Frame) ( imageFrames.elementAt( i ) )
                 * ).setTitle( "Locked: " + titles[i] ); ( (Frame) ( imageFrames.elementAt( i ) ) ).setEnabled( false );
                 * userInterface.unregisterFrame( (Frame) ( imageFrames.elementAt( i ) ) );}*/

                if (isRunInSeparateThread()) {

                    // Start the thread as a low priority because we wish to still have user interface work fast.
                    if (RGBAlgo.startMethod(Thread.MIN_PRIORITY) == false) {
                        MipavUtil.displayError("A thread is already running on this object");
                    }
                } else {
                    RGBAlgo.run();
                }

            } catch (OutOfMemoryError x) {
                System.gc();
                MipavUtil.displayError("Dialog RGBtoGRAY: unable to allocate enough memory");

                return;
            }
        } // displayLoc == REPLACE
    }

    /**
     * Store the result image in the script runner's image table now that the action execution is finished.
     */
    protected void doPostAlgorithmActions() {

        if (displayLoc == NEW) {
            AlgorithmParameters.storeImageInRunner(getResultImage());
        }
    }

    /**
     * {@inheritDoc}
     */
    protected void setGUIFromParams() {
        imageA = scriptParameters.retrieveInputImage();
        parentFrame = imageA.getParentFrame();

        if (scriptParameters.doOutputNewImage()) {
            setDisplayLocNew();
        } else {
            setDisplayLocReplace();
        }

        float[] rgb = scriptParameters.getParams().getList("rgb_values").getAsFloatArray();
        setRGBValues(rgb[0], rgb[1], rgb[2]);
        setThresholdAverage(scriptParameters.getParams().getBoolean("do_threshold_average"));
        setThreshold(scriptParameters.getParams().getFloat("threshold"));
        setIntensityAverage(scriptParameters.getParams().getBoolean("do_intensity_average"));
        setEqualRange(scriptParameters.getParams().getBoolean("equal_range"));
        setMinR(scriptParameters.getParams().getFloat("min_r"));
        setMaxR(scriptParameters.getParams().getFloat("max_r"));
        setMinG(scriptParameters.getParams().getFloat("min_g"));
        setMaxG(scriptParameters.getParams().getFloat("max_g"));
        setMinB(scriptParameters.getParams().getFloat("min_b"));
        setMaxB(scriptParameters.getParams().getFloat("max_b"));
    }

    /**
     * {@inheritDoc}
     */
    protected void storeParamsFromGUI() throws ParserException {
        scriptParameters.storeInputImage(imageA);
        scriptParameters.storeOutputImageParams(getResultImage(), (displayLoc == NEW));

        scriptParameters.getParams().put(ParameterFactory.newParameter("rgb_values",
                                                                       new float[] { redValue, greenValue, blueValue }));
        scriptParameters.getParams().put(ParameterFactory.newParameter("do_threshold_average", thresholdAverage));
        scriptParameters.getParams().put(ParameterFactory.newParameter("threshold", threshold));
        scriptParameters.getParams().put(ParameterFactory.newParameter("do_intensity_average", intensityAverage));
        scriptParameters.getParams().put(ParameterFactory.newParameter("equal_range", equalRange));
        scriptParameters.getParams().put(ParameterFactory.newParameter("min_r", minR));
        scriptParameters.getParams().put(ParameterFactory.newParameter("max_r", maxR));
        scriptParameters.getParams().put(ParameterFactory.newParameter("min_g", minG));
        scriptParameters.getParams().put(ParameterFactory.newParameter("max_g", maxG));
        scriptParameters.getParams().put(ParameterFactory.newParameter("min_b", minB));
        scriptParameters.getParams().put(ParameterFactory.newParameter("max_B", maxB));
    }

    /**
     * Sets up the GUI (panels, buttons, etc) and displays it on the screen.
     */
    private void init() {
        setForeground(Color.black);

        setTitle("RGB -> Gray");
        getContentPane().setLayout(new BorderLayout());

        JPanel mainPanel;
        mainPanel = new JPanel();
        mainPanel.setBorder(BorderFactory.createEmptyBorder(3, 3, 3, 3));
        mainPanel.setLayout(new GridBagLayout());

        GridBagConstraints gbc = new GridBagConstraints();
        gbc.gridwidth = 1;
        gbc.gridheight = 1;
        gbc.anchor = GridBagConstraints.WEST;
        gbc.weightx = 1;
        gbc.insets = new Insets(3, 3, 3, 3);
        gbc.gridx = 0;
        gbc.gridy = 0;
        gbc.fill = GridBagConstraints.HORIZONTAL;

        JPanel optionsPanel = new JPanel(new GridLayout(3, 1));
        optionsPanel.setForeground(Color.black);
        optionsPanel.setBorder(buildTitledBorder("Weighting methods"));
        mainPanel.add(optionsPanel, gbc);

        ButtonGroup weightGroup = new ButtonGroup();
        equalButton = new JRadioButton("Equal weights", true); // default
        equalButton.setFont(serif12);
        equalButton.addActionListener(this);
        weightGroup.add(equalButton);
        optionsPanel.add(equalButton, BorderLayout.NORTH);

        graphicsButton = new JRadioButton("Computer graphics", false);
        graphicsButton.setFont(serif12);
        graphicsButton.addActionListener(this);
        weightGroup.add(graphicsButton);
        optionsPanel.add(graphicsButton, BorderLayout.CENTER);

        userButton = new JRadioButton("User specified", false);
        userButton.setFont(serif12);
        userButton.addActionListener(this);
        weightGroup.add(userButton);
        optionsPanel.add(userButton, BorderLayout.SOUTH);

        gbc.gridx = 0;
        gbc.gridy = 1;

        JPanel RGBPanel = new JPanel(new GridLayout(4, 2));
        RGBPanel.setForeground(Color.black);
        RGBPanel.setBorder(buildTitledBorder("RGB weight factors"));
        mainPanel.add(RGBPanel, gbc);

        labelR = new JLabel("Red");
        labelR.setForeground(Color.black);
        labelR.setFont(serif12);
        labelR.setEnabled(false);
        RGBPanel.add(labelR);
        textR = new JTextField();
        textR.setText("0.3333");
        textR.setFont(serif12);
        textR.setEnabled(false);
        RGBPanel.add(textR);

        labelG = new JLabel("Green");
        labelG.setForeground(Color.black);
        labelG.setFont(serif12);
        labelG.setEnabled(false);
        RGBPanel.add(labelG);
        textG = new JTextField();
        textG.setText("0.3333");
        textG.setFont(serif12);
        textG.setEnabled(false);
        RGBPanel.add(textG);

        labelB = new JLabel("Blue");
        labelB.setForeground(Color.black);
        labelB.setFont(serif12);
        labelB.setEnabled(false);
        RGBPanel.add(labelB);
        textB = new JTextField();
        textB.setText("0.3333");
        textB.setFont(serif12);
        textB.setEnabled(false);
        RGBPanel.add(textB);

        thresholdCheckBox = new JCheckBox("Only average RGB values greater than");
        thresholdCheckBox.setFont(serif12);
        thresholdCheckBox.setForeground(Color.black);
        thresholdCheckBox.setEnabled(true);
        thresholdCheckBox.setSelected(false);
        thresholdCheckBox.addActionListener(this);
        RGBPanel.add(thresholdCheckBox);

        textThreshold = new JTextField();
        textThreshold.setText("0");
        textThreshold.setFont(serif12);
        textThreshold.setEnabled(false);
        RGBPanel.add(textThreshold);
        
        gbc.gridx = 0;
        gbc.gridy = 2;

        JPanel rangePanel = new JPanel(new GridBagLayout());
        rangePanel.setForeground(Color.black);
        rangePanel.setBorder(buildTitledBorder("Color ranges"));
        mainPanel.add(rangePanel, gbc);
        
        GridBagConstraints gbc2 = new GridBagConstraints();
        gbc2.gridwidth = 1;
        gbc2.gridheight = 1;
        gbc2.anchor = GridBagConstraints.WEST;
        gbc2.weightx = 1;
        gbc2.insets = new Insets(3, 3, 3, 3);
        gbc2.gridx = 0;
        gbc2.gridy = 0;
        gbc2.fill = GridBagConstraints.HORIZONTAL;
        
        ButtonGroup rangeGroup = new ButtonGroup();
        equalRangeButton = new JRadioButton("Equal ranges", true); // default
        equalRangeButton.setFont(serif12);
        equalRangeButton.addActionListener(this);
        rangeGroup.add(equalRangeButton);
        rangePanel.add(equalRangeButton, gbc2);

        unequalRangeButton = new JRadioButton("Unequal ranges", false);
        unequalRangeButton.setFont(serif12);
        unequalRangeButton.addActionListener(this);
        rangeGroup.add(unequalRangeButton);
        gbc2.gridy = 1;
        rangePanel.add(unequalRangeButton, gbc2);
        
        labelMinR = new JLabel("Red minimum");
        labelMinR.setForeground(Color.black);
        labelMinR.setFont(serif12);
        labelMinR.setEnabled(false);
        gbc2.gridy = 2;
        rangePanel.add(labelMinR, gbc2);
        textMinR = new JTextField();
        textMinR.setText(String.valueOf(imageA.getMinR()));
        textMinR.setFont(serif12);
        textMinR.setEnabled(false);
        gbc2.gridx = 1;
        rangePanel.add(textMinR, gbc2);
        
        labelMaxR = new JLabel("Red maximum");
        labelMaxR.setForeground(Color.black);
        labelMaxR.setFont(serif12);
        labelMaxR.setEnabled(false);
        gbc2.gridx = 0;
        gbc2.gridy = 3;
        rangePanel.add(labelMaxR, gbc2);
        textMaxR = new JTextField();
        textMaxR.setText(String.valueOf(imageA.getMaxR()));
        textMaxR.setFont(serif12);
        textMaxR.setEnabled(false);
        gbc2.gridx = 1;
        rangePanel.add(textMaxR, gbc2);
        
        labelMinG = new JLabel("Green minimum");
        labelMinG.setForeground(Color.black);
        labelMinG.setFont(serif12);
        labelMinG.setEnabled(false);
        gbc2.gridx = 0;
        gbc2.gridy = 4;
        rangePanel.add(labelMinG, gbc2);
        textMinG = new JTextField();
        textMinG.setText(String.valueOf(imageA.getMinG()));
        textMinG.setFont(serif12);
        textMinG.setEnabled(false);
        gbc2.gridx = 1;
        rangePanel.add(textMinG, gbc2);
        
        labelMaxG = new JLabel("Green maximum");
        labelMaxG.setForeground(Color.black);
        labelMaxG.setFont(serif12);
        labelMaxG.setEnabled(false);
        gbc2.gridx = 0;
        gbc2.gridy = 5;
        rangePanel.add(labelMaxG, gbc2);
        textMaxG = new JTextField();
        textMaxG.setText(String.valueOf(imageA.getMaxG()));
        textMaxG.setFont(serif12);
        textMaxG.setEnabled(false);
        gbc2.gridx = 1;
        rangePanel.add(textMaxG, gbc2);

        labelMinB = new JLabel("Blue minimum");
        labelMinB.setForeground(Color.black);
        labelMinB.setFont(serif12);
        labelMinB.setEnabled(false);
        gbc2.gridx = 0;
        gbc2.gridy = 6;
        rangePanel.add(labelMinB, gbc2);
        textMinB = new JTextField();
        textMinB.setText(String.valueOf(imageA.getMinB()));
        textMinB.setFont(serif12);
        textMinB.setEnabled(false);
        gbc2.gridx = 1;
        rangePanel.add(textMinB, gbc2);
        
        labelMaxB = new JLabel("Blue maximum");
        labelMaxB.setForeground(Color.black);
        labelMaxB.setFont(serif12);
        labelMaxB.setEnabled(false);
        gbc2.gridx = 0;
        gbc2.gridy = 7;
        rangePanel.add(labelMaxB, gbc2);
        textMaxB = new JTextField();
        textMaxB.setText(String.valueOf(imageA.getMaxB()));
        textMaxB.setFont(serif12);
        textMaxB.setEnabled(false);
        gbc2.gridx = 1;
        rangePanel.add(textMaxB, gbc2);

        gbc.gridx = 0;
        gbc.gridy = 3;
        destinationPanel = new JPanel(new BorderLayout());
        destinationPanel.setForeground(Color.black);
        destinationPanel.setBorder(buildTitledBorder("Destination"));
        mainPanel.add(destinationPanel, gbc);

        destinationGroup = new ButtonGroup();
        newImage = new JRadioButton("New image", true);
        newImage.setFont(serif12);
        destinationGroup.add(newImage);
        destinationPanel.add(newImage, BorderLayout.NORTH);

        replaceImage = new JRadioButton("Replace image", false);
        replaceImage.setFont(serif12);
        destinationGroup.add(replaceImage);
        destinationPanel.add(replaceImage, BorderLayout.CENTER);

        // Only if the image is unlocked can it be replaced.
        if (imageA.getLockStatus() == ModelStorageBase.UNLOCKED) {
            replaceImage.setEnabled(true);
        } else {
            replaceImage.setEnabled(false);
        }


        JPanel buttonPanel = new JPanel();

        /*
         * buildOKButton(); buttonPanel.add(OKButton); buildCancelButton(); buttonPanel.add(cancelButton);
         */
        buttonPanel.add(buildButtons());

        getContentPane().add(mainPanel, BorderLayout.CENTER);
        getContentPane().add(buttonPanel, BorderLayout.SOUTH);
        pack();
        setResizable(true);
        setVisible(true);

        System.gc();
    }


    /**
     * Use the GUI results to set up the variables needed to run the algorithm.
     *
     * @return  <code>true</code> if parameters set successfully, <code>false</code> otherwise.
     */
    private boolean setVariables() {
        String tmpStr;
        float minValue, maxValue;

        if (replaceImage.isSelected()) {
            displayLoc = REPLACE;
        } else if (newImage.isSelected()) {
            displayLoc = NEW;
        }
        
        equalRange = equalRangeButton.isSelected();
        
        if (!equalRange) {
            tmpStr = textMinR.getText();

            if (testParameter(tmpStr, -Float.MAX_VALUE, Float.MAX_VALUE)) {
                minR = Float.valueOf(tmpStr).floatValue();
            } else {
                textMinR.requestFocus();
                textMinR.selectAll();

                return false;
            }
            
            tmpStr = textMaxR.getText();

            if (testParameter(tmpStr, minR, Float.MAX_VALUE)) {
                maxR = Float.valueOf(tmpStr).floatValue();
            } else {
                textMaxR.requestFocus();
                textMaxR.selectAll();

                return false;
            }

            tmpStr = textMinG.getText();

            if (testParameter(tmpStr, -Float.MAX_VALUE, Float.MAX_VALUE)) {
                minG = Float.valueOf(tmpStr).floatValue();
            } else {
                textMinG.requestFocus();
                textMinG.selectAll();

                return false;
            }
            
            tmpStr = textMaxG.getText();

            if (testParameter(tmpStr, minG, Float.MAX_VALUE)) {
                maxG = Float.valueOf(tmpStr).floatValue();
            } else {
                textMaxG.requestFocus();
                textMaxG.selectAll();

                return false;
            }

            tmpStr = textMinB.getText();

            if (testParameter(tmpStr, -Float.MAX_VALUE, Float.MAX_VALUE)) {
                minB = Float.valueOf(tmpStr).floatValue();
            } else {
                textMinB.requestFocus();
                textMinB.selectAll();

                return false;
            } 
            
            tmpStr = textMaxB.getText();

            if (testParameter(tmpStr, minB, Float.MAX_VALUE)) {
                maxB = Float.valueOf(tmpStr).floatValue();
            } else {
                textMaxB.requestFocus();
                textMaxB.selectAll();

                return false;
            }    
        } // if (!equalRange)

        if (equalButton.isSelected()) {
            redValue = greenValue = blueValue = 1.0f / 3.0f;
            thresholdAverage = thresholdCheckBox.isSelected();

            if (thresholdAverage) {
                tmpStr = textThreshold.getText();

                if (imageA.getType() == ModelImage.ARGB) {
                    minValue = 0.0f;
                    maxValue = 255.0f;
                } else if (imageA.getType() == ModelImage.ARGB_USHORT) {
                    minValue = 0.0f;
                    maxValue = 65535.0f;
                } else {
                    minValue = -Float.MAX_VALUE;
                    maxValue = Float.MAX_VALUE;
                }

                if (testParameter(tmpStr, minValue, maxValue)) {
                    threshold = Float.valueOf(tmpStr).floatValue();
                } else {
                    textThreshold.requestFocus();
                    textThreshold.selectAll();

                    return false;
                }
            } // if (thresholdAverage)

            return true;
        } // if (equalButton.isSelected())

        if (graphicsButton.isSelected()) {
            redValue = 0.299f;
            greenValue = 0.587f;
            blueValue = 0.114f;

            return true;
        }

        tmpStr = textR.getText();

        if (testParameter(tmpStr, -Float.MAX_VALUE, Float.MAX_VALUE)) {
            redValue = Float.valueOf(tmpStr).floatValue();
        } else {
            textR.requestFocus();
            textR.selectAll();

            return false;
        }

        tmpStr = textG.getText();

        if (testParameter(tmpStr, -Float.MAX_VALUE, Float.MAX_VALUE)) {
            greenValue = Float.valueOf(tmpStr).floatValue();
        } else {
            textG.requestFocus();
            textG.selectAll();

            return false;
        }

        tmpStr = textB.getText();

        if (testParameter(tmpStr, -Float.MAX_VALUE, Float.MAX_VALUE)) {
            blueValue = Float.valueOf(tmpStr).floatValue();
        } else {
            textB.requestFocus();
            textB.selectAll();

            return false;
        }

        return true;
    }
}
