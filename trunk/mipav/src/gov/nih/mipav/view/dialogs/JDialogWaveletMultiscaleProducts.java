package gov.nih.mipav.view.dialogs;


import gov.nih.mipav.model.algorithms.*;
import gov.nih.mipav.model.algorithms.filters.*;
import gov.nih.mipav.model.scripting.*;
import gov.nih.mipav.model.scripting.parameters.*;
import gov.nih.mipav.model.structures.*;

import gov.nih.mipav.view.*;

import java.awt.*;
import java.awt.event.*;

import java.util.*;

import javax.swing.*;


/**
 * DOCUMENT ME!
 */
public class JDialogWaveletMultiscaleProducts extends JDialogScriptableBase implements AlgorithmInterface, ActionDiscovery, ScriptableActionInterface {

    //~ Static fields/initializers -------------------------------------------------------------------------------------

    /** Use serialVersionUID for interoperability. */
    //private static final long serialVersionUID;

    public static final int MINIMUM_PHASE = 1;
    
    public static final int MID_PHASE = 2;
    
    public static final int MAXIMUM_PHASE = 3;

    /** thresholding estimator. */
    public static final int HARD = 1;

    /** DOCUMENT ME! */
    public static final int SOFT = 2;

    /** DOCUMENT ME! */
    public static final int NONNEGATIVE_GARROTE = 3;

    /** DOCUMENT ME! */
    public static final int SCAD = 4;

    //~ Instance fields ------------------------------------------------------------------------------------------------

    private int filterLength;
    
    private int filterType;

    /** DOCUMENT ME! */
    private JTextField textFilterLength;

    /** DOCUMENT ME! */
    private int displayLoc; // Flag indicating if a new image is to be generated

    /** DOCUMENT ME! */
    private boolean doWaveletImages;

    /** DOCUMENT ME! */
    private JRadioButton garrote;

    /** DOCUMENT ME! */
    private JRadioButton hard;

    /** DOCUMENT ME! */
    private ModelImage image; // source image

    /** DOCUMENT ME! */
    private JRadioButton newImage;

    /** DOCUMENT ME! */
    private JRadioButton replaceImage;

    /** DOCUMENT ME! */
    private ModelImage resultImage = null; // result image

    /** DOCUMENT ME! */
    private JRadioButton scad;

    /** DOCUMENT ME! */
    private JRadioButton soft;

    /** DOCUMENT ME! */
    private JTextField textThreshold;

    /** DOCUMENT ME! */
    private float threshold;

    /** DOCUMENT ME! */
    private int thresholdType; // SOFT or HARD

    /** DOCUMENT ME! */
    private String[] titles;


    /** DOCUMENT ME! */
    private ViewUserInterface userInterface;

    /** DOCUMENT ME! */
    private AlgorithmRiceWaveletTools waveletAlgo;

    /** DOCUMENT ME! */
    private JCheckBox waveletCheckBox;

    /** DOCUMENT ME! */
    private ModelImage[] waveletImage = null;
    
    private JRadioButton minimum;
    
    private JRadioButton mid;
    
    private JRadioButton maximum;
    
    // numberOfLevels == Integer.MAX_VALUE means that the maximum possible number of levels
    // is calculated and used.
    private int numberOfLevels = Integer.MAX_VALUE;
    
    private JRadioButton maximumLevelsButton;
    
    private JRadioButton userLevelsButton;
    
    private JTextField textLevels;
    
    private int xDim;
    
    private int yDim;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Empty constructor needed for dynamic instantiation (used during scripting).
     */
    public JDialogWaveletMultiscaleProducts() { }

    /**
     * Creates new dialog for entering parameters for wavelet multiscale products.
     *
     * @param  theParentFrame  Parent frame
     * @param  im              Source image
     */
    public JDialogWaveletMultiscaleProducts(Frame theParentFrame, ModelImage im) {
        super(theParentFrame, false);
        image = im;
        xDim = image.getExtents()[0];
        if ((xDim % 2) == 1) {
            MipavUtil.displayError("1 level of decomposition requires an even xDim");
            return;
        }
        yDim = image.getExtents()[1];
        if ((yDim % 2) == 1) {
            MipavUtil.displayError("1 level of decomposition requires an even yDim");
            return;
        }
        userInterface = ViewUserInterface.getReference();
        init();
    }

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * actionPerformed - Closes dialog box when the OK button is pressed and calls the algorithm.
     *
     * @param  event  event that triggers function
     */
    public void actionPerformed(ActionEvent event) {
        String command = event.getActionCommand();
        Object source = event.getSource();

        if (command.equals("OK")) {

            if (setVariables()) {
                callAlgorithm();
            }
        } else if (command.equals("Help")) {
            //MipavUtil.showHelp("");
        } else if (command.equals("Cancel")) {
            dispose();
        } else if ((source == maximumLevelsButton) || (source == userLevelsButton)) {
            if (maximumLevelsButton.isSelected()) {
                textLevels.setEnabled(false);
            }
            else {
                textLevels.setEnabled(true);
            }
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
        int i;

        if (algorithm instanceof AlgorithmRiceWaveletTools) {
            image.clearMask();

            if ((waveletAlgo.isCompleted() == true) && (resultImage != null)) {
                // The algorithm has completed and produced a new image to be displayed.

                updateFileInfo(image, resultImage);

                try {

                    // resultImage.setImageName("Unsharp mask");
                    new ViewJFrameImage(resultImage, null, new Dimension(610, 200));
                } catch (OutOfMemoryError error) {
                    MipavUtil.displayError("Out of memory: unable to open new frame");
                }

                if (doWaveletImages) {
                    waveletImage = waveletAlgo.getWaveletImages();

                    if (waveletImage != null) {
                        for (i = 0; i < waveletImage.length; i++) {
                            
                            if (waveletImage[i] != null) {

                                // waveletImage is same size as original image
                                updateFileInfo(image, waveletImage[i]);
        
                                try {
                                    new ViewJFrameImage(waveletImage[i], null, new Dimension(610, (220 + i * 20)));
                                } catch (OutOfMemoryError error) {
                                    MipavUtil.displayError("Out of memory: Unable to open wavelet image frame");
                                }
                            } // if (waveletImage[i] != null)
                            else {
                                MipavUtil.displayError("waveletImage[" + i + "] is null");
                            }
                        } // for (i = 0; i < waveletImage.length; i++)
                    } else {
                        MipavUtil.displayError("waveletImage array is null");
                    }
                } // if (doWaveletImages)*/
            } else if (resultImage == null) {

                // These next lines set the titles in all frames where the source image is displayed to
                // image name so as to indicate that the image is now unlocked!
                // The image frames are enabled and then registed to the userinterface.
                Vector<ViewImageUpdateInterface> imageFrames = image.getImageFrameVector();

                for (i = 0; i < imageFrames.size(); i++) {
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
                
                if (doWaveletImages) {
                    waveletImage = waveletAlgo.getWaveletImages();

                    if (waveletImage != null) {
                        for (i = 0; i < waveletImage.length; i++) {
                            
                            if (waveletImage[i] != null) {

                                // waveletImage is same size as original image
                                updateFileInfo(image, waveletImage[i]);
        
                                try {
                                    new ViewJFrameImage(waveletImage[i], null, new Dimension(610, (200 + i * 20)));
                                } catch (OutOfMemoryError error) {
                                    MipavUtil.displayError("Out of memory: Unable to open wavelet image frame");
                                }
                            } // if (waveletImage[i] != null)
                            else {
                                MipavUtil.displayError("waveletImage[" + i + "] is null");
                            }
                        } // for (i = 0; i < waveletImage.length; i++)
                    } else {
                        MipavUtil.displayError("waveletImage array is null");
                    }
                } // if (doWaveletImages)*/
                
            } else if (resultImage != null) {

                // algorithm failed but result image still has garbage
                resultImage.disposeLocal(); // clean up memory
                resultImage = null;
            }
        }
        // Update frame
        // ((ViewJFrameBase)parentFrame).updateImages(true);

        if (algorithm.isCompleted()) {
            insertScriptLine();
        }
     // save the completion status for later
        setComplete(algorithm.isCompleted());

        waveletAlgo.finalize();
        waveletAlgo = null;
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

    // *******************************************************************
    // ************************* Item Events ****************************
    // *******************************************************************

    /**
     * Method to handle item events.
     *
     * @param  event  event that cause the method to fire
     */
    public void itemStateChanged(ItemEvent event) {
        // Object source = event.getSource();
        // float tempNum;

    }

    /**
     * Accessor that sets the filter length.
     *
     * @param  filterLength  DOCUMENT ME!
     */
    public void setFilterLength(int filterLength) {
        this.filterLength = filterLength;
    }
    
    public void setNumberOfLevels(int numberOfLevels) {
        this.numberOfLevels = numberOfLevels;
    }
    
    /**
     * Accessor that sets the filter type to minimum phase, mid phase, or maximum phase
     * @param filterType
     */
    public void setFilterType(int filterType) {
        this.filterType = filterType;
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
     * Accessor that sets whether or not the wavelet images are displayed.
     *
     * @param  doWaveletImage  DOCUMENT ME!
     */
    public void setDoWaveletImage(boolean doWaveletImages) {
        this.doWaveletImages = doWaveletImages;
    }

    /**
     * Accessor that sets the threshold.
     *
     * @param  threshold  DOCUMENT ME!
     */
    public void setThreshold(float threshold) {
        this.threshold = threshold;
    }

    /**
     * Accessor that sets whether soft or hard thresholding is used.
     *
     * @param  thresholdType  DOCUMENT ME!
     */
    public void setThresholdType(int thresholdType) {
        this.thresholdType = thresholdType;
    }

    /**
     * Once all the necessary variables are set, call the UnsharpMark algorithm based on what type of image this is and
     * whether or not there is a separate destination image.
     */
    protected void callAlgorithm() {
        String name = makeImageName(image.getImageName(), "_waveletThr");

        if (image.getNDims() == 2) { // source image is 2D

            int[] destExtents = new int[2];
            destExtents[0] = image.getExtents()[0]; // X dim
            destExtents[1] = image.getExtents()[1]; // Y dim

            if (displayLoc == NEW) {

                try {

                    // Make result image of float type
                    // resultImage = new ModelImage(ModelImage.FLOAT, destExtents, name, userInterface);
                    resultImage = (ModelImage) image.clone();
                    resultImage.setImageName(name);
                    resultImage.resetVOIs();

                    /*if ((resultImage.getFileInfo()[0]).getFileFormat() == FileUtility.DICOM){
                     *  ((FileInfoDicom)(resultImage.getFileInfo(0))).setSecondaryCaptureTags();
                     *      } */
                    // Make algorithm
                    waveletAlgo = new AlgorithmRiceWaveletTools(resultImage, image, filterLength, filterType,
                                      numberOfLevels, doWaveletImages);

                    // This is very important. Adding this object as a listener allows the algorithm to
                    // notify this object when it has completed of failed. See algorithm performed event.
                    // This is made possible by implementing AlgorithmedPerformed interface
                    waveletAlgo.addListener(this);

                    createProgressBar(image.getImageName(), waveletAlgo);

                    // Hide dialog
                    setVisible(false);

                    if (isRunInSeparateThread()) {

                        // Start the thread as a low priority because we wish to still have user interface work fast.
                        if (waveletAlgo.startMethod(Thread.MIN_PRIORITY) == false) {
                            MipavUtil.displayError("A thread is already running on this object");
                        }
                    } else {
                        waveletAlgo.run();
                    }
                } catch (OutOfMemoryError x) {
                    MipavUtil.displayError("Dialog WaveletThreshold: unable to allocate enough memory");

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
                    waveletAlgo = new AlgorithmRiceWaveletTools(image, filterLength, filterType, numberOfLevels,
                            doWaveletImages);

                    // This is very important. Adding this object as a listener allows the algorithm to
                    // notify this object when it has completed of failed. See algorithm performed event.
                    // This is made possible by implementing AlgorithmedPerformed interface
                    waveletAlgo.addListener(this);

                    createProgressBar(image.getImageName(), waveletAlgo);

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

                        // Start the thread as a low priority because we wish to still have user interface.
                        if (waveletAlgo.startMethod(Thread.MIN_PRIORITY) == false) {
                            MipavUtil.displayError("A thread is already running on this object");
                        }
                    } else {
                        waveletAlgo.run();
                    }
                } catch (OutOfMemoryError x) {
                    MipavUtil.displayError("Dialog WaveletThreshold: unable to allocate enough memory");

                    return;
                }
            }
        } else if (image.getNDims() == 3) {
            int[] destExtents = new int[3];
            destExtents[0] = image.getExtents()[0];
            destExtents[1] = image.getExtents()[1];
            destExtents[2] = image.getExtents()[2];

            if (displayLoc == NEW) {

                try {

                    // Make result image of float type
                    // resultImage  = new ModelImage(ModelImage.FLOAT, destExtents, name, userInterface);
                    resultImage = (ModelImage) image.clone();
                    resultImage.setImageName(name);
                    resultImage.resetVOIs();

                    /* if ((resultImage.getFileInfo()[0]).getFileFormat() == FileUtility.DICOM){
                     * for (int i=0; i < resultImage.getExtents()[2]; i++) {
                     * ((FileInfoDicom)(resultImage.getFileInfo(i))).setSecondaryCaptureTags(); }}*/
                    // Make algorithm
                    waveletAlgo = new AlgorithmRiceWaveletTools(resultImage, image, filterLength, filterType, numberOfLevels,
                            doWaveletImages);

                    // This is very important. Adding this object as a listener allows the algorithm to
                    // notify this object when it has completed of failed. See algorithm performed event.
                    // This is made possible by implementing AlgorithmedPerformed interface
                    waveletAlgo.addListener(this);

                    createProgressBar(image.getImageName(), waveletAlgo);

                    // Hide dialog
                    setVisible(false);

                    if (isRunInSeparateThread()) {

                        // Start the thread as a low priority because we wish to still have user interface work fast
                        if (waveletAlgo.startMethod(Thread.MIN_PRIORITY) == false) {
                            MipavUtil.displayError("A thread is already running on this object");
                        }
                    } else {
                        waveletAlgo.run();
                    }
                } catch (OutOfMemoryError x) {
                    MipavUtil.displayError("Dialog WaveletThreshold: unable to allocate enough memory");

                    if (resultImage != null) {
                        resultImage.disposeLocal(); // Clean up image memory
                        resultImage = null;
                    }

                    return;
                }
            } else {

                try {

                    // Make algorithm
                    waveletAlgo = new AlgorithmRiceWaveletTools(image, filterLength, filterType, numberOfLevels, doWaveletImages);

                    // This is very important. Adding this object as a listener allows the algorithm to
                    // notify this object when it has completed of failed. See algorithm performed event.
                    // This is made possible by implementing AlgorithmedPerformed interface
                    waveletAlgo.addListener(this);

                    createProgressBar(image.getImageName(), waveletAlgo);

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

                    // Start the thread as a low priority because we wish to still have user interface work fast
                    if (isRunInSeparateThread()) {

                        if (waveletAlgo.startMethod(Thread.MIN_PRIORITY) == false) {
                            MipavUtil.displayError("A thread is already running on this object");
                        }
                    } else {
                        waveletAlgo.run();
                    }
                } catch (OutOfMemoryError x) {
                    MipavUtil.displayError("Dialog WaveletThreshold: unable to allocate enough memory");

                    return;
                }
            }
        }
    }

    /**
     * {@inheritDoc}
     */
    protected void doPostAlgorithmActions() {

        if (displayLoc == NEW) {
            AlgorithmParameters.storeImageInRunner(resultImage);
        }
    }

    /**
     * {@inheritDoc}
     */
    protected void setGUIFromParams() {
        image = scriptParameters.retrieveInputImage();

        if (scriptParameters.doOutputNewImage()) {
            this.setDisplayLocNew();
        } else {
            this.setDisplayLocReplace();
        }

        userInterface = ViewUserInterface.getReference();
        parentFrame = image.getParentFrame();

        filterLength = scriptParameters.getParams().getInt("filter_length");
        numberOfLevels = scriptParameters.getParams().getInt("number_of_levels");
        filterType = scriptParameters.getParams().getInt("filter_type");
        thresholdType = scriptParameters.getParams().getInt("threshold_type");
        threshold = scriptParameters.getParams().getFloat("threshold");
        doWaveletImages = scriptParameters.getParams().getBoolean("do_show_wavelet_images");

    }

    /**
     * {@inheritDoc}
     */
    protected void storeParamsFromGUI() throws ParserException {
        scriptParameters.storeInputImage(image);
        scriptParameters.storeOutputImageParams(resultImage, (displayLoc == NEW));

        scriptParameters.getParams().put(ParameterFactory.newParameter("filter_length", filterLength));
        scriptParameters.getParams().put(ParameterFactory.newParameter("number_of_levels", numberOfLevels));
        scriptParameters.getParams().put(ParameterFactory.newParameter("Filter_type", filterType));
        scriptParameters.getParams().put(ParameterFactory.newParameter("threshold_type", thresholdType));
        scriptParameters.getParams().put(ParameterFactory.newParameter("threshold", threshold));
        scriptParameters.getParams().put(ParameterFactory.newParameter("do_show_wavelet_images", doWaveletImages));
    }

    /**
     * Sets up the GUI (panels, buttons, etc) and displays it on the screen.
     */
    private void init() {
        setTitle("Wavelet Multiscale Products");

        GridBagConstraints gbc = new GridBagConstraints();
        gbc.weightx = 1;
        gbc.anchor = GridBagConstraints.WEST;
        gbc.fill = GridBagConstraints.HORIZONTAL;

        JPanel paramPanel = new JPanel(new GridBagLayout());
        paramPanel.setBorder(buildTitledBorder("Parameters"));

        JLabel coeffLabel = new JLabel("Filter length (must be even)");
        coeffLabel.setForeground(Color.black);
        coeffLabel.setFont(serif12);
        paramPanel.add(coeffLabel, gbc);

        textFilterLength = new JTextField(10);
        textFilterLength.setFont(serif12);
        textFilterLength.setText("4");
        gbc.gridx = 1;
        paramPanel.add(textFilterLength, gbc);
        
        JLabel levelsLabel = new JLabel("Number of levels:");
        levelsLabel.setForeground(Color.black);
        levelsLabel.setFont(serif12);
        gbc.gridx = 0;
        gbc.gridy = 1;
        paramPanel.add(levelsLabel, gbc);
        
        ButtonGroup levelsGroup = new ButtonGroup();
        maximumLevelsButton = new JRadioButton("Maximum possible", true);
        maximumLevelsButton.setFont(serif12);
        maximumLevelsButton.addActionListener(this);
        levelsGroup.add(maximumLevelsButton);
        gbc.gridx = 0;
        gbc.gridy = 2;
        paramPanel.add(maximumLevelsButton);
        
        userLevelsButton = new JRadioButton("User specified", false);
        userLevelsButton.setFont(serif12);
        userLevelsButton.addActionListener(this);
        levelsGroup.add(userLevelsButton);
        gbc.gridx = 0;
        gbc.gridy = 3;
        paramPanel.add(userLevelsButton);
        
        textLevels = new JTextField(10);
        textLevels.setFont(serif12);
        textLevels.setText("2");
        textLevels.setEnabled(false);
        gbc.gridx = 1;
        paramPanel.add(textLevels, gbc);

        JLabel labelThreshold = new JLabel("Wavelet threshold");
        labelThreshold.setForeground(Color.black);
        labelThreshold.setFont(serif12);
        gbc.gridx = 0;
        gbc.gridy = 4;
        paramPanel.add(labelThreshold, gbc);

        textThreshold = new JTextField(10);
        textThreshold.setText("1.0E-4");
        textThreshold.setFont(serif12);
        gbc.gridx = 1;
        paramPanel.add(textThreshold, gbc);
        
        ButtonGroup phaseGroup = new ButtonGroup();
        minimum = new JRadioButton("Minimum phase", true);
        minimum.setFont(serif12);
        phaseGroup.add(minimum);
        gbc.gridx = 0;
        gbc.gridy = 5;
        paramPanel.add(minimum, gbc);
        
        mid = new JRadioButton("Mid phase", false);
        mid.setFont(serif12);
        phaseGroup.add(mid);
        gbc.gridx = 0;
        gbc.gridy = 6;
        paramPanel.add(mid, gbc);
        
        maximum = new JRadioButton("Maximum phase", false);
        maximum.setFont(serif12);
        phaseGroup.add(maximum);
        gbc.gridx = 0;
        gbc.gridy = 7;
        paramPanel.add(maximum, gbc);

        ButtonGroup thresholdGroup = new ButtonGroup();
        hard = new JRadioButton("Hard", false);
        hard.setFont(serif12);
        hard.addActionListener(this);
        thresholdGroup.add(hard);
        gbc.gridx = 0;
        gbc.gridy = 8;
        paramPanel.add(hard, gbc);

        soft = new JRadioButton("Soft", true);
        soft.setFont(serif12);
        soft.addActionListener(this);
        thresholdGroup.add(soft);
        gbc.gridx = 0;
        gbc.gridy = 9;
        paramPanel.add(soft, gbc);

        garrote = new JRadioButton("Nonnegative garrote", false);
        garrote.setFont(serif12);
        garrote.addActionListener(this);
        thresholdGroup.add(garrote);
        gbc.gridx = 0;
        gbc.gridy = 10;
        paramPanel.add(garrote, gbc);

        scad = new JRadioButton("SCAD", false);
        scad.setFont(serif12);
        scad.addActionListener(this);
        thresholdGroup.add(scad);
        gbc.gridx = 0;
        gbc.gridy = 11;
        paramPanel.add(scad, gbc);

        waveletCheckBox = new JCheckBox("Display log magnitude wavelet images");
        waveletCheckBox.setFont(serif12);
        waveletCheckBox.setSelected(false);
        gbc.gridx = 0;
        gbc.gridy = 12;
        gbc.gridwidth = 2;
        paramPanel.add(waveletCheckBox, gbc);

        JPanel destinationPanel = new JPanel(new GridLayout(2, 1));
        destinationPanel.setForeground(Color.black);
        destinationPanel.setBorder(buildTitledBorder("Destination"));

        ButtonGroup destinationGroup = new ButtonGroup();
        newImage = new JRadioButton("New image", true);
        newImage.setFont(serif12);
        destinationGroup.add(newImage);
        destinationPanel.add(newImage);

        replaceImage = new JRadioButton("Replace image", false);
        replaceImage.setFont(serif12);
        destinationGroup.add(replaceImage);
        destinationPanel.add(replaceImage);

        if (image.getLockStatus() == ModelStorageBase.UNLOCKED) {
            replaceImage.setEnabled(true);
        } else {
            replaceImage.setEnabled(false);
        }

        JPanel mainPanel = new JPanel(new GridBagLayout());
        gbc.gridx = 0;
        gbc.gridy = 0;
        gbc.gridwidth = 1;
        mainPanel.add(paramPanel, gbc);
        gbc.gridy = 1;
        mainPanel.add(destinationPanel, gbc);
        mainPanel.setBorder(BorderFactory.createEmptyBorder(5, 5, 5, 5));

        JPanel buttonPanel = new JPanel();
        buildOKButton();
        buttonPanel.add(OKButton);
        buildCancelButton();
        buttonPanel.add(cancelButton);
        buildHelpButton();
        buttonPanel.add(helpButton);

        getContentPane().add(mainPanel);
        getContentPane().add(buttonPanel, BorderLayout.SOUTH);

        pack();
        //setResizable(false);
        setVisible(true);
    }

    /**
     * Use the GUI results to set up the variables needed to run the algorithm.
     *
     * @return  <code>true</code> if parameters set successfully, <code>false</code> otherwise.
     */
    private boolean setVariables() {

        System.gc();

        String tmpStr;
        int divisor;
        int i, j, k;

        if (replaceImage.isSelected()) {
            displayLoc = REPLACE;
        } else if (newImage.isSelected()) {
            displayLoc = NEW;
        }

        tmpStr = textFilterLength.getText();
        
        if (testParameter(tmpStr, 2, 100)) {
            filterLength = Integer.valueOf(tmpStr).intValue();
        }
        else {
            textFilterLength.requestFocus();
            textFilterLength.selectAll();
            
            return false;
        }
        
        if ((filterLength % 2) == 1) {
            MipavUtil.displayError("Filter length must be even");
            return false;
        }
        
        if (maximumLevelsButton.isSelected()) {
            i = xDim;
            j = 0;
            while ((i % 2) == 0) {
                i = (i >> 1);
                j++;
            }
            k = yDim;
            i = 0;
            while((k % 2) == 0) {
                k = (k >> 1);
                i++;
            }
            
            numberOfLevels = Math.min(i, j);
            Preferences.debug("The maximum possible number of levels = " + numberOfLevels + "\n", Preferences.DEBUG_FILEIO);
        }
        else {
            tmpStr = textLevels.getText();
            if (testParameter(tmpStr, 1, 20)) {
                numberOfLevels = Integer.valueOf(tmpStr).intValue();
            } else {
                textLevels.requestFocus();
                textLevels.selectAll();
                
                return false;
            }
            divisor = 1;
            for (i = 1; i <= numberOfLevels; i++) {
                divisor *= 2;
            }
            if ((xDim % divisor) != 0) {
                MipavUtil.displayError("Error!  xDim mod " + divisor + " does not equal 0");
                textLevels.requestFocus();
                textLevels.selectAll();
                
                return false;
            }
            
            if ((yDim % divisor) != 0) {
                MipavUtil.displayError("Error!  yDim mod " + divisor + " does not equal 0");
                textLevels.requestFocus();
                textLevels.selectAll();
                
                return false;
            }
        }
        
        if (minimum.isSelected()) {
            filterType = MINIMUM_PHASE;
        } else if (mid.isSelected()) {
            filterType = MID_PHASE;
        } else {
            filterType = MAXIMUM_PHASE;
        }

        if (hard.isSelected()) {
            thresholdType = HARD;
        } else if (soft.isSelected()) {
            thresholdType = SOFT;
        } else if (garrote.isSelected()) {
            thresholdType = NONNEGATIVE_GARROTE;
        } else {
            thresholdType = SCAD;
        }

        tmpStr = textThreshold.getText();

        if (testParameter(tmpStr, 0.0, 1.0)) {
            threshold = Float.valueOf(tmpStr).floatValue();
        } else {
            textThreshold.requestFocus();
            textThreshold.selectAll();

            return false;
        }

        if (waveletCheckBox.isSelected()) {
            doWaveletImages = true;
        } else {
            doWaveletImages = false;
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
                return new String("Algorithms.Filters (wavelet)");
            }

            public String getDescription() {
                return new String("Applies a wavelet threshold.");
            }

            public String getDescriptionLong() {
                return new String("Applies a wavelet threshold.");
            }

            public String getShortLabel() {
                return new String("WaveletThreshold");
            }

            public String getLabel() {
                return new String("Wavelet Threshold");
            }

            public String getName() {
                return new String("Wavelet Threshold");
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
            table.put(new ParameterInt("filter_Length", 4));
            table.put(new ParameterInt("number_of_levels", Integer.MAX_VALUE));
            table.put(new ParameterInt("filter_type", MINIMUM_PHASE));
            table.put(new ParameterInt("threshold_type", 2));
            table.put(new ParameterFloat("threshold", .0001f));
            table.put(new ParameterBoolean("do_show_wavelet_images", false));
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
