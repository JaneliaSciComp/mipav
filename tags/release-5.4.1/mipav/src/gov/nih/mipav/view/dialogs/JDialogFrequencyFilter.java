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
 * Dialog to get user input, then call the algorithm. The user has the option to generate a new image or replace the
 * source image. It should be noted that the algorithms are executed in their own threads.
 *
 * @version  1.0 Aug 24, 1999
 * @author   Matthew J. McAuliffe, Ph.D.
 * @see      AlgorithmFrequencyFilter
 */
public class JDialogFrequencyFilter extends JDialogScriptableBase implements AlgorithmInterface, ItemListener, ActionDiscovery, ScriptableActionInterface {

    //~ Static fields/initializers -------------------------------------------------------------------------------------

    /** Use serialVersionUID for interoperability. */
    private static final long serialVersionUID = 4979580288393418858L;

    /** DOCUMENT ME! */
    public static final int LOWPASS = 1;

    /** DOCUMENT ME! */
    public static final int HIGHPASS = 2;

    /** DOCUMENT ME! */
    public static final int BANDPASS = 3;

    /** DOCUMENT ME! */
    public static final int BANDSTOP = 4;

    /** DOCUMENT ME! */
    public static final int WINDOW = 1;

    /** DOCUMENT ME! */
    public static final int GAUSSIAN = 2;

    /** DOCUMENT ME! */
    public static final int BUTTERWORTH = 3;

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** DOCUMENT ME! */
    private JRadioButton bandPass;

    /** DOCUMENT ME! */
    private JRadioButton bandStop;

    /** DOCUMENT ME! */
    private JRadioButton butterworthFilter;

    /** DOCUMENT ME! */
    private int butterworthOrder;

    /** DOCUMENT ME! */
    private ButtonGroup constructionGroup;

    /** DOCUMENT ME! */
    private int constructionMethod;

    /** DOCUMENT ME! */
    private JPanel constructionPanel;

    /** DOCUMENT ME! */
    private ButtonGroup destinationGroup;

    /** DOCUMENT ME! */
    private JPanel destinationPanel;

    /** DOCUMENT ME! */
    private int displayLoc; // Flag indicating if a new image is to be generated

    /** DOCUMENT ME! */
    private JPanel filterPanel;

    /** DOCUMENT ME! */
    private int filterType;

    /** DOCUMENT ME! */
    private ButtonGroup filterTypeGroup;

    /** DOCUMENT ME! */
    private float freq1;

    /** DOCUMENT ME! */
    private float freq2;

    /** DOCUMENT ME! */
    private AlgorithmFrequencyFilter FrequencyFilterAlgo = null;
    
    private AlgorithmFrequencyFilterColor FrequencyFilterColorAlgo = null;

    /** DOCUMENT ME! */
    private JRadioButton gaussianFilter;

    /** DOCUMENT ME! */
    private JRadioButton highPass;

    /** DOCUMENT ME! */
    private ModelImage image; // source image

    /** DOCUMENT ME! */
    private boolean image25D = false;

    /** DOCUMENT ME! */
    private JCheckBox image25DCheckbox;

    /** DOCUMENT ME! */
    private boolean imageCrop;

    /** DOCUMENT ME! */
    private JCheckBox imageCropCheckbox;

    /** DOCUMENT ME! */
    private int kernelDiameter;

    /** DOCUMENT ME! */
    private JLabel labelF1;

    /** DOCUMENT ME! */
    private JLabel labelF2;

    /** DOCUMENT ME! */
    private JLabel labelKernelDiameter;

    /** DOCUMENT ME! */
    private JLabel labelOrder;

    /** DOCUMENT ME! */
    private JRadioButton lowPass;

    /** DOCUMENT ME! */
    private JPanel mainPanel;

    /** DOCUMENT ME! */
    private JRadioButton newImage;

    /** DOCUMENT ME! */
    private JRadioButton replaceImage;

    /** DOCUMENT ME! */
    private ModelImage resultImage = null; // result image

    /** DOCUMENT ME! */
    private JTextField textF1;

    /** DOCUMENT ME! */
    private JTextField textF2;

    /** DOCUMENT ME! */
    private JTextField textKernelDiameter;

    /** DOCUMENT ME! */
    private JTextField textOrder;

    /** DOCUMENT ME! */
    private String[] titles;

    /** DOCUMENT ME! */
    private ViewUserInterface userInterface;

    /** DOCUMENT ME! */
    private JRadioButton windowFilter;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Empty constructor needed for dynamic instantiation (used during scripting).
     */
    public JDialogFrequencyFilter() { }

    /**
     * Used primarily for the script to store variables and run the algorithm. No actual dialog will appear but the set
     * up info and result image will be stored here.
     *
     * @param  im  Source image.
     */
    public JDialogFrequencyFilter(ModelImage im) {
        super();
        userInterface = ViewUserInterface.getReference();
        image = im;
        parentFrame = image.getParentFrame();
    }

    // or if the source image is to be replaced
    /**
     * Creates a new JDialogFrequencyFilter object.
     *
     * @param  theParentFrame  Parent frame.
     * @param  im              Source image.
     */
    public JDialogFrequencyFilter(Frame theParentFrame, ModelImage im) {
        super(theParentFrame, false);
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
            	if (!image.isColorImage()) {
                    callAlgorithm();
            	}
            	else {
            		callColorAlgorithm();
            	}
            }
        } else if (source == windowFilter) {
            imageCropCheckbox.setEnabled(true);
            textKernelDiameter.setEnabled(true);
            labelKernelDiameter.setEnabled(true);
            labelF1.setText("Frequency F1 0.0 to 1.0 ");
            textOrder.setEnabled(false);
            labelOrder.setEnabled(false);
            bandPass.setEnabled(true);
            bandStop.setEnabled(true);
        } else if (source == gaussianFilter) {
            imageCropCheckbox.setEnabled(false);
            imageCropCheckbox.setSelected(false);
            textKernelDiameter.setEnabled(false);
            labelKernelDiameter.setEnabled(false);
            labelF1.setText("Frequency F1 exceeds 0.0 ");
            textOrder.setEnabled(false);
            labelOrder.setEnabled(false);
            textF2.setEnabled(false);
            labelF2.setEnabled(false);
            bandPass.setEnabled(false);
            if (bandPass.isSelected()) {
                bandPass.setSelected(false);
                highPass.setSelected(true);
            }
            bandStop.setEnabled(false);
            if (bandStop.isSelected()) {
                bandStop.setSelected(false);
                highPass.setSelected(true);
            }
        } else if (source == butterworthFilter) {
            imageCropCheckbox.setEnabled(false);
            imageCropCheckbox.setSelected(false);
            textKernelDiameter.setEnabled(false);
            labelKernelDiameter.setEnabled(false);
            labelF1.setText("Frequency F1 >0.0 to 1.0 ");
            textOrder.setEnabled(true);
            labelOrder.setEnabled(true);
            bandPass.setEnabled(true);
            bandStop.setEnabled(true);
        } else if ((source == lowPass) || (source == highPass)) {
            textF2.setEnabled(false);
            labelF2.setEnabled(false);
        } else if ((source == bandPass) || (source == bandStop)) {
            textF2.setEnabled(true);
            labelF2.setEnabled(true);
        } else if (command.equals("Cancel")) {
            dispose();
        } else if (source == helpButton) {
             MipavUtil.showHelp("FilFr010");
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

        if (algorithm instanceof AlgorithmFrequencyFilter) {

            if ((algorithm.isCompleted() == true) && (resultImage != null)) {

            	updateFileTypeInfo(image, resultImage, ModelStorageBase.FLOAT);

                // resultImage is the same or smaller than image.
                // The algorithm has completed and produced a new image to be displayed.
                try {

                    // resultImage.setImageName("Frequency Filtered image");
                    new ViewJFrameImage(resultImage, null, new Dimension(610, 200));
                } catch (OutOfMemoryError error) {
                    System.gc();
                    MipavUtil.displayError("Out of memory: unable to open new frame");
                }
            } else if (resultImage == null) {

                // These next lines set the titles in all frames where the source image is displayed to
                // image name so as to indicate that the image is now unlocked!
                // The image frames are enabled and then registed to the userinterface.
                Vector<ViewImageUpdateInterface> imageFrames = image.getImageFrameVector();

                for (int i = 0; i < imageFrames.size(); i++) {
                    ((ViewJFrameBase) (imageFrames.elementAt(i))).setTitle(titles[i]);
                    ((ViewJFrameBase) (imageFrames.elementAt(i))).setEnabled(true);

                    if (((Frame) (imageFrames.elementAt(i))) != parentFrame) {
                        ((ViewJFrameBase) parentFrame).getUserInterface().registerFrame((Frame)
                                                                                        (imageFrames.elementAt(i)));
                    }
                }

                if (parentFrame != null) {
                    ((ViewJFrameBase) parentFrame).getUserInterface().registerFrame(parentFrame);
                    ((ViewJFrameImage) parentFrame).getComponentImage().setLogMagDisplay(true);
                }

                updateFileTypeInfo(image, ModelStorageBase.FLOAT);
                image.notifyImageDisplayListeners(null, true);
            } else if (resultImage != null) {

                // algorithm failed but result image still has garbage
                resultImage.disposeLocal(); // clean up memory
                resultImage = null;
            }

            if (algorithm.isCompleted()) {
                insertScriptLine();
            }
        }
        
        if (algorithm instanceof AlgorithmFrequencyFilterColor) {

            if ((algorithm.isCompleted() == true) && (resultImage != null)) {

            	updateFileTypeInfo(image, resultImage, image.getType());

                // resultImage is the same or smaller than image.
                // The algorithm has completed and produced a new image to be displayed.
                try {

                    // resultImage.setImageName("Frequency Filtered image");
                    new ViewJFrameImage(resultImage, null, new Dimension(610, 200));
                } catch (OutOfMemoryError error) {
                    System.gc();
                    MipavUtil.displayError("Out of memory: unable to open new frame");
                }
            } else if (resultImage == null) {

                // These next lines set the titles in all frames where the source image is displayed to
                // image name so as to indicate that the image is now unlocked!
                // The image frames are enabled and then registed to the userinterface.
                Vector<ViewImageUpdateInterface> imageFrames = image.getImageFrameVector();

                for (int i = 0; i < imageFrames.size(); i++) {
                    ((ViewJFrameBase) (imageFrames.elementAt(i))).setTitle(titles[i]);
                    ((ViewJFrameBase) (imageFrames.elementAt(i))).setEnabled(true);

                    if (((Frame) (imageFrames.elementAt(i))) != parentFrame) {
                        ((ViewJFrameBase) parentFrame).getUserInterface().registerFrame((Frame)
                                                                                        (imageFrames.elementAt(i)));
                    }
                }

                if (parentFrame != null) {
                    ((ViewJFrameBase) parentFrame).getUserInterface().registerFrame(parentFrame);
                    ((ViewJFrameImage) parentFrame).getComponentImage().setLogMagDisplay(true);
                }

                updateFileTypeInfo(image, image.getType());
                image.notifyImageDisplayListeners(null, true);
            } else if (resultImage != null) {

                // algorithm failed but result image still has garbage
                resultImage.disposeLocal(); // clean up memory
                resultImage = null;
            }

            if (algorithm.isCompleted()) {
                insertScriptLine();
            }
        }
     // save the completion status for later
        setComplete(algorithm.isCompleted());

        if (FrequencyFilterAlgo != null) {
            FrequencyFilterAlgo.finalize();
            FrequencyFilterAlgo = null;
        }
        if (FrequencyFilterColorAlgo != null) {
            FrequencyFilterColorAlgo.finalize();
            FrequencyFilterColorAlgo = null;
        }
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
     * Accessor that sets the butterworth order.
     *
     * @param  order  Value to set the butterworth order to.
     */
    public void setButterworthOrder(int order) {
        butterworthOrder = order;
    }

    /**
     * Accessor that sets the kernel diameter.
     *
     * @param  diameter  Value to set the kernel diameter to.
     */
    public void setDiameter(int diameter) {
        kernelDiameter = diameter;
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
     * Accessor that sets the filter type (LOWPASS, HIGHPASS, BANDPASS, BANDSTOP).
     *
     * @param  type  Value to set the filter type to.
     */
    public void setFilterType(int type) {
        filterType = type;
    }

    /**
     * Accessor that sets the frequency 1 variable.
     *
     * @param  scale  Value to set frequency 1 to.
     */
    public void setFreq1(float scale) {
        freq1 = scale;
    }

    /**
     * Accessor that sets the frequency 2 variable.
     *
     * @param  scale  Value to set frequency 2 to.
     */
    public void setFreq2(float scale) {
        freq2 = scale;
    }

    /**
     * Accessor that sets image25D for single slice processing.
     *
     * @param  image25D  DOCUMENT ME!
     */
    public void setImage25D(boolean image25D) {
        this.image25D = image25D;
    }

    /**
     * Accessor that sets the image crop flag.
     *
     * @param  flag  <code>true</code> indicates image crop, <code>false</code> otherwise.
     */
    public void setImageCrop(boolean flag) {
        imageCrop = flag;
    }

    /**
     * Accessor that sets the construction method (WINDOW, GAUSSIAN, BUTTERWORTH).
     *
     * @param  method  Value to set the construction method to.
     */
    public void setMethod(int method) {
        constructionMethod = method;
    }

    /**
     * Once all the necessary variables are set, call the Frequency Filter algorithm based on what type of image this is
     * and whether or not there is a separate destination image.
     */
    protected void callAlgorithm() {
        image.setOriginalCropCheckbox(imageCrop);

        String name = makeImageName(image.getImageName(), "_freqFilter");

        if (displayLoc == NEW) {

            try {
                resultImage = (ModelImage) image.clone();
                resultImage.setImageName(name);
                resultImage.resetVOIs();

                // Make algorithm
                FrequencyFilterAlgo = new AlgorithmFrequencyFilter(resultImage, image, image25D, imageCrop,
                                                                   kernelDiameter, filterType, freq1, freq2,
                                                                   constructionMethod, butterworthOrder);

                // This is very important. Adding this object as a listener allows the algorithm to
                // notify this object when it has completed or failed. See algorithm performed event.
                // This is made possible by implementing AlgorithmedPerformed interface
                FrequencyFilterAlgo.addListener(this);
                createProgressBar(image.getImageName(), FrequencyFilterAlgo);

                // Hide dialog since the algorithm is about to run
                setVisible(false);

                if (isRunInSeparateThread()) {

                    // Start the thread as a low priority because we wish to still have user interface work fast.
                    if (FrequencyFilterAlgo.startMethod(Thread.MIN_PRIORITY) == false) {
                        MipavUtil.displayError("A thread is already running on this object");
                    }
                } else {

                    FrequencyFilterAlgo.run();
                }
            } catch (OutOfMemoryError x) {
                MipavUtil.displayError("Dialog FFT: unable to allocate enough memory");

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
                FrequencyFilterAlgo = new AlgorithmFrequencyFilter(image, image25D, imageCrop, kernelDiameter,
                                                                   filterType, freq1, freq2, constructionMethod,
                                                                   butterworthOrder);

                // This is very important. Adding this object as a listener allows the algorithm to
                // notify this object when it has completed or failed. See algorithm performed event.
                // This is made possible by implementing AlgorithmedPerformed interface
                FrequencyFilterAlgo.addListener(this);

                createProgressBar(image.getImageName(), FrequencyFilterAlgo);

                // Hide the dialog since the algorithm is about to run.
                setVisible(false);

                // These next lines set the titles in all frames where the source image is displayed to
                // "locked - " image name so as to indicate that the image is now read/write locked!
                // The image frames are disabled and then unregisted from the userinterface until the
                // algorithm has completed.
                Vector<ViewImageUpdateInterface> imageFrames = image.getImageFrameVector();
                titles = new String[imageFrames.size()];

                for (int i = 0; i < imageFrames.size(); i++) {
                    titles[i] = ((ViewJFrameBase) (imageFrames.elementAt(i))).getTitle();
                    ((ViewJFrameBase) (imageFrames.elementAt(i))).setTitle("Locked: " + titles[i]);
                    ((ViewJFrameBase) (imageFrames.elementAt(i))).setEnabled(false);
                    userInterface.unregisterFrame((Frame) (imageFrames.elementAt(i)));
                }

                if (isRunInSeparateThread()) {

                    // Start the thread as a low priority because we wish to still have user interface work fast.
                    if (FrequencyFilterAlgo.startMethod(Thread.MIN_PRIORITY) == false) {
                        MipavUtil.displayError("A thread is already running on this object");
                    }
                } else {


                    FrequencyFilterAlgo.run();
                }
            } catch (OutOfMemoryError x) {
                MipavUtil.displayError("Dialog FFT: unable to allocate enough memory");

                return;
            }
        }
    }
    
    /**
     * Once all the necessary variables are set, call the Frequency Filter algorithm based on what type of image this is
     * and whether or not there is a separate destination image.
     */
    protected void callColorAlgorithm() {
        image.setOriginalCropCheckbox(imageCrop);

        String name = makeImageName(image.getImageName(), "_freqFilter");

        if (displayLoc == NEW) {

            try {
                resultImage = (ModelImage) image.clone();
                resultImage.setImageName(name);
                resultImage.resetVOIs();

                // Make algorithm
                FrequencyFilterColorAlgo = new AlgorithmFrequencyFilterColor(resultImage, image, image25D, imageCrop,
                                                                   kernelDiameter, filterType, freq1, freq2,
                                                                   constructionMethod, butterworthOrder);

                // This is very important. Adding this object as a listener allows the algorithm to
                // notify this object when it has completed or failed. See algorithm performed event.
                // This is made possible by implementing AlgorithmedPerformed interface
                FrequencyFilterColorAlgo.addListener(this);
                createProgressBar(image.getImageName(), FrequencyFilterColorAlgo);

                // Hide dialog since the algorithm is about to run
                setVisible(false);

                if (isRunInSeparateThread()) {

                    // Start the thread as a low priority because we wish to still have user interface work fast.
                    if (FrequencyFilterColorAlgo.startMethod(Thread.MIN_PRIORITY) == false) {
                        MipavUtil.displayError("A thread is already running on this object");
                    }
                } else {

                    FrequencyFilterColorAlgo.run();
                }
            } catch (OutOfMemoryError x) {
                MipavUtil.displayError("Dialog FFT: unable to allocate enough memory");

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
                FrequencyFilterColorAlgo = new AlgorithmFrequencyFilterColor(image, image25D, imageCrop, kernelDiameter,
                                                                   filterType, freq1, freq2, constructionMethod,
                                                                   butterworthOrder);

                // This is very important. Adding this object as a listener allows the algorithm to
                // notify this object when it has completed or failed. See algorithm performed event.
                // This is made possible by implementing AlgorithmedPerformed interface
                FrequencyFilterColorAlgo.addListener(this);

                createProgressBar(image.getImageName(), FrequencyFilterColorAlgo);

                // Hide the dialog since the algorithm is about to run.
                setVisible(false);

                // These next lines set the titles in all frames where the source image is displayed to
                // "locked - " image name so as to indicate that the image is now read/write locked!
                // The image frames are disabled and then unregisted from the userinterface until the
                // algorithm has completed.
                Vector<ViewImageUpdateInterface> imageFrames = image.getImageFrameVector();
                titles = new String[imageFrames.size()];

                for (int i = 0; i < imageFrames.size(); i++) {
                    titles[i] = ((ViewJFrameBase) (imageFrames.elementAt(i))).getTitle();
                    ((ViewJFrameBase) (imageFrames.elementAt(i))).setTitle("Locked: " + titles[i]);
                    ((ViewJFrameBase) (imageFrames.elementAt(i))).setEnabled(false);
                    userInterface.unregisterFrame((Frame) (imageFrames.elementAt(i)));
                }

                if (isRunInSeparateThread()) {

                    // Start the thread as a low priority because we wish to still have user interface work fast.
                    if (FrequencyFilterColorAlgo.startMethod(Thread.MIN_PRIORITY) == false) {
                        MipavUtil.displayError("A thread is already running on this object");
                    }
                } else {


                    FrequencyFilterColorAlgo.run();
                }
            } catch (OutOfMemoryError x) {
                MipavUtil.displayError("Dialog FFT: unable to allocate enough memory");

                return;
            }
        }
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
        image = scriptParameters.retrieveInputImage();
        userInterface = ViewUserInterface.getReference();
        parentFrame = image.getParentFrame();

        if (scriptParameters.getParams().getBoolean(AlgorithmParameters.DO_OUTPUT_NEW_IMAGE)) {
            setDisplayLocNew();
        } else {
            setDisplayLocReplace();
        }

        setImage25D(scriptParameters.getParams().getBoolean(AlgorithmParameters.DO_PROCESS_3D_AS_25D));
        setImageCrop(scriptParameters.getParams().getBoolean("do_crop_image"));
        setDiameter(scriptParameters.getParams().getInt("kernel_diameter"));
        setFilterType(scriptParameters.getParams().getInt("filter_type"));
        setFreq1(scriptParameters.getParams().getFloat("freq1"));
        setFreq2(scriptParameters.getParams().getFloat("freq2"));
        setMethod(scriptParameters.getParams().getInt("construction_method"));
        setButterworthOrder(scriptParameters.getParams().getInt("butterworth_order"));
    }

    /**
     * {@inheritDoc}
     */
    protected void storeParamsFromGUI() throws ParserException {
        scriptParameters.storeInputImage(image);
        scriptParameters.storeOutputImageParams(getResultImage(), (displayLoc == NEW));

        scriptParameters.storeProcess3DAs25D(image25D);
        scriptParameters.getParams().put(ParameterFactory.newParameter("do_crop_image", imageCrop));
        scriptParameters.getParams().put(ParameterFactory.newParameter("kernel_diameter", kernelDiameter));
        scriptParameters.getParams().put(ParameterFactory.newParameter("filter_type", filterType));
        scriptParameters.getParams().put(ParameterFactory.newParameter("freq1", freq1));
        scriptParameters.getParams().put(ParameterFactory.newParameter("freq2", freq2));
        scriptParameters.getParams().put(ParameterFactory.newParameter("construction_method", constructionMethod));
        scriptParameters.getParams().put(ParameterFactory.newParameter("butterworth_order", butterworthOrder));
    }

    /**
     * Sets up the GUI (panels, buttons, etc) and displays it on the screen.
     */
    private void init() {
        setTitle("Frequency Filter");

        JPanel optionsPanel = new JPanel(new GridLayout(1, 1));
        optionsPanel.setBorder(buildTitledBorder("Options"));

        image25DCheckbox = new JCheckBox("Process each slice independently (2.5D)");
        image25DCheckbox.setFont(serif12);
        image25DCheckbox.setForeground(Color.black);

        if ((image.getNDims() == 3) && (image.isColorImage())) {
            image25DCheckbox.setEnabled(false);
            image25DCheckbox.setSelected(true);
        }
        else if ((image.getNDims() == 3) && (image.getType() != ModelStorageBase.COMPLEX)) {
            image25DCheckbox.setEnabled(true);
            image25DCheckbox.setSelected(false);
        } else if ((image.getNDims() == 3) && (image.getType() == ModelStorageBase.COMPLEX)) {
            image25DCheckbox.setEnabled(false);
            image25DCheckbox.setSelected(image.getImage25D());
        } else {
            image25DCheckbox.setEnabled(false);
            image25DCheckbox.setSelected(false);
        }

        image25DCheckbox.addActionListener(this);
        optionsPanel.add(image25DCheckbox);
        constructionPanel = new JPanel(new GridBagLayout());
        constructionPanel.setBorder(buildTitledBorder("Filter construction methods"));

        constructionGroup = new ButtonGroup();
        windowFilter = new JRadioButton("Windowed finite impulse response", true);
        windowFilter.setFont(serif12);
        windowFilter.setForeground(Color.black);
        windowFilter.addActionListener(this);
        constructionGroup.add(windowFilter);

        if (!image.isComplexImage()) {
            windowFilter.setEnabled(true);
        } else {
            windowFilter.setEnabled(false);
            constructionMethod = image.getOriginalFilterConstruction();

            if (constructionMethod == WINDOW) {
                windowFilter.setSelected(true);
            } else {
                windowFilter.setSelected(false);
            }
        }

        imageCropCheckbox = new JCheckBox("Crop image to save memory");
        imageCropCheckbox.setFont(serif12);
        imageCropCheckbox.setForeground(Color.black);

        if (!image.isComplexImage()) {
            imageCropCheckbox.setEnabled(true);
            imageCropCheckbox.setSelected(true);
        } else {
            imageCropCheckbox.setEnabled(false);
            imageCropCheckbox.setSelected(image.getOriginalCropCheckbox());
        }

        imageCropCheckbox.addActionListener(this);

        textKernelDiameter = new JTextField(10);

        if (!image.isComplexImage()) {
            textKernelDiameter.setText("15");
        } else {
            kernelDiameter = image.getOriginalKernelDimension();
            textKernelDiameter.setText(String.valueOf(kernelDiameter));
        }

        textKernelDiameter.setFont(serif12);
        textKernelDiameter.setForeground(Color.black);

        if (!image.isComplexImage()) {
            textKernelDiameter.setEnabled(true);
        } else {
            textKernelDiameter.setEnabled(false);
        }

        textKernelDiameter.addFocusListener(this);

        labelKernelDiameter = new JLabel("Convolution kernel diameter - odd");
        labelKernelDiameter.setForeground(Color.black);
        labelKernelDiameter.setFont(serif12);

        if (!image.isComplexImage()) {
            labelKernelDiameter.setEnabled(true);
        } else {
            labelKernelDiameter.setEnabled(false);
        }

        gaussianFilter = new JRadioButton("Gaussian filter", false);
        gaussianFilter.setFont(serif12);
        gaussianFilter.setForeground(Color.black);
        gaussianFilter.addActionListener(this);
        constructionGroup.add(gaussianFilter);

        if (!image.isComplexImage()) {
            gaussianFilter.setEnabled(true);
        } else {
            gaussianFilter.setEnabled(false);

            if (constructionMethod == GAUSSIAN) {
                gaussianFilter.setSelected(true);
            } else {
                gaussianFilter.setSelected(false);
            }
        }

        butterworthFilter = new JRadioButton("Butterworth filter", false);
        butterworthFilter.setFont(serif12);
        butterworthFilter.setForeground(Color.black);
        butterworthFilter.addActionListener(this);
        constructionGroup.add(butterworthFilter);

        if (!image.isComplexImage()) {
            butterworthFilter.setEnabled(true);
        } else {
            butterworthFilter.setEnabled(false);

            if (constructionMethod == BUTTERWORTH) {
                butterworthFilter.setSelected(true);
            } else {
                butterworthFilter.setSelected(false);
            }
        }

        textOrder = new JTextField(10);

        if (!image.isComplexImage()) {
            textOrder.setText("1");
        } else {
            butterworthOrder = image.getOriginalButterworthOrder();
            textOrder.setText(String.valueOf(butterworthOrder));
        }

        textOrder.setFont(serif12);
        textOrder.setForeground(Color.black);
        textOrder.setEnabled(false);

        labelOrder = new JLabel("Order");
        labelOrder.setForeground(Color.black);
        labelOrder.setFont(serif12);
        labelOrder.setEnabled(false);

        GridBagConstraints gbc = new GridBagConstraints();
        gbc.gridwidth = 3;
        gbc.anchor = GridBagConstraints.WEST;
        gbc.weightx = 1;

        gbc.gridx = 0;
        gbc.gridy = 0;
        constructionPanel.add(windowFilter, gbc);
        gbc.gridy = 1;
        gbc.gridwidth = 1;
        constructionPanel.add(Box.createHorizontalStrut(5), gbc);
        gbc.gridx = 1;
        constructionPanel.add(imageCropCheckbox, gbc);
        gbc.gridx = 1;
        gbc.gridy = 2;
        constructionPanel.add(labelKernelDiameter, gbc);
        gbc.gridx = 2;
        constructionPanel.add(textKernelDiameter, gbc);
        gbc.gridx = 0;
        gbc.gridy = 3;
        gbc.gridwidth = 3;
        constructionPanel.add(gaussianFilter, gbc);
        gbc.gridy = 4;
        constructionPanel.add(butterworthFilter, gbc);
        gbc.gridx = 1;
        gbc.gridy = 5;
        gbc.gridwidth = 1;
        constructionPanel.add(labelOrder, gbc);
        gbc.gridx = 2;
        constructionPanel.add(textOrder, gbc);

        destinationPanel = new JPanel(new GridLayout(2, 1));
        destinationPanel.setForeground(Color.black);
        destinationPanel.setBorder(buildTitledBorder("Destination"));

        destinationGroup = new ButtonGroup();
        newImage = new JRadioButton("New image", true);
        newImage.setFont(serif12);
        newImage.setForeground(Color.black);
        destinationGroup.add(newImage);
        destinationPanel.add(newImage);

        replaceImage = new JRadioButton("Replace image", false);
        replaceImage.setFont(serif12);
        replaceImage.setForeground(Color.black);
        destinationGroup.add(replaceImage);
        destinationPanel.add(replaceImage);

        // Only if the image is unlocked can it be replaced.
        if (image.getLockStatus() == ModelStorageBase.UNLOCKED) {
            replaceImage.setEnabled(true);
        } else {
            replaceImage.setEnabled(false);
        }

        filterPanel = new JPanel(new GridBagLayout());
        filterPanel.setBorder(buildTitledBorder("Filter specifications"));

        filterTypeGroup = new ButtonGroup();
        filterType = image.getFilterType();

        if (filterType == LOWPASS) {
            lowPass = new JRadioButton("Lowpass", true);
        } else {
            lowPass = new JRadioButton("Lowpass", false);
        }

        lowPass.setFont(serif12);
        lowPass.setForeground(Color.black);
        lowPass.addActionListener(this);
        filterTypeGroup.add(lowPass);

        if (filterType == HIGHPASS) {
            highPass = new JRadioButton("Highpass", true);
        } else {
            highPass = new JRadioButton("Highpass", false);
        }

        highPass.setFont(serif12);
        highPass.addActionListener(this);
        highPass.setForeground(Color.black);
        filterTypeGroup.add(highPass);

        if (filterType == BANDPASS) {
            bandPass = new JRadioButton("Bandpass", true);
        } else {
            bandPass = new JRadioButton("Bandpass", false);
        }

        bandPass.setFont(serif12);
        bandPass.setForeground(Color.black);
        bandPass.addActionListener(this);
        filterTypeGroup.add(bandPass);

        if (filterType == BANDSTOP) {
            bandStop = new JRadioButton("Bandstop", true);
        } else {
            bandStop = new JRadioButton("Bandstop", false);
        }

        bandStop.setForeground(Color.black);
        bandStop.setFont(serif12);
        bandStop.addActionListener(this);
        filterTypeGroup.add(bandStop);

        textF1 = new JTextField(10);
        freq1 = image.getFreq1();
        textF1.setText(String.valueOf(freq1));
        textF1.setFont(serif12);
        textF1.setEnabled(true);

        labelF1 = new JLabel("Frequency F1 0.0 to 1.0 ");
        labelF1.setForeground(Color.black);
        labelF1.setFont(serif12);
        labelF1.setEnabled(true);

        textF2 = new JTextField(10);
        freq2 = image.getFreq2();
        textF2.setText(String.valueOf(freq2));
        textF2.setFont(serif12);
        textF2.setEnabled(false);

        labelF2 = new JLabel("F2 exceeds F1 0.0 to 1.0 ");
        labelF2.setForeground(Color.black);
        labelF2.setFont(serif12);
        labelF2.setEnabled(false);

        gbc.gridx = 0;
        gbc.gridy = 0;
        gbc.gridwidth = 1;
        gbc.weightx = 0;
        gbc.fill = GridBagConstraints.NONE;
        filterPanel.add(lowPass, gbc);
        gbc.gridx = 1;
        filterPanel.add(labelF1, gbc);
        gbc.gridx = 2;
        gbc.weightx = .5;
        gbc.fill = GridBagConstraints.HORIZONTAL;
        filterPanel.add(textF1, gbc);
        gbc.gridx = 0;
        gbc.gridy = 1;
        filterPanel.add(highPass, gbc);
        gbc.gridy = 2;
        filterPanel.add(bandPass, gbc);
        gbc.gridx = 1;
        filterPanel.add(labelF2, gbc);
        gbc.gridx = 2;
        filterPanel.add(textF2, gbc);
        gbc.gridx = 0;
        gbc.gridy = 3;
        filterPanel.add(bandStop, gbc);

        mainPanel = new JPanel(new GridBagLayout());
        gbc.gridx = 0;
        gbc.gridy = 0;
        gbc.fill = GridBagConstraints.HORIZONTAL;
        mainPanel.add(optionsPanel, gbc);
        gbc.gridy = 1;
        mainPanel.add(constructionPanel, gbc);
        gbc.gridy = 2;
        mainPanel.add(destinationPanel, gbc);
        gbc.gridy = 3;
        mainPanel.add(filterPanel, gbc);

        getContentPane().add(mainPanel);
        getContentPane().add(buildButtons(), BorderLayout.SOUTH);

        String labelS = labelF1.getText();
        labelF1.setText("Frequency F1 exceeds 0.0 ");
        pack();
        labelF1.setText(labelS);
        setVisible(true);
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

        if (image25DCheckbox.isSelected()) {
            image25D = true;
        } else {
            image25D = false;
        }

        tmpStr = textKernelDiameter.getText();
        kernelDiameter = Integer.parseInt(tmpStr);

        if (kernelDiameter < 3) {
            MipavUtil.displayError("kernelDiameter must be at least 3");
            textKernelDiameter.requestFocus();
            textKernelDiameter.selectAll();

            return false;
        } else if ((kernelDiameter % 2) != 1) {
            MipavUtil.displayError("kernelDiameter must be odd");
            textKernelDiameter.requestFocus();
            textKernelDiameter.selectAll();

            return false;
        }

        if (windowFilter.isSelected()) {
            constructionMethod = WINDOW;
        } else if (gaussianFilter.isSelected()) {
            constructionMethod = GAUSSIAN;
        } else if (butterworthFilter.isSelected()) {
            constructionMethod = BUTTERWORTH;
            tmpStr = textOrder.getText();
            butterworthOrder = Integer.parseInt(tmpStr);

            if (butterworthOrder < 1) {
                MipavUtil.displayError("Butterworth order must be at least 1");
                textOrder.requestFocus();
                textOrder.selectAll();

                return false;
            }
        } // end of else if (butterworthFilter.isSelected())

        if (lowPass.isSelected()) {
            filterType = LOWPASS;
        } else if (highPass.isSelected()) {
            filterType = HIGHPASS;
        } else if (bandPass.isSelected()) {
            filterType = BANDPASS;
        } else if (bandStop.isSelected()) {
            filterType = BANDSTOP;
        }

        tmpStr = textF1.getText();

        if (constructionMethod == WINDOW) {

            if (testParameter(tmpStr, 0.0, 1.0)) {
                freq1 = Float.valueOf(tmpStr).floatValue();
                freq1 = (float) Math.PI * freq1;
            } else {
                MipavUtil.displayError("F1 must be between 0.0 and 1.0");
                textF1.requestFocus();
                textF1.selectAll();

                return false;
            }
        } // end of if (constructionMethod == WINDOW)
        else if (constructionMethod == GAUSSIAN) {
            freq1 = Float.valueOf(tmpStr).floatValue();

            if (freq1 <= 0.0) {
                MipavUtil.displayError("F1 must exceed 0.0");
                textF1.requestFocus();
                textF1.selectAll();

                return false;
            }
        } // end of else if (constructionMethod == GAUSSIAN)
        else if (constructionMethod == BUTTERWORTH) {
            freq1 = Float.valueOf(tmpStr).floatValue();

            if (freq1 <= 0.0) {
                MipavUtil.displayError("F1 must exceed 0.0");
                textF1.requestFocus();
                textF1.selectAll();

                return false;
            } else if (freq1 > 1.0) {
                MipavUtil.displayError("F1 must not exceed 1.0");
                textF1.requestFocus();
                textF1.selectAll();

                return false;
            }
        } // end of else if (constructionMethod == BUTTERWORTH)

        if ((filterType == BANDPASS) || (filterType == BANDSTOP)) {
            tmpStr = textF2.getText();

            if (testParameter(tmpStr, 0.0, 1.0)) {
                freq2 = Float.valueOf(tmpStr).floatValue();

                if (constructionMethod == WINDOW) {
                    freq2 = (float) Math.PI * freq2;
                }

                if (freq2 <= freq1) {
                    MipavUtil.displayError("F2 must exceed F1");
                    textF2.requestFocus();
                    textF2.selectAll();

                    return false;
                }
            } else {
                MipavUtil.displayError("F2 must be between 0.0 and 1.0");
                textF2.requestFocus();
                textF2.selectAll();

                return false;
            }

        } // end of if ((filterType == BANDPASS) || (filterType == BUTTERWORTH))

        if (imageCropCheckbox.isSelected()) {
            imageCrop = true;
        } else {
            imageCrop = false;
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
                return new String("Algorithms");
            }

            public String getDescription() {
                return new String("Filters an image through the frequency domain");
            }

            public String getDescriptionLong() {
                return new String("Applies a simple gaussian blur filter.");
            }

            public String getShortLabel() {
                return new String("FreqFilter");
            }

            public String getLabel() {
                return new String("Frequency Filter");
            }

            public String getName() {
                return new String("Frequency Filter");
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
            table.put(new ParameterBoolean(AlgorithmParameters.DO_PROCESS_3D_AS_25D, false));
            table.put(new ParameterBoolean("do_crop_image", true));
            table.put(new ParameterInt("filter_type", 2));
            table.put(new ParameterFloat("freq1", (float) 0.4));
            table.put(new ParameterFloat("freq2", (float) 0.7));
            table.put(new ParameterInt("construction_method", 1));
            table.put(new ParameterInt("butterworth_order", 0));
            table.put(new ParameterInt("kernel_diameter", 15));
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
