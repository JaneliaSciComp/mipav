package gov.nih.mipav.view.dialogs;


import gov.nih.mipav.model.algorithms.*;
import gov.nih.mipav.model.algorithms.filters.*;
import gov.nih.mipav.model.scripting.ParserException;
import gov.nih.mipav.model.scripting.parameters.ParameterFactory;
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
 * @see  AlgorithmFFT
 */
public class JDialogFFT extends JDialogScriptableBase implements AlgorithmInterface, ItemListener {

    //~ Static fields/initializers -------------------------------------------------------------------------------------

    /** Use serialVersionUID for interoperability. */
    private static final long serialVersionUID = -7969114857894881595L;

    /** DOCUMENT ME! */
    public static final int INVERSE = -1;

    /** DOCUMENT ME! */
    public static final int FILTER = 0;

    /** DOCUMENT ME! */
    public static final int FORWARD = 1;

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
    private JPanel buttonPanel;

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
    private ButtonGroup directionGroup;


    /** DOCUMENT ME! */
    private JPanel directionPanel;

    /** DOCUMENT ME! */
    private int displayLoc; // Flag indicating if a new image is to be generated
                            // or if the source image is to be replaced

    /** DOCUMENT ME! */
    private AlgorithmFFT FFTAlgo = null;

    /** DOCUMENT ME! */
    private JPanel filterPanel;

    /** DOCUMENT ME! */
    private int filterType;

    /** DOCUMENT ME! */
    private ButtonGroup filterTypeGroup;

    /** DOCUMENT ME! */
    private JRadioButton forwardFFT;

    /** DOCUMENT ME! */
    private float freq1;

    /** DOCUMENT ME! */
    private float freq2;

    /** DOCUMENT ME! */
    private JRadioButton frequencyFilter;

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
    private JRadioButton inverseFFT;

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

    /** private JPanel optionsPanel;. */
    private JCheckBox logDisplayCheckbox;

    /** DOCUMENT ME! */
    private boolean logMagDisplay;

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
    private int transformDir;

    /** DOCUMENT ME! */
    private boolean unequalDim;

    /** DOCUMENT ME! */
    private JCheckBox unequalDimCheckbox;

    /** DOCUMENT ME! */
    private ViewUserInterface userInterface;

    /** DOCUMENT ME! */
    private JRadioButton windowFilter;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Empty constructor needed for dynamic instantiation (used during scripting).
     */
    public JDialogFFT() { }

    /**
     * Creates new FFT dialog.
     *
     * @param  theParentFrame  Parent frame
     * @param  im              Source image
     */
    public JDialogFFT(Frame theParentFrame, ModelImage im) {
        super(theParentFrame, false);
        image = im;
        userInterface = ViewUserInterface.getReference();
        init();
    }

    

    //~ Methods --------------------------------------------------------------------------------------------------------
 /*   image, transformDir, logMagDisplay, unequalDim, image25D, imageCrop,
    kernelDiameter, filterType, freq1, freq2, constructionMethod,
    butterworthOrder*/
    
    
    
      
    /**
     * Record the parameters just used to run this algorithm in a script.
     * 
     * @throws  ParserException  If there is a problem creating/recording the new parameters.
     */
    protected void storeParamsFromGUI() throws ParserException{
        scriptParameters.storeInputImage(image);
        scriptParameters.storeOutputImageParams(resultImage, (displayLoc == NEW)); 
        
        scriptParameters.getParams().put(ParameterFactory.newParameter("transformDir",transformDir));
        scriptParameters.getParams().put(ParameterFactory.newParameter("logMagDisplay",logMagDisplay));
        scriptParameters.getParams().put(ParameterFactory.newParameter("unequalDim",unequalDim));
        scriptParameters.getParams().put(ParameterFactory.newParameter("image25D",image25D));
        scriptParameters.getParams().put(ParameterFactory.newParameter("imageCrop",imageCrop));
        scriptParameters.getParams().put(ParameterFactory.newParameter("filterType",filterType));
        scriptParameters.getParams().put(ParameterFactory.newParameter("freq1",freq1));
        scriptParameters.getParams().put(ParameterFactory.newParameter("freq2",freq2));
        scriptParameters.getParams().put(ParameterFactory.newParameter("constructionMethod",constructionMethod));
        scriptParameters.getParams().put(ParameterFactory.newParameter("butterworthOrder",butterworthOrder));
        scriptParameters.getParams().put(ParameterFactory.newParameter("kernelDiameter",kernelDiameter));
   }
    
    /**
     * Set the dialog GUI using the script parameters while running this algorithm as part of a script.
     */
    protected void setGUIFromParams(){
        image = scriptParameters.retrieveInputImage();
        if (scriptParameters.getParams().getBoolean(AlgorithmParameters.DO_OUTPUT_NEW_IMAGE)) {
            setDisplayLocNew();
        } else {
            setDisplayLocReplace();
        }
        
        transformDir = scriptParameters.getParams().getInt("transformDir");
        logMagDisplay = scriptParameters.getParams().getBoolean("logMagDisplay");
        unequalDim = scriptParameters.getParams().getBoolean("unequalDim");
        image25D = scriptParameters.getParams().getBoolean("image25D");
        imageCrop = scriptParameters.getParams().getBoolean("imageCrop");
        filterType = scriptParameters.getParams().getInt("filterType");
        freq1 = scriptParameters.getParams().getInt("freq1");
        freq2 = scriptParameters.getParams().getInt("freq2");
        constructionMethod = scriptParameters.getParams().getInt("constructionMethod");
        butterworthOrder = scriptParameters.getParams().getInt("butterworthOrder");
        kernelDiameter = scriptParameters.getParams().getInt("kernelDiameter");
   }
    
    /**
     * Used to perform actions after the execution of the algorithm is completed (e.g., put the result image in the image table).
     * Defaults to no action, override to actually have it do something.
     */
    protected void doPostAlgorithmActions() {
        if (displayLoc == NEW) {
            AlgorithmParameters.storeImageInRunner(getResultImage());
        }
    }
  
    
    
    /**
     * Closes dialog box when the OK button is pressed and calls the algorithm.
     *
     * @param  event  event that triggers function
     */
    public void actionPerformed(ActionEvent event) {
        String command = event.getActionCommand();
        Object source = event.getSource();
        transformDir = FORWARD;

        if (source == OKButton) {

            if (setVariables()) {
                callAlgorithm();
            }
        } else if ((source == forwardFFT) || (source == frequencyFilter) || (source == inverseFFT)) {

            if (forwardFFT.isSelected()) {

                if (image.getNDims() == 3) {
                    image25DCheckbox.setEnabled(true);
                } else {
                    image25DCheckbox.setEnabled(false);
                }

                logDisplayCheckbox.setEnabled(true);
                unequalDimCheckbox.setEnabled(true);

                if (windowFilter.isSelected()) {
                    imageCropCheckbox.setEnabled(true);
                    textKernelDiameter.setEnabled(true);
                    labelKernelDiameter.setEnabled(true);
                    textOrder.setEnabled(false);
                    labelOrder.setEnabled(false);
                } else if (gaussianFilter.isSelected()) {
                    imageCropCheckbox.setEnabled(false);
                    textKernelDiameter.setEnabled(false);
                    labelKernelDiameter.setEnabled(false);
                    textOrder.setEnabled(false);
                    labelOrder.setEnabled(false);
                } else if (butterworthFilter.isSelected()) {
                    imageCropCheckbox.setEnabled(false);
                    textKernelDiameter.setEnabled(false);
                    labelKernelDiameter.setEnabled(false);
                    textOrder.setEnabled(true);
                    labelOrder.setEnabled(true);
                }

                lowPass.setEnabled(false);
                highPass.setEnabled(false);
                bandPass.setEnabled(false);
                bandStop.setEnabled(false);
                textF1.setEnabled(false);
                labelF1.setEnabled(false);
                textF2.setEnabled(false);
                labelF2.setEnabled(false);
                windowFilter.setEnabled(true);
                gaussianFilter.setEnabled(true);
                butterworthFilter.setEnabled(true);
            } else if (frequencyFilter.isSelected()) {
                image25DCheckbox.setEnabled(false);
                logDisplayCheckbox.setEnabled(true);
                unequalDimCheckbox.setEnabled(false);
                imageCropCheckbox.setEnabled(false);
                textKernelDiameter.setEnabled(false);
                labelKernelDiameter.setEnabled(false);
                lowPass.setEnabled(true);
                highPass.setEnabled(true);

                if (windowFilter.isSelected()) {
                    bandPass.setEnabled(true);
                    bandStop.setEnabled(true);
                } else if (gaussianFilter.isSelected()) {
                    bandPass.setEnabled(false);
                    bandStop.setEnabled(false);
                } else if (butterworthFilter.isSelected()) {
                    bandPass.setEnabled(true);
                    bandStop.setEnabled(true);
                }

                textF1.setEnabled(true);
                labelF1.setEnabled(true);
                windowFilter.setEnabled(false);
                gaussianFilter.setEnabled(false);
                butterworthFilter.setEnabled(false);
                textOrder.setEnabled(false);
                labelOrder.setEnabled(false);
            } else if (inverseFFT.isSelected()) {
                image25DCheckbox.setEnabled(false);
                logDisplayCheckbox.setEnabled(false);
                unequalDimCheckbox.setEnabled(false);
                imageCropCheckbox.setEnabled(false);
                textKernelDiameter.setEnabled(false);
                labelKernelDiameter.setEnabled(false);
                lowPass.setEnabled(false);
                highPass.setEnabled(false);
                bandPass.setEnabled(false);
                bandStop.setEnabled(false);
                textF1.setEnabled(false);
                labelF1.setEnabled(false);
                textF2.setEnabled(false);
                labelF2.setEnabled(false);
                windowFilter.setEnabled(false);
                gaussianFilter.setEnabled(false);
                butterworthFilter.setEnabled(false);
                textOrder.setEnabled(false);
                labelOrder.setEnabled(false);
            }
        } else if (source == windowFilter) {
            imageCropCheckbox.setEnabled(true);
            textKernelDiameter.setEnabled(true);
            labelKernelDiameter.setEnabled(true);
            labelF1.setText("Frequency F1 0.0 to 1.0 ");
            textOrder.setEnabled(false);
            labelOrder.setEnabled(false);
        } else if (source == gaussianFilter) {
            imageCropCheckbox.setEnabled(false);
            imageCropCheckbox.setSelected(false);
            textKernelDiameter.setEnabled(false);
            labelKernelDiameter.setEnabled(false);
            labelF1.setText("Frequency F1 exceeds 0.0 ");
            textOrder.setEnabled(false);
            labelOrder.setEnabled(false);
        } else if (source == butterworthFilter) {
            imageCropCheckbox.setEnabled(false);
            imageCropCheckbox.setSelected(false);
            textKernelDiameter.setEnabled(false);
            labelKernelDiameter.setEnabled(false);
            labelF1.setText("Frequency F1 >0.0 to 1.0 ");
            textOrder.setEnabled(true);
            labelOrder.setEnabled(true);
        } else if ((source == lowPass) || (source == highPass)) {
            textF2.setEnabled(false);
            labelF2.setEnabled(false);
        } else if ((source == bandPass) || (source == bandStop)) {
            textF2.setEnabled(true);
            labelF2.setEnabled(true);
        } else if (source == helpButton) {
            MipavUtil.showHelp("10003");
        } else if (source == cancelButton) {
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
     * @param  algorithm  algorithm that caused the event.
     */
    public void algorithmPerformed(AlgorithmBase algorithm) {

        ViewJFrameImage imageFrame = null;

        if (algorithm instanceof AlgorithmFFT) {

            if ((FFTAlgo.isCompleted() == true) && (resultImage != null)) {

                if ((transformDir == FORWARD) || (transformDir == FILTER)) {

                    // resultImage is the same or bigger than image
                    updateFFTFileInfo(image, resultImage, ModelStorageBase.COMPLEX);
                } else if (transformDir == INVERSE) {
                    updateFileTypeInfo(image, resultImage, ModelStorageBase.FLOAT);
                    // resultImage is the same or smaller than image.
                }

                // The algorithm has completed and produced a new image to be displayed.
                try {

                    if (transformDir == FORWARD) {
                        resultImage.setImageName(image.getImageName() + "_FFT");
                    } else if (transformDir == FILTER) {
                        resultImage.setImageName("_FilteredFFT");
                    } else if (transformDir == INVERSE) {
                        resultImage.setImageName("_InverseFFT");
                    }

                    imageFrame = new ViewJFrameImage(resultImage, null, new Dimension(610, 200), logMagDisplay);
                } catch (OutOfMemoryError error) {
                    MipavUtil.displayError("Out of memory: unable to open new frame");
                }
            } else if (resultImage == null) {

                if (transformDir == FORWARD) {
                    updateFileTypeInfo(image, ModelStorageBase.COMPLEX);
                } else if (transformDir == INVERSE) {
                    updateFileTypeInfo(image, ModelStorageBase.FLOAT);
                }

                Dimension parentLocation;

                if (parentFrame != null) {
                    parentLocation = new Dimension(parentFrame.getLocationOnScreen().x,
                                                   parentFrame.getLocationOnScreen().y);
                } else {
                    parentLocation = new Dimension(Toolkit.getDefaultToolkit().getScreenSize().width / 2,
                                                   Toolkit.getDefaultToolkit().getScreenSize().height / 2);
                }


                // The framelist of image2 is null
                // Because the image dimensions can change in the FFT and inverse FFT
                // close all the old frames
                ModelImage image2 = (ModelImage) image.clone();
                String imageName = image.getImageName();

                Vector imageFrames = image.getImageFrameVector();

                for (int i = 0; i < imageFrames.size(); i++) {
                    ((ViewJFrameBase) (imageFrames.elementAt(i))).close();
                }

                image2.setImageName(imageName);

                try {
                    imageFrame = new ViewJFrameImage(image2, null, parentLocation);
                } catch (OutOfMemoryError error) {
                    MipavUtil.displayError("Out of memory: unable to open new frame");
                }

                ((ViewJFrameImage) imageFrame).getComponentImage().setLogMagDisplay(true);

                image2.notifyImageDisplayListeners(null, true);
            } else if (resultImage != null) {

                // algorithm failed but result image still has garbage
                resultImage.disposeLocal(); // clean up memory
                resultImage = null;
            }

            insertScriptLine();
        }


        // Update frame
        // ((ViewJFrameBase)parentFrame).updateImages(true);
        FFTAlgo.finalize();
        FFTAlgo = null;
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
     * Accessor that turns log mag display on or off.
     *
     * @param  logMagDisplay  DOCUMENT ME!
     */
    public void setLogMagDisplay(boolean logMagDisplay) {
        this.logMagDisplay = logMagDisplay;
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
     * Accessor that sets the transform direction.
     *
     * @param  transformDir  DOCUMENT ME!
     */
    public void setTransformDir(int transformDir) {
        this.transformDir = transformDir;
    }

    /**
     * Accessor that sets whether or not the picture dimension can be unequal.
     *
     * @param  unequalDim  DOCUMENT ME!
     */
    public void setUnequalDim(boolean unequalDim) {
        this.unequalDim = unequalDim;
    }

    /**
     * Once all the necessary variables are set, call the FFT algorithm based on what type of image this is and whether
     * or not there is a separate destination image.
     */
    protected void callAlgorithm() {
        image.setOriginalCropCheckbox(imageCrop);

        String name = makeImageName(image.getImageName(), "_FFT");

        if (displayLoc == NEW) {

            try {
                resultImage = (ModelImage) image.clone();
                resultImage.setImageName(name);
                resultImage.resetVOIs();

                // Make algorithm
                FFTAlgo = new AlgorithmFFT(resultImage, image, transformDir, logMagDisplay, unequalDim, image25D,
                                           imageCrop, kernelDiameter, filterType, freq1, freq2, constructionMethod,
                                           butterworthOrder);

                // This is very important. Adding this object as a listener allows the algorithm to
                // notify this object when it has completed or failed. See algorithm performed event.
                // This is made possible by implementing AlgorithmedPerformed interface
                FFTAlgo.addListener(this);

                // Hide dialog since the algorithm is about to run
                setVisible(false);

                // Start the thread as a low priority because we wish to still have user interface work fast.
                if (FFTAlgo.startMethod(Thread.MIN_PRIORITY) == false) {
                    MipavUtil.displayError("A thread is already running on this object");
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
                FFTAlgo = new AlgorithmFFT(image, transformDir, logMagDisplay, unequalDim, image25D, imageCrop,
                                           kernelDiameter, filterType, freq1, freq2, constructionMethod,
                                           butterworthOrder);

                // This is very important. Adding this object as a listener allows the algorithm to
                // notify this object when it has completed or failed. See algorithm performed event.
                // This is made possible by implementing AlgorithmedPerformed interface
                FFTAlgo.addListener(this);

                // Hide the dialog since the algorithm is about to run.
                setVisible(false);

                // These next lines set the titles in all frames where the source image is displayed to
                // "locked - " image name so as to indicate that the image is now read/write locked!
                // The image frames are disabled and then unregisted from the userinterface until the
                // algorithm has completed.
                Vector imageFrames = image.getImageFrameVector();
                titles = new String[imageFrames.size()];

                for (int i = 0; i < imageFrames.size(); i++) {
                    titles[i] = ((ViewJFrameBase) (imageFrames.elementAt(i))).getTitle();
                    ((ViewJFrameBase) (imageFrames.elementAt(i))).setTitle("Locked: " + titles[i]);
                    ((ViewJFrameBase) (imageFrames.elementAt(i))).setEnabled(false);
                    ((ViewJFrameBase) parentFrame).getUserInterface().unregisterFrame((Frame) (imageFrames.elementAt(i)));
                }

                // Start the thread as a low priority because we wish to still have user interface.
                if (FFTAlgo.startMethod(Thread.MIN_PRIORITY) == false) {
                    MipavUtil.displayError("A thread is already running on this object");
                }
            } catch (OutOfMemoryError x) {
                MipavUtil.displayError("Dialog FFT: unable to allocate enough memory");

                return;
            }
        }
    }

    /**
     * Initializes GUI dialog.
     */
    private void init() {
        setTitle("FFT");

        JPanel optionsPanel = new JPanel(new GridLayout(3, 1));
        optionsPanel.setBorder(buildTitledBorder("Options"));

        image25DCheckbox = new JCheckBox("Process each slice independently (2.5D)");
        image25DCheckbox.setFont(serif12);
        image25DCheckbox.setForeground(Color.black);

        if ((image.getNDims() == 3) && (image.getType() != ModelStorageBase.COMPLEX)) {
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

        logDisplayCheckbox = new JCheckBox("Display log magnitude");
        logDisplayCheckbox.setFont(serif12);
        logDisplayCheckbox.setForeground(Color.black);
        logDisplayCheckbox.setSelected(false);

        if (image.getType() != ModelStorageBase.COMPLEX) {
            logDisplayCheckbox.setEnabled(true);
        } else {
            logDisplayCheckbox.setEnabled(false);
        }

        logDisplayCheckbox.addActionListener(this);
        optionsPanel.add(logDisplayCheckbox);

        unequalDimCheckbox = new JCheckBox("Allow unequal FFT dimensions");
        unequalDimCheckbox.setFont(serif12);
        unequalDimCheckbox.setForeground(Color.black);

        if (image.getType() != ModelStorageBase.COMPLEX) {
            unequalDimCheckbox.setEnabled(true);
            unequalDimCheckbox.setSelected(false);
        } else {
            unequalDimCheckbox.setEnabled(false);
            unequalDimCheckbox.setSelected(image.getUnequalDim());
        }

        unequalDimCheckbox.addActionListener(this);
        optionsPanel.add(unequalDimCheckbox);

        constructionPanel = new JPanel(new GridBagLayout());
        constructionPanel.setBorder(buildTitledBorder("Filter construction methods"));

        constructionGroup = new ButtonGroup();
        windowFilter = new JRadioButton("Windowed finite impulse response", true);
        windowFilter.setFont(serif12);
        windowFilter.setForeground(Color.black);
        windowFilter.addActionListener(this);
        constructionGroup.add(windowFilter);

        if (image.getType() != ModelStorageBase.COMPLEX) {
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

        if (image.getType() != ModelStorageBase.COMPLEX) {
            imageCropCheckbox.setEnabled(true);
            imageCropCheckbox.setSelected(true);
        } else {
            imageCropCheckbox.setEnabled(false);
            imageCropCheckbox.setSelected(image.getOriginalCropCheckbox());
        }

        imageCropCheckbox.addActionListener(this);

        textKernelDiameter = new JTextField(10);

        if (image.getType() != ModelStorageBase.COMPLEX) {
            textKernelDiameter.setText("15");
        } else {
            kernelDiameter = image.getOriginalKernelDimension();
            textKernelDiameter.setText(String.valueOf(kernelDiameter));
        }

        textKernelDiameter.setFont(serif12);
        textKernelDiameter.setForeground(Color.black);

        if (image.getType() != ModelStorageBase.COMPLEX) {
            textKernelDiameter.setEnabled(true);
        } else {
            textKernelDiameter.setEnabled(false);
        }

        textKernelDiameter.addFocusListener(this);

        labelKernelDiameter = new JLabel("Convolution kernel diameter - odd");
        labelKernelDiameter.setForeground(Color.black);
        labelKernelDiameter.setFont(serif12);

        if (image.getType() != ModelStorageBase.COMPLEX) {
            labelKernelDiameter.setEnabled(true);
        } else {
            labelKernelDiameter.setEnabled(false);
        }

        gaussianFilter = new JRadioButton("Gaussian filter", false);
        gaussianFilter.setFont(serif12);
        gaussianFilter.setForeground(Color.black);
        gaussianFilter.addActionListener(this);
        constructionGroup.add(gaussianFilter);

        if (image.getType() != ModelStorageBase.COMPLEX) {
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

        if (image.getType() != ModelStorageBase.COMPLEX) {
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

        if (image.getType() != ModelStorageBase.COMPLEX) {
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

        directionPanel = new JPanel(new GridLayout(3, 1));
        directionPanel.setBorder(buildTitledBorder("FFT"));

        directionGroup = new ButtonGroup();
        forwardFFT = new JRadioButton("Forward FFT", true);
        forwardFFT.setForeground(Color.black);
        forwardFFT.setFont(serif12);
        forwardFFT.addActionListener(this);
        directionGroup.add(forwardFFT);
        directionPanel.add(forwardFFT);

        frequencyFilter = new JRadioButton("Frequency filter", false);
        frequencyFilter.setForeground(Color.black);
        frequencyFilter.setFont(serif12);
        frequencyFilter.addActionListener(this);
        directionGroup.add(frequencyFilter);
        directionPanel.add(frequencyFilter);

        inverseFFT = new JRadioButton("Inverse FFT", false);
        inverseFFT.setFont(serif12);
        inverseFFT.setForeground(Color.black);
        inverseFFT.addActionListener(this);
        directionGroup.add(inverseFFT);
        directionPanel.add(inverseFFT);

        if (image.getType() != ModelStorageBase.COMPLEX) {
            forwardFFT.setSelected(true);
            frequencyFilter.setEnabled(false);
            inverseFFT.setEnabled(false);
        } else {
            frequencyFilter.setEnabled(true);
            inverseFFT.setEnabled(true);
            inverseFFT.setSelected(true);
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
        lowPass.setEnabled(false);

        if (filterType == HIGHPASS) {
            highPass = new JRadioButton("Highpass", true);
        } else {
            highPass = new JRadioButton("Highpass", false);
        }

        highPass.setFont(serif12);
        highPass.addActionListener(this);
        highPass.setForeground(Color.black);
        filterTypeGroup.add(highPass);
        highPass.setEnabled(false);

        if (filterType == BANDPASS) {
            bandPass = new JRadioButton("Bandpass", true);
        } else {
            bandPass = new JRadioButton("Bandpass", false);
        }

        bandPass.setFont(serif12);
        bandPass.setForeground(Color.black);
        bandPass.addActionListener(this);
        filterTypeGroup.add(bandPass);
        bandPass.setEnabled(false);

        if (filterType == BANDSTOP) {
            bandStop = new JRadioButton("Bandstop", true);
        } else {
            bandStop = new JRadioButton("Bandstop", false);
        }

        bandStop.setForeground(Color.black);
        bandStop.setFont(serif12);
        bandStop.addActionListener(this);
        filterTypeGroup.add(bandStop);
        bandStop.setEnabled(false);

        textF1 = new JTextField(10);
        freq1 = image.getFreq1();
        textF1.setText(String.valueOf(freq1));
        textF1.setFont(serif12);
        textF1.setEnabled(false);

        labelF1 = new JLabel("Frequency F1 0.0 to 1.0   ");

        if ((image.getType() == ModelStorageBase.COMPLEX) && (constructionMethod == GAUSSIAN)) {
            labelF1.setText("Frequency F1 exceeds 0.0 ");
        } else if ((image.getType() == ModelStorageBase.COMPLEX) && (constructionMethod == BUTTERWORTH)) {
            labelF1.setText("Frequency F1 >0.0 to 1.0 ");
        }

        labelF1.setForeground(Color.black);
        labelF1.setFont(serif12);
        labelF1.setEnabled(false);

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
        filterPanel.add(lowPass, gbc);
        gbc.gridx = 1;
        filterPanel.add(labelF1, gbc);
        gbc.gridx = 2;
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

        buttonPanel = new JPanel();
        buildOKButton();
        buttonPanel.add(OKButton);
        buildCancelButton();
        buttonPanel.add(cancelButton);

        mainPanel = new JPanel(new GridBagLayout());
        gbc.gridx = 0;
        gbc.gridy = 0;
        gbc.gridwidth = 2;
        gbc.fill = GridBagConstraints.HORIZONTAL;
        mainPanel.add(optionsPanel, gbc);
        gbc.gridy = 1;
        mainPanel.add(constructionPanel, gbc);
        gbc.gridy = 2;
        gbc.gridwidth = 1;
        gbc.weighty = 1;
        gbc.fill = GridBagConstraints.BOTH;
        mainPanel.add(destinationPanel, gbc);
        gbc.gridx = 1;
        mainPanel.add(directionPanel, gbc);
        gbc.gridx = 0;
        gbc.gridy = 3;
        gbc.gridwidth = 2;
        gbc.fill = GridBagConstraints.HORIZONTAL;
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

        if (forwardFFT.isSelected()) {
            transformDir = FORWARD;

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
        } // end of if (forwardFFT.isSelected())
        else if (frequencyFilter.isSelected()) {
            transformDir = FILTER;

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

            }
        } else if (inverseFFT.isSelected()) {
            transformDir = INVERSE;
        }


        if (logDisplayCheckbox.isSelected()) {
            logMagDisplay = true;
        } else {
            logMagDisplay = false;
        }

        if (unequalDimCheckbox.isSelected()) {
            unequalDim = true;
        } else {
            unequalDim = false;
        }

        if (imageCropCheckbox.isSelected()) {
            imageCrop = true;
        } else {
            imageCrop = false;
        }

        return true;
    }
}
