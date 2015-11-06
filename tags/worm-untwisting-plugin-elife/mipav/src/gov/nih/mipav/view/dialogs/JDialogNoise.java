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

import java.util.*;

import javax.swing.*;


/**
 * DOCUMENT ME!
 *
 * @version  2.0 July 28, 2008
 * @author   GAUSSIAN and UNIFORM, Matthew J. McAuliffe, Ph.D.
 *           POISSON William Gandler
 */
public class JDialogNoise extends JDialogScriptableBase implements AlgorithmInterface, ItemListener, ActionDiscovery, ScriptableActionInterface {

    //~ Static fields/initializers -------------------------------------------------------------------------------------

    /** Use serialVersionUID for interoperability. */
    private static final long serialVersionUID = 3214808660601346651L;

    /** DOCUMENT ME! */
    public static final int GAUSSIAN = 0;
    
    public static final int POISSON = 1;

    /** DOCUMENT ME! */
    public static final int UNIFORM = 2;
    
    public static final int RAYLEIGH = 3;
    
    public static final int RICIAN = 4;

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** DOCUMENT ME! */
    private ButtonGroup destinationGroup;

    /** DOCUMENT ME! */
    private JPanel destinationPanel;

    /** DOCUMENT ME! */
    private int displayLoc; // Flag indicating if a new image is to be generated

    /** DOCUMENT ME! */
    private ButtonGroup group1;

    /** DOCUMENT ME! */
    private ModelImage image;

    /** DOCUMENT ME! */
    private JLabel imageRangeLabel;

    /** DOCUMENT ME! */
    private double maximumNoise = 0.0;

    /** DOCUMENT ME! */
    private double min, max;

    /** DOCUMENT ME! */
    private JRadioButton newImage;

    /** DOCUMENT ME! */
    private int noiseType;

    /** DOCUMENT ME! */
    private JPanel panelImageType;

    /** DOCUMENT ME! */
    private JPanel panelGU;
    
    private JPanel panelPO;
    
    private JPanel panelRayleigh;
    
    private JPanel panelRician;

    /** DOCUMENT ME! */
    private JRadioButton radioGaussian;

    /** DOCUMENT ME! */
    private JRadioButton radioUniform;
    
    private JRadioButton radioPoisson;
    
    private JRadioButton radioRayleigh;
    
    private JRadioButton radioRician;

    /** DOCUMENT ME! */
    private AlgorithmNoise randomAlgo;

    /** DOCUMENT ME! */
    private JRadioButton replaceImage;

    /** DOCUMENT ME! */
    private ModelImage resultImage = null; // result image

    /** DOCUMENT ME! */
    private JLabel maxNoiseLabel;
    
    private JLabel maxNoiseLabel2;
    
    private JTextField meanText;
    
    private double mean = 5.0;
    
    private JTextField gainText;
    
    private double gain = 1.0;
    
    private JTextField offsetText;
    
    private double offset = 0.0;

    /** DOCUMENT ME! */
    private JTextField textMaxNoise;
    
    private JTextField textMaxNoise2;

    /** DOCUMENT ME! */
    private String[] titles;

    /** DOCUMENT ME! */
    private ViewUserInterface userInterface;
    
    private JTextField sigmaText;
    
    private double sigma = 1.0;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Empty constructor needed for dynamic instantiation (used during scripting).
     */
    public JDialogNoise() { }

    /**
     * Constructor.
     *
     * @param  theParentFrame  parent frame
     * @param  _image          title of dialog frame
     */
    public JDialogNoise(Frame theParentFrame, ModelImage _image) {
        super(theParentFrame, false);
        image = _image;
        userInterface = ViewUserInterface.getReference();
        init();
    }

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * actionPerformed - closes dialog box when the OK button is pressed and sets the variables.
     *
     * @param  event  event that triggers this function
     */
    public void actionPerformed(ActionEvent event) {
        String command = event.getActionCommand();

        if (command.equals("OK")) {

            if (setVariables()) {
                callAlgorithm();
            }
        } else if (command.equals("Cancel")) {
            cancelFlag = true;
            dispose();
        } else if (command.equals("Help")) {
            //MipavUtil.showHelp("U4039");
            MipavUtil.showWebHelp("Adding_noise_to_images");
        } else { // else if (source == thresholdCheckbox)
            super.actionPerformed(event);
        }
    }

    // ************************************************************************
    // ************************** Algorithm Events ****************************
    // ************************************************************************

    /**
     * algorithmPerformed - this method is required if the AlgorithmPerformed interface is implemented. It is called by
     * the algorithms when it has completed or failed to to complete, so that the dialog can be display the result image
     * and/or clean up.
     *
     * @param  algorithm  algorithm that caused the event.
     */
    public void algorithmPerformed(AlgorithmBase algorithm) {

        if (algorithm instanceof AlgorithmNoise) {
            image.clearMask();

            if ((randomAlgo.isCompleted() == true) && (resultImage != null)) {
                updateFileInfo(image, resultImage);
                resultImage.clearMask();

                // The algorithm has completed and produced a new image to be displayed.
                try {

                    // resultImage.setImageName("Noise image");
                    new ViewJFrameImage(resultImage, null, new Dimension(610, 200));
                } catch (OutOfMemoryError error) {
                    MipavUtil.displayError("Out of memory: unable to open new frame");
                }
            } else if (resultImage == null) {

                // These next lines set the titles in all frames where the source image is displayed to
                // image name so as to indicate that the image is now unlocked!
                // The image frames are enabled and then registed to the userinterface.
                Vector<ViewImageUpdateInterface> imageFrames = image.getImageFrameVector();

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
        // Update frame
        // ((ViewJFrameBase)parentFrame).updateImages(true);

        if (algorithm.isCompleted()) {
            insertScriptLine();
        }
     // save the completion status for later
        setComplete(algorithm.isCompleted());
        randomAlgo.finalize();
        randomAlgo = null;
        dispose();
        System.gc();
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
     * Accessor that sets the noise level.
     *
     * @param  n  maximum noise excursion above and below zero
     */
    public void setNoiseLevel(double n) {
        maximumNoise = n;
    }

    /**
     * Accessor that sets the noise type.
     *
     * @param  n  noise level
     */
    public void setNoiseType(int n) {
        noiseType = n;
    }
    
    /**
     * 
     * @param mean
     */
    public void setMean(double mean) {
        this.mean = mean;
    }
    
    /**
     * 
     * @param gain
     */
    public void setGain(double gain) {
        this.gain = gain;
    }
    
    /**
     * 
     * @param offset
     */
    public void setOffset(double offset) {
        this.offset = offset;
    }
    
    public void setSigma(double sigma) {
    	this.sigma = sigma;
    }

    /**
     * Once all the necessary variables are set, call the Gaussian Blur algorithm based on what type of image this is
     * and whether or not there is a separate destination image.
     */
    protected void callAlgorithm() {

        String name = makeImageName(image.getImageName(), "_noise");
        int numberFrames = 1;
        if (image.getNDims() >= 3) {
            numberFrames *= image.getExtents()[2];
        }
        if (image.getNDims() >= 4) {
            numberFrames *= image.getExtents()[3];
        }    
        
        if (displayLoc == NEW) {

            try {

                // Make result image of float type
                resultImage = (ModelImage) image.clone();
                resultImage.setImageName(name);

                if ((resultImage.getFileInfo()[0]).getFileFormat() == FileUtility.DICOM) {

                    for (int i = 0; i < numberFrames; i++) {
                        ((FileInfoDicom) (resultImage.getFileInfo(i))).setSecondaryCaptureTags();
                    }
                }

                // Make algorithm
                randomAlgo = new AlgorithmNoise(resultImage, image, noiseType, maximumNoise, mean, gain, offset, sigma);

                // This is very important. Adding this object as a listener allows the algorithm to
                // notify this object when it has completed of failed. See algorithm performed event.
                // This is made possible by implementing AlgorithmedPerformed interface
                randomAlgo.addListener(this);

                createProgressBar(image.getImageName(), randomAlgo);

                // Hide dialog
                setVisible(false);

                if (isRunInSeparateThread()) {

                    // Start the thread as a low priority because we wish to still have user interface work fast
                    if (randomAlgo.startMethod(Thread.MIN_PRIORITY) == false) {
                        MipavUtil.displayError("A thread is already running on this object");
                    }
                } else {
                    randomAlgo.run();
                }
            } catch (OutOfMemoryError x) {

                if (resultImage != null) {
                    resultImage.disposeLocal(); // Clean up image memory
                    resultImage = null;
                }

                MipavUtil.displayError("Dialog random: unable to allocate enough memory");

                return;
            }
        } else {

            try {

                // Make algorithm
                randomAlgo = new AlgorithmNoise(image, noiseType, maximumNoise, mean, gain, offset, sigma);

                // This is very important. Adding this object as a listener allows the algorithm to
                // notify this object when it has completed of failed. See algorithm performed event.
                // This is made possible by implementing AlgorithmedPerformed interface
                randomAlgo.addListener(this);

                createProgressBar(image.getImageName(), randomAlgo);

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

                if (isRunInSeparateThread()) {

                    // Start the thread as a low priority because we wish to still have user interface work fast
                    if (randomAlgo.startMethod(Thread.MIN_PRIORITY) == false) {
                        MipavUtil.displayError("A thread is already running on this object");
                    }
                } else {
                    randomAlgo.run();
                }
            } catch (OutOfMemoryError x) {
                MipavUtil.displayError("Dialog random: unable to allocate enough memory");

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

        setNoiseType(scriptParameters.getParams().getInt("noise_type"));
        setNoiseLevel(scriptParameters.getParams().getDouble("maximum_noise"));
        setMean(scriptParameters.getParams().getDouble("poisson_mean"));
        setGain(scriptParameters.getParams().getDouble("poisson_gain"));
        setOffset(scriptParameters.getParams().getDouble("poisson_offset"));
        setSigma(scriptParameters.getParams().getDouble("rayleigh_sigma"));
    }

    /**
     * {@inheritDoc}
     */
    protected void storeParamsFromGUI() throws ParserException {
        scriptParameters.storeInputImage(image);
        scriptParameters.storeOutputImageParams(getResultImage(), (displayLoc == NEW));

        scriptParameters.getParams().put(ParameterFactory.newParameter("noise_type", noiseType));
        scriptParameters.getParams().put(ParameterFactory.newParameter("maximum_noise", maximumNoise));
        scriptParameters.getParams().put(ParameterFactory.newParameter("poisson_mean", mean));
        scriptParameters.getParams().put(ParameterFactory.newParameter("poisson_gain", gain));
        scriptParameters.getParams().put(ParameterFactory.newParameter("poisson_offset", offset));
        scriptParameters.getParams().put(ParameterFactory.newParameter("rayleigh_sigma", sigma));
    }


    /**
     * Sets up the GUI (panels, buttons, etc) and displays it on the screen.
     */
    private void init() {
        setForeground(Color.black);

        cancelFlag = false;
        GridBagConstraints gbc = new GridBagConstraints();
        int yPos = 0;
        gbc.gridwidth = 1;
        gbc.gridheight = 1;
        gbc.anchor = GridBagConstraints.WEST;
        gbc.weightx = 1;
        gbc.insets = new Insets(3, 3, 3, 3);
        gbc.fill = GridBagConstraints.HORIZONTAL;
        gbc.gridx = 0;
        gbc.gridy = yPos;
        
        
        getContentPane().setLayout(new GridBagLayout());
        setTitle("Noise");
        
        GridBagConstraints gbc2 = new GridBagConstraints();
        int yPos2 = 0;
        gbc2.gridwidth = 1;
        gbc2.gridheight = 1;
        gbc2.anchor = GridBagConstraints.WEST;
        gbc2.weightx = 1;
        gbc2.insets = new Insets(3, 3, 3, 3);
        gbc2.fill = GridBagConstraints.HORIZONTAL;
        gbc2.gridx = 0;
        gbc2.gridy = yPos2;
        
        panelGU = new JPanel(new GridBagLayout());
        panelGU.setForeground(Color.black);
        panelGU.setBorder(buildTitledBorder("Gaussian and Uniform"));
        gbc.gridy = yPos++;
        getContentPane().add(panelGU, gbc);
        
        JLabel plusMinusLabel = new JLabel("Image(i) = Image(i) +/- noise");
        plusMinusLabel.setFont(serif12);
        plusMinusLabel.setForeground(Color.black);
        gbc2.gridy = yPos2++;
        panelGU.add(plusMinusLabel, gbc2);

        maxNoiseLabel = new JLabel("Starting range (0 to end):  ");
        maxNoiseLabel.setFont(serif12);
        maxNoiseLabel.setForeground(Color.black);
        gbc2.gridy = yPos2;
        panelGU.add(maxNoiseLabel, gbc2);

        textMaxNoise = new JTextField(10);
        textMaxNoise.setText("0");
        textMaxNoise.setFont(serif12);
        textMaxNoise.addFocusListener(this);
        gbc2.gridx = 1;
        gbc2.gridy = yPos2++;
        panelGU.add(textMaxNoise, gbc2);

        JLabel gaussLabel = new JLabel("Maximum noise for Gaussian is at 4 standard deviations");
        gaussLabel.setFont(serif12);
        gaussLabel.setForeground(Color.black);
        gbc2.gridx = 0;
        gbc2.gridy = yPos2++;
        panelGU.add(gaussLabel, gbc2);

        imageRangeLabel = new JLabel("Image: ( min = " + image.getMin() + "  max = " + image.getMax() + " )");
        imageRangeLabel.setFont(serif12);
        imageRangeLabel.setForeground(Color.black);
        gbc2.gridy = yPos2++;
        panelGU.add(imageRangeLabel, gbc2);
        
        panelPO = new JPanel(new GridBagLayout());
        panelPO.setForeground(Color.black);
        panelPO.setBorder(buildTitledBorder("Poisson"));
        gbc.gridy = yPos++;
        getContentPane().add(panelPO, gbc);
        
        JLabel poissonLabel = new JLabel("Out = Gain * Poisson(mean) + Offset");
        poissonLabel.setFont(serif12);
        poissonLabel.setForeground(Color.black);
        gbc2.gridx = 0;
        yPos2 = 0;
        gbc2.gridy = yPos2++;
        panelPO.add(poissonLabel, gbc2);
        
        JLabel meanLabel = new JLabel("Mean ( > 0) ");
        meanLabel.setFont(serif12);
        meanLabel.setForeground(Color.black);
        gbc2.gridx = 0;
        gbc2.gridy = yPos2;
        panelPO.add(meanLabel, gbc2);
        
        meanText = new JTextField(10);
        meanText.setText("5.0");
        meanText.setFont(serif12);
        meanText.setForeground(Color.black);
        gbc2.gridx = 1;
        gbc2.gridy = yPos2++;
        panelPO.add(meanText, gbc2);
        
        JLabel gainLabel = new JLabel("Gain ");
        gainLabel.setFont(serif12);
        gainLabel.setForeground(Color.black);
        gbc2.gridx = 0;
        gbc2.gridy = yPos2;
        panelPO.add(gainLabel, gbc2);
        
        gainText = new JTextField(10);
        gainText.setText("1.0");
        gainText.setFont(serif12);
        gainText.setForeground(Color.black);
        gbc2.gridx = 1;
        gbc2.gridy = yPos2++;
        panelPO.add(gainText, gbc2);
        
        JLabel offsetLabel = new JLabel("Offset ");
        offsetLabel.setFont(serif12);
        offsetLabel.setForeground(Color.black);
        gbc2.gridx = 0;
        gbc2.gridy = yPos2;
        panelPO.add(offsetLabel, gbc2);
        
        offsetText = new JTextField(10);
        offsetText.setText("0.0");
        offsetText.setFont(serif12);
        offsetText.setForeground(Color.black);
        gbc2.gridx = 1;
        gbc2.gridy = yPos2++;
        panelPO.add(offsetText, gbc2);
        
        panelRayleigh = new JPanel(new GridBagLayout());
        panelRayleigh.setForeground(Color.black);
        panelRayleigh.setBorder(buildTitledBorder("Rayleigh"));
        gbc.gridy = yPos++;
        getContentPane().add(panelRayleigh, gbc);
        
        JLabel rayleighLabel = new JLabel("Image(i) = Image(i) + sigma*sqrt(-2*ln(U))");
        rayleighLabel.setFont(serif12);
        rayleighLabel.setForeground(Color.black);
        gbc2.gridx = 0;
        gbc2.gridy = 0;
        panelRayleigh.add(rayleighLabel, gbc2);
        
        JLabel rayleighLabel2 = new JLabel("where U is uniformly distributed from Double.MIN_VALUE to 1");
        rayleighLabel2.setFont(serif12);
        rayleighLabel2.setForeground(Color.black);
        gbc2.gridx = 0;
        gbc2.gridy = 1;
        panelRayleigh.add(rayleighLabel2, gbc2);
        
        JLabel sigmaLabel = new JLabel("Sigma");
        sigmaLabel.setFont(serif12);
        sigmaLabel.setForeground(Color.black);
        gbc2.gridx = 0;
        gbc2.gridy = 2;
        panelRayleigh.add(sigmaLabel, gbc2);
        
        sigmaText = new JTextField(10);
        sigmaText.setText("1.0");
        sigmaText.setFont(serif12);
        sigmaText.setForeground(Color.black);
        gbc2.gridx = 1;
        gbc2.gridy = 2;
        panelRayleigh.add(sigmaText, gbc2);
        
        panelRician = new JPanel(new GridBagLayout());
        panelRician.setForeground(Color.black);
        panelRician.setBorder(buildTitledBorder("Rician"));
        gbc.gridy = yPos++;
        getContentPane().add(panelRician, gbc);
        
        JLabel plusMinusLabel2 = new JLabel("Image(i) = sqrt((Image(i) +/- noise)**2 + noise**2");
        plusMinusLabel2.setFont(serif12);
        plusMinusLabel2.setForeground(Color.black);
        gbc2.gridx = 0;
        gbc2.gridy = 0;
        panelRician.add(plusMinusLabel2, gbc2);
        
        JLabel gLabel = new JLabel("with 2 zero mean equal standard deviation gaussian distributions");
        gLabel.setFont(serif12);
        gLabel.setForeground(Color.black);
        gbc2.gridx = 0;
        gbc2.gridy = 1;
        panelRician.add(gLabel, gbc2);
        
        maxNoiseLabel2 = new JLabel("Starting range (0 to end):  ");
        maxNoiseLabel2.setFont(serif12);
        maxNoiseLabel2.setForeground(Color.black);
        gbc2.gridy = 2;
        panelRician.add(maxNoiseLabel2, gbc2);

        textMaxNoise2 = new JTextField(10);
        textMaxNoise2.setText("0");
        textMaxNoise2.setFont(serif12);
        textMaxNoise2.addFocusListener(this);
        gbc2.gridx = 1;
        gbc2.gridy = 2;
        panelRician.add(textMaxNoise2, gbc2);

        JLabel gaussLabel2 = new JLabel("Maximum noise for Gaussian is at 4 standard deviations");
        gaussLabel2.setFont(serif12);
        gaussLabel2.setForeground(Color.black);
        gbc2.gridx = 0;
        gbc2.gridy = 3;
        panelRician.add(gaussLabel2, gbc2);

        JPanel outputOptPanel = new JPanel(new GridLayout(1, 2));
        panelImageType = new JPanel(new GridBagLayout());
        panelImageType.setForeground(Color.black);
        panelImageType.setBorder(buildTitledBorder("Noise Type"));
        outputOptPanel.add(panelImageType);
        
        GridBagConstraints gbc3 = new GridBagConstraints();
        gbc3.gridwidth = 1;
        gbc3.gridheight = 1;
        gbc3.anchor = GridBagConstraints.WEST;
        gbc3.weightx = 1;
        gbc3.insets = new Insets(3, 3, 3, 3);
        gbc3.fill = GridBagConstraints.HORIZONTAL;

        group1 = new ButtonGroup();
        radioGaussian = new JRadioButton("Gaussian", true);
        radioGaussian.setFont(serif12);
        radioGaussian.addItemListener(this);
        group1.add(radioGaussian);
        gbc3.gridx = 0;
        gbc3.gridy = 0;
        panelImageType.add(radioGaussian, gbc3);
        
        radioPoisson = new JRadioButton("Poisson", false);
        radioPoisson.setFont(serif12);
        radioPoisson.addItemListener(this);
        group1.add(radioPoisson);
        gbc3.gridy = 1;
        panelImageType.add(radioPoisson, gbc3);

        radioUniform = new JRadioButton("Uniform", false);
        radioUniform.setFont(serif12);
        radioUniform.addItemListener(this);
        group1.add(radioUniform);
        gbc3.gridy = 2;
        panelImageType.add(radioUniform, gbc3);
        
        radioRayleigh = new JRadioButton("Rayleigh", false);
        radioRayleigh.setFont(serif12);
        radioRayleigh.addItemListener(this);
        group1.add(radioRayleigh);
        gbc3.gridy = 3;
        panelImageType.add(radioRayleigh, gbc3);
        
        radioRician = new JRadioButton("Rician", false);
        radioRician.setFont(serif12);
        radioRician.addItemListener(this);
        group1.add(radioRician);
        gbc3.gridy = 4;
        panelImageType.add(radioRician, gbc3);

        destinationPanel = new JPanel(new BorderLayout());
        destinationPanel.setForeground(Color.black);
        destinationPanel.setBorder(buildTitledBorder("Destination"));
        outputOptPanel.add(destinationPanel);

        destinationGroup = new ButtonGroup();
        newImage = new JRadioButton("New image", true);
        newImage.setFont(serif12);
        destinationGroup.add(newImage);
        destinationPanel.add(newImage, BorderLayout.NORTH);

        replaceImage = new JRadioButton("Replace image", false);
        replaceImage.setFont(serif12);
        destinationGroup.add(replaceImage);
        destinationPanel.add(replaceImage, BorderLayout.CENTER);

        if (image.getLockStatus() == ModelStorageBase.UNLOCKED) {
            replaceImage.setEnabled(true);
        } else {
            replaceImage.setEnabled(false);
        }

        setRange();

        JPanel buttonPanel = new JPanel();

        /*
         * buildOKButton(); buttonPanel.add(OKButton); buildCancelButton(); buttonPanel.add(cancelButton);
         */
        buttonPanel.add(buildButtons());

        gbc.gridy = yPos++;
        getContentPane().add(outputOptPanel, gbc);
        gbc.gridy = yPos++;
        getContentPane().add(buttonPanel, gbc);
        pack();
        setResizable(true);
        setVisible(true);

        System.gc();
    }

    /**
     * setRange.
     */
    private void setRange() {

        maxNoiseLabel.setEnabled(true);
        textMaxNoise.setEnabled(true);
        maxNoiseLabel2.setEnabled(true);
        textMaxNoise2.setEnabled(true);

        double imageRange = image.getMax() - image.getMin();
        double noiseMax = imageRange;
        double noiseStart = imageRange / 20.0;

        if ((image.getType() != ModelStorageBase.FLOAT) && (image.getType() != ModelStorageBase.DOUBLE)) {
            maxNoiseLabel.setText("Maximum noise (0 - " + Math.round(noiseMax) + "):  ");
            textMaxNoise.setText(String.valueOf(Math.round(noiseStart)));
            maxNoiseLabel2.setText("Maximum noise (0 - " + Math.round(noiseMax) + "):  ");
            textMaxNoise2.setText(String.valueOf(Math.round(noiseStart)));
            min = 0;
            max = Math.round(noiseMax);
        } else {
            maxNoiseLabel.setText("Maximum noise (0 - " + noiseMax + "):  ");
            textMaxNoise.setText(String.valueOf(noiseStart));
            maxNoiseLabel2.setText("Maximum noise (0 - " + noiseMax + "):  ");
            textMaxNoise2.setText(String.valueOf(noiseStart));
            min = 0;
            max = noiseMax;
        }
    }

    /**
     * Use the GUI results to set up the variables needed to run the algorithm.
     *
     * @return  <code>true</code> if parameters set successfully, <code>false</code> otherwise.
     */
    private boolean setVariables() {
        String tmpStr;

        if (radioGaussian.isSelected()) {
            noiseType = GAUSSIAN;
        } else if (radioPoisson.isSelected()) {
            noiseType = POISSON;
        } else if (radioUniform.isSelected()) {
            noiseType = UNIFORM;
        } else if (radioRayleigh.isSelected()) {
        	noiseType = RAYLEIGH;
        } else if (radioRician.isSelected()) {
        	noiseType = RICIAN;
        }

        if (replaceImage.isSelected()) {
            displayLoc = REPLACE;
        } else if (newImage.isSelected()) {
            displayLoc = NEW;
        }

        if ((noiseType == GAUSSIAN) || (noiseType == UNIFORM)) {
            tmpStr = textMaxNoise.getText();
            if (testParameter(tmpStr, min, max)) {
                maximumNoise = Double.valueOf(tmpStr).doubleValue();
            } else {
                textMaxNoise.requestFocus();
                textMaxNoise.selectAll();
    
                return false;
            }
        } // if ((noiseType == GAUSSIAN) || (noiseType == UNIFORM))
        
        if (noiseType == POISSON) {
            tmpStr = meanText.getText();
            mean = Double.valueOf(tmpStr).doubleValue();
            if (mean <= 0.0) {
                MipavUtil.displayError("Mean must be greater than zero");
                meanText.requestFocus();
                meanText.selectAll();
                return false;
            }
            tmpStr = gainText.getText();
            gain = Double.valueOf(tmpStr).doubleValue();
            tmpStr = offsetText.getText();
            offset = Double.valueOf(tmpStr).doubleValue();
        } // if (noiseType == POISSON)
        
        if (noiseType == RAYLEIGH) {
        	tmpStr = sigmaText.getText();
        	sigma = Double.valueOf(tmpStr).doubleValue();
        	if (sigma <= 0.0) {
                MipavUtil.displayError("Sigma must be greater than zero");
                sigmaText.requestFocus();
                sigmaText.selectAll();
                return false;
            }
        } // if (noiseType == RAYLEIGH)
        
        if (noiseType == RICIAN) {
        	tmpStr = textMaxNoise2.getText();
            if (testParameter(tmpStr, min, max)) {
                maximumNoise = Double.valueOf(tmpStr).doubleValue();
            } else {
                textMaxNoise2.requestFocus();
                textMaxNoise2.selectAll();
    
                return false;
            }    	
        } // if (noiseType == RICIAN)

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
                return new String("Utilities");
            }

            public String getDescription() {
                return new String("Adds noise to an image.");
            }

            public String getDescriptionLong() {
                return new String("Adds noise to an image.");
            }

            public String getShortLabel() {
                return new String("Noise");
            }

            public String getLabel() {
                return new String("Noise");
            }

            public String getName() {
                return new String("Noise");
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
            table.put(new ParameterInt("noise_type", 0));
            table.put(new ParameterDouble("maximum_noise", 148));
            table.put(new ParameterDouble("poisson_mean", 5));
            table.put(new ParameterDouble("poisson_gain", 1));
            table.put(new ParameterDouble("poisson_offset", 0));
            table.put(new ParameterDouble("rayleigh_sigma", 1.0));
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
