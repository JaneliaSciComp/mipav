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
 * @version  2.0 July 25, 2008
 * @author   GAUSSIAN and UNIFORM, Matthew J. McAuliffe, Ph.D.
 *           POISSON William Gandler
 */
public class JDialogNoise extends JDialogScriptableBase implements AlgorithmInterface, ItemListener {

    //~ Static fields/initializers -------------------------------------------------------------------------------------

    /** Use serialVersionUID for interoperability. */
    private static final long serialVersionUID = 3214808660601346651L;

    /** DOCUMENT ME! */
    public static final int GAUSSIAN = 0;
    
    public static final int POISSON = 1;

    /** DOCUMENT ME! */
    public static final int UNIFORM = 2;
    
    

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

    /** DOCUMENT ME! */
    private JRadioButton radioGaussian;

    /** DOCUMENT ME! */
    private JRadioButton radioUniform;
    
    private JRadioButton radioPoisson;

    /** DOCUMENT ME! */
    private AlgorithmNoise randomAlgo;

    /** DOCUMENT ME! */
    private JRadioButton replaceImage;

    /** DOCUMENT ME! */
    private ModelImage resultImage = null; // result image

    /** DOCUMENT ME! */
    private JLabel start;
    
    private JTextField backgroundText;
    
    private double background = 0.0;
    
    private JTextField gainText;
    
    private double gain = 1.0;

    /** DOCUMENT ME! */
    private JTextField textStart;

    /** DOCUMENT ME! */
    private String[] titles;

    /** DOCUMENT ME! */
    private ViewUserInterface userInterface;

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
            MipavUtil.showHelp("U4039");
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
        // Update frame
        // ((ViewJFrameBase)parentFrame).updateImages(true);

        if (algorithm.isCompleted()) {
            insertScriptLine();
        }

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
     * @param background
     */
    public void setBackground(double background) {
        this.background = background;
    }
    
    /**
     * 
     * @param gain
     */
    public void setGain(double gain) {
        this.gain = gain;
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
                randomAlgo = new AlgorithmNoise(resultImage, image, noiseType, maximumNoise, background, gain);

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
                randomAlgo = new AlgorithmNoise(image, noiseType, maximumNoise, background, gain);

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
        setNoiseLevel(scriptParameters.getParams().getDouble("starting_range"));
        setBackground(scriptParameters.getParams().getDouble("poisson_background"));
        setGain(scriptParameters.getParams().getDouble("poisson_gain"));
    }

    /**
     * {@inheritDoc}
     */
    protected void storeParamsFromGUI() throws ParserException {
        scriptParameters.storeInputImage(image);
        scriptParameters.storeOutputImageParams(getResultImage(), (displayLoc == NEW));

        scriptParameters.getParams().put(ParameterFactory.newParameter("noise_type", noiseType));
        scriptParameters.getParams().put(ParameterFactory.newParameter("starting_range", min));
        scriptParameters.getParams().put(ParameterFactory.newParameter("poisson_background", background));
        scriptParameters.getParams().put(ParameterFactory.newParameter("poisson_gain", gain));
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

        start = new JLabel("Starting range (0 to end):  ");
        start.setFont(serif12);
        start.setForeground(Color.black);
        gbc2.gridy = yPos2;
        panelGU.add(start, gbc2);

        textStart = new JTextField(10);
        textStart.setText("0");
        textStart.setFont(serif12);
        textStart.addFocusListener(this);
        gbc2.gridx = 1;
        gbc2.gridy = yPos2++;
        panelGU.add(textStart, gbc2);

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
        
        JLabel poissonLabel = new JLabel("Out = Poisson((in + background)*gain) / gain");
        poissonLabel.setFont(serif12);
        poissonLabel.setForeground(Color.black);
        gbc2.gridx = 0;
        yPos2 = 0;
        gbc2.gridy = yPos2++;
        panelPO.add(poissonLabel, gbc2);
        
        JLabel backgroundLabel = new JLabel("Background");
        backgroundLabel.setFont(serif12);
        backgroundLabel.setForeground(Color.black);
        gbc2.gridy = yPos2;
        panelPO.add(backgroundLabel, gbc2);
        
        backgroundText = new JTextField(10);
        backgroundText.setText("0.0");
        backgroundText.setFont(serif12);
        backgroundText.setForeground(Color.black);
        gbc2.gridx = 1;
        gbc2.gridy = yPos2++;
        panelPO.add(backgroundText, gbc2);
        
        JLabel gainLabel = new JLabel("Gain");
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

        JPanel outputOptPanel = new JPanel(new GridLayout(1, 2));
        panelImageType = new JPanel(new BorderLayout());
        panelImageType.setForeground(Color.black);
        panelImageType.setBorder(buildTitledBorder("Noise Type"));
        outputOptPanel.add(panelImageType);

        group1 = new ButtonGroup();
        radioGaussian = new JRadioButton("Gaussian", true);
        radioGaussian.setFont(serif12);
        radioGaussian.addItemListener(this);
        group1.add(radioGaussian);
        panelImageType.add(radioGaussian, BorderLayout.NORTH);
        
        radioPoisson = new JRadioButton("Poisson", false);
        radioPoisson.setFont(serif12);
        radioPoisson.addItemListener(this);
        group1.add(radioPoisson);
        panelImageType.add(radioPoisson, BorderLayout.CENTER);

        radioUniform = new JRadioButton("Uniform", false);
        radioUniform.setFont(serif12);
        radioUniform.addItemListener(this);
        group1.add(radioUniform);
        panelImageType.add(radioUniform, BorderLayout.SOUTH);

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

        start.setEnabled(true);
        textStart.setEnabled(true);

        double imageRange = image.getMax() - image.getMin();
        double noiseMax = imageRange;
        double noiseStart = imageRange / 20.0;

        if ((image.getType() != ModelStorageBase.FLOAT) && (image.getType() != ModelStorageBase.DOUBLE)) {
            start.setText("Maximum noise (0 - " + Math.round(noiseMax) + "):  ");
            textStart.setText(String.valueOf(Math.round(noiseStart)));
            min = 0;
            max = Math.round(noiseMax);
        } else {
            start.setText("Maximum noise (0 - " + noiseMax + "):  ");
            textStart.setText(String.valueOf(noiseStart));
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
        }

        if (replaceImage.isSelected()) {
            displayLoc = REPLACE;
        } else if (newImage.isSelected()) {
            displayLoc = NEW;
        }

        if ((noiseType == GAUSSIAN) || (noiseType == UNIFORM)) {
            tmpStr = textStart.getText();
            if (testParameter(tmpStr, min, max)) {
                maximumNoise = Double.valueOf(tmpStr).doubleValue();
            } else {
                textStart.requestFocus();
                textStart.selectAll();
    
                return false;
            }
        } // if ((noiseType == GAUSSIAN) || (noiseType == UNIFORM))
        
        if (noiseType == POISSON) {
            tmpStr = backgroundText.getText();
            background = Double.valueOf(tmpStr).doubleValue();
            tmpStr = gainText.getText();
            gain = Double.valueOf(tmpStr).doubleValue();
        } // if (noiseType == POISSON)

        return true;
    }

}
