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
 * Dialog to get user input, then call the algorithm. The user is able to control the degree of blurring in x and y
 * dimensions. The user also specifies the the normalization constant and the number of iterations. The user has the
 * option to use a threshold level. The user has the option to generate a new image or replace the source image. It
 * should be noted that the algorithms are executed in their own thread.
 *
 * @version  0.1 March 2, 2005
 * @author   William Gandler
 * @see      AlgorithmMRIShadingCorrection
 */
public class JDialogMRIShadingCorrection extends JDialogScriptableBase
        implements AlgorithmInterface {

    //~ Static fields/initializers -------------------------------------------------------------------------------------

    /** Use serialVersionUID for interoperability. */
    private static final long serialVersionUID = 2132712619224805730L;

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** DOCUMENT ME! */
    private ButtonGroup destinationGroup;


    /** DOCUMENT ME! */
    private JPanel destinationPanel;

    /** DOCUMENT ME! */
    private int displayLoc; // Flag indicating if a new image is to be generated

    /** DOCUMENT ME! */
    private ModelImage image; // source image

    /** DOCUMENT ME! */
    private int iters;

    /** DOCUMENT ME! */
    private JLabel labelGaussX;

    /** DOCUMENT ME! */
    private JLabel labelGaussY;

    /** DOCUMENT ME! */
    private JLabel labelIters;

    /** DOCUMENT ME! */
    private JLabel labelNorm;

    /** DOCUMENT ME! */
    private AlgorithmMRIShadingCorrection mAlgo;

    /** DOCUMENT ME! */
    private JRadioButton newImage;

    /** DOCUMENT ME! */
    private float norm;

    /** DOCUMENT ME! */
    private JPanel paramPanel;

    /** DOCUMENT ME! */
    private JRadioButton replaceImage;

    /** DOCUMENT ME! */
    private ModelImage resultImage = null; // result image

    /** DOCUMENT ME! */
    private float scaleX;

    /** DOCUMENT ME! */
    private float scaleY;

    /** DOCUMENT ME! */
    private JTextField textGaussX;

    /** DOCUMENT ME! */
    private JTextField textGaussY;

    /** DOCUMENT ME! */
    private JTextField textIters;

    /** DOCUMENT ME! */
    private JTextField textNorm;

    /** DOCUMENT ME! */
    private JCheckBox thresholdCheckbox;

    /** DOCUMENT ME! */
    private JLabel thresholdLabel;

    /** DOCUMENT ME! */
    private float thresholdLevel;

    /** DOCUMENT ME! */
    private boolean thresholdSelected = true;

    /** DOCUMENT ME! */
    private JTextField thresholdText;

    /** DOCUMENT ME! */
    private String[] titles;

    /** DOCUMENT ME! */
    private ViewUserInterface userInterface;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Empty constructor needed for dynamic instantiation.
     */
    public JDialogMRIShadingCorrection() { }

    // or if the source image is to be replaced

    /**
     * Creates a new JDialogMRIShadingCorrection object.
     *
     * @param  theParentFrame  Parent frame.
     * @param  im              Source image.
     */
    public JDialogMRIShadingCorrection(Frame theParentFrame, ModelImage im) {
        super(theParentFrame, false);
        image = im;
        userInterface = ViewUserInterface.getReference();
        init();
        loadDefaults();
        setVisible(true);
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
        } else if (command.equals("Cancel")) {
            dispose();
        } else if (command.equals("Help")) {
            // MipavUtil.showHelp( "" );
        } else if (source == thresholdCheckbox) {

            if (thresholdCheckbox.isSelected()) {
                thresholdLabel.setEnabled(true);
                thresholdText.setEnabled(true);
            } else {
                thresholdLabel.setEnabled(false);
                thresholdText.setEnabled(false);
            }
        } else { // else if (source == thresholdCheckbox)
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

        if (algorithm instanceof AlgorithmMRIShadingCorrection) {
            image.clearMask();

            if ((mAlgo.isCompleted() == true) && (resultImage != null)) {
                // The algorithm has completed and produced a new image to be displayed.

                updateFileInfo(image, resultImage);

                try {

                    // resultImage.setImageName("Unsharp mask");
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

        mAlgo.finalize();
        mAlgo = null;
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
     * Accessor that sets the number of iterations.
     *
     * @param  iters  int
     */
    public void setIters(int iters) {
        this.iters = iters;
    }

    /**
     * Accessor that sets the normalization constant.
     *
     * @param  norm  float
     */
    public void setNorm(float norm) {
        this.norm = norm;
    }

    /**
     * Accessor that sets the x scale.
     *
     * @param  scale  Value to set x scale to (should be between 0.5 and 5.0).
     */
    public void setScaleX(float scale) {
        scaleX = scale;
    }

    /**
     * Accessor that sets the y scale.
     *
     * @param  scale  Value to set y scale to (should be between 0.5 and 5.0).
     */
    public void setScaleY(float scale) {
        scaleY = scale;
    }

    /**
     * Accessor that sets the thresholdLevel variable.
     *
     * @param  thresholdLevel  float
     */
    public void setThresholdLevel(float thresholdLevel) {
        this.thresholdLevel = thresholdLevel;
    }

    /**
     * Accessor that sets the thresholdSelected variable.
     *
     * @param  thresholdSelected  boolean
     */
    public void setThresholdSelected(boolean thresholdSelected) {
        this.thresholdSelected = thresholdSelected;
    }

    /**
     * Once all the necessary variables are set, call the Entropy Minimization algorithm based on what type of image
     * this is and whether or not there is a separate destination image.
     */
    protected void callAlgorithm() {
        String name = makeImageName(image.getImageName(), "_mriShadingCorr");

        if (displayLoc == NEW) {

            try {

                // Make result image of float type
                // resultImage = new ModelImage(ModelImage.FLOAT, destExtents, name, userInterface);
                resultImage = (ModelImage) image.clone();
                resultImage.setImageName(name);
                resultImage.resetVOIs();

                /*if ((resultImage.getFileInfo()[0]).getFileFormat() == FileUtility.DICOM){
                 *  ((FileInfoDicom)(resultImage.getFileInfo(0))).setSecondaryCaptureTags();
                 *  } */
                // Make algorithm
                mAlgo = new AlgorithmMRIShadingCorrection(resultImage, image, norm, scaleX, scaleY, iters,
                                                          thresholdSelected, thresholdLevel);

                // This is very important. Adding this object as a listener allows the algorithm to
                // notify this object when it has completed of failed. See algorithm performed event.
                // This is made possible by implementing AlgorithmedPerformed interface
                mAlgo.addListener(this);

                createProgressBar(image.getImageName(), mAlgo);

                // Hide dialog
                setVisible(false);

                if (isRunInSeparateThread()) {

                    // Start the thread as a low priority because we wish to still have user interface work fast.
                    if (mAlgo.startMethod(Thread.MIN_PRIORITY) == false) {
                        MipavUtil.displayError("A thread is already running on this object");
                    }
                } else {
                    mAlgo.run();
                }
            } catch (OutOfMemoryError x) {
                MipavUtil.displayError("Dialog MRI Shading Correction: unable to allocate enough memory");

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
                mAlgo = new AlgorithmMRIShadingCorrection(image, norm, scaleX, scaleY, iters, thresholdSelected,
                                                          thresholdLevel);

                // This is very important. Adding this object as a listener allows the algorithm to
                // notify this object when it has completed of failed. See algorithm performed event.
                // This is made possible by implementing AlgorithmedPerformed interface
                mAlgo.addListener(this);

                createProgressBar(image.getImageName(), mAlgo);

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
                    if (mAlgo.startMethod(Thread.MIN_PRIORITY) == false) {
                        MipavUtil.displayError("A thread is already running on this object");
                    }
                } else {
                    mAlgo.run();
                }
            } catch (OutOfMemoryError x) {
                MipavUtil.displayError("Dialog MRI Shading Correction: unable to allocate enough memory");

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

        if (scriptParameters.doOutputNewImage()) {
            setDisplayLocNew();
        } else {
            setDisplayLocReplace();
        }

        setNorm(scriptParameters.getParams().getFloat("normalization_const"));

        float[] sigmas = scriptParameters.getUnnormalizedSigmas();
        setScaleX(sigmas[0]);
        setScaleY(sigmas[1]);
        setIters(scriptParameters.getNumIterations());
        setThresholdSelected(scriptParameters.getParams().getBoolean("do_periphery_threshold"));
        setThresholdLevel(scriptParameters.getParams().getFloat("periphery_threshold_level"));
    }

    /**
     * {@inheritDoc}
     */
    protected void storeParamsFromGUI() throws ParserException {
        scriptParameters.storeInputImage(image);
        scriptParameters.storeOutputImageParams(getResultImage(), (displayLoc == NEW));

        scriptParameters.getParams().put(ParameterFactory.newParameter("normalization_const", norm));
        scriptParameters.getParams().put(ParameterFactory.newParameter(AlgorithmParameters.SIGMAS,
                                                                       new float[] { scaleX, scaleY }));
        scriptParameters.storeNumIterations(iters);
        scriptParameters.getParams().put(ParameterFactory.newParameter("do_periphery_threshold", thresholdSelected));
        scriptParameters.getParams().put(ParameterFactory.newParameter("periphery_threshold_level", thresholdLevel));
    }

    /**
     * Sets up the GUI (panels, buttons, etc) and displays it on the screen.
     */
    private void init() {
        setForeground(Color.black);

        setTitle("MRI Shading Correction");
        getContentPane().setLayout(new BorderLayout());

        JPanel mainPanel;

        mainPanel = new JPanel();
        mainPanel.setBorder(BorderFactory.createEmptyBorder(3, 3, 3, 3));
        mainPanel.setLayout(new GridBagLayout());

        GridBagConstraints gbc = new GridBagConstraints();

        gbc.gridwidth = 6;
        gbc.gridheight = 1;
        gbc.anchor = GridBagConstraints.WEST;
        gbc.weightx = 1;
        gbc.insets = new Insets(3, 3, 3, 3);
        gbc.gridx = 0;
        gbc.gridy = 0;
        gbc.fill = GridBagConstraints.HORIZONTAL;

        paramPanel = new JPanel(new GridBagLayout());
        paramPanel.setForeground(Color.black);
        paramPanel.setBorder(buildTitledBorder("Parameters"));

        labelNorm = new JLabel("Normalization constant ");
        labelNorm.setForeground(Color.black);
        labelNorm.setFont(serif12);
        paramPanel.add(labelNorm, gbc);
        textNorm = new JTextField();
        textNorm.setText("0.02");
        textNorm.setFont(serif12);
        gbc.gridx = 6;
        paramPanel.add(textNorm, gbc);

        labelGaussX = new JLabel("X gaussian std. dev. (0.5 - 5.0) ");
        labelGaussX.setForeground(Color.black);
        labelGaussX.setFont(serif12);
        gbc.gridx = 0;
        gbc.gridy = 1;
        paramPanel.add(labelGaussX, gbc);
        textGaussX = new JTextField();
        textGaussX.setText("1.0");
        textGaussX.setFont(serif12);
        gbc.gridx = 6;
        paramPanel.add(textGaussX, gbc);

        labelGaussY = new JLabel("Y gaussian std. dev. (0.5 - 5.0) ");
        labelGaussY.setForeground(Color.black);
        labelGaussY.setFont(serif12);
        gbc.gridx = 0;
        gbc.gridy = 2;
        paramPanel.add(labelGaussY, gbc);
        textGaussY = new JTextField();
        textGaussY.setText("1.0");
        textGaussY.setFont(serif12);
        gbc.gridx = 6;
        paramPanel.add(textGaussY, gbc);

        labelIters = new JLabel("Number of iterations ");
        labelIters.setForeground(Color.black);
        labelIters.setFont(serif12);
        gbc.gridx = 0;
        gbc.gridy = 3;
        paramPanel.add(labelIters, gbc);
        textIters = new JTextField();
        textIters.setText("30");
        textIters.setFont(serif12);
        gbc.gridx = 6;
        paramPanel.add(textIters, gbc);

        gbc.gridwidth = 12;
        thresholdCheckbox = new JCheckBox("Threshold to exclude periphery pixels");
        thresholdCheckbox.setFont(serif12);
        thresholdCheckbox.setForeground(Color.black);
        thresholdCheckbox.setEnabled(true);
        thresholdCheckbox.setSelected(true);
        thresholdCheckbox.addActionListener(this);
        gbc.gridx = 0;
        gbc.gridy = 4;
        paramPanel.add(thresholdCheckbox, gbc);

        thresholdLabel = new JLabel("Threshold level");
        thresholdLabel.setForeground(Color.black);
        thresholdLabel.setFont(serif12);
        thresholdLabel.setEnabled(true);
        gbc.gridy = 5;
        gbc.gridwidth = 1;
        paramPanel.add(Box.createHorizontalStrut(5), gbc);
        gbc.gridwidth = 5;
        gbc.gridx = 1;
        paramPanel.add(thresholdLabel, gbc);

        image.calcMinMax();
        thresholdLevel = (float) (image.getMin() + 1.0);
        thresholdText = new JTextField(10);
        thresholdText.setText(String.valueOf(thresholdLevel));
        thresholdText.setFont(serif12);
        thresholdText.setForeground(Color.black);
        thresholdText.setEnabled(true);
        gbc.gridwidth = 6;
        gbc.gridx = 6;
        paramPanel.add(thresholdText, gbc);
        gbc.gridwidth = 12;


        destinationPanel = new JPanel(new BorderLayout());
        destinationPanel.setForeground(Color.black);
        destinationPanel.setBorder(buildTitledBorder("Destination"));

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
        if (image.getLockStatus() == ModelStorageBase.UNLOCKED) {
            replaceImage.setEnabled(true);
        } else {
            replaceImage.setEnabled(false);
        }

        gbc.gridx = 0;
        gbc.gridy = 0;
        mainPanel.add(paramPanel, gbc);
        gbc.gridx = 0;
        gbc.gridy = 1;
        mainPanel.add(destinationPanel, gbc);

        getContentPane().add(mainPanel, BorderLayout.CENTER);
        getContentPane().add(buildButtons(), BorderLayout.SOUTH);
        pack();
        setResizable(true);

        // setVisible( true );

        System.gc();
    }


    // *******************************************************************
    // ************************* Item Events ****************************
    // *******************************************************************


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

        tmpStr = textNorm.getText();

        if (testParameter(tmpStr, Double.MIN_VALUE, Double.MAX_VALUE)) {
            norm = Float.valueOf(tmpStr).floatValue();
        } else {
            textNorm.requestFocus();
            textNorm.selectAll();

            return false;
        }

        tmpStr = textGaussX.getText();

        if (testParameter(tmpStr, 0.5, 5.0)) {
            scaleX = Float.valueOf(tmpStr).floatValue();
        } else {
            textGaussX.requestFocus();
            textGaussX.selectAll();

            return false;
        }

        tmpStr = textGaussY.getText();

        if (testParameter(tmpStr, 0.5, 5.0)) {
            scaleY = Float.valueOf(tmpStr).floatValue();
        } else {
            textGaussY.requestFocus();
            textGaussY.selectAll();

            return false;
        }

        tmpStr = textIters.getText();

        if (testParameter(tmpStr, 1.0, 1000.0)) {
            iters = Integer.valueOf(tmpStr).intValue();
        } else {
            textIters.requestFocus();
            textIters.selectAll();

            return false;
        }

        thresholdSelected = thresholdCheckbox.isSelected();

        if (thresholdSelected) {
            tmpStr = thresholdText.getText();

            if (testParameter(tmpStr, image.getMin(), image.getMax())) {
                thresholdLevel = Float.valueOf(tmpStr).floatValue();
            } else {
                MipavUtil.displayError("Threshold level must be between " + image.getMin() + " and " + image.getMax());
                thresholdText.requestFocus();
                thresholdText.selectAll();

                return false;
            }
        } // if (thresholdSelected)

        return true;
    }


}
