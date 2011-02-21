package gov.nih.mipav.view.dialogs;


import gov.nih.mipav.model.algorithms.*;
import gov.nih.mipav.model.algorithms.filters.*;
import gov.nih.mipav.model.file.FileInfoBase.Unit;
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
 * source image. The program called performs nonlinear noise reduction on 2D and 3D black and white images. It does this
 * by only averaging a voxel with local voxels which have similar intensity. Algorithms are executed in their own
 * thread.
 *
 * @see  AlgorithmNLNoiseReduction
 */
public class JDialogNLNoiseReduction extends JDialogScriptableBase implements AlgorithmInterface, ActionDiscovery, ScriptableActionInterface {

    //~ Static fields/initializers -------------------------------------------------------------------------------------

    /** Use serialVersionUID for interoperability. */
    private static final long serialVersionUID = -6988226818561396907L;

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** DOCUMENT ME! */
    private double bt; // brightness threshold.  This allows the program to

    /** DOCUMENT ME! */
    private ButtonGroup destinationGroup;

    /** DOCUMENT ME! */
    private JPanel destinationPanel;

    /** DOCUMENT ME! */
    private int displayLoc; // Flag indicating if a new image is to be generated

    // or if the source image is to be replaced

    /** DOCUMENT ME! */
    private ModelImage image; // source image

    /** DOCUMENT ME! */
    private boolean image25D = false;

    /**
     * of similar brightness voxels is empty, a local median filter is used. This allows the correction of impulse
     * ("salt-and-pepper") noise. If false, when no neighborhood is found, the original intensity of the voxel of
     * interest remains unchanged.
     */
    private JCheckBox image25DCheckbox;

    /** DOCUMENT ME! */
    private JLabel labelMaskSD;

    /** DOCUMENT ME! */
    private JLabel labelThreshold;

    /** DOCUMENT ME! */
    private float maskSD; // This determines the spatial extent of the smoothing.

    /** DOCUMENT ME! */
    private double maximumThresholdDouble, defaultThresholdDouble;

    /**
     * The mask is basically Gaussian with standard deviation in image units - e.g. mm set by the user. However, for a
     * small, fast, flat response with a 3x3 or 3x3x3 voxel mask, set maskSD to 0.
     */
    private JCheckBox medianCheckbox;

    /** DOCUMENT ME! */
    private long minimumThreshold, maximumThreshold, defaultThreshold;

    /** DOCUMENT ME! */
    private JRadioButton newImage;

    /** DOCUMENT ME! */
    private AlgorithmNLNoiseReduction nlnrAlgo;

    /** DOCUMENT ME! */
    private JPanel optionsPanel;

    /** DOCUMENT ME! */
    private JRadioButton replaceImage;

    /** DOCUMENT ME! */
    private ModelImage resultImage = null; // result image

    /**
     * discriminate between noise and the underlying image. Ideally, the value should be set greater than the noise
     * level and less than the contrast of the underlying image. Edges of contrast smaller than this threshold will tend
     * to be blurred whereas those of greater contrast will not be.
     */
    private JTextField textMaskSD;

    /** DOCUMENT ME! */
    private JTextField textThreshold;

    /** DOCUMENT ME! */
    private String[] titles;

    /** DOCUMENT ME! */
    private boolean useMedian; // If true(the default), when the local neighborhood

    /** DOCUMENT ME! */
    private ViewUserInterface userInterface;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Empty constructor needed for dynamic instantiation (used during scripting).
     */
    public JDialogNLNoiseReduction() { }

    /**
     * Creates a new JDialogNLNoiseReduction object.
     *
     * @param  theParentFrame  Parent frame.
     * @param  im              Source image.
     */
    public JDialogNLNoiseReduction(Frame theParentFrame, ModelImage im) {
        super(theParentFrame, false);
        userInterface = ViewUserInterface.getReference();
        image = im;
        setDefaults();
        init();
    }

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * Closes dialog box when the OK button is pressed, sets variables and calls algorithm.
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
        } else if (command.equals("Help")) {
            MipavUtil.showHelp("10019");
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

        if (algorithm instanceof AlgorithmNLNoiseReduction) {
            image.clearMask();

            if ((nlnrAlgo.isCompleted() == true) && (resultImage != null)) {

                updateFileInfo(image, resultImage);
                resultImage.clearMask();

                // The algorithm has completed and produced a new image to be displayed.
                try {
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

        if (algorithm.isCompleted()) {
            insertScriptLine();
        }
     // save the completion status for later
        setComplete(algorithm.isCompleted());

        nlnrAlgo.finalize();
        nlnrAlgo = null;
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
     * Accessor that sets the brightness threshold.
     *
     * @param  bt  the brightness threshold. (values between 0.0 and (image.getMax()-image.getMin())).
     */
    public void setBt(double bt) {

        if (bt < 0.0) {
            bt = 0.0;
        }

        if (bt > (image.getMax() - image.getMin())) {
            bt = (image.getMax() - image.getMin());
        }

        this.bt = bt;
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
     * Accessor that sets the slicing flag.
     *
     * @param  flag  <code>true</code> indicates slices should be blurred independently.
     */
    public void setImage25D(boolean flag) {
        image25D = flag;
    }

    /**
     * Accessor that sets the mask standard deviation (in the same units as the image).
     *
     * @param  maskSD  the maskSD. (values between 0.0 and max float).
     */
    public void setMaskSD(float maskSD) {

        if (maskSD < (float) 0.0) {
            maskSD = (float) 0.0;
        }

        if (maskSD > Float.MAX_VALUE) {
            maskSD = Float.MAX_VALUE;
        }

        this.maskSD = maskSD;
    }

    /**
     * Accessor that sets the useMedian flag.
     *
     * @param  flag  <code>true</code> indicates useMedian should be processed.
     */
    public void setUseMedian(boolean flag) {
        useMedian = flag;
    }

    /**
     * Once all the necessary variables are set, call the Gaussian Blur algorithm based on what type of image this is
     * and whether or not there is a separate destination image.
     */
    protected void callAlgorithm() {
        String name = makeImageName(image.getImageName(), "_nlnoisereduction");

        if (image.getNDims() == 2) { // source image is 2D

            int[] destExtents = new int[2];
            destExtents[0] = image.getExtents()[0]; // X dim
            destExtents[1] = image.getExtents()[1]; // Y dim

            if (displayLoc == NEW) {

                try {

                    // Make result image of float type
                    // resultImage= new ModelImage(ModelImage.FLOAT, destExtents, name, userInterface);
                    resultImage = (ModelImage) image.clone();
                    resultImage.setImageName(name);

                    /* if ((resultImage.getFileInfo()[0]).getFileFormat() == FileUtility.DICOM){
                     *   ((FileInfoDicom)(resultImage.getFileInfo(0))).setSecondaryCaptureTags();
                     * }*/
                    // Make algorithm
                    nlnrAlgo = new AlgorithmNLNoiseReduction(resultImage, image, bt, maskSD, useMedian, false);

                    // This is very important. Adding this object as a listener allows the algorithm to
                    // notify this object when it has completed of failed. See algorithm performed event.
                    // This is made possible by implementing AlgorithmedPerformed interface
                    nlnrAlgo.addListener(this);

                    createProgressBar(image.getImageName(), nlnrAlgo);

                    // Hide dialog
                    setVisible(false);

                    if (isRunInSeparateThread()) {

                        // Start the thread as a low priority because we wish to still have user interface work fast.
                        if (nlnrAlgo.startMethod(Thread.MIN_PRIORITY) == false) {
                            MipavUtil.displayError("A thread is already running on this object");
                        }
                    } else {
                        nlnrAlgo.run();
                    }

                } catch (OutOfMemoryError x) {
                    MipavUtil.displayError("Dialog NLNoiseReduction: unable to allocate enough memory");

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
                    nlnrAlgo = new AlgorithmNLNoiseReduction(image, bt, maskSD, useMedian, false);

                    // This is very important. Adding this object as a listener allows the algorithm to
                    // notify this object when it has completed of failed. See algorithm performed event.
                    // This is made possible by implementing AlgorithmedPerformed interface
                    nlnrAlgo.addListener(this);

                    createProgressBar(image.getImageName(), nlnrAlgo);

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

                        // Start the thread as a low priority because we wish to still have user interface work fast.
                        if (nlnrAlgo.startMethod(Thread.MIN_PRIORITY) == false) {
                            MipavUtil.displayError("A thread is already running on this object");
                        }
                    } else {
                        nlnrAlgo.run();
                    }

                } catch (OutOfMemoryError x) {
                    MipavUtil.displayError("Dialog NLNoiseReduction: unable to allocate enough memory");

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
                    // resultImage = new ModelImage(ModelImage.FLOAT, destExtents, name, userInterface);
                    resultImage = (ModelImage) image.clone();
                    resultImage.setImageName(name);

                    /*if ((resultImage.getFileInfo()[0]).getFileFormat() == FileUtility.DICOM){
                     *             for (int i=0; i < resultImage.getExtents()[2]; i++) {
                     * ((FileInfoDicom)(resultImage.getFileInfo(i))).setSecondaryCaptureTags();
                     *    }              }*/
                    // Make algorithm
                    nlnrAlgo = new AlgorithmNLNoiseReduction(resultImage, image, bt, maskSD, useMedian, image25D);

                    // This is very important. Adding this object as a listener allows the algorithm to
                    // notify this object when it has completed of failed. See algorithm performed event.
                    // This is made possible by implementing AlgorithmedPerformed interface
                    nlnrAlgo.addListener(this);

                    createProgressBar(image.getImageName(), nlnrAlgo);

                    // Hide dialog
                    setVisible(false);

                    if (isRunInSeparateThread()) {

                        // Start the thread as a low priority because we wish to still have user interface work fast.
                        if (nlnrAlgo.startMethod(Thread.MIN_PRIORITY) == false) {
                            MipavUtil.displayError("A thread is already running on this object");
                        }
                    } else {
                        nlnrAlgo.run();
                    }

                } catch (OutOfMemoryError x) {
                    MipavUtil.displayError("Dialog NLNoiseReduction: unable to allocate enough memory");

                    if (resultImage != null) {
                        resultImage.disposeLocal(); // Clean up image memory
                        resultImage = null;
                    }

                    return;
                }
            } else {

                try {

                    // Make algorithm
                    nlnrAlgo = new AlgorithmNLNoiseReduction(image, bt, maskSD, useMedian, image25D);

                    // This is very important. Adding this object as a listener allows the algorithm to
                    // notify this object when it has completed of failed. See algorithm performed event.
                    // This is made possible by implementing AlgorithmedPerformed interface
                    nlnrAlgo.addListener(this);

                    createProgressBar(image.getImageName(), nlnrAlgo);

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

                        // Start the thread as a low priority because we wish to still have user interface work fast.
                        if (nlnrAlgo.startMethod(Thread.MIN_PRIORITY) == false) {
                            MipavUtil.displayError("A thread is already running on this object");
                        }
                    } else {
                        nlnrAlgo.run();
                    }

                } catch (OutOfMemoryError x) {
                    MipavUtil.displayError("Dialog NLNoiseReduction: unable to allocate enough memory");

                    return;
                }
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

        setDefaults();

        if (scriptParameters.doOutputNewImage()) {
            setDisplayLocNew();
        } else {
            setDisplayLocReplace();
        }

        setBt(scriptParameters.getParams().getDouble("brightness_threshold"));
        setMaskSD(scriptParameters.getParams().getFloat("mask_gaussian_std_dev"));
        setUseMedian(scriptParameters.getParams().getBoolean("do_use_median"));
        setImage25D(scriptParameters.doProcess3DAs25D());
    }

    /**
     * {@inheritDoc}
     */
    protected void storeParamsFromGUI() throws ParserException {
        scriptParameters.storeInputImage(image);
        scriptParameters.storeOutputImageParams(getResultImage(), (displayLoc == NEW));

        scriptParameters.getParams().put(ParameterFactory.newParameter("brightness_threshold", bt));
        scriptParameters.getParams().put(ParameterFactory.newParameter("mask_gaussian_std_dev", maskSD));
        scriptParameters.getParams().put(ParameterFactory.newParameter("do_use_median", useMedian));
        scriptParameters.storeProcess3DAs25D(image25D);
    }

    /**
     * Initializes the GUI by creating the components, placing them in the dialog, and displaying them.
     */
    private void init() {

        setForeground(Color.black);

        getContentPane().setLayout(new BorderLayout());
        setTitle("Nonlinear Noise Reduction");

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
        gbc.fill = GridBagConstraints.HORIZONTAL;
        gbc.gridx = 0;
        gbc.gridy = 0;

        optionsPanel = new JPanel(new GridBagLayout());
        optionsPanel.setForeground(Color.black);
        optionsPanel.setBorder(buildTitledBorder("Options"));
        getContentPane().add(optionsPanel);

        if ((image.getType() != ModelStorageBase.FLOAT) && (image.getType() != ModelStorageBase.DOUBLE)) {
            labelThreshold = createLabel("Brightness threshold (" + minimumThreshold + " - " + maximumThreshold + ")");
        } else {
            labelThreshold = createLabel("Brightness threshold ((>0.0) - (<" + maximumThresholdDouble + ") ");
        }

        optionsPanel.add(labelThreshold, gbc);

        gbc.gridx = 1;
        gbc.gridy = 0;
        textThreshold = new JTextField();

        if ((image.getType() != ModelStorageBase.FLOAT) && (image.getType() != ModelStorageBase.DOUBLE)) {
            textThreshold.setText(String.valueOf(defaultThreshold));
        } else {
            textThreshold.setText(String.valueOf(defaultThresholdDouble));
        }

        textThreshold.setFont(serif12);
        optionsPanel.add(textThreshold, gbc);

        gbc.gridx = 0;
        gbc.gridy = 1;
        labelMaskSD = createLabel("Mask Gaussian SD (0 for flat response) " +
                                  Unit.getUnitFromLegacyNum(image.getFileInfo()[0].getUnitsOfMeasure()[0]).toString() + " ");
        optionsPanel.add(labelMaskSD, gbc);

        gbc.gridx = 1;
        gbc.gridy = 1;
        textMaskSD = new JTextField();
        textMaskSD.setText(String.valueOf(image.getFileInfo()[0].getResolutions()[0]));
        textMaskSD.setFont(serif12);
        optionsPanel.add(textMaskSD, gbc);

        gbc.gridwidth = 2;
        gbc.gridx = 0;
        gbc.gridy = 2;
        medianCheckbox = new JCheckBox("Use median when neighborhood not found");
        medianCheckbox.setFont(serif12);
        optionsPanel.add(medianCheckbox, gbc);
        medianCheckbox.setSelected(true);

        gbc.gridx = 0;
        gbc.gridy = 3;
        image25DCheckbox = new JCheckBox("Process each slice independently (2.5D)");
        image25DCheckbox.setFont(serif12);
        optionsPanel.add(image25DCheckbox, gbc);
        image25DCheckbox.addItemListener(this);
        image25DCheckbox.setSelected(false);

        if (image.getNDims() == 2) {
            image25DCheckbox.setEnabled(false);
        }

        gbc.gridx = 0;
        gbc.gridy = 0;
        mainPanel.add(optionsPanel, gbc);

        JPanel outputOptPanel = new JPanel(new GridLayout(1, 1));
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

        // Only if the image is unlocked can it be replaced.
        if (image.getLockStatus() == ModelStorageBase.UNLOCKED) {
            replaceImage.setEnabled(true);
        } else {
            replaceImage.setEnabled(false);
        }

        gbc.gridx = 0;
        gbc.gridy = 1;
        mainPanel.add(outputOptPanel, gbc);

        getContentPane().add(mainPanel, BorderLayout.CENTER);
        getContentPane().add(buildButtons(), BorderLayout.SOUTH);
        pack();
        setResizable(true);
        setVisible(true);

        System.gc();
    }

    /**
     * Set the default values for the parameters.
     */
    private void setDefaults() {

        minimumThreshold = 1;

        if ((image.getType() != ModelStorageBase.FLOAT) && (image.getType() != ModelStorageBase.DOUBLE)) {
            maximumThreshold = (long) (image.getMax() - image.getMin() - 1);
            defaultThreshold = (long) (0.1 * (image.getMax() - image.getMin()));
            this.bt = defaultThreshold; // default
        } else {
            maximumThresholdDouble = image.getMax() - image.getMin();
            defaultThresholdDouble = 0.1 * (image.getMax() - image.getMin());
            this.bt = defaultThresholdDouble; // default
        }

        this.maskSD = image.getFileInfo()[0].getResolutions()[0]; // default value
        this.useMedian = true; // default
        this.image25D = false; // default
        this.setDisplayLocNew(); // default
    }

    /**
     * Use the GUI results to set up the variables needed to run the algorithm.
     *
     * @return  <code>true</code> if parameters set successfully, <code>false</code> otherwise.
     */
    private boolean setVariables() {
        String tmpStr;
        System.gc();

        if (medianCheckbox.isSelected()) {
            useMedian = true;
        } else {
            useMedian = false;
        }

        if (replaceImage.isSelected()) {
            displayLoc = REPLACE;
        } else if (newImage.isSelected()) {
            displayLoc = NEW;
        }

        if (image25DCheckbox.isSelected()) {
            image25D = true;
        }

        tmpStr = textThreshold.getText();

        if (testParameter(tmpStr, 0.0, image.getMax() - image.getMin())) {
            bt = Double.valueOf(tmpStr).doubleValue();
        } else {
            textThreshold.requestFocus();
            textThreshold.selectAll();

            return false;
        }

        tmpStr = textMaskSD.getText();

        if (testParameter(tmpStr, 0.0, Float.MAX_VALUE)) {
            maskSD = Float.valueOf(tmpStr).floatValue();
        } else {
            textMaskSD.requestFocus();
            textMaskSD.selectAll();

            return false;
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
                return new String("Algorithms.Filters (spatial)");
            }

            public String getDescription() {
                return new String("Performs a nonlinear noise reduction.");
            }

            public String getDescriptionLong() {
                return new String("Performs a nonlinear noise reduction.");
            }

            public String getShortLabel() {
                return new String("NonlinearNoiseReduction");
            }

            public String getLabel() {
                return new String("Nonlinear Noise Reduction");
            }

            public String getName() {
                return new String("Nonlinear Noise Reduction");
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
            table.put(new ParameterDouble("brightness_threshold"));
            table.put(new ParameterFloat("mask_gaussian_std_dev"));
            table.put(new ParameterBoolean("do_use_median", true));
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
