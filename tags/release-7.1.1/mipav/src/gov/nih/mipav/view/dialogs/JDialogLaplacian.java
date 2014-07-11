package gov.nih.mipav.view.dialogs;


import gov.nih.mipav.model.algorithms.AlgorithmBase;
import gov.nih.mipav.model.algorithms.AlgorithmInterface;
import gov.nih.mipav.model.algorithms.filters.AlgorithmLaplacian;
import gov.nih.mipav.model.algorithms.filters.OpenCL.filters.OpenCLAlgorithmFFT;
import gov.nih.mipav.model.algorithms.filters.OpenCL.filters.OpenCLAlgorithmLaplacian;
import gov.nih.mipav.model.file.FileInfoDicom;
import gov.nih.mipav.model.file.FileUtility;
import gov.nih.mipav.model.scripting.ParserException;
import gov.nih.mipav.model.scripting.ScriptableActionInterface;
import gov.nih.mipav.model.scripting.parameters.Parameter;
import gov.nih.mipav.model.scripting.parameters.ParameterBoolean;
import gov.nih.mipav.model.scripting.parameters.ParameterExternalImage;
import gov.nih.mipav.model.scripting.parameters.ParameterFactory;
import gov.nih.mipav.model.scripting.parameters.ParameterFloat;
import gov.nih.mipav.model.scripting.parameters.ParameterImage;
import gov.nih.mipav.model.scripting.parameters.ParameterInt;
import gov.nih.mipav.model.scripting.parameters.ParameterList;
import gov.nih.mipav.model.scripting.parameters.ParameterTable;
import gov.nih.mipav.model.structures.ModelImage;
import gov.nih.mipav.model.structures.ModelStorageBase;

import gov.nih.mipav.view.LegacyDialogDefaultsInterface;
import gov.nih.mipav.view.MipavUtil;
import gov.nih.mipav.view.Preferences;
import gov.nih.mipav.view.ViewImageUpdateInterface;
import gov.nih.mipav.view.ViewJFrameImage;
import gov.nih.mipav.view.ViewUserInterface;
import gov.nih.mipav.view.components.JPanelAlgorithmOutputOptions;
import gov.nih.mipav.view.components.JPanelSigmas;
import gov.nih.mipav.view.components.PanelManager;
import gov.nih.mipav.view.components.WidgetFactory;

import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.Dimension;
import java.awt.Frame;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.GridLayout;
import java.awt.event.ActionEvent;
import java.awt.event.ItemEvent;
import java.util.StringTokenizer;
import java.util.Vector;

import javax.swing.BorderFactory;
import javax.swing.JCheckBox;
import javax.swing.JComboBox;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JTextField;


/**
 * Dialog to get user input, then call the algorithm. The user is able to control the degree of blurring in all
 * dimensions and indicate if a correction factor be applied to the z-dimension to account for differing resolutions
 * between the xy resolutions (intra-plane) and the z resolution (inter-plane). The user has the option to generate a
 * new image or replace the source image. User can indicate whether to have algorithm applied to whole image or to the
 * VOI regions. Algorithms are executed in their own thread.
 * 
 * @version 0.1 Nov 17, 1998
 * @author Matthew J. McAuliffe, Ph.D.
 * @see AlgorithmLaplacian
 */
public class JDialogLaplacian extends JDialogScriptableBase implements AlgorithmInterface,
        LegacyDialogDefaultsInterface, ActionDiscovery, ScriptableActionInterface {

    // ~ Static fields/initializers
    // -------------------------------------------------------------------------------------

    /** Use serialVersionUID for interoperability. */
    private static final long serialVersionUID = 2631054897246586311L;

    // ~ Instance fields
    // ------------------------------------------------------------------------------------------------

    /** DOCUMENT ME! */
    private float ampFactor = 1.0f;

    /** DOCUMENT ME! */
    private ModelImage image; // source image

    private JCheckBox nonLinearCheckBox;

    private boolean nonLinear = false;

    private JLabel labelKernelSize;

    private JComboBox comboBoxKernelSize;

    private int kernelSize = 3;

    /** DOCUMENT ME! */
    private boolean image25D = false;

    /** Indicates whether user wants openCL for processing. */
    private JCheckBox useOCLCheckbox;

    /** Flag indicating whether to use OpenCL processing. */
    private boolean useOCL;

    /** DOCUMENT ME! */
    private JCheckBox image25DCheckbox;

    /** DOCUMENT ME! */
    private boolean separable = true;

    /** DOCUMENT ME! */
    private JCheckBox sepCheckbox;

    /** DOCUMENT ME! */
    private JLabel labelAmpFact;

    /** DOCUMENT ME! */
    private AlgorithmLaplacian laplacianAlgo;

    /** DOCUMENT ME! */
    private JPanelAlgorithmOutputOptions outputPanel;

    /** DOCUMENT ME! */
    private ModelImage resultImage = null; // result image

    /** DOCUMENT ME! */
    private JPanelSigmas sigmasPanel;

    /** DOCUMENT ME! */
    private JTextField textAmpFact;

    /** DOCUMENT ME! */
    private String[] titles;

    /** DOCUMENT ME! */
    private ViewUserInterface userInterface;

    /** display progress bar or not. */
    private boolean displayProgressBar = true;

    // ~ Constructors
    // ---------------------------------------------------------------------------------------------------

    /**
     * Empty constructor needed for dynamic instantiation (used during scripting).
     */
    public JDialogLaplacian() {}

    /**
     * Creates a new JDialogLaplacian object.
     * 
     * @param theParentFrame Parent frame.
     * @param im Source image.
     */
    public JDialogLaplacian(final Frame theParentFrame, final ModelImage im) {
        super(theParentFrame, false);
        image = im;
        userInterface = ViewUserInterface.getReference();
        init();
    }

    // ~ Methods
    // --------------------------------------------------------------------------------------------------------

    /**
     * Closes dialog box when the OK button is pressed, sets variables and calls algorithm.
     * 
     * @param event Event that triggers function.
     */
    @Override
    public void actionPerformed(final ActionEvent event) {
        final String command = event.getActionCommand();

        if (command.equals("OK")) {

            if (setVariables()) {
                callAlgorithm();
            }
        } else if (command.equals("Cancel")) {
            dispose();
        } else if (command.equals("Help")) {
            // MipavUtil.showHelp("10013");
            MipavUtil.showWebHelp("Filters_(Spatial)_Laplacian#Applying_the_Laplacian_algorithm");
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
     * @param algorithm Algorithm that caused the event.
     */
    @Override
    public void algorithmPerformed(final AlgorithmBase algorithm) {
        Preferences.debug("Laplacian: " + algorithm.getElapsedTime());

        if (algorithm instanceof OpenCLAlgorithmLaplacian) {
            if (algorithm.isCompleted()) {
                if (displayInNewFrame) {
                    resultImage = algorithm.getDestImage();
                    new ViewJFrameImage(resultImage);
                } else {
                    // These next lines set the titles in all frames where the source image is displayed to
                    // image name so as to indicate that the image is now unlocked!
                    // The image frames are enabled and then registered to the userinterface.
                    final Vector<ViewImageUpdateInterface> imageFrames = image.getImageFrameVector();

                    for (int i = 0; i < imageFrames.size(); i++) {
                        ((Frame) (imageFrames.elementAt(i))).setTitle(titles[i]);
                        ((Frame) (imageFrames.elementAt(i))).setEnabled(true);

                        if ( ((Frame) (imageFrames.elementAt(i))) != parentFrame) {
                            userInterface.registerFrame((Frame) (imageFrames.elementAt(i)));
                        }
                    }

                    if (parentFrame != null) {
                        userInterface.registerFrame(parentFrame);
                    }

                    image.notifyImageDisplayListeners(null, true);
                }
            }
        }
        if (algorithm instanceof AlgorithmLaplacian) {
            image.clearMask();

            if ( (laplacianAlgo.isCompleted() == true) && (resultImage != null)) {

                updateFileInfo(image, resultImage);
                resultImage.clearMask();

                // The algorithm has completed and produced a new image to be displayed.

                try {
                    new ViewJFrameImage(resultImage, null, new Dimension(610, 200));
                } catch (final OutOfMemoryError error) {
                    MipavUtil.displayError("Out of memory: unable to open new frame");
                }
            } else if (resultImage == null) {

                // These next lines set the titles in all frames where the source image is displayed to
                // image name so as to indicate that the image is now unlocked!
                // The image frames are enabled and then registed to the userinterface.
                final Vector<ViewImageUpdateInterface> imageFrames = image.getImageFrameVector();

                for (int i = 0; i < imageFrames.size(); i++) {
                    ((Frame) (imageFrames.elementAt(i))).setTitle(titles[i]);
                    ((Frame) (imageFrames.elementAt(i))).setEnabled(true);

                    if ( ((Frame) (imageFrames.elementAt(i))) != parentFrame) {
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

        if (laplacianAlgo != null) {
            laplacianAlgo.finalize();
            laplacianAlgo = null;
        }

        dispose();
    }

    /**
     * Accessor that returns the image.
     * 
     * @return The result image.
     */
    public ModelImage getResultImage() {
        return resultImage;
    }

    // *******************************************************************
    // ************************* Item Events ****************************
    // *******************************************************************

    /**
     * Resets labels if checkboxes are checked or unchecked.
     * 
     * @param event Event that cause the method to fire.
     */
    @Override
    public void itemStateChanged(final ItemEvent event) {
        final Object source = event.getSource();

        if (source == image25DCheckbox) {
            sigmasPanel.enable3DComponents( !image25DCheckbox.isSelected());
            if (image.getNDims() == 3) {
                nonLinearCheckBox.setEnabled(image25DCheckbox.isSelected());
                if ( !image25DCheckbox.isSelected()) {
                    nonLinearCheckBox.setSelected(false);
                    labelKernelSize.setEnabled(false);
                    comboBoxKernelSize.setEnabled(false);
                }
            }
        }
        if (source == nonLinearCheckBox) {
            if (nonLinearCheckBox.isSelected()) {
                sepCheckbox.setEnabled(false);
                sepCheckbox.setSelected(false);
                useOCLCheckbox.setEnabled(false);
                useOCLCheckbox.setSelected(false);
                labelKernelSize.setEnabled(true);
                comboBoxKernelSize.setEnabled(true);
            } else {
                useOCLCheckbox.setEnabled(Preferences.isGpuCompEnabled() && OpenCLAlgorithmFFT.isOCLAvailable());
                if ( !useOCLCheckbox.isEnabled() && OpenCLAlgorithmFFT.isOCLAvailable()) {
                    useOCLCheckbox.setToolTipText("see Help->Mipav Options->Other to enable GPU computing");
                }
                sepCheckbox.setEnabled(useOCLCheckbox.isSelected());
                sepCheckbox.setSelected(useOCLCheckbox.isSelected());
                labelKernelSize.setEnabled(false);
                comboBoxKernelSize.setEnabled(false);
            }
        }
        if (source == sepCheckbox || source == useOCLCheckbox) {
            sepCheckbox.removeItemListener(this);
            useOCLCheckbox.removeItemListener(this);
            if (source == useOCLCheckbox) {
                sepCheckbox.setEnabled(useOCLCheckbox.isSelected());
                if ( !sepCheckbox.isEnabled()) {
                    sepCheckbox.setSelected(false);
                }
            }
            sepCheckbox.addItemListener(this);
            useOCLCheckbox.addItemListener(this);
        }
    }

    /**
     * Loads the default settings from Preferences to set up the dialog.
     */
    @Override
    public void legacyLoadDefaults() {
        final String defaultsString = Preferences.getDialogDefaults(getDialogName());

        if ( (defaultsString != null) && (outputPanel != null)) {

            try {
                final StringTokenizer st = new StringTokenizer(defaultsString, ",");

                outputPanel.setProcessWholeImage(MipavUtil.getBoolean(st));
                outputPanel.setOutputNewImage(MipavUtil.getBoolean(st));

                nonLinearCheckBox.setSelected(MipavUtil.getBoolean(st));
                comboBoxKernelSize.setSelectedIndex( (MipavUtil.getInt(st) - 1) / 2);

                sepCheckbox.setSelected(MipavUtil.getBoolean(st));
                image25DCheckbox.setSelected(MipavUtil.getBoolean(st));

                sigmasPanel.setSigmaX(MipavUtil.getFloat(st));
                sigmasPanel.setSigmaY(MipavUtil.getFloat(st));
                sigmasPanel.setSigmaZ(MipavUtil.getFloat(st));
                sigmasPanel.enableResolutionCorrection(MipavUtil.getBoolean(st));

                textAmpFact.setText(st.nextToken());
            } catch (final Exception ex) {

                // since there was a problem parsing the defaults string, start over with the original defaults
                Preferences.debug("Resetting defaults for dialog: " + getDialogName());
                Preferences.removeProperty(getDialogName());
                ex.printStackTrace();
            }
        }

    }

    /**
     * Saves the default settings into the Preferences file.
     */
    @Override
    public void legacySaveDefaults() {
        final String delim = ",";

        String defaultsString = outputPanel.isProcessWholeImageSet() + delim;
        defaultsString += outputPanel.isOutputNewImageSet() + delim;
        defaultsString += nonLinear + delim;
        defaultsString += kernelSize + delim;
        defaultsString += image25D + delim;
        defaultsString += sigmasPanel.getUnnormalized3DSigmas()[0] + delim;
        defaultsString += sigmasPanel.getUnnormalized3DSigmas()[1] + delim;
        defaultsString += sigmasPanel.getUnnormalized3DSigmas()[2] + delim;
        defaultsString += sigmasPanel.isResolutionCorrectionEnabled() + delim;
        defaultsString += ampFactor;

        Preferences.saveDialogDefaults(getDialogName(), defaultsString);
    }

    /**
     * Accessor that sets the amplication factor.
     * 
     * @param ampFact should range between 0.5 and 2.
     */
    public void setAmpFactor(final float ampFact) {
        ampFactor = ampFact;
    }

    /**
     * Accessor that sets the slicing flag.
     * 
     * @param flag <code>true</code> indicates slices should be blurred independently.
     */
    public void setImage25D(final boolean flag) {
        image25D = flag;
    }

    /**
     * Changes whether a new image should be generated by the algorithm.
     * 
     * @param flag true if a new image should be made, false otherwise
     */
    public void setOutputNewImage(final boolean flag) {
        outputPanel.setOutputNewImage(flag);
    }

    /**
     * Set the display progress bar flag.
     * 
     * @param flag display or not
     */
    public void setDisplayProgressBar(final boolean flag) {
        displayProgressBar = flag;
    }

    public void setSeparable(final boolean separable) {
        this.separable = separable;
        if (sepCheckbox != null) {
            sepCheckbox.setSelected(this.separable);
        }
    }

    /**
     * Accessor that sets whether to use OpenCL processing (may still not be set if it is not supported on the system).
     * 
     * @param useOCL Whether to try to use OpenCL processing.
     */
    public void setUseOCL(final boolean useOCL) {
        this.useOCL = useOCL & (Preferences.isGpuCompEnabled() && OpenCLAlgorithmFFT.isOCLAvailable());
        if (useOCLCheckbox != null) {
            useOCLCheckbox.setSelected(this.useOCL);
        }
    }

    /**
     * 
     * @param nonLinear
     */
    public void setNonLinear(final boolean nonLinear) {
        this.nonLinear = nonLinear;
    }

    /**
     * 
     * @param kernelSize
     */
    public void setKernelSize(final int kernelSize) {
        this.kernelSize = kernelSize;
    }

    /**
     * Once all the necessary variables are set, call the Gaussian Blur algorithm based on what type of image this is
     * and whether or not there is a separate destination image.
     */
    @Override
    protected void callAlgorithm() {
        final String name = makeImageName(image.getImageName(), "_laplacian");
        displayInNewFrame = outputPanel.isOutputNewImageSet();

        // Check if the algorithm should use OpenCL, calculate and return:
        if (useOCL) {
            final float[] sigmas = sigmasPanel.getNormalizedSigmas();

            OpenCLAlgorithmLaplacian laplacianAlgo;
            if (displayInNewFrame) {
                resultImage = new ModelImage(ModelStorageBase.FLOAT, image.getExtents(), name);
                JDialogBase.updateFileInfo(image, resultImage);
                laplacianAlgo = new OpenCLAlgorithmLaplacian(resultImage, image, sigmas,
                        outputPanel.isProcessWholeImageSet(), separable, image25D, ampFactor);
            } else {
                final Vector<ViewImageUpdateInterface> imageFrames = image.getImageFrameVector();

                titles = new String[imageFrames.size()];

                for (int i = 0; i < imageFrames.size(); i++) {
                    titles[i] = ((Frame) (imageFrames.elementAt(i))).getTitle();
                    ((Frame) (imageFrames.elementAt(i))).setTitle("Locked: " + titles[i]);
                    ((Frame) (imageFrames.elementAt(i))).setEnabled(false);
                    userInterface.unregisterFrame((Frame) (imageFrames.elementAt(i)));
                }
                laplacianAlgo = new OpenCLAlgorithmLaplacian(image, sigmas, outputPanel.isProcessWholeImageSet(),
                        separable, image25D, ampFactor);
            }

            laplacianAlgo.addListener(this);
            // Hide the dialog since the algorithm is about to run.
            setVisible(false);
            laplacianAlgo.run();
            return;
        }

        if (image.getNDims() == 2) { // source image is 2D

            final float[] sigmas = sigmasPanel.getNormalizedSigmas();

            if (outputPanel.isOutputNewImageSet()) {

                try {

                    // Make result image of float type
                    resultImage = new ModelImage(ModelImage.FLOAT, image.getExtents(), name);

                    // resultImage = (ModelImage)image.clone();
                    // resultImage.setImageName(name);
                    if ( (resultImage.getFileInfo()[0]).getFileFormat() == FileUtility.DICOM) {
                        ((FileInfoDicom) (resultImage.getFileInfo(0))).setSecondaryCaptureTags();
                    }

                    // Make algorithm
                    laplacianAlgo = new AlgorithmLaplacian(resultImage, image, nonLinear, kernelSize, sigmas,
                            outputPanel.isProcessWholeImageSet(), false, ampFactor);

                    // This is very important. Adding this object as a listener allows the algorithm to
                    // notify this object when it has completed of failed. See algorithm performed event.
                    // This is made possible by implementing AlgorithmedPerformed interface
                    laplacianAlgo.addListener(this);

                    if (displayProgressBar) {
                        createProgressBar(image.getImageName(), laplacianAlgo);
                    }

                    // Hide dialog
                    setVisible(false);

                    if (isRunInSeparateThread()) {

                        // Start the thread as a low priority because we wish to still have user interface work fast
                        if (laplacianAlgo.startMethod(Thread.MIN_PRIORITY) == false) {
                            MipavUtil.displayError("A thread is already running on this object");
                        }
                    } else {

                        laplacianAlgo.run();
                    }
                } catch (final OutOfMemoryError x) {
                    MipavUtil.displayError("Dialog laplacian: unable to allocate enough memory");

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
                    laplacianAlgo = new AlgorithmLaplacian(image, nonLinear, kernelSize, sigmas,
                            outputPanel.isProcessWholeImageSet(), false, ampFactor);

                    // This is very important. Adding this object as a listener allows the algorithm to
                    // notify this object when it has completed of failed. See algorithm performed event.
                    // This is made possible by implementing AlgorithmedPerformed interface
                    laplacianAlgo.addListener(this);

                    if (displayProgressBar) {
                        createProgressBar(image.getImageName(), laplacianAlgo);
                    }

                    // Hide the dialog since the algorithm is about to run.
                    setVisible(false);

                    // These next lines set the titles in all frames where the source image is displayed to
                    // "locked - " image name so as to indicate that the image is now read/write locked!
                    // The image frames are disabled and then unregisted from the userinterface until the
                    // algorithm has completed.
                    final Vector<ViewImageUpdateInterface> imageFrames = image.getImageFrameVector();

                    titles = new String[imageFrames.size()];

                    for (int i = 0; i < imageFrames.size(); i++) {
                        titles[i] = ((Frame) (imageFrames.elementAt(i))).getTitle();
                        ((Frame) (imageFrames.elementAt(i))).setTitle("Locked: " + titles[i]);
                        ((Frame) (imageFrames.elementAt(i))).setEnabled(false);
                        userInterface.unregisterFrame((Frame) (imageFrames.elementAt(i)));
                    }

                    if (isRunInSeparateThread()) {

                        // Start the thread as a low priority because we wish to still have user interface work fast
                        if (laplacianAlgo.startMethod(Thread.MIN_PRIORITY) == false) {
                            MipavUtil.displayError("A thread is already running on this object");
                        }
                    } else {

                        laplacianAlgo.run();
                    }
                } catch (final OutOfMemoryError x) {
                    MipavUtil.displayError("Dialog laplacian: unable to allocate enough memory");

                    return;
                }
            }
        } else if (image.getNDims() == 3) {

            final float[] sigmas = sigmasPanel.getNormalizedSigmas();

            if (outputPanel.isOutputNewImageSet()) {

                try {
                    // Make result image of float type

                    resultImage = new ModelImage(ModelImage.FLOAT, image.getExtents(), name);
                    // resultImage = (ModelImage) image.clone();
                    resultImage.setImageName(name);

                    if ( (resultImage.getFileInfo()[0]).getFileFormat() == FileUtility.DICOM) {

                        for (int i = 0; i < resultImage.getExtents()[2]; i++) {
                            ((FileInfoDicom) (resultImage.getFileInfo(i))).setSecondaryCaptureTags();
                        }
                    }

                    // Make algorithm
                    laplacianAlgo = new AlgorithmLaplacian(resultImage, image, nonLinear, kernelSize, sigmas,
                            outputPanel.isProcessWholeImageSet(), image25D, ampFactor);

                    // This is very important. Adding this object as a listener allows the algorithm to
                    // notify this object when it has completed of failed. See algorithm performed event.
                    // This is made possible by implementing AlgorithmedPerformed interface
                    laplacianAlgo.addListener(this);

                    if (displayProgressBar) {
                        createProgressBar(image.getImageName(), laplacianAlgo);
                    }

                    // Hide dialog
                    setVisible(false);

                    if (isRunInSeparateThread()) {

                        // Start the thread as a low priority because we wish to still have user interface work fast
                        if (laplacianAlgo.startMethod(Thread.MIN_PRIORITY) == false) {
                            MipavUtil.displayError("A thread is already running on this object");
                        }
                    } else {

                        laplacianAlgo.run();
                    }
                } catch (final OutOfMemoryError x) {
                    MipavUtil.displayError("Dialog laplacian: unable to allocate enough memory");

                    if (resultImage != null) {
                        resultImage.disposeLocal(); // Clean up image memory
                        resultImage = null;
                    }

                    return;
                }
            } else {

                try {

                    // Make algorithm
                    laplacianAlgo = new AlgorithmLaplacian(image, nonLinear, kernelSize, sigmas,
                            outputPanel.isProcessWholeImageSet(), image25D, ampFactor);

                    // This is very important. Adding this object as a listener allows the algorithm to
                    // notify this object when it has completed of failed. See algorithm performed event.
                    // This is made possible by implementing AlgorithmedPerformed interface
                    laplacianAlgo.addListener(this);
                    if (displayProgressBar) {
                        createProgressBar(image.getImageName(), laplacianAlgo);
                    }

                    // Hide dialog
                    setVisible(false);

                    // These next lines set the titles in all frames where the source image is displayed to
                    // "locked - " image name so as to indicate that the image is now read/write locked!
                    // The image frames are disabled and then unregisted from the userinterface until the
                    // algorithm has completed.
                    final Vector<ViewImageUpdateInterface> imageFrames = image.getImageFrameVector();

                    titles = new String[imageFrames.size()];

                    for (int i = 0; i < imageFrames.size(); i++) {
                        titles[i] = ((Frame) (imageFrames.elementAt(i))).getTitle();
                        ((Frame) (imageFrames.elementAt(i))).setTitle("Locked: " + titles[i]);
                        ((Frame) (imageFrames.elementAt(i))).setEnabled(false);
                        userInterface.unregisterFrame((Frame) (imageFrames.elementAt(i)));
                    }

                    if (isRunInSeparateThread()) {

                        // Start the thread as a low priority because we wish to still have user interface work fast
                        if (laplacianAlgo.startMethod(Thread.MIN_PRIORITY) == false) {
                            MipavUtil.displayError("A thread is already running on this object");
                        }
                    } else {

                        laplacianAlgo.run();
                    }
                } catch (final OutOfMemoryError x) {
                    MipavUtil.displayError("Dialog laplacian: unable to allocate enough memory");

                    return;
                }
            }
        }

    }

    /**
     * Store the result image in the script runner's image table now that the action execution is finished.
     */
    @Override
    protected void doPostAlgorithmActions() {

        if (outputPanel.isOutputNewImageSet()) {
            AlgorithmParameters.storeImageInRunner(getResultImage());
        }
    }

    /**
     * {@inheritDoc}
     */
    @Override
    protected void setGUIFromParams() {
        image = scriptParameters.retrieveInputImage();
        userInterface = ViewUserInterface.getReference();
        parentFrame = image.getParentFrame();

        outputPanel = new JPanelAlgorithmOutputOptions(image);
        sigmasPanel = new JPanelSigmas(image);

        scriptParameters.setOutputOptionsGUI(outputPanel);
        setNonLinear(scriptParameters.getParams().getBoolean("non_linear"));
        setKernelSize(scriptParameters.getParams().getInt("kernel_size"));
        setImage25D(scriptParameters.getParams().getBoolean(AlgorithmParameters.DO_PROCESS_3D_AS_25D));
        scriptParameters.setSigmasGUI(sigmasPanel);
        setAmpFactor(scriptParameters.getParams().getFloat("amplification_factor"));

        if (scriptParameters.getParams().containsParameter(AlgorithmParameters.USE_OPENCL)) {
            setUseOCL(scriptParameters.getParams().getBoolean(AlgorithmParameters.USE_OPENCL));
        }
    }

    /**
     * {@inheritDoc}
     */
    @Override
    protected void storeParamsFromGUI() throws ParserException {
        scriptParameters.storeInputImage(image);
        scriptParameters.storeOutputImageParams(resultImage, outputPanel.isOutputNewImageSet());

        scriptParameters.getParams().put(ParameterFactory.newParameter("non_linear", nonLinear));
        scriptParameters.getParams().put(ParameterFactory.newParameter("kernel_size", kernelSize));
        scriptParameters.storeProcessingOptions(outputPanel.isProcessWholeImageSet(), image25D);
        scriptParameters.storeSigmas(sigmasPanel);
        scriptParameters.getParams().put(ParameterFactory.newParameter("amplification_factor", ampFactor));
        scriptParameters.getParams().put(ParameterFactory.newParameter(AlgorithmParameters.USE_OPENCL, useOCL));
    }

    /**
     * Initializes the GUI by creating the components, placing them in the dialog, and displaying them.
     */
    private void init() {
        setForeground(Color.black);

        getContentPane().setLayout(new BorderLayout());
        setTitle("Laplacian");

        final JPanel mainPanel = new JPanel(new GridLayout());
        mainPanel.setBorder(BorderFactory.createEmptyBorder(3, 3, 3, 3));
        mainPanel.setLayout(new GridBagLayout());

        final GridBagConstraints gbc = new GridBagConstraints();
        gbc.gridwidth = 1;
        gbc.gridheight = 1;
        gbc.anchor = GridBagConstraints.WEST;
        gbc.weightx = 1;
        gbc.fill = GridBagConstraints.HORIZONTAL;
        gbc.gridx = 0;
        gbc.gridy = 0;

        sigmasPanel = new JPanelSigmas(image);
        mainPanel.add(sigmasPanel, gbc);

        final PanelManager algoPanel = new PanelManager("Algorithm options");
        labelAmpFact = createLabel("Amplification factor (1.0 - 2.0) ");
        algoPanel.add(labelAmpFact);
        textAmpFact = createTextField("1.0");
        algoPanel.add(textAmpFact);

        nonLinearCheckBox = WidgetFactory.buildCheckBox("Use nonlinear Laplcian", false, this);
        if (image.getNDims() == 2) {
            nonLinearCheckBox.setEnabled(true);
        } else {
            nonLinearCheckBox.setEnabled(false);
        }
        algoPanel.addOnNextLine(nonLinearCheckBox);

        labelKernelSize = WidgetFactory.buildLabel("Kernel size:"); // make & set a label
        labelKernelSize.setEnabled(false);
        algoPanel.addOnNextLine(labelKernelSize); // add kernel label

        comboBoxKernelSize = new JComboBox();
        comboBoxKernelSize.setFont(serif12);
        comboBoxKernelSize.setBackground(Color.white);
        this.buildKernelSizeComboBox();
        comboBoxKernelSize.setEnabled(false);
        algoPanel.add(comboBoxKernelSize);

        sepCheckbox = WidgetFactory.buildCheckBox("Use separable convolution kernels", true, this);
        algoPanel.addOnNextLine(sepCheckbox);

        image25DCheckbox = new JCheckBox("Process each slice independently (2.5D)");
        image25DCheckbox.setFont(serif12);
        algoPanel.addOnNextLine(image25DCheckbox);
        image25DCheckbox.setSelected(false);
        image25DCheckbox.addItemListener(this);

        useOCLCheckbox = WidgetFactory.buildCheckBox("Use OpenCL", false, this);
        useOCLCheckbox.setFont(serif12);
        useOCLCheckbox.setForeground(Color.black);
        useOCLCheckbox.setEnabled(Preferences.isGpuCompEnabled() && OpenCLAlgorithmFFT.isOCLAvailable());
        if ( !useOCLCheckbox.isEnabled() && OpenCLAlgorithmFFT.isOCLAvailable()) {
            useOCLCheckbox.setToolTipText("see Help->Mipav Options->Other to enable GPU computing");
        }
        sepCheckbox.setEnabled(useOCLCheckbox.isSelected());
        sepCheckbox.setSelected(useOCLCheckbox.isSelected());
        algoPanel.addOnNextLine(useOCLCheckbox);

        if (image.getNDims() != 3) {
            image25DCheckbox.setEnabled(false);
        }

        gbc.gridy++;
        mainPanel.add(algoPanel.getPanel(), gbc);

        outputPanel = new JPanelAlgorithmOutputOptions(image);
        gbc.gridy++;
        mainPanel.add(outputPanel, gbc);

        getContentPane().add(mainPanel, BorderLayout.CENTER);
        getContentPane().add(buildButtons(), BorderLayout.SOUTH);
        pack();
        setResizable(true);
        // setVisible(true);

        System.gc();
    }

    /**
     * Creates the combo-box that allows user to select the size of the kernel (mask).
     * 
     * @param singleSlices DOCUMENT ME!
     */
    private void buildKernelSizeComboBox() {
        comboBoxKernelSize.addItem("3x3");
        comboBoxKernelSize.addItem("5x5");
        comboBoxKernelSize.addItem("7x7");
        comboBoxKernelSize.addItem("9x9");
        comboBoxKernelSize.addItem("11x11");
    }

    /**
     * Use the GUI results to set up the variables needed to run the algorithm.
     * 
     * @return <code>true</code> if parameters set successfully, <code>false</code> otherwise.
     */
    private boolean setVariables() {
        String tmpStr;

        System.gc();

        nonLinear = nonLinearCheckBox.isSelected();
        image25D = image25DCheckbox.isSelected();
        separable = sepCheckbox.isSelected();
        useOCL = useOCLCheckbox.isSelected();

        if ( !sigmasPanel.testSigmaValues()) {
            return false;
        }

        tmpStr = textAmpFact.getText();

        if (testParameter(tmpStr, 1.0, 2.0)) {
            ampFactor = Float.valueOf(tmpStr).floatValue();
        } else {
            textAmpFact.requestFocus();
            textAmpFact.selectAll();

            return false;
        }

        if (nonLinear) {
            this.determineKernelSize();
        }

        return true;
    }

    /**
     * Associate one side of the kernel size with selectBox choice.
     */
    private void determineKernelSize() {

        if (comboBoxKernelSize.getSelectedIndex() == 0) {
            kernelSize = 3;
        } else if (comboBoxKernelSize.getSelectedIndex() == 1) {
            kernelSize = 5;
        } else if (comboBoxKernelSize.getSelectedIndex() == 2) {
            kernelSize = 7;
        } else if (comboBoxKernelSize.getSelectedIndex() == 3) {
            kernelSize = 9;
        } else if (comboBoxKernelSize.getSelectedIndex() == 4) {
            kernelSize = 11;
        }
    }

    /**
     * Return meta-information about this discoverable action for categorization and labeling purposes.
     * 
     * @return Metadata for this action.
     */
    @Override
    public ActionMetadata getActionMetadata() {
        return new MipavActionMetadata() {
            @Override
            public String getCategory() {
                return new String("Algorithms.Filters (spatial)");
            }

            @Override
            public String getDescription() {
                return new String("Calculates the Laplacian of an image.");
            }

            @Override
            public String getDescriptionLong() {
                return new String("Calculates the Laplacian of an image.");
            }

            @Override
            public String getShortLabel() {
                return new String("Laplacian");
            }

            @Override
            public String getLabel() {
                return new String("Laplacian");
            }

            @Override
            public String getName() {
                return new String("Laplacian");
            }
        };
    }

    /**
     * Returns a table listing the input parameters of this algorithm (which should match up with the scripting
     * parameters used in {@link #setGUIFromParams()}).
     * 
     * @return A parameter table listing the inputs of this algorithm.
     */
    @Override
    public ParameterTable createInputParameters() {
        final ParameterTable table = new ParameterTable();

        try {
            table.put(new ParameterExternalImage(AlgorithmParameters.getInputImageLabel(1)));
            table.put(new ParameterBoolean(AlgorithmParameters.DO_OUTPUT_NEW_IMAGE, true));
            table.put(new ParameterBoolean(AlgorithmParameters.DO_PROCESS_WHOLE_IMAGE, true));
            table.put(new ParameterBoolean(AlgorithmParameters.DO_PROCESS_SEPARABLE, true));
            table.put(new ParameterBoolean(AlgorithmParameters.DO_PROCESS_3D_AS_25D, false));
            table.put(new ParameterList(AlgorithmParameters.SIGMAS, Parameter.PARAM_FLOAT, "1.0,1.0,1.0"));
            table.put(new ParameterBoolean(AlgorithmParameters.SIGMA_DO_Z_RES_CORRECTION, true));
            table.put(new ParameterFloat("amplification_factor", 1));
            table.put(new ParameterBoolean("non_linear", false));
            table.put(new ParameterInt("kernel_size, 3"));

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
    @Override
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
    @Override
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
    @Override
    public boolean isActionComplete() {
        return isComplete();
    }

}
