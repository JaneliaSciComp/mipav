package gov.nih.mipav.view.dialogs;


import gov.nih.mipav.model.algorithms.*;
import gov.nih.mipav.model.algorithms.filters.*;
import gov.nih.mipav.model.file.*;
import gov.nih.mipav.model.scripting.*;
import gov.nih.mipav.model.structures.*;

import gov.nih.mipav.view.*;
import gov.nih.mipav.view.components.*;

import java.awt.*;
import java.awt.event.*;

import java.util.*;

import javax.swing.*;


/**
 * Dialog to get user input, then call the algorithm. The user is able to control the degree of blurring in all
 * dimensions and indicate if a correction factor be applied to the z-dimension to account for differing resolutions
 * between the xy resolutions (intra-plane) and the z resolution (inter-plane). The user has the option to generate a
 * new image or replace the source image. In addition the user can indicate if he/she wishes to have the algorithm
 * applied to whole image or to the VOI regions. It should be noted that the algorithms are executed in their own
 * thread.
 *
 * @see  AlgorithmWaterShedITK
 */
public class JDialogWaterShedITK extends JDialogScriptableBase
        implements AlgorithmInterface, DialogDefaultsInterface {

    //~ Static fields/initializers -------------------------------------------------------------------------------------

    /** Use serialVersionUID for interoperability. */
    private static final long serialVersionUID = -4057161307906161677L;

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** DOCUMENT ME! */
    private JPanelColorChannels colorChannelPanel;

    /** DOCUMENT ME! */
    private AlgorithmWaterShedITK waterShedlAlgo;

    /** DOCUMENT ME! */
    private ModelImage image; // source image

    /** false = apply algorithm only to VOI regions. */
    private boolean image25D = false; // flag indicating if slices should be blurred independently

    /** DOCUMENT ME! */
    private JCheckBox image25DCheckbox;

    /** DOCUMENT ME! */
    private JPanelAlgorithmOutputOptions outputOptionsPanel;

    /** DOCUMENT ME! */
    private ModelImage resultImage = null; // result image

    /** DOCUMENT ME! */
    private String[] titles;

    /** DOCUMENT ME! */
    private ViewUserInterface userInterface;

    /** WaterShed parameters panel. */
    private JPanelWaterShedITK waterShedPanel;
    
    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Empty constructor needed for dynamic instantiation.
     */
    public JDialogWaterShedITK() { }

    /**
     * Creates a new JDialogGaussianBlurITK object.
     *
     * @param  theParentFrame  Parent frame.
     * @param  im              Source image.
     */
    public JDialogWaterShedITK(Frame theParentFrame, ModelImage im) {
        super(theParentFrame, false);
        image = im;
        userInterface = ViewUserInterface.getReference();
        init();
        // loadDefaults();
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

        if (command.equals("OK")) {

            if (setVariables()) {
                callAlgorithm();
            }
        } else if (command.equals("Cancel")) {
            dispose();
        } 
        else if (command.equals("Help")) {
            MipavUtil.showHelp("WShed10");
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

        if (Preferences.is(Preferences.PREF_SAVE_DEFAULTS) && (this.getOwner() != null) && !isScriptRunning()) {
            saveDefaults();
        }

        image.clearMask();

        if ((waterShedlAlgo.isCompleted() == true) && (resultImage != null)) {

            // The algorithm has completed and produced a new image to be displayed.
            if (resultImage.isColorImage()) {
                updateFileInfo(image, resultImage);
            }

            resultImage.clearMask();

            try {
                new ViewJFrameImage(resultImage, null, new Dimension(610, 200));
            } catch (OutOfMemoryError error) {
                System.gc();
                MipavUtil.displayError("Out of memory: unable to open new frame");
            }
        } else if (resultImage == null) {

            // These next lines set the titles in all frames where the source image is displayed to
            // image name so as to indicate that the image is now unlocked!
            // The image frames are enabled and then registered to the userinterface.
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
            System.gc();

        }

        if (algorithm.isCompleted()) {
            insertScriptLine();
        }

        if (waterShedlAlgo != null) {
        	waterShedlAlgo.finalize();
        	waterShedlAlgo = null;
        }

        dispose();
    }

    /**
     * Construct a delimited string that contains the parameters to this algorithm.
     *
     * @param   delim  the parameter delimiter (defaults to " " if empty)
     *
     * @return  the parameter string
     */
    public String getParameterString(String delim) {

        if (delim.equals("")) {
            delim = " ";
        }

        String str = new String();
        str += outputOptionsPanel.isProcessWholeImageSet() + delim;
        str += image25D + delim;
        str += waterShedPanel.getParameters()[0] + delim;
        str += waterShedPanel.getParameters()[1] + delim;
        str += waterShedPanel.getParameters()[2] + delim;
        str += waterShedPanel.getParameters()[3] + delim;
        str += waterShedPanel.getParameters()[4] + delim;
        str += colorChannelPanel.isRedProcessingRequested() + delim;
        str += colorChannelPanel.isGreenProcessingRequested() + delim;
        str += colorChannelPanel.isBlueProcessingRequested();

        return str;
    }

    /**
     * Accessor that returns the image.
     *
     * @return  The result image.
     */
    public ModelImage getResultImage() {
        return resultImage;
    }

    // *******************************************************************
    // ************************* Item Events ****************************
    // *******************************************************************

    /**
     * Changes labels based on whether or not check box is checked.
     *
     * @param  event  event that cause the method to fire
     */
    public void itemStateChanged(ItemEvent event) {
        Object source = event.getSource();

        if (source == image25DCheckbox) {
        	// waterShedPanel.enable3DComponents(!image25DCheckbox.isSelected());
        }
    }

    /**
     * Loads the default settings from Preferences to set up the dialog.
     */
    public void loadDefaults() {
        String defaultsString = Preferences.getDialogDefaults(getDialogName());

        if ((defaultsString != null) && (outputOptionsPanel != null)) {

            try {
                // Preferences.debug(defaultsString);

                StringTokenizer st = new StringTokenizer(defaultsString, ",");

                outputOptionsPanel.setProcessWholeImage(MipavUtil.getBoolean(st));

                image25DCheckbox.setSelected(MipavUtil.getBoolean(st));
                waterShedPanel.setConductance(MipavUtil.getFloat(st));
                waterShedPanel.setIterations(MipavUtil.getFloat(st));
                waterShedPanel.setThreshold(MipavUtil.getFloat(st));
                waterShedPanel.setLevel(MipavUtil.getFloat(st));
                waterShedPanel.setTimeStep(MipavUtil.getFloat(st));

                colorChannelPanel.setRedProcessingRequested(MipavUtil.getBoolean(st));
                colorChannelPanel.setGreenProcessingRequested(MipavUtil.getBoolean(st));
                colorChannelPanel.setBlueProcessingRequested(MipavUtil.getBoolean(st));

                outputOptionsPanel.setOutputNewImage(MipavUtil.getBoolean(st));
            } catch (Exception ex) {

                // since there was a problem parsing the defaults string, start over with the original defaults
                Preferences.debug("Resetting defaults for dialog: " + getDialogName());
                Preferences.removeProperty(getDialogName());
            }
        }
    }

    /**
     * Saves the default settings into the Preferences file.
     */
    public void saveDefaults() {
        String defaultsString = new String(getParameterString(",") + "," + outputOptionsPanel.isOutputNewImageSet());
        Preferences.saveDialogDefaults(getDialogName(), defaultsString);
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
     * Once all the necessary variables are set, call the Gaussian Blur algorithm based on what type of image this is
     * and whether or not there is a separate destination image.
     */
    protected void callAlgorithm() {
        String name = makeImageName(image.getImageName(), "_WaterShed");

        if (image.getNDims() == 2) { // source image is 2D and kernel not separable

            if (outputOptionsPanel.isOutputNewImageSet()) {

                try {

                    // Make result image
                    if (image.getType() == ModelImage.ARGB) {
                        resultImage = new ModelImage(ModelImage.ARGB, image.getExtents(), name);
                    } else if (image.getType() == ModelImage.ARGB_USHORT) {
                        resultImage = new ModelImage(ModelImage.ARGB_USHORT, image.getExtents(), name);
                    } else if (image.getType() == ModelImage.ARGB_FLOAT) {
                        resultImage = new ModelImage(ModelImage.ARGB_FLOAT, image.getExtents(), name);
                    } else {

                        // resultImage     = new ModelImage(ModelImage.FLOAT, destExtents, name, userInterface);
                        resultImage = (ModelImage) image.clone();
                        resultImage.setImageName(name);

                        if ((resultImage.getFileInfo()[0]).getFileFormat() == FileUtility.DICOM) {
                            ((FileInfoDicom) (resultImage.getFileInfo(0))).setSecondaryCaptureTags();
                        }
                    }

                    ModelImage float_src_image = image;
                    if (image.getType() != ModelImage.ARGB_FLOAT ||
                        image.getType() != ModelImage.FLOAT) {
                        float_src_image = (ModelImage) image.clone();
                        try {
                            float_src_image.convertToFloat();
                        } catch (java.io.IOException ioe) {
                            return;
                        }
                    }
                    // Make algorithm
                    waterShedlAlgo = new AlgorithmWaterShedITK(resultImage, float_src_image, waterShedPanel.getParameters(), false);

                    // This is very important. Adding this object as a listener allows the algorithm to
                    // notify this object when it has completed of failed. See algorithm performed event.
                    // This is made possible by implementing AlgorithmedPerformed interface
                    waterShedlAlgo.addListener(this);

                    createProgressBar(image.getImageName(), waterShedlAlgo);

                    waterShedlAlgo.setRed(colorChannelPanel.isRedProcessingRequested());
                    waterShedlAlgo.setGreen(colorChannelPanel.isGreenProcessingRequested());
                    waterShedlAlgo.setBlue(colorChannelPanel.isBlueProcessingRequested());

                    if (!outputOptionsPanel.isProcessWholeImageSet()) {
                    	waterShedlAlgo.setMask(image.generateVOIMask());
                    }

                    // Hide dialog
                    setVisible(false);

                    if (isRunInSeparateThread()) {

                        // Start the thread as a low priority because we wish to still have user interface work fast.
                        if (waterShedlAlgo.startMethod(Thread.MIN_PRIORITY) == false) {
                            MipavUtil.displayError("A thread is already running on this object");
                        }
                    } else {
                    	waterShedlAlgo.run();
                    }
                } catch (OutOfMemoryError x) {

                    if (resultImage != null) {
                        resultImage.disposeLocal(); // Clean up memory of result image
                        resultImage = null;
                    }

                    System.gc();
                    MipavUtil.displayError("Dialog Gaussian blur: unable to allocate enough memory");

                    return;
                }
            } else {

                try {

                    if (image.getType() != ModelImage.ARGB_FLOAT ||
                        image.getType() != ModelImage.FLOAT) {
                        MipavUtil.displayError("Cannot replace original image unless pixels are of type Float. Please convert the input image first.");

                        return;
                    }
                    // No need to make new image space because the user has choosen to replace the source image
                    // Make the algorithm class
                    waterShedlAlgo = new AlgorithmWaterShedITK(image, waterShedPanel.getParameters(), false);

                    // This is very important. Adding this object as a listener allows the algorithm to
                    // notify this object when it has completed of failed. See algorithm performed event.
                    // This is made possible by implementing AlgorithmedPerformed interface
                    waterShedlAlgo.addListener(this);
                    createProgressBar(image.getImageName(), waterShedlAlgo);

                    waterShedlAlgo.setRed(colorChannelPanel.isRedProcessingRequested());
                    waterShedlAlgo.setGreen(colorChannelPanel.isGreenProcessingRequested());
                    waterShedlAlgo.setBlue(colorChannelPanel.isBlueProcessingRequested());

                    if (!outputOptionsPanel.isProcessWholeImageSet()) {
                    	waterShedlAlgo.setMask(image.generateVOIMask());
                    }

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
                        if (waterShedlAlgo.startMethod(Thread.MIN_PRIORITY) == false) {
                            MipavUtil.displayError("A thread is already running on this object");
                        }
                    } else {
                    	waterShedlAlgo.run();
                    }
                } catch (OutOfMemoryError x) {
                    System.gc();
                    MipavUtil.displayError("Dialog Gaussian blur: unable to allocate enough memory");

                    return;
                }
            }
        } else if (image.getNDims() >= 3) { // kerenl not separable

            if (outputOptionsPanel.isOutputNewImageSet()) {

                try {

                    // Make result image
                    if (image.getType() == ModelImage.ARGB) {
                        resultImage = new ModelImage(ModelImage.ARGB, image.getExtents(), name);
                    } else if (image.getType() == ModelImage.ARGB_USHORT) {
                        resultImage = new ModelImage(ModelImage.ARGB_USHORT, image.getExtents(), name);
                    } else if (image.getType() == ModelImage.ARGB_FLOAT) {
                        resultImage = new ModelImage(ModelImage.ARGB_FLOAT, image.getExtents(), name);
                    } else {

                        // resultImage     = new ModelImage(ModelImage.FLOAT, destExtents, name, userInterface);
                        resultImage = (ModelImage) image.clone();
                        resultImage.setImageName(name);

                        if ((resultImage.getFileInfo()[0]).getFileFormat() == FileUtility.DICOM) {

                            for (int i = 0; i < resultImage.getExtents()[2]; i++) {
                                ((FileInfoDicom) (resultImage.getFileInfo(i))).setSecondaryCaptureTags();
                            }
                        }
                    }

                    ModelImage float_src_image = image;
                    if (image.getType() != ModelImage.ARGB_FLOAT ||
                        image.getType() != ModelImage.FLOAT) {
                        float_src_image = (ModelImage) image.clone();
                        try {
                            float_src_image.convertToFloat();
                        } catch (java.io.IOException ioe) {
                            return;
                        }
                    }
                    // Make algorithm
                    waterShedlAlgo = new AlgorithmWaterShedITK(resultImage, float_src_image, waterShedPanel.getParameters(), image25D);

                    // This is very important. Adding this object as a listener allows the algorithm to
                    // notify this object when it has completed of failed. See algorithm performed event.
                    // This is made possible by implementing AlgorithmedPerformed interface
                    waterShedlAlgo.addListener(this);

                    createProgressBar(image.getImageName(), waterShedlAlgo);

                    waterShedlAlgo.setRed(colorChannelPanel.isRedProcessingRequested());
                    waterShedlAlgo.setGreen(colorChannelPanel.isGreenProcessingRequested());
                    waterShedlAlgo.setBlue(colorChannelPanel.isBlueProcessingRequested());

                    if (!outputOptionsPanel.isProcessWholeImageSet()) {
                    	waterShedlAlgo.setMask(image.generateVOIMask());
                    }

                    // Hide dialog
                    setVisible(false);

                    if (isRunInSeparateThread()) {

                        // Start the thread as a low priority because we wish to still have user interface work fast.
                        if (waterShedlAlgo.startMethod(Thread.MIN_PRIORITY) == false) {
                            MipavUtil.displayError("A thread is already running on this object");
                        }
                    } else {
                    	waterShedlAlgo.run();
                    }
                } catch (OutOfMemoryError x) {

                    if (resultImage != null) {
                        resultImage.disposeLocal(); // Clean up image memory
                        resultImage = null;
                    }

                    System.gc();
                    MipavUtil.displayError("Dialog Gaussian blur: unable to allocate enough memory");

                    return;
                }
            } else {

                try {

                    if (image.getType() != ModelImage.ARGB_FLOAT ||
                        image.getType() != ModelImage.FLOAT) {
                        MipavUtil.displayError("Cannot replace original image unless pixels are of type Float. Please convert the input image first.");

                        return;
                    }
                    // Make algorithm
                    waterShedlAlgo = new AlgorithmWaterShedITK(image, waterShedPanel.getParameters(), image25D);

                    // This is very important. Adding this object as a listener allows the algorithm to
                    // notify this object when it has completed of failed. See algorithm performed event.
                    // This is made possible by implementing AlgorithmedPerformed interface
                    waterShedlAlgo.addListener(this);

                    createProgressBar(image.getImageName(), waterShedlAlgo);

                    waterShedlAlgo.setRed(colorChannelPanel.isRedProcessingRequested());
                    waterShedlAlgo.setGreen(colorChannelPanel.isGreenProcessingRequested());
                    waterShedlAlgo.setBlue(colorChannelPanel.isBlueProcessingRequested());

                    if (!outputOptionsPanel.isProcessWholeImageSet()) {
                    	waterShedlAlgo.setMask(image.generateVOIMask());
                    }

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
                        if (waterShedlAlgo.startMethod(Thread.MIN_PRIORITY) == false) {
                            MipavUtil.displayError("A thread is already running on this object");
                        }
                    } else {
                    	waterShedlAlgo.run();
                    }
                } catch (OutOfMemoryError x) {
                    System.gc();
                    MipavUtil.displayError("Dialog Gaussian blur: unable to allocate enough memory");

                    return;
                }
            }
        }
    }

    /**
     * Perform any actions required after the running of the algorithm is complete.
     */
    protected void doPostAlgorithmActions() {

        if (outputOptionsPanel.isOutputNewImageSet()) {
            AlgorithmParameters.storeImageInRunner(getResultImage());
        }
    }

    /**
     * Set up the dialog GUI based on the parameters before running the algorithm as part of a script.
     */
    protected void setGUIFromParams() {
        image = scriptParameters.retrieveInputImage();
        userInterface = ViewUserInterface.getReference();
        parentFrame = image.getParentFrame();

        outputOptionsPanel = new JPanelAlgorithmOutputOptions(image);
        waterShedPanel = new JPanelWaterShedITK();
        colorChannelPanel = new JPanelColorChannels(image);

        scriptParameters.setOutputOptionsGUI(outputOptionsPanel);
        setImage25D(scriptParameters.getParams().getBoolean(AlgorithmParameters.DO_PROCESS_3D_AS_25D));
        scriptParameters.setSigmasGUI(waterShedPanel);
        scriptParameters.setColorOptionsGUI(colorChannelPanel);
    }

    /**
     * Store the parameters from the dialog to record the execution of this algorithm.
     *
     * @throws  ParserException  If there is a problem creating one of the new parameters.
     */
    protected void storeParamsFromGUI() throws ParserException {
        scriptParameters.storeInputImage(image);
        scriptParameters.storeOutputImageParams(resultImage, outputOptionsPanel.isOutputNewImageSet());

        scriptParameters.storeProcessingOptions(outputOptionsPanel.isProcessWholeImageSet(), image25D);
        scriptParameters.storeWaterShed(waterShedPanel);
        scriptParameters.storeColorOptions(colorChannelPanel);
    }

    /**
     * Sets up the GUI (panels, buttons, etc) and displays it on the screen.
     */
    private void init() {
        setForeground(Color.black);

        setTitle("WaterShed");
        getContentPane().setLayout(new BorderLayout());

        waterShedPanel = new JPanelWaterShedITK();
        
        image25DCheckbox = WidgetFactory.buildCheckBox("Process each slice independently (2.5D)", false, this);

        if (image.getNDims() != 3) {
            image25DCheckbox.setEnabled(false);
        } else {
            image25DCheckbox.setSelected(image.getFileInfo()[0].getIs2_5D());
        }

        PanelManager kernelOptionsPanelManager = new PanelManager("Options");
        kernelOptionsPanelManager.add(image25DCheckbox);

        colorChannelPanel = new JPanelColorChannels(image);
        outputOptionsPanel = new JPanelAlgorithmOutputOptions(image);

        PanelManager paramPanelManager = new PanelManager();
        paramPanelManager.add(waterShedPanel);
        paramPanelManager.addOnNextLine(kernelOptionsPanelManager.getPanel());
        paramPanelManager.addOnNextLine(colorChannelPanel);
        paramPanelManager.addOnNextLine(outputOptionsPanel);

        getContentPane().add(paramPanelManager.getPanel(), BorderLayout.CENTER);
        getContentPane().add(buildButtons(), BorderLayout.SOUTH);
        pack();
        setResizable(true);

        System.gc();
    }

    /**
     * Use the GUI results to set up the variables needed to run the algorithm.
     *
     * @return  <code>true</code> if parameters set successfully, <code>false</code> otherwise.
     */
    private boolean setVariables() {

        if (image25DCheckbox.isSelected()) {
            image25D = true;
        } else {
            image25D = false;
        }
                
        return true;
    }
}
