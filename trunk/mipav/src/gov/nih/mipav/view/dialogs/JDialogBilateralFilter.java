package gov.nih.mipav.view.dialogs;


import gov.nih.mipav.model.algorithms.AlgorithmBase;
import gov.nih.mipav.model.algorithms.AlgorithmInterface;
import gov.nih.mipav.model.algorithms.filters.AlgorithmBilateralFilter;
import gov.nih.mipav.model.file.FileInfoDicom;
import gov.nih.mipav.model.file.FileUtility;
import gov.nih.mipav.model.scripting.ParserException;
import gov.nih.mipav.model.scripting.ScriptableActionInterface;
import gov.nih.mipav.model.scripting.parameters.*;
import gov.nih.mipav.model.structures.ModelImage;
import gov.nih.mipav.view.DialogDefaultsInterface;
import gov.nih.mipav.view.MipavUtil;
import gov.nih.mipav.view.Preferences;
import gov.nih.mipav.view.ViewImageUpdateInterface;
import gov.nih.mipav.view.ViewUserInterface;
import gov.nih.mipav.view.components.JPanelAlgorithmOutputOptions;
import gov.nih.mipav.view.components.JPanelSigmas;
import gov.nih.mipav.view.components.PanelManager;
import gov.nih.mipav.view.components.WidgetFactory;

import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.Frame;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.event.ActionEvent;
import java.awt.event.ItemEvent;
import java.util.StringTokenizer;
import java.util.Vector;

import javax.swing.JPanel;
import javax.swing.JLabel;
import javax.swing.JTextField;
import javax.swing.JCheckBox;


/**
 * Dialog to get user input, then call the algorithm. The user is able to control the degree of blurring in all
 * dimensions and indicate if a correction factor be applied to the z-dimension to account for differing resolutions
 * between the xy resolutions (intra-plane) and the z resolution (inter-plane). In addition to the spatial domain
 * filtering, range filtering also occurs so that pixels with similar intensity values are more heavily weighed.
 * The user has the option to generate a new image or replace the source image. In addition the user can indicate
 * if he/she wishes to have the algorithm applied to whole image or to the VOI regions. It should be noted that the
 * algorithms are executed in their own thread.
 *
 * @version  0.1 February 5, 2009
 * @author   William Gandler
 * @see      AlgorithmBilateralFilter
 */
public class JDialogBilateralFilter extends JDialogScriptableBase implements AlgorithmInterface, DialogDefaultsInterface, ActionDiscovery, ScriptableActionInterface {

    //~ Static fields/initializers -------------------------------------------------------------------------------------

    /** Use serialVersionUID for interoperability. */
    //private static final long serialVersionUID;

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** DOCUMENT ME! */
    private AlgorithmBilateralFilter bilateralFilterAlgo;

    /** Source image. */
    private ModelImage image;

    /** Flag indicating if slices should be blurred independently. */
    private boolean image25D = false;

    /** DOCUMENT ME! */
    private JCheckBox image25DCheckbox;

    /** DOCUMENT ME! */
    private JPanelAlgorithmOutputOptions outputOptionsPanel;

    /** Result image. */
    private ModelImage resultImage = null;

    /** DOCUMENT ME! */
    private JPanelSigmas sigmaPanel;
    
    private JPanel intensityPanel;
    
    private JLabel intensityLabel;
    private JTextField intensityText;
    
    // units of intensity range; multiplied by intensity range to create intensity sigma.
    private float intensityFraction;

    /** DOCUMENT ME! */
    private String[] titles;

    /** DOCUMENT ME! */
    private ViewUserInterface userInterface;
    
    /**
     * Empty constructor needed for dynamic instantiation.
     */
    public JDialogBilateralFilter() { }

    /**
     * Construct the bilateral filter dialog.
     *
     * @param  theParentFrame  Parent frame.
     * @param  im              Source image.
     */
    public JDialogBilateralFilter(Frame theParentFrame, ModelImage im) {
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

        if (command.equals("OK")) {

            if (setVariables()) {
                callAlgorithm();
            }
        } else if (command.equals("Cancel")) {
            dispose();
        } else if (command.equals("Help")) {
            //MipavUtil.showHelp("");
        } else {
            super.actionPerformed(event);
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

        if (algorithm instanceof AlgorithmBilateralFilter) {
            Preferences.debug("Bilateral Filter Elapsed: " + algorithm.getElapsedTime());
            image.clearMask();

            if ((bilateralFilterAlgo.isCompleted() == true) && (resultImage != null)) {

                // The algorithm has completed and produced a new image to be displayed.
                if (resultImage.isColorImage()) {
                    updateFileInfo(image, resultImage);
                }

                resultImage.clearMask();

                try {
                	openNewFrame(resultImage);
                 //   openNewFrame(resultImage);
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
        } // if (algorithm instanceof AlgorithmGaussianBlur)

        

        if (bilateralFilterAlgo != null) {
        	// save the completion status for later
        	setComplete(algorithm.isCompleted());

            bilateralFilterAlgo.finalize();
            bilateralFilterAlgo = null;
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
     * Changes labels based on whether or not check box is checked.
     *
     * @param  event  event that cause the method to fire
     */
    public void itemStateChanged(ItemEvent event) {
        Object source = event.getSource();

        if (source == image25DCheckbox) {
            sigmaPanel.enable3DComponents(!image25DCheckbox.isSelected());
        }
    }

    /**
     * Loads the default settings from Preferences to set up the dialog.
     */
    public void loadDefaults() {
        String defaultsString = Preferences.getDialogDefaults(getDialogName());

        if ((defaultsString != null) && (outputOptionsPanel != null)) {

            try {
                StringTokenizer st = new StringTokenizer(defaultsString, ",");
                outputOptionsPanel.setProcessWholeImage(MipavUtil.getBoolean(st));
                outputOptionsPanel.setOutputNewImage(MipavUtil.getBoolean(st));

                image25DCheckbox.setSelected(MipavUtil.getBoolean(st));
                sigmaPanel.setSigmaX(MipavUtil.getFloat(st));
                sigmaPanel.setSigmaY(MipavUtil.getFloat(st));
                sigmaPanel.setSigmaZ(MipavUtil.getFloat(st));
                sigmaPanel.enableResolutionCorrection(MipavUtil.getBoolean(st));
                intensityFraction = MipavUtil.getFloat(st);
                intensityText.setText(String.valueOf(intensityFraction));
                
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
        String delim = ",";
        String defaultsString = outputOptionsPanel.isProcessWholeImageSet() + delim;
        defaultsString += outputOptionsPanel.isOutputNewImageSet() + delim;
        defaultsString += image25D + delim;
        defaultsString += sigmaPanel.getUnnormalized3DSigmas()[0] + delim;
        defaultsString += sigmaPanel.getUnnormalized3DSigmas()[1] + delim;
        defaultsString += sigmaPanel.getUnnormalized3DSigmas()[2] + delim;
        defaultsString += sigmaPanel.isResolutionCorrectionEnabled() + delim;
        defaultsString += intensityFraction;

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
     * Once all the necessary variables are set, call the Bilateral Filter algorithm based on what type of image this is
     * and whether or not there is a separate destination image.
     */
    protected void callAlgorithm() {
        String name = makeImageName(image.getImageName(), "_bilFil");
        displayInNewFrame = outputOptionsPanel.isOutputNewImageSet();
        
		if (image.getNDims() == 2) { // source image is 2D and kernel not
											// separable

            float[] sigmas = sigmaPanel.getNormalizedSigmas();

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

                    // Make algorithm
                    bilateralFilterAlgo = new AlgorithmBilateralFilter(resultImage, image, sigmas, intensityFraction, 
                                                                 outputOptionsPanel.isProcessWholeImageSet(), false);

                    // This is very important. Adding this object as a listener allows the algorithm to
                    // notify this object when it has completed of failed. See algorithm performed event.
                    // This is made possible by implementing AlgorithmedPerformed interface
                    bilateralFilterAlgo.addListener(this);

                    createProgressBar(image.getImageName(), bilateralFilterAlgo);

                    if (!outputOptionsPanel.isProcessWholeImageSet()) {
                        bilateralFilterAlgo.setMask(image.generateVOIMask());
                    }

                    // Hide dialog
                    setVisible(false);

                    if (isRunInSeparateThread()) {

                        // Start the thread as a low priority because we wish to still have user interface work fast.
                        if (bilateralFilterAlgo.startMethod(Thread.MIN_PRIORITY) == false) {
                            MipavUtil.displayError("A thread is already running on this object");
                        }
                    } else {
                        bilateralFilterAlgo.run();
                    }
                } catch (OutOfMemoryError x) {

                    if (resultImage != null) {
                        resultImage.disposeLocal(); // Clean up memory of result image
                        resultImage = null;
                    }

                    System.gc();
                    MipavUtil.displayError("Dialog Bilateral Filter: unable to allocate enough memory");

                    return;
                }
            } else {

                try {

                    // No need to make new image space because the user has choosen to replace the source image
                    // Make the algorithm class
                    bilateralFilterAlgo = new AlgorithmBilateralFilter(image, sigmas, intensityFraction, 
                                                                 outputOptionsPanel.isProcessWholeImageSet(), false);

                    // This is very important. Adding this object as a listener allows the algorithm to
                    // notify this object when it has completed of failed. See algorithm performed event.
                    // This is made possible by implementing AlgorithmedPerformed interface
                    bilateralFilterAlgo.addListener(this);

                    createProgressBar(image.getImageName(), bilateralFilterAlgo);

                    if (!outputOptionsPanel.isProcessWholeImageSet()) {
                        bilateralFilterAlgo.setMask(image.generateVOIMask());
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
                        if (bilateralFilterAlgo.startMethod(Thread.MIN_PRIORITY) == false) {
                            MipavUtil.displayError("A thread is already running on this object");
                        }
                    } else {
                        bilateralFilterAlgo.run();
                    }
                } catch (OutOfMemoryError x) {
                    System.gc();
                    MipavUtil.displayError("Dialog BilateralFilter: unable to allocate enough memory");

                    return;
                }
            }
        } else if (image.getNDims() >= 3) { // kerenl not separable

            float[] sigmas = sigmaPanel.getNormalizedSigmas();

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

                    // Make algorithm
                    bilateralFilterAlgo = new AlgorithmBilateralFilter(resultImage, image, sigmas, intensityFraction, 
                                                                 outputOptionsPanel.isProcessWholeImageSet(), image25D);

                    // This is very important. Adding this object as a listener allows the algorithm to
                    // notify this object when it has completed of failed. See algorithm performed event.
                    // This is made possible by implementing AlgorithmedPerformed interface
                    bilateralFilterAlgo.addListener(this);

                    createProgressBar(image.getImageName(), bilateralFilterAlgo);

                    if (!outputOptionsPanel.isProcessWholeImageSet()) {
                        bilateralFilterAlgo.setMask(image.generateVOIMask());
                    }

                    // Hide dialog
                    setVisible(false);

                    if (isRunInSeparateThread()) {

                        // Start the thread as a low priority because we wish to still have user interface work fast.
                        if (bilateralFilterAlgo.startMethod(Thread.MIN_PRIORITY) == false) {
                            MipavUtil.displayError("A thread is already running on this object");
                        }
                    } else {
                        bilateralFilterAlgo.run();
                        
                    }
                } catch (OutOfMemoryError x) {
                	
                    if (resultImage != null) {
                        resultImage.disposeLocal(); // Clean up image memory
                        resultImage = null;
                    }

                    System.gc();
                    MipavUtil.displayError("Dialog Bilateral Filter: unable to allocate enough memory");

                    return;
                }
            } else {

                try {

                    // Make algorithm
                    bilateralFilterAlgo = new AlgorithmBilateralFilter(image, sigmas, intensityFraction, 
                                                                 outputOptionsPanel.isProcessWholeImageSet(), image25D);

                    // This is very important. Adding this object as a listener allows the algorithm to
                    // notify this object when it has completed of failed. See algorithm performed event.
                    // This is made possible by implementing AlgorithmedPerformed interface
                    bilateralFilterAlgo.addListener(this);

                    if (!outputOptionsPanel.isProcessWholeImageSet()) {
                        bilateralFilterAlgo.setMask(image.generateVOIMask());
                    }

                    // Hide dialog
                    setVisible(false);

                    createProgressBar(image.getImageName(), bilateralFilterAlgo);

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
                        if (bilateralFilterAlgo.startMethod(Thread.MIN_PRIORITY) == false) {
                            MipavUtil.displayError("A thread is already running on this object");
                        }
                    } else {
                        bilateralFilterAlgo.run();
                    }
                } catch (OutOfMemoryError x) {
                    System.gc();
                    MipavUtil.displayError("Dialog Bilateral Filter: unable to allocate enough memory");

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
        sigmaPanel = new JPanelSigmas(image);

        scriptParameters.setOutputOptionsGUI(outputOptionsPanel);
        setImage25D(scriptParameters.doProcess3DAs25D());
        scriptParameters.setSigmasGUI(sigmaPanel);
        intensityFraction = scriptParameters.getParams().getFloat("intensity_fraction");
        //intensityText.setText(String.valueOf(intensityFraction));
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
        scriptParameters.storeSigmas(sigmaPanel);
        scriptParameters.getParams().put(ParameterFactory.newParameter("intensity_fraction", intensityFraction));
    }

    /**
     * Sets up the GUI (panels, buttons, etc) and displays it on the screen.
     */
    private void init() {
        setForeground(Color.black);

        setTitle("Bilateral Filter");
        getContentPane().setLayout(new BorderLayout());

        sigmaPanel = new JPanelSigmas(image);
        sigmaPanel.setBorderName("Scale of the Spatial Gaussian");
        
        intensityPanel = new JPanel(new GridBagLayout());
        GridBagConstraints gbc = new GridBagConstraints();
        intensityPanel.setForeground(Color.black);
        intensityPanel.setBorder(buildTitledBorder("Scale of the Intensity Gaussian"));
        intensityLabel = new JLabel("Intensity sigma in units of intensity range");
        intensityLabel.setForeground(Color.black);
        intensityLabel.setFont(serif12);
        gbc.gridwidth = 1;
        gbc.anchor = GridBagConstraints.WEST;
        gbc.weightx = 1;
        gbc.gridx = 0;
        gbc.gridy = 0;
        intensityPanel.add(intensityLabel, gbc);
        intensityText = new JTextField("0.1");
        intensityText.setFont(serif12);
        intensityText.setColumns(5);
        gbc.anchor = GridBagConstraints.EAST;
        gbc.gridx = 1;
        intensityPanel.add(intensityText, gbc);

        image25DCheckbox = WidgetFactory.buildCheckBox("Process each slice independently (2.5D)", false, this);

        if (image.getNDims() != 3) {
            image25DCheckbox.setEnabled(false);
        } else {
            image25DCheckbox.setSelected(image.getFileInfo()[0].getIs2_5D());
        }

        PanelManager kernelOptionsPanelManager = new PanelManager("Options");
        kernelOptionsPanelManager.addOnNextLine(image25DCheckbox);

        outputOptionsPanel = new JPanelAlgorithmOutputOptions(image);

        PanelManager paramPanelManager = new PanelManager();
        paramPanelManager.add(sigmaPanel);
        paramPanelManager.addOnNextLine(intensityPanel);
        paramPanelManager.addOnNextLine(kernelOptionsPanelManager.getPanel());
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
        String tmpStr;

        if (image25DCheckbox.isSelected()) {
            image25D = true;
        } else {
            image25D = false;
        }

        if (!sigmaPanel.testSigmaValues()) {
            return false;
        }
        
        tmpStr = intensityText.getText();
        intensityFraction = Float.parseFloat(tmpStr);

        if (intensityFraction <= 0.0f) {
            MipavUtil.displayError("intensityFraction must be greater than 0.0");
            intensityText.requestFocus();
            intensityText.selectAll();

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
                return new String("Applies a bilateral filter.");
            }

            public String getDescriptionLong() {
                return new String("Applies a bilateral filter.");
            }

            public String getShortLabel() {
                return new String("BilateralFilter");
            }

            public String getLabel() {
                return new String("Bilateral Filter");
            }

            public String getName() {
                return new String("Bilateral Filter");
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
            table.put(new ParameterList(AlgorithmParameters.SIGMAS, Parameter.PARAM_FLOAT, "1.0,1.0,1.0"));
            table.put(new ParameterBoolean(AlgorithmParameters.DO_OUTPUT_NEW_IMAGE, true));
            table.put(new ParameterBoolean(AlgorithmParameters.DO_PROCESS_WHOLE_IMAGE, true));
            table.put(new ParameterBoolean(AlgorithmParameters.DO_PROCESS_3D_AS_25D, false));
            table.put(new ParameterBoolean(AlgorithmParameters.SIGMA_DO_Z_RES_CORRECTION, true));
            table.put(new ParameterFloat("intensity_fraction", .1f));
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
