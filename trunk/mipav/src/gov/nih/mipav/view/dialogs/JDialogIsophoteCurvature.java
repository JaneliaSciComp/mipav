package gov.nih.mipav.view.dialogs;


import gov.nih.mipav.model.algorithms.*;
import gov.nih.mipav.model.file.*;
import gov.nih.mipav.model.scripting.*;
import gov.nih.mipav.model.scripting.parameters.*;
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
 * new image or replace the source image. User can indicate whether to have algorithm applied to whole image or to the
 * VOI regions. Algorithms are executed in their own thread.
 *
 * @version  0.1 May 29, 2013
 * @author   William Gandler
 * @see      AlgorithmIsophoteCurvature
 */
public class JDialogIsophoteCurvature extends JDialogScriptableBase implements AlgorithmInterface, ActionDiscovery, ScriptableActionInterface {

    //~ Static fields/initializers -------------------------------------------------------------------------------------

    /** Use serialVersionUID for interoperability. */
    //private static final long serialVersionUID;

    //~ Instance fields ------------------------------------------------------------------------------------------------


    /** DOCUMENT ME! */
    private ModelImage image; // source image

    /** DOCUMENT ME! */
    private boolean image25D = false;
    
    /** DOCUMENT ME! */
    private JCheckBox image25DCheckbox;
    

    /** DOCUMENT ME! */
    private AlgorithmIsophoteCurvature isoCurvAlgo;

    /** DOCUMENT ME! */
    private JPanelAlgorithmOutputOptions outputPanel;

    /** DOCUMENT ME! */
    private ModelImage resultImage = null; // result image

    /** DOCUMENT ME! */
    private JPanelSigmas sigmasPanel;

    /** DOCUMENT ME! */
    private String[] titles;

    /** DOCUMENT ME! */
    private ViewUserInterface userInterface;
    
    /** display progress bar or not. */
    private boolean displayProgressBar = true;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Empty constructor needed for dynamic instantiation (used during scripting).
     */
    public JDialogIsophoteCurvature() { }

    /**
     * Creates a new JDialogLaplacian object.
     *
     * @param  theParentFrame  Parent frame.
     * @param  im              Source image.
     */
    public JDialogIsophoteCurvature(Frame theParentFrame, ModelImage im) {
        super(theParentFrame, false);
        image = im;
        userInterface = ViewUserInterface.getReference();
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
            //MipavUtil.showWebHelp("");
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
     * @param  algorithm  Algorithm that caused the event.
     */
    public void algorithmPerformed(AlgorithmBase algorithm) {
        Preferences.debug("AlgorithmIsoophoteCurvature: " + algorithm.getElapsedTime());

       
        if (algorithm instanceof AlgorithmIsophoteCurvature) {
            image.clearMask();

            if ((isoCurvAlgo.isCompleted() == true) && (resultImage != null)) {

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

        if (isoCurvAlgo != null) {
            isoCurvAlgo.finalize();
            isoCurvAlgo = null;
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

    // *******************************************************************
    // ************************* Item Events ****************************
    // *******************************************************************

    /**
     * Resets labels if checkboxes are checked or unchecked.
     *
     * @param  event  Event that cause the method to fire.
     */
    public void itemStateChanged(ItemEvent event) {
        Object source = event.getSource();

        if (source == image25DCheckbox) {
            sigmasPanel.enable3DComponents(!image25DCheckbox.isSelected());
        }
        
    }

    /**
     * Loads the default settings from Preferences to set up the dialog.
     */
    public void legacyLoadDefaults() {
        String defaultsString = Preferences.getDialogDefaults(getDialogName());

        if ((defaultsString != null) && (outputPanel != null)) {

            try {
                StringTokenizer st = new StringTokenizer(defaultsString, ",");

                outputPanel.setProcessWholeImage(MipavUtil.getBoolean(st));
                outputPanel.setOutputNewImage(MipavUtil.getBoolean(st));

                image25DCheckbox.setSelected(MipavUtil.getBoolean(st));

                sigmasPanel.setSigmaX(MipavUtil.getFloat(st));
                sigmasPanel.setSigmaY(MipavUtil.getFloat(st));
                sigmasPanel.setSigmaZ(MipavUtil.getFloat(st));
                sigmasPanel.enableResolutionCorrection(MipavUtil.getBoolean(st));
            } catch (Exception ex) {

                // since there was a problem parsing the defaults string, start over with the original defaults
                Preferences.debug("Resetting defaults for dialog: " + getDialogName());
                Preferences.removeProperty(getDialogName());
                ex.printStackTrace();
            }
        }

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
     * Changes whether a new image should be generated by the algorithm.
     *
     * @param  flag  true if a new image should be made, false otherwise
     */
    public void setOutputNewImage(boolean flag)
    {
        outputPanel.setOutputNewImage(flag);
    }
    
    /**
     * Set the display progress bar flag. 
     * @param flag  display or not
     */
    public void setDisplayProgressBar(boolean flag) {
    	displayProgressBar = flag;
    }
    
    

    /**
     * Once all the necessary variables are set, call the Isophote Curvature algorithm based on what type of image this is
     * and whether or not there is a separate destination image.
     */
    protected void callAlgorithm() {
        String name = makeImageName(image.getImageName(), "_isophoteCurvature");  
    	displayInNewFrame = outputPanel.isOutputNewImageSet();

        

        if (image.getNDims() == 2) { // source image is 2D

            float[] sigmas = sigmasPanel.getNormalizedSigmas();

            if (outputPanel.isOutputNewImageSet()) {

                try {

                    // Make result image of float type
                    resultImage = new ModelImage(ModelImage.FLOAT, image.getExtents(), name);

                    // resultImage = (ModelImage)image.clone();
                    // resultImage.setImageName(name);
                    if ((resultImage.getFileInfo()[0]).getFileFormat() == FileUtility.DICOM) {
                        ((FileInfoDicom) (resultImage.getFileInfo(0))).setSecondaryCaptureTags();
                    }

                    // Make algorithm
                    isoCurvAlgo = new AlgorithmIsophoteCurvature(resultImage, image, sigmas,
                                                           outputPanel.isProcessWholeImageSet(), false);

                    // This is very important. Adding this object as a listener allows the algorithm to
                    // notify this object when it has completed of failed. See algorithm performed event.
                    // This is made possible by implementing AlgorithmedPerformed interface
                    isoCurvAlgo.addListener(this);

                    if ( displayProgressBar )
                    	createProgressBar(image.getImageName(), isoCurvAlgo);

                    // Hide dialog
                    setVisible(false);

                    if (isRunInSeparateThread()) {

                        // Start the thread as a low priority because we wish to still have user interface work fast
                        if (isoCurvAlgo.startMethod(Thread.MIN_PRIORITY) == false) {
                            MipavUtil.displayError("A thread is already running on this object");
                        }
                    } else {

                        isoCurvAlgo.run();
                    }
                } catch (OutOfMemoryError x) {
                    MipavUtil.displayError("Dialog Isophote Curvature: unable to allocate enough memory");

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
                    isoCurvAlgo = new AlgorithmIsophoteCurvature(image, sigmas, outputPanel.isProcessWholeImageSet(), false);

                    // This is very important. Adding this object as a listener allows the algorithm to
                    // notify this object when it has completed of failed. See algorithm performed event.
                    // This is made possible by implementing AlgorithmedPerformed interface
                    isoCurvAlgo.addListener(this);

                    if ( displayProgressBar )
                    	createProgressBar(image.getImageName(), isoCurvAlgo);

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

                        // Start the thread as a low priority because we wish to still have user interface work fast
                        if (isoCurvAlgo.startMethod(Thread.MIN_PRIORITY) == false) {
                            MipavUtil.displayError("A thread is already running on this object");
                        }
                    } else {

                        isoCurvAlgo.run();
                    }
                } catch (OutOfMemoryError x) {
                    MipavUtil.displayError("Dialog Isophote Curvature: unable to allocate enough memory");

                    return;
                }
            }
        } else if (image.getNDims() == 3) {

            float[] sigmas = sigmasPanel.getNormalizedSigmas();

            if (outputPanel.isOutputNewImageSet()) {

                try {
                    // Make result image of float type

                    resultImage = new ModelImage(ModelImage.FLOAT, image.getExtents(), name);
                    //resultImage = (ModelImage) image.clone();
                    resultImage.setImageName(name);

                    if ((resultImage.getFileInfo()[0]).getFileFormat() == FileUtility.DICOM) {

                        for (int i = 0; i < resultImage.getExtents()[2]; i++) {
                            ((FileInfoDicom) (resultImage.getFileInfo(i))).setSecondaryCaptureTags();
                        }
                    }

                    // Make algorithm
                    isoCurvAlgo = new AlgorithmIsophoteCurvature(resultImage, image, sigmas,
                                                           outputPanel.isProcessWholeImageSet(), image25D);

                    // This is very important. Adding this object as a listener allows the algorithm to
                    // notify this object when it has completed of failed. See algorithm performed event.
                    // This is made possible by implementing AlgorithmedPerformed interface
                    isoCurvAlgo.addListener(this);

                    if ( displayProgressBar )
                    	createProgressBar(image.getImageName(), isoCurvAlgo);

                    // Hide dialog
                    setVisible(false);

                    if (isRunInSeparateThread()) {

                        // Start the thread as a low priority because we wish to still have user interface work fast
                        if (isoCurvAlgo.startMethod(Thread.MIN_PRIORITY) == false) {
                            MipavUtil.displayError("A thread is already running on this object");
                        }
                    } else {

                        isoCurvAlgo.run();
                    }
                } catch (OutOfMemoryError x) {
                    MipavUtil.displayError("Dialog Isophote Curvature: unable to allocate enough memory");

                    if (resultImage != null) {
                        resultImage.disposeLocal(); // Clean up image memory
                        resultImage = null;
                    }

                    return;
                }
            } else {

                try {

                    // Make algorithm
                    isoCurvAlgo = new AlgorithmIsophoteCurvature(image, sigmas, outputPanel.isProcessWholeImageSet(), image25D);

                    // This is very important. Adding this object as a listener allows the algorithm to
                    // notify this object when it has completed of failed. See algorithm performed event.
                    // This is made possible by implementing AlgorithmedPerformed interface
                    isoCurvAlgo.addListener(this);
                    if ( displayProgressBar )
                    	createProgressBar(image.getImageName(), isoCurvAlgo);

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
                        if (isoCurvAlgo.startMethod(Thread.MIN_PRIORITY) == false) {
                            MipavUtil.displayError("A thread is already running on this object");
                        }
                    } else {

                        isoCurvAlgo.run();
                    }
                } catch (OutOfMemoryError x) {
                    MipavUtil.displayError("Dialog Isophote Curvature: unable to allocate enough memory");

                    return;
                }
            }
        }

    }

    /**
     * Store the result image in the script runner's image table now that the action execution is finished.
     */
    protected void doPostAlgorithmActions() {

        if (outputPanel.isOutputNewImageSet()) {
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

        outputPanel = new JPanelAlgorithmOutputOptions(image);
        sigmasPanel = new JPanelSigmas(image);

        scriptParameters.setOutputOptionsGUI(outputPanel);
        setImage25D(scriptParameters.getParams().getBoolean(AlgorithmParameters.DO_PROCESS_3D_AS_25D));
        scriptParameters.setSigmasGUI(sigmasPanel);
    }

    /**
     * {@inheritDoc}
     */
    protected void storeParamsFromGUI() throws ParserException {
        scriptParameters.storeInputImage(image);
        scriptParameters.storeOutputImageParams(resultImage, outputPanel.isOutputNewImageSet());

        scriptParameters.storeProcessingOptions(outputPanel.isProcessWholeImageSet(), image25D);
        scriptParameters.storeSigmas(sigmasPanel);
    }

    /**
     * Initializes the GUI by creating the components, placing them in the dialog, and displaying them.
     */
    private void init() {
        setForeground(Color.black);

        getContentPane().setLayout(new BorderLayout());
        setTitle("Isophote Curvature");

        JPanel mainPanel = new JPanel(new GridLayout());
        mainPanel.setBorder(BorderFactory.createEmptyBorder(3, 3, 3, 3));
        mainPanel.setLayout(new GridBagLayout());

        GridBagConstraints gbc = new GridBagConstraints();
        gbc.gridwidth = 1;
        gbc.gridheight = 1;
        gbc.anchor = GridBagConstraints.WEST;
        gbc.weightx = 1;
        gbc.fill = GridBagConstraints.HORIZONTAL;
        gbc.gridx = 0;
        gbc.gridy = 0;

        sigmasPanel = new JPanelSigmas(image);
        mainPanel.add(sigmasPanel, gbc);

        PanelManager algoPanel = new PanelManager("Algorithm options");
        
        image25DCheckbox = new JCheckBox("Process each slice independently (2.5D)");
        image25DCheckbox.setFont(serif12);
        algoPanel.addOnNextLine(image25DCheckbox);
        image25DCheckbox.setSelected(false);
        image25DCheckbox.addItemListener(this);

        if (image.getNDims() != 3) {
            image25DCheckbox.setEnabled(false);
        }

        gbc.gridy++;
        mainPanel.add(algoPanel.getPanel(), gbc);

        outputPanel = new JPanelAlgorithmOutputOptions(image);
        // Must be able to have negative and positive curvatures
        if ((image.getType() == ModelStorageBase.UBYTE) || (image.getType() == ModelStorageBase.USHORT) ||
            (image.getType() == ModelStorageBase.UINTEGER)) {
            outputPanel.setOutputNewImage(true);
            outputPanel.setOutputImageOptionsEnabled(false);
        }
        gbc.gridy++;
        mainPanel.add(outputPanel, gbc);

        getContentPane().add(mainPanel, BorderLayout.CENTER);
        getContentPane().add(buildButtons(), BorderLayout.SOUTH);
        pack();
        setResizable(true);
        setVisible(true);

        System.gc();
    }

    /**
     * Use the GUI results to set up the variables needed to run the algorithm.
     *
     * @return  <code>true</code> if parameters set successfully, <code>false</code> otherwise.
     */
    private boolean setVariables() {

        System.gc();

        image25D = image25DCheckbox.isSelected();

        if (!sigmasPanel.testSigmaValues()) {
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
                return new String("Algorithms");
            }

            public String getDescription() {
                return new String("Calculates the Isophote Curvature of an image.");
            }

            public String getDescriptionLong() {
                return new String("Calculates the Isophote Curvature of an image.");
            }

            public String getShortLabel() {
                return new String("Isophote Curvature");
            }

            public String getLabel() {
                return new String("Isophote Curvature");
            }

            public String getName() {
                return new String("Isophote Curvature");
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
            table.put(new ParameterBoolean(AlgorithmParameters.DO_PROCESS_WHOLE_IMAGE, true));
            table.put(new ParameterBoolean(AlgorithmParameters.DO_PROCESS_3D_AS_25D, false));
            table.put(new ParameterList(AlgorithmParameters.SIGMAS, Parameter.PARAM_FLOAT, "1.0,1.0,1.0"));
            table.put(new ParameterBoolean(AlgorithmParameters.SIGMA_DO_Z_RES_CORRECTION, true));
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
