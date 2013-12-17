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

import javax.swing.*;


/**
 * Dialog to get user input, then call the algorithm. The user is able to control the degree of blurring in all
 * dimensions. User can indicate whether to have algorithm applied to whole image or to the VOI regions. Algorithms are
 * executed in their own thread.
 *
 * @version  0.1 Nov 17, 1998
 * @author   Matthew J. McAuliffe, Ph.D.
 * @see      gov.nih.mipav.model.algorithms.filters.AlgorithmGaussianBlur
 */
public class JDialogEdgeLaplacian extends JDialogScriptableBase implements AlgorithmInterface, ActionDiscovery, ScriptableActionInterface {

    //~ Static fields/initializers -------------------------------------------------------------------------------------

    /** Use serialVersionUID for interoperability. */
    private static final long serialVersionUID = -4875093981299828397L;

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** DOCUMENT ME! */
    private ModelImage edgeImage = null;

    /** DOCUMENT ME! */
    private ModelImage image; // source image

    /** false = apply algorithm only to VOI regions. */
    private boolean image25D = false; // Flag for applying to every slice

    /** DOCUMENT ME! */
    private JCheckBox image25DCheckbox;

    /** DOCUMENT ME! */
    private AlgorithmEdgeLaplacian laplacianAlgo;

    /** DOCUMENT ME! */
    private AlgorithmEdgeLaplacianSep laplacianSepAlgo;

    /** DOCUMENT ME! */
    private JPanelAlgorithmOutputOptions outputPanel;

    /** DOCUMENT ME! */
    private ModelImage resultImage = null; // result image

    /** DOCUMENT ME! */
    private boolean separable = true;

    /** DOCUMENT ME! */
    private JCheckBox sepCheckBox;

    /** DOCUMENT ME! */
    private JPanelSigmas sigmaPanel;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Empty constructor needed for dynamic instantiation (used during scripting).
     */
    public JDialogEdgeLaplacian() { }

    /**
     * Creates new dialog and displays it.
     *
     * @param  theParentFrame  Parent frame.
     * @param  im              Source image.
     */
    public JDialogEdgeLaplacian(Frame theParentFrame, ModelImage im) {
        super(theParentFrame, false);
        image = im;
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

        if (command.equals("OK")) {

            if (setVariables()) {
                callAlgorithm();
            }
        } else if (command.equals("Cancel")) {
            dispose();
        } else if (command.equals("Help")) {
        	//MipavUtil.showHelp("Edge020");
        	MipavUtil.showWebHelp("Edge_Detection:_Zero_X_Laplacian");
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

        if (algorithm instanceof AlgorithmEdgeLaplacianSep) {
            image.clearMask();

            if ((laplacianSepAlgo.isCompleted() == true) && (resultImage != null)) {

                // resultImage.clearMask();
                // The algorithm has completed and produced a new image to be displayed.
                try {

                    edgeImage = laplacianSepAlgo.getZeroXMask();
                    updateFileInfo(image, edgeImage);
                    resultImage.disposeLocal();
                    resultImage = null;
                    new ViewJFrameImage(edgeImage);

                } catch (OutOfMemoryError error) {
                    MipavUtil.displayError("Out of memory: unable to open new frame");
                }
            } // if (laplacianSepAlgo.isCompleted() == true && resultImage != null)
            else if (resultImage != null) {

                // algorithm failed but result image still has garbage
                resultImage.disposeLocal(); // clean up memory
                resultImage = null;
            }
        } else if (algorithm instanceof AlgorithmEdgeLaplacian) {
            image.clearMask();

            if ((laplacianAlgo.isCompleted() == true) && (resultImage != null)) {

                // The algorithm has completed and produced a new image to be displayed.
                try {
                    // resultImage.setImageName("EdgeLap");
                    // imageFrame = new ViewJFrameImage(resultImage, null, new Dimension(610,200), userInterface);

                    edgeImage = laplacianAlgo.getZeroXMask();
                    updateFileInfo(image,edgeImage);
                    resultImage.disposeLocal();
                    new ViewJFrameImage(edgeImage);

                } catch (OutOfMemoryError error) {
                    MipavUtil.displayError("Out of memory: unable to open new frame");
                }
            } // if (laplacianAlgo.isCompleted() == true && resultImage != null)
            else if (resultImage != null) {

                // algorithm failed but result image still has garbage
                resultImage.disposeLocal(); // clean up memory
                resultImage = null;
            }
        }

        if (algorithm.isCompleted()) {
            insertScriptLine();
        }
        
        if (algorithm instanceof AlgorithmEdgeLaplacianSep) {
            // save the completion status for later
            setComplete(laplacianSepAlgo.isCompleted());
        }
        
        if (algorithm instanceof AlgorithmEdgeLaplacian) {
            // save the completion status for later
            setComplete(laplacianAlgo.isCompleted());
        }

        if (laplacianSepAlgo != null) {
            laplacianSepAlgo.finalize();
            laplacianSepAlgo = null;
        }

        if (laplacianAlgo != null) {
            laplacianAlgo.finalize();
            laplacianAlgo = null;
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
     * Method to handle item events.
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
     * Accessor that sets the slicing flag.
     *
     * @param  flag  <code>true</code> indicates slices should be blurred independently.
     */
    public void setImage25D(boolean flag) {
        image25D = flag;
    }

    /**
     * Accessor that sets whether or not the separable convolution kernel is used.
     *
     * @param  separable  DOCUMENT ME!
     */
    public void setSeparable(boolean separable) {
        this.separable = separable;
    }

    /**
     * Once all the necessary variables are set, call the algorithm based on what type of image this is and whether or
     * not there is a separate destination image.
     */
    protected void callAlgorithm() {
        System.gc();

        String name = makeImageName(image.getImageName(), "_edgeLap");

        if ((image.getNDims() == 2) && separable) { // source image is 2D and separable convolution

            int[] destExtents = new int[2];

            destExtents[0] = image.getExtents()[0]; // X dim
            destExtents[1] = image.getExtents()[1]; // Y dim

            float[] sigmas = sigmaPanel.getNormalizedSigmas();

            try {

                // Make result image of float type
                // Do not clone because you must have the ability to generate negative
                // values and this will not happen if your image is UBYTE or USHORT
                resultImage = new ModelImage(ModelImage.FLOAT, destExtents, name);

                if ((resultImage.getFileInfo()[0]).getFileFormat() == FileUtility.DICOM) {
                    ((FileInfoDicom) (resultImage.getFileInfo(0))).setSecondaryCaptureTags();
                }

                // Make algorithm
                laplacianSepAlgo = new AlgorithmEdgeLaplacianSep(resultImage, image, sigmas,
                                                                 outputPanel.isProcessWholeImageSet(), image25D);

                // This is very important. Adding this object as a listener allows the algorithm to
                // notify this object when it has completed of failed. See algorithm performed event.
                // This is made possible by implementing AlgorithmedPerformed interface
                laplacianSepAlgo.addListener(this);

                createProgressBar(image.getImageName(), "Calculating the Edge ...", laplacianSepAlgo);

                // Hide dialog
                setVisible(false);

                if (isRunInSeparateThread()) {

                    // Start the thread as a low priority because we wish to still have user interface work fast.
                    if (laplacianSepAlgo.startMethod(Thread.MIN_PRIORITY) == false) {
                        MipavUtil.displayError("A thread is already running on this object");
                    }
                } else {
                    laplacianSepAlgo.run();
                }
            } catch (OutOfMemoryError x) {
                MipavUtil.displayError("Dialog laplacian: unable to allocate enough memory");

                if (resultImage != null) {
                    resultImage.disposeLocal(); // Clean up memory of result image
                    resultImage = null;
                }

                return;
            }
        } else if ((image.getNDims() == 3) && separable) { // separable convolution kernels

            int[] destExtents = new int[3];

            destExtents[0] = image.getExtents()[0];
            destExtents[1] = image.getExtents()[1];
            destExtents[2] = image.getExtents()[2];

            float[] sigmas = sigmaPanel.getNormalizedSigmas();

            try {

                // Make result image of float type
                // Do not clone because you must have the ability to generate negative
                // values and this will not happen if your image is UBYTE or USHORT
                resultImage = new ModelImage(ModelImage.FLOAT, destExtents, name);

                if ((resultImage.getFileInfo()[0]).getFileFormat() == FileUtility.DICOM) {

                    for (int i = 0; i < resultImage.getExtents()[2]; i++) {
                        ((FileInfoDicom) (resultImage.getFileInfo(i))).setSecondaryCaptureTags();
                    }
                }

                // Make algorithm
                laplacianSepAlgo = new AlgorithmEdgeLaplacianSep(resultImage, image, sigmas,
                                                                 outputPanel.isProcessWholeImageSet(), image25D);

                // This is very important. Adding this object as a listener allows the algorithm to
                // notify this object when it has completed of failed. See algorithm performed event.
                // This is made possible by implementing AlgorithmedPerformed interface
                laplacianSepAlgo.addListener(this);

                createProgressBar(image.getImageName(), "Calculating the Edge ...", laplacianSepAlgo);

                // Hide dialog
                setVisible(false);

                if (isRunInSeparateThread()) {

                    // Start the thread as a low priority because we wish to still have user interface work fast
                    if (laplacianSepAlgo.startMethod(Thread.MIN_PRIORITY) == false) {
                        MipavUtil.displayError("A thread is already running on this object");
                    }
                } else {
                    laplacianSepAlgo.run();
                }
            } catch (OutOfMemoryError x) {
                MipavUtil.displayError("Dialog laplacian: unable to allocate enough memory");

                if (resultImage != null) {
                    resultImage.disposeLocal(); // Clean up image memory
                    resultImage = null;
                }

                return;
            }
        } else if (image.getNDims() == 2) { // source image is 2D and convolution kernel not separable

            int[] destExtents = new int[2];

            destExtents[0] = image.getExtents()[0]; // X dim
            destExtents[1] = image.getExtents()[1]; // Y dim

            float[] sigmas = sigmaPanel.getNormalizedSigmas();

            try {

                // Make result image of float type
                // Do not clone because you must have the ability to generate negative
                // values and this will not happen if your image is UBYTE or USHORT
                resultImage = new ModelImage(ModelImage.FLOAT, destExtents, name);

                if ((resultImage.getFileInfo()[0]).getFileFormat() == FileUtility.DICOM) {
                    ((FileInfoDicom) (resultImage.getFileInfo(0))).setSecondaryCaptureTags();
                }

                // Make algorithm
                laplacianAlgo = new AlgorithmEdgeLaplacian(resultImage, image, sigmas,
                                                           outputPanel.isProcessWholeImageSet(), image25D);

                // This is very important. Adding this object as a listener allows the algorithm to
                // notify this object when it has completed of failed. See algorithm performed event.
                // This is made possible by implementing AlgorithmedPerformed interface
                laplacianAlgo.addListener(this);

                createProgressBar(image.getImageName(), "Calculating the Edge ...", laplacianAlgo);

                // Hide dialog
                setVisible(false);

                if (isRunInSeparateThread()) {

                    // Start the thread as a low priority because we wish to still have user interface work fast.
                    if (laplacianAlgo.startMethod(Thread.MIN_PRIORITY) == false) {
                        MipavUtil.displayError("A thread is already running on this object");
                    }
                } else {
                    laplacianAlgo.run();
                }
            } catch (OutOfMemoryError x) {
                MipavUtil.displayError("Dialog laplacian: unable to allocate enough memory");

                if (resultImage != null) {
                    resultImage.disposeLocal(); // Clean up memory of result image
                    resultImage = null;
                }

                return;
            }
        } else if (image.getNDims() == 3) { // convolution kernel is not separable

            int[] destExtents = new int[3];

            destExtents[0] = image.getExtents()[0];
            destExtents[1] = image.getExtents()[1];
            destExtents[2] = image.getExtents()[2];

            float[] sigmas = sigmaPanel.getNormalizedSigmas();

            try {

                // Make result image of float type
                // Do not clone because you must have the ability to generate negative
                // values and this will not happen if your image is UBYTE or USHORT
                resultImage = new ModelImage(ModelImage.FLOAT, destExtents, name);

                if ((resultImage.getFileInfo()[0]).getFileFormat() == FileUtility.DICOM) {

                    for (int i = 0; i < resultImage.getExtents()[2]; i++) {
                        ((FileInfoDicom) (resultImage.getFileInfo(i))).setSecondaryCaptureTags();
                    }
                }

                // Make algorithm
                laplacianAlgo = new AlgorithmEdgeLaplacian(resultImage, image, sigmas,
                                                           outputPanel.isProcessWholeImageSet(), image25D);

                // This is very important. Adding this object as a listener allows the algorithm to
                // notify this object when it has completed of failed. See algorithm performed event.
                // This is made possible by implementing AlgorithmedPerformed interface
                laplacianAlgo.addListener(this);

                createProgressBar(image.getImageName(), "Calculating the Edge ...", laplacianAlgo);

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
            } catch (OutOfMemoryError x) {
                MipavUtil.displayError("Dialog laplacian: unable to allocate enough memory");

                if (resultImage != null) {
                    resultImage.disposeLocal(); // Clean up image memory
                    resultImage = null;
                }

                return;
            }
        }
    }

    /**
     * Store the result image in the script runner's image table now that the action execution is finished.
     */
    protected void doPostAlgorithmActions() {
        AlgorithmParameters.storeImageInRunner(edgeImage);
    }

    /**
     * {@inheritDoc}
     */
    protected void setGUIFromParams() {
        image = scriptParameters.retrieveInputImage();

        parentFrame = image.getParentFrame();
        sigmaPanel = new JPanelSigmas(image);
        scriptParameters.setSigmasGUI(sigmaPanel);

        outputPanel = new JPanelAlgorithmOutputOptions(image);
        scriptParameters.setOutputOptionsGUI(outputPanel);

        image25D = scriptParameters.doProcess3DAs25D();
        separable = scriptParameters.doProcessSeparable();
    }

    /**
     * {@inheritDoc}
     */
    protected void storeParamsFromGUI() throws ParserException {
        scriptParameters.storeInputImage(image);
        scriptParameters.storeOutputImageParams(edgeImage, outputPanel.isOutputNewImageSet());

        scriptParameters.storeProcessWholeImage(outputPanel.isProcessWholeImageSet());

        scriptParameters.storeSigmas(sigmaPanel);

        scriptParameters.storeProcess3DAs25D(image25D);
        scriptParameters.storeProcessSeparable(separable);
    }

    /**
     * Sets up the GUI (panels, buttons, etc) and displays it on the screen.
     */
    private void init() {
        setForeground(Color.black);

        getContentPane().setLayout(new BorderLayout());
        setTitle("EdgeLap");

        sigmaPanel = new JPanelSigmas(image);

        sepCheckBox = WidgetFactory.buildCheckBox("Use separable convolution kernels", true);

        image25DCheckbox = WidgetFactory.buildCheckBox("Process each slice independently (2.5D)", false, this);

        if (image.getNDims() != 3) { // if the source image is 3D then allow
            image25DCheckbox.setEnabled(false);
        }

        PanelManager optionsPanelManager = new PanelManager("Options");
        optionsPanelManager.add(sepCheckBox);
        optionsPanelManager.addOnNextLine(image25DCheckbox);

        outputPanel = new JPanelAlgorithmOutputOptions(image);
        outputPanel.setOutputImageOptionsEnabled(false);
        outputPanel.setOutputNewImage(true);

        PanelManager mainPanelManager = new PanelManager();
        mainPanelManager.add(sigmaPanel);
        mainPanelManager.addOnNextLine(optionsPanelManager.getPanel());
        mainPanelManager.addOnNextLine(outputPanel);

        JPanel buttonPanel = new JPanel();
        buildOKButton();
        buttonPanel.add(OKButton);
        buildCancelButton();
        buttonPanel.add(cancelButton);
        buildHelpButton();
        buttonPanel.add(helpButton);

        getContentPane().add(mainPanelManager.getPanel(), BorderLayout.CENTER);
        getContentPane().add(buttonPanel, BorderLayout.SOUTH);
        pack();
        //setResizable(false);
        setVisible(true);

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
        }

        if (!sigmaPanel.testSigmaValues()) {
            return false;
        }

        separable = sepCheckBox.isSelected();

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
                return new String("Algorithms.Edge detection");
            }

            public String getDescription() {
                return new String("Applies a zero X laplacian algorithm.");
            }

            public String getDescriptionLong() {
                return new String("Applies a zero X laplacian algorithm.");
            }

            public String getShortLabel() {
                return new String("ZeroXLaplacian");
            }

            public String getLabel() {
                return new String("Zero X Laplacian");
            }

            public String getName() {
                return new String("Zero X Laplacian");
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
            table.put(new ParameterBoolean(AlgorithmParameters.DO_PROCESS_SEPARABLE, true));
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
