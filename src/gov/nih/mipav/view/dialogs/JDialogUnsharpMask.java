package gov.nih.mipav.view.dialogs;


import gov.nih.mipav.model.algorithms.*;
import gov.nih.mipav.model.algorithms.filters.*;
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
 * new image or replace the source image. In addition the user can indicate if you wishes to have the algorithm applied
 * to whole image or to the VOI regions. In should be noted that the algorithms are executed in their own thread.
 *
 * @version  0.1 Nov 17, 1998
 * @author   Matthew J. McAuliffe, Ph.D.
 * @see      AlgorithmUnsharpMask
 */
public class JDialogUnsharpMask extends JDialogScriptableBase implements AlgorithmInterface, ActionDiscovery, ScriptableActionInterface {

    //~ Static fields/initializers -------------------------------------------------------------------------------------

    /** Use serialVersionUID for interoperability. */
    private static final long serialVersionUID = -3776616289889677111L;

    /** DOCUMENT ME! */
    private static final String DELIMITER = ",";

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** DOCUMENT ME! */
    private ModelImage image; // source image

    /** DOCUMENT ME! */
    private boolean image25D = false;

    /** DOCUMENT ME! */
    private JCheckBox image25DCheckbox;
    
    private JCheckBox cubicCheckbox;

    /** DOCUMENT ME! */
    private JPanelAlgorithmOutputOptions outputPanel;

    /** DOCUMENT ME! */
    private ModelImage resultImage = null; // result image

    /** DOCUMENT ME! */
    private JPanelSigmas sigmaPanel;

    /** DOCUMENT ME! */
    private JTextField textWeight;

    /** DOCUMENT ME! */
    private String[] titles;

    /** DOCUMENT ME! */
    private AlgorithmUnsharpMask unsharpMaskAlgo;

    /** DOCUMENT ME! */
    private ViewUserInterface userInterface;

 // Ramponi paper uses weight = 6.0E-4 and 1.5E-3 for cubic algorithm
    private double weight;
    
    // If true, use nonlinear cubic filter
    private boolean cubic = false;
    
    // Absolute value of maximum correction allowed in cubic algorithm
    private double maximumCorrection = 50.0;
    
    private JLabel correctionLabel;
    
    private JTextField correctionText;
    
    private JPanel paramPanel;
    
    private JPanel weightPanel;
    
    private JLabel labelWeight;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Empty constructor needed for dynamic instantiation (used during scripting).
     */
    public JDialogUnsharpMask() { }

    /**
     * Constructor.
     *
     * @param  theParentFrame  parent frame
     * @param  im              source image
     */
    public JDialogUnsharpMask(Frame theParentFrame, ModelImage im) {
        super(theParentFrame, false);
        image = im;
        userInterface = ViewUserInterface.getReference();
        init();
    }

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * actionPerformed - Closes dialog box when the OK button is pressed and calls the algorithm.
     *
     * @param  event  event that triggers function
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
            //MipavUtil.showHelp("10025");
            MipavUtil.showWebHelp("Filters_(Spatial):_Unsharp_Mask");
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

        if (algorithm instanceof AlgorithmUnsharpMask) {
            image.clearMask();

            if ((unsharpMaskAlgo.isCompleted() == true) && (resultImage != null)) {
                // The algorithm has completed and produced a new image to be displayed.

                updateFileInfo(image, resultImage);
                resultImage.clearMask();

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

            if (algorithm.isCompleted()) {
                insertScriptLine();
            }
        }

     // save the completion status for later
        setComplete(algorithm.isCompleted());

        unsharpMaskAlgo.finalize();
        unsharpMaskAlgo = null;
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

    // *******************************************************************
    // ************************* Item Events ****************************
    // *******************************************************************

    /**
     * itemStateChanged - method to handle item events.
     *
     * @param  event  event that cause the method to fire
     */
    public void itemStateChanged(ItemEvent event) {
        Object source = event.getSource();
        
        if (source == cubicCheckbox) {
            if (cubicCheckbox.isSelected()) {
            	if (image.getNDims() > 2) {
            	    image25DCheckbox.removeItemListener(this);
            	    image25DCheckbox.setSelected(true);
            	    image25DCheckbox.addItemListener(this);
            	}
            	sigmaPanel.setEnabled(false);
            	sigmaPanel.enable3DComponents(false);
            	sigmaPanel.enableLabelGaussX(false);
            	sigmaPanel.enableLabelGaussY(false);
            	sigmaPanel.enableTextGaussX(false);
            	sigmaPanel.enableTextGaussY(false);
            	correctionLabel.setEnabled(true);
            	correctionText.setEnabled(true);
            	weightPanel.setBorder(buildTitledBorder("Weight of nonlinear cubic correction"));
            	labelWeight.setText("Image + (k * cubic correction)");
            	textWeight.setText("1.0E-2");
            }
            else {
            	correctionLabel.setEnabled(false);
            	correctionText.setEnabled(false);
            	weightPanel.setBorder(buildTitledBorder("Weight of blurred image"));
            	labelWeight.setText("Image - (k * blurred image) where 0 < k < 1");
            	textWeight.setText("0.75");
            	sigmaPanel.setEnabled(true);
            	if (image.getNDims() > 2) {
            	    sigmaPanel.enable3DComponents(!image25DCheckbox.isSelected());
            	}
            	sigmaPanel.enableLabelGaussX(true);
            	sigmaPanel.enableLabelGaussY(true);
            	sigmaPanel.enableTextGaussX(true);
            	sigmaPanel.enableTextGaussY(true);
            }
        }
        else if (source == image25DCheckbox) {
        	sigmaPanel.setEnabled(true);
            sigmaPanel.enable3DComponents(!image25DCheckbox.isSelected());
            sigmaPanel.enableLabelGaussX(true);
        	sigmaPanel.enableLabelGaussY(true);
            sigmaPanel.enableTextGaussX(true);
        	sigmaPanel.enableTextGaussY(true);
            if (image25DCheckbox.isSelected()) {
            	cubicCheckbox.removeItemListener(this);
            	cubicCheckbox.setSelected(false);
            	cubicCheckbox.addItemListener(this);
            	correctionLabel.setEnabled(false);
            	correctionText.setEnabled(false);
            	weightPanel.setBorder(buildTitledBorder("Weight of blurred image"));
            	labelWeight.setText("Image - (k * blurred image) where 0 < k < 1");
            	textWeight.setText("0.75");
            	sigmaPanel.setEnabled(true);
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
     * Accessor that sets the weight.
     *
     * @param  val  Value to set weight to.
     */
    public void setWeight(double val) {
        weight = val;
    }

    /**
     * Once all the necessary variables are set, call the UnsharpMark algorithm based on what type of image this is and
     * whether or not there is a separate destination image.
     */
    protected void callAlgorithm() {
        String name = makeImageName(image.getImageName(), "_unsharp");

        if (image.getNDims() == 2) { // source image is 2D

            int[] destExtents = new int[2];
            destExtents[0] = image.getExtents()[0]; // X dim
            destExtents[1] = image.getExtents()[1]; // Y dim

            float[] sigmas = sigmaPanel.getNormalizedSigmas();

            if (outputPanel.isOutputNewImageSet()) {

                try {

                    // Make result image of float type
                    // Will not work for UBYTE and USHORT
                    resultImage = new ModelImage(ModelImage.DOUBLE, destExtents, name);

                    if ((resultImage.getFileInfo()[0]).getFileFormat() == FileUtility.DICOM) {
                        ((FileInfoDicom) (resultImage.getFileInfo(0))).setSecondaryCaptureTags(false,
                        		resultImage.getFileInfo()[0].getDataType());
                    }

                    // Make algorithm
                    unsharpMaskAlgo = new AlgorithmUnsharpMask(resultImage, image, sigmas, weight,
                                                               outputPanel.isProcessWholeImageSet(), false,
                                                               cubic, maximumCorrection);

                    // This is very important. Adding this object as a listener allows the algorithm to
                    // notify this object when it has completed of failed. See algorithm performed event.
                    // This is made possible by implementing AlgorithmedPerformed interface
                    unsharpMaskAlgo.addListener(this);

                    System.err.println("creating pBar");
                    createProgressBar(image.getImageName(), unsharpMaskAlgo);

                    // Hide dialog
                    setVisible(false);

                    if (isRunInSeparateThread()) {

                        // Start the thread as a low priority because we wish to still have user interface work fast.
                        if (unsharpMaskAlgo.startMethod(Thread.MIN_PRIORITY) == false) {
                            MipavUtil.displayError("A thread is already running on this object");
                        }
                    } else {
                        unsharpMaskAlgo.run();
                    }
                } catch (OutOfMemoryError x) {
                    MipavUtil.displayError("Dialog Unsharp Mask: unable to allocate enough memory");

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
                    unsharpMaskAlgo = new AlgorithmUnsharpMask(image, sigmas, weight,
                                                               outputPanel.isProcessWholeImageSet(), false,
                                                               cubic, maximumCorrection);

                    // This is very important. Adding this object as a listener allows the algorithm to
                    // notify this object when it has completed of failed. See algorithm performed event.
                    // This is made possible by implementing AlgorithmedPerformed interface
                    unsharpMaskAlgo.addListener(this);

                    createProgressBar(image.getImageName(), unsharpMaskAlgo);

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
                        if (unsharpMaskAlgo.startMethod(Thread.MIN_PRIORITY) == false) {
                            MipavUtil.displayError("A thread is already running on this object");
                        }
                    } else {
                        unsharpMaskAlgo.run();
                    }
                } catch (OutOfMemoryError x) {
                    MipavUtil.displayError("Dialog Unsharp Mask: unable to allocate enough memory");

                    return;
                }
            }
        } else if (image.getNDims() == 3) {
            int[] destExtents = new int[3];
            destExtents[0] = image.getExtents()[0];
            destExtents[1] = image.getExtents()[1];
            destExtents[2] = image.getExtents()[2];

            float[] sigmas = sigmaPanel.getNormalizedSigmas();

            if (outputPanel.isOutputNewImageSet()) {

                try {

                    // Make result image of float type
                    // Will not work for UBYTE and USHORT
                    resultImage = new ModelImage(ModelImage.DOUBLE, destExtents, name);

                    if ((resultImage.getFileInfo()[0]).getFileFormat() == FileUtility.DICOM) {

                        for (int i = 0; i < resultImage.getExtents()[2]; i++) {
                            ((FileInfoDicom) (resultImage.getFileInfo(i))).setSecondaryCaptureTags(true,
                            		resultImage.getFileInfo()[0].getDataType());
                        }
                    }

                    // Make algorithm
                    unsharpMaskAlgo = new AlgorithmUnsharpMask(resultImage, image, sigmas, weight,
                                                               outputPanel.isProcessWholeImageSet(), image25D,
                                                               cubic, maximumCorrection);

                    // This is very important. Adding this object as a listener allows the algorithm to
                    // notify this object when it has completed of failed. See algorithm performed event.
                    // This is made possible by implementing AlgorithmedPerformed interface
                    unsharpMaskAlgo.addListener(this);

                    createProgressBar(image.getImageName(), unsharpMaskAlgo);

                    // Hide dialog
                    setVisible(false);

                    if (isRunInSeparateThread()) {

                        // Start the thread as a low priority because we wish to still have user interface work fast
                        if (unsharpMaskAlgo.startMethod(Thread.MIN_PRIORITY) == false) {
                            MipavUtil.displayError("A thread is already running on this object");
                        }
                    } else {
                        unsharpMaskAlgo.run();
                    }
                } catch (OutOfMemoryError x) {
                    MipavUtil.displayError("Dialog Unsharp Mask: unable to allocate enough memory");

                    if (resultImage != null) {
                        resultImage.disposeLocal(); // Clean up image memory
                        resultImage = null;
                    }

                    return;
                }
            } else {

                try {

                    // Make algorithm
                    unsharpMaskAlgo = new AlgorithmUnsharpMask(image, sigmas, weight,
                                                               outputPanel.isProcessWholeImageSet(), image25D,
                                                               cubic, maximumCorrection);

                    // This is very important. Adding this object as a listener allows the algorithm to
                    // notify this object when it has completed of failed. See algorithm performed event.
                    // This is made possible by implementing AlgorithmedPerformed interface
                    unsharpMaskAlgo.addListener(this);

                    createProgressBar(image.getImageName(), unsharpMaskAlgo);

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

                    // Start the thread as a low priority because we wish to still have user interface work fast
                    if (isRunInSeparateThread()) {

                        if (unsharpMaskAlgo.startMethod(Thread.MIN_PRIORITY) == false) {
                            MipavUtil.displayError("A thread is already running on this object");
                        }
                    } else {
                        unsharpMaskAlgo.run();
                    }
                } catch (OutOfMemoryError x) {
                    MipavUtil.displayError("Dialog Unsharp Mask: unable to allocate enough memory");

                    return;
                }
            }
        }
    }

    /**
     * Register the result image in the script runner.
     */
    protected void doPostAlgorithmActions() {

        if (outputPanel.isOutputNewImageSet()) {
            AlgorithmParameters.storeImageInRunner(resultImage);
        }
    }

    /**
     * {@inheritDoc}
     */
    protected void setGUIFromParams() {
        image = scriptParameters.retrieveInputImage();
        userInterface = ViewUserInterface.getReference();
        parentFrame = image.getParentFrame();

        sigmaPanel = new JPanelSigmas(image);
        scriptParameters.setSigmasGUI(sigmaPanel);

        outputPanel = new JPanelAlgorithmOutputOptions(image);
        scriptParameters.setOutputOptionsGUI(outputPanel);

        weight = scriptParameters.getParams().getDouble("blurred_image_weight");
        image25D = scriptParameters.doProcess3DAs25D();
        cubic = scriptParameters.getParams().getBoolean("cub");
        maximumCorrection = scriptParameters.getParams().getDouble("maximum_correction");
    }

    /**
     * {@inheritDoc}
     */
    protected void storeParamsFromGUI() throws ParserException {
        scriptParameters.storeInputImage(image);
        scriptParameters.storeOutputImageParams(resultImage, outputPanel.isOutputNewImageSet());

        scriptParameters.storeProcessingOptions(outputPanel.isProcessWholeImageSet(), image25D);

        scriptParameters.storeSigmas(sigmaPanel);

        scriptParameters.getParams().put(ParameterFactory.newParameter("blurred_image_weight", weight));
        scriptParameters.getParams().put(ParameterFactory.newParameter("cub", cubic));
        scriptParameters.getParams().put(ParameterFactory.newParameter("maximum_correction", maximumCorrection));
    }

    /**
     * Sets up the GUI (panels, buttons, etc) and displays it on the screen.
     */
    private void init() {
        setForeground(Color.black);

        getContentPane().setLayout(new BorderLayout());
        setTitle("Unsharp Mask");

        PanelManager mainPanelManager = new PanelManager();
        mainPanelManager.getPanel().setBorder(BorderFactory.createEmptyBorder(3, 3, 3, 3));

        sigmaPanel = new JPanelSigmas(image);
        mainPanelManager.add(sigmaPanel);

        GridBagConstraints gbc = new GridBagConstraints();
        gbc.gridwidth = 1;
        gbc.gridheight = 1;
        gbc.anchor = GridBagConstraints.WEST;
        gbc.weightx = 1;
        gbc.insets = new Insets(3, 3, 3, 3);
        gbc.fill = GridBagConstraints.HORIZONTAL;
        gbc.gridx = 0;
        gbc.gridy = 0;
        
        paramPanel = new JPanel(new GridBagLayout());
        paramPanel.setForeground(Color.black);
        paramPanel.setBorder(buildTitledBorder("Options"));
     
        cubicCheckbox = new JCheckBox("Use nonlinear cubic algorithm");
        cubicCheckbox.setFont(serif12);
        paramPanel.add(cubicCheckbox, gbc);
        cubicCheckbox.setSelected(false);
        cubicCheckbox.addItemListener(this);
        
        correctionLabel = new JLabel("Maximum cubic correction");
        correctionLabel.setFont(serif12);
        gbc.gridy++;
        paramPanel.add(correctionLabel, gbc);
        correctionLabel.setEnabled(false);
        
        correctionText = new JTextField();
        correctionText.setText("50.0");
        correctionText.setFont(serif12);
        gbc.gridx = 1;
        paramPanel.add(correctionText, gbc);
        correctionText.setEnabled(false);
        
        image25DCheckbox = new JCheckBox("Process each slice independently (2.5D).");
        image25DCheckbox.setFont(serif12);
        gbc.gridx = 0;
        gbc.gridy++;
        paramPanel.add(image25DCheckbox, gbc);
        image25DCheckbox.setSelected(false);
        image25DCheckbox.addItemListener(this);

        if (image.getNDims() != 3) {
            image25DCheckbox.setEnabled(false);
        }

        mainPanelManager.addOnNextLine(paramPanel);

        weightPanel = new JPanel(new GridBagLayout());
        weightPanel.setForeground(Color.black);
        weightPanel.setBorder(buildTitledBorder("Weight of blurred image"));
        gbc.gridx = 0;
        gbc.gridy = 0;

        labelWeight = new JLabel("Image - (k * blurred image) where 0 < k < 1");
        labelWeight.setFont(serif12);
        weightPanel.add(labelWeight, gbc);
        
        textWeight = new JTextField();
        textWeight.setText("0.75");
        textWeight.setFont(serif12);
        gbc.gridx = 1;
        weightPanel.add(textWeight, gbc);

        mainPanelManager.addOnNextLine(weightPanel);

        outputPanel = new JPanelAlgorithmOutputOptions(image);
        mainPanelManager.addOnNextLine(outputPanel);

        getContentPane().add(mainPanelManager.getPanel(), BorderLayout.CENTER);
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
        String tmpStr;
        System.gc();
        
        cubic = cubicCheckbox.isSelected();
        
        if (cubic) {
        	image25D = false;
        	tmpStr = correctionText.getText();
        	if (testParameter(tmpStr, 0, Double.MAX_VALUE)) {
        		maximumCorrection = Double.valueOf(tmpStr).doubleValue();
        	}
        	else {
        		correctionText.requestFocus();
        		correctionText.selectAll();
        		
        		return false;
        	}
        }
        else if (image25DCheckbox.isSelected()) {
            image25D = true;
        }

        if (!sigmaPanel.testSigmaValues()) {
            return false;
        }

        tmpStr = textWeight.getText();

        if (testParameter(tmpStr, 0, 0.999)) {
            weight = Double.valueOf(tmpStr).doubleValue();
        } else {
            textWeight.requestFocus();
            textWeight.selectAll();

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
                return new String("Applies a unsharp mask.");
            }

            public String getDescriptionLong() {
                return new String("Applies a unsharp mask.");
            }

            public String getShortLabel() {
                return new String("UnsharpMask");
            }

            public String getLabel() {
                return new String("Unsharp Mask");
            }

            public String getName() {
                return new String("Unsharp Mask");
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
            table.put(new ParameterDouble("blurred_image_weight", .75));
            table.put(new ParameterBoolean("cub", true));
            table.put(new ParameterDouble("maximum_correction", 50.0));
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
