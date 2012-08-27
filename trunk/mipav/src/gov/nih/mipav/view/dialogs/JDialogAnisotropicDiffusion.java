package gov.nih.mipav.view.dialogs;


import gov.nih.mipav.model.algorithms.*;
import gov.nih.mipav.model.algorithms.filters.*;
import gov.nih.mipav.model.scripting.ParserException;
import gov.nih.mipav.model.scripting.ScriptableActionInterface;
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
 * to whole image or to the VOI regions. In should be noted, that the algorithms are executed in their own thread.
 *
 * @version  0.1 Nov 17, 1998
 * @author   Matthew J. McAuliffe, Ph.D.
 * @see      AlgorithmGaussianBlur
 */
public class JDialogAnisotropicDiffusion extends JDialogScriptableBase
        implements AlgorithmInterface, ActionDiscovery, ScriptableActionInterface {

    //~ Static fields/initializers -------------------------------------------------------------------------------------

    /** Use serialVersionUID for interoperability. */
    private static final long serialVersionUID = -7624088316240843823L;

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** DOCUMENT ME! */
    private AlgorithmAnisotropicDiffusion diffusionAlgo;

    /** Source image. */
    private ModelImage image;

    /** DOCUMENT ME! */
    private boolean image25D = false;

    /** DOCUMENT ME! */
    private JCheckBox image25DCheckbox;

    /** DOCUMENT ME! */
    private int iters;

    /** DOCUMENT ME! */
    private float konst;
    
    /** Result image. */
    private ModelImage resultImage = null;

    /** DOCUMENT ME! */
    private JTextField textIters;

    /** DOCUMENT ME! */
    private JTextField textK;

    /** DOCUMENT ME! */
    private String[] titles;

    /** DOCUMENT ME! */
    private ViewUserInterface userInterface;
    
    private JPanelSigmas sigmaPanel;
    
    private JPanelAlgorithmOutputOptions outputPanel;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Empty constructor needed for dynamic instantiation (used during scripting).
     */
    public JDialogAnisotropicDiffusion() { }

    /**
     * Creates a new JDialogAnisotropicDiffusion object.
     *
     * @param  theParentFrame  Parent frame.
     * @param  im              Source image.
     */
    public JDialogAnisotropicDiffusion(Frame theParentFrame, ModelImage im) {
        super(theParentFrame, false);
        image = im;
        userInterface = ViewUserInterface.getReference();
        init();
        loadDefaults();
        setVisible(true);
    }

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * Performs some error checking, then closes dialog box when the OK button is pressed and calls the algorithm.
     *
     * @param  event  Event that triggers method.
     */
    public void actionPerformed(ActionEvent event) {

        // Object source = event.getSource();
        String command = event.getActionCommand();

        if (command.equals("OK")) {

            if (setVariables()) {
                callAlgorithm();
            }
        } else if (command.equals("Cancel")) {
            dispose();
        } else if (command.equals("Help")) {
            //MipavUtil.showHelp("10007");
            MipavUtil.showWebHelp("Filters_(Spatial)_Anisotropic_Diffusion#Applying_the_Anisotropic_Diffusion_algorithm");
        } else {
            super.actionPerformed(event);
        }
    } 
     
    /**
     * Record the parameters just used to run this algorithm in a script.
     * 
     * @throws ParserException If there is a problem creating/recording the new parameters.
     */
    protected void storeParamsFromGUI() throws ParserException {
        scriptParameters.storeInputImage(image);
        scriptParameters.storeOutputImageParams(resultImage, outputPanel.isOutputNewImageSet());
        
        scriptParameters.storeProcessingOptions(outputPanel.isProcessWholeImageSet(), image25D);
        
        scriptParameters.storeSigmas(sigmaPanel);
        
        scriptParameters.storeNumIterations(iters);
        scriptParameters.getParams().put(ParameterFactory.newParameter("diffusion_k", konst));
    }

    /**
     * Set the dialog GUI using the script parameters while running this algorithm as part of a script.
     */
    protected void setGUIFromParams() {
        image = scriptParameters.retrieveInputImage();
        parentFrame = image.getParentFrame();
        userInterface = ViewUserInterface.getReference();
        
        outputPanel = new JPanelAlgorithmOutputOptions(image);
        scriptParameters.setOutputOptionsGUI(outputPanel);
        
        sigmaPanel = new JPanelSigmas(image);
        scriptParameters.setSigmasGUI(sigmaPanel);
        
        setImage25D(scriptParameters.getParams().getBoolean(AlgorithmParameters.DO_PROCESS_3D_AS_25D));
        setIters(scriptParameters.getNumIterations());
        setK(scriptParameters.getParams().getFloat("diffusion_k"));
    }

    /**
     * Used to perform actions after the execution of the algorithm is completed (e.g., put the result image in the
     * image table). Defaults to no action, override to actually have it do something.
     */
    protected void doPostAlgorithmActions() {
        if (outputPanel.isOutputNewImageSet()) {
            AlgorithmParameters.storeImageInRunner(getResultImage());
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
        if (Preferences.is(Preferences.PREF_SAVE_DEFAULTS) && (this.getOwner() != null) && !isScriptRunning()) {
            saveDefaults();
        }

        if (algorithm instanceof AlgorithmAnisotropicDiffusion) {
            image.clearMask();

            if ((diffusionAlgo.isCompleted() == true) && (resultImage != null)) {
            	setComplete(algorithm.isCompleted());
                // The algorithm has completed and produced a new image to be displayed.
                updateFileInfo(image, resultImage);
                resultImage.clearMask();

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
                    ((ViewJFrameBase) parentFrame).getUserInterface().registerFrame(parentFrame);
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

        diffusionAlgo.finalize();
        diffusionAlgo = null;
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
     * @param  event  Event that caused the method to fire.
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
     * Accessor that sets the number of iterations.
     *
     * @param  num  Value to set iterations to (should be between 1 and 10000).
     */
    public void setIters(int num) {
        iters = num;
    }

    /**
     * Accessor that sets the k value.
     *
     * @param  k  Value to set k value to (should be between 1 and 100).
     */
    public void setK(float k) {
        this.konst = k;
    }

    /**
     * Once all the necessary variables are set, call the Gaussian Blur algorithm based on what type of image this is
     * and whether or not there is a separate destination image.
     */
    protected void callAlgorithm() {
        String name = makeImageName(image.getImageName(), "_adiffusion");
        
        if (image.getNDims() == 2) { // source image is 2D

            int[] destExtents = new int[2];
            destExtents[0] = image.getExtents()[0]; // X dim
            destExtents[1] = image.getExtents()[1]; // Y dim

            float[] sigmas = sigmaPanel.getNormalizedSigmas();

            if (outputPanel.isOutputNewImageSet()) {

                try {

                    // Make result image of float type
                    resultImage = new ModelImage(ModelImage.FLOAT, destExtents, name);

                    // Make algorithm
                    diffusionAlgo = new AlgorithmAnisotropicDiffusion(resultImage, image, sigmas, iters, konst,
                                                                      outputPanel.isProcessWholeImageSet(), 
                                                                      false);

                    // This is very important. Adding this object as a listener allows the algorithm to
                    // notify this object when it has completed of failed. See algorithm performed event.
                    // This is made possible by implementing AlgorithmedPerformed interface
                    diffusionAlgo.addListener(this);

                    createProgressBar(image.getImageName(), diffusionAlgo);
                    
                    // Hide dialog
                    setVisible(false);

                    if (isRunInSeparateThread()) {

                        // Start the thread as a low priority because we wish to still have user interface work fast.
                        if (diffusionAlgo.startMethod(Thread.MIN_PRIORITY) == false) {
                            MipavUtil.displayError("A thread is already running on this object");
                        }
                    } else {
                        diffusionAlgo.run();
                    }
                } catch (OutOfMemoryError x) {
                    MipavUtil.displayError("Dialog diffusion: unable to allocate enough memory");

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
                    diffusionAlgo = new AlgorithmAnisotropicDiffusion(image, sigmas, iters, konst, 
                            outputPanel.isProcessWholeImageSet(), false);

                    // This is very important. Adding this object as a listener allows the algorithm to
                    // notify this object when it has completed of failed. See algorithm performed event.
                    // This is made possible by implementing AlgorithmedPerformed interface
                    diffusionAlgo.addListener(this);

                    createProgressBar(image.getImageName(), diffusionAlgo);
                    
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
                        if (diffusionAlgo.startMethod(Thread.MIN_PRIORITY) == false) {
                            MipavUtil.displayError("A thread is already running on this object");
                        }
                    } else {
                    
                        diffusionAlgo.run();
                    }
                } catch (OutOfMemoryError x) {
                    MipavUtil.displayError("Dialog Diffusion: unable to allocate enough memory");

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
                    resultImage = new ModelImage(ModelImage.FLOAT, destExtents, name);

                    // Make algorithm
                    diffusionAlgo = new AlgorithmAnisotropicDiffusion(resultImage, image, sigmas, iters, konst,
                                                                      outputPanel.isProcessWholeImageSet(), image25D);

                    // This is very important. Adding this object as a listener allows the algorithm to
                    // notify this object when it has completed of failed. See algorithm performed event.
                    // This is made possible by implementing AlgorithmedPerformed interface
                    diffusionAlgo.addListener(this);

                    createProgressBar(image.getImageName(), diffusionAlgo);
                    
                    // Hide dialog
                    setVisible(false);

                    if (isRunInSeparateThread()) {

                        // Start the thread as a low priority because we wish to still have user interface work fast.
                        if (diffusionAlgo.startMethod(Thread.MIN_PRIORITY) == false) {
                            MipavUtil.displayError("A thread is already running on this object");
                        }
                    } else {
                
                        diffusionAlgo.run();
                    }
                } catch (OutOfMemoryError x) {
                    MipavUtil.displayError("Dialog Diffusion: unable to allocate enough memory");

                    if (resultImage != null) {
                        resultImage.disposeLocal(); // Clean up image memory
                        resultImage = null;
                    }

                    return;
                }
            } else {

                try {

                    // Make algorithm
                    diffusionAlgo = new AlgorithmAnisotropicDiffusion(image, sigmas, iters, konst, outputPanel.isProcessWholeImageSet(),
                                                                      image25D);

                    // This is very important. Adding this object as a listener allows the algorithm to
                    // notify this object when it has completed of failed. See algorithm performed event.
                    // This is made possible by implementing AlgorithmedPerformed interface
                    diffusionAlgo.addListener(this);

                    createProgressBar(image.getImageName(), diffusionAlgo);
                    
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
                        if (diffusionAlgo.startMethod(Thread.MIN_PRIORITY) == false) {
                            MipavUtil.displayError("A thread is already running on this object");
                        }
                    } else {
                   
                        diffusionAlgo.run();
                    }
                } catch (OutOfMemoryError x) {
                    MipavUtil.displayError("Dialog diffusion: unable to allocate enough memory");

                    return;
                }
            }
        }

    }

    /**
     * Initializes dialog by setting up components and placing them in dialog frame.
     */
    private void init() {
        JPanel paramPanel;
        JLabel labelIters;
        JLabel labelK;
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
        gbc.gridx = 0;
        gbc.gridy = 0;
        gbc.fill = GridBagConstraints.HORIZONTAL;

        setTitle("Anisotropic Diffusion");

        sigmaPanel = new JPanelSigmas(image);
        mainPanel.add(sigmaPanel, gbc);
        
        gbc.gridx = 0;
        gbc.gridy = 1;

        JPanel optionPanel = new JPanel(new BorderLayout());

        image25DCheckbox = new JCheckBox("Process each slice independently (2.5D)");
        image25DCheckbox.setFont(serif12);
        optionPanel.add(image25DCheckbox, BorderLayout.SOUTH);
        image25DCheckbox.setSelected(false);
        image25DCheckbox.addItemListener(this);

        if (image.getNDims() != 3) {
            image25DCheckbox.setEnabled(false);
        }

        mainPanel.add(optionPanel, gbc);

        paramPanel = new JPanel(new GridLayout(2, 2));
        paramPanel.setForeground(Color.black);
        paramPanel.setBorder(buildTitledBorder("Algorithm parameters"));
        mainPanel.add(paramPanel);

        labelIters = createLabel("Iterations (1-10000)  ");
        paramPanel.add(labelIters);

        textIters = createTextField("10");
        paramPanel.add(textIters);

        labelK = createLabel("k ( k -> 1 slows diffusion )  ");
        paramPanel.add(labelK);

        textK = createTextField("15");
        paramPanel.add(textK);
        gbc.gridx = 0;
        gbc.gridy = 2;
        mainPanel.add(paramPanel, gbc);
        
        outputPanel = new JPanelAlgorithmOutputOptions(image);
        gbc.gridx = 0;
        gbc.gridy = 3;
        mainPanel.add(outputPanel, gbc);

        mainDialogPanel.add(mainPanel, BorderLayout.CENTER);
        mainDialogPanel.add(buildButtons(), BorderLayout.SOUTH);

        getContentPane().add(mainDialogPanel);

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

        System.gc();

        if (image25DCheckbox.isSelected()) {
            image25D = true;
        }

        if (!sigmaPanel.testSigmaValues()) {
            return false;
        }

        tmpStr = textIters.getText();

        if (testParameter(tmpStr, 1, 10000)) {
            iters = Integer.valueOf(tmpStr).intValue();
        } else {
            textIters.requestFocus();
            textIters.selectAll();

            return false;
        }

        tmpStr = textK.getText();

        if (testParameter(tmpStr, 1, 10000)) {
            konst = Float.valueOf(tmpStr).floatValue();
        } else {
            textK.requestFocus();
            textK.selectAll();

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
                return new String("Performs a Anisotropic Diffusion.");
            }

            public String getDescriptionLong() {
                return new String("Performs a Anisotropic Diffusion.");
            }

            public String getShortLabel() {
                return new String("AnisotropicDiffusion");
            }

            public String getLabel() {
                return new String("Anisotropic Diffusion");
            }

            public String getName() {
                return new String("Anisotropic Diffusion");
            }
        };
    }

	@Override
	public ParameterTable createInputParameters() {
        final ParameterTable table = new ParameterTable();
        
        try {
            table.put(new ParameterExternalImage(AlgorithmParameters.getInputImageLabel(1)));
            table.put(new ParameterList(AlgorithmParameters.SIGMAS, Parameter.PARAM_FLOAT, "1.0,1.0,1.0"));
            table.put(new ParameterInt(AlgorithmParameters.NUM_ITERATIONS, 10));
            table.put(new ParameterBoolean(AlgorithmParameters.DO_PROCESS_3D_AS_25D, true));
            table.put(new ParameterBoolean(AlgorithmParameters.SIGMA_DO_Z_RES_CORRECTION, true));
            table.put(new ParameterBoolean(AlgorithmParameters.DO_OUTPUT_NEW_IMAGE, true));
            table.put(new ParameterBoolean(AlgorithmParameters.DO_PROCESS_WHOLE_IMAGE, true));
            table.put(new ParameterFloat("diffusion_k", 15));
        } catch (final ParserException e) {
            // this shouldn't really happen since there isn't any real parsing going on...
            e.printStackTrace();
        }

        return table;

	}

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

	@Override
	public String getOutputImageName(String imageParamName) {
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

	@Override
	public boolean isActionComplete() {
        return isComplete();

	}
}
