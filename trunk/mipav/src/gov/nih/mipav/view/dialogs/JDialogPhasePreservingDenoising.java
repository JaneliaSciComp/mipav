package gov.nih.mipav.view.dialogs;


import gov.nih.mipav.model.algorithms.AlgorithmBase;
import gov.nih.mipav.model.algorithms.AlgorithmInterface;
import gov.nih.mipav.model.algorithms.filters.PhasePreservingDenoising;
import gov.nih.mipav.model.scripting.ParserException;
import gov.nih.mipav.model.scripting.ScriptableActionInterface;
import gov.nih.mipav.model.scripting.parameters.*;
import gov.nih.mipav.model.structures.ModelImage;
import gov.nih.mipav.model.structures.ModelStorageBase;
import gov.nih.mipav.view.MipavUtil;
import gov.nih.mipav.view.Preferences;
import gov.nih.mipav.view.ViewImageUpdateInterface;
import gov.nih.mipav.view.ViewJFrameImage;
import gov.nih.mipav.view.ViewUserInterface;

import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.Dimension;
import java.awt.Frame;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.GridLayout;
import java.awt.Insets;
import java.awt.event.ActionEvent;
import java.util.Vector;

import javax.swing.JPanel;
import javax.swing.JRadioButton;
import javax.swing.JLabel;
import javax.swing.JTextField;
import javax.swing.BorderFactory;
import javax.swing.ButtonGroup;

public class JDialogPhasePreservingDenoising extends JDialogScriptableBase implements AlgorithmInterface, ActionDiscovery, ScriptableActionInterface {

    //~ Static fields/initializers -------------------------------------------------------------------------------------

    /** Use serialVersionUID for interoperability. */
    //private static final long serialVersionUID;

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** DOCUMENT ME! */
    private PhasePreservingDenoising ppDenoisingAlgo;
    
    /** Source image. */
    private ModelImage image;
    
    /** DOCUMENT ME! */
    private ButtonGroup destinationGroup;

    /** DOCUMENT ME! */
    private JPanel destinationPanel;
    
    private JPanel paramPanel;
    
    private JRadioButton newImage;
    
    private JRadioButton replaceImage;

    /** DOCUMENT ME! */
    private int displayLoc; // Flag indicating if a new image is to be generated
    
    private ModelImage resultImage = null;
    
    private ViewUserInterface userInterface;
    
    private String[] titles;

    // Number of filter scales to use (5-7) - the more scales used the more low frequencies are covered
 	private int nscale = 5;
 	// Number of orientations to use (6)
 	private int norient = 6;
 	// Multiplying factor between successive scales (2.5-3)
 	private double mult = 2.5;
 	// Wavelength of smallest scale factor (2)
 	private double minwavelength = 2.0;
 	// Ratio of the standard deviation of the Gaussian describing the log Gabor's transfer function
 	// in the frequency domain to the filter center frequency (0.55)
 	private double sigmaonf = 0.55;
 	// Ratio of angular interval between filter orientations and the standard deviation of the
 	// angular Gaussian (1)
 	// function used to construct filters in the frequency plane
 	private double dthetaonsigma = 1.0;
 	// Number of standard deviations of noise to reject (2-3)
 	private double k = 3.0;
 	// Degree of soft thresholding (0-hard to 1-soft)
 	private double softness = 1.0;
 	private boolean useProgressBar = true;
    private JLabel labelScale;
	private JTextField textScale;
	private JLabel labelOrient;
	private JTextField textOrient;
	private JLabel labelMult;
	private JTextField textMult;
	private JLabel labelMinwave;
	private JTextField textMinwave;
	private JLabel labelSigmaon;
	private JTextField textSigmaon;
	private JLabel labelDtheta;
	private JTextField textDtheta;
	private JLabel labelK;
	private JTextField textK;
	private JLabel labelSoft;
	private JTextField textSoft;
    
	  //~ Constructors ---------------------------------------------------------------------------------------------------

	    /**
	     * Empty constructor needed for dynamic instantiation (used during scripting).
	     */
	    public JDialogPhasePreservingDenoising() { }

	    /**
	     * Creates a new JDialogPhasePreservingDenoising object.
	     *
	     * @param  theParentFrame  Parent frame.
	     * @param  im              Source image.
	     */
	    public JDialogPhasePreservingDenoising(Frame theParentFrame, ModelImage im) {
	        super(theParentFrame, false);
	        image = im;
	        userInterface = ViewUserInterface.getReference();
	        init();
	        setVisible(true);
	    }
	    
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
	            //MipavUtil.showHelp("");
	        } else { 
	            super.actionPerformed(event);
	        }
	    }
	    
	    /**
	     * Initializes the GUI by creating the components, placing them in the dialog, and displaying them.
	     */
	    private void init() {
	        setForeground(Color.black);

	        setTitle("Phase Preserving Denoising");

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

	        paramPanel = new JPanel(new GridBagLayout());
	        paramPanel.setForeground(Color.black);
	        paramPanel.setBorder(buildTitledBorder("Parameters"));
	        mainPanel.add(paramPanel, gbc);

	        GridBagConstraints gbc2 = new GridBagConstraints();
	        gbc2.gridwidth = 2;
	        gbc2.gridheight = 1;
	        gbc2.anchor = GridBagConstraints.WEST;
	        gbc2.weightx = 1;
	        gbc2.insets = new Insets(3, 3, 3, 3);
	        gbc2.gridx = 0;
	        gbc2.gridy = 0;
	        gbc2.fill = GridBagConstraints.HORIZONTAL;
	        
	        gbc2.gridwidth = 1;
	        gbc2.gridy++;
	        labelScale = createLabel("Number of filter scales (5-7)");
	        paramPanel.add(labelScale, gbc2);
	        
	        gbc2.gridx = 1;
	        textScale = createTextField("5");
	        paramPanel.add(textScale, gbc2);
	        
	        gbc2.gridx = 0;
	        gbc2.gridy++;
	        labelOrient = createLabel("Number of orientations");
	        paramPanel.add(labelOrient, gbc2);
	        
	        gbc2.gridx = 1;
	        textOrient = createTextField("6");
	        paramPanel.add(textOrient, gbc2);
	        
	        gbc2.gridx = 0;
	        gbc2.gridy++;
	        labelMult = createLabel("Multiplying factor between successive scales (2.5-3)");
	        paramPanel.add(labelMult, gbc2);
	        
	        gbc2.gridx = 1;
	        textMult = createTextField("2.5");
	        paramPanel.add(textMult, gbc2);
	        
	        gbc2.gridx = 0;
	        gbc2.gridy++;
	        labelMinwave = createLabel("Wavelength of smallest scale factor");
	        paramPanel.add(labelMinwave, gbc2);
	        
	        gbc2.gridx = 1;
	        textMinwave = createTextField("2.0");
	        paramPanel.add(textMinwave, gbc2);
	        
	        gbc2.gridx = 0;
	        gbc2.gridy++;
	        labelSigmaon = createLabel("sigmaonf");
	        paramPanel.add(labelSigmaon, gbc2);
	        
	        gbc2.gridx = 1;
	        textSigmaon = createTextField("0.55");
	        paramPanel.add(textSigmaon, gbc2);
	        
	        gbc2.gridx = 0;
	        gbc2.gridy++;
	        labelDtheta = createLabel("dthetaonsigma");
	        paramPanel.add(labelDtheta, gbc2);
	        
	        gbc2.gridx = 1;
	        textDtheta = createTextField("1.0");
	        paramPanel.add(textDtheta, gbc2);
	        
	        gbc2.gridx = 0;
	        gbc2.gridy++;
	        labelK = createLabel("Number of standard deviations of noise to reject (2-3)");
	        paramPanel.add(labelK, gbc2);
	        
	        gbc2.gridx = 1;
	        textK = createTextField("3.0");
	        paramPanel.add(textK, gbc2);
	        
	        gbc2.gridx = 0;
	        gbc2.gridy++;
	        labelSoft = createLabel("Degree of soft thresholding (0-hard to 1-soft)");
	        paramPanel.add(labelSoft, gbc2);
	        
	        gbc2.gridx = 1;
	        textSoft = createTextField("1.0");
	        paramPanel.add(textSoft, gbc2);
	        
	        JPanel outputOptPanel = new JPanel(new GridLayout(1, 2));
	        destinationPanel = new JPanel(new BorderLayout());
	        destinationPanel.setForeground(Color.black);
	        destinationPanel.setBorder(buildTitledBorder("Destination"));
	        outputOptPanel.add(destinationPanel);

	        destinationGroup = new ButtonGroup();
	        newImage = new JRadioButton("New image", true);
	        newImage.setBounds(10, 16, 120, 25);
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

	        mainDialogPanel.add(mainPanel, BorderLayout.CENTER);
	        mainDialogPanel.add(buildButtons(), BorderLayout.SOUTH);

	        getContentPane().add(mainDialogPanel);

	        pack();
	        setResizable(true);
	        // setVisible(true);

	        System.gc();
	    }
	    
	    /**
	     * Once all the necessary variables are set, call the Nonlocal Means filter algorithm based on what type of image this is
	     * and whether or not there is a separate destination image.
	     */
	    protected void callAlgorithm() {
	        String name = makeImageName(image.getImageName(), "_ppDenoising");
	        int[] destExtents;

	        destExtents = new int[2];
	        destExtents[0] = image.getExtents()[0]; // X dim
	        destExtents[1] = image.getExtents()[1]; // Y dim     

	        if (displayLoc == NEW) {

	            try {

	                
	                resultImage = new ModelImage(ModelImage.DOUBLE, destExtents, name);

	                // Make algorithm
	                ppDenoisingAlgo = new PhasePreservingDenoising(resultImage, image, nscale, norient, mult, minwavelength, sigmaonf, dthetaonsigma,
	            			k, softness, useProgressBar);

	                // This is very important. Adding this object as a listener allows the algorithm to
	                // notify this object when it has completed of failed. See algorithm performed event.
	                // This is made possible by implementing AlgorithmedPerformed interface
	                ppDenoisingAlgo.addListener(this);
	                createProgressBar(image.getImageName(), ppDenoisingAlgo);

	                // Hide dialog
	                setVisible(false);

	                if (isRunInSeparateThread()) {

	                    // Start the thread as a low priority because we wish to still have user interface work fast
	                    if (ppDenoisingAlgo.startMethod(Thread.MIN_PRIORITY) == false) {
	                        MipavUtil.displayError("A thread is already running on this object");
	                    }
	                } else {
	                    ppDenoisingAlgo.run();
	                }
	            } catch (OutOfMemoryError x) {
	                MipavUtil.displayError("Dialog Phase Preserving Denoising: unable to allocate enough memory");

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
	            	// Make algorithm
	            	ppDenoisingAlgo = new PhasePreservingDenoising(image, nscale, norient, mult, minwavelength, sigmaonf, dthetaonsigma,
	            			k, softness, useProgressBar);

	                // This is very important. Adding this object as a listener allows the algorithm to
	                // notify this object when it has completed of failed. See algorithm performed event.
	                // This is made possible by implementing AlgorithmedPerformed interface
	                ppDenoisingAlgo.addListener(this);
	                createProgressBar(image.getImageName(), ppDenoisingAlgo);

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
	                    if (ppDenoisingAlgo.startMethod(Thread.MIN_PRIORITY) == false) {
	                        MipavUtil.displayError("A thread is already running on this object");
	                    }
	                } else {
	                    ppDenoisingAlgo.run();
	                }
	            } catch (OutOfMemoryError x) {
	                MipavUtil.displayError("Dialog Phase Preserving Denoising: unable to allocate enough memory");

	                return;
	            }
	        }
	    }
	    
	    /**
	     * This method is required if the AlgorithmPerformed interface is implemented. It is called by the algorithms when
	     * it has completed or failed to to complete, so that the dialog can be display the result image and/or clean up.
	     *
	     * @param  algorithm  Algorithm that caused the event.
	     */
	    public void algorithmPerformed(AlgorithmBase algorithm) {

	        if (algorithm instanceof PhasePreservingDenoising) {
	            image.clearMask();

	            if ((ppDenoisingAlgo.isCompleted() == true) && (resultImage != null)) {

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

	        ppDenoisingAlgo.finalize();
	        ppDenoisingAlgo = null;
	        dispose();
	    }
	    
	    /**
	     * Use the GUI results to set up the variables needed to run the algorithm.
	     *
	     * @return  <code>true</code> if parameters set successfully, <code>false</code> otherwise.
	     */
	    private boolean setVariables() {
	        String tmpStr;
	        
	        tmpStr = textScale.getText();

	        if (testParameter(tmpStr, 5, 7)) {
	            nscale = Integer.valueOf(tmpStr).intValue();
	        } else {
	            MipavUtil.displayError("Number of scales must be between 5 and 7");
	            textScale.requestFocus();
	            textScale.selectAll();

	            return false;
	        }
	        
        	tmpStr = textOrient.getText();
        	
        	if (testParameter(tmpStr, 3, 24)) {
	            norient = Integer.valueOf(tmpStr).intValue();
	        } else {
	            MipavUtil.displayError("Number of orientations must be between 3 and 24");
	            textOrient.requestFocus();
	            textOrient.selectAll();

	            return false;
	        }
        	
            tmpStr = textMult.getText();
        	
        	if (testParameter(tmpStr, 2.5, 3.0)) {
	            mult = Double.valueOf(tmpStr).doubleValue();
	        } else {
	            MipavUtil.displayError("mult must be between 2.5 and 3.0");
	            textMult.requestFocus();
	            textMult.selectAll();

	            return false;
	        }
        	
            tmpStr = textMinwave.getText();
        	
        	if (testParameter(tmpStr, 1.2, 4.0)) {
	            minwavelength = Double.valueOf(tmpStr).doubleValue();
	        } else {
	            MipavUtil.displayError("minwavelength must be between 1.2 and 4.0");
	            textMinwave.requestFocus();
	            textMinwave.selectAll();

	            return false;
	        }
        	
            tmpStr = textSigmaon.getText();
        	
        	if (testParameter(tmpStr, 0.2, 2.0)) {
	            sigmaonf = Double.valueOf(tmpStr).doubleValue();
	        } else {
	            MipavUtil.displayError("sigmaonf must be between 0.2 and 2.0");
	            textSigmaon.requestFocus();
	            textSigmaon.selectAll();

	            return false;
	        }
        	
            tmpStr = textDtheta.getText();
        	
        	if (testParameter(tmpStr, 0.2, 5.0)) {
	            dthetaonsigma = Double.valueOf(tmpStr).doubleValue();
	        } else {
	            MipavUtil.displayError("dthetaonsigma must be between 0.2 and 5.0");
	            textDtheta.requestFocus();
	            textDtheta.selectAll();

	            return false;
	        }	
        	
            tmpStr = textK.getText();
        	
        	if (testParameter(tmpStr, 2.0, 3.0)) {
	            k = Double.valueOf(tmpStr).doubleValue();
	        } else {
	            MipavUtil.displayError("Number of standard deviations of noise to reject must be between 2.0 and 3.0");
	            textK.requestFocus();
	            textK.selectAll();

	            return false;
	        }
        	
            tmpStr = textSoft.getText();
        	
        	if (testParameter(tmpStr, 0.0, 1.0)) {
	            softness = Double.valueOf(tmpStr).doubleValue();
	        } else {
	            MipavUtil.displayError("Degree of soft thresholding must be between 0.0 and 1.0");
	            textSoft.requestFocus();
	            textSoft.selectAll();

	            return false;
	        }	
	        
	        if (replaceImage.isSelected()) {
	            displayLoc = REPLACE;
	        } else if (newImage.isSelected()) {
	            displayLoc = NEW;
	        }
	        
	        return true;
	    }
	    
	    /**
	     * Returns whether the action has successfully completed its execution.
	     * 
	     * @return True, if the action is complete. False, if the action failed or is still running.
	     */
	    public boolean isActionComplete() {
	        return isComplete();
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
	            table.put(new ParameterInt("n_scale", 5));
	            table.put(new ParameterInt("n_orient", 6));
	            table.put(new ParameterDouble("mul", 2.5));
	            table.put(new ParameterDouble("min_wave", 2.0));
	            table.put(new ParameterDouble("sigma_on", 0.55));
	            table.put(new ParameterDouble("d_theta", 1.0));
	            table.put(new ParameterDouble("std", 3.0));
	            table.put(new ParameterDouble("soft", 1.0));
	        } catch (final ParserException e) {
	            // this shouldn't really happen since there isn't any real parsing going on...
	            e.printStackTrace();
	        }

	        return table;
	    }
	    
	    protected void setGUIFromParams() {
	        image = scriptParameters.retrieveInputImage();
	        userInterface = ViewUserInterface.getReference();
	        parentFrame = image.getParentFrame();

	        if (scriptParameters.doOutputNewImage()) {
	            setDisplayLocNew();
	        } else {
	            setDisplayLocReplace();
	        }
	        
	        nscale = scriptParameters.getParams().getInt("n_scale");
	        norient = scriptParameters.getParams().getInt("n_orient");
	        mult = scriptParameters.getParams().getDouble("mul");
	        minwavelength = scriptParameters.getParams().getDouble("min_wave");
	        sigmaonf = scriptParameters.getParams().getDouble("sigma_on");
	        dthetaonsigma = scriptParameters.getParams().getDouble("d_theta");
	        k = scriptParameters.getParams().getDouble("std");
	        softness = scriptParameters.getParams().getDouble("soft");
	    }
	    
	    /**
	     * {@inheritDoc}
	     */
	    protected void storeParamsFromGUI() throws ParserException {
	        scriptParameters.storeInputImage(image);
	        scriptParameters.storeOutputImageParams(getResultImage(), (displayLoc == NEW));
	        scriptParameters.getParams().put(ParameterFactory.newParameter("n_scale", nscale));
	        scriptParameters.getParams().put(ParameterFactory.newParameter("n_orient", norient));
	        scriptParameters.getParams().put(ParameterFactory.newParameter("mul", mult));
	        scriptParameters.getParams().put(ParameterFactory.newParameter("min_wave", minwavelength));
	        scriptParameters.getParams().put(ParameterFactory.newParameter("sigma_on", sigmaonf));
	        scriptParameters.getParams().put(ParameterFactory.newParameter("d_theta", dthetaonsigma));
	        scriptParameters.getParams().put(ParameterFactory.newParameter("std", k));
	        scriptParameters.getParams().put(ParameterFactory.newParameter("soft", softness));
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
	     * Return meta-information about this discoverable action for categorization and labeling purposes.
	     * 
	     * @return Metadata for this action.
	     */
	    public ActionMetadata getActionMetadata() {
	        return new MipavActionMetadata() {
	            public String getCategory() {
	                return new String("Algorithms.Filters)");
	            }

	            public String getDescription() {
	                return new String("Applies a Phase Preserving Denoising filter.");
	            }

	            public String getDescriptionLong() {
	                return new String("Applies a Phase Preserving Denoising filter.");
	            }

	            public String getShortLabel() {
	                return new String("Phase Preserving Denoising");
	            }

	            public String getLabel() {
	                return new String("Phase Preserving Denoising");
	            }

	            public String getName() {
	                return new String("Phase Preserving Denoising");
	            }
	        };
	    }


}