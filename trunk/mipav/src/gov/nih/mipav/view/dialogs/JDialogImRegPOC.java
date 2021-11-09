package gov.nih.mipav.view.dialogs;


import gov.nih.mipav.model.algorithms.*;
import gov.nih.mipav.model.algorithms.registration.ImRegPOC;
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
 * source image. Algorithms are executed in their own thread.
 *
 * @see  ImRegPOC
 */
public class JDialogImRegPOC extends JDialogScriptableBase
        implements AlgorithmInterface, ScriptableActionInterface, ActionDiscovery {

    //~ Static fields/initializers -------------------------------------------------------------------------------------

    /** Use serialVersionUID for interoperability. */
    //private static final long serialVersionUID;

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** DOCUMENT ME! */
    private ImRegPOC imRegPOCAlgo;

    /** DOCUMENT ME! */
    private ButtonGroup destinationGroup;

    /** DOCUMENT ME! */
    private JPanel destinationPanel;
    
    private JPanel paramPanel;
    
    private JRadioButton newImage;
    
    private JRadioButton replaceImage;

    /** DOCUMENT ME! */
    private int displayLoc; // Flag indicating if a new image is to be generated

    // or if the source image is to be replaced

    /** DOCUMENT ME! */
    private ModelImage matchImage; // source image
    
    private JLabel labelReference;
    
    private ModelImage refImage = null;
    
    private ModelImage resultImage = null;
    
    private ViewUserInterface userInterface;
    
    private String[] titles;
    
    private JComboBox<String> comboBoxImage;
    
    private double alpha = 0.5;
    
    private double beta = 0.8;
    
    private JTextField textAlpha;
    
    private JTextField textBeta;
    
  //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Empty constructor needed for dynamic instantiation (used during scripting).
     */
    public JDialogImRegPOC() { }

    /**
     * Creates a new JDialogImRegPOC object.
     *
     * @param  theParentFrame  Parent frame.
     * @param  im              Source image.
     */
    public JDialogImRegPOC(Frame theParentFrame, ModelImage im) {
        super(theParentFrame, false);
        matchImage = im;
        userInterface = ViewUserInterface.getReference();
        init();
        setVisible(true);
    }
    
    public void actionPerformed(ActionEvent event) {
        String command = event.getActionCommand();
        //Object source = event.getSource();

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

        setTitle("Phase Only Correlation 2D Image Registration");

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
        gbc2.gridwidth = 1;
        gbc2.gridheight = 1;
        gbc2.anchor = GridBagConstraints.WEST;
        gbc2.weightx = 1;
        gbc2.insets = new Insets(3, 3, 3, 3);
        gbc2.gridx = 0;
        gbc2.gridy = 0;
        gbc2.fill = GridBagConstraints.HORIZONTAL;
        
        JLabel labelSource = createLabel("Match image: ");
        paramPanel.add(labelSource, gbc2);
        
        gbc2.gridx = 1;
        String imageName = matchImage.getImageName();
        JLabel labelImage = createLabel(imageName);
        paramPanel.add(labelImage, gbc2);
        
        gbc2.gridx = 0;
        gbc2.gridy = 1;
        gbc2.gridwidth = 1;
        labelReference = createLabel("Reference image: ");
        paramPanel.add(labelReference, gbc2);
        
        gbc2.gridx = 1;
        comboBoxImage = new JComboBox<String>();
        buildComboBoxImage();
        comboBoxImage.setFont(serif12);
        comboBoxImage.setBackground(Color.white);
        paramPanel.add(comboBoxImage, gbc2);
        
        gbc2.gridx = 0;
        gbc2.gridy++;
        JLabel labelAlpha = createLabel("Low pass factor alpha");
        paramPanel.add(labelAlpha, gbc2);
        
        gbc2.gridx = 1;
        textAlpha = createTextField("0.5");
        paramPanel.add(textAlpha, gbc2);
        
        gbc2.gridx = 0;
        gbc2.gridy++;
        JLabel labelBeta = createLabel("High pass factor beta");
        paramPanel.add(labelBeta, gbc2);
        
        gbc2.gridx = 1;
        textBeta = createTextField("0.8");
        paramPanel.add(textBeta, gbc2);
        
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
        if (matchImage.getLockStatus() == ModelStorageBase.UNLOCKED) {
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
     * Builds a list of images to operate on from the source image.
     */
    private void buildComboBoxImage() {
        int j;
        ViewUserInterface UI;
        boolean sameDims = true;

        comboBoxImage.removeAllItems();

        UI = ViewUserInterface.getReference();

        Enumeration<String> names = UI.getRegisteredImageNames();

        // Add images from user interface that have the same exact dimensionality
        // Guaranteed to have at least one unique potential image B, because it's
        // tested for in ViewJFrameImage before this dialog is created.
        while (names.hasMoreElements()) {
            String name = names.nextElement();
            sameDims = true;

            if (!matchImage.getImageName().equals(name)) {
                ModelImage img = UI.getRegisteredImageByName(name);

                if (UI.getFrameContainingImage(img) != null) {
                	
                	if ((!img.isColorImage()) && (!img.isComplexImage())) {

	                    if (matchImage.getNDims() == img.getNDims()) {
	
	                        for (j = 0; j < matchImage.getNDims(); j++) {
	
	                            if (matchImage.getExtents()[j] != img.getExtents()[j]) {
	                                sameDims = false;
	                            }
	                        }
	                        if (sameDims == true) {
	                            comboBoxImage.addItem(name);
	                        }
	
	                    }
                	}
                }
            }
        }
    }
    
    /**
     * Use the GUI results to set up the variables needed to run the algorithm.
     *
     * @return  <code>true</code> if parameters set successfully, <code>false</code> otherwise.
     */
    private boolean setVariables() {
        String tmpStr;
        
        
    	if(comboBoxImage.getModel().getSize() == 0) {
            MipavUtil.displayError("No reference image was added to combBoxImage.");
            return false;
        }
    	refImage = userInterface.getRegisteredImageByName((String) comboBoxImage.getSelectedItem());
    	
    	tmpStr = textAlpha.getText();

        if (testParameter(tmpStr, 0.01, 0.99)) {
            alpha = Double.valueOf(tmpStr).doubleValue();
        } else {
            MipavUtil.displayError("alpha must be between 0.01 and 0.99");
            textAlpha.requestFocus();
            textAlpha.selectAll();

            return false;
        }
        
        tmpStr = textBeta.getText();
        
        if (testParameter(tmpStr, 0.01, 0.99)) {
            beta = Double.valueOf(tmpStr).doubleValue();
        } else {
            MipavUtil.displayError("beta must be between 0.01 and 0.99");
            textBeta.requestFocus();
            textBeta.selectAll();

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
     * Once all the necessary variables are set, call the Nonlocal Means filter algorithm based on what type of image this is
     * and whether or not there is a separate destination image.
     */
    protected void callAlgorithm() {
        String name = makeImageName(matchImage.getImageName(), "_register");
        int[] destExtents;

        destExtents = new int[2];
        destExtents[0] = matchImage.getExtents()[0]; // X dim
        destExtents[1] = matchImage.getExtents()[1]; // Y dim     

        if (displayLoc == NEW) {

            try {

                // Make result image of double type
                resultImage = new ModelImage(ModelImage.DOUBLE, destExtents, name);

                // Make algorithm
                imRegPOCAlgo = new ImRegPOC(resultImage, refImage, matchImage, alpha, beta);

                // This is very important. Adding this object as a listener allows the algorithm to
                // notify this object when it has completed of failed. See algorithm performed event.
                // This is made possible by implementing AlgorithmedPerformed interface
                imRegPOCAlgo.addListener(this);
                createProgressBar(matchImage.getImageName(), imRegPOCAlgo);

                // Hide dialog
                setVisible(false);

                if (isRunInSeparateThread()) {

                    // Start the thread as a low priority because we wish to still have user interface work fast
                    if (imRegPOCAlgo.startMethod(Thread.MIN_PRIORITY) == false) {
                        MipavUtil.displayError("A thread is already running on this object");
                    }
                } else {
                    imRegPOCAlgo.run();
                }
            } catch (OutOfMemoryError x) {
                MipavUtil.displayError("Dialog Image Registration Phase Only Correlation: unable to allocate enough memory");

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
            	imRegPOCAlgo = new ImRegPOC(null, refImage, matchImage, alpha, beta);

                // This is very important. Adding this object as a listener allows the algorithm to
                // notify this object when it has completed of failed. See algorithm performed event.
                // This is made possible by implementing AlgorithmedPerformed interface
                imRegPOCAlgo.addListener(this);
                createProgressBar(matchImage.getImageName(), imRegPOCAlgo);

                // Hide the dialog since the algorithm is about to run.
                setVisible(false);

                // These next lines set the titles in all frames where the source image is displayed to
                // "locked - " image name so as to indicate that the image is now read/write locked!
                // The image frames are disabled and then unregisted from the userinterface until the
                // algorithm has completed.
                Vector<ViewImageUpdateInterface> imageFrames = matchImage.getImageFrameVector();
                titles = new String[imageFrames.size()];

                for (int i = 0; i < imageFrames.size(); i++) {
                    titles[i] = ((Frame) (imageFrames.elementAt(i))).getTitle();
                    ((Frame) (imageFrames.elementAt(i))).setTitle("Locked: " + titles[i]);
                    ((Frame) (imageFrames.elementAt(i))).setEnabled(false);
                    userInterface.unregisterFrame((Frame) (imageFrames.elementAt(i)));
                }

                if (isRunInSeparateThread()) {

                    // Start the thread as a low priority because we wish to still have user interface work fast
                    if (imRegPOCAlgo.startMethod(Thread.MIN_PRIORITY) == false) {
                        MipavUtil.displayError("A thread is already running on this object");
                    }
                } else {
                    imRegPOCAlgo.run();
                }
            } catch (OutOfMemoryError x) {
                MipavUtil.displayError("Dialog Image Registration Phase Only Correlation: unable to allocate enough memory");

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

        if (algorithm instanceof ImRegPOC) {
            matchImage.clearMask();

            if ((imRegPOCAlgo.isCompleted() == true) && (resultImage != null)) {

                updateFileInfo(matchImage, resultImage);
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
                Vector<ViewImageUpdateInterface> imageFrames = matchImage.getImageFrameVector();

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

                matchImage.notifyImageDisplayListeners(null, true);
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

        imRegPOCAlgo.finalize();
        imRegPOCAlgo = null;
        dispose();
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
     * Accessor to get the result image.
     *
     * @return  Result image.
     */
    public ModelImage getResultImage() {
        return resultImage;
    }
    
    protected void setGUIFromParams() {
        matchImage = scriptParameters.retrieveInputImage();
        refImage = scriptParameters.retrieveInputImage(2);
        userInterface = ViewUserInterface.getReference();
        parentFrame = matchImage.getParentFrame();

        if (scriptParameters.doOutputNewImage()) {
            setDisplayLocNew();
        } else {
            setDisplayLocReplace();
        }
        
        alpha = scriptParameters.getParams().getDouble("alph");
        beta = scriptParameters.getParams().getDouble("bet");
    }
    
    /**
     * {@inheritDoc}
     */
    protected void storeParamsFromGUI() throws ParserException {
        scriptParameters.storeInputImage(matchImage);
        scriptParameters.storeInputImage(refImage);
        
        if (getResultImage() != null) {
            scriptParameters.storeImageInRecorder(getResultImage());
        }
        
        scriptParameters.getParams().put(ParameterFactory.newParameter("alph", alpha));
        scriptParameters.getParams().put(ParameterFactory.newParameter("bet", beta));
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
                return matchImage.getImageName();
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
                return new String("Algorithms.Registration");
            }

            public String getDescription() {
                return new String("Applies a Phase Only Correlation Registration.");
            }

            public String getDescriptionLong() {
                return new String("Applies a Phase Only Correlation Registration.");
            }

            public String getShortLabel() {
                return new String("POCRegistration");
            }

            public String getLabel() {
                return new String("POC Registration");
            }

            public String getName() {
                return new String("POC Registration");
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
            table.put(new ParameterExternalImage(AlgorithmParameters.getInputImageLabel(2)));
            table.put(new ParameterBoolean(AlgorithmParameters.DO_OUTPUT_NEW_IMAGE, true));
            table.put(new ParameterDouble("alph",0.5));
            table.put(new ParameterDouble("bet",0.8));
            
	    } catch (final ParserException e) {
	        // this shouldn't really happen since there isn't any real parsing going on...
	        e.printStackTrace();
	    }
	
	    return table;
    }

}