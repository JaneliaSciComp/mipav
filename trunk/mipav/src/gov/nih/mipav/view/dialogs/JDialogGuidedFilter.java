package gov.nih.mipav.view.dialogs;


import gov.nih.mipav.model.algorithms.*;
import gov.nih.mipav.model.algorithms.filters.*;
import gov.nih.mipav.model.algorithms.utilities.AlgorithmImageCalculator;
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
 * @see  AlgorithmGuidedFilter
 */
public class JDialogGuidedFilter extends JDialogScriptableBase
        implements AlgorithmInterface, ScriptableActionInterface, ActionDiscovery {

    //~ Static fields/initializers -------------------------------------------------------------------------------------

    /** Use serialVersionUID for interoperability. */
    //private static final long serialVersionUID;

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** DOCUMENT ME! */
    private AlgorithmGuidedFilter guidedFilterAlgo;

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
    private ModelImage image; // source image
    
    private ModelImage resultImage = null;
    
    private ViewUserInterface userInterface;
    
    private String[] titles;
    
    private ButtonGroup guidedGroup;
    
    private JRadioButton sameButton;
    
    private JRadioButton differentButton;
    
    private boolean sameImage = true;
    
    private JComboBox comboBoxImage;
    
    private JLabel guidedLabel;
    
    private GridBagConstraints gbc2;
    
    // Filter radius
	private int radius = 3;

	private JTextField textRadius;
	// Value controlling sharpness
	private double eps = 1.0E-5;
	private JTextField textEps;
	
	private ModelImage guidedImage;
	
	//~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Empty constructor needed for dynamic instantiation (used during scripting).
     */
    public JDialogGuidedFilter() { }

    /**
     * Creates a new JDialogGuidedFilter object.
     *
     * @param  theParentFrame  Parent frame.
     * @param  im              Source image.
     */
    public JDialogGuidedFilter(Frame theParentFrame, ModelImage im) {
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
        } else if ((source == sameButton) || (source == differentButton)) {
        	if (sameButton.isSelected()) {
        		guidedLabel.setEnabled(false);
        		comboBoxImage.setEnabled(false);
        	}
        	else {
        		guidedLabel.setEnabled(true);
        		buildComboBoxImage();

                if(comboBoxImage.getModel().getSize() == 0) {
                    MipavUtil.displayWarning("A guided image must have the same extents as the source image");
                    comboBoxImage.setEnabled(false);
                    return;
                }
                gbc2.gridx = 1;
                gbc2.gridy = 3;
                gbc2.gridwidth = 1;
                comboBoxImage.setEnabled(true);
                paramPanel.add(comboBoxImage, gbc2);
        	}
        } else { 
            super.actionPerformed(event);
        }
    }
    
    /**
     * Builds a list of images to operate on from the template image.
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

            if (!image.getImageName().equals(name)) {
                ModelImage img = UI.getRegisteredImageByName(name);

                if (UI.getFrameContainingImage(img) != null) {

                    if (image.getNDims() == img.getNDims()) {

                        for (j = 0; j < image.getNDims(); j++) {

                            if (image.getExtents()[j] != img.getExtents()[j]) {
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
    
    /**
     * Initializes the GUI by creating the components, placing them in the dialog, and displaying them.
     */
    private void init() {
        setForeground(Color.black);

        setTitle("Guided Filter");

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

        gbc2 = new GridBagConstraints();
        gbc2.gridwidth = 1;
        gbc2.gridheight = 1;
        gbc2.anchor = GridBagConstraints.WEST;
        gbc2.weightx = 1;
        gbc2.insets = new Insets(3, 3, 3, 3);
        gbc2.gridx = 0;
        gbc2.gridy = 0;
        gbc2.fill = GridBagConstraints.HORIZONTAL;
        
        JLabel labelSource = createLabel("Source image: ");
        paramPanel.add(labelSource, gbc2);
        
        JLabel labelSourceName = createLabel(image.getImageName());
        gbc2.gridx = 1;
        paramPanel.add(labelSourceName, gbc2);
        
        guidedGroup = new ButtonGroup();
        gbc2.gridx = 0;
        gbc2.gridy = 1;
        gbc2.gridwidth = 2;
        sameButton = new JRadioButton("Source image is guided image", true);
        sameButton.setFont(serif12);
        sameButton.addActionListener(this);
        guidedGroup.add(sameButton);
        paramPanel.add(sameButton, gbc2);
        
        gbc2.gridy = 2;
        differentButton = new JRadioButton("Different image is guided image", false);
        differentButton.setFont(serif12);
        differentButton.addActionListener(this);
        guidedGroup.add(differentButton);
        paramPanel.add(differentButton, gbc2);
        
        gbc2.gridy = 3;
        gbc2.gridwidth = 1;
        guidedLabel = createLabel("Guided image: ");
        guidedLabel.setEnabled(false);
        paramPanel.add(guidedLabel, gbc2);
        
        gbc2.gridx = 1;
        comboBoxImage = new JComboBox();
        comboBoxImage.setFont(serif12);
        comboBoxImage.setBackground(Color.white);
        comboBoxImage.setEnabled(false);
        paramPanel.add(comboBoxImage, gbc2);
        
        gbc2.gridx = 0;
        gbc2.gridy++;
        JLabel labelRadius = createLabel("Filter radius");
        paramPanel.add(labelRadius, gbc2);
        
        gbc2.gridx = 1;
        textRadius = createTextField("3");
        paramPanel.add(textRadius, gbc2);
        
        gbc2.gridx = 0;
        gbc2.gridy++;
        JLabel labelEps = createLabel("Sharpness value");
        paramPanel.add(labelEps, gbc2);
        
        gbc2.gridx = 1;
        textEps = createTextField("1.0E-5");
        paramPanel.add(textEps, gbc2);
        
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
        String name = makeImageName(image.getImageName(), "_guided");
        int[] destExtents;

        destExtents = new int[2];
        destExtents[0] = image.getExtents()[0]; // X dim
        destExtents[1] = image.getExtents()[1]; // Y dim     

        if (displayLoc == NEW) {

            try {

                // Make result image of float type
                if ((guidedImage.isColorImage()) || (image.isColorImage())) {
                    resultImage = new ModelImage(ModelImage.ARGB_FLOAT, destExtents, name);
                } else {
                    resultImage = new ModelImage(ModelImage.DOUBLE, destExtents, name);
                }

                // Make algorithm
                guidedFilterAlgo = new AlgorithmGuidedFilter(resultImage, image, guidedImage, radius, eps);

                // This is very important. Adding this object as a listener allows the algorithm to
                // notify this object when it has completed of failed. See algorithm performed event.
                // This is made possible by implementing AlgorithmedPerformed interface
                guidedFilterAlgo.addListener(this);
                createProgressBar(image.getImageName(), guidedFilterAlgo);

                // Hide dialog
                setVisible(false);

                if (isRunInSeparateThread()) {

                    // Start the thread as a low priority because we wish to still have user interface work fast
                    if (guidedFilterAlgo.startMethod(Thread.MIN_PRIORITY) == false) {
                        MipavUtil.displayError("A thread is already running on this object");
                    }
                } else {
                    guidedFilterAlgo.run();
                }
            } catch (OutOfMemoryError x) {
                MipavUtil.displayError("Dialog Guided Filter: unable to allocate enough memory");

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
                guidedFilterAlgo = new AlgorithmGuidedFilter(resultImage, image, guidedImage, radius, eps);

                // This is very important. Adding this object as a listener allows the algorithm to
                // notify this object when it has completed of failed. See algorithm performed event.
                // This is made possible by implementing AlgorithmedPerformed interface
                guidedFilterAlgo.addListener(this);
                createProgressBar(image.getImageName(), guidedFilterAlgo);

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
                    if (guidedFilterAlgo.startMethod(Thread.MIN_PRIORITY) == false) {
                        MipavUtil.displayError("A thread is already running on this object");
                    }
                } else {
                    guidedFilterAlgo.run();
                }
            } catch (OutOfMemoryError x) {
                MipavUtil.displayError("Dialog Guided Filter: unable to allocate enough memory");

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

        if (algorithm instanceof AlgorithmGuidedFilter) {
            image.clearMask();

            if ((guidedFilterAlgo.isCompleted() == true) && (resultImage != null)) {

                updateFileInfo(image, resultImage);
                resultImage.clearMask();

                // The algorithm has completed and produced a new image to be displayed.
                try {
                    new ViewJFrameImage(resultImage, null, new Dimension(610, 200));
                } catch (OutOfMemoryError error) {
                    MipavUtil.displayError("Out of memory: unable to open new frame");
                }
            } else if ((resultImage == null) && (!guidedImage.isColorImage())) {

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

        guidedFilterAlgo.finalize();
        guidedFilterAlgo = null;
        dispose();
    }
    
    /**
     * Use the GUI results to set up the variables needed to run the algorithm.
     *
     * @return  <code>true</code> if parameters set successfully, <code>false</code> otherwise.
     */
    private boolean setVariables() {
    	String tmpStr;

        if (newImage.isSelected()) {
            displayLoc = NEW;
        } else {
            displayLoc = REPLACE;
        }
        
        if (sameButton.isSelected()) {
        	sameImage = true;
        	guidedImage = image;
        }
        else {
        	sameImage = false;
            String selectedName = (String) comboBoxImage.getSelectedItem();
            guidedImage = ViewUserInterface.getReference().getRegisteredImageByName(selectedName);
        }
        
        tmpStr = textRadius.getText();

        if (testParameter(tmpStr, 3, 99)) {
            radius = Integer.valueOf(tmpStr).intValue();
        } else {
            MipavUtil.displayError("Filter radius must be between 3 and 99");
            textRadius.requestFocus();
            textRadius.selectAll();

            return false;
        }
        
        if ((radius % 2) == 0) {
            MipavUtil.displayError("Filter radius be an odd number");
            textRadius.requestFocus();
            textRadius.selectAll();
            return false;
        }
        
        tmpStr = textEps.getText();
    	
        if (testParameter(tmpStr, 1.0E-12, 1.0)) {
            eps = Double.valueOf(tmpStr).doubleValue();
        } else {
            MipavUtil.displayError("Sharpness value must be between 1.0E-12 and 1.0");
            textEps.requestFocus();
            textEps.selectAll();

            return false;
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
     * Accessor that returns the image.
     *
     * @return  The result image.
     */
    public ModelImage getResultImage() {
        return resultImage;
    }
    
    /**
     * Accessor to set the guided image.
     *
     * @param  im  Guided image.
     */
    public void setGuidedImage(ModelImage im) {
        guidedImage = im;
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
            table.put(new ParameterBoolean("same_image", true));
            table.put(new ParameterInt("rad_val", 3));
            table.put(new ParameterDouble("eps_val",1.0E-5));
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
        
        sameImage = scriptParameters.getParams().getBoolean("same_image");
        
        if (sameImage) {
        	setGuidedImage(image);
        }
        else {
            setGuidedImage(scriptParameters.retrieveInputImage(2));
        }
        
        radius = scriptParameters.getParams().getInt("rad_val");
        eps = scriptParameters.getParams().getDouble("eps_val");
    }
    
    protected void storeParamsFromGUI() throws ParserException {
        scriptParameters.storeInputImage(image);
        scriptParameters.storeOutputImageParams(getResultImage(), (displayLoc == NEW));
        scriptParameters.getParams().put(ParameterFactory.newParameter("same_image", sameImage));
        if (!sameImage) {
            scriptParameters.storeInputImage(guidedImage);
        }
        scriptParameters.getParams().put(ParameterFactory.newParameter("rad_val", radius));
        scriptParameters.getParams().put(ParameterFactory.newParameter("eps_val", eps));
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
                return new String("Algorithms.Filters (spatial)");
            }

            public String getDescription() {
                return new String("Applies a Guided filter.");
            }

            public String getDescriptionLong() {
                return new String("Applies a Guided filter.");
            }

            public String getShortLabel() {
                return new String("Guided");
            }

            public String getLabel() {
                return new String("Guided");
            }

            public String getName() {
                return new String("Guided");
            }
        };
    }

}