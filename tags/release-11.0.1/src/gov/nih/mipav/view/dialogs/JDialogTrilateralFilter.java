package gov.nih.mipav.view.dialogs;


import gov.nih.mipav.model.algorithms.AlgorithmBase;
import gov.nih.mipav.model.algorithms.AlgorithmInterface;
import gov.nih.mipav.model.algorithms.filters.AlgorithmTrilateralFilter;
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

public class JDialogTrilateralFilter extends JDialogScriptableBase implements AlgorithmInterface, ActionDiscovery, ScriptableActionInterface {

    //~ Static fields/initializers -------------------------------------------------------------------------------------

    /** Use serialVersionUID for interoperability. */
    //private static final long serialVersionUID;

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** DOCUMENT ME! */
    private AlgorithmTrilateralFilter trilateralFilterAlgo;
    
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
    
    private ButtonGroup typeGroup;
    private JRadioButton LUTButton;
    private JRadioButton fastButton;
    private final int TYPE_LUT = 0;
	
	private final int TYPE_FAST = 1;
	
	private int filterType = TYPE_FAST;

    private double sigmaSpace = 5.0;
    private JLabel labelSigma;
	private JTextField textSigma;
	
	// epsilon, the required accuracy, is only used for filterType = TYPE_FAST
	private double epsilon = 1.0E-12;
	private JLabel labelEpsilon;
	private JTextField textEpsilon;
    
	  //~ Constructors ---------------------------------------------------------------------------------------------------

	    /**
	     * Empty constructor needed for dynamic instantiation (used during scripting).
	     */
	    public JDialogTrilateralFilter() { }

	    /**
	     * Creates a new JDialogTrilateralFilter object.
	     *
	     * @param  theParentFrame  Parent frame.
	     * @param  im              Source image.
	     */
	    public JDialogTrilateralFilter(Frame theParentFrame, ModelImage im) {
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
	        } else if ((source == fastButton) || (source == LUTButton)) {
	        	labelEpsilon.setEnabled(fastButton.isSelected());
	        	textEpsilon.setEnabled(fastButton.isSelected());
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

	        setTitle("Trilateral Filter");

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
	        
	        typeGroup = new ButtonGroup();
	        fastButton = new JRadioButton("Fast filter", true);
	        fastButton.setFont(serif12);
	        fastButton.addActionListener(this);
	        typeGroup.add(fastButton);
	        paramPanel.add(fastButton, gbc2);
	        
	        gbc2.gridy++;
	        LUTButton = new JRadioButton("LUT filter", false);
	        LUTButton.setFont(serif12);
	        LUTButton.addActionListener(this);
	        typeGroup.add(LUTButton);
	        paramPanel.add(LUTButton, gbc2);
	        
	        gbc2.gridwidth = 1;
	        gbc2.gridy++;
	        labelSigma = createLabel("Spatial sigma");
	        paramPanel.add(labelSigma, gbc2);
	        
	        gbc2.gridx = 1;
	        textSigma = createTextField("5.0");
	        paramPanel.add(textSigma, gbc2);
	        
	        gbc2.gridx = 0;
	        gbc2.gridy++;
	        labelEpsilon = createLabel("Required accuracy");
	        paramPanel.add(labelEpsilon, gbc2);
	        
	        gbc2.gridx = 1;
	        textEpsilon = createTextField("1.0E-12");
	        paramPanel.add(textEpsilon, gbc2);
	        
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
	        String name = makeImageName(image.getImageName(), "_trilateral");
	        int[] destExtents;

	        destExtents = new int[2];
	        destExtents[0] = image.getExtents()[0]; // X dim
	        destExtents[1] = image.getExtents()[1]; // Y dim     

	        if (displayLoc == NEW) {

	            try {

	                
	                resultImage = new ModelImage(ModelImage.DOUBLE, destExtents, name);

	                // Make algorithm
	                trilateralFilterAlgo = new AlgorithmTrilateralFilter(resultImage, image, sigmaSpace,
	                		epsilon, filterType);

	                // This is very important. Adding this object as a listener allows the algorithm to
	                // notify this object when it has completed of failed. See algorithm performed event.
	                // This is made possible by implementing AlgorithmedPerformed interface
	                trilateralFilterAlgo.addListener(this);
	                createProgressBar(image.getImageName(), trilateralFilterAlgo);

	                // Hide dialog
	                setVisible(false);

	                if (isRunInSeparateThread()) {

	                    // Start the thread as a low priority because we wish to still have user interface work fast
	                    if (trilateralFilterAlgo.startMethod(Thread.MIN_PRIORITY) == false) {
	                        MipavUtil.displayError("A thread is already running on this object");
	                    }
	                } else {
	                    trilateralFilterAlgo.run();
	                }
	            } catch (OutOfMemoryError x) {
	                MipavUtil.displayError("Dialog Trilateral Filter: unable to allocate enough memory");

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
	            	trilateralFilterAlgo = new AlgorithmTrilateralFilter(null, image, sigmaSpace,
	                		epsilon, filterType);

	                // This is very important. Adding this object as a listener allows the algorithm to
	                // notify this object when it has completed of failed. See algorithm performed event.
	                // This is made possible by implementing AlgorithmedPerformed interface
	                trilateralFilterAlgo.addListener(this);
	                createProgressBar(image.getImageName(), trilateralFilterAlgo);

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
	                    if (trilateralFilterAlgo.startMethod(Thread.MIN_PRIORITY) == false) {
	                        MipavUtil.displayError("A thread is already running on this object");
	                    }
	                } else {
	                    trilateralFilterAlgo.run();
	                }
	            } catch (OutOfMemoryError x) {
	                MipavUtil.displayError("Dialog Trilateral Filter: unable to allocate enough memory");

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

	        if (algorithm instanceof AlgorithmTrilateralFilter) {
	            image.clearMask();

	            if ((trilateralFilterAlgo.isCompleted() == true) && (resultImage != null)) {

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

	        trilateralFilterAlgo.finalize();
	        trilateralFilterAlgo = null;
	        dispose();
	    }
	    
	    /**
	     * Use the GUI results to set up the variables needed to run the algorithm.
	     *
	     * @return  <code>true</code> if parameters set successfully, <code>false</code> otherwise.
	     */
	    private boolean setVariables() {
	        String tmpStr;
	        
	        if (fastButton.isSelected()) {
	        	filterType = TYPE_FAST;
	        }
	        else {
	        	filterType = TYPE_LUT;
	        }
	        
	        tmpStr = textSigma.getText();

	        if (testParameter(tmpStr, 3, 99)) {
	            sigmaSpace = Double.valueOf(tmpStr).doubleValue();
	        } else {
	            MipavUtil.displayError("Sigma space must be between 3 and 99");
	            textSigma.requestFocus();
	            textSigma.selectAll();

	            return false;
	        }
	        
	        if (filterType == TYPE_FAST) {
	        	tmpStr = textEpsilon.getText();
	        	
	        	if (testParameter(tmpStr, 1.0E-30, 1.0)) {
		            epsilon = Double.valueOf(tmpStr).doubleValue();
		        } else {
		            MipavUtil.displayError("epsilon must be between 1.0E-30 and 1.0");
		            textEpsilon.requestFocus();
		            textEpsilon.selectAll();

		            return false;
		        }	
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
	            table.put(new ParameterInt("filter_type", TYPE_FAST));
	            table.put(new ParameterDouble("sigma_space", 5.0));
	            table.put(new ParameterDouble("eps", 1.0E-9));
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
	        
	        filterType = scriptParameters.getParams().getInt("filter_type");
	        sigmaSpace = scriptParameters.getParams().getDouble("sigma_space");
	        epsilon = scriptParameters.getParams().getDouble("eps");
	    }
	    
	    /**
	     * {@inheritDoc}
	     */
	    protected void storeParamsFromGUI() throws ParserException {
	        scriptParameters.storeInputImage(image);
	        scriptParameters.storeOutputImageParams(getResultImage(), (displayLoc == NEW));
	        scriptParameters.getParams().put(ParameterFactory.newParameter("filter_type", filterType));
	        scriptParameters.getParams().put(ParameterFactory.newParameter("sigma_space", sigmaSpace));
	        scriptParameters.getParams().put(ParameterFactory.newParameter("eps", epsilon));
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
	                return new String("Applies a Trilateral filter.");
	            }

	            public String getDescriptionLong() {
	                return new String("Applies a Trilateral filter.");
	            }

	            public String getShortLabel() {
	                return new String("Trilateral");
	            }

	            public String getLabel() {
	                return new String("Trilateral");
	            }

	            public String getName() {
	                return new String("Trilateral");
	            }
	        };
	    }


}