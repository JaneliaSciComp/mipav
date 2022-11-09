package gov.nih.mipav.view.dialogs;


import gov.nih.mipav.model.algorithms.*;
import gov.nih.mipav.model.algorithms.filters.*;
import gov.nih.mipav.model.scripting.*;
import gov.nih.mipav.model.scripting.parameters.*;
import gov.nih.mipav.model.structures.*;

import gov.nih.mipav.view.*;

import java.awt.*;
import java.awt.event.*;

import java.util.*;

import javax.swing.*;


/**
 * Dialog to get user input, then call the algorithm.  Algorithms are executed in their own thread.
 *
 * @see  AlgoroithmWaveletFuse
 */
public class JDialogWaveletFuse extends JDialogScriptableBase
        implements AlgorithmInterface, ScriptableActionInterface, ActionDiscovery {

    //~ Static fields/initializers -------------------------------------------------------------------------------------

    /** Use serialVersionUID for interoperability. */
    //private static final long serialVersionUID;

    //~ Instance fields ------------------------------------------------------------------------------------------------
	private ModelImage fuseImage;
	private int kernel_size = 37;
	private PyWavelets.WAVELET_NAME names[] = new PyWavelets.WAVELET_NAME[] {PyWavelets.WAVELET_NAME.SYM};
	private int orders[] = new int[] {13};
	private int max_depth = 999;
	private boolean slow = false;
    /** DOCUMENT ME! */
    private AlgorithmWaveletFuse waveletFuseAlgo;
    
    private JTextField textKernelSize;
    
    private JLabel labelName;
    
    private JComboBox<String> comboBoxName;
    
    private JLabel labelOrder;
    
    private JComboBox<String> comboBoxOrder;
    
    private String lastNameString = "Symlets";
    
    private JTextField textDepth;
    
    private ButtonGroup fastGroup;
    
    private JRadioButton fastButton;
    
    private JRadioButton slowButton;
    
    
    private JPanel paramPanel;

    /** DOCUMENT ME! */
    private ModelImage image; // source image
    
    private ModelImage resultImage = null;
    
    private ViewUserInterface userInterface;
    
    private JComboBox<String> comboBoxImage;
    
    private GridBagConstraints gbc2;
    
  //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Empty constructor needed for dynamic instantiation (used during scripting).
     */
    public JDialogWaveletFuse() { }

    /**
     * Creates a new JDialogWaveletFuse object.
     *
     * @param  theParentFrame  Parent frame.
     * @param  im              Source image.
     */
    public JDialogWaveletFuse(Frame theParentFrame, ModelImage im) {
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
        } else if (source == comboBoxName) {
        	String nameString = (String) comboBoxName.getSelectedItem();
        	if (!nameString.equals(lastNameString)) {
        		lastNameString = nameString;
        		comboBoxOrder = buildWaveletOrderComboBox(nameString);
        		gbc2.gridx = 1;
        		gbc2.gridy = 4;
                paramPanel.add(comboBoxOrder, gbc2);
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
    	int i;
        setForeground(Color.black);

        setTitle("Wavelet Fuse");

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
        
        JLabel labelSource = createLabel("Source image 1: ");
        paramPanel.add(labelSource, gbc2);
        
        gbc2.gridx = 1;
        String imageName = image.getImageName();
        JLabel labelImage = createLabel(imageName);
        paramPanel.add(labelImage, gbc2);
        
        gbc2.gridx = 0;
        gbc2.gridy = 1;
        gbc2.gridwidth = 1;
        JLabel labelFuse = createLabel("Source image 2: ");
        paramPanel.add(labelFuse, gbc2);
        
        gbc2.gridx = 1;
        comboBoxImage = new JComboBox<String>();
        buildComboBoxImage();
        comboBoxImage.setFont(serif12);
        comboBoxImage.setBackground(Color.white);
        comboBoxImage.setEnabled(true);
        paramPanel.add(comboBoxImage, gbc2);
        
        gbc2.gridx = 0;
        gbc2.gridy = 2;
        JLabel labelKernelSize = createLabel("Kernel size");
        paramPanel.add(labelKernelSize, gbc2);
        
        gbc2.gridx = 1;
        textKernelSize = createTextField("37");
        paramPanel.add(textKernelSize, gbc2);
        
        gbc2.gridx = 0;
        gbc2.gridy = 3;
        labelName = createLabel("Wavelet family name");
        paramPanel.add(labelName, gbc2);
        
        comboBoxName = buildWaveletNameComboBox();
        gbc2.gridx = 1;
        paramPanel.add(comboBoxName, gbc2);
        
        gbc2.gridx = 0;
        gbc2.gridy = 4;
   	    labelOrder = createLabel("Wavelet order");
        paramPanel.add(labelOrder, gbc2);
     
        comboBoxOrder = buildWaveletOrderComboBox("Daubechies");
        gbc2.gridx = 1;
        paramPanel.add(comboBoxOrder, gbc2);
        
        gbc2.gridx = 0;
        gbc2.gridy = 5;
        JLabel labelDepth = createLabel("Maximum depth");
        paramPanel.add(labelDepth, gbc2);
        
        gbc2.gridx = 1;
        textDepth = createTextField("12");
        paramPanel.add(textDepth, gbc2);
        
        fastGroup = new ButtonGroup();
        gbc2.gridx = 0;
        gbc2.gridy = 6;
        gbc2.gridwidth = 2;
        fastButton = new JRadioButton("Fast", true);
        fastButton.setFont(serif12);
        fastGroup.add(fastButton);
        paramPanel.add(fastButton, gbc2);
        
        gbc2.gridy = 7;
        slowButton = new JRadioButton("Slow", false);
        slowButton.setFont(serif12);
        fastGroup.add(slowButton);
        paramPanel.add(slowButton, gbc2);

        mainDialogPanel.add(mainPanel, BorderLayout.CENTER);
        mainDialogPanel.add(buildButtons(), BorderLayout.SOUTH);

        getContentPane().add(mainDialogPanel);

        pack();
        setResizable(true);
        // setVisible(true);

        System.gc();
    }
    
    private JComboBox<String> buildWaveletNameComboBox() {
    	final JComboBox<String> comboBox = new JComboBox<String>();
        comboBox.setFont(serif12);
        comboBox.setBackground(Color.white);
        comboBox.addItem("Biorthogonal");
        comboBox.addItem("Coiflets");
        comboBox.addItem("Daubechies");
        comboBox.addItem("Haar");
        comboBox.addItem("Reverse biorthogonal");
        comboBox.addItem("Symlets");
        comboBox.setSelectedIndex(5);
        comboBox.addActionListener(this);
        return comboBox;
    }
    
    private JComboBox<String> buildWaveletOrderComboBox(String family) {
    	int i;
    	final JComboBox<String> comboBox = new JComboBox<String>();
    	comboBox.setFont(serif12);
        comboBox.setBackground(Color.white);
    	comboBox.removeAllItems();
    	if ((family.equals("Biorthogonal")) || (family.equals("Reverse biorthogonal"))) {
    		comboBox.addItem("11");
    		comboBox.addItem("13");
    		comboBox.addItem("15");
    		comboBox.addItem("22");
    		comboBox.addItem("24");
    		comboBox.addItem("26");
    		comboBox.addItem("28");
    		comboBox.addItem("31");
    		comboBox.addItem("33");
    		comboBox.addItem("35");
    		comboBox.addItem("37");
    		comboBox.addItem("39");
    		comboBox.addItem("44");
    		comboBox.addItem("55");
    		comboBox.addItem("68");
    	}
    	else if (family.equals("Coiflets")) {
    	    for (i = 0; i < 17; i++) {
    	    	comboBox.addItem(String.valueOf(i+1));
    	    }
    	}
    	else if (family.equals("Daubechies")) {
    		for (i = 0; i < 38; i++) {
    	    	comboBox.addItem(String.valueOf(i+1));
    	    }
    		comboBox.setSelectedIndex(12);
    	}
    	else if (family.equals("Haar")) {
    		comboBox.addItem("1");
    	}
    	else if (family.equals("Symlets")) {
    	    for (i = 0; i < 19; i++) {
    	    	comboBox.addItem(String.valueOf(i+2));	
    	    }
    	    comboBox.setSelectedIndex(11);
    	}
    	return comboBox;
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
     * Use the GUI results to set up the variables needed to run the algorithm.
     *
     * @return  <code>true</code> if parameters set successfully, <code>false</code> otherwise.
     */
    private boolean setVariables() {
        String tmpStr;
        
    	if(comboBoxImage.getModel().getSize() == 0) {
            MipavUtil.displayError("No source image 2 was added to comboBoxImage.");
            return false;
        }
    	fuseImage = userInterface.getRegisteredImageByName((String) comboBoxImage.getSelectedItem());
        
        tmpStr = textKernelSize.getText();

        if (testParameter(tmpStr, 3, 201)) {
            kernel_size = Integer.valueOf(tmpStr).intValue();
        } else {
            MipavUtil.displayError("Kernel size must be between 3 and 201");
            textKernelSize.requestFocus();
            textKernelSize.selectAll();

            return false;
        }
        
        if ((kernel_size % 2) != 1) {
        	MipavUtil.displayError("Kernel size must be an odd number");
            textKernelSize.requestFocus();
            textKernelSize.selectAll();

            return false;	
        }
        
        String nameString = (String) comboBoxName.getSelectedItem();
        names = new PyWavelets.WAVELET_NAME[1];
        if (nameString.equals("Biorthogonal")) {
    		names[0] = PyWavelets.WAVELET_NAME.BIOR;
    	}
    	else if (nameString.equals("Coiflets")) {
    		names[0] = PyWavelets.WAVELET_NAME.COIF;
    	}
    	else if (nameString.equals("Daubechies")) {
    		names[0] = PyWavelets.WAVELET_NAME.DB;
    	}
    	else if (nameString.equals("Haar")) {
    		names[0] = PyWavelets.WAVELET_NAME.HAAR;
    	}
    	else if (nameString.equals("Reverse biorthogonal")) {
    		names[0] = PyWavelets.WAVELET_NAME.RBIO;
    	}
    	else if (nameString.equals("Symlets")) {
    		names[0] = PyWavelets.WAVELET_NAME.SYM;
    	}
        
        orders = new int[1];
        tmpStr = (String) comboBoxOrder.getSelectedItem();
        orders[0] = Integer.valueOf(tmpStr).intValue();
        
        tmpStr = textDepth.getText();
    	
        if (testParameter(tmpStr, 1, 999)) {
            max_depth = Integer.valueOf(tmpStr).intValue();
        } else {
            MipavUtil.displayError("max_depth must be between 1 and 999");
            textDepth.requestFocus();
            textDepth.selectAll();

            return false;
        }
        
        slow = slowButton.isSelected();
        
        return true;
    }
    
    /**
     * Once all the necessary variables are set, call the Nonlocal Means filter algorithm based on what type of image this is
     * and whether or not there is a separate destination image.
     */
    protected void callAlgorithm() {
        String name = makeImageName(image.getImageName(), "_waveletFuse");
        int[] destExtents;

        destExtents = new int[2];
        destExtents[0] = image.getExtents()[0]; // X dim
        destExtents[1] = image.getExtents()[1]; // Y dim     


        try {

            // Make result image of float type
            if (image.isColorImage() || fuseImage.isColorImage()) {
                resultImage = new ModelImage(ModelImage.ARGB_FLOAT, destExtents, name);
            } else {
                resultImage = new ModelImage(ModelImage.DOUBLE, destExtents, name);
            }

            // Make algorithm
            waveletFuseAlgo = new AlgorithmWaveletFuse(resultImage, image, fuseImage,
            		kernel_size, names, orders, max_depth, slow);

            // This is very important. Adding this object as a listener allows the algorithm to
            // notify this object when it has completed of failed. See algorithm performed event.
            // This is made possible by implementing AlgorithmedPerformed interface
            waveletFuseAlgo.addListener(this);
            createProgressBar(image.getImageName(), waveletFuseAlgo);

            // Hide dialog
            setVisible(false);

            if (isRunInSeparateThread()) {

                // Start the thread as a low priority because we wish to still have user interface work fast
                if (waveletFuseAlgo.startMethod(Thread.MIN_PRIORITY) == false) {
                    MipavUtil.displayError("A thread is already running on this object");
                }
            } else {
                waveletFuseAlgo.run();
            }
        } catch (OutOfMemoryError x) {
            MipavUtil.displayError("Dialog Wavelet Fuse: unable to allocate enough memory");

            if (resultImage != null) {
                resultImage.disposeLocal(); // Clean up memory of result image
                resultImage = null;
            }

            return;
        }
        
    }
    
    /**
     * This method is required if the AlgorithmPerformed interface is implemented. It is called by the algorithms when
     * it has completed or failed to to complete, so that the dialog can be display the result image and/or clean up.
     *
     * @param  algorithm  Algorithm that caused the event.
     */
    public void algorithmPerformed(AlgorithmBase algorithm) {

        if (algorithm instanceof AlgorithmWaveletFuse) {
            image.clearMask();

            if ((waveletFuseAlgo.isCompleted() == true) && (resultImage != null)) {

                updateFileInfo(image, resultImage);
                resultImage.clearMask();

                // The algorithm has completed and produced a new image to be displayed.
                try {
                    new ViewJFrameImage(resultImage, null, new Dimension(610, 200));
                } catch (OutOfMemoryError error) {
                    MipavUtil.displayError("Out of memory: unable to open new frame");
                }
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

        waveletFuseAlgo.finalize();
        waveletFuseAlgo = null;
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
     * Accessor to get the result image.
     *
     * @return  Result image.
     */
    public ModelImage getResultImage() {
        return resultImage;
    }
    
    protected void setGUIFromParams() {
        image = scriptParameters.retrieveInputImage();
        fuseImage = scriptParameters.retrieveInputImage(2);
        userInterface = ViewUserInterface.getReference();
        parentFrame = image.getParentFrame();
        kernel_size = scriptParameters.getParams().getInt("ks");
        max_depth = scriptParameters.getParams().getInt("depth");
        slow = scriptParameters.getParams().getBoolean("sl");
    }
    
    /**
     * {@inheritDoc}
     */
    protected void storeParamsFromGUI() throws ParserException {
        scriptParameters.storeInputImage(image);
        scriptParameters.storeInputImage(fuseImage);
        
        if (getResultImage() != null) {
            scriptParameters.storeImageInRecorder(getResultImage());
        }
        
        scriptParameters.getParams().put(ParameterFactory.newParameter("ks", kernel_size));
        scriptParameters.getParams().put(ParameterFactory.newParameter("depth", max_depth));
        scriptParameters.getParams().put(ParameterFactory.newParameter("sl", slow));
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
                return new String("Algorithms.Filters (wavelet)");
            }

            public String getDescription() {
                return new String("Applies a Wavelet Fuse.");
            }

            public String getDescriptionLong() {
                return new String("Applies a Wavelet Fuse.");
            }

            public String getShortLabel() {
                return new String("Wavelet Fuse");
            }

            public String getLabel() {
                return new String("Wavelet Fuse");
            }

            public String getName() {
                return new String("Wavelet Fuse");
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
            table.put(new ParameterExternalImage("fuse_image"));
            table.put(new ParameterInt("ks", 37));
            table.put(new ParameterInt("depth", 12));
            table.put(new ParameterBoolean("sl",false));
	    } catch (final ParserException e) {
	        // this shouldn't really happen since there isn't any real parsing going on...
	        e.printStackTrace();
	    }
	
	    return table;
    }

}