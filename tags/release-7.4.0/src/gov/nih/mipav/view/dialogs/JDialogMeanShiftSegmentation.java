package gov.nih.mipav.view.dialogs;

import gov.nih.mipav.model.algorithms.AlgorithmMeanShiftSegmentation;

import gov.nih.mipav.model.algorithms.AlgorithmBase;
import gov.nih.mipav.model.algorithms.AlgorithmInterface;
import gov.nih.mipav.model.scripting.ParserException;
import gov.nih.mipav.model.scripting.parameters.ParameterFactory;
import gov.nih.mipav.model.structures.ModelImage;
import gov.nih.mipav.model.structures.ModelStorageBase;
import gov.nih.mipav.view.MipavUtil;
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

import javax.swing.BorderFactory;
import javax.swing.ButtonGroup;
import javax.swing.JCheckBox;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JRadioButton;
import javax.swing.JTextField;

@SuppressWarnings("serial")
public class JDialogMeanShiftSegmentation extends JDialogScriptableBase implements AlgorithmInterface {
	private int displayLoc; // Flag indicating if a new image is to be generated
	
	private ModelImage image; // source image
	
	private ViewUserInterface userInterface;
	
	private AlgorithmMeanShiftSegmentation msAlgo;
	
	private ModelImage resultImage = null;
	
	private String[] titles;
	
	private AlgorithmMeanShiftSegmentation.kernelType spatialKernelType = AlgorithmMeanShiftSegmentation.kernelType.Uniform;
	
	private AlgorithmMeanShiftSegmentation.kernelType rangeKernelType = AlgorithmMeanShiftSegmentation.kernelType.Uniform;
	
    private float spatialBandwidth = 8.0f;
    
    private float rangeBandwidth = 7.0f;
    
    private int minRegion;
    
    private AlgorithmMeanShiftSegmentation.SpeedUpLevel speedUpLevel;

	private boolean measureTime;
	
	private double speedThreshold = 0.5; // the fraction of window radius used in new optimized filter 2.
	
	// WEIGHT MAP USED WHEN COMPUTING MEAN SHIFT ON A LATTICE
	private double  weightMap[]; // weight map that may be used to weight the kernel
	 					         // upon performing mean shift on a lattice

    private boolean	weightMapDefined = false; // used to indicate if a lattice weight map has been defined
    
    private ButtonGroup spatialGroup;
    
    private JRadioButton spatialUniformButton;
    
    private JRadioButton spatialGaussianButton;
    
    private ButtonGroup rangeGroup;
    
    private JRadioButton rangeUniformButton;
    
    private JRadioButton rangeGaussianButton;
    
    private JTextField spatialText;
    
    private JTextField rangeText;
    
    private JTextField minRegionText;
    
    private ButtonGroup speedUpGroup;
    
    private JRadioButton noSpeedUpButton;
    
    private JRadioButton mediumSpeedUpButton;
    
    private JRadioButton highSpeedUpButton;
    
    private JLabel speedThresholdLabel;
    
    private JTextField speedThresholdText;
    
    private JCheckBox measureTimeCheckBox;
    
    private boolean isSpatialUniform;
    
    private boolean isRangeUniform;
    
    private int speedInt;
    
    private JRadioButton newImage;
    
    private JRadioButton replaceImage;
    
    private boolean createFilteredImage = false;
    
    private ModelImage filteredImage = null;
    
    private JCheckBox filterCheckBox;
    
    private boolean createBoundariesImage = false;
    
    private ModelImage boundariesImage = null;
    
    private JCheckBox boundariesCheckBox;
	
	//~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Empty constructor needed for dynamic instantiation (used during scripting).
     */
    public JDialogMeanShiftSegmentation() { }

    // or if the source image is to be replaced

    /**
     * Creates new dialog for entering parameters for mean shift segmentation.
     *
     * @param  theParentFrame  Parent frame
     * @param  im              Source image
     */
    public JDialogMeanShiftSegmentation(Frame theParentFrame, ModelImage im) {
        super(theParentFrame, false);
        image = im;
        userInterface =  ViewUserInterface.getReference();
        init();
    }
    
    /**
     * Sets up the GUI (panels, buttons, etc) and displays it on the screen.
     */
    private void init() {
    	
        JPanel mainPanel = new JPanel(new GridBagLayout());
        GridBagConstraints gbc = new GridBagConstraints();
        gbc.gridwidth = 1;
        gbc.gridheight = 1;
        gbc.anchor = GridBagConstraints.WEST;
        gbc.weightx = 1;
        gbc.insets = new Insets(3, 3, 3, 3);
        gbc.fill = GridBagConstraints.HORIZONTAL;
        gbc.gridx = 0;
        gbc.gridy = 0;
        gbc.gridx = 0;
    	setForeground(Color.black);
        setTitle("Mean Shift Segmentation");
        
        spatialGroup = new ButtonGroup();
        spatialUniformButton = new JRadioButton("Spatial kernel uniform", true);
        spatialUniformButton.setFont(serif12);
        spatialUniformButton.setForeground(Color.black);
        spatialGroup.add(spatialUniformButton);
        gbc.gridx = 0;
        gbc.gridy++;
        mainPanel.add(spatialUniformButton, gbc);
        
        spatialGaussianButton = new JRadioButton("Spatial kernel gaussian", false);
        spatialGaussianButton.setFont(serif12);
        spatialGaussianButton.setForeground(Color.black);
        spatialGroup.add(spatialGaussianButton);
        gbc.gridx = 0;
        gbc.gridy++;
        mainPanel.add(spatialGaussianButton, gbc);
        
        rangeGroup = new ButtonGroup();
        rangeUniformButton = new JRadioButton("Range kernel uniform", true);
        rangeUniformButton.setFont(serif12);
        rangeUniformButton.setForeground(Color.black);
        rangeGroup.add(rangeUniformButton);
        gbc.gridx = 0;
        gbc.gridy++;
        mainPanel.add(rangeUniformButton, gbc);
        
        rangeGaussianButton = new JRadioButton("Range kernel gaussian", false);
        rangeGaussianButton.setFont(serif12);
        rangeGaussianButton.setForeground(Color.black);
        rangeGroup.add(rangeGaussianButton);
        gbc.gridx = 0;
        gbc.gridy++;
        mainPanel.add(rangeGaussianButton, gbc);
        
        JLabel spatialLabel = new JLabel("Spatial bandwidth");
        spatialLabel.setForeground(Color.black);
        spatialLabel.setFont(serif12);
        gbc.fill = GridBagConstraints.NONE;
        gbc.gridx = 0;
        gbc.gridy++;
        mainPanel.add(spatialLabel, gbc);
        
        spatialText = new JTextField("8.0");
        spatialText.setForeground(Color.black);
        spatialText.setFont(serif12);
        gbc.fill = GridBagConstraints.HORIZONTAL;
        gbc.gridx = 1;
        mainPanel.add(spatialText, gbc);
        
        JLabel rangeLabel = new JLabel("Range bandwidth");
        rangeLabel.setForeground(Color.black);
        rangeLabel.setFont(serif12);
        gbc.fill = GridBagConstraints.NONE;
        gbc.gridx = 0;
        gbc.gridy++;
        mainPanel.add(rangeLabel, gbc);
        
        rangeText = new JTextField("7.0");
        rangeText.setForeground(Color.black);
        rangeText.setFont(serif12);
        gbc.fill = GridBagConstraints.HORIZONTAL;
        gbc.gridx = 1;
        mainPanel.add(rangeText, gbc);
        
        JLabel minRegionLabel = new JLabel("Minimum region pixels");
        minRegionLabel.setForeground(Color.black);
        minRegionLabel.setFont(serif12);
        gbc.fill = GridBagConstraints.NONE;
        gbc.gridx = 0;
        gbc.gridy++;
        mainPanel.add(minRegionLabel, gbc);
        
        minRegionText = new JTextField("20");
        minRegionText.setForeground(Color.black);
        minRegionText.setFont(serif12);
        gbc.fill = GridBagConstraints.HORIZONTAL;
        gbc.gridx = 1;
        mainPanel.add(minRegionText, gbc);
        
        speedUpGroup = new ButtonGroup();
        noSpeedUpButton = new JRadioButton("No speedup", true);
        noSpeedUpButton.setFont(serif12);
        noSpeedUpButton.setForeground(Color.black);
        noSpeedUpButton.addActionListener(this);
        speedUpGroup.add(noSpeedUpButton);
        gbc.gridx = 0;
        gbc.gridy++;
        mainPanel.add(noSpeedUpButton, gbc);
        
        mediumSpeedUpButton = new JRadioButton("Medium speedup", false);
        mediumSpeedUpButton.setFont(serif12);
        mediumSpeedUpButton.setForeground(Color.black);
        mediumSpeedUpButton.addActionListener(this);
        speedUpGroup.add(mediumSpeedUpButton);
        gbc.gridx = 0;
        gbc.gridy++;
        mainPanel.add(mediumSpeedUpButton, gbc);
        
        highSpeedUpButton = new JRadioButton("High speedup", false);
        highSpeedUpButton.setFont(serif12);
        highSpeedUpButton.setForeground(Color.black);
        highSpeedUpButton.addActionListener(this);
        speedUpGroup.add(highSpeedUpButton);
        gbc.gridx = 0;
        gbc.gridy++;
        mainPanel.add(highSpeedUpButton, gbc);
        
        speedThresholdLabel = new JLabel("Fraction of window radius used");
        speedThresholdLabel.setForeground(Color.black);
        speedThresholdLabel.setFont(serif12);
        speedThresholdLabel.setEnabled(false);
        gbc.fill = GridBagConstraints.NONE;
        gbc.gridx = 0;
        gbc.gridy++;
        mainPanel.add(speedThresholdLabel, gbc);
        
        speedThresholdText = new JTextField("0.5");
        speedThresholdText.setForeground(Color.black);
        speedThresholdText.setFont(serif12);
        gbc.fill = GridBagConstraints.HORIZONTAL;
        gbc.gridx = 1;
        mainPanel.add(speedThresholdText, gbc);
        
        measureTimeCheckBox = new JCheckBox("Measure time", false);
        measureTimeCheckBox.setFont(serif12);
        measureTimeCheckBox.setForeground(Color.black);
        gbc.fill = GridBagConstraints.NONE;
        gbc.gridx = 0;
        gbc.gridy++;
        mainPanel.add(measureTimeCheckBox, gbc);
        
        filterCheckBox = new JCheckBox("Create filtered image", false);
        filterCheckBox.setFont(serif12);
        filterCheckBox.setForeground(Color.black);
        gbc.fill = GridBagConstraints.NONE;
        gbc.gridx = 0;
        gbc.gridy++;
        mainPanel.add(filterCheckBox, gbc);
        
        boundariesCheckBox = new JCheckBox("Create segmentation boundaries image", false);
        boundariesCheckBox.setFont(serif12);
        boundariesCheckBox.setForeground(Color.black);
        gbc.fill = GridBagConstraints.NONE;
        gbc.gridx = 0;
        gbc.gridy++;
        mainPanel.add(boundariesCheckBox, gbc);
        
        JPanel destinationPanel = new JPanel(new GridLayout(2, 1));
        destinationPanel.setForeground(Color.black);
        destinationPanel.setBorder(buildTitledBorder("Destination"));

        ButtonGroup destinationGroup = new ButtonGroup();
        newImage = new JRadioButton("New image", true);
        newImage.setFont(serif12);
        destinationGroup.add(newImage);
        destinationPanel.add(newImage);

        replaceImage = new JRadioButton("Replace image", false);
        replaceImage.setFont(serif12);
        destinationGroup.add(replaceImage);
        destinationPanel.add(replaceImage);

        if (image.getLockStatus() == ModelStorageBase.UNLOCKED) {
            replaceImage.setEnabled(true);
        } else {
            replaceImage.setEnabled(false);
        }
        gbc.gridy++;
        mainPanel.add(destinationPanel, gbc);
        mainPanel.setBorder(BorderFactory.createEmptyBorder(5, 5, 5, 5));

        getContentPane().add(mainPanel, BorderLayout.CENTER);
        getContentPane().add(buildButtons(), BorderLayout.SOUTH);

        pack();
        setSize(600,600);
        setVisible(true);
    }
    
    /**
     * actionPerformed - Closes dialog box when the OK button is pressed and calls the algorithm.
     *
     * @param  event  event that triggers function
     */
    public void actionPerformed(ActionEvent event) {
        String command = event.getActionCommand();
        Object source = event.getSource();

        if (command.equals("OK")) {

            if (setVariables()) {
                callAlgorithm();
            }
        } else if ((source == noSpeedUpButton) || (source == mediumSpeedUpButton) || (source == highSpeedUpButton)) {
        	if (highSpeedUpButton.isSelected()) {
        		speedThresholdLabel.setEnabled(true);
        		speedThresholdText.setEnabled(true);
        	}
        	else {
        		speedThresholdLabel.setEnabled(false);
        		speedThresholdText.setEnabled(false);	
        	}
        } else if (command.equals("Cancel")) {
            dispose();
        } else { // else if (source == thresholdCheckbox)
            super.actionPerformed(event);
        }
        
    }
    
    /**
     * Use the GUI results to set up the variables needed to run the algorithm.
     *
     * @return  <code>true</code> if parameters set successfully, <code>false</code> otherwise.
     */
    private boolean setVariables() {
        String tmpStr;
        
        if (spatialUniformButton.isSelected()) {
        	spatialKernelType = AlgorithmMeanShiftSegmentation.kernelType.Uniform;	
        }
        else {
        	spatialKernelType = AlgorithmMeanShiftSegmentation.kernelType.Gaussian;
        }
        
        if (rangeUniformButton.isSelected()) {
        	rangeKernelType = AlgorithmMeanShiftSegmentation.kernelType.Uniform;	
        }
        else {
        	rangeKernelType = AlgorithmMeanShiftSegmentation.kernelType.Gaussian;
        }
        
        tmpStr = spatialText.getText();
        try {
        	spatialBandwidth = Float.parseFloat(tmpStr);
        }
        catch (NumberFormatException e) {
			MipavUtil.displayError("The spatial bandwidth entry is not a valid float");
			spatialText.requestFocus();
			spatialText.selectAll();
			return false;
		}
        if (spatialBandwidth <= 0) {
        	MipavUtil.displayError("The spatial bandwidth must be greater than zero");
        	spatialText.requestFocus();
			spatialText.selectAll();
        	return false;
        }
        
        tmpStr = rangeText.getText();
        try {
        	rangeBandwidth = Float.parseFloat(tmpStr);
        }
        catch (NumberFormatException e) {
			MipavUtil.displayError("The range bandwidth entry is not a valid float");
			rangeText.requestFocus();
			rangeText.selectAll();
			return false;
		}
        if (rangeBandwidth <= 0) {
        	MipavUtil.displayError("The range bandwidth must be greater than zero");
        	rangeText.requestFocus();
			rangeText.selectAll();
        	return false;
        }
        
        tmpStr = minRegionText.getText();
        try {
        	minRegion = Integer.parseInt(tmpStr);
        }
        catch (NumberFormatException e) {
			MipavUtil.displayError("The minimum region pixels entry is not a valid integer");
			minRegionText.requestFocus();
			minRegionText.selectAll();
			return false;
		}
        if (minRegion <= 0) {
        	MipavUtil.displayError("The minimum region pixels must be greater than zero");
        	minRegionText.requestFocus();
			minRegionText.selectAll();
        	return false;
        }
        
        if (noSpeedUpButton.isSelected()) {
        	speedUpLevel = AlgorithmMeanShiftSegmentation.SpeedUpLevel.NO_SPEEDUP;
        }
        else if (mediumSpeedUpButton.isSelected()) {
        	speedUpLevel = AlgorithmMeanShiftSegmentation.SpeedUpLevel.MED_SPEEDUP;	
        }
        else {
        	speedUpLevel = AlgorithmMeanShiftSegmentation.SpeedUpLevel.HIGH_SPEEDUP;
        	tmpStr = speedThresholdText.getText();
        	try {
        	    speedThreshold = Double.parseDouble(tmpStr)	;
        	}
        	catch (NumberFormatException e) {
    			MipavUtil.displayError("The fraction of window radius used is not a valid double");
    			speedThresholdText.requestFocus();
    			speedThresholdText.selectAll();
    			return false;
    		}
        	if (speedThreshold <= 0.0) {
        		MipavUtil.displayError("The fraction of window radius must be greater than 0");
        		speedThresholdText.requestFocus();
    			speedThresholdText.selectAll();
        		return false;
        	}
        	if (speedThreshold > 1.0) {
        		MipavUtil.displayError("The fraction of window radius cannot exceed 1");
        		speedThresholdText.requestFocus();
    			speedThresholdText.selectAll();
        		return false;	
        	}
        }
        
        measureTime = measureTimeCheckBox.isSelected();
        weightMap = new double[image.getExtents()[0] * image.getExtents()[1]];
        createFilteredImage = filterCheckBox.isSelected();
        createBoundariesImage = boundariesCheckBox.isSelected();
        
        if (replaceImage.isSelected()) {
            displayLoc = REPLACE;
        } else if (newImage.isSelected()) {
            displayLoc = NEW;
        }
        return true;
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
        if (algorithm instanceof AlgorithmMeanShiftSegmentation) {
            image.clearMask();

            if ((msAlgo.isCompleted() == true) && (resultImage != null)) {
                // The algorithm has completed and produced a new image to be displayed.

                updateFileInfo(image, resultImage);

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
            
            if ((msAlgo.isCompleted() == true) && (filteredImage != null)) {
                // The algorithm has completed and produced a new image to be displayed.

                updateFileInfo(image, filteredImage);

                try {

                    new ViewJFrameImage(filteredImage, null, new Dimension(610, 220));
                } catch (OutOfMemoryError error) {
                    MipavUtil.displayError("Out of memory: unable to open new frame");
                }
            }
            
            if ((msAlgo.isCompleted() == true) && (boundariesImage != null)) {
                // The algorithm has completed and produced a new image to be displayed.

                updateFileInfo(image, boundariesImage);

                try {

                    new ViewJFrameImage(boundariesImage, null, new Dimension(610, 240));
                } catch (OutOfMemoryError error) {
                    MipavUtil.displayError("Out of memory: unable to open new frame");
                }
            }

        }
        // Update frame
        // ((ViewJFrameBase)parentFrame).updateImages(true);

        if (algorithm.isCompleted()) {
            insertScriptLine();
        }

        msAlgo.finalize();
        msAlgo = null;
        dispose();
    }
    
    /**
     * Once all the necessary variables are set, call the Mean Shift Segmentation algorithm based on what type of image
     * this is and whether or not there is a separate destination image.
     */
    protected void callAlgorithm() {
        String name = makeImageName(image.getImageName(), "_meanShiftSegmentation");
        
        if (createFilteredImage) {
        	String filteredName = makeImageName(image.getImageName(), "_filter");
        	if (image.isColorImage()) {
                filteredImage = new ModelImage(ModelStorageBase.ARGB, image.getExtents(), filteredName);
            }
            else {
            	filteredImage = new ModelImage(ModelStorageBase.UBYTE, image.getExtents(), filteredName);	
            }	
        }
        
        if (createBoundariesImage) {
        	String boundariesName = makeImageName(image.getImageName(), "_boundaries");
            boundariesImage = new ModelImage(ModelStorageBase.BYTE, image.getExtents(), boundariesName);		
        }

        if (displayLoc == NEW) {

            try {
                if (image.isColorImage()) {
                    resultImage = new ModelImage(ModelStorageBase.ARGB, image.getExtents(), name);
                }
                else {
                	resultImage = new ModelImage(ModelStorageBase.UBYTE, image.getExtents(), name);	
                }

                /*if ((resultImage.getFileInfo()[0]).getFileFormat() == FileUtility.DICOM){
                 *  ((FileInfoDicom)(resultImage.getFileInfo(0))).setSecondaryCaptureTags();
                 *}
                 */
                
                // Make algorithm
                msAlgo = new AlgorithmMeanShiftSegmentation(resultImage, image, spatialKernelType, rangeKernelType,
                		spatialBandwidth, rangeBandwidth, minRegion, speedUpLevel, measureTime, speedThreshold, 
                		weightMap, weightMapDefined, filteredImage, boundariesImage);

                // This is very important. Adding this object as a listener allows the algorithm to
                // notify this object when it has completed of failed. See algorithm performed event.
                // This is made possible by implementing AlgorithmedPerformed interface
                msAlgo.addListener(this);

                createProgressBar(image.getImageName(), msAlgo);
                
                // Hide dialog
                setVisible(false);

                if (isRunInSeparateThread()) {

                    // Start the thread as a low priority because we wish to still have user interface work fast.
                    if (msAlgo.startMethod(Thread.MIN_PRIORITY) == false) {
                        MipavUtil.displayError("A thread is already running on this object");
                    }
                } else {
                    msAlgo.run();
                }
            } catch (OutOfMemoryError x) {
                MipavUtil.displayError("Dialog Mean Shift Segmentation: unable to allocate enough memory");

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
                msAlgo = new AlgorithmMeanShiftSegmentation(null, image, spatialKernelType, rangeKernelType,
                		spatialBandwidth, rangeBandwidth, minRegion, speedUpLevel, measureTime, speedThreshold, 
                		weightMap, weightMapDefined, filteredImage, boundariesImage);

                // This is very important. Adding this object as a listener allows the algorithm to
                // notify this object when it has completed of failed. See algorithm performed event.
                // This is made possible by implementing AlgorithmedPerformed interface
                msAlgo.addListener(this);

                createProgressBar(image.getImageName(), msAlgo);
                
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
                    if (msAlgo.startMethod(Thread.MIN_PRIORITY) == false) {
                        MipavUtil.displayError("A thread is already running on this object");
                    }
                } else {
                    msAlgo.run();
                }
            } catch (OutOfMemoryError x) {
                MipavUtil.displayError("Dialog Mean Shift Segmentation: unable to allocate enough memory");

                return;
            }
        }

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
     * {@inheritDoc}
     */
    protected void setGUIFromParams() {
        image = scriptParameters.retrieveInputImage();
        userInterface = ViewUserInterface.getReference();
        parentFrame = image.getParentFrame();
        
        if (scriptParameters.doOutputNewImage()) {
            setDisplayLocNew();
        } else {
            setDisplayLocReplace();
        }
        isSpatialUniform = scriptParameters.getParams().getBoolean("is_spatial_uniform");
        if (isSpatialUniform) {
        	spatialKernelType = AlgorithmMeanShiftSegmentation.kernelType.Uniform;	
        }
        else {
        	spatialKernelType = AlgorithmMeanShiftSegmentation.kernelType.Gaussian;	
        }
        isRangeUniform = scriptParameters.getParams().getBoolean("is_range_uniform");
        if (isRangeUniform) {
        	rangeKernelType = AlgorithmMeanShiftSegmentation.kernelType.Uniform;	
        }
        else {
        	rangeKernelType = AlgorithmMeanShiftSegmentation.kernelType.Gaussian;	
        }
        spatialBandwidth = scriptParameters.getParams().getFloat("spatial_bandwidth");
        rangeBandwidth = scriptParameters.getParams().getFloat("range_bandwidth");
        minRegion = scriptParameters.getParams().getInt("min_region");
        speedInt = scriptParameters.getParams().getInt("speedInt");
        if (speedInt == 0) {
        	speedUpLevel = AlgorithmMeanShiftSegmentation.SpeedUpLevel.NO_SPEEDUP;	
        }
        else if (speedInt == 1) {
        	speedUpLevel = AlgorithmMeanShiftSegmentation.SpeedUpLevel.MED_SPEEDUP;
        }
        else {
        	speedUpLevel = AlgorithmMeanShiftSegmentation.SpeedUpLevel.HIGH_SPEEDUP;
        }
        speedThreshold = scriptParameters.getParams().getDouble("speed_threshold");
        measureTime = scriptParameters.getParams().getBoolean("measure_time");
        createFilteredImage = scriptParameters.getParams().getBoolean("create_filtered_image");
        createBoundariesImage = scriptParameters.getParams().getBoolean("create_boundaries_image");
    }
    
    /**
     * {@inheritDoc}
     */
    protected void storeParamsFromGUI() throws ParserException {
        scriptParameters.storeInputImage(image);
        scriptParameters.storeOutputImageParams(resultImage, (displayLoc == NEW));
        if (spatialKernelType == AlgorithmMeanShiftSegmentation.kernelType.Uniform) {
        	isSpatialUniform = true;
        }
        scriptParameters.getParams().put(ParameterFactory.newParameter("is_spatial_uniform", isSpatialUniform));
        if (rangeKernelType == AlgorithmMeanShiftSegmentation.kernelType.Uniform) {
        	isRangeUniform = true;
        }
        scriptParameters.getParams().put(ParameterFactory.newParameter("is_range_uniform", isRangeUniform));
        scriptParameters.getParams().put(ParameterFactory.newParameter("spatial_bandwidth", spatialBandwidth));
        scriptParameters.getParams().put(ParameterFactory.newParameter("range_bandwidth", rangeBandwidth));
        scriptParameters.getParams().put(ParameterFactory.newParameter("min_region", minRegion));
        if (speedUpLevel == AlgorithmMeanShiftSegmentation.SpeedUpLevel.NO_SPEEDUP) {
        	speedInt = 0;
        }
        else if (speedUpLevel == AlgorithmMeanShiftSegmentation.SpeedUpLevel.MED_SPEEDUP) {
        	speedInt = 1;
        }
        else {
        	speedInt = 2;
        }
        scriptParameters.getParams().put(ParameterFactory.newParameter("speed_int", speedInt));
        scriptParameters.getParams().put(ParameterFactory.newParameter("speed_threshold", speedThreshold));
        scriptParameters.getParams().put(ParameterFactory.newParameter("measure_time", measureTime));
        scriptParameters.getParams().put(ParameterFactory.newParameter("create_filtered_image", createFilteredImage));
        scriptParameters.getParams().put(ParameterFactory.newParameter("create_boundaries_image", createBoundariesImage));
    }

}