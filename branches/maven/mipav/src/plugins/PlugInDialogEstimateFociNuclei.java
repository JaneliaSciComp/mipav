import gov.nih.mipav.model.algorithms.*;

import gov.nih.mipav.model.file.FileInfoBase.Unit;
import gov.nih.mipav.model.scripting.ParserException;
import gov.nih.mipav.model.scripting.parameters.ParameterFactory;
import gov.nih.mipav.model.structures.*;

import gov.nih.mipav.view.*;
import gov.nih.mipav.view.dialogs.*;

import java.awt.*;
import java.awt.event.*;

import javax.swing.*;
/**
 * 
 */

/**
 * @author joshim2
 */
public class PlugInDialogEstimateFociNuclei extends JDialogScriptableBase implements AlgorithmInterface {

// ~ Instance fields ------------------------------------------------------------------------
	
	/** Source Image */
	private ModelImage image;
	
	/** Label: Number of desired classes */
	private JLabel labelNClasses;
	
	/** Textfield: Number of desired classes */
	private JTextField textNClasses;
	
	/** Label: Desired exponent value */
	private JLabel labelExpo;
	
	/** Textfield: Desired exponent value */
	private JTextField textExpo;
	
	/** Label: End tolerance */
	private JLabel labelEndTol;
	
	/** Textfield: End tolerance */
	private JTextField textEndTol;
	
	/** Label: Maximum number of iterations */
	private JLabel labelMaxIter;
	
	/** Textfield: Maximum number of iterations */
	private JTextField textMaxIter;
	
	/** Label: Signal threshold */
	private JLabel labelSignal;
	
	/** Textfield: Signal threshold */
	private JTextField textSignal;
	
	/** Label: Number of open */
	private JLabel labelNIterOpen;
	
	/** Textfield: Number of open */
	private JTextField textNIterOpen;
	
	/** Label: Number of open */
	private JLabel labelKernelOpen;
	
	/** ComboBox: Kernel Selection for Open */
	private JComboBox comboBoxKernelOpen;
	
	/** Label: Kernel size selection for Open */
	private JLabel labelKernelSizeOpen;
	
	/** Textfield: Kernel size selection for Open */
	private JTextField textKernelSizeOpen;
	
	/** Label: Number of close */
	private JLabel labelNIterClose;
	
	/** Textfield: Number of close */
	private JTextField textNIterClose;
	
	/** Label: Number of close */
	private JLabel labelKernelClose;
	
	/** ComboBox: Kernel Selection for Close */
	private JComboBox comboBoxKernelClose;
	
	/** Label: Kernel size selection for Close */
	private JLabel labelKernelSizeClose;
	
	/** Textfield: Kernel size selection for Close */
	private JTextField textKernelSizeClose;
	
	/** Label: ID Objects Max Size */
	private JLabel labelMax;
	
	/** Textfield: ID Objects Max Size */
	private JTextField textMax;
	
	/** Label: ID Objects Min Size */
	private JLabel labelMin;
	
	/** Textfield: ID Objects Min Size */
	private JTextField textMin;
	
	/** Segmentation: Number of desired classes */
    private int nClasses;
    
    /** Segmentation: Desired exponent value */
    private float expValue;
	
    /** Segmentation: End toelrance*/
    private float endTol;
    
    /** Segmentation: Maximum number of iterations */
    private int maxIter;
    
    /** Segmentation: Signal threshold */
    private float threshold;
    
    /** Particle Analysis: Number of open */
    private int itersOpen;
    
    /** Particle Analysis: Kernel Size (Open) */
    private float kernelSizeOpen;
    
    /** Particle Analysis: Kernel Type (Open) */
    private int kernelOpen;
    
    /** Particle Analysis: Number of close */
    private int itersClose;
    
    /** Particle Analysis: Kernel Size (Close) */
    private float kernelSizeClose;
    
    /** Particle Analysis: Kernel Type (Close) */
    private int kernelClose;
    
    /** ID Objects: Max size */
    private int maxSize;
    
    /** ID Objects: Min size */
    private int minSize;
    
    /** Algorithm instance */
    private PlugInAlgorithmEstimateFociNuclei algoFociNuclei;
    
    /** Result image to be displayed */
    private ModelImage resultImage;
    
    
//	~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Empty constructor needed for dynamic instantiation (used during scripting).
     */
    public PlugInDialogEstimateFociNuclei() { }

    /**
     * Creates a new PluginDialogEstimateFociNuclei object.
     *
     * @param  theParentFrame  parent frame
     * @param  im              source image
     */
    public PlugInDialogEstimateFociNuclei(Frame theParentFrame, ModelImage im) {
        super(theParentFrame, false);
        image = im;
        init();
    }
    
//  ~ Methods --------------------------------------------------------------------------------------------------------

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
        } else {
            super.actionPerformed(event);
        }
    }
    
    
    /**
     * This method is required if the AlgorithmPerformed interface is implemented. It is called by the algorithms when
     * it has completed or failed to complete, so that the dialog can display the result image and/or clean up.
     *
     * @param  algorithm  Algorithm that caused the event.
     */
    public void algorithmPerformed(AlgorithmBase algorithm) {
    	
    	if (algorithm instanceof PlugInAlgorithmEstimateFociNuclei) {
    		resultImage = algoFociNuclei.getResultImage();
    		
    		/* Display the result */

            try {
                new ViewJFrameImage(resultImage, null, new Dimension(610, 200));
            } catch (OutOfMemoryError error) {
                System.gc();
                MipavUtil.displayError("Out of memory: unable to open new frame");
            }
            
            insertScriptLine();
    	}
    	
    	algoFociNuclei.finalize();
    	algoFociNuclei = null;
    }
    
    /**
     * Once all the necessary variables are set, call the Estimate Foci/Nuclei algorithm.
     */
    protected void callAlgorithm() {
    	try {
            System.gc();

            // Make algorithm
            algoFociNuclei = new PlugInAlgorithmEstimateFociNuclei(image, nClasses, expValue, endTol, maxIter,
            									threshold, itersOpen, kernelSizeOpen, kernelOpen, itersClose,
            									kernelSizeClose, kernelClose, maxSize, minSize);

            // This is very important. Adding this object as a listener allows the algorithm to
            // notify this object when it has completed of failed. See algorithm performed event.
            // This is made possible by implementing AlgorithmedPerformed interface
            algoFociNuclei.addListener(this);


            createProgressBar(image.getImageName(), algoFociNuclei);

            // Hide dialog
            setVisible(false);

            if (isRunInSeparateThread()) {

                // Start the thread as a low priority because we wish to still have user interface work fast.
                if (algoFociNuclei.startMethod(Thread.MIN_PRIORITY) == false) {
                    MipavUtil.displayError("A thread is already running on this object");
                }
            } else {
                algoFociNuclei.run();
            }
        } catch (OutOfMemoryError x) {
            System.gc();
            MipavUtil.displayError("Dialog Plugin Estimate Foci Nuclei: unable to allocate enough memory");

            return;
        }
    }
    
    /**
     * Sets up the GUI (panels, buttons, etc) and displays it on the screen.
     */
    private void init() {
    	
    	setTitle("Estimate Foci/Nuclei");
    	
    	// Segmentation Dialog
    	labelNClasses = new JLabel("Number of desired classes");
        labelNClasses.setForeground(Color.black);
        labelNClasses.setFont(serif12);

        textNClasses = new JTextField(5);
        textNClasses.setText("2");
        textNClasses.setFont(serif12);
        
        labelExpo = new JLabel("Desired exponent value");
        labelExpo.setForeground(Color.black);
        labelExpo.setFont(serif12);

        textExpo = new JTextField(5);
        textExpo.setText("2");
        textExpo.setFont(serif12);
        
        labelEndTol = new JLabel("End tolerance.");
        labelEndTol.setForeground(Color.black);
        labelEndTol.setFont(serif12);

        textEndTol = new JTextField(5);
        textEndTol.setText("0.01");
        textEndTol.setFont(serif12);
        
        labelMaxIter = new JLabel("Maximum number of iterations");
        labelMaxIter.setForeground(Color.black);
        labelMaxIter.setFont(serif12);

        textMaxIter = new JTextField(5);
        textMaxIter.setText("200");
        textMaxIter.setFont(serif12);

        labelSignal = new JLabel("Signal threshold");
        labelSignal.setForeground(Color.black);
        labelSignal.setFont(serif12);

        textSignal = new JTextField(5);
        textSignal.setText("0.0");
        textSignal.setFont(serif12);
        
        JPanel segmentationPanel = new JPanel(new GridBagLayout());
        segmentationPanel.setForeground(Color.black);
        segmentationPanel.setBorder(buildTitledBorder("Segmentation"));

        GridBagConstraints gbc = new GridBagConstraints();
        gbc.gridwidth = 1;
        gbc.gridheight = 1;
        gbc.anchor = GridBagConstraints.WEST;
        gbc.insets = new Insets(5, 5, 5, 5);

        gbc.gridx = 0;
        gbc.gridy = 0;
        gbc.weightx = 0;
        gbc.fill = GridBagConstraints.NONE;
        segmentationPanel.add(labelNClasses, gbc);
        gbc.gridx = 1;
        gbc.gridy = 0;
        gbc.weightx = 1;
        gbc.fill = GridBagConstraints.HORIZONTAL;
        segmentationPanel.add(textNClasses, gbc);
        gbc.gridx = 0;
        gbc.gridy = 1;
        gbc.weightx = 0;
        gbc.fill = GridBagConstraints.NONE;
        segmentationPanel.add(labelExpo, gbc);
        gbc.gridx = 1;
        gbc.gridy = 1;
        gbc.weightx = 1;
        gbc.fill = GridBagConstraints.HORIZONTAL;
        segmentationPanel.add(textExpo, gbc);
        gbc.gridx = 0;
        gbc.gridy = 2;
        gbc.weightx = 0;
        gbc.fill = GridBagConstraints.NONE;
        segmentationPanel.add(labelEndTol, gbc);
        gbc.gridx = 1;
        gbc.gridy = 2;
        gbc.weightx = 1;
        gbc.fill = GridBagConstraints.HORIZONTAL;
        segmentationPanel.add(textEndTol, gbc);
        gbc.gridx = 0;
        gbc.gridy = 3;
        gbc.weightx = 0;
        gbc.fill = GridBagConstraints.NONE;
        segmentationPanel.add(labelMaxIter, gbc);
        gbc.gridx = 1;
        gbc.gridy = 3;
        gbc.weightx = 1;
        gbc.fill = GridBagConstraints.HORIZONTAL;
        segmentationPanel.add(textMaxIter, gbc);
        gbc.gridx = 0;
        gbc.gridy = 4;
        gbc.weightx = 0;
        gbc.fill = GridBagConstraints.NONE;
        segmentationPanel.add(labelSignal, gbc);
        gbc.gridx = 1;
        gbc.gridy = 4;
        gbc.weightx = 1;
        gbc.fill = GridBagConstraints.HORIZONTAL;
        segmentationPanel.add(textSignal, gbc);
        
        // Open Parameters Dialog
        labelNIterOpen = new JLabel("Number of open (1-20)");
        labelNIterOpen.setForeground(Color.black);
        labelNIterOpen.setFont(serif12);

        textNIterOpen = new JTextField(5);
        textNIterOpen.setText("3");
        textNIterOpen.setFont(serif12);

        labelKernelOpen = new JLabel("Kernel selection");
        labelKernelOpen.setForeground(Color.black);
        labelKernelOpen.setFont(serif12);

        buildComboBox();
        comboBoxKernelOpen.addItemListener(this);

        String unitString = null;

        unitString = Unit.getUnitFromLegacyNum(image.getFileInfo()[0].getUnitsOfMeasure(0)).toString();

        if (image.getNDims() == 2) {
            labelKernelSizeOpen = new JLabel("Circle diameter - " + unitString);
        } else {
            labelKernelSizeOpen = new JLabel("Sphere diameter - " + unitString);
        }

        String defKernelSize = java.lang.String.valueOf(3 * java.lang.Math.max(image.getFileInfo(0).getResolutions()[0], image.getFileInfo(0).getResolutions()[1]));
        labelKernelSizeOpen.setForeground(Color.black);
        labelKernelSizeOpen.setBounds(75, 120, 155, 25);
        labelKernelSizeOpen.setFont(serif12);
        labelKernelSizeOpen.setEnabled(false);
        
        textKernelSizeOpen = new JTextField(5);
        textKernelSizeOpen.setText(defKernelSize);
        textKernelSizeOpen.setFont(serif12);
        textKernelSizeOpen.setEnabled(false);

        JPanel maskPanelOpen = new JPanel(new GridBagLayout());
        maskPanelOpen.setForeground(Color.black);
        maskPanelOpen.setBorder(buildTitledBorder("Open Parameters"));

        GridBagConstraints gbc1 = new GridBagConstraints();
        gbc1.gridwidth = 1;
        gbc1.gridheight = 1;
        gbc1.anchor = GridBagConstraints.WEST;
        gbc1.insets = new Insets(5, 5, 5, 5);

        gbc1.gridx = 0;
        gbc1.gridy = 0;
        gbc1.weightx = 0;
        gbc1.fill = GridBagConstraints.NONE;
        maskPanelOpen.add(labelNIterOpen, gbc1);
        gbc1.gridx = 1;
        gbc1.gridy = 0;
        gbc1.weightx = 1;
        gbc1.fill = GridBagConstraints.HORIZONTAL;
        maskPanelOpen.add(textNIterOpen, gbc1);
        gbc1.gridx = 0;
        gbc1.gridy = 1;
        gbc1.weightx = 0;
        gbc1.fill = GridBagConstraints.NONE;
        maskPanelOpen.add(labelKernelOpen, gbc1);
        gbc1.gridx = 1;
        gbc1.gridy = 1;
        gbc1.weightx = 1;
        gbc1.fill = GridBagConstraints.HORIZONTAL;
        maskPanelOpen.add(comboBoxKernelOpen, gbc1);
        gbc1.gridx = 0;
        gbc1.gridy = 2;
        gbc1.weightx = 0;
        gbc1.fill = GridBagConstraints.NONE;
        maskPanelOpen.add(labelKernelSizeOpen, gbc1);
        gbc1.gridx = 1;
        gbc1.gridy = 2;
        gbc1.weightx = 1;
        gbc1.fill = GridBagConstraints.HORIZONTAL;
        maskPanelOpen.add(textKernelSizeOpen, gbc1);

        labelNIterClose = new JLabel("Number of close (1-20)");
        labelNIterClose.setForeground(Color.black);
        labelNIterClose.setFont(serif12);

        textNIterClose = new JTextField(5);
        textNIterClose.setText("4");
        textNIterClose.setFont(serif12);

        labelKernelClose = new JLabel("Kernel selection");
        labelKernelClose.setForeground(Color.black);
        labelKernelClose.setFont(serif12);

        comboBoxKernelClose = new JComboBox();
        comboBoxKernelClose.setFont(serif12);
        comboBoxKernelClose.setBackground(Color.white);

        if (image.getNDims() == 2) {
            comboBoxKernelClose.addItem("3x3 -  4 connected");
            comboBoxKernelClose.addItem("5x5 - 12 connected");
            comboBoxKernelClose.addItem("User sized circle.");
            comboBoxKernelClose.setSelectedIndex(2);
        } else if (image.getNDims() == 3) {
            comboBoxKernelClose.addItem("3x3x3 -  6 connected (2.5D: 4)");
            comboBoxKernelClose.addItem("5x5x5 - 24 connected (2.5D: 12)");
            comboBoxKernelClose.addItem("User sized sphere.");
        }

        comboBoxKernelClose.addItemListener(this);

        unitString = null;

        unitString = Unit.getUnitFromLegacyNum(image.getFileInfo()[0].getUnitsOfMeasure(0)).toString();

        if (image.getNDims() == 2) {
            labelKernelSizeClose = new JLabel("Circle diameter - " + unitString);
        } else {
            labelKernelSizeClose = new JLabel("Sphere diameter - " + unitString);
        }

        labelKernelSizeClose.setForeground(Color.black);
        labelKernelSizeClose.setBounds(75, 120, 155, 25);
        labelKernelSizeClose.setFont(serif12);
        labelKernelSizeClose.setEnabled(false);

        textKernelSizeClose = new JTextField(5);
        textKernelSizeClose.setText(defKernelSize);
        textKernelSizeClose.setFont(serif12);
        textKernelSizeClose.setEnabled(false);

        JPanel maskPanelClose = new JPanel(new GridBagLayout());
        maskPanelClose.setForeground(Color.black);
        maskPanelClose.setBorder(buildTitledBorder("Close Parameters"));

        GridBagConstraints gbc2 = new GridBagConstraints();

        gbc2.gridwidth = 1;
        gbc2.gridheight = 1;
        gbc2.anchor = GridBagConstraints.WEST;
        gbc2.insets = new Insets(5, 5, 5, 5);

        gbc2.gridx = 0;
        gbc2.gridy = 0;
        gbc2.weightx = 0;
        gbc2.fill = GridBagConstraints.NONE;
        maskPanelClose.add(labelNIterClose, gbc2);
        gbc2.gridx = 1;
        gbc2.gridy = 0;
        gbc2.weightx = 1;
        gbc2.fill = GridBagConstraints.HORIZONTAL;
        maskPanelClose.add(textNIterClose, gbc2);
        gbc2.gridx = 0;
        gbc2.gridy = 1;
        gbc2.weightx = 0;
        gbc2.fill = GridBagConstraints.NONE;
        maskPanelClose.add(labelKernelClose, gbc2);
        gbc2.gridx = 1;
        gbc2.gridy = 1;
        gbc2.weightx = 1;
        gbc2.fill = GridBagConstraints.HORIZONTAL;
        maskPanelClose.add(comboBoxKernelClose, gbc2);
        gbc2.gridx = 0;
        gbc2.gridy = 2;
        gbc2.weightx = 0;
        gbc2.fill = GridBagConstraints.NONE;
        maskPanelClose.add(labelKernelSizeClose, gbc2);
        gbc2.gridx = 1;
        gbc2.gridy = 2;
        gbc2.weightx = 1;
        gbc2.fill = GridBagConstraints.HORIZONTAL;
        maskPanelClose.add(textKernelSizeClose, gbc2);
        
        // ID Objects Dialog

        JPanel idObjectsPanel = new JPanel(new GridBagLayout());
        idObjectsPanel.setForeground(Color.black);
        idObjectsPanel.setBorder(buildTitledBorder("Delete particles outside range"));

        labelMax = new JLabel("Maximum size");
        labelMax.setForeground(Color.black);
        labelMax.setFont(serif12);

        textMax = new JTextField(5);
        textMax.setText("4000");
        textMax.setFont(serif12);

        labelMin = new JLabel("Minimum size");
        labelMin.setForeground(Color.black);
        labelMin.setFont(serif12);

        textMin = new JTextField(5);
        textMin.setText("1000");
        textMin.setFont(serif12);

        GridBagConstraints gbc3 = new GridBagConstraints();
        gbc3.gridwidth = 1;
        gbc3.gridheight = 1;
        gbc3.anchor = GridBagConstraints.WEST;
        gbc3.insets = new Insets(5, 5, 5, 5);

        gbc3.gridx = 0;
        gbc3.gridy = 0;
        gbc3.weightx = 0;
        gbc3.fill = GridBagConstraints.NONE;
        idObjectsPanel.add(labelMax, gbc3);
        gbc3.gridx = 1;
        gbc3.gridy = 0;
        gbc3.weightx = 1;
        gbc3.fill = GridBagConstraints.HORIZONTAL;
        idObjectsPanel.add(textMax, gbc3);
        gbc3.gridx = 0;
        gbc3.gridy = 1;
        gbc3.weightx = 0;
        gbc3.fill = GridBagConstraints.NONE;
        idObjectsPanel.add(labelMin, gbc3);
        gbc3.gridx = 1;
        gbc3.gridy = 1;
        gbc3.weightx = 1;
        gbc3.fill = GridBagConstraints.HORIZONTAL;
        idObjectsPanel.add(textMin, gbc3);
        
        // Build "OK" and "Cancel" buttons
        
        JPanel buttonPanel = new JPanel();

        buildOKButton();
        buttonPanel.add(OKButton);
        buildCancelButton();
        buttonPanel.add(cancelButton);
        
        JPanel mainPanel = new JPanel(new GridBagLayout());
        
        gbc.gridy = 0;
		mainPanel.add(segmentationPanel, gbc);
		
		gbc.gridy = 1;
		mainPanel.add(maskPanelOpen, gbc);
		
		gbc.gridy = 2;
		mainPanel.add(maskPanelClose, gbc);
				
		gbc.gridy = 3;
		mainPanel.add(idObjectsPanel, gbc);

		getContentPane().add(mainPanel);
        getContentPane().add(buttonPanel, BorderLayout.SOUTH);

		pack();
		
		textKernelSizeOpen.setEnabled(true);
        labelKernelSizeOpen.setEnabled(true);
        textKernelSizeClose.setEnabled(true);
        labelKernelSizeClose.setEnabled(true);
        
		setVisible(true);
		setResizable(false);
    }
    
    /**
     * Use the GUI results to set up the variables needed to run the algorithm.
     *
     * @return  <code>true</code> if parameters set successfully, <code>false</code> otherwise.
     */
    private boolean setVariables() {
    	String tmpStr;
    	
    	// Set variables for segmentation
    	tmpStr = textNClasses.getText();
    	if (testParameter(tmpStr, 1.0, 6.0)) {
            nClasses = Integer.valueOf(tmpStr).intValue();
        } else {
            textNClasses.requestFocus();
            textNClasses.selectAll();

            return false;
        }
    	
    	tmpStr = textExpo.getText();

        if (testParameter(tmpStr, 1.1, 5.0)) {
            expValue = Float.valueOf(tmpStr).floatValue();
        } else {
            textExpo.requestFocus();
            textExpo.selectAll();

            return false;
        }
        
        tmpStr = textEndTol.getText();

        if (testParameter(tmpStr, Float.MIN_VALUE, 1.0)) {
            endTol = Float.valueOf(tmpStr).floatValue();
        } else {
            textEndTol.requestFocus();
            textEndTol.selectAll();

            return false;
        }

        tmpStr = textMaxIter.getText();

        if (testParameter(tmpStr, 1.0, 10000.0)) {
            maxIter = Integer.valueOf(tmpStr).intValue();
        } else {
            textMaxIter.requestFocus();
            textMaxIter.selectAll();

            return false;
        }
        
        tmpStr = textSignal.getText();

        if (testParameter(tmpStr, -Float.MAX_VALUE, Float.MAX_VALUE)) {
            threshold = Float.valueOf(tmpStr).floatValue();
        } else {
            textSignal.requestFocus();
            textSignal.selectAll();

            return false;
        }
        
        // Set variables for Particle analysis
        
        tmpStr = textNIterOpen.getText();

        if (testParameter(tmpStr, 1, 20)) {
            itersOpen = Integer.valueOf(tmpStr).intValue();
        } else {
            textNIterOpen.requestFocus();
            textNIterOpen.selectAll();

            return false;
        }
        
        tmpStr = textKernelSizeOpen.getText();

        float max = (float) (image.getExtents()[0] * image.getFileInfo()[0].getResolutions()[0] / 5);
        
        if (textKernelSizeOpen.isEnabled() == true) {

            if (testParameter(tmpStr, 0, max)) {
                kernelSizeOpen = Float.valueOf(tmpStr).floatValue();
            } else {
                textKernelSizeOpen.requestFocus();
                textKernelSizeOpen.selectAll();

                return false;
            }
        }
        
        if (image.getNDims() == 2) {

            if (comboBoxKernelOpen.getSelectedIndex() == 0) {
                kernelOpen = AlgorithmMorphology2D.CONNECTED4;
            } else if (comboBoxKernelOpen.getSelectedIndex() == 1) {
                kernelOpen = AlgorithmMorphology2D.CONNECTED12;
            } else if (comboBoxKernelOpen.getSelectedIndex() == 2) {
                kernelOpen = AlgorithmMorphology2D.SIZED_CIRCLE;
            }
        } else if (image.getNDims() == 3) {

            if (comboBoxKernelOpen.getSelectedIndex() == 0) {
                kernelOpen = AlgorithmMorphology3D.CONNECTED6;
            } else if (comboBoxKernelOpen.getSelectedIndex() == 1) {
                kernelOpen = AlgorithmMorphology3D.CONNECTED24;
            } else if (comboBoxKernelOpen.getSelectedIndex() == 2) {
                kernelOpen = AlgorithmMorphology3D.SIZED_SPHERE;
            }
        }
        
        tmpStr = textNIterClose.getText();

        if (testParameter(tmpStr, 1, 20)) {
            itersClose = Integer.valueOf(tmpStr).intValue();
        } else {
            textNIterClose.requestFocus();
            textNIterClose.selectAll();

            return false;
        }
        
        tmpStr = textKernelSizeClose.getText();
        max = (float) (image.getExtents()[0] * image.getFileInfo()[0].getResolutions()[0] / 5);

        if (textKernelSizeClose.isEnabled() == true) {

            if (testParameter(tmpStr, 0, max)) {
                kernelSizeClose = Float.valueOf(tmpStr).floatValue();
            } else {
                textKernelSizeClose.requestFocus();
                textKernelSizeClose.selectAll();

                return false;
            }
        }

        if (image.getNDims() == 2) {

            if (comboBoxKernelClose.getSelectedIndex() == 0) {
                kernelClose = AlgorithmMorphology2D.CONNECTED4;
            } else if (comboBoxKernelClose.getSelectedIndex() == 1) {
                kernelClose = AlgorithmMorphology2D.CONNECTED12;
            } else if (comboBoxKernelClose.getSelectedIndex() == 2) {
                kernelClose = AlgorithmMorphology2D.SIZED_CIRCLE;
            }
        } else if (image.getNDims() == 3) {

            if (comboBoxKernelClose.getSelectedIndex() == 0) {
                kernelClose = AlgorithmMorphology3D.CONNECTED6;
            } else if (comboBoxKernelClose.getSelectedIndex() == 1) {
                kernelClose = AlgorithmMorphology3D.CONNECTED24;
            } else if (comboBoxKernelClose.getSelectedIndex() == 2) {
                kernelClose = AlgorithmMorphology3D.SIZED_SPHERE;
            }
        }
        
        tmpStr = textMax.getText();

        if (testParameter(tmpStr, 1, 1000000000)) {
            maxSize = Integer.valueOf(tmpStr).intValue();
        } else {
            textMax.requestFocus();
            textMax.selectAll();

            return false;
        }

        tmpStr = textMin.getText();

        if (testParameter(tmpStr, 1, 1000000000)) {
            minSize = Integer.valueOf(tmpStr).intValue();
        } else {
            textMin.requestFocus();
            textMin.selectAll();

            return false;
        }
        
        return true;
    	
    }
    
    /**
     * Builds kernel combo box.
     */
    private void buildComboBox() {

        comboBoxKernelOpen = new JComboBox();
        comboBoxKernelOpen.setFont(serif12);
        comboBoxKernelOpen.setBackground(Color.white);

        if (image.getNDims() == 2) {
            comboBoxKernelOpen.addItem("3x3 -  4 connected");
            comboBoxKernelOpen.addItem("5x5 - 12 connected");
            comboBoxKernelOpen.addItem("User sized circle.");
            comboBoxKernelOpen.setSelectedIndex(2);
        } else if (image.getNDims() == 3) {
            comboBoxKernelOpen.addItem("3x3x3 -  6 connected (2.5D: 4)");
            comboBoxKernelOpen.addItem("5x5x5 - 24 connected (2.5D: 12)");
            comboBoxKernelOpen.addItem("User sized sphere.");
        }
    }
    
    /**
     * Accessor that sets the number of classes.
     *
     * @param  classes  The number of classes.
     */
    public void setNClasses(int classes) {
        nClasses = classes;
    }
    
    /**
     * Accessor that sets the exponent value.
     *
     * @param  scale  Value to set desired exponent value.
     */
    public void setExponent(float scale) {
        expValue = scale;
    }
    
    /**
     * Accessor that sets the threshold.
     *
     * @param  scale  Value to set the threshold to.
     */
    public void setThreshold(float scale) {
        threshold = scale;
    }
    
    /**
     * Accessor that sets the end tol.
     *
     * @param  scale  Value to set end tol to.
     */
    public void setEndTol(float scale) {
        endTol = scale;
    }
    
    /**
     * Accessor that sets the max iterations.
     *
     * @param  max  The max iterations
     */
    public void setMaxIter(int max) {
        maxIter = max;
    }
    
    /**
     * Accessor that sets the kernel size.
     *
     * @param  s  the desired kernel size
     */
    public void setKernelSizeOpen(float s) {
        kernelSizeOpen = s;
    }

    /**
     * Accessor that sets the kernel type to use.
     *
     * @param  krnl  the kernel type to use (either AlgorithmMorphology2D.CONNECTED4, AlgorithmMorphology2D.CONNECTED12,
     *               AlgorithmMorphology2D.SIZED_CIRCLE, AlgorithmMorphology3D.CONNECTED6,
     *               AlgorithmMorphology3D.CONNECTED24, AlgorithmMorphology3D.SIZED_SPHERE (or the ones in
     *               AlgorithmMorphology25D))
     */
    public void setKernelTypeOpen(int krnl) {
        kernelOpen = krnl;
    }
    
    /**
     * Accessor that sets the kernel size.
     *
     * @param  s  the desired kernel size
     */
    public void setKernelSizeClose(float s) {
        kernelSizeClose = s;
    }

    /**
     * Accessor that sets the kernel type to use.
     *
     * @param  krnl  the kernel type to use (either AlgorithmMorphology2D.CONNECTED4, AlgorithmMorphology2D.CONNECTED12,
     *               AlgorithmMorphology2D.SIZED_CIRCLE, AlgorithmMorphology3D.CONNECTED6,
     *               AlgorithmMorphology3D.CONNECTED24, AlgorithmMorphology3D.SIZED_SPHERE (or the ones in
     *               AlgorithmMorphology25D))
     */
    public void setKernelTypeClose(int krnl) {
        kernelClose = krnl;
    }

    /**
     * Accessor that sets the number of dilations to perform.
     *
     * @param  n  The number of erosions to do.
     */
    public void setNumClose(int n) {
        itersClose = n;
    }

    /**
     * Accessor that sets the number of dilations to perform.
     *
     * @param  n  The number of erosions to do.
     */
    public void setNumOpens(int n) {
        itersOpen = n;
    }
    
    /**
     * Accessor that sets the minimum size of objects.
     *
     * @param  min  The minimum size of an object.
     */
    public void setMinSize(int n) {
        minSize = n;
    }
    
    /**
     * Accessor that sets the maximum size of objects.
     *
     * @param  min  The minimum size of an object.
     */
    public void setMaxSize(int n) {
        maxSize = n;
    }
    
    /**
     * {@inheritDoc}
     */
    protected void setGUIFromParams() {
        image = scriptParameters.retrieveInputImage();
        parentFrame = image.getParentFrame();
        setNClasses(scriptParameters.getParams().getInt("number_of_classes"));
        setExponent(scriptParameters.getParams().getFloat("exponent_q"));
        setThreshold(scriptParameters.getParams().getFloat("threshold"));
        setEndTol(scriptParameters.getParams().getFloat("end_tolerance"));
        setMaxIter(scriptParameters.getParams().getInt("max_iterations"));
        //setCentroids(scriptParameters.getParams().getList("centroids").getAsFloatArray());
        setKernelSizeOpen(scriptParameters.getParams().getFloat("kernel_size_open"));
        setKernelTypeOpen(scriptParameters.getParams().getInt("kernel_type_open"));
        setKernelSizeClose(scriptParameters.getParams().getFloat("kernel_size_close"));
        setKernelTypeClose(scriptParameters.getParams().getInt("kernel_type_close"));
        setNumOpens(scriptParameters.getParams().getInt("iters_open"));
        setNumClose(scriptParameters.getParams().getInt("iters_close"));
        setMinSize(scriptParameters.getParams().getInt("min_size"));
        setMaxSize(scriptParameters.getParams().getInt("max_size"));
   }
   
    /**
     * {@inheritDoc}
     */
    protected void storeParamsFromGUI() throws ParserException {
        scriptParameters.storeInputImage(image);
        scriptParameters.getParams().put(ParameterFactory.newParameter("number_of_classes", nClasses));
        scriptParameters.getParams().put(ParameterFactory.newParameter("exponent_q", expValue));
        scriptParameters.getParams().put(ParameterFactory.newParameter("threshold", threshold));
        scriptParameters.getParams().put(ParameterFactory.newParameter("end_tolerance", endTol));
        scriptParameters.getParams().put(ParameterFactory.newParameter("max_iterations", maxIter));
        scriptParameters.getParams().put(ParameterFactory.newParameter("kernel_size_open", kernelSizeOpen));
        scriptParameters.getParams().put(ParameterFactory.newParameter("kernel_type_open", kernelOpen));
        scriptParameters.getParams().put(ParameterFactory.newParameter("kernel_size_close", kernelSizeClose));
        scriptParameters.getParams().put(ParameterFactory.newParameter("kernel_type_close", kernelClose));
        scriptParameters.getParams().put(ParameterFactory.newParameter("iters_open", itersOpen));
        scriptParameters.getParams().put(ParameterFactory.newParameter("iters_close", itersClose));
        scriptParameters.getParams().put(ParameterFactory.newParameter("min_size", minSize));
        scriptParameters.getParams().put(ParameterFactory.newParameter("max_size", maxSize));
    }
    
	

}
