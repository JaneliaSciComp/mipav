package gov.nih.mipav.view.dialogs;


import gov.nih.mipav.model.algorithms.*;
import gov.nih.mipav.model.file.FileInfoDicom;
import gov.nih.mipav.model.file.FileUtility;
import gov.nih.mipav.model.scripting.*;
import gov.nih.mipav.model.scripting.parameters.*;
import gov.nih.mipav.model.structures.*;
import gov.nih.mipav.view.*;

import java.awt.*;
import java.awt.event.*;

import javax.swing.*;


/**
 * DOCUMENT ME!
 */
public class JDialogPbBoundaryDetection extends JDialogScriptableBase implements AlgorithmInterface, ActionDiscovery, ScriptableActionInterface {

    //~ Static fields/initializers -------------------------------------------------------------------------------------

    /** Use serialVersionUID for interoperability. */
    //private static final long serialVersionUID;

    
    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** DOCUMENT ME! */
    private ModelImage image; // source image

    /** DOCUMENT ME! */
    private ModelImage resultImage = null; // result image
    
    private static final int BGTG = 1;
    
    private static final int CGTG = 2;
    
    private static final int BG = 3;
	
	private static final int CG = 4;
	
	private static final int TG = 5;
	
	private static final int GM = 6;
	
	private static final int GM2 = 7;
	
	private static final int TWOMM = 8;
	
	private static final int TWOMM2 = 9;
	
	private static final int CANNY = 10;
	
	private int gradientType = BG;
	
	private static final int GRAY_PRESENTATION = 1;
	
	private static final int COLOR_PRESENTATION = 2;
	
	private int presentation = GRAY_PRESENTATION;
	
	private double lowRadius = 0.01;
	
	private double highRadius = 0.02;
	
	private int numOrientations = 8;
	
	private double sigma = 2.0;
	
	private JLabel labelOrientations;
	
	private JTextField textOrientations;

    /** DOCUMENT ME! */
    private AlgorithmPbBoundaryDetection pbAlgo;
    
    private ButtonGroup gradientGroup;
    
    private JRadioButton bgButton;
    
    private JRadioButton bgtgButton;
    
    private JRadioButton cgButton;
    
    private JRadioButton cgtgButton;
    
    private JRadioButton gmButton;
    
    private JRadioButton gm2Button;
    
    private JRadioButton tgButton;
    
    private JRadioButton twoMMButton;
    
    private JRadioButton twoMM2Button;
    
    private JRadioButton CannyButton;
    
    private String smooth = "savgol";
    
    private ButtonGroup smoothGroup;
    
    private JRadioButton savgolButton;
    
    private JRadioButton gaussianButton;
    
    private JRadioButton noneButton;
    
    private ButtonGroup sigmaGroup;
    
    private JRadioButton sigma1Button;
    
    private JRadioButton sigma2Button;
    
    private JRadioButton sigma4Button;
    
    private JRadioButton sigma8Button;
    
    private JRadioButton sigma16Button;
    
    private JPanel sigmaPanel;
    
    private JPanel CannyPanel;
    
    // Resolution for pb
 	private int nthresh = 100;
 	
 	private JLabel labelThresh;
 	
 	private JTextField textThresh;
 	
 	// Multiplier for lower hysteresis threshold, in [0, 1].
 	private double hmult = 1.0/3.0;
 	
 	private JLabel labelMult;
 	
 	private JTextField textMult;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Empty constructor needed for dynamic instantiation (used during scripting).
     */
    public JDialogPbBoundaryDetection() { }

    /**
     * Creates new dialog for entering parameters for Pb boundary detection.
     *
     * @param  theParentFrame  Parent frame
     * @param  im              Source image
     */
    public JDialogPbBoundaryDetection(Frame theParentFrame, ModelImage im) {
        super(theParentFrame, false);
        image = im;
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
        Object source = event.getSource();

        if (command.equals("OK")) {

            if (setVariables()) {
                callAlgorithm();
            }
        } else if (command.equals("Help")) {
            //MipavUtil.showHelp("");
        } else if (command.equals("Cancel")) {
            dispose();
        } else if ((source == bgButton) || (source == bgtgButton) || (source == cgButton) || (source == cgtgButton) ||
        		(source == gmButton) || (source == gm2Button) || (source == twoMMButton) || (source == twoMM2Button) ||
        		(source == tgButton) || (source == CannyButton)) {
        	if (gmButton.isSelected() || twoMMButton.isSelected()) {
        		sigma1Button.setEnabled(true);
        		sigma2Button.setEnabled(true);
        		sigma4Button.setEnabled(true);
        		sigma8Button.setEnabled(true);
        		sigma16Button.setEnabled(true);
        		sigma2Button.setSelected(true);
        		sigmaPanel.setEnabled(true);
        		labelOrientations.setEnabled(false);
        		textOrientations.setEnabled(false);
        		labelThresh.setEnabled(false);
        		textThresh.setEnabled(false);
        		labelMult.setEnabled(false);
        		textMult.setEnabled(false);
        		CannyPanel.setEnabled(false);
        	}
        	else if (gm2Button.isSelected()) {
        		sigma1Button.setEnabled(true);
        		sigma2Button.setEnabled(true);
        		sigma4Button.setEnabled(true);
        		sigma8Button.setEnabled(false);
        		sigma16Button.setEnabled(false);
        		sigma2Button.setSelected(true);
        		sigmaPanel.setEnabled(true);
        		labelOrientations.setEnabled(false);
        		textOrientations.setEnabled(false);
        		labelThresh.setEnabled(false);
        		textThresh.setEnabled(false);
        		labelMult.setEnabled(false);
        		textMult.setEnabled(false);
        		CannyPanel.setEnabled(false);
        	}
        	else if (twoMM2Button.isSelected()) {
        		sigma1Button.setEnabled(true);
        		sigma2Button.setEnabled(true);
        		sigma4Button.setEnabled(false);
        		sigma8Button.setEnabled(false);
        		sigma16Button.setEnabled(false);
        		sigma1Button.setSelected(true);
        		sigmaPanel.setEnabled(true);
        		labelOrientations.setEnabled(false);
        		textOrientations.setEnabled(false);	
        		labelThresh.setEnabled(false);
        		textThresh.setEnabled(false);
        		labelMult.setEnabled(false);
        		textMult.setEnabled(false);
        		CannyPanel.setEnabled(false);
        	}
        	else if (CannyButton.isSelected()) {
        		sigma1Button.setEnabled(true);
        		sigma2Button.setEnabled(true);
        		sigma4Button.setEnabled(true);
        		sigma8Button.setEnabled(true);
        		sigma16Button.setEnabled(true);
        		sigma2Button.setSelected(true);
        		sigmaPanel.setEnabled(true);
        		labelOrientations.setEnabled(false);
        		textOrientations.setEnabled(false);
        		labelThresh.setEnabled(true);
        		textThresh.setEnabled(true);
        		labelMult.setEnabled(true);
        		textMult.setEnabled(true);	
        		CannyPanel.setEnabled(true);
        	}
        	else {
        		sigma1Button.setEnabled(false);
        		sigma2Button.setEnabled(false);
        		sigma4Button.setEnabled(false);
        		sigma8Button.setEnabled(false);
        		sigma16Button.setEnabled(false);
        		sigmaPanel.setEnabled(false);
        		labelOrientations.setEnabled(true);
        		textOrientations.setEnabled(true);
        		labelThresh.setEnabled(false);
        		textThresh.setEnabled(false);
        		labelMult.setEnabled(false);
        		textMult.setEnabled(false);
        		CannyPanel.setEnabled(false);
        	}
        } else {
            super.actionPerformed(event);
        }
    }

    // ************************************************************************
    // ************************** Algorithm Events ****************************
    // ************************************************************************
    
    /**
     * This method is required if the AlgorithmPerformed interface is implemented. It is called by the algorithm when it
     * has completed or failed to to complete, so that the dialog can be display the result image and/or clean up.
     *
     * @param  algorithm  Algorithm that caused the event.
     */
    public void algorithmPerformed(AlgorithmBase algorithm) {

        if (algorithm instanceof AlgorithmPbBoundaryDetection) {
            System.err.println("Pb Boundary Detection Elapsed: " + algorithm.getElapsedTime());
            image.clearMask();

            if ((pbAlgo.isCompleted() == true) && (resultImage != null)) {
                // The algorithm has completed and produced a new image to be displayed.

                updateFileInfo(image, resultImage);
                resultImage.clearMask();

                try {

                    new ViewJFrameImage(resultImage, null, new Dimension(610, 200));
                } catch (OutOfMemoryError error) {
                    System.gc();
                    MipavUtil.displayError("Out of memory: unable to open new frame");
                }
            } else if (resultImage != null) {

                // algorithm failed but result image still has garbage
                resultImage.disposeLocal(); // clean up memory
                resultImage = null;
                System.gc();
            }
        }

        if (algorithm.isCompleted()) {
            insertScriptLine();
        }
        // save the completion status for later
        setComplete(algorithm.isCompleted());

        pbAlgo.finalize();
        pbAlgo = null;
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
     * Method to handle item events.
     *
     * @param  event  event that cause the method to fire
     */
    public void itemStateChanged(ItemEvent event) {
        // Object source = event.getSource();
        // float tempNum;

    }
    
    /**
     * 
     * @param sigma
     */
    public void setNumOrientations(int numOrientations) {
        this.numOrientations = numOrientations;
    }
    
    /**
     * Once all the necessary variables are set, call the rule based contrast enhancement algorithm based on what type of image this is and
     * whether or not there is a separate destination image.
     */
    protected void callAlgorithm() {
        String name = makeImageName(image.getImageName(), "_Detection");

        try {
        	
            resultImage     = new ModelImage(ModelStorageBase.DOUBLE, image.getExtents(), name);
            if ((resultImage.getFileInfo()[0]).getFileFormat() == FileUtility.DICOM) {
                ((FileInfoDicom) (resultImage.getFileInfo(0))).setSecondaryCaptureTags(resultImage.getNDims() > 2,
                		resultImage.getFileInfo()[0].getDataType());
            }

            // Make algorithm
            pbAlgo = new AlgorithmPbBoundaryDetection(resultImage, image, gradientType, presentation, lowRadius, highRadius,
            		numOrientations, smooth, sigma, nthresh, hmult);

            // This is very important. Adding this object as a listener allows the algorithm to
            // notify this object when it has completed or failed. See algorithm performed event.
            // This is made possible by implementing AlgorithmedPerformed interface
            pbAlgo.addListener(this);

            createProgressBar(image.getImageName(), pbAlgo);

            setVisible(false); // Hide dialog

            if (isRunInSeparateThread()) {

                // Start the thread as a low priority because we wish to still have user interface work fast.
                if (pbAlgo.startMethod(Thread.MIN_PRIORITY) == false) {
                    MipavUtil.displayError("A thread is already running on this object");
                }
            } else {

                pbAlgo.run();
            }
        } catch (OutOfMemoryError x) {
            MipavUtil.displayError("Dialog Pb boundary Detection: unable to allocate enough memory");

            if (resultImage != null) {
                resultImage.disposeLocal(); // Clean up memory of result image
                resultImage = null;
            }

            return;
        }
           
    }

    

    /**
     * {@inheritDoc}
     */
    protected void doPostAlgorithmActions() {

        AlgorithmParameters.storeImageInRunner(resultImage);
    }

    /**
     * {@inheritDoc}
     */
    protected void setGUIFromParams() {
        image = scriptParameters.retrieveInputImage();
        numOrientations = scriptParameters.getParams().getInt("num_orientations");
        gradientType = scriptParameters.getParams().getInt("grad_type");
        smooth = scriptParameters.getParams().getString("smooth_type");
        sigma = scriptParameters.getParams().getDouble("sig");
        nthresh = scriptParameters.getParams().getInt("thresh");
        hmult = scriptParameters.getParams().getDouble("h_mult");
    }

    /**
     * {@inheritDoc}
     */
    protected void storeParamsFromGUI() throws ParserException {
        scriptParameters.storeInputImage(image);
        scriptParameters.storeOutputImageParams(resultImage, true);
        scriptParameters.getParams().put(ParameterFactory.newParameter("num_orientations", numOrientations));
        scriptParameters.getParams().put(ParameterFactory.newParameter("grad_type", gradientType));
        scriptParameters.getParams().put(ParameterFactory.newParameter("smooth_type", smooth));
        scriptParameters.getParams().put(ParameterFactory.newParameter("sig", sigma));
        scriptParameters.getParams().put(ParameterFactory.newParameter("thresh", nthresh));
        scriptParameters.getParams().put(ParameterFactory.newParameter("h_mult", hmult));
    }

    /**
     * Sets up the GUI (panels, buttons, etc) and displays it on the screen.
     */
    private void init() {
        setTitle("Pb boundary Detection");

        GridBagConstraints gbc = new GridBagConstraints();
        gbc.weightx = 1;
        gbc.anchor = GridBagConstraints.WEST;
        gbc.fill = GridBagConstraints.HORIZONTAL;

        JPanel paramPanel = new JPanel(new GridBagLayout());
        paramPanel.setBorder(buildTitledBorder("Parameters"));
        
        labelOrientations = new JLabel("Number of orientations");
        labelOrientations.setForeground(Color.black);
        labelOrientations.setFont(serif12);
        gbc.gridx = 0;
        gbc.gridy = 0;
        paramPanel.add(labelOrientations, gbc);

        textOrientations = new JTextField(10);
        textOrientations.setText("8");
        textOrientations.setFont(serif12);
        gbc.gridx = 1;
        paramPanel.add(textOrientations, gbc);
        
        JPanel gradientPanel = new JPanel(new GridBagLayout());
        gradientPanel.setBorder(buildTitledBorder("Gradient type"));
        GridBagConstraints gbc2 = new GridBagConstraints();
        gbc2.weightx = 1;
        gbc2.anchor = GridBagConstraints.WEST;
        gbc2.fill = GridBagConstraints.HORIZONTAL;
        
        gradientGroup = new ButtonGroup();
        if (image.isColorImage()) {
            bgButton = new JRadioButton("Brightness gradient", false);
        }
        else {
        	bgButton = new JRadioButton("Brightness gradient", true);
        }
        bgButton.setFont(serif12);
        bgButton.setForeground(Color.black);
        bgButton.addActionListener(this);
        gradientGroup.add(bgButton);
        gbc2.gridx = 0;
        gbc2.gridy = 0;
        gradientPanel.add(bgButton, gbc2);
        
        bgtgButton = new JRadioButton("Brightness gradient texture gradient", false);
        bgtgButton.setFont(serif12);
        bgtgButton.setForeground(Color.black);
        bgtgButton.addActionListener(this);
        gradientGroup.add(bgtgButton);
        gbc2.gridy++;
        gbc2.gridx = 0;
        gradientPanel.add(bgtgButton, gbc2);
        
        CannyButton = new JRadioButton("Canny and gradient magnitude", false);
        CannyButton.setFont(serif12);
        CannyButton.setForeground(Color.black);
        CannyButton.addActionListener(this);
        gradientGroup.add(CannyButton);
        gbc2.gridy++;
        gbc2.gridx = 0;
        gradientPanel.add(CannyButton, gbc2);
        
        if (image.isColorImage()) {
            cgButton = new JRadioButton("Color gradient", true);
        }
        else {
        	cgButton = new JRadioButton("Color gradient", false);
        	cgButton.setEnabled(false);
        }
        cgButton.setFont(serif12);
        cgButton.setForeground(Color.black);
        cgButton.addActionListener(this);
        gradientGroup.add(cgButton);
        gbc2.gridy++;
        gbc2.gridx = 0;
        gradientPanel.add(cgButton, gbc2);
        
        cgtgButton = new JRadioButton("Color gradient texture gradient", false);
        if (!image.isColorImage()) {
        	cgtgButton.setEnabled(false);
        }
        cgtgButton.setFont(serif12);
        cgtgButton.setForeground(Color.black);
        cgtgButton.addActionListener(this);
        gradientGroup.add(cgtgButton);
        gbc2.gridy++;
        gbc2.gridx = 0;
        gradientPanel.add(cgtgButton, gbc2);
        
        gmButton = new JRadioButton("Gradient magnitude", false);
        gmButton.setFont(serif12);
        gmButton.setForeground(Color.black);
        gmButton.addActionListener(this);
        gradientGroup.add(gmButton);
        gbc2.gridy++;
        gbc2.gridx = 0;
        gradientPanel.add(gmButton, gbc2);
        
        gm2Button = new JRadioButton("Gradient magnitude at 2 scales", false);
        gm2Button.setFont(serif12);
        gm2Button.setForeground(Color.black);
        gm2Button.addActionListener(this);
        gradientGroup.add(gm2Button);
        gbc2.gridy++;
        gbc2.gridx = 0;
        gradientPanel.add(gm2Button, gbc2);
        
        twoMMButton = new JRadioButton("Second moment matrix", false);
        twoMMButton.setFont(serif12);
        twoMMButton.setForeground(Color.black);
        twoMMButton.addActionListener(this);
        gradientGroup.add(twoMMButton);
        gbc2.gridy++;
        gbc2.gridx = 0;
        gradientPanel.add(twoMMButton, gbc2);
        
        twoMM2Button = new JRadioButton("Second moment matrix at 2 scales", false);
        twoMM2Button.setFont(serif12);
        twoMM2Button.setForeground(Color.black);
        twoMM2Button.addActionListener(this);
        gradientGroup.add(twoMM2Button);
        gbc2.gridy++;
        gbc2.gridx = 0;
        gradientPanel.add(twoMM2Button, gbc2);
        
        tgButton = new JRadioButton("Texture gradient", false);
        tgButton.setFont(serif12);
        tgButton.setForeground(Color.black);
        tgButton.addActionListener(this);
        gradientGroup.add(tgButton);
        gbc2.gridy++;
        gbc2.gridx = 0;
        gradientPanel.add(tgButton, gbc2);
        
        gbc.gridx = 0;
        gbc.gridy++;
        paramPanel.add(gradientPanel, gbc);
        
        JPanel smoothPanel = new JPanel(new GridBagLayout());
        smoothPanel.setBorder(buildTitledBorder("Smooothing"));
        GridBagConstraints gbc3 = new GridBagConstraints();
        gbc3.weightx = 1;
        gbc3.anchor = GridBagConstraints.WEST;
        gbc3.fill = GridBagConstraints.HORIZONTAL;
        
        
        smoothGroup = new ButtonGroup();
        savgolButton = new JRadioButton("Savitsky-Golay smoothing", true);
        savgolButton.setFont(serif12);
        savgolButton.setForeground(Color.black);
        smoothGroup.add(savgolButton);
        gbc3.gridx = 0;
        gbc3.gridy = 0;
        smoothPanel.add(savgolButton, gbc3);
        
        gaussianButton = new JRadioButton("Gaussian smoothing", false);
        gaussianButton.setFont(serif12);
        gaussianButton.setForeground(Color.black);
        smoothGroup.add(gaussianButton);
        gbc3.gridy++;
        gbc3.gridx = 0;
        smoothPanel.add(gaussianButton, gbc3);
        
        noneButton = new JRadioButton("No smoothing", false);
        noneButton.setFont(serif12);
        noneButton.setForeground(Color.black);
        smoothGroup.add(noneButton);
        gbc3.gridy++;
        gbc3.gridx = 0;
        smoothPanel.add(noneButton, gbc3);

        gbc.gridx = 0;
        gbc.gridy++;
        paramPanel.add(smoothPanel, gbc);
        
        sigmaPanel = new JPanel(new GridBagLayout());
        sigmaPanel.setBorder(buildTitledBorder("Sigma"));
        sigmaPanel.setEnabled(false);
        GridBagConstraints gbc4 = new GridBagConstraints();
        gbc4.weightx = 1;
        gbc4.anchor = GridBagConstraints.WEST;
        gbc4.fill = GridBagConstraints.HORIZONTAL;
        
        sigmaGroup = new ButtonGroup();
        sigma1Button = new JRadioButton("sigma = 1.0", false);
        sigma1Button.setFont(serif12);
        sigma1Button.setForeground(Color.black);
        sigma1Button.setEnabled(false);
        sigmaGroup.add(sigma1Button);
        gbc4.gridx = 0;
        gbc4.gridy = 0;
        sigmaPanel.add(sigma1Button, gbc4);
        
        sigma2Button = new JRadioButton("sigma = 2.0", true);
        sigma2Button.setFont(serif12);
        sigma2Button.setForeground(Color.black);
        sigma2Button.setEnabled(false);
        sigmaGroup.add(sigma2Button);
        gbc4.gridx = 0;
        gbc4.gridy++;
        sigmaPanel.add(sigma2Button, gbc4);
        
        sigma4Button = new JRadioButton("sigma = 4.0", false);
        sigma4Button.setFont(serif12);
        sigma4Button.setForeground(Color.black);
        sigma4Button.setEnabled(false);
        sigmaGroup.add(sigma4Button);
        gbc4.gridx = 0;
        gbc4.gridy++;
        sigmaPanel.add(sigma4Button, gbc4);
        
        sigma8Button = new JRadioButton("sigma = 8.0", false);
        sigma8Button.setFont(serif12);
        sigma8Button.setForeground(Color.black);
        sigma8Button.setEnabled(false);
        sigmaGroup.add(sigma8Button);
        gbc4.gridx = 0;
        gbc4.gridy++;
        sigmaPanel.add(sigma8Button, gbc4);
        
        sigma16Button = new JRadioButton("sigma = 16.0", false);
        sigma16Button.setFont(serif12);
        sigma16Button.setForeground(Color.black);
        sigma16Button.setEnabled(false);
        sigmaGroup.add(sigma16Button);
        gbc4.gridx = 0;
        gbc4.gridy++;
        sigmaPanel.add(sigma16Button, gbc4);
        
        gbc.gridx = 0;
        gbc.gridy++;
        paramPanel.add(sigmaPanel, gbc);
        
        CannyPanel = new JPanel(new GridBagLayout());
        CannyPanel.setBorder(buildTitledBorder("Canny"));
        CannyPanel.setEnabled(false);
        GridBagConstraints gbc5 = new GridBagConstraints();
        gbc5.weightx = 1;
        gbc5.anchor = GridBagConstraints.WEST;
        gbc5.fill = GridBagConstraints.HORIZONTAL;
        
        labelThresh = new JLabel("Number of resolutions for pb");
        labelThresh.setForeground(Color.black);
        labelThresh.setFont(serif12);
        labelThresh.setEnabled(false);
        gbc5.gridx = 0;
        gbc5.gridy = 0;
        CannyPanel.add(labelThresh, gbc5);

        textThresh = new JTextField(10);
        textThresh.setText("100");
        textThresh.setFont(serif12);
        textThresh.setEnabled(false);
        gbc5.gridx = 1;
        CannyPanel.add(textThresh, gbc5);
        
        labelMult = new JLabel("Multiplier for lower hysteresis");
        labelMult.setForeground(Color.black);
        labelMult.setFont(serif12);
        labelMult.setEnabled(false);
        gbc5.gridx = 0;
        gbc5.gridy = 1;
        CannyPanel.add(labelMult, gbc5);

        textMult = new JTextField(10);
        textMult.setText("0.333333");
        textMult.setFont(serif12);
        textMult.setEnabled(false);
        gbc5.gridx = 1;
        CannyPanel.add(textMult, gbc5);
        
        gbc.gridx = 0;
        gbc.gridy++;
        paramPanel.add(CannyPanel, gbc);

        JPanel buttonPanel = new JPanel();
        buildOKButton();
        buttonPanel.add(OKButton);
        buildCancelButton();
        buttonPanel.add(cancelButton);
        buildHelpButton();
        buttonPanel.add(helpButton);

        getContentPane().add(paramPanel);
        getContentPane().add(buttonPanel, BorderLayout.SOUTH);

        pack();
        //setResizable(false);
        setVisible(true);
    }

    /**
     * Use the GUI results to set up the variables needed to run the algorithm.
     *
     * @return  <code>true</code> if parameters set successfully, <code>false</code> otherwise.
     */
    private boolean setVariables() {

        System.gc();

        String tmpStr;
        
        if (bgButton.isSelected()) {
        	gradientType = BG;
        }
        else if (bgtgButton.isSelected()) {
        	gradientType = BGTG;
        	if (image.isColorImage()) {
        		presentation = COLOR_PRESENTATION;
        	}
        }
        else if (cgButton.isSelected()) {
        	gradientType = CG;
        	lowRadius = 0.02;
        }
        else if (cgtgButton.isSelected()) {
        	gradientType = CGTG;
        }
        else if (tgButton.isSelected()){
        	gradientType = TG;
        	lowRadius = 0.02;
        }
        else if (gmButton.isSelected()) {
        	gradientType = GM;
        }
        else if (gm2Button.isSelected()) {
        	gradientType = GM2;
        }
        else if (twoMMButton.isSelected()) {
        	gradientType = TWOMM;
        }
        else if (twoMM2Button.isSelected()) {
        	gradientType = TWOMM2;
        }
        else if (CannyButton.isSelected()) {
        	gradientType = CANNY;
        }
        
        if ((gradientType != GM) && (gradientType != GM2) && (gradientType != TWOMM) && (gradientType != TWOMM2) &&
        		(gradientType != CANNY)) {
	        tmpStr = textOrientations.getText();
	
	        if (testParameter(tmpStr, 1.0, 100.0)) {
	            numOrientations = Integer.valueOf(tmpStr).intValue();
	        } else {
	            textOrientations.requestFocus();
	            textOrientations.selectAll();
	
	            return false;
	        }
        }
        
        if ((gradientType == GM) || (gradientType == GM2) || (gradientType == TWOMM) || (gradientType == TWOMM2) ||
        		(gradientType == CANNY)) {
             if (sigma1Button.isSelected()) {
            	 sigma = 1.0;
             }
             else if (sigma2Button.isSelected()) {
                 sigma = 2.0;
             }
             else if (sigma4Button.isSelected()) {
                 sigma = 4.0;
             }
             else if (sigma8Button.isSelected()) {
                 sigma = 8.0;
             }
             else if (sigma16Button.isSelected()) {
                 sigma = 16.0;
             }
        }
        
        if (savgolButton.isSelected()) {
        	smooth = "savgol";
        }
        else if (gaussianButton.isSelected()) {
        	smooth = "gaussian";
        }
        else {
        	smooth = "none";
        }
        
        if (gradientType == CANNY) {
        	tmpStr = textThresh.getText();
        	
	        if (testParameter(tmpStr, 1.0, 1000.0)) {
	            nthresh = Integer.valueOf(tmpStr).intValue();
	        } else {
	            textThresh.requestFocus();
	            textThresh.selectAll();
	
	            return false;
	        }
	        
	        tmpStr = textMult.getText();
	    	
	        if (testParameter(tmpStr, 0.0, 1.0)) {
	            hmult = Double.valueOf(tmpStr).doubleValue();
	        } else {
	            textMult.requestFocus();
	            textMult.selectAll();
	
	            return false;
	        }
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
                return new String("Algorithms.EdgeDetection (Pb Boundary)");
            }

            public String getDescription() {
                return new String("Applies Pb Boundary Detection.");
            }

            public String getDescriptionLong() {
                return new String("Applies Pb Boundary Detection.");
            }

            public String getShortLabel() {
                return new String("PbBoundaryDetection");
            }

            public String getLabel() {
                return new String("Pb Boundary Detection");
            }

            public String getName() {
                return new String("Pb Boundary Detection");
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
            table.put(new ParameterInt("num_orientations", 9));
            table.put(new ParameterInt("grad_type", BG));
            table.put(new ParameterString("smooth_type", "savgol"));
            table.put(new ParameterDouble("sig", sigma));
            table.put(new ParameterInt("thresh", nthresh));
            table.put(new ParameterDouble("h_mult", hmult));
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
