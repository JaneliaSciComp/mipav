package gov.nih.mipav.view.dialogs;


import gov.nih.mipav.model.algorithms.*;
import gov.nih.mipav.model.algorithms.filters.BM3D;
import gov.nih.mipav.model.scripting.*;
import gov.nih.mipav.model.scripting.parameters.*;
import gov.nih.mipav.model.structures.*;

import gov.nih.mipav.view.*;

import java.awt.*;
import java.awt.event.*;
import java.util.BitSet;

import javax.swing.*;


/**
 * Dialog to get user input, then call the algorithm.
 *
 * @see  BM3D
 */
public class JDialogBM3D extends JDialogScriptableBase implements AlgorithmInterface, ActionDiscovery, ScriptableActionInterface {
	
	//~ Static fields/initializers -------------------------------------------------------------------------------------

	    /** Use serialVersionUID for interoperability. */
	    //private static final long serialVersionUID;
	
	    private ModelImage image; // source image
		
		// destImage[0] is output of stage 1 with hard thresholding
		// destImage[1] is output of stage 2 with Wiener thresholding
		private ModelImage[] destImage;
		
		private BM3D bm3dAlgo;
		
		// White Gaussian noise standard deviation
		private double sigma; 
		
		// Hard thresholding search window size
		private int n_H = 16;
		
		// Hard thresholding maximum number of similar patches kept
		private int N_H = 16;
		
		// Hard thresholding  In order to speed up the processing, the loop over of the pixels of the
		// image is done with a step p (integer) in row and column.  For example if p = 3 the algorithm
		// is accelerated by a 9 factor.
		private int p_H = 3;
		
		private boolean useSD_H = false;
		
		// Threshold for 2D transform applied to each patch of the 3D group.  This threshold only
		// appears for sigma > 40 since tau_2D_H = 0 for sigma <= 40.  Applying a theshold to the
		// 2D transform coefficients for low values of sigma is useless since there is no 
		// improvement after the second step.  Moreover for a noise lower than 5 the results are
		// degraded.  tau_2D_H is a Bior1.5 transform whatever the value of sigma.  For the 
		// bi-orthogonal spline wavelet the vanishing moments of the  decomposing and reconstructing
		// wavelete functions are 1 and 5 repectively.
		private String tau_2D_H = "BIOR";
		
		// Coefficient hard thresholding level of the 3D group in the transform domain during the first
		// filtering sub-step.  The chosen value is 2.7.
		private double lambda3D_H = 2.7;
		
		// Wiener thresholding search window size
	    private int n_W = 16;
	 	
	    // Wiener thresholding maximum number of similar patches kept
	 	private int N_W = 32;
	 	
	    // Wiener thresholding  In order to speed up the processing, the loop over of the pixels of the
	 	// image is done with a step p (integer) in row and column.  For example if p = 3 the algorithm
	 	// is accelerated by a 9 factor.
	 	private int p_W = 3;
	 	
	 	private boolean useSD_W = true;
	 	
	    // Threshold for 2D transform applied to each patch of the 3D group.
	 	// A 2D DCT transform is used.
	 	private String tau_2D_W = "DCT";
	 	
	 	private JPanel filterPanel;
	 	private JLabel labelSigma;
	 	private JTextField textSigma;
	 	private JLabel labelHardSearchSize;
	 	private JTextField textHardSearchSize;
	 	private JLabel labelHardSimilar;
	 	private JTextField textHardSimilar;
	 	private JLabel labelHardStep;
	 	private JTextField textHardStep;
	 	private JCheckBox checkBoxHardSDWeighting;
	 	private JLabel labelHardTransform;
	 	private ButtonGroup hardTransformGroup;
	 	private JRadioButton hardBIORButton;
	 	private JRadioButton hardDCTButton;
	 	private JLabel labelHardCoefficient;
	 	private JTextField textHardCoefficient;
	 	private JLabel labelWienerSearchSize;
	 	private JTextField textWienerSearchSize;
	 	private JLabel labelWienerSimilar;
	 	private JTextField textWienerSimilar;
	 	private JLabel labelWienerStep;
	 	private JTextField textWienerStep;
	 	private JCheckBox checkBoxWienerSDWeighting;
	 	private JLabel labelWienerTransform;
	 	private ButtonGroup WienerTransformGroup;
	 	private JRadioButton WienerBIORButton;
	 	private JRadioButton WienerDCTButton;
	 	
	 	//~ Constructors ---------------------------------------------------------------------------------------------------

	    /**
	     * Empty constructor needed for dynamic instantiation (used during scripting).
	     */
	    public JDialogBM3D() { }

	    // false = apply algorithm only to VOI regions

	    /**
	     * Creates a new JDialogBM3D object.
	     *
	     * @param  theParentFrame  parent frame
	     * @param  im              source image
	     */
	    public JDialogBM3D(Frame theParentFrame, ModelImage im) {
	        super(theParentFrame, false);
	        image = im;
	        init();
	    }
	    
	  //~ Methods --------------------------------------------------------------------------------------------------------

	    /**
	     * Closes dialog box when the OK button is pressed and calls the algorithm.
	     *
	     * @param  event  Event that triggers function.
	     */
	    public void actionPerformed(ActionEvent event) {
	        String command = event.getActionCommand();
	        Object source = event.getSource();

	        if (command.equals("OK")) {

	            if (setVariables()) {
	                callAlgorithm();
	            }
	        } else if (command.equals("Cancel")) {
	            dispose();
	        } else if (source == helpButton) {
	            //MipavUtil.showHelp("");
	        } else {
	            super.actionPerformed(event);
	        }
	    }
	    
	    /**
	     * Sets up the GUI (panels, buttons, etc) and displays it on the screen.
	     */
	    private void init() {
	        setTitle("BM3D");

	        GridBagConstraints gbc = new GridBagConstraints();
	        gbc.gridwidth = 3;
	        gbc.anchor = GridBagConstraints.WEST;
	        gbc.weightx = 1;
	        
	        filterPanel = new JPanel(new GridBagLayout());
	        filterPanel.setBorder(buildTitledBorder("BM3D filter specifications"));

	        gbc.gridx = 0;
	        gbc.gridy = 0;
	        gbc.gridwidth = 1;
	        gbc.weightx = .5;
	        gbc.fill = GridBagConstraints.HORIZONTAL;
	        labelSigma = new JLabel("White Gaussian noise standard deviation ");
	        labelSigma.setForeground(Color.black);
	        labelSigma.setFont(serif12);
	        labelSigma.setEnabled(true);
	        filterPanel.add(labelSigma, gbc);

	        gbc.gridx = 1;
	        textSigma = new JTextField(10);
	        textSigma.setText("20.0");
	        textSigma.setFont(serif12);
	        textSigma.setEnabled(true);
	        filterPanel.add(textSigma, gbc);
	        
	        gbc.gridx = 0;
	        gbc.gridy++;
	        labelHardSearchSize = new JLabel("Hard thresholding search window size");
	        labelHardSearchSize.setForeground(Color.black);
	        labelHardSearchSize.setFont(serif12);
	        labelHardSearchSize.setEnabled(true);
	        filterPanel.add(labelHardSearchSize, gbc);
	        
	        gbc.gridx = 1;
	        textHardSearchSize = new JTextField(10);
	        textHardSearchSize.setText("16");
	        textHardSearchSize.setFont(serif12);
	        textHardSearchSize.setEnabled(true);
	        filterPanel.add(textHardSearchSize, gbc);
	        
	        gbc.gridx = 0;
	        gbc.gridy++;
	        labelHardSimilar = new JLabel("Hard thresholding maximum similar patches");
	        labelHardSimilar.setForeground(Color.black);
	        labelHardSimilar.setFont(serif12);
	        labelHardSimilar.setEnabled(true);
	        filterPanel.add(labelHardSimilar, gbc);
	        
	        gbc.gridx = 1;
	        textHardSimilar = new JTextField(10);
	        textHardSimilar.setText("16");
	        textHardSimilar.setFont(serif12);
	        textHardSimilar.setEnabled(true);
	        filterPanel.add(textHardSimilar, gbc);
	        
	        gbc.gridx = 0;
	        gbc.gridy++;
	        labelHardStep = new JLabel("Hard thresholding loop over step");
	        labelHardStep.setForeground(Color.black);
	        labelHardStep.setFont(serif12);
	        labelHardStep.setEnabled(true);
	        filterPanel.add(labelHardStep, gbc);
	        
	        gbc.gridx = 1;
	        textHardStep = new JTextField(10);
	        textHardStep.setText("3");
	        textHardStep.setFont(serif12);
	        textHardStep.setEnabled(true);
	        filterPanel.add(textHardStep, gbc);
	        
	        gbc.gridx = 0;
	        gbc.gridy++;
	        gbc.gridwidth = 2;
	        checkBoxHardSDWeighting = new JCheckBox("Use hard thresholding standard deviation weighting");
	        checkBoxHardSDWeighting.setFont(serif12);
	        checkBoxHardSDWeighting.setSelected(false);
	        checkBoxHardSDWeighting.setEnabled(true);
	        filterPanel.add(checkBoxHardSDWeighting, gbc);
	        
	        gbc.gridx = 0;
	        gbc.gridy++;
	        labelHardTransform = new JLabel("Hard thresholding 2D transform type:");
	        labelHardTransform.setForeground(Color.black);
	        labelHardTransform.setFont(serif12);
	        labelHardTransform.setEnabled(true);
	        filterPanel.add(labelHardTransform, gbc);
	        
	        hardTransformGroup = new ButtonGroup();
	        gbc.gridx = 0;
	        gbc.gridy++;
	        hardBIORButton = new JRadioButton("Biorthogonal spline wavelets", true);
	        hardBIORButton.setFont(serif12);
	        hardTransformGroup.add(hardBIORButton);
	        filterPanel.add(hardBIORButton, gbc);
	        
	        gbc.gridx = 0;
	        gbc.gridy++;
	        hardDCTButton = new JRadioButton("Discrete cosine transform", false);
	        hardDCTButton.setFont(serif12);
	        hardTransformGroup.add(hardDCTButton);
	        filterPanel.add(hardDCTButton, gbc);
	        
	        gbc.gridx = 0;
	        gbc.gridy++;
	        gbc.gridwidth = 1;
	        labelHardCoefficient = new JLabel("Coefficient hard thresholding level");
	        labelHardCoefficient.setForeground(Color.black);
	        labelHardCoefficient.setFont(serif12);
	        labelHardCoefficient.setEnabled(true);
	        filterPanel.add(labelHardCoefficient, gbc);
	        
	        gbc.gridx = 1;
	        textHardCoefficient = new JTextField(10);
	        textHardCoefficient.setText("2.7");
	        textHardCoefficient.setFont(serif12);
	        textHardCoefficient.setEnabled(true);
	        filterPanel.add(textHardCoefficient, gbc);
	        
	        gbc.gridx = 0;
	        gbc.gridy++;
	        labelWienerSearchSize = new JLabel("Wiener thresholding search window size");
	        labelWienerSearchSize.setForeground(Color.black);
	        labelWienerSearchSize.setFont(serif12);
	        labelWienerSearchSize.setEnabled(true);
	        filterPanel.add(labelWienerSearchSize, gbc);
	        
	        gbc.gridx = 1;
	        textWienerSearchSize = new JTextField(10);
	        textWienerSearchSize.setText("16");
	        textWienerSearchSize.setFont(serif12);
	        textWienerSearchSize.setEnabled(true);
	        filterPanel.add(textWienerSearchSize, gbc);
	        
	        gbc.gridx = 0;
	        gbc.gridy++;
	        labelWienerSimilar = new JLabel("Wiener thresholding maximum similar patches");
	        labelWienerSimilar.setForeground(Color.black);
	        labelWienerSimilar.setFont(serif12);
	        labelWienerSimilar.setEnabled(true);
	        filterPanel.add(labelWienerSimilar, gbc);
	        
	        gbc.gridx = 1;
	        textWienerSimilar = new JTextField(10);
	        textWienerSimilar.setText("32");
	        textWienerSimilar.setFont(serif12);
	        textWienerSimilar.setEnabled(true);
	        filterPanel.add(textWienerSimilar, gbc);
	        
	        gbc.gridx = 0;
	        gbc.gridy++;
	        labelWienerStep = new JLabel("Wiener thresholding loop over step");
	        labelWienerStep.setForeground(Color.black);
	        labelWienerStep.setFont(serif12);
	        labelWienerStep.setEnabled(true);
	        filterPanel.add(labelWienerStep, gbc);
	        
	        gbc.gridx = 1;
	        textWienerStep = new JTextField(10);
	        textWienerStep.setText("3");
	        textWienerStep.setFont(serif12);
	        textWienerStep.setEnabled(true);
	        filterPanel.add(textWienerStep, gbc);
	        
	        gbc.gridx = 0;
	        gbc.gridy++;
	        gbc.gridwidth = 2;
	        checkBoxWienerSDWeighting = new JCheckBox("Use Wiener thresholding standard deviation weighting");
	        checkBoxWienerSDWeighting.setFont(serif12);
	        checkBoxWienerSDWeighting.setSelected(true);
	        checkBoxWienerSDWeighting.setEnabled(true);
	        filterPanel.add(checkBoxWienerSDWeighting, gbc);
	        
	        gbc.gridx = 0;
	        gbc.gridy++;
	        labelWienerTransform = new JLabel("Wiener thresholding 2D transform type:");
	        labelWienerTransform.setForeground(Color.black);
	        labelWienerTransform.setFont(serif12);
	        labelWienerTransform.setEnabled(true);
	        filterPanel.add(labelWienerTransform, gbc);
	        
	        WienerTransformGroup = new ButtonGroup();
	        gbc.gridx = 0;
	        gbc.gridy++;
	        WienerBIORButton = new JRadioButton("Biorthogonal spline wavelets", false);
	        WienerBIORButton.setFont(serif12);
	        WienerTransformGroup.add(WienerBIORButton);
	        filterPanel.add(WienerBIORButton, gbc);
	        
	        gbc.gridx = 0;
	        gbc.gridy++;
	        WienerDCTButton = new JRadioButton("Discrete cosine transform", true);
	        WienerDCTButton.setFont(serif12);
	        WienerTransformGroup.add(WienerDCTButton);
	        filterPanel.add(WienerDCTButton, gbc);
	        
	        getContentPane().add(filterPanel);
	        getContentPane().add(buildButtons(), BorderLayout.SOUTH);
	        pack();
	        setVisible(true);
	    }
	    
	    /**
	     * Use the GUI results to set up the variables needed to run the algorithm.
	     * 
	     * @return <code>true</code> if parameters set successfully, <code>false</code> otherwise.
	     */
	    private boolean setVariables() {
	        String tmpStr;
	        
	        tmpStr = textSigma.getText();

	        if (testParameter(tmpStr, 0.1, 200.0)) {
	            sigma = Double.valueOf(tmpStr).doubleValue();
	        } else {
	            MipavUtil.displayError("White Gaussian noise standard deviation must be between 0.1 and 200.0");
	            textSigma.requestFocus();
	            textSigma.selectAll();

	            return false;
	        }
	        
	        tmpStr = textHardSearchSize.getText();

	        if (testParameter(tmpStr, 2, 256)) {
	            n_H = Integer.valueOf(tmpStr).intValue();
	        } else {
	            MipavUtil.displayError("Hard thresholding search window size must be between 2 and 256");
	            textHardSearchSize.requestFocus();
	            textHardSearchSize.selectAll();

	            return false;
	        }
	        
	        tmpStr = textHardSimilar.getText();

	        if (testParameter(tmpStr, 2, 256)) {
	            N_H = Integer.valueOf(tmpStr).intValue();
	        } else {
	            MipavUtil.displayError("Hard thresholding maximum number of similar patches kept must be between 2 and 256");
	            textHardSimilar.requestFocus();
	            textHardSimilar.selectAll();

	            return false;
	        }
	        
	        tmpStr = textHardStep.getText();

	        if (testParameter(tmpStr, 1, 10)) {
	            p_H = Integer.valueOf(tmpStr).intValue();
	        } else {
	            MipavUtil.displayError("Hard thresholding loop over step must be between 1 and 10");
	            textHardStep.requestFocus();
	            textHardStep.selectAll();

	            return false;
	        }
	        
	        useSD_H = checkBoxHardSDWeighting.isSelected();
	        
	        if (hardBIORButton.isSelected()) {
	        	tau_2D_H = "BIOR";
	        }
	        else {
	        	tau_2D_H = "DCT";
	        }
	        
	        tmpStr = textHardCoefficient.getText();

	        if (testParameter(tmpStr, 0.1, 200.0)) {
	            lambda3D_H = Double.valueOf(tmpStr).doubleValue();
	        } else {
	            MipavUtil.displayError("Coefficient hard thresholding level must be between 0.1 and 200.0");
	            textHardCoefficient.requestFocus();
	            textHardCoefficient.selectAll();

	            return false;
	        }
	        
	        tmpStr = textWienerSearchSize.getText();

	        if (testParameter(tmpStr, 2, 256)) {
	            n_W = Integer.valueOf(tmpStr).intValue();
	        } else {
	            MipavUtil.displayError("Wiener thresholding search window size must be between 2 and 256");
	            textWienerSearchSize.requestFocus();
	            textWienerSearchSize.selectAll();

	            return false;
	        }
	        
	        tmpStr = textWienerSimilar.getText();

	        if (testParameter(tmpStr, 2, 256)) {
	            N_W = Integer.valueOf(tmpStr).intValue();
	        } else {
	            MipavUtil.displayError("Wiener thresholding maximum number of similar patches kept must be between 2 and 256");
	            textWienerSimilar.requestFocus();
	            textWienerSimilar.selectAll();

	            return false;
	        }
	        
	        tmpStr = textWienerStep.getText();

	        if (testParameter(tmpStr, 1, 10)) {
	            p_W = Integer.valueOf(tmpStr).intValue();
	        } else {
	            MipavUtil.displayError("Wiener thresholding loop over step must be between 1 and 10");
	            textWienerStep.requestFocus();
	            textWienerStep.selectAll();

	            return false;
	        }
	        
            useSD_W = checkBoxWienerSDWeighting.isSelected();
	        
	        if (WienerBIORButton.isSelected()) {
	        	tau_2D_W = "BIOR";
	        }
	        else {
	        	tau_2D_W = "DCT";
	        }
	        
	        return true;
	    }
	    
	    /**
	     * Once all the necessary variables are set, call the Fuzzy C Means algorithm based on what type of image this is
	     * and whether or not there is a separate destination image.
	     */
	    protected void callAlgorithm() {
	    	try {
		        destImage = new ModelImage[2];
		        destImage[0] = new ModelImage(ModelStorageBase.DOUBLE, image.getExtents(), image.getImageName()+"_BM3D_stage1");
		        destImage[1] = new ModelImage(ModelStorageBase.DOUBLE, image.getExtents(), image.getImageName()+"_BM3D_stage2");
		        bm3dAlgo = new BM3D(destImage, image, sigma, n_H, N_H,
				    p_H, useSD_H, tau_2D_H, lambda3D_H,
				    n_W, N_W, p_W, useSD_W, tau_2D_W);
		        
		        // This is very important. Adding this object as a listener allows the algorithm to
	            // notify this object when it has completed of failed. See algorithm performed event.
	            // This is made possible by implementing AlgorithmedPerformed interface
	            bm3dAlgo.addListener(this);
	
	            createProgressBar(image.getImageName(), bm3dAlgo);
	            
	            // Hide dialog
	            setVisible(false);
	
	            if (isRunInSeparateThread()) {
	
	                // Start the thread as a low priority because we wish to still have user interface work fast.
	                if (bm3dAlgo.startMethod(Thread.MIN_PRIORITY) == false) {
	                    MipavUtil.displayError("A thread is already running on this object");
	                }
	            } else {
	                bm3dAlgo.run();
	            }
	        } catch (OutOfMemoryError x) {
	
	            if (destImage != null) {
	                if (destImage[0] != null) {
	                	destImage[0].disposeLocal();
	                	destImage[0] = null;
	                }
	                if (destImage[1] != null) {
	                	destImage[1].disposeLocal();
	                	destImage[1] = null;
	                }
	                destImage = null;
	                
	            }
	
	            System.gc();
	            MipavUtil.displayError("Dialog BM3D: unable to allocate enough memory");
	
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
	        int i;
	        ViewJFrameImage[] imageFrame = new ViewJFrameImage[2];

	        if (algorithm instanceof BM3D) {
	            image.clearMask();

	            if ((bm3dAlgo.isCompleted() == true) && (destImage != null)) {

	                // The algorithm has completed and produced a new image to be displayed.
	                for (i = 0; i < 2; i++) {
	                    updateFileInfo(image, destImage[i]);
	                    destImage[i].clearMask();

	                    try {
	                        imageFrame[i] = new ViewJFrameImage(destImage[i], null, new Dimension(610, 200 + (i * 20)));
	                    } catch (OutOfMemoryError error) {
	                        System.gc();
	                        JOptionPane.showMessageDialog(null, "Out of memory: unable to open new frame", "Error",
	                                                      JOptionPane.ERROR_MESSAGE);
	                    }
	                    
	                	// Copy original source NIFTI matrices to result images
	                    destImage[i].getMatrixHolder().replaceMatrices(image.getMatrixHolder().getMatrices());
	                }
	            } else if (destImage != null) {

	                // algorithm failed but result image still has garbage
	                for (i = 0; i < 2; i++) {

	                    if (destImage[i] != null) {
	                        destImage[i].disposeLocal(); // Clean up memory of result image
	                        destImage[i] = null;
	                    }
	                }

	                destImage = null;
	                System.gc();
	            }
	        }

	        if (algorithm.isCompleted()) {
	            insertScriptLine();
	        }
	     // save the completion status for later
	        setComplete(algorithm.isCompleted());

	        bm3dAlgo.finalize();
	        bm3dAlgo = null;
	        dispose();
	    }
	    
	    /**
	     * Returns whether the action has successfully completed its execution.
	     * 
	     * @return True, if the action is complete. False, if the action failed or is still running.
	     */
	    @Override
	    public boolean isActionComplete() {
	        return isComplete();
	    }
	    
	    /**
	     * Accessor that returns the image.
	     *
	     * @return  The result image.
	     */
	    public ModelImage[] getResultImage() {
	        return destImage;
	    }
	    
	    /**
	     * {@inheritDoc}
	     */
	    protected void setGUIFromParams() {
	        image = scriptParameters.retrieveInputImage();
	        parentFrame = image.getParentFrame();
	        sigma = scriptParameters.getParams().getDouble("sig");
	        n_H = scriptParameters.getParams().getInt("nH");
	        N_H = scriptParameters.getParams().getInt("NH");
	        p_H = scriptParameters.getParams().getInt("pH");
	        useSD_H = scriptParameters.getParams().getBoolean("useSDH");
	        tau_2D_H = scriptParameters.getParams().getString("tau2DH");
	        lambda3D_H = scriptParameters.getParams().getDouble("lamda3DH");
	        n_W = scriptParameters.getParams().getInt("nW");
	        N_W = scriptParameters.getParams().getInt("NW");
	        p_W = scriptParameters.getParams().getInt("pW");
	        useSD_W = scriptParameters.getParams().getBoolean("useSDW");
	        tau_2D_W = scriptParameters.getParams().getString("tau2DW");
	        
	        destImage = new ModelImage[2];
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
	            table.put(new ParameterDouble("sig",20.0));
	            table.put(new ParameterInt("nH",16));
	            table.put(new ParameterInt("NH",16));
	            table.put(new ParameterInt("pH",3));
	            table.put(new ParameterBoolean("useSDH", false));
	            table.put(new ParameterString("tau2DH", "BIOR"));
	            table.put(new ParameterDouble("lambda3DH",2.7));
	            table.put(new ParameterInt("nW",16));
	            table.put(new ParameterInt("NW",32));
	            table.put(new ParameterInt("pW",3));
	            table.put(new ParameterBoolean("useSDW", true));
	            table.put(new ParameterString("tau2DW", "DCT"));
	        } catch (final ParserException e) {
	            // this shouldn't really happen since there isn't any real parsing going on...
	            e.printStackTrace();
	        }

	        return table;
	    }
	    
	    /**
	     * {@inheritDoc}
	     */
	    protected void storeParamsFromGUI() throws ParserException {
	        scriptParameters.storeInputImage(image);

	        for (int i = 0; i < 2; i++) {
	            scriptParameters.storeImageInRecorder(getResultImage()[i]);
	        }
	        
	        scriptParameters.getParams().put(ParameterFactory.newParameter("sig", sigma));
	        scriptParameters.getParams().put(ParameterFactory.newParameter("nH", n_H));
	        scriptParameters.getParams().put(ParameterFactory.newParameter("NH", N_H));
	        scriptParameters.getParams().put(ParameterFactory.newParameter("pH", p_H));
	        scriptParameters.getParams().put(ParameterFactory.newParameter("useSDH", useSD_H));
	        scriptParameters.getParams().put(ParameterFactory.newParameter("tau2DH", tau_2D_H));
	        scriptParameters.getParams().put(ParameterFactory.newParameter("lambda3DH", lambda3D_H));
	        scriptParameters.getParams().put(ParameterFactory.newParameter("nW", n_W));
	        scriptParameters.getParams().put(ParameterFactory.newParameter("NW", N_W));
	        scriptParameters.getParams().put(ParameterFactory.newParameter("pW", p_W));
	        scriptParameters.getParams().put(ParameterFactory.newParameter("useSDW", useSD_W));
	        scriptParameters.getParams().put(ParameterFactory.newParameter("tau2DW", tau_2D_W));
	    }
	    
	    /**
	     * Returns the name of an image output by this algorithm, the image returned depends on the parameter label given
	     * (which can be used to retrieve the image object from the image registry).
	     * 
	     * @param imageParamName The output image parameter label for which to get the image name.
	     * @return The image name of the requested output image parameter label.
	     */
	    public String getOutputImageName(final String imageParamName) {
	        if (imageParamName.equals(AlgorithmParameters.RESULT_IMAGE+"1")) {
	        	return destImage[0].getImageName();
	        }
	        else if (imageParamName.equals(AlgorithmParameters.RESULT_IMAGE+"2")) {
	        	return destImage[1].getImageName();
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
	            table.put(new ParameterImage(AlgorithmParameters.RESULT_IMAGE+"1"));
	            table.put(new ParameterImage(AlgorithmParameters.RESULT_IMAGE+"2"));
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
	                return new String("Algorithms.filter");
	            }

	            public String getDescription() {
	                return new String("Applies a BM3D filter.");
	            }

	            public String getDescriptionLong() {
	                return new String("Applies a BM3D filter.");
	            }

	            public String getShortLabel() {
	                return new String("BM3D");
	            }

	            public String getLabel() {
	                return new String("BM3D");
	            }

	            public String getName() {
	                return new String("BM3D");
	            }
	        };
	    }

	
}