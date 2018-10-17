package gov.nih.mipav.view.dialogs;


import gov.nih.mipav.model.algorithms.*;
import gov.nih.mipav.model.algorithms.filters.BiorthogonalWavelets;
import gov.nih.mipav.model.scripting.*;
import gov.nih.mipav.model.scripting.parameters.ParameterFactory;
import gov.nih.mipav.model.structures.*;

import gov.nih.mipav.view.*;

import java.awt.*;
import java.awt.event.*;

import javax.swing.*;


/**
 * Dialog to get user input, then call the algorithm.
 */
public class JDialogBiorthogonalWavelets extends JDialogScriptableBase implements AlgorithmInterface{

    //~ Static fields/initializers -------------------------------------------------------------------------------------

    /** Use serialVersionUID for interoperability. */
    //private static final long serialVersionUID;
	
    private final int CDF_type = 1;
	
	private final int spline_type = 2;
	
	private final int waveletMatrix_method = 1;
	
	private final int convolution_method = 2;
	
	private BiorthogonalWavelets bwAlgo;
	
	private ModelImage srcImage;
	
    private ModelImage transformImage;
    
    private ModelImage compressedTransformImage;
    
    private ModelImage reconstructedImage;
    
    private int iterations;
    
    private JTextField iterationsText;
    
    private JTextField compressionText;
    // 0 <= compressionFactor <= 1.
    // Alternatively, when compressionFactor == -1, the lowest frequency approximation is retained.
    private double compressionFactor;
    
    private ButtonGroup typeGroup;
    private JRadioButton CDFButton;
    private JRadioButton splineButton;
    // Type can be "spline" or "CDF" and controls the type of biorthogonal wavelet filter used
    private int type;  
    
    // 2*(l+R) + 1 and 2*(lt + C) + 1 are the lengths of the lowpass biorthogonal filters h and ht,
    // where R and C are the number of real and complex zeros of P(t), deg(P) = l + lt - 1,
    // respectively when type == "CDF".
    //private int l;
    
    //private int lt;
    
    // 2*N+Nt-1 and Nt+1 are the lengths of the lowpass biorthogonal spline filters h and ht,
    // respectively when type == "spline".
    //private int N;
    
    //private int Nt;
    
    // Equal l and lt for CDF_type and N and Nt for spline_type
    private JTextField lenText;
    private JTextField lentText;
    private int len;
    
    private int lent;
    
    private JCheckBox displayCheckBox;
    // Plot result or not
    private boolean display = true;
    
    private ButtonGroup methodGroup;
    private JRadioButton convolutionButton;
    private JRadioButton waveletMatrixButton;
    // Forward transform computation method
    // convolution_method or waveletMatrix_method
    int method = convolution_method;
	
	/**
     * Empty constructor needed for dynamic instantiation.
     */
    public JDialogBiorthogonalWavelets() { }

    /**
     * Construct the biorthogonal wavelets dialog.
     *
     * @param  theParentFrame  Parent frame.
     * @param  im              Source image.
     */
    public JDialogBiorthogonalWavelets(Frame theParentFrame, ModelImage im) {
        super(theParentFrame, false);
        srcImage = im;
        init();
        setVisible(true);
    }

    //~ Methods --------------------------------------------------------------------------------------------------------
    
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
        } else if (command.equals("Help")) {
            //MipavUtil.showHelp("");
        } else {
            super.actionPerformed(event);
        }
    }
    
    /**
     * Sets up the GUI (panels, buttons, etc) and displays it on the screen.
     */
    private void init() {
        setForeground(Color.black);

        setTitle("Biorthogonal Wavelets");
        getContentPane().setLayout(new BorderLayout());
        JPanel paramsPanel = new JPanel(new GridBagLayout());
        paramsPanel.setBorder(buildTitledBorder("Wavelet parameters"));
        
        JLabel iterationsLabel = new JLabel("Iterations");
        iterationsLabel.setForeground(Color.black);
        iterationsLabel.setFont(serif12);
        iterationsLabel.setEnabled(true);
        
        GridBagConstraints gbc = new GridBagConstraints();
        gbc.gridwidth = 1;
        gbc.anchor = GridBagConstraints.WEST;
        gbc.weightx = 1;

        gbc.gridx = 0;
        gbc.gridy = 0;
        paramsPanel.add(iterationsLabel, gbc);
        
        iterationsText = new JTextField(10);
        iterationsText.setText("10");
        iterationsText.setFont(serif12);
        iterationsText.setForeground(Color.black);
        iterationsText.setEnabled(true);
        
        gbc.gridx = 1;
        gbc.gridy = 0;
        paramsPanel.add(iterationsText, gbc);
        
        JLabel compressionLabel = new JLabel("Compression factor (0.0 to 1.0) thrown away");
        compressionLabel.setForeground(Color.black);
        compressionLabel.setFont(serif12);
        compressionLabel.setEnabled(true);
        
        gbc.gridx = 0;
        gbc.gridy = 1;
        paramsPanel.add(compressionLabel, gbc);
        
        JLabel compressionLabel2 = new JLabel("Alternatively, when compressionFactor == -1,");
        compressionLabel2.setForeground(Color.black);
        compressionLabel2.setFont(serif12);
        compressionLabel2.setEnabled(true);
        
        gbc.gridx = 0;
        gbc.gridy = 2;
        paramsPanel.add(compressionLabel2, gbc);
        
        JLabel compressionLabel3 = new JLabel("the lowest frequency approximation is retained.");
        compressionLabel3.setForeground(Color.black);
        compressionLabel3.setFont(serif12);
        compressionLabel3.setEnabled(true);
        
        gbc.gridx = 0;
        gbc.gridy = 3;
        paramsPanel.add(compressionLabel3, gbc);
        
        compressionText = new JTextField(10);
        compressionText.setText("0.5");
        compressionText.setFont(serif12);
        compressionText.setForeground(Color.black);
        compressionText.setEnabled(true);
        
        gbc.gridx = 0;
        gbc.gridy = 4;
        paramsPanel.add(compressionText, gbc);
        
        displayCheckBox = new JCheckBox("Display wavelets");
        displayCheckBox.setFont(serif12);
        displayCheckBox.setForeground(Color.black);
        displayCheckBox.setEnabled(true);
        displayCheckBox.setSelected(true);
        
        gbc.gridx = 0;
        gbc.gridy = 5;
        paramsPanel.add(displayCheckBox, gbc);
        
        JLabel methodLabel = new JLabel("Forward transform computation method");
        methodLabel.setForeground(Color.black);
        methodLabel.setFont(serif12);
        methodLabel.setEnabled(true);
        
        gbc.gridx = 0;
        gbc.gridy = 6;
        paramsPanel.add(methodLabel, gbc);
        
        methodGroup = new ButtonGroup();
        convolutionButton = new JRadioButton("Convolution", true);
        convolutionButton.setFont(serif12);
        methodGroup.add(convolutionButton);
        
        gbc.gridx = 0;
        gbc.gridy = 7;
        paramsPanel.add(convolutionButton, gbc);

        waveletMatrixButton = new JRadioButton("Wavelet matrix", false);
        waveletMatrixButton.setFont(serif12);
        methodGroup.add(waveletMatrixButton);

        gbc.gridx = 0;
        gbc.gridy = 8;
        paramsPanel.add(waveletMatrixButton, gbc);
        
        JLabel typeLabel = new JLabel("Type of biorthogonal wavelet filter");
        typeLabel.setForeground(Color.black);
        typeLabel.setFont(serif12);
        typeLabel.setEnabled(true);
        
        gbc.gridx = 0;
        gbc.gridy = 9;
        paramsPanel.add(typeLabel, gbc);
        
        typeGroup = new ButtonGroup();
        CDFButton = new JRadioButton("CDF", true);
        CDFButton.setFont(serif12);
        typeGroup.add(CDFButton);
        
        gbc.gridx = 0;
        gbc.gridy = 10;
        paramsPanel.add(CDFButton, gbc);

        splineButton = new JRadioButton("Spline", false);
        splineButton.setFont(serif12);
        typeGroup.add(splineButton);

        gbc.gridx = 0;
        gbc.gridy = 11;
        paramsPanel.add(splineButton, gbc);
        
        JLabel lengthLabel = new JLabel("Determining low pass filter lengths:");
        lengthLabel.setForeground(Color.black);
        lengthLabel.setFont(serif12);
        lengthLabel.setEnabled(true);
        
        gbc.gridx = 0;
        gbc.gridy = 12;
        paramsPanel.add(lengthLabel, gbc);
        

        JLabel lengthLabel2 = new JLabel("len");
        lengthLabel2.setForeground(Color.black);
        lengthLabel2.setFont(serif12);
        lengthLabel2.setEnabled(true);
        
        gbc.gridx = 0;
        gbc.gridy = 13;
        paramsPanel.add(lengthLabel2, gbc);
        
        lenText = new JTextField(10);
        lenText.setText("2");
        lenText.setFont(serif12);
        lenText.setForeground(Color.black);
        lenText.setEnabled(true);
        
        gbc.gridx = 1;
        gbc.gridy = 13;
        paramsPanel.add(lenText, gbc);
        
        JLabel lengthLabel3 = new JLabel("lent");
        lengthLabel3.setForeground(Color.black);
        lengthLabel3.setFont(serif12);
        lengthLabel3.setEnabled(true);
        
        gbc.gridx = 0;
        gbc.gridy = 14;
        paramsPanel.add(lengthLabel3, gbc);
        
        lentText = new JTextField(10);
        lentText.setText("2");
        lentText.setFont(serif12);
        lentText.setForeground(Color.black);
        lentText.setEnabled(true);
        
        gbc.gridx = 1;
        gbc.gridy = 14;
        paramsPanel.add(lentText, gbc);
        
        getContentPane().add(paramsPanel, BorderLayout.CENTER);
        getContentPane().add(buildButtons(), BorderLayout.SOUTH);
        pack();
        setResizable(true);

        System.gc();
    }
    
    private boolean setVariables() {
    	String tmpStr;
    	tmpStr = iterationsText.getText();
    	try {
            iterations = Integer.valueOf(tmpStr).intValue();
    	}
    	catch(NumberFormatException e) {
    		MipavUtil.displayError("Iterations must be an integer");
    		iterationsText.requestFocus();
    		iterationsText.selectAll();
    		return false;
    	}

        if (iterations <= 0) {
            MipavUtil.displayError("iterations must be greater than 0");
            iterationsText.requestFocus();
            iterationsText.selectAll();
            return false;
        }
        
        tmpStr = compressionText.getText();
        try {
        	compressionFactor = Double.valueOf(tmpStr).doubleValue();
        }
        catch(NumberFormatException e) {
    		MipavUtil.displayError("compressionFactor must be a number");
    		compressionText.requestFocus();
    		compressionText.selectAll();
    		return false;
    	}
        
        if ((compressionFactor != -1.0) && ((compressionFactor < 0.0) || (compressionFactor > 1.0))) {
        	MipavUtil.displayError("compressionFactor must == -1.0 or must have 0.0 <= compressionFactor <= 1.0");
        	compressionText.requestFocus();
        	compressionText.selectAll();
        	return false;
        }
        
        display = displayCheckBox.isSelected();
        
        if (convolutionButton.isSelected()) {
        	method = convolution_method;
        }
        else {
        	method = waveletMatrix_method;
        }
        
        if (CDFButton.isSelected()) {
        	type = CDF_type;
        }
        else {
        	type = spline_type;
        }
        
        tmpStr = lenText.getText();
        try {
            len = Integer.valueOf(tmpStr).intValue();	
        }
        catch(NumberFormatException e) {
    		MipavUtil.displayError("len must be an integer");
    		lenText.requestFocus();
    		lenText.selectAll();
    		return false;
    	}
        
        tmpStr = lentText.getText();
        try {
            lent = Integer.valueOf(tmpStr).intValue();	
        }
        catch(NumberFormatException e) {
    		MipavUtil.displayError("lent must be an integer");
    		lentText.requestFocus();
    		lentText.selectAll();
    		return false;
    	}
        
    	return true;
    }
	
	protected void callAlgorithm() {
		try {
		    transformImage = new ModelImage(ModelStorageBase.DOUBLE, srcImage.getExtents(), srcImage.getImageName() + "_transform");
		    compressedTransformImage = new ModelImage(ModelStorageBase.DOUBLE, srcImage.getExtents(), 
		    		srcImage.getImageName() + "_compressedTransform");
		    reconstructedImage = new ModelImage(ModelStorageBase.DOUBLE, srcImage.getExtents(), srcImage.getImageName() + "_reconstructed");
		    
		    bwAlgo = new BiorthogonalWavelets(transformImage, compressedTransformImage, 
		    		reconstructedImage, srcImage, iterations, compressionFactor,
		    		display, method, type, len, lent);
		    
		   // This is very important. Adding this object as a listener allows the algorithm to
		    // notify this object when it has completed of failed. See algorithm performed event.
		    // This is made possible by implementing AlgorithmedPerformed interface
		    bwAlgo.addListener(this);
		
		
		    createProgressBar(srcImage.getImageName(), bwAlgo);
		
		    // Hide dialog
		    setVisible(false);
		
		    if (isRunInSeparateThread()) {
		
		        // Start the thread as a low priority because we wish to still have user interface work fast.
		        if (bwAlgo.startMethod(Thread.MIN_PRIORITY) == false) {
		            MipavUtil.displayError("A thread is already running on this object");
		        }
		    } else {
		
		        bwAlgo.run();
		    }
	    } catch (OutOfMemoryError x) {
	    	
	    	if (transformImage != null) {
	    		transformImage.disposeLocal();
	    		transformImage = null;
	    	}
	    	
	    	if (compressedTransformImage != null) {
	    		compressedTransformImage.disposeLocal();
	    		compressedTransformImage = null;
	    	}
	    	
	    	if (reconstructedImage != null) {
	    		reconstructedImage.disposeLocal();
	    		reconstructedImage = null;
	    	}
	
	        System.gc();
	        MipavUtil.displayError("Dialog Biorthogonal Wavelets: unable to allocate enough memory");
	        return;
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
    	
        int i = 0;

        if (algorithm instanceof BiorthogonalWavelets) {
        	
        	if (bwAlgo.isCompleted()) {
        		if (transformImage != null) {
        			updateFileInfo(srcImage, transformImage);
        			
        			try {
                        new ViewJFrameImage(transformImage, null, new Dimension(610, 200 + (i * 20)));
                        i++;
                    } catch (OutOfMemoryError error) {
                        System.gc();
                        JOptionPane.showMessageDialog(null, "Out of memory: unable to open new transformImage frame",
                                                      "Error", JOptionPane.ERROR_MESSAGE);
                    }
        		} // if (transformImage != null)
        		
        		if (compressedTransformImage != null) {
        			updateFileInfo(srcImage, compressedTransformImage);
        			
        			try {
                        new ViewJFrameImage(compressedTransformImage, null, new Dimension(610, 200 + (i * 20)));
                        i++;
                    } catch (OutOfMemoryError error) {
                        System.gc();
                        JOptionPane.showMessageDialog(null, "Out of memory: unable to open new compressedTransformImage frame",
                                                      "Error", JOptionPane.ERROR_MESSAGE);
                    }
        		} // if (compressedTransformImage != null)
        		
        		if (reconstructedImage != null) {
        			updateFileInfo(srcImage, reconstructedImage);
        			
        			try {
                        new ViewJFrameImage(reconstructedImage, null, new Dimension(610, 200 + (i * 20)));
                        i++;
                    } catch (OutOfMemoryError error) {
                        System.gc();
                        JOptionPane.showMessageDialog(null, "Out of memory: unable to open new reconstructedImage frame",
                                                      "Error", JOptionPane.ERROR_MESSAGE);
                    }
        		} // if (reconstructedImage != null)
        	} // if (bwAlgo.isCompleted())
        	else {
        		if (transformImage != null) {
    	    		transformImage.disposeLocal();
    	    		transformImage = null;
    	    	}
    	    	
    	    	if (compressedTransformImage != null) {
    	    		compressedTransformImage.disposeLocal();
    	    		compressedTransformImage = null;
    	    	}
    	    	
    	    	if (reconstructedImage != null) {
    	    		reconstructedImage.disposeLocal();
    	    		reconstructedImage = null;
    	    	}	

                System.gc();
        	}
        }

        if (algorithm.isCompleted()) {
            insertScriptLine();
        }

        dispose();
    }
	
	protected void setGUIFromParams() {
		srcImage = scriptParameters.retrieveInputImage();
		iterations = scriptParameters.getParams().getInt("iters");
		compressionFactor = scriptParameters.getParams().getDouble("compression");
		display = scriptParameters.getParams().getBoolean("disp");
		method = scriptParameters.getParams().getInt("meth");
		type = scriptParameters.getParams().getInt("typ");
		len = scriptParameters.getParams().getInt("le");
		lent = scriptParameters.getParams().getInt("let");
	}
	
	protected void storeParamsFromGUI() throws ParserException {
		scriptParameters.storeInputImage(srcImage);	
		scriptParameters.storeImageInRecorder(transformImage);
		scriptParameters.storeImageInRecorder(compressedTransformImage);
		scriptParameters.storeImageInRecorder(reconstructedImage);
		scriptParameters.getParams().put(ParameterFactory.newParameter("iters", iterations));
		scriptParameters.getParams().put(ParameterFactory.newParameter("compression", compressionFactor));
		scriptParameters.getParams().put(ParameterFactory.newParameter("disp", display));
		scriptParameters.getParams().put(ParameterFactory.newParameter("meth", method));
		scriptParameters.getParams().put(ParameterFactory.newParameter("typ", type));
		scriptParameters.getParams().put(ParameterFactory.newParameter("le", len));
		scriptParameters.getParams().put(ParameterFactory.newParameter("let", lent));
	}
}
