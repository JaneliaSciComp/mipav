package gov.nih.mipav.view.dialogs;


import gov.nih.mipav.model.algorithms.*;
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
public class JDialogHartleyTransform extends JDialogScriptableBase implements AlgorithmInterface{

    //~ Static fields/initializers -------------------------------------------------------------------------------------

    /** Use serialVersionUID for interoperability. */
    //private static final long serialVersionUID;
	private boolean multiProcessor = false;
		
	private ModelImage srcImage;
	
    private ModelImage transformImage;
    
    private ModelImage inverseImage;
    
    private HartleyTransform2 htAlgo;
    
    private int constructionMethod;
    public static final int CONSTRUCTION_NONE = 0;
    public static final int GAUSSIAN = 2;
    public static final int BUTTERWORTH = 3;
    public static final int CHEBYSHEV_TYPE_I = 5;
    public static final int CHEBYSHEV_TYPE_II = 6;
    public static final int ELLIPTIC = 7;
    
    private int filterType;
    private double f1;
    private double f2;
	public static final int LOWPASS = 1;
    public static final int HIGHPASS = 2;
    public static final int BANDPASS = 3;
    public static final int BANDSTOP = 4;
    private int filterOrder;
    // maximum ripple in Chebyshev filters
    // passband ripple in dB = 10*log(1 + e[0]**2) in Elliptic filter
    private double epsilon;  
    
    private double rs; // decibels stopband is down in Elliptic filter
    
    private JPanel constructionPanel;
    private ButtonGroup constructionGroup;
    private JRadioButton butterworthFilter;
    private JRadioButton gaussianFilter;
    private JRadioButton chebyshevIFilter;
    private JRadioButton chebyshevIIFilter;
    private JRadioButton ellipticFilter;
    private JLabel labelOrder;
    private JLabel labelEpsilon;
    private JLabel labelRs;
    private JTextField textOrder;
    private JTextField textEpsilon;
    private JTextField textRs;
    
    private JPanel filterPanel;
    private ButtonGroup filterTypeGroup;
    private JRadioButton noneButton;
    private JRadioButton lowPass;
    private JRadioButton highPass;
    private JRadioButton bandPass;
    private JRadioButton bandStop;
    private JLabel labelF1;
    private JLabel labelF2;
    private JTextField textF1;
    private JTextField textF2;
    private JPanel mainPanel;
    
    
    //private ButtonGroup processorGroup;
    //private JRadioButton singleButton;
    //private JRadioButton multiButton;
	
	/**
     * Empty constructor needed for dynamic instantiation.
     */
    public JDialogHartleyTransform() { }

    /**
     * Construct the Hartley Transform dialog.
     *
     * @param  theParentFrame  Parent frame.
     * @param  im              Source image.
     */
    public JDialogHartleyTransform(Frame theParentFrame, ModelImage im) {
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
        Object source = event.getSource();

        if (command.equals("OK")) {

            if (setVariables()) {
                callAlgorithm();
            }
        } else if (source == noneButton) {
            textOrder.setEnabled(false);
            labelOrder.setEnabled(false);
            textEpsilon.setEnabled(false);
            labelEpsilon.setEnabled(false);
            textRs.setEnabled(false);
            labelRs.setEnabled(false);
            lowPass.setEnabled(false);
            highPass.setEnabled(false);
            bandPass.setEnabled(false);
            bandStop.setEnabled(false);
            textF1.setEnabled(false);
            labelF1.setText("Frequency F1 > 0.0 to 1.0 ");
            labelF1.setEnabled(false);
            textF2.setEnabled(false);
            labelF2.setEnabled(false);
        } else if (source == gaussianFilter) {
            textOrder.setEnabled(false);
            labelOrder.setEnabled(false);
            textEpsilon.setEnabled(false);
            labelEpsilon.setEnabled(false);
            textRs.setEnabled(false);
            labelRs.setEnabled(false);
            textF1.setEnabled(true);
            labelF1.setText("Frequency F1 > 0.0 ");
            labelF1.setEnabled(true);
            textF2.setEnabled(false);
            labelF2.setEnabled(false);
            lowPass.setEnabled(true);
            highPass.setEnabled(true);
            bandPass.setEnabled(false);
            if (bandPass.isSelected()) {
                bandPass.setSelected(false);
                highPass.setSelected(true);
            }
            bandStop.setEnabled(false);
            if (bandStop.isSelected()) {
                bandStop.setSelected(false);
                highPass.setSelected(true);
            }
        } else if (source == butterworthFilter) {
            textOrder.setEnabled(true);
            labelOrder.setEnabled(true);
            textEpsilon.setEnabled(false);
            labelEpsilon.setEnabled(false);
            textRs.setEnabled(false);
            labelRs.setEnabled(false);
            lowPass.setEnabled(true);
            highPass.setEnabled(true);
            bandPass.setEnabled(true);
            bandStop.setEnabled(true);
            textF1.setEnabled(true);
            labelF1.setText("Frequency F1 > 0.0 to 1.0 ");
            labelF1.setEnabled(true);
            if (bandPass.isSelected() || bandStop.isSelected()) {
                textF2.setEnabled(true);
                labelF2.setEnabled(true);
            }
            else {
            	textF2.setEnabled(false);
                labelF2.setEnabled(false);	
            }
        } else if (source == chebyshevIFilter) {
            textOrder.setEnabled(true);
            labelOrder.setEnabled(true);
            textEpsilon.setEnabled(true);
            labelEpsilon.setEnabled(true);
            labelEpsilon.setText("Maximum pass band ripple");
            textRs.setEnabled(false);
            labelRs.setEnabled(false);
            lowPass.setEnabled(true);
            highPass.setEnabled(true);
            bandPass.setEnabled(true);
            bandStop.setEnabled(true);
            textF1.setEnabled(true);
            labelF1.setText("Frequency F1 > 0.0 to 1.0 ");
            labelF1.setEnabled(true);
            if (bandPass.isSelected() || bandStop.isSelected()) {
                textF2.setEnabled(true);
                labelF2.setEnabled(true);
            }
            else {
            	textF2.setEnabled(false);
                labelF2.setEnabled(false);	
            }
        } else if (source == chebyshevIIFilter) {
        	textF1.setEnabled(true);
        	labelF1.setText("Freq F1 > 1/(2*PI) to 1.0 ");
            labelF1.setEnabled(true);
            if (bandPass.isSelected() || bandStop.isSelected()) {
                textF2.setEnabled(true);
                labelF2.setEnabled(true);
            }
            else {
            	textF2.setEnabled(false);
                labelF2.setEnabled(false);	
            }
            textOrder.setEnabled(true);
            labelOrder.setEnabled(true);
            textEpsilon.setEnabled(true);
            labelEpsilon.setEnabled(true);
            labelEpsilon.setText("Maximum stop band ripple");
            textRs.setEnabled(false);
            labelRs.setEnabled(false);
            lowPass.setEnabled(true);
            highPass.setEnabled(true);
            bandPass.setEnabled(true);
            bandStop.setEnabled(true);
        } else if (source == ellipticFilter) {
        	textF1.setEnabled(true);
            labelF1.setText("Frequency F1 > 0.0 to 1.0 ");
            labelF1.setEnabled(true);
            if (bandPass.isSelected() || bandStop.isSelected()) {
                textF2.setEnabled(true);
                labelF2.setEnabled(true);
            }
            else {
            	textF2.setEnabled(false);
                labelF2.setEnabled(false);	
            }
            textOrder.setEnabled(true);
            textOrder.setText("7");
            labelOrder.setEnabled(true);
            textEpsilon.setText("0.1");
            textEpsilon.setEnabled(true);
            labelEpsilon.setEnabled(true);
            labelEpsilon.setText("Passband ripple in decibels");
            textRs.setEnabled(true);
            labelRs.setEnabled(true);
            lowPass.setEnabled(true);
            highPass.setEnabled(true);
            bandPass.setEnabled(true);
            bandStop.setEnabled(true);    
        } else if ((source == lowPass) || (source == highPass)) {
            textF2.setEnabled(false);
            labelF2.setEnabled(false);
        } else if ((source == bandPass) || (source == bandStop)) {
            textF2.setEnabled(true);
            labelF2.setEnabled(true);
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

        setTitle("Hartley Transform");
        getContentPane().setLayout(new BorderLayout());
        
        constructionPanel = new JPanel(new GridBagLayout());
        constructionPanel.setBorder(buildTitledBorder("Filter construction methods"));

        constructionGroup = new ButtonGroup();
        noneButton = new JRadioButton("No filter", true);
        noneButton.setFont(serif12);
        noneButton.setForeground(Color.black);
        noneButton.addActionListener(this);
        constructionGroup.add(noneButton);
       
        butterworthFilter = new JRadioButton("Butterworth filter", false);
        butterworthFilter.setFont(serif12);
        butterworthFilter.setForeground(Color.black);
        butterworthFilter.addActionListener(this);
        constructionGroup.add(butterworthFilter);

        gaussianFilter = new JRadioButton("Gaussian filter", false);
        gaussianFilter.setFont(serif12);
        gaussianFilter.setForeground(Color.black);
        gaussianFilter.addActionListener(this);
        constructionGroup.add(gaussianFilter);
        
        chebyshevIFilter = new JRadioButton("Chebyshev Type I filter", false);
        chebyshevIFilter.setFont(serif12);
        chebyshevIFilter.setForeground(Color.black);
        chebyshevIFilter.addActionListener(this);
        constructionGroup.add(chebyshevIFilter);

        chebyshevIIFilter = new JRadioButton("Chebyshev Type II filter", false);
        chebyshevIIFilter.setFont(serif12);
        chebyshevIIFilter.setForeground(Color.black);
        chebyshevIIFilter.addActionListener(this);
        constructionGroup.add(chebyshevIIFilter);
        
        ellipticFilter = new JRadioButton("Elliptic filter", false);
        ellipticFilter.setFont(serif12);
        ellipticFilter.setForeground(Color.black);
        ellipticFilter.addActionListener(this);
        constructionGroup.add(ellipticFilter);

        textOrder = new JTextField(10);
        textOrder.setText("1");
        textOrder.setFont(serif12);
        textOrder.setForeground(Color.black);
        textOrder.setEnabled(false);

        labelOrder = new JLabel("Order");
        labelOrder.setForeground(Color.black);
        labelOrder.setFont(serif12);
        labelOrder.setEnabled(false);
        
        textEpsilon = new JTextField(10);
        textEpsilon.setText("0.5");
        textEpsilon.setFont(serif12);
        textEpsilon.setForeground(Color.black);
        textEpsilon.setEnabled(false);

        labelEpsilon = new JLabel("Maximum ripple");
        labelEpsilon.setForeground(Color.black);
        labelEpsilon.setFont(serif12);
        labelEpsilon.setEnabled(false);
        
        textRs = new JTextField(10);
        textRs.setText("55.43");
        textRs.setFont(serif12);
        textRs.setForeground(Color.black);
        textRs.setEnabled(false);
        
        labelRs = new JLabel("Decibels stop band is down");
        labelRs.setForeground(Color.black);
        labelRs.setFont(serif12);
        labelRs.setEnabled(false);
        
        GridBagConstraints gbc = new GridBagConstraints();
        gbc.gridwidth = 3;
        gbc.anchor = GridBagConstraints.WEST;
        gbc.weightx = 1;
        
        gbc.gridx = 0;
        gbc.gridy = 0;
        constructionPanel.add(noneButton, gbc);
        gbc.gridy = 1;
        constructionPanel.add(butterworthFilter, gbc);
        gbc.gridy = 2;
        constructionPanel.add(gaussianFilter, gbc);
        gbc.gridy = 3;
        constructionPanel.add(chebyshevIFilter, gbc);
        gbc.gridy = 4;
        constructionPanel.add(chebyshevIIFilter, gbc);
        gbc.gridy = 5;
        constructionPanel.add(ellipticFilter, gbc);
        gbc.gridx = 1;
        gbc.gridy = 6;
        gbc.gridwidth = 1;
        constructionPanel.add(labelOrder, gbc);
        gbc.gridx = 2;
        constructionPanel.add(textOrder, gbc);
        gbc.gridx = 1;
        gbc.gridy = 7;
        constructionPanel.add(labelEpsilon, gbc);
        gbc.gridx = 2;
        constructionPanel.add(textEpsilon, gbc);
        gbc.gridx = 1;
        gbc.gridy = 8;
        constructionPanel.add(labelRs, gbc);
        gbc.gridx = 2;
        constructionPanel.add(textRs, gbc);
        
        filterPanel = new JPanel(new GridBagLayout());
        filterPanel.setBorder(buildTitledBorder("Filter specifications"));

        filterTypeGroup = new ButtonGroup();

        lowPass = new JRadioButton("Lowpass", true);
        lowPass.setFont(serif12);
        lowPass.setForeground(Color.black);
        lowPass.addActionListener(this);
        lowPass.setEnabled(false);
        filterTypeGroup.add(lowPass);

            
        highPass = new JRadioButton("Highpass", false);
        highPass.setFont(serif12);
        highPass.addActionListener(this);
        highPass.setForeground(Color.black);
        highPass.setEnabled(false);
        filterTypeGroup.add(highPass);

        bandPass = new JRadioButton("Bandpass", false);
        bandPass.setFont(serif12);
        bandPass.setForeground(Color.black);
        bandPass.addActionListener(this);
        bandPass.setEnabled(false);
        filterTypeGroup.add(bandPass);

        bandStop = new JRadioButton("Bandstop", false);
        bandStop.setForeground(Color.black);
        bandStop.setFont(serif12);
        bandStop.addActionListener(this);
        bandStop.setEnabled(false);
        filterTypeGroup.add(bandStop);

        textF1 = new JTextField(10);
        textF1.setText("0.1");
        textF1.setFont(serif12);
        textF1.setEnabled(false);

        labelF1 = new JLabel("Frequency F1 > 0.0 to 1.0 ");
        labelF1.setForeground(Color.black);
        labelF1.setFont(serif12);
        labelF1.setEnabled(false);

        textF2 = new JTextField(10);
        textF2.setText("0.4");
        textF2.setFont(serif12);
        textF2.setEnabled(false);

        labelF2 = new JLabel("F2 exceeds F1 0.0 to 1.0 ");
        labelF2.setForeground(Color.black);
        labelF2.setFont(serif12);
        labelF2.setEnabled(false);

        gbc.gridx = 0;
        gbc.gridy = 0;
        gbc.gridwidth = 1;
        gbc.weightx = 0;
        gbc.fill = GridBagConstraints.NONE;
        filterPanel.add(lowPass, gbc);
        gbc.gridx = 1;
        filterPanel.add(labelF1, gbc);
        gbc.gridx = 2;
        gbc.weightx = .5;
        gbc.fill = GridBagConstraints.HORIZONTAL;
        filterPanel.add(textF1, gbc);
        gbc.gridx = 0;
        gbc.gridy = 1;
        filterPanel.add(highPass, gbc);
        gbc.gridy = 2;
        filterPanel.add(bandPass, gbc);
        gbc.gridx = 1;
        filterPanel.add(labelF2, gbc);
        gbc.gridx = 2;
        filterPanel.add(textF2, gbc);
        gbc.gridx = 0;
        gbc.gridy = 3;
        filterPanel.add(bandStop, gbc);

        mainPanel = new JPanel(new GridBagLayout());
        gbc.gridx = 0;
        gbc.gridy = 0;
        gbc.fill = GridBagConstraints.HORIZONTAL;
        mainPanel.add(constructionPanel, gbc);
        gbc.gridy = 1;
        mainPanel.add(filterPanel, gbc);

        getContentPane().add(mainPanel);
        getContentPane().add(buildButtons(), BorderLayout.SOUTH);
        pack();
        setResizable(true);

        System.gc();
    }
    
    private boolean setVariables() {
    	String tmpStr;
    	if (noneButton.isSelected()) {
            constructionMethod = CONSTRUCTION_NONE;
        } else if (gaussianFilter.isSelected()) {
            constructionMethod = GAUSSIAN;
        } else if (butterworthFilter.isSelected()) {
            constructionMethod = BUTTERWORTH;
            tmpStr = textOrder.getText();
            filterOrder = Integer.parseInt(tmpStr);

            if (filterOrder < 1) {
                MipavUtil.displayError("Butterworth order must be at least 1");
                textOrder.requestFocus();
                textOrder.selectAll();

                return false;
            }
        } else if (chebyshevIFilter.isSelected()) {
        	constructionMethod = CHEBYSHEV_TYPE_I;
            tmpStr = textOrder.getText();
            filterOrder = Integer.parseInt(tmpStr);

            if (filterOrder < 1) {
                MipavUtil.displayError("Chebyshev order must be at least 1");
                textOrder.requestFocus();
                textOrder.selectAll();

                return false;
            }
            
            tmpStr = textEpsilon.getText();
            epsilon = Double.parseDouble(tmpStr);
            
            if (epsilon <= 0.0) {
            	MipavUtil.displayError("Chebyshev maximum ripple epsilon must be greater than 0.0");
            	textEpsilon.requestFocus();
            	textEpsilon.selectAll();
            	
            	return false;
            }
        }  else if (chebyshevIIFilter.isSelected()) {
        	constructionMethod = CHEBYSHEV_TYPE_II;
            tmpStr = textOrder.getText();
            filterOrder = Integer.parseInt(tmpStr);

            if (filterOrder < 1) {
                MipavUtil.displayError("Chebyshev order must be at least 1");
                textOrder.requestFocus();
                textOrder.selectAll();

                return false;
            }
            
            tmpStr = textEpsilon.getText();
            epsilon = Double.parseDouble(tmpStr);
            
            if (epsilon <= 0.0) {
            	MipavUtil.displayError("Chebyshev maximum ripple epsilon mut be greater than 0.0");
            	textEpsilon.requestFocus();
            	textEpsilon.selectAll();
            	
            	return false;
            }
        } else if (ellipticFilter.isSelected()) {
        	constructionMethod = ELLIPTIC;
            tmpStr = textOrder.getText();
            filterOrder = Integer.parseInt(tmpStr);

            if (filterOrder < 1) {
                MipavUtil.displayError("Elliptic order must be at least 1");
                textOrder.requestFocus();
                textOrder.selectAll();

                return false;
            }
            
            tmpStr = textEpsilon.getText();
            epsilon = Double.parseDouble(tmpStr);
            
            if (epsilon <= 0.0) {
            	MipavUtil.displayError("Elliptic paasband ripple in decibels epsilon mut be greater than 0.0");
            	textEpsilon.requestFocus();
            	textEpsilon.selectAll();
            	
            	return false;
            }
            
            tmpStr = textRs.getText();
            rs = Double.parseDouble(tmpStr);
            
            if (rs <= 0.0) {
            	MipavUtil.displayError("Elliptic decibels stopband is down must be greater than 0.0");
            	textRs.requestFocus();
            	textRs.selectAll();
            	
            	return false;
            }
        }

    	if (lowPass.isSelected()) {
            filterType = LOWPASS;
        } else if (highPass.isSelected()) {
            filterType = HIGHPASS;
        } else if (bandPass.isSelected()) {
            filterType = BANDPASS;
        } else if (bandStop.isSelected()) {
            filterType = BANDSTOP;
        }

        tmpStr = textF1.getText();

        
        if (constructionMethod == GAUSSIAN) {
            f1 = Double.valueOf(tmpStr).doubleValue();

            if (f1 <= 0.0) {
                MipavUtil.displayError("F1 must exceed 0.0");
                textF1.requestFocus();
                textF1.selectAll();

                return false;
            }
        } // end of else if (constructionMethod == GAUSSIAN)
        else if ((constructionMethod == BUTTERWORTH) || (constructionMethod == CHEBYSHEV_TYPE_I) || (constructionMethod == ELLIPTIC)) {
            f1 = Double.valueOf(tmpStr).doubleValue();

            if (f1 <= 0.0) {
                MipavUtil.displayError("F1 must exceed 0.0");
                textF1.requestFocus();
                textF1.selectAll();

                return false;
            } else if (f1 > 1.0) {
                MipavUtil.displayError("F1 must not exceed 1.0");
                textF1.requestFocus();
                textF1.selectAll();

                return false;
            }
        } // else if ((constructionMethod == BUTTERWORTH) || (constructionMethod == CHEBYSHEV_TYPE_I) || (constructionMethod == ELLIPTIC))
        else if (constructionMethod == CHEBYSHEV_TYPE_II) {
        	f1 = Double.valueOf(tmpStr).doubleValue();

            if (f1 <= 1.0/(2.0*Math.PI)) {
                MipavUtil.displayError("F1 must exceed 1/(2.0*PI)");
                textF1.requestFocus();
                textF1.selectAll();

                return false;
            } else if (f1 > 1.0) {
                MipavUtil.displayError("F1 must not exceed 1.0");
                textF1.requestFocus();
                textF1.selectAll();

                return false;
            }
        	
        } // else if (constructionMethod == CHEBYSHEV_TYPE_II)
       

        if (((filterType == BANDPASS) || (filterType == BANDSTOP)) && (constructionMethod != CONSTRUCTION_NONE)) {
            tmpStr = textF2.getText();

            if (testParameter(tmpStr, 0.0, 1.0)) {
                f2 = Double.valueOf(tmpStr).doubleValue();


                if (f2 <= f1) {
                    MipavUtil.displayError("F2 must exceed F1");
                    textF2.requestFocus();
                    textF2.selectAll();

                    return false;
                }
            } else {
                MipavUtil.displayError("F2 must be between 0.0 and 1.0");
                textF2.requestFocus();
                textF2.selectAll();

                return false;
            }

        } // if (((filterType == BANDPASS) || (filterType == BANDSTOP)) && (constructionMethod != CONSTRUCTION_NONE))

    	return true;
    }
	
	protected void callAlgorithm() {
		try {
			
		    transformImage = new ModelImage(ModelStorageBase.DOUBLE, srcImage.getExtents(), srcImage.getImageName() + "_transform");
			
		    inverseImage = new ModelImage(ModelStorageBase.DOUBLE, srcImage.getExtents(), srcImage.getImageName() + "_inverse");	
		    
		    
		    htAlgo = new HartleyTransform2(transformImage, inverseImage, srcImage, constructionMethod, filterType, f1, f2,
		    		filterOrder, epsilon, rs);
		    
		    // This is very important. Adding this object as a listener allows the algorithm to
		    // notify this object when it has completed of failed. See algorithm performed event.
		    // This is made possible by implementing AlgorithmedPerformed interface
		    htAlgo.addListener(this);
		
		
		    createProgressBar(srcImage.getImageName(), htAlgo);
		
		    // Hide dialog
		    setVisible(false);
		
		    if (isRunInSeparateThread()) {
		
		        // Start the thread as a low priority because we wish to still have user interface work fast.
		        if (htAlgo.startMethod(Thread.MIN_PRIORITY) == false) {
		            MipavUtil.displayError("A thread is already running on this object");
		        }
		    } else {
		
		        htAlgo.run();
		    }
		   
	    } catch (OutOfMemoryError x) {
	    	
	    	if (transformImage != null) {
	    		transformImage.disposeLocal();
	    		transformImage = null;
	    	}
	    	
	        if (inverseImage != null) {
	    		inverseImage.disposeLocal();
	    		inverseImage = null;
	    	}
	
	        System.gc();
	        MipavUtil.displayError("Hartley Transform: unable to allocate enough memory");
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

        if (algorithm instanceof HartleyTransform2) {
        	
        	if (htAlgo.isCompleted()) {
        		if (transformImage != null) {
        			updateFileInfo(srcImage, transformImage);
        			
        			try {
                        new ViewJFrameImage(transformImage, null, new Dimension(610, 200));
                    } catch (OutOfMemoryError error) {
                        System.gc();
                        JOptionPane.showMessageDialog(null, "Out of memory: unable to open new transformImage frame",
                                                      "Error", JOptionPane.ERROR_MESSAGE);
                    }
        		} // if (transformImage != null)
        		
        		if (inverseImage != null) {
        			updateFileInfo(srcImage, inverseImage);
        			
        			try {
                        new ViewJFrameImage(inverseImage, null, new Dimension(610, 220));
                    } catch (OutOfMemoryError error) {
                        System.gc();
                        JOptionPane.showMessageDialog(null, "Out of memory: unable to open new inverseImage frame",
                                                      "Error", JOptionPane.ERROR_MESSAGE);
                    }
        		} // if (inverseImage != null)
        	} // if (dsAlgo.isCompleted())
        	else {
        		if (transformImage != null) {
    	    		transformImage.disposeLocal();
    	    		transformImage = null;
    	    	}
    	    	
    	    	if (inverseImage != null) {
    	    		inverseImage.disposeLocal();
    	    		inverseImage = null;
    	    	}	

                System.gc();
        	}
        }
        

        if (algorithm.isCompleted()) {
            insertScriptLine();
        }

        dispose();
    }
    
    /**
     * Accessor that sets the filter type (LOWPASS, HIGHPASS, BANDPASS, BANDSTOP).
     *
     * @param  type  Value to set the filter type to.
     */
    public void setFilterType(int type) {
        filterType = type;
    }

    /**
     * Accessor that sets the frequency 1 variable.
     *
     * @param  scale  Value to set frequency 1 to.
     */
    public void setF1(double scale) {
        f1 = scale;
    }

    /**
     * Accessor that sets the frequency 2 variable.
     *
     * @param  scale  Value to set frequency 2 to.
     */
    public void setF2(double scale) {
        f2 = scale;
    }
    
    /**
     * Accessor that sets the construction method (WINDOW, GAUSSIAN, BUTTERWORTH).
     *
     * @param  method  Value to set the construction method to.
     */
    public void setMethod(int method) {
        constructionMethod = method;
    }
    
   /**
    * 
    * @param epsilon
    */
    public void setEpsilon(double epsilon) {
    	this.epsilon = epsilon;
    }
    
    /**
     * 
     * @param rs
     */
     public void setRs(double rs) {
     	this.rs = rs;
     }
    
    /**
     * Accessor that sets the butterworth order.
     *
     * @param  order  Value to set the butterworth order to.
     */
    public void setfilterOrder(int order) {
        filterOrder = order;
    }
	
	protected void setGUIFromParams() {
		srcImage = scriptParameters.retrieveInputImage();
		setFilterType(scriptParameters.getParams().getInt("filter_type"));
        setF1(scriptParameters.getParams().getDouble("freq1"));
        setF2(scriptParameters.getParams().getDouble("freq2"));
        setMethod(scriptParameters.getParams().getInt("construction_method"));
        setfilterOrder(scriptParameters.getParams().getInt("filter_order"));
        setEpsilon(scriptParameters.getParams().getDouble("epsilon"));
        setRs(scriptParameters.getParams().getDouble("rs"));
	}
	
	protected void storeParamsFromGUI() throws ParserException {
		scriptParameters.storeInputImage(srcImage);	
		scriptParameters.storeImageInRecorder(transformImage);
		scriptParameters.storeImageInRecorder(inverseImage);
		scriptParameters.getParams().put(ParameterFactory.newParameter("filter_type", filterType));
        scriptParameters.getParams().put(ParameterFactory.newParameter("freq1", f1));
        scriptParameters.getParams().put(ParameterFactory.newParameter("freq2", f2));
        scriptParameters.getParams().put(ParameterFactory.newParameter("construction_method", constructionMethod));
        scriptParameters.getParams().put(ParameterFactory.newParameter("filter_order", filterOrder));
        scriptParameters.getParams().put(ParameterFactory.newParameter("epsilon", epsilon));
        scriptParameters.getParams().put(ParameterFactory.newParameter("rs", rs));
	}
}
