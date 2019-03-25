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
public class JDialogDiscreteSineTransform extends JDialogScriptableBase implements AlgorithmInterface{

    //~ Static fields/initializers -------------------------------------------------------------------------------------

    /** Use serialVersionUID for interoperability. */
    //private static final long serialVersionUID;
	private boolean multiProcessor = false;
		
	private ModelImage srcImage;
	
    private ModelImage transformImage;
    
    private ModelImage inverseImage;
    
    private DiscreteSineTransform dsAlgo;
    
    private ButtonGroup processorGroup;
    private JRadioButton singleButton;
    private JRadioButton multiButton;
    
    private int filterType;
    
    private double filterVal1;
    
    private double filterVal2;
    
    private JComboBox<String> comboBoxFilterType;
    private JLabel labelVal1;
    private JTextField textVal1;
    private JLabel labelVal2;
    private JTextField textVal2;
    
    private final int FILTER_NONE = 0;
	private final int FILTER_SOFT = 1;
	private final int FILTER_NN_GARROTE = 2;
	private final int FILTER_HARD = 3;
	private final int FILTER_GREATER = 4;
	private final int FILTER_LESS = 5;
	private final int FILTER_THRESHOLD_FIRM = 6;
	
	/**
     * Empty constructor needed for dynamic instantiation.
     */
    public JDialogDiscreteSineTransform() { }

    /**
     * Construct the Discrete Sine Transform dialog.
     *
     * @param  theParentFrame  Parent frame.
     * @param  im              Source image.
     */
    public JDialogDiscreteSineTransform(Frame theParentFrame, ModelImage im) {
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
        } else if (source == comboBoxFilterType) {
        	String selection = (String)comboBoxFilterType.getSelectedItem();
			if (selection.equals("THRESHOLD_FIRM")) {
				labelVal1.setText("Low value");
				labelVal2.setText("High value");
			}
			else {
				labelVal1.setText("Value");
				labelVal2.setText("Substitute");
			}
			if (selection.equals("NONE")) {
				labelVal1.setEnabled(false);
				labelVal2.setEnabled(false);
				textVal1.setEnabled(false);
				textVal2.setEnabled(false);
			}
			else {
				labelVal1.setEnabled(true);
				labelVal2.setEnabled(true);
				textVal1.setEnabled(true);
				textVal2.setEnabled(true);	
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

        setTitle("Discrete Sine Transform");
        getContentPane().setLayout(new BorderLayout());
        JPanel paramsPanel = new JPanel(new GridBagLayout());
        paramsPanel.setBorder(buildTitledBorder("Transform parameters"));
        
        GridBagConstraints gbc = new GridBagConstraints();
        gbc.gridwidth = 1;
        gbc.anchor = GridBagConstraints.WEST;
        gbc.weightx = 1;
        
        processorGroup = new ButtonGroup();
        singleButton = new JRadioButton("Single processor", true);
        singleButton.setFont(serif12);
        processorGroup.add(singleButton);
        gbc.gridx = 0;
        gbc.gridy = 0;
        paramsPanel.add(singleButton, gbc);
        
        multiButton = new JRadioButton("Multiple processors", false);
        multiButton.setFont(serif12);
        processorGroup.add(multiButton);
        gbc.gridx = 0;
        gbc.gridy = 1;
        paramsPanel.add(multiButton, gbc);
        
        comboBoxFilterType = buildFilterTypeComboBox();
        gbc.gridx = 0;
        gbc.gridy = 2;
        JLabel labelFilter = new JLabel("Coefficients filter");
        labelFilter.setFont(serif12);
        labelFilter.setForeground(Color.black);
        paramsPanel.add(labelFilter, gbc);
        gbc.gridx = 1;
        paramsPanel.add(comboBoxFilterType,gbc);
        
        labelVal1 = new JLabel("Value");
        labelVal1.setFont(serif12);
        labelVal1.setForeground(Color.black);
        labelVal1.setEnabled(false);
        gbc.gridx = 0;
        gbc.gridy = 3;
        paramsPanel.add(labelVal1, gbc);
        
        textVal1 = new JTextField(10);
        textVal1.setFont(serif12);
        textVal1.setForeground(Color.black);
        textVal1.setEnabled(false);
        gbc.gridx = 1;
        paramsPanel.add(textVal1, gbc);
        
        labelVal2 = new JLabel("Substitute");
        labelVal2.setFont(serif12);
        labelVal2.setForeground(Color.black);
        labelVal2.setEnabled(false);
        gbc.gridx = 0;
        gbc.gridy = 4;
        paramsPanel.add(labelVal2, gbc);
        
        textVal2 = new JTextField(10);
        textVal2.setFont(serif12);
        textVal2.setForeground(Color.black);
        textVal2.setEnabled(false);
        gbc.gridx = 1;
        paramsPanel.add(textVal2, gbc);
           
        getContentPane().add(paramsPanel, BorderLayout.CENTER);
        getContentPane().add(buildButtons(), BorderLayout.SOUTH);
        pack();
        setResizable(true);

        System.gc();
    }
    
    private JComboBox<String> buildFilterTypeComboBox() {
    	final JComboBox<String> comboBox = new JComboBox<String>();
    	comboBox.setFont(serif12);
        comboBox.setBackground(Color.white);
        comboBox.addItem("NONE");
        comboBox.addItem("SOFT");
        comboBox.addItem("NN_GARROTE");
        comboBox.addItem("HARD");
        comboBox.addItem("GREATER");
        comboBox.addItem("LESS");
        comboBox.addItem("THRESHOLD_FIRM");
        comboBox.setSelectedIndex(0);
        comboBox.addActionListener(this);
    	return comboBox;
    }
    
    private boolean setVariables() {
    	String tmpStr;
        multiProcessor = multiButton.isSelected();
        
    	String selection = (String)comboBoxFilterType.getSelectedItem();
    	if (selection.equals("NONE")) {
    		filterType = FILTER_NONE;
    	}
    	else if (selection.equals("SOFT")) {
    		filterType = FILTER_SOFT;
    	}
    	else if (selection.equals("NN_GARROTE")) {
    		filterType = FILTER_NN_GARROTE;
    	}
    	else if (selection.equals("HARD")) {
    	    filterType = FILTER_HARD;
    	}
    	else if (selection.equals("GREATER")) {
    	    filterType = FILTER_GREATER;
    	}
    	else if (selection.equals("LESS")) {
    	    filterType = FILTER_LESS;
    	}
    	else if (selection.equals("THRESHOLD_FIRM")) {
    	    filterType = FILTER_THRESHOLD_FIRM;
    	}
    	
    	if (filterType != FILTER_NONE) {
    		tmpStr = textVal1.getText();	
    		try {
    			filterVal1 = Double.valueOf(tmpStr).doubleValue();
    		}
    		catch (NumberFormatException e) {
    		    MipavUtil.displayError("textval1 does not have a proper number");
    		    textVal1.requestFocus();
    		    textVal1.selectAll();
    		    return false;
    		}
    		
    		tmpStr = textVal2.getText();	
    		try {
    			filterVal2 = Double.valueOf(tmpStr).doubleValue();
    		}
    		catch (NumberFormatException e) {
    		    MipavUtil.displayError("textval2 does not have a proper number");
    		    textVal2.requestFocus();
    		    textVal2.selectAll();
    		    return false;
    		}
    	} // if (filterType != FILTER_NONE)
    	
    	if (filterType == FILTER_THRESHOLD_FIRM) {
    		if (filterVal1 < 0.0) {
    			MipavUtil.displayError("filterVal1 must be >= 0");
    			textVal1.requestFocus();
    		    textVal1.selectAll();
    		    return false;
    		}
    		if (filterVal2 < filterVal1) {
    			MipavUtil.displayError("filterVal2 must be >= filterVal1");
    			textVal2.requestFocus();
    		    textVal2.selectAll();
    		    return false;
    		}
    	}
    	return true;
    }
	
	protected void callAlgorithm() {
		try {
			
		    transformImage = new ModelImage(ModelStorageBase.DOUBLE, srcImage.getExtents(), srcImage.getImageName() + "_transform");
			
		    inverseImage = new ModelImage(ModelStorageBase.DOUBLE, srcImage.getExtents(), srcImage.getImageName() + "_inverse");	
		    
		    
		    dsAlgo = new DiscreteSineTransform(transformImage, inverseImage, srcImage, multiProcessor,
		    		filterType, filterVal1, filterVal2);
		    
		    // This is very important. Adding this object as a listener allows the algorithm to
		    // notify this object when it has completed of failed. See algorithm performed event.
		    // This is made possible by implementing AlgorithmedPerformed interface
		    dsAlgo.addListener(this);
		
		
		    createProgressBar(srcImage.getImageName(), dsAlgo);
		
		    // Hide dialog
		    setVisible(false);
		
		    if (isRunInSeparateThread()) {
		
		        // Start the thread as a low priority because we wish to still have user interface work fast.
		        if (dsAlgo.startMethod(Thread.MIN_PRIORITY) == false) {
		            MipavUtil.displayError("A thread is already running on this object");
		        }
		    } else {
		
		        dsAlgo.run();
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
	        MipavUtil.displayError("Dialog Discrete Sine Transform: unable to allocate enough memory");
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

        if (algorithm instanceof DiscreteSineTransform) {
        	
        	if (dsAlgo.isCompleted()) {
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
	
	protected void setGUIFromParams() {
		srcImage = scriptParameters.retrieveInputImage();
		multiProcessor = scriptParameters.getParams().getBoolean("mp");
		filterType = scriptParameters.getParams().getInt("filter_type");
		filterVal1 = scriptParameters.getParams().getDouble("filter_val1");
		filterVal2 = scriptParameters.getParams().getDouble("filter_val2");
	}
	
	protected void storeParamsFromGUI() throws ParserException {
		scriptParameters.storeInputImage(srcImage);	
		scriptParameters.storeImageInRecorder(transformImage);
		scriptParameters.storeImageInRecorder(inverseImage);
		scriptParameters.getParams().put(ParameterFactory.newParameter("mp", multiProcessor));
		scriptParameters.getParams().put(ParameterFactory.newParameter("filter_type", filterType));
		scriptParameters.getParams().put(ParameterFactory.newParameter("filter_val1", filterVal1));
		scriptParameters.getParams().put(ParameterFactory.newParameter("filter_val2", filterVal2));
	}
}
