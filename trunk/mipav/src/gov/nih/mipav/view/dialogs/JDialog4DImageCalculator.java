package gov.nih.mipav.view.dialogs;

import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.Frame;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.Insets;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.ItemEvent;
import java.awt.event.ItemListener;

import javax.swing.ButtonGroup;
import javax.swing.JComboBox;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JRadioButton;
import javax.swing.event.ChangeEvent;
import javax.swing.event.ChangeListener;

import gov.nih.mipav.model.algorithms.AlgorithmBase;
import gov.nih.mipav.model.algorithms.AlgorithmInterface;
import gov.nih.mipav.model.algorithms.utilities.Algorithm4DImageCalculator;
import gov.nih.mipav.model.algorithms.utilities.AlgorithmConcatMult3Dto4D;
import gov.nih.mipav.model.scripting.ParserException;
import gov.nih.mipav.model.structures.ModelImage;
import gov.nih.mipav.model.structures.ModelStorageBase;
import gov.nih.mipav.view.MipavUtil;
import gov.nih.mipav.view.ViewJFrameImage;
import gov.nih.mipav.view.ViewUserInterface;


/**
 * @author pandyan
 * This is the dialog for the 4D Image Calculator
 *
 * */
public class JDialog4DImageCalculator extends JDialogScriptableBase implements AlgorithmInterface, ItemListener {
	
	/** images **/
	private ModelImage image, destImage;
	
	/** combo box of operations **/
	private JComboBox comboBoxOperator;
	
	/** clip or promote radio button options **/
	private JRadioButton radioClip, radioPromote;
	
	/** handle to alg **/
	private Algorithm4DImageCalculator alg;
	
	
	
	
	
	/**
     * Empty constructor needed for dynamic instantiation (used during scripting).
     */
    public JDialog4DImageCalculator() { }

    /**
     * Creates new image calculator dialog and displays.
     *
     * @param  theParentFrame  Parent frame.
     * @param  im              Source image.
     */
    public JDialog4DImageCalculator(Frame theParentFrame, ModelImage im) {
        super(theParentFrame, false);
        image = im;
        init();
    }
	
	
    /**
     * init
     */
	private void init() {
		setForeground(Color.black);
        setTitle("4D Image math");

        JPanel mainPanel = new JPanel(new GridBagLayout());
        mainPanel.setForeground(Color.black);
        
        
        JLabel labelOperator = new JLabel("Operator:");
        labelOperator.setForeground(Color.black);
        labelOperator.setFont(serif12);

        comboBoxOperator = new JComboBox();
        comboBoxOperator.setFont(serif12);
        comboBoxOperator.setBackground(Color.white);

        comboBoxOperator.addItem("Add");
        comboBoxOperator.addItem("Average");
        comboBoxOperator.addItem("Minimum");
        comboBoxOperator.addItem("Maximum");
        if ((image.getType() != ModelStorageBase.COMPLEX) && (image.getType() != ModelStorageBase.DCOMPLEX)) {
            comboBoxOperator.addItem("L2 Norm");
            comboBoxOperator.addItem("Standard Deviation");
        }

        comboBoxOperator.addItemListener(this);
        comboBoxOperator.addActionListener(this);
        ButtonGroup group = new ButtonGroup();
        radioClip = new JRadioButton("Clip", true);
        radioClip.setFont(serif12);
        
        group.add(radioClip);
        
        radioPromote = new JRadioButton("Promote destination image type", false);
        radioPromote.setFont(serif12);
        group.add(radioPromote);
        
        if (image.getType() == ModelStorageBase.DOUBLE || image.getType() == ModelStorageBase.ARGB_FLOAT ||
    			image.getType() == ModelStorageBase.DCOMPLEX) { 
    	    radioClip.setEnabled(false);
    	    radioPromote.setEnabled(false);
        }
        
        JPanel OKCancelPanel = new JPanel();
        buildOKButton();
        OKCancelPanel.add(OKButton);
        buildCancelButton();
        OKCancelPanel.add(cancelButton);
        
        GridBagConstraints gbc = new GridBagConstraints();
        gbc.gridx = 0;
        gbc.gridy = 0;
        gbc.weightx = 0;
        gbc.weighty = 0;
        gbc.fill = GridBagConstraints.BOTH;
        gbc.anchor = GridBagConstraints.CENTER;
        gbc.insets = new Insets(5, 5, 5, 5);
        mainPanel.add(labelOperator, gbc);
        
        gbc.gridx = 1;
        gbc.gridy = 0;
        gbc.weightx = 1;
        gbc.weighty = 0;
        gbc.fill = GridBagConstraints.BOTH;
        gbc.anchor = GridBagConstraints.CENTER;
        gbc.insets = new Insets(5, 5, 5, 5);
        mainPanel.add(comboBoxOperator, gbc);
        
        gbc.gridx = 0;
        gbc.gridy = 1;
        gbc.weightx = 1;
        gbc.weighty = 1;
        gbc.fill = GridBagConstraints.BOTH;
        gbc.anchor = GridBagConstraints.WEST;
        gbc.gridwidth = 2;
        gbc.insets = new Insets(5, 5, 5, 5);
        mainPanel.add(radioClip, gbc);
        
        gbc.gridx = 0;
        gbc.gridy = 2;
        gbc.weightx = 1;
        gbc.weighty = 1;
        gbc.fill = GridBagConstraints.BOTH;
        gbc.anchor = GridBagConstraints.WEST;
        gbc.gridwidth = 2;
        gbc.insets = new Insets(5, 5, 5, 5);
        mainPanel.add(radioPromote, gbc);

        getContentPane().add(mainPanel);
        getContentPane().add(OKCancelPanel, BorderLayout.SOUTH);
        pack();
		setMinimumSize(this.getSize());
		setVisible(true);

	}

	/**
	 * action performed
	 */
	public void actionPerformed(ActionEvent e) {
		String command = e.getActionCommand();
		if(command.equalsIgnoreCase("ok")) {
			callAlgorithm();
		}else if(command.equalsIgnoreCase("cancel")) {
			dispose();
		}

	}
	
	/**
	 * call algorithm
	 */
	protected void callAlgorithm() {
		int[] extents = new int[3];
		extents[0] = image.getExtents()[0];
		extents[1] = image.getExtents()[1];
		extents[2] = image.getExtents()[2];
		
		int operationType = -1;
		String operation = (String)comboBoxOperator.getSelectedItem();

		if(operation.equals("Add")) {
			operationType = Algorithm4DImageCalculator.ADD;
		}else if(operation.equals("Average")) {
			operationType = Algorithm4DImageCalculator.AVERAGE;
		}else if(operation.equals("Minimum")) {
			operationType = Algorithm4DImageCalculator.MINIMUM;
		}else if(operation.equals("Maximum")) {
			operationType = Algorithm4DImageCalculator.MAXIMUM;
		}else if(operation.equals("Standard Deviation")) {
			operationType = Algorithm4DImageCalculator.STDDEV;
		}else if(operation.equals("L2 Norm")) {
			operationType = Algorithm4DImageCalculator.NORM;
		}
		
		boolean doClip = radioClip.isSelected();
		
		int type = image.getType();
		
		if((operationType == Algorithm4DImageCalculator.ADD && !doClip) || (operationType == Algorithm4DImageCalculator.NORM && !doClip)) {
			//this means promote
			if(type == ModelStorageBase.BYTE) {
				type = ModelStorageBase.SHORT;
			}else if (type == ModelStorageBase.UBYTE) {
				type = ModelStorageBase.SHORT;
			}else if(type == ModelStorageBase.SHORT) {
				type = ModelStorageBase.INTEGER;
			}else if (type == ModelStorageBase.USHORT) {
				type = ModelStorageBase.INTEGER;
			}else if(type == ModelStorageBase.INTEGER) {
				type = ModelStorageBase.LONG;
			} else if(type == ModelStorageBase.UINTEGER) {
				type = ModelStorageBase.LONG;
			}else if (type == ModelStorageBase.LONG) {
				type = ModelStorageBase.DOUBLE;
			}else if(type == ModelStorageBase.FLOAT) {
				type = ModelStorageBase.DOUBLE;
			}else if (type == ModelStorageBase.COMPLEX) {
				type = ModelStorageBase.DCOMPLEX;
			}else if (type == ModelStorageBase.ARGB) {
				type = ModelStorageBase.ARGB_USHORT;
			}else if (type == ModelStorageBase.ARGB_USHORT) {
				type = ModelStorageBase.ARGB_FLOAT;
			}
		}
		
		
		destImage = new ModelImage(type, extents, makeImageName(image.getImageName(), "_calc"));
		
		alg = new Algorithm4DImageCalculator(image, destImage, operationType, doClip);
		
		 alg.addListener(this);

         createProgressBar("", alg);
         
         if (isRunInSeparateThread()) {

             // Start the thread as a low priority because we wish to still have user interface work fast.
             if (alg.startMethod(Thread.MIN_PRIORITY) == false) {
                 MipavUtil.displayError("A thread is already running on this object");
             }
         } else {
             alg.run();
         }
	}

	

	/**
	 * algorithm performed
	 */
	public void algorithmPerformed(AlgorithmBase algorithm) {
		if(algorithm instanceof Algorithm4DImageCalculator) {
			if (alg.isCompleted() == true) {
				new ViewJFrameImage(destImage);
				dispose();
			}
		}

	}
	
	/**
	 * set gui
	 */
	protected void setGUIFromParams() {
		// TODO Auto-generated method stub

	}

	/**
	 * store params
	 */
	protected void storeParamsFromGUI() throws ParserException {
		// TODO Auto-generated method stub

	}

	
	/**
	 * item state changed
	 */
	 public void itemStateChanged(ItemEvent event) {
	        Object source = event.getSource();
	        if (source == comboBoxOperator) {
	            int index = comboBoxOperator.getSelectedIndex();
	            if((index == 0 || index == 4) && 
	            	(image.getType() != ModelStorageBase.DOUBLE && image.getType() != ModelStorageBase.ARGB_FLOAT &&
	            			image.getType() != ModelStorageBase.DCOMPLEX)) { 
	            	radioClip.setEnabled(true);
	            	radioPromote.setEnabled(true);
	            }else {
	            	radioClip.setEnabled(false);
	            	radioPromote.setEnabled(false);
	            }
	            
	        }
	        
	 }

}
