package gov.nih.mipav.view.dialogs;

import java.awt.*;
import java.awt.event.*;

import javax.swing.*;

/**
 * <p>Title:</p>
 *
 * <p>Description:</p>
 *
 * <p>Copyright: Copyright (c) 2004</p>
 *
 * <p>Company:</p>
 *
 * @author   not attributable
 * @version  1.0
 */

public class JDialogGUIDClient extends JDialog implements ActionListener, ItemListener {

    //~ Static fields/initializers -------------------------------------------------------------------------------------
	//private static final long serialVersionUID = -6802105487936807310L;

    //~ Instance fields ------------------------------------------------------------------------------------------------

	private JCheckBox doubleEntryBox;
	
	private JTextField guidField;
	private JTextField [] mainFields;
	private JTextField [] secondaryFields;
	
	JPanel mainPanel;
    GridBagConstraints gbc;
	
    private JButton helpButton, cancelButton, OKButton;
    
	private static final int NUM_FIELDS = 17;
	
	private String [] fieldNames = new String[] { 
			"Social Security Number [###-##-####]",
			"Complete legal given name of subject at birth",
			"Complete legal family name of subject at birth",
			"Complete additional legal name or names at birth, if any, such as middle name",
			"Day of month of birth [1-31]",
			"Month of birth [1-12]",
			"Year of birth [####]",
			"Physical sex of subject at birth [M/F]",
			"Name of city/municipality in which subject was born",
			"Mother's complete legal given name at birth",
			"Mother's complete legal family name at birth",
			"Father's complete legal given name at birth",
			"Father's complete legal family name at birth",
			"Mother's day of month of birth [1-31]",
			"Mother's month of birth [1-12]",
			"Father's day of month of birth [1-31]",
			"Father's month of birth [1-12]"
			};
	
	//private static final int GUID_SSN = 0;
	//private static final int GUID_FN  = 1;
	//private static final int GUID_LN =  2;
	//private static final int GUID_MN =  3;
	//private static final int GUID_DOB = 4;
	//private static final int GUID_MOB = 5;
	//private static final int GUID_YOB = 6;
	//private static final int GUID_SEX = 7;
	//private static final int GUID_COB = 8;
	//private static final int GUID_MFN = 9;
	//private static final int GUID_MLN = 10;
	//private static final int GUID_FFN = 11;
	//private static final int GUID_FLN = 12;
	//private static final int GUID_MDOB = 13;
	//private static final int GUID_MMOB = 14;
	//private static final int GUID_FDOB = 15;
	//private static final int GUID_FMOB = 16;
	
    //~ Constructors ---------------------------------------------------------------------------------------------------

  
    public JDialogGUIDClient(Frame theParentFrame) {
        super(theParentFrame, false);
        
        init();
        setSize(new Dimension(600,500));
       
        setVisible(true);
        
        validate();
    }

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * DOCUMENT ME!
     *
     * @param  e  DOCUMENT ME!
     */
    public void actionPerformed(ActionEvent e) {

        /**
         * @todo  Implement this java.awt.event.ActionListener abstract method
         */

        String command = e.getActionCommand();

        if (command.equals("OK")) {
        	if (setVariables()) {
        		
        	}
        }
    }

    public void itemStateChanged(ItemEvent e) {
    	if (e.getSource().equals(doubleEntryBox)) {
    		setDoubleDataFields(doubleEntryBox.isSelected());
    	}
    }
    
    /**
     * Checks the fields to see they are properly formed/entered
     * @return whether the dialog is ready to request GUID 
     */
    private boolean setVariables() {
    	
    	if (doubleEntryBox.isSelected()) {
    		
    		boolean mismatch = false;
    		for (int i = 0; i < NUM_FIELDS; i++) {
    			if (!mainFields[i].getText().equals(secondaryFields[i].getText())) {
    				mismatch = true;
    				mainFields[i].setText("");
    				secondaryFields[i].setText("");
    			}
    		}
    		if (mismatch) {
    			JOptionPane.showMessageDialog(null, "Field mismatch, re-enter information", 
    	        		   "Error", JOptionPane.ERROR_MESSAGE);
    			return false;
    		}
    		
    	} 
    	
    	for (int i = 0; i < NUM_FIELDS; i++) {
    		if (mainFields[i].getText().equals("")) {
    			JOptionPane.showMessageDialog(null, "Missing information", 
 	        		   "Error", JOptionPane.ERROR_MESSAGE);
    			mainFields[i].requestFocus();
    			return false;
    		}
    	}
    	
    	//String ssn = mainFields[GUID_SSN].getText();
    	//String fn = mainFields[GUID_FN].getText();
    	//String ln = mainFields[GUID_LN].getText();
    	//String mn = mainFields[GUID_MN].getText();
    	
    	return true;
    }
    
    /**
     * Sets the dialog to use/not use double data entry (re-type each field)
     * @param doDouble whether to use double data entry
     */
    private void setDoubleDataFields(boolean doDouble) {
    	if (doDouble) {
    		setVisible(false);
           JOptionPane.showMessageDialog(null, "Copy/paste functions have been disabled", 
        		   "Information", JOptionPane.INFORMATION_MESSAGE);
           
    		secondaryFields = new JTextField[NUM_FIELDS];
    		gbc.insets = new Insets(0, 0, 0, 10);
    		gbc.weightx = 1;
    		gbc.fill = GridBagConstraints.HORIZONTAL;
    		ActionMap am;
    		for (int i = 0; i < NUM_FIELDS; i++) {
            	gbc.gridy = i + 1;
            	gbc.gridx = 2;
            	secondaryFields[i] = new JTextField("");
            	secondaryFields[i].setForeground(Color.black);
            	
            	am = secondaryFields[i].getActionMap();
            	am.get("paste").setEnabled(false);
                am.get("paste-from-clipboard").setEnabled(false);
            	
            	mainPanel.add(secondaryFields[i], gbc);
            }
    		
    		setSize(new Dimension(800,500));
    		setVisible(true);
    	} else {
    		
    		setVisible(false);
    		
    		for (int i = 0; i < NUM_FIELDS; i++) {
    			mainPanel.remove(secondaryFields[i]);
    			secondaryFields[i] = null;
    		}
    		secondaryFields = null;
    		setSize(new Dimension(600,500));
    		setVisible(true);
    		
    	}
    }
    
    /**
     * Initialize the GUI (defaults to NOT using double data entry)
     *
     */
    private void init() {
        setTitle("GUID Client");

        mainFields = new JTextField[NUM_FIELDS];
        
        mainPanel = new JPanel(new GridBagLayout());
        gbc = new GridBagConstraints();
        gbc.anchor = GridBagConstraints.WEST;
        gbc.fill = GridBagConstraints.BOTH;
       
        gbc.gridx = 0;
        doubleEntryBox = new JCheckBox("Double data entry");
        doubleEntryBox.setSelected(false);
        doubleEntryBox.addItemListener(this);
        
        mainPanel.add(doubleEntryBox, gbc);
        
        gbc.weighty = 1;
    	gbc.gridwidth = 1;
        gbc.gridheight =1;
    	
        ActionMap am;
        Insets insets1 = new Insets(0, 10, 0, 0);
        Insets insets2 = new Insets(0, 0, 0, 10);
        
        gbc.insets = insets1;
        for (int i = 0; i < NUM_FIELDS; i++) {
        	gbc.gridy = i + 1;
        	gbc.gridx = 0;
        	gbc.weightx = 0;
        	gbc.insets = insets1;
        	mainPanel.add(new JLabel(fieldNames[i]), gbc);
        	
        	gbc.weightx = .5;
        	gbc.gridx++;
        	gbc.insets = insets2;
        	
        	mainFields[i] = new JTextField("");
        	mainFields[i].setForeground(Color.black);
        	
        	am = mainFields[i].getActionMap();
        	am.get("copy").setEnabled(false);
            am.get("copy-to-clipboard").setEnabled(false);
        	
        	mainPanel.add(mainFields[i], gbc);
        }
        
        gbc.gridy++;
        gbc.gridx = 0;
        gbc.weightx = 0;
        gbc.insets = insets1;
        
        mainPanel.add(new JLabel("GUID"), gbc);
        
        gbc.weightx = .5;
    	gbc.gridx++;
        gbc.insets = insets2;
        
        guidField = new JTextField(10);
        guidField.setEditable(false);
        mainPanel.add(guidField, gbc);
        
        OKButton = new JButton("OK");
        OKButton.addActionListener(this);

        
        helpButton = new JButton("Help");
        helpButton.addActionListener(this);

        
        cancelButton = new JButton("Cancel");
        cancelButton.addActionListener(this);

        JPanel buttonPanel = new JPanel();
        buttonPanel.add(OKButton);
        buttonPanel.add(cancelButton);
        buttonPanel.add(helpButton);
        
        getContentPane().add(mainPanel, BorderLayout.CENTER);
        getContentPane().add(buttonPanel, BorderLayout.SOUTH);
    }  
    
}
