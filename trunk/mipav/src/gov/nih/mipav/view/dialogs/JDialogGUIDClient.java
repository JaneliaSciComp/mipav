package gov.nih.mipav.view.dialogs;


import gov.nih.mipav.model.file.*;
import gov.nih.mipav.model.srb.SRBFileTransferer;
import gov.nih.mipav.model.structures.*;


import gov.nih.mipav.view.*;
import gov.nih.mipav.view.srb.*;
import gov.nih.mipav.view.components.*;

import java.awt.*;
import java.awt.event.*;

import java.io.*;

import javax.swing.*;
import javax.swing.event.*;

import edu.sdsc.grid.io.*;
import edu.sdsc.grid.io.local.*;
import edu.sdsc.grid.io.srb.*;

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


    //~ Instance fields ------------------------------------------------------------------------------------------------

	private JCheckBox doubleEntryBox;
	
	private JTextField [] mainFields;
	private JTextField [] secondaryFields;
	
	JPanel mainPanel;
    GridBagConstraints gbc;
	
	private static final int NUM_FIELDS = 17;
	
	private String [] fieldNames = new String[] { 
			"Social Security Number",
			"Complete legal given name of subject at birth",
			"Complete legal family name of subject at birth",
			"Complete additional legal name or names at birth, if any, such as middle name",
			"Day of month of birth",
			"Month of birth",
			"Year of birth",
			"Physical sex of subject at birth [M/F]",
			"Name of city/municipality in which subject was born",
			"Mother's complete legal given name at birth",
			"Mother's complete legal family name at birth",
			"Father's complete legal given name at birth",
			"Father's complete legal family name at birth",
			"Mother's day of month of birth",
			"Mother's month of birth",
			"Father's day of month of birth",
			"Father's month of birth"
			};
	
	
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

        if (command.equals("Next")) {
        	
        }
    }

    public void itemStateChanged(ItemEvent e) {
    	if (e.getSource().equals(doubleEntryBox)) {
    		setDoubleDataFields(doubleEntryBox.isSelected());
    	}
    }
    
    private void setDoubleDataFields(boolean doDouble) {
    	if (doDouble) {
    		setVisible(false);
    		
    		secondaryFields = new JTextField[NUM_FIELDS];
    		
    		ActionMap am;
    		for (int i = 0; i < NUM_FIELDS; i++) {
            	gbc.gridy = i + 1;
            	gbc.gridx = 2;
            	secondaryFields[i] = WidgetFactory.buildTextField("");
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
     * DOCUMENT ME!
     */
    private void init() {
        setTitle("GUID Client");

        mainFields = new JTextField[NUM_FIELDS];
        
        mainPanel = new JPanel(new GridBagLayout());
        gbc = new GridBagConstraints();
        gbc.anchor = GridBagConstraints.WEST;
        gbc.fill = GridBagConstraints.BOTH;
       
        gbc.gridx = 0;
        doubleEntryBox = WidgetFactory.buildCheckBox("Double data entry", false, this);
        
        mainPanel.add(doubleEntryBox, gbc);
        
        gbc.weighty = 1;
    	gbc.gridwidth = 1;
        gbc.gridheight =1;
    	
        ActionMap am;
        Insets insets1 = new Insets(0, 10, 0, 0);
        Insets insets2 = new Insets(0, 0, 0, 0);
        
        gbc.insets = insets1;
        for (int i = 0; i < NUM_FIELDS; i++) {
        	gbc.gridy = i + 1;
        	gbc.gridx = 0;
        	gbc.weightx = .5;
        	gbc.insets = insets1;
        	mainPanel.add(WidgetFactory.buildLabel(fieldNames[i]), gbc);
        	
        	gbc.weightx = 1;
        	gbc.gridx++;
        	gbc.insets = insets2;
        	
        	mainFields[i] = WidgetFactory.buildTextField("");
        	am = mainFields[i].getActionMap();
        	am.get("copy").setEnabled(false);
            am.get("copy-to-clipboard").setEnabled(false);
        	
        	mainPanel.add(mainFields[i], gbc);
        }
        
        getContentPane().add(mainPanel, BorderLayout.CENTER);
        
    }  
    
}
