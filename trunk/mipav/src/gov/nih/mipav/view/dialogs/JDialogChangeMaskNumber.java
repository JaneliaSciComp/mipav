package gov.nih.mipav.view.dialogs;




import gov.nih.mipav.view.MipavUtil;

import java.awt.*;
import java.awt.event.*;
import java.util.*;

import javax.swing.*;




/**
 * This is a dialog that allows the user to change the mask number on the buttons
 *
 * @author   Nish Pandya
 *
 *   
 **/
public class JDialogChangeMaskNumber extends JDialogBase {
	
	  /** this is the text in the Dialog */
    private JLabel changeLabel;
    
    /** this is the input for the user to enter what number they want to change to*/
    private JTextField numberField;
    
    /** this is the action button to change */
    private JButton changeButton;
    
    /** this is a ref to the source button so that the text on the button can be changed */
    private JButton srcButton;
    
    /** this is a ref to the corresponding button becasue they need to be in sync */
    private JButton correspButton;
    
    /** this is the array list of the texts on the buttons */
    private ArrayList btArrayList;
    
    /** this represents the current index of the btArrayList */
    private int currentMaskNumberIndex;
    
  

	public void actionPerformed(ActionEvent e) {
		String command = e.getActionCommand();
		if(command.equals("change mask number")) {
			for(Iterator iter = btArrayList.iterator();iter.hasNext();) {
				Integer num = (Integer)iter.next();
				int numCheck = num.intValue();
				if(((Integer.valueOf(srcButton.getText())).intValue()) == numCheck) {
					currentMaskNumberIndex =btArrayList.indexOf(num);
				}
				try {
					if(((Integer.valueOf(numberField.getText())).intValue()) == numCheck) {
						MipavUtil.displayError("Must enter a unique numeric value less than 255");
						return;
					}
					if (((Integer.valueOf(numberField.getText())).intValue()) > 255) {
						MipavUtil.displayError("Must enter a unique numeric value less than 255");
						return;
					}
				}
				catch(NumberFormatException nfe) {
					MipavUtil.displayError("Must enter a unique numeric value less than 255");
					return;
				}
			}
			srcButton.setText(numberField.getText());
			correspButton.setText(numberField.getText());
			btArrayList.set(currentMaskNumberIndex,new Integer(numberField.getText()));
			dispose();
		}

	}
	

	
	public JDialogChangeMaskNumber(JButton theSrcButton, JButton theCorrespButton, ArrayList buttonTextArrayList) {
		init(theSrcButton,theCorrespButton,buttonTextArrayList);
	}
	
	public void init(JButton theSrcButton, JButton theCorrespButton, ArrayList buttonTextArrayList) {
		
		srcButton = theSrcButton;
		correspButton = theCorrespButton;
		
		setTitle("Change Mask Number");
		
		btArrayList = buttonTextArrayList;
		
		changeLabel = new JLabel("Change Mask Number from " + theSrcButton.getText() + " to : ");
		changeLabel.setForeground(Color.black);
		changeLabel.setFont(serif12);
		changeLabel.setToolTipText("Specify the new mask number");
		
		numberField = new JTextField(3);
		numberField.setFont(serif12);
		
		changeButton = new JButton("Change");
		changeButton.addActionListener(this);
		changeButton.setActionCommand("change mask number");
		
		getContentPane().setLayout(new FlowLayout());
		getContentPane().add(changeLabel);
		getContentPane().add(numberField);
		getContentPane().add(changeButton);
		
		
	}
	
	

}
