package gov.nih.mipav.view.dialogs;




import gov.nih.mipav.model.structures.ModelImage;
import gov.nih.mipav.model.structures.ModelLUT;
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
    private ArrayList<Integer> btArrayList;
    
    /** this represents the current index of the btArrayList */
    private int currentMaskNumberIndex;
    
    /** re f to Color array **/
    private Color[] color;
    
    /** lutB **/
    private ModelLUT lutB;
    
    /** ref to image **/
    private ModelImage image;
    
    /** ref to IntensityLockVector **/
    private Vector<Integer> intensityLockVector;
    
    /** src button text number **/
    private int srcValue;
    
  

	public void actionPerformed(ActionEvent e) {
		String command = e.getActionCommand();
		if(command.equals("change mask number")) {
			for(Iterator<Integer> iter = btArrayList.iterator();iter.hasNext();) {
				Integer num = (Integer)iter.next();
				int numCheck = num.intValue();
				try {
					if(((Integer.valueOf(numberField.getText())).intValue()) == numCheck) {
						MipavUtil.displayError("Must enter a unique numeric value less than 255");
						return;
					}
					if (((Integer.valueOf(numberField.getText())).intValue()) > 255) {
						MipavUtil.displayError("Must enter a numeric value less than 255");
						return;
					}
				}
				catch(NumberFormatException nfe) {
					MipavUtil.displayError("Must enter a unique numeric value less than 255");
					return;
				}
			}

			int newValue = (new Integer(numberField.getText())).intValue();
			//since the color array starts at 1
			color[currentMaskNumberIndex] = lutB.getColor(newValue);
			srcButton.setText(numberField.getText());
			correspButton.setText(numberField.getText());
			srcButton.setBackground(color[currentMaskNumberIndex]);
			correspButton.setBackground(color[currentMaskNumberIndex]);
			btArrayList.set(currentMaskNumberIndex - 1,new Integer(numberField.getText()));
			image.getParentFrame().getComponentImage().setIntensityDropper((float) newValue);
            image.getParentFrame().getControls().getTools().setPaintColor(color[currentMaskNumberIndex]);
            image.getParentFrame().getComponentImage().updatePaintBrushCursor();
            //retrieve the mask
            BitSet obj = image.getParentFrame().getComponentImage().getPaintMask();
            refreshImagePaint(image, obj);
            
            //now that the number has been changed....we need to update the intensityLockVector in case the old num was in it...if it was remove it and add the new num

            
            if(intensityLockVector != null) {
	            for (int i = 0; i < intensityLockVector.size(); i++) {
	
	                try {
	                    Integer lockedIntensity = (Integer) intensityLockVector.elementAt(i);
	
	                    if ((lockedIntensity != null) && (lockedIntensity.intValue() == srcValue)) {
	                    	intensityLockVector.removeElementAt(i);
	                    	Integer intensityLockInteger = new Integer(newValue);
	                    	 intensityLockVector.add(intensityLockInteger);
	                    }
	                } catch (Exception ex) {
	                    continue;
	                }
	            }
            }


			dispose();
		} else {
            super.actionPerformed(e);
        }

	}
	

	
	public JDialogChangeMaskNumber(JButton theSrcButton, JButton theCorrespButton, ArrayList<Integer> buttonTextArrayList, Color[] color, ModelLUT lutB, ModelImage image, int selected, Vector<Integer> theIntensityLockVector) {
		init(theSrcButton,theCorrespButton,buttonTextArrayList,color,lutB,image,selected, theIntensityLockVector);
	}
	
	public void init(JButton theSrcButton, JButton theCorrespButton, ArrayList<Integer> buttonTextArrayList, Color[] color, ModelLUT lutB, ModelImage image, int selected, Vector<Integer> theIntensityLockVector) {
		
		srcButton = theSrcButton;
		srcValue = Integer.parseInt(srcButton.getText());
		correspButton = theCorrespButton;
		this.color = color;
		this.lutB = lutB;
		this.image = image;
		currentMaskNumberIndex = selected;
		intensityLockVector = theIntensityLockVector;
		
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
	
	
	/**
     * Refreshes the displayed paint mask.
     *
     * @param  img  the image
     * @param  obj  mask
     */
    private void refreshImagePaint(ModelImage img, BitSet obj) {

        // replace it by previous
        img.getParentFrame().getComponentImage().setPaintMask(obj);
        img.setMask(obj);

        // show result
        img.getParentFrame().updateImages(true);

        if (img.getTriImageFrame() != null) {
            img.getTriImageFrame().getTriImage(0).setPaintMask(obj);
            img.getTriImageFrame().getTriImage(1).setPaintMask(obj);
            img.getTriImageFrame().getTriImage(2).setPaintMask(obj);
            img.getTriImageFrame().updateImages(true);
        }
    }
	
	

}
