package gov.nih.mipav.view.dialogs;


import gov.nih.mipav.model.structures.*;

import java.awt.event.*;
import java.awt.*;
import javax.swing.*;


/**
*   Dialog used to determine how the transformation matrix should
*   be adjusted to account for different coordinate systems. Two
*   check boxes are available to modify the data so that the
*
*		@version    , 2001
*		@author     Matthew J. McAuliffe, Ph.D.
*       @see        AlgorithmMask
*
*/
public class JDialogOrientMatrix extends JDialogBase {

    private     ModelImage          image;

    private     JPanel              panelAxisMode;
	private     JCheckBox           worldCoordCheckbox;
	private     JCheckBox           leftHandRuleCheckbox;

	private     TransMatrix         matrix;
	private     JDialogBase    pFrame;

    /**
    *  Constructor
    *  @param parent          parent frame
    *  @param im              source image
    */
	public JDialogOrientMatrix(Frame theParentFrame, JDialogBase _pFrame) {
		super(theParentFrame, true);

        setForeground(Color.black);
        getContentPane().setLayout(new BorderLayout());

        // this.pFrame = _pFrame;
        if ( _pFrame instanceof JDialogTransform ) {
          this.pFrame = (JDialogTransform)_pFrame;
        } else if ( _pFrame instanceof JDialogImageInfo) {
          this.pFrame = (JDialogImageInfo)_pFrame;
        }
        setTitle("Adjust matrix axis");
        panelAxisMode = new JPanel();
        panelAxisMode.setLayout(new BorderLayout());
        panelAxisMode.setForeground(Color.black);
        panelAxisMode.setBorder(buildTitledBorder("Options"));
        getContentPane().add(panelAxisMode);

		worldCoordCheckbox = new JCheckBox("Change to world coordinate system.");
		worldCoordCheckbox.setFont(serif12);
		worldCoordCheckbox.addItemListener(this);
		panelAxisMode.add(worldCoordCheckbox, BorderLayout.NORTH);

		leftHandRuleCheckbox = new JCheckBox("Change to left-hand rule.");
		leftHandRuleCheckbox.setFont(serif12);
		leftHandRuleCheckbox.addItemListener(this);
		panelAxisMode.add(leftHandRuleCheckbox, BorderLayout.SOUTH);

        JPanel buttonPanel = new JPanel();
        buildOKButton();
		buttonPanel.add(OKButton);
        buildCancelButton();
		buttonPanel.add(cancelButton);

		getContentPane().add(buttonPanel, BorderLayout.SOUTH);

		pack();
		setVisible(true);
		setResizable(false);
	}

    /**
    *   Returns the matrix
    *
    */
    public TransMatrix getMatrix() { return matrix;}



   /**
   *  actionPerformed -  Closes dialog box when the OK button is pressed and
   *                     calls the algorithm
   *  @param event       event that triggers function
   */
   public void actionPerformed(ActionEvent event) {
   	  Object source = event.getSource();

      if ( source == OKButton ) {

        if (worldCoordCheckbox.isSelected() && leftHandRuleCheckbox.isSelected()) {
            pFrame.setWCSystem(true);
            pFrame.setLeftHandSystem(true);
            dispose();
        }
        else if (worldCoordCheckbox.isSelected()) {    // Change just to the world coordinate system
            pFrame.setWCSystem(true);
            pFrame.setLeftHandSystem(false);
            dispose();
        }
        else if (leftHandRuleCheckbox.isSelected()) { // Change just to the "left-hand" system
            pFrame.setWCSystem(false);
            pFrame.setLeftHandSystem(true);
            dispose();
        }
        else { // No changes
            pFrame.setWCSystem(false);
            pFrame.setLeftHandSystem(false);
            dispose();
        }
      }
      else if (source == cancelButton) {
        pFrame.setWCSystem(false);
        pFrame.setLeftHandSystem(false);
        dispose();
      }
   }
}
