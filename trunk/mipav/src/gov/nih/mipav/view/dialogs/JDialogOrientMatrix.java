package gov.nih.mipav.view.dialogs;


import gov.nih.mipav.model.algorithms.utilities.AlgorithmMask;
import gov.nih.mipav.model.structures.TransMatrix;

import java.awt.*;
import java.awt.event.ActionEvent;

import javax.swing.*;


/**
 * Dialog used to determine how the transformation matrix should be adjusted to account for different coordinate
 * systems. Two check boxes are available to modify the data so that the
 * 
 * @version , 2001
 * @author Matthew J. McAuliffe, Ph.D.
 * @see AlgorithmMask
 */
public class JDialogOrientMatrix extends JDialogBase {

    // ~ Static fields/initializers
    // -------------------------------------------------------------------------------------

    /** Use serialVersionUID for interoperability. */
    private static final long serialVersionUID = -5386555201530113228L;

    // ~ Instance fields
    // ------------------------------------------------------------------------------------------------

    /** DOCUMENT ME! */
    private final JCheckBox leftHandRuleCheckbox;

    /** DOCUMENT ME! */
    private TransMatrix matrix;

    /** DOCUMENT ME! */
    private final JPanel panelAxisMode;

    /** DOCUMENT ME! */
    private JDialogBase pFrame;

    /** DOCUMENT ME! */
    private final JCheckBox worldCoordCheckbox;

    // ~ Constructors
    // ---------------------------------------------------------------------------------------------------

    /**
     * Constructor.
     * 
     * @param theParentFrame parent frame
     * @param _pFrame source image
     */
    public JDialogOrientMatrix(final Frame theParentFrame, final JDialogBase _pFrame) {
        super(theParentFrame, false);

        setForeground(Color.black);
        getContentPane().setLayout(new BorderLayout());

        // this.pFrame = _pFrame;
        if (_pFrame instanceof JDialogScriptableTransform) {
            this.pFrame = _pFrame;
        } else if (_pFrame instanceof JDialogImageInfo) {
            this.pFrame = _pFrame;
        }

        setTitle("Adjust matrix axis");
        panelAxisMode = new JPanel();
        panelAxisMode.setLayout(new BorderLayout());
        panelAxisMode.setForeground(Color.black);
        panelAxisMode.setBorder(buildTitledBorder("Options"));
        getContentPane().add(panelAxisMode);

        worldCoordCheckbox = new JCheckBox("Change from world coordinate system.");
        worldCoordCheckbox.setFont(serif12);
        worldCoordCheckbox.addItemListener(this);
        panelAxisMode.add(worldCoordCheckbox, BorderLayout.NORTH);

        leftHandRuleCheckbox = new JCheckBox("Change from left-hand rule.");
        leftHandRuleCheckbox.setFont(serif12);
        leftHandRuleCheckbox.addItemListener(this);
        panelAxisMode.add(leftHandRuleCheckbox, BorderLayout.SOUTH);

        final JPanel buttonPanel = new JPanel();
        buildOKButton();
        buttonPanel.add(OKButton);
        buildCancelButton();
        buttonPanel.add(cancelButton);

        getContentPane().add(buttonPanel, BorderLayout.SOUTH);

        pack();
        setVisible(true);
        setResizable(false);
    }

    // ~ Methods
    // --------------------------------------------------------------------------------------------------------

    /**
     * actionPerformed - Closes dialog box when the OK button is pressed and calls the algorithm.
     * 
     * @param event event that triggers function
     */
    public void actionPerformed(final ActionEvent event) {
        final Object source = event.getSource();

        if (source == OKButton) {

            if (worldCoordCheckbox.isSelected() && leftHandRuleCheckbox.isSelected()) {
                pFrame.setWCSystem(true);
                pFrame.setLeftHandSystem(true);
                dispose();
            } else if (worldCoordCheckbox.isSelected()) { // Change just from the world coordinate system
                pFrame.setWCSystem(true);
                pFrame.setLeftHandSystem(false);
                dispose();
            } else if (leftHandRuleCheckbox.isSelected()) { // Change just from the "left-hand" system
                pFrame.setWCSystem(false);
                pFrame.setLeftHandSystem(true);
                dispose();
            } else { // No changes
                pFrame.setWCSystem(false);
                pFrame.setLeftHandSystem(false);
                dispose();
            }
        } else if (source == cancelButton) {
            pFrame.setWCSystem(false);
            pFrame.setLeftHandSystem(false);
            dispose();
        } else {
            super.actionPerformed(event);
        }
    }

    /**
     * Returns the matrix.
     * 
     * @return DOCUMENT ME!
     */
    public TransMatrix getMatrix() {
        return matrix;
    }
}
