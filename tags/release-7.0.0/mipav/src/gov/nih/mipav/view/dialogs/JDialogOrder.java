package gov.nih.mipav.view.dialogs;


import gov.nih.mipav.model.file.*;
import gov.nih.mipav.view.Preferences;

import java.awt.*;
import java.awt.event.*;

import javax.swing.*;


/**
 * Confirmation dialog used in AFNI to decide whether to reorder the image to dicom order.
 */
public class JDialogOrder extends JDialogBase {

    //~ Static fields/initializers -------------------------------------------------------------------------------------

    /** Use serialVersionUID for interoperability. */
    private static final long serialVersionUID = 670499188247272869L;

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** Load image using dataset-specific ordering */
    private JRadioButton datasetOrder;

    /** Load image using dicom ordering (R-L, A-P, I-S) */
    private JRadioButton dicomOrder;

    /** Whether to ask user next time choice is available for AFNI ordering. */
	private JCheckBox dontAskAgain;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Creates new dialog.
     *
     * @param  theParentFrame  Parent frame of dialog.
     * @param  orients         Axis orientation of image.
     */
    public JDialogOrder(Frame theParentFrame, int[] orients) {
        super(theParentFrame, false);
        init(orients);
    }

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * Disposes of dialog.
     *
     * @param  event  Event that triggered this function.
     */
    public void actionPerformed(ActionEvent event) {
        if(dontAskAgain.isSelected()) {
        	Preferences.setProperty(Preferences.PREF_AFNI_ORDER_LOAD, Boolean.valueOf(dicomOrder.isSelected()).toString());
        }
    	dispose();
        super.actionPerformed(event);
    }


    /**
     * Accessor that returns flag for ordering to dicom.
     *
     * @return  <code>true</code> if should order to dicom.
     */
    public boolean doDicom() {
        return dicomOrder.isSelected();
    }

    /**
     * Creates and displays dialog.
     *
     * @param  orientSpecific  Orientation of the image's axes.
     */
    private void init(int[] orientSpecific) {
        setTitle("Choose image ordering");
        JPanel datasetPanel = new JPanel(new GridLayout(3, 1));
        datasetPanel.setBorder(buildTitledBorder("Dataset order"));

        datasetPanel.add(createLabel("X " + FileInfoBase.getAxisOrientationStr(orientSpecific[0]).toLowerCase()));
        datasetPanel.add(createLabel("Y " + FileInfoBase.getAxisOrientationStr(orientSpecific[1]).toLowerCase()));
        datasetPanel.add(createLabel("Z " + FileInfoBase.getAxisOrientationStr(orientSpecific[2]).toLowerCase()));

        JPanel dicomPanel = new JPanel(new GridLayout(3, 1));
        dicomPanel.setBorder(buildTitledBorder("Dicom order"));

        dicomPanel.add(createLabel("X right to left"));
        dicomPanel.add(createLabel("Y anterior to posterior"));
        dicomPanel.add(createLabel("Z inferior to superior"));

        JPanel createPanel = new JPanel(new GridLayout(1, 2));
        createPanel.setBorder(buildTitledBorder("Create image with"));

        ButtonGroup orderGroup = new ButtonGroup();
        datasetOrder = new JRadioButton("dataset order", true);
        datasetOrder.setFont(serif12);
        orderGroup.add(datasetOrder);

        createPanel.add(datasetOrder);

        dicomOrder = new JRadioButton("dicom order", false);
        dicomOrder.setFont(serif12);
        orderGroup.add(dicomOrder);
        createPanel.add(dicomOrder);

        JPanel buttonPanel = new JPanel();
        buildOKButton();
        buttonPanel.add(OKButton);
        
        GridBagConstraints gbc = new GridBagConstraints();
        gbc.gridx = 0;
        gbc.gridy = 0;
        gbc.fill = GridBagConstraints.BOTH;
        gbc.weightx = 1;
        gbc.weighty = 1;
        JPanel mainPanel = new JPanel(new GridBagLayout());
        mainPanel.add(datasetPanel, gbc);
        gbc.gridx++;
        mainPanel.add(dicomPanel, gbc);
        gbc.gridy++;
        gbc.gridx = 0;
        gbc.gridwidth = 2;
        mainPanel.add(createPanel, gbc);
        dontAskAgain = new JCheckBox("Remember decision", false);
        dontAskAgain.setFont(serif12);
        gbc.gridy++;
        mainPanel.add(dontAskAgain, gbc);

        getContentPane().add(mainPanel);
        getContentPane().add(buttonPanel, BorderLayout.SOUTH);
        
        pack();
        setVisible(true);

        if(Preferences.getProperty(Preferences.PREF_AFNI_ORDER_LOAD) != null) {
        	if(Preferences.is(Preferences.PREF_AFNI_ORDER_LOAD)) {
        		dicomOrder.setSelected(true);
        	} else {
        		datasetOrder.setSelected(true);
        	}
        	OKButton.doClick();
        }
        
    }

}
