package gov.nih.mipav.view.dialogs;

import java.awt.*;
import java.awt.event.ActionEvent;
import javax.swing.*;
import gov.nih.mipav.model.file.FileInfoBase;

/**
*   Confirmation dialog used in AFNI to decide whether to
*   reorder the image to dicom order.
*/
public class JDialogOrder extends JDialogBase {

    private JRadioButton datasetOrder;
    private JRadioButton dicomOrder;

    /**
    *   Creates new dialog.
    *   @param theParentFrame   Parent frame of dialog.
    *   @param orients          Axis orientation of image.
    */
    public JDialogOrder(Frame theParentFrame, int[] orients) {
        super(theParentFrame, true);
        init(orients);
    }

    /**
    *   Creates and displays dialog.
    *   @param orientSpecific   Orientation of the image's axes.
    */
    private void init(int[] orientSpecific) {
        setTitle("Choose image ordering");
	    JPanel datasetPanel = new JPanel(new GridLayout(3,1));
	    datasetPanel.setBorder(buildTitledBorder("Dataset order"));

	    switch(orientSpecific[0]) {
	        case FileInfoBase.ORI_R2L_TYPE:
	            datasetPanel.add(createLabel("X right to left"));
	            break;
	        case FileInfoBase.ORI_L2R_TYPE:
	            datasetPanel.add(createLabel("X left to right"));
	            break;
	        case FileInfoBase.ORI_P2A_TYPE:
	            datasetPanel.add(createLabel("X posterior to anterior"));
	            break;
	        case FileInfoBase.ORI_A2P_TYPE:
	            datasetPanel.add(createLabel("X anterior to posterior"));
	            break;
	        case FileInfoBase.ORI_I2S_TYPE:
	            datasetPanel.add(createLabel("X inferior to superior"));
	            break;
	        case FileInfoBase.ORI_S2I_TYPE:
	            datasetPanel.add(createLabel("X superior to inferior"));
	            break;
	        default:
	            datasetPanel.add(createLabel("X unknown"));
	            break;
	    }

		switch(orientSpecific[1]) {
	        case FileInfoBase.ORI_R2L_TYPE:
	            datasetPanel.add(createLabel("Y right to left"));
	            break;
	        case FileInfoBase.ORI_L2R_TYPE:
	            datasetPanel.add(createLabel("Y left to right"));
	            break;
	        case FileInfoBase.ORI_P2A_TYPE:
	            datasetPanel.add(createLabel("Y posterior to anterior"));
	            break;
	        case FileInfoBase.ORI_A2P_TYPE:
	            datasetPanel.add(createLabel("Y anterior to posterior"));
	            break;
	        case FileInfoBase.ORI_I2S_TYPE:
	            datasetPanel.add(createLabel("Y inferior to superior"));
	            break;
	        case FileInfoBase.ORI_S2I_TYPE:
	            datasetPanel.add(createLabel("Y superior to inferior"));
	            break;
	        default:
	            datasetPanel.add(createLabel("Y unknown"));
	            break;
	    }

		switch(orientSpecific[2]) {
	        case FileInfoBase.ORI_R2L_TYPE:
	            datasetPanel.add(createLabel("Z right to left"));
	            break;
	        case FileInfoBase.ORI_L2R_TYPE:
	            datasetPanel.add(createLabel("Z left to right"));
	            break;
	        case FileInfoBase.ORI_P2A_TYPE:
	            datasetPanel.add(createLabel("Z posterior to anterior"));
	            break;
	        case FileInfoBase.ORI_A2P_TYPE:
	            datasetPanel.add(createLabel("Z anterior to posterior"));
	            break;
	        case FileInfoBase.ORI_I2S_TYPE:
	            datasetPanel.add(createLabel("Z inferior to superior"));
	            break;
	        case FileInfoBase.ORI_S2I_TYPE:
	            datasetPanel.add(createLabel("Z superior to inferior"));
	            break;
	        default:
	            datasetPanel.add(createLabel("Z unknown"));
	            break;
	    }

	    JPanel dicomPanel = new JPanel(new GridLayout(3,1));
        dicomPanel.setBorder(buildTitledBorder("Dicom order"));

        dicomPanel.add(createLabel("X right to left"));
        dicomPanel.add(createLabel("Y anterior to posterior"));
        dicomPanel.add(createLabel("Z inferior to superior"));

        JPanel createPanel = new JPanel(new GridLayout(1,2));
        createPanel.setBorder(buildTitledBorder("Create image with"));

        ButtonGroup orderGroup = new ButtonGroup();
        datasetOrder = new JRadioButton("dataset order", true);
		datasetOrder.setFont(serif12);
		orderGroup.add(datasetOrder);

        createPanel.add(datasetOrder);

		dicomOrder = new JRadioButton("dicom order",false);
		dicomOrder.setFont(serif12);
		orderGroup.add(dicomOrder);

        createPanel.add(dicomOrder);

        JPanel buttonPanel = new JPanel();
        buildOKButton();
        buttonPanel.add(OKButton);

        JPanel orderPanel = new JPanel(new GridLayout(1,2));
        orderPanel.add(datasetPanel);
        orderPanel.add(dicomPanel);

        JPanel mainPanel = new JPanel(new BorderLayout());
        mainPanel.add(orderPanel);
        mainPanel.add(createPanel, BorderLayout.SOUTH);

        getContentPane().add(mainPanel);
        getContentPane().add(buttonPanel, BorderLayout.SOUTH);
        pack();
        setVisible(true);
    }


    /**
    *   Accessor that returns flag for ordering to dicom.
    *   @return <code>true</code> if should order to dicom.
    */
    public boolean doDicom() {
        return dicomOrder.isSelected();
    }

    /**
    *   Disposes of dialog.
    *   @param event    Event that triggered this function.
    */
	public void actionPerformed(ActionEvent event) {
        dispose();
    }

}
