package gov.nih.mipav.view.dialogs;


import gov.nih.mipav.model.file.*;

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

    /** DOCUMENT ME! */
    private JRadioButton datasetOrder;

    /** DOCUMENT ME! */
    private JRadioButton dicomOrder;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Creates new dialog.
     *
     * @param  theParentFrame  Parent frame of dialog.
     * @param  orients         Axis orientation of image.
     */
    public JDialogOrder(Frame theParentFrame, int[] orients) {
        super(theParentFrame, true);
        init(orients);
    }

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * Disposes of dialog.
     *
     * @param  event  Event that triggered this function.
     */
    public void actionPerformed(ActionEvent event) {
        dispose();
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

        JPanel orderPanel = new JPanel(new GridLayout(1, 2));
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

}
