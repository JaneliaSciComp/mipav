package gov.nih.mipav.view.dialogs;


import gov.nih.mipav.model.file.*;
import gov.nih.mipav.model.structures.*;

import gov.nih.mipav.view.*;

import java.awt.*;
import java.awt.event.*;

import javax.swing.*;


/**
 * Dialog to get image type for blank image.
 *
 * @version  0.1 Aug 1, 1997
 * @author   Matthew J. McAuliffe, Ph.D.
 * @see      FileIO
 */
public class JDialogBlankImage extends JDialogBase {

    //~ Static fields/initializers -------------------------------------------------------------------------------------

    /** Use serialVersionUID for interoperability. */
    private static final long serialVersionUID = 8301756369819912860L;

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** DOCUMENT ME! */
    private int dataType;

    /** DOCUMENT ME! */
    private ModelImage image;

    /** DOCUMENT ME! */
    private JRadioButton radioBool;

    /** DOCUMENT ME! */
    private JRadioButton radioByte;

    /** DOCUMENT ME! */
    private JRadioButton radioDouble;

    /** DOCUMENT ME! */
    private JRadioButton radioFloat;

    /** DOCUMENT ME! */
    private JRadioButton radioInt;

    /** DOCUMENT ME! */
    private JRadioButton radioLong;

    /** DOCUMENT ME! */
    private JRadioButton radioShort;

    /** DOCUMENT ME! */
    private JRadioButton radioUByte;

    /** DOCUMENT ME! */
    private JRadioButton radioUInt;

    /** DOCUMENT ME! */
    private JRadioButton radioUShort;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Creates new dialog to get image type of blank image.
     *
     * @param  theParentFrame  Parent frame
     */
    public JDialogBlankImage(Frame theParentFrame) {
        super(theParentFrame, true);
        setTitle("Load blank image");

        ButtonGroup group1 = new ButtonGroup();
        radioBool = new JRadioButton("Boolean", false);
        radioBool.setFont(serif12);
        group1.add(radioBool);

        radioByte = new JRadioButton("Byte", false);
        radioByte.setFont(serif12);
        group1.add(radioByte);

        radioUByte = new JRadioButton("Unsigned Byte", false);
        radioUByte.setFont(serif12);
        group1.add(radioUByte);

        radioShort = new JRadioButton("Short", true);
        radioShort.setFont(serif12);
        group1.add(radioShort);

        radioUShort = new JRadioButton("Unsigned Short", false);
        radioUShort.setFont(serif12);
        group1.add(radioUShort);

        radioInt = new JRadioButton("Integer", false);
        radioInt.setFont(serif12);
        group1.add(radioInt);

        radioUInt = new JRadioButton("Unsigned Integer", false);
        radioUInt.setFont(serif12);
        group1.add(radioUInt);

        radioLong = new JRadioButton("Long", false);
        radioLong.setFont(serif12);
        group1.add(radioLong);

        radioFloat = new JRadioButton("Float", false);
        radioFloat.setFont(serif12);
        group1.add(radioFloat);

        radioDouble = new JRadioButton("Double", false);
        radioDouble.setFont(serif12);
        group1.add(radioDouble);

        JPanel panelImageType = new JPanel(new GridBagLayout());
        panelImageType.setBorder(buildTitledBorder("Image Type"));

        GridBagConstraints gbc = new GridBagConstraints();
        gbc.gridwidth = 1;
        gbc.gridheight = 1;
        gbc.anchor = GridBagConstraints.WEST;

        gbc.gridx = 0;
        gbc.gridy = 0;
        panelImageType.add(radioBool, gbc);
        gbc.gridy = 1;
        panelImageType.add(radioByte, gbc);
        gbc.gridy = 2;
        panelImageType.add(radioUByte, gbc);
        gbc.gridy = 3;
        panelImageType.add(radioShort, gbc);
        gbc.gridy = 4;
        panelImageType.add(radioUShort, gbc);
        gbc.gridx = 1;
        gbc.gridy = 0;
        panelImageType.add(radioInt, gbc);
        gbc.gridy = 1;
        panelImageType.add(radioUInt, gbc);
        gbc.gridy = 2;
        panelImageType.add(radioLong, gbc);
        gbc.gridy = 3;
        panelImageType.add(radioFloat, gbc);
        gbc.gridy = 4;
        panelImageType.add(radioDouble, gbc);

        JPanel buttonPanel = new JPanel();
        buildOKButton();
        buttonPanel.add(OKButton);
        buildCancelButton();
        buttonPanel.add(cancelButton);

        mainDialogPanel.add(panelImageType);
        mainDialogPanel.add(buttonPanel, BorderLayout.SOUTH);

        getContentPane().add(mainDialogPanel);

        pack();
        setVisible(true);
    }

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * Closes dialog box when the OK button is pressed and sets the variables.
     *
     * @param  event  event that triggers this function
     */
    public void actionPerformed(ActionEvent event) {
        Object source = event.getSource();

        if (source == OKButton) {

            if (radioBool.isSelected()) {
                dataType = ModelStorageBase.BOOLEAN;
            } else if (radioByte.isSelected()) {
                dataType = ModelStorageBase.BYTE;
            } else if (radioUByte.isSelected()) {
                dataType = ModelStorageBase.UBYTE;
            } else if (radioShort.isSelected()) {
                dataType = ModelStorageBase.SHORT;
            } else if (radioUShort.isSelected()) {
                dataType = ModelStorageBase.USHORT;
            } else if (radioInt.isSelected()) {
                dataType = ModelStorageBase.INTEGER;
            } else if (radioUInt.isSelected()) {
                dataType = ModelStorageBase.UINTEGER;
            } else if (radioLong.isSelected()) {
                dataType = ModelStorageBase.LONG;
            } else if (radioFloat.isSelected()) {
                dataType = ModelStorageBase.FLOAT;
            } else if (radioDouble.isSelected()) {
                dataType = ModelStorageBase.DOUBLE;
            } else {
                dataType = ModelStorageBase.BYTE;
            }

            makeImage();
            dispose();
        } else if (source == cancelButton) {
            cancelFlag = true;
            dispose();
        }
    }

    /**
     * Accessor that returns the data type.
     *
     * @return  the data type
     */
    public int getDataType() {
        return dataType;
    }

    /**
     * Accessor that returns the image.
     *
     * @return  the image
     */
    public ModelImage getImage() {
        return image;
    }

    /**
     * Creates new image with given data type.
     */
    private void makeImage() {
        int[] destExtents = ((ViewJFrameImage) (parentFrame)).getImageA().getExtents();
        image = new ModelImage(dataType, destExtents, " Blank");

    }

}
