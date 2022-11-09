package gov.nih.mipav.view.dialogs;


import gov.nih.mipav.model.file.*;
import gov.nih.mipav.model.structures.*;
import gov.nih.mipav.model.structures.ModelStorageBase.DataType;

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

    /** The ModelStorageBase.DataType of the blank image. */
    private DataType dataType;

    /** The blank image to be created. */
    private ModelImage image;
    
    /** The Radio buttons for selecting the new data type of the image */
    private JRadioButton[] radioTypes = new JRadioButton[DataType.values().length];

    /** The composed group of radio buttons. */
    private ButtonGroup group1;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Creates new dialog to get image type of blank image.
     *
     * @param  theParentFrame  Parent frame
     */
    public JDialogBlankImage(Frame theParentFrame) {
        super(theParentFrame, true);
        setTitle("Load blank image");

        group1 = new ButtonGroup();
        for(int i=0; i<radioTypes.length; i++) {
            radioTypes[i] = new JRadioButton(DataType.values()[i].name(), false);
            radioTypes[i].setFont(MipavUtil.font12);
            radioTypes[i].setActionCommand(DataType.values()[i].name());
            group1.add(radioTypes[i]);
        }
        radioTypes[0].setSelected(true);

        JPanel panelImageType = new JPanel(new GridBagLayout());
        panelImageType.setBorder(buildTitledBorder("Image Type"));

        GridBagConstraints gbc = new GridBagConstraints();
        gbc.gridwidth = 1;
        gbc.gridheight = 1;
        gbc.anchor = GridBagConstraints.WEST;

        gbc.gridx = 0;
        gbc.gridy = -1;
        for(int i=0; i<radioTypes.length; i++) {
            gbc.gridy++;
            panelImageType.add(radioTypes[i], gbc);
            if(gbc.gridy == radioTypes.length/2) {
                gbc.gridx++;
                gbc.gridy = -1;
            }
        }

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
        if (source.equals(OKButton)) {
            
            dataType = DataType.valueOf(group1.getSelection().getActionCommand());
            Preferences.debug("Data type of created image is "+dataType.name(), Preferences.DEBUG_MINOR);

            makeImage();
            dispose();
        } else if (source.equals(cancelButton)) {
            cancelFlag = true;
            dispose();
        } else {
            super.actionPerformed(event);
        }
    }

    /**
     * Accessor that returns the data type.
     *
     * @return  the data type
     */
    public DataType getDataType() {
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
