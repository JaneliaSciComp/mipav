package gov.nih.mipav.view.dialogs;


import gov.nih.mipav.model.file.*;
import gov.nih.mipav.model.structures.*;

import gov.nih.mipav.view.*;

import java.awt.*;
import java.awt.event.*;

import javax.swing.*;


/**
 * This is simple text dialog that displays in the center of the screen. It automatically adjusts the dialog size to the
 * length of the input string.
 *
 * @version  0.1 Oct 1, 1998
 * @author   Matthew J. McAuliffe, Ph.D.
 */
public class JDialogTextGE extends JDialogText {

    //~ Static fields/initializers -------------------------------------------------------------------------------------

    /** Use serialVersionUID for interoperability. */
    private static final long serialVersionUID = 419753934724528702L;

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** DOCUMENT ME! */
    JButton convertButton;

    /** DOCUMENT ME! */
    FileInfoBase[] fileInfo;

    /** DOCUMENT ME! */
    ModelImage image;

    /** DOCUMENT ME! */
    int slice;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Constructs resizable dialog with text area in the middle.
     *
     * @param  parent  Parent frame.
     * @param  title   Title of dialog frame.
     * @param  _image  DOCUMENT ME!
     * @param  loc     DOCUMENT ME!
     */
    public JDialogTextGE(Frame parent, String title, ModelImage _image, int loc) {
        super(parent, title);
        addConvert();
        pack();
        image = _image;
        fileInfo = image.getFileInfo();
        slice = loc;
    }

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * Converts the file infos to FileInfoDicoms.
     *
     * @param  event  Event that triggers this function.
     */
    public void actionPerformed(ActionEvent event) {
        Object source = event.getSource();

        if (source == cancelButton) {
            dispose();
        }

        if (source == convertButton) {
            FileInfoDicom dicomInfo;

            for (int i = 0; i < fileInfo.length; i++) {
                dicomInfo = ((FileInfoGESigna5X) fileInfo[i]).convertToDICOMInfo(i);
                image.setFileInfo(dicomInfo, i);
            }

            image.calcMinMax();
            dispose();
            ((ViewJFrameBase) parentFrame).about(slice, 0);
        }
    }

    /**
     * Creates the convert button and adds it to the button panel.
     */
    private void addConvert() {
        convertButton = new JButton("Convert to DICOM");
        convertButton.addActionListener(this);
        convertButton.setFont(serif12B);
        convertButton.setPreferredSize(new Dimension(140, 30));
        getButtonPanel().add(convertButton);
    }
}
