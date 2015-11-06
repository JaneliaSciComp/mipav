package gov.nih.mipav.view.dialogs;


import gov.nih.mipav.model.file.*;
import gov.nih.mipav.model.structures.*;

import gov.nih.mipav.view.*;
import gov.nih.mipav.view.components.WidgetFactory;

import java.awt.*;
import java.awt.event.*;
import java.util.Vector;

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
        addAnonymizeButton();
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
        } else if (event.getActionCommand().equals("Anonymize")) {
            //new JDialogAnonymizeImage(this, ((ViewJFrameImage) parentFrame).getActiveImage()); // changes the image
                                                                                               // internally,
        	((ViewJFrameImage)parentFrame).getActiveImage().anonymize(null, false);

            // so we don't need to remember the dialog.
            // now that dialog has finished,
            // tell any other objects that care that there are new data (ie, a new name) & update
            Vector<ViewImageUpdateInterface> imageFrames = ((ViewJFrameImage) parentFrame).getActiveImage().getImageFrameVector();

            for (int i = 0; i < imageFrames.size(); i++) {
                ((ViewJFrameBase) (imageFrames.elementAt(i))).setTitle();
            }

            setTitle(((ViewJFrameImage) parentFrame).getActiveImage().getImageName());
            MipavUtil.displayInfo("Anonymization complete");

            dispose();
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
    
    /**
     * Creates the anonymization button and adds it to the button panel.
     */
    private void addAnonymizeButton() {
        JButton anonButton = WidgetFactory.buildTextButton("Anonymize", "Anonymize image info", "Anonymize", this);
        anonButton.setPreferredSize(new Dimension(WidgetFactory.getDefaultButtonSize().width, 30));
        getButtonPanel().add(anonButton);
    }
}
