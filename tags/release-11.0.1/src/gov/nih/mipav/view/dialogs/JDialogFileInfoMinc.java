package gov.nih.mipav.view.dialogs;


import gov.nih.mipav.view.*;
import gov.nih.mipav.view.components.*;

import java.awt.*;
import java.awt.event.*;

import java.util.*;

import javax.swing.*;


/**
 * Text dialog filled with the a minc image's file info. Also has a button to initiate anonymization.
 *
 * @author  mccreedy
 */
public class JDialogFileInfoMinc extends JDialogText {

    //~ Static fields/initializers -------------------------------------------------------------------------------------

    /** Use serialVersionUID for interoperability. */
    private static final long serialVersionUID = -966963254580510543L;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Constructs resizable dialog with text area in the middle.
     *
     * @param  parent  Parent frame.
     * @param  title   Title of dialog frame.
     */
    public JDialogFileInfoMinc(Frame parent, String title) {
        super(parent, title);
        addAnonymizeButton();
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
        } else if (event.getActionCommand().equals("Anonymize")) {
            new JDialogAnonymizeImage(this, ((ViewJFrameImage) parentFrame).getActiveImage()); // changes the image
                                                                                               // internally,

            // so we don't need to remember the dialog.
            // now that dialog has finished,
            // tell any other objects that care that there are new data (ie, a new name) & update
            Vector<ViewImageUpdateInterface> imageFrames = ((ViewJFrameImage) parentFrame).getActiveImage().getImageFrameVector();

            for (int i = 0; i < imageFrames.size(); i++) {
                ((ViewJFrameBase) (imageFrames.elementAt(i))).setTitle();
            }

            setTitle(((ViewJFrameImage) parentFrame).getActiveImage().getImageName());

            dispose();
        }
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
