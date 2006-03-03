package gov.nih.mipav.view.dialogs;

import gov.nih.mipav.view.*;
import gov.nih.mipav.model.file.*;
import gov.nih.mipav.model.structures.ModelImage;

import javax.swing.*;

import java.awt.event.*;
import java.awt.*;

/**
*   This is simple text dialog that displays in the center of the screen. It
*   automatically adjusts the dialog size to the length of the input string.
*
*		@version    0.1 Oct 1, 1998
*		@author     Matthew J. McAuliffe, Ph.D.
*
*/
public class JDialogTextGE extends JDialogText {

	FileInfoBase[]	fileInfo;
	ModelImage		image;
	JButton 		convertButton;
	int 			slice;

    /**
    *  Constructs resizable dialog with text area in the middle.
    *  @param parent        Parent frame.
    *  @param title         Title of dialog frame.
    */
	public JDialogTextGE(Frame parent, String title, ModelImage _image, int loc) {
	    super(parent, title);
		addConvert();
		pack();
		image = _image;
		fileInfo = image.getFileInfo();
		slice = loc;
	}

    /**
    *  Creates the convert button and adds it to the button panel.
    */
	private void addConvert() {
		convertButton = new JButton("Convert to DICOM");
		convertButton.addActionListener(this);
		convertButton.setFont(serif12B);
		convertButton.setPreferredSize(new Dimension(140, 30));
		getButtonPanel().add(convertButton);
	}

   /**
   *    Converts the file infos to FileInfoDicoms.
   *    @param event	Event that triggers this function.
   */
   public void actionPerformed(ActionEvent event) {
        Object source = event.getSource();
        if ( source == cancelButton ) {
            dispose();
    	}
    	if ( source == convertButton ) {
            FileInfoDicom dicomInfo;
            for (int i=0; i<fileInfo.length; i++) {
                dicomInfo = ((FileInfoGESigna5X)fileInfo[i]).convertToDICOMInfo(i);
                image.setFileInfo(dicomInfo, i);
            }
            image.calcMinMax();
            dispose();
            ((ViewJFrameBase)parentFrame).about(slice, 0);
    	}
   }
}
