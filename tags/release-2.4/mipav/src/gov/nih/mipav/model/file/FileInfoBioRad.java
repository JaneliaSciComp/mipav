package gov.nih.mipav.model.file;

import gov.nih.mipav.view.dialogs.*;
import gov.nih.mipav.model.structures.*;

/**
*   This structures contains the information that describes how
*   a Biorad image is stored on disk.
*
*       @see        FileBioRad
*/
public class FileInfoBioRad extends FileInfoBase {

    String imageDescription = null;;

    /**
    *  FileInfoBioRad     File info storage constructor
    *  @param name        file name
    *  @param directory   directory
    *  @param format      file format
    */
    public FileInfoBioRad(String name, String directory, int format) {
        super(name, directory, format);
    }

    public void setImageDescription(String imageDescription) {
        this.imageDescription = imageDescription;
    }

    /**
    *  Displays the file information
    *  @param dlog    dialog box that is written to
    *  @param matrix  transformation matrix
    */
    public void displayAboutInfo(JDialogBase dlog, TransMatrix matrix){
        JDialogText dialog = (JDialogText) dlog;

        displayPrimaryInfo(dialog, matrix);
        if (imageDescription != null) {
            dialog.append("\n\n                Other information\n\n");
            dialog.append("Image description:\n" + imageDescription + "\n");
        }

    }
}
