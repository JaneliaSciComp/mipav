package gov.nih.mipav.model.file;

import gov.nih.mipav.view.dialogs.*;
import gov.nih.mipav.model.structures.*;

/**
*   This structures contains the information that describes how
*   a Tiff image is stored on disk.
*
*		@version    0.1 May 5, 1997
*		@author     Matthew J. McAuliffe, Ph.D.
*       @see        FileTiff
*
*/

public class FileInfoTiff extends FileInfoBase {

    String imageDescription = null;

    /**
    *  File info storage constructor
    *  @param name        file name
    *  @param directory   directory
    *  @param format      file format
    */
    public FileInfoTiff(String name, String directory, int format) {
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
    }
}
