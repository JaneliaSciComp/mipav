package gov.nih.mipav.model.file;

import gov.nih.mipav.model.structures.*;
import gov.nih.mipav.view.dialogs.*;

/**
*   This structures contains the information that describes how
*   an analyze image is stored on disk. Cheshire.
*
*		@version    0.1 Oct 14, 1997
*		@author     Matthew J. McAuliffe, Ph.D.
*       @see        FileAnalyze
*
*/

public class FileInfoCheshire extends FileInfoBase {

    /**
    *  File info storage constructor
    *  @param name        file name
    *  @param directory   directory
    *  @param format      file format
    */
    public FileInfoCheshire(String name, String directory, int format) {
        super(name, directory, format);
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
