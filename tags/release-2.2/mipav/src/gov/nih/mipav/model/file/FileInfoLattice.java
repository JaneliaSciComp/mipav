package gov.nih.mipav.model.file;

import gov.nih.mipav.model.structures.*;
import gov.nih.mipav.view.dialogs.*;

/**
*   This structure contains the information that describes how
*   a Lattice image is stored on disk.
*
*		@version    0.1 May 5, 1997
*		@author     Matthew J. McAuliffe, Ph.D.
*/
public class FileInfoLattice extends FileInfoBase {

    /**
    *  File info storage constructor
    *  @param name        file name
    *  @param directory   directory
    *  @param format      file format
    */
    public FileInfoLattice(String name, String directory, int format) {
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
