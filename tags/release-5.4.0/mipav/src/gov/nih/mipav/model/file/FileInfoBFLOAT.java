package gov.nih.mipav.model.file;


import gov.nih.mipav.model.structures.*;

import gov.nih.mipav.view.dialogs.*;


public class FileInfoBFLOAT extends FileInfoBase {

    //~ Static fields/initializers -------------------------------------------------------------------------------------

    /** Use serialVersionUID for interoperability. */
    //private static final long serialVersionUID;

    //~ Instance fields ------------------------------------------------------------------------------------------------

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * FileInfoInterfile - file info storage constructor.
     *
     * @param  name       file name
     * @param  directory  directory
     * @param  format     file format
     */
    public FileInfoBFLOAT(String name, String directory, int format) {
        super(name, directory, format);
    }

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * Displays the file information.
     *
     * @param  dlog    dialog box that is written to
     * @param  matrix  transformation matrix
     */
    public void displayAboutInfo(JDialogBase dlog, TransMatrix matrix) {
        JDialogText dialog = (JDialogText) dlog;
        displayPrimaryInfo(dialog, matrix);
    }
}
