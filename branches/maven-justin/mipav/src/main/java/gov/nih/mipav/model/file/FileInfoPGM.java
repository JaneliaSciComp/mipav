package gov.nih.mipav.model.file;


import gov.nih.mipav.model.structures.*;
import gov.nih.mipav.view.dialogs.*;

public class FileInfoPGM extends FileInfoBase {

    //~ Static fields/initializers -------------------------------------------------------------------------------------

    /** Use serialVersionUID for interoperability. */
    //private static final long serialVersionUID;
	
	//~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * FileInfoPGM.
     *
     * @param  name       the file name
     * @param  directory  the file directory
     * @param  format     the file format (Portable Greymap Utilities)
     */
    public FileInfoPGM(String name, String directory, int format) {
        super(name, directory, format);
    }

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * displayAboutInfo - displays the file information.
     *
     * @param  dlog    dialog box that is written to
     * @param  matrix  transformation matrix
     */
    public void displayAboutInfo(JDialogBase dlog, TransMatrix matrix) {
        JDialogText dialog = (JDialogText) dlog;
        displayPrimaryInfo(dialog, matrix);
        dialog.append("\n\n                Other information\n\n");

        
        if (matrix != null) {

            // when using displayAboutInfo(dialog) this doesn't appear
            // calling prg might use an editing panel to adjust this matrix
            dialog.append("Matrix: \n" + matrix.matrixToString(10, 4) + "\n");
        }
    }
}