package gov.nih.mipav.model.file;


import gov.nih.mipav.model.structures.*;

import gov.nih.mipav.view.dialogs.*;


/**
 * This structures contains the information that describes how a Biorad image is stored on disk.
 *
 * @see  FileBioRad
 */
public class FileInfoBioRad extends FileInfoBase {

    //~ Static fields/initializers -------------------------------------------------------------------------------------

    /** Use serialVersionUID for interoperability. */
    private static final long serialVersionUID = 4850818161207332016L;

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** DOCUMENT ME! */
    String imageDescription = null;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * FileInfoBioRad File info storage constructor.
     *
     * @param  name       file name
     * @param  directory  directory
     * @param  format     file format
     */
    public FileInfoBioRad(String name, String directory, int format) {
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

        if (imageDescription != null) {
            dialog.append("\n\n                Other information\n\n");
            dialog.append("Image description:\n" + imageDescription + "\n");
        }

    }

    /**
     * DOCUMENT ME!
     *
     * @param  imageDescription  DOCUMENT ME!
     */
    public void setImageDescription(String imageDescription) {
        this.imageDescription = imageDescription;
    }
}
