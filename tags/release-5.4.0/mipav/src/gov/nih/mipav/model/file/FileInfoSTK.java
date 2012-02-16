package gov.nih.mipav.model.file;


import gov.nih.mipav.model.structures.*;

import gov.nih.mipav.view.dialogs.*;


/**
 * This structures contains the information that describes how a STK image is stored on disk.
 */

public class FileInfoSTK extends FileInfoBase {

    //~ Static fields/initializers -------------------------------------------------------------------------------------

    /** Use serialVersionUID for interoperability. */
    private static final long serialVersionUID = 4681901066117260212L;

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** DOCUMENT ME! */
    String imageDescription = null;

    /** DOCUMENT ME! */
    boolean sameZPosition = false;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * File info storage constructor.
     *
     * @param  name       file name
     * @param  directory  directory
     * @param  format     file format
     */
    public FileInfoSTK(String name, String directory, int format) {
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
        int i;
        int j;
        int planeNumber;
        JDialogText dialog = (JDialogText) dlog;
        displayPrimaryInfo(dialog, matrix);

        if (imageDescription != null) {
            dialog.append("\n\n                Other information\n\n");
            planeNumber = 1;
            j = 0;

            for (i = 0; i < imageDescription.length(); i++) {

                if (imageDescription.charAt(i) == 0) {
                    dialog.append("Image description for plane number " + planeNumber + ":\n" +
                                  imageDescription.substring(j, i) + "\n");
                    j = i + 1;
                    planeNumber++;
                }
            }
        } // if (imageDescription != null)

    }

    /**
     * DOCUMENT ME!
     *
     * @param  imageDescription  DOCUMENT ME!
     */
    public void setImageDescription(String imageDescription) {
        this.imageDescription = imageDescription;
    }

    /**
     * DOCUMENT ME!
     *
     * @param  sameZPosition  DOCUMENT ME!
     */
    public void setSameZPosition(boolean sameZPosition) {
        this.sameZPosition = sameZPosition;
    }
}
