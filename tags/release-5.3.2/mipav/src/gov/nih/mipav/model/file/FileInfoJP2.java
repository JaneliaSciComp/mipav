package gov.nih.mipav.model.file;


import gov.nih.mipav.model.structures.*;

import gov.nih.mipav.view.dialogs.*;


/**
 * This structures contains the information that describes how a JP2 compressed image of 3D medical image is stored on disk.
 *
 * @version  0.1 Mar *, 2007
 * @author   Dzung Nguyen
 * @see      FileJP2
 */

public class FileInfoJP2 extends FileInfoBase {

    //~ Static fields/initializers -------------------------------------------------------------------------------------

//    /** Use serialVersionUID for interoperability. */
//    private static final long serialVersionUID = 7421068113479441629L;

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** DOCUMENT ME! */
    String imageDescription = null;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * File info storage constructor.
     *
     * @param  name       file name
     * @param  directory  directory
     * @param  format     file format
     */
    public FileInfoJP2(String name, String directory, int format) {
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

    /**
     * DOCUMENT ME!
     *
     * @param  imageDescription  DOCUMENT ME!
     */
    public void setImageDescription(String imageDescription) {
        this.imageDescription = imageDescription;
    }
}
