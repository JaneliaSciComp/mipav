package gov.nih.mipav.model.file;


import gov.nih.mipav.model.structures.*;

import gov.nih.mipav.view.dialogs.*;


/**
 * This structures contains the information that describes how a TMG image is stored on disk.
 */

public class FileInfoTMG extends FileInfoBase {

    //~ Static fields/initializers -------------------------------------------------------------------------------------

    /** Use serialVersionUID for interoperability. */
    private static final long serialVersionUID = 652378870162150500L;

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** DOCUMENT ME! */
    private String commentStr = null;

    /** DOCUMENT ME! */
    private String nameStr = null;

    /** DOCUMENT ME! */
    private String timeStr = null;

    /** DOCUMENT ME! */
    private String titleStr = null;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * FileInfoTMG - file info storage constructor.
     *
     * @param  name       file name
     * @param  directory  directory
     * @param  format     file format
     */
    public FileInfoTMG(String name, String directory, int format) {
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
        dialog.append("\n\n                Other information\n\n");

        if (titleStr != null) {
            dialog.append("Title = " + titleStr + "\n");
        }

        if (nameStr != null) {
            dialog.append("Name = " + nameStr + "\n");
        }

        if (commentStr != null) {
            dialog.append("Comment = " + commentStr + "\n");
        }

        if (timeStr != null) {
            dialog.append("Time = " + timeStr + "\n");
        }
    }

    /**
     * DOCUMENT ME!
     *
     * @param  commentStr  DOCUMENT ME!
     */
    public void setCommentStr(String commentStr) {
        this.commentStr = commentStr;
    }

    /**
     * DOCUMENT ME!
     *
     * @param  nameStr  DOCUMENT ME!
     */
    public void setNameStr(String nameStr) {
        this.nameStr = nameStr;
    }

    /**
     * DOCUMENT ME!
     *
     * @param  timeStr  DOCUMENT ME!
     */
    public void setTimeStr(String timeStr) {
        this.timeStr = timeStr;
    }

    /**
     * DOCUMENT ME!
     *
     * @param  titleStr  DOCUMENT ME!
     */
    public void setTitleStr(String titleStr) {
        this.titleStr = titleStr;
    }
}
