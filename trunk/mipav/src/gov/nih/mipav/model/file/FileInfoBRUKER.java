package gov.nih.mipav.model.file;


import gov.nih.mipav.model.structures.*;

import gov.nih.mipav.view.dialogs.*;


/**
 * DOCUMENT ME!
 */

public class FileInfoBRUKER extends FileInfoBase {

    //~ Static fields/initializers -------------------------------------------------------------------------------------

    /** Use serialVersionUID for interoperability. */
    private static final long serialVersionUID = 292865443840539139L;

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** DOCUMENT ME! */
    private boolean haveZResol = false;

    /** DOCUMENT ME! */
    private int recoSize = -1;


    /** DOCUMENT ME! */
    private String sliceSeparationMode = null;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * File info storage constructor.
     *
     * @param  name       file name
     * @param  directory  directory
     * @param  format     file format
     */
    public FileInfoBRUKER(String name, String directory, int format) {
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

        if (sliceSeparationMode != null) {
            dialog.append("Slice separation mode:" + sliceSeparationMode + "\n");
        }

        if (getSliceThickness() > 0.0f) {
            dialog.append("Slice thickness:      " + getSliceThickness() + " millimeters\n");
        }
    }

    /**
     * Accessor to get the flag for having a z resolution.
     *
     * @return  <code>true</code> if has a z resolution.
     */
    public boolean getHaveZResol() {
        return haveZResol;
    }

    /**
     * Accessor to get the reco size.
     *
     * @return  The reco size.
     */
    public int getRecoSize() {
        return recoSize;
    }

    /**
     * Accessor to set the flag for having a z resolution.
     *
     * @param  haveZResol  Flag to set.
     */
    public void setHaveZResol(boolean haveZResol) {
        this.haveZResol = haveZResol;
    }

    /**
     * Accessor to set the reco size.
     *
     * @param  recoSize  Value to set.
     */
    public void setRecoSize(int recoSize) {
        this.recoSize = recoSize;
    }

    /**
     * Accessor to set the slice separation mode.
     *
     * @param  sliceSeparationMode  Value to set.
     */
    public void setSliceSeparationMode(String sliceSeparationMode) {
        this.sliceSeparationMode = sliceSeparationMode;
    }
}
