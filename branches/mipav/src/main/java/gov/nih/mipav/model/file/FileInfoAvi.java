package gov.nih.mipav.model.file;


import gov.nih.mipav.model.structures.*;

import gov.nih.mipav.view.dialogs.*;


/**
 * This structures contains the information that describes how an Avi image is stored on disk.
 *
 * @see  FileAvi
 */

public class FileInfoAvi extends FileInfoBase {

    //~ Static fields/initializers -------------------------------------------------------------------------------------

    /** Use serialVersionUID for interoperability. */
    private static final long serialVersionUID = -5524135420342875298L;

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** DOCUMENT ME! */
    private int numFrames = 0; // actual number of frames (zDim * tDim)

    /** DOCUMENT ME! */
    private int totalFrames = 0;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * File info storage constructor.
     *
     * @param  name       file name
     * @param  directory  directory
     * @param  format     file format
     */
    public FileInfoAvi(String name, String directory, int format) {
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

        // add duration, frames per sec

        float microSecPerFrame = getResolutions()[2];

        float fps = 1.0f / (microSecPerFrame / 1000000.0f);
        int frames = getExtents()[2];

        if (getExtents().length > 3) {
            frames *= getExtents()[3];
        }

        float duration = frames / fps;

        dialog.append("Duration: " + duration + "\n");
        dialog.append("Frames per second: " + fps + "\n");
    }

    /**
     * DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    public int getNumFrames() {
        return this.numFrames;
    }

    /**
     * DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    public int getTotalFrames() {
        return this.totalFrames;
    }

    /**
     * DOCUMENT ME!
     *
     * @param  numFrames  DOCUMENT ME!
     */
    public void setNumFrames(int numFrames) {
        this.numFrames = numFrames;
    }

    /**
     * DOCUMENT ME!
     *
     * @param  totalFrames  DOCUMENT ME!
     */
    public void setTotalFrames(int totalFrames) {
        this.totalFrames = totalFrames;
    }

}
