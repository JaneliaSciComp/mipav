package gov.nih.mipav.model.file;

import gov.nih.mipav.view.*;
import gov.nih.mipav.view.dialogs.*;
import gov.nih.mipav.model.structures.*;

/**
*   This structures contains the information that describes how
*   an Avi image is stored on disk.
*
*       @see        FileAvi
*
*/

public class FileInfoAvi extends FileInfoBase {

    private int totalFrames = 0;
    private int numFrames = 0;  //actual number of frames (zDim * tDim)


    /**
    *   File info storage constructor
    *   @param name        file name
    *   @param directory   directory
    *   @param format      file format
    */
    public FileInfoAvi(String name, String directory, int format) {
        super(name, directory, format);
    }

    /**
    *   Displays the file information
    *   @param dlog      dialog box that is written to
    *   @param matrix      transformation matrix
    */
    public void displayAboutInfo(JDialogBase dlog, TransMatrix matrix){
        JDialogText dialog = (JDialogText) dlog;
        displayPrimaryInfo(dialog, matrix);

        //add duration, frames per sec

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

    public void setNumFrames(int numFrames) {
        this.numFrames = numFrames;
    }

    public void setTotalFrames(int totalFrames) {
        this.totalFrames = totalFrames;
    }

    public int getNumFrames() { return this.numFrames; }

    public int getTotalFrames() { return this.totalFrames; }

}
