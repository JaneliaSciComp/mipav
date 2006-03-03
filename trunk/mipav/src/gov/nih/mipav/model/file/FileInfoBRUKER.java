package gov.nih.mipav.model.file;

import gov.nih.mipav.view.dialogs.*;
import gov.nih.mipav.model.structures.*;

/**
*
*/

public class FileInfoBRUKER extends FileInfoBase {


    private String  sliceSeparationMode = null;
    private int     recoSize = -1;
    private float   sliceThickness = -1.0f;
    private boolean haveZResol = false;

    /**
    *  File info storage constructor
    *  @param name        file name
    *  @param directory   directory
    *  @param format      file format
    */
    public FileInfoBRUKER(String name, String directory, int format) {
        super(name, directory, format);
    }

    /**
    *   Accessor to set the slice separation mode.
    *   @param sliceSeparationMode  Value to set.
    */
    public void setSliceSeparationMode(String sliceSeparationMode) {
        this.sliceSeparationMode = sliceSeparationMode;
    }

    /**
    *   Accessor to set the reco size.
    *   @param recoSize     Value to set.
    */
    public void setRecoSize(int recoSize) {
        this.recoSize = recoSize;
    }

    /**
    *   Accessor to get the reco size.
    *   @return     The reco size.
    */
    public int getRecoSize() {
        return recoSize;
    }

    /**
    *   Accessor to set the slice thickness.
    *   @param sliceThickness   Thickness to set.
    */
    public void setSliceThickness(float sliceThickness) {
        this.sliceThickness = sliceThickness;
    }

    /**
    *   Accessor to set the flag for having a z resolution.
    *   @param haveZResol   Flag to set.
    */
    public void setHaveZResol(boolean haveZResol) {
        this.haveZResol = haveZResol;
    }

    /**
    *   Accessor to get the flag for having a z resolution.
    *   @return <code>true</code> if has a z resolution.
    */
    public boolean getHaveZResol() {
        return haveZResol;
    }

    /**
    *   Displays the file information
    *   @param dlog    dialog box that is written to
    *   @param matrix  transformation matrix
    */
    public void displayAboutInfo(JDialogBase dlog, TransMatrix matrix){
        JDialogText dialog = (JDialogText) dlog;

        displayPrimaryInfo(dialog, matrix);
        dialog.append("\n\n                Other information\n\n");

        if (sliceSeparationMode != null) {
            dialog.append("Slice separation mode:" + sliceSeparationMode + "\n");
        }

        if (sliceThickness > 0.0f) {
            dialog.append("Slice thickness:      " + sliceThickness + " millimeters\n");
        }
    }
}
