package gov.nih.mipav.model.file;

import gov.nih.mipav.view.dialogs.*;
import gov.nih.mipav.model.structures.*;

/**
*   File info storage container.
*/
public class FileInfoCOR extends FileInfoBase {

    private float  fov  = -1.0f;
    private float  tr   = -1.0f;
    private float  te   = -1.0f;
    private float  ti   = -1.0f;

    /**
    *   File info storage constructor
    *   @param name        file name
    *   @param directory   directory
    *   @param format      file format
    */
    public FileInfoCOR(String name, String directory, int format) {
        super(name, directory, format);
    }

    /**
    *   Accessor to set the field of vision.
    *   @param fov  Field of vision to set.
    */
    public void setFOV(float fov) {
        this.fov = fov;
    }

    /**
    *   Accessor to set the TE.
    *   @param te   TE to set.
    */
    public void setTE(float te) {
        this.te = te;
    }

    /**
    *   Accessor to set the TR.
    *   @param tr  TR to set.
    */
    public void setTR(float tr) {
        this.tr = tr;
    }

    /**
    *   Accessor to set the TI.
    *   @param ti   TI to set.
    */
    public void setTI(float ti) {
        this.ti = ti;
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

        if (fov > 0.0f) {
            dialog.append("Field of view:        " + fov + "\n");
        }

        if (tr >= 0.0f) {
            dialog.append("T_r:                  " + tr + "\n");
        }

        if (te >= 0.0f) {
            dialog.append("T_e:                  " + te + "\n");
        }

        if (ti >= 0.0f) {
            dialog.append("T_i:                  " + ti + "\n");
        }
    }
}
