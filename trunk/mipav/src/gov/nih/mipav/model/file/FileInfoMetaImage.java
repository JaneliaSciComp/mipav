package gov.nih.mipav.model.file;


import gov.nih.mipav.model.structures.*;

import gov.nih.mipav.view.dialogs.*;


/**
 * This structures contains the information that describes how a MetaImage image is stored on disk.
 */
public class FileInfoMetaImage extends FileInfoBase {

    //~ Static fields/initializers -------------------------------------------------------------------------------------

    /** Use serialVersionUID for interoperability. */
    //private static final long serialVersionUID;
    private double centerOfRotation[] = null;
    

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * File info storage constructor.
     *
     * @param  name       file name
     * @param  directory  directory
     * @param  format     file format
     */
    public FileInfoMetaImage(String name, String directory, int format) {
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
        JDialogText dialog = (JDialogText) dlog;
        displayPrimaryInfo(dialog, matrix);
        
        dialog.append("\n\n                Other information\n\n");
        
        if (centerOfRotation != null) {
            for (i = 0; i < centerOfRotation.length; i++) {
                dialog.append("Center of rotation[" + i + "] = " + centerOfRotation[i] + "\n");
            }
        }
        
    }
    
    public void setCenterOfRotation(double centerOfRotation[]) {
        this.centerOfRotation = centerOfRotation;    
    }
    
    
}
