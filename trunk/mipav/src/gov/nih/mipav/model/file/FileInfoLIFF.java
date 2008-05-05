package gov.nih.mipav.model.file;


import gov.nih.mipav.model.structures.*;

import gov.nih.mipav.view.dialogs.*;


/**
 * File info storage container.
 */
public class FileInfoLIFF extends FileInfoBase {

    //~ Static fields/initializers -------------------------------------------------------------------------------------

    /** Use serialVersionUID for interoperability. */
    //private static final long serialVersionUID;
    private String dyeString[] = null;
    short bitDepth = 0;

    //~ Instance fields ------------------------------------------------------------------------------------------------

    

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * File info storage constructor.
     *
     * @param  name       file name
     * @param  directory  directory
     * @param  format     file format
     */
    public FileInfoLIFF(String name, String directory, int format) {
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
        if (dyeString != null) {
            for (i = 0; i < dyeString.length; i++) {
                if (dyeString[i] != null) {
                    dialog.append("Dye " + (i + 1) + " = " + dyeString[i].trim() + "\n");
                }
            }
        }
        
        if (bitDepth > 0) {
            dialog.append("Bit depth = " + bitDepth + "\n");
        }
        
    }
    
    public void setDyeString(String dyeString[]) {
        this.dyeString = dyeString;
    }
    
    public void setBitDepth(short bitDepth) {
        this.bitDepth = bitDepth;
    }

    
}
