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
    private String layerString[] = null;
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
        if (layerString != null) {
            for (i = 0; i < layerString.length; i++) {
                if (layerString[i] != null) {
                    dialog.append("Layer " + (i + 1) + " = " + layerString[i].trim() + "\n");
                }
            }
        }
        
        if (bitDepth > 0) {
            dialog.append("Bit depth = " + bitDepth + "\n");
        }
        
    }
    
    public void setLayerString(String layerString[]) {
        this.layerString = layerString;
    }
    
    public void setBitDepth(short bitDepth) {
        this.bitDepth = bitDepth;
    }

    
}
