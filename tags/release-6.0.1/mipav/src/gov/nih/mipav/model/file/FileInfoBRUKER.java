package gov.nih.mipav.model.file;


import gov.nih.mipav.model.structures.*;

import gov.nih.mipav.view.dialogs.*;


/**
 * File information related to the Bruker/Biospin scanner format.
 */

public class FileInfoBRUKER extends FileInfoBase {

    //~ Static fields/initializers -------------------------------------------------------------------------------------

    /** Use serialVersionUID for interoperability. */
    private static final long serialVersionUID = 292865443840539139L;

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** Whether the z resolution is set in the acqp or reco files*/
    private boolean haveZResol = false;

    /** The size of the reconstruction */
    private int recoSize = -1;
    
    /** The slice inversion time of the scan */
    private String sliceSeparationMode = null;

    /** The inversion time of an MR scan. */
    private double inversionTime;

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
            dialog.append("Slice separation mode:\t" + sliceSeparationMode + "\n");
        }

        if (getSliceThickness() > 0.0f) {
            dialog.append("Slice thickness:\t" + getSliceThickness() + " mm\n");
        }
        
        if(inversionTime != 0) {
            dialog.append("Inversion time:\t\t" + inversionTime + " ms\n");
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
     * Gets the size of the reconstruction.
     *
     * @return  The reco size.
     */
    public int getRecoSize() {
        return recoSize;
    }
    
    /**
     * Gets the inversion time of the scan.
     * 
     * @return The inversion time.
     */
    public double getInversionTime() {
        return inversionTime;
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

    /**
     * Accessor to set the inversion time of the scan
     * 
     * @param inversionTime the inversion time of the scan
     */
    public void setInversionTime(double inversionTime) {
        this.inversionTime = inversionTime;        
    }
}
