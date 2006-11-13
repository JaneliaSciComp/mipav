package gov.nih.mipav.model.file;


import gov.nih.mipav.model.structures.*;

import gov.nih.mipav.view.dialogs.*;


/**
 * This structure contains the information that describes how an MedVision image is stored on disk, as well as header
 * information.
 *
 * @version  0.1 June 25, 1998
 * @author   Matthew J. McAuliffe, Ph.D.
 * @see      FileMedVision
 */


public class FileInfoMedVision extends FileInfoBase {

    //~ Static fields/initializers -------------------------------------------------------------------------------------

    /** Use serialVersionUID for interoperability. */
    private static final long serialVersionUID = -88313565378572414L;

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** DOCUMENT ME! */
    public double cineRate;

    /** DOCUMENT ME! */
    public short cols = -1;

    /** DOCUMENT ME! */
    public short dataType = -1;

    /** DOCUMENT ME! */
    public String dateOfBirth = null;

    /** DOCUMENT ME! */
    public String eqManuf = null;

    /** DOCUMENT ME! */
    public String eqType = null;

    /** File header data structure format. */
    public String fileType = null;

    /** DOCUMENT ME! */
    public short frameRefNum = -1;

    /** DOCUMENT ME! */
    public int imageOffset = -1;

    /** DOCUMENT ME! */
    public double imageScale = -1;

    /** DOCUMENT ME! */
    public String institution = null;

    /** DOCUMENT ME! */
    public String modality = null;

    /** Info data structure format. */
    public String patientName = null;

    /** DOCUMENT ME! */
    public double pixelSizeH = -1;

    /** hDat dataStructure format public long version;. */
    public double pixelSizeV = -1;

    /** DOCUMENT ME! */
    public String referringPhys = null;

    /** Slice Header data structure format. */
    public short rows = -1;

    /** DOCUMENT ME! */
    public String sex = null;

    /** DOCUMENT ME! */
    public short sliceHdrSize = -1;

    /** DOCUMENT ME! */
    public double slicePosition = -1;

    /** DOCUMENT ME! */
    public short sliceRefNum = -1;

    /** DOCUMENT ME! */
    public double sliceThickness = -1;

    /** DOCUMENT ME! */
    public double sliceTime = -1;

    /** DOCUMENT ME! */
    public short standardCols = -1;

    /** DOCUMENT ME! */
    public short standardRows = -1;

    /** DOCUMENT ME! */
    public String studyDate = null;

    /** DOCUMENT ME! */
    public String studyName = null;

    /** DOCUMENT ME! */
    public String studyTime = null;

    /** DOCUMENT ME! */
    public short totalSlices = -1;

    /** DOCUMENT ME! */
    public int version = -1;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * FileInfoMedVision.
     *
     * @param  name       the file name
     * @param  directory  the file directory
     * @param  format     the file format (MedVision)
     */
    public FileInfoMedVision(String name, String directory, int format) {
        super(name, directory, format);
    }

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * displayAboutInfo - displays the file information.
     *
     * @param  dlog    dialog box that is written to
     * @param  matrix  transformation matrix
     */
    public void displayAboutInfo(JDialogBase dlog, TransMatrix matrix) {
        JDialogText dialog = (JDialogText) dlog;
        displayPrimaryInfo(dialog, matrix);
        dialog.append("\n\n                Other information\n\n");

        // dialog.append("Patient name:     " + patientName + "\n");
        dialog.append("Institution:      " + institution + "\n");
        dialog.append("Study date:       " + studyDate + "\n");
        dialog.append("Study time:       " + studyTime + "\n");
        dialog.append("Date of birth:    " + dateOfBirth + "\n");

        // dialog.append("Sex:" + sex + "\n");
        dialog.append("Modality:         " + modality + "\n");
        dialog.append("Equipment maker:  " + eqManuf + "\n");
        dialog.append("Equipment model:  " + eqType + "\n");

        if (matrix != null) {

            // when using displayAboutInfo(dialog) this doesn't appear
            // calling prg might use an editing panel to adjust this matrix
            dialog.append("Matrix: \n" + matrix.matrixToString(10, 4) + "\n");
        }
    }
}
