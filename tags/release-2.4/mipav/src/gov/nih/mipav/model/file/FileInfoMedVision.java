package gov.nih.mipav.model.file;

import gov.nih.mipav.model.structures.*;
import gov.nih.mipav.view.dialogs.*;

/**
*   This structure contains the information that describes how
*   an MedVision image is stored on disk, as well as header information.
*
*		@version    0.1 June 25, 1998
*		@author     Matthew J. McAuliffe, Ph.D.
*       @see        FileMedVision
*
*
*/


public class FileInfoMedVision extends FileInfoBase {


    //Info data structure format
    public String patientName       = null;
    public String institution       = null;
    public String studyDate         = null;
    public String studyTime         = null;
    public String studyName         = null;
    public String dateOfBirth       = null;
    public String sex               = null;
    public String modality          = null;
    public String eqManuf           = null;
    public String eqType            = null;
    public String referringPhys     = null;
    public double cineRate;

    //hDat dataStructure format
    //public long   version;
    public double pixelSizeV      = -1;
    public double pixelSizeH      = -1;
    public double sliceThickness  = -1;
    public double slicePosition   = -1;
    public double sliceTime       = -1;
    public double imageScale      = -1;

    //File header data structure format
    public String  fileType         = null;
    public int     version          = -1;
    public int     imageOffset      = -1;
    public short   totalSlices      = -1;
    public short   sliceHdrSize     = -1;
    public short   standardRows     = -1;
    public short   standardCols     = -1;

    // Slice Header data structure format
    public short rows               = -1;
    public short cols               = -1;
    public short dataType           = -1;
    public short frameRefNum        = -1;
    public short sliceRefNum        = -1;

    /**
    *  FileInfoMedVision
    *  @param name         the file name
    *  @param directory    the file directory
    *  @param format       the file format (MedVision)
    */
    public FileInfoMedVision(String name, String directory, int format) {
        super(name, directory, format);
    }

    /**
    *  displayAboutInfo - displays the file information
    *  @param dlog    dialog box that is written to
    *  @param matrix  transformation matrix
    */
    public void displayAboutInfo(JDialogBase dlog, TransMatrix matrix){
        JDialogText dialog = (JDialogText) dlog;
        displayPrimaryInfo(dialog, matrix);
        dialog.append("\n\n                Other information\n\n");

        //dialog.append("Patient name:     " + patientName + "\n");
        dialog.append("Institution:      " + institution + "\n");
        dialog.append("Study date:       " + studyDate + "\n");
        dialog.append("Study time:       " + studyTime + "\n");
        dialog.append("Date of birth:    " + dateOfBirth + "\n");
        //dialog.append("Sex:" + sex + "\n");
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
