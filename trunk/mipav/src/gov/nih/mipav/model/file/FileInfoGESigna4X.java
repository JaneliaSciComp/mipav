package gov.nih.mipav.model.file;


import gov.nih.mipav.model.algorithms.*;
import gov.nih.mipav.model.structures.*;

import gov.nih.mipav.view.dialogs.*;


/**
 * This structures contains the information that describes how a GE Signa 5X image is stored on disk.
 */

public class FileInfoGESigna4X extends FileInfoBase {

    //~ Static fields/initializers -------------------------------------------------------------------------------------

    /** Use serialVersionUID for interoperability. */
    //private static final long serialVersionUID = -4456298776612648834L;

    //~ Instance fields ------------------------------------------------------------------------------------------------

    private String studyNumber = null;
    
    private String studyDate = null;
    
    private String studyTime = null;
    
    private String patientName = null;
    
    private String patientID = null;
    
    private String patientAge = null;
    
    private String patientSex = null;
    
    private String seriesNumber = null;
    
    private String seriesDescription = null;
    
    private String seriesType = null;
    
    private String coilType = null;
    
    private String coilName = null;
    
    private short contrastDescription = -32768;
    
    private String imageMode = null;
    
    private short fieldStrength = -32768; // gauss
    
    private String pulseSequence = null;
    
    private String pulseSequenceSubtype = null;
    
    private float fieldOfView = Float.NaN;
    
    private float rlCenter = Float.NaN;
    
    private float apCenter = Float.NaN;
    
    private float siCenter = Float.NaN;
    
    private String orientation = null;
    
    private String position = null;
    
    private String longitudinalAnatomicalReference = null;
    
    private String verticalAnatomicalReference = null;
    
    private short scanMatrixX = -32768;
    
    private short scanMatrixY = -32768;
    
    private short imageMatrix = -32768;
    
    private String imageNumber = null;
    
    private float imageLocation = Float.NaN;
    
    private float tablePosition = Float.NaN;
    
    private float imageThickness = Float.NaN;
    
    private float tr = Float.NaN;
    
    private float te = Float.NaN;
    
    private float ti = Float.NaN;
    
    private short numberOfEchos = -32768;
    
    private short echoNumber = -32768;
    
    private short nexShort = -32768;
    
    private float nexFloat = Float.NaN;
    
    private short flipAngle = -32768;

    /** DOCUMENT ME! */
    private int year;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * File info storage constructor.
     *
     * @param  name       file name
     * @param  directory  directory
     * @param  format     file format
     */
    public FileInfoGESigna4X(String name, String directory, int format) {
        super(name, directory, format);
    }

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * DOCUMENT ME!
     *
     * @param   i  DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    public FileInfoDicom convertToDICOMInfo(int i) {

        FileInfoDicom fileInfo = null;
        int[] extents = null;
        int[] units = null;
        float[] resols = null;


        if (getExtents().length == 2) {
            extents = new int[2];
            resols = new float[2];
            units = new int[5];
            extents[0] = getExtents()[0];
            extents[1] = getExtents()[1];
            resols[0] = getResolutions()[0];
            resols[1] = getResolutions()[1];
            units[0] = FileInfoBase.MILLIMETERS;
            units[1] = FileInfoBase.MILLIMETERS;
            units[2] = FileInfoBase.MILLIMETERS;
            units[3] = FileInfoBase.MILLIMETERS;
            units[4] = FileInfoBase.MILLIMETERS;
        } else if (getExtents().length == 3) {
            extents = new int[3];
            resols = new float[3];
            units = new int[5];
            extents[0] = getExtents()[0];
            extents[1] = getExtents()[1];
            extents[2] = getExtents()[2];
            resols[0] = getResolutions()[0];
            resols[1] = getResolutions()[1];
            resols[2] = getResolutions()[2];
            units[0] = FileInfoBase.MILLIMETERS;
            units[1] = FileInfoBase.MILLIMETERS;
            units[2] = FileInfoBase.MILLIMETERS;
            units[3] = FileInfoBase.MILLIMETERS;
            units[4] = FileInfoBase.MILLIMETERS;
        }

        String name = JDialogBase.makeImageName(fileName, ".dcm");
        fileInfo = new FileInfoDicom(name, getFileDirectory(), FileBase.DICOM);
        fileInfo.setExtents(extents);
        fileInfo.setResolutions(resols);
        fileInfo.setUnitsOfMeasure(units);
        fileInfo.setDataType(ModelImage.SHORT);
        fileInfo.setImageOrientation(getImageOrientation());

        //
        // set a bunch of variables from GE to DICOM ....

        // fileInfo.setValue("0002,0001", version, 2);
        // fileInfo.setValue("0002,0002", "1.2.840.10008.5.1.4.1.1.7 ", 26); // Secondary Capture SOP UID
        fileInfo.setValue("0002,0003", "1.2.840.999999999999999999", 26); // bogus SOP Instance UID
        fileInfo.setValue("0002,0010", "1.2.840.10008.1.2 ", 18); // Little Endian transfer syntax
        fileInfo.setValue("0002,0012", "1.2.840.34379.17", 16); // bogus Implementation UID made up by Matt
        fileInfo.setValue("0002,0013", "MIPAV--NIH", 10); //

        fileInfo.setEndianess(FileBase.LITTLE_ENDIAN); // ??
        fileInfo.setRescaleIntercept(getRescaleIntercept()); // ??
        fileInfo.setRescaleSlope(getRescaleSlope()); // ??

        // Column and row
        fileInfo.setValue("0028,0011", new Short((short) getExtents()[0]), 2);
        fileInfo.setValue("0028,0010", new Short((short) getExtents()[1]), 2);

        fileInfo.setValue("0028,0100", new Short((short) 16), 2);
        fileInfo.setValue("0028,0101", new Short((short) 16), 2);
        fileInfo.setValue("0028,0102", new Short((short) 15), 2);
        fileInfo.setValue("0028,0002", new Short((short) 1), 2); // samples per pixel
        fileInfo.setValue("0028,0004", new String("MONOCHROME2 "), 12); // photometric
        fileInfo.setValue("0028,0103", new Short((short) 1), 2);

        // Instance number
        fileInfo.setValue("0020,0013", String.valueOf(i + 1).trim(), String.valueOf(i + 1).trim().length());

        // Pixel resolutions X, and Y.
        String s = String.valueOf(1.0f) + "\\" + String.valueOf(1.0f);
        String yearStr;
        String mmStr;
        String ddStr;
        String hhStr;
        String ssStr;
        fileInfo.setValue("0028,0030", s, s.length());

        // Slice thickness
        s = String.valueOf(imageThickness);
        fileInfo.setValue("0018,0050", s, s.length()); // slice thickness
        s = String.valueOf(getSliceSpacing());
        fileInfo.setValue("0018,0088", s, s.length()); // spacing between slices

        
        fileInfo.setValue("0008,0060", "MR", 2);
        fileInfo.setValue("0008,0016", "1.2.840.10008.5.1.4.1.1.4", 25);
        fileInfo.setValue("0002,0002", "1.2.840.10008.5.1.4.1.1.4", 25);

        // fileInfo.setValue("0008,0016", "1.2.840.10008.5.1.4.1.1.7 ", 26); // Secondary Capture UID
        fileInfo.setValue("0008,0018", "1.2.840.999999999999999999", 26); // bogus SOP Instance UID all secondary
                                                                          // capture info is installed by
                                                                          // FileDicom.writeImage(), under the
                                                                          // assumption that all saves must have been
                                                                          // modified (and need that stuff)

       
        year = Integer.valueOf(studyDate.substring(7)).intValue();
        if (year < 50) {
            yearStr = "20".concat(studyDate.substring(7));
        }
        else {
            yearStr = "19".concat(studyDate.substring(7)); 
        }
        
        mmStr = studyDate.substring(3,6);
        if (mmStr.equalsIgnoreCase("JAN")) {
            mmStr = "01";
        }
        else if (mmStr.equalsIgnoreCase("FEB")) {
            mmStr = "02";
        }
        else if (mmStr.equalsIgnoreCase("MAR")) {
            mmStr = "03";
        }
        else if (mmStr.equalsIgnoreCase("APR")) {
            mmStr = "04";
        }
        else if (mmStr.equalsIgnoreCase("MAY")) {
            mmStr = "05";
        }
        else if (mmStr.equalsIgnoreCase("JUN")) {
            mmStr = "06";
        }
        else if (mmStr.equalsIgnoreCase("JUL")) {
            mmStr = "07";
        }
        else if (mmStr.equalsIgnoreCase("AUG")) {
            mmStr = "08";
        }
        else if (mmStr.equalsIgnoreCase("SEP")) {
            mmStr = "09";
        }
        else if (mmStr.equalsIgnoreCase("OCT")) {
            mmStr = "10";
        }
        else if (mmStr.equalsIgnoreCase("NOV")) {
            mmStr = "11";
        }
        else {
            mmStr = "12";
        }
        
        ddStr = studyDate.substring(0,2);

        s = yearStr + mmStr + ddStr;
        fileInfo.setValue("0008,0020", s, s.length()); // Study date
        
        hhStr = studyTime.substring(0,2);
        mmStr = studyTime.substring(3,5);
        ssStr = studyTime.substring(6);

        s = hhStr + mmStr + ssStr + ".0";
        fileInfo.setValue("0008,0030", s, s.length()); // Study time

        fileInfo.setValue("0008,0050", "123456", 6);
        fileInfo.setValue("0008,103E", seriesDescription.trim(), seriesDescription.trim().length()); // Series description

        fileInfo.setValue("0010,0010", patientName.trim(), patientName.trim().length());
        fileInfo.setValue("0010,0020", patientID.trim(), patientID.trim().length());
        fileInfo.setValue("0010,1010", "0" + String.valueOf(patientAge), String.valueOf(patientAge).length() + 1);

        RandomNumberGen randomNum = new RandomNumberGen();
        randomNum.genUniformRandomNum(1, 100000);
        s = "1.2.840.34379.17.139875.234.455." + randomNum.genUniformRandomNum(1, 100000);
        fileInfo.setValue("0020,000D", s, s.length()); // study UID
        s = "1.2.840.34379.17.139875.234.456." + randomNum.genUniformRandomNum(1, 100000);
        fileInfo.setValue("0020,000E", s, s.length()); // series UID


        // study Number  (SH  short string)
        fileInfo.setValue("0020,0010", studyNumber, studyNumber.length());

        // series Number (IS integer string)
        fileInfo.setValue("0020,0011", seriesNumber, seriesNumber.length());              

        s = String.valueOf(imageLocation);
        fileInfo.setValue("0020,1041", s, s.length()); // slice location

        return fileInfo;
    }

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

        dialog.append("Image width  = " + 256 + "\n");
        dialog.append("Image height = " + 256 + "\n");
        dialog.append("Number of bits  = " + 12 + "\n");

        dialog.append("\nStudy header\n");
        if (studyNumber != null) {
            dialog.append("Study number = " + studyNumber.trim() + "\n");
        }
        
        if (studyDate != null) {
            dialog.append("Study date = " + studyDate.trim() + "\n");
        }
        
        if (studyTime != null) {
            dialog.append("Study time = " + studyTime.trim() + "\n");
        }
        
        if (patientName != null) {
            dialog.append("Patient name = " + patientName.trim() + "\n");
        }
        
        if (patientID != null) {
            dialog.append("Patient ID = " + patientID.trim() + "\n");
        }
        
        if (patientAge != null) {
            dialog.append("Patient age = " + patientAge.trim() + "\n");
        }
        
        if (patientSex != null) {
            dialog.append("Patient sex = " + patientSex.trim() + "\n");
        }
        
        dialog.append("\nSeries header\n");
        if (seriesNumber != null) {
            dialog.append("Series number = " + seriesNumber.trim() + "\n");
        }
        
        if (seriesDescription != null) {
            dialog.append("Series description = " + seriesDescription.trim() + "\n");
        }
        
        if (seriesType != null) {
            dialog.append("Series type = " + seriesType.trim() + "\n");
        }
        
        if (coilType != null) {
            dialog.append("Coil type = " + coilType.trim() + "\n");
        }
        
        if (coilName != null) {
            dialog.append("Coil name = " + coilName.trim() + "\n");
        }
        
        if (contrastDescription != -32768) {
            dialog.append("Contrast description = " + contrastDescription + "\n");
        }
        
        if (imageMode != null) {
            dialog.append("Image mode = " + imageMode.trim() + "\n");
        }
        
        if (fieldStrength != -32768) {
            dialog.append("Field strength = " + fieldStrength + " gauss\n");
        }
        
        if (pulseSequence != null) {
            dialog.append("Pulse sequence = " + pulseSequence.trim() + "\n");
        }
        
        if (pulseSequenceSubtype  != null) {
            dialog.append("Pulse sequence subtype = " + 
                    pulseSequenceSubtype.trim() + "\n");
        }
        
        if (!Float.isNaN(fieldOfView)) {
            dialog.append("Field of view = " + fieldOfView + " mm\n");
        }
        
        if (!Float.isNaN(rlCenter)) {
            dialog.append("R+L- center = " + rlCenter + "\n");
        }
        
        if (!Float.isNaN(apCenter)) {
            dialog.append("A+P- center = " + apCenter + "\n");
        }
        
        if (!Float.isNaN(siCenter)) {
            dialog.append("S+I- center = " + siCenter + "\n");
        }
        
        if (orientation != null) {
            dialog.append("Orientation = " + orientation.trim() + "\n");
        }
        
        if (position != null) {
            dialog.append("Position = " + position.trim() + "\n");
        }
        
        if (longitudinalAnatomicalReference != null) {
            dialog.append("Longitudinal anatomical reference = " +
                           longitudinalAnatomicalReference.trim() + "\n");
        }
        
        if (verticalAnatomicalReference != null) {
            dialog.append("Vertical anatomical reference = " +
                           verticalAnatomicalReference.trim() + "\n");
        }
        
        if (scanMatrixX != -32768) {
            dialog.append("Scan matrix X = " + scanMatrixX + "\n");
        }
        
        if (scanMatrixY != -32768) {
            dialog.append("Scan matrix Y = " + scanMatrixY + "\n");
        }
        
        if (imageMatrix != -32768) {
            dialog.append("Image matrix = " + imageMatrix + "\n");
        }
        
        dialog.append("\nImage header\n");
        
        if (imageNumber != null) {
            dialog.append("Image number = " + imageNumber.trim() + "\n");
        }
        
        if (!Float.isNaN(imageLocation)) {
            dialog.append("Image location = " + imageLocation + "\n");
        }
        
        if (!Float.isNaN(tablePosition)) {
            dialog.append("Table position = " + tablePosition + "\n");
        }
        
        if (!Float.isNaN(imageThickness)) {
            dialog.append("Image thickness = " + imageThickness + "\n");
        }
        
        dialog.append("Image spacing " + getSliceSpacing() + "\n");
        
        if (!Float.isNaN(tr)) {
            dialog.append("tr = " + tr + " usec\n");
        }
        
        if (!Float.isNaN(te)) {
            dialog.append("te = " + te + " usec\n");
        }
        
        if (!Float.isNaN(ti)) {
            dialog.append("ti = " + ti + " usec\n");
        }
        
        if (numberOfEchos != -32768) {
            dialog.append("Number of echos = " + numberOfEchos + "\n");
        }
        
        if (echoNumber != -32768) {
            dialog.append("Echo number = " + echoNumber + "\n");
        }
        
        if (nexShort != -32768) {
            dialog.append("NEX (if not fractional) = " + nexShort + "\n");
        }
        
        if (!Float.isNaN(nexFloat)) {
            dialog.append("NEX = " + nexFloat + "\n");
        }
        
        if (flipAngle != -32768) {
            dialog.append("Flip angle = " + flipAngle + "\n");
        }
        
        dialog.setSize(600, 500);
    }

    

    
    
    /**
     * 
     * @param studyNumber
     */
    public void setStudyNumber (String studyNumber){
        this.studyNumber = studyNumber;
    }
    
    /**
     * 
     * @param studyDate
     */
    public void setStudyDate (String studyDate){
        this.studyDate = studyDate;
    }
    
    /**
     * 
     * @param studyTime
     */
    public void setStudyTime (String studyTime){
        this.studyTime = studyTime;
    }
    
    /**
     * 
     * @param patientName
     */
    public void setPatientName(String patientName) {
        this.patientName = patientName;
    }
    
    /**
     * 
     * @param patientID
     */
    public void setPatientID(String patientID) {
        this.patientID = patientID;
    }
    
    /**
     * 
     * @param patientAge
     */
    public void setPatientAge(String patientAge) {
        this.patientAge = patientAge;
    }
    
    /**
     * 
     * @param patientSex
     */
    public void setPatientSex(String patientSex) {
        this.patientSex = patientSex;
    }
    
    /**
     * 
     * @param seriesNumber
     */
    public void setSeriesNumber(String seriesNumber) {
        this.seriesNumber = seriesNumber;
    }
    
    /**
     * 
     * @param seriesDescription
     */
    public void setSeriesDescription(String seriesDescription) {
        this.seriesDescription = seriesDescription;
    }
    
    /**
     * 
     * @param seriesType
     */
    public void setSeriesType(String seriesType) {
        this.seriesType = seriesType;
    }
    
    /**
     * 
     * @param coilType
     */
    public void setCoilType(String coilType) {
        this.coilType = coilType;
    }
    
    /**
     * 
     * @param coilName
     */
    public void setCoilName(String coilName) {
        this.coilName = coilName;
    }
    
    /**
     * 
     * @param contrastDescription
     */
    public void setContrastDescription(short contrastDescription) {
        this.contrastDescription = contrastDescription;
    }
    
    /**
     * 
     * @param imageMode
     */
    public void setImageMode(String imageMode) {
        this.imageMode = imageMode;
    }
    
    /**
     * 
     * @param fieldStrength
     */
    public void setFieldStrength(short fieldStrength) {
        this.fieldStrength = fieldStrength;
    }
    
    /**
     * 
     * @param pulseSequence
     */
    public void setPulseSequence(String pulseSequence) {
        this.pulseSequence = pulseSequence;
    }
    
    /**
     * 
     * @param pulseSequenceSubtype
     */
    public void setPulseSequenceSubtype(String pulseSequenceSubtype) {
        this.pulseSequenceSubtype = pulseSequenceSubtype;
    }
    
    public void setFieldOfView(float fieldOfView) {
        this.fieldOfView = fieldOfView;
    }
    
    /**
     * 
     * @param rlCenter
     */
    public void setRLCenter(float rlCenter) {
        this.rlCenter = rlCenter;
    }
    
    /**
     * 
     * @param apCenter
     */
    public void setAPCenter(float apCenter) {
        this.apCenter = apCenter;
    }
    
    /**
     * 
     * @param siCenter
     */
    public void setSICenter(float siCenter) {
        this.siCenter = siCenter;
    }
    
    /**
     * 
     * @param orientation
     */
    public void setOrientation(String orientation) {
        this.orientation = orientation;
    }
    
    /**
     * 
     * @param position
     */
    public void setPosition(String position) {
        this.position = position;
    }
    
    /**
     * 
     * @param longitudinalAnatomicalReference
     */
    public void setLongitudinalAnatomicalReference(String longitudinalAnatomicalReference) {
        this.longitudinalAnatomicalReference = longitudinalAnatomicalReference;
    }
    
    /**
     * 
     * @param verticalAnatomicalReference
     */
    public void setVerticalAnatomicalReference(String verticalAnatomicalReference) {
        this.verticalAnatomicalReference = verticalAnatomicalReference;
    }
    
    /**
     * 
     * @param scanMatrixX
     */
    public void setScanMatrixX(short scanMatrixX) {
        this.scanMatrixX = scanMatrixX;
    }
    
    /**
     * 
     * @param scanMatrixY
     */
    public void setScanMatrixY(short scanMatrixY) {
        this.scanMatrixY = scanMatrixY;
    }
    
    /**
     * 
     * @param imageMatrix
     */
    public void setImageMatrix(short imageMatrix) {
        this.imageMatrix = imageMatrix;
    }
    
    /**
     * 
     * @param imageNumber
     */
    public void setImageNumber(String imageNumber) {
        this.imageNumber = imageNumber;
    }
    
    /**
     * 
     * @param imageLocation
     */
    public void setImageLocation(float imageLocation) {
        this.imageLocation = imageLocation;
    }
    
    /**
     * 
     * @param tablePosition
     */
    public void setTablePosition(float tablePosition) {
        this.tablePosition = tablePosition;
    }
    
    /**
     * 
     * @param imageThickness
     */
    public void setImageThickness(float imageThickness) {
        this.imageThickness = imageThickness;
    }
    
    /**
     * 
     * @param tr
     */
    public void setTR(float tr) {
        this.tr = tr;
    }
    
    /**
     * 
     * @param te
     */
    public void setTE(float te) {
        this.te = te;
    }
    
    /**
     * 
     * @param ti
     */
    public void setTI(float ti) {
        this.ti = ti;
    }
    
    /**
     * 
     * @param numberOfEchos
     */
    public void setNumberOfEchos(short numberOfEchos) {
        this.numberOfEchos = numberOfEchos;
    }
    
    /**
     * 
     * @param echoNumber
     */
    public void setEchoNumber(short echoNumber) {
        this.echoNumber = echoNumber;
    }
    
    /**
     * 
     * @param nexShort
     */
    public void setNexShort(short nexShort) {
        this.nexShort = nexShort;
    }
    
    /**
     * 
     * @param nexFloat
     */
    public void setNexFloat(float nexFloat) {
        this.nexFloat = nexFloat;
    }
    
    public void setFlipAngle(short flipAngle) {
        this.flipAngle = flipAngle;
    }

}
