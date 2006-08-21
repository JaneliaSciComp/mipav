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

    private String systemID = null;
    
    private String systemConfigHospitalName = null;
    
    private String studyHeaderID = null;
    
    private String studyHeaderRevisionNumber = null;
    
    private short studyHeaderBlocks = -32768;
    
    private String MRIProcessName = null;
    
    private short studyTaskID = -32768;
    
    private String rawDataStudyNumber = null;
    
    private String studyNumber = null;
    
    private String rawDataSystemID = null;
    
    private String systemGenerationID = null;
    
    private String studyDate = null;
    
    private String studyTime = null;
    
    private String patientName = null;
    
    private String patientID = null;
    
    private String patientAge = null;
    
    private String patientSex = null;
    
    private int patientWeight = -1;
    
    private String referringPhysician = null;
    
    private String diognostician = null;
    
    private String operator = null;
    
    private String studyDescription = null;
    
    private String history = null;
    
    private String hospitalName = null;
    
    private String patientStatus = null;
    
    private String requestedNumber = null;
    
    private String seriesHeaderID = null;
    
    private String seriesHeaderRevisionNumber = null;
    
    private short seriesHeaderBlocks = -32768;
    
    private String seriesProcessName = null;
    
    private short seriesTaskID = -32768;
    
    private String originalSeriesNumber = null;
    
    private String seriesNumber = null;
    
    private String seriesRawDataSystemID = null;
    
    private String seriesSystemGenerationID = null;
    
    private String seriesDate = null;
    
    private String seriesTime = null;
    
    private String seriesDescription = null;
    
    private String seriesType = null;
    
    private String coilType = null;
    
    private String coilName = null;
    
    private String contrastDescription = null;
    
    private String planeType = null;
    
    private String planeName = null;
    
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
    
    private float verticalLandmark = Float.NaN;
    
    private float horizontalLandmark = Float.NaN;
    
    private float tableLocation = Float.NaN;
    
    private short scanMatrixX = -32768;
    
    private short scanMatrixY = -32768;
    
    private short imageMatrix = -32768;
    
    private short imagesAllocated = -32768;
    
    private String gatingType = null;
    
    private String pulseSequenceMode = null;
    
    private String seriesPSDName = null;
    
    private int landmarkCounter = Integer.MIN_VALUE;
    
    private String scanProtocolName = null;
    
    private String surfaceCoilType = null;
    
    private String suppressionTechnique = null;
    
    private String satSelections = null;
    
    private String surfaceCoilIntensityCorrection = null;
    
    private short satXLoc1 = -32768;
    
    private short satXLoc2 = -32768;
    
    private short satYLoc1 = -32768;
    
    private short satYLoc2 = -32768;
    
    private short satZLoc1 = -32768;
    
    private short satZLoc2 = -32768;
    
    private short satXThick = -32768;
    
    private short satYThick = -32768;
    
    private short satZThick = -32768;
    
    private String vasMode = null;
    
    private String phaseContrastFlowAxis = null;
    
    private String imageHeaderID = null;
    
    private String imageHeaderRevisionNumber = null;
    
    private String imageCreationDate = null;
    
    private String imageCreationTime = null;
    
    private String imageNumber = null;
    
    private String series = null;
    
    private float imageLocation = Float.NaN;
    
    private float tablePosition = Float.NaN;
    
    private float thickness = Float.NaN; // In theory 20 * resolution[0] = 20 * resolution[1]
                                         // In practice 10 * res[0] = 10 * res[1]
    
    private float tr = Float.NaN;
    
    private float ts = Float.NaN;
    
    private float te = Float.NaN;
    
    private float ti = Float.NaN;
    
    private short numberOfEchos = -32768;
    
    private short echoNumber = -32768;
    
    private short sliceQuantity = -32768;
    
    private float pixelSize = Float.NaN; // In theory 20 * res[2]
                                         // In practice 5 * res[2]
    
    private short averagesNumber = -32768;
    
    private String researchMode = null;
    
    private String psdFileName = null;
    
    private String graphicallyPrescribed = null;
    
    private String prescribedSeriesNumbers = null;
    
    private String prescribedImageNumbers = null;
    
    private float excitationsNumber = Float.NaN;
    
    private float peakSAR = Float.NaN;
    
    private float averageSAR = Float.NaN;
    
    private String SARMonitored = null;
    
    private String contiguousSlices = null;
    
    private short cardiacHeartRate = -32768;
    
    private float totalPostTriggerDelayTime = Float.NaN;
    
    private short arrythmiaRejectionRatio = -32768;
    
    private short cardiacRepTime = -32768;
    
    private short imagesPerCardiacCycle = -32768;
    
    private int scanARRs = Integer.MIN_VALUE;
    
    private short transmitAttenuatorSetting = -32768;
    
    private short receiveAttenuatorSetting = -32768;
    
    private short imageOffset = -32768;
    
    private float interImageDelay = Float.NaN;
    
    private String psdName = null;
    
    private short flipAngle = -32768;
    
    private String extremityCoil = null;
    
    private float rCenter = Float.NaN;
    
    private float aCenter = Float.NaN;
    
    private float sCenter = Float.NaN;
    
    private float rNormal = Float.NaN;
    
    private float aNormal = Float.NaN;
    
    private float sNormal = Float.NaN;
    
    private float imgTLHC_R = Float.NaN;
    
    private float imgTLHC_A = Float.NaN;
    
    private float imgTLHC_S = Float.NaN;
    
    private float imgTRHC_R = Float.NaN;
    
    private float imgTRHC_A = Float.NaN;
    
    private float imgTRHC_S = Float.NaN;
    
    private float imgBLHC_R = Float.NaN;
    
    private float imgBLHC_A = Float.NaN;
    
    private float imgBLHC_S = Float.NaN;
    
    private short minimumDelay = -32768;
    
    private short sliceMultiplier = -32768;
    
    private short pauseInterval = -32768;
    
    private float pauseTime = Float.NaN;
    
    private String contrastUsed = null;
    
    private String contrastAgent = null;
    
    private float contrastAmount = Float.NaN;
    
    private short fileFormat = -32768;
    
    private short autoCenterFrequency = -32768;

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
        String s = String.valueOf(resols[0]) + "\\" + String.valueOf(resols[1]);
        String yearStr;
        String mmStr;
        String ddStr;
        String hhStr;
        String ssStr;
        fileInfo.setValue("0028,0030", s, s.length());

        // Slice thickness
        s = String.valueOf(getResolutions()[2]);
        fileInfo.setValue("0018,0050", s, s.length()); // slice thickness
        s = String.valueOf(getResolutions()[2]);
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
        
        year = Integer.valueOf(seriesDate.substring(7)).intValue();
        if (year < 50) {
            yearStr = "20".concat(seriesDate.substring(7));
        }
        else {
            yearStr = "19".concat(seriesDate.substring(7)); 
        }
        
        mmStr = seriesDate.substring(3,6);
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
        
        ddStr = seriesDate.substring(0,2);

        s = yearStr + mmStr + ddStr;
        fileInfo.setValue("0008,0021", s, s.length()); // Series date
        
        hhStr = seriesTime.substring(0,2);
        mmStr = seriesTime.substring(3,5);
        ssStr = seriesTime.substring(6);

        s = hhStr + mmStr + ssStr + ".0";
        fileInfo.setValue("0008,0031", s, s.length()); // Series time
        
        year = Integer.valueOf(imageCreationDate.substring(7)).intValue();
        if (year < 50) {
            yearStr = "20".concat(imageCreationDate.substring(7));
        }
        else {
            yearStr = "19".concat(imageCreationDate.substring(7)); 
        }
        
        mmStr = imageCreationDate.substring(3,6);
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
        
        ddStr = imageCreationDate.substring(0,2);

        s = yearStr + mmStr + ddStr;
        fileInfo.setValue("0008,0023", s, s.length()); // Image date
        
        hhStr = imageCreationTime.substring(0,2);
        mmStr = imageCreationTime.substring(3,5);
        ssStr = imageCreationTime.substring(6);

        s = hhStr + mmStr + ssStr + ".0";
        fileInfo.setValue("0008,0033", s, s.length()); // Image time

        fileInfo.setValue("0008,0050", "123456", 6);
        fileInfo.setValue("0008,0080", hospitalName.trim(), hospitalName.trim().length()); // Institution name
        fileInfo.setValue("0008,1030", studyDescription.trim(), studyDescription.trim().length()); // Study description
        fileInfo.setValue("0008,103E", seriesDescription.trim(), seriesDescription.trim().length()); // Series description

        fileInfo.setValue("0010,0010", patientName.trim(), patientName.trim().length());
        fileInfo.setValue("0010,0020", patientID.trim(), patientID.trim().length());
        fileInfo.setValue("0010,1010", "0" + String.valueOf(patientAge), String.valueOf(patientAge).length() + 1);
        fileInfo.setValue("0010,21B0", history.trim(), history.trim().length());

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
        
        s = -imgTLHC_R + "\\" + -imgTLHC_A + "\\" + imgTLHC_S;

        // s = imgTLHC_R + "\\" + imgTLHC_A + "\\" + imgTLHC_S;
        fileInfo.setValue("0020,0032", s, s.length()); // image position Right center .....
        
        // int[] orients = getAxisOrientation();
        float[] dicomOrients = new float[6];

        for (int j = 0; j < 6; j++) {
            dicomOrients[j] = 0;
        }

        if (imageOrientation == SAGITTAL) {

            if (axisOrientation[0] == ORI_A2P_TYPE) {
                dicomOrients[1] = 1f;
            } else {
                dicomOrients[1] = -1f;
            }

            if (axisOrientation[1] == ORI_I2S_TYPE) {
                dicomOrients[5] = 1f;
            } else {
                dicomOrients[5] = -1f;
            }
        } else if (imageOrientation == CORONAL) {

            if (axisOrientation[0] == ORI_R2L_TYPE) {
                dicomOrients[0] = 1f;
            } else {
                dicomOrients[0] = -1f;
            }

            if (axisOrientation[1] == ORI_I2S_TYPE) {
                dicomOrients[5] = 1f;
            } else {
                dicomOrients[5] = -1f;
            }
        } else { // AXIAL, default

            if (axisOrientation[0] == ORI_R2L_TYPE) {
                dicomOrients[0] = 1f;
            } else {
                dicomOrients[0] = -1f;
            }

            if (axisOrientation[1] == ORI_A2P_TYPE) {
                dicomOrients[4] = 1f;
            } else {
                dicomOrients[4] = -1f;
            }
        }

        s = "";

        for (int j = 0; j < 5; j++) {
            s += dicomOrients[j] + "\\";
        }

        s += dicomOrients[5];
        fileInfo.setValue("0020,0037", s, s.length()); // image orientation

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
        
        dialog.append("\nSystem configuration header\n");
        if (systemID != null) {
            dialog.append("System ID = " + systemID.trim() + "\n");
        }
        
        if (systemConfigHospitalName != null) {
            dialog.append("Hospital name = " + systemConfigHospitalName.trim() + "\n");
        }

        dialog.append("\nStudy header\n");
        if (studyHeaderID != null) {
            dialog.append("Study header ID = " + studyHeaderID.trim() + "\n");
        }
        
        if (studyHeaderRevisionNumber != null) {
            dialog.append("Study header revision number = " + studyHeaderRevisionNumber.trim() + "\n");
        }
        
        if (studyHeaderBlocks >= 0) {
            dialog.append("Number of study header blocks = " + studyHeaderBlocks + "\n");
        }
        
        if (MRIProcessName != null) {
            dialog.append("MRI process name = " + MRIProcessName.trim() + "\n");
        }
        
        if (studyTaskID != -32768) {
            dialog.append("Unique processes task ID of creator = " + studyTaskID + "\n");
        }
        
        if (rawDataStudyNumber != null) {
            dialog.append("Raw data study number = " + rawDataStudyNumber.trim() + "\n");
        }
        
        if (studyNumber != null) {
            dialog.append("Study number = " + studyNumber.trim() + "\n");
        }
        
        if ((rawDataSystemID != null) && (rawDataSystemID.trim() != null) &&
            (rawDataSystemID.trim().length() != 0)) {
            dialog.append("Raw data ID from original study number = " + rawDataSystemID.trim() + "\n");
        }
        
        if (systemGenerationID != null) {
            dialog.append("System generation ID = " + systemGenerationID.trim() + "\n");
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
        
        if (patientWeight != -1) {
            dialog.append("Patient weight = " + patientWeight + " grams\n");
        }
        
        if (referringPhysician != null) {
            dialog.append("Referring physician = " + referringPhysician.trim() + "\n");
        }
        
        if (diognostician != null) {
            dialog.append("Diognostician = " + diognostician.trim() + "\n");
        }
        
        if (operator != null) {
            dialog.append("Operator = " + operator.trim() + "\n");
        }
        
        if (studyDescription != null) {
            dialog.append("Study description = " + studyDescription.trim() + "\n");
        }
        
        if (history != null) {
            dialog.append("History = " + history.trim() + "\n");
        }
        
        if (hospitalName != null) {
            dialog.append("Hospital name = " + hospitalName.trim() + "\n");
        }
        
        if (patientStatus != null) {
            dialog.append("Patient status = " + patientStatus + "\n");
        }
        
        if ((requestedNumber != null) && (requestedNumber.trim() != null) &&
            (requestedNumber.trim().length() != 0)) {
            dialog.append("Requested number for patient logging system = " + requestedNumber.trim() + "\n");
        }
        
        dialog.append("\nSeries header\n");
        if (seriesHeaderID != null) {
            dialog.append("Series header ID = " + seriesHeaderID.trim() + "\n");
        }
        
        if (seriesHeaderRevisionNumber != null) {
            dialog.append("Series header revision number = " + seriesHeaderRevisionNumber.trim() + "\n");
        }
        
        if (seriesHeaderBlocks >= 0) {
            dialog.append("Number of series header blocks = " + seriesHeaderBlocks + "\n");
        }
        
        if (seriesProcessName != null) {
            dialog.append("MRI process name = " + seriesProcessName.trim() + "\n");    
        }
        
        if (seriesTaskID != -32768) {
            dialog.append("Task ID of creating task within process = " + seriesTaskID + "\n");
        }
        
        if (originalSeriesNumber != null) {
            dialog.append("Original series number = " + originalSeriesNumber.trim() + "\n");
        }
        
        if (seriesNumber != null) {
            dialog.append("Series number = " + seriesNumber.trim() + "\n");
        }
        
        if (seriesRawDataSystemID != null) {
            dialog.append("Raw data system ID from study = " + seriesRawDataSystemID.trim() + "\n");
        }
        
        if (seriesSystemGenerationID != null) {
            dialog.append("System generation ID = " + seriesSystemGenerationID.trim() + "\n");
        }
        
        if (seriesDate != null) {
            dialog.append("Series date = " + seriesDate.trim() + "\n");
        }
        
        if (seriesTime != null) {
            dialog.append("Series time = " + seriesTime.trim() + "\n");
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
        
        if ((coilName != null) && (coilName.trim() != null) &&
            (coilName.trim().length() != 0)) {
            dialog.append("Coil name = " + coilName.trim() + "\n");
        }
        
        if ((contrastDescription != null) && (contrastDescription.trim() != null) &&
            (contrastDescription.trim().length() != 0)) {
            dialog.append("Contrast description = " + contrastDescription.trim() + "\n");
        }
        
        if (planeType != null) {
            dialog.append("Plane type = " + planeType + "\n");
        }
        
        if (planeName != null) {
            dialog.append("Plane name = " + planeName.trim() + "\n");
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
        
        dialog.append("Center is relative to patient landmark\n");
        
        if (!Float.isNaN(rlCenter)) {
            dialog.append("R+L- center = " + rlCenter + "\n"); // MIPAV is R to L
        }
        
        if (!Float.isNaN(apCenter)) {
            dialog.append("A+P- center = " + apCenter + "\n"); // MIPAV is A to P
        }
        
        if (!Float.isNaN(siCenter)) {
            dialog.append("S+I- center = " + siCenter + "\n"); // MIPAV is I to S
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
        
        if ((verticalAnatomicalReference != null) &&
            (verticalAnatomicalReference.trim() != null) &&
            (verticalAnatomicalReference.trim().length() != 0)) {
            dialog.append("Vertical anatomical reference = " +
                           verticalAnatomicalReference.trim() + "\n");
        }
        
        if (!Float.isNaN(verticalLandmark)) {
            dialog.append("Vertical landmark = " + verticalLandmark + " relative to table top in millimeters\n");
        }
        
        if (!Float.isNaN(horizontalLandmark)) {
            dialog.append("Horizontal landmark = " + horizontalLandmark + " relative to table center in millimeters\n");
        }
        
        if (!Float.isNaN(tableLocation)) {
            dialog.append("Physical table location relative to home = " + tableLocation + "\n");
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
        
        if (imagesAllocated != -32768) {
            dialog.append("Number of images allocated = " + imagesAllocated + "\n");
        }
        
        if (gatingType != null) {
            dialog.append("Gating type = " + gatingType + "\n");
        }
        
        if (pulseSequenceMode != null) {
            dialog.append("Pulse sequence mode = " + pulseSequenceMode + "\n");
        }
        
        if ((seriesPSDName != null) && (seriesPSDName.trim() != null) &&
            (seriesPSDName.trim().length() != 0)) {
            dialog.append("PSD name = " + seriesPSDName.trim() + "\n");
        }
        
        if (landmarkCounter >= 0) {
            dialog.append("Number of times landmark established = " + landmarkCounter + "\n");
        }
        
        if (scanProtocolName != null) {
            dialog.append("Protocol name for scan = " + scanProtocolName.trim() + "\n");
        }
        
        if (surfaceCoilType != null) {
            dialog.append("Surface coil type = " + surfaceCoilType + "\n");
        }
        
        if (suppressionTechnique != null) {
            dialog.append("Suppression technique = " + suppressionTechnique + "\n");
        }
        
        if (satSelections != null) {
            dialog.append("SAT selections = " + satSelections + "\n");
        }
        
        if (surfaceCoilIntensityCorrection != null) {
            dialog.append("Surface coil intensity correction = " + surfaceCoilIntensityCorrection + "\n");
        }
        
        if (satXLoc1 != -32768) {
            dialog.append("R-side SAT pulse location relative to landmark = " + satXLoc1 + 
                          " millimeters\n");
        }
        
        if (satXLoc2 != -32768) {
            dialog.append("L-side SAT pulse location relative to landmark = " + satXLoc2 + 
                          " millimeters\n");
        }
        
        if (satYLoc1 != -32768) {
            dialog.append("A-side SAT pulse location relative to landmark = " + satYLoc1 + 
                          " millimeters\n");
        }
        
        if (satYLoc2 != -32768) {
            dialog.append("P-side SAT pulse location relative to landmark = " + satYLoc2 + 
                          " millimeters\n");
        }
        
        if (satZLoc1 != -32768) {
            dialog.append("S-side SAT pulse location relative to landmark = " + satZLoc1 + 
                          " millimeters\n");
        }
        
        if (satZLoc2 != -32768) {
            dialog.append("I-side SAT pulse location relative to landmark = " + satZLoc2 + 
                          " millimeters\n");
        }
        
        if (satXThick != -32768) {
            dialog.append("Thickness of X-axis SAT pulses = " + satXThick + " millimeters\n");
        }
        
        if (satYThick != -32768) {
            dialog.append("Thickness of Y-axis SAT pulses = " + satYThick + " millimeters\n");
        }
        
        if (satZThick != -32768) {
            dialog.append("Thickness of Z-axis SAT pulses = " + satZThick + " millimeters\n");
        }
        
        if (vasMode != null) {
            dialog.append("TOF/PC image = " + vasMode + "\n");
        }
        
        if (phaseContrastFlowAxis != null) {
            dialog.append("Phase contrast flow axis = " + phaseContrastFlowAxis + "\n");
        }
        
        dialog.append("\nImage header\n");
        
        if (imageHeaderID != null) {
            dialog.append("Image header ID = " + imageHeaderID.trim() + "\n");
        }
        
        if (imageHeaderRevisionNumber != null) {
            dialog.append("Image header revision number = " + imageHeaderRevisionNumber.trim() + "\n");
        }
        
        if (imageCreationDate != null) {
            dialog.append("Image creation date = " + imageCreationDate.trim() + "\n");
        }
        
        if (imageCreationTime != null) {
            dialog.append("Image creation time = " + imageCreationTime.trim() + "\n");
        }
        
        if (imageNumber != null) {
            dialog.append("Image number = " + imageNumber.trim() + "\n");
        }
        
        if (series != null) {
            dialog.append("Series = " + series.trim() + "\n");
        }
        
        if (!Float.isNaN(imageLocation)) {
            dialog.append("Image location = " + imageLocation + "\n");
        }
        
        if (!Float.isNaN(tablePosition)) {
            dialog.append("Table position = " + tablePosition + "\n");
        }
        
        if (!Float.isNaN(thickness)) {
            dialog.append("10*resolution[0] = 10*resolution[1] equals approximately " + thickness + "\n");
        }
        
        if (getSliceSpacing() != 0.0) {
            dialog.append("Image spacing = " + getSliceSpacing() + "\n");
        }
        
        if (!Float.isNaN(tr)) {
            dialog.append("Repetition/recovery time tr = " + tr + " usec\n");
        }
        
        if (!Float.isNaN(ts)) {
            dialog.append("Scan time ts = " + ts + "\n");
        }
        
        if (!Float.isNaN(te)) {
            dialog.append("Echo delay te = " + te + " usec\n");
        }
        
        if (!Float.isNaN(ti)) {
            dialog.append("Inversion time ti = " + ti + " usec\n");
        }
        
        if (numberOfEchos != -32768) {
            dialog.append("Number of echos = " + numberOfEchos + "\n");
        }
        
        if (echoNumber != -32768) {
            dialog.append("Echo number = " + echoNumber + "\n");
        }
        
        if (sliceQuantity > 0) {
            dialog.append("Number of slices in scan group = " + sliceQuantity + "\n");
        }
        
        if (!Float.isNaN(pixelSize)) {
            dialog.append("5 * pixel resolution[2] = " + pixelSize + "\n");
        }
        
        if (averagesNumber != -32768) {
            dialog.append("Number of averages = " + averagesNumber + "\n");
        }
        
        if (researchMode != null) {
            dialog.append(researchMode + "\n");
        }
        
        if ((psdFileName != null) && (psdFileName.trim() != null) &&
            (psdFileName.trim().length() != 0)) {
            dialog.append("PSD file name = " + psdFileName.trim() + "\n");
        }
        
        if (graphicallyPrescribed != null) {
            dialog.append(graphicallyPrescribed + "\n");
        }
        
        if ((prescribedSeriesNumbers != null) && 
           (prescribedSeriesNumbers.trim() != null) &&
           (prescribedSeriesNumbers.trim().length() != 0)) {
            dialog.append("Prescribed series numbers = " +
                           prescribedSeriesNumbers.trim() + "\n");
        }
        
        if ((prescribedImageNumbers != null) && 
                (prescribedImageNumbers.trim() != null) &&
                (prescribedImageNumbers.trim().length() != 0)) {
                 dialog.append("Prescribed image numbers = " +
                                prescribedImageNumbers.trim() + "\n");
             }
        
        if (!Float.isNaN(excitationsNumber)) {
            dialog.append("Number of excitations = " + excitationsNumber + "\n");
        }
        
        if (!Float.isNaN(peakSAR)) {
            dialog.append("Value of peak SAR = " + peakSAR + "\n");
        }
        
        if (!Float.isNaN(averageSAR)) {
            dialog.append("Value of average SAR = " + averageSAR + "\n");
        }
        
        if (SARMonitored != null) {
            dialog.append(SARMonitored + "\n");
        }
        
        if (contiguousSlices != null) {
            dialog.append(contiguousSlices + "\n");
        }
        
        if (cardiacHeartRate != -32768) {
            dialog.append("Cardiac heart rate = " + cardiacHeartRate + "\n");
        }
        
        if (!Float.isNaN(totalPostTriggerDelayTime)) {
            dialog.append("Total delay time after trigger = " +
                           totalPostTriggerDelayTime + "\n");
        }
        
        if (arrythmiaRejectionRatio != -32768) {
            dialog.append("Arrythmia rejection ratio = " + 
                           arrythmiaRejectionRatio + "\n");
        }
        
        if (cardiacRepTime != -32768) {
            dialog.append("Cardiac rep time = " + cardiacRepTime + "\n");
        }
        
        if (imagesPerCardiacCycle != -32768) {
            dialog.append("Images per cardiac cycle = " +
                           imagesPerCardiacCycle + "\n");
        }
        
        if (scanARRs != Integer.MIN_VALUE) {
            dialog.append("Number of ARR's during the scan = " + scanARRs + "\n");
        }
        
        if (transmitAttenuatorSetting != -32768) {
            dialog.append("Transmit attenuator setting = " +
                           transmitAttenuatorSetting + "\n");
        }
        
        if (receiveAttenuatorSetting != -32768) {
            dialog.append("Receive attenuator setting = " +
                           receiveAttenuatorSetting + "\n");
        }
        
        if (imageOffset != -32768) {
            dialog.append("Image offset = " + imageOffset + "\n");
        }
        
        if (!Float.isNaN(interImageDelay)) {
            dialog.append("Inter image/ inter location delay = " + interImageDelay + "\n");
        }
        if ((psdName != null) && (psdName.trim() != null) &&
            (psdName.trim().length() != 0)) {
            dialog.append("PSD name = " + psdName.trim() + "\n");
        }
        
        if (flipAngle != -32768) {
            dialog.append("Flip angle = " + flipAngle + "\n");
        }
        
        if (extremityCoil != null) {
            dialog.append(extremityCoil + "\n");
        }
        
        if (!Float.isNaN(rCenter)) {
            dialog.append("R center coordinate = " + rCenter + "\n");
        }
        
        if (!Float.isNaN(aCenter)) {
            dialog.append("A center coordinate = " + aCenter + "\n");
        }
        
        if (!Float.isNaN(sCenter)) {
            dialog.append("S center coordinate = " + sCenter + "\n");
        }
        
        if (!Float.isNaN(rNormal)) {
            dialog.append("R normal coordinate = " + rNormal + "\n");
        }
        
        if (!Float.isNaN(aNormal)) {
            dialog.append("A normal coordinate = " + aNormal + "\n");
        }
        
        if (!Float.isNaN(sNormal)) {
            dialog.append("S normal coordinate = " + sNormal + "\n");
        }
        
        if (!Float.isNaN(imgTLHC_R)) { 
            dialog.append("Right top left hand corner = " + imgTLHC_R + "\n");
        }
        
        if (!Float.isNaN(imgTLHC_A)) {
            dialog.append("Anterior top left hand corner = " + imgTLHC_A + "\n");
        }
        
        if (!Float.isNaN(imgTLHC_S)) {
            dialog.append("Superior top left hand corner = " + imgTLHC_S + "\n");
        }
        
        if (!Float.isNaN(imgTRHC_R)) {
            dialog.append("Right top right hand corner = " + imgTRHC_R + "\n");
        }
        
        if (!Float.isNaN(imgTRHC_A)) {
            dialog.append("Anterior top right hand corner = " + imgTRHC_A + "\n");
        }
        
        if (!Float.isNaN(imgTRHC_S)) {
            dialog.append("Superior top right hand corner = " + imgTRHC_S + "\n");
        }
        
        if (!Float.isNaN(imgBLHC_R)) {
            dialog.append("Right bottom left hand corner = " + imgBLHC_R + "\n");
        }
        
        if (!Float.isNaN(imgBLHC_A)) {
            dialog.append("Anterior bottom left hand corner = " + imgBLHC_A + "\n");
        }
        
        if (!Float.isNaN(imgBLHC_S)) {
            dialog.append("Superior bottom left hand corner = " + imgBLHC_S + "\n");
        }
        
        if (minimumDelay != -32768) {
            dialog.append("Minimum delay after trigger = " + minimumDelay + "\n");
        }
        
        if (sliceMultiplier != -32768) {
            dialog.append("Multiplier of slices to obtain phase = " + 
                           sliceMultiplier + "\n");
        }
        
        if (pauseInterval != -32768) {
            dialog.append("Pause interval = " + pauseInterval + "\n");   
        }
        
        if (!Float.isNaN(pauseTime)) {
            dialog.append("Pause time = " + pauseTime + "\n");
        }
        
        if (contrastUsed != null) {
            dialog.append(contrastUsed + "\n");
        }
        
        if (contrastAgent != null) {
            dialog.append("Contrast agent = " + contrastAgent + "\n");
        }
        
        if (!Float.isNaN(contrastAmount)) {
            dialog.append("Contrast amount = " + contrastAmount + "\n");
        }
        
        if (fileFormat > 0) {
            dialog.append("File format = " + fileFormat + "\n");
        }
        
        if (autoCenterFrequency != -32768) {
            dialog.append("Auto center frequency = " + autoCenterFrequency + "\n");
        }
        
        dialog.setSize(600, 500);
    }

    
    /**
     * Gets the origin of a particular slice; resets for the z dimension.
     *
     * @return  New start locations
     */
    public float[] getOriginAtSlice() {
        float[] newOrigin = new float[3];

        for (int i = 0; i < 3; i++) {
            newOrigin[i] = origin[i];
        }

        switch (imageOrientation) {

            case CORONAL:
                newOrigin[2] = -imgTLHC_A;
                break;

            case SAGITTAL:
                newOrigin[2] = -imgTLHC_R;
                break;

            case AXIAL:
            default:
                newOrigin[2] = imgTLHC_S;
                break;
        }

        return newOrigin;
    }
    
    /**
     * 
     * @param systemID
     */
    public void setSystemID(String systemID) {
        this.systemID = systemID;
    }
    
    /**
     * 
     * @param systemConfigHospitalName
     */
    public void setSystemConfigHospitalName(String systemConfigHospitalName) {
        this.systemConfigHospitalName = systemConfigHospitalName;
    }
    
    /**
     * 
     * @param studyHeaderID
     */
    public void setStudyHeaderID(String studyHeaderID) {
        this.studyHeaderID = studyHeaderID;
    }
    
    /**
     * 
     * @param studyHeaderRevisionNumber
     */
    public void setStudyHeaderRevisionNumber(String studyHeaderRevisionNumber) {
        this.studyHeaderRevisionNumber = studyHeaderRevisionNumber;
    }
    
    /**
     * 
     * @param studyHeaderBlocks
     */
    public void setStudyHeaderBlocks(short studyHeaderBlocks) {
        this.studyHeaderBlocks = studyHeaderBlocks;
    }
    
    /**
     * 
     * @param MRIProcessName
     */
    public void setMRIProcessName(String MRIProcessName) {
        this.MRIProcessName = MRIProcessName;
    }
    
    /**
     * 
     * @param studyTaskID
     */
    public void setStudyTaskID(short studyTaskID) {
        this.studyTaskID = studyTaskID;
    }
    
    /**
     * 
     * @param rawDataStudyNumber
     */
    public void setRawDataStudyNumber(String rawDataStudyNumber) {
        this.rawDataStudyNumber = rawDataStudyNumber;
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
     * @param rawDataSystemID
     */
    public void setRawDataSystemID(String rawDataSystemID) {
        this.rawDataSystemID = rawDataSystemID;
    }
    
    /**
     * 
     * @param systemGenerationID
     */
    public void setSystemGenerationID(String systemGenerationID) {
        this.systemGenerationID = systemGenerationID;
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
     * @param patientWeight
     */
    public void setPatientWeight(int patientWeight) {
        this.patientWeight = patientWeight;
    }
    
    /**
     * 
     * @param referringPhysician
     */
    public void setReferringPhysician(String referringPhysician) {
        this.referringPhysician = referringPhysician;
    }
    
    /**
     * 
     * @param diognostician
     */
    public void setDiognostician(String diognostician) {
        this.diognostician = diognostician;
    }
    
    /**
     * 
     * @param operator
     */
    public void setOperator(String operator) {
        this.operator = operator;
    }
    
    /**
     * 
     * @param studyDescription
     */
    public void setStudyDescription(String studyDescription) {
        this.studyDescription = studyDescription;
    }
    
    /**
     * 
     * @param history
     */
    public void setHistory(String history) {
        this.history = history;
    }
    
    /**
     * 
     * @param hospitalName
     */
    public void setHospitalName(String hospitalName) {
        this.hospitalName = hospitalName;
    }
    
    /**
     * 
     * @param patientStatus
     */
    public void setPatientStatus(String patientStatus) {
        this.patientStatus = patientStatus;
    }
    
    /**
     * 
     * @param requestedNumber
     */
    public void setRequestedNumber(String requestedNumber) {
        this.requestedNumber = requestedNumber;
    }
    
    /**
     * 
     * @param seriesHeaderID
     */
    public void setSeriesHeaderID(String seriesHeaderID) {
        this.seriesHeaderID = seriesHeaderID;
    }
    
    /**
     * 
     * @param seriesHeaderRevisionNumber
     */
    public void setSeriesHeaderRevisionNumber(String seriesHeaderRevisionNumber) {
        this.seriesHeaderRevisionNumber = seriesHeaderRevisionNumber;
    }
    
    /**
     * 
     * @param seriesHeaderBlocks
     */
    public void setSeriesHeaderBlocks(short seriesHeaderBlocks) {
        this.seriesHeaderBlocks = seriesHeaderBlocks;
    }
    
    /**
     * 
     * @param seriesProcessName
     */
    public void setSeriesProcessName(String seriesProcessName) {
        this.seriesProcessName = seriesProcessName;
    }
    
    /**
     * 
     * @param seriesTaskID
     */
    public void setSeriesTaskID(short seriesTaskID) {
        this.seriesTaskID = seriesTaskID;
    }
    
    /**
     * 
     * @param originalSeriesNumber
     */
    public void setOriginalSeriesNumber(String originalSeriesNumber) {
        this.originalSeriesNumber = originalSeriesNumber;
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
     * @param seriesRawDataSystemID
     */
    public void setSeriesRawDataSystemID(String seriesRawDataSystemID) {
        this.seriesRawDataSystemID = seriesRawDataSystemID;
    }
    
    /**
     * 
     * @param seriesSystemGenerationID
     */
    public void setSeriesSystemGenerationID(String seriesSystemGenerationID) {
        this.seriesSystemGenerationID = seriesSystemGenerationID;
    }
    
    /**
     * 
     * @param seriesDate
     */
    public void setSeriesDate (String seriesDate){
        this.seriesDate = seriesDate;
    }
    
    /**
     * 
     * @param seriesTime
     */
    public void setSeriesTime (String seriesTime){
        this.seriesTime = seriesTime;
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
    public void setContrastDescription(String contrastDescription) {
        this.contrastDescription = contrastDescription;
    }
    
    /**
     * 
     * @param planeType
     */
    public void setPlaneType(String planeType) {
        this.planeType = planeType;
    }
    
    /**
     * 
     * @param planeName
     */
    public void setPlaneName(String planeName) {
        this.planeName = planeName;
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
     * @param verticalLandmark
     */
    public void setVerticalLandmark(float verticalLandmark) {
        this.verticalLandmark = verticalLandmark;
    }
    
    /**
     * 
     * @param horizontalLandmark
     */
    public void setHorizontalLandmark(float horizontalLandmark) {
        this.horizontalLandmark = horizontalLandmark;
    }
    
    /**
     * 
     * @param tableLocation
     */
    public void setTableLocation(float tableLocation) {
        this.tableLocation = tableLocation;
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
     * @param imagesAllocated
     */
    public void setImagesAllocated(short imagesAllocated) {
        this.imagesAllocated = imagesAllocated;
    }
    
    /**
     * 
     * @param gatingType
     */
    public void setGatingType(String gatingType) {
        this.gatingType = gatingType;
    }
    
    /**
     * 
     * @param pulseSequenceMode
     */
    public void setPulseSequenceMode(String pulseSequenceMode) {
        this.pulseSequenceMode = pulseSequenceMode;
    }
    
    /**
     * 
     * @param seriesPSDName
     */
    public void setSeriesPSDName(String seriesPSDName) {
        this.seriesPSDName = seriesPSDName;
    }
    
    /**
     * 
     * @param landmarkCounter
     */
    public void setLandmarkCounter(int landmarkCounter) {
        this.landmarkCounter = landmarkCounter;
    }
    
    /**
     * 
     * @param scanProtocolName
     */
    public void setScanProtocolName(String scanProtocolName) {
        this.scanProtocolName = scanProtocolName;
    }
    
    /**
     * 
     * @param surfaceCoilType
     */
    public void setSurfaceCoilType(String surfaceCoilType) {
        this.surfaceCoilType = surfaceCoilType;
    }
    
    /**
     * 
     * @param suppressionTechnique
     */
    public void setSuppressionTechnique(String suppressionTechnique) {
        this.suppressionTechnique = suppressionTechnique;
    }
    
    /**
     * 
     * @param satSelections
     */
    public void setSATSelections(String satSelections) {
        this.satSelections = satSelections;
    }
    
    /**
     * 
     * @param surfaceCoilIntensityCorrection
     */
    public void setSurfaceCoilIntensityCorrection(String surfaceCoilIntensityCorrection) {
        this.surfaceCoilIntensityCorrection = surfaceCoilIntensityCorrection;
    }
    
    /**
     * 
     * @param satXLoc1
     */
    public void setSATXLoc1(short satXLoc1) {
        this.satXLoc1 = satXLoc1;
    }
    
    /**
     * 
     * @param satXLoc2
     */
    public void setSATXLoc2(short satXLoc2) {
        this.satXLoc2 = satXLoc2;
    }
    
    /**
     * 
     * @param satYLoc1
     */
    public void setSATYLoc1(short satYLoc1) {
        this.satYLoc1 = satYLoc1;
    }
    
    /**
     * 
     * @param satYLoc2
     */
    public void setSATYLoc2(short satYLoc2) {
        this.satYLoc2 = satYLoc2;
    }
    
    /**
     * 
     * @param satZLoc1
     */
    public void setSATZLoc1(short satZLoc1) {
        this.satZLoc1 = satZLoc1;
    }
    
    /**
     * 
     * @param satZLoc2
     */
    public void setSATZLoc2(short satZLoc2) {
        this.satZLoc2 = satZLoc2;
    }
    
    /**
     * 
     * @param satXThick
     */
    public void setSATXThick(short satXThick) {
        this.satXThick = satXThick;
    }
    
    /**
     * 
     * @param satYThick
     */
    public void setSATYThick(short satYThick) {
        this.satYThick = satYThick;
    }
    
    /**
     * 
     * @param satZThick
     */
    public void setSATZThick(short satZThick) {
        this.satZThick = satZThick;
    }
    
    /**
     * 
     * @param vasMode
     */
    public void setVasMode(String vasMode) {
        this.vasMode = vasMode;
    }
    
    /**
     *
     * @param phaseContrastFlowAxis
     */
    public void setPhaseContrastFlowAxis(String phaseContrastFlowAxis) {
        this.phaseContrastFlowAxis = phaseContrastFlowAxis;
    }
    
    /**
     * 
     * @param imageHeaderID
     */
    public void setImageHeaderID(String imageHeaderID) {
        this.imageHeaderID = imageHeaderID;
    }
    
    /**
     * 
     * @param imageHeaderRevisionNumber
     */
    public void setImageHeaderRevisionNumber(String imageHeaderRevisionNumber) {
        this.imageHeaderRevisionNumber = imageHeaderRevisionNumber;
    }
    
    /**
     * 
     * @param imageCreationDate
     */
    public void setImageCreationDate (String imageCreationDate){
        this.imageCreationDate = imageCreationDate;
    }
    
    /**
     * 
     * @param imageCreationTime
     */
    public void setImageCreationTime (String imageCreationTime){
        this.imageCreationTime = imageCreationTime;
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
     * @param series
     */
    public void setSeries(String series) {
        this.series = series;
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
     * @param imageThickness = in theory 20*res[0] = 20*res[1]
     *                       = in practice 10*res[0] = 10 *res[1]
     */
    public void setThickness(float thickness) {
        this.thickness = thickness;
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
     * @param ts
     */
    public void setTS(float ts) {
        this.ts = ts;
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
     * @param sliceQuantity
     */
    public void setSliceQuantity(short sliceQuantity) {
        this.sliceQuantity  = sliceQuantity;
    }
    
    /**
     * 
     * @param pixelSize // 20 * pixel resolution[2]
     */
    public void setPixelSize(float pixelSize) {
        this.pixelSize = pixelSize;
    }
    
    /**
     * 
     * @param averagesNumber
     */
    public void setAveragesNumber(short averagesNumber) {
        this.averagesNumber = averagesNumber;
    }
    
    /**
     * 
     * @param researchMode
     */
    public void setResearchMode(String researchMode) {
        this.researchMode = researchMode;
    }
    
    /**
     * 
     * @param psdFileName
     */
    public void setPSDFileName(String psdFileName) {
        this.psdFileName = psdFileName;
    }
    
    /**
     * 
     * @param graphicallyPrescribed
     */
    public void setGraphicallyPrescribed(String graphicallyPrescribed) {
        this.graphicallyPrescribed = graphicallyPrescribed;
    }
    
    /**
     * 
     * @param prescribedSeriesNumbers
     */
    public void setPrescribedSeriesNumbers(String prescribedSeriesNumbers) {
        this.prescribedSeriesNumbers = prescribedSeriesNumbers;
    }
    
    /**
     * 
     * @param prescribedImageNumbers
     */
    public void setPrescribedImageNumbers(String prescribedImageNumbers) {
        this.prescribedImageNumbers = prescribedImageNumbers;
    }
    
    /**
     * 
     * @param excitationsNumber
     */
    public void setExcitationsNumber(float excitationsNumber) {
        this.excitationsNumber = excitationsNumber;
    }
    
    /**
     * 
     * @param peakSAR
     */
    public void setPeakSAR(float peakSAR) {
        this.peakSAR = peakSAR;
    }
    
    /**
     * 
     * @param averageSAR
     */
    public void setAverageSAR(float averageSAR) {
        this.averageSAR = averageSAR;
    }
    
    /**
     * 
     * @param SARMonitored
     */
    public void setSARMonitored(String SARMonitored) {
        this.SARMonitored = SARMonitored;
    }
    
    /**
     * 
     * @param contiguousSlices
     */
    public void setContiguousSlices(String contiguousSlices) {
        this.contiguousSlices = contiguousSlices;
    }
    
    /**
     * 
     * @param cardiacHeartRate
     */
    public void setCardiacHeartRate(short cardiacHeartRate) {
        this.cardiacHeartRate = cardiacHeartRate;
    }
    
    /**
     * 
     * @param totalPostTriggerDelayTime
     */
    public void setTotalPostTriggerDelayTime(float totalPostTriggerDelayTime) {
        this.totalPostTriggerDelayTime = totalPostTriggerDelayTime;
    }
    
    /**
     * 
     * @param arrythmiaRejectionRatio
     */
    public void setArrythmiaRejectionRatio(short arrythmiaRejectionRatio) {
        this.arrythmiaRejectionRatio = arrythmiaRejectionRatio;
    }
    
    /**
     * 
     * @param cardiacRepTime
     */
    public void setCardiacRepTime(short cardiacRepTime) {
        this.cardiacRepTime = cardiacRepTime;
    }
    
    /**
     * 
     * @param imagesPerCardiacCycle
     */
    public void setImagesPerCardiacCycle(short imagesPerCardiacCycle) {
        this.imagesPerCardiacCycle = imagesPerCardiacCycle;
    }
    
    /**
     * 
     * @param scanARRs
     */
    public void setScanARRs(int scanARRs) {
        this.scanARRs = scanARRs;
    }
    
    /**
     * 
     * @param transmitAttenuatorSetting
     */
    public void setTransmitAttenuatorSetting(short transmitAttenuatorSetting) {
        this.transmitAttenuatorSetting = transmitAttenuatorSetting;
    }
    
    /**
     * 
     * @param receiveAttenuatorSetting
     */
    public void setReceiveAttenuatorSetting(short receiveAttenuatorSetting) {
        this.receiveAttenuatorSetting = receiveAttenuatorSetting;
    }
    
    /**
     * 
     * @param imageOffset
     */
    public void setImageOffset(short imageOffset) {
        this.imageOffset = imageOffset;
    }
    
    /**
     * 
     * @param interImageDelay
     */
    public void setInterImageDelay(float interImageDelay) {
        this.interImageDelay = interImageDelay;
    }
    
    public void setPSDName(String psdName) {
        this.psdName = psdName;
    }
    
    /**
     * 
     * @param flipAngle
     */
    public void setFlipAngle(short flipAngle) {
        this.flipAngle = flipAngle;
    }
    
    /**
     * 
     * @param extremityCoil
     */
    public void setExtremityCoil(String extremityCoil) {
        this.extremityCoil = extremityCoil;
    }
    
    public void setMinimumDelay(short minimumDelay) {
        this.minimumDelay = minimumDelay;
    }
    
    /**
     * 
     * @param rCenter
     */
    public void setRCenter(float rCenter) {
        this.rCenter = rCenter;
    }
    
    /**
     * 
     * @param aCenter
     */
    public void setACenter(float aCenter) {
        this.aCenter = aCenter;
    }
    
    /**
     * 
     * @param sCenter
     */
    public void setSCenter(float sCenter) {
        this.sCenter = sCenter;
    }
    
    /**
     * 
     * @param rNormal
     */
    public void setRNormal(float rNormal) {
        this.rNormal = rNormal;
    }
    
    /**
     * 
     * @param aNormal
     */
    public void setANormal(float aNormal) {
        this.aNormal = aNormal;
    }
    
    /**
     * 
     * @param sNormal
     */
    public void setSNormal(float sNormal) {
        this.sNormal = sNormal;
    }
    
    /**
     * 
     * @param imgTLHC_R
     */
    public void setImgTLHC_R(float imgTLHC_R) {
        this.imgTLHC_R = imgTLHC_R;
    }
    
    /**
     * 
     * @param imgTLHC_A
     */
    public void setImgTLHC_A(float imgTLHC_A) {
        this.imgTLHC_A = imgTLHC_A;
    }
    
    /**
     * 
     * @param imgTLHC_S
     */
    public void setImgTLHC_S(float imgTLHC_S) {
        this.imgTLHC_S = imgTLHC_S;
    }
    
    /**
     * 
     * @param imgTRHC_R
     */
    public void setImgTRHC_R(float imgTRHC_R) {
        this.imgTRHC_R = imgTRHC_R;
    }
    
    /**
     * 
     * @param imgTRHC_A
     */
    public void setImgTRHC_A(float imgTRHC_A) {
        this.imgTRHC_A = imgTRHC_A;
    }
    
    /**
     * 
     * @param imgTRHC_S
     */
    public void setImgTRHC_S(float imgTRHC_S) {
        this.imgTRHC_S = imgTRHC_S;
    }
    
    /**
     * 
     * @param imgBLHC_R
     */
    public void setImgBLHC_R(float imgBLHC_R) {
        this.imgBLHC_R = imgBLHC_R;
    }
    
    /**
     * 
     * @param imgBLHC_A
     */
    public void setImgBLHC_A(float imgBLHC_A) {
        this.imgBLHC_A = imgBLHC_A;
    }
    
    /**
     * 
     * @param imgBLHC_S
     */
    public void setImgBLHC_S(float imgBLHC_S) {
        this.imgBLHC_S = imgBLHC_S;
    }
    
    /**
     * 
     * @param sliceMultiplier
     */
    public void setSliceMultiplier(short sliceMultiplier) {
        this.sliceMultiplier = sliceMultiplier;
    }
    
    /**
     * 
     * @param pauseInterval
     */
    public void setPauseInterval(short pauseInterval) {
        this.pauseInterval = pauseInterval;
    }
    
    /**
     * 
     * @param pauseTime
     */
    public void setPauseTime(float pauseTime) {
        this.pauseTime = pauseTime;
    }
    
    /**
     * 
     * @param contrastUsed
     */
    public void setContrastUsed(String contrastUsed) {
        this.contrastUsed = contrastUsed;
    }
    
    /**
     * 
     * @param contrastAgent
     */
    public void setContrastAgent(String contrastAgent) {
        this.contrastAgent = contrastAgent;
    }
    
    /**
     * 
     * @param contrastAmount
     */
    public void setContrastAmount(float contrastAmount) {
        this.contrastAmount = contrastAmount;
    }
    
    /**
     * 
     * @param fileFormat
     */
    public void setFileFormat(short fileFormat) {
        this.fileFormat = fileFormat;
    }
    
    /**
     * 
     * @param autoCenterFrequency
     */
    public void setAutoCenterFrequency(short autoCenterFrequency) {
        this.autoCenterFrequency = autoCenterFrequency;
    }

}
