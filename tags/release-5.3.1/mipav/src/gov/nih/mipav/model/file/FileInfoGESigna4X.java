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
    private static final long serialVersionUID = -6230885244735581982L;

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** DOCUMENT ME! */
    private float aCenter = Float.NaN;

    /** DOCUMENT ME! */
    private int actualReceiveFrequency = Integer.MIN_VALUE;

    /** DOCUMENT ME! */
    private int actualTransmitFrequency = Integer.MIN_VALUE;

    /** DOCUMENT ME! */
    private float aNormal = Float.NaN;

    /** DOCUMENT ME! */
    private float apCenter = Float.NaN;

    /** DOCUMENT ME! */
    private short arrythmiaRejectionRatio = -32768;

    /** DOCUMENT ME! */
    private String autoCenterFrequency = null;

    /** DOCUMENT ME! */
    private String autoManualPrescan = null;

    /** DOCUMENT ME! */
    private float averageSAR = Float.NaN;

    /** DOCUMENT ME! */
    private short averagesNumber = -32768;

    /** DOCUMENT ME! */
    private short bitsPerPixel;

    /** DOCUMENT ME! */
    private short cardiacHeartRate = -32768;

    /** DOCUMENT ME! */
    private short cardiacPhaseNumber = -32768;

    /** DOCUMENT ME! */
    private String cardiacRepTime = null;

    /** DOCUMENT ME! */
    private String changedValues = null;

    /** DOCUMENT ME! */
    private String coilName = null;

    /** DOCUMENT ME! */
    private String coilType = null;

    /** DOCUMENT ME! */
    private String collapseImage = null;

    /** DOCUMENT ME! */
    private String concatenatedSATSelection = null;

    /** DOCUMENT ME! */
    private String contiguousSlices = null;

    /** DOCUMENT ME! */
    private String contrastAgent = null;

    /** DOCUMENT ME! */
    private float contrastAmount = Float.NaN;

    /** DOCUMENT ME! */
    private String contrastDescription = null;

    /** DOCUMENT ME! */
    private String contrastUsed = null;

    /** DOCUMENT ME! */
    private short cPhase = -32768;

    /** DOCUMENT ME! */
    private short defaultLevel = -32768;

    /** DOCUMENT ME! */
    private short defaultWindow = -32768;

    /** DOCUMENT ME! */
    private String diognostician = null;

    /** DOCUMENT ME! */
    private short echoNumber = -32768;

    /** DOCUMENT ME! */
    private int echoTrainLength = Integer.MIN_VALUE;

    /** DOCUMENT ME! */
    private float endX = Float.NaN;

    /** DOCUMENT ME! */
    private float endY = Float.NaN;

    /** DOCUMENT ME! */
    private float endZ = Float.NaN;

    /** DOCUMENT ME! */
    private float excitationsNumber = Float.NaN;

    /** DOCUMENT ME! */
    private String extremityCoil = null;

    /** DOCUMENT ME! */
    private float fieldOfView = Float.NaN;

    /** DOCUMENT ME! */
    private short fieldStrength = -32768; // gauss

    /** DOCUMENT ME! */
    private short fileBlocks = -32768;

    /** DOCUMENT ME! */
    private String fileFormat = null;

    /** DOCUMENT ME! */
    private short flipAngle = -32768;

    /** DOCUMENT ME! */
    private String fractionalEffectiveEcho = null;

    /** DOCUMENT ME! */
    private String gatingType = null;

    /** DOCUMENT ME! */
    private String gatingType2 = null;

    /** DOCUMENT ME! */
    private String graphicallyPrescribed = null;

    /** DOCUMENT ME! */
    private String histogramPresent = null;

    /** DOCUMENT ME! */
    private String history = null;

    /** DOCUMENT ME! */
    private float horizontalLandmark = Float.NaN;

    /** DOCUMENT ME! */
    private String hospitalName = null;

    /** DOCUMENT ME! */
    private String imageCreationDate = null;

    /** DOCUMENT ME! */
    private String imageCreationTime = null;

    /** DOCUMENT ME! */
    private int imageFieldStrength = Integer.MIN_VALUE;

    /** DOCUMENT ME! */
    private short imageHeaderBlocks = -32768;

    /** DOCUMENT ME! */
    private String imageHeaderCreatorProcess = null;

    /** DOCUMENT ME! */
    private short imageHeaderCreatorTask = -32768;

    /** DOCUMENT ME! */
    private short imageHeaderDisclaimer = -32768;

    /** DOCUMENT ME! */
    private String imageHeaderID = null;

    /** DOCUMENT ME! */
    private String imageHeaderRevisionNumber = null;

    /** DOCUMENT ME! */
    private float imageLocation = Float.NaN;

    /** DOCUMENT ME! */
    private short imageMatrix = -32768;

    /** DOCUMENT ME! */
    private String imageMode = null;

    /** DOCUMENT ME! */
    private String imageNumber = null;

    /** DOCUMENT ME! */
    private short imageOffset = -32768;

    /** DOCUMENT ME! */
    private String imageRawDataSystemID = null;

    /** DOCUMENT ME! */
    private short imagesAllocated = -32768;

    /** DOCUMENT ME! */
    private String imageShape = null;

    /** DOCUMENT ME! */
    private float imageSpacing = Float.NaN;

    /** DOCUMENT ME! */
    private short imagesPerCardiacCycle = -32768;

    /** DOCUMENT ME! */
    private String imageSystemGenerationID = null;

    /** DOCUMENT ME! */
    private String imageType = null;

    /** DOCUMENT ME! */
    private float imgBLHC_A = Float.NaN;

    /** DOCUMENT ME! */
    private float imgBLHC_R = Float.NaN;

    /** DOCUMENT ME! */
    private float imgBLHC_S = Float.NaN;

    /** DOCUMENT ME! */
    private float imgTLHC_A = Float.NaN;

    /** DOCUMENT ME! */
    private float imgTLHC_R = Float.NaN;

    /** DOCUMENT ME! */
    private float imgTLHC_S = Float.NaN;

    /** DOCUMENT ME! */
    private float imgTRHC_A = Float.NaN;

    /** DOCUMENT ME! */
    private float imgTRHC_R = Float.NaN;

    /** DOCUMENT ME! */
    private float imgTRHC_S = Float.NaN;

    /** DOCUMENT ME! */
    private float interImageDelay = Float.NaN;

    /** DOCUMENT ME! */
    private int landmarkCounter = Integer.MIN_VALUE;

    /** DOCUMENT ME! */
    private String longitudinalAnatomicalReference = null;

    /** DOCUMENT ME! */
    private short minimumDelay = -32768;

    /** DOCUMENT ME! */
    private String MRIProcessName = null;

    /** DOCUMENT ME! */
    private short numberOfEchos = -32768;

    /** DOCUMENT ME! */
    private String obliquePlane = null;

    /** DOCUMENT ME! */
    private String operator = null;

    /** DOCUMENT ME! */
    private String orientation = null;

    /** DOCUMENT ME! */
    private String originalSeriesNumber = null;

    /** DOCUMENT ME! */
    private String patientAge = null;

    /** DOCUMENT ME! */
    private String patientID = null;

    /** DOCUMENT ME! */
    private String patientName = null;

    /** DOCUMENT ME! */
    private String patientSex = null;

    /** DOCUMENT ME! */
    private String patientStatus = null;

    /** DOCUMENT ME! */
    private int patientWeight = -1;

    /** DOCUMENT ME! */
    private short pauseInterval = -32768;

    /** DOCUMENT ME! */
    private float pauseTime = Float.NaN;

    /** DOCUMENT ME! */
    private short PCVelocityEncoding = -32768;

    /** DOCUMENT ME! */
    private float peakSAR = Float.NaN;

    /** DOCUMENT ME! */
    private String pfSwapped = null;

    /** DOCUMENT ME! */
    private String phaseContrastFlowAxis = null;

    /** DOCUMENT ME! */
    private String pImage2 = null;

    /** DOCUMENT ME! */
    private float pixelSize = Float.NaN; // In practice 5 * res[2]

    /** DOCUMENT ME! */
    private String planeName = null;

    /** DOCUMENT ME! */
    private String planeType = null;

    /** DOCUMENT ME! */
    private String position = null;

    /** DOCUMENT ME! */
    private short prescanReceiveAttenuation1 = -32768;

    /** DOCUMENT ME! */
    private short prescanReceiveAttenuation2 = -32768;

    /** DOCUMENT ME! */
    private String prescribedImageNumbers = null;

    /** DOCUMENT ME! */
    private String prescribedSeriesNumbers = null;

    /** DOCUMENT ME! */
    private float projectionAngle = Float.NaN;

    /** DOCUMENT ME! */
    private short psdDay;

    /** DOCUMENT ME! */
    private String psdFileName = null;

    /** DOCUMENT ME! */
    private short psdHour;

    /** DOCUMENT ME! */
    private short psdMinute;

    /** DOCUMENT ME! */
    private short psdMonth;

    /** DOCUMENT ME! */
    private String psdName = null;

    /** DOCUMENT ME! */
    private short psdSeconds;

    /** DOCUMENT ME! */
    private short psdYear; // Year - 1900

    /** DOCUMENT ME! */
    private String pSeries2 = null;

    /** DOCUMENT ME! */
    private String pulseSequence = null;

    /** DOCUMENT ME! */
    private String pulseSequenceMode = null;

    /** DOCUMENT ME! */
    private String pulseSequenceSubtype = null;

    /** DOCUMENT ME! */
    private short R1 = -32768;

    /** DOCUMENT ME! */
    private short R2 = -32768;

    /** DOCUMENT ME! */
    private String rawDataStudyNumber = null;

    /** DOCUMENT ME! */
    private String rawDataSystemID = null;

    /** DOCUMENT ME! */
    private float rCenter = Float.NaN;

    /** DOCUMENT ME! */
    private short receiveAttenuatorSetting = -32768;

    /** DOCUMENT ME! */
    private int recommendedReceiveAttenuation = Integer.MIN_VALUE;

    /** DOCUMENT ME! */
    private int recommendedReceiveFrequency = Integer.MIN_VALUE;

    /** DOCUMENT ME! */
    private int recommendedTransmitAttenuation = Integer.MIN_VALUE;

    /** DOCUMENT ME! */
    private int recommendedTransmitFrequency = Integer.MIN_VALUE;

    /** DOCUMENT ME! */
    private String referringPhysician = null;

    /** DOCUMENT ME! */
    private String requestedNumber = null;

    /** DOCUMENT ME! */
    private String researchMode = null;

    /** DOCUMENT ME! */
    private float rlCenter = Float.NaN;

    /** DOCUMENT ME! */
    private float rNormal = Float.NaN;

    /** DOCUMENT ME! */
    private String round = null;

    /** DOCUMENT ME! */
    private String SARMonitored = null;

    /** DOCUMENT ME! */
    private String satSelections = null;

    /** DOCUMENT ME! */
    private short satXLoc1 = -32768;

    /** DOCUMENT ME! */
    private short satXLoc2 = -32768;

    /** DOCUMENT ME! */
    private short satXThick = -32768;

    /** DOCUMENT ME! */
    private short satYLoc1 = -32768;

    /** DOCUMENT ME! */
    private short satYLoc2 = -32768;

    /** DOCUMENT ME! */
    private short satYThick = -32768;

    /** DOCUMENT ME! */
    private short satZLoc1 = -32768;

    /** DOCUMENT ME! */
    private short satZLoc2 = -32768;

    /** DOCUMENT ME! */
    private short satZThick = -32768;

    /** DOCUMENT ME! */
    private short scanAcquisitionNumber = -32768;

    /** DOCUMENT ME! */
    private int scanARRs = Integer.MIN_VALUE;

    /** DOCUMENT ME! */
    private short scanMatrixX = -32768;

    /** DOCUMENT ME! */
    private short scanMatrixY = -32768;

    /** DOCUMENT ME! */
    private String scanProtocolName = null;

    /** DOCUMENT ME! */
    private float sCenter = Float.NaN;

    /** DOCUMENT ME! */
    private String scIma = null;

    /** DOCUMENT ME! */
    private String scSer = null;

    /** DOCUMENT ME! */
    private String series = null;

    /** DOCUMENT ME! */
    private String seriesDate = null;

    /** DOCUMENT ME! */
    private String seriesDescription = null;

    /** DOCUMENT ME! */
    private short seriesHeaderBlocks = -32768;

    /** DOCUMENT ME! */
    private String seriesHeaderID = null;

    /** DOCUMENT ME! */
    private String seriesHeaderRevisionNumber = null;

    /** DOCUMENT ME! */
    private String seriesNumber = null;

    /** DOCUMENT ME! */
    private String seriesProcessName = null;

    /** DOCUMENT ME! */
    private String seriesPSDName = null;

    /** DOCUMENT ME! */
    private String seriesRawDataSystemID = null;

    /** DOCUMENT ME! */
    private String seriesSystemGenerationID = null;

    /** DOCUMENT ME! */
    private short seriesTaskID = -32768;

    /** DOCUMENT ME! */
    private String seriesTime = null;

    /** DOCUMENT ME! */
    private String seriesType = null;

    /** DOCUMENT ME! */
    private float siCenter = Float.NaN;

    /** DOCUMENT ME! */
    private short sliceMultiplier = -32768;

    /** DOCUMENT ME! */
    private short sliceQuantity = -32768;

    /** DOCUMENT ME! */
    private String sliceThicknessDisclaimer = null;

    /** DOCUMENT ME! */
    private float sNormal = Float.NaN;

    /** DOCUMENT ME! */
    private float startX = Float.NaN;

    /** DOCUMENT ME! */
    private float startY = Float.NaN;

    /** DOCUMENT ME! */
    private float startZ = Float.NaN;

    /** DOCUMENT ME! */
    private String studyDate = null;

    /** DOCUMENT ME! */
    private String studyDescription = null;

    /** DOCUMENT ME! */
    private short studyHeaderBlocks = -32768;

    /** DOCUMENT ME! */
    private String studyHeaderID = null;

    /** DOCUMENT ME! */
    private String studyHeaderRevisionNumber = null;

    /** DOCUMENT ME! */
    private String studyNumber = null;

    /** DOCUMENT ME! */
    private short studyTaskID = -32768;

    /** DOCUMENT ME! */
    private String studyTime = null;

    /** DOCUMENT ME! */
    private String suppressionTechnique = null;

    /** DOCUMENT ME! */
    private String surfaceCoilIntensityCorrection = null;

    /** DOCUMENT ME! */
    private String surfaceCoilsCorrectionType = null;

    /** DOCUMENT ME! */
    private String surfaceCoilType = null;

    /** DOCUMENT ME! */
    private String swapPF = null;

    /** DOCUMENT ME! */
    private String systemConfigHospitalName = null;

    /** DOCUMENT ME! */
    private String systemGenerationID = null;

    /** Use serialVersionUID for interoperability. */
    // private static final long serialVersionUID = -4456298776612648834L;

    private String systemID = null;

    /** DOCUMENT ME! */
    private float tableLocation = Float.NaN;

    /** DOCUMENT ME! */
    private float tablePosition = Float.NaN;

    /** DOCUMENT ME! */
    private float te = Float.NaN;

    /** DOCUMENT ME! */
    private float TE2 = Float.NaN;

    /** DOCUMENT ME! */
    private float thickness = Float.NaN; // In practice 10 * res[0] = 10 * res[1]

    /** DOCUMENT ME! */
    private float ti = Float.NaN;

    /** DOCUMENT ME! */
    private float totalPostTriggerDelayTime = Float.NaN;

    /** DOCUMENT ME! */
    private float tr = Float.NaN;

    /** DOCUMENT ME! */
    private short transmitAttenuatorSetting = -32768;

    /** DOCUMENT ME! */
    private float ts = Float.NaN;

    /** DOCUMENT ME! */
    private float user0 = Float.NaN;

    /** DOCUMENT ME! */
    private float user1 = Float.NaN;

    /** DOCUMENT ME! */
    private float user2 = Float.NaN;

    /** DOCUMENT ME! */
    private float user3 = Float.NaN;

    /** DOCUMENT ME! */
    private float user4 = Float.NaN;

    /** DOCUMENT ME! */
    private float user5 = Float.NaN;

    /** DOCUMENT ME! */
    private float user6 = Float.NaN;

    /** DOCUMENT ME! */
    private float user7 = Float.NaN;

    /** DOCUMENT ME! */
    private float user8 = Float.NaN;

    /** DOCUMENT ME! */
    private float user9 = Float.NaN;

    /** DOCUMENT ME! */
    private String variableBandwidth = null;

    /** DOCUMENT ME! */
    private String vascularImagingFlags = null;

    /** DOCUMENT ME! */
    private String vasMode = null;

    /** DOCUMENT ME! */
    private float vencScalingFactor = Float.NaN;

    /** DOCUMENT ME! */
    private String verticalAnatomicalReference = null;

    /** DOCUMENT ME! */
    private float verticalLandmark = Float.NaN;

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
            units[0] = Unit.MILLIMETERS.getLegacyNum();
            units[1] = Unit.MILLIMETERS.getLegacyNum();
            units[2] = Unit.MILLIMETERS.getLegacyNum();
            units[3] = Unit.MILLIMETERS.getLegacyNum();
            units[4] = Unit.MILLIMETERS.getLegacyNum();
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
            units[0] = Unit.MILLIMETERS.getLegacyNum();
            units[1] = Unit.MILLIMETERS.getLegacyNum();
            units[2] = Unit.MILLIMETERS.getLegacyNum();
            units[3] = Unit.MILLIMETERS.getLegacyNum();
            units[4] = Unit.MILLIMETERS.getLegacyNum();
        }

        String name = JDialogBase.makeImageName(fileName, ".dcm");
        fileInfo = new FileInfoDicom(name, getFileDirectory(), FileUtility.DICOM);
        fileInfo.setExtents(extents);
        fileInfo.setResolutions(resols);
        fileInfo.setUnitsOfMeasure(units);
        fileInfo.setDataType(ModelImage.SHORT);
        fileInfo.setImageOrientation(getImageOrientation());

        //
        // set a bunch of variables from GE to DICOM ....

        // fileInfo.setValue("0002,0001", version, 2);
        // fileInfo.setValue("0002,0002", "1.2.840.10008.5.1.4.1.1.7 ", 26); // Secondary Capture SOP UID
        fileInfo.getTagTable().setValue("0002,0003", "1.2.840.999999999999999999", 26); // bogus SOP Instance UID
        fileInfo.getTagTable().setValue("0002,0010", "1.2.840.10008.1.2 ", 18); // Little Endian transfer syntax
        fileInfo.getTagTable().setValue("0002,0012", "1.2.840.34379.17", 16); // bogus Implementation UID made up by
                                                                              // Matt
        fileInfo.getTagTable().setValue("0002,0013", "MIPAV--NIH", 10); //

        fileInfo.setEndianess(FileBase.LITTLE_ENDIAN); // ??
        fileInfo.setRescaleIntercept(getRescaleIntercept()); // ??
        fileInfo.setRescaleSlope(getRescaleSlope()); // ??

        // Column and row
        fileInfo.getTagTable().setValue("0028,0011", new Short((short) getExtents()[0]), 2);
        fileInfo.getTagTable().setValue("0028,0010", new Short((short) getExtents()[1]), 2);

        fileInfo.getTagTable().setValue("0028,0100", new Short((short) 16), 2);
        fileInfo.getTagTable().setValue("0028,0101", new Short((short) 16), 2);
        fileInfo.getTagTable().setValue("0028,0102", new Short((short) 15), 2);
        fileInfo.getTagTable().setValue("0028,0002", new Short((short) 1), 2); // samples per pixel
        fileInfo.getTagTable().setValue("0028,0004", new String("MONOCHROME2 "), 12); // photometric
        fileInfo.getTagTable().setValue("0028,0103", new Short((short) 1), 2);

        // Instance number
        fileInfo.getTagTable().setValue("0020,0013", String.valueOf(i + 1).trim(),
                                        String.valueOf(i + 1).trim().length());

        // Pixel resolutions X, and Y.
        String s = String.valueOf(resols[1]) + "\\" + String.valueOf(resols[0]);
        String yearStr;
        String mmStr;
        String ddStr;
        String hhStr;
        String ssStr;
        fileInfo.getTagTable().setValue("0028,0030", s, s.length());

        // Slice thickness
        s = String.valueOf(getResolutions()[2]);
        fileInfo.getTagTable().setValue("0018,0050", s, s.length()); // slice thickness
        s = String.valueOf(getResolutions()[2]);
        fileInfo.getTagTable().setValue("0018,0088", s, s.length()); // spacing between slices


        fileInfo.getTagTable().setValue("0008,0060", "MR", 2);
        fileInfo.getTagTable().setValue("0008,0016", "1.2.840.10008.5.1.4.1.1.4", 25);
        fileInfo.getTagTable().setValue("0002,0002", "1.2.840.10008.5.1.4.1.1.4", 25);

        // fileInfo.setValue("0008,0016", "1.2.840.10008.5.1.4.1.1.7 ", 26); // Secondary Capture UID
        fileInfo.getTagTable().setValue("0008,0018", "1.2.840.999999999999999999", 26); // bogus SOP Instance UID all
                                                                                        // secondary capture info is
                                                                                        // installed by
                                                                                        // FileDicom.writeImage(), under
                                                                                        // the assumption that all saves
                                                                                        // must have been modified (and
                                                                                        // need that stuff)


        year = Integer.valueOf(studyDate.substring(7)).intValue();

        if (year < 50) {
            yearStr = "20".concat(studyDate.substring(7));
        } else {
            yearStr = "19".concat(studyDate.substring(7));
        }

        mmStr = studyDate.substring(3, 6);

        if (mmStr.equalsIgnoreCase("JAN")) {
            mmStr = "01";
        } else if (mmStr.equalsIgnoreCase("FEB")) {
            mmStr = "02";
        } else if (mmStr.equalsIgnoreCase("MAR")) {
            mmStr = "03";
        } else if (mmStr.equalsIgnoreCase("APR")) {
            mmStr = "04";
        } else if (mmStr.equalsIgnoreCase("MAY")) {
            mmStr = "05";
        } else if (mmStr.equalsIgnoreCase("JUN")) {
            mmStr = "06";
        } else if (mmStr.equalsIgnoreCase("JUL")) {
            mmStr = "07";
        } else if (mmStr.equalsIgnoreCase("AUG")) {
            mmStr = "08";
        } else if (mmStr.equalsIgnoreCase("SEP")) {
            mmStr = "09";
        } else if (mmStr.equalsIgnoreCase("OCT")) {
            mmStr = "10";
        } else if (mmStr.equalsIgnoreCase("NOV")) {
            mmStr = "11";
        } else {
            mmStr = "12";
        }

        ddStr = studyDate.substring(0, 2);

        s = yearStr + mmStr + ddStr;
        fileInfo.getTagTable().setValue("0008,0020", s, s.length()); // Study date

        hhStr = studyTime.substring(0, 2);
        mmStr = studyTime.substring(3, 5);
        ssStr = studyTime.substring(6);

        s = hhStr + mmStr + ssStr + ".0";
        fileInfo.getTagTable().setValue("0008,0030", s, s.length()); // Study time

        year = Integer.valueOf(seriesDate.substring(7)).intValue();

        if (year < 50) {
            yearStr = "20".concat(seriesDate.substring(7));
        } else {
            yearStr = "19".concat(seriesDate.substring(7));
        }

        mmStr = seriesDate.substring(3, 6);

        if (mmStr.equalsIgnoreCase("JAN")) {
            mmStr = "01";
        } else if (mmStr.equalsIgnoreCase("FEB")) {
            mmStr = "02";
        } else if (mmStr.equalsIgnoreCase("MAR")) {
            mmStr = "03";
        } else if (mmStr.equalsIgnoreCase("APR")) {
            mmStr = "04";
        } else if (mmStr.equalsIgnoreCase("MAY")) {
            mmStr = "05";
        } else if (mmStr.equalsIgnoreCase("JUN")) {
            mmStr = "06";
        } else if (mmStr.equalsIgnoreCase("JUL")) {
            mmStr = "07";
        } else if (mmStr.equalsIgnoreCase("AUG")) {
            mmStr = "08";
        } else if (mmStr.equalsIgnoreCase("SEP")) {
            mmStr = "09";
        } else if (mmStr.equalsIgnoreCase("OCT")) {
            mmStr = "10";
        } else if (mmStr.equalsIgnoreCase("NOV")) {
            mmStr = "11";
        } else {
            mmStr = "12";
        }

        ddStr = seriesDate.substring(0, 2);

        s = yearStr + mmStr + ddStr;
        fileInfo.getTagTable().setValue("0008,0021", s, s.length()); // Series date

        hhStr = seriesTime.substring(0, 2);
        mmStr = seriesTime.substring(3, 5);
        ssStr = seriesTime.substring(6);

        s = hhStr + mmStr + ssStr + ".0";
        fileInfo.getTagTable().setValue("0008,0031", s, s.length()); // Series time

        year = Integer.valueOf(imageCreationDate.substring(7)).intValue();

        if (year < 50) {
            yearStr = "20".concat(imageCreationDate.substring(7));
        } else {
            yearStr = "19".concat(imageCreationDate.substring(7));
        }

        mmStr = imageCreationDate.substring(3, 6);

        if (mmStr.equalsIgnoreCase("JAN")) {
            mmStr = "01";
        } else if (mmStr.equalsIgnoreCase("FEB")) {
            mmStr = "02";
        } else if (mmStr.equalsIgnoreCase("MAR")) {
            mmStr = "03";
        } else if (mmStr.equalsIgnoreCase("APR")) {
            mmStr = "04";
        } else if (mmStr.equalsIgnoreCase("MAY")) {
            mmStr = "05";
        } else if (mmStr.equalsIgnoreCase("JUN")) {
            mmStr = "06";
        } else if (mmStr.equalsIgnoreCase("JUL")) {
            mmStr = "07";
        } else if (mmStr.equalsIgnoreCase("AUG")) {
            mmStr = "08";
        } else if (mmStr.equalsIgnoreCase("SEP")) {
            mmStr = "09";
        } else if (mmStr.equalsIgnoreCase("OCT")) {
            mmStr = "10";
        } else if (mmStr.equalsIgnoreCase("NOV")) {
            mmStr = "11";
        } else {
            mmStr = "12";
        }

        ddStr = imageCreationDate.substring(0, 2);

        s = yearStr + mmStr + ddStr;
        fileInfo.getTagTable().setValue("0008,0023", s, s.length()); // Image date

        hhStr = imageCreationTime.substring(0, 2);
        mmStr = imageCreationTime.substring(3, 5);
        ssStr = imageCreationTime.substring(6);

        s = hhStr + mmStr + ssStr + ".0";
        fileInfo.getTagTable().setValue("0008,0033", s, s.length()); // Image time

        fileInfo.getTagTable().setValue("0008,0050", "123456", 6);
        fileInfo.getTagTable().setValue("0008,0080", hospitalName.trim(), hospitalName.trim().length()); // Institution name
        fileInfo.getTagTable().setValue("0008,1030", studyDescription.trim(), studyDescription.trim().length()); // Study description
        fileInfo.getTagTable().setValue("0008,103E", seriesDescription.trim(), seriesDescription.trim().length()); // Series description

        fileInfo.getTagTable().setValue("0010,0010", patientName.trim(), patientName.trim().length());
        fileInfo.getTagTable().setValue("0010,0020", patientID.trim(), patientID.trim().length());
        fileInfo.getTagTable().setValue("0010,1010", "0" + String.valueOf(patientAge),
                                        String.valueOf(patientAge).length() + 1);
        fileInfo.getTagTable().setValue("0010,21B0", history.trim(), history.trim().length());

        RandomNumberGen randomNum = new RandomNumberGen();
        randomNum.genUniformRandomNum(1, 100000);
        s = "1.2.840.34379.17.139875.234.455." + randomNum.genUniformRandomNum(1, 100000);
        fileInfo.getTagTable().setValue("0020,000D", s, s.length()); // study UID
        s = "1.2.840.34379.17.139875.234.456." + randomNum.genUniformRandomNum(1, 100000);
        fileInfo.getTagTable().setValue("0020,000E", s, s.length()); // series UID


        // study Number  (SH  short string)
        fileInfo.getTagTable().setValue("0020,0010", studyNumber, studyNumber.length());

        // series Number (IS integer string)
        fileInfo.getTagTable().setValue("0020,0011", seriesNumber, seriesNumber.length());

        s = -imgTLHC_R + "\\" + -imgTLHC_A + "\\" + imgTLHC_S;

        // s = imgTLHC_R + "\\" + imgTLHC_A + "\\" + imgTLHC_S;
        fileInfo.getTagTable().setValue("0020,0032", s, s.length()); // image position Right center .....

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
        fileInfo.getTagTable().setValue("0020,0037", s, s.length()); // image orientation

        s = String.valueOf(imageLocation);
        fileInfo.getTagTable().setValue("0020,1041", s, s.length()); // slice location

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

        if ((rawDataSystemID != null) && (rawDataSystemID.trim() != null) && (rawDataSystemID.trim().length() != 0)) {
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

        if ((requestedNumber != null) && (requestedNumber.trim() != null) && (requestedNumber.trim().length() != 0)) {
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

        if ((coilName != null) && (coilName.trim() != null) && (coilName.trim().length() != 0)) {
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

        if (pulseSequenceSubtype != null) {
            dialog.append("Pulse sequence subtype = " + pulseSequenceSubtype.trim() + "\n");
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
            dialog.append("Longitudinal anatomical reference = " + longitudinalAnatomicalReference.trim() + "\n");
        }

        if ((verticalAnatomicalReference != null) && (verticalAnatomicalReference.trim() != null) &&
                (verticalAnatomicalReference.trim().length() != 0)) {
            dialog.append("Vertical anatomical reference = " + verticalAnatomicalReference.trim() + "\n");
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

        if ((seriesPSDName != null) && (seriesPSDName.trim() != null) && (seriesPSDName.trim().length() != 0)) {
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
            dialog.append("R-side SAT pulse location relative to landmark = " + satXLoc1 + " millimeters\n");
        }

        if (satXLoc2 != -32768) {
            dialog.append("L-side SAT pulse location relative to landmark = " + satXLoc2 + " millimeters\n");
        }

        if (satYLoc1 != -32768) {
            dialog.append("A-side SAT pulse location relative to landmark = " + satYLoc1 + " millimeters\n");
        }

        if (satYLoc2 != -32768) {
            dialog.append("P-side SAT pulse location relative to landmark = " + satYLoc2 + " millimeters\n");
        }

        if (satZLoc1 != -32768) {
            dialog.append("S-side SAT pulse location relative to landmark = " + satZLoc1 + " millimeters\n");
        }

        if (satZLoc2 != -32768) {
            dialog.append("I-side SAT pulse location relative to landmark = " + satZLoc2 + " millimeters\n");
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

        if (gatingType2 != null) {
            dialog.append("Gating type - 2nd word = " + gatingType2 + "\n");
        }

        dialog.append("\nImage header\n");

        if (imageHeaderID != null) {
            dialog.append("Image header ID = " + imageHeaderID.trim() + "\n");
        }

        if (imageHeaderRevisionNumber != null) {
            dialog.append("Image header revision number = " + imageHeaderRevisionNumber.trim() + "\n");
        }

        if (imageHeaderBlocks >= 0) {
            dialog.append("Number of image header blocks = " + imageHeaderBlocks + "\n");
        }

        if (imageHeaderCreatorProcess != null) {
            dialog.append("Image header creator process = " + imageHeaderCreatorProcess.trim() + "\n");
        }

        if (imageHeaderCreatorTask != -32768) {
            dialog.append("Image header creator task = " + imageHeaderCreatorTask + "\n");
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
            dialog.append("Image series number = " + series.trim() + "\n");
        }

        if (imageRawDataSystemID != null) {
            dialog.append("Raw data system ID = " + imageRawDataSystemID.trim() + "\n");
        }

        if (imageSystemGenerationID != null) {
            dialog.append("System generation ID = " + imageSystemGenerationID.trim() + "\n");
        }

        if (!Float.isNaN(startX)) {
            dialog.append("Start location X, right minimum = " + startX + "\n");
        }

        if (!Float.isNaN(endX)) {
            dialog.append("End location X, right maximum = " + endX + "\n");
        }

        if (!Float.isNaN(startY)) {
            dialog.append("Start location Y, anterior minimum = " + startY + "\n");
        }

        if (!Float.isNaN(endY)) {
            dialog.append("End location Y, anterior maximum = " + endY + "\n");
        }

        if (!Float.isNaN(startZ)) {
            dialog.append("Start location Z, superior minimum = " + startZ + "\n");
        }

        if (!Float.isNaN(endZ)) {
            dialog.append("End location Z, superior maximum = " + endZ + "\n");
        }

        if (!Float.isNaN(imageLocation)) {
            dialog.append("Image location relative to landmark = " + imageLocation + "\n");
        }

        if (!Float.isNaN(tablePosition)) {
            dialog.append("Table position = " + tablePosition + "\n");
        }

        if (!Float.isNaN(thickness)) {
            dialog.append("Image thickness = " + thickness + "\n");
        }

        if (!Float.isNaN(imageSpacing)) {
            dialog.append("Spacing between slices = " + imageSpacing + "\n");
        }

        if (round != null) {
            dialog.append(round + "\n");
        }

        if (!Float.isNaN(tr)) {
            dialog.append("Repetition/recovery time tr = " + tr + " usec\n");
        }

        if (!Float.isNaN(ts)) {
            dialog.append("Scan time ts = " + ts + " usec\n");
        }

        if (!Float.isNaN(te)) {
            dialog.append("Echo delay time te = " + te + " usec\n");
        }

        if (!Float.isNaN(ti)) {
            dialog.append("Inversion recovery time ti = " + ti + " usec\n");
        }

        if (numberOfEchos != -32768) {
            dialog.append("Total number of echos = " + numberOfEchos + "\n");
        }

        if (echoNumber != -32768) {
            dialog.append("Echo number of current image = " + echoNumber + "\n");
        }

        if (sliceQuantity > 0) {
            dialog.append("Number of slices in this scan group = " + sliceQuantity + "\n");
        }

        if (averagesNumber != -32768) {
            dialog.append("Number of averages = " + averagesNumber + "\n");
        }

        if (researchMode != null) {
            dialog.append(researchMode + "\n");
        }

        if ((psdFileName != null) && (psdFileName.trim() != null) && (psdFileName.trim().length() != 0)) {
            dialog.append("PSD file name = " + psdFileName.trim() + "\n");
        }

        if (graphicallyPrescribed != null) {
            dialog.append(graphicallyPrescribed + "\n");
        }

        if ((prescribedSeriesNumbers != null) && (prescribedSeriesNumbers.trim() != null) &&
                (prescribedSeriesNumbers.trim().length() != 0)) {
            dialog.append("Series numbers from which image was prescribed = " + prescribedSeriesNumbers.trim() + "\n");
        }

        if ((prescribedImageNumbers != null) && (prescribedImageNumbers.trim() != null) &&
                (prescribedImageNumbers.trim().length() != 0)) {
            dialog.append("Image numbers from which image was prescribed = " + prescribedImageNumbers.trim() + "\n");
        }

        if (imageShape != null) {
            dialog.append("Image shape = " + imageShape + "\n");
        }

        dialog.append("X size in pixels = " + getExtents()[0] + "\n");

        dialog.append("Y size in pixels = " + getExtents()[1] + "\n");

        if (!Float.isNaN(pixelSize)) {
            dialog.append("Pixel size = " + pixelSize + " millimeters\n");
        }

        dialog.append("Image not compressed\n");

        dialog.append("Bits per pixel = " + bitsPerPixel + "\n");

        if (defaultWindow != -32768) {
            dialog.append("Default window = " + defaultWindow + "\n");
        }

        if (defaultLevel != -32768) {
            dialog.append("Default level = " + defaultLevel + "\n");
        }

        if (fileBlocks != -32768) {
            dialog.append("Number of blocks in file including headers = " + fileBlocks + "\n");
        }

        if (!Float.isNaN(excitationsNumber)) {
            dialog.append("Number of excitations = " + excitationsNumber + "\n");
        }

        if (!Float.isNaN(peakSAR)) {
            dialog.append("Value of peak SAR = " + peakSAR + " watts/kg\n");
        }

        if (!Float.isNaN(averageSAR)) {
            dialog.append("Value of average SAR = " + averageSAR + " watts/kg\n");
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
            dialog.append("Time between QRS signal trigger (peak) and the first excitation pulse = " +
                          totalPostTriggerDelayTime + " milliseconds\n");
        }

        if (arrythmiaRejectionRatio != -32768) {
            dialog.append("Percentage of average R-R interval in which excitation pulse trigger is recognized = " +
                          arrythmiaRejectionRatio + "\n");
        }

        if (cardiacRepTime != null) {
            dialog.append(cardiacRepTime + "\n");
        }

        if (imagesPerCardiacCycle != -32768) {
            dialog.append("Number of images to be acquired after initial trigger = " + imagesPerCardiacCycle + "\n");
        }

        if (scanARRs != Integer.MIN_VALUE) {
            dialog.append("Number of R-R intervals during the scan = " + scanARRs + "\n");
        }

        if (transmitAttenuatorSetting != -32768) {
            dialog.append("Transmit attenuator setting = " + transmitAttenuatorSetting + " 1/10 DB\n");
        }

        if (receiveAttenuatorSetting != -32768) {
            dialog.append("Receive attenuator setting = " + receiveAttenuatorSetting + " 1/10 DB\n");
        }

        if (imageFieldStrength != Integer.MIN_VALUE) {
            dialog.append("Magnetic field strength = " + imageFieldStrength + " 10 microgauss\n");
        }

        if (imageOffset != -32768) {
            dialog.append("Frequency/phase offset for image = " + imageOffset + "\n");
        }

        if (!Float.isNaN(interImageDelay)) {
            dialog.append("Time between excitation pulses within the R-R interval = " + interImageDelay +
                          " milliseconds\n");
        }

        if ((psdName != null) && (psdName.trim() != null) && (psdName.trim().length() != 0)) {
            dialog.append("PSD name = " + psdName.trim() + "\n");

            String monthStr = null;

            switch (psdMonth) {

                case 1:
                    monthStr = "January";
                    break;

                case 2:
                    monthStr = "February";
                    break;

                case 3:
                    monthStr = "March";
                    break;

                case 4:
                    monthStr = "April";
                    break;

                case 5:
                    monthStr = "May";
                    break;

                case 6:
                    monthStr = "June";
                    break;

                case 7:
                    monthStr = "July";
                    break;

                case 8:
                    monthStr = "August";
                    break;

                case 9:
                    monthStr = "September";
                    break;

                case 10:
                    monthStr = "October";
                    break;

                case 11:
                    monthStr = "November";
                    break;

                case 12:
                    monthStr = "December";
                    break;

                default:
                    monthStr = "Unknown month";
            }

            String dayStr = String.valueOf(psdDay);
            String yearStr = String.valueOf(psdYear + 1900);
            dialog.append("PSD file creation date = " + monthStr + " " + dayStr + ", " + yearStr + "\n");

            String hourStr = String.valueOf(psdHour);
            String minuteStr = String.valueOf(psdMinute);

            if (minuteStr.length() < 2) {
                minuteStr = "0".concat(minuteStr);
            }

            String secondsStr = String.valueOf(psdSeconds);

            if (secondsStr.length() < 2) {
                secondsStr = "0".concat(secondsStr);
            }

            dialog.append("PSD file creation time = " + hourStr + ":" + minuteStr + ":" + secondsStr + "\n");
        }

        if (flipAngle != -32768) {
            dialog.append("Flip angle for GRASS = " + flipAngle + " degrees\n");
        }

        if ((surfaceCoilsCorrectionType != null) && (surfaceCoilsCorrectionType.trim() != null) &&
                (surfaceCoilsCorrectionType.trim().length() != 0)) {
            dialog.append("Type of correction for surface coils = " + surfaceCoilsCorrectionType.trim() + "\n");
        }

        if ((scSer != null) && (scSer.trim() != null) && (scSer.trim().length() != 0)) {
            dialog.append("Series number of corrected/uncorrected image = " + scSer.trim() + "\n");
        }

        if ((scIma != null) && (scIma.trim() != null) && (scIma.trim().length() != 0)) {
            dialog.append("Image number of corrected/uncorrected image = " + scIma.trim() + "\n");
        }

        if (extremityCoil != null) {
            dialog.append(extremityCoil + "\n");
        }

        if ((pSeries2 != null) && (pSeries2.trim() != null) && (pSeries2.trim().length() != 0)) {
            dialog.append("Series number of second localizer = " + pSeries2.trim() + "\n");
        }

        if ((pImage2 != null) && (pImage2.trim() != null) && (pImage2.trim().length() != 0)) {
            dialog.append("Image number of corrected/uncorrected image = " + pImage2.trim() + "\n");
        }

        if (!Float.isNaN(rCenter)) {
            dialog.append("R center coordinate on plane image = " + rCenter + " millimeters\n");
        }

        if (!Float.isNaN(aCenter)) {
            dialog.append("A center coordinate on plane image = " + aCenter + " millimeters\n");
        }

        if (!Float.isNaN(sCenter)) {
            dialog.append("S center coordinate on plane image = " + sCenter + " millimeters\n");
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
            dialog.append("Right top left hand corner = " + imgTLHC_R + " millimeters\n");
        }

        if (!Float.isNaN(imgTLHC_A)) {
            dialog.append("Anterior top left hand corner = " + imgTLHC_A + " millimeters\n");
        }

        if (!Float.isNaN(imgTLHC_S)) {
            dialog.append("Superior top left hand corner = " + imgTLHC_S + " millimeters\n");
        }

        if (!Float.isNaN(imgTRHC_R)) {
            dialog.append("Right top right hand corner = " + imgTRHC_R + " millimeters\n");
        }

        if (!Float.isNaN(imgTRHC_A)) {
            dialog.append("Anterior top right hand corner = " + imgTRHC_A + " millimeters\n");
        }

        if (!Float.isNaN(imgTRHC_S)) {
            dialog.append("Superior top right hand corner = " + imgTRHC_S + " millimeters\n");
        }

        if (!Float.isNaN(imgBLHC_R)) {
            dialog.append("Right bottom left hand corner = " + imgBLHC_R + " millimeters\n");
        }

        if (!Float.isNaN(imgBLHC_A)) {
            dialog.append("Anterior bottom left hand corner = " + imgBLHC_A + " millimeters\n");
        }

        if (!Float.isNaN(imgBLHC_S)) {
            dialog.append("Superior bottom left hand corner = " + imgBLHC_S + " millimeters\n");
        }

        if (imageHeaderDisclaimer != -32768) {
            dialog.append("Image header disclaimer = " + imageHeaderDisclaimer + "\n");
        }

        if (minimumDelay != -32768) {
            dialog.append("Minimum delay after cardiac trigger = " + minimumDelay + " milliseconds\n");
        }

        if (cPhase != -32768) {
            dialog.append("Number of cardiac phases to reconstruct = " + cPhase + "\n");
        }

        if (!Float.isNaN(TE2)) {
            dialog.append("TE2 (VEMP) = " + TE2 + " milliseconds\n");
        }

        if (swapPF != null) {
            dialog.append(swapPF + "\n");
        }

        if (pauseInterval != -32768) {
            dialog.append("Pause interval = " + pauseInterval + " milliseconds\n");
        }

        if (!Float.isNaN(pauseTime)) {
            dialog.append("Pause time = " + pauseTime + " milliseconds\n");
        }

        if (!Float.isNaN(user0)) {
            dialog.append("User 0 = " + user0 + "\n");
        }

        if (!Float.isNaN(user1)) {
            dialog.append("User 1 = " + user1 + "\n");
        }

        if (!Float.isNaN(user2)) {
            dialog.append("User 2 = " + user2 + "\n");
        }

        if (!Float.isNaN(user3)) {
            dialog.append("User 3 = " + user3 + "\n");
        }

        if (!Float.isNaN(user4)) {
            dialog.append("User 4 = " + user4 + "\n");
        }

        if (!Float.isNaN(user5)) {
            dialog.append("User 5 = " + user5 + "\n");
        }

        if (!Float.isNaN(user6)) {
            dialog.append("User 6 = " + user6 + "\n");
        }

        if (!Float.isNaN(user7)) {
            dialog.append("User 7 = " + user7 + "\n");
        }

        if (!Float.isNaN(user8)) {
            dialog.append("User 8 = " + user8 + "\n");
        }

        if (!Float.isNaN(user9)) {
            dialog.append("User 9 = " + user9 + "\n");
        }

        if (obliquePlane != null) {
            dialog.append("Oblique plane = " + obliquePlane + "\n");
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

        if (fileFormat != null) {
            dialog.append("File format = " + fileFormat + "\n");
        }

        if (autoCenterFrequency != null) {
            dialog.append("Auto center frequency = " + autoCenterFrequency + "\n");
        }

        if (actualTransmitFrequency >= 0) {
            dialog.append("Actual transmit frequency used on scan = " + actualTransmitFrequency + "\n");
        }

        if (actualReceiveFrequency >= 0) {
            dialog.append("Actual receive frequency used on scan = " + actualReceiveFrequency + "\n");
        }

        if (recommendedTransmitFrequency >= 0) {
            dialog.append("Recommended automated transmit frequency = " + recommendedTransmitFrequency + "\n");
        }

        if (recommendedReceiveFrequency >= 0) {
            dialog.append("Recommended automated receive frequency = " + recommendedReceiveFrequency + "\n");
        }

        if (recommendedTransmitAttenuation >= 0) {
            dialog.append("Recommended automated transmit attenuation = " + recommendedTransmitAttenuation +
                          " 1/10 db\n");
        }

        if (recommendedReceiveAttenuation >= 0) {
            dialog.append("Recommended automated receive attenuation = " + recommendedReceiveAttenuation +
                          " 1/10 db\n");
        }

        if (histogramPresent != null) {
            dialog.append(histogramPresent + "\n");
        }

        if (pfSwapped != null) {
            dialog.append(pfSwapped + "\n");
        }

        if (R1 != -32768) {
            dialog.append("R1 = " + R1 + "\n");
        }

        if (R2 != -32768) {
            dialog.append("R2 = " + R2 + "\n");
        }

        if (variableBandwidth != null) {
            dialog.append("Variable bandwidth = " + variableBandwidth + "\n");
        }

        if (prescanReceiveAttenuation1 > 0) {
            dialog.append("Prescan Receive Attenuation 1 = " + prescanReceiveAttenuation1 + "\n");
        }

        if (prescanReceiveAttenuation2 > 0) {
            dialog.append("Prescan Receive Attenuation 2 = " + prescanReceiveAttenuation2 + "\n");
        }

        if (autoManualPrescan != null) {
            dialog.append(autoManualPrescan + "\n");
        }

        if (changedValues != null) {
            dialog.append("Changed values = " + changedValues + "\n");
        }

        if (imageType != null) {
            dialog.append("Type = " + imageType + "\n");
        }

        if (collapseImage != null) {
            dialog.append("Collapse Image = " + collapseImage + "\n");
        }

        if (sliceThicknessDisclaimer != null) {
            dialog.append("Slice thickness disclaimer = " + sliceThicknessDisclaimer + "\n");
        }

        if (PCVelocityEncoding != -32768) {
            dialog.append("PC velocity encoding = " + PCVelocityEncoding + " mm/sec\n");
        }

        if (!Float.isNaN(projectionAngle)) {
            dialog.append("Tardis projection angle = " + projectionAngle + " degrees\n");
        }

        if (concatenatedSATSelection != null) {
            dialog.append("Concatenated SAT selection = " + concatenatedSATSelection + "\n");
        }

        if (fractionalEffectiveEcho != null) {
            dialog.append(fractionalEffectiveEcho + "\n");
        }

        if (echoTrainLength >= 0) {
            dialog.append("Echo train length = " + echoTrainLength + "\n");
        }

        if (sliceMultiplier != -32768) {
            dialog.append("Slice multiplier to obtain phases for FAST = " + sliceMultiplier + "\n");
        }

        if (cardiacPhaseNumber != -32768) {
            dialog.append("Cardiac phase number = " + cardiacPhaseNumber + "\n");
        }

        if (scanAcquisitionNumber >= 0) {
            dialog.append("Number of acquisitions in scan = " + scanAcquisitionNumber + "\n");
        }

        if (vascularImagingFlags != null) {
            dialog.append("Vascular imaging flags = " + vascularImagingFlags + "\n");
        }

        if (!Float.isNaN(vencScalingFactor)) {
            dialog.append("Venc scaling factor = " + vencScalingFactor + "\n");
        }

        dialog.setSize(600, 500);
    }

    /**
     * Returns the image name which should be used for the image this file info is attached to (studyNum_seriesNum).
     *
     * @return  The name to give to this file info's image.
     */
    public String getImageNameFromInfo() {
        return studyNumber.trim() + "_" + seriesNumber.trim();
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
     * DOCUMENT ME!
     *
     * @param  aCenter  DOCUMENT ME!
     */
    public void setACenter(float aCenter) {
        this.aCenter = aCenter;
    }
    
    public float getACenter() {
    	return aCenter;
    }

    /**
     * DOCUMENT ME!
     *
     * @param  actualReceiveFrequency  DOCUMENT ME!
     */
    public void setActualReceiveFrequency(int actualReceiveFrequency) {
        this.actualReceiveFrequency = actualReceiveFrequency;
    }
    
    public int getActualReceiveFrequency() {
    	return actualReceiveFrequency;
    }

    /**
     * DOCUMENT ME!
     *
     * @param  actualTransmitFrequency  DOCUMENT ME!
     */
    public void setActualTransmitFrequency(int actualTransmitFrequency) {
        this.actualTransmitFrequency = actualTransmitFrequency;
    }
    
    public int getActualTransmitFrequency() {
    	return actualTransmitFrequency;
    }

    /**
     * DOCUMENT ME!
     *
     * @param  aNormal  DOCUMENT ME!
     */
    public void setANormal(float aNormal) {
        this.aNormal = aNormal;
    }
    
    public float getANormal() {
    	return aNormal;
    }

    /**
     * DOCUMENT ME!
     *
     * @param  apCenter  DOCUMENT ME!
     */
    public void setAPCenter(float apCenter) {
        this.apCenter = apCenter;
    }
    
    public float getAPCenter() {
    	return apCenter;
    }

    /**
     * DOCUMENT ME!
     *
     * @param  arrythmiaRejectionRatio  DOCUMENT ME!
     */
    public void setArrythmiaRejectionRatio(short arrythmiaRejectionRatio) {
        this.arrythmiaRejectionRatio = arrythmiaRejectionRatio;
    }
    
    public short getArrythmiaRejectionRatio() {
    	return arrythmiaRejectionRatio;
    }

    /**
     * DOCUMENT ME!
     *
     * @param  autoCenterFrequency  DOCUMENT ME!
     */
    public void setAutoCenterFrequency(String autoCenterFrequency) {
        this.autoCenterFrequency = autoCenterFrequency;
    }
    
    public String getAutoCenterFrequency() {
    	return autoCenterFrequency;
    }

    /**
     * DOCUMENT ME!
     *
     * @param  autoManualPrescan  DOCUMENT ME!
     */
    public void setAutoManualPrescan(String autoManualPrescan) {
        this.autoManualPrescan = autoManualPrescan;
    }
    
    public String getAutoManualPrescan() {
    	return autoManualPrescan;
    }

    /**
     * DOCUMENT ME!
     *
     * @param  averageSAR  DOCUMENT ME!
     */
    public void setAverageSAR(float averageSAR) {
        this.averageSAR = averageSAR;
    }
    
    public float getAverageSAR() {
    	return averageSAR;
    }

    /**
     * DOCUMENT ME!
     *
     * @param  averagesNumber  DOCUMENT ME!
     */
    public void setAveragesNumber(short averagesNumber) {
        this.averagesNumber = averagesNumber;
    }
    
    public short getAveragesNumber() {
    	return averagesNumber;
    }

    /**
     * DOCUMENT ME!
     *
     * @param  bitsPerPixel  DOCUMENT ME!
     */
    public void setBitsPerPixel(short bitsPerPixel) {
        this.bitsPerPixel = bitsPerPixel;
    }
    
    public short getBitsPerPixel() {
    	return bitsPerPixel;
    }

    /**
     * DOCUMENT ME!
     *
     * @param  cardiacHeartRate  DOCUMENT ME!
     */
    public void setCardiacHeartRate(short cardiacHeartRate) {
        this.cardiacHeartRate = cardiacHeartRate;
    }
    
    public short getCardiacHeartRate(){
    	return cardiacHeartRate;
    }
    /**
     * DOCUMENT ME!
     *
     * @param  cardiacPhaseNumber  DOCUMENT ME!
     */
    public void setCardiacPhaseNumber(short cardiacPhaseNumber) {
        this.cardiacPhaseNumber = cardiacPhaseNumber;
    }
    
    public short getCardiacPhaseNumber() {
    	return cardiacPhaseNumber;
    }

    /**
     * DOCUMENT ME!
     *
     * @param  cardiacRepTime  DOCUMENT ME!
     */
    public void setCardiacRepTime(String cardiacRepTime) {
        this.cardiacRepTime = cardiacRepTime;
    }
    
    public String getCardiacRepTime() {
        return cardiacRepTime;
    }

    /**
     * DOCUMENT ME!
     *
     * @param  changedValues  DOCUMENT ME!
     */
    public void setChangedValues(String changedValues) {
        this.changedValues = changedValues;
    }
    
    public String getChangedValues() {
        return changedValues;
    }

    /**
     * DOCUMENT ME!
     *
     * @param  coilName  DOCUMENT ME!
     */
    public void setCoilName(String coilName) {
        this.coilName = coilName;
    }
    
    public String getCoilName() {
        return coilName;
    }

    /**
     * DOCUMENT ME!
     *
     * @param  coilType  DOCUMENT ME!
     */
    public void setCoilType(String coilType) {
        this.coilType = coilType;
    }
    
    public String getCoilType() {
        return coilType;
    }

    /**
     * DOCUMENT ME!
     *
     * @param  collapseImage  DOCUMENT ME!
     */
    public void setCollapseImage(String collapseImage) {
        this.collapseImage = collapseImage;
    }
    
    public String getCollapseImage() {
        return collapseImage;
    }

    /**
     * DOCUMENT ME!
     *
     * @param  concatenatedSATSelection  DOCUMENT ME!
     */
    public void setConcatenatedSATSelection(String concatenatedSATSelection) {
        this.concatenatedSATSelection = concatenatedSATSelection;
    }
    
    public String getConcatenatedSATSelection() {
        return concatenatedSATSelection;
    }

    /**
     * DOCUMENT ME!
     *
     * @param  contiguousSlices  DOCUMENT ME!
     */
    public void setContiguousSlices(String contiguousSlices) {
        this.contiguousSlices = contiguousSlices;
    }
    
    public String getContiguousSlices() {
        return contiguousSlices;
    }

    /**
     * DOCUMENT ME!
     *
     * @param  contrastAgent  DOCUMENT ME!
     */
    public void setContrastAgent(String contrastAgent) {
        this.contrastAgent = contrastAgent;
    }
    
    public String getContrastAgent() {
        return contrastAgent;
    }

    /**
     * DOCUMENT ME!
     *
     * @param  contrastAmount  DOCUMENT ME!
     */
    public void setContrastAmount(float contrastAmount) {
        this.contrastAmount = contrastAmount;
    }
    
    public float getContrastAmount() {
        return contrastAmount;
    }

    /**
     * DOCUMENT ME!
     *
     * @param  contrastDescription  DOCUMENT ME!
     */
    public void setContrastDescription(String contrastDescription) {
        this.contrastDescription = contrastDescription;
    }
    
    public String getContrastDescription() {
        return contrastDescription;
    }

    /**
     * DOCUMENT ME!
     *
     * @param  contrastUsed  DOCUMENT ME!
     */
    public void setContrastUsed(String contrastUsed) {
        this.contrastUsed = contrastUsed;
    }
    
    public String getContrastUsed() {
        return contrastUsed;
    }

    /**
     * DOCUMENT ME!
     *
     * @param  cPhase  DOCUMENT ME!
     */
    public void setCPhase(short cPhase) {
        this.cPhase = cPhase;
    }
    
    public short getCPhase() {
        return cPhase;
    }

    /**
     * DOCUMENT ME!
     *
     * @param  defaultLevel  DOCUMENT ME!
     */
    public void setDefaultLevel(short defaultLevel) {
        this.defaultLevel = defaultLevel;
    }
    
    public short getDefaultLevel() {
        return defaultLevel;
    }

    /**
     * DOCUMENT ME!
     *
     * @param  defaultWindow  DOCUMENT ME!
     */
    public void setDefaultWindow(short defaultWindow) {
        this.defaultWindow = defaultWindow;
    }
    
    public short getDefaultWindow() {
        return defaultWindow;
    }

    /**
     * DOCUMENT ME!
     *
     * @param  diognostician  DOCUMENT ME!
     */
    public void setDiognostician(String diognostician) {
        this.diognostician = diognostician;
    }
    
    public String getDiognostician() {
        return diognostician;
    }

    /**
     * DOCUMENT ME!
     *
     * @param  echoNumber  DOCUMENT ME!
     */
    public void setEchoNumber(short echoNumber) {
        this.echoNumber = echoNumber;
    }
    
    public short getEchoNumber() {
        return echoNumber;
    }

    /**
     * DOCUMENT ME!
     *
     * @param  echoTrainLength  DOCUMENT ME!
     */
    public void setEchoTrainLength(int echoTrainLength) {
        this.echoTrainLength = echoTrainLength;
    }
    
    public int getEchoTrainLength() {
        return echoTrainLength;
    }

    /**
     * DOCUMENT ME!
     *
     * @param  endX  DOCUMENT ME!
     */
    public void setEndX(float endX) {
        this.endX = endX;
    }
    
    public float getEndX() {
        return endX;
    }

    /**
     * DOCUMENT ME!
     *
     * @param  endY  DOCUMENT ME!
     */
    public void setEndY(float endY) {
        this.endY = endY;
    }
    
    public float getEndY() {
        return endY;
    }

    /**
     * DOCUMENT ME!
     *
     * @param  endZ  DOCUMENT ME!
     */
    public void setEndZ(float endZ) {
        this.endZ = endZ;
    }
    
    public float getEndZ() {
        return endZ;
    }

    /**
     * DOCUMENT ME!
     *
     * @param  excitationsNumber  DOCUMENT ME!
     */
    public void setExcitationsNumber(float excitationsNumber) {
        this.excitationsNumber = excitationsNumber;
    }
    
    public float getExcitationsNumber() {
        return excitationsNumber;
    }

    /**
     * DOCUMENT ME!
     *
     * @param  extremityCoil  DOCUMENT ME!
     */
    public void setExtremityCoil(String extremityCoil) {
        this.extremityCoil = extremityCoil;
    }
    
    public String getExtremityCoil() {
        return extremityCoil;
    }

    /**
     * DOCUMENT ME!
     *
     * @param  fieldOfView  DOCUMENT ME!
     */
    public void setFieldOfView(float fieldOfView) {
        this.fieldOfView = fieldOfView;
    }
    
    public float getFieldOfView() {
        return fieldOfView;
    }

    /**
     * DOCUMENT ME!
     *
     * @param  fieldStrength  DOCUMENT ME!
     */
    public void setFieldStrength(short fieldStrength) {
        this.fieldStrength = fieldStrength;
    }
    
    public short getFieldStrength() {
        return fieldStrength;
    }

    /**
     * DOCUMENT ME!
     *
     * @param  fileBlocks  DOCUMENT ME!
     */
    public void setFileBlocks(short fileBlocks) {
        this.fileBlocks = fileBlocks;
    }
    
    public short getFileBlocks() {
        return fileBlocks;
    }

    /**
     * DOCUMENT ME!
     *
     * @param  fileFormat  DOCUMENT ME!
     */
    public void setFileFormat(String fileFormat) {
        this.fileFormat = fileFormat;
    }
    
    public String getSignaFileFormat() {
        return fileFormat;
    }

    /**
     * DOCUMENT ME!
     *
     * @param  flipAngle  DOCUMENT ME!
     */
    public void setFlipAngle(short flipAngle) {
        this.flipAngle = flipAngle;
    }
    
    public short getFlipAngle() {
        return flipAngle;
    }

    /**
     * DOCUMENT ME!
     *
     * @param  fractionalEffectiveEcho  DOCUMENT ME!
     */
    public void setFractionalEffectiveEcho(String fractionalEffectiveEcho) {
        this.fractionalEffectiveEcho = fractionalEffectiveEcho;
    }
    
    public String getFractionalEffectiveEcho() {
        return fractionalEffectiveEcho;
    }

    /**
     * DOCUMENT ME!
     *
     * @param  gatingType  DOCUMENT ME!
     */
    public void setGatingType(String gatingType) {
        this.gatingType = gatingType;
    }
    
    public String getGatingType() {
        return gatingType;
    }

    /**
     * DOCUMENT ME!
     *
     * @param  gatingType2  DOCUMENT ME!
     */
    public void setGatingType2(String gatingType2) {
        this.gatingType2 = gatingType2;
    }
    
    public String getGatingType2() {
        return gatingType2;
    }

    /**
     * DOCUMENT ME!
     *
     * @param  graphicallyPrescribed  DOCUMENT ME!
     */
    public void setGraphicallyPrescribed(String graphicallyPrescribed) {
        this.graphicallyPrescribed = graphicallyPrescribed;
    }
    
    public String getGraphicallyPrescribed() {
        return graphicallyPrescribed;
    }

    /**
     * DOCUMENT ME!
     *
     * @param  histogramPresent  DOCUMENT ME!
     */
    public void setHistogramPresent(String histogramPresent) {
        this.histogramPresent = histogramPresent;
    }
    
    public String getHistogramPresent() {
        return histogramPresent;
    }

    /**
     * DOCUMENT ME!
     *
     * @param  history  DOCUMENT ME!
     */
    public void setHistory(String history) {
        this.history = history;
    }
    
    public String getHistory() {
        return history;
    }

    /**
     * DOCUMENT ME!
     *
     * @param  horizontalLandmark  DOCUMENT ME!
     */
    public void setHorizontalLandmark(float horizontalLandmark) {
        this.horizontalLandmark = horizontalLandmark;
    }
    
    public float getHorizontalLandmark() {
        return horizontalLandmark;
    }

    /**
     * DOCUMENT ME!
     *
     * @param  hospitalName  DOCUMENT ME!
     */
    public void setHospitalName(String hospitalName) {
        this.hospitalName = hospitalName;
    }
    
    public String getHospitalName() {
        return hospitalName;
    }

    /**
     * DOCUMENT ME!
     *
     * @param  imageCreationDate  DOCUMENT ME!
     */
    public void setImageCreationDate(String imageCreationDate) {
        this.imageCreationDate = imageCreationDate;
    }
    
    public String getImageCreationDate() {
        return imageCreationDate;
    }

    /**
     * DOCUMENT ME!
     *
     * @param  imageCreationTime  DOCUMENT ME!
     */
    public void setImageCreationTime(String imageCreationTime) {
        this.imageCreationTime = imageCreationTime;
    }
    
    public String getImageCreationTime() {
        return imageCreationTime;
    }

    /**
     * DOCUMENT ME!
     *
     * @param  imageFieldStrength  DOCUMENT ME!
     */
    public void setImageFieldStrength(int imageFieldStrength) {
        this.imageFieldStrength = imageFieldStrength;
    }
    
    public int getImageFieldStrength() {
        return imageFieldStrength;
    }

    /**
     * DOCUMENT ME!
     *
     * @param  imageHeaderBlocks  DOCUMENT ME!
     */
    public void setImageHeaderBlocks(short imageHeaderBlocks) {
        this.imageHeaderBlocks = imageHeaderBlocks;
    }
    
    public short getImageHeaderBlocks() {
        return imageHeaderBlocks;
    }

    /**
     * DOCUMENT ME!
     *
     * @param  imageHeaderCreatorProcess  DOCUMENT ME!
     */
    public void setImageHeaderCreatorProcess(String imageHeaderCreatorProcess) {
        this.imageHeaderCreatorProcess = imageHeaderCreatorProcess;
    }
    
    public String getImageHeaderCreatorProcess() {
        return imageHeaderCreatorProcess;
    }

    /**
     * DOCUMENT ME!
     *
     * @param  imageHeaderCreatorTask  DOCUMENT ME!
     */
    public void setImageHeaderCreatorTask(short imageHeaderCreatorTask) {
        this.imageHeaderCreatorTask = imageHeaderCreatorTask;
    }
    
    public short getImageHeaderCreatorTask() {
        return imageHeaderCreatorTask;
    }

    /**
     * DOCUMENT ME!
     *
     * @param  imageHeaderDisclaimer  DOCUMENT ME!
     */
    public void setImageHeaderDisclaimer(short imageHeaderDisclaimer) {
        this.imageHeaderDisclaimer = imageHeaderDisclaimer;
    }
    
    public short getImageHeaderDisclaimer() {
        return imageHeaderDisclaimer;
    }

    /**
     * DOCUMENT ME!
     *
     * @param  imageHeaderID  DOCUMENT ME!
     */
    public void setImageHeaderID(String imageHeaderID) {
        this.imageHeaderID = imageHeaderID;
    }
    
    public String getImageHeaderID() {
        return imageHeaderID;
    }

    /**
     * DOCUMENT ME!
     *
     * @param  imageHeaderRevisionNumber  DOCUMENT ME!
     */
    public void setImageHeaderRevisionNumber(String imageHeaderRevisionNumber) {
        this.imageHeaderRevisionNumber = imageHeaderRevisionNumber;
    }
    
    public String getImageHeaderRevisionNumber() {
        return imageHeaderRevisionNumber;
    }

    /**
     * DOCUMENT ME!
     *
     * @param  imageLocation  DOCUMENT ME!
     */
    public void setImageLocation(float imageLocation) {
        this.imageLocation = imageLocation;
    }
    
    public float getImageLocation() {
        return imageLocation;
    }

    /**
     * DOCUMENT ME!
     *
     * @param  imageMatrix  DOCUMENT ME!
     */
    public void setImageMatrix(short imageMatrix) {
        this.imageMatrix = imageMatrix;
    }
    
    public short getImageMatrix() {
        return imageMatrix;
    }

    /**
     * DOCUMENT ME!
     *
     * @param  imageMode  DOCUMENT ME!
     */
    public void setImageMode(String imageMode) {
        this.imageMode = imageMode;
    }
    
    public String getImageMode() {
        return imageMode;
    }

    /**
     * DOCUMENT ME!
     *
     * @param  imageNumber  DOCUMENT ME!
     */
    public void setImageNumber(String imageNumber) {
        this.imageNumber = imageNumber;
    }
    
    public String getImageNumber() {
        return imageNumber;
    }

    /**
     * DOCUMENT ME!
     *
     * @param  imageOffset  DOCUMENT ME!
     */
    public void setImageOffset(short imageOffset) {
        this.imageOffset = imageOffset;
    }
    
    public short getImageOffset() {
        return imageOffset;
    }

    /**
     * DOCUMENT ME!
     *
     * @param  imageRawDataSystemID  DOCUMENT ME!
     */
    public void setImageRawDataSystemID(String imageRawDataSystemID) {
        this.imageRawDataSystemID = imageRawDataSystemID;
    }
    
    public String getImageRawDataSystemID() {
        return imageRawDataSystemID;
    }

    /**
     * DOCUMENT ME!
     *
     * @param  imagesAllocated  DOCUMENT ME!
     */
    public void setImagesAllocated(short imagesAllocated) {
        this.imagesAllocated = imagesAllocated;
    }
    
    public short getImagesAllocated() {
        return imagesAllocated;
    }

    /**
     * DOCUMENT ME!
     *
     * @param  imageShape  DOCUMENT ME!
     */
    public void setImageShape(String imageShape) {
        this.imageShape = imageShape;
    }
    
    public String getImageShape() {
        return imageShape;
    }

    /**
     * DOCUMENT ME!
     *
     * @param  imageSpacing  DOCUMENT ME!
     */
    public void setImageSpacing(float imageSpacing) {
        this.imageSpacing = imageSpacing;
    }
    
    public float getImageSpacing() {
        return imageSpacing;
    }

    /**
     * DOCUMENT ME!
     *
     * @param  imagesPerCardiacCycle  DOCUMENT ME!
     */
    public void setImagesPerCardiacCycle(short imagesPerCardiacCycle) {
        this.imagesPerCardiacCycle = imagesPerCardiacCycle;
    }
    
    public short getImagesPerCardiacCycle() {
        return imagesPerCardiacCycle;
    }

    /**
     * DOCUMENT ME!
     *
     * @param  imageSystemGenerationID  DOCUMENT ME!
     */
    public void setImageSystemGenerationID(String imageSystemGenerationID) {
        this.imageSystemGenerationID = imageSystemGenerationID;
    }
    
    public String getImageSystemGenerationID() {
        return imageSystemGenerationID;
    }

    /**
     * DOCUMENT ME!
     *
     * @param  imageType  DOCUMENT ME!
     */
    public void setImageType(String imageType) {
        this.imageType = imageType;
    }
    
    public String getImageType() {
        return imageType;
    }

    /**
     * DOCUMENT ME!
     *
     * @param  imgBLHC_A  DOCUMENT ME!
     */
    public void setImgBLHC_A(float imgBLHC_A) {
        this.imgBLHC_A = imgBLHC_A;
    }
    
    public float getImgBLHC_A() {
        return imgBLHC_A;
    }

    /**
     * DOCUMENT ME!
     *
     * @param  imgBLHC_R  DOCUMENT ME!
     */
    public void setImgBLHC_R(float imgBLHC_R) {
        this.imgBLHC_R = imgBLHC_R;
    }
    
    public float getImgBLHC_R() {
        return imgBLHC_R;
    }

    /**
     * DOCUMENT ME!
     *
     * @param  imgBLHC_S  DOCUMENT ME!
     */
    public void setImgBLHC_S(float imgBLHC_S) {
        this.imgBLHC_S = imgBLHC_S;
    }
    
    public float getImgBLHC_S() {
        return imgBLHC_S;
    }

    /**
     * DOCUMENT ME!
     *
     * @param  imgTLHC_A  DOCUMENT ME!
     */
    public void setImgTLHC_A(float imgTLHC_A) {
        this.imgTLHC_A = imgTLHC_A;
    }
    
    public float getImgTLHC_A() {
        return imgTLHC_A;
    }

    /**
     * DOCUMENT ME!
     *
     * @param  imgTLHC_R  DOCUMENT ME!
     */
    public void setImgTLHC_R(float imgTLHC_R) {
        this.imgTLHC_R = imgTLHC_R;
    }
    
    public float getImgTLHC_R() {
        return imgTLHC_R;
    }

    /**
     * DOCUMENT ME!
     *
     * @param  imgTLHC_S  DOCUMENT ME!
     */
    public void setImgTLHC_S(float imgTLHC_S) {
        this.imgTLHC_S = imgTLHC_S;
    }
    
    public float getImgTLHC_S() {
        return imgTLHC_S;
    }

    /**
     * DOCUMENT ME!
     *
     * @param  imgTRHC_A  DOCUMENT ME!
     */
    public void setImgTRHC_A(float imgTRHC_A) {
        this.imgTRHC_A = imgTRHC_A;
    }
    
    public float getImgTRHC_A() {
        return imgTRHC_A;
    }

    /**
     * DOCUMENT ME!
     *
     * @param  imgTRHC_R  DOCUMENT ME!
     */
    public void setImgTRHC_R(float imgTRHC_R) {
        this.imgTRHC_R = imgTRHC_R;
    }
    
    public float getImgTRHC_R() {
        return imgTRHC_R;
    }

    /**
     * DOCUMENT ME!
     *
     * @param  imgTRHC_S  DOCUMENT ME!
     */
    public void setImgTRHC_S(float imgTRHC_S) {
        this.imgTRHC_S = imgTRHC_S;
    }
    
    public float getImgTRHC_S() {
        return imgTRHC_S;
    }

    /**
     * DOCUMENT ME!
     *
     * @param  interImageDelay  DOCUMENT ME!
     */
    public void setInterImageDelay(float interImageDelay) {
        this.interImageDelay = interImageDelay;
    }
    
    public float getInterImageDelay() {
        return interImageDelay;
    }

    /**
     * DOCUMENT ME!
     *
     * @param  landmarkCounter  DOCUMENT ME!
     */
    public void setLandmarkCounter(int landmarkCounter) {
        this.landmarkCounter = landmarkCounter;
    }
    
    public int getLandmarkCounter() {
        return landmarkCounter;
    }

    /**
     * DOCUMENT ME!
     *
     * @param  longitudinalAnatomicalReference  DOCUMENT ME!
     */
    public void setLongitudinalAnatomicalReference(String longitudinalAnatomicalReference) {
        this.longitudinalAnatomicalReference = longitudinalAnatomicalReference;
    }
    
    public String getLongitudinalAnatomicalReference() {
        return longitudinalAnatomicalReference;
    }

    /**
     * DOCUMENT ME!
     *
     * @param  minimumDelay  DOCUMENT ME!
     */
    public void setMinimumDelay(short minimumDelay) {
        this.minimumDelay = minimumDelay;
    }
    
    public short getMinimumDelay() {
        return minimumDelay;
    }

    /**
     * DOCUMENT ME!
     *
     * @param  MRIProcessName  DOCUMENT ME!
     */
    public void setMRIProcessName(String MRIProcessName) {
        this.MRIProcessName = MRIProcessName;
    }
    
    public String getMRIProcessName() {
        return MRIProcessName;
    }

    /**
     * DOCUMENT ME!
     *
     * @param  numberOfEchos  DOCUMENT ME!
     */
    public void setNumberOfEchos(short numberOfEchos) {
        this.numberOfEchos = numberOfEchos;
    }
    
    public short getNumberOfEchos() {
        return numberOfEchos;
    }

    /**
     * DOCUMENT ME!
     *
     * @param  obliquePlane  DOCUMENT ME!
     */
    public void setObliquePlane(String obliquePlane) {
        this.obliquePlane = obliquePlane;
    }
    
    public String getObliquePlane() {
        return obliquePlane;
    }

    /**
     * DOCUMENT ME!
     *
     * @param  operator  DOCUMENT ME!
     */
    public void setOperator(String operator) {
        this.operator = operator;
    }
    
    public String getOperator() {
        return operator;
    }

    /**
     * DOCUMENT ME!
     *
     * @param  orientation  DOCUMENT ME!
     */
    public void setOrientation(String orientation) {
        this.orientation = orientation;
    }
    
    public String getOrientation() {
        return orientation;
    }

    /**
     * DOCUMENT ME!
     *
     * @param  originalSeriesNumber  DOCUMENT ME!
     */
    public void setOriginalSeriesNumber(String originalSeriesNumber) {
        this.originalSeriesNumber = originalSeriesNumber;
    }
    
    public String getOriginalSeriesNumber() {
        return originalSeriesNumber;
    }

    /**
     * DOCUMENT ME!
     *
     * @param  patientAge  DOCUMENT ME!
     */
    public void setPatientAge(String patientAge) {
        this.patientAge = patientAge;
    }
    
    public String getPatientAge() {
        return patientAge;
    }

    /**
     * DOCUMENT ME!
     *
     * @param  patientID  DOCUMENT ME!
     */
    public void setPatientID(String patientID) {
        this.patientID = patientID;
    }
    
    public String getPatientID() {
        return patientID;
    }

    /**
     * DOCUMENT ME!
     *
     * @param  patientName  DOCUMENT ME!
     */
    public void setPatientName(String patientName) {
        this.patientName = patientName;
    }
    
    public String getPatientName() {
        return patientName;
    }

    /**
     * DOCUMENT ME!
     *
     * @param  patientSex  DOCUMENT ME!
     */
    public void setPatientSex(String patientSex) {
        this.patientSex = patientSex;
    }
    
    public String getPatientSex() {
        return patientSex;
    }

    /**
     * DOCUMENT ME!
     *
     * @param  patientStatus  DOCUMENT ME!
     */
    public void setPatientStatus(String patientStatus) {
        this.patientStatus = patientStatus;
    }
    
    public String getPatientStatus() {
        return patientStatus;
    }

    /**
     * DOCUMENT ME!
     *
     * @param  patientWeight  DOCUMENT ME!
     */
    public void setPatientWeight(int patientWeight) {
        this.patientWeight = patientWeight;
    }
    
    public int getPatientWeight() {
        return patientWeight;
    }

    /**
     * DOCUMENT ME!
     *
     * @param  pauseInterval  DOCUMENT ME!
     */
    public void setPauseInterval(short pauseInterval) {
        this.pauseInterval = pauseInterval;
    }
    
    public short getPauseInterval() {
        return pauseInterval;
    }

    /**
     * DOCUMENT ME!
     *
     * @param  pauseTime  DOCUMENT ME!
     */
    public void setPauseTime(float pauseTime) {
        this.pauseTime = pauseTime;
    }
    
    public float getPauseTime() {
        return pauseTime;
    }

    /**
     * DOCUMENT ME!
     *
     * @param  PCVelocityEncoding  DOCUMENT ME!
     */
    public void setPCVelocityEncoding(short PCVelocityEncoding) {
        this.PCVelocityEncoding = PCVelocityEncoding;
    }
    
    public short getPCVelocityEncoding() {
        return PCVelocityEncoding;
    }

    /**
     * DOCUMENT ME!
     *
     * @param  peakSAR  DOCUMENT ME!
     */
    public void setPeakSAR(float peakSAR) {
        this.peakSAR = peakSAR;
    }
    
    public float getPeakSAR() {
        return peakSAR;
    }

    /**
     * DOCUMENT ME!
     *
     * @param  pfSwapped  DOCUMENT ME!
     */
    public void setPFSwapped(String pfSwapped) {
        this.pfSwapped = pfSwapped;
    }
    
    public String getPFSwapped() {
        return pfSwapped;
    }

    /**
     * DOCUMENT ME!
     *
     * @param  phaseContrastFlowAxis  DOCUMENT ME!
     */
    public void setPhaseContrastFlowAxis(String phaseContrastFlowAxis) {
        this.phaseContrastFlowAxis = phaseContrastFlowAxis;
    }
    
    public String getPhaseContrastFlowAxis() {
        return phaseContrastFlowAxis;
    }

    /**
     * DOCUMENT ME!
     *
     * @param  pImage2  DOCUMENT ME!
     */
    public void setPImage2(String pImage2) {
        this.pImage2 = pImage2;
    }
    
    public String getPImage2() {
        return pImage2;
    }

    /**
     * DOCUMENT ME!
     *
     * @param  pixelSize  // In practice 5 * pixel resolution[2]
     */
    public void setPixelSize(float pixelSize) {
        this.pixelSize = pixelSize;
    }
    
    public float getPixelSize() {
        return pixelSize;
    }

    /**
     * DOCUMENT ME!
     *
     * @param  planeName  DOCUMENT ME!
     */
    public void setPlaneName(String planeName) {
        this.planeName = planeName;
    }
    
    public String getPlaneName() {
        return planeName;
    }

    /**
     * DOCUMENT ME!
     *
     * @param  planeType  DOCUMENT ME!
     */
    public void setPlaneType(String planeType) {
        this.planeType = planeType;
    }
    
    public String getPlaneType() {
        return planeType;
    }

    /**
     * DOCUMENT ME!
     *
     * @param  position  DOCUMENT ME!
     */
    public void setPosition(String position) {
        this.position = position;
    }
    
    public String getPosition() {
        return position;
    }

    /**
     * DOCUMENT ME!
     *
     * @param  prescanReceiveAttenuation1  DOCUMENT ME!
     */
    public void setPrescanReceiveAttenuation1(short prescanReceiveAttenuation1) {
        this.prescanReceiveAttenuation1 = prescanReceiveAttenuation1;
    }
    
    public short getPrescanReceiveAttenuation1() {
        return prescanReceiveAttenuation1;
    }

    /**
     * DOCUMENT ME!
     *
     * @param  prescanReceiveAttenuation2  DOCUMENT ME!
     */
    public void setPrescanReceiveAttenuation2(short prescanReceiveAttenuation2) {
        this.prescanReceiveAttenuation2 = prescanReceiveAttenuation2;
    }
    
    public short getPrescanReceiveAttenuation2() {
        return prescanReceiveAttenuation2;
    }

    /**
     * DOCUMENT ME!
     *
     * @param  prescribedImageNumbers  DOCUMENT ME!
     */
    public void setPrescribedImageNumbers(String prescribedImageNumbers) {
        this.prescribedImageNumbers = prescribedImageNumbers;
    }
    
    public String getPrescribedImageNumbers() {
        return prescribedImageNumbers;
    }

    /**
     * DOCUMENT ME!
     *
     * @param  prescribedSeriesNumbers  DOCUMENT ME!
     */
    public void setPrescribedSeriesNumbers(String prescribedSeriesNumbers) {
        this.prescribedSeriesNumbers = prescribedSeriesNumbers;
    }
    
    public String getPrescribedSeriesNumbers() {
        return prescribedSeriesNumbers;
    }

    /**
     * DOCUMENT ME!
     *
     * @param  projectionAngle  DOCUMENT ME!
     */
    public void setProjectionAngle(float projectionAngle) {
        this.projectionAngle = projectionAngle;
    }
    
    public float getProjectionAngle() {
        return projectionAngle;
    }

    /**
     * DOCUMENT ME!
     *
     * @param  psdDay  DOCUMENT ME!
     */
    public void setPSDDay(short psdDay) {
        this.psdDay = psdDay;
    }
    
    public short getPSDDay() {
        return psdDay;
    }

    /**
     * DOCUMENT ME!
     *
     * @param  psdFileName  DOCUMENT ME!
     */
    public void setPSDFileName(String psdFileName) {
        this.psdFileName = psdFileName;
    }
    
    public String getPSDFileName() {
        return psdFileName;
    }

    /**
     * DOCUMENT ME!
     *
     * @param  psdHour  DOCUMENT ME!
     */
    public void setPSDHour(short psdHour) {
        this.psdHour = psdHour;
    }
    
    public short getPSDHour() {
        return psdHour;
    }

    /**
     * DOCUMENT ME!
     *
     * @param  psdMinute  DOCUMENT ME!
     */
    public void setPSDMinute(short psdMinute) {
        this.psdMinute = psdMinute;
    }
    
    public short getPSDMinute() {
        return psdMinute;
    }

    /**
     * DOCUMENT ME!
     *
     * @param  psdMonth  DOCUMENT ME!
     */
    public void setPSDMonth(short psdMonth) {
        this.psdMonth = psdMonth;
    }
    
    public short getPSDMonth() {
        return psdMonth;
    }

    /**
     * DOCUMENT ME!
     *
     * @param  psdName  DOCUMENT ME!
     */
    public void setPSDName(String psdName) {
        this.psdName = psdName;
    }
    
    public String getPSDName() {
        return psdName;
    }

    /**
     * DOCUMENT ME!
     *
     * @param  psdSeconds  DOCUMENT ME!
     */
    public void setPSDSeconds(short psdSeconds) {
        this.psdSeconds = psdSeconds;
    }
    
    public short getPSDSeconds() {
        return psdSeconds;
    }

    /**
     * DOCUMENT ME!
     *
     * @param  psdYear  Year - 1900
     */
    public void setPSDYear(short psdYear) {
        this.psdYear = psdYear;
    }
    
    public short getPSDYear() {
        return psdYear;
    }

    /**
     * DOCUMENT ME!
     *
     * @param  pSeries2  DOCUMENT ME!
     */
    public void setPSeries2(String pSeries2) {
        this.pSeries2 = pSeries2;
    }
    
    public String getPSeries2() {
        return pSeries2;
    }

    /**
     * DOCUMENT ME!
     *
     * @param  pulseSequence  DOCUMENT ME!
     */
    public void setPulseSequence(String pulseSequence) {
        this.pulseSequence = pulseSequence;
    }
    
    public String getPulseSequence() {
        return pulseSequence;
    }

    /**
     * DOCUMENT ME!
     *
     * @param  pulseSequenceMode  DOCUMENT ME!
     */
    public void setPulseSequenceMode(String pulseSequenceMode) {
        this.pulseSequenceMode = pulseSequenceMode;
    }
    
    public String getPulseSequenceMode() {
        return pulseSequenceMode;
    }

    /**
     * DOCUMENT ME!
     *
     * @param  pulseSequenceSubtype  DOCUMENT ME!
     */
    public void setPulseSequenceSubtype(String pulseSequenceSubtype) {
        this.pulseSequenceSubtype = pulseSequenceSubtype;
    }
    
    public String getPulseSequenceSubtype() {
        return pulseSequenceSubtype;
    }

    /**
     * DOCUMENT ME!
     *
     * @param  R1  DOCUMENT ME!
     */
    public void setR1(short R1) {
        this.R1 = R1;
    }
    
    public short getR1() {
        return R1;
    }

    /**
     * DOCUMENT ME!
     *
     * @param  R2  DOCUMENT ME!
     */
    public void setR2(short R2) {
        this.R2 = R2;
    }
    
    public short getR2() {
        return R2;
    }

    /**
     * DOCUMENT ME!
     *
     * @param  rawDataStudyNumber  DOCUMENT ME!
     */
    public void setRawDataStudyNumber(String rawDataStudyNumber) {
        this.rawDataStudyNumber = rawDataStudyNumber;
    }
    
    public String getRawDataStudyNumber() {
        return rawDataStudyNumber;
    }

    /**
     * DOCUMENT ME!
     *
     * @param  rawDataSystemID  DOCUMENT ME!
     */
    public void setRawDataSystemID(String rawDataSystemID) {
        this.rawDataSystemID = rawDataSystemID;
    }
    
    public String getRawDataSystemID() {
        return rawDataSystemID;
    }

    /**
     * DOCUMENT ME!
     *
     * @param  rCenter  DOCUMENT ME!
     */
    public void setRCenter(float rCenter) {
        this.rCenter = rCenter;
    }
    
    public float getRCenter() {
        return rCenter;
    }

    /**
     * DOCUMENT ME!
     *
     * @param  receiveAttenuatorSetting  DOCUMENT ME!
     */
    public void setReceiveAttenuatorSetting(short receiveAttenuatorSetting) {
        this.receiveAttenuatorSetting = receiveAttenuatorSetting;
    }
    
    public short getReceiveAttenuatorSetting() {
        return receiveAttenuatorSetting;
    }

    /**
     * DOCUMENT ME!
     *
     * @param  recommendedReceiveAttenuation  DOCUMENT ME!
     */
    public void setRecommendedReceiveAttenuation(int recommendedReceiveAttenuation) {
        this.recommendedReceiveAttenuation = recommendedReceiveAttenuation;
    }
    
    public int getRecommendedReceiveAttenuation() {
        return recommendedReceiveAttenuation;
    }

    /**
     * DOCUMENT ME!
     *
     * @param  recommendedReceiveFrequency  DOCUMENT ME!
     */
    public void setRecommendedReceiveFrequency(int recommendedReceiveFrequency) {
        this.recommendedReceiveFrequency = recommendedReceiveFrequency;
    }
    
    public int getRecommendedReceiveFrequency() {
        return recommendedReceiveFrequency;
    }

    /**
     * DOCUMENT ME!
     *
     * @param  recommendedTransmitAttenuation  DOCUMENT ME!
     */
    public void setRecommendedTransmitAttenuation(int recommendedTransmitAttenuation) {
        this.recommendedTransmitAttenuation = recommendedTransmitAttenuation;
    }
    
    public int getRecommendedTransmitAttenuation() {
        return recommendedTransmitAttenuation;
    }

    /**
     * DOCUMENT ME!
     *
     * @param  recommendedTransmitFrequency  DOCUMENT ME!
     */
    public void setRecommendedTransmitFrequency(int recommendedTransmitFrequency) {
        this.recommendedTransmitFrequency = recommendedTransmitFrequency;
    }
    
    public int getRecommendedTransmitFrequency() {
        return recommendedTransmitFrequency;
    }

    /**
     * DOCUMENT ME!
     *
     * @param  referringPhysician  DOCUMENT ME!
     */
    public void setReferringPhysician(String referringPhysician) {
        this.referringPhysician = referringPhysician;
    }
    
    public String getReferringPhysician() {
        return referringPhysician;
    }

    /**
     * DOCUMENT ME!
     *
     * @param  requestedNumber  DOCUMENT ME!
     */
    public void setRequestedNumber(String requestedNumber) {
        this.requestedNumber = requestedNumber;
    }
    
    public String getRequestedNumber() {
        return requestedNumber;
    }
    
    public void setResearchMode(String researchMode) {
    	this.researchMode = researchMode;
    }

    /**
     * DOCUMENT ME!
     *
     * @param  researchMode  DOCUMENT ME!
     */
    public String getResearchMode() {
        return researchMode;
    }

    /**
     * DOCUMENT ME!
     *
     * @param  rlCenter  DOCUMENT ME!
     */
    public void setRLCenter(float rlCenter) {
        this.rlCenter = rlCenter;
    }
    
    public float getRLCenter() {
        return rlCenter;
    }

    /**
     * DOCUMENT ME!
     *
     * @param  rNormal  DOCUMENT ME!
     */
    public void setRNormal(float rNormal) {
        this.rNormal = rNormal;
    }
    
    public float getRNormal() {
        return rNormal;
    }

    /**
     * DOCUMENT ME!
     *
     * @param  round  DOCUMENT ME!
     */
    public void setRound(String round) {
        this.round = round;
    }
    
    public String getRound() {
        return round;
    }

    /**
     * DOCUMENT ME!
     *
     * @param  SARMonitored  DOCUMENT ME!
     */
    public void setSARMonitored(String SARMonitored) {
        this.SARMonitored = SARMonitored;
    }
    
    public String getSARMonitored() {
        return SARMonitored;
    }

    /**
     * DOCUMENT ME!
     *
     * @param  satSelections  DOCUMENT ME!
     */
    public void setSATSelections(String satSelections) {
        this.satSelections = satSelections;
    }
    
    public String getSATSelections() {
        return satSelections;
    }

    /**
     * DOCUMENT ME!
     *
     * @param  satXLoc1  DOCUMENT ME!
     */
    public void setSATXLoc1(short satXLoc1) {
        this.satXLoc1 = satXLoc1;
    }
    
    public short getSATXLoc1() {
        return satXLoc1;
    }

    /**
     * DOCUMENT ME!
     *
     * @param  satXLoc2  DOCUMENT ME!
     */
    public void setSATXLoc2(short satXLoc2) {
        this.satXLoc2 = satXLoc2;
    }
    
    public short getSATXLoc2() {
        return satXLoc2;
    }

    /**
     * DOCUMENT ME!
     *
     * @param  satXThick  DOCUMENT ME!
     */
    public void setSATXThick(short satXThick) {
        this.satXThick = satXThick;
    }
    
    public short getSATXThick() {
        return satXThick;
    }

    /**
     * DOCUMENT ME!
     *
     * @param  satYLoc1  DOCUMENT ME!
     */
    public void setSATYLoc1(short satYLoc1) {
        this.satYLoc1 = satYLoc1;
    }
    
    public short getSATYLoc1() {
        return satYLoc1;
    }

    /**
     * DOCUMENT ME!
     *
     * @param  satYLoc2  DOCUMENT ME!
     */
    public void setSATYLoc2(short satYLoc2) {
        this.satYLoc2 = satYLoc2;
    }
    
    public short getSATYLoc2() {
        return satYLoc2;
    }

    /**
     * DOCUMENT ME!
     *
     * @param  satYThick  DOCUMENT ME!
     */
    public void setSATYThick(short satYThick) {
        this.satYThick = satYThick;
    }
    
    public short getSATYThick() {
        return satYThick;
    }

    /**
     * DOCUMENT ME!
     *
     * @param  satZLoc1  DOCUMENT ME!
     */
    public void setSATZLoc1(short satZLoc1) {
        this.satZLoc1 = satZLoc1;
    }
    
    public short getSATZLoc1() {
        return satZLoc1;
    }

    /**
     * DOCUMENT ME!
     *
     * @param  satZLoc2  DOCUMENT ME!
     */
    public void setSATZLoc2(short satZLoc2) {
        this.satZLoc2 = satZLoc2;
    }
    
    public short getSATZLoc2() {
        return satZLoc2;
    }

    /**
     * DOCUMENT ME!
     *
     * @param  satZThick  DOCUMENT ME!
     */
    public void setSATZThick(short satZThick) {
        this.satZThick = satZThick;
    }
    
    public short getSATZThick() {
        return satZThick;
    }

    /**
     * DOCUMENT ME!
     *
     * @param  scanAcquisitionNumber  DOCUMENT ME!
     */
    public void setScanAcquisitionNumber(short scanAcquisitionNumber) {
        this.scanAcquisitionNumber = scanAcquisitionNumber;
    }
    
    public short getScanAcquisitionNumber() {
        return scanAcquisitionNumber;
    }

    /**
     * DOCUMENT ME!
     *
     * @param  scanARRs  DOCUMENT ME!
     */
    public void setScanARRs(int scanARRs) {
        this.scanARRs = scanARRs;
    }
    
    public int getScanARRs() {
        return scanARRs;
    }

    /**
     * DOCUMENT ME!
     *
     * @param  scanMatrixX  DOCUMENT ME!
     */
    public void setScanMatrixX(short scanMatrixX) {
        this.scanMatrixX = scanMatrixX;
    }
    
    public short getScanMatrixX() {
        return scanMatrixX;
    }

    /**
     * DOCUMENT ME!
     *
     * @param  scanMatrixY  DOCUMENT ME!
     */
    public void setScanMatrixY(short scanMatrixY) {
        this.scanMatrixY = scanMatrixY;
    }
    
    public short getScanMatrixY() {
        return scanMatrixY;
    }

    /**
     * DOCUMENT ME!
     *
     * @param  scanProtocolName  DOCUMENT ME!
     */
    public void setScanProtocolName(String scanProtocolName) {
        this.scanProtocolName = scanProtocolName;
    }
    
    public String getScanProtocolName() {
        return scanProtocolName;
    }

    /**
     * DOCUMENT ME!
     *
     * @param  sCenter  DOCUMENT ME!
     */
    public void setSCenter(float sCenter) {
        this.sCenter = sCenter;
    }
    
    public float getSCenter() {
        return sCenter;
    }

    /**
     * DOCUMENT ME!
     *
     * @param  scIma  DOCUMENT ME!
     */
    public void setScIma(String scIma) {
        this.scIma = scIma;
    }
    
    public String getScIma() {
        return scIma;
    }

    /**
     * DOCUMENT ME!
     *
     * @param  scSer  DOCUMENT ME!
     */
    public void setScSer(String scSer) {
        this.scSer = scSer;
    }
    
    public String getScSer() {
        return scSer;
    }

    /**
     * DOCUMENT ME!
     *
     * @param  series  DOCUMENT ME!
     */
    public void setSeries(String series) {
        this.series = series;
    }
    
    public String getSeries() {
        return series;
    }

    /**
     * DOCUMENT ME!
     *
     * @param  seriesDate  DOCUMENT ME!
     */
    public void setSeriesDate(String seriesDate) {
        this.seriesDate = seriesDate;
    }
    
    public String getSeriesDate() {
        return seriesDate;
    }

    /**
     * DOCUMENT ME!
     *
     * @param  seriesDescription  DOCUMENT ME!
     */
    public void setSeriesDescription(String seriesDescription) {
        this.seriesDescription = seriesDescription;
    }
    
    public String getSeriesDescription() {
        return seriesDescription;
    }

    /**
     * DOCUMENT ME!
     *
     * @param  seriesHeaderBlocks  DOCUMENT ME!
     */
    public void setSeriesHeaderBlocks(short seriesHeaderBlocks) {
        this.seriesHeaderBlocks = seriesHeaderBlocks;
    }
    
    public short getSeriesHeaderBlocks() {
        return seriesHeaderBlocks;
    }

    /**
     * DOCUMENT ME!
     *
     * @param  seriesHeaderID  DOCUMENT ME!
     */
    public void setSeriesHeaderID(String seriesHeaderID) {
        this.seriesHeaderID = seriesHeaderID;
    }
    
    public String getSeriesHeaderID() {
        return seriesHeaderID;
    }

    /**
     * DOCUMENT ME!
     *
     * @param  seriesHeaderRevisionNumber  DOCUMENT ME!
     */
    public void setSeriesHeaderRevisionNumber(String seriesHeaderRevisionNumber) {
        this.seriesHeaderRevisionNumber = seriesHeaderRevisionNumber;
    }
    
    public String getSeriesHeaderRevisionNumber() {
        return seriesHeaderRevisionNumber;
    }

    /**
     * DOCUMENT ME!
     *
     * @param  seriesNumber  DOCUMENT ME!
     */
    public void setSeriesNumber(String seriesNumber) {
        this.seriesNumber = seriesNumber;
    }
    
    public String getSeriesNumber() {
        return seriesNumber;
    }

    /**
     * DOCUMENT ME!
     *
     * @param  seriesProcessName  DOCUMENT ME!
     */
    public void setSeriesProcessName(String seriesProcessName) {
        this.seriesProcessName = seriesProcessName;
    }
    
    public String getSeriesProcessName() {
        return seriesProcessName;
    }

    /**
     * DOCUMENT ME!
     *
     * @param  seriesPSDName  DOCUMENT ME!
     */
    public void setSeriesPSDName(String seriesPSDName) {
        this.seriesPSDName = seriesPSDName;
    }
    
    public String getSeriesPSDName() {
        return seriesPSDName;
    }

    /**
     * DOCUMENT ME!
     *
     * @param  seriesRawDataSystemID  DOCUMENT ME!
     */
    public void setSeriesRawDataSystemID(String seriesRawDataSystemID) {
        this.seriesRawDataSystemID = seriesRawDataSystemID;
    }
    
    public String getSeriesRawDataSystemID() {
        return seriesRawDataSystemID;
    }

    /**
     * DOCUMENT ME!
     *
     * @param  seriesSystemGenerationID  DOCUMENT ME!
     */
    public void setSeriesSystemGenerationID(String seriesSystemGenerationID) {
        this.seriesSystemGenerationID = seriesSystemGenerationID;
    }
    
    public String getSeriesSystemGenerationID() {
        return seriesSystemGenerationID;
    }

    /**
     * DOCUMENT ME!
     *
     * @param  seriesTaskID  DOCUMENT ME!
     */
    public void setSeriesTaskID(short seriesTaskID) {
        this.seriesTaskID = seriesTaskID;
    }
    
    public short getSeriesTaskID() {
        return seriesTaskID;
    }

    /**
     * DOCUMENT ME!
     *
     * @param  seriesTime  DOCUMENT ME!
     */
    public void setSeriesTime(String seriesTime) {
        this.seriesTime = seriesTime;
    }
    
    public String getSeriesTime() {
        return seriesTime;
    }

    /**
     * DOCUMENT ME!
     *
     * @param  seriesType  DOCUMENT ME!
     */
    public void setSeriesType(String seriesType) {
        this.seriesType = seriesType;
    }
    
    public String getSeriesType() {
        return seriesType;
    }

    /**
     * DOCUMENT ME!
     *
     * @param  siCenter  DOCUMENT ME!
     */
    public void setSICenter(float siCenter) {
        this.siCenter = siCenter;
    }
    
    public float getSICenter() {
        return siCenter;
    }

    /**
     * DOCUMENT ME!
     *
     * @param  sliceMultiplier  DOCUMENT ME!
     */
    public void setSliceMultiplier(short sliceMultiplier) {
        this.sliceMultiplier = sliceMultiplier;
    }
    
    public short getSliceMultiplier() {
        return sliceMultiplier;
    }

    /**
     * DOCUMENT ME!
     *
     * @param  sliceQuantity  DOCUMENT ME!
     */
    public void setSliceQuantity(short sliceQuantity) {
        this.sliceQuantity = sliceQuantity;
    }
    
    public short getSliceQuantity() {
        return sliceQuantity;
    }

    /**
     * DOCUMENT ME!
     *
     * @param  sliceThicknessDisclaimer  DOCUMENT ME!
     */
    public void setSliceThicknessDisclaimer(String sliceThicknessDisclaimer) {
        this.sliceThicknessDisclaimer = sliceThicknessDisclaimer;
    }
    
    public String getSliceThicknessDisclaimer() {
        return sliceThicknessDisclaimer;
    }

    /**
     * DOCUMENT ME!
     *
     * @param  sNormal  DOCUMENT ME!
     */
    public void setSNormal(float sNormal) {
        this.sNormal = sNormal;
    }
    
    public float getSNormal() {
        return sNormal;
    }

    /**
     * DOCUMENT ME!
     *
     * @param  startX  DOCUMENT ME!
     */
    public void setStartX(float startX) {
        this.startX = startX;
    }
    
    public float getStartX() {
        return startX;
    }

    /**
     * DOCUMENT ME!
     *
     * @param  startY  DOCUMENT ME!
     */
    public void setStartY(float startY) {
        this.startY = startY;
    }
    
    public float getStartY() {
        return startY;
    }

    /**
     * DOCUMENT ME!
     *
     * @param  startZ  DOCUMENT ME!
     */
    public void setStartZ(float startZ) {
        this.startZ = startZ;
    }
    
    public float getStartZ() {
        return startZ;
    }

    /**
     * DOCUMENT ME!
     *
     * @param  studyDate  DOCUMENT ME!
     */
    public void setStudyDate(String studyDate) {
        this.studyDate = studyDate;
    }
    
    public String getStudyDate() {
        return studyDate;
    }

    /**
     * DOCUMENT ME!
     *
     * @param  studyDescription  DOCUMENT ME!
     */
    public void setStudyDescription(String studyDescription) {
        this.studyDescription = studyDescription;
    }
    
    public String getStudyDescription() {
        return studyDescription;
    }

    /**
     * DOCUMENT ME!
     *
     * @param  studyHeaderBlocks  DOCUMENT ME!
     */
    public void setStudyHeaderBlocks(short studyHeaderBlocks) {
        this.studyHeaderBlocks = studyHeaderBlocks;
    }
    
    public short getStudyHeaderBlocks() {
        return studyHeaderBlocks;
    }

    /**
     * DOCUMENT ME!
     *
     * @param  studyHeaderID  DOCUMENT ME!
     */
    public void setStudyHeaderID(String studyHeaderID) {
        this.studyHeaderID = studyHeaderID;
    }
    
    public String getStudyHeaderID() {
        return studyHeaderID;
    }

    /**
     * DOCUMENT ME!
     *
     * @param  studyHeaderRevisionNumber  DOCUMENT ME!
     */
    public void setStudyHeaderRevisionNumber(String studyHeaderRevisionNumber) {
        this.studyHeaderRevisionNumber = studyHeaderRevisionNumber;
    }
    
    public String getStudyHeaderRevisionNumber() {
        return studyHeaderRevisionNumber;
    }

    /**
     * DOCUMENT ME!
     *
     * @param  studyNumber  DOCUMENT ME!
     */
    public void setStudyNumber(String studyNumber) {
        this.studyNumber = studyNumber;
    }
    
    public String getStudyNumber() {
        return studyNumber;
    }

    /**
     * DOCUMENT ME!
     *
     * @param  studyTaskID  DOCUMENT ME!
     */
    public void setStudyTaskID(short studyTaskID) {
        this.studyTaskID = studyTaskID;
    }
    
    public short getStudyTaskID() {
        return studyTaskID;
    }

    /**
     * DOCUMENT ME!
     *
     * @param  studyTime  DOCUMENT ME!
     */
    public void setStudyTime(String studyTime) {
        this.studyTime = studyTime;
    }
    
    public String getStudyTime() {
        return studyTime;
    }

    /**
     * DOCUMENT ME!
     *
     * @param  suppressionTechnique  DOCUMENT ME!
     */
    public void setSuppressionTechnique(String suppressionTechnique) {
        this.suppressionTechnique = suppressionTechnique;
    }
    
    public String getSuppressionTechnique() {
        return suppressionTechnique;
    }

    /**
     * DOCUMENT ME!
     *
     * @param  surfaceCoilIntensityCorrection  DOCUMENT ME!
     */
    public void setSurfaceCoilIntensityCorrection(String surfaceCoilIntensityCorrection) {
        this.surfaceCoilIntensityCorrection = surfaceCoilIntensityCorrection;
    }
    
    public String getSurfaceCoilIntensityCorrection() {
        return surfaceCoilIntensityCorrection;
    }

    /**
     * DOCUMENT ME!
     *
     * @param  surfaceCoilsCorrectionType  DOCUMENT ME!
     */
    public void setSurfaceCoilsCorrectionType(String surfaceCoilsCorrectionType) {
        this.surfaceCoilsCorrectionType = surfaceCoilsCorrectionType;
    }
    
    public String getSurfaceCoilsCorrectionType() {
        return surfaceCoilsCorrectionType;
    }

    /**
     * DOCUMENT ME!
     *
     * @param  surfaceCoilType  DOCUMENT ME!
     */
    public void setSurfaceCoilType(String surfaceCoilType) {
        this.surfaceCoilType = surfaceCoilType;
    }
    
    public String getSurfaceCoilType() {
        return surfaceCoilType;
    }

    /**
     * DOCUMENT ME!
     *
     * @param  swapPF  DOCUMENT ME!
     */
    public void setSwapPF(String swapPF) {
        this.swapPF = swapPF;
    }
    
    public String getSwapPF() {
        return swapPF;
    }

    /**
     * DOCUMENT ME!
     *
     * @param  systemConfigHospitalName  DOCUMENT ME!
     */
    public void setSystemConfigHospitalName(String systemConfigHospitalName) {
        this.systemConfigHospitalName = systemConfigHospitalName;
    }
    
    public String getSystemConfigHospitalName() {
        return systemConfigHospitalName;
    }

    /**
     * DOCUMENT ME!
     *
     * @param  systemGenerationID  DOCUMENT ME!
     */
    public void setSystemGenerationID(String systemGenerationID) {
        this.systemGenerationID = systemGenerationID;
    }
    
    public String getSystemGenerationID() {
        return systemGenerationID;
    }

    /**
     * DOCUMENT ME!
     *
     * @param  systemID  DOCUMENT ME!
     */
    public void setSystemID(String systemID) {
        this.systemID = systemID;
    }
    
    public String getSystemID() {
        return systemID;
    }

    /**
     * DOCUMENT ME!
     *
     * @param  tableLocation  DOCUMENT ME!
     */
    public void setTableLocation(float tableLocation) {
        this.tableLocation = tableLocation;
    }
    
    public float getTableLocation() {
        return tableLocation;
    }

    /**
     * DOCUMENT ME!
     *
     * @param  tablePosition  DOCUMENT ME!
     */
    public void setTablePosition(float tablePosition) {
        this.tablePosition = tablePosition;
    }
    
    public float getTablePosition() {
        return tablePosition;
    }

    /**
     * DOCUMENT ME!
     *
     * @param  te  DOCUMENT ME!
     */
    public void setTE(float te) {
        this.te = te;
    }
    
    public float getTE() {
        return te;
    }

    /**
     * DOCUMENT ME!
     *
     * @param  TE2  DOCUMENT ME!
     */
    public void setTE2(float TE2) {
        this.TE2 = TE2;
    }
    
    public float getTE2() {
        return TE2;
    }

    /**
     * DOCUMENT ME!
     *
     * @param  thickness  = in practice 10*res[0] = 10 *res[1]
     */
    public void setThickness(float thickness) {
        this.thickness = thickness;
    }
    
    public float getThickness() {
        return thickness;
    }

    /**
     * DOCUMENT ME!
     *
     * @param  ti  DOCUMENT ME!
     */
    public void setTI(float ti) {
        this.ti = ti;
    }
    
    public float getTI() {
        return ti;
    }

    /**
     * DOCUMENT ME!
     *
     * @param  totalPostTriggerDelayTime  DOCUMENT ME!
     */
    public void setTotalPostTriggerDelayTime(float totalPostTriggerDelayTime) {
        this.totalPostTriggerDelayTime = totalPostTriggerDelayTime;
    }
    
    public float getTotalPostTriggerDelayTime() {
        return totalPostTriggerDelayTime;
    }

    /**
     * DOCUMENT ME!
     *
     * @param  tr  DOCUMENT ME!
     */
    public void setTR(float tr) {
        this.tr = tr;
    }
    
    public float getTR() {
        return tr;
    }

    /**
     * DOCUMENT ME!
     *
     * @param  transmitAttenuatorSetting  DOCUMENT ME!
     */
    public void setTransmitAttenuatorSetting(short transmitAttenuatorSetting) {
        this.transmitAttenuatorSetting = transmitAttenuatorSetting;
    }
    
    public short getTransmitAttenuatorSetting() {
        return transmitAttenuatorSetting;
    }

    /**
     * DOCUMENT ME!
     *
     * @param  ts  DOCUMENT ME!
     */
    public void setTS(float ts) {
        this.ts = ts;
    }
    
    public float getTS() {
        return ts;
    }

    /**
     * DOCUMENT ME!
     *
     * @param  user0  DOCUMENT ME!
     */
    public void setUser0(float user0) {
        this.user0 = user0;
    }
    
    public float getUser0() {
        return user0;
    }
    
    public void setUser1(float user1) {
    	this.user1 = user1;
    }

    /**
     * DOCUMENT ME!
     *
     * @param  user1  DOCUMENT ME!
     */
    public float getUser1() {
        return user1;
    }

    /**
     * DOCUMENT ME!
     *
     * @param  user2  DOCUMENT ME!
     */
    public void setUser2(float user2) {
        this.user2 = user2;
    }
    
    public float getUser2() {
        return user2;
    }

    /**
     * DOCUMENT ME!
     *
     * @param  user3  DOCUMENT ME!
     */
    public void setUser3(float user3) {
        this.user3 = user3;
    }
    
    public float getUser3() {
        return user3;
    }

    /**
     * DOCUMENT ME!
     *
     * @param  user4  DOCUMENT ME!
     */
    public void setUser4(float user4) {
        this.user4 = user4;
    }
    
    public float getUser4() {
        return user4;
    }

    /**
     * DOCUMENT ME!
     *
     * @param  user5  DOCUMENT ME!
     */
    public void setUser5(float user5) {
        this.user5 = user5;
    }
    
    public float getUser5() {
        return user5;
    }

    /**
     * DOCUMENT ME!
     *
     * @param  user6  DOCUMENT ME!
     */
    public void setUser6(float user6) {
        this.user6 = user6;
    }
    
    public float getUser6() {
        return user6;
    }

    /**
     * DOCUMENT ME!
     *
     * @param  user7  DOCUMENT ME!
     */
    public void setUser7(float user7) {
        this.user7 = user7;
    }
    
    public float getUser7() {
        return user7;
    }

    /**
     * DOCUMENT ME!
     *
     * @param  user8  DOCUMENT ME!
     */
    public void setUser8(float user8) {
        this.user8 = user8;
    }
    
    public float getUser8() {
        return user8;
    }

    /**
     * DOCUMENT ME!
     *
     * @param  user9  DOCUMENT ME!
     */
    public void setUser9(float user9) {
        this.user9 = user9;
    }
    
    public float getUser9() {
        return user9;
    }

    /**
     * DOCUMENT ME!
     *
     * @param  variableBandwidth  DOCUMENT ME!
     */
    public void setVariableBandwidth(String variableBandwidth) {
        this.variableBandwidth = variableBandwidth;
    }
    
    public String getVariableBandwidth() {
        return variableBandwidth;
    }

    /**
     * DOCUMENT ME!
     *
     * @param  vascularImagingFlags  DOCUMENT ME!
     */
    public void setVascularImagingFlags(String vascularImagingFlags) {
        this.vascularImagingFlags = vascularImagingFlags;
    }
    
    public String getVascularImagingFlags() {
        return vascularImagingFlags;
    }

    /**
     * DOCUMENT ME!
     *
     * @param  vasMode  DOCUMENT ME!
     */
    public void setVasMode(String vasMode) {
        this.vasMode = vasMode;
    }
    
    public String getVasMode() {
        return vasMode;
    }

    /**
     * DOCUMENT ME!
     *
     * @param  vencScalingFactor  DOCUMENT ME!
     */
    public void setVencScalingFactor(float vencScalingFactor) {
        this.vencScalingFactor = vencScalingFactor;
    }
    
    public float getVencScalingFactor() {
        return vencScalingFactor;
    }

    /**
     * DOCUMENT ME!
     *
     * @param  verticalAnatomicalReference  DOCUMENT ME!
     */
    public void setVerticalAnatomicalReference(String verticalAnatomicalReference) {
        this.verticalAnatomicalReference = verticalAnatomicalReference;
    }
    
    public String getVerticalAnatomicalReference() {
        return verticalAnatomicalReference;
    }

    /**
     * DOCUMENT ME!
     *
     * @param  verticalLandmark  DOCUMENT ME!
     */
    public void setVerticalLandmark(float verticalLandmark) {
        this.verticalLandmark = verticalLandmark;
    }
    
    public float getVerticalLandmark() {
        return verticalLandmark;
    }
    
    public void anonymize() {
    	patientID = new String("XXXXXX      "); // length 12
    	patientName = new String("XXXXX, XXXXX                    "); // length 32
    }
}
