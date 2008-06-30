package gov.nih.mipav.model.file;


import gov.nih.mipav.model.structures.*;

import gov.nih.mipav.view.*;

import java.io.*;


/**
 * Fixed format header
 * Image data is not compressed
 * Image data fixed offset 14336 bytes
 * Data General host
 * 
 * The image files are of fixed layout, described here as a series of 256 by 16
 * bit word blocks (512 bytes), blocks numbered from 0.  The headers start at 
 * the following block offsets:
 *     block 0  - length 4 blocks    -     System configuration
 *     block 4  - length 2 blocks    -     Site customization
 *     block 6  - length 2 blocks    -     Study header
 *     block 8  - length 2 blocks    -     Series header
 *     block 10 - length 2 blocks    -     Image header
 *     block 12 - length 4 blocks    -     Raw database header
 *     block 16 - length 10 blocks   -     Pulse sequence description
 *     block 26 - length 2 blocks    -     Pixel map (? not ever used)
 *     block 28 - length 256 blocks  -     Image data
 *     
 * The header is a fixed length of 14336 bytes, after which the uncompressed
 * image data starts.
 * 
 * 16 bit big endian shorts are used.  Ascii strings are FORTRAN style
 * specifications with length in bytes.  4 byte floats are used.
 * 
 * Spacing between slices was seen to vary so resolution[2] cannot be obtained
 * by checking the difference between slice spacing.  The field called pixel
 * size actually gives 5 * resolution[2]. 
 * resolution[0] and resolution[1] can in theory be calculated in 3 different
 * ways with 3 slightly different results.  (fov/256)*kludge factor 2,
 * (difference between corners/255) * kludge factor 2, or
 * using the thickness field equals about 10 * resolution[0] = 10 * resolution[1].
 * Use fov/256 multiplied by a kludge factor of 2 to obtain the right answer.
 * 
 * On one run SWAP_PF said Operator selects to swap phase and frequency and PF_SWAPPED
 * said Phase and frequency not swapped, a seeming contradiction.  The field strength
 * in the series header was read as -99 gauss.  The 6 values for SAT pulse location
 * relative to landmark were all 9999 millimeters. So there are problems.
 */
public class FileGESigna4X extends FileBase {

    //~ Instance fields ------------------------------------------------------------------------------------------------

    private String systemID = null;
    
    private String systemConfigHospitalName = null;
    
    private String studyHeaderID = null;
    
    private String studyHeaderRevisionNumber = null;
    
    private short studyHeaderBlocks;
    
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
    
    private int patientWeight;
    
    private String referringPhysician = null;
    
    private String diognostician = null;
    
    private String operator = null;
    
    private String studyDescription = null;
    
    private String history = null;
    
    private String hospitalName = null;
    
    private short patientStatus;
    
    private String requestedNumber = null;
    
    private String seriesHeaderID = null;
    
    private String seriesHeaderRevisionNumber = null;
    
    private int seriesRevisionSubnumber;
    
    private short seriesHeaderBlocks;
    
    private String seriesProcessName = null;
    
    private short seriesTaskID;
    
    private String originalSeriesNumber = null;
    
    private String seriesNumber = null;
    
    private String seriesRawDataSystemID = null;
    
    private String seriesSystemGenerationID = null;
    
    private String seriesDate = null;
    
    private String seriesTime = null;
    
    private String seriesDescription = null;
    
    private short seriesType;
    
    private short coilType;
    
    private String coilName = null;
    
    private String contrastDescription = null;
    
    private short planeType;
    
    private String planeName;
    
    private short imageMode;
    
    private short fieldStrength; // gauss
    
    private short pulseSequence;
    
    private short pulseSequenceSubtype;
    
    private float fieldOfView; // mm
    
    private float rlCenter;
    
    private float apCenter;
    
    private float siCenter;
    
    private short orientation;
    
    private short position;
    
    private String longitudinalAnatomicalReference = null;
    
    private String verticalAnatomicalReference = null;
    
    private float verticalLandmark;
    
    private float horizontalLandmark;
    
    private float tableLocation;
    
    private short scanMatrixX;
    
    private short scanMatrixY;
    
    private short imageMatrix;
    
    private short imagesAllocated;
    
    private short gatingType;
    
    private short pulseSequenceMode;
    
    private String gatingTypeString = null;
    
    private String seriesPSDName = null;
    
    private int landmarkCounter;
    
    private String scanProtocolName = null;
    
    private short surfaceCoilType;
    
    private short suppressionTechnique;
    
    private short satSelections;
    
    private String satSelectionsString = null;
    
    private short surfaceCoilIntensityCorrection;
    
    private short satXLoc1;
    
    private short satXLoc2;
    
    private short satYLoc1;
    
    private short satYLoc2;
    
    private short satZLoc1;
    
    private short satZLoc2;
    
    private short satXThick;
    
    private short satYThick;
    
    private short satZThick;
    
    private short vasMode;
    
    private short phaseContrastFlowAxis;
    
    private short gatingType2;
    
    private String imageHeaderID = null;
    
    private String imageHeaderRevisionNumber = null;
    
    private int imageRevisionSubnumber;
    
    private short imageHeaderBlocks;
    
    private String imageHeaderCreatorProcess = null;
    
    private short imageHeaderCreatorTask;
    
    private String imageCreationDate = null;
    
    private String imageCreationTime = null;
    
    private String imageNumber = null;
    
    private String series = null;
    
    private String imageRawDataSystemID = null;
    
    private String imageSystemGenerationID = null;
    
    private float startX;
    
    private float endX;
    
    private float startY;
    
    private float endY;
    
    private float startZ;
    
    private float endZ;
    
    private float imageLocation;
    
    private float tablePosition;
    
    private float thickness; // In practice 10*res[0] = 10*res[1]
    
    private float imageSpacing;
    
    private short round;
    
    private float tr; // usec
    
    private float ts;
    
    private float te; // usec
    
    private float ti; // usec
    
    private short numberOfEchos;
    
    private short echoNumber;
    
    private short sliceQuantity;
    
    private short averagesNumber;
    
    private short researchMode;
    
    private String psdFileName;
    
    private short psdDay;
    
    private short psdMonth;
    
    private short psdYear; // Year - 1900
    
    private short psdHour;
    
    private short psdMinute;
    
    private short psdSeconds;
    
    private short graphicallyPrescribed;
    
    private String prescribedSeriesNumbers;
    
    private String prescribedImageNumbers;
    
    private short imageShape;
    
    private float pixelSize; // In practice 5 * res[2]
    
    private short defaultWindow;
    
    private short defaultLevel;
    
    private short fileBlocks;
    
    private float excitationsNumber;
    
    private float peakSAR;
    
    private float averageSAR;
    
    private short SARMonitored;
    
    private short contiguousSlices;
    
    private short cardiacHeartRate;
    
    private float totalPostTriggerDelayTime;
    
    private short arrythmiaRejectionRatio;
    
    private short cardiacRepTime;
    
    private short imagesPerCardiacCycle;
    
    private int scanARRs;
    
    private short transmitAttenuatorSetting;
    
    private short receiveAttenuatorSetting;
    
    private int imageFieldStrength;
    
    private short imageOffset;
    
    private float interImageDelay;
    
    private String psdName;
    
    private short flipAngle;
    
    private String surfaceCoilsCorrectionType;
    
    private String scSer;
    
    private String scIma;
    
    private short extremityCoil;
    
    private String pSeries2;
    
    private String pImage2;
    
    private float rCenter;
    
    private float aCenter;
    
    private float sCenter;
    
    private float rNormal;
    
    private float aNormal;
    
    private float sNormal;
    
    private float imgTLHC_R;
    
    private float imgTLHC_A;
    
    private float imgTLHC_S;
    
    private float imgTRHC_R;
    
    private float imgTRHC_A;
    
    private float imgTRHC_S;
    
    private float imgBLHC_R;
    
    private float imgBLHC_A;
    
    private float imgBLHC_S;
    
    private short imageHeaderDisclaimer;
    
    private short minimumDelay;
    
    private short cPhase;
    
    private float TE2;
    
    private short swapPF;
    
    private short pauseInterval;
    
    private float pauseTime;
    
    private short userBitmap;
    
    private float user0;
    
    private float user1;
    
    private float user2;
    
    private float user3;
    
    private float user4;
    
    private float user5;
    
    private float user6;
    
    private float user7;
    
    private float user8;
    
    private float user9;
    
    private short obliquePlane;
    
    private short contrastUsed;
    
    private String contrastAgent;
    
    private float contrastAmount;
    
    private short fileFormat;
    
    private short autoCenterFrequency;
    
    private int actualTransmitFrequency;
    
    private int actualReceiveFrequency;
    
    private int recommendedTransmitFrequency;
    
    private int recommendedReceiveFrequency;
    
    private int recommendedTransmitAttenuation;
    
    private int recommendedReceiveAttenuation;
    
    private short histogramPresent;
    
    private short pfSwapped;
    
    private short R1;
    
    private short R2;
    
    private short variableBandwidth;
    
    private short prescanReceiveAttenuation1;
    
    private short prescanReceiveAttenuation2;
    
    private short autoManualPrescan;
    
    private short changedValuesBitmap;
    
    private String changedValuesString;
    
    private short imageType;
    
    private short collapseImage;
    
    private short sliceThicknessDisclaimer;
    
    private short PCVelocityEncoding;
    
    private float projectionAngle;
    
    private short concatenatedSATSelection;
    
    private short fractionalEffectiveEcho;
    
    private int echoTrainLength;
    
    private short sliceMultiplier;
    
    private short cardiacPhaseNumber;
    
    private short scanAcquisitionNumber;
    
    private short vascularImagingFlags;
    
    private float vencScalingFactor;
    
    /** DOCUMENT ME! */
    private byte[] byteBuffer = null;

    /** DOCUMENT ME! */
    private String fileDir;

    /** DOCUMENT ME! */
    private File fileHeader;

    /** DOCUMENT ME! */
    private FileInfoGESigna4X fileInfo;

    /** DOCUMENT ME! */
    private String fileName;
    
    private int[] orient = new int[3];

    private float[] start = new float[3];
    
//  The data types are Sun, hence the byte order is big-endian.
    private boolean endianess = BIG_ENDIAN;
    
    private short width;
    
    private short height;
    
    private short compression;
    
    private short bitsPerPixel;
    
    private float resX;
    
    private float resY;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * reader/writer constructor.
     *
     * @param      fileName  file name
     * @param      fileDir   file directory
     *
     * @exception  IOException  if there is an error making the file
     */
    public FileGESigna4X(String fileName, String fileDir) throws IOException {

        this.fileName = fileName;
        this.fileDir = fileDir;
    }

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * Prepares this class for cleanup. Calls the <code>finalize</code> method for existing elements, closes any open
     * files and sets other elements to <code>null</code>.
     */
    public void finalize() {
        fileName = null;
        fileDir = null;
        fileInfo = null;
        systemID = null;
        systemConfigHospitalName = null;
        studyHeaderID = null;
        studyHeaderRevisionNumber = null;
        MRIProcessName = null;
        rawDataStudyNumber = null;
        studyNumber = null;
        rawDataSystemID = null;
        systemGenerationID = null;
        studyDate = null;
        studyTime = null;
        patientName = null;
        patientID = null;
        patientAge = null;
        patientSex = null;
        referringPhysician = null;
        diognostician = null;
        operator = null;
        studyDescription = null;
        history = null;
        hospitalName = null;
        requestedNumber = null;
        seriesHeaderID = null;
        seriesHeaderRevisionNumber = null;
        seriesProcessName = null;
        originalSeriesNumber = null;
        seriesNumber = null;
        seriesRawDataSystemID = null;
        seriesSystemGenerationID = null;
        seriesDate = null;
        seriesTime = null;
        seriesDescription = null;
        coilName = null;
        contrastDescription = null;
        planeName = null;
        longitudinalAnatomicalReference = null;
        verticalAnatomicalReference = null;
        gatingTypeString = null;
        seriesPSDName = null;
        scanProtocolName = null;
        satSelectionsString = null;
        imageHeaderID = null;
        imageHeaderRevisionNumber = null;
        imageHeaderCreatorProcess = null;
        imageCreationDate = null;
        imageCreationTime = null;
        imageNumber = null;
        series = null;
        imageRawDataSystemID = null;
        imageSystemGenerationID = null;
        psdFileName = null;
        prescribedSeriesNumbers = null;
        prescribedImageNumbers = null;
        psdName = null;
        surfaceCoilsCorrectionType = null;
        scSer = null;
        scIma = null;
        pSeries2 = null;
        pImage2 = null;
        contrastAgent = null;
        changedValuesString = null;
        byteBuffer = null;
        orient = null;
        start = null;
        try {
            super.finalize();
        } catch (Throwable er) { }
    }
    
    /**
     * DOCUMENT ME!
     *
     * @return  FileInfoGESigna4X fileInfo
     */
    public FileInfoGESigna4X getFileInfo() {
        return fileInfo;
    }
    
    /**
     * DOCUMENT ME!
     *
     * @return  int image number
     */
    public int getImageNumber() {
        return Integer.valueOf(imageNumber).intValue();
    }
    
    /**
     * 
     * @return
     */
    public String getPatientName() {
        return patientName;
    }
    
    /**
     * DOCUMENT ME!
     *
     * @return  int width
     */
    public int getWidth() {
        return width;
    }
    
    /**
     * DOCUMENT ME!
     *
     * @return  int height
     */
    public int getHeight() {
        return height;
    }
    
    /**
     * 
     * @return
     */
    public float getImageLocation() {
        return imageLocation;
    }
    
    /**
     * 
     * @return
     */
    public float getImgTLHC_R() {
         return imgTLHC_R;
    }
    
    /**
     * 
     * @return
     */
    public float getImgTLHC_A() {
         return imgTLHC_A;
    }
    
    /**
     * 
     * @return
     */
    public float getImgTLHC_S() {
         return imgTLHC_S;
    }
    
    /**
     * 
     * @return
     */
    public float getImgTRHC_R() {
         return imgTRHC_R;
    }
    
    /**
     * 
     * @return
     */
    public float getImgTRHC_A() {
         return imgTRHC_A;
    }
    
    /**
     * 
     * @return
     */
    public float getImgTRHC_S() {
         return imgTRHC_S;
    }
    
    /**
     * 
     * @return
     */
    public float getImgBLHC_R() {
         return imgBLHC_R;
    }
    
    /**
     * 
     * @return
     */
    public float getImgBLHC_A() {
         return imgBLHC_A;
    }
    
    /**
     * 
     * @return
     */
    public float getImgBLHC_S() {
         return imgBLHC_S;
    }

    
    /**
     * Looks for the image header ID "IMAGE HEADER" and number of image header blocks
     * (usually 2 in units of 512 bytes )in GE Signa 4.X file in the File header.
     * If present, the image is GE SIGNA 4.X format.
     *
     * @throws  IOException  Indicates error reading the file
     *
     * @return  boolean true if the image header ID and 2 image header blocks
     * was found in the image header.
     */
    public boolean isGESigna4X() throws IOException {
    	
    	try {

            if (raFile != null) {
                raFile.close();
            }

            fileHeader = new File(fileDir + fileName);
            raFile = new RandomAccessFile(fileHeader, "r");
            
    	} catch (OutOfMemoryError e) {
            MipavUtil.displayError("Out of memory in FileGESigna4X.isGESigna4X.");
            throw new IOException();
        }
    	
    	if (raFile == null) {
            return false;
        }

        if (raFile.length() <= 143336) {
            return false;
        }
//      block 10 - image header
        raFile.seek(10*512 + 2*0);
        // "IMAGE HEADER"
        String imHeaderID = getString(14);
                
        raFile.seek(10*512 + 2*11);
        // Number of image header blocks in units of 512 bytes; value of 2 expected
        short imHeaderBlocks = (short)getSignedShort(endianess);
        
        
        if (imHeaderID.equals("IMAGE HEADER  ") && imHeaderBlocks == 2){
        	return true;
        } else {
        	return false;
        }
        
        
    }
    
    /**
     * reads the Signa 4X file header and data.
     *
     * @param      buffer  DOCUMENT ME!
     *
     * @exception  IOException  if there is an error reading the file
     */
    public void readImage(float[] buffer) throws IOException {

        int i;
        // Block 0 system configuration header
        raFile.seek(0*512 + 2*6);
        systemID = getString(4);
        fileInfo.setSystemID(systemID);
        
        raFile.seek(0*512 + 2*16);
        systemConfigHospitalName = getString(32);
        fileInfo.setSystemConfigHospitalName(systemConfigHospitalName);
        
        // Read in fields from the block 6 study header
        raFile.seek(6*512 + 2*0);
        studyHeaderID = getString(14); // "STUDY HEADER", 14 bytes allocated
        fileInfo.setStudyHeaderID(studyHeaderID);
        
        // 6*512 + 2*7
        // Character string containing the release revision number of the header in
        // the form xx.xx.xx
        studyHeaderRevisionNumber = getString(8);
        fileInfo.setStudyHeaderRevisionNumber(studyHeaderRevisionNumber);
         
        // 6*512 + 2*11
        // Number of study header blocks in units of 512 bytes; Value of 2 expected
        studyHeaderBlocks = (short)getSignedShort(endianess);
        fileInfo.setStudyHeaderBlocks(studyHeaderBlocks);
        
        // 6*512 + 2*12
        // Unique MRI process name(Proc Name: PID)
        MRIProcessName = getString(32);
        fileInfo.setMRIProcessName(MRIProcessName);
        
        // 6*512 + 2*28
        // Unique processes task ID of creator
        studyTaskID = (short)getSignedShort(endianess);
        fileInfo.setStudyTaskID(studyTaskID);
        
        // 6*512 + 2*29
        // Original study number from which the data was produced.
        // Included only if raw data from different study, else null
        rawDataStudyNumber = getString(5);
        fileInfo.setRawDataStudyNumber(rawDataStudyNumber);
        
        raFile.seek(6*512 + 2*32);
        studyNumber = getString(5);
        fileInfo.setStudyNumber(studyNumber);
        
        raFile.seek(6*512 + 2*35);
        // Three digit raw data ID from original study number
        rawDataSystemID = getString(3);
        fileInfo.setRawDataSystemID(rawDataSystemID);
        
        raFile.seek(6*512 + 2*37);
        systemGenerationID = getString(3);
        fileInfo.setSystemGenerationID(systemGenerationID);
        
        raFile.seek(6*512 + 2*39);
        studyDate = getString(9); // (dd-mmm-yy)
        fileInfo.setStudyDate(studyDate);
        
        raFile.seek(6*512 + 2*47);
        studyTime = getString(8); // (hh:mm:ss)
        fileInfo.setStudyTime(studyTime);
        
        raFile.seek(6*512 + 2*54);
        patientName = getString(32);
        fileInfo.setPatientName(patientName);
        
        // 6*512 + 2*70
        patientID = getString(12);
        fileInfo.setPatientID(patientID);
        
        raFile.seek(6*512 + 2*78);
        patientAge = getString(3);
        fileInfo.setPatientAge(patientAge); // Age xxx years or xxD or xxW or
                                            // xxM or xxY
        
        raFile.seek(6*512 + 2*80);
        patientSex = getString(1);
        fileInfo.setPatientSex(patientSex);
        
        raFile.seek(6*512 + 2*81);
        patientWeight = getInt(endianess); // grams
        fileInfo.setPatientWeight(patientWeight);
        
        // 6*512 + 2*83
        referringPhysician = getString(32);
        fileInfo.setReferringPhysician(referringPhysician);
        
        // 6*512 + 2*99
        diognostician = getString(32);
        fileInfo.setDiognostician(diognostician);
        
        // 6*512 + 2*115
        operator = getString(32);
        fileInfo.setOperator(operator);
        
        // 6*512 + 2*131
        studyDescription = getString(60);
        fileInfo.setStudyDescription(studyDescription);
        
        // 6*512 + 2*161
        history = getString(120);
        fileInfo.setHistory(history);
        
        raFile.seek(6*512 + 2*223);
        hospitalName = getString(32);
        fileInfo.setHospitalName(hospitalName);
        
        // 6*512 + 2*239
        patientStatus = (short)getSignedShort(endianess);
        if (patientStatus == 0) {
            fileInfo.setPatientStatus("in-patient");
        }
        else if (patientStatus == 1) {
            fileInfo.setPatientStatus("out-patient");
        }
        else if (patientStatus == 2) {
            fileInfo.setPatientStatus("emergency");
        }
        else if (patientStatus == 3) {
            fileInfo.setPatientStatus("referral");
        }
        else if (patientStatus == 4) {
            fileInfo.setPatientStatus("blank");
        }
        else {
            Preferences.debug("Patient status = " + patientStatus + "\n");
        }
        
        // 6*512 + 2*240
        // Requested number from Scan Rx first page used for patient logging system
        requestedNumber = getString(12);
        fileInfo.setRequestedNumber(requestedNumber);
        
        // Block 8 - series header
        raFile.seek(8*512 + 2*0);
        // "SERIES HEADER"
        seriesHeaderID = getString(14);
        fileInfo.setSeriesHeaderID(seriesHeaderID);
        
        // 8*512 + 2*7
        // Character string containing the release revision number of the header in the
        // form xx.xx.xx
        seriesHeaderRevisionNumber = getString(8);
        fileInfo.setSeriesHeaderRevisionNumber(seriesHeaderRevisionNumber);
        seriesRevisionSubnumber = Integer.valueOf(seriesHeaderRevisionNumber.substring(3,5)).intValue();
        
        // 8*512 + 2*11
        // Number of series header blocks in units of 512 bytes; Value of 2 expected
        seriesHeaderBlocks = (short)getSignedShort(endianess);
        fileInfo.setSeriesHeaderBlocks(seriesHeaderBlocks);
        
        // 8*512 + 2*12
        // Unique MRI process name(Proc Name: PID)
        seriesProcessName = getString(32);
        fileInfo.setSeriesProcessName(seriesProcessName);
        
        // 8*512 + 2*28
        seriesTaskID = (short)getSignedShort(endianess);
        fileInfo.setSeriesTaskID(seriesTaskID);
        
        // 8*512 + 2*29
        // Original data for which raw data was produced
        originalSeriesNumber = getString(3);
        fileInfo.setOriginalSeriesNumber(originalSeriesNumber);
        
        raFile.seek(8*512 + 2*31);
        seriesNumber = getString(3);
        fileInfo.setSeriesNumber(seriesNumber);
        
        raFile.seek(8*512 + 2*33);
        // Raw data system ID from study
        seriesRawDataSystemID = getString(3);
        fileInfo.setSeriesRawDataSystemID(seriesRawDataSystemID);
        
        raFile.seek(8*512 + 2*35);
        // System generation ID
        seriesSystemGenerationID = getString(3);
        fileInfo.setSeriesSystemGenerationID(seriesSystemGenerationID);
        
        raFile.seek(8*512 + 2*37);
        seriesDate = getString(9); // (dd-mmm-yy)
        fileInfo.setSeriesDate(seriesDate);
        
        raFile.seek(8*512 + 2*45);
        seriesTime = getString(8); // (hh:mm:ss)
        fileInfo.setSeriesTime(seriesTime);
        
        raFile.seek(8*512 + 2*52);
        seriesDescription = getString(120);
        fileInfo.setSeriesDescription(seriesDescription);
        
        // 8*512 + 2*112
        seriesType = (short)getSignedShort(endianess);
        if (seriesType == 0) {
            fileInfo.setSeriesType("normal");
        }
        else if (seriesType == 1) {
            fileInfo.setSeriesType("screensave");
        }
        else if (seriesType == 2) {
            fileInfo.setSeriesType("composite");
        }
        else {
            Preferences.debug("seriesType had an illegal value of " + seriesType + "\n");
        }
        
        // 8*512 + 2*113
        coilType = (short)getSignedShort(endianess);
        if (coilType == 0) {
            fileInfo.setCoilType("head");
        }
        else if (coilType == 1) {
            fileInfo.setCoilType("body");
        }
        else if (coilType == 2) {
            fileInfo.setCoilType("surface");
        }
        else {
            Preferences.debug("coilType had an illegal value of " + coilType + "\n");
        }
        
        // 8*512 + 2*114
        coilName = getString(16);
        fileInfo.setCoilName(coilName);
        
        // 8*512 + 2*122
        contrastDescription = getString(32);
        fileInfo.setContrastDescription(contrastDescription);
        
        // 8*512 + 2*138
        planeType = (short)getSignedShort(endianess);
        if (planeType == 0) {
            fileInfo.setImageOrientation(FileInfoBase.AXIAL);
            fileInfo.setPlaneType("axial");
        }
        else if (planeType == 1) {
            fileInfo.setImageOrientation(FileInfoBase.SAGITTAL);
            fileInfo.setPlaneType("sagittal");
        }
        else if (planeType == 2) {
            fileInfo.setImageOrientation(FileInfoBase.CORONAL);
            fileInfo.setPlaneType("coronal");
        }
        else if (planeType == 3) {
            fileInfo.setImageOrientation(FileInfoBase.UNKNOWN_ORIENT);
            fileInfo.setPlaneType("oblique");
        }
        else if (planeType == 4) {
            fileInfo.setImageOrientation(FileInfoBase.UNKNOWN_ORIENT);
            fileInfo.setPlaneType("screen save");
        }
        else {
            Preferences.debug("Plane type had an illegal value of " + planeType + "\n");
            fileInfo.setImageOrientation(FileInfoBase.UNKNOWN_ORIENT);
        }
        
        // 8*512 + 2*139
        planeName = getString(16);
        fileInfo.setPlaneName(planeName);
        
        raFile.seek(8*512 + 2*147);
        imageMode = (short)getSignedShort(endianess);
        if (imageMode == 0) {
            fileInfo.setImageMode("2D single");
        }
        else if (imageMode == 1) {
            fileInfo.setImageMode("2D multiple");
        }
        else if (imageMode == 2) {
            fileInfo.setImageMode("3D volume");
        }
        else if (imageMode == 3) {
            fileInfo.setImageMode("cine");
        }
        else if (imageMode == 4) {
            fileInfo.setImageMode("spectroscopy");
        }
        else {
            Preferences.debug("imageMode had an illegal value of " + imageMode + "\n");
        }
        
        // 8*512 + 2*148
        fieldStrength = (short)getSignedShort(endianess); // gauss
        fileInfo.setFieldStrength(fieldStrength);
        
        // 8*512 + 2*149
        pulseSequence = (short)getSignedShort(endianess);
        if (pulseSequence == 0) {
            fileInfo.setPulseSequence("memp");
        }
        else if (pulseSequence == 1) {
            fileInfo.setPulseSequence("ir");
        }
        else if (pulseSequence == 2) {
            fileInfo.setPulseSequence("ps");
        }
        else if (pulseSequence == 3) {
            fileInfo.setPulseSequence("rm");
        }
        else if (pulseSequence == 4) {
            fileInfo.setPulseSequence("rmge");
        }
        else if (pulseSequence == 5) {
            fileInfo.setPulseSequence("gre");
        }
        else if (pulseSequence == 6) {
            fileInfo.setPulseSequence("vemp");
        }
        else if (pulseSequence == 7) {
            fileInfo.setPulseSequence("mpgr");
        }
        else if (pulseSequence == 8) {
            fileInfo.setPulseSequence("mpgrv");
        }
        else if (pulseSequence == 9) {
            fileInfo.setPulseSequence("mpirs");
        }
        else if (pulseSequence == 10) {
            fileInfo.setPulseSequence("mpiri");
        }
        else if (pulseSequence == 11) {
            fileInfo.setPulseSequence("3d/gre");
        }
        else if (pulseSequence == 12) {
            fileInfo.setPulseSequence("cine/gre");
        }
        else if (pulseSequence == 13) {
            fileInfo.setPulseSequence("spgr");
        }
        else if (pulseSequence == 14) {
            fileInfo.setPulseSequence("sspf");
        }
        else if (pulseSequence == 15) {
            fileInfo.setPulseSequence("cin/spgr");
        }
        else if (pulseSequence == 16) {
            fileInfo.setPulseSequence("3d/spgr");
        }
        else if (pulseSequence == 17) {
            fileInfo.setPulseSequence("fse");
        }
        else if (pulseSequence == 18) {
            fileInfo.setPulseSequence("fve");
        }
        else if (pulseSequence == 19) {
            fileInfo.setPulseSequence("fspgr");
        }
        else if (pulseSequence == 20) {
            fileInfo.setPulseSequence("fgr");
        }
        else if (pulseSequence == 21) {
            fileInfo.setPulseSequence("fmpspgr");
        }
        else if (pulseSequence == 22) {
            fileInfo.setPulseSequence("fmpgr");
        }
        else if (pulseSequence == 23) {
            fileInfo.setPulseSequence("fmpir");
        }
        else if (pulseSequence == 24) {
            fileInfo.setPulseSequence("probe.s");
        }
        else if (pulseSequence == 25) {
            fileInfo.setPulseSequence("probe.p");
        }
        else {
            Preferences.debug("pulseSequence had an illegal value of " + 
                               pulseSequence + "\n");
        }
        
        // 8*512 + 2*150
        pulseSequenceSubtype = (short)getSignedShort(endianess);
        if (pulseSequenceSubtype == 0) {
            fileInfo.setPulseSequenceSubtype("chopper");
        }
        else {
            Preferences.debug("pulseSequenceSubtype had an illegal value of " +
                               pulseSequenceSubtype + "\n");
        }
        
        // 8*512 + 2*151
        fieldOfView = getFloat(endianess); // mm
        fileInfo.setFieldOfView(fieldOfView);
        // Multiply by a kludge factor of 2 for the right answer
        fileInfo.setResolutions(fieldOfView*2/width, 0);
        fileInfo.setResolutions(fieldOfView*2/height, 1);
        
        // Centers are relative to patient landmark
        // 8*512 + 2*153
        rlCenter = getFloat(endianess); // R+L-  MIPAV is R to L
        fileInfo.setRLCenter(rlCenter);
        
        // 8*512 + 2*155
        apCenter = getFloat(endianess); // A+P-  MIPAV is A to P
        fileInfo.setAPCenter(apCenter);
        
        // 8*512 + 2*157
        siCenter = getFloat(endianess); // S+I-  MIPAV is I to S
        fileInfo.setSICenter(siCenter);
        
        // 8*512 + 2*159
        orientation = (short)getSignedShort(endianess);
        if (orientation == 0) {
            fileInfo.setOrientation("supine");
        }
        else if (orientation == 1) {
            fileInfo.setOrientation("prone");
        }
        else if (orientation == 2) {
            fileInfo.setOrientation("decubitus left");
        }
        else if (orientation == 3) {
            fileInfo.setOrientation("decubitus right");
        }
        else {
            Preferences.debug("orientation had an illegal value of " +
                               orientation + "\n");
        }
        
        // 8*512 + 2*160
        position = (short)getSignedShort(endianess);
        if (position == 0) {
            fileInfo.setPosition("head first");
        }
        else if (position == 1) {
            fileInfo.setPosition("feet first");
        }
        else {
            Preferences.debug("position had an illegal value of " +
                               position + "\n");
        }
        
        // 8*512 + 2*161
        // Character string contains whole name in plasma button. 'Naision, 'Sternal',
        // 'Xyphoid', 'Iliac', 'Sym-Pub', or other entered name.
        longitudinalAnatomicalReference = getString(32);
        fileInfo.setLongitudinalAnatomicalReference(longitudinalAnatomicalReference);
        
        // 8*512 + 2*177
        // Obsolete  Same format as longitudinalAnatomicalReference
        // 'TNTIP_OF_NOSE', BTBIG-TOE', NBNAUGHTY-BITS', 'EMEXTERNAL-MEATUS', 'MAMID_AXIS'
        verticalAnatomicalReference = getString(32);
        fileInfo.setVerticalAnatomicalReference(verticalAnatomicalReference);
        
        // 8*512 + 2*193
        // Relative to table top (Y) in millimeters
        verticalLandmark = getFloat(endianess);
        fileInfo.setVerticalLandmark(verticalLandmark);
        
        // 8*512 + 2*195
        // Relative to table center (X) in millimeters
        horizontalLandmark = getFloat(endianess);
        fileInfo.setHorizontalLandmark(horizontalLandmark);
        
        // 8*512 + 2*197
        // Physcial table location relative to home
        tableLocation = getFloat(endianess);
        fileInfo.setTableLocation(tableLocation);
        
        // 8*512 + 2*199
        scanMatrixX = (short)getSignedShort(endianess);
        fileInfo.setScanMatrixX(scanMatrixX);
        
        // 8*512 + 2*200
        scanMatrixY = (short)getSignedShort(endianess);
        fileInfo.setScanMatrixY(scanMatrixY);
        
        // 8*512 + 2*201
        // 256 or 512
        imageMatrix = (short)getSignedShort(endianess);
        fileInfo.setImageMatrix(imageMatrix);
        
        // 8*512 + 2*202
        // 1 to 512.  Number of images allocated when the series was originally created.
        imagesAllocated = (short)getSignedShort(endianess);
        fileInfo.setImagesAllocated(imagesAllocated);
        
        // 8*512 + 2*203
        gatingType = (short)getSignedShort(endianess);
        gatingTypeString = new String("none");
        if ((gatingType & 1) != 0) {
            gatingTypeString = gatingTypeString.concat("_ECG");
        }
        if ((gatingType & 2) != 0) {
            gatingTypeString = gatingTypeString.concat("_RESP");
        }
        if ((gatingType & 4) != 0) {
            gatingTypeString = gatingTypeString.concat("_RCOMP");
        }
        if ((gatingType & 8) != 0) {
            gatingTypeString = gatingTypeString.concat("_FC");
        }
        if ((gatingType & 16) != 0) {
            gatingTypeString = gatingTypeString.concat("_CI");
        }
        if ((gatingType & 32) != 0) {
            gatingTypeString = gatingTypeString.concat("_St");
        }
        if ((gatingType & 64) != 0) {
            gatingTypeString = gatingTypeString.concat("_PG");
        }
        if ((gatingType & 128) != 0) {
            gatingTypeString = gatingTypeString.concat("_NP");
        }
        if ((gatingType & 256) != 0) {
            gatingTypeString = gatingTypeString.concat("_RF");
        }
        if ((gatingType & 512) != 0) {
            gatingTypeString = gatingTypeString.concat("_Rt");
        }
        if ((gatingType & 1024) != 0) {
            gatingTypeString = gatingTypeString.concat("_VB");
        }
        if ((gatingType & 2048) != 0) {
            gatingTypeString = gatingTypeString.concat("_ED");
        }
        if ((gatingType & 4096) != 0) {
            gatingTypeString = gatingTypeString.concat("_PM");
        }
        if (gatingType < 0) {
            gatingTypeString = gatingTypeString.concat("_MP");
        }
        if (gatingType != 0) {
            i = gatingTypeString.indexOf("_");
            gatingTypeString = gatingTypeString.substring(i+1);
        }
        fileInfo.setGatingType(gatingTypeString);
        
        // 8*512 + 2*204
        pulseSequenceMode = (short)getSignedShort(endianess);
        if (pulseSequenceMode == 0) {
            fileInfo.setPulseSequenceMode("PRD");
        }
        else if (pulseSequenceMode == 1) {
            fileInfo.setPulseSequenceMode("RM");
        }
        else if (pulseSequenceMode == 2) {
            fileInfo.setPulseSequenceMode("RMGE");
        }
        else {
            Preferences.debug("Pulse sequence mode = " + pulseSequenceMode + "\n");
        }
        
        // 8*512 + 2*205
        seriesPSDName = getString(12);
        fileInfo.setSeriesPSDName(seriesPSDName);
        
        // 8*512 + 2*211
        landmarkCounter = getInt(endianess);
        fileInfo.setLandmarkCounter(landmarkCounter);
        
        // 8*512 + 2*213
        scanProtocolName = getString(20);
        fileInfo.setScanProtocolName(scanProtocolName);
        
        // 8*512 + 2*223
        surfaceCoilType = (short)getSignedShort(endianess);
        if (surfaceCoilType == 0) {
            fileInfo.setSurfaceCoilType("receive");
        }
        else if (surfaceCoilType == 1) {
            fileInfo.setSurfaceCoilType("transmit/receive");
        }
        else {
            Preferences.debug("Surface coil type = " + surfaceCoilType + "\n");
        }
        
        // 8*512 + 2*224
        suppressionTechnique = (short)getSignedShort(endianess);
        if (suppressionTechnique == 0) {
            fileInfo.setSuppressionTechnique("none");
        }
        else if (suppressionTechnique == 1) {
            fileInfo.setSuppressionTechnique("fat");
        }
        else if (suppressionTechnique == 2) {
            fileInfo.setSuppressionTechnique("water");
        }
        else {
            Preferences.debug("Suppresion technique = " + suppressionTechnique + "\n");
        }
        
        // 8*512 + 2*225
        satSelections = (short)getSignedShort(endianess);
        satSelectionsString = new String("none");
        if ((satSelections & 1) != 0) {
            satSelectionsString = satSelectionsString.concat("_S");
        }
        if ((satSelections & 2) != 0) {
            satSelectionsString = satSelectionsString.concat("_I");
        }
        if ((satSelections & 4) != 0) {
            satSelectionsString = satSelectionsString.concat("_R");
        }
        if ((satSelections & 8) != 0) {
            satSelectionsString = satSelectionsString.concat("_L");
        }
        if ((satSelections & 16) != 0) {
            satSelectionsString = satSelectionsString.concat("_A");
        }
        if ((satSelections & 32) != 0) {
            satSelectionsString = satSelectionsString.concat("_P");
        }
        if ((satSelections & 64) != 0) {
            satSelectionsString = satSelectionsString.concat("_s");
        }
        if ((satSelections & 128) != 0) {
            satSelectionsString = satSelectionsString.concat("_i");
        }
        if ((satSelections & 256) != 0) {
            satSelectionsString = satSelectionsString.concat("_r");
        }
        if ((satSelections & 512) != 0) {
            satSelectionsString = satSelectionsString.concat("_l");
        }
        if ((satSelections & 1024) != 0) {
            satSelectionsString = satSelectionsString.concat("_a");
        }
        if ((satSelections & 2048) != 0) {
            satSelectionsString = satSelectionsString.concat("_p");
        }
        if (satSelections != 0) {
            i = satSelectionsString.indexOf("_");
            satSelectionsString = satSelectionsString.substring(i+1);
        }
        fileInfo.setSATSelections(satSelectionsString);
        
        // 8*512 + 2*226
        surfaceCoilIntensityCorrection = (short)getSignedShort(endianess);
        if (surfaceCoilIntensityCorrection == 0) {
            fileInfo.setSurfaceCoilIntensityCorrection("off");
        }
        else if (surfaceCoilIntensityCorrection == 1) {
            fileInfo.setSurfaceCoilIntensityCorrection("on");
        }
        else {
            Preferences.debug("Surface coil intensity correction = " + surfaceCoilIntensityCorrection + "\n");
        }
        
        // 8*512 + 2*227
        // R-side SAT pulse location relative to landmark in millimeters
        satXLoc1 = (short)getSignedShort(endianess);
        fileInfo.setSATXLoc1(satXLoc1);
        
        // 8*512 + 2*228
        // L-side SAT pulse location relative to landmark in millimeters
        satXLoc2 = (short)getSignedShort(endianess);
        fileInfo.setSATXLoc2(satXLoc2);
        
        // 8*512 + 2*229
        // A-side SAT pulse location relative to landmark in millimeters
        satYLoc1 = (short)getSignedShort(endianess);
        fileInfo.setSATYLoc1(satYLoc1);
        
        // 8*512 + 2*230
        // P-side SAT pulse location relative to landmark in millimeters
        satYLoc2 = (short)getSignedShort(endianess);
        fileInfo.setSATYLoc2(satYLoc2);
        
        // 8*512 + 2*231
        // S-side SAT pulse location relative to landmark in millimeters
        satZLoc1 = (short)getSignedShort(endianess);
        fileInfo.setSATZLoc1(satZLoc1);
        
        // 8*512 + 2*232
        // I-side SAT pulse location relative to landmark in millimeters
        satZLoc2 = (short)getSignedShort(endianess);
        fileInfo.setSATZLoc2(satZLoc2);
        
        // 8*512 + 2*233
        // Thickness of X-axis SAT pulses in millimeters
        satXThick = (short)getSignedShort(endianess);
        fileInfo.setSATXThick(satXThick);
        
        // 8*512 + 2*234
        // Thickness of Y-axis SAT pulses in millimeters
        satYThick = (short)getSignedShort(endianess);
        fileInfo.setSATYThick(satYThick);
        
        // 8*512 + 2*235
        // Thickness of Z-axis SAT pulses in millimeters
        satZThick = (short)getSignedShort(endianess);
        fileInfo.setSATZThick(satZThick);
        
        // 8*512 + 2*236
        // TOF/PC image
        vasMode = (short)getSignedShort(endianess);
        if (vasMode == 0) {
            fileInfo.setVasMode("none");
        }
        else if (vasMode == 1) {
            fileInfo.setVasMode("TOF");
        }
        else if (vasMode == 2) {
            fileInfo.setVasMode("PC");
        }
        else {
            Preferences.debug("vasMode = " + vasMode + "\n");
        }
        
        // 8*512 + 2*237
        phaseContrastFlowAxis = (short)getSignedShort(endianess);
        if (phaseContrastFlowAxis == 0) {
            fileInfo.setPhaseContrastFlowAxis("none");
        }
        else if (phaseContrastFlowAxis == 1) {
            fileInfo.setPhaseContrastFlowAxis("S/I");
        }
        else if (phaseContrastFlowAxis == 2) {
            fileInfo.setPhaseContrastFlowAxis("R/L");
        }
        else if (phaseContrastFlowAxis == 4) {
            fileInfo.setPhaseContrastFlowAxis("A/P");
        }
        else if (phaseContrastFlowAxis == 7) {
            fileInfo.setPhaseContrastFlowAxis("all");
        }
        else if (phaseContrastFlowAxis == 8) {
            fileInfo.setPhaseContrastFlowAxis("slice");
        }
        else {
            Preferences.debug("Phase contrast flow axis = " + phaseContrastFlowAxis + "\n");
        }
        
        if (seriesRevisionSubnumber >= 7) {
            raFile.seek(8*512 + 2*239);
            gatingType2 = (short)getSignedShort(endianess);
            if (gatingType2 == 0) {
                fileInfo.setGatingType2("None");
            }
            else if (gatingType2 == 1) {
                fileInfo.setGatingType2("IR");
            }
            else if (gatingType2 == 2) {
                fileInfo.setGatingType2("DE");
            }
            else if (gatingType2 == 3) {
                fileInfo.setGatingType2("IR_DE");
            }
            else {
                Preferences.debug("gatingType2 = " + gatingType2 + "\n");
            }
        } // if (seriesRevisionSubnumber >= 7)
        
        // block 10 - image header
        raFile.seek(10*512 + 2*0);
        // "IMAGE HEADER"
        imageHeaderID = getString(14);
        fileInfo.setImageHeaderID(imageHeaderID);
        
        // 10*512 + 2*7
        // Character string containing the revision number of the string in the
        // form xx.xx.xx
        imageHeaderRevisionNumber = getString(8);
        fileInfo.setImageHeaderRevisionNumber(imageHeaderRevisionNumber);
        imageRevisionSubnumber = Integer.valueOf(imageHeaderRevisionNumber.substring(3,5)).intValue();
        
        // 10*512 + 2*11
        // Number of image header blocks in units of 512 bytes; value of 2 expected
        imageHeaderBlocks = (short)getSignedShort(endianess);
        fileInfo.setImageHeaderBlocks(imageHeaderBlocks);
        
        // 10*512 + 2*12
        // Unique MRI process ID
        imageHeaderCreatorProcess = getString(32);
        fileInfo.setImageHeaderCreatorProcess(imageHeaderCreatorProcess);
        
        // 10*512 + 2*28
        // Task ID of creating task in process
        imageHeaderCreatorTask = (short)getSignedShort(endianess);
        fileInfo.setImageHeaderCreatorTask(imageHeaderCreatorTask);
        
        // 10*512 + 2*29
        imageCreationDate = getString(9); // (dd-mmm-yy)
        fileInfo.setImageCreationDate(imageCreationDate);
        
        raFile.seek(10*512 + 2*37);
        imageCreationTime = getString(8); // (hh:mm:ss)
        fileInfo.setImageCreationTime(imageCreationTime);
        
        raFile.seek(10*512 + 2*44);
        imageNumber = getString(3);
        fileInfo.setImageNumber(imageNumber);
        
        raFile.seek(10*512 + 2*46);
        // Image series number
        series = getString(3);
        fileInfo.setSeries(series);
        
        raFile.seek(10*512 + 2*48);
        imageRawDataSystemID = getString(3);
        fileInfo.setImageRawDataSystemID(imageRawDataSystemID);
        
        raFile.seek(10*512 + 2*50);
        imageSystemGenerationID = getString(3);
        fileInfo.setImageSystemGenerationID(imageSystemGenerationID);
        
        // The next six entries startX to endZ define the starting and ending
        // location for the current scan sequence.  Depending on the plane
        // type, some information can also be calculated from the field of view
        // and its center coordinates
        
        raFile.seek(10*512 + 2*52);
        // start location X, right minimum
        startX = getFloat(endianess);
        fileInfo.setStartX(startX);
        
        // 10*512 + 2*54
        // end location X, right maximum
        endX = getFloat(endianess);
        fileInfo.setEndX(endX);
        
        // 10*512 + 2*56
        // start location Y, anterior minimum
        startY = getFloat(endianess);
        fileInfo.setStartY(startY);
        
        // 10*512 + 2*58
        // end location Y, anterior maximum
        endY = getFloat(endianess);
        fileInfo.setEndY(endY);
        
        // 10*512 + 2*60
        // start location Z, superior minimum
        startZ = getFloat(endianess);
        fileInfo.setStartZ(startZ);
        
        // 10*512 + 2*62
        // End location Z, superior maximum
        endZ = getFloat(endianess);
        fileInfo.setEndZ(endZ);
        
        raFile.seek(10*512 + 2*73);
        // image location relative to landmark
        imageLocation = getFloat(endianess);
        fileInfo.setImageLocation(imageLocation);
        
        // 10*512 + 2*75
        tablePosition = getFloat(endianess);
        fileInfo.setTablePosition(tablePosition);
        
        // 10*512 + 2*77
        // image thickness
        thickness = getFloat(endianess);  // In practice 10*res[0] = 10*res[1]
        fileInfo.setThickness(thickness);
        
        // 10*512 + 2*79
        // spacing between slices
        imageSpacing = getFloat(endianess);
        if (imageSpacing != 0.0f) {
            fileInfo.setResolutions(imageSpacing, 2);
        }
        fileInfo.setImageSpacing(imageSpacing);
        
        // 10*512 + 2*81
        round = (short)getSignedShort(endianess);
        if (round != 0) {
            fileInfo.setRound("Round to nearest slice");
        }
        else {
            fileInfo.setRound("Round to zero");
        }
        
        raFile.seek(10*512 + 2*82);
        tr = getFloat(endianess); // repetition/recovery time usec
        fileInfo.setTR(tr);
        
        // 10*512 + 2*84
        ts = getFloat(endianess); // scan time usec
        fileInfo.setTS(ts);
        
        raFile.seek(10*512 + 2*86);
        te = getFloat(endianess); //  Echo delay usec
        fileInfo.setTE(te);
        
        // 10*512 + 2*88
        ti = getFloat(endianess); // Inversion Recovery time usec
        fileInfo.setTI(ti);
        
        raFile.seek(10*512 + 2*98);
        // Total number of echos
        numberOfEchos = (short)getSignedShort(endianess);
        fileInfo.setNumberOfEchos(numberOfEchos);
        
        // 10*512 + 2*99
        // Echo number of current image
        echoNumber = (short)getSignedShort(endianess);
        fileInfo.setEchoNumber(echoNumber);
        
        // 19*512 + 2*100
        // Number of slices in this scan group
        sliceQuantity = (short)getSignedShort(endianess);
        fileInfo.setSliceQuantity(sliceQuantity);
        
        // 10*512 + 2*101
        averagesNumber = (short)getSignedShort(endianess); // Number of averages
        fileInfo.setAveragesNumber(averagesNumber);
       
        // 10*512 + 2*102
        researchMode = (short)getSignedShort(endianess);
        if (researchMode != 0) {
            fileInfo.setResearchMode("Research mode used");
        }
        else {
            fileInfo.setResearchMode("Research mode not used");
        }
        
        // 10*512 + 2*103
        psdFileName = getString(32);
        fileInfo.setPSDFileName(psdFileName);
        
        // 10*512 + 2*119
        psdDay = (short)getSignedShort(endianess);
        fileInfo.setPSDDay(psdDay);
        
        // 10*512 + 2*120
        psdMonth = (short)getSignedShort(endianess);
        fileInfo.setPSDMonth(psdMonth);
        
        // 10*512 + 2*121
        // Year - 1900
        psdYear = (short)getSignedShort(endianess);
        fileInfo.setPSDYear(psdYear);
        
        // 10*512 + 2*122
        psdHour = (short)getSignedShort(endianess);
        fileInfo.setPSDHour(psdHour);
        
        // 10*512 + 2*123
        psdMinute = (short)getSignedShort(endianess);
        fileInfo.setPSDMinute(psdMinute);
        
        // 10*512 + 2*124
        psdSeconds = (short)getSignedShort(endianess);
        fileInfo.setPSDSeconds(psdSeconds);
        
        // 10*512 + 2*125
        graphicallyPrescribed = (short)getSignedShort(endianess);
        if (graphicallyPrescribed != 0) {
            fileInfo.setGraphicallyPrescribed("Graphically prescribed");
        }
        else {
            fileInfo.setGraphicallyPrescribed("Not graphically prescribed");
        }
        
        if (graphicallyPrescribed != 0) {
            // 10*512 + 2*126
            prescribedSeriesNumbers = getString(9);
            fileInfo.setPrescribedSeriesNumbers(prescribedSeriesNumbers);
            
            raFile.seek(10*512 + 2*131);
            prescribedImageNumbers = getString(9);
            fileInfo.setPrescribedImageNumbers(prescribedImageNumbers);
        } // if (graphicallyPrescribed != 0)
        
        raFile.seek(10*512 + 2*136);
        imageShape = (short)getSignedShort(endianess);
        if (imageShape == 0) {
            fileInfo.setImageShape("box");
        }
        else if (imageShape == 1) {
            fileInfo.setImageShape("ellipse");
        }
        else {
            Preferences.debug("Image shape = " + imageShape + "\n");
        }
        
        raFile.seek(10*512 + 2*139);
        pixelSize = getFloat(endianess); // In practice 5*res[2]
        fileInfo.setPixelSize(pixelSize); // millimeters
        fileInfo.setResolutions(pixelSize/5.0f, 2);
        
        raFile.seek(10*512 + 2*143);
        defaultWindow = (short)getSignedShort(endianess);
        fileInfo.setDefaultWindow(defaultWindow);
        
        // 10*512 + 2*144
        defaultLevel = (short)getSignedShort(endianess);
        fileInfo.setDefaultLevel(defaultLevel);
        
        // 10*512 + 2*145
        // Number of blocks in file including headers; value of 28 + 256 = 284 expected
        fileBlocks = (short)getSignedShort(endianess);
        fileInfo.setFileBlocks(fileBlocks);
        
        // 10*512 + 2*146
        excitationsNumber = getFloat(endianess); // Number of excitations
        fileInfo.setExcitationsNumber(excitationsNumber);
        
        // 10*512 + 2*148
        // watts/kg
        peakSAR = getFloat(endianess);
        fileInfo.setPeakSAR(peakSAR);
        
        // 10*512 + 2*150
        // watts/kg
        averageSAR = getFloat(endianess);
        fileInfo.setAverageSAR(averageSAR);
        
        // 10*512 + 2*152
        SARMonitored = (short)getSignedShort(endianess);
        if (SARMonitored != 0) {
            fileInfo.setSARMonitored("SAR monitored");
        }
        else {
            fileInfo.setSARMonitored("SAR not monitored");
        }
        
        // 10*512 + 2*153
        contiguousSlices = (short)getSignedShort(endianess);
        if (contiguousSlices != 0) {
            fileInfo.setContiguousSlices("Slices are selected");
        }
        else {
            fileInfo.setContiguousSlices("Slices are not selected");
        }
        
        // 10*512 + 2*154
        // Heart rate taken from cardiac monitor
        cardiacHeartRate = (short)getSignedShort(endianess);
        fileInfo.setCardiacHeartRate(cardiacHeartRate);
        
        // 10*512 + 2*155
        // Time in milliseconds between QRS signal trigger (peak) and the first
        // excitation pulse
        totalPostTriggerDelayTime = getFloat(endianess);
        fileInfo.setTotalPostTriggerDelayTime(totalPostTriggerDelayTime);
        
        // 10*512 + 2*157
        // Percentage of average R-R interval in which excitation pulse
        // trigger is recognized
        arrythmiaRejectionRatio = (short)getSignedShort(endianess);
        fileInfo.setArrythmiaRejectionRatio(arrythmiaRejectionRatio);
        
        // 10*512 + 2*158
        // Frequency of data acquisition with respect to cardiac rate.
        // 1 - Pulse every beat, 2 - Pulse every other beat, etc.
        cardiacRepTime = (short)getSignedShort(endianess);
        if (cardiacRepTime == 1) {
            fileInfo.setCardiacRepTime("Frequency of data acquisition with respect to cardiac rate is every beat");
        }
        else if (cardiacRepTime > 1) {
            fileInfo.setCardiacRepTime("Frequency of data acquisition with respect to cardiac rate is every " +
                                        cardiacRepTime + " beats");
        }
        else {
            Preferences.debug("cardiacRepTime = " + cardiacRepTime + "\n");
        }
        
        // 10*512 + 2*159
        // Number of images to be acquired after initial trigger
        // Single scan only!
        imagesPerCardiacCycle = (short)getSignedShort(endianess);
        fileInfo.setImagesPerCardiacCycle(imagesPerCardiacCycle);
        
        // 10*512 + 2*160
        // Number of R-R intervals during scan
        scanARRs = getInt(endianess);
        fileInfo.setScanARRs(scanARRs);
        
        // 10*512 + 2*162
        // 1/10 DB in range 0-319.
        transmitAttenuatorSetting = (short)getSignedShort(endianess);
        fileInfo.setTransmitAttenuatorSetting(transmitAttenuatorSetting);
        
        // 10*512 + 2*163
        // 1/10 DB in range 0-629
        receiveAttenuatorSetting = (short)getSignedShort(endianess);
        fileInfo.setReceiveAttenuatorSetting(receiveAttenuatorSetting);
        
        // 10*512 + 2*164
        // Magnetic field strength in 10 microgauss
        imageFieldStrength = getInt(endianess);
        fileInfo.setImageFieldStrength(imageFieldStrength);
        
        // 10*512 + 2*166
        // Frequency/phase offset for image [-256...256]
        imageOffset = (short)getSignedShort(endianess);
        fileInfo.setImageOffset(imageOffset);
        
        // 10*512 + 2*167
        // Time in milliseconds between excitiation pulses within the R-R interval
        interImageDelay = getFloat(endianess);
        fileInfo.setInterImageDelay(interImageDelay);
        
        // 10*512 + 2*169
        psdName = getString(12);
        fileInfo.setPSDName(psdName);
        
        // 10*512 + 2*175
        // Flip angle for GRASS in degrees
        flipAngle = (short)getSignedShort(endianess);
        fileInfo.setFlipAngle(flipAngle);
        
        // 10*512 + 2*176
        surfaceCoilsCorrectionType = getString(4);
        fileInfo.setSurfaceCoilsCorrectionType(surfaceCoilsCorrectionType);
        
        // 10*512 + 2*178
        // Series number of corrected/uncorrected image
        scSer = getString(3);
        fileInfo.setScSer(scSer);
        
        raFile.seek(10*512 + 2*180);
        // Image number of corrected/uncorrected image
        scIma = getString(3);
        fileInfo.setScIma(scIma);
        
        raFile.seek(10*512 + 2*182);
        extremityCoil = (short)getSignedShort(endianess);
        if (extremityCoil != 0) {
            fileInfo.setExtremityCoil("Extremity coil present");
        }
        else {
            fileInfo.setExtremityCoil("Extremity coil not present");
        }
        
        raFile.seek(10*512 + 2*193);
        // Series number of second localizer for double oblique GRx.
        pSeries2 = getString(3);
        fileInfo.setPSeries2(pSeries2);
        
        raFile.seek(10*512 + 2*195);
        // Image number of second localizer for double oblique GRx.
        pImage2 = getString(3);
        fileInfo.setPImage2(pImage2);
        
        raFile.seek(10*512 + 2*197);
        // R center coordinate on plane image in millimeters
        rCenter = getFloat(endianess);
        fileInfo.setRCenter(rCenter);
        
        // 10*512 + 2*199
        // A center coordinate on plane image in millimeters
        aCenter = getFloat(endianess);
        fileInfo.setACenter(aCenter);
        
        // 10*512 + 2*201
        // S center coordinate on plane image in millimeters
        sCenter = getFloat(endianess);
        fileInfo.setSCenter(sCenter);
        
        // 10*512 + 2*203
        // Unit vector used in Graphic Rx and Display
        rNormal = getFloat(endianess);
        fileInfo.setRNormal(rNormal);
        
        // 10*512 + 2*205
        // Unit vector used in Graphic Rx and Display
        aNormal = getFloat(endianess);
        fileInfo.setANormal(aNormal);
        
        // 10*512 + 2*207
        // Unit vector used in Graphic Rx and Display
        sNormal = getFloat(endianess);
        fileInfo.setSNormal(sNormal);
        
        // The next 9 fields define the 3 corners of the image.  
        // Used in display features.  In millimeters.
        
        // 10*512 + 2*209
        imgTLHC_R = getFloat(endianess);
        fileInfo.setImgTLHC_R(imgTLHC_R);
        
        // 10*512 + 2*211
        imgTLHC_A = getFloat(endianess);
        fileInfo.setImgTLHC_A(imgTLHC_A);
        
        // 10*512 + 2*213
        imgTLHC_S = getFloat(endianess);
        fileInfo.setImgTLHC_S(imgTLHC_S);
        
        // 10*512 + 2*215
        imgTRHC_R = getFloat(endianess);
        fileInfo.setImgTRHC_R(imgTRHC_R);
        
        // 10*512 + 2*217
        imgTRHC_A = getFloat(endianess);
        fileInfo.setImgTRHC_A(imgTRHC_A);
        
        // 10*512 + 2*219
        imgTRHC_S = getFloat(endianess);
        fileInfo.setImgTRHC_S(imgTRHC_S);
        
        // 10*512 + 2*221
        imgBLHC_R = getFloat(endianess);
        fileInfo.setImgBLHC_R(imgBLHC_R);
        
        // 10*512 + 2*223
        imgBLHC_A = getFloat(endianess);
        fileInfo.setImgBLHC_A(imgBLHC_A);
        
        // 10*512 + 2*225
        imgBLHC_S = getFloat(endianess);
        fileInfo.setImgBLHC_S(imgBLHC_S);
        
        if (fileInfo.imageOrientation == FileInfoBase.CORONAL) {
            start[0] = -imgTLHC_R;
            start[1] = imgTLHC_S;
            start[2] = -imgTLHC_A;

            if (imgTLHC_R > imgTRHC_R) {
                orient[0] = FileInfoBase.ORI_R2L_TYPE;
            } else {
                orient[0] = FileInfoBase.ORI_L2R_TYPE;
            }

            if (imgTLHC_S > imgBLHC_S) {
                orient[1] = FileInfoBase.ORI_S2I_TYPE;
            } else {
                orient[1] = FileInfoBase.ORI_I2S_TYPE;
            }
            resX = Math.abs(imgTLHC_R - imgTRHC_R)/(width-1);
            resY = Math.abs(imgTLHC_S = imgBLHC_S)/(height-1);
        } else if (fileInfo.imageOrientation == FileInfoBase.SAGITTAL) {
            start[0] = -imgTLHC_A;
            start[1] = imgTLHC_S;
            start[2] = -imgTLHC_R;

            if (imgTLHC_A > imgTRHC_A) {
                orient[0] = FileInfoBase.ORI_A2P_TYPE;
            } else {
                orient[0] = FileInfoBase.ORI_P2A_TYPE;
            }

            if (imgTLHC_S > imgBLHC_S) {
                orient[1] = FileInfoBase.ORI_S2I_TYPE;
            } else {
                orient[1] = FileInfoBase.ORI_I2S_TYPE;
            }
            resX = Math.abs(imgTLHC_A - imgTRHC_A)/(width-1);
            resY = Math.abs(imgTLHC_S - imgBLHC_S)/(height-1);
        } else { // AXIAL
            start[0] = -imgTLHC_R;
            start[1] = -imgTLHC_A;
            start[2] = imgTLHC_S;

            if (imgTLHC_R > imgTRHC_R) {
                orient[0] = FileInfoBase.ORI_R2L_TYPE;
            } else {
                orient[0] = FileInfoBase.ORI_L2R_TYPE;
            }

            if (imgTLHC_A > imgBLHC_A) {
                orient[1] = FileInfoBase.ORI_A2P_TYPE;
            } else {
                orient[1] = FileInfoBase.ORI_P2A_TYPE;
            }
            resX = Math.abs(imgTLHC_R - imgTRHC_R)/(width-1);
            resY = Math.abs(imgTLHC_A - imgBLHC_A)/(height-1);
        }

        // orient[2] is calculated in FileIo.java by comparing position values
        // of the top left hand corner between slice 0 and slice 1.
        // Must multiply resX and resY by kludge factors of 2 for the right answers.
        fileInfo.setAxisOrientation(orient);
        fileInfo.setOrigin(start);
        
        // 10*512 + 2*227
        imageHeaderDisclaimer = (short)getSignedShort(endianess);
        fileInfo.setImageHeaderDisclaimer(imageHeaderDisclaimer);
        
        // 10*512 + 2*228
        // Minimum delay after cardiac trigger in milliseconds
        minimumDelay = (short)getSignedShort(endianess);
        fileInfo.setMinimumDelay(minimumDelay);
        
        // 10*512 + 2*229
        // User type-in.  Number of cardiac phases to reconstruct [1...32]
        cPhase = (short)getSignedShort(endianess);
        fileInfo.setCPhase(cPhase);
        
        // 10*512 + 2*230
        // TE2 (VEMP) in milliseconds
        TE2 = getFloat(endianess);
        fileInfo.setTE2(TE2);
        
        // 10*512 + 2*232
        swapPF = (short)getSignedShort(endianess);
        if (swapPF == 0) {
            fileInfo.setSwapPF("Phase and frequency swap not selected");
        }
        else if (swapPF == 1) {
            fileInfo.setSwapPF("Operator selects to swap phase and frequency");
        }
        else {
            Preferences.debug("swapPF = " + swapPF + "\n");
        }
        
        // 10*512 + 2*233
        // milliseconds
        pauseInterval = (short)getSignedShort(endianess);
        fileInfo.setPauseInterval(pauseInterval);
        
        // 10*512 + 2*234
        // milliseconds
        pauseTime = getFloat(endianess);
        fileInfo.setPauseTime(pauseTime);
        
        // 10*512 + 2*236
        // Bitmap defining user CV's.  Bit mask, tells how many scan fields come up
        userBitmap = (short)getSignedShort(endianess);
        // User defined variables that are PSD dependent
        if ((userBitmap & 1) != 0) {
            // 10*512 + 2*237
            user0 = getFloat(endianess);
            fileInfo.setUser0(user0);
        }
        if ((userBitmap & 2) != 0) {
            raFile.seek(10*512 + 2*239);
            user1 = getFloat(endianess);
            fileInfo.setUser1(user1);
        }
        if ((userBitmap & 4) != 0) {
            raFile.seek(10*512 + 2*241);
            user2 = getFloat(endianess);
            fileInfo.setUser2(user2);
        }
        if ((userBitmap & 8) != 0) {
            raFile.seek(10*512 + 2*243);
            user3 = getFloat(endianess);
            fileInfo.setUser3(user3);
        }
        if ((userBitmap & 16) != 0) {
            raFile.seek(10*512 + 2*245);
            user4 = getFloat(endianess);
            fileInfo.setUser4(user4);
        }
        if ((userBitmap & 32) != 0) {
            raFile.seek(10*512 + 2*247);
            user5 = getFloat(endianess);
            fileInfo.setUser5(user5);
        }
        if ((userBitmap & 64) != 0) {
            raFile.seek(10*512 + 2*249);
            user6 = getFloat(endianess);
            fileInfo.setUser6(user6);
        }
        if ((userBitmap & 128) != 0) {
            raFile.seek(10*512 + 2*251);
            user7 = getFloat(endianess);
            fileInfo.setUser7(user7);
        }
        if ((userBitmap & 256) != 0) {
            raFile.seek(10*512 + 2*253);
            user8 = getFloat(endianess);
            fileInfo.setUser8(user8);
        }
        if ((userBitmap & 512) != 0) {
            raFile.seek(10*512 + 2*255);
            user9 = getFloat(endianess);
            fileInfo.setUser9(user9);
        }
        
        raFile.seek(10*512 + 2*257);
        // Most like normal plane
        obliquePlane = (short)getSignedShort(endianess);
        if (obliquePlane == 0) {
            fileInfo.setObliquePlane("Axial");
        }
        else if (obliquePlane == 1) {
            fileInfo.setObliquePlane("Sagittal");
        }
        else if (obliquePlane == 2) {
            fileInfo.setObliquePlane("Coronal");
        }
        else if (obliquePlane == 5) {
            fileInfo.setObliquePlane("Oax");
        }
        else if (obliquePlane == 6) {
            fileInfo.setObliquePlane("Osag");
        }
        else if (obliquePlane == 7) {
            fileInfo.setObliquePlane("Ocor");
        }
        else {
            Preferences.debug("obliquePlane = " + obliquePlane + "\n");
        }
        
        // 10*512 + 2*258
        contrastUsed = (short)getSignedShort(endianess);
        if (contrastUsed != 0) {
            fileInfo.setContrastUsed("Contrast used");
        }
        else {
            fileInfo.setContrastUsed("Contrast not used");
        }
        
        if (contrastUsed != 0) {
            // 10*512 + 2*259
            // Operator type in.
            contrastAgent = getString(10);
            fileInfo.setContrastAgent(contrastAgent);
            
            // 10*512 + 2*264
            // Operator type in.
            contrastAmount = getFloat(endianess);
            fileInfo.setContrastAmount(contrastAmount);
        } // if (contrastUsed != 0)
        
        raFile.seek(10*512 + 2*266);
        fileFormat = (short)getSignedShort(endianess);
        if (fileFormat == 0) {
            fileInfo.setFileFormat("Pre 3.0");
        }
        else if (fileFormat == 1) {
            fileInfo.setFileFormat("Post 3.0");
        }
        else {
            Preferences.debug("fileFormat = " + fileFormat + "\n");
        }
        
        // 10*512 + 2*267
        autoCenterFrequency = (short)getSignedShort(endianess);
        if (autoCenterFrequency == 0) {
            fileInfo.setAutoCenterFrequency("current");
        }
        else if (autoCenterFrequency == 1) {
            fileInfo.setAutoCenterFrequency("Midpoint");
        }
        else if (autoCenterFrequency == 2) {
            fileInfo.setAutoCenterFrequency("Water");
        }
        else if (autoCenterFrequency == 3) {
            fileInfo.setAutoCenterFrequency("Fat");
        }
        else if (autoCenterFrequency == 4) {
            fileInfo.setAutoCenterFrequency("Peak");
        }
        else if (autoCenterFrequency == 5) {
            fileInfo.setAutoCenterFrequency("Centroid");
        }
        else {
            Preferences.debug("autoCenterFrequency = " + autoCenterFrequency + "\n");
        }
        
        // 10*512 + 2*268
        // Actual transmit frequency used on scan [310,000,000...640,000,000] (310-640 Mhz)
        actualTransmitFrequency = getInt(endianess);
        fileInfo.setActualTransmitFrequency(actualTransmitFrequency);
        
        // 10*512 + 2*270
        // Actual receive frequency used on scan [310,000,000...640,000,000] (310-640 Mhz)
        actualReceiveFrequency = getInt(endianess);
        fileInfo.setActualReceiveFrequency(actualReceiveFrequency);
        
        // 10*512 + 2*272
        // Recommended automated transmit frequency [310,000,000...640,000,000] (310-640 Mhz)
        recommendedTransmitFrequency = getInt(endianess);
        fileInfo.setRecommendedTransmitFrequency(recommendedTransmitFrequency);
        
        // 10*512 + 2*274
        // Recommended automated receive frequency [310,000,000...640,000,000] (310-640 Mhz)
        recommendedReceiveFrequency = getInt(endianess);
        fileInfo.setRecommendedReceiveFrequency(recommendedReceiveFrequency);
        
        // 10*512 + 2*276
        // Recommended automated transmit attenuation 1/10 db [0...319]
        recommendedTransmitAttenuation = getInt(endianess);
        fileInfo.setRecommendedTransmitAttenuation(recommendedTransmitAttenuation);
        
        // 10*512 + 2*278
        // Recommended automated receive attenuation 1/10 db [0...629]
        recommendedReceiveAttenuation = getInt(endianess);
        fileInfo.setRecommendedReceiveAttenuation(recommendedReceiveAttenuation);
        
        // 10*512 + 2*280
        histogramPresent = (short)getSignedShort(endianess);
        if (histogramPresent == 0) {
            fileInfo.setHistogramPresent("Histogram not present in raw header");
        }
        else {
            fileInfo.setHistogramPresent("Histogram present in raw header");
        }
        
        // 10*512 + 2*281
        // See swapPF at 10*512 + 2*232
        pfSwapped = (short)getSignedShort(endianess);
        if (pfSwapped == 0) {
            fileInfo.setPFSwapped("Phase and frequency not swapped");
        }
        else {
            fileInfo.setPFSwapped("Geometries swap phase and frequency either by rules or by user selection");
        }
        
        // 10*512 + 2*282
        // For prescan [1...7]
        R1 = (short)getSignedShort(endianess);
        fileInfo.setR1(R1);
        
        // 10*512 + 2*283
        // For prescan [1...30]
        R2 = (short)getSignedShort(endianess);
        fileInfo.setR2(R2);
        
        // 10*512 + 2*284
        variableBandwidth = (short)getSignedShort(endianess);
        if (variableBandwidth == 0) {
            fileInfo.setVariableBandwidth("Off");
        }
        else {
            fileInfo.setVariableBandwidth("On");
        }
        
        // 10*512 + 2*285
        // [1...7]
        prescanReceiveAttenuation1 = (short)getSignedShort(endianess);
        fileInfo.setPrescanReceiveAttenuation1(prescanReceiveAttenuation1);
        
        // 10*512 + 2*286
        // [1...30]
        prescanReceiveAttenuation2 = (short)getSignedShort(endianess);
        fileInfo.setPrescanReceiveAttenuation2(prescanReceiveAttenuation2);
        
        // 10*512 + 2*287
        autoManualPrescan = (short)getSignedShort(endianess);
        if (autoManualPrescan == 0) {
            fileInfo.setAutoManualPrescan("No auto or manual prescan");
        }
        else if (autoManualPrescan == 1) {
            fileInfo.setAutoManualPrescan("Auto prescan failed.");
        }
        else if (autoManualPrescan == 2) {
            fileInfo.setAutoManualPrescan("Auto prescan succeeded.");
        }
        else if (autoManualPrescan == 3) {
            fileInfo.setAutoManualPrescan("Manual prescan");
        }
        else if (autoManualPrescan == 4) {
            fileInfo.setAutoManualPrescan("Auto prescan failed.  Manual prescan done.");
        }
        else if (autoManualPrescan == 5) {
            fileInfo.setAutoManualPrescan("Auto prescan succeeded.  Manual prescan done.");
        }
        else {
            Preferences.debug("autoManualPrescan = " + autoManualPrescan + "\n");
        }
        
        // 10*512 + 2*288
        changedValuesBitmap = (short)getSignedShort(endianess);
        changedValuesString = "none";
        if ((changedValuesBitmap & 1) != 0) {
            changedValuesString = changedValuesString.concat("_CF");
        }
        if ((changedValuesBitmap & 2) != 0) {
            changedValuesString = changedValuesString.concat("_TA");
        }
        if ((changedValuesBitmap & 4) != 0) {
            changedValuesString = changedValuesString.concat("_R1");
        }
        if ((changedValuesBitmap & 8) != 0) {
            changedValuesString = changedValuesString.concat("_R2");
        }
        
        if (changedValuesBitmap != 0) {
            i = changedValuesString.indexOf("_");
            changedValuesString = changedValuesString.substring(i+1);
        }
        fileInfo.setChangedValues(changedValuesString);
        
        // 10*512 + 2*289
        imageType = (short)getSignedShort(endianess);
        if (imageType == 0) {
            fileInfo.setImageType("Magnitude");
        }
        else if (imageType == 1) {
            fileInfo.setImageType("Phase");
        }
        else if (imageType == 2) {
            fileInfo.setImageType("Real");
        }
        else if (imageType == 3) {
            fileInfo.setImageType("Imaginary");
        }
        else {
            Preferences.debug("imageType = " + imageType + "\n");
        }
        
        // 10*512 + 2*290
        collapseImage = (short)getSignedShort(endianess);
        if (collapseImage == 0) {
            fileInfo.setCollapseImage("Off");
        }
        else if (collapseImage == 1) {
            fileInfo.setCollapseImage("Col");
        }
        else if (collapseImage == 2) {
            fileInfo.setCollapseImage("Mag");
        }
        else if (collapseImage == 3) {
            fileInfo.setCollapseImage("R/L");
        }
        else if (collapseImage == 4) {
            fileInfo.setCollapseImage("A/P");
        }
        else if (collapseImage == 5) {
            fileInfo.setCollapseImage("S/I");
        }
        else if (collapseImage == 6) {
            fileInfo.setCollapseImage("PJN");
        }
        else if (collapseImage == 7) {
            fileInfo.setCollapseImage("ALL");
        }
        else if (collapseImage == 8) {
            fileInfo.setCollapseImage("Omag");
        }
        else if (collapseImage == 9) {
            fileInfo.setCollapseImage("OR/L");
        }
        else if (collapseImage == 10) {
            fileInfo.setCollapseImage("OA/P");
        }
        else if (collapseImage == 11) {
            fileInfo.setCollapseImage("OS/I");
        }
        else if (collapseImage == 12) {
            fileInfo.setCollapseImage("OALL");
        }
        else if (collapseImage == 13) {
            fileInfo.setCollapseImage("OCOL");
        }
        else {
            Preferences.debug("collapseImage = " + collapseImage + "\n");
        }
        
        // 10*512 + 2*291
        sliceThicknessDisclaimer = (short)getSignedShort(endianess);
        if (sliceThicknessDisclaimer == 0) {
            fileInfo.setSliceThicknessDisclaimer("No");
        }
        else {
            fileInfo.setSliceThicknessDisclaimer("Yes");
        }
        
        // 10*512 + 2*292
        // Operator type in.  In units of mm/sec.
        PCVelocityEncoding = (short)getSignedShort(endianess);
        fileInfo.setPCVelocityEncoding(PCVelocityEncoding);
        
        // 10*512 + 2*293
        // Tardis projection angle in degrees.
        projectionAngle = getFloat(endianess);
        fileInfo.setProjectionAngle(projectionAngle);
        
        // 10*512 + 2*295
        concatenatedSATSelection = (short)getSignedShort(endianess);
        if (concatenatedSATSelection == 0) {
            fileInfo.setConcatenatedSATSelection("Off");
        }
        else {
            fileInfo.setConcatenatedSATSelection("On");
        }
        
        if (imageRevisionSubnumber >= 7) {
            // 10*512 + 2*296
            fractionalEffectiveEcho = (short)getSignedShort(endianess);
            if (fractionalEffectiveEcho == 0) {
                fileInfo.setFractionalEffectiveEcho("Fractional/Effective Echo Off");
            }
            else if (fractionalEffectiveEcho == 1) {
                fileInfo.setFractionalEffectiveEcho("Fractional Echo");
            }
            else if (fractionalEffectiveEcho == 2) {
                fileInfo.setFractionalEffectiveEcho("Effective Echo");
            }
            else if (fractionalEffectiveEcho == 3) {
                fileInfo.setFractionalEffectiveEcho("Fractional/Effective Echo");
            }
            else {
                Preferences.debug("fractionalEffectiveEcho = " + fractionalEffectiveEcho + "\n");
            }
            
            // 10*512 + 2*297
            // Echo train length [4...16]
            echoTrainLength = getInt(endianess);
            fileInfo.setEchoTrainLength(echoTrainLength);
            
            // 10*512 + 2*299
            // Slice multiplier to obtain phases for FAST
            // For multiphase scans(MP option).  Number of phases per location
            sliceMultiplier = (short)getSignedShort(endianess);
            fileInfo.setSliceMultiplier(sliceMultiplier);
        } // if (imageRevisionSubnumber >= 7)
        
        if (imageRevisionSubnumber >= 8) {
            // 10*512 + 2*300
            // Cardiac phase number that the current image represents
            cardiacPhaseNumber = (short)getSignedShort(endianess);
            fileInfo.setCardiacPhaseNumber(cardiacPhaseNumber);
            
            // 10*512 + 2*301
            // Number of acquisitions in scan [1...60]
            scanAcquisitionNumber = (short)getSignedShort(endianess);
            fileInfo.setScanAcquisitionNumber(scanAcquisitionNumber);
            
            // 10*512 + 2*302
            vascularImagingFlags = (short)getSignedShort(endianess);
            if ((vascularImagingFlags & 1) != 0) {
                fileInfo.setVascularImagingFlags("No Flags");
            }
            else if ((vascularImagingFlags & 2) != 0) {
                fileInfo.setVascularImagingFlags("Magweight");
            }
            else {
                Preferences.debug("vascularImagingFlags = " + vascularImagingFlags + "\n");
            }
            
            // 10*512 + 2*303
            // Scaling factor for venc. from Recon.
            vencScalingFactor = getFloat(endianess);
            fileInfo.setVencScalingFactor(vencScalingFactor);
        } // if (imageRevisionSubnumber >= 8)
         
        raFile.seek(14336);
        readBuffer(buffer);
    }


    


    /**
     * Accessor that sets the file name and allocates new FileInfo, File and RandomAccess file objects.
     *
     * @param      fName  File name.
     *
     * @exception  IOException  if there is an error constructing the files.
     */
    public void setFileName(String fName) throws IOException {
        fileName = fName;

        try {

            if (raFile != null) {
                raFile.close();
            }

            fileHeader = new File(fileDir + fileName);
            raFile = new RandomAccessFile(fileHeader, "r");
            fileInfo = new FileInfoGESigna4X(fileName, fileDir, FileUtility.GE_SIGNA4X);
        } catch (OutOfMemoryError e) {
            MipavUtil.displayError("Out of memory in FileGEGenesis.setFileName.");
            throw new IOException();
        }

        fileInfo.setEndianess(BIG_ENDIAN);
        fileInfo.setFileName(fileName);
        fileInfo.setDataType(ModelImage.USHORT);
    }


    /**
     * Reads a slice of data at a time and stores the results in the buffer.
     *
     * @param   buffer  buffer where the info is stored
     *
     * @throws  IOException  DOCUMENT ME!
     */
    private void readBuffer(float[] buffer) throws IOException {
        int i = 0;
        int j;
        int nBytes;
        int b1, b2;

        nBytes = 2 * buffer.length;
        raFile.read(byteBuffer, 0, nBytes);

        for (j = 0; j < nBytes; j += 2, i++) {
            b1 = getUnsignedByte(byteBuffer, j);
            b2 = getUnsignedByte(byteBuffer, j + 1);
            buffer[i] = (short) ((b1 << 8) + b2);
        }
    }

    /**
     * DOCUMENT ME!
     *
     * @return  int size of image file data
     */
    public int readImageFileData() {

        try {

            raFile.seek(10*512 + 2*137);
            width = (short)getSignedShort(endianess);
            // 10*512 + 2*138
            height = (short)getSignedShort(endianess);
            
            raFile.seek(10*512 + 2*141);
            compression = (short)getSignedShort(endianess);
            if (compression != 0) {
                Preferences.debug("compression = " + compression + 
                                  " instead of the required 0 for uncompressed\n");
                return - 1;
            }
            
            // 10*512 + 2*142
            bitsPerPixel = (short)getSignedShort(endianess);
            if (bitsPerPixel <= 0) {
                bitsPerPixel = 16;
            }
            if (bitsPerPixel != 16) {
                Preferences.debug("Number of bits used to represent image = " +
                                   bitsPerPixel + " instead of the required 16\n");
            }
            fileInfo.setBitsPerPixel(bitsPerPixel);
            
            raFile.seek(10*512 + 2*44);
            imageNumber = getString(3);

            if (Integer.valueOf(imageNumber).intValue() == 0) {
                imageNumber = "001";
            }
            
            raFile.seek(10*512 + 2*73);
            imageLocation = getFloat(endianess);

            byteBuffer = new byte[2 * width * height];
        } catch (IOException e) {
            return -2;
        } catch (OutOfMemoryError e) {
            MipavUtil.displayError("Out of memory in FileGESigna4x.");
        }

        return (width * height);
    }
}
