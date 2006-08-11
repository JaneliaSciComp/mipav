package gov.nih.mipav.model.file;


import gov.nih.mipav.model.structures.*;

import gov.nih.mipav.view.*;

import java.awt.*;
import java.awt.Dialog.*;

import java.io.*;


/**
 Fixed format header
 Image data is not compressed
 Image data fixed offset 14336 bytes
 Data General host
 
 The image files are of fixed layout, described here as a series of 256 by 16
 bit word blocks (512 bytes), blocks numbered from 0.  The headers start at 
 the following block offsets:
     block 0  - length 4 blocks    -     System configuration
     block 4  - length 2 blocks    -     Site customization
     block 6  - length 2 blocks    -     Study header
     block 8  - length 2 blocks    -     Series header
     block 10 - length 2 blocks    -     Image header
     block 12 - length 4 blocks    -     Raw database header
     block 16 - length 10 blocks   -     Pulse sequence description
     block 26 - length 2 blocks    -     Pixel map (? not ever used)
     block 28 - length 256 blocks  -     Image data
     
 The header is a fixed length of 14336 bytes, after which the uncompressed
 image data starts.
 
 16 bit big endian shorts are used.  Ascii strings are FORTRAN style
 specifications with length in bytes.  4 byte floats are used.
 
 Spacing between slices was seen to vary so resolution[2] cannot be obtained
 by checking the difference between slice spacing.  The field called pixel
 size actually gives 20 * resolution[2], so this field is used to obtain the
 resolution[2].  resolution[0] and resolution[1] can be calculated in 3 different
 ways with 3 slightly different results.  fov/256, difference between corners/255, or
 using the fact that the thickness field = 20 * resolution[0] = 20 * resolution[1].
 I have used the difference between corners since this provides actual coordinates.
 */

public class FileGESigna4X extends FileBase {

    //~ Instance fields ------------------------------------------------------------------------------------------------

    private String studyNumber = null;
    
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
    
    private String seriesNumber = null;
    
    private String seriesDate = null;
    
    private String seriesTime = null;
    
    private String seriesDescription = null;
    
    private short seriesType;
    
    private short coilType;
    
    private String coilName = null;
    
    private short contrastDescription;
    
    private short planeType;
    
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
    
    private short scanMatrixX;
    
    private short scanMatrixY;
    
    private short imageMatrix;
    
    private short imagesAllocated;
    
    private String scanSequence = null;
    
    private String scanProtocolName = null;
    
    private String imageCreationDate = null;
    
    private String imageCreationTime = null;
    
    private String imageNumber = null;
    
    private String series = null;
    
    private float imageLocation;
    
    private float tablePosition;
    
    private float thickness; // 20*res[0] = 20*res[1]
    
    private float imageSpacing;
    
    private float tr; // usec
    
    private float ts;
    
    private float te; // usec
    
    private float ti; // usec
    
    private short numberOfEchos;
    
    private short echoNumber;
    
    private short averagesNumber;
    
    private float pixelSize; // 20*res[2]
    
    private float excitationsNumber;
    
    private float peakSAR;
    
    private float averageSAR;
    
    private short SARMonitored;
    
    private short contiguousSlices;
    
    private short flipAngle;
    
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
    
    private short depth;
    
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
     * reads the Signa 4X file header and data.
     *
     * @param      buffer  DOCUMENT ME!
     *
     * @exception  IOException  if there is an error reading the file
     */
    public void readImage(float[] buffer) throws IOException {

        // try {
        // Read in fields from the block 6 study header
        raFile.seek(6*512 + 2*32);
        studyNumber = getString(5);
        fileInfo.setStudyNumber(studyNumber);
        
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
        
        // Block 8 - series header
        raFile.seek(8*512 + 2*31);
        seriesNumber = getString(3);
        fileInfo.setSeriesNumber(seriesNumber);
        
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
        contrastDescription = (short)getSignedShort(endianess);
        fileInfo.setContrastDescription(contrastDescription);
        
        raFile.seek(8*512 + 2*138);
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
            fileInfo.setOrientation("left");
        }
        else if (orientation == 3) {
            fileInfo.setOrientation("right");
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
        longitudinalAnatomicalReference = getString(32);
        fileInfo.setLongitudinalAnatomicalReference(longitudinalAnatomicalReference);
        
        // 8*512 + 2*177
        verticalAnatomicalReference = getString(32);
        fileInfo.setVerticalAnatomicalReference(verticalAnatomicalReference);
        
        raFile.seek(8*512 + 2*199);
        scanMatrixX = (short)getSignedShort(endianess);
        fileInfo.setScanMatrixX(scanMatrixX);
        
        // 8*512 + 2*200
        scanMatrixY = (short)getSignedShort(endianess);
        fileInfo.setScanMatrixY(scanMatrixY);
        
        // 8*512 + 2*201
        imageMatrix = (short)getSignedShort(endianess);
        fileInfo.setImageMatrix(imageMatrix);
        
        // 8*512 + 2*202
        imagesAllocated = (short)getSignedShort(endianess);
        fileInfo.setImagesAllocated(imagesAllocated);
        
        raFile.seek(8*512 + 2*205);
        scanSequence = getString(12);
        fileInfo.setScanSequence(scanSequence);
        
        raFile.seek(8*512 + 2*213);
        scanProtocolName = getString(20);
        fileInfo.setScanProtocolName(scanProtocolName);
        
        // block 10 - image header
        raFile.seek(10*512 + 2*29);
        imageCreationDate = getString(9); // (dd-mmm-yy)
        fileInfo.setImageCreationDate(imageCreationDate);
        
        raFile.seek(10*512 + 2*37);
        imageCreationTime = getString(8); // (hh:mm:ss)
        fileInfo.setImageCreationTime(imageCreationTime);
        
        raFile.seek(10*512 + 2*44);
        imageNumber = getString(3);
        fileInfo.setImageNumber(imageNumber);
        
        raFile.seek(10*512 + 2*46);
        series = getString(6);
        fileInfo.setSeries(series);
        
        raFile.seek(10*512 + 2*73);
        imageLocation = getFloat(endianess);
        fileInfo.setImageLocation(imageLocation);
        
        // 10*512 + 2*75
        tablePosition = getFloat(endianess);
        fileInfo.setTablePosition(tablePosition);
        
        // 10*512 + 2*77
        thickness = getFloat(endianess);  // 20*res[0] = 20*res[1]
        fileInfo.setThickness(thickness);
        
        // 10*512 + 2*79
        imageSpacing = getFloat(endianess);
        if (imageSpacing != 0.0f) {
            fileInfo.setSliceSpacing(imageSpacing);
        }
        
        raFile.seek(10*512 + 2*82);
        tr = getFloat(endianess); // repetition/recovery time usec
        fileInfo.setTR(tr);
        
        // 10*512 + 2*84
        ts = getFloat(endianess); // scan time
        fileInfo.setTS(ts);
        
        raFile.seek(10*512 + 2*86);
        te = getFloat(endianess); //  Echo delay usec
        fileInfo.setTE(te);
        
        // 10*512 + 2*88
        ti = getFloat(endianess); // Inversion time usec
        fileInfo.setTI(ti);
        
        raFile.seek(10*512 + 2*98);
        numberOfEchos = (short)getSignedShort(endianess);
        fileInfo.setNumberOfEchos(numberOfEchos);
        
        // 10*512 + 2*99
        echoNumber = (short)getSignedShort(endianess);
        fileInfo.setEchoNumber(echoNumber);
        
        raFile.seek(10*512 + 2*101);
        averagesNumber = (short)getSignedShort(endianess); // Number of averages
        fileInfo.setAveragesNumber(averagesNumber);
        
        raFile.seek(10*512 + 2*139);
        pixelSize = getFloat(endianess); // 20*res[2]
        fileInfo.setPixelSize(pixelSize);
        fileInfo.setResolutions(pixelSize/20.0f, 2);
        
        raFile.seek(10*512 + 2*146);
        excitationsNumber = getFloat(endianess); // Number of excitations
        fileInfo.setExcitationsNumber(excitationsNumber);
        
        // 10*512 + 2*148
        peakSAR = getFloat(endianess);
        fileInfo.setPeakSAR(peakSAR);
        
        // 10*512 + 2*150
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
            fileInfo.setContiguousSlices("Slices are contiguous");
        }
        else {
            fileInfo.setContiguousSlices("Slices are not contiguous");
        }
        
        raFile.seek(10*512 + 2*175);
        flipAngle = (short)getSignedShort(endianess);
        fileInfo.setFlipAngle(flipAngle);
        
        raFile.seek(10*512 + 2*197);
        rCenter = getFloat(endianess);
        fileInfo.setRCenter(rCenter);
        
        // 10*512 + 2*199
        aCenter = getFloat(endianess);
        fileInfo.setACenter(aCenter);
        
        // 10*512 + 2*201
        sCenter = getFloat(endianess);
        fileInfo.setSCenter(sCenter);
        
        // 10*512 + 2*203
        rNormal = getFloat(endianess);
        fileInfo.setRNormal(rNormal);
        
        // 10*512 + 2*205
        aNormal = getFloat(endianess);
        fileInfo.setANormal(aNormal);
        
        // 10*512 + 2*207
        sNormal = getFloat(endianess);
        fileInfo.setSNormal(sNormal);
        
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
        fileInfo.setResolutions(resX, 0);
        fileInfo.setResolutions(resY, 1);

        // orient[2] is calculated in FileIo.java by comparing position values
        // of the top left hand corner between slice 0 and slice 1.
        fileInfo.setAxisOrientation(orient);
        fileInfo.setOrigin(start);
        
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
            fileInfo = new FileInfoGESigna4X(fileName, fileDir, FileBase.GE_SIGNA4X);
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
            depth = (short)getSignedShort(endianess);
            if (depth <= 0) {
                depth = 16;
            }
            if (depth != 16) {
                Preferences.debug("Pixel depth = " + depth + " instead of the required 16\n");
            }
            
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
