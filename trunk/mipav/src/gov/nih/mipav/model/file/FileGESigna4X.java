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
    
    private String seriesNumber = null;
    
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
    
    private String imageNumber = null;
    
    private float imageLocation;
    
    private float tablePosition;
    
    private float imageThickness;
    
    private float imageSpacing;
    
    private float tr; // usec
    
    private float te; // usec
    
    private float ti; // usec
    
    private short numberOfEchos;
    
    private short echoNumber;
    
    private short nexShort;
    
    private float nexFloat;
    
    private short flipAngle;
    
    /** DOCUMENT ME! */
    private byte[] byteBuffer = new byte[2 * 256 * 256];

    /** DOCUMENT ME! */
    private String fileDir;

    /** DOCUMENT ME! */
    private File fileHeader;

    /** DOCUMENT ME! */
    private FileInfoGESigna4X fileInfo;

    /** DOCUMENT ME! */
    private String fileName;

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
     * reads the Signa 4X file header and data.
     *
     * @param      buffer  DOCUMENT ME!
     *
     * @exception  IOException  if there is an error reading the file
     */
    public void readImage(float[] buffer) throws IOException {

        // The data types are Sun, hence the byte order is big-endian.
        boolean endianess = BIG_ENDIAN;

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
        
        // Block 8 - series header
        raFile.seek(8*512 + 2*31);
        seriesNumber = getString(3);
        fileInfo.setSeriesNumber(seriesNumber);
        
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
        
        // 8*512 + 2*112
        contrastDescription = (short)getSignedShort(endianess);
        fileInfo.setContrastDescription(contrastDescription);
        
        raFile.seek(8*512 + 2*138);
        planeType = (short)getSignedShort(endianess);
        if (planeType == 0) {
            fileInfo.setImageOrientation(FileInfoBase.AXIAL);
        }
        else if (planeType == 1) {
            fileInfo.setImageOrientation(FileInfoBase.SAGITTAL);
        }
        else if (planeType == 2) {
            fileInfo.setImageOrientation(FileInfoBase.CORONAL);
        }
        else if (planeType == 3) {
            Preferences.debug("Plane type = oblique\n");
            fileInfo.setImageOrientation(FileInfoBase.UNKNOWN_ORIENT);
        }
        else if (planeType == 4) {
            Preferences.debug("Plane type = screen save\n");
            fileInfo.setImageOrientation(FileInfoBase.UNKNOWN_ORIENT);
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
        rlCenter = -getFloat(endianess); // R+L-  MIPAV is R to L
        fileInfo.setRLCenter(rlCenter);
        
        // 8*512 + 2*155
        apCenter = -getFloat(endianess); // A+P-  MIPAV is A to P
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
        
        // block 10 - image header
        raFile.seek(10*512 + 2*44);
        imageNumber = getString(3);
        fileInfo.setImageNumber(imageNumber);
        
        raFile.seek(10*512 + 2*73);
        imageLocation = getFloat(endianess);
        fileInfo.setImageLocation(imageLocation);
        
        // 10*512 + 2*75
        tablePosition = getFloat(endianess);
        fileInfo.setTablePosition(tablePosition);
        
        // 10*512 + 2*77
        imageThickness = getFloat(endianess);
        fileInfo.setImageThickness(imageThickness);
        
        // 10*512 + 2*79
        imageSpacing = getFloat(endianess);
        fileInfo.setSliceSpacing(imageSpacing);
        fileInfo.setResolutions(imageSpacing, 2);
        
        raFile.seek(10*512 + 2*82);
        tr = getFloat(endianess); // usec
        fileInfo.setTR(tr);
        
        raFile.seek(10*512 + 2*86);
        te = getFloat(endianess); //usec
        fileInfo.setTE(te);
        
        // 10*512 + 2*88
        ti = getFloat(endianess); // usec
        fileInfo.setTI(ti);
        
        raFile.seek(10*512 + 2*98);
        numberOfEchos = (short)getSignedShort(endianess);
        fileInfo.setNumberOfEchos(numberOfEchos);
        
        // 10*512 + 2*99
        echoNumber = (short)getSignedShort(endianess);
        fileInfo.setEchoNumber(echoNumber);
        
        raFile.seek(10*512 + 2*101);
        nexShort = (short)getSignedShort(endianess); // NEX (if not fractional)
        fileInfo.setNexShort(nexShort);
        
        raFile.seek(10*512 + 2*146);
        nexFloat = getFloat(endianess); // NEX
        fileInfo.setNexFloat(nexFloat);
        
        raFile.seek(10*512 + 2*175);
        flipAngle = (short)getSignedShort(endianess);
        fileInfo.setFlipAngle(flipAngle);
        
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


}
