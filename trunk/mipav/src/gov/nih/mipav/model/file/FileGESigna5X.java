package gov.nih.mipav.model.file;


import gov.nih.mipav.model.structures.*;

import gov.nih.mipav.view.*;

import java.io.*;


/**
 * Reads both GE Neducak Systems MR 5X and LX. Modify to handle MRGE files from a GE signa-5 with IMGF starting at
 * locating 3228 and image data starting at location 8424. Follows file format thru lenUsrDefData field; then varies
 * from format.
 */

public class FileGESigna5X extends FileBase {

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** DOCUMENT ME! */
    private byte[] byteBuffer = null;

    /** DOCUMENT ME! */
    private int compression; // 0 as is
                             // 1 retangular
                             // 2 packed
                             // 3 compressed
                             // 4 compressed and packed

    /** DOCUMENT ME! */
    private int depth; // number of bits

    /** DOCUMENT ME! */
    private String fileDir;

    /** DOCUMENT ME! */
    private File fileHeader;

    /** DOCUMENT ME! */
    private FileInfoGESigna5X fileInfo;

    /** DOCUMENT ME! */
    private String fileName;

    /** DOCUMENT ME! */
    private int imageNumber;

    /** DOCUMENT ME! */
    private int imagePtr;

    /** DOCUMENT ME! */
    private int lenExamHdr; // should be 1024 or 1040 bytes

    /** DOCUMENT ME! */
    private int lenImageHdr; // should be 1022 or 1044 bytes

    /** DOCUMENT ME! */
    private int lenSeriesHdr; // should be 1020 or 1028 bytes

    /** DOCUMENT ME! */
    private int[] orient = new int[3];

    /** DOCUMENT ME! */
    private float[] start = new float[3];

    /** DOCUMENT ME! */
    private int startAdjust = 0;

    /** DOCUMENT ME! */
    private int width = 0, height = 0;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * reader/writer constructor.
     *
     * @param      fileName  file name
     * @param      fileDir   file directory
     *
     * @exception  IOException  if there is an error making the file
     */
    public FileGESigna5X(String fileName, String fileDir) throws IOException {

        this.fileName = fileName;
        this.fileDir = fileDir;
    }

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * Prepares this class for cleanup. Calls the <code>finalize</code> method for existing elements, closes any open
     * files and sets other elements to <code>null</code>.
     */
    public void finalize() {
        byteBuffer = null;
        fileName = null;
        fileDir = null;
        fileInfo = null;
        fileHeader = null;
        orient = null;
        start = null;
        try {
            super.finalize();
        } catch (Throwable er) { }
    }
    
    /**
     * DOCUMENT ME!
     *
     * @return  FileInfoGESigna5X fileInfo
     */
    public FileInfoGESigna5X getFileInfo() {
        return fileInfo;
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
     * DOCUMENT ME!
     *
     * @return  int image number
     */
    public int getImageNumber() {
        return imageNumber;
    }

    /**
     * DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    public int getStartAdjust() {
        return startAdjust;
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
     * reads the Signa 5X file header and data.
     *
     * @param      buffer  DOCUMENT ME!
     *
     * @exception  IOException  if there is an error reading the file
     */
    public void readImage(float[] buffer) throws IOException {

        // The data types are Sun, hence the byte order is big-endian.
        boolean endianess = BIG_ENDIAN;

        // try {
        if (startAdjust > 0) {
            raFile.seek(startAdjust + 8 + 4);
        } else {
            raFile.seek(4); //
        }

        fileInfo.ptrImage = raFile.readInt() + startAdjust;
        fileInfo.width = raFile.readInt();
        fileInfo.height = raFile.readInt();
        fileInfo.depth = raFile.readInt();
        fileInfo.compression = raFile.readInt();

        if (startAdjust > 0) {
            raFile.seek(32 + startAdjust + 8);
        } else {
            raFile.seek(32);
        }

        fileInfo.valueBg = raFile.readInt();

        if (startAdjust > 0) {
            raFile.seek(54 + startAdjust + 8);
        } else {
            raFile.seek(54);
        }

        fileInfo.checkSum = getUnsignedShort(endianess);
        fileInfo.ptrUID = raFile.readInt() + startAdjust;
        fileInfo.lenUID = raFile.readInt();
        fileInfo.ptrUnpackHdr = raFile.readInt() + startAdjust;
        fileInfo.lenUnpackHdr = raFile.readInt();
        fileInfo.ptrCmprsnHdr = raFile.readInt() + startAdjust;
        fileInfo.lenCmprsnHdr = raFile.readInt();
        fileInfo.ptrHistoHdr = raFile.readInt() + startAdjust;
        fileInfo.lenHistoHdr = raFile.readInt();
        fileInfo.ptrTextPlane = raFile.readInt() + startAdjust;
        fileInfo.lenTextPlane = raFile.readInt();
        fileInfo.ptrGraphics = raFile.readInt() + startAdjust;
        fileInfo.lenGraphics = raFile.readInt();
        fileInfo.ptrDBHdr = raFile.readInt() + startAdjust;
        fileInfo.lenDBHdr = raFile.readInt();
        fileInfo.addValue = raFile.readInt();
        fileInfo.ptrUsrDefData = raFile.readInt() + startAdjust;
        fileInfo.lenUsrDefData = raFile.readInt();

        if (startAdjust > 0) {
            raFile.seek(imagePtr);
            readBuffer(buffer);

            return;
        }

        fileInfo.ptrSuiteHdr = raFile.readInt();
        fileInfo.lenSuiteHdr = raFile.readInt();
        fileInfo.ptrExamHdr = raFile.readInt();
        lenExamHdr = raFile.readInt();

        if ((lenExamHdr != 1024) && (lenExamHdr != 1040)) {
            MipavUtil.displayError("length of exam header is = " + lenExamHdr +
                                   ", rather than the required 1024 or 1040 ");

            return;
        }

        fileInfo.lenExamHdr = lenExamHdr;
        fileInfo.ptrSeriesHdr = raFile.readInt();
        lenSeriesHdr = raFile.readInt();

        if ((lenSeriesHdr != 1020) && (lenSeriesHdr != 1028)) {
            MipavUtil.displayError("length of series header is = " + lenSeriesHdr +
                                   ", rather than the required 1020 or 1028");

            return;
        }

        fileInfo.lenSeriesHdr = lenSeriesHdr;
        fileInfo.ptrImageHdr = raFile.readInt();
        lenImageHdr = raFile.readInt();

        // The 1044 length header has 8 2-byte fillers that are not present in the
        // 1022 byte header and there are 3 fillers which are 1 byte in length in
        // the 1022 byte header but 3 bytes in length in the 1044 byte header.
        if ((lenImageHdr != 1022) && (lenImageHdr != 1044)) {
            MipavUtil.displayError("length of image header is = " + lenImageHdr +
                                   ", rather than the required 1022 or 1044");

            return;
        }

        fileInfo.lenImageHdr = lenImageHdr;

        // Read in exam header data
        raFile.seek(fileInfo.ptrExamHdr);
        fileInfo.suiteID = getString(4); // 0
        fileInfo.exUniq = (short) getSignedShort(endianess); // 4
        fileInfo.exDiskID = getString(1); // 6
        raFile.readByte(); // filler0
        fileInfo.examNum = getUnsignedShort(endianess); // 8
        fileInfo.hospName = getString(33); // 10
        raFile.readByte(); // filler1
        fileInfo.detect = (short) getSignedShort(endianess); // 44

        if (lenExamHdr == 1040) {
            raFile.readByte(); // filler2
            raFile.readByte();
        }

        fileInfo.numCells = getInt(endianess); // 46
        fileInfo.zeroCell = getFloat(endianess); // 50
        fileInfo.cellSpace = getFloat(endianess); // 54
        fileInfo.srcToDet = getFloat(endianess); // 58
        fileInfo.srcToIso = getFloat(endianess); // 62
        fileInfo.tubeType = (short) getSignedShort(endianess); // 66
        fileInfo.dasType = (short) getSignedShort(endianess); // 68
        fileInfo.numDcnK = (short) getSignedShort(endianess); // 70
        fileInfo.dcnLen = (short) getSignedShort(endianess); // 72
        fileInfo.dcnDensity = (short) getSignedShort(endianess); // 74
        fileInfo.dcnStepSize = (short) getSignedShort(endianess); // 76
        fileInfo.dcnShiftCnt = (short) getSignedShort(endianess); // 78

        if (lenExamHdr == 1040) {
            raFile.readByte(); // filler3
            raFile.readByte();
        }

        fileInfo.magStrength = getInt(endianess); // 80
        fileInfo.patientID = getString(13); // 84
        fileInfo.patientName = getString(25); // 97
        fileInfo.patientAge = (short) getSignedShort(endianess); // 122
        fileInfo.patian = (short) getSignedShort(endianess); // 124
        fileInfo.patientSex = (short) getSignedShort(endianess); // 126
        fileInfo.patWeight = getInt(endianess); // 128
        fileInfo.trauma = (short) getSignedShort(endianess); // 132
        fileInfo.hist = getString(61); // 134
        fileInfo.reqnum = getString(13); // 195
        fileInfo.exDateTime = getInt(endianess); // 208
        fileInfo.refPhy = getString(33); // 212
        fileInfo.diagRad = getString(33); // 245
        fileInfo.op = getString(4); // 278
        fileInfo.exDesc = getString(23); // 282
        fileInfo.examType = getString(3); // 305
        fileInfo.exFormat = (short) getSignedShort(endianess); // 308

        if (lenExamHdr == 1040) {
            raFile.readByte(); // filler4
            raFile.readByte();
            raFile.readByte();
            raFile.readByte();
            raFile.readByte();
            raFile.readByte();
        }

        fileInfo.firstAxTime = getLong(endianess); // 310
        fileInfo.exSysID = getString(9); // 318

        if (lenExamHdr == 1024) {
            raFile.readByte(); // filler 2
        } else { // lenExamHdr == 1040
            raFile.readByte(); // filler 5
            raFile.readByte();
            raFile.readByte();
        }

        fileInfo.exLastMod = getInt(endianess); // 328
        fileInfo.protocolFlag = (short) getSignedShort(endianess); // 332
        fileInfo.exAllocKey = getString(13); // 334
        raFile.readByte(); // filler 3 for lenExamHdr == 1024, filler 6 for lenExamHdr == 1040
        fileInfo.exDeltaCount = getInt(endianess); // 348
        fileInfo.exVersCre = getString(2); // 352
        fileInfo.exVersCur = getString(2); // 354
        fileInfo.exChecksum = getInt(endianess); // 356
        fileInfo.exComplete = getInt(endianess); // 360
        fileInfo.exSeriesCt = getInt(endianess); // 364
        fileInfo.exNumArch = getInt(endianess); // 368
        fileInfo.exNumSeries = getInt(endianess); // 372
        fileInfo.exSeriesLen = getInt(endianess); // 376
        fileInfo.exSeriesData = getInt(endianess); // 380
        fileInfo.exNumUnSer = getInt(endianess); // 384
        fileInfo.exUnSeriesLen = getInt(endianess); // 388
        fileInfo.exUnSeriesData = getInt(endianess); // 392
        fileInfo.exToArchCnt = getInt(endianess); // 396
        fileInfo.exToArchiveLen = getInt(endianess); // 400
        fileInfo.exToArchiveData = getInt(endianess); // 404
        fileInfo.exProspCnt = getInt(endianess); // 408
        fileInfo.exProspLen = getInt(endianess); // 412
        fileInfo.exProspData = getInt(endianess); // 416
        fileInfo.exModelNum = getInt(endianess); // 420
        fileInfo.exModelCnt = getInt(endianess); // 424
        fileInfo.exModelsLen = getInt(endianess); // 428
        fileInfo.exModelsData = getInt(endianess); // 432
        fileInfo.exStat = (short) getSignedShort(endianess); // 436
        fileInfo.uniqSysID = getString(16); // 438
        fileInfo.serviceID = getString(16); // 454
        fileInfo.mobileLoc = getString(4); // 470
        fileInfo.studyUID = getString(32); // 474
        fileInfo.studyStatus = (short) getSignedShort(endianess); // 506
        fileInfo.exPadding = getString(516); // 508

        // Read in series header data
        raFile.seek(fileInfo.ptrSeriesHdr);
        fileInfo.seSuid = getString(4); // 0
        fileInfo.seUniq = (short) getSignedShort(endianess); // 4
        fileInfo.seDiskID = raFile.readByte(); // 6
        raFile.readByte(); // filler 0
        fileInfo.seExamNo = (short) getSignedShort(endianess); // 8
        fileInfo.seriesNum = (short) getSignedShort(endianess); // 10
        fileInfo.seDateTime = getInt(endianess); // 12
        fileInfo.seActualDT = getInt(endianess); // 16
        fileInfo.seDesc = getString(30); // 20
        fileInfo.prSysID = getString(9); // 50
        fileInfo.panSysID = getString(9); // 59
        fileInfo.seType = (short) getSignedShort(endianess); // 68
        fileInfo.seSource = (short) getSignedShort(endianess); // 70
        fileInfo.sePlane = (short) getSignedShort(endianess); // 72
        fileInfo.scanType = (short) getSignedShort(endianess); // 74
        fileInfo.position = getInt(endianess); // 76
        fileInfo.entry = getInt(endianess); // 80
        fileInfo.anatomicalRef = getString(3); // 84
        raFile.readByte(); // filler 1
        fileInfo.lmHor = getFloat(endianess); // 88
        fileInfo.scanProtocolName = getString(25); // 92
        raFile.readByte(); // filler 2
        fileInfo.seContrast = (short) getSignedShort(endianess); // 118
        fileInfo.startRAS = getString(1); // 120
        raFile.readByte(); // filler 3

        if (lenSeriesHdr == 1028) {
            raFile.readByte();
            raFile.readByte();
        }

        fileInfo.startLoc = getFloat(endianess); // 122
        fileInfo.endRAS = getString(1); // 126
        raFile.readByte(); // filler 4

        if (lenSeriesHdr == 1028) {
            raFile.readByte();
            raFile.readByte();
        }

        fileInfo.endLoc = getFloat(endianess); // 128
        fileInfo.sePseq = (short) getSignedShort(endianess); // 132
        fileInfo.seSortOrder = (short) getSignedShort(endianess); // 134
        fileInfo.seLandmarkCnt = getInt(endianess); // 136
        fileInfo.seNacq = (short) getSignedShort(endianess); // 140
        fileInfo.xBaseSt = (short) getSignedShort(endianess); // 142
        fileInfo.xBaseEnd = (short) getSignedShort(endianess); // 144
        fileInfo.xenhSt = (short) getSignedShort(endianess); // 146
        fileInfo.xenhEnd = (short) getSignedShort(endianess); // 148

        if (lenSeriesHdr == 1028) {
            raFile.readByte(); // filler 5
            raFile.readByte();
        }

        fileInfo.seLastMod = getInt(endianess); // 150
        fileInfo.seAllocKey = getString(13); // 154

        if (lenSeriesHdr == 1020) {
            raFile.readByte(); // filler 5
        } else { // lenSeriesHdr = 1028
            raFile.readByte(); // filler 6
            raFile.readByte();
            raFile.readByte();
        }

        fileInfo.seDeltaCnt = getInt(endianess); // 168
        fileInfo.seVersCre = getString(2); // 172
        fileInfo.seVersCur = getString(2); // 174
        fileInfo.sePdsA = getFloat(endianess); // 176
        fileInfo.sePdsC = getFloat(endianess); // 180
        fileInfo.sePdsU = getFloat(endianess); // 184
        fileInfo.seChecksum = getInt(endianess); // 188
        fileInfo.seComplete = getInt(endianess); // 192
        fileInfo.seNumArch = getInt(endianess); // 196
        fileInfo.seImageCt = getInt(endianess); // 200
        fileInfo.seNumImages = getInt(endianess); // 204
        fileInfo.imagesLen = getInt(endianess); // 208
        fileInfo.imagesData = getInt(endianess); // 212
        fileInfo.numUnImg = getInt(endianess); // 216
        fileInfo.unImagesLen = getInt(endianess); // 220
        fileInfo.unImagesData = getInt(endianess); // 224
        fileInfo.toArchiveCnt = getInt(endianess); // 228
        fileInfo.toArchiveLen = getInt(endianess); // 232
        fileInfo.toArchiveData = getInt(endianess); // 236
        fileInfo.echo1Alpha = getFloat(endianess); // 240
        fileInfo.echo1Beta = getFloat(endianess); // 244
        fileInfo.echo1Window = (short) getSignedShort(endianess); // 248
        fileInfo.echo1Level = (short) getSignedShort(endianess); // 250
        fileInfo.echo2Alpha = getFloat(endianess); // 252
        fileInfo.echo2Beta = getFloat(endianess); // 256
        fileInfo.echo2Window = (short) getSignedShort(endianess); // 260
        fileInfo.echo2Level = (short) getSignedShort(endianess); // 262
        fileInfo.echo3Alpha = getFloat(endianess); // 264
        fileInfo.echo3Beta = getFloat(endianess); // 268
        fileInfo.echo3Window = (short) getSignedShort(endianess); // 272
        fileInfo.echo3Level = (short) getSignedShort(endianess); // 274
        fileInfo.echo4Alpha = getFloat(endianess); // 276
        fileInfo.echo4Beta = getFloat(endianess); // 280
        fileInfo.echo4Window = (short) getSignedShort(endianess); // 284
        fileInfo.echo4Level = (short) getSignedShort(endianess); // 286
        fileInfo.echo5Alpha = getFloat(endianess); // 288
        fileInfo.echo5Beta = getFloat(endianess); // 292
        fileInfo.echo5Window = (short) getSignedShort(endianess); // 296
        fileInfo.echo5Level = (short) getSignedShort(endianess); // 298
        fileInfo.echo6Alpha = getFloat(endianess); // 300
        fileInfo.echo6Beta = getFloat(endianess); // 304
        fileInfo.echo6Window = (short) getSignedShort(endianess); // 308
        fileInfo.echo6Level = (short) getSignedShort(endianess); // 310
        fileInfo.echo7Alpha = getFloat(endianess); // 312
        fileInfo.echo7Beta = getFloat(endianess); // 316
        fileInfo.echo7Window = (short) getSignedShort(endianess); // 320
        fileInfo.echo7Level = (short) getSignedShort(endianess); // 322
        fileInfo.echo8Alpha = getFloat(endianess); // 324
        fileInfo.echo8Beta = getFloat(endianess); // 328
        fileInfo.echo8Window = (short) getSignedShort(endianess); // 332
        fileInfo.echo8Level = (short) getSignedShort(endianess); // 334
        fileInfo.seriesUID = getString(32); // 336
        fileInfo.landmarkUID = getString(32); // 368
        fileInfo.equipmentUID = getString(32); // 400
        fileInfo.sePadding = getString(588); // 432

        // Image stuff
        raFile.seek(fileInfo.ptrImageHdr);
        fileInfo.imgHdrSuiteID = getString(4); // 0
        fileInfo.uniq = (short) getSignedShort(endianess); // 4
        fileInfo.diskid = raFile.readByte(); // 6
        raFile.readByte(); // filler0                                // 7
        fileInfo.imgHdrExamNum = getUnsignedShort(endianess); // 8
        fileInfo.imgHdrSeriesNum = (short) getSignedShort(endianess); // 10
        fileInfo.imageNum = (short) getSignedShort(endianess); // 12

        if (lenImageHdr == 1044) {
            raFile.readByte(); // filler1
            raFile.readByte();
        }

        fileInfo.dateTime = getInt(endianess); // 14
        fileInfo.actualDateTime = getInt(endianess); // 18
        fileInfo.scTime = getFloat(endianess); // 22
        fileInfo.sliceThickness = getFloat(endianess); // 26
        fileInfo.matrixSizeX = (short) getSignedShort(endianess); // 30
        fileInfo.matrixSizeY = (short) getSignedShort(endianess); // 32
        fileInfo.FOVX = getFloat(endianess); // 34
        fileInfo.FOVY = getFloat(endianess); // 38
        fileInfo.imageDimX = getFloat(endianess); // 42
        fileInfo.imageDimY = getFloat(endianess); // 46
        fileInfo.pixelResX = getFloat(endianess); // 50
        fileInfo.pixelResY = getFloat(endianess); // 54

        float[] resols = new float[5];
        resols[0] = fileInfo.pixelResX;
        resols[1] = fileInfo.pixelResY;
        resols[2] = fileInfo.sliceThickness;
        resols[3] = 0.0f;
        resols[4] = 0.0f;
        fileInfo.setResolutions(resols);

        int[] units = new int[5];
        units[0] = FileInfoBase.MILLIMETERS;
        units[1] = FileInfoBase.MILLIMETERS;
        units[2] = FileInfoBase.MILLIMETERS;
        units[3] = FileInfoBase.UNKNOWN_MEASURE;
        units[4] = FileInfoBase.UNKNOWN_MEASURE;
        fileInfo.setUnitsOfMeasure(units);

        fileInfo.pixelID = getString(14); // 58
        fileInfo.IVCntrstAgent = getString(17); // 72
        fileInfo.OralCntrstAgent = getString(17); // 89
        fileInfo.contrastMode = (short) getSignedShort(endianess); // 106
        fileInfo.serrx = (short) getSignedShort(endianess); // 108
        fileInfo.imgrx = (short) getSignedShort(endianess); // 110
        fileInfo.screenFormat = (short) getSignedShort(endianess); // 112
        fileInfo.planeType = (short) getSignedShort(endianess); // 114

        if (lenImageHdr == 1044) {
            raFile.readByte(); // filler2
            raFile.readByte();
        }

        fileInfo.scanSpacing = getFloat(endianess); // 116
        resols[2] += fileInfo.scanSpacing;
        fileInfo.compress = (short) getSignedShort(endianess); // 120
        fileInfo.scoutType = (short) getSignedShort(endianess); // 122
        fileInfo.loc_ras = getString(1); // 124

        if (lenImageHdr == 1022) {
            raFile.readByte(); // filler1                         // 125
        } else { // lenImageHdr == 1044
            raFile.readByte(); // filler3
            raFile.readByte();
            raFile.readByte();
        }

        fileInfo.imgLoc = getFloat(endianess); // 126
        fileInfo.imgCtrR = getFloat(endianess); // 130
        fileInfo.imgCtrA = getFloat(endianess); // 134
        fileInfo.imgCtrS = getFloat(endianess); // 138
        fileInfo.norm_R = getFloat(endianess); // 142
        fileInfo.norm_A = getFloat(endianess); // 146
        fileInfo.norm_S = getFloat(endianess); // 150

        if ((Math.abs(fileInfo.norm_R) > Math.abs(fileInfo.norm_A)) &&
                (Math.abs(fileInfo.norm_R) > Math.abs(fileInfo.norm_S))) {
            fileInfo.setImageOrientation(FileInfoBase.SAGITTAL);
        } else if ((Math.abs(fileInfo.norm_A) > Math.abs(fileInfo.norm_R)) &&
                       (Math.abs(fileInfo.norm_A) > Math.abs(fileInfo.norm_S))) {
            fileInfo.setImageOrientation(FileInfoBase.CORONAL);
        } else {
            fileInfo.setImageOrientation(FileInfoBase.AXIAL);
        }

        fileInfo.imgTLHC_R = getFloat(endianess); // 154
        fileInfo.imgTLHC_A = getFloat(endianess); // 158
        fileInfo.imgTLHC_S = getFloat(endianess); // 162
        fileInfo.imgTRHC_R = getFloat(endianess); // 166
        fileInfo.imgTRHC_A = getFloat(endianess); // 170
        fileInfo.imgTRHC_S = getFloat(endianess); // 174
        fileInfo.imgBRHC_R = getFloat(endianess); // 178
        fileInfo.imgBRHC_A = getFloat(endianess); // 182
        fileInfo.imgBRHC_S = getFloat(endianess); // 186

        if (fileInfo.imageOrientation == FileInfoBase.CORONAL) {
            start[0] = -fileInfo.imgTLHC_R;
            start[1] = fileInfo.imgTLHC_S;
            start[2] = -fileInfo.imgTLHC_A;

            if (fileInfo.imgTLHC_R > fileInfo.imgTRHC_R) {
                orient[0] = FileInfoBase.ORI_R2L_TYPE;
            } else {
                orient[0] = FileInfoBase.ORI_L2R_TYPE;
            }

            if (fileInfo.imgTRHC_S > fileInfo.imgBRHC_S) {
                orient[1] = FileInfoBase.ORI_S2I_TYPE;
            } else {
                orient[1] = FileInfoBase.ORI_I2S_TYPE;
            }
        } else if (fileInfo.imageOrientation == FileInfoBase.SAGITTAL) {
            start[0] = -fileInfo.imgTLHC_A;
            start[1] = fileInfo.imgTLHC_S;
            start[2] = -fileInfo.imgTLHC_R;

            if (fileInfo.imgTLHC_A > fileInfo.imgTRHC_A) {
                orient[0] = FileInfoBase.ORI_A2P_TYPE;
            } else {
                orient[0] = FileInfoBase.ORI_P2A_TYPE;
            }

            if (fileInfo.imgTRHC_S > fileInfo.imgBRHC_S) {
                orient[1] = FileInfoBase.ORI_S2I_TYPE;
            } else {
                orient[1] = FileInfoBase.ORI_I2S_TYPE;
            }
        } else { // AXIAL
            start[0] = -fileInfo.imgTLHC_R;
            start[1] = -fileInfo.imgTLHC_A;
            start[2] = fileInfo.imgTLHC_S;

            if (fileInfo.imgTLHC_R > fileInfo.imgTRHC_R) {
                orient[0] = FileInfoBase.ORI_R2L_TYPE;
            } else {
                orient[0] = FileInfoBase.ORI_L2R_TYPE;
            }

            if (fileInfo.imgTRHC_A > fileInfo.imgBRHC_A) {
                orient[1] = FileInfoBase.ORI_A2P_TYPE;
            } else {
                orient[1] = FileInfoBase.ORI_P2A_TYPE;
            }
        }

        // orient[2] is calculated in FileIo.java by comparing position values
        // of the top left hand corner between slice 0 and slice 1.
        fileInfo.setAxisOrientation(orient);
        fileInfo.setOrigin(start);
        fileInfo.forImgRev = getString(4); // 190
        fileInfo.pulseRepTime = getInt(endianess); // 194
        fileInfo.inverTime = getInt(endianess); // 198
        fileInfo.echoTime = getInt(endianess); // 202
        fileInfo.te2 = getInt(endianess); // 206
        fileInfo.nEchoes = (short) getSignedShort(endianess); // 210
        fileInfo.echoNum = (short) getSignedShort(endianess); // 212
        fileInfo.tableDelta = getFloat(endianess); // 214
        fileInfo.NEX = getFloat(endianess); // 218
        fileInfo.contig = (short) getSignedShort(endianess); // 222
        fileInfo.heartRate = (short) getSignedShort(endianess); // 224
        fileInfo.tDel = getInt(endianess); // 226
        fileInfo.sarAvg = getFloat(endianess); // 230
        fileInfo.sarPeak = getFloat(endianess); // 234
        fileInfo.monSar = (short) getSignedShort(endianess); // 238
        fileInfo.trgWindow = (short) getSignedShort(endianess); // 240
        fileInfo.repTime = getFloat(endianess); // 242
        fileInfo.imgPCycle = (short) getSignedShort(endianess); // 246
        fileInfo.xmtGain = (short) getSignedShort(endianess); // 248
        fileInfo.rcvGain1 = (short) getSignedShort(endianess); // 250
        fileInfo.rcvGain2 = (short) getSignedShort(endianess); // 252
        fileInfo.mr_flip = (short) getSignedShort(endianess); // 254

        if (lenImageHdr == 1044) {
            raFile.readByte(); // filler4
            raFile.readByte();
        }

        fileInfo.minDAT = getInt(endianess); // 256
        fileInfo.cPhase = (short) getSignedShort(endianess); // 260
        fileInfo.swapPF = (short) getSignedShort(endianess); // 262
        fileInfo.pauseInterval = (short) getSignedShort(endianess); // 264

        if (lenImageHdr == 1044) {
            raFile.readByte(); // filler5
            raFile.readByte();
        }

        fileInfo.pauseTime = getFloat(endianess); // 266
        fileInfo.obliquePlane = getInt(endianess); // 270
        fileInfo.slocfov = getInt(endianess); // 274
        fileInfo.xmtFreq = getInt(endianess); // 278
        fileInfo.autoXmtFreq = getInt(endianess); // 282
        fileInfo.autoXmtGain = (short) getSignedShort(endianess); // 286
        fileInfo.prescan_r1 = (short) getSignedShort(endianess); // 288
        fileInfo.prescan_r2 = (short) getSignedShort(endianess); // 290

        if (lenImageHdr == 1044) {
            raFile.readByte(); // filler5
            raFile.readByte();
        }

        fileInfo.user_bitmap = getInt(endianess); // 292
        fileInfo.cenFreq = (short) getSignedShort(endianess); // 296
        fileInfo.iMode = (short) getSignedShort(endianess); // 298
        fileInfo.iOptions = getInt(endianess); // 300
        fileInfo.pSeq = (short) getSignedShort(endianess); // 304
        fileInfo.pulseSeqMode = (short) getSignedShort(endianess); // 306
        fileInfo.pulseSeqName = getString(33); // 308

        if (lenImageHdr == 1022) {
            raFile.readByte(); // filler2
        } else { // lenImageHdr == 1044
            raFile.readByte(); // filler7
            raFile.readByte();
            raFile.readByte();
        }

        fileInfo.psd_dateTime = getInt(endianess); // 342
        fileInfo.psd_iname = getString(13); // 346
        raFile.readByte(); // filler3 for lenImageHdr == 1022, filler8 for lenImageHdr == 1044
        fileInfo.coilType = (short) getSignedShort(endianess); // 360
        fileInfo.coilName = getString(17); // 362
        raFile.readByte(); // filler4 for lenImageHdr == 1022, filler9 for lenImageHdr == 1044
        fileInfo.surfaceCoilType = (short) getSignedShort(endianess); // 380
        fileInfo.surfcext = (short) getSignedShort(endianess); // 382

        if (lenImageHdr == 1044) {
            raFile.readByte(); // filler10
            raFile.readByte();
        }

        fileInfo.rawRunNum = getInt(endianess); // 384
        fileInfo.calFldStr = getInt(endianess); // 388
        fileInfo.supp_tech = (short) getSignedShort(endianess); // 392

        if (lenImageHdr == 1044) {
            raFile.readByte(); // filler11
            raFile.readByte();
        }

        fileInfo.vbw = getFloat(endianess); // 394
        fileInfo.slquant = (short) getSignedShort(endianess); // 398
        fileInfo.gpre = (short) getSignedShort(endianess); // 400
        fileInfo.intr_del = getInt(endianess); // 402
        fileInfo.user0 = getFloat(endianess); // 406
        fileInfo.user1 = getFloat(endianess); // 410
        fileInfo.user2 = getFloat(endianess); // 414
        fileInfo.user3 = getFloat(endianess); // 418
        fileInfo.user4 = getFloat(endianess); // 422
        fileInfo.user5 = getFloat(endianess); // 426
        fileInfo.user6 = getFloat(endianess); // 430
        fileInfo.user7 = getFloat(endianess); // 434
        fileInfo.user8 = getFloat(endianess); // 438
        fileInfo.user9 = getFloat(endianess); // 442
        fileInfo.user10 = getFloat(endianess); // 446
        fileInfo.user11 = getFloat(endianess); // 450
        fileInfo.user12 = getFloat(endianess); // 454
        fileInfo.user13 = getFloat(endianess); // 458
        fileInfo.user14 = getFloat(endianess); // 462
        fileInfo.user15 = getFloat(endianess); // 466
        fileInfo.user16 = getFloat(endianess); // 470
        fileInfo.user17 = getFloat(endianess); // 474
        fileInfo.user18 = getFloat(endianess); // 478
        fileInfo.user19 = getFloat(endianess); // 482
        fileInfo.user20 = getFloat(endianess); // 486
        fileInfo.user21 = getFloat(endianess); // 490
        fileInfo.user22 = getFloat(endianess); // 494
        fileInfo.projectAngle = getFloat(endianess); // 498
        fileInfo.user24 = getFloat(endianess); // 502
        fileInfo.im_alloc_key = getString(13); // 506

        if (lenImageHdr == 1022) {
            raFile.readByte(); // filler5
        } else { // lenImageHdr == 1044
            raFile.readByte(); // filler12
            raFile.readByte();
            raFile.readByte();
        }

        fileInfo.im_lastmod = getInt(endianess); // 520
        fileInfo.im_verscre = getString(2); // 524
        fileInfo.im_verscur = getString(2); // 526
        fileInfo.im_pds_a = getInt(endianess); // 528
        fileInfo.im_pds_c = getInt(endianess); // 532
        fileInfo.im_pds_u = getInt(endianess); // 536
        fileInfo.im_checksum = getInt(endianess); // 540
        fileInfo.im_archived = getInt(endianess); // 544
        fileInfo.im_complete = getInt(endianess); // 548
        fileInfo.satbits = (short) getSignedShort(endianess); // 552
        fileInfo.scic = (short) getSignedShort(endianess); // 554
        fileInfo.satxloc1 = (short) getSignedShort(endianess); // 556
        fileInfo.satxloc2 = (short) getSignedShort(endianess); // 558
        fileInfo.satyloc1 = (short) getSignedShort(endianess); // 560
        fileInfo.satyloc2 = (short) getSignedShort(endianess); // 562
        fileInfo.satzloc1 = (short) getSignedShort(endianess); // 564
        fileInfo.satzloc2 = (short) getSignedShort(endianess); // 566
        fileInfo.satxthick = (short) getSignedShort(endianess); // 568
        fileInfo.satythick = (short) getSignedShort(endianess); // 570
        fileInfo.satzthick = (short) getSignedShort(endianess); // 572
        fileInfo.flax = (short) getSignedShort(endianess); // 574
        fileInfo.venc = (short) getSignedShort(endianess); // 576
        fileInfo.thk_disclmr = (short) getSignedShort(endianess); // 578
        fileInfo.ps_flag = (short) getSignedShort(endianess); // 580
        fileInfo.ps_status = (short) getSignedShort(endianess); // 582
        fileInfo.image_type = (short) getSignedShort(endianess); // 584
        fileInfo.vas_collapse = (short) getSignedShort(endianess); // 586
        fileInfo.user23n = getFloat(endianess); // 588
        fileInfo.user24n = getFloat(endianess); // 592
        fileInfo.proj_alg = (short) getSignedShort(endianess); // 596
        fileInfo.proj_name = getString(13);
        raFile.readByte(); // filler6 for lenImageHdr == 1022, filler13 for lenImageHdr == 1044

        fileInfo.xAxisRot = getFloat(endianess); // 612
        fileInfo.yAxisRot = getFloat(endianess); // 616
        fileInfo.zAxisRot = getFloat(endianess); // 620
        fileInfo.threshMin1 = getInt(endianess); // 624
        fileInfo.threshMax1 = getInt(endianess); // 628
        fileInfo.threshMin2 = getInt(endianess); // 632
        fileInfo.threshMax2 = getInt(endianess); // 636
        fileInfo.ETL = (short) getSignedShort(endianess); // 640
        fileInfo.fracEcho = (short) getSignedShort(endianess); // 642
        fileInfo.prepPulse = (short) getSignedShort(endianess); // 644
        fileInfo.cPhaseNum = (short) getSignedShort(endianess); // 646
        fileInfo.varEcho = (short) getSignedShort(endianess); // 648
        fileInfo.refImg = getString(1); // 650
        fileInfo.sumImg = getString(1); // 651
        fileInfo.imgWindow = (short) getSignedShort(endianess); // 652
        fileInfo.imgLevel = (short) getSignedShort(endianess); // 654
        fileInfo.slop_int_1 = getInt(endianess); // 656
        fileInfo.slop_int_2 = getInt(endianess); // 660
        fileInfo.slop_int_3 = getInt(endianess); // 664
        fileInfo.slop_int_3 = getInt(endianess); // 668
        fileInfo.slop_int_4 = getInt(endianess); // 672
        fileInfo.slop_float_1 = getFloat(endianess); // 676
        fileInfo.slop_float_2 = getFloat(endianess); // 680
        fileInfo.slop_float_3 = getFloat(endianess); // 684
        fileInfo.slop_float_4 = getFloat(endianess); // 688
        fileInfo.slop_float_5 = getFloat(endianess); // 692
        fileInfo.slop_str_1 = getString(16); // 696
        fileInfo.slop_str_2 = getString(16); // 712
        fileInfo.scanAcqNum = (short) getSignedShort(endianess); // 728
        fileInfo.magWgtFlag = (short) getSignedShort(endianess); // 730
        fileInfo.vencScale = getFloat(endianess); // 732
        fileInfo.integrity = (short) getSignedShort(endianess); // 736

        if (lenImageHdr == 1044) {
            raFile.readByte(); // filler14
            raFile.readByte();
        }

        fileInfo.nPhase = getInt(endianess); // 738
        fileInfo.freqDir = (short) getSignedShort(endianess); // 742
        fileInfo.vasMode = (short) getSignedShort(endianess); // 744
        fileInfo.image_uid = getString(32); // 746
        fileInfo.sop_uid = getString(32); // 778
        getSignedShort(endianess); // dont_use_1
        getSignedShort(endianess); // dont_use_2
        getSignedShort(endianess); // dont_use_3
        fileInfo.preScanOpts = (short) getSignedShort(endianess); // 816
        fileInfo.gOffsetX = (short) getSignedShort(endianess); // 818
        fileInfo.gOffsetY = (short) getSignedShort(endianess); // 820
        fileInfo.gOffsetZ = (short) getSignedShort(endianess); // 822
        fileInfo.unOriginal = (short) getSignedShort(endianess); // 824
        fileInfo.nEPI = (short) getSignedShort(endianess); // 826
        fileInfo.effEchoSpace = (short) getSignedShort(endianess); // 828
        fileInfo.viewsPerSeg = (short) getSignedShort(endianess); // 830
        fileInfo.rbpm = (short) getSignedShort(endianess); // 832
        fileInfo.rtPoint = (short) getSignedShort(endianess); // 834
        fileInfo.rcvType = (short) getSignedShort(endianess); // 836
        fileInfo.dbdt = getFloat(endianess); // 838
        fileInfo.dbdtPer = getFloat(endianess); // 842
        fileInfo.estdbdtPer = getFloat(endianess); // 846
        fileInfo.estdbdtts = getFloat(endianess); // 850
        fileInfo.sarAvgHead = getFloat(endianess); // 854
        fileInfo.negScanSpacing = getFloat(endianess); // 858
        fileInfo.offsetFreq = getInt(endianess); // 862
        fileInfo.userUsageTag = getInt(endianess); // 866
        fileInfo.userFillMapMSW = getInt(endianess); // 870
        fileInfo.userFillMapLSW = getInt(endianess); // 874
        fileInfo.user25 = getFloat(endianess); // 878
        fileInfo.user26 = getFloat(endianess); // 882
        fileInfo.user27 = getFloat(endianess); // 886
        fileInfo.user28 = getFloat(endianess); // 890
        fileInfo.user29 = getFloat(endianess); // 894
        fileInfo.user30 = getFloat(endianess); // 898
        fileInfo.user31 = getFloat(endianess); // 902
        fileInfo.user32 = getFloat(endianess); // 906
        fileInfo.user33 = getFloat(endianess); // 910
        fileInfo.user34 = getFloat(endianess); // 914
        fileInfo.user35 = getFloat(endianess); // 918
        fileInfo.user36 = getFloat(endianess); // 922
        fileInfo.user37 = getFloat(endianess); // 926
        fileInfo.user38 = getFloat(endianess); // 930
        fileInfo.user39 = getFloat(endianess); // 934
        fileInfo.user40 = getFloat(endianess); // 938
        fileInfo.user41 = getFloat(endianess); // 942
        fileInfo.user42 = getFloat(endianess); // 946
        fileInfo.user43 = getFloat(endianess); // 950
        fileInfo.user44 = getFloat(endianess); // 954
        fileInfo.user45 = getFloat(endianess); // 958
        fileInfo.user46 = getFloat(endianess); // 962
        fileInfo.user47 = getFloat(endianess); // 966
        fileInfo.user48 = getFloat(endianess); // 970
        fileInfo.slop_int_6 = getInt(endianess); // 974
        fileInfo.slop_int_7 = getInt(endianess); // 978
        fileInfo.slop_int_8 = getInt(endianess); // 982
        fileInfo.slop_int_9 = getInt(endianess); // 986
        fileInfo.mr_padding = getString(32); // 990
        raFile.seek(imagePtr);
        readBuffer(buffer);
    }


    /**
     * DOCUMENT ME!
     *
     * @return  int size of image file data
     */
    public int readImageFileData() {

        try {

            if (checkMagicNumber() == true) {
                imagePtr = raFile.readInt() + startAdjust; // location 4
                width = raFile.readInt(); // location 8
                height = raFile.readInt(); // location 12
                depth = raFile.readInt(); // location 16
                compression = raFile.readInt(); // location 20

                raFile.seek(startAdjust + 148);

                int imageHeaderPtr = raFile.readInt();

                raFile.seek(imageHeaderPtr + 12);
                imageNumber = raFile.readShort();

                if (imageNumber == 0) {
                    imageNumber = 1;
                }

                if (compression > 1) {
                    return -1; // compression not supported
                }
            } else {
                return -2; /// not magic number
            }

            byteBuffer = new byte[2 * width * height];
        } catch (IOException e) {
            return -2;
        } catch (OutOfMemoryError e) {
            MipavUtil.displayError("Out of memory in FileGEGenesis.");
        }

        return (width * height);
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
            fileInfo = new FileInfoGESigna5X(fileName, fileDir, FileUtility.GE_GENESIS);
        } catch (OutOfMemoryError e) {
            MipavUtil.displayError("Out of memory in FileGEGenesis.setFileName.");
            throw new IOException();
        }

        fileInfo.setEndianess(BIG_ENDIAN);
        fileInfo.setFileName(fileName);
        fileInfo.setDataType(ModelImage.USHORT);
    }
    
    /**
     * Looks for the magic number "IMGF" in image header for GE Signa 5.X type file.
     * If present, the image is GE SIGNA 5.X format.
     *
     * @throws  IOException  Indicates error reading the file
     *
     * @return  boolean true if the magic number was found in the image header.
     */
    public boolean isGESigna5X() throws IOException {
    	
    	try {

            if (raFile != null) {
                raFile.close();
            }

            fileHeader = new File(fileDir + fileName);
            raFile = new RandomAccessFile(fileHeader, "r");
            
    	} catch (OutOfMemoryError e) {
            MipavUtil.displayError("Out of memory in FileGESigna5X.isGESigna5X.");
            throw new IOException();
    	}
    	if (raFile == null) {
            return false;
        }
    	
    	if(checkMagicNumber()== true){
    		return true;
    	} else {
    		return false;
    	}
   	}
      

    /**
     * DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    private boolean checkMagicNumber() {

        try {
            String tmpStr = getString(4);

            if (tmpStr.equals("IMGF")) {
                return true;
            } else {
                raFile.seek(3228);
                tmpStr = getString(4);

                if (tmpStr.equals("IMGF")) {
                    startAdjust = 3220;

                    return true;
                } else {
                    return false; // not a signa 5X file!
                }
            }
        } catch (IOException error) {
            return false;
        }
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
