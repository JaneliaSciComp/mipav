package gov.nih.mipav.model.file;


import gov.nih.mipav.model.file.FileInfoBase.Unit;
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

    @SuppressWarnings("unused")
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
        fileInfo.startAdjust = startAdjust;

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
        units[0] = Unit.MILLIMETERS.getLegacyNum();
        units[1] = Unit.MILLIMETERS.getLegacyNum();
        units[2] = Unit.MILLIMETERS.getLegacyNum();
        units[3] = Unit.UNKNOWN_MEASURE.getLegacyNum();
        units[4] = Unit.UNKNOWN_MEASURE.getLegacyNum();
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
            	startAdjust = 0;
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
    
    public void writeImage(final ModelImage image, final FileWriteOptions options) throws IOException {
        File file;
        boolean endianess;
        byte magicNumber[] = new byte[4];
        int j;
        short shortBuffer[] = null;
        byte byteBuffer[] = null;
        int sliceSize;
        long location;
        long bytesSkipped;
        int sBegin;
        int sEnd;
        int tBegin;
        int tEnd;
        int t;
        int z;
        int fileIndex;
        int imageIndex;
        int zDim;
        String indexStr;
        int lastPeriod;
        String fileBase;
        
        magicNumber[0] = 73; // I
        magicNumber[1] = 77; // M
        magicNumber[2] = 71; // G
        magicNumber[3] = 70; // F
        
        if (image.getNDims() >= 3) {
            sBegin = options.getBeginSlice();
            sEnd = options.getEndSlice();
            zDim = image.getExtents()[2];
        } else {
            sBegin = 0;
            sEnd = 0;
            zDim = 1;
        }

        if (image.getNDims() == 4) {
            tBegin = options.getBeginTime();
            tEnd = options.getEndTime();
        } else {
            tBegin = 0;
            tEnd = 0;
        }
        lastPeriod = fileName.lastIndexOf(".");
        fileBase = fileName.substring(0,lastPeriod+1);
        
        for (t = tBegin; t <= tEnd; t++) {
        for (z = sBegin; z <= sEnd; z++) {
        imageIndex = z + t*zDim;
        fileIndex = (z - sBegin + 1) + (t - tBegin)*(sEnd - sBegin + 1);
        indexStr = Integer.toString(fileIndex);
        while (indexStr.length() < 3) {
        	indexStr = "0" + indexStr;
        }
        
        if (image.getNDims() == 2) {
        	file = new File(fileDir + fileName);
        }
        else {
            file = new File(fileDir + fileBase + indexStr);
        }
        raFile = new RandomAccessFile(file, "rw");
        // Necessary so that if this is an overwritten file there isn't any
        // junk at the end
        raFile.setLength(0);
        
       // The data types are Sun, hence the byte order is big-endian.
        endianess = BIG_ENDIAN;
        try {
            fileInfo = (FileInfoGESigna5X)image.getFileInfo()[imageIndex];
        }
        catch (ClassCastException e) {
        	MipavUtil.displayError("Can only write GESigna5X files to disk");
        	return;
        }
        startAdjust = fileInfo.startAdjust;
        
        
        if (startAdjust > 0) {	
            byteBuffer = new byte[3228];
            raFile.write(byteBuffer);
        }      	
      
        raFile.write(magicNumber);
        writeInt(fileInfo.ptrImage-startAdjust, endianess); // 4
        writeInt(fileInfo.width,endianess); // 8
        writeInt(fileInfo.height,endianess); // 12
        sliceSize = fileInfo.width * fileInfo.height;
        writeInt(fileInfo.depth, endianess); // 16
        writeInt(fileInfo.compression,endianess); // 20
        byteBuffer = new byte[8];
        raFile.write(byteBuffer); // 24
        writeInt(fileInfo.valueBg, endianess); // 32
        byteBuffer = new byte[18]; 
        raFile.write(byteBuffer); // 36

        writeShort((short)fileInfo.checkSum,endianess); // 54
        writeInt(fileInfo.ptrUID - startAdjust, endianess); // 56
        writeInt(fileInfo.lenUID,endianess); // 60
        writeInt(fileInfo.ptrUnpackHdr - startAdjust, endianess); // 64
        writeInt(fileInfo.lenUnpackHdr, endianess); // 68
        writeInt(fileInfo.ptrCmprsnHdr - startAdjust,endianess); // 72
        writeInt(fileInfo.lenCmprsnHdr, endianess); // 76
        writeInt(fileInfo.ptrHistoHdr - startAdjust,endianess); // 80
        writeInt(fileInfo.lenHistoHdr,endianess); // 84
        writeInt(fileInfo.ptrTextPlane - startAdjust,endianess); // 88
        writeInt(fileInfo.lenTextPlane,endianess); // 92
        writeInt(fileInfo.ptrGraphics - startAdjust,endianess); // 96
        writeInt(fileInfo.lenGraphics,endianess); // 100
        writeInt(fileInfo.ptrDBHdr - startAdjust,endianess); // 104
        writeInt(fileInfo.lenDBHdr,endianess); // 108
        writeInt(fileInfo.addValue,endianess); // 112
        writeInt(fileInfo.ptrUsrDefData - startAdjust,endianess); // 116
        writeInt(fileInfo.lenUsrDefData,endianess); // 120

        if (startAdjust > 0) {
            location = raFile.getFilePointer();
            byteBuffer = new byte[(int)(fileInfo.ptrImage - location)];
            raFile.write(byteBuffer);
            // store as 16 bit signed short
            shortBuffer = new short[sliceSize];
            byteBuffer = new byte[2 * sliceSize];
            
            image.exportSliceXY(imageIndex, shortBuffer);

            for (j = 0; j < sliceSize; j++) {
                byteBuffer[2 * j] = (byte) (shortBuffer[j] >>> 8);
                byteBuffer[ (2 * j) + 1] = (byte) (shortBuffer[j]);
            }

            raFile.write(byteBuffer);
            raFile.close();   

            return;
        }

        writeInt(fileInfo.ptrSuiteHdr,endianess); // 124
        writeInt(fileInfo.lenSuiteHdr,endianess); // 128
        writeInt(fileInfo.ptrExamHdr,endianess); // 132
        writeInt(fileInfo.lenExamHdr,endianess); // 136

        writeInt(fileInfo.ptrSeriesHdr,endianess); // 140
        writeInt(fileInfo.lenSeriesHdr,endianess); // 144

        writeInt(fileInfo.ptrImageHdr,endianess); // 148
        writeInt(fileInfo.lenImageHdr,endianess); // 152
      
        // Write exam header data
        location = raFile.getFilePointer();
        if (raFile.length() >= fileInfo.ptrExamHdr) {
            raFile.seek(fileInfo.ptrExamHdr);
        }
        else {
        	bytesSkipped = raFile.length() - location;
        	raFile.seek(raFile.length());
        	byteBuffer = new byte[(int)(fileInfo.ptrExamHdr - (location+bytesSkipped))];
        	raFile.write(byteBuffer);
        }
        raFile.write((fileInfo.suiteID).getBytes()); // 0
        writeShort(fileInfo.exUniq,endianess); // 4
        raFile.write((fileInfo.exDiskID).getBytes()); // 6
        byteBuffer = new byte[1];
        raFile.write(byteBuffer); // 7 filler0
        writeShort((short)fileInfo.examNum, endianess); // 8
        raFile.write((fileInfo.hospName).getBytes()); // 10
        raFile.write(byteBuffer); // 43
        writeShort(fileInfo.detect,endianess); // 44

        if (fileInfo.lenExamHdr == 1040) {
            raFile.write(byteBuffer); // filler2
            raFile.write(byteBuffer);
        }

        writeInt((int)fileInfo.numCells,endianess); // 46
        writeFloat(fileInfo.zeroCell,endianess); // 50
        writeFloat(fileInfo.cellSpace,endianess); // 54
        writeFloat(fileInfo.srcToDet,endianess); // 58
        writeFloat(fileInfo.srcToIso,endianess); // 62
        writeShort(fileInfo.tubeType,endianess); // 66
        writeShort(fileInfo.dasType,endianess); // 68
        writeShort(fileInfo.numDcnK,endianess); // 70
        writeShort(fileInfo.dcnLen,endianess); // 72
        writeShort(fileInfo.dcnDensity,endianess); // 74
        writeShort(fileInfo.dcnStepSize,endianess); // 76
        writeShort(fileInfo.dcnShiftCnt,endianess); // 78

        if (fileInfo.lenExamHdr == 1040) {
            raFile.write(byteBuffer); // filler3
            raFile.write(byteBuffer);
        }

        writeInt(fileInfo.magStrength,endianess); // 80
        raFile.write((fileInfo.patientID).getBytes()); // 84
        raFile.write((fileInfo.patientName).getBytes()); // 97
        writeShort(fileInfo.patientAge,endianess); // 122
        writeShort(fileInfo.patian,endianess); // 124
        writeShort(fileInfo.patientSex,endianess); // 126
        writeInt(fileInfo.patWeight,endianess); // 128
        writeShort(fileInfo.trauma,endianess); // 132
        raFile.write((fileInfo.hist).getBytes()); // 134
        raFile.write((fileInfo.reqnum).getBytes()); // 195
        writeInt(fileInfo.exDateTime,endianess); // 208
        raFile.write((fileInfo.refPhy).getBytes()); // 212
        raFile.write((fileInfo.diagRad).getBytes()); // 245
        raFile.write((fileInfo.op).getBytes()); // 278
        raFile.write((fileInfo.exDesc).getBytes()); // 282
        raFile.write((fileInfo.examType).getBytes()); // 305
        writeShort(fileInfo.exFormat,endianess); // 308

        if (fileInfo.lenExamHdr == 1040) {
            raFile.write(byteBuffer); // filler4
            raFile.write(byteBuffer);
            raFile.write(byteBuffer);
            raFile.write(byteBuffer);
            raFile.write(byteBuffer);
            raFile.write(byteBuffer);
        }

        writeLong(fileInfo.firstAxTime,endianess); // 310
        raFile.write((fileInfo.exSysID).getBytes()); // 318

        if (fileInfo.lenExamHdr == 1024) {
            raFile.write(byteBuffer); // filler 2
        } else { // lenExamHdr == 1040
            raFile.write(byteBuffer); // filler 5
            raFile.write(byteBuffer);
            raFile.write(byteBuffer);
        }

        writeInt(fileInfo.exLastMod,endianess); // 328
        writeShort(fileInfo.protocolFlag,endianess); // 332
        raFile.write((fileInfo.exAllocKey).getBytes()); // 334
        raFile.write(byteBuffer); // filler 3 for lenExamHdr == 1024, filler 6 for lenExamHdr == 1040
        writeInt(fileInfo.exDeltaCount,endianess); // 348
        raFile.write((fileInfo.exVersCre).getBytes()); // 352
        raFile.write((fileInfo.exVersCur).getBytes()); // 354
        writeInt(fileInfo.exChecksum,endianess); // 356
        writeInt(fileInfo.exComplete,endianess); // 360
        writeInt(fileInfo.exSeriesCt,endianess); // 364
        writeInt(fileInfo.exNumArch,endianess); // 368
        writeInt(fileInfo.exNumSeries,endianess); // 372
        writeInt(fileInfo.exSeriesLen,endianess); // 376
        writeInt(fileInfo.exSeriesData,endianess); // 380
        writeInt(fileInfo.exNumUnSer,endianess); // 384
        writeInt(fileInfo.exUnSeriesLen,endianess); // 388
        writeInt(fileInfo.exUnSeriesData,endianess); // 392
        writeInt(fileInfo.exToArchCnt,endianess); // 396
        writeInt(fileInfo.exToArchiveLen,endianess); // 400
        writeInt(fileInfo.exToArchiveData,endianess); // 404
        writeInt(fileInfo.exProspCnt,endianess); // 408
        writeInt(fileInfo.exProspLen,endianess); // 412
        writeInt(fileInfo.exProspData,endianess); // 416
        writeInt(fileInfo.exModelNum,endianess); // 420
        writeInt(fileInfo.exModelCnt,endianess); // 424
        writeInt(fileInfo.exModelsLen,endianess); // 428
        writeInt(fileInfo.exModelsData,endianess); // 432
        writeShort(fileInfo.exStat,endianess); // 436
        raFile.write((fileInfo.uniqSysID).getBytes()); // 438
        raFile.write((fileInfo.serviceID).getBytes()); // 454
        raFile.write((fileInfo.mobileLoc).getBytes()); // 470
        raFile.write((fileInfo.studyUID).getBytes()); // 474
        writeShort(fileInfo.studyStatus,endianess); // 506
        raFile.write((fileInfo.exPadding).getBytes()); // 508

        // write series header data
        location = raFile.getFilePointer();
        if (raFile.length() > fileInfo.ptrSeriesHdr) {
            raFile.seek(fileInfo.ptrSeriesHdr);
        }
        else {
        	bytesSkipped = raFile.length() - location;
        	raFile.seek(raFile.length());
        	byteBuffer = new byte[(int)(fileInfo.ptrSeriesHdr - (location+bytesSkipped))];
        	raFile.write(byteBuffer);
        }
        raFile.write((fileInfo.seSuid).getBytes()); // 0
        writeShort(fileInfo.seUniq,endianess); // 4
        raFile.writeByte(fileInfo.seDiskID); // 6
        byteBuffer = new byte[1];
        raFile.write(byteBuffer); // 7 filler 0
        writeShort(fileInfo.seExamNo,endianess); // 8
        writeShort(fileInfo.seriesNum,endianess); // 10
        writeInt(fileInfo.seDateTime,endianess); // 12
        writeInt(fileInfo.seActualDT,endianess); // 16
        raFile.write((fileInfo.seDesc).getBytes()); // 20
        raFile.write((fileInfo.prSysID).getBytes()); // 50
        raFile.write((fileInfo.panSysID).getBytes()); // 59
        writeShort(fileInfo.seType,endianess); // 68
        writeShort(fileInfo.seSource,endianess); // 70
        writeShort(fileInfo.sePlane,endianess); // 72
        writeShort(fileInfo.scanType,endianess); // 74
        writeInt(fileInfo.position,endianess); // 76
        writeInt(fileInfo.entry,endianess); // 80
        raFile.write((fileInfo.anatomicalRef).getBytes()); // 84
        raFile.write(byteBuffer); // filler 1
        writeFloat(fileInfo.lmHor,endianess); // 88
        raFile.write((fileInfo.scanProtocolName).getBytes()); // 92
        raFile.write(byteBuffer); // filler 2
        writeShort(fileInfo.seContrast,endianess); // 118
        raFile.write((fileInfo.startRAS).getBytes()); // 120
        raFile.write(byteBuffer); // filler 3

        if (fileInfo.lenSeriesHdr == 1028) {
            raFile.write(byteBuffer);
            raFile.write(byteBuffer);
        }

        writeFloat(fileInfo.startLoc,endianess); // 122
        raFile.write((fileInfo.endRAS).getBytes()); // 126
        raFile.write(byteBuffer); // filler 4

        if (fileInfo.lenSeriesHdr == 1028) {
            raFile.write(byteBuffer);
            raFile.write(byteBuffer);
        }

        writeFloat(fileInfo.endLoc,endianess); // 128
        writeShort(fileInfo.sePseq,endianess); // 132
        writeShort(fileInfo.seSortOrder,endianess); // 134
        writeInt(fileInfo.seLandmarkCnt,endianess); // 136
        writeShort(fileInfo.seNacq,endianess); // 140
        writeShort(fileInfo.xBaseSt,endianess); // 142
        writeShort(fileInfo.xBaseEnd,endianess); // 144
        writeShort(fileInfo.xenhSt,endianess); // 146
        writeShort(fileInfo.xenhEnd,endianess); // 148

        if (fileInfo.lenSeriesHdr == 1028) {
            raFile.write(byteBuffer); // filler 5
            raFile.write(byteBuffer);
        }

        writeInt(fileInfo.seLastMod,endianess); // 150
        raFile.write((fileInfo.seAllocKey).getBytes()); // 154

        if (fileInfo.lenSeriesHdr == 1020) {
            raFile.write(byteBuffer); // filler 5
        } else { // lenSeriesHdr = 1028
            raFile.write(byteBuffer); // filler 6
            raFile.write(byteBuffer);
            raFile.write(byteBuffer);
        }

        writeInt(fileInfo.seDeltaCnt,endianess); // 168
        raFile.write((fileInfo.seVersCre).getBytes()); // 172
        raFile.write((fileInfo.seVersCur).getBytes()); // 174
        writeFloat(fileInfo.sePdsA,endianess); // 176
        writeFloat(fileInfo.sePdsC,endianess); // 180
        writeFloat(fileInfo.sePdsU,endianess); // 184
        writeInt(fileInfo.seChecksum,endianess); // 188
        writeInt(fileInfo.seComplete,endianess); // 192
        writeInt(fileInfo.seNumArch,endianess); // 196
        writeInt(fileInfo.seImageCt,endianess); // 200
        writeInt(fileInfo.seNumImages,endianess); // 204
        writeInt(fileInfo.imagesLen,endianess); // 208
        writeInt(fileInfo.imagesData,endianess); // 212
        writeInt(fileInfo.numUnImg,endianess); // 216
        writeInt(fileInfo.unImagesLen,endianess); // 220
        writeInt(fileInfo.unImagesData,endianess); // 224
        writeInt(fileInfo.toArchiveCnt,endianess); // 228
        writeInt(fileInfo.toArchiveLen,endianess); // 232
        writeInt(fileInfo.toArchiveData,endianess); // 236
        writeFloat(fileInfo.echo1Alpha,endianess); // 240
        writeFloat(fileInfo.echo1Beta,endianess); // 244
        writeShort(fileInfo.echo1Window,endianess); // 248
        writeShort(fileInfo.echo1Level,endianess); // 250
        writeFloat(fileInfo.echo2Alpha,endianess); // 252
        writeFloat(fileInfo.echo2Beta,endianess); // 256
        writeShort(fileInfo.echo2Window,endianess); // 260
        writeShort(fileInfo.echo2Level,endianess); // 262
        writeFloat(fileInfo.echo3Alpha,endianess); // 264
        writeFloat(fileInfo.echo3Beta,endianess); // 268
        writeShort(fileInfo.echo3Window,endianess); // 272
        writeShort(fileInfo.echo3Level,endianess); // 274
        writeFloat(fileInfo.echo4Alpha,endianess); // 276
        writeFloat(fileInfo.echo4Beta,endianess); // 280
        writeShort(fileInfo.echo4Window,endianess); // 284
        writeShort(fileInfo.echo4Level,endianess); // 286
        writeFloat(fileInfo.echo5Alpha,endianess); // 288
        writeFloat(fileInfo.echo5Beta,endianess); // 292
        writeShort(fileInfo.echo5Window,endianess); // 296
        writeShort(fileInfo.echo5Level,endianess); // 298
        writeFloat(fileInfo.echo6Alpha,endianess); // 300
        writeFloat(fileInfo.echo6Beta,endianess); // 304
        writeShort(fileInfo.echo6Window,endianess); // 308
        writeShort(fileInfo.echo6Level,endianess); // 310
        writeFloat(fileInfo.echo7Alpha,endianess); // 312
        writeFloat(fileInfo.echo7Beta,endianess); // 316
        writeShort(fileInfo.echo7Window,endianess); // 320
        writeShort(fileInfo.echo7Level,endianess); // 322
        writeFloat(fileInfo.echo8Alpha,endianess); // 324
        writeFloat(fileInfo.echo8Beta,endianess); // 328
        writeShort(fileInfo.echo8Window,endianess); // 332
        writeShort(fileInfo.echo8Level,endianess); // 334
        raFile.write((fileInfo.seriesUID).getBytes()); // 336
        raFile.write((fileInfo.landmarkUID).getBytes()); // 368
        raFile.write((fileInfo.equipmentUID).getBytes()); // 400
        raFile.write((fileInfo.sePadding).getBytes()); // 432

        // Image stuff
        location = raFile.getFilePointer();
        if (raFile.length()> fileInfo.ptrImageHdr) {
            raFile.seek(fileInfo.ptrImageHdr);
        }
        else {
        	bytesSkipped = raFile.length() - location;
        	raFile.seek(raFile.length());
        	byteBuffer = new byte[(int)(fileInfo.ptrImageHdr - (location+bytesSkipped))];
        	raFile.write(byteBuffer);
        }
        raFile.write((fileInfo.imgHdrSuiteID).getBytes()); // 0
        writeShort(fileInfo.uniq,endianess); // 4
        raFile.writeByte(fileInfo.diskid); // 6
        byteBuffer = new byte[1];
        raFile.write(byteBuffer); // filler0 // 7
        writeShort((short)fileInfo.imgHdrExamNum,endianess); // 8
        writeShort(fileInfo.imgHdrSeriesNum,endianess); // 10
        writeShort(fileInfo.imageNum,endianess); // 12

        if (fileInfo.lenImageHdr == 1044) {
            raFile.write(byteBuffer); // filler1
            raFile.write(byteBuffer);
        }

        writeInt(fileInfo.dateTime,endianess); // 14
        writeInt(fileInfo.actualDateTime,endianess); // 18
        writeFloat(fileInfo.scTime,endianess); // 22
        writeFloat(fileInfo.sliceThickness,endianess); // 26
        writeShort(fileInfo.matrixSizeX,endianess); // 30
        writeShort(fileInfo.matrixSizeY,endianess); // 32
        writeFloat(fileInfo.FOVX,endianess); // 34
        writeFloat(fileInfo.FOVY,endianess); // 38
        writeFloat(fileInfo.imageDimX,endianess); // 42
        writeFloat(fileInfo.imageDimY,endianess); // 46
        writeFloat(fileInfo.pixelResX,endianess); // 50
        writeFloat(fileInfo.pixelResY,endianess); // 54

        raFile.write((fileInfo.pixelID).getBytes()); // 58
        raFile.write((fileInfo.IVCntrstAgent).getBytes()); // 72
        raFile.write((fileInfo.OralCntrstAgent).getBytes()); // 89
        writeShort(fileInfo.contrastMode,endianess); // 106
        writeShort(fileInfo.serrx,endianess); // 108
        writeShort(fileInfo.imgrx,endianess); // 110
        writeShort(fileInfo.screenFormat,endianess); // 112
        writeShort(fileInfo.planeType,endianess); // 114

        if (fileInfo.lenImageHdr == 1044) {
            raFile.write(byteBuffer); // filler2
            raFile.write(byteBuffer);
        }

        writeFloat(fileInfo.scanSpacing,endianess); // 116
        writeShort(fileInfo.compress,endianess); // 120
        writeShort(fileInfo.scoutType,endianess); // 122
        raFile.write((fileInfo.loc_ras).getBytes()); // 124

        if (fileInfo.lenImageHdr == 1022) {
            raFile.write(byteBuffer); // filler1                         // 125
        } else { // lenImageHdr == 1044
            raFile.write(byteBuffer); // filler3
            raFile.write(byteBuffer);
            raFile.write(byteBuffer);
        }

        writeFloat(fileInfo.imgLoc,endianess); // 126
        writeFloat(fileInfo.imgCtrR,endianess); // 130
        writeFloat(fileInfo.imgCtrA,endianess); // 134
        writeFloat(fileInfo.imgCtrS,endianess); // 138
        writeFloat(fileInfo.norm_R,endianess); // 142
        writeFloat(fileInfo.norm_A,endianess); // 146
        writeFloat(fileInfo.norm_S,endianess); // 150

        writeFloat(fileInfo.imgTLHC_R,endianess); // 154
        writeFloat(fileInfo.imgTLHC_A,endianess); // 158
        writeFloat(fileInfo.imgTLHC_S,endianess); // 162
        writeFloat(fileInfo.imgTRHC_R,endianess); // 166
        writeFloat(fileInfo.imgTRHC_A,endianess); // 170
        writeFloat(fileInfo.imgTRHC_S,endianess); // 174
        writeFloat(fileInfo.imgBRHC_R,endianess); // 178
        writeFloat(fileInfo.imgBRHC_A,endianess); // 182
        writeFloat(fileInfo.imgBRHC_S,endianess); // 186

        raFile.write((fileInfo.forImgRev).getBytes()); // 190
        writeInt(fileInfo.pulseRepTime,endianess); // 194
        writeInt(fileInfo.inverTime,endianess); // 198
        writeInt(fileInfo.echoTime,endianess); // 202
        writeInt(fileInfo.te2,endianess); // 206
        writeShort(fileInfo.nEchoes,endianess); // 210
        writeShort(fileInfo.echoNum,endianess); // 212
        writeFloat(fileInfo.tableDelta,endianess); // 214
        writeFloat(fileInfo.NEX,endianess); // 218
        writeShort(fileInfo.contig,endianess); // 222
        writeShort(fileInfo.heartRate,endianess); // 224
        writeInt(fileInfo.tDel,endianess); // 226
        writeFloat(fileInfo.sarAvg,endianess); // 230
        writeFloat(fileInfo.sarPeak,endianess); // 234
        writeShort(fileInfo.monSar,endianess); // 238
        writeShort(fileInfo.trgWindow,endianess); // 240
        writeFloat(fileInfo.repTime,endianess); // 242
        writeShort(fileInfo.imgPCycle,endianess); // 246
        writeShort(fileInfo.xmtGain,endianess); // 248
        writeShort(fileInfo.rcvGain1,endianess); // 250
        writeShort(fileInfo.rcvGain2,endianess); // 252
        writeShort(fileInfo.mr_flip,endianess); // 254

        if (fileInfo.lenImageHdr == 1044) {
            raFile.write(byteBuffer); // filler4
            raFile.write(byteBuffer);
        }

        writeInt(fileInfo.minDAT,endianess); // 256
        writeShort(fileInfo.cPhase,endianess); // 260
        writeShort(fileInfo.swapPF,endianess); // 262
        writeShort(fileInfo.pauseInterval,endianess); // 264

        if (fileInfo.lenImageHdr == 1044) {
            raFile.write(byteBuffer); // filler5
            raFile.write(byteBuffer);
        }

        writeFloat(fileInfo.pauseTime,endianess); // 266
        writeInt(fileInfo.obliquePlane,endianess); // 270
        writeInt(fileInfo.slocfov,endianess); // 274
        writeInt(fileInfo.xmtFreq,endianess); // 278
        writeInt(fileInfo.autoXmtFreq,endianess); // 282
        writeShort(fileInfo.autoXmtGain,endianess); // 286
        writeShort(fileInfo.prescan_r1,endianess); // 288
        writeShort(fileInfo.prescan_r2,endianess); // 290

        if (fileInfo.lenImageHdr == 1044) {
            raFile.write(byteBuffer); // filler5
            raFile.write(byteBuffer);
        }

        writeInt(fileInfo.user_bitmap,endianess); // 292
        writeShort(fileInfo.cenFreq,endianess); // 296
        writeShort(fileInfo.iMode,endianess); // 298
        writeInt((int)fileInfo.iOptions,endianess); // 300
        writeShort(fileInfo.pSeq,endianess); // 304
        writeShort(fileInfo.pulseSeqMode,endianess); // 306
        raFile.write((fileInfo.pulseSeqName).getBytes()); // 308

        if (fileInfo.lenImageHdr == 1022) {
            raFile.write(byteBuffer); // filler2
        } else { // lenImageHdr == 1044
            raFile.write(byteBuffer); // filler7
            raFile.write(byteBuffer);
            raFile.write(byteBuffer);
        }

        writeInt(fileInfo.psd_dateTime,endianess); // 342
        raFile.write((fileInfo.psd_iname).getBytes()); // 346
        raFile.write(byteBuffer); // filler3 for lenImageHdr == 1022, filler8 for lenImageHdr == 1044
        writeShort(fileInfo.coilType,endianess); // 360
        raFile.write((fileInfo.coilName).getBytes()); // 362
        raFile.write(byteBuffer); // filler4 for lenImageHdr == 1022, filler9 for lenImageHdr == 1044
        writeShort(fileInfo.surfaceCoilType,endianess); // 380
        writeShort(fileInfo.surfcext,endianess); // 382

        if (fileInfo.lenImageHdr == 1044) {
            raFile.write(byteBuffer); // filler10
            raFile.write(byteBuffer);
        }

        writeInt(fileInfo.rawRunNum,endianess); // 384
        writeInt(fileInfo.calFldStr,endianess); // 388
        writeShort(fileInfo.supp_tech,endianess); // 392

        if (fileInfo.lenImageHdr == 1044) {
            raFile.write(byteBuffer); // filler11
            raFile.write(byteBuffer);
        }

        writeFloat(fileInfo.vbw,endianess); // 394
        writeShort(fileInfo.slquant,endianess); // 398
        writeShort(fileInfo.gpre,endianess); // 400
        writeInt((int)fileInfo.intr_del,endianess); // 402
        writeFloat(fileInfo.user0,endianess); // 406
        writeFloat(fileInfo.user1,endianess); // 410
        writeFloat(fileInfo.user2,endianess); // 414
        writeFloat(fileInfo.user3,endianess); // 418
        writeFloat(fileInfo.user4,endianess); // 422
        writeFloat(fileInfo.user5,endianess); // 426
        writeFloat(fileInfo.user6,endianess); // 430
        writeFloat(fileInfo.user7,endianess); // 434
        writeFloat(fileInfo.user8,endianess); // 438
        writeFloat(fileInfo.user9,endianess); // 442
        writeFloat(fileInfo.user10,endianess); // 446
        writeFloat(fileInfo.user11,endianess); // 450
        writeFloat(fileInfo.user12,endianess); // 454
        writeFloat(fileInfo.user13,endianess); // 458
        writeFloat(fileInfo.user14,endianess); // 462
        writeFloat(fileInfo.user15,endianess); // 466
        writeFloat(fileInfo.user16,endianess); // 470
        writeFloat(fileInfo.user17,endianess); // 474
        writeFloat(fileInfo.user18,endianess); // 478
        writeFloat(fileInfo.user19,endianess); // 482
        writeFloat(fileInfo.user20,endianess); // 486
        writeFloat(fileInfo.user21,endianess); // 490
        writeFloat(fileInfo.user22,endianess); // 494
        writeFloat(fileInfo.projectAngle,endianess); // 498
        writeFloat(fileInfo.user24,endianess); // 502
        raFile.write((fileInfo.im_alloc_key).getBytes()); // 506

        if (fileInfo.lenImageHdr == 1022) {
            raFile.write(byteBuffer); // filler5
        } else { // lenImageHdr == 1044
            raFile.write(byteBuffer); // filler12
            raFile.write(byteBuffer);
            raFile.write(byteBuffer);
        }

        writeInt(fileInfo.im_lastmod,endianess); // 520
        raFile.write((fileInfo.im_verscre).getBytes()); // 524
        raFile.write((fileInfo.im_verscur).getBytes()); // 526
        writeInt(fileInfo.im_pds_a,endianess); // 528
        writeInt(fileInfo.im_pds_c,endianess); // 532
        writeInt(fileInfo.im_pds_u,endianess); // 536
        writeInt(fileInfo.im_checksum,endianess); // 540
        writeInt(fileInfo.im_archived,endianess); // 544
        writeInt(fileInfo.im_complete,endianess); // 548
        writeShort(fileInfo.satbits,endianess); // 552
        writeShort(fileInfo.scic,endianess); // 554
        writeShort(fileInfo.satxloc1,endianess); // 556
        writeShort(fileInfo.satxloc2,endianess); // 558
        writeShort(fileInfo.satyloc1,endianess); // 560
        writeShort(fileInfo.satyloc2,endianess); // 562
        writeShort(fileInfo.satzloc1,endianess); // 564
        writeShort(fileInfo.satzloc2,endianess); // 566
        writeShort(fileInfo.satxthick,endianess); // 568
        writeShort(fileInfo.satythick,endianess); // 570
        writeShort(fileInfo.satzthick,endianess); // 572
        writeShort(fileInfo.flax,endianess); // 574
        writeShort(fileInfo.venc,endianess); // 576
        writeShort(fileInfo.thk_disclmr,endianess); // 578
        writeShort(fileInfo.ps_flag,endianess); // 580
        writeShort(fileInfo.ps_status,endianess); // 582
        writeShort(fileInfo.image_type,endianess); // 584
        writeShort(fileInfo.vas_collapse,endianess); // 586
        writeFloat(fileInfo.user23n,endianess); // 588
        writeFloat(fileInfo.user24n,endianess); // 592
        writeShort(fileInfo.proj_alg,endianess); // 596
        raFile.write((fileInfo.proj_name).getBytes());
        raFile.write(byteBuffer); // filler6 for lenImageHdr == 1022, filler13 for lenImageHdr == 1044

        writeFloat(fileInfo.xAxisRot,endianess); // 612
        writeFloat(fileInfo.yAxisRot,endianess); // 616
        writeFloat(fileInfo.zAxisRot,endianess); // 620
        writeInt(fileInfo.threshMin1,endianess); // 624
        writeInt(fileInfo.threshMax1,endianess); // 628
        writeInt(fileInfo.threshMin2,endianess); // 632
        writeInt(fileInfo.threshMax2,endianess); // 636
        writeShort(fileInfo.ETL,endianess); // 640
        writeShort(fileInfo.fracEcho,endianess); // 642
        writeShort(fileInfo.prepPulse,endianess); // 644
        writeShort(fileInfo.cPhaseNum,endianess); // 646
        writeShort(fileInfo.varEcho,endianess); // 648
        raFile.write((fileInfo.refImg).getBytes()); // 650
        raFile.write((fileInfo.sumImg).getBytes()); // 651
        writeShort(fileInfo.imgWindow,endianess); // 652
        writeShort(fileInfo.imgLevel,endianess); // 654
        writeInt(fileInfo.slop_int_1,endianess); // 656
        writeInt(fileInfo.slop_int_2,endianess); // 660
        writeInt(fileInfo.slop_int_3,endianess); // 664
        writeInt(fileInfo.slop_int_3,endianess); // 668
        writeInt(fileInfo.slop_int_4,endianess); // 672
        writeFloat(fileInfo.slop_float_1,endianess); // 676
        writeFloat(fileInfo.slop_float_2,endianess); // 680
        writeFloat(fileInfo.slop_float_3,endianess); // 684
        writeFloat(fileInfo.slop_float_4,endianess); // 688
        writeFloat(fileInfo.slop_float_5,endianess); // 692
        raFile.write((fileInfo.slop_str_1).getBytes()); // 696
        raFile.write((fileInfo.slop_str_2).getBytes()); // 712
        writeShort(fileInfo.scanAcqNum,endianess); // 728
        writeShort(fileInfo.magWgtFlag,endianess); // 730
        writeFloat(fileInfo.vencScale,endianess); // 732
        writeShort(fileInfo.integrity,endianess); // 736

        if (fileInfo.lenImageHdr == 1044) {
            raFile.write(byteBuffer); // filler14
            raFile.write(byteBuffer);
        }

        writeInt(fileInfo.nPhase,endianess); // 738
        writeShort(fileInfo.freqDir,endianess); // 742
        writeShort(fileInfo.vasMode,endianess); // 744
        raFile.write((fileInfo.image_uid).getBytes()); // 746
        raFile.write((fileInfo.sop_uid).getBytes()); // 778
        writeShort((short)0,endianess); // dont_use_1
        writeShort((short)0,endianess); // dont_use_2
        writeShort((short)0,endianess); // dont_use_3
        writeShort(fileInfo.preScanOpts,endianess); // 816
        writeShort(fileInfo.gOffsetX,endianess); // 818
        writeShort(fileInfo.gOffsetY,endianess); // 820
        writeShort(fileInfo.gOffsetZ,endianess); // 822
        writeShort(fileInfo.unOriginal,endianess); // 824
        writeShort(fileInfo.nEPI,endianess); // 826
        writeShort(fileInfo.effEchoSpace,endianess); // 828
        writeShort(fileInfo.viewsPerSeg,endianess); // 830
        writeShort(fileInfo.rbpm,endianess); // 832
        writeShort(fileInfo.rtPoint,endianess); // 834
        writeShort(fileInfo.rcvType,endianess); // 836
        writeFloat(fileInfo.dbdt,endianess); // 838
        writeFloat(fileInfo.dbdtPer,endianess); // 842
        writeFloat(fileInfo.estdbdtPer,endianess); // 846
        writeFloat(fileInfo.estdbdtts,endianess); // 850
        writeFloat(fileInfo.sarAvgHead,endianess); // 854
        writeFloat(fileInfo.negScanSpacing,endianess); // 858
        writeInt(fileInfo.offsetFreq,endianess); // 862
        writeInt(fileInfo.userUsageTag,endianess); // 866
        writeInt(fileInfo.userFillMapMSW,endianess); // 870
        writeInt(fileInfo.userFillMapLSW,endianess); // 874
        writeFloat(fileInfo.user25,endianess); // 878
        writeFloat(fileInfo.user26,endianess); // 882
        writeFloat(fileInfo.user27,endianess); // 886
        writeFloat(fileInfo.user28,endianess); // 890
        writeFloat(fileInfo.user29,endianess); // 894
        writeFloat(fileInfo.user30,endianess); // 898
        writeFloat(fileInfo.user31,endianess); // 902
        writeFloat(fileInfo.user32,endianess); // 906
        writeFloat(fileInfo.user33,endianess); // 910
        writeFloat(fileInfo.user34,endianess); // 914
        writeFloat(fileInfo.user35,endianess); // 918
        writeFloat(fileInfo.user36,endianess); // 922
        writeFloat(fileInfo.user37,endianess); // 926
        writeFloat(fileInfo.user38,endianess); // 930
        writeFloat(fileInfo.user39,endianess); // 934
        writeFloat(fileInfo.user40,endianess); // 938
        writeFloat(fileInfo.user41,endianess); // 942
        writeFloat(fileInfo.user42,endianess); // 946
        writeFloat(fileInfo.user43,endianess); // 950
        writeFloat(fileInfo.user44,endianess); // 954
        writeFloat(fileInfo.user45,endianess); // 958
        writeFloat(fileInfo.user46,endianess); // 962
        writeFloat(fileInfo.user47,endianess); // 966
        writeFloat(fileInfo.user48,endianess); // 970
        writeInt(fileInfo.slop_int_6,endianess); // 974
        writeInt(fileInfo.slop_int_7,endianess); // 978
        writeInt(fileInfo.slop_int_8,endianess); // 982
        writeInt(fileInfo.slop_int_9,endianess); // 986
        raFile.write((fileInfo.mr_padding).getBytes()); // 990
        location = raFile.getFilePointer();
        if (raFile.length() > fileInfo.ptrImage) {
            raFile.seek(fileInfo.ptrImage);
        }
        else {
        	bytesSkipped = raFile.length() - location;
        	raFile.seek(raFile.length());
        	byteBuffer = new byte[(int)(fileInfo.ptrImage - (location+bytesSkipped))];
        	raFile.write(byteBuffer);
        }
     
        // store as 16 bit signed short
        shortBuffer = new short[sliceSize];
        byteBuffer = new byte[2 * sliceSize];
        
        image.exportSliceXY(imageIndex, shortBuffer);

        for (j = 0; j < sliceSize; j++) {
            byteBuffer[2 * j] = (byte) (shortBuffer[j] >>> 8);
            byteBuffer[ (2 * j) + 1] = (byte) (shortBuffer[j]);
        }

        raFile.write(byteBuffer);
        raFile.close();
        } // for (z = sBegin; z <= sEnd; z++)
        } // for (t = tBegin; t <= tEnd; t++)
        
        return;
        
    }


}
