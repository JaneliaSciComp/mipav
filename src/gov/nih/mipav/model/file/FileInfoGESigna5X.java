package gov.nih.mipav.model.file;


import gov.nih.mipav.model.algorithms.*;
import gov.nih.mipav.model.structures.*;
import gov.nih.mipav.view.dialogs.*;


/**
 * This structures contains the information that describes how a GE Signa 5X image is stored on disk.
 */

public class FileInfoGESigna5X extends FileInfoBase {

    //~ Static fields/initializers -------------------------------------------------------------------------------------

    /** Use serialVersionUID for interoperability. */
    private static final long serialVersionUID = -4456298776612648834L;

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** 18. */
    public int actualDateTime;

    /** 112 - value to add to pixels. */
    public int addValue;

    /** 84 - 3 bytes - Anatomical Reference. */
    public String anatomicalRef;

    /** 282 Auto Center Frequency (0.1 Hz). */
    public int autoXmtFreq;

    /** 286 Auto Transmit Gain (0.1 dB). */
    public short autoXmtGain;

    /** 388 Calibrated Field Strength (x10 uGauss). */
    public int calFldStr;

    /** 54 Cell spacing. */
    public float cellSpace;

    /** 296 Center frequency method. */
    public short cenFreq;

    /** 54 - 16 bit end around carry sum of pixels **** (read as unsigned short). */
    public int checkSum;

    /** 362 17 characters. */
    public String coilName;

    /** 360. */
    public short coilType;

    /** 120 image compression type for allocation. */
    public short compress;

    /** 20 0 as is 1 retangular 2 packed 3 compressed 4 compressed and packed. */
    public int compression;

    /** 222 Continuous slices flag. */
    public short contig;

    /** 106. */
    public short contrastMode;

    /** 260 Total cardiac phase prescribed. */
    public short cPhase;

    /** 646 Cardiac phase number. */
    public short cPhaseNum;

    /** 68 DAS type. */
    public short dasType;

    /** 14 allocation date time. */
    public int dateTime;

    /** 838 peak rate of change in gradient field, tesla&#47sec. */
    public float dbdt;

    /** 842 limit in units of percent of theoretical curve. */
    public float dbdtPer;

    /** 74 Decon kernel density. */
    public short dcnDensity;

    /** 72 Number of elements in Decon kernel. */
    public short dcnLen;

    /** 78 Decon kernel shift count. */
    public short dcnShiftCnt;

    /** 76 Decon kernel stepsize. */
    public short dcnStepSize;

    /** 16 - number of bits. */
    public int depth;

    /** 44 detector type. */
    public short detect;

    /** 245 - 33 bytes Diagnostician&#47Radiologist. */
    public String diagRad;

    /** 6 - (internal use) Disk ID. */
    public byte diskid;

    /** 240. */
    public float echo1Alpha;

    /** 244. */
    public float echo1Beta;

    /** 250. */
    public short echo1Level;

    /** 248. */
    public short echo1Window;

    /** 252. */
    public float echo2Alpha;

    /** 256. */
    public float echo2Beta;

    /** 262. */
    public short echo2Level;

    /** 260. */
    public short echo2Window;

    /** 264. */
    public float echo3Alpha;

    /** 268. */
    public float echo3Beta;

    /** 274. */
    public short echo3Level;

    /** 272. */
    public short echo3Window;

    /** 276. */
    public float echo4Alpha;

    /** 280. */
    public float echo4Beta;

    /** 286. */
    public short echo4Level;

    /** 284. */
    public short echo4Window;

    /** 288. */
    public float echo5Alpha;

    /** 292. */
    public float echo5Beta;

    /** 298. */
    public short echo5Level;

    /** 296. */
    public short echo5Window;

    /** 300. */
    public float echo6Alpha;

    /** 304. */
    public float echo6Beta;

    /** 310. */
    public short echo6Level;

    /** 308. */
    public short echo6Window;

    /** 312. */
    public float echo7Alpha;

    /** 316. */
    public float echo7Beta;

    /** 322. */
    public short echo7Level;

    /** 320. */
    public short echo7Window;

    /** 324. */
    public float echo8Alpha;

    /** 328. */
    public float echo8Beta;

    /** 334. */
    public short echo8Level;

    /** 332. */
    public short echo8Window;

    /** 212 echo number. */
    public short echoNum;

    /** 202 pulse echo time (usec). */
    public int echoTime;

    /** 828 effective echo spacing for EPI. */
    public short effEchoSpace;

    // filler4
    /** 128 - Last scan location (L&#47S). */
    public float endLoc;

    /** 126 - 1 byte - RAS letter for last scan location (L&#47S). */
    public String endRAS;

    /** 80 - Patient Entry. */
    public int entry;

    /** 400 - 32 bytes. */
    public String equipmentUID;

    /** 846 PSD estimated limit in units of percent. */
    public float estdbdtPer;

    /** 850 PSD estimated limit in Tesla&#47sec. */
    public float estdbdtts;

    /** 640 Echo train length. */
    public short ETL;

    /** 334 - 13 bytes Process that allocated this record. */
    public String exAllocKey;

    /** 8 - Read as unsigned short. */
    public int examNum;

    /** 305 - 3 bytes i.e. &quotMR&quot or &quotCT&quot */
    public String examType;

    /** 356 (internal use) Exam Record Checksum. */
    public int exChecksum;

    /** 360 (internal use) Exam complete flag. */
    public int exComplete;

    /** 208 - Exam date/time stamp. */
    public int exDateTime;

    /** 348 (internal use) Number of updates to header. */
    public int exDeltaCount;

    /** 282 - 23 bytes Exam Description. */
    public String exDesc;

    /** 6 - 1 byte (internal use) Disk ID for this exam. */
    public String exDiskID;

    /** 308 Exam format. */
    public short exFormat;

    /** 328 Date&#47Time of Last Change. */
    public int exLastMod;

    /** 424 (internal use) Number of 3D Models. */
    public int exModelCnt;

    /** 420 (internal use) Last Model Number Used. */
    public int exModelNum;

    /** 432. */
    public int exModelsData;

    /** 428. */
    public int exModelsLen;

    /** 368 (internal use) Number of Series Archived. */
    public int exNumArch;

    /** 372 (internal use) Number of Series Existing. */
    public int exNumSeries;

    /** 384 (internal use) Number of Unstored series. */
    public int exNumUnSer;

    /** 508 - 516 bytes. */
    public String exPadding;

    // Series header for 1020 bytes, 1028 identical except for 8 more padding bytes

    /** 408 (internal use) number of Prosp&#47Scout Series. */
    public int exProspCnt;

    /** 416. */
    public int exProspData;

    /** 412 (internal use) Prosp&#47Scout Sers Keys in Exam. */
    public int exProspLen;

    /** 364 (innternl use) Last Series Number Used. */
    public int exSeriesCt;

    /** 380. */
    public int exSeriesData;

    /** 376 (internal use) Series keys for this exam. */
    public int exSeriesLen;

    /** 436 Patient status. */
    public short exStat;

    /** 318 - 9 bytes Creator suite and host. */
    public String exSysID;

    /** 396 (internal use) number of Unarchived Series. */
    public int exToArchCnt;

    /** 404. */
    public int exToArchiveData;

    /** 400 (internal use) Unarchived series Keys in Exam. */
    public int exToArchiveLen;

    /** 4 (internal use) The Make_Unique Flag. */
    public short exUniq;

    /** 392. */
    public int exUnSeriesData;

    /** 388 (internal use) Unstored Sers Keys in Exam. */
    public int exUnSeriesLen;

    /** 352 - 2 bytes Genesis Version - Created. */
    public String exVersCre;

    /** 354 - 2 bytes Genesis Version - Now. */
    public String exVersCur;

    /** 310 Start time(secs) of first axial in exam. */
    public long firstAxTime;

    /** 574 Phase contrast flow axis. */
    public short flax;

    // For MR header continues (1022 bytes)
    /** 190 foreign image revision. */
    public String forImgRev;

    /** 34. */
    public float FOVX;

    /** 38. */
    public float FOVY;

    /** 642 Fractional echo-effective TE flag. */
    public short fracEcho;

    /** 742 Frequency Direction. */
    public short freqDir;

    /** 818 gradient offset in x-direction. */
    public short gOffsetX;

    /** 820 gradient offset in y-direction. */
    public short gOffsetY;

    /** 822 gradient offset in z-direction. */
    public short gOffsetZ;

    /** 400 Graphically prescribed. */
    public short gpre;

    /** 224 cardiac heart rate (beats per minute). */
    public short heartRate;

    /** 12 - height of image. */
    public int height;

    /** 134 - 61 bytes Patient history. */
    public String hist;

    /** 10 - 33 characters hospital name. */
    public String hospName;

    /** 506 13 characters Process that allocated this record. */
    public String im_alloc_key;

    /** 544 (internal use) Image Archive Flag. */
    public int im_archived;

    /** 540 (internal use) AcqRecon record checksum. */
    public int im_checksum;

    /** 548 (Internal use) Image Complete Flag. */
    public int im_complete;

    /** 520. */
    public int im_lastmod;

    /** 528 PixelData size - as stored. */
    public int im_pds_a;

    /** 532 PixelData size - compressed. */
    public int im_pds_c;

    /** 536 PixelData size - Uncompressed. */
    public int im_pds_u;

    /** 524 2 characters Genesis Version - Created. */
    public String im_verscre;

    /** 526 2 characters Genesis Version - Now. */
    public String im_verscur;

    /** 584 Magnitude, Phase, Imaginary, or Real. */
    public short image_type;

    /** 746 32 characters Image Unique ID. */
    public String image_uid;

    /** 42. */
    public float imageDimX;

    /** 46. */
    public float imageDimY;

    /** 12. */
    public short imageNum;

    /** 212. */
    public int imagesData;

    /** 208 - (internal use) Image keys for this Series. */
    public int imagesLen;

    /** 182 Bottom Right Hand Corner Anterior. */
    public float imgBRHC_A;

    /** 178 Bottom Right Hand Corner Right. */
    public float imgBRHC_R;

    /** 186 Bottom Right Hand Corner Superior. */
    public float imgBRHC_S;

    /** 134 Anterior(i.e. X positive to anterior) */
    public float imgCtrA;

    /** 130 Right (i.e. X positive to right) */
    public float imgCtrR;

    /** 138 Superior(i.e. X positive to superior) */
    public float imgCtrS;

    /** 8 - Read as unsigned short. */
    public int imgHdrExamNum;

    /** 10. */
    public short imgHdrSeriesNum;


    // Image header for 1022 bytes, 1044 identical except for 22 more padding bytes */
    /** 0 - 4 bytes - suite ID. */
    public String imgHdrSuiteID;

    /** 654 Level value. */
    public short imgLevel;

    /** 126. */
    public float imgLoc;

    /** 246 Images per cardiac .1 cycle. */
    public short imgPCycle;

    /** 110 - image from which prescribed. */
    public short imgrx;

    /** 158 Top Left Hand Corner Anterior. */
    public float imgTLHC_A;

    /** 154 Top Left Hand Corner Right. */
    public float imgTLHC_R;

    /** 162 Top Left Hand Corner Superior. */
    public float imgTLHC_S;

    /** 170 Top Right Hand Corner Anterior. */
    public float imgTRHC_A;

    /** 166 Top Right Hand Corner Right. */
    public float imgTRHC_R;

    /** 174 Top Right Hand Corner Superior. */
    public float imgTRHC_S;

    /** 652 Window value. */
    public short imgWindow;

    /** 298 Imaging mode. */
    public short iMode;

    /** 736 GE Image Integrity. */
    public short integrity;

    /** 402 Interimage&#47interloc delay (uSec). */
    public long intr_del;

    /** 198 pulse inversion time (usec). */
    public int inverTime;

    /** 300 Imaging options. */
    public long iOptions;

    /** 72 - 17 bytes. */
    public String IVCntrstAgent;

    /** 368 - 32 bytes. */
    public String landmarkUID;

    /** 76 - length of. */
    public int lenCmprsnHdr;

    /** 108 - length of. */
    public int lenDBHdr;

    /** 136 - length of. */
    public int lenExamHdr;

    /** 100 - length of. */
    public int lenGraphics;

    /** 84 - length of. */
    public int lenHistoHdr;

    /** 152 - length of. */
    public int lenImageHdr;

    /** 144 - length of. */
    public int lenSeriesHdr;

    /** 128 - length of. */
    public int lenSuiteHdr;

    /** 92 - length of. */
    public int lenTextPlane;

    /** 60 - length of UID. */
    public int lenUID;

    /** 68 - length of. */
    public int lenUnpackHdr;

    /** 120 - length of. */
    public int lenUsrDefData;

    // filler1
    /** 88 - Horizontal landmark. */
    public float lmHor;

    /** 124 1 byte RAS letter of image location. */
    public String loc_ras;

    /** 0x494d4746 or "IMGF" or 1229801286. */
    public int magicNumber;

    /** 80 Magnet strength (in gauss). */
    public int magStrength;

    /** 730 Magnitude Weighting Flag. */
    public short magWgtFlag;

    /** 30. */
    public short matrixSizeX;

    /** 32. */
    public short matrixSizeY;

    /** 256 minimum delay after trigger (usec). */
    public int minDAT;

    /** 470 - 4 bytes. */
    public String mobileLoc;

    /** 238 Monitor SAR Flag. */
    public short monSar;

    /** 254 Flip angle for grass scans (degrees). */
    public short mr_flip;

    /** 990 spare space. */
    public String mr_padding;

    /** 210 number of echoes. */
    public short nEchoes;

    /** 858 Negative scan spacing for overlap slices. */
    public float negScanSpacing;

    /** 826 number of EPI shots. */
    public short nEPI;

    /** 218 Number of excitations. */
    public float NEX;

    /** 146 Normal A coordinate. */
    public float norm_A;

    /** 142 Normal R coordinate. */
    public float norm_R;

    /** 150 Normal S coordinate. */
    public float norm_S;

    /** 738 Number of Phases. */
    public int nPhase;

    /** 46 Number of cells in det. */
    public long numCells;

    /** 70 Number of Decon kernels. */
    public short numDcnK;

    /** 216 - (internal use) Number of Unstored Images. */
    public int numUnImg;

    /** 270. */
    public int obliquePlane;

    /** 862 offset Frequency_Mag. Transfer */
    public int offsetFreq;

    /** 278 - 4 bytes operator. */
    public String op;

    /** 89 - 17 bytes. */
    public String OralCntrstAgent;

    /** 59 - 9 bytes - Archiver Suite and Host. */
    public String panSysID;

    /** 124 Patient Age notation. */
    public short patian;

    /** 122. */
    public short patientAge;

    /** 84 - 13 bytes. */
    public String patientID;

    /** 97 - 25 bytes. */
    public String patientName;

    /** 126. */
    public short patientSex;

    /** 128 Patient Weight. */
    public int patWeight;

    /** 264 Pause interval (slices). */
    public short pauseInterval;

    /** 266. */
    public float pauseTime;

    /** 58 - 14 bytes. */
    public String pixelID;

    /** 50. */
    public float pixelResX;

    /** 54. */
    public float pixelResY;

    /** 114. */
    public short planeType;

    /** 76 - Patient Poisition. */
    public int position;

    /** 644 Prepartory pulse option. */
    public short prepPulse;

    /** 288 PreScan R1-Analog. */
    public short prescan_r1;

    /** 290 PreScan R2-DIgital. */
    public short prescan_r2;

    /** 816 bitmap of prescan options. */
    public short preScanOpts;

    /** 596 Projection Algorithm. */
    public short proj_alg;

    /** 598 13 characters Projection Algorithm Name. */
    public String proj_name;

    /** 498 Projection Angle. */
    public float projectAngle;

    /** 332 Non-zero indicates protocol exam. */
    public short protocolFlag;

    /** 50 - 9 bytes - Primary Receiver Suite and Host. */
    public String prSysID;

    /** 580 Auto&#47Manual Prescan flag. */
    public short ps_flag;

    /** 582 Bitmap of changed values. */
    public short ps_status;

    /** 342 PSD Creation Date and Time. */
    public int psd_dateTime;

    /** 346 13 characters. */
    public String psd_iname;

    /** 304 Pulse sequence. */
    public short pSeq;

    /** 72 - pointer to. */
    public int ptrCmprsnHdr;

    /** 104 - pointer to. */
    public int ptrDBHdr;

    /** 132 - pointer to. */
    public int ptrExamHdr;

    /** 96 - pointer to. */
    public int ptrGraphics;

    /** 80 - pointer to. */
    public int ptrHistoHdr;

    /** 4 - byte displacement to pixel data. */
    public int ptrImage;

    /** 148 - pointer to. */
    public int ptrImageHdr;

    /** 140 - pointer to. */
    public int ptrSeriesHdr;

    /** 124 - pointer to. */
    public int ptrSuiteHdr;

    /** 88 - pointer to. */
    public int ptrTextPlane;

    /** 56 - pointer to unique image identifier (UID). */
    public int ptrUID;

    /** 64 - pointer to. */
    public int ptrUnpackHdr;

    /** 116 - pointer to. */
    public int ptrUsrDefData;

    /** 194 pulse repetition time (usec). */
    public int pulseRepTime;

    /** 306. */
    public short pulseSeqMode;

    /** 308 33 characters. */
    public String pulseSeqName;

    /** 384 RawData Run Number. */
    public int rawRunNum;

    /** 832 respiratory rate, breaths per minute. */
    public short rbpm;

    /** 250 Actual receive gain analog (.1db). */
    public short rcvGain1;

    /** 252 Actual receive gain digital (.1db). */
    public short rcvGain2;

    /** 836 type of receiver used. */
    public short rcvType;

    /** 650 Reference image field. */
    public String refImg;

    /** 212 - 33 bytes Referring Physician. */
    public String refPhy;

    /** 242 Cardiac repertition time. */
    public float repTime;

    /** 195 - 13 bytes Requisition Number. */
    public String reqnum;

    /** 834 repiratory trigger point as percent of max. */
    public short rtPoint;

    /** 230 Average SAR. */
    public float sarAvg;

    /** 854 Avg head SAR. */
    public float sarAvgHead;

    /** 234 Peak SAR. */
    public float sarPeak;

    /** 552 Bitmap of SAT selections. */
    public short satbits;

    /** 556 R-side pulse rel to lndmrk. */
    public short satxloc1;

    /** 558 L-side pulse rel to lndmrk. */
    public short satxloc2;

    /** 568 Thickness of X-axis SAT pulse. */
    public short satxthick;

    /** 560 A-side SAT pulse rel to lndmrk. */
    public short satyloc1;

    /** 562 P-side SAT pulse rel to lndmrk. */
    public short satyloc2;

    /** 570 Thickness of Y-axis SAT pulse. */
    public short satythick;

    /** 564 S-side SAT pulse rel to lndmrk. */
    public short satzloc1;

    /** 566 I-side SAT pulse rel to lndmrk. */
    public short satzloc2;

    /** 572 Thickness of Z-axis SAT pulse. */
    public short satzthick;

    /** 728 Scan Acquisition Number. */
    public short scanAcqNum;

    /** 92 - 25 bytes - Scan Protocol Name. */
    public String scanProtocolName;

    /** 116 Spacing between scans (mm?). */
    public float scanSpacing;

    /** 74 - Scout or Axial (for CT). */
    public short scanType;

    /** 554 Surface Coil Intensity Correction Flag. */
    public short scic;

    /** 122 Scout Type (AP or lateral). */
    public short scoutType;

    /** 112 - (8&#4716) bits. */
    public short screenFormat;

    /** 22 scan duration in seconds. */
    public float scTime;

    // secs2date required
    /** 16 - Actual Series Date&#47Time Stamp. */
    public int seActualDT;

    /** 154 - 13 bytes - Process that allocated this record. */
    public String seAllocKey;

    /** 188 - (internal use) Series Record checksum. */
    public int seChecksum;

    /** 192 - (internal use) Series Complete Flag. */
    public int seComplete;

    // filler2
    /** 118 - greater than zero if image used contrast (L&#47S). */
    public short seContrast;

    // secs2date required
    /** 12 - Allocation series Date&#47Time Stamp. */
    public int seDateTime;

    // filler5
    /** 168 - (internal use) number of updates to header. */
    public int seDeltaCnt;

    /** 20 - 30 bytes - Series Description. */
    public String seDesc;

    /** 6 - Disk ID for this series - GE internal. */
    public byte seDiskID;

    // filler0
    /** 8 - Exam number. */
    public short seExamNo;

    /** 200 - (internal use) Last Image Number Used. */
    public int seImageCt;

    /** 136 - Landmark counter. */
    public int seLandmarkCnt;

    // secs2date required
    /** 150 - Date&#47Time of last change. */
    public int seLastMod;

    /** 140 - Number of acquisitions. */
    public short seNacq;

    /** 196 - (internal use) Number of Images Archived. */
    public int seNumArch;

    /** 204 - (internal use) Number of Images Existing. */
    public int seNumImages;

    /** 432 - 588 bytes. */
    public String sePadding;

    /** 176 - Pixel data size - as stored. */
    public float sePdsA;

    /** 180 - Pixel data size - Compressed. */
    public float sePdsC;

    /** 184 - Pixel data size - Uncompressed. */
    public float sePdsU;

    /** 72 - Most-like Plane (for L&#47S). */
    public short sePlane;

    /** 132 - Last Pulse Sequence Used (L&#47S). */
    public short sePseq;

    /** 10 - series number. */
    public short seriesNum;

    /** 336 - 32 bytes. */
    public String seriesUID;

    /** 108 - series from which prescribed. */
    public short serrx;

    /** 454 - 16 bytes. */
    public String serviceID;

    /** 134 - (internal use) Image Sort Order (L&#47S). */
    public short seSortOrder;

    /** 70 - Series from which prescribed. */
    public short seSource;

    /** 0 - 4 bytes - Suite ID for this Series. */
    public String seSuid;

    /** 68 - Series Type. */
    public short seType;

    /** 4 - The make-unique flag - GE internal. */
    public short seUniq;

    /** 172 - 2 bytes - Genesis Version - Created. */
    public String seVersCre;

    /** 174 - 2 bytes - Genesis Version - Now. */
    public String seVersCur;

    /** 26 in mm. */
    public float sliceThickness;

    /** 274 Slice offsets on frequency axis. */
    public int slocfov;

    /** 676 Float Slop Field 1. */
    public float slop_float_1;

    /** 680 Float Slop Field 2. */
    public float slop_float_2;

    /** 684 Float Slop Field 3. */
    public float slop_float_3;

    /** 688 Float Slop Field 4. */
    public float slop_float_4;

    /** 692 Float Slop Field 5. */
    public float slop_float_5;

    /** 656 Integer Slop Field 1. */
    public int slop_int_1;

    /** 660 Integer Slop Field 2. */
    public int slop_int_2;

    /** 664 Integer Slop Field 3. */
    public int slop_int_3;

    /** 668 Integer Slop Field 4. */
    public int slop_int_4;

    /** 672 Integer Slop Field 5. */
    public int slop_int_5;

    /** 974 Integer Slop Field 6. */
    public int slop_int_6;

    /** 978 Integer Slop Field 7. */
    public int slop_int_7;

    /** 982 Integer Slop Field 8. */
    public int slop_int_8;

    /** 986 Integer Slop Field 9. */
    public int slop_int_9;

    /** 696 16 characters String Slop Field 1. */
    public String slop_str_1;

    /** 712 16 characters String Slop Field 2. */
    public String slop_str_2;

    /** 398 Number of slices in this scan group. */
    public short slquant;

    /** 778 32 characters Service Obj Clas Unique ID. */
    public String sop_uid;

    /** 58 Distance from source to detector. */
    public float srcToDet;

    /** 62 Distance from source to iso. */
    public float srcToIso;

    // filler3
    /** 122 - First scan location (L&#47S). */
    public float startLoc;

    /** 120 - 1 byte - RAS letter for first scan location (L&#47S). */
    public String startRAS;

    /** 506 Indicates if study has complete info (DICOM&#47genesis). */
    public short studyStatus;

    /** 474 - 32 bytes. */
    public String studyUID;


    // Exam header for 1024 bytes, 1040 identical except for 16 more padding bytes

    /** 0 - 4 bytes. */
    public String suiteID;

    /** 651 Summary image field. */
    public String sumImg;

    /** 392 SAT fat&#47water&#47none. */
    public short supp_tech;

    /** 380. */
    public short surfaceCoilType;

    /** 382 Extremity Coil Flag. */
    public short surfcext;

    /** 262 Swap phase-frequency axis. */
    public short swapPF;

    /** 214. */
    public float tableDelta;

    /** 226 Delay time after trigger (msec). */
    public int tDel;

    /** 206 second echo echo (usec). */
    public int te2;

    /** 578 Slice Thickness. */
    public short thk_disclmr;

    /** 628 Upper range of pixels 1. */
    public int threshMax1;

    /** 636 Upper range of pixels 2. */
    public int threshMax2;

    /** 624 Lower range of pixels 1. */
    public int threshMin1;

    /** 632 Lower range of pixels 2. */
    public int threshMin2;

    /** 228 - (internal use) number of Unarchived images. */
    public int toArchiveCnt;

    /** 236. */
    public int toArchiveData;

    /** 232 - (internal use) Unarchived images keys in Series. */
    public int toArchiveLen;

    /** 132 Trauma Flag. */
    public short trauma;

    /** 240 Trigger window (&#37 of R-R interval). */
    public short trgWindow;

    /** 66. */
    public short tubeType;

    /** 224. */
    public int unImagesData;

    /** 220 - (internal use) Unstored Image keys in Series. */
    public int unImagesLen;

    /** 4 - (internal use) Make-Unique Flag. */
    public short uniq;

    /** 438 - 16 bytes. */
    public String uniqSysID;

    /** 824 identifies image as original or unoriginal. */
    public short unOriginal;

    /** 406 User Variable 0. */
    public float user0;

    /** 410 User Variable 1. */
    public float user1;

    /** 446 User Variable 10. */
    public float user10;

    /** 450 User Variable 11. */
    public float user11;

    /** 454 User Variable 12. */
    public float user12;

    /** 458 User Variable 13. */
    public float user13;

    /** 462 User Variable 14. */
    public float user14;

    /** 466 User Variable 15. */
    public float user15;

    /** 470 User Variable 16. */
    public float user16;

    /** 474 User Variable 17. */
    public float user17;

    /** 478 User Variable 18. */
    public float user18;

    /** 482 User Variable 19. */
    public float user19;

    /** 414 User Variable 2. */
    public float user2;

    /** 486 User Variable 20. */
    public float user20;

    /** 490 User Variable 21. */
    public float user21;

    /** 494 User Variable 22. */
    public float user22;

    /** 588 User Variable 23. */
    public float user23n;

    /** 502 Concat Sat Type Flag. */
    public float user24;

    /** 592 User Variable 24. */
    public float user24n;

    /** 878 User Variable 25. */
    public float user25;

    /** 882 User Variable 26. */
    public float user26;

    /** 886 User Variable 27. */
    public float user27;

    /** 890 User Variable 28. */
    public float user28;

    /** 894 User Variable 29. */
    public float user29;

    /** 418 User Variable 3. */
    public float user3;

    /** 898 User Variable 30. */
    public float user30;

    /** 902 User Variable 31. */
    public float user31;

    /** 906 User Variable 32. */
    public float user32;

    /** 910 User Variable 33. */
    public float user33;

    /** 914 User Variable 34. */
    public float user34;

    /** 918 User Variable 35. */
    public float user35;

    /** 922 User Variable 36. */
    public float user36;

    /** 926 User Variable 37. */
    public float user37;

    /** 930 User Variable 38. */
    public float user38;

    /** 934 User Variable 39. */
    public float user39;

    /** 422 User Variable 4. */
    public float user4;

    /** 938 User Variable 40. */
    public float user40;

    /** 942 User Variable 41. */
    public float user41;

    /** 946 User Variable 42. */
    public float user42;

    /** 950 User Variable 43. */
    public float user43;

    /** 954 User Variable 44. */
    public float user44;

    /** 958 User Variable 45. */
    public float user45;

    /** 962 User Variable 46. */
    public float user46;

    /** 966 User Variable 47. */
    public float user47;

    /** 970 User variable 48. */
    public float user48;

    /** 426 User Variable 5. */
    public float user5;

    /** 430 User Variable 6. */
    public float user6;

    /** 434 User Variable 7. */
    public float user7;

    /** 438 User Variable 8. */
    public float user8;

    /** 442 User Variable 9. */
    public float user9;

    /** 292 Bitmap defining user Cvs. */
    public int user_bitmap;

    /** 874 Describes what process fills in the user CVs, ifcc or TIR. */
    public int userFillMapLSW;

    /** 870 Describes what process fills in the user CVs, ifcc or TIR. */
    public int userFillMapMSW;

    /** 866 Defines how following user CVs are to be filled in. */
    public int userUsageTag;

    /** 32 - background value of non image locations. */
    public int valueBg;

    /** 648 Variable echo flag. */
    public short varEcho;

    /** 586 Collapse image. */
    public short vas_collapse;

    /** 744 Vascular Mode. */
    public short vasMode;

    /** 394 Variable Bandwidth (Hz). */
    public float vbw;

    /** 576 Phase contrast velocity encoding (mm&#47sec). */
    public short venc;

    /** 732 Scale Weighting Venc (Velocity Encoding&#47PI). */
    public float vencScale;

    /** 830 views per segment. */
    public short viewsPerSeg;

    /** 8 - width of image. */
    public int width;

    /** 612 X axis rotation. */
    public float xAxisRot;

    /** 144 - Ending number for baselines. */
    public short xBaseEnd;

    /** 142 - Starting number for baselines. */
    public short xBaseSt;

    /** 148 - Ending number for enhanced scans. */
    public short xenhEnd;

    /** 146 - Starting number for enhanced scans. */
    public short xenhSt;

    /** 278 Center Frequency (0.1 Hz). */
    public int xmtFreq;

    /** 248 Actual transmit gain (.1 db). */
    public short xmtGain;

    /** 616 Yaxis rotation. */
    public float yAxisRot;

    /** 620 Z axis rotation. */
    public float zAxisRot;

    /** 50 Cell number at theta. */
    public float zeroCell;
    
    public int startAdjust;

    /** DOCUMENT ME! */
    private long currentSeconds;

    /** DOCUMENT ME! */
    private int day;

    /** DOCUMENT ME! */
    private int hour;

    /** DOCUMENT ME! */
    private int minute;

    /** DOCUMENT ME! */
    private String month;

    /** DOCUMENT ME! */
    private int monthIndex;

    /** DOCUMENT ME! */
    private int seconds;

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
    public FileInfoGESigna5X(String name, String directory, int format) {
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

        // fileInfo.getTagTable().setValue("0002,0001", version, 2);
        // fileInfo.getTagTable().setValue("0002,0002", "1.2.840.10008.5.1.4.1.1.7 ", 26); // Secondary Capture SOP UID
        Character nullChar = Character.MIN_VALUE;
        fileInfo.getTagTable().setValue("0002,0010", "1.2.840.10008.1.2" +  String.valueOf(nullChar), 18); // Little Endian transfer syntax
        // Bogus implementation UID
        fileInfo.getTagTable().setValue("0002,0012", "2.25.219181047665313190039273556003903477538", 44);
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
        String s = String.valueOf(pixelResY) + "\\" + String.valueOf(pixelResX);
        String mmStr;
        String ddStr;
        String hhStr;
        String ssStr;
        fileInfo.getTagTable().setValue("0028,0030", s, s.length());

        // Slice thickness
        s = String.valueOf(sliceThickness);
        fileInfo.getTagTable().setValue("0018,0050", s, s.length()); // slice thickness
        s = String.valueOf(scanSpacing);
        fileInfo.getTagTable().setValue("0018,0088", s, s.length()); // spacing between slices

        if (examType.trim().equals("CT")) {
            fileInfo.getTagTable().setValue("0008,0060", "CT", 2);
            fileInfo.getTagTable().setValue("0002,0002", "1.2.840.10008.5.1.4.1.1.2" +  String.valueOf(nullChar), 26);
            fileInfo.getTagTable().setValue("0002,0003", "1.2.840.10008.5.1.4.1.1.2" +  String.valueOf(nullChar), 26);
            fileInfo.getTagTable().setValue("0008,0016", "1.2.840.10008.5.1.4.1.1.2" +  String.valueOf(nullChar), 26);
            fileInfo.getTagTable().setValue("0008,0018", "1.2.840.10008.5.1.4.1.1.2" +  String.valueOf(nullChar), 26);
        } else {
            fileInfo.getTagTable().setValue("0008,0060", "MR", 2);
            fileInfo.getTagTable().setValue("0002,0002", "1.2.840.10008.5.1.4.1.1.4" +  String.valueOf(nullChar), 26);
            fileInfo.getTagTable().setValue("0002,0003", "1.2.840.10008.5.1.4.1.1.4" +  String.valueOf(nullChar), 26);
            fileInfo.getTagTable().setValue("0008,0016", "1.2.840.10008.5.1.4.1.1.4" +  String.valueOf(nullChar), 26);
            fileInfo.getTagTable().setValue("0008,0018", "1.2.840.10008.5.1.4.1.1.4" +  String.valueOf(nullChar), 26);
        }

        // fileInfo.getTagTable().setValue("0008,0016", "1.2.840.10008.5.1.4.1.1.7 ", 26); // Secondary Capture UID
        

        secs2date(exDateTime);

        if (monthIndex < 10) {
            mmStr = "0" + monthIndex;
        } else {
            mmStr = "" + monthIndex;
        }

        if (day < 10) {
            ddStr = "0" + day;
        } else {
            ddStr = "" + day;
        }

        s = year + mmStr + ddStr;
        fileInfo.getTagTable().setValue("0008,0020", s, s.length()); // Study date

        if (hour < 10) {
            hhStr = "0" + hour;
        } else {
            hhStr = "" + hour;
        }

        if (minute < 10) {
            mmStr = "0" + minute;
        } else {
            mmStr = "" + minute;
        }

        if (seconds < 10) {
            ssStr = "0" + seconds;
        } else {
            ssStr = "" + seconds;
        }

        s = hhStr + mmStr + ssStr + ".0";
        fileInfo.getTagTable().setValue("0008,0030", s, s.length()); // Study time

        secs2date(seDateTime);

        if (monthIndex < 10) {
            mmStr = "0" + monthIndex;
        } else {
            mmStr = "" + monthIndex;
        }

        if (day < 10) {
            ddStr = "0" + day;
        } else {
            ddStr = "" + day;
        }

        s = year + mmStr + ddStr;
        fileInfo.getTagTable().setValue("0008,0021", s, s.length()); // Series date

        if (hour < 10) {
            hhStr = "0" + hour;
        } else {
            hhStr = "" + hour;
        }

        if (minute < 10) {
            mmStr = "0" + minute;
        } else {
            mmStr = "" + minute;
        }

        if (seconds < 10) {
            ssStr = "0" + seconds;
        } else {
            ssStr = "" + seconds;
        }

        s = hhStr + mmStr + ssStr + ".0";
        fileInfo.getTagTable().setValue("0008,0031", s, s.length()); // Series time

        secs2date(actualDateTime);

        if (monthIndex < 10) {
            mmStr = "0" + monthIndex;
        } else {
            mmStr = "" + monthIndex;
        }

        if (day < 10) {
            ddStr = "0" + day;
        } else {
            ddStr = "" + day;
        }

        s = year + mmStr + ddStr;
        fileInfo.getTagTable().setValue("0008,0023", s, s.length()); // Image date

        if (hour < 10) {
            hhStr = "0" + hour;
        } else {
            hhStr = "" + hour;
        }

        if (minute < 10) {
            mmStr = "0" + minute;
        } else {
            mmStr = "" + minute;
        }

        if (seconds < 10) {
            ssStr = "0" + seconds;
        } else {
            ssStr = "" + seconds;
        }

        s = hhStr + mmStr + ssStr + ".0";
        fileInfo.getTagTable().setValue("0008,0033", s, s.length()); // Image time

        fileInfo.getTagTable().setValue("0008,0050", "123456", 6);
        fileInfo.getTagTable().setValue("0008,0080", hospName.trim(), hospName.trim().length()); // Institution name
        fileInfo.getTagTable().setValue("0008,1030", exDesc.trim(), exDesc.trim().length()); // Study description
        fileInfo.getTagTable().setValue("0008,103E", seDesc.trim(), seDesc.trim().length()); // Series description

        fileInfo.getTagTable().setValue("0010,0010", patientName.trim(), patientName.trim().length());
        fileInfo.getTagTable().setValue("0010,0020", patientID.trim(), patientID.trim().length());
        fileInfo.getTagTable().setValue("0010,1010", "0" + String.valueOf(patientAge),
                                        String.valueOf(patientAge).length() + 1);
        fileInfo.getTagTable().setValue("0010,21B0", hist.trim(), hist.trim().length());

        RandomNumberGen randomNum = new RandomNumberGen();
        randomNum.genUniformRandomNum(1, 100000);
        s = "2.25.219181047665313190039273556003903477538." + randomNum.genUniformRandomNum(1, 100000);
        if ((s.length() % 2) == 1) {
        	s = s + String.valueOf(Character.MIN_VALUE);
        }
        fileInfo.getTagTable().setValue("0020,000D", s, s.length()); // study UID
        s = "2.25.219181047665313190039273556003903477538." + randomNum.genUniformRandomNum(1, 100000);
        if ((s.length() % 2) == 1) {
        	s = s + String.valueOf(Character.MIN_VALUE);
        }
        fileInfo.getTagTable().setValue("0020,000E", s, s.length()); // series UID


        // study Number  (SH  short string)
        fileInfo.getTagTable().setValue("0020,0010", String.valueOf((short) examNum),
                                        String.valueOf((short) examNum).length());

        // series Number (IS integer string)
        fileInfo.getTagTable().setValue("0020,0011", String.valueOf((int) seriesNum),
                                        String.valueOf((int) seriesNum).length());

        s = -imgTLHC_R + "\\" + -imgTLHC_A + "\\" + imgTLHC_S;

        // s = imgTLHC_R + "\\" + imgTLHC_A + "\\" + imgTLHC_S;
        fileInfo.getTagTable().setValue("0020,0032", s, s.length()); // image position Right center .....


        // This will depend on MR or CT
        // fileInfo.getTagTable().setValue("0020,0037", ,   );  // image orientation
        if (examType.trim().equals("CT")) {

            // axial
            s = "1.0" + "\\" + "0.0" + "\\" + "0.0" + "\\" + "0.0" + "\\" + "1.0" + "\\" + "0.0";
            fileInfo.getTagTable().setValue("0020,0037", s, s.length()); // image orientation
        } else {

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
        }

        s = String.valueOf(imgLoc);
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

        dialog.append(getAboutInfo(matrix));

        dialog.setSize(600, 500);
    }
    
    public String getAboutInfo(TransMatrix matrix) {
        String infoStr = "";
        
        infoStr += getPrimaryInfo(matrix);

        infoStr += "\n\n                Other information\n\n";

        infoStr += "Image width  = " + width + "\n";
        infoStr += "Image height = " + height + "\n";
        infoStr += "Image depth  = " + depth + "\n";
        infoStr += "Image compression = " + compression + "\n";
        infoStr += "Image background value      = " + valueBg + "\n";
        infoStr += "Image value to add to image = " + addValue + "\n";

        /***********************************************************************************************/
        infoStr += "\nExam informaton \n";

        if (suiteID != null) {
            infoStr += "Suite ID for this exam = " + suiteID.trim() + "\n";
        }

        infoStr += "Examination number = " + examNum + "\n";

        if (hospName != null) {
            infoStr += "Hospital name = " + hospName.trim() + "\n";
        }

        infoStr += "Detector type = " + detect + "\n";
        infoStr += "Number of cells in detector = " + numCells + "\n";
        infoStr += "Cell number at theta = " + zeroCell + "\n";
        infoStr += "Cell spacing = " + cellSpace + "\n";
        infoStr += "Distance from source to detector = " + srcToDet + "\n";
        infoStr += "Distance from source to iso = " + srcToIso + "\n";
        infoStr += "Tube type = " + tubeType + "\n";
        infoStr += "DAS type = " + dasType + "\n";
        infoStr += "Number of Decon kernels = " + numDcnK + "\n";
        infoStr += "Number of elements in Decon kernel = " + dcnLen + "\n";
        infoStr += "Decon kernel density = " + dcnDensity + "\n";
        infoStr += "Decon kernel stepsize = " + dcnStepSize + "\n";
        infoStr += "Decon kernel Shift Count = " + dcnShiftCnt + "\n";
        infoStr += "Magnet strength (in gauss) = " + magStrength + "\n";

        if (patientID != null) {
            infoStr += "Patient ID for this exam = " + patientID.trim() + "\n";
        }

        if (patientName != null) {
            infoStr += "Patient name = " + patientName.trim() + "\n";
        }

        infoStr += "Patient age = " + patientAge + "\n";
        infoStr += "Patient age notation = " + patian + "\n";
        infoStr += "Patient sex = " + patientSex + "\n";
        infoStr += "Patient weight = " + patWeight + "\n";
        infoStr += "Trauma flag = " + trauma + "\n";

        if (hist != null) {
            infoStr += "Patient history = " + hist.trim() + "\n";
        }

        if (reqnum != null) {
            infoStr += "Requisition number = " + reqnum.trim() + "\n";
        }

        secs2date(exDateTime);
        infoStr += "Exam date/time stamp = " + month + " " + day + "," + " " + year + " " + hour + ":" + minute +
                      ":" + seconds + "\n";

        if (refPhy != null) {
            infoStr += "Referring physician = " + refPhy.trim() + "\n";
        }

        if (diagRad != null) {
            infoStr += "Diagnostician/Radiologist = " + diagRad.trim() + "\n";
        }

        if (op != null) {
            infoStr += "Operator = " + op.trim() + "\n";
        }

        if (exDesc != null) {
            infoStr += "Exam Description = " + exDesc.trim() + "\n";
        }

        if (examType != null) {
            infoStr += "Examination type = " + examType.trim() + "\n";
        }

        infoStr += "Exam format = " + exFormat + "\n";
        infoStr += "Start time (secs) of first axial in exam = " + firstAxTime + "\n";

        if (exSysID != null) {
            infoStr += "Creator Suite and Host = " + exSysID.trim() + "\n";
        }

        secs2date(exLastMod);
        infoStr += "Date/Time of Last Change = " + month + " " + day + "," + " " + year + " " + hour + ":" + minute +
                      ":" + seconds + "\n";
        infoStr += "Non-zero indicates Protocol Exam = " + protocolFlag + "\n";

        if (exAllocKey != null) {
            infoStr += "Process that allocated this record = " + exAllocKey.trim() + "\n";
        }

        infoStr += "Genesis Version - Created = " + exVersCre + "\n";
        infoStr += "Genesis Version - Now = " + exVersCur + "\n";
        infoStr += "Patient Status = " + exStat + "\n";

        if (studyUID != null) {
            infoStr += "Study UID = " + studyUID.trim() + "\n";
        }

        infoStr += "Indicates if study has complete info (DICOM/genesis) = " + studyStatus + "\n";

        if (exPadding != null) {
            infoStr += "exPadding = " + exPadding.trim() + "\n";
        }

        /***********************************************************************************************/
        infoStr += "\nSeries information \n";

        if (seSuid != null) {
            infoStr += "Suite ID for this Series = " + seSuid.trim() + "\n";
        }

        infoStr += "Exam Number = " + seExamNo + "\n";
        infoStr += "Series Number = " + seriesNum + "\n";
        secs2date(seDateTime);
        infoStr += "Allocation Series Date/Time Stamp = " + month + " " + day + "," + " " + year + " " + hour + ":" +
                      minute + ":" + seconds + "\n";
        secs2date(seActualDT);
        infoStr += "Actual Series Date/Time Stamp = " + month + " " + day + "," + " " + year + " " + hour + ":" +
                      minute + ":" + seconds + "\n";

        if (seDesc != null) {
            infoStr += "Series Description = " + seDesc.trim() + "\n";
        }

        if (prSysID != null) {
            infoStr += "Primary Receiver Suite and Host = " + prSysID.trim() + "\n";
        }

        if (panSysID != null) {
            infoStr += "Archiver Suite and Host = " + panSysID.trim() + "\n";
        }

        infoStr += "Series Type = " + seType + "\n";
        infoStr += "Series from which prescribed = " + seSource + "\n";
        infoStr += "Most-like Plane (for L/S) = " + sePlane + "\n";
        infoStr += "Scout or Axial (for CT) = " + scanType + "\n";
        infoStr += "Patient Position = " + position + "\n";
        infoStr += "Patient Entry = " + entry + "\n";

        if (anatomicalRef != null) {
            infoStr += "Anatomical reference = " + anatomicalRef.trim() + "\n";
        }

        infoStr += "Horizontal landmark = " + lmHor + "\n";

        if (scanProtocolName != null) {
            infoStr += "Scan Protocol Name = " + scanProtocolName.trim() + "\n";
        }

        infoStr += "If greater than 0 image used contrast (L/S) = " + seContrast + "\n";
        infoStr += "RAS letter for first scan location (L/S) = " + startRAS + "\n";
        infoStr += "First scan location (L/S) = " + startLoc + "\n";
        infoStr += "RAS letter for last scan loction (L/S) = " + endRAS + "\n";
        infoStr += "Last scan location (L/S) = " + endLoc + "\n";
        infoStr += "Last pulse sequence used (L/S) = " + sePseq + "\n";
        infoStr += "Landmark Counter = " + seLandmarkCnt + "\n";
        infoStr += "Number of Acquisitions = " + seNacq + "\n";
        infoStr += "Starting number for baselines = " + xBaseSt + "\n";
        infoStr += "Ending number for baselines = " + xBaseEnd + "\n";
        infoStr += "Starting number for enhanced scans = " + xenhSt + "\n";
        infoStr += "Ending number for enhanced scans = " + xenhEnd + "\n";
        secs2date(seLastMod);
        infoStr += "Date/Time of Last Change = " + month + " " + day + "," + " " + year + " " + hour + ":" + minute +
                      ":" + seconds + "\n";

        if (seAllocKey != null) {
            infoStr += "Process that allocated this record = " + seAllocKey.trim() + "\n";
        }

        infoStr += "Genesis Version - Created = " + seVersCre + "\n";
        infoStr += "Genesis Version - Now = " + seVersCur + "\n";
        infoStr += "Pixel Data size - as stored = " + sePdsA + "\n";
        infoStr += "Pixel Data size - Compressed = " + sePdsC + "\n";
        infoStr += "Pixel Data size - Uncompressed = " + sePdsU + "\n";
        infoStr += "Echo 1 Alpha Value = " + echo1Alpha + "\n";
        infoStr += "Echo 1 Beta Value = " + echo1Beta + "\n";
        infoStr += "Echo 1 Window Value = " + echo1Window + "\n";
        infoStr += "Echo 1 Level Value = " + echo1Level + "\n";
        infoStr += "Echo 2 Alpha Value = " + echo2Alpha + "\n";
        infoStr += "Echo 2 Beta Value = " + echo2Beta + "\n";
        infoStr += "Echo 2 Window Value = " + echo2Window + "\n";
        infoStr += "Echo 2 Level Value = " + echo2Level + "\n";
        infoStr += "Echo 3 Alpha Value = " + echo3Alpha + "\n";
        infoStr += "Echo 3 Beta Value = " + echo3Beta + "\n";
        infoStr += "Echo 3 Window Value = " + echo3Window + "\n";
        infoStr += "Echo 3 Level Value = " + echo3Level + "\n";
        infoStr += "Echo 4 Alpha Value = " + echo4Alpha + "\n";
        infoStr += "Echo 4 Beta Value = " + echo4Beta + "\n";
        infoStr += "Echo 4 Window Value = " + echo4Window + "\n";
        infoStr += "Echo 4 Level Value = " + echo4Level + "\n";
        infoStr += "Echo 5 Alpha Value = " + echo5Alpha + "\n";
        infoStr += "Echo 5 Beta Value = " + echo5Beta + "\n";
        infoStr += "Echo 5 Window Value = " + echo5Window + "\n";
        infoStr += "Echo 5 Level Value = " + echo5Level + "\n";
        infoStr += "Echo 6 Alpha Value = " + echo6Alpha + "\n";
        infoStr += "Echo 6 Beta Value = " + echo6Beta + "\n";
        infoStr += "Echo 6 Window Value = " + echo6Window + "\n";
        infoStr += "Echo 6 Level Value = " + echo6Level + "\n";
        infoStr += "Echo 7 Alpha Value = " + echo7Alpha + "\n";
        infoStr += "Echo 7 Beta Value = " + echo7Beta + "\n";
        infoStr += "Echo 7 Window Value = " + echo7Window + "\n";
        infoStr += "Echo 7 Level Value = " + echo7Level + "\n";
        infoStr += "Echo 8 Alpha Value = " + echo8Alpha + "\n";
        infoStr += "Echo 8 Beta Value = " + echo8Beta + "\n";
        infoStr += "Echo 8 Window Value = " + echo8Window + "\n";
        infoStr += "Echo 8 Level Value = " + echo8Level + "\n";

        if (sePadding != null) {
            infoStr += "sePadding = " + sePadding.trim() + "\n";
        }

        /***********************************************************************************************/
        infoStr += "\nImage informaton \n";
        infoStr += "Image Header Suite ID = " + imgHdrSuiteID + "\n";
        infoStr += "Image Exam number = " + imgHdrExamNum + "\n";
        infoStr += "Image Series number = " + imgHdrSeriesNum + "\n";
        infoStr += "Image number = " + imageNum + "\n";
        currentSeconds = System.currentTimeMillis() / 1000L;

        // dateTime >= 0 means dateTime does not exceed January 19, 2038 3:14:07
        if ((dateTime >= 0) && (dateTime < currentSeconds)) {
            secs2date(dateTime);
            infoStr += "Image date time = " + month + " " + day + "," + " " + year + " " + hour + ":" + minute +
                          ":" + seconds + "\n";
        }

        // actualDateTime >= 0 means actualDateTime does not exceed January 19, 2038 3:14:07
        if ((actualDateTime >= 0) && (actualDateTime < currentSeconds)) {
            secs2date(actualDateTime);
            infoStr += "Image actual time = " + month + " " + day + "," + " " + year + " " + hour + ":" + minute +
                          ":" + seconds + "\n";
        }

        infoStr += "Image scan time = " + scTime + "\n";
        infoStr += "Slice thickness = " + sliceThickness + "\n";
        infoStr += "Matrix size X = " + matrixSizeX + "\n";
        infoStr += "Matrix size Y = " + matrixSizeY + "\n";
        infoStr += "Field of view X = " + FOVX + "\n";
        infoStr += "Field of view Y = " + FOVY + "\n";
        infoStr += "Image dim X = " + imageDimX + "\n";
        infoStr += "Image dim Y = " + imageDimY + "\n";
        infoStr += "Pixel resolution X = " + pixelResX + "\n";
        infoStr += "Pixel resolution Y = " + pixelResY + "\n";

        if (pixelID != null) {
            infoStr += "Pixel ID = " + pixelID.trim() + "\n";
        }

        if (IVCntrstAgent != null) {
            infoStr += "IV contrast agent = " + IVCntrstAgent.trim() + "\n";
        }

        if (OralCntrstAgent != null) {
            infoStr += "Oral contrast agent = " + OralCntrstAgent.trim() + "\n";
        }

        infoStr += "Image contrast mode = " + contrastMode + "\n";
        infoStr += "Series from which prescribed = " + serrx + "\n";
        infoStr += "Image from which prescribed = " + imgrx + "\n";
        infoStr += "Screen format (8/16) bit = " + screenFormat + "\n";
        infoStr += "Plane type = " + planeType + "\n";
        infoStr += "Spacing between scans = " + scanSpacing + "\n";
        infoStr += "Image compression type for allocation = " + compress + "\n";
        infoStr += "Scout type = " + scoutType + "\n";

        infoStr += "RAS letter of image location = " + loc_ras + "\n";
        infoStr += "Image location = " + imgLoc + "\n";
        infoStr += "Image center right = " + imgCtrR + "\n";
        infoStr += "Image center anterior = " + imgCtrA + "\n";
        infoStr += "Image center superior = " + imgCtrS + "\n";
        infoStr += "Normal right = " + norm_R + "\n";
        infoStr += "Normal anterior = " + norm_A + "\n";
        infoStr += "Normal superior = " + norm_S + "\n";
        infoStr += "Right top left hand corner = " + imgTLHC_R + "\n";
        infoStr += "Anterior top left hand corner = " + imgTLHC_A + "\n";
        infoStr += "Superior top left hand corner = " + imgTLHC_S + "\n";
        infoStr += "Right top right hand corner = " + imgTRHC_R + "\n";
        infoStr += "Anterior top right hand corner = " + imgTRHC_A + "\n";
        infoStr += "Superior top right hand corner = " + imgTRHC_S + "\n";
        infoStr += "Right bottom right hand corner = " + imgBRHC_R + "\n";
        infoStr += "Anterior bottom right hand corner = " + imgBRHC_A + "\n";
        infoStr += "Superior bottom right hand corner = " + imgBRHC_S + "\n";

        if ((examType != null) && (examType.trim().equals("MR"))) {

            if (forImgRev != null) {
                infoStr += "Foreign Image Revision = " + forImgRev.trim() + "\n";
            }

            infoStr += "Pulse repetition time (usec) = " + pulseRepTime + "\n";
            infoStr += "Pulse Inversion time (usec) = " + inverTime + "\n";
            infoStr += "Pulse echo time (usec) = " + echoTime + "\n";
            infoStr += "Second echo (usec) = " + te2 + "\n";

            infoStr += "Number of echoes = " + nEchoes + "\n";
            infoStr += "Echo number = " + echoNum + "\n";
            infoStr += "Table delta = " + tableDelta + "\n";
            infoStr += "Number of excitations = " + NEX + "\n";
            infoStr += "Continous Slices Flag = " + contig + "\n";
            infoStr += "Caridac eart rate (bpm) = " + heartRate + "\n";
            infoStr += "Delay time after trigger (msec) = " + tDel + "\n";
            infoStr += "SAR average = " + sarAvg + "\n";
            infoStr += "SAR peak = " + sarPeak + "\n";
            infoStr += "Monitor SAR flag = " + monSar + "\n";
            infoStr += "Trigger window = " + trgWindow + "\n";
            infoStr += "Cardiac repetition time = " + repTime + "\n";
            infoStr += "Images per cardiac .1 cycle = " + imgPCycle + "\n";
            infoStr += "Actual transmit gain (.1db) = " + xmtGain + "\n";
            infoStr += "Actual Receive gain Analog (.1db) = " + rcvGain1 + "\n";
            infoStr += "Actual Receive gain Digital (.1db) = " + rcvGain2 + "\n";
            infoStr += "Flip angle for grass scans = " + mr_flip + "\n";
            infoStr += "Minimum delay after trigger (usec) = " + minDAT + "\n";
            infoStr += "Total cardiac phase prescribed = " + cPhase + "\n";
            infoStr += "Swap Phase/Frequency Axis = " + swapPF + "\n";
            infoStr += "Pause interval (slices) = " + pauseInterval + "\n";
            infoStr += "Pause time = " + pauseTime + "\n";
            infoStr += "Oblique plane = " + obliquePlane + "\n";
            infoStr += "Slice offsets on Freq axis = " + slocfov + "\n";

            if (xmtFreq > 0) {
                infoStr += "Center Frequency (0.1Hz) = " + xmtFreq + "\n";
            }

            if (autoXmtFreq > 0) {
                infoStr += "Auto Center frequency (0.1Hz) = " + autoXmtFreq + "\n";
            }

            infoStr += "Auto transmit gain (0.1dB) = " + autoXmtGain + "\n";
            infoStr += "Prescan R1-Analog = " + prescan_r1 + "\n";
            infoStr += "Prescan R2-Digital = " + prescan_r2 + "\n";
            infoStr += "Bitmap defining user CVs = " + user_bitmap + "\n";
            infoStr += "Center Frequency Method = " + cenFreq + "\n";

            infoStr += "Imaging mode = " + iMode + "\n";
            infoStr += "Imaging options = " + iOptions + "\n";
            infoStr += "Pulse sequence = " + pSeq + "\n";
            infoStr += "Pulse sequence mode = " + pulseSeqMode + "\n";

            if (pulseSeqName != null) {
                infoStr += "Pulse sequence name = " + pulseSeqName.trim() + "\n";
            }

            // psd_dateTime >= 0 menas psd_dateTime does not exceed January 19, 2038 3:14:07
            if ((psd_dateTime >= 0) && (psd_dateTime < currentSeconds)) {
                secs2date(psd_dateTime);
                infoStr += "PSD Creation date and time = " + month + " " + day + "," + " " + year + " " + hour +
                              ":" + minute + ":" + seconds + "\n";
            }

            if (psd_iname != null) {
                infoStr += "PSD name from inside PSD = " + psd_iname.trim() + "\n";
            }

            infoStr += "Coil type = " + coilType + "\n";

            if (coilName != null) {
                infoStr += "Coil name = " + coilName.trim() + "\n";
            }

            infoStr += "Surface coil type = " + surfaceCoilType + "\n";
            infoStr += "Extremity coil flag = " + surfcext + "\n";
            infoStr += "Raw data run number = " + rawRunNum + "\n";
            infoStr += "Calibrated field strength (x10 Gauss) = " + calFldStr + "\n";
            infoStr += "SAT fat/water/none = " + supp_tech + "\n";
            infoStr += "Variable bandwidth (Hz) = " + vbw + "\n";
            infoStr += "Number of slices in this scan group = " + slquant + "\n";
            infoStr += "Graphically prescribed = " + gpre + "\n";
            infoStr += "Interimage/interloc delay (uSec) = " + intr_del + "\n";
            infoStr += "User var 0  = " + user0 + "\n";
            infoStr += "User var 1  = " + user1 + "\n";
            infoStr += "User var 2  = " + user2 + "\n";
            infoStr += "User var 3  = " + user3 + "\n";
            infoStr += "User var 4  = " + user4 + "\n";
            infoStr += "User var 5  = " + user5 + "\n";
            infoStr += "User var 6  = " + user6 + "\n";
            infoStr += "User var 7  = " + user7 + "\n";
            infoStr += "User var 8  = " + user8 + "\n";
            infoStr += "User var 9  = " + user9 + "\n";
            infoStr += "User var 10 = " + user10 + "\n";
            infoStr += "User var 11 = " + user11 + "\n";
            infoStr += "User var 12 = " + user12 + "\n";
            infoStr += "User var 13 = " + user13 + "\n";
            infoStr += "User var 14 = " + user14 + "\n";
            infoStr += "User var 15 = " + user15 + "\n";
            infoStr += "User var 16 = " + user16 + "\n";
            infoStr += "User var 17 = " + user17 + "\n";
            infoStr += "User var 18 = " + user18 + "\n";
            infoStr += "User var 19 = " + user19 + "\n";
            infoStr += "User var 20 = " + user20 + "\n";
            infoStr += "User var 21 = " + user21 + "\n";
            infoStr += "User var 22 = " + user22 + "\n";

            infoStr += "Projection angle = " + projectAngle + "\n";
            infoStr += "Concat SAT type flag = " + user24 + "\n";

            if (im_alloc_key != null) {
                infoStr += "Process that allocated record = " + im_alloc_key.trim() + "\n";
            }

            // im_lastmod menas im_lastmod does not exceed January 19, 2038 3:14:07
            if ((im_lastmod >= 0) && (im_lastmod < currentSeconds)) {
                secs2date(im_lastmod);
                infoStr += "Date/Time of last change = " + month + " " + day + "," + " " + year + " " + hour + ":" +
                              minute + ":" + seconds + "\n";
            }

            infoStr += "Genesis version - created = " + im_verscre + "\n";

            if (im_verscur != null) {
                infoStr += "Genesis version - now = " + im_verscur.trim() + "\n";
            }

            infoStr += "Pixel data size - as stored = " + im_pds_a + "\n";
            infoStr += "Pixel data size - compressed  = " + im_pds_c + "\n";
            infoStr += "Pixel data size - uncompressed = " + im_pds_u + "\n";

            // infoStr += "AcqRecon record checksum = " + im_checksum + "\n";
            // infoStr += "Image Archive Flag = " + im_archived + "\n";
            // infoStr += "Image Complete Flag = " + im_complete + "\n";
            infoStr += "Bitmap of SAT selections = " + satbits + "\n";
            infoStr += "Surface coil intensity correction flag = " + scic + "\n";
            infoStr += "R-side pulse loc rel to landmark = " + satxloc1 + "\n";
            infoStr += "L-side pulse loc rel to landmark = " + satxloc2 + "\n";
            infoStr += "A-side pulse loc rel to landmark = " + satyloc1 + "\n";
            infoStr += "P-side pulse loc rel to landmark = " + satyloc2 + "\n";
            infoStr += "S-side pulse loc rel to landmark = " + satzloc1 + "\n";
            infoStr += "I-side pulse loc rel to landmark = " + satzloc2 + "\n";
            infoStr += "Thickness of X-axis SAT pulse = " + satxthick + "\n";
            infoStr += "Thickness of Y-axis SAT pulse = " + satythick + "\n";
            infoStr += "Thickness of Z-axis SAT pulse = " + satzthick + "\n";

            infoStr += "Phase contrast flow axis = " + flax + "\n";
            infoStr += "Phase contrast velocity encoding (mm/sec) = " + venc + "\n";
            infoStr += "Slice thickness = " + thk_disclmr + "\n";
            infoStr += "Auto/manual prescan flag = " + ps_flag + "\n";
            infoStr += "Bitmap of change values = " + ps_status + "\n";
            infoStr += "Magnitude, phase, imaginary, or real = " + image_type + "\n";
            infoStr += "Collapse image = " + vas_collapse + "\n";
            infoStr += "User variable 23 = " + user23n + "\n";
            infoStr += "User variable 24 = " + user24n + "\n";
            infoStr += "Projection algorithm = " + proj_alg + "\n";

            if (proj_name != null) {
                infoStr += "Projection algorithm name = " + proj_name.trim() + "\n";
            }

            infoStr += "X axis rotation = " + xAxisRot + "\n";
            infoStr += "Y axis rotation = " + yAxisRot + "\n";
            infoStr += "Z axis rotation = " + zAxisRot + "\n";
            infoStr += "Lower range of pixels 1 = " + threshMin1 + "\n";
            infoStr += "Upper range of pixels 1 = " + threshMax1 + "\n";
            infoStr += "Lower range of pixels 2 = " + threshMin2 + "\n";
            infoStr += "Upper range of pixels 2 = " + threshMax2 + "\n";
            infoStr += "Echo train length for fast spin echo = " + ETL + "\n";
            infoStr += "Fractional echo-effective TE flag = " + fracEcho + "\n";
            infoStr += "Preparatory pulse option = " + prepPulse + "\n";
            infoStr += "Cardiac phase number = " + cPhaseNum + "\n";
            infoStr += "Variable echo flag = " + varEcho + "\n";

            if (refImg != null) {
                infoStr += "Reference image field = " + refImg.trim() + "\n";
            }

            if (sumImg != null) {
                infoStr += "Summary image field = " + sumImg.trim() + "\n";
            }

            infoStr += "Window value = " + imgWindow + "\n";
            infoStr += "Level value = " + imgLevel + "\n";
            infoStr += "Integer Slop Field 1 = " + slop_int_1 + "\n";
            infoStr += "Integer Slop Field 2 = " + slop_int_2 + "\n";
            infoStr += "Integer Slop Field 3 = " + slop_int_3 + "\n";
            infoStr += "Integer Slop Field 4 = " + slop_int_4 + "\n";
            infoStr += "Integer Slop Field 5 = " + slop_int_5 + "\n";
            infoStr += "Float Slop Field 1 = " + slop_float_1 + "\n";
            infoStr += "Float Slop Field 2 = " + slop_float_2 + "\n";
            infoStr += "Float Slop Field 3 = " + slop_float_3 + "\n";
            infoStr += "Float Slop Field 4 = " + slop_float_4 + "\n";
            infoStr += "Float Slop Field 5 = " + slop_float_5 + "\n";

            if (slop_str_1 != null) {
                infoStr += "String Slop Field 1 = " + slop_str_1.trim() + "\n";
            }

            if (slop_str_2 != null) {
                infoStr += "String Slop Field 2 = " + slop_str_2.trim() + "\n";
            }

            infoStr += "Scan Acquisition Number = " + scanAcqNum + "\n";
            infoStr += "Magnitude Weighting Flag = " + magWgtFlag + "\n";
            infoStr += "Scale Weighted Venc (Velocity Encoding/PI) = " + vencScale + "\n";
            infoStr += "GE Image Integrity = " + integrity + "\n";
            infoStr += "Number of Phases = " + nPhase + "\n";
            infoStr += "Frequency Direction = " + freqDir + "\n";
            infoStr += "Vascular Mode = " + vasMode + "\n";

            if (image_uid != null) {
                infoStr += "Image Unique ID = " + image_uid.trim() + "\n";
            }

            if (sop_uid != null) {
                infoStr += "Service Obj Clas Unique ID = " + sop_uid.trim() + "\n";
            }

            infoStr += "Bitmap of prescan options = " + preScanOpts + "\n";
            infoStr += "Gradient offset in the x-direction = " + gOffsetX + "\n";
            infoStr += "Gradient offset in the y-direction = " + gOffsetY + "\n";
            infoStr += "Gradient offset in the z-direction = " + gOffsetZ + "\n";
            infoStr += "Identifies images as original or unoriginal = " + unOriginal + "\n";
            infoStr += "Number of EPI shots = " + nEPI + "\n";
            infoStr += "Effective echo spacing for EPI = " + effEchoSpace + "\n";
            infoStr += "Views per segment = " + viewsPerSeg + "\n";
            infoStr += "Respiratory rate, breaths per minute = " + rbpm + "\n";
            infoStr += "Respiratory trigger point as percent of max = " + rtPoint + "\n";
            infoStr += "Type of receiver used = " + rcvType + "\n";
            infoStr += "Peak rate of change in gradient field, tesla/sec = " + dbdt + "\n";
            infoStr += "Limit in units of percent of theoretical curve = " + dbdtPer + "\n";
            infoStr += "PSD estimated limit in units of percent = " + estdbdtPer + "\n";
            infoStr += "PSD estimated limit in Tesla/sec = " + estdbdtts + "\n";
            infoStr += "Avg head SAR = " + sarAvgHead + "\n";
            infoStr += "Negative scan spacing for overlap slices = " + negScanSpacing + "\n";
            infoStr += "Offset Frequency-Mag. Transfer = " + offsetFreq + "\n";
            infoStr += "Defines how the following user tags are to be filled in = " + userUsageTag + "\n";
            infoStr += "Define what process fills in the user CVs, ifcc or TIR = " + userFillMapMSW + "\n";
            infoStr += "Define what process fills in the user CVs, ifcc or TIR = " + userFillMapLSW + "\n";
            infoStr += "User Variable 25 = " + user25 + "\n";
            infoStr += "User Variable 26 = " + user26 + "\n";
            infoStr += "User Variable 27 = " + user27 + "\n";
            infoStr += "User Variable 28 = " + user28 + "\n";
            infoStr += "User Variable 29 = " + user29 + "\n";
            infoStr += "User Variable 30 = " + user30 + "\n";
            infoStr += "User Variable 31 = " + user31 + "\n";
            infoStr += "User Variable 32 = " + user32 + "\n";
            infoStr += "User Variable 33 = " + user33 + "\n";
            infoStr += "User Variable 34 = " + user34 + "\n";
            infoStr += "User Variable 35 = " + user35 + "\n";
            infoStr += "User Variable 36 = " + user36 + "\n";
            infoStr += "User Variable 37 = " + user37 + "\n";
            infoStr += "User Variable 38 = " + user38 + "\n";
            infoStr += "User Variable 39 = " + user39 + "\n";
            infoStr += "User Variable 40 = " + user40 + "\n";
            infoStr += "User Variable 41 = " + user41 + "\n";
            infoStr += "User Variable 42 = " + user42 + "\n";
            infoStr += "User Variable 43 = " + user43 + "\n";
            infoStr += "User Variable 44 = " + user44 + "\n";
            infoStr += "User Variable 45 = " + user45 + "\n";
            infoStr += "User Variable 46 = " + user46 + "\n";
            infoStr += "User Variable 47 = " + user47 + "\n";
            infoStr += "User Variable 48 = " + user48 + "\n";
            infoStr += "Integer Slop Field 6 = " + slop_int_6 + "\n";
            infoStr += "Integer Slop Field 7 = " + slop_int_7 + "\n";
            infoStr += "Integer Slop Field 8 = " + slop_int_8 + "\n";
            infoStr += "Integer Slop Field 9 = " + slop_int_9 + "\n";

            if (mr_padding != null) {
                infoStr += "Spare Space = " + mr_padding.trim() + "\n";
            }
        }
        
        return infoStr;
    }

    /**
     * Returns the image name which should be used for the image this file info is attached to (studyNum_seriesNum).
     *
     * @return  The name to give to this file info's image.
     */
    public String getImageNameFromInfo() {
        return examNum + "_" + seriesNum;
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
     * @param  numberSeconds  DOCUMENT ME!
     */
    private void secs2date(int numberSeconds) {
        long longSeconds;
        long secondsPerMinute = 60L;
        long secondsPerHour = 60L * secondsPerMinute;
        long secondsPerDay = 24L * secondsPerHour;
        long JanuarySeconds = 31L * secondsPerDay;
        long FebNoLeapSeconds = 28L * secondsPerDay;
        long FebLeapSeconds = 29L * secondsPerDay;
        long MarchSeconds = 31L * secondsPerDay;
        long AprilSeconds = 30L * secondsPerDay;
        long MaySeconds = 31L * secondsPerDay;
        long JuneSeconds = 30L * secondsPerDay;
        long JulySeconds = 31L * secondsPerDay;
        long AugustSeconds = 31L * secondsPerDay;
        long SeptemberSeconds = 30L * secondsPerDay;
        long OctoberSeconds = 31L * secondsPerDay;
        long NovemberSeconds = 30L * secondsPerDay;
        long DecemberSeconds = 31L * secondsPerDay;
        long secondsPerNonLeapYear = JanuarySeconds + FebNoLeapSeconds + MarchSeconds + AprilSeconds + MaySeconds +
                                     JuneSeconds + JulySeconds + AugustSeconds + SeptemberSeconds + OctoberSeconds +
                                     NovemberSeconds + DecemberSeconds;

        long secondsPerLeapYear = JanuarySeconds + FebLeapSeconds + MarchSeconds + AprilSeconds + MaySeconds +
                                  JuneSeconds + JulySeconds + AugustSeconds + SeptemberSeconds + OctoberSeconds +
                                  NovemberSeconds + DecemberSeconds;
        int leapIndex = 2;
        boolean doYear = true;
        boolean doMonth = true;
        long[] monthSeconds = new long[12];
        monthSeconds[0] = JanuarySeconds;
        monthSeconds[2] = MarchSeconds;
        monthSeconds[3] = AprilSeconds;
        monthSeconds[4] = MaySeconds;
        monthSeconds[5] = JuneSeconds;
        monthSeconds[6] = JulySeconds;
        monthSeconds[7] = AugustSeconds;
        monthSeconds[8] = SeptemberSeconds;
        monthSeconds[9] = OctoberSeconds;
        monthSeconds[10] = NovemberSeconds;
        monthSeconds[11] = DecemberSeconds;

        String[] monthString = {
            "January", "Februrary", "March", "April", "May", "June", "July", "August", "September", "October",
            "November", "December"
        };
        monthIndex = 0;
        year = 1970;

        // This is the number of seconds since January 1, 1970
        // numberSeconds is meant to be unsigned, so convert to long
        longSeconds = numberSeconds;
        longSeconds = 0x0ffffffffL & longSeconds;

        while (doYear) {

            if ((leapIndex % 4) != 0) {

                if (longSeconds >= secondsPerNonLeapYear) {
                    year++;
                    longSeconds -= secondsPerNonLeapYear;
                    leapIndex++;
                } else {
                    doYear = false;
                }
            } // if ((leapIndex%4) != 0)
            else { // (leapIndex%4) == 0

                if (longSeconds >= secondsPerLeapYear) {
                    year++;
                    longSeconds -= secondsPerLeapYear;
                    leapIndex++;
                } else {
                    doYear = false;
                }
            }
        } // while (doYear)

        if ((leapIndex % 4) != 0) {
            monthSeconds[1] = FebNoLeapSeconds;
        } else {
            monthSeconds[1] = FebLeapSeconds;
        }

        month = monthString[0];

        while (doMonth) {

            if (longSeconds >= monthSeconds[monthIndex]) {
                longSeconds -= monthSeconds[monthIndex];
                monthIndex++;
                month = monthString[monthIndex];
            } else {
                doMonth = false;
            }
        } // while (doMonth);

        day = (int) (longSeconds / secondsPerDay) + 1;
        longSeconds = longSeconds - ((day - 1) * secondsPerDay);
        hour = (int) (longSeconds / secondsPerHour);
        longSeconds = longSeconds - (hour * secondsPerHour);
        minute = (int) (longSeconds / secondsPerMinute);
        seconds = (int) (longSeconds - (minute * secondsPerMinute));

        return;
    }
    
    public void anonymize() {
    	patientID = new String("XX-XX-XX-X   "); // length 13
    	patientName = new String("XXXXX, XXXXX             "); // length 25
    }
}
