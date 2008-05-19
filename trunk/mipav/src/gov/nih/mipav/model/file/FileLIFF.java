package gov.nih.mipav.model.file;


import gov.nih.mipav.model.structures.*;

import java.io.*;
import gov.nih.mipav.view.*;

/**
 
 */

public class FileLIFF extends FileBase {
    /* In 3 color images, if CY3, FITC, and DAPI are used, they are set to red, green,
     * and blue respectively.
     */
    
    private static final short kMasterImageLayer = 0;
    private static final short kGeneralImageLayer = 1;
    private static final short kBinaryLayer = 2;
    private static final short kRGBChannelLayer = 3;
    private static final short kRedChannelLayer = 4;
    private static final short kGreenChannelLayer = 5;
    private static final short kBlueChannelLayer = 6;
    private static final short kCyanChannelLayer = 7;
    private static final short kMagentaChannelLayer = 8;
    private static final short kYellowChannelLayer = 9;
    private static final short kBlackChannelLayer = 10;
    private static final short kLuminosityLayer = 11;
    private static final short kMaskLayer = 12;
    private static final short kDeepMaskLayer = 13;
    private static final short kAnnotationLayer = 14;
    private static final short kMovieLayer = 15;
    private static final short kDarkFieldLayer = 16;
    private static final short kBrightFieldLayer = 17;
    
    private static final int UNKNOWN = 0;
    // Note that types 1 thru 6 all have associated color tables in their PackBitsRect opcodes.
    // The color tables contain 3 USHORT values, ranging from 0 to 65535
    // For GREYS the 3 USHORT values are identical.
    // For COLORS the 3 USHORT values can be different.
    // For the below data types the most significant bit (MSB) corresponds to the leftmost
    // pixel.  The least significant bit (LSB) corresponds to the rightmost pixel.
    
    // k1IndexedGrayPixelFormat Each bit represents a pixel, which is used as an index
    // into the associated gray Color Table.  This is a legacy gray indexed format from
    // the Mac platform.
    private static final int MAC_1_BIT = 1;
    // k2IndexedGrayPixelFormat Each pixel is represented by two bits, which is used as an
    // index into the associated 2-bit gray Color Table.  This is a legacy gray indexed
    // format from the Mac platform.
    private static final int MAC_4_GREYS = 2;
    // k4IndexedGrayPixelFormat Each pixel is represented by four bits, which is used as
    // an index into the associated 4-bit gray Color Table.  This is a legacy gray 
    // indexed format from the Mac platform.
    private static final int MAC_16_GREYS = 3;
    // k4IndexedPixelFormat Each pixel is represented by four bits, which are used as
    // an index into the associated Color Table.  The four bit indexed format is native
    // to both the Mac and Win32 platforms.  The pixel defined by the most significant
    // four bits of a byte come before the pixel defined by the least significant four
    // bits.
    private static final int MAC_16_COLORS = 4;
    // k8IndexedGrayPixelFormat Each pixel is represented by eight bits, which is used as
    // an index into the associated 8-bit gray Color Table.  This is a legacy gray indexed
    // format from the Mac platform.
    private static final int MAC_256_GREYS = 5;
    // k8IndexedPixelFormat Each pixel is represented by eight bits, which are used as
    // an index into the associated Color Table.  The eight bit indexed format is native
    // to both Mac and Win32 platforms.
    private static final int MAC_256_COLORS = 6;
    // Have k16BE555PixelFormat  Each pixel is represented by 16 bits.  The MSB is unused,
    // followed by five bits per each Red, Green, and Blue Component.  This is the
    // native 16 bit format for the MAC platform.  For the MAC_16_BIT_COLOR
    // BIT DEPTH OF LAYER = 15 as expected and bits per component = 5 as expected.
    private static final int MAC_16_BIT_COLOR = 7;
    // LIFF file format documentation has openlab_mac32bitColourImageType = 8L,
    // openlab_mac24bitColourImageType = openlab_mac32bitColourImageType,
    // OpenlabReader.java has MAC_24_BIT_COLOR = 8;
    // k24RGBPixelFormat  Each pixel is represented by 24 bits.  Eight bits per
    // each Red, Green, and Blue Component.  This is the native 24 bit format
    // for the Mac platform.
    // k32ARGBPixelFormat Each pixel is represented by 32 bits.  Eight bits per
    // each Alpha, Red, Green, and Blue Component.  This is the native 32 bit
    // format for the Mac platform.
    private static final int MAC_24_BIT_COLOR = 8;
    private static final int DEEP_GREY_9 = 9;
    private static final int DEEP_GREY_10 = 10;
    private static final int DEEP_GREY_11 = 11;
    private static final int DEEP_GREY_12 = 12;
    private static final int DEEP_GREY_13 = 13;
    private static final int DEEP_GREY_14 = 14;
    private static final int DEEP_GREY_15 = 15;
    private static final int DEEP_GREY_16 = 16;
    
    private static final short kUnitsPixels = 1;
    private static final short kUnitsNanometers = 3;
    private static final short kUnitsMicrons = 4;
    private static final short kUnitsMillimeters = 5;
    private static final short kUnitsCentimeters = 6;
    private static final short kUnitsMeters = 7;
    private static final short kUnitsKilometers = 8;
    private static final short kUnused = 9;
    private static final short kUnitsInches = 10;
    private static final short kUnitsFeet = 11;
    private static final short kUnitsYards = 12;
    private static final short kUnitsMiles = 13;
    private static final short kUnused2 = 14;
    private static final short kUnitsOther = 15;
    
    private static final short NOP = 0x0000;
    private static final short Clip = 0x0001;
    private static final short TxFont = 0x0003;
    private static final short FillPat = 0x000A;
    private static final short TxSize = 0x000D;
    private static final short DefHilite = 0x001E;
    private static final short DHDVText = 0x002B;
    private static final short fontName = 0x002C;
    private static final short lineJustify = 0x002D;
    private static final short glyphState = 0x002E;
    private static final short eraseRect = 0x0032;
    private static final short PackBitsRect = 0x0098;
    private static final short DirectBitsRect = 0x009A;
    private static final short LongComment = 0x00A1;
    private static final short OpEndPic = 0x00FF;
    private static final short HeaderOp = 0x0C00;
    
    

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** DOCUMENT ME! */
    private File file;

    /** DOCUMENT ME! */
    private String fileDir;


    /** DOCUMENT ME! */
    private FileInfoLIFF fileInfo;

    /** DOCUMENT ME! */
    private String fileName;

    /** DOCUMENT ME! */
    private ModelImage image;

    /** DOCUMENT ME! */
    private int[] imageExtents = null;

    /** DOCUMENT ME! */
    private float[] imgBuffer = null;

    /** DOCUMENT ME! */
    private float[] imgResols = new float[5];

    /** DOCUMENT ME! */
    private ModelLUT LUT = null;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * LIFF reader/writer constructor.
     *
     * @param      fileName  file name
     * @param      fileDir   file directory
     *
     * @exception  IOException  if there is an error making the file
     */
    public FileLIFF(String fileName, String fileDir) throws IOException {

        this.fileName = fileName;
        this.fileDir = fileDir;
    }

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * Accessor that returns the file info.
     *
     * @return  FileInfoBase containing the file info
     */
    public FileInfoBase getFileInfo() {
        return fileInfo;
    }


    /**
     * Accessor that returns the image buffer.
     *
     * @return  buffer of image.
     */
    public float[] getImageBuffer() {
        return imgBuffer;
    }

    /**
     * Rreturns LUT if defined.
     *
     * @return  the LUT if defined else it is null
     */
    public ModelLUT getModelLUT() {
        return LUT;
    }
    
    /**
     * Reads the LIFF header which indicates endianess, the TIFF magic number, and the offset in bytes of the first IFD.
     * It then reads all the IFDs. This method then opens a Model of an image and imports the the images one slice at a
     * time. Image slices are separated by an IFD.
     *
     * @param      multiFile  <code>true</code> if a set of files each containing a separate 2D image is present <code>
     *                        false</code> if one file with either a 2D image or a stack of 2D images
     * @param      one        <code>true</code> if only want to read in one image of the 3D set
     *
     * @return     returns the image
     *
     * @exception  IOException  if there is an error reading the file
     */
    public ModelImage readImage(boolean multiFile, boolean one) throws IOException {
        long fileLength;
        boolean endianess;
        int i, j, k;
        short tagType;
        short subType;
        long nextOffset;
        String formatStr;
        long blkSize;
        int imageSlices = 0;
        byte isOpenlab2Header[] = new byte[1];
        byte spareByte[] = new byte[1];
        short sortTag;
        short layerID;
        short layerType;
        short layerDepth;
        short layerOpacity;
        short layerMode;
        byte selected[] = new byte[1];
        byte layerStoreFlag[] = new byte[1];
        byte layerPrintFlag[] = new byte[1];
        byte layerHasGWorld[] = new byte[1];
        int layerImage;
        int imageType;
        int redColor;
        int greenColor;
        int blueColor;
        int imageTypeCount[] = new int[17];
        int majorType = UNKNOWN;
        int majorTypeCount = 0;
        String[] typeStr = new String[17];
        typeStr[0] = "UNKNOWN";
        typeStr[1] = "MAC_1_BIT";
        typeStr[2] = "MAC_4_GREYS";
        typeStr[3] = "MAC_16_GREYS";
        typeStr[4] = "MAC_16_COLORS";
        typeStr[5] = "MAC_256_GREYS";
        typeStr[6] = "MAC_256_COLORS";
        typeStr[7] = "MAC_16_BIT_COLOR";
        typeStr[8] = "MAC_24_BIT_COLOR";
        typeStr[9] = "DEEP_GREY_9";
        typeStr[10] = "DEEP_GREY_10";
        typeStr[11] = "DEEP_GREY_11";
        typeStr[12] = "DEEP_GREY_12";
        typeStr[13] = "DEEP_GREY_13";
        typeStr[14] = "DEEP_GREY_14";
        typeStr[15] = "DEEP_GREY_15";
        typeStr[16] = "DEEP_GREY_16";
        int refCon;
        int layerTimeStamp;
        byte markDeleted[] = new byte[1];
        byte imageUpdated[] = new byte[1];
        int layerNameLength;
        byte prefix[] = new byte[1];
        String layerName;
        long microSecsTimeStamp;
        byte isBaseTimeLayer[] = new byte[1];
        byte spare[] = new byte[118];
        byte pad[];
        String appSignature;
        String kindSignature;
        int blkCount;
        int totalBlocks;
        int originalSize;
        int compressedSize;
        int picBlkSize;
        short bitDepth = 0;
        short bitShift;
        String platform;
        short units = 0;
        String unitStr[] = new String[16];
        unitStr[1] = "pixels";
        unitStr[3] = "nanometers";
        unitStr[4] = "microns";
        unitStr[5] = "millimeters";
        unitStr[6] = "centimeters";
        unitStr[7] = "meters";
        unitStr[8] = "kilometers";
        unitStr[9] = "unused";
        unitStr[10] = "inches";
        unitStr[11] = "feet";
        unitStr[12] = "yards";
        unitStr[13] = "miles";
        unitStr[14] = "unused2";
        unitStr[15] = "other";
        short calLayerID;
        byte square[] = new byte[1];
        byte positiveY[] = new byte[1];
        int otherUnitStrLength;
        String otherUnitString;
        int top;
        int left;
        int bottom;
        int right;
        int width;
        int height;
        int xDim = 0;
        int yDim = 0;
        long pictLocation[] = new long[24000];
        int  imageTypeLocation[] = new int[24000];
        short packTypeArray[] = new short[24000];
        short rowBytesArray[] = new short[24000];
        short pixelSizeArray[] = new short[24000];
        short boundsTopArray[] = new short[24000];
        short boundsBottomArray[] = new short[24000];
        short cmpCountArray[] = new short[24000];
        long LUTSizeLocation[] = new long[24000];
        double zStepArray[] = new double[1500];
        int zStepNumber = 0;
        boolean haveLUT = false;
        int dimExtentsLUT[];
        int subPictCount = 0;
        int pictCount = 0;
        String layerString[] = new String[10];
        int layerNumber = 0;
        int layerTypeArray[] = new int[10];
        int spaceIndex;
        boolean found = false;
        int bitNumber = 0;
        int pictureBytes;
        int version;
        int pictureVersion;
        int headerOpcode;
        short versionOpcode;
        int reserved;
        float bestHorizontalResolution;
        int bestHorizontalResolutionFract;
        short bestHorizontalResolutionShort;
        float bestVerticalResolution;
        int bestVerticalResolutionFract;
        short bestVerticalResolutionShort;
        float horizontalResolution;
        int horizontalResolutionFract;
        short horizontalResolutionShort;
        float verticalResolution;
        int verticalResolutionFract;
        short verticalResolutionShort;
        short hResShort;
        int hResFract;
        float hRes;
        short vResShort;
        int vResFract;
        float vRes;
        short xTopLeft;
        short yTopLeft;
        short xBottomRight;
        short yBottomRight;
        int opcode;
        int fillPat1;
        int fillPat2;
        int fillPat3;
        int fillPat4;
        int dataLength;
        int oldFontID;
        int nameLength;
        String fontNameStr;
        int fontNumber;
        int textSize;
        byte outlinePreferred[] = new byte[1];
        byte preserveGlyph[] = new byte[1];
        byte fractionalWidths[] = new byte[1];
        byte scalingDisabled[] = new byte[1];
        int dh;
        int dv;
        int count;
        String textStr;
        int commentKind;
        int commentSize;
        long baseAddr;
        short rowBytes = 0;
        short pmVersion;
        short packType;
        long packSize;
        short pixelType;
        short pixelSize;
        short cmpCount;
        short cmpSize;
        long planeBytes;
        long pmTable;
        int pmReserved;
        String [] modeStr = new String[65];
        // The 16 transfer modes
        modeStr[0] = new String("srcCopy");
        modeStr[1] = new String("srcOr");
        modeStr[2] = new String("srcXor");
        modeStr[3] = new String("srcBic");
        modeStr[4] = new String("notSrcCopy");
        modeStr[5] = new String("notSrcOr");
        modeStr[6] = new String("notSrcXor");
        modeStr[7] = new String("notSrcBic");
        modeStr[8] = new String("patCopy");
        modeStr[9] = new String("patOr");
        modeStr[10] = new String("patXor");
        modeStr[11] = new String("patBic");
        modeStr[12] = new String("notPatCopy");
        modeStr[13] = new String("notPatOr");
        modeStr[14] = new String("notPatXor");
        modeStr[15] = new String("notPatBic");
        // 2 special text transfer modes
        modeStr[49] = new String("grayishTextOr");
        modeStr[50] = new String("hilitetransfermode");
        // 2 arithmetic transfer modes
        modeStr[32] = new String("blend");
        modeStr[33] = new String("addPin");
        // 6 QuickDraw color separation constants
        modeStr[34] = new String("addOver");
        modeStr[35] = new String("subPin");
        modeStr[37] = new String("addMax");
        modeStr[38] = new String("subOver");
        modeStr[39] = new String("adMin");
        modeStr[64] = new String("ditherCopy");
        // Transparent mode constant
        modeStr[36] = new String("transparent");
        short mode;
        short boundsTop;
        short boundsLeft;
        short boundsBottom;
        short boundsRight;
        int byteCount;
        int regionSize;
        short clipTop;
        short clipLeft;
        short clipBottom;
        short clipRight;
        int totalByteCount = 0;
        short headerVersion;
        short headerReservedShort;
        int headerReservedInt;
        int userTagNum = 0;
        String className;
        short numVars;
        double doubleValue = 0.0;
        byte derivedClassVersion[] = new byte[1];
        byte baseClassVersion[] = new byte[1];
        int strSize;
        String nameStr;
        String stringValue;
        int unitsOfMeasure[];
        long commentPointer;
        float origin[] = new float[4];
        short shortBuffer[] = null;
        int index;
        byte sliceColorBuffer[] = null;
        int sliceColorBytes = 0;
        int color = 1;
        int len;
        int sliceNumber = 0;
        int majorLayerNumber = 0;
        String majorLayerString[] = new String[10];
        boolean doDeepGreyColor = false;
        boolean haveCY3 = false;
        boolean haveFITC = false;
        boolean haveDAPI = false;
        int colorSequence[] = new int[3];
        int ctSeed;
        short ctFlags;
        short ctSize;
        int rowBytesRead = 0;
        int rowIndex = 0;
        byte b1;
        byte byteBuffer[] = null;
        byte byteBuffer2[] = null;
        int component = 0;
        int componentArray[];
        int sliceBytes = 0;
        int maxValue;
        int rightShift;
        int sliceSize = 0;
        short shortColor;
        int bufferSize;
        boolean booleanBuffer[] = null;
        int x;
        int y;
        int xDimArray[] = new int[17];
        int yDimArray[] = new int[17];
        int rowBytesTypeArray[] = new int[17];
        int jstart;
        boolean sameStep;
        int uncompressedSize;
        int dataWidth;
        byte compressedBuffer[];
        byte uncompressedBuffer[];
        LZOCodec lzo;
        int srcIndex;
        int destIndex;

        try {
            imgResols[0] = imgResols[1] = imgResols[2] = imgResols[3] = imgResols[4] = (float) 1.0;
            file = new File(fileDir + fileName);
            raFile = new RandomAccessFile(file, "r");
            
            fileLength = raFile.length();
            
            int byteOrder = raFile.readInt();

            if (byteOrder == 0xffff0000) {
                endianess = FileBase.LITTLE_ENDIAN;
                Preferences.debug("\nByte order in unexpectedly little-endian\n");
            } else if (byteOrder == 0x0000ffff) {
                endianess = FileBase.BIG_ENDIAN;
                Preferences.debug("\nByte order is the expected big-endian (Macintosh)\n");
            } else {
                raFile.close();
                throw new IOException("LIFF Read Header: Error - first 4 bytes are an illegal " + byteOrder);
            }
            
            fileInfo = new FileInfoLIFF(fileName, fileDir, FileUtility.LIFF); // dummy fileInfo
            fileInfo.setEndianess(endianess);
            
            String sigStr = getString(4);
            if (sigStr.equals("impr")) {
                Preferences.debug("sigBytes field is properly set to impr\n");
            }
            else {
                Preferences.debug("sigBytes field is an unexpected " + sigStr + "\n");
                raFile.close();
                throw new IOException("sigBytes filed is an unexpected " + sigStr);
            }
            
            int versionNumber = getInt(endianess);
            Preferences.debug("Version number of the LIFF format is " + versionNumber + "\n");
            
            // layerCount is the total number of tag blocks in the file of all types.  There
            // may actually be fewer actual layers than this, but not more.
            // When parsing the file, it is more reliable to read until there is no more data
            // rather than rely on the layerCount value.
            int layerCount = getUnsignedShort(endianess);
            Preferences.debug("Total number of tag blocks of all types = " + layerCount + "\n");
            
            int layerIDSeed = getUnsignedShort(endianess);
            Preferences.debug("Seed for layer IDs = " + layerIDSeed + "\n");
            
            long firstTagOffset = getUInt(endianess);
            Preferences.debug("Absolute offset of first tag block is " + firstTagOffset + "\n");
            
            for (nextOffset = firstTagOffset, i = 1; nextOffset < fileLength-1; i++) {
                raFile.seek(nextOffset);
                Preferences.debug("Reading tag " + i + "\n"); 
                // An image layer will have a tag ID of 67 or 68 (the two types are identical;
                // for historical reasons the redundancy here has not been removed.)
                tagType = readShort(endianess);
                if ((tagType == 67) || (tagType == 68)) {
                    Preferences.debug("Tag type = " + tagType + " indicates image layer\n");
                    imageSlices++;
                }
                else if (tagType == 69) {
                    Preferences.debug("Tag type = " + tagType + " indicates calibration\n");
                }
                else if (tagType == 72) {
                    Preferences.debug("Tag type = " + tagType + " indicates user\n");
                }
                else {
                    Preferences.debug("Tag type = " + tagType + "\n");
                }
                
                subType = readShort(endianess);
                // For image tags, this will generally be set to zero.
                Preferences.debug("Subtype ID = " + subType + "\n");
                
                // nextOffset is the absolute location of the next tag header
                if (versionNumber <= 2) {
                    nextOffset = getUInt(endianess);    
                }
                else {
                    nextOffset = readLong(endianess);
                }
                Preferences.debug("Absolute location of next tag header = " + nextOffset + "\n");
                
                formatStr = getString(4);
                if ((tagType == 67) || (tagType == 68)) {
                    // This field will most often contain 'PICT', indicating that the image data
                    // is a Macintosh Picture.  For Openlab 5 LIFF files this will be a 'RAWi'
                    // type - this is a compressed raw image instead of PICT data.
                    Preferences.debug("Format of the data in the image tag is " + formatStr + "\n");
                }
                else if (tagType == 69) {
                    if (formatStr.equals("cali")) {
                        Preferences.debug("calibration tag type has expected format string of cali\n");
                    }
                    else {
                        Preferences.debug("calibration tag type unexpectedly has format string of " 
                                          + formatStr + "\n");    
                    }
                }
                else if (tagType == 72) {
                    // Have seen "USER"
                    Preferences.debug("user tag type has format string of " + formatStr + "\n");
                }
                
                if (versionNumber <= 2) {
                    blkSize = getUInt(endianess);
                }
                else {
                    blkSize = readLong(endianess);
                }
                // The blkSize field does not include the layerinfo record for 
                // tag types 67 and 68
                Preferences.debug("Number of bytes in this block = " + blkSize + "\n");
                if ((tagType == 67) || (tagType == 68)) {
                    // Read layerinfo record if image tag
                    // isOpenlab2Header is set to true for files written with Openlab 2.0 and higher.
                    // It indicates that the layer name is only 127 characters instead of 255 and that
                    // there is aditional info at the end of the header.
                    raFile.read(isOpenlab2Header);
                    if (isOpenlab2Header[0] == 1) {
                        Preferences.debug("This is an Openlab 2.x header\n");
                    }
                    else {
                        Preferences.debug("This is not an Openlab 2.x header\n");
                    }
                    // There is a spare byte between isOpenlab2Header and sortTag
                    raFile.read(spareByte);
                    // sortTag is no longer used.  Should be set to zero
                    sortTag = readShort(endianess);
                    if (sortTag == 0) {
                        Preferences.debug("sortTag is 0 as expected\n");    
                    }
                    else {
                        Preferences.debug("sortTag unexpectedly = " + sortTag + "\n");
                    }
                    layerID = readShort(endianess);
                    Preferences.debug("The ID number for the layer = " + layerID + "\n");
                    layerType = readShort(endianess);
                    switch (layerType) {
                        case kMasterImageLayer:
                            // This type is no longer used in Openlab or other Improvision software
                            // If Openlab encounters a layer with this type in a LIFF file, it
                            // will convert it to kGeneralImageLayer
                            Preferences.debug("kMasterImageLayer, which is no longer used\n");
                            break;
                        case kGeneralImageLayer:
                            // This is a layer containing an image with depth, colors, etc.
                            // It has no special properties.  Most layers will be of this type.
                            Preferences.debug("kGeneralImageLayer, an image with any depth, colors, etc\n");
                            break;
                        case kBinaryLayer:
                            // This layer contains only binary image data.  Its bit depth is always 1.
                            // The image itself can be a bitmap or a pixel map (usually the latter if
                            // GWorlds is used).  When rendering, the opacity shoudl be ignored, but 
                            // the mode examined.  Acceptable modes are srcOr(transparent rendering) or
                            // srcCopy (opaque rendering).  You can also use inverted modes if you need
                            // to.  The layerColour attribute should also be used to tint the bitmap
                            // when rendered.  This is very simple if using CopyBits, etc., just set
                            // the ForeColor to layerColour before calling it.
                            Preferences.debug("kBinaryLayer contains only binary image data\n");
                            break;
                        case kRGBChannelLayer:
                            // This type is not currently used.
                            Preferences.debug("kRGBChannelLayer not currently used\n");
                            break;
                        case kRedChannelLayer:
                            // This is an image filtered to display only the red channel of the RGB image.
                            // The image itself will be full color.  It is up to you to apply the correct
                            // filtering when rendering a layer of this type.
                            Preferences.debug("kRedChannelLayer filtered to only display the red channel\n");
                            break;
                        case kGreenChannelLayer:
                            // Same as above, but for green channel.
                            Preferences.debug("kGreenChannelLayer filtered to only display the green channel\n");
                            break;
                        case kBlueChannelLayer:
                            Preferences.debug("kBlueChannelLayer filtered to only display the blue channel\n");
                            break;
                        case kCyanChannelLayer:
                            Preferences.debug("kCyanChannelLayer\n");
                            break;
                        case kMagentaChannelLayer:
                            Preferences.debug("kMagentaChannelLayer\n");
                            break;
                        case kYellowChannelLayer:
                            // Same as above, etc.
                            Preferences.debug("kYellowChannelLayer\n");
                            break;
                        case kBlackChannelLayer:
                            // Similar to above, but it black wherever there is color in the original, and
                            // white where there is white.  (Alternatively, you can allow this to mean black
                            // where the original is black, and white elsewhere - the convention is invoked
                            // by the application, not by anything inherent in the file).
                            Preferences.debug("kBlackChannelLayer\n");
                            break;
                        case kLuminosityLayer:
                            // A grayscale representation of the master image (8-bit) that maps the relative
                            // luminosity of the colors to the shade of gray.
                            Preferences.debug("kLuminosityLayer\n");
                            break;
                        case kMaskLayer:
                            Preferences.debug("kMaskLayer\n");
                            break;
                        case kDeepMaskLayer:
                            Preferences.debug("kDeepMaskLayer\n");
                            break;
                        case kAnnotationLayer:
                            // A layer (generally 8-bit, though not enforced) that can be used to add
                            // annotations to an image.  Usually, such layers will be transparent by 
                            // default.
                            Preferences.debug("kAnnotationLayer\n");
                            break;
                        case kMovieLayer:
                            // A layer type reserved for animated or live images.  LIFF files should
                            // generally not contain this type.  If Openlab encounters a layer with 
                            // this type in a LIFF file, it will ignore it.
                            Preferences.debug("kMovieLayer\n");
                            break;
                        case kDarkFieldLayer:
                            Preferences.debug("kDarkFieldLayer\n");
                            break;
                        case kBrightFieldLayer:
                            Preferences.debug("kBrightFieldLayer\n");
                            break;
                        default:
                            Preferences.debug("layerType has unrecognized value = " + layerType + "\n");
                    } // switch (layerType)
                    // layerDepth is the bit-depth of the layer, where this makes sense.  Note that
                    // vector-based layers have no inherent bit-depth as they are rendered to the
                    // current window when needed.  This can be 1, 2, 4, 8, 15, 32, or zero.  LIFF files
                    // may also contain "deep gray" image data embedded within the following PICT.  If
                    // this is the case, this field may contain values 9, 10, 11, 12, 13, 14, or 16.
                    // Note that 15 bit deep-grey data is not supported for historical reasons.
                    layerDepth = readShort(endianess);
                    Preferences.debug("Bit depth of the layer = " + layerDepth + "\n");
                    // layerOpacity is the relative percentage opacity of the layer.  It is an integer
                    // from 0 to 100.  A value of 100 indicates totally opaque, and a value of 0
                    // totally transparent.  Note that the actual value of opacity can be modified
                    // by the layer type field.
                    layerOpacity = readShort(endianess);
                    Preferences.debug("Percentage opacity of the layer = " + layerOpacity + "\n");
                    // layerMode is the drawing mode that the layer uses to render its image.  It is
                    // a QuickDraw mode constant, such as srcCopy, srcOr, etc.  In general, this field
                    // should not be relied on as an absolute indicator of the drawing mode - this 
                    // should ideally be determined on the fly from other factors.
                    layerMode = readShort(endianess);
                    if ((layerMode >= 0) && (layerMode <= 64) && (modeStr[layerMode] != null)) {
                        Preferences.debug("layerMode = " + modeStr[layerMode] + "\n");
                    }
                    else {
                        Preferences.debug("layerMode has unrecognized value = " + layerMode + "\n");    
                    }
                    // selected ignore;  set to true if this layer is the 'current' layer in
                    // the Layers Manager
                    raFile.read(selected);
                    // layerStoreFlag ignore; set to zero
                    raFile.read(layerStoreFlag);
                    // layerPrintFlag ignore; set to zero
                    raFile.read(layerPrintFlag);
                    // layerHasGWorld ignore; set to zero
                    raFile.read(layerHasGWorld);
                    // GWorldPtr layerImage ignore; set to zero
                    layerImage = readInt(endianess);
                    imageType = readInt(endianess);
                    if ((imageType >= 0) && (imageType <= 16)) {
                        Preferences.debug("Image type = " + typeStr[imageType] + "\n");
                    }
                    else {
                        Preferences.debug("imageType has unrecognized value = " + imageType + "\n");
                        raFile.close();
                        throw new IOException("imageType has unrecognized value = " + imageType);    
                    }
                    imageTypeCount[imageType]++;
                    // read RGBColor =lyaerColor, which consists of 3 16-bit unsigned integers
                    // for red, green, and blue.  For binary layers (layerType == kBinaryLayer), this
                    // is the color that should be used to render the bitmap with.  The user chose
                    // this color to represent that layer.  For other layer types this may not be
                    // defined, and should be set to black.
                    redColor = getUnsignedShort(endianess);
                    greenColor = getUnsignedShort(endianess);
                    blueColor = getUnsignedShort(endianess);
                    if (layerType == kBinaryLayer) {
                        Preferences.debug("redColor = " + redColor + "\n");
                        Preferences.debug("greenColor = " + greenColor + "\n");
                        Preferences.debug("blueColor = " + blueColor + "\n");
                    } // if (layerType == kBinaryLayer)
                    // ignore refCon; set to zero
                    refCon = readInt(endianess);
                    // A layer may represent a portion of a sequence captured at a particular 
                    // time value.  This field contains the time value from the start of the 
                    // sequence.  The time value is in ticks (60ths of a second).  This value
                    // was replaced in Openlab 2.0 and higher by a microseconds timestamp.
                    layerTimeStamp = readInt(endianess);
                    if (isOpenlab2Header[0] == 0) {
                        // Timestamp only used for pre-Openlab 2.0 files
                        Preferences.debug("Time from start of sequence in ticks = " + layerTimeStamp + "\n");
                    }
                    // markDeleted ignore; set to zero
                    raFile.read(markDeleted);
                    // imageUpdated ignore; set to zero
                    raFile.read(imageUpdated);
                    // Read length of layerName string
                    raFile.read(prefix);
                    layerNameLength = prefix[0] & 0xff;
                    Preferences.debug("layer name length = " + layerNameLength + "\n");
                    layerName = getString(layerNameLength);
                    Preferences.debug("layer name = " + layerName.trim() + "\n");
                    if (!layerName.toUpperCase().trim().equals("ORIGINAL IMAGE")) {
                        spaceIndex = layerName.indexOf(" ");
                        layerName = layerName.substring(0, spaceIndex);
                        Preferences.debug("layerName = " + layerName + "\n");
                        found = false;
                        for (j = 0; j < layerNumber && (!found); j++) {
                           if (layerName.equals(layerString[j])) {
                               found = true;
                           }
                        }
                        if (!found) {
                            layerTypeArray[layerNumber] = imageType;
                            layerString[layerNumber++] = layerName;
                        }
                    } // if (!layerName.trim().equals("Original image"))
                    if (isOpenlab2Header[0]  == 1) {
                        // This section only exists if isOpenlab2Header is true
                        if ((128 - (layerNameLength + 1)) > 0) {
                            pad = new byte[128 - (layerNameLength + 1)];
                            raFile.read(pad);
                        }
                        // microSecsTimeStamp is a 64 bit long value giving the absolute timestamp of
                        // the layer in microseconds since January 1, 1904.
                        // Note that java.util.date is a long integer giving milliseconds since
                        // January 1, 1970.
                        microSecsTimeStamp = readLong(endianess);
                        Preferences.debug("microSecsTimeStamp = " + microSecsTimeStamp + "\n");
                        // isBaseTimeLayer is set to true if this layer is being used as the timebase
                        // for relative layer times
                        raFile.read(isBaseTimeLayer);
                        if (isBaseTimeLayer[0] == 1) {
                            Preferences.debug("This layer is being used as the timebase for relative layer times\n");    
                        }
                        else {
                            Preferences.debug("This layer is not being used as the timebase for relative layer times\n");
                        }
                        // read in 1 byte for alignment
                        raFile.read(spareByte);
                        // spare space set to zero when writing files
                        raFile.read(spare);
                    } // if (isOpenlab2Header[0] == 1)
                    else { // isOpenlab2header[0] == 0
                        if ((256 - (layerNameLength + 1)) > 0) {
                            pad = new byte[256 - (layerNameLength + 1)];
                            raFile.read(pad);
                        }
                    } // else isOpenlab2Header[0] == 0
                    if (versionNumber <= 2) {
                        // picSize record
                        // picture size; don't use this value for picture size
                        pictureBytes  = getUnsignedShort(endianess);
                        Preferences.debug("Picture size in bytes = " + pictureBytes + "\n");
                        top = getUnsignedShort(endianess);
                        Preferences.debug("top = " + top + "\n");
                        left = getUnsignedShort(endianess);
                        Preferences.debug("left = " + left + "\n");
                        bottom = getUnsignedShort(endianess);
                        Preferences.debug("bottom = " + bottom + "\n");
                        right = getUnsignedShort(endianess);
                        Preferences.debug("right = " + right + "\n");
                        width = right - left;
                        Preferences.debug("width = " + width + "\n");
                        xDimArray[imageType] = width;
                        height = bottom - top;
                        Preferences.debug("height = " + height + "\n");
                        yDimArray[imageType] = height;
                        // picFrame (PICT v2.0) record
                        // version should be 0x0011
                        version = getUnsignedShort(endianess);
                        if (version == 0x0011) {
                            Preferences.debug("picFrame version is 0x0011 as expected\n");
                        }
                        else {
                            Preferences.debug("picFrame version is unexpectedly " + version + "\n");
                        }
                        // Picture version should be 0x02ff
                        pictureVersion = getUnsignedShort(endianess);
                        if (pictureVersion == 0x02ff) {
                            Preferences.debug("Picture version is 0x02ff as expected\n");
                        }
                        else {
                            Preferences.debug("Picture version is unexpectedly " + pictureVersion + "\n");
                        }
                        // Header opcode should be 0x0C00 in an extended version 2 or a version 2
                        // format picture
                        headerOpcode = getUnsignedShort(endianess);
                        if (headerOpcode == 0x0C00) {
                            Preferences.debug("Header opcode is 0x0C00 as expected\n");
                        }
                        else {
                            Preferences.debug("Header opcode is unexpectedly " + headerOpcode + "\n");
                        }
                        // The version opcode has a value of -2 for an extneded version 2 picture and a
                        // value of -1 for a version 2 picture.  The rest of the header for an extended
                        // version 2 picture contains resolution information;  the reset of the header
                        // for a version 2 picture specifies a fixed-point bounding box.
                        versionOpcode = readShort(endianess);
                        if (versionOpcode == -2) {
                            Preferences.debug("The version opcode = -2 indicates an extended version 2 picture\n");
                        }
                        else if (versionOpcode == -1) {
                            Preferences.debug("The version opcode = -1 indicates a version 2 picture\n");
                        }
                        else {
                            Preferences.debug("The version opcode is unexpectedly = " + versionOpcode + "\n");
                        }
                        // reserved should be 0
                        reserved = getUnsignedShort(endianess);
                        Preferences.debug("reserved = " + reserved + "\n");
                        bestHorizontalResolutionShort = readShort(endianess);
                        bestHorizontalResolutionFract = getUnsignedShort(endianess);
                        bestHorizontalResolution = (float)(bestHorizontalResolutionShort + 
                                                    Math.pow(2.0,-16.0)*bestHorizontalResolutionFract);
                        Preferences.debug("Best horizontal resolution = " +
                                          bestHorizontalResolution + " pixels per inch\n");
                        bestVerticalResolutionShort = readShort(endianess);
                        bestVerticalResolutionFract = getUnsignedShort(endianess);
                        bestVerticalResolution = (float)(bestVerticalResolutionShort + 
                                                    Math.pow(2.0,-16.0)*bestVerticalResolutionFract);
                        Preferences.debug("Best vertical resolution = " +
                                          bestVerticalResolution + " pixels per inch\n");
                        yTopLeft = readShort(endianess);
                        Preferences.debug("y top left = " + yTopLeft + "\n");
                        xTopLeft = readShort(endianess);
                        Preferences.debug("x top left = " + xTopLeft + "\n");
                        yBottomRight = readShort(endianess);
                        Preferences.debug("y bottom right = " + yBottomRight + "\n");
                        xBottomRight = readShort(endianess);
                        Preferences.debug("x bottom right = " + xBottomRight + "\n");
                        // reserved should be 0
                        reserved = readInt(endianess);
                        Preferences.debug("reserved = " + reserved + "\n");
                        found = false;
                        while (!found) {
                            opcode = getUnsignedShort(endianess);
                            switch (opcode) {
                                case NOP:
                                    Preferences.debug("NOP\n");
                                    break;
                                case Clip:
                                    Preferences.debug("Clip: Clipping region\n");
                                    regionSize = getUnsignedShort(endianess);
                                    // For rectangular regions (or empty regions), the region size
                                    // field contains 10.
                                    Preferences.debug("Size in bytes of clipping record = " + regionSize + "\n");
                                    // Enclosing rectangle
                                    clipTop = readShort(endianess);
                                    Preferences.debug("Top of clipping rectangle = " + clipTop + "\n");
                                    clipLeft = readShort(endianess);
                                    Preferences.debug("Left of clipping rectangle = " + clipLeft + "\n");
                                    clipBottom = readShort(endianess);
                                    Preferences.debug("Bottom of clipping rectangle = " + clipBottom + "\n");
                                    clipRight = readShort(endianess);
                                    Preferences.debug("Right of clipping rectangle = " + clipRight + "\n");
                                    if (regionSize > 10) {
                                        pad = new byte[regionSize - 10];
                                        raFile.read(pad);
                                        if (((regionSize - 10) % 2) == 1) {
                                            raFile.read(spareByte);
                                        }
                                    }
                                    break;
                                case TxFont:
                                    Preferences.debug("TxFont: Font number for text\n");
                                    fontNumber = getUnsignedShort(endianess);
                                    Preferences.debug("Font number = " + fontNumber + "\n");
                                    break;
                                case FillPat:
                                    Preferences.debug("FillPat: Fill pattern\n");
                                    fillPat1 = getUnsignedShort(endianess);
                                    Preferences.debug("fill pattern 1 = " + fillPat1 + "\n");
                                    fillPat2 = getUnsignedShort(endianess);
                                    Preferences.debug("fill pattern 2 = " + fillPat2 + "\n");
                                    fillPat3 = getUnsignedShort(endianess);
                                    Preferences.debug("fill pattern 3 = " + fillPat3 + "\n");
                                    fillPat4 = getUnsignedShort(endianess);
                                    Preferences.debug("fill pattern 4 = " + fillPat4 + "\n");
                                    break;
                                case TxSize:
                                    Preferences.debug("TxSize: Text size\n");
                                    textSize = getUnsignedShort(endianess);
                                    Preferences.debug("Text size = " + textSize + "\n");
                                    break;
                                case DefHilite:
                                    Preferences.debug("DefHilite: use default highlight color\n");
                                    break;
                                case DHDVText:
                                    Preferences.debug("DHDVText\n");
                                    raFile.read(prefix);
                                    dh = prefix[0] & 0xff;
                                    Preferences.debug("dh = " + dh + "\n");
                                    raFile.read(prefix);
                                    dv = prefix[0] & 0xff;
                                    Preferences.debug("dv = " + dv + "\n");
                                    raFile.read(prefix);
                                    count = prefix[0] & 0xff;
                                    Preferences.debug("count = " + count + "\n");
                                    textStr = getString(count);
                                    Preferences.debug("DHDVText string = " + textStr + "\n");
                                    if (((3 + count) % 2)  == 1) {
                                        raFile.read(spareByte);
                                    }
                                    break;
                                case fontName:
                                    Preferences.debug("fontName\n");
                                    dataLength = getUnsignedShort(endianess);
                                    Preferences.debug("Data length = " + dataLength + "\n");
                                    oldFontID = getUnsignedShort(endianess);
                                    Preferences.debug("old font ID = " + oldFontID + "\n");
                                    raFile.read(prefix);
                                    nameLength = prefix[0] & 0xff;
                                    Preferences.debug("name length = " + nameLength + "\n");
                                    fontNameStr = getString(nameLength);
                                    Preferences.debug("font name = " + fontNameStr + "\n");
                                    if (((5 + nameLength) % 2) == 1) {
                                        raFile.read(spareByte);
                                    }
                                    break;
                                case lineJustify:
                                    Preferences.debug("lineJustify\n");
                                    dataLength = getUnsignedShort(endianess);
                                    Preferences.debug("Data length = " + dataLength + "\n");
                                    // 2 fixed numbers
                                    // intercharacter spacing
                                    // total extra space for justification
                                    readLong(endianess);
                                    readLong(endianess);
                                    break;
                                case glyphState:
                                    Preferences.debug("glyphState\n");
                                    dataLength = getUnsignedShort(endianess);
                                    Preferences.debug("Data length = " + dataLength + "\n");
                                    if (dataLength >= 1) {
                                        raFile.read(outlinePreferred);
                                        if (outlinePreferred[0] == 1) {
                                            Preferences.debug("Outline preferred\n");
                                        }
                                        else {
                                            Preferences.debug("Outline not preferred\n");
                                        }
                                    } // if (dataLength >= 1)
                                    if (dataLength >= 2) {
                                        raFile.read(preserveGlyph);
                                        if (preserveGlyph[0] == 1) {
                                            Preferences.debug("Preserve glyph");
                                        }
                                        else {
                                            Preferences.debug("Don't preserve glyph\n");
                                        }
                                    } // if (dataLength >= 2)
                                    if (dataLength >= 3) {
                                        raFile.read(fractionalWidths);
                                        if (fractionalWidths[0] == 1) {
                                            Preferences.debug("Fractional widths\n");
                                        }
                                        else {
                                            Preferences.debug("No fractional widths\n");
                                        }
                                    } // if (dataLength >= 3)
                                    if (dataLength >= 4) {
                                        raFile.read(scalingDisabled);
                                        if (scalingDisabled[0] == 1) {
                                            Preferences.debug("Scaling disabled\n");
                                        }
                                        else {
                                            Preferences.debug("Scaling not disabled\n");
                                        }
                                    } // if (dataLength >= 4)
                                    if ((dataLength % 2) == 1) {
                                        raFile.read(spareByte);
                                    }
                                    break;
                                case eraseRect:
                                    Preferences.debug("eraseRect\n");
                                    yTopLeft = readShort(endianess);
                                    Preferences.debug("y top left = " + yTopLeft + "\n");
                                    xTopLeft = readShort(endianess);
                                    Preferences.debug("x top left = " + xTopLeft + "\n");
                                    yBottomRight = readShort(endianess);
                                    Preferences.debug("y bottom right = " + yBottomRight + "\n");
                                    xBottomRight = readShort(endianess);
                                    Preferences.debug("x bottom right = " + xBottomRight + "\n");
                                    break;
                                case PackBitsRect:
                                    Preferences.debug("PackBitsRect\n");
                                    // PixMap, ColorTable, srcRect, dstRect, mode(short), PixData
                                    // rowBytes, The offset in bytes from one row of the image to the next.
                                    // The value must be even, less than $4000, and for best performance it
                                    // should be a multiple of 4.  The high 2 bits of rowBytes are used as
                                    // flags.  If bit 15 = 1, the data structure pointed to is a PixMap 
                                    // record; otherwise it is a bitMap record.
                                    rowBytes = readShort(endianess);
                                    if ((rowBytes & 0x8000) != 0) {
                                        Preferences.debug("The data structure pointed to is a PixMap record\n");
                                    }
                                    else {
                                        Preferences.debug("The data structure pointed to is a BitMap record\n");
                                    }
                                    // Strip out the 2 flag bits
                                    rowBytes = (short)(rowBytes & 0x3fff);
                                    Preferences.debug("Offset in bytes from one row of the image to the next = "
                                                       + rowBytes + "\n");
                                    // The boundary rectangle, which links the local coordinate system of a
                                    // graphics port to QuickDraw's global coordinate system and defines the
                                    // area of the bit image into which QuickDraw can draw.  By default,
                                    // the boundary rectangle is the entire main screen.
                                    boundsTop = readShort(endianess);
                                    Preferences.debug("Boundary rectangle y top left = " + boundsTop + "\n");
                                    boundsLeft = readShort(endianess);
                                    Preferences.debug("Boundary rectangle x top left = " + boundsLeft + "\n");
                                    boundsBottom = readShort(endianess);
                                    Preferences.debug("Boundary rectangle y bottom right = " + boundsBottom + "\n");
                                    boundsRight = readShort(endianess);
                                    Preferences.debug("Boundary rectangle x bottom right = " + boundsRight + "\n");
                                    // PixMap record version number.  The version number of Color QuickDraw
                                    // that created this pixMap record.  The value of pmVersion is normally 0.
                                    // If pmVersion is 4, Color QuickDraw treats the pixMap reocrd's baseAddr
                                    // field as 32-bit clean.  (All other flags are private.)
                                    pmVersion = readShort(endianess);
                                    Preferences.debug("The PixMap record version number = " + pmVersion + "\n");
                                    // The packing algorithm used to compress image data.
                                    // To facilitate banding fo images when memory is short, all data
                                    // compression is done on a scan-line basis.  The following pseudocode
                                    // describes the pixel data:
                                    // PixData
                                    // if packType = 1 (unpacked) or rowBytes < 8 then
                                    //     data is unpacked;
                                    //     data size = rowBytes * (bounds.bottom - bounds.top);
                                    
                                    // if packType = 2 (drop pad byte) then
                                    //     the high-order pad byte of a 32-bit direct pixel is dropped;
                                    //     data size = (3/4) * rowBytes * (bounds.bottom - bounds.top);
                                    
                                    // if packType > 2 (packed) then
                                    //    image contains (bounds.bottom - bounds.top) packed scan lines;
                                    //    each scan line consists of [byteCount] [data];
                                    //    if rowBytes > 250 then
                                    //        byteCount is a short
                                    //    else
                                    //        byteCount is a byte
                                    // Here are the currently defined packing types:
                                    // Packing type    Meaning
                                    // 0               Use default packing
                                    //                 The default for a pixelSize value of 16 is 3
                                    //                 The default for a pixelSize value of 32 is 4
                                    // 1               Use no packing
                                    // 2               Remove pad byte -- supported only for 32-bit pixels
                                    //                 (24-bit data)
                                    // 3               Run length encoding by pixelSize chunks, one scan line
                                    //                 at a time -- supported only for 16-bit pixels.
                                    // 4               Run length encoding one component at a time, one scan
                                    //                 line at a time, red component first--supported only for
                                    //                 32-bit pixels (24-bit data)
                                    // Each scan line of packed data is preceded by a byte or a short
                                    // giving the count of data
                                    // When the pixel type is direct, cmpCount * cmpSize is less than or equal
                                    // to pixelSize.  For storing 24-bit data in a 32-bit pixel, set cmpSize
                                    // to 8 and cmpCount to 3.  If you set cmpCount to 4, then the high byte
                                    // is compressed by packing scheme 4 and stored in the picture.
                                    packType = readShort(endianess);
                                    Preferences.debug("The pack type used to compress image data = " +
                                                       packType + "\n");
                                    // The size of the packed image in bytes.  Since each scan line of 
                                    // packed data is preceded by a byte count, packSize is not used and
                                    // must be 0 for future compatibility.
                                    packSize = getUInt(endianess);
                                    if (packSize == 0) {
                                        Preferences.debug("The field for the size of the packed image in bytes " 
                                                    +  "is set to 0 as expected\n");
                                    }
                                    else {
                                        Preferences.debug("The size of the packed image in bytes = " 
                                             + packSize + "\n");         
                                    }
                                    // The horizontal resolution of the pixel image in pixels per inch.
                                    // This value is of type Fixed; by default, the value here is
                                    // $00480000(for 72 pixels per inch).
                                    horizontalResolutionShort = readShort(endianess);
                                    horizontalResolutionFract = getUnsignedShort(endianess);
                                    horizontalResolution = (float)(horizontalResolutionShort + 
                                                                Math.pow(2.0,-16.0)*horizontalResolutionFract);
                                    Preferences.debug("Horizontal resolution = " +
                                                      horizontalResolution + " pixels per inch\n");
                                    // The vertical resolution of the pixel image in pixels per inch.
                                    // This value is of type Fixed; by default, the value here is
                                    // $00480000(for 72 pixels per inch).
                                    verticalResolutionShort = readShort(endianess);
                                    verticalResolutionFract = getUnsignedShort(endianess);
                                    verticalResolution = (float)(verticalResolutionShort + 
                                                                Math.pow(2.0,-16.0)*verticalResolutionFract);
                                    Preferences.debug("Vertical resolution = " +
                                                      verticalResolution + " pixels per inch\n");
                                    // The storage format for a pixel image.  Indexed pixels are indicated
                                    // by a value of 0.  Direct pixels sare specified by a value of RGBDirect,
                                    // or 16.
                                    pixelType = readShort(endianess);
                                    if (pixelType == 0) {
                                        Preferences.debug("Pixel image has indexed pixels\n");
                                    }
                                    else if (pixelType == 16) {
                                        Preferences.debug("Pixel image has direct pixels\n");
                                    }
                                    else {
                                        Preferences.debug("pixelType has unrecognized value = " + pixelType + "\n");
                                    }
                                    // Pixel depth; that is, the number of bits used to represent a pixel.
                                    // Indexed pixels can have sizes of 1, 2, 4, and 8 bits;  direct pixel
                                    // sizes are 16 and 32 bits.
                                    pixelSize = readShort(endianess);
                                    Preferences.debug("pixelSize = " + pixelSize + "\n");
                                    // cmpCount - logical components per pixel
                                    // The number of components used to represent a color for a pixel.
                                    // With indexed pixels, each pixel is a single value representing an
                                    // index in a color table, and therefore this field contains the value
                                    // 1 -- the index is the single component.  With direct pixels, each
                                    // pixel contains 3 components--one short each for the intnesities
                                    // of red, green, and blue--so this field contains the value 3.
                                    cmpCount = readShort(endianess);
                                    Preferences.debug("Components per pixel = " + cmpCount + "\n");
                                    if (cmpCount == 4) {
                                        Preferences.debug("Alpha channel bytes are placed before red bytes\n");
                                    }
                                    // cmpSize - logical bits per component
                                    // The size in bits of each component for a pixel.  Color QuickDraw
                                    // expects that the sizes of all components are the same, and that the
                                    // value of the cmpCount field multiplied by the cmpSize field is less 
                                    // than or equal to the value in the pixelSize field.
                                    // For an indexed value, which has only 1 component, the value of the
                                    // cmpSize is the same as the value of the pixelSize field--that is,
                                    // 1, 2, 4, or 8.
                                    // For direct pixels there are 2 additional possibilities:
                                    // A 16-bit pixel, which has 3 components, has a cmpSize value of 5.
                                    // This leaves an unused high-order bit, which Color QuickDraw sets
                                    // to 0.  A 32-bit pixel, which has 3 components(red, green, and blue),
                                    // has a cmpSize vlaue of 8.  This leaves an unused high-order byte,
                                    // which Color QuickDraw sets to 0.
                                    cmpSize = readShort(endianess);
                                    Preferences.debug("Bits per component = " + cmpSize + "\n");
                                    // planeBytes - the offset in bytes form one drawing plane to the
                                    // next.  This field is set to 0.
                                    planeBytes = getUInt(endianess);
                                    if (planeBytes == 0) {
                                        Preferences.debug("The planeBytes field is set to 0 as expected\n");
                                    }
                                    else {
                                        Preferences.debug("planeBytes unexpectedly = " + planeBytes + "\n");
                                    }
                                    // pmTable - a pointer to a ColorTable record for the colors in this\
                                    // pixel map.
                                    pmTable = getUInt(endianess);
                                    Preferences.debug("Location of ColorTable record = " + pmTable + "\n");
                                    // pmReserved - reserved for future expansion.  This field must be
                                    // set to 0 for future compatibility.
                                    pmReserved = readInt(endianess);
                                    if (pmReserved == 0) {
                                        Preferences.debug("pmReserved = 0 as expected\n");
                                    }
                                    else {
                                        Preferences.debug("pmReserved unexpectedly = " + pmReserved + "\n");
                                    }
                                    // ColorTable
                                    // Identifies a particular instance of the color table.
                                    ctSeed = readInt(endianess);
                                    Preferences.debug("ctSeed = " + ctSeed + "\n");
                                    // Flags that distinguish pixel map color tables from color tables
                                    // in GDevice records
                                    ctFlags = readShort(endianess);
                                    Preferences.debug("ctFlags = " + ctFlags + "\n");
                                    // One less than the number of entries in the table
                                    LUTSizeLocation[subPictCount] = raFile.getFilePointer();
                                    ctSize = readShort(endianess);
                                    Preferences.debug("ctSize = " + ctSize + "\n");
                                    // An array of ColorSpec records
                                    for (j = 0; j <= ctSize; j++) {
                                        index = getUnsignedShort(endianess);
                                        redColor = getUnsignedShort(endianess);
                                        greenColor = getUnsignedShort(endianess);
                                        blueColor = getUnsignedShort(endianess);
                                        Preferences.debug("j = " + j + " index = " + index + " red = "
                                         + redColor + " green = " + greenColor + " blue = " +
                                         blueColor + "\n");
                                    }
                                    // Source rectangle
                                    yTopLeft = readShort(endianess);
                                    Preferences.debug("Source rectangle y top left = " + yTopLeft + "\n");
                                    xTopLeft = readShort(endianess);
                                    Preferences.debug("Source rectangle x top left = " + xTopLeft + "\n");
                                    yBottomRight = readShort(endianess);
                                    Preferences.debug("Source rectangle y bottom right = " + yBottomRight + "\n");
                                    xBottomRight = readShort(endianess);
                                    Preferences.debug("Source rectangle x bottom right = " + xBottomRight + "\n");
                                    // Destination rectangle
                                    yTopLeft = readShort(endianess);
                                    Preferences.debug("Destination rectangle y top left = " + yTopLeft + "\n");
                                    xTopLeft = readShort(endianess);
                                    Preferences.debug("Destination rectangle x top left = " + xTopLeft + "\n");
                                    yBottomRight = readShort(endianess);
                                    Preferences.debug("Destination rectangle y bottom right = " + yBottomRight + "\n");
                                    xBottomRight = readShort(endianess);
                                    Preferences.debug("Destination rectangle x bottom right = " + xBottomRight + "\n");
                                    // mode
                                    mode = readShort(endianess);
                                    if ((mode >= 0) && (mode <= 64) && (modeStr[mode] != null)) {
                                        Preferences.debug("mode = " + modeStr[mode] + "\n");
                                    }
                                    else {
                                        Preferences.debug("mode has unrecognized value = " + mode + "\n");    
                                    }
                                    // PixData
                                    Preferences.debug("PixData\n");
                                    // Also read in pixData later if necessary
                                    pictLocation[subPictCount] = raFile.getFilePointer();
                                    packTypeArray[subPictCount] = packType;
                                    rowBytesArray[subPictCount] = rowBytes;
                                    rowBytesTypeArray[imageType] = rowBytes;
                                    pixelSizeArray[subPictCount] = pixelSize;
                                    boundsTopArray[subPictCount] = boundsTop;
                                    boundsBottomArray[subPictCount] = boundsBottom;
                                    cmpCountArray[subPictCount] = cmpCount;
                                    imageTypeLocation[subPictCount++] = imageType;
                                    if ((packType == 0) || (packType > 2)) {
                                        totalByteCount = 0;
                                        for (j = boundsTop; j < boundsBottom; j++) {
                                            if (rowBytes > 250) {
                                                byteCount = getUnsignedShort(endianess);
                                                totalByteCount += (byteCount + 2);
                                            }
                                            else {
                                                raFile.read(prefix);
                                                byteCount = prefix[0] & 0xff;
                                                totalByteCount += (byteCount + 1);
                                            }
                                            Preferences.debug("row number = " + j + " byte count = " + byteCount + "\n");
                                            pad = new byte[byteCount];
                                            raFile.read(pad);
                                        }
                                        
                                        if ((totalByteCount % 2) == 1) {
                                            raFile.read(spareByte);
                                        }
                                    } // if ((packType == 0) || (packType > 2))
                                    break;
                                case DirectBitsRect:
                                    Preferences.debug("DirectBitsRect\n");
                                    // PixMap, srcRect, dstRect, mode(short), PixData
                                    // The unsigned 32 bit base address is set to $000000FF because
                                    // of the direct pixMap used here.  This is done because machines
                                    // without support for direct pixMaps read a word from the picture,
                                    // skip that many bytes, and continue picture parsing.  When such a
                                    // machine encounters the $000000FF baseAddr, the number of bytes
                                    // skipped is $0000 and the next opcode is $00FF, which ends the
                                    // picture playback.  A graceful exit from a tough situation
                                    baseAddr = getUInt(endianess);
                                    if (baseAddr == 0x000000FF) {
                                        Preferences.debug("baseAddr is 0x000000FF as expected\n");
                                    }
                                    else {
                                        Preferences.debug("baseAddr is unexpectedly " + baseAddr + "\n");
                                    }
                                    // rowBytes, The offset in bytes from one row of the image to the next.
                                    // The value must be even, less than $4000, and for best performance it
                                    // should be a multiple of 4.  The high 2 bits of rowBytes are used as
                                    // flags.  If bit 15 = 1, the data structure pointed to is a PixMap 
                                    // record; otherwise it is a bitMap record.
                                    rowBytes = readShort(endianess);
                                    if ((rowBytes & 0x8000) != 0) {
                                        Preferences.debug("The data structure pointed to is a PixMap record\n");
                                    }
                                    else {
                                        Preferences.debug("The data structure pointed to is a BitMap record\n");
                                    }
                                    // Strip out the 2 flag bits
                                    rowBytes = (short)(rowBytes & 0x3fff);
                                    Preferences.debug("Offset in bytes from one row of the image to the next = "
                                                       + rowBytes + "\n");
                                    // The boundary rectangle, which links the local coordinate system of a
                                    // graphics port to QuickDraw's global coordinate system and defines the
                                    // area of the bit image into which QuickDraw can draw.  By default,
                                    // the boundary rectangle is the entire main screen.
                                    boundsTop = readShort(endianess);
                                    Preferences.debug("Boundary rectangle y top left = " + boundsTop + "\n");
                                    boundsLeft = readShort(endianess);
                                    Preferences.debug("Boundary rectangle x top left = " + boundsLeft + "\n");
                                    boundsBottom = readShort(endianess);
                                    Preferences.debug("Boundary rectangle y bottom right = " + boundsBottom + "\n");
                                    boundsRight = readShort(endianess);
                                    Preferences.debug("Boundary rectangle x bottom right = " + boundsRight + "\n");
                                    // PixMap record version number.  The version number of Color QuickDraw
                                    // that created this pixMap record.  The value of pmVersion is normally 0.
                                    // If pmVersion is 4, Color QuickDraw treats the pixMap reocrd's baseAddr
                                    // field as 32-bit clean.  (All other flags are private.)
                                    pmVersion = readShort(endianess);
                                    Preferences.debug("The PixMap record version number = " + pmVersion + "\n");
                                    // The packing algorithm used to compress image data.
                                    // To facilitate banding fo images when memory is short, all data
                                    // compression is done on a scan-line basis.  The following pseudocode
                                    // describes the pixel data:
                                    // PixData
                                    // if packType = 1 (unpacked) or rowBytes < 8 then
                                    //     data is unpacked;
                                    //     data size = rowBytes * (bounds.bottom - bounds.top);
                                    
                                    // if packType = 2 (drop pad byte) then
                                    //     the high-order pad byte of a 32-bit direct pixel is dropped;
                                    //     data size = (3/4) * rowBytes * (bounds.bottom - bounds.top);
                                    
                                    // if packType > 2 (packed) then
                                    //    image contains (bounds.bottom - bounds.top) packed scan lines;
                                    //    each scan line consists of [byteCount] [data];
                                    //    if rowBytes > 250 then
                                    //        byteCount is a short
                                    //    else
                                    //        byteCount is a byte
                                    // Here are the currently defined packing types:
                                    // Packing type    Meaning
                                    // 0               Use default packing
                                    //                 The default for a pixelSize value of 16 is 3
                                    //                 The default for a pixelSize value of 32 is 4
                                    // 1               Use no packing
                                    // 2               Remove pad byte -- supported only for 32-bit pixels
                                    //                 (24-bit data)
                                    // 3               Run length encoding by pixelSize chunks, one scan line
                                    //                 at a time -- supported only for 16-bit pixels.
                                    // 4               Run length encoding one component at a time, one scan
                                    //                 line at a time, red component first--supported only for
                                    //                 32-bit pixels (24-bit data)
                                    // Each scan line of packed data is preceded by a byte or a short
                                    // giving the count of data
                                    // When the pixel type is direct, cmpCount * cmpSize is less than or equal
                                    // to pixelSize.  For storing 24-bit data in a 32-bit pixel, set cmpSize
                                    // to 8 and cmpCount to 3.  If you set cmpCount to 4, then the high byte
                                    // is compressed by packing scheme 4 and stored in the picture.
                                    packType = readShort(endianess);
                                    Preferences.debug("The pack type used to compress image data = " +
                                                       packType + "\n");
                                    // The size of the packed image in bytes.  Since each scan line of 
                                    // packed data is preceded by a byte count, packSize is not used and
                                    // must be 0 for future compatibility.
                                    packSize = getUInt(endianess);
                                    if (packSize == 0) {
                                        Preferences.debug("The field for the size of the packed image in bytes " 
                                                    +  "is set to 0 as expected\n");
                                    }
                                    else {
                                        Preferences.debug("The size of the packed image in bytes = " 
                                             + packSize + "\n");         
                                    }
                                    // The horizontal resolution of the pixel image in pixels per inch.
                                    // This value is of type Fixed; by default, the value here is
                                    // $00480000(for 72 pixels per inch).
                                    horizontalResolutionShort = readShort(endianess);
                                    horizontalResolutionFract = getUnsignedShort(endianess);
                                    horizontalResolution = (float)(horizontalResolutionShort + 
                                                                Math.pow(2.0,-16.0)*horizontalResolutionFract);
                                    Preferences.debug("Horizontal resolution = " +
                                                      horizontalResolution + " pixels per inch\n");
                                    // The vertical resolution of the pixel image in pixels per inch.
                                    // This value is of type Fixed; by default, the value here is
                                    // $00480000(for 72 pixels per inch).
                                    verticalResolutionShort = readShort(endianess);
                                    verticalResolutionFract = getUnsignedShort(endianess);
                                    verticalResolution = (float)(verticalResolutionShort + 
                                                                Math.pow(2.0,-16.0)*verticalResolutionFract);
                                    Preferences.debug("Vertical resolution = " +
                                                      verticalResolution + " pixels per inch\n");
                                    // The storage format for a pixel image.  Indexed pixels are indicated
                                    // by a value of 0.  Direct pixels sare specified by a value of RGBDirect,
                                    // or 16.
                                    pixelType = readShort(endianess);
                                    if (pixelType == 0) {
                                        Preferences.debug("Pixel image has indexed pixels\n");
                                    }
                                    else if (pixelType == 16) {
                                        Preferences.debug("Pixel image has direct pixels\n");
                                    }
                                    else {
                                        Preferences.debug("pixelType has unrecognized value = " + pixelType + "\n");
                                    }
                                    // Pixel depth; that is, the number of bits used to represent a pixel.
                                    // Indexed pixels can have sizes of 1, 2, 4, and 8 bits;  direct pixel
                                    // sizes are 16 and 32 bits.
                                    pixelSize = readShort(endianess);
                                    Preferences.debug("pixelSize = " + pixelSize + "\n");
                                    // cmpCount - logical components per pixel
                                    // The number of components used to represent a color for a pixel.
                                    // With indexed pixels, each pixel is a single value representing an
                                    // index in a color table, and therefore this field contains the value
                                    // 1 -- the index is the single component.  With direct pixels, each
                                    // pixel contains 3 components--one short each for the intnesities
                                    // of red, green, and blue--so this field contains the value 3.
                                    cmpCount = readShort(endianess);
                                    Preferences.debug("Components per pixel = " + cmpCount + "\n");
                                    if (cmpCount == 4) {
                                        Preferences.debug("Alpha channel bytes are placed before red bytes\n");
                                    }
                                    // cmpSize - logical bits per component
                                    // The size in bits of each component for a pixel.  Color QuickDraw
                                    // expects that the sizes of all components are the same, and that the
                                    // value of the cmpCount field multiplied by the cmpSize field is less 
                                    // than or equal to the value in the pixelSize field.
                                    // For an indexed value, which has only 1 component, the value of the
                                    // cmpSize is the same as the value of the pixelSize field--that is,
                                    // 1, 2, 4, or 8.
                                    // For direct pixels there are 2 additional possibilities:
                                    // A 16-bit pixel, which has 3 components, has a cmpSize value of 5.
                                    // This leaves an unused high-order bit, which Color QuickDraw sets
                                    // to 0.  A 32-bit pixel, which has 3 components(red, green, and blue),
                                    // has a cmpSize vlaue of 8.  This leaves an unused high-order byte,
                                    // which Color QuickDraw sets to 0.
                                    cmpSize = readShort(endianess);
                                    Preferences.debug("Bits per component = " + cmpSize + "\n");
                                    // planeBytes - the offset in bytes form one drawing plane to the
                                    // next.  This field is set to 0.
                                    planeBytes = getUInt(endianess);
                                    if (planeBytes == 0) {
                                        Preferences.debug("The planeBytes field is set to 0 as expected\n");
                                    }
                                    else {
                                        Preferences.debug("planeBytes unexpectedly = " + planeBytes + "\n");
                                    }
                                    // pmTable - a pointer to a ColorTable record for the colors in this\
                                    // pixel map.
                                    pmTable = getUInt(endianess);
                                    Preferences.debug("Location of ColorTable record = " + pmTable + "\n");
                                    // pmReserved - reserved for future expansion.  This field must be
                                    // set to 0 for future compatibility.
                                    pmReserved = readInt(endianess);
                                    if (pmReserved == 0) {
                                        Preferences.debug("pmReserved = 0 as expected\n");
                                    }
                                    else {
                                        Preferences.debug("pmReserved unexpectedly = " + pmReserved + "\n");
                                    }
                                    // Source rectangle
                                    yTopLeft = readShort(endianess);
                                    Preferences.debug("Source rectangle y top left = " + yTopLeft + "\n");
                                    xTopLeft = readShort(endianess);
                                    Preferences.debug("Source rectangle x top left = " + xTopLeft + "\n");
                                    yBottomRight = readShort(endianess);
                                    Preferences.debug("Source rectangle y bottom right = " + yBottomRight + "\n");
                                    xBottomRight = readShort(endianess);
                                    Preferences.debug("Source rectangle x bottom right = " + xBottomRight + "\n");
                                    // Destination rectangle
                                    yTopLeft = readShort(endianess);
                                    Preferences.debug("Destination rectangle y top left = " + yTopLeft + "\n");
                                    xTopLeft = readShort(endianess);
                                    Preferences.debug("Destination rectangle x top left = " + xTopLeft + "\n");
                                    yBottomRight = readShort(endianess);
                                    Preferences.debug("Destination rectangle y bottom right = " + yBottomRight + "\n");
                                    xBottomRight = readShort(endianess);
                                    Preferences.debug("Destination rectangle x bottom right = " + xBottomRight + "\n");
                                    // mode
                                    mode = readShort(endianess);
                                    if ((mode >= 0) && (mode <= 64) && (modeStr[mode] != null)) {
                                        Preferences.debug("mode = " + modeStr[mode] + "\n");
                                    }
                                    else {
                                        Preferences.debug("mode has unrecognized value = " + mode + "\n");    
                                    }
                                    // PixData
                                    Preferences.debug("PixData\n");
                                    // Also read in pixData later if necessary
                                    pictLocation[subPictCount] = raFile.getFilePointer();
                                    packTypeArray[subPictCount] = packType;
                                    rowBytesArray[subPictCount] = rowBytes;
                                    rowBytesTypeArray[imageType] = rowBytes;
                                    pixelSizeArray[subPictCount] = pixelSize;
                                    boundsTopArray[subPictCount] = boundsTop;
                                    boundsBottomArray[subPictCount] = boundsBottom;
                                    cmpCountArray[subPictCount] = cmpCount;
                                    imageTypeLocation[subPictCount++] = imageType;
                                    if ((packType == 0) || (packType > 2)) {
                                        totalByteCount = 0;
                                        for (j = boundsTop; j < boundsBottom; j++) {
                                            if (rowBytes > 250) {
                                                byteCount = getUnsignedShort(endianess);
                                                totalByteCount += (byteCount + 2);
                                            }
                                            else {
                                                raFile.read(prefix);
                                                byteCount = prefix[0] & 0xff;
                                                totalByteCount += (byteCount + 1);
                                            }
                                            Preferences.debug("row number = " + j + " byte count = " + byteCount + "\n");
                                            pad = new byte[byteCount];
                                            raFile.read(pad);
                                        }
                                        
                                        if ((totalByteCount % 2) == 1) {
                                            raFile.read(spareByte);
                                        }
                                    } // if ((packType == 0) || (packType > 2))
                                    break;
                                case LongComment:
                                    Preferences.debug("LongComment for imageType = " + typeStr[imageType] + "\n");
                                    commentKind = readUnsignedShort(endianess);
                                    Preferences.debug("Comment kind = " + commentKind + "\n");
                                    commentSize = readUnsignedShort(endianess);
                                    Preferences.debug("Comment size = " + commentSize + "\n");
                                    commentPointer = raFile.getFilePointer();
                                    if (commentKind == 101) {
                                        // Read in first part of comment 101
                                        appSignature = getString(4);
                                        if (appSignature.equals("IVEA")) {
                                            Preferences.debug("PIC Comment 101 appSignature has expected " +
                                                              appSignature + "\n");
                                        }
                                        else {
                                            Preferences.debug("PIC Comment 101 appSignature is unexpectedly " +
                                                              appSignature + "\n");
                                        }
                                        kindSignature = getString(4);
                                        if (kindSignature.equals("dbpq")) {
                                            Preferences.debug("kindSignature dbpq indicates that image data follows\n");
                                            // Read in the second part of comment 101 later
                                            pictLocation[subPictCount] = raFile.getFilePointer();
                                            imageTypeLocation[subPictCount++] = imageType;
                                        }
                                        else if (kindSignature.equals("dbpl")) {
                                            // Read in the second part of comment 101 now
                                            Preferences.debug("kindSignature dbpl indicates that LUT data follows\n");
                                            blkCount = readInt(endianess);
                                            Preferences.debug("blkCount = " + blkCount + "\n");
                                            // totalBlocks is the number of blocks that make up this picture or LUT
                                            totalBlocks = readInt(endianess);
                                            Preferences.debug("totalBlocks = " + totalBlocks + "\n");
                                            // originalSize is the count of bytes in the original image or LUT
                                            originalSize = readInt(endianess);
                                            Preferences.debug("originalSize = " + originalSize + "\n");
                                            // compressedSize is always the same as originalSize
                                            compressedSize = readInt(endianess);
                                            Preferences.debug("compressedSize = " + compressedSize + "\n");
                                            // picBlkSize is the count of bytes of data in the remainder of the
                                            // comment.  This size does not include the header.  This is the count
                                            // of bytes after bitShift.
                                            picBlkSize = readInt(endianess);
                                            Preferences.debug("Bytes in Pic Comment 101 not including header = "
                                                              + picBlkSize + "\n");
                                            // bitDepth is the logical bitdepth of the image, and may be any
                                            // value from 9 to 16.
                                            bitDepth = readShort(endianess);
                                            Preferences.debug("bitDepth = " + bitDepth + "\n");
                                            // bitShift should be ignored
                                            bitShift = readShort(endianess);
                                        }
                                        else {
                                            Preferences.debug("kindSignature has unrecognized value = "
                                                    + kindSignature + "\n");
                                        }
                                        
                                    } // if (commentKind == 101)
                                    raFile.seek(commentPointer + commentSize);
                                    if (((4 + commentSize) % 2) == 1) {
                                        raFile.read(spareByte);
                                    }
                                    break;
                                case OpEndPic:
                                    found = true;
                                    Preferences.debug("OpEndPic: End of picture\n");
                                    break;
                                case HeaderOp:
                                    Preferences.debug("HeaderOp\n");
                                    headerVersion = readShort(endianess);
                                    Preferences.debug("Version = " + headerVersion + "\n");
                                    headerReservedShort = readShort(endianess);
                                    // The horizontal resolution of the pixel image in pixels per inch.
                                    // This value is of type Fixed; by default, the value here is
                                    // $00480000(for 72 pixels per inch).
                                    hResShort = readShort(endianess);
                                    hResFract = getUnsignedShort(endianess);
                                    hRes = (float)(hResShort + Math.pow(2.0,-16.0)*hResFract);
                                    Preferences.debug("Horizontal resolution = " +
                                                      hRes + " pixels per inch\n");
                                    // The vertical resolution of the pixel image in pixels per inch.
                                    // This value is of type Fixed; by default, the value here is
                                    // $00480000(for 72 pixels per inch).
                                    vResShort = readShort(endianess);
                                    vResFract = getUnsignedShort(endianess);
                                    vRes = (float)(vResShort + Math.pow(2.0,-16.0)*vResFract);
                                    Preferences.debug("Vertical resolution = " +
                                                      vRes + " pixels per inch\n");
                                    // Source rectangle
                                    yTopLeft = readShort(endianess);
                                    Preferences.debug("Source rectangle y top left = " + yTopLeft + "\n");
                                    xTopLeft = readShort(endianess);
                                    Preferences.debug("Source rectangle x top left = " + xTopLeft + "\n");
                                    yBottomRight = readShort(endianess);
                                    Preferences.debug("Source rectangle y bottom right = " + yBottomRight + "\n");
                                    xBottomRight = readShort(endianess);
                                    Preferences.debug("Source rectangle x bottom right = " + xBottomRight + "\n");
                                    headerReservedInt = readInt(endianess);
                                    break;
                                default:
                                    Preferences.debug("opcode = " + opcode + "\n");
                            } // switch (opcode)
                        } // while (!found)
                    } // if (versionNumber <= 2)
                    else {
                        // Image width
                        width = readInt(endianess);
                        Preferences.debug("width = " + width + "\n");
                        xDimArray[imageType] = width;
                        // Image height
                        height = readInt(endianess);
                        Preferences.debug("height = " + height + "\n");
                        yDimArray[imageType] = height;
                        pictLocation[subPictCount] = raFile.getFilePointer();
                        pixelSizeArray[subPictCount] = layerDepth;
                        imageTypeLocation[subPictCount++] = imageType;
                        // Read in rest of data later if necessary
                    }
                    
                    if ((imageType >= DEEP_GREY_9)  && (imageType <= DEEP_GREY_16)) {
                        bitNumber = layerDepth;
                        pictCount++;
                    } // // if ((imageType >= DEEP_GREY_9)  && (imageType <= DEEP_GREY_16))
                } // if ((tagType == 67) || (tagType == 68))
                else if (tagType == 69) {
                    // calibration
                    platform = getString(4);
                    if (platform.equals("pwpc")) {
                        Preferences.debug("Platform is PowerPC\n");
                    }
                    else if (platform.equals("m68k")) {
                        Preferences.debug("Platform is Motorola MC680x0 processor\n");
                    }
                    else if (platform.equals("unkn")) {
                        Preferences.debug("Platform is unknown\n");
                    }
                    else {
                        Preferences.debug("Platform has unrecognized value of " + platform + "\n");
                    }
                    units = readShort(endianess);
                    if ((units == 1) || ((units >= 3) && (units <= 15))) {
                        Preferences.debug("Units are " + unitStr[units] + "\n");
                    }
                    else {
                        Preferences.debug("Unit has unrecognized value of " + units + "\n");
                    }
                    // calLayerID is obsolete and shoud be set to zero.  In Openlab, calibration is
                    // not done on a per-layer basis, but on a per document basis with multiple layers.
                    calLayerID = readShort(endianess);
                    // square is true if x and y calibrations are identical, false if they are different.
                    // If true, you should use the values for the x dimension to determine the
                    // calibration.  y may not be defined
                    raFile.read(square);
                    if (square[0] == 1) {
                        Preferences.debug("x and y calibrations are identical\n");
                    }
                    else {
                        Preferences.debug("x and y calibrations are different\n");
                    }
                    // There is a spare byte between square and xOrigin
                    raFile.read(spareByte);
                    // The storage format for the 4 floats is platform dependent.
                    // On the PowerPC the IEEE-754 standard is followed for 32 bit float
                    // and 64 bit double
                    // xOrigin is the absolute position of the left of the calibrated image.
                    // It will almost always be set to 0.0.
                    origin[0] = readFloat(endianess);
                    Preferences.debug("xOrigin = " + origin[0] + "\n");
                    // yOrigin is the absolute position of the top of the calibrated image.
                    // It will almost always be set to 0.0.
                    origin[1] = readFloat(endianess);
                    Preferences.debug("yOrigin = " + origin[1] + "\n");
                    // xScale is the horizontal dimension of a single pixel in the calibrated units.
                    // For example, if this is set to 0.01, and the units are in microns, the length
                    // of a 100 pixel line in the image is 1 micron
                    imgResols[0] = readFloat(endianess);
                    if (((units >= 3) && (units <= 8)) || ((units >= 10) && (units <= 13))) {
                        Preferences.debug("The width of a pixel is " + imgResols[0] + " " + unitStr[units] + "\n");
                    }
                    else {
                        Preferences.debug("The width of a pixel is " + imgResols[0] + "\n");
                    }
                    // yScale is the vertical dimension of a single pixel in the calibrated units
                    imgResols[1] = readFloat(endianess);
                    if (((units >= 3) && (units <= 8)) || ((units >= 10) && (units <= 13))) {
                        Preferences.debug("The height of a pixel is " + imgResols[1] + " " + unitStr[units] + "\n");
                    }
                    else {
                        Preferences.debug("The height of a pixel is " + imgResols[1] + "\n");
                    }
                    // positiveY is a flag indicating the convention of the direction for the Y axis.
                    // In mathematics, the Y axis normally is indicated increasing in value the
                    // further toward the top of the graph you go.  Computer graphics usually adopt
                    // the opposite convention for simplicity where y increases toward the bottom of
                    // the graph.  Openlab can use either, as indicated by this flag.  If true, the 
                    // mathematical convention is used.  If false, the computer convention is used.
                    raFile.read(positiveY);
                    if (positiveY[0] == 1) {
                        Preferences.debug("y increases in value toward the top of the image\n");
                    }
                    else {
                        Preferences.debug("y increases in value toward the bottom of the image\n");
                    }
                    if (units == kUnitsOther) {
                        raFile.read(prefix);
                        otherUnitStrLength = prefix[0] & 0xff;
                        Preferences.debug("Other unit string length = " + otherUnitStrLength + "\n");
                        otherUnitString = getString(otherUnitStrLength);
                        Preferences.debug("Other unit name = " + otherUnitString.trim() + "\n");
                    }
                } // else if (tagType == 69)
                else if (tagType == 72) {
                    Preferences.debug("User tag number = " + (userTagNum+1) + "\n");
                    userTagNum++;
                    className = readCString();
                    Preferences.debug("className = " + className.trim() + "\n");
                    if (className.trim().equals("CVariableList")) {
                        numVars = readShort(endianess);
                        Preferences.debug("numVars = " + numVars + "\n");
                        for (j = 0; j < numVars; j++) {
                            className = readCString();
                            Preferences.debug("j = " + j + " className = " + className.trim() + "\n");
                            if ((!className.trim().equals("CStringVariable")) &&
                                (!className.trim().equals("CFloatVariable"))) {
                                break;
                            }
                            raFile.read(derivedClassVersion);
                            if (derivedClassVersion[0] == 1) {
                                Preferences.debug("derivedClassVersion[0] = 1 as expected\n");
                            }
                            else {
                                Preferences.debug("Invalid derivedClassVersion[0] = " + 
                                                  derivedClassVersion[0] + "\n");
                            }
                            if (className.trim().equals("CStringVariable")) {
                                strSize = readInt(endianess);
                                Preferences.debug("strSize = " + strSize + "\n");
                                stringValue = getString(strSize);
                                Preferences.debug("stringValue = " + stringValue + "\n");
                                raFile.skipBytes(1);
                            }
                            else if (className.trim().equals("CFloatVariable")) {
                                doubleValue = readDouble(endianess);
                                Preferences.debug("doubleValue = " + doubleValue + "\n");
                            } // else if (className.trim().equals("CFloatVariable"))
                            
                            raFile.read(baseClassVersion);
                            if ((baseClassVersion[0] == 1) || (baseClassVersion[0] == 2)) {
                                Preferences.debug("baseClassVersion[0] legally = " + baseClassVersion[0] + "\n");    
                            }
                            else {
                                Preferences.debug("invalid baseClassVersion[0] = " + baseClassVersion[0] + "\n");
                                break;
                            }
                            strSize = readInt(endianess);
                            Preferences.debug("strSize = " + strSize + "\n");
                            nameStr = getString(strSize);
                            Preferences.debug("name = " + nameStr.trim() + "\n");
                            if (nameStr.trim().toUpperCase().equals("ZSTEP")) {
                               zStepArray[zStepNumber++] = doubleValue;    
                            }
                            raFile.skipBytes(2*baseClassVersion[0] + 1);
                        } // for (j = 0; j < numVars; j++)
                    } // if (className.equals("CVariableList"))*/
                } // else if (tagType == 72)
            } // for (nextOffset = firstTagOffset, i = 1; nextOffset < fileLength - 1; i++)
            
            fireProgressStateChanged(50);
            
            if (zStepNumber >= 1) {
                imgResols[2] = (float)zStepArray[0];
                sameStep = true;
                for (i = 1; i < zStepNumber & sameStep; i++) {
                  if (zStepArray[0] != zStepArray[1]) {
                      sameStep = false;
                  }
                }
                if (sameStep) {
                    Preferences.debug("All " + zStepNumber + " instances of ZStep are " + zStepArray[0] + "\n");
                }
                else {
                   Preferences.debug("Warning, not all " + zStepNumber + " instances of ZStep are the same\n");
                }
            } // if (ZStepNumber >= 1)
            
            for (i = 0; i <= 16; i++) {
                if (imageTypeCount[i] == 1) {
                    Preferences.debug("The image has 1 slice of type " + typeStr[i] + "\n");
                }
                else if (imageTypeCount[i] > 1) {
                    Preferences.debug("The image has " + imageTypeCount[i] +
                                      " slices of type " + typeStr[i] + "\n");
                }
                if (imageTypeCount[i] > majorTypeCount) {
                    majorType = i;
                    majorTypeCount = imageTypeCount[i];
                    xDim = xDimArray[majorType];
                    yDim = yDimArray[majorType];
                    rowBytes = (short)rowBytesTypeArray[majorType];
                }
            } // for (i = 0; i <= 16; i++)
            
            for (i = 0; i < layerNumber; i++) {
                if (layerTypeArray[i] == majorType) {
                    majorLayerString[majorLayerNumber++] = layerString[i]; 
                    if (layerString[i].equals("CY3")) {
                        haveCY3 = true;
                    }
                    else if (layerString[i].equals("FITC")) {
                        haveFITC = true;
                    }
                    else if (layerString[i].equals("DAPI")) {
                        haveDAPI = true;
                    }
                }
            }
            
            
            Preferences.debug("The number of layers for the major image type is " + majorLayerNumber + "\n");
            Preferences.debug("The major image type layers are: \n");
            for (i = 0; i < majorLayerNumber; i++) {
                Preferences.debug(majorLayerString[i] + "\n");    
            }
            fileInfo.setLayerString(majorLayerString);
            if (((majorType >= DEEP_GREY_9) && (majorType <= DEEP_GREY_16)) && ((majorTypeCount % 3) == 0) 
                 && (majorLayerNumber == 3)) {
                doDeepGreyColor = true;
                Preferences.debug("Found " + majorTypeCount + " slices of type " + typeStr[majorType] + "\n");
                Preferences.debug("These are RGB stored in " + bitNumber + " depth\n");
                imageSlices = majorTypeCount/3;
                if (imageSlices > 1) {
                    Preferences.debug("The MIPAV image will have " + imageSlices + " slices of type ARGB_USHORT\n");
                }
                else {
                    Preferences.debug("The MIPAV image will have 1 slice of type ARGB_USHORT\n");    
                }
                if (imageSlices > 1) {
                    imageExtents = new int[3];
                    imageExtents[0] = xDim;
                    imageExtents[1] = yDim;
                    imageExtents[2] = imageSlices;
                }
                else {
                    imageExtents = new int[2];
                    imageExtents[0] = xDim;
                    imageExtents[1] = yDim;
                }
                sliceSize = xDim * yDim;
                fileInfo.setExtents(imageExtents);
                image = new ModelImage(ModelStorageBase.ARGB_USHORT, imageExtents, fileName);
                shortBuffer = new short[4 * xDim * yDim];
                sliceColorBytes = 2 * xDim * yDim;
                if (versionNumber <= 2) {
                    sliceColorBuffer = new byte[sliceColorBytes];
                }
            }
            else if ((majorType >= DEEP_GREY_9) && (majorType <= DEEP_GREY_16)) {
                if (majorTypeCount > 1) {
                    Preferences.debug("Found " + majorTypeCount + " slices of type " + typeStr[majorType] + "\n");
                }
                else {
                    Preferences.debug("Found 1 slice of type " + typeStr[majorType] + "\n");    
                }
                Preferences.debug("These are USHORT stored in " + bitNumber + " depth\n");
                imageSlices = majorTypeCount;
                if (imageSlices > 1) {
                    Preferences.debug("The MIPAV image will have " + imageSlices + " slices of type USHORT\n");
                }
                else {
                    Preferences.debug("The MIPAV image will have 1 slice of type USHORT\n");    
                }
                if (imageSlices > 1) {
                    imageExtents = new int[3];
                    imageExtents[0] = xDim;
                    imageExtents[1] = yDim;
                    imageExtents[2] = imageSlices;
                }
                else {
                    imageExtents = new int[2];
                    imageExtents[0] = xDim;
                    imageExtents[1] = yDim;
                }
                sliceSize = xDim * yDim;
                fileInfo.setExtents(imageExtents);
                image = new ModelImage(ModelStorageBase.USHORT, imageExtents, fileName);
                byteBuffer = new byte[2 * xDim * yDim];
                shortBuffer = new short[xDim * yDim]; 
                sliceBytes = 2 * xDim * yDim;
            }
            else if ((majorType == MAC_24_BIT_COLOR) || (majorType == MAC_16_BIT_COLOR)) {
                if (majorTypeCount > 1) {
                    Preferences.debug("Found " + majorTypeCount + " slices of type " +
                            typeStr[majorType] + "\n");
                }
                else {
                    Preferences.debug("Found 1 slice of type " +
                            typeStr[majorType] + "\n");    
                }
                imageSlices = majorTypeCount;
                if (imageSlices > 1) {
                    Preferences.debug("The MIPAV image will have " + imageSlices + " slices of type ARGB\n");
                }
                else {
                    Preferences.debug("The MIPAV image will have 1 slice of type ARGB\n");    
                }
                if (imageSlices > 1) {
                    imageExtents = new int[3];
                    imageExtents[0] = xDim;
                    imageExtents[1] = yDim;
                    imageExtents[2] = imageSlices;
                }
                else {
                    imageExtents = new int[2];
                    imageExtents[0] = xDim;
                    imageExtents[1] = yDim;
                }
                sliceSize = xDim * yDim;
                fileInfo.setExtents(imageExtents);
                image = new ModelImage(ModelStorageBase.ARGB, imageExtents, fileName);
                if (majorType == MAC_16_BIT_COLOR) {
                    if (versionNumber <= 2) {
                        byteBuffer = new byte[2 * xDim * yDim];
                    }
                    byteBuffer2 = new byte[4 * xDim * yDim];
                }
                else {
                    byteBuffer = new byte[4 * xDim * yDim];
                }
            }
            else if ((majorType == MAC_256_GREYS) || (majorType == MAC_256_COLORS) ||
                     (majorType == MAC_16_GREYS) || (majorType == MAC_16_COLORS) || 
                     (majorType == MAC_4_GREYS)) {
                if (majorTypeCount > 1) {
                    Preferences.debug("Found " + majorTypeCount + " slices of type " + typeStr[majorType] + "\n");
                }
                else {
                    Preferences.debug("Found 1 slice of type " + typeStr[majorType] + "\n");    
                }
                imageSlices = majorTypeCount;
                if (imageSlices > 1) {
                    Preferences.debug("The MIPAV image will have " + imageSlices + " slices of type UBYTE\n");
                }
                else {
                    Preferences.debug("The MIPAV image will have 1 slice of type UBYTE\n");    
                }
                if (imageSlices > 1) {
                    imageExtents = new int[3];
                    imageExtents[0] = xDim;
                    imageExtents[1] = yDim;
                    imageExtents[2] = imageSlices;
                }
                else {
                    imageExtents = new int[2];
                    imageExtents[0] = xDim;
                    imageExtents[1] = yDim;
                }
                sliceSize = xDim * yDim;
                fileInfo.setExtents(imageExtents);
                image = new ModelImage(ModelStorageBase.UBYTE, imageExtents, fileName);
                if ((majorType == MAC_16_GREYS) || (majorType == MAC_16_COLORS)) {
                    if (versionNumber <= 2) {
                        bufferSize = rowBytes * yDim;
                        byteBuffer = new byte[bufferSize];
                    }
                    byteBuffer2 = new byte[sliceSize];
                }
                else if (majorType == MAC_4_GREYS) {
                    if (versionNumber <= 2) {
                        bufferSize = rowBytes * yDim;
                        byteBuffer = new byte[bufferSize];
                    }
                    byteBuffer2 = new byte[sliceSize];
                }
                else {
                    byteBuffer = new byte[sliceSize];  
                }
            } // else if ((majorType == MAC_256_GREYS) || (majorType == MAC_256_COLORS) ||
            else if (majorType == MAC_1_BIT) {
                if (majorTypeCount > 1) {
                    Preferences.debug("Found " + majorTypeCount + " slices of type MAC_1_BIT\n");
                }
                else {
                    Preferences.debug("Found 1 slice of type MAC_1_BIT\n");    
                }
                imageSlices = majorTypeCount;
                if (imageSlices > 1) {
                    Preferences.debug("The MIPAV image will have " + imageSlices + " slices of type BOOLEAN\n");
                }
                else {
                    Preferences.debug("The MIPAV image will have 1 slice of type BOOLEAN\n");    
                }
                if (imageSlices > 1) {
                    imageExtents = new int[3];
                    imageExtents[0] = xDim;
                    imageExtents[1] = yDim;
                    imageExtents[2] = imageSlices;
                }
                else {
                    imageExtents = new int[2];
                    imageExtents[0] = xDim;
                    imageExtents[1] = yDim;
                }
                sliceSize = xDim * yDim;
                fileInfo.setExtents(imageExtents);
                image = new ModelImage(ModelStorageBase.BOOLEAN, imageExtents, fileName); 
                if (versionNumber <= 2) {
                    bufferSize = rowBytes * yDim;
                    byteBuffer = new byte[bufferSize];
                }
                booleanBuffer = new boolean[sliceSize];
            } // else if (majorType == MAC_1_BIT)
            else {
                Preferences.debug("The MIPAV image will have " + majorTypeCount + 
                                  " slices of type " + typeStr[majorType] + "\n");
                imageSlices = majorTypeCount;
            }
            fileInfo.setDataType(image.getType());
            if (((units >= 3) && (units <= 8)) || (units == 10) || (units == 13)) {
                unitsOfMeasure = new int[3];
                switch (units) {
                    case 3:
                        unitsOfMeasure[0] = FileInfoBase.NANOMETERS;
                        break;
                    case 4:
                        unitsOfMeasure[0] = FileInfoBase.MICROMETERS;
                        break;
                    case 5:
                        unitsOfMeasure[0] = FileInfoBase.MILLIMETERS;
                        break;
                    case 6:
                        unitsOfMeasure[0] = FileInfoBase.CENTIMETERS;
                        break;
                    case 7:
                        unitsOfMeasure[0] = FileInfoBase.METERS;
                        break;
                    case 8:
                        unitsOfMeasure[0] = FileInfoBase.KILOMETERS;
                        break;
                    case 10:
                        unitsOfMeasure[0] = FileInfoBase.INCHES;
                        break;
                    case 13:
                        unitsOfMeasure[0] = FileInfoBase.MILES;
                        break;
                } // switch (units)
                unitsOfMeasure[1] = unitsOfMeasure[0];
                unitsOfMeasure[2] = unitsOfMeasure[0];
                fileInfo.setUnitsOfMeasure(unitsOfMeasure);
            } // if (((units >= 3) && (units <= 8)) || (units == 10) || (units == 13))
            fileInfo.setResolutions(imgResols);
            fileInfo.setOrigin(origin);
            
            if (versionNumber <= 2) {
                if (doDeepGreyColor) {
                    colorSequence[0] = 1;
                    colorSequence[1] = 2;
                    colorSequence[2] = 3;
                    if (haveCY3 && haveFITC && haveDAPI) {
                        // CY3 is red, FITC is green, and DAPI is blue
                        for (i = 0; i < 3; i++) {
                            if (majorLayerString[i].equals("CY3")) {
                                colorSequence[i] = 1;
                            }
                            else if (majorLayerString[i].equals("FITC")) {
                                colorSequence[i] = 2;
                            }
                            else {
                                colorSequence[i] = 3;
                            }
                        }
                    } // if (haveCY3 && haveFITC && haveFITC)
                    index = 0;
                    color = colorSequence[0];
                    sliceNumber = 0;
                    for (i = 0; i < subPictCount; i++) {
                        if (imageTypeLocation[i] == majorType) {
                            raFile.seek(pictLocation[i]);
                            Preferences.debug("Located at subPictCount " + (i+1) + " of " + subPictCount + "\n");
                            // Read in second section of comment 101
                            // blkCount contains the index of this block in the sequence, and will be
                            // from 0 to totalBlocks - 1.  Remember that it will generally require a
                            // series of blocks to build a single image.  Openlab assumes that pic 
                            // comment blocks arrive in the correct sequential order, starting at 0.
                            blkCount = readInt(endianess);
                            Preferences.debug("blkCount = " + blkCount + "\n");
                            // totalBlocks is the number of blocks that make up this picture or LUT
                            totalBlocks = readInt(endianess);
                            Preferences.debug("totalBlocks = " + totalBlocks + "\n");
                            // originalSize is the count of bytes in the original image or LUT
                            originalSize = readInt(endianess);
                            Preferences.debug("originalSize = " + originalSize + "\n");
                            // compressedSize is always the same as originalSize
                            compressedSize = readInt(endianess);
                            Preferences.debug("compressedSize = " + compressedSize + "\n");
                            // picBlkSize is the count of bytes of data in the remainder of the
                            // comment.  This size does not include the header.  This is the count
                            // of bytes after bitShift.
                            picBlkSize = readInt(endianess);
                            Preferences.debug("Bytes in Pic Comment 101 not including header = "
                                              + picBlkSize + "\n");
                            // bitDepth is the logical bitdepth of the image, and may be any
                            // value from 9 to 16.
                            bitDepth = readShort(endianess);
                            Preferences.debug("bitDepth = " + bitDepth + "\n");
                            // bitShift should be ignored
                            bitShift = readShort(endianess);
                            len = Math.min(picBlkSize, sliceColorBytes - index);
                            raFile.read(sliceColorBuffer, index, len);
                            index += len;
                            if (index == sliceColorBytes) {
                                for (j = 0; j < sliceColorBytes/2; j++) {
                                    shortBuffer[4*j + color] = getBufferShort(sliceColorBuffer, 2*j, endianess);
                                } // for (j = 0; j < sliceColorBytes/2; j++)
                                index = 0;
                                if (color == colorSequence[0]) {
                                    color = colorSequence[1];
                                }
                                else if (color == colorSequence[1]) {
                                    color = colorSequence[2];
                                }
                                else if (color == colorSequence[2]) {
                                    color = colorSequence[0];
                                    image.importData(4* sliceNumber * xDim * yDim, shortBuffer, false);
                                    sliceNumber++;
                                }
                            } // if (index == sliceColorBytes)
                         } // if (imageTypeLocation[i] == majorType)
                    } // for (i = 0; i < greySubPictCount; i++)
                } // if (doDeepGreyColor)
                else if ((majorType >= DEEP_GREY_9) && (majorType <= DEEP_GREY_16)) {
                    index = 0;
                    sliceNumber = 0;
                    for (i = 0; i < subPictCount; i++) {
                        if (imageTypeLocation[i] == majorType) {
                            raFile.seek(pictLocation[i]);
                            Preferences.debug("Located at subPictCount " + (i+1) + " of " + subPictCount + "\n");
                            // Read in second section of comment 101
                            // blkCount contains the index of this block in the sequence, and will be
                            // from 0 to totalBlocks - 1.  Remember that it will generally require a
                            // series of blocks to build a single image.  Openlab assumes that pic 
                            // comment blocks arrive in the correct sequential order, starting at 0.
                            blkCount = readInt(endianess);
                            Preferences.debug("blkCount = " + blkCount + "\n");
                            // totalBlocks is the number of blocks that make up this picture or LUT
                            totalBlocks = readInt(endianess);
                            Preferences.debug("totalBlocks = " + totalBlocks + "\n");
                            // originalSize is the count of bytes in the original image or LUT
                            originalSize = readInt(endianess);
                            Preferences.debug("originalSize = " + originalSize + "\n");
                            // compressedSize is always the same as originalSize
                            compressedSize = readInt(endianess);
                            Preferences.debug("compressedSize = " + compressedSize + "\n");
                            // picBlkSize is the count of bytes of data in the remainder of the
                            // comment.  This size does not include the header.  This is the count
                            // of bytes after bitShift.
                            picBlkSize = readInt(endianess);
                            Preferences.debug("Bytes in Pic Comment 101 not including header = "
                                              + picBlkSize + "\n");
                            // bitDepth is the logical bitdepth of the image, and may be any
                            // value from 9 to 16.
                            bitDepth = readShort(endianess);
                            Preferences.debug("bitDepth = " + bitDepth + "\n");
                            // bitShift should be ignored
                            bitShift = readShort(endianess);
                            len = Math.min(picBlkSize, sliceBytes - index);
                            raFile.read(byteBuffer, index, len);
                            index += len;
                            if (index == sliceBytes) {
                                for (j = 0; j < sliceBytes/2; j++) {
                                    shortBuffer[j] = getBufferShort(byteBuffer, 2*j, endianess);
                                } // for (j = 0; j < sliceColorBytes/2; j++)
                                index = 0;
                                image.importData(sliceNumber * xDim * yDim, shortBuffer, false);
                                sliceNumber++;
                            } // if (index == sliceBytes)
                         } // if (imageTypeLocation[i] == majorType)
                    } // for (i = 0; i < greySubPictCount; i++)
                } // else if ((majorType >= DEEP_GREY_9) && (majorType <= DEEP_GREY_16))
                else {
                    for (i = 0; i < subPictCount; i++) {
                        index = 0;
                        component = 0;
                        componentArray = new int[1];
                        if (imageTypeLocation[i] == majorType) {
                            rowBytes = rowBytesArray[i];
                            raFile.seek(pictLocation[i]);
                            Preferences.debug("Located at subPictCount " + (i+1) + " of " + subPictCount + "\n");
                            if ((packTypeArray[i] == 0) || (packTypeArray[i] > 2)) {
                                totalByteCount = 0;
                                if (majorType == MAC_16_BIT_COLOR) {
                                    if (packTypeArray[i] == 0) {
                                        packTypeArray[i] = 3;
                                    }
                                }
                                else if (packTypeArray[i] == 0) {
                                        packTypeArray[i] = 4;
                                }
                                if (cmpCountArray[i] == 3) {
                                    componentArray = new int[3];
                                    componentArray[0] = 1;
                                    componentArray[1] = 2;
                                    componentArray[2] = 3;
                                }
                                else  if (cmpCountArray[i] == 4) {
                                    // To make the sample embryo2 LIFF file MAC_24_BIT_COLOR slices look like the 
                                    // DEEP_GREY_12 slices actually requires an order of 0, 2, 1, 3.
                                    componentArray = new int[4];
                                    componentArray[0] = 0;
                                    componentArray[1] = 1;
                                    componentArray[2] = 2;
                                    componentArray[3] = 3;
                                }
                                for (j = boundsTopArray[i]; j < boundsBottomArray[i]; j++) {
                                    if (rowBytesArray[i] > 250) {
                                        byteCount = getUnsignedShort(endianess);
                                        totalByteCount += (byteCount + 2);
                                    }
                                    else {
                                        raFile.read(prefix);
                                        byteCount = prefix[0] & 0xff;
                                        totalByteCount += (byteCount + 1);
                                    }
                                    pad = new byte[byteCount];
                                    raFile.read(pad);
                                    rowBytesRead = 0;
                                    rowIndex = 0;
                                    if (packTypeArray[i] == 3) {
                                        while ((rowBytesRead < rowBytesArray[i]) && (rowIndex < byteCount)) {
                                            b1 = pad[rowIndex++]; 
                                            if (b1 >= 0) { // 127 >= b1 >= 0
                                              for (k = 0; k < 2*(b1 + 1); k++) {
                                                  byteBuffer[index++] = pad[rowIndex++];
                                              }
                                            } // if (b1 >= 0)
                                            else if (b1 != -128) { // -1 >= b1 >= -127
                                              len = -b1 + 1;
                                              for (k = 0; k < 2*len; k++) {
                                                  byteBuffer[index++] = pad[rowIndex];
                                              }
                                              rowIndex += 2;
                                            } // else if (b1 != -128)
                                         } // while (rowBytesRead < rowBytesArray[i])
                                    } // if (packTypeArray[i] == 3)
                                    else { // packTypeArray[i] == 4
                                        while ((rowBytesRead < rowBytesArray[i]) && (rowIndex < byteCount)) {
                                           b1 = pad[rowIndex++]; 
                                           if (b1 >= 0) { // 127 >= b1 >= 0
                                             for (k = 0; k < (b1 + 1); k++) {
                                                 byteBuffer[(cmpCountArray[i]*index++) + componentArray[component]] =
                                                                                         pad[rowIndex++];
                                                 if ((index % xDim) == 0) {
                                                     if (component < cmpCountArray[i] - 1) {
                                                       component++;
                                                       index = index - xDim;
                                                     }
                                                     else {
                                                       component = 0;
                                                     }
                                                 }
                                             }
                                           } // if (b1 >= 0)
                                           else if (b1 != -128) { // -1 >= b1 >= -127
                                             len = -b1 + 1;
                                             for (k = 0; k < len; k++) {
                                                 byteBuffer[(cmpCountArray[i]*index++) + componentArray[component]] =
                                                     pad[rowIndex];
                                                 if ((index % xDim) == 0) {
                                                     if (component < cmpCountArray[i] - 1) {
                                                       component++;
                                                       index = index - xDim;
                                                     }
                                                     else {
                                                       component = 0;
                                                     }
                                                 }
                                             }
                                             rowIndex++;
                                           } // else if (b1 != -128)
                                        } // while (rowBytesRead < rowBytesArray[i])
                                    } // else packTypeArray[i] == 4
                                } // for (j = boundsTopArray[i]; j < boundsBottomArray[i]; j++)
                                if (majorType == MAC_24_BIT_COLOR) {
                                    image.importData(4 * sliceNumber * sliceSize, byteBuffer, false);
                                }
                                else if (majorType == MAC_16_BIT_COLOR) {    
                                    for (j = 0; j < sliceSize; j++) {
                                        byteBuffer2[4*j] = (byte)255;
                                        shortColor = (short) (((byteBuffer[2*j] & 0xff) << 8) |
                                                               (byteBuffer[2*j + 1] & 0xff));
                                        byteBuffer2[4*j+1] = (byte)((shortColor & 0x7C00) >>> 7);
                                        byteBuffer2[4*j+2] = (byte)((shortColor & 0x03E0) >>> 2);
                                        byteBuffer2[4*j+3] = (byte)((shortColor & 0x001F) << 3);
                                    }
                                    image.importData(4 * sliceNumber * sliceSize, byteBuffer2, false); 
                                } // else if (majorType == MAC_16_BIT_COLOR)
                                else if ((majorType == MAC_16_GREYS) || (majorType == MAC_16_COLORS)) {
                                    for (j = 0, jstart = 0,y = 0; y < yDim; y++) {
                                        if ((j != 0) && (j < jstart + 2*rowBytes)) {
                                            j = jstart + 2*rowBytes;
                                        }
                                        for (x = 0, jstart = j, k = 0; x < xDim; x++) {
                                            index = x + y*xDim;
                                            if (k == 0) {
                                                byteBuffer2[index] = (byte)((byteBuffer[(j++) >> 1] & 0xf0) >>> 4);
                                                k = 1;
                                            }
                                            else if (k == 1) {
                                                byteBuffer2[index] = (byte)(byteBuffer[(j++) >> 1] & 0x0f);  
                                                k = 0;
                                            }
                                        }
                                    }
                                    image.importData(sliceNumber * sliceSize, byteBuffer2, false);
                                } // else if ((majorType == MAC_16_GREYS) || (majorType == MAC_16_COLORS))
                                else if (majorType == MAC_4_GREYS) {
                                    for (j = 0, jstart = 0, y = 0; y < yDim; y++) {
                                        if ((j != 0) && (j < jstart + 4*rowBytes)) {
                                            j = jstart + 4*rowBytes;
                                        }
                                        for (x = 0, jstart = j, k = 0; x < xDim; x++) {
                                            index = x + y*xDim;
                                            if (k == 0) {
                                                byteBuffer2[index] = (byte)((byteBuffer[(j++) >> 2] & 0xc0) >>> 6);
                                                k = 1;
                                            }
                                            else if (k == 1) {
                                                byteBuffer2[index] = (byte)((byteBuffer[(j++) >> 2] & 0x30) >>> 4);
                                                k = 2;
                                            }
                                            else if (k == 2) {
                                                byteBuffer2[index] = (byte)((byteBuffer[(j++) >> 2] & 0x0c) >>> 2);
                                                k = 3;
                                            }
                                            else if (k == 3) {
                                                byteBuffer2[index] = (byte)(byteBuffer[(j++) >> 2] & 0x03);
                                                k = 0;
                                            }
                                        }
                                    }
                                    image.importData(sliceNumber * sliceSize, byteBuffer2, false);
                                } // else if (majorType == MAC_4_GREYS)
                                else if (majorType == MAC_1_BIT) {
                                    for (j = 0, jstart = 0, y = 0; y < yDim; y++) {
                                        if ((j != 0) && (j < jstart + 8*rowBytes)) {
                                            j = jstart + 8*rowBytes;
                                        }
                                        for (x = 0, jstart = j, k = 0x80; x < xDim; x++) {
                                            index = x + y*xDim;
                                            
                                            if ((byteBuffer[(j++) >>> 3] & k) != 0) {
                                                booleanBuffer[index] = true;
                                            }
                                            else {
                                                booleanBuffer[index] = false;
                                            }
                                            if (k == 0x01) {
                                                k = 0x80;
                                            }
                                            else {
                                                k = k >>> 1;
                                            }
                                        }
                                    }
                                    image.importData(sliceNumber * sliceSize, booleanBuffer, false);
                                } // else if (majorType == MAC_1_BIT)
                                else {
                                    image.importData(sliceNumber * sliceSize, byteBuffer, false);
                                }
                                sliceNumber++;
                            } // if ((packType == 0) || (packType > 2))
                            if ((LUTSizeLocation[i] > 0L) && (!haveLUT)) {
                                raFile.seek(LUTSizeLocation[i]); 
                                haveLUT = true;
                                // One less than the number of entries in the table
                                ctSize = readShort(endianess);
                                Preferences.debug("ctSize = " + ctSize + "\n");
                                // read the color table into a LUT
                                dimExtentsLUT = new int[2];
                                dimExtentsLUT[0] = 4;
                                dimExtentsLUT[1] = 256;
                                LUT = new ModelLUT(ModelLUT.GRAY, (ctSize + 1), dimExtentsLUT);
                                maxValue = 0;
                                // An array of ColorSpec records
                                for (j = 0; j <= ctSize; j++) {
                                    index = getUnsignedShort(endianess);
                                    redColor = getUnsignedShort(endianess);
                                    if (redColor > maxValue) {
                                        maxValue = redColor;
                                    }
                                    greenColor = getUnsignedShort(endianess);
                                    if (greenColor > maxValue) {
                                        maxValue = greenColor;
                                    }
                                    blueColor = getUnsignedShort(endianess);
                                    if (blueColor > maxValue) {
                                        maxValue = blueColor;
                                    }
                                } // for (j = 0; j <= ctSize; j++)
                                rightShift = 0;
                                while (maxValue >= 256) {
                                    maxValue = maxValue >> 1;
                                    rightShift++;
                                }
                                raFile.seek(LUTSizeLocation[i] + 2);
                                for (j = 0; j <= ctSize; j++) {
                                    index = getUnsignedShort(endianess);
                                    redColor = (getUnsignedShort(endianess) >>> rightShift);
                                    greenColor = (getUnsignedShort(endianess) >>> rightShift);
                                    blueColor = (getUnsignedShort(endianess) >>> rightShift);
                                    LUT.setColor(j, 1, redColor, greenColor, blueColor);
                                } // for (j = 0; j <= ctSize; j++)
                                for (j = (ctSize+1); j < 256; j++) {
                                    LUT.setColor(j, 1, 0, 0, 0);    
                                } // for (j = (ctSize+1); j < 256; j++)
                                LUT.makeIndexedLUT(null);
                            } // if (LUTSizeLocation[i] > 0L) && (!haveLUT))
                        } // if (imageTypeLocation[i] == majorType)
                    } // for (i = 0; i < subPictCount; i++)
                } // else
            } // if (versionNumber <= 2)
            else { // versionNumber == 5
                if (doDeepGreyColor) {
                    colorSequence[0] = 1;
                    colorSequence[1] = 2;
                    colorSequence[2] = 3;
                    if (haveCY3 && haveFITC && haveDAPI) {
                        // CY3 is red, FITC is green, and DAPI is blue
                        for (i = 0; i < 3; i++) {
                            if (majorLayerString[i].equals("CY3")) {
                                colorSequence[i] = 1;
                            }
                            else if (majorLayerString[i].equals("FITC")) {
                                colorSequence[i] = 2;
                            }
                            else {
                                colorSequence[i] = 3;
                            }
                        }
                    } // if (haveCY3 && haveFITC && haveFITC)
                    index = 0;
                    color = colorSequence[0];
                    sliceNumber = 0;
                    for (i = 0; i < subPictCount; i++) {
                        if (imageTypeLocation[i] == majorType) {
                            raFile.seek(pictLocation[i]);
                            Preferences.debug("Located at subPictCount " + (i+1) + " of " + subPictCount + "\n");
                            uncompressedSize = readInt(endianess);
                            Preferences.debug("Expected uncompressed data size = " + uncompressedSize + "\n");
                            compressedSize = readInt(endianess);
                            Preferences.debug("Compressed data size = " + compressedSize + "\n");
                            dataWidth = uncompressedSize/yDim;
                            Preferences.debug("dataWidth = " + dataWidth + "\n");
                            // The data is compressed using MiniLZO.
                            compressedBuffer = new byte[compressedSize];
                            raFile.read(compressedBuffer);
                            lzo = new LZOCodec();
                            try {
                                uncompressedBuffer = lzo.decompress(compressedBuffer);
                            }
                            catch (Exception e){
                                MipavUtil.displayError("Exception on LZOCodec.decompress(compressedBuffer)");
                                raFile.close();
                                return null;
                            }
                            Preferences.debug("Actual uncompressed data size = " + uncompressedBuffer.length + "\n");
                            for (y = 0; y < yDim; y++) {
                                for (x = 0; x < xDim; x++) {
                                    destIndex = 4*y*xDim + 4*x + color;
                                    srcIndex = 2*y*xDim + 2*x;
                                    shortBuffer[destIndex] = getBufferShort(uncompressedBuffer, srcIndex, endianess);
                                }
                            }
                            index = 0;
                            if (color == colorSequence[0]) {
                                color = colorSequence[1];
                            }
                            else if (color == colorSequence[1]) {
                                color = colorSequence[2];
                            }
                            else if (color == colorSequence[2]) {
                                color = colorSequence[0];
                                image.importData(4* sliceNumber * xDim * yDim, shortBuffer, false);
                                sliceNumber++;
                            }
                        } // if (imageTypeLocation[i] == majorType)
                    } // for (i = 0; i < greySubPictCount; i++)
                } // if (doDeepGreyColor)
                else if ((majorType >= DEEP_GREY_9) && (majorType <= DEEP_GREY_16)) {
                    index = 0;
                    sliceNumber = 0;
                    for (i = 0; i < subPictCount; i++) {
                        if (imageTypeLocation[i] == majorType) {
                            raFile.seek(pictLocation[i]);
                            Preferences.debug("Located at subPictCount " + (i+1) + " of " + subPictCount + "\n");
                            uncompressedSize = readInt(endianess);
                            Preferences.debug("Uncompressed data size = " + uncompressedSize + "\n");
                            compressedSize = readInt(endianess);
                            Preferences.debug("Compressed data size = " + compressedSize + "\n");
                            dataWidth = uncompressedSize/yDim;
                            Preferences.debug("dataWidth = " + dataWidth + "\n");
//                          The data is compressed using MiniLZO. 
                            compressedBuffer = new byte[compressedSize];
                            raFile.read(compressedBuffer);
                            lzo = new LZOCodec();
                            try {
                                uncompressedBuffer = lzo.decompress(compressedBuffer);
                            }
                            catch (Exception e){
                                MipavUtil.displayError("Exception on LZOCodec.decompress(compressedBuffer)");
                                raFile.close();
                                return null;
                            }
                            Preferences.debug("Actual uncompressed data size = " + uncompressedBuffer.length + "\n");
                            for (y = 0; y < yDim; y++) {
                                for (x = 0; x < xDim; x++) {
                                    destIndex = y*xDim + x;
                                    srcIndex = 2*y*xDim + 2*x;
                                    shortBuffer[destIndex] = getBufferShort(uncompressedBuffer, srcIndex, endianess);
                                }
                            }
                            image.importData(sliceNumber * xDim * yDim, shortBuffer, false);
                            sliceNumber++;
                        } // if (imageTypeLocation[i] == majorType)
                    } // for (i = 0; i < greySubPictCount; i++)
                } // else if ((majorType >= DEEP_GREY_9) && (majorType <= DEEP_GREY_16))
                else {
                    for (i = 0; i < subPictCount; i++) {
                        index = 0;
                        component = 0;
                        componentArray = new int[1];
                        if (imageTypeLocation[i] == majorType) {
                            raFile.seek(pictLocation[i]);
                            Preferences.debug("Located at subPictCount " + (i+1) + " of " + subPictCount + "\n");
                            uncompressedSize = readInt(endianess);
                            Preferences.debug("Uncompressed data size = " + uncompressedSize + "\n");
                            compressedSize = readInt(endianess);
                            Preferences.debug("Compressed data size = " + compressedSize + "\n");
                            dataWidth = uncompressedSize/yDim;
                            Preferences.debug("dataWidth = " + dataWidth + "\n");
                            // The data is compressed using MiniLZO.
                            compressedBuffer = new byte[compressedSize];
                            raFile.read(compressedBuffer);
                            lzo = new LZOCodec();
                            try {
                                uncompressedBuffer = lzo.decompress(compressedBuffer);
                            }
                            catch (Exception e){
                                MipavUtil.displayError("Exception on LZOCodec.decompress(compressedBuffer)");
                                raFile.close();
                                return null;
                            }
                            Preferences.debug("Actual uncompressed data size = " + uncompressedBuffer.length + "\n");
                            if ((majorType == MAC_24_BIT_COLOR) && (pixelSizeArray[i] == 32)) {
                                for (y = 0; y < yDim; y++) {
                                    for (x = 0; x < xDim; x++) {
                                        for (color = 1; color <= 3; color++) {
                                            destIndex = 4*y*xDim + 4*x + color;
                                            srcIndex = y*dataWidth + 4*x + color;
                                            byteBuffer[destIndex] = uncompressedBuffer[srcIndex];   
                                        }
                                    }
                                }
                                image.importData(4 * sliceNumber * sliceSize, byteBuffer, false);
                            } // if ((majorType = MAC_24_BIT_COLOR) && (pixelSizeArray[i] == 32))
                            else if (majorType == MAC_16_BIT_COLOR) {
                                for (y = 0; y < yDim; y++) {
                                    for (x = 0; x < xDim; x++) {
                                        srcIndex = y*dataWidth + 2*x;
                                        shortColor = (short) (((uncompressedBuffer[srcIndex] & 0xff) << 8) |
                                                               (uncompressedBuffer[srcIndex + 1] & 0xff));
                                        destIndex = 4*y*xDim + 4*x;
                                        byteBuffer2[destIndex] = (byte)255;
                                        byteBuffer2[destIndex+1] = (byte)((shortColor & 0x7C00) >>> 7);
                                        byteBuffer2[destIndex+2] = (byte)((shortColor & 0x03E0) >>> 2);
                                        byteBuffer2[destIndex+3] = (byte)((shortColor & 0x001F) << 3);
                                    }
                                }
                                image.importData(4 * sliceNumber * sliceSize, byteBuffer2, false);     
                            } // else if (majorType == MAC_16_BIT_COLOR)
                            else if ((majorType == MAC_256_GREYS) || (majorType == MAC_256_COLORS)) {
                                for (y = 0; y < yDim; y++) {
                                    for (x = 0; x < xDim; x++) {
                                        destIndex = y*xDim + x;
                                        srcIndex = y*dataWidth + x;
                                        byteBuffer[destIndex] = uncompressedBuffer[srcIndex];
                                    }
                                }
                                image.importData(sliceNumber * sliceSize, byteBuffer, false);
                            } // else if ((majorType == MAC_256_GREYS) || (majorType == MAC_256_COLORS))
                            else if ((majorType == MAC_16_GREYS) || (majorType == MAC_16_COLORS)) {
                                for (y = 0; y < yDim; y++){
                                    for (x = 0, k = 0; x < xDim; x++) {
                                        srcIndex = (y*dataWidth + (x >> 1));
                                        destIndex = y*xDim + x;
                                        if (k == 0) {
                                            byteBuffer2[destIndex] = (byte)((uncompressedBuffer[srcIndex] & 0xf0) >>> 4);
                                            k = 1;
                                        }
                                        else if (k == 1) {
                                            byteBuffer2[destIndex] = (byte)(uncompressedBuffer[srcIndex] & 0x0f);  
                                            k = 0;
                                        }
                                    }
                                }
                                image.importData(sliceNumber * sliceSize, byteBuffer2, false);
                            } // else if ((majorType == MAC_16_GREYS) || (majorType == MAC_16_COLORS))
                            else if (majorType == MAC_4_GREYS) {
                                for (y = 0; y < yDim; y++) {
                                    for (x = 0, k = 0; x < xDim; x++) {
                                        srcIndex = (y*dataWidth + (x >> 2));
                                        destIndex = y*xDim + x;
                                        if (k == 0) {
                                            byteBuffer2[destIndex] = (byte)((uncompressedBuffer[srcIndex] & 0xc0) >>> 6);
                                            k = 1;
                                        }
                                        else if (k == 1) {
                                            byteBuffer2[destIndex] = (byte)((uncompressedBuffer[srcIndex] & 0x30) >>> 4);
                                            k = 2;
                                        }
                                        else if (k == 2) {
                                            byteBuffer2[destIndex] = (byte)((uncompressedBuffer[srcIndex] & 0x0c) >>> 2);
                                            k = 3;
                                        }
                                        else if (k == 3) {
                                            byteBuffer2[destIndex] = (byte)(uncompressedBuffer[srcIndex] & 0x03);
                                            k = 0;
                                        }
                                    }
                                }
                                image.importData(sliceNumber * sliceSize, byteBuffer2, false);
                            } // else if (majorType == MAC_4_GREYS)
                            else if (majorType == MAC_1_BIT) {
                                for (y = 0; y < yDim; y++) {
                                    for (x = 0, k = 0x80; x < xDim; x++) {
                                        srcIndex = (y*dataWidth + (x >> 3));
                                        destIndex = y*xDim + x;
                                        if ((uncompressedBuffer[srcIndex] & k) != 0) {
                                            booleanBuffer[destIndex] = true;
                                        }
                                        else {
                                            booleanBuffer[destIndex] = false;
                                        }
                                        if (k == 0x01) {
                                            k = 0x80;
                                        }
                                        else {
                                            k = k >>> 1;
                                        }
                                    }
                                }
                                image.importData(sliceNumber * sliceSize, booleanBuffer, false);
                            } // else if (majorType == MAC_1_BIT)
                            sliceNumber++;
                        } // if (imageTypeLocation[i] == majorType)
                    } // for (i = 0; i < subPictCount; i++)
                } // else
            } // else versionNumber == 5
            fileInfo.setBitDepth(bitDepth);
            for (i = 0; i < imageSlices; i++) {
                image.setFileInfo(fileInfo, i);
            }
            
            image.calcMinMax();
            fireProgressStateChanged(100);
            
        } catch (OutOfMemoryError error) {

            if (image != null) {
                image.disposeLocal();
                image = null;
            }

            System.gc();
            throw error;
        }

        return image;
    }
    
    private String readCString() throws IOException {
        String cString = "";
        boolean nullFound = false;
        byte oneByte[] = new byte[1];
        while (!nullFound) {
            raFile.read(oneByte);
            if (oneByte[0]  == 0) {
                nullFound = true;
            }
            else {
                cString += new String(oneByte);
            }
        } // while (!nullFound)
        return cString;
    }

    
    /**
     * Accessor to set the file name (used for reading COR multiFile).
     *
     * @param  fName  file name of image to read.
     */
    public void setFileName(String fName) {
        fileName = fName;
    }

    
}
