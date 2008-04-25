package gov.nih.mipav.model.file;


import gov.nih.mipav.model.structures.*;

import java.io.*;
import gov.nih.mipav.view.*;

/**
 
 */

public class FileLIFF extends FileBase {
    
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
    private static final int MAC_1_BIT = 1;
    private static final int MAC_4_GREYS = 2;
    private static final int MAC_16_GREYS = 3;
    private static final int MAC_16_COLORS = 4;
    private static final int MAC_256_GREYS = 5;
    private static final int MAC_256_COLORS = 6;
    private static final int MAC_16_BIT_COLOR = 7;
    // LIFF file format documentation has openlab_mac32bitColourImageType = 8L,
    // openlab_mac24bitColourImageType = openlab_mac32bitColourImageType,
    // OpenlabReader.java has MAC_24_BIT_COLOR = 8;
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
    private boolean foundEOF = false;

    /** DOCUMENT ME! */
    private ModelImage image;

    /** DOCUMENT ME! */
    private int[] imageExtents = new int[3];

    /** DOCUMENT ME! */
    private int imageOrientation;

    /** DOCUMENT ME! */
    private float[] imgBuffer = null;

    /** DOCUMENT ME! */
    private float[] imgResols = new float[5];

    /** DOCUMENT ME! */
    private ModelLUT LUT = null;

    /** DOCUMENT ME! */
    private int[] orient = new int[3];
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
        int[] imgExtents;
        int totalSize;
        long fileLength;
        boolean endianess;
        int i, j;
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
        byte pad196[] = new byte[196];
        String appSignature;
        String kindSignature;
        int blkCount;
        int totalBlocks;
        int originalSize;
        int compressedSize;
        int picBlkSize;
        short bitDepth;
        short bitShift;
        String platform;
        short units;
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

        try {
            file = new File(fileDir + fileName);
            raFile = new RandomAccessFile(file, "r");
            
            fileLength = raFile.length();
            
            int byteOrder = raFile.readInt();

            if (byteOrder == 0xffff0000) {
                endianess = FileBase.LITTLE_ENDIAN;
                Preferences.debug("Byte order in unexpectedly little-endian\n");
            } else if (byteOrder == 0x0000ffff) {
                endianess = FileBase.BIG_ENDIAN;
                Preferences.debug("Byte order is the expected big-endian (Macintosh)\n");
            } else {
                raFile.close();
                throw new IOException("LIFF Read Header: Error - first 4 bytes are an illegal " + byteOrder);
            }
            
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
                
                // If the tag is not an image tag, this field should be set to zero
                formatStr = getString(4);
                if ((tagType == 67) || (tagType == 68)) {
                    // This field will most often contain 'PICT', indicating that the image data
                    // is a Macintosh Picture.  For Openlab 5 LIFF files this will be a 'RAWi'
                    // type - this is a compressed raw image instead of PICT data.
                    Preferences.debug("Format of the data in the image tag is " + formatStr + "\n");
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
                    switch (layerMode) {
                        // The 16 transfer modes
                        case 0:
                            Preferences.debug("layerMode = srcCopy\n");
                            break;
                        case 1:
                            Preferences.debug("layerMode = srcOr\n");
                            break;
                        case 2:
                            Preferences.debug("layerMode = srcXor\n");
                            break;
                        case 3:
                            Preferences.debug("layerMode = srcBic\n");
                            break;
                        case 4:
                            Preferences.debug("layerMode = notSrcCopy\n");
                            break;
                        case 5:
                            Preferences.debug("layerMode = notSrcOr\n");
                            break;
                        case 6:
                            Preferences.debug("layerMode = notSrcXor\n");
                            break;
                        case 7:
                            Preferences.debug("layerMode = notSrcBic\n");
                            break;
                        case 8:
                            Preferences.debug("layerMode = patCopy\n");
                            break;
                        case 9:
                            Preferences.debug("layerMode = patOr\n");
                            break;
                        case 10:
                            Preferences.debug("layerMode = patXor\n");
                            break;
                        case 11:
                            Preferences.debug("layerMode = patBic\n");
                            break;
                        case 12:
                            Preferences.debug("layerMode = notPatCopy\n");
                            break;
                        case 13:
                            Preferences.debug("layerMode = notPatOr\n");
                            break;
                        case 14:
                            Preferences.debug("layerMode =  notPatXor\n");
                            break;
                        case 15:
                            Preferences.debug("layerMode = notPatBic\n");
                            break;
                        // 2 special text transfer modes
                        case 49:
                            Preferences.debug("layerMode = grayishTextOr\n");
                            break;
                        case 50:
                            Preferences.debug("layerMode = hilitetransfermode\n");
                            break;
                        // 2 arithmetic transfer modes
                        case 32:
                            Preferences.debug("layerMode = blend\n");
                            break;
                        case 33:
                            Preferences.debug("layerMode = addPin\n");
                            break;
                        // 6 QuickDraw color separation constants
                        case 34:
                            Preferences.debug("layerMode = addOver\n");
                            break;
                        case 35:
                            Preferences.debug("layerMode = subPin\n");
                            break;
                        case 37:
                            Preferences.debug("layerMode = addMax\n");
                            break;
                        case 38:
                            Preferences.debug("layerMode = subOver\n");
                            break;
                        case 39:
                            Preferences.debug("layerMode = adMin\n");
                            break;
                        case 64:
                            Preferences.debug("layerMode = ditherCopy\n");
                            break;
                        // Transparent mode constant
                        case 36:
                            Preferences.debug("layerMode = transparent\n");
                            break;
                        default:
                            Preferences.debug("layerMode has unrecognized value = " + layerMode + "\n");
                    } // switch (layerMode)
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
                    if (isOpenlab2Header[0]  == 1) {
                        // This section only exists if isOpenlab2Header is true
                        if ((128 - (layerNameLength + 1)) > 0) {
                            pad = new byte[128 - (layerNameLength + 1)];
                            raFile.read(pad);
                        }
                        // microSecsTimeStamp is a 64 bit long value giving the absolute timestamp of
                        // the layer in microseconds since January 1, 1904.
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
                    if ((imageType >= DEEP_GREY_9)  && (imageType <= DEEP_GREY_16)) {
                        // Have read 298 bytes, but there are 494 bytes from start of layerinfo record
                        // till "IVEA" starts in PIC comment 101.  So read in another 196 bytes.
                        raFile.read(pad196);
                        appSignature = getString(4);
                        if (appSignature.equals("IVEA")) {
                            Preferences.debug("PIC Comment 101 appSignature is expected " +
                                              appSignature + "\n");
                        }
                        else {
                            Preferences.debug("PIC Comment 101 appSignature is unexpectedly " +
                                              appSignature + "\n");
                        }
                        kindSignature = getString(4);
                        if (kindSignature.equals("dbpq")) {
                            Preferences.debug("kindSignature dbpq indicates that image data follows\n");
                        }
                        else if (kindSignature.equals("dbpl")) {
                            Preferences.debug("kindSignature dbpl indicates that LUT data follows\n");
                        }
                        else {
                            Preferences.debug("kindSignature has unrecognized value = "
                                    + kindSignature + "\n");
                        }
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
                } // else if (tagType == 69)
            } // for (nextOffset = firstTagOffset, i = 1; nextOffset < fileLength - 1; i++)
            
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
                }
            } // for (i = 0; i <= 16; i++)
            Preferences.debug("The MIPAV image will have " + majorTypeCount + 
                              " slices of type " + typeStr[majorType] + "\n");

            
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

    
    /**
     * Accessor to set the file name (used for reading COR multiFile).
     *
     * @param  fName  file name of image to read.
     */
    public void setFileName(String fName) {
        fileName = fName;
    }

    
}
