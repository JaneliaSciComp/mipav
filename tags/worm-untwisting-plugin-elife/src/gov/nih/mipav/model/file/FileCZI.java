package gov.nih.mipav.model.file;


import gov.nih.mipav.model.file.FileInfoBase.Unit;
import gov.nih.mipav.model.structures.ModelImage;
import gov.nih.mipav.model.structures.ModelStorageBase;
import gov.nih.mipav.view.Preferences;

import java.io.File;
import java.io.IOException;
import java.io.RandomAccessFile;
import java.util.Vector;


/**
 * File written using Zeiss Release Version 1.1 for ZEN 2012
 */

public class FileCZI extends FileBase {

    /** 8 bit unsigned */
    private static final int Gray8 = 0;

    /** 16 bit unsigned */
    private static final int Gray16 = 1;

    /** 32 bit IEEE float */
    private static final int Gray32Float = 2;

    /** 8 bit triples, representing the color channels blue, green, and red */
    private static final int Bgr24 = 3;

    /** 16 bit triples, representing the color channels blue, green, and red */
    private static final int Bgr48 = 4;

    /** Triple of 4 byte IEEE float, representing the color channels blue, green, and red */
    private static final int Bgr96Float = 8;

    /** 8 bit triples followed by an alpha (transparency) channel */
    private static final int Bgra32 = 9;

    /** 2 x 4 byte IEEE float, representing the real and imaginary part of a complex number */
    private static final int Gray64ComplexFloat = 10;

    /**
     * A triple of 2 x 4 byte IEE float, representing real and imaginary part of a complex number, for the color
     * channels blue, green, and red
     */
    private static final int Bgr192ComplexFloat = 11;

    /** 32 bit integer [planned] */
    private static final int Gray32 = 12;

    /** Double precision floating point [planned] */
    private static final int Gray64 = 13;

    private static final int Uncompressed = 0;

    private static final int LZW = 2;

    private static final int JpgFile = 1;

    private static final int JpegXrFile = 4;

    // ~ Instance fields
    // ------------------------------------------------------------------------------------------------

    /** DOCUMENT ME! */
    private File file;

    /** DOCUMENT ME! */
    private String fileDir;

    /** DOCUMENT ME! */
    private FileInfoCZI fileInfo;

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

    private final boolean endianess = FileBase.LITTLE_ENDIAN;

    /** DOCUMENT ME! */
    // private ModelLUT LUT = null;

    // ~ Constructors
    // ---------------------------------------------------------------------------------------------------

    /**
     * LIFF reader/writer constructor.
     * 
     * @param fileName file name
     * @param fileDir file directory
     * 
     * @exception IOException if there is an error making the file
     */
    public FileCZI(final String fileName, final String fileDir) throws IOException {

        this.fileName = fileName;
        this.fileDir = fileDir;
    }

    // ~ Methods
    // --------------------------------------------------------------------------------------------------------

    /**
     * Prepares this class for cleanup. Calls the <code>finalize</code> method for existing elements, closes any open
     * files and sets other elements to <code>null</code>.
     */
    @Override
    public void finalize() {
        fileName = null;
        fileDir = null;
        fileInfo = null;
        file = null;
        image = null;
        imageExtents = null;
        imgBuffer = null;
        imgResols = null;
        // LUT = null;

        try {
            super.finalize();
        } catch (final Throwable er) {}
    }

    /**
     * Accessor that returns the file info.
     * 
     * @return FileInfoBase containing the file info
     */
    public FileInfoBase getFileInfo() {
        return fileInfo;
    }

    /**
     * Accessor that returns the image buffer.
     * 
     * @return buffer of image.
     */
    public float[] getImageBuffer() {
        return imgBuffer;
    }

    /**
     * Rreturns LUT if defined.
     * 
     * @return the LUT if defined else it is null
     */
    /*
     * public ModelLUT getModelLUT() { return LUT; }
     */

    /**
     * Reads the CZI header. This method then opens a Model of an image.
     * 
     * @param multiFile <code>true</code> if a set of files each containing a separate 2D image is present <code>
     *                        false</code> if one file with either a 2D image or a stack of 2D images
     * @param one <code>true</code> if only want to read in one image of the 3D set
     * 
     * @return returns the image
     * 
     * @exception IOException if there is an error reading the file
     */
    public ModelImage readImage(final boolean multiFile, final boolean one) throws IOException {
        long fileLength;
        long allocatedSize;
        long usedSize;
        long position;
        int major;
        int minor;
        int reserved1;
        int reserved2;
        long primaryFileGuid;
        long primaryFileGuid2;
        long fileGuid;
        long fileGuid2;
        int filePart;
        long directoryPosition = 0;
        long metadataPosition = 0;
        int updatePending;
        long attachmentDirectoryPosition = 0;
        int xmlSize;
        int attachmentSize;
        String xmlData;
        int metadataSize;
        long dataSize;
        String schemaType;
        int pixelType = Gray8;
        final String typeString[] = new String[14];
        typeString[0] = new String("Gray8");
        typeString[1] = new String("Gray16");
        typeString[2] = new String("Gray32Float");
        typeString[3] = new String("Bgr24");
        typeString[4] = new String("Bgr48");
        typeString[8] = new String("Bgr96Float");
        typeString[9] = new String("Bgra32");
        typeString[10] = new String("Gray64ComplexFloat");
        typeString[11] = new String("Bgr192ComplexFloat");
        typeString[12] = new String("Gray32");
        typeString[13] = new String("Gray64");
        int dataType = ModelStorageBase.UBYTE;
        long filePosition;
        int compression;
        byte pyramidType;
        int dimensionCount;
        int i;
        String dimension[];
        int start[];
        int size[];
        float startCoordinate[];
        int storedSize[];
        int index;
        final Vector<String> imageDimension[] = new Vector[8];
        final Vector<Integer> imageSize[] = new Vector[8];
        final Vector<Float> imageStartCoordinate[] = new Vector[8];
        final Vector<Integer> imageStartIndex[] = new Vector[8];
        final Vector<Long> imageDataSize = new Vector<Long>();
        final Vector<Long> imageDataLocation = new Vector<Long>();
        Vector<Integer> imageColorStartIndex = new Vector<Integer>();
        Vector<Integer> imageMStartIndex = new Vector<Integer>();
        Vector<Integer> imageSStartIndex = new Vector<Integer>();
        Vector<Integer> imageZStartIndex = new Vector<Integer>();
        Vector<Integer> imageTStartIndex = new Vector<Integer>();
        Vector<Integer> imageHStartIndex = new Vector<Integer>();
        long subBlockStart;
        long location;
        int j;
        String metaData;
        int xIndex = -1;
        int yIndex = -1;
        int zIndex = -1;
        int tIndex = -1;
        int hIndex = -1;
        int actualDimensions = 0;
        int firstDimension = -1;
        int secondDimension = -1;
        int thirdDimension = -1;
        int fourthDimension = -1;
        int fifthDimension = -1;
        int bytesSought = 0;
        int bytesRead = 0;
        int totalValuesRead = 0;
        int subBlockValues = 1;
        byte byteBuffer[] = null;
        byte byteBuffer2[] = null;
        short shortBuffer[] = null;
        boolean isColor4 = false;
        boolean isColor3 = false;
        boolean isColor2 = false;
        boolean isColor = false;
        int tagsStart;
        int tagsEnd;
        String tags;
        int focusPositionStart;
        int focusPositionEnd;
        String focusPosition;
        int stageXPositionStart;
        int stageXPositionEnd;
        String stageXPosition;
        int stageYPositionStart;
        int stageYPositionEnd;
        String stageYPosition;
        int acquisitionTimeStart;
        int acquisitionTimeEnd;
        String acquisitionTime;
        int dataSchemaStart;
        int dataSchemaEnd;
        String dataSchema;
        int validBitsPerPixelStart;
        int validBitsPerPixelEnd;
        String validBitsPerPixel;
        int entryCount;
    	String contentFileType;
    	String name;
        int timeStampSegmentSize;
        int numberTimeStamps;
        double timeStamps[];
        int eventListSegmentSize;
        int numberEvents;
        int eventListEntrySize;
        double eventTimes[];
        int eventTypes[];
        int descriptionSize;
        String eventDescriptions[];
        float origin[] = new float[4];
        int lookupTablesSegmentSize;
        int numberLookupTables;
        int lookupTableEntrySize;
        long lookupEntryStartPosition;
        String identifier;
        int numberComponents;
        int componentEntrySize;
        int componentType;
        int numberIntensities;
        int k;
        short intensity[];
        int scalingStart;
        int scalingEnd;
        String scaling;
        int itemsStart;
        int itemsEnd;
        String items;
        int distanceXStart;
        int distanceXEnd;
        String distanceX;
        int xUnitStart;
        int xUnitEnd;
        String xUnit;
        int xValueStart;
        int xValueEnd;
        String xValue;
        int distanceYStart;
        int distanceYEnd;
        String distanceY;
        int yUnitStart;
        int yUnitEnd;
        String yUnit;
        int yValueStart;
        int yValueEnd;
        String yValue;
        int distanceZStart;
        int distanceZEnd;
        String distanceZ;
        int zUnitStart;
        int zUnitEnd;
        String zUnit;
        int zValueStart;
        int zValueEnd;
        String zValue;
        int timeZStart;
        int timeZEnd;
        String timeZ;
        int informationStart;
        int informationEnd;
        String information;
        int documentStart;
        int documentEnd;
        String document;
        int nameStart;
        int nameEnd;
        String imageName;
        int authorStart;
        int authorEnd;
        String author;
        int userNameStart;
        int userNameEnd;
        String userName;
        int subTypeStart;
        int subTypeEnd;
        String subType;
        int titleStart;
        int titleEnd;
        String title;
        int creationDateStart;
        int creationDateEnd;
        String creationDate;
        int descriptionStart;
        int descriptionEnd;
        String description;
        int thumbnailStart;
        int thumbnailEnd;
        String thumbnail;
        int commentStart;
        int commentEnd;
        String comment;
        int ratingStart;
        int ratingEnd;
        String rating;
        int keywordsStart;
        int keywordsEnd;
        String keywords;
        int userStart;
        int userEnd;
        String user;
        int IDStart;
        int IDEnd;
        String ID;
        int displayNameStart;
        int displayNameEnd;
        String displayName;
        int firstNameStart;
        int firstNameEnd;
        String firstName;
        int middleNameStart;
        int middleNameEnd;
        String middleName;
        int lastNameStart;
        int lastNameEnd;
        String lastName;
        int emailStart;
        int emailEnd;
        String email;
        int institutionStart;
        int institutionEnd;
        String institution;
        int experimenterNameStart;
        int experimenterNameEnd;
        String experimenterName;
        int phoneStart;
        int phoneEnd;
        String phone;
        int faxStart;
        int faxEnd;
        String fax;
        int addressStart;
        int addressEnd;
        String address;
        int cityStart;
        int cityEnd;
        String city;
        int stateStart;
        int stateEnd;
        String state;
        int countryStart;
        int countryEnd;
        String country;
        int imageStart;
        int imageEnd;
        String imageString;
        int sizeXStart;
        int sizeXEnd;
        String sizeX;
        int dimX = -1;
        int sizeYStart;
        int sizeYEnd;
        String sizeY;
        int dimY = -1;
        int sizeCStart;
        int sizeCEnd;
        String sizeC;
        int dimC = -1;
        int sizeZStart;
        int sizeZEnd;
        String sizeZ;
        int dimZ = -1;
        int sizeTStart;
        int sizeTEnd;
        String sizeT;
        int dimT = -1;
        int sizeHStart;
        int sizeHEnd;
        String sizeH;
        int dimH = -1;
        int sizeRStart;
        int sizeREnd;
        String sizeR;
        int dimR = -1;
        int sizeSStart;
        int sizeSEnd;
        String sizeS;
        int dimS = -1;
        int sizeIStart;
        int sizeIEnd;
        String sizeI;
        int dimI = -1;
        int sizeMStart;
        int sizeMEnd;
        String sizeM;
        int dimM = -1;
        int sizeBStart;
        int sizeBEnd;
        String sizeB;
        int dimB = -1;
        int sizeVStart;
        int sizeVEnd;
        String sizeV;
        int dimV = -1;
        int pixelTypeStart;
        int pixelTypeEnd;
        String pixelTypeString;
        int componentBitCountStart;
        int componentBitCountEnd;
        String componentBitCount;
        int originalScanDataStart;
        int originalScanDataEnd;
        String originalScanData;
        boolean originalScan;
        int dimensionsStart;
        int dimensionsEnd;
        String dimensions;
        int channelsStart;
        int channelsEnd;
        String channels;
        int channelStart;
        int channelEnd;
        String channel;
        int channelIDStart;
        int channelIDEnd;
        int channelsFound = -1;
        String channelID[] = new String[]{null, null, null, null};
        boolean firstChannelFind;
        int channelNameStart = -1;
        int channelNameEnd = -1;
        String channelName[] = new String[]{null, null, null, null};
        int acquisitionModeStart;
        int acquisitionModeEnd;
        String acquisitionMode[] = new String[]{null, null, null, null};
        int illuminationTypeStart;
        int illuminationTypeEnd;
        String illuminationType[] = new String[]{null, null, null, null};
        int contrastMethodStart;
        int contrastMethodEnd;
        String contrastMethod[] = new String[]{null, null, null, null};
        int illuminationWavelengthStart;
        int illuminationWavelengthEnd;
        String illuminationWavelength[] = new String[]{null, null, null, null};
        int detectionWavelengthStart;
        int detectionWavelengthEnd;
        int rangesStart;
        int rangesEnd;
        String detectionWavelength[] = new String[]{null, null, null, null};
        int excitationWavelengthStart;
        int excitationWavelengthEnd;
        String excitationWavelength[] = new String[]{null, null, null, null};
        int emissionWavelengthStart;
        int emissionWavelengthEnd;
        String emissionWavelength[] = new String[]{null, null, null, null};
        int dyeIDStart;
        int dyeIDEnd;
        String dyeID[] = new String[]{null, null, null, null};
        int dyeDatabaseIDStart;
        int dyeDatabaseIDEnd;
        String dyeDatabaseID[] = new String[]{null, null, null, null};
        int pinholeSizeStart;
        int pinholeSizeEnd;
        String pinholeSize[] = new String[]{null, null, null, null};
        int pinholeSizeAiryStart;
        int pinholeSizeAiryEnd;
        String pinholeSizeAiry[] = new String[]{null, null, null, null};
        int pinholeGeometryStart;
        int pinholeGeometryEnd;
        String pinholeGeometry[] = new String[]{null, null, null, null};
        int displaySettingStart;
        int displaySettingEnd;
        String displaySetting;
        int colorStart;
        int colorEnd;
        String color;
        int unitsOfMeasure[] = new int[4];
        int imageSlices = 1;
        int zDim = -1;
        int tDim = -1;
        int hDim = -1;
        int mDim = -1;
        int sDim = -1;
        // Default color channels are blue, green, read
        int channelColor[] = new int[]{3,2,1,0};
        boolean rapidChangeColor = false;
        boolean slowChangeColor = false;
        boolean phaseColor2Mosaic = false;
        short mosaicShortBuffer[] = null;
        int h;
        int c;
        int m;
        int x;
        int y;
        int z;
        int s;
        int mosaicMinX = 0;
        int mosaicMaxX;
        int mosaicSizeX = 0;
        int mosaicMinY = 0;
        int mosaicMaxY;
        int mosaicSizeY = 0;
        int localSizeX;
        int localSizeY;
        int localMinX;
        int localMinY;
        int sceneMinX = 0;
        int sceneMaxX;
        int sceneSizeX = 0;
        int sceneMinY = 0;
        int sceneMaxY;
        int sceneSizeY = 0;
        boolean zScene = false;
        short sceneShortBuffer[] = null;
        int singlePeakStart;
        int singlePeakEnd;
        int fluorStart;
        int fluorEnd;
        String fluor[] = new String[]{null, null, null, null};
        int NDFilterStart;
        int NDFilterEnd;
        String NDFilter[] = new String[]{null, null, null, null};
        int pockelCellSettingStart;
        int pockelCellSettingEnd;
        String pockelCellSetting[] = new String[]{null, null, null, null};
        int originalColorStart;
        int originalColorEnd;
        String originalColor[] = new String[]{null, null, null, null};
        int exposureTimeStart;
        int exposureTimeEnd;
        String exposureTime[] = new String[]{null, null, null, null};
        int sectionThicknessStart;
        int sectionThicknessEnd;
        String sectionThickness[] = new String[]{null, null, null, null};
        int reflectorStart;
        int reflectorEnd;
        String reflector[] = new String[]{null, null, null, null};
        int condenserContrastStart;
        int condenserContrastEnd;
        String condenserContrast[] = new String[]{null, null, null, null};
        int NACondenserStart;
        int NACondenserEnd;
        String NACondenser[] = new String[]{null, null, null, null};
        int ratioStart;
        int ratioEnd;
        String ratio[] = new String[]{null, null, null, null};
        int detectorSettingsStart;
        int detectorSettingsEnd;
        String detectorSettings;
        int detectorBinningStart;
        int detectorBinningEnd;
        String detectorBinning[] = new String[]{null, null, null, null};
        int detectorGainStart;
        int detectorGainEnd;
        String detectorGain[] = new String[]{null, null, null, null};
        int detectorDigitalGainStart;
        int detectorDigitalGainEnd;
        String detectorDigitalGain[] = new String[]{null, null, null, null};
        int detectorOffsetStart;
        int detectorOffsetEnd;
        String detectorOffset[] = new String[]{null, null, null, null};
        int detectorEMGainStart;
        int detectorEMGainEnd;
        String detectorEMGain[] = new String[]{null, null, null, null};
        int detectorVoltageStart;
        int detectorVoltageEnd;
        String detectorVoltage[] = new String[]{null, null, null, null};
        int detectorReadOutRateStart;
        int detectorReadOutRateEnd;
        String detectorReadOutRate[] = new String[]{null, null, null, null};
        int detectorUseBrightnessContrastCorrectionStart;
        int detectorUseBrightnessContrastCorrectionEnd;
        String detectorUseBrightnessContrastCorrection[] = new String[]{null, null, null, null};
        int lightSourceSettingsStart;
        int lightSourceSettingsEnd;
        String lightSourceSettings;
        int lightSourceWavelengthStart;
        int lightSourceWavelengthEnd;
        String lightSourceWavelength[] = new String[]{null, null, null, null};
        int lightSourceAttenuationStart;
        int lightSourceAttenuationEnd;
        String lightSourceAttenuation[] = new String[]{null, null, null, null};
        int lightSourceIntensityStart;
        int lightSourceIntensityEnd;
        String lightSourceIntensity[] = new String[]{null, null, null, null};
        boolean channelIDFoundBefore[] = new boolean[4];
        boolean channelIDsFoundBefore;
        int lowStart;
        int lowEnd;
        String low[] = new String[]{null, null, null, null};
        int highStart;
        int highEnd;
        String high[] = new String[]{null, null, null, null};
        int gammaStart;
        int gammaEnd;
        String gamma[] = new String[]{null, null, null, null};
        int modeStart;
        int modeEnd;
        String mode[] = new String[]{null, null, null, null};
        int pointsStart;
        int pointsEnd;
        String points[] = new String[]{null, null, null, null};
        int channelDescriptionStart;
        int channelDescriptionEnd;
        String channelDescription[] = new String[]{null, null, null, null};
        int channelWeightStart;
        int channelWeightEnd;
        String channelWeight[] = new String[]{null, null, null, null};
        int instrumentStart;
        int instrumentEnd;
        String instrument;
        int microscopesStart;
        int microscopesEnd;
        String microscopes;
        int microscopeStart;
        int microscopeEnd;
        String microscope;
        int microscopeSystemStart;
        int microscopeSystemEnd;
        String microscopeSystem;
        int microscopeTypeStart;
        int microscopeTypeEnd;
        String microscopeType;
        
        try {
            fileInfo = new FileInfoCZI(fileName, fileDir, FileUtility.CZI); // dummy fileInfo
            fileInfo.setEndianess(endianess);
            imgResols[0] = imgResols[1] = imgResols[2] = imgResols[3] = imgResols[4] = (float) 1.0;

            file = new File(fileDir + fileName);
            raFile = new RandomAccessFile(file, "r");

            fileLength = raFile.length();
            Preferences.debug("fileLength = " + fileLength + "\n", Preferences.DEBUG_FILEIO);
            boolean readSegment = true;
            position = raFile.getFilePointer();
            while (readSegment) {
                // Read a unique 16 byte ANSI -character ID, each prefixed with "ZISRAW"
                final String charID = getString(16);
                // The special name "DELETED" marks a segment as deleted - readers should ignore or skip this segment.
                if (charID.trim().equals("DELETED")) {
                    Preferences.debug("Character ID field = DELETED\n", Preferences.DEBUG_FILEIO);
                    Preferences.debug("Segment being skipped\n", Preferences.DEBUG_FILEIO);
                    allocatedSize = readLong(endianess);
                    if ( (allocatedSize % 32) != 0) {
                        Preferences.debug("Illegal allocated size = " + allocatedSize + " instead of required multiple of 32\n", Preferences.DEBUG_FILEIO);
                        raFile.close();
                        throw new IOException("Illegal allocated size = " + allocatedSize + " instead of required multiple of 32");
                    }
                    Preferences.debug("Allocated size = " + allocatedSize + "\n", Preferences.DEBUG_FILEIO);
                    position += (allocatedSize + 32);
                    if (position < fileLength) {
                        raFile.seek(position);
                        Preferences.debug("position = " + position + "\n", Preferences.DEBUG_FILEIO);
                        continue;
                    } else {
                        break;
                    }
                }
                if ( !charID.startsWith("ZISRAW")) {
                    Preferences.debug("Illegal character ID field without required ZISRAW start = " + charID + "\n", Preferences.DEBUG_FILEIO);
                    raFile.close();
                    throw new IOException("Illegal character ID field without required ZISRAW start = " + charID);
                }
                Preferences.debug("Character ID field = " + charID + "\n", Preferences.DEBUG_FILEIO);
                allocatedSize = readLong(endianess);
                if ( (allocatedSize % 32) != 0) {
                    Preferences.debug("Illegal allocated size = " + allocatedSize + " instead of required multiple of 32\n", Preferences.DEBUG_FILEIO);
                    raFile.close();
                    throw new IOException("Illegal allocated size = " + allocatedSize + " instead of required multiple of 32");
                }
                Preferences.debug("Allocated size = " + allocatedSize + "\n", Preferences.DEBUG_FILEIO);
                usedSize = readLong(endianess);
                if (usedSize > allocatedSize) {
                    Preferences.debug("Illegal used size = " + usedSize + " greater than allocatedSize = " + allocatedSize + "\n", Preferences.DEBUG_FILEIO);
                    raFile.close();
                    throw new IOException("Illegal used size = " + usedSize + " greater than allocatedSize = " + allocatedSize);
                }
                if (usedSize == 0) {
                    usedSize = allocatedSize;
                }
                Preferences.debug("Used size = " + usedSize + "\n", Preferences.DEBUG_FILEIO);
                if (charID.trim().equals("ZISRAWFILE")) {
                    // File header segment
                    major = readInt(endianess);
                    if (major == 1) {
                        Preferences.debug("Major in file header = 1 as expected\n", Preferences.DEBUG_FILEIO);
                    } else {
                        Preferences.debug("Major in file header = " + major + " instead of the expected 1\n", Preferences.DEBUG_FILEIO);
                    }
                    minor = readInt(endianess);
                    if (minor == 0) {
                        Preferences.debug("Minor in file header = 0 as expected\n", Preferences.DEBUG_FILEIO);
                    } else {
                        Preferences.debug("Major in file header = " + minor + " instead of the expected 0\n", Preferences.DEBUG_FILEIO);
                    }
                    reserved1 = readInt(endianess);
                    reserved2 = readInt(endianess);
                    // Single file: primaryFileGuid and the fileGuid are identical. The filePart is 0.
                    // Multi file: In the master file, the primaryFileGuid and the fileGuid are identical. In file
                    // parts, the
                    // primaryFileGuid is the guid of the master file and fileParts > 0.
                    // 16 byte primaryFileGuid but Java long is only 8 bytes
                    primaryFileGuid = readLong(endianess);
                    primaryFileGuid2 = readLong(endianess);
                    // 16 byte fileGuid but Java long is only 8 bytes
                    fileGuid = readLong(endianess);
                    fileGuid2 = readLong(endianess);
                    filePart = readInt(endianess);
                    Preferences.debug("File part number = " + filePart + "\n", Preferences.DEBUG_FILEIO);
                    directoryPosition = readLong(endianess);
                    Preferences.debug("File position of the Directory Segment = " + directoryPosition + "\n", Preferences.DEBUG_FILEIO);
                    metadataPosition = readLong(endianess);
                    Preferences.debug("File position of the Metadata Segment = " + metadataPosition + "\n", Preferences.DEBUG_FILEIO);
                    updatePending = readInt(endianess);
                    if (updatePending == 0) {
                        Preferences.debug("No update is pending\n", Preferences.DEBUG_FILEIO);
                    } else {
                        Preferences.debug("Update pending flag = " + updatePending + "\n", Preferences.DEBUG_FILEIO);
                        Preferences.debug("This flag indicates an update pending\n", Preferences.DEBUG_FILEIO);
                    }
                    attachmentDirectoryPosition = readLong(endianess);
                    Preferences.debug("File position of the Attachment Directory Segment = " + attachmentDirectoryPosition + "\n", Preferences.DEBUG_FILEIO);
                } // if (charID.trim().equals("ZISRAWFILE"))
                else if (charID.trim().equals("ZISRAWDIRECTORY")) {
                    if (directoryPosition != 0) {
                        Preferences.debug("File header gave Directory Segment position as " + directoryPosition + "\n", Preferences.DEBUG_FILEIO);
                    }
                    Preferences.debug("Actual Directory Segment position = " + position + "\n", Preferences.DEBUG_FILEIO);
                    entryCount = readInt(endianess);
                    Preferences.debug("The number of entries = " + entryCount + "\n", Preferences.DEBUG_FILEIO);
                    // Skip 124 reserved bytes
                    raFile.seek(position + 128);
                    // List of entryCount items
                    // Each item is a copy of the directoryEntry in the referenced subBlock segment
                    // No need to read this information twice
                } // else if (charID.trim().equals("ZISRAWDIRECTORY"))
                else if (charID.trim().equals("ZISRAWMETADATA")) {
                    if (metadataPosition != 0) {
                        Preferences.debug("File header gave Metadata Segment position as " + metadataPosition + "\n", Preferences.DEBUG_FILEIO);
                    }
                    Preferences.debug("Actual Metadata Segment position = " + position + "\n", Preferences.DEBUG_FILEIO);
                    xmlSize = readInt(endianess);
                    Preferences.debug("Size of the xml data = " + xmlSize + "\n", Preferences.DEBUG_FILEIO);
                    attachmentSize = readInt(endianess);
                    // Size of the (binary) attachments. NOT USED CURRENTLY
                    Preferences.debug("Currently unused size of binary attachments = " + attachmentSize + "\n", Preferences.DEBUG_FILEIO);
                    // Skip spare 248 bytes
                    raFile.seek(position+256L);
                    xmlData = getString(xmlSize);
                    scalingStart = xmlData.indexOf("<Scaling>");
                    scalingEnd = xmlData.indexOf("</Scaling>");
                    if ((scalingStart >= 0) && (scalingEnd > scalingStart)) {
                        scaling = xmlData.substring(scalingStart, scalingEnd);
                        scalingStart = scaling.indexOf(">");
                        scaling = scaling.substring(scalingStart+1);
                        itemsStart = scaling.indexOf("<Items>");
                        itemsEnd = scaling.indexOf("</Items>");
                        if ((itemsStart >= 0) && (itemsEnd > itemsStart)) {
                            items = scaling.substring(itemsStart, itemsEnd);
                            itemsStart = items.indexOf(">");
                            items = items.substring(itemsStart + 1);
                            //Preferences.debug("Items:\n", Preferences.DEBUG_FILEIO);
                            //Preferences.debug(items + "\n", Preferences.DEBUG_FILEIO);
                            distanceXStart = items.indexOf("<Distance Id=\"X\">");
                            if (distanceXStart >= 0) {
                                distanceX = items.substring(distanceXStart);
                                distanceXEnd = distanceX.indexOf("</Distance>");
                                distanceXStart = distanceX.indexOf(">");
                                distanceX = distanceX.substring(distanceXStart+1, distanceXEnd);
                                xUnitStart = distanceX.indexOf("<DefaultUnitFormat>");
                                xUnitEnd = distanceX.indexOf("</DefaultUnitFormat>");
                                if ((xUnitStart >= 0) && (xUnitEnd > xUnitStart)) {
                                    xUnit = distanceX.substring(xUnitStart,xUnitEnd);
                                    xUnitStart = xUnit.indexOf(">");
                                    xUnit = xUnit.substring(xUnitStart+1);
                                    if (xUnit.equals("m")) {
                                    	unitsOfMeasure[0] = Unit.METERS.getLegacyNum();
                                    	Preferences.debug("X units of measure are meters\n");
                                    }
                                    else if (xUnit.equals("cm")) {
                                    	unitsOfMeasure[0] = Unit.CENTIMETERS.getLegacyNum();
                                    	Preferences.debug("X units of measure are centimeters\n");
                                    }
                                    else if (xUnit.equals("mm")) {
                                    	unitsOfMeasure[0] = Unit.MILLIMETERS.getLegacyNum();
                                    	Preferences.debug("X units of measure are millimeters\n");
                                    }
                                    else if ((xUnit.equals("u")) || (xUnit.equals("um"))) {
                                    	unitsOfMeasure[0] = Unit.MICROMETERS.getLegacyNum();
                                    	Preferences.debug("X units of measure are micrometers\n");
                                    }
                                    else if (xUnit.equals("nm")) {
                                    	unitsOfMeasure[0] = Unit.NANOMETERS.getLegacyNum();
                                    	Preferences.debug("X units of measure are nanometers\n");
                                    }
                                    else if ((xUnit.equals("i")) || (xUnit.equals("inch"))) {
                                    	unitsOfMeasure[0] = Unit.INCHES.getLegacyNum();
                                    	Preferences.debug("X units of measure are inches\n");
                                    }
                                    else if (xUnit.equals("mil")) {
                                    	unitsOfMeasure[0] = Unit.MILS.getLegacyNum();
                                    	Preferences.debug("X units of measure are mils (thousandths of an inch)\n");
                                    }
                                } // if ((xUnitStart >= 0) && (xUnitEnd > xUnitStart))
                                else {
                                	unitsOfMeasure[0] = Unit.METERS.getLegacyNum();
                                	Preferences.debug("X units of measure are meters\n");	
                                }
                                xValueStart = distanceX.indexOf("<Value>");
                                xValueEnd = distanceX.indexOf("</Value>");
                                if ((xValueStart >= 0) && (xValueEnd > xValueStart)) {
                                    xValue = distanceX.substring(xValueStart, xValueEnd);
                                    xValueStart = xValue.indexOf(">");
                                    xValue = xValue.substring(xValueStart+1);
                                    if (Float.valueOf(xValue).floatValue() != 0) {
                                        imgResols[0] = Float.valueOf(xValue).floatValue();
                                        Preferences.debug("imgResols[0] = " + imgResols[0] + "\n", Preferences.DEBUG_FILEIO);
                                    }
                                } // if ((xValueStart >= 0) && (xValueEnd > xValueStart)) 
                            } // if (distanceXStart >= 0)
                            distanceYStart = items.indexOf("<Distance Id=\"Y\">");
                            if (distanceYStart >= 0) {
                                distanceY = items.substring(distanceYStart);
                                distanceYEnd = distanceY.indexOf("</Distance>");
                                distanceYStart = distanceY.indexOf(">");
                                distanceY = distanceY.substring(distanceYStart+1, distanceYEnd);
                                yUnitStart = distanceY.indexOf("<DefaultUnitFormat>");
                                yUnitEnd = distanceY.indexOf("</DefaultUnitFormat>");
                                if ((yUnitStart >= 0) && (yUnitEnd > yUnitStart)) {
                                    yUnit = distanceY.substring(yUnitStart,yUnitEnd);
                                    yUnitStart = yUnit.indexOf(">");
                                    yUnit = yUnit.substring(yUnitStart+1);
                                    if (yUnit.equals("m")) {
                                    	unitsOfMeasure[1] = Unit.METERS.getLegacyNum();
                                    	Preferences.debug("Y units of measure are meters\n");
                                    }
                                    else if (yUnit.equals("cm")) {
                                    	unitsOfMeasure[1] = Unit.CENTIMETERS.getLegacyNum();
                                    	Preferences.debug("Y units of measure are centimeters\n");
                                    }
                                    else if (yUnit.equals("mm")) {
                                    	unitsOfMeasure[1] = Unit.MILLIMETERS.getLegacyNum();
                                    	Preferences.debug("Y units of measure are millimeters\n");
                                    }
                                    else if ((yUnit.equals("u")) || (yUnit.equals("um"))) {
                                    	unitsOfMeasure[1] = Unit.MICROMETERS.getLegacyNum();
                                    	Preferences.debug("Y units of measure are micrometers\n");
                                    }
                                    else if (yUnit.equals("nm")) {
                                    	unitsOfMeasure[1] = Unit.NANOMETERS.getLegacyNum();
                                    	Preferences.debug("Y units of measure are nanometers\n");
                                    }
                                    else if ((yUnit.equals("i")) || (yUnit.equals("inch"))) {
                                    	unitsOfMeasure[1] = Unit.INCHES.getLegacyNum();
                                    	Preferences.debug("Y units of measure are inches\n");
                                    }
                                    else if (yUnit.equals("mil")) {
                                    	unitsOfMeasure[1] = Unit.MILS.getLegacyNum();
                                    	Preferences.debug("Y units of measure are mils (thousandths of an inch)\n");
                                    }
                                } // if ((yUnitStart >= 0) && (yUnitEnd > yUnitStart))
                                else {
                                	unitsOfMeasure[1] = Unit.METERS.getLegacyNum();
                                	Preferences.debug("Y units of measure are meters\n");	
                                }
                                yValueStart = distanceY.indexOf("<Value>");
                                yValueEnd = distanceY.indexOf("</Value>");
                                if ((yValueStart >= 0) && (yValueEnd > yValueStart)) {
                                    yValue = distanceY.substring(yValueStart, yValueEnd);
                                    yValueStart = yValue.indexOf(">");
                                    yValue = yValue.substring(yValueStart+1);
                                    if (Float.valueOf(yValue).floatValue() != 0) {
                                        imgResols[1] = Float.valueOf(yValue).floatValue();
                                        Preferences.debug("imgResols[1] = " + imgResols[1] + "\n", Preferences.DEBUG_FILEIO);
                                    }
                                } // if ((yValueStart >= 0) && (yValueEnd > yValueStart)) 
                            } // if (distanceYStart >= 0)
                            distanceZStart = items.indexOf("<Distance Id=\"Z\">");
                            if (distanceZStart >= 0) {
                                distanceZ = items.substring(distanceZStart);
                                distanceZEnd = distanceZ.indexOf("</Distance>");
                                distanceZStart = distanceZ.indexOf(">");
                                distanceZ = distanceZ.substring(distanceZStart+1, distanceZEnd);
                                zUnitStart = distanceZ.indexOf("<DefaultUnitFormat>");
                                zUnitEnd = distanceZ.indexOf("</DefaultUnitFormat>");
                                if ((zUnitStart >= 0) && (zUnitEnd > zUnitStart)) {
                                    zUnit = distanceZ.substring(zUnitStart,zUnitEnd);
                                    zUnitStart = zUnit.indexOf(">");
                                    zUnit = zUnit.substring(zUnitStart+1);
                                    if (zUnit.equals("m")) {
                                    	unitsOfMeasure[2] = Unit.METERS.getLegacyNum();
                                    	Preferences.debug("Z units of measure are meters\n");
                                    }
                                    else if (zUnit.equals("cm")) {
                                    	unitsOfMeasure[2] = Unit.CENTIMETERS.getLegacyNum();
                                    	Preferences.debug("Z units of measure are centimeters\n");
                                    }
                                    else if (zUnit.equals("mm")) {
                                    	unitsOfMeasure[2] = Unit.MILLIMETERS.getLegacyNum();
                                    	Preferences.debug("Z units of measure are millimeters\n");
                                    }
                                    else if ((zUnit.equals("u")) || (zUnit.equals("um"))) {
                                    	unitsOfMeasure[2] = Unit.MICROMETERS.getLegacyNum();
                                    	Preferences.debug("Z units of measure are micrometers\n");
                                    }
                                    else if (zUnit.equals("nm")) {
                                    	unitsOfMeasure[2] = Unit.NANOMETERS.getLegacyNum();
                                    	Preferences.debug("Z units of measure are nanometers\n");
                                    }
                                    else if ((zUnit.equals("i")) || (zUnit.equals("inch"))) {
                                    	unitsOfMeasure[2] = Unit.INCHES.getLegacyNum();
                                    	Preferences.debug("Z units of measure are inches\n");
                                    }
                                    else if (zUnit.equals("mil")) {
                                    	unitsOfMeasure[2] = Unit.MILS.getLegacyNum();
                                    	Preferences.debug("Z units of measure are mils (thousandths of an inch)\n");
                                    }
                                } // if ((zUnitStart >= 0) && (zUnitEnd > zUnitStart))
                                else {
                                	unitsOfMeasure[2] = Unit.METERS.getLegacyNum();
                                	Preferences.debug("Z units of measure are meters\n");	
                                }
                                zValueStart = distanceZ.indexOf("<Value>");
                                zValueEnd = distanceZ.indexOf("</Value>");
                                if ((zValueStart >= 0) && (zValueEnd > zValueStart)) {
                                    zValue = distanceZ.substring(zValueStart, zValueEnd);
                                    zValueStart = zValue.indexOf(">");
                                    zValue = zValue.substring(zValueStart+1);
                                    if (Float.valueOf(zValue).floatValue() != 0) {
                                        imgResols[2] = Float.valueOf(zValue).floatValue();
                                        Preferences.debug("imgResols[2] = " + imgResols[2] + "\n", Preferences.DEBUG_FILEIO);
                                    }
                                } // if ((zValueStart >= 0) && (zValueEnd > zValueStart)) 
                            } // if (distanceZStart >= 0)
                            timeZStart = items.indexOf("<TimeSpan Id=\"Z\">");
                            if (timeZStart >= 0) {
                                timeZ = items.substring(timeZStart);
                                timeZEnd = timeZ.indexOf("</TimeSpan>");
                                timeZStart = timeZ.indexOf(">");
                                timeZ = timeZ.substring(timeZStart+1, timeZEnd);
                                zUnitStart = timeZ.indexOf("<DefaultUnitFormat>");
                                zUnitEnd = timeZ.indexOf("</DefaultUnitFormat>");
                                if ((zUnitStart >= 0) && (zUnitEnd > zUnitStart)) {
                                    zUnit = timeZ.substring(zUnitStart,zUnitEnd);
                                    zUnitStart = zUnit.indexOf(">");
                                    zUnit = zUnit.substring(zUnitStart+1);
                                    if (zUnit.equals("s")) {
                                    	unitsOfMeasure[2] = Unit.SECONDS.getLegacyNum();
                                    	Preferences.debug("Z units of measure are seconds\n");
                                    }
                                    else if (zUnit.equals("ms")) {
                                    	unitsOfMeasure[2] = Unit.MILLISEC.getLegacyNum();
                                    	Preferences.debug("Z units of measure are milliseconds\n");
                                    }
                                    else if (zUnit.equals("us")) {
                                    	unitsOfMeasure[2] = Unit.MICROSEC.getLegacyNum();
                                    	Preferences.debug("Z units of measure are microseconds\n");
                                    }
                                    else if (zUnit.equals("ns")) {
                                    	unitsOfMeasure[2] = Unit.NANOSEC.getLegacyNum();
                                    	Preferences.debug("Z units of measure are nanoseconds\n");
                                    }
                                } // if ((zUnitStart >= 0) && (zUnitEnd > zUnitStart))
                                else {
                                	unitsOfMeasure[2] = Unit.SECONDS.getLegacyNum();
                                	Preferences.debug("Z units of measure are seconds\n");
                                }
                                zValueStart = timeZ.indexOf("<Value>");
                                zValueEnd = timeZ.indexOf("</Value>");
                                if ((zValueStart >= 0) && (zValueEnd > zValueStart)) {
                                    zValue = timeZ.substring(zValueStart, zValueEnd);
                                    zValueStart = zValue.indexOf(">");
                                    zValue = zValue.substring(zValueStart+1);
                                    if (Float.valueOf(zValue).floatValue() != 0) {
                                        imgResols[2] = Float.valueOf(zValue).floatValue();
                                        Preferences.debug("imgResols[2] = " + imgResols[2] + "\n", Preferences.DEBUG_FILEIO);
                                    }
                                } // if ((zValueStart >= 0) && (zValueEnd > zValueStart)) 
                            } // if (timeZStart >= 0)
                        } // if ((itemsStart >= 0) && (itemsEnd > itemsStart))
                    } // if ((scalingStart >= 0) && (scalingEnd > scalingStart))
                    informationStart = xmlData.indexOf("<Information>");
                    informationEnd = xmlData.indexOf("</Information>");
                    if ((informationStart >= 0) && (informationEnd > informationStart)) {
                        information = xmlData.substring(informationStart, informationEnd);
                        informationStart = information.indexOf(">");
                        information = information.substring(informationStart+1);
                        documentStart = information.indexOf("<Document>");
                        documentEnd = information.indexOf("</Document>");
                        if ((documentStart >= 0) && (documentEnd > documentStart)) {
                        	document = information.substring(documentStart, documentEnd);
                        	documentStart = document.indexOf(">");
                        	document = document.substring(documentStart + 1);
	                        nameStart = document.indexOf("<Name>");
	                        nameEnd = document.indexOf("</Name>");
	                        if ((nameStart >= 0) && (nameEnd > nameStart)) {
	                            imageName = document.substring(nameStart, nameEnd);
	                            nameStart = imageName.indexOf(">");
	                            imageName = imageName.substring(nameStart+1);
	                            imageName = imageName.trim();
	                            Preferences.debug("Image name = " + imageName + "\n", Preferences.DEBUG_FILEIO);
	                            if (imageName.length() > 0) {
	                                fileInfo.setImageName(imageName);
	                            }
	                        } // if ((nameStart >= 0) && (nameEnd > nameStart))
	                        authorStart = document.indexOf("<Author>");
	                        authorEnd = document.indexOf("</Author>");
	                        if ((authorStart >= 0) && (authorEnd > authorStart)) {
	                            author = document.substring(authorStart, authorEnd);
	                            authorStart = author.indexOf(">");
	                            author = author.substring(authorStart+1);
	                            author = author.trim();
	                            Preferences.debug("Author = " + author + "\n", Preferences.DEBUG_FILEIO);
	                            if (author.length() > 0) {
	                                fileInfo.setAuthor(author);
	                            }
	                        } // if ((authorStart >= 0) && (authorEnd > authorStart))
	                        userNameStart = document.indexOf("<UserName>");
	                        userNameEnd = document.indexOf("</UserName>");
	                        if ((userNameStart >= 0) && (userNameEnd > userNameStart)) {
	                            userName = document.substring(userNameStart, userNameEnd);
	                            userNameStart = userName.indexOf(">");
	                            userName = userName.substring(userNameStart+1);
	                            userName = userName.trim();
	                            Preferences.debug("User name = " + userName + "\n", Preferences.DEBUG_FILEIO);
	                            if (userName.length() > 0) {
	                                fileInfo.setUserName(userName);
	                            }
	                        } // if ((userNameStart >= 0) && (userNameEnd > userNameStart))
	                        subTypeStart = document.indexOf("<SubType>");
	                        subTypeEnd = document.indexOf("</SubType>");
	                        if ((subTypeStart >= 0) && (subTypeEnd > subTypeStart)) {
	                            subType = document.substring(subTypeStart,subTypeEnd);
	                            subTypeStart = subType.indexOf(">");
	                            subType = subType.substring(subTypeStart+1);
	                            subType = subType.trim();
	                            Preferences.debug("SubType = " + subType + "\n", Preferences.DEBUG_FILEIO);
	                            if (subType.length() > 0) {
	                                fileInfo.setSubType(subType);
	                            }
	                        } // if ((subTypeStart >= 0) && (subTypeEnd > subTypeStart))
	                        titleStart = document.indexOf("<Title>");
	                        titleEnd = document.indexOf("</Title>");
	                        if ((titleStart >= 0) && (titleEnd > titleStart)) {
	                        	title = document.substring(titleStart, titleEnd);
	                        	titleStart = title.indexOf(">");
	                        	title = title.substring(titleStart+1);
	                        	title = title.trim();
	                        	Preferences.debug("Title = " + title + "\n", Preferences.DEBUG_FILEIO);
	                        	if (title.length() > 0) {
	                        	    fileInfo.setTitle(title);
	                        	}
	                        } // if ((titleStart >= 0) && (titleEnd > titleStart)) 
	                        creationDateStart = document.indexOf("<CreationDate>");
	                        creationDateEnd = document.indexOf("</CreationDate>");
	                        if ((creationDateStart >= 0) && (creationDateEnd > creationDateStart)) {
	                            creationDate = document.substring(creationDateStart, creationDateEnd);
	                            creationDateStart = creationDate.indexOf(">");
	                            creationDate = creationDate.substring(creationDateStart + 1);
	                            creationDate = creationDate.trim();
	                            Preferences.debug("Creation date = " + creationDate + "\n", Preferences.DEBUG_FILEIO);
	                            if (creationDate.length() > 0) {
	                                fileInfo.setCreationDate(creationDate);
	                            }
	                        } // if ((creationDateStart >= 0) && (creationDateEnd > creationDateStart))
	                        descriptionStart = document.indexOf("<Description>");
	                        descriptionEnd = document.indexOf("</Description>");
	                        if ((descriptionStart >= 0) && (descriptionEnd > descriptionStart)) {
	                            description = document.substring(descriptionStart, descriptionEnd);
	                            descriptionStart = description.indexOf(">");
	                            description = description.substring(descriptionStart+1);
	                            description = description.trim();
	                            Preferences.debug("Description = " + description + "\n", Preferences.DEBUG_FILEIO);
	                            if (description.length() > 0) {
	                                fileInfo.setDescription(description);
	                            }
	                        } // if ((descriptionStart >= 0) && (descriptionEnd > descriptionStart))
	                        thumbnailStart = document.indexOf("<Thumbnail>");
	                        thumbnailEnd = document.indexOf("</Thumbnail>");
	                        if ((thumbnailStart >= 0) && (thumbnailEnd >= thumbnailStart)) {
	                            thumbnail = document.substring(thumbnailStart, thumbnailEnd);
	                            thumbnailStart = thumbnail.indexOf(">");
	                            thumbnail = thumbnail.substring(thumbnailStart + 1);
	                            thumbnail = thumbnail.trim();
	                            Preferences.debug("Thumbnail = " + thumbnail + "\n", Preferences.DEBUG_FILEIO);
	                            if (thumbnail.length() > 0) {
	                                fileInfo.setThumbnail(thumbnail);
	                            }
	                        } // if ((thumbnailStart >= 0) && (thumbnailEnd >= thumbnailStart))
	                        commentStart = document.indexOf("<Comment>");
	                        commentEnd = document.indexOf("</Comment>");
	                        if ((commentStart >= 0) && (commentEnd > commentStart)) {
	                            comment = document.substring(commentStart, commentEnd);
	                            commentStart = comment.indexOf(">");
	                            comment = comment.substring(commentStart + 1);
	                            comment = comment.trim();
	                            Preferences.debug("Comment = " + comment + "\n", Preferences.DEBUG_FILEIO);
	                            if (comment.length() > 0) {
	                                fileInfo.setComment(comment);
	                            }
	                        } // if ((commentStart >= 0) && (commentEnd > commentStart))
	                        ratingStart = document.indexOf("<Rating>");
	                        ratingEnd = document.indexOf("</Rating>");
	                        if ((ratingStart >= 0) && (ratingEnd > ratingStart)) {
	                            rating = document.substring(ratingStart, ratingEnd);
	                            ratingStart = rating.indexOf(">");
	                            rating = rating.substring(ratingStart + 1);
	                            rating = rating.trim();
	                            Preferences.debug("Rating = " + rating + "\n", Preferences.DEBUG_FILEIO);
	                            if (rating.length() > 0) {
	                                fileInfo.setRating(rating);
	                            }
	                        } // if ((ratingStart >= 0) && (ratingEnd > ratingStart))
	                        keywordsStart = document.indexOf("<Keywords>");
	                        keywordsEnd = document.indexOf("</Keywords>");
	                        if ((keywordsStart >= 0) && (keywordsEnd > keywordsStart)) {
	                            keywords = document.substring(keywordsStart, keywordsEnd);
	                            keywordsStart = keywords.indexOf(">");
	                            keywords = keywords.substring(keywordsStart+1);
	                            keywords = keywords.trim();
	                            Preferences.debug("Keywords = " + keywords + "\n", Preferences.DEBUG_FILEIO);
	                            if (keywords.length() > 0) {
	                                fileInfo.setKeywords(keywords);
	                            }
	                        } // if ((keywordsStart >= 0) && (keywordsEnd > keywordsStart)) 
                        } // if ((documentStart >= 0) && (documentEnd > documentStart))
                        userStart= information.indexOf("<User>");
                        userEnd = information.indexOf("</User>");
                        if ((userStart >= 0) && (userEnd > userStart)) {
                            user = information.substring(userStart, userEnd);
                            userStart = user.indexOf(">");
                            user = user.substring(userStart+1);
                            IDStart = user.indexOf("<Id>");
                            IDEnd = user.indexOf("</Id>");
                            if ((IDStart >= 0) && (IDEnd > IDStart)) {
                                ID = user.substring(IDStart, IDEnd);
                                IDStart = ID.indexOf(">");
                                ID = ID.substring(IDStart+1);
                                ID = ID.trim();
                                Preferences.debug("ID = " + ID + "\n", Preferences.DEBUG_FILEIO);
                                if (ID.length() > 0) {
                                    fileInfo.setID(ID);	
                                }
                            } // if ((IDStart >= 0) && (IDEnd > IDStart))
                            displayNameStart = user.indexOf("<DisplayName>");
                            displayNameEnd = user.indexOf("</DisplayName>");
                            if ((displayNameStart >= 0) && (displayNameEnd > displayNameStart)) {
                                displayName = user.substring(displayNameStart, displayNameEnd);
                                displayNameStart = displayName.indexOf(">");
                                displayName = displayName.substring(displayNameStart+1);
                                displayName = displayName.trim();
                                Preferences.debug("Display name = " + displayName + "\n", Preferences.DEBUG_FILEIO);
                                if (displayName.length() > 0) {
                                	fileInfo.setDisplayName(displayName);
                                }
                            } // if ((displayNameStart >= 0) && (displayNameEnd > displayNameStart))
                            firstNameStart = user.indexOf("<FirstName>");
                            firstNameEnd = user.indexOf("</FirstName>");
                            if ((firstNameStart >= 0) && (firstNameEnd > firstNameStart)) {
                                firstName = user.substring(firstNameStart, firstNameEnd);
                                firstNameStart = firstName.indexOf(">");
                                firstName = firstName.substring(firstNameStart+1);
                                firstName = firstName.trim();
                                Preferences.debug("First name = " + firstName + "\n", Preferences.DEBUG_FILEIO);
                                if (firstName.length() > 0) {
                                	fileInfo.setFirstName(firstName);
                                }
                            } // if ((firstNameStart >= 0) && (firstNameEnd > firstNameStart)) 
                            middleNameStart = user.indexOf("<MiddleName>");
                            middleNameEnd = user.indexOf("</MiddleName>");
                            if ((middleNameStart >= 0) && (middleNameEnd > middleNameStart)) {
                                middleName = user.substring(middleNameStart, middleNameEnd);
                                middleNameStart = middleName.indexOf(">");
                                middleName = middleName.substring(middleNameStart+1);
                                middleName = middleName.trim();
                                Preferences.debug("Middle name = " + middleName + "\n", Preferences.DEBUG_FILEIO);
                                if (middleName.length() > 0) {
                                	fileInfo.setMiddleName(middleName);
                                }
                            } // if ((middleNameStart >= 0) && (middleNameEnd > middleNameStart)) 
                            lastNameStart = user.indexOf("<LastName>");
                            lastNameEnd = user.indexOf("</LastName>");
                            if ((lastNameStart >= 0) && (lastNameEnd > lastNameStart)) {
                                lastName = user.substring(lastNameStart, lastNameEnd);
                                lastNameStart = lastName.indexOf(">");
                                lastName = lastName.substring(lastNameStart+1);
                                lastName = lastName.trim();
                                Preferences.debug("Last name = " + lastName + "\n", Preferences.DEBUG_FILEIO);
                                if (lastName.length() > 0) {
                                	fileInfo.setLastName(lastName);
                                }
                            } // if ((lastNameStart >= 0) && (lastNameEnd > lastNameStart)) 
                            emailStart = user.indexOf("<Email>");
                            emailEnd = user.indexOf("</Email>");
                            if ((emailStart >= 0) && (emailEnd > emailStart)) {
                                email = user.substring(emailStart, emailEnd);
                                emailStart = email.indexOf(">");
                                email = email.substring(emailStart);
                                email = email.trim();
                                Preferences.debug("Email = " + email + "\n", Preferences.DEBUG_FILEIO);
                                if (email.length() > 0) {
                                	fileInfo.setEmail(email);
                                }
                            } // if ((emailStart >= 0) && (emailEnd > emailStart))
                            institutionStart = user.indexOf("<Institution>");
                            institutionEnd = user.indexOf("</Institution>");
                            if ((institutionStart >= 0) && (institutionEnd > institutionStart)) {
                            	institution = user.substring(institutionStart, institutionEnd);
                            	institutionStart = institution.indexOf(">");
                            	institution = institution.substring(institutionStart+1);
                            	institution = institution.trim();
                            	Preferences.debug("Institution = " + institution + "\n", Preferences.DEBUG_FILEIO);
                            	if (institution.length() > 0) {
                            		fileInfo.setInstitution(institution);
                            	}
                            } // if ((institutionStart >= 0) && (institutionEnd > institutionStart)) 
                            experimenterNameStart = user.indexOf("<UserName>");
                            experimenterNameEnd = user.indexOf("</UserName>");
                            if ((experimenterNameStart >= 0) && (experimenterNameEnd > experimenterNameStart)) {
                                experimenterName = user.substring(experimenterNameStart, experimenterNameEnd);
                                experimenterNameStart = experimenterName.indexOf(">");
                                experimenterName = experimenterName.substring(experimenterNameStart+1);
                                experimenterName = experimenterName.trim();
                                Preferences.debug("Experimenter name = " + experimenterName + "\n", Preferences.DEBUG_FILEIO);
                                if (experimenterName.length() > 0) {
                                	fileInfo.setExperimenterName(experimenterName);
                                }
                            } // if ((experimenterNameStart >= 0) && (experimenterNameEnd > experimenterNameStart))
                            phoneStart = user.indexOf("<Phone>");
                            phoneEnd = user.indexOf("</Phone>");
                            if ((phoneStart >= 0) && (phoneEnd > phoneStart)) {
                                phone = user.substring(phoneStart, phoneEnd);
                                phoneStart = phone.indexOf(">");
                                phone = phone.substring(phoneStart+1);
                                phone = phone.trim();
                                Preferences.debug("Phone = " + phone + "\n", Preferences.DEBUG_FILEIO);
                                if (phone.length() > 0) {
                                	fileInfo.setPhone(phone);
                                }
                            } // if ((phoneStart >= 0) && (phoneEnd > phoneStart))
                            faxStart = user.indexOf("<Fax>");
                            faxEnd = user.indexOf("</Fax>");
                            if ((faxStart >= 0) && (faxEnd > faxStart)) {
                                fax = user.substring(faxStart, faxEnd);
                                faxStart = fax.indexOf(">");
                                fax = fax.substring(faxStart+1);
                                fax = fax.trim();
                                Preferences.debug("Fax = " + fax + "\n", Preferences.DEBUG_FILEIO);
                                if (fax.length() > 0) {
                                	fileInfo.setFax(fax);
                                }
                            } // if ((faxStart >= 0) && (faxEnd > faxStart))
                            addressStart = user.indexOf("<Address>");
                            addressEnd = user.indexOf("</Address>");
                            if ((addressStart >= 0) && (addressEnd > addressStart)) {
                                address = user.substring(addressStart, addressEnd);
                                addressStart = address.indexOf(">");
                                address = address.substring(addressStart+1);
                                address = address.trim();
                                Preferences.debug("Address = " + address + "\n", Preferences.DEBUG_FILEIO);
                                if (address.length() > 0) {
                                	fileInfo.setAddress(address);
                                }
                            } // if ((addressStart >= 0) && (addressEnd > addressStart))
                            cityStart = user.indexOf("<City>");
                            cityEnd = user.indexOf("</City>");
                            if ((cityStart >= 0) && (cityEnd > cityStart)) {
                                city = user.substring(cityStart,cityEnd);
                                cityStart = city.indexOf(">");
                                city = city.substring(cityStart+1);
                                city = city.trim();
                                Preferences.debug("City = " + city + "\n", Preferences.DEBUG_FILEIO);
                                if (city.length() > 0) {
                                	fileInfo.setCity(city);
                                }
                            } // if ((cityStart >= 0) && (cityEnd > cityStart)) 
                            stateStart = user.indexOf("<State>");
                            stateEnd = user.indexOf("</State>");
                            if ((stateStart >= 0) && (stateEnd > stateStart)) {
                                state = user.substring(stateStart, stateEnd);
                                stateStart = state.indexOf(">");
                                state = state.substring(stateStart+1);
                                state = state.trim();
                                Preferences.debug("State = " + state + "\n", Preferences.DEBUG_FILEIO);
                                if (state.length() > 0) {
                                	fileInfo.setState(state);
                                }
                            } // if ((stateStart >= 0) && (stateEnd > stateStart))
                            countryStart = user.indexOf("<Country>");
                            countryEnd = user.indexOf("</Country>");
                            if ((countryStart >= 0) && (countryEnd > countryStart)) {
                                country = user.substring(countryStart, countryEnd);
                                countryStart = country.indexOf(">");
                                country = country.substring(countryStart+1);
                                country = country.trim();
                                Preferences.debug("Country = " + country + "\n", Preferences.DEBUG_FILEIO);
                                if (country.length() > 0) {
                                	fileInfo.setCountry(country);
                                }
                            } // if ((countryStart >= 0) && (countryEnd > countryStart))
                        } // if ((userStart >= 0) && (userEnd > userStart)) 
                        instrumentStart = information.indexOf("<Instrument>");
                        instrumentEnd = information.indexOf("</Instrument>");
                        if ((instrumentStart >= 0) && (instrumentEnd > instrumentStart)) {
                        	instrument = information.substring(instrumentStart, instrumentEnd);
                        	instrumentStart = instrument.indexOf(">");
                        	instrument = instrument.substring(instrumentStart + 1);
                        	microscopesStart = instrument.indexOf("<Microscopes>");
                        	microscopesEnd = instrument.indexOf("</Microscopes>");
                        	if ((microscopesStart  >= 0) && (microscopesEnd > microscopesStart)) {
                        	    microscopes = instrument.substring(microscopesStart, microscopesEnd);
                        	    microscopesStart = microscopes.indexOf(">");
                        	    microscopes = microscopes.substring(microscopesStart+1);
                        	    microscopeStart = microscopes.indexOf("<Microscope Id=\"Microscope:");
                            	microscopeEnd = microscopes.indexOf("</Microscope>");
                            	if ((microscopeStart  >= 0) && (microscopeEnd > microscopeStart)) {
                            	    microscope = microscopes.substring(microscopeStart, microscopeEnd);
                            	    microscopeStart = microscope.indexOf(">");
                            	    microscope = microscope.substring(microscopeStart+1);
                            	    microscopeSystemStart = microscope.indexOf("<System>");
                                	microscopeSystemEnd = microscope.indexOf("</System>");
                                	if ((microscopeSystemStart  >= 0) && (microscopeSystemEnd > microscopeSystemStart)) {
                                	    microscopeSystem = microscope.substring(microscopeSystemStart, microscopeSystemEnd);
                                	    microscopeSystemStart = microscopeSystem.indexOf(">");
                                	    microscopeSystem = microscopeSystem.substring(microscopeSystemStart+1);
                                	    Preferences.debug("Microscope system = " + microscopeSystem + "\n", Preferences.DEBUG_FILEIO);
                                	    fileInfo.setMicroscopeSystem(microscopeSystem);
                                	} // if ((microscopeSystemStart  >= 0) && (microscopeSystemEnd > microscopeSystemStart))
                                	microscopeTypeStart = microscope.indexOf("<Type>");
                                	microscopeTypeEnd = microscope.indexOf("</Type>");
                                	if ((microscopeTypeStart  >= 0) && (microscopeTypeEnd > microscopeTypeStart)) {
                                	    microscopeType = microscope.substring(microscopeTypeStart, microscopeTypeEnd);
                                	    microscopeTypeStart = microscopeType.indexOf(">");
                                	    microscopeType = microscopeType.substring(microscopeTypeStart+1);
                                	    Preferences.debug("Microscope type = " + microscopeType + "\n", Preferences.DEBUG_FILEIO);
                                	    fileInfo.setMicroscopeType(microscopeType);
                                	} // if ((microscopeTypeStart  >= 0) && (microscopeTypeEnd > microscopeTypeStart))
                            	} // if ((microscopeStart  >= 0) && (microscopeEnd > microscopeStart))
                        	} // if ((microscopesStart  >= 0) && (microscopesEnd > microscopesStart))
                        } // if ((instrumentStart >= 0) && (instrumentEnd > instrumentStart))
                        imageStart = information.indexOf("<Image>");
                        imageEnd = information.indexOf("</Image>");
                        if ((imageStart >= 0) && (imageEnd > imageStart)) {
                            imageString = information.substring(imageStart, imageEnd);	
                            imageStart = imageString.indexOf(">");
                            imageString = imageString.substring(imageStart+1);
                            sizeXStart = imageString.indexOf("<SizeX>");
                            sizeXEnd = imageString.indexOf("</SizeX>");
                            if ((sizeXStart >= 0) && (sizeXEnd > sizeXStart)) {
                            	sizeX = imageString.substring(sizeXStart, sizeXEnd);
                            	sizeXStart = sizeX.indexOf(">");
                            	sizeX = sizeX.substring(sizeXStart+1);
                            	dimX = Integer.valueOf(sizeX).intValue();
                            	Preferences.debug("dimX from xmlData = " + dimX + "\n", Preferences.DEBUG_FILEIO);
                            } // if ((sizeXStart >= 0) && (sizeXEnd > sizeXStart))
                            sizeYStart = imageString.indexOf("<SizeY>");
                            sizeYEnd = imageString.indexOf("</SizeY>");
                            if ((sizeYStart >= 0) && (sizeYEnd > sizeYStart)) {
                            	sizeY = imageString.substring(sizeYStart, sizeYEnd);
                            	sizeYStart = sizeY.indexOf(">");
                            	sizeY = sizeY.substring(sizeYStart+1);
                            	dimY = Integer.valueOf(sizeY).intValue();
                            	Preferences.debug("dimY from xmlData = " + dimY + "\n", Preferences.DEBUG_FILEIO);
                            } // if ((sizeYStart >= 0) && (sizeYEnd > sizeYStart))
                            sizeCStart = imageString.indexOf("<SizeC>");
                            sizeCEnd = imageString.indexOf("</SizeC>");
                            if ((sizeCStart >= 0) && (sizeCEnd > sizeCStart)) {
                            	sizeC = imageString.substring(sizeCStart, sizeCEnd);
                            	sizeCStart = sizeC.indexOf(">");
                            	sizeC = sizeC.substring(sizeCStart+1);
                            	dimC = Integer.valueOf(sizeC).intValue();
                            	Preferences.debug("dimC from xmlData = " + dimC + "\n", Preferences.DEBUG_FILEIO);
                            } // if ((sizeCStart >= 0) && (sizeCEnd > sizeCStart))
                            sizeZStart = imageString.indexOf("<SizeZ>");
                            sizeZEnd = imageString.indexOf("</SizeZ>");
                            if ((sizeZStart >= 0) && (sizeZEnd > sizeZStart)) {
                            	sizeZ = imageString.substring(sizeZStart, sizeZEnd);
                            	sizeZStart = sizeZ.indexOf(">");
                            	sizeZ = sizeZ.substring(sizeZStart+1);
                            	dimZ = Integer.valueOf(sizeZ).intValue();
                            	Preferences.debug("dimZ from xmlData = " + dimZ + "\n", Preferences.DEBUG_FILEIO);
                            } // if ((sizeZStart >= 0) && (sizeZEnd > sizeZStart))
                            sizeTStart = imageString.indexOf("<SizeT>");
                            sizeTEnd = imageString.indexOf("</SizeT>");
                            if ((sizeTStart >= 0) && (sizeTEnd > sizeTStart)) {
                            	sizeT = imageString.substring(sizeTStart, sizeTEnd);
                            	sizeTStart = sizeT.indexOf(">");
                            	sizeT = sizeT.substring(sizeTStart+1);
                            	dimT = Integer.valueOf(sizeT).intValue();
                            	Preferences.debug("dimT from xmlData = " + dimT + "\n", Preferences.DEBUG_FILEIO);
                            } // if ((sizeTStart >= 0) && (sizeTEnd > sizeTStart))
                            sizeHStart = imageString.indexOf("<SizeH>");
                            sizeHEnd = imageString.indexOf("</SizeH>");
                            if ((sizeHStart >= 0) && (sizeHEnd > sizeHStart)) {
                            	sizeH = imageString.substring(sizeHStart, sizeHEnd);
                            	sizeHStart = sizeH.indexOf(">");
                            	sizeH = sizeH.substring(sizeHStart+1);
                            	dimH = Integer.valueOf(sizeH).intValue();
                            	Preferences.debug("Number of phases = " + dimH + "\n", Preferences.DEBUG_FILEIO);
                            	fileInfo.setDimH(dimH);
                            } // if ((sizeHStart >= 0) && (sizeHEnd > sizeHStart))
                            sizeRStart = imageString.indexOf("<SizeR>");
                            sizeREnd = imageString.indexOf("</SizeR>");
                            if ((sizeRStart >= 0) && (sizeREnd > sizeRStart)) {
                            	sizeR = imageString.substring(sizeRStart, sizeREnd);
                            	sizeRStart = sizeR.indexOf(">");
                            	sizeR = sizeR.substring(sizeRStart+1);
                            	dimR = Integer.valueOf(sizeR).intValue();
                            	Preferences.debug("Number of rotation angles (indices) = " + dimR + "\n", Preferences.DEBUG_FILEIO);
                            	fileInfo.setDimR(dimR);
                            } // if ((sizeRStart >= 0) && (sizeREnd > sizeRStart))
                            sizeSStart = imageString.indexOf("<SizeS>");
                            sizeSEnd = imageString.indexOf("</SizeS>");
                            if ((sizeSStart >= 0) && (sizeSEnd > sizeSStart)) {
                            	sizeS = imageString.substring(sizeSStart, sizeSEnd);
                            	sizeSStart = sizeS.indexOf(">");
                            	sizeS = sizeS.substring(sizeSStart+1);
                            	dimS = Integer.valueOf(sizeS).intValue();
                            	Preferences.debug("Number of scenes = " + dimS + "\n", Preferences.DEBUG_FILEIO);
                            	fileInfo.setDimS(dimS);
                            } // if ((sizeSStart >= 0) && (sizeSEnd > sizeSStart))
                            sizeIStart = imageString.indexOf("<SizeI>");
                            sizeIEnd = imageString.indexOf("</SizeI>");
                            if ((sizeIStart >= 0) && (sizeIEnd > sizeIStart)) {
                            	sizeI = imageString.substring(sizeIStart, sizeIEnd);
                            	sizeIStart = sizeI.indexOf(">");
                            	sizeI = sizeI.substring(sizeIStart+1);
                            	dimI = Integer.valueOf(sizeI).intValue();
                            	Preferences.debug("Number of illumination direction indices = " + dimI + "\n", Preferences.DEBUG_FILEIO);
                            	fileInfo.setDimI(dimI);
                            } // if ((sizeIStart >= 0) && (sizeIEnd > sizeIStart))
                            sizeMStart = imageString.indexOf("<SizeM>");
                            sizeMEnd = imageString.indexOf("</SizeM>");
                            if ((sizeMStart >= 0) && (sizeMEnd > sizeMStart)) {
                            	sizeM = imageString.substring(sizeMStart, sizeMEnd);
                            	sizeMStart = sizeM.indexOf(">");
                            	sizeM = sizeM.substring(sizeMStart+1);
                            	dimM = Integer.valueOf(sizeM).intValue();
                            	Preferences.debug("Number of mosaic tiles (regular mosaics only) = " + dimM + 
                            			"\n", Preferences.DEBUG_FILEIO);
                            	fileInfo.setDimM(dimM);
                            } // if ((sizeMStart >= 0) && (sizeMEnd > sizeMStart))
                            sizeBStart = imageString.indexOf("<SizeB>");
                            sizeBEnd = imageString.indexOf("</SizeB>");
                            if ((sizeBStart >= 0) && (sizeBEnd > sizeBStart)) {
                            	sizeB = imageString.substring(sizeBStart, sizeBEnd);
                            	sizeBStart = sizeB.indexOf(">");
                            	sizeB = sizeB.substring(sizeBStart+1);
                            	dimB = Integer.valueOf(sizeB).intValue();
                            	Preferences.debug("Number of acquisition /recording / blocks = " + dimB + 
                            			"\n", Preferences.DEBUG_FILEIO);
                            	fileInfo.setDimB(dimB);
                            } // if ((sizeBStart >= 0) && (sizeBEnd > sizeBStart))
                            sizeVStart = imageString.indexOf("<SizeV>");
                            sizeVEnd = imageString.indexOf("</SizeV>");
                            if ((sizeVStart >= 0) && (sizeVEnd > sizeVStart)) {
                            	sizeV = imageString.substring(sizeVStart, sizeVEnd);
                            	sizeVStart = sizeV.indexOf(">");
                            	sizeV = sizeV.substring(sizeVStart+1);
                            	dimV = Integer.valueOf(sizeV).intValue();
                            	Preferences.debug("Number of views in a multi-view image = " + dimV + 
                            			"\n", Preferences.DEBUG_FILEIO);
                            	fileInfo.setDimV(dimV);
                            } // if ((sizeVStart >= 0) && (sizeVEnd > sizeVStart))
                            pixelTypeStart = imageString.indexOf("<PixelType>");
                            pixelTypeEnd = imageString.indexOf("</PixelType>");
                            if ((pixelTypeStart >= 0) && (pixelTypeEnd > pixelTypeStart)) {
                                pixelTypeString = imageString.substring(pixelTypeStart, pixelTypeEnd);	
                                pixelTypeStart = pixelTypeString.indexOf(">");
                                pixelTypeString = pixelTypeString.substring(pixelTypeStart+1);
                                Preferences.debug("Pixel type = " + pixelTypeString + "\n", Preferences.DEBUG_FILEIO);
                            } // if ((pixelTypeStart >= 0) && (pixelTypeEnd > pixelTypeStart)) 
                            componentBitCountStart = imageString.indexOf("<ComponentBitCount>");
                            componentBitCountEnd = imageString.indexOf("</ComponentBitCount>");
                            if ((componentBitCountStart >= 0) && (componentBitCountEnd > componentBitCountStart)) {
	                            componentBitCount = imageString.substring(componentBitCountStart, componentBitCountEnd);
	                            componentBitCountStart = componentBitCount.indexOf(">");
	                            componentBitCount = componentBitCount.substring(componentBitCountStart+1);
	                            Preferences.debug("Component bit count for the entire image = " + componentBitCount + "\n",
	                            		Preferences.DEBUG_FILEIO);
                            } // if ((componentBitCountStart >= 0) && (componentBitCountEnd > componentBitCountStart))
                            originalScanDataStart = imageString.indexOf("<OriginalScanData>");
                            originalScanDataEnd = imageString.indexOf("</OriginalScanData>");
                            if ((originalScanDataStart >= 0) && (originalScanDataEnd > originalScanDataStart)) {
                                originalScanData = imageString.substring(originalScanDataStart, originalScanDataEnd);
                                originalScanDataStart = originalScanData.indexOf(">");
                                originalScanData = originalScanData.substring(originalScanDataStart+1);
                                originalScan = Boolean.valueOf(originalScanData).booleanValue();
                                if (originalScan) {
                                	Preferences.debug("The image is the output of a scanning process and has not been modified\n",
                                			Preferences.DEBUG_FILEIO);
                                }
                                else {
                                	Preferences.debug("The image is not the original data of a scanning process\n",
                                			Preferences.DEBUG_FILEIO);
                                }
                                fileInfo.setOriginalScanData(originalScanData);
                            } // if ((originalScanDataStart >= 0) && (originalScanDataEnd > originalScanDataStart))
                            dimensionsStart = imageString.indexOf("<Dimensions>");
                            dimensionsEnd = imageString.indexOf("</Dimensions>");
                            if ((dimensionsStart >= 0) && (dimensionsEnd > dimensionsStart)) {
                                dimensions = imageString.substring(dimensionsStart, dimensionsEnd);
                                dimensionsStart = dimensions.indexOf(">");
                                dimensions = dimensions.substring(dimensionsStart+1);
                                channelsStart = dimensions.indexOf("<Channels>");
                                channelsEnd = dimensions.indexOf("</Channels>");
                                if ((channelsStart >= 0) && (channelsEnd > channelsStart)) {
                                    channels = dimensions.substring(channelsStart, channelsEnd);
                                    firstChannelFind = true;
                                    channelsStart = channels.indexOf(">");
                                    channels = channels.substring(channelsStart+1);
                                    channelStart = channels.indexOf("<Channel Id=\"Channel:");
                                    channelEnd = channels.indexOf("</Channel>");
                                    if (channelStart == -1) {
                                    	channelStart = channels.indexOf("Channel Id=\"");
                                    	firstChannelFind = false;
                                    }
                                    channelsFound = -1;
                                    while ((channelStart >= 0) && (channelEnd > channelStart)) {
                                    	channelsFound++;
                                        channel = channels.substring(channelStart, channelEnd);
                                        if (firstChannelFind) {
                                            channelIDStart = channel.indexOf(":");
                                            channelIDEnd = channel.indexOf("\">"); 
                                        }
                                        else {
                                        	channelIDStart = channel.indexOf("\"");
                                        	channelIDEnd = channel.indexOf("\"", channelStart+1);
                                        	channelNameStart = channel.indexOf("Name=\"");
                                        	channelNameStart = channel.indexOf("\"", channelNameStart+1);
                                        	channelNameEnd = channel.indexOf("\"", channelNameStart+1);
                                        }
                                        if ((channelIDStart >= 0) && (channelIDEnd > channelIDStart)) {
                                            channelID[channelsFound] = channel.substring(channelIDStart+1, channelIDEnd);
                                            Preferences.debug("Channel ID = " + channelID[channelsFound] + "\n", Preferences.DEBUG_FILEIO);
                                        } // if ((channelIDStart >= 0) && (channelIDEnd > channelIDStart))
                                        if ((channelNameStart >= 0) && (channelNameEnd > channelNameStart)) {
                                            channelName[channelsFound] = channel.substring(channelNameStart+1, channelNameEnd);
                                            Preferences.debug("Channel name = " + channelName[channelsFound] + "\n", 
                                            		Preferences.DEBUG_FILEIO);
                                        } // if ((channelNameStart >= 0) && (channelNameEnd > channelNameStart))
                                        acquisitionModeStart = channel.indexOf("<AcquisitionMode>");
                                        acquisitionModeEnd = channel.indexOf("</AcquisitionMode>");
                                        if ((acquisitionModeStart >= 0) && (acquisitionModeEnd > acquisitionModeStart)) {
                                            acquisitionMode[channelsFound] = channel.substring(acquisitionModeStart, acquisitionModeEnd);
                                            acquisitionModeStart = acquisitionMode[channelsFound].indexOf(">");
                                            acquisitionMode[channelsFound] = 
                                            		acquisitionMode[channelsFound].substring(acquisitionModeStart+1);
                                            Preferences.debug("Acquisition mode = " + acquisitionMode[channelsFound] + "\n",
                                            		Preferences.DEBUG_FILEIO);
                                        } // if ((acquisitionModeStart >= 0) && (acquisitionModeEnd > acquisitionModeStart))
                                        illuminationTypeStart = channel.indexOf("<IlluminationType>");
                                        illuminationTypeEnd = channel.indexOf("</IlluminationType>");
                                        if ((illuminationTypeStart >= 0) && (illuminationTypeEnd > illuminationTypeStart)) {
                                            illuminationType[channelsFound] = 
                                            		channel.substring(illuminationTypeStart, illuminationTypeEnd);
                                            illuminationTypeStart = illuminationType[channelsFound].indexOf(">");
                                            illuminationType[channelsFound] = 
                                            		illuminationType[channelsFound].substring(illuminationTypeStart + 1);
                                            Preferences.debug("Illumination type = " + illuminationType[channelsFound] + "\n",
                                            		Preferences.DEBUG_FILEIO);
                                        } // if ((illuminationTypeStart >= 0) && (illuminationTypeEnd > illuminationTypeStart))
                                        contrastMethodStart = channel.indexOf("<ContrastMethod>");
                                        contrastMethodEnd = channel.indexOf("</ContrastMethod>");
                                        if ((contrastMethodStart >= 0) && (contrastMethodEnd > contrastMethodStart)) {
                                            contrastMethod[channelsFound] =
                                            		channel.substring(contrastMethodStart, contrastMethodEnd);
                                            contrastMethodStart = contrastMethod[channelsFound].indexOf(">");
                                            contrastMethod[channelsFound] = 
                                            		contrastMethod[channelsFound].substring(contrastMethodStart + 1);
                                            Preferences.debug("Contrast method = " + contrastMethod[channelsFound] + "\n",
                                            		Preferences.DEBUG_FILEIO);
                                        } // if ((contrastMethodStart >= 0) && (contrastMethodEnd > contrastMethodStart))
                                        illuminationWavelengthStart = channel.indexOf("<IlluminationWavelength>");
                                        illuminationWavelengthEnd = channel.indexOf("</IlluminationWavelength>");
                                        if ((illuminationWavelengthStart >= 0) && 
                                        		(illuminationWavelengthEnd > illuminationWavelengthStart)) {
                                            illuminationWavelength[channelsFound] =
                                            		channel.substring(illuminationWavelengthStart, illuminationWavelengthEnd);
                                            illuminationWavelengthStart = illuminationWavelength[channelsFound].indexOf(">");
                                            illuminationWavelength[channelsFound] = 
                                            		illuminationWavelength[channelsFound].substring(illuminationWavelengthStart+1);
                                            singlePeakStart = illuminationWavelength[channelsFound].indexOf("<SinglePeak>");
                                            singlePeakEnd = illuminationWavelength[channelsFound].indexOf("</SinglePeak>");
                                            if ((singlePeakStart >= 0) && (singlePeakEnd > singlePeakStart)) {
                                            	singlePeakStart = illuminationWavelength[channelsFound].indexOf(">", singlePeakStart+1);
                                            	illuminationWavelength[channelsFound] = 
                                            			illuminationWavelength[channelsFound].substring(singlePeakStart+1, singlePeakEnd);
                                            }
                                            Preferences.debug("Illumination wavelength = " + illuminationWavelength[channelsFound] +
                                            		"\n", Preferences.DEBUG_FILEIO);		
                                        } // if ((illuminationWavelengthStart >= 0) && 
                                        detectionWavelengthStart = channel.indexOf("<DetectionWavelength>");
                                        detectionWavelengthEnd = channel.indexOf("</DetectionWavelength>");
                                        if ((detectionWavelengthStart >= 0) && 
                                        		(detectionWavelengthEnd > detectionWavelengthStart)) {
                                            detectionWavelength[channelsFound] =
                                            		channel.substring(detectionWavelengthStart, detectionWavelengthEnd);
                                            detectionWavelengthStart = detectionWavelength[channelsFound].indexOf(">");
                                            detectionWavelength[channelsFound] = 
                                            		detectionWavelength[channelsFound].substring(detectionWavelengthStart+1);
                                            singlePeakStart = detectionWavelength[channelsFound].indexOf("<SinglePeak>");
                                            singlePeakEnd = detectionWavelength[channelsFound].indexOf("</SinglePeak>");
                                            if ((singlePeakStart >= 0) && (singlePeakEnd > singlePeakStart)) {
                                            	singlePeakStart = detectionWavelength[channelsFound].indexOf(">", singlePeakStart+1);
                                            	detectionWavelength[channelsFound] = 
                                            			detectionWavelength[channelsFound].substring(singlePeakStart+1, singlePeakEnd);
                                            }
                                            rangesStart = detectionWavelength[channelsFound].indexOf("<Ranges>");
                                            rangesEnd = detectionWavelength[channelsFound].indexOf("</Ranges>");
                                            if ((rangesStart >= 0) && (rangesEnd > rangesStart)) {
                                            	rangesStart = detectionWavelength[channelsFound].indexOf(">");
                                            	detectionWavelength[channelsFound] =
                                            			detectionWavelength[channelsFound].substring(rangesStart+1,rangesEnd);
                                            }
                                            Preferences.debug("Detection wavelength = " + detectionWavelength[channelsFound] +
                                            		"\n", Preferences.DEBUG_FILEIO);		
                                        } // if ((detectionWavelengthStart >= 0) && 
                                        excitationWavelengthStart = channel.indexOf("<ExcitationWavelength>");
                                        excitationWavelengthEnd = channel.indexOf("</ExcitationWavelength>");
                                        if ((excitationWavelengthStart >= 0) && 
                                        		(excitationWavelengthEnd > excitationWavelengthStart)) {
                                            excitationWavelength[channelsFound] =
                                            		channel.substring(excitationWavelengthStart, excitationWavelengthEnd);
                                            excitationWavelengthStart = excitationWavelength[channelsFound].indexOf(">");
                                            excitationWavelength[channelsFound] = 
                                            		excitationWavelength[channelsFound].substring(excitationWavelengthStart+1);
                                            Preferences.debug("Excitation wavelength in nanometers = " +
                                            		excitationWavelength[channelsFound] + "\n", Preferences.DEBUG_FILEIO);		
                                        } // if ((excitationWavelengthStart >= 0) && 
                                        emissionWavelengthStart = channel.indexOf("<EmissionWavelength>");
                                        emissionWavelengthEnd = channel.indexOf("</EmissionWavelength>");
                                        if ((emissionWavelengthStart >= 0) && 
                                        		(emissionWavelengthEnd > emissionWavelengthStart)) {
                                            emissionWavelength[channelsFound] =
                                            		channel.substring(emissionWavelengthStart, emissionWavelengthEnd);
                                            emissionWavelengthStart = emissionWavelength[channelsFound].indexOf(">");
                                            emissionWavelength[channelsFound] = 
                                            		emissionWavelength[channelsFound].substring(emissionWavelengthStart+1);
                                            Preferences.debug("Emission wavelength in nanometers = " + 
                                            		emissionWavelength[channelsFound] + "\n", Preferences.DEBUG_FILEIO);		
                                        } // if ((emissionWavelengthStart >= 0) && 
                                        dyeIDStart = channel.indexOf("<DyeId>");
                                        dyeIDEnd = channel.indexOf("</DyeId>");
                                        if ((dyeIDStart >= 0) && (dyeIDEnd > dyeIDStart)) {
	                                        dyeID[channelsFound] = channel.substring(dyeIDStart, dyeIDEnd);
	                                        dyeIDStart = dyeID[channelsFound].indexOf(">");
	                                        dyeID[channelsFound] = dyeID[channelsFound].substring(dyeIDStart+1);
	                                        Preferences.debug("Dye ID = " + dyeID[channelsFound] + "\n", Preferences.DEBUG_FILEIO);
                                        } // if ((dyeIDStart >= 0) && (dyeIDEnd > dyeIDStart))
                                        dyeDatabaseIDStart = channel.indexOf("<DyeDatabaseId>");
                                        dyeDatabaseIDEnd = channel.indexOf("</DyeDatabaseId>");
                                        if ((dyeDatabaseIDStart >= 0) && (dyeDatabaseIDEnd > dyeDatabaseIDStart)) {
	                                        dyeDatabaseID[channelsFound] = channel.substring(dyeDatabaseIDStart, dyeDatabaseIDEnd);
	                                        dyeDatabaseIDStart = dyeDatabaseID[channelsFound].indexOf(">");
	                                        dyeDatabaseID[channelsFound] = dyeDatabaseID[channelsFound].substring(dyeDatabaseIDStart+1);
	                                        Preferences.debug("Dye database ID = " + dyeDatabaseID[channelsFound] + "\n",
	                                        		Preferences.DEBUG_FILEIO);
                                        } // if ((dyeDatabaseIDStart >= 0) && (dyeDatabaseIDEnd > dyeDatabaseIDStart))
                                        pinholeSizeStart = channel.indexOf("<PinholeSize>");
                                        pinholeSizeEnd = channel.indexOf("</PinholeSize>");
                                        if ((pinholeSizeStart >= 0) && (pinholeSizeEnd > pinholeSizeStart)) {
                                            pinholeSize[channelsFound] = channel.substring(pinholeSizeStart, pinholeSizeEnd);
                                            pinholeSizeStart = pinholeSize[channelsFound].indexOf(">");
                                            pinholeSize[channelsFound] = pinholeSize[channelsFound].substring(pinholeSizeStart+1);
                                            Preferences.debug("Pinhole size in micrometers = " + pinholeSize[channelsFound] + "\n",
                                            		Preferences.DEBUG_FILEIO);
                                        } // if ((pinholeSizeStart >= 0) && (pinholeSizeEnd > pinholeSizeStart))
                                        pinholeSizeAiryStart = channel.indexOf("<PinholeSizeAiry>");
                                        pinholeSizeAiryEnd = channel.indexOf("</PinholeSizeAiry>");
                                        if ((pinholeSizeAiryStart >= 0) && (pinholeSizeAiryEnd > pinholeSizeAiryStart)) {
                                            pinholeSizeAiry[channelsFound] = channel.substring(pinholeSizeAiryStart, pinholeSizeAiryEnd);
                                            pinholeSizeAiryStart = pinholeSizeAiry[channelsFound].indexOf(">");
                                            pinholeSizeAiry[channelsFound] = 
                                            		pinholeSizeAiry[channelsFound].substring(pinholeSizeAiryStart+1);
                                            Preferences.debug("Pinhole size in airy disc units = " + pinholeSizeAiry[channelsFound] + "\n",
                                            		Preferences.DEBUG_FILEIO);
                                        } // if ((pinholeSizeAiryStart >= 0) && (pinholeSizeAiryEnd > pinholeSizeAiryStart)) 
                                        pinholeGeometryStart = channel.indexOf("<PinholeGeometry>");
                                        pinholeGeometryEnd = channel.indexOf("</PinholeGeometry>");
                                        if ((pinholeGeometryStart >= 0) && (pinholeGeometryEnd > pinholeGeometryStart)) {
                                            pinholeGeometry[channelsFound] = channel.substring(pinholeGeometryStart, pinholeGeometryEnd);
                                            pinholeGeometryStart = pinholeGeometry[channelsFound].indexOf(">");
                                            pinholeGeometry[channelsFound] = 
                                            		pinholeGeometry[channelsFound].substring(pinholeGeometryStart+1);
                                            Preferences.debug("Pinhole geometry = " + pinholeGeometry[channelsFound] + "\n",
                                            		Preferences.DEBUG_FILEIO);
                                        } // if ((pinholeGeometryStart >= 0) && (pinholeGeometryEnd > pinholeGeometryStart))
                                        fluorStart = channel.indexOf("<Fluor>");
                                        fluorEnd = channel.indexOf("</Fluor>");
                                        if ((fluorStart >= 0) && (fluorEnd > fluorStart)) {
                                            fluor[channelsFound] = channel.substring(fluorStart, fluorEnd);
                                            fluorStart = fluor[channelsFound].indexOf(">");
                                            fluor[channelsFound] = 
                                            		fluor[channelsFound].substring(fluorStart+1);
                                            Preferences.debug("Fluorophore name = " + fluor[channelsFound] + "\n",
                                            		Preferences.DEBUG_FILEIO);
                                        } // if ((fluorStart >= 0) && (fluorEnd > fluorStart))
                                        NDFilterStart = channel.indexOf("<NDFilter>");
                                        NDFilterEnd = channel.indexOf("</NDFilter>");
                                        if ((NDFilterStart >= 0) && (NDFilterEnd > NDFilterStart)) {
                                            NDFilter[channelsFound] = channel.substring(NDFilterStart, NDFilterEnd);
                                            NDFilterStart = NDFilter[channelsFound].indexOf(">");
                                            NDFilter[channelsFound] = 
                                            		NDFilter[channelsFound].substring(NDFilterStart+1);
                                            Preferences.debug("Neutral density filter optical density = " + NDFilter[channelsFound] + "\n",
                                            		Preferences.DEBUG_FILEIO);
                                        } // if ((NDFilterStart >= 0) && (NDFilterEnd > NDFilterStart)) 
                                        pockelCellSettingStart = channel.indexOf("<PockelCellSetting>");
                                        pockelCellSettingEnd = channel.indexOf("</PockelCellSetting>");
                                        if ((pockelCellSettingStart >= 0) && (pockelCellSettingEnd > pockelCellSettingStart)) {
                                            pockelCellSetting[channelsFound] = 
                                            		channel.substring(pockelCellSettingStart, pockelCellSettingEnd);
                                            pockelCellSettingStart = pockelCellSetting[channelsFound].indexOf(">");
                                            pockelCellSetting[channelsFound] = 
                                            		pockelCellSetting[channelsFound].substring(pockelCellSettingStart+1);
                                            Preferences.debug("Pockel cell setting = " + pockelCellSetting[channelsFound] + "\n",
                                            		Preferences.DEBUG_FILEIO);
                                        } // if ((pockelCellSettingStart >= 0) && (pockelCellSettingEnd > pockelCellSettingStart))
                                        originalColorStart = channel.indexOf("<Color>");
                                        originalColorEnd = channel.indexOf("</Color>");
                                        if ((originalColorStart >= 0) && (originalColorEnd > originalColorStart)) {
                                            originalColor[channelsFound] = channel.substring(originalColorStart, originalColorEnd);
                                            originalColorStart = originalColor[channelsFound].indexOf(">");
                                            originalColor[channelsFound] = 
                                            		originalColor[channelsFound].substring(originalColorStart+1);
                                            Preferences.debug("Original color = " + originalColor[channelsFound] + "\n",
                                            		Preferences.DEBUG_FILEIO);
                                        } // if ((originalColorStart >= 0) && (originalColorEnd > originalColorStart))
                                        exposureTimeStart = channel.indexOf("<ExposureTime>");
                                        exposureTimeEnd = channel.indexOf("</ExposureTime>");
                                        if ((exposureTimeStart >= 0) && (exposureTimeEnd > exposureTimeStart)) {
                                            exposureTime[channelsFound] = channel.substring(exposureTimeStart, exposureTimeEnd);
                                            exposureTimeStart = exposureTime[channelsFound].indexOf(">");
                                            exposureTime[channelsFound] = 
                                            		exposureTime[channelsFound].substring(exposureTimeStart+1);
                                            Preferences.debug("Exposure time in nanoseconds = " + exposureTime[channelsFound] + "\n",
                                            		Preferences.DEBUG_FILEIO);
                                        } // if ((exposureTimeStart >= 0) && (exposureTimeEnd > exposureTimeStart))
                                        sectionThicknessStart = channel.indexOf("<SectionThickness>");
                                        sectionThicknessEnd = channel.indexOf("</SectionThickness>");
                                        if ((sectionThicknessStart >= 0) && (sectionThicknessEnd > sectionThicknessStart)) {
                                            sectionThickness[channelsFound] = 
                                            		channel.substring(sectionThicknessStart, sectionThicknessEnd);
                                            sectionThicknessStart = sectionThickness[channelsFound].indexOf(">");
                                            sectionThickness[channelsFound] = 
                                            		sectionThickness[channelsFound].substring(sectionThicknessStart+1);
                                            Preferences.debug("Section thickness in micrometers = " + sectionThickness[channelsFound]
                                            		+ "\n", Preferences.DEBUG_FILEIO);
                                        } // if ((sectionThicknessStart >= 0) && (sectionThicknessEnd > sectionThicknessStart))
                                        reflectorStart = channel.indexOf("<Reflector>");
                                        reflectorEnd = channel.indexOf("</Reflector>");
                                        if ((reflectorStart >= 0) && (reflectorEnd > reflectorStart)) {
                                            reflector[channelsFound] = channel.substring(reflectorStart, reflectorEnd);
                                            reflectorStart = reflector[channelsFound].indexOf(">");
                                            reflector[channelsFound] = 
                                            		reflector[channelsFound].substring(reflectorStart+1);
                                            Preferences.debug("Reflector = " + reflector[channelsFound] + "\n",
                                            		Preferences.DEBUG_FILEIO);
                                        } // if ((reflectorStart >= 0) && (reflectorEnd > reflectorStart))
                                        condenserContrastStart = channel.indexOf("<CondenserContrast>");
                                        condenserContrastEnd = channel.indexOf("</CondenserContrast>");
                                        if ((condenserContrastStart >= 0) && (condenserContrastEnd > condenserContrastStart)) {
                                            condenserContrast[channelsFound] = 
                                            		channel.substring(condenserContrastStart, condenserContrastEnd);
                                            condenserContrastStart = condenserContrast[channelsFound].indexOf(">");
                                            condenserContrast[channelsFound] = 
                                            		condenserContrast[channelsFound].substring(condenserContrastStart+1);
                                            Preferences.debug("Condenser contrast = " + condenserContrast[channelsFound] + "\n",
                                            		Preferences.DEBUG_FILEIO);
                                        } // if ((condenserContrastStart >= 0) && (condenserContrastEnd > condenserContrastStart))
                                        NACondenserStart = channel.indexOf("<NACondenser>");
                                        NACondenserEnd = channel.indexOf("</NACondenser>");
                                        if ((NACondenserStart >= 0) && (NACondenserEnd > NACondenserStart)) {
                                            NACondenser[channelsFound] = channel.substring(NACondenserStart, NACondenserEnd);
                                            NACondenserStart = NACondenser[channelsFound].indexOf(">");
                                            NACondenser[channelsFound] = 
                                            		NACondenser[channelsFound].substring(NACondenserStart+1);
                                            Preferences.debug("NA condenser = " + NACondenser[channelsFound] + "\n",
                                            		Preferences.DEBUG_FILEIO);
                                        } // if ((NACondenserStart >= 0) && (NACondenserEnd > NACondenserStart))
                                        ratioStart = channel.indexOf("<Ratio>");
                                        ratioEnd = channel.indexOf("</Ratio>");
                                        if ((ratioStart >= 0) && (ratioEnd > ratioStart)) {
                                            ratio[channelsFound] = channel.substring(ratioStart, ratioEnd);
                                            ratioStart = ratio[channelsFound].indexOf(">");
                                            ratio[channelsFound] = 
                                            		ratio[channelsFound].substring(ratioStart+1);
                                            Preferences.debug("Ratio between two active channels = " + ratio[channelsFound] + "\n",
                                            		Preferences.DEBUG_FILEIO);
                                        } // if ((ratioStart >= 0) && (ratioEnd > ratioStart))
                                        detectorSettingsStart = channel.indexOf("<DetectorSettings>");
                                        detectorSettingsEnd = channel.indexOf("</DetectorSettings>");
                                        if ((detectorSettingsStart >= 0) && (detectorSettingsEnd > detectorSettingsStart)) {
                                            detectorSettings = channel.substring(detectorSettingsStart, detectorSettingsEnd);
                                            detectorSettingsStart = detectorSettings.indexOf(">");
                                            detectorSettings =  detectorSettings.substring(detectorSettingsStart+1);
                                            detectorBinningStart = detectorSettings.indexOf("<Binning>");
                                            detectorBinningEnd = detectorSettings.indexOf("</Binning>");
                                            if ((detectorBinningStart >= 0) && (detectorBinningEnd > detectorBinningStart)) {
                                                detectorBinning[channelsFound] = 
                                                		detectorSettings.substring(detectorBinningStart, detectorBinningEnd);
                                                detectorBinningStart = detectorBinning[channelsFound].indexOf(">");
                                                detectorBinning[channelsFound] = 
                                                		detectorBinning[channelsFound].substring(detectorBinningStart+1);
                                                Preferences.debug("Detector binning = " + detectorBinning[channelsFound] + "\n",
                                                		Preferences.DEBUG_FILEIO);
                                            } // if ((detectorBinningStart >= 0) && (detectorBinningEnd > detectorBinningStart))
                                            detectorGainStart = detectorSettings.indexOf("<Gain>");
                                            detectorGainEnd = detectorSettings.indexOf("</Gain>");
                                            if ((detectorGainStart >= 0) && (detectorGainEnd > detectorGainStart)) {
                                                detectorGain[channelsFound] = 
                                                		detectorSettings.substring(detectorGainStart, detectorGainEnd);
                                                detectorGainStart = detectorGain[channelsFound].indexOf(">");
                                                detectorGain[channelsFound] = 
                                                		detectorGain[channelsFound].substring(detectorGainStart+1);
                                                Preferences.debug("Detector gain = " + detectorGain[channelsFound] + "\n",
                                                		Preferences.DEBUG_FILEIO);
                                            } // if ((detectorGainStart >= 0) && (detectorGainEnd > detectorGainStart))
                                            detectorDigitalGainStart = detectorSettings.indexOf("<DigitalGain>");
                                            detectorDigitalGainEnd = detectorSettings.indexOf("</DigitalGain>");
                                            if ((detectorDigitalGainStart >= 0) && (detectorDigitalGainEnd > detectorDigitalGainStart)) {
                                                detectorDigitalGain[channelsFound] = 
                                                		detectorSettings.substring(detectorDigitalGainStart, detectorDigitalGainEnd);
                                                detectorDigitalGainStart = detectorDigitalGain[channelsFound].indexOf(">");
                                                detectorDigitalGain[channelsFound] = 
                                                		detectorDigitalGain[channelsFound].substring(detectorDigitalGainStart+1);
                                                Preferences.debug("Detector digital gain = " + detectorDigitalGain[channelsFound] + "\n",
                                                		Preferences.DEBUG_FILEIO);
                                            } // if ((detectorDigitalGainStart >= 0) && (detectorDigitalGainEnd > detectorDigitalGainStart))
                                            detectorOffsetStart = detectorSettings.indexOf("<Offset>");
                                            detectorOffsetEnd = detectorSettings.indexOf("</Offset>");
                                            if ((detectorOffsetStart >= 0) && (detectorOffsetEnd > detectorOffsetStart)) {
                                                detectorOffset[channelsFound] = 
                                                		detectorSettings.substring(detectorOffsetStart, detectorOffsetEnd);
                                                detectorOffsetStart = detectorOffset[channelsFound].indexOf(">");
                                                detectorOffset[channelsFound] = 
                                                		detectorOffset[channelsFound].substring(detectorOffsetStart+1);
                                                Preferences.debug("Detector gain offset = " + detectorOffset[channelsFound] + "\n",
                                                		Preferences.DEBUG_FILEIO);
                                            } // if ((detectorOffsetStart >= 0) && (detectorOffsetEnd > detectorOffsetStart))
                                            detectorEMGainStart = detectorSettings.indexOf("<EMGain>");
                                            detectorEMGainEnd = detectorSettings.indexOf("</EMGain>");
                                            if ((detectorEMGainStart >= 0) && (detectorEMGainEnd > detectorEMGainStart)) {
                                                detectorEMGain[channelsFound] = 
                                                		detectorSettings.substring(detectorEMGainStart, detectorEMGainEnd);
                                                detectorEMGainStart = detectorEMGain[channelsFound].indexOf(">");
                                                detectorEMGain[channelsFound] = 
                                                		detectorEMGain[channelsFound].substring(detectorEMGainStart+1);
                                                Preferences.debug("Detector EM gain = " + detectorEMGain[channelsFound] + "\n",
                                                		Preferences.DEBUG_FILEIO);
                                            } // if ((detectorEMGainStart >= 0) && (detectorEMGainEnd > detectorEMGainStart))
                                            detectorVoltageStart = detectorSettings.indexOf("<Voltage>");
                                            detectorVoltageEnd = detectorSettings.indexOf("</Voltage>");
                                            if ((detectorVoltageStart >= 0) && (detectorVoltageEnd > detectorVoltageStart)) {
                                                detectorVoltage[channelsFound] = 
                                                		detectorSettings.substring(detectorVoltageStart, detectorVoltageEnd);
                                                detectorVoltageStart = detectorVoltage[channelsFound].indexOf(">");
                                                detectorVoltage[channelsFound] = 
                                                		detectorVoltage[channelsFound].substring(detectorVoltageStart+1);
                                                Preferences.debug("Detector voltage in volts = " + detectorVoltage[channelsFound] + "\n",
                                                		Preferences.DEBUG_FILEIO);
                                            } // if ((detectorVoltageStart >= 0) && (detectorVoltageEnd > detectorVoltageStart))
                                            detectorReadOutRateStart = detectorSettings.indexOf("<ReadOutRate>");
                                            detectorReadOutRateEnd = detectorSettings.indexOf("</ReadOutRate>");
                                            if ((detectorReadOutRateStart >= 0) && (detectorReadOutRateEnd > detectorReadOutRateStart)) {
                                                detectorReadOutRate[channelsFound] = 
                                                		detectorSettings.substring(detectorReadOutRateStart, detectorReadOutRateEnd);
                                                detectorReadOutRateStart = detectorReadOutRate[channelsFound].indexOf(">");
                                                detectorReadOutRate[channelsFound] = 
                                                		detectorReadOutRate[channelsFound].substring(detectorReadOutRateStart+1);
                                                Preferences.debug("Detector read out rate in megahertz = " + 
                                                		detectorReadOutRate[channelsFound] + "\n", Preferences.DEBUG_FILEIO);
                                            } // if ((detectorReadOutRateStart >= 0) && (detectorReadOutRateEnd > detectorReadOutRateStart))
                                            detectorUseBrightnessContrastCorrectionStart = 
                                            		detectorSettings.indexOf("<UseBrightnessContrastCorrection>");
                                            detectorUseBrightnessContrastCorrectionEnd = 
                                            		detectorSettings.indexOf("</UseBrightnessContrastCorrection>");
                                            if ((detectorUseBrightnessContrastCorrectionStart >= 0) &&
                                            		(detectorUseBrightnessContrastCorrectionEnd > 
                                            		detectorUseBrightnessContrastCorrectionStart)) {
                                                detectorUseBrightnessContrastCorrection[channelsFound] = 
                                                		detectorSettings.substring(detectorUseBrightnessContrastCorrectionStart, 
                                                				detectorUseBrightnessContrastCorrectionEnd);
                                                detectorUseBrightnessContrastCorrectionStart = 
                                                		detectorUseBrightnessContrastCorrection[channelsFound].indexOf(">");
                                                detectorUseBrightnessContrastCorrection[channelsFound] = 
                                                    detectorUseBrightnessContrastCorrection[channelsFound].substring(
                                                    		detectorUseBrightnessContrastCorrectionStart+1);
                                                Preferences.debug("Detector use brightness contrast correction = " + 
                                                    detectorUseBrightnessContrastCorrection[channelsFound] + "\n",
                                                		Preferences.DEBUG_FILEIO);
                                            } // if ((detectorUseBrightnessContrastCorrectionStart >= 0) && 
                                        } // if ((detectorSettingsStart >= 0) && (detectorSettingsEnd > detectorSettingsStart))
                                        lightSourceSettingsStart = channel.indexOf("<LightSourceSettings>");
                                        lightSourceSettingsEnd = channel.indexOf("</LightSourceSettings>");
                                        if ((lightSourceSettingsStart >= 0) && (lightSourceSettingsEnd > lightSourceSettingsStart)) {
                                            lightSourceSettings = channel.substring(lightSourceSettingsStart, lightSourceSettingsEnd);
                                            lightSourceSettingsStart = lightSourceSettings.indexOf(">");
                                            lightSourceSettings =  lightSourceSettings.substring(lightSourceSettingsStart+1);
                                            lightSourceWavelengthStart = lightSourceSettings.indexOf("<Wavelength>");
                                            lightSourceWavelengthEnd = lightSourceSettings.indexOf("</Wavelength>");
                                            if ((lightSourceWavelengthStart >= 0) && 
                                            		(lightSourceWavelengthEnd > lightSourceWavelengthStart)) {
                                                lightSourceWavelength[channelsFound] = 
                                                		lightSourceSettings.substring(lightSourceWavelengthStart, lightSourceWavelengthEnd);
                                                lightSourceWavelengthStart = lightSourceWavelength[channelsFound].indexOf(">");
                                                lightSourceWavelength[channelsFound] = 
                                                		lightSourceWavelength[channelsFound].substring(lightSourceWavelengthStart+1);
                                                Preferences.debug("Light source wavelength in nanometers = " + lightSourceWavelength[channelsFound] +
                                                		"\n", Preferences.DEBUG_FILEIO);
                                            } // if ((lightSourceWavelengthStart >= 0) && (lightSourceWavelengthEnd > 
                                            lightSourceAttenuationStart = lightSourceSettings.indexOf("<Attenuation>");
                                            lightSourceAttenuationEnd = lightSourceSettings.indexOf("</Attenuation>");
                                            if ((lightSourceAttenuationStart >= 0) && 
                                            		(lightSourceAttenuationEnd > lightSourceAttenuationStart)) {
                                                lightSourceAttenuation[channelsFound] = 
                                                		lightSourceSettings.substring(
                                                				lightSourceAttenuationStart, lightSourceAttenuationEnd);
                                                lightSourceAttenuationStart = lightSourceAttenuation[channelsFound].indexOf(">");
                                                lightSourceAttenuation[channelsFound] = 
                                                		lightSourceAttenuation[channelsFound].substring(lightSourceAttenuationStart+1);
                                                Preferences.debug("Light source attenuation = " + lightSourceAttenuation[channelsFound] +
                                                		"\n", Preferences.DEBUG_FILEIO);
                                            } // if ((lightSourceAttenuationStart >= 0) && (lightSourceAttenuationEnd > 
                                            lightSourceIntensityStart = lightSourceSettings.indexOf("<Intensity>");
                                            lightSourceIntensityEnd = lightSourceSettings.indexOf("</Intensity>");
                                            if ((lightSourceIntensityStart >= 0) && 
                                            		(lightSourceIntensityEnd > lightSourceIntensityStart)) {
                                                lightSourceIntensity[channelsFound] = 
                                                		lightSourceSettings.substring(lightSourceIntensityStart, lightSourceIntensityEnd);
                                                lightSourceIntensityStart = lightSourceIntensity[channelsFound].indexOf(">");
                                                lightSourceIntensity[channelsFound] = 
                                                		lightSourceIntensity[channelsFound].substring(lightSourceIntensityStart+1);
                                                Preferences.debug("Light source intensity = " + lightSourceIntensity[channelsFound] +
                                                		"\n", Preferences.DEBUG_FILEIO);
                                            } // if ((lightSourceIntensityStart >= 0) && (lightSourceIntensityEnd > 
                                        } // if ((lightSourceSettingsStart >= 0) && (lightSourceSettingsEnd > lightSourceSettingsStart))
                                        channels = channels.substring(channelEnd + 10);
                                        if (channels == null) {
                                        	break;
                                        }
                                        if (firstChannelFind) {
                                            channelStart = channels.indexOf("<Channel Id=\"Channel:");
                                        }
                                        else {
                                        	channelStart = channels.indexOf("Channel Id=\"");	
                                        }
                                        channelEnd = channels.indexOf("</Channel>");
                                    } // while ((channelStart >= 0) && (channelEnd > channelStart))
                                    fileInfo.setChannelsFound(channelsFound+1);
                                    // Use clone so not overwritten by channelID and channelName in DisplaySetings
                                    fileInfo.setChannelID(channelID.clone());
                                    fileInfo.setChannelName(channelName.clone());
                                    fileInfo.setAcquisitionMode(acquisitionMode);
                                    fileInfo.setIlluminationType(illuminationType);
                                    fileInfo.setContrastMethod(contrastMethod);
                                    fileInfo.setIlluminationWavelength(illuminationWavelength);
                                    fileInfo.setDetectionWavelength(detectionWavelength);
                                    fileInfo.setExcitationWavelength(excitationWavelength);
                                    fileInfo.setEmissionWavelength(emissionWavelength);
                                    fileInfo.setDyeID(dyeID);
                                    fileInfo.setDyeDatabaseID(dyeDatabaseID);
                                    fileInfo.setPinholeSize(pinholeSize);
                                    fileInfo.setPinholeSizeAiry(pinholeSizeAiry);
                                    fileInfo.setPinholeGeometry(pinholeGeometry);
                                    fileInfo.setFluor(fluor);
                                    fileInfo.setNDFilter(NDFilter);
                                    fileInfo.setPockelCellSetting(pockelCellSetting);
                                    fileInfo.setColor(originalColor);
                                    fileInfo.setExposureTime(exposureTime);
                                    fileInfo.setSectionThickness(sectionThickness);
                                    fileInfo.setReflector(reflector);
                                    fileInfo.setCondenserContrast(condenserContrast);
                                    fileInfo.setNACondenser(NACondenser);
                                    fileInfo.setRatio(ratio);
                                    fileInfo.setDetectorBinning(detectorBinning);
                                    fileInfo.setDetectorGain(detectorGain);
                                    fileInfo.setDetectorDigitalGain(detectorDigitalGain);
                                    fileInfo.setDetectorOffset(detectorOffset);
                                    fileInfo.setDetectorEMGain(detectorEMGain);
                                    fileInfo.setDetectorVoltage(detectorVoltage);
                                    fileInfo.setDetectorReadOutRate(detectorReadOutRate);
                                    fileInfo.setDetectorUseBrightnessContrastCorrection(detectorUseBrightnessContrastCorrection);
                                    fileInfo.setLightSourceWavelength(lightSourceWavelength);
                                    fileInfo.setLightSourceAttenuation(lightSourceAttenuation);
                                    fileInfo.setLightSourceIntensity(lightSourceIntensity);
                                } // if ((channelsStart >= 0) && (channelsEnd > channelsStart))
                            } // if ((dimensionsStart >= 0) && (dimensionsEnd > dimensionsStart))
                        } // if ((imageStart >= 0) && (imageEnd > imageStart))
                    } // if ((informationStart >= 0) && (informationEnd > informationStart))
                    displaySettingStart = xmlData.indexOf("<DisplaySetting>");
                    displaySettingEnd = xmlData.indexOf("</DisplaySetting>");
                    if ((displaySettingStart >= 0) && (displaySettingEnd > displaySettingStart)) {
                        displaySetting = xmlData.substring(displaySettingStart, displaySettingEnd);
                        displaySettingStart = displaySetting.indexOf(">");
                        displaySetting = displaySetting.substring(displaySettingStart+1);
                        channelsStart = displaySetting.indexOf("<Channels>");
                        channelsEnd = displaySetting.indexOf("</Channels>");
                        if ((channelsStart >= 0) && (channelsEnd > channelsStart)) {
                            channels = displaySetting.substring(channelsStart, channelsEnd);
                            firstChannelFind = true;
                            channelsStart = channels.indexOf(">");
                            channels = channels.substring(channelsStart+1);
                            //Preferences.debug("channels = " + channels + "\n", Preferences.DEBUG_FILEIO);
                            channelStart = channels.indexOf("<Channel Id=\"Channel:");
                            channelEnd = channels.indexOf("</Channel>");
                            if (channelStart == -1) {
                            	channelStart = channels.indexOf("Channel Id=\"");
                            	firstChannelFind = false;
                            }
                            channelsFound = -1;
                            while ((channelStart >= 0) && (channelEnd > channelStart)) {
                            	channelsFound++;
                                channel = channels.substring(channelStart, channelEnd);
                                //Preferences.debug("In DisplaySettings channel = " + channel + "\n", Preferences.DEBUG_FILEIO);
                                if (firstChannelFind) {
                                    channelIDStart = channel.indexOf(":");
                                    channelIDEnd = channel.indexOf("\"", channelIDStart+1);
                                	channelNameStart = channel.indexOf("Name=\"");
                                	channelNameStart = channel.indexOf("\"", channelNameStart+1);
                                	channelNameEnd = channel.indexOf("\"", channelNameStart+1);
                                }
                                else {
                                	channelIDStart = channel.indexOf("\"");
                                	channelIDEnd = channel.indexOf("\"", channelStart+1);
                                	channelNameStart = channel.indexOf("Name=\"");
                                	channelNameStart = channel.indexOf("\"", channelNameStart+1);
                                	channelNameEnd = channel.indexOf("\"", channelNameStart+1);
                                }
                                if ((channelIDStart >= 0) && (channelIDEnd > channelIDStart)) {
                                    channelID[channelsFound] = channel.substring(channelIDStart+1, channelIDEnd);
                                    Preferences.debug("In DisplaySettings Channel ID = " + channelID[channelsFound] + "\n",
                                    		Preferences.DEBUG_FILEIO);
                                    if ((fileInfo.getChannelID() != null) && (fileInfo.getChannelID()[channelsFound] != null)  &&
                                    		fileInfo.getChannelID()[channelsFound].equals(channelID[channelsFound])) {
                                        channelIDFoundBefore[channelsFound] = true;	
                                    }
                                    else {
                                    	channelIDFoundBefore[channelsFound] = false;
                                    }
                                } // if ((channelIDStart >= 0) && (channelIDEnd > channelIDStart))
                                if ((channelNameStart >= 0) && (channelNameEnd > channelNameStart)) {
                                    channelName[channelsFound] = channel.substring(channelNameStart+1, channelNameEnd);
                                    Preferences.debug("In DisplaySettings Channel name = " + channelName[channelsFound] + "\n", 
                                    		Preferences.DEBUG_FILEIO);
                                } // if ((channelNameStart >= 0) && (channelNameEnd > channelNameStart))
                                colorStart = channel.indexOf("<Color>");
                                colorEnd = channel.indexOf("</Color>");
                                if ((colorStart >= 0) && (colorEnd > colorStart)) {
                                    color = channel.substring(colorStart,colorEnd);
                                    colorStart = color.indexOf(">");
                                    color = color.substring(colorStart+1);
                                    //Preferences.debug("color = " + color + "\n", Preferences.DEBUG_FILEIO);
                                    if ((color.equals("#FF0000")) || (color.equals("#FFFF0000"))) {
                                    	// Red
                                    	channelColor[channelsFound] = 1;
                                    }
                                    else if ((color.equals("#00FF00")) || (color.equals("#FF00FF00"))) {
                                    	// Green
                                    	channelColor[channelsFound] = 2;
                                    }
                                    else if ((color.equals("#0000FF")) || (color.equals("#FF0000FF"))) {
                                    	// Blue
                                    	channelColor[channelsFound] = 3;
                                    }
                                    else if ((color.equals("#FFFF00")) || (color.equals("#FFFFFF00"))) {
                                    	// Red and green - just do red
                                    	channelColor[channelsFound] = 1;
                                    }
                                } // if ((colorStart >= 0) && (colorEnd > colorStart))
                                lowStart = channel.indexOf("<Low>");
                                lowEnd = channel.indexOf("</Low>");
                                if ((lowStart >= 0) && (lowEnd > lowStart)) {
                                    low[channelsFound] = channel.substring(lowStart,lowEnd);
                                    lowStart = low[channelsFound].indexOf(">");
                                    low[channelsFound] = low[channelsFound].substring(lowStart+1);
                                    Preferences.debug("Normalized low(=black) value of the mapping range = " +
                                        low[channelsFound] + "\n", Preferences.DEBUG_FILEIO);
                                } //  if ((lowStart >= 0) && (lowEnd > lowStart))
                                highStart = channel.indexOf("<High>");
                                highEnd = channel.indexOf("</High>");
                                if ((highStart >= 0) && (highEnd > highStart)) {
                                    high[channelsFound] = channel.substring(highStart,highEnd);
                                    highStart = high[channelsFound].indexOf(">");
                                    high[channelsFound] = high[channelsFound].substring(highStart+1);
                                    Preferences.debug("Normalized high(=white) value of the mapping range = " +
                                        high[channelsFound] + "\n", Preferences.DEBUG_FILEIO);
                                } //  if ((highStart >= 0) && (highEnd > highStart))
                                gammaStart = channel.indexOf("<Gamma>");
                                gammaEnd = channel.indexOf("</Gamma>");
                                if ((gammaStart >= 0) && (gammaEnd > gammaStart)) {
                                    gamma[channelsFound] = channel.substring(gammaStart,gammaEnd);
                                    gammaStart = gamma[channelsFound].indexOf(">");
                                    gamma[channelsFound] = gamma[channelsFound].substring(gammaStart+1);
                                    Preferences.debug("Gamma value to be applied to the mapping range = " +
                                        gamma[channelsFound] + "\n", Preferences.DEBUG_FILEIO);
                                } //  if ((gammaStart >= 0) && (gammaEnd > gammaStart))
                                modeStart = channel.indexOf("<Mode>");
                                modeEnd = channel.indexOf("</Mode>");
                                if ((modeStart >= 0) && (modeEnd > modeStart)) {
                                    mode[channelsFound] = channel.substring(modeStart,modeEnd);
                                    modeStart = mode[channelsFound].indexOf(">");
                                    mode[channelsFound] = mode[channelsFound].substring(modeStart+1);
                                    Preferences.debug("Mode = " + mode[channelsFound] + "\n", Preferences.DEBUG_FILEIO);
                                } //  if ((modeStart >= 0) && (modeEnd > modeStart))
                                pointsStart = channel.indexOf("<Points>");
                                pointsEnd = channel.indexOf("</Points>");
                                if ((pointsStart >= 0) && (pointsEnd > pointsStart)) {
                                    points[channelsFound] = channel.substring(pointsStart,pointsEnd);
                                    pointsStart = points[channelsFound].indexOf(">");
                                    points[channelsFound] = points[channelsFound].substring(pointsStart+1);
                                    Preferences.debug("Points = " + points[channelsFound] + "\n", Preferences.DEBUG_FILEIO);
                                } //  if ((pointsStart >= 0) && (pointsEnd > pointsStart))
                                channelDescriptionStart = channel.indexOf("<Description>");
                                channelDescriptionEnd = channel.indexOf("</Description>");
                                if ((channelDescriptionStart >= 0) && (channelDescriptionEnd > channelDescriptionStart)) {
                                    channelDescription[channelsFound] = channel.substring(channelDescriptionStart,channelDescriptionEnd);
                                    channelDescriptionStart = channelDescription[channelsFound].indexOf(">");
                                    channelDescription[channelsFound] = 
                                    		channelDescription[channelsFound].substring(channelDescriptionStart+1);
                                    Preferences.debug("Channel description = " + channelDescription[channelsFound] + "\n", 
                                    		Preferences.DEBUG_FILEIO);
                                } //  if ((channelDescriptionStart >= 0) && (channelDescriptionEnd > channelDescriptionStart))
                                channelWeightStart = channel.indexOf("<ChannelWeight>");
                                channelWeightEnd = channel.indexOf("</ChannelWeight>");
                                if ((channelWeightStart >= 0) && (channelWeightEnd > channelWeightStart)) {
                                    channelWeight[channelsFound] = channel.substring(channelWeightStart,channelWeightEnd);
                                    channelWeightStart = channelWeight[channelsFound].indexOf(">");
                                    channelWeight[channelsFound] = channelWeight[channelsFound].substring(channelWeightStart+1);
                                    Preferences.debug("Channel weight (ratioamong all selected channels) = " +
                                        channelWeight[channelsFound] + "\n", Preferences.DEBUG_FILEIO);
                                } //  if ((channelWeightStart >= 0) && (channelWeightEnd > channelWeightStart))
                                channels = channels.substring(channelEnd + 10);
                                if (channels == null) {
                                	break;
                                }
                                if (firstChannelFind) {
                                    channelStart = channels.indexOf("<Channel Id=\"Channel:");
                                }
                                else {
                                	channelStart = channels.indexOf("Channel Id=\"");	
                                }
                                channelEnd = channels.indexOf("</Channel>");
                            } // while ((channelStart >= 0) && (channelEnd > channelStart))
                            channelIDsFoundBefore = true;
                            for (i = 0; i <= channelsFound; i++) {
                                if (channelIDFoundBefore[i] == false) {
                                	channelIDsFoundBefore = false;
                                }
                            }
                            if (channelIDsFoundBefore) {
                            	fileInfo.setLow(low);
                                fileInfo.setHigh(high);
                                fileInfo.setGamma(gamma);
                                fileInfo.setMode(mode);
                                fileInfo.setPoints(points);
                                fileInfo.setChannelDescription(channelDescription);
                                fileInfo.setChannelWeight(channelWeight);
                            }
                        } // if ((channelsStart >= 0) && (channelsEnd > channelsStart))
                    } // if ((displaySettingStart >= 0) && (displaySettingEnd > displaySettingStart))
                    Preferences.debug("XML data: \n", Preferences.DEBUG_FILEIO);
                    Preferences.debug(xmlData + "\n", Preferences.DEBUG_FILEIO);
                } // else if (charID.trim().equals("ZISRAWMETADATA"))
                else if (charID.trim().equals("ZISRAWATTDIR")) {
                    if (attachmentDirectoryPosition != 0) {
                        Preferences.debug("File header gave Attachment Directory Segment position as " + attachmentDirectoryPosition + "\n",
                                Preferences.DEBUG_FILEIO);
                    }
                    Preferences.debug("Actual Attachment Directory Segment position = " + position + "\n", Preferences.DEBUG_FILEIO);
                    entryCount = readInt(endianess);
                    Preferences.debug("Number of entries = " + entryCount + "\n", Preferences.DEBUG_FILEIO);
                    // Already found in individual ZISRAWATTACH
                    // Skip 252 reserved bytes
                    /*raFile.seek(position + 256L);
                    for (i = 0; i < entryCount; i++) {
                        startAttachmentEntry = raFile.getFilePointer();	
                        
                        if (i < entryCount - 1) {
                        	raFile.seek(startAttachmentEntry + 128L);
                        }
                    } // for (i = 0; i < entryCount; i++)*/
                } // else if (charID.trim().equals("ZISRAWATTDIR"))
                else if (charID.trim().equals("ZISRAWSUBBLOCK")) {
                    subBlockStart = raFile.getFilePointer();
                    metadataSize = readInt(endianess);
                    Preferences.debug("Size of the SubBlock metadata section = " + metadataSize + "\n", Preferences.DEBUG_FILEIO);
                    attachmentSize = readInt(endianess);
                    Preferences.debug("Size of the SubBlock optional attachment section = " + attachmentSize + "\n", Preferences.DEBUG_FILEIO);
                    dataSize = readLong(endianess);
                    Preferences.debug("Size of the SubBlock data section = " + dataSize + "\n", Preferences.DEBUG_FILEIO);
                    imageDataSize.add(dataSize);
                    // Reading Directory Entry DV
                    schemaType = getString(2);
                    if (schemaType.equals("DV")) {
                        Preferences.debug("Directory Entry DV Schema Type = DV as expected\n", Preferences.DEBUG_FILEIO);
                    } else {
                        Preferences.debug("Directory Entry DV Schema Type = " + schemaType + " instead of the expected DV\n", Preferences.DEBUG_FILEIO);
                        raFile.close();

                        throw new IOException("Directory Entry DV Schema Type = " + schemaType + " instead of the expected DV");
                    }
                    pixelType = readInt(endianess);
                    Preferences.debug("CZI pixelType = " + typeString[pixelType] + "\n", Preferences.DEBUG_FILEIO);
                    filePosition = readLong(endianess);
                    Preferences.debug("Seek offset of the referenced SubBlockSegment relative to the first byte of the file = " + filePosition + "\n",
                            Preferences.DEBUG_FILEIO);
                    // Reserved
                    filePart = readInt(endianess);
                    compression = readInt(endianess);
                    if (compression == Uncompressed) {
                        Preferences.debug("Compression = Uncompressed\n", Preferences.DEBUG_FILEIO);
                    } else if (compression == LZW) {
                        Preferences.debug("Compression = LZW\n", Preferences.DEBUG_FILEIO);
                    } else if (compression == JpgFile) {
                        Preferences.debug("Compression == JpgFile\n", Preferences.DEBUG_FILEIO);
                    } else if (compression == JpegXrFile) {
                        Preferences.debug("Compression == JpegXrFile\n", Preferences.DEBUG_FILEIO);
                    } else if ( (compression >= 100) && (compression <= 999)) {
                        Preferences.debug("Compression = Camera specific RAW data\n", Preferences.DEBUG_FILEIO);
                    } else if (compression >= 1000) {
                        Preferences.debug("Compression = System specific RAW data\n", Preferences.DEBUG_FILEIO);
                    } else {
                        Preferences.debug("Unrecognized compression\n", Preferences.DEBUG_FILEIO);
                    }
                    // [INTERNAL} Contains information for automatic image pyramids using SubBlocks of different
                    // resolution.
                    // Current values are: None = 0, SingleSublock = 1, MultiSubblock = 2.
                    pyramidType = raFile.readByte();
                    if (pyramidType == 0) {
                        Preferences.debug("PyramidType = None\n", Preferences.DEBUG_FILEIO);
                    } else if (pyramidType == 1) {
                        Preferences.debug("PyramidType = SingleSubblock\n", Preferences.DEBUG_FILEIO);
                    } else if (pyramidType == 2) {
                        Preferences.debug("PyramidType = MultiSubblock\n", Preferences.DEBUG_FILEIO);
                    } else {
                        Preferences.debug("Unrecognized pyramidType = " + pyramidType + "\n", Preferences.DEBUG_FILEIO);
                    }
                    // Reserved byte spare
                    raFile.readByte();
                    // Reserved byte[4] spare
                    readInt(endianess);
                    // Number of entries. Minimum is 1
                    dimensionCount = readInt(endianess);
                    if (dimensionCount < 1) {
                        Preferences.debug("dimensionCount = " + dimensionCount + " is less than the legal mininum of 1\n", Preferences.DEBUG_FILEIO);
                        raFile.close();
                        throw new IOException("dimensionCount = " + dimensionCount + " is less than the legal mininum of 1");
                    }
                    Preferences.debug("dimensionCount = " + dimensionCount + "\n", Preferences.DEBUG_FILEIO);
                    dimension = new String[dimensionCount];
                    start = new int[dimensionCount];
                    size = new int[dimensionCount];
                    startCoordinate = new float[dimensionCount];
                    storedSize = new int[dimensionCount];
                    index = 0;
                    for (i = 0; i < dimensionCount; i++) {
                        // Read 20 byte dimension entry
                        dimension[i] = getString(4).trim();
                        Preferences.debug("dimension[" + i + "] = " + dimension[i] + "\n", Preferences.DEBUG_FILEIO);
                        // Start position / index. May be less than 0.
                        start[i] = readInt(endianess);
                        Preferences.debug("Start index[" + i + "] = " + start[i] + "\n", Preferences.DEBUG_FILEIO);
                        // Size in units of pixels (logical size). Must be > 0.
                        size[i] = readInt(endianess);
                        if (size[i] < 1) {
                            Preferences.debug("size[" + i + "] = " + size[i] + " is less than the legal minuimum of 1\n", Preferences.DEBUG_FILEIO);
                            raFile.close();
                            throw new IOException("size[" + i + "] = " + size[i] + " is less than the legal minuimum of 1");
                        }
                        Preferences.debug("size[" + i + "] = " + size[i] + "\n", Preferences.DEBUG_FILEIO);
                        if ((dimension[i].equals("C")) && (size[i] == 1)) {
                        	imageColorStartIndex.add(start[i]);
                        }
                        else if ((dimension[i].equals("Z")) && (size[i] == 1)) {
                        	imageZStartIndex.add(start[i]);
                        }
                        else if ((dimension[i].equals("T")) && (size[i] == 1)) {
                        	imageTStartIndex.add(start[i]);
                        }
                        else if ((dimension[i].equals("H")) && (size[i] == 1)) {
                        	imageHStartIndex.add(start[i]);
                        }
                        else if ((dimension[i].equals("M")) && (size[i] == 1)) {
                        	imageMStartIndex.add(start[i]);
                        }
                        else if ((dimension[i].equals("S")) && (size[i] == 1)) {
                        	imageSStartIndex.add(start[i]);
                        }
                        startCoordinate[i] = readFloat(endianess);
                        Preferences.debug("startCoordinate[" + i + "] = " + startCoordinate[i] + "\n", Preferences.DEBUG_FILEIO);
                        if (size[i] > 1) {
                            if (imageDimension[index] == null) {
                                imageDimension[index] = new Vector<String>();
                            }
                            imageDimension[index].add(dimension[i]);
                            if (dimension[i].equals("X")) {
                            	xIndex = index;
                            }
                            else if (dimension[i].equals("Y")) {
                            	yIndex = index;
                            }
                            else if (dimension[i].equals("Z")) {
                            	zIndex = index;
                            }
                            else if (dimension[i].equals("T")) {
                            	tIndex = index;
                            }
                            else if (dimension[i].equals("T")) {
                            	tIndex = index;
                            }
                            else if (dimension[i].equals("H")) {
                            	hIndex = index;
                            }
                            if (imageSize[index] == null) {
                                imageSize[index] = new Vector<Integer>();
                            }
                            imageSize[index].add(size[i]);
                            if (imageStartCoordinate[index] == null) {
                                imageStartCoordinate[index] = new Vector<Float>();
                            }
                            imageStartCoordinate[index].add(startCoordinate[i]);
                            if (imageStartIndex[index] == null) {
                            	imageStartIndex[index] = new Vector<Integer>();
                            }
                            imageStartIndex[index].add(start[i]);
                            index++;
                        }
                        storedSize[i] = readInt(endianess);
                        // Stored size (if sub/supersampling) else 0)
                        if (storedSize[i] == 0) {
                            Preferences.debug("storedSize[" + i + "] = 0 as expected with no sub / supersampling\n", Preferences.DEBUG_FILEIO);
                        } else {
                            Preferences.debug("storedSize[" + i + "] = " + storedSize[i] + " with this sub /supersampling\n", Preferences.DEBUG_FILEIO);
                        }
                    } // for (i = 0; i < dimensionCount; i++)
                    location = raFile.getFilePointer();
                    if ( (location - subBlockStart) < 256) {
                        raFile.seek(subBlockStart + 256);
                    }
                    metaData = getString(metadataSize);
                    tagsStart = metaData.indexOf("<Tags>");
                    tagsEnd = metaData.indexOf("</Tags>");
                    if ((tagsStart >= 0) && (tagsEnd > tagsStart)) {
                    	tags = metaData.substring(tagsStart, tagsEnd);
                    	tagsStart = tags.indexOf(">");
                    	tags = tags.substring(tagsStart+1);
                    	focusPositionStart = tags.indexOf("<FocusPosition>");
                    	focusPositionEnd = tags.indexOf("</FocusPosition>");
                    	if ((focusPositionStart >= 0) && (focusPositionEnd > focusPositionStart)) {
                    	    focusPosition = tags.substring(focusPositionStart, focusPositionEnd);
                    	    focusPositionStart = focusPosition.indexOf(">");
                    	    focusPosition = focusPosition.substring(focusPositionStart+1);
                    	    Preferences.debug("Focus position in micrometers = " + focusPosition + "\n", Preferences.DEBUG_FILEIO);
                    	    fileInfo.setFocusPosition(focusPosition);
                    	} // if ((focusPositionStart >= 0) && (focusPositionEnd > focusPositionStart))
                    	acquisitionTimeStart = tags.indexOf("<AcquisitionTime>");
                    	acquisitionTimeEnd = tags.indexOf("</AcquisitionTime>");
                    	if ((acquisitionTimeStart >= 0) && (acquisitionTimeEnd > acquisitionTimeStart)) {
                    	    acquisitionTime = tags.substring(acquisitionTimeStart, acquisitionTimeEnd);
                    	    acquisitionTimeStart = acquisitionTime.indexOf(">");
                    	    acquisitionTime = acquisitionTime.substring(acquisitionTimeStart + 1);
                    	    Preferences.debug("Acquisition time = " + acquisitionTime + "\n", Preferences.DEBUG_FILEIO);
                    	    fileInfo.setAcquisitionTime(acquisitionTime);
                    	} // if ((acquisitionTimeStart >= 0) && (acquisitionTimeEnd > acquisitionTimeStart))
                    	stageXPositionStart = tags.indexOf("<StageXPosition>");
                    	stageXPositionEnd = tags.indexOf("</StageXPosition>");
                    	if ((stageXPositionStart >= 0) && (stageXPositionEnd > stageXPositionStart)) {
                    	    stageXPosition = tags.substring(stageXPositionStart, stageXPositionEnd);
                    	    stageXPositionStart = stageXPosition.indexOf(">");
                    	    stageXPosition = stageXPosition.substring(stageXPositionStart + 1);
                    	    Preferences.debug("Stage axis X position in micrometers = " + stageXPosition + "\n", Preferences.DEBUG_FILEIO);
                    	    fileInfo.setStageXPosition(stageXPosition);
                    	} // if ((stageXPositionStart >= 0) && (stageXPositionEnd > stageXPositionStart))
                    	stageYPositionStart = tags.indexOf("<StageYPosition>");
                    	stageYPositionEnd = tags.indexOf("</StageYPosition>");
                    	if ((stageYPositionStart >= 0) && (stageYPositionEnd > stageYPositionStart)) {
                    	    stageYPosition = tags.substring(stageYPositionStart, stageYPositionEnd);
                    	    stageYPositionStart = stageYPosition.indexOf(">");
                    	    stageYPosition = stageYPosition.substring(stageYPositionStart + 1);
                    	    Preferences.debug("Stage axis Y position in micrometers = " + stageYPosition + "\n", Preferences.DEBUG_FILEIO);
                    	    fileInfo.setStageYPosition(stageYPosition);
                    	} // if ((stageYPositionStart >= 0) && (stageYPositionEnd > stageYPositionStart))
                    } // if ((tagsStart >= 0) && (tagsEnd > tagsStart))
                    dataSchemaStart = metaData.indexOf("<DataSchema>");
                    dataSchemaEnd = metaData.indexOf("</DataSchema>");
                    if ((dataSchemaStart >= 0) && (dataSchemaEnd > dataSchemaStart)) {
                        dataSchema = metaData.substring(dataSchemaStart, dataSchemaEnd);
                        dataSchemaStart = dataSchema.indexOf(">");
                        dataSchema = dataSchema.substring(dataSchemaStart);
                        validBitsPerPixelStart = dataSchema.indexOf("<ValidBitsPerPixel>");
                        validBitsPerPixelEnd = dataSchema.indexOf("</ValidBitsPerPixel>");
                        if ((validBitsPerPixelStart >= 0) && (validBitsPerPixelEnd > validBitsPerPixelStart)) {
                            validBitsPerPixel = dataSchema.substring(validBitsPerPixelStart, validBitsPerPixelEnd);	
                            validBitsPerPixelStart = validBitsPerPixel.indexOf(">");
                            validBitsPerPixel = validBitsPerPixel.substring(validBitsPerPixelStart+1);
                            Preferences.debug("Valid bits per pixel = " + validBitsPerPixel + "\n",Preferences.DEBUG_FILEIO);
                            fileInfo.setValidBitsPerPixel(validBitsPerPixel);
                        } // if ((validBitsPerPixelStart >= 0) && (validBitsPerPixelEnd > validBitsPerPixelStart))
                    } // if ((dataSchemaStart >= 0) && (dataSchemaEnd > dataSchemaStart))
                    //Preferences.debug("SubBlock Segment metadata:\n", Preferences.DEBUG_FILEIO);
                    //Preferences.debug(metaData + "\n");
                    location = raFile.getFilePointer();
                    if (location + dataSize < raFile.length()) {
                        imageDataLocation.add(location);
                    }
                    // Skip attachments for now
                } // else if (charID.trim().equals("ZISRAWSUBBLOCK"))
                else if (charID.trim().equals("ZISRAWATTACH")) {
                	dataSize = readInt(endianess);
                	Preferences.debug("Size of the Attachment Segment data section = " + dataSize + "\n", Preferences.DEBUG_FILEIO);
                	// Skip 12 reserved bytes
                    byte spare[] = new byte[12];
                    raFile.read(spare);
                 // Read AttachEntry A1
                	schemaType = getString(2);
                	if (schemaType.equals("A1")) {
                	    Preferences.debug("SchemaType is A1 as expected\n", Preferences.DEBUG_FILEIO);
                	    byte reserved[] = new byte[10];
                	    raFile.read(reserved);
                	    filePosition = readLong(endianess);
                	    Preferences.debug("Seek offset relative to the first byte of the file = " +
                	    filePosition + "\n", Preferences.DEBUG_FILEIO);
                	    // Reserved filePart
                	    readInt(endianess);
                	    // Java cannot handle 16 bit GUID
                	    // Unique ID to be used in strong, fully qualified references
                	    readLong(endianess);
                	    readLong(endianess);
                	    contentFileType = getString(8).trim();
                	    if (contentFileType.equals("ZIP")) {
                	    	Preferences.debug("Attachment file is a ZIP compressed stream\n", Preferences.DEBUG_FILEIO);
                	    }
                	    else if (contentFileType.equals("ZISRAW")) {
                	    	Preferences.debug("Attachment file is an embedded ZISRAW file\n", Preferences.DEBUG_FILEIO);
                	    }
                	    else if (contentFileType.equals("CZTIMS")) {
                	    	Preferences.debug("Attachment file is a time stamp list\n", Preferences.DEBUG_FILEIO);
                	    }
                	    else if (contentFileType.equals("CZEVL")) {
                	    	Preferences.debug("Attachment file is an event list\n", Preferences.DEBUG_FILEIO);
                	    }
                	    else if (contentFileType.equals("CZLUT")) {
                	    	Preferences.debug("Attachment file is a lookup table\n", Preferences.DEBUG_FILEIO);
                	    }
                	    else if (contentFileType.equals("CZPML")) {
                	    	Preferences.debug("Attachment file is a Pal molecule list\n", Preferences.DEBUG_FILEIO);
                	    }
                	    else {
                	    	Preferences.debug("Attachment file is " + contentFileType + "\n", Preferences.DEBUG_FILEIO);
                	    }
                	    name = readCString();
                	    Preferences.debug("Name for attachment file = " + name + "\n", Preferences.DEBUG_FILEIO);
                	    // Go to start of embedded file
                	    raFile.seek(filePosition + 288L);
                	    if (name.equals("TimeStamps")) {
                	        timeStampSegmentSize = readInt(endianess);
                	        Preferences.debug("Time stamp segment size = " + timeStampSegmentSize + "\n", Preferences.DEBUG_FILEIO);
                	        numberTimeStamps = readInt(endianess);
                	        Preferences.debug("Number of time stamps in the list = " + numberTimeStamps + "\n");
                	        timeStamps = new double[numberTimeStamps];
                	        Preferences.debug("Time stamps in seconds relative to the start time of acquisition:\n",
                	        		Preferences.DEBUG_FILEIO);
                	        for (i = 0; i < numberTimeStamps; i++) {
                	        	timeStamps[i] = readDouble(endianess);
                	        	Preferences.debug("Time stamp " + i + ":     " + timeStamps[i] + "\n", Preferences.DEBUG_FILEIO);
                	        }
                	        fileInfo.setTimeStamps(timeStamps);
                	    } // if (name.equals("TimeStamps"))
                	    else if (name.equals("EventList")) {
                	        eventListSegmentSize = readInt(endianess);	
                	        Preferences.debug("Event list segment size = " + eventListSegmentSize + "\n", Preferences.DEBUG_FILEIO);
                	        numberEvents = readInt(endianess);
                	        Preferences.debug("Number of events = " + numberEvents + "\n", Preferences.DEBUG_FILEIO);
                	        eventTimes = new double[numberEvents];
                	        eventTypes = new int[numberEvents];
                	        eventDescriptions = new String[numberEvents];
                	        for (i = 0; i < numberEvents; i++) {
                	        	Preferences.debug("Event " + i + ":\n", Preferences.DEBUG_FILEIO);
                	            eventListEntrySize = readInt(endianess);
                	            Preferences.debug("Event list entry size in bytes = " + eventListEntrySize + "\n",
                	            		Preferences.DEBUG_FILEIO);
                	            eventTimes[i] = readDouble(endianess);
                	            Preferences.debug("Time of the event in seconds relative to the start time of the LSM electronic\n",
                	            Preferences.DEBUG_FILEIO);
                	            Preferences.debug("module controller program = " + eventTimes[i] + "\n", Preferences.DEBUG_FILEIO);
                	            eventTypes[i] = readInt(endianess);
                	            if (eventTypes[i] == 0) {
                	            	Preferences.debug("Experimental annotation\n", Preferences.DEBUG_FILEIO);
                	            }
                	            else if (eventTypes[i] == 1) {
                	                Preferences.debug("The time interval has changed\n", Preferences.DEBUG_FILEIO);	
                	            }
                	            else if (eventTypes[i] == 2) {
                	            	Preferences.debug("Start of a bleach operation\n", Preferences.DEBUG_FILEIO);
                	            }
                	            else if (eventTypes[i] == 3) {
                	            	Preferences.debug("End of a bleach operation\n", Preferences.DEBUG_FILEIO);
                	            }
                	            else if (eventTypes[i] == 4) {
                	            	Preferences.debug("A trigger signal was detected on the user port of the elctronic module\n",
                	            			Preferences.DEBUG_FILEIO);
                	            }
                	            descriptionSize = readInt(endianess);
                	            Preferences.debug("Size of the description character array = " + descriptionSize + "\n",
                	            		Preferences.DEBUG_FILEIO);
                	            eventDescriptions[i] = readCString();
                	            Preferences.debug("Description = " + eventDescriptions[i] + "\n", Preferences.DEBUG_FILEIO);
                	        }
                	    } // else if (name.equals("EventList"))
                	    else if (name.equals("LookupTables")) {
                	        lookupTablesSegmentSize = readInt(endianess);
                	        Preferences.debug("Lookup tables segment size in bytes = " + lookupTablesSegmentSize + "\n",
                	        		Preferences.DEBUG_FILEIO);
                	        numberLookupTables = readInt(endianess);
                	        Preferences.debug("Number of lookup tables = " + numberLookupTables + "\n", Preferences.DEBUG_FILEIO);
                	        for (i = 0; i < numberLookupTables; i++) {
                	            // Read LookupTableEntry
                	        	lookupEntryStartPosition = raFile.getFilePointer();
                	        	lookupTableEntrySize = readInt(endianess);
                	        	Preferences.debug("Lookup table entry size not including size field = " +
                	        	    lookupTableEntrySize + "\n", Preferences.DEBUG_FILEIO);
                	        	identifier = readCString();
                	        	Preferences.debug("Lookup table name = " + identifier + "\n", Preferences.DEBUG_FILEIO);
                	        	raFile.seek(lookupEntryStartPosition + 84);
                	        	numberComponents = readInt(endianess);
                	        	Preferences.debug("Number of components in the lookup table = " + numberComponents + "\n",
                	        			Preferences.DEBUG_FILEIO);
                	        	for (j = 0; j < numberComponents; j++) {
                	        	    componentEntrySize = readInt(endianess);
                	        	    Preferences.debug("Component entry size not including the size field = " +
                	        	        componentEntrySize + "\n", Preferences.DEBUG_FILEIO);
                	        	    componentType = readInt(endianess);
                	        	    if (componentType == -1) {
                	        	    	Preferences.debug("Component type = all(RGB)\n", Preferences.DEBUG_FILEIO);
                	        	    }
                	        	    else if (componentType == 0) {
                	        	    	Preferences.debug("Component type = red\n", Preferences.DEBUG_FILEIO);
                	        	    }
                	        	    else if (componentType == 1) {
                	        	    	Preferences.debug("Component type = green\n", Preferences.DEBUG_FILEIO);
                	        	    }
                	        	    else if (componentType == 2) {
                	        	    	Preferences.debug("Component type = blue\n", Preferences.DEBUG_FILEIO);
                	        	    }
                	        	    numberIntensities = readInt(endianess);
                	        	    Preferences.debug("Number of intensities = " + numberIntensities + "\n", Preferences.DEBUG_FILEIO);
                	        	    intensity = new short[numberIntensities];
                	        	    for (k = 0; k < numberIntensities; k++) {
                	        	        intensity[k] = readShort(endianess);
                	        	        Preferences.debug("Intensity["+k+"] = " + intensity[k] + "\n", Preferences.DEBUG_FILEIO);
                	        	    } // for (k = 0; k < numberIntensities; k++)
                	        	} // for (j = 0; j < numberComponents; j++)
                	        } // for (i = 0; i < numberLookupTables; i++) 
                	    } //  else if (name.equals("LookupTables"))
                	} // if (schemaType.equals("A1"))
                	else {
                		Preferences.debug("SchemaType is " + schemaType + " instead of the expected A1\n", Preferences.DEBUG_FILEIO);
                	}	
                }
                position += (allocatedSize + 32);
                if (position < fileLength) {
                    raFile.seek(position);
                    Preferences.debug("location = " + position + "\n", Preferences.DEBUG_FILEIO);
                } else {
                    readSegment = false;
                }
            } // while (readSegment)

            for (i = 0; imageDimension[i] != null && i < 8; i++) {
                for (j = 1; j < imageDimension[i].size(); j++) {
                    if ( !imageDimension[i].get(0).equals(imageDimension[i].get(j))) {
                        Preferences.debug("imageDimension[" + i + "].get(0) = " + imageDimension[i].get(0) + "\n", Preferences.DEBUG_FILEIO);
                        Preferences.debug("imageDimension[" + i + "].get(" + j + ") = " + imageDimension[i].get(j) + "\n", Preferences.DEBUG_FILEIO);
                        raFile.close();
                        throw new IOException("imageDimension[" + i + "] had values of " + imageDimension[i].get(0) + " and " + imageDimension[i].get(j));
                    }
                }
            }

            for (i = 0; imageSize[i] != null && i < 8; i++) {
                for (j = 1; j < imageSize[i].size(); j++) {
                    if (imageSize[i].get(0).intValue() != imageSize[i].get(j).intValue()) {
                        Preferences.debug("imageSize[" + i + "].get(0).intValue() = " + imageSize[i].get(0).intValue() + "\n", Preferences.DEBUG_FILEIO);
                        Preferences
                                .debug("imageSize[" + i + "].get(" + j + ").intValue() = " + imageSize[i].get(j).intValue() + "\n", Preferences.DEBUG_FILEIO);
                        raFile.close();
                        throw new IOException("imageSize[" + i + "] had values of " + imageSize[i].get(0).intValue() + " and " + imageSize[i].get(j).intValue());
                    }
                }
            }

            for (i = 0; imageStartCoordinate[i] != null && i < 8; i++) {
                for (j = 1; j < imageStartCoordinate[i].size(); j++) {
                    if (imageStartCoordinate[i].get(0).floatValue() != imageStartCoordinate[i].get(j).floatValue()) {
                        Preferences.debug("imageStartCoordinate[" + i + "].get(0).floatValue() = " + imageStartCoordinate[i].get(0).floatValue() + "\n",
                                Preferences.DEBUG_FILEIO);
                        Preferences.debug(
                                "imageStartCoordinate[" + i + "].get(" + j + ").floatValue() = " + imageStartCoordinate[i].get(j).floatValue() + "\n",
                                Preferences.DEBUG_FILEIO);
                        raFile.close();
                        throw new IOException("imageStartCoordinate[" + i + "] had values of " + imageStartCoordinate[i].get(0).floatValue() + " and "
                                + imageStartCoordinate[i].get(j).floatValue());
                    }
                }
            }
            
            // This handles images where the last t slice is missing the last h slices.
            hDim = 1;
            if (imageHStartIndex.size() == imageDimension[0].size()) {
            	for (i = 0; i < imageHStartIndex.size(); i++) {
            		if ((imageHStartIndex.get(i) + 1) > hDim) {
            			hDim = imageHStartIndex.get(i) + 1;
            		}
            	}
            }
            
            // This handles images where the last t slice is missing the last m slices.
            mDim = 1;
            if (imageMStartIndex.size() == imageDimension[0].size()) {
            	for (i = 0; i < imageMStartIndex.size(); i++) {
            		if ((imageMStartIndex.get(i) + 1) > mDim) {
            			mDim = imageMStartIndex.get(i) + 1;
            		}
            	}
            }
            
           // This handles images where the last t slice is missing the last s slices.
            sDim = 1;
            if (imageSStartIndex.size() == imageDimension[0].size()) {
            	for (i = 0; i < imageSStartIndex.size(); i++) {
            		if ((imageSStartIndex.get(i) + 1) > sDim) {
            			sDim = imageSStartIndex.get(i) + 1;
            		}
            	}
            }
            
            if ((imageColorStartIndex.size() == imageDimension[0].size()) && ((imageColorStartIndex.size() % 4) == 0) &&
            		(mDim < 2) && (sDim < 2)) {
            	isColor4 = true;
            	rapidChangeColor = true;
            	for (i = 0; i < imageColorStartIndex.size(); i += 4) {
            		if ((imageColorStartIndex.get(i).intValue() != 0) || (imageColorStartIndex.get(i+1).intValue() != 1) ||
            			(imageColorStartIndex.get(i+2).intValue() != 2) || (imageColorStartIndex.get(i+1).intValue() != 3)) {
            			isColor4 = false;
            			rapidChangeColor = false;
            		}
            	}
            }
            
            if ((!isColor4) && (imageColorStartIndex.size() == imageDimension[0].size()) && ((imageColorStartIndex.size() % 3) == 0)
            		&& (mDim < 2) && (sDim < 2)) {
            	isColor3 = true;
            	rapidChangeColor = true;
            	for (i = 0; i < imageColorStartIndex.size(); i += 3) {
            		if ((imageColorStartIndex.get(i).intValue() != 0) || (imageColorStartIndex.get(i+1).intValue() != 1) ||
            			(imageColorStartIndex.get(i+2).intValue() != 2)) {
            			isColor3 = false;
            			rapidChangeColor = false;
            		}
            	}
            }
            
            if ((!isColor4) && (!isColor3) && (imageColorStartIndex.size() == imageDimension[0].size()) && 
            		((imageColorStartIndex.size() % 2) == 0) && (mDim < 2) && (sDim < 2)) {
            	isColor2 = true;
            	rapidChangeColor = true;
            	for (i = 0; i < imageColorStartIndex.size(); i += 2) {
            		if ((imageColorStartIndex.get(i).intValue() != 0) || (imageColorStartIndex.get(i+1).intValue() != 1)) {
            			isColor2 = false;
            			rapidChangeColor = false;
            		}
            	}
            }
            
            if ((!rapidChangeColor) && (imageColorStartIndex.size() == imageDimension[0].size()) &&
            		((imageColorStartIndex.size() % 4) == 0) && (mDim < 2) && (sDim < 2)) {
            	isColor4 = true;
            	slowChangeColor = true;
            	for (i = 0; i < imageColorStartIndex.size()/4; i++) {
            		if (imageColorStartIndex.get(i).intValue() != 0) {
            			isColor4 = false;
            			slowChangeColor = false;
            		}
            	}
            	for (i = imageColorStartIndex.size()/4; i < 2 * imageColorStartIndex.size()/4; i++) {
            		if (imageColorStartIndex.get(i).intValue() != 1) {
            			isColor4 = false;
            			slowChangeColor = false;
            		}
            	}
            	for (i = 2 * imageColorStartIndex.size()/4; i < 3 * imageColorStartIndex.size()/4; i++) {
            		if (imageColorStartIndex.get(i).intValue() != 2) {
            			isColor4 = false;
            			slowChangeColor = false;
            		}
            	}
            	for (i = 3 * imageColorStartIndex.size()/4; i < imageColorStartIndex.size(); i++) {
            		if (imageColorStartIndex.get(i).intValue() != 3) {
            			isColor4 = false;
            			slowChangeColor = false;
            		}
            	}
            }
            
            if ((!isColor4) && (!rapidChangeColor) && (imageColorStartIndex.size() == imageDimension[0].size()) &&
            		((imageColorStartIndex.size() % 3) == 0) && (mDim < 2) && (sDim < 2)) {
            	isColor3 = true;
            	slowChangeColor = true;
            	for (i = 0; i < imageColorStartIndex.size()/3; i++) {
            		if (imageColorStartIndex.get(i).intValue() != 0) {
            			isColor3 = false;
            			slowChangeColor = false;
            		}
            	}
            	for (i = imageColorStartIndex.size()/3; i < 2 * imageColorStartIndex.size()/3; i++) {
            		if (imageColorStartIndex.get(i).intValue() != 1) {
            			isColor3 = false;
            			slowChangeColor = false;
            		}
            	}
            	for (i = 2 * imageColorStartIndex.size()/3; i < imageColorStartIndex.size(); i++) {
            		if (imageColorStartIndex.get(i).intValue() != 2) {
            			isColor3 = false;
            			slowChangeColor = false;
            		}
            	}
            }
            
            if ((!rapidChangeColor) && (!isColor4) && (!isColor3) && (imageColorStartIndex.size() == imageDimension[0].size()) &&
            		((imageColorStartIndex.size() % 2) == 0) && (mDim < 2) && (sDim < 2)) {
            	isColor2 = true;
            	slowChangeColor = true;
            	for (i = 0; i < imageColorStartIndex.size()/2; i++) {
            		if (imageColorStartIndex.get(i).intValue() != 0) {
            			isColor2 = false;
            			slowChangeColor = false;
            		}
            	}
            	for (i = imageColorStartIndex.size()/2; i < imageColorStartIndex.size(); i++) {
            		if (imageColorStartIndex.get(i).intValue() != 1) {
            			isColor2 = false;
            			slowChangeColor = false;
            		}
            	}
            }
            
            if ((!rapidChangeColor) && (!slowChangeColor) && (!isColor4) && (!isColor3) &&
            		(imageColorStartIndex.size() == imageDimension[0].size()) &&
            		((imageColorStartIndex.size() % 2) == 0) && (mDim > 1)) {
            	phaseColor2Mosaic = true;
            	for (i = 0, h = 0, c = 0, m = 0; i < imageColorStartIndex.size(); i++) {
            		if (imageHStartIndex.size() == imageColorStartIndex.size()) {
	            		if ((imageHStartIndex.get(i).intValue() != h) || (imageColorStartIndex.get(i).intValue() != c) ||
	            		    (imageMStartIndex.get(i).intValue() != m)) {
	            			phaseColor2Mosaic = false;
	            		}
            		}
            		else if ((imageColorStartIndex.get(i).intValue() != c) ||
    	            		    (imageMStartIndex.get(i).intValue() != m)) {
    	                phaseColor2Mosaic = false;	
            		}
            		if (h < hDim-1) {
            			h++;
            		}
            		else if ( c == 0) {
            			h = 0;
            			c = 1;
            		}
            		else {
            			h = 0;
            			c = 0;
            			m++;
            		}
            	}
            }
            
            isColor = isColor2 || isColor3 || phaseColor2Mosaic;
            
            if (phaseColor2Mosaic) {
                mosaicMinX = Integer.MAX_VALUE;
                mosaicMaxX = Integer.MIN_VALUE;
                mosaicMinY = Integer.MAX_VALUE;
                mosaicMaxY = Integer.MIN_VALUE;
                for (i = 0; i < imageStartIndex[xIndex].size(); i++) {
                	if (imageStartIndex[xIndex].get(i).intValue() < mosaicMinX) {
                		mosaicMinX = imageStartIndex[xIndex].get(i).intValue();
                	}
                	if ((imageStartIndex[xIndex].get(i).intValue() + imageSize[xIndex].get(i).intValue() - 1) > mosaicMaxX) {
                		mosaicMaxX = imageStartIndex[xIndex].get(i).intValue() + imageSize[xIndex].get(i).intValue() - 1;
                	}
                	if (imageStartIndex[yIndex].get(i).intValue() < mosaicMinY) {
                		mosaicMinY = imageStartIndex[yIndex].get(i).intValue();
                	}
                	if ((imageStartIndex[yIndex].get(i).intValue() + imageSize[yIndex].get(i).intValue() - 1) > mosaicMaxY) {
                		mosaicMaxY = imageStartIndex[yIndex].get(i).intValue() + imageSize[yIndex].get(i).intValue() - 1;
                	}
                }
                mosaicSizeX = mosaicMaxX - mosaicMinX + 1;
                mosaicSizeY = mosaicMaxY - mosaicMinY + 1;
            } // if (phaseColor2Mosaic)
            
            // This handles images where the last t slice is missing the last z slices.
            zDim = 1;
            if (imageZStartIndex.size() == imageDimension[0].size()) {
            	for (i = 0; i < imageZStartIndex.size(); i++) {
            		if ((imageZStartIndex.get(i) + 1) > zDim) {
            			zDim = imageZStartIndex.get(i) + 1;
            		}
            	}
            }
            
            if (imageTStartIndex.size() == imageDimension[0].size()) {
            	tDim = imageTStartIndex.get(imageTStartIndex.size() - 1) + 1;
            }
            
            if ((!rapidChangeColor) && (!slowChangeColor) && (!isColor4) && (!isColor3) &&
            		 (sDim > 1) && (zDim > 1)) {
            	zScene = true;
            	for (i = 0, z = 0, s = 0; i < imageSStartIndex.size(); i++) {
            		if ((imageZStartIndex.get(i).intValue() != z) || (imageSStartIndex.get(i).intValue() != s)) {
            			zScene = false;
            		}
            		
            		if (z < zDim-1) {
            			z++;
            		}
            		
            		else {
            			z = 0;
            			s++;
            		}
            	}
            }
            
            if (zScene) {
                sceneMinX = Integer.MAX_VALUE;
                sceneMaxX = Integer.MIN_VALUE;
                sceneMinY = Integer.MAX_VALUE;
                sceneMaxY = Integer.MIN_VALUE;
                for (i = 0; i < imageStartIndex[xIndex].size(); i++) {
                	if (imageStartIndex[xIndex].get(i).intValue() < sceneMinX) {
                		sceneMinX = imageStartIndex[xIndex].get(i).intValue();
                	}
                	if ((imageStartIndex[xIndex].get(i).intValue() + imageSize[xIndex].get(i).intValue() - 1) > sceneMaxX) {
                		sceneMaxX = imageStartIndex[xIndex].get(i).intValue() + imageSize[xIndex].get(i).intValue() - 1;
                	}
                	if (imageStartIndex[yIndex].get(i).intValue() < sceneMinY) {
                		sceneMinY = imageStartIndex[yIndex].get(i).intValue();
                	}
                	if ((imageStartIndex[yIndex].get(i).intValue() + imageSize[yIndex].get(i).intValue() - 1) > sceneMaxY) {
                		sceneMaxY = imageStartIndex[yIndex].get(i).intValue() + imageSize[yIndex].get(i).intValue() - 1;
                	}
                }
                sceneSizeX = sceneMaxX - sceneMinX + 1;
                sceneSizeY = sceneMaxY - sceneMinY + 1;
            } // if (zScene)
            
            if (pixelType == Gray8) {
            	if (!isColor) {
                    dataType = ModelStorageBase.UBYTE;
                    Preferences.debug("MIPAV data type = UBYTE\n", Preferences.DEBUG_FILEIO);
            	}
            	else {
            		dataType = ModelStorageBase.ARGB;
            		Preferences.debug("MIPAV data type = ARGB\n", Preferences.DEBUG_FILEIO);
            	}
            } else if (pixelType == Gray16) {
            	if (!isColor) {
                    dataType = ModelStorageBase.USHORT;
                    Preferences.debug("MIPAV data type = USHORT\n", Preferences.DEBUG_FILEIO);
            	}
            	else {
            		dataType = ModelStorageBase.ARGB_USHORT;
            		Preferences.debug("MIPAV data type = ARGB_USHORT\n", Preferences.DEBUG_FILEIO);
            	}
            } else if (pixelType == Gray32Float) {
                dataType = ModelStorageBase.FLOAT;
                Preferences.debug("MIPAV data type = FLOAT\n", Preferences.DEBUG_FILEIO);
            } else if (pixelType == Bgr24) {
                dataType = ModelStorageBase.ARGB;
                Preferences.debug("MIPAV data type = ARGB\n", Preferences.DEBUG_FILEIO);
            } else if (pixelType == Bgr48) {
                dataType = ModelStorageBase.ARGB_USHORT;
                Preferences.debug("MIPAV data type = ARGB_USHORT\n", Preferences.DEBUG_FILEIO);
            } else if (pixelType == Bgr96Float) {
                dataType = ModelStorageBase.ARGB_FLOAT;
                Preferences.debug("MIPAV data type = ARGB_FLOAT\n", Preferences.DEBUG_FILEIO);
            } else if (pixelType == Bgra32) {
                dataType = ModelStorageBase.ARGB;
                Preferences.debug("MIPAV data type = ARGB\n", Preferences.DEBUG_FILEIO);
            } else if (pixelType == Gray64ComplexFloat) {
                dataType = ModelStorageBase.COMPLEX;
                Preferences.debug("MIPAV data type = COMPLEX\n", Preferences.DEBUG_FILEIO);
            } else if (pixelType == Bgr192ComplexFloat) {
                Preferences.debug("Bgr192ComplexFloat has no corresponding MIPAV data type\n", Preferences.DEBUG_FILEIO);
                raFile.close();
                throw new IOException("Bgr192ComplexFloat has no corresponding MIPAV data type");
            } else if (pixelType == Gray32) {
                dataType = ModelStorageBase.INTEGER;
                Preferences.debug("MIPAV data type = INTEGER\n", Preferences.DEBUG_FILEIO);
            } else if (pixelType == Gray64) {
                dataType = ModelStorageBase.DOUBLE;
                Preferences.debug("MIPAV data type = DOUBLE\n", Preferences.DEBUG_FILEIO);
            }
            fileInfo.setDataType(dataType);
            
            if (xIndex >= 0) {
            	actualDimensions++;
            	if (phaseColor2Mosaic) {
            		firstDimension = mosaicSizeX;
            		subBlockValues *= mosaicSizeX;
            	}
            	else if (zScene) {
            		firstDimension = sceneSizeX;
            		subBlockValues *= sceneSizeX;
            	}
            	else {
            	    firstDimension = imageSize[xIndex].get(0);
            	    subBlockValues *= imageSize[xIndex].get(0);
            	}
            	origin[0] = imageStartCoordinate[xIndex].get(0);
            }
            if (yIndex >= 0) {
            	actualDimensions++;
            	if (phaseColor2Mosaic) {
            	    secondDimension = mosaicSizeY;
            	    origin[1] = imageStartCoordinate[yIndex].get(0);
            	    subBlockValues *= mosaicSizeY;
            	} // if (phaseColor2Mosaic)
            	else if (zScene) {
            		secondDimension = sceneSizeY;
            		origin[1] = imageStartCoordinate[yIndex].get(0);
            		subBlockValues *= sceneSizeY;
            	}
            	else {
	            	if (firstDimension == -1) {
	            		firstDimension = imageSize[yIndex].get(0);
	            		origin[0] = imageStartCoordinate[yIndex].get(0);
	            	}
	            	else {
	            		secondDimension = imageSize[yIndex].get(0);
	            		origin[1] = imageStartCoordinate[yIndex].get(0);
	            	}
	            	subBlockValues *= imageSize[yIndex].get(0);
            	}
            }
            if (hIndex >= 0) {
            	actualDimensions++;
            	if (firstDimension == -1) {
            		firstDimension = imageSize[hIndex].get(0);
            		origin[0] = imageStartCoordinate[hIndex].get(0);
            	}
            	else if (secondDimension == -1) {
            	    secondDimension	= imageSize[hIndex].get(0);
            	    origin[1] = imageStartCoordinate[hIndex].get(0);
            	}
            	else {
            		thirdDimension = imageSize[hIndex].get(0);
            		origin[2] = imageStartCoordinate[hIndex].get(0);
            	}
            	subBlockValues *= imageSize[hIndex].get(0);
            }
            if (zIndex >= 0) {
            	actualDimensions++;
            	if (firstDimension == -1) {
            		firstDimension = imageSize[zIndex].get(0);
            		origin[0] = imageStartCoordinate[zIndex].get(0);
            	}
            	else if (secondDimension == -1) {
            	    secondDimension	= imageSize[zIndex].get(0);
            	    origin[1] = imageStartCoordinate[zIndex].get(0);
            	}
            	else if (thirdDimension == -1){
            		thirdDimension = imageSize[zIndex].get(0);
            		origin[2] = imageStartCoordinate[zIndex].get(0);
            	}
            	else {
            		fourthDimension = imageSize[zIndex].get(0);
            		origin[3] = imageStartCoordinate[zIndex].get(0);	
            	}
            	subBlockValues *= imageSize[zIndex].get(0);
            }
            if (tIndex >= 0) {
            	actualDimensions++;
            	if (firstDimension == -1) {
            		firstDimension = imageSize[tIndex].get(0);
            		origin[0] = imageStartCoordinate[tIndex].get(0);
            	}
            	else if (secondDimension == -1) {
            		secondDimension = imageSize[tIndex].get(0);
            		origin[1] = imageStartCoordinate[tIndex].get(0);
            	}
            	else if (thirdDimension == -1) {
            		thirdDimension = imageSize[tIndex].get(0);
            		origin[2] = imageStartCoordinate[tIndex].get(0);
            	}
            	else {
            		fourthDimension = imageSize[tIndex].get(0);
            		origin[3] = imageStartCoordinate[tIndex].get(0);
            	}
            	subBlockValues *= imageSize[tIndex].get(0);
            }
            Preferences.debug("subBlockValues = " + subBlockValues + "\n", Preferences.DEBUG_FILEIO);
            fileInfo.setOrigin(origin);
            fileInfo.setResolutions(imgResols);
            fileInfo.setUnitsOfMeasure(unitsOfMeasure);
            
            if (hDim > 1) {
            	actualDimensions++;
            	if (firstDimension == -1) {
            		firstDimension = hDim;
            	}
            	else if (secondDimension == -1) {
            		secondDimension = hDim;
            	}
            	else if (thirdDimension == -1) {
            		thirdDimension = hDim;
            	}
            	else if (fourthDimension == -1) {
            	    fourthDimension = hDim;	
            	}
            }
            
            if (zDim > 1) {
            	actualDimensions++;
            	if (firstDimension == -1) {
            		firstDimension = zDim;
            	}
            	else if (secondDimension == -1) {
            		secondDimension = zDim;
            	}
            	else if (thirdDimension == -1) {
            		thirdDimension = zDim;
            	}
            	else if (fourthDimension == -1) {
            	    fourthDimension = zDim;	
            	}
            }
            
            if (tDim > 1) {
            	actualDimensions++;
            	if (firstDimension == -1) {
            		firstDimension = tDim;
            	}
            	else if (secondDimension == -1) {
            		secondDimension = tDim;
            	}
            	else if (thirdDimension == -1) {
            		thirdDimension = tDim;
            	}
            	else if (fourthDimension == -1) {
            	    fourthDimension = tDim;	
            	}
            }
            
            if (isColor4) {
            	actualDimensions++;
            	if (firstDimension == -1) {
            		firstDimension = 4;
            	}
            	else if (secondDimension == -1) {
            		secondDimension = 4;
            	}
            	else if (thirdDimension == -1) {
            		thirdDimension = 4;
            	}
            	else if (fourthDimension == -1) {
            	    fourthDimension = 4;	
            	}	
            }
            
            if (actualDimensions == 1) {
            	Preferences.debug("One dimensional ModelImage cannot be handled by MIPAV\n", Preferences.DEBUG_FILEIO);
            	raFile.close();
            	throw new IOException("One dimensional ModelImage cannot be handled by MIPAV");
            }
            imageExtents = new int[actualDimensions];
            imageExtents[0] = firstDimension;
            Preferences.debug("imageExtents[0] = " + imageExtents[0] + "\n", Preferences.DEBUG_FILEIO);
            imageExtents[1] = secondDimension;
            Preferences.debug("imageExtents[1] = " + imageExtents[1] + "\n", Preferences.DEBUG_FILEIO);
            if (actualDimensions > 2) {
            	imageExtents[2] = thirdDimension;
            	imageSlices *= imageExtents[2];
            	 Preferences.debug("imageExtents[2] = " + imageExtents[2] + "\n", Preferences.DEBUG_FILEIO);
            	if (actualDimensions > 3) {
            		imageExtents[3] = fourthDimension;
            		imageSlices *= imageExtents[3];
            		 Preferences.debug("imageExtents[3] = " + imageExtents[3] + "\n", Preferences.DEBUG_FILEIO);
            	}
            }
            fileInfo.setExtents(imageExtents);
            
            image = new ModelImage(dataType, imageExtents, fileName);
            
            if (dataType == ModelStorageBase.UBYTE) {
            	byteBuffer = new byte[subBlockValues];
            }
            else if (dataType == ModelStorageBase.USHORT) {
            	if (zScene) {
            		sceneShortBuffer = new short[sceneSizeX * sceneSizeY];
            	}
            	else {
            	    shortBuffer = new short[subBlockValues];
            	}
            }
            else if ((pixelType == Gray8) && (dataType == ModelStorageBase.ARGB)) {
            	byteBuffer = new byte[subBlockValues];
            }
            else if ((pixelType == Gray16) && (dataType == ModelStorageBase.ARGB_USHORT)) {
            	if (phaseColor2Mosaic) {
            		mosaicShortBuffer = new short[mosaicSizeX * mosaicSizeY];
            	}
            	else {
            	    shortBuffer = new short[subBlockValues];
            	}
            }
            else if (pixelType == Bgr24) {
            	byteBuffer = new byte[subBlockValues];
            }
            else if (pixelType == Bgra32) {
            	byteBuffer = new byte[4*subBlockValues];
            	byteBuffer2 = new byte[4*subBlockValues];
            }
            
            Preferences.debug("imageDataLocation.size() = " + imageDataLocation.size() + "\n", Preferences.DEBUG_FILEIO);
            if (phaseColor2Mosaic) {
            	totalValuesRead = 0;
            	// hDim can be one
                for (h = 0; h < hDim; h++) {
                	for (c = 0; c < 2; c++) {
                		for (m = 0; m < mDim; m++) {
                			index = m*2*hDim + c*hDim + h;
                			raFile.seek(imageDataLocation.get(index));
                			localSizeX = imageSize[xIndex].get(index).intValue(); 
                			localSizeY = imageSize[yIndex].get(index).intValue();
                			localMinX = imageStartIndex[xIndex].get(index).intValue();
                			localMinY = imageStartIndex[yIndex].get(index).intValue();
                			for (y = 0; y < localSizeY; y++) {
                				for (x = 0; x < localSizeX; x++) {
                					mosaicShortBuffer[x + (localMinX - mosaicMinX) + mosaicSizeX * (y + (localMinY - mosaicMinY))] =
                							readShort(endianess);
                				} // for (x = 0; x < localSizeX; x++)
                			} // for (y = 0; y < localSizeY; y++)
                		} // for (m = 0; m < mDim; m++)
                		if (c == 0) {
                		    image.importRGBData(channelColor[0], totalValuesRead, mosaicShortBuffer, false);
                		}
                		else {
                			image.importRGBData(channelColor[1], totalValuesRead, mosaicShortBuffer, false);
                			totalValuesRead += 4*mosaicSizeX*mosaicSizeY;
                		}
                	} // for (c = 0; c < 2; c++)
                } // for (h = 0; h < hDim; h++) 
            } // if (phaseColor2Mosaic)
            else if (zScene) {
            	totalValuesRead = 0;
                for (z = 0; z < zDim; z++) {
            		for (s = 0; s < sDim; s++) {
            			index = s*zDim + z;
            			raFile.seek(imageDataLocation.get(index));
            			localSizeX = imageSize[xIndex].get(index).intValue(); 
            			localSizeY = imageSize[yIndex].get(index).intValue();
            			localMinX = imageStartIndex[xIndex].get(index).intValue();
            			localMinY = imageStartIndex[yIndex].get(index).intValue();
            			for (y = 0; y < localSizeY; y++) {
            				for (x = 0; x < localSizeX; x++) {
            					sceneShortBuffer[x + (localMinX - sceneMinX) + sceneSizeX * (y + (localMinY - sceneMinY))] =
            							readShort(endianess);
            				} // for (x = 0; x < localSizeX; x++)
            			} // for (y = 0; y < localSizeY; y++)
            		} // for (s = 0; s < sDim; s++)
            		image.importData(totalValuesRead, sceneShortBuffer, false);
            		totalValuesRead += sceneSizeX * sceneSizeY;
                } // for (z = 0; z < zDim; z++) 	
            } // else if (zScene)
            else {
	            for (i = 0; i < imageDataLocation.size(); i++) {
	            	raFile.seek(imageDataLocation.get(i));
	            	if (dataType == ModelStorageBase.UBYTE) {
		            	Preferences.debug("bytes sought from file = " + subBlockValues + "\n", Preferences.DEBUG_FILEIO);
		            	bytesRead = raFile.read(byteBuffer);
		            	Preferences.debug("bytes read from file = " + bytesRead + "\n", Preferences.DEBUG_FILEIO);
		            	image.importData(totalValuesRead, byteBuffer, false);
		            	totalValuesRead += subBlockValues;
	            	} // if (dataType == ModelStorageBase.UBYTE)
	            	else if (dataType == ModelStorageBase.USHORT) {
	            		// Works for slowChangeColor isColor4
	            		for (j = 0; j < subBlockValues; j++) {
	            			shortBuffer[j] = readShort(endianess);
	            		}
	            		//Preferences.debug("i = " + i + " totalValuesRead before importData = " + totalValuesRead +
	            				//"\n", Preferences.DEBUG_FILEIO);
	            		image.importData(totalValuesRead, shortBuffer, false);
	            		totalValuesRead +=subBlockValues;
	            	} // else if (dataType == ModelStorageBase.USHORT)
	            	else if (((pixelType == Gray8) && (dataType == ModelStorageBase.ARGB)) || (pixelType == Bgr24)) {
	            		Preferences.debug("bytes sought from file = " + subBlockValues + "\n", Preferences.DEBUG_FILEIO);
		            	bytesRead = raFile.read(byteBuffer);
		            	Preferences.debug("bytes read from file = " + bytesRead + "\n", Preferences.DEBUG_FILEIO);
		            	if (rapidChangeColor) {
			            	if (isColor3) {
			            		if ((i % 3) == 0) {
			            			image.importRGBData(channelColor[0], totalValuesRead, byteBuffer, false);
			            		}
			            		else if ((i % 3) == 1) {
			            			image.importRGBData(channelColor[1], totalValuesRead, byteBuffer, false);
			            		}
			            		else {
			            			image.importRGBData(channelColor[2], totalValuesRead, byteBuffer, false);
			            			totalValuesRead += 4 * subBlockValues;
			            		}
			            	} // if (isColor3)
			            	else { // isColor2
			            	    if ((i % 2) == 0) {
			            			image.importRGBData(channelColor[0], totalValuesRead, byteBuffer, false);	
			            	    }
			            	    else {
			            			image.importRGBData(channelColor[1], totalValuesRead, byteBuffer, false);	
			            			totalValuesRead += 4 * subBlockValues;
			            	    }
			            	} // else isColor2
		            	} // if (rapidChangeColor)
		            	else if (slowChangeColor) {
		            	    if (isColor3) {
		            	    	if (i < imageDataLocation.size()/3) {
		            	    		image.importRGBData(channelColor[0], totalValuesRead, byteBuffer, false);	
		            	    	}
		            	    	else if (i < 2 * imageDataLocation.size()/3) {
		            	    		image.importRGBData(channelColor[1], totalValuesRead, byteBuffer, false);	
		            	    	}
		            	    	else {
		            	    		image.importRGBData(channelColor[2], totalValuesRead, byteBuffer, false);	
		            	    	}
		            	    	if ((i == (imageDataLocation.size()/3 - 1)) || (i == (2 *imageDataLocation.size()/3 - 1))) {
		            	    		totalValuesRead = 0;
		            	    	}
		            	    	else {
		            	    		totalValuesRead += 4 * subBlockValues;	
		            	    	}
		            	    } // if (isColor3)
		            	    else { // isColor2
		            	    	if (i < imageDataLocation.size()/2) {
		            	    		image.importRGBData(channelColor[0], totalValuesRead, byteBuffer, false);	
		            	    	}
		            	    	else {
		            	    		image.importRGBData(channelColor[1], totalValuesRead, byteBuffer, false);	
		            	    	}
		            	    	if (i == (imageDataLocation.size()/2 - 1)) {
		            	    		totalValuesRead = 0;
		            	    	}
		            	    	else {
		            	    		totalValuesRead += 4 * subBlockValues;	
		            	    	}
		            	    } // else isColor2
		            	} // else if (slowChangeColor)
	            	} // else if (((pixelType == Gray8) && (dataType == ModelStorageBase.ARGB)) || (pixelType == Bgr24))
	            	else if ((pixelType == Gray16) && (dataType == ModelStorageBase.ARGB_USHORT)) {
	            		for (j = 0; j < subBlockValues; j++) {
	            			shortBuffer[j] = readShort(endianess);
	            		}
	            		if (rapidChangeColor) {
		            		if (isColor3) {
			            		if ((i % 3) == 0) {
			            			image.importRGBData(channelColor[0], totalValuesRead, shortBuffer, false);
			            		}
			            		else if ((i % 3) == 1) {
			            			image.importRGBData(channelColor[1], totalValuesRead, shortBuffer, false);
			            		}
			            		else {
			            			image.importRGBData(channelColor[2], totalValuesRead, shortBuffer, false);
			            			totalValuesRead += 4 * subBlockValues;
			            		}
		            		} // if (isColor3)
		            		else { // isColor2
		            		    if ((i % 2) == 0) {
			            			image.importRGBData(channelColor[0], totalValuesRead, shortBuffer, false);	
		            		    }
		            		    else {
			            			image.importRGBData(channelColor[1], totalValuesRead, shortBuffer, false);
			            			totalValuesRead += 4 * subBlockValues;
		            		    }
		            		} // else isColor2
	            		} // if (rapidChangeColor)
	            		else if (slowChangeColor) {
		            	    if (isColor3) {
		            	    	if (i < imageDataLocation.size()/3) {
		            	    		image.importRGBData(channelColor[0], totalValuesRead, shortBuffer, false);	
		            	    	}
		            	    	else if (i < 2 * imageDataLocation.size()/3) {
		            	    		image.importRGBData(channelColor[1], totalValuesRead, shortBuffer, false);	
		            	    	}
		            	    	else {
		            	    		image.importRGBData(channelColor[2], totalValuesRead, shortBuffer, false);	
		            	    	}
		            	    	if ((i == (imageDataLocation.size()/3 - 1)) || (i == (2 *imageDataLocation.size()/3 - 1))) {
		            	    		totalValuesRead = 0;
		            	    	}
		            	    	else {
		            	    		totalValuesRead += 4 * subBlockValues;	
		            	    	}
		            	    } // if (isColor3)
		            	    else { // isColor2
		            	    	if (i < imageDataLocation.size()/2) {
		            	    		image.importRGBData(channelColor[0], totalValuesRead, shortBuffer, false);	
		            	    	}
		            	    	else {
		            	    		image.importRGBData(channelColor[1], totalValuesRead, shortBuffer, false);	
		            	    	}
		            	    	if (i == (imageDataLocation.size()/2 - 1)) {
		            	    		totalValuesRead = 0;
		            	    	}
		            	    	else {
		            	    		totalValuesRead += 4 * subBlockValues;	
		            	    	}
		            	    } // else isColor2
		            	} // else if (slowChangeColor)
	            	} // else if ((pixelType == Gray16) && (dataType == ModelStorageBase.ARGB_USHORT))
	            	else if (pixelType == Bgra32) {
	            		Preferences.debug("bytes sought from file = " + (4*subBlockValues) + "\n", Preferences.DEBUG_FILEIO);
		            	bytesRead = raFile.read(byteBuffer);
		            	Preferences.debug("bytes read from file = " + bytesRead + "\n", Preferences.DEBUG_FILEIO);
		            	for (j = 0; j < subBlockValues; j++) {
		            		byteBuffer2[4*j] = byteBuffer[4*j+3];
		            		byteBuffer2[4*j+1] = byteBuffer[4*j+2];
		            		byteBuffer2[4*j+2] = byteBuffer[4*j+1];
		            		byteBuffer2[4*j+3] = byteBuffer[4*j];
		            	}
		            	image.importData(totalValuesRead, byteBuffer2, false);
		            	totalValuesRead += 4*subBlockValues;	
	            	}
	            } // for (i = 0; i < imageDataLocation.size(); i++)
            } // else 
            raFile.close();

            image.calcMinMax();
            
            for (i = 0; i < imageSlices; i++) {
                image.setFileInfo((FileInfoCZI)fileInfo.clone(), i);
            }

            fireProgressStateChanged(100);

        } catch (final OutOfMemoryError error) {

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

}
