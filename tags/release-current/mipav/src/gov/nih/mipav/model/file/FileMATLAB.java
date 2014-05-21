package gov.nih.mipav.model.file;


import gov.nih.mipav.model.algorithms.AlgorithmMorphology2D;
import gov.nih.mipav.model.algorithms.AlgorithmMorphology3D;
import gov.nih.mipav.model.algorithms.AlgorithmVOIExtraction;
import gov.nih.mipav.model.algorithms.utilities.AlgorithmChangeType;
import gov.nih.mipav.model.structures.*;

import java.io.*;
import java.util.zip.*;

import gov.nih.mipav.view.*;

/**
 
 */

public class FileMATLAB extends FileBase {
    // .mat file data types
	/** 8 bit, signed */
	private static final int miINT8 = 1;
	/** 8 bit, unsigned */
	private static final int miUINT8 = 2;
	/** 16-bit, signed */
	private static final int miINT16 = 3;
	/** 16-bit, unsigned */
	private static final int miUINT16 = 4;
	/** 32-bit signed */
	private static final int miINT32 = 5;
	/** 32-bit, unsigned */
	private static final int miUINT32 = 6;
	/** IEEE 754 single format */
	private static final int miSINGLE = 7;
	/** IEEE 754 double format */
	private static final int miDOUBLE = 9;
	/** 64-bit, signed */
	private static final int miINT64 = 12;
	/** 64-bit, unsigned */
	private static final int miUINT64 = 13;
	/** MATLAB ARRAY */
	private static final int miMATRIX = 14;
	/** Compressed Data */
	private static final int miCOMPRESSED = 15;
	/** Unicode UTF-8 Encoded Character Data */
	private static final int miUTF8 = 16;
	/** Unicode UTF-16 Encoded Character Data */
	private static final int miUTF16 = 17;
	/** Unicode UTF-32 Encoded Character Data */
	private static final int miUTF32 = 18;
	
	// MATLAB Array Types (Classes)
	/** Cell array */
	private static final byte mxCELL_CLASS = 1;
	/** Structure */
	private static final byte mxSTRUCT_CLASS = 2;
	/** Object */
	private static final byte mxOBJECT_CLASS = 3;
	/** Character array */
	private static final byte mxCHAR_CLASS = 4;
	/** Sparse array */
	private static final byte mxSPARSE_CLASS = 5;
	/** Double precision array */
	private static final byte mxDOUBLE_CLASS = 6;
	/** Single precision array */
	private static final byte mxSINGLE_CLASS = 7;
	/** 8-bit, signed integer */
	private static final byte mxINT8_CLASS = 8;
	/** 8-bit, unsigned integer */
	private static final byte mxUINT8_CLASS = 9;
	/** 16-bit, signed integer */
	private static final byte mxINT16_CLASS = 10;
	/** 16-bit, unsigned integer */
	private static final byte mxUINT16_CLASS = 11;
	/** 32-bit, signed integer */
	private static final byte mxINT32_CLASS = 12;
	/** 32-bit, unsigned integer */
	private static final byte mxUINT32_CLASS = 13;
	/** 64-bit, signed integer */
	private static final byte mxINT64_CLASS = 14;
	/** 64-bit, unsigned integer */
	private static final byte mxUINT64_CLASS = 15;
	
    

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** DOCUMENT ME! */
    private File file;

    /** DOCUMENT ME! */
    private String fileDir;


    /** DOCUMENT ME! */
    private FileInfoMATLAB fileInfo;
    
    private FileInfoMATLAB fileInfo2;
    
    private FileInfoMATLAB maskFileInfo;
    
    private FileInfoMATLAB maskFileInfo2;

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
    
    private Inflater zlibDecompresser = null;
    
    private ModelImage image2 = null;
    
    private ViewJFrameImage vFrame2 = null;
    
    private ModelImage maskImage = null;
    
    private ViewJFrameImage vmFrame = null;
    
    private ModelImage maskImage2 = null;
    
    private ViewJFrameImage vmFrame2 = null;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * LIFF reader/writer constructor.
     *
     * @param      fileName  file name
     * @param      fileDir   file directory
     *
     * @exception  IOException  if there is an error making the file
     */
    public FileMATLAB(String fileName, String fileDir) throws IOException {

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
        file = null;
        image = null;
        imageExtents = null;
        imgBuffer = null;
        imgResols = null;
        LUT = null;
        
        try {
            super.finalize();
        } catch (Throwable er) { }
    }
    
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
     * Reads the  MATLAB header.
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
        int i, j;
        int imageSlices = 1;
        int imageSlices2 = 1;
        byte firstEndianByte;
        byte secondEndianByte;
        String headerTextField;
        byte firstByte;
        byte secondByte;
        byte thirdByte;
        byte fourthByte;
        boolean level5Format;
        long subsystemSpecificDataOffset;
        int version;
        long nextElementAddress;
        int dataType;
        int elementBytes;
        int padBytes;
        byte buffer[] = null;
        String str;
        boolean isCompressed = false;
        int s;
        File ufile = null;
        byte[] decomp = null;
        int resultLength = 0;
        int arrayFlagsDataType;
        int arrayFlagsBytes;
        int arrayFlags;
        boolean complexFlag = false;
        @SuppressWarnings("unused")
        boolean globalFlag = false;
        boolean logicalFlag = false;
        int arrayClass;
        int dimensionsArrayDataType;
        int dimensionsArrayBytes;
        int nDim;
        int arrayNameDataType;
        int arrayNameBytes;
        String arrayName;
        int realDataType;
        int realDataBytes;
        int imaginaryDataType;
        int imaginaryDataBytes;
        short shortBuffer[] = null;
        int intBuffer[] = null;
        long longBuffer[] = null;
        float floatBuffer[] = null;
        float realBuffer[] = null;
        float imaginaryBuffer[] = null;
        double doubleBuffer[] = null;
        double realDBuffer[] = null;
        double imaginaryDBuffer[] = null;
        byte tBuffer[] = null;
        int imageLength = 1;
        int x;
        int y;
        String uncompressedName = null;
        int imagesFound = 0;
        int structureDimensions[];
        int maximumFieldNameLengthBytes;
        int maximumFieldNameLengthDataType;
        int maximumFieldNameLength;
        int fieldNamesDataType;
        int fieldNamesBytes;
        int fieldNumber = 1;
        String fieldNames[] = null;
        int bytesRead;
        int field;
        int numericArrayDataType;
        int numericArrayBytes;
        int numericArrayFlagsDataType;
        int numericArrayFlagsBytes;
        int numericArrayFlags;
        int numericArrayClass;
        int numericArrayNameDataType;
        int numericArrayNameBytes;
        String numericArrayName;
        int logicalFields = 0;
        boolean isVOI = false;
        boolean isVOI2 = false;
        int nonLogicalField = 0;
        int adjustedFieldDim;
        int newExtents[] = null;
        int totalNumber = 1;
        boolean logMagDisplay = Preferences.is(Preferences.PREF_LOGMAG_DISPLAY);
        int maskExtents[] = null;
        boolean haveSmallRealData;
        boolean haveSmallImaginaryData;
        String voiFieldName = null;
        String voi2FieldName = null;
        int sliceSize;
        int index;
        int shortNumber;
        int intNumber;
        int longNumber;
        int floatNumber;
        int doubleNumber;
        int b1;
        int b2;
        int b3;
        int b4;
        long b1L;
        long b2L;
        long b3L;
        long b4L;
        long b5L;
        long b6L;
        long b7L;
        long b8L;
        int tmpInt;
        long tmpLong;
        boolean isColor = false;
        int numberSlices;
        int st;
        int elementNumber = 0;
        int totalNumber2;
        int shortMin;
        int shortMax;
        int shortRange;
        int maxColorIndex;
        double scale;
        double scaledIndex;
        int lowIndex;
        int highIndex;
        double lowFraction;
        double highFraction;
        byte buf[] = null;
        int bufSize;
        boolean memoryError;

        try {
           
            imgResols[0] = imgResols[1] = imgResols[2] = imgResols[3] = imgResols[4] = (float) 1.0;
            file = new File(fileDir + fileName);
            raFile = new RandomAccessFile(file, "r");
            st = fileName.lastIndexOf(".");
            
            fileLength = raFile.length();
            Preferences.debug("fileLength = " + fileLength + "\n", Preferences.DEBUG_FILEIO);
            raFile.seek(126L);
            firstEndianByte = raFile.readByte();
            secondEndianByte = raFile.readByte();
            if ((firstEndianByte == 77) && (secondEndianByte == 73)) {
            	// M followed by I
            	endianess = FileBase.BIG_ENDIAN;
            	Preferences.debug("The MATLAB file is big endian\n", Preferences.DEBUG_FILEIO);
            }
            else if ((firstEndianByte == 73) && (secondEndianByte == 77)) {
            	// I followed by M
            	endianess = FileBase.LITTLE_ENDIAN;
            	Preferences.debug("The MATLAB file is little endian\n", Preferences.DEBUG_FILEIO);
            }
            else {
            	raFile.close();
            	throw new IOException("MATLAB Read Header: Endian Indicator at bytes 126 and 127 are illegal " + firstEndianByte + 
            			                 " followed by " + secondEndianByte);
            }
            
            fileInfo = new FileInfoMATLAB(fileName, fileDir, FileUtility.MATLAB); // dummy fileInfo
            fileInfo.setEndianess(endianess);
            
            raFile.seek(0L);
            firstByte = raFile.readByte();
            secondByte = raFile.readByte();
            thirdByte = raFile.readByte();
            fourthByte = raFile.readByte();
            if ((firstByte == 0) || (secondByte == 0) || (thirdByte == 0) || (fourthByte == 0)) {
            	 // MATLAB uses level 4 format
                 level5Format = false;	
                 Preferences.debug("The MATLAB file uses level 4 format\n", Preferences.DEBUG_FILEIO);
            }
            else {
            	// MATLAB uses level 5 format
            	level5Format = true;
            	Preferences.debug("The MATLAB file uses level 5 format\n", Preferences.DEBUG_FILEIO);
            }
            
            fileInfo.setLevel5Format(level5Format);
            
            raFile.seek(0L);
            
            headerTextField = getString(116);
            Preferences.debug("Header text field = " + headerTextField.trim() + "\n", Preferences.DEBUG_FILEIO);
            fileInfo.setHeaderTextField(headerTextField);
            
            // Location 116
            subsystemSpecificDataOffset = getLong(endianess);
            // All zeros or all spaces in this field indicate that there is no 
            // subsystem-specific data stored in this file
            if ((subsystemSpecificDataOffset == 0L) || (subsystemSpecificDataOffset == 0x2020202020202020L)) {
            	Preferences.debug("No subsystem specific data stored in file\n", Preferences.DEBUG_FILEIO);
            }
            else {
            	Preferences.debug("Subystem specific data stored at location " + subsystemSpecificDataOffset + "\n", 
            			Preferences.DEBUG_FILEIO);
            	fileInfo.setSubsystemSpecificDataOffset(subsystemSpecificDataOffset);
            }
            
            // Location 124
            version = getUnsignedShort(endianess);
            if (version == 256) {
                Preferences.debug("The version number is the expected 256\n", Preferences.DEBUG_FILEIO);	
            }
            else {
            	Preferences.debug("The version number = " + version + " instead of the expected 256\n", 
            			Preferences.DEBUG_FILEIO);
            }
            fileInfo.setVersion(version);
            
            // Go to first data element location
            nextElementAddress = 128L;
            while (nextElementAddress < fileLength) {
            	elementNumber++;
            	isCompressed = false;
                raFile.seek(nextElementAddress);
                dataType = getInt(endianess);
                if ((dataType & 0xffff0000) != 0) {
                	// Small Data Element Format
                	elementBytes = (dataType & 0xffff0000) >>> 16;
                	dataType = dataType & 0xffff;
                	nextElementAddress = nextElementAddress + 8;
                }
                else {
                    elementBytes = getInt(endianess);
                    // Must pad to make sure the tag of the next data element
                    // falls on a 64-bit boundary.
                    padBytes = 0;
                    if ((elementBytes % 8) != 0) {
                    	padBytes = 8 - (elementBytes % 8);
                    }
                    if (dataType == miCOMPRESSED) {
                        nextElementAddress = nextElementAddress + elementBytes + 8;
                    }
                    else {
                        nextElementAddress = nextElementAddress + elementBytes + padBytes + 8;
                    }
                }
                Preferences.debug("nextElementAddress = " + nextElementAddress + "\n", Preferences.DEBUG_FILEIO);
                if (dataType == miCOMPRESSED) {
                	fireProgressStateChanged("Decompressing element number " + String.valueOf(elementNumber));
                	if (elementNumber == 1) {
                		fireProgressStateChanged(25);
                	}
                	else if (elementNumber == 2) {
                		fireProgressStateChanged(60);
                	}
                	else {
                		fireProgressStateChanged(85);
                	}
                	isCompressed = true;
                	Preferences.debug("Data type = miCOMPRESSED\n", Preferences.DEBUG_FILEIO);
                	Preferences.debug("Bytes in data element = " + elementBytes + "\n", Preferences.DEBUG_FILEIO);
                	buffer = new byte[elementBytes];
                	raFile.read(buffer);
                	zlibDecompresser = new Inflater();
                	zlibDecompresser.setInput(buffer);
                	
                	// Create an expandable byte array to hand the decompressed data
                	ByteArrayOutputStream bos = new ByteArrayOutputStream(buffer.length);
                	
                	// Decompress the data
                	// Let buf be the smallest power of 2 which is at least 65536 and twice the uncompressed
                	// size so as to balance the need for speed against excessive memory use
                	// The maximum integer value is 2**31 -1, so limit size to 2**30.
                	// Cast elementBytes to long in while loop in case elementBytes >= 2**30.
                	bufSize = 65536;
                	while ((bufSize < 2*(long)elementBytes) && (bufSize < (int)Math.pow(2,30))){
                		bufSize *= 2;
                	}
                	memoryError = true;
                	while (memoryError) {
	                	try {
	                	     buf = new byte[bufSize];
	                	     memoryError = false;
	                	}
	                	catch (OutOfMemoryError e) {
	                	    bufSize = bufSize/2;
	                	    memoryError = true;
	                	}
                	}
                	try {
                		while (true) {
                			int count = zlibDecompresser.inflate(buf);
                			if (count > 0) {
                				bos.write(buf, 0 , count);
                			}
                			else if (count == 0 && zlibDecompresser.finished()) {
                	            break;
                			} else  {
                				throw new RuntimeException("bad zip data, size:" + buffer.length);
                			} 
                		}
                	} catch (Throwable t) {
                		throw new RuntimeException(t);
                	} finally {
                		zlibDecompresser.end();
                	}
                	// Get the decompressed data
                    decomp = bos.toByteArray();
                    resultLength = decomp.length;
                    uncompressedName = fileDir + fileName.substring(0, st) + "uncompressed.mat";
                    ufile = new File(uncompressedName);
                    raFile = new RandomAccessFile(ufile, "rw");
                    raFile.setLength(0);
                    raFile.write(decomp, 0, resultLength);
                    decomp = null;
                    raFile.seek(0L);
                    dataType = getInt(endianess);
                    if ((dataType & 0xffff0000) != 0) {
                    	// Small Data Element Format
                    	elementBytes = (dataType & 0xffff0000) >>> 16;
                    	dataType = dataType & 0xffff;
                    }
                    else {
                        elementBytes = getInt(endianess);
                    }
                } // if (dataTypeCOMPRESSED)
                fireProgressStateChanged("Reading element number " + String.valueOf(elementNumber));
                if (elementNumber == 1) {
                    fireProgressStateChanged(50);
                }
                else if (elementNumber == 2) {
                	fireProgressStateChanged(75);
                }
                else {
                	fireProgressStateChanged(85);
                }
                switch(dataType) {
                case miINT8:
                	Preferences.debug("Data type = miINT8\n", Preferences.DEBUG_FILEIO);
                	Preferences.debug("Bytes in data element = " + elementBytes + "\n", Preferences.DEBUG_FILEIO);
                	break;
                case miUINT8:
                	Preferences.debug("Data type = miUINT8\n", Preferences.DEBUG_FILEIO);
                	Preferences.debug("Bytes in data element = " + elementBytes + "\n", Preferences.DEBUG_FILEIO);
                	break;
                case miINT16:
                	Preferences.debug("Data type = miINT16\n", Preferences.DEBUG_FILEIO);
                	Preferences.debug("Bytes in data element = " + elementBytes + "\n", Preferences.DEBUG_FILEIO);
                	break;
                case miUINT16:
                	Preferences.debug("Data type = miUINT16\n", Preferences.DEBUG_FILEIO);
                	Preferences.debug("Bytes in data element = " + elementBytes + "\n", Preferences.DEBUG_FILEIO);
                	break;
                case miINT32:
                	Preferences.debug("Data type = miINT32\n", Preferences.DEBUG_FILEIO);
                	Preferences.debug("Bytes in data element = " + elementBytes + "\n", Preferences.DEBUG_FILEIO);
                	break;
                case miUINT32:
                	Preferences.debug("Data type = miUINT32\n", Preferences.DEBUG_FILEIO);
                	Preferences.debug("Bytes in data element = " + elementBytes + "\n", Preferences.DEBUG_FILEIO);
                	break;
                case miSINGLE:
                	Preferences.debug("Data type = miSINGLE\n", Preferences.DEBUG_FILEIO);
                	Preferences.debug("Bytes in data element = " + elementBytes + "\n", Preferences.DEBUG_FILEIO);
                	break;
                case miDOUBLE:
                	Preferences.debug("Data type = miDOUBLE\n", Preferences.DEBUG_FILEIO);
                	Preferences.debug("Bytes in data element = " + elementBytes + "\n", Preferences.DEBUG_FILEIO);
                	break;
                case miINT64:
                	Preferences.debug("Data type = miINT64\n", Preferences.DEBUG_FILEIO);
                	Preferences.debug("Bytes in data element = " + elementBytes + "\n", Preferences.DEBUG_FILEIO);
                	break;
                case miUINT64:
                	Preferences.debug("Data type = miUINT64\n", Preferences.DEBUG_FILEIO);
                	Preferences.debug("Bytes in data element = " + elementBytes + "\n", Preferences.DEBUG_FILEIO);
                	break;
                case miMATRIX:
                	Preferences.debug("Data type = miMATRIX\n", Preferences.DEBUG_FILEIO);
                	Preferences.debug("Bytes in data element = " + elementBytes + "\n", Preferences.DEBUG_FILEIO);
                	imageExtents = null;
                	imagesFound++;
                	logicalFields = 0;
                	arrayFlagsDataType = getInt(endianess);
                	if (arrayFlagsDataType == miUINT32) {
                		Preferences.debug("Array flags data type is the expected miUINT32\n", Preferences.DEBUG_FILEIO);
                	}
                	else {
                		Preferences.debug("Array flags data type is an unexpected " + arrayFlagsDataType + "\n", 
                				Preferences.DEBUG_FILEIO);
                	}
                    arrayFlagsBytes = getInt(endianess);
                    if (arrayFlagsBytes == 8) {
                    	Preferences.debug("Array flags byte length = 8 as expected\n", Preferences.DEBUG_FILEIO);
                    }
                    else {
                    	Preferences.debug("Array flags byte length is an unexpected " + arrayFlagsBytes + "\n", 
                    			Preferences.DEBUG_FILEIO);
                    }
                    arrayFlags = getInt(endianess);
                    arrayClass = arrayFlags & 0x000000ff;
                    switch(arrayClass) {
                    case mxCELL_CLASS:
                    	Preferences.debug("Array type is cell array\n", Preferences.DEBUG_FILEIO);
                    	break;
                    case mxSTRUCT_CLASS:
                    	Preferences.debug("Array type is structure\n", Preferences.DEBUG_FILEIO);
                    	break;
                    case mxOBJECT_CLASS:
                    	Preferences.debug("Array type is object\n", Preferences.DEBUG_FILEIO);
                    	break;
                    case mxCHAR_CLASS:
                    	Preferences.debug("Array type is character\n", Preferences.DEBUG_FILEIO);
                    	break;
                    case mxSPARSE_CLASS:
                    	Preferences.debug("Array type is sparse\n", Preferences.DEBUG_FILEIO);
                    	break;
                    case mxDOUBLE_CLASS:
                    	Preferences.debug("Array type is 8 byte double\n", Preferences.DEBUG_FILEIO);
                    	break;
                    case mxSINGLE_CLASS:
                    	Preferences.debug("Array type is 4 byte float\n", Preferences.DEBUG_FILEIO);
                    	break;
                    case mxINT8_CLASS:
                    	Preferences.debug("Array type is signed byte\n", Preferences.DEBUG_FILEIO);
                    	break;
                    case mxUINT8_CLASS:
                    	Preferences.debug("Array type is unsigned byte\n", Preferences.DEBUG_FILEIO);
                    	break;
                    case mxINT16_CLASS:
                    	Preferences.debug("Array type is signed short\n", Preferences.DEBUG_FILEIO);
                    	break;
                    case mxUINT16_CLASS:
                    	Preferences.debug("Array type is unsigned short\n", Preferences.DEBUG_FILEIO);
                    	break;
                    case mxINT32_CLASS:
                        Preferences.debug("Array type is signed integer\n", Preferences.DEBUG_FILEIO);
                        break;
                    case mxUINT32_CLASS:
                    	Preferences.debug("Array type is unsigned integer\n", Preferences.DEBUG_FILEIO);
                    	break;
                    case mxINT64_CLASS:
                    	Preferences.debug("Array type is signed long\n", Preferences.DEBUG_FILEIO);
                    	break;
                    case mxUINT64_CLASS:
                    	Preferences.debug("Array type is unsigned long\n", Preferences.DEBUG_FILEIO);
                    	break;
                    default:
                    	Preferences.debug("Array type is an illegal = " + arrayClass + "\n", Preferences.DEBUG_FILEIO);
                    }
                    if (arrayClass == mxCHAR_CLASS) {
                    	imagesFound--;
                    	if (isCompressed) {
                        	raFile.close();
        	                try {
        	                    ufile.delete();
        	                } catch (final SecurityException sc) {
        	                    MipavUtil.displayError("Security error occurs while trying to delete " + uncompressedName);
        	                }
                        	raFile = new RandomAccessFile(file, "r");
                        }
                    	continue;
                    }
                    if (imagesFound == 2) {
                    	fileInfo2 = new FileInfoMATLAB(fileName + "_2", fileDir, FileUtility.MATLAB); // dummy fileInfo
                        fileInfo2.setEndianess(endianess);
                        fileInfo2.setLevel5Format(level5Format);
                        fileInfo2.setHeaderTextField(headerTextField);
                        fileInfo2.setVersion(version);
                    }
                    if ((arrayFlags & 0x00000800) != 0) {
                    	complexFlag = true;
                    	Preferences.debug("Complex flag is set\n", Preferences.DEBUG_FILEIO);
                    }
                    else {
                    	complexFlag = false;
                    	Preferences.debug("Complex flag is not set\n", Preferences.DEBUG_FILEIO);
                    }
                    if ((arrayFlags & 0x00000400) != 0) {
                    	globalFlag = true;
                    	Preferences.debug("Global flag is set\n", Preferences.DEBUG_FILEIO);
                    }
                    else {
                    	globalFlag = false;
                    	Preferences.debug("Global flag is not set\n", Preferences.DEBUG_FILEIO);
                    }
                    if ((arrayFlags & 0x00000200) != 0) {
                    	logicalFlag = true;
                    	Preferences.debug("Logical flag is set\n", Preferences.DEBUG_FILEIO);
                    }
                    else {
                    	logicalFlag = false;
                    	Preferences.debug("Logical flag is not set\n", Preferences.DEBUG_FILEIO);
                    }
                    // 4 undefined bytes
                	getInt(endianess);
                	dimensionsArrayDataType = getInt(endianess);
                	if (dimensionsArrayDataType == miINT32) {
                		Preferences.debug("Dimensions array data type is the expected miINT32\n", Preferences.DEBUG_FILEIO);
                	}
                	else {
                		Preferences.debug("Dimensions array data type is an unexpected " + dimensionsArrayDataType + "\n", 
                				Preferences.DEBUG_FILEIO);
                	}
                	dimensionsArrayBytes = getInt(endianess);
                	Preferences.debug("dimensionsArrayBytes = " + dimensionsArrayBytes + "\n", Preferences.DEBUG_FILEIO);
                	if ((dimensionsArrayBytes % 4) == 0) {
                		Preferences.debug("dimensionsArrayBytes is a multiple of 4 as expected\n", Preferences.DEBUG_FILEIO);
                	}
                	else {
                		Preferences.debug("dimensionArrayBytes is unexpectedly not a multiple of 4\n", 
                				Preferences.DEBUG_FILEIO);
                	}
                	nDim = dimensionsArrayBytes/4;
                	Preferences.debug("Number of dimensions = " + nDim + "\n", Preferences.DEBUG_FILEIO);
                	if (nDim < 2) {
                		Preferences.debug("Error! All numeric arrays should have at least 2 dimensions\n",
                				Preferences.DEBUG_FILEIO);
                	}
                	if (arrayClass == mxSTRUCT_CLASS) {
                		structureDimensions = new int[nDim];
                	    for (i = 0; i < nDim; i++) {
                	    	// Ignore structure dimensions
                	    	structureDimensions[i] = getInt(endianess);
                	    	Preferences.debug("Ignored structureDimensions[" + i + " ] = " + structureDimensions[i] + "\n", 
                	    			Preferences.DEBUG_FILEIO);
                	    }
                	}
                	else { // arrayClass != mxSTRUCT_CLASS
	                	imageExtents = new int[nDim];
	                	imageLength = 1;
	                	if (imagesFound == 1) {
	                		imageSlices = 1;
	                	}
	                	else {
	                		imageSlices2 = 1;
	                	}
	                	for (i = 0; i < nDim; i++) {
	                		if (i == 0) {
	                			imageExtents[1] = getInt(endianess);
	                			Preferences.debug("imageExtents[1] = " + imageExtents[1] + "\n", Preferences.DEBUG_FILEIO);
	                		}
	                		else if (i == 1) {
	                			imageExtents[0] = getInt(endianess);
	                			Preferences.debug("imageExtents[0] = " + imageExtents[0] + "\n", Preferences.DEBUG_FILEIO);
	                		}
	                		else {
	                		    imageExtents[i] = getInt(endianess);
	                		    Preferences.debug("imageExtents["+ i + "] = " + imageExtents[i] + "\n", 
	                		    		Preferences.DEBUG_FILEIO);
	                		}
	                		imageLength = imageLength * imageExtents[i];
	                		if (i > 1) {
	                			if (imagesFound == 1) {
	                				imageSlices = imageSlices * imageExtents[i];
	                			}
	                			else {
	                				imageSlices2 = imageSlices2 * imageExtents[i];
	                			}
	                		}
	                	}
	                	if ((imageExtents[0] == 1) || (imageExtents[1] == 1)) {
	                		imagesFound--;
	                    	if (isCompressed) {
	                        	raFile.close();
	        	                try {
	        	                    ufile.delete();
	        	                } catch (final SecurityException sc) {
	        	                    MipavUtil.displayError("Security error occurs while trying to delete " + uncompressedName);
	        	                }
	                        	raFile = new RandomAccessFile(file, "r");
	                        }
	                    	continue;	
	                	}
	                	if ((nDim == 4) && (imageExtents[2] == 1)) {
	                		nDim = 3;
	                		newExtents = new int[3];
	                		newExtents[0] = imageExtents[0];
	                		newExtents[1] = imageExtents[1];
	                		newExtents[2] = imageExtents[3];
	                		imageExtents = new int[3];
	                		imageExtents[0] = newExtents[0];
	                		imageExtents[1] = newExtents[1];
	                		imageExtents[2] = newExtents[2];
	                	}
	                	if (imagesFound == 1) {
	                		fileInfo.setExtents(imageExtents);
	                	}
	                	else {
	                		fileInfo2.setExtents(imageExtents);
	                	}
                	} // else arrayClass != mxSTRUCT_CLASS
                	if ((dimensionsArrayBytes % 8) != 0) {
                		// Skip over padding bytes
                		padBytes = 8 - (dimensionsArrayBytes % 8);
                		for (i = 0; i < padBytes; i++) {
                		    raFile.readByte();
                		}
                	} // if ((dimensionsArrayBytes % 8) != 0)
                	arrayNameDataType = getInt(endianess);
                    if ((arrayNameDataType & 0xffff0000) != 0) {
                        // Small data element format    
                    	arrayNameBytes = (arrayNameDataType & 0xffff0000) >>> 16;
                    	arrayNameDataType = arrayNameDataType & 0xffff;
                    	arrayName = getString(arrayNameBytes);
                    	if (arrayNameBytes < 4) {
                    		for (i = 0; i < 4 - arrayNameBytes; i++) {
                    			// Skip over padding bytes
                    			raFile.readByte();
                    		}
                    	}
                    }
                    else {
                    	arrayNameBytes = getInt(endianess);
                    	arrayName = getString(arrayNameBytes);
                    	// Skip over padding bytes
                    	if ((arrayNameBytes % 8) != 0) {
	                		padBytes = 8 - (arrayNameBytes % 8);
	                		for (i = 0; i < padBytes; i++) {
	                		    raFile.readByte();
	                		}
                    	}
                    }
                    Preferences.debug("Array name = " + arrayName + "\n", Preferences.DEBUG_FILEIO);
                    if (imagesFound == 1) {
                	    fileInfo.setArrayName(arrayName);
                	}
                	else {
                        fileInfo2.setArrayName(arrayName);
                	}
                    if (arrayClass == mxSTRUCT_CLASS) {
                      // The field name length subelement always uses the compressed data element format
                    	maximumFieldNameLengthDataType = getInt(endianess);
                    	maximumFieldNameLengthBytes = (maximumFieldNameLengthDataType & 0xffff0000) >>> 16;
                    	maximumFieldNameLengthDataType = maximumFieldNameLengthDataType & 0xffff;
                    	if (maximumFieldNameLengthDataType == miINT32) {
                    		Preferences.debug("maximumFieldNameLengthDataType == miINT32 as expected\n", 
                    				Preferences.DEBUG_FILEIO);
                    	}
                    	else {
                    		Preferences.debug("maximumFieldNameLengthDataType unexpectedly == " + 
                    				maximumFieldNameLengthDataType + "\n", Preferences.DEBUG_FILEIO);
                    	}
                    	if (maximumFieldNameLengthBytes == 4) {
                    		Preferences.debug("maximumFieldNameLengthBytes == 4 as expected\n", Preferences.DEBUG_FILEIO);
                    	}
                    	else {
                    		Preferences.debug("maximumFieldNameLengthBytes == " + maximumFieldNameLengthBytes +
                    				          " instead of the expected 4\n", Preferences.DEBUG_FILEIO);
                    	}
                    	maximumFieldNameLength = getInt(endianess);
                    	Preferences.debug("maximumFieldNameLength including null terminator = " + 
                    			maximumFieldNameLength + "\n", Preferences.DEBUG_FILEIO);
                    	if (maximumFieldNameLength > 32) {
                    		Preferences.debug("maximumFieldNameLength should not be greater than 32\n", 
                    				Preferences.DEBUG_FILEIO);
                    	}
                    	fieldNamesDataType = getInt(endianess);
                    	if (fieldNamesDataType == miINT8) {
                    		Preferences.debug("fieldNamesDataType == miINT8 as expected\n", Preferences.DEBUG_FILEIO);
                    	}
                    	else {
                    		Preferences.debug("fieldNamesDataType unexpectely == " + fieldNamesDataType + "\n", 
                    				Preferences.DEBUG_FILEIO);
                    	}
                    	fieldNamesBytes = getInt(endianess);
                    	Preferences.debug("fieldNamesBytes = " + fieldNamesBytes + "\n", Preferences.DEBUG_FILEIO);
                    	if ((fieldNamesBytes % maximumFieldNameLength) == 0) {
                    		Preferences.debug("fieldNamesBytes % maximumFieldNameLength == 0 as expected\n", 
                    				Preferences.DEBUG_FILEIO);
                    	}
                    	else {
                    		Preferences.debug("fieldNamesBytes % maximumFieldNameLength unexpectedly == " +
                    				(fieldNamesBytes % maximumFieldNameLength) + "\n", Preferences.DEBUG_FILEIO);
                    	}
                    	fieldNumber = fieldNamesBytes / maximumFieldNameLength;
                    	Preferences.debug("Field number = " + fieldNumber + "\n", Preferences.DEBUG_FILEIO);
                    	fieldNames = new String[fieldNumber];
                    	for (i = 0; i < fieldNumber; i++) {
                    	    fieldNames[i] = readCString();
                    	    Preferences.debug("field name " + i + " = " + fieldNames[i] + "\n", Preferences.DEBUG_FILEIO);
                    	    bytesRead = fieldNames[i].length() + 1;
                    	    padBytes = maximumFieldNameLength - bytesRead;
                    	    for (j = 0; j < padBytes; j++) {
                    	    	raFile.readByte();
                    	    }
                    	}
                    	if (imagesFound == 1) {
                    		fileInfo.setFieldNames(fieldNames);
                    	}
                    	else {
                    		fileInfo2.setFieldNames(fieldNames);
                    	}
                    	if ((fieldNamesBytes % 8) != 0) {
                    	    padBytes = 8 - (fieldNamesBytes % 8);
                    	    for (i = 0; i < padBytes; i++) {
                    	    	raFile.readByte();
                    	    }
                    	}
                    } // if (arrayClass == mxSTRUCT_CLASS)
                    for (field = 0; field < fieldNumber; field++) {
                    if (arrayClass == mxSTRUCT_CLASS) {
                    	Preferences.debug("Reading numeric array number " + field + "\n", Preferences.DEBUG_FILEIO);
                        numericArrayDataType = getInt(endianess);
                        if (numericArrayDataType == miMATRIX) {
                        	Preferences.debug("Numeric array data type == miMATRIX as expected\n");
                        }
                        else {
                        	Preferences.debug("Numeric array data type unexpectedly == " + numericArrayDataType + "\n", 
                        			Preferences.DEBUG_FILEIO);
                        }
                        numericArrayBytes= getInt(endianess);
                        Preferences.debug("Numeric array bytes = " + numericArrayBytes + "\n", Preferences.DEBUG_FILEIO);
                        numericArrayFlagsDataType = getInt(endianess);
                    	if (arrayFlagsDataType == miUINT32) {
                    		Preferences.debug("Numeric array flags data type is the expected miUINT32\n",
                    				Preferences.DEBUG_FILEIO);
                    	}
                    	else {
                    		Preferences.debug("Numeric array flags data type is an unexpected " + numericArrayFlagsDataType +
                    				"\n", Preferences.DEBUG_FILEIO);
                    	}
                        numericArrayFlagsBytes = getInt(endianess);
                        if (numericArrayFlagsBytes == 8) {
                        	Preferences.debug("Numeric array flags byte length = 8 as expected\n", Preferences.DEBUG_FILEIO);
                        }
                        else {
                        	Preferences.debug("Numeric array flags byte length is an unexpected " + numericArrayFlagsBytes +
                        			"\n", Preferences.DEBUG_FILEIO);
                        }
                        numericArrayFlags = getInt(endianess);
                        numericArrayClass = numericArrayFlags & 0x000000ff;
                        switch(numericArrayClass) {
                        case mxCELL_CLASS:
                        	Preferences.debug("Numeric array type is cell array\n", Preferences.DEBUG_FILEIO);
                        	break;
                        case mxSTRUCT_CLASS:
                        	Preferences.debug("Numeric array type is structure\n", Preferences.DEBUG_FILEIO);
                        	break;
                        case mxOBJECT_CLASS:
                        	Preferences.debug("Numeric array type is object\n", Preferences.DEBUG_FILEIO);
                        	break;
                        case mxCHAR_CLASS:
                        	Preferences.debug("Numeric array type is character\n", Preferences.DEBUG_FILEIO);
                        	break;
                        case mxSPARSE_CLASS:
                        	Preferences.debug("Numereic array type is sparse\n", Preferences.DEBUG_FILEIO);
                        	break;
                        case mxDOUBLE_CLASS:
                        	Preferences.debug("Numeric array type is 8 byte float\n", Preferences.DEBUG_FILEIO);
                        	break;
                        case mxSINGLE_CLASS:
                        	Preferences.debug("Numeric array type is 4 byte float\n", Preferences.DEBUG_FILEIO);
                        	break;
                        case mxINT8_CLASS:
                        	Preferences.debug("Numeric array type is signed byte\n", Preferences.DEBUG_FILEIO);
                        	break;
                        case mxUINT8_CLASS:
                        	Preferences.debug("Numeric array type is unsigned byte\n", Preferences.DEBUG_FILEIO);
                        	break;
                        case mxINT16_CLASS:
                        	Preferences.debug("Numeric array type is signed short\n", Preferences.DEBUG_FILEIO);
                        	break;
                        case mxUINT16_CLASS:
                        	Preferences.debug("Numeric array type is unsigned short\n", Preferences.DEBUG_FILEIO);
                        	break;
                        case mxINT32_CLASS:
                            Preferences.debug("Numeric array type is signed integer\n", Preferences.DEBUG_FILEIO);
                            break;
                        case mxUINT32_CLASS:
                        	Preferences.debug("Numeric array type is unsigned integer\n", Preferences.DEBUG_FILEIO);
                        	break;
                        default:
                        	Preferences.debug("Numeric array type is an illegal = " + numericArrayClass + "\n", 
                        			Preferences.DEBUG_FILEIO);
                        }
                        
                        if ((numericArrayFlags & 0x00000800) != 0) {
                        	complexFlag = true;
                        	Preferences.debug("Complex flag is set\n", Preferences.DEBUG_FILEIO);
                        }
                        else {
                        	complexFlag = false;
                        	Preferences.debug("Complex flag is not set\n", Preferences.DEBUG_FILEIO);
                        }
                        if ((numericArrayFlags & 0x00000400) != 0) {
                        	globalFlag = true;
                        	Preferences.debug("Global flag is set\n", Preferences.DEBUG_FILEIO);
                        }
                        else {
                        	globalFlag = false;
                        	Preferences.debug("Global flag is not set\n", Preferences.DEBUG_FILEIO);
                        }
                        if ((numericArrayFlags & 0x00000200) != 0) {
                        	logicalFlag = true;
                        	Preferences.debug("Logical flag is set\n", Preferences.DEBUG_FILEIO);
                        	logicalFields++;
                        	if (imagesFound == 1) {
                        	    isVOI = true;
                        	}
                        	else {
                        		isVOI2 = true;
                        	}
                        }
                        else {
                        	logicalFlag = false;
                        	Preferences.debug("Logical flag is not set\n", Preferences.DEBUG_FILEIO);
                        	if (imagesFound == 1) {
                        	    isVOI = false;
                        	}
                        	else {
                        		isVOI2 = false;
                        	}
                        }
                        nonLogicalField = field - logicalFields;
                        if ((imagesFound == 1) && (fieldNumber == 2) && (fieldNames != null) && logicalFlag) {
                        	if (field == 0) {
                        		voiFieldName = fieldNames[0];
                        	}
                        	else {
                        		voiFieldName = fieldNames[1];
                        	}
                        }
                        if ((imagesFound == 2) && (fieldNumber == 2) && (fieldNames != null) && logicalFlag) {
                        	if (field == 0) {
                        		voi2FieldName = fieldNames[0];
                        	}
                        	else {
                        		voi2FieldName = fieldNames[1];
                        	}
                        }
                        
                        // 4 undefined bytes
                    	getInt(endianess);
                    	dimensionsArrayDataType = getInt(endianess);
                    	if (dimensionsArrayDataType == miINT32) {
                    		Preferences.debug("Dimensions array data type is the expected miINT32\n", Preferences.DEBUG_FILEIO);
                    	}
                    	else {
                    		Preferences.debug("Dimensions array data type is an unexpected " + dimensionsArrayDataType + "\n", 
                    				Preferences.DEBUG_FILEIO);
                    	}
                    	dimensionsArrayBytes = getInt(endianess);
                    	Preferences.debug("dimensionsArrayBytes = " + dimensionsArrayBytes + "\n", Preferences.DEBUG_FILEIO);
                    	if ((dimensionsArrayBytes % 4) == 0) {
                    		Preferences.debug("dimensionsArrayBytes is a multiple of 4 as expected\n", 
                    				Preferences.DEBUG_FILEIO);
                    	}
                    	else {
                    		Preferences.debug("dimensionArrayBytes is unexpectedly not a multiple of 4\n", 
                    				Preferences.DEBUG_FILEIO);
                    	}
                    	nDim = dimensionsArrayBytes/4;
                    	Preferences.debug("Number of dimensions = " + nDim + "\n", Preferences.DEBUG_FILEIO);
                    	if (nDim < 2) {
                    		Preferences.debug("Error! All numeric arrays should have at least 2 dimensions\n", 
                    				Preferences.DEBUG_FILEIO);
                    	}
                    	
                    	if (logicalFlag) {
                    		// MATLAB function used to create VOIs (roipoly) always returns a 2D mask
                    		maskExtents = new int[nDim];
                    		for (i = 0; i < nDim; i++) {
                    			maskExtents[i] = getInt(endianess);
                    		}
                    		if (imageExtents != null) {
	                    		for (i = 0; i < 2; i++) {
	                    		    if (imageExtents[i] != maskExtents[i]) {
	                    		    	if (imagesFound == 1) {
	                    		    	    isVOI = false;
	                    		    	}
	                    		    	else {
	                    		    		isVOI2 = false;
	                    		    	}
	                    		    }
	                    		}
                    		}
                    		if (imagesFound == 1) {
                    			maskFileInfo = new FileInfoMATLAB(fileName + "_mask", fileDir, FileUtility.MATLAB); // dummy fileInfo
                                maskFileInfo.setEndianess(endianess);
                                maskFileInfo.setLevel5Format(level5Format);
                                maskFileInfo.setHeaderTextField(headerTextField);
                                maskFileInfo.setVersion(version);
                                maskFileInfo.setExtents(maskExtents);
                                maskFileInfo.setArrayName(arrayName);
                    		}
                    		else {
                    			maskFileInfo2 = new FileInfoMATLAB(fileName + "_2_mask", fileDir, FileUtility.MATLAB); // dummy fileInfo
                                maskFileInfo2.setEndianess(endianess);
                                maskFileInfo2.setLevel5Format(level5Format);
                                maskFileInfo2.setHeaderTextField(headerTextField);
                                maskFileInfo2.setVersion(version);
                                maskFileInfo2.setExtents(maskExtents);
                                maskFileInfo2.setArrayName(arrayName);	
                    		}
                    	} // if (logicalFlag)
                    	else { // !logicalFlag
	                    	imageExtents = new int[nDim+1];
		                	imageLength = 1;
		                	if (imagesFound == 1) {
		                		imageSlices = 1;
		                	}
		                	else {
		                		imageSlices2 = 1;
		                	}
		                	for (i = 0; i < nDim; i++) {
		                		if (i == 0) {
		                			imageExtents[1] = getInt(endianess);
		                			Preferences.debug("imageExtents[1] = " + imageExtents[1] + "\n", Preferences.DEBUG_FILEIO);
		                		}
		                		else if (i == 1) {
		                			imageExtents[0] = getInt(endianess);
		                			Preferences.debug("imageExtents[0] = " + imageExtents[0] + "\n", Preferences.DEBUG_FILEIO);
		                		}
		                		else {
		                		    imageExtents[i] = getInt(endianess);
		                		    Preferences.debug("imageExtents["+ i + "] = " + imageExtents[i] + "\n", 
		                		    		Preferences.DEBUG_FILEIO);
		                		}
		                		imageLength = imageLength * imageExtents[i];
		                		
		                		if (i > 1) {
		                			if (imagesFound == 1) {
		                				imageSlices = imageSlices * imageExtents[i];
		                			}
		                			else {
		                				imageSlices2 = imageSlices2 * imageExtents[i];
		                			}
		                		}
		                	}
		                	
		                	
		                	// Note that imageLength only includes slices in one field of a structure
		                	imageExtents[nDim] = fieldNumber;
		                	Preferences.debug("imageExtents[" + nDim + "] = " + imageExtents[nDim] + "\n", 
		                			Preferences.DEBUG_FILEIO);
		                	if (nDim > 1) {
		                		if (imagesFound == 1) {
		                			imageSlices = imageSlices * fieldNumber;
		                		}
		                		else {
		                			imageSlices2 = imageSlices2 * fieldNumber;
		                		}
		                	}
		                	if (imagesFound == 1) {
		                		fileInfo.setExtents(imageExtents);
		                	}
		                	else {
		                		fileInfo2.setExtents(imageExtents);
		                	}
                    	} // else !logicalFlag
	                	
	                	if ((dimensionsArrayBytes % 8) != 0) {
	                		// Skip over padding bytes
	                		padBytes = 8 - (dimensionsArrayBytes % 8);
	                		for (i = 0; i < padBytes; i++) {
	                		    raFile.readByte();
	                		}
	                	} // if ((dimensionsArrayBytes % 8) != 0)
	                	
	                	numericArrayNameDataType = getInt(endianess);
	                    if ((numericArrayNameDataType & 0xffff0000) != 0) {
	                        // Small data element format    
	                    	numericArrayNameBytes = (numericArrayNameDataType & 0xffff0000) >>> 16;
	                    	numericArrayNameDataType = numericArrayNameDataType & 0xffff;
	                    	numericArrayName = getString(numericArrayNameBytes);
	                    	if (numericArrayNameBytes < 4) {
	                    		for (i = 0; i < 4 - numericArrayNameBytes; i++) {
	                    			// Skip over padding bytes
	                    			raFile.readByte();
	                    		}
	                    	}
	                    }
	                    else {
	                    	numericArrayNameBytes = getInt(endianess);
	                    	numericArrayName = getString(numericArrayNameBytes);
	                    	// Skip over padding bytes
	                    	if (numericArrayNameBytes > 0) {
	                		    padBytes = 8 - (numericArrayNameBytes % 8);
	                		    for (i = 0; i < padBytes; i++) {
	                		       raFile.readByte();
	                		    }
	                    	}
	                    }
	                    Preferences.debug("Numeric array name = " + numericArrayName + "\n", Preferences.DEBUG_FILEIO);
                    } // if (arrayClass == mxSTRUCT_CLASS)
                    realDataType = getInt(endianess);
                    if ((realDataType & 0xffff0000) != 0) {
                        // Small data element format    
                    	realDataBytes = (realDataType & 0xffff0000) >>> 16;
                    	realDataType = realDataType & 0xffff;
                    	haveSmallRealData = true;
                    }
                    else {
                        realDataBytes = getInt(endianess);
                        haveSmallRealData = false;
                    }
                    if ((nDim >= 3) && (imageExtents[nDim - 1] == 3) && (!logicalFlag) && (!complexFlag) &&
                    		((realDataType == miUINT8) || (realDataType == miUINT16) || (realDataType == miDOUBLE)) &&
                    		(nonLogicalField == 0)) {
                    	newExtents = new int[nDim-1];
                    	for (i = 0; i < nDim-1; i++) {
                    		newExtents[i] = imageExtents[i];
                    	}
                    	imageExtents = new int[nDim-1];
                    	for (i = 0; i < nDim-1; i++) {
                    		imageExtents[i] = newExtents[i];
                    	}
                    	if (imagesFound == 1) {
                			imageSlices = imageSlices/3;
                			fileInfo.setExtents(imageExtents);
                		}
                		else {
                			imageSlices2 = imageSlices2/3;
                			fileInfo2.setExtents(imageExtents);
                		}
                    	isColor = true;
                    }
                    else {
                    	isColor = false;
                    }
                    sliceSize = imageExtents[0] * imageExtents[1];
                    if (imagesFound == 1) {
                    switch(realDataType) {
                    case miINT8:
                    	Preferences.debug("Real data type = miINT8\n", Preferences.DEBUG_FILEIO);
                    	Preferences.debug("Real data bytes = " + realDataBytes + "\n", Preferences.DEBUG_FILEIO);
                    	if (logicalFlag) {
                    	    if (logicalFields == 1) {
	                    	    maskImage = new ModelImage(ModelStorageBase.BYTE, maskExtents, fileName + "_mask");
	                    	    maskFileInfo.setDataType(ModelStorageBase.BYTE);
	                    	    buffer = new byte[realDataBytes];
		                    	raFile.read(buffer);
		                    	tBuffer = new byte[buffer.length];
	                    		numberSlices = buffer.length/sliceSize;
		                    	j = 0;
		                    	for (s = 0; s < numberSlices; s++) {
			                    	for (x = 0; x < imageExtents[1]; x++) {
			                    		for (y = 0; y < imageExtents[0]; y++) {
                                            tBuffer[j++] = buffer[x + imageExtents[1] * y + s * sliceSize];			                    			
			                    		}
			                    	}
		                    	}
		                    	if (haveSmallRealData) {
	                    		    if (realDataBytes < 4) {
	                    		    	padBytes = 4 - realDataBytes;
	                    		    	for (i = 0; i < padBytes; i++) {
	                    		    		raFile.readByte();
	                    		    	}
	                    		    }
	                    		}
	                    		else if ((realDataBytes % 8) != 0) {
	                    	    	padBytes = 8 - (realDataBytes % 8);
	                    	    	for (i = 0; i < padBytes; i++) {
	                        	    	raFile.readByte();
	                        	    }
	                    	    }  
		                    	try {
		                			maskImage.importData(0, tBuffer, true);
		                		}
		                		catch(IOException e) {
		                		   MipavUtil.displayError("IOException on maskImage.importData(0, tBuffer, true)");
		                		   throw e;
		                		}
                    	    } // if (logicalFields == 1)
                    	}
                    	else if (!complexFlag) {
                    		if (nonLogicalField == 0) {
	                    	    image = new ModelImage(ModelStorageBase.BYTE, imageExtents, fileName); 
	                    	    fileInfo.setDataType(ModelStorageBase.BYTE);
                    		}
                    		buffer = new byte[realDataBytes];
	                    	raFile.read(buffer);
	                    	tBuffer = new byte[buffer.length];
                    		numberSlices = buffer.length/sliceSize;
                    		j = 0;
	                    	for (s = 0; s < numberSlices; s++) {
		                    	for (x = 0; x < imageExtents[1]; x++) {
		                    		for (y = 0; y < imageExtents[0]; y++) {
                                        tBuffer[j++] = buffer[x + imageExtents[1] * y + s * sliceSize];			                    			
		                    		}
		                    	}
	                    	}
	                    	if (haveSmallRealData) {
                    		    if (realDataBytes < 4) {
                    		    	padBytes = 4 - realDataBytes;
                    		    	for (i = 0; i < padBytes; i++) {
                    		    		raFile.readByte();
                    		    	}
                    		    }
                    		}
                    		else if ((realDataBytes % 8) != 0) {
                    	    	padBytes = 8 - (realDataBytes % 8);
                    	    	for (i = 0; i < padBytes; i++) {
                        	    	raFile.readByte();
                        	    }
                    	    }  
	                    	try {
	                			image.importData(nonLogicalField * tBuffer.length, tBuffer, true);
	                		}
	                		catch(IOException e) {
	                		   MipavUtil.displayError("IOException on image.importData(nonLogicalField* tBuffer.length, tBuffer, true)");
	                		   throw e;
	                		}
                    	}
                    	else {
                    		if (nonLogicalField == 0) {
                    		    image = new ModelImage(ModelStorageBase.COMPLEX, imageExtents, fileName); 
                    		    fileInfo.setDataType(ModelStorageBase.COMPLEX);
                    		}
                    		buffer = new byte[realDataBytes];
                    		raFile.read(buffer);
                    		realBuffer = new float[realDataBytes];
                    		numberSlices = buffer.length/sliceSize;
                    		j = 0;
	                    	for (s = 0; s < numberSlices; s++) {
		                    	for (x = 0; x < imageExtents[1]; x++) {
		                    		for (y = 0; y < imageExtents[0]; y++) {
                                        realBuffer[j++] = (float)buffer[x + imageExtents[1] * y + s * sliceSize];			                    			
		                    		}
		                    	}
	                    	}
                    		
                    		if (haveSmallRealData) {
                    		    if (realDataBytes < 4) {
                    		    	padBytes = 4 - realDataBytes;
                    		    	for (i = 0; i < padBytes; i++) {
                    		    		raFile.readByte();
                    		    	}
                    		    }
                    		}
                    		else if ((realDataBytes % 8) != 0) {
                    	    	padBytes = 8 - (realDataBytes % 8);
                    	    	for (i = 0; i < padBytes; i++) {
                        	    	raFile.readByte();
                        	    }
                    	    }   
                    	    imaginaryDataType = getInt(endianess);
                    	    if ((imaginaryDataType & 0xffff0000) != 0) {
                                // Small data element format    
                            	imaginaryDataBytes = (imaginaryDataType & 0xffff0000) >>> 16;
                            	imaginaryDataType = imaginaryDataType & 0xffff;
                            	haveSmallImaginaryData = true;
                            }
                            else {
                                imaginaryDataBytes = getInt(endianess);
                                haveSmallImaginaryData = false;
                            }
                    	    if (imaginaryDataType == miINT8) {
                    	    	Preferences.debug("imaginaryDataType == miINT8 as expected\n", Preferences.DEBUG_FILEIO);
                    	    }
                    	    else {
                    	    	Preferences.debug("imaginaryDataType unexpectedly == " + imaginaryDataType + "\n",
                    	    			Preferences.DEBUG_FILEIO);
                    	    }
          
                            if (imaginaryDataBytes == realDataBytes) {
                            	Preferences.debug("imaginaryDataBytes == realDataBytes as expected\n", 
                            			Preferences.DEBUG_FILEIO);
                            }
                            else {
                            	Preferences.debug("imaginaryDataBytes unexpectedly != realDataBytes\n", 
                            			Preferences.DEBUG_FILEIO);
                            }
                            raFile.read(buffer);
                            imaginaryBuffer = new float[imaginaryDataBytes];
                            j = 0;
	                    	for (s = 0; s < numberSlices; s++) {
		                    	for (x = 0; x < imageExtents[1]; x++) {
		                    		for (y = 0; y < imageExtents[0]; y++) {
                                        imaginaryBuffer[j++] = (float)buffer[x + imageExtents[1] * y + s * sliceSize];			                    			
		                    		}
		                    	}
	                    	}
                            
                            if (haveSmallImaginaryData) {
                    		    if (imaginaryDataBytes < 4) {
                    		    	padBytes = 4 - imaginaryDataBytes;
                    		    	for (i = 0; i < padBytes; i++) {
                    		    		raFile.readByte();
                    		    	}
                    		    }
                    		}
                    		else if ((imaginaryDataBytes % 8) != 0) {
                    	    	padBytes = 8 - (imaginaryDataBytes % 8);
                    	    	for (i = 0; i < padBytes; i++) {
                        	    	raFile.readByte();
                        	    }
                    	    }   
                            
                            try {
                    			image.importComplexData(2 * nonLogicalField* realBuffer.length, realBuffer, imaginaryBuffer, true, logMagDisplay);
                    		}
                    		catch(IOException e) {
                    		   MipavUtil.displayError("IOException on image.importComplexData(2 * nonLogicalField * realBuffer.length," +
                    		   		"  realBuffer, imaginaryBuffer, true, logMagDisplay)");
                    		   throw e;
                    		}		
                    	}
                    	break;
                    case miUINT8:
                    	Preferences.debug("Real data type = miUINT8\n", Preferences.DEBUG_FILEIO);
                    	Preferences.debug("Real data bytes = " + realDataBytes + "\n", Preferences.DEBUG_FILEIO);
                    	if (logicalFlag) {
                    		if (logicalFields == 1) {
	                    	    maskImage = new ModelImage(ModelStorageBase.UBYTE, maskExtents, fileName + "_mask");
	                    	    maskFileInfo.setDataType(ModelStorageBase.UBYTE);
	                    	    buffer = new byte[realDataBytes];
	                    	    shortBuffer = new short[realDataBytes];
	                    	    raFile.read(buffer);
	                    	    numberSlices = buffer.length/sliceSize;
	                    	    j = 0;
		                    	for (s = 0; s < numberSlices; s++) {
			                    	for (x = 0; x < imageExtents[1]; x++) {
			                    		for (y = 0; y < imageExtents[0]; y++) {
                                            shortBuffer[j++] = (short) (buffer[x + imageExtents[1] * y + s * sliceSize] & 0xff);			                    			
			                    		}
			                    	}
		                    	}
	                    	    
	                    	    if (haveSmallRealData) {
	                    		    if (realDataBytes < 4) {
	                    		    	padBytes = 4 - realDataBytes;
	                    		    	for (i = 0; i < padBytes; i++) {
	                    		    		raFile.readByte();
	                    		    	}
	                    		    }
	                    		}
	                    		else if ((realDataBytes % 8) != 0) {
	                    	    	padBytes = 8 - (realDataBytes % 8);
	                    	    	for (i = 0; i < padBytes; i++) {
	                        	    	raFile.readByte();
	                        	    }
	                    	    }  
		                    	try {
		                			maskImage.importUData(0, shortBuffer, true);
		                		}
		                		catch(IOException e) {
		                		   MipavUtil.displayError("IOException on maskImage.importUData(0, shortBuffer, true)");
		                		   throw e;
		                		}
                    	    } // if (logicalFields == 1)	
                    	}
                    	else if (isColor) {
                    		if (nonLogicalField == 0) {
                    			image = new ModelImage(ModelStorageBase.ARGB, imageExtents, fileName);
                    			fileInfo.setDataType(ModelStorageBase.ARGB);
                    		}
                    		if ((realDataBytes % 3) == 0) {
	                    		buffer = new byte[realDataBytes/3];
	                    		tBuffer = new byte[buffer.length];
	                    		numberSlices = buffer.length/sliceSize;
	                    		for (i = 0; i < 3; i++) {
			                    	raFile.read(buffer);
			                    	j = 0;
			                    	for (s = 0; s < numberSlices; s++) {
				                    	for (x = 0; x < imageExtents[1]; x++) {
				                    		for (y = 0; y < imageExtents[0]; y++) {
	                                            tBuffer[j++] = buffer[x + imageExtents[1] * y + s * sliceSize];			                    			
				                    		}
				                    	}
			                    	}
			                    	
			                    	try {
			                			image.importRGBData(i+1, nonLogicalField * tBuffer.length, tBuffer, true);
			                		}
			                		catch(IOException e) {
			                		   MipavUtil.displayError("IOException on image.importRGBData(i," +
			                		   		" nonLogicalField * tBuffer.length, tBuffer, true)");
			                		   throw e;
			                		}
	                    		} // if (i = 0; i < 3; i++)
	
		                    	
		                    	if (haveSmallRealData) {
	                    		    if (realDataBytes < 4) {
	                    		    	padBytes = 4 - realDataBytes;
	                    		    	for (i = 0; i < padBytes; i++) {
	                    		    		raFile.readByte();
	                    		    	}
	                    		    }
	                    		}
	                    		else if ((realDataBytes % 8) != 0) {
	                    	    	padBytes = 8 - (realDataBytes % 8);
	                    	    	for (i = 0; i < padBytes; i++) {
	                        	    	raFile.readByte();
	                        	    }
	                    	    } 
	                    	
	                        } // if (realDataBytes % 3) == 0)
                    	} // else if (isColor)
                    	else if (!complexFlag) {
                    		if (nonLogicalField == 0) {
	                    	    image = new ModelImage(ModelStorageBase.UBYTE, imageExtents, fileName);
	                    	    fileInfo.setDataType(ModelStorageBase.UBYTE);
                    		}
	                    	buffer = new byte[realDataBytes];
	                    	raFile.read(buffer);
	                    	shortBuffer = new short[realDataBytes];
	                    	numberSlices = buffer.length/sliceSize;
                    	    j = 0;
	                    	for (s = 0; s < numberSlices; s++) {
		                    	for (x = 0; x < imageExtents[1]; x++) {
		                    		for (y = 0; y < imageExtents[0]; y++) {
                                        shortBuffer[j++] = (short) (buffer[x + imageExtents[1] * y + s * sliceSize] & 0xff);			                    			
		                    		}
		                    	}
	                    	}
	                    	
	                    	if (haveSmallRealData) {
                    		    if (realDataBytes < 4) {
                    		    	padBytes = 4 - realDataBytes;
                    		    	for (i = 0; i < padBytes; i++) {
                    		    		raFile.readByte();
                    		    	}
                    		    }
                    		}
                    		else if ((realDataBytes % 8) != 0) {
                    	    	padBytes = 8 - (realDataBytes % 8);
                    	    	for (i = 0; i < padBytes; i++) {
                        	    	raFile.readByte();
                        	    }
                    	    }  
	                    	try {
	                			image.importUData(nonLogicalField * shortBuffer.length, shortBuffer, true);
	                		}
	                		catch(IOException e) {
	                		   MipavUtil.displayError("IOException on image.importData(nonLogicalField * shortBuffer.length, shortBuffer, true)");
	                		   throw e;
	                		}
                    	}
                    	else {
                    		if (nonLogicalField == 0) {
                    		    image = new ModelImage(ModelStorageBase.COMPLEX, imageExtents, fileName);
                    		    fileInfo.setDataType(ModelStorageBase.COMPLEX);
                    		}
                    		buffer = new byte[realDataBytes];
                    		raFile.read(buffer);
                    		realBuffer = new float[realDataBytes];
                    		numberSlices = buffer.length/sliceSize;
                    	    j = 0;
	                    	for (s = 0; s < numberSlices; s++) {
		                    	for (x = 0; x < imageExtents[1]; x++) {
		                    		for (y = 0; y < imageExtents[0]; y++) {
                                        realBuffer[j++] = (float) (buffer[x + imageExtents[1] * y + s * sliceSize] & 0xff);			                    			
		                    		}
		                    	}
	                    	}
                    		
                    		if (haveSmallRealData) {
                    		    if (realDataBytes < 4) {
                    		    	padBytes = 4 - realDataBytes;
                    		    	for (i = 0; i < padBytes; i++) {
                    		    		raFile.readByte();
                    		    	}
                    		    }
                    		}
                    		else if ((realDataBytes % 8) != 0) {
                    	    	padBytes = 8 - (realDataBytes % 8);
                    	    	for (i = 0; i < padBytes; i++) {
                        	    	raFile.readByte();
                        	    }
                    	    }  
                    	    imaginaryDataType = getInt(endianess);
                    	    if ((imaginaryDataType & 0xffff0000) != 0) {
                                // Small data element format    
                            	imaginaryDataBytes = (imaginaryDataType & 0xffff0000) >>> 16;
                            	imaginaryDataType = imaginaryDataType & 0xffff;
                            	haveSmallImaginaryData = true;
                            }
                            else {
                                imaginaryDataBytes = getInt(endianess);
                                haveSmallImaginaryData = false;
                            }
                    	    if (imaginaryDataType == miUINT8) {
                    	    	Preferences.debug("imaginaryDataType == miUINT8 as expected\n", Preferences.DEBUG_FILEIO);
                    	    }
                    	    else {
                    	    	Preferences.debug("imaginaryDataType unexpectedly == " + imaginaryDataType + "\n", 
                    	    			Preferences.DEBUG_FILEIO);
                    	    }
                         
                            if (imaginaryDataBytes == realDataBytes) {
                            	Preferences.debug("imaginaryDataBytes == realDataBytes as expected\n", 
                            			Preferences.DEBUG_FILEIO);
                            }
                            else {
                            	Preferences.debug("imaginaryDataBytes unexpectedly != realDataBytes\n", 
                            			Preferences.DEBUG_FILEIO);
                            }
                            raFile.read(buffer);
                            imaginaryBuffer = new float[imaginaryDataBytes];
                    	    j = 0;
	                    	for (s = 0; s < numberSlices; s++) {
		                    	for (x = 0; x < imageExtents[1]; x++) {
		                    		for (y = 0; y < imageExtents[0]; y++) {
                                        imaginaryBuffer[j++] = (float) (buffer[x + imageExtents[1] * y + s * sliceSize] & 0xff);			                    			
		                    		}
		                    	}
	                    	}
                            
                            if (haveSmallImaginaryData) {
                    		    if (imaginaryDataBytes < 4) {
                    		    	padBytes = 4 - imaginaryDataBytes;
                    		    	for (i = 0; i < padBytes; i++) {
                    		    		raFile.readByte();
                    		    	}
                    		    }
                    		}
                    		else if ((imaginaryDataBytes % 8) != 0) {
                    	    	padBytes = 8 - (imaginaryDataBytes % 8);
                    	    	for (i = 0; i < padBytes; i++) {
                        	    	raFile.readByte();
                        	    }
                    	    }   
                            
                            try {
                    			image.importComplexData(2 * nonLogicalField * realBuffer.length, realBuffer, imaginaryBuffer, true, logMagDisplay);
                    		}
                    		catch(IOException e) {
                    		   MipavUtil.displayError("IOException on image.importComplexData(2 * nonLogicalField * realBuffer.length," +
                    		   		"  realBuffer, imaginaryBuffer, true, logMagDisplay)");
                    		   throw e;
                    		}		
                    	}
                    	break;
                    case miINT16:
                    	Preferences.debug("Real data type = miINT16\n", Preferences.DEBUG_FILEIO);
                    	Preferences.debug("Real data bytes = " + realDataBytes + "\n", Preferences.DEBUG_FILEIO);
                    	if (!complexFlag) {
                    		if (nonLogicalField == 0) {
                    		    image = new ModelImage(ModelStorageBase.SHORT, imageExtents, fileName); 
                    		    fileInfo.setDataType(ModelStorageBase.SHORT);
                    		}
                    		buffer = new byte[realDataBytes];
                    		raFile.read(buffer);
                    		shortNumber = realDataBytes/2;
                    		shortBuffer = new short[shortNumber];
                    		numberSlices = shortNumber/sliceSize;
                    		j = 0;
	                    	for (s = 0; s < numberSlices; s++) {
		                    	for (x = 0; x < imageExtents[1]; x++) {
		                    		for (y = 0; y < imageExtents[0]; y++) {
		                    			index = 2*(x + imageExtents[1] * y + s * sliceSize);
                                        b1 = buffer[index] & 0xff;
                                        b2 = buffer[index+1] & 0xff;
                                        if (endianess == BIG_ENDIAN) {
                                        	shortBuffer[j++] = (short)((b1 << 8) | b2);	
                                        }
                                        else {
                                        	shortBuffer[j++] = (short)((b2 << 8) | b1);
                                        }
		                    		}
		                    	}
	                    	}
                    		 
                    		if (haveSmallRealData) {
                    		    if (realDataBytes < 4) {
                    		    	padBytes = 4 - realDataBytes;
                    		    	for (i = 0; i < padBytes; i++) {
                    		    		raFile.readByte();
                    		    	}
                    		    }
                    		}
                    		else if ((realDataBytes % 8) != 0) {
                    	    	padBytes = 8 - (realDataBytes % 8);
                    	    	for (i = 0; i < padBytes; i++) {
                        	    	raFile.readByte();
                        	    }
                    	    }  
                    		try {
                    			image.importData(nonLogicalField * shortBuffer.length, shortBuffer, true);
                    		}
                    		catch(IOException e) {
                    		   MipavUtil.displayError("IOException on image.importData(nonLogicalField * shortBuffer.length, shortBuffer, true)");
                    		   throw e;
                    		}
                    	}
                    	else {
                    		if (nonLogicalField == 0) {
                    		    image = new ModelImage(ModelStorageBase.COMPLEX, imageExtents, fileName); 
                    		    fileInfo.setDataType(ModelStorageBase.COMPLEX);
                    		}
                    		buffer = new byte[realDataBytes];
                    		raFile.read(buffer);
                    		shortNumber = realDataBytes/2;
                    		realBuffer = new float[shortNumber];
                    		numberSlices = shortNumber/sliceSize;
                    		j = 0;
	                    	for (s = 0; s < numberSlices; s++) {
		                    	for (x = 0; x < imageExtents[1]; x++) {
		                    		for (y = 0; y < imageExtents[0]; y++) {
		                    			index = 2*(x + imageExtents[1] * y + s * sliceSize);
                                        b1 = buffer[index] & 0xff;
                                        b2 = buffer[index+1] & 0xff;
                                        if (endianess == BIG_ENDIAN) {
                                        	realBuffer[j++] = (float)((b1 << 8) | b2);	
                                        }
                                        else {
                                        	realBuffer[j++] = (float)((b2 << 8) | b1);
                                        }
		                    		}
		                    	}
	                    	}
                    		
                    		if (haveSmallRealData) {
                    		    if (realDataBytes < 4) {
                    		    	padBytes = 4 - realDataBytes;
                    		    	for (i = 0; i < padBytes; i++) {
                    		    		raFile.readByte();
                    		    	}
                    		    }
                    		}
                    		else if ((realDataBytes % 8) != 0) {
                    	    	padBytes = 8 - (realDataBytes % 8);
                    	    	for (i = 0; i < padBytes; i++) {
                        	    	raFile.readByte();
                        	    }
                    	    }    
                    	    imaginaryDataType = getInt(endianess);
                    	    if ((imaginaryDataType & 0xffff0000) != 0) {
                                // Small data element format    
                            	imaginaryDataBytes = (imaginaryDataType & 0xffff0000) >>> 16;
                            	imaginaryDataType = imaginaryDataType & 0xffff;
                            	haveSmallImaginaryData = true;
                            }
                            else {
                                imaginaryDataBytes = getInt(endianess);
                                haveSmallImaginaryData = false;
                            }
                    	    if (imaginaryDataType == miINT16) {
                    	    	Preferences.debug("imaginaryDataType == miINT16 as expected\n", Preferences.DEBUG_FILEIO);
                    	    }
                    	    else {
                    	    	Preferences.debug("imaginaryDataType unexpectedly == " + imaginaryDataType + "\n", 
                    	    			Preferences.DEBUG_FILEIO);
                    	    }
                            
                            if (imaginaryDataBytes == realDataBytes) {
                            	Preferences.debug("imaginaryDataBytes == realDataBytes as expected\n", 
                            			Preferences.DEBUG_FILEIO);
                            }
                            else {
                            	Preferences.debug("imaginaryDataBytes unexpectedly != realDataBytes\n", 
                            			Preferences.DEBUG_FILEIO);
                            }
                            raFile.read(buffer);
                            imaginaryBuffer = new float[shortNumber];
                            j = 0;
	                    	for (s = 0; s < numberSlices; s++) {
		                    	for (x = 0; x < imageExtents[1]; x++) {
		                    		for (y = 0; y < imageExtents[0]; y++) {
		                    			index = 2*(x + imageExtents[1] * y + s * sliceSize);
                                        b1 = buffer[index] & 0xff;
                                        b2 = buffer[index+1] & 0xff;
                                        if (endianess == BIG_ENDIAN) {
                                        	imaginaryBuffer[j++] = (float)((b1 << 8) | b2);	
                                        }
                                        else {
                                        	imaginaryBuffer[j++] = (float)((b2 << 8) | b1);
                                        }
		                    		}
		                    	}
	                    	}
                            
                            if (haveSmallImaginaryData) {
                    		    if (imaginaryDataBytes < 4) {
                    		    	padBytes = 4 - imaginaryDataBytes;
                    		    	for (i = 0; i < padBytes; i++) {
                    		    		raFile.readByte();
                    		    	}
                    		    }
                    		}
                    		else if ((imaginaryDataBytes % 8) != 0) {
                    	    	padBytes = 8 - (imaginaryDataBytes % 8);
                    	    	for (i = 0; i < padBytes; i++) {
                        	    	raFile.readByte();
                        	    }
                    	    }   
                            
                            try {
                    			image.importComplexData(2 * nonLogicalField * realBuffer.length, realBuffer, imaginaryBuffer, true, logMagDisplay);
                    		}
                    		catch(IOException e) {
                    		   MipavUtil.displayError("IOException on image.importComplexData(2 * nonLogicalField * realBuffer.length," +
                    		   		"  realBuffer, imaginaryBuffer, true, logMagDisplay)");
                    		   throw e;
                    		}		
                    	}
                    	break;
                    case miUINT16:
                    	Preferences.debug("Real data type = miUINT16\n", Preferences.DEBUG_FILEIO);
                    	Preferences.debug("Real data bytes = " + realDataBytes + "\n", Preferences.DEBUG_FILEIO);
                    	if (isColor) {
                    		if (nonLogicalField == 0) {
                    			image = new ModelImage(ModelStorageBase.ARGB_USHORT, imageExtents, fileName);
                    			fileInfo.setDataType(ModelStorageBase.ARGB_USHORT);
                    		}
                    		if ((realDataBytes % 3) == 0) {
	                    		buffer = new byte[realDataBytes/3];
	                    		shortNumber = realDataBytes/6;
	                    		shortBuffer = new short[shortNumber];
	                    		numberSlices = shortNumber/sliceSize;
	                    		for (i = 0; i < 3; i++) {
			                    	raFile.read(buffer);
			                    	j = 0;
			                    	for (s = 0; s < numberSlices; s++) {
				                    	for (x = 0; x < imageExtents[1]; x++) {
				                    		for (y = 0; y < imageExtents[0]; y++) {
				                    			index = 2*(x + imageExtents[1] * y + s * sliceSize);
	                                            b1 = buffer[index] & 0xff;
	                                            b2 = buffer[index+1] & 0xff;
	                                            if (endianess == BIG_ENDIAN) {
	                                            	shortBuffer[j++] = (short) ((b1 << 8) | b2);	
	                                            }
	                                            else {
	                                            	shortBuffer[j++] = (short) ((b2 << 8) | b1);
	                                            }
				                    		}
				                    	}
			                    	}
			                    	
			                    	try {
			                			image.importRGBData(i+1, nonLogicalField * shortBuffer.length, shortBuffer, true);
			                		}
			                		catch(IOException e) {
			                		   MipavUtil.displayError("IOException on image.importRGBData(i," +
			                		   		" nonLogicalField * shortBuffer.length, shortBuffer, true)");
			                		   throw e;
			                		}
	                    		} // if (i = 0; i < 3; i++)
	
		                    	
		                    	if (haveSmallRealData) {
	                    		    if (realDataBytes < 4) {
	                    		    	padBytes = 4 - realDataBytes;
	                    		    	for (i = 0; i < padBytes; i++) {
	                    		    		raFile.readByte();
	                    		    	}
	                    		    }
	                    		}
	                    		else if ((realDataBytes % 8) != 0) {
	                    	    	padBytes = 8 - (realDataBytes % 8);
	                    	    	for (i = 0; i < padBytes; i++) {
	                        	    	raFile.readByte();
	                        	    }
	                    	    } 
	                    	
	                        } // if (realDataBytes % 3) == 0)
                    	} // if (isColor)
                    	else if (!complexFlag) {
                    		if (nonLogicalField == 0) {
                    		    image = new ModelImage(ModelStorageBase.USHORT, imageExtents, fileName);
                    		    fileInfo.setDataType(ModelStorageBase.USHORT);
                    		}
                    		buffer =  new byte[realDataBytes];
                    		raFile.read(buffer);
                    		shortNumber = realDataBytes/2;
                    		intBuffer = new int[shortNumber];
                    		numberSlices = shortNumber/sliceSize;
                    		j = 0;
	                    	for (s = 0; s < numberSlices; s++) {
		                    	for (x = 0; x < imageExtents[1]; x++) {
		                    		for (y = 0; y < imageExtents[0]; y++) {
		                    			index = 2*(x + imageExtents[1] * y + s * sliceSize);
                                        b1 = buffer[index] & 0xff;
                                        b2 = buffer[index+1] & 0xff;
                                        if (endianess == BIG_ENDIAN) {
                                        	intBuffer[j++] = ((b1 << 8) | b2);	
                                        }
                                        else {
                                        	intBuffer[j++] = ((b2 << 8) | b1);
                                        }
		                    		}
		                    	}
	                    	}
                    		 
                    		if (haveSmallRealData) {
                    		    if (realDataBytes < 4) {
                    		    	padBytes = 4 - realDataBytes;
                    		    	for (i = 0; i < padBytes; i++) {
                    		    		raFile.readByte();
                    		    	}
                    		    }
                    		}
                    		else if ((realDataBytes % 8) != 0) {
                    	    	padBytes = 8 - (realDataBytes % 8);
                    	    	for (i = 0; i < padBytes; i++) {
                        	    	raFile.readByte();
                        	    }
                    	    }  
                    		try {
                    			image.importUData(nonLogicalField * intBuffer.length, intBuffer, true);
                    		}
                    		catch(IOException e) {
                    		   MipavUtil.displayError("IOException on image.importUData(nonLogicalField * intBuffer.length, intBuffer, true)");
                    		   throw e;
                    		}
                    	}
                    	else {
                    		if (nonLogicalField == 0) {
                    		    image = new ModelImage(ModelStorageBase.COMPLEX, imageExtents, fileName); 
                    		    fileInfo.setDataType(ModelStorageBase.COMPLEX);
                    		}
                    		buffer = new byte[realDataBytes];
                    		raFile.read(buffer);
                    		shortNumber = realDataBytes/2;
                    		realBuffer = new float[shortNumber];
                    		numberSlices = shortNumber/sliceSize;
                    		j = 0;
	                    	for (s = 0; s < numberSlices; s++) {
		                    	for (x = 0; x < imageExtents[1]; x++) {
		                    		for (y = 0; y < imageExtents[0]; y++) {
		                    			index = 2*(x + imageExtents[1] * y + s * sliceSize);
                                        b1 = buffer[index] & 0xff;
                                        b2 = buffer[index+1] & 0xff;
                                        if (endianess == BIG_ENDIAN) {
                                            realBuffer[j++] = (float) ((b1 << 8) | b2);	
                                        }
                                        else {
                                        	realBuffer[j++] = (float) ((b2 << 8) | b1);
                                        }
		                    		}
		                    	}
	                    	}
                    		
                    		if (haveSmallRealData) {
                    		    if (realDataBytes < 4) {
                    		    	padBytes = 4 - realDataBytes;
                    		    	for (i = 0; i < padBytes; i++) {
                    		    		raFile.readByte();
                    		    	}
                    		    }
                    		}
                    		else if ((realDataBytes % 8) != 0) {
                    	    	padBytes = 8 - (realDataBytes % 8);
                    	    	for (i = 0; i < padBytes; i++) {
                        	    	raFile.readByte();
                        	    }
                    	    }  
                    	    imaginaryDataType = getInt(endianess);
                    	    if ((imaginaryDataType & 0xffff0000) != 0) {
                                // Small data element format    
                            	imaginaryDataBytes = (imaginaryDataType & 0xffff0000) >>> 16;
                            	imaginaryDataType = imaginaryDataType & 0xffff;
                            	haveSmallImaginaryData = true;
                            }
                            else {
                                imaginaryDataBytes = getInt(endianess);
                                haveSmallImaginaryData = false;
                            }
                    	    if (imaginaryDataType == miUINT16) {
                    	    	Preferences.debug("imaginaryDataType == miUINT16 as expected\n", Preferences.DEBUG_FILEIO);
                    	    }
                    	    else {
                    	    	Preferences.debug("imaginaryDataType unexpectedly == " + imaginaryDataType + "\n", 
                    	    			Preferences.DEBUG_FILEIO);
                    	    }
                            
                            if (imaginaryDataBytes == realDataBytes) {
                            	Preferences.debug("imaginaryDataBytes == realDataBytes as expected\n", 
                            			Preferences.DEBUG_FILEIO);
                            }
                            else {
                            	Preferences.debug("imaginaryDataBytes unexpectedly != realDataBytes\n", 
                            			Preferences.DEBUG_FILEIO);
                            }
                            raFile.read(buffer);
                            imaginaryBuffer = new float[shortNumber];
                            j = 0;
	                    	for (s = 0; s < numberSlices; s++) {
		                    	for (x = 0; x < imageExtents[1]; x++) {
		                    		for (y = 0; y < imageExtents[0]; y++) {
		                    			index = 2*(x + imageExtents[1] * y + s * sliceSize);
                                        b1 = buffer[index] & 0xff;
                                        b2 = buffer[index+1] & 0xff;
                                        if (endianess == BIG_ENDIAN) {
                                            imaginaryBuffer[j++] = (float) ((b1 << 8) | b2);	
                                        }
                                        else {
                                        	imaginaryBuffer[j++] = (float) ((b2 << 8) | b1);
                                        }
		                    		}
		                    	}
	                    	}
                            
                            if (haveSmallImaginaryData) {
                    		    if (imaginaryDataBytes < 4) {
                    		    	padBytes = 4 - imaginaryDataBytes;
                    		    	for (i = 0; i < padBytes; i++) {
                    		    		raFile.readByte();
                    		    	}
                    		    }
                    		}
                    		else if ((imaginaryDataBytes % 8) != 0) {
                    	    	padBytes = 8 - (imaginaryDataBytes % 8);
                    	    	for (i = 0; i < padBytes; i++) {
                        	    	raFile.readByte();
                        	    }
                    	    }   
                            
                            try {
                    			image.importComplexData(2 * nonLogicalField * realBuffer.length, realBuffer, imaginaryBuffer, true, logMagDisplay);
                    		}
                    		catch(IOException e) {
                    		   MipavUtil.displayError("IOException on image.importComplexData(2 * nonLogicalField * realBuffer.length," +
                    		   		"  realBuffer, imaginaryBuffer, true, logMagDisplay)");
                    		   throw e;
                    		}		
                    	}
                    	break;
                    case miINT32:
                    	Preferences.debug("Real data type = miINT32\n", Preferences.DEBUG_FILEIO);
                    	Preferences.debug("Real data bytes = " + realDataBytes + "\n", Preferences.DEBUG_FILEIO);
                    	if (!complexFlag) {
                    		if (nonLogicalField == 0) {
                    		    image = new ModelImage(ModelStorageBase.INTEGER, imageExtents, fileName);
                    		    fileInfo.setDataType(ModelStorageBase.INTEGER);
                    		}
                    		buffer = new byte[realDataBytes];
                    		raFile.read(buffer);
                    		intNumber = realDataBytes/4;
                    		intBuffer = new int[intNumber];
                    		numberSlices = intNumber/sliceSize;
                    		j = 0;
	                    	for (s = 0; s < numberSlices; s++) {
		                    	for (x = 0; x < imageExtents[1]; x++) {
		                    		for (y = 0; y < imageExtents[0]; y++) {
		                    			index = 4*(x + imageExtents[1] * y + s * sliceSize);
                                        b1 = buffer[index] & 0xff;
                                        b2 = buffer[index+1] & 0xff;
                                        b3 = buffer[index+2] & 0xff;
                                        b4 = buffer[index+3] & 0xff;
                                        if (endianess == BIG_ENDIAN) {
                                        	intBuffer[j++] = ((b1 << 24) | (b2 << 16) | (b3 << 8) | b4);
                                        }
                                        else {
                                        	intBuffer[j++] = ((b4 << 24) | (b3 << 16) | (b2 << 8) | b1);
                                        }
		                    		}
		                    	}
	                    	}
                    		
                    		if (haveSmallRealData) {
                    		    if (realDataBytes < 4) {
                    		    	padBytes = 4 - realDataBytes;
                    		    	for (i = 0; i < padBytes; i++) {
                    		    		raFile.readByte();
                    		    	}
                    		    }
                    		}
                    		else if ((realDataBytes % 8) != 0) {
                    	    	padBytes = 8 - (realDataBytes % 8);
                    	    	for (i = 0; i < padBytes; i++) {
                        	    	raFile.readByte();
                        	    }
                    	    }  
                    		try {
                    			image.importData(nonLogicalField * intBuffer.length, intBuffer, true);
                    		}
                    		catch(IOException e) {
                    		   MipavUtil.displayError("IOException on image.importData(nonLogicalField * intBuffer.length, intBuffer, true)");
                    		   throw e;
                    		}
                    	}
                    	else {
                    		if (nonLogicalField == 0) {
                    		    image = new ModelImage(ModelStorageBase.DCOMPLEX, imageExtents, fileName); 
                    		    fileInfo.setDataType(ModelStorageBase.DCOMPLEX);
                    		}
                    		buffer = new byte[realDataBytes];
                    		raFile.read(buffer);
                    		intNumber = realDataBytes/4;
                    		realDBuffer = new double[intNumber];
                    		numberSlices = intNumber/sliceSize;
                    		j = 0;
	                    	for (s = 0; s < numberSlices; s++) {
		                    	for (x = 0; x < imageExtents[1]; x++) {
		                    		for (y = 0; y < imageExtents[0]; y++) {
		                    			index = 4*(x + imageExtents[1] * y + s * sliceSize);
                                        b1 = buffer[index] & 0xff;
                                        b2 = buffer[index+1] & 0xff;
                                        b3 = buffer[index+2] & 0xff;
                                        b4 = buffer[index+3] & 0xff;
                                        if (endianess == BIG_ENDIAN) {
                                        	realDBuffer[j++] = (double)((b1 << 24) | (b2 << 16) | (b3 << 8) | b4);
                                        }
                                        else {
                                        	realDBuffer[j++] = (double)((b4 << 24) | (b3 << 16) | (b2 << 8) | b1);
                                        }
		                    		}
		                    	}
	                    	}
                    		
                    		if (haveSmallRealData) {
                    		    if (realDataBytes < 4) {
                    		    	padBytes = 4 - realDataBytes;
                    		    	for (i = 0; i < padBytes; i++) {
                    		    		raFile.readByte();
                    		    	}
                    		    }
                    		}
                    		else if ((realDataBytes % 8) != 0) {
                    	    	padBytes = 8 - (realDataBytes % 8);
                    	    	for (i = 0; i < padBytes; i++) {
                        	    	raFile.readByte();
                        	    }
                    	    }  
                    	    imaginaryDataType = getInt(endianess);
                    	    if ((imaginaryDataType & 0xffff0000) != 0) {
                                // Small data element format    
                            	imaginaryDataBytes = (imaginaryDataType & 0xffff0000) >>> 16;
                            	imaginaryDataType = imaginaryDataType & 0xffff;
                            	haveSmallImaginaryData = true;
                            }
                            else {
                                imaginaryDataBytes = getInt(endianess);
                                haveSmallImaginaryData = false;
                            }
                    	    if (imaginaryDataType == miINT32) {
                    	    	Preferences.debug("imaginaryDataType == miINT32 as expected\n", Preferences.DEBUG_FILEIO);
                    	    }
                    	    else {
                    	    	Preferences.debug("imaginaryDataType unexpectedly == " + imaginaryDataType + "\n", 
                    	    			Preferences.DEBUG_FILEIO);
                    	    }
                    	    
                            if (imaginaryDataBytes == realDataBytes) {
                            	Preferences.debug("imaginaryDataBytes == realDataBytes as expected\n", 
                            			Preferences.DEBUG_FILEIO);
                            }
                            else {
                            	Preferences.debug("imaginaryDataBytes unexpectedly != realDataBytes\n", 
                            			Preferences.DEBUG_FILEIO);
                            }
                            raFile.read(buffer);
                            imaginaryDBuffer = new double[intNumber];
                            j = 0;
	                    	for (s = 0; s < numberSlices; s++) {
		                    	for (x = 0; x < imageExtents[1]; x++) {
		                    		for (y = 0; y < imageExtents[0]; y++) {
		                    			index = 4*(x + imageExtents[1] * y + s * sliceSize);
                                        b1 = buffer[index] & 0xff;
                                        b2 = buffer[index+1] & 0xff;
                                        b3 = buffer[index+2] & 0xff;
                                        b4 = buffer[index+3] & 0xff;
                                        if (endianess == BIG_ENDIAN) {
                                        	imaginaryDBuffer[j++] = (double)((b1 << 24) | (b2 << 16) | (b3 << 8) | b4);
                                        }
                                        else {
                                        	imaginaryDBuffer[j++] = (double)((b4 << 24) | (b3 << 16) | (b2 << 8) | b1);
                                        }
		                    		}
		                    	}
	                    	}
                            
                            if (haveSmallImaginaryData) {
                    		    if (imaginaryDataBytes < 4) {
                    		    	padBytes = 4 - imaginaryDataBytes;
                    		    	for (i = 0; i < padBytes; i++) {
                    		    		raFile.readByte();
                    		    	}
                    		    }
                    		}
                    		else if ((imaginaryDataBytes % 8) != 0) {
                    	    	padBytes = 8 - (imaginaryDataBytes % 8);
                    	    	for (i = 0; i < padBytes; i++) {
                        	    	raFile.readByte();
                        	    }
                    	    }   
                            
                            try {
                    			image.importDComplexData(2 * nonLogicalField * realDBuffer.length, realDBuffer, imaginaryDBuffer, true, logMagDisplay);
                    		}
                    		catch(IOException e) {
                    		   MipavUtil.displayError("IOException on image.importDComplexData(2 * nonLogicalField * realDBuffer.length," +
                    		   		"  realDBuffer, imaginaryDBuffer, true, logMagDisplay)");
                    		   throw e;
                    		}		
                    	}
                    	break;
                    case miUINT32:
                    	Preferences.debug("Real data type = miUINT32\n", Preferences.DEBUG_FILEIO);
                    	Preferences.debug("Real data bytes = " + realDataBytes + "\n", Preferences.DEBUG_FILEIO);
                    	if (!complexFlag) {
                    		if (nonLogicalField == 0) {
                    		    image = new ModelImage(ModelStorageBase.UINTEGER, imageExtents, fileName); 
                    		    fileInfo.setDataType(ModelStorageBase.UINTEGER);
                    		}
                    		buffer = new byte[realDataBytes];
                    		raFile.read(buffer);
                    		intNumber = realDataBytes/4;
                    		longBuffer = new long[intNumber];
                    		numberSlices = intNumber/sliceSize;
                    		j = 0;
	                    	for (s = 0; s < numberSlices; s++) {
		                    	for (x = 0; x < imageExtents[1]; x++) {
		                    		for (y = 0; y < imageExtents[0]; y++) {
		                    			index = 4*(x + imageExtents[1] * y + s * sliceSize);
                                        b1L = buffer[index] & 0xffL;
                                        b2L = buffer[index+1] & 0xffL;
                                        b3L = buffer[index+2] & 0xffL;
                                        b4L = buffer[index+3] & 0xffL;
                                        if (endianess == BIG_ENDIAN) {
                                        	longBuffer[j++] = ((b1L << 24) | (b2L << 16) | (b3L << 8) | b4L);
                                        }
                                        else {
                                        	longBuffer[j++] = ((b4L << 24) | (b3L << 16) | (b2L << 8) | b1L);
                                        }
		                    		}
		                    	}
	                    	}
                    		
                    		if (haveSmallRealData) {
                    		    if (realDataBytes < 4) {
                    		    	padBytes = 4 - realDataBytes;
                    		    	for (i = 0; i < padBytes; i++) {
                    		    		raFile.readByte();
                    		    	}
                    		    }
                    		}
                    		else if ((realDataBytes % 8) != 0) {
                    	    	padBytes = 8 - (realDataBytes % 8);
                    	    	for (i = 0; i < padBytes; i++) {
                        	    	raFile.readByte();
                        	    }
                    	    }  
                    		try {
                    			image.importUData(nonLogicalField * longBuffer.length, longBuffer, true);
                    		}
                    		catch(IOException e) {
                    		   MipavUtil.displayError("IOException on image.importUData(nonLogicalField * longBuffer.length, longBuffer, true)");
                    		   throw e;
                    		}
                    	}
                    	else {
                    		if (nonLogicalField == 0) {
                    		    image = new ModelImage(ModelStorageBase.DCOMPLEX, imageExtents, fileName); 
                    		    fileInfo.setDataType(ModelStorageBase.DCOMPLEX);
                    		}
                    		buffer = new byte[realDataBytes];
                    		raFile.read(buffer);
                    		intNumber = realDataBytes/4;
                    		realDBuffer = new double[intNumber];
                    		numberSlices = intNumber/sliceSize;
                    		j = 0;
	                    	for (s = 0; s < numberSlices; s++) {
		                    	for (x = 0; x < imageExtents[1]; x++) {
		                    		for (y = 0; y < imageExtents[0]; y++) {
		                    			index = 4*(x + imageExtents[1] * y + s * sliceSize);
                                        b1L = buffer[index] & 0xffL;
                                        b2L = buffer[index+1] & 0xffL;
                                        b3L = buffer[index+2] & 0xffL;
                                        b4L = buffer[index+3] & 0xffL;
                                        if (endianess == BIG_ENDIAN) {
                                        	realDBuffer[j++] = (double)((b1L << 24) | (b2L << 16) | (b3L << 8) | b4L);
                                        }
                                        else {
                                        	realDBuffer[j++] = (double)((b4L << 24) | (b3L << 16) | (b2L << 8) | b1L);
                                        }
		                    		}
		                    	}
	                    	}
                    		
                    		if (haveSmallRealData) {
                    		    if (realDataBytes < 4) {
                    		    	padBytes = 4 - realDataBytes;
                    		    	for (i = 0; i < padBytes; i++) {
                    		    		raFile.readByte();
                    		    	}
                    		    }
                    		}
                    		else if ((realDataBytes % 8) != 0) {
                    	    	padBytes = 8 - (realDataBytes % 8);
                    	    	for (i = 0; i < padBytes; i++) {
                        	    	raFile.readByte();
                        	    }
                    	    }   
                    	    imaginaryDataType = getInt(endianess);
                    	    if ((imaginaryDataType & 0xffff0000) != 0) {
                                // Small data element format    
                            	imaginaryDataBytes = (imaginaryDataType & 0xffff0000) >>> 16;
                            	imaginaryDataType = imaginaryDataType & 0xffff;
                            	haveSmallImaginaryData = true;
                            }
                            else {
                                imaginaryDataBytes = getInt(endianess);
                                haveSmallImaginaryData = false;
                            }
                    	    if (imaginaryDataType == miUINT32) {
                    	    	Preferences.debug("imaginaryDataType == miUINT32 as expected\n", Preferences.DEBUG_FILEIO);
                    	    }
                    	    else {
                    	    	Preferences.debug("imaginaryDataType unexpectedly == " + imaginaryDataType + "\n", 
                    	    			Preferences.DEBUG_FILEIO);
                    	    }
                    	    
                            if (imaginaryDataBytes == realDataBytes) {
                            	Preferences.debug("imaginaryDataBytes == realDataBytes as expected\n", 
                            			Preferences.DEBUG_FILEIO);
                            }
                            else {
                            	Preferences.debug("imaginaryDataBytes unexpectedly != realDataBytes\n", 
                            			Preferences.DEBUG_FILEIO);
                            }
                            raFile.read(buffer);
                            imaginaryDBuffer = new double[intNumber];
                            j = 0;
	                    	for (s = 0; s < numberSlices; s++) {
		                    	for (x = 0; x < imageExtents[1]; x++) {
		                    		for (y = 0; y < imageExtents[0]; y++) {
		                    			index = 4*(x + imageExtents[1] * y + s * sliceSize);
                                        b1L = buffer[index] & 0xffL;
                                        b2L = buffer[index+1] & 0xffL;
                                        b3L = buffer[index+2] & 0xffL;
                                        b4L = buffer[index+3] & 0xffL;
                                        if (endianess == BIG_ENDIAN) {
                                        	imaginaryDBuffer[j++] = (double)((b1L << 24) | (b2L << 16) | (b3L << 8) | b4L);
                                        }
                                        else {
                                        	imaginaryDBuffer[j++] = (double)((b4L << 24) | (b3L << 16) | (b2L << 8) | b1L);
                                        }
		                    		}
		                    	}
	                    	}
                            
                            if (haveSmallImaginaryData) {
                    		    if (imaginaryDataBytes < 4) {
                    		    	padBytes = 4 - imaginaryDataBytes;
                    		    	for (i = 0; i < padBytes; i++) {
                    		    		raFile.readByte();
                    		    	}
                    		    }
                    		}
                    		else if ((imaginaryDataBytes % 8) != 0) {
                    	    	padBytes = 8 - (imaginaryDataBytes % 8);
                    	    	for (i = 0; i < padBytes; i++) {
                        	    	raFile.readByte();
                        	    }
                    	    }   
                            
                            try {
                    			image.importDComplexData(2 * nonLogicalField * realDBuffer.length, realDBuffer, imaginaryDBuffer, true, logMagDisplay);
                    		}
                    		catch(IOException e) {
                    		   MipavUtil.displayError("IOException on image.importDComplexData(2 * nonLogicalField * realDBuffer.length," +
                    		   		"  realDBuffer, imaginaryDBuffer, true, logMagDisplay)");
                    		   throw e;
                    		}		
                    	}
                    	break;
                    case miSINGLE:
                    	Preferences.debug("Real data type = miSINGLE\n", Preferences.DEBUG_FILEIO);
                    	Preferences.debug("Real data bytes = " + realDataBytes + "\n", Preferences.DEBUG_FILEIO);
                    	if (!complexFlag) {
                    		if (nonLogicalField == 0) {
                    		    image = new ModelImage(ModelStorageBase.FLOAT, imageExtents, fileName); 
                    		    fileInfo.setDataType(ModelStorageBase.FLOAT);
                    		}
                    		buffer = new byte[realDataBytes];
                    		raFile.read(buffer);
                    		floatNumber = realDataBytes/4;
                    		floatBuffer = new float[floatNumber];
                    		numberSlices = floatNumber/sliceSize;
                    		j = 0;
	                    	for (s = 0; s < numberSlices; s++) {
		                    	for (x = 0; x < imageExtents[1]; x++) {
		                    		for (y = 0; y < imageExtents[0]; y++) {
		                    			index = 4*(x + imageExtents[1] * y + s * sliceSize);
                                        b1 = buffer[index] & 0xff;
                                        b2 = buffer[index+1] & 0xff;
                                        b3 = buffer[index+2] & 0xff;
                                        b4 = buffer[index+3] & 0xff;
                                        if (endianess == BIG_ENDIAN) {
                                        	tmpInt = ((b1 << 24) | (b2 << 16) | (b3 << 8) | b4);
                                        }
                                        else {
                                        	tmpInt = ((b4 << 24) | (b3 << 16) | (b2 << 8) | b1);
                                        }
                                        floatBuffer[j++] = Float.intBitsToFloat(tmpInt);
		                    		}
		                    	}
	                    	}
                    		
                    		if (haveSmallRealData) {
                    		    if (realDataBytes < 4) {
                    		    	padBytes = 4 - realDataBytes;
                    		    	for (i = 0; i < padBytes; i++) {
                    		    		raFile.readByte();
                    		    	}
                    		    }
                    		}
                    		else if ((realDataBytes % 8) != 0) {
                    	    	padBytes = 8 - (realDataBytes % 8);
                    	    	for (i = 0; i < padBytes; i++) {
                        	    	raFile.readByte();
                        	    }
                    	    }  
                    		try {
                    			image.importData(nonLogicalField * floatBuffer.length, floatBuffer, true);
                    		}
                    		catch(IOException e) {
                    		   MipavUtil.displayError("IOException on image.importData(nonLogicalField * floatBuffer.length, floatBuffer, true)");
                    		   throw e;
                    		}
                    	}
                    	else {
                    		if (nonLogicalField == 0) {
                    		    image = new ModelImage(ModelStorageBase.COMPLEX, imageExtents, fileName); 
                    		    fileInfo.setDataType(ModelStorageBase.COMPLEX);
                    		}
                    		buffer = new byte[realDataBytes];
                    		raFile.read(buffer);
                    		floatNumber = realDataBytes/4;
                    		realBuffer = new float[floatNumber];
                    		numberSlices = floatNumber/sliceSize;
                    		j = 0;
	                    	for (s = 0; s < numberSlices; s++) {
		                    	for (x = 0; x < imageExtents[1]; x++) {
		                    		for (y = 0; y < imageExtents[0]; y++) {
		                    			index = 4*(x + imageExtents[1] * y + s * sliceSize);
                                        b1 = buffer[index] & 0xff;
                                        b2 = buffer[index+1] & 0xff;
                                        b3 = buffer[index+2] & 0xff;
                                        b4 = buffer[index+3] & 0xff;
                                        if (endianess == BIG_ENDIAN) {
                                        	tmpInt = ((b1 << 24) | (b2 << 16) | (b3 << 8) | b4);
                                        }
                                        else {
                                        	tmpInt = ((b4 << 24) | (b3 << 16) | (b2 << 8) | b1);
                                        }
                                        realBuffer[j++] = Float.intBitsToFloat(tmpInt);
		                    		}
		                    	}
	                    	}
                    		
                    		if (haveSmallRealData) {
                    		    if (realDataBytes < 4) {
                    		    	padBytes = 4 - realDataBytes;
                    		    	for (i = 0; i < padBytes; i++) {
                    		    		raFile.readByte();
                    		    	}
                    		    }
                    		}
                    		else if ((realDataBytes % 8) != 0) {
                    	    	padBytes = 8 - (realDataBytes % 8);
                    	    	for (i = 0; i < padBytes; i++) {
                        	    	raFile.readByte();
                        	    }
                    	    }    
                    	    imaginaryDataType = getInt(endianess);
                    	    if ((imaginaryDataType & 0xffff0000) != 0) {
                                // Small data element format    
                            	imaginaryDataBytes = (imaginaryDataType & 0xffff0000) >>> 16;
                            	imaginaryDataType = imaginaryDataType & 0xffff;
                            	haveSmallImaginaryData = true;
                            }
                            else {
                                imaginaryDataBytes = getInt(endianess);
                                haveSmallImaginaryData = false;
                            }
                    	    if (imaginaryDataType == miSINGLE) {
                    	    	Preferences.debug("imaginaryDataType == miSINGLE as expected\n", Preferences.DEBUG_FILEIO);
                    	    }
                    	    else {
                    	    	Preferences.debug("imaginaryDataType unexpectedly == " + imaginaryDataType + "\n", 
                    	    			Preferences.DEBUG_FILEIO);
                    	    }
                    	   
                            if (imaginaryDataBytes == realDataBytes) {
                            	Preferences.debug("imaginaryDataBytes == realDataBytes as expected\n", 
                            			Preferences.DEBUG_FILEIO);
                            }
                            else {
                            	Preferences.debug("imaginaryDataBytes unexpectedly != realDataBytes\n", 
                            			Preferences.DEBUG_FILEIO);
                            }
                            raFile.read(buffer);
                            imaginaryBuffer = new float[floatNumber];
                            j = 0;
	                    	for (s = 0; s < numberSlices; s++) {
		                    	for (x = 0; x < imageExtents[1]; x++) {
		                    		for (y = 0; y < imageExtents[0]; y++) {
		                    			index = 4*(x + imageExtents[1] * y + s * sliceSize);
                                        b1 = buffer[index] & 0xff;
                                        b2 = buffer[index+1] & 0xff;
                                        b3 = buffer[index+2] & 0xff;
                                        b4 = buffer[index+3] & 0xff;
                                        if (endianess == BIG_ENDIAN) {
                                        	tmpInt = ((b1 << 24) | (b2 << 16) | (b3 << 8) | b4);
                                        }
                                        else {
                                        	tmpInt = ((b4 << 24) | (b3 << 16) | (b2 << 8) | b1);
                                        }
                                        imaginaryBuffer[j++] = Float.intBitsToFloat(tmpInt);
		                    		}
		                    	}
	                    	}
                          
                            if (haveSmallImaginaryData) {
                    		    if (imaginaryDataBytes < 4) {
                    		    	padBytes = 4 - imaginaryDataBytes;
                    		    	for (i = 0; i < padBytes; i++) {
                    		    		raFile.readByte();
                    		    	}
                    		    }
                    		}
                    		else if ((imaginaryDataBytes % 8) != 0) {
                    	    	padBytes = 8 - (imaginaryDataBytes % 8);
                    	    	for (i = 0; i < padBytes; i++) {
                        	    	raFile.readByte();
                        	    }
                    	    }   
                            
                            try {
                    			image.importComplexData(2 * nonLogicalField * realBuffer.length, realBuffer, imaginaryBuffer, true, logMagDisplay);
                    		}
                    		catch(IOException e) {
                    		   MipavUtil.displayError("IOException on image.importComplexData(2 * nonLogicalField * realBuffer.length," +
                    		   		" realBuffer, imaginaryBuffer, true, logMagDisplay)");
                    		   throw e;
                    		}
                    	}
                    	break;
                    case miDOUBLE:
                    	Preferences.debug("Real data type = miDOUBLE\n", Preferences.DEBUG_FILEIO);
                    	Preferences.debug("Real data bytes = " + realDataBytes + "\n", Preferences.DEBUG_FILEIO);
                    	if (isColor) {
                    		if (nonLogicalField == 0) {
                    			image = new ModelImage(ModelStorageBase.ARGB_FLOAT, imageExtents, fileName);
                    			fileInfo.setDataType(ModelStorageBase.ARGB_FLOAT);
                    		}
                    		if ((realDataBytes % 3) == 0) {
	                    		buffer = new byte[realDataBytes/3];
	                    		doubleNumber = realDataBytes/24;
	                    		floatBuffer = new float[doubleNumber];
	                    		numberSlices = doubleNumber/sliceSize;
	                    		for (i = 0; i < 3; i++) {
			                    	raFile.read(buffer);
			                    	j = 0;
			                    	for (s = 0; s < numberSlices; s++) {
				                    	for (x = 0; x < imageExtents[1]; x++) {
				                    		for (y = 0; y < imageExtents[0]; y++) {
				                    			index = 8*(x + imageExtents[1] * y + s * sliceSize);
	                                            b1L = buffer[index] & 0xffL;
	                                            b2L = buffer[index+1] & 0xffL;
	                                            b3L = buffer[index+2] & 0xffL;
	                                            b4L = buffer[index+3] & 0xffL;
	                                            b5L = buffer[index+4] & 0xffL;
	                                            b6L = buffer[index+5] & 0xffL;
	                                            b7L = buffer[index+6] & 0xffL;
	                                            b8L = buffer[index+7] & 0xffL;
	                                            if (endianess == BIG_ENDIAN) {
	                                            	tmpLong = ((b1L << 56) | (b2L << 48) | (b3L << 40) | (b4L << 32) |
			                                                   (b5L << 24) | (b6L << 16) | (b7L << 8) | b8L);	
	                                            }
	                                            else {
	                                            	tmpLong = ((b8L << 56) | (b7L << 48) | (b6L << 40) | (b5L << 32) |
			                                                   (b4L << 24) | (b3L << 16) | (b2L << 8) | b1L);
	                                            }
	                                            floatBuffer[j++] = (float)(255.0 * Double.longBitsToDouble(tmpLong));
				                    		}
				                    	}
			                    	}
			                    	
			                    	try {
			                			image.importRGBData(i+1, nonLogicalField * floatBuffer.length, floatBuffer, true);
			                		}
			                		catch(IOException e) {
			                		   MipavUtil.displayError("IOException on image.importRGBData(i," +
			                		   		" nonLogicalField * floatBuffer.length, floatBuffer, true)");
			                		   throw e;
			                		}
	                    		} // if (i = 0; i < 3; i++)
	
		                    	
		                    	if (haveSmallRealData) {
	                    		    if (realDataBytes < 4) {
	                    		    	padBytes = 4 - realDataBytes;
	                    		    	for (i = 0; i < padBytes; i++) {
	                    		    		raFile.readByte();
	                    		    	}
	                    		    }
	                    		}
	                    		else if ((realDataBytes % 8) != 0) {
	                    	    	padBytes = 8 - (realDataBytes % 8);
	                    	    	for (i = 0; i < padBytes; i++) {
	                        	    	raFile.readByte();
	                        	    }
	                    	    } 
	                    	
	                        } // if (realDataBytes % 3) == 0)
                    	} // else if (isColor)
                    	else if (!complexFlag) {
                    		if (nonLogicalField == 0) {
                    		    image = new ModelImage(ModelStorageBase.DOUBLE, imageExtents, fileName); 
                    		    fileInfo.setDataType(ModelStorageBase.DOUBLE);
                    		}
                    		buffer = new byte[realDataBytes];
                    		raFile.read(buffer);
                    		doubleNumber = realDataBytes/8;
                    		doubleBuffer = new double[doubleNumber];
                    		numberSlices = doubleNumber/sliceSize;
                    		j = 0;
	                    	for (s = 0; s < numberSlices; s++) {
		                    	for (x = 0; x < imageExtents[1]; x++) {
		                    		for (y = 0; y < imageExtents[0]; y++) {
		                    			index = 8*(x + imageExtents[1] * y + s * sliceSize);
                                        b1L = buffer[index] & 0xffL;
                                        b2L = buffer[index+1] & 0xffL;
                                        b3L = buffer[index+2] & 0xffL;
                                        b4L = buffer[index+3] & 0xffL;
                                        b5L = buffer[index+4] & 0xffL;
                                        b6L = buffer[index+5] & 0xffL;
                                        b7L = buffer[index+6] & 0xffL;
                                        b8L = buffer[index+7] & 0xffL;
                                        if (endianess == BIG_ENDIAN) {
                                        	tmpLong = ((b1L << 56) | (b2L << 48) | (b3L << 40) | (b4L << 32) |
	                                                   (b5L << 24) | (b6L << 16) | (b7L << 8) | b8L);	
                                        }
                                        else {
                                        	tmpLong = ((b8L << 56) | (b7L << 48) | (b6L << 40) | (b5L << 32) |
	                                                   (b4L << 24) | (b3L << 16) | (b2L << 8) | b1L);
                                        }
                                        doubleBuffer[j++] = Double.longBitsToDouble(tmpLong);
		                    		}
		                    	}
	                    	}
                    		
                    		if (haveSmallRealData) {
                    		    if (realDataBytes < 4) {
                    		    	padBytes = 4 - realDataBytes;
                    		    	for (i = 0; i < padBytes; i++) {
                    		    		raFile.readByte();
                    		    	}
                    		    }
                    		}
                    		else if ((realDataBytes % 8) != 0) {
                    	    	padBytes = 8 - (realDataBytes % 8);
                    	    	for (i = 0; i < padBytes; i++) {
                        	    	raFile.readByte();
                        	    }
                    	    }  
                    		try {
                    			image.importData(nonLogicalField * doubleBuffer.length, doubleBuffer, true);
                    		}
                    		catch(IOException e) {
                    		   MipavUtil.displayError("IOException on image.importData(nonLogicalField * doubleBuffer.length, doubleBuffer, true)");
                    		   throw e;
                    		}
                    	}
                    	else {
                    		if (nonLogicalField == 0) {
                    		    image = new ModelImage(ModelStorageBase.DCOMPLEX, imageExtents, fileName); 
                    		    fileInfo.setDataType(ModelStorageBase.DCOMPLEX);
                    		}
                    		buffer = new byte[realDataBytes];
                    		raFile.read(buffer);
                    		doubleNumber = realDataBytes/8;
                    		realDBuffer = new double[doubleNumber];
                    		numberSlices = doubleNumber/sliceSize;
                    		j = 0;
	                    	for (s = 0; s < numberSlices; s++) {
		                    	for (x = 0; x < imageExtents[1]; x++) {
		                    		for (y = 0; y < imageExtents[0]; y++) {
		                    			index = 8*(x + imageExtents[1] * y + s * sliceSize);
                                        b1L = buffer[index] & 0xffL;
                                        b2L = buffer[index+1] & 0xffL;
                                        b3L = buffer[index+2] & 0xffL;
                                        b4L = buffer[index+3] & 0xffL;
                                        b5L = buffer[index+4] & 0xffL;
                                        b6L = buffer[index+5] & 0xffL;
                                        b7L = buffer[index+6] & 0xffL;
                                        b8L = buffer[index+7] & 0xffL;
                                        if (endianess == BIG_ENDIAN) {
                                        	tmpLong = ((b1L << 56) | (b2L << 48) | (b3L << 40) | (b4L << 32) |
	                                                   (b5L << 24) | (b6L << 16) | (b7L << 8) | b8L);	
                                        }
                                        else {
                                        	tmpLong = ((b8L << 56) | (b7L << 48) | (b6L << 40) | (b5L << 32) |
	                                                   (b4L << 24) | (b3L << 16) | (b2L << 8) | b1L);
                                        }
                                        realDBuffer[j++] = Double.longBitsToDouble(tmpLong);
		                    		}
		                    	}
	                    	}
                    		
                    		if (haveSmallRealData) {
                    		    if (realDataBytes < 4) {
                    		    	padBytes = 4 - realDataBytes;
                    		    	for (i = 0; i < padBytes; i++) {
                    		    		raFile.readByte();
                    		    	}
                    		    }
                    		}
                    		else if ((realDataBytes % 8) != 0) {
                    	    	padBytes = 8 - (realDataBytes % 8);
                    	    	for (i = 0; i < padBytes; i++) {
                        	    	raFile.readByte();
                        	    }
                    	    }   
                    	    imaginaryDataType = getInt(endianess);
                    	    if ((imaginaryDataType & 0xffff0000) != 0) {
                                // Small data element format    
                            	imaginaryDataBytes = (imaginaryDataType & 0xffff0000) >>> 16;
                            	imaginaryDataType = imaginaryDataType & 0xffff;
                            	haveSmallImaginaryData = true;
                            }
                            else {
                                imaginaryDataBytes = getInt(endianess);
                                haveSmallImaginaryData = false;
                            }
                    	    if (imaginaryDataType == miDOUBLE) {
                    	    	Preferences.debug("imaginaryDataType == miDOUBLE as expected\n", Preferences.DEBUG_FILEIO);
                    	    }
                    	    else {
                    	    	Preferences.debug("imaginaryDataType unexpectedly == " + imaginaryDataType + "\n", 
                    	    			Preferences.DEBUG_FILEIO);
                    	    }
                    	    
                            if (imaginaryDataBytes == realDataBytes) {
                            	Preferences.debug("imaginaryDataBytes == realDataBytes as expected\n", 
                            			Preferences.DEBUG_FILEIO);
                            }
                            else {
                            	Preferences.debug("imaginaryDataBytes unexpectedly != realDataBytes\n", 
                            			Preferences.DEBUG_FILEIO);
                            }
                            raFile.read(buffer);
                            imaginaryDBuffer = new double[doubleNumber];
                            j = 0;
	                    	for (s = 0; s < numberSlices; s++) {
		                    	for (x = 0; x < imageExtents[1]; x++) {
		                    		for (y = 0; y < imageExtents[0]; y++) {
		                    			index = 8*(x + imageExtents[1] * y + s * sliceSize);
                                        b1L = buffer[index] & 0xffL;
                                        b2L = buffer[index+1] & 0xffL;
                                        b3L = buffer[index+2] & 0xffL;
                                        b4L = buffer[index+3] & 0xffL;
                                        b5L = buffer[index+4] & 0xffL;
                                        b6L = buffer[index+5] & 0xffL;
                                        b7L = buffer[index+6] & 0xffL;
                                        b8L = buffer[index+7] & 0xffL;
                                        if (endianess == BIG_ENDIAN) {
                                        	tmpLong = ((b1L << 56) | (b2L << 48) | (b3L << 40) | (b4L << 32) |
	                                                   (b5L << 24) | (b6L << 16) | (b7L << 8) | b8L);	
                                        }
                                        else {
                                        	tmpLong = ((b8L << 56) | (b7L << 48) | (b6L << 40) | (b5L << 32) |
	                                                   (b4L << 24) | (b3L << 16) | (b2L << 8) | b1L);
                                        }
                                        imaginaryDBuffer[j++] = Double.longBitsToDouble(tmpLong);
		                    		}
		                    	}
	                    	}
                            
                            if (haveSmallImaginaryData) {
                    		    if (imaginaryDataBytes < 4) {
                    		    	padBytes = 4 - imaginaryDataBytes;
                    		    	for (i = 0; i < padBytes; i++) {
                    		    		raFile.readByte();
                    		    	}
                    		    }
                    		}
                    		else if ((imaginaryDataBytes % 8) != 0) {
                    	    	padBytes = 8 - (imaginaryDataBytes % 8);
                    	    	for (i = 0; i < padBytes; i++) {
                        	    	raFile.readByte();
                        	    }
                    	    }   
                            
                            try {
                    			image.importDComplexData(2 * nonLogicalField * realDBuffer.length, realDBuffer, imaginaryDBuffer, true, logMagDisplay);
                    		}
                    		catch(IOException e) {
                    		   MipavUtil.displayError("IOException on image.importDComplexData(2 * nonLogicalField * realDBuffer.length," +
                    		   		" realDBuffer, imaginaryDBuffer, true, logMagDisplay)");
                    		   throw e;
                    		}
                    	}
                    	break;
                    case miINT64:
                    	Preferences.debug("Real data type = miINT64\n", Preferences.DEBUG_FILEIO);
                    	Preferences.debug("Real data bytes = " + realDataBytes + "\n", Preferences.DEBUG_FILEIO);
                    	if (!complexFlag) {
                    		if (nonLogicalField == 0) {
                    		    image = new ModelImage(ModelStorageBase.LONG, imageExtents, fileName);
                    		    fileInfo.setDataType(ModelStorageBase.LONG);
                    		}
                    		buffer = new byte[realDataBytes];
                    		raFile.read(buffer);
                    		longNumber = realDataBytes/8;
                    		longBuffer = new long[longNumber];
                    		numberSlices = longNumber/sliceSize;
                    		j = 0;
	                    	for (s = 0; s < numberSlices; s++) {
		                    	for (x = 0; x < imageExtents[1]; x++) {
		                    		for (y = 0; y < imageExtents[0]; y++) {
		                    			index = 8*(x + imageExtents[1] * y + s * sliceSize);
                                        b1L = buffer[index] & 0xffL;
                                        b2L = buffer[index+1] & 0xffL;
                                        b3L = buffer[index+2] & 0xffL;
                                        b4L = buffer[index+3] & 0xffL;
                                        b5L = buffer[index+4] & 0xffL;
                                        b6L = buffer[index+5] & 0xffL;
                                        b7L = buffer[index+6] & 0xffL;
                                        b8L = buffer[index+7] & 0xffL;
                                        if (endianess == BIG_ENDIAN) {
                                        	longBuffer[j++] = ((b1L << 56) | (b2L << 48) | (b3L << 40) | (b4L << 32) |
	                                                   (b5L << 24) | (b6L << 16) | (b7L << 8) | b8L);	
                                        }
                                        else {
                                        	longBuffer[j++] = ((b8L << 56) | (b7L << 48) | (b6L << 40) | (b5L << 32) |
	                                                   (b4L << 24) | (b3L << 16) | (b2L << 8) | b1L);
                                        }
		                    		}
		                    	}
	                    	}
                    		
                    		if (haveSmallRealData) {
                    		    if (realDataBytes < 4) {
                    		    	padBytes = 4 - realDataBytes;
                    		    	for (i = 0; i < padBytes; i++) {
                    		    		raFile.readByte();
                    		    	}
                    		    }
                    		}
                    		else if ((realDataBytes % 8) != 0) {
                    	    	padBytes = 8 - (realDataBytes % 8);
                    	    	for (i = 0; i < padBytes; i++) {
                        	    	raFile.readByte();
                        	    }
                    	    }  
                    		try {
                    			image.importData(nonLogicalField * longBuffer.length, longBuffer, true);
                    		}
                    		catch(IOException e) {
                    		   MipavUtil.displayError("IOException on image.importData(nonLogicalField * longBuffer.length, longBuffer, true)");
                    		   throw e;
                    		}
                    	}
                    	else {
                    		if (nonLogicalField == 0) {
                    		    image = new ModelImage(ModelStorageBase.DCOMPLEX, imageExtents, fileName); 
                    		    fileInfo.setDataType(ModelStorageBase.DCOMPLEX);
                    		}
                    		buffer = new byte[realDataBytes];
                    		raFile.read(buffer);
                    		longNumber = realDataBytes/8;
                    		realDBuffer = new double[longNumber];
                    		numberSlices = longNumber/sliceSize;
                    		j = 0;
	                    	for (s = 0; s < numberSlices; s++) {
		                    	for (x = 0; x < imageExtents[1]; x++) {
		                    		for (y = 0; y < imageExtents[0]; y++) {
		                    			index = 8*(x + imageExtents[1] * y + s * sliceSize);
                                        b1L = buffer[index] & 0xffL;
                                        b2L = buffer[index+1] & 0xffL;
                                        b3L = buffer[index+2] & 0xffL;
                                        b4L = buffer[index+3] & 0xffL;
                                        b5L = buffer[index+4] & 0xffL;
                                        b6L = buffer[index+5] & 0xffL;
                                        b7L = buffer[index+6] & 0xffL;
                                        b8L = buffer[index+7] & 0xffL;
                                        if (endianess == BIG_ENDIAN) {
                                        	realDBuffer[j++] = (double)((b1L << 56) | (b2L << 48) | (b3L << 40) | (b4L << 32) |
	                                                   (b5L << 24) | (b6L << 16) | (b7L << 8) | b8L);	
                                        }
                                        else {
                                        	realDBuffer[j++] = (double)((b8L << 56) | (b7L << 48) | (b6L << 40) | (b5L << 32) |
	                                                   (b4L << 24) | (b3L << 16) | (b2L << 8) | b1L);
                                        }
		                    		}
		                    	}
	                    	}
                    		
                    		if (haveSmallRealData) {
                    		    if (realDataBytes < 4) {
                    		    	padBytes = 4 - realDataBytes;
                    		    	for (i = 0; i < padBytes; i++) {
                    		    		raFile.readByte();
                    		    	}
                    		    }
                    		}
                    		else if ((realDataBytes % 8) != 0) {
                    	    	padBytes = 8 - (realDataBytes % 8);
                    	    	for (i = 0; i < padBytes; i++) {
                        	    	raFile.readByte();
                        	    }
                    	    }    
                    	    imaginaryDataType = getInt(endianess);
                    	    if ((imaginaryDataType & 0xffff0000) != 0) {
                                // Small data element format    
                            	imaginaryDataBytes = (imaginaryDataType & 0xffff0000) >>> 16;
                            	imaginaryDataType = imaginaryDataType & 0xffff;
                            	haveSmallImaginaryData = true;
                            }
                            else {
                                imaginaryDataBytes = getInt(endianess);
                                haveSmallImaginaryData = false;
                            }
                    	    if (imaginaryDataType == miINT64) {
                    	    	Preferences.debug("imaginaryDataType == miINT64 as expected\n", Preferences.DEBUG_FILEIO);
                    	    }
                    	    else {
                    	    	Preferences.debug("imaginaryDataType unexpectedly == " + imaginaryDataType + "\n", 
                    	    			Preferences.DEBUG_FILEIO);
                    	    }
                    	    
                            if (imaginaryDataBytes == realDataBytes) {
                            	Preferences.debug("imaginaryDataBytes == realDataBytes as expected\n", 
                            			Preferences.DEBUG_FILEIO);
                            }
                            else {
                            	Preferences.debug("imaginaryDataBytes unexpectedly != realDataBytes\n", 
                            			Preferences.DEBUG_FILEIO);
                            }
                            raFile.read(buffer);
                            imaginaryDBuffer = new double[longNumber];
                    		j = 0;
	                    	for (s = 0; s < numberSlices; s++) {
		                    	for (x = 0; x < imageExtents[1]; x++) {
		                    		for (y = 0; y < imageExtents[0]; y++) {
		                    			index = 8*(x + imageExtents[1] * y + s * sliceSize);
                                        b1L = buffer[index] & 0xffL;
                                        b2L = buffer[index+1] & 0xffL;
                                        b3L = buffer[index+2] & 0xffL;
                                        b4L = buffer[index+3] & 0xffL;
                                        b5L = buffer[index+4] & 0xffL;
                                        b6L = buffer[index+5] & 0xffL;
                                        b7L = buffer[index+6] & 0xffL;
                                        b8L = buffer[index+7] & 0xffL;
                                        if (endianess == BIG_ENDIAN) {
                                        	imaginaryDBuffer[j++] = (double)((b1L << 56) | (b2L << 48) | (b3L << 40) | (b4L << 32) |
	                                                   (b5L << 24) | (b6L << 16) | (b7L << 8) | b8L);	
                                        }
                                        else {
                                        	imaginaryDBuffer[j++] = (double)((b8L << 56) | (b7L << 48) | (b6L << 40) | (b5L << 32) |
	                                                   (b4L << 24) | (b3L << 16) | (b2L << 8) | b1L);
                                        }
		                    		}
		                    	}
	                    	}
                            
                            if (haveSmallImaginaryData) {
                    		    if (imaginaryDataBytes < 4) {
                    		    	padBytes = 4 - imaginaryDataBytes;
                    		    	for (i = 0; i < padBytes; i++) {
                    		    		raFile.readByte();
                    		    	}
                    		    }
                    		}
                    		else if ((imaginaryDataBytes % 8) != 0) {
                    	    	padBytes = 8 - (imaginaryDataBytes % 8);
                    	    	for (i = 0; i < padBytes; i++) {
                        	    	raFile.readByte();
                        	    }
                    	    }   
                            
                            try {
                    			image.importDComplexData(2 * nonLogicalField * realDBuffer.length, realDBuffer, imaginaryDBuffer, true, logMagDisplay);
                    		}
                    		catch(IOException e) {
                    		   MipavUtil.displayError("IOException on image.importDComplexData(2 * nonLogicalField * realDBuffer.length," +
                    		   		" realDBuffer, imaginaryDBuffer, true, logMagDisplay)");
                    		   throw e;
                    		}
                    	}
                    	break;
                    case miUINT64:
                    	Preferences.debug("Real data type = miUINT64\n", Preferences.DEBUG_FILEIO);
                    	Preferences.debug("Real data bytes = " + realDataBytes + "\n", Preferences.DEBUG_FILEIO);
                    	if (!complexFlag) {
                    		if (nonLogicalField == 0) {
                    		    image = new ModelImage(ModelStorageBase.LONG, imageExtents, fileName);
                    		    fileInfo.setDataType(ModelStorageBase.LONG);
                    		}
                    		buffer = new byte[realDataBytes];
                    		raFile.read(buffer);
                    		longNumber = realDataBytes/8;
                    		longBuffer = new long[longNumber];
                    		numberSlices = longNumber/sliceSize;
                    		j = 0;
	                    	for (s = 0; s < numberSlices; s++) {
		                    	for (x = 0; x < imageExtents[1]; x++) {
		                    		for (y = 0; y < imageExtents[0]; y++) {
		                    			index = 8*(x + imageExtents[1] * y + s * sliceSize);
                                        b1L = buffer[index] & 0xffL;
                                        b2L = buffer[index+1] & 0xffL;
                                        b3L = buffer[index+2] & 0xffL;
                                        b4L = buffer[index+3] & 0xffL;
                                        b5L = buffer[index+4] & 0xffL;
                                        b6L = buffer[index+5] & 0xffL;
                                        b7L = buffer[index+6] & 0xffL;
                                        b8L = buffer[index+7] & 0xffL;
                                        if (endianess == BIG_ENDIAN) {
                                        	longBuffer[j++] = ((b1L << 56) | (b2L << 48) | (b3L << 40) | (b4L << 32) |
	                                                   (b5L << 24) | (b6L << 16) | (b7L << 8) | b8L);	
                                        }
                                        else {
                                        	longBuffer[j++] = ((b8L << 56) | (b7L << 48) | (b6L << 40) | (b5L << 32) |
	                                                   (b4L << 24) | (b3L << 16) | (b2L << 8) | b1L);
                                        }
		                    		}
		                    	}
	                    	}
                    		
                    		if (haveSmallRealData) {
                    		    if (realDataBytes < 4) {
                    		    	padBytes = 4 - realDataBytes;
                    		    	for (i = 0; i < padBytes; i++) {
                    		    		raFile.readByte();
                    		    	}
                    		    }
                    		}
                    		else if ((realDataBytes % 8) != 0) {
                    	    	padBytes = 8 - (realDataBytes % 8);
                    	    	for (i = 0; i < padBytes; i++) {
                        	    	raFile.readByte();
                        	    }
                    	    }  
                    		try {
                    			image.importData(nonLogicalField * longBuffer.length, longBuffer, true);
                    		}
                    		catch(IOException e) {
                    		   MipavUtil.displayError("IOException on image.importData(nonLogicalField * longBuffer.length, longBuffer, true)");
                    		   throw e;
                    		}
                    	}
                    	else {
                    		if (nonLogicalField == 0) {
                    		    image = new ModelImage(ModelStorageBase.DCOMPLEX, imageExtents, fileName);
                    		    fileInfo.setDataType(ModelStorageBase.DCOMPLEX);
                    		}
                    		buffer = new byte[realDataBytes];
                    		raFile.read(buffer);
                    		longNumber = realDataBytes/8;
                    		realDBuffer = new double[longNumber];
                    		numberSlices = longNumber/sliceSize;
                    		j = 0;
	                    	for (s = 0; s < numberSlices; s++) {
		                    	for (x = 0; x < imageExtents[1]; x++) {
		                    		for (y = 0; y < imageExtents[0]; y++) {
		                    			index = 8*(x + imageExtents[1] * y + s * sliceSize);
                                        b1L = buffer[index] & 0xffL;
                                        b2L = buffer[index+1] & 0xffL;
                                        b3L = buffer[index+2] & 0xffL;
                                        b4L = buffer[index+3] & 0xffL;
                                        b5L = buffer[index+4] & 0xffL;
                                        b6L = buffer[index+5] & 0xffL;
                                        b7L = buffer[index+6] & 0xffL;
                                        b8L = buffer[index+7] & 0xffL;
                                        if (endianess == BIG_ENDIAN) {
                                        	realDBuffer[j++] = (double)((b1L << 56) | (b2L << 48) | (b3L << 40) | (b4L << 32) |
	                                                   (b5L << 24) | (b6L << 16) | (b7L << 8) | b8L);	
                                        }
                                        else {
                                        	realDBuffer[j++] = (double)((b8L << 56) | (b7L << 48) | (b6L << 40) | (b5L << 32) |
	                                                   (b4L << 24) | (b3L << 16) | (b2L << 8) | b1L);
                                        }
		                    		}
		                    	}
	                    	}
                    		
                    		if (haveSmallRealData) {
                    		    if (realDataBytes < 4) {
                    		    	padBytes = 4 - realDataBytes;
                    		    	for (i = 0; i < padBytes; i++) {
                    		    		raFile.readByte();
                    		    	}
                    		    }
                    		}
                    		else if ((realDataBytes % 8) != 0) {
                    	    	padBytes = 8 - (realDataBytes % 8);
                    	    	for (i = 0; i < padBytes; i++) {
                        	    	raFile.readByte();
                        	    }
                    	    }   
                    	    imaginaryDataType = getInt(endianess);
                    	    if ((imaginaryDataType & 0xffff0000) != 0) {
                                // Small data element format    
                            	imaginaryDataBytes = (imaginaryDataType & 0xffff0000) >>> 16;
                            	imaginaryDataType = imaginaryDataType & 0xffff;
                            	haveSmallImaginaryData = true;
                            }
                            else {
                                imaginaryDataBytes = getInt(endianess);
                                haveSmallImaginaryData = false;
                            }
                    	    if (imaginaryDataType == miUINT64) {
                    	    	Preferences.debug("imaginaryDataType == miUINT64 as expected\n", Preferences.DEBUG_FILEIO);
                    	    }
                    	    else {
                    	    	Preferences.debug("imaginaryDataType unexpectedly == " + imaginaryDataType + "\n", 
                    	    			Preferences.DEBUG_FILEIO);
                    	    }
                    	    
                            if (imaginaryDataBytes == realDataBytes) {
                            	Preferences.debug("imaginaryDataBytes == realDataBytes as expected\n", 
                            			Preferences.DEBUG_FILEIO);
                            }
                            else {
                            	Preferences.debug("imaginaryDataBytes unexpectedly != realDataBytes\n", 
                            			Preferences.DEBUG_FILEIO);
                            }
                            raFile.read(buffer);
                            imaginaryDBuffer = new double[longNumber];
                            numberSlices = longNumber/sliceSize;
                    		j = 0;
	                    	for (s = 0; s < numberSlices; s++) {
		                    	for (x = 0; x < imageExtents[1]; x++) {
		                    		for (y = 0; y < imageExtents[0]; y++) {
		                    			index = 8*(x + imageExtents[1] * y + s * sliceSize);
                                        b1L = buffer[index] & 0xffL;
                                        b2L = buffer[index+1] & 0xffL;
                                        b3L = buffer[index+2] & 0xffL;
                                        b4L = buffer[index+3] & 0xffL;
                                        b5L = buffer[index+4] & 0xffL;
                                        b6L = buffer[index+5] & 0xffL;
                                        b7L = buffer[index+6] & 0xffL;
                                        b8L = buffer[index+7] & 0xffL;
                                        if (endianess == BIG_ENDIAN) {
                                        	imaginaryDBuffer[j++] = (double)((b1L << 56) | (b2L << 48) | (b3L << 40) | (b4L << 32) |
	                                                   (b5L << 24) | (b6L << 16) | (b7L << 8) | b8L);	
                                        }
                                        else {
                                        	imaginaryDBuffer[j++] = (double)((b8L << 56) | (b7L << 48) | (b6L << 40) | (b5L << 32) |
	                                                   (b4L << 24) | (b3L << 16) | (b2L << 8) | b1L);
                                        }
		                    		}
		                    	}
	                    	}
                            
                            if (haveSmallImaginaryData) {
                    		    if (imaginaryDataBytes < 4) {
                    		    	padBytes = 4 - imaginaryDataBytes;
                    		    	for (i = 0; i < padBytes; i++) {
                    		    		raFile.readByte();
                    		    	}
                    		    }
                    		}
                    		else if ((imaginaryDataBytes % 8) != 0) {
                    	    	padBytes = 8 - (imaginaryDataBytes % 8);
                    	    	for (i = 0; i < padBytes; i++) {
                        	    	raFile.readByte();
                        	    }
                    	    }   
                            
                            try {
                    			image.importDComplexData(2 * nonLogicalField * realDBuffer.length, realDBuffer, imaginaryDBuffer, true, logMagDisplay);
                    		}
                    		catch(IOException e) {
                    		   MipavUtil.displayError("IOException on image.importDComplexData(2 * nonLogicalField * realDBuffer.length, " +
                    		   		"realDBuffer, imaginaryDBuffer, true, logMagDisplay)");
                    		   throw e;
                    		}
                    	}
                    	break;
                    default:
                    	Preferences.debug("Illegal data type = " + realDataType + "\n", Preferences.DEBUG_FILEIO);
                    	Preferences.debug("Real data bytes = " + realDataBytes + "\n", Preferences.DEBUG_FILEIO);
                    }
                    
                    } // if (imagesFound == 1)
                    else { // imagesFound > 1
                    	switch(realDataType) {
                        case miINT8:
                        	Preferences.debug("Real data type = miINT8\n", Preferences.DEBUG_FILEIO);
                        	Preferences.debug("Real data bytes = " + realDataBytes + "\n", Preferences.DEBUG_FILEIO);
                        	if (logicalFlag) {
                        		if (logicalFields == 1) {
    	                    	    maskImage2 = new ModelImage(ModelStorageBase.BYTE, maskExtents, fileName + "_mask");
    	                    	    maskFileInfo2.setDataType(ModelStorageBase.BYTE);
    	                    	    buffer = new byte[realDataBytes];
    		                    	raFile.read(buffer);
    		                    	tBuffer = new byte[buffer.length];
    	                    		numberSlices = buffer.length/sliceSize;
    		                    	j = 0;
    		                    	for (s = 0; s < numberSlices; s++) {
    			                    	for (x = 0; x < imageExtents[1]; x++) {
    			                    		for (y = 0; y < imageExtents[0]; y++) {
                                                tBuffer[j++] = buffer[x + imageExtents[1] * y + s * sliceSize];			                    			
    			                    		}
    			                    	}
    		                    	}
    		                    	if (haveSmallRealData) {
    	                    		    if (realDataBytes < 4) {
    	                    		    	padBytes = 4 - realDataBytes;
    	                    		    	for (i = 0; i < padBytes; i++) {
    	                    		    		raFile.readByte();
    	                    		    	}
    	                    		    }
    	                    		}
    	                    		else if ((realDataBytes % 8) != 0) {
    	                    	    	padBytes = 8 - (realDataBytes % 8);
    	                    	    	for (i = 0; i < padBytes; i++) {
    	                        	    	raFile.readByte();
    	                        	    }
    	                    	    }  
    		                    	try {
    		                			maskImage2.importData(0, tBuffer, true);
    		                		}
    		                		catch(IOException e) {
    		                		   MipavUtil.displayError("IOException on maskImage2.importData(0, tBuffer, true)");
    		                		   throw e;
    		                		}
                        	    } // if (logicalFields == 1)	
                        	}
                        	else if (!complexFlag) {
                        		if (nonLogicalField == 0) {
    	                    	    image2 = new ModelImage(ModelStorageBase.BYTE, imageExtents, fileName);
    	                    	    fileInfo2.setDataType(ModelStorageBase.BYTE);
                        		}
    	                    	buffer = new byte[realDataBytes];
    	                    	raFile.read(buffer);
    	                    	tBuffer = new byte[buffer.length];
	                    		numberSlices = buffer.length/sliceSize;
		                    	j = 0;
		                    	for (s = 0; s < numberSlices; s++) {
			                    	for (x = 0; x < imageExtents[1]; x++) {
			                    		for (y = 0; y < imageExtents[0]; y++) {
                                            tBuffer[j++] = buffer[x + imageExtents[1] * y + s * sliceSize];			                    			
			                    		}
			                    	}
		                    	}
    	                    	if (haveSmallRealData) {
	                    		    if (realDataBytes < 4) {
	                    		    	padBytes = 4 - realDataBytes;
	                    		    	for (i = 0; i < padBytes; i++) {
	                    		    		raFile.readByte();
	                    		    	}
	                    		    }
	                    		}
	                    		else if ((realDataBytes % 8) != 0) {
	                    	    	padBytes = 8 - (realDataBytes % 8);
	                    	    	for (i = 0; i < padBytes; i++) {
	                        	    	raFile.readByte();
	                        	    }
	                    	    }  
    	                    	try {
    	                			image2.importData(nonLogicalField * tBuffer.length, tBuffer, true);
    	                		}
    	                		catch(IOException e) {
    	                		   MipavUtil.displayError("IOException on image2.importData(nonLogicalField * tBuffer.length, tBuffer, true)");
    	                		   throw e;
    	                		}
                        	}
                        	else {
                        		if (nonLogicalField == 0) {
                        		    image2 = new ModelImage(ModelStorageBase.COMPLEX, imageExtents, fileName); 
                        		    fileInfo2.setDataType(ModelStorageBase.COMPLEX);
                        		}
                        		buffer = new byte[realDataBytes];
                        		raFile.read(buffer);
                        		realBuffer = new float[realDataBytes];
                        		numberSlices = buffer.length/sliceSize;
                        		j = 0;
    	                    	for (s = 0; s < numberSlices; s++) {
    		                    	for (x = 0; x < imageExtents[1]; x++) {
    		                    		for (y = 0; y < imageExtents[0]; y++) {
                                            realBuffer[j++] = (float)buffer[x + imageExtents[1] * y + s * sliceSize];			                    			
    		                    		}
    		                    	}
    	                    	}
                        		
                        		if (haveSmallRealData) {
	                    		    if (realDataBytes < 4) {
	                    		    	padBytes = 4 - realDataBytes;
	                    		    	for (i = 0; i < padBytes; i++) {
	                    		    		raFile.readByte();
	                    		    	}
	                    		    }
	                    		}
	                    		else if ((realDataBytes % 8) != 0) {
	                    	    	padBytes = 8 - (realDataBytes % 8);
	                    	    	for (i = 0; i < padBytes; i++) {
	                        	    	raFile.readByte();
	                        	    }
	                    	    }    
                        	    imaginaryDataType = getInt(endianess);
                        	    if ((imaginaryDataType & 0xffff0000) != 0) {
                                    // Small data element format    
                                	imaginaryDataBytes = (imaginaryDataType & 0xffff0000) >>> 16;
                                	imaginaryDataType = imaginaryDataType & 0xffff;
                                	haveSmallImaginaryData = true;
                                }
                                else {
                                    imaginaryDataBytes = getInt(endianess);
                                    haveSmallImaginaryData = false;
                                }
                        	    if (imaginaryDataType == miINT8) {
                        	    	Preferences.debug("imaginaryDataType == miINT8 as expected\n", Preferences.DEBUG_FILEIO);
                        	    }
                        	    else {
                        	    	Preferences.debug("imaginaryDataType unexpectedly == " + imaginaryDataType + "\n", 
                        	    			Preferences.DEBUG_FILEIO);
                        	    }
                               
                                if (imaginaryDataBytes == realDataBytes) {
                                	Preferences.debug("imaginaryDataBytes == realDataBytes as expected\n", 
                                			Preferences.DEBUG_FILEIO);
                                }
                                else {
                                	Preferences.debug("imaginaryDataBytes unexpectedly != realDataBytes\n", 
                                			Preferences.DEBUG_FILEIO);
                                }
                                raFile.read(buffer);
                                imaginaryBuffer = new float[imaginaryDataBytes];
                                j = 0;
    	                    	for (s = 0; s < numberSlices; s++) {
    		                    	for (x = 0; x < imageExtents[1]; x++) {
    		                    		for (y = 0; y < imageExtents[0]; y++) {
                                            imaginaryBuffer[j++] = (float)buffer[x + imageExtents[1] * y + s * sliceSize];			                    			
    		                    		}
    		                    	}
    	                    	}
                                
                                if (haveSmallImaginaryData) {
                        		    if (imaginaryDataBytes < 4) {
                        		    	padBytes = 4 - imaginaryDataBytes;
                        		    	for (i = 0; i < padBytes; i++) {
                        		    		raFile.readByte();
                        		    	}
                        		    }
                        		}
                        		else if ((imaginaryDataBytes % 8) != 0) {
                        	    	padBytes = 8 - (imaginaryDataBytes % 8);
                        	    	for (i = 0; i < padBytes; i++) {
                            	    	raFile.readByte();
                            	    }
                        	    }   
                                
                                try {
                        			image2.importComplexData(2 * nonLogicalField * realBuffer.length, realBuffer, imaginaryBuffer, true, logMagDisplay);
                        		}
                        		catch(IOException e) {
                        		   MipavUtil.displayError("IOException on image2.importComplexData(2 * nonLogicalField * realBuffer.length, " +
                        		   		"realBuffer, imaginaryBuffer, true, logMagDisplay)");
                        		   throw e;
                        		}		
                        	}
                        	break;
                        case miUINT8:
                        	Preferences.debug("Real data type = miUINT8\n", Preferences.DEBUG_FILEIO);
                        	Preferences.debug("Real data bytes = " + realDataBytes + "\n", Preferences.DEBUG_FILEIO);
                        	if (logicalFlag) {
                        		if (logicalFields == 1) {
    	                    	    maskImage2 = new ModelImage(ModelStorageBase.UBYTE, maskExtents, fileName + "_mask");
    	                    	    maskFileInfo2.setDataType(ModelStorageBase.UBYTE);
    	                    	    buffer = new byte[realDataBytes];
    	                    	    shortBuffer = new short[realDataBytes];
    	                    	    raFile.read(buffer);
    	                    	    numberSlices = buffer.length/sliceSize;
    	                    	    j = 0;
    		                    	for (s = 0; s < numberSlices; s++) {
    			                    	for (x = 0; x < imageExtents[1]; x++) {
    			                    		for (y = 0; y < imageExtents[0]; y++) {
                                                shortBuffer[j++] = (short) (buffer[x + imageExtents[1] * y + s * sliceSize] & 0xff);			                    			
    			                    		}
    			                    	}
    		                    	}
    	                    	    
    	                    	    if (haveSmallRealData) {
    	                    		    if (realDataBytes < 4) {
    	                    		    	padBytes = 4 - realDataBytes;
    	                    		    	for (i = 0; i < padBytes; i++) {
    	                    		    		raFile.readByte();
    	                    		    	}
    	                    		    }
    	                    		}
    	                    		else if ((realDataBytes % 8) != 0) {
    	                    	    	padBytes = 8 - (realDataBytes % 8);
    	                    	    	for (i = 0; i < padBytes; i++) {
    	                        	    	raFile.readByte();
    	                        	    }
    	                    	    }  
    		                    	try {
    		                			maskImage2.importUData(0, shortBuffer, true);
    		                		}
    		                		catch(IOException e) {
    		                		   MipavUtil.displayError("IOException on maskImage2.importUData(0, shortBuffer, true)");
    		                		   throw e;
    		                		}
                        	    } // if (logicalFields == 1)		
                        	}
                        	else if (isColor) {
                        		if (nonLogicalField == 0) {
                        			image2 = new ModelImage(ModelStorageBase.ARGB, imageExtents, fileName);
                        			fileInfo2.setDataType(ModelStorageBase.ARGB);
                        		}
                        		if ((realDataBytes % 3) == 0) {
    	                    		buffer = new byte[realDataBytes/3];
    	                    		tBuffer = new byte[buffer.length];
    	                    		numberSlices = buffer.length/sliceSize;
    	                    		for (i = 0; i < 3; i++) {
    			                    	raFile.read(buffer);
    			                    	j = 0;
    			                    	for (s = 0; s < numberSlices; s++) {
    				                    	for (x = 0; x < imageExtents[1]; x++) {
    				                    		for (y = 0; y < imageExtents[0]; y++) {
    	                                            tBuffer[j++] = buffer[x + imageExtents[1] * y + s * sliceSize];			                    			
    				                    		}
    				                    	}
    			                    	}
    			                    	
    			                    	try {
    			                			image2.importRGBData(i+1, nonLogicalField * tBuffer.length, tBuffer, true);
    			                		}
    			                		catch(IOException e) {
    			                		   MipavUtil.displayError("IOException on image2.importRGBData(i," +
    			                		   		" nonLogicalField * tBuffer.length, tBuffer, true)");
    			                		   throw e;
    			                		}
    	                    		} // if (i = 0; i < 3; i++)
    	
    		                    	
    		                    	if (haveSmallRealData) {
    	                    		    if (realDataBytes < 4) {
    	                    		    	padBytes = 4 - realDataBytes;
    	                    		    	for (i = 0; i < padBytes; i++) {
    	                    		    		raFile.readByte();
    	                    		    	}
    	                    		    }
    	                    		}
    	                    		else if ((realDataBytes % 8) != 0) {
    	                    	    	padBytes = 8 - (realDataBytes % 8);
    	                    	    	for (i = 0; i < padBytes; i++) {
    	                        	    	raFile.readByte();
    	                        	    }
    	                    	    } 
    	                    	
    	                        } // if (realDataBytes % 3) == 0)
                        	} // else if (isColor)
                        	else if (!complexFlag) {
                        		if (nonLogicalField == 0) {
    	                    	    image2 = new ModelImage(ModelStorageBase.UBYTE, imageExtents, fileName);
    	                    	    fileInfo2.setDataType(ModelStorageBase.UBYTE);
                        		}
    	                    	buffer = new byte[realDataBytes];
    	                    	raFile.read(buffer);
    	                    	shortBuffer = new short[realDataBytes];
    	                    	numberSlices = buffer.length/sliceSize;
    	                    	j = 0;
    	                    	for (s = 0; s < numberSlices; s++) {
    		                    	for (x = 0; x < imageExtents[1]; x++) {
    		                    		for (y = 0; y < imageExtents[0]; y++) {
                                            shortBuffer[j++] = (short) (buffer[x + imageExtents[1] * y + s * sliceSize] & 0xff);			                    			
    		                    		}
    		                    	}
    	                    	}
    	                    	
    	                    	if (haveSmallRealData) {
	                    		    if (realDataBytes < 4) {
	                    		    	padBytes = 4 - realDataBytes;
	                    		    	for (i = 0; i < padBytes; i++) {
	                    		    		raFile.readByte();
	                    		    	}
	                    		    }
	                    		}
	                    		else if ((realDataBytes % 8) != 0) {
	                    	    	padBytes = 8 - (realDataBytes % 8);
	                    	    	for (i = 0; i < padBytes; i++) {
	                        	    	raFile.readByte();
	                        	    }
	                    	    }  
    	                    	try {
    	                			image2.importUData(nonLogicalField * shortBuffer.length, shortBuffer, true);
    	                		}
    	                		catch(IOException e) {
    	                		   MipavUtil.displayError("IOException on image2.importData(nonLogicalField * shortBuffer.length, shortBuffer, true)");
    	                		   throw e;
    	                		}
                        	}
                        	else {
                        		if (nonLogicalField == 0) {
                        		    image2 = new ModelImage(ModelStorageBase.COMPLEX, imageExtents, fileName); 
                        		    fileInfo2.setDataType(ModelStorageBase.COMPLEX);
                        		}
                        		buffer = new byte[realDataBytes];
                        		raFile.read(buffer);
                        		realBuffer = new float[realDataBytes];
                        		numberSlices = buffer.length/sliceSize;
                        	    j = 0;
    	                    	for (s = 0; s < numberSlices; s++) {
    		                    	for (x = 0; x < imageExtents[1]; x++) {
    		                    		for (y = 0; y < imageExtents[0]; y++) {
                                            realBuffer[j++] = (float) (buffer[x + imageExtents[1] * y + s * sliceSize] & 0xff);			                    			
    		                    		}
    		                    	}
    	                    	}
                        		
                        		if (haveSmallRealData) {
	                    		    if (realDataBytes < 4) {
	                    		    	padBytes = 4 - realDataBytes;
	                    		    	for (i = 0; i < padBytes; i++) {
	                    		    		raFile.readByte();
	                    		    	}
	                    		    }
	                    		}
	                    		else if ((realDataBytes % 8) != 0) {
	                    	    	padBytes = 8 - (realDataBytes % 8);
	                    	    	for (i = 0; i < padBytes; i++) {
	                        	    	raFile.readByte();
	                        	    }
	                    	    }   
                        	    imaginaryDataType = getInt(endianess);
                        	    if ((imaginaryDataType & 0xffff0000) != 0) {
                                    // Small data element format    
                                	imaginaryDataBytes = (imaginaryDataType & 0xffff0000) >>> 16;
                                	imaginaryDataType = imaginaryDataType & 0xffff;
                                	haveSmallImaginaryData = true;
                                }
                                else {
                                    imaginaryDataBytes = getInt(endianess);
                                    haveSmallImaginaryData = false;
                                }
                        	    if (imaginaryDataType == miUINT8) {
                        	    	Preferences.debug("imaginaryDataType == miUINT8 as expected\n", Preferences.DEBUG_FILEIO);
                        	    }
                        	    else {
                        	    	Preferences.debug("imaginaryDataType unexpectedly == " + imaginaryDataType + "\n", 
                        	    			Preferences.DEBUG_FILEIO);
                        	    }
                                
                                if (imaginaryDataBytes == realDataBytes) {
                                	Preferences.debug("imaginaryDataBytes == realDataBytes as expected\n", 
                                			Preferences.DEBUG_FILEIO);
                                }
                                else {
                                	Preferences.debug("imaginaryDataBytes unexpectedly != realDataBytes\n", 
                                			Preferences.DEBUG_FILEIO);
                                }
                                raFile.read(buffer);
                                imaginaryBuffer = new float[imaginaryDataBytes];
                                j = 0;
    	                    	for (s = 0; s < numberSlices; s++) {
    		                    	for (x = 0; x < imageExtents[1]; x++) {
    		                    		for (y = 0; y < imageExtents[0]; y++) {
                                            imaginaryBuffer[j++] = (float) (buffer[x + imageExtents[1] * y + s * sliceSize] & 0xff);			                    			
    		                    		}
    		                    	}
    	                    	}
                                
                                if (haveSmallImaginaryData) {
                        		    if (imaginaryDataBytes < 4) {
                        		    	padBytes = 4 - imaginaryDataBytes;
                        		    	for (i = 0; i < padBytes; i++) {
                        		    		raFile.readByte();
                        		    	}
                        		    }
                        		}
                        		else if ((imaginaryDataBytes % 8) != 0) {
                        	    	padBytes = 8 - (imaginaryDataBytes % 8);
                        	    	for (i = 0; i < padBytes; i++) {
                            	    	raFile.readByte();
                            	    }
                        	    }   
                                
                                try {
                        			image2.importComplexData(2 * nonLogicalField * realBuffer.length, realBuffer, imaginaryBuffer, true, logMagDisplay);
                        		}
                        		catch(IOException e) {
                        		   MipavUtil.displayError("IOException on image2.importComplexData(2 * nonLogicalField * realBuffer.length," +
                        		   		" realBuffer, imaginaryBuffer, true, logMagDisplay)");
                        		   throw e;
                        		}	
                        	}
                        	break;
                        case miINT16:
                        	Preferences.debug("Real data type = miINT16\n", Preferences.DEBUG_FILEIO);
                        	Preferences.debug("Real data bytes = " + realDataBytes + "\n", Preferences.DEBUG_FILEIO);
                        	if (!complexFlag) {
                        		if (nonLogicalField == 0) {
                        		    image2 = new ModelImage(ModelStorageBase.SHORT, imageExtents, fileName); 
                        		    fileInfo2.setDataType(ModelStorageBase.SHORT);
                        		}
                        		buffer = new byte[realDataBytes];
                        		raFile.read(buffer);
                        		shortNumber = realDataBytes/2;
                        		shortBuffer = new short[shortNumber];
                        		numberSlices = shortNumber/sliceSize;
                        		j = 0;
    	                    	for (s = 0; s < numberSlices; s++) {
    		                    	for (x = 0; x < imageExtents[1]; x++) {
    		                    		for (y = 0; y < imageExtents[0]; y++) {
    		                    			index = 2*(x + imageExtents[1] * y + s * sliceSize);
                                            b1 = buffer[index] & 0xff;
                                            b2 = buffer[index+1] & 0xff;
                                            if (endianess == BIG_ENDIAN) {
                                            	shortBuffer[j++] = (short)((b1 << 8) | b2);	
                                            }
                                            else {
                                            	shortBuffer[j++] = (short)((b2 << 8) | b1);
                                            }
    		                    		}
    		                    	}
    	                    	}
                        		
                        		if (haveSmallRealData) {
	                    		    if (realDataBytes < 4) {
	                    		    	padBytes = 4 - realDataBytes;
	                    		    	for (i = 0; i < padBytes; i++) {
	                    		    		raFile.readByte();
	                    		    	}
	                    		    }
	                    		}
	                    		else if ((realDataBytes % 8) != 0) {
	                    	    	padBytes = 8 - (realDataBytes % 8);
	                    	    	for (i = 0; i < padBytes; i++) {
	                        	    	raFile.readByte();
	                        	    }
	                    	    }  
                        		try {
                        			image2.importData(nonLogicalField * shortBuffer.length, shortBuffer, true);
                        		}
                        		catch(IOException e) {
                        		   MipavUtil.displayError("IOException on image2.importData(nonLogicalField * shortBuffer.length, shortBuffer, true)");
                        		   throw e;
                        		}
                        	}
                        	else {
                        		if (nonLogicalField == 0) {
                        		    image2 = new ModelImage(ModelStorageBase.COMPLEX, imageExtents, fileName); 
                        		    fileInfo2.setDataType(ModelStorageBase.COMPLEX);
                        		}
                        		buffer = new byte[realDataBytes];
                        		raFile.read(buffer);
                        		shortNumber = realDataBytes/2;
                        		realBuffer = new float[shortNumber];
                        		numberSlices = shortNumber/sliceSize;
                        		j = 0;
    	                    	for (s = 0; s < numberSlices; s++) {
    		                    	for (x = 0; x < imageExtents[1]; x++) {
    		                    		for (y = 0; y < imageExtents[0]; y++) {
    		                    			index = 2*(x + imageExtents[1] * y + s * sliceSize);
                                            b1 = buffer[index] & 0xff;
                                            b2 = buffer[index+1] & 0xff;
                                            if (endianess == BIG_ENDIAN) {
                                            	realBuffer[j++] = (float)((b1 << 8) | b2);	
                                            }
                                            else {
                                            	realBuffer[j++] = (float)((b2 << 8) | b1);
                                            }
    		                    		}
    		                    	}
    	                    	}
                        		
                        		if (haveSmallRealData) {
	                    		    if (realDataBytes < 4) {
	                    		    	padBytes = 4 - realDataBytes;
	                    		    	for (i = 0; i < padBytes; i++) {
	                    		    		raFile.readByte();
	                    		    	}
	                    		    }
	                    		}
	                    		else if ((realDataBytes % 8) != 0) {
	                    	    	padBytes = 8 - (realDataBytes % 8);
	                    	    	for (i = 0; i < padBytes; i++) {
	                        	    	raFile.readByte();
	                        	    }
	                    	    }  
                        	    imaginaryDataType = getInt(endianess);
                        	    if ((imaginaryDataType & 0xffff0000) != 0) {
                                    // Small data element format    
                                	imaginaryDataBytes = (imaginaryDataType & 0xffff0000) >>> 16;
                                	imaginaryDataType = imaginaryDataType & 0xffff;
                                	haveSmallImaginaryData = true;
                                }
                                else {
                                    imaginaryDataBytes = getInt(endianess);
                                    haveSmallImaginaryData = false;
                                }
                        	    if (imaginaryDataType == miINT16) {
                        	    	Preferences.debug("imaginaryDataType == miINT16 as expected\n", Preferences.DEBUG_FILEIO);
                        	    }
                        	    else {
                        	    	Preferences.debug("imaginaryDataType unexpectedly == " + imaginaryDataType + "\n", 
                        	    			Preferences.DEBUG_FILEIO);
                        	    }
                                
                                if (imaginaryDataBytes == realDataBytes) {
                                	Preferences.debug("imaginaryDataBytes == realDataBytes as expected\n", 
                                			Preferences.DEBUG_FILEIO);
                                }
                                else {
                                	Preferences.debug("imaginaryDataBytes unexpectedly != realDataBytes\n", 
                                			Preferences.DEBUG_FILEIO);
                                }
                                raFile.read(buffer);
                                imaginaryBuffer = new float[shortNumber];
                                j = 0;
    	                    	for (s = 0; s < numberSlices; s++) {
    		                    	for (x = 0; x < imageExtents[1]; x++) {
    		                    		for (y = 0; y < imageExtents[0]; y++) {
    		                    			index = 2*(x + imageExtents[1] * y + s * sliceSize);
                                            b1 = buffer[index] & 0xff;
                                            b2 = buffer[index+1] & 0xff;
                                            if (endianess == BIG_ENDIAN) {
                                            	imaginaryBuffer[j++] = (float)((b1 << 8) | b2);	
                                            }
                                            else {
                                            	imaginaryBuffer[j++] = (float)((b2 << 8) | b1);
                                            }
    		                    		}
    		                    	}
    	                    	}
                                
                                if (haveSmallImaginaryData) {
                        		    if (imaginaryDataBytes < 4) {
                        		    	padBytes = 4 - imaginaryDataBytes;
                        		    	for (i = 0; i < padBytes; i++) {
                        		    		raFile.readByte();
                        		    	}
                        		    }
                        		}
                        		else if ((imaginaryDataBytes % 8) != 0) {
                        	    	padBytes = 8 - (imaginaryDataBytes % 8);
                        	    	for (i = 0; i < padBytes; i++) {
                            	    	raFile.readByte();
                            	    }
                        	    }   
                                
                                try {
                        			image2.importComplexData(2 * nonLogicalField * realBuffer.length, realBuffer, imaginaryBuffer, true, logMagDisplay);
                        		}
                        		catch(IOException e) {
                        		   MipavUtil.displayError("IOException on image2.importComplexData(2 * nonLogicalField * realBuffer.length, " +
                        		   		"realBuffer, imaginaryBuffer, true, logMagDisplay)");
                        		   throw e;
                        		}		
                        	}
                        	break;
                        case miUINT16:
                        	Preferences.debug("Real data type = miUINT16\n", Preferences.DEBUG_FILEIO);
                        	Preferences.debug("Real data bytes = " + realDataBytes + "\n", Preferences.DEBUG_FILEIO);
                        	if (isColor) {
                        		if (nonLogicalField == 0) {
                        			image2 = new ModelImage(ModelStorageBase.ARGB_USHORT, imageExtents, fileName);
                        			fileInfo2.setDataType(ModelStorageBase.ARGB_USHORT);
                        		}
                        		if ((realDataBytes % 3) == 0) {
    	                    		buffer = new byte[realDataBytes/3];
    	                    		shortNumber = realDataBytes/6;
    	                    		shortBuffer = new short[shortNumber];
    	                    		numberSlices = shortNumber/sliceSize;
		                    		for (i = 0; i < 3; i++) {
				                    	raFile.read(buffer);
				                    	j = 0;
				                    	for (s = 0; s < numberSlices; s++) {
					                    	for (x = 0; x < imageExtents[1]; x++) {
					                    		for (y = 0; y < imageExtents[0]; y++) {
					                    			index = 2*(x + imageExtents[1] * y + s * sliceSize);
		                                            b1 = buffer[index] & 0xff;
		                                            b2 = buffer[index+1] & 0xff;
		                                            if (endianess == BIG_ENDIAN) {
		                                            	shortBuffer[j++] = (short) ((b1 << 8) | b2);	
		                                            }
		                                            else {
		                                            	shortBuffer[j++] = (short) ((b2 << 8) | b1);
		                                            }
					                    		}
					                    	}
				                    	}
    			                    	
    			                    	try {
    			                			image2.importRGBData(i+1, nonLogicalField * shortBuffer.length, shortBuffer, true);
    			                		}
    			                		catch(IOException e) {
    			                		   MipavUtil.displayError("IOException on image2.importRGBData(i," +
    			                		   		" nonLogicalField * shortBuffer.length, shortBuffer, true)");
    			                		   throw e;
    			                		}
    	                    		} // if (i = 0; i < 3; i++)
    	
    		                    	
    		                    	if (haveSmallRealData) {
    	                    		    if (realDataBytes < 4) {
    	                    		    	padBytes = 4 - realDataBytes;
    	                    		    	for (i = 0; i < padBytes; i++) {
    	                    		    		raFile.readByte();
    	                    		    	}
    	                    		    }
    	                    		}
    	                    		else if ((realDataBytes % 8) != 0) {
    	                    	    	padBytes = 8 - (realDataBytes % 8);
    	                    	    	for (i = 0; i < padBytes; i++) {
    	                        	    	raFile.readByte();
    	                        	    }
    	                    	    } 
    	                    	
    	                        } // if (realDataBytes % 3) == 0)
                        	} // if (isColor)
                        	else if (!complexFlag) {
                        		if (nonLogicalField == 0) {
                        		    image2 = new ModelImage(ModelStorageBase.USHORT, imageExtents, fileName); 
                        		    fileInfo2.setDataType(ModelStorageBase.USHORT);
                        		}
                        		buffer =  new byte[realDataBytes];
                        		raFile.read(buffer);
                        		shortNumber = realDataBytes/2;
                        		intBuffer = new int[shortNumber];
                        		numberSlices = shortNumber/sliceSize;
                        		j = 0;
    	                    	for (s = 0; s < numberSlices; s++) {
    		                    	for (x = 0; x < imageExtents[1]; x++) {
    		                    		for (y = 0; y < imageExtents[0]; y++) {
    		                    			index = 2*(x + imageExtents[1] * y + s * sliceSize);
                                            b1 = buffer[index] & 0xff;
                                            b2 = buffer[index+1] & 0xff;
                                            if (endianess == BIG_ENDIAN) {
                                            	intBuffer[j++] = ((b1 << 8) | b2);	
                                            }
                                            else {
                                            	intBuffer[j++] = ((b2 << 8) | b1);
                                            }
    		                    		}
    		                    	}
    	                    	}
                        		
                        		if (haveSmallRealData) {
	                    		    if (realDataBytes < 4) {
	                    		    	padBytes = 4 - realDataBytes;
	                    		    	for (i = 0; i < padBytes; i++) {
	                    		    		raFile.readByte();
	                    		    	}
	                    		    }
	                    		}
	                    		else if ((realDataBytes % 8) != 0) {
	                    	    	padBytes = 8 - (realDataBytes % 8);
	                    	    	for (i = 0; i < padBytes; i++) {
	                        	    	raFile.readByte();
	                        	    }
	                    	    }  
                        		try {
                        			image2.importUData(nonLogicalField * intBuffer.length, intBuffer, true);
                        		}
                        		catch(IOException e) {
                        		   MipavUtil.displayError("IOException on image2.importUData(nonLogicalField * intBuffer.length, intBuffer, true)");
                        		   throw e;
                        		}
                        	}
                        	else {
                        		if (nonLogicalField == 0) {
                        		    image2 = new ModelImage(ModelStorageBase.COMPLEX, imageExtents, fileName); 
                        		    fileInfo2.setDataType(ModelStorageBase.COMPLEX);
                        		}
                        		buffer = new byte[realDataBytes];
                        		raFile.read(buffer);
                        		shortNumber = realDataBytes/2;
                        		realBuffer = new float[shortNumber];
                        		numberSlices = shortNumber/sliceSize;
                        		j = 0;
    	                    	for (s = 0; s < numberSlices; s++) {
    		                    	for (x = 0; x < imageExtents[1]; x++) {
    		                    		for (y = 0; y < imageExtents[0]; y++) {
    		                    			index = 2*(x + imageExtents[1] * y + s * sliceSize);
                                            b1 = buffer[index] & 0xff;
                                            b2 = buffer[index+1] & 0xff;
                                            if (endianess == BIG_ENDIAN) {
                                                realBuffer[j++] = (float) ((b1 << 8) | b2);	
                                            }
                                            else {
                                            	realBuffer[j++] = (float) ((b2 << 8) | b1);
                                            }
    		                    		}
    		                    	}
    	                    	}
                        		
                        		if (haveSmallRealData) {
	                    		    if (realDataBytes < 4) {
	                    		    	padBytes = 4 - realDataBytes;
	                    		    	for (i = 0; i < padBytes; i++) {
	                    		    		raFile.readByte();
	                    		    	}
	                    		    }
	                    		}
	                    		else if ((realDataBytes % 8) != 0) {
	                    	    	padBytes = 8 - (realDataBytes % 8);
	                    	    	for (i = 0; i < padBytes; i++) {
	                        	    	raFile.readByte();
	                        	    }
	                    	    }   
                        	    imaginaryDataType = getInt(endianess);
                        	    if ((imaginaryDataType & 0xffff0000) != 0) {
                                    // Small data element format    
                                	imaginaryDataBytes = (imaginaryDataType & 0xffff0000) >>> 16;
                                	imaginaryDataType = imaginaryDataType & 0xffff;
                                	haveSmallImaginaryData = true;
                                }
                                else {
                                    imaginaryDataBytes = getInt(endianess);
                                    haveSmallImaginaryData = false;
                                }
                        	    if (imaginaryDataType == miUINT16) {
                        	    	Preferences.debug("imaginaryDataType == miUINT16 as expected\n", Preferences.DEBUG_FILEIO);
                        	    }
                        	    else {
                        	    	Preferences.debug("imaginaryDataType unexpectedly == " + imaginaryDataType + "\n", 
                        	    			Preferences.DEBUG_FILEIO);
                        	    }
                               
                                if (imaginaryDataBytes == realDataBytes) {
                                	Preferences.debug("imaginaryDataBytes == realDataBytes as expected\n", 
                                			Preferences.DEBUG_FILEIO);
                                }
                                else {
                                	Preferences.debug("imaginaryDataBytes unexpectedly != realDataBytes\n", 
                                			Preferences.DEBUG_FILEIO);
                                }
                                raFile.read(buffer);
                                imaginaryBuffer = new float[shortNumber];
                                j = 0;
    	                    	for (s = 0; s < numberSlices; s++) {
    		                    	for (x = 0; x < imageExtents[1]; x++) {
    		                    		for (y = 0; y < imageExtents[0]; y++) {
    		                    			index = 2*(x + imageExtents[1] * y + s * sliceSize);
                                            b1 = buffer[index] & 0xff;
                                            b2 = buffer[index+1] & 0xff;
                                            if (endianess == BIG_ENDIAN) {
                                                imaginaryBuffer[j++] = (float) ((b1 << 8) | b2);	
                                            }
                                            else {
                                            	imaginaryBuffer[j++] = (float) ((b2 << 8) | b1);
                                            }
    		                    		}
    		                    	}
    	                    	}
                                
                                if (haveSmallImaginaryData) {
                        		    if (imaginaryDataBytes < 4) {
                        		    	padBytes = 4 - imaginaryDataBytes;
                        		    	for (i = 0; i < padBytes; i++) {
                        		    		raFile.readByte();
                        		    	}
                        		    }
                        		}
                        		else if ((imaginaryDataBytes % 8) != 0) {
                        	    	padBytes = 8 - (imaginaryDataBytes % 8);
                        	    	for (i = 0; i < padBytes; i++) {
                            	    	raFile.readByte();
                            	    }
                        	    }   
                                
                                try {
                        			image2.importComplexData(2 * nonLogicalField * realBuffer.length, realBuffer, imaginaryBuffer, true, logMagDisplay);
                        		}
                        		catch(IOException e) {
                        		   MipavUtil.displayError("IOException on image2.importComplexData(2 * nonLogicalField * realBuffer.length," +
                        		   		" realBuffer, imaginaryBuffer, true, logMagDisplay)");
                        		   throw e;
                        		}		
                        	}
                        	break;
                        case miINT32:
                        	Preferences.debug("Real data type = miINT32\n", Preferences.DEBUG_FILEIO);
                        	Preferences.debug("Real data bytes = " + realDataBytes + "\n", Preferences.DEBUG_FILEIO);
                        	if (!complexFlag) {
                        		if (nonLogicalField == 0) {
                        		    image2 = new ModelImage(ModelStorageBase.INTEGER, imageExtents, fileName);
                        		    fileInfo2.setDataType(ModelStorageBase.INTEGER);
                        		}
                        		buffer = new byte[realDataBytes];
                        		raFile.read(buffer);
                        		intNumber = realDataBytes/4;
                        		intBuffer = new int[intNumber];
                        		numberSlices = intNumber/sliceSize;
                        		j = 0;
    	                    	for (s = 0; s < numberSlices; s++) {
    		                    	for (x = 0; x < imageExtents[1]; x++) {
    		                    		for (y = 0; y < imageExtents[0]; y++) {
    		                    			index = 4*(x + imageExtents[1] * y + s * sliceSize);
                                            b1 = buffer[index] & 0xff;
                                            b2 = buffer[index+1] & 0xff;
                                            b3 = buffer[index+2] & 0xff;
                                            b4 = buffer[index+3] & 0xff;
                                            if (endianess == BIG_ENDIAN) {
                                            	intBuffer[j++] = ((b1 << 24) | (b2 << 16) | (b3 << 8) | b4);
                                            }
                                            else {
                                            	intBuffer[j++] = ((b4 << 24) | (b3 << 16) | (b2 << 8) | b1);
                                            }
    		                    		}
    		                    	}
    	                    	}
                        		
                        		if (haveSmallRealData) {
	                    		    if (realDataBytes < 4) {
	                    		    	padBytes = 4 - realDataBytes;
	                    		    	for (i = 0; i < padBytes; i++) {
	                    		    		raFile.readByte();
	                    		    	}
	                    		    }
	                    		}
	                    		else if ((realDataBytes % 8) != 0) {
	                    	    	padBytes = 8 - (realDataBytes % 8);
	                    	    	for (i = 0; i < padBytes; i++) {
	                        	    	raFile.readByte();
	                        	    }
	                    	    }  
                        		try {
                        			image2.importData(nonLogicalField * intBuffer.length, intBuffer, true);
                        		}
                        		catch(IOException e) {
                        		   MipavUtil.displayError("IOException on image2.importData(nonLogicalField * intBuffer.length, intBuffer, true)");
                        		   throw e;
                        		}
                        	}
                        	else {
                        		if (nonLogicalField == 0) {
                        		    image2 = new ModelImage(ModelStorageBase.DCOMPLEX, imageExtents, fileName); 
                        		    fileInfo2.setDataType(ModelStorageBase.DCOMPLEX);
                        		}
                        		buffer = new byte[realDataBytes];
                        		raFile.read(buffer);
                        		intNumber = realDataBytes/4;
                        		realDBuffer = new double[intNumber];
                        		numberSlices = intNumber/sliceSize;
                        		j = 0;
    	                    	for (s = 0; s < numberSlices; s++) {
    		                    	for (x = 0; x < imageExtents[1]; x++) {
    		                    		for (y = 0; y < imageExtents[0]; y++) {
    		                    			index = 4*(x + imageExtents[1] * y + s * sliceSize);
                                            b1 = buffer[index] & 0xff;
                                            b2 = buffer[index+1] & 0xff;
                                            b3 = buffer[index+2] & 0xff;
                                            b4 = buffer[index+3] & 0xff;
                                            if (endianess == BIG_ENDIAN) {
                                            	realDBuffer[j++] = (double)((b1 << 24) | (b2 << 16) | (b3 << 8) | b4);
                                            }
                                            else {
                                            	realDBuffer[j++] = (double)((b4 << 24) | (b3 << 16) | (b2 << 8) | b1);
                                            }
    		                    		}
    		                    	}
    	                    	}
                        		
                        		if (haveSmallRealData) {
	                    		    if (realDataBytes < 4) {
	                    		    	padBytes = 4 - realDataBytes;
	                    		    	for (i = 0; i < padBytes; i++) {
	                    		    		raFile.readByte();
	                    		    	}
	                    		    }
	                    		}
	                    		else if ((realDataBytes % 8) != 0) {
	                    	    	padBytes = 8 - (realDataBytes % 8);
	                    	    	for (i = 0; i < padBytes; i++) {
	                        	    	raFile.readByte();
	                        	    }
	                    	    }    
                        	    imaginaryDataType = getInt(endianess);
                        	    if ((imaginaryDataType & 0xffff0000) != 0) {
                                    // Small data element format    
                                	imaginaryDataBytes = (imaginaryDataType & 0xffff0000) >>> 16;
                                	imaginaryDataType = imaginaryDataType & 0xffff;
                                	haveSmallImaginaryData = true;
                                }
                                else {
                                    imaginaryDataBytes = getInt(endianess);
                                    haveSmallImaginaryData = false;
                                }
                        	    if (imaginaryDataType == miINT32) {
                        	    	Preferences.debug("imaginaryDataType == miINT32 as expected\n", Preferences.DEBUG_FILEIO);
                        	    }
                        	    else {
                        	    	Preferences.debug("imaginaryDataType unexpectedly == " + imaginaryDataType + "\n", 
                        	    			Preferences.DEBUG_FILEIO);
                        	    }
                        	    
                                if (imaginaryDataBytes == realDataBytes) {
                                	Preferences.debug("imaginaryDataBytes == realDataBytes as expected\n", 
                                			Preferences.DEBUG_FILEIO);
                                }
                                else {
                                	Preferences.debug("imaginaryDataBytes unexpectedly != realDataBytes\n", 
                                			Preferences.DEBUG_FILEIO);
                                }
                                raFile.read(buffer);
                                imaginaryDBuffer = new double[intNumber];
                                j = 0;
    	                    	for (s = 0; s < numberSlices; s++) {
    		                    	for (x = 0; x < imageExtents[1]; x++) {
    		                    		for (y = 0; y < imageExtents[0]; y++) {
    		                    			index = 4*(x + imageExtents[1] * y + s * sliceSize);
                                            b1 = buffer[index] & 0xff;
                                            b2 = buffer[index+1] & 0xff;
                                            b3 = buffer[index+2] & 0xff;
                                            b4 = buffer[index+3] & 0xff;
                                            if (endianess == BIG_ENDIAN) {
                                            	imaginaryDBuffer[j++] = (double)((b1 << 24) | (b2 << 16) | (b3 << 8) | b4);
                                            }
                                            else {
                                            	imaginaryDBuffer[j++] = (double)((b4 << 24) | (b3 << 16) | (b2 << 8) | b1);
                                            }
    		                    		}
    		                    	}
    	                    	}
                                
                                if (haveSmallImaginaryData) {
                        		    if (imaginaryDataBytes < 4) {
                        		    	padBytes = 4 - imaginaryDataBytes;
                        		    	for (i = 0; i < padBytes; i++) {
                        		    		raFile.readByte();
                        		    	}
                        		    }
                        		}
                        		else if ((imaginaryDataBytes % 8) != 0) {
                        	    	padBytes = 8 - (imaginaryDataBytes % 8);
                        	    	for (i = 0; i < padBytes; i++) {
                            	    	raFile.readByte();
                            	    }
                        	    }   
                                
                                try {
                        			image2.importDComplexData(2 * nonLogicalField * realDBuffer.length, realDBuffer, imaginaryDBuffer, true, logMagDisplay);
                        		}
                        		catch(IOException e) {
                        		   MipavUtil.displayError("IOException on image2.importDComplexData(2 * nonLogicalField * realDBuffer.length, " +
                        		   		"realDBuffer, imaginaryDBuffer, true, logMagDisplay)");
                        		   throw e;
                        		}
                        	}
                        	break;
                        case miUINT32:
                        	Preferences.debug("Real data type = miUINT32\n", Preferences.DEBUG_FILEIO);
                        	Preferences.debug("Real data bytes = " + realDataBytes + "\n", Preferences.DEBUG_FILEIO);
                        	if (!complexFlag) {
                        		if (nonLogicalField == 0) {
                        		    image2 = new ModelImage(ModelStorageBase.UINTEGER, imageExtents, fileName);
                        		    fileInfo2.setDataType(ModelStorageBase.UINTEGER);
                        		}
                        		buffer = new byte[realDataBytes];
                        		raFile.read(buffer);
                        		intNumber = realDataBytes/4;
                        		longBuffer = new long[intNumber];
                        		numberSlices = intNumber/sliceSize;
                        		j = 0;
    	                    	for (s = 0; s < numberSlices; s++) {
    		                    	for (x = 0; x < imageExtents[1]; x++) {
    		                    		for (y = 0; y < imageExtents[0]; y++) {
    		                    			index = 4*(x + imageExtents[1] * y + s * sliceSize);
                                            b1L = buffer[index] & 0xffL;
                                            b2L = buffer[index+1] & 0xffL;
                                            b3L = buffer[index+2] & 0xffL;
                                            b4L = buffer[index+3] & 0xffL;
                                            if (endianess == BIG_ENDIAN) {
                                            	longBuffer[j++] = ((b1L << 24) | (b2L << 16) | (b3L << 8) | b4L);
                                            }
                                            else {
                                            	longBuffer[j++] = ((b4L << 24) | (b3L << 16) | (b2L << 8) | b1L);
                                            }
    		                    		}
    		                    	}
    	                    	}
                        		
                        		if (haveSmallRealData) {
	                    		    if (realDataBytes < 4) {
	                    		    	padBytes = 4 - realDataBytes;
	                    		    	for (i = 0; i < padBytes; i++) {
	                    		    		raFile.readByte();
	                    		    	}
	                    		    }
	                    		}
	                    		else if ((realDataBytes % 8) != 0) {
	                    	    	padBytes = 8 - (realDataBytes % 8);
	                    	    	for (i = 0; i < padBytes; i++) {
	                        	    	raFile.readByte();
	                        	    }
	                    	    }  
                        		try {
                        			image2.importUData(nonLogicalField * longBuffer.length, longBuffer, true);
                        		}
                        		catch(IOException e) {
                        		   MipavUtil.displayError("IOException on image2.importUData(nonLogicalField * longBuffer.length, longBuffer, true)");
                        		   throw e;
                        		}
                        	}
                        	else {
                        		if (nonLogicalField == 0) {
                        		    image2 = new ModelImage(ModelStorageBase.DCOMPLEX, imageExtents, fileName);
                        		    fileInfo2.setDataType(ModelStorageBase.DCOMPLEX);
                        		}
                        		buffer = new byte[realDataBytes];
                        		raFile.read(buffer);
                        		intNumber = realDataBytes/4;
                        		realDBuffer = new double[intNumber];
                        		numberSlices = intNumber/sliceSize;
                        		j = 0;
    	                    	for (s = 0; s < numberSlices; s++) {
    		                    	for (x = 0; x < imageExtents[1]; x++) {
    		                    		for (y = 0; y < imageExtents[0]; y++) {
    		                    			index = 4*(x + imageExtents[1] * y + s * sliceSize);
                                            b1L = buffer[index] & 0xffL;
                                            b2L = buffer[index+1] & 0xffL;
                                            b3L = buffer[index+2] & 0xffL;
                                            b4L = buffer[index+3] & 0xffL;
                                            if (endianess == BIG_ENDIAN) {
                                            	realDBuffer[j++] = (double)((b1L << 24) | (b2L << 16) | (b3L << 8) | b4L);
                                            }
                                            else {
                                            	realDBuffer[j++] = (double)((b4L << 24) | (b3L << 16) | (b2L << 8) | b1L);
                                            }
    		                    		}
    		                    	}
    	                    	}
                        		
                        		if (haveSmallRealData) {
	                    		    if (realDataBytes < 4) {
	                    		    	padBytes = 4 - realDataBytes;
	                    		    	for (i = 0; i < padBytes; i++) {
	                    		    		raFile.readByte();
	                    		    	}
	                    		    }
	                    		}
	                    		else if ((realDataBytes % 8) != 0) {
	                    	    	padBytes = 8 - (realDataBytes % 8);
	                    	    	for (i = 0; i < padBytes; i++) {
	                        	    	raFile.readByte();
	                        	    }
	                    	    }   
                        	    imaginaryDataType = getInt(endianess);
                        	    if ((imaginaryDataType & 0xffff0000) != 0) {
                                    // Small data element format    
                                	imaginaryDataBytes = (imaginaryDataType & 0xffff0000) >>> 16;
                                	imaginaryDataType = imaginaryDataType & 0xffff;
                                	haveSmallImaginaryData = true;
                                }
                                else {
                                    imaginaryDataBytes = getInt(endianess);
                                    haveSmallImaginaryData = false;
                                }
                        	    if (imaginaryDataType == miUINT32) {
                        	    	Preferences.debug("imaginaryDataType == miUINT32 as expected\n", Preferences.DEBUG_FILEIO);
                        	    }
                        	    else {
                        	    	Preferences.debug("imaginaryDataType unexpectedly == " + imaginaryDataType + "\n", 
                        	    			Preferences.DEBUG_FILEIO);
                        	    }
                        	    
                                if (imaginaryDataBytes == realDataBytes) {
                                	Preferences.debug("imaginaryDataBytes == realDataBytes as expected\n", 
                                			Preferences.DEBUG_FILEIO);
                                }
                                else {
                                	Preferences.debug("imaginaryDataBytes unexpectedly != realDataBytes\n", 
                                			Preferences.DEBUG_FILEIO);
                                }
                                raFile.read(buffer);
                                imaginaryDBuffer = new double[intNumber];
                                j = 0;
    	                    	for (s = 0; s < numberSlices; s++) {
    		                    	for (x = 0; x < imageExtents[1]; x++) {
    		                    		for (y = 0; y < imageExtents[0]; y++) {
    		                    			index = 4*(x + imageExtents[1] * y + s * sliceSize);
                                            b1L = buffer[index] & 0xffL;
                                            b2L = buffer[index+1] & 0xffL;
                                            b3L = buffer[index+2] & 0xffL;
                                            b4L = buffer[index+3] & 0xffL;
                                            if (endianess == BIG_ENDIAN) {
                                            	imaginaryDBuffer[j++] = (double)((b1L << 24) | (b2L << 16) | (b3L << 8) | b4L);
                                            }
                                            else {
                                            	imaginaryDBuffer[j++] = (double)((b4L << 24) | (b3L << 16) | (b2L << 8) | b1L);
                                            }
    		                    		}
    		                    	}
    	                    	}
                                
                                if (haveSmallImaginaryData) {
                        		    if (imaginaryDataBytes < 4) {
                        		    	padBytes = 4 - imaginaryDataBytes;
                        		    	for (i = 0; i < padBytes; i++) {
                        		    		raFile.readByte();
                        		    	}
                        		    }
                        		}
                        		else if ((imaginaryDataBytes % 8) != 0) {
                        	    	padBytes = 8 - (imaginaryDataBytes % 8);
                        	    	for (i = 0; i < padBytes; i++) {
                            	    	raFile.readByte();
                            	    }
                        	    }   
                                
                                try {
                        			image2.importDComplexData(2 * nonLogicalField * realDBuffer.length, realDBuffer, imaginaryDBuffer, true, logMagDisplay);
                        		}
                        		catch(IOException e) {
                        		   MipavUtil.displayError("IOException on image2.importDComplexData(2 * nonLogicalField * realDBuffer.length," +
                        		   		" realDBuffer, imaginaryDBuffer, true, logMagDisplay)");
                        		   throw e;
                        		}
                        	}
                        	break;
                        case miSINGLE:
                        	Preferences.debug("Real data type = miSINGLE\n", Preferences.DEBUG_FILEIO);
                        	Preferences.debug("Real data bytes = " + realDataBytes + "\n", Preferences.DEBUG_FILEIO);
                        	if (!complexFlag) {
                        		if (nonLogicalField == 0) {
                        		    image2 = new ModelImage(ModelStorageBase.FLOAT, imageExtents, fileName); 
                        		    fileInfo2.setDataType(ModelStorageBase.FLOAT);
                        		}
                        		buffer = new byte[realDataBytes];
                        		raFile.read(buffer);
                        		floatNumber = realDataBytes/4;
                        		floatBuffer = new float[floatNumber];
                        		numberSlices = floatNumber/sliceSize;
                        		j = 0;
    	                    	for (s = 0; s < numberSlices; s++) {
    		                    	for (x = 0; x < imageExtents[1]; x++) {
    		                    		for (y = 0; y < imageExtents[0]; y++) {
    		                    			index = 4*(x + imageExtents[1] * y + s * sliceSize);
                                            b1 = buffer[index] & 0xff;
                                            b2 = buffer[index+1] & 0xff;
                                            b3 = buffer[index+2] & 0xff;
                                            b4 = buffer[index+3] & 0xff;
                                            if (endianess == BIG_ENDIAN) {
                                            	tmpInt = ((b1 << 24) | (b2 << 16) | (b3 << 8) | b4);
                                            }
                                            else {
                                            	tmpInt = ((b4 << 24) | (b3 << 16) | (b2 << 8) | b1);
                                            }
                                            floatBuffer[j++] = Float.intBitsToFloat(tmpInt);
    		                    		}
    		                    	}
    	                    	}
                        		
                        		if (haveSmallRealData) {
	                    		    if (realDataBytes < 4) {
	                    		    	padBytes = 4 - realDataBytes;
	                    		    	for (i = 0; i < padBytes; i++) {
	                    		    		raFile.readByte();
	                    		    	}
	                    		    }
	                    		}
	                    		else if ((realDataBytes % 8) != 0) {
	                    	    	padBytes = 8 - (realDataBytes % 8);
	                    	    	for (i = 0; i < padBytes; i++) {
	                        	    	raFile.readByte();
	                        	    }
	                    	    }  
                        		try {
                        			image2.importData(nonLogicalField * floatBuffer.length, floatBuffer, true);
                        		}
                        		catch(IOException e) {
                        		   MipavUtil.displayError("IOException on image2.importData(nonLogicalField * floatBuffer.length, floatBuffer, true)");
                        		   throw e;
                        		}
                        	}
                        	else {
                        		if (nonLogicalField == 0) {
                        		    image2 = new ModelImage(ModelStorageBase.COMPLEX, imageExtents, fileName);
                        		    fileInfo2.setDataType(ModelStorageBase.COMPLEX);
                        		}
                        		buffer = new byte[realDataBytes];
                        		raFile.read(buffer);
                        		floatNumber = realDataBytes/4;
                        		realBuffer = new float[floatNumber];
                        		numberSlices = floatNumber/sliceSize;
                        		j = 0;
    	                    	for (s = 0; s < numberSlices; s++) {
    		                    	for (x = 0; x < imageExtents[1]; x++) {
    		                    		for (y = 0; y < imageExtents[0]; y++) {
    		                    			index = 4*(x + imageExtents[1] * y + s * sliceSize);
                                            b1 = buffer[index] & 0xff;
                                            b2 = buffer[index+1] & 0xff;
                                            b3 = buffer[index+2] & 0xff;
                                            b4 = buffer[index+3] & 0xff;
                                            if (endianess == BIG_ENDIAN) {
                                            	tmpInt = ((b1 << 24) | (b2 << 16) | (b3 << 8) | b4);
                                            }
                                            else {
                                            	tmpInt = ((b4 << 24) | (b3 << 16) | (b2 << 8) | b1);
                                            }
                                            realBuffer[j++] = Float.intBitsToFloat(tmpInt);
    		                    		}
    		                    	}
    	                    	}
                        		
                        		if (haveSmallRealData) {
	                    		    if (realDataBytes < 4) {
	                    		    	padBytes = 4 - realDataBytes;
	                    		    	for (i = 0; i < padBytes; i++) {
	                    		    		raFile.readByte();
	                    		    	}
	                    		    }
	                    		}
	                    		else if ((realDataBytes % 8) != 0) {
	                    	    	padBytes = 8 - (realDataBytes % 8);
	                    	    	for (i = 0; i < padBytes; i++) {
	                        	    	raFile.readByte();
	                        	    }
	                    	    }  
                        	    imaginaryDataType = getInt(endianess);
                        	    if ((imaginaryDataType & 0xffff0000) != 0) {
                                    // Small data element format    
                                	imaginaryDataBytes = (imaginaryDataType & 0xffff0000) >>> 16;
                                	imaginaryDataType = imaginaryDataType & 0xffff;
                                	haveSmallImaginaryData = true;
                                }
                                else {
                                    imaginaryDataBytes = getInt(endianess);
                                    haveSmallImaginaryData = false;
                                }
                        	    if (imaginaryDataType == miSINGLE) {
                        	    	Preferences.debug("imaginaryDataType == miSINGLE as expected\n", Preferences.DEBUG_FILEIO);
                        	    }
                        	    else {
                        	    	Preferences.debug("imaginaryDataType unexpectedly == " + imaginaryDataType + "\n", 
                        	    			Preferences.DEBUG_FILEIO);
                        	    }
                        	    
                                if (imaginaryDataBytes == realDataBytes) {
                                	Preferences.debug("imaginaryDataBytes == realDataBytes as expected\n", 
                                			Preferences.DEBUG_FILEIO);
                                }
                                else {
                                	Preferences.debug("imaginaryDataBytes unexpectedly != realDataBytes\n", 
                                			Preferences.DEBUG_FILEIO);
                                }
                                raFile.read(buffer);
                                imaginaryBuffer = new float[floatNumber];
                                j = 0;
    	                    	for (s = 0; s < numberSlices; s++) {
    		                    	for (x = 0; x < imageExtents[1]; x++) {
    		                    		for (y = 0; y < imageExtents[0]; y++) {
    		                    			index = 4*(x + imageExtents[1] * y + s * sliceSize);
                                            b1 = buffer[index] & 0xff;
                                            b2 = buffer[index+1] & 0xff;
                                            b3 = buffer[index+2] & 0xff;
                                            b4 = buffer[index+3] & 0xff;
                                            if (endianess == BIG_ENDIAN) {
                                            	tmpInt = ((b1 << 24) | (b2 << 16) | (b3 << 8) | b4);
                                            }
                                            else {
                                            	tmpInt = ((b4 << 24) | (b3 << 16) | (b2 << 8) | b1);
                                            }
                                            imaginaryBuffer[j++] = Float.intBitsToFloat(tmpInt);
    		                    		}
    		                    	}
    	                    	}
                                
                                if (haveSmallImaginaryData) {
                        		    if (imaginaryDataBytes < 4) {
                        		    	padBytes = 4 - imaginaryDataBytes;
                        		    	for (i = 0; i < padBytes; i++) {
                        		    		raFile.readByte();
                        		    	}
                        		    }
                        		}
                        		else if ((imaginaryDataBytes % 8) != 0) {
                        	    	padBytes = 8 - (imaginaryDataBytes % 8);
                        	    	for (i = 0; i < padBytes; i++) {
                            	    	raFile.readByte();
                            	    }
                        	    }   
                                
                                try {
                        			image2.importComplexData(2 * nonLogicalField * realBuffer.length, realBuffer, imaginaryBuffer, true, logMagDisplay);
                        		}
                        		catch(IOException e) {
                        		   MipavUtil.displayError("IOException on image2.importComplexData(2 * nonLogicalField * realBuffer.length, " +
                        		   		"realBuffer, imaginaryBuffer, true, logMagDisplay)");
                        		   throw e;
                        		}
                        	}
                        	break;
                        case miDOUBLE:
                        	Preferences.debug("Real data type = miDOUBLE\n", Preferences.DEBUG_FILEIO);
                        	Preferences.debug("Real data bytes = " + realDataBytes + "\n", Preferences.DEBUG_FILEIO);
                        	if (isColor) {
                        		if (nonLogicalField == 0) {
                        			image2 = new ModelImage(ModelStorageBase.ARGB_FLOAT, imageExtents, fileName);
                        			fileInfo2.setDataType(ModelStorageBase.ARGB_FLOAT);
                        		}
                        		if ((realDataBytes % 3) == 0) {
    	                    		buffer = new byte[realDataBytes/3];
    	                    		doubleNumber = realDataBytes/24;
    	                    		floatBuffer = new float[doubleNumber];
    	                    		numberSlices = doubleNumber/sliceSize;
    	                    		for (i = 0; i < 3; i++) {
    			                    	raFile.read(buffer);
    			                    	j = 0;
    			                    	for (s = 0; s < numberSlices; s++) {
    				                    	for (x = 0; x < imageExtents[1]; x++) {
    				                    		for (y = 0; y < imageExtents[0]; y++) {	
    				                    			index = 8*(x + imageExtents[1] * y + s * sliceSize);
    	                                            b1L = buffer[index] & 0xffL;
    	                                            b2L = buffer[index+1] & 0xffL;
    	                                            b3L = buffer[index+2] & 0xffL;
    	                                            b4L = buffer[index+3] & 0xffL;
    	                                            b5L = buffer[index+4] & 0xffL;
    	                                            b6L = buffer[index+5] & 0xffL;
    	                                            b7L = buffer[index+6] & 0xffL;
    	                                            b8L = buffer[index+7] & 0xffL;
    	                                            if (endianess == BIG_ENDIAN) {
    	                                            	tmpLong = ((b1L << 56) | (b2L << 48) | (b3L << 40) | (b4L << 32) |
    			                                                   (b5L << 24) | (b6L << 16) | (b7L << 8) | b8L);	
    	                                            }
    	                                            else {
    	                                            	tmpLong = ((b8L << 56) | (b7L << 48) | (b6L << 40) | (b5L << 32) |
    			                                                   (b4L << 24) | (b3L << 16) | (b2L << 8) | b1L);
    	                                            }
    	                                            floatBuffer[j++] = (float)(255.0 * Double.longBitsToDouble(tmpLong));
    				                    		}
    				                    	}
    			                    	}
    			                    	
    			                    	try {
    			                			image2.importRGBData(i+1, nonLogicalField * floatBuffer.length, floatBuffer, true);
    			                		}
    			                		catch(IOException e) {
    			                		   MipavUtil.displayError("IOException on image2.importRGBData(i," +
    			                		   		" nonLogicalField * floatBuffer.length, floatBuffer, true)");
    			                		   throw e;
    			                		}
    	                    		} // if (i = 0; i < 3; i++)
    	
    		                    	
    		                    	if (haveSmallRealData) {
    	                    		    if (realDataBytes < 4) {
    	                    		    	padBytes = 4 - realDataBytes;
    	                    		    	for (i = 0; i < padBytes; i++) {
    	                    		    		raFile.readByte();
    	                    		    	}
    	                    		    }
    	                    		}
    	                    		else if ((realDataBytes % 8) != 0) {
    	                    	    	padBytes = 8 - (realDataBytes % 8);
    	                    	    	for (i = 0; i < padBytes; i++) {
    	                        	    	raFile.readByte();
    	                        	    }
    	                    	    } 
    	                    	
    	                        } // if (realDataBytes % 3) == 0)
                        	} // else if (isColor)
                        	else if (!complexFlag) {
                        		if (nonLogicalField == 0) {
                        		    image2 = new ModelImage(ModelStorageBase.DOUBLE, imageExtents, fileName); 
                        		    fileInfo2.setDataType(ModelStorageBase.DOUBLE);
                        		}
                        		buffer = new byte[realDataBytes];
                        		raFile.read(buffer);
                        		doubleNumber = realDataBytes/8;
                        		doubleBuffer = new double[doubleNumber];
                        		numberSlices = doubleNumber/sliceSize;
                        		j = 0;
    	                    	for (s = 0; s < numberSlices; s++) {
    		                    	for (x = 0; x < imageExtents[1]; x++) {
    		                    		for (y = 0; y < imageExtents[0]; y++) {
    		                    			index = 8*(x + imageExtents[1] * y + s * sliceSize);
                                            b1L = buffer[index] & 0xffL;
                                            b2L = buffer[index+1] & 0xffL;
                                            b3L = buffer[index+2] & 0xffL;
                                            b4L = buffer[index+3] & 0xffL;
                                            b5L = buffer[index+4] & 0xffL;
                                            b6L = buffer[index+5] & 0xffL;
                                            b7L = buffer[index+6] & 0xffL;
                                            b8L = buffer[index+7] & 0xffL;
                                            if (endianess == BIG_ENDIAN) {
                                            	tmpLong = ((b1L << 56) | (b2L << 48) | (b3L << 40) | (b4L << 32) |
    	                                                   (b5L << 24) | (b6L << 16) | (b7L << 8) | b8L);	
                                            }
                                            else {
                                            	tmpLong = ((b8L << 56) | (b7L << 48) | (b6L << 40) | (b5L << 32) |
    	                                                   (b4L << 24) | (b3L << 16) | (b2L << 8) | b1L);
                                            }
                                            doubleBuffer[j++] = Double.longBitsToDouble(tmpLong);
    		                    		}
    		                    	}
    	                    	}
                        		
                        		if (haveSmallRealData) {
	                    		    if (realDataBytes < 4) {
	                    		    	padBytes = 4 - realDataBytes;
	                    		    	for (i = 0; i < padBytes; i++) {
	                    		    		raFile.readByte();
	                    		    	}
	                    		    }
	                    		}
	                    		else if ((realDataBytes % 8) != 0) {
	                    	    	padBytes = 8 - (realDataBytes % 8);
	                    	    	for (i = 0; i < padBytes; i++) {
	                        	    	raFile.readByte();
	                        	    }
	                    	    }  
                        		try {
                        			image2.importData(nonLogicalField * doubleBuffer.length, doubleBuffer, true);
                        		}
                        		catch(IOException e) {
                        		   MipavUtil.displayError("IOException on image2.importData(nonLogicalField * doubleBuffer.length, doubleBuffer, true)");
                        		   throw e;
                        		}
                        	}
                        	else {
                        		if (nonLogicalField == 0) {
                        		    image2 = new ModelImage(ModelStorageBase.DCOMPLEX, imageExtents, fileName); 
                        		    fileInfo2.setDataType(ModelStorageBase.DCOMPLEX);
                        		}
                        		buffer = new byte[realDataBytes];
                        		raFile.read(buffer);
                        		doubleNumber = realDataBytes/8;
                        		realDBuffer = new double[doubleNumber];
                        		numberSlices = doubleNumber/sliceSize;
                        		j = 0;
    	                    	for (s = 0; s < numberSlices; s++) {
    		                    	for (x = 0; x < imageExtents[1]; x++) {
    		                    		for (y = 0; y < imageExtents[0]; y++) {
    		                    			index = 8*(x + imageExtents[1] * y + s * sliceSize);
                                            b1L = buffer[index] & 0xffL;
                                            b2L = buffer[index+1] & 0xffL;
                                            b3L = buffer[index+2] & 0xffL;
                                            b4L = buffer[index+3] & 0xffL;
                                            b5L = buffer[index+4] & 0xffL;
                                            b6L = buffer[index+5] & 0xffL;
                                            b7L = buffer[index+6] & 0xffL;
                                            b8L = buffer[index+7] & 0xffL;
                                            if (endianess == BIG_ENDIAN) {
                                            	tmpLong = ((b1L << 56) | (b2L << 48) | (b3L << 40) | (b4L << 32) |
    	                                                   (b5L << 24) | (b6L << 16) | (b7L << 8) | b8L);	
                                            }
                                            else {
                                            	tmpLong = ((b8L << 56) | (b7L << 48) | (b6L << 40) | (b5L << 32) |
    	                                                   (b4L << 24) | (b3L << 16) | (b2L << 8) | b1L);
                                            }
                                            realDBuffer[j++] = Double.longBitsToDouble(tmpLong);
    		                    		}
    		                    	}
    	                    	}
                        		
                        		if (haveSmallRealData) {
	                    		    if (realDataBytes < 4) {
	                    		    	padBytes = 4 - realDataBytes;
	                    		    	for (i = 0; i < padBytes; i++) {
	                    		    		raFile.readByte();
	                    		    	}
	                    		    }
	                    		}
	                    		else if ((realDataBytes % 8) != 0) {
	                    	    	padBytes = 8 - (realDataBytes % 8);
	                    	    	for (i = 0; i < padBytes; i++) {
	                        	    	raFile.readByte();
	                        	    }
	                    	    }    
                        	    imaginaryDataType = getInt(endianess);
                        	    if ((imaginaryDataType & 0xffff0000) != 0) {
                                    // Small data element format    
                                	imaginaryDataBytes = (imaginaryDataType & 0xffff0000) >>> 16;
                                	imaginaryDataType = imaginaryDataType & 0xffff;
                                	haveSmallImaginaryData = true;
                                }
                                else {
                                    imaginaryDataBytes = getInt(endianess);
                                    haveSmallImaginaryData = false;
                                }
                        	    if (imaginaryDataType == miDOUBLE) {
                        	    	Preferences.debug("imaginaryDataType == miDOUBLE as expected\n", Preferences.DEBUG_FILEIO);
                        	    }
                        	    else {
                        	    	Preferences.debug("imaginaryDataType unexpectedly == " + imaginaryDataType + "\n", 
                        	    			Preferences.DEBUG_FILEIO);
                        	    }
                        	    
                                if (imaginaryDataBytes == realDataBytes) {
                                	Preferences.debug("imaginaryDataBytes == realDataBytes as expected\n", 
                                			Preferences.DEBUG_FILEIO);
                                }
                                else {
                                	Preferences.debug("imaginaryDataBytes unexpectedly != realDataBytes\n", 
                                			Preferences.DEBUG_FILEIO);
                                }
                                raFile.read(buffer);
                                imaginaryDBuffer = new double[doubleNumber];
                                j = 0;
    	                    	for (s = 0; s < numberSlices; s++) {
    		                    	for (x = 0; x < imageExtents[1]; x++) {
    		                    		for (y = 0; y < imageExtents[0]; y++) {
    		                    			index = 8*(x + imageExtents[1] * y + s * sliceSize);
                                            b1L = buffer[index] & 0xffL;
                                            b2L = buffer[index+1] & 0xffL;
                                            b3L = buffer[index+2] & 0xffL;
                                            b4L = buffer[index+3] & 0xffL;
                                            b5L = buffer[index+4] & 0xffL;
                                            b6L = buffer[index+5] & 0xffL;
                                            b7L = buffer[index+6] & 0xffL;
                                            b8L = buffer[index+7] & 0xffL;
                                            if (endianess == BIG_ENDIAN) {
                                            	tmpLong = ((b1L << 56) | (b2L << 48) | (b3L << 40) | (b4L << 32) |
    	                                                   (b5L << 24) | (b6L << 16) | (b7L << 8) | b8L);	
                                            }
                                            else {
                                            	tmpLong = ((b8L << 56) | (b7L << 48) | (b6L << 40) | (b5L << 32) |
    	                                                   (b4L << 24) | (b3L << 16) | (b2L << 8) | b1L);
                                            }
                                            imaginaryDBuffer[j++] = Double.longBitsToDouble(tmpLong);
    		                    		}
    		                    	}
    	                    	}
                                
                                if (haveSmallImaginaryData) {
                        		    if (imaginaryDataBytes < 4) {
                        		    	padBytes = 4 - imaginaryDataBytes;
                        		    	for (i = 0; i < padBytes; i++) {
                        		    		raFile.readByte();
                        		    	}
                        		    }
                        		}
                        		else if ((imaginaryDataBytes % 8) != 0) {
                        	    	padBytes = 8 - (imaginaryDataBytes % 8);
                        	    	for (i = 0; i < padBytes; i++) {
                            	    	raFile.readByte();
                            	    }
                        	    }   
                                
                                try {
                        			image2.importDComplexData(2 * nonLogicalField * realDBuffer.length, realDBuffer, imaginaryDBuffer, true, logMagDisplay);
                        		}
                        		catch(IOException e) {
                        		   MipavUtil.displayError("IOException on image2.importDComplexData(2 * nonLogicalField * realDBuffer.length, " +
                        		   		"realDBuffer, imaginaryDBuffer, true, logMagDisplay)");
                        		   throw e;
                        		}
                        	}
                        	break;
                        case miINT64:
                        	Preferences.debug("Real data type = miINT64\n", Preferences.DEBUG_FILEIO);
                        	Preferences.debug("Real data bytes = " + realDataBytes + "\n", Preferences.DEBUG_FILEIO);
                        	if (!complexFlag) {
                        		if (nonLogicalField == 0) {
                        		    image2 = new ModelImage(ModelStorageBase.LONG, imageExtents, fileName);
                        		    fileInfo2.setDataType(ModelStorageBase.LONG);
                        		}
                        		buffer = new byte[realDataBytes];
                        		raFile.read(buffer);
                        		longNumber = realDataBytes/8;
                        		longBuffer = new long[longNumber];
                        		numberSlices = longNumber/sliceSize;
                        		j = 0;
    	                    	for (s = 0; s < numberSlices; s++) {
    		                    	for (x = 0; x < imageExtents[1]; x++) {
    		                    		for (y = 0; y < imageExtents[0]; y++) {
    		                    			index = 8*(x + imageExtents[1] * y + s * sliceSize);
                                            b1L = buffer[index] & 0xffL;
                                            b2L = buffer[index+1] & 0xffL;
                                            b3L = buffer[index+2] & 0xffL;
                                            b4L = buffer[index+3] & 0xffL;
                                            b5L = buffer[index+4] & 0xffL;
                                            b6L = buffer[index+5] & 0xffL;
                                            b7L = buffer[index+6] & 0xffL;
                                            b8L = buffer[index+7] & 0xffL;
                                            if (endianess == BIG_ENDIAN) {
                                            	longBuffer[j++] = ((b1L << 56) | (b2L << 48) | (b3L << 40) | (b4L << 32) |
    	                                                   (b5L << 24) | (b6L << 16) | (b7L << 8) | b8L);	
                                            }
                                            else {
                                            	longBuffer[j++] = ((b8L << 56) | (b7L << 48) | (b6L << 40) | (b5L << 32) |
    	                                                   (b4L << 24) | (b3L << 16) | (b2L << 8) | b1L);
                                            }
    		                    		}
    		                    	}
    	                    	}
                        		
                        		if (haveSmallRealData) {
	                    		    if (realDataBytes < 4) {
	                    		    	padBytes = 4 - realDataBytes;
	                    		    	for (i = 0; i < padBytes; i++) {
	                    		    		raFile.readByte();
	                    		    	}
	                    		    }
	                    		}
	                    		else if ((realDataBytes % 8) != 0) {
	                    	    	padBytes = 8 - (realDataBytes % 8);
	                    	    	for (i = 0; i < padBytes; i++) {
	                        	    	raFile.readByte();
	                        	    }
	                    	    }  
                        		try {
                        			image2.importData(nonLogicalField * longBuffer.length, longBuffer, true);
                        		}
                        		catch(IOException e) {
                        		   MipavUtil.displayError("IOException on image2.importData(nonLogicalField * longBuffer.length, longBuffer, true)");
                        		   throw e;
                        		}
                        	}
                        	else {
                        		if (nonLogicalField == 0) {
                        		    image2 = new ModelImage(ModelStorageBase.DCOMPLEX, imageExtents, fileName); 
                        		    fileInfo2.setDataType(ModelStorageBase.DCOMPLEX);
                        		}
                        		buffer = new byte[realDataBytes];
                        		raFile.read(buffer);
                        		longNumber = realDataBytes/8;
                        		realDBuffer = new double[longNumber];
                        		numberSlices = longNumber/sliceSize;
                        		j = 0;
    	                    	for (s = 0; s < numberSlices; s++) {
    		                    	for (x = 0; x < imageExtents[1]; x++) {
    		                    		for (y = 0; y < imageExtents[0]; y++) {
    		                    			index = 8*(x + imageExtents[1] * y + s * sliceSize);
                                            b1L = buffer[index] & 0xffL;
                                            b2L = buffer[index+1] & 0xffL;
                                            b3L = buffer[index+2] & 0xffL;
                                            b4L = buffer[index+3] & 0xffL;
                                            b5L = buffer[index+4] & 0xffL;
                                            b6L = buffer[index+5] & 0xffL;
                                            b7L = buffer[index+6] & 0xffL;
                                            b8L = buffer[index+7] & 0xffL;
                                            if (endianess == BIG_ENDIAN) {
                                            	realDBuffer[j++] = (double)((b1L << 56) | (b2L << 48) | (b3L << 40) | (b4L << 32) |
    	                                                   (b5L << 24) | (b6L << 16) | (b7L << 8) | b8L);	
                                            }
                                            else {
                                            	realDBuffer[j++] = (double)((b8L << 56) | (b7L << 48) | (b6L << 40) | (b5L << 32) |
    	                                                   (b4L << 24) | (b3L << 16) | (b2L << 8) | b1L);
                                            }
    		                    		}
    		                    	}
    	                    	}
                        		
                        		if (haveSmallRealData) {
	                    		    if (realDataBytes < 4) {
	                    		    	padBytes = 4 - realDataBytes;
	                    		    	for (i = 0; i < padBytes; i++) {
	                    		    		raFile.readByte();
	                    		    	}
	                    		    }
	                    		}
	                    		else if ((realDataBytes % 8) != 0) {
	                    	    	padBytes = 8 - (realDataBytes % 8);
	                    	    	for (i = 0; i < padBytes; i++) {
	                        	    	raFile.readByte();
	                        	    }
	                    	    }   
                        	    imaginaryDataType = getInt(endianess);
                        	    if ((imaginaryDataType & 0xffff0000) != 0) {
                                    // Small data element format    
                                	imaginaryDataBytes = (imaginaryDataType & 0xffff0000) >>> 16;
                                	imaginaryDataType = imaginaryDataType & 0xffff;
                                	haveSmallImaginaryData = true;
                                }
                                else {
                                    imaginaryDataBytes = getInt(endianess);
                                    haveSmallImaginaryData = false;
                                }
                        	    if (imaginaryDataType == miINT64) {
                        	    	Preferences.debug("imaginaryDataType == miINT64 as expected\n", Preferences.DEBUG_FILEIO);
                        	    }
                        	    else {
                        	    	Preferences.debug("imaginaryDataType unexpectedly == " + imaginaryDataType + "\n", 
                        	    			Preferences.DEBUG_FILEIO);
                        	    }
                        	    
                                if (imaginaryDataBytes == realDataBytes) {
                                	Preferences.debug("imaginaryDataBytes == realDataBytes as expected\n", 
                                			Preferences.DEBUG_FILEIO);
                                }
                                else {
                                	Preferences.debug("imaginaryDataBytes unexpectedly != realDataBytes\n", 
                                			Preferences.DEBUG_FILEIO);
                                }
                                raFile.read(buffer);
                                imaginaryDBuffer = new double[longNumber];
                                j = 0;
    	                    	for (s = 0; s < numberSlices; s++) {
    		                    	for (x = 0; x < imageExtents[1]; x++) {
    		                    		for (y = 0; y < imageExtents[0]; y++) {
    		                    			index = 8*(x + imageExtents[1] * y + s * sliceSize);
                                            b1L = buffer[index] & 0xffL;
                                            b2L = buffer[index+1] & 0xffL;
                                            b3L = buffer[index+2] & 0xffL;
                                            b4L = buffer[index+3] & 0xffL;
                                            b5L = buffer[index+4] & 0xffL;
                                            b6L = buffer[index+5] & 0xffL;
                                            b7L = buffer[index+6] & 0xffL;
                                            b8L = buffer[index+7] & 0xffL;
                                            if (endianess == BIG_ENDIAN) {
                                            	imaginaryDBuffer[j++] = (double)((b1L << 56) | (b2L << 48) | (b3L << 40) | (b4L << 32) |
    	                                                   (b5L << 24) | (b6L << 16) | (b7L << 8) | b8L);	
                                            }
                                            else {
                                            	imaginaryDBuffer[j++] = (double)((b8L << 56) | (b7L << 48) | (b6L << 40) | (b5L << 32) |
    	                                                   (b4L << 24) | (b3L << 16) | (b2L << 8) | b1L);
                                            }
    		                    		}
    		                    	}
    	                    	}
                                
                                if (haveSmallImaginaryData) {
                        		    if (imaginaryDataBytes < 4) {
                        		    	padBytes = 4 - imaginaryDataBytes;
                        		    	for (i = 0; i < padBytes; i++) {
                        		    		raFile.readByte();
                        		    	}
                        		    }
                        		}
                        		else if ((imaginaryDataBytes % 8) != 0) {
                        	    	padBytes = 8 - (imaginaryDataBytes % 8);
                        	    	for (i = 0; i < padBytes; i++) {
                            	    	raFile.readByte();
                            	    }
                        	    }   
                                
                                try {
                        			image2.importDComplexData(2 * nonLogicalField * realDBuffer.length, realDBuffer, imaginaryDBuffer, true, logMagDisplay);
                        		}
                        		catch(IOException e) {
                        		   MipavUtil.displayError("IOException on image2.importDComplexData(2 * nonLogicalField * realDBuffer.length, " +
                        		   		"realDBuffer, imaginaryDBuffer, true, logMagDisplay)");
                        		   throw e;
                        		}
                        	}
                        	break;
                        case miUINT64:
                        	Preferences.debug("Real data type = miUINT64\n", Preferences.DEBUG_FILEIO);
                        	Preferences.debug("Real data bytes = " + realDataBytes + "\n", Preferences.DEBUG_FILEIO);
                        	if (!complexFlag) {
                        		if (nonLogicalField == 0) {
                        		    image2 = new ModelImage(ModelStorageBase.LONG, imageExtents, fileName); 
                        		    fileInfo2.setDataType(ModelStorageBase.LONG);
                        		}
                        		buffer = new byte[realDataBytes];
                        		raFile.read(buffer);
                        		longNumber = realDataBytes/8;
                        		longBuffer = new long[longNumber];
                        		numberSlices = longNumber/sliceSize;
                        		j = 0;
    	                    	for (s = 0; s < numberSlices; s++) {
    		                    	for (x = 0; x < imageExtents[1]; x++) {
    		                    		for (y = 0; y < imageExtents[0]; y++) {
    		                    			index = 8*(x + imageExtents[1] * y + s * sliceSize);
                                            b1L = buffer[index] & 0xffL;
                                            b2L = buffer[index+1] & 0xffL;
                                            b3L = buffer[index+2] & 0xffL;
                                            b4L = buffer[index+3] & 0xffL;
                                            b5L = buffer[index+4] & 0xffL;
                                            b6L = buffer[index+5] & 0xffL;
                                            b7L = buffer[index+6] & 0xffL;
                                            b8L = buffer[index+7] & 0xffL;
                                            if (endianess == BIG_ENDIAN) {
                                            	longBuffer[j++] = ((b1L << 56) | (b2L << 48) | (b3L << 40) | (b4L << 32) |
    	                                                   (b5L << 24) | (b6L << 16) | (b7L << 8) | b8L);	
                                            }
                                            else {
                                            	longBuffer[j++] = ((b8L << 56) | (b7L << 48) | (b6L << 40) | (b5L << 32) |
    	                                                   (b4L << 24) | (b3L << 16) | (b2L << 8) | b1L);
                                            }
    		                    		}
    		                    	}
    	                    	}
                        		
                        		if (haveSmallRealData) {
	                    		    if (realDataBytes < 4) {
	                    		    	padBytes = 4 - realDataBytes;
	                    		    	for (i = 0; i < padBytes; i++) {
	                    		    		raFile.readByte();
	                    		    	}
	                    		    }
	                    		}
	                    		else if ((realDataBytes % 8) != 0) {
	                    	    	padBytes = 8 - (realDataBytes % 8);
	                    	    	for (i = 0; i < padBytes; i++) {
	                        	    	raFile.readByte();
	                        	    }
	                    	    }  
                        		try {
                        			image2.importData(nonLogicalField * longBuffer.length, longBuffer, true);
                        		}
                        		catch(IOException e) {
                        		   MipavUtil.displayError("IOException on image2.importData(nonLogicalField * longBuffer.length, longBuffer, true)");
                        		   throw e;
                        		}
                        	}
                        	else {
                        		if (nonLogicalField == 0) {
                        		    image2 = new ModelImage(ModelStorageBase.DCOMPLEX, imageExtents, fileName); 
                        		    fileInfo2.setDataType(ModelStorageBase.DCOMPLEX);
                        		}
                        		buffer = new byte[realDataBytes];
                        		raFile.read(buffer);
                        		longNumber = realDataBytes/8;
                        		realDBuffer = new double[longNumber];
                        		numberSlices = longNumber/sliceSize;
                        		j = 0;
    	                    	for (s = 0; s < numberSlices; s++) {
    		                    	for (x = 0; x < imageExtents[1]; x++) {
    		                    		for (y = 0; y < imageExtents[0]; y++) {
    		                    			index = 8*(x + imageExtents[1] * y + s * sliceSize);
                                            b1L = buffer[index] & 0xffL;
                                            b2L = buffer[index+1] & 0xffL;
                                            b3L = buffer[index+2] & 0xffL;
                                            b4L = buffer[index+3] & 0xffL;
                                            b5L = buffer[index+4] & 0xffL;
                                            b6L = buffer[index+5] & 0xffL;
                                            b7L = buffer[index+6] & 0xffL;
                                            b8L = buffer[index+7] & 0xffL;
                                            if (endianess == BIG_ENDIAN) {
                                            	realDBuffer[j++] = (double)((b1L << 56) | (b2L << 48) | (b3L << 40) | (b4L << 32) |
    	                                                   (b5L << 24) | (b6L << 16) | (b7L << 8) | b8L);	
                                            }
                                            else {
                                            	realDBuffer[j++] = (double)((b8L << 56) | (b7L << 48) | (b6L << 40) | (b5L << 32) |
    	                                                   (b4L << 24) | (b3L << 16) | (b2L << 8) | b1L);
                                            }
    		                    		}
    		                    	}
    	                    	}
                        		
                        		if (haveSmallRealData) {
	                    		    if (realDataBytes < 4) {
	                    		    	padBytes = 4 - realDataBytes;
	                    		    	for (i = 0; i < padBytes; i++) {
	                    		    		raFile.readByte();
	                    		    	}
	                    		    }
	                    		}
	                    		else if ((realDataBytes % 8) != 0) {
	                    	    	padBytes = 8 - (realDataBytes % 8);
	                    	    	for (i = 0; i < padBytes; i++) {
	                        	    	raFile.readByte();
	                        	    }
	                    	    }  
                        	    imaginaryDataType = getInt(endianess);
                        	    if ((imaginaryDataType & 0xffff0000) != 0) {
                                    // Small data element format    
                                	imaginaryDataBytes = (imaginaryDataType & 0xffff0000) >>> 16;
                                	imaginaryDataType = imaginaryDataType & 0xffff;
                                	haveSmallImaginaryData = true;
                                }
                                else {
                                    imaginaryDataBytes = getInt(endianess);
                                    haveSmallImaginaryData = false;
                                }
                        	    if (imaginaryDataType == miUINT64) {
                        	    	Preferences.debug("imaginaryDataType == miUINT64 as expected\n", Preferences.DEBUG_FILEIO);
                        	    }
                        	    else {
                        	    	Preferences.debug("imaginaryDataType unexpectedly == " + imaginaryDataType + "\n", 
                        	    			Preferences.DEBUG_FILEIO);
                        	    }
                        	     
                                if (imaginaryDataBytes == realDataBytes) {
                                	Preferences.debug("imaginaryDataBytes == realDataBytes as expected\n", 
                                			Preferences.DEBUG_FILEIO);
                                }
                                else {
                                	Preferences.debug("imaginaryDataBytes unexpectedly != realDataBytes\n", 
                                			Preferences.DEBUG_FILEIO);
                                }
                                raFile.read(buffer);
                                imaginaryDBuffer = new double[longNumber];
                                j = 0;
    	                    	for (s = 0; s < numberSlices; s++) {
    		                    	for (x = 0; x < imageExtents[1]; x++) {
    		                    		for (y = 0; y < imageExtents[0]; y++) {
    		                    			index = 8*(x + imageExtents[1] * y + s * sliceSize);
                                            b1L = buffer[index] & 0xffL;
                                            b2L = buffer[index+1] & 0xffL;
                                            b3L = buffer[index+2] & 0xffL;
                                            b4L = buffer[index+3] & 0xffL;
                                            b5L = buffer[index+4] & 0xffL;
                                            b6L = buffer[index+5] & 0xffL;
                                            b7L = buffer[index+6] & 0xffL;
                                            b8L = buffer[index+7] & 0xffL;
                                            if (endianess == BIG_ENDIAN) {
                                            	imaginaryDBuffer[j++] = (double)((b1L << 56) | (b2L << 48) | (b3L << 40) | (b4L << 32) |
    	                                                   (b5L << 24) | (b6L << 16) | (b7L << 8) | b8L);	
                                            }
                                            else {
                                            	imaginaryDBuffer[j++] = (double)((b8L << 56) | (b7L << 48) | (b6L << 40) | (b5L << 32) |
    	                                                   (b4L << 24) | (b3L << 16) | (b2L << 8) | b1L);
                                            }
    		                    		}
    		                    	}
    	                    	}
                                
                                if (haveSmallImaginaryData) {
                        		    if (imaginaryDataBytes < 4) {
                        		    	padBytes = 4 - imaginaryDataBytes;
                        		    	for (i = 0; i < padBytes; i++) {
                        		    		raFile.readByte();
                        		    	}
                        		    }
                        		}
                        		else if ((imaginaryDataBytes % 8) != 0) {
                        	    	padBytes = 8 - (imaginaryDataBytes % 8);
                        	    	for (i = 0; i < padBytes; i++) {
                            	    	raFile.readByte();
                            	    }
                        	    }   
                                
                                try {
                        			image2.importDComplexData(2 * nonLogicalField * realDBuffer.length, realDBuffer, imaginaryDBuffer, true, logMagDisplay);
                        		}
                        		catch(IOException e) {
                        		   MipavUtil.displayError("IOException on image2.importDComplexData(2 * nonLogicalField * realDBuffer.length, " +
                        		   		"realDBuffer, imaginaryDBuffer, true, logMagDisplay)");
                        		   throw e;
                        		}
                        	}
                        	break;
                        default:
                        	Preferences.debug("Illegal data type = " + realDataType + "\n", Preferences.DEBUG_FILEIO);
                        	Preferences.debug("Real data bytes = " + realDataBytes + "\n", Preferences.DEBUG_FILEIO);
                        }	
                    	
                    } // else imagesFound > 1
                    } // for (field = 0; field < fieldNumber; field++)
                    if (logicalFields >= 1) {
                	    adjustedFieldDim = imageExtents[imageExtents.length-1] - logicalFields;
                	    if (adjustedFieldDim >= 2) {
                	    	newExtents = new int[imageExtents.length];
                	    	for (i = 0; i < imageExtents.length - 1; i++) {
                	    		newExtents[i] = imageExtents[i];
                	    	}
                	    	newExtents[imageExtents.length-1] = adjustedFieldDim;
                	    } // if (adjustedFieldDim >= 2)
                	    else {
                	    	newExtents = new int[imageExtents.length-1];
                	    	for (i = 0; i < imageExtents.length - 1; i++) {
                	    		newExtents[i] = imageExtents[i];
                	    	}
                	    }
                	    totalNumber = 1;
                	    imageSlices = 1;
                	    for (i = 0; i < newExtents.length; i++) {
                	    	totalNumber *= newExtents[i];
                	    	if (i > 1) {
                	    		imageSlices *= newExtents[i];
                	    	}
                	    }
                	} // if (logicalFields >= 1)
                    if (imagesFound == 1) {
                        if (logicalFields >= 1) {
                        	switch(image.getType()) {
                        	case ModelStorageBase.BYTE:
                        		buffer = new byte[totalNumber];
                        		try {
                        			image.exportData(0, totalNumber, buffer);
                        		}
                        		catch(IOException e) {
                        		   MipavUtil.displayError("IOException on image.exportData(0, totalNumber, buffer)");
                        		   throw e;
                        		}
                        		image.changeExtents(newExtents);
                        		image.recomputeDataSize();
                        		try {
                        			image.importData(0, buffer, true);
                        		}
                        		catch(IOException e) {
                        			MipavUtil.displayError("IOException on image.importData(0, buffer, true)");
                        			throw e;
                        		}
                        		break;
                        	case ModelStorageBase.UBYTE:
                        		shortBuffer = new short[totalNumber];
                        		try {
                        			image.exportData(0, totalNumber, shortBuffer);
                        		}
                        		catch(IOException e) {
                        		   MipavUtil.displayError("IOException on image.exportData(0, totalNumber, shortBuffer)");
                        		   throw e;
                        		}
                        		image.changeExtents(newExtents);
                        		image.recomputeDataSize();
                        		try {
                        			image.importUData(0, shortBuffer, true);
                        		}
                        		catch(IOException e) {
                        			MipavUtil.displayError("IOException on image.importUData(0, shortBuffer, true)");
                        			throw e;
                        		}
                        		break;
                        	case ModelStorageBase.SHORT:
                        		shortBuffer = new short[totalNumber];
                        		try {
                        			image.exportData(0, totalNumber, shortBuffer);
                        		}
                        		catch(IOException e) {
                        		   MipavUtil.displayError("IOException on image.exportData(0, totalNumber, shortBuffer)");
                        		   throw e;
                        		}
                        		image.changeExtents(newExtents);
                        		image.recomputeDataSize();
                        		try {
                        			image.importData(0, shortBuffer, true);
                        		}
                        		catch(IOException e) {
                        			MipavUtil.displayError("IOException on image.importData(0, shortBuffer, true)");
                        			throw e;
                        		}
                        		break;
                        	case ModelStorageBase.USHORT:
                        		intBuffer = new int[totalNumber];
                        		try {
                        			image.exportData(0, totalNumber, intBuffer);
                        		}
                        		catch(IOException e) {
                        		   MipavUtil.displayError("IOException on image.exportData(0, totalNumber, intBuffer)");
                        		   throw e;
                        		}
                        		image.changeExtents(newExtents);
                        		image.recomputeDataSize();
                        		try {
                        			image.importUData(0, intBuffer, true);
                        		}
                        		catch(IOException e) {
                        			MipavUtil.displayError("IOException on image.importUData(0, intBuffer, true)");
                        			throw e;
                        		}
                        		break;
                        	case ModelStorageBase.INTEGER:
                        		intBuffer = new int[totalNumber];
                        		try {
                        			image.exportData(0, totalNumber, intBuffer);
                        		}
                        		catch(IOException e) {
                        		   MipavUtil.displayError("IOException on image.exportData(0, totalNumber, intBuffer)");
                        		   throw e;
                        		}
                        		image.changeExtents(newExtents);
                        		image.recomputeDataSize();
                        		try {
                        			image.importData(0, intBuffer, true);
                        		}
                        		catch(IOException e) {
                        			MipavUtil.displayError("IOException on image.importData(0, intBuffer, true)");
                        			throw e;
                        		}
                        		break;
                        	case ModelStorageBase.UINTEGER:
                        		longBuffer = new long[totalNumber];
                        		try {
                        			image.exportData(0, totalNumber, longBuffer);
                        		}
                        		catch(IOException e) {
                        		   MipavUtil.displayError("IOException on image.exportData(0, totalNumber, longBuffer)");
                        		   throw e;
                        		}
                        		image.changeExtents(newExtents);
                        		image.recomputeDataSize();
                        		try {
                        			image.importUData(0, longBuffer, true);
                        		}
                        		catch(IOException e) {
                        			MipavUtil.displayError("IOException on image.importUData(0, longBuffer, true)");
                        			throw e;
                        		}
                        		break;	
                        	case ModelStorageBase.LONG:
                        		longBuffer = new long[totalNumber];
                        		try {
                        			image.exportData(0, totalNumber, longBuffer);
                        		}
                        		catch(IOException e) {
                        		   MipavUtil.displayError("IOException on image.exportData(0, totalNumber, longBuffer)");
                        		   throw e;
                        		}
                        		image.changeExtents(newExtents);
                        		image.recomputeDataSize();
                        		try {
                        			image.importData(0, longBuffer, true);
                        		}
                        		catch(IOException e) {
                        			MipavUtil.displayError("IOException on image.importData(0, longBuffer, true)");
                        			throw e;
                        		}
                        		break;
                        	case ModelStorageBase.FLOAT:
                        		floatBuffer = new float[totalNumber];
                        		try {
                        			image.exportData(0, totalNumber, floatBuffer);
                        		}
                        		catch(IOException e) {
                        		   MipavUtil.displayError("IOException on image.exportData(0, totalNumber, floatBuffer)");
                        		   throw e;
                        		}
                        		image.changeExtents(newExtents);
                        		image.recomputeDataSize();
                        		try {
                        			image.importData(0, floatBuffer, true);
                        		}
                        		catch(IOException e) {
                        			MipavUtil.displayError("IOException on image.importData(0, floatBuffer, true)");
                        			throw e;
                        		}
                        		break;
                        	case ModelStorageBase.DOUBLE:
                        		doubleBuffer = new double[totalNumber];
                        		try {
                        			image.exportData(0, totalNumber, doubleBuffer);
                        		}
                        		catch(IOException e) {
                        		   MipavUtil.displayError("IOException on image.exportData(0, totalNumber, doubleBuffer)");
                        		   throw e;
                        		}
                        		image.changeExtents(newExtents);
                        		image.recomputeDataSize();
                        		try {
                        			image.importData(0, doubleBuffer, true);
                        		}
                        		catch(IOException e) {
                        			MipavUtil.displayError("IOException on image.importData(0, doubleBuffer, true)");
                        			throw e;
                        		}
                        		break;
                        	case ModelStorageBase.COMPLEX:
                        		realBuffer = new float[totalNumber];
                        		imaginaryBuffer = new float[totalNumber];
                        		try {
                        			image.exportComplexData(0, totalNumber, realBuffer, imaginaryBuffer);
                        		}
                        		catch(IOException e) {
                        		   MipavUtil.displayError("IOException on image.exportComplexData(0, totalNumber, realBuffer, imaginaryBuffer)");
                        		   throw e;
                        		}
                        		image.changeExtents(newExtents);
                        		image.recomputeDataSize();
                        	    
                                try {
                        			image.importComplexData(0, realBuffer, imaginaryBuffer, true, logMagDisplay);
                        		}
                        		catch(IOException e) {
                        		   MipavUtil.displayError("IOException on image.importComplexData(0, realBuffer, imaginaryBuffer, true, logMagDisplay)");
                        		   throw e;
                        		}		
                        		break;
                        	case ModelStorageBase.DCOMPLEX:
                        		realDBuffer = new double[totalNumber];
                        		imaginaryDBuffer = new double[totalNumber];
                        		try {
                        			image.exportDComplexData(0, totalNumber, realDBuffer, imaginaryDBuffer);
                        		}
                        		catch(IOException e) {
                        		   MipavUtil.displayError("IOException on image.exportDComplexData(0, totalNumber, realDBuffer, imaginaryDBuffer)");
                        		   throw e;
                        		}
                        		image.changeExtents(newExtents);
                        		image.recomputeDataSize();
                        		
                                try {
                        			image.importDComplexData(0, realDBuffer, imaginaryDBuffer, true, logMagDisplay);
                        		}
                        		catch(IOException e) {
                        		   MipavUtil.displayError("IOException on image.importDComplexData(0, realDBuffer, imaginaryDBuffer, true, logMagDisplay)");
                        		   throw e;
                        		}		
                        		break;
                        	} // switch(image.getType())
                        	fileInfo.setExtents(newExtents);
                        } // if (logicalFields >= 1)
                    	image.calcMinMax();
                    	fileInfo.setMin(image.getMin());
                    	fileInfo.setMax(image.getMax());	
                    }
                    else {
                    	if (logicalFields >= 1) {
                    		switch(image2.getType()) {
                        	case ModelStorageBase.BYTE:
                        		buffer = new byte[totalNumber];
                        		try {
                        			image2.exportData(0, totalNumber, buffer);
                        		}
                        		catch(IOException e) {
                        		   MipavUtil.displayError("IOException on image2.exportData(0, totalNumber, buffer)");
                        		   throw e;
                        		}
                        		image2.changeExtents(newExtents);
                        		image2.recomputeDataSize();
                        		try {
                        			image2.importData(0, buffer, true);
                        		}
                        		catch(IOException e) {
                        			MipavUtil.displayError("IOException on image2.importData(0, buffer, true)");
                        			throw e;
                        		}
                        		break;
                        	case ModelStorageBase.UBYTE:
                        		shortBuffer = new short[totalNumber];
                        		try {
                        			image2.exportData(0, totalNumber, shortBuffer);
                        		}
                        		catch(IOException e) {
                        		   MipavUtil.displayError("IOException on image2.exportData(0, totalNumber, shortBuffer)");
                        		   throw e;
                        		}
                        		image2.changeExtents(newExtents);
                        		image2.recomputeDataSize();
                        		try {
                        			image2.importUData(0, shortBuffer, true);
                        		}
                        		catch(IOException e) {
                        			MipavUtil.displayError("IOException on image2.importUData(0, shortBuffer, true)");
                        			throw e;
                        		}
                        		break;
                        	case ModelStorageBase.SHORT:
                        		shortBuffer = new short[totalNumber];
                        		try {
                        			image2.exportData(0, totalNumber, shortBuffer);
                        		}
                        		catch(IOException e) {
                        		   MipavUtil.displayError("IOException on image2.exportData(0, totalNumber, shortBuffer)");
                        		   throw e;
                        		}
                        		image2.changeExtents(newExtents);
                        		image2.recomputeDataSize();
                        		try {
                        			image2.importData(0, shortBuffer, true);
                        		}
                        		catch(IOException e) {
                        			MipavUtil.displayError("IOException on image2.importData(0, shortBuffer, true)");
                        			throw e;
                        		}
                        		break;
                        	case ModelStorageBase.USHORT:
                        		intBuffer = new int[totalNumber];
                        		try {
                        			image2.exportData(0, totalNumber, intBuffer);
                        		}
                        		catch(IOException e) {
                        		   MipavUtil.displayError("IOException on image2.exportData(0, totalNumber, intBuffer)");
                        		   throw e;
                        		}
                        		image2.changeExtents(newExtents);
                        		image2.recomputeDataSize();
                        		try {
                        			image2.importUData(0, intBuffer, true);
                        		}
                        		catch(IOException e) {
                        			MipavUtil.displayError("IOException on image2.importUData(0, intBuffer, true)");
                        			throw e;
                        		}
                        		break;
                        	case ModelStorageBase.INTEGER:
                        		intBuffer = new int[totalNumber];
                        		try {
                        			image2.exportData(0, totalNumber, intBuffer);
                        		}
                        		catch(IOException e) {
                        		   MipavUtil.displayError("IOException on image2.exportData(0, totalNumber, intBuffer)");
                        		   throw e;
                        		}
                        		image2.changeExtents(newExtents);
                        		image2.recomputeDataSize();
                        		try {
                        			image2.importData(0, intBuffer, true);
                        		}
                        		catch(IOException e) {
                        			MipavUtil.displayError("IOException on image2.importData(0, intBuffer, true)");
                        			throw e;
                        		}
                        		break;
                        	case ModelStorageBase.UINTEGER:
                        		longBuffer = new long[totalNumber];
                        		try {
                        			image2.exportData(0, totalNumber, longBuffer);
                        		}
                        		catch(IOException e) {
                        		   MipavUtil.displayError("IOException on image2.exportData(0, totalNumber, longBuffer)");
                        		   throw e;
                        		}
                        		image2.changeExtents(newExtents);
                        		image2.recomputeDataSize();
                        		try {
                        			image2.importUData(0, longBuffer, true);
                        		}
                        		catch(IOException e) {
                        			MipavUtil.displayError("IOException on image2.importUData(0, longBuffer, true)");
                        			throw e;
                        		}
                        		break;	
                        	case ModelStorageBase.LONG:
                        		longBuffer = new long[totalNumber];
                        		try {
                        			image2.exportData(0, totalNumber, longBuffer);
                        		}
                        		catch(IOException e) {
                        		   MipavUtil.displayError("IOException on image2.exportData(0, totalNumber, longBuffer)");
                        		   throw e;
                        		}
                        		image2.changeExtents(newExtents);
                        		image2.recomputeDataSize();
                        		try {
                        			image2.importData(0, longBuffer, true);
                        		}
                        		catch(IOException e) {
                        			MipavUtil.displayError("IOException on image2.importData(0, longBuffer, true)");
                        			throw e;
                        		}
                        		break;
                        	case ModelStorageBase.FLOAT:
                        		floatBuffer = new float[totalNumber];
                        		try {
                        			image2.exportData(0, totalNumber, floatBuffer);
                        		}
                        		catch(IOException e) {
                        		   MipavUtil.displayError("IOException on image2.exportData(0, totalNumber, floatBuffer)");
                        		   throw e;
                        		}
                        		image2.changeExtents(newExtents);
                        		image2.recomputeDataSize();
                        		try {
                        			image2.importData(0, floatBuffer, true);
                        		}
                        		catch(IOException e) {
                        			MipavUtil.displayError("IOException on image2.importData(0, floatBuffer, true)");
                        			throw e;
                        		}
                        		break;
                        	case ModelStorageBase.DOUBLE:
                        		doubleBuffer = new double[totalNumber];
                        		try {
                        			image2.exportData(0, totalNumber, doubleBuffer);
                        		}
                        		catch(IOException e) {
                        		   MipavUtil.displayError("IOException on image2.exportData(0, totalNumber, doubleBuffer)");
                        		   throw e;
                        		}
                        		image2.changeExtents(newExtents);
                        		image2.recomputeDataSize();
                        		try {
                        			image2.importData(0, doubleBuffer, true);
                        		}
                        		catch(IOException e) {
                        			MipavUtil.displayError("IOException on image2.importData(0, doubleBuffer, true)");
                        			throw e;
                        		}
                        		break;
                        	case ModelStorageBase.COMPLEX:
                        		realBuffer = new float[totalNumber];
                        		imaginaryBuffer = new float[totalNumber];
                        		try {
                        			image2.exportComplexData(0, totalNumber, realBuffer, imaginaryBuffer);
                        		}
                        		catch(IOException e) {
                        		   MipavUtil.displayError("IOException on image2.exportComplexData(0, totalNumber, realBuffer, imaginaryBuffer)");
                        		   throw e;
                        		}
                        		image2.changeExtents(newExtents);
                        		image2.recomputeDataSize();
                        	    
                                try {
                        			image2.importComplexData(0, realBuffer, imaginaryBuffer, true, logMagDisplay);
                        		}
                        		catch(IOException e) {
                        		   MipavUtil.displayError("IOException on image2.importComplexData(0, realBuffer, imaginaryBuffer, true, logMagDisplay)");
                        		   throw e;
                        		}		
                        		break;
                        	case ModelStorageBase.DCOMPLEX:
                        		realDBuffer = new double[totalNumber];
                        		imaginaryDBuffer = new double[totalNumber];
                        		try {
                        			image2.exportDComplexData(0, totalNumber, realDBuffer, imaginaryDBuffer);
                        		}
                        		catch(IOException e) {
                        		   MipavUtil.displayError("IOException on image2.exportDComplexData(0, totalNumber, realDBuffer, imaginaryDBuffer)");
                        		   throw e;
                        		}
                        		image2.changeExtents(newExtents);
                        		image2.recomputeDataSize();
                        		
                                try {
                        			image2.importDComplexData(0, realDBuffer, imaginaryDBuffer, true, logMagDisplay);
                        		}
                        		catch(IOException e) {
                        		   MipavUtil.displayError("IOException on image2.importDComplexData(0, realDBuffer, imaginaryDBuffer, true, logMagDisplay)");
                        		   throw e;
                        		}		
                        		break;
                        	} // switch(image.getType())
                    	    fileInfo2.setExtents(newExtents);	
                    	}
                    	image2.calcMinMax();
                        fileInfo2.setMin(image2.getMin());
                	    fileInfo2.setMax(image2.getMax());
                    }
                	break;
                case miUTF8:
                	Preferences.debug("Data type = miUTF8\n", Preferences.DEBUG_FILEIO);
                	Preferences.debug("Bytes in data element = " + elementBytes + "\n", Preferences.DEBUG_FILEIO);
                	buffer = new byte[elementBytes];
                	raFile.read(buffer);
                	str = new String(buffer, 0, elementBytes, "UTF-8");
                	Preferences.debug("UTF-8 encoded character data:\n" + str + "\n", Preferences.DEBUG_FILEIO);
                	break;
                case miUTF16:
                	Preferences.debug("Data type = miUTF16\n", Preferences.DEBUG_FILEIO);
                	Preferences.debug("Bytes in data element = " + elementBytes + "\n", Preferences.DEBUG_FILEIO);
                	buffer = new byte[elementBytes];
                	raFile.read(buffer);
                	if (endianess == FileBase.BIG_ENDIAN) {
                	    str = new String(buffer, 0, elementBytes, "UTF-16BE");
                	}
                	else {
                		str = new String(buffer, 0, elementBytes, "UTF-16LE");	
                	}
                	Preferences.debug("UTF-16 encoded character data:\n" + str + "\n", Preferences.DEBUG_FILEIO);
                	break;
                case miUTF32:
                	Preferences.debug("Data type = miUTF32\n", Preferences.DEBUG_FILEIO);
                	Preferences.debug("Bytes in data element = " + elementBytes + "\n", Preferences.DEBUG_FILEIO);
                	buffer = new byte[elementBytes];
                	raFile.read(buffer);
                	if (endianess == FileBase.BIG_ENDIAN) {
                	    str = new String(buffer, 0, elementBytes, "UTF-32BE");
                	}
                	else {
                		str = new String(buffer, 0, elementBytes, "UTF-32LE");	
                	}
                	Preferences.debug("UTF-32 encoded character data:\n" + str + "\n", Preferences.DEBUG_FILEIO);
                	break;
                default:
                	Preferences.debug("Illegal data type = " + dataType + "\n", Preferences.DEBUG_FILEIO);
                	Preferences.debug("Bytes in data element = " + elementBytes + "\n", Preferences.DEBUG_FILEIO);
                }
                if (isCompressed) {
                	raFile.close();
	                try {
	                    ufile.delete();
	                } catch (final SecurityException sc) {
	                    MipavUtil.displayError("Security error occurs while trying to delete " + uncompressedName);
	                }
                	raFile = new RandomAccessFile(file, "r");
                }
            } // while (nextElementAddress)
            raFile.close();
            if (zlibDecompresser != null) {
                zlibDecompresser.end();
                zlibDecompresser = null;
            }
            
           
            if (fileInfo.getArrayName() != null) {
            	image.setImageName(fileInfo.getArrayName());
            	fileInfo.setFileName(fileInfo.getArrayName());
            }
            fileInfo.setSourceFile(fileDir + fileName);
            for (i = 0; i < imageSlices; i++) {
                
                image.setFileInfo((FileInfoMATLAB)fileInfo.clone(), i);
            }
            
            if (((image.getType() == ModelStorageBase.UBYTE)|| (image.getType() == ModelStorageBase.SHORT)) &&
            	(image2 != null) && (image2.getType() == ModelStorageBase.DOUBLE) &&
            	(image2.getExtents()[0]  == 3)  && (image2.getExtents()[1] <= 256)) {
            	// image2 is really a lookup table for image
            	totalNumber = 1;
            	for (i = 0; i < image.getNDims(); i++) {
            		totalNumber = totalNumber * image.getExtents()[i];
            	}
            	shortBuffer = new short[totalNumber];
        		try {
        			image.exportData(0, totalNumber, shortBuffer);
        		}
        		catch(IOException e) {
        		   MipavUtil.displayError("IOException on image.exportData(0, totalNumber, shortBuffer)");
        		   throw e;
        		}
        		shortMin = 65535;
        		shortMax = -32768;
        		for (i = 0; i < totalNumber; i++) {
        			if (shortBuffer[i] < shortMin) {
        				shortMin = shortBuffer[i];
        			}
        			if (shortBuffer[i] > shortMax) {
        				shortMax = shortBuffer[i];
        			}
        		}
        		
        		shortRange = shortMax - shortMin;
        		image = new ModelImage(ModelStorageBase.ARGB_FLOAT, image.getExtents(), image.getImageFileName());
    			fileInfo.setDataType(ModelStorageBase.ARGB_FLOAT);
    			floatBuffer = new float[4 * totalNumber];
    			totalNumber2 = 1;
    			for (i = 0; i < image2.getNDims(); i++) {
    				totalNumber2 = totalNumber2 * image2.getExtents()[i];
    			}
    			doubleBuffer = new double[totalNumber2];
    			maxColorIndex = (totalNumber2/3) - 1;
    			scale = (double)maxColorIndex/(double)shortRange;
    			try {
    				image2.exportData(0, totalNumber2, doubleBuffer);
    			}
    			catch(IOException e) {
         		   MipavUtil.displayError("IOException on image2.exportData(0, totalNumber2, doubleBuffer)");
         		   throw e;
         		}
    			
    			image2.disposeLocal();
    			image2 = null;
    			for (i = 0; i < totalNumber; i++) {
    				floatBuffer[4*i] = 0.0f;
    				scaledIndex = (scale*(shortBuffer[i] - shortMin));
    				lowIndex = Math.max(0,(int)(scale*(shortBuffer[i] - shortMin)));
    				highIndex = Math.min(maxColorIndex,lowIndex+1);
    				if (lowIndex == highIndex) {
    					lowFraction = 1.0;
    					highFraction = 0.0;
    				}
    				else {
    				    lowFraction = Math.min(1.0, highIndex - scaledIndex);
    				    highFraction = Math.min(1.0, scaledIndex - lowIndex);
    				}
    				floatBuffer[4*i+1] = (float)(255*(lowFraction*doubleBuffer[3*lowIndex] + highFraction*doubleBuffer[3*highIndex]));
    				floatBuffer[4*i+2] = (float)(255*(lowFraction*doubleBuffer[3*lowIndex+1] + highFraction*doubleBuffer[3*highIndex+1]));
    				floatBuffer[4*i+3] = (float)(255*(lowFraction*doubleBuffer[3*lowIndex+2] + highFraction*doubleBuffer[3*highIndex+2]));
    			}
    			try {
    				image.importData(0, floatBuffer, true);
    			}
    			catch(IOException e) {
    				MipavUtil.displayError("IOException on image.importData(0, floatBuffer, true)");
    				throw e;
    			}
            }
            else if ((image.getType() == ModelStorageBase.DOUBLE) && (image.getExtents()[0]  == 3)  && (image.getExtents()[1] <= 256) &&
            		 (image2 != null) && ((image2.getType() == ModelStorageBase.UBYTE) || (image2.getType() == ModelStorageBase.SHORT))) {
                	// image2 is really a lookup table for image
                	totalNumber = 1;
                	for (i = 0; i < image2.getNDims(); i++) {
                		totalNumber = totalNumber * image2.getExtents()[i];
                	}
                	shortBuffer = new short[totalNumber];
            		try {
            			image2.exportData(0, totalNumber, shortBuffer);
            		}
            		catch(IOException e) {
            		   MipavUtil.displayError("IOException on image2.exportData(0, totalNumber, shortBuffer)");
            		   throw e;
            		}
            		shortMin = 65535;
            		shortMax = -32768;
            		for (i = 0; i < totalNumber; i++) {
            			if (shortBuffer[i] < shortMin) {
            				shortMin = shortBuffer[i];
            			}
            			if (shortBuffer[i] > shortMax) {
            				shortMax = shortBuffer[i];
            			}
            		}
            		shortRange = shortMax - shortMin;
        			floatBuffer = new float[4 * totalNumber];
        			totalNumber2 = 1;
        			for (i = 0; i < image.getNDims(); i++) {
        				totalNumber2 = totalNumber2 * image.getExtents()[i];
        			}
        			doubleBuffer = new double[totalNumber2];
        			maxColorIndex = (totalNumber2/3) - 1;
        			scale = (double)maxColorIndex/(double)shortRange;
        			try {
        				image.exportData(0, totalNumber2, doubleBuffer);
        			}
        			catch(IOException e) {
             		   MipavUtil.displayError("IOException on image.exportData(0, totalNumber2, doubleBuffer)");
             		   throw e;
             		}
        			
        			image = new ModelImage(ModelStorageBase.ARGB_FLOAT, image2.getExtents(), image2.getImageFileName());
        			fileInfo = (FileInfoMATLAB)fileInfo2.clone();
        			fileInfo.setDataType(ModelStorageBase.ARGB_FLOAT);
        			image2.disposeLocal();
        			image2 = null;
        			for (i = 0; i < totalNumber; i++) {
        				floatBuffer[4*i] = 0.0f;
        				scaledIndex = (scale*(shortBuffer[i] - shortMin));
        				lowIndex = Math.max(0,(int)(scale*(shortBuffer[i] - shortMin)));
        				highIndex = Math.min(maxColorIndex,lowIndex+1);
        				if (lowIndex == highIndex) {
        					lowFraction = 1.0;
        					highFraction = 0.0;
        				}
        				else {
        				    lowFraction = Math.min(1.0, highIndex - scaledIndex);
        				    highFraction = Math.min(1.0, scaledIndex - lowIndex);
        				}
        				floatBuffer[4*i+1] = (float)(255*(lowFraction*doubleBuffer[3*lowIndex] + highFraction*doubleBuffer[3*highIndex]));
        				floatBuffer[4*i+2] = (float)(255*(lowFraction*doubleBuffer[3*lowIndex+1] + highFraction*doubleBuffer[3*highIndex+1]));
        				floatBuffer[4*i+3] = (float)(255*(lowFraction*doubleBuffer[3*lowIndex+2] + highFraction*doubleBuffer[3*highIndex+2]));
        			}
        			try {
        				image.importData(0, floatBuffer, true);
        			}
        			catch(IOException e) {
        				MipavUtil.displayError("IOException on image.importData(0, floatBuffer, true)");
        				throw e;
        			}
                }
                
            
            
            if (image2 != null) {
            	fileInfo2.setSourceFile(fileDir + fileName);
            	if (fileInfo2.getArrayName() != null) {
            		image2.setImageName(fileInfo2.getArrayName());
            		fileInfo2.setFileName(fileInfo2.getArrayName());
            	}
            	 for (i = 0; i < imageSlices2; i++) {
                     
                     image2.setFileInfo((FileInfoMATLAB)fileInfo2.clone(), i);
                 }
            	 vFrame2 = new ViewJFrameImage(image2);
            	 if (fileInfo2.getArrayName() != null) {
            		 vFrame2.setTitle(fileInfo2.getArrayName());
            	 }
            	 else {
            	     vFrame2.setTitle(fileName.substring(0,st) + "_2");
            	 }
            }
            
            if (maskImage != null) {
            	if (!isVOI) {
            		// If xDim or yDim of maskImage does not equal xDim or yDim of image,
            		// then do not convert to a VOI
	            	maskImage.setFileInfo(maskFileInfo, 0);
	            	vmFrame = new ViewJFrameImage(maskImage);
	            	if (voiFieldName != null) {
	            		vmFrame.setTitle(voiFieldName);
	            	}
	            	else {
	            	    vmFrame.setTitle(fileName.substring(0,st) + "_mask");
	            	}
            	} // if (!isVOI)
            	else { // convert maskImage to VOI
	            	if ((image.getNDims() >= 3) && (maskImage.getNDims() == 2)) {
	            		// Convert maskImage from 2D to 3D
	            		sliceSize = image.getExtents()[0]*image.getExtents()[1];
	            		newExtents = new int[3];
	            	    for (i = 0; i < 3; i++) {
	            	    	newExtents[i] = image.getExtents()[i];
	            	    }
	            		switch(maskImage.getType()) {
                    	case ModelStorageBase.BYTE:
                    		buffer = new byte[sliceSize];
                    		try {
                    			maskImage.exportData(0, sliceSize, buffer);
                    		}
                    		catch(IOException e) {
                    		   MipavUtil.displayError("IOException on maskImage.exportData(0, sliceSize, buffer)");
                    		   throw e;
                    		}
                    		maskImage.changeExtents(newExtents);
                    		maskImage.recomputeDataSize();
                    		for (i = 0; i < image.getExtents()[2]; i++) {
	                    		try {
	                    			maskImage.importData(i * sliceSize, buffer, false);
	                    		}
	                    		catch(IOException e) {
	                    			MipavUtil.displayError("IOException on maskImage.importData(i * sliceSize, buffer, false)");
	                    			throw e;
	                    		}
                    		}
                    		maskImage.calcMinMax();
                    		break;
                    	case ModelStorageBase.UBYTE:
                    		shortBuffer = new short[sliceSize];
                    		try {
                    			maskImage.exportData(0, sliceSize, shortBuffer);
                    		}
                    		catch(IOException e) {
                    		   MipavUtil.displayError("IOException on maskImage.exportData(0, sliceSize, shortBuffer)");
                    		   throw e;
                    		}
                    		maskImage.changeExtents(newExtents);
                    		maskImage.recomputeDataSize();
                    		for (i = 0; i < image.getExtents()[2]; i++) {
	                    		try {
	                    			maskImage.importUData(i * sliceSize, shortBuffer, false);
	                    		}
	                    		catch(IOException e) {
	                    			MipavUtil.displayError("IOException on maskImage.importUData(i * sliceSize, shortBuffer, false)");
	                    			throw e;
	                    		}
                    		}
                    		maskImage.calcMinMax();
                    		break;
	            		}
	            	} // if ((image.getNDims() >= 3) && (maskImage.getNDims() == 2))
	            	boolean wholeImage = true;
	            	if (maskImage.getNDims() == 2) { 
	                    AlgorithmMorphology2D idObjectsAlgo2D;
	                    int method = AlgorithmMorphology2D.ID_OBJECTS;
	                    
	                    idObjectsAlgo2D = new AlgorithmMorphology2D(maskImage, 0, 0, method, 0, 0, 0, 0, wholeImage);
	                    idObjectsAlgo2D.setMinMax(1, Integer.MAX_VALUE);
	                    idObjectsAlgo2D.run();
	                    idObjectsAlgo2D.finalize();
	                    idObjectsAlgo2D = null;
	            	}
	            	else { 
	                    AlgorithmMorphology3D idObjectsAlgo3D;
	                    int method = AlgorithmMorphology3D.ID_OBJECTS;
	                    
	                    idObjectsAlgo3D = new AlgorithmMorphology3D(maskImage, 0, 0, method, 0, 0, 0, 0, wholeImage);
	                    idObjectsAlgo3D.setMinMax(1, Integer.MAX_VALUE);
	                    idObjectsAlgo3D.run();
	                    idObjectsAlgo3D.finalize();
	                    idObjectsAlgo3D = null;
	            	}
	            	maskImage.calcMinMax();
	                final AlgorithmVOIExtraction VOIExtractionAlgo = new AlgorithmVOIExtraction(maskImage);

	                // VOIExtractionAlgo.setActiveImage(false);
	                VOIExtractionAlgo.run();
	                
	                VOIVector kVOIs = maskImage.getVOIs();
	                for (i = 0; i < kVOIs.size(); i++ )
	                {
	                    VOI kCurrentGroup = kVOIs.get(i);
	                    kCurrentGroup.setAllActive(true);
	                }
	                maskImage.groupVOIs();
	                kVOIs = maskImage.getVOIs();
	                if (voiFieldName != null) {
	                    ((VOI)kVOIs.get(0)).setName(voiFieldName);
	                }
	                image.setVOIs(kVOIs);
	                maskImage.disposeLocal();
	                maskImage = null;
            	} // convert maskImage to VOI
            }
            
            if (maskImage2 != null) {
            	if (!isVOI2) {
            		// If xDim or yDim of maskImage2 does not equal xDim or yDim of image2,
            		// then do not convert to a VOI
	            	maskImage2.setFileInfo(maskFileInfo2, 0);
	            	vmFrame2 = new ViewJFrameImage(maskImage2);
	            	if (voi2FieldName != null) {
	            		vmFrame2.setTitle(voi2FieldName);
	            	}
	            	else {
	            	    vmFrame2.setTitle(fileName.substring(0,st) + "_2_mask");
	            	}
            	} // if (!isVOI2)
            	else { // convert maskImage2 to VOI
	            	if ((image2.getNDims() >= 3) && (maskImage2.getNDims() == 2)) {
	            		// Convert maskImage2 from 2D to 3D
	            		sliceSize = image2.getExtents()[0]*image2.getExtents()[1];
	            		newExtents = new int[3];
	            	    for (i = 0; i < 3; i++) {
	            	    	newExtents[i] = image2.getExtents()[i];
	            	    }
	            		switch(maskImage2.getType()) {
                    	case ModelStorageBase.BYTE:
                    		buffer = new byte[sliceSize];
                    		try {
                    			maskImage2.exportData(0, sliceSize, buffer);
                    		}
                    		catch(IOException e) {
                    		   MipavUtil.displayError("IOException on maskImage2.exportData(0, sliceSize, buffer)");
                    		   throw e;
                    		}
                    		maskImage2.changeExtents(newExtents);
                    		maskImage2.recomputeDataSize();
                    		for (i = 0; i < image2.getExtents()[2]; i++) {
	                    		try {
	                    			maskImage2.importData(i * sliceSize, buffer, false);
	                    		}
	                    		catch(IOException e) {
	                    			MipavUtil.displayError("IOException on maskImage2.importData(i * sliceSize, buffer, false)");
	                    			throw e;
	                    		}
                    		}
                    		maskImage2.calcMinMax();
                    		break;
                    	case ModelStorageBase.UBYTE:
                    		shortBuffer = new short[sliceSize];
                    		try {
                    			maskImage2.exportData(0, sliceSize, shortBuffer);
                    		}
                    		catch(IOException e) {
                    		   MipavUtil.displayError("IOException on maskImage2.exportData(0, sliceSize, shortBuffer)");
                    		   throw e;
                    		}
                    		maskImage2.changeExtents(newExtents);
                    		maskImage2.recomputeDataSize();
                    		for (i = 0; i < image2.getExtents()[2]; i++) {
	                    		try {
	                    			maskImage2.importUData(i * sliceSize, shortBuffer, false);
	                    		}
	                    		catch(IOException e) {
	                    			MipavUtil.displayError("IOException on maskImage2.importUData(i * sliceSize, shortBuffer, false)");
	                    			throw e;
	                    		}
                    		}
                    		maskImage2.calcMinMax();
                    		break;
	            		}
	            	} // if ((image.getNDims() >= 3) && (maskImage.getNDims() == 2))
	            	boolean wholeImage = true;
	            	if (maskImage2.getNDims() == 2) { 
	                    AlgorithmMorphology2D idObjectsAlgo2D;
	                    int method = AlgorithmMorphology2D.ID_OBJECTS;
	                    
	                    idObjectsAlgo2D = new AlgorithmMorphology2D(maskImage2, 0, 0, method, 0, 0, 0, 0, wholeImage);
	                    idObjectsAlgo2D.setMinMax(1, Integer.MAX_VALUE);
	                    idObjectsAlgo2D.run();
	                    idObjectsAlgo2D.finalize();
	                    idObjectsAlgo2D = null;
	            	}
	            	else { 
	                    AlgorithmMorphology3D idObjectsAlgo3D;
	                    int method = AlgorithmMorphology3D.ID_OBJECTS;
	                    
	                    idObjectsAlgo3D = new AlgorithmMorphology3D(maskImage2, 0, 0, method, 0, 0, 0, 0, wholeImage);
	                    idObjectsAlgo3D.setMinMax(1, Integer.MAX_VALUE);
	                    idObjectsAlgo3D.run();
	                    idObjectsAlgo3D.finalize();
	                    idObjectsAlgo3D = null;
	            	}
	            	maskImage2.calcMinMax();
	                final AlgorithmVOIExtraction VOIExtractionAlgo = new AlgorithmVOIExtraction(maskImage2);

	                // VOIExtractionAlgo.setActiveImage(false);
	                VOIExtractionAlgo.run();
	                
	                VOIVector kVOIs = maskImage2.getVOIs();
	                for (i = 0; i < kVOIs.size(); i++ )
	                {
	                    VOI kCurrentGroup = kVOIs.get(i);
	                    kCurrentGroup.setAllActive(true);
	                }
	                maskImage2.groupVOIs();
	                kVOIs = maskImage2.getVOIs();
	                if (voi2FieldName != null) {
	                    ((VOI)kVOIs.get(0)).setName(voi2FieldName);
	                }
	                image2.setVOIs(kVOIs);
	                maskImage2.disposeLocal();
	                maskImage2 = null;
            	} // convert maskImage2 to VOI
            }
            
            fireProgressStateChanged(100);
            
        } catch (OutOfMemoryError error) {

            if (image != null) {
                image.disposeLocal();
                image = null;
            }
            
            if (image2 != null) {
                image2.disposeLocal();
                image2 = null;
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
    
    /**
     * Writes a MATLAB format type image.
     * 
     * @param image Image model of data to write.
     * @param options options such as starting and ending slices and times
     * 
     * @exception IOException if there is an error writing the file
     */
    public void writeImage(final ModelImage image, final FileWriteOptions options) throws IOException {
    	boolean endianess;
    	byte descriptiveText[] = new byte[116];
    	byte endianIndicator[] = new byte[2];
    	int nDims;
    	int imageExtents[] = null;
    	int dataType;
    	int imageLength;
    	int realImageBytes;
    	int i;
    	int j;
    	int sBegin; // first z slice to write
        int sEnd; // last z slice to write
        int tBegin; // first t time to write
        int tEnd; // last t time to write
        int z, t;
        int zDim;
        byte arrayClass;
        int matDataType;
        boolean complexFlag;
        int arrayFlags;
        int dimensionsArrayBytes;
        byte arrayName[] = new byte[8];
        int sliceSize;
        int numberSlices;
        int count;
        byte[] byteBuffer = null;
        short[] shortBuffer;
        int[] intBuffer;
        long[] longBuffer = null;
        float[] floatBuffer = null;
        float[] floatIBuffer = null;
        int tmpInt;
        double[] doubleBuffer = null;
        double[] doubleIBuffer = null;
        byte[] tBuffer = null;
        long tmpLong;
        int padBytes;
        int matrixByteLength;
        int x;
        int y;
        int index;
        int dimsWritten;
        int offset;
        double maxValue;
        
        if (image.getType() == ModelStorageBase.BOOLEAN) {
        	Preferences.debug("BOOLEAN is not a legal MATLAB type - converting to UBYTE\n", Preferences.DEBUG_FILEIO);
        	AlgorithmChangeType changeTypeAlgo = new AlgorithmChangeType(image, ModelStorageBase.UBYTE, 0, 1, 0, 1,
                    false);
		    changeTypeAlgo.run();
            changeTypeAlgo.finalize();
            changeTypeAlgo = null;
        }
         
         if (image.getNDims() >= 3) {
             sBegin = options.getBeginSlice();
             sEnd = options.getEndSlice();
         } else {
             sBegin = 0;
             sEnd = 0;
         }

         if (image.getNDims() == 4) {
             tBegin = options.getBeginTime();
             tEnd = options.getEndTime();
         } else {
             tBegin = 0;
             tEnd = 0;
         }
    	
    	file = new File(fileDir + fileName);
        raFile = new RandomAccessFile(file, "rw");
        // Necessary so that if this is an overwritten file there isn't any
        // junk at the end
        raFile.setLength(0);
        
        endianess = FileBase.BIG_ENDIAN; // true
        // Bytes 0 to 115 contain the descriptive text
        descriptiveText[0] = 77; // M
        descriptiveText[1] = 73; // I
        descriptiveText[2] = 80; // P
        descriptiveText[3] = 65; // A
        descriptiveText[4] = 86; // V
        descriptiveText[5] = 32; // sp
        descriptiveText[6] = 105; // i
        descriptiveText[7] = 115; // s
        descriptiveText[8] = 32; // sp
        descriptiveText[9] = 119; // w
        descriptiveText[10] = 114; // r
        descriptiveText[11]= 105; // i
        descriptiveText[12] = 116; // t
        descriptiveText[13] = 105; // i
        descriptiveText[14] = 110; // n
        descriptiveText[15] = 103; // g
        descriptiveText[16] = 32; // sp
        descriptiveText[17] = 97; // a
        descriptiveText[18] = 32; // sp
        descriptiveText[19] = 46; // .
        descriptiveText[20] = 109; // m
        descriptiveText[21] = 97; // a
        descriptiveText[22] = 116; // t
        descriptiveText[23] = 32; // sp
        descriptiveText[24] = 102; // f
        descriptiveText[25] = 105; // i
        descriptiveText[26] = 108; // l
        descriptiveText[27] = 101; // e
        descriptiveText[28] = 46; // .
        raFile.write(descriptiveText);
        // Bytes 116 thru 123 contain an offset to subsystem-specific data in the .mat file.
        // All zeros or all spaces in this field indicate that there is no subsystem-specific
        // data stored in the file.
        writeLong(0L, endianess);
        // Write the version 256 at bytes 124-125
        writeShort((short)256, endianess);
        // Write the endian indicator at bytes 126-127
    	endianIndicator[0] = 77; // M
    	endianIndicator[1] = 73; // I
    	raFile.write(endianIndicator);
    	
    	nDims = image.getNDims();
    	imageExtents = image.getExtents();
    	dataType = image.getType();
    	imageLength = 1;
    	for (i = 0; i < nDims; i++) {
    		imageLength *= imageExtents[i];
    	}
    	
    	// Bytes 128-131 write miMATRIX
    	writeInt(miMATRIX, endianess);
    	// Bytes 132-135 write total number of following bytes
    	// Put in 0 for now as a place holder
    	writeInt(0, endianess);
    	// Write the data type of the array flags
    	writeInt(miUINT32, endianess);
    	// Write the number of bytes in the array flags
    	writeInt(8, endianess);
    	switch(dataType) {
    	case ModelStorageBase.BYTE:
    		arrayClass = mxINT8_CLASS;
    		matDataType = miINT8;
    		complexFlag = false;
    		realImageBytes = imageLength;
    		break;
    	case ModelStorageBase.UBYTE:
    		arrayClass = mxUINT8_CLASS;
    		matDataType = miUINT8;
    		complexFlag = false;
    		realImageBytes = imageLength;
    		break;
    	case ModelStorageBase.SHORT:
    		arrayClass = mxINT16_CLASS;
    		matDataType = miINT16;
    		complexFlag = false;
    		realImageBytes = 2 * imageLength;
    		break;
    	case ModelStorageBase.USHORT:
    		arrayClass = mxUINT16_CLASS;
    		matDataType = miUINT16;
    		complexFlag = false;
    		realImageBytes = 2 * imageLength;
    		break;
    	case ModelStorageBase.INTEGER:
    		arrayClass = mxINT32_CLASS;
    		matDataType = miINT32;
    		complexFlag = false;
    		realImageBytes = 4 * imageLength;
    	    break;
    	case ModelStorageBase.UINTEGER:
    		arrayClass = mxUINT32_CLASS;
    		matDataType = miUINT32;
    		complexFlag = false;
    		realImageBytes = 4 * imageLength;
    		break;
    	case ModelStorageBase.LONG:
    		arrayClass = mxINT64_CLASS;
    		matDataType = miINT64;
    		complexFlag = false;
    		realImageBytes = 8 * imageLength;
    		break;
    	case ModelStorageBase.FLOAT:
    		arrayClass = mxSINGLE_CLASS;
    		matDataType = miSINGLE;
    		complexFlag = false;
    		realImageBytes = 4 * imageLength;
    		break;
    	case ModelStorageBase.DOUBLE:
    		arrayClass = mxDOUBLE_CLASS;
    		matDataType = miDOUBLE;
    		complexFlag = false;
    		realImageBytes = 8 * imageLength;
    		break;
    	case ModelStorageBase.COMPLEX:
    		arrayClass = mxSINGLE_CLASS;
    		matDataType = miSINGLE;
    		complexFlag = true;
    		realImageBytes = 4 * imageLength;
    		break;
    	case ModelStorageBase.DCOMPLEX:
    		arrayClass = mxDOUBLE_CLASS;
    		matDataType = miDOUBLE;
    		complexFlag = true;
    		realImageBytes = 8 * imageLength;
    		break;
    	case ModelStorageBase.ARGB:
    		arrayClass = mxUINT8_CLASS;
    		matDataType = miUINT8;
    		complexFlag = false;
    		realImageBytes = 3 * imageLength;
    		break;
    	case ModelStorageBase.ARGB_USHORT:
    		arrayClass = mxUINT16_CLASS;
    		matDataType = miUINT16;
    		complexFlag = false;
    		realImageBytes = 6 * imageLength;
    		break;
    	case ModelStorageBase.ARGB_FLOAT:
    		arrayClass = mxDOUBLE_CLASS;
    		matDataType = miDOUBLE;
    		complexFlag = false;
    		realImageBytes = 24 * imageLength;
    		break;
    	default:
    		arrayClass = mxINT8_CLASS;
    		matDataType = miINT8;
    		complexFlag = false;
    		realImageBytes = imageLength;
    	}
    	
    	arrayFlags = 0x000000ff & arrayClass;
    	if (complexFlag) {
    	  arrayFlags = arrayFlags | 0x00000800;	
    	}
    	// write the arrayFlags
    	writeInt(arrayFlags, endianess);
    	// write the 4 undefined bytes following the array flags
    	writeInt(0, endianess);
    	// Write the data type of the dimensions array
    	writeInt(miINT32, endianess);
    	if (image.isColorImage()) {
    		dimensionsArrayBytes = 4 * (nDims + 1);
    	}
    	else {
    	    dimensionsArrayBytes = 4 * nDims;
    	}
    	// Write the number of bytes in the dimensions array
    	writeInt(dimensionsArrayBytes, endianess);
    	// Write the dimensions array
    	writeInt(imageExtents[1], endianess);
    	writeInt(imageExtents[0], endianess);
    	for (i = 2; i < nDims; i++) {
    		writeInt(imageExtents[i], endianess);
    	}
    	dimsWritten = nDims;
    	if (image.isColorImage()) {
    		writeInt(3, endianess);
    		dimsWritten++;
    	}
    	// Write 4 pad bytes if dimensions array bytes is not a multiple of 8
    	if ((dimsWritten == 3) || (dimsWritten == 5)) {
    		writeInt(0, endianess);
    	}
    	// Write the data type of the array name
    	writeInt(miINT8, endianess);
    	// Write the number of bytes in the array name
    	writeInt(8, endianess);
    	arrayName[0] = 109; // m
    	arrayName[1] = 97; // a
    	arrayName[2] = 116; // t
    	arrayName[3] = 65; // A
    	arrayName[4] = 114; // r
    	arrayName[5] = 114; // r
    	arrayName[6] = 97; // a
    	arrayName[7] = 121; // y
    	// Write the array name
    	raFile.write(arrayName);
    	// write the real data type
    	writeInt(matDataType, endianess);
    	// write the number of real data bytes
    	writeInt(realImageBytes, endianess);
    	
    	if (image.getNDims() == 2) {
            numberSlices = 1;
            zDim = 1;
        } else if (image.getNDims() == 3) {
            numberSlices = sEnd - sBegin + 1;
            zDim = imageExtents[2];
        } else {
            numberSlices = (sEnd - sBegin + 1) * (tEnd - tBegin + 1);
            zDim = imageExtents[2];
        }

        sliceSize = image.getSliceSize();

        count = 0;	
        
        switch(dataType) {
        case ModelStorageBase.BYTE:
        case ModelStorageBase.UBYTE:

            byteBuffer = new byte[sliceSize];
            tBuffer = new byte[byteBuffer.length];
            for (t = tBegin; t <= tEnd; t++) {

                for (z = sBegin; z <= sEnd; z++, count++) {
                    i = (t * zDim) + z;
                    fireProgressStateChanged(count * 100 / numberSlices);
                    image.exportSliceXY(i, byteBuffer);
                    j = 0;
                    for (x = 0; x < imageExtents[0]; x++) {
                    	for (y = 0; y < imageExtents[1]; y++) {
                    	    tBuffer[j++] = byteBuffer[x + imageExtents[0] * y];	
                    	}
                    }
                    raFile.write(tBuffer);
                } // for (z = sBegin; z <= sEnd; z++,count++)
            } // for (t = tBegin; t <= tEnd; t++)

            break;
            
        case ModelStorageBase.ARGB:
        	byteBuffer = new byte[sliceSize];
            tBuffer = new byte[byteBuffer.length];
            for (offset = 1; offset <= 3; offset++) {
	            for (t = tBegin; t <= tEnd; t++) {
	
	                for (z = sBegin; z <= sEnd; z++, count++) {
	                    i = (t * zDim) + z;
	                    fireProgressStateChanged(count * 100 / (3 * numberSlices));
	                    image.exportRGBData(offset, 4*i*sliceSize, sliceSize, byteBuffer);
	                    j = 0;
	                    for (x = 0; x < imageExtents[0]; x++) {
	                    	for (y = 0; y < imageExtents[1]; y++) {
	                    	    tBuffer[j++] = byteBuffer[x + imageExtents[0] * y];	
	                    	}
	                    }
	                    raFile.write(tBuffer);
	                } // for (z = sBegin; z <= sEnd; z++,count++)
	            } // for (t = tBegin; t <= tEnd; t++)
            } // for (offset = 1; offset <= 3; offset++)
        	break;

        case ModelStorageBase.SHORT:
        case ModelStorageBase.USHORT:

            shortBuffer = new short[sliceSize];
            byteBuffer = new byte[2 * sliceSize];
            for (t = tBegin; t <= tEnd; t++) {

                for (z = sBegin; z <= sEnd; z++, count++) {
                    i = (t * zDim) + z;
                    fireProgressStateChanged(count * 100 / numberSlices);
                    image.exportSliceXY(i, shortBuffer);
                    
                    j = 0;
                    for (x = 0; x < imageExtents[0]; x++) {
                    	for (y = 0; y < imageExtents[1]; y++, j++) {
                    		index = x + imageExtents[0] * y;
                    	    byteBuffer[2*j] = (byte)(shortBuffer[index] >>> 8);	
                    	    byteBuffer[(2*j) + 1] = (byte) (shortBuffer[index]);
                    	}
                    }

                    raFile.write(byteBuffer);
                } // for (z = sBegin; z <= sEnd; z++,count++)
            } // for (t = tBegin; t <= tEnd; t++)

            break;
            
        case ModelStorageBase.ARGB_USHORT:
        	shortBuffer = new short[sliceSize];
            byteBuffer = new byte[2 * sliceSize];
            for (offset = 1; offset <= 3; offset++) {
	            for (t = tBegin; t <= tEnd; t++) {
	
	                for (z = sBegin; z <= sEnd; z++, count++) {
	                    i = (t * zDim) + z;
	                    fireProgressStateChanged(count * 100 / (3 * numberSlices));
	                    image.exportRGBData(offset, 4*i*sliceSize, sliceSize, shortBuffer);
	                    
	                    j = 0;
	                    for (x = 0; x < imageExtents[0]; x++) {
	                    	for (y = 0; y < imageExtents[1]; y++, j++) {
	                    		index = x + imageExtents[0] * y;
	                    	    byteBuffer[2*j] = (byte)(shortBuffer[index] >>> 8);	
	                    	    byteBuffer[(2*j) + 1] = (byte) (shortBuffer[index]);
	                    	}
	                    }
	
	                    raFile.write(byteBuffer);
	                } // for (z = sBegin; z <= sEnd; z++,count++)
	            } // for (t = tBegin; t <= tEnd; t++)
            } // for (offset = 1; offset <= 3; offset++)
        	break;

        case ModelStorageBase.INTEGER:
        case ModelStorageBase.UINTEGER:

            // store as 32 bit signed integer
            intBuffer = new int[sliceSize];
            byteBuffer = new byte[4 * sliceSize];
            for (t = tBegin; t <= tEnd; t++) {

                for (z = sBegin; z <= sEnd; z++, count++) {
                    i = (t * zDim) + z;
                    fireProgressStateChanged(count * 100 / numberSlices);
                    image.exportSliceXY(i, intBuffer);
                    
                    j = 0;
                    for (x = 0; x < imageExtents[0]; x++) {
                    	for (y = 0; y < imageExtents[1]; y++, j++) {
                    		index = x + imageExtents[0] * y;
                    	    byteBuffer[4*j] = (byte)(intBuffer[index] >>> 24);	
                    	    byteBuffer[(4*j) + 1] = (byte) (intBuffer[index] >>> 16);
                    	    byteBuffer[(4*j) + 2] = (byte) (intBuffer[index] >>> 8);
                    	    byteBuffer[(4*j) + 3] = (byte) (intBuffer[index]);
                    	}
                    }

                    raFile.write(byteBuffer);
                } // for (z = sBegin; z <= sEnd; z++,count++)
            } // for (t = tBegin; t <= tEnd; t++)

            break;
            
        case ModelStorageBase.LONG:
        	
        	// store as 64 bit signed integer
            longBuffer = new long[sliceSize];
            byteBuffer = new byte[8 * sliceSize];
            for (t = tBegin; t <= tEnd; t++) {

                for (z = sBegin; z <= sEnd; z++, count++) {
                    i = (t * zDim) + z;
                    fireProgressStateChanged(count * 100 / numberSlices);
                    image.exportSliceXY(i, longBuffer);
		        	j = 0;
		        	for (x = 0; x < imageExtents[0]; x++) {
		            	for (y = 0; y < imageExtents[1]; y++, j++) {
		            		index = x + imageExtents[0] * y;
		            		tmpLong = Double.doubleToLongBits(longBuffer[index]);
		            		byteBuffer[8 * j] = (byte) (longBuffer[index] >>> 56);
		                    byteBuffer[ (8 * j) + 1] = (byte) (longBuffer[index] >>> 48);
		                    byteBuffer[ (8 * j) + 2] = (byte) (longBuffer[index] >>> 40);
		                    byteBuffer[ (8 * j) + 3] = (byte) (longBuffer[index] >>> 32);
		                    byteBuffer[ (8 * j) + 4] = (byte) (longBuffer[index] >>> 24);
		                    byteBuffer[ (8 * j) + 5] = (byte) (longBuffer[index] >>> 16);
		                    byteBuffer[ (8 * j) + 6] = (byte) (longBuffer[index] >>> 8);
		                    byteBuffer[ (8 * j) + 7] = (byte) (longBuffer[index]);
		            	}
		            }
            
		            raFile.write(byteBuffer);
                } // for (z = sBegin; z <= sEnd; z++,count++)
            } // for (t = tBegin; t <= tEnd; t++)
        	break;

        case ModelStorageBase.FLOAT:

            // store as 32 bit float
            floatBuffer = new float[sliceSize];
            byteBuffer = new byte[4 * sliceSize];
            for (t = tBegin; t <= tEnd; t++) {

                for (z = sBegin; z <= sEnd; z++, count++) {
                    i = (t * zDim) + z;
                    fireProgressStateChanged(count * 100 / numberSlices);
                    image.exportSliceXY(i, floatBuffer);
                    
                    j = 0;
                    for (x = 0; x < imageExtents[0]; x++) {
                    	for (y = 0; y < imageExtents[1]; y++, j++) {
                    		index = x + imageExtents[0] * y;
                    		tmpInt = Float.floatToIntBits(floatBuffer[index]);
                    	    byteBuffer[4*j] = (byte)(tmpInt >>> 24);	
                    	    byteBuffer[(4*j) + 1] = (byte) (tmpInt >>> 16);
                    	    byteBuffer[(4*j) + 2] = (byte) (tmpInt >>> 8);
                    	    byteBuffer[(4*j) + 3] = (byte) (tmpInt);
                    	}
                    }

                    raFile.write(byteBuffer);
                } // for (z = sBegin; z <= sEnd; z++,count++)
            } // for (t = tBegin; t <= tEnd; t++)

            break;

        case ModelStorageBase.DOUBLE:

            // store as 64 bit double precision float
            doubleBuffer = new double[sliceSize];
            byteBuffer = new byte[8 * sliceSize];
            for (t = tBegin; t <= tEnd; t++) {

                for (z = sBegin; z <= sEnd; z++, count++) {
                    i = (t * zDim) + z;
                    fireProgressStateChanged(count * 50 / numberSlices);
                    image.exportSliceXY(i, doubleBuffer);
                    
                    j = 0;
                    for (x = 0; x < imageExtents[0]; x++) {
                    	for (y = 0; y < imageExtents[1]; y++, j++) {
                    		index = x + imageExtents[0] * y;
                    		tmpLong = Double.doubleToLongBits(doubleBuffer[index]);
                    		byteBuffer[8 * j] = (byte) (tmpLong >>> 56);
                            byteBuffer[ (8 * j) + 1] = (byte) (tmpLong >>> 48);
                            byteBuffer[ (8 * j) + 2] = (byte) (tmpLong >>> 40);
                            byteBuffer[ (8 * j) + 3] = (byte) (tmpLong >>> 32);
                            byteBuffer[ (8 * j) + 4] = (byte) (tmpLong >>> 24);
                            byteBuffer[ (8 * j) + 5] = (byte) (tmpLong >>> 16);
                            byteBuffer[ (8 * j) + 6] = (byte) (tmpLong >>> 8);
                            byteBuffer[ (8 * j) + 7] = (byte) (tmpLong);
                    	}
                    }

                    raFile.write(byteBuffer);
                } // for (z = sBegin; z <= sEnd; z++,count++)
            } // for (t = tBegin; t <= tEnd; t++)

            break;
            
        case ModelStorageBase.ARGB_FLOAT:
        	floatBuffer = new float[sliceSize];
            byteBuffer = new byte[8 * sliceSize];
            maxValue = image.getMax();
            for (offset = 1; offset <= 3; offset++) {
	            for (t = tBegin; t <= tEnd; t++) {
	
	                for (z = sBegin; z <= sEnd; z++, count++) {
	                    i = (t * zDim) + z;
	                    fireProgressStateChanged(count * 50 / numberSlices);
	                    image.exportRGBData(offset, 4*i*sliceSize, sliceSize, floatBuffer);
	                    
	                    j = 0;
	                    for (x = 0; x < imageExtents[0]; x++) {
	                    	for (y = 0; y < imageExtents[1]; y++, j++) {
	                    		index = x + imageExtents[0] * y;
	                    		tmpLong = Double.doubleToLongBits(floatBuffer[index]/maxValue);
	                    		byteBuffer[8 * j] = (byte) (tmpLong >>> 56);
	                            byteBuffer[ (8 * j) + 1] = (byte) (tmpLong >>> 48);
	                            byteBuffer[ (8 * j) + 2] = (byte) (tmpLong >>> 40);
	                            byteBuffer[ (8 * j) + 3] = (byte) (tmpLong >>> 32);
	                            byteBuffer[ (8 * j) + 4] = (byte) (tmpLong >>> 24);
	                            byteBuffer[ (8 * j) + 5] = (byte) (tmpLong >>> 16);
	                            byteBuffer[ (8 * j) + 6] = (byte) (tmpLong >>> 8);
	                            byteBuffer[ (8 * j) + 7] = (byte) (tmpLong);
	                    	}
	                    }
	
	                    raFile.write(byteBuffer);
	                } // for (z = sBegin; z <= sEnd; z++,count++)
	            } // for (t = tBegin; t <= tEnd; t++)
            } // for (offset = 1; offset <= 3; offset++)

        	break;
            
        case ModelStorageBase.COMPLEX:

            // store as 32 bit float
            floatBuffer = new float[sliceSize];
            floatIBuffer = new float[sliceSize];
            byteBuffer = new byte[4 * sliceSize];
            for (t = tBegin; t <= tEnd; t++) {

                for (z = sBegin; z <= sEnd; z++, count++) {
                    i = (t * zDim) + z;
                    fireProgressStateChanged(count * 100 / numberSlices);
                    image.exportComplexData(2*i*sliceSize, sliceSize, floatBuffer, floatIBuffer);
                    
                    j = 0;
                    for (x = 0; x < imageExtents[0]; x++) {
                    	for (y = 0; y < imageExtents[1]; y++, j++) {
                    		index = x + imageExtents[0] * y;
                    		tmpInt = Float.floatToIntBits(floatBuffer[index]);
                    	    byteBuffer[4*j] = (byte)(tmpInt >>> 24);	
                    	    byteBuffer[(4*j) + 1] = (byte) (tmpInt >>> 16);
                    	    byteBuffer[(4*j) + 2] = (byte) (tmpInt >>> 8);
                    	    byteBuffer[(4*j) + 3] = (byte) (tmpInt);
                    	}
                    }

                    raFile.write(byteBuffer);
                } // for (z = sBegin; z <= sEnd; z++,count++)
            } // for (t = tBegin; t <= tEnd; t++)

            break;
            
        case ModelStorageBase.DCOMPLEX:

            // store as 64 bit double precision float
            doubleBuffer = new double[sliceSize];
            doubleIBuffer = new double[sliceSize];
            byteBuffer = new byte[8 * sliceSize];
            for (t = tBegin; t <= tEnd; t++) {

                for (z = sBegin; z <= sEnd; z++, count++) {
                    i = (t * zDim) + z;
                    fireProgressStateChanged(count * 50 / numberSlices);
                    image.exportDComplexData(2*i*sliceSize, sliceSize, doubleBuffer, doubleIBuffer);
                    
                    j = 0;
                    for (x = 0; x < imageExtents[0]; x++) {
                    	for (y = 0; y < imageExtents[1]; y++, j++) {
                    		index = x + imageExtents[0] * y;
                    		tmpLong = Double.doubleToLongBits(doubleBuffer[index]);
                    		byteBuffer[8 * j] = (byte) (tmpLong >>> 56);
                            byteBuffer[ (8 * j) + 1] = (byte) (tmpLong >>> 48);
                            byteBuffer[ (8 * j) + 2] = (byte) (tmpLong >>> 40);
                            byteBuffer[ (8 * j) + 3] = (byte) (tmpLong >>> 32);
                            byteBuffer[ (8 * j) + 4] = (byte) (tmpLong >>> 24);
                            byteBuffer[ (8 * j) + 5] = (byte) (tmpLong >>> 16);
                            byteBuffer[ (8 * j) + 6] = (byte) (tmpLong >>> 8);
                            byteBuffer[ (8 * j) + 7] = (byte) (tmpLong);
                    	}
                    }

                    raFile.write(byteBuffer);
                } // for (z = sBegin; z <= sEnd; z++,count++)
            } // for (t = tBegin; t <= tEnd; t++)

            break;
        }
        
        if ((raFile.getFilePointer() % 8) != 0) {
            padBytes = (int)(8 - (raFile.getFilePointer() % 8));
            for (i = 0; i < padBytes; i++) {
            	raFile.writeByte(0);
            }
        }
        
       if (complexFlag) {
    	// write the imaginary data type
       	writeInt(matDataType, endianess);
       	// write the number of imaginary data bytes = number of real data bytes
       	writeInt(realImageBytes, endianess);
    	   if (dataType == ModelStorageBase.COMPLEX) {
    		// store as 32 bit float
               for (t = tBegin; t <= tEnd; t++) {

                   for (z = sBegin; z <= sEnd; z++, count++) {
                       i = (t * zDim) + z;
                       fireProgressStateChanged(50 + (count * 50 / numberSlices));
                       image.exportComplexData(2*i*sliceSize, sliceSize, floatBuffer, floatIBuffer);
                       
                       j = 0;
                       for (x = 0; x < imageExtents[0]; x++) {
                       	   for (y = 0; y < imageExtents[1]; y++, j++) {
                       		   index = x + imageExtents[0] * y;
                       		   tmpInt = Float.floatToIntBits(floatIBuffer[index]);
                       	       byteBuffer[4*j] = (byte)(tmpInt >>> 24);	
                       	       byteBuffer[(4*j) + 1] = (byte) (tmpInt >>> 16);
                       	       byteBuffer[(4*j) + 2] = (byte) (tmpInt >>> 8);
                       	       byteBuffer[(4*j) + 3] = (byte) (tmpInt);
                       	   }
                       }

                       raFile.write(byteBuffer);
                   } // for (z = sBegin; z <= sEnd; z++,count++)
               } // for (t = tBegin; t <= tEnd; t++)
               
               if ((raFile.getFilePointer() % 8) != 0) {
                   padBytes = (int)(8 - (raFile.getFilePointer() % 8));
                   for (i = 0; i < padBytes; i++) {
                   	raFile.writeByte(0);
                   }
               }
   
    	   } // if (dataType == ModelStorageBase.COMPLEX)
    	   else { // dataType == ModelStorageBase.DCOMPLEX
    		   for (t = tBegin; t <= tEnd; t++) {

                   for (z = sBegin; z <= sEnd; z++, count++) {
                       i = (t * zDim) + z;
                       fireProgressStateChanged(50 + (count * 50 / numberSlices));
                       image.exportDComplexData(2*i*sliceSize, sliceSize, doubleBuffer, doubleIBuffer);
                       
                       j = 0;
                       for (x = 0; x < imageExtents[0]; x++) {
                       	   for (y = 0; y < imageExtents[1]; y++, j++) {
                       		   index = x + imageExtents[0] * y;
                       		   tmpLong = Double.doubleToLongBits(doubleIBuffer[index]);
                       		   byteBuffer[8 * j] = (byte) (tmpLong >>> 56);
                               byteBuffer[ (8 * j) + 1] = (byte) (tmpLong >>> 48);
                               byteBuffer[ (8 * j) + 2] = (byte) (tmpLong >>> 40);
                               byteBuffer[ (8 * j) + 3] = (byte) (tmpLong >>> 32);
                               byteBuffer[ (8 * j) + 4] = (byte) (tmpLong >>> 24);
                               byteBuffer[ (8 * j) + 5] = (byte) (tmpLong >>> 16);
                               byteBuffer[ (8 * j) + 6] = (byte) (tmpLong >>> 8);
                               byteBuffer[ (8 * j) + 7] = (byte) (tmpLong);
                       	   }
                       }

                       raFile.write(byteBuffer);
                   } // for (z = sBegin; z <= sEnd; z++,count++)
               } // for (t = tBegin; t <= tEnd; t++)   
    	   } // else dataType == ModelStorageBase.DCOMPLEX
       } // if (complexFlag)
        
       matrixByteLength = (int)(raFile.length() -  136);
       raFile.seek(132L);
       writeInt(matrixByteLength, endianess);
       raFile.close();
    }

    
}
