package gov.nih.mipav.model.file;


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
	
    

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** DOCUMENT ME! */
    private File file;

    /** DOCUMENT ME! */
    private String fileDir;


    /** DOCUMENT ME! */
    private FileInfoMATLAB fileInfo;
    
    private FileInfoMATLAB fileInfo2;

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
        int i, j, k;
        int xDim = 0;
        int yDim = 0;
        int imageSlices = 1;
        int imageSlices2 = 1;
        int byteCount;
        int totalByteCount = 0;
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
        byte buffer[];
        String str;
        boolean isCompressed = false;
        FileInputStream fis;
        int totalBytesRead = 0;
        long currentLocation;
        int s;
        File ufile = null;
        long bytesSkipped;
        byte[] decomp = null;
        int resultLength = 0;
        int arrayFlagsDataType;
        int arrayFlagsBytes;
        int arrayFlags;
        boolean complexFlag = false;
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
        int imageLength;
        int x;
        int y;
        int z;
        int t;
        long decompSize;
        String uncompressedName = null;
        int imagesFound = 0;

        try {
            
            imgResols[0] = imgResols[1] = imgResols[2] = imgResols[3] = imgResols[4] = (float) 1.0;
            file = new File(fileDir + fileName);
            raFile = new RandomAccessFile(file, "r");
            s = fileName.lastIndexOf(".");
            
            fileLength = raFile.length();
            Preferences.debug("fileLength = " + fileLength + "\n");
            raFile.seek(126L);
            firstEndianByte = raFile.readByte();
            secondEndianByte = raFile.readByte();
            if ((firstEndianByte == 77) && (secondEndianByte == 73)) {
            	// M followed by I
            	endianess = FileBase.BIG_ENDIAN;
            	Preferences.debug("The MATLAB file is big endian\n");
            }
            else if ((firstEndianByte == 73) && (secondEndianByte == 77)) {
            	// I followed by M
            	endianess = FileBase.LITTLE_ENDIAN;
            	Preferences.debug("The MATLAB file is little endian\n");
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
                 Preferences.debug("The MATLAB file uses level 4 format\n");
            }
            else {
            	// MATLAB uses level 5 format
            	level5Format = true;
            	Preferences.debug("The MATLAB file uses level 5 format\n");
            }
            
            fileInfo.setLevel5Format(level5Format);
            
            raFile.seek(0L);
            
            headerTextField = getString(116);
            Preferences.debug("Header text field = " + headerTextField.trim() + "\n");
            fileInfo.setHeaderTextField(headerTextField);
            
            // Location 116
            subsystemSpecificDataOffset = getLong(endianess);
            // All zeros or all spaces in this field indicate that there is no 
            // subsystem-specific data stored in this file
            if ((subsystemSpecificDataOffset == 0L) || (subsystemSpecificDataOffset == 0x2020202020202020L)) {
            	Preferences.debug("No subsystem specific data stored in file\n");
            }
            else {
            	Preferences.debug("Subystem specific data stored at location " + subsystemSpecificDataOffset + "\n");
            	fileInfo.setSubsystemSpecificDataOffset(subsystemSpecificDataOffset);
            }
            
            // Location 124
            version = getUnsignedShort(endianess);
            if (version == 256) {
                Preferences.debug("The version number is the expected 256\n");	
            }
            else {
            	Preferences.debug("The version number = " + version + " instead of the expected 256\n");
            }
            fileInfo.setVersion(version);
            
            // Go to first data element location
            nextElementAddress = 128L;
            while (nextElementAddress < fileLength) {
            	isCompressed = false;
                raFile.seek(nextElementAddress);
                dataType = getInt(endianess);
                if ((dataType & 0xffff0000) != 0) {
                	// Small Data Element Format
                	raFile.seek(nextElementAddress);
                	elementBytes = getUnsignedShort(endianess);
                	dataType = getUnsignedShort(endianess);
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
                    Preferences.debug("padBytes = " + padBytes + "\n");
                    if (dataType == miCOMPRESSED) {
                        nextElementAddress = nextElementAddress + elementBytes + 8;
                    }
                    else {
                        nextElementAddress = nextElementAddress + elementBytes + padBytes + 8;
                    }
                }
                Preferences.debug("nextElementAddress = " + nextElementAddress + "\n");
                if (dataType == miCOMPRESSED) {
                	isCompressed = true;
                	Preferences.debug("Data type = miCOMPRESSED\n");
                	Preferences.debug("Bytes in data element = " + elementBytes + "\n");
                	buffer = new byte[elementBytes];
                	raFile.read(buffer);
                	zlibDecompresser = new Inflater();
                	zlibDecompresser.setInput(buffer, 0, elementBytes);
                	try {
                		decompSize = 1000L * elementBytes;
                		if (decompSize < Integer.MAX_VALUE) {
                			decomp =  new byte[1000 * elementBytes];
                		}
                		else {
                			decomp = new byte[Integer.MAX_VALUE];
                		}
                        resultLength = zlibDecompresser.inflate(decomp);
                        Preferences.debug("Decompressed length = " + resultLength + "\n");
                    }
                    catch (DataFormatException e){
                        MipavUtil.displayError("DataFormatException on zlibDecompresser.inflate(decomp)");  
                    }
                    zlibDecompresser.reset();
                    uncompressedName = fileDir + fileName.substring(0, s) + "uncompressed.mat";
                    ufile = new File(uncompressedName);
                    raFile = new RandomAccessFile(ufile, "rw");
                    raFile.setLength(0);
                    raFile.write(decomp, 0, resultLength);
                    decomp = null;
                    raFile.seek(0L);
                    dataType = getInt(endianess);
                    if ((dataType & 0xffff0000) != 0) {
                    	// Small Data Element Format
                    	raFile.seek(0L);
                    	elementBytes = getUnsignedShort(endianess);
                    	dataType = getUnsignedShort(endianess);
                    }
                    else {
                        elementBytes = getInt(endianess);
                    }
                } // if (dataTypeCOMPRESSED)
                switch(dataType) {
                case miINT8:
                	Preferences.debug("Data type = miINT8\n");
                	Preferences.debug("Bytes in data element = " + elementBytes + "\n");
                	break;
                case miUINT8:
                	Preferences.debug("Data type = miUINT8\n");
                	Preferences.debug("Bytes in data element = " + elementBytes + "\n");
                	break;
                case miINT16:
                	Preferences.debug("Data type = miINT16\n");
                	Preferences.debug("Bytes in data element = " + elementBytes + "\n");
                	break;
                case miUINT16:
                	Preferences.debug("Data type = miUINT16\n");
                	Preferences.debug("Bytes in data element = " + elementBytes + "\n");
                	break;
                case miINT32:
                	Preferences.debug("Data type = miINT32\n");
                	Preferences.debug("Bytes in data element = " + elementBytes + "\n");
                	break;
                case miUINT32:
                	Preferences.debug("Data type = miUINT32\n");
                	Preferences.debug("Bytes in data element = " + elementBytes + "\n");
                	break;
                case miSINGLE:
                	Preferences.debug("Data type = miSINGLE\n");
                	Preferences.debug("Bytes in data element = " + elementBytes + "\n");
                	break;
                case miDOUBLE:
                	Preferences.debug("Data type = miDOUBLE\n");
                	Preferences.debug("Bytes in data element = " + elementBytes + "\n");
                	break;
                case miINT64:
                	Preferences.debug("Data type = miINT64\n");
                	Preferences.debug("Bytes in data element = " + elementBytes + "\n");
                	break;
                case miUINT64:
                	Preferences.debug("Data type = miUINT64\n");
                	Preferences.debug("Bytes in data element = " + elementBytes + "\n");
                	break;
                case miMATRIX:
                	Preferences.debug("Data type = miMATRIX\n");
                	Preferences.debug("Bytes in data element = " + elementBytes + "\n");
                	imagesFound++;
                	arrayFlagsDataType = getInt(endianess);
                	if (arrayFlagsDataType == miUINT32) {
                		Preferences.debug("Array flags data type is the expected miUINT32\n");
                	}
                	else {
                		Preferences.debug("Array flags data type is an unexpected " + arrayFlagsDataType + "\n");
                	}
                    arrayFlagsBytes = getInt(endianess);
                    if (arrayFlagsBytes == 8) {
                    	Preferences.debug("Array flags byte length = 8 as expected\n");
                    }
                    else {
                    	Preferences.debug("Array flags byte length is an unexpected " + arrayFlagsBytes + "\n");
                    }
                    arrayFlags = getInt(endianess);
                    arrayClass = arrayFlags & 0x000000ff;
                    switch(arrayClass) {
                    case mxCELL_CLASS:
                    	Preferences.debug("Array type is cell array\n");
                    	break;
                    case mxSTRUCT_CLASS:
                    	Preferences.debug("Array type is structure\n");
                    	break;
                    case mxOBJECT_CLASS:
                    	Preferences.debug("Array type is object\n");
                    	break;
                    case mxCHAR_CLASS:
                    	Preferences.debug("Array type is character\n");
                    	break;
                    case mxSPARSE_CLASS:
                    	Preferences.debug("Array type is sparse\n");
                    	break;
                    case mxDOUBLE_CLASS:
                    	Preferences.debug("Array type is 8 byte float\n");
                    	break;
                    case mxSINGLE_CLASS:
                    	Preferences.debug("Array type is 4 byte float\n");
                    	break;
                    case mxINT8_CLASS:
                    	Preferences.debug("Array type is signed byte\n");
                    	break;
                    case mxUINT8_CLASS:
                    	Preferences.debug("Array type is unsigned byte\n");
                    	break;
                    case mxINT16_CLASS:
                    	Preferences.debug("Array type is signed short\n");
                    	break;
                    case mxUINT16_CLASS:
                    	Preferences.debug("Array type is unsigned short\n");
                    	break;
                    case mxINT32_CLASS:
                        Preferences.debug("Array type is signed integer\n");
                        break;
                    case mxUINT32_CLASS:
                    	Preferences.debug("Array type is unsigned integer\n");
                    	break;
                    default:
                    	Preferences.debug("Array type is an illegal = " + arrayClass + "\n");
                    }
                    if (arrayClass == mxCHAR_CLASS) {
                    	continue;
                    }
                    if ((arrayFlags & 0x00000800) != 0) {
                    	complexFlag = true;
                    	Preferences.debug("Complex flag is set\n");
                    }
                    else {
                    	complexFlag = false;
                    	Preferences.debug("Complex flag is not set\n");
                    }
                    if ((arrayFlags & 0x00000400) != 0) {
                    	globalFlag = true;
                    	Preferences.debug("Global flag is set\n");
                    }
                    else {
                    	globalFlag = false;
                    	Preferences.debug("Global flag is not set\n");
                    }
                    if ((arrayFlags & 0x00000200) != 0) {
                    	logicalFlag = true;
                    	Preferences.debug("Logical flag is set\n");
                    }
                    else {
                    	logicalFlag = false;
                    	Preferences.debug("Logical flag is not set\n");
                    }
                    // 4 undefined bytes
                	getInt(endianess);
                	dimensionsArrayDataType = getInt(endianess);
                	if (dimensionsArrayDataType == miINT32) {
                		Preferences.debug("Dimensions array data type is the expected miINT32\n");
                	}
                	else {
                		Preferences.debug("Dimensions array data type is an unexpected " + dimensionsArrayDataType + "\n");
                	}
                	dimensionsArrayBytes = getInt(endianess);
                	Preferences.debug("dimensionsArrayBytes = " + dimensionsArrayBytes + "\n");
                	if ((dimensionsArrayBytes % 4) == 0) {
                		Preferences.debug("dimensionsArrayBytes is a multiple of 4 as expected\n");
                	}
                	else {
                		Preferences.debug("dimensionArrayBytes is unexpectedly not a multiple of 4\n");
                	}
                	nDim = dimensionsArrayBytes/4;
                	Preferences.debug("Number of dimensions = " + nDim + "\n");
                	if (nDim < 2) {
                		Preferences.debug("Error! All numeric arrays should have at least 2 dimensions\n");
                	}
                	imageExtents = new int[nDim];
                	imageLength = 1;
                	if (imagesFound == 1) {
                		imageSlices = 1;
                	}
                	else {
                		imageSlices2 = 1;
                	}
                	for (i = 0; i < nDim; i++) {
                		imageExtents[i] = getInt(endianess);
                		imageLength = imageLength * imageExtents[i];
                		Preferences.debug("imageExtents["+ i + "] = " + imageExtents[i] + "\n");
                		if (i > 1) {
                			if (imagesFound == 1) {
                				imageSlices = imageSlices * imageExtents[i];
                			}
                			else {
                				imageSlices2 = imageSlices2 * imageExtents[i];
                			}
                		}
                	}
                	if ((dimensionsArrayBytes % 8) != 0) {
                		// Skip over padding bytes
                		padBytes = 8 - (dimensionsArrayBytes % 8);
                		for (i = 0; i < padBytes; i++) {
                		    raFile.readByte();
                		}
                	} // if ((dimensionsArrayBytes % 8) != 0)
                	currentLocation = raFile.getFilePointer();
                	arrayNameDataType = getInt(endianess);
                    if ((arrayNameDataType & 0xffff0000) != 0) {
                        // Small data element format    
                    	raFile.seek(currentLocation);
                    	arrayNameBytes = getUnsignedShort(endianess);
                    	arrayNameDataType = getUnsignedShort(endianess);
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
                		padBytes = 8 - (arrayNameBytes % 8);
                		for (i = 0; i < padBytes; i++) {
                		    raFile.readByte();
                		}
                    }
                    Preferences.debug("Array name = " + arrayName + "\n");
                    if (imagesFound == 1) {
                	    fileInfo.setExtents(imageExtents);
                	    fileInfo.setArrayName(arrayName);
                	}
                	else {
                		fileInfo2 = new FileInfoMATLAB(fileName + "_2", fileDir, FileUtility.MATLAB); // dummy fileInfo
                        fileInfo2.setEndianess(endianess);
                        fileInfo2.setLevel5Format(level5Format);
                        fileInfo2.setHeaderTextField(headerTextField);
                        fileInfo2.setVersion(version);
                        fileInfo2.setExtents(imageExtents);
                        fileInfo2.setArrayName(arrayName);
                	}
                    realDataType = getInt(endianess);
                    realDataBytes = getInt(endianess);
                    if (imagesFound == 1) {
                    switch(realDataType) {
                    case miINT8:
                    	Preferences.debug("Real data type = miINT8\n");
                    	Preferences.debug("Real data bytes = " + realDataBytes + "\n");
                    	if (!complexFlag) {
	                    	image = new ModelImage(ModelStorageBase.BYTE, imageExtents, fileName); 
	                    	fileInfo.setDataType(ModelStorageBase.BYTE);
	                    	buffer = new byte[imageLength];
	                    	raFile.read(buffer);
	                    	try {
	                			image.importData(0, buffer, true);
	                		}
	                		catch(IOException e) {
	                		   MipavUtil.displayError("IOException on image.importData(0, buffer, true)");
	                		   throw e;
	                		}
                    	}
                    	else {
                    		image = new ModelImage(ModelStorageBase.COMPLEX, imageExtents, fileName); 
                    		fileInfo.setDataType(ModelStorageBase.COMPLEX);
                    		buffer = new byte[imageLength];
                    		raFile.read(buffer);
                    		realBuffer = new float[imageLength];
                    		for (i = 0; i < imageLength; i++) {
                    		    realBuffer[i] = (float)(buffer[i]);
                    		}
                    		if ((realDataBytes % 8) != 0) {
                    	    	padBytes = 8 - (realDataBytes % 8);
                    	    	for (i = 0; i < padBytes; i++) {
                        	    	raFile.readByte();
                        	    }
                    	    }  
                    	    imaginaryDataType = getInt(endianess);
                    	    if (imaginaryDataType == miINT8) {
                    	    	Preferences.debug("imaginaryDataType == miINT8 as expected\n");
                    	    }
                    	    else {
                    	    	Preferences.debug("imaginaryDataType unexpectedly == " + imaginaryDataType + "\n");
                    	    }
                            imaginaryDataBytes = getInt(endianess);
                            if (imaginaryDataBytes == realDataBytes) {
                            	Preferences.debug("imaginaryDataByts == realDataBytes as expected\n");
                            }
                            else {
                            	Preferences.debug("imaginaryDataBytes unexpectedly != realDataBytes\n");
                            }
                            raFile.read(buffer);
                            imaginaryBuffer = new float[imageLength];
                            for (i = 0; i < imageLength; i++) {
                            	imaginaryBuffer[i] = (float)(buffer[i]);
                            }
                            boolean logMagDisplay = true;
                            try {
                    			image.importComplexData(0, realBuffer, imaginaryBuffer, true, logMagDisplay);
                    		}
                    		catch(IOException e) {
                    		   MipavUtil.displayError("IOException on image.importComplexData(0, realBuffer, imaginaryBuffer, true, logMagDisplay)");
                    		   throw e;
                    		}		
                    	}
                    	break;
                    case miUINT8:
                    	Preferences.debug("Real data type = miUINT8\n");
                    	Preferences.debug("Real data bytes = " + realDataBytes + "\n");
                    	if (!complexFlag) {
	                    	image = new ModelImage(ModelStorageBase.UBYTE, imageExtents, fileName);
	                    	fileInfo.setDataType(ModelStorageBase.UBYTE);
	                    	buffer = new byte[imageLength];
	                    	raFile.read(buffer);
	                    	shortBuffer = new short[imageLength];
	                    	for (i = 0; i < imageLength; i++) {
	                		    shortBuffer[i] = (short)(buffer[i] & 0xff);
	                		}
	                    	try {
	                			image.importUData(0, shortBuffer, true);
	                		}
	                		catch(IOException e) {
	                		   MipavUtil.displayError("IOException on image.importData(0, shortBuffer, true)");
	                		   throw e;
	                		}
                    	}
                    	else {
                    		image = new ModelImage(ModelStorageBase.COMPLEX, imageExtents, fileName);
                    		fileInfo.setDataType(ModelStorageBase.COMPLEX);
                    		buffer = new byte[imageLength];
                    		raFile.read(buffer);
                    		realBuffer = new float[imageLength];
                    		for (i = 0; i < imageLength; i++) {
                    		    realBuffer[i] = (float)(buffer[i] & 0xff);
                    		}
                    		if ((realDataBytes % 8) != 0) {
                    	    	padBytes = 8 - (realDataBytes % 8);
                    	    	for (i = 0; i < padBytes; i++) {
                        	    	raFile.readByte();
                        	    }
                    	    }  
                    	    imaginaryDataType = getInt(endianess);
                    	    if (imaginaryDataType == miUINT8) {
                    	    	Preferences.debug("imaginaryDataType == miUINT8 as expected\n");
                    	    }
                    	    else {
                    	    	Preferences.debug("imaginaryDataType unexpectedly == " + imaginaryDataType + "\n");
                    	    }
                            imaginaryDataBytes = getInt(endianess);
                            if (imaginaryDataBytes == realDataBytes) {
                            	Preferences.debug("imaginaryDataByts == realDataBytes as expected\n");
                            }
                            else {
                            	Preferences.debug("imaginaryDataBytes unexpectedly != realDataBytes\n");
                            }
                            raFile.read(buffer);
                            imaginaryBuffer = new float[imageLength];
                            for (i = 0; i < imageLength; i++) {
                            	imaginaryBuffer[i] = (float)(buffer[i] & 0xff);
                            }
                            boolean logMagDisplay = true;
                            try {
                    			image.importComplexData(0, realBuffer, imaginaryBuffer, true, logMagDisplay);
                    		}
                    		catch(IOException e) {
                    		   MipavUtil.displayError("IOException on image.importComplexData(0, realBuffer, imaginaryBuffer, true, logMagDisplay)");
                    		   throw e;
                    		}	
                    	}
                    	break;
                    case miINT16:
                    	Preferences.debug("Real data type = miINT16\n");
                    	Preferences.debug("Real data bytes = " + realDataBytes + "\n");
                    	if (!complexFlag) {
                    		image = new ModelImage(ModelStorageBase.SHORT, imageExtents, fileName); 
                    		fileInfo.setDataType(ModelStorageBase.SHORT);
                    		shortBuffer = new short[imageLength];
                    		for (i = 0; i < imageLength; i++) {
                    		    shortBuffer[i] = readShort(endianess);
                    		}
                    		try {
                    			image.importData(0, shortBuffer, true);
                    		}
                    		catch(IOException e) {
                    		   MipavUtil.displayError("IOException on image.importData(0, shortBuffer, true)");
                    		   throw e;
                    		}
                    	}
                    	else {
                    		image = new ModelImage(ModelStorageBase.COMPLEX, imageExtents, fileName); 
                    		fileInfo.setDataType(ModelStorageBase.COMPLEX);
                    		realBuffer = new float[imageLength];
                    		for (i = 0; i < imageLength; i++) {
                    		    realBuffer[i] = (float)(readShort(endianess));
                    		}
                    		if ((realDataBytes % 8) != 0) {
                    	    	padBytes = 8 - (realDataBytes % 8);
                    	    	for (i = 0; i < padBytes; i++) {
                        	    	raFile.readByte();
                        	    }
                    	    }  
                    	    imaginaryDataType = getInt(endianess);
                    	    if (imaginaryDataType == miINT16) {
                    	    	Preferences.debug("imaginaryDataType == miINT16 as expected\n");
                    	    }
                    	    else {
                    	    	Preferences.debug("imaginaryDataType unexpectedly == " + imaginaryDataType + "\n");
                    	    }
                            imaginaryDataBytes = getInt(endianess);
                            if (imaginaryDataBytes == realDataBytes) {
                            	Preferences.debug("imaginaryDataByts == realDataBytes as expected\n");
                            }
                            else {
                            	Preferences.debug("imaginaryDataBytes unexpectedly != realDataBytes\n");
                            }
                            imaginaryBuffer = new float[imageLength];
                            for (i = 0; i < imageLength; i++) {
                            	imaginaryBuffer[i] = (float)(readShort(endianess));
                            }
                            boolean logMagDisplay = true;
                            try {
                    			image.importComplexData(0, realBuffer, imaginaryBuffer, true, logMagDisplay);
                    		}
                    		catch(IOException e) {
                    		   MipavUtil.displayError("IOException on image.importComplexData(0, realBuffer, imaginaryBuffer, true, logMagDisplay)");
                    		   throw e;
                    		}		
                    	}
                    	break;
                    case miUINT16:
                    	Preferences.debug("Real data type = miUINT16\n");
                    	Preferences.debug("Real data bytes = " + realDataBytes + "\n");
                    	if (!complexFlag) {
                    		image = new ModelImage(ModelStorageBase.USHORT, imageExtents, fileName);
                    		fileInfo.setDataType(ModelStorageBase.USHORT);
                    		intBuffer = new int[imageLength];
                    		for (i = 0; i < imageLength; i++) {
                    		    intBuffer[i] = getUnsignedShort(endianess);
                    		}
                    		try {
                    			image.importUData(0, intBuffer, true);
                    		}
                    		catch(IOException e) {
                    		   MipavUtil.displayError("IOException on image.importUData(0, intBuffer, true)");
                    		   throw e;
                    		}
                    	}
                    	else {
                    		image = new ModelImage(ModelStorageBase.COMPLEX, imageExtents, fileName); 
                    		fileInfo.setDataType(ModelStorageBase.COMPLEX);
                    		realBuffer = new float[imageLength];
                    		for (i = 0; i < imageLength; i++) {
                    		    realBuffer[i] = (float)(getUnsignedShort(endianess));
                    		}
                    		if ((realDataBytes % 8) != 0) {
                    	    	padBytes = 8 - (realDataBytes % 8);
                    	    	for (i = 0; i < padBytes; i++) {
                        	    	raFile.readByte();
                        	    }
                    	    }  
                    	    imaginaryDataType = getInt(endianess);
                    	    if (imaginaryDataType == miUINT16) {
                    	    	Preferences.debug("imaginaryDataType == miUINT16 as expected\n");
                    	    }
                    	    else {
                    	    	Preferences.debug("imaginaryDataType unexpectedly == " + imaginaryDataType + "\n");
                    	    }
                            imaginaryDataBytes = getInt(endianess);
                            if (imaginaryDataBytes == realDataBytes) {
                            	Preferences.debug("imaginaryDataByts == realDataBytes as expected\n");
                            }
                            else {
                            	Preferences.debug("imaginaryDataBytes unexpectedly != realDataBytes\n");
                            }
                            imaginaryBuffer = new float[imageLength];
                            for (i = 0; i < imageLength; i++) {
                            	imaginaryBuffer[i] = (float)(getUnsignedShort(endianess));
                            }
                            boolean logMagDisplay = true;
                            try {
                    			image.importComplexData(0, realBuffer, imaginaryBuffer, true, logMagDisplay);
                    		}
                    		catch(IOException e) {
                    		   MipavUtil.displayError("IOException on image.importComplexData(0, realBuffer, imaginaryBuffer, true, logMagDisplay)");
                    		   throw e;
                    		}		
                    	}
                    	break;
                    case miINT32:
                    	Preferences.debug("Real data type = miINT32\n");
                    	Preferences.debug("Real data bytes = " + realDataBytes + "\n");
                    	if (!complexFlag) {
                    		image = new ModelImage(ModelStorageBase.INTEGER, imageExtents, fileName);
                    		fileInfo.setDataType(ModelStorageBase.INTEGER);
                    		intBuffer = new int[imageLength];
                    		for (i = 0; i < imageLength; i++) {
                    		    intBuffer[i] = getInt(endianess);
                    		}
                    		try {
                    			image.importData(0, intBuffer, true);
                    		}
                    		catch(IOException e) {
                    		   MipavUtil.displayError("IOException on image.importData(0, intBuffer, true)");
                    		   throw e;
                    		}
                    	}
                    	else {
                    		image = new ModelImage(ModelStorageBase.DCOMPLEX, imageExtents, fileName); 
                    		fileInfo.setDataType(ModelStorageBase.DCOMPLEX);
                    		realDBuffer = new double[imageLength];
                    		for (i = 0; i < imageLength; i++) {
                    		    realDBuffer[i] = (double)getInt(endianess);
                    		}
                    	    imaginaryDataType = getInt(endianess);
                    	    if (imaginaryDataType == miINT32) {
                    	    	Preferences.debug("imaginaryDataType == miINT32 as expected\n");
                    	    }
                    	    else {
                    	    	Preferences.debug("imaginaryDataType unexpectedly == " + imaginaryDataType + "\n");
                    	    }
                    	    if ((realDataBytes % 8) != 0) {
                    	    	padBytes = 8 - (realDataBytes % 8);
                    	    	for (i = 0; i < padBytes; i++) {
                        	    	raFile.readByte();
                        	    }
                    	    }  
                            imaginaryDataBytes = getInt(endianess);
                            if (imaginaryDataBytes == realDataBytes) {
                            	Preferences.debug("imaginaryDataByts == realDataBytes as expected\n");
                            }
                            else {
                            	Preferences.debug("imaginaryDataBytes unexpectedly != realDataBytes\n");
                            }
                            imaginaryDBuffer = new double[imageLength];
                            for (i = 0; i < imageLength; i++) {
                            	imaginaryDBuffer[i] = (double)getInt(endianess);
                            }
                            boolean logMagDisplay = true;
                            try {
                    			image.importDComplexData(0, realDBuffer, imaginaryDBuffer, true, logMagDisplay);
                    		}
                    		catch(IOException e) {
                    		   MipavUtil.displayError("IOException on image.importDComplexData(0, realDBuffer, imaginaryDBuffer, true, logMagDisplay)");
                    		   throw e;
                    		}
                    	}
                    	break;
                    case miUINT32:
                    	Preferences.debug("Real data type = miUINT32\n");
                    	Preferences.debug("Real data bytes = " + realDataBytes + "\n");
                    	if (!complexFlag) {
                    		image = new ModelImage(ModelStorageBase.UINTEGER, imageExtents, fileName); 
                    		fileInfo.setDataType(ModelStorageBase.UINTEGER);
                    		longBuffer = new long[imageLength];
                    		for (i = 0; i < imageLength; i++) {
                    		    longBuffer[i] = getUInt(endianess);
                    		}
                    		try {
                    			image.importUData(0, longBuffer, true);
                    		}
                    		catch(IOException e) {
                    		   MipavUtil.displayError("IOException on image.importUData(0, longBuffer, true)");
                    		   throw e;
                    		}
                    	}
                    	else {
                    		image = new ModelImage(ModelStorageBase.DCOMPLEX, imageExtents, fileName); 
                    		fileInfo.setDataType(ModelStorageBase.DCOMPLEX);
                    		realDBuffer = new double[imageLength];
                    		for (i = 0; i < imageLength; i++) {
                    		    realDBuffer[i] = (double)getUInt(endianess);
                    		}
                    	    imaginaryDataType = getInt(endianess);
                    	    if (imaginaryDataType == miUINT32) {
                    	    	Preferences.debug("imaginaryDataType == miUINT32 as expected\n");
                    	    }
                    	    else {
                    	    	Preferences.debug("imaginaryDataType unexpectedly == " + imaginaryDataType + "\n");
                    	    }
                    	    if ((realDataBytes % 8) != 0) {
                    	    	padBytes = 8 - (realDataBytes % 8);
                    	    	for (i = 0; i < padBytes; i++) {
                        	    	raFile.readByte();
                        	    }
                    	    }  
                            imaginaryDataBytes = getInt(endianess);
                            if (imaginaryDataBytes == realDataBytes) {
                            	Preferences.debug("imaginaryDataByts == realDataBytes as expected\n");
                            }
                            else {
                            	Preferences.debug("imaginaryDataBytes unexpectedly != realDataBytes\n");
                            }
                            imaginaryDBuffer = new double[imageLength];
                            for (i = 0; i < imageLength; i++) {
                            	imaginaryDBuffer[i] = (double)getUInt(endianess);
                            }
                            boolean logMagDisplay = true;
                            try {
                    			image.importDComplexData(0, realDBuffer, imaginaryDBuffer, true, logMagDisplay);
                    		}
                    		catch(IOException e) {
                    		   MipavUtil.displayError("IOException on image.importDComplexData(0, realDBuffer, imaginaryDBuffer, true, logMagDisplay)");
                    		   throw e;
                    		}
                    	}
                    	break;
                    case miSINGLE:
                    	Preferences.debug("Real data type = miSINGLE\n");
                    	Preferences.debug("Real data bytes = " + realDataBytes + "\n");
                    	if (!complexFlag) {
                    		image = new ModelImage(ModelStorageBase.FLOAT, imageExtents, fileName); 
                    		fileInfo.setDataType(ModelStorageBase.FLOAT);
                    		floatBuffer = new float[imageLength];
                    		for (i = 0; i < imageLength; i++) {
                    		    floatBuffer[i] = getFloat(endianess);
                    		}
                    		try {
                    			image.importData(0, floatBuffer, true);
                    		}
                    		catch(IOException e) {
                    		   MipavUtil.displayError("IOException on image.importData(0, floatBuffer, true)");
                    		   throw e;
                    		}
                    	}
                    	else {
                    		image = new ModelImage(ModelStorageBase.COMPLEX, imageExtents, fileName); 
                    		fileInfo.setDataType(ModelStorageBase.COMPLEX);
                    		realBuffer = new float[imageLength];
                    		for (i = 0; i < imageLength; i++) {
                    		    realBuffer[i] = getFloat(endianess);
                    		}
                    	    imaginaryDataType = getInt(endianess);
                    	    if (imaginaryDataType == miSINGLE) {
                    	    	Preferences.debug("imaginaryDataType == miSINGLE as expected\n");
                    	    }
                    	    else {
                    	    	Preferences.debug("imaginaryDataType unexpectedly == " + imaginaryDataType + "\n");
                    	    }
                    	    if ((realDataBytes % 8) != 0) {
                    	    	padBytes = 8 - (realDataBytes % 8);
                    	    	for (i = 0; i < padBytes; i++) {
                        	    	raFile.readByte();
                        	    }
                    	    }  
                            imaginaryDataBytes = getInt(endianess);
                            if (imaginaryDataBytes == realDataBytes) {
                            	Preferences.debug("imaginaryDataByts == realDataBytes as expected\n");
                            }
                            else {
                            	Preferences.debug("imaginaryDataBytes unexpectedly != realDataBytes\n");
                            }
                            imaginaryBuffer = new float[imageLength];
                            for (i = 0; i < imageLength; i++) {
                            	imaginaryBuffer[i] = getFloat(endianess);
                            }
                            boolean logMagDisplay = true;
                            try {
                    			image.importComplexData(0, realBuffer, imaginaryBuffer, true, logMagDisplay);
                    		}
                    		catch(IOException e) {
                    		   MipavUtil.displayError("IOException on image.importComplexData(0, realBuffer, imaginaryBuffer, true, logMagDisplay)");
                    		   throw e;
                    		}
                    	}
                    	break;
                    case miDOUBLE:
                    	Preferences.debug("Real data type = miDOUBLE\n");
                    	Preferences.debug("Real data bytes = " + realDataBytes + "\n");
                    	if (!complexFlag) {
                    		image = new ModelImage(ModelStorageBase.DOUBLE, imageExtents, fileName); 
                    		fileInfo.setDataType(ModelStorageBase.DOUBLE);
                    		doubleBuffer = new double[imageLength];
                    		for (i = 0; i < imageLength; i++) {
                    		    doubleBuffer[i] = getDouble(endianess);
                    		}
                    		try {
                    			image.importData(0, doubleBuffer, true);
                    		}
                    		catch(IOException e) {
                    		   MipavUtil.displayError("IOException on image.importData(0, doubleBuffer, true)");
                    		   throw e;
                    		}
                    	}
                    	else {
                    		image = new ModelImage(ModelStorageBase.DCOMPLEX, imageExtents, fileName); 
                    		fileInfo.setDataType(ModelStorageBase.DCOMPLEX);
                    		realDBuffer = new double[imageLength];
                    		for (i = 0; i < imageLength; i++) {
                    		    realDBuffer[i] = getDouble(endianess);
                    		}
                    	    imaginaryDataType = getInt(endianess);
                    	    if (imaginaryDataType == miDOUBLE) {
                    	    	Preferences.debug("imaginaryDataType == miDOUBLE as expected\n");
                    	    }
                    	    else {
                    	    	Preferences.debug("imaginaryDataType unexpectedly == " + imaginaryDataType + "\n");
                    	    }
                    	    if ((realDataBytes % 8) != 0) {
                    	    	padBytes = 8 - (realDataBytes % 8);
                    	    	for (i = 0; i < padBytes; i++) {
                        	    	raFile.readByte();
                        	    }
                    	    }  
                            imaginaryDataBytes = getInt(endianess);
                            if (imaginaryDataBytes == realDataBytes) {
                            	Preferences.debug("imaginaryDataByts == realDataBytes as expected\n");
                            }
                            else {
                            	Preferences.debug("imaginaryDataBytes unexpectedly != realDataBytes\n");
                            }
                            imaginaryDBuffer = new double[imageLength];
                            for (i = 0; i < imageLength; i++) {
                            	imaginaryDBuffer[i] = getDouble(endianess);
                            }
                            boolean logMagDisplay = true;
                            try {
                    			image.importDComplexData(0, realDBuffer, imaginaryDBuffer, true, logMagDisplay);
                    		}
                    		catch(IOException e) {
                    		   MipavUtil.displayError("IOException on image.importDComplexData(0, realDBuffer, imaginaryDBuffer, true, logMagDisplay)");
                    		   throw e;
                    		}
                    	}
                    	break;
                    case miINT64:
                    	Preferences.debug("Real data type = miINT64\n");
                    	Preferences.debug("Real data bytes = " + realDataBytes + "\n");
                    	if (!complexFlag) {
                    		image = new ModelImage(ModelStorageBase.LONG, imageExtents, fileName);
                    		fileInfo.setDataType(ModelStorageBase.LONG);
                    		longBuffer = new long[imageLength];
                    		for (i = 0; i < imageLength; i++) {
                    		    longBuffer[i] = getLong(endianess);
                    		}
                    		try {
                    			image.importData(0, longBuffer, true);
                    		}
                    		catch(IOException e) {
                    		   MipavUtil.displayError("IOException on image.importData(0, longBuffer, true)");
                    		   throw e;
                    		}
                    	}
                    	else {
                    		image = new ModelImage(ModelStorageBase.DCOMPLEX, imageExtents, fileName); 
                    		fileInfo.setDataType(ModelStorageBase.DCOMPLEX);
                    		realDBuffer = new double[imageLength];
                    		for (i = 0; i < imageLength; i++) {
                    		    realDBuffer[i] = (double)getLong(endianess);
                    		}
                    	    imaginaryDataType = getInt(endianess);
                    	    if (imaginaryDataType == miINT64) {
                    	    	Preferences.debug("imaginaryDataType == miINT64 as expected\n");
                    	    }
                    	    else {
                    	    	Preferences.debug("imaginaryDataType unexpectedly == " + imaginaryDataType + "\n");
                    	    }
                    	    if ((realDataBytes % 8) != 0) {
                    	    	padBytes = 8 - (realDataBytes % 8);
                    	    	for (i = 0; i < padBytes; i++) {
                        	    	raFile.readByte();
                        	    }
                    	    }  
                            imaginaryDataBytes = getInt(endianess);
                            if (imaginaryDataBytes == realDataBytes) {
                            	Preferences.debug("imaginaryDataByts == realDataBytes as expected\n");
                            }
                            else {
                            	Preferences.debug("imaginaryDataBytes unexpectedly != realDataBytes\n");
                            }
                            imaginaryDBuffer = new double[imageLength];
                            for (i = 0; i < imageLength; i++) {
                            	imaginaryDBuffer[i] = (double)getLong(endianess);
                            }
                            boolean logMagDisplay = true;
                            try {
                    			image.importDComplexData(0, realDBuffer, imaginaryDBuffer, true, logMagDisplay);
                    		}
                    		catch(IOException e) {
                    		   MipavUtil.displayError("IOException on image.importDComplexData(0, realDBuffer, imaginaryDBuffer, true, logMagDisplay)");
                    		   throw e;
                    		}
                    	}
                    	break;
                    case miUINT64:
                    	Preferences.debug("Real data type = miUINT64\n");
                    	Preferences.debug("Real data bytes = " + realDataBytes + "\n");
                    	if (!complexFlag) {
                    		image = new ModelImage(ModelStorageBase.LONG, imageExtents, fileName);
                    		fileInfo.setDataType(ModelStorageBase.LONG);
                    		longBuffer = new long[imageLength];
                    		for (i = 0; i < imageLength; i++) {
                    		    longBuffer[i] = getLong(endianess);
                    		}
                    		try {
                    			image.importData(0, longBuffer, true);
                    		}
                    		catch(IOException e) {
                    		   MipavUtil.displayError("IOException on image.importData(0, longBuffer, true)");
                    		   throw e;
                    		}
                    	}
                    	else {
                    		image = new ModelImage(ModelStorageBase.DCOMPLEX, imageExtents, fileName);
                    		fileInfo.setDataType(ModelStorageBase.DCOMPLEX);
                    		realDBuffer = new double[imageLength];
                    		for (i = 0; i < imageLength; i++) {
                    		    realDBuffer[i] = (double)getLong(endianess);
                    		}
                    	    imaginaryDataType = getInt(endianess);
                    	    if (imaginaryDataType == miUINT64) {
                    	    	Preferences.debug("imaginaryDataType == miUINT64 as expected\n");
                    	    }
                    	    else {
                    	    	Preferences.debug("imaginaryDataType unexpectedly == " + imaginaryDataType + "\n");
                    	    }
                    	    if ((realDataBytes % 8) != 0) {
                    	    	padBytes = 8 - (realDataBytes % 8);
                    	    	for (i = 0; i < padBytes; i++) {
                        	    	raFile.readByte();
                        	    }
                    	    }  
                            imaginaryDataBytes = getInt(endianess);
                            if (imaginaryDataBytes == realDataBytes) {
                            	Preferences.debug("imaginaryDataByts == realDataBytes as expected\n");
                            }
                            else {
                            	Preferences.debug("imaginaryDataBytes unexpectedly != realDataBytes\n");
                            }
                            imaginaryDBuffer = new double[imageLength];
                            for (i = 0; i < imageLength; i++) {
                            	imaginaryDBuffer[i] = (double)getLong(endianess);
                            }
                            boolean logMagDisplay = true;
                            try {
                    			image.importDComplexData(0, realDBuffer, imaginaryDBuffer, true, logMagDisplay);
                    		}
                    		catch(IOException e) {
                    		   MipavUtil.displayError("IOException on image.importDComplexData(0, realDBuffer, imaginaryDBuffer, true, logMagDisplay)");
                    		   throw e;
                    		}
                    	}
                    	break;
                    default:
                    	Preferences.debug("Illegal data type = " + realDataType + "\n");
                    	Preferences.debug("Real data bytes = " + realDataBytes + "\n");
                    }
                    fileInfo.setMin(image.getMin());
                	fileInfo.setMax(image.getMax());
                    } // if (imagesFound == 1)
                    else { // imagesFound > 1
                    	switch(realDataType) {
                        case miINT8:
                        	Preferences.debug("Real data type = miINT8\n");
                        	Preferences.debug("Real data bytes = " + realDataBytes + "\n");
                        	if (!complexFlag) {
    	                    	image2 = new ModelImage(ModelStorageBase.BYTE, imageExtents, fileName);
    	                    	fileInfo2.setDataType(ModelStorageBase.BYTE);
    	                    	buffer = new byte[imageLength];
    	                    	raFile.read(buffer);
    	                    	try {
    	                			image2.importData(0, buffer, true);
    	                		}
    	                		catch(IOException e) {
    	                		   MipavUtil.displayError("IOException on image2.importData(0, buffer, true)");
    	                		   throw e;
    	                		}
                        	}
                        	else {
                        		image2 = new ModelImage(ModelStorageBase.COMPLEX, imageExtents, fileName); 
                        		fileInfo2.setDataType(ModelStorageBase.COMPLEX);
                        		buffer = new byte[imageLength];
                        		raFile.read(buffer);
                        		realBuffer = new float[imageLength];
                        		for (i = 0; i < imageLength; i++) {
                        		    realBuffer[i] = (float)(buffer[i]);
                        		}
                        		if ((realDataBytes % 8) != 0) {
                        	    	padBytes = 8 - (realDataBytes % 8);
                        	    	for (i = 0; i < padBytes; i++) {
                            	    	raFile.readByte();
                            	    }
                        	    }  
                        	    imaginaryDataType = getInt(endianess);
                        	    if (imaginaryDataType == miINT8) {
                        	    	Preferences.debug("imaginaryDataType == miINT8 as expected\n");
                        	    }
                        	    else {
                        	    	Preferences.debug("imaginaryDataType unexpectedly == " + imaginaryDataType + "\n");
                        	    }
                                imaginaryDataBytes = getInt(endianess);
                                if (imaginaryDataBytes == realDataBytes) {
                                	Preferences.debug("imaginaryDataByts == realDataBytes as expected\n");
                                }
                                else {
                                	Preferences.debug("imaginaryDataBytes unexpectedly != realDataBytes\n");
                                }
                                raFile.read(buffer);
                                imaginaryBuffer = new float[imageLength];
                                for (i = 0; i < imageLength; i++) {
                                	imaginaryBuffer[i] = (float)(buffer[i]);
                                }
                                boolean logMagDisplay = true;
                                try {
                        			image2.importComplexData(0, realBuffer, imaginaryBuffer, true, logMagDisplay);
                        		}
                        		catch(IOException e) {
                        		   MipavUtil.displayError("IOException on image2.importComplexData(0, realBuffer, imaginaryBuffer, true, logMagDisplay)");
                        		   throw e;
                        		}		
                        	}
                        	break;
                        case miUINT8:
                        	Preferences.debug("Real data type = miUINT8\n");
                        	Preferences.debug("Real data bytes = " + realDataBytes + "\n");
                        	if (!complexFlag) {
    	                    	image2 = new ModelImage(ModelStorageBase.UBYTE, imageExtents, fileName);
    	                    	fileInfo2.setDataType(ModelStorageBase.UBYTE);
    	                    	buffer = new byte[imageLength];
    	                    	raFile.read(buffer);
    	                    	shortBuffer = new short[imageLength];
    	                    	for (i = 0; i < imageLength; i++) {
    	                		    shortBuffer[i] = (short)(buffer[i] & 0xff);
    	                		}
    	                    	try {
    	                			image2.importUData(0, shortBuffer, true);
    	                		}
    	                		catch(IOException e) {
    	                		   MipavUtil.displayError("IOException on image2.importData(0, shortBuffer, true)");
    	                		   throw e;
    	                		}
                        	}
                        	else {
                        		image2 = new ModelImage(ModelStorageBase.COMPLEX, imageExtents, fileName); 
                        		fileInfo2.setDataType(ModelStorageBase.COMPLEX);
                        		buffer = new byte[imageLength];
                        		raFile.read(buffer);
                        		realBuffer = new float[imageLength];
                        		for (i = 0; i < imageLength; i++) {
                        		    realBuffer[i] = (float)(buffer[i] & 0xff);
                        		}
                        		if ((realDataBytes % 8) != 0) {
                        	    	padBytes = 8 - (realDataBytes % 8);
                        	    	for (i = 0; i < padBytes; i++) {
                            	    	raFile.readByte();
                            	    }
                        	    }  
                        	    imaginaryDataType = getInt(endianess);
                        	    if (imaginaryDataType == miUINT8) {
                        	    	Preferences.debug("imaginaryDataType == miUINT8 as expected\n");
                        	    }
                        	    else {
                        	    	Preferences.debug("imaginaryDataType unexpectedly == " + imaginaryDataType + "\n");
                        	    }
                                imaginaryDataBytes = getInt(endianess);
                                if (imaginaryDataBytes == realDataBytes) {
                                	Preferences.debug("imaginaryDataByts == realDataBytes as expected\n");
                                }
                                else {
                                	Preferences.debug("imaginaryDataBytes unexpectedly != realDataBytes\n");
                                }
                                raFile.read(buffer);
                                imaginaryBuffer = new float[imageLength];
                                for (i = 0; i < imageLength; i++) {
                                	imaginaryBuffer[i] = (float)(buffer[i] & 0xff);
                                }
                                boolean logMagDisplay = true;
                                try {
                        			image2.importComplexData(0, realBuffer, imaginaryBuffer, true, logMagDisplay);
                        		}
                        		catch(IOException e) {
                        		   MipavUtil.displayError("IOException on image2.importComplexData(0, realBuffer, imaginaryBuffer, true, logMagDisplay)");
                        		   throw e;
                        		}	
                        	}
                        	break;
                        case miINT16:
                        	Preferences.debug("Real data type = miINT16\n");
                        	Preferences.debug("Real data bytes = " + realDataBytes + "\n");
                        	if (!complexFlag) {
                        		image2 = new ModelImage(ModelStorageBase.SHORT, imageExtents, fileName); 
                        		fileInfo2.setDataType(ModelStorageBase.SHORT);
                        		shortBuffer = new short[imageLength];
                        		for (i = 0; i < imageLength; i++) {
                        		    shortBuffer[i] = readShort(endianess);
                        		}
                        		try {
                        			image2.importData(0, shortBuffer, true);
                        		}
                        		catch(IOException e) {
                        		   MipavUtil.displayError("IOException on image2.importData(0, shortBuffer, true)");
                        		   throw e;
                        		}
                        	}
                        	else {
                        		image2 = new ModelImage(ModelStorageBase.COMPLEX, imageExtents, fileName); 
                        		fileInfo2.setDataType(ModelStorageBase.COMPLEX);
                        		realBuffer = new float[imageLength];
                        		for (i = 0; i < imageLength; i++) {
                        		    realBuffer[i] = (float)(readShort(endianess));
                        		}
                        		if ((realDataBytes % 8) != 0) {
                        	    	padBytes = 8 - (realDataBytes % 8);
                        	    	for (i = 0; i < padBytes; i++) {
                            	    	raFile.readByte();
                            	    }
                        	    }  
                        	    imaginaryDataType = getInt(endianess);
                        	    if (imaginaryDataType == miINT16) {
                        	    	Preferences.debug("imaginaryDataType == miINT16 as expected\n");
                        	    }
                        	    else {
                        	    	Preferences.debug("imaginaryDataType unexpectedly == " + imaginaryDataType + "\n");
                        	    }
                                imaginaryDataBytes = getInt(endianess);
                                if (imaginaryDataBytes == realDataBytes) {
                                	Preferences.debug("imaginaryDataByts == realDataBytes as expected\n");
                                }
                                else {
                                	Preferences.debug("imaginaryDataBytes unexpectedly != realDataBytes\n");
                                }
                                imaginaryBuffer = new float[imageLength];
                                for (i = 0; i < imageLength; i++) {
                                	imaginaryBuffer[i] = (float)(readShort(endianess));
                                }
                                boolean logMagDisplay = true;
                                try {
                        			image2.importComplexData(0, realBuffer, imaginaryBuffer, true, logMagDisplay);
                        		}
                        		catch(IOException e) {
                        		   MipavUtil.displayError("IOException on image2.importComplexData(0, realBuffer, imaginaryBuffer, true, logMagDisplay)");
                        		   throw e;
                        		}		
                        	}
                        	break;
                        case miUINT16:
                        	Preferences.debug("Real data type = miUINT16\n");
                        	Preferences.debug("Real data bytes = " + realDataBytes + "\n");
                        	if (!complexFlag) {
                        		image2 = new ModelImage(ModelStorageBase.USHORT, imageExtents, fileName); 
                        		fileInfo2.setDataType(ModelStorageBase.USHORT);
                        		intBuffer = new int[imageLength];
                        		for (i = 0; i < imageLength; i++) {
                        		    intBuffer[i] = getUnsignedShort(endianess);
                        		}
                        		try {
                        			image2.importUData(0, intBuffer, true);
                        		}
                        		catch(IOException e) {
                        		   MipavUtil.displayError("IOException on image2.importUData(0, intBuffer, true)");
                        		   throw e;
                        		}
                        	}
                        	else {
                        		image2 = new ModelImage(ModelStorageBase.COMPLEX, imageExtents, fileName); 
                        		fileInfo2.setDataType(ModelStorageBase.COMPLEX);
                        		realBuffer = new float[imageLength];
                        		for (i = 0; i < imageLength; i++) {
                        		    realBuffer[i] = (float)(getUnsignedShort(endianess));
                        		}
                        		if ((realDataBytes % 8) != 0) {
                        	    	padBytes = 8 - (realDataBytes % 8);
                        	    	for (i = 0; i < padBytes; i++) {
                            	    	raFile.readByte();
                            	    }
                        	    }  
                        	    imaginaryDataType = getInt(endianess);
                        	    if (imaginaryDataType == miUINT16) {
                        	    	Preferences.debug("imaginaryDataType == miUINT16 as expected\n");
                        	    }
                        	    else {
                        	    	Preferences.debug("imaginaryDataType unexpectedly == " + imaginaryDataType + "\n");
                        	    }
                                imaginaryDataBytes = getInt(endianess);
                                if (imaginaryDataBytes == realDataBytes) {
                                	Preferences.debug("imaginaryDataByts == realDataBytes as expected\n");
                                }
                                else {
                                	Preferences.debug("imaginaryDataBytes unexpectedly != realDataBytes\n");
                                }
                                imaginaryBuffer = new float[imageLength];
                                for (i = 0; i < imageLength; i++) {
                                	imaginaryBuffer[i] = (float)(getUnsignedShort(endianess));
                                }
                                boolean logMagDisplay = true;
                                try {
                        			image2.importComplexData(0, realBuffer, imaginaryBuffer, true, logMagDisplay);
                        		}
                        		catch(IOException e) {
                        		   MipavUtil.displayError("IOException on image2.importComplexData(0, realBuffer, imaginaryBuffer, true, logMagDisplay)");
                        		   throw e;
                        		}		
                        	}
                        	break;
                        case miINT32:
                        	Preferences.debug("Real data type = miINT32\n");
                        	Preferences.debug("Real data bytes = " + realDataBytes + "\n");
                        	if (!complexFlag) {
                        		image2 = new ModelImage(ModelStorageBase.INTEGER, imageExtents, fileName);
                        		fileInfo2.setDataType(ModelStorageBase.INTEGER);
                        		intBuffer = new int[imageLength];
                        		for (i = 0; i < imageLength; i++) {
                        		    intBuffer[i] = getInt(endianess);
                        		}
                        		try {
                        			image2.importData(0, intBuffer, true);
                        		}
                        		catch(IOException e) {
                        		   MipavUtil.displayError("IOException on image2.importData(0, intBuffer, true)");
                        		   throw e;
                        		}
                        	}
                        	else {
                        		image2 = new ModelImage(ModelStorageBase.DCOMPLEX, imageExtents, fileName); 
                        		fileInfo2.setDataType(ModelStorageBase.DCOMPLEX);
                        		realDBuffer = new double[imageLength];
                        		for (i = 0; i < imageLength; i++) {
                        		    realDBuffer[i] = (double)getInt(endianess);
                        		}
                        	    imaginaryDataType = getInt(endianess);
                        	    if (imaginaryDataType == miINT32) {
                        	    	Preferences.debug("imaginaryDataType == miINT32 as expected\n");
                        	    }
                        	    else {
                        	    	Preferences.debug("imaginaryDataType unexpectedly == " + imaginaryDataType + "\n");
                        	    }
                        	    if ((realDataBytes % 8) != 0) {
                        	    	padBytes = 8 - (realDataBytes % 8);
                        	    	for (i = 0; i < padBytes; i++) {
                            	    	raFile.readByte();
                            	    }
                        	    }  
                                imaginaryDataBytes = getInt(endianess);
                                if (imaginaryDataBytes == realDataBytes) {
                                	Preferences.debug("imaginaryDataByts == realDataBytes as expected\n");
                                }
                                else {
                                	Preferences.debug("imaginaryDataBytes unexpectedly != realDataBytes\n");
                                }
                                imaginaryDBuffer = new double[imageLength];
                                for (i = 0; i < imageLength; i++) {
                                	imaginaryDBuffer[i] = (double)getInt(endianess);
                                }
                                boolean logMagDisplay = true;
                                try {
                        			image2.importDComplexData(0, realDBuffer, imaginaryDBuffer, true, logMagDisplay);
                        		}
                        		catch(IOException e) {
                        		   MipavUtil.displayError("IOException on image2.importDComplexData(0, realDBuffer, imaginaryDBuffer, true, logMagDisplay)");
                        		   throw e;
                        		}
                        	}
                        	break;
                        case miUINT32:
                        	Preferences.debug("Real data type = miUINT32\n");
                        	Preferences.debug("Real data bytes = " + realDataBytes + "\n");
                        	if (!complexFlag) {
                        		image2 = new ModelImage(ModelStorageBase.UINTEGER, imageExtents, fileName);
                        		fileInfo2.setDataType(ModelStorageBase.UINTEGER);
                        		longBuffer = new long[imageLength];
                        		for (i = 0; i < imageLength; i++) {
                        		    longBuffer[i] = getUInt(endianess);
                        		}
                        		try {
                        			image2.importUData(0, longBuffer, true);
                        		}
                        		catch(IOException e) {
                        		   MipavUtil.displayError("IOException on image2.importUData(0, longBuffer, true)");
                        		   throw e;
                        		}
                        	}
                        	else {
                        		image2 = new ModelImage(ModelStorageBase.DCOMPLEX, imageExtents, fileName);
                        		fileInfo2.setDataType(ModelStorageBase.DCOMPLEX);
                        		realDBuffer = new double[imageLength];
                        		for (i = 0; i < imageLength; i++) {
                        		    realDBuffer[i] = (double)getUInt(endianess);
                        		}
                        	    imaginaryDataType = getInt(endianess);
                        	    if (imaginaryDataType == miUINT32) {
                        	    	Preferences.debug("imaginaryDataType == miUINT32 as expected\n");
                        	    }
                        	    else {
                        	    	Preferences.debug("imaginaryDataType unexpectedly == " + imaginaryDataType + "\n");
                        	    }
                        	    if ((realDataBytes % 8) != 0) {
                        	    	padBytes = 8 - (realDataBytes % 8);
                        	    	for (i = 0; i < padBytes; i++) {
                            	    	raFile.readByte();
                            	    }
                        	    }  
                                imaginaryDataBytes = getInt(endianess);
                                if (imaginaryDataBytes == realDataBytes) {
                                	Preferences.debug("imaginaryDataByts == realDataBytes as expected\n");
                                }
                                else {
                                	Preferences.debug("imaginaryDataBytes unexpectedly != realDataBytes\n");
                                }
                                imaginaryDBuffer = new double[imageLength];
                                for (i = 0; i < imageLength; i++) {
                                	imaginaryDBuffer[i] = (double)getUInt(endianess);
                                }
                                boolean logMagDisplay = true;
                                try {
                        			image2.importDComplexData(0, realDBuffer, imaginaryDBuffer, true, logMagDisplay);
                        		}
                        		catch(IOException e) {
                        		   MipavUtil.displayError("IOException on image2.importDComplexData(0, realDBuffer, imaginaryDBuffer, true, logMagDisplay)");
                        		   throw e;
                        		}
                        	}
                        	break;
                        case miSINGLE:
                        	Preferences.debug("Real data type = miSINGLE\n");
                        	Preferences.debug("Real data bytes = " + realDataBytes + "\n");
                        	if (!complexFlag) {
                        		image2 = new ModelImage(ModelStorageBase.FLOAT, imageExtents, fileName); 
                        		fileInfo2.setDataType(ModelStorageBase.FLOAT);
                        		floatBuffer = new float[imageLength];
                        		for (i = 0; i < imageLength; i++) {
                        		    floatBuffer[i] = getFloat(endianess);
                        		}
                        		try {
                        			image2.importData(0, floatBuffer, true);
                        		}
                        		catch(IOException e) {
                        		   MipavUtil.displayError("IOException on image2.importData(0, floatBuffer, true)");
                        		   throw e;
                        		}
                        	}
                        	else {
                        		image2 = new ModelImage(ModelStorageBase.COMPLEX, imageExtents, fileName);
                        		fileInfo2.setDataType(ModelStorageBase.COMPLEX);
                        		realBuffer = new float[imageLength];
                        		for (i = 0; i < imageLength; i++) {
                        		    realBuffer[i] = getFloat(endianess);
                        		}
                        	    imaginaryDataType = getInt(endianess);
                        	    if (imaginaryDataType == miSINGLE) {
                        	    	Preferences.debug("imaginaryDataType == miSINGLE as expected\n");
                        	    }
                        	    else {
                        	    	Preferences.debug("imaginaryDataType unexpectedly == " + imaginaryDataType + "\n");
                        	    }
                        	    if ((realDataBytes % 8) != 0) {
                        	    	padBytes = 8 - (realDataBytes % 8);
                        	    	for (i = 0; i < padBytes; i++) {
                            	    	raFile.readByte();
                            	    }
                        	    }  
                                imaginaryDataBytes = getInt(endianess);
                                if (imaginaryDataBytes == realDataBytes) {
                                	Preferences.debug("imaginaryDataByts == realDataBytes as expected\n");
                                }
                                else {
                                	Preferences.debug("imaginaryDataBytes unexpectedly != realDataBytes\n");
                                }
                                imaginaryBuffer = new float[imageLength];
                                for (i = 0; i < imageLength; i++) {
                                	imaginaryBuffer[i] = getFloat(endianess);
                                }
                                boolean logMagDisplay = true;
                                try {
                        			image2.importComplexData(0, realBuffer, imaginaryBuffer, true, logMagDisplay);
                        		}
                        		catch(IOException e) {
                        		   MipavUtil.displayError("IOException on image2.importComplexData(0, realBuffer, imaginaryBuffer, true, logMagDisplay)");
                        		   throw e;
                        		}
                        	}
                        	break;
                        case miDOUBLE:
                        	Preferences.debug("Real data type = miDOUBLE\n");
                        	Preferences.debug("Real data bytes = " + realDataBytes + "\n");
                        	if (!complexFlag) {
                        		image2 = new ModelImage(ModelStorageBase.DOUBLE, imageExtents, fileName); 
                        		fileInfo2.setDataType(ModelStorageBase.DOUBLE);
                        		doubleBuffer = new double[imageLength];
                        		for (i = 0; i < imageLength; i++) {
                        		    doubleBuffer[i] = getDouble(endianess);
                        		}
                        		try {
                        			image2.importData(0, doubleBuffer, true);
                        		}
                        		catch(IOException e) {
                        		   MipavUtil.displayError("IOException on image2.importData(0, doubleBuffer, true)");
                        		   throw e;
                        		}
                        	}
                        	else {
                        		image2 = new ModelImage(ModelStorageBase.DCOMPLEX, imageExtents, fileName); 
                        		fileInfo2.setDataType(ModelStorageBase.DCOMPLEX);
                        		realDBuffer = new double[imageLength];
                        		for (i = 0; i < imageLength; i++) {
                        		    realDBuffer[i] = getDouble(endianess);
                        		}
                        	    imaginaryDataType = getInt(endianess);
                        	    if (imaginaryDataType == miDOUBLE) {
                        	    	Preferences.debug("imaginaryDataType == miDOUBLE as expected\n");
                        	    }
                        	    else {
                        	    	Preferences.debug("imaginaryDataType unexpectedly == " + imaginaryDataType + "\n");
                        	    }
                        	    if ((realDataBytes % 8) != 0) {
                        	    	padBytes = 8 - (realDataBytes % 8);
                        	    	for (i = 0; i < padBytes; i++) {
                            	    	raFile.readByte();
                            	    }
                        	    }  
                                imaginaryDataBytes = getInt(endianess);
                                if (imaginaryDataBytes == realDataBytes) {
                                	Preferences.debug("imaginaryDataByts == realDataBytes as expected\n");
                                }
                                else {
                                	Preferences.debug("imaginaryDataBytes unexpectedly != realDataBytes\n");
                                }
                                imaginaryDBuffer = new double[imageLength];
                                for (i = 0; i < imageLength; i++) {
                                	imaginaryDBuffer[i] = getDouble(endianess);
                                }
                                boolean logMagDisplay = true;
                                try {
                        			image2.importDComplexData(0, realDBuffer, imaginaryDBuffer, true, logMagDisplay);
                        		}
                        		catch(IOException e) {
                        		   MipavUtil.displayError("IOException on image2.importDComplexData(0, realDBuffer, imaginaryDBuffer, true, logMagDisplay)");
                        		   throw e;
                        		}
                        	}
                        	break;
                        case miINT64:
                        	Preferences.debug("Real data type = miINT64\n");
                        	Preferences.debug("Real data bytes = " + realDataBytes + "\n");
                        	if (!complexFlag) {
                        		image2 = new ModelImage(ModelStorageBase.LONG, imageExtents, fileName);
                        		fileInfo2.setDataType(ModelStorageBase.LONG);
                        		longBuffer = new long[imageLength];
                        		for (i = 0; i < imageLength; i++) {
                        		    longBuffer[i] = getLong(endianess);
                        		}
                        		try {
                        			image2.importData(0, longBuffer, true);
                        		}
                        		catch(IOException e) {
                        		   MipavUtil.displayError("IOException on image2.importData(0, longBuffer, true)");
                        		   throw e;
                        		}
                        	}
                        	else {
                        		image2 = new ModelImage(ModelStorageBase.DCOMPLEX, imageExtents, fileName); 
                        		fileInfo2.setDataType(ModelStorageBase.DCOMPLEX);
                        		realDBuffer = new double[imageLength];
                        		for (i = 0; i < imageLength; i++) {
                        		    realDBuffer[i] = (double)getLong(endianess);
                        		}
                        	    imaginaryDataType = getInt(endianess);
                        	    if (imaginaryDataType == miINT64) {
                        	    	Preferences.debug("imaginaryDataType == miINT64 as expected\n");
                        	    }
                        	    else {
                        	    	Preferences.debug("imaginaryDataType unexpectedly == " + imaginaryDataType + "\n");
                        	    }
                        	    if ((realDataBytes % 8) != 0) {
                        	    	padBytes = 8 - (realDataBytes % 8);
                        	    	for (i = 0; i < padBytes; i++) {
                            	    	raFile.readByte();
                            	    }
                        	    }  
                                imaginaryDataBytes = getInt(endianess);
                                if (imaginaryDataBytes == realDataBytes) {
                                	Preferences.debug("imaginaryDataByts == realDataBytes as expected\n");
                                }
                                else {
                                	Preferences.debug("imaginaryDataBytes unexpectedly != realDataBytes\n");
                                }
                                imaginaryDBuffer = new double[imageLength];
                                for (i = 0; i < imageLength; i++) {
                                	imaginaryDBuffer[i] = (double)getLong(endianess);
                                }
                                boolean logMagDisplay = true;
                                try {
                        			image2.importDComplexData(0, realDBuffer, imaginaryDBuffer, true, logMagDisplay);
                        		}
                        		catch(IOException e) {
                        		   MipavUtil.displayError("IOException on image2.importDComplexData(0, realDBuffer, imaginaryDBuffer, true, logMagDisplay)");
                        		   throw e;
                        		}
                        	}
                        	break;
                        case miUINT64:
                        	Preferences.debug("Real data type = miUINT64\n");
                        	Preferences.debug("Real data bytes = " + realDataBytes + "\n");
                        	if (!complexFlag) {
                        		image2 = new ModelImage(ModelStorageBase.LONG, imageExtents, fileName); 
                        		fileInfo2.setDataType(ModelStorageBase.LONG);
                        		longBuffer = new long[imageLength];
                        		for (i = 0; i < imageLength; i++) {
                        		    longBuffer[i] = getLong(endianess);
                        		}
                        		try {
                        			image2.importData(0, longBuffer, true);
                        		}
                        		catch(IOException e) {
                        		   MipavUtil.displayError("IOException on image2.importData(0, longBuffer, true)");
                        		   throw e;
                        		}
                        	}
                        	else {
                        		image2 = new ModelImage(ModelStorageBase.DCOMPLEX, imageExtents, fileName); 
                        		fileInfo2.setDataType(ModelStorageBase.DCOMPLEX);
                        		realDBuffer = new double[imageLength];
                        		for (i = 0; i < imageLength; i++) {
                        		    realDBuffer[i] = (double)getLong(endianess);
                        		}
                        	    imaginaryDataType = getInt(endianess);
                        	    if (imaginaryDataType == miUINT64) {
                        	    	Preferences.debug("imaginaryDataType == miUINT64 as expected\n");
                        	    }
                        	    else {
                        	    	Preferences.debug("imaginaryDataType unexpectedly == " + imaginaryDataType + "\n");
                        	    }
                        	    if ((realDataBytes % 8) != 0) {
                        	    	padBytes = 8 - (realDataBytes % 8);
                        	    	for (i = 0; i < padBytes; i++) {
                            	    	raFile.readByte();
                            	    }
                        	    }  
                                imaginaryDataBytes = getInt(endianess);
                                if (imaginaryDataBytes == realDataBytes) {
                                	Preferences.debug("imaginaryDataByts == realDataBytes as expected\n");
                                }
                                else {
                                	Preferences.debug("imaginaryDataBytes unexpectedly != realDataBytes\n");
                                }
                                imaginaryDBuffer = new double[imageLength];
                                for (i = 0; i < imageLength; i++) {
                                	imaginaryDBuffer[i] = (double)getLong(endianess);
                                }
                                boolean logMagDisplay = true;
                                try {
                        			image2.importDComplexData(0, realDBuffer, imaginaryDBuffer, true, logMagDisplay);
                        		}
                        		catch(IOException e) {
                        		   MipavUtil.displayError("IOException on image2.importDComplexData(0, realDBuffer, imaginaryDBuffer, true, logMagDisplay)");
                        		   throw e;
                        		}
                        	}
                        	break;
                        default:
                        	Preferences.debug("Illegal data type = " + realDataType + "\n");
                        	Preferences.debug("Real data bytes = " + realDataBytes + "\n");
                        }	
                    	fileInfo2.setMin(image2.getMin());
                    	fileInfo2.setMax(image2.getMax());
                    } // else imagesFound > 1
                	break;
                case miUTF8:
                	Preferences.debug("Data type = miUTF8\n");
                	Preferences.debug("Bytes in data element = " + elementBytes + "\n");
                	buffer = new byte[elementBytes];
                	raFile.read(buffer);
                	str = new String(buffer, 0, elementBytes, "UTF-8");
                	Preferences.debug("UTF-8 encoded character data:\n" + str + "\n");
                	break;
                case miUTF16:
                	Preferences.debug("Data type = miUTF16\n");
                	Preferences.debug("Bytes in data element = " + elementBytes + "\n");
                	buffer = new byte[elementBytes];
                	raFile.read(buffer);
                	if (endianess == FileBase.BIG_ENDIAN) {
                	    str = new String(buffer, 0, elementBytes, "UTF-16BE");
                	}
                	else {
                		str = new String(buffer, 0, elementBytes, "UTF-16LE");	
                	}
                	Preferences.debug("UTF-16 encoded character data:\n" + str + "\n");
                	break;
                case miUTF32:
                	Preferences.debug("Data type = miUTF32\n");
                	Preferences.debug("Bytes in data element = " + elementBytes + "\n");
                	buffer = new byte[elementBytes];
                	raFile.read(buffer);
                	if (endianess == FileBase.BIG_ENDIAN) {
                	    str = new String(buffer, 0, elementBytes, "UTF-32BE");
                	}
                	else {
                		str = new String(buffer, 0, elementBytes, "UTF-32LE");	
                	}
                	Preferences.debug("UTF-32 encoded character data:\n" + str + "\n");
                	break;
                default:
                	Preferences.debug("Illegal data type = " + dataType + "\n");
                	Preferences.debug("Bytes in data element = " + elementBytes + "\n");
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
            
           
            for (i = 0; i < imageSlices; i++) {
                
                image.setFileInfo((FileInfoMATLAB)fileInfo.clone(), i);
            }
            if (image2 != null) {
            	 for (i = 0; i < imageSlices2; i++) {
                     
                     image2.setFileInfo((FileInfoMATLAB)fileInfo2.clone(), i);
                 }
            	 vFrame2 = new ViewJFrameImage(image2);
            	 vFrame2.setTitle(fileName.substring(0,s) + "_2");
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

    
}
