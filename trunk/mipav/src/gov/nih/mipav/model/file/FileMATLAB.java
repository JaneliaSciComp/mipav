package gov.nih.mipav.model.file;


import gov.nih.mipav.model.structures.*;

import java.io.*;

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
        int imageSlices = 0;
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

        try {
            
            imgResols[0] = imgResols[1] = imgResols[2] = imgResols[3] = imgResols[4] = (float) 1.0;
            file = new File(fileDir + fileName);
            raFile = new RandomAccessFile(file, "r");
            
            fileLength = raFile.length();
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
                    nextElementAddress = nextElementAddress + elementBytes + padBytes + 8;
                }
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
                	break;
                case miCOMPRESSED:
                	Preferences.debug("Data type = miCOMPRESSED\n");
                	Preferences.debug("Bytes in data element = " + elementBytes + "\n");
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
            } // while (nextElementAddress)
            
           
            for (i = 0; i < imageSlices; i++) {
                
                image.setFileInfo((FileInfoMATLAB)fileInfo.clone(), i);
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
