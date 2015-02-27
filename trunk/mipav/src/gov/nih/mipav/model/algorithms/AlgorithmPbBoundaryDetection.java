package gov.nih.mipav.model.algorithms;

import gov.nih.mipav.model.algorithms.filters.AlgorithmHilbertTransform;
import gov.nih.mipav.model.algorithms.filters.FFTUtility;
import gov.nih.mipav.model.algorithms.utilities.AlgorithmChangeType;
import gov.nih.mipav.model.algorithms.utilities.AlgorithmRGBtoGray;
import gov.nih.mipav.model.file.FileBase;
import gov.nih.mipav.model.file.FileInfoBase;
import gov.nih.mipav.model.file.FileInfoMATLAB;
import gov.nih.mipav.model.file.FileUtility;
import gov.nih.mipav.model.structures.*;
import gov.nih.mipav.util.MipavMath;
import gov.nih.mipav.view.*;

import java.io.*;
import java.util.zip.Inflater;

import Jama.Matrix;

/**
 * Compute probability of boundary using brightness gradient and texture gradient
 * Original MATLAB code written by David R. Martin in April 2003
 * 
 * Reference: "Learning to Detect Natural Image Boundaries Using Local Brightness, Color,
 *             and Texture Cues" by David R. Martin, Charless C. Fowlkes, and Jitendra
 *             Malik, IEEE Transactions on Pattern Analysis and Machine Intelligence,
 *             Vol. 26, No. 5, May, 2004, pp. 530-549.
 * @author ilb
 *
 */

public class AlgorithmPbBoundaryDetection extends AlgorithmBase {
	private static final int BGTG = 1;
	
	private int gradientType = BGTG;
	
	private static final int GRAY_PRESENTATION = 1;
	
	private static final int COLOR_PRESENTATION = 2;
	
	private int presentation = GRAY_PRESENTATION;
	
	private double lowRadius = 0.01;
	
	private double highRadius = 0.02;
	
	private int numOrientations = 8;
	
	private int xDim;
	
	private int yDim;
	
	private RandomAccessFile raFile;
	
	/** byte array for short * */
    private final byte[] byteShortBuffer = new byte[2];
    
    /** byte array for int * */
    private final byte[] byteIntBuffer = new byte[4];
	
	 /** byte array for long * */
    private final byte[] byteLongBuffer = new byte[8];
	
	// epsilon = D1MACH(4)
	// Machine epsilon is the smallest positive epsilon such that
	// (1.0 + epsilon) != 1.0.
	// epsilon = 2**(1 - doubleDigits) = 2**(1 - 53) = 2**(-52)
	// epsilon = 2.2204460e-16
	// epsilon is called the largest relative spacing
	private final double epsilon = Math.pow(2.0, -52);
	
	
	 //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * AlgorithmPbBoundaryDetection - default constructor.
     */
    public AlgorithmPbBoundaryDetection() { }
    
    /**
     * AlgorithmPbBoundaryDetection.
     *
     * @param  destImg  DOCUMENT ME!
     * @param  srcImg   DOCUMENT ME!
     * @param  gradientType
       @param  presentation
       @param  lowRadius
       @param  highRadius
       @param  numOrientations
     */
    public AlgorithmPbBoundaryDetection(ModelImage destImg, ModelImage srcImg, int gradientType, int presentation, double lowRadius,
                                       double highRadius, int numOrientations) {
        super(destImg, srcImg);
        this.gradientType = gradientType;
        this.presentation = presentation;
        this.lowRadius = lowRadius;
        this.highRadius = highRadius;
        this.numOrientations = numOrientations;   
    }
    
    public void runAlgorithm() {
    	double pb[][];
    	double theta[][];
    	 xDim = srcImage.getExtents()[0];
         yDim = srcImage.getExtents()[1];
    	
    	if (srcImage == null) {
            displayError("Source Image is null");
            finalize();

            return;
        }
        
        fireProgressStateChanged(srcImage.getImageName(), "Running Pb Boundary Detection ...");
        
        if (gradientType == BGTG) {
        	pb = new double[yDim][xDim];
        	theta = new double[yDim][xDim];
        	pbBGTG(pb, theta);
        }
    }
    
    
      private void pbBGTG(double pb[][], double theta[][]) {

      	double beta[] = new double[3];
      	double fstd[] = new double[3];
      	double diag;
      	ModelImage grayImage = null;
      	ModelImage inputImage;
      	double bg[][][];
      	double gtheta[];
      	AlgorithmChangeType changeTypeAlgo;
      	FileInfoBase[] fileInfo;
        
        // beta from logistic fits (trainBGTG.m)
        if ((lowRadius == 0.01) && (highRadius == 0.02)) {
        	// 64 textons
        	if (presentation == GRAY_PRESENTATION) {
        		// trained on grayscale segmentations
        	    beta[0] = -4.6522915;
        	    beta[1] = 0.71345115;
        	    beta[2] = 0.70333326;
        	    fstd[0] = 1.0;
        	    fstd[1] = 0.37408935;
        	    fstd[2] = 0.19171689;
        	}
        	else if (presentation == COLOR_PRESENTATION) {
        		// trained on color segmentations
        		beta[0] = -4.4880396;
        		beta[1] = 0.70690368;
        		beta[2] = 0.65740193;
        		fstd[0] = 1.0;
        		fstd[1] = 0.37401028;
        		fstd[2] = 0.19181055;
        	}
        	else {
        		MipavUtil.displayError("Unknown presentation");
        		setCompleted(false);
        		return;
        	}
        	beta[0] = beta[0]/fstd[0];
        	beta[1] = beta[1]/fstd[1];
        	beta[2] = beta[2]/fstd[2];
        } // if ((lowRadius == 0.01) && (highRadius == 0.02))
        else {
        	MipavUtil.displayError("No parameters for lowRadius = " + lowRadius + " highRadius = " + highRadius);
        	setCompleted(false);
        	return;
        }
        
        // Get gradients
        // Compute smoothed but not thinned BG and TG fields
        diag = Math.sqrt(xDim*xDim + yDim*yDim);
        if (srcImage.isColorImage()) {
			final boolean thresholdAverage = false;
			final float threshold = 0.0f;
			final boolean intensityAverage = false;
			final boolean equalRange = true;
			final float minR = 0.0f;
			final float minG = 0.0f;
			final float minB = 0.0f;
			float redValue;
			float greenValue;
			float blueValue;
			float maxR;
			float maxG;
			float maxB;
			AlgorithmRGBtoGray gAlgo;
			if (srcImage.getMinR() == srcImage.getMaxR()) {
				redValue = 0.0f;
				greenValue = 0.5f;
				blueValue = 0.5f;
			} else if (srcImage.getMinG() == srcImage.getMaxG()) {
				redValue = 0.5f;
				greenValue = 0.0f;
				blueValue = 0.5f;
			} else if (srcImage.getMinB() == srcImage.getMaxB()) {
				redValue = 0.5f;
				greenValue = 0.5f;
				blueValue = 0.0f;
			} else {
				redValue = (float) (1.0 / 3.0);
				greenValue = redValue;
				blueValue = redValue;

			}
			maxR = (float) srcImage.getMaxR();
			maxG = (float) srcImage.getMaxG();
			maxB = (float) srcImage.getMaxB();
			grayImage = new ModelImage(ModelStorageBase.DOUBLE,
					srcImage.getExtents(), "grayImage");
			gAlgo = new AlgorithmRGBtoGray(grayImage, srcImage,
					redValue, greenValue, blueValue, thresholdAverage,
					threshold, intensityAverage, equalRange, minR, maxR,
					minG, maxG, minB, maxB);
			gAlgo.run();
			gAlgo.finalize();
		} // if (srcImage.isColorImage())
        
        inputImage = new ModelImage(ModelStorageBase.DOUBLE,
				srcImage.getExtents(), "changeTypeImage");
		inputImage.getFileInfo(0).setEndianess(FileBase.LITTLE_ENDIAN);
		if (srcImage.isColorImage()) {
			changeTypeAlgo = new AlgorithmChangeType(inputImage,
					grayImage, grayImage.getMin(), grayImage.getMax(),
					0.0, 1.0, image25D);
		} else {
			changeTypeAlgo = new AlgorithmChangeType(inputImage,
					srcImage, srcImage.getMin(),
					srcImage.getMax(), 0.0, 1.0, image25D);
		}
		changeTypeAlgo.run();
		changeTypeAlgo.finalize();
		changeTypeAlgo = null;
		if (grayImage != null) {
			grayImage.disposeLocal();
			grayImage = null;
		}
		fileInfo = inputImage.getFileInfo();
        fileInfo[0].setModality(srcImage.getFileInfo()[0].getModality());
        fileInfo[0].setFileDirectory(srcImage.getFileInfo()[0].getFileDirectory());
        fileInfo[0].setUnitsOfMeasure(srcImage.getFileInfo()[0].getUnitsOfMeasure());
        fileInfo[0].setResolutions(srcImage.getFileInfo()[0].getResolutions());
        fileInfo[0].setExtents(inputImage.getExtents());
        fileInfo[0].setMax(inputImage.getMax());
        fileInfo[0].setMin(inputImage.getMin());
        fileInfo[0].setImageOrientation(srcImage.getImageOrientation());
        fileInfo[0].setAxisOrientation(srcImage.getFileInfo()[0].getAxisOrientation());
        fileInfo[0].setOrigin(srcImage.getFileInfo()[0].getOrigin());
        // Compute brightness gradient
        bg = new double[yDim][xDim][numOrientations];
        gtheta = new double[numOrientations];
        cgmo(bg, gtheta, inputImage, diag*lowRadius, numOrientations, "savgol", diag*lowRadius);
        
        // Must read in 286,160 byte MATLAB file unitex_6_1_2_1.4_2_64.mat
        readFile();
    }
      
    private void readFile() {
    	/** 8 bit, signed */
    	final int miINT8 = 1;
    	/** 8 bit, unsigned */
    	final int miUINT8 = 2;
    	/** 16-bit, signed */
    	final int miINT16 = 3;
    	/** 16-bit, unsigned */
    	final int miUINT16 = 4;
    	/** 32-bit signed */
    	final int miINT32 = 5;
    	/** 32-bit, unsigned */
    	final int miUINT32 = 6;
    	/** IEEE 754 single format */
    	final int miSINGLE = 7;
    	/** IEEE 754 double format */
    	final int miDOUBLE = 9;
    	/** 64-bit, signed */
    	final int miINT64 = 12;
    	/** 64-bit, unsigned */
    	final int miUINT64 = 13;
    	/** MATLAB ARRAY */
    	final int miMATRIX = 14;
    	/** Compressed Data */
    	final int miCOMPRESSED = 15;
    	/** Unicode UTF-8 Encoded Character Data */
    	final int miUTF8 = 16;
    	/** Unicode UTF-16 Encoded Character Data */
    	final int miUTF16 = 17;
    	/** Unicode UTF-32 Encoded Character Data */
    	final int miUTF32 = 18;
    	// MATLAB Array Types (Classes)
    	/** Cell array */
    	final byte mxCELL_CLASS = 1;
    	/** Structure */
    	final byte mxSTRUCT_CLASS = 2;
    	/** Object */
        final byte mxOBJECT_CLASS = 3;
    	/** Character array */
    	final byte mxCHAR_CLASS = 4;
    	/** Sparse array */
    	final byte mxSPARSE_CLASS = 5;
    	/** Double precision array */
    	final byte mxDOUBLE_CLASS = 6;
    	/** Single precision array */
    	final byte mxSINGLE_CLASS = 7;
    	/** 8-bit, signed integer */
    	final byte mxINT8_CLASS = 8;
    	/** 8-bit, unsigned integer */
    	final byte mxUINT8_CLASS = 9;
    	/** 16-bit, signed integer */
    	final byte mxINT16_CLASS = 10;
    	/** 16-bit, unsigned integer */
    	final byte mxUINT16_CLASS = 11;
    	/** 32-bit, signed integer */
    	final byte mxINT32_CLASS = 12;
    	/** 32-bit, unsigned integer */
    	final byte mxUINT32_CLASS = 13;
    	/** 64-bit, signed integer */
    	final byte mxINT64_CLASS = 14;
    	/** 64-bit, unsigned integer */
    	final byte mxUINT64_CLASS = 15;
        String fileDir;
        String fileName;
        File file;
        long fileLength = 0L;
        byte firstEndianByte;
        byte secondEndianByte;
        boolean endianess = FileBase.BIG_ENDIAN;
        FileInfoMATLAB fileInfo;
        byte firstByte;
        byte secondByte;
        byte thirdByte;
        byte fourthByte;
        boolean level5Format;
        String headerTextField;
        long subsystemSpecificDataOffset;
        int version;
        long nextElementAddress;
        int elementNumber = 0;
        boolean isCompressed;
        int dataType;
        int elementBytes;
        int padBytes;
        byte buffer[] = null;
        Inflater zlibDecompresser = null;
        int bufSize;
        boolean memoryError;
        byte buf[] = null;
        byte[] decomp = null;
        int resultLength = 0;
        String uncompressedName = null;
        File ufile = null;
        int st;
        int[] imageExtents = null;
        int imagesFound = 0;
        int logicalFields = 0;
        int arrayFlagsDataType;
        int arrayFlagsBytes;
        int arrayFlags;
        int arrayClass;
        FileInfoMATLAB fileInfo2 = null;
        boolean complexFlag = false;
        @SuppressWarnings("unused")
        boolean globalFlag = false;
        boolean logicalFlag = false;
        int dimensionsArrayDataType;
        int dimensionsArrayBytes;
        int nDim;
        int structureDimensions[];
        int i, j;
        int imageLength = 1;
        int imageSlices = 1;
        int imageSlices2 = 1;
        int newExtents[] = null;
        int arrayNameDataType;
        int arrayNameBytes;
        String arrayName;
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
        boolean isVOI = false;
        boolean isVOI2 = false;
        int nonLogicalField = 0;
        int adjustedFieldDim;
        String voiFieldName = null;
        String voi2FieldName = null;
        int maskExtents[] = null;
        FileInfoMATLAB maskFileInfo = null;
        FileInfoMATLAB maskFileInfo2 = null;
        int realDataType;
        int realDataBytes;
        int imaginaryDataType;
        int imaginaryDataBytes;
        boolean haveSmallRealData;
        boolean haveSmallImaginaryData;
        boolean isColor = false;
        int sliceSize;
        ModelImage maskImage = null;
        byte tBuffer[] = null;
        int numberSlices;
        int x;
        int y;
        int s;
        ModelImage image = null;
        float realBuffer[] = null;
        float imaginaryBuffer[] = null;
        boolean logMagDisplay = Preferences.is(Preferences.PREF_LOGMAG_DISPLAY);
        short shortBuffer[] = null;
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
        int intBuffer[] = null;
        double realDBuffer[] = null;
        double imaginaryDBuffer[] = null;
        long longBuffer[] = null;
        float floatBuffer[] = null;
        double doubleBuffer[] = null;
        ModelImage maskImage2 = null;
        ModelImage image2 = null;
        int totalNumber = 1;
        String str;
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
        int totalNumber2;
        ViewJFrameImage vmFrame = null;
        ViewJFrameImage vmFrame2 = null;
        ViewJFrameImage vFrame2 = null;
        
        try {
	        fileDir = "C:/segbench/Textons/";
	        fileName = "unitex_6_1_2_1.4_2_64.mat";
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
                                          if (endianess == FileBase.BIG_ENDIAN) {
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
                                          if (endianess == FileBase.BIG_ENDIAN) {
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
                                          if (endianess == FileBase.BIG_ENDIAN) {
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
  	                                            if (endianess == FileBase.BIG_ENDIAN) {
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
                                          if (endianess == FileBase.BIG_ENDIAN) {
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
                                          if (endianess == FileBase.BIG_ENDIAN) {
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
                                          if (endianess == FileBase.BIG_ENDIAN) {
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
                                          if (endianess == FileBase.BIG_ENDIAN) {
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
                                          if (endianess == FileBase.BIG_ENDIAN) {
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
                                          if (endianess == FileBase.BIG_ENDIAN) {
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
                                          if (endianess == FileBase.BIG_ENDIAN) {
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
                                          if (endianess == FileBase.BIG_ENDIAN) {
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
                                          if (endianess == FileBase.BIG_ENDIAN) {
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
                                          if (endianess == FileBase.BIG_ENDIAN) {
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
                                          if (endianess == FileBase.BIG_ENDIAN) {
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
                                          if (endianess == FileBase.BIG_ENDIAN) {
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
  	                                            if (endianess == FileBase.BIG_ENDIAN) {
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
                                          if (endianess == FileBase.BIG_ENDIAN) {
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
                                          if (endianess == FileBase.BIG_ENDIAN) {
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
                                          if (endianess == FileBase.BIG_ENDIAN) {
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
                                          if (endianess == FileBase.BIG_ENDIAN) {
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
                                          if (endianess == FileBase.BIG_ENDIAN) {
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
                                          if (endianess == FileBase.BIG_ENDIAN) {
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
                                          if (endianess == FileBase.BIG_ENDIAN) {
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
                                          if (endianess == FileBase.BIG_ENDIAN) {
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
                                          if (endianess == FileBase.BIG_ENDIAN) {
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
                      case miMATRIX:
                    	  Preferences.debug("Real data type = miMATRIX\n", Preferences.DEBUG_FILEIO);
                          Preferences.debug("Real data bytes = " + realDataBytes + "\n", Preferences.DEBUG_FILEIO);
                          if (!complexFlag) {
                        	  buffer = new byte[realDataBytes];
                        	  raFile.read(buffer);
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
                                              if (endianess == FileBase.BIG_ENDIAN) {
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
                                              if (endianess == FileBase.BIG_ENDIAN) {
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
                                              if (endianess == FileBase.BIG_ENDIAN) {
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
  		                                            if (endianess == FileBase.BIG_ENDIAN) {
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
                                              if (endianess == FileBase.BIG_ENDIAN) {
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
                                              if (endianess == FileBase.BIG_ENDIAN) {
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
                                              if (endianess == FileBase.BIG_ENDIAN) {
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
                                              if (endianess == FileBase.BIG_ENDIAN) {
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
                                              if (endianess == FileBase.BIG_ENDIAN) {
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
                                              if (endianess == FileBase.BIG_ENDIAN) {
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
                                              if (endianess == FileBase.BIG_ENDIAN) {
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
                                              if (endianess == FileBase.BIG_ENDIAN) {
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
                                              if (endianess == FileBase.BIG_ENDIAN) {
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
                                              if (endianess == FileBase.BIG_ENDIAN) {
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
                                              if (endianess == FileBase.BIG_ENDIAN) {
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
                                              if (endianess == FileBase.BIG_ENDIAN) {
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
      	                                            if (endianess == FileBase.BIG_ENDIAN) {
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
                                              if (endianess == FileBase.BIG_ENDIAN) {
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
                                              if (endianess == FileBase.BIG_ENDIAN) {
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
                                              if (endianess == FileBase.BIG_ENDIAN) {
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
                                              if (endianess == FileBase.BIG_ENDIAN) {
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
                                              if (endianess == FileBase.BIG_ENDIAN) {
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
                                              if (endianess == FileBase.BIG_ENDIAN) {
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
                                              if (endianess == FileBase.BIG_ENDIAN) {
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
                                              if (endianess == FileBase.BIG_ENDIAN) {
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
                                              if (endianess == FileBase.BIG_ENDIAN) {
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
        }
        catch(IOException e) {
        	e.printStackTrace();
        	return;
        }
        
    }
    
    /**
     * Reads a string from a file of given <code>length</code>.
     * 
     * @param length Number of bytes that form the string.
     * 
     * @return The string read from the file.
     * 
     * @exception IOException if there is an error reading the file
     */
    public final String getString(final int length) throws IOException {

        if (length <= 0) {
            return new String("");
        }

        final byte[] b = new byte[length];
        raFile.readFully(b);

        return new String(b);
    }
    
    /**
     * Reads eight unsigned bytes from file.
     * 
     * @param bigEndian <code>true</code> indicates big endian byte order, <code>false</code> indicates little
     *            endian.
     * 
     * @return The value of the long read from the file.
     * 
     * @exception IOException if there is an error reading the file
     */
    public final long getLong(final boolean bigEndian) throws IOException {

        raFile.readFully(byteLongBuffer);

        long tmpLong;

        if (bigEndian) {
            tmpLong = ( ( (byteLongBuffer[0] & 0xffL) << 56) | ( (byteLongBuffer[1] & 0xffL) << 48)
                    | ( (byteLongBuffer[2] & 0xffL) << 40) | ( (byteLongBuffer[3] & 0xffL) << 32)
                    | ( (byteLongBuffer[4] & 0xffL) << 24) | ( (byteLongBuffer[5] & 0xffL) << 16)
                    | ( (byteLongBuffer[6] & 0xffL) << 8) | (byteLongBuffer[7] & 0xffL));

            return (tmpLong);
        } else {
            tmpLong = ( ( (byteLongBuffer[7] & 0xffL) << 56) | ( (byteLongBuffer[6] & 0xffL) << 48)
                    | ( (byteLongBuffer[5] & 0xffL) << 40) | ( (byteLongBuffer[4] & 0xffL) << 32)
                    | ( (byteLongBuffer[3] & 0xffL) << 24) | ( (byteLongBuffer[2] & 0xffL) << 16)
                    | ( (byteLongBuffer[1] & 0xffL) << 8) | (byteLongBuffer[0] & 0xffL));

            return (tmpLong);
        }
    }
    
    /**
     * Reads four signed bytes from file.
     * 
     * @param bigEndian <code>true</code> indicates big endian byte order, <code>false</code> indicates little
     *            endian.
     * 
     * @return The value of the integer read from the file.
     * 
     * @exception IOException if there is an error reading the file
     */
    public final int getInt(final boolean bigEndian) throws IOException {

        raFile.readFully(byteIntBuffer);

        if (bigEndian) {
            return ( ( (byteIntBuffer[0] & 0xff) << 24) | ( (byteIntBuffer[1] & 0xff) << 16)
                    | ( (byteIntBuffer[2] & 0xff) << 8) | (byteIntBuffer[3] & 0xff)); // Big Endian
        } else {
            return ( ( (byteIntBuffer[3] & 0xff) << 24) | ( (byteIntBuffer[2] & 0xff) << 16)
                    | ( (byteIntBuffer[1] & 0xff) << 8) | (byteIntBuffer[0] & 0xff));
        }
    }
    
    /**
     * Reads two unsigned bytes from file.
     * 
     * @param bigEndian <code>true</code> indicates big endian byte order, <code>false</code> indicates little
     *            endian.
     * 
     * @return The value of unsigned short read from the file returned as an int.
     * 
     * @exception IOException if there is an error reading the file
     */
    public final int getUnsignedShort(final boolean bigEndian) throws IOException {

        raFile.readFully(byteShortBuffer);

        if (bigEndian) {
            return ( ( (byteShortBuffer[0] & 0xff) << 8) | (byteShortBuffer[1] & 0xff)); // Big
                                                                                                            // Endian
        } else {
            return ( ( (byteShortBuffer[1] & 0xff) << 8) | (byteShortBuffer[0] & 0xff)); // Little
                                                                                                            // Endian
        }
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
      
    private void cgmo(double cg[][][], double theta[], ModelImage image, double radius, int numOrientations,
    		String smooth, double sigmaSmooth) {
        int nbins = 32;
        double sigmaSim = 0.1;
        double gamma = 2.5;
        cgmo(cg, theta, image, radius, numOrientations, nbins, sigmaSim, gamma, smooth, sigmaSmooth);
    }
    
    /**
     * Do a separate cgmoColor
     * Compute the color gradient at a single scale and multiple orientations
     * @param cg output [yDim][xDim][1][numOrientations] array for black and white 
     *           output [yDim][xDim][3][numOrientations] array for color
     * @param output [numOrientations] theta
     * @param image  Grayscale or RGB image, values in [0, 1].
     * @param radius Radius of disc for cg array
     * @param numOrientations Number of orientations for cg array
     * @param nbins Number of bins; should be > 1/sigmaSim.
     * @param sigmaSim For color similarity function
     * @param gamma Gamma correction for LAB [2.5].
     * @param smooth Smoothing method, one of {"gaussian", "savgol", "none"}, default none
     * @param sigmaSmooth Sigma for smoothing, default to radius
     */
    private void cgmo(double cg[][][], double theta[],
    		ModelImage image, double radius, int numOrientations, int nbins, double sigmaSim, double gamma, 
    		String smooth, double sigmaSmooth) {
        double abmin;
        double abmax;
        int cmap[][];
        int y;
        int x;
        double buffer[];
        int sliceSize = xDim * yDim;
        double bc[];
        int i;
        double xArr[][];
        double yArr[][];
        double csim[][];
        double diff;
        double denom;
        
        // Min and max values used for a,b channels of LAB
        // Used to scale values into the unit interval
        abmin = -73;
        abmax = 95;
        
        // Make sure bin is large enough with respect to sigmaSim
        if (nbins < 1.0/sigmaSim) {
        	MipavUtil.displayWarning("nbins < 1/sigmaSim is suspect");
        }
        
        if (image.getMin() < 0.0 || image.getMax() > 1.0) {
        	MipavUtil.displayError("Pixel values out of range 0 to 1");
        	return;
        }
        
        // Compute cg from gray values
        cmap = new int[yDim][xDim];
        buffer = new double[sliceSize];
        try {
        	image.exportData(0, sliceSize, buffer);
        }
        catch(IOException e) {
        	e.printStackTrace();
        }
        
        for (y = 0; y < yDim; y++) {
        	for (x = 0; x < xDim; x++) {
        		cmap[y][x] = Math.max(1, (int)Math.ceil(buffer[x + y * xDim] * nbins));
        	}
        }
        
        // Compute color similarity matrix assuming colors are in [0, 1]
        bc = new double[nbins];
        for (i = 0; i < nbins; i++) {
        	// Calculate bin centers
        	bc[i] = (i + 0.5)/nbins;
        }
        xArr = new double[nbins][nbins];
        yArr = new double[nbins][nbins];
        for (y = 0; y <  nbins; y++) {
        	for (x = 0; x < nbins; x++) {
        		xArr[y][x] = bc[x];
        		yArr[y][x] = bc[y];
        	}
        }
        csim = new double[nbins][nbins];
        denom = 2.0 * sigmaSim * sigmaSim;
        for (y = 0; y < nbins; y++) {
        	for (x = 0; x < nbins; x++) {
        		diff = xArr[y][x] - yArr[y][x];
        		csim[y][x] = 1.0 - Math.exp(-diff*diff/denom);
        	}
        }
        tgmo(cg, theta, cmap, nbins, radius, numOrientations, csim, smooth, sigmaSmooth);
    }
    
    /**
     * Compute the texture gradient at a single scale and multiple orientations
     * @param output tg  [yDim][xDim][numOrientations] array
     * @param output [numOrientations] theta Disc orientations (which are orthogonal to the texture gradient). 
     * @param tmap [yDim][xDim] Texton map, values in [1, ntex]
     * @param ntext Number of textons
     * @param radius Radius of disc for texture gradient
     * @param numOrientations Number of orientations at which to compute the texture graident
     * @param tsim Texton similarity matrix.  If not provided, then use chi-squared.
     * @param smooth Smoothing method.  One of "gaussian", "savgol", "none".  Default "none".
     * @param sigma Sigma for smoothing.  Default to radius.
     */
    private void tgmo(double tg[][][], double theta[], int tmap[][], int ntex, double radius, int numOrientations, 
    		double tsim[][], String smooth, double sigma) {
    	int i;
    	boolean usechi2;
    	double tgArray[][];
    	int y;
    	int x;
    	
    	if (tsim != null) {
    		usechi2 = false;
    	}
    	else {
    		usechi2 = true;
    	}
    	
    	radius = Math.max(1.0,  radius);
    	numOrientations = Math.max(1, numOrientations);
    	for (i = 0; i < numOrientations; i++) {
    		theta[i] = ((double)i)/numOrientations*Math.PI;
    	}
    	tgArray = new double[yDim][xDim];
    	for (i = 0; i < numOrientations; i++) {
    		if (usechi2) {
    		    tgso(tgArray, tmap, ntex, radius, theta[i], smooth, sigma, null);	
    		}
    		else {
    			tgso(tgArray, tmap, ntex, radius, theta[i], smooth, sigma, tsim);
    		}
    		for (y = 0; y < yDim; y++) {
    			for (x = 0; x < xDim; x++) {
    				tg[y][x][i] = tgArray[y][x];
    			}
    		}
    	} // for (i = 0; i < numOrientations; i++)
    }
    
    /**
     * Compute the texture graident at a single orientation and scale
     * @param output tg [yDim][xDim] array
     * @param tmap [yDim][xDim] Texton map, values in [1, ntex].
     * @param ntex Number of textons
     * @param radius Radius of disc for tg
     * @param theta Orientation orthogonal to tg.
     * @param smooth Smoothing method, one of {"gaussian", "savgol", "none"}, default = "none".
     * @param sigma Sigma for smoothing.  Default to radius.
     * @param tsim [ntex][ntex] Texton similarity matrix.  If not provided, then use chi-squared.
     */
    private void tgso(double tg[][], int tmap[][], int ntex, double radius, double theta, String smooth, double sigma, double tsim[][]) {
        boolean usechi2;
        int y;
        int x;
        int wr;
        double xgrid[][];
        double ygrid[][];
        double gamma[][];
        byte mask[][];
        int count;
        byte side[][];
        int sum1;
        int sum2;
        double lmask[][];
        double rmask[][];
        int i;
        double im[][];
        double tgL[][];
        double tgR[][];
        double diff;
        double d[][];
        Matrix dMat;
        Matrix tsimMat;
        double dtsim[][];
        int hsz;
        int sz;
        double f[][];
        double a[][] = null;
        double b[][] = null;
        double c[][] = null;
        
        if (tsim != null) {
    		usechi2 = false;
    	}
    	else {
    		usechi2 = true;
    	}
        
        radius = Math.max(1.0,  radius);
        theta = theta % Math.PI;
        for (y = 0; y < yDim; y++) {
        	for (x = 0; x < xDim; x++) {
        		if ((tmap[y][x] < 1) || (tmap[y][x] > ntex)) {
        			MipavUtil.displayError("texton label["+y+"]["+x+"] = " + tmap[y][x] + " is out of range");
        			return;
        		}
        	}
        }
        
        // Radius of discrete disc
        wr = (int)Math.floor(radius);
        
        // Count number of pixels in a disc
        xgrid = new double[2*wr+1][2*wr+1];
        ygrid = new double[2*wr+1][2*wr+1];
        for (y = -wr; y <= wr; y++) {
        	for (x = -wr; x <= wr; x++) {
        		ygrid[y+wr][x+wr] = y;
        		xgrid[y+wr][x+wr] = x;
        	}
        }
        gamma = new double[2*wr+1][2*wr+1];
        for (y = 0; y < 2*wr+1; y++) {
        	for (x = 0; x < 2*wr+1; x++) {
        		gamma[y][x] = Math.atan2(ygrid[y][x], xgrid[y][x]);
        	}
        }
        mask = new byte[2*wr+1][2*wr+1];
        for (y = 0; y < 2*wr+1; y++) {
        	for (x = 0; x < 2*wr+1; x++) {
        		if (xgrid[y][x]*xgrid[y][x] + ygrid[y][x]*ygrid[y][x] <= radius*radius) {
        			mask[y][x] = 1;
        		}
        	}
        }
        // Mask ot center pixel to remove bias
        mask[wr][wr] = 0;
        count = 0;
        for (y = 0; y < 2*wr+1; y++) {
        	for (x = 0; x < 2*wr+1; x++) {
        	    count += mask[y][x];	
        	}
        }
        
        // Determine which half of the disc pixels fall in
        // (0 = masked 1 = left 2 = right)
        sum1 = 0;
        sum2 = 0;
        side = new byte[2*wr+1][2*wr+1];
        for (y = 0; y < 2*wr+1; y++) {
        	for (x = 0; x < 2*wr+1; x++) {
        		if (((gamma[y][x] - theta)%(2.0 * Math.PI)) < Math.PI) {
        			side[y][x] = (byte)(2 *mask[y][x]);
        			if (side[y][x] == 2) {
        				sum2++;
        			}
        		}
        		else {
        			side[y][x] = mask[y][x];
        			if (side[y][x] == 1) {
        				sum1++;
        			}
        		}
        	}
        } // for (y = 0; y < 2*wr+1; y++)
        if (sum1 != sum2) {
        	MipavUtil.displayError("Sum imbalance in tgso sum1 = " + sum1 + " sum2 = " + sum2);
        	return;
        }
        lmask = new double[2*wr+1][2*wr+1];
        rmask = new double[2*wr+1][2*wr+1];
        for (y = 0; y < 2*wr+1; y++) {
        	for (x = 0; x < 2*wr + 1; x++) {
        		if (side[y][x] == 1) {
        			lmask[y][x] = 1.0/count * 2;
        		}
        		else if (side[y][x] == 2) {
        			rmask[y][x] = 1.0/count * 2;
        		}
        	}
        }
        
        // Compute tg using 2*ntex convolutions
        im = new double[yDim][xDim];
        tgL = new double[yDim][xDim];
        tgR = new double[yDim][xDim];
        if (usechi2) {
            for (i = 1; i <= ntex; i++) {
            	for (y = 0; y < yDim; y++) {
            		for (x = 0; x < xDim; x++) {
            	        if (tmap[y][x] == i) {
            	        	im[y][x] = 1.0;
            	        }
            	        else {
            	        	im[y][x] = 0.0;
            	        }
            		}
            	} // for (y = 0; y < yDim; y++)
            	conv2(im, lmask, tgL);
            	conv2(im, rmask, tgR);
            	for (y = 0; y < yDim; y++) {
            		for (x = 0; x < xDim; x++) {
            			diff = tgL[y][x] - tgR[y][x];
            			tg[y][x] = tg[y][x] + diff*diff/(tgL[y][x] + tgR[y][x] + epsilon);
            		}
            	}
            } // for ( i = 1; i <= ntex; i++)
            for (y = 0; y < yDim; y++) {
        		for (x = 0; x < xDim; x++) {
        			tg[y][x] = 0.5 * tg[y][x];
        		}
            }
        } // if (usechi2)
        else { // !usechi2
            d = new double[yDim*xDim][ntex];
            for (i = 1; i <= ntex; i++) {
            	for (y = 0; y < yDim; y++) {
            		for (x = 0; x < xDim; x++) {
            	        if (tmap[y][x] == i) {
            	        	im[y][x] = 1.0;
            	        }
            	        else {
            	        	im[y][x] = 0.0;
            	        }
            		}
            	} // for (y = 0; y < yDim; y++)
            	conv2(im, lmask, tgL);
            	conv2(im, rmask, tgR);
            	for (y = 0; y < yDim; y++) {
            		for (x = 0; x < xDim; x++) {
            			d[x + y * xDim][i] = Math.abs(tgL[y][x] - tgR[y][x]);
            		}
            	}
            } // for ( i = 1; i <= ntex; i++)
            dMat = new Matrix(d);
            tsimMat = new Matrix(tsim);
            dtsim = (dMat.times(tsimMat)).getArray();
            for (y = 0; y < xDim * yDim; y++) {
            	for (x = 0; x < ntex; x++) {
            		d[y][x] = dtsim[y][x] * d[y][x];
            	}
            }
            for (y = 0; y < yDim; y++) {
            	for (x = 0; x < xDim; x++) {
            		tg[y][x] = 0.0;
            	}
            }
            for (y = 0; y < yDim; y++) {
            	for (x = 0; x < xDim; x++) {
            		for (i = 0; i < ntex; i++) {
            			tg[y][x] = tg[y][x] + d[x + y * xDim][i];
            		}
            	}
            }
        } // else !usechi2
        
        if (smooth.equals("gaussian")) {
            hsz = (int)Math.max(Math.ceil(sigma * 3), Math.ceil(0.5 * 3));
            sz = 2 * hsz + 1;
            f = new double[sz][sz];
            oeFilter(f, sigma, 0.5, 3, theta + Math.PI/2.0);
            fbRun(tg, f, tg);
        } // if (smooth.equals("gaussian"))
        else if (smooth.equals("savgol")) {
        	a = new double[tg.length][tg[0].length];
            fitparab(a, b, c, tg, sigma, sigma/4.0, theta);  
            for (y = 0; y < yDim; y++) {
            	for (x = 0; x < xDim; x++) {
            		tg[y][x] = Math.max(0.0, a[y][x]);
            	}
            }
        } // else if (smooth.equals("savgol"))
    }
    
    private void fitparab(double a[][], double b[][], double c[][], double z[][], double ra, double rb, double theta) {
    	double ira2;
    	double irb2;
    	int wr;
    	double sint;
    	double cost;
    	int h;
    	int w;
    	double d0;
    	double d1;
    	double d2;
    	double d3;
    	double d4;
    	double v0;
    	double v1;
    	double v2;
    	int u;
    	int v;
    	int x;
    	int y;
    	int xi;
    	int yi;
    	double di;
    	double ei;
    	double zi;
    	double di2;
    	double detA;
    	double invA[][];
    	double param[];
    	// Fit cylindrical parabolas to elliptical patches of z at each pixel
    	
    	// Input
    	// z Values to fit
    	// ra, rb Radius of elliptical neighborhood, ra = major axis
    	// theta Orientation of fit (i.e. of minor axis).
    	
    	// Output
    	// a[][], b[][], c[][] Coefficients of fit: a + bx + cx^2
    	
    	ra = Math.max(1.5, ra);
    	rb = Math.max(1.5, rb);
    	ira2 = 1.0/(ra*ra);
    	irb2 = 1.0/(rb*rb);
    	wr = (int)Math.floor(Math.max(ra,rb));
    	sint = Math.sin(theta);
    	cost = Math.cos(theta);
    	
    	// Compute the interior quickly with convolutions
    	savgol(a, z, 2, 1, ra, rb, theta);
    	if (b != null) {
    		savgol(b, z, 2, 2, ra, rb, theta);	
    	}
    	if (c != null) {
    		savgol(c, z, 2, 3, ra, rb, theta);		
    	}
    	
    	// Recompute the border since the convolution screws it up
    	h = z.length;
    	w = z[0].length;
    	for (x = 1; x <= w; x++) {
    		for (y = 1; y <= h; y++) {
    			if ((x > wr) && (x <= w-wr) && (y > wr) && (y <= h-wr)) {
    				continue;
    			}
    			d0 = 0.0;
    			d1 = 0.0;
    			d2 = 0.0;
    			d3 = 0.0;
    			d4 = 0.0;
    			v0 = 0.0;
    			v1 = 0.0;
    			v2 = 0.0;
    			for (u = -wr; u <= wr; u++) {
    				xi = x + u;
    				if ((xi < 1) || (xi > w)) {
    					continue;
    				}
    				for (v = -wr; v <= wr; v++) {
    					yi = y + v;
    					if ((yi < 1) || (yi > h)) {
    						continue;
    					}
    					// Distance along major axis
    					di = -u*sint + v*cost;
    					// Distance along minor axis (at theta)
    					ei = u * cost + v * sint;
    					if (di*di*ira2 + ei*ei*irb2 >1) {
    						continue;
    					}
    					zi = z[yi-1][xi-1];
    					di2 = di*di;
    					d0 = d0 + 1;
    					d1 = d1 + di;
    					d2 = d2 + di2;
    					d3 = d3 + di*di2;
    					d4 = d4 + di2*di2;
    					v0 = v0 + zi;
    					v1 = v1 + zi*di;
    					v2 = v2 + zi*di2;
    				}
    			}
    			
    			// Much faster to do 3x3 matrix inverse manually
    			detA = -d2*d2*d2 + 2*d1*d2*d3 - d0*d3*d3 - d1*d1*d4 + d0*d2*d4;
    			invA = new double[3][3];
    			invA[0][0] = -d3*d3+d2*d4;
    			invA[0][1] = d2*d3-d1*d4;
    			invA[0][2] = -d2*d2+d1*d3;
    			invA[1][0] = d2*d3-d1*d4;
    			invA[1][1] = -d2*d2+d0*d4;
    			invA[1][2] = d1*d2-d0*d3;
    			invA[2][0] = -d2*d2+d1*d3;
    			invA[2][1] = d1*d2-d0*d3;
    			invA[2][2] = -d1*d1+d0*d2;
    			for (y = 0; y < 3; y++) {
    				for (x = 0; x < 3; x++) {
    					invA[y][x] = invA[y][x]/(detA + epsilon);
    				}
    			}
    			param = new double[3];
    			param[0] = invA[0][0]*v0 + invA[0][1]*v1 + invA[0][2]*v2;
    			a[y-1][x-1] = param[0];
    			if (b != null) {
    				param[1] = invA[1][0]*v0 + invA[1][1]*v1 + invA[1][2]*v2;
        			b[y-1][x-1] = param[1];	
    			}
    			if (c != null) {
    				param[2] = invA[2][0]*v0 + invA[2][1]*v1 + invA[2][2]*v2;
        			c[y-1][x-1] = param[2];	
    			}
    		}
    	}
    }
    
    private void savgol(double c[][], double z[][], int d, int k, double ra, double rb, double theta) {
        // Directional 2D Savitsky-Golay filtering with elliptical support.
    	// The computation is done with a convolution, so the boundary of the output will be biased.
    	// The boundary is of size floor(max(ra,rb)).
    	
    	// Input
    	// z Values to fit
    	// d Degree of fit, usually 2 or 4.
    	// k Coefficient to return in [1,d+1], 1 for smoothing.
    	// ra, rb Radius of elliptical neighborhood, ra = major axis.
    	// theta Orientation of fit (1.e. of minor axis).
    	
    	// Output 
    	// c[0] COefficient of fit
    	double ira2;
    	double irb2;
    	int wr;
    	int wd;
    	double sint;
    	double cost;
    	double filt[][][];
    	double filtk[][];
    	double xx[];
    	int u;
    	int v;
    	double ai;
    	double bi;
    	double A[][];
    	double yy[][];
    	int i;
    	int j;
    	Matrix matA;
    	Matrix matyy;
    	double prod[][];
    	int x;
    	int y;
    	
    	if (d < 0) {
    		MipavUtil.displayError("d = " + d + " is invalid in savgol");
    		return;
    	}
    	if ((k < 1) || ( k > d+1)) {
    		MipavUtil.displayError("k = " + k + " is invalid in savgol");
    		return;
    	}
    	
    	ra = Math.max(1.5, ra);
    	rb = Math.max(1.5, rb);
    	ira2 = 1.0/(ra*ra);
    	irb2 = 1.0/(rb*rb);
    	wr = (int)Math.floor(Math.max(ra,rb));
    	wd = 2*wr+1;
    	sint = Math.sin(theta);
    	cost = Math.cos(theta);
    	
    	// 1. Compute linear filters for coefficients
    	// (a) Compute inverse of least-squares problem matrix
    	filt = new double[wd][wd][d+1];
    	xx = new double[2*d+1];
    	for (u = -wr; u <= wr; u++) {
    		for (v = -wr; v <= wr; v++) {
    			// Distance along major axis
    			ai = -u*sint + v*cost;
    			// Distance along minor axis
    			bi = u*cost + v*sint;
    			if (ai*ai*ira2 + bi*bi*irb2 > 1) {
    				continue;
    			}
    			xx[0] = xx[0] + 1;
    			for (i = 1; i <= 2*d; i++) {
    			    xx[i] = xx[i] + Math.pow(ai,i);	
    			}
    		}
    	}
    	A = new double[d+1][d+1];
    	for (i = 0; i <= d; i++) {
    		for (j = i; j <= i+d; j++) {
    		    A[j][i] = xx[j];	
    		}
    	}
    	matA = new Matrix(A);
    	A = (matA.inverse()).getArray();
    	// (b) solve least-squares problem for delta function at each pixel
    	for (u = -wr; u <= wr; u++) {
    		for (v = -wr; v <= wr; v++) {
    		    yy = new double[d+1][1];
    		    // Distance along major axis
    		    ai = -u*sint + v*cost;
    		    // Distance along minor axis
    		    bi = u*cost + v*sint;
    		    if (ai*ai*ira2 + bi*bi*irb2 > 1) {
    		    	continue;
    		    }
		    	yy[0][0] = 1;
		    	for (i = 1; i <= d; i++) {
		    		yy[i][0] = Math.pow(ai,i); 
		    	}
		    	matA = new Matrix(A);
		    	matyy = new Matrix(yy);
		    	prod = (matA.times(matyy)).getArray();
		    	for (i = 0; i < d+1; i++) {
		    		filt[v+wr][u+wr][i] = prod[i][0];
		    	}
    		}
    	}
    	// 2. Apply the filter to get te fit coefficient at each pixel
    	filtk = new double[wd][wd];
    	for (y = 0; y < wd; y++) {
    		for (x = 0; x < wd; x++) {
    			filtk[y][x] = filt[y][x][k-1];
    		}
    	}
    	conv2(z, filtk, c);
    }
    
    private void fbRun(double fim[][], double fb[][], double im[][]) {
        // Run a filterbank on an image with reflected boundary conditions
    	int maxsz;
    	int r;
    	double impad[][];
    	double fimpad[][];
    	int y;
    	int x;
    	maxsz = Math.max(fb.length, fb[0].length);
    	
    	// Pad the image
    	r = (int)Math.floor(maxsz/2);
    	impad = new double[im.length+2*r][im[0].length + 2*r];
    	padReflect(impad, im, r);
    	fimpad = new double[impad.length][impad[0].length];
    	if (fb.length < 50) {
    	    conv2(impad,fb,fimpad);	
    	}
    	else {
    	    fftconv2(fimpad, impad, fb);	
    	}
    	for (y = r; y < impad.length - r; y++) {
    	    for (x = r; x < impad[0].length - r; x++) {
    	    	fim[y-r][x-r] = fimpad[y][x];
    	    }
    	}
    }
    
    private void fftconv2(double fim[][], double im[][], double f[][]) {
    	double padf[][];
    	int r;
    	int y;
    	int x;
    	FFTUtility fft;
    	int i;
    	// Convolution using fft
    	
    	// Wrap the filter around the origin and pad with zeros
    	padf = new double[im.length][im[0].length];
    	r = (int)Math.floor(f.length/2);
    	for (y = r; y < f.length; y++) {
    		for (x = r; x < f[0].length; x++) {
    			padf[y-r][x-r] = f[y][x];
    		}
    	}
    	for (y = r+1; y < f.length; y++) {
    		for (x = 0; x < r; x++) {
    			padf[y - r - 1][im[0].length - r + x] = f[y][x];
    		}
    	}
    	for (y = 0; y < r; y++) {
    		for (x = r+1; x < f[0].length; x++) {
    			padf[im.length - r + y][x - r - 1] = f[y][x];
    		}
    	}
    	for (y = 0; y < r; y++) {
    		for (x = 0; x < r; x++) {
    			padf[im.length - r + y][im[0].length - r + x] = f[y][x];
    		}
    	}
    	
    	// Magic
    	double imFFT[] = new double[im.length * im[0].length];
    	double imFFTImag[] = new double[im.length * im[0].length];
    	double padfFFT[] = new double[im.length * im[0].length];
    	double padfFFTImag[] = new double[im.length * im[0].length];
    	double prod[] = new double[im.length * im[0].length];
    	double prodImag[] = new double[im.length * im[0].length];
    	for (y = 0; y < im.length; y++) {
    		for (x = 0; x < im[0].length; x++) {
    			imFFT[x + y * im[0].length] = im[y][x];
    			padfFFT[x + y * im[0].length] = padf[y][x];
    		}
    	}
    	fft = new FFTUtility(imFFT, imFFTImag, im.length, im[0].length, 1,
				-1, FFTUtility.FFT);
		fft.setShowProgress(false);
		fft.run();
		fft.finalize();
		fft = null;
		fft = new FFTUtility(imFFT, imFFTImag, 1, im.length, im[0].length,
				-1, FFTUtility.FFT);
		fft.setShowProgress(false);
		fft.run();
		fft.finalize();
		fft = null;
		
		fft = new FFTUtility(padfFFT, padfFFTImag, im.length, im[0].length, 1,
				-1, FFTUtility.FFT);
		fft.setShowProgress(false);
		fft.run();
		fft.finalize();
		fft = null;
		fft = new FFTUtility(padfFFT, padfFFTImag, 1, im.length, im[0].length,
				-1, FFTUtility.FFT);
		fft.setShowProgress(false);
		fft.run();
		fft.finalize();
		fft = null;
		
		for (i = 0; i < im.length * im[0].length; i++) {
			prod[i] = imFFT[i]*padfFFT[i] - imFFTImag[i]*padfFFTImag[i];
			prodImag[i] = imFFT[i]*padfFFTImag[i] + imFFTImag[i]*padfFFT[i];
		}
		// Inverse fft
		fft = new FFTUtility(prod, prodImag, im.length, im[0].length, 1,
				1, FFTUtility.FFT);
		fft.setShowProgress(false);
		fft.run();
		fft.finalize();
		fft = null;
		fft = new FFTUtility(prod, prodImag, 1, im.length, im[0].length,
				1, FFTUtility.FFT);
		fft.setShowProgress(false);
		fft.run();
		fft.finalize();
		fft = null;
		for (y = 0; y < im.length; y++) {
			for (x = 0; x < im[0].length; x++) {
				fim[y][x] = prod[x + y * im[0].length];
			}
		}
		return;
    }
    
    private void padReflect(double impad[][], double im[][], int r) {
    	// Pad an image with a border of size r, and reflect the image into the border
    	int x;
    	int y;
    	// Middle
    	for (y = 0; y < yDim; y++) {
    		for (x = 0; x < xDim; x++) {
    			impad[y + r][x + r] = im[y][x];
    		}
    	}
    	// Top
    	for (y = 0; y < r; y++) {
    		for (x = 0; x < xDim; x++) {
    			impad[r - y - 1][x + r] = im[y][x];
    		}
    	}
    	// Bottom
    	for (y = yDim - r; y < yDim; y++) {
    		for (x = 0; x < xDim; x++) {
    			impad[2*yDim + r - y - 1][x + r] = im[y][x];
    		}
    	}
    	// Left
    	for (y = 0; y < yDim; y++) {
    		for (x = 0; x < r; x++) {
    			impad[y+r][r - x - 1] = im[y][x];
    		}
    	}
    	// Right
    	for (y = 0; y < yDim; y++) {
    		for (x = xDim - r; x < xDim; x++) {
    			impad[y+r][2*xDim + r - x - 1] = im[y][x];
    		}
    	}
    	// Top-left
    	for (y = 0; y < r; y++) {
    		for (x = 0; x < r; x++) {
    			impad[r - y - 1][r - x - 1] = im[y][x];
    		}
    	}
    	// Top-right
    	for (y = 0; y < r; y++) {
    		for (x = xDim - r; x < xDim; x++) {
    			impad[r - y - 1][2*xDim + r - x - 1] = im[y][x];
    		}
    	}
    	// Bottom-left
    	for (y = yDim - r; y < yDim; y++) {
    		for (x = 0; x < r; x++) {
    			impad[2*yDim + r - y - 1][r - x - 1] = im[y][x];
    		}
    	}
    	// Bottom-right
    	for (y = yDim - r; y < yDim; y++) {
    		for (x = xDim - r; x < xDim; x++) {
    			impad[2*yDim + r - y - 1][2*xDim + r - x - 1] = im[y][x];
    		}
    	}
    }
    
    private void oeFilter(double f[][], double sigmaX, double sigmaY, int support, double theta) {
       int deriv = 0;
       boolean dohil = false;
       boolean dovis = false;
       oeFilter(f, sigmaX, sigmaY, support, theta, deriv, dohil, dovis);
    }
    
    /**
     * Compute unit L1- norm 2D filter.
     * The filter is a Gaussian in the x direction
     * The filter is a Gaussian derivative with optional Hilbert transform in the y direction.
     * The filter is zero-meaned if deriv > 0.
     * @param f output [sz][sz] square filter
     * @param sigmaX
     * @param sigmaY
     * @param support Make filter +/- this many sigma
     * @param theta Orientation of x axis, in radians
     * @param deriv Degree of y derivative, one of {0, 1, 2}.
     * @param dohil Do Hilbert transform in y direction?
     * @param dovis Visualization for debugging?
     */
    private void oeFilter(double f[][], double sigmaX, double sigmaY, int support, double theta,
    		int deriv, boolean dohil, boolean dovis) {
    	int hsz;
    	int sz;
    	int maxsamples;
    	int maxrate;
    	int frate;
    	int rate;
    	int samples;
    	double r;
    	double dom[];
    	double stepSize;
    	int i;
    	double sx[][];
    	double sy[][];
    	int x;
    	int y;
    	int mx[][];
    	int my[][];
    	int membership[][];
    	double su[][];
    	double sv[][];
    	double R;
    	int fsamples;
    	double fdom[];
    	double gap;
    	double fx[];
    	double fy[];
    	double denom;
    	int xi[][];
    	int yi[][];
    	double fprecursor[][];
    	int v;
    	double fsum;
    	double fmean;
    	double fsumabs;
    	int paddedfsamples;
    	double paddedfy[];
    	AlgorithmHilbertTransform ht;
    
    	if ((deriv < 0) || (deriv > 2)) {
    		MipavUtil.displayError("deriv = " + deriv + "in oeFilter");
    		return;
    	}
    	
    	// Calculate filter size, make sure it's odd
    	 hsz = (int)Math.max(Math.ceil(sigmaX * support), Math.ceil(sigmaY * support));
         sz = 2 * hsz + 1;
         
         // Sampling limits
         // Max samples in each dimension
         maxsamples = 1000;
         // Maximum sampling rate
         maxrate = 10;
         // Over-sampling rate for function evaluation
         frate = 10;
         
         // Calculate sampling rate and number of samples
         rate = (int)Math.min(maxrate,  Math.max(1, Math.floor(maxsamples/sz)));
         samples = sz * rate;
         
         // The 2D sampling grid
         r = Math.floor(sz/2.0) + 0.5 * (1.0 - 1.0/rate);
         dom = new double[samples];
         dom[0] = -r;
         dom[samples-1] = r;
         stepSize = (2.0*r)/(samples - 1.0);
         for (i = 1; i < samples-1; i++) {
             dom[i] = -r + i * stepSize;	 
         }
         sx = new double[samples][samples];
         sy = new double[samples][samples];
         for (y = 0; y < samples; y++) {
        	 for (x = 0; x < samples; x++) {
        		 sx[y][x] = dom[x];
        		 sy[y][x] = dom[y];
        	 }
         }
         mx = new int[samples][samples];
         my = new int[samples][samples];
         for (y = 0; y < samples; y++) {
        	 for (x = 0; x < samples; x++) {
        		 mx[y][x] = (int)Math.round(sx[y][x]);
        		 my[y][x] = (int)Math.round(sy[y][x]);
        	 }
         }
         membership = new int[samples][samples];
         for (y = 0; y < samples; y++) {
        	 for (x = 0; x < samples; x++) {
        	     membership[y][x] = (mx[y][x] + hsz + 1) + (my[y][x]+ hsz)*sz;	 
        	 }
         }
         
         // Rotate the 2D sampling grid by theta
         
         su = new double[samples][samples];
         sv = new double[samples][samples];
         for (y = 0; y < samples; y++) {
        	 for (x = 0; x < samples; x++) {
        		 su[y][x] = sx[y][x]*Math.sin(theta) + sy[y][x]*Math.cos(theta);
        		 sv[y][x] = sx[y][x]*Math.cos(theta) - sy[y][x]*Math.sin(theta);
        	 }
         }
         
         if (dovis) {
        	 
         } // if (dovis)
         
         // Evaluate the function separably on a finer grid
         // Radius of domain, enlarged by > sqrt(2)
         R = r * Math.sqrt(2.0) * 1.01;
         // Number of samples
         fsamples = (int)Math.ceil(R * rate * frate);
         // Must be odd
         fsamples = fsamples + ((fsamples+1)%2);
         // Domain for function evaluation
         fdom = new double[fsamples];
         fdom[0] = -R;
         fdom[fsamples-1] = R;
         // Distance between samples
         gap = (2.0*R)/(fsamples - 1.0);
         for (i = 1; i < fsamples-1; i++) {
             fdom[i] = -R + i * gap;	 
         } 
         
         // The function is a Gaussian in the x direction
         fx = new double[fsamples];
         denom = 2.0*sigmaX*sigmaX;
         for (i = 0; i < fsamples; i++) {
        	 fx[i] = Math.exp(-fdom[i]*fdom[i]/denom);
         }
         // .. and a Gaussian derivative in the y direction
         fy = new double[fsamples];
         denom = 2.0*sigmaY*sigmaY;
         for (i = 0; i < fsamples; i++) {
        	 fy[i] = Math.exp(-fdom[i]*fdom[i]/denom);
         }
         switch (deriv) {
         case 1:
        	 denom = sigmaY*sigmaY;
        	 for (i = 0; i < fsamples; i++) {
        		 fy[i] = fy[i] * (-fdom[i]/denom);
        	 }
        	 break;
         case 2:
        	 denom = sigmaY *sigmaY;
        	 for (i = 0; i < fsamples; i++) {
        		 fy[i] = fy[i] *(fdom[i]*fdom[i]/denom - 1.0);
        	 }
         } // switch(deriv)
         // an optional Hilbert transform
         if (dohil) {
        	 paddedfsamples = MipavMath.findMinimumPowerOfTwo(fsamples);
        	 paddedfy = new double[2 * paddedfsamples];
        	 for (i = 0; i < fsamples; i++) {
        		 paddedfy[2*i] = fy[i];
        	 }
        	 ht = new AlgorithmHilbertTransform(paddedfy, paddedfsamples);
        	 ht.run();
        	 for (i = 0; i < fsamples; i++) {
        		 fy[i] = paddedfy[2*i+1];
        	 }
         }
         
         // Evaluate the function with NN interpolation
         xi = new int[samples][samples];
         for (y = 0; y < samples; y++) {
        	 for (x = 0; x < samples; x++) {
        		 xi[y][x] =(int) Math.round(su[y][x]/gap) + (int)Math.floor(fsamples/2) + 1;
        	 }
         }
         yi = new int[samples][samples];
         for (y = 0; y < samples; y++) {
        	 for (x = 0; x < samples; x++) {
        		 yi[y][x] =(int) Math.round(sv[y][x]/gap) + (int)Math.floor(fsamples/2) + 1;
        	 }
         }
         fprecursor = new double[samples][samples];
         for (y = 0; y < samples; y++) {
        	 for (x = 0; x < samples; x++) {
        	     fprecursor[y][x] = fx[xi[y][x]] * fy[yi[y][x]]; 
        	 }
         }
         for (y = 0; y < samples; y++) {
        	 for (x = 0; x < samples; x++) {
        		 v = membership[y][x];
        		 if (v < 1) {
        			 continue;
        		 }
        		 if (v > sz*sz) {
        			 continue;
        		 }
        		 f[(v-1)/sz][(v-1)%sz] += fprecursor[y][x];
        	 }
         }
         
         // Zero mean
         if (deriv > 0) {
	         fsum = 0.0;
	         for (y = 0; y < sz; y++) {
	        	 for (x = 0; x < sz; x++) {
	        	     fsum += f[y][x];	 
	        	 }
	         }
	         fmean = fsum/(sz*sz);
	         for (y = 0; y < sz; y++) {
	        	 for (x = 0; x < sz; x++) {
	        		 f[y][x] -= fmean;
	        	 }
	         }
         } // if (deriv > 0)
         
         // Unit L1-norm
         fsumabs = 0.0;
         for (y = 0; y < sz; y++) {
        	 for (x = 0; x < sz; x++) {
        		 fsumabs += Math.abs(f[y][x]);
        	 }
         }
         if (fsumabs > 0.0) {
        	 for (y = 0; y < sz; y++) {
        		 for (x = 0; x < sz; x++) {
        			 f[y][x] = f[y][x]/fsumabs;
        		 }
        	 }
         }
    }
    
    private void conv2(double A[][], double B[][], double Cout[][]) {
		double L[][];
		double S[][];
		int ml;
		int nl;
		int ms;
		int ns;
		int y;
		int x;
		int y2;
		int x2;
		double C[][];
		double small;
		int yoff;
		int xoff;
		if (A.length * A[0].length >= B.length * B[0].length) {
			ml = A.length;
			nl = A[0].length;
			L = A;
			ms = B.length;
			ns = B[0].length;
			S = B;
		} else {
			ml = B.length;
			nl = B[0].length;
			L = B;
			ms = A.length;
			ns = A[0].length;
			S = A;
		}
		C = new double[ml + ms - 1][nl + ns - 1];
		for (y = 0; y < ms; y++) {
			for (x = 0; x < ns; x++) {
				small = S[y][x];
				if (small != 0.0) {
					for (y2 = 0; y2 < ml; y2++) {
						for (x2 = 0; x2 < nl; x2++) {
							C[y + y2][x + x2] += L[y2][x2] * small;
						}
					}
				}
			}
		}
		yoff = (int) Math.floor(B.length / 2.0);
		xoff = (int) Math.floor(B[0].length / 2.0);
		for (y = 0; y < A.length; y++) {
			for (x = 0; x < A[0].length; x++) {
				Cout[y][x] = C[y + yoff][x + xoff];
			}
		}
		return;
	}
    
    
}