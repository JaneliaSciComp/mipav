package gov.nih.mipav.model.file;


import gov.nih.mipav.model.file.FileInfoBase.Unit;
import gov.nih.mipav.model.structures.*;

import gov.nih.mipav.view.*;

import java.io.*;

import java.util.*;

import javax.swing.*;


/**
 * @author  William Gandler
 * @see     FileIO
 */

public class FileInterfile extends FileBase {

    //~ Static fields/initializers -------------------------------------------------------------------------------------

    /** DOCUMENT ME! */
    private static final int SIGNED_INTEGER = 1;

    /** DOCUMENT ME! */
    private static final int UNSIGNED_INTEGER = 2;

    /** DOCUMENT ME! */
    private static final int LONG_FLOAT = 3;

    /** DOCUMENT ME! */
    private static final int SHORT_FLOAT = 4;

    /** DOCUMENT ME! */
    private static final int BIT = 5;

    /** DOCUMENT ME! */
    private static final int ASCII = 6;

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** DOCUMENT ME! */
    private int bufferSize;

    /** DOCUMENT ME! */
    private int bytesPerPixel;

    /** DOCUMENT ME! */
    private long dataByteOffset;

    /** DOCUMENT ME! */
    private File dataFile;

    /** DOCUMENT ME! */
    private String dataFileName = null;
    
    private String headerFileName = null;

    /** DOCUMENT ME! */
    private int dataStartingBlock;

    /** DOCUMENT ME! */
    private boolean endianess;

    /** DOCUMENT ME! */
    private File file;

    /** DOCUMENT ME! */
    private String fileDir;

    /** DOCUMENT ME! */
    private FileInfoInterfile fileInfo;
    
    private FileInfoInterfile fileInfoCopy;

    /** DOCUMENT ME! */
    private long fileLength;

    /** DOCUMENT ME! */
    private String fileName;

    /** DOCUMENT ME! */
    private FileRawChunk fileRW;

    /** DOCUMENT ME! */
    private boolean foundEOF = false;

    /** DOCUMENT ME! */
    private boolean haveBytesPerPixel = false;

    /** DOCUMENT ME! */
    private boolean haveDataByteOffset = false;

    /** DOCUMENT ME! */
    private boolean haveDataStartingBlock = false;

    /** DOCUMENT ME! */
    private boolean haveDecayCorrected = false;

    /** DOCUMENT ME! */
    private boolean haveDetectorHeadNumber = false;

    /** DOCUMENT ME! */
    private boolean haveEndianess = false;

    /** DOCUMENT ME! */
    private boolean haveEnergyWindowsNumber = false;

    /** DOCUMENT ME! */
    private boolean haveFileName = false;

    /** DOCUMENT ME! */
    private boolean haveFloodCorrected = false;

    /** DOCUMENT ME! */
    private boolean haveImagesPerEWindow = false;

    /** DOCUMENT ME! */
    private boolean haveNumberFormat = false;

    /** DOCUMENT ME! */
    private boolean haveOrientation = false;

    /** DOCUMENT ME! */
    private boolean haveReconstructed = false;

    /** DOCUMENT ME! */
    private boolean haveReferenceFrameNumber = false;

    /** DOCUMENT ME! */
    private boolean haveResolsT = false;

    /** DOCUMENT ME! */
    private boolean haveResolsX = false;

    /** DOCUMENT ME! */
    private boolean haveResolsY = false;

    /** DOCUMENT ME! */
    private boolean haveResolsZ = false;

    /** DOCUMENT ME! */
    private boolean haveSliceThickness = false;

    @SuppressWarnings("unused")
    private boolean haveTDim = false;

    /** DOCUMENT ME! */
    private boolean haveTomographic = false;

    /** DOCUMENT ME! */
    private boolean haveTotalImageNumber = false;

    /** DOCUMENT ME! */
    private boolean haveXDim = false;

    /** DOCUMENT ME! */
    private boolean haveYDim = false;

    /** DOCUMENT ME! */
    private boolean haveZDim = false;

    /** DOCUMENT ME! */
    private ModelImage image;

    /** DOCUMENT ME! */
    private int imageOrientation;

    /** DOCUMENT ME! */
    private float[] imgBuffer = null;

    /** DOCUMENT ME! */
    private double[] imgDBuffer = null;


    /** DOCUMENT ME! */
    private int[] imgExtents;

    /** DOCUMENT ME! */
    private long[] imgLBuffer = null;

    /** DOCUMENT ME! */
    private float[] imgResols = new float[5];

    /** DOCUMENT ME! */
    private String isotopeNumber = null;

    /** DOCUMENT ME! */
    private ModelLUT LUT;

    /** DOCUMENT ME! */
    private int numberFormat;

    /** DOCUMENT ME! */
    private int numberSlices; // zDim for 3D and zDim * tDim for 4D

    /** DOCUMENT ME! */
    private int orientation;

    /** DOCUMENT ME! */
    private String originalFileName;

    /** DOCUMENT ME! */
    private int resXUnit, resYUnit, resZUnit;

    /** DOCUMENT ME! */
    private float scalingFactor1;

    /** DOCUMENT ME! */
    private float scalingFactor2;

    /** DOCUMENT ME! */
    private float scalingFactor3;

    /** DOCUMENT ME! */
    private float scalingFactor4;

    /** DOCUMENT ME! */
    private int tDim = 1;

    /** DOCUMENT ME! */
    private String timeFrame;

    /** DOCUMENT ME! */
    private int totalImageNumber;

    /** DOCUMENT ME! */
    private int type;

    /** DOCUMENT ME! */
    private String windowNumber = null;


    /** DOCUMENT ME! */
    private int xDim = 0;

    /** DOCUMENT ME! */
    private float xResol, yResol, zResol;

    /** DOCUMENT ME! */
    private int yDim = 0;

    /** DOCUMENT ME! */
    private int zDim = 1;
    
    @SuppressWarnings("unused")
    private boolean haveStaticStudy = false;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Interfile reader/writer constructor.
     *
     * @param      originalFileName  file name
     * @param      fileDir           file directory
     *
     * @exception  IOException  if there is an error making the file
     */
    public FileInterfile(String originalFileName, String fileDir) throws IOException {

        this.originalFileName = originalFileName;
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
        fileInfoCopy = null;
        image = null;
        dataFile = null;
        dataFileName = null;
        headerFileName = null;
        imgBuffer = null;
        imgDBuffer = null;
        imgExtents = null;
        imgLBuffer = null;
        imgResols = null;
        isotopeNumber = null;
        LUT = null;
        originalFileName = null;
        timeFrame = null;
        windowNumber = null;
        
        if (fileRW != null) {

            try {
                fileRW.close();
                // System.err.println("closed FileInterfile: fileRW (FileRawChunk)");
            } catch (IOException ex) { }

            fileRW.finalize();
        }

        fileRW = null;

        try {
            super.finalize();
        } catch (Throwable er) { }
    }
    
    /**
     * Method to test to determine if the image format is Cheshire, so appropriate read method may be called.
     *
     * @param   fName  File name of image.
     * @param   fDir   Directory.
     *
     * @return  <code>true</code> if Cheshire file, <code>false</code> otherwise.
     */
    public static String isInterfile(String fName, String fDir) {
       
        try {
            RandomAccessFile raFile;
            int index = fName.length();

            for (int i = fName.length() - 1; i >= 0; i--) {

                if (fName.charAt(i) == '.') {
                    index = i;

                    break;
                }
            }

            String fileHeaderName = fName.substring(0, index) + ".hdr";
            File fileHeader = new File(fDir + fileHeaderName);

            if (fileHeader.exists() == false) {
                fileHeaderName = fName.substring(0, index) + ".HDR";
                fileHeader = new File(fDir + fileHeaderName);

                if (fileHeader.exists() == false) {
                    fileHeaderName = fName;
                    fileHeader = new File(fDir + fileHeaderName);
                    if (fileHeader.exists()  == false) {
                    	Preferences.debug("FileInterfile: Error reading file: File not found - " +
                    			fDir + File.separator + fName + "\n", Preferences.DEBUG_FILEIO);	
                    	return null;
                    }
                }
            }

            raFile = new RandomAccessFile(fileHeader, "r");
            if (raFile.length() == 0) {
            	// Needed because raFile.read returns null without an IOException if the EOF
            	// is encountered before even one byte is read.
            	Preferences.debug("FileInterfile: Error reading file: File is zero length - " +
            			fDir + File.separator + fName + "\n", Preferences.DEBUG_FILEIO);	
            	return null;	
            }

            // Check that this is an Interfile file
            String tempString = null;
            boolean foundEOF = false;

            while ((tempString == null) && !foundEOF) {

                try {
                    tempString = raFile.readLine();
                } catch (IOException error) {
                    tempString = null;
                    foundEOF = true;
                } 

                if (tempString != null) {

                    index = tempString.indexOf(";");

                    if (index != -1) {
                        tempString = tempString.substring(0, index);
                    }

                    tempString = tempString.trim();
                }

                if (tempString != null) {

                    if (tempString.length() == 0) {
                        tempString = null;
                    }
                }
            }

            raFile.close();

            if (tempString == null) {
                return null;
            }

            String removedString = removeChars(tempString);
            String keyString = returnKey(removedString);

            if ((keyString == null) || !keyString.equalsIgnoreCase("INTERFILE")) {
                return null;
            } else {
                return fileHeaderName;
            }
        } catch (FileNotFoundException e) {
            // do not display a hard error in the is*() methods
            // MipavUtil.displayError("FileInterfile: Error reading file.");
            Preferences.debug("FileInterfile: Error reading file: File not found - " + fDir + File.separator + fName + "\n", Preferences.DEBUG_FILEIO);
        } catch (IOException e) {
            // do not display a hard error in the is*() methods
            // MipavUtil.displayError("FileInterfile: Error reading file.");
            Preferences.debug("FileInterfile: Error reading file - " + fDir + File.separator + fName + "\n", Preferences.DEBUG_FILEIO);
        }

        return null;

    }

    /**
     * Returns the file info.
     *
     * @return  FileInfoBase containing the file info
     */
    public FileInfoInterfile getFileInfo() {
        return fileInfo;
    }


    /**
     * Returns the image buffer.
     *
     * @return  buffer of image.
     */
    public float[] getImageBuffer() {
        return imgBuffer;
    }


    /**
     * Returns LUT if defined.
     *
     * @return  the LUT if defined else it is null
     */
    public ModelLUT getModelLUT() {
        return LUT;
    }

    /**
     * Reads the image.
     *
     * @param      one  Flag for only reading one image of dataset.
     * @param      readData
     *
     * @return     returns the image
     *
     * @exception  IOException  if there is an error reading the file
     */
    public ModelImage readImage(boolean one, boolean readData) throws IOException {

        int i;
        String lineString;
        String removedString;
        String keyString;
        String valueString;
        String removedValueString;
        int index;
        int energyWindowsNumber = 1;
        int imagesPerEWindow = 1;

        try {
            index = originalFileName.length();

            for (i = originalFileName.length() - 1; i >= 0; i--) {

                if (originalFileName.charAt(i) == '.') {
                    index = i;

                    break;
                }
            }

            String fileHeaderName = originalFileName.substring(0, index) + ".hdr";
            File fileHeader = new File(fileDir + fileHeaderName);

            if (fileHeader.exists() == false) {
                fileHeaderName = originalFileName.substring(0, index) + ".HDR";
                fileHeader = new File(fileDir + fileHeaderName);

                if (fileHeader.exists() == false) {
                    fileHeaderName = originalFileName;
                    fileHeader = new File(fileDir + fileHeaderName);
                }
            }
          
            raFile = new RandomAccessFile(fileHeader, "r");
            fileLength = raFile.length();

            fileInfo = new FileInfoInterfile(originalFileName, fileDir, FileUtility.INTERFILE); // dummy fileInfo

            imgResols[0] = 1.0f;
            imgResols[1] = 1.0f;

            // Check that this is an Interfile file
            lineString = readLine();

            if (lineString == null) {
                raFile.close();
                throw new IOException("The file had no uncommented lines");
            }

            removedString = removeChars(lineString);
            keyString = returnKey(removedString);

            if ((keyString == null) || !keyString.equalsIgnoreCase("INTERFILE")) {
                raFile.close();
                throw new IOException("Required INTERFILE key not found at start of file");
            }

            while (lineString != null) {
                lineString = readLine();

                if (lineString != null) {
                    removedString = removeChars(lineString);
                    keyString = returnKey(removedString);
                    valueString = returnValue(lineString);
                    removedValueString = returnValue(removedString);

                    if (keyString.equalsIgnoreCase("DATAOFFSETINBYTES")) {
                        dataByteOffset = Long.valueOf(valueString).longValue();
                        haveDataByteOffset = true;
                    } else if (keyString.equalsIgnoreCase("DATASTARTINGBLOCK")) {
                        dataStartingBlock = Integer.valueOf(valueString).intValue();
                        haveDataStartingBlock = true;
                    } else if (keyString.equalsIgnoreCase("NAMEOFDATAFILE")) {
                        fileName = valueString;
                        haveFileName = true;
                    } else if (keyString.equalsIgnoreCase("TOTALNUMBEROFIMAGES")) {
                        totalImageNumber = Integer.valueOf(valueString).intValue();
                        haveTotalImageNumber = true;
                        fileInfo.setTotalImageNumber(valueString);
                    } else if (keyString.equalsIgnoreCase("IMAGEDATABYTEORDER")) {

                        if (valueString.equalsIgnoreCase("BIGENDIAN")) {
                            endianess = FileBase.BIG_ENDIAN;
                            haveEndianess = true;
                        } else if (valueString.equalsIgnoreCase("LITTLEENDIAN")) {
                            endianess = FileBase.LITTLE_ENDIAN;
                            haveEndianess = true;
                        } else {
                            raFile.close();
                            MipavUtil.displayError("Impossible value of " + valueString + "for image data byte order");
                            throw new IOException();
                        }
                    } // else if (keyString.equalsIgnoreCase("IMAGEDATABYTEORDER"))
                    else if ((keyString.equalsIgnoreCase("MATRIXSIZE[1]")) ||
                             (keyString.equalsIgnoreCase("MATRIXSIZE(X)"))) {
                        xDim = Integer.valueOf(valueString).intValue();
                        haveXDim = true;
                        fileInfo.setMatrixSize1(valueString);
                    } else if ((keyString.equalsIgnoreCase("MATRIXSIZE[2]")) ||
                               (keyString.equalsIgnoreCase("MATRIXSIZE(Y)"))) {
                        yDim = Integer.valueOf(valueString).intValue();
                        haveYDim = true;
                        fileInfo.setMatrixSize2(valueString);
                    } else if ((keyString.equalsIgnoreCase("MATRIXSIZE[3]"))||
                               (keyString.equalsIgnoreCase("MATRIXSIZE(Z)"))) {
                        zDim = Integer.valueOf(valueString).intValue();
                        haveZDim = true;
                    } else if ((keyString.equalsIgnoreCase("MATRIXSIZE[4]")) ||
                                   (keyString.equalsIgnoreCase("MATRIXSIZE(T)")) ||
                                   (keyString.equalsIgnoreCase("NUMBEROFTIMEFRAMES"))) {
                        tDim = Integer.valueOf(valueString).intValue();
                        haveTDim = true;

                        if (keyString.equalsIgnoreCase("NUMBEROFTIMEFRAMES")) {
                            fileInfo.setTimeFrames(valueString);
                        }
                    } else if (keyString.equalsIgnoreCase("NUMBERFORMAT")) {

                        if ((removedValueString.equalsIgnoreCase("SIGNEDINTEGER")) ||
                            (removedValueString.equalsIgnoreCase("SIGNED"))) {
                            numberFormat = SIGNED_INTEGER;
                            haveNumberFormat = true;
                        } else if ((removedValueString.equalsIgnoreCase("UNSIGNEDINTEGER")) ||
                                   (removedValueString.equalsIgnoreCase("UNSIGNED"))) {
                            numberFormat = UNSIGNED_INTEGER;
                            haveNumberFormat = true;
                        } else if (removedValueString.equalsIgnoreCase("LONGFLOAT")) {
                            numberFormat = LONG_FLOAT;
                            haveNumberFormat = true;
                        } else if (removedValueString.equalsIgnoreCase("SHORTFLOAT")) {
                            numberFormat = SHORT_FLOAT;
                            haveNumberFormat = true;
                        } else if (removedValueString.equalsIgnoreCase("BIT")) {
                            numberFormat = BIT;
                            haveNumberFormat = true;
                        } else if (removedValueString.equalsIgnoreCase("ASCII")) {
                            numberFormat = ASCII;
                            haveNumberFormat = true;
                        } else {
                            raFile.close();
                            MipavUtil.displayError("Number format has illegal value of " + valueString);
                            throw new IOException();
                        }

                        fileInfo.setNumberFormat(valueString);
                    } // else if (keyString.equalsIgnoreCase("NUMBERFORMAT"))
                    else if (keyString.equalsIgnoreCase("NUMBEROFBYTESPERPIXEL")) {
                        bytesPerPixel = Integer.valueOf(valueString).intValue();
                        haveBytesPerPixel = true;
                        fileInfo.setBytesPerPixel(valueString);
                    } else if (keyString.equalsIgnoreCase("NUMBEROFDIMENSIONS")) {
                        fileInfo.setDimensionNumber(valueString);
                    } else if (keyString.equalsIgnoreCase("SCALINGFACTOR(MM/PIXEL)[1]")) {
                        scalingFactor1 = Float.valueOf(valueString).floatValue();
                        haveResolsX = true;
                        fileInfo.setScalingFactor1(valueString);
                    } else if (keyString.equalsIgnoreCase("SCALINGFACTOR(MM/PIXEL)[2]")) {
                        scalingFactor2 = Float.valueOf(valueString).floatValue();
                        haveResolsY = true;
                        fileInfo.setScalingFactor2(valueString);
                    } else if (keyString.equalsIgnoreCase("SCALINGFACTOR(MM/PIXEL)[3]")) {
                        scalingFactor3 = Float.valueOf(valueString).floatValue();
                        haveResolsZ = true;
                    } else if (keyString.equalsIgnoreCase("SCALINGFACTOR(MM/PIXEL)[4]")) {
                        scalingFactor4 = Float.valueOf(valueString).floatValue();
                        haveResolsT = true;
                    } else if (keyString.equalsIgnoreCase("SLICEORIENTATION")) {

                        if (valueString.equalsIgnoreCase("TRANSVERSE")) {
                            orientation = FileInfoBase.AXIAL;
                            haveOrientation = true;
                        } else if (valueString.equalsIgnoreCase("CORONAL")) {
                            orientation = FileInfoBase.CORONAL;
                            haveOrientation = true;
                        } else if (valueString.equalsIgnoreCase("SAGITTAL")) {
                            orientation = FileInfoBase.SAGITTAL;
                            haveOrientation = true;
                        } else if (valueString.equalsIgnoreCase("OTHER")) {
                            orientation = FileInfoBase.UNKNOWN_ORIENT;
                            haveOrientation = true;
                        } else {
                            raFile.close();
                            MipavUtil.displayError("Slice orientation has illegal value of " + valueString);
                            throw new IOException();
                        }
                        fileInfo.setSliceOrientation(valueString);
                    } // else if (keyString.equalsIgnoreCase("SLICEORIENTATION"))
                    else if (keyString.equalsIgnoreCase("IMAGINGMODALITY")) {
                        fileInfo.setImagingModality(valueString);
                    } else if (keyString.equalsIgnoreCase("ORIGINATINGSYSTEM")) {
                        fileInfo.setOriginatingSystem(valueString);
                    } else if (keyString.equalsIgnoreCase("VERSIONOFKEYS")) {
                        fileInfo.setKeysVersion(valueString);
                    } else if (keyString.equalsIgnoreCase("DATEOFKEYS")) {
                        fileInfo.setKeysDate(valueString);
                    } else if (keyString.equalsIgnoreCase("CONVERSIONPROGRAM")) {
                        fileInfo.setConversionProgram(valueString);
                    } else if (keyString.equalsIgnoreCase("PROGRAMAUTHOR")) {
                        fileInfo.setProgramAuthor(valueString);
                    } else if (keyString.equalsIgnoreCase("PROGRAMVERSION")) {
                        fileInfo.setProgramVersion(valueString);
                    } else if (keyString.equalsIgnoreCase("PROGRAMDATE")) {
                        fileInfo.setProgramDate(valueString);
                    } else if (keyString.equalsIgnoreCase("GENERALDATA")) {
                        fileInfo.setGeneralData(true);
                    } else if (keyString.equalsIgnoreCase("ORIGINALINSTITUTION")) {
                        fileInfo.setOriginalInstitution(valueString);
                    } else if (keyString.equalsIgnoreCase("CONTACTPERSON")) {
                        fileInfo.setContactPerson(valueString);
                    } else if (keyString.equalsIgnoreCase("DATADESCRIPTION")) {
                        fileInfo.setDataDescription(valueString);
                    } else if (keyString.equalsIgnoreCase("PATIENTNAME")) {
                        fileInfo.setPatientName(valueString);
                    } else if (keyString.equalsIgnoreCase("PATIENTID")) {
                        fileInfo.setPatientID(valueString);
                    } else if (keyString.equalsIgnoreCase("PATIENTDOB")) {
                        fileInfo.setPatientDOB(valueString);
                    } else if (keyString.equalsIgnoreCase("PATIENTSEX")) {

                        if ((valueString == null) || (valueString.equalsIgnoreCase("M")) ||
                                (valueString.equalsIgnoreCase("F")) || (valueString.equalsIgnoreCase("UNKNOWN"))) {
                            fileInfo.setPatientSex(valueString);
                        } else {
                            raFile.close();
                            MipavUtil.displayError("Patient sex has impossible value of " + valueString);
                            throw new IOException();
                        }
                    } // else if (keyString.equalsIgnoreCase("PATIENTSEX"))
                    else if (keyString.equalsIgnoreCase("STUDYID")) {
                        fileInfo.setStudyID(valueString);
                    } else if (keyString.equalsIgnoreCase("EXAMTYPE")) {
                        fileInfo.setExamType(valueString);
                    } else if ((keyString.equalsIgnoreCase("DATACOMPRESSION")) && (valueString !=  null)) {
                        fileInfo.setDataCompression(valueString);

                        if (!valueString.equalsIgnoreCase("NONE")) {
                            MipavUtil.displayWarning("Data compression = " + valueString);
                        }
                    } else if ((keyString.equalsIgnoreCase("DATAENCODE")) && (valueString != null)) {
                        fileInfo.setDataEncode(valueString);

                        if (!valueString.equalsIgnoreCase("NONE")) {
                            MipavUtil.displayWarning("Data encode = " + valueString);
                        }
                    } else if (keyString.equalsIgnoreCase("GENERALIMAGEDATA")) {
                        fileInfo.setGeneralImageData(true);
                    } else if (keyString.equalsIgnoreCase("TYPEOFDATA")) {

                        if ((valueString.equalsIgnoreCase("STATIC")) ||
                                (valueString.equalsIgnoreCase("STATIC STUDY")) ||
                                (valueString.equalsIgnoreCase("DYNAMIC")) ||
                                (valueString.equalsIgnoreCase("DYNAMIC STUDY")) ||
                                (valueString.equalsIgnoreCase("GATED")) ||
                                (valueString.equalsIgnoreCase("GATED STUDY")) ||
                                (valueString.equalsIgnoreCase("TOMOGRAPHIC")) ||
                                (valueString.equalsIgnoreCase("TOMOGRAPHIC STUDY")) ||
                                (valueString.equalsIgnoreCase("CURVE")) || (valueString.equalsIgnoreCase("ROI")) ||
                                (valueString.equalsIgnoreCase("PET")) || (valueString.equalsIgnoreCase("OTHER"))) {
                            if (valueString.equalsIgnoreCase("STATIC STUDY")) {
                                fileInfo.setInterfileDataType("STATIC");
                            }
                            else if (valueString.equalsIgnoreCase("DYNAMIC STUDY")) {
                                fileInfo.setInterfileDataType("DYNAMIC");
                            }
                            else if (valueString.equalsIgnoreCase("GATED STUDY")) {
                                fileInfo.setInterfileDataType("GATED");
                            }
                            else if (valueString.equalsIgnoreCase("TOMOGRAPHIC STUDY")) {
                                fileInfo.setInterfileDataType("TOMOGRAPHIC");
                            }
                            else {
                                fileInfo.setInterfileDataType(valueString);
                            }
                            
                            if ((valueString.equalsIgnoreCase("STATIC")) || 
                                (valueString.equalsIgnoreCase("STATIC STUDY")) ||
                                (valueString.equalsIgnoreCase("ROI"))) {
                                haveStaticStudy = true;
                            }

                            if ((valueString.equalsIgnoreCase("TOMOGRAPHIC")) ||
                                (valueString.equalsIgnoreCase("TOMOGRAPHIC STUDY"))) {
                                haveTomographic = true;
                            }
                        } else {
                            raFile.close();
                            MipavUtil.displayError("Type of data has an illegal value of " + valueString);
                            throw new IOException();
                        }
                    } // else if (keyString.equalsIgnoreCase("TYPEOFDATA"))
                    else if (keyString.equalsIgnoreCase("STUDYDATE")) {
                        fileInfo.setStudyDate(valueString);
                    } else if (keyString.equalsIgnoreCase("STUDYTIME")) {
                        fileInfo.setStudyTime(valueString);
                    } else if (keyString.equalsIgnoreCase("NUMBEROFISOTOPES")) {
                        fileInfo.setIsotopeNumber(valueString);
                    } else if ((keyString.length() >= 14) &&
                                   (keyString.substring(0, 12).equalsIgnoreCase("ISOTOPENAME["))) {
                        index = keyString.indexOf("]");

                        if (index == -1) {
                            raFile.close();
                            MipavUtil.displayError("] not found in isotope name [ keystring");
                            throw new IOException();
                        }

                        isotopeNumber = keyString.substring(12, index);
                        fileInfo.setIsotopeName(isotopeNumber, valueString);
                    } else if (keyString.equalsIgnoreCase("ISOTOPENAME")) {
                        fileInfo.setIsotopeName("1", valueString);
                    } else if ((keyString.length() >= 27) &&
                                   (keyString.substring(0, 25).equalsIgnoreCase("ISOTOPEBETAHALFLIFE(SEC)["))) {
                        index = keyString.indexOf("]");

                        if (index == -1) {
                            raFile.close();
                            MipavUtil.displayError("] not found in isotope beta halflife (sec) [ keystring");
                            throw new IOException();
                        }

                        isotopeNumber = keyString.substring(25, index);
                        fileInfo.setBetaHalflife(isotopeNumber, valueString);
                    } else if (keyString.equalsIgnoreCase("ISOTOPEBETAHALFLIFE(SEC)")) {
                        fileInfo.setBetaHalflife("1", valueString);
                    } else if ((keyString.length() >= 28) &&
                                   (keyString.substring(0, 26).equalsIgnoreCase("ISOTOPEGAMMAHALFLIFE(SEC)["))) {
                        index = keyString.indexOf("]");

                        if (index == -1) {
                            raFile.close();
                            MipavUtil.displayError("] not found in isotope gamma halflife (sec) [ keystring");
                            throw new IOException();
                        }

                        isotopeNumber = keyString.substring(26, index);
                        fileInfo.setGammaHalflife(isotopeNumber, valueString);
                    } else if (keyString.equalsIgnoreCase("ISOTOPEGAMMAHALFLIFE(SEC)")) {
                        fileInfo.setGammaHalflife("1", valueString);
                    } else if ((keyString.length() >= 25) &&
                                   (keyString.substring(0, 23).equalsIgnoreCase("ISOTOPEBRANCHINGFACTOR["))) {
                        index = keyString.indexOf("]");

                        if (index == -1) {
                            raFile.close();
                            MipavUtil.displayError("] not found in isotope branching factor [ keystring");
                            throw new IOException();
                        }

                        isotopeNumber = keyString.substring(23, index);
                        fileInfo.setBranchingFactor(isotopeNumber, valueString);
                    } else if (keyString.equalsIgnoreCase("ISOTOPEBRANCHINGFACTOR")) {
                        fileInfo.setBranchingFactor("1", valueString);
                    } else if (keyString.equalsIgnoreCase("RADIOPHARMACEUTICAL")) {
                        fileInfo.setRadiopharmaceutical(valueString);
                    } else if ((keyString.equalsIgnoreCase("NUMBEROFENERGYWINDOWS")) ||
                               (keyString.equalsIgnoreCase("NUMBEROFWINDOWS"))) {
                        fileInfo.setEnergyWindowsNumber(valueString);
                        haveEnergyWindowsNumber = true;
                        energyWindowsNumber = Integer.valueOf(valueString).intValue();
                    } else if ((keyString.length() >= 15) &&
                                   (keyString.substring(0, 13).equalsIgnoreCase("ENERGYWINDOW["))) {
                        index = keyString.indexOf("]");

                        if (index == -1) {
                            raFile.close();
                            MipavUtil.displayError("] not found in energy window keystring");
                            throw new IOException();
                        }

                        windowNumber = keyString.substring(13, index);
                        fileInfo.setEnergyWindow(windowNumber, valueString);
                    } // else if ((keyString.length() >= 15) &&
                      // (keyString.substring(0,13).equalsIgnoreCase("ENERGYWINDOW[")))
                    else if ((keyString.length() >= 25) &&
                                 (keyString.substring(0, 23).equalsIgnoreCase("ENERGYWINDOWLOWERLEVEL["))) {
                        index = keyString.indexOf("]");

                        if (index == -1) {
                            raFile.close();
                            MipavUtil.displayError("] not found in energy window lower level keystring");
                            throw new IOException();
                        }

                        windowNumber = keyString.substring(23, index);
                        fileInfo.setEnergyWindowLowerLevel(windowNumber, valueString);
                    } // else if ((keyString.length() >= 25) &&
                      // (keyString.substring(0,23).equalsIgnoreCase("ENERGYWINDOWLOWERLEVEL[")))
                    else if ((keyString.length() >= 25) &&
                                 (keyString.substring(0, 23).equalsIgnoreCase("ENERGYWINDOWUPPERLEVEL["))) {
                        index = keyString.indexOf("]");

                        if (index == -1) {
                            raFile.close();
                            MipavUtil.displayError("] not found in energy window upper level keystring");
                            throw new IOException();
                        }

                        windowNumber = keyString.substring(23, index);
                        fileInfo.setEnergyWindowUpperLevel(windowNumber, valueString);
                    } // else if ((keyString.length() >= 25) &&
                      // (keyString.substring(0,23).equalsIgnoreCase("ENERGYWINDOWUPPERLEVEL[")))
                    else if (keyString.equalsIgnoreCase("WINDOWA")) {
                        fileInfo.setWindowA(valueString);
                    }
                    else if (keyString.equalsIgnoreCase("WINDOWB")) {
                        fileInfo.setWindowB(valueString);
                    }
                    else if (keyString.equalsIgnoreCase("WINDOWC")) {
                        fileInfo.setWindowC(valueString);
                    }
                    else if (keyString.equalsIgnoreCase("FLOODCORRECTED")) {

                        if ((valueString.equalsIgnoreCase("Y")) || (valueString.equalsIgnoreCase("N"))) {
                            fileInfo.setFloodCorrected(valueString);
                            haveFloodCorrected = true;
                        } else {
                            raFile.close();
                            MipavUtil.displayError("Flood corrected has an illegal value of " + valueString);
                            throw new IOException();
                        }
                    } // else if (keyString.equalsIgnoreCase("FLOODCORRECTED"))
                    else if (keyString.equalsIgnoreCase("DECAYCORRECTED")) {

                        if ((valueString.equalsIgnoreCase("Y")) || (valueString.equalsIgnoreCase("N"))) {
                            fileInfo.setDecayCorrected(valueString);
                            haveDecayCorrected = true;
                        } else {
                            raFile.close();
                            MipavUtil.displayError("Decay corrected has an illegal value of " + valueString);
                            throw new IOException();
                        }
                    } // else if (keyString.equalsIgnoreCase("DECAYCORRECTED"))
                    else if (keyString.equalsIgnoreCase("PETSTUDY(GENERAL)")) {
                        fileInfo.setPETStudyGeneral(true);
                    } else if (keyString.equalsIgnoreCase("SCANNERQUANTIFICATIONFACTOR")) {
                        fileInfo.setScannerQuantificationFactor(valueString);
                    } else if (keyString.equalsIgnoreCase("QUANTIFICATIONUNITS")) {
                        fileInfo.setQuantificationUnits(valueString);
                    } else if (keyString.equalsIgnoreCase("PETDATATYPE")) {

                        if ((valueString.equalsIgnoreCase("EMISSION")) ||
                                (valueString.equalsIgnoreCase("TRANSMISSION")) ||
                                (valueString.equalsIgnoreCase("BLANK")) ||
                                (valueString.equalsIgnoreCase("ATTENUATIONCORRECTION")) ||
                                (valueString.equalsIgnoreCase("NORMALISATION")) ||
                                (valueString.equalsIgnoreCase("IMAGE"))) {
                            fileInfo.setPETDataType(valueString);
                        } else {
                            raFile.close();
                            MipavUtil.displayError("PET data type has an illegal value of " + valueString);
                            throw new IOException();
                        }
                    } // else if (keyString.equalsIgnoreCase("PETDATATYPE"))
                    else if (keyString.equalsIgnoreCase("STARTHORIZONTALBEDPOSITION(MM)")) {
                        fileInfo.setStartHorizontalBedPosition(valueString);
                    } else if (keyString.equalsIgnoreCase("DYNAMICSTUDY(GENERAL)")) {
                        fileInfo.setDynamicStudyGeneral(true);
                    } else if (keyString.equalsIgnoreCase("GATEDSTUDY(GENERAL)")) {
                        fileInfo.setGatedStudyGeneral(true);
                    } else if (keyString.equalsIgnoreCase("SPECTSTUDY(GENERAL)")) {
                        fileInfo.setSpectStudyGeneral(true);
                    } else if (keyString.equalsIgnoreCase("STATICSTUDY(GENERAL)")) {
                        fileInfo.setStaticStudyGeneral(true);
                    } else if (keyString.equalsIgnoreCase("NUMBEROFDETECTORHEADS")) {
                        fileInfo.setDetectorHeadNumber(valueString);
                        haveDetectorHeadNumber = true;
                    } else if ((keyString.equalsIgnoreCase("NUMBEROFIMAGES/ENERGYWINDOW")) ||
                               (keyString.equalsIgnoreCase("NUMBEROFIMAGES/WINDOW"))) {
                        fileInfo.setImagesPerEWindow(valueString);
                        haveImagesPerEWindow = true;
                        imagesPerEWindow = Integer.valueOf(valueString).intValue();
                    } else if (keyString.equalsIgnoreCase("PROCESSSTATUS")) {

                        if ((valueString.equalsIgnoreCase("RECONSTRUCTED")) ||
                                (valueString.equalsIgnoreCase("ACQUIRED")) ||
                                (valueString.equalsIgnoreCase("FUNCTIONAL")) ||
                                (valueString.equalsIgnoreCase("SIMULATED")) ||
                                (valueString.equalsIgnoreCase("PHANTOMDESCRIPTION"))) {
                            fileInfo.setProcessStatus(valueString);

                            if (valueString.equalsIgnoreCase("RECONSTRUCTED")) {
                                haveReconstructed = true;
                            }
                        } else {
                            raFile.close();
                            MipavUtil.displayError("Process status has an illegal value of " + valueString);
                            throw new IOException();
                        }
                    } // else if (keyString.equalsIgnoreCase("PROCESSSTATUS"))
                    else if (keyString.equalsIgnoreCase("NUMBEROFPROJECTIONS")) {
                        fileInfo.setProjectionNumber(valueString);
                    } else if (keyString.equalsIgnoreCase("EXTENTOFROTATION")) {
                        fileInfo.setRotationExtent(valueString);
                    } else if (keyString.equalsIgnoreCase("TIMEPERPROJECTION(SEC)")) {
                        fileInfo.setProjectionTime(valueString);
                    } else if (keyString.equalsIgnoreCase("STUDYDURATION(SEC)")) {
                        fileInfo.setStudyDuration(valueString);
                    } else if (keyString.equalsIgnoreCase("MAXIMUMPIXELCOUNT")) {
                        fileInfo.setMaximumPixelCount(valueString);
                    } else if (keyString.equalsIgnoreCase("PATIENTORIENTATION")) {
                        fileInfo.setPatientOrientation(valueString);
                    } else if (keyString.equalsIgnoreCase("PATIENTROTATION")) {

                        if ((valueString.equalsIgnoreCase("PRONE")) || (valueString.equalsIgnoreCase("SUPINE")) ||
                                (valueString.equalsIgnoreCase("OTHER"))) {
                            fileInfo.setPatientRotation(valueString);
                        } else {
                            raFile.close();
                            MipavUtil.displayError("Patient rotation has an illegal value of " + valueString);
                            throw new IOException();
                        }
                    } // else if (keyString.equalsIgnoreCase("PATIENTROTATION"))
                    else if (keyString.equalsIgnoreCase("PETSTUDY(IMAGEDATA)")) {
                        fileInfo.setPETStudyImageData(true);
                    } else if (keyString.equalsIgnoreCase("SPECTSTUDY(RECONSTRUCTEDDATA)")) {
                        fileInfo.setSpectStudyReconstructedData(true);
                    } else if (keyString.equalsIgnoreCase("METHODOFRECONSTRUCTION")) {
                        fileInfo.setReconstructionMethod(valueString);
                    } else if (keyString.equalsIgnoreCase("NUMBEROFSLICES")) {
                        fileInfo.setSliceNumber(valueString);
                    } else if (keyString.equalsIgnoreCase("NUMBEROFREFERENCEFRAME")) {
                        fileInfo.setReferenceFrameNumber(valueString);
                        haveReferenceFrameNumber = true;
                    } else if (keyString.equalsIgnoreCase("SLICETHICKNESS(PIXELS)")) {
                        fileInfo.setSliceThickness(Float.parseFloat(valueString));
                        haveSliceThickness = true;
                    } else if ((keyString.equalsIgnoreCase("CENTRE-CENTRESLICESEPARATION(PIXELS)")) ||
                                   (keyString.equalsIgnoreCase("CENTER-CENTERSLICESEPARATION(PIXELS)"))) {
                        fileInfo.setCenterCenter(valueString);
                    } else if (keyString.equalsIgnoreCase("FILTERNAME")) {
                        fileInfo.setFilterName(valueString);
                    } else if (keyString.equalsIgnoreCase("Z-AXISFILTER")) {
                        fileInfo.setZAxisFilter(valueString);
                    } else if (keyString.equalsIgnoreCase("APPLIEDCORRECTIONS")) {
                        fileInfo.setAppliedCorrections(valueString);
                    } else if (keyString.equalsIgnoreCase("METHODOFATTENUATIONCORRECTION")) {
                        fileInfo.setAttenuationCorrection(valueString);
                    } else if (keyString.equalsIgnoreCase("SCATTERCORRECTED")) {

                        if ((valueString.equalsIgnoreCase("Y")) || (valueString.equalsIgnoreCase("N"))) {
                            fileInfo.setScatterCorrected(valueString);
                        } else {
                            raFile.close();
                            MipavUtil.displayError("Scatter corrected has an illegal value of " + valueString);
                            throw new IOException();
                        }
                    } // else if (keyString.equalsIgnoreCase("SCATTERCORRECTED"))
                    else if (keyString.equalsIgnoreCase("METHODOFSCATTERCORRECTION")) {
                        fileInfo.setScatterCorrectionMethod(valueString);
                    } else if (keyString.equalsIgnoreCase("OBLIQUERECONSTRUCTION")) {

                        if ((valueString.equalsIgnoreCase("Y")) || (valueString.equalsIgnoreCase("N"))) {
                            fileInfo.setObliqueReconstruction(valueString);
                        } else {
                            raFile.close();
                            MipavUtil.displayError("Oblique reconstruction has an illegal value of " + valueString);
                            throw new IOException();
                        }
                    } // else if (keyString.equalsIgnoreCase("OBLIQUERECONSTRUCTION"))
                    else if (keyString.equalsIgnoreCase("IMAGEDATADESCRIPTION")) {
                        fileInfo.setImageDataDescription(true);
                    } else if (keyString.equalsIgnoreCase("INDEXNESTINGLEVEL")) {
                        fileInfo.setIndexNestingLevel(valueString);
                    } else if ((keyString.length() >= 21) &&
                                   (keyString.substring(0, 19).equalsIgnoreCase("IMAGEDURATION(SEC)["))) {
                        index = keyString.indexOf("]");

                        if (index == -1) {
                            raFile.close();
                            MipavUtil.displayError("] not found in image duration (sec) keystring");
                            throw new IOException();
                        }

                        timeFrame = keyString.substring(19, index);
                        fileInfo.setImageDuration(timeFrame, valueString);
                    } else if (keyString.equalsIgnoreCase("IMAGEDURATION(SEC)")) {
                        fileInfo.setImageDuration(valueString);
                    } else if ((keyString.length() >= 30) &&
                                   (keyString.substring(0, 28).equalsIgnoreCase("IMAGERELATIVESTARTTIME(SEC)["))) {
                        index = keyString.indexOf("]");

                        if (index == -1) {
                            raFile.close();
                            MipavUtil.displayError("] not found in image relative start time (sec) keystring");
                            throw new IOException();
                        }

                        timeFrame = keyString.substring(28, index);
                        fileInfo.setImageRelativeStartTime(timeFrame, valueString);
                    } else if (keyString.equalsIgnoreCase("IMAGERELATIVESTARTTIME(SEC)")) {
                        fileInfo.setImageRelativeStartTime(valueString);
                    } else if (keyString.equalsIgnoreCase("IMAGESTARTTIME")) {
                        fileInfo.setImageStartTime(valueString);
                    } else if (keyString.equalsIgnoreCase("LABEL")) {
                        fileInfo.setLabel(valueString);
                    } else if (keyString.equalsIgnoreCase("TOTALCOUNTS")) {
                        fileInfo.setTotalCounts(valueString);
                    } else if (keyString.equalsIgnoreCase("NUMBEROFFRAMEGROUPS")) {
                        fileInfo.setFrameGroupNumber(valueString);
                    } else if (keyString.equalsIgnoreCase("NUMBEROFIMAGESTHISFRAMEGROUP")) {
                        fileInfo.setFrameGroupImages(valueString);
                    } else if (keyString.equalsIgnoreCase("PAUSEBETWEENIMAGES(SEC)")) {
                        fileInfo.setPauseBetweenImages(valueString);
                    } else if (keyString.equalsIgnoreCase("PAUSEBETWEENFRAMEGROUPS(SEC)")) {
                        fileInfo.setPauseBetweenFrameGroups(valueString);
                    } else if (keyString.equalsIgnoreCase("MAXIMUMPIXELCOUNTINGROUP")) {
                        fileInfo.setMaximumPixelCountInGroup(valueString);
                    } else if (keyString.equalsIgnoreCase("STUDYDURATION(ELAPSED)SEC")) {
                        fileInfo.setElapsedStudyDuration(valueString);
                    } else if (keyString.equalsIgnoreCase("NUMBEROFCARDIACCYCLES(OBSERVED)")) {
                        fileInfo.setObservedCardiacCycles(valueString);
                    } else if (keyString.equalsIgnoreCase("NUMBEROFTIMEWINDOWS")) {
                        fileInfo.setTimeWindows(valueString);
                    } else if (keyString.equalsIgnoreCase("NUMBEROFIMAGESINTIMEWINDOW")) {
                        fileInfo.setTimeWindowImages(valueString);
                    } else if (keyString.equalsIgnoreCase("NUMBEROFIMAGESINWINDOW")){
                        fileInfo.setTimeWindowImages(valueString);
                    } else if (keyString.equalsIgnoreCase("FRAMINGMETHOD")) {
                        fileInfo.setFramingMethod(valueString);
                    } else if (keyString.equalsIgnoreCase("TIMEWINDOWLOWERLIMIT(SEC)")) {
                        fileInfo.setTimeWindowLowerLimit(valueString);
                    } else if (keyString.equalsIgnoreCase("TIMEWINDOWUPPERLIMIT(SEC)")) {
                        fileInfo.setTimeWindowUpperLimit(valueString);
                    } else if (keyString.equalsIgnoreCase("%R-RCYCLESACQUIREDTHISWINDOW")) {
                        fileInfo.setRRCycles(valueString);
                    } else if (keyString.equalsIgnoreCase("NUMBEROFCARDIACCYCLES(ACQUIRED)")) {
                        fileInfo.setAcquiredCardiacCycles(valueString);
                    } else if (keyString.equalsIgnoreCase("STUDYDURATION(ACQUIRED)SEC")) {
                        fileInfo.setAcquiredStudyDuration(valueString);
                    } else if (keyString.equalsIgnoreCase("R-RHISTOGRAM")) {
                        fileInfo.setRRHistogram(valueString);
                    } else if (keyString.equalsIgnoreCase("ORGAN")) {
                        fileInfo.setOrgan(valueString);
                    }
                    else if (keyString.equalsIgnoreCase("GATEDFRAMEMODE")) {
                        fileInfo.setGatedFrameMode(valueString);
                    }
                    // else {
                    // MipavUtil.displayError(keyString);
                    // }
                } // if (lineString != null)
            } // while (lineString != null)

            raFile.close();

            if (!haveFileName) {
                MipavUtil.displayError("Name of data file key string not found");
                throw new IOException();
            }

            if (fileName == null) {
                MipavUtil.displayError("Name of data file is null indicating adminstrative data only");
                throw new IOException();
            }

            if (!haveXDim) {
                MipavUtil.displayError("Matrix size [1] key string not found");
                throw new IOException();
            }

            if (!haveYDim) {
                MipavUtil.displayError("Matrix size [2] key string not found");
                throw new IOException();
            }

            if ((!haveZDim) && (haveTotalImageNumber)) {
                zDim = totalImageNumber;
            }

            if ((!haveZDim) && (!haveTotalImageNumber)) {
                totalImageNumber = energyWindowsNumber * imagesPerEWindow;
                zDim = energyWindowsNumber * imagesPerEWindow;
            }

            if (tDim > 1) {
                imgExtents = new int[4];
                imgExtents[3] = tDim;
                imgExtents[2] = zDim;
            } else if (zDim > 1) {
                imgExtents = new int[3];
                imgExtents[2] = zDim;
            } else {
                imgExtents = new int[2];
            }

            imgExtents[0] = xDim;
            imgExtents[1] = yDim;
            fileInfo.setExtents(imgExtents);

            if (!haveNumberFormat) {
                MipavUtil.displayError("Number format key string not found");
                throw new IOException();
            }

            if (!haveBytesPerPixel) {
                MipavUtil.displayError("Number of bytes per pixel key string not found");
                throw new IOException();
            }

            if (numberFormat == SIGNED_INTEGER) {

                if (bytesPerPixel == 1) {
                    type = ModelStorageBase.BYTE;
                } else if (bytesPerPixel == 2) {
                    type = ModelStorageBase.SHORT;
                } else if (bytesPerPixel == 4) {
                    type = ModelStorageBase.INTEGER;
                } else if (bytesPerPixel == 8) {
                    type = ModelStorageBase.LONG;
                } else {
                    MipavUtil.displayError("Cannot handle signed integer with " + bytesPerPixel + " bytes");
                    throw new IOException();
                }
            } // if (numberFormat == SIGNED_INTEGER)
            else if (numberFormat == UNSIGNED_INTEGER) {

                if (bytesPerPixel == 1) {
                    type = ModelStorageBase.UBYTE;
                } else if (bytesPerPixel == 2) {
                    type = ModelStorageBase.USHORT;
                } else if (bytesPerPixel == 4) {
                    type = ModelStorageBase.UINTEGER;
                } else if (bytesPerPixel == 8) {
                    type = ModelStorageBase.LONG;
                    MipavUtil.displayWarning("Warning: Reading unsigned long as signed long");
                } else {
                    MipavUtil.displayError("Cannot handle unsigned integer with " + bytesPerPixel + " bytes");
                    throw new IOException();
                }
            } // else if (numberFormat == UNSIGNED_INTEGER)
            else if (numberFormat == SHORT_FLOAT) {

                if (bytesPerPixel == 4) {
                    type = ModelStorageBase.FLOAT;
                } else {
                    MipavUtil.displayError("Short float had " + bytesPerPixel + " bytes rather than the expected 4");
                    throw new IOException();
                }
            } // else if (numberFormat == SHORT_FLOAT)
            else if (numberFormat == LONG_FLOAT) {

                if (bytesPerPixel == 8) {
                    type = ModelStorageBase.DOUBLE;
                } else {
                    MipavUtil.displayError("Long float had " + bytesPerPixel + " bytes rather than the expected 8");
                    throw new IOException();
                }
            } // else if (numberFormat == LONG_FLOAT)
            else if (numberFormat == BIT) {
                // Our BufferBoolean(size) = new BitSet(size) = new long[(size+63)>>6],
                // so our Boolean is stored in series of 64 bit units.
                // However, this BIT is stored in series of 8 bit units with the MSB being
                // the leftmost pixel.  Here BIT(size) = new byte[(size+7)>>3].
            } else if (numberFormat == ASCII) { }

            fileInfo.setDataType(type);

            if (readData) {
                if (one) {
                    image = new ModelImage(type, new int[] { xDim, yDim }, fileName);
                } else {
                    image = new ModelImage(type, imgExtents, fileName);
                }
            } // if (readData)

            if (!haveOrientation) {

                // default value
                if ((haveTomographic) && (haveReconstructed)) {
                    orientation = FileInfoBase.AXIAL;
                } else {
                    orientation = FileInfoBase.UNKNOWN_ORIENT;
                }
            }

            fileInfo.setImageOrientation(orientation);

            if (!haveEndianess) {

                // default value
                endianess = FileBase.BIG_ENDIAN;
            }

            fileInfo.setEndianess(endianess);

            if (!haveEnergyWindowsNumber) {
                fileInfo.setEnergyWindowsNumber("1");
            }

            if (!haveImagesPerEWindow) {
                fileInfo.setImagesPerEWindowBrief("1");
            }

            if (!haveFloodCorrected) {
                fileInfo.setFloodCorrected("Y");
            }

            if (!haveDecayCorrected) {
                fileInfo.setDecayCorrected("N");
            }

            if ((haveTomographic) && (!haveDetectorHeadNumber)) {
                fileInfo.setDetectorHeadNumber("1");
            }

            if ((haveTomographic) && (haveReconstructed) && (!haveReferenceFrameNumber)) {
                fileInfo.setReferenceFrameNumber("0");
            }

            if ((haveTomographic) && (haveReconstructed) && (!haveSliceThickness)) {
                fileInfo.setSliceThickness(1);
            }

            // slice thickness & centre-centre slice separation keys needs to be
            // specified in pixel units.  However, there is no key that specifies
            // the pixel size in the Z dimension.  Therefore, use the average of
            // the pixel sizes in the x and y dimensions if the data is not tomographic
            // or is not of equal dimensions.  Howerver, if the data is tomographic and is
            // of equal dimensions assume that the standard will be violated to store
            // the x pixel resolution == y pixel resolution in scalingFactor 1 and
            // the z pixel resolution in scalingfactor2.
            if (haveResolsT) {
                imgResols[3] = scalingFactor4;
            }

            if ((haveResolsX) && (haveResolsY) && (haveResolsZ)) {
                imgResols[0] = scalingFactor1;
                imgResols[1] = scalingFactor2;
                imgResols[2] = scalingFactor3;
            } else if ((haveTomographic) && (xDim == yDim) && (zDim > 1)) {
                imgResols[0] = scalingFactor1;
                imgResols[1] = scalingFactor1;
                imgResols[2] = scalingFactor2;
            } else {
                imgResols[0] = scalingFactor1;
                imgResols[1] = scalingFactor2;
                imgResols[2] = (imgResols[0] + imgResols[1]) / 2.0f;
            }

            fileInfo.setResolutions(imgResols);

            // x,y,z Interfile units are always in millimeters
            fileInfo.setUnitsOfMeasure(Unit.MILLIMETERS.getLegacyNum(), 0);
            fileInfo.setUnitsOfMeasure(Unit.MILLIMETERS.getLegacyNum(), 1);
            fileInfo.setUnitsOfMeasure(Unit.MILLIMETERS.getLegacyNum(), 2);

            if (haveResolsT) {
                fileInfo.setUnitsOfMeasure(Unit.MILLIMETERS.getLegacyNum(), 3);
            }

            if ((!haveDataByteOffset) && (!haveDataStartingBlock)) {
                dataByteOffset = 0L;
            }

            if (haveDataStartingBlock) {
                dataByteOffset = 2048L * dataStartingBlock;
            }
            
            if (!readData) {
                return null;
            }

            file = new File(fileDir + fileName);
            try {
                raFile = new RandomAccessFile(file, "r");
            }
            catch (FileNotFoundException e) {
                index = originalFileName.lastIndexOf('.');
                String defaultFileName = originalFileName.substring(0,index) + ".img";
                if (!defaultFileName.equalsIgnoreCase(fileName)) {
                    MipavUtil.displayWarning("Could not find specified data file " + fileName + "\n");
                    MipavUtil.displayWarning("Trying default file name " + defaultFileName + "\n");
                    file = new File(fileDir + defaultFileName);
                    raFile = new RandomAccessFile(file, "r"); 
                }
                else {
                    MipavUtil.displayError("Could not find specified data file " + fileName + "\n");
                    throw new IOException();
                }
            }
            fileLength = raFile.length();

            bufferSize = xDim * yDim;
            numberSlices = zDim * tDim;

            if (one) {

                if (imgExtents.length > 2) {
                    int middle = imgExtents[2] / 2;

                    if ((type == ModelStorageBase.SHORT) || (type == ModelStorageBase.USHORT)) {
                        middle *= 2;
                    } else if ((type == ModelStorageBase.INTEGER) || (type == ModelStorageBase.UINTEGER) ||
                                   (type == ModelStorageBase.FLOAT)) {
                        middle *= 4;
                    }

                    dataByteOffset += middle * bufferSize;
                }

                numberSlices = 1;
            }

            raFile.seek(dataByteOffset);

            if (numberFormat == BIT) {
                imgBuffer = new float[bufferSize];

                for (i = 0; i < numberSlices; i++) {
                    readBBuffer(i, imgBuffer);
                    image.importData(i * bufferSize, imgBuffer, false);
                }
            } else if (type == ModelStorageBase.DOUBLE) {
                imgDBuffer = new double[bufferSize];

                for (i = 0; i < numberSlices; i++) {
                    readDBuffer(i, imgDBuffer);
                    image.importData(i * bufferSize, imgDBuffer, false);
                }
            } // else if (type == ModelStorageBase.DOUBLE)
            else if ((type == ModelStorageBase.LONG) || (type == ModelStorageBase.UINTEGER)) {
                imgLBuffer = new long[bufferSize];

                for (i = 0; i < numberSlices; i++) {
                    readLBuffer(i, imgLBuffer);
                    image.importData(i * bufferSize, imgLBuffer, false);
                }
            } // else if ((type == ModelStorageBase.LONG)||
              // (type == ModelStorageBase.UINTEGER))
            else {
                imgBuffer = new float[bufferSize];

                for (i = 0; i < numberSlices; i++) {
                    readBuffer(i, imgBuffer);
                    image.importData(i * bufferSize, imgBuffer, false);
                }
            }

            raFile.close();
            image.calcMinMax();
            fileInfo.setMin(image.getMin());
            fileInfo.setMax(image.getMax());

            image.setFileInfo(fileInfo, 0);
            for (i = 1; i < numberSlices; i++) {
                fileInfoCopy = (FileInfoInterfile)fileInfo.clone();
                image.setFileInfo(fileInfoCopy, i);
            }


            return image;
        } // try
        catch (OutOfMemoryError error) {

            if (image != null) {
                image.disposeLocal();
                image = null;
            }

            System.gc();
            throw error;
        }
    }

    /**
     * Sets the file name (used when reading TIFF multiFile).
     *
     * @param  fName  file name of image to read.
     */
    public void setFileName(String fName) {
        fileName = fName;
    }

    /**
     * Writes an Interfile format type image.
     *
     * @param      image    Image model of data to write.
     * @param      options  options such as starting and ending slices and times
     *
     * @exception  IOException  if there is an error writing the file
     */
    public void writeImage(ModelImage image, FileWriteOptions options) throws IOException {
        int zBeginOriginal; // first z slice to write
        int zEndOriginal; // last z slice to write
        int tBeginOriginal; // first t time to write
        int tEndOriginal; // last t time to write
        int zBegin = 0;
        int zEnd = 0;
        int tBegin = 0;
        int tEnd = 0;
        int seq = 0;
        String modifiedFileName = null;
        byte[] lineBytes = new byte[255];
        FileInfoBase baseInfo;
        FileInfoInterfile fileInfo;
        boolean simple = false;
        String imagingModality = null;
        String originatingSystem = null;
        String keysDate = null;
        String conversionProgram = null;
        String programAuthor = null;
        String programVersion = null;
        String programDate = null;
        String originalInstitution = null;
        String contactPerson = null;
        String dataDescription = null;
        String patientName = null;
        String patientID = null;
        String patientDOB = null;
        String patientSex = null;
        String studyID = null;
        String examType = null;
        String organ = null;
        String interfileDataType = null;
        String studyDate = null;
        String studyTime = null;
        String isotopeNumber = null;
        int isoNumber = 1;
        String[] isotopeName = null;
        String[] betaHalflife = null;
        String[] gammaHalflife = null;
        String[] branchingFactor = null;
        String radiopharmaceutical = null;
        String energyWindowsNumber = null;
        int energyNumber = 0;
        String[] energyWindowName = null;
        String[] lowerLevel = null;
        String[] upperLevel = null;
        String floodCorrected = null;
        String decayCorrected = null;
        String scannerQuantificationFactor = null;
        String quantificationUnits = null;
        String PETDataType = null;
        String startHorizontalBedPosition = null;
        String detectorHeadNumber = null;
        String imagesPerEWindow = null;
        int numberImagesPerEWindow = 1;
        String processStatus = null;
        String projectionNumber = null;
        String rotationExtent = null;
        String projectionTime = null;
        String studyDuration = null;
        String[] maximumPixelCount = null;
        int maximumPixelCountIndex = 0;
        String patientOrientation = null;
        String patientRotation = null;
        boolean PETStudyImageData = false;
        boolean spectStudyReconstructedData = false;
        String reconstructionMethod = null;
        String sliceNumber = null;
        String referenceFrameNumber = null;
        String sliceThickness = null;
        String centerCenter = null;
        String filterName = null;
        String zAxisFilter = null;
        String appliedCorrections = null;
        String attenuationCorrection = null;
        String scatterCorrected = null;
        String scatterCorrectionMethod = null;
        String obliqueReconstruction = null;
        boolean imageDataDescription = false;
        String indexNestingLevel = null;
        int iLevel = 0;
        String[] imageDuration = null;
        String[] gatedFrameMode = null;
        String[] imageRelativeStartTime = null;
        int index;
        int i;
        int t, z;
        int timeOffset;
        int volSize;
        int sliceTotal;
        int sliceNum;
        BitSet bufferBitSet;
        byte[] bufferByte;
        boolean haveStaticStudy = false;
        boolean haveDynamicStudy = false;
        boolean haveGatedStudy = false;
        String[] matrixSize1;
        String[] matrixSize2;
        String[] numberFormatString = null;
        String[] bytesPerPixelString = null;
        String[] scalingFactor1String = null;
        String[] scalingFactor2String = null;
        String[] imageStartTime = null;
        String[] label = null;
        String[] totalCounts = null;
        String frameGroupNumber = null;
        int fGroupNumber = 1;
        String[] frameGroupImages = null;
        String[] pauseBetweenImages = null;
        String[] pauseBetweenFrameGroups = null;
        String[] maximumPixelCountInGroup = null;
        String elapsedStudyDuration = null;
        String observedCardiacCycles = null;
        String timeWindows = null;
        int timeWindowsNumber = 1;
        String[] timeWindowImages = null;
        String[] framingMethod = null;
        String[] timeWindowLowerLimit = null;
        String[] timeWindowUpperLimit = null;
        String[] RRCycles = null;
        String[] acquiredCardiacCycles = null;
        String[] acquiredStudyDuration = null;
        String[] RRHistogram = null;
        String windowA = null;
        String windowB = null;
        String windowC = null;

        if (image.getNDims() >= 3) {
            zBeginOriginal = options.getBeginSlice();
            zEndOriginal = options.getEndSlice();
        } else {
            zBeginOriginal = 0;
            zEndOriginal = 0;
        }

        if (image.getNDims() == 4) {
            tBeginOriginal = options.getBeginTime();
            tEndOriginal = options.getEndTime();
        } else {
            tBeginOriginal = 0;
            tEndOriginal = 0;
        }
        
        if (!options.isMultiFile()) {
            zBegin = zBeginOriginal;
            zEnd = zEndOriginal;
            tBegin = tBeginOriginal;
            tEnd = tEndOriginal;
        }
        else if (image.getNDims() == 4) {
            zBegin = zBeginOriginal;
            zEnd = zEndOriginal;
            tBegin = tBeginOriginal;
            tEnd = tBeginOriginal;
        }
        else if (image.getNDims() == 3) {
            zBegin = zBeginOriginal;
            zEnd = zBeginOriginal;
            tBegin = 0;
            tEnd = 0;
        }
        
        seq = options.getStartNumber();
        
        while (true) {

            index = originalFileName.indexOf(".");
    
            if (!options.isMultiFile()) {
                if (index != -1) {
        
                    if (originalFileName.length() > (index + 1))  {
                        dataFileName = originalFileName.substring(0, index + 1) + "img";
                        headerFileName = originalFileName.substring(0, index + 1) + "hdr";
                    }
                } else {
                    dataFileName = originalFileName + ".img";
                    headerFileName = originalFileName + ".hdr";
                }
            } // if (!options.isMultiFile())
            else { // isMultiFile
                if (index != -1) {
                    
                    if (originalFileName.length() > (index + 1))  {
                        modifiedFileName = originalFileName.substring(0, index);
                    }
                } else {
                    modifiedFileName = new String(originalFileName);
                }
                if (options.getDigitNumber() == 1) {
                    modifiedFileName += Integer.toString(seq);
                } else if (options.getDigitNumber() == 2) {

                    if (seq < 10) {
                        modifiedFileName += "0" + Integer.toString(seq);
                    } else {
                        modifiedFileName += Integer.toString(seq);
                    }
                } else if (options.getDigitNumber() == 3) {

                    if (seq < 10) {
                        modifiedFileName += "00" + Integer.toString(seq);
                    } else if (seq < 100) {
                        modifiedFileName += "0" + Integer.toString(seq);
                    } else {
                        modifiedFileName += Integer.toString(seq);
                    }
                } else if (options.getDigitNumber() == 4) {

                    if (seq < 10) {
                        modifiedFileName += "000" + Integer.toString(seq);
                    } else if (seq < 100) {
                        modifiedFileName += "00" + Integer.toString(seq);
                    } else if (seq < 1000) {
                        modifiedFileName += "0" + Integer.toString(seq);
                    } else {
                        modifiedFileName += Integer.toString(seq);
                    }
                } 
                dataFileName = modifiedFileName + ".img";
                headerFileName = modifiedFileName + ".hdr";
            } // isMultiFile
            file = new File(fileDir + headerFileName);
            raFile = new RandomAccessFile(file, "rw");
            // Necessary so that if this is an overwritten file there isn't any
            // junk at the end
            raFile.setLength(0);
    
            try { // In this case, the file must be Interfile
                fileInfo = (FileInfoInterfile) image.getFileInfo(0);
            } catch (ClassCastException e) { // If it isn't, catch the exception
                                             // and make a new fileInfo
                fileInfo = new FileInfoInterfile(fileName, fileDir, FileUtility.INTERFILE);
                simple = true; // Write the header without all the Interfile info
            }
    
            baseInfo = image.getFileInfo()[0];
    
            lineBytes = new String("!INTERFILE := \r\n").getBytes();
            raFile.write(lineBytes);
    
            if (!simple) {
                imagingModality = fileInfo.getImagingModality();
    
                if (imagingModality != null) {
                    imagingModality = new String("!imaging Modality := " + imagingModality + "\r\n");
                    lineBytes = imagingModality.getBytes();
                    raFile.write(lineBytes);
                }
    
                originatingSystem = fileInfo.getOriginatingSystem();
    
                if (originatingSystem != null) {
                    originatingSystem = new String("!originating system := " + originatingSystem + "\r\n");
                    lineBytes = originatingSystem.getBytes();
                    raFile.write(lineBytes);
                }
            } // if (!simple)
    
            lineBytes = new String("!version of keys := 4.0\r\n").getBytes();
            raFile.write(lineBytes);
    
            if (!simple) {
                keysDate = fileInfo.getKeysDate();
    
                if (keysDate != null) {
                    keysDate = new String("date of keys := " + keysDate + "\r\n");
                    lineBytes = keysDate.getBytes();
                    raFile.write(lineBytes);
                }
    
                conversionProgram = fileInfo.getConversionProgram();
    
                if (conversionProgram != null) {
                    conversionProgram = new String("conversion program := " + conversionProgram + "\r\n");
                    lineBytes = conversionProgram.getBytes();
                    raFile.write(lineBytes);
                }
    
                programAuthor = fileInfo.getProgramAuthor();
    
                if (programAuthor != null) {
                    programAuthor = new String("program author := " + programAuthor + "\r\n");
                    lineBytes = programAuthor.getBytes();
                    raFile.write(lineBytes);
                }
    
                programVersion = fileInfo.getProgramVersion();
    
                if (programVersion != null) {
                    programVersion = new String("program version := " + programVersion + "\r\n");
                    lineBytes = programVersion.getBytes();
                    raFile.write(lineBytes);
                }
    
                programDate = fileInfo.getProgramDate();
    
                if (programDate != null) {
                    programDate = new String("program date := " + programDate + "\r\n");
                    lineBytes = programDate.getBytes();
                    raFile.write(lineBytes);
                }
            } // if (!simple)
    
            lineBytes = new String("GENERAL DATA :=\r\n").getBytes();
            raFile.write(lineBytes);
    
            if (!simple) {
                originalInstitution = fileInfo.getOriginalInstitution();
    
                if (originalInstitution != null) {
                    originalInstitution = new String("original institution := " + originalInstitution + "\r\n");
                    lineBytes = originalInstitution.getBytes();
                    raFile.write(lineBytes);
                }
    
                contactPerson = fileInfo.getContactPerson();
    
                if (contactPerson != null) {
                    contactPerson = new String("contact person := " + contactPerson + "\r\n");
                    lineBytes = contactPerson.getBytes();
                    raFile.write(lineBytes);
                }
    
                dataDescription = fileInfo.getDataDescription();
    
                if (dataDescription != null) {
                    dataDescription = new String("data description := " + dataDescription + "\r\n");
                    lineBytes = dataDescription.getBytes();
                    raFile.write(lineBytes);
                }
            } // if (!simple)
    
            lineBytes = new String("data offset in bytes := 0\r\n").getBytes();
            raFile.write(lineBytes);
            
    
            dataFile = new File(fileDir + dataFileName);
    
            if (dataFile.exists()) {
    
                // MipavUtil.displayError(dataFile + " already exists - use another name");
                int response = JOptionPane.showConfirmDialog(ViewUserInterface.getReference().getMainFrame(),
                                                             new String(fileDir + dataFileName +
                                                                        "\nalready exists.  Do you want to replace it?"),
                                                             "Save As", JOptionPane.YES_NO_OPTION,
                                                             JOptionPane.QUESTION_MESSAGE);
    
                if (response == JOptionPane.NO_OPTION) {
                    raFile.close();
    
                    // throw new IOException();
                    return;
                }
            }
    
            lineBytes = new String("name of data file := " + dataFileName + "\r\n").getBytes();
            raFile.write(lineBytes);
    
            if (!simple) {
                patientName = fileInfo.getPatientName();
    
                if (patientName != null) {
                    patientName = new String("patient name := " + patientName + "\r\n");
                    lineBytes = patientName.getBytes();
                    raFile.write(lineBytes);
                }
    
                patientID = fileInfo.getPatientID();
    
                if (patientID != null) {
                    patientID = new String("patient ID := " + patientID + "\r\n");
                    lineBytes = patientID.getBytes();
                    raFile.write(lineBytes);
                }
    
                patientDOB = fileInfo.getPatientDOB();
    
                if (patientDOB != null) {
                    patientDOB = new String("patient DOB := " + patientDOB + "\r\n");
                    lineBytes = patientDOB.getBytes();
                    raFile.write(lineBytes);
                }
    
                patientSex = fileInfo.getPatientSex();
    
                if (patientSex != null) {
                    patientSex = new String("patient sex := " + patientSex + "\r\n");
                    lineBytes = patientSex.getBytes();
                    raFile.write(lineBytes);
                }
    
                studyID = fileInfo.getStudyID();
    
                if (studyID != null) {
                    studyID = new String("study id := " + studyID + "\r\n");
                    lineBytes = studyID.getBytes();
                    raFile.write(lineBytes);
                }
    
                examType = fileInfo.getExamType();
    
                if (examType != null) {
                    examType = new String("exam type := " + examType + "\r\n");
                    lineBytes = examType.getBytes();
                    raFile.write(lineBytes);
                }
            } // if (!simple)
    
            lineBytes = new String("data compression := none\r\n").getBytes();
            raFile.write(lineBytes);
            lineBytes = new String("data encode := none\r\n").getBytes();
            raFile.write(lineBytes);
            
            if (!simple) {
                organ = fileInfo.getOrgan();
                
                if (organ != null) {
                    lineBytes = new String("organ := " + organ + "\r\n").getBytes();
                    raFile.write(lineBytes);
                }
            }
            lineBytes = new String("!GENERAL IMAGE DATA :=\r\n").getBytes();
            raFile.write(lineBytes);
    
            if (!simple) {
                interfileDataType = fileInfo.getInterfileDataType();
    
                if (interfileDataType != null) {
                    lineBytes = new String("!type of data := " + interfileDataType + "\r\n").getBytes();
                    raFile.write(lineBytes);
                }
            } // if (!simple)
    
            tDim = tEnd - tBegin + 1;
            zDim = zEnd - zBegin + 1;
            lineBytes = new String("total number of images := " + (zDim * tDim) + "\r\n").getBytes();
            raFile.write(lineBytes);
    
            if (!simple) {
                studyDate = fileInfo.getStudyDate();
    
                if (studyDate != null) {
                    studyDate = new String("study date := " + studyDate + "\r\n");
                    lineBytes = studyDate.getBytes();
                    raFile.write(lineBytes);
                }
    
                studyTime = fileInfo.getStudyTime();
    
                if (studyTime != null) {
                    studyTime = new String("study time := " + studyTime + "\r\n");
                    lineBytes = studyTime.getBytes();
                    raFile.write(lineBytes);
                }
    
                isotopeNumber = fileInfo.getIsotopeNumber();
    
                if (isotopeNumber != null) {
                    isoNumber = Integer.valueOf(isotopeNumber).intValue();
                    isotopeNumber = new String("Number of isotopes = " + isotopeNumber + "\r\n");
                    lineBytes = isotopeNumber.getBytes();
                    raFile.write(lineBytes);
                }
    
                isotopeName = fileInfo.getIsotopeName();
                betaHalflife = fileInfo.getBetaHalflife();
                gammaHalflife = fileInfo.getGammaHalflife();
                branchingFactor = fileInfo.getBranchingFactor();
    
                for (i = 0; i < isoNumber; i++) {
    
                    if (isoNumber == 1) {
    
                        if (isotopeName != null) {
    
                            if (isotopeName[i] != null) {
                                isotopeName[i] = new String("isotope name := " + isotopeName[i] + "\r\n");
                                lineBytes = isotopeName[i].getBytes();
                                raFile.write(lineBytes);
                            }
                        }
    
                        if (betaHalflife != null) {
    
                            if (betaHalflife[i] != null) {
                                betaHalflife[i] = new String("isotope beta halflife (sec) := " + betaHalflife[i] + "\r\n");
                                lineBytes = betaHalflife[i].getBytes();
                                raFile.write(lineBytes);
                            }
                        }
    
                        if (gammaHalflife != null) {
    
                            if (gammaHalflife[i] != null) {
                                gammaHalflife[i] = new String("isotope gamma halflife (sec) := " + gammaHalflife[i] +
                                                              "\r\n");
                                lineBytes = gammaHalflife[i].getBytes();
                                raFile.write(lineBytes);
                            }
                        }
    
                        if (branchingFactor != null) {
    
                            if (branchingFactor[i] != null) {
                                branchingFactor[i] = new String("isotope branching factor := " + branchingFactor[i] +
                                                                "\r\n");
                                lineBytes = branchingFactor[i].getBytes();
                                raFile.write(lineBytes);
                            }
                        }
                    } // if (isoNumber == 1)
                    else {
    
                        if (isotopeName != null) {
    
                            if (isotopeName[i] != null) {
                                isotopeName[i] = new String("isotope name [" + (i + 1) + "] := " + isotopeName[i] + "\r\n");
                                lineBytes = isotopeName[i].getBytes();
                                raFile.write(lineBytes);
                            }
                        }
    
                        if (betaHalflife != null) {
    
                            if (betaHalflife[i] != null) {
                                betaHalflife[i] = new String("isotope beta halflife (sec) [" + (i + 1) + "] := " +
                                                             betaHalflife[i] + "\r\n");
                                lineBytes = betaHalflife[i].getBytes();
                                raFile.write(lineBytes);
                            }
                        }
    
                        if (gammaHalflife != null) {
    
                            if (gammaHalflife[i] != null) {
                                gammaHalflife[i] = new String("isotope gamma halflife (sec) [" + (i + 1) + "] := " +
                                                              gammaHalflife[i] + "\r\n");
                                lineBytes = gammaHalflife[i].getBytes();
                                raFile.write(lineBytes);
                            }
                        }
    
                        if (branchingFactor != null) {
    
                            if (branchingFactor[i] != null) {
                                branchingFactor[i] = new String("isotope branching factor [" + (i + 1) + "] := " +
                                                                branchingFactor[i] + "\r\n");
                                lineBytes = branchingFactor[i].getBytes();
                                raFile.write(lineBytes);
                            }
                        }
                    } // isoNumber > 1
                } // for (i = 0; i < isoNumber; i++)
    
                radiopharmaceutical = fileInfo.getRadiopharmaceutical();
    
                if (radiopharmaceutical != null) {
                    radiopharmaceutical = new String("radiopharmaceutical := " + radiopharmaceutical + "\r\n");
                    lineBytes = radiopharmaceutical.getBytes();
                    raFile.write(lineBytes);
                }
            } // if (!simple)
    
            endianess = baseInfo.getEndianess();
    
            if (endianess) {
                lineBytes = new String("imagedata byte order := bigendian\r\n").getBytes();
            } else {
                lineBytes = new String("imagedata byte order := littleendian\r\n").getBytes();
            }
    
            raFile.write(lineBytes);
    
            if (!simple) {
                energyWindowsNumber = fileInfo.getEnergyWindowsNumber();
    
                if (energyWindowsNumber != null) {
                    energyNumber = Integer.valueOf(energyWindowsNumber).intValue();
                    energyWindowsNumber = new String("!number of energy windows := " + energyWindowsNumber + "\r\n");
                    lineBytes = energyWindowsNumber.getBytes();
                    raFile.write(lineBytes);
                }
                
                windowA = fileInfo.getWindowA();
                
                if (windowA != null) {
                    lineBytes = new String("Window A := " + windowA + "\r\n").getBytes();
                    raFile.write(lineBytes);
                }
                
                windowB = fileInfo.getWindowB();
                
                if (windowB != null) {
                    lineBytes = new String("Window B := " + windowB + "\r\n").getBytes();
                    raFile.write(lineBytes);
                }
                
                windowC = fileInfo.getWindowC();
                
                if (windowC != null) {
                    lineBytes = new String("Window C := " + windowC + "\r\n").getBytes();
                    raFile.write(lineBytes);
                }
    
                energyWindowName = fileInfo.getEnergyWindow();
                lowerLevel = fileInfo.getEnergyWindowLowerLevel();
                upperLevel = fileInfo.getEnergyWindowUpperLevel();
    
                for (i = 0; i < energyNumber; i++) {
    
                    if (energyWindowName != null) {
    
                        if (energyWindowName[i] != null) {
                            energyWindowName[i] = new String("energy window [" + (i + 1) + "] := " + energyWindowName[i] +
                                                             "\r\n");
                            lineBytes = energyWindowName[i].getBytes();
                            raFile.write(lineBytes);
                        }
                    }
    
                    if (lowerLevel != null) {
    
                        if (lowerLevel[i] != null) {
                            lowerLevel[i] = new String("energy window lower level [" + (i + 1) + "] := " + lowerLevel[i] +
                                                       "\r\n");
                            lineBytes = lowerLevel[i].getBytes();
                            raFile.write(lineBytes);
                        }
                    }
    
                    if (upperLevel != null) {
    
                        if (upperLevel[i] != null) {
                            upperLevel[i] = new String("energy window upper level [" + (i + 1) + "] := " + upperLevel[i] +
                                                       "\r\n");
                            lineBytes = upperLevel[i].getBytes();
                            raFile.write(lineBytes);
                        }
                    }
                } // for (i = 0; i < energyNumber; i++)
    
                floodCorrected = fileInfo.getFloodCorrected();
    
                if (floodCorrected != null) {
                    floodCorrected = new String("flood corrected := " + floodCorrected + "\r\n");
                    lineBytes = floodCorrected.getBytes();
                    raFile.write(lineBytes);
                }
    
                decayCorrected = fileInfo.getDecayCorrected();
    
                if (decayCorrected != null) {
                    decayCorrected = new String("decay corrected := " + decayCorrected + "\r\n");
                    lineBytes = decayCorrected.getBytes();
                    raFile.write(lineBytes);
                }
            } // if (!simple)
    
            if (interfileDataType == null) {
                lineBytes = new String("!STATIC STUDY (General) :=\r\n").getBytes();
            } else if ((interfileDataType.equalsIgnoreCase("STATIC")) || (interfileDataType.equalsIgnoreCase("ROI"))) {
                lineBytes = new String("!STATIC STUDY (General) :=\r\n").getBytes();
                raFile.write(lineBytes);
                haveStaticStudy = true;
                imagesPerEWindow = fileInfo.getImagesPerEWindow();
    
                if (imagesPerEWindow != null) {
                    lineBytes = new String("number of images/energy window := " + imagesPerEWindow + "\r\n").getBytes();
                    raFile.write(lineBytes);
                    numberImagesPerEWindow = Integer.valueOf(imagesPerEWindow).intValue();
                }
    
                matrixSize1 = fileInfo.getMatrixSize1();
                matrixSize2 = fileInfo.getMatrixSize2();
                numberFormatString = fileInfo.getNumberFormat();
                bytesPerPixelString = fileInfo.getBytesPerPixel();
                scalingFactor1String = fileInfo.getScalingFactor1();
                scalingFactor2String = fileInfo.getScalingFactor2();
                imageDuration = fileInfo.getImageDuration();
                imageStartTime = fileInfo.getImageStartTime();
                label = fileInfo.getLabel();
                maximumPixelCount = fileInfo.getMaximumPixelCount();
                totalCounts = fileInfo.getTotalCounts();
    
                for (i = 0; i < numberImagesPerEWindow; i++) {
                    lineBytes = new String("!Static Study (each frame) :=\r\n").getBytes();
                    raFile.write(lineBytes);
                    lineBytes = new String("!image number :=" + (i + 1) + "\r\n").getBytes();
                    raFile.write(lineBytes);
    
                    if (matrixSize1 != null) {
    
                        if (matrixSize1[i] != null) {
                            lineBytes = new String("!matrix size [1] :=" + matrixSize1[i] + "\r\n").getBytes();
                            raFile.write(lineBytes);
                        }
                    }
    
                    if (matrixSize2 != null) {
    
                        if (matrixSize2[i] != null) {
                            lineBytes = new String("!matrix size [2] :=" + matrixSize2[i] + "\r\n").getBytes();
                            raFile.write(lineBytes);
                        }
                    }
    
                    if (numberFormatString != null) {
    
                        if (numberFormatString[i] != null) {
                            lineBytes = new String("!number format :=" + numberFormatString[i] + "\r\n").getBytes();
                            raFile.write(lineBytes);
                        }
                    }
    
                    if (bytesPerPixelString != null) {
    
                        if (bytesPerPixelString[i] != null) {
                            lineBytes = new String("!number of bytes per pixel :=" + bytesPerPixelString[i] + "\r\n").getBytes();
                            raFile.write(lineBytes);
                        }
                    }
    
                    if (scalingFactor1String != null) {
    
                        if (scalingFactor1String[i] != null) {
                            lineBytes = new String("scaling factor (mm/pixel) [1] :=" + scalingFactor1String[i] + "\r\n").getBytes();
                            raFile.write(lineBytes);
                        }
                    }
    
                    if (scalingFactor2String != null) {
    
                        if (scalingFactor2String[i] != null) {
                            lineBytes = new String("scaling factor (mm/pixel) [2] :=" + scalingFactor2String[i] + "\r\n").getBytes();
                            raFile.write(lineBytes);
                        }
                    }
    
                    if (imageDuration != null) {
    
                        if (imageDuration[i] != null) {
                            lineBytes = new String("!image duration (sec) :=" + imageDuration[i] + "\r\n").getBytes();
                            raFile.write(lineBytes);
                        }
                    }
    
                    if (imageStartTime != null) {
    
                        if (imageStartTime[i] != null) {
                            lineBytes = new String("image start time :=" + imageStartTime[i] + "\r\n").getBytes();
                            raFile.write(lineBytes);
                        }
                    }
    
                    if (label != null) {
    
                        if (label[i] != null) {
                            lineBytes = new String("label := " + label[i] + "\r\n").getBytes();
                            raFile.write(lineBytes);
                        }
                    }
    
                    if (maximumPixelCount != null) {
    
                        if (maximumPixelCount[i] != null) {
                            lineBytes = new String("maximum pixel count :=" + maximumPixelCount[i] + "\r\n").getBytes();
                            raFile.write(lineBytes);
                        }
                    }
    
                    if (totalCounts != null) {
    
                        if (totalCounts[i] != null) {
                            lineBytes = new String("total counts :=" + totalCounts[i] + "\r\n").getBytes();
                            raFile.write(lineBytes);
                        }
                    }
                }
            } // else if ((interfileDataType.equalsIgnoreCase("STATIC")) ||
              // (interfileDataType.equalsIgnoreCase("ROI")))
            else if (interfileDataType.equalsIgnoreCase("TOMOGRAPHIC")) {
                lineBytes = new String("!SPECT STUDY (General) :=\r\n").getBytes();
            } else if (interfileDataType.equalsIgnoreCase("PET")) {
                lineBytes = new String("!PET STUDY (General) :=\r\n").getBytes();
            } else if (interfileDataType.equalsIgnoreCase("DYNAMIC")) {
                lineBytes = new String("!DYNAMIC STUDY (General) :=\r\n").getBytes();
                raFile.write(lineBytes);
                haveDynamicStudy = true;
                frameGroupNumber = fileInfo.getFrameGroupNumber();
                fGroupNumber = Integer.valueOf(frameGroupNumber).intValue();
                lineBytes = new String("!number of frame groups := " + frameGroupNumber + "\r\n").getBytes();
                raFile.write(lineBytes);
                matrixSize1 = fileInfo.getMatrixSize1();
                matrixSize2 = fileInfo.getMatrixSize2();
                numberFormatString = fileInfo.getNumberFormat();
                bytesPerPixelString = fileInfo.getBytesPerPixel();
                scalingFactor1String = fileInfo.getScalingFactor1();
                scalingFactor2String = fileInfo.getScalingFactor2();
                frameGroupImages = fileInfo.getFrameGroupImages();
                imageDuration = fileInfo.getImageDuration();
                pauseBetweenImages = fileInfo.getPauseBetweenImages();
                pauseBetweenFrameGroups = fileInfo.getPauseBetweenFrameGroups();
                maximumPixelCountInGroup = fileInfo.getMaximumPixelCountInGroup();
    
                for (i = 0; i < fGroupNumber; i++) {
                    lineBytes = new String("!Dynamic Study (each frame group) :=\r\n").getBytes();
                    raFile.write(lineBytes);
                    lineBytes = new String("!frame group number := " + (i + 1) + "\r\n").getBytes();
                    raFile.write(lineBytes);
    
                    if (matrixSize1 != null) {
    
                        if (matrixSize1[i] != null) {
                            lineBytes = new String("!matrix size [1] := " + matrixSize1[i] + "\r\n").getBytes();
                            raFile.write(lineBytes);
                        }
                    }
    
                    if (matrixSize2 != null) {
    
                        if (matrixSize2[i] != null) {
                            lineBytes = new String("!matrix size [2] := " + matrixSize2[i] + "\r\n").getBytes();
                            raFile.write(lineBytes);
                        }
                    }
    
                    if (numberFormatString != null) {
    
                        if (numberFormatString[i] != null) {
                            lineBytes = new String("!number format := " + numberFormatString[i] + "\r\n").getBytes();
                            raFile.write(lineBytes);
                        }
                    }
    
                    if (bytesPerPixelString != null) {
    
                        if (bytesPerPixelString[i] != null) {
                            lineBytes = new String("!number of bytes per pixel := " + bytesPerPixelString[i] + "\r\n").getBytes();
                            raFile.write(lineBytes);
                        }
                    }
    
                    if (scalingFactor1String != null) {
    
                        if (scalingFactor1String[i] != null) {
                            lineBytes = new String("scaling factor (mm/pixel) [1] := " + scalingFactor1String[i] + "\r\n").getBytes();
                            raFile.write(lineBytes);
                        }
                    }
    
                    if (scalingFactor2String != null) {
    
                        if (scalingFactor2String[i] != null) {
                            lineBytes = new String("scaling factor (mm/pixel) [2] := " + scalingFactor2String[i] + "\r\n").getBytes();
                            raFile.write(lineBytes);
                        }
                    }
    
                    if (frameGroupImages != null) {
    
                        if (frameGroupImages[i] != null) {
                            lineBytes = new String("!number of images this frame group := " + frameGroupImages[i] + "\r\n").getBytes();
                            raFile.write(lineBytes);
                        }
                    }
    
                    if (imageDuration != null) {
    
                        if (imageDuration[i] != null) {
                            lineBytes = new String("!image duration (sec) := " + imageDuration[i] + "\r\n").getBytes();
                            raFile.write(lineBytes);
                        }
                    }
    
                    if (pauseBetweenImages != null) {
    
                        if (pauseBetweenImages[i] != null) {
                            lineBytes = new String("pause between images (sec) := " + pauseBetweenImages[i] + "\r\n").getBytes();
                            raFile.write(lineBytes);
                        }
                    }
    
                    if (pauseBetweenFrameGroups != null) {
    
                        if (pauseBetweenFrameGroups[i] != null) {
                            lineBytes = new String("pause between frame groups (sec) := " + pauseBetweenFrameGroups[i] +
                                                   "\r\n").getBytes();
                            raFile.write(lineBytes);
                        }
                    }
    
                    if (maximumPixelCountInGroup != null) {
    
                        if (maximumPixelCountInGroup[i] != null) {
                            lineBytes = new String("!maximum pixel count in group := " + maximumPixelCountInGroup[i] +
                                                   "\r\n").getBytes();
                            raFile.write(lineBytes);
                        }
                    }
                } // for (i = 0; i < fGroupNumber; i++)
            } // else if (interfileDataType.equalsIgnoreCase("DYNAMIC"))
            else if (interfileDataType.equalsIgnoreCase("GATED")) {
                lineBytes = new String("!GATED STUDY (General) :=\r\n").getBytes();
                haveGatedStudy = true;
            } else if (interfileDataType.equalsIgnoreCase("CURVE")) {
                lineBytes = new String("!CURVE DATA := \r\n").getBytes();
            }
    
            if ((!haveStaticStudy) && (!haveDynamicStudy)) {
                raFile.write(lineBytes);
    
                if (!simple) {
                    detectorHeadNumber = fileInfo.getDetectorHeadNumber();
    
                    if (detectorHeadNumber != null) {
                        lineBytes = new String("number of detector heads := " + detectorHeadNumber + "\r\n").getBytes();
                        raFile.write(lineBytes);
                    }
    
                    imagesPerEWindow = fileInfo.getImagesPerEWindow();
    
                    if (imagesPerEWindow != null) {
                        lineBytes = new String("number of images/energy window := " + imagesPerEWindow + "\r\n").getBytes();
                        raFile.write(lineBytes);
                    }
    
                    scannerQuantificationFactor = fileInfo.getScannerQuantificationFactor();
    
                    if (scannerQuantificationFactor != null) {
                        lineBytes = new String("scanner quantification factor := " + scannerQuantificationFactor + "\r\n").getBytes();
                        raFile.write(lineBytes);
                    }
    
                    quantificationUnits = fileInfo.getQuantificationUnits();
    
                    if (quantificationUnits != null) {
                        lineBytes = new String("quantification units := " + quantificationUnits + "\r\n").getBytes();
                        raFile.write(lineBytes);
                    }
    
                    PETDataType = fileInfo.getPETDataType();
    
                    if (PETDataType != null) {
                        lineBytes = new String("PET data type := " + PETDataType + "\r\n").getBytes();
                        raFile.write(lineBytes);
                    }
    
                    processStatus = fileInfo.getProcessStatus();
    
                    if (processStatus != null) {
                        lineBytes = new String("!process status := " + processStatus + "\r\n").getBytes();
                        raFile.write(lineBytes);
                    }
                } // if (!simple)
    
                if (image.getType() == ModelStorageBase.BOOLEAN) {
                    lineBytes = new String("!number format := bit\r\n").getBytes();
                    raFile.write(lineBytes);
                    lineBytes = new String("!number of bytes per pixel :=\r\n").getBytes();
                    raFile.write(lineBytes);
                } else if (image.getType() == ModelStorageBase.BYTE) {
                    lineBytes = new String("!number format := signed integer\r\n").getBytes();
                    raFile.write(lineBytes);
                    lineBytes = new String("!number of bytes per pixel := 1\r\n").getBytes();
                    raFile.write(lineBytes);
                } else if (image.getType() == ModelStorageBase.UBYTE) {
                    lineBytes = new String("!number format := unsigned integer\r\n").getBytes();
                    raFile.write(lineBytes);
                    lineBytes = new String("!number of bytes per pixel := 1\r\n").getBytes();
                    raFile.write(lineBytes);
                } else if (image.getType() == ModelStorageBase.SHORT) {
                    lineBytes = new String("!number format := signed integer\r\n").getBytes();
                    raFile.write(lineBytes);
                    lineBytes = new String("!number of bytes per pixel := 2\r\n").getBytes();
                    raFile.write(lineBytes);
                } else if (image.getType() == ModelStorageBase.USHORT) {
                    lineBytes = new String("!number format := unsigned integer\r\n").getBytes();
                    raFile.write(lineBytes);
                    lineBytes = new String("!number of bytes per pixel := 2\r\n").getBytes();
                    raFile.write(lineBytes);
                } else if (image.getType() == ModelStorageBase.INTEGER) {
                    lineBytes = new String("!number format := signed integer\r\n").getBytes();
                    raFile.write(lineBytes);
                    lineBytes = new String("!number of bytes per pixel := 4\r\n").getBytes();
                    raFile.write(lineBytes);
                } else if (image.getType() == ModelStorageBase.UINTEGER) {
                    lineBytes = new String("!number format := unsigned integer\r\n").getBytes();
                    raFile.write(lineBytes);
                    lineBytes = new String("!number of bytes per pixel := 4\r\n").getBytes();
                    raFile.write(lineBytes);
                } else if (image.getType() == ModelStorageBase.LONG) {
                    lineBytes = new String("!number format := signed integer\r\n").getBytes();
                    raFile.write(lineBytes);
                    lineBytes = new String("!number of bytes per pixel := 8\r\n").getBytes();
                    raFile.write(lineBytes);
                } else if (image.getType() == ModelStorageBase.FLOAT) {
                    lineBytes = new String("!number format := short float\r\n").getBytes();
                    raFile.write(lineBytes);
                    lineBytes = new String("!number of bytes per pixel := 4\r\n").getBytes();
                    raFile.write(lineBytes);
                } else if (image.getType() == ModelStorageBase.DOUBLE) {
                    lineBytes = new String("!number format := long float\r\n").getBytes();
                    raFile.write(lineBytes);
                    lineBytes = new String("!number of bytes per pixel := 8\r\n").getBytes();
                    raFile.write(lineBytes);
                }
    
                if (zDim > 1) {
                    lineBytes = new String("!number of dimensions := 3\r\n").getBytes();
                } else {
                    lineBytes = new String("!number of dimensions := 2\r\n").getBytes();
                }
    
                raFile.write(lineBytes);
                lineBytes = new String("!matrix size [1] := " + image.getExtents()[0] + "\r\n").getBytes();
                raFile.write(lineBytes);
                lineBytes = new String("!matrix size [2] := " + image.getExtents()[1] + "\r\n").getBytes();
                raFile.write(lineBytes);
    
                if (zDim > 1) {
                    lineBytes = new String("!matrix size [3] := " + zDim + "\r\n").getBytes();
                    raFile.write(lineBytes);
                }
    
                resXUnit = baseInfo.getUnitsOfMeasure(0);
    
                if ((resXUnit == Unit.INCHES.getLegacyNum()) || (resXUnit == Unit.MILS.getLegacyNum()) ||
                        (resXUnit == Unit.CENTIMETERS.getLegacyNum()) ||
                        (resXUnit == Unit.ANGSTROMS.getLegacyNum()) || (resXUnit == Unit.NANOMETERS.getLegacyNum()) ||
                        (resXUnit == Unit.MICROMETERS.getLegacyNum()) || (resXUnit == Unit.MILLIMETERS.getLegacyNum()) ||
                        (resXUnit == Unit.METERS.getLegacyNum()) || (resXUnit == Unit.KILOMETERS.getLegacyNum()) ||
                        (resXUnit == Unit.MILES.getLegacyNum())) {
    
                    // convert to millimeters
                    xResol = baseInfo.getResolutions()[0];
    
                    if (resXUnit == Unit.INCHES.getLegacyNum()) {
                        xResol = 25.4f * xResol;
                    } else if (resXUnit == Unit.MILS.getLegacyNum()) {
                        xResol = 2.54e-2f * xResol;
                    } else if (resXUnit == Unit.CENTIMETERS.getLegacyNum()) {
                        xResol = 10.0f * xResol;
                    } else if (resXUnit == Unit.ANGSTROMS.getLegacyNum()) {
                        xResol = 1.0e-7f * xResol;
                    } else if (resXUnit == Unit.NANOMETERS.getLegacyNum()) {
                        xResol = 1.0e-6f * xResol;
                    } else if (resXUnit == Unit.MICROMETERS.getLegacyNum()) {
                        xResol = 1.0e-3f * xResol;
                    } else if (resXUnit == Unit.METERS.getLegacyNum()) {
                        xResol = 1.0e3f * xResol;
                    } else if (resXUnit == Unit.KILOMETERS.getLegacyNum()) {
                        xResol = 1.0e6f * xResol;
                    } else if (resXUnit == Unit.MILES.getLegacyNum()) {
                        xResol = 1.6093e6f * xResol;
                    }
    
                    lineBytes = new String("scaling factor (mm/pixel) [1] := " + xResol + "\r\n").getBytes();
                    raFile.write(lineBytes);
                }
    
                resYUnit = baseInfo.getUnitsOfMeasure(1);
    
                if ((resYUnit == Unit.INCHES.getLegacyNum()) || (resYUnit == Unit.MILS.getLegacyNum()) ||
                        (resYUnit == Unit.CENTIMETERS.getLegacyNum()) ||
                        (resYUnit == Unit.ANGSTROMS.getLegacyNum()) || (resYUnit == Unit.NANOMETERS.getLegacyNum()) ||
                        (resYUnit == Unit.MICROMETERS.getLegacyNum()) || (resYUnit == Unit.MILLIMETERS.getLegacyNum()) ||
                        (resYUnit == Unit.METERS.getLegacyNum()) || (resYUnit == Unit.KILOMETERS.getLegacyNum()) ||
                        (resYUnit == Unit.MILES.getLegacyNum())) {
    
                    // convert to millimeters
                    yResol = baseInfo.getResolutions()[1];
    
                    if (resYUnit == Unit.INCHES.getLegacyNum()) {
                        yResol = 25.4f * yResol;
                    } else if (resYUnit == Unit.MILS.getLegacyNum()) {
                        yResol = 2.54e-2f * yResol;
                    } else if (resYUnit == Unit.CENTIMETERS.getLegacyNum()) {
                        yResol = 10.0f * yResol;
                    } else if (resYUnit == Unit.ANGSTROMS.getLegacyNum()) {
                        yResol = 1.0e-7f * yResol;
                    } else if (resYUnit == Unit.NANOMETERS.getLegacyNum()) {
                        yResol = 1.0e-6f * yResol;
                    } else if (resYUnit == Unit.MICROMETERS.getLegacyNum()) {
                        yResol = 1.0e-3f * yResol;
                    } else if (resYUnit == Unit.METERS.getLegacyNum()) {
                        yResol = 1.0e3f * yResol;
                    } else if (resYUnit == Unit.KILOMETERS.getLegacyNum()) {
                        yResol = 1.0e6f * yResol;
                    } else if (resYUnit == Unit.MILES.getLegacyNum()) {
                        yResol = 1.6093e6f * yResol;
                    }
    
                    lineBytes = new String("scaling factor (mm/pixel) [2] := " + yResol + "\r\n").getBytes();
                    raFile.write(lineBytes);
                }
    
                if (haveGatedStudy) {
                    elapsedStudyDuration = fileInfo.getElapsedStudyDuration();
    
                    if (elapsedStudyDuration != null) {
                        lineBytes = new String("study duration (elapsed) sec := " + elapsedStudyDuration + "\r\n").getBytes();
                        raFile.write(lineBytes);
                    }
    
                    observedCardiacCycles = fileInfo.getObservedCardiacCycles();
    
                    if (observedCardiacCycles != null) {
                        lineBytes = new String("number of cardiac cycles (observed) := " + observedCardiacCycles + "\r\n").getBytes();
                        raFile.write(lineBytes);
                    }
    
                    timeWindows = fileInfo.getTimeWindows();
    
                    if (timeWindows != null) {
                        timeWindowsNumber = Integer.valueOf(timeWindows).intValue();
                    }
    
                    lineBytes = new String("number of time windows := " + timeWindowsNumber + "\r\n").getBytes();
                    raFile.write(lineBytes);
                    timeWindowImages = fileInfo.getTimeWindowImages();
                    imageDuration = fileInfo.getImageDuration();
                    gatedFrameMode = fileInfo.getGatedFrameMode();
                    framingMethod = fileInfo.getFramingMethod();
                    maximumPixelCount = fileInfo.getMaximumPixelCount();
                    timeWindowLowerLimit = fileInfo.getTimeWindowLowerLimit();
                    timeWindowUpperLimit = fileInfo.getTimeWindowUpperLimit();
                    RRCycles = fileInfo.getRRCycles();
                    acquiredCardiacCycles = fileInfo.getAcquiredCardiacCycles();
                    acquiredStudyDuration = fileInfo.getAcquiredStudyDuration();
                    RRHistogram = fileInfo.getRRHistogram();
    
                    for (i = 0; i < timeWindowsNumber; i++) {
                        lineBytes = new String("!Gated Study (each time window) :=\r\n").getBytes();
                        raFile.write(lineBytes);
                        lineBytes = new String("!time window number := " + (i + 1) + "\r\n").getBytes();
                        raFile.write(lineBytes);
    
                        if (timeWindowImages != null) {
    
                            if (timeWindowImages[i] != null) {
                                lineBytes = new String("!number of images in time window := " + timeWindowImages[i] +
                                                       "\r\n").getBytes();
                                raFile.write(lineBytes);
                            }
                        }
    
                        if (imageDuration != null) {
    
                            if (imageDuration[i] != null) {
                                lineBytes = new String("!image duration (sec) := " + imageDuration[i] + "\r\n").getBytes();
                                raFile.write(lineBytes);
                            }
                        }
                        
                        if (gatedFrameMode != null) {
    
                            if (gatedFrameMode[i] != null) {
                                lineBytes = new String("!gated frame mode := " + gatedFrameMode[i] + "\r\n").getBytes();
                                raFile.write(lineBytes);
                            }
                        }
    
                        if (framingMethod != null) {
    
                            if (framingMethod[i] != null) {
                                lineBytes = new String("framing method := " + framingMethod[i] + "\r\n").getBytes();
                                raFile.write(lineBytes);
                            }
                        }
    
                        if (timeWindowLowerLimit != null) {
    
                            if (timeWindowLowerLimit[i] != null) {
                                lineBytes = new String("time window lower limit (sec) := " + timeWindowLowerLimit[i] +
                                                       "\r\n").getBytes();
                                raFile.write(lineBytes);
                            }
                        }
    
                        if (timeWindowUpperLimit != null) {
    
                            if (timeWindowUpperLimit[i] != null) {
                                lineBytes = new String("time window upper limit (sec) := " + timeWindowUpperLimit[i] +
                                                       "\r\n").getBytes();
                                raFile.write(lineBytes);
                            }
                        }
    
                        if (RRCycles != null) {
    
                            if (RRCycles[i] != null) {
                                lineBytes = new String("% R-R cycles acquired this window := " + RRCycles[i] + "\r\n").getBytes();
                                raFile.write(lineBytes);
                            }
                        }
    
                        if (acquiredCardiacCycles != null) {
    
                            if (acquiredCardiacCycles[i] != null) {
                                lineBytes = new String("number of cardiac cycles (acquired) := " +
                                                       acquiredCardiacCycles[i] + "\r\n").getBytes();
                                raFile.write(lineBytes);
                            }
                        }
    
                        if (acquiredStudyDuration != null) {
    
                            if (acquiredStudyDuration[i] != null) {
                                lineBytes = new String("study duration acquired (sec) := " + acquiredStudyDuration[i] +
                                                       "\r\n").getBytes();
                                raFile.write(lineBytes);
                            }
                        }
    
                        if (maximumPixelCount != null) {
    
                            if (maximumPixelCount[i] != null) {
                                lineBytes = new String("!maximum pixel count := " + maximumPixelCount[i] + "\r\n").getBytes();
                                raFile.write(lineBytes);
                            }
                        }
    
                        if (RRHistogram != null) {
    
                            if (RRHistogram[i] != null) {
                                lineBytes = new String("R-R histogram := " + RRHistogram[i] + "\r\n").getBytes();
                                raFile.write(lineBytes);
                            }
                        }
                    } // for (i = 0; i < timeWindowsNumber; i++)
                } // if (haveGatedStudy)
    
                resZUnit = baseInfo.getUnitsOfMeasure(2);
    
                if ((zDim > 1) &&
                        ((resZUnit == Unit.INCHES.getLegacyNum()) || (resZUnit == Unit.MILS.getLegacyNum()) ||
                             (resZUnit == Unit.CENTIMETERS.getLegacyNum()) ||
                             (resZUnit == Unit.ANGSTROMS.getLegacyNum()) || (resZUnit == Unit.NANOMETERS.getLegacyNum()) ||
                             (resZUnit == Unit.MICROMETERS.getLegacyNum()) || (resZUnit == Unit.MILLIMETERS.getLegacyNum()) ||
                             (resZUnit == Unit.METERS.getLegacyNum()) || (resZUnit == Unit.KILOMETERS.getLegacyNum()) ||
                             (resZUnit == Unit.MILES.getLegacyNum()))) {
    
                    // convert to millimeters
                    zResol = baseInfo.getResolutions()[2];
    
                    if (resZUnit == Unit.INCHES.getLegacyNum()) {
                        zResol = 25.4f * zResol;
                    } else if (resZUnit == Unit.MILS.getLegacyNum()) {
                        zResol = 2.54e-2f * zResol;
                    } else if (resZUnit == Unit.CENTIMETERS.getLegacyNum()) {
                        zResol = 10.0f * zResol;
                    } else if (resZUnit == Unit.ANGSTROMS.getLegacyNum()) {
                        zResol = 1.0e-7f * zResol;
                    } else if (resZUnit == Unit.NANOMETERS.getLegacyNum()) {
                        zResol = 1.0e-6f * zResol;
                    } else if (resZUnit == Unit.MICROMETERS.getLegacyNum()) {
                        zResol = 1.0e-3f * zResol;
                    } else if (resZUnit == Unit.METERS.getLegacyNum()) {
                        zResol = 1.0e3f * zResol;
                    } else if (resZUnit == Unit.KILOMETERS.getLegacyNum()) {
                        zResol = 1.0e6f * zResol;
                    } else if (resZUnit == Unit.MILES.getLegacyNum()) {
                        zResol = 1.6093e6f * zResol;
                    }
    
                    lineBytes = new String("scaling factor (mm/pixel) [3] := " + zResol + "\r\n").getBytes();
                    raFile.write(lineBytes);
                }
    
                if ((!simple) && (!haveGatedStudy)) {
                    startHorizontalBedPosition = fileInfo.getStartHorizontalBedPosition();
    
                    if (startHorizontalBedPosition != null) {
                        lineBytes = new String("start horizontal bed position (mm) := " + startHorizontalBedPosition +
                                               "\r\n").getBytes();
                        raFile.write(lineBytes);
                    }
    
                    lineBytes = new String("number of time frames := " + tDim + "\r\n").getBytes();
                    raFile.write(lineBytes);
                    projectionNumber = fileInfo.getProjectionNumber();
    
                    if (projectionNumber != null) {
                        lineBytes = new String("!number of projections := " + projectionNumber + "\r\n").getBytes();
                        raFile.write(lineBytes);
                    }
    
                    rotationExtent = fileInfo.getRotationExtent();
    
                    if (rotationExtent != null) {
                        lineBytes = new String("!extent of rotation := " + rotationExtent + "\r\n").getBytes();
                        raFile.write(lineBytes);
                    }
    
                    projectionTime = fileInfo.getProjectionTime();
    
                    if (projectionTime != null) {
                        lineBytes = new String("!time per projection (sec) := " + projectionTime + "\r\n").getBytes();
                        raFile.write(lineBytes);
                    }
    
                    studyDuration = fileInfo.getStudyDuration();
    
                    if (studyDuration != null) {
                        lineBytes = new String("study duration (sec) := " + studyDuration + "\r\n").getBytes();
                        raFile.write(lineBytes);
                    }
    
                    maximumPixelCount = fileInfo.getMaximumPixelCount();
    
                    if (maximumPixelCount != null) {
    
                        if (maximumPixelCount[maximumPixelCountIndex] != null) {
                            lineBytes = new String("!maximum pixel count := " +
                                                   maximumPixelCount[maximumPixelCountIndex++] + "\r\n").getBytes();
                            raFile.write(lineBytes);
                        }
                    }
    
                    patientOrientation = fileInfo.getPatientOrientation();
    
                    if (patientOrientation != null) {
                        lineBytes = new String("patient orientation := " + patientOrientation + "\r\n").getBytes();
                        raFile.write(lineBytes);
                    }
    
                    patientRotation = fileInfo.getPatientRotation();
    
                    if (patientRotation != null) {
                        lineBytes = new String("patient rotation := " + patientRotation + "\r\n").getBytes();
                        raFile.write(lineBytes);
                    }
    
                    PETStudyImageData = fileInfo.getPETStudyImageData();
    
                    if (PETStudyImageData) {
                        lineBytes = new String("!PET STUDY (Image data) :=\r\n").getBytes();
                        raFile.write(lineBytes);
                    }
    
                    spectStudyReconstructedData = fileInfo.getSpectStudyReconstructedData();
    
                    if (spectStudyReconstructedData) {
                        lineBytes = new String("!SPECT STUDY (reconstructed data) :=\r\n").getBytes();
                        raFile.write(lineBytes);
                    }
    
                    reconstructionMethod = fileInfo.getReconstructionMethod();
    
                    if (reconstructionMethod != null) {
                        lineBytes = new String("method of reconstruction := " + reconstructionMethod + "\r\n").getBytes();
                        raFile.write(lineBytes);
                    }
    
                    sliceNumber = fileInfo.getSliceNumber();
    
                    if (sliceNumber != null) {
                        lineBytes = new String("!number of slices := " + sliceNumber + "\r\n").getBytes();
                        raFile.write(lineBytes);
                    }
    
                    referenceFrameNumber = fileInfo.getReferenceFrameNumber();
    
                    if (referenceFrameNumber != null) {
                        lineBytes = new String("number of reference frame := " + referenceFrameNumber + "\r\n").getBytes();
                        raFile.write(lineBytes);
                    }
    
                    imageOrientation = baseInfo.getImageOrientation();
    
                    if (imageOrientation == FileInfoBase.AXIAL) {
                        lineBytes = new String("slice orientation := Transverse\r\n").getBytes();
                        raFile.write(lineBytes);
                    } else if (imageOrientation == FileInfoBase.CORONAL) {
                        lineBytes = new String("slice orientation := Coronal\r\n").getBytes();
                        raFile.write(lineBytes);
                    } else if (imageOrientation == FileInfoBase.SAGITTAL) {
                        lineBytes = new String("slice orientation := Sagittal\r\n").getBytes();
                        raFile.write(lineBytes);
                    }
    
                    sliceThickness = String.valueOf(fileInfo.getSliceThickness());
    
                    if (sliceThickness != null) {
                        lineBytes = new String("slice thickness (pixels) := " + sliceThickness + "\r\n").getBytes();
                        raFile.write(lineBytes);
                    }
    
                    centerCenter = fileInfo.getCenterCenter();
    
                    if (centerCenter != null) {
                        lineBytes = new String("centre-centre slice separation (pixels) := " + centerCenter + "\r\n").getBytes();
                        raFile.write(lineBytes);
                    }
    
                    filterName = fileInfo.getFilterName();
    
                    if (filterName != null) {
                        lineBytes = new String("filter name := " + filterName + "\r\n").getBytes();
                        raFile.write(lineBytes);
                    }
    
                    zAxisFilter = fileInfo.getZAxisFilter();
    
                    if (zAxisFilter != null) {
                        lineBytes = new String("z-axis filter := " + zAxisFilter + "\r\n").getBytes();
                        raFile.write(lineBytes);
                    }
    
                    appliedCorrections = fileInfo.getAppliedCorrections();
    
                    if (appliedCorrections != null) {
                        lineBytes = new String("applied corrections := " + appliedCorrections + "\r\n").getBytes();
                        raFile.write(lineBytes);
                    }
    
                    attenuationCorrection = fileInfo.getAttenuationCorrection();
    
                    if (attenuationCorrection != null) {
                        lineBytes = new String("method of attenuation correction := " + attenuationCorrection + "\r\n").getBytes();
                        raFile.write(lineBytes);
                    }
    
                    scatterCorrected = fileInfo.getScatterCorrected();
    
                    if (scatterCorrected != null) {
                        lineBytes = new String("scatter corrected := " + scatterCorrected + "\r\n").getBytes();
                        raFile.write(lineBytes);
                    }
    
                    scatterCorrectionMethod = fileInfo.getScatterCorrectionMethod();
    
                    if (scatterCorrectionMethod != null) {
                        lineBytes = new String("method of scatter correction := " + scatterCorrectionMethod + "\r\n").getBytes();
                        raFile.write(lineBytes);
                    }
    
                    obliqueReconstruction = fileInfo.getObliqueReconstruction();
    
                    if (obliqueReconstruction != null) {
                        lineBytes = new String("oblique reconstruction := " + obliqueReconstruction + "\r\n").getBytes();
                        raFile.write(lineBytes);
                    }
    
                    imageDataDescription = fileInfo.getImageDataDescription();
    
                    if (imageDataDescription) {
                        lineBytes = new String("!IMAGE DATA DESCRIPTION := \r\n").getBytes();
                        raFile.write(lineBytes);
                    }
    
                    indexNestingLevel = fileInfo.getIndexNestingLevel();
    
                    if (indexNestingLevel != null) {
                        lineBytes = new String("index nesting level := " + indexNestingLevel + "\r\n").getBytes();
                        raFile.write(lineBytes);
    
                        if (removeChars(indexNestingLevel).equalsIgnoreCase("{TIMEFRAME}")) {
                            iLevel = tEnd - tBegin + 1;
                        }
                    }
    
                    imageDuration = fileInfo.getImageDuration();
                    imageRelativeStartTime = fileInfo.getImageRelativeStartTime();
    
                    for (i = 0; i < iLevel; i++) {
    
                        if (imageDuration != null) {
    
                            if (imageDuration[i] != null) {
                                lineBytes = new String("image duration (sec) [" + (i + 1) + "] := " + imageDuration[i] +
                                                       "\r\n").getBytes();
                                raFile.write(lineBytes);
                            }
                        }
    
                        if (imageRelativeStartTime != null) {
    
                            if (imageRelativeStartTime[i] != null) {
                                lineBytes = new String("image relative start time (sec) [" + (i + 1) + "] := " +
                                                       imageRelativeStartTime[i] + "\r\n").getBytes();
                                raFile.write(lineBytes);
                            }
                        }
                    }
                } // if ((!simple) && (!haveGatedStudy))
            } // if ((!haveStaticStudy) && (!haveDynamicStudy)
    
            lineBytes = new String("!END OF INTERFILE :=\r\n\r\n").getBytes();
            raFile.write(lineBytes);
            raFile.close();
            raFile = new RandomAccessFile(dataFile, "rw");
            raFile.setLength(0); // necessary so that if this is an overwritten file there isn't junk at the end
            fileRW = new FileRawChunk(raFile, image.getFileInfo(0));
            bufferSize = image.getExtents()[0] * image.getExtents()[1];
    
            if (zDim > 1) {
                volSize = bufferSize * image.getExtents()[2];
            } else {
                volSize = 0;
            }
    
            sliceNum = 1;
            sliceTotal = zDim * tDim;
    
            // BOOLEAN is saved in the Interfile BIT format where BIT(size) = new byte[(size+7)>>3]
            // as opposed to the BitSet format where BitSet(size) = new long[(size+63)>>6].
            if (image.getType() == ModelStorageBase.BOOLEAN) {
                bufferBitSet = new BitSet(bufferSize);
                bufferByte = new byte[(bufferSize + 7) >> 3];
    
                for (t = tBegin; t <= tEnd; t++) {
                    timeOffset = t * volSize;
    
                    for (z = zBegin; z <= zEnd; z++, sliceNum++) {
    
                        try {
                            image.exportData(timeOffset + (z * bufferSize), bufferSize, bufferBitSet);
    
                            for (i = 0; i < bufferByte.length; i++) {
                                bufferByte[i] = 0;
                            }
    
                            for (i = 0; i < bufferSize; i++) {
    
                                if (bufferBitSet.get(i)) {
                                    bufferByte[i >> 3] |= (1 << (7-(i % 8)));
                                }
                            }
    
                            raFile.write(bufferByte);
                        } catch (IOException error) {
                            throw error;
                        }
    
                        fireProgressStateChanged(100 * sliceNum / sliceTotal);
                    }
                }
            } // if (image.getType() == ModelStorageBase.BOOLEAN)
            else { // not ModelStorageBase.BOOLEAN
    
                for (t = tBegin; t <= tEnd; t++) {
                    timeOffset = t * volSize;
    
                    for (z = zBegin; z <= zEnd; z++, sliceNum++) {
                        fileRW.writeImage(image, timeOffset + (z * bufferSize), timeOffset + (z * bufferSize) + bufferSize,
                                          0);
                        fireProgressStateChanged(100 * sliceNum / sliceTotal);
                    }
                }
            } // not ModelStorageBase.BOOLEAN
    
            raFile.close();
            if (!options.isMultiFile()) {
                break;
            }
            else if (options.isMultiFile()) {
                if (image.getNDims() == 4) {
                    if (tEnd == tEndOriginal) {
                        break;
                    } // if (tEnd == tEndOriginal)
                    else {
                        tBegin++;
                        tEnd++;
                        seq++;
                    }
                } // if (image.getNDims() == 4)
                else { // image.getNDims() == 3
                    if (zEnd == zEndOriginal) {
                        break;
                    }
                    else {
                        zBegin++;
                        zEnd++;
                        seq++;
                    }
                } // else image.getNDims() == 3
            } // else if (options.isMultiFile())
        } // while True;

    }

    /**
     * Removes white space and exclamation marks.
     *
     * @param   inString  String to remove characters from.
     *
     * @return  new String with removed white space.
     */
    private static String removeChars(String inString) {
        String outString;
        char temp;
        int i, j;

        if (inString != null) {
            char[] val = new char[inString.length()];

            for (i = 0, j = 0; i < inString.length(); i++) {
                temp = inString.charAt(i);

                if (temp > 0x21) {
                    val[j++] = temp;
                }
            }

            if (j > 0) {
                outString = new String(val, 0, j);

                return outString;
            } // if (j > 0)
            else {
                return null;
            }
        } // if (inString != null)
        else {
            return null;
        }
    }

    /**
     * Returns the key found in the string.
     *
     * @param   inString  String to search for key in.
     *
     * @return  Key, or <code>null</code> if not found
     */
    private static String returnKey(String inString) {
        int i;
        int j = 0;
        String outString;

        if (inString != null) {

            for (i = 0; i < (inString.length() - 1); i++) {

                if ((inString.charAt(i) == ':') && (inString.charAt(i + 1) == '=')) {
                    j = i;
                }
            }

            if (j > 0) {
                outString = inString.substring(0, j);

                return outString;
            } else {
                return null;
            }
        } else {
            return null;
        }
    }

    /**
     * DOCUMENT ME!
     *
     * @param   slice   DOCUMENT ME!
     * @param   buffer  DOCUMENT ME!
     *
     * @throws  IOException  DOCUMENT ME!
     */
    private void readBBuffer(int slice, float[] buffer) throws IOException {
        int i = 0;
        int j;
        int nBytes;
        byte[] byteBuffer;
        int progress, progressLength, mod;

        byteBuffer = new byte[(buffer.length + 7) >> 3];
        nBytes = (buffer.length + 7) >> 3;
        raFile.read(byteBuffer, 0, nBytes);
        progress = slice * buffer.length;
        progressLength = buffer.length * numberSlices;
        mod = progressLength / 100;


        for (j = 0; j < buffer.length; j++, i++) {

            if (((i + progress) % mod) == 0) {
                fireProgressStateChanged(Math.round((float) (i + progress) / progressLength * 100));
            }

            buffer[i] = byteBuffer[j >> 3] & (1 << (j % 8));
        }
    }

    /**
     * Reads a slice of data at a time and stores the results in the buffer.
     *
     * @param      slice   offset into the file stored in the dataOffset array
     * @param      buffer  buffer where the info is stored
     *
     * @exception  IOException  if there is an error reading the file
     */
    private void readBuffer(int slice, float[] buffer) throws IOException {
        int i = 0;
        int j;
        int nBytes;
        int b1, b2, b3, b4;
        byte[] byteBuffer;
        int progress, progressLength, mod;
        int tmpInt;

        switch (type) {

            case ModelStorageBase.BYTE:
                byteBuffer = new byte[buffer.length];
                nBytes = buffer.length;
                raFile.read(byteBuffer, 0, nBytes);
                progress = slice * buffer.length;
                progressLength = buffer.length * numberSlices;
                mod = progressLength / 100;

                for (j = 0; j < nBytes; j++, i++) {

                    if (((i + progress) % mod) == 0) {
                        fireProgressStateChanged(Math.round((float) (i + progress) / progressLength * 100));
                    }

                    buffer[i] = byteBuffer[j];
                }

                break;

            case ModelStorageBase.UBYTE:
                byteBuffer = new byte[buffer.length];
                nBytes = buffer.length;
                raFile.read(byteBuffer, 0, nBytes);
                progress = slice * buffer.length;
                progressLength = buffer.length * numberSlices;
                mod = progressLength / 100;

                for (j = 0; j < nBytes; j++, i++) {

                    if (((i + progress) % mod) == 0) {
                        fireProgressStateChanged(Math.round((float) (i + progress) / progressLength * 100));
                    }

                    buffer[i] = byteBuffer[j] & 0xff;
                }

                break;

            case ModelStorageBase.SHORT:
                byteBuffer = new byte[2 * buffer.length];
                nBytes = 2 * buffer.length;
                raFile.read(byteBuffer, 0, nBytes);
                progress = slice * buffer.length;
                progressLength = buffer.length * numberSlices;
                mod = progressLength / 10;

                for (j = 0; j < nBytes; j += 2, i++) {

                    if (((i + progress) % mod) == 0) {
                        fireProgressStateChanged(Math.round((float) (i + progress) / progressLength * 100));
                    }

                    b1 = getUnsignedByte(byteBuffer, j);
                    b2 = getUnsignedByte(byteBuffer, j + 1);

                    if (endianess) {
                        buffer[i] = (short) ((b1 << 8) + b2);
                    } else {
                        buffer[i] = (short) ((b2 << 8) + b1);
                    }

                } // for (j = 0; j < nBytes; j+=2, i++ )

                break;

            case ModelStorageBase.USHORT:
                byteBuffer = new byte[2 * buffer.length];
                nBytes = 2 * buffer.length;
                raFile.read(byteBuffer, 0, nBytes);
                progress = slice * buffer.length;
                progressLength = buffer.length * numberSlices;
                mod = progressLength / 10;

                for (j = 0; j < nBytes; j += 2, i++) {

                    if (((i + progress) % mod) == 0) {
                        fireProgressStateChanged(Math.round((float) (i + progress) / progressLength * 100));
                    }

                    b1 = getUnsignedByte(byteBuffer, j);
                    b2 = getUnsignedByte(byteBuffer, j + 1);

                    if (endianess) {
                        buffer[i] = ((b1 << 8) + b2);
                    } else {
                        buffer[i] = ((b2 << 8) + b1);
                    }

                } // for (j = 0; j < nBytes; j+=2, i++ )

                break;

            case ModelStorageBase.INTEGER:
                byteBuffer = new byte[4 * buffer.length];
                nBytes = 4 * buffer.length;
                raFile.read(byteBuffer, 0, nBytes);
                progress = slice * buffer.length;
                progressLength = buffer.length * numberSlices;
                mod = progressLength / 10;

                for (j = 0; j < nBytes; j += 4, i++) {

                    if (((i + progress) % mod) == 0) {
                        fireProgressStateChanged(Math.round((float) (i + progress) / progressLength * 100));
                    }

                    b1 = getUnsignedByte(byteBuffer, j);
                    b2 = getUnsignedByte(byteBuffer, j + 1);
                    b3 = getUnsignedByte(byteBuffer, j + 2);
                    b4 = getUnsignedByte(byteBuffer, j + 3);

                    if (endianess) {
                        buffer[i] = ((b1 << 24) | (b2 << 16) | (b3 << 8) | b4); // Big Endian
                    } else {
                        buffer[i] = ((b4 << 24) | (b3 << 16) | (b2 << 8) | b1); // Little Endian
                    }
                } // for (j =0; j < nBytes; j+=4, i++ )

                break;

            case ModelStorageBase.FLOAT:
                byteBuffer = new byte[4 * buffer.length];
                nBytes = 4 * buffer.length;
                raFile.read(byteBuffer, 0, nBytes);
                progress = slice * buffer.length;
                progressLength = buffer.length * numberSlices;
                mod = progressLength / 10;

                for (j = 0; j < nBytes; j += 4, i++) {

                    if (((i + progress) % mod) == 0) {
                        fireProgressStateChanged(Math.round((float) (i + progress) / progressLength * 100));
                    }

                    b1 = getUnsignedByte(byteBuffer, j);
                    b2 = getUnsignedByte(byteBuffer, j + 1);
                    b3 = getUnsignedByte(byteBuffer, j + 2);
                    b4 = getUnsignedByte(byteBuffer, j + 3);

                    if (endianess) {
                        tmpInt = ((b1 << 24) | (b2 << 16) | (b3 << 8) | b4); // Big Endian
                    } else {
                        tmpInt = ((b4 << 24) | (b3 << 16) | (b2 << 8) | b1); // Little Endian
                    }

                    buffer[i] = Float.intBitsToFloat(tmpInt);
                } // for (j =0; j < nBytes; j+=4, i++ )

                break;

        } // switch(type)

    }

    /**
     * Reads a slice of data at a time and stores the results in the buffer.
     *
     * @param      slice   offset into the file stored in the dataOffset array
     * @param      buffer  buffer where the info is stored
     *
     * @exception  IOException  if there is an error reading the file
     */
    private void readDBuffer(int slice, double[] buffer) throws IOException {
        int i = 0;
        int j;
        int nBytes;
        long b1, b2, b3, b4, b5, b6, b7, b8;
        byte[] byteBuffer;
        int progress, progressLength, mod;
        long tmpLong;

        byteBuffer = new byte[8 * buffer.length];
        nBytes = 8 * buffer.length;
        raFile.read(byteBuffer, 0, nBytes);
        progress = slice * buffer.length;
        progressLength = buffer.length * numberSlices;
        mod = progressLength / 10;


        for (j = 0; j < nBytes; j += 8, i++) {

            if (((i + progress) % mod) == 0) {
                fireProgressStateChanged(Math.round((float) (i + progress) / progressLength * 100));
            }

            b1 = getUnsignedByte(byteBuffer, j);
            b2 = getUnsignedByte(byteBuffer, j + 1);
            b3 = getUnsignedByte(byteBuffer, j + 2);
            b4 = getUnsignedByte(byteBuffer, j + 3);
            b5 = getUnsignedByte(byteBuffer, j + 4);
            b6 = getUnsignedByte(byteBuffer, j + 5);
            b7 = getUnsignedByte(byteBuffer, j + 6);
            b8 = getUnsignedByte(byteBuffer, j + 7);

            if (endianess) {
                tmpLong = ((b1 << 56) | (b2 << 48) | (b3 << 40) | (b4 << 32) | (b5 << 24) | (b6 << 16) | (b7 << 8) |
                               b8); // Big Endian
            } else {
                tmpLong = ((b8 << 56) | (b7 << 48) | (b6 << 40) | (b5 << 32) | (b4 << 24) | (b3 << 16) | (b2 << 8) |
                               b1); // Little Endian
            }

            buffer[i] = Double.longBitsToDouble(tmpLong);

        } // for (j =0; j < nBytes; j+=8, i++ )

    }

    /**
     * Reads a slice of data at a time and stores the results in the buffer.
     *
     * @param      slice   offset into the file stored in the dataOffset array
     * @param      buffer  buffer where the info is stored
     *
     * @exception  IOException  if there is an error reading the file
     */
    private void readLBuffer(int slice, long[] buffer) throws IOException {
        int i = 0;
        int j;
        int nBytes;
        long b1, b2, b3, b4, b5, b6, b7, b8;
        byte[] byteBuffer;
        int progress, progressLength, mod;

        if (type == ModelStorageBase.UINTEGER) { // reading 4 byte unsigned integers
            byteBuffer = new byte[4 * buffer.length];
            nBytes = 4 * buffer.length;
            raFile.read(byteBuffer, 0, nBytes);
            progress = slice * buffer.length;
            progressLength = buffer.length * numberSlices;
            mod = progressLength / 10;


            for (j = 0; j < nBytes; j += 4, i++) {

                if (((i + progress) % mod) == 0) {
                    fireProgressStateChanged(Math.round((float) (i + progress) / progressLength * 100));
                }

                b1 = getUnsignedByte(byteBuffer, j);
                b2 = getUnsignedByte(byteBuffer, j + 1);
                b3 = getUnsignedByte(byteBuffer, j + 2);
                b4 = getUnsignedByte(byteBuffer, j + 3);

                if (endianess) {
                    buffer[i] = ((b1 << 24) | (b2 << 16) | (b3 << 8) | b4) & 0xffffffffL;
                } else {
                    buffer[i] = ((b4 << 24) | (b3 << 16) | (b2 << 8) | b1) & 0xffffffffL;
                }
            } // for (j =0; j < nBytes; j+=4, i++ )
        } // if (type == ModelStorageBase.UINTEGER)
        else { // reading 8 byte integers
            byteBuffer = new byte[8 * buffer.length];
            nBytes = 8 * buffer.length;
            raFile.read(byteBuffer, 0, nBytes);
            progress = slice * buffer.length;
            progressLength = buffer.length * numberSlices;
            mod = progressLength / 10;


            for (j = 0; j < nBytes; j += 8, i++) {

                if (((i + progress) % mod) == 0) {
                    fireProgressStateChanged(Math.round((float) (i + progress) / progressLength * 100));
                }

                b1 = getUnsignedByte(byteBuffer, j);
                b2 = getUnsignedByte(byteBuffer, j + 1);
                b3 = getUnsignedByte(byteBuffer, j + 2);
                b4 = getUnsignedByte(byteBuffer, j + 3);
                b5 = getUnsignedByte(byteBuffer, j + 4);
                b6 = getUnsignedByte(byteBuffer, j + 5);
                b7 = getUnsignedByte(byteBuffer, j + 6);
                b8 = getUnsignedByte(byteBuffer, j + 7);

                if (endianess) {
                    buffer[i] = ((b1 << 56) | (b2 << 48) | (b3 << 40) | (b4 << 32) | (b5 << 24) | (b6 << 16) |
                                     (b7 << 8) | b8); // Big Endian
                } else {
                    buffer[i] = ((b8 << 56) | (b7 << 48) | (b6 << 40) | (b5 << 32) | (b4 << 24) | (b3 << 16) |
                                     (b2 << 8) | b1); // Little Endian
                }
            } // for (j =0; j < nBytes; j+=8, i++ )
        } // else reading 8 byte integers
    }

    /**
     * Reads lines of the file and strips comments indicated by the ; symbol until a nonnull String results or the end
     * of the file is reached.
     *
     * @return     the line read in
     *
     * @exception  IOException  if there is an error reading the file
     */
    private String readLine() throws IOException {
        String tempString = null;
        int index;

        while ((tempString == null) && (raFile.getFilePointer() < (fileLength - 1)) && (!foundEOF)) {

            try {
                tempString = raFile.readLine();
            } catch (EOFException error) {
                tempString = null;
                foundEOF = true;
            } catch (IOException error) {
                throw (error);
            }

            if (tempString != null) {
                index = tempString.indexOf(";");

                if (index != -1) {
                    tempString = tempString.substring(0, index);
                }

                tempString = tempString.trim();
            }

            if (tempString != null) {

                if (tempString.length() == 0) {
                    tempString = null;
                }
            }
        } // while

        return tempString;
    }

    /**
     * Returns the value found in the string.
     *
     * @param   inString  String to search for value in.
     *
     * @return  Value, or <code>null</code> if not found
     */
    private String returnValue(String inString) {

        // return the value
        int i;
        int j = 0;
        String outString;

        if (inString != null) {

            for (i = 0; i < (inString.length() - 1); i++) {

                if ((inString.charAt(i) == ':') && (inString.charAt(i + 1) == '=')) {
                    j = i;
                }
            }

            if ((j > 0) && (j < (inString.length() - 2))) {
                outString = inString.substring(j + 2, inString.length());

                return outString.trim();
            } else {
                return null;
            }
        } else {
            return null;
        }
    }


}
