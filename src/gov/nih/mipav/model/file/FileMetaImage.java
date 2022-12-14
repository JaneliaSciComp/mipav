package gov.nih.mipav.model.file;


import gov.nih.mipav.model.algorithms.utilities.*;
import gov.nih.mipav.model.file.FileInfoBase.Unit;
import gov.nih.mipav.model.structures.*;

import Jama.*;

import gov.nih.mipav.view.*;
import gov.nih.mipav.view.dialogs.*;

import java.io.*;
import java.util.zip.GZIPInputStream;
import java.util.zip.Inflater;
import java.util.zip.ZipInputStream;
import java.text.*;


public class FileMetaImage extends FileBase {
    /** DOCUMENT ME! */
    private String fileDir;

    /** DOCUMENT ME! */
    private FileInfoMetaImage fileInfo = null;

    /** DOCUMENT ME! */
    private String fileName;
    
    private long headerSize;
    
    private long currentLocation;

    /** DOCUMENT ME! */
    private File file;
    
    /** DOCUMENT ME! */
    private ModelImage image;
    
    /** DOCUMENT ME! */
    private float[] origin = null;
    
    private float[] resolutions;
    
    private int numValues;
    
    /** DOCUMENT ME! */
    private String fieldSeparator = new String(" ");
    
    private String category;
    
    private String baseName;
    
    private String[] values = new String[10];
    
    private int nDims = -1;
    
    private String defaultFileDataName;
    private File defaultFileData;
    private boolean defaultFileDataExists = false;
    private boolean asciiFormat = false;
    private boolean compressedData = false;
    private int extents[] = null;
    private TransMatrix matrix = null;
    private int compressedDataSize = -1;
    private boolean sepFound = true;
    private Inflater zlibDecompresser = null;
    
    /** If true, header and data both stored in .mha file.
     *  If false, header stored in filename.mhd and data
     *  stored in filename.raw. */
    private boolean oneFile;
    private File fileHeader;
    
    /**
     * Constructs new file object.
     *
     * @param  fName  File name.
     * @param  fDir   File directory.
     */
    public FileMetaImage(String fName, String fDir) {
        
        int index = fName.length();

        for (int i = fName.length() - 1; i >= 0; i--) {

            if (fName.charAt(i) == '.') {
                index = i;

                break;
            }
        }
        
        baseName = fName.substring(0, index);
        defaultFileDataName = fName.substring(0, index) + ".raw";
        defaultFileData = new File(fDir + defaultFileDataName);
        if (defaultFileData.exists()) {
            defaultFileDataExists = true;
            Preferences.debug("Default data file = " + defaultFileDataName + " exists\n", Preferences.DEBUG_FILEIO);
        }
            
        fileName = fName;
        fileDir = fDir;
    }
    
    /**
     * reads the MetaImatge file header and data.
     * 
     * @exception IOException if there is an error reading the file
     * 
     * @param one DOCUMENT ME!
     * 
     * @return DOCUMENT ME!
     */
    public ModelImage readImage(final boolean one) throws IOException {
        int i;
        double matValues[];
        String dataString = null;
        int dataLength;
        boolean haveRL = false;
        boolean haveAP = false;
        boolean haveIS = false;
        boolean haveAnatomicalOrientation = false;
        double centerOfRotation[] = null;
        int axisOrientation[] = null;
        int dataType = ModelStorageBase.BYTE;
        int numChannels = 1;
        String fileDataName = null;
        int bytesPerPixel = 1;
        String rawDataName;
        byte buffer[];
        //int bufSize;
        //boolean memoryError;
        byte buf[] = null;
        byte decomp[];
        int resultLength;
       
        try {

            file = new File(fileDir + fileName);
            fileInfo = new FileInfoMetaImage(fileName, fileDir, FileUtility.METAIMAGE);
            raFile = new RandomAccessFile(file, "r");
            headerSize = raFile.length();
            
            currentLocation = raFile.getFilePointer();
            while (currentLocation < headerSize-1) {
                readLine();
                if (!sepFound) {
                    break;
                }
                currentLocation = raFile.getFilePointer();
                if (category.equalsIgnoreCase("ObjectType")) {
                    if (numValues == 1) {
                        Preferences.debug("ObjectType has the expected 1 value\n", Preferences.DEBUG_FILEIO);
                        if (values[0].equalsIgnoreCase("Image")) {
                            Preferences.debug("ObjectType has the expected value of Image\n", Preferences.DEBUG_FILEIO);
                        }
                        else{
                            Preferences.debug("ObjectType unexpectedly has value = " + values[0] + "\n", Preferences.DEBUG_FILEIO);
                        }
                    }
                    else {
                        Preferences.debug("ObjectType unexpectedly has " + numValues + "values\n", Preferences.DEBUG_FILEIO);
                        Preferences.debug("Those values are:\n", Preferences.DEBUG_FILEIO);
                        for (i = 0; i < numValues; i++) {
                            Preferences.debug("Value[" + i + "] = " + values[i] + "\n", Preferences.DEBUG_FILEIO);
                        }
                    }
                } // if (category.equalsIgnoreCase("ObjectType"))
                else if (category.equalsIgnoreCase("NDims")) {
                    if (numValues == 1) {
                        Preferences.debug("NDims has the expected 1 value\n", Preferences.DEBUG_FILEIO);
                        try {
                            nDims = Integer.parseInt(values[0].trim());
                            Preferences.debug("nDims = " + nDims + "\n", Preferences.DEBUG_FILEIO);
                            if (nDims <= 0) {
                                raFile.close();
                                MipavUtil.displayError("NDims has an illegal value of " + nDims);
                                throw new IOException();
                            }
                            if ((nDims < 2) || (nDims > 5)) {
                                raFile.close();
                                MipavUtil.displayError("MIPAV cannot handle NDims values of " + nDims);
                                throw new IOException();
                            }
                        } catch (final NumberFormatException e) {
                            raFile.close();

                            MipavUtil.displayError("Instead of integer NDims line has " + values[0]);
                            throw new IOException();
                        }
                    }
                    else {
                        Preferences.debug("NDims unexpectedly has " + numValues + "values\n", Preferences.DEBUG_FILEIO);
                        Preferences.debug("Those values are:\n", Preferences.DEBUG_FILEIO);
                        for (i = 0; i < numValues; i++) {
                            Preferences.debug("Value[" + i + "] = " + values[i] + "\n", Preferences.DEBUG_FILEIO);
                        }
                        MipavUtil.displayError("NDims has " + numValues + " values");
                        throw new IOException();
                    }    
                } // else if (category.equalsIgnoreCase("NDims"))
                else if (category.equalsIgnoreCase("BinaryData")) {
                    if (numValues == 1) {
                        Preferences.debug("BinaryData has the expected 1 value\n", Preferences.DEBUG_FILEIO);
                        if (values[0].equalsIgnoreCase("True")) {
                            Preferences.debug("BinaryData = True\n", Preferences.DEBUG_FILEIO);
                        }
                        else if (values[0].equalsIgnoreCase("False")) {
                            Preferences.debug("BinaryData = false.  The data is stored in asciiFormat\n", Preferences.DEBUG_FILEIO);
                            asciiFormat = true;
                        }
                        else{
                            Preferences.debug("BinaryData unexpectedly has value = " + values[0] + "\n", Preferences.DEBUG_FILEIO);
                        }
                    }
                    else {
                        Preferences.debug("BinaryData unexpectedly has " + numValues + "values\n", Preferences.DEBUG_FILEIO);
                        Preferences.debug("Those values are:\n", Preferences.DEBUG_FILEIO);
                        for (i = 0; i < numValues; i++) {
                            Preferences.debug("Value[" + i + "] = " + values[i] + "\n", Preferences.DEBUG_FILEIO);
                        }
                    }    
                } // else if (category.equalsIgnoreCase("BinaryData"))
                else if ((category.equalsIgnoreCase("ElementByteOrderMSB")) ||
                         (category.equalsIgnoreCase("BinaryDataByteOrderMSB"))) {
                    if (numValues == 1) {
                        Preferences.debug("BinaryDataByteOrderMSB has the expected 1 value\n", Preferences.DEBUG_FILEIO);
                        if (values[0].equalsIgnoreCase("False")) {
                            Preferences.debug("Endianess = little endian\n", Preferences.DEBUG_FILEIO);
                            fileInfo.setEndianess(LITTLE_ENDIAN);
                        }
                        else if (values[0].equalsIgnoreCase("True")) {
                            Preferences.debug("Endianess = big endian\n", Preferences.DEBUG_FILEIO);
                            fileInfo.setEndianess(BIG_ENDIAN);
                        }
                        else{
                            Preferences.debug("BinaryDataByteOrderMSB unexpectedly has value = " + values[0] + "\n", Preferences.DEBUG_FILEIO);
                            MipavUtil.displayError("BinaryDataByteOrderMSB has " + values[0]);
                            throw new IOException();
                        }
                    }
                    else {
                        Preferences.debug("BinaryDataByteOrderMSB unexpectedly has " + numValues + "values\n", Preferences.DEBUG_FILEIO);
                        Preferences.debug("Those values are:\n", Preferences.DEBUG_FILEIO);
                        for (i = 0; i < numValues; i++) {
                            Preferences.debug("Value[" + i + "] = " + values[i] + "\n", Preferences.DEBUG_FILEIO);
                        }
                        MipavUtil.displayError("BinaryDataByteOrderMSB has " + numValues + " values");
                        throw new IOException();
                    }
    
                } // else if (category.equalsIgnoreCase("ElementByteOrderMSB"))
                else if (category.equals("CompressedData")) {
                    if (numValues == 1) {
                        Preferences.debug("CompressedData has the expected 1 value\n", Preferences.DEBUG_FILEIO);
                        if (values[0].equalsIgnoreCase("False")) {
                            Preferences.debug("CompressedData = false\n",Preferences.DEBUG_FILEIO);
                            compressedData = false;
                        }
                        else if (values[0].equalsIgnoreCase("True")) {
                            Preferences.debug("CompressedData = true\n", Preferences.DEBUG_FILEIO); 
                            compressedData = true;
                        }
                        else{
                            Preferences.debug("CompressedData unexpectedly has value = " + values[0] + "\n", Preferences.DEBUG_FILEIO);
                        }
                    }
                    else {
                        Preferences.debug("CompressedData unexpectedly has " + numValues + "values\n", Preferences.DEBUG_FILEIO);
                        Preferences.debug("Those values are:\n", Preferences.DEBUG_FILEIO);
                        for (i = 0; i < numValues; i++) {
                            Preferences.debug("Value[" + i + "] = " + values[i] + "\n", Preferences.DEBUG_FILEIO);
                        }
                    }    
                } // else if (category.equals("CompressedData"))
                else if (category.equals("CompressedDataSize")) {
                    if (numValues == 1) {
                        Preferences.debug("CompressedDataSize has the expected 1 value\n", Preferences.DEBUG_FILEIO);
                        compressedDataSize = Integer.parseInt(values[0].trim());
                        Preferences.debug("Compressed data size = " + compressedDataSize + "\n", Preferences.DEBUG_FILEIO);
                        if (compressedDataSize <= 0) {
                            raFile.close();
                            MipavUtil.displayError("compressedDataSize has an illegal value of " + compressedDataSize);
                            throw new IOException();
                        }
                    }
                    else {
                        Preferences.debug("CompressedDataSize unexpectedly has " + numValues + "values\n", Preferences.DEBUG_FILEIO);
                        Preferences.debug("Those values are:\n", Preferences.DEBUG_FILEIO);
                        for (i = 0; i < numValues; i++) {
                            Preferences.debug("Value[" + i + "] = " + values[i] + "\n", Preferences.DEBUG_FILEIO);
                        }
                    }    
                } // else if (category.equals("CompressedDataSize"))
                else if ((category.equalsIgnoreCase("TransformMatrix")) || (category.equalsIgnoreCase("Orientation"))||
                         (category.equalsIgnoreCase("Rotation"))) {
                    if ((nDims == 3) && (numValues != 9)) {
                        MipavUtil.displayError("nDims = 3 but numValues = " + numValues + " instead of 9 for TransformMatrix");
                        throw new IOException();
                    }
                    if ((nDims == 2) && (numValues != 4)) {
                        Preferences.debug("Those values are:\n", Preferences.DEBUG_FILEIO);
                        for (i = 0; i < numValues; i++) {
                            Preferences.debug("Value[" + i + "] = " + values[i] + "\n", Preferences.DEBUG_FILEIO);
                        }
                        //MipavUtil.displayError("nDims = 2 but numValues = " + numValues + " instead of 4 for TransformMatrix");
                        throw new IOException();    
                    }
                    matValues = new double[numValues];
                    for (i = 0; i < numValues; i++) {
                        try {
                            matValues[i] = Double.parseDouble(values[i].trim());
                            Preferences.debug("TransformMatrix[" + i + "] = " + matValues[i] + "\n", Preferences.DEBUG_FILEIO);
                        }
                        catch (final NumberFormatException e) {
                            raFile.close();

                            MipavUtil.displayError("Instead of double TransformMatrix[" + i + "] =  has " + values[i]);
                            throw new IOException();
                        }
                    }
                    if (numValues == 9) {
                        matrix = new TransMatrix(4);
                        matrix.set(0, 0, matValues[0]);
                        matrix.set(0, 1, matValues[1]);
                        matrix.set(0, 2, matValues[2]);
                        matrix.set(1, 0, matValues[3]);
                        matrix.set(1, 1, matValues[4]);
                        matrix.set(1, 2, matValues[5]);
                        matrix.set(2, 0, matValues[6]);
                        matrix.set(2, 1, matValues[7]);
                        matrix.set(2, 2, matValues[8]);
                    } 
                    if (numValues == 4) {
                        matrix = new TransMatrix(3);
                        matrix.set(0, 0, matValues[0]);
                        matrix.set(0, 1, matValues[1]);
                        matrix.set(1, 0, matValues[2]);
                        matrix.set(1, 1, matValues[3]);
                    }
                } // else if ((category.equalsIgnoreCase("TransformMatrix")) || (category.equalsIgnoreCase("Orientation"))||
                else if ((category.equalsIgnoreCase("Offset")) || (category.equalsIgnoreCase("Origin")) ||
                        (category.equalsIgnoreCase("Position"))) {
                    if ((nDims >= 2)  && (numValues < nDims)) {
                        Preferences.debug("nDims = " + nDims + " but numValues = " + numValues + " for Offset\n", Preferences.DEBUG_FILEIO);
                    }
                    origin = new float[nDims];
                    for (i = 0; i < Math.min(nDims, numValues); i++) {
                        try {
                            origin[i] = Float.parseFloat(values[i].trim());
                            Preferences.debug("Origin[" + i + "] = " + origin[i] + "\n", Preferences.DEBUG_FILEIO);
                        }
                        catch (final NumberFormatException e) {
                            raFile.close();

                            MipavUtil.displayError("Instead of float offset[" + i + "] =  has " + values[i]);
                            throw new IOException();
                        }
                    }
                    fileInfo.setOrigin(origin);
                } // else if ((category.equalsIgnoreCase("Offset")) || (category.equalsIgnoreCase("Origin")) ||
                else if (category.equalsIgnoreCase("CenterOfRotation")) {
                    centerOfRotation = new double[numValues];
                    for (i = 0; i < numValues; i++) {
                        try {
                            centerOfRotation[i] = Double.parseDouble(values[i].trim());
                            Preferences.debug("Center of rotation[" + i + "] = " + centerOfRotation[i] + "\n", Preferences.DEBUG_FILEIO);
                        }
                        catch (final NumberFormatException e) {
                            raFile.close();

                            MipavUtil.displayError("Instead of double centerOfRotation[" + i + "] =  has " + values[i]);
                            throw new IOException();
                        }    
                    } // for (i = 0; i < numValues; i++)
                    fileInfo.setCenterOfRotation(centerOfRotation);
                } // else if (category.equalsIgnoreCase("CenterOfRotation")) 
                else if (category.equalsIgnoreCase("AnatomicalOrientation")) {
                    if (numValues == 1) {
                        Preferences.debug("AnatomicalOrientation has the expected 1 value\n", Preferences.DEBUG_FILEIO);
                        if (values[0].trim().length() == 3) {
                            Preferences.debug("values[0].trim().length() == 3 as expected\n", Preferences.DEBUG_FILEIO);
                            axisOrientation = new int[3];
                            for (i = 0; i < 3;  i++) {
                                if (values[0].trim().substring(i, i+1).equalsIgnoreCase("R")) {
                                    axisOrientation[i] = FileInfoBase.ORI_R2L_TYPE;
                                    Preferences.debug("axisOrientation[" + i + "] = R to L\n", Preferences.DEBUG_FILEIO);
                                    haveRL = true;
                                }
                                else if (values[0].trim().substring(i, i+1).equalsIgnoreCase("L")) {
                                    axisOrientation[i] = FileInfoBase.ORI_L2R_TYPE; 
                                    Preferences.debug("axisOrientation[" + i + "] = L to R\n", Preferences.DEBUG_FILEIO);
                                    haveRL = true;
                                }
                                else if (values[0].trim().substring(i, i+1).equalsIgnoreCase("A")) {
                                    axisOrientation[i] = FileInfoBase.ORI_A2P_TYPE; 
                                    Preferences.debug("axisOrientation[" + i + "] = A to P\n", Preferences.DEBUG_FILEIO);
                                    haveAP = true;
                                }
                                else if (values[0].trim().substring(i, i+1).equalsIgnoreCase("P")) {
                                    axisOrientation[i] = FileInfoBase.ORI_P2A_TYPE;   
                                    Preferences.debug("axisOrientation[" + i + "] = P to A\n", Preferences.DEBUG_FILEIO);
                                    haveAP = true;
                                }
                                else if (values[0].trim().substring(i, i+1).equalsIgnoreCase("I")) {
                                    axisOrientation[i] = FileInfoBase.ORI_I2S_TYPE;  
                                    Preferences.debug("axisOrientation[" + i + "] = I to S\n", Preferences.DEBUG_FILEIO);
                                    haveIS = true;
                                }
                                else if (values[0].trim().substring(i, i+1).equalsIgnoreCase("S")) {
                                    axisOrientation[i] = FileInfoBase.ORI_S2I_TYPE;
                                    Preferences.debug("axisOrientation[" + i + "] = S to I\n", Preferences.DEBUG_FILEIO);
                                    haveIS = true;
                                }
                                else {
                                    axisOrientation[i] = FileInfoBase.ORI_UNKNOWN_TYPE;
                                    Preferences.debug("axisOrientation[" + i + "] = unknown orientation\n", Preferences.DEBUG_FILEIO);
                                }
                            }
                            if (haveRL && haveAP && haveIS) {
                                haveAnatomicalOrientation = true;
                                fileInfo.setAxisOrientation(axisOrientation);
                                if ((axisOrientation[2] == FileInfoBase.ORI_I2S_TYPE) || (axisOrientation[2] == FileInfoBase.ORI_S2I_TYPE)) {
                                    fileInfo.setImageOrientation(FileInfoBase.AXIAL);
                                    Preferences.debug("Image orientation == AXIAL\n", Preferences.DEBUG_FILEIO);
                                }
                                else if ((axisOrientation[2] == FileInfoBase.ORI_L2R_TYPE) || (axisOrientation[2] == FileInfoBase.ORI_R2L_TYPE)) {
                                    fileInfo.setImageOrientation(FileInfoBase.SAGITTAL);
                                    Preferences.debug("Image orientation == SAGITTAL\n", Preferences.DEBUG_FILEIO);
                                }
                                else {
                                    fileInfo.setImageOrientation(FileInfoBase.CORONAL);
                                    Preferences.debug("Image orientation == CORONAL\n", Preferences.DEBUG_FILEIO);    
                                }
                            }
                        }
                        else if (values[0].trim().equals("??")) {
                            fileInfo.setImageOrientation(FileInfoBase.UNKNOWN_ORIENT);
                            Preferences.debug("Image orientation == UNKNOWN_ORIENT\n", Preferences.DEBUG_FILEIO);
                        }
                        else {
                            Preferences.debug("values[0].trim().length() unexpectedly == " + values[0].trim().length() + "\n",
                                    Preferences.DEBUG_FILEIO);
                            Preferences.debug("values[0] = " + values[0] + "\n", Preferences.DEBUG_FILEIO);
                        }
                    }
                    else {
                        Preferences.debug("AnatomicalOrientation unexpectedly has " + numValues + "values\n", Preferences.DEBUG_FILEIO);
                        Preferences.debug("Those values are:\n", Preferences.DEBUG_FILEIO);
                        for (i = 0; i < numValues; i++) {
                            Preferences.debug("Value[" + i + "] = " + values[i] + "\n", Preferences.DEBUG_FILEIO);
                        }
                    }    
                } // else if (category.equalsIgnoreCase("AnatomicalOrientation"))
                else if (category.equalsIgnoreCase("ElementSpacing")) {
                    if ((nDims >= 2)  && (numValues < nDims)) {
                        Preferences.debug("nDims = " + nDims + " numValues = " + numValues + " for ElementSpacing\n", Preferences.DEBUG_FILEIO);
                    }
                    resolutions = new float[nDims];
                    for (i = 0; i < nDims; i++) {
                        resolutions[i] = 1.0f;
                    }
                    for (i = 0; i < Math.min(nDims, numValues); i++) {
                        try {
                            resolutions[i] = Float.parseFloat(values[i].trim());
                            Preferences.debug("Resolutions[" + i + "] = " + resolutions[i] + "\n", Preferences.DEBUG_FILEIO);
                        }
                        catch (final NumberFormatException e) {
                            raFile.close();

                            MipavUtil.displayError("Instead of float ElementSpacing[" + i + "] =  has " + values[i]);
                            throw new IOException();
                        }
                    }
                    fileInfo.setResolutions(resolutions);
                    // The units used are millimeters
                    for (i = 0; i <= Math.min(3, nDims); i++) {
                        fileInfo.setUnitsOfMeasure(FileInfoBase.MILLIMETERS, i);
                    }
                } //  else if (category.equalsIgnoreCase("ElementSpacing"))
                else if (category.equalsIgnoreCase("DimSize")) {
                    if ((nDims >= 2)  && (numValues != nDims)) {
                        MipavUtil.displayError(nDims + " != " + numValues + " for DimSize");
                        throw new IOException();
                    }
                    else {
                        nDims = numValues;
                    }
                    extents = new int[nDims];
                    for (i = 0; i < nDims; i++) {
                        try {
                            extents[i] = Integer.parseInt(values[i].trim());
                            Preferences.debug("Extents[" + i + "] = " + extents[i] + "\n", Preferences.DEBUG_FILEIO);
                        }
                        catch (final NumberFormatException e) {
                            raFile.close();

                            MipavUtil.displayError("Instead of integer extents[" + i + "] =  has " + values[i]);
                            throw new IOException();
                        }
                    }
                    fileInfo.setExtents(extents);
                } // else if (category.equalsIgnoreCase("DimSize"))
                else if (category.equalsIgnoreCase("ElementNumberOfChannels")) {
                    if (numValues == 1) {
                        Preferences.debug("ElementNumberOfChannels has the expected 1 value\n", Preferences.DEBUG_FILEIO);
                        try {
                            numChannels = Integer.parseInt(values[0].trim());
                            Preferences.debug("numChannels = " + numChannels + "\n", Preferences.DEBUG_FILEIO);
                            if (numChannels <= 0) {
                                raFile.close();
                                MipavUtil.displayError("numChannels has an illegal value of " + numChannels);
                                throw new IOException();
                            }
                        } catch (final NumberFormatException e) {
                            raFile.close();

                            MipavUtil.displayError("Instead of integer ElementNumberOfChannels has " + values[0]);
                            throw new IOException();
                        }
                    }
                    else {
                        Preferences.debug("ElementNumberOfChannels unexpectedly has " + numValues + "values\n", Preferences.DEBUG_FILEIO);
                        Preferences.debug("Those values are:\n", Preferences.DEBUG_FILEIO);
                        for (i = 0; i < numValues; i++) {
                            Preferences.debug("Value[" + i + "] = " + values[i] + "\n", Preferences.DEBUG_FILEIO);
                        }
                    }        
                } // else if (category.equalsIgnoreCase("ElementNumberOfChannels"))
                else if (category.equalsIgnoreCase("ElementType")) {
                    if (numValues == 1) {
                        Preferences.debug("ElementType has the expected 1 value\n", Preferences.DEBUG_FILEIO);
                        dataString = values[0].trim();
                    }
                    else {
                        Preferences.debug("ElementType unexpectedly has " + numValues + "values\n", Preferences.DEBUG_FILEIO);
                        Preferences.debug("Those values are:\n", Preferences.DEBUG_FILEIO);
                        for (i = 0; i < numValues; i++) {
                            Preferences.debug("Value[" + i + "] = " + values[i] + "\n", Preferences.DEBUG_FILEIO);
                        }
                    }    
                } // else if (category.equalsIgnoreCase("ElementType"))
                else if (category.equalsIgnoreCase("ElementDataFile")) {
                    // Should be the last tag read
                    // If ElementDataFile = LOCAL, data should start in the next line.
                    if (numValues == 1) {
                        Preferences.debug("ElementDataFile has the expected 1 value\n", Preferences.DEBUG_FILEIO);
                        fileDataName = values[0].trim();
                        Preferences.debug("ElementDataFile = " + fileDataName + "\n", Preferences.DEBUG_FILEIO);
                    }
                    else {
                        Preferences.debug("ElementDataFile unexpectedly has " + numValues + "values\n", Preferences.DEBUG_FILEIO);
                        Preferences.debug("Those values are:\n", Preferences.DEBUG_FILEIO);
                        for (i = 0; i < numValues; i++) {
                            Preferences.debug("Value[" + i + "] = " + values[i] + "\n", Preferences.DEBUG_FILEIO);
                        }
                    } 
                    currentLocation = raFile.getFilePointer();
                    break;
                } // else if (category.equalsIgnoreCase("ElementDataFile"))
            } // while (currentLocation < headerSize-1)
        
            if ((matrix != null) && (origin !=  null)) {
                if (nDims == 2) {
                    matrix.set(0, 2, origin[0]);
                    matrix.set(1, 2, origin[1]);
                }
                if (nDims >= 3) {
                    matrix.set(0, 3, origin[0]);
                    matrix.set(1, 3, origin[1]);
                    matrix.set(2, 3, origin[2]);
                }
                if (haveAnatomicalOrientation) {
                    matrix.setTransformID(TransMatrix.TRANSFORM_SCANNER_ANATOMICAL);    
                }
                else {
                    matrix.setTransformID(TransMatrix.TRANSFORM_UNKNOWN);
                }
            } // if ((matrix != null) && (origin !=  null))
            if (dataString != null) {
                if (numChannels == 1) {
                    if (dataString.equalsIgnoreCase("MET_CHAR")) {
                        dataType = ModelStorageBase.BYTE;
                        Preferences.debug("Data type = ModelStorageBase.BYTE\n", Preferences.DEBUG_FILEIO);
                        bytesPerPixel = 1;
                    }
                    else if (dataString.equalsIgnoreCase("MET_UCHAR")) {
                        dataType = ModelStorageBase.UBYTE;
                        Preferences.debug("Data type = ModelStorageBase.UBYTE\n", Preferences.DEBUG_FILEIO);
                        bytesPerPixel = 1;
                    }
                    else if (dataString.equalsIgnoreCase("MET_SHORT")) {
                        dataType = ModelStorageBase.SHORT;
                        Preferences.debug("Data type = ModelStorageBase.SHORT\n", Preferences.DEBUG_FILEIO);
                        bytesPerPixel = 2;
                    }
                    else if (dataString.equalsIgnoreCase("MET_USHORT")) {
                        dataType = ModelStorageBase.USHORT;
                        Preferences.debug("Data type = ModelStorageBase.USHORT\n", Preferences.DEBUG_FILEIO);
                        bytesPerPixel = 2;
                    }
                    else if (dataString.equalsIgnoreCase("MET_INT")) {
                        dataType = ModelStorageBase.INTEGER;
                        Preferences.debug("Data type = ModelStorageBase.INTEGER\n", Preferences.DEBUG_FILEIO);
                        bytesPerPixel = 4;
                    }
                    else if (dataString.equalsIgnoreCase("MET_UINT")) {
                        dataType = ModelStorageBase.UINTEGER;
                        Preferences.debug("Data type = ModelStorageBase.UINTEGER\n", Preferences.DEBUG_FILEIO);
                        bytesPerPixel = 4;
                    }
                    else if (dataString.equalsIgnoreCase("MET_LONG")) {
                        dataType = ModelStorageBase.LONG;
                        Preferences.debug("Data type = ModelStorageBase.LONG\n", Preferences.DEBUG_FILEIO);
                        bytesPerPixel = 8;
                    }
                    else if (dataString.equalsIgnoreCase("MET_ULONG")) {
                        dataType = ModelStorageBase.LONG;
                        Preferences.debug("Data type = Originally ULONG.  Set to ModelStorageBase.LONG\n", Preferences.DEBUG_FILEIO);
                        bytesPerPixel = 8;
                    }
                    else if (dataString.equalsIgnoreCase("MET_FLOAT")) {
                        dataType = ModelStorageBase.FLOAT;
                        Preferences.debug("Data type = ModelStorageBase.FLOAT\n", Preferences.DEBUG_FILEIO);
                        bytesPerPixel = 4;
                    }
                    else if (dataString.equalsIgnoreCase("MET_DOUBLE")) {
                        dataType = ModelStorageBase.DOUBLE;
                        Preferences.debug("Data type = ModelStorageBase.DOUBLE\n", Preferences.DEBUG_FILEIO);
                        bytesPerPixel = 8;
                    }
                    else {
                        raFile.close();
                        MipavUtil.displayError("Cannot handle " + dataString + " data type for numChannels = 1");
                        throw new IOException();
                    }
                } // if (numChannels == 1)
                else if (numChannels == 2) {
                    if (dataString.equalsIgnoreCase("MET_FLOAT")) {
                        dataType = ModelStorageBase.COMPLEX;
                        Preferences.debug("Data type = ModelStorageBase.COMPLEX\n", Preferences.DEBUG_FILEIO);
                        bytesPerPixel = 8;
                    }    
                }
                else if (numChannels == 3) {
                    if (dataString.equals("MET_UCHAR_ARRAY")) {
                        dataType = ModelStorageBase.ARGB;
                        Preferences.debug("Data type = ModelStorageBase.ARGB\n", Preferences.DEBUG_FILEIO);
                        bytesPerPixel = 3;
                    }
                    else if (dataString.equals("MET_USHORT_ARRAY")) {
                        dataType = ModelStorageBase.ARGB_USHORT;
                        Preferences.debug("Data type = ModelStorageBase.ARGB_USHORT\n", Preferences.DEBUG_FILEIO); 
                        bytesPerPixel = 6;
                    }
                    else {
                        raFile.close();
                        MipavUtil.displayError("Cannot handle " + dataString + " data type for numChannels = 3");
                        throw new IOException();
                    }
                }
                else if (numChannels == 4) {
                    if (dataString.equals("MET_FLOAT")) {
                        dataType = ModelStorageBase.ARGB_FLOAT;
                        Preferences.debug("Data type = ModelStorageBase.ARGB_FLOAT\n", Preferences.DEBUG_FILEIO);
                        bytesPerPixel = 16;
                    }
                }
                else {
                    raFile.close();
                    MipavUtil.displayError("Cannot handle " + dataString + " data type for numChannels = " + numChannels);
                    throw new IOException();    
                }
                fileInfo.setDataType(dataType);    
            } // if (dataString != null)
            
            dataLength = bytesPerPixel;
            for (i = 0; i < nDims; i++) {
                dataLength *= extents[i];
            }
            Preferences.debug("dataLength of uncompressed data = " + dataLength + "\n", Preferences.DEBUG_FILEIO);
           
            if ((fileDataName != null) && (fileDataName.length() > 0)) {
                if (fileDataName.equalsIgnoreCase("LOCAL")) {
                    raFile.seek(currentLocation);
                    rawDataName = new String(fileName);
                }
                else {
                    raFile.close();
                    file = new File(fileDir + fileDataName);
                    raFile = new RandomAccessFile(file, "r");
                    rawDataName = new String(fileDataName);
                    currentLocation = 0L;
                }
            }
            else if (defaultFileDataExists) {
                raFile.close();
                file = new File(fileDir + defaultFileDataName);
                raFile = new RandomAccessFile(file, "r");
                rawDataName = new String(defaultFileDataName);
                currentLocation = 0L;
            }
            else {
                raFile.seek(currentLocation); 
                rawDataName = new String(fileName);
            }
            
            if (one) {
                
                image = new ModelImage(dataType, new int[] { extents[0], extents[1] }, fileName);
            } else {
                image = new ModelImage(dataType, extents, fileName);
            }
            
            if (matrix == null) {
                matrix = new TransMatrix(Math.min(4, nDims+1));    
            }
            image.setMatrix(matrix);
            image.setImageOrientation(fileInfo.getImageOrientation());
            if (origin == null) {
                origin = new float[nDims];
                fileInfo.setOrigin(origin);
            }
            
            if (nDims == 2) {
                image.setFileInfo(fileInfo, 0); // Otherwise just set the first fileInfo
            } else if (nDims == 3) { // If there is more than one image

                for (i = 0; i < extents[2]; i++) {
                    FileInfoMetaImage newFileInfo = (FileInfoMetaImage) fileInfo.clone();
                    newFileInfo.setOrigin(fileInfo.getOriginAtSlice(i));
                    image.setFileInfo(newFileInfo, i); // Set the array of fileInfos in ModelImage
                }
            } else if (nDims == 4) { // If there is more than one image
                for (i = 0; i < (extents[2] * extents[3]); i++) {
                    FileInfoMetaImage newFileInfo = (FileInfoMetaImage) fileInfo.clone();
                    newFileInfo.setOrigin(fileInfo.getOriginAtSlice(i));
                    image.setFileInfo(newFileInfo, i); // Set the array of fileInfos in ModelImage
                }
            }
        
            updateorigins(image.getFileInfo());
            
            if (compressedData) {
                buffer = new byte[compressedDataSize];
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
                /*bufSize = 65536;
                while ((bufSize < 2*(long)compressedDataSize) && (bufSize < (int)Math.pow(2,30))){
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
                Preferences.debug("bufSize of byte buf[] used for decompression = " + bufSize + "\n", Preferences.DEBUG_FILEIO);*/
                buf = new byte[dataLength];
                try {
                    while (true) {
                        int count = 0;
                        try {
                            count = zlibDecompresser.inflate(buf);
                            Preferences.debug("count from zlibDecompresser.inflate(buf) =  " + count + "\n", Preferences.DEBUG_FILEIO);
                        }
                        catch (Exception e) {
                            MipavUtil.displayError("zlibDecompresser.inflate(buf) gives exception " + e);
                            throw new IOException();
                        }
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
                try {
                    decomp = bos.toByteArray();
                }
                catch (Exception e) {
                    MipavUtil.displayError("decomp = bos.toByteArray() gives exception " + e);
                    throw new IOException();
                }
                resultLength = decomp.length;
                Preferences.debug("resultLength from decom.length() = " + resultLength + "\n", Preferences.DEBUG_FILEIO);
                rawDataName = baseName + "_uncompressed";
                raFile.close();
                file = new File(fileDir + rawDataName);
                raFile = new RandomAccessFile(file, "rw");
                raFile.setLength(0);
                raFile.write(decomp, 0, resultLength);
                raFile.seek(0L);
            } // if (compressedData)
            
            FileRaw rawFile;
            rawFile = new FileRaw(rawDataName, fileDir, fileInfo, FileBase.READ);
            if (numChannels == 4) {
                rawFile.setNumChannels(4);
                rawFile.setRGBAOrder(false);
            }
            linkProgress(rawFile);
            
            rawFile.readImage(image, raFile.getFilePointer());
            
            if (compressedData) {
                file.delete();
            }
                      
            return image;
            
        }
        catch (final Exception e) {

            if (image != null) {
                image.disposeLocal();
                image = null;
            }

            System.gc();
            throw new IOException();
        }
    }
    
    /**
     * Updates the start locations. Each image has a fileinfo where the start locations are stored. Note that the start
     * location for the Z (3rd) dimension change with the change is the slice. The origin is in the upper left corner
     * and we are using the right hand rule. + x -> left to right; + y -> top to bottom and + z -> into screen.
     *
     * @param  fileInfo  DOCUMENT ME!
     */
    private void updateorigins(FileInfoBase[] fileInfo) {

        float[] origin = (float[]) (fileInfo[0].getOrigin().clone());
        float[] resolutions = fileInfo[0].getResolutions();

        if (image.getNDims() == 3) {

            for (int i = 0; i < image.getExtents()[2]; i++) {
                fileInfo[i].setOrigin(origin[0] + (matrix.get(0, 2) * i), 0);
                fileInfo[i].setOrigin(origin[1] + (matrix.get(1, 2) * i), 1);
                fileInfo[i].setOrigin(origin[2] + (matrix.get(2, 2) * i), 2);
            }
        } else if (image.getNDims() == 4) {

            for (int i = 0; i < image.getExtents()[3]; i++) {

                for (int j = 0; j < image.getExtents()[2]; j++) {
                    fileInfo[i * image.getExtents()[2] + j].setOrigin(origin[0] + (matrix.get(0, 2) * j), 0);
                    fileInfo[i * image.getExtents()[2] + j].setOrigin(origin[1] + (matrix.get(1, 2) * j), 1);
                    fileInfo[i * image.getExtents()[2] + j].setOrigin(origin[2] + (matrix.get(2, 2) * j), 2);
                    fileInfo[(i * image.getExtents()[2]) + j].setOrigin(origin[3] + i * resolutions[3], 3);
                    
                }
            }
        }
        /*else if (image.getNDims() == 5) {
         * fileInfo = image.getFileInfo(); for (int i = 0;    i <    image.getExtents()[2] * image.getExtents()[3] *
         * image.getExtents()[4];    i++) { fileInfo[i].setorigins(startLocs); startLocs[4] += resolutions[4]; }  }*/
    }
    
    /**
     * readLine() - reads a line of the file header separate into category, subcategory, and values.
     *
     * @exception  IOException  if there is an error reading the file
     */
    private void readLine() throws IOException {
        String tempString = null;
        int index;
        numValues = 0;
        
        try {
            tempString = raFile.readLine();
        } catch (IOException error) {
            throw (error);
        }
        tempString = tempString.trim();

        index = tempString.indexOf(" = ");

        if (index != -1) {
            category = tempString.substring(0, index);

            
        } else {
            Preferences.debug("Separator between category and values not found\n", Preferences.DEBUG_FILEIO);
            sepFound = false;
            return;
        }

        tempString = tempString.substring(index + 3);
        while (tempString.indexOf(fieldSeparator) == 0) {
            tempString = tempString.substring(1);
        }

        index = tempString.indexOf(fieldSeparator);
        
        if (index == -1) {
            values[numValues++] = tempString;
            return;
        }

        do {
            values[numValues++] = tempString.substring(0, index);
            tempString = tempString.substring(index + 1);
            while (tempString.indexOf(fieldSeparator) == 0) {
                tempString = tempString.substring(1);
            }
            index = tempString.indexOf(fieldSeparator);
        } while (index != -1);
        
        values[numValues++] = tempString;
        return;
    }
    
    /**
     * Writes an MetaImage format type image.
     *
     * @param      image  Image model of data to write.
     *
     * @exception  IOException  if there is an error writing the file
     *
     * @see        FileInfoMeta
     * @see        FileRaw
     */
    public void writeImage(ModelImage image, FileWriteOptions options) throws IOException {
        String fhName;
        int index;
        int nImagesSaved;
        int nTimePeriodsSaved;
        String suffix;
        int headerIndex = 0;
        long finalHeaderPosition[] = null;

        suffix = FileUtility.getExtension(fileName);

        if (suffix.equalsIgnoreCase(".mha")) {
            oneFile = true;
        } else if (suffix.equalsIgnoreCase(".raw")) {
            oneFile = false;
        } else if (suffix.equalsIgnoreCase(".mhd")) {
            oneFile = false;
        } else {
            JDialogMetaImageChoice choice = new JDialogMetaImageChoice(ViewUserInterface.getReference().getMainFrame());

            if (!choice.okayPressed()) {
                throw new IOException("FileMetaImageWrite dialog error");
            }

            oneFile = choice.getOneFile();
        }

        index = fileName.lastIndexOf(".");

        if (index != -1) {
            fhName = fileName.substring(0, index);
            if (suffix.equalsIgnoreCase(".mhd")) {
                fileName = fhName + ".raw";
            }
        } else {
            fhName = fileName.substring(0);
        }
        nImagesSaved = options.getEndSlice() - options.getBeginSlice() + 1;
        nTimePeriodsSaved = options.getEndTime() - options.getBeginTime() + 1;

        if (options.isMultiFile()) {
            FileRaw rawFile;
            rawFile = new FileRaw(image.getFileInfo(0));
            rawFile.setZeroLengthFlag(true);
            if (image.isColorImage()) {
                rawFile.setNumChannels(4);
                rawFile.setRGBAOrder(false);
            }
            linkProgress(rawFile);
            if (image.getNDims() == 3) {
                finalHeaderPosition = new long[nImagesSaved];	
            }
            else if (image.getNDims() == 4) {
            	finalHeaderPosition = new long[nTimePeriodsSaved];
            }

            if (oneFile) {

                if (image.getNDims() == 3) {
                	writeHeader3DTo2D(image, fhName, fileDir, options, oneFile, finalHeaderPosition);
                    rawFile.writeImage3DTo2D(image, options, ".mha", finalHeaderPosition);
                } else if (image.getNDims() == 4) {
                	writeHeader4DTo3D(image, fhName, fileDir, options, oneFile, finalHeaderPosition);
                    rawFile.writeImage4DTo3D(image, options, ".mha", finalHeaderPosition);
                }
            } // if (oneFile)
            else { // 2 files
                rawFile.setStartPosition(0L);

                if (image.getNDims() == 3) {
                	writeHeader3DTo2D(image, fhName, fileDir, options, oneFile, finalHeaderPosition);
                    rawFile.writeImage3DTo2D(image, options, ".raw");
                } else if (image.getNDims() == 4) {
                	writeHeader4DTo3D(image, fhName, fileDir, options, oneFile, finalHeaderPosition);
                    rawFile.writeImage4DTo3D(image, options, ".raw");
                }
            } // else 2 files

        } else {

            try {
                FileRaw rawFile;
                rawFile = new FileRaw(fileName, fileDir, image.getFileInfo(0), FileBase.READ_WRITE);
                rawFile.setZeroLengthFlag(true);
                if (image.isColorImage()) {
                    rawFile.setNumChannels(4);
                    rawFile.setRGBAOrder(false);
                }
                linkProgress(rawFile);
                finalHeaderPosition = new long[1];
                if (nImagesSaved != 0) {
                    writeHeader(image, nImagesSaved, nTimePeriodsSaved, fhName, fileDir,oneFile,headerIndex,finalHeaderPosition);
                }
               
                if (oneFile) {
                    rawFile.setStartPosition(finalHeaderPosition[0]);
                    rawFile.setZeroLengthFlag(false);
                } else {
                    rawFile.setStartPosition(0L);
                }

                rawFile.writeImage(image, options);

            } catch (IOException error) {
                throw new IOException("FileMetaImageWrite: " + error);
            } catch (OutOfMemoryError error) {
                throw (error);
            }
        }

        fireProgressStateChanged(100);

        // With extents from rawFile
    }
    
    /**
     * Writes a MetaImage header to a separate file.
     *
     * @param      image     Image model of data to write.
     * @param      fileName  File name.
     * @param      fileDir   File directory.
     *
     * @return     Flag to confirm a successful read.
     *
     * @exception  IOException  if there is an error
     *
     * @see        FileInfoMetaImage
     */
    public boolean writeHeader(ModelImage image, int nImagesSaved, int nTimeSaved, String fileName, String fileDir,
    		                   boolean oneFile, int headerIndex, long finalHeaderPosition[])
            throws IOException {
    	FileInfoBase myFileInfo;
    	boolean endianess;
    	boolean isMetaImage = true;
    	String fileHeaderName;
    	int nDims;
    	int extents[];
    	int dataType;
    	String fileDataName;
    	float res[];
    	float offset[];
    	TransMatrix matrix;
    	boolean haveOrientation;
    	String orientation[] = new String[3];
    	int axisOrientation[];
    	double centerOfRotation[];
    	int i;
    	
    	 myFileInfo = image.getFileInfo(0); // A safeguard in case the file is not MetaImage
         endianess = myFileInfo.getEndianess();

         try { // In this case, the file must be MetaImage
             fileInfo = (FileInfoMetaImage) image.getFileInfo(0);
         } catch (ClassCastException e) { // If it isn't, catch the exception

             // and make a new fileInfo
             fileInfo = new FileInfoMetaImage(fileName, fileDir, FileUtility.METAIMAGE);
             isMetaImage = false; // Write the header without all the NIFTI info
         }
         
         if (oneFile) {
             fileHeaderName = fileName + ".mha";
             fileDataName = fileName + ".mha";
         } else {
             fileHeaderName = fileName + ".mhd";
             fileDataName = fileName + ".raw";
         }

         fileHeader = new File(fileDir + fileHeaderName);
         raFile = new RandomAccessFile(fileHeader, "rw");
         raFile.setLength(0);
         
         raFile.writeBytes("ObjectType = Image\n");
         nDims = image.getNDims();
         raFile.writeBytes("NDims = " + String.valueOf(nDims) + "\n");
         raFile.writeBytes("BinaryData = True\n");
         if (endianess) {
        	 raFile.writeBytes("BinaryDataByteOrderMSB = True\n");
         }
         else {
        	 raFile.writeBytes("BinaryDataByteOrderMSB = False\n");
         }
         raFile.writeBytes("CompressedData = False\n");
         extents = image.getExtents();
         if (nDims == 2) {
        	 raFile.writeBytes("DimSize = " + String.valueOf(extents[0]) + " " + String.valueOf(extents[1]) + "\n");
         }
         else if (nDims == 3) {
        	 raFile.writeBytes("DimSize = " + String.valueOf(extents[0]) + " " + String.valueOf(extents[1]) + 
        			 " " + String.valueOf(extents[2]) + "\n");	 
         }
         else if (nDims == 4) {
        	 raFile.writeBytes("DimSize = " + String.valueOf(extents[0]) + " " + String.valueOf(extents[1]) + 
        			 " " + String.valueOf(extents[2]) + " " + String.valueOf(extents[3]) + "\n");	 	 
         }
         res = image.getResolutions(0);
         if (nDims == 2) {
        	 raFile.writeBytes("ElementSpacing = " + String.valueOf(res[0]) + " " + String.valueOf(res[1]) + "\n");
         }
         else if (nDims == 3) {
        	 raFile.writeBytes("ElementSpacing = " + String.valueOf(res[0]) + " " + String.valueOf(res[1]) + 
        			 " " + String.valueOf(res[2]) + "\n");	 
         }
         else if (nDims == 4) {
        	 raFile.writeBytes("ElementSpacing = " + String.valueOf(res[0]) + " " + String.valueOf(res[1]) + 
        			 " " + String.valueOf(res[2]) + " " + String.valueOf(res[3]) + "\n");	 	 
         }
         offset = image.getOrigin();
         if (nDims == 2) {
        	 raFile.writeBytes("Offset = " + String.valueOf(offset[0]) + " " + String.valueOf(offset[1]) + "\n");
         }
         else if (nDims == 3) {
        	 raFile.writeBytes("Offset = " + String.valueOf(offset[0]) + " " + String.valueOf(offset[1]) + 
        			 " " + String.valueOf(offset[2]) + "\n");	 
         }
         else if (nDims == 4) {
        	 raFile.writeBytes("Offset = " + String.valueOf(offset[0]) + " " + String.valueOf(offset[1]) + 
        			 " " + String.valueOf(offset[2]) + " " + String.valueOf(offset[3]) + "\n");	 	 
         }
         if (((nDims == 3) || (nDims == 4)) && (image.getMatrix() != null)) {
             matrix = image.getMatrix();
             raFile.writeBytes("TransformMatrix = " + String.valueOf(matrix.get(0,0)) + " " + String.valueOf(matrix.get(0,1))
             + " " + String.valueOf(matrix.get(0,2)) + " " + String.valueOf(matrix.get(1,0)) + " " +
            		 String.valueOf(matrix.get(1,1)) + " " + String.valueOf(matrix.get(1,2)) + " " +
                     String.valueOf(matrix.get(2,0)) + " " + String.valueOf(matrix.get(2,1)) + " " +
            		 String.valueOf(matrix.get(2,2)) + "\n");
         }
         else if ((nDims == 2) && (image.getMatrix() != null)) {
        	 matrix = image.getMatrix();
        	 raFile.writeBytes("TransformMatrix = " + String.valueOf(matrix.get(0,0)) + " " + String.valueOf(matrix.get(0,1))
             + " " + String.valueOf(matrix.get(1,0)) + " " + String.valueOf(matrix.get(1,1)) + "\n"); 
         }
         if (nDims >= 3) {
             haveOrientation = true;
             axisOrientation = image.getAxisOrientation();
             for (i = 0; i < 3; i++) {
                 switch(axisOrientation[i]) {
                 case FileInfoBase.ORI_R2L_TYPE:
                	 orientation[i] = "R";
                	 break;
                 case FileInfoBase.ORI_L2R_TYPE:
                	 orientation[i] = "L";
                	 break;
                 case FileInfoBase.ORI_A2P_TYPE:
                	 orientation[i] = "A";
                     break;
                 case FileInfoBase.ORI_P2A_TYPE:
                	 orientation[i] = "P";
                	 break;
                 case FileInfoBase.ORI_I2S_TYPE:
                	 orientation[i] = "I";
                	 break;
                 case FileInfoBase.ORI_S2I_TYPE:
                	 orientation[i] = "S";
                	 break;
                 case FileInfoBase.ORI_UNKNOWN_TYPE:
                	 haveOrientation = false;
                 }
             }
             if (haveOrientation) {
                 raFile.writeBytes("AnatomicalOrientation = " + orientation[0] + orientation[1] + orientation[2] + "\n");	 
             }
         } // if (nDims >= 3)
         if (isMetaImage && fileInfo.getCenterOfRotation() != null) {
             centerOfRotation = fileInfo.getCenterOfRotation();	 
             if (nDims == 2) {
            	 raFile.writeBytes("CenterOfRotation = " + String.valueOf(centerOfRotation[0]) + " " + 
             String.valueOf(centerOfRotation[1]) + "\n");
             }
             else if (nDims >= 3) {
            	 raFile.writeBytes("CenterOfRotation = " + String.valueOf(centerOfRotation[0]) + " " + String.valueOf(centerOfRotation[1]) + 
            			 " " + String.valueOf(centerOfRotation[2]) + "\n");	 
             }
         }
         dataType = image.getType();
         switch(dataType) {
         case ModelStorageBase.BYTE:
        	 raFile.writeBytes("ElementType =  MET_CHAR\n");
        	 break;
         case ModelStorageBase.UBYTE:
         case ModelStorageBase.ARGB:
        	 raFile.writeBytes("ElementType = MET_UCHAR\n");
        	 break;
         case ModelStorageBase.SHORT:
        	 raFile.writeBytes("ElementType = MET_SHORT\n");
        	 break;
         case ModelStorageBase.USHORT:
         case ModelStorageBase.ARGB_USHORT:
        	 raFile.writeBytes("ElementType = MET_USHORT\n");
        	 break;
         case ModelStorageBase.INTEGER:
        	 raFile.writeBytes("ElementType = MET_INT\n");
        	 break;
         case ModelStorageBase.UINTEGER:
        	 raFile.writeBytes("ElementType = MET_UINT\n");
        	 break;
         case ModelStorageBase.LONG:
        	 raFile.writeBytes("ElementType = MET_LONG\n");
        	 break;
         case ModelStorageBase.FLOAT:
         case ModelStorageBase.ARGB_FLOAT:
         case ModelStorageBase.COMPLEX:
        	 raFile.writeBytes("ElementType = MET_FLOAT\n");
        	 break;
         case ModelStorageBase.DOUBLE:
         case ModelStorageBase.DCOMPLEX:
        	 raFile.writeBytes("ElementType = MET_DOUBLE\n");
        	 break;
         }
         if (image.isColorImage()) {
        	 raFile.writeBytes("ElementNumberOfChannels = 4\n");
         }
         else if (image.isComplexImage()) {
        	 raFile.writeBytes("ElementNumberOfChannels = 2\n");
         }
         else {
        	 raFile.writeBytes("ElementNumberOfChannels = 1\n");
         }
         if (oneFile) {
        	 raFile.writeBytes("ElementDataFile = LOCAL\n");
         }
         else {
             raFile.writeBytes("ElementDataFile = " + fileDataName + "\n");
         }
         
         finalHeaderPosition[headerIndex] = raFile.getFilePointer();
    	 return true; // Successful write
    }
    
    /**
     * This method is used when saving a 3D image in an array of 2D files. The file name has numbers appended to
     * correctly order the images.
     *
     * @param   image     the image dataset to be saved
     * @param   fileName  the file name
     * @param   fileDir   the file directory
     * @param   options   file options indicate how to save the image
     *
     * @throws  IOException  DOCUMENT ME!
     */
    private void writeHeader3DTo2D(ModelImage image, String fileName, String fileDir, FileWriteOptions options,
    		                       boolean oneFile, long finalHeaderPosition[])
            throws IOException {
        int k, seq;
        int beginSlice = options.getBeginSlice();
        int endSlice = options.getEndSlice();
        String origName = new String(fileName);

        for (k = beginSlice, seq = options.getStartNumber(); k <= endSlice; k++, seq++) {
            fileName = origName;

            if (options.getDigitNumber() == 1) {
                fileName += Integer.toString(seq);
            } else if (options.getDigitNumber() == 2) {

                if (seq < 10) {
                    fileName += "0" + Integer.toString(seq);
                } else {
                    fileName += Integer.toString(seq);
                }
            } else if (options.getDigitNumber() == 3) {

                if (seq < 10) {
                    fileName += "00" + Integer.toString(seq);
                } else if (seq < 100) {
                    fileName += "0" + Integer.toString(seq);
                } else {
                    fileName += Integer.toString(seq);
                }
            } else if (options.getDigitNumber() == 4) {

                if (seq < 10) {
                    fileName += "000" + Integer.toString(seq);
                } else if (seq < 100) {
                    fileName += "00" + Integer.toString(seq);
                } else if (seq < 1000) {
                    fileName += "0" + Integer.toString(seq);
                } else {
                    fileName += Integer.toString(seq);
                }
            }

            writeHeader(image, 1, 1, fileName, fileDir, oneFile, k-beginSlice, finalHeaderPosition);

        } // end for loop

    }
    
    /**
     * This method is used when saving a 4D image in an array of 3D files. The file name has numbers appended to
     * correctly order the images.
     *
     * @param   image     the image dataset to be saved
     * @param   fileName  the file name
     * @param   fileDir   the file directory
     * @param   options   file options indicate how to save the image
     *
     * @throws  IOException  DOCUMENT ME!
     */
    private void writeHeader4DTo3D(ModelImage image, String fileName, String fileDir, FileWriteOptions options,
    		                       boolean oneFile, long finalHeaderPosition[])
            throws IOException {
        int k, seq;
        int beginTime = options.getBeginTime();
        int endTime = options.getEndTime();
        String origName = new String(fileName);

        for (k = beginTime, seq = options.getStartNumber(); k <= endTime; k++, seq++) {
            fileName = origName;

            if (options.getDigitNumber() == 1) {
                fileName += Integer.toString(seq);
            } else if (options.getDigitNumber() == 2) {

                if (seq < 10) {
                    fileName += "0" + Integer.toString(seq);
                } else {
                    fileName += Integer.toString(seq);
                }
            } else if (options.getDigitNumber() == 3) {

                if (seq < 10) {
                    fileName += "00" + Integer.toString(seq);
                } else if (seq < 100) {
                    fileName += "0" + Integer.toString(seq);
                } else {
                    fileName += Integer.toString(seq);
                }
            } else if (options.getDigitNumber() == 4) {

                if (seq < 10) {
                    fileName += "000" + Integer.toString(seq);
                } else if (seq < 100) {
                    fileName += "00" + Integer.toString(seq);
                } else if (seq < 1000) {
                    fileName += "0" + Integer.toString(seq);
                } else {
                    fileName += Integer.toString(seq);
                }
            }
            // write header with image, # of images per, and 1 time slice

            writeHeader(image, image.getExtents()[2], 1, fileName, fileDir,oneFile,k-beginTime,finalHeaderPosition);

        } // end for loop

    }
}