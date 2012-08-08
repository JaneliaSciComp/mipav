package gov.nih.mipav.model.file;


import gov.nih.mipav.model.algorithms.utilities.*;
import gov.nih.mipav.model.file.FileInfoBase.Unit;
import gov.nih.mipav.model.structures.*;

import Jama.*;

import gov.nih.mipav.view.*;
import gov.nih.mipav.view.dialogs.*;

import java.io.*;
import java.util.zip.GZIPInputStream;
import java.util.zip.ZipInputStream;
import java.text.*;


public class FileMetaImage extends FileBase {
    /** DOCUMENT ME! */
    private String fileDir;

    /** DOCUMENT ME! */
    private File fileHeader;

    /** DOCUMENT ME! */
    private FileInfoMetaImage fileInfo = null;

    /** DOCUMENT ME! */
    private String fileName;
    
    private long headerSize;
    
    private long currentLocation;
    
    /** DOCUMENT ME! */
    private boolean endianess;

    /** DOCUMENT ME! */
    private File file;
    
    /** If true, header and data both stored in .mha file.
     *  If false, header stored in filename.mhd and data
     *  stored in filename.raw. */
    private boolean oneFile;

    /** DOCUMENT ME! */
    private ModelImage image;
    
    /** DOCUMENT ME! */
    private float[] origin;
    
    private float[] resolutions;
    
    private int numValues;
    
    /** DOCUMENT ME! */
    private String fieldSeparator = new String(" ");
    
    private String category;
    
    private String[] values = new String[10];
    
    private int nDims = -1;
    
    private String defaultFileDataName;
    private File defaultFileData;
    private boolean defaultFileDataExists = false;
    private boolean asciiFormat = false;
    private boolean compressedData = false;
    private int extents[] = null;
    
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
        if (fName.substring(index).equalsIgnoreCase(".MHD")) {
            defaultFileDataName = fName.substring(0, index) + ".raw";
            defaultFileData = new File(fDir + defaultFileDataName);
            if (defaultFileData.exists()) {
                defaultFileDataExists = true;
                Preferences.debug("Default data file = " + defaultFileDataName + " exists\n", Preferences.DEBUG_FILEIO);
            }
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
       
        try {

            file = new File(fileDir + fileName);
            fileInfo = new FileInfoMetaImage(fileName, fileDir, FileUtility.METAIMAGE);
            raFile = new RandomAccessFile(file, "r");
            headerSize = raFile.length();
            
            currentLocation = raFile.getFilePointer();
            while (currentLocation < headerSize-1) {
                readLine();
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
                else if (category.equalsIgnoreCase("Offset")) {
                    if ((nDims >= 2)  && (numValues != nDims)) {
                        MipavUtil.displayError(nDims + " != " + numValues + " for Offset");
                        throw new IOException();
                    }
                    else {
                        nDims = numValues;
                    }
                    origin = new float[nDims];
                    for (i = 0; i < nDims; i++) {
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
                } // else if (category.equalsIgnoreCase("Offset"))
                else if (category.equalsIgnoreCase("ElementSpacing")) {
                    if ((nDims >= 2)  && (numValues != nDims)) {
                        MipavUtil.displayError(nDims + " != " + numValues + " for ElementSpacing");
                        throw new IOException();
                    }
                    else {
                        nDims = numValues;
                    }
                    resolutions = new float[nDims];
                    for (i = 0; i < nDims; i++) {
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
            } // while (readAgain)
            
                      
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
     * readLine() - reads a line of the file header separate into category, subcategory, and values.
     *
     * @exception  IOException  if there is an error reading the file
     */
    private void readLine() throws IOException {
        String tempString;
        int index;
        numValues = 0;

        try {
            tempString = raFile.readLine();
        } catch (IOException error) {
            throw (error);
        }

        index = tempString.indexOf(" = ");

        if (index != -1) {
            category = tempString.substring(0, index);

            
        } else {
            Preferences.debug("Separator between category and values not found\n", Preferences.DEBUG_FILEIO);
            Preferences.debug("Header line = " + tempString, Preferences.DEBUG_FILEIO);
            throw new IOException("Separator between category and values not found\n");
        }

        tempString = tempString.substring(index + 3);

        index = tempString.indexOf(fieldSeparator);
        
        if (index == -1) {
            values[numValues++] = tempString;
            return;
        }

        do {
            values[numValues++] = tempString.substring(0, index);
            tempString = tempString.substring(index + 1);
            index = tempString.indexOf(fieldSeparator);
        } while (index != -1);
        
        values[numValues++] = tempString;
        return;
    }
}