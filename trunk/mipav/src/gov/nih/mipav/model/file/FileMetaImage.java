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
    
    private int headerSize;
    
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
    
    /**
     * Constructs new file object.
     *
     * @param  fName  File name.
     * @param  fDir   File directory.
     */
    public FileMetaImage(String fName, String fDir) {
        String fileDataName;
        File fileData;
        int index = fName.length();

        for (int i = fName.length() - 1; i >= 0; i--) {

            if (fName.charAt(i) == '.') {
                index = i;

                break;
            }
        }
        if (fName.substring(index).equalsIgnoreCase(".MHD")) {
            fileDataName = fName.substring(0, index) + ".raw";
            fileData = new File(fDir + fileDataName);
            if (fileData.exists()) {
                fName = fileDataName;
            }
            else {
                fileDataName = fName.substring(0, index) + ".MHA";
                fileData = new File(fDir + fileDataName); 
                if (fileData.exists()) {
                    fName = fileDataName;
                }
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
        boolean readAgain;
        try {

            file = new File(fileDir + fileName);

            fileInfo = new FileInfoMetaImage(fileName, fileDir, FileUtility.METAIMAGE);

            raFile = new RandomAccessFile(file, "r");

            readAgain = true;
            
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
}