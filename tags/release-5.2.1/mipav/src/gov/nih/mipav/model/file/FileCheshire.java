package gov.nih.mipav.model.file;


import gov.nih.mipav.model.file.FileInfoBase.Unit;
import gov.nih.mipav.model.structures.*;

import gov.nih.mipav.view.*;

import java.io.*;


/**
 * The class reads Cheshire file images.
 *
 * @version  0.1 Oct 14, 1997
 * @author   Matthew J. McAuliffe, Ph.D.
 * @see      FileIO
 * @see      FileInfoAnalyze
 * @see      FileRaw
 */

public class FileCheshire extends FileBase {

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** DOCUMENT ME! */
    private String fileDir;

    /** DOCUMENT ME! */
    private FileInfoCheshire fileInfo = null;
    
    private FileInfoCheshire fileInfoCopy = null;

    /** DOCUMENT ME! */
    private String fileName;

    /** DOCUMENT ME! */
    private ModelImage image;

    /** DOCUMENT ME! */
    private int nBytesPerVoxel;

    /** DOCUMENT ME! */
    private float[] scaleFactor;

    /** DOCUMENT ME! */
    private int startFirstImage;

    /** DOCUMENT ME! */
    private int startSecondImage;

    /** DOCUMENT ME! */
    private int subLength;

    /** DOCUMENT ME! */
    private int xDim, yDim, zDim, tDim;

    /** DOCUMENT ME! */
    private float xRes, yRes, zRes, tRes;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Creates a new FileCheshire object.
     *
     * @param  fName  file name
     * @param  fDir   file directory
     * @param  show   boolean for showing the progress bar
     */
    public FileCheshire(String fName, String fDir, boolean show) {

        fileName = fName;
        fileDir = fDir;
        pBarVisible = show;
        fireProgressStateChanged(0);
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
        scaleFactor = null;
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
    public static boolean isCheshire(String fName, String fDir) {

        try {
            RandomAccessFile raFile = new RandomAccessFile(fDir + fName, "r");

            byte[] b = new byte[8];
            raFile.read(b);

            String tmpString = new String(b);

            raFile.close();

            if (!tmpString.equals("IMGN0001")) {
                return false;
            } else {
                return true;
            }

        } catch (FileNotFoundException e) {
            MipavUtil.displayError("FileCheshire: Error reading file.");
        } catch (IOException e) {
            MipavUtil.displayError("FileCheshire: Error reading file.");
        }

        return false;

    }

    /**
     * Returns the FileInfoCheshire read from the file.
     *
     * @return  File info structure read from file, or null if file has not been read.
     */
    public FileInfoCheshire getFileInfo() {
        return fileInfo;
    }

    /**
     * Reads the header and stores the information in fileInfo.
     *
     * @param      imageFileName  File name of image.
     * @param      fileDir        Directory.
     *
     * @return     Flag to confirm a successful read.
     *
     * @exception  IOException  if there is an error reading the header
     *
     * @see        FileInfoAnalyze
     */
    public boolean readHeader(String imageFileName, String fileDir) throws IOException {
        boolean endianess;

        int[] mipavExtents;
        float[] mipavResolutions;


        if (fileInfo == null) { // if the file info does not yet exist: make it
            fileInfo = new FileInfoCheshire(fileName, fileDir, FileUtility.CHESHIRE);
        }

        fileInfo.setEndianess(BIG_ENDIAN);
        endianess = fileInfo.getEndianess();

        raFile = new RandomAccessFile(fileDir + imageFileName, "r");

        String tmpString = getString(8);

        if (!tmpString.equals("IMGN0001")) {
            return false;
        }

        startFirstImage = getInt(endianess);
        startSecondImage = startFirstImage + getInt(endianess);

        xDim = getUnsignedShort(endianess);
        yDim = getUnsignedShort(endianess);
        zDim = getUnsignedShort(endianess);
        tDim = getUnsignedShort(endianess);
        scaleFactor = new float[zDim * tDim];

        xRes = getFloat(endianess);
        yRes = getFloat(endianess);
        zRes = getFloat(endianess);
        tRes = getFloat(endianess);

        if ((zDim == 1) || (zDim == 0)) {
            mipavExtents = new int[2];
            mipavExtents[0] = xDim;
            mipavExtents[1] = yDim;

            mipavResolutions = new float[2];
            mipavResolutions[0] = xRes;
            mipavResolutions[1] = yRes;
        } else if ((tDim == 1) || (tDim == 0)) {
            mipavExtents = new int[3];
            mipavExtents[0] = xDim;
            mipavExtents[1] = yDim;
            mipavExtents[2] = zDim;

            mipavResolutions = new float[3];
            mipavResolutions[0] = xRes;
            mipavResolutions[1] = yRes;
            mipavResolutions[2] = zRes;
        } else {
            mipavExtents = new int[4];
            mipavExtents[0] = xDim;
            mipavExtents[1] = yDim;
            mipavExtents[2] = zDim;
            mipavExtents[3] = tDim;

            mipavResolutions = new float[4];
            mipavResolutions[0] = xRes;
            mipavResolutions[1] = yRes;
            mipavResolutions[2] = zRes;
            mipavResolutions[3] = tRes;
        }

        fileInfo.setExtents(mipavExtents);
        fileInfo.setResolutions(mipavResolutions);

        raFile.seek(startFirstImage);

        subLength = getInt(endianess); // I think this is really bits per pixel
        subLength = 16;
        scaleFactor[0] = getFloat(endianess);

        nBytesPerVoxel = (startSecondImage - startFirstImage - subLength) / (xDim * yDim);


        // Float images are stored as signed shorts with a scale factor (float) found
        // just in front of the image slice. One per slice.
        long offset;
        int imageLength = xDim * yDim;

        for (int t = 0; t < tDim; t++) {

            for (int z = 1; z < zDim; z++) {
                offset = startFirstImage + 4 + (z * ((imageLength * nBytesPerVoxel) + subLength)) +
                         (t * (zDim * ((imageLength * nBytesPerVoxel) + subLength)));
                raFile.seek(offset);
                scaleFactor[(t * zDim) + z] = getFloat(endianess);
            }
        }

        if (nBytesPerVoxel == 1) {
            fileInfo.setDataType(ModelImage.UBYTE);
        } else if (nBytesPerVoxel == 2) {
            fileInfo.setDataType(ModelImage.SHORT);
        }

        if ((nBytesPerVoxel == 2) && (scaleFactor[0] != 1.0f)) {
            fileInfo.setDataType(ModelImage.FLOAT);
        }

        raFile.close();

        return true;
    }


    /**
     * Reads an Cheshire image file by reading the header then making a FileRaw to read the image for all filenames in
     * the file list. Only the one file directory (currently) supported.
     *
     * @exception  IOException  if there is an error reading the file
     *
     * @see        FileRaw
     */
    public ModelImage readImage() throws IOException, OutOfMemoryError {
        int i;

        fileInfo = new FileInfoCheshire(fileName, fileDir, FileUtility.CHESHIRE);

        if (!readHeader(fileInfo.getFileName(), fileInfo.getFileDirectory())) {
            raFile.close();
            throw (new IOException(" Cheshire header file error"));
        }

        try {
            image = new ModelImage(fileInfo.getDataType(), fileInfo.getExtents(), fileInfo.getFileName());
        } catch (OutOfMemoryError error) {
            throw (error);
        }

        fileInfo.setImageOrientation(FileInfoBase.UNKNOWN_ORIENT);


        if (image.getNDims() == 2) {
            fileInfo.setUnitsOfMeasure(Unit.MILLIMETERS.getLegacyNum(), 0);
            fileInfo.setUnitsOfMeasure(Unit.MILLIMETERS.getLegacyNum(), 1);
            image.setFileInfo(fileInfo, 0); // Otherwise just set the first fileInfo
        } else if (image.getNDims() == 3) { // If there is more than one image
            fileInfo.setUnitsOfMeasure(Unit.MILLIMETERS.getLegacyNum(), 0);
            fileInfo.setUnitsOfMeasure(Unit.MILLIMETERS.getLegacyNum(), 1);
            fileInfo.setUnitsOfMeasure(Unit.MILLIMETERS.getLegacyNum(), 2);

            image.setFileInfo(fileInfo, 0);
            for (i = 1; i < image.getExtents()[2]; i++) {
                fileInfoCopy = (FileInfoCheshire)fileInfo.clone();
                image.setFileInfo(fileInfoCopy, i); // Set the array of fileInfos in ModelImage
            }
        } else if (image.getNDims() == 4) { // If there is more than one image
            fileInfo.setUnitsOfMeasure(Unit.MILLIMETERS.getLegacyNum(), 0);
            fileInfo.setUnitsOfMeasure(Unit.MILLIMETERS.getLegacyNum(), 1);
            fileInfo.setUnitsOfMeasure(Unit.MILLIMETERS.getLegacyNum(), 2);
            fileInfo.setUnitsOfMeasure(Unit.SECONDS.getLegacyNum(), 3);

            image.setFileInfo(fileInfo, 0);
            for (i = 1; i < (image.getExtents()[2] * image.getExtents()[3]); i++) {
                fileInfoCopy = (FileInfoCheshire)fileInfo.clone();
                image.setFileInfo(fileInfoCopy, i); // Set the array of fileInfos in ModelImage
            }
        }

        int imageLength = xDim * yDim;

        if (tDim == 0) {
            tDim = 1;
        }

        if (zDim == 0) {
            zDim = 1;
        }

        int volLength = xDim * yDim * zDim;
        int nTotalSlices = tDim * zDim;

        try { // Construct a FileRaw to actually read the image.

            FileRaw rawFile;
            int offset;
            float[] buffer = new float[imageLength];
            rawFile = new FileRaw(fileName, fileDir, fileInfo, FileBase.READ);

            for (int t = 0; t < tDim; t++) {

                for (int z = 0; z < zDim; z++) {
                    fireProgressStateChanged(Math.round((float) ((t * zDim) + z) / (nTotalSlices - 1) * 100));

                    if (z == 0) {

                        if (fileInfo.getDataType() == ModelImage.UBYTE) {
                            rawFile.readImage(buffer, startFirstImage + subLength, ModelImage.UBYTE);
                        } else if (fileInfo.getDataType() == ModelImage.SHORT) {
                            rawFile.readImage(buffer, startFirstImage + subLength, ModelImage.SHORT);
                        } else if (fileInfo.getDataType() == ModelImage.FLOAT) {
                            rawFile.readImage(buffer, startFirstImage + subLength, ModelImage.SHORT);
                        }
                        // rawFile.readImage(buffer, startFirstImage+subLength, fileInfo.getDataType());
                    } else {
                        offset = startFirstImage + subLength + (z * ((imageLength * nBytesPerVoxel) + subLength)) +
                                 (t * (zDim * ((imageLength * nBytesPerVoxel) + subLength)));

                        if (fileInfo.getDataType() == ModelImage.UBYTE) {
                            rawFile.readImage(buffer, offset, ModelImage.UBYTE);
                        } else if (fileInfo.getDataType() == ModelImage.SHORT) {
                            rawFile.readImage(buffer, offset, ModelImage.SHORT);
                        } else if (fileInfo.getDataType() == ModelImage.FLOAT) {
                            rawFile.readImage(buffer, offset, ModelImage.SHORT);
                        }
                    }

                    if (fileInfo.getDataType() == ModelStorageBase.FLOAT) {

                        for (int m = 0; m < buffer.length; m++) {
                            buffer[m] = buffer[m] * scaleFactor[(t * zDim) + z];
                        }
                    }

                    image.importData((t * volLength) + (z * imageLength), buffer, false);
                }
            }

            rawFile.raFile.close();
        } catch (IOException error) {

            throw new IOException("FileCheshire: " + error);
        } catch (OutOfMemoryError e) {

            throw (e);
        }


        // image.calcMinMax();
        return image;
    }

}
