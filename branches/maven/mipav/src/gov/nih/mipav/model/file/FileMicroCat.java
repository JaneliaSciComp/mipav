package gov.nih.mipav.model.file;


import gov.nih.mipav.model.structures.*;

import gov.nih.mipav.view.*;

import java.io.*;


/**
 * @author  Matthew J. McAuliffe
 * @see     FileIO
 */

public class FileMicroCat extends FileBase {

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** DOCUMENT ME! */
    private File file;

    /** DOCUMENT ME! */
    private String fileDir;

    /** DOCUMENT ME! */
    private String fileName;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * MicroCat reader/writer constructor.
     *
     * @param      fileName  file name
     * @param      fileDir   file directory
     *
     * @exception  IOException  if there is an error making the file
     */
    public FileMicroCat(String fileName, String fileDir) throws IOException {
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
        file = null;
    
        try {
            super.finalize();
        } catch (Throwable er) { }
    }
    
    /**
     * Trims the numbers(numerics and some special characters [-, _, .) off the end of a string (file name).
     *
     * @param   fName  File name where the last characters are alpha-numerics indicating the image number.
     *
     * @return  File name without numbers on the end.
     */
    public static String trimmer(String fName) {
        int i;
        char ch;
        int length = fName.lastIndexOf("."); // Start before suffix.

        for (i = length - 1; i > -1; i--) {
            ch = fName.charAt(i);

            if ((ch == '0') || (ch == '1') || (ch == '2') || (ch == '3') || (ch == '4') || (ch == '5') || (ch == '6') ||
                    (ch == '7') || (ch == '8') || (ch == '9')) {
                // Do nothing.
            } else {
                break;
            }
        }

        if (i <= -1) {
            return new String();
        }

        return (new String(fName.substring(0, i + 1)));

    }

    /**
     * Looks in the image directory and returns all images with the same suffix as <code>fileName</code>, sorted in
     * lexicographical order. Will set the number of images (<code>nImages</code>) for the calling program.
     *
     * @param   fileDir   Directory to looks for images in.
     * @param   fileName  File name of the image.
     * @param   quiet     DOCUMENT ME!
     *
     * @return  An array of the image names to be read in or saved as.
     */
    public String[] getFileList(String fileDir, String fileName, boolean quiet) {
        int i;
        int j = 0;
        int result = 0;
        String[] fileList;
        String[] fileList2;
        String[] fileListBuffer;
        String fileTemp;
        File imageDir;
        int nImages;

        imageDir = new File(fileDir);

        // Read directory and find no. of images
        fileListBuffer = imageDir.list();
        fileList = new String[fileListBuffer.length];

        String subName = fileName + "_";
        String suffix = ".ct";

        // check to see that they end in suffix.  If so, store, count.
        for (i = 0; i < fileListBuffer.length; i++) {

            if (fileListBuffer[i].trim().toLowerCase().endsWith(suffix)) {

                if (trimmer(fileListBuffer[i].trim()).equals(subName)) {
                    fileList[j] = fileListBuffer[i];
                    j++;
                }
            }
        }

        // Number of images is index of last image read into fileList
        nImages = j;

        if (nImages == 0) {

            if (!quiet) {
                MipavUtil.displayError("FileMicroCat: No images that start with " + subName + " and end with .ct");
            }

            return null;
        }

        fileList2 = new String[nImages];

        for (i = 0; i < nImages; i++) {
            fileList2[i] = fileList[i];
        }

        // sort to ensure that files are in correct (lexicographical) order
        for (i = 0; i < nImages; i++) { // (bubble sort? ... )

            for (j = i + 1; j < nImages; j++) {
                result = fileList2[i].compareTo(fileList2[j]);

                if (result > 0) {
                    fileTemp = fileList2[i];
                    fileList2[i] = fileList2[j];
                    fileList2[j] = fileTemp;
                } // end of if (result > 0)
            } // end of for (j = i+1; j < nImages; j++)
        } // end of for (i = 0; i < nImages; i++)

        return fileList2;
    }


    /**
     * DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     *
     * @throws  IOException  DOCUMENT ME!
     */
    public FileInfoMicroCat readHeader() throws IOException {

        String lineString = null;
        String keyString;
        String valString;
        FileInfoMicroCat fileInfo;

        try {
            file = new File(fileDir + fileName);
            raFile = new RandomAccessFile(file, "r");
            fileInfo = new FileInfoMicroCat(fileName, fileDir, FileUtility.MICRO_CAT);
        } catch (OutOfMemoryError error) {
            raFile.close();
            System.gc();

            return null;
        }

        fileInfo.setEndianess(FileBase.LITTLE_ENDIAN);
        lineString = readLine();

        while (lineString != null) {

            if (lineString == null) {
                raFile.close();
            }

            // tmpStr = getKeyString(lineString);
            keyString = getKeyString(lineString);
            valString = getValString(lineString);
            Preferences.debug("\nKey value = " + keyString + "\n", Preferences.DEBUG_FILEIO);
            Preferences.debug("Value     = " + valString + "\n", Preferences.DEBUG_FILEIO);

            if (keyString != null) {

                if (keyString.equals("CBR Host Name")) {
                    fileInfo.setCBRHostName(valString);
                    Preferences.debug("Host Name = " + valString + "\n", Preferences.DEBUG_FILEIO);
                } else if (keyString.equals("RPC Port #")) {
                    fileInfo.setRPCPortNum(valString);
                    Preferences.debug("RPC Port # = " + valString + "\n", Preferences.DEBUG_FILEIO);

                } else if (keyString.equals("CAT File Name")) {
                    fileInfo.setCATFileName(valString);
                    Preferences.debug("CAT File Name = " + valString + "\n", Preferences.DEBUG_FILEIO);

                } else if (keyString.equals("Image Destination Directory")) {
                    fileInfo.setImageDestinationDirectory(valString);
                    Preferences.debug("Image Destination Directory = " + valString + "\n", Preferences.DEBUG_FILEIO);

                } else if (keyString.equals("Base Name for Reconstructed Slices")) {
                    fileInfo.setBaseNameforReconstructedSlices(valString);
                    Preferences.debug("Base Name for Reconstructed Slices = " + valString + "\n", Preferences.DEBUG_FILEIO);

                } else if (keyString.equals("Projection U Size")) {
                    fileInfo.setProjectionUSize(Integer.valueOf(valString).intValue());
                    Preferences.debug("Projection U Size = " + valString + "\n", Preferences.DEBUG_FILEIO);

                } else if (keyString.equals("Projection V Size")) {
                    fileInfo.setProjectionVSize(Integer.valueOf(valString).intValue());
                    Preferences.debug("Projection V Size = " + valString + "\n", Preferences.DEBUG_FILEIO);

                } else if (keyString.equals("Number of Projections")) {
                    fileInfo.setNumberOfProjections(Integer.valueOf(valString).intValue());
                    Preferences.debug("Number of Projections = " + valString + "\n", Preferences.DEBUG_FILEIO);

                } else if (keyString.equals("CBR Projection Directory")) {
                    fileInfo.setCBRProjectionDirectory(valString);
                    Preferences.debug("CBR Projection Directory = " + valString + "\n", Preferences.DEBUG_FILEIO);

                } else if (keyString.equals("CBR Image Directory")) {
                    fileInfo.setCBRImageDirectory(valString);
                    Preferences.debug("CBR Image Directory = " + valString + "\n", Preferences.DEBUG_FILEIO);

                } else if (keyString.equals("Volume Size X")) {
                    fileInfo.setVolumeSizeX(Integer.valueOf(valString).intValue());
                    Preferences.debug("Volume Size X = " + valString + "\n", Preferences.DEBUG_FILEIO);

                } else if (keyString.equals("Volume Size Y")) {
                    fileInfo.setVolumeSizeY(Integer.valueOf(valString).intValue());
                    Preferences.debug("Volume Size Y = " + valString + "\n", Preferences.DEBUG_FILEIO);

                } else if (keyString.equals("Volume Size Z")) {
                    fileInfo.setVolumeSizeZ(Integer.valueOf(valString).intValue());
                    Preferences.debug("Volume Size Z = " + valString + "\n", Preferences.DEBUG_FILEIO);

                } else if (keyString.equals("Voxel Size X (mm)")) {
                    fileInfo.setVoxelSizeX(Float.valueOf(valString).floatValue());
                    Preferences.debug("Volume Size X = " + valString + "\n", Preferences.DEBUG_FILEIO);

                } else if (keyString.equals("Voxel Size Y (mm)")) {
                    fileInfo.setVoxelSizeY(Float.valueOf(valString).floatValue());
                    Preferences.debug("Volume Size Y = " + valString + "\n", Preferences.DEBUG_FILEIO);

                } else if (keyString.equals("Voxel Size Z (mm)")) {
                    fileInfo.setVoxelSizeZ(Float.valueOf(valString).floatValue());
                    Preferences.debug("Volume Size Z = " + valString + "\n", Preferences.DEBUG_FILEIO);

                } else if (keyString.equals("Filter Type")) {
                    fileInfo.setFilterType(valString);
                    Preferences.debug("Filter Type = " + valString + "\n", Preferences.DEBUG_FILEIO);

                } else if (keyString.equals("U Center Offset (pixels)")) {
                    fileInfo.setUCenterOffset(Integer.valueOf(valString).intValue());
                    Preferences.debug("U Center Offset (pixels) = " + valString + "\n", Preferences.DEBUG_FILEIO);

                } else if (keyString.equals("V Center Offset (pixels)")) {
                    fileInfo.setVCenterOffset(Integer.valueOf(valString).intValue());
                    Preferences.debug("V Center Offset (pixels) = " + valString + "\n", Preferences.DEBUG_FILEIO);

                } else if (keyString.equals("Volume Origin X")) {
                    fileInfo.setVolumeOriginX(Integer.valueOf(valString).intValue());
                    Preferences.debug("Volume Origin X = " + valString + "\n", Preferences.DEBUG_FILEIO);

                } else if (keyString.equals("Volume Origin Y")) {
                    fileInfo.setVolumeOriginY(Integer.valueOf(valString).intValue());
                    Preferences.debug("Volume Origin Y = " + valString + "\n", Preferences.DEBUG_FILEIO);

                } else if (keyString.equals("Volume Origin Z")) {
                    fileInfo.setVolumeOriginZ(Integer.valueOf(valString).intValue());
                    Preferences.debug("Volume Origin Z = " + valString + "\n", Preferences.DEBUG_FILEIO);

                } else if (keyString.equals("Log Scale")) {
                    fileInfo.setLogScale(Float.valueOf(valString).floatValue());
                    Preferences.debug("Log Scale = " + valString + "\n", Preferences.DEBUG_FILEIO);

                } else if (keyString.equals("Start Angle")) {
                    fileInfo.setStartAngle(Float.valueOf(valString).floatValue());
                    Preferences.debug("Start Angle = " + valString + "\n", Preferences.DEBUG_FILEIO);

                } else if (keyString.equals("Offset")) {
                    fileInfo.setOffset(Integer.valueOf(valString).intValue());
                    Preferences.debug("Offset = " + valString + "\n", Preferences.DEBUG_FILEIO);

                } else if (keyString.equals("Cylinder Reconstruction")) {
                    fileInfo.setCylinderReconstruction(Integer.valueOf(valString).intValue());
                    Preferences.debug("Cylinder Reconstruction = " + valString + "\n", Preferences.DEBUG_FILEIO);

                } else if (keyString.equals("Enable Step Size")) {
                    fileInfo.setEnableStepSize(Integer.valueOf(valString).intValue());
                    Preferences.debug("Enable Step Size = " + valString + "\n", Preferences.DEBUG_FILEIO);

                } else if (keyString.equals("Step Size File")) {
                    fileInfo.setStepSizeFile(valString);
                    Preferences.debug("Step Size File = " + valString + "\n", Preferences.DEBUG_FILEIO);

                } else if (keyString.equals("Rotation Direction (CW=-1, CCW=1)")) {
                    fileInfo.setRotationDirection(Integer.valueOf(valString).intValue());
                    Preferences.debug("Rotation Direction (CW=-1, CCW=1) = " + valString + "\n", Preferences.DEBUG_FILEIO);

                } else if (keyString.equals("Timeout Between Projection Transfers (ms)")) {
                    fileInfo.setTimeoutBetweenProjectionTransfers(Integer.valueOf(valString).intValue());
                    Preferences.debug("Timeout Between Projection Transfers (ms) = " + valString + "\n",
                                Preferences.DEBUG_FILEIO);

                } else if (keyString.equals("Auto-start the CBR Engine")) {
                    fileInfo.setAutoStartCBREngine(valString);
                    Preferences.debug("Auto-start the CBR Engine = " + valString + "\n", Preferences.DEBUG_FILEIO);

                } else if (keyString.equals("Projection Data already on CBR")) {
                    fileInfo.setProjectionDataAlreadyOnCBR(valString);
                    Preferences.debug("Projection Data already on CBR = " + valString + "\n", Preferences.DEBUG_FILEIO);

                } else {
                    Preferences.debug("Unknown tag = " + valString + "\n", Preferences.DEBUG_FILEIO);
                }

            }

            lineString = readLine();
        } // while (lineString != null)

        return fileInfo;
    }

    /**
     * Reads the image as a full 3D dataset.
     *
     * @param   quiet  Flag for printing statements to the screen.
     *
     * @return  The image.
     *
     * @throws  IOException  DOCUMENT ME!
     */
    public ModelImage readImage(boolean quiet) throws IOException {
        return readImage(quiet, false, "");
    }


    /**
     * Reads the image.
     *
     * @param      quiet      Flag for printing statements to the screen.
     * @param      one        Flag for reading only 1 image of 3D dataset
     * @param      inputName  If <code>one</code> is <code>true</code>, contains name of image to read.
     *
     * @return     returns the image
     *
     * @exception  IOException  if there is an error reading the file
     */
    public ModelImage readImage(boolean quiet, boolean one, String inputName) throws IOException {
        ModelImage image = null;
        float[] buffer;
        FileRaw imageFile;
        FileInfoMicroCat[] nFileInfos;
        FileInfoMicroCat fileInfo;
        String[] fileList;
        int i;
        int length;
        int nImages;

        try {
            fileInfo = readHeader();

            if (one) {
                fileList = new String[] { inputName };
            } else {
                fileList = getFileList(fileDir, fileInfo.getBaseNameforReconstructedSlices(), quiet);
            }

            if (fileList == null) {
                return null;
            }

            fireProgressStateChanged(0);
            i = 0;


            nImages = fileList.length; // total number of suspected files to import into an image

            // if nImages == 1 then display error and return ???
            nFileInfos = new FileInfoMicroCat[nImages];

            if (nImages > 1) {
                int[] extents = new int[3];
                extents[0] = fileInfo.getVolumeSizeX();
                extents[1] = fileInfo.getVolumeSizeY();
                extents[2] = nImages;

                // extents[2] = fileInfo.volumeSizeZ;
                fileInfo.setExtents(extents);

                float[] resols = new float[3];
                resols[0] = fileInfo.getVoxelSizeX();
                resols[1] = fileInfo.getVoxelSizeY();
                resols[2] = fileInfo.getVoxelSizeZ();
                fileInfo.setResolutions(resols);
            } else {
                int[] extents = new int[2];
                extents[0] = fileInfo.getVolumeSizeX();
                extents[1] = fileInfo.getVolumeSizeY();
                fileInfo.setExtents(extents);

                float[] resols = new float[2];
                resols[0] = fileInfo.getVoxelSizeX();
                resols[1] = fileInfo.getVoxelSizeY();
                fileInfo.setResolutions(resols);
            }

            fileInfo.setDataType(ModelStorageBase.SHORT);
            length = fileInfo.getExtents()[0] * fileInfo.getExtents()[1];

            buffer = new float[length];
            image = new ModelImage(fileInfo.getDataType(), fileInfo.getExtents(), fileName);

            // Copy all
            if (fileInfo.getExtents().length > 2) { // Set file info

                for (i = 0; i < nImages; i++) {
                    nFileInfos[i] = (FileInfoMicroCat) (fileInfo.clone());
                }

                image.setFileInfo(nFileInfos);
            } else {
                image.setFileInfo(fileInfo, 0);
            }


            for (int m = 0; m < nImages; m++) {
                imageFile = new FileRaw(fileList[m], fileDir, fileInfo, FileBase.READ);
                fireProgressStateChanged((int) (((float) m / (float) nImages) * 100.0f));
                imageFile.readImage(buffer, (long)fileInfo.getOffset(), fileInfo.getDataType());
                image.importData(m * length, buffer, false);
            }


        } catch (IOException error) {

            if (image != null) {
                image.disposeLocal();
                image = null;
            }

            buffer = null;
            System.gc();

            if (!quiet) {
                MipavUtil.displayError("FileIO: " + error);
            }

            return null;
        } catch (OutOfMemoryError error) {

            if (image != null) {
                image.disposeLocal();
                image = null;
            }

            buffer = null;

            System.gc();

            if (!quiet) {
                MipavUtil.displayError("FileIO: " + error);
            }

            return null;
        } catch (NullPointerException error) {

            if (image != null) {
                image.disposeLocal();
                image = null;
            }

            buffer = null;

            System.gc();

            if (!quiet) {
                MipavUtil.displayError("FileIO: " + error);
            }

            return null;
        }

        return image;

    }

    /**
     * Sets the file name.
     *
     * @param  fileName  New file name.
     */
    public void setFileName(String fileName) {
        this.fileName = fileName;
    }


    /**
     * DOCUMENT ME!
     *
     * @param   str  DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    private String getKeyString(String str) {

        int index;

        if (str != null) {
            index = str.indexOf(":");

            if (index != -1) {
                str = str.substring(0, index);
                str = str.trim();
            } else {
                str = null;
            }
        }

        return str;
    }

    /**
     * DOCUMENT ME!
     *
     * @param   str  DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    private String getValString(String str) {

        int index;

        if (str != null) {
            index = str.indexOf(":");

            if (index != -1) {
                str = str.substring(index + 1, str.length());
                str = str.trim();
            } else {
                str = null;
            }
        }

        return str;
    }

    /**
     * Reads lines of the file and strips comments indicated by the : symbol until a nonnull String results or the end
     * of the file is reached.
     *
     * @return     the line read in
     *
     * @exception  IOException  if there is an error reading the file
     */
    private String readLine() throws IOException {
        String tempString = null;

        try {
            tempString = raFile.readLine();
        } catch (EOFException error) {
            tempString = null;
            raFile.close();

            return null;
        } catch (IOException error) {
            raFile.close();
            throw (error);
        }

        if ((tempString != null) && (tempString.length() == 0)) {
            tempString = null;
        }

        return tempString;
    }

}
