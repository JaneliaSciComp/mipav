package gov.nih.mipav.model.file;


import gov.nih.mipav.model.file.FileInfoBase.Unit;
import gov.nih.mipav.model.structures.*;

import gov.nih.mipav.view.*;

import java.io.*;


/**
 * The class reads and writes Analyze Version 7.5.x files.
 * Note that there are actually 3 variants of Analyze, all using .img and .hdr files,
 * with very slight differences:
 * Mayo Analyze 7.5, SPM99, and SPM2
 * Variant differences: 
 * Location 0 is an int32 with sizeof_hdr.  In Non-SPM2, always 348, used to
 * test whether the file is big-endian or little-endian.  SPM2 can be 348 or greater.  If SPM2 > 348,
 * indicates extended header.
 * 
 * Original Mayo Analyze 7.5, SPM99, and SPM2 all have at location 60 a uchar[8] for cal_units and at location 
 * 68 an int16 for unused1.  However, MIPAV analyze only preserves at location 60 a uchar[4] for cal_units and
 * has hacked locations 64, 66, and 68 with short integers for axis orientation.  MIPAV calls these locations 
 * unused1, unused2, and unused3.  Note that MIPAV's unused3 is at the same location as unused1 in any
 * original format.
 * 
 * Original Mayo Analyze 7.5 has at locations 112, 116, and 120 3 unused floats, funused1, funused2, and
 * funused3.  SPM99 and SPM2 have at location 112 a float scale factor.  SPM2 has at location 116 a float 
 * dcoff, which is an intensity zero-intercept.  MIPAV analyze has hacked these 3 locations to store 3 floats
 * giving the x-origin, y-origin, and z-origin locations.
 * 
 * Mayo Analyze 7.5 has at location 253 a uchar[10] called originator.  SPM99 and SPM2 have at location 253
 * 5-int16 called origin[0] thru origin[4].  In SPM99 X, Y, and Z are near the anterior commissure. 
 * If the first 3 shorts of a 3D image are set to 0, 0, 0, the origin is assumed to be at the center of
 * the volume, since in SPM the corner voxel is at 1, 1, 1. The position(x,y,z) in mm. is determined by the
 * distance(x,y,z) from the origin multiplied by the vox_units(x,y,z).</p>
 *
 * <p>In SPM the signed byte datatype was added with DT_BYTE = 130. MIPAV ANALYZE uses UNSIGNED_SHORT = 6 while
 * SPM uses DT_UNSIGNED_SHORT = 132. The SPM standard also provides for an unsigned int = 136, but MIPAV does not
 * used the unsigned int data type. Note that in SPM while DATA = datatype * 256 for swapped bytes, only datatype
 * and not DATA is written to the file.
 * 
 * The most obvious way to test for whether a file is Mayo or SPM would be to see if location 112 is a
 * funused1 = 0 or a nonzero scale factor, but this is complicated by the MIPAV hacking at 112 to store the
 * x-origin location as a float.  Asking the users what variant there analyze file is would probably cause
 * massive confusion, so it is probably best just to default to the hacked 7.5 code here unless a pressing 
 * need arises.
 *  
 *
 * @version  0.1 Oct 14, 1997
 * @author   Matthew J. McAuliffe, Ph.D.
 * @see      FileIO
 * @see      FileInfoAnalyze
 * @see      FileRaw
 */

public class FileAnalyze extends FileBase {

    //~ Static fields/initializers -------------------------------------------------------------------------------------

    /** The extensions of ANALYZE file. */
    public static final String[] EXTENSIONS = { ".hdr", ".img" };

    /** The size of the header, always 348 for Analyze images. */
    public static final int HEADER_SIZE = 348;

    /** The extent size of the ANALYZE file. */
    private static final int EXTENTS = 16384;

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** Storage buffer for the header. */
    private byte[] bufferImageHeader = null;

    /** File directory of the image. */
    private String fileDir;

    /** Reference to the header file (*.hdr). */
    private File fileHeader;

    /** Reference to the file info. for an Analyze header */
    private FileInfoAnalyze fileInfo = null;

    /** File name of the image. */
    private String fileName;

    /** Header size for an Analyze image is 348. */
    private int headerSize = 348;

    /** The image read in from the file. */
    private ModelImage image;

    /** Voxel offset tag used to read in the image. */
    private float vox_offset = 0.0f;
    
    /** If true, zero funused fields */
    private boolean zerofunused = false;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Creates a new FileAnalyze object.
     *
     * @param  fileNames  DOCUMENT ME!
     */
    public FileAnalyze(String[] fileNames) {
        super(fileNames);
    }

    /**
     * Constructs new file object.
     *
     * @param  fName  File name.
     * @param  fDir   File directory.
     */
    public FileAnalyze(String fName, String fDir) {
        String fileDataName;
        File fileData;
        int index = fName.length();

        for (int i = fName.length() - 1; i >= 0; i--) {

            if (fName.charAt(i) == '.') {
                index = i;

                break;
            }
        }
        if (fName.substring(index).equalsIgnoreCase(".HDR")) {
            fileDataName = fName.substring(0, index) + ".img";
            fileData = new File(fDir + fileDataName);
            if (fileData.exists()) {
                fName = fileDataName;
            }
            else {
                fileDataName = fName.substring(0, index) + ".IMG";
                fileData = new File(fDir + fileDataName); 
                if (fileData.exists()) {
                    fName = fileDataName;
                }
            }
        }
        fileName = fName;
        fileDir = fDir;
    }

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * Prepares this class for cleanup. Calls the <code>finalize</code> method for existing elements, closes any open
     * files and sets other elements to <code>null</code>.
     */
    public void finalize() {
        bufferImageHeader = null;
        fileName = null;
        fileDir = null;
        fileInfo = null;
        image = null;
        try {
            super.finalize();
        } catch (Throwable er) { }
    }
    
    /**
     * 
     * @param zerofunused
     */
    public void setZerofunused(boolean zerofunused) {
    	this.zerofunused = zerofunused;
    }
    
    /**
     * Flips image. Analyze stores its data "upside down".
     *
     * @param   image  Image to flip.
     *
     * @throws  IOException  DOCUMENT ME!
     */
    public static final void flipTopBottom(ModelImage image) throws IOException {

        try {
            int nBuffers;
            int bufferSize;
            float[] buffer = null;
            float[] resultBuffer = null;

            if (image.getNDims() > 1) {
                bufferSize = image.getSliceSize();
            } else {
                bufferSize = image.getExtents()[0];
            }

            if (image.getNDims() == 5) {
                nBuffers = image.getExtents()[4] * image.getExtents()[3] * image.getExtents()[2];

            } else if (image.getNDims() == 4) {
                nBuffers = image.getExtents()[3] * image.getExtents()[2];
            } else if (image.getNDims() == 3) {
                nBuffers = image.getExtents()[2];
            } else {
                nBuffers = 1;
            }

            if (image.isColorImage()) {

                buffer = new float[bufferSize * 4];
                resultBuffer = new float[bufferSize * 4];
                bufferSize = bufferSize * 4;

                int i, j, k;
                int xDim = image.getExtents()[0] * 4;
                int yDim = image.getExtents()[1];

                for (k = 0; k < nBuffers; k++) {
                    image.exportData(k * bufferSize, bufferSize, buffer);

                    for (j = 0; j < yDim; j++) {

                        for (i = 0; i < xDim; i += 4) {
                            resultBuffer[(j * xDim) + i] = 255;
                            resultBuffer[(j * xDim) + i + 1] = buffer[((yDim - 1 - j) * xDim) + i + 1];
                            resultBuffer[(j * xDim) + i + 2] = buffer[((yDim - 1 - j) * xDim) + i + 2];
                            resultBuffer[(j * xDim) + i + 3] = buffer[((yDim - 1 - j) * xDim) + i + 3];
                        }
                    }

                    image.importData(k * bufferSize, resultBuffer, false);
                }
            } else if (image.getType() == ModelStorageBase.COMPLEX) {

                buffer = new float[bufferSize * 2];
                resultBuffer = new float[bufferSize * 2];
                bufferSize = bufferSize * 2;

                int i, j, k;
                int xDim = image.getExtents()[0] * 2;
                int yDim = image.getExtents()[1];

                for (k = 0; k < nBuffers; k++) {
                    image.exportData(k * bufferSize, bufferSize, buffer);

                    for (j = 0; j < yDim; j++) {

                        for (i = 0; i < xDim; i += 2) {
                            resultBuffer[(j * xDim) + i] = buffer[((yDim - 1 - j) * xDim) + i];
                            resultBuffer[(j * xDim) + i + 1] = buffer[((yDim - 1 - j) * xDim) + i + 1];
                        }
                    }

                    image.importData(k * bufferSize, resultBuffer, false);
                }
            } else {
                buffer = new float[bufferSize];
                resultBuffer = new float[bufferSize];

                int i, j, k;
                int xDim = image.getExtents()[0];
                int yDim = image.getExtents()[1];

                for (k = 0; k < nBuffers; k++) {
                    image.exportData(k * bufferSize, bufferSize, buffer);

                    for (j = 0; j < yDim; j++) {

                        for (i = 0; i < xDim; i++) {
                            resultBuffer[(j * xDim) + i] = buffer[((yDim - 1 - j) * xDim) + i];
                        }
                    }

                    image.importData(k * bufferSize, resultBuffer, false);
                }
            }
        } catch (IOException error) {
            throw new IOException("FileAnalyze.flipTopBottom: " + error);
        } catch (OutOfMemoryError error) {
            throw (error);
        }
    }

    /**
     * Returns the complete list of file names according to given file name.
     *
     * @param   absolutePath  one file name of ANALYZE.
     *
     * @return  the complete list of file names.
     */
    public static String[] getCompleteFileNameList(String absolutePath) {
        String[] completeFileNameList = new String[2];

        if (isHeaderFile(absolutePath)) {
            completeFileNameList[0] = absolutePath;

            completeFileNameList[1] = absolutePath.substring(0, absolutePath.lastIndexOf(".")) + EXTENSIONS[1];
        } else if (isImageFile(absolutePath)) {
            completeFileNameList[1] = absolutePath;
            
            int extLoc = absolutePath.lastIndexOf(".");
            if(extLoc == -1) {
                extLoc = absolutePath.length(); //isImageFile() allows for extension-less images
            }
            
            completeFileNameList[0] = absolutePath.substring(0, extLoc) + EXTENSIONS[0];
        } else {
            completeFileNameList = null;
        }

        return completeFileNameList;
    }

    /**
     * Returns the header file (ends in .hdr) as a string.
     *
     * @param   fileNames  DOCUMENT ME!
     *
     * @return  The header file (ends in .hdr)
     */
    public static String getHeaderFile(String[] fileNames) {

        if ((fileNames == null) || (fileNames.length != 2)) {
            return null;
        }

        for (int i = 0; i < fileNames.length; i++) {

            if (FileUtility.getExtension(fileNames[i]).equals(EXTENSIONS[0])) {
                return fileNames[i];
            }
        }

        return null;
    }

    /**
     * Returns the image file list.
     *
     * @param   fileNames  DOCUMENT ME!
     *
     * @return  the image file list.
     */
    public static String[] getImageFiles(String[] fileNames) {

        if (fileNames == null) {
            return null;
        }

        String[] result = new String[1];

        for (int i = 0; i < fileNames.length; i++) {

            if (FileUtility.getExtension(fileNames[i]).equals(EXTENSIONS[1])) {
                result[0] = fileNames[i];

                return result;
            }
        }

        return null;
    }

    /**
     * Determines whether this file is ANALYZE file or not based on three fields of the header file: sizeof_hdr, extent
     * and regular.  Note that all Mayo Analyze 7.5 and all SPM99 variants will pass this test.  SPM2 variants with
     * a regular header size of 348 will also pass.  Only SPM2 files with an extended header > 348 in size will
     * fail.
     *
     * @param   absolutePath  the file name.
     *
     * @return  FileUtility.ANALYZE, FileUtility.SPM, or FileUtility.UNDEFINED
     *
     * @throws  FileNotFoundException  thrown when the file can't be found.
     * @throws  IOException            thrown when the I/O error happens.
     */
    public static int isAnalyzeOrSPM(String absolutePath) throws FileNotFoundException, IOException {
        if ((absolutePath == null) || (absolutePath.length() == 0)) {
            return FileUtility.UNDEFINED;
        }

        String[] completeFileNames = getCompleteFileNameList(absolutePath);

        if ((completeFileNames == null) || (completeFileNames.length != 2)) {
            return FileUtility.UNDEFINED;
        }

        for (int i = 0; i < completeFileNames.length; i++) {
            File file = new File(completeFileNames[i]);

            if (!file.exists()) {
                Preferences.debug("FileAnalyze: The file can not be found: " + completeFileNames[i] + "!",
                		Preferences.DEBUG_FILEIO);
                return FileUtility.UNDEFINED;
            }
        }

        RandomAccessFile raFile = new RandomAccessFile(getHeaderFile(completeFileNames), "r");

        /** Check whether the value of sizeof_hdr field equals to the 348 */
        byte[] buffer = new byte[4];
        raFile.readFully(buffer);

        boolean bigEndian = true;
        int sizeOfHeader = FileBase.bytesToInt(bigEndian, 0, buffer);
        if (sizeOfHeader != HEADER_SIZE) {
            bigEndian = false;
            sizeOfHeader = FileBase.bytesToInt(bigEndian, 0, buffer);
            if (sizeOfHeader != HEADER_SIZE) {
                return FileUtility.UNDEFINED;
            }
        }

        /** Check whether the value of extents field equals to 16384 */
        raFile.seek(32);
        raFile.readFully(buffer);
        
        int extents = FileBase.bytesToInt(bigEndian, 0, buffer);

        if (extents != EXTENTS) {
            Preferences.debug("extents = " + extents + " instead of the expected 16384\n", Preferences.DEBUG_FILEIO);
        }

        /** Check whether the value of regular field equals to "r" */
        raFile.seek(38);

        byte regular = raFile.readByte();
        if (regular != 'r') {
            Preferences.debug("regular = " + regular + " instead of the expected 'r'\n", Preferences.DEBUG_FILEIO);
        }
        
        // 80 byte description field
        raFile.seek(148);
        byte description[] = new byte[80];
        raFile.readFully(description);
        raFile.close();
        String desc = new String(description);
        if ((desc.contains("spm")) || (desc.contains("SPM"))) {
            return FileUtility.SPM;
        }

        return FileUtility.ANALYZE;

    }

    /**
     * Return true if the file specified by absolutePath is header file of ANALYZE.
     *
     * @param   absolutePath  the file name including path information.
     *
     * @return  true if the specified file is header file.
     */
    public static boolean isHeaderFile(String absolutePath) {
        String fileName = FileUtility.getFileName(absolutePath);
        String extension = FileUtility.getExtension(fileName);

        if (extension.equalsIgnoreCase(EXTENSIONS[0])) {
            return true;
        }

        return false;
    }

    /**
     * Return true if the file specified by absolutePath is image file of ANALYZE.
     *
     * @param   absolutePath  the file name including path information.
     *
     * @return  true if the specified file is image file.
     */
    public static boolean isImageFile(String absolutePath) {
        String fileName = FileUtility.getFileName(absolutePath);
        String extension = FileUtility.getExtension(fileName);

        if (extension.equalsIgnoreCase(EXTENSIONS[1])) {
            return true;
        } else if (extension.length() == 0) { // Support for image file with no extension.
            return true;
        }

        return false;
    }

    /**
     * Take the absolute value of image.
     *
     * @param   image  Image to take absolute value of.
     *
     * @throws  IOException  DOCUMENT ME!
     */
    public void absoluteValue(ModelImage image) throws IOException {

        try {
            int nBuffers;
            int bufferSize;
            float[] buffer = null;

            if (image.getNDims() > 1) {
                bufferSize = image.getSliceSize();
            } else {
                bufferSize = image.getExtents()[0];
            }

            if (image.getNDims() == 5) {
                nBuffers = image.getExtents()[4] * image.getExtents()[3] * image.getExtents()[2];

            } else if (image.getNDims() == 4) {
                nBuffers = image.getExtents()[3] * image.getExtents()[2];
            } else if (image.getNDims() == 3) {
                nBuffers = image.getExtents()[2];
            } else {
                nBuffers = 1;
            }


            if (!image.isColorImage() && (image.getType() != ModelStorageBase.COMPLEX) &&
                    (image.getType() != ModelStorageBase.DCOMPLEX)) {
                buffer = new float[bufferSize];

                int i, j, k;
                int xDim = image.getExtents()[0];
                int yDim = image.getExtents()[1];

                for (k = 0; k < nBuffers; k++) {
                    image.exportData(k * bufferSize, bufferSize, buffer);

                    for (j = 0; j < yDim; j++) {

                        for (i = 0; i < xDim; i++) {
                            buffer[(j * xDim) + i] = Math.abs(buffer[(j * xDim) + i]);
                        }
                    }

                    image.importData(k * bufferSize, buffer, false);
                }
            }
        } catch (IOException error) {
            throw new IOException("FileAnalyze.absoluteValue: " + error);
        } catch (OutOfMemoryError error) {
            throw (error);
        }
    }


    /**
     * Flips image. Analyze stores its data "upside down".
     *
     * @param   buffer    Buffer holding image to flip.
     * @param   fileInfo  File info structure for image to flip.
     *
     * @throws  IOException  DOCUMENT ME!
     */
    public void flipTopBottom(float[] buffer, FileInfoAnalyze fileInfo) throws IOException {
        int nBuffers;
        int bufferSize;
        float[] resultBuffer = null;

        try {

            if ((fileInfo.getExtents().length - 1) > 1) {
                bufferSize = fileInfo.getExtents()[0] * fileInfo.getExtents()[1];
            } else {
                bufferSize = fileInfo.getExtents()[0];
            }

            if ((fileInfo.getExtents().length - 1) == 5) {
                nBuffers = fileInfo.getExtents()[4] * fileInfo.getExtents()[3] * fileInfo.getExtents()[2];

            } else if ((fileInfo.getExtents().length - 1) == 4) {
                nBuffers = fileInfo.getExtents()[3] * fileInfo.getExtents()[2];
            } else if ((fileInfo.getExtents().length - 1) == 3) {
                nBuffers = fileInfo.getExtents()[2];
            } else {
                nBuffers = 1;
            }

            if (fileInfo.getDataTypeCode() == FileInfoAnalyze.DT_RGB) {

                resultBuffer = new float[bufferSize * 4];
                bufferSize = bufferSize * 4;

                int i, j, k;
                int xDim = image.getExtents()[0] * 4;
                int yDim = image.getExtents()[1];

                for (k = 0; k < nBuffers; k++) {

                    for (j = 0; j < yDim; j++) {

                        for (i = 0; i < xDim; i += 4) {
                            resultBuffer[(j * xDim) + i] = 255;
                            resultBuffer[(k * bufferSize) + (j * xDim) + i + 1] = buffer[(k * bufferSize) +
                                                                                         ((yDim - 1 - j) * xDim) + i + 1];
                            resultBuffer[(k * bufferSize) + (j * xDim) + i + 2] = buffer[(k * bufferSize) +
                                                                                         ((yDim - 1 - j) * xDim) + i + 2];
                            resultBuffer[(k * bufferSize) + (j * xDim) + i + 3] = buffer[(k * bufferSize) +
                                                                                         ((yDim - 1 - j) * xDim) + i + 3];
                        }
                    }
                }
            } else if (fileInfo.getDataTypeCode() == FileInfoAnalyze.DT_COMPLEX) {

                resultBuffer = new float[bufferSize * 2];
                bufferSize = bufferSize * 2;

                int i, j, k;
                int xDim = image.getExtents()[0] * 2;
                int yDim = image.getExtents()[1];

                for (k = 0; k < nBuffers; k++) {

                    for (j = 0; j < yDim; j++) {

                        for (i = 0; i < xDim; i += 2) {
                            resultBuffer[(j * xDim) + i] = buffer[(k * bufferSize) + ((yDim - 1 - j) * xDim) + i];
                            resultBuffer[(k * bufferSize) + (j * xDim) + i + 1] = buffer[(k * bufferSize) +
                                                                                         ((yDim - 1 - j) * xDim) + i + 1];
                        }
                    }
                }
            } else {
                resultBuffer = new float[buffer.length];

                int i, j, k;
                int xDim = fileInfo.getExtents()[0];
                int yDim = fileInfo.getExtents()[1];

                for (k = 0; k < nBuffers; k++) {

                    for (j = 0; j < yDim; j++) {

                        for (i = 0; i < xDim; i++) {
                            resultBuffer[(k * bufferSize) + (j * xDim) + i] = buffer[(k * bufferSize) +
                                                                                     ((yDim - 1 - j) * xDim) + i];
                        }
                    }
                }
            }
        } catch (OutOfMemoryError error) {
            throw (error);
        }

        System.arraycopy(resultBuffer, 0, buffer, 0, buffer.length); // buffer = resultBuffer;
    }

    /**
     * Returns the FileInfoAnalyze read from the file.
     *
     * @return  File info read from file, or null if it has not been read.
     */
    public FileInfoAnalyze getFileInfo() {
        return fileInfo;
    }

    /**
     * Returns the header file.
     *
     * @return  DOCUMENT ME!
     */
    public String getHeaderFile() {

        if ((fileNames == null) || (fileNames.length != 2)) {
            return null;
        }

        for (int i = 0; i < fileNames.length; i++) {

            if (FileUtility.getExtension(fileNames[i]).equals(EXTENSIONS[0])) {
                return fileNames[i];
            }
        }

        return null;
    }

    /**
     * Returns the image file list.
     *
     * @return  the image file list.
     */
    public String[] getImageFiles() {

        if ((fileNames == null) || (fileNames.length != 2)) {
            return null;
        }

        String[] result = new String[1];

        for (int i = 0; i < fileNames.length; i++) {

            if (FileUtility.getExtension(fileNames[i]).equals(EXTENSIONS[1])) {
                result[0] = fileNames[i];

                return result;
            }
        }

        return null;
    }

    /**
     * Reads the analyze header and stores the information in fileInfo.
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
        int i, j;
        int index;
        String fileHeaderName;
        boolean endianess;
        int[] analyzeExtents = new int[5];
        int numDims = 0;
        float[] pixdim;

        bufferImageHeader = new byte[headerSize];
        index = fileName.length();

        for (i = fileName.length() - 1; i >= 0; i--) {

            if (fileName.charAt(i) == '.') {
                index = i;

                break;
            }
        }

        fileHeaderName = fileName.substring(0, index) + ".hdr";
        Preferences.debug(" fileHeaderName = " + fileHeaderName + "\n", Preferences.DEBUG_FILEIO);
        fileHeader = new File(fileDir + fileHeaderName);

        if (fileHeader.exists() == false) {
            Preferences.debug(fileDir + fileHeaderName + " cannot be found.\n", Preferences.DEBUG_FILEIO);
            fileHeaderName = fileName.substring(0, index) + ".HDR";
            Preferences.debug("fileHeaderName = " + fileHeaderName + "\n", Preferences.DEBUG_FILEIO);
            fileHeader = new File(fileDir + fileHeaderName);

            if (fileHeader.exists() == false) {
                Preferences.debug(fileDir + fileHeaderName + " cannot be found.\n", Preferences.DEBUG_FILEIO);

                return false;
            }
        }

        // Required to read in multi-file analyze images. i.e. a set of 3D images to make a 4D dataset
        // The files should all have the same prefix. fooR_001.img, fooR_002.img etc.
        if (fileInfo == null) { // if the file info does not yet exist: make it
            Preferences.debug("fileInfo is null\n", Preferences.DEBUG_FILEIO);
            fileInfo = new FileInfoAnalyze(imageFileName, fileDir, FileUtility.ANALYZE);

            if (!readHeader(fileInfo.getFileName(), fileInfo.getFileDirectory())) { // Why 3/20/2001
                throw (new IOException(" Analyze header file error"));
            }
        }

        try {
            raFile = new RandomAccessFile(fileHeader, "r");
        } catch (FileNotFoundException e) {
            Preferences.debug("raFile = new RandomAccessFile(fileHeader, r) gave " + "FileNotFoundException " + e
            		+ "\n", Preferences.DEBUG_FILEIO);
            throw new IOException("Error on raFile = new RandomAccessFile(fileHeader,r)");
        }

        try {
            raFile.read(bufferImageHeader);
        } catch (IOException e) {
            Preferences.debug("raFile.read(bufferImageHeader gave IOException " + e + "\n", Preferences.DEBUG_FILEIO);
            throw new IOException(" Error on raFile.read(bufferImageHeader)");
        }

        try {
            raFile.close();
        } catch (IOException e) {
            Preferences.debug("raFile.close() gave IOException " + e + "\n", Preferences.DEBUG_FILEIO);
            throw new IOException(" Error on raFile.close()");
        }

        fileInfo.setEndianess(BIG_ENDIAN);
        fileInfo.setSizeOfHeader(getBufferInt(bufferImageHeader, 0, BIG_ENDIAN));

        if (fileInfo.getSizeOfHeader() != headerSize) { // Set the endianess based on header size = 348 Big Endian
            fileInfo.setEndianess(LITTLE_ENDIAN); // or 1,543,569,408 Little endian
            fileInfo.setSizeOfHeader(getBufferInt(bufferImageHeader, 0, LITTLE_ENDIAN));
            Preferences.debug("FileAnalyze:readHeader Endianess = Little endian.\n", Preferences.DEBUG_FILEIO);
        } else {
            Preferences.debug("FileAnalyze:readHeader Endianess = Big endian.\n", Preferences.DEBUG_FILEIO);
        }

        if (fileInfo.getSizeOfHeader() != headerSize) {
            Preferences.debug("FileAnalyze:readHeader Analyze header length = " + fileInfo.getSizeOfHeader() +
                              " instead of expected 348.\n", Preferences.DEBUG_FILEIO);

            return false;
        }

        endianess = fileInfo.getEndianess();

        // The following reads in certain tags.  In some cases, it returns false and exits out of readHeader
        // if the information is wrong.
        fileInfo.setDataType(new String(bufferImageHeader, 4, 10));
        Preferences.debug("Data type name = " + fileInfo.getDataTypeName() + "\n", Preferences.DEBUG_FILEIO);

        fileInfo.setDBname(new String(bufferImageHeader, 14, 18));
        Preferences.debug("DB name = " + fileInfo.getDBname() + "\n", Preferences.DEBUG_FILEIO);

        fileInfo.setSessionErr(getBufferShort(bufferImageHeader, 36, endianess));
        Preferences.debug("session error = " + fileInfo.getSessionErr() + "\n", Preferences.DEBUG_FILEIO);
        fileInfo.setRegular((char) bufferImageHeader[38]);

        if (fileInfo.getRegular() != 'r') {
            Preferences.debug("fileInfo.getRegular() gave " + fileInfo.getRegular() + " instead of expected r\n",
            		Preferences.DEBUG_FILEIO);
        }

        fileInfo.setHkey((char) bufferImageHeader[39]);

        int dims = getBufferShort(bufferImageHeader, 40, endianess); // number of Dimensions should = 4

        // analyze dims = 4
        Preferences.debug("FileAnalyze:readHeader. Number of dimensions = " + dims + "\n", Preferences.DEBUG_FILEIO);

        for (i = 0; i < dims; i++) {
            analyzeExtents[i] = getBufferShort(bufferImageHeader, 42 + (2 * i), endianess);
            Preferences.debug("FileAnalyze:readHeader. Dimension " + (i + 1) + " = " + analyzeExtents[i] + "\n",
            		Preferences.DEBUG_FILEIO);

            if (analyzeExtents[i] > 1) {
                numDims++;
            }
        }

        int[] extents = new int[numDims];

        for (i = 0, j = 0; i < dims; i++) {

            if (analyzeExtents[i] > 1) {
                extents[j++] = analyzeExtents[i];
            }
        }

        fileInfo.setExtents(extents);
        fileInfo.setVoxUnits(new String(bufferImageHeader, 56, 4));
        Preferences.debug("FileAnalyze:readHeader. Voxel unit = " + fileInfo.getVoxUnits() + "\n", Preferences.DEBUG_FILEIO);
        fileInfo.setCalUnits(new String(bufferImageHeader, 60, 4));
        
        fileInfo.setOrientation((byte) bufferImageHeader[252]);

        // MIPAV is hacking the analyze format to use the unused
        // variables to store axis orientation.
        fileInfo.setUnused1(getBufferShort(bufferImageHeader, 64, endianess));
        fileInfo.setUnused2(getBufferShort(bufferImageHeader, 66, endianess));
        fileInfo.setUnused3(getBufferShort(bufferImageHeader, 68, endianess));
        
        int xAxisOrientation = fileInfo.getAxisOrientation()[0];
        int yAxisOrientation = fileInfo.getAxisOrientation()[1];
        int zAxisOrientation = fileInfo.getAxisOrientation()[2];
        if  ((xAxisOrientation == FileInfoBase.ORI_R2L_TYPE) &&
             (yAxisOrientation ==  FileInfoBase.ORI_A2P_TYPE) &&
             (zAxisOrientation ==  FileInfoBase.ORI_I2S_TYPE)) {
            fileInfo.setOrientation(FileInfoSPM.TRANSVERSE_UNFLIPPED);
        }
        else if ((xAxisOrientation == FileInfoBase.ORI_R2L_TYPE) &&
                (yAxisOrientation ==  FileInfoBase.ORI_P2A_TYPE) &&
                (zAxisOrientation ==  FileInfoBase.ORI_I2S_TYPE)) {
               fileInfo.setOrientation(FileInfoSPM.TRANSVERSE_FLIPPED);
        }
        else if ((xAxisOrientation == FileInfoBase.ORI_R2L_TYPE) &&
                (yAxisOrientation ==  FileInfoBase.ORI_S2I_TYPE) &&
                (zAxisOrientation ==  FileInfoBase.ORI_P2A_TYPE)) {
               fileInfo.setOrientation(FileInfoSPM.CORONAL_UNFLIPPED);
        }
        else if ((xAxisOrientation == FileInfoBase.ORI_R2L_TYPE) &&
                (yAxisOrientation ==  FileInfoBase.ORI_I2S_TYPE) &&
                (zAxisOrientation ==  FileInfoBase.ORI_P2A_TYPE)) {
               fileInfo.setOrientation(FileInfoSPM.CORONAL_FLIPPED);
        }
        else if ((xAxisOrientation == FileInfoBase.ORI_P2A_TYPE) &&
                (yAxisOrientation ==  FileInfoBase.ORI_S2I_TYPE) &&
                (zAxisOrientation ==  FileInfoBase.ORI_R2L_TYPE)) {
               fileInfo.setOrientation(FileInfoSPM.SAGITTAL_UNFLIPPED);
        }
        else if ((xAxisOrientation == FileInfoBase.ORI_P2A_TYPE) &&
                (yAxisOrientation ==  FileInfoBase.ORI_I2S_TYPE) &&
                (zAxisOrientation ==  FileInfoBase.ORI_R2L_TYPE)) {
               fileInfo.setOrientation(FileInfoSPM.SAGITTAL_FLIPPED);
        }
        else if ((xAxisOrientation != FileInfoBase.ORI_UNKNOWN_TYPE) &&
            (yAxisOrientation != FileInfoBase.ORI_UNKNOWN_TYPE) &&
            (zAxisOrientation != FileInfoBase.ORI_UNKNOWN_TYPE)) {
           if ((zAxisOrientation == FileInfoBase.ORI_I2S_TYPE) ||
               (zAxisOrientation == FileInfoBase.ORI_S2I_TYPE)) {
               fileInfo.setImageOrientation(FileInfoBase.AXIAL);
           }
           else if ((zAxisOrientation == FileInfoBase.ORI_A2P_TYPE) ||
                    (zAxisOrientation == FileInfoBase.ORI_P2A_TYPE)) {
               fileInfo.setImageOrientation(FileInfoBase.CORONAL);
           }
           else {
               fileInfo.setImageOrientation(FileInfoBase.SAGITTAL);
           }
        }
        else { // an axis has type FileInfoBase.ORI_UNKNOWN_TYPE;

            // this call will reset the axis orientations.
            fileInfo.setOrientation((byte) bufferImageHeader[252]);
        }

        fileInfo.setDataType(getBufferShort(bufferImageHeader, 70, endianess));
        Preferences.debug("FileAnalyze:readHeader. Data type = " + fileInfo.getDataTypeCode() + "\n",
        		Preferences.DEBUG_FILEIO);

        switch (fileInfo.getDataTypeCode()) { // Set the dataType in ModelStorage based on this tag

            case FileInfoAnalyze.DT_UNKNOWN:
                return false;

            case FileInfoAnalyze.DT_BINARY:
                fileInfo.setDataType(ModelStorageBase.BOOLEAN);
                break;

            case FileInfoAnalyze.DT_UNSIGNED_CHAR:
                fileInfo.setDataType(ModelStorageBase.UBYTE);
                break;

            case FileInfoAnalyze.DT_SIGNED_SHORT:
                fileInfo.setDataType(ModelStorageBase.SHORT);
                break;

            case FileInfoAnalyze.DT_SIGNED_INT:
                fileInfo.setDataType(ModelStorageBase.INTEGER);
                break;

            case FileInfoAnalyze.DT_FLOAT:
                fileInfo.setDataType(ModelStorageBase.FLOAT);
                break;

            case FileInfoAnalyze.DT_COMPLEX:
                fileInfo.setDataType(ModelStorageBase.COMPLEX);
                break;

            case FileInfoAnalyze.DT_DOUBLE:
                fileInfo.setDataType(ModelStorageBase.DOUBLE);
                break;

            case FileInfoAnalyze.DT_RGB:
                fileInfo.setDataType(ModelStorageBase.ARGB);
                break;

            default:
                return false;
        }

        fileInfo.setBitPix(getBufferShort(bufferImageHeader, 72, endianess));
        Preferences.debug("FileAnalyze:readHeader. bits per pixel = " + fileInfo.getBitPix() + "\n",
        		Preferences.DEBUG_FILEIO);

        fileInfo.setDim(getBufferShort(bufferImageHeader, 74, endianess));

        pixdim = new float[dims + 1];

        float[] resolutions = new float[numDims];

        for (i = 0, j = 0; i < (dims + 1); i++) {
            pixdim[i] = getBufferFloat(bufferImageHeader, 76 + (4 * i), endianess);

            if ((i >= 1) && (analyzeExtents[i - 1] > 1)) {
                resolutions[j] = Math.abs(pixdim[i]);
                Preferences.debug("FileAnalyze:readHeader. Resolutions " + (j + 1) + " = " + resolutions[j] + "\n",
                		Preferences.DEBUG_FILEIO);
                j++;
            }
        }

        fileInfo.setResolutions(resolutions);

        vox_offset = getBufferFloat(bufferImageHeader, 108, endianess);
        fileInfo.setOffset((int) vox_offset);

        // MIPAV is hacking the analyze format to use the unused
        // variables to store start locations.
        fileInfo.setFunused1(getBufferFloat(bufferImageHeader, 112, endianess));
        fileInfo.setFunused2(getBufferFloat(bufferImageHeader, 116, endianess));
        fileInfo.setFunused3(getBufferFloat(bufferImageHeader, 120, endianess));

        fileInfo.setCalMax(getBufferFloat(bufferImageHeader, 124, endianess));
        fileInfo.setCalMin(getBufferFloat(bufferImageHeader, 128, endianess));
        fileInfo.setCompressed(getBufferFloat(bufferImageHeader, 132, endianess));
        fileInfo.setVerified(getBufferFloat(bufferImageHeader, 136, endianess));

        fileInfo.setGLmax(getBufferInt(bufferImageHeader, 140, endianess));
        Preferences.debug("FileAnalyze:readHeader. global max intensity = " + fileInfo.getGLmax() + "\n",
        		Preferences.DEBUG_FILEIO);

        fileInfo.setGLmin(getBufferInt(bufferImageHeader, 144, endianess));
        Preferences.debug("FileAnalyze:readHeader. global min intensity = " + fileInfo.getGLmin() + "\n",
        		Preferences.DEBUG_FILEIO);

        fileInfo.setDescription(new String(bufferImageHeader, 148, 80));

        // update the fileInfo modality based on the description
        // if the description contains something other than modality, then
        // the modality will be set to unknown.
        fileInfo.setModality(FileInfoBase.getModalityFromStr(fileInfo.getDescription()));

        fileInfo.setAuxFile(new String(bufferImageHeader, 228, 24));
        fileInfo.setOriginator(new String(bufferImageHeader, 253, 10));
        fileInfo.setGenerated(new String(bufferImageHeader, 263, 10));
        fileInfo.setScanNum(new String(bufferImageHeader, 273, 10));
        fileInfo.setPatientID(new String(bufferImageHeader, 283, 10));
        fileInfo.setExperimentDate(new String(bufferImageHeader, 293, 10));
        fileInfo.setExperimentTime(new String(bufferImageHeader, 303, 10));
        fileInfo.setHist(new String(bufferImageHeader, 313, 3));

        fileInfo.setViews(getBufferInt(bufferImageHeader, 316, endianess));
        fileInfo.setVolsAdded(getBufferInt(bufferImageHeader, 320, endianess));
        fileInfo.setStartField(getBufferInt(bufferImageHeader, 324, endianess));
        fileInfo.setFieldSkip(getBufferInt(bufferImageHeader, 328, endianess));
        fileInfo.setOmax(getBufferInt(bufferImageHeader, 332, endianess));
        fileInfo.setOmin(getBufferInt(bufferImageHeader, 336, endianess));
        fileInfo.setSmax(getBufferInt(bufferImageHeader, 340, endianess));
        fileInfo.setSmin(getBufferInt(bufferImageHeader, 344, endianess));

        return true; // If it got this far, it has successfully read in the header
    }

    /**
     * Reads an analyze image file by reading the header then making a FileRaw to read the image for all filenames in
     * the file list. Only the one file directory (currently) supported.
     *
     * @param      one  flag indicating one image of a 3D dataset should be read in.
     *
     * @exception  IOException  if there is an error reading the file
     *
     * @return     The image.
     *
     * @see        FileRaw
     */
    public ModelImage readImage(boolean one) throws IOException, OutOfMemoryError {
        fileInfo = new FileInfoAnalyze(fileName, fileDir, FileUtility.ANALYZE);

        if (!readHeader(fileInfo.getFileName(), fileInfo.getFileDirectory())) {
            throw (new IOException(" Analyze header file error"));
        }

        int[] extents = null;

        try {

            if (one) {
                extents = new int[fileInfo.getExtents().length];

                for (int i = 0; i < extents.length; i++) {
                    extents[i] = fileInfo.getExtents()[i];
                }

                image = new ModelImage(fileInfo.getDataType(), new int[] { extents[0], extents[1] },
                                       fileInfo.getFileName());
            } else {
                image = new ModelImage(fileInfo.getDataType(), fileInfo.getExtents(), fileInfo.getFileName());
            }
        } catch (OutOfMemoryError error) {
            throw (error);
        }

        // if vox units defines the units of measure, then use that instead
        // clones the file info
        updateUnitsOfMeasure(fileInfo, image);
        updateStartLocations(image.getFileInfo());


        try { // Construct a FileRaw to actually read the image.

            FileRaw rawFile;
            rawFile = new FileRaw(fileInfo.getFileName(), fileInfo.getFileDirectory(), fileInfo, FileBase.READ);
            if (image.getType() == ModelStorageBase.BOOLEAN) {
                rawFile.setMinimumBitsMinus1(7);
                rawFile.setShiftToDivide(3);
            }
            linkProgress(rawFile);

            int offset = (int) Math.abs(vox_offset);

            if (one) {

                if (fileInfo.getExtents().length > 2) {
                    offset = offset + getOffset(fileInfo);
                }
            }

            rawFile.readImage(image, offset);

            if (vox_offset < 0.0f) {
                absoluteValue(image);
            }

            flipTopBottom(image);

            if (one) {
                fileInfo.setExtents(extents);
            }
        } catch (IOException error) {
            throw new IOException("FileAnalyze: " + error);
        } catch (OutOfMemoryError e) {
            throw (e);
        }

        return image;
    }

    /**
     * Reads an analyze image file by reading the header then making a FileRaw to read the file. Image data is left in
     * buffer. If the fileInfo cannot be found, the header will be located and read first. Image is not 'flipped', and
     * neither units of measure nor orientation are set.
     *
     * @param      buffer  Image buffer to store image data into.
     *
     * @exception  IOException  if there is an error reading the file
     *
     * @see        FileRaw
     */
    public void readImage(float[] buffer) throws IOException, OutOfMemoryError {
        int i;
        int index;
        String fileBase;
        String dataFileName;

        if (fileInfo == null) { // if no file info yet, make it.
            fileInfo = new FileInfoAnalyze(fileName, fileDir, FileUtility.ANALYZE);

            if (!readHeader(fileInfo.getFileName(), fileInfo.getFileDirectory())) {
                throw (new IOException("Cannot read image because of analyze header file error"));
            }
        }

        // if vox units defines the units of measure, then use that instead
        // updateUnitsOfMeasure(fileInfo);
        int units = (Unit.getUnit(fileInfo.getVoxUnits())).getLegacyNum();

        if (units == Unit.UNKNOWN_MEASURE.getLegacyNum()) { // default to millimeters
            fileInfo.setUnitsOfMeasure(Unit.MILLIMETERS.getLegacyNum(), 0);
            fileInfo.setUnitsOfMeasure(Unit.MILLIMETERS.getLegacyNum(), 1);
        } else {
            fileInfo.setUnitsOfMeasure(units, 0);
            fileInfo.setUnitsOfMeasure(units, 1);
        }


        try { // Construct a FileRaw to actually read the image.

            FileRaw rawFile;
            index = fileInfo.getFileName().lastIndexOf(".");
            fileBase = fileInfo.getFileName().substring(0, index+1);
            dataFileName = fileBase + "img";
            rawFile = new FileRaw(dataFileName, fileInfo.getFileDirectory(), fileInfo, FileBase.READ);
            if (fileInfo.getDataType() == ModelStorageBase.BOOLEAN) {
                rawFile.setMinimumBitsMinus1(7);
                rawFile.setShiftToDivide(3);
            }

            int offset = (int) Math.abs(vox_offset);
            rawFile.readImage(buffer, offset, fileInfo.getDataType());
            rawFile.raFile.close();

            if (vox_offset < 0.0f) {

                for (i = 0; i < buffer.length; i++) {
                    buffer[i] = Math.abs(buffer[i]);
                }
            }

            flipTopBottom(buffer, fileInfo);
        } catch (IOException error) {
            error.printStackTrace();
            throw new IOException("FileAnalyz: " + error);
        } catch (OutOfMemoryError e) {
            throw (e);
        }

        return;
    }

    // Is this method used any place ??
    /**
     * Takes the image and sets it to analyze defaults, using the specified info.
     *
     * @param  image     Image to set to analyze defaults.
     * @param  fileInfo  File info structure to change.
     */
    public void reOrgInfo(ModelImage image, FileInfoAnalyze fileInfo) {
        int i;

        fileInfo.setOrientation(fileInfo.getOrientation());

        if (image.getNDims() == 2) {
            fileInfo.setUnitsOfMeasure(Unit.MILLIMETERS.getLegacyNum(), 0);
            fileInfo.setUnitsOfMeasure(Unit.MILLIMETERS.getLegacyNum(), 1);
            image.setFileInfo(fileInfo, 0); // Otherwise just set the first fileInfo
        } else if (image.getNDims() == 3) { // If there is more than one image
            fileInfo.setUnitsOfMeasure(Unit.MILLIMETERS.getLegacyNum(), 0);
            fileInfo.setUnitsOfMeasure(Unit.MILLIMETERS.getLegacyNum(), 1);
            fileInfo.setUnitsOfMeasure(Unit.MILLIMETERS.getLegacyNum(), 2);

            for (i = 0; i < image.getExtents()[2]; i++) { // update all fileInfo
                image.setFileInfo(fileInfo, i); // Set the array of fileInfos in ModelImage
            }
        } else if (image.getNDims() == 4) { // If there is more than one image
            fileInfo.setUnitsOfMeasure(Unit.MILLIMETERS.getLegacyNum(), 0);
            fileInfo.setUnitsOfMeasure(Unit.MILLIMETERS.getLegacyNum(), 1);
            fileInfo.setUnitsOfMeasure(Unit.MILLIMETERS.getLegacyNum(), 2);
            fileInfo.setUnitsOfMeasure(Unit.MILLIMETERS.getLegacyNum(), 3);

            for (i = 0; i < (image.getExtents()[2] * image.getExtents()[3]); i++) { // update all fileInfo
                image.setFileInfo(fileInfo, i); // Set the array of fileInfos in ModelImage
            }
        }

    }

    /**
     * Writes an analyze format type image.
     *
     * @param      image  Image model of data to write.
     *
     * @exception  IOException  if there is an error writing the file
     *
     * @see        FileInfoAnalyze
     * @see        FileRaw
     */
    public void writeImage(ModelImage image, FileWriteOptions options) throws IOException {
        String fhName;
        int index;
        int nImagesSaved;
        int nTimePeriodsSaved;
        String suffix;


        index = fileName.lastIndexOf(".");

        if (index != -1) {
            fhName = fileName.substring(0, index);
            suffix = fileName.substring(index);
            if (suffix.equalsIgnoreCase(".hdr")) {
                fileName = fhName + ".img";
            }
        } else {
            fhName = fileName.substring(0);
        }

        if (options.isMultiFile()) {
            FileRaw rawFile;
            rawFile = new FileRaw(image.getFileInfo(0));
            if (image.getType() == ModelStorageBase.BOOLEAN) {
                rawFile.setMinimumBitsMinus1(7);
                rawFile.setShiftToDivide(3);
            }
            rawFile.setZeroLengthFlag(true);
            linkProgress(rawFile);
            flipTopBottom(image);

            if (image.getNDims() == 3) {
                rawFile.writeImage3DTo2D(image, options, ".img");
                writeHeader3DTo2D(image, fhName, fileDir, options);
            } else if (image.getNDims() == 4) {
                rawFile.writeImage4DTo3D(image, options, ".img");
                writeHeader4DTo3D(image, fhName, fileDir, options);
            }

            flipTopBottom(image);
        } else {

            try {
                FileRaw rawFile;
                rawFile = new FileRaw(fileName, fileDir, image.getFileInfo(0), FileBase.READ_WRITE);
                if (image.getType() == ModelStorageBase.BOOLEAN) {
                    rawFile.setMinimumBitsMinus1(7);
                    rawFile.setShiftToDivide(3);
                }
                rawFile.setZeroLengthFlag(true);
                linkProgress(rawFile);

                flipTopBottom(image);
                rawFile.writeImage(image, options);
                nImagesSaved = rawFile.getNImages();
                nTimePeriodsSaved = rawFile.getNTimePeriods();

                if (nImagesSaved != 0) {
                    writeHeader(image, nImagesSaved, nTimePeriodsSaved, fhName, fileDir);
                }

                flipTopBottom(image);
            } catch (IOException error) {
                throw new IOException("FileAnalyzeWrite: " + error);
            } catch (OutOfMemoryError error) {
                throw (error);
            }
        }

    }


    /**
     * Helper method to calculate the offset for getting only the middle analyze image slice from the 3D file.
     *
     * @param   fileInfo  File info.
     *
     * @return  offset
     */
    private int getOffset(FileInfoAnalyze fileInfo) {
        int offset = fileInfo.getExtents()[0] * fileInfo.getExtents()[1] * (fileInfo.getExtents()[2] / 2);

        switch (fileInfo.getDataType()) {

            case ModelStorageBase.BOOLEAN:
            case ModelStorageBase.BYTE:
            case ModelStorageBase.UBYTE:
                break;

            case ModelStorageBase.SHORT:
            case ModelStorageBase.USHORT:
                offset *= 2;
                break;

            case ModelStorageBase.FLOAT:
            case ModelStorageBase.INTEGER:
                offset *= 4;
                break;

            case ModelStorageBase.LONG:
            case ModelStorageBase.DOUBLE:
            case ModelStorageBase.COMPLEX:
                offset *= 8;
                break;

            case ModelStorageBase.ARGB:
                offset *= 3;
                break;

            case ModelStorageBase.ARGB_USHORT:
                offset *= 6;
                break;
        }

        return offset;
    }

    /**
     * Updates the start locations. Each image has a fileinfo where the start locations are stored. Note that the start
     * location for the Z (3rd) dimension change with the change is the slice. The origin is in the upper left corner
     * and we are using the right hand rule. + x -> left to right; + y -> top to bottom and + z -> into screen.
     *
     * @param  fileInfo  DOCUMENT ME!
     */
    private void updateStartLocations(FileInfoBase[] fileInfo) {
        int axisOrient;

        float[] origin = (float[]) (fileInfo[0].getOrigin().clone());
        float[] resolutions = fileInfo[0].getResolutions();

        if (image.getNDims() == 3) {

            for (int i = 0; i < image.getExtents()[2]; i++) {
                fileInfo[i].setOrigin(origin);
                axisOrient = fileInfo[i].getAxisOrientation(2);

                if ((axisOrient == FileInfoBase.ORI_R2L_TYPE) || (axisOrient == FileInfoBase.ORI_P2A_TYPE) ||
                        (axisOrient == FileInfoBase.ORI_I2S_TYPE)) {
                    origin[2] += resolutions[2];
                } else { // ORI_L2R_TYPE, ORI_A2P_TYPE, ORI_S2I_TYPE
                    origin[2] -= resolutions[2];
                }
            }
        } else if (image.getNDims() == 4) {
            float tmp = origin[2];

            for (int i = 0; i < image.getExtents()[3]; i++) {

                for (int j = 0; j < image.getExtents()[2]; j++) {
                    fileInfo[(i * image.getExtents()[2]) + j].setOrigin(origin);
                    axisOrient = fileInfo[i].getAxisOrientation(2);

                    if ((axisOrient == FileInfoBase.ORI_R2L_TYPE) || (axisOrient == FileInfoBase.ORI_P2A_TYPE) ||
                            (axisOrient == FileInfoBase.ORI_I2S_TYPE)) {
                        origin[2] += resolutions[2];
                    } else { // ORI_L2R_TYPE, ORI_A2P_TYPE, ORI_S2I_TYPE
                        origin[2] -= resolutions[2];
                    }
                }

                origin[3] += resolutions[3];
                origin[2] = tmp;
            }
        }
    }

    /**
     * Updates the units of Measure in the file info based on the voxUnits from an Analyze Header.
     *
     * @param  fileInfo  -- an Analyze file Info that has already been read
     * @param  image     -- a ModelImage that the fileInfo needs to be attached to
     */
    private void updateUnitsOfMeasure(FileInfoAnalyze fileInfo, ModelImage image) {

        int[] extents = fileInfo.getExtents();

        // if vox units defines the units of measure, then use that instead
        int units = (Unit.getUnit(fileInfo.getVoxUnits())).getLegacyNum();

        if (image.getNDims() == 2) {

            if (units == Unit.UNKNOWN_MEASURE.getLegacyNum()) { // default to millimeters
                fileInfo.setUnitsOfMeasure(Unit.MILLIMETERS.getLegacyNum(), 0);
                fileInfo.setUnitsOfMeasure(Unit.MILLIMETERS.getLegacyNum(), 1);
            } else {
                fileInfo.setUnitsOfMeasure(units, 0);
                fileInfo.setUnitsOfMeasure(units, 1);
            }

            image.setFileInfo(fileInfo, 0); // Otherwise just set the first fileInfo
        } else if (image.getNDims() == 3) { // If there is more than one image

            if (units == Unit.UNKNOWN_MEASURE.getLegacyNum()) { // default to millimeters
                fileInfo.setUnitsOfMeasure(Unit.MILLIMETERS.getLegacyNum(), 0);
                fileInfo.setUnitsOfMeasure(Unit.MILLIMETERS.getLegacyNum(), 1);
                fileInfo.setUnitsOfMeasure(Unit.MILLIMETERS.getLegacyNum(), 2);
            } else {
                fileInfo.setUnitsOfMeasure(units, 0);
                fileInfo.setUnitsOfMeasure(units, 1);
                fileInfo.setUnitsOfMeasure(units, 2);
            }

            for (int i = 0; i < extents[2]; i++) {
                FileInfoAnalyze newFileInfo = (FileInfoAnalyze) fileInfo.clone();
                newFileInfo.setOrigin(fileInfo.getOriginAtSlice(i));
                image.setFileInfo(newFileInfo, i); // Set the array of fileInfos in ModelImage
            }
        } else if (image.getNDims() == 4) { // If there is more than one image

            if (units == Unit.UNKNOWN_MEASURE.getLegacyNum()) { // default to millimeters and msec.
                fileInfo.setUnitsOfMeasure(Unit.MILLIMETERS.getLegacyNum(), 0);
                fileInfo.setUnitsOfMeasure(Unit.MILLIMETERS.getLegacyNum(), 1);
                fileInfo.setUnitsOfMeasure(Unit.MILLIMETERS.getLegacyNum(), 2);
                fileInfo.setUnitsOfMeasure(Unit.MILLISEC.getLegacyNum(), 3);
            } else {
                fileInfo.setUnitsOfMeasure(units, 0);
                fileInfo.setUnitsOfMeasure(units, 1);
                fileInfo.setUnitsOfMeasure(units, 2);
                fileInfo.setUnitsOfMeasure(units, 3);
            }

            for (int i = 0; i < (extents[2] * extents[3]); i++) {
                FileInfoAnalyze newFileInfo = (FileInfoAnalyze) fileInfo.clone();
                newFileInfo.setOrigin(fileInfo.getOriginAtSlice(i));
                image.setFileInfo(newFileInfo, i); // Set the array of fileInfos in ModelImage
            }
        }

    } // end updateUnitsOfMeasure()

    /**
     * Writes an Analyze header to a separate file.
     *
     * @param      image     Image model of data to write.
     * @param      fileName  File name.
     * @param      fileDir   File directory.
     *
     * @return     Flag to confirm a successful read.
     *
     * @exception  IOException  if there is an error
     *
     * @see        FileInfoAnalyze
     */
    private boolean writeHeader(ModelImage image, int nImagesSaved, int nTimeSaved, String fileName, String fileDir)
            throws IOException {

        boolean simple = false; // A simple write only writes

        // absolutely neccessary information
        boolean endianess;
        String fileHeaderName;
        int nDims;
        int[] extents;
        float[] resolutions;
        FileInfoBase myFileInfo;
        int[] analyzeExtents;
        int imageMax, imageMin;

        myFileInfo = image.getFileInfo(0); // A safeguard in case the file is not Analyze
        endianess = myFileInfo.getEndianess();

        try { // In this case, the file must be Analyze
            fileInfo = (FileInfoAnalyze) image.getFileInfo(0);
        } catch (ClassCastException e) { // If it isn't, catch the exception

            // and make a new fileInfo
            fileInfo = new FileInfoAnalyze(fileName, fileDir, FileUtility.ANALYZE);
            simple = true; // Write the header without all the Analyze info
        }

        fileHeaderName = fileName + ".hdr";
        fileHeader = new File(fileDir + fileHeaderName);
        raFile = new RandomAccessFile(fileHeader, "rw");
        raFile.setLength(0);
        bufferImageHeader = new byte[headerSize];

        // Set certain neccessary information
        fileInfo.setSizeOfHeader(headerSize);
        fileInfo.setDBname(fileName);
        fileInfo.setFileExtents(16384);
        fileInfo.setRegular('r');

        extents = myFileInfo.getExtents();
        nDims = 4; // Analyze always expects 4D !!!!!!!!!.

        // 2D example = 256 x 256 x  1 x 1
        // 3D example = 256 x 256 x 17 x 1

        Preferences.debug("FileAnalyze:writeHeader - nImagesSaved = " + nImagesSaved + "\n", Preferences.DEBUG_FILEIO);
        Preferences.debug("FileAnalyze:writeHeader - nDims = " + nDims + "\n", Preferences.DEBUG_FILEIO);

        analyzeExtents = new int[] { 1, 1, 1, 1, 1 };
        analyzeExtents[0] = 4;

        for (int i = 1; i <= nDims; i++) {

            if (i == 3) {
                analyzeExtents[i] = nImagesSaved;
            } else if (i == 4) {
                analyzeExtents[i] = nTimeSaved;
            } else {
                analyzeExtents[i] = extents[i - 1];
            }
        }

        for (int i = 0; i < analyzeExtents.length; i++) {
            Preferences.debug("FileAnalyze:writeHeader - i = " + i + " dim = " + analyzeExtents[i] + "\n",
            		Preferences.DEBUG_FILEIO);
        }

        switch (image.getType()) {

            case ModelStorageBase.BOOLEAN:
                fileInfo.setDataType(FileInfoAnalyze.DT_BINARY);
                break;

            case ModelStorageBase.BYTE:
                fileInfo.setDataType(FileInfoAnalyze.DT_UNSIGNED_CHAR);
                break;

            case ModelStorageBase.UBYTE:
                fileInfo.setDataType(FileInfoAnalyze.DT_UNSIGNED_CHAR);
                break;

            case ModelStorageBase.SHORT:
                fileInfo.setDataType(FileInfoAnalyze.DT_SIGNED_SHORT);
                break;

            case ModelStorageBase.USHORT:
                fileInfo.setDataType(FileInfoAnalyze.DT_UNSIGNED_SHORT); // MIPAV mode = 6
                break;

            case ModelStorageBase.INTEGER:
                fileInfo.setDataType(FileInfoAnalyze.DT_SIGNED_INT);
                break;

            case ModelStorageBase.LONG:
                return false;

            case ModelStorageBase.FLOAT:
                fileInfo.setDataType(FileInfoAnalyze.DT_FLOAT);
                break;

            case ModelStorageBase.DOUBLE:
                fileInfo.setDataType(FileInfoAnalyze.DT_DOUBLE);
                break;

            case ModelStorageBase.ARGB: // only RGB for Analyze images
                fileInfo.setDataType(FileInfoAnalyze.DT_RGB);
                fileInfo.setBitPix((short) 24);
                break;

            case ModelStorageBase.COMPLEX:
                fileInfo.setDataType(FileInfoAnalyze.DT_COMPLEX);
                break;

            default:
                return false;
        }

        if (!simple) { // Must be an Analyze file, can set all Analyze information based on fileInfo
            setBufferInt(bufferImageHeader, fileInfo.getSizeOfHeader(), 0, endianess);
            setBufferString(bufferImageHeader, fileInfo.getDataTypeName(), 4);
            setBufferString(bufferImageHeader, fileInfo.getDBname(), 14);
            setBufferInt(bufferImageHeader, fileInfo.getFileExtents(), 32, endianess);
            setBufferShort(bufferImageHeader, fileInfo.getSessionErr(), 36, endianess);
            bufferImageHeader[38] = (byte) 'r';
            bufferImageHeader[39] = 0;

            for (int i = 0; i < analyzeExtents.length; i++) {
                setBufferShort(bufferImageHeader, (short) analyzeExtents[i], 40 + (i * 2), endianess);
            }

            // make sure that VoxUnits has been updated to match the unitsOfMeasure
            // in FileInfoBase.  Assume that this is the unit of measure for the x and
            // y dimensions.
            fileInfo.setVoxUnits((Unit.getUnitFromLegacyNum(fileInfo.getUnitsOfMeasure(0))).getAbbrev());
            setBufferString(bufferImageHeader, fileInfo.getVoxUnits(), 56);
            setBufferString(bufferImageHeader, fileInfo.getCalUnits(), 60);

            setBufferShort(bufferImageHeader, fileInfo.getUnused1(), 64, endianess);
            setBufferShort(bufferImageHeader, fileInfo.getUnused2(), 66, endianess);
            setBufferShort(bufferImageHeader, fileInfo.getUnused3(), 68, endianess);

            setBufferShort(bufferImageHeader, fileInfo.getDataTypeCode(), 70, endianess);
            setBufferShort(bufferImageHeader, fileInfo.getBitPix(), 72, endianess);
            setBufferShort(bufferImageHeader, fileInfo.getDim(), 74, endianess);

            // setBufferFloat(bufferImageHeader, nDims+1, 76 + i*4, endianess);

            nDims = image.getNDims();

            for (int i = 0; i < nDims; i++) {
                setBufferFloat(bufferImageHeader, fileInfo.getResolutions()[i], 80 + (i * 4), endianess);
            }

            // vox_offset at location 108 gives the byte offset in the .img file at
            // which the voxels start.  Always start at byte 0.
            setBufferFloat(bufferImageHeader, 0.0f, 108, endianess);

            // MIPAV is hacking the analyze format to use the unused
            // variables to store axis orientation.
            if (zerofunused) {
            	setBufferFloat(bufferImageHeader, 0.0f, 112, endianess);
	            setBufferFloat(bufferImageHeader, 0.0f, 116, endianess);
	            setBufferFloat(bufferImageHeader, 0.0f, 120, endianess);	
            }
            else {
	            setBufferFloat(bufferImageHeader, fileInfo.getOrigin(0), 112, endianess);
	            setBufferFloat(bufferImageHeader, fileInfo.getOrigin(1), 116, endianess);
	            setBufferFloat(bufferImageHeader, fileInfo.getOrigin(2), 120, endianess);
            }

            setBufferFloat(bufferImageHeader, fileInfo.getCalMax(), 124, endianess);
            setBufferFloat(bufferImageHeader, fileInfo.getCalMin(), 128, endianess);
            setBufferFloat(bufferImageHeader, fileInfo.getCompressed(), 132, endianess);
            setBufferFloat(bufferImageHeader, fileInfo.getVerified(), 136, endianess);

            image.calcMinMax();

            if (image.isColorImage()) {
                imageMax = (int) Math.max(image.getMaxR(), Math.max(image.getMaxG(), image.getMaxB()));
                imageMin = (int) Math.min(image.getMinR(), Math.min(image.getMinG(), image.getMinB()));
            } else {
                imageMax = (int) image.getMax();
                imageMin = (int) image.getMin();
            }

            setBufferInt(bufferImageHeader, imageMax, 140, endianess);
            setBufferInt(bufferImageHeader, imageMin, 144, endianess);

            // make sure that description has been updated to match the modality
            // in FileInfoBase. If the modality is unknown, then leave description alone.
            int modality = fileInfo.getModality();

            if (modality != FileInfoBase.UNKNOWN_MODALITY) {
                fileInfo.setDescription(FileInfoBase.getModalityStr(modality));
            }

            setBufferString(bufferImageHeader, fileInfo.getDescription(), 148);
            setBufferString(bufferImageHeader, fileInfo.getAuxFile(), 228);
            bufferImageHeader[252] = fileInfo.getOrientation();
            setBufferString(bufferImageHeader, fileInfo.getOriginator(), 253);
            setBufferString(bufferImageHeader, fileInfo.getGenerated(), 263);
            setBufferString(bufferImageHeader, fileInfo.getScanNum(), 273);
            setBufferString(bufferImageHeader, fileInfo.getPatientID(), 283);
            setBufferString(bufferImageHeader, fileInfo.getExperimentDate(), 293);
            setBufferString(bufferImageHeader, fileInfo.getExperimentTime(), 303);

            setBufferString(bufferImageHeader, fileInfo.getHist(), 313);
            setBufferInt(bufferImageHeader, fileInfo.getViews(), 316, endianess);
            setBufferInt(bufferImageHeader, fileInfo.getVolsAdded(), 320, endianess);
            setBufferInt(bufferImageHeader, fileInfo.getStartField(), 324, endianess);
            setBufferInt(bufferImageHeader, fileInfo.getFieldSkip(), 328, endianess);
            setBufferInt(bufferImageHeader, fileInfo.getOmax(), 332, endianess);
            setBufferInt(bufferImageHeader, fileInfo.getOmin(), 336, endianess);
            setBufferInt(bufferImageHeader, fileInfo.getSmax(), 340, endianess);
            setBufferInt(bufferImageHeader, fileInfo.getSmin(), 344, endianess);
        } else { // Not an Analyze file.  Pad the header with blanks and set all known info
            resolutions = myFileInfo.getResolutions();

            if (resolutions == null) {
                nDims = image.getNDims();
                resolutions = new float[nDims];

                for (int i = 0; i < nDims; i++) {
                    resolutions[i] = 1.0f;
                }
            }

            fileInfo.setResolutions(resolutions);
            fileInfo.setEndianess(endianess);

            setBufferInt(bufferImageHeader, fileInfo.getSizeOfHeader(), 0, endianess);

            fileInfo.setDataType("          "); // 10 Spaces - not sure what really goes here.
            setBufferString(bufferImageHeader, fileInfo.getDataTypeName(), 4);
            setBufferString(bufferImageHeader, fileInfo.getDBname(), 14);
            setBufferInt(bufferImageHeader, fileInfo.getFileExtents(), 32, endianess);
            setBufferShort(bufferImageHeader, (short) 0, 36, endianess);
            bufferImageHeader[38] = (byte) 'r';
            bufferImageHeader[39] = 0;

            for (int i = 0; i < analyzeExtents.length; i++) {
                setBufferShort(bufferImageHeader, (short) analyzeExtents[i], 40 + (i * 2), endianess);
            }

            // set the voxUnits based on the Units of Measure
            int[] units = myFileInfo.getUnitsOfMeasure();
            String voxUnits = (Unit.getUnitFromLegacyNum(units[0])).getAbbrev();
            fileInfo.setUnitsOfMeasure(units);
            fileInfo.setVoxUnits(voxUnits);
            setBufferString(bufferImageHeader, voxUnits, 56);
            setBufferString(bufferImageHeader, "   \n", 60);

            // MIPAV is hacking the analyze format to use the unused
            // variables to store axis orientation.
            setBufferShort(bufferImageHeader, (short) myFileInfo.getAxisOrientation()[0], 64, endianess);
            setBufferShort(bufferImageHeader, (short) myFileInfo.getAxisOrientation()[1], 66, endianess);
            setBufferShort(bufferImageHeader, (short) myFileInfo.getAxisOrientation()[2], 68, endianess);

            Preferences.debug("FileAnalyze:writeHeader(simple): data type = " + fileInfo.getDataTypeCode() + "\n",
            		Preferences.DEBUG_FILEIO);
            setBufferShort(bufferImageHeader, fileInfo.getDataTypeCode(), 70, endianess);

            switch (image.getType()) {

                case ModelStorageBase.BOOLEAN:
                    fileInfo.setBitPix((short) 1);
                    break;

                case ModelStorageBase.BYTE:
                    fileInfo.setBitPix((short) 8);
                    break;

                case ModelStorageBase.UBYTE:
                    fileInfo.setBitPix((short) 8);
                    break;

                case ModelStorageBase.SHORT:
                    fileInfo.setBitPix((short) 16);
                    break;

                case ModelStorageBase.USHORT:
                    fileInfo.setBitPix((short) 16);
                    break;

                case ModelStorageBase.INTEGER:
                    fileInfo.setBitPix((short) 32);
                    break;

                case ModelStorageBase.LONG:
                    return false;

                case ModelStorageBase.FLOAT:
                    fileInfo.setBitPix((short) 32);
                    break;

                case ModelStorageBase.DOUBLE:
                    fileInfo.setBitPix((short) 64);
                    break;

                case ModelStorageBase.ARGB: // only RGB for Analyze images
                    fileInfo.setBitPix((short) 24);
                    break;

                case ModelStorageBase.COMPLEX:
                    fileInfo.setBitPix((short) 64);
                    break;

                default:
                    return false;
            }

            Preferences.debug("FileAnalyze:writeHeader(simple): bits per pixel = " + fileInfo.getBitPix() + "\n", 
            		Preferences.DEBUG_FILEIO);
            setBufferShort(bufferImageHeader, (short) fileInfo.getBitPix(), 72, endianess);
            setBufferShort(bufferImageHeader, (short) 0, 74, endianess);

            nDims = fileInfo.getResolutions().length;

            for (int i = 0; i < nDims; i++) {
                setBufferFloat(bufferImageHeader, fileInfo.getResolutions()[i], 80 + (i * 4), endianess);
            }

            setBufferFloat(bufferImageHeader, (float) 0, 108, endianess);

            // MIPAV is hacking the analyze format to use the funused
            // variables to store start location.
            if (zerofunused) {
            	setBufferFloat(bufferImageHeader, 0.0f, 112, endianess);
	            setBufferFloat(bufferImageHeader, 0.0f, 116, endianess);
	            setBufferFloat(bufferImageHeader, 0.0f, 120, endianess);    	
            }
            else {
	            setBufferFloat(bufferImageHeader, myFileInfo.getOrigin(0), 112, endianess);
	            setBufferFloat(bufferImageHeader, myFileInfo.getOrigin(1), 116, endianess);
	            setBufferFloat(bufferImageHeader, myFileInfo.getOrigin(2), 120, endianess);
            }

            setBufferFloat(bufferImageHeader, (float) 0, 124, endianess);
            setBufferFloat(bufferImageHeader, (float) 0, 128, endianess);
            setBufferFloat(bufferImageHeader, (float) 0, 132, endianess);
            setBufferFloat(bufferImageHeader, (float) 0, 136, endianess);

            image.calcMinMax();

            if (image.isColorImage()) {
                imageMax = (int) Math.max(image.getMaxR(), Math.max(image.getMaxG(), image.getMaxB()));
                imageMin = (int) Math.min(image.getMinR(), Math.min(image.getMinG(), image.getMinB()));
            } else {
                imageMax = (int) image.getMax();
                imageMin = (int) image.getMin();
            }

            setBufferInt(bufferImageHeader, imageMax, 140, endianess);
            setBufferInt(bufferImageHeader, imageMin, 144, endianess);

            int modality = myFileInfo.getModality();
            fileInfo.setModality(modality);
            setBufferString(bufferImageHeader, FileInfoBase.getModalityStr(modality), 148);

            setBufferString(bufferImageHeader, " ", 228);

            byte tmpByte;
            int axis[] = myFileInfo.getAxisOrientation();
            int imageOrientation = myFileInfo.getImageOrientation();
            // Remember y axis gets flipped
            if ((axis[0] == FileInfoBase.ORI_R2L_TYPE) && (axis[1] == FileInfoBase.ORI_P2A_TYPE) &&
                (axis[2] == FileInfoBase.ORI_I2S_TYPE)) {
                tmpByte = 3; // transverse flipped
            }
            else if ((axis[0] == FileInfoBase.ORI_R2L_TYPE) && (axis[1] == FileInfoBase.ORI_I2S_TYPE) &&
                     (axis[2] == FileInfoBase.ORI_P2A_TYPE)) {
                tmpByte = 4; // coronal flipped
            }
            else if ((axis[0] == FileInfoBase.ORI_P2A_TYPE) && (axis[1] == FileInfoBase.ORI_I2S_TYPE) &&
                     (axis[2] == FileInfoBase.ORI_R2L_TYPE)) {
                tmpByte = 5; // sagittal flipped
            }
            else if ((axis[0] == FileInfoBase.ORI_R2L_TYPE) && (axis[1] == FileInfoBase.ORI_A2P_TYPE) &&
                     (axis[2] == FileInfoBase.ORI_I2S_TYPE)) {
                tmpByte = 0; // transverse unflipped
            }
            else if ((axis[0] == FileInfoBase.ORI_R2L_TYPE) && (axis[1] == FileInfoBase.ORI_S2I_TYPE) &&
                     (axis[2] == FileInfoBase.ORI_P2A_TYPE)) {
                tmpByte = 1; // coronal unflipped
            }
            else if ((axis[0] == FileInfoBase.ORI_P2A_TYPE) && (axis[1] == FileInfoBase.ORI_S2I_TYPE) &&
                     (axis[2] == FileInfoBase.ORI_R2L_TYPE)) {
                tmpByte = 2; // sagittal unflipped
            }
            else if (imageOrientation == FileInfoBase.AXIAL) {
                tmpByte = 0; // transverse unflipped
            }
            else if (imageOrientation == FileInfoBase.CORONAL) {
                tmpByte = 1; // coronal unflipped
            }
            else if (imageOrientation == FileInfoBase.SAGITTAL) {
                tmpByte = 2; // sagittal unflipped
            } 
            else {
                tmpByte = 0; // transverse unflipped
            }

            bufferImageHeader[252] = tmpByte;
            setBufferString(bufferImageHeader, " ", 253);
            setBufferString(bufferImageHeader, " ", 263);
            setBufferString(bufferImageHeader, " ", 273);
            setBufferString(bufferImageHeader, " ", 283);
            setBufferString(bufferImageHeader, " ", 293);
            setBufferString(bufferImageHeader, " ", 303);

            setBufferString(bufferImageHeader, " ", 313);
            setBufferInt(bufferImageHeader, 0, 316, endianess);
            setBufferInt(bufferImageHeader, 0, 320, endianess);
            setBufferInt(bufferImageHeader, 0, 324, endianess);
            setBufferInt(bufferImageHeader, 0, 328, endianess);
            setBufferInt(bufferImageHeader, 0, 332, endianess);
            setBufferInt(bufferImageHeader, 0, 336, endianess);
            setBufferInt(bufferImageHeader, 0, 340, endianess);
            setBufferInt(bufferImageHeader, 0, 344, endianess);
        }

        raFile.write(bufferImageHeader);
        raFile.close();

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
    private void writeHeader3DTo2D(ModelImage image, String fileName, String fileDir, FileWriteOptions options)
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

            writeHeader(image, 1, 1, fileName, fileDir);

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
    private void writeHeader4DTo3D(ModelImage image, String fileName, String fileDir, FileWriteOptions options)
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

            writeHeader(image, image.getExtents()[2], 1, fileName, fileDir);

        } // end for loop

    }
}
