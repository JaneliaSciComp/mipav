package gov.nih.mipav.model.file;


import gov.nih.mipav.model.structures.*;

import java.io.*;

import java.util.*;


/**
 * @author  William Gandler
 * @see     FileIO
 */

public class FileBFLOAT extends FileBase {

    //~ Static fields/initializers -------------------------------------------------------------------------------------


    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** DOCUMENT ME! */
    private int bufferSize;

    /** DOCUMENT ME! */
    private boolean endianess;

    /** DOCUMENT ME! */
    private File file;

    /** DOCUMENT ME! */
    private String fileDir;

    /** DOCUMENT ME! */
    private FileInfoBFLOAT fileInfo;
    
    private FileInfoBFLOAT fileInfoCopy;

    /** DOCUMENT ME! */
    private long fileLength;

    /** DOCUMENT ME! */
    private String fileName;

    /** DOCUMENT ME! */
    private boolean foundEOF = false;   

    /** DOCUMENT ME! */
    private ModelImage image;

    /** DOCUMENT ME! */
    private float[] imgBuffer = null;

    /** DOCUMENT ME! */
    private int[] imgExtents;

    /** DOCUMENT ME! */
    private float[] imgResols = new float[5];

    /** DOCUMENT ME! */
    private String originalFileName;

    /** DOCUMENT ME! */
    private int xDim = 0;

    /** DOCUMENT ME! */
    private int yDim = 0;

    /** DOCUMENT ME! */
    private int zDim = 1;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Interfile reader/writer constructor.
     *
     * @param      originalFileName  file name
     * @param      fileDir           file directory
     *
     * @exception  IOException  if there is an error making the file
     */
    public FileBFLOAT(String originalFileName, String fileDir) throws IOException {

        this.originalFileName = originalFileName;
        this.fileDir = fileDir;
    }

    //~ Methods --------------------------------------------------------------------------------------------------------

    

    /**
     * Returns the file info.
     *
     * @return  FileInfoBase containing the file info
     */
    public FileInfoBFLOAT getFileInfo() {
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
     * Prepares this class for cleanup. Calls the <code>finalize</code> method for existing elements, closes any open
     * files and sets other elements to <code>null</code>.
     */
    public void finalize() {
        fileName = null;
        fileDir = null;
        fileInfo = null;
        fileInfoCopy = null;
        file = null;
        image = null;
        imgBuffer = null;
        imgExtents = null;
        imgResols = null;
        originalFileName = null;
        try {
            super.finalize();
        } catch (Throwable er) { }
    }


    /**
     * Reads the image.
     *
     * @param      one  Flag for only reading one image of dataset.
     * @return     returns the image
     *
     * @exception  IOException  if there is an error reading the file
     */
    public ModelImage readImage(boolean one) throws IOException {

        int i;
        boolean countRead;
        int lineCounts;
        String intString;
        StringTokenizer t;
        boolean exceptionOccurred;
        String varString;
        int intVar[] = new int[4];
        int countEntries = 4;
        long dataByteOffset = 0L;
        int index;

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
            }
          
            raFile = new RandomAccessFile(fileHeader, "r");
            fileLength = raFile.length();

            fileInfo = new FileInfoBFLOAT(originalFileName, fileDir, FileUtility.BFLOAT); // dummy fileInfo

            imgResols[0] = 1.0f;
            imgResols[1] = 1.0f;
            imgResols[2] = 1.0f;
            
            countRead = false;
            i = 0;
            lineCounts = 1;

            while ((!countRead) && (lineCounts > 0)) {
                intString = readLine();
                t = new StringTokenizer(intString);
                exceptionOccurred = false;
                lineCounts = 0;

                while ((!exceptionOccurred) && (!countRead)) {

                    try {
                        varString = t.nextToken();
                        intVar[i++] = Integer.parseInt(varString);
                        lineCounts++;

                        if (i == countEntries) {
                            countRead = true;
                        }
                    } catch (NoSuchElementException e) {
                        exceptionOccurred = true;
                    }
                } // while ((!exceptionOcurred) && (!countRead))
            } // while ((!countRead)&& (lineCounts > 0))

            if (!countRead) {
                raFile.close();
                throw new IOException("BFLOAT Read Header Error: For " + fileHeaderName + " read " + i +
                                      " out of " + countEntries + " integers");
            } // if (!countRead)
            raFile.close();

            // Note that yDim comes before xDim
            xDim = intVar[1];
            yDim = intVar[0];
            zDim = intVar[2];
            if (zDim > 1) {
                imgExtents = new int[3];
                imgExtents[2] = zDim;
            } else {
                imgExtents = new int[2];
            }

            imgExtents[0] = xDim;
            imgExtents[1] = yDim;
            fileInfo.setExtents(imgExtents);

            fileInfo.setDataType(ModelStorageBase.FLOAT);

            fileName = originalFileName.substring(0, index) + ".bfloat";
            image = new ModelImage(ModelStorageBase.FLOAT, imgExtents, fileName);

            if (intVar[3] == 0) {
                endianess = FileBase.BIG_ENDIAN;
            }
            else if (intVar[3] == 1) {
                endianess = FileBase.LITTLE_ENDIAN;
            }
            else {
                throw new IOException("BFLOAT Read Header Error: For " + fileHeaderName + " read " + intVar[3] +
                        " for endianess");    
            }

            fileInfo.setEndianess(endianess);

            
            fileInfo.setResolutions(imgResols);

           

            file = new File(fileDir + fileName);
            raFile = new RandomAccessFile(file, "r");
            fileLength = raFile.length();

            bufferSize = xDim * yDim;

            if (one) {

                if (imgExtents.length > 2) {
                    int middle = 4 * (imgExtents[2] / 2);
                    dataByteOffset += middle * bufferSize;
                }

                zDim = 1;
            }

            raFile.seek(dataByteOffset);

            
            imgBuffer = new float[bufferSize];

            for (i = 0; i < zDim; i++) {
                readBuffer(i, imgBuffer);
                image.importData(i * bufferSize, imgBuffer, false);
            }

            raFile.close();
            image.calcMinMax();
            fileInfo.setMin(image.getMin());
            fileInfo.setMax(image.getMax());

            image.setFileInfo(fileInfo, 0);
            for (i = 1; i < zDim; i++) {
                fileInfoCopy = (FileInfoBFLOAT)fileInfo.clone();
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

        byteBuffer = new byte[4 * buffer.length];
        nBytes = 4 * buffer.length;
        raFile.read(byteBuffer, 0, nBytes);
        progress = slice * buffer.length;
        progressLength = buffer.length * zDim;
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

    


}
