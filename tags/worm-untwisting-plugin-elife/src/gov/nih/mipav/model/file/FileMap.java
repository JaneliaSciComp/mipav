package gov.nih.mipav.model.file;


import gov.nih.mipav.model.structures.*;

import java.io.*;


/**
 * Special for Benes.
 *
 * @version  0.1 Sept 2, 1997
 * @author   Matthew J. McAuliffe, Ph.D.
 * @see      FileRaw
 */

public class FileMap extends FileBase {

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** DOCUMENT ME! */
    private File file;


    /** DOCUMENT ME! */
    private FileInfoBase fileInfo;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Creates a new FileMap object.
     *
     * @param   fileName  DOCUMENT ME!
     * @param   fileDir   DOCUMENT ME!
     * @param   fInfo     DOCUMENT ME!
     * @param   rwFlag    DOCUMENT ME!
     *
     * @throws  IOException  DOCUMENT ME!
     */
    public FileMap(String fileName, String fileDir, FileInfoBase fInfo, int rwFlag) throws IOException {

        fileInfo = fInfo;
        file = new File(fileDir + fileName);

        if (rwFlag == READ) {
            raFile = new RandomAccessFile(file, "r");
        } else if (rwFlag == READ_WRITE) {
            raFile = new RandomAccessFile(file, "rw");
        }

    }

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * readImage - This method reads a raw chunk from a file.
     *
     * @param      image   the ModelImage to read the image file into
     * @param      offset  the offset into the image file (from the end of the file)
     *
     * @exception  IOException  if there is an error reading the file
     */
    public void readImage(ModelImage image, int offset) throws IOException {
        int i;
        int index;
        boolean endianess = fileInfo.getEndianess();
        byte[] bufferByte = null;
        short[] bufferShort = null;
        int bufferSize;

        bufferSize = (int) (raFile.length() - offset);

        // bufferSize = image.getSliceSize();
        try {
            bufferShort = new short[bufferSize];
            bufferByte = new byte[2 * bufferSize];
        } catch (OutOfMemoryError error) {
            bufferShort = null;
            bufferByte = null;
            System.gc();
            throw error;
        }

        raFile.seek(offset);

        try {
            raFile.read(bufferByte);

            if (endianess == BIG_ENDIAN) {

                for (i = 0, index = 0; i < bufferSize; i++, index += 2) {
                    bufferShort[i] = (short) (((bufferByte[index] & 0xff) << 8) + (bufferByte[index + 1] & 0xff));
                }
            } else {

                for (i = 0, index = 0; i < bufferSize; i++, index += 2) {
                    bufferShort[i] = (short) (((bufferByte[index + 1] & 0xff) << 8) + (bufferByte[index] & 0xff));
                }
            }
        } catch (IOException error) {
            throw error;
        }

        parseBuffer(bufferShort, image);
    }


    /**
     * parseImage 03 00 or 00 03 is the delimiter.
     *
     * @param  buffer  DOCUMENT ME!
     * @param  image   DOCUMENT ME!
     */
    private void parseBuffer(short[] buffer, ModelImage image) {

        int i, r, c;
        int xDim = image.getExtents()[0];
        short[] rowBuffer = null;

        try {
            rowBuffer = new short[xDim];
        } catch (OutOfMemoryError error) {
            return;
        }

        for (i = 0, c = 0; i < buffer.length; i++, c++) {

            for (r = 0; r < xDim; r++) {
                rowBuffer[r] = buffer[i];
                i++;
            }

            try {
                image.importData(c * xDim, rowBuffer, false);
            } catch (IOException error) {
                return;
            }

            // one value is skipped 00 03 which is the delimiter
        }

        image.calcMinMax();
    }


}
