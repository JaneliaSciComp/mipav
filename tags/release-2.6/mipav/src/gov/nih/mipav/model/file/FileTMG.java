package gov.nih.mipav.model.file;


import gov.nih.mipav.model.structures.*;

import gov.nih.mipav.view.*;

import java.awt.*;
import java.awt.Dialog.*;

import java.io.*;


/**
 * DOCUMENT ME!
 */
public class FileTMG extends FileBase {

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** DOCUMENT ME! */
    private int dataType;

    /** DOCUMENT ME! */
    private boolean endianess;

    /** DOCUMENT ME! */
    private File file;

    /** DOCUMENT ME! */
    private String fileDir;

    /** DOCUMENT ME! */
    private FileInfoTMG fileInfo;

    /** DOCUMENT ME! */
    private String fileName;

    /** DOCUMENT ME! */
    private ModelImage image;

    /** DOCUMENT ME! */
    private int[] imgExtents = new int[2];

    /** DOCUMENT ME! */
    private ModelLUT LUT = null;

    /** DOCUMENT ME! */
    private ViewJProgressBar progressBar = null;

    /** DOCUMENT ME! */
    private int rangeX, rangeY;

    /** DOCUMENT ME! */
    private byte[] sBuffer = new byte[80];

    /** DOCUMENT ME! */
    private ViewUserInterface UI;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * TMG reader/writer constructor.
     *
     * @param      _UI       user interface reference
     * @param      fileName  file name
     * @param      fileDir   file directory
     *
     * @exception  IOException  if there is an error making the file
     */
    public FileTMG(ViewUserInterface _UI, String fileName, String fileDir) throws IOException {

        UI = _UI;
        this.fileName = fileName;
        this.fileDir = fileDir;
    }

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * returns LUT if defined.
     *
     * @return  the LUT if defined else it is null
     */
    public ModelLUT getModelLUT() {
        return LUT;
    }

    /**
     * reads the TMG file header and data.
     *
     * @exception  IOException  if there is an error reading the file
     *
     * @return     DOCUMENT ME!
     */
    public ModelImage readImage() throws IOException {

        int i;
        float[] imgBuffer;
        int bufferSize;
        float[] imgResols = new float[] { 1.0f, 1.0f, 1.0f, 1.0f, 1.0f };
        String s;
        String titleStr = null;
        String nameStr = null;
        String commentStr = null;
        String timeStr = null;

        try {
            progressBar = new ViewJProgressBar(fileName, "Reading TMG file...", 0, 100, true, null, null);

            progressBar.setLocation((int) Toolkit.getDefaultToolkit().getScreenSize().getWidth() / 2, 50);
            progressBar.setVisible(true);

            file = new File(fileDir + fileName);

            endianess = FileBase.LITTLE_ENDIAN; // false
            fileInfo = new FileInfoTMG(fileName, fileDir, FileBase.MRC);

            raFile = new RandomAccessFile(file, "r");

            endianess = FileBase.LITTLE_ENDIAN;
            fileInfo.setEndianess(endianess);
            dataType = ModelStorageBase.USHORT;
            fileInfo.setDataType(dataType);
            raFile.seek(54L);
            rangeX = getUnsignedShort(endianess); // First dimension range in microns
            raFile.seek(58L);
            rangeY = getUnsignedShort(endianess); // Second dimension range in microns
            raFile.seek(60L);
            imgExtents[0] = getUnsignedShort(endianess);
            raFile.seek(64L);
            imgExtents[1] = getUnsignedShort(endianess);
            fileInfo.setExtents(imgExtents);

            image = new ModelImage(dataType, imgExtents, fileInfo.getFileName(), UI);


            imgResols[0] = (float) rangeX / (float) imgExtents[0];
            imgResols[1] = (float) rangeY / (float) imgExtents[1];
            fileInfo.setResolutions(imgResols);
            fileInfo.setUnitsOfMeasure(FileInfoBase.MICROMETERS, 0);
            fileInfo.setUnitsOfMeasure(FileInfoBase.MICROMETERS, 1);

            raFile.seek(68L);
            titleStr = readStr();
            fileInfo.setTitleStr(titleStr);
            raFile.seek(196L);
            nameStr = readStr();
            fileInfo.setNameStr(nameStr);
            raFile.seek(228L);
            commentStr = readStr();
            fileInfo.setCommentStr(commentStr);
            raFile.seek(368L);
            timeStr = readStr();
            fileInfo.setTimeStr(timeStr);

            // The data starts at 512 bytes
            raFile.seek(512L);
            bufferSize = imgExtents[0] * imgExtents[1];
            imgBuffer = new float[bufferSize];
            image.setFileInfo(fileInfo, 0);
            readBuffer(0, imgBuffer);
            image.importData(0, imgBuffer, true);

            raFile.close();
            progressBar.dispose();

            return image;
        } catch (Exception e) {

            if (image != null) {
                image.disposeLocal();
                image = null;
            }

            System.gc();
            throw new IOException();
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
        int b1, b2;
        byte[] byteBuffer = null;
        int progress, progressLength, mod;
        int imageSlice = 1;

        if (byteBuffer == null) {
            byteBuffer = new byte[2 * buffer.length];
        }

        nBytes = 2 * buffer.length;
        raFile.read(byteBuffer, 0, nBytes);
        progress = slice * buffer.length;
        progressLength = buffer.length * imageSlice;
        mod = progressLength / 10;
        progressBar.setVisible(isProgressBarVisible());

        for (j = 0; j < nBytes; j += 2, i++) {

            if (((i + progress) % mod) == 0) {
                progressBar.updateValueImmed(Math.round((float) (i + progress) / progressLength * 100));
            }

            b1 = getUnsignedByte(byteBuffer, j);
            b2 = getUnsignedByte(byteBuffer, j + 1);

            if (endianess) {
                buffer[i] = ((b1 << 8) + b2);
            } else {
                buffer[i] = ((b2 << 8) + b1);
            }
        }


    }


    /**
     * DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     *
     * @throws  IOException  DOCUMENT ME!
     */
    private String readStr() throws IOException {
        int i;
        boolean readAgain = true;
        String rString = null;

        try {

            for (i = 0; i < 80; i++) {
                sBuffer[i] = 0;
            }

            i = 0;

            while (readAgain) {
                sBuffer[i++] = raFile.readByte();

                if (sBuffer[i - 1] == 0) {
                    readAgain = false;
                }
            }

            rString = new String(sBuffer, 0, i - 1);

            return rString;
        } catch (Exception e) {

            if (image != null) {
                image.disposeLocal();
                image = null;
            }

            System.gc();
            throw new IOException();
        }

    }


}
