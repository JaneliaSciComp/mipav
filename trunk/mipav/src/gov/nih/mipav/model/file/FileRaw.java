package gov.nih.mipav.model.file;


import gov.nih.mipav.*;

import gov.nih.mipav.model.structures.*;

import gov.nih.mipav.view.*;

import java.io.*;


/**
 * The class reads and writes raw files of all data types: boolean, byte, short, int, long, float, and double. For the
 * read process an offset can be pass as a parameter to identify the location in the file where the data starts. A
 * number of file formats while not "raw" have data at a specific location after a fixed length but unknown header and
 * therefore can be treated as raw.
 *
 * @version  0.1 Sept 2, 1997
 * @see      FileIO
 * @see      FileInfoXML
 * @see      FileRawChunk
 */

public class FileRaw extends FileBase {

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** DOCUMENT ME! */
    private int compressionType = FileInfoBase.COMPRESSION_NONE;

    /** DOCUMENT ME! */
    private File file = null;

    /** DOCUMENT ME! */
    private FileInfoBase fileInfo;

    /** DOCUMENT ME! */
    private String fileName;

    /** DOCUMENT ME! */
    private FileRawChunk fileRW = null;

    /** DOCUMENT ME! */
    private int nImages;

    /** DOCUMENT ME! */
    private int nTimePeriods;

    /** DOCUMENT ME! */
    private int planarConfig;

    /** DOCUMENT ME! */
    private long startPosition = 0;
    
    private int numColors = 3;
    
    /** Allow reading from 4 color files with RGBA order */
    private boolean RGBAOrder = false;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Constructor for Raw Files that will be used to write from 4D to 3D or from 3D to 2D.
     *
     * @param  fInfo            fileinfo
     */

    public FileRaw(FileInfoBase fInfo) {
        fileInfo = fInfo;

        // check to see if compression handling is to be used
        compressionType = fInfo.getCompressionType();
    }

    /**
     * Raw reader/writer constructor.
     *
     * @param      fileName         Complete file name
     * @param      fInfo            Information that describes the image.
     * @param      rwFlag           Read/write flag.
     *
     * @exception  IOException  if there is an error making the files
     */
    public FileRaw(String fileName, FileInfoBase fInfo, int rwFlag) throws IOException {
        fileInfo = fInfo;

        compressionType = fInfo.getCompressionType();

        // check to see if compression handling is to be used
        if (compressionType == FileInfoBase.COMPRESSION_NONE) {
            file = new File(fileName);

            if (raFile != null) {

                try {
                    raFile.close();
                } catch (IOException ioex) { }
            }

            if (rwFlag == READ) {
                raFile = new RandomAccessFile(file, "r");
            } else if (rwFlag == READ_WRITE) {
                raFile = new RandomAccessFile(file, "rw");
            }
        }

        this.fileName = fileName;

        if (compressionType == FileInfoBase.COMPRESSION_NONE) {
            fileRW = new FileRawChunk(raFile, fileInfo);
        } else {
            fileRW = new FileRawChunk(fileName, fileInfo, rwFlag, compressionType);
        }
    }

    /**
     * Raw reader/writer constructor..
     *
     * @param      fileName         File name (no path)
     * @param      fileDir          File directory (with trailing file separator).
     * @param      fInfo            Information that describes the image.
     * @param      showProgressBar  Boolean indicating if progess bar should be displayed.
     * @param      rwFlag           Read/write flag.
     *
     * @exception  IOException  if there is an error making the files
     */
    public FileRaw(String fileName, String fileDir, FileInfoBase fInfo, int rwFlag)
            throws IOException {
        this(fileDir + fileName, fInfo, rwFlag);
    }
    
    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * Closes random access file associated with this object.
     *
     * @throws  IOException  DOCUMENT ME!
     */
    public void close() throws IOException {

        if (compressionType == FileInfoBase.COMPRESSION_NONE) {

            if (raFile != null) {
                raFile.close();
                raFile = null;
            }
            // System.err.println("closed and nulled FileRaw: raFile");
        }
    }

    /**
     * Prepares this class for cleanup.
     */
    public void finalize() {
        file = null;
        fileName = null;
        fileInfo = null;

        if (fileRW != null) {

            try {
                fileRW.close();
                // System.err.println("closed FileRaw: fileRW (FileRawChunk)");
            } catch (IOException ex) { }

            fileRW.finalize();
        }

        fileRW = null;

        try {
            super.finalize();
        } catch (Throwable er) { }
    }

    /**
     * Accessor that returns the number of image slices saved.
     *
     * @return  The number of images.
     */
    public int getNImages() {
        return nImages;
    }

    /**
     * Accessor that returns the number of time periods saved.
     *
     * @return  The number of time periods.
     */
    public int getNTimePeriods() {
        return nTimePeriods;
    }

    /**
     * This method reads a raw image file (1D - 5D).
     *
     * @param      image   Image model where the data will be stored.
     * @param      offset  Points to where the data of the image is located. It is equal to the header length.
     *
     * @exception  IOException  if there is an error reading the file
     *
     * @see        FileInfoXML
     * @see        FileRawChunk
     */
    public void readImage(ModelImage image, int offset) throws IOException {
        int i, k;
        int ii;
        int[] extents;
        int nBuffers;
        int bufferSize;
        byte[] buffer = null;

        try {
            fireProgressStateChanged(0);

            extents = new int[image.getNDims()];

            for (i = 0; i < image.getNDims(); i++) {
                extents[i] = image.getExtents()[i];
            }

            fileInfo.setExtents(extents);

            if (image.getType() == ModelStorageBase.ARGB) {
                buffer = new byte[extents[0] * extents[1] * 4];
            } else if (image.getType() == ModelStorageBase.ARGB_USHORT) {
                buffer = new byte[extents[0] * extents[1] * 8];
            } else if (image.getType() == ModelStorageBase.ARGB_FLOAT) {
                buffer = new byte[extents[0] * extents[1] * 16];
            }
        } catch (OutOfMemoryError error) {
            throw error;
        }

        fileInfo.setDataType(image.getType());

        if (image.getNDims() > 1) {
            bufferSize = extents[0] * extents[1];
        } else {
            bufferSize = extents[0];
        }

        if (image.getNDims() == 5) {
            nBuffers = extents[4] * extents[3] * extents[2];
        } else if (image.getNDims() == 4) {
            nBuffers = extents[3] * extents[2];
        } else if (image.getNDims() == 3) {
            nBuffers = extents[2];
        } else {
            nBuffers = 1;
        }

        // System.err.println("N BUFFERS: " + nBuffers);

        for (k = 0; k < nBuffers; k++) {

            fireProgressStateChanged(MipavMath.round((float) k / (nBuffers - 1) * 100));

            switch (image.getType()) {

                case ModelStorageBase.BOOLEAN:
                    try {
                        fileRW.readImage(ModelStorageBase.BOOLEAN, (k * 8 * ((bufferSize + 63) >> 6)) + offset,
                                         bufferSize);

                        image.importData(k * bufferSize, fileRW.getBitSetBuffer(), false);
                    } catch (IOException error) {
                        throw error;
                    }

                    break;

                case ModelStorageBase.BYTE:
                    try {
                        fileRW.readImage(ModelStorageBase.BYTE, (k * bufferSize) + offset, bufferSize);

                        image.importData(k * bufferSize, fileRW.getByteBuffer(), false);
                    } catch (IOException error) {
                        throw error;
                    }

                    break;

                case ModelStorageBase.UBYTE:
                    try {
                        fileRW.readImage(ModelStorageBase.UBYTE, (k * bufferSize) + offset, bufferSize);

                        image.importUData(k * bufferSize, fileRW.getShortBuffer(), false);
                    } catch (IOException error) {
                        throw error;
                    }

                    break;

                case ModelStorageBase.SHORT:
                    try {
                        fileRW.readImage(ModelStorageBase.SHORT, (k * bufferSize * 2) + offset, bufferSize);

                        image.importData(k * bufferSize, fileRW.getShortBuffer(), false);
                    } catch (IOException error) {
                        throw error;
                    }

                    break;

                case ModelStorageBase.USHORT:
                    try {
                        fileRW.readImage(ModelStorageBase.USHORT, (k * bufferSize * 2) + offset, bufferSize);

                        image.importUData(k * bufferSize, fileRW.getShortBuffer(), false);
                    } catch (IOException error) {
                        throw error;
                    }

                    break;

                case ModelStorageBase.INTEGER:
                    try {
                        fileRW.readImage(ModelStorageBase.INTEGER, (k * bufferSize * 4) + offset, bufferSize);

                        image.importData(k * bufferSize, fileRW.getIntBuffer(), false);
                    } catch (IOException error) {
                        throw error;
                    }

                    break;

                case ModelStorageBase.UINTEGER:
                    try {
                        fileRW.readImage(ModelStorageBase.UINTEGER, (k * bufferSize * 4) + offset, bufferSize);

                        image.importData(k * bufferSize, fileRW.getIntBuffer(), false);
                    } catch (IOException error) {
                        throw error;
                    }

                    break;

                case ModelStorageBase.LONG:
                    try {
                        fileRW.readImage(ModelStorageBase.LONG, (k * bufferSize * 8) + offset, bufferSize);

                        image.importData(k * bufferSize, fileRW.getLongBuffer(), false);
                    } catch (IOException error) {
                       throw error;
                    }

                    break;

                case ModelStorageBase.FLOAT:
                    try {
                        fileRW.readImage(ModelStorageBase.FLOAT, (k * bufferSize * 4) + offset, bufferSize);

                        image.importData(k * bufferSize, fileRW.getFloatBuffer(), false);
                    } catch (IOException error) {
                       throw error;
                    }

                    break;

                case ModelStorageBase.DOUBLE:
                    try {
                        fileRW.readImage(ModelStorageBase.DOUBLE, (k * bufferSize * 8) + offset, bufferSize);

                        image.importData(k * bufferSize, fileRW.getDoubleBuffer(), false);
                    } catch (IOException error) {
                        throw error;
                    }

                    break;

                case ModelStorageBase.ARGB:
                    try {
                        fileRW.readImage(ModelStorageBase.ARGB, (k * bufferSize * numColors) + offset,
                                                                 bufferSize * numColors);

                        if (numColors == 2) {
                            if (planarConfig == 0) { // RG
                                
                                byte[] tmpBuffer = fileRW.getByteBuffer();
    
                                for (i = 0, ii = 0; i < tmpBuffer.length; i += 2, ii += 4) {
                                    buffer[ii] = 1;
                                    buffer[ii + 1] = tmpBuffer[i];
                                    buffer[ii + 2] = tmpBuffer[i + 1];
                                    buffer[ii + 3] = 0;
                                }
                            } else { // RRRRR GGGGG
    
                                byte[] tmpBuffer = fileRW.getByteBuffer();
                                int bufferOffset = tmpBuffer.length / 2;
    
                                for (i = 0, ii = 0; i < bufferOffset; i++, ii += 4) {
                                    buffer[ii] = 1;
                                    buffer[ii + 1] = tmpBuffer[i];
                                    buffer[ii + 2] = tmpBuffer[i + bufferOffset];
                                    buffer[ii + 3] = 0;
                                }
                            }    
                        } // if (numColors == 2)
                        else if (numColors == 3) {
                            if (planarConfig == 0) { // RGB
    
                                byte[] tmpBuffer = fileRW.getByteBuffer();
    
                                for (i = 0, ii = 0; i < tmpBuffer.length; i += 3, ii += 4) {
                                    buffer[ii] = 1;
                                    buffer[ii + 1] = tmpBuffer[i];
                                    buffer[ii + 2] = tmpBuffer[i + 1];
                                    buffer[ii + 3] = tmpBuffer[i + 2];
                                }
                            } else { // RRRRR GGGGG BBBBB
    
                                byte[] tmpBuffer = fileRW.getByteBuffer();
                                int bufferOffset = tmpBuffer.length / 3;
    
                                for (i = 0, ii = 0; i < bufferOffset; i++, ii += 4) {
                                    buffer[ii] = 1;
                                    buffer[ii + 1] = tmpBuffer[i];
                                    buffer[ii + 2] = tmpBuffer[i + bufferOffset];
                                    buffer[ii + 3] = tmpBuffer[i + (2 * bufferOffset)];
                                }
                            }
                        } // else if (numColors == 3)
                        else { // numColors == 4
                            if (!RGBAOrder) { // ARGB order
                                if (planarConfig == 0) { // ARGB
                                    
                                    byte[] tmpBuffer = fileRW.getByteBuffer();
        
                                    for (i = 0; i < tmpBuffer.length; i ++) {
                                        buffer[i] = tmpBuffer[i];
                                    }
                                } else { // AAAA RRRRR GGGGG BBBBB
        
                                    byte[] tmpBuffer = fileRW.getByteBuffer();
                                    int bufferOffset = tmpBuffer.length / 4;
        
                                    for (i = 0, ii = 0; i < bufferOffset; i++, ii += 4) {
                                        buffer[ii] = tmpBuffer[i];
                                        buffer[ii + 1] = tmpBuffer[i + bufferOffset];
                                        buffer[ii + 2] = tmpBuffer[i + (2 * bufferOffset)];
                                        buffer[ii + 3] = tmpBuffer[i + (3 * bufferOffset)];
                                    }
                                }    
                            }
                            else { // RGBAOrder
                                if (planarConfig == 0) { // RGBA
                                    
                                    byte[] tmpBuffer = fileRW.getByteBuffer();
        
                                    for (i = 0; i < tmpBuffer.length; i += 4) {
                                        buffer[i] = tmpBuffer[i+3];
                                        buffer[i + 1] = tmpBuffer[i];
                                        buffer[i + 2] = tmpBuffer[i + 1];
                                        buffer[i + 3] = tmpBuffer[i + 2];
                                    }
                                } else { // RRRRR GGGGG BBBBB AAAA
        
                                    byte[] tmpBuffer = fileRW.getByteBuffer();
                                    int bufferOffset = tmpBuffer.length / 4;
        
                                    for (i = 0, ii = 0; i < bufferOffset; i++, ii += 4) {
                                        buffer[ii] = tmpBuffer[i + (3* bufferOffset)];
                                        buffer[ii + 1] = tmpBuffer[i];
                                        buffer[ii + 2] = tmpBuffer[i + bufferOffset];
                                        buffer[ii + 3] = tmpBuffer[i + (2 * bufferOffset)];
                                    }
                                }   
                            } // else RGBAOrder
                        } // numColors == 4

                        image.importData(k * buffer.length, buffer, false);
                    } catch (IOException error) {
                        throw error;
                    }

                    break;

                case ModelStorageBase.ARGB_USHORT:
                    try {
                        fileRW.readImage(ModelStorageBase.ARGB_USHORT, (k * bufferSize * 2 * numColors) + offset,
                                                                        bufferSize * numColors);

                        short[] shortBuffer = new short[4 * extents[0] * extents[1]];
                        
                        if (numColors == 2) {
                            if (planarConfig == 0) { // RG
                                
                                short[] tmpBuffer = fileRW.getShortBuffer();
    
                                for (i = 0, ii = 0; i < tmpBuffer.length; i += 2, ii += 4) {
                                    shortBuffer[ii] = (short) 65535;
                                    shortBuffer[ii + 1] = tmpBuffer[i];
                                    shortBuffer[ii + 2] = tmpBuffer[i + 1];
                                    shortBuffer[ii + 3] = 0;
                                }
                            } else { // RRRRR GGGGG
    
                                short[] tmpBuffer = fileRW.getShortBuffer();
                                int bufferOffset = tmpBuffer.length / 2;
    
                                for (i = 0, ii = 0; i < bufferOffset; i++, ii += 4) {
                                    shortBuffer[ii] = (short) 65535;
                                    shortBuffer[ii + 1] = tmpBuffer[i];
                                    shortBuffer[ii + 2] = tmpBuffer[i + bufferOffset];
                                    shortBuffer[ii + 3] = 0;
                                }
                            }    
                        } // if (numColors == 2)
                        else if (numColors == 3) {
                            if (planarConfig == 0) { // RGB
    
                                short[] tmpBuffer = fileRW.getShortBuffer();
    
                                for (i = 0, ii = 0; i < tmpBuffer.length; i += 3, ii += 4) {
                                    shortBuffer[ii] = (short) 65535;
                                    shortBuffer[ii + 1] = tmpBuffer[i];
                                    shortBuffer[ii + 2] = tmpBuffer[i + 1];
                                    shortBuffer[ii + 3] = tmpBuffer[i + 2];
                                }
                            } else { // RRRRR GGGGG BBBBB
    
                                short[] tmpBuffer = fileRW.getShortBuffer();
                                int bufferOffset = tmpBuffer.length / 3;
    
                                for (i = 0, ii = 0; i < bufferOffset; i++, ii += 4) {
                                    shortBuffer[ii] = (short) 65535;
                                    shortBuffer[ii + 1] = tmpBuffer[i];
                                    shortBuffer[ii + 2] = tmpBuffer[i + bufferOffset];
                                    shortBuffer[ii + 3] = tmpBuffer[i + (2 * bufferOffset)];
                                }
                            }
                        } // else if (numColors == 3)
                        else { // numColors == 4
                          if (!RGBAOrder) { // ARGB order
                              if (planarConfig == 0) { // ARGB
                                  
                                  short[] tmpBuffer = fileRW.getShortBuffer();
      
                                  for (i = 0; i < tmpBuffer.length; i++) {
                                      shortBuffer[i] = tmpBuffer[i];
                                  }
                              } else { // AAAA RRRRR GGGGG BBBBB
      
                                  short[] tmpBuffer = fileRW.getShortBuffer();
                                  int bufferOffset = tmpBuffer.length / 4;
      
                                  for (i = 0, ii = 0; i < bufferOffset; i++, ii += 4) {
                                      shortBuffer[ii] = tmpBuffer[i];
                                      shortBuffer[ii + 1] = tmpBuffer[i + bufferOffset];
                                      shortBuffer[ii + 2] = tmpBuffer[i + (2 * bufferOffset)];
                                      shortBuffer[ii + 3] = tmpBuffer[i + (3 * bufferOffset)];
                                  }
                              }    
                          } // if (!RGBAOrder)
                          else { // RGBAOrder 
                              if (planarConfig == 0) { // RGBA
                                  
                                  short[] tmpBuffer = fileRW.getShortBuffer();
      
                                  for (i = 0; i < tmpBuffer.length; i += 4) {
                                      shortBuffer[i] = tmpBuffer[i + 3];
                                      shortBuffer[i + 1] = tmpBuffer[i];
                                      shortBuffer[i + 2] = tmpBuffer[i + 1];
                                      shortBuffer[i + 3] = tmpBuffer[i + 2];
                                  }
                              } else { // RRRRR GGGGG BBBBB AAAAA
      
                                  short[] tmpBuffer = fileRW.getShortBuffer();
                                  int bufferOffset = tmpBuffer.length / 4;
      
                                  for (i = 0, ii = 0; i < bufferOffset; i++, ii += 4) {
                                      shortBuffer[ii] = tmpBuffer[i + (3 * bufferOffset)];
                                      shortBuffer[ii + 1] = tmpBuffer[i];
                                      shortBuffer[ii + 2] = tmpBuffer[i + bufferOffset];
                                      shortBuffer[ii + 3] = tmpBuffer[i + (2 * bufferOffset)];
                                  }
                              }    
                          } // else RGBAOrder
                        } // else numColors == 4

                        image.importUData(k * shortBuffer.length, shortBuffer, false);
                    } catch (IOException error) {
                        throw error;
                    }

                    break;

                case ModelStorageBase.COMPLEX:
                    try {
                        fileRW.readImage(ModelStorageBase.COMPLEX, (k * bufferSize * 8) + offset, bufferSize * 2);

                        float[] realBuffer = new float[bufferSize];
                        float[] imagBuffer = new float[bufferSize];
                        float[] tmpBuffer = fileRW.getFloatBuffer();

                        for (i = 0, ii = 0; i < bufferSize; i++, ii += 2) {
                            realBuffer[i] = tmpBuffer[ii];
                            imagBuffer[i] = tmpBuffer[ii + 1];
                        }

                        image.importComplexData(2 * k * bufferSize, realBuffer, imagBuffer, false, true);
                    } catch (IOException error) {
                        throw error;
                    }

                    break;
                    
                case ModelStorageBase.DCOMPLEX:
                    try {
                        fileRW.readImage(ModelStorageBase.DCOMPLEX, (k * bufferSize * 16) + offset, bufferSize * 2);

                        double[] realBuffer = new double[bufferSize];
                        double[] imagBuffer = new double[bufferSize];
                        double[] tmpBuffer = fileRW.getDoubleBuffer();

                        for (i = 0, ii = 0; i < bufferSize; i++, ii += 2) {
                            realBuffer[i] = tmpBuffer[ii];
                            imagBuffer[i] = tmpBuffer[ii + 1];
                        }

                        image.importDComplexData(2 * k * bufferSize, realBuffer, imagBuffer, false, true);
                    } catch (IOException error) {
                        throw error;
                    }

                    break;

                default:
                    throw new IOException();
            }
        }

        fireProgressStateChanged(ViewJProgressBar.PROGRESS_WINDOW_CLOSING);

        // image.calcMinMax();
        if (compressionType != 1) {
            raFile.close();
        }
    }

    /**
     * This method reads a file and puts the data in the buffer.
     *
     * @param      buffer     float buffer where the data will be stored.
     * @param      offset     points to where the data of the image is located. It is equal to the header length.
     * @param      imageType  ModelImage type (i.e. boolean, byte ...);
     *
     * @exception  IOException  if there is an error reading the file
     *
     * @see        FileInfoXML
     * @see        FileRawChunk
     */
    public void readImage(float[] buffer, int offset, int imageType) throws IOException {
        int i;
        int ii;
        int bufferSize = buffer.length;

        switch (imageType) {

            case ModelStorageBase.BOOLEAN:
                try {
                    fileRW.readImage(ModelStorageBase.BOOLEAN, offset, bufferSize);

                    byte[] tmpBuffer = fileRW.getByteBuffer();

                    for (i = 0; i < bufferSize; i++) {
                        buffer[i] = tmpBuffer[i];
                    }
                } catch (IOException error) {
                    throw error;
                }

                break;

            case ModelStorageBase.BYTE:
                try {
                    fileRW.readImage(ModelStorageBase.BYTE, offset, bufferSize);

                    byte[] tmpBuffer = fileRW.getByteBuffer();

                    for (i = 0; i < bufferSize; i++) {
                        buffer[i] = tmpBuffer[i];
                    }
                } catch (IOException error) {
                    throw error;
                }

                break;

            case ModelStorageBase.UBYTE:
                try {
                    fileRW.readImage(ModelStorageBase.UBYTE, offset, bufferSize);

                    byte[] tmpBuffer = fileRW.getByteBuffer();

                    for (i = 0; i < bufferSize; i++) {

                        // buffer[i] = tmpBuffer[i];
                        buffer[i] = (float) (tmpBuffer[i] & 0xff);
                    }
                } catch (IOException error) {
                    throw error;
                }

                break;

            case ModelStorageBase.SHORT:
                try {
                    fileRW.readImage(ModelStorageBase.SHORT, offset, bufferSize);

                    short[] tmpBuffer = fileRW.getShortBuffer();

                    for (i = 0; i < bufferSize; i++) {
                        buffer[i] = tmpBuffer[i];
                    }
                } catch (IOException error) {
                    throw error;
                }

                break;

            case ModelStorageBase.USHORT:
                try {
                    fileRW.readImage(ModelStorageBase.USHORT, offset, bufferSize);

                    short[] tmpBuffer = fileRW.getShortBuffer();

                    for (i = 0; i < bufferSize; i++) {

                        // buffer[i] = tmpBuffer[i];
                        buffer[i] = (float) (tmpBuffer[i] & 0xffff);
                    }
                } catch (IOException error) {
                    throw error;
                }

                break;

            case ModelStorageBase.INTEGER:
                try {
                    fileRW.readImage(ModelStorageBase.INTEGER, offset, bufferSize);

                    int[] tmpBuffer = fileRW.getIntBuffer();

                    for (i = 0; i < bufferSize; i++) {
                        buffer[i] = tmpBuffer[i];
                    }
                } catch (IOException error) {
                    throw error;
                }

                break;

            case ModelStorageBase.UINTEGER:
                try {
                    fileRW.readImage(ModelStorageBase.UINTEGER, offset, bufferSize);

                    int[] tmpBuffer = fileRW.getIntBuffer();

                    for (i = 0; i < bufferSize; i++) {
                        buffer[i] = (float) (tmpBuffer[i] & 0xffffffffL);
                    }
                } catch (IOException error) {
                    throw error;
                }

                break;

            case ModelStorageBase.LONG:
                try {
                    fileRW.readImage(ModelStorageBase.LONG, offset, bufferSize);

                    long[] tmpBuffer = fileRW.getLongBuffer();

                    for (i = 0; i < bufferSize; i++) {
                        buffer[i] = tmpBuffer[i];
                    }
                } catch (IOException error) {
                    throw error;
                }

                break;

            case ModelStorageBase.FLOAT:
                try {
                    fileRW.readImage(ModelStorageBase.FLOAT, offset, bufferSize);

                    float[] tmpBuffer = fileRW.getFloatBuffer();

                    for (i = 0; i < bufferSize; i++) {
                        buffer[i] = tmpBuffer[i];
                        // Try array copy
                    }

                } catch (IOException error) {
                    throw error;
                }

                break;

            case ModelStorageBase.DOUBLE:
                try {
                    fileRW.readImage(ModelStorageBase.DOUBLE, offset, bufferSize);

                    double[] tmpBuffer = fileRW.getDoubleBuffer();

                    for (i = 0; i < bufferSize; i++) {
                        buffer[i] = (float) tmpBuffer[i];
                    }
                } catch (IOException error) {
                    throw error;
                }

                break;

            case ModelStorageBase.ARGB:
                ii = 0;
                i = 0;

                try {
                    fileRW.readImage(ModelStorageBase.ARGB, offset, bufferSize / 4 * numColors);
                    if (numColors == 2) {
                        if (planarConfig == 0) { // RG
                            
                            byte[] tmpBuffer = fileRW.getByteBuffer();

                            for (i = 0, ii = 0; i < tmpBuffer.length; i += 2, ii += 4) {
                                buffer[ii] = 1;
                                buffer[ii + 1] = tmpBuffer[i];
                                buffer[ii + 2] = tmpBuffer[i + 1];
                                buffer[ii + 3] = 0;
                            }
                        } else { // RRRRR GGGGG

                            byte[] tmpBuffer = fileRW.getByteBuffer();
                            int bufferOffset = tmpBuffer.length / 2;

                            for (i = 0, ii = 0; i < bufferOffset; i++, ii += 4) {
                                buffer[ii] = 1;
                                buffer[ii + 1] = tmpBuffer[i];
                                buffer[ii + 2] = tmpBuffer[i + bufferOffset];
                                buffer[ii + 3] = 0;
                            }
                        }    
                    } // if (numColors == 2)
                    else if (numColors == 3) {
                        if (planarConfig == 0) { // RGB

                            byte[] tmpBuffer = fileRW.getByteBuffer();

                            for (i = 0, ii = 0; i < tmpBuffer.length; i += 3, ii += 4) {
                                buffer[ii] = 1;
                                buffer[ii + 1] = tmpBuffer[i];
                                buffer[ii + 2] = tmpBuffer[i + 1];
                                buffer[ii + 3] = tmpBuffer[i + 2];
                            }
                        } else { // RRRRR GGGGG BBBBB

                            byte[] tmpBuffer = fileRW.getByteBuffer();
                            int bufferOffset = tmpBuffer.length / 3;

                            for (i = 0, ii = 0; i < bufferOffset; i++, ii += 4) {
                                buffer[ii] = 1;
                                buffer[ii + 1] = tmpBuffer[i];
                                buffer[ii + 2] = tmpBuffer[i + bufferOffset];
                                buffer[ii + 3] = tmpBuffer[i + (2 * bufferOffset)];
                            }
                        }
                    } // else if (numColors == 3)
                    else { // numColors == 4
                        if (!RGBAOrder) { // ARGB order
                            if (planarConfig == 0) { // ARGB
                                
                                byte[] tmpBuffer = fileRW.getByteBuffer();
    
                                for (i = 0; i < tmpBuffer.length; i ++) {
                                    buffer[i] = tmpBuffer[i];
                                }
                            } else { // AAAA RRRRR GGGGG BBBBB
    
                                byte[] tmpBuffer = fileRW.getByteBuffer();
                                int bufferOffset = tmpBuffer.length / 4;
    
                                for (i = 0, ii = 0; i < bufferOffset; i++, ii += 4) {
                                    buffer[ii] = tmpBuffer[i];
                                    buffer[ii + 1] = tmpBuffer[i + bufferOffset];
                                    buffer[ii + 2] = tmpBuffer[i + (2 * bufferOffset)];
                                    buffer[ii + 3] = tmpBuffer[i + (3 * bufferOffset)];
                                }
                            }    
                        }
                        else { // RGBAOrder
                            if (planarConfig == 0) { // RGBA
                                
                                byte[] tmpBuffer = fileRW.getByteBuffer();
    
                                for (i = 0; i < tmpBuffer.length; i += 4) {
                                    buffer[i] = tmpBuffer[i+3];
                                    buffer[i + 1] = tmpBuffer[i];
                                    buffer[i + 2] = tmpBuffer[i + 1];
                                    buffer[i + 3] = tmpBuffer[i + 2];
                                }
                            } else { // RRRRR GGGGG BBBBB AAAA
    
                                byte[] tmpBuffer = fileRW.getByteBuffer();
                                int bufferOffset = tmpBuffer.length / 4;
    
                                for (i = 0, ii = 0; i < bufferOffset; i++, ii += 4) {
                                    buffer[ii] = tmpBuffer[i + (3* bufferOffset)];
                                    buffer[ii + 1] = tmpBuffer[i];
                                    buffer[ii + 2] = tmpBuffer[i + bufferOffset];
                                    buffer[ii + 3] = tmpBuffer[i + (2 * bufferOffset)];
                                }
                            }   
                        } // else RGBAOrder
                    } // numColors == 4
                    
                } catch (IOException error) {
                    throw error;
                }

                break;

            case ModelStorageBase.ARGB_USHORT:
                ii = 0;
                i = 0;

                try {
                    fileRW.readImage(ModelStorageBase.ARGB_USHORT, offset, bufferSize / 4 * numColors);
                    
                    if (numColors == 2) {
                        if (planarConfig == 0) { // RG
                            
                            short[] tmpBuffer = fileRW.getShortBuffer();

                            for (i = 0, ii = 0; i < tmpBuffer.length; i += 2, ii += 4) {
                                buffer[ii] = 65535.0f;
                                buffer[ii + 1] = (float) (tmpBuffer[i] & 0xffff);
                                buffer[ii + 2] = (float) (tmpBuffer[i + 1] & 0xffff);
                                buffer[ii + 3] = 0.0f;
                            }
                        } else { // RRRRR GGGGG

                            short[] tmpBuffer = fileRW.getShortBuffer();
                            int bufferOffset = tmpBuffer.length / 2;

                            for (i = 0, ii = 0; i < bufferOffset; i++, ii += 4) {
                                buffer[ii] = 65535.0f;
                                buffer[ii + 1] = (float) (tmpBuffer[i] & 0xffff);
                                buffer[ii + 2] = (float) (tmpBuffer[i + bufferOffset] & 0xffff);
                                buffer[ii + 3] = 0.0f;
                            }
                        }    
                    } // if (numColors == 2)
                    else if (numColors == 3) {
                        if (planarConfig == 0) { // RGB

                            short[] tmpBuffer = fileRW.getShortBuffer();

                            for (i = 0, ii = 0; i < tmpBuffer.length; i += 3, ii += 4) {
                                buffer[ii] = 65535.0f;
                                buffer[ii + 1] = (float) (tmpBuffer[i] & 0xffff);
                                buffer[ii + 2] = (float) (tmpBuffer[i + 1] & 0xffff);
                                buffer[ii + 3] = (float) (tmpBuffer[i + 2] & 0xffff);
                            }
                        } else { // RRRRR GGGGG BBBBB

                            short[] tmpBuffer = fileRW.getShortBuffer();
                            int bufferOffset = tmpBuffer.length / 3;

                            for (i = 0, ii = 0; i < bufferOffset; i++, ii += 4) {
                                buffer[ii] = 65535.0f;
                                buffer[ii + 1] = (float) (tmpBuffer[i] & 0xffff);
                                buffer[ii + 2] = (float) (tmpBuffer[i + bufferOffset] & 0xffff);
                                buffer[ii + 3] = (float) (tmpBuffer[i + (2 * bufferOffset)] & 0xffff);
                            }
                        }
                    } // else if (numColors == 3)
                    else { // numColors == 4
                      if (!RGBAOrder) { // ARGB order
                          if (planarConfig == 0) { // ARGB
                              
                              short[] tmpBuffer = fileRW.getShortBuffer();
  
                              for (i = 0; i < tmpBuffer.length; i++) {
                                  buffer[i] = (float) (tmpBuffer[i] & 0xffff);
                              }
                          } else { // AAAA RRRRR GGGGG BBBBB
  
                              short[] tmpBuffer = fileRW.getShortBuffer();
                              int bufferOffset = tmpBuffer.length / 4;
  
                              for (i = 0, ii = 0; i < bufferOffset; i++, ii += 4) {
                                  buffer[ii] = (float) (tmpBuffer[i] & 0xffff);
                                  buffer[ii + 1] = (float) (tmpBuffer[i + bufferOffset] & 0xffff);
                                  buffer[ii + 2] = (float) (tmpBuffer[i + (2 * bufferOffset)] & 0xffff);
                                  buffer[ii + 3] = (float) (tmpBuffer[i + (3 * bufferOffset)] & 0xffff);
                              }
                          }    
                      } // if (!RGBAOrder)
                      else { // RGBAOrder 
                          if (planarConfig == 0) { // RGBA
                              
                              short[] tmpBuffer = fileRW.getShortBuffer();
  
                              for (i = 0; i < tmpBuffer.length; i += 4) {
                                  buffer[i] = (float) (tmpBuffer[i + 3] & 0xffff);
                                  buffer[i + 1] = (float) (tmpBuffer[i] & 0xffff);
                                  buffer[i + 2] = (float) (tmpBuffer[i + 1] & 0xffff);
                                  buffer[i + 3] = (float) (tmpBuffer[i + 2] & 0xffff);
                              }
                          } else { // RRRRR GGGGG BBBBB AAAAA
  
                              short[] tmpBuffer = fileRW.getShortBuffer();
                              int bufferOffset = tmpBuffer.length / 4;
  
                              for (i = 0, ii = 0; i < bufferOffset; i++, ii += 4) {
                                  buffer[ii] = (float) (tmpBuffer[i + (3 * bufferOffset)] & 0xffff);
                                  buffer[ii + 1] = (float) (tmpBuffer[i] & 0xffff);
                                  buffer[ii + 2] = (float) (tmpBuffer[i + bufferOffset] & 0xffff);
                                  buffer[ii + 3] = (float) (tmpBuffer[i + (2 * bufferOffset)] & 0xffff);
                              }
                          }    
                      } // else RGBAOrder
                    } // else numColors == 4

                } catch (IOException error) {
                    throw error;
                }

                break;

            default:
                throw new IOException();
        }
    }

    /**
     * This method reads a file and puts the data in the buffer. Added here to help speed the reading of DICOM images
     *
     * @param      buffer     float buffer where the data will be stored.
     * @param      offset     points to where the data of the image is located. It is equal to the header length.
     * @param      imageType  ModelImage type (i.e. boolean, byte ...);
     *
     * @exception  IOException  if there is an error reading the file
     *
     * @see        FileInfoXML
     * @see        FileRawChunk
     */
    public void readImage(short[] buffer, int offset, int imageType) throws IOException {
        int i;
        int ii;
        int bufferSize = buffer.length;

        switch (imageType) {

            case ModelStorageBase.BOOLEAN:
                try {
                    fileRW.readImage(ModelStorageBase.BOOLEAN, offset, bufferSize);

                    byte[] tmpBuffer = fileRW.getByteBuffer();

                    for (i = 0; i < bufferSize; i++) {
                        buffer[i] = tmpBuffer[i];
                    }
                } catch (IOException error) {
                    throw error;
                }

                break;

            case ModelStorageBase.BYTE:
                try {
                    fileRW.readImage(ModelStorageBase.BYTE, offset, bufferSize);

                    byte[] tmpBuffer = fileRW.getByteBuffer();

                    for (i = 0; i < bufferSize; i++) {
                        buffer[i] = tmpBuffer[i];
                    }
                } catch (IOException error) {
                    throw error;
                }

                break;

            case ModelStorageBase.UBYTE:
                try {
                    fileRW.readImage(ModelStorageBase.UBYTE, offset, bufferSize);

                    byte[] tmpBuffer = fileRW.getByteBuffer();

                    for (i = 0; i < bufferSize; i++) {

                        // buffer[i] = tmpBuffer[i];
                        buffer[i] = (short) (tmpBuffer[i] & 0xff);
                    }
                } catch (IOException error) {
                    throw error;
                }

                break;

            case ModelStorageBase.SHORT:
                try {
                    fileRW.readImage(ModelStorageBase.SHORT, offset, bufferSize);

                    short[] tmpBuffer = fileRW.getShortBuffer();

                    // for (i = 0; i < bufferSize; i++) {
                    // buffer[i] = tmpBuffer[i];
                    // }
                    // buffer = tmpBuffer;
                    System.arraycopy(tmpBuffer, 0, buffer, 0, tmpBuffer.length);
                } catch (IOException error) {
                    throw error;
                }

                break;

            case ModelStorageBase.USHORT:
                try {
                    fileRW.readImage(ModelStorageBase.USHORT, offset, bufferSize);

                    short[] tmpBuffer = fileRW.getShortBuffer();

                    for (i = 0; i < bufferSize; i++) {

                        // buffer[i] = tmpBuffer[i];
                        buffer[i] = (short) (tmpBuffer[i] & 0xffff);
                    }
                } catch (IOException error) {
                    throw error;
                }

                break;

            case ModelStorageBase.ARGB:
                ii = 0;
                i = 0;

                try {
                    fileRW.readImage(ModelStorageBase.ARGB, offset, bufferSize / 4 * numColors);
                    if (numColors == 2) {
                        if (planarConfig == 0) { // RG
                            
                            byte[] tmpBuffer = fileRW.getByteBuffer();

                            for (i = 0, ii = 0; i < tmpBuffer.length; i += 2, ii += 4) {
                                buffer[ii] = 1;
                                buffer[ii + 1] = tmpBuffer[i];
                                buffer[ii + 2] = tmpBuffer[i + 1];
                                buffer[ii + 3] = 0;
                            }
                        } else { // RRRRR GGGGG

                            byte[] tmpBuffer = fileRW.getByteBuffer();
                            int bufferOffset = tmpBuffer.length / 2;

                            for (i = 0, ii = 0; i < bufferOffset; i++, ii += 4) {
                                buffer[ii] = 1;
                                buffer[ii + 1] = tmpBuffer[i];
                                buffer[ii + 2] = tmpBuffer[i + bufferOffset];
                                buffer[ii + 3] = 0;
                            }
                        }    
                    } // if (numColors == 2)
                    else if (numColors == 3) {
                        if (planarConfig == 0) { // RGB

                            byte[] tmpBuffer = fileRW.getByteBuffer();

                            for (i = 0, ii = 0; i < tmpBuffer.length; i += 3, ii += 4) {
                                buffer[ii] = 1;
                                buffer[ii + 1] = tmpBuffer[i];
                                buffer[ii + 2] = tmpBuffer[i + 1];
                                buffer[ii + 3] = tmpBuffer[i + 2];
                            }
                        } else { // RRRRR GGGGG BBBBB

                            byte[] tmpBuffer = fileRW.getByteBuffer();
                            int bufferOffset = tmpBuffer.length / 3;

                            for (i = 0, ii = 0; i < bufferOffset; i++, ii += 4) {
                                buffer[ii] = 1;
                                buffer[ii + 1] = tmpBuffer[i];
                                buffer[ii + 2] = tmpBuffer[i + bufferOffset];
                                buffer[ii + 3] = tmpBuffer[i + (2 * bufferOffset)];
                            }
                        }
                    } // else if (numColors == 3)
                    else { // numColors == 4
                        if (!RGBAOrder) { // ARGB order
                            if (planarConfig == 0) { // ARGB
                                
                                byte[] tmpBuffer = fileRW.getByteBuffer();
    
                                for (i = 0; i < tmpBuffer.length; i ++) {
                                    buffer[i] = tmpBuffer[i];
                                }
                            } else { // AAAA RRRRR GGGGG BBBBB
    
                                byte[] tmpBuffer = fileRW.getByteBuffer();
                                int bufferOffset = tmpBuffer.length / 4;
    
                                for (i = 0, ii = 0; i < bufferOffset; i++, ii += 4) {
                                    buffer[ii] = tmpBuffer[i];
                                    buffer[ii + 1] = tmpBuffer[i + bufferOffset];
                                    buffer[ii + 2] = tmpBuffer[i + (2 * bufferOffset)];
                                    buffer[ii + 3] = tmpBuffer[i + (3 * bufferOffset)];
                                }
                            }    
                        }
                        else { // RGBAOrder
                            if (planarConfig == 0) { // RGBA
                                
                                byte[] tmpBuffer = fileRW.getByteBuffer();
    
                                for (i = 0; i < tmpBuffer.length; i += 4) {
                                    buffer[i] = tmpBuffer[i+3];
                                    buffer[i + 1] = tmpBuffer[i];
                                    buffer[i + 2] = tmpBuffer[i + 1];
                                    buffer[i + 3] = tmpBuffer[i + 2];
                                }
                            } else { // RRRRR GGGGG BBBBB AAAA
    
                                byte[] tmpBuffer = fileRW.getByteBuffer();
                                int bufferOffset = tmpBuffer.length / 4;
    
                                for (i = 0, ii = 0; i < bufferOffset; i++, ii += 4) {
                                    buffer[ii] = tmpBuffer[i + (3* bufferOffset)];
                                    buffer[ii + 1] = tmpBuffer[i];
                                    buffer[ii + 2] = tmpBuffer[i + bufferOffset];
                                    buffer[ii + 3] = tmpBuffer[i + (2 * bufferOffset)];
                                }
                            }   
                        } // else RGBAOrder
                    } // numColors == 4
                    
                } catch (IOException error) {
                    throw error;
                }

                break;

            case ModelStorageBase.ARGB_USHORT:
                ii = 0;
                i = 0;

          
                try {
                    fileRW.readImage(ModelStorageBase.ARGB_USHORT, offset, bufferSize / 4 * 3);
                    if (numColors == 2) {
                        if (planarConfig == 0) { // RG
                            
                            short[] tmpBuffer = fileRW.getShortBuffer();

                            for (i = 0, ii = 0; i < tmpBuffer.length; i += 2, ii += 4) {
                                buffer[ii] = 32767;
                                buffer[ii + 1] = (short) (tmpBuffer[i] & 0xffff);
                                buffer[ii + 2] = (short) (tmpBuffer[i + 1] & 0xffff);
                                buffer[ii + 3] = 0;
                            }
                        } else { // RRRRR GGGGG

                            short[] tmpBuffer = fileRW.getShortBuffer();
                            int bufferOffset = tmpBuffer.length / 2;

                            for (i = 0, ii = 0; i < bufferOffset; i++, ii += 4) {
                                buffer[ii] = 32767;
                                buffer[ii + 1] = (short) (tmpBuffer[i] & 0xffff);
                                buffer[ii + 2] = (short) (tmpBuffer[i + bufferOffset] & 0xffff);
                                buffer[ii + 3] = 0;
                            }
                        }    
                    } // if (numColors == 2)
                    else if (numColors == 3) {
                        if (planarConfig == 0) { // RGB

                            short[] tmpBuffer = fileRW.getShortBuffer();

                            for (i = 0, ii = 0; i < tmpBuffer.length; i += 3, ii += 4) {
                                buffer[ii] = 32767;
                                buffer[ii + 1] = (short) (tmpBuffer[i] & 0xffff);
                                buffer[ii + 2] = (short) (tmpBuffer[i + 1] & 0xffff);
                                buffer[ii + 3] = (short) (tmpBuffer[i + 2] & 0xffff);
                            }
                        } else { // RRRRR GGGGG BBBBB

                            short[] tmpBuffer = fileRW.getShortBuffer();
                            int bufferOffset = tmpBuffer.length / 3;

                            for (i = 0, ii = 0; i < bufferOffset; i++, ii += 4) {
                                buffer[ii] = 32767;
                                buffer[ii + 1] = (short) (tmpBuffer[i] & 0xffff);
                                buffer[ii + 2] = (short) (tmpBuffer[i + bufferOffset] & 0xffff);
                                buffer[ii + 3] = (short) (tmpBuffer[i + (2 * bufferOffset)] & 0xffff);
                            }
                        }
                    } // else if (numColors == 3)
                    else { // numColors == 4
                      if (!RGBAOrder) { // ARGB order
                          if (planarConfig == 0) { // ARGB
                              
                              short[] tmpBuffer = fileRW.getShortBuffer();
  
                              for (i = 0; i < tmpBuffer.length; i++) {
                                  buffer[i] = (short) (tmpBuffer[i] & 0xffff);
                              }
                          } else { // AAAA RRRRR GGGGG BBBBB
  
                              short[] tmpBuffer = fileRW.getShortBuffer();
                              int bufferOffset = tmpBuffer.length / 4;
  
                              for (i = 0, ii = 0; i < bufferOffset; i++, ii += 4) {
                                  buffer[ii] = (short) (tmpBuffer[i] & 0xffff);
                                  buffer[ii + 1] = (short) (tmpBuffer[i + bufferOffset] & 0xffff);
                                  buffer[ii + 2] = (short) (tmpBuffer[i + (2 * bufferOffset)] & 0xffff);
                                  buffer[ii + 3] = (short) (tmpBuffer[i + (3 * bufferOffset)] & 0xffff);
                              }
                          }    
                      } // if (!RGBAOrder)
                      else { // RGBAOrder 
                          if (planarConfig == 0) { // RGBA
                              
                              short[] tmpBuffer = fileRW.getShortBuffer();
  
                              for (i = 0; i < tmpBuffer.length; i += 4) {
                                  buffer[i] = (short) (tmpBuffer[i + 3] & 0xffff);
                                  buffer[i + 1] = (short) (tmpBuffer[i] & 0xffff);
                                  buffer[i + 2] = (short) (tmpBuffer[i + 1] & 0xffff);
                                  buffer[i + 3] = (short) (tmpBuffer[i + 2] & 0xffff);
                              }
                          } else { // RRRRR GGGGG BBBBB AAAAA
  
                              short[] tmpBuffer = fileRW.getShortBuffer();
                              int bufferOffset = tmpBuffer.length / 4;
  
                              for (i = 0, ii = 0; i < bufferOffset; i++, ii += 4) {
                                  buffer[ii] = (short) (tmpBuffer[i + (3 * bufferOffset)] & 0xffff);
                                  buffer[ii + 1] = (short) (tmpBuffer[i] & 0xffff);
                                  buffer[ii + 2] = (short) (tmpBuffer[i + bufferOffset] & 0xffff);
                                  buffer[ii + 3] = (short) (tmpBuffer[i + (2 * bufferOffset)] & 0xffff);
                              }
                          }    
                      } // else RGBAOrder
                    } // else numColors == 4
                    
                } catch (IOException error) {
                    throw error;
                }

                break;

            default:
                throw new IOException();
        }
    }

    /**
     * Sets the name and directory to new values opens the file.
     *
     * @param      fileName  DOCUMENT ME!
     * @param      fileDir   File directory.
     * @param      fInfo     Information that describes the image.
     * @param      rwFlag    Read/write flag.
     *
     * @exception  IOException  if there is an error making the files
     */
    public void setImageFile(String fileName, String fileDir, FileInfoBase fInfo, int rwFlag) throws IOException {

        try {
            fileInfo = fInfo;
            file = new File(fileDir + fileName);

            if (raFile != null) {

                try {
                    raFile.close();
                } catch (IOException ioex) { }
            }

            if (rwFlag == READ) {
                raFile = new RandomAccessFile(file, "r");
            } else if (rwFlag == READ_WRITE) {
                raFile = new RandomAccessFile(file, "rw");
            }

            this.fileName = fileName;

            fileRW.setImageFile(raFile, fileInfo);
        } catch (OutOfMemoryError error) {
            throw error;
        }
    }

    /**
     * Sets the planar configuration for RGB images.
     *
     * @param  _planarConfig  0 indicates pixels are RGB, RGB chunky<br>
     *                        1 indicates pixels are RRR, GGG, BBB planar
     */
    public void setPlanarConfig(int _planarConfig) {
        planarConfig = _planarConfig;
        fileRW.setPlanarConfig(_planarConfig);
    }
    
    /**
     * Sets the number of colors used in RGB files
     * @param numColors
     */
    public void setNumColors(int numColors) {
        this.numColors = numColors;
        fileRW.setNumColors(numColors);
    }
    
    public void setRGBAOrder(boolean RGBAOrder) {
        this.RGBAOrder = RGBAOrder;
        fileRW.setRGBAOrder(RGBAOrder);
    }

    /**
     * DOCUMENT ME!
     *
     * @param  startPosition  starting byte position for writing data
     */
    public void setStartPosition(long startPosition) {
        this.startPosition = startPosition;
    }

    /**
     * This method writes a raw image file.
     *
     * @param      image    image model where the data is stored.
     * @param      options  DOCUMENT ME!
     *
     * @exception  IOException  if there is an error writing the file
     */
    public void writeImage(ModelImage image, FileWriteOptions options) throws IOException {
        int i, k;
        int[] extents;
        int nBuffers;
        int bufferSize;
        int offset = 0;
        int beginSlice = options.getBeginSlice();
        int endSlice = options.getEndSlice();
        int beginTimePeriod = options.getBeginTime();
        int endTimePeriod = options.getEndTime();

        // System.err.println("Compression type set to: " + compressionType);

        try {
            extents = new int[image.getNDims()];

            for (i = 0; i < image.getNDims(); i++) {
                extents[i] = image.getExtents()[i];
            }

            if (image.getNDims() > 1) {
                bufferSize = extents[0] * extents[1];
            } else {
                bufferSize = extents[0];
            }

            // Right now, because we don't use nBuffers, this code doesn't do
            // anything.  However, it will be needed in the future.
            if (image.getNDims() == 5) {
                nBuffers = extents[4] * extents[3] * extents[2];
                offset = extents[3] * extents[2] * extents[1] * extents[0];
            } else if (image.getNDims() == 4) {
                nBuffers = extents[3] * extents[2];
                offset = extents[2] * extents[1] * extents[0];
            } else if (image.getNDims() == 3) {
                nBuffers = extents[2];
                offset = extents[1] * extents[0];
            } else {
                nBuffers = 1;
            }

            nImages = endSlice - beginSlice + 1; // nImages to be written
            nTimePeriods = endTimePeriod - beginTimePeriod + 1;

            if (compressionType == FileInfoBase.COMPRESSION_NONE) {
                raFile.setLength(0);
                raFile.seek(startPosition);
            }

            fireProgressStateChanged(0);

            for (int t = beginTimePeriod; t <= endTimePeriod; t++) {

                for (k = beginSlice; k <= endSlice; k++) {
                    fireProgressStateChanged(MipavMath.round((float) ((t * (endSlice - beginSlice)) + k) / (nImages) * 100));

                    try {
                        fileRW.writeImage(image, (t * offset) + (k * bufferSize),
                                          (t * offset) + (k * bufferSize) + bufferSize, 0);
                    } catch (IOException error) {

                        if (compressionType == FileInfoBase.COMPRESSION_NONE) {
                            raFile.close();
                        }

                        throw error;
                    }
                }
            }

            fireProgressStateChanged(100);
            
            if (compressionType == FileInfoBase.COMPRESSION_NONE) {
                raFile.close();
            }

            return;
        } catch (OutOfMemoryError error) {

            if (compressionType == FileInfoBase.COMPRESSION_NONE) {
                raFile.close();
            }

            throw error;
        }
    }

    /**
     * Method to save 3D images to an array of 2D images. Images will be name sequentially with the given start # and #
     * of digits
     *
     * @param   image    Image to be saved (broken down into 2D images)
     * @param   options  File Write options
     * @param   suffix   Suffix for file names. Example: ".xml", ".img"
     *
     * @throws  IOException  DOCUMENT ME!
     */
    public void writeImage3DTo2D(ModelImage image, FileWriteOptions options, String suffix) throws IOException {
        String prefix;

        int k, seq;
        int[] extents;
        int bufferSize;
        int offset = 0;
        int beginSlice = options.getBeginSlice();
        int endSlice = options.getEndSlice();

        extents = image.getExtents();
        bufferSize = extents[0] * extents[1];

        nImages = endSlice - beginSlice + 1;
        nTimePeriods = 1;


        String fileName = options.getFileName();
        String fileDir = options.getFileDirectory();

        fireProgressStateChanged(0);

        int index = fileName.lastIndexOf(".");

        if (index != -1) {
            prefix = fileName.substring(0, index);
        } else {
            prefix = fileName;
        }

        String fileString = new String("");
        float prog = 1.0f;

        for (k = beginSlice, seq = options.getStartNumber(); k <= endSlice; k++, seq++, prog++) {

            if (options.getDigitNumber() == 1) {
                fileString = prefix + Integer.toString(seq) + suffix;

            } else if (options.getDigitNumber() == 2) {

                if (seq < 10) {
                    fileString = prefix + "0" + Integer.toString(seq) + suffix;
                } else {
                    fileString = prefix + Integer.toString(seq) + suffix;
                }
            } else if (options.getDigitNumber() == 3) {

                if (seq < 10) {
                    fileString = prefix + "00" + Integer.toString(seq) + suffix;
                } else if (seq < 100) {
                    fileString = prefix + "0" + Integer.toString(seq) + suffix;
                } else {
                    fileString = prefix + Integer.toString(seq) + suffix;
                }
            } else if (options.getDigitNumber() == 4) {

                if (seq < 10) {
                    fileString = prefix + "000" + Integer.toString(seq) + suffix;
                } else if (seq < 100) {
                    fileString = prefix + "00" + Integer.toString(seq) + suffix;
                } else if (seq < 1000) {
                    fileString = prefix + "0" + Integer.toString(seq) + suffix;
                } else {
                    fileString = prefix + Integer.toString(seq) + suffix;
                }
            }

            //fireProgressStateChanged(MipavMath.round((prog / (endSlice - beginSlice + 1)) * 100), "Saving image " + fileString, "Saving image ...");
            fireProgressStateChanged(MipavMath.round((prog / (endSlice - beginSlice + 1)) * 100));
            if (compressionType == FileInfoBase.COMPRESSION_NONE) {
                file = new File(fileDir + fileString);
                raFile = new RandomAccessFile(file, "rw");
                raFile.setLength(0);
                raFile.seek(startPosition);
                fileRW = new FileRawChunk(raFile, image.getFileInfo(0));
            } else {
                fileRW = new FileRawChunk(fileDir + fileString, fileInfo, FileBase.READ_WRITE, compressionType);
            }


            try {
                fileRW.writeImage(image, k * bufferSize, (k * bufferSize) + bufferSize, 0);

                if (compressionType == FileInfoBase.COMPRESSION_NONE) {
                    raFile.close();
                }
            } catch (IOException error) {
                raFile.close();
                throw error;
            }
        } // end for loop
    }

    /**
     * Method to write 4D images to an array of 3D image files. the files will be named sequentially with the given # of
     * digits and given starting #. This method will save ALL slices and the files will be split up by time-period
     *
     * @param   image    Image that is to be saved
     * @param   options  File write options
     * @param   suffix   file suffix for saving name-convention example ".raw", ".img"
     *
     * @throws  IOException  DOCUMENT ME!
     */

    public void writeImage4DTo3D(ModelImage image, FileWriteOptions options, String suffix) throws IOException {

        String prefix;

        int[] extents;
        int volSize = 0;
        int offset = 0;
        int beginSlice = options.getBeginSlice();
        int endSlice = options.getEndSlice();

        int beginTimePeriod = options.getBeginTime();
        int endTimePeriod = options.getEndTime();

        extents = image.getExtents();

        volSize = extents[2] * extents[1] * extents[0];

        this.nTimePeriods = endTimePeriod - beginTimePeriod + 1;

        this.nImages = extents[2];


        String fileName = options.getFileName();
        String fileDir = options.getFileDirectory();

        fireProgressStateChanged(0, "Saving " + fileName, "Saving image ...");

        int index = fileName.lastIndexOf(".");

        if (index != -1) {
            prefix = fileName.substring(0, index);
        } else {
            prefix = fileName;
        }

        String fileString = new String("");
        float prog = 1.0f;

        // name the files sequentially
        for (int t = beginTimePeriod, seq = options.getStartNumber(); t <= endTimePeriod; t++, seq++, prog++) {

            if (options.getDigitNumber() == 1) {
                fileString = prefix + Integer.toString(seq) + suffix;

            } else if (options.getDigitNumber() == 2) {

                if (seq < 10) {
                    fileString = prefix + "0" + Integer.toString(seq) + suffix;
                } else {
                    fileString = prefix + Integer.toString(seq) + suffix;
                }
            } else if (options.getDigitNumber() == 3) {

                if (seq < 10) {
                    fileString = prefix + "00" + Integer.toString(seq) + suffix;
                } else if (seq < 100) {
                    fileString = prefix + "0" + Integer.toString(seq) + suffix;
                } else {
                    fileString = prefix + Integer.toString(seq) + suffix;
                }
            } else if (options.getDigitNumber() == 4) {

                if (seq < 10) {
                    fileString = prefix + "000" + Integer.toString(seq) + suffix;
                } else if (seq < 100) {
                    fileString = prefix + "00" + Integer.toString(seq) + suffix;
                } else if (seq < 1000) {
                    fileString = prefix + "0" + Integer.toString(seq) + suffix;
                } else {
                    fileString = prefix + Integer.toString(seq) + suffix;
                }
            }

            //fireProgressStateChanged(MipavMath.round((prog / (endTimePeriod - beginTimePeriod + 1)) * 100), "Saving image " + fileString, "Saving image ...");
            fireProgressStateChanged(MipavMath.round((prog / (endTimePeriod - beginTimePeriod + 1)) * 100));
            if (compressionType == FileInfoBase.COMPRESSION_NONE) {
                file = new File(fileDir + fileString);
                raFile = new RandomAccessFile(file, "rw");
                raFile.setLength(0);
                raFile.seek(startPosition);
                fileRW = new FileRawChunk(raFile, image.getFileInfo(0));
            } else {
                fileRW = new FileRawChunk(fileDir + fileString, fileInfo, FileBase.READ_WRITE, compressionType);
            }

            // write the given start/end point in the image to a file
            try {
                fileRW.writeImage(image, t * volSize, (t * volSize) + volSize, 0);

                if (compressionType == FileInfoBase.COMPRESSION_NONE) {
                    raFile.close();
                }
            } catch (IOException error) {

                if (compressionType == FileInfoBase.COMPRESSION_NONE) {
                    raFile.close();
                }

                throw error;
            }
        }

    }
}
