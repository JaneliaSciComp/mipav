package gov.nih.mipav.model.file;


import gov.nih.mipav.model.structures.*;

import gov.nih.mipav.util.MipavMath;
import gov.nih.mipav.view.*;

import java.io.*;
import java.util.*;


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

    @SuppressWarnings("unused")
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
    
    /** flag that indicates if raFile should first be set to length of 0 **/
    private boolean zeroLengthFlag = true;
    
    private String dataFileName[] = null;
    
    // Default of 63 and 6 for 8 byte units.  For Analyze FileAnalyze sets to 7 and 3
    // for byte units.
    /** Used in reading and writing boolean */
    private int minimumBitsMinus1 = 63;
    
    /** Used in reading and writing boolean */
    private int shiftToDivide = 6;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Constructor for Raw Files that will be used to write from 4D to 3D or from 3D to 2D.
     *
     * @param  fInfo            fileinfo
     */

    public FileRaw(FileInfoBase fInfo) {
        fileInfo = fInfo;

        // check to see if compression handling is to be used
        // compression performed in FileIO.readImage, FileIO.writeImage, and FileImageXML.readImage.
        // Note that bzip2 cannot decompress into InflaterInputStream in FileRawChunk.
        //compressionType = fInfo.getCompressionType();
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
        // compression performed in FileIO.readImage, FileIO.writeImage, and FileImageXML.readImage.
        // Note that bzip2 cannot decompress into InflaterInputStream in FileRawChunk.
        //compressionType = fInfo.getCompressionType();

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
        if (dataFileName != null) {
            for (int i = 0; i < dataFileName.length; i++) {
                dataFileName[i] = null;
            }
            dataFileName = null;
        }

        try {
            super.finalize();
        } catch (Throwable er) { }
    }
    
    /**
     * Used in reading and writing boolean
     * @param minimumBitsMinus1
     */
    public void setMinimumBitsMinus1(int minimumBitsMinus1) {
        this.minimumBitsMinus1 = minimumBitsMinus1;
        if (fileRW != null) {
            fileRW.setMinimumBitsMinus1(minimumBitsMinus1);
        }
    }
    
    /**
     * Used in reading and writing boolean
     * @param shiftToDivide
     */
    public void setShiftToDivide(int shiftToDivide) {
        this.shiftToDivide = shiftToDivide;
        if (fileRW != null) {
            fileRW.setShiftToDivide(shiftToDivide);
        }
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
     * Accessor that returns the array of data file names
     * @return
     */
    public String[] getDataFileName() {
            return dataFileName;
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
                        if (shiftToDivide == 3) {
                            fileRW.readImage(ModelStorageBase.BOOLEAN,
                                            (k * ((bufferSize + minimumBitsMinus1) >> shiftToDivide)) + offset,
                                             bufferSize);
                        }
                        else if (shiftToDivide == 6) {
                            fileRW.readImage(ModelStorageBase.BOOLEAN,
                                    (8 * k * ((bufferSize + minimumBitsMinus1) >> shiftToDivide)) + offset,
                                     bufferSize);    
                        }

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
                    
                case ModelStorageBase.ARGB_FLOAT:
                    try {
                        fileRW.readImage(ModelStorageBase.ARGB_FLOAT, (k * bufferSize * 4 * numColors) + offset,
                                                                        bufferSize * numColors);

                        float[] floatBuffer = new float[4 * extents[0] * extents[1]];
                        
                        if (numColors == 2) {
                            if (planarConfig == 0) { // RG
                                
                                float[] tmpBuffer = fileRW.getFloatBuffer();
    
                                for (i = 0, ii = 0; i < tmpBuffer.length; i += 2, ii += 4) {
                                    floatBuffer[ii] = 255.0f;
                                    floatBuffer[ii + 1] = tmpBuffer[i];
                                    floatBuffer[ii + 2] = tmpBuffer[i + 1];
                                    floatBuffer[ii + 3] = 0;
                                }
                            } else { // RRRRR GGGGG
    
                                float[] tmpBuffer = fileRW.getFloatBuffer();
                                int bufferOffset = tmpBuffer.length / 2;
    
                                for (i = 0, ii = 0; i < bufferOffset; i++, ii += 4) {
                                    floatBuffer[ii] = 255.0f;
                                    floatBuffer[ii + 1] = tmpBuffer[i];
                                    floatBuffer[ii + 2] = tmpBuffer[i + bufferOffset];
                                    floatBuffer[ii + 3] = 0;
                                }
                            }    
                        } // if (numColors == 2)
                        else if (numColors == 3) {
                            if (planarConfig == 0) { // RGB
    
                                float[] tmpBuffer = fileRW.getFloatBuffer();
    
                                for (i = 0, ii = 0; i < tmpBuffer.length; i += 3, ii += 4) {
                                    floatBuffer[ii] = 255.0f;
                                    floatBuffer[ii + 1] = tmpBuffer[i];
                                    floatBuffer[ii + 2] = tmpBuffer[i + 1];
                                    floatBuffer[ii + 3] = tmpBuffer[i + 2];
                                }
                            } else { // RRRRR GGGGG BBBBB
    
                                float[] tmpBuffer = fileRW.getFloatBuffer();
                                int bufferOffset = tmpBuffer.length / 3;
    
                                for (i = 0, ii = 0; i < bufferOffset; i++, ii += 4) {
                                    floatBuffer[ii] = 255.0f;
                                    floatBuffer[ii + 1] = tmpBuffer[i];
                                    floatBuffer[ii + 2] = tmpBuffer[i + bufferOffset];
                                    floatBuffer[ii + 3] = tmpBuffer[i + (2 * bufferOffset)];
                                }
                            }
                        } // else if (numColors == 3)
                        else { // numColors == 4
                          if (!RGBAOrder) { // ARGB order
                              if (planarConfig == 0) { // ARGB
                                  
                                  float[] tmpBuffer = fileRW.getFloatBuffer();
      
                                  for (i = 0; i < tmpBuffer.length; i++) {
                                      floatBuffer[i] = tmpBuffer[i];
                                  }
                              } else { // AAAA RRRRR GGGGG BBBBB
      
                                  float[] tmpBuffer = fileRW.getFloatBuffer();
                                  int bufferOffset = tmpBuffer.length / 4;
      
                                  for (i = 0, ii = 0; i < bufferOffset; i++, ii += 4) {
                                      floatBuffer[ii] = tmpBuffer[i];
                                      floatBuffer[ii + 1] = tmpBuffer[i + bufferOffset];
                                      floatBuffer[ii + 2] = tmpBuffer[i + (2 * bufferOffset)];
                                      floatBuffer[ii + 3] = tmpBuffer[i + (3 * bufferOffset)];
                                  }
                              }    
                          } // if (!RGBAOrder)
                          else { // RGBAOrder 
                              if (planarConfig == 0) { // RGBA
                                  
                                  float[] tmpBuffer = fileRW.getFloatBuffer();
      
                                  for (i = 0; i < tmpBuffer.length; i += 4) {
                                      floatBuffer[i] = tmpBuffer[i + 3];
                                      floatBuffer[i + 1] = tmpBuffer[i];
                                      floatBuffer[i + 2] = tmpBuffer[i + 1];
                                      floatBuffer[i + 3] = tmpBuffer[i + 2];
                                  }
                              } else { // RRRRR GGGGG BBBBB AAAAA
      
                                  float[] tmpBuffer = fileRW.getFloatBuffer();
                                  int bufferOffset = tmpBuffer.length / 4;
      
                                  for (i = 0, ii = 0; i < bufferOffset; i++, ii += 4) {
                                      floatBuffer[ii] = tmpBuffer[i + (3 * bufferOffset)];
                                      floatBuffer[ii + 1] = tmpBuffer[i];
                                      floatBuffer[ii + 2] = tmpBuffer[i + bufferOffset];
                                      floatBuffer[ii + 3] = tmpBuffer[i + (2 * bufferOffset)];
                                  }
                              }    
                          } // else RGBAOrder
                        } // else numColors == 4

                        image.importData(k * floatBuffer.length, floatBuffer, false);
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

                        image.importComplexData(2 * k * bufferSize, realBuffer, imagBuffer, false, Preferences.is(Preferences.PREF_LOGMAG_DISPLAY));
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

                        image.importDComplexData(2 * k * bufferSize, realBuffer, imagBuffer, false, Preferences.is(Preferences.PREF_LOGMAG_DISPLAY));
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
         * This method reads a raw image file (1D - 5D).  Then multiplies raw data by a scaleFactor[k] 
         * and adds an offsetAdjustment[k], where k is the slice index
         *
         * @param      image   Image model where the data will be stored.  Either a ModelStorageBase.FLOAT
         *                     or a ModelStorageBase.ARGB_FLOAT.
         * @param      originalDataType
         * @param      scaleFactor array with a multiplicative factor for each slice
         * @param      offsetAdjustment array with an additive offset adjustment for each slice
         * @param      offset  Points to where the data of the image is located. It is equal to the header length.
         *
         * @exception  IOException  if there is an error reading the file
         *
         * @see        FileInfoXML
         * @see        FileRawChunk
         */
        public void readFloatImage(ModelImage image, int originalDataType, float scaleFactor[], 
                                   float offsetAdjustment[], int offset) throws IOException {
            int i, k;
            int ii;
            int[] extents;
            int nBuffers;
            int bufferSize;

            try {
                fireProgressStateChanged(0);

                extents = new int[image.getNDims()];

                for (i = 0; i < image.getNDims(); i++) {
                    extents[i] = image.getExtents()[i];
                }

                fileInfo.setExtents(extents);

                
            } catch (OutOfMemoryError error) {
                throw error;
            }

            if (image.getType() == ModelStorageBase.ARGB_FLOAT) {
                fileInfo.setDataType(ModelStorageBase.ARGB_FLOAT);
            }
            else {
                fileInfo.setDataType(ModelStorageBase.FLOAT);
            }

            if (image.getNDims() > 1) {
                bufferSize = extents[0] * extents[1];
            } else {
                bufferSize = extents[0];
            }
            
            float floatBuffer[];
            if (image.getType() == ModelStorageBase.ARGB_FLOAT) {
                floatBuffer = new float[4 * bufferSize];
            }
            else {
                floatBuffer = new float[bufferSize];
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

                switch (originalDataType) {

                    case ModelStorageBase.BOOLEAN:
                        try {
                            fileRW.readImage(ModelStorageBase.BOOLEAN,
                                            (k * 8 * ((bufferSize + minimumBitsMinus1) >> shiftToDivide)) + offset,
                                             bufferSize);
                            for (i = 0; i < bufferSize; i++) {
                                if (fileRW.getBitSetBuffer().get(i)) {
                                    floatBuffer[i] = scaleFactor[k] + offsetAdjustment[k];
                                }
                                else {
                                    floatBuffer[i] = offsetAdjustment[k];
                                }
                            }

                            image.importData(k * bufferSize, floatBuffer, false);
                        } catch (IOException error) {
                            throw error;
                        }

                        break;

                    case ModelStorageBase.BYTE:
                        try {
                            fileRW.readImage(ModelStorageBase.BYTE, (k * bufferSize) + offset, bufferSize);
                            byte buf[] = fileRW.getByteBuffer();
                            for (i = 0; i < bufferSize; i++) {
                                floatBuffer[i] = scaleFactor[k] * buf[i] + offsetAdjustment[k];
                            }

                            image.importData(k * bufferSize, floatBuffer, false);
                        } catch (IOException error) {
                            throw error;
                        }

                        break;

                    case ModelStorageBase.UBYTE:
                        try {
                            fileRW.readImage(ModelStorageBase.UBYTE, (k * bufferSize) + offset, bufferSize);
                            short buf[] = fileRW.getShortBuffer();
                            for (i = 0; i < bufferSize; i++) {
                                floatBuffer[i] = scaleFactor[k] * buf[i] + offsetAdjustment[k];
                            }

                            image.importData(k * bufferSize, floatBuffer, false);
                        } catch (IOException error) {
                            throw error;
                        }

                        break;

                    case ModelStorageBase.SHORT:
                        try {
                            fileRW.readImage(ModelStorageBase.SHORT, (k * bufferSize * 2) + offset, bufferSize);
                            short buf[] = fileRW.getShortBuffer();
                            for (i = 0; i < bufferSize; i++) {
                                floatBuffer[i] = scaleFactor[k] * buf[i] + offsetAdjustment[k];
                            }

                            image.importData(k * bufferSize, floatBuffer, false);
                        } catch (IOException error) {
                            throw error;
                        }

                        break;

                    case ModelStorageBase.USHORT:
                        try {
                            fileRW.readImage(ModelStorageBase.USHORT, (k * bufferSize * 2) + offset, bufferSize);
                            short buf[] = fileRW.getShortBuffer();
                            for (i = 0; i < bufferSize; i++) {
                                floatBuffer[i] = scaleFactor[k] * (buf[i] & 0xffff) + offsetAdjustment[k];
                            }

                            image.importData(k * bufferSize, floatBuffer, false);
                        } catch (IOException error) {
                            throw error;
                        }

                        break;

                    case ModelStorageBase.INTEGER:
                        try {
                            fileRW.readImage(ModelStorageBase.INTEGER, (k * bufferSize * 4) + offset, bufferSize);
                            int buf[] = fileRW.getIntBuffer();
                            for (i = 0; i < bufferSize; i++) {
                                floatBuffer[i] = scaleFactor[k] * buf[i] + offsetAdjustment[k];
                            }

                            image.importData(k * bufferSize, floatBuffer, false);
                        } catch (IOException error) {
                            throw error;
                        }

                        break;

                    case ModelStorageBase.UINTEGER:
                        try {
                            fileRW.readImage(ModelStorageBase.UINTEGER, (k * bufferSize * 4) + offset, bufferSize);
                            int buf[] = fileRW.getIntBuffer();
                            for (i = 0; i < bufferSize; i++) {
                                floatBuffer[i] = scaleFactor[k] * (buf[i] & 0xFFFFFFFFL) + offsetAdjustment[k];
                            }

                            image.importData(k * bufferSize, floatBuffer, false);
                        } catch (IOException error) {
                            throw error;
                        }

                        break;

                    case ModelStorageBase.LONG:
                        try {
                            fileRW.readImage(ModelStorageBase.LONG, (k * bufferSize * 8) + offset, bufferSize);
                            long buf[] = fileRW.getLongBuffer();
                            for (i = 0; i < bufferSize; i++) {
                                floatBuffer[i] = scaleFactor[k] * buf[i] + offsetAdjustment[k];
                            }

                            image.importData(k * bufferSize, floatBuffer, false);
                        } catch (IOException error) {
                           throw error;
                        }

                        break;

                    case ModelStorageBase.FLOAT:
                        try {
                            fileRW.readImage(ModelStorageBase.FLOAT, (k * bufferSize * 4) + offset, bufferSize);
                            float buf[] = fileRW.getFloatBuffer();
                            for (i = 0; i < bufferSize; i++) {
                                buf[i] = scaleFactor[k] * buf[i] + offsetAdjustment[k];
                            }

                            image.importData(k * bufferSize, buf, false);
                        } catch (IOException error) {
                           throw error;
                        }

                        break;

                    case ModelStorageBase.DOUBLE:
                        try {
                            fileRW.readImage(ModelStorageBase.DOUBLE, (k * bufferSize * 8) + offset, bufferSize);
                            double buf[] = fileRW.getDoubleBuffer();
                            for (i = 0; i < bufferSize; i++) {
                                floatBuffer[i] = (float)(scaleFactor[k] * buf[i] + offsetAdjustment[k]);
                            }

                            image.importData(k * bufferSize, floatBuffer, false);
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
                                        floatBuffer[ii] = 1.0f;
                                        floatBuffer[ii + 1] = scaleFactor[k] * (tmpBuffer[i] & 0xff) + offsetAdjustment[k];
                                        floatBuffer[ii + 2] = scaleFactor[k] * (tmpBuffer[i + 1] & 0xff) + offsetAdjustment[k];
                                        floatBuffer[ii + 3] = 0.0f;
                                    }
                                } else { // RRRRR GGGGG
        
                                    byte[] tmpBuffer = fileRW.getByteBuffer();
                                    int bufferOffset = tmpBuffer.length / 2;
        
                                    for (i = 0, ii = 0; i < bufferOffset; i++, ii += 4) {
                                        floatBuffer[ii] = 1.0f;
                                        floatBuffer[ii + 1] = scaleFactor[k] * (tmpBuffer[i] & 0xff) + offsetAdjustment[k];
                                        floatBuffer[ii + 2] = scaleFactor[k] * (tmpBuffer[i + bufferOffset] & 0xff) + offsetAdjustment[k];
                                        floatBuffer[ii + 3] = 0.0f;
                                    }
                                }    
                            } // if (numColors == 2)
                            else if (numColors == 3) {
                                if (planarConfig == 0) { // RGB
        
                                    byte[] tmpBuffer = fileRW.getByteBuffer();
        
                                    for (i = 0, ii = 0; i < tmpBuffer.length; i += 3, ii += 4) {
                                        floatBuffer[ii] = 1.0f;
                                        floatBuffer[ii + 1] = scaleFactor[k] * (tmpBuffer[i] & 0xff) + offsetAdjustment[k];
                                        floatBuffer[ii + 2] = scaleFactor[k] * (tmpBuffer[i + 1] & 0xff) + offsetAdjustment[k];
                                        floatBuffer[ii + 3] = scaleFactor[k] * (tmpBuffer[i + 2] & 0xff) + offsetAdjustment[k];
                                    }
                                } else { // RRRRR GGGGG BBBBB
        
                                    byte[] tmpBuffer = fileRW.getByteBuffer();
                                    int bufferOffset = tmpBuffer.length / 3;
        
                                    for (i = 0, ii = 0; i < bufferOffset; i++, ii += 4) {
                                        floatBuffer[ii] = 1.0f;
                                        floatBuffer[ii + 1] = scaleFactor[k] * (tmpBuffer[i] & 0xff) + offsetAdjustment[k];
                                        floatBuffer[ii + 2] = scaleFactor[k] * (tmpBuffer[i + bufferOffset] & 0xff) + offsetAdjustment[k];
                                        floatBuffer[ii + 3] = scaleFactor[k] * (tmpBuffer[i + (2 * bufferOffset)] & 0xff) +
                                                              offsetAdjustment[k];
                                    }
                                }
                            } // else if (numColors == 3)
                            else { // numColors == 4
                                if (!RGBAOrder) { // ARGB order
                                    if (planarConfig == 0) { // ARGB
                                        
                                        byte[] tmpBuffer = fileRW.getByteBuffer();
            
                                        for (i = 0; i < tmpBuffer.length; i ++) {
                                            floatBuffer[i] = scaleFactor[k] * (tmpBuffer[i] & 0xff) + offsetAdjustment[k];
                                        }
                                    } else { // AAAA RRRRR GGGGG BBBBB
            
                                        byte[] tmpBuffer = fileRW.getByteBuffer();
                                        int bufferOffset = tmpBuffer.length / 4;
            
                                        for (i = 0, ii = 0; i < bufferOffset; i++, ii += 4) {
                                            floatBuffer[ii] = scaleFactor[k] * (tmpBuffer[i] & 0xff) + offsetAdjustment[k];
                                            floatBuffer[ii + 1] = scaleFactor[k] * (tmpBuffer[i + bufferOffset] & 0xff) +
                                                                  offsetAdjustment[k];
                                            floatBuffer[ii + 2] = scaleFactor[k] * (tmpBuffer[i + (2 * bufferOffset)] & 0xff) +
                                                                  offsetAdjustment[k];
                                            floatBuffer[ii + 3] = scaleFactor[k] * (tmpBuffer[i + (3 * bufferOffset)] & 0xff) +
                                                                  offsetAdjustment[k];
                                        }
                                    }    
                                }
                                else { // RGBAOrder
                                    if (planarConfig == 0) { // RGBA
                                        
                                        byte[] tmpBuffer = fileRW.getByteBuffer();
            
                                        for (i = 0; i < tmpBuffer.length; i += 4) {
                                            floatBuffer[i] = scaleFactor[k] * (tmpBuffer[i+3] & 0xff) + offsetAdjustment[k];
                                            floatBuffer[i + 1] = scaleFactor[k] * (tmpBuffer[i] & 0xff) + offsetAdjustment[k];
                                            floatBuffer[i + 2] = scaleFactor[k] * (tmpBuffer[i + 1] & 0xff) + offsetAdjustment[k];
                                            floatBuffer[i + 3] = scaleFactor[k] * (tmpBuffer[i + 2] & 0xff) + offsetAdjustment[k];
                                        }
                                    } else { // RRRRR GGGGG BBBBB AAAA
            
                                        byte[] tmpBuffer = fileRW.getByteBuffer();
                                        int bufferOffset = tmpBuffer.length / 4;
            
                                        for (i = 0, ii = 0; i < bufferOffset; i++, ii += 4) {
                                            floatBuffer[ii] = scaleFactor[k] * (tmpBuffer[i + (3* bufferOffset)] & 0xff) +
                                                              offsetAdjustment[k];
                                            floatBuffer[ii + 1] = scaleFactor[k] * (tmpBuffer[i] & 0xff) + offsetAdjustment[k];
                                            floatBuffer[ii + 2] = scaleFactor[k] * (tmpBuffer[i + bufferOffset] & 0xff) +
                                                             offsetAdjustment[k];
                                            floatBuffer[ii + 3] = scaleFactor[k] * (tmpBuffer[i + (2 * bufferOffset)] & 0xff) +
                                                             offsetAdjustment[k];
                                        }
                                    }   
                                } // else RGBAOrder
                            } // numColors == 4

                            image.importData(k * floatBuffer.length, floatBuffer, false);
                        } catch (IOException error) {
                            throw error;
                        }

                        break;

                    case ModelStorageBase.ARGB_USHORT:
                        try {
                            fileRW.readImage(ModelStorageBase.ARGB_USHORT, (k * bufferSize * 2 * numColors) + offset,
                                                                            bufferSize * numColors);
                            
                            if (numColors == 2) {
                                if (planarConfig == 0) { // RG
                                    
                                    short[] tmpBuffer = fileRW.getShortBuffer();
        
                                    for (i = 0, ii = 0; i < tmpBuffer.length; i += 2, ii += 4) {
                                        floatBuffer[ii] = 65535.0f;
                                        floatBuffer[ii + 1] = scaleFactor[k] * (tmpBuffer[i] & 0xffff) + offsetAdjustment[k];
                                        floatBuffer[ii + 2] = scaleFactor[k] * (tmpBuffer[i + 1] & 0xffff) + offsetAdjustment[k];
                                        floatBuffer[ii + 3] = 0.0f;
                                    }
                                } else { // RRRRR GGGGG
        
                                    short[] tmpBuffer = fileRW.getShortBuffer();
                                    int bufferOffset = tmpBuffer.length / 2;
        
                                    for (i = 0, ii = 0; i < bufferOffset; i++, ii += 4) {
                                        floatBuffer[ii] = 65535.0f;
                                        floatBuffer[ii + 1] = scaleFactor[k] * (tmpBuffer[i] & 0xffff) + offsetAdjustment[k];
                                        floatBuffer[ii + 2] = scaleFactor[k] * (tmpBuffer[i + bufferOffset] & 0xffff) +
                                                              offsetAdjustment[k];
                                        floatBuffer[ii + 3] = 0.0f;
                                    }
                                }    
                            } // if (numColors == 2)
                            else if (numColors == 3) {
                                if (planarConfig == 0) { // RGB
        
                                    short[] tmpBuffer = fileRW.getShortBuffer();
        
                                    for (i = 0, ii = 0; i < tmpBuffer.length; i += 3, ii += 4) {
                                        floatBuffer[ii] = 65535.0f;
                                        floatBuffer[ii + 1] = scaleFactor[k] * (tmpBuffer[i] & 0xffff) + offsetAdjustment[k];
                                        floatBuffer[ii + 2] = scaleFactor[k] * (tmpBuffer[i + 1] & 0xffff) + offsetAdjustment[k];
                                        floatBuffer[ii + 3] = scaleFactor[k] * (tmpBuffer[i + 2] & 0xffff) + offsetAdjustment[k];
                                    }
                                } else { // RRRRR GGGGG BBBBB
        
                                    short[] tmpBuffer = fileRW.getShortBuffer();
                                    int bufferOffset = tmpBuffer.length / 3;
        
                                    for (i = 0, ii = 0; i < bufferOffset; i++, ii += 4) {
                                        floatBuffer[ii] = 65535.0f;
                                        floatBuffer[ii + 1] = scaleFactor[k] * (tmpBuffer[i] & 0xffff) + offsetAdjustment[k];
                                        floatBuffer[ii + 2] = scaleFactor[k] * (tmpBuffer[i + bufferOffset] & 0xffff) +
                                                              offsetAdjustment[k];
                                        floatBuffer[ii + 3] = scaleFactor[k] * (tmpBuffer[i + (2 * bufferOffset)] & 0xffff) +
                                                              offsetAdjustment[k];
                                    }
                                }
                            } // else if (numColors == 3)
                            else { // numColors == 4
                              if (!RGBAOrder) { // ARGB order
                                  if (planarConfig == 0) { // ARGB
                                      
                                      short[] tmpBuffer = fileRW.getShortBuffer();
          
                                      for (i = 0; i < tmpBuffer.length; i++) {
                                          floatBuffer[i] = scaleFactor[k] * (tmpBuffer[i] & 0xffff) + offsetAdjustment[k];
                                      }
                                  } else { // AAAA RRRRR GGGGG BBBBB
          
                                      short[] tmpBuffer = fileRW.getShortBuffer();
                                      int bufferOffset = tmpBuffer.length / 4;
          
                                      for (i = 0, ii = 0; i < bufferOffset; i++, ii += 4) {
                                          floatBuffer[ii] = scaleFactor[k] * (tmpBuffer[i] & 0xffff) + offsetAdjustment[k];
                                          floatBuffer[ii + 1] = scaleFactor[k] * (tmpBuffer[i + bufferOffset] & 0xffff) +
                                                                offsetAdjustment[k];
                                          floatBuffer[ii + 2] = scaleFactor[k] * (tmpBuffer[i + (2 * bufferOffset)] & 0xffff) +
                                                                offsetAdjustment[k];
                                          floatBuffer[ii + 3] = scaleFactor[k] * (tmpBuffer[i + (3 * bufferOffset)] & 0xffff) +
                                                                offsetAdjustment[k];
                                      }
                                  }    
                              } // if (!RGBAOrder)
                              else { // RGBAOrder 
                                  if (planarConfig == 0) { // RGBA
                                      
                                      short[] tmpBuffer = fileRW.getShortBuffer();
          
                                      for (i = 0; i < tmpBuffer.length; i += 4) {
                                          floatBuffer[i] = scaleFactor[k] * (tmpBuffer[i + 3] & 0xffff) + offsetAdjustment[k];
                                          floatBuffer[i + 1] = scaleFactor[k] * (tmpBuffer[i] & 0xffff) + offsetAdjustment[k];
                                          floatBuffer[i + 2] = scaleFactor[k] * (tmpBuffer[i + 1] & 0xffff) + offsetAdjustment[k];
                                          floatBuffer[i + 3] = scaleFactor[k] * (tmpBuffer[i + 2] & 0xffff) + offsetAdjustment[k];
                                      }
                                  } else { // RRRRR GGGGG BBBBB AAAAA
          
                                      short[] tmpBuffer = fileRW.getShortBuffer();
                                      int bufferOffset = tmpBuffer.length / 4;
          
                                      for (i = 0, ii = 0; i < bufferOffset; i++, ii += 4) {
                                          floatBuffer[ii] = scaleFactor[k] * (tmpBuffer[i + (3 * bufferOffset)] & 0xffff) +
                                                            offsetAdjustment[k];
                                          floatBuffer[ii + 1] = scaleFactor[k] * (tmpBuffer[i] & 0xffff) + offsetAdjustment[k];
                                          floatBuffer[ii + 2] = scaleFactor[k] * (tmpBuffer[i + bufferOffset] & 0xffff) +
                                                                offsetAdjustment[k];
                                          floatBuffer[ii + 3] = scaleFactor[k] * (tmpBuffer[i + (2 * bufferOffset)] & 0xffff) +
                                                                offsetAdjustment[k];
                                      }
                                  }    
                              } // else RGBAOrder
                            } // else numColors == 4

                            image.importData(k * floatBuffer.length, floatBuffer, false);
                        } catch (IOException error) {
                            throw error;
                        }

                        break;
                        
                    case ModelStorageBase.ARGB_FLOAT:
                        try {
                            fileRW.readImage(ModelStorageBase.ARGB_FLOAT, (k * bufferSize * 4 * numColors) + offset,
                                                                            bufferSize * numColors);
                            
                            if (numColors == 2) {
                                if (planarConfig == 0) { // RG
                                    
                                    float[] tmpBuffer = fileRW.getFloatBuffer();
        
                                    for (i = 0, ii = 0; i < tmpBuffer.length; i += 2, ii += 4) {
                                        floatBuffer[ii] = 255.0f;
                                        floatBuffer[ii + 1] = scaleFactor[k] * tmpBuffer[i] + offsetAdjustment[k];
                                        floatBuffer[ii + 2] = scaleFactor[k] * tmpBuffer[i + 1] + offsetAdjustment[k];
                                        floatBuffer[ii + 3] = 0.0f;
                                    }
                                } else { // RRRRR GGGGG
        
                                    float[] tmpBuffer = fileRW.getFloatBuffer();
                                    int bufferOffset = tmpBuffer.length / 2;
        
                                    for (i = 0, ii = 0; i < bufferOffset; i++, ii += 4) {
                                        floatBuffer[ii] = 255.0f;
                                        floatBuffer[ii + 1] = scaleFactor[k] * tmpBuffer[i] + offsetAdjustment[k];
                                        floatBuffer[ii + 2] = scaleFactor[k] * tmpBuffer[i + bufferOffset] +
                                                              offsetAdjustment[k];
                                        floatBuffer[ii + 3] = 0.0f;
                                    }
                                }    
                            } // if (numColors == 2)
                            else if (numColors == 3) {
                                if (planarConfig == 0) { // RGB
        
                                    float[] tmpBuffer = fileRW.getFloatBuffer();
        
                                    for (i = 0, ii = 0; i < tmpBuffer.length; i += 3, ii += 4) {
                                        floatBuffer[ii] = 255.0f;
                                        floatBuffer[ii + 1] = scaleFactor[k] * tmpBuffer[i] + offsetAdjustment[k];
                                        floatBuffer[ii + 2] = scaleFactor[k] * tmpBuffer[i + 1] + offsetAdjustment[k];
                                        floatBuffer[ii + 3] = scaleFactor[k] * tmpBuffer[i + 2] + offsetAdjustment[k];
                                    }
                                } else { // RRRRR GGGGG BBBBB
        
                                    float[] tmpBuffer = fileRW.getFloatBuffer();
                                    int bufferOffset = tmpBuffer.length / 3;
        
                                    for (i = 0, ii = 0; i < bufferOffset; i++, ii += 4) {
                                        floatBuffer[ii] = 255.0f;
                                        floatBuffer[ii + 1] = scaleFactor[k] * tmpBuffer[i] + offsetAdjustment[k];
                                        floatBuffer[ii + 2] = scaleFactor[k] * tmpBuffer[i + bufferOffset] +
                                                              offsetAdjustment[k];
                                        floatBuffer[ii + 3] = scaleFactor[k] * tmpBuffer[i + (2 * bufferOffset)] +
                                                              offsetAdjustment[k];
                                    }
                                }
                            } // else if (numColors == 3)
                            else { // numColors == 4
                              if (!RGBAOrder) { // ARGB order
                                  if (planarConfig == 0) { // ARGB
                                      
                                      float[] tmpBuffer = fileRW.getFloatBuffer();
          
                                      for (i = 0; i < tmpBuffer.length; i++) {
                                          floatBuffer[i] = scaleFactor[k] * tmpBuffer[i] + offsetAdjustment[k];
                                      }
                                  } else { // AAAA RRRRR GGGGG BBBBB
          
                                      float[] tmpBuffer = fileRW.getFloatBuffer();
                                      int bufferOffset = tmpBuffer.length / 4;
          
                                      for (i = 0, ii = 0; i < bufferOffset; i++, ii += 4) {
                                          floatBuffer[ii] = scaleFactor[k] * tmpBuffer[i] + offsetAdjustment[k];
                                          floatBuffer[ii + 1] = scaleFactor[k] * tmpBuffer[i + bufferOffset] +
                                                                offsetAdjustment[k];
                                          floatBuffer[ii + 2] = scaleFactor[k] * tmpBuffer[i + (2 * bufferOffset)] +
                                                                offsetAdjustment[k];
                                          floatBuffer[ii + 3] = scaleFactor[k] * tmpBuffer[i + (3 * bufferOffset)] +
                                                                offsetAdjustment[k];
                                      }
                                  }    
                              } // if (!RGBAOrder)
                              else { // RGBAOrder 
                                  if (planarConfig == 0) { // RGBA
                                      
                                      float[] tmpBuffer = fileRW.getFloatBuffer();
          
                                      for (i = 0; i < tmpBuffer.length; i += 4) {
                                          floatBuffer[i] = scaleFactor[k] * tmpBuffer[i + 3] + offsetAdjustment[k];
                                          floatBuffer[i + 1] = scaleFactor[k] * tmpBuffer[i] + offsetAdjustment[k];
                                          floatBuffer[i + 2] = scaleFactor[k] * tmpBuffer[i + 1] + offsetAdjustment[k];
                                          floatBuffer[i + 3] = scaleFactor[k] * tmpBuffer[i + 2] + offsetAdjustment[k];
                                      }
                                  } else { // RRRRR GGGGG BBBBB AAAAA
          
                                      float[] tmpBuffer = fileRW.getFloatBuffer();
                                      int bufferOffset = tmpBuffer.length / 4;
          
                                      for (i = 0, ii = 0; i < bufferOffset; i++, ii += 4) {
                                          floatBuffer[ii] = scaleFactor[k] * tmpBuffer[i + (3 * bufferOffset)] +
                                                            offsetAdjustment[k];
                                          floatBuffer[ii + 1] = scaleFactor[k] * tmpBuffer[i] + offsetAdjustment[k];
                                          floatBuffer[ii + 2] = scaleFactor[k] * tmpBuffer[i + bufferOffset] +
                                                                offsetAdjustment[k];
                                          floatBuffer[ii + 3] = scaleFactor[k] * tmpBuffer[i + (2 * bufferOffset)] +
                                                                offsetAdjustment[k];
                                      }
                                  }    
                              } // else RGBAOrder
                            } // else numColors == 4

                            image.importData(k * floatBuffer.length, floatBuffer, false);
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
                    tmpBuffer = null;
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
                    tmpBuffer = null;
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
                    tmpBuffer = null;
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
                    tmpBuffer = null;
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
                    tmpBuffer = null;
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
                    tmpBuffer = null;
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
                    tmpBuffer = null;
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
                    tmpBuffer = null;
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
                    tmpBuffer = null;
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
                    tmpBuffer = null;
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
                            tmpBuffer = null;
                        } else { // RRRRR GGGGG

                            byte[] tmpBuffer = fileRW.getByteBuffer();
                            int bufferOffset = tmpBuffer.length / 2;

                            for (i = 0, ii = 0; i < bufferOffset; i++, ii += 4) {
                                buffer[ii] = 1;
                                buffer[ii + 1] = tmpBuffer[i];
                                buffer[ii + 2] = tmpBuffer[i + bufferOffset];
                                buffer[ii + 3] = 0;
                            }
                            tmpBuffer = null;
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
                            tmpBuffer = null;
                        } else { // RRRRR GGGGG BBBBB

                            byte[] tmpBuffer = fileRW.getByteBuffer();
                            int bufferOffset = tmpBuffer.length / 3;

                            for (i = 0, ii = 0; i < bufferOffset; i++, ii += 4) {
                                buffer[ii] = 1;
                                buffer[ii + 1] = tmpBuffer[i];
                                buffer[ii + 2] = tmpBuffer[i + bufferOffset];
                                buffer[ii + 3] = tmpBuffer[i + (2 * bufferOffset)];
                            }
                            tmpBuffer = null;
                        }
                    } // else if (numColors == 3)
                    else { // numColors == 4
                        if (!RGBAOrder) { // ARGB order
                            if (planarConfig == 0) { // ARGB
                                
                                byte[] tmpBuffer = fileRW.getByteBuffer();
    
                                for (i = 0; i < tmpBuffer.length; i ++) {
                                    buffer[i] = tmpBuffer[i];
                                }
                                tmpBuffer = null;
                            } else { // AAAA RRRRR GGGGG BBBBB
    
                                byte[] tmpBuffer = fileRW.getByteBuffer();
                                int bufferOffset = tmpBuffer.length / 4;
    
                                for (i = 0, ii = 0; i < bufferOffset; i++, ii += 4) {
                                    buffer[ii] = tmpBuffer[i];
                                    buffer[ii + 1] = tmpBuffer[i + bufferOffset];
                                    buffer[ii + 2] = tmpBuffer[i + (2 * bufferOffset)];
                                    buffer[ii + 3] = tmpBuffer[i + (3 * bufferOffset)];
                                }
                                tmpBuffer = null;
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
                                tmpBuffer = null;
                            } else { // RRRRR GGGGG BBBBB AAAA
    
                                byte[] tmpBuffer = fileRW.getByteBuffer();
                                int bufferOffset = tmpBuffer.length / 4;
    
                                for (i = 0, ii = 0; i < bufferOffset; i++, ii += 4) {
                                    buffer[ii] = tmpBuffer[i + (3* bufferOffset)];
                                    buffer[ii + 1] = tmpBuffer[i];
                                    buffer[ii + 2] = tmpBuffer[i + bufferOffset];
                                    buffer[ii + 3] = tmpBuffer[i + (2 * bufferOffset)];
                                }
                                tmpBuffer = null;
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
                            tmpBuffer = null;
                        } else { // RRRRR GGGGG

                            short[] tmpBuffer = fileRW.getShortBuffer();
                            int bufferOffset = tmpBuffer.length / 2;

                            for (i = 0, ii = 0; i < bufferOffset; i++, ii += 4) {
                                buffer[ii] = 65535.0f;
                                buffer[ii + 1] = (float) (tmpBuffer[i] & 0xffff);
                                buffer[ii + 2] = (float) (tmpBuffer[i + bufferOffset] & 0xffff);
                                buffer[ii + 3] = 0.0f;
                            }
                            tmpBuffer = null;
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
                            tmpBuffer = null;
                        } else { // RRRRR GGGGG BBBBB

                            short[] tmpBuffer = fileRW.getShortBuffer();
                            int bufferOffset = tmpBuffer.length / 3;

                            for (i = 0, ii = 0; i < bufferOffset; i++, ii += 4) {
                                buffer[ii] = 65535.0f;
                                buffer[ii + 1] = (float) (tmpBuffer[i] & 0xffff);
                                buffer[ii + 2] = (float) (tmpBuffer[i + bufferOffset] & 0xffff);
                                buffer[ii + 3] = (float) (tmpBuffer[i + (2 * bufferOffset)] & 0xffff);
                            }
                            tmpBuffer = null;
                        }
                    } // else if (numColors == 3)
                    else { // numColors == 4
                      if (!RGBAOrder) { // ARGB order
                          if (planarConfig == 0) { // ARGB
                              
                              short[] tmpBuffer = fileRW.getShortBuffer();
  
                              for (i = 0; i < tmpBuffer.length; i++) {
                                  buffer[i] = (float) (tmpBuffer[i] & 0xffff);
                              }
                              tmpBuffer = null;
                          } else { // AAAA RRRRR GGGGG BBBBB
  
                              short[] tmpBuffer = fileRW.getShortBuffer();
                              int bufferOffset = tmpBuffer.length / 4;
  
                              for (i = 0, ii = 0; i < bufferOffset; i++, ii += 4) {
                                  buffer[ii] = (float) (tmpBuffer[i] & 0xffff);
                                  buffer[ii + 1] = (float) (tmpBuffer[i + bufferOffset] & 0xffff);
                                  buffer[ii + 2] = (float) (tmpBuffer[i + (2 * bufferOffset)] & 0xffff);
                                  buffer[ii + 3] = (float) (tmpBuffer[i + (3 * bufferOffset)] & 0xffff);
                              }
                              tmpBuffer = null;
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
                              tmpBuffer = null;
                          } else { // RRRRR GGGGG BBBBB AAAAA
  
                              short[] tmpBuffer = fileRW.getShortBuffer();
                              int bufferOffset = tmpBuffer.length / 4;
  
                              for (i = 0, ii = 0; i < bufferOffset; i++, ii += 4) {
                                  buffer[ii] = (float) (tmpBuffer[i + (3 * bufferOffset)] & 0xffff);
                                  buffer[ii + 1] = (float) (tmpBuffer[i] & 0xffff);
                                  buffer[ii + 2] = (float) (tmpBuffer[i + bufferOffset] & 0xffff);
                                  buffer[ii + 3] = (float) (tmpBuffer[i + (2 * bufferOffset)] & 0xffff);
                              }
                              tmpBuffer = null;
                          }    
                      } // else RGBAOrder
                    } // else numColors == 4

                } catch (IOException error) {
                    throw error;
                }

                break;
                
            case ModelStorageBase.ARGB_FLOAT:
                ii = 0;
                i = 0;

                try {
                    fileRW.readImage(ModelStorageBase.ARGB_FLOAT, offset, bufferSize / 4 * numColors);
                    
                    if (numColors == 2) {
                        if (planarConfig == 0) { // RG
                            
                            float[] tmpBuffer = fileRW.getFloatBuffer();

                            for (i = 0, ii = 0; i < tmpBuffer.length; i += 2, ii += 4) {
                                buffer[ii] = 255.0f;
                                buffer[ii + 1] = tmpBuffer[i];
                                buffer[ii + 2] = tmpBuffer[i + 1];
                                buffer[ii + 3] = 0.0f;
                            }
                            tmpBuffer = null;
                        } else { // RRRRR GGGGG

                            float[] tmpBuffer = fileRW.getFloatBuffer();
                            int bufferOffset = tmpBuffer.length / 2;

                            for (i = 0, ii = 0; i < bufferOffset; i++, ii += 4) {
                                buffer[ii] = 255.0f;
                                buffer[ii + 1] = tmpBuffer[i];
                                buffer[ii + 2] = tmpBuffer[i + bufferOffset];
                                buffer[ii + 3] = 0.0f;
                            }
                            tmpBuffer = null;
                        }    
                    } // if (numColors == 2)
                    else if (numColors == 3) {
                        if (planarConfig == 0) { // RGB

                            float[] tmpBuffer = fileRW.getFloatBuffer();

                            for (i = 0, ii = 0; i < tmpBuffer.length; i += 3, ii += 4) {
                                buffer[ii] = 255.0f;
                                buffer[ii + 1] = tmpBuffer[i];
                                buffer[ii + 2] = tmpBuffer[i + 1];
                                buffer[ii + 3] = tmpBuffer[i + 2];
                            }
                            tmpBuffer = null;
                        } else { // RRRRR GGGGG BBBBB

                            float[] tmpBuffer = fileRW.getFloatBuffer();
                            int bufferOffset = tmpBuffer.length / 3;

                            for (i = 0, ii = 0; i < bufferOffset; i++, ii += 4) {
                                buffer[ii] = 255.0f;
                                buffer[ii + 1] = tmpBuffer[i];
                                buffer[ii + 2] = tmpBuffer[i + bufferOffset];
                                buffer[ii + 3] = tmpBuffer[i + (2 * bufferOffset)];
                            }
                            tmpBuffer = null;
                        }
                    } // else if (numColors == 3)
                    else { // numColors == 4
                      if (!RGBAOrder) { // ARGB order
                          if (planarConfig == 0) { // ARGB
                              
                              float[] tmpBuffer = fileRW.getFloatBuffer();
  
                              for (i = 0; i < tmpBuffer.length; i++) {
                                  buffer[i] = tmpBuffer[i];
                              }
                              tmpBuffer = null;
                          } else { // AAAA RRRRR GGGGG BBBBB
  
                              float[] tmpBuffer = fileRW.getFloatBuffer();
                              int bufferOffset = tmpBuffer.length / 4;
  
                              for (i = 0, ii = 0; i < bufferOffset; i++, ii += 4) {
                                  buffer[ii] = tmpBuffer[i];
                                  buffer[ii + 1] = tmpBuffer[i + bufferOffset];
                                  buffer[ii + 2] = tmpBuffer[i + (2 * bufferOffset)];
                                  buffer[ii + 3] = tmpBuffer[i + (3 * bufferOffset)];
                              }
                              tmpBuffer = null;
                          }    
                      } // if (!RGBAOrder)
                      else { // RGBAOrder 
                          if (planarConfig == 0) { // RGBA
                              
                              float[] tmpBuffer = fileRW.getFloatBuffer();
  
                              for (i = 0; i < tmpBuffer.length; i += 4) {
                                  buffer[i] = tmpBuffer[i + 3];
                                  buffer[i + 1] = tmpBuffer[i];
                                  buffer[i + 2] = tmpBuffer[i + 1];
                                  buffer[i + 3] = tmpBuffer[i + 2];
                              }
                              tmpBuffer = null;
                          } else { // RRRRR GGGGG BBBBB AAAAA
  
                              float[] tmpBuffer = fileRW.getFloatBuffer();
                              int bufferOffset = tmpBuffer.length / 4;
  
                              for (i = 0, ii = 0; i < bufferOffset; i++, ii += 4) {
                                  buffer[ii] = tmpBuffer[i + (3 * bufferOffset)];
                                  buffer[ii + 1] = tmpBuffer[i];
                                  buffer[ii + 2] = tmpBuffer[i + bufferOffset];
                                  buffer[ii + 3] = tmpBuffer[i + (2 * bufferOffset)];
                              }
                              tmpBuffer = null;
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
     * This method reads a file and puts the data in the buffer. 
     *
     * @param      buffer     double buffer where the data will be stored.
     * @param      offset     points to where the data of the image is located. It is equal to the header length.
     *
     * @exception  IOException  if there is an error reading the file
     *
     * @see        FileInfoXML
     * @see        FileRawChunk
     */
    public void readImage(double[] buffer, int offset) throws IOException {
    	int bufferSize = buffer.length;
    	try {
            fileRW.readImage(ModelStorageBase.DOUBLE, offset, bufferSize);

            double[] tmpBuffer = fileRW.getDoubleBuffer();


            System.arraycopy(tmpBuffer, 0, buffer, 0, tmpBuffer.length);
        } catch (IOException error) {
            throw error;
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

                    BitSet bufferBitSet = fileRW.getBitSetBuffer();
 
                    for (i = 0; i < bufferSize; i++) {
                    	if (bufferBitSet.get(i)) {
                            buffer[i] = 1;
                    	}
                    	else {
                    		buffer[i] = 0;
                    	}
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
    public void readImage(int[] buffer, int offset, int imageType) throws IOException {
        int i;
        int ii;
        int bufferSize = buffer.length;

        switch (imageType) {

            case ModelStorageBase.BOOLEAN:
                try {
                    fileRW.readImage(ModelStorageBase.BOOLEAN, offset, bufferSize);

                    BitSet bufferBitSet = fileRW.getBitSetBuffer();
                    
                    for (i = 0; i < bufferSize; i++) {
                    	if (bufferBitSet.get(i)) {
                            buffer[i] = 1;
                    	}
                    	else {
                    		buffer[i] = 0;
                    	}
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

            case ModelStorageBase.INTEGER:
                try {
                    fileRW.readImage(ModelStorageBase.INTEGER, offset, bufferSize);

                    int[] tmpBuffer = fileRW.getIntBuffer();

                    // for (i = 0; i < bufferSize; i++) {
                    // buffer[i] = tmpBuffer[i];
                    // }
                    // buffer = tmpBuffer;
                    System.arraycopy(tmpBuffer, 0, buffer, 0, tmpBuffer.length);
                } catch (IOException error) {
                    throw error;
                }

                break;

            case ModelStorageBase.UINTEGER:
                try {
                    fileRW.readImage(ModelStorageBase.UINTEGER, offset, bufferSize);

                    int[] tmpBuffer = fileRW.getIntBuffer();

                    for (i = 0; i < bufferSize; i++) {

                        // buffer[i] = tmpBuffer[i];
                        buffer[i] = (int) (tmpBuffer[i] & 0xffffffff);
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
        @SuppressWarnings("unused")
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
            	if(zeroLengthFlag) {
            		raFile.setLength(0);
            	}
                raFile.seek(startPosition);
            }

            fireProgressStateChanged(0);

            for (int t = beginTimePeriod; t <= endTimePeriod; t++) {

                for (k = beginSlice; k <= endSlice; k++) {
                    fireProgressStateChanged(MipavMath.round((float) ((t * (endSlice - beginSlice)) + k) / (nImages) * 100));

                    try {
                    	
                        fileRW.writeImage(image, (t * offset) + (k * bufferSize),
                                          (t * offset) + (k * bufferSize) + bufferSize);
                    	
                    	
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
     * This method writes a raw image file.
     *
     * @param      image    image model where the data is stored.
     * @param      beginSlice
     * @param      endSlice
     * @param      beginTimePeriod
     * @param      endTimePeriod
     *
     * @exception  IOException  if there is an error writing the file
     */
    public void writeImage(ModelImage image, int beginSlice, int endSlice, int beginTimePeriod, int endTimePeriod) throws IOException {
        int i, k;
        int[] extents;
        @SuppressWarnings("unused")
        int nBuffers;
        int bufferSize;
        int offset = 0;

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
                if(zeroLengthFlag) {
                    raFile.setLength(0);
                }
                raFile.seek(startPosition);
            }

            fireProgressStateChanged(0);

            for (int t = beginTimePeriod; t <= endTimePeriod; t++) {

                for (k = beginSlice; k <= endSlice; k++) {
                    fireProgressStateChanged(MipavMath.round((float) ((t * (endSlice - beginSlice)) + k) / (nImages) * 100));

                    try {
                        
                        fileRW.writeImage(image, (t * offset) + (k * bufferSize),
                                          (t * offset) + (k * bufferSize) + bufferSize);
                        
                        
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
        int beginSlice = options.getBeginSlice();
        int endSlice = options.getEndSlice();

        extents = image.getExtents();
        bufferSize = extents[0] * extents[1];

        nImages = endSlice - beginSlice + 1;
        dataFileName = new String[nImages];
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
                dataFileName[k - beginSlice] = fileString;
                raFile = new RandomAccessFile(file, "rw");
                if(zeroLengthFlag) {
            		raFile.setLength(0);
            	}
                raFile.seek(startPosition);
                fileRW = new FileRawChunk(raFile, image.getFileInfo(0));
            } else {
                fileRW = new FileRawChunk(fileDir + fileString, fileInfo, FileBase.READ_WRITE, compressionType);
            }


            try {
                fileRW.writeImage(image, k * bufferSize, (k * bufferSize) + bufferSize);

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
        int sliceSize;

        int beginTimePeriod = options.getBeginTime();
        int endTimePeriod = options.getEndTime();
        int beginSlice = options.getBeginSlice();
        int endSlice = options.getEndSlice();

        extents = image.getExtents();

        volSize = extents[2] * extents[1] * extents[0];
        sliceSize = extents[1] * extents[0];

        this.nTimePeriods = endTimePeriod - beginTimePeriod + 1;

        this.nImages = extents[2];
        dataFileName = new String[nTimePeriods];


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
                dataFileName[t - beginTimePeriod] = fileString;
                raFile = new RandomAccessFile(file, "rw");
                if(zeroLengthFlag) {
            		raFile.setLength(0);
            	}
                raFile.seek(startPosition);
                fileRW = new FileRawChunk(raFile, image.getFileInfo(0));
            } else {
                fileRW = new FileRawChunk(fileDir + fileString, fileInfo, FileBase.READ_WRITE, compressionType);
            }

            // write the given start/end point in the image to a file
            try {
                fileRW.writeImage(image, t * volSize + beginSlice * sliceSize, (t * volSize) + (endSlice + 1) * sliceSize);

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

    
    /**
     * setZeroLengthFlag
     * @param zeroLengthFlag
     */
	public void setZeroLengthFlag(boolean zeroLengthFlag) {
		this.zeroLengthFlag = zeroLengthFlag;
	}
    
    
}
