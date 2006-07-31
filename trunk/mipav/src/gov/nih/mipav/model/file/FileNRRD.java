package gov.nih.mipav.model.file;


import gov.nih.mipav.model.algorithms.utilities.*;
import gov.nih.mipav.model.structures.*;
import gov.nih.mipav.model.structures.jama.*;

import gov.nih.mipav.view.*;
import gov.nih.mipav.view.dialogs.*;

import java.awt.Toolkit;
import java.io.*;
import java.util.zip.GZIPInputStream;


/**
 
 * @see  FileIO
 * @see  FileInfoNRRD
 * @see  FileRaw
 */

public class FileNRRD extends FileBase {

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** DOCUMENT ME! */
    private int[] axisOrientation;

    

    /** DOCUMENT ME! */
    private String fileDir;

    /** DOCUMENT ME! */
    private File file;

    /** DOCUMENT ME! */
    private FileInfoNRRD fileInfo = null;

    /** DOCUMENT ME! */
    private String fileName;

    /** DOCUMENT ME! */
    private ModelImage image;

    
    /** DOCUMENT ME! */
    private TransMatrix matrix = new TransMatrix(4);

    
    /** DOCUMENT ME! */
    private boolean oneFile;

    /** DOCUMENT ME! */
    private boolean oneFileStorage;

    /** DOCUMENT ME! */
    private float[] origin;
    
    
    /** DOCUMENT ME! */
    private float[] resolutions;

   
    /** DOCUMENT ME! */
    private boolean showProgress = true;

    /** DOCUMENT ME! */
    private boolean foundEOF = false;
    
    private boolean foundEOHeader = false;
    
    private long eohLocation = 0L;

    /** DOCUMENT ME! */
    private ViewUserInterface UI;
    
    private long fileLength;
    
    private int versionNumber;
    
    /** Does not tell if color or black and white */
    private int nrrdDataType;

    /** May not be the same as MIPAV dimensions because in nrrd color is a dimension */
    private int nrrdDimensions;
    
    private int nrrdSizes[];
    
    private int startBlank[];
    
    private int finishBlank[];
    
    private int mipavDataType;
    
    private int mipavDimensions;
    
    private int imgExtents[];
    
    /** DOCUMENT ME! */
    private boolean endianess;
    
    private int skippedLines = 0;
    
    private int skippedBytes = 0;
    
    private long offset1;
    
    /** If true, data has gzip compression */
    private boolean gunzip = false;
    
    private double spacings[] = null;
    
    private float resols[];
    
    private double axisMins[];
    
    private double axisMaxs[];
    
    private int nrrdUnits[] = null;
    
    private int mipavUnits[];

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Constructs new file object.
     *
     * @param  _UI    User interface.
     * @param  fName  File name.
     * @param  fDir   File directory.
     * @param  show   Flag for showing the progress bar.
     */
    public FileNRRD(ViewUserInterface _UI, String fName, String fDir, boolean show) {
        UI = _UI;
        fileName = fName;
        fileDir = fDir;
        showProgress = show;
    }

    //~ Methods --------------------------------------------------------------------------------------------------------

    
    

    /**
     * Returns the FileInfoNRRD read from the file.
     *
     * @return  File info read from file, or null if it has not been read.
     */
    public FileInfoNRRD getFileInfo() {
        return fileInfo;
    }

    /**
     * Reads the NRRD header and stores the information in fileInfo.
     *
     * @param      imageFileName  File name of image.
     * @param      fileDir        Directory.
     *
     * @return     Flag to confirm a successful read.
     *
     * @exception  IOException  if there is an error reading the header
     *
     * @see        FileInfoNRRD
     */
    public boolean readHeader(String imageFileName, String fileDir) throws IOException {
        int i, j;
        int index;
        String fileHeaderName;
        int[] nrrdExtents = new int[5];
        int numDims = 0;
        String lineString;
        int colonIndex;
        int equalIndex;
        String fieldIDString;
        String fieldDescriptorString;
        boolean haveBlank;
        int lastSlashIndex;

        // index         = fileName.toLowerCase().indexOf(".img");
        index = fileName.lastIndexOf(".");

        if (fileName.substring(index + 1).equalsIgnoreCase("nrrd")) {
            oneFileStorage = true;
            fileHeaderName = fileName;
        } else {
            oneFileStorage = false;
            fileHeaderName = fileName.substring(0, index) + ".nhdr";
        }

        file = new File(fileDir + fileHeaderName);

        if (file.exists() == false) {
            fileHeaderName = fileName.substring(0, index) + ".NHDR";
            file = new File(fileDir + fileHeaderName);

            if (file.exists() == false) {
                return false;
            }
        }

        raFile = new RandomAccessFile(file, "r");
        fileLength = raFile.length();
        
        // Check that this is a NRRD file
        lineString = readLine();

        if (lineString == null) {
            raFile.close();
            throw new IOException("The file had no uncommented lines");
        }

        if (!lineString.substring(0,4).equalsIgnoreCase("NRRD")) {
            raFile.close();
            throw new IOException("Required NRRD magic not found at start of file");
        }
        lineString = lineString.substring(4);
        versionNumber = Integer.valueOf(lineString).intValue();
        fileInfo.setVersionNumber(versionNumber);
        Preferences.debug("versionNumber = " + versionNumber + "\n");
        
        while (lineString != null) {
            lineString = readLine();

            if (lineString != null) {
                 equalIndex = lineString.lastIndexOf("=");
                 colonIndex = lineString.lastIndexOf(":");
                 if ((equalIndex >= 2) && (colonIndex == (equalIndex - 1))) {
                     // Key-value pair present
                 } // if ((equalIndex >= 2) && (colonIndex == (equalIndex - 1)))
                 else if (colonIndex >= 1) {
                     // field identifier: field descriptor present
                     fieldIDString = lineString.substring(0,colonIndex);
                     fieldDescriptorString = lineString.substring(colonIndex+2).trim();
                     if (fieldIDString.equalsIgnoreCase("TYPE")) {
                         if ((fieldDescriptorString.equalsIgnoreCase("SIGNED CHAR")) ||
                             (fieldDescriptorString.equalsIgnoreCase("INT8"))||
                             (fieldDescriptorString.equalsIgnoreCase("INT8_T"))) {
                             nrrdDataType = ModelStorageBase.BYTE;
                             Preferences.debug("NRRD data type = BYTE\n");
                         }
                         else if ((fieldDescriptorString.equalsIgnoreCase("UCHAR")) ||
                             (fieldDescriptorString.equalsIgnoreCase("UNSIGNED CHAR")) ||
                             (fieldDescriptorString.equalsIgnoreCase("UINT8"))||
                             (fieldDescriptorString.equalsIgnoreCase("UINT8_T"))) {
                             nrrdDataType = ModelStorageBase.UBYTE;
                             Preferences.debug("NRRD data type = UBYTE\n");
                         }
                         else if ((fieldDescriptorString.equalsIgnoreCase("SHORT")) ||
                                  (fieldDescriptorString.equalsIgnoreCase("SHORT INT")) ||
                                  (fieldDescriptorString.equalsIgnoreCase("SIGNED SHORT")) ||
                                  (fieldDescriptorString.equalsIgnoreCase("SIGNED SHORT INT")) ||
                                  (fieldDescriptorString.equalsIgnoreCase("INT16")) ||
                                  (fieldDescriptorString.equalsIgnoreCase("INT16_T"))) {
                             nrrdDataType = ModelStorageBase.SHORT;
                             Preferences.debug("NRRD data type = SHORT\n");
                         }
                         else if ((fieldDescriptorString.equalsIgnoreCase("USHORT")) ||
                                 (fieldDescriptorString.equalsIgnoreCase("UNSIGNED SHORT")) ||
                                 (fieldDescriptorString.equalsIgnoreCase("UNSIGNED SHORT INT")) ||
                                 (fieldDescriptorString.equalsIgnoreCase("UINT16")) ||
                                 (fieldDescriptorString.equalsIgnoreCase("UINT16_T"))) {
                             nrrdDataType = ModelStorageBase.USHORT;
                             Preferences.debug("NRRD data type = USHORT\n");
                         }
                         else if ((fieldDescriptorString.equalsIgnoreCase("INT")) ||
                                  (fieldDescriptorString.equalsIgnoreCase("SIGNED INT")) ||
                                  (fieldDescriptorString.equalsIgnoreCase("INT32")) ||
                                  (fieldDescriptorString.equalsIgnoreCase("INT32_T"))) {
                             nrrdDataType = ModelStorageBase.INTEGER;
                             Preferences.debug("NRRD data type = INTEGER\n");
                         }
                         else if ((fieldDescriptorString.equalsIgnoreCase("UINT")) ||
                                 (fieldDescriptorString.equalsIgnoreCase("UNSIGNED INT")) ||
                                 (fieldDescriptorString.equalsIgnoreCase("UINT32")) ||
                                 (fieldDescriptorString.equalsIgnoreCase("UINT32_T"))) {
                             nrrdDataType = ModelStorageBase.UINTEGER;
                             Preferences.debug("NRRD data type = UINTEGER\n");
                         }
                         else if ((fieldDescriptorString.equalsIgnoreCase("LONGLONG")) ||
                                  (fieldDescriptorString.equalsIgnoreCase("LONG LONG")) ||
                                  (fieldDescriptorString.equalsIgnoreCase("LONG LONG INT")) ||
                                  (fieldDescriptorString.equalsIgnoreCase("SIGNED LONG LONG")) ||
                                  (fieldDescriptorString.equalsIgnoreCase("SIGNED LONG LONG INT")) ||
                                  (fieldDescriptorString.equalsIgnoreCase("INT64")) ||
                                  (fieldDescriptorString.equalsIgnoreCase("INT64_T"))) {
                             nrrdDataType = ModelStorageBase.LONG;
                             Preferences.debug("NRRD data type = LONG\n");
                         }
                         else if ((fieldDescriptorString.equalsIgnoreCase("ULONGLONG")) ||
                                 (fieldDescriptorString.equalsIgnoreCase("UNSIGNED LONG LONG")) ||
                                 (fieldDescriptorString.equalsIgnoreCase("UNSIGNED LONG LONG INT")) ||
                                 (fieldDescriptorString.equalsIgnoreCase("UINT64")) ||
                                 (fieldDescriptorString.equalsIgnoreCase("UINT64_T"))) {
                             nrrdDataType = ModelStorageBase.LONG;
                             MipavUtil.displayWarning("Warning: Reading unsigned long as signed long");
                             Preferences.debug("Warning: Reading unsigned long as signed long\n");
                         }
                         else if (fieldDescriptorString.equalsIgnoreCase("FLOAT")) {
                             nrrdDataType = ModelStorageBase.FLOAT;
                             Preferences.debug("NRRD data type = FLOAT\n");
                         }
                         else if (fieldDescriptorString.equalsIgnoreCase("DOUBLE")) {
                             nrrdDataType = ModelStorageBase.DOUBLE;
                             Preferences.debug("NRRD data type = DOUBLE\n");
                         }
                         else if (fieldDescriptorString.equalsIgnoreCase("BLOCK")) {
                             MipavUtil.displayError("Cannot handle nrrd block data type");
                             Preferences.debug("NRRD data type = BLOCK\n");
                             throw new IOException();  
                         }
                         else {
                             MipavUtil.displayError("Unknown NRRD data type = " + fieldDescriptorString);
                             Preferences.debug("Unknown NRRD data type = " + fieldDescriptorString + "\n");
                             throw new IOException();
                         }
                     } // if (fieldIDString.equalsIgnoreCase("TYPE"))
                     else if (fieldIDString.equalsIgnoreCase("DIMENSION")) {
                         nrrdDimensions = Integer.valueOf(fieldDescriptorString).intValue();
                         Preferences.debug("NRRD dimensions = " + nrrdDimensions + "\n");
                         startBlank = new int[nrrdDimensions-1];
                         finishBlank = new int[nrrdDimensions-1];
                     } // else if (fieldIDString.equalsIgnoreCase("DIMENSION"))
                     else if (fieldIDString.equalsIgnoreCase("SIZES")) {
                         nrrdSizes = new int[nrrdDimensions];
                         for (i = 0, j = 0; i < nrrdDimensions-1;) {
                          if (!fieldDescriptorString.substring(j,j+1).equals(" ")) {
                              j++;
                          }
                          else {
                              startBlank[i] = j;
                              finishBlank[i] = j++;
                              while (fieldDescriptorString.substring(j,j+1).equals(" ")) {
                                  finishBlank[i] = j++;
                              }
                              if (i == 0) {
                                  nrrdSizes[i] = Integer.valueOf
                                  (fieldDescriptorString.substring(0,startBlank[0])).intValue();
                                  Preferences.debug("NRRD sizes[" + i + "] = " + nrrdSizes[i] + "\n");
                              }
                              else {
                                  nrrdSizes[i] = Integer.valueOf
                                  (fieldDescriptorString.substring(finishBlank[i-1]+1, startBlank[i])).intValue();
                                  Preferences.debug("NRRD sizes[" + i + "] = " + nrrdSizes[i] + "\n");
                              }
                              i++;
                          }
                         } // for (i = 0; j = 0; i < nrrdDimensions-1;)
                         nrrdSizes[nrrdDimensions-1] = Integer.valueOf
                         (fieldDescriptorString.substring(finishBlank[nrrdDimensions-2]+1)).intValue();
                         Preferences.debug("NRRD sizes[" + i + "] = " + nrrdSizes[i] + "\n");
                     } // else if (fieldIDString.equalsIgnoreCase("SIZES"))
                     else if (fieldIDString.equalsIgnoreCase("ENDIAN")) {
                         if (fieldDescriptorString.equalsIgnoreCase("BIG")) {
                             endianess = FileBase.BIG_ENDIAN;
                             fileInfo.setEndianess(endianess);
                             Preferences.debug("BIG ENDIAN\n");
                         }
                         else if (fieldDescriptorString.equalsIgnoreCase("LITTLE")) {
                             endianess = FileBase.LITTLE_ENDIAN;
                             fileInfo.setEndianess(endianess);
                             Preferences.debug("LITTLE ENDIAN\n");
                         }
                         else {
                             MipavUtil.displayError("Illegal endian value of " + fieldDescriptorString);
                             Preferences.debug("Illegal endian value of " + fieldDescriptorString + "\n");
                             throw new IOException();
                         }
                     } // else if (fieldIDString.equalsIgnoreCase("ENDIAN"))
                     else if ((fieldIDString.equalsIgnoreCase("LINE SKIP")) ||
                              (fieldIDString.equalsIgnoreCase("LINESKIP"))) {
                          skippedLines = Integer.valueOf(fieldDescriptorString).intValue();
                          Preferences.debug("Skipped lines = " + skippedLines + "\n");
                     } // else if ((fieldIDString.equalsIgnoreCase("LINE SKIP")) ||
                     else if ((fieldIDString.equalsIgnoreCase("BYTE SKIP")) ||
                              (fieldIDString.equalsIgnoreCase("BYTESKIP"))) {
                          skippedBytes = Integer.valueOf(fieldDescriptorString).intValue();
                          Preferences.debug("Skipped bytes = " + skippedBytes + "\n");
                     } // else if ((fieldIDString.equalsIgnoreCase("BYTE SKIP")) ||
                     else if ((fieldIDString.equalsIgnoreCase("DATA FILE")) ||
                              (fieldIDString.equalsIgnoreCase("DATAFILE"))) {
                         haveBlank = false;
                         for (i = 0; i < fieldDescriptorString.length() && !haveBlank; i++) {
                              if (fieldDescriptorString.substring(i,i+1).equals(" ")) {
                                  haveBlank = true;
                              }
                         }
                         if ((!haveBlank) && (!fieldDescriptorString.equalsIgnoreCase("LIST"))) {
                             // There is a single detached data file and its file directory
                             // and fileName are given by fieldDescriptorString
                             if ((fieldDescriptorString.substring(0,1).equals("."))  &&
                                 (fieldDescriptorString.substring(1,2).equals("/"))) {
                                 lastSlashIndex = fieldDescriptorString.lastIndexOf("/");
                                 if (lastSlashIndex > 1) {
                                     fileDir = fileDir.concat
                                     (fieldDescriptorString.substring(1,lastSlashIndex+1));
                                     Preferences.debug("Data file directory = " + fileDir + "\n");
                                     fileInfo.setFileDirectory(fileDir);
                                     fileName = fieldDescriptorString.substring(lastSlashIndex+1);
                                     Preferences.debug("Data file name = " + fileName + "\n");
                                     fileInfo.setFileName(fileName);
                                 }
                                 else {
                                     fileName = fieldDescriptorString.substring(2);
                                     Preferences.debug("Data file name = " + fileName + "\n");
                                     fileInfo.setFileName(fileName);
                                 }
                             }
                             else if (fieldDescriptorString.substring(0,1).equals(".")) {
                                lastSlashIndex = fieldDescriptorString.lastIndexOf("/");
                                fileDir = fieldDescriptorString.substring(0,lastSlashIndex+1);
                                Preferences.debug("Data file directory = " + fileDir + "\n");
                                fileInfo.setFileDirectory(fileName);
                                fileName = fieldDescriptorString.substring(lastSlashIndex+1);
                                Preferences.debug("Data file name = " + fileName + "\n");
                                fileInfo.setFileName(fileName);
                             }
                             else {
                                 fileName = fieldDescriptorString;
                                 Preferences.debug("Data file name = " + fileName + "\n");
                                 fileInfo.setFileName(fileName);
                             }
                         }
                     } // else if ((fieldIDString.equalsIgnoreCase("DATA FILE"))
                     else if (fieldIDString.equalsIgnoreCase("ENCODING")) {
                         if (fieldDescriptorString.equalsIgnoreCase("RAW")) {
                             Preferences.debug("Encoding = raw\n");
                             fileInfo.setEncoding("Raw");
                         }
                         else if ((fieldDescriptorString.equalsIgnoreCase("GZ")) ||
                                  (fieldDescriptorString.equalsIgnoreCase("GZIP"))) {
                             Preferences.debug("Encoding = gzip\n");
                             fileInfo.setEncoding("Gzip");
                             gunzip = true;
                         }
                     } // else if (fieldIDString.equalsIgnoreCase("ENCODING"))
                     else if (fieldIDString.equalsIgnoreCase("CONTENT")) {
                         Preferences.debug("Content = " + fieldDescriptorString + "\n");
                         fileInfo.setContent(fieldDescriptorString);
                     } // else if (fieldIDString.equalsIgnoreCase("CONTENT"))
                     else if (fieldIDString.equalsIgnoreCase("SPACINGS")) {
                         spacings = new double[nrrdDimensions];
                         for (i = 0, j = 0; i < nrrdDimensions-1;) {
                          if (!fieldDescriptorString.substring(j,j+1).equals(" ")) {
                              j++;
                          }
                          else {
                              startBlank[i] = j;
                              finishBlank[i] = j++;
                              while (fieldDescriptorString.substring(j,j+1).equals(" ")) {
                                  finishBlank[i] = j++;
                              }
                              if (i == 0) {
                                  if (fieldDescriptorString.substring(0,startBlank[0]).equalsIgnoreCase("NAN")) {
                                      spacings[i] = Double.NaN;
                                  }
                                  else {
                                      spacings[i] = Double.valueOf
                                      (fieldDescriptorString.substring(0,startBlank[0])).doubleValue();
                                  }
                                  Preferences.debug("NRRD spacings[" + i + "] = " + spacings[i] + "\n");
                              }
                              else {
                                  if (fieldDescriptorString.substring(0,startBlank[0]).equalsIgnoreCase("NAN")) {
                                      spacings[i] = Double.NaN;
                                  }
                                  else {
                                      spacings[i] = Double.valueOf(fieldDescriptorString.substring
                                                     (finishBlank[i-1]+1, startBlank[i])).doubleValue();
                                  }
                                  Preferences.debug("NRRD spacings[" + i + "] = " + spacings[i] + "\n");
                              }
                              i++;
                          }
                         } // for (i = 0; j = 0; i < nrrdDimensions-1;)
                         if (fieldDescriptorString.substring(0,startBlank[0]).equalsIgnoreCase("NAN")) {
                             spacings[nrrdDimensions-1] = Double.NaN;
                         }
                         else {
                             spacings[nrrdDimensions-1] = Double.valueOf
                             (fieldDescriptorString.substring(finishBlank[nrrdDimensions-2]+1)).doubleValue();
                         }
                         Preferences.debug("NRRD spacings[" + i + "] = " + spacings[i] + "\n");    
                     } // else if (fieldIDString.equalsIgnoreCase("SPACINGS"))
                     else if ((fieldIDString.equalsIgnoreCase("AXIS MINS")) ||
                             (fieldIDString.equalsIgnoreCase("AXISMINS"))) {
                        axisMins = new double[nrrdDimensions];
                        for (i = 0, j = 0; i < nrrdDimensions-1;) {
                            if (!fieldDescriptorString.substring(j,j+1).equals(" ")) {
                                j++;
                            }
                            else {
                                startBlank[i] = j;
                                finishBlank[i] = j++;
                                while (fieldDescriptorString.substring(j,j+1).equals(" ")) {
                                    finishBlank[i] = j++;
                                }
                                if (i == 0) {
                                    if (fieldDescriptorString.substring(0,startBlank[0]).equalsIgnoreCase("NAN")) {
                                        axisMins[i] = Double.NaN;
                                    }
                                    else {
                                        axisMins[i] = Double.valueOf
                                        (fieldDescriptorString.substring(0,startBlank[0])).doubleValue();
                                    }
                                    Preferences.debug("NRRD axis minimum[" + i + "] = " + axisMins[i] + "\n");
                                }
                                else {
                                    if (fieldDescriptorString.substring(finishBlank[i-1]+1,startBlank[i])
                                            .equalsIgnoreCase("NAN")) {
                                        axisMins[i] = Double.NaN;
                                    }
                                    else {
                                        axisMins[i] = Double.valueOf(fieldDescriptorString.substring
                                                       (finishBlank[i-1]+1, startBlank[i])).doubleValue();
                                    }
                                    Preferences.debug("NRRD axis minimum[" + i + "] = " + axisMins[i] + "\n");
                                }
                                i++;
                            }
                        } // for (i = 0; j = 0; i < nrrdDimensions-1;)
                        if (fieldDescriptorString.substring(finishBlank[nrrdDimensions-2]+1).equalsIgnoreCase("NAN")) {
                            axisMins[nrrdDimensions-1] = Double.NaN;
                        }
                        else {
                            axisMins[nrrdDimensions-1] = Double.valueOf
                            (fieldDescriptorString.substring(finishBlank[nrrdDimensions-2]+1)).doubleValue();
                        }
                        Preferences.debug("NRRD axis minimum[" + i + "] = " + axisMins[i] + "\n");    
                     } // else if (fieldIDString.equalsIgnoreCase("AXIS MINS")) ||
                     else if ((fieldIDString.equalsIgnoreCase("AXIS MAXS")) ||
                             (fieldIDString.equalsIgnoreCase("AXISMAXS"))) {
                        axisMaxs = new double[nrrdDimensions];
                        for (i = 0, j = 0; i < nrrdDimensions-1;) {
                            if (!fieldDescriptorString.substring(j,j+1).equals(" ")) {
                                j++;
                            }
                            else {
                                startBlank[i] = j;
                                finishBlank[i] = j++;
                                while (fieldDescriptorString.substring(j,j+1).equals(" ")) {
                                    finishBlank[i] = j++;
                                }
                                if (i == 0) {
                                    if (fieldDescriptorString.substring(0,startBlank[0]).equalsIgnoreCase("NAN")) {
                                        axisMaxs[i] = Double.NaN;
                                    }
                                    else {
                                        axisMaxs[i] = Double.valueOf
                                        (fieldDescriptorString.substring(0,startBlank[0])).doubleValue();
                                    }
                                    Preferences.debug("NRRD axis maximum[" + i + "] = " + axisMaxs[i] + "\n");
                                }
                                else {
                                    if (fieldDescriptorString.substring(finishBlank[i-1]+1, startBlank[i])
                                            .equalsIgnoreCase("NAN")) {
                                        axisMaxs[i] = Double.NaN;
                                    }
                                    else {
                                        axisMaxs[i] = Double.valueOf(fieldDescriptorString.substring
                                                       (finishBlank[i-1]+1, startBlank[i])).doubleValue();
                                    }
                                    Preferences.debug("NRRD axis maximum[" + i + "] = " + axisMaxs[i] + "\n");
                                }
                                i++;
                            }
                        } // for (i = 0; j = 0; i < nrrdDimensions-1;)
                        if (fieldDescriptorString.substring(finishBlank[nrrdDimensions-2]+1).equalsIgnoreCase("NAN")) {
                            axisMaxs[nrrdDimensions-1] = Double.NaN;
                        }
                        else {
                            axisMaxs[nrrdDimensions-1] = Double.valueOf
                            (fieldDescriptorString.substring(finishBlank[nrrdDimensions-2]+1)).doubleValue();
                        }
                        Preferences.debug("NRRD axis maximum[" + i + "] = " + axisMaxs[i] + "\n");    
                     } // else if (fieldIDString.equalsIgnoreCase("AXIS MAXS")) ||
                     else if (fieldIDString.equalsIgnoreCase("UNITS")) {
                         
                     } // else if (fieldIDString.equalsIgnoreCase("UNITS))
                 } // else if (colonIndex >= 1)
            } // if (lineString != null)
        } // while (lineString != null)
        
        if (!oneFileStorage) {
            raFile.close();
        }
        
        if (((nrrdSizes[0] == 2) || (nrrdSizes[0] == 3)) && (nrrdDimensions >= 3) &&
             ((nrrdDataType == ModelStorageBase.UBYTE) || (nrrdDataType == ModelStorageBase.USHORT) ||
              (nrrdDataType == ModelStorageBase.FLOAT))) { 
            if (nrrdDataType == ModelStorageBase.UBYTE) {
                mipavDataType = ModelStorageBase.ARGB;
            }
            else if (nrrdDataType == ModelStorageBase.USHORT) {
                mipavDataType = ModelStorageBase.ARGB_USHORT;
            }
            else {
                mipavDataType = ModelStorageBase.ARGB_FLOAT;
            }
            mipavDimensions = nrrdDimensions - 1;
            imgExtents = new int[mipavDimensions];
            resols = new float[mipavDimensions];
            for (i = 0; i < mipavDimensions; i++) {
                imgExtents[i] = nrrdSizes[i+1];
                if (spacings != null) {
                    if (Double.isNaN(spacings[i+1])) {
                        resols[i] = 1.0f;
                    }
                    else {
                        resols[i] = (float)Math.abs(spacings[i+1]);
                    }
                } // if (spacings != null)
                else { // spacings == null
                    resols[i] = 1.0f;
                } // else spacings == null
            }
        } // if (((nrrdSizes[0] == 2) || (nrrdSizes[0] == 3)) && (nrrdDimensions >= 3))
        else {
            mipavDataType = nrrdDataType;
            mipavDimensions = nrrdDimensions;
            imgExtents = new int[mipavDimensions];
            resols = new float[mipavDimensions];
            for (i = 0; i < mipavDimensions; i++) {
                imgExtents[i] = nrrdSizes[i];
                if (spacings != null) {
                    if (Double.isNaN(spacings[i])) {
                        resols[i] = 1.0f;
                    }
                    else {
                        resols[i] = (float)Math.abs(spacings[i]);
                    }
                } // if (spacings != null)
                else { // spacings == null
                    resols[i] = 1.0f;
                } // else spacings == null
            }
        }
        
        fileInfo.setDataType(mipavDataType);
        fileInfo.setExtents(imgExtents);
        fileInfo.setResolutions(resols);
        
        return true; // If it got this far, it has successfully read in the header
    }
    
    /**
     * Reads lines of the file and strips comments indicated by the # symbol until a nonnull String results or the end
     * of the file is reached or a blank line containing end of header with zero characters is reached.
     *
     * @return     the line read in
     *
     * @exception  IOException  if there is an error reading the file
     */
    private String readLine() throws IOException {
        String tempString = null;
        int index;

        while ((tempString == null) && (raFile.getFilePointer() < (fileLength - 1)) && (!foundEOF) 
               && (!foundEOHeader)) {

            try {
                tempString = raFile.readLine();
            } catch (EOFException error) {
                tempString = null;
                foundEOF = true;
            } catch (IOException error) {
                throw (error);
            }
            
            if (tempString == null) {
                foundEOHeader = true;
            }
            else if (tempString.length() == 0) {
                foundEOHeader = true;
            }
            if (foundEOHeader) {
                eohLocation = raFile.getFilePointer();
            }

            if (tempString != null) {
                index = tempString.indexOf("#");

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

    /**
     * Reads a NRRD image file by reading the header then making a FileRaw to read the image for all filenames in the
     * file list. Only the one file directory (currently) supported.
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
        int offset;
        int linesFound;
        int i;
        byte buf[] = new byte[1];
        fileInfo = new FileInfoNRRD(fileName, fileDir, FileBase.NRRD);
        long dataSize;
        FileInputStream fis;
        int s;
        
        progressBar = new ViewJProgressBar(ViewUserInterface.getReference().getProgressBarPrefix() + fileName,
                ViewUserInterface.getReference().getProgressBarPrefix() + "NRRD image(s) ...",
                0, 100, false, null, null);
        progressBar.setLocation((int) Toolkit.getDefaultToolkit().getScreenSize().getWidth() / 2, 50);
        setProgressBarVisible(ViewUserInterface.getReference().isAppFrameVisible());

        if (!readHeader(fileInfo.getFileName(), fileInfo.getFileDirectory())) {
            throw (new IOException(" NRRD header file error"));
        }
        
        if (!oneFileStorage) {
            file = new File(fileDir + fileName);
            raFile = new RandomAccessFile(file, "r");
            fileLength = raFile.length();
        }
        
        // Do line skipping before byte skipping
        // Do line skipping before decompression
        if (skippedLines > 0) {
            linesFound = 0;
            for (i = 0; linesFound < skippedLines; i++) {
                raFile.read(buf);
                if (buf[0] == 10 /* new line */) {
                    linesFound++;
                }
            }
        } // if(skippedLines > 0)
        offset1 = raFile.getFilePointer();
        raFile.close();
        
        if (gunzip) {
            int totalBytesRead = 0;
            progressBar.setVisible(isProgressBarVisible());
            progressBar.setMessage("Uncompressing GZIP file ...");
            fis = new FileInputStream(file);
            fis.skip(offset1);

            GZIPInputStream gzin = new GZIPInputStream(new BufferedInputStream(fis));
            s = fileName.lastIndexOf(".");
            String uncompressedName = fileDir + fileName.substring(0, s);
            FileOutputStream out = new FileOutputStream(uncompressedName);
            byte[] buffer = new byte[256];

            while (true) {
                int bytesRead = gzin.read(buffer);

                if (bytesRead == -1) {
                    break;
                }

                totalBytesRead += bytesRead;
                out.write(buffer, 0, bytesRead);
            }

            out.close();
            file = new File(uncompressedName);
            fileInfo.setFileName(fileName.substring(0,s));
            offset1 = 0;
            raFile = new RandomAccessFile(file, "r");
            fileLength = raFile.length();
            raFile.close();
        } // if (gunzip)
        
        // Do byte skipping after decompression
        if (skippedBytes >= 0) {
            offset = (int)(offset1 + skippedBytes);
        }
        else { // skippedBytes < 0
            dataSize = imgExtents[0] * imgExtents[1];
            if (mipavDimensions >= 3) {
                dataSize *= imgExtents[2];
            }
            if (mipavDimensions >= 4) {
                dataSize *= imgExtents[3];
            }
            switch (nrrdDataType) {

                case ModelStorageBase.BYTE:
                case ModelStorageBase.UBYTE:
                    break;
    
                case ModelStorageBase.SHORT:
                case ModelStorageBase.USHORT:
                    dataSize *= 2;
                    break;
    
                case ModelStorageBase.INTEGER:
                case ModelStorageBase.UINTEGER:
                case ModelStorageBase.FLOAT:
                    dataSize *= 4;
                    break;
    
                case ModelStorageBase.LONG:
                case ModelStorageBase.DOUBLE:
                    dataSize *= 8;
                    break;

            } // switch (nrrdDataType)
            offset = (int)(fileLength - dataSize);
        } // else skippedBytes < 0

        int[] extents = null;

        try {

            if (one) {
                extents = new int[fileInfo.getExtents().length];

                for (i = 0; i < extents.length; i++) {
                    extents[i] = fileInfo.getExtents()[i];
                }

                image = new ModelImage(fileInfo.getDataType(), new int[] { extents[0], extents[1] },
                                       fileInfo.getFileName(), UI);
            } else {
                image = new ModelImage(fileInfo.getDataType(), fileInfo.getExtents(), fileInfo.getFileName(), UI);
            }
        } catch (OutOfMemoryError error) {
            throw (error);
        }

        updateUnitsOfMeasure(fileInfo, image);
        updateorigins(image.getFileInfo());
        image.setMatrix(matrix);
        
        try { // Construct a FileRaw to actually read the image.

            FileRaw rawFile;
            rawFile = new FileRaw(fileInfo.getFileName(), fileInfo.getFileDirectory(), fileInfo, showProgress,
                                  FileBase.READ);

            
            if (one) {

                if (fileInfo.getExtents().length > 2) {
                    offset = offset + getOffset(fileInfo);
                }
            }
            progressBar.setVisible(isProgressBarVisible());
            progressBar.setMessage("Reading in data ...");
            rawFile.readImage(image, offset);

            
            if (one) {
                fileInfo.setExtents(extents);
            }
        } catch (IOException error) {
            throw new IOException("FileNRRD: " + error);
        } catch (OutOfMemoryError e) {
            throw (e);
        }


        
        if (progressBar != null) {
            progressBar.dispose();
        }
        return image;
    }
    
    /**
     * DOCUMENT ME!
     *
     * @param  fileInfo  -- a NRRD file Info that has already been read
     * @param  image     -- a ModelImage that the fileInfo needs to be attached to
     */
    private void updateUnitsOfMeasure(FileInfoNRRD fileInfo, ModelImage image) {

        int[] extents = fileInfo.getExtents();

        if (image.getNDims() == 2) {
            //fileInfo.setUnitsOfMeasure(FileInfoBase.MILLIMETERS, 0);
            //fileInfo.setUnitsOfMeasure(FileInfoBase.MILLIMETERS, 1);
            image.setFileInfo(fileInfo, 0); // Otherwise just set the first fileInfo
        } else if (image.getNDims() == 3) { // If there is more than one image
            //fileInfo.setUnitsOfMeasure(FileInfoBase.MILLIMETERS, 0);
            //fileInfo.setUnitsOfMeasure(FileInfoBase.MILLIMETERS, 1);
            //fileInfo.setUnitsOfMeasure(FileInfoBase.MILLIMETERS, 2);

            for (int i = 0; i < extents[2]; i++) {
                FileInfoNRRD newFileInfo = (FileInfoNRRD) fileInfo.clone();
                newFileInfo.setOrigin(fileInfo.getOriginAtSlice(i));
                image.setFileInfo(newFileInfo, i); // Set the array of fileInfos in ModelImage
            }
        } else if (image.getNDims() == 4) { // If there is more than one image
            //fileInfo.setUnitsOfMeasure(FileInfoBase.MILLIMETERS, 0);
            //fileInfo.setUnitsOfMeasure(FileInfoBase.MILLIMETERS, 1);
            //fileInfo.setUnitsOfMeasure(FileInfoBase.MILLIMETERS, 2);
            //fileInfo.setUnitsOfMeasure(FileInfoBase.MILLISEC, 3);

            for (int i = 0; i < (extents[2] * extents[3]); i++) {
                FileInfoNRRD newFileInfo = (FileInfoNRRD) fileInfo.clone();
                newFileInfo.setOrigin(fileInfo.getOriginAtSlice(i));
                image.setFileInfo(newFileInfo, i); // Set the array of fileInfos in ModelImage
            }
        }

    } // updateUnitsOfMeasure()
    
    /**
     * Updates the start locations. Each image has a fileinfo where the start locations are stored. Note that the start
     * location for the Z (3rd) dimension change with the change is the slice. The origin is in the upper left corner
     * and we are using the right hand rule. + x -> left to right; + y -> top to bottom and + z -> into screen.
     *
     * @param  fileInfo  DOCUMENT ME!
     */
    private void updateorigins(FileInfoBase[] fileInfo) {
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
        /*else if (image.getNDims() == 5) {
         * fileInfo = image.getFileInfo(); for (int i = 0;    i <    image.getExtents()[2] * image.getExtents()[3] *
         * image.getExtents()[4];    i++) { fileInfo[i].setorigins(startLocs); startLocs[4] += resolutions[4]; }  }*/
    }
    
    /**
     * Helper method to calculate the offset for getting only the middle NIFTI image slice from the 3D file.
     *
     * @param   fileInfo  File info.
     *
     * @return  offset
     */
    private int getOffset(FileInfoNRRD fileInfo) {
        int offset = fileInfo.getExtents()[0] * fileInfo.getExtents()[1] * (fileInfo.getExtents()[2] / 2);

        switch (nrrdDataType) {

            case ModelStorageBase.BYTE:
            case ModelStorageBase.UBYTE:
                break;

            case ModelStorageBase.SHORT:
            case ModelStorageBase.USHORT:
                offset *= 2;
                break;

            case ModelStorageBase.INTEGER:
            case ModelStorageBase.UINTEGER:
            case ModelStorageBase.FLOAT:
                offset *= 4;
                break;

            case ModelStorageBase.LONG:
            case ModelStorageBase.DOUBLE:
                offset *= 8;
                break;

        }

        return offset;
    }

    /**
     * Reads a NRRD image file by reading the header then making a FileRaw to read the file. Image data is left in
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
        int offset;

        

        return;
    }

    
   
}
