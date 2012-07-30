package gov.nih.mipav.model.file;


import gov.nih.mipav.model.file.FileInfoBase.Unit;
import gov.nih.mipav.model.structures.*;

import java.io.*;
import gov.nih.mipav.view.*;

/**
   Documentation used was the ZVI Format Specification V 2.0.5 - August, 2010.
   The following email was sent to Zeiss support:
   Here are problems I spotted in ZVI Format Specification V 2.0.5 - August, 2010:
2.1.1 <Contents> stream of the container image:
TypeDescription is VT_EMPTY rather than VT_BSTR
FileName is VT_EMPTY rather than VT_BSTR
m_PluginCLSID is VT_BLOB rather than VT_CLSID
The {Tags} VT_STORED_OBJECT that goes between the {Layers} and {Scaling} VT_STORED_OBJECTs is missing.

2.2.1 <Contents> stream of the image item:
Version is used rather than unused with minor = 1003 hex and major = 3000 hex.
TypeDescription is VT_EMPTY rather than VT_BSTR.
FileName is VT_EMPTY rather than VT_BSTR
m_PluginCLSID is VT_BLOB rather than VT_CLSID
Items after the {Others} VT_BLOB are completely different from what's listed.  I find:
VT_DISPATCH
VT_STORED_OBJECT with a string = Tags
VT_DISPATCH
VT_DISPATCH
VT_DISPATCH
RAW pixel data

3.3 Coordinate ID for Image Dimensions:
Index should be 0 1 2 3 4 5 6 7 instead of the existing 0 1 3 4 5 6 7 8.

3.4 Tag IDs
ID 301 is used for both ImageBaseTimeFirst and ImageBaseTime1.  I suspect the ImageBaseTimeFirst entry should be deleted.
The following tags show up in .ZVI files but are not listed in your table:
2071, 20478.  What is the info for these tags?
65781 has both AuroxCamRes6 and AuroxCamCFactor.

Information for decoding the 64-bit VT_DATE structure is missing.  Is it available somewhere?

Is it possible for me to obtain sample .ZVI files so I can improve the ZVI file read in the MIPAV image processing program?

                                                                                           Sincerely,

                                                                                       William Gandler

 */

public class FileZVI extends FileBase {
    
    private static final short VT_EMPTY = 0;
    
    private static final short VT_BOOL = 11;
    
    private static final short VT_I2 = 2;
    
    //private static final short VT_UI2 = 18;
    
    private static final short VT_I4 = 3;
    
    private static final short VT_R8 = 5;
    
    private static final short VT_BSTR = 8;
    
    private static final short VT_STORED_OBJECT = 69;
    
    private static final short VT_DATE = 7;
    
    private static final short VT_DISPATCH = 9;
    
    //private static final short VT_UNKNOWN = 13;
    
    private static final short VT_BLOB = 65;
    
    //private static final short VT_CLSID = 72;
   
    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** DOCUMENT ME! */
    private File file;

    /** DOCUMENT ME! */
    private String fileDir;


    /** DOCUMENT ME! */
    private FileInfoZVI fileInfo;

    /** DOCUMENT ME! */
    private String fileName;

    /** DOCUMENT ME! */
    private ModelImage image;

    /** DOCUMENT ME! */
    private int[] imageExtents = null;

    /** DOCUMENT ME! */
    private float[] imgBuffer = null;

    /** DOCUMENT ME! */
    private float[] imgResols = new float[5];

    /** DOCUMENT ME! */
    private ModelLUT LUT = null;
    
    private boolean endianess;
    
    private int sectorSize;
    
    private int shortSectorSize;
    
    private int shortSectorTable[] = null;
    
    private int shortStreamStartSect;
    
    private long totalShortStreamSize;
    
    private int imageWidth = 0;
    
    private int imageHeight = 0;
    
    private int imageCount = 0;
    
    private int imagePixelFormat = 0;
    
    private int zDim = 1;
    
    private int backupZDim = 1;
    
    private int tDim = 1;
    
    private int channelNumber = 1;
    
    private int positionNumber = 1;
    
    private int zArray[] = null;
    private int cArray[] = null;
    private int tArray[] = null;
    private int positionArray[] = null;
    private int startSectorArray[] = null;
    private int offsetArray[] = null;
    // array pointer
    private int ap = 0;
    private double imageFocusPositionArray[] = null;
    private double imageStagePositionXArray[] = null;
    private int imageZXArray[] = null;
    private int imageZYArray[] = null;
    private double imageStagePositionYArray[] = null;
    private int imageZArray[] = null;
    private int imageZ2Array[] = null;
    private int imageC2Array[] = null;
    private int imageT2Array[] = null;
    private double imageBlackValueArray[] = null;
    private int imageZ3Array[] = null;
    private int imageC3Array[] = null;
    private int imageT3Array[] = null;
    private double imageWhiteValueArray[] = null;
    private double imageRelFocusPosition1Array[] = null;
    private int imageZ4Array[] = null;
    private double imageRelFocusPosition2Array[] = null;
    private int imageZ5Array[] = null;
    private double cameraImageAcquisitionTime[] = null;
    private int imageZ6Array[] = null;
    private int imageC6Array[] = null;
    private int imageT6Array[] = null;
    private double imageRelativeTime[] = null;
    private int imageZ7Array[] = null;
    private int imageC7Array[] = null;
    private int imageT7Array[] = null;
    // image count pointer for imageFocusPositionArray
    private int icp = 0;
    // image count pointer for imageBlackValueArray
    private int icp2 = 0;
    // image count pointer for imageWhiteValueArray 
    private int icp3 = 0;
    // image count pointer for imageRelFocusPosition1Array
    private int icp4 = 0;
    // image count pointer for imageRelFocusPosition2Array
    private int icp5 = 0;
    // image count pointer for cameraImageAcquisitionTime
    private int icp6 = 0;
    // image count pointer for imageRelativeTime
    private int icp7 = 0;
    // image count pointer for imageStagePositionX
    private int icpX = 0;
    // image count pointer for imageStagePositionY
    private int icpY = 0;
    
    // Sector allocation table
    private int sat[] = null;
    
    private boolean add128 = false;
    
    private int minC = Integer.MAX_VALUE;
    
    private int maxC = Integer.MIN_VALUE;
    
    private int minZ = Integer.MAX_VALUE;
    
    private int maxZ = Integer.MIN_VALUE;
    
    private int minT = Integer.MAX_VALUE;
    
    private int maxT = Integer.MIN_VALUE;
    
    private int minPosition = Integer.MAX_VALUE;
    
    private int maxPosition = Integer.MIN_VALUE;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * ZVI reader constructor.
     *
     * @param      fileName  file name
     * @param      fileDir   file directory
     *
     * @exception  IOException  if there is an error making the file
     */
    public FileZVI(String fileName, String fileDir) throws IOException {

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
        fileInfo = null;
        file = null;
        image = null;
        imageExtents = null;
        imgBuffer = null;
        imgResols = null;
        LUT = null;
        zArray = null;
        cArray = null;
        tArray = null;
        positionArray = null;
        imageFocusPositionArray = null;
        imageStagePositionXArray = null;
        imageZXArray = null;
        imageStagePositionYArray = null;
        imageZYArray = null;
        shortSectorTable = null;
        startSectorArray = null;
        offsetArray = null;
        imageZArray = null;
        imageZ2Array = null;
        imageC2Array = null;
        imageT2Array = null;
        imageBlackValueArray = null;
        imageZ3Array = null;
        imageC3Array = null;
        imageT3Array = null;
        imageWhiteValueArray = null;
        imageZ4Array = null;
        imageRelFocusPosition1Array = null;
        imageZ5Array = null;
        imageRelFocusPosition2Array = null;
        cameraImageAcquisitionTime = null;
        imageZ6Array = null;
        imageC6Array = null;
        imageT6Array = null;
        imageRelativeTime = null;
        imageZ7Array = null;
        imageC7Array = null;
        imageT7Array = null;
        sat = null;
        
        try {
            super.finalize();
        } catch (Throwable er) { }
    }
    
    /**
     * Accessor that returns the file info.
     *
     * @return  FileInfoBase containing the file info
     */
    public FileInfoBase getFileInfo() {
        return fileInfo;
    }


    /**
     * Accessor that returns the image buffer.
     *
     * @return  buffer of image.
     */
    public float[] getImageBuffer() {
        return imgBuffer;
    }

    /**
     * Rreturns LUT if defined.
     *
     * @return  the LUT if defined else it is null
     */
    public ModelLUT getModelLUT() {
        return LUT;
    }
    
    /**
     * Reads the LIFF header which indicates endianess, the TIFF magic number, and the offset in bytes of the first IFD.
     * It then reads all the IFDs. This method then opens a Model of an image and imports the the images one slice at a
     * time. Image slices are separated by an IFD.
     *
     * @param      multiFile  <code>true</code> if a set of files each containing a separate 2D image is present <code>
     *                        false</code> if one file with either a 2D image or a stack of 2D images
     * @param      one        <code>true</code> if only want to read in one image of the 3D set
     *
     * @return     returns the image
     *
     * @exception  IOException  if there is an error reading the file
     */
    public ModelImage readImage(boolean multiFile, boolean one) throws IOException {
        int i, j;
        int sliceBytes;
        int bytesToRead;
        int bytesRead;
        int dataType;
        int t;
        int z;
        int c;
        byte byteBuffer[] = null;
        byte byteBuffer2[] = null;
        short shortBuffer[] = null;
        int intBuffer[] = null;
        float floatBuffer[] = null;
        double doubleBuffer[] = null;
        long tmpLong;
        int tmpInt;
        int index;
        int b1 = 0;
        int b2 = 0;
        int b3 = 0;
        int b4 = 0;
        int b5 = 0;
        int b6 = 0;
        int b7 = 0;
        int b8 = 0;
        int sliceSize;
        int presentSector;
        double processedFocusPositionArray[] = null;
        double processedRelFocusPosition1Array[] = null;
        double processedRelFocusPosition2Array[] = null;
        double processedStagePositionXArray[] = null;
        double processedStagePositionYArray[] = null;
        double blackValue0Array[] = null;
        double blackValue1Array[] = null;
        double blackValue2Array[] = null;
        double blackValue3Array[] = null;
        double whiteValue0Array[] = null;
        double whiteValue1Array[] = null;
        double whiteValue2Array[] = null;
        double whiteValue3Array[] = null;
        double cameraImageAcquisitionTime0[] = null;
        double cameraImageAcquisitionTime1[] = null;
        double cameraImageAcquisitionTime2[] = null;
        double cameraImageAcquisitionTime3[] = null;
        double imageRelativeTime0[] = null;
        double imageRelativeTime1[] = null;
        double imageRelativeTime2[] = null;
        double imageRelativeTime3[] = null;
        int numberFocusPositions = 0;
        int numberRelFocusPosition1s = 0;
        int numberRelFocusPosition2s = 0;
        int numberStagePositionXs = 0;
        int numberStagePositionYs = 0;
        int numberBlack0Values = 0;
        int numberBlack1Values = 0;
        int numberBlack2Values = 0;
        int numberBlack3Values = 0;
        int numberWhite0Values = 0;
        int numberWhite1Values = 0;
        int numberWhite2Values = 0;
        int numberWhite3Values = 0;
        int numberAcqTime0Values = 0;
        int numberAcqTime1Values = 0;
        int numberAcqTime2Values = 0;
        int numberAcqTime3Values = 0;
        int numberImageRelativeTime0Values = 0;
        int numberImageRelativeTime1Values = 0;
        int numberImageRelativeTime2Values = 0;
        int numberImageRelativeTime3Values = 0;
        boolean haveFirstChannel = false;
        boolean haveSecondChannel = false;
        boolean haveThirdChannel = false;
        boolean haveFourthChannel = false;
        int ch0 = Integer.MIN_VALUE;
        int ch1 = Integer.MIN_VALUE;
        int ch2 = Integer.MIN_VALUE;
        int ch3 = Integer.MIN_VALUE;
        FileInfoBase fInfo[] = null;
        
        try {
            
            imgResols[0] = imgResols[1] = imgResols[2] = imgResols[3] = imgResols[4] = (float) 1.0;
            file = new File(fileDir + fileName);
            raFile = new RandomAccessFile(file, "r");
            
            fileInfo = new FileInfoZVI(fileName, fileDir, FileUtility.ZVI); // dummy fileInfo
            fileInfo.setEndianess(FileBase.LITTLE_ENDIAN);
            endianess = FileBase.LITTLE_ENDIAN;
            
            readHeader();
            
            for (i = 0; i < imageCount; i++) {
                if (cArray[i] < minC) {
                    minC = cArray[i];
                }
                if (cArray[i] > maxC) {
                    maxC = cArray[i];
                }
                if (zArray[i] < minZ) {
                    minZ = zArray[i];
                }
                if (zArray[i] > maxZ) {
                    maxZ = zArray[i];
                }
                if (tArray[i] < minT) {
                    minT = tArray[i];
                }
                if (tArray[i] > maxT) {
                    maxT = tArray[i];
                }
                
                if (positionArray[i] < minPosition) {
                    minPosition = positionArray[i];
                }
                
                if (positionArray[i] > maxPosition) {
                    maxPosition = positionArray[i];
                }
            }
            
            channelNumber = maxC - minC + 1;
            zDim = maxZ - minZ + 1;
            positionNumber = maxPosition - minPosition + 1;
            if ((zDim == 1) && (positionNumber > 1)) {
                zDim = positionNumber;
                minZ = minPosition;
                maxZ = maxPosition; 
                for (i = 0; i < zDim; i++) {
                    zArray[i] = positionArray[i];
                }
            }
            if ((zDim == 1) && (backupZDim > 1)) {
                zDim = backupZDim;
                minZ = 0;
                maxZ = zDim - 1;
                for (i = 0; i < zDim; i++) {
                    zArray[i] = i;
                }
            }
            tDim = maxT - minT + 1;
            
            processedFocusPositionArray = new double[zDim];
            numberFocusPositions = 0;
            for (z = minZ; z <= maxZ; z++) {
                for (i = 0; i < imageCount; i++) {
                    if ((imageZArray[i] == z) && (!Double.isNaN(imageFocusPositionArray[i]))) {
                        processedFocusPositionArray[numberFocusPositions++] = imageFocusPositionArray[i];    
                    }
                }
            }
            
            processedRelFocusPosition1Array = new double[zDim];
            numberRelFocusPosition1s = 0;
            for (z = minZ; z <= maxZ; z++) {
                for (i = 0; i < imageCount; i++) {
                    if ((imageZ4Array[i] == z) && (!Double.isNaN(imageRelFocusPosition1Array[i]))) {
                        processedRelFocusPosition1Array[numberRelFocusPosition1s++] = imageRelFocusPosition1Array[i];    
                    }
                }
            }
            
            processedRelFocusPosition2Array = new double[zDim];
            numberRelFocusPosition2s = 0;
            for (z = minZ; z <= maxZ; z++) {
                for (i = 0; i < imageCount; i++) {
                    if ((imageZ5Array[i] == z) && (!Double.isNaN(imageRelFocusPosition2Array[i]))) {
                        processedRelFocusPosition2Array[numberRelFocusPosition2s++] = imageRelFocusPosition2Array[i];    
                    }
                }
            }
            
            processedStagePositionXArray = new double[zDim];
            numberStagePositionXs = 0;
            for (z = minZ; z <= maxZ; z++) {
                for (i = 0; i < imageCount; i++) {
                    if ((imageZXArray[i] == z) && (!Double.isNaN(imageStagePositionXArray[i]))) {
                        processedStagePositionXArray[numberStagePositionXs++] = imageStagePositionXArray[i];    
                    }
                }
            }
            
            processedStagePositionYArray = new double[zDim];
            numberStagePositionYs = 0;
            for (z = minZ; z <= maxZ; z++) {
                for (i = 0; i < imageCount; i++) {
                    if ((imageZYArray[i] == z) && (!Double.isNaN(imageStagePositionYArray[i]))) {
                        processedStagePositionYArray[numberStagePositionYs++] = imageStagePositionYArray[i];    
                    }
                }
            }
            
            for (i = 0; i < imageCount; i++) {
                if (imageC2Array[i] != Integer.MIN_VALUE) {
                   if (!haveFirstChannel) {
                       haveFirstChannel = true;
                       ch0 = imageC2Array[i];
                   }
                   else if ((haveFirstChannel) && (ch0 == imageC2Array[i])) {
                       
                   }
                   else if (!haveSecondChannel) {
                       haveSecondChannel = true;
                       ch1 = imageC2Array[i];
                   }
                   else if ((haveSecondChannel) && (ch1 == imageC2Array[i])) {
                       
                   }
                   else if (!haveThirdChannel) {
                       haveThirdChannel = true;
                       ch2 = imageC2Array[i];
                   }
                   else if ((haveThirdChannel) && (ch2 == imageC2Array[i])) {
                       
                   }
                   else if (!haveFourthChannel) {
                       haveFourthChannel = true;
                       ch3 = imageC2Array[i];
                   }
                }
            }
            
            for (i = 0; i < imageCount; i++) {
                if (imageC3Array[i] != Integer.MIN_VALUE) {
                   if (!haveFirstChannel) {
                       haveFirstChannel = true;
                       ch0 = imageC3Array[i];
                   }
                   else if ((haveFirstChannel) && (ch0 == imageC3Array[i])) {
                       
                   }
                   else if (!haveSecondChannel) {
                       haveSecondChannel = true;
                       ch1 = imageC3Array[i];
                   }
                   else if ((haveSecondChannel) && (ch1 == imageC3Array[i])) {
                       
                   }
                   else if (!haveThirdChannel) {
                       haveThirdChannel = true;
                       ch2 = imageC3Array[i];
                   }
                   else if ((haveThirdChannel) && (ch2 == imageC3Array[i])) {
                       
                   }
                   else if (!haveFourthChannel) {
                       haveFourthChannel = true;
                       ch3 = imageC3Array[i];
                   }
                }
            }
            
            for (i = 0; i < imageCount; i++) {
                if (imageC6Array[i] != Integer.MIN_VALUE) {
                   if (!haveFirstChannel) {
                       haveFirstChannel = true;
                       ch0 = imageC6Array[i];
                   }
                   else if ((haveFirstChannel) && (ch0 == imageC6Array[i])) {
                       
                   }
                   else if (!haveSecondChannel) {
                       haveSecondChannel = true;
                       ch1 = imageC6Array[i];
                   }
                   else if ((haveSecondChannel) && (ch1 == imageC6Array[i])) {
                       
                   }
                   else if (!haveThirdChannel) {
                       haveThirdChannel = true;
                       ch2 = imageC6Array[i];
                   }
                   else if ((haveThirdChannel) && (ch2 == imageC6Array[i])) {
                       
                   }
                   else if (!haveFourthChannel) {
                       haveFourthChannel = true;
                       ch3 = imageC6Array[i];
                   }
                }
            }
            
            for (i = 0; i < imageCount; i++) {
                if (imageC7Array[i] != Integer.MIN_VALUE) {
                   if (!haveFirstChannel) {
                       haveFirstChannel = true;
                       ch0 = imageC7Array[i];
                   }
                   else if ((haveFirstChannel) && (ch0 == imageC7Array[i])) {
                       
                   }
                   else if (!haveSecondChannel) {
                       haveSecondChannel = true;
                       ch1 = imageC7Array[i];
                   }
                   else if ((haveSecondChannel) && (ch1 == imageC7Array[i])) {
                       
                   }
                   else if (!haveThirdChannel) {
                       haveThirdChannel = true;
                       ch2 = imageC7Array[i];
                   }
                   else if ((haveThirdChannel) && (ch2 == imageC7Array[i])) {
                       
                   }
                   else if (!haveFourthChannel) {
                       haveFourthChannel = true;
                       ch3 = imageC7Array[i];
                   }
                }
            }
            
            if (ch0 != Integer.MIN_VALUE) {
                fileInfo.setChannel0(ch0);
            }
            
            if (ch1 != Integer.MIN_VALUE) {
                fileInfo.setChannel1(ch1);
            }
            
            if (ch2 != Integer.MIN_VALUE) {
                fileInfo.setChannel2(ch2);
            }
            
            if (ch3 != Integer.MIN_VALUE) {
                fileInfo.setChannel3(ch3);
            }
            
            blackValue0Array = new double[zDim*tDim];
            blackValue1Array = new double[zDim*tDim];
            blackValue2Array = new double[zDim*tDim];
            blackValue3Array = new double[zDim*tDim];
            
            numberBlack0Values = 0;
            for (t = minT; t <= maxT; t++) {
                for (z = minZ; z <= maxZ; z++) {
                    for (i = 0; i < imageCount; i++) {
                        if((imageT2Array[i] == t) && (imageZ2Array[i] == z) && 
                           (imageC2Array[i] == ch0) && (!Double.isNaN(imageBlackValueArray[i]))) {
                                blackValue0Array[numberBlack0Values++] = imageBlackValueArray[i];
                        } // if((imageT2Array[i] == t) && (imageZ2Array[i] == z)
                    } // for (i = 0; i < imageCount; i++)
                } // for (z = minZ; z <= maxZ; z++)
            } // for (t = minT; t <= maxT; t++)
            
            numberBlack1Values = 0;
            for (t = minT; t <= maxT; t++) {
                for (z = minZ; z <= maxZ; z++) {
                    for (i = 0; i < imageCount; i++) {
                        if((imageT2Array[i] == t) && (imageZ2Array[i] == z) && 
                           (imageC2Array[i] == ch1) && (!Double.isNaN(imageBlackValueArray[i]))) {
                                blackValue1Array[numberBlack1Values++] = imageBlackValueArray[i];
                        } // if((imageT2Array[i] == t) && (imageZ2Array[i] == z)
                    } // for (i = 0; i < imageCount; i++)
                } // for (z = minZ; z <= maxZ; z++)
            } // for (t = minT; t <= maxT; t++)
            
            numberBlack2Values = 0;
            for (t = minT; t <= maxT; t++) {
                for (z = minZ; z <= maxZ; z++) {
                    for (i = 0; i < imageCount; i++) {
                        if((imageT2Array[i] == t) && (imageZ2Array[i] == z) && 
                           (imageC2Array[i] == ch2) && (!Double.isNaN(imageBlackValueArray[i]))) {
                                blackValue2Array[numberBlack2Values++] = imageBlackValueArray[i];
                        } // if((imageT2Array[i] == t) && (imageZ2Array[i] == z)
                    } // for (i = 0; i < imageCount; i++)
                } // for (z = minZ; z <= maxZ; z++)
            } // for (t = minT; t <= maxT; t++)
            
            numberBlack3Values = 0;
            for (t = minT; t <= maxT; t++) {
                for (z = minZ; z <= maxZ; z++) {
                    for (i = 0; i < imageCount; i++) {
                        if((imageT2Array[i] == t) && (imageZ2Array[i] == z) && 
                           (imageC2Array[i] == ch3) && (!Double.isNaN(imageBlackValueArray[i]))) {
                                blackValue3Array[numberBlack3Values++] = imageBlackValueArray[i];
                        } // if((imageT2Array[i] == t) && (imageZ2Array[i] == z)
                    } // for (i = 0; i < imageCount; i++)
                } // for (z = minZ; z <= maxZ; z++)
            } // for (t = minT; t <= maxT; t++)
            
            whiteValue0Array = new double[zDim*tDim];
            whiteValue1Array = new double[zDim*tDim];
            whiteValue2Array = new double[zDim*tDim];
            whiteValue3Array = new double[zDim*tDim];
            
            numberWhite0Values = 0;
            for (t = minT; t <= maxT; t++) {
                for (z = minZ; z <= maxZ; z++) {
                    for (i = 0; i < imageCount; i++) {
                        if((imageT3Array[i] == t) && (imageZ3Array[i] == z) && 
                           (imageC3Array[i] == ch0) && (!Double.isNaN(imageWhiteValueArray[i]))) {
                                whiteValue0Array[numberWhite0Values++] = imageWhiteValueArray[i];
                        } // if((imageT3Array[i] == t) && (imageZ3Array[i] == z)
                    } // for (i = 0; i < imageCount; i++)
                } // for (z = minZ; z <= maxZ; z++)
            } // for (t = minT; t <= maxT; t++)
            
            numberWhite1Values = 0;
            for (t = minT; t <= maxT; t++) {
                for (z = minZ; z <= maxZ; z++) {
                    for (i = 0; i < imageCount; i++) {
                        if((imageT3Array[i] == t) && (imageZ3Array[i] == z) && 
                           (imageC3Array[i] == ch1) && (!Double.isNaN(imageWhiteValueArray[i]))) {
                                whiteValue1Array[numberWhite1Values++] = imageWhiteValueArray[i];
                        } // if((imageT3Array[i] == t) && (imageZ3Array[i] == z)
                    } // for (i = 0; i < imageCount; i++)
                } // for (z = minZ; z <= maxZ; z++)
            } // for (t = minT; t <= maxT; t++)
            
            numberWhite2Values = 0;
            for (t = minT; t <= maxT; t++) {
                for (z = minZ; z <= maxZ; z++) {
                    for (i = 0; i < imageCount; i++) {
                        if((imageT3Array[i] == t) && (imageZ3Array[i] == z) && 
                           (imageC3Array[i] == ch2) && (!Double.isNaN(imageWhiteValueArray[i]))) {
                                whiteValue2Array[numberWhite2Values++] = imageWhiteValueArray[i];
                        } // if((imageT3Array[i] == t) && (imageZ3Array[i] == z)
                    } // for (i = 0; i < imageCount; i++)
                } // for (z = minZ; z <= maxZ; z++)
            } // for (t = minT; t <= maxT; t++)
            
            numberWhite3Values = 0;
            for (t = minT; t <= maxT; t++) {
                for (z = minZ; z <= maxZ; z++) {
                    for (i = 0; i < imageCount; i++) {
                        if((imageT3Array[i] == t) && (imageZ3Array[i] == z) && 
                           (imageC3Array[i] == ch3) && (!Double.isNaN(imageWhiteValueArray[i]))) {
                                whiteValue3Array[numberWhite3Values++] = imageWhiteValueArray[i];
                        } // if((imageT3Array[i] == t) && (imageZ3Array[i] == z)
                    } // for (i = 0; i < imageCount; i++)
                } // for (z = minZ; z <= maxZ; z++)
            } // for (t = minT; t <= maxT; t++)
            
            cameraImageAcquisitionTime0 = new double[zDim * tDim];
            cameraImageAcquisitionTime1 = new double[zDim * tDim];
            cameraImageAcquisitionTime2 = new double[zDim * tDim];
            cameraImageAcquisitionTime3 = new double[zDim * tDim];
            
            numberAcqTime0Values = 0;
            for (t = minT; t <= maxT; t++) {
                for (z = minZ; z <= maxZ; z++) {
                    for (i = 0; i < imageCount; i++) {
                        if((imageT6Array[i] == t) && (imageZ6Array[i] == z) && 
                           (imageC6Array[i] == ch0) && (!Double.isNaN(cameraImageAcquisitionTime[i]))) {
                                cameraImageAcquisitionTime0[numberAcqTime0Values++] = cameraImageAcquisitionTime[i];
                        } // if((imageT6Array[i] == t) && (imageZ6Array[i] == z)
                    } // for (i = 0; i < imageCount; i++)
                } // for (z = minZ; z <= maxZ; z++)
            } // for (t = minT; t <= maxT; t++)
            
            numberAcqTime1Values = 0;
            for (t = minT; t <= maxT; t++) {
                for (z = minZ; z <= maxZ; z++) {
                    for (i = 0; i < imageCount; i++) {
                        if((imageT6Array[i] == t) && (imageZ6Array[i] == z) && 
                           (imageC6Array[i] == ch1) && (!Double.isNaN(cameraImageAcquisitionTime[i]))) {
                                cameraImageAcquisitionTime1[numberAcqTime1Values++] = cameraImageAcquisitionTime[i];
                        } // if((imageT6Array[i] == t) && (imageZ6Array[i] == z)
                    } // for (i = 0; i < imageCount; i++)
                } // for (z = minZ; z <= maxZ; z++)
            } // for (t = minT; t <= maxT; t++)
            
            numberAcqTime2Values = 0;
            for (t = minT; t <= maxT; t++) {
                for (z = minZ; z <= maxZ; z++) {
                    for (i = 0; i < imageCount; i++) {
                        if((imageT6Array[i] == t) && (imageZ6Array[i] == z) && 
                           (imageC6Array[i] == ch2) && (!Double.isNaN(cameraImageAcquisitionTime[i]))) {
                                cameraImageAcquisitionTime2[numberAcqTime2Values++] = cameraImageAcquisitionTime[i];
                        } // if((imageT6Array[i] == t) && (imageZ6Array[i] == z)
                    } // for (i = 0; i < imageCount; i++)
                } // for (z = minZ; z <= maxZ; z++)
            } // for (t = minT; t <= maxT; t++)
            
            numberAcqTime3Values = 0;
            for (t = minT; t <= maxT; t++) {
                for (z = minZ; z <= maxZ; z++) {
                    for (i = 0; i < imageCount; i++) {
                        if((imageT6Array[i] == t) && (imageZ6Array[i] == z) && 
                           (imageC6Array[i] == ch3) && (!Double.isNaN(cameraImageAcquisitionTime[i]))) {
                                cameraImageAcquisitionTime3[numberAcqTime3Values++] = cameraImageAcquisitionTime[i];
                        } // if((imageT6Array[i] == t) && (imageZ6Array[i] == z)
                    } // for (i = 0; i < imageCount; i++)
                } // for (z = minZ; z <= maxZ; z++)
            } // for (t = minT; t <= maxT; t++)
            
            imageRelativeTime0 = new double[zDim * tDim];
            imageRelativeTime1 = new double[zDim * tDim];
            imageRelativeTime2 = new double[zDim * tDim];
            imageRelativeTime3 = new double[zDim * tDim];
            
            numberImageRelativeTime0Values = 0;
            for (t = minT; t <= maxT; t++) {
                for (z = minZ; z <= maxZ; z++) {
                    for (i = 0; i < imageCount; i++) {
                        if((imageT7Array[i] == t) && (imageZ7Array[i] == z) && 
                           (imageC7Array[i] == ch0) && (!Double.isNaN(imageRelativeTime[i]))) {
                                imageRelativeTime0[numberImageRelativeTime0Values++] = imageRelativeTime[i];
                        } // if((imageT7Array[i] == t) && (imageZ7Array[i] == z)
                    } // for (i = 0; i < imageCount; i++)
                } // for (z = minZ; z <= maxZ; z++)
            } // for (t = minT; t <= maxT; t++)
            
            numberImageRelativeTime1Values = 0;
            for (t = minT; t <= maxT; t++) {
                for (z = minZ; z <= maxZ; z++) {
                    for (i = 0; i < imageCount; i++) {
                        if((imageT7Array[i] == t) && (imageZ7Array[i] == z) && 
                           (imageC7Array[i] == ch1) && (!Double.isNaN(imageRelativeTime[i]))) {
                                imageRelativeTime1[numberImageRelativeTime1Values++] = imageRelativeTime[i];
                        } // if((imageT7Array[i] == t) && (imageZ7Array[i] == z)
                    } // for (i = 0; i < imageCount; i++)
                } // for (z = minZ; z <= maxZ; z++)
            } // for (t = minT; t <= maxT; t++)
            
            numberImageRelativeTime2Values = 0;
            for (t = minT; t <= maxT; t++) {
                for (z = minZ; z <= maxZ; z++) {
                    for (i = 0; i < imageCount; i++) {
                        if((imageT7Array[i] == t) && (imageZ7Array[i] == z) && 
                           (imageC7Array[i] == ch2) && (!Double.isNaN(imageRelativeTime[i]))) {
                                imageRelativeTime2[numberImageRelativeTime2Values++] = imageRelativeTime[i];
                        } // if((imageT7Array[i] == t) && (imageZ7Array[i] == z)
                    } // for (i = 0; i < imageCount; i++)
                } // for (z = minZ; z <= maxZ; z++)
            } // for (t = minT; t <= maxT; t++)
            
            numberImageRelativeTime3Values = 0;
            for (t = minT; t <= maxT; t++) {
                for (z = minZ; z <= maxZ; z++) {
                    for (i = 0; i < imageCount; i++) {
                        if((imageT7Array[i] == t) && (imageZ7Array[i] == z) && 
                           (imageC7Array[i] == ch3) && (!Double.isNaN(imageRelativeTime[i]))) {
                                imageRelativeTime3[numberImageRelativeTime3Values++] = imageRelativeTime[i];
                        } // if((imageT7Array[i] == t) && (imageZ7Array[i] == z)
                    } // for (i = 0; i < imageCount; i++)
                } // for (z = minZ; z <= maxZ; z++)
            } // for (t = minT; t <= maxT; t++)
            
            if ((zDim > 1) && (tDim > 1)) {
                imageExtents = new int[4];
                imageExtents[2] = zDim;
                imageExtents[3] = tDim;
            }
            else if (tDim > 1) {
                imageExtents = new int[3];
                imageExtents[2] = tDim;
            }
            else if (zDim > 1) {
                imageExtents = new int[3];
                imageExtents[2] = zDim;
            }
            else {
                imageExtents = new int[2];
            }
            imageExtents[0] = imageWidth;
            imageExtents[1] = imageHeight;
            for (i = 0; i < imageExtents.length; i++) {
                Preferences.debug("extents[" + i + "] = " + imageExtents[i] + "\n", Preferences.DEBUG_FILEIO);
            }
            sliceSize = imageExtents[0] * imageExtents[1];
            
            Preferences.debug("minC = " + minC + " maxC = " + maxC + "\n", Preferences.DEBUG_FILEIO);
            Preferences.debug("Channel number = " + channelNumber + "\n", Preferences.DEBUG_FILEIO);

            fileInfo.setExtents(imageExtents);
            
            switch (imagePixelFormat) {
                case 1: // 8-bit BGR 3 bytes/pixel
                    dataType = ModelStorageBase.ARGB;
                    Preferences.debug("Data type = ARGB\n", Preferences.DEBUG_FILEIO);
                    sliceBytes = 3*sliceSize;
                    byteBuffer = new byte[3*sliceSize];
                    byteBuffer2 = new byte[4*sliceSize];
                    break;
                case 2: // 8-bit BGRA 4 bytes/pixel
                    dataType = ModelStorageBase.ARGB;
                    Preferences.debug("Data type = ARGB\n", Preferences.DEBUG_FILEIO);
                    sliceBytes = 4*sliceSize;
                    byteBuffer = new byte[4*sliceSize];
                    byteBuffer2 = new byte[4*sliceSize];
                    break;
                case 3: // 8-bit grayscale
                    if (channelNumber == 1) {
                        dataType = ModelStorageBase.UBYTE;
                        Preferences.debug("Data type = UBYTE\n", Preferences.DEBUG_FILEIO);
                        sliceBytes = sliceSize;
                        byteBuffer = new byte[sliceSize];
                    }
                    else {
                        dataType = ModelStorageBase.ARGB;
                        Preferences.debug("Data type = ARGB\n", Preferences.DEBUG_FILEIO);
                        sliceBytes = sliceSize;
                        byteBuffer = new byte[sliceSize];
                        byteBuffer2 = new byte[4*sliceSize];
                    }
                    break;
                case 4: // 16-bit integer
                    if (channelNumber == 1) {
                        dataType = ModelStorageBase.SHORT;
                        Preferences.debug("Data type = SHORT\n", Preferences.DEBUG_FILEIO);
                        sliceBytes = 2*sliceSize;
                        byteBuffer = new byte[2*sliceSize];
                        shortBuffer = new short[sliceSize];
                    }
                    else {
                        dataType = ModelStorageBase.ARGB_USHORT;
                        Preferences.debug("Data type = ARGB_USHORT\n", Preferences.DEBUG_FILEIO);
                        sliceBytes = 2*sliceSize;
                        byteBuffer = new byte[2*sliceSize];
                        shortBuffer = new short[4*sliceSize];
                    }
                    break;
                case 5: // 32-bit integer - 4 bytes/pixel
                    dataType = ModelStorageBase.INTEGER;
                    Preferences.debug("Data type = INTEGER\n", Preferences.DEBUG_FILEIO);
                    sliceBytes = 4*sliceSize;
                    byteBuffer = new byte[4*sliceSize];
                    intBuffer = new int[sliceSize];
                    break;
                case 6: // 32-bit IEEE float - 4 bytes/pixel
                    dataType = ModelStorageBase.FLOAT;
                    Preferences.debug("Data type = FLOAT\n", Preferences.DEBUG_FILEIO);
                    sliceBytes = 4*sliceSize;
                    byteBuffer = new byte[4*sliceSize];
                    floatBuffer = new float[sliceSize];
                    break;
                case 7: // 64-bit IEEE float - 8 bytes/pixel
                    dataType = ModelStorageBase.DOUBLE;
                    Preferences.debug("Data type = DOUBLE\n", Preferences.DEBUG_FILEIO);
                    sliceBytes = 8*sliceSize;
                    byteBuffer = new byte[8*sliceSize];
                    doubleBuffer = new double[sliceSize];
                    break;
                case 8: // 16-bit BGR - 6 bytes/pixel
                    dataType = ModelStorageBase.ARGB_USHORT;
                    Preferences.debug("Data type = ARGB_USHORT\n", Preferences.DEBUG_FILEIO);
                    sliceBytes = 6*sliceSize;
                    byteBuffer = new byte[6*sliceSize];
                    shortBuffer = new short[4*sliceSize];
                    break;
                /*case 9: // 32-bit integer triples (B, G, R) - 12 bytes/pixel
                    Preferences.debug("Data type = ARGB_UINT\n", Preferences.DEBUG_FILEIO);
                    sliceBytes = 12*sliceSize;
                    byteBuffer = new byte[12*sliceSize];
                    intBuffer = new int[4*sliceSize];
                    break;*/
                default:
                    dataType = ModelStorageBase.UBYTE;
                    sliceBytes = sliceSize;
            }
            fileInfo.setDataType(dataType);
            
            image = new ModelImage(dataType, imageExtents, fileName);
            
            for (t = minT; t <= maxT; t++) {
                for (z = minZ; z <= maxZ; z++) {
                    fireProgressStateChanged(((t-minT)*zDim + (z-minZ))*100/(tDim*zDim));
                    for (c = minC; c <= maxC; c++) {
                        for (i = 0; i < imageCount; i++) {
                            if ((cArray[i] == c) && (zArray[i] == z) && (tArray[i] == t)) {
                                presentSector = startSectorArray[i];
                                bytesToRead = sliceBytes;
                                bytesRead = 0;
                                if (add128) {
                                    raFile.seek((presentSector+1)*sectorSize + 128 + offsetArray[i]);
                                }
                                else {
                                    raFile.seek((presentSector+1)*sectorSize + offsetArray[i]);   
                                }
                                raFile.read(byteBuffer, 0, Math.min(sectorSize-offsetArray[i], bytesToRead));
                                bytesRead += Math.min(sectorSize-offsetArray[i], bytesToRead);
                                bytesToRead -= Math.min(sectorSize-offsetArray[i], bytesToRead);
                                presentSector = sat[presentSector];
                                while ((bytesToRead > 0) && (presentSector >= 0)) {
                                    if (add128) {
                                        raFile.seek((presentSector+1)*sectorSize + 128);
                                    }
                                    else {
                                        raFile.seek((presentSector+1)*sectorSize);    
                                    }
                                    raFile.read(byteBuffer, bytesRead, Math.min(sectorSize, bytesToRead));
                                    bytesRead += Math.min(sectorSize, bytesToRead);
                                    bytesToRead -= Math.min(sectorSize, bytesToRead);
                                    presentSector = sat[presentSector];
                                }    
                                
                                switch (imagePixelFormat) {
                                    case 1: // 8-bit BGR 3 bytes/pixel
                                        for (j = 0; j < sliceSize; j++) {
                                            byteBuffer2[4*j + 3] = byteBuffer[3*j];
                                            byteBuffer2[4*j + 2] = byteBuffer[3*j+1];
                                            byteBuffer2[4*j + 1] = byteBuffer[3*j+2];
                                        }
                                        break;
                                    case 2: // 8-bit BGRA 4 bytes/pixel
                                        for (j = 0; j < sliceSize; j++) {
                                            byteBuffer2[4*j+3] = byteBuffer[4*j];
                                            byteBuffer2[4*j+2] = byteBuffer[4*j+1];
                                            byteBuffer2[4*j+1] = byteBuffer[4*j+2];
                                            byteBuffer2[4*j] = byteBuffer[4*j+3];
                                        }
                                        break;
                                    case 3: // 8-bit grayscale
                                        if (channelNumber > 1) {
                                            for (j = 0; j < sliceSize; j++) {
                                                byteBuffer2[4*j + (c - minC + 1)] = byteBuffer[j];    
                                            }    
                                        }
                                        break;
                                    case 4: // 16-bit integer
                                        if (channelNumber == 1) {
                                            for (j = 0, index = 0; j < sliceSize; j++) {
                                                b1 = byteBuffer[index++] & 0xff;
                                                b2 = byteBuffer[index++] & 0xff;
                                                shortBuffer[j] = (short) ((b2 << 8) | b1);
                                            }
                                        }
                                        else {
                                            for (j = 0, index = 0; j < sliceSize; j++) {
                                                b1 = byteBuffer[index++] & 0xff;
                                                b2 = byteBuffer[index++] & 0xff;
                                                shortBuffer[4*j + (c - minC + 1)] = (short) ((b2 << 8) | b1);
                                            }    
                                        }
                                        break;
                                    case 5: // 32-bit integer
                                        for (j = 0, index = 0; j < sliceSize; j++) {
                                            b1 = byteBuffer[index++] & 0xff;
                                            b2 = byteBuffer[index++] & 0xff;
                                            b3 = byteBuffer[index++] & 0xff;
                                            b4 = byteBuffer[index++] & 0xff;
                                            intBuffer[j] = ((b4 << 24) | (b3 << 16) | (b2 << 8) | b1);
                                        }
                                        break;
                                    case 6: // 32-bit IEEE float
                                        for (j = 0, index = 0; j < sliceSize; j++) {
                                            b1 = byteBuffer[index++] & 0xff;
                                            b2 = byteBuffer[index++] & 0xff;
                                            b3 = byteBuffer[index++] & 0xff;
                                            b4 = byteBuffer[index++] & 0xff;
                                            tmpInt = ((b4 << 24) | (b3 << 16) | (b2 << 8) | b1);

                                            floatBuffer[j] = Float.intBitsToFloat(tmpInt);
                                        }
                                        break;
                                    case 7: // 64-bit IEEE double
                                        for (j = 0, index = 0; j < sliceSize; j++) {
                                            b1 = byteBuffer[index++] & 0xff;
                                            b2 = byteBuffer[index++] & 0xff;
                                            b3 = byteBuffer[index++] & 0xff;
                                            b4 = byteBuffer[index++] & 0xff;
                                            b5 = byteBuffer[index++] & 0xff;
                                            b6 = byteBuffer[index++] & 0xff;
                                            b7 = byteBuffer[index++] & 0xff;
                                            b8 = byteBuffer[index++] & 0xff;
                                            tmpLong = (((long) b8 << 56) | ((long) b7 << 48) | ((long) b6 << 40) | ((long) b5 << 32) |
                                                           ((long) b4 << 24) | ((long) b3 << 16) | ((long) b2 << 8) | (long) b1);
                                            doubleBuffer[j] = Double.longBitsToDouble(tmpLong);
                                        }
                                        break;
                                    case 8: // 16-bit BGR - 6 bytes/pixel
                                        for (j = 0, index = 0; j < sliceSize; j++) {
                                            b1 = byteBuffer[index++] & 0xff;
                                            b2 = byteBuffer[index++] & 0xff;
                                            shortBuffer[4*j + 3] = (short) ((b2 << 8) | b1);
                                            b1 = byteBuffer[index++] & 0xff;
                                            b2 = byteBuffer[index++] & 0xff;
                                            shortBuffer[4*j + 2] = (short) ((b2 << 8) | b1);
                                            b1 = byteBuffer[index++] & 0xff;
                                            b2 = byteBuffer[index++] & 0xff;
                                            shortBuffer[4*j + 1] = (short) ((b2 << 8) | b1);
                                        }
                                        break;
                                }
                                // break out of i loop
                                break;
                            }
                        } // for (i = 0; i < si.length; i++)
                    } // for (c = minC; c <= maxC; c++)
                    switch (dataType) {
                        case ModelStorageBase.UBYTE:
                            image.importData((t-minT)*zDim*sliceSize + (z-minZ)*sliceSize, byteBuffer, false);
                            break;
                        case ModelStorageBase.SHORT:
                            image.importData((t-minT)*zDim*sliceSize + (z-minZ)*sliceSize, shortBuffer, false);
                            break;
                        case ModelStorageBase.INTEGER:
                            image.importData((t-minT)*zDim*sliceSize + (z-minZ)*sliceSize, intBuffer, false);
                            break;
                        case ModelStorageBase.FLOAT:
                            image.importData((t-minT)*zDim*sliceSize + (z-minZ)*sliceSize, floatBuffer, false);
                            break;
                        case ModelStorageBase.DOUBLE:
                            image.importData((t-minT)*zDim*sliceSize + (z-minZ)*sliceSize, doubleBuffer, false);
                            break;
                        case ModelStorageBase.ARGB:
                            image.importData(4*((t-minT)*zDim*sliceSize + (z-minZ)*sliceSize), byteBuffer2, false);
                            break;
                        case ModelStorageBase.ARGB_USHORT:
                            image.importData(4*((t-minT)*zDim*sliceSize + (z-minZ)*sliceSize), shortBuffer, false);
                            break;
                    }
                    
                } // for (z = minZ; z <= maxZ; z++)
            } // for (t = minT; t <= maxT; t++)
            
            if (image.getNDims() == 2) {
                image.setFileInfo(fileInfo, 0); // Otherwise just set the first fileInfo
            } else if (image.getNDims() == 3) { // If there is more than one image

                for (i = 0; i < imageExtents[2]; i++) {
                    FileInfoZVI newFileInfo = (FileInfoZVI) fileInfo.clone();
                    image.setFileInfo(newFileInfo, i); // Set the array of fileInfos in ModelImage
                }
            } else if (image.getNDims() == 4) { // If there is more than one image
                for (i = 0; i < (imageExtents[2] * imageExtents[3]); i++) {
                    FileInfoZVI newFileInfo = (FileInfoZVI) fileInfo.clone();
                    image.setFileInfo(newFileInfo, i); // Set the array of fileInfos in ModelImage
                }
            }
            
            fInfo = image.getFileInfo();
            if (numberFocusPositions == zDim) {
                for (t = 0; t < tDim; t++) {
                    for (z = 0; z < zDim; z++) {
                        index = z + t * zDim;
                        ((FileInfoZVI)fInfo[index]).setFocusPosition(processedFocusPositionArray[z]);
                    }
                }
            }
            
            if (numberRelFocusPosition1s == zDim) {
                for (t = 0; t < tDim; t++) {
                    for (z = 0; z < zDim; z++) {
                        index = z + t * zDim;
                        ((FileInfoZVI)fInfo[index]).setRelFocusPosition1(processedRelFocusPosition1Array[z]);
                    }
                }
            }
            
            if (numberRelFocusPosition2s == zDim) {
                for (t = 0; t < tDim; t++) {
                    for (z = 0; z < zDim; z++) {
                        index = z + t * zDim;
                        ((FileInfoZVI)fInfo[index]).setRelFocusPosition2(processedRelFocusPosition2Array[z]);
                    }
                }
            }
            
            if (numberStagePositionXs == zDim) {
                for (t = 0; t < tDim; t++) {
                    for (z = 0; z < zDim; z++) {
                        index = z + t * zDim;
                        ((FileInfoZVI)fInfo[index]).setStagePositionX(processedStagePositionXArray[z]);
                    }
                }
            }
            
            if (numberStagePositionYs == zDim) {
                for (t = 0; t < tDim; t++) {
                    for (z = 0; z < zDim; z++) {
                        index = z + t * zDim;
                        ((FileInfoZVI)fInfo[index]).setStagePositionY(processedStagePositionYArray[z]);
                    }
                }
            }
            
            if (numberBlack0Values == zDim * tDim) {
                for (t = 0; t < tDim; t++) {
                    for (z = 0; z < zDim; z++) {
                        index = z + t * zDim;
                        ((FileInfoZVI)fInfo[index]).setBlackValue0(blackValue0Array[index]);
                    }
                }    
            }
            
            if (numberBlack1Values == zDim * tDim) {
                for (t = 0; t < tDim; t++) {
                    for (z = 0; z < zDim; z++) {
                        index = z + t * zDim;
                        ((FileInfoZVI)fInfo[index]).setBlackValue1(blackValue1Array[index]);
                    }
                }    
            }
            
            if (numberBlack2Values == zDim * tDim) {
                for (t = 0; t < tDim; t++) {
                    for (z = 0; z < zDim; z++) {
                        index = z + t * zDim;
                        ((FileInfoZVI)fInfo[index]).setBlackValue2(blackValue2Array[index]);
                    }
                }    
            }
            
            if (numberBlack3Values == zDim * tDim) {
                for (t = 0; t < tDim; t++) {
                    for (z = 0; z < zDim; z++) {
                        index = z + t * zDim;
                        ((FileInfoZVI)fInfo[index]).setBlackValue3(blackValue3Array[index]);
                    }
                }    
            }
            
            if (numberWhite0Values == zDim * tDim) {
                for (t = 0; t < tDim; t++) {
                    for (z = 0; z < zDim; z++) {
                        index = z + t * zDim;
                        ((FileInfoZVI)fInfo[index]).setWhiteValue0(whiteValue0Array[index]);
                    }
                }    
            }
            
            if (numberWhite1Values == zDim * tDim) {
                for (t = 0; t < tDim; t++) {
                    for (z = 0; z < zDim; z++) {
                        index = z + t * zDim;
                        ((FileInfoZVI)fInfo[index]).setWhiteValue1(whiteValue1Array[index]);
                    }
                }    
            }
            
            if (numberWhite2Values == zDim * tDim) {
                for (t = 0; t < tDim; t++) {
                    for (z = 0; z < zDim; z++) {
                        index = z + t * zDim;
                        ((FileInfoZVI)fInfo[index]).setWhiteValue2(whiteValue2Array[index]);
                    }
                }    
            }
            
            if (numberWhite3Values == zDim * tDim) {
                for (t = 0; t < tDim; t++) {
                    for (z = 0; z < zDim; z++) {
                        index = z + t * zDim;
                        ((FileInfoZVI)fInfo[index]).setWhiteValue3(whiteValue3Array[index]);
                    }
                }    
            }
            
            if (numberAcqTime0Values == zDim * tDim) {
                for (t = 0; t < tDim; t++) {
                    for (z = 0; z < zDim; z++) {
                        index = z + t * zDim;
                        ((FileInfoZVI)fInfo[index]).setCameraImageAcquisitionTime0
                        (calculateVTDateTimeString(cameraImageAcquisitionTime0[index]));
                    }
                }    
            }
            
            if (numberAcqTime1Values == zDim * tDim) {
                for (t = 0; t < tDim; t++) {
                    for (z = 0; z < zDim; z++) {
                        index = z + t * zDim;
                        ((FileInfoZVI)fInfo[index]).setCameraImageAcquisitionTime1
                        (calculateVTDateTimeString(cameraImageAcquisitionTime1[index]));
                    }
                }    
            }
            
            if (numberAcqTime2Values == zDim * tDim) {
                for (t = 0; t < tDim; t++) {
                    for (z = 0; z < zDim; z++) {
                        index = z + t * zDim;
                        ((FileInfoZVI)fInfo[index]).setCameraImageAcquisitionTime2
                        (calculateVTDateTimeString(cameraImageAcquisitionTime2[index]));
                    }
                }    
            }
            
            if (numberAcqTime3Values == zDim * tDim) {
                for (t = 0; t < tDim; t++) {
                    for (z = 0; z < zDim; z++) {
                        index = z + t * zDim;
                        ((FileInfoZVI)fInfo[index]).setCameraImageAcquisitionTime3
                        (calculateVTDateTimeString(cameraImageAcquisitionTime3[index]));
                    }
                }    
            }
            
            if (numberImageRelativeTime0Values == zDim * tDim) {
                for (t = 0; t < tDim; t++) {
                    for (z = 0; z < zDim; z++) {
                        index = z + t * zDim;
                        ((FileInfoZVI)fInfo[index]).setImageRelativeTime0
                        (calculateRelativeTime(imageRelativeTime0[index]));
                    }
                }    
            }
            
            if (numberImageRelativeTime1Values == zDim * tDim) {
                for (t = 0; t < tDim; t++) {
                    for (z = 0; z < zDim; z++) {
                        index = z + t * zDim;
                        ((FileInfoZVI)fInfo[index]).setImageRelativeTime1
                        (calculateRelativeTime(imageRelativeTime1[index]));
                    }
                }    
            }
            
            if (numberImageRelativeTime2Values == zDim * tDim) {
                for (t = 0; t < tDim; t++) {
                    for (z = 0; z < zDim; z++) {
                        index = z + t * zDim;
                        ((FileInfoZVI)fInfo[index]).setImageRelativeTime2
                        (calculateRelativeTime(imageRelativeTime2[index]));
                    }
                }    
            }
            
            if (numberImageRelativeTime3Values == zDim * tDim) {
                for (t = 0; t < tDim; t++) {
                    for (z = 0; z < zDim; z++) {
                        index = z + t * zDim;
                        ((FileInfoZVI)fInfo[index]).setImageRelativeTime3
                        (calculateRelativeTime(imageRelativeTime3[index]));
                    }
                }    
            }
            
            image.calcMinMax(); 
            fireProgressStateChanged(100);
            
        } catch (OutOfMemoryError error) {

            if (image != null) {
                image.disposeLocal();
                image = null;
            }

            System.gc();
            throw error;
        }

        return image;
    }
    
    private void readHeader() throws IOException {
        int i;
        int j;
        byte[] b;
        int startSect;
        long streamSize;
        long pos;
        int sectorsIntoShortStream;
        int presentShortSector;
        int bytesToRead;
        int presentSector;
        int presentSectorOffset;
        int bytesRead;
        int bp;
        short dType;
        byte bf[];
        int imageDepth = 0;
        int imageValidBitsPerPixel = 0;
        int sliceBytes = 0;
        int validBitsPerPixel = 0;
        int count = 0;
        int pixelFormat = 0;
        int depth = 0;
        int width = 0;
        int height = 0;
        int pixelWidth = 0;
        long shortSectorTableAddress;
        long directoryStart;
        int intValue = 0;
        int stringBytes;
        @SuppressWarnings("unused")
        String stringValue = null;
        double doubleValue = 0.0;
        short shortValue;
        long tmpLong;
        boolean booleanValue = false;
        boolean readImagePixels;
        int measureUnits;
        double exposureTime = Double.NaN;;
        int apotomeGridPosition = Integer.MIN_VALUE;
        double focusPosition = Double.NaN;
        double relFocusPosition1 = Double.NaN;
        double relFocusPosition2 = Double.NaN;
        double stagePositionX = Double.NaN;
        double stagePositionY = Double.NaN;
        int zValue = Integer.MIN_VALUE;
        int cValue = Integer.MIN_VALUE;
        int tValue = Integer.MIN_VALUE;
        int tileValue = Integer.MIN_VALUE;
        int zTileValue;
        boolean haveFirstZValue = false;
        boolean haveSecondZValue = false;
        boolean haveFirstTileValue = false;
        boolean haveSecondTileValue = false;
        boolean useZValue = true;
        boolean useTileValue = false;
        int firstZValue = Integer.MIN_VALUE;
        int secondZValue = Integer.MIN_VALUE;
        int firstTileValue = Integer.MIN_VALUE;
        int secondTileValue = Integer.MIN_VALUE;
        double blackValue = Double.NaN;
        double whiteValue = Double.NaN;
        int reflectorPosition = Integer.MIN_VALUE;
        int multichannelColor = Integer.MIN_VALUE;
        int excitationWavelength = Integer.MIN_VALUE;
        int emissionWavelength = Integer.MIN_VALUE;
        double acqTime = Double.NaN;
        double relTime = Double.NaN;
        String lastElementName = null;
        String twoAgoElementName = null;
        //      Start reading ole compound file structure
        // The header is always 512 bytes long and should be located at offset zero.
        // Offset 0 Length 8 bytes olecf file signature
        long olecfFileSignature = getLong(endianess);
        if (olecfFileSignature == 0xe11ab1a1e011cfd0L) {
            Preferences.debug("Found olecf file signature at position 0\n", Preferences.DEBUG_FILEIO);
        }
        else {
            Preferences.debug("Instead of olecf file signature found = " + olecfFileSignature + " at position 0\n", 
            		Preferences.DEBUG_FILEIO);
            Preferences.debug("Look for file signature at position 128\n", Preferences.DEBUG_FILEIO);
            raFile.seek(128);
            olecfFileSignature = getLong(endianess);
            if (olecfFileSignature == 0xe11ab1a1e011cfd0L) {
                add128 = true;
                Preferences.debug("Found olecf file signature at position 128\n", Preferences.DEBUG_FILEIO);
            }
            else {
                Preferences.debug("Instead of olecf file signature found = " + olecfFileSignature + " at position 128\n", 
                		Preferences.DEBUG_FILEIO);
            }
        }
        
        // Location 8 Length 16 bytes class id
        getLong(endianess);
        getLong(endianess);
        // Location 24 Length 2 bytes Minor version of the format: 33 is written by reference implementation
        int minorVersion = getUnsignedShort(endianess);
        Preferences.debug("Minor version of OLE format = " + minorVersion + "\n", Preferences.DEBUG_FILEIO);
        // Location 26 Length 2 bytes Major version of the dll/format
        int majorVersion = getUnsignedShort(endianess);
        Preferences.debug("Major version of the OLE format = " + majorVersion + "\n", Preferences.DEBUG_FILEIO);
        // Location 28 Length 2 bytes ByteOrder Should be 0xfffe for intel or little endian
        int byteOrder = getUnsignedShort(endianess);
        if (byteOrder == 0xfffe) {
            Preferences.debug("Byte order is the expected little endian\n", Preferences.DEBUG_FILEIO);
        }
        else {
            Preferences.debug("Unexpected byte order value = " + byteOrder + "\n", Preferences.DEBUG_FILEIO);
        }
        // Location 30 Length 2 bytes Sector size in power of 2 (9 indicates 512 byte sectors)
        sectorSize = getUnsignedShort(endianess);
        sectorSize = (int)Math.round(Math.pow(2,sectorSize));
        Preferences.debug("The sector byte length = " + sectorSize + "\n", Preferences.DEBUG_FILEIO);
        // Location 32 Length 2 bytes Mini-sector size in power of 2 (6 indicates 64 byte mini-sectors)
        shortSectorSize = getUnsignedShort(endianess);
        shortSectorSize = (int)Math.round(Math.pow(2, shortSectorSize));
        Preferences.debug("The mini-sector byte length = " + shortSectorSize + "\n", Preferences.DEBUG_FILEIO);
        // Location 34 Length 2 bytes reserved must be zero
        int reserved = getUnsignedShort(endianess);
        if (reserved == 0) {
            Preferences.debug("Reserved is the expected zero\n", Preferences.DEBUG_FILEIO);
        }
        else {
            Preferences.debug("Reserved = " + reserved + " instead of the expected zero\n", Preferences.DEBUG_FILEIO);
        }
        // Location 36 Length 4 bytes reserved1 must be zero
        long reserved1 = getUInt(endianess);
        if (reserved1 == 0) {
            Preferences.debug("Reserved1 is the expected zero\n", Preferences.DEBUG_FILEIO);
        }
        else {
            Preferences.debug("Reserved1 = " + reserved1 + " instead of the expected zero\n", Preferences.DEBUG_FILEIO);
        }
        // Location 40 Length 4 bytes reserved2 must be zero
        long reserved2 = getUInt(endianess);
        if (reserved2 == 0) {
            Preferences.debug("Reserved2 is the expected zero\n", Preferences.DEBUG_FILEIO);
        }
        else {
            Preferences.debug("Reserved2 = " + reserved2 + " instead of the expected zero\n", Preferences.DEBUG_FILEIO);
        }
        // Location 44 Length 4 bytes Number of sectors used for the sector allocation table
        int sectorNumber = getInt(endianess);
        Preferences.debug("Number of sectors used for the sector allocation table = " + sectorNumber + "\n",
        		Preferences.DEBUG_FILEIO);
        // Location 48 Length 4 bytes First sector in the directory chain
        int directoryStartSector = readInt(endianess);
        if (directoryStartSector == -2) {
            Preferences.debug("First sector in the directory chain = END OF CHAIN\n", Preferences.DEBUG_FILEIO);
        }
        else {
            Preferences.debug("First sector in the directory chain = " + directoryStartSector + "\n",
            		Preferences.DEBUG_FILEIO);
        }
        // Location 52 Length 4 bytes Signature used for transactioning: must be zero.  The
        // reference implementation does not support transactioning.
        long signature = getUInt(endianess);
        if (signature == 0) {
            Preferences.debug("The transactioning signature is the expected zero\n", Preferences.DEBUG_FILEIO);
        }
        else {
            Preferences.debug("The transactioning signature = " + signature + " instead of the expected zero\n", 
            		Preferences.DEBUG_FILEIO);
        }
        // Location 56 Length 4 bytes Maximum size for mini-streams
        // Streams with an actual size smaller than (and not equal to) this value are stored as mini-streams.
        long miniSectorCutoff = getUInt(endianess);
        Preferences.debug("The minimum byte size for standard-streams = " + miniSectorCutoff + "\n", 
        		Preferences.DEBUG_FILEIO);
        // Location 60 Length 4 bytes First sector in the short sector allocation table
        int shortStartSector = readInt(endianess);
        if (shortStartSector == -2) {
            Preferences.debug("The first sector in the short sector allocation table = END OF CHAIN\n", 
            		Preferences.DEBUG_FILEIO);    
        }
        else {
            Preferences.debug("The first sector in the short sector allocation table = " + shortStartSector + "\n", 
            		Preferences.DEBUG_FILEIO);
        }
        // Location 64 Length 4 bytes Number of sectors in the short sector allocation table
        long shortTableSectors = getUInt(endianess);
        Preferences.debug("Number of sectors in the short sector allocation table = " + shortTableSectors + "\n", 
        		Preferences.DEBUG_FILEIO);
        // Location 68 Length 4 bytes First sector of the master sector allocation table
        // End of chain if no additional sectors used.
        int difStartSector = readInt(endianess);
        if (difStartSector == -2) {
            Preferences.debug("First sector of the master sector allocation table = END OF CHAIN\n", 
            		Preferences.DEBUG_FILEIO);
        }
        else {
            Preferences.debug("First sector of the master sector allocation table = " + difStartSector + "\n", 
            		Preferences.DEBUG_FILEIO);
        }
        // Location 72 Length 4 bytes Number of sectors used for the master sector allocation table
        long difSectors = getUInt(endianess);
        Preferences.debug("The number of sectors used for the master sector allocation table = " + difSectors + "\n", 
        		Preferences.DEBUG_FILEIO);
        // Location 76 Length 4*109 = 436 bytes First part of the master sector allocation table
        // containing 109 secIDs.
        int sectors[] = new int[sectorNumber];
        // Entries in sector allocation table
        // -1 freeSecID  Free sector, may exist in file, but is not part of any stream
        // -2 End of Chain SecID Trailing SecID in a SecID chain
        // -3 SAT SecID First entry in the sector allocation table.
        sat = new int[sectorNumber*sectorSize/4];
        int sp = 0;
        for (i = 0; i < Math.min(sectorNumber,109); i++) {
            sectors[i] = readInt(endianess);
            //Preferences.debug("Sector " + i + " = " + sectors[i] + "\n", Preferences.DEBUG_FILEIO);
            pos = raFile.getFilePointer(); 
            if (add128) {
                raFile.seek((sectors[i]+1)*sectorSize + 128);    
            }
            else {
                raFile.seek((sectors[i]+1)*sectorSize);
            }
            for (j = 0; j < sectorSize/4; j++) {
                sat[sp]= getInt(endianess);
                if (sp == 0) {
                    if (sat[sp] == -3) {
                        Preferences.debug("First sector specified for sector allocation table starts with expected -3\n", 
                        		Preferences.DEBUG_FILEIO);
                    }
                    else {
                        Preferences.debug("first sector specifed for sector allocation table starts with " + sat[sp] + 
                                      " instead of expected -3\n", Preferences.DEBUG_FILEIO);
                    }
                }
                //Preferences.debug("sat[" + sp + "] = " + sat[sp] + "\n", Preferences.DEBUG_FILEIO);
                sp++;
            }
            raFile.seek(pos);
        } // for (i = 0; i < Math.min(sectorNumber, 109); i++)
        if (sectorNumber > 109) {
            if (add128) {
                raFile.seek((difStartSector+1)*sectorSize + 128);    
            }
            else {
                raFile.seek((difStartSector+1)*sectorSize);
            }
            for (i = 109; i < sectorNumber; i++) {
                sectors[i] = readInt(endianess);
                //Preferences.debug("Sector " + i + " = " + sectors[i] + "\n", Preferences.DEBUG_FILEIO);
                if (((i - 109 + 1) % (sectorSize/4)) == 0) {
                    if (add128) {
                        pos = (sectors[i]+1)*sectorSize + 128;
                    }
                    else {
                        pos = (sectors[i]+1)*sectorSize;
                    }
                    raFile.seek(pos);
                    sectors[i] = readInt(endianess);
                }
                pos = raFile.getFilePointer(); 
                if (add128) {
                    raFile.seek((sectors[i]+1)*sectorSize + 128);    
                }
                else {
                    raFile.seek((sectors[i]+1)*sectorSize);
                }
                for (j = 0; j < sectorSize/4; j++) {
                    sat[sp]= getInt(endianess);
                    //Preferences.debug("sat[" + sp + "] = " + sat[sp] + "\n", Preferences.DEBUG_FILEIO);
                    sp++;
                }
                raFile.seek(pos);  
            } // for (i = 109; i < sectorNumber; i++)
        } // if (sectorNumber > 109)
        
        // Read short sector allocation table
        if (shortTableSectors > 0) {
            Preferences.debug("\nReading the short sector allocation table\n", Preferences.DEBUG_FILEIO);
            if (add128) {
                shortSectorTableAddress = (shortStartSector+1)*sectorSize + 128;    
            }
            else {
                shortSectorTableAddress = (shortStartSector+1)*sectorSize;
            }
            int shortSector = shortStartSector;
            raFile.seek(shortSectorTableAddress);
            shortSectorTable = new int[(int)shortTableSectors*sectorSize/4];
            for (i = 0; i < shortTableSectors*sectorSize/4; i++) {
                shortSectorTable[i] = readInt(endianess);
                //Preferences.debug("shortSectorTable[" + i + "] = " + shortSectorTable[i] + "\n", Preferences.DEBUG_FILEIO);
                if (((i+1) % (sectorSize/4) == 0) && ((i+1) < shortTableSectors*sectorSize/4)) {
                     shortSector = sat[shortSector];
                     if (add128) {
                         shortSectorTableAddress = (shortSector+1)*sectorSize + 128;
                     }
                     else {
                         shortSectorTableAddress = (shortSector+1)*sectorSize;    
                     }
                     raFile.seek(shortSectorTableAddress);
                }
            }
        } // if (shortSectors > 0)
        
        
        // Determine the numbers of sectors in the directory chain
        int directorySectors = 1;
        int ds = directoryStartSector;
        while (sat[ds] != -2) {
            directorySectors++;
            ds = sat[ds];
        }
        Preferences.debug("The number of directory sectors = " + directorySectors + "\n", Preferences.DEBUG_FILEIO);
        int directoryTable[] = new int[directorySectors];
        directoryTable[0] = directoryStartSector;
        Preferences.debug("directoryTable[0] = " + directoryTable[0] + "\n", Preferences.DEBUG_FILEIO);
        for (i = 1; i < directorySectors; i++) {
            directoryTable[i] = sat[directoryTable[i-1]];
            Preferences.debug("directoryTable[" + i + "] = " + directoryTable[i] + "\n", Preferences.DEBUG_FILEIO);
        }
        if (sat[directoryTable[directorySectors-1]] == -2) {
            Preferences.debug("sat[directoryTable[directorySectors-1]] == -2 as expected\n", Preferences.DEBUG_FILEIO);    
        }
        else {
            Preferences.debug("sat[directoryTable[directorySectors-1]] == " + 
                    sat[directoryTable[directorySectors-1]] + 
                    " instead of expected -2\n", Preferences.DEBUG_FILEIO);        
        }
        
        // Read the first sector of the directory chain (also referred to as the first element of the 
        // Directory array, or SID 0) is known as the Root Directory Entry
        Preferences.debug("\nReading the first sector of the directory chain\n", Preferences.DEBUG_FILEIO);
        if (add128) {
            directoryStart = (directoryTable[0]+1)*sectorSize + 128;
        }
        else {
            directoryStart = (directoryTable[0]+1)*sectorSize;    
        }
        raFile.seek(directoryStart+64);
        // Read the length of the element name in bytes.  Each Unicode character is 2 bytes
        int elementNameBytes = getUnsignedShort(endianess);
        Preferences.debug("The element name has " + (elementNameBytes/2) + " unicode characters\n",
        		Preferences.DEBUG_FILEIO); 
        // Read the element name
        raFile.seek(directoryStart);
        b = new byte[elementNameBytes];
        raFile.readFully(b);
        String elementName = new String(b, "UTF-16LE").trim();
        // The element name is typically Root Entry in Unicode, although
        // some versions of structured storage (particularly the preliminary
        // reference implementation and the Macintosh version) store only
        // the first letter of this string "R".  This string is always
        // ignored, since the Root Directory Entry is known by its position
        // SID 0 rather than its name, and its name is not otherwise used.
        Preferences.debug("The element name is " + elementName + "\n", Preferences.DEBUG_FILEIO);
        // Read the type of object
        raFile.seek(directoryStart + 66);
        byte objectType[] = new byte[1];
        raFile.read(objectType);
        if (objectType[0] == 5) {
            Preferences.debug("Object type is root as expected\n", Preferences.DEBUG_FILEIO);
        }
        else if (objectType[0] == 0) {
            Preferences.debug("Object type is unexpectedly invalid\n", Preferences.DEBUG_FILEIO);
        }
        else if (objectType[0] == 1) {
            Preferences.debug("Object type is unexpectedly storage\n", Preferences.DEBUG_FILEIO);
        }
        else if (objectType[0] == 2) {
            Preferences.debug("Object type is unexpectedly stream\n", Preferences.DEBUG_FILEIO);
        }
        else if (objectType[0] == 3) {
            Preferences.debug("Object type is unexpectedly lockbytes\n", Preferences.DEBUG_FILEIO);
        }
        else if (objectType[0] == 4) {
            Preferences.debug("Object type is unexpectedly property\n", Preferences.DEBUG_FILEIO);
        }
        else {
            Preferences.debug("Object type is an illegal " + objectType[0] + "\n", Preferences.DEBUG_FILEIO);
        }
        // offset 67 color.  Since the root directory does not have siblings, it's
        // color is irrelevant and may therefore be either red or black.
        byte color[] = new byte[1];
        raFile.read(color);
        if (color[0] == 0) {
            Preferences.debug("Node is red\n", Preferences.DEBUG_FILEIO);
        }
        else if (color[0] == 1) {
            Preferences.debug("Node is black\n", Preferences.DEBUG_FILEIO);
        }
        else {
            Preferences.debug("Node has illegal color value = " + color[0] + "\n", Preferences.DEBUG_FILEIO);
        }
        // offset 68 length 4 bytes SID of the left child of this entry in the directory tree
        int leftChild = readInt(endianess);
        if (leftChild == -1) {
            Preferences.debug("No left child for this entry\n", Preferences.DEBUG_FILEIO);
        }
        else {
            Preferences.debug("The SID of the left child of this entry in the directory tree = " + leftChild + "\n", 
            		Preferences.DEBUG_FILEIO);
        }
        // offset 72 length 4 bytes SID of the right child of this entry in the directory tree
        int rightChild = readInt(endianess);
        if (rightChild == -1) {
            Preferences.debug("No right child for this entry\n", Preferences.DEBUG_FILEIO);
        }
        else {
            Preferences.debug("The SID of the right child of this entry in the directory tree = " + rightChild + "\n",
            		Preferences.DEBUG_FILEIO);
        }
        // offset 76 length 4 bytes SID of the root node entry of the red-black tree of all storage members
        // if this entry is storage, -1 otherwise
        int rootNodeEntry = readInt(endianess);
        if (rootNodeEntry == -1) {
            Preferences.debug("No root node entry for this entry\n", Preferences.DEBUG_FILEIO);
        }
        else {
            Preferences.debug("The root node entry of the red-black tree of all storage members = " + rootNodeEntry + "\n",
            		Preferences.DEBUG_FILEIO);
        }
        // offset 80 length 16 bytes class id
        getLong(endianess);
        getLong(endianess);
        // offset 96 length 4 bytes userFlags not applicable for root object
        long userFlags = getUInt(endianess);
        Preferences.debug("User flags = " + userFlags + "\n", Preferences.DEBUG_FILEIO);
        // offset 100 length 8 bytes creation time stamp
        long creationTimeStamp = getLong(endianess);
        if (creationTimeStamp == 0) {
            Preferences.debug("Creation time stamp not set\n", Preferences.DEBUG_FILEIO);
        }
        else {
            Preferences.debug("Creation time stamp = " + 
                    calculateTimeStampString(creationTimeStamp) + "\n", Preferences.DEBUG_FILEIO);
        }
        // offset 108 length 8 bytes modification time stamp
        long modificationTimeStamp = getLong(endianess);
        if (creationTimeStamp == 0) {
            Preferences.debug("Modification time stamp not set\n", Preferences.DEBUG_FILEIO);
        }
        else {
            Preferences.debug("Modification time stamp = " + 
                    calculateTimeStampString(modificationTimeStamp) + "\n", Preferences.DEBUG_FILEIO);
        }
        // offset 116 length 4 bytes first sector of short stream container stream
        shortStreamStartSect = readInt(endianess);
        Preferences.debug("First sector of the short stream container stream = " +
                          shortStreamStartSect + "\n", Preferences.DEBUG_FILEIO);
        // Offset 120 length 4 bytes Total size of the short stream container stream
        totalShortStreamSize = getUInt(endianess);
            Preferences.debug("Total byte size of the short stream container stream = " +
                              totalShortStreamSize + "\n", Preferences.DEBUG_FILEIO);
        int shortStreamSectorNumber = (int)(totalShortStreamSize/sectorSize);
        if ((totalShortStreamSize % sectorSize) != 0) {
            shortStreamSectorNumber++;
        }
        Preferences.debug("The number of bytes in the short stream container stream requires " +
                           shortStreamSectorNumber + " sectors\n", Preferences.DEBUG_FILEIO);
        int shortSectors[] = new int[shortStreamSectorNumber];
        shortSectors[0] = shortStreamStartSect;
        Preferences.debug("shortSectors[0] = " + shortSectors[0] + "\n");
        for (i = 1; i < shortStreamSectorNumber; i++) {
            shortSectors[i] = sat[shortSectors[i-1]];
            Preferences.debug("shortSectors[" + i + "] = " + shortSectors[i] + "\n", Preferences.DEBUG_FILEIO);
        }
        if (sat[shortSectors[shortStreamSectorNumber-1]] == -2) {
            Preferences.debug("sat[shortSectors[shortStreamSectorNumber-1]] == -2 as expected\n", Preferences.DEBUG_FILEIO);    
        }
        else {
            Preferences.debug("sat[shortSectors[shortStreamSectorNumber-1]] == " + 
                    sat[shortSectors[shortStreamSectorNumber-1]] + 
                    " instead of expected -2\n", Preferences.DEBUG_FILEIO);        
        }
        // Offset 124 length 2 bytes dptPropType Reserved for future use.  Must be zero
        int dptPropType = getUnsignedShort(endianess);
        if (dptPropType == 0) {
            Preferences.debug("dptPropType = 0 as expected\n", Preferences.DEBUG_FILEIO);
        }
        else {
            Preferences.debug("dptProptType = " + dptPropType + " instead of the expected 0\n", Preferences.DEBUG_FILEIO);
        }
        
        int directoryEntry = 1;
        directoryStart = directoryStart + 128;
        int maximumDirectoryEntriesPerSector = sectorSize/128;
        int dp = 0;
        while (dp < directorySectors) {
            Preferences.debug("\nReading element " + directoryEntry + " of the directory array\n", Preferences.DEBUG_FILEIO);
            directoryEntry++;
            raFile.seek(directoryStart+64);
            // Read the length of the element name in bytes.  Each Unicode character is 2 bytes
            elementNameBytes = getUnsignedShort(endianess);
            Preferences.debug("The element name has " + (elementNameBytes/2) + " unicode characters\n",
            		Preferences.DEBUG_FILEIO);
            if (elementNameBytes <= 0) {
                break;
            }
            // Read the element name
            raFile.seek(directoryStart);
            b = new byte[elementNameBytes];
            raFile.readFully(b);
            if (lastElementName != null) {
                twoAgoElementName = new String(lastElementName);
            }
            lastElementName = new String(elementName);
            elementName = new String(b, "UTF-16LE").trim();
            Preferences.debug("The element name is " + elementName + "\n", Preferences.DEBUG_FILEIO);
            // Read the type of object
            raFile.seek(directoryStart + 66);
            raFile.read(objectType);
            if (objectType[0] == 0) {
                Preferences.debug("Object type is invalid\n", Preferences.DEBUG_FILEIO);
            }
            else if (objectType[0] == 1) {
                Preferences.debug("Object type is storage\n", Preferences.DEBUG_FILEIO);
            }
            else if (objectType[0] == 2) {
                Preferences.debug("Object type is stream\n", Preferences.DEBUG_FILEIO);
            }
            else if (objectType[0] == 3) {
                Preferences.debug("Object type is lockbytes\n", Preferences.DEBUG_FILEIO);
            }
            else if (objectType[0] == 4) {
                Preferences.debug("Object type is property\n", Preferences.DEBUG_FILEIO);
            }
            else if (objectType[0] == 5) {
                Preferences.debug("Object type is incorrectly root\n", Preferences.DEBUG_FILEIO);
            }
            else {
                Preferences.debug("Object type is an illegal " + objectType[0] + "\n", Preferences.DEBUG_FILEIO);
                break;
            }
            // offset 67 color.  
            raFile.read(color);
            if (color[0] == 0) {
                Preferences.debug("Node is red\n", Preferences.DEBUG_FILEIO);
            }
            else if (color[0] == 1) {
                Preferences.debug("Node is black\n", Preferences.DEBUG_FILEIO);
            }
            else {
                Preferences.debug("Node has illegal color value = " + color[0] + "\n", Preferences.DEBUG_FILEIO);
                break;
            }
            // offset 68 length 4 bytes SID of the left child of this entry in the directory tree
            leftChild = readInt(endianess);
            if (leftChild == -1) {
                Preferences.debug("No left child for this entry\n", Preferences.DEBUG_FILEIO);
            }
            else {
                Preferences.debug("The SID of the left child of this entry in the directory tree = " + leftChild + "\n",
                		Preferences.DEBUG_FILEIO);
            }
            // offset 72 length 4 bytes SID of the right child of this entry in the directory tree
            rightChild = readInt(endianess);
            if (rightChild == -1) {
                Preferences.debug("No right child for this entry\n", Preferences.DEBUG_FILEIO);
            }
            else {
                Preferences.debug("The SID of the right child of this entry in the directory tree = " + rightChild + "\n", 
                		Preferences.DEBUG_FILEIO);
            }
            // offset 76 length 4 bytes SID of the root node entry of the red-black tree of all storage members
            // if this entry is storage, -1 otherwise
            rootNodeEntry = readInt(endianess);
            if (rootNodeEntry == -1) {
                Preferences.debug("No root node entry for this entry\n", Preferences.DEBUG_FILEIO);
            }
            else {
                Preferences.debug("The root node entry of the red-black tree of all storage members = " + rootNodeEntry + "\n", 
                		Preferences.DEBUG_FILEIO);
            }    
            
            // offset 80 length 16 bytes class id
            getLong(endianess);
            getLong(endianess);
            // offset 96 length 4 bytes userFlags
            userFlags = getUInt(endianess);
            Preferences.debug("User flags = " + userFlags + "\n", Preferences.DEBUG_FILEIO);
            // offset 100 length 8 bytes creation time stamp
            creationTimeStamp = getLong(endianess);
            if (creationTimeStamp == 0) {
                Preferences.debug("Creation time stamp not set\n", Preferences.DEBUG_FILEIO);
            }
            else {
                Preferences.debug("Creation time stamp = " + 
                                   calculateTimeStampString(creationTimeStamp) + "\n", Preferences.DEBUG_FILEIO);
            }
            // offset 108 length 8 bytes modification time stamp
            modificationTimeStamp = getLong(endianess);
            if (creationTimeStamp == 0) {
                Preferences.debug("Modification time stamp not set\n", Preferences.DEBUG_FILEIO);
            }
            else {
                Preferences.debug("Modification time stamp = " +
                                  calculateTimeStampString(modificationTimeStamp) + "\n", Preferences.DEBUG_FILEIO);
            }
            // offset 116 length 4 bytes starting sector of the stream
            startSect = readInt(endianess);
            // Offset 120 length 4 bytes Size of the stream in byes
            streamSize = getUInt(endianess);
            if (streamSize <= miniSectorCutoff) {
                Preferences.debug("Starting sector of the ministream = " + startSect + "\n", Preferences.DEBUG_FILEIO);
                Preferences.debug("Size of the ministream in bytes = " + streamSize + "\n", Preferences.DEBUG_FILEIO);
            }
            else {
                Preferences.debug("Starting sector of the stream = " + startSect + "\n", Preferences.DEBUG_FILEIO);
                Preferences.debug("Size of the stream in bytes = " + streamSize + "\n", Preferences.DEBUG_FILEIO);    
            }
            // Offset 124 length 2 bytes dptPropType Reserved for future use.  Must be zero
            dptPropType = getUnsignedShort(endianess);
            if (dptPropType == 0) {
                Preferences.debug("dptPropType = 0 as expected\n", Preferences.DEBUG_FILEIO);
            }
            else {
                Preferences.debug("dptProptType = " + dptPropType + " instead of the expected 0\n", Preferences.DEBUG_FILEIO);
                break;
            }
            
            directoryStart = directoryStart + 128;
            
            if ((((lastElementName.equals("Image")) &&
                    (elementName.equals("Contents"))) ||
                 ((twoAgoElementName != null ) && (twoAgoElementName.equals("Image")) &&
                  (lastElementName.substring(0,4).equals("Item")) && (elementName.equals("Contents"))))
                 && (objectType[0] == 2) && (streamSize > 0)) {
                Preferences.debug("Reading the contents stream of the container image\n", Preferences.DEBUG_FILEIO);
                if  ((twoAgoElementName != null ) && (twoAgoElementName.equals("Image")) &&
                        (lastElementName.substring(0,4).equals("Item")) && (elementName.equals("Contents"))) {
                            readImagePixels = true;    
                        }
                else {
                    readImagePixels = false;
                }
                      
                if (readImagePixels) {
                    startSectorArray = new int[100];
                    startSectorArray[ap] = startSect;    
                }
                bytesToRead = (int)streamSize;
                Preferences.debug("bytesToRead = " + bytesToRead + "\n", Preferences.DEBUG_FILEIO);
                b = new byte[bytesToRead];
                bytesRead = 0;
                    if (streamSize < miniSectorCutoff) {
                        presentShortSector = startSect;
                        while (bytesToRead > 0) {
                            sectorsIntoShortStream = presentShortSector*shortSectorSize/sectorSize;
                            presentSector = shortSectors[sectorsIntoShortStream];
                            presentSectorOffset = (presentShortSector*shortSectorSize) % sectorSize;
                            if (add128) {
                                raFile.seek((presentSector+1)*sectorSize + 128 + presentSectorOffset);
                            }
                            else {
                                raFile.seek((presentSector+1)*sectorSize + presentSectorOffset);    
                            }
                            raFile.read(b, bytesRead, Math.min(shortSectorSize, bytesToRead));
                            bytesRead += Math.min(shortSectorSize, bytesToRead);
                            bytesToRead -= Math.min(shortSectorSize, bytesToRead);
                            presentShortSector = shortSectorTable[presentShortSector];
                        }
                } // if (streamSize < miniSectorCutoff)
                else { // else streamSize >= miniSectorCutoff
                    presentSector = startSect;
                    while (bytesToRead > 0) {
                        if (add128) {
                            raFile.seek((presentSector+1)*sectorSize + 128);
                        }
                        else {
                            raFile.seek((presentSector+1)*sectorSize);    
                        }
                        raFile.read(b, bytesRead, Math.min(sectorSize, bytesToRead));
                        bytesRead += Math.min(sectorSize, bytesToRead);
                        bytesToRead -= Math.min(sectorSize, bytesToRead);
                        presentSector = sat[presentSector];
                    }    
                } // else streamSize >= miniSectorCutoff
                while (true) {
                    bp = 0;
                    dType = (short) (((b[bp+1] & 0xff) << 8) | (b[bp] & 0xff));
                    bp += 2;
                    if (dType == VT_I4) {
                        Preferences.debug("Expected VT_I4 data type for version\n", Preferences.DEBUG_FILEIO);
                    }
                    else {
                        Preferences.debug("dType = " + dType + " instead of expected VT_I4 for version\n", 
                        		Preferences.DEBUG_FILEIO);
                        break;
                    }
                    minorVersion =  (((b[bp+1] & 0xff) << 8) | (b[bp] & 0xff));
                    bp += 2;
                    Preferences.debug("Minor version is " + minorVersion + "\n", Preferences.DEBUG_FILEIO);
                    Preferences.debug("Current version is 4099\n", Preferences.DEBUG_FILEIO);
                    majorVersion =  (((b[bp+1] & 0xff) << 8) | (b[bp] & 0xff));
                    bp += 2;
                    Preferences.debug("Major version is " + majorVersion + "\n", Preferences.DEBUG_FILEIO);
                    Preferences.debug("Current version is 12288\n", Preferences.DEBUG_FILEIO);
                    dType = (short) (((b[bp+1] & 0xff) << 8) | (b[bp] & 0xff));
                    bp += 2;
                    if (dType == VT_I4) {
                        Preferences.debug("Expected VT_I4 data type for file type\n", Preferences.DEBUG_FILEIO);
                    }
                    else {
                        Preferences.debug("dType = " + dType + " instead of expected VT_I4 for file type\n",
                        		Preferences.DEBUG_FILEIO);
                        break;
                    }
                    // File type not used
                    int fileType = (((b[bp + 3] & 0xff) << 24) | ((b[bp + 2] & 0xff) << 16) | 
                            ((b[bp + 1] & 0xff) << 8) | (b[bp] & 0xff));
                    bp += 4;
                    Preferences.debug("Unused file type = " + fileType + "\n", Preferences.DEBUG_FILEIO);
                    dType = (short) (((b[bp+1] & 0xff) << 8) | (b[bp] & 0xff));
                    bp += 2;
                    if (dType == VT_EMPTY) {
                        Preferences.debug("Expected VT_EMPTY data type for unused type description\n", Preferences.DEBUG_FILEIO);
                    }
                    else if (dType == VT_BSTR) {
                        Preferences.debug("VT_BSTR for type description\n", Preferences.DEBUG_FILEIO);
                        stringBytes = (((b[bp + 3] & 0xff) << 24) | ((b[bp + 2] & 0xff) << 16) | 
                                ((b[bp + 1] & 0xff) << 8) | (b[bp] & 0xff));
                        bp += 4;
                        bf = new byte[stringBytes];
                        for (i = 0; i < stringBytes; i++) {
                            bf[i] = b[bp++];
                        }
                        String typeDescription = new String(b, "UTF-16LE").trim();
                        Preferences.debug("Type description = " + typeDescription + "\n", Preferences.DEBUG_FILEIO);
                    }
                    else {
                        Preferences.debug("dType = " + dType + 
                                " instead of expected VT_EMPTY or VT_BSTR for type description\n", Preferences.DEBUG_FILEIO);
                        break;
                    }
                    dType = (short) (((b[bp+1] & 0xff) << 8) | (b[bp] & 0xff));
                    bp += 2;
                    if (dType == VT_EMPTY) {
                        Preferences.debug("Expected VT_EMPTY data type for name of zvi file\n", Preferences.DEBUG_FILEIO);
                    }
                    else if (dType == VT_BSTR) {
                        Preferences.debug("VT_BSTR for name of zvi file\n", Preferences.DEBUG_FILEIO);
                        stringBytes = (((b[bp + 3] & 0xff) << 24) | ((b[bp + 2] & 0xff) << 16) | 
                                ((b[bp + 1] & 0xff) << 8) | (b[bp] & 0xff));
                        bp += 4;
                        bf = new byte[stringBytes];
                        for (i = 0; i < stringBytes; i++) {
                            bf[i] = b[bp++];
                        }
                        String fileName = new String(b, "UTF-16LE").trim();
                        Preferences.debug("Name of zvi file = " + fileName + "\n", Preferences.DEBUG_FILEIO);
                    }
                    else {
                        Preferences.debug("dType = " + dType + 
                                " instead of expected VT_EMPTY or VT_BSTR for name of zvi file\n", Preferences.DEBUG_FILEIO);
                        break;
                    }
                    dType = (short) (((b[bp+1] & 0xff) << 8) | (b[bp] & 0xff));
                    bp += 2;
                    if (dType == VT_I4) {
                        Preferences.debug("Expected VT_I4 data type for imageWidth\n", Preferences.DEBUG_FILEIO);
                    }
                    else {
                        Preferences.debug("dType = " + dType + " instead of expected VT_I4 for imageWidth\n", 
                        		Preferences.DEBUG_FILEIO);
                        break;
                    }
                    imageWidth = (((b[bp + 3] & 0xff) << 24) | ((b[bp + 2] & 0xff) << 16) | 
                            ((b[bp + 1] & 0xff) << 8) | (b[bp] & 0xff));
                    bp += 4;
                    Preferences.debug("Image width = " + imageWidth + "\n", Preferences.DEBUG_FILEIO);
                    dType = (short) (((b[bp+1] & 0xff) << 8) | (b[bp] & 0xff));
                    bp += 2;
                    if (dType == VT_I4) {
                        Preferences.debug("Expected VT_I4 data type for imageHeight\n", Preferences.DEBUG_FILEIO);
                    }
                    else {
                        Preferences.debug("dType = " + dType + " instead of expected VT_I4 for imageHeight\n", 
                        		Preferences.DEBUG_FILEIO);
                        break;
                    }
                    imageHeight = (((b[bp + 3] & 0xff) << 24) | ((b[bp + 2] & 0xff) << 16) | 
                            ((b[bp + 1] & 0xff) << 8) | (b[bp] & 0xff));
                    bp += 4;
                    Preferences.debug("Image height = " + imageHeight + "\n", Preferences.DEBUG_FILEIO);
                    dType = (short) (((b[bp+1] & 0xff) << 8) | (b[bp] & 0xff));
                    bp += 2;
                    if (dType == VT_I4) {
                        Preferences.debug("Expected VT_I4 data type for imageDepth\n", Preferences.DEBUG_FILEIO);
                    }
                    else {
                        Preferences.debug("dType = " + dType + " instead of expected VT_I4 for imageDepth\n", Preferences.DEBUG_FILEIO);
                        break;
                    }
                    imageDepth = (((b[bp + 3] & 0xff) << 24) | ((b[bp + 2] & 0xff) << 16) | 
                            ((b[bp + 1] & 0xff) << 8) | (b[bp] & 0xff));
                    bp += 4;
                    Preferences.debug("Unused image depth = " + imageDepth + "\n", Preferences.DEBUG_FILEIO);
                    dType = (short) (((b[bp+1] & 0xff) << 8) | (b[bp] & 0xff));
                    bp += 2;
                    if (dType == VT_I4) {
                        Preferences.debug("Expected VT_I4 data type for imagePixelFormat\n", Preferences.DEBUG_FILEIO);
                    }
                    else {
                        Preferences.debug("dType = " + dType + " instead of expected VT_I4 for imagePixelFormat\n", 
                        		Preferences.DEBUG_FILEIO);
                        break;
                    }
                    imagePixelFormat = (((b[bp + 3] & 0xff) << 24) | ((b[bp + 2] & 0xff) << 16) | 
                            ((b[bp + 1] & 0xff) << 8) | (b[bp] & 0xff));
                    bp += 4;
                    switch(imagePixelFormat) {
                        case 1:
                            Preferences.debug("Image pixel format = 8-bit B, G, R - 3 bytes/pixel\n", Preferences.DEBUG_FILEIO);
                            sliceBytes = 3 * imageWidth * imageHeight;
                            break;
                        case 2:
                            Preferences.debug("Image pixel format = 8-bit B, G, R, A - 4 bytes/pixel\n", Preferences.DEBUG_FILEIO);
                            sliceBytes = 4 * imageWidth * imageHeight;
                            break;
                        case 3:
                            Preferences.debug("Image pixel format = 8-bit grayscale\n", Preferences.DEBUG_FILEIO);
                            sliceBytes = imageWidth * imageHeight;
                            break;
                        case 4:
                            Preferences.debug("Image pixel format = 16-bit integer\n", Preferences.DEBUG_FILEIO);
                            sliceBytes = 2 * imageWidth * imageHeight;
                            break;
                        case 5:
                            Preferences.debug("Image pixel format = 32-bit integer\n", Preferences.DEBUG_FILEIO);
                            sliceBytes = 4 * imageWidth * imageHeight;
                            break;
                        case 6:
                            Preferences.debug("Image pixel format = 32-bit IEEE float\n", Preferences.DEBUG_FILEIO);
                            sliceBytes = 4 * imageWidth * imageHeight;
                            break;
                        case 7:
                            Preferences.debug("Image pixel format = 64-bit IEEE double\n", Preferences.DEBUG_FILEIO);
                            sliceBytes = 8 * imageWidth * imageHeight;
                            break;
                        case 8:
                            Preferences.debug("Image pixel format = 16-bit B, G, R - 6 bytes/pixel\n", Preferences.DEBUG_FILEIO);
                            sliceBytes = 6 * imageWidth * imageHeight;
                            break;
                        case 9:
                            Preferences.debug("Image pixel format = 32-bit B, G, R = 12 bytes/pixel\n", Preferences.DEBUG_FILEIO);
                            sliceBytes = 12 * imageWidth * imageHeight;
                            break;
                        default:
                            Preferences.debug("imagePixelFormat has an unrecognized value = " + imagePixelFormat + "\n", 
                            		Preferences.DEBUG_FILEIO);
                    }
                    dType = (short) (((b[bp+1] & 0xff) << 8) | (b[bp] & 0xff));
                    bp += 2;
                    if (dType == VT_I4) {
                        Preferences.debug("Expected VT_I4 data type for imageCount\n", Preferences.DEBUG_FILEIO);
                    }
                    else {
                        Preferences.debug("dType = " + dType + " instead of expected VT_I4 for imageCount\n", Preferences.DEBUG_FILEIO);
                        break;
                    }
                    imageCount = (((b[bp + 3] & 0xff) << 24) | ((b[bp + 2] & 0xff) << 16) | 
                            ((b[bp + 1] & 0xff) << 8) | (b[bp] & 0xff));
                    backupZDim = imageCount;
                    if (imageCount == 0) {
                        Preferences.debug("Original total number of image item storages = 0.  Setting to 100\n",
                                          Preferences.DEBUG_FILEIO);
                        imageCount = 100;
                    }
                    bp += 4;
                    Preferences.debug("Total number of image item storages = " + imageCount + "\n", Preferences.DEBUG_FILEIO);
                    zArray = new int[imageCount];
                    cArray = new int[imageCount];
                    tArray = new int[imageCount];
                    positionArray = new int[imageCount];
                    if (!readImagePixels) {
                        startSectorArray = new int[imageCount];
                    }
                    offsetArray = new int[imageCount];
                    imageFocusPositionArray = new double[imageCount+10];
                    imageZArray = new int[imageCount+10];
                    imageZ2Array = new int[imageCount+10];
                    imageC2Array = new int[imageCount+10];
                    imageT2Array = new int[imageCount+10];
                    imageBlackValueArray = new double[imageCount+10];
                    imageZ3Array = new int[imageCount+10];
                    imageC3Array = new int[imageCount+10];
                    imageT3Array = new int[imageCount+10];
                    imageWhiteValueArray = new double[imageCount+10];
                    imageZ4Array = new int[imageCount+10];
                    imageRelFocusPosition1Array = new double[imageCount+10];
                    imageZ5Array = new int[imageCount+10];
                    imageRelFocusPosition2Array = new double[imageCount+10];
                    imageZ6Array = new int[imageCount+10];
                    imageC6Array = new int[imageCount+10];
                    imageT6Array = new int[imageCount+10];
                    cameraImageAcquisitionTime = new double[imageCount+10];
                    imageZ7Array = new int[imageCount+10];
                    imageC7Array = new int[imageCount+10];
                    imageT7Array = new int[imageCount+10];
                    imageRelativeTime = new double[imageCount+10];
                    imageStagePositionXArray = new double[imageCount+10];
                    imageZXArray = new int[imageCount+10];
                    imageStagePositionYArray = new double[imageCount+10];
                    imageZYArray = new int[imageCount+10];
                    for (i = 0; i < imageCount+10; i++) {
                        imageFocusPositionArray[i] = Double.NaN;
                        imageZArray[i] = Integer.MIN_VALUE;
                        imageZ2Array[i] = Integer.MIN_VALUE;
                        imageC2Array[i] = Integer.MIN_VALUE;
                        imageT2Array[i] = Integer.MIN_VALUE;
                        imageBlackValueArray[i] = Double.NaN;
                        imageZ3Array[i] = Integer.MIN_VALUE;
                        imageC3Array[i] = Integer.MIN_VALUE;
                        imageT3Array[i] = Integer.MIN_VALUE;
                        imageWhiteValueArray[i] = Double.NaN;
                        imageZ4Array[i] = Integer.MIN_VALUE;
                        imageRelFocusPosition1Array[i] = Double.NaN;
                        imageZ5Array[i] = Integer.MIN_VALUE;
                        imageRelFocusPosition2Array[i] = Double.NaN;
                        imageZ6Array[i] = Integer.MIN_VALUE;
                        imageC6Array[i] = Integer.MIN_VALUE;
                        imageT6Array[i] = Integer.MIN_VALUE;
                        cameraImageAcquisitionTime[i] = Double.NaN;
                        imageZ7Array[i] = Integer.MIN_VALUE;
                        imageC7Array[i] = Integer.MIN_VALUE;
                        imageT7Array[i] = Integer.MIN_VALUE;
                        imageRelativeTime[i] = Double.NaN;
                        imageStagePositionXArray[i] = Double.NaN;
                        imageZXArray[i] = Integer.MIN_VALUE;
                        imageStagePositionYArray[i] = Double.NaN;
                        imageZYArray[i] = Integer.MIN_VALUE;
                    }
                    dType = (short) (((b[bp+1] & 0xff) << 8) | (b[bp] & 0xff));
                    bp += 2;
                    if (dType == VT_I4) {
                        Preferences.debug("Expected VT_I4 data type for imageValidBitsPerPixel\n", Preferences.DEBUG_FILEIO);
                    }
                    else {
                        Preferences.debug("dType = " + dType + " instead of expected VT_I4 for imageValidBitsPerPixel\n", 
                        		Preferences.DEBUG_FILEIO);
                        break;
                    }
                    // Valid bits per pixel in raw image data if 16 bit image (may be 12, 14, or 16)
                    imageValidBitsPerPixel = (((b[bp + 3] & 0xff) << 24) | ((b[bp + 2] & 0xff) << 16) | 
                            ((b[bp + 1] & 0xff) << 8) | (b[bp] & 0xff));
                    bp += 4;
                    // Acquisition bit depth for tagID = 531 gave 12 while imageValidBitsPerPixel gave 16.
                    Preferences.debug("Valid bits per pixel in raw image data = " + imageValidBitsPerPixel + "\n", 
                    		Preferences.DEBUG_FILEIO);
                    //fileInfo.setValidBitsPerPixel(imageValidBitsPerPixel);
                    if (readImagePixels) {
                        bp = 296;
                        // Read the image header at 296 bytes from the stream beginning
                        // Don't read dType any more
                        Preferences.debug("Reading the image header\n", Preferences.DEBUG_FILEIO);
                        // Stream version ID
                        // 2 byte minor 0x2000 followed by 2 byte major 0x1000
                        minorVersion = (((b[bp+1] & 0xff) << 8) | (b[bp] & 0xff));
                        bp += 2;
                        Preferences.debug("The minor version = " + minorVersion + "\n", Preferences.DEBUG_FILEIO);
                        Preferences.debug("The current minor version is 8192\n", Preferences.DEBUG_FILEIO);
                        majorVersion = (((b[bp+1] & 0xff) << 8) | (b[bp] & 0xff));
                        bp += 2;
                        Preferences.debug("The major version = " + majorVersion + "\n", Preferences.DEBUG_FILEIO);
                        Preferences.debug("The current major version is 4096\n", Preferences.DEBUG_FILEIO);
                        width = (((b[bp + 3] & 0xff) << 24) | ((b[bp + 2] & 0xff) << 16) | 
                                ((b[bp + 1] & 0xff) << 8) | (b[bp] & 0xff));
                        bp += 4;
                        Preferences.debug("Width = " + width + "\n", Preferences.DEBUG_FILEIO);
                        height = (((b[bp + 3] & 0xff) << 24) | ((b[bp + 2] & 0xff) << 16) | 
                                ((b[bp + 1] & 0xff) << 8) | (b[bp] & 0xff));
                        bp += 4;
                        Preferences.debug("Height = " + height + "\n", Preferences.DEBUG_FILEIO);
                        depth = (((b[bp + 3] & 0xff) << 24) | ((b[bp + 2] & 0xff) << 16) | 
                                ((b[bp + 1] & 0xff) << 8) | (b[bp] & 0xff));
                        bp += 4;
                        Preferences.debug("Unused depth = " + depth + "\n", Preferences.DEBUG_FILEIO);
                        pixelWidth = (((b[bp + 3] & 0xff) << 24) | ((b[bp + 2] & 0xff) << 16) | 
                                ((b[bp + 1] & 0xff) << 8) | (b[bp] & 0xff));
                        bp += 4;
                        Preferences.debug("Size in bytes of an image pixel = " + pixelWidth + "\n", Preferences.DEBUG_FILEIO);
                        pixelFormat = (((b[bp + 3] & 0xff) << 24) | ((b[bp + 2] & 0xff) << 16) | 
                                ((b[bp + 1] & 0xff) << 8) | (b[bp] & 0xff));
                        bp += 4;
                        displayPixelFormat(pixelFormat);
                        // Valid bits per pixel in raw image data if 16 bit image (may be 12, 14, or 16)
                        validBitsPerPixel = (((b[bp + 3] & 0xff) << 24) | ((b[bp + 2] & 0xff) << 16) | 
                                ((b[bp + 1] & 0xff) << 8) | (b[bp] & 0xff));
                        bp += 4;
                        Preferences.debug("Valid bits per pixel in raw image data = " + validBitsPerPixel + "\n", 
                                Preferences.DEBUG_FILEIO);
                        // Raw bytes for image slice are stored here
                        // Store the offset into the starting sector
                        offsetArray[ap++] = bp;
                        break;    
                    } // readImgagePixels
                    else { //  not readImagePixels
                        dType = (short) (((b[bp+1] & 0xff) << 8) | (b[bp] & 0xff));
                        bp += 2;
                        if (dType == VT_BLOB) {
                            Preferences.debug("Expected VT_BLOB data type for {m_PluginCLSID}\n", Preferences.DEBUG_FILEIO);
                        }
                        else {
                            Preferences.debug("dType = " + dType + " instead of expected VT_BLOB for {m_PluginCLSID}\n", 
                            		Preferences.DEBUG_FILEIO);
                            break;
                        }
                        int pluginLength = (((b[bp + 3] & 0xff) << 24) | ((b[bp + 2] & 0xff) << 16) | 
                                ((b[bp + 1] & 0xff) << 8) | (b[bp] & 0xff));
                        bp += 4;
                        Preferences.debug("The length of the {m_PluginCLSID} binary data = " + pluginLength + "\n", 
                        		Preferences.DEBUG_FILEIO);
                        bp += pluginLength;
                        dType = (short) (((b[bp+1] & 0xff) << 8) | (b[bp] & 0xff));
                        bp += 2;
                        if (dType == VT_BLOB) {
                            Preferences.debug("Expected VT_BLOB data type for {Others}\n", Preferences.DEBUG_FILEIO);
                        }
                        else {
                            Preferences.debug("dType = " + dType + " instead of expected VT_BLOB for {Others}\n", 
                            		Preferences.DEBUG_FILEIO);
                            break;
                        }
                        int othersLength = (((b[bp + 3] & 0xff) << 24) | ((b[bp + 2] & 0xff) << 16) | 
                                ((b[bp + 1] & 0xff) << 8) | (b[bp] & 0xff));
                        bp += 4;
                        Preferences.debug("The length of the {Others} binary data = " + othersLength + "\n", 
                        		Preferences.DEBUG_FILEIO);
                        bp += othersLength;
                        dType = (short) (((b[bp+1] & 0xff) << 8) | (b[bp] & 0xff));
                        bp += 2;
                        if (dType == VT_STORED_OBJECT) {
                            Preferences.debug("Expected VT_STORED_OBJECT data type for {Layers}\n", Preferences.DEBUG_FILEIO);
                        }
                        else {
                            Preferences.debug("dType = " + dType + " instead of expected VT_STORED_OBJECT for {Layers}\n", 
                            		Preferences.DEBUG_FILEIO);
                            break;
                        }
                        int layersLength =  (((b[bp + 3] & 0xff) << 24) | ((b[bp + 2] & 0xff) << 16) | 
                                ((b[bp + 1] & 0xff) << 8) | (b[bp] & 0xff));
                        bp += 4;
                        Preferences.debug("Byte length of unicode string in Layers stored object = " + layersLength + "\n", 
                        		Preferences.DEBUG_FILEIO);
                        byte obj[] = new byte[layersLength];
                        for (i = 0; i < layersLength; i++) {
                            obj[i] = b[bp++];
                        }
                        String str = new String(obj, "UTF-16LE").trim();
                        Preferences.debug("Name of the storage containing the vector overlay layers = " + str + "\n", 
                        		Preferences.DEBUG_FILEIO);
                        dType = (short) (((b[bp+1] & 0xff) << 8) | (b[bp] & 0xff));
                        bp += 2;
                        if (dType == VT_STORED_OBJECT) {
                            Preferences.debug("Expected VT_STORED_OBJECT data type for {Tags}\n", Preferences.DEBUG_FILEIO);
                        }
                        else {
                            Preferences.debug("dType = " + dType + " instead of expected VT_STORED_OBJECT for {Tags}\n", 
                            		Preferences.DEBUG_FILEIO);
                            break;
                        }
                        int tagsLength =  (((b[bp + 3] & 0xff) << 24) | ((b[bp + 2] & 0xff) << 16) | 
                                ((b[bp + 1] & 0xff) << 8) | (b[bp] & 0xff));
                        bp += 4;
                        Preferences.debug("Byte length of unicode string in Tags stored object = " + tagsLength + "\n", 
                        		Preferences.DEBUG_FILEIO);
                        obj = new byte[tagsLength];
                        for (i = 0; i < tagsLength; i++) {
                            obj[i] = b[bp++];
                        }
                        str = new String(obj, "UTF-16LE").trim();
                        Preferences.debug("Name of the storage containing the Tags information = " + str + "\n", 
                        		Preferences.DEBUG_FILEIO);
                        dType = (short) (((b[bp+1] & 0xff) << 8) | (b[bp] & 0xff));
                        bp += 2;
                        if (dType == VT_STORED_OBJECT) {
                            Preferences.debug("Expected VT_STORED_OBJECT data type for {Scaling}\n", Preferences.DEBUG_FILEIO);
                        }
                        else {
                            Preferences.debug("dType = " + dType + " instead of expected VT_STORED_OBJECT for {Scaling}\n", 
                            		Preferences.DEBUG_FILEIO);
                            break;
                        }
                        int scalingLength =  (((b[bp + 3] & 0xff) << 24) | ((b[bp + 2] & 0xff) << 16) | 
                                ((b[bp + 1] & 0xff) << 8) | (b[bp] & 0xff));
                        bp += 4;
                        Preferences.debug("Byte length of unicode string in Scaling stored object = " + scalingLength + "\n", 
                        		Preferences.DEBUG_FILEIO);
                        obj = new byte[scalingLength];
                        for (i = 0; i < scalingLength; i++) {
                            obj[i] = b[bp++];
                        }
                        str = new String(obj, "UTF-16LE").trim();
                        Preferences.debug("Name of the storage containing the scaling information = " + str + "\n", 
                        		Preferences.DEBUG_FILEIO);
                        dType = (short) (((b[bp+1] & 0xff) << 8) | (b[bp] & 0xff));
                        bp += 2;
                        if (dType == VT_STORED_OBJECT) {
                            Preferences.debug("Expected VT_STORED_OBJECT data type for {RootFloder}\n", Preferences.DEBUG_FILEIO);
                        }
                        else {
                            Preferences.debug("dType = " + dType + " instead of expected VT_STORED_OBJECT for {RootFolder}\n", 
                            		Preferences.DEBUG_FILEIO);
                            break;
                        }
                        int rootFolderLength =  (((b[bp + 3] & 0xff) << 24) | ((b[bp + 2] & 0xff) << 16) | 
                                ((b[bp + 1] & 0xff) << 8) | (b[bp] & 0xff));
                        bp += 4;
                        Preferences.debug("Byte length of unicode string in RootFolder stored object = " + rootFolderLength + "\n", 
                        		Preferences.DEBUG_FILEIO);
                        obj = new byte[rootFolderLength];
                        for (i = 0; i < rootFolderLength; i++) {
                            obj[i] = b[bp++];
                        }
                        str = new String(obj, "UTF-16LE").trim();
                        Preferences.debug("Name of the storage containing a ZiFolder object with advanced information = " 
                                          + str + "\n", Preferences.DEBUG_FILEIO);
                        break;
                    } // else not readImagePixels
                } // while (true)
            } // if ((lastElementName.equals("Image")) &&
            
            if ((lastElementName.length() > 4) && (lastElementName.substring(0,4).equals("Item")) &&
                    (elementName.equals("Contents")) && (objectType[0] == 2) && (streamSize >= sliceBytes) &&
                    ((twoAgoElementName == null) || (!twoAgoElementName.equals("Image")))) {
                
                Preferences.debug("Reading Contents of " + lastElementName + "\n", Preferences.DEBUG_FILEIO);
                startSectorArray[ap] = startSect;
                bytesToRead = (int)streamSize;
                Preferences.debug("bytesToRead = " + bytesToRead + "\n", Preferences.DEBUG_FILEIO);
                b = new byte[bytesToRead];
                bytesRead = 0;
                    if (streamSize < miniSectorCutoff) {
                        presentShortSector = startSect;
                        while (bytesToRead > 0) {
                            sectorsIntoShortStream = presentShortSector*shortSectorSize/sectorSize;
                            presentSector = shortSectors[sectorsIntoShortStream];
                            presentSectorOffset = (presentShortSector*shortSectorSize) % sectorSize;
                            if (add128) {
                                raFile.seek((presentSector+1)*sectorSize + 128 + presentSectorOffset);
                            }
                            else {
                                raFile.seek((presentSector+1)*sectorSize + presentSectorOffset);    
                            }
                            raFile.read(b, bytesRead, Math.min(shortSectorSize, bytesToRead));
                            bytesRead += Math.min(shortSectorSize, bytesToRead);
                            bytesToRead -= Math.min(shortSectorSize, bytesToRead);
                            presentShortSector = shortSectorTable[presentShortSector];
                        }
                } // if (streamSize < miniSectorCutoff)
                else { // else streamSize >= miniSectorCutoff
                    // One sector is plenty to read for getting the data fields before the raw image data
                    if (add128) {
                        raFile.seek((startSect+1)*sectorSize + 128);
                    }
                    else {
                        raFile.seek((startSect+1)*sectorSize);
                    }
                    raFile.read(b, 0, Math.min(sectorSize, bytesToRead));
                    /*presentSector = startSect;
                    while (bytesToRead > 0) {
                        raFile.seek((presentSector+1)*sectorSize);
                        raFile.read(b, bytesRead, Math.min(sectorSize, bytesToRead));
                        bytesRead += Math.min(sectorSize, bytesToRead);
                        bytesToRead -= Math.min(sectorSize, bytesToRead);
                        presentSector = sat[presentSector];
                    } */   
                } // else streamSize >= miniSectorCutoff
                while (true) {
                    bp = 0;
                    dType = (short) (((b[bp+1] & 0xff) << 8) | (b[bp] & 0xff));
                    bp += 2;
                    if (dType == VT_I4) {
                        Preferences.debug("Expected VT_I4 data type for version\n", Preferences.DEBUG_FILEIO);
                    }
                    else {
                        Preferences.debug("dType = " + dType + " instead of expected VT_I4 for version\n", 
                        		Preferences.DEBUG_FILEIO);
                        break;
                    }
                    minorVersion =  (((b[bp+1] & 0xff) << 8) | (b[bp] & 0xff));
                    bp += 2;
                    Preferences.debug("Minor version is " + minorVersion + "\n", Preferences.DEBUG_FILEIO);
                    Preferences.debug("Current version is 4099\n", Preferences.DEBUG_FILEIO);
                    majorVersion =  (((b[bp+1] & 0xff) << 8) | (b[bp] & 0xff));
                    bp += 2;
                    Preferences.debug("Major version is " + majorVersion + "\n", Preferences.DEBUG_FILEIO);
                    Preferences.debug("Current version is 12288\n", Preferences.DEBUG_FILEIO);
                    dType = (short) (((b[bp+1] & 0xff) << 8) | (b[bp] & 0xff));
                    bp += 2;
                    if (dType == VT_I4) {
                        Preferences.debug("Expected VT_I4 data type for file type\n", Preferences.DEBUG_FILEIO);
                    }
                    else {
                        Preferences.debug("dType = " + dType + " instead of expected VT_I4 for file type\n", 
                        		Preferences.DEBUG_FILEIO);
                        break;
                    }
                    // File type not used
                    int fileType = (((b[bp + 3] & 0xff) << 24) | ((b[bp + 2] & 0xff) << 16) | 
                            ((b[bp + 1] & 0xff) << 8) | (b[bp] & 0xff));
                    bp += 4;
                    Preferences.debug("Unused file type = " + fileType + "\n", Preferences.DEBUG_FILEIO);
                    dType = (short) (((b[bp+1] & 0xff) << 8) | (b[bp] & 0xff));
                    bp += 2;
                    if (dType == VT_EMPTY) {
                        Preferences.debug("Expected VT_EMPTY data type for unused type description\n", Preferences.DEBUG_FILEIO);
                    }
                    else if (dType == VT_BSTR) {
                        Preferences.debug("VT_BSTR for type description\n", Preferences.DEBUG_FILEIO);
                        stringBytes = (((b[bp + 3] & 0xff) << 24) | ((b[bp + 2] & 0xff) << 16) | 
                                ((b[bp + 1] & 0xff) << 8) | (b[bp] & 0xff));
                        bp += 4;
                        bf = new byte[stringBytes];
                        for (i = 0; i < stringBytes; i++) {
                            bf[i] = b[bp++];
                        }
                        String typeDescription = new String(b, "UTF-16LE").trim();
                        Preferences.debug("Type description = " + typeDescription + "\n", Preferences.DEBUG_FILEIO);
                    }
                    else {
                        Preferences.debug("dType = " + dType + 
                                " instead of expected VT_EMPTY or VT_BSTR for type description\n", Preferences.DEBUG_FILEIO);
                        break;
                    }
                    dType = (short) (((b[bp+1] & 0xff) << 8) | (b[bp] & 0xff));
                    bp += 2;
                    if (dType == VT_EMPTY) {
                        Preferences.debug("Expected VT_EMPTY data type for name of zvi file\n", Preferences.DEBUG_FILEIO);
                    }
                    else if (dType == VT_BSTR) {
                        Preferences.debug("VT_BSTR for name of zvi file\n", Preferences.DEBUG_FILEIO);
                        stringBytes = (((b[bp + 3] & 0xff) << 24) | ((b[bp + 2] & 0xff) << 16) | 
                                ((b[bp + 1] & 0xff) << 8) | (b[bp] & 0xff));
                        bp += 4;
                        bf = new byte[stringBytes];
                        for (i = 0; i < stringBytes; i++) {
                            bf[i] = b[bp++];
                        }
                        String fileName = new String(b, "UTF-16LE").trim();
                        Preferences.debug("Name of zvi file = " + fileName + "\n", Preferences.DEBUG_FILEIO);
                    }
                    else {
                        Preferences.debug("dType = " + dType + 
                                " instead of expected VT_EMPTY or VT_BSTR for name of zvi file\n", Preferences.DEBUG_FILEIO);
                        break;
                    }
                    dType = (short) (((b[bp+1] & 0xff) << 8) | (b[bp] & 0xff));
                    bp += 2;
                    if (dType == VT_I4) {
                        Preferences.debug("Expected VT_I4 data type for width\n", Preferences.DEBUG_FILEIO);
                    }
                    else {
                        Preferences.debug("dType = " + dType + " instead of expected VT_I4 for width\n", 
                        		Preferences.DEBUG_FILEIO);
                        break;
                    }
                    width = (((b[bp + 3] & 0xff) << 24) | ((b[bp + 2] & 0xff) << 16) | 
                            ((b[bp + 1] & 0xff) << 8) | (b[bp] & 0xff));
                    bp += 4;
                    Preferences.debug("Width = " + width + "\n", Preferences.DEBUG_FILEIO);
                    dType = (short) (((b[bp+1] & 0xff) << 8) | (b[bp] & 0xff));
                    bp += 2;
                    if (dType == VT_I4) {
                        Preferences.debug("Expected VT_I4 data type for height\n", Preferences.DEBUG_FILEIO);
                    }
                    else {
                        Preferences.debug("dType = " + dType + " instead of expected VT_I4 for height\n", 
                        		Preferences.DEBUG_FILEIO);
                        break;
                    }
                    height = (((b[bp + 3] & 0xff) << 24) | ((b[bp + 2] & 0xff) << 16) | 
                            ((b[bp + 1] & 0xff) << 8) | (b[bp] & 0xff));
                    bp += 4;
                    Preferences.debug("Height = " + height + "\n", Preferences.DEBUG_FILEIO);
                    dType = (short) (((b[bp+1] & 0xff) << 8) | (b[bp] & 0xff));
                    bp += 2;
                    if (dType == VT_I4) {
                        Preferences.debug("Expected VT_I4 data type for depth\n", Preferences.DEBUG_FILEIO);
                    }
                    else {
                        Preferences.debug("dType = " + dType + " instead of expected VT_I4 for depth\n", 
                        		Preferences.DEBUG_FILEIO);
                        break;
                    }
                    depth = (((b[bp + 3] & 0xff) << 24) | ((b[bp + 2] & 0xff) << 16) | 
                            ((b[bp + 1] & 0xff) << 8) | (b[bp] & 0xff));
                    bp += 4;
                    Preferences.debug("Unused depth = " + depth + "\n", Preferences.DEBUG_FILEIO);
                    dType = (short) (((b[bp+1] & 0xff) << 8) | (b[bp] & 0xff));
                    bp += 2;
                    if (dType == VT_I4) {
                        Preferences.debug("Expected VT_I4 data type for pixelFormat\n", Preferences.DEBUG_FILEIO);
                    }
                    else {
                        Preferences.debug("dType = " + dType + " instead of expected VT_I4 for pixelFormat\n", 
                        		Preferences.DEBUG_FILEIO);
                        break;
                    }
                    pixelFormat = (((b[bp + 3] & 0xff) << 24) | ((b[bp + 2] & 0xff) << 16) | 
                            ((b[bp + 1] & 0xff) << 8) | (b[bp] & 0xff));
                    bp += 4;
                    displayPixelFormat(pixelFormat);
                    dType = (short) (((b[bp+1] & 0xff) << 8) | (b[bp] & 0xff));
                    bp += 2;
                    if (dType == VT_I4) {
                        Preferences.debug("Expected VT_I4 data type for count\n", Preferences.DEBUG_FILEIO);
                    }
                    else {
                        Preferences.debug("dType = " + dType + " instead of expected VT_I4 for count\n", 
                        		Preferences.DEBUG_FILEIO);
                        break;
                    }
                    count = (((b[bp + 3] & 0xff) << 24) | ((b[bp + 2] & 0xff) << 16) | 
                            ((b[bp + 1] & 0xff) << 8) | (b[bp] & 0xff));
                    bp += 4;
                    if (count == 0) {
                        Preferences.debug("Count = 0 as expected\n", Preferences.DEBUG_FILEIO);
                    }
                    else {
                        Preferences.debug("Count = " + count + " instead of the expected 0\n", Preferences.DEBUG_FILEIO);
                    }
                    dType = (short) (((b[bp+1] & 0xff) << 8) | (b[bp] & 0xff));
                    bp += 2;
                    if (dType == VT_I4) {
                        Preferences.debug("Expected VT_I4 data type for validBitsPerPixel\n", Preferences.DEBUG_FILEIO);
                    }
                    else {
                        Preferences.debug("dType = " + dType + " instead of expected VT_I4 for validBitsPerPixel\n", 
                        		Preferences.DEBUG_FILEIO);
                        break;
                    }
                    // Valid bits per pixel in raw image data if 16 bit image (may be 12, 14, or 16)
                    validBitsPerPixel = (((b[bp + 3] & 0xff) << 24) | ((b[bp + 2] & 0xff) << 16) | 
                            ((b[bp + 1] & 0xff) << 8) | (b[bp] & 0xff));
                    bp += 4;
                    Preferences.debug("Valid bits per pixel in raw image data = " + validBitsPerPixel + "\n", 
                    		Preferences.DEBUG_FILEIO);
                    dType = (short) (((b[bp+1] & 0xff) << 8) | (b[bp] & 0xff));
                    bp += 2;
                    // Note that the ZVI Format Specification V 2.0.4 - June, 2009 incorrectly specifies a 
                    // VT_CLSID for m_PluginCLSID.
                    if (dType == VT_BLOB) {
                        Preferences.debug("Expected VT_BLOB data type for m_PluginCSLID\n", Preferences.DEBUG_FILEIO);
                    }
                    else {
                        Preferences.debug("dType = " + dType + " instead of expected VT_BLOB for m_pluginCLSID\n", 
                        		Preferences.DEBUG_FILEIO);
                        break;
                    }
                    int pluginLength = (((b[bp + 3] & 0xff) << 24) | ((b[bp + 2] & 0xff) << 16) | 
                            ((b[bp + 1] & 0xff) << 8) | (b[bp] & 0xff));
                    bp += 4;
                    Preferences.debug("Length of binary data for m_pluginCLSID = " + pluginLength + "\n", 
                    		Preferences.DEBUG_FILEIO);
                    // Skip over m_pluginCLSID data field
                    bp += pluginLength;
                    dType = (short) (((b[bp+1] & 0xff) << 8) | (b[bp] & 0xff));
                    bp += 2;
                    if (dType == VT_BLOB) {
                        Preferences.debug("Expected VT_BLOB data type for coordinate block stored as {Others}\n", 
                        		Preferences.DEBUG_FILEIO);
                    }
                    else {
                        Preferences.debug("dType = " + dType + 
                                " instead of expected VT_BLOB for coordinate block stored as {Others}\n", 
                                Preferences.DEBUG_FILEIO);
                        break;
                    }
                    int othersLength = (((b[bp + 3] & 0xff) << 24) | ((b[bp + 2] & 0xff) << 16) | 
                            ((b[bp + 1] & 0xff) << 8) | (b[bp] & 0xff));
                    bp += 4;
                    Preferences.debug("The {Others} length = " + othersLength + "\n", Preferences.DEBUG_FILEIO);
                    int U = (((b[bp + 3] & 0xff) << 24) | ((b[bp + 2] & 0xff) << 16) | 
                            ((b[bp + 1] & 0xff) << 8) | (b[bp] & 0xff));
                    bp += 4;
                    if (U == 0) {
                        Preferences.debug("The U tile ID is 0 as expected\n", Preferences.DEBUG_FILEIO);
                    }
                    else {
                        Preferences.debug("The U tile ID = " + U + " instead of the expected 0\n", Preferences.DEBUG_FILEIO);
                    }
                    int V = (((b[bp + 3] & 0xff) << 24) | ((b[bp + 2] & 0xff) << 16) | 
                            ((b[bp + 1] & 0xff) << 8) | (b[bp] & 0xff));
                    bp += 4;
                    if (V == 0) {
                        Preferences.debug("The V tile ID is 0 as expected\n", Preferences.DEBUG_FILEIO);
                    }
                    else {
                        Preferences.debug("The V tile ID = " + V + " instead of the expected 0\n", Preferences.DEBUG_FILEIO);
                    }
                    // Z ID
                    zArray[ap] = (((b[bp + 3] & 0xff) << 24) | ((b[bp + 2] & 0xff) << 16) | 
                            ((b[bp + 1] & 0xff) << 8) | (b[bp] & 0xff));
                    bp += 4;
                    Preferences.debug("Z ID = " + zArray[ap] + "\n", Preferences.DEBUG_FILEIO);
                    // Channel ID
                    cArray[ap] = (((b[bp + 3] & 0xff) << 24) | ((b[bp + 2] & 0xff) << 16) | 
                            ((b[bp + 1] & 0xff) << 8) | (b[bp] & 0xff));
                    bp += 4;
                    Preferences.debug("Channel ID = " + cArray[ap] + "\n", Preferences.DEBUG_FILEIO);
                    // Time ID
                    tArray[ap] = (((b[bp + 3] & 0xff) << 24) | ((b[bp + 2] & 0xff) << 16) | 
                            ((b[bp + 1] & 0xff) << 8) | (b[bp] & 0xff));
                    bp += 4;
                    Preferences.debug("Time ID = " + tArray[ap] + "\n", Preferences.DEBUG_FILEIO);
                    int sceneID = (((b[bp + 3] & 0xff) << 24) | ((b[bp + 2] & 0xff) << 16) | 
                            ((b[bp + 1] & 0xff) << 8) | (b[bp] & 0xff));
                    bp += 4;
                    Preferences.debug("Scene ID = " + sceneID + "\n", Preferences.DEBUG_FILEIO);
                    positionArray[ap] = (((b[bp + 3] & 0xff) << 24) | ((b[bp + 2] & 0xff) << 16) | 
                            ((b[bp + 1] & 0xff) << 8) | (b[bp] & 0xff));
                    bp += 4;
                    Preferences.debug("Position ID = " + positionArray[ap] + "\n", Preferences.DEBUG_FILEIO);
                    int A = (((b[bp + 3] & 0xff) << 24) | ((b[bp + 2] & 0xff) << 16) | 
                            ((b[bp + 1] & 0xff) << 8) | (b[bp] & 0xff));
                    bp += 4;
                    Preferences.debug("Unused A = " + A + "\n", Preferences.DEBUG_FILEIO);
                    // othersLength - 8 integers in coordinate block
                    bp += othersLength - 32;
                    int bytesToImageHeader = 88;
                    while (bytesToImageHeader > 0) {
                        dType = (short) (((b[bp+1] & 0xff) << 8) | (b[bp] & 0xff));
                        bp += 2;
                        bytesToImageHeader -= 2;
                        if (dType == VT_DISPATCH) {
                            Preferences.debug("dType = VT_DISPATCH\n", Preferences.DEBUG_FILEIO);
                            bp += Math.min(bytesToImageHeader, 16);
                            bytesToImageHeader -= Math.min(bytesToImageHeader, 16);
                        }
                        else if (dType == VT_STORED_OBJECT) {
                            Preferences.debug("dType = VT_STORED_OBJECT\n", Preferences.DEBUG_FILEIO);
                            int objectLength =  (((b[bp + 3] & 0xff) << 24) | ((b[bp + 2] & 0xff) << 16) | 
                                    ((b[bp + 1] & 0xff) << 8) | (b[bp] & 0xff));
                            bp += 4;
                            bytesToImageHeader -= 4;
                            Preferences.debug("Byte length of unicode string in stored object = " + objectLength + "\n", 
                            		Preferences.DEBUG_FILEIO);
                            byte obj[] = new byte[Math.min(objectLength, bytesToImageHeader)];
                            for (i = 0; i < Math.min(objectLength, bytesToImageHeader); i++) {
                                obj[i] = b[bp++];
                            }
                            bytesToImageHeader -= Math.min(objectLength, bytesToImageHeader);
                            String str = new String(obj, "UTF-16LE").trim();
                            Preferences.debug("Object string = " + str + "\n", Preferences.DEBUG_FILEIO);
                        }
                    } // while (bytesToImageHeader > 0)
                    // Read the image header at 296 bytes from the stream beginning
                    // Don't read dType any more
                    Preferences.debug("Reading the image header\n", Preferences.DEBUG_FILEIO);
                    // Stream version ID
                    // 2 byte minor 0x2000 followed by 2 byte major 0x1000
                    minorVersion = (((b[bp+1] & 0xff) << 8) | (b[bp] & 0xff));
                    bp += 2;
                    Preferences.debug("The minor version = " + minorVersion + "\n", Preferences.DEBUG_FILEIO);
                    Preferences.debug("The current minor version is 8192\n", Preferences.DEBUG_FILEIO);
                    majorVersion = (((b[bp+1] & 0xff) << 8) | (b[bp] & 0xff));
                    bp += 2;
                    Preferences.debug("The major version = " + majorVersion + "\n", Preferences.DEBUG_FILEIO);
                    Preferences.debug("The current major version is 4096\n", Preferences.DEBUG_FILEIO);
                    width = (((b[bp + 3] & 0xff) << 24) | ((b[bp + 2] & 0xff) << 16) | 
                            ((b[bp + 1] & 0xff) << 8) | (b[bp] & 0xff));
                    bp += 4;
                    Preferences.debug("Width = " + width + "\n", Preferences.DEBUG_FILEIO);
                    height = (((b[bp + 3] & 0xff) << 24) | ((b[bp + 2] & 0xff) << 16) | 
                            ((b[bp + 1] & 0xff) << 8) | (b[bp] & 0xff));
                    bp += 4;
                    Preferences.debug("Height = " + height + "\n", Preferences.DEBUG_FILEIO);
                    depth = (((b[bp + 3] & 0xff) << 24) | ((b[bp + 2] & 0xff) << 16) | 
                            ((b[bp + 1] & 0xff) << 8) | (b[bp] & 0xff));
                    bp += 4;
                    Preferences.debug("Unused depth = " + depth + "\n", Preferences.DEBUG_FILEIO);
                    pixelWidth = (((b[bp + 3] & 0xff) << 24) | ((b[bp + 2] & 0xff) << 16) | 
                            ((b[bp + 1] & 0xff) << 8) | (b[bp] & 0xff));
                    bp += 4;
                    Preferences.debug("Size in bytes of an image pixel = " + pixelWidth + "\n", Preferences.DEBUG_FILEIO);
                    pixelFormat = (((b[bp + 3] & 0xff) << 24) | ((b[bp + 2] & 0xff) << 16) | 
                            ((b[bp + 1] & 0xff) << 8) | (b[bp] & 0xff));
                    bp += 4;
                    displayPixelFormat(pixelFormat);
                    // Valid bits per pixel in raw image data if 16 bit image (may be 12, 14, or 16)
                    validBitsPerPixel = (((b[bp + 3] & 0xff) << 24) | ((b[bp + 2] & 0xff) << 16) | 
                            ((b[bp + 1] & 0xff) << 8) | (b[bp] & 0xff));
                    bp += 4;
                    Preferences.debug("Valid bits per pixel in raw image data = " + validBitsPerPixel + "\n", 
                    		Preferences.DEBUG_FILEIO);
                    // Raw bytes for image slice are stored here
                    // Store the offset into the starting sector
                    offsetArray[ap++] = bp;
                    break;
                } // while (true)
            } // if ((lastElementName.length() > 4) && (lastElementName.substring(0,4).equals("Item")) &&
            
            if ((lastElementName.equals("Scaling")) &&
                    (elementName.equals("Contents")) && (objectType[0] == 2) && (streamSize > 0)) {
                Preferences.debug("Reading the contents stream of the Scaling storage\n", Preferences.DEBUG_FILEIO);
                      
                bytesToRead = (int)streamSize;
                b = new byte[bytesToRead];
                bytesRead = 0;
                    if (streamSize < miniSectorCutoff) {
                        presentShortSector = startSect;
                        while (bytesToRead > 0) {
                            sectorsIntoShortStream = presentShortSector*shortSectorSize/sectorSize;
                            presentSector = shortSectors[sectorsIntoShortStream];
                            presentSectorOffset = (presentShortSector*shortSectorSize) % sectorSize;
                            if (add128) {
                                raFile.seek((presentSector+1)*sectorSize + 128 + presentSectorOffset);
                            }
                            else {
                                raFile.seek((presentSector+1)*sectorSize + presentSectorOffset);    
                            }
                            raFile.read(b, bytesRead, Math.min(shortSectorSize, bytesToRead));
                            bytesRead += Math.min(shortSectorSize, bytesToRead);
                            bytesToRead -= Math.min(shortSectorSize, bytesToRead);
                            presentShortSector = shortSectorTable[presentShortSector];
                        }
                } // if (streamSize < miniSectorCutoff)
                else { // else streamSize >= miniSectorCutoff
                    presentSector = startSect;
                    while (bytesToRead > 0) {
                        if (add128) {
                            raFile.seek((presentSector+1)*sectorSize + 128);
                        }
                        else {
                            raFile.seek((presentSector+1)*sectorSize);   
                        }
                        raFile.read(b, bytesRead, Math.min(sectorSize, bytesToRead));
                        bytesRead += Math.min(sectorSize, bytesToRead);
                        bytesToRead -= Math.min(sectorSize, bytesToRead);
                        presentSector = sat[presentSector];
                    }    
                } // else streamSize >= miniSectorCutoff
                while (true) {
                    bp = 0;
                    dType = (short) (((b[bp+1] & 0xff) << 8) | (b[bp] & 0xff));
                    bp += 2;
                    if (dType == VT_I4) {
                        Preferences.debug("Expected VT_I4 data type for version\n", Preferences.DEBUG_FILEIO);
                    }
                    else {
                        Preferences.debug("dType = " + dType + " instead of expected VT_I4 for version\n", 
                        		Preferences.DEBUG_FILEIO);
                        break;
                    }
                    minorVersion =  (((b[bp+1] & 0xff) << 8) | (b[bp] & 0xff));
                    bp += 2;
                    Preferences.debug("Minor version is " + minorVersion + "\n", Preferences.DEBUG_FILEIO);
                    Preferences.debug("Current version is 4098\n", Preferences.DEBUG_FILEIO);
                    majorVersion =  (((b[bp+1] & 0xff) << 8) | (b[bp] & 0xff));
                    bp += 2;
                    Preferences.debug("Major version is " + majorVersion + "\n", Preferences.DEBUG_FILEIO);
                    Preferences.debug("Current version is 8193\n", Preferences.DEBUG_FILEIO);
                    dType = (short) (((b[bp+1] & 0xff) << 8) | (b[bp] & 0xff));
                    bp += 2;
                    if (dType == VT_BSTR) {
                        Preferences.debug("Expected VT_BSTR data type for original key name\n", Preferences.DEBUG_FILEIO);
                    }
                    else {
                        Preferences.debug("dType = " + dType + " instead of expected VT_BSTR for original key name\n", 
                        		Preferences.DEBUG_FILEIO);
                        break;
                    }
                    stringBytes = (((b[bp + 3] & 0xff) << 24) | ((b[bp + 2] & 0xff) << 16) | 
                            ((b[bp + 1] & 0xff) << 8) | (b[bp] & 0xff));
                    bp += 4;
                    bf = new byte[stringBytes];
                    for (i = 0; i < stringBytes; i++) {
                        bf[i] = b[bp++];
                    }
                    String originalKeyName = new String(b, "UTF-16LE").trim();
                    Preferences.debug("Original key name = " + originalKeyName + "\n", Preferences.DEBUG_FILEIO);
                    dType = (short) (((b[bp+1] & 0xff) << 8) | (b[bp] & 0xff));
                    bp += 2;
                    if (dType == VT_I4) {
                        Preferences.debug("Expected VT_I4 data type for unused scaling category\n", Preferences.DEBUG_FILEIO);
                    }
                    else {
                        Preferences.debug("dType = " + dType + " instead of expected VT_I4 for unused scaling category\n", 
                        		Preferences.DEBUG_FILEIO);
                        break;
                    }
                    int scalingCategory = (((b[bp + 3] & 0xff) << 24) | ((b[bp + 2] & 0xff) << 16) | 
                            ((b[bp + 1] & 0xff) << 8) | (b[bp] & 0xff));
                    bp += 4;
                    Preferences.debug("Unused scaling category = " + scalingCategory + "\n", Preferences.DEBUG_FILEIO);
                    dType = (short) (((b[bp+1] & 0xff) << 8) | (b[bp] & 0xff));
                    bp += 2;
                    if (dType == VT_R8) {
                        Preferences.debug("Expected VT_R8 data type for scaling factor(units per pixel)\n", 
                        		Preferences.DEBUG_FILEIO);
                    }
                    else {
                        Preferences.debug("dType = " + dType + " instead of expected VT_R8 for unused scaling factor\n", 
                        		Preferences.DEBUG_FILEIO);
                        break;
                    }
                    tmpLong = (((b[bp+7] & 0xffL) << 56) | ((b[bp+6] & 0xffL) << 48) | ((b[bp+5] & 0xffL) << 40) | 
                            ((b[bp+4] & 0xffL) << 32) | ((b[bp+3] & 0xffL) << 24) | ((b[bp+2] & 0xffL) << 16) |
                            ((b[bp+1] & 0xffL) << 8) | (b[bp] & 0xffL));
                    bp += 8;
                    double scalingFactor = Double.longBitsToDouble(tmpLong);
                    Preferences.debug("Scaling factor (units per pixel) = " + scalingFactor + "\n", Preferences.DEBUG_FILEIO);
                    fileInfo.setResolutions((float)scalingFactor, 0);
                    fileInfo.setResolutions((float)scalingFactor, 1);
                    dType = (short) (((b[bp+1] & 0xff) << 8) | (b[bp] & 0xff));
                    bp += 2;
                    if (dType == VT_I4) {
                        Preferences.debug("Expected VT_I4 data type for scaling unit type\n", Preferences.DEBUG_FILEIO);
                    }
                    else {
                        Preferences.debug("dType = " + dType + " instead of expected VT_I4 for scaling unit type\n", 
                        		Preferences.DEBUG_FILEIO);
                        break;
                    }
                    int scalingUnitType = (((b[bp + 3] & 0xff) << 24) | ((b[bp + 2] & 0xff) << 16) | 
                            ((b[bp + 1] & 0xff) << 8) | (b[bp] & 0xff));
                    bp += 4;
                    measureUnits = zviToMipavMeasurementUnits(scalingUnitType);
                    fileInfo.setUnitsOfMeasure(measureUnits, 0);
                    fileInfo.setUnitsOfMeasure(measureUnits, 1);
                    
                    break;
                } // while (true)
            } // if ((lastElementName.equals("Scaling")) &&
            
            if ((lastElementName.equals("Tags")) &&
                    (elementName.equals("Contents")) && (objectType[0] == 2) && (streamSize > 0)) {
                Preferences.debug("Reading the contents stream of Tags storage\n", Preferences.DEBUG_FILEIO);
                      
                bytesToRead = (int)streamSize;
                b = new byte[bytesToRead];
                bytesRead = 0;
                    if (streamSize < miniSectorCutoff) {
                        presentShortSector = startSect;
                        while (bytesToRead > 0) {
                            sectorsIntoShortStream = presentShortSector*shortSectorSize/sectorSize;
                            presentSector = shortSectors[sectorsIntoShortStream];
                            presentSectorOffset = (presentShortSector*shortSectorSize) % sectorSize;
                            if (add128) {
                                raFile.seek((presentSector+1)*sectorSize + 128 + presentSectorOffset);
                            }
                            else {
                                raFile.seek((presentSector+1)*sectorSize + presentSectorOffset);    
                            }
                            raFile.read(b, bytesRead, Math.min(shortSectorSize, bytesToRead));
                            bytesRead += Math.min(shortSectorSize, bytesToRead);
                            bytesToRead -= Math.min(shortSectorSize, bytesToRead);
                            presentShortSector = shortSectorTable[presentShortSector];
                        }
                } // if (streamSize < miniSectorCutoff)
                else { // else streamSize >= miniSectorCutoff
                    presentSector = startSect;
                    while (bytesToRead > 0) {
                        if (add128) {
                            raFile.seek((presentSector+1)*sectorSize + 128);
                        }
                        else {
                            raFile.seek((presentSector+1)*sectorSize);   
                        }
                        raFile.read(b, bytesRead, Math.min(sectorSize, bytesToRead));
                        bytesRead += Math.min(sectorSize, bytesToRead);
                        bytesToRead -= Math.min(sectorSize, bytesToRead);
                        presentSector = sat[presentSector];
                    }    
                } // else streamSize >= miniSectorCutoff
                trueLoop:
                while (true) {
                    bp = 0;
                    dType = (short) (((b[bp+1] & 0xff) << 8) | (b[bp] & 0xff));
                    bp += 2;
                    if (dType == VT_I4) {
                        Preferences.debug("Expected VT_I4 data type for version\n", Preferences.DEBUG_FILEIO);
                    }
                    else {
                        Preferences.debug("dType = " + dType + " instead of expected VT_I4 for version\n", 
                        		Preferences.DEBUG_FILEIO);
                        break;
                    }
                    minorVersion =  (((b[bp+1] & 0xff) << 8) | (b[bp] & 0xff));
                    bp += 2;
                    Preferences.debug("Minor version is " + minorVersion + "\n", Preferences.DEBUG_FILEIO);
                    Preferences.debug("Current version is 4096\n", Preferences.DEBUG_FILEIO);
                    majorVersion =  (((b[bp+1] & 0xff) << 8) | (b[bp] & 0xff));
                    bp += 2;
                    Preferences.debug("Major version is " + majorVersion + "\n", Preferences.DEBUG_FILEIO);
                    Preferences.debug("Current version is 8192\n", Preferences.DEBUG_FILEIO);
                    dType = (short) (((b[bp+1] & 0xff) << 8) | (b[bp] & 0xff));
                    bp += 2;
                    if (dType == VT_I4) {
                        Preferences.debug("Expected VT_I4 data type for count of token triples\n", Preferences.DEBUG_FILEIO);
                    }
                    else {
                        Preferences.debug("dType = " + dType + " instead of expected VT_I4 for count of token triples\n", 
                        		Preferences.DEBUG_FILEIO);
                        break;
                    }
                    int tokenCount = (((b[bp + 3] & 0xff) << 24) | ((b[bp + 2] & 0xff) << 16) | 
                            ((b[bp + 1] & 0xff) << 8) | (b[bp] & 0xff));
                    bp += 4;
                    Preferences.debug("Count of token triples = " + tokenCount + "\n", Preferences.DEBUG_FILEIO);
                   exposureTime = Double.NaN;
                   apotomeGridPosition = Integer.MIN_VALUE;
                   focusPosition = Double.NaN;
                   relFocusPosition1 = Double.NaN;
                   relFocusPosition2 = Double.NaN;
                   stagePositionX = Double.NaN;
                   stagePositionY = Double.NaN;
                   zValue = Integer.MIN_VALUE;
                   cValue = Integer.MIN_VALUE;
                   tValue = Integer.MIN_VALUE;
                   tileValue = Integer.MIN_VALUE;
                   blackValue = Double.NaN;
                   whiteValue = Double.NaN;
                   reflectorPosition = Integer.MIN_VALUE;
                   multichannelColor = Integer.MIN_VALUE;
                   excitationWavelength = Integer.MIN_VALUE;
                   emissionWavelength = Integer.MIN_VALUE;
                   acqTime = Double.NaN;
                   relTime = Double.NaN;
                    for (i = 0; i < tokenCount && bp < b.length - 13; i++) {
                        short valueDType = (short) (((b[bp+1] & 0xff) << 8) | (b[bp] & 0xff));
                        bp += 2;
                        switch(valueDType) {
                            case VT_EMPTY:
                                Preferences.debug("Data type of value is VT_EMPTY\n", Preferences.DEBUG_FILEIO);
                                break;
                            case VT_BOOL:
                                Preferences.debug("Data type of value is VT_BOOL\n", Preferences.DEBUG_FILEIO);
                                shortValue = (short) (((b[bp+1] & 0xff) << 8) | (b[bp] & 0xff));
                                if (shortValue != 0) {
                                    booleanValue = true;
                                }
                                else {
                                    booleanValue = false;
                                }
                                Preferences.debug("Value = " + booleanValue + "\n", Preferences.DEBUG_FILEIO);
                                bp += 2;
                                break;
                            case VT_I2:
                                Preferences.debug("Data type of value is VT_I2\n", Preferences.DEBUG_FILEIO);
                                shortValue = (short) (((b[bp+1] & 0xff) << 8) | (b[bp] & 0xff));
                                Preferences.debug("Value = " + shortValue + "\n");
                                bp += 2;
                                break;
                            case VT_I4:
                                Preferences.debug("Data type of value is VT_I4\n", Preferences.DEBUG_FILEIO);
                                intValue = (((b[bp + 3] & 0xff) << 24) | ((b[bp + 2] & 0xff) << 16) | 
                                           ((b[bp + 1] & 0xff) << 8) | (b[bp] & 0xff));
                                Preferences.debug("Value = " + intValue + "\n", Preferences.DEBUG_FILEIO);
                                bp += 4;
                                break;
                            case VT_R8:
                                Preferences.debug("Data type of value is VT_R8\n", Preferences.DEBUG_FILEIO);
                                tmpLong = (((b[bp+7] & 0xffL) << 56) | ((b[bp+6] & 0xffL) << 48) | ((b[bp+5] & 0xffL) << 40) | 
                                        ((b[bp+4] & 0xffL) << 32) | ((b[bp+3] & 0xffL) << 24) | ((b[bp+2] & 0xffL) << 16) |
                                        ((b[bp+1] & 0xffL) << 8) | (b[bp] & 0xffL));
                                bp += 8;
                                doubleValue = Double.longBitsToDouble(tmpLong);
                                Preferences.debug("Value = " + doubleValue + "\n", Preferences.DEBUG_FILEIO);
                                break;
                            case VT_BSTR:
                                Preferences.debug("Data type of value is VT_BSTR\n", Preferences.DEBUG_FILEIO);
                                stringBytes = (((b[bp + 3] & 0xff) << 24) | ((b[bp + 2] & 0xff) << 16) | 
                                        ((b[bp + 1] & 0xff) << 8) | (b[bp] & 0xff));
                                bp += 4;
                                bf = new byte[stringBytes];
                                for (i = 0; i < stringBytes; i++) {
                                    bf[i] = b[bp++];
                                }
                                stringValue = new String(b, "UTF-16LE").trim();
                                //Preferences.debug("Value = " + valueString + "\n", Preferences.DEBUG_FILEIO);
                                break;
                            case VT_STORED_OBJECT:
                                Preferences.debug("Data type of value is VT_STORED_OBJECT\n", Preferences.DEBUG_FILEIO);
                                stringBytes = (((b[bp + 3] & 0xff) << 24) | ((b[bp + 2] & 0xff) << 16) | 
                                        ((b[bp + 1] & 0xff) << 8) | (b[bp] & 0xff));
                                bp += 4;
                                bf = new byte[stringBytes];
                                for (i = 0; i < stringBytes; i++) {
                                    bf[i] = b[bp++];
                                }
                                stringValue = new String(b, "UTF-16LE").trim();
                                //Preferences.debug("Value = " + valueString + "\n", Preferences.DEBUG_FILEIO);
                                break;
                            case VT_DISPATCH:
                                Preferences.debug("Data type of value is VT_DISPATCH\n", Preferences.DEBUG_FILEIO);
                                bp += 16;
                                break;
                            case VT_DATE:
                                Preferences.debug("Data type of value is VT_DATE\n", Preferences.DEBUG_FILEIO);
                                tmpLong = (((b[bp+7] & 0xffL) << 56) | ((b[bp+6] & 0xffL) << 48) | ((b[bp+5] & 0xffL) << 40) | 
                                        ((b[bp+4] & 0xffL) << 32) | ((b[bp+3] & 0xffL) << 24) | ((b[bp+2] & 0xffL) << 16) |
                                        ((b[bp+1] & 0xffL) << 8) | (b[bp] & 0xffL));
                                bp += 8;
                                doubleValue = Double.longBitsToDouble(tmpLong);
                                Preferences.debug("Value = " + doubleValue + "\n", Preferences.DEBUG_FILEIO);
                                break;
                            default:
                                Preferences.debug("Unrecognized data type of value = " + valueDType + "\n", 
                                		Preferences.DEBUG_FILEIO);
                                break trueLoop;
                        } // switch(dType)
                        
                        dType = (short) (((b[bp+1] & 0xff) << 8) | (b[bp] & 0xff));
                        bp += 2;
                        if (dType == VT_I4) {
                            Preferences.debug("Expected VT_I4 data type for tagID\n", Preferences.DEBUG_FILEIO);
                        }
                        else {
                            Preferences.debug("dType = " + dType + " instead of expected VT_I4 for tagID\n", 
                            		Preferences.DEBUG_FILEIO);
                            break trueLoop;
                        }
                        int tagID = (((b[bp + 3] & 0xff) << 24) | ((b[bp + 2] & 0xff) << 16) | 
                                ((b[bp + 1] & 0xff) << 8) | (b[bp] & 0xff));
                        bp += 4;
                        switch(tagID) {
                            case 222:
                                Preferences.debug("tagID = Compression\n", Preferences.DEBUG_FILEIO);
                                break;
                            case 257:
                                Preferences.debug("tagID = Date mapping table\n", Preferences.DEBUG_FILEIO);
                                break;
                            case 258:
                                Preferences.debug("tagID = Black value\n", Preferences.DEBUG_FILEIO);
                                // A different value for every channel and z slice
                                if (valueDType == VT_R8) {
                                    blackValue = doubleValue;
                                }
                                break;
                            case 259:
                                Preferences.debug("tagID = White value\n", Preferences.DEBUG_FILEIO);
                                // A different value for every channel and z slice
                                if (valueDType == VT_R8) {
                                    whiteValue = doubleValue;
                                }
                                break;
                            case 260:
                                Preferences.debug("tagID = Image data mapping auto range\n", Preferences.DEBUG_FILEIO);
                                break;
                            case 261:
                                Preferences.debug("tagID = Image thumbnail\n", Preferences.DEBUG_FILEIO);
                                break;
                            case 262:
                                Preferences.debug("tagID = Gamma value\n", Preferences.DEBUG_FILEIO);
                                if (valueDType == VT_R8) {
                                    fileInfo.setGammaValue(doubleValue);
                                }
                                break;
                            case 264:
                                Preferences.debug("tagID = Image over exposure\n", Preferences.DEBUG_FILEIO);
                                break;
                            // Image relative times 1, 2, 3, and 4 are not implemented because in
                            // every sample .ZVI file that I possess they had the same values as
                            // image relative time.
                            case 265:
                                Preferences.debug("tagID = Image relative time 1\n", Preferences.DEBUG_FILEIO);
                                break;
                            case 266:
                                Preferences.debug("tagID = Image relative time 2\n", Preferences.DEBUG_FILEIO);
                                break;
                            case 267:
                                Preferences.debug("tagID = Image relative time 3\n", Preferences.DEBUG_FILEIO);
                                break;
                            case 268:
                                Preferences.debug("tagID = Image relative time 4\n", Preferences.DEBUG_FILEIO);
                                break;
                            case 300:
                                Preferences.debug("tagID = Image relative time\n", Preferences.DEBUG_FILEIO);
                                if (valueDType == VT_DATE) {
                                    relTime = doubleValue;
                                }
                                break;
                            case 301:
                                Preferences.debug("tagID = Image base time 1\n", Preferences.DEBUG_FILEIO);
                                break;
                            case 302:
                                Preferences.debug("tagID = Image base time 2\n", Preferences.DEBUG_FILEIO);
                                break;
                            case 303:
                                Preferences.debug("tagID = Image base time 3\n", Preferences.DEBUG_FILEIO);
                                break;
                            case 304:
                                Preferences.debug("tagID = Image base time 4\n", Preferences.DEBUG_FILEIO);
                                break;
                            case 333:
                                Preferences.debug("tagID = RelFocusPosition1\n", Preferences.DEBUG_FILEIO);
                                if (valueDType == VT_R8) {
                                    // Different value for every z slice
                                    relFocusPosition1 = doubleValue;
                                }
                                break;
                            case 334:
                                Preferences.debug("tagID = RelFocusPosition2\n", Preferences.DEBUG_FILEIO);
                                if (valueDType == VT_R8) {
                                    // Different value for every z slice
                                    relFocusPosition2 = doubleValue;    
                                }
                                break;
                            case 513:
                                Preferences.debug("tagID = Object type\n", Preferences.DEBUG_FILEIO);
                                if (valueDType == VT_I4) {
                                    fileInfo.setObjectType(intValue);
                                }
                                break;
                            case 515:
                                Preferences.debug("tagID = Image width in pixels\n", Preferences.DEBUG_FILEIO);
                                break;
                            case 516:
                                Preferences.debug("tagID = Image height in pixels\n", Preferences.DEBUG_FILEIO);
                                break;
                            case 517:
                                Preferences.debug("tagID = Image count raw\n", Preferences.DEBUG_FILEIO);
                                break;
                            case 518:
                                Preferences.debug("tagID = Pixel type\n", Preferences.DEBUG_FILEIO);
                                if (valueDType == VT_I4) {
                                    displayPixelFormat(intValue);
                                }
                                break;
                            case 519:
                                Preferences.debug("tagID = Number raw images\n", Preferences.DEBUG_FILEIO);
                                break;
                            case 520:
                                Preferences.debug("tagID = Image size\n", Preferences.DEBUG_FILEIO);
                                break;
                            case 521:
                                Preferences.debug("tagID = Compression factor for save\n", Preferences.DEBUG_FILEIO);
                                break;
                            case 522:
                                Preferences.debug("tagID = Document save flags\n", Preferences.DEBUG_FILEIO);
                                break;
                            case 523:
                                Preferences.debug("tagID = Acquisition pause annotation\n", Preferences.DEBUG_FILEIO);
                                break;
                            case 530:
                                Preferences.debug("tagID = Document subtype\n", Preferences.DEBUG_FILEIO);
                                break;
                            case 531:
                                Preferences.debug("tagID = Acquisition bit depth\n", Preferences.DEBUG_FILEIO);
                                if (valueDType == VT_I4) {
                                    fileInfo.setAcquisitionBitDepth(intValue);
                                }
                                break;
                            case 532:
                                Preferences.debug("tagID = Image memory usage (RAM)\n", Preferences.DEBUG_FILEIO);
                                if (valueDType == VT_R8) {
                                    fileInfo.setImageMemoryUsage(doubleValue);
                                }
                                break;
                            case 534:
                                Preferences.debug("tagID = Z-stack single representative\n", Preferences.DEBUG_FILEIO);
                                break;
                            case 769:
                                Preferences.debug("tagID = Scale factor for X\n", Preferences.DEBUG_FILEIO);
                                if ((valueDType == VT_R8) && (doubleValue != 1.0)) {
                                    fileInfo.setResolutions((float)doubleValue, 0);
                                }
                                break;
                            case 770:
                                Preferences.debug("tagID = Scale unit for X\n", Preferences.DEBUG_FILEIO);
                                if (valueDType == VT_I4) {
                                    measureUnits = zviToMipavMeasurementUnits(intValue);
                                    if (measureUnits != Unit.UNKNOWN_MEASURE.getLegacyNum()) {
                                        fileInfo.setUnitsOfMeasure(measureUnits, 0);
                                    }
                                }
                                break;
                            case 771:
                                Preferences.debug("tagID = Scale width\n", Preferences.DEBUG_FILEIO);
                                if ((valueDType == VT_R8) && (doubleValue != imageWidth)){
                                    fileInfo.setScaleWidth(doubleValue);
                                }
                                break;
                            case 772:
                                Preferences.debug("tagID = Scale factor for Y\n", Preferences.DEBUG_FILEIO);
                                if ((valueDType == VT_R8) && (doubleValue != 1.0)) {
                                    fileInfo.setResolutions((float)doubleValue, 1);
                                }
                                break;
                            case 773:
                                Preferences.debug("tagID = Scale unit for Y\n", Preferences.DEBUG_FILEIO);
                                if (valueDType == VT_I4) {
                                    measureUnits = zviToMipavMeasurementUnits(intValue);
                                    if (measureUnits != Unit.UNKNOWN_MEASURE.getLegacyNum()) {
                                            fileInfo.setUnitsOfMeasure(measureUnits, 1);
                                    }
                                }
                                break;
                            case 774:
                                Preferences.debug("tagID = Scale height\n", Preferences.DEBUG_FILEIO);
                                if ((valueDType == VT_R8) && (doubleValue != imageHeight)) {
                                    fileInfo.setScaleHeight(doubleValue);
                                }
                                break;
                            case 775:
                                Preferences.debug("tagID = Scale factor for Z\n", Preferences.DEBUG_FILEIO);
                                if ((valueDType == VT_R8) && (doubleValue != 1.0)) {
                                    fileInfo.setResolutions((float)doubleValue, 2);
                                }
                                break;
                            case 776:
                                Preferences.debug("tagID = Scale unit for Z\n", Preferences.DEBUG_FILEIO);
                                if (valueDType == VT_I4) {
                                    measureUnits = zviToMipavMeasurementUnits(intValue);
                                    if (measureUnits != Unit.UNKNOWN_MEASURE.getLegacyNum()) {
                                        fileInfo.setUnitsOfMeasure(measureUnits, 2);
                                    }
                                }
                                break;
                            case 777:
                                Preferences.debug("tagID = Scale depth\n", Preferences.DEBUG_FILEIO);
                                break;
                            case 778:
                                Preferences.debug("tagID = Scaling parent\n", Preferences.DEBUG_FILEIO);
                                break;
                            case 1001:
                                Preferences.debug("tagID = Date\n", Preferences.DEBUG_FILEIO);
                                break;
                            case 1002:
                                Preferences.debug("tagID = Code\n", Preferences.DEBUG_FILEIO);
                                break;
                            case 1003:
                                Preferences.debug("tagID = Source\n", Preferences.DEBUG_FILEIO);
                                break;
                            case 1004:
                                Preferences.debug("tagID = Message\n", Preferences.DEBUG_FILEIO);
                                break;
                            case 1025:
                                Preferences.debug("tagID = Camera image acquisition time\n", Preferences.DEBUG_FILEIO);
                                if (valueDType == VT_DATE) {
                                    acqTime = doubleValue;
                                }
                                break;
                            case 1026:
                                Preferences.debug("tagID = 8-bit acquisition\n", Preferences.DEBUG_FILEIO);
                                break;
                            case 1027:
                                Preferences.debug("tagID = Camera bit depth\n", Preferences.DEBUG_FILEIO);
                                break;
                            case 1029:
                                Preferences.debug("tagID = Mono reference low\n", Preferences.DEBUG_FILEIO);
                                break;
                            case 1030:
                                Preferences.debug("tagID = Mono reference high\n", Preferences.DEBUG_FILEIO);
                                break;
                            case 1031:
                                Preferences.debug("tagID = Red reference low\n", Preferences.DEBUG_FILEIO);
                                break;
                            case 1032:
                                Preferences.debug("tagID = Red reference high\n", Preferences.DEBUG_FILEIO);
                                break;
                            case 1033:
                                Preferences.debug("tagID = Green reference low\n", Preferences.DEBUG_FILEIO);
                                break;
                            case 1034:
                                Preferences.debug("tagID = Green reference high\n", Preferences.DEBUG_FILEIO);
                                break;
                            case 1035:
                                Preferences.debug("tagID = Blue reference low\n", Preferences.DEBUG_FILEIO);
                                break;
                            case 1036:
                                Preferences.debug("tagID = Blue reference high\n", Preferences.DEBUG_FILEIO);
                                break;
                            case 1041:
                                Preferences.debug("tagID = Framegrabber name\n", Preferences.DEBUG_FILEIO);
                                break;
                            case 1042:
                                Preferences.debug("tagID = Camera\n", Preferences.DEBUG_FILEIO);
                                break;
                            case 1044:
                                Preferences.debug("tagID = Camera trigger signal type\n", Preferences.DEBUG_FILEIO);
                                break;
                            case 1045:
                                Preferences.debug("tagID = Camera trigger enable\n", Preferences.DEBUG_FILEIO);
                                break;
                            case 1046:
                                Preferences.debug("tagID = Grabber timeout\n", Preferences.DEBUG_FILEIO);
                                break;
                            case 1281:
                                Preferences.debug("tagID = Multichannel enabled\n", Preferences.DEBUG_FILEIO);
                                break;
                            case 1282:
                                Preferences.debug("tagID = Multichannel color\n", Preferences.DEBUG_FILEIO);
                                if (valueDType == VT_I4) {
                                    multichannelColor = intValue;
                                }
                                break;
                            case 1283:
                                Preferences.debug("tagID = Multichannel weight\n", Preferences.DEBUG_FILEIO);
                                if (valueDType == VT_R8) {
                                    fileInfo.setMultichannelWeight(doubleValue);
                                }
                                break;
                            case 1284:
                                Preferences.debug("tagID = Channel name\n", Preferences.DEBUG_FILEIO);
                                break;
                            case 1536:
                                Preferences.debug("tagID = Document information group\n", Preferences.DEBUG_FILEIO);
                                break;
                            case 1537:
                                Preferences.debug("tagID = Title\n", Preferences.DEBUG_FILEIO);
                                break;
                            case 1538:
                                Preferences.debug("tagID = Author\n", Preferences.DEBUG_FILEIO);
                                break;
                            case 1539:
                                Preferences.debug("tagID = Keywords\n", Preferences.DEBUG_FILEIO);
                                break;
                            case 1540:
                                Preferences.debug("tagID = Comments\n", Preferences.DEBUG_FILEIO);
                                break;
                            case 1541:
                                Preferences.debug("tagID = Sample ID\n", Preferences.DEBUG_FILEIO);
                                break;
                            case 1542:
                                Preferences.debug("tagID = Subject\n", Preferences.DEBUG_FILEIO);
                                break;
                            case 1543:
                                Preferences.debug("tagID = Revision number\n", Preferences.DEBUG_FILEIO);
                                break;
                            case 1544:
                                Preferences.debug("tagID = Save folder\n", Preferences.DEBUG_FILEIO);
                                break;
                            case 1545:
                                Preferences.debug("tagID = File link\n", Preferences.DEBUG_FILEIO);
                                break;
                            case 1546:
                                Preferences.debug("tagID = Document type\n", Preferences.DEBUG_FILEIO);
                                break;
                            case 1547:
                                Preferences.debug("tagID = Storage media\n", Preferences.DEBUG_FILEIO);
                                break;
                            case 1548:
                                Preferences.debug("tagID = File ID\n", Preferences.DEBUG_FILEIO);
                                break;
                            case 1549:
                                Preferences.debug("tagID = Reference\n", Preferences.DEBUG_FILEIO);
                                break;
                            case 1550:
                                Preferences.debug("tagID = File date\n", Preferences.DEBUG_FILEIO);
                                if (valueDType == VT_DATE) {
                                    fileInfo.setFileDate(calculateVTDateTimeString(doubleValue));
                                }
                                break;
                            case 1551:
                                Preferences.debug("tagID = File size\n", Preferences.DEBUG_FILEIO);
                                if (valueDType == VT_I4) {
                                    fileInfo.setFileSize(intValue);
                                }
                                break;
                            case 1553:
                                Preferences.debug("tagID = Filename\n", Preferences.DEBUG_FILEIO);
                                break;
                            case 1554:
                                Preferences.debug("tagID = File attributes\n", Preferences.DEBUG_FILEIO);
                                break;
                            case 1792:
                                Preferences.debug("tagID = Project group\n", Preferences.DEBUG_FILEIO);
                                break;
                            case 1793:
                                Preferences.debug("tagID = Acquisition date\n", Preferences.DEBUG_FILEIO);
                                break;
                            case 1794:
                                Preferences.debug("tagID = Last modified by\n", Preferences.DEBUG_FILEIO);
                                break;
                            case 1795:
                                Preferences.debug("tagID = User company\n", Preferences.DEBUG_FILEIO);
                                break;
                            case 1796:
                                Preferences.debug("tagID = User company logo\n", Preferences.DEBUG_FILEIO);
                                break;
                            case 1797:
                                Preferences.debug("tagID = Image\n", Preferences.DEBUG_FILEIO);
                                break;
                            case 1800:
                                Preferences.debug("tagID = User ID\n", Preferences.DEBUG_FILEIO);
                                break;
                            case 1801:
                                Preferences.debug("tagID = User name\n", Preferences.DEBUG_FILEIO);
                                break;
                            case 1802:
                                Preferences.debug("tagID = User city\n", Preferences.DEBUG_FILEIO);
                                break;
                            case 1803:
                                Preferences.debug("tagID = User address\n", Preferences.DEBUG_FILEIO);
                                break;
                            case 1804:
                                Preferences.debug("tagID = User country\n", Preferences.DEBUG_FILEIO);
                                break;
                            case 1805:
                                Preferences.debug("tagID = User phone\n", Preferences.DEBUG_FILEIO);
                                break;
                            case 1806:
                                Preferences.debug("tagID = User fax\n", Preferences.DEBUG_FILEIO);
                                break;
                            case 2049:
                                Preferences.debug("tagID = Objective name\n", Preferences.DEBUG_FILEIO);
                                break;
                            case 2050:
                                Preferences.debug("tagID = Optovar\n", Preferences.DEBUG_FILEIO);
                                break;
                            case 2051:
                                Preferences.debug("tagID = Reflector\n", Preferences.DEBUG_FILEIO);
                                break;
                            case 2052:
                                Preferences.debug("tagID = Condenser contrast\n", Preferences.DEBUG_FILEIO);
                                break;
                            case 2053:
                                Preferences.debug("tagID = Transmitted light filter 1\n", Preferences.DEBUG_FILEIO);
                                break;
                            case 2054:
                                Preferences.debug("tagID = Transmitted light filter 2\n", Preferences.DEBUG_FILEIO);
                                break;
                            case 2055:
                                Preferences.debug("tagID = Reflected light shutter\n", Preferences.DEBUG_FILEIO);
                                if (valueDType == VT_I4) {
                                    fileInfo.setReflectedLightShutter(intValue);
                                }
                                break;
                            case 2056:
                                Preferences.debug("tagID = Condenser front lens\n", Preferences.DEBUG_FILEIO);
                                break;
                            case 2057:
                                Preferences.debug("tagID = Excitation filer name\n", Preferences.DEBUG_FILEIO);
                                break;
                            case 2060:
                                Preferences.debug("tagID = Transmitted light fieldstop aperture\n", Preferences.DEBUG_FILEIO);
                                break;
                            case 2061:
                                Preferences.debug("tagID = Reflected light aperture\n", Preferences.DEBUG_FILEIO);
                                break;
                            case 2062:
                                Preferences.debug("tagID = Condenser N.A.\n", Preferences.DEBUG_FILEIO);
                                break;
                            case 2063:
                                Preferences.debug("tagID = Light path\n", Preferences.DEBUG_FILEIO);
                                break;
                            case 2064:
                                Preferences.debug("tagID = Halogen lamp on\n", Preferences.DEBUG_FILEIO);
                                break;
                            case 2065:
                                Preferences.debug("tagID = Halogen lamp mode\n", Preferences.DEBUG_FILEIO);
                                break;
                            case 2066:
                                Preferences.debug("tagID = Halogen lamp voltage\n", Preferences.DEBUG_FILEIO);
                                break;
                            case 2068:
                                Preferences.debug("tagID = Fluorescence lamp level\n", Preferences.DEBUG_FILEIO);
                                break;
                            case 2069:
                                Preferences.debug("tagID = Fluorsecence lamp intensity\n", Preferences.DEBUG_FILEIO);
                                break;
                            case 2070:
                                Preferences.debug("tagID = Light manager is enabled\n", Preferences.DEBUG_FILEIO);
                                if (valueDType == VT_BOOL) {
                                    if (booleanValue) {
                                        fileInfo.setLightManagerEnabled("Light manager is enabled");
                                    }
                                    else {
                                        fileInfo.setLightManagerEnabled("Light manager is not enabled");
                                    }
                                }
                                break;
                            case 2072:
                                Preferences.debug("tagID = Focus position\n", Preferences.DEBUG_FILEIO);
                                if (valueDType == VT_R8) {
                                    // Different value for every z slice
                                    focusPosition = doubleValue;
                                }
                                break;
                            case 2073:
                                Preferences.debug("tagID = Stage position X\n", Preferences.DEBUG_FILEIO);
                                if (valueDType == VT_R8) {
                                    // Different value for every z slice
                                    stagePositionX = doubleValue;
                                }
                                break;
                            case 2074:
                                Preferences.debug("tagID = Stage position Y\n", Preferences.DEBUG_FILEIO);
                                if (valueDType == VT_R8) {
                                    // Different value for every z slice
                                    stagePositionY = doubleValue;
                                }
                                break;
                            case 2075:
                                Preferences.debug("tagID = Microscope name\n", Preferences.DEBUG_FILEIO);
                                break;
                            case 2076:
                                Preferences.debug("tagID = Objective magnification\n", Preferences.DEBUG_FILEIO);
                                if (valueDType == VT_R8) {
                                    fileInfo.setObjectiveMagnification(doubleValue);
                                }
                                break;
                            case 2077:
                                Preferences.debug("tagID = Objective N.A.\n", Preferences.DEBUG_FILEIO);
                                if (valueDType == VT_R8) {
                                    fileInfo.setObjectiveNA(doubleValue);
                                }
                                break;
                            case 2078:
                                Preferences.debug("tagID = Microscope illumination\n", Preferences.DEBUG_FILEIO);
                                if (valueDType == VT_I4) {
                                    fileInfo.setMicroscopeIllumination(intValue);
                                }
                                break;
                            case 2079:
                                Preferences.debug("tagID = External shutter 1\n", Preferences.DEBUG_FILEIO);
                                break;
                            case 2080:
                                Preferences.debug("tagID = External shutter 2\n", Preferences.DEBUG_FILEIO);
                                break;
                            case 2081:
                                Preferences.debug("tagID = External shutter 3\n", Preferences.DEBUG_FILEIO);
                                break;
                            case 2082:
                                Preferences.debug("tagID = External filter wheel 1 name\n", Preferences.DEBUG_FILEIO);
                                break;
                            case 2083:
                                Preferences.debug("tagID = External filter wheel 2 name\n", Preferences.DEBUG_FILEIO);
                                break;
                            case 2084:
                                Preferences.debug("tagID = Parfocal correction\n", Preferences.DEBUG_FILEIO);
                                if (valueDType == VT_BOOL) {
                                    if (booleanValue) {
                                        fileInfo.setParfocalCorrection("Parfocal correction is present");    
                                    }
                                    else {
                                        fileInfo.setParfocalCorrection("Parfocal correction is not present");
                                    }
                                }
                                break;
                            case 2086:
                                Preferences.debug("tagID = External shutter 4\n", Preferences.DEBUG_FILEIO);
                                break;
                            case 2087:
                                Preferences.debug("tagID = External shutter 5\n", Preferences.DEBUG_FILEIO);
                                break;
                            case 2088:
                                Preferences.debug("tagID = External shutter 6\n", Preferences.DEBUG_FILEIO);
                                break;
                            case 2089:
                                Preferences.debug("tagID = External filter wheel 3 name\n", Preferences.DEBUG_FILEIO);
                                break;
                            case 2090:
                                Preferences.debug("tagID = External filter wheel 4 name\n", Preferences.DEBUG_FILEIO);
                                break;
                            case 2103:
                                Preferences.debug("tagID = Objective turret position\n", Preferences.DEBUG_FILEIO);
                                if (valueDType == VT_I4) {
                                    fileInfo.setObjectiveTurretPosition(intValue);
                                }
                                break;
                            case 2104:
                                Preferences.debug("tagID = Objective contrast method\n", Preferences.DEBUG_FILEIO);
                                if (valueDType == VT_I4) {
                                    fileInfo.setObjectiveContrastMethod(intValue);
                                }
                                break;
                            case 2105:
                                Preferences.debug("tagID = Objective immersion type\n", Preferences.DEBUG_FILEIO);
                                if (valueDType == VT_I4) {
                                    switch (intValue) {
                                        case 1:
                                            fileInfo.setObjectiveImmersionType("No immersion");
                                            break;
                                        case 2:
                                            fileInfo.setObjectiveImmersionType("Oil");
                                            break;
                                        case 3:
                                            fileInfo.setObjectiveImmersionType("Water");
                                            break;
                                        default:
                                            fileInfo.setObjectiveImmersionType("Unknown");
                                    }
                                }
                                break;
                            case 2107:
                                Preferences.debug("tagID = Reflector position\n", Preferences.DEBUG_FILEIO);
                                // Different value for every channel
                                if (valueDType == VT_I4) {
                                    reflectorPosition = intValue;
                                }
                                break;
                            case 2109:
                                Preferences.debug("tagID = Transmitted light filter 1 position\n", Preferences.DEBUG_FILEIO);
                                break;
                            case 2110:
                                Preferences.debug("tagID = Transmitted light filter 2 position\n", Preferences.DEBUG_FILEIO);
                                break;
                            case 2112:
                                Preferences.debug("tagID = Excitation filter position\n", Preferences.DEBUG_FILEIO);
                                break;
                            case 2113:
                                Preferences.debug("tagID = Lamp mirror position (ERSETZT DURCH 241!)\n", 
                                		Preferences.DEBUG_FILEIO);
                                break;
                            case 2114:
                                Preferences.debug("tagID = External filter wheel 1 position\n", Preferences.DEBUG_FILEIO);
                                break;
                            case 2115:
                                Preferences.debug("tagID = External filter wheel 2 position\n", Preferences.DEBUG_FILEIO);
                                break;
                            case 2116:
                                Preferences.debug("tagID = External filter wheel 3 position\n", Preferences.DEBUG_FILEIO);
                                break;
                            case 2117:
                                Preferences.debug("tagID = External filter wheel 4 position\n", Preferences.DEBUG_FILEIO);
                                break;
                            case 2118:
                                Preferences.debug("tagID = Light manager mode\n", Preferences.DEBUG_FILEIO);
                                if (valueDType == VT_I4) {
                                    fileInfo.setLightManagerMode(intValue);
                                }
                                break;
                            case 2119:
                                Preferences.debug("tagID = Halogen lamp calibration\n", Preferences.DEBUG_FILEIO);
                                break;
                            case 2120:
                                Preferences.debug("tagID = Condenser NA go speed\n", Preferences.DEBUG_FILEIO);
                                break;
                            case 2121:
                                Preferences.debug("tagID = Transmitted light field stop go speed\n", Preferences.DEBUG_FILEIO);
                                break;
                            case 2122:
                                Preferences.debug("tagID = Optovar  go speed\n", Preferences.DEBUG_FILEIO);
                                break;
                            case 2123:
                                Preferences.debug("tagID = Focus calibrated\n", Preferences.DEBUG_FILEIO);
                                if (valueDType == VT_BOOL) {
                                    if (booleanValue) {
                                        fileInfo.setFocusCalibrated("Focus is calibrated");
                                    }
                                    else {
                                        fileInfo.setFocusCalibrated("Focus is not calibrated");
                                    }
                                }
                                break;
                            case 2124:
                                Preferences.debug("tagID = Focus basic position\n", Preferences.DEBUG_FILEIO);
                                break;
                            case 2125:
                                Preferences.debug("tagID = Focus power\n", Preferences.DEBUG_FILEIO);
                                break;
                            case 2126:
                                Preferences.debug("tagID = Focus backlash\n", Preferences.DEBUG_FILEIO);
                                break;
                            case 2127:
                                Preferences.debug("tagID = Focus measurement origin\n", Preferences.DEBUG_FILEIO);
                                break;
                            case 2128:
                                Preferences.debug("tagID = Focus measurement distance\n", Preferences.DEBUG_FILEIO);
                                break;
                            case 2129:
                                Preferences.debug("tagID = Focus speed\n", Preferences.DEBUG_FILEIO);
                                break;
                            case 2130:
                                Preferences.debug("tagID = Focus go speed\n", Preferences.DEBUG_FILEIO);
                                break;
                            case 2131:
                                Preferences.debug("tagID = Focus distance\n", Preferences.DEBUG_FILEIO);
                                break;
                            case 2132:
                                Preferences.debug("tagID = Focus init position\n", Preferences.DEBUG_FILEIO);
                                break;
                            case 2133:
                                Preferences.debug("tagID = Stage calibrated\n", Preferences.DEBUG_FILEIO);
                                break;
                            case 2134:
                                Preferences.debug("tagID = Stage power\n", Preferences.DEBUG_FILEIO);
                                break;
                            case 2135:
                                Preferences.debug("tagID = Stage X backlash\n", Preferences.DEBUG_FILEIO);
                                break;
                            case 2136:
                                Preferences.debug("tagID = Stage Y backlash\n", Preferences.DEBUG_FILEIO);
                                break;
                            case 2137:
                                Preferences.debug("tagID = Stage speed X\n", Preferences.DEBUG_FILEIO);
                                break;
                            case 2138:
                                Preferences.debug("tagID = Stage speed Y\n", Preferences.DEBUG_FILEIO);
                                break;
                            case 2139:
                                Preferences.debug("tagID = Stage speed\n", Preferences.DEBUG_FILEIO);
                                break;
                            case 2140:
                                Preferences.debug("tagID = Stage go speed X\n", Preferences.DEBUG_FILEIO);
                                break;
                            case 2141:
                                Preferences.debug("tagID = Stage go speed Y\n", Preferences.DEBUG_FILEIO);
                                break;
                            case 2142:
                                Preferences.debug("tagID = Stage step distance X\n", Preferences.DEBUG_FILEIO);
                                break;
                            case 2143:
                                Preferences.debug("tagID = Stage step distance Y\n", Preferences.DEBUG_FILEIO);
                                break;
                            case 2144:
                                Preferences.debug("tagID = Stage initialization position X\n", Preferences.DEBUG_FILEIO);
                                break;
                            case 2145:
                                Preferences.debug("tagID = Stage initialization position Y\n", Preferences.DEBUG_FILEIO);
                                break;
                            case 2146:
                                Preferences.debug("tagID = Microscope magnification\n", Preferences.DEBUG_FILEIO);
                                if (valueDType == VT_R8) {
                                    fileInfo.setMicroscopeMagnification(doubleValue);
                                }
                                break;
                            case 2147:
                                Preferences.debug("tagID = Reflector magnification\n", Preferences.DEBUG_FILEIO);
                                if (valueDType == VT_R8) {
                                    fileInfo.setReflectorMagnification(doubleValue);
                                }
                                break;
                            case 2148:
                                Preferences.debug("tagID = Lamp mirror position\n", Preferences.DEBUG_FILEIO);
                                break;
                            case 2149:
                                Preferences.debug("tagID = Focus depth\n", Preferences.DEBUG_FILEIO);
                                if (valueDType == VT_R8) {
                                    fileInfo.setFocusDepth(doubleValue);
                                }
                                break;
                            case 2150:
                                Preferences.debug("tagID = Microscope type\n", Preferences.DEBUG_FILEIO);
                                if (valueDType == VT_I4) {
                                    fileInfo.setMicroscopeType(intValue);
                                }
                                break;
                            case 2151:
                                Preferences.debug("tagID = Objective working distance\n", Preferences.DEBUG_FILEIO);
                                if (valueDType == VT_R8) {
                                    fileInfo.setObjectiveWorkingDistance(doubleValue);
                                }
                                break;
                            case 2152:
                                Preferences.debug("tagID = Reflected light aperture go speed\n", Preferences.DEBUG_FILEIO);
                                break;
                            case 2153:
                                Preferences.debug("tagID = External shutter\n", Preferences.DEBUG_FILEIO);
                                break;
                            case 2154:
                                Preferences.debug("tagID = Objective immersion stop\n", Preferences.DEBUG_FILEIO);
                                break;
                            case 2155:
                                Preferences.debug("tagID = Focus start speed\n", Preferences.DEBUG_FILEIO);
                                break;
                            case 2156:
                                Preferences.debug("tagID = Focus acceleration\n", Preferences.DEBUG_FILEIO);
                                break;
                            case 2157:
                                Preferences.debug("tagID = Reflected light fieldstop\n", Preferences.DEBUG_FILEIO);
                                break;
                            case 2158:
                                Preferences.debug("tagID = Reflected light fieldstop go speed\n", Preferences.DEBUG_FILEIO);
                                break;
                            case 2159:
                                Preferences.debug("tagID = Reflected light filter 1\n", Preferences.DEBUG_FILEIO);
                                break;
                            case 2160:
                                Preferences.debug("tagID = Reflector light filter 2\n", Preferences.DEBUG_FILEIO);
                                break;
                            case 2161:
                                Preferences.debug("tagID = Reflected light filter 1 position\n", Preferences.DEBUG_FILEIO);
                                break;
                            case 2162:
                                Preferences.debug("tagID = Reflected light filter 2 position\n", Preferences.DEBUG_FILEIO);
                                break;
                            case 2163:
                                Preferences.debug("tagID = Transmitted light attenuator\n", Preferences.DEBUG_FILEIO);
                                break;
                            case 2164:
                                Preferences.debug("tagID = Reflected light attenuator\n", Preferences.DEBUG_FILEIO);
                                break;
                            case 2165:
                                Preferences.debug("tagID = Transmitted light shutter\n", Preferences.DEBUG_FILEIO);
                                if (valueDType == VT_I4) {
                                    fileInfo.setTransmittedLightShutter(intValue);
                                }
                                break;
                            case 2166:
                                Preferences.debug("tagID = Transmitted light attenuator go speed\n", Preferences.DEBUG_FILEIO);
                                break;
                            case 2167:
                                Preferences.debug("tagID = Reflected light attenuator go speed\n", Preferences.DEBUG_FILEIO);
                                break;
                            case 2176:
                                Preferences.debug("tagID = Transmitted light virtual filter position\n", 
                                		Preferences.DEBUG_FILEIO);
                                break;
                            case 2177:
                                Preferences.debug("tagID = Transmitted light virtual filter\n", Preferences.DEBUG_FILEIO);
                                break;
                            case 2178:
                                Preferences.debug("tagID = Reflected light virtual filter position\n", Preferences.DEBUG_FILEIO);
                                break;
                            case 2179:
                                Preferences.debug("tagID = Reflected light virtual filter\n", Preferences.DEBUG_FILEIO);
                                break;
                            case 2180:
                                Preferences.debug("tagID = Reflected light halogen lamp mode\n", Preferences.DEBUG_FILEIO);
                                if (valueDType == VT_I4) {
                                    fileInfo.setReflectedLightHalogenLampMode(intValue);
                                }
                                break;
                            case 2181:
                                Preferences.debug("tagID = Reflected light halogen lamp voltage\n", Preferences.DEBUG_FILEIO);
                                if (valueDType == VT_R8) {
                                    fileInfo.setReflectedLightHalogenLampVoltage(doubleValue);
                                }
                                break;
                            case 2182:
                                Preferences.debug("tagID = Reflected light halogen lamp color temperature\n", 
                                		Preferences.DEBUG_FILEIO);
                                break;
                            case 2183:
                                Preferences.debug("tagID = Contrast manager mode\n", Preferences.DEBUG_FILEIO);
                                if (valueDType == VT_I4) {
                                    fileInfo.setContrastManagerMode(intValue);
                                }
                                break;
                            case 2184:
                                Preferences.debug("tagID = Dazzle protection acitve\n", Preferences.DEBUG_FILEIO);
                                if (valueDType == VT_BOOL) {
                                    if (booleanValue) {
                                        fileInfo.setDazzleProtection("Dazzle protection is active");
                                    }
                                    else {
                                        fileInfo.setDazzleProtection("Dazzle protection is not active");
                                    }
                                }
                                break;
                            case 2195:
                                Preferences.debug("tagID = Zoom\n", Preferences.DEBUG_FILEIO);
                                break;
                            case 2196:
                                Preferences.debug("tagID = Zoom go speed\n", Preferences.DEBUG_FILEIO);
                                break;
                            case 2197:
                                Preferences.debug("tagID = Light zoom\n", Preferences.DEBUG_FILEIO);
                                break;
                            case 2198:
                                Preferences.debug("tagID = Light zoom go speed\n", Preferences.DEBUG_FILEIO);
                                break;
                            case 2199:
                                Preferences.debug("tagID = Light zoom coupled\n", Preferences.DEBUG_FILEIO);
                                break;
                            case 2200:
                                Preferences.debug("tagID = Transmitted light halogen lamp mode\n", Preferences.DEBUG_FILEIO);
                                if (valueDType == VT_I4) {
                                    fileInfo.setTransmittedLightHalogenLampMode(intValue);
                                }
                                break;
                            case 2201:
                                Preferences.debug("tagID = Transmitted light halogen lamp voltage\n", Preferences.DEBUG_FILEIO);
                                if (valueDType == VT_R8) {
                                    fileInfo.setTransmittedLightHalogenLampVoltage(doubleValue);
                                }
                                break;
                            case 2202:
                                Preferences.debug("tagID = Transmitted light halogen lamp color temperature\n", 
                                		Preferences.DEBUG_FILEIO);
                                break;
                            case 2203:
                                Preferences.debug("tagID = Reflected cold light mode\n", Preferences.DEBUG_FILEIO);
                                break;
                            case 2204:
                                Preferences.debug("tagID = Reflected cold light intensity\n", Preferences.DEBUG_FILEIO);
                                break;
                            case 2205:
                                Preferences.debug("tagID = Reflected cold light color temperature\n", Preferences.DEBUG_FILEIO);
                                break;
                            case 2206:
                                Preferences.debug("tagID = Transmitted cold light mode\n", Preferences.DEBUG_FILEIO);
                                break;
                            case 2207:
                                Preferences.debug("tagID = Transmitted cold light intensity\n", Preferences.DEBUG_FILEIO);
                                break;
                            case 2208:
                                Preferences.debug("tagID = Transmitted cold light color temperature\n",
                                		Preferences.DEBUG_FILEIO);
                                break;
                            case 2209:
                                Preferences.debug("tagID = Infinity space port changer position\n", Preferences.DEBUG_FILEIO);
                                break;
                            case 2210:
                                Preferences.debug("tagID = Beam splitter infinity space\n", Preferences.DEBUG_FILEIO);
                                break;
                            case 2211:
                                Preferences.debug("tagID = Two TV vis cam changer position\n", Preferences.DEBUG_FILEIO);
                                break;
                            case 2212:
                                Preferences.debug("tagID = Beam splitter ocular\n", Preferences.DEBUG_FILEIO);
                                break;
                            case 2213:
                                Preferences.debug("tagID = Two TV cameras changer position\n", Preferences.DEBUG_FILEIO);
                                break;
                            case 2214:
                                Preferences.debug("tagID = Beam splitter cameras\n", Preferences.DEBUG_FILEIO);
                                break;
                            case 2215:
                                Preferences.debug("tagID = Ocular shutter\n", Preferences.DEBUG_FILEIO);
                                break;
                            case 2216:
                                Preferences.debug("tagID = Two TV cameras changer cube\n", Preferences.DEBUG_FILEIO);
                                break;
                            case 2217:
                                Preferences.debug("tagID = Light wavelength\n", Preferences.DEBUG_FILEIO);
                                break;
                            case 2218:
                                Preferences.debug("tagID = Ocular magnification\n", Preferences.DEBUG_FILEIO);
                                break;
                            case 2219:
                                Preferences.debug("tagID = Camera adapter magnification\n", Preferences.DEBUG_FILEIO);
                                if (valueDType == VT_R8) {
                                    fileInfo.setCameraAdapterMagnification(doubleValue);
                                }
                                break;
                            case 2220:
                                Preferences.debug("tagID = Microscope port\n", Preferences.DEBUG_FILEIO);
                                if (valueDType == VT_I4) {
                                    fileInfo.setMicroscopePort(intValue);
                                }
                                break;
                            case 2221:
                                Preferences.debug("tagID = Ocular total magnification\n", Preferences.DEBUG_FILEIO);
                                if (valueDType == VT_R8) {
                                    fileInfo.setOcularTotalMagnification(doubleValue);
                                }
                                break;
                            case 2222:
                                Preferences.debug("tagID = Field of view\n", Preferences.DEBUG_FILEIO);
                                break;
                            case 2223:
                                Preferences.debug("tagID = Ocular\n", Preferences.DEBUG_FILEIO);
                                break;
                            case 2224:
                                Preferences.debug("tagID = Camera adapter\n", Preferences.DEBUG_FILEIO);
                                break;
                            case 2225:
                                Preferences.debug("tagID = Stage joystick enabled\n", Preferences.DEBUG_FILEIO);
                                break;
                            case 2226:
                                Preferences.debug("tagID = Contrast manager contrast method\n", Preferences.DEBUG_FILEIO);
                                break;
                            case 2229:
                                Preferences.debug("tagID = Cameras changer beam splitter type\n", Preferences.DEBUG_FILEIO);
                                break;
                            case 2235:
                                Preferences.debug("tagID = Rear port slider position\n", Preferences.DEBUG_FILEIO);
                                break;
                            case 2236:
                                Preferences.debug("tagID = Rear port source\n", Preferences.DEBUG_FILEIO);
                                break;
                            case 2237:
                                Preferences.debug("tagID = Beam splitter type infinity space\n", Preferences.DEBUG_FILEIO);
                                break;
                            case 2238:
                                Preferences.debug("tagID = Fluorescence attenuator\n", Preferences.DEBUG_FILEIO);
                                break;
                            case 2239:
                                Preferences.debug("tagID = Fluorescence attenuator position\n", Preferences.DEBUG_FILEIO);
                                break;
                            case 2261:
                                Preferences.debug("tagID = Objective ID\n", Preferences.DEBUG_FILEIO);
                                break;
                            case 2262:
                                Preferences.debug("tagID = Reflector ID\n", Preferences.DEBUG_FILEIO);
                                break;
                            case 2307:
                                Preferences.debug("tagID = Camera frame start left\n", Preferences.DEBUG_FILEIO);
                                if (valueDType == VT_I4) {
                                    fileInfo.setCameraFrameStartLeft(intValue);
                                }
                                break;
                            case 2308:
                                Preferences.debug("tagID = Camera frame start top\n", Preferences.DEBUG_FILEIO);
                                if (valueDType == VT_I4) {
                                    fileInfo.setCameraFrameStartTop(intValue);
                                }
                                break;
                            case 2309:
                                Preferences.debug("tagID = Camera frame width\n", Preferences.DEBUG_FILEIO);
                                if (valueDType == VT_I4) {
                                    fileInfo.setCameraFrameWidth(intValue);
                                }
                                break;
                            case 2310:
                                Preferences.debug("tagID = Camera frame height\n", Preferences.DEBUG_FILEIO);
                                if (valueDType == VT_I4) {
                                    fileInfo.setCameraFrameHeight(intValue);
                                }
                                break;
                            case 2311:
                                Preferences.debug("tagID = Camera binning\n", Preferences.DEBUG_FILEIO);
                                if (valueDType == VT_I4) {
                                    fileInfo.setCameraBinning(intValue);
                                }
                                break;
                            case 2312:
                                Preferences.debug("tagID = Camera frame full\n", Preferences.DEBUG_FILEIO);
                                break;
                            case 2313:
                                Preferences.debug("tagID = Camera frame pixel distance\n", Preferences.DEBUG_FILEIO);
                                if (valueDType == VT_R8) {
                                    fileInfo.setCameraFramePixelDistance(doubleValue);
                                }
                                break;
                            case 2318:
                                Preferences.debug("tagID = Data format use scaling\n", Preferences.DEBUG_FILEIO);
                                break;
                            case 2319:
                                Preferences.debug("tagID = Camera frame image orientation\n", Preferences.DEBUG_FILEIO);
                                if (valueDType == VT_I4) {
                                    fileInfo.setCameraFrameImageOrientation(intValue);
                                }
                                break;
                            case 2320:
                                Preferences.debug("tagID = Video monochrome signal type\n", Preferences.DEBUG_FILEIO);
                                break;
                            case 2321:
                                Preferences.debug("tagID = Video color signal type\n", Preferences.DEBUG_FILEIO);
                                break;
                            case 2322:
                                Preferences.debug("tagID = Meteor channel input\n", Preferences.DEBUG_FILEIO);
                                break;
                            case 2323:
                                Preferences.debug("tagID = Meteor channel sync\n", Preferences.DEBUG_FILEIO);
                                break;
                            case 2324:
                                Preferences.debug("tagID = White balance enabled\n", Preferences.DEBUG_FILEIO);
                                break;
                            case 2325:
                                Preferences.debug("tagID = Camera white balance red\n", Preferences.DEBUG_FILEIO);
                                break;
                            case 2326:
                                Preferences.debug("tagID = Camera white balance green\n", Preferences.DEBUG_FILEIO);
                                break;
                            case 2327:
                                Preferences.debug("tagID = Camera white balance blue\n", Preferences.DEBUG_FILEIO);
                                break;
                            case 2331:
                                Preferences.debug("tagID = Camera frame scaling factor\n", Preferences.DEBUG_FILEIO);
                                if (valueDType == VT_R8) {
                                   fileInfo.setCameraFrameScalingFactor(doubleValue);
                                }
                                break;
                            case 2562:
                                Preferences.debug("tagID = Meteor camera type\n", Preferences.DEBUG_FILEIO);
                                break;
                            case 2564:
                                Preferences.debug("tagID = Exposure time in msec\n", Preferences.DEBUG_FILEIO);
                                // A different exposure time seen for each channel
                                if (valueDType == VT_R8) {
                                    exposureTime = doubleValue;
                                }
                                break;
                            case 2568:
                                Preferences.debug("tagID = Camera exposure time auto calculate\n", Preferences.DEBUG_FILEIO);
                                break;
                            case 2569:
                                Preferences.debug("tagID = Meteor gain value\n", Preferences.DEBUG_FILEIO);
                                break;
                            case 2571:
                                Preferences.debug("tagID = Meteor gain automatic\n", Preferences.DEBUG_FILEIO);
                                break;
                            case 2572:
                                Preferences.debug("tagID = Meteor adjust hue\n", Preferences.DEBUG_FILEIO);
                                break;
                            case 2573:
                                Preferences.debug("tagID = Meteor adjust saturation\n", Preferences.DEBUG_FILEIO);
                                break;
                            case 2574:
                                Preferences.debug("tagID = Meteor adjust red low\n", Preferences.DEBUG_FILEIO);
                                break;
                            case 2575:
                                Preferences.debug("tagID = Meteor adjust green low\n", Preferences.DEBUG_FILEIO);
                                break;
                            case 2576:
                                Preferences.debug("tagID = Meteor adjust blue low\n", Preferences.DEBUG_FILEIO);
                                break;
                            case 2577:
                                Preferences.debug("tagID = Meteor adjust red high\n", Preferences.DEBUG_FILEIO);
                                break;
                            case 2578:
                                Preferences.debug("tagID = Meteor adjust green high\n", Preferences.DEBUG_FILEIO);
                                break;
                            case 2579:
                                Preferences.debug("tagID = Meteor adjust blue high\n", Preferences.DEBUG_FILEIO);
                                break;
                            case 2582:
                                Preferences.debug("tagID = Camera exposure time calculation control\n", 
                                		Preferences.DEBUG_FILEIO);
                                break;
                            case 2585:
                                Preferences.debug("tagID = Axio cam fading correction enable\n", Preferences.DEBUG_FILEIO);
                                break;
                            case 2587:
                                Preferences.debug("tagID = Camera live image\n", Preferences.DEBUG_FILEIO);
                                break;
                            case 2588:
                                Preferences.debug("tagID = Camera live enabled\n", Preferences.DEBUG_FILEIO);
                                break;
                            case 2589:
                                Preferences.debug("tagID = Live image sync object name\n", Preferences.DEBUG_FILEIO);
                                break;
                            case 2590:
                                Preferences.debug("tagID = Camera live speed\n", Preferences.DEBUG_FILEIO);
                                break;
                            case 2591:
                                Preferences.debug("tagID = Camera image\n", Preferences.DEBUG_FILEIO);
                                break;
                            case 2592:
                                Preferences.debug("tagID = Camera image width\n", Preferences.DEBUG_FILEIO);
                                break;
                            case 2593:
                                Preferences.debug("tagID = Camera image height\n", Preferences.DEBUG_FILEIO);
                                break;
                            case 2594:
                                Preferences.debug("tagID = Camera image pixel type\n", Preferences.DEBUG_FILEIO);
                                break;
                            case 2595:
                                Preferences.debug("tagID = Camera image sh memory name\n", Preferences.DEBUG_FILEIO);
                                break;
                            case 2596:
                                Preferences.debug("tagID = Camera live image width\n", Preferences.DEBUG_FILEIO);
                                break;
                            case 2597:
                                Preferences.debug("tagID = Camera live image height\n", Preferences.DEBUG_FILEIO);
                                break;
                            case 2598:
                                Preferences.debug("tagID = Camera live image pixel type\n", Preferences.DEBUG_FILEIO);
                                break;
                            case 2599:
                                Preferences.debug("tagID = Camera live image sh memory name\n", Preferences.DEBUG_FILEIO);
                                break;
                            case 2600:
                                Preferences.debug("tagID = Camera live maximum speed\n", Preferences.DEBUG_FILEIO);
                                break;
                            case 2601:
                                Preferences.debug("tagID = Camera live binning\n", Preferences.DEBUG_FILEIO);
                                break;
                            case 2602:
                                Preferences.debug("tagID = Camera live gain value\n", Preferences.DEBUG_FILEIO);
                                break;
                            case 2603:
                                Preferences.debug("tagID = Camera live exposure time value\n", Preferences.DEBUG_FILEIO);
                                break;
                            case 2604:
                                Preferences.debug("tagID = Camera live scaling factor\n", Preferences.DEBUG_FILEIO);
                                if (valueDType == VT_R8) {
                                    fileInfo.setCameraLiveScalingFactor(doubleValue);
                                }
                                break;
                            case 2817:
                                Preferences.debug("tagID = Image index U\n", Preferences.DEBUG_FILEIO);
                                break;
                            case 2818:
                                Preferences.debug("tagID = Image index V\n", Preferences.DEBUG_FILEIO);
                                break;
                            case 2819:
                                Preferences.debug("tagID = Image index Z\n", Preferences.DEBUG_FILEIO);
                                if (valueDType == VT_I4) {
                                    zValue = intValue;
                                    if (haveFirstZValue && (!haveSecondZValue) && (zValue != firstZValue)) {
                                        haveSecondZValue = true;
                                        secondZValue = zValue;
                                    }
                                    if (!haveFirstZValue) {
                                        haveFirstZValue = true;
                                        firstZValue = zValue;
                                    }
                                }
                                break;
                            case 2820:
                                Preferences.debug("tagID = Image index C\n", Preferences.DEBUG_FILEIO);
                                if (valueDType == VT_I4) {
                                    cValue = intValue;
                                }
                                break;
                            case 2821:
                                Preferences.debug("tagID = Image index T\n", Preferences.DEBUG_FILEIO);
                                if (valueDType == VT_I4) {
                                    tValue = intValue;
                                }
                                break;
                            case 2822:
                                Preferences.debug("tagID = Image tile index\n", Preferences.DEBUG_FILEIO);
                                if (valueDType == VT_I4) {
                                    tileValue = intValue;                       
                                    if (haveFirstTileValue && (!haveSecondTileValue) && (tileValue != firstTileValue)) {
                                        haveSecondTileValue = true;
                                        secondTileValue = tileValue;
                                    }
                                    if (!haveFirstTileValue) {
                                        haveFirstTileValue = true;
                                        firstTileValue = tileValue;
                                    }
                                }
                                break;
                            case 2823:
                                Preferences.debug("tagID = Image acquisition index\n", Preferences.DEBUG_FILEIO);
                                break;
                            case 2824:
                                Preferences.debug("tagID = Image count tiles\n", Preferences.DEBUG_FILEIO);
                                break;
                            case 2825:
                                Preferences.debug("tagID = Image count A\n", Preferences.DEBUG_FILEIO);
                                break;
                            case 2827:
                                Preferences.debug("tagID = Image index S\n", Preferences.DEBUG_FILEIO);
                                break;
                            case 2828:
                                Preferences.debug("tagID = Image index raw\n", Preferences.DEBUG_FILEIO);
                                break;
                            case 2832:
                                Preferences.debug("tagID = Image count Z\n", Preferences.DEBUG_FILEIO);
                                break;
                            case 2833:
                                Preferences.debug("tagID = Image count C\n", Preferences.DEBUG_FILEIO);
                                break;
                            case 2834:
                                Preferences.debug("tagID = Image count T\n", Preferences.DEBUG_FILEIO);
                                break;
                            case 2838:
                                Preferences.debug("tagID = Image count U\n", Preferences.DEBUG_FILEIO);
                                break;
                            case 2839:
                                Preferences.debug("tagID = Image count V\n", Preferences.DEBUG_FILEIO);
                                break;
                            case 2840:
                                Preferences.debug("tagID = Image count S\n", Preferences.DEBUG_FILEIO);
                                break;
                            case 2841:
                                Preferences.debug("tagiD = Original stage position X\n", Preferences.DEBUG_FILEIO);
                                break;
                            case 2842:
                                Preferences.debug("tagID = Original stage position Y\n", Preferences.DEBUG_FILEIO);
                                break;
                            case 3088:
                                Preferences.debug("tagID = Layer draw flags\n", Preferences.DEBUG_FILEIO);
                                break;
                            case 3334:
                                Preferences.debug("tagID = Remaining time\n", Preferences.DEBUG_FILEIO);
                                break;
                            case 3585:
                                Preferences.debug("tagID = User field 1\n", Preferences.DEBUG_FILEIO);
                                break;
                            case 3586:
                                Preferences.debug("tagID = User field 2\n", Preferences.DEBUG_FILEIO);
                                break;
                            case 3587:
                                Preferences.debug("tagID = User field 3\n", Preferences.DEBUG_FILEIO);
                                break;
                            case 3588:
                                Preferences.debug("tagID = User field 4\n", Preferences.DEBUG_FILEIO);
                                break;
                            case 3589:
                                Preferences.debug("tagID = User field 5\n", Preferences.DEBUG_FILEIO);
                                break;
                            case 3590:
                                Preferences.debug("tagID = User field 6\n", Preferences.DEBUG_FILEIO);
                                break;
                            case 3591:
                                Preferences.debug("tagID = User field 7\n", Preferences.DEBUG_FILEIO);
                                break;
                            case 3592:
                                Preferences.debug("tagID = User field 8\n", Preferences.DEBUG_FILEIO);
                                break;
                            case 3593:
                                Preferences.debug("tagID = User field 9\n", Preferences.DEBUG_FILEIO);
                                break;
                            case 3594:
                                Preferences.debug("tagID = User field 10\n", Preferences.DEBUG_FILEIO);
                                break;
                            case 3840:
                                Preferences.debug("tagID = ID\n", Preferences.DEBUG_FILEIO);
                                break;
                            case 3841:
                                Preferences.debug("tagID = Name\n", Preferences.DEBUG_FILEIO);
                                break;
                            case 3842:
                                Preferences.debug("tagiD = Value\n", Preferences.DEBUG_FILEIO);
                                break;
                            case 5501:
                                Preferences.debug("tagID = Pv cam clocking mode\n", Preferences.DEBUG_FILEIO);
                                break;
                            case 8193:
                                Preferences.debug("tagID = Autofocus status report\n", Preferences.DEBUG_FILEIO);
                                break;
                            case 8194:
                                Preferences.debug("tagID = Autofocus position\n", Preferences.DEBUG_FILEIO);
                                break;
                            case 8195:
                                Preferences.debug("tagID = Autofocus position offset\n", Preferences.DEBUG_FILEIO);
                                break;
                            case 8196:
                                Preferences.debug("tagID = Autofocus empty field threshold\n", Preferences.DEBUG_FILEIO);
                                break;
                            case 8197:
                                Preferences.debug("tagID = Autofocus calibration name\n", Preferences.DEBUG_FILEIO);
                                break;
                            case 8198:
                                Preferences.debug("tagID = Autofocus current calibration item\n", Preferences.DEBUG_FILEIO);
                                break;
                            case 65537:
                                Preferences.debug("tagID = Camera frame full width\n", Preferences.DEBUG_FILEIO);
                                break;
                            case 65538:
                                Preferences.debug("tagID = Camera frame full height\n", Preferences.DEBUG_FILEIO);
                                break;
                            case 65541:
                                Preferences.debug("tagID = Axio cam shutter signal\n", Preferences.DEBUG_FILEIO);
                                if (valueDType == VT_I4) {
                                    fileInfo.setAxioCamShutterSignal(intValue);
                                }
                                break;
                            case 65542:
                                Preferences.debug("tagID = Axio cam delay time\n", Preferences.DEBUG_FILEIO);
                                if (valueDType == VT_I4) {
                                    fileInfo.setAxioCamDelayTime(intValue);
                                }
                                break;
                            case 65543:
                                Preferences.debug("tagid = Axio cam shutter control\n", Preferences.DEBUG_FILEIO);
                                if (valueDType == VT_I4) {
                                    fileInfo.setAxioCamShutterControl(intValue);
                                }
                                break;
                            case 65544:
                                Preferences.debug("tagID = Axio cam black refls calculated\n", Preferences.DEBUG_FILEIO);
                                break;
                            case 65545:
                                Preferences.debug("tagID = Axio cam black reference\n", Preferences.DEBUG_FILEIO);
                                if (valueDType == VT_I4) {
                                    fileInfo.setAxioCamBlackReference(intValue);
                                }
                                break;
                            case 65547:
                                Preferences.debug("tagID = Camera shading correction\n", Preferences.DEBUG_FILEIO);
                                if (valueDType == VT_I4) {
                                    fileInfo.setCameraShadingCorrection(intValue);
                                }
                                break;
                            case 65550:
                                Preferences.debug("tagID = Axio cam enhance color\n", Preferences.DEBUG_FILEIO);
                                if (valueDType == VT_I4) {
                                    fileInfo.setAxioCamEnhanceColor(intValue);
                                }
                                break;
                            case 65551:
                                Preferences.debug("tagID = Axio cam NIR mode\n", Preferences.DEBUG_FILEIO);
                                if (valueDType == VT_I4) {
                                    fileInfo.setAxioCamNIRMode(intValue);
                                }
                                break;
                            case 65552:
                                Preferences.debug("tagID = Camera shutter close delay\n", Preferences.DEBUG_FILEIO);
                                break;
                            case 65553:
                                Preferences.debug("tagID = Camera white balance auto calculate\n", Preferences.DEBUG_FILEIO);
                                break;
                            case 65556:
                                Preferences.debug("tagID = Axio cam NIR mode available\n", Preferences.DEBUG_FILEIO);
                                break;
                            case 65557:
                                Preferences.debug("tagID = Axio cam fading correction available\n", Preferences.DEBUG_FILEIO);
                                break;
                            case 65559:
                                Preferences.debug("tagID = Axio cam enhance color available\n", Preferences.DEBUG_FILEIO);
                                break;
                            case 65565:
                                Preferences.debug("tagID = Meteor video norm\n", Preferences.DEBUG_FILEIO);
                                break;
                            case 65566:
                                Preferences.debug("tagID = Meteor adjust white reference\n", Preferences.DEBUG_FILEIO);
                                break;
                            case 65567:
                                Preferences.debug("tagID = Meteor black reference\n", Preferences.DEBUG_FILEIO);
                                break;
                            case 65568:
                                Preferences.debug("tagID = Meteor channel input count mono\n", Preferences.DEBUG_FILEIO);
                                break;
                            case 65570:
                                Preferences.debug("tagID = Meteor channel input count RGB\n", Preferences.DEBUG_FILEIO);
                                break;
                            case 65571:
                                Preferences.debug("tagID = Meteor enable VCR\n", Preferences.DEBUG_FILEIO);
                                break;
                            case 65572:
                                Preferences.debug("tagID = Meteor brightness\n", Preferences.DEBUG_FILEIO);
                                break;
                            case 65573:
                                Preferences.debug("tagID = Meteor contrast\n", Preferences.DEBUG_FILEIO);
                                break;
                            case 65575:
                                Preferences.debug("tagID = Axio cam selector\n", Preferences.DEBUG_FILEIO);
                                if (valueDType == VT_I4) {
                                    fileInfo.setAxioCamSelector(intValue);
                                }
                                break;
                            case 65576:
                                Preferences.debug("tagID = Axio cam type\n", Preferences.DEBUG_FILEIO);
                                if (valueDType == VT_I4) {
                                    fileInfo.setAxioCamType(intValue);
                                }
                                break;
                            case 65577:
                                Preferences.debug("tagID = Axio cam info\n", Preferences.DEBUG_FILEIO);
                                break;
                            case 65580:
                                Preferences.debug("tagID = Axio cam resolution\n", Preferences.DEBUG_FILEIO);
                                if (valueDType == VT_I4) {
                                    fileInfo.setAxioCamResolution(intValue);
                                }
                                break;
                            case 65581:
                                Preferences.debug("tagID = Axio cam color model\n", Preferences.DEBUG_FILEIO);
                                if (valueDType == VT_I4) {
                                    fileInfo.setAxioCamColorModel(intValue);
                                }
                                break;
                            case 65582:
                                Preferences.debug("tagID = Axio cam micro scanning\n", Preferences.DEBUG_FILEIO);
                                if (valueDType == VT_I4) {
                                    fileInfo.setAxioCamMicroScanning(intValue);
                                }
                                break;
                            case 65585:
                                Preferences.debug("tagID = Amplification index\n", Preferences.DEBUG_FILEIO);
                                if (valueDType == VT_I4) {
                                    fileInfo.setAmplificationIndex(intValue);
                                }
                                break;
                            case 65586:
                                Preferences.debug("tagID = Device command\n", Preferences.DEBUG_FILEIO);
                                break;
                            case 65587:
                                Preferences.debug("tagID = Beam location\n", Preferences.DEBUG_FILEIO);
                                break;
                            case 65588:
                                Preferences.debug("tagID = Component type\n", Preferences.DEBUG_FILEIO);
                                break;
                            case 65589:
                                Preferences.debug("tagID = Controller type\n", Preferences.DEBUG_FILEIO);
                                break;
                            case 65590:
                                Preferences.debug("tagID = Camera white balance calculation red paint\n", 
                                		Preferences.DEBUG_FILEIO);
                                break;
                            case 65591:
                                Preferences.debug("tagID = Camera white balance calculation blue paint\n", 
                                		Preferences.DEBUG_FILEIO);
                                break;
                            case 65592:
                                Preferences.debug("tagID= Camera white balance set red\n", Preferences.DEBUG_FILEIO);
                                break;
                            case 65593:
                                Preferences.debug("tagID = Camera white balance set green\n", Preferences.DEBUG_FILEIO);
                                break;
                            case 65594:
                                Preferences.debug("tagID = Camera white balance set blue\n", Preferences.DEBUG_FILEIO);
                                break;
                            case 65595:
                                Preferences.debug("tagID = Camera white balance set target red\n", Preferences.DEBUG_FILEIO);
                                break;
                            case 65596:
                                Preferences.debug("tagID = Camera white balance set target green\n", Preferences.DEBUG_FILEIO);
                                break;
                            case 65597:
                                Preferences.debug("tagID = Camera white balance set target blue\n", Preferences.DEBUG_FILEIO);
                                break;
                            case 65598:
                                Preferences.debug("tagID = Apotome cam calibration mode\n", Preferences.DEBUG_FILEIO);
                                break;
                            case 65599:
                                Preferences.debug("tagID = Apotome grid position\n", Preferences.DEBUG_FILEIO);
                                if (valueDType == VT_I4) {
                                    // A different value seen for each channel
                                    apotomeGridPosition = intValue;
                                }
                                break;
                            case 65600:
                                Preferences.debug("tagID = Apotome cam scanner position\n", Preferences.DEBUG_FILEIO);
                                break;
                            case 65601:
                                Preferences.debug("tagID = Apotome full phase shift\n", Preferences.DEBUG_FILEIO);
                                if (valueDType == VT_I4) {
                                    fileInfo.setApotomeFullPhaseShift(intValue);
                                }
                                break;
                            case 65602:
                                Preferences.debug("tagID = Apotome grid name\n", Preferences.DEBUG_FILEIO);
                                break;
                            case 65603:
                                Preferences.debug("tagID = Apotome staining\n", Preferences.DEBUG_FILEIO);
                                break;
                            case 65604:
                                Preferences.debug("tagID = Apotome processing mode\n", Preferences.DEBUG_FILEIO);
                                if (valueDType == VT_I4) {
                                    fileInfo.setApotomeProcessingMode(intValue);
                                }
                                break;
                            case 65605:
                                Preferences.debug("tagID = Apotome cam live combine mode\n", Preferences.DEBUG_FILEIO);
                                break;
                            case 65606:
                                Preferences.debug("tagID = Apotome filter name\n", Preferences.DEBUG_FILEIO);
                                break;
                            case 65607:
                                Preferences.debug("tagID = Apotome filter strength\n", Preferences.DEBUG_FILEIO);
                                if (valueDType == VT_R8) {
                                    fileInfo.setApotomeFilterStrength(doubleValue);
                                }
                                break;
                            case 65608:
                                Preferences.debug("tagID = Apotome cam filter harmonics\n", Preferences.DEBUG_FILEIO);
                                if (valueDType == VT_I4) {
                                    fileInfo.setApotomeCamFilterHarmonics(intValue);
                                }
                                break;
                            case 65609:
                                Preferences.debug("tagID = Apotome grating period\n", Preferences.DEBUG_FILEIO);
                                if (valueDType == VT_R8) {
                                    fileInfo.setApotomeGratingPeriod(doubleValue);
                                }
                                break;
                            case 65610:
                                Preferences.debug("tagID = Apotome auto shutter used\n", Preferences.DEBUG_FILEIO);
                                if (valueDType == VT_I4) {
                                    fileInfo.setApotomeAutoShutterUsed(intValue);
                                }
                                break;
                            case 65611:
                                Preferences.debug("tagID = Apotome cam status\n", Preferences.DEBUG_FILEIO);
                                break;
                            case 65612:
                                Preferences.debug("tagID = Apotome cam normalize\n", Preferences.DEBUG_FILEIO);
                                if (valueDType == VT_I4) {
                                    fileInfo.setApotomeCamNormalize(intValue);
                                }
                                break;
                            case 65613:
                                Preferences.debug("tagID = Apotome cam settings manager\n", Preferences.DEBUG_FILEIO);
                                break;
                            case 65614:
                                Preferences.debug("tagID = Deep view cam supervisor mode\n", Preferences.DEBUG_FILEIO);
                                break;
                            case 65615:
                                Preferences.debug("tagID = Deep view processing\n", Preferences.DEBUG_FILEIO);
                                break;
                            case 65616:
                                Preferences.debug("tagID = Deep view cam filter name\n", Preferences.DEBUG_FILEIO);
                                break;
                            case 65617:
                                Preferences.debug("tagID = Deep view cam status\n", Preferences.DEBUG_FILEIO);
                                break;
                            case 65618:
                                Preferences.debug("tagID = Deep view cam settings manager\n", Preferences.DEBUG_FILEIO);
                                break;
                            case 65619:
                                Preferences.debug("tagID = Device scaling name\n", Preferences.DEBUG_FILEIO);
                                break;
                            case 65620:
                                Preferences.debug("tagID = Camera shading is calculated\n", Preferences.DEBUG_FILEIO);
                                break;
                            case 65621:
                                Preferences.debug("tagID = Camera shading calculation name\n", Preferences.DEBUG_FILEIO);
                                break;
                            case 65622:
                                Preferences.debug("tagID = Camera shading autocalculate\n", Preferences.DEBUG_FILEIO);
                                break;
                            case 65623:
                                Preferences.debug("tagID = Camera trigger available\n", Preferences.DEBUG_FILEIO);
                                break;
                            case 65626:
                                Preferences.debug("tagID = Camera shutter available\n", Preferences.DEBUG_FILEIO);
                                break;
                            case 65627:
                                Preferences.debug("tagID = Axio cam shutter micro scanning enable\n", Preferences.DEBUG_FILEIO);
                                break;
                            case 65628:
                                Preferences.debug("tagID = Apotome cam live focus\n", Preferences.DEBUG_FILEIO);
                                break;
                            case 65629:
                                Preferences.debug("tagID = Device init status\n", Preferences.DEBUG_FILEIO);
                                break;
                            case 65630:
                                Preferences.debug("tagID = Device error status\n", Preferences.DEBUG_FILEIO);
                                break;
                            case 65631:
                                Preferences.debug("tagID = Apotome cam slider in grid position\n", Preferences.DEBUG_FILEIO);
                                break;
                            case 65632:
                                Preferences.debug("tagID = Orca NIR mode used\n", Preferences.DEBUG_FILEIO);
                                break;
                            case 65633:
                                Preferences.debug("tagID = Orca analog gain\n", Preferences.DEBUG_FILEIO);
                                break;
                            case 65634:
                                Preferences.debug("tagID = Orca analog offset\n", Preferences.DEBUG_FILEIO);
                                break;
                            case 65635:
                                Preferences.debug("tagID = Orca binning\n", Preferences.DEBUG_FILEIO);
                                break;
                            case 65536:
                                Preferences.debug("tagID = Orca bit depth\n", Preferences.DEBUG_FILEIO);
                                break;
                            case 65637:
                                Preferences.debug("tagID = Apotome averaging count\n", Preferences.DEBUG_FILEIO);
                                if (valueDType == VT_I4) {
                                    fileInfo.setApotomeAveragingCount(intValue);
                                }
                                break;
                            case 65638:
                                Preferences.debug("tagID = Deep view DoF\n", Preferences.DEBUG_FILEIO);
                                break;
                            case 65639:
                                Preferences.debug("tagID = Deep view EDoF\n", Preferences.DEBUG_FILEIO);
                                break;
                            case 65643:
                                Preferences.debug("tagID = Deep view slider name\n", Preferences.DEBUG_FILEIO);
                                break;
                            case 65644:
                                Preferences.debug("tagID = Roper Cam Gain\n", Preferences.DEBUG_FILEIO);
                                break;
                            case 65646:
                                Preferences.debug("tagID = Roper Cam Pixel Clock\n", Preferences.DEBUG_FILEIO);
                                break;
                            case 65647:
                                Preferences.debug("tagID = Roper Cam Temperature\n", Preferences.DEBUG_FILEIO);
                                break;
                            case 65648:
                                Preferences.debug("tagID = Camera Image Mem Unit Names\n", Preferences.DEBUG_FILEIO);
                                break;
                            case 65649:
                                Preferences.debug("tagID = Apotome Cam Live Phase\n", Preferences.DEBUG_FILEIO);
                                break;
                            case 65650:
                                Preferences.debug("tagID = Dual Axio Cam Algorithm Type\n", Preferences.DEBUG_FILEIO);
                                break;
                            case 65651:
                                Preferences.debug("tagID = Apotome Cam Decay\n", Preferences.DEBUG_FILEIO);
                                break;
                            case 65652:
                                Preferences.debug("tagID = Apotome Cam Epsilon\n", Preferences.DEBUG_FILEIO);
                                break;
                            case 65653:
                                Preferences.debug("tagID = Axio Cam HSBuffer Number\n", Preferences.DEBUG_FILEIO);
                                break;
                            case 65654:
                                Preferences.debug("tagID = Axio Cam HSFrame Time\n", Preferences.DEBUG_FILEIO);
                                break;
                            case 65655:
                                Preferences.debug("tagID = Axio Cam Analog Gain Enable\n", Preferences.DEBUG_FILEIO);
                                break;
                            case 65656:
                                Preferences.debug("tagID = Axio Cam Analog Gain Available\n", Preferences.DEBUG_FILEIO);
                                break;
                            case 65657:
                                Preferences.debug("tagID = Apotome Cam Phase Angles\n", Preferences.DEBUG_FILEIO);
                                break;
                            case 65658:
                                Preferences.debug("tagID = Apotome Cam Image Format\n", Preferences.DEBUG_FILEIO);
                                break;
                            case 65659:
                                Preferences.debug("tagID = Camera Shading Count\n", Preferences.DEBUG_FILEIO);
                                break;
                            case 65660:
                                Preferences.debug("tagID = Camera Image Raw Size\n", Preferences.DEBUG_FILEIO);
                                break;
                            case 65661:
                                Preferences.debug("tagID = Apotome Cam Burst Mode\n", Preferences.DEBUG_FILEIO);
                                break;
                            case 65662:
                                Preferences.debug("tagID = Apotome Cam Generic Camera Name\n", Preferences.DEBUG_FILEIO);
                                break;
                            case 65663:
                                Preferences.debug("tagID = Acquisition Device\n", Preferences.DEBUG_FILEIO);
                                break;
                            case 65664:
                                Preferences.debug("tagID = Apotome Grating Period Measured\n", Preferences.DEBUG_FILEIO);
                                break;
                            case 65665:
                                Preferences.debug("tagID = Camera Lut Enable\n", Preferences.DEBUG_FILEIO);
                                break;
                            case 65666:
                                Preferences.debug("tagID = Axio Cam Saturation\n", Preferences.DEBUG_FILEIO);
                                break;
                            case 65667:
                                Preferences.debug("tagID = Camera Color Correction\n", Preferences.DEBUG_FILEIO);
                                break;
                            case 65668:
                                Preferences.debug("tagID = Camera Color Processing Enable\n", Preferences.DEBUG_FILEIO);
                                break;
                            case 65669:
                                Preferences.debug("tagID = Camera Analog Gain\n", Preferences.DEBUG_FILEIO);
                                break;
                            case 65670:
                                Preferences.debug("tagID = Camera White Balance Target PosX\n", Preferences.DEBUG_FILEIO);
                                break;
                            case 65671:
                                Preferences.debug("tagID = Camera White Balance Target PosY\n", Preferences.DEBUG_FILEIO);
                                break;
                            case 65672:
                                Preferences.debug("tagID = Camera Shutter Signal Port\n", Preferences.DEBUG_FILEIO);
                                break;
                            case 65673:
                                Preferences.debug("tagID = Axio Cam IC Saturation\n", Preferences.DEBUG_FILEIO);
                                break;
                            case 65674:
                                Preferences.debug("tagID = Apotome Cam Cam Calib Mode\n", Preferences.DEBUG_FILEIO);
                                break;
                            case 65675:
                                Preferences.debug("tagID = Apotome Cam Cam Calib Value\n", Preferences.DEBUG_FILEIO);
                                break;
                            case 65676:
                                Preferences.debug("tagID = Apotome Cam Admin Calib Mode\n", Preferences.DEBUG_FILEIO);
                                break;
                            case 65677:
                                Preferences.debug("tagID = Apotome Cam Is Admin\n", Preferences.DEBUG_FILEIO);
                                break;
                            case 65678:
                                Preferences.debug("tagID = Apotome Cam Pw\n", Preferences.DEBUG_FILEIO);
                                break;
                            case 65679:
                                Preferences.debug("tagID = Apotome Cam Admin Name\n", Preferences.DEBUG_FILEIO);
                                break;
                            case 65680:
                                Preferences.debug("tagID = Camera Shutter Live Enable\n", Preferences.DEBUG_FILEIO);
                                break;
                            case 65681:
                                Preferences.debug("tagID = Camera Exposure Time Auto Live Enable\n", Preferences.DEBUG_FILEIO);
                                break;
                            case 65682:
                                Preferences.debug("tagID = Camera EM Gain\n", Preferences.DEBUG_FILEIO);
                                break;
                            case 65683:
                                Preferences.debug("tagID = Apotome Cam Hardware Version\n", Preferences.DEBUG_FILEIO);
                                break;
                            case 65684:
                                Preferences.debug("tagID = Apotome Cam Grid Position\n", Preferences.DEBUG_FILEIO);
                                break;
                            case 65685:
                                Preferences.debug("tagID = Apotome Cam Auto Grid\n", Preferences.DEBUG_FILEIO);
                                break;
                            case 65703:
                                Preferences.debug("tagID = Orca Cam Number Of Scan Mode\n", Preferences.DEBUG_FILEIO);
                                break;
                            case 65704:
                                Preferences.debug("tagID = Orca Cam Scan Mode\n", Preferences.DEBUG_FILEIO);
                                break;
                            case 65705:
                                Preferences.debug("tagID = Orca Cam EMCCD Mode\n", Preferences.DEBUG_FILEIO);
                                break;
                            case 65706:
                                Preferences.debug("tagID = Orca Cam EMCCD Gain\n", Preferences.DEBUG_FILEIO);
                                break;
                            case 65707:
                                Preferences.debug("tagID = Orca Cam Fast Acq\n", Preferences.DEBUG_FILEIO);
                                break;
                            case 65708:
                                Preferences.debug("tagID = Orca Cam Min Exposure Time\n", Preferences.DEBUG_FILEIO);
                                break;
                            case 65709:
                                Preferences.debug("tagID = Orca Cam Number Of Photon Imaging Mode\n", Preferences.DEBUG_FILEIO);
                                break;
                            case 65710:
                                Preferences.debug("tagID = Orca Cam Photon Imaging Mode\n", Preferences.DEBUG_FILEIO);
                                break;
                            case 65711:
                                Preferences.debug("tagID = Orca Cam Direct EM Gain Available\n", Preferences.DEBUG_FILEIO);
                                break;
                            case 65712:
                                Preferences.debug("tagID = Orca Cam Direct EM Gain\n", Preferences.DEBUG_FILEIO);
                                break;
                            case 65713:
                                Preferences.debug("tagID = Orca Cam EM Gain Protection Available\n", Preferences.DEBUG_FILEIO);
                                break;
                            case 65714:
                                Preferences.debug("tagID = Orca Cam EM Gain Protection\n", Preferences.DEBUG_FILEIO);
                                break;
                            case 65716:
                                Preferences.debug("tagID = Camera EM Gain Minimum\n", Preferences.DEBUG_FILEIO);
                                break;
                            case 65717:
                                Preferences.debug("tagID = Camera EM Gain Maximum\n", Preferences.DEBUG_FILEIO);
                                break;
                            case 65718:
                                Preferences.debug("tagID = Camera EM Gain Available\n", Preferences.DEBUG_FILEIO);
                                break;
                            case 65719:
                                Preferences.debug("tagID = Camera EM Gain Enabled\n", Preferences.DEBUG_FILEIO);
                                break;
                            case 65736:
                                Preferences.debug("tagID = Yokogawa Synchronize\n", Preferences.DEBUG_FILEIO);
                                break;
                            case 65737:
                                Preferences.debug("tagID = Yokogawa Is In Sync\n", Preferences.DEBUG_FILEIO);
                                break;
                            case 65738:
                                Preferences.debug("tagID = Yokogawa Status\n", Preferences.DEBUG_FILEIO);
                                break;
                            case 65739:
                                Preferences.debug("tagID = Yokogawa Keep In Sync\n", Preferences.DEBUG_FILEIO);
                                break;
                            case 65740:
                                Preferences.debug("tagID = Yokogawa Is Busy\n", Preferences.DEBUG_FILEIO);
                                break;
                            case 65741:
                                Preferences.debug("tagID = Yokogawa Stop Disc\n", Preferences.DEBUG_FILEIO);
                                break;
                            case 65742:
                                Preferences.debug("tagID = Yokogawa Cam Exposure Time\n", Preferences.DEBUG_FILEIO);
                                break;
                            case 65743:
                                Preferences.debug("tagID = Yokogawa Mode\n", Preferences.DEBUG_FILEIO);
                                break;
                            case 65744:
                                Preferences.debug("tagID = Yokogawa Depth\n", Preferences.DEBUG_FILEIO);
                                break;
                            case 65745:
                                Preferences.debug("tagID = Yokogawa Cam Reserved12\n", Preferences.DEBUG_FILEIO);
                                break;
                            case 65746:
                                Preferences.debug("tagID = Yokogawa Cam Reserved13\n", Preferences.DEBUG_FILEIO);
                                break;
                            case 65747:
                                Preferences.debug("tagID = Yokogawa Cam Reserved14\n", Preferences.DEBUG_FILEIO);
                                break;
                            case 65748:
                                Preferences.debug("tagID = Yokogawa Cam Reserved15\n", Preferences.DEBUG_FILEIO);
                                break;
                            case 65749:
                                Preferences.debug("tagID = Yokogawa Cam Reserved16\n", Preferences.DEBUG_FILEIO);
                                break;
                            case 65750:
                                Preferences.debug("tagID = Yokogawa Cam Reserved17\n", Preferences.DEBUG_FILEIO);
                                break;
                            case 65751:
                                Preferences.debug("tagID = Yokogawa Cam Reserved18\n", Preferences.DEBUG_FILEIO);
                                break;
                            case 65752:
                                Preferences.debug("tagID = Yokogawa Cam Reserved19\n", Preferences.DEBUG_FILEIO);
                                break;
                            case 65753:
                                Preferences.debug("tagID = Yokogawa Cam Reserved20\n", Preferences.DEBUG_FILEIO);
                                break;
                            case 65754:
                                Preferences.debug("tagID = Aurox Cam Status\n", Preferences.DEBUG_FILEIO);
                                break;
                            case 65755:
                                Preferences.debug("tagID = Aurox Cam Input Mode\n", Preferences.DEBUG_FILEIO);
                                break;
                            case 65756:
                                Preferences.debug("tagID = Aurox Cam Live Mode\n", Preferences.DEBUG_FILEIO);
                                break;
                            case 65757:
                                Preferences.debug("tagID = Aurox Cam Calibration Mode\n", Preferences.DEBUG_FILEIO);
                                break;
                            case 65758:
                                Preferences.debug("tagID = Aurox Cam Generic Camera Name\n", Preferences.DEBUG_FILEIO);
                                break;
                            case 65759:
                                Preferences.debug("tagID = Aurox Cam Button Mode\n", Preferences.DEBUG_FILEIO);
                                break;
                            case 65760:
                                Preferences.debug("tagID = Aurox Cam Depth\n", Preferences.DEBUG_FILEIO);
                                break;
                            case 65761:
                                Preferences.debug("tagID = Aurox Cam Center\n", Preferences.DEBUG_FILEIO);
                                break;
                            case 65762:
                                Preferences.debug("tagID = Aurox Cam Factor\n", Preferences.DEBUG_FILEIO);
                                break;
                            case 65763:
                                Preferences.debug("tagID = Aurox Cam Create Registration\n", Preferences.DEBUG_FILEIO);
                                break;
                            case 65764:
                                Preferences.debug("tagID = Aurox Cam Regoistration Valid\n", Preferences.DEBUG_FILEIO);
                                break;
                            case 65765:
                                Preferences.debug("tagID = Aurox Cam Registration Error\n", Preferences.DEBUG_FILEIO);
                                break;
                            case 65766:
                                Preferences.debug("tagID = Aurox Cam Shading Image Mode\n", Preferences.DEBUG_FILEIO);
                                break;
                            case 65767:
                                Preferences.debug("tagID = Aurox Cam Shading Image Available\n", Preferences.DEBUG_FILEIO);
                                break;
                            case 65768:
                                Preferences.debug("tagID = Aurox Cam Quality\n", Preferences.DEBUG_FILEIO);
                                break;
                            case 65769:
                                Preferences.debug("tagID = Aurox Cam Cut Left\n", Preferences.DEBUG_FILEIO);
                                break;
                            case 65770:
                                Preferences.debug("tagID = Aurox Cam Cut Top\n", Preferences.DEBUG_FILEIO);
                                break;
                            case 65771:
                                Preferences.debug("tagID = Aurox Cam Cut Right\n", Preferences.DEBUG_FILEIO);
                                break;
                            case 65772:
                                Preferences.debug("tagID = Aurox Cam Cut Bottom\n", Preferences.DEBUG_FILEIO);
                                break;
                            case 65773:
                                Preferences.debug("tagID = Aurox Cam Mean\n", Preferences.DEBUG_FILEIO);
                                break;
                            case 65774:
                                Preferences.debug("tagID = Aurox Cam Normalize\n", Preferences.DEBUG_FILEIO);
                                break;
                            case 65775:
                                Preferences.debug("tagID = Aurox Cam Use Shading\n", Preferences.DEBUG_FILEIO);
                                break;
                            case 65776:
                                Preferences.debug("tagID = Aurox Cam Shading Valid\n", Preferences.DEBUG_FILEIO);
                                break;
                            case 65777:
                                Preferences.debug("tagID = Aurox Cam Notification\n", Preferences.DEBUG_FILEIO);
                                break;
                            case 65778:
                                Preferences.debug("tagID = Aurox Cam Calibration ID\n", Preferences.DEBUG_FILEIO);
                                break;
                            case 65779:
                                Preferences.debug("tagID = Aurox Cam Simple Calib Mode\n", Preferences.DEBUG_FILEIO);
                                break;
                            case 65780:
                                Preferences.debug("tagID = Aurox Cam Calibration Name\n", Preferences.DEBUG_FILEIO);
                                break;
                            case 65781:
                                Preferences.debug("tagID = Aurox Cam CFactor\n", Preferences.DEBUG_FILEIO);
                                break;
                            case 65782:
                                Preferences.debug("tagID = Aurox Cam Registration Center\n", Preferences.DEBUG_FILEIO);
                                break;
                            case 65783:
                                Preferences.debug("tagID = Aurox Cam Calibration ID2\n", Preferences.DEBUG_FILEIO);
                                break;
                            case 65784:
                                Preferences.debug("tagID = Aurox Cam Averaging\n", Preferences.DEBUG_FILEIO);
                                break;
                            case 65785:
                                Preferences.debug("tagID = Aurox Cam Unique ID\n", Preferences.DEBUG_FILEIO);
                                break;
                            case 65786:
                                Preferences.debug("tagID = Aurox Cam Auto Normalize\n", Preferences.DEBUG_FILEIO);
                                break;
                            case 65787:
                                Preferences.debug("tagID = Aurox Cam Reserved3\n", Preferences.DEBUG_FILEIO);
                                break;
                            case 65788:
                                Preferences.debug("tagID = Aurox Cam Reserved4\n", Preferences.DEBUG_FILEIO);
                                break;
                            case 65789:
                                Preferences.debug("tagID = Aurox Cam Reserved5\n", Preferences.DEBUG_FILEIO);
                                break;
                            case 5439491:
                                Preferences.debug("tag ID = Acquisition software\n", Preferences.DEBUG_FILEIO);
                                break;
                            case 16777488:
                                Preferences.debug("tagID = Excitation wavelength\n", Preferences.DEBUG_FILEIO);
                                if (valueDType == VT_I4) {
                                    excitationWavelength = intValue;
                                }
                                break;
                            case 16777489:
                                Preferences.debug("tagID = Emission wavelength\n", Preferences.DEBUG_FILEIO);
                                if (valueDType == VT_I4) {
                                    emissionWavelength = intValue;
                                }
                                break;
                            case 101515267:
                                Preferences.debug("tagID = File name\n", Preferences.DEBUG_FILEIO);
                                break;
                            case 101253123:
                            case 101777411:
                                Preferences.debug("tagID = Image name\n", Preferences.DEBUG_FILEIO);
                                break;
                            default: Preferences.debug("Unrecognized tagID value = " + tagID + "\n", Preferences.DEBUG_FILEIO);
                        }
                        
                        dType = (short) (((b[bp+1] & 0xff) << 8) | (b[bp] & 0xff));
                        bp += 2;
                        if (dType == VT_I4) {
                            Preferences.debug("Expected VT_I4 data type for unused attribute\n", Preferences.DEBUG_FILEIO);
                        }
                        else {
                            Preferences.debug("dType = " + dType + " instead of expected VT_I4 for unused attribute\n", 
                            		Preferences.DEBUG_FILEIO);
                            break trueLoop;
                        }
                        int attribute = (((b[bp + 3] & 0xff) << 24) | ((b[bp + 2] & 0xff) << 16) | 
                                ((b[bp + 1] & 0xff) << 8) | (b[bp] & 0xff));
                        bp += 4;
                        Preferences.debug("Unused attribute = " + attribute + "\n", Preferences.DEBUG_FILEIO);
                    } // for (i = 0; i < tokenCount  && bp < b.length - 13; i++)
                    
                    if (!Double.isNaN(exposureTime)) {
                        switch(cValue) {
                            case 0:
                                fileInfo.setExposureTime0(exposureTime);
                                break;
                            case 1:
                                fileInfo.setExposureTime1(exposureTime);
                                break;
                            case 2:
                                fileInfo.setExposureTime2(exposureTime);
                                break;
                            case 3:
                                fileInfo.setExposureTime3(exposureTime);
                                break;
                            default:
                        } // switch(cValue)
                    } // if (!Double.isNaN(exposureTime)
                    
                    if (apotomeGridPosition != Integer.MIN_VALUE) {
                        switch(cValue) {
                            case 0:
                                fileInfo.setApotomeGridPosition0(apotomeGridPosition);
                                break;
                            case 1:
                                fileInfo.setApotomeGridPosition1(apotomeGridPosition);
                                break;
                            case 2:
                                fileInfo.setApotomeGridPosition2(apotomeGridPosition);
                                break;
                            case 3:
                                fileInfo.setApotomeGridPosition3(apotomeGridPosition);
                                break;
                            default:
                        } // switch(cValue)
                    } // if (apotomeGridPosition >= 0)
                    
                    if (excitationWavelength != Integer.MIN_VALUE) {
                        switch(cValue) {
                            case 0:
                                fileInfo.setExcitationWavelength0(excitationWavelength);
                                break;
                            case 1:
                                fileInfo.setExcitationWavelength1(excitationWavelength);
                                break;
                            case 2:
                                fileInfo.setExcitationWavelength2(excitationWavelength);
                                break;
                            case 3:
                                fileInfo.setExcitationWavelength3(excitationWavelength);
                                break;
                        } // switch(cValue)
                    } // if (excitationWavelength != Integer.MIN_VALUE)
                    
                    if (emissionWavelength != Integer.MIN_VALUE) {
                        switch(cValue) {
                            case 0:
                                fileInfo.setEmissionWavelength0(emissionWavelength);
                                break;
                            case 1:
                                fileInfo.setEmissionWavelength1(emissionWavelength);
                                break;
                            case 2:
                                fileInfo.setEmissionWavelength2(emissionWavelength);
                                break;
                            case 3:
                                fileInfo.setEmissionWavelength3(emissionWavelength);
                                break;
                        } // switch(cValue)
                    } // if (emissionWavelength != Integer.MIN_VALUE)
                    
                    if (reflectorPosition != Integer.MIN_VALUE) {
                        switch(cValue) {
                            case 0:
                                fileInfo.setReflectorPosition0(reflectorPosition);
                                break;
                            case 1:
                                fileInfo.setReflectorPosition1(reflectorPosition);
                                break;
                            case 2:
                                fileInfo.setReflectorPosition2(reflectorPosition);
                                break;
                            case 3:
                                fileInfo.setReflectorPosition3(reflectorPosition);
                                break;
                            default:
                        } // switch(cValue)
                    } // if (reflectorPosition != Integer.MIN_VALUE)
                    
                    if (multichannelColor != Integer.MIN_VALUE) {
                        switch(cValue) {
                            case 0:
                                fileInfo.setMultichannelColor0(multichannelColor);
                                break;
                            case 1:
                                fileInfo.setMultichannelColor1(multichannelColor);
                                break;
                            case 2:
                                fileInfo.setMultichannelColor2(multichannelColor);
                                break;
                            case 3:
                                fileInfo.setMultichannelColor3(multichannelColor);
                                break;
                            default:
                        } // switch(cValue)
                    } // if (multichannelColor != Integer.MIN_VALUE)
                    
                    if (useZValue && haveSecondTileValue && (!haveSecondZValue)) {
                        useZValue = false;
                        useTileValue = true;
                        if (imageZArray[0] != Integer.MIN_VALUE) {
                            imageZArray[0] = firstTileValue;
                        }
                        if (imageZ2Array[0] != Integer.MIN_VALUE) {
                            imageZ2Array[0] = firstTileValue;
                        }
                        if (imageZ3Array[0] != Integer.MIN_VALUE) {
                            imageZ3Array[0] = firstTileValue;
                        }
                        if (imageZ4Array[0] != Integer.MIN_VALUE) {
                            imageZ4Array[0] = firstTileValue;
                        }
                        if (imageZ5Array[0] != Integer.MIN_VALUE) {
                            imageZ5Array[0] = firstTileValue;
                        }
                        if (imageZ6Array[0] != Integer.MIN_VALUE) {
                            imageZ6Array[0] = firstTileValue;
                        }
                        if (imageZ7Array[0] != Integer.MIN_VALUE) {
                            imageZ7Array[0] = firstTileValue;
                        }
                        if (imageZXArray[0] != Integer.MIN_VALUE) {
                            imageZXArray[0] = firstTileValue;
                        }
                        if (imageZYArray[0] != Integer.MIN_VALUE) {
                            imageZYArray[0] = firstTileValue;
                        }
                    }
                    
                    if (useZValue) {
                        zTileValue = zValue;
                    }
                    else {
                        zTileValue = tileValue;
                    }
                    
                    
                    if ((zTileValue != Integer.MIN_VALUE) && (!Double.isNaN(focusPosition))) {
               
                        boolean doFill = true;
                        for (i = 0; i < icp; i++) {
                            if (imageZArray[i] == zTileValue) {
                                doFill = false;
                            }
                        }
                        if (doFill) {
                            imageZArray[icp] = zTileValue;
                            imageFocusPositionArray[icp++] = focusPosition;
                        }
                    }
                    
                    if ((zTileValue != Integer.MIN_VALUE) && (!Double.isNaN(relFocusPosition1))) {
                        boolean doFill = true;
                        for (i = 0; i < icp4; i++) {
                            if (imageZ4Array[i] == zTileValue) {
                                doFill = false;
                            }
                        }
                        if (doFill) {
                            imageZ4Array[icp4] = zTileValue;
                            imageRelFocusPosition1Array[icp4++] = relFocusPosition1;
                        }
                    }
                    
                    if ((zTileValue != Integer.MIN_VALUE) && (!Double.isNaN(relFocusPosition2))) {
                        boolean doFill = true;
                        for (i = 0; i < icp5; i++) {
                            if (imageZ5Array[i] == zTileValue) {
                                doFill = false;
                            }
                        }
                        if (doFill) {
                            imageZ5Array[icp5] = zTileValue;
                            imageRelFocusPosition2Array[icp5++] = relFocusPosition2;
                        }
                    }
                    
                    if ((zTileValue != Integer.MIN_VALUE) && (!Double.isNaN(stagePositionX))) {
                        boolean doFill = true;
                        for (i = 0; i < icpX; i++) {
                            if (imageZXArray[i] == zTileValue) {
                                doFill = false;
                            }
                        }
                        if (doFill) {
                            imageZXArray[icpX] = zTileValue;
                            imageStagePositionXArray[icpX++] = stagePositionX;
                        }
                    }
                    
                    if ((zTileValue != Integer.MIN_VALUE) && (!Double.isNaN(stagePositionY))) {
                        boolean doFill = true;
                        for (i = 0; i < icpY; i++) {
                            if (imageZYArray[i] == zTileValue) {
                                doFill = false;
                            }
                        }
                        if (doFill) {
                            imageZYArray[icpY] = zTileValue;
                            imageStagePositionYArray[icpY++] = stagePositionY;
                        }
                    }
                    
                    if ((zTileValue != Integer.MIN_VALUE) && (cValue != Integer.MIN_VALUE) &&
                        (tValue != Integer.MIN_VALUE) && (!Double.isNaN(blackValue))) {
                        boolean doFill = true;
                        for (i = 0; i < icp2; i++) {
                            if ((imageZ2Array[i] == zTileValue) && (imageC2Array[i] == cValue) &&
                                (imageT2Array[i] == tValue)) {
                                doFill = false;
                            }
                        }
                        if (doFill) {
                            imageZ2Array[icp2] = zTileValue;
                            imageC2Array[icp2] = cValue;
                            imageT2Array[icp2] = tValue;
                            imageBlackValueArray[icp2++] = blackValue;
                        }
                    }
                    
                    if ((zTileValue != Integer.MIN_VALUE) && (cValue != Integer.MIN_VALUE) &&
                        (tValue != Integer.MIN_VALUE) && (!Double.isNaN(whiteValue))) {
                        boolean doFill = true;
                        for (i = 0; i < icp3; i++) {
                            if ((imageZ3Array[i] == zTileValue) && (imageC3Array[i] == cValue) &&
                                (imageT3Array[i] == tValue)) {
                                doFill = false;
                            }
                        }
                        if (doFill) {
                            imageZ3Array[icp3] = zTileValue;
                            imageC3Array[icp3] = cValue;
                            imageT3Array[icp3] = tValue;
                            imageWhiteValueArray[icp3++] = whiteValue;
                        }
                    }
                    
                    if ((zTileValue != Integer.MIN_VALUE) && (cValue != Integer.MIN_VALUE) &&
                        (tValue != Integer.MIN_VALUE) && (!Double.isNaN(acqTime))) {
                        boolean doFill = true;
                        for (i = 0; i < icp6; i++) {
                            if ((imageZ6Array[i] == zTileValue) && (imageC6Array[i] == cValue) &&
                                (imageT6Array[i] == tValue)) {
                                doFill = false;
                            }
                        }
                        if (doFill) {
                            imageZ6Array[icp6] = zTileValue;
                            imageC6Array[icp6] = cValue;
                            imageT6Array[icp6] = tValue;
                            cameraImageAcquisitionTime[icp6++] = acqTime;
                        }
                    }
                    
                    if ((zTileValue != Integer.MIN_VALUE) && (cValue != Integer.MIN_VALUE) &&
                        (tValue != Integer.MIN_VALUE) && (!Double.isNaN(relTime))) {
                        boolean doFill = true;
                        for (i = 0; i < icp7; i++) {
                            if ((imageZ7Array[i] == zTileValue) && (imageC7Array[i] == cValue) &&
                                (imageT7Array[i] == tValue)) {
                                doFill = false;
                            }
                        }
                        if (doFill) {
                            imageZ7Array[icp7] = zTileValue;
                            imageC7Array[icp7] = cValue;
                            imageT7Array[icp7] = tValue;
                            imageRelativeTime[icp7++] = relTime;
                        }
                    }
                    
                    break;
                } // while (true)
            } // if ((lastElementName.equals("Tags"))
            
            if ((directoryEntry % maximumDirectoryEntriesPerSector) == 0) {
                if (add128) {
                    directoryStart =  (directoryTable[++dp]+1)*sectorSize + 128;
                }
                else {
                    directoryStart =  (directoryTable[++dp]+1)*sectorSize;    
                }
            }
            
        } // while (true)
        
          return;

    }
    
    private int zviToMipavMeasurementUnits (int zviScalingUnit){
        int measureUnits;
        switch (zviScalingUnit) {
            case 0:
                Preferences.debug("Scaling unit type = no scaling\n", Preferences.DEBUG_FILEIO);
                measureUnits = Unit.UNKNOWN_MEASURE.getLegacyNum();
                break;
            case 72:
                Preferences.debug("Scaling unit type = meter\n", Preferences.DEBUG_FILEIO);
                measureUnits = Unit.METERS.getLegacyNum();
                break;
            case 76:
                Preferences.debug("Scaling unit type = micrometer\n", Preferences.DEBUG_FILEIO);
                measureUnits = Unit.MICROMETERS.getLegacyNum();
                break;
            case 77:
                Preferences.debug("Scaling unit type = nanometer\n", Preferences.DEBUG_FILEIO);
                measureUnits = Unit.NANOMETERS.getLegacyNum();
                break;
            case 81:
                 Preferences.debug("Scaling unit type = inch\n", Preferences.DEBUG_FILEIO);
                 measureUnits = Unit.INCHES.getLegacyNum();
                 break;
            case 84:
                Preferences.debug("Scaling unit type = mil (thousandth of an inch)\n", Preferences.DEBUG_FILEIO);
                measureUnits = Unit.MILS.getLegacyNum();
                break;
            case 136:
                Preferences.debug("Scaling unit type = second\n", Preferences.DEBUG_FILEIO);
                measureUnits = Unit.SECONDS.getLegacyNum();
                break;
            case 139:
                Preferences.debug("Scaling unit type = millisecond\n", Preferences.DEBUG_FILEIO);
                measureUnits = Unit.MILLISEC.getLegacyNum();
                break;
            case 140:
                Preferences.debug("Scaling unit type = microsecond\n", Preferences.DEBUG_FILEIO);
                measureUnits = Unit.MICROSEC.getLegacyNum();
                break;
            case 145:
                Preferences.debug("Scaling unit type = minute\n", Preferences.DEBUG_FILEIO);
                measureUnits = Unit.MINUTES.getLegacyNum();
                break;
            case 146:
                Preferences.debug("Scaling unit type = hour\n", Preferences.DEBUG_FILEIO);
                measureUnits = Unit.HOURS.getLegacyNum();
                break;
            default:
                Preferences.debug("Scaling unit type is an unrecognized " + zviScalingUnit + "\n", Preferences.DEBUG_FILEIO);
                measureUnits = Unit.UNKNOWN_MEASURE.getLegacyNum();     
        }
        return measureUnits;
    } // private int zviToMipavMeasurementUnits (int zviScalingUnit)
    
    
    private void displayPixelFormat(int pixelFormat) {
        switch(pixelFormat) {
            case 1:
                Preferences.debug("Pixel format = 8-bit B, G, R - 3 bytes/pixel\n", Preferences.DEBUG_FILEIO);
                break;
            case 2:
                Preferences.debug("Pixel format = 8-bit B, G, R, A - 4 bytes/pixel\n", Preferences.DEBUG_FILEIO);
                break;
            case 3:
                Preferences.debug("Pixel format = 8-bit grayscale\n", Preferences.DEBUG_FILEIO);
                break;
            case 4:
                Preferences.debug("Pixel format = 16-bit integer\n", Preferences.DEBUG_FILEIO);
                break;
            case 5:
                Preferences.debug("Pixel format = 32-bit integer\n", Preferences.DEBUG_FILEIO);
                break;
            case 6:
                Preferences.debug("Pixel format = 32-bit IEEE float\n", Preferences.DEBUG_FILEIO);
                break;
            case 7:
                Preferences.debug("Pixel format = 64-bit IEEE double\n", Preferences.DEBUG_FILEIO);
                break;
            case 8:
                Preferences.debug("Pixel format = 16-bit B, G, R - 6 bytes/pixel\n", Preferences.DEBUG_FILEIO);
                break;
            case 9:
                Preferences.debug("Pixel format = 32-bit B, G, R = 12 bytes/pixel\n", Preferences.DEBUG_FILEIO);
                break;
            default:
                Preferences.debug("pixelFormat has an unrecognized value = " + pixelFormat + "\n", Preferences.DEBUG_FILEIO);
        }    
    } // private void displayPixelFormat(int pixelFormat)
    
    String calculateTimeStampString(long timeStamp) {
        // The time stamp field is an unsigned 64-bit integer value that contains the time elapsed since
        // 1601-Jan-01 00:00:00 (Gregorian calendar).  One unit of this value is equal to 100 nanoseconds.
        // This means each second the time stamp value will be increased by 10 million units.
        // When calculating a date from a time stamp, the correct rules of leap year handling have to be
        // respected:
        // a year divisible by 4 is a leap year.
        // with the exception that a year divisible by 100 is not a leap year (e.g. 1900 was no leap year);
        // with the exception that a year divisible by 400 is a leap year (e.g. 2000 was a leap year).
        String timeStampString = null;
        // Calcuate fractional amount of a second
        String fractionalSecond = Double.toString((timeStamp % 10000000)/10000000);
        int index = fractionalSecond.indexOf(".");
        fractionalSecond = fractionalSecond.substring(index);
        long remainingEntireSeconds = timeStamp/10000000;
        String secondsInAMinute = Long.toString(remainingEntireSeconds % 60);
        if ((remainingEntireSeconds % 60) < 10) {
            secondsInAMinute = "0"+secondsInAMinute;
        }
        long remainingEntireMinutes = remainingEntireSeconds/60;
        String minutesInAnHour = Long.toString(remainingEntireMinutes % 60);
        if ((remainingEntireMinutes % 60) < 10) {
            minutesInAnHour = "0"+minutesInAnHour;
        }
        long remainingEntireHours = remainingEntireMinutes/60;
        String hoursInADay = Long.toString(remainingEntireHours % 24);
        long remainingEntireDays = remainingEntireHours/24;
        long currentYear = 1601;
        long daysInYear = 365;
        while (remainingEntireDays >= daysInYear) {
            remainingEntireDays -= daysInYear;
            currentYear++;
            if ((currentYear % 4) != 0) {
                daysInYear = 365;
            }
            else if (((currentYear % 100) == 0) && ((currentYear % 400) != 0)) {
                daysInYear = 365;
            }
            else {
                daysInYear = 366;
            }
        } // while (remainingEntireDays >= daysInYear)
        String yearString = Long.toString(currentYear);
        String monthString;
        String dayString;
        int addDay;
        if (daysInYear == 366) {
            addDay = 1;
        }
        else {
            addDay = 0;
        }
        if (remainingEntireDays < 31) {
            monthString = "January";
            dayString = Long.toString(remainingEntireDays);
        }
        else if (remainingEntireDays < 59 + addDay) {
            monthString = "February";
            dayString = Long.toString(remainingEntireDays - 31);
        }
        else if (remainingEntireDays < 90 + addDay) {
            monthString = "March";
            dayString = Long.toString(remainingEntireDays - (59 + addDay));
        }
        else if (remainingEntireDays < 120 + addDay) {
            monthString = "April";
            dayString = Long.toString(remainingEntireDays - (90 + addDay));
        }
        else if (remainingEntireDays < 151 + addDay) {
            monthString = "May";
            dayString = Long.toString(remainingEntireDays - (120 + addDay));
        }
        else if (remainingEntireDays < 181 + addDay) {
            monthString = "June";
            dayString = Long.toString(remainingEntireDays - (151 + addDay));
        }
        else if (remainingEntireDays < 212 + addDay) {
            monthString = "July";
            dayString = Long.toString(remainingEntireDays - (181 + addDay));
        }
        else if (remainingEntireDays < 243 + addDay) {
            monthString = "August";
            dayString = Long.toString(remainingEntireDays - (212 + addDay));
        }
        else if (remainingEntireDays < 273 + addDay) {
            monthString = "September";
            dayString = Long.toString(remainingEntireDays - (243 + addDay));
        }
        else if (remainingEntireDays < 304 + addDay) {
            monthString = "October";
            dayString = Long.toString(remainingEntireDays - (273 + addDay));
        }
        else if (remainingEntireDays < 334 + addDay) {
            monthString = "November";
            dayString = Long.toString(remainingEntireDays - (304 + addDay));
        }
        else {
            monthString = "December";
            dayString = Long.toString(remainingEntireDays - (334 + addDay));
        }
        timeStampString = monthString + " " + dayString + ", " + yearString + " " + 
                          hoursInADay + ":" + minutesInAnHour + ":" + secondsInAMinute +
                          fractionalSecond;
       return timeStampString;
    }
    
    private String calculateVTDateTimeString(double dateTime) {
        // The OLE automation date format is a floating point value, counting days since midnight 30 
        // 1899.  Hours and minutes are represented as fractional days.  So what is 1.75? That's
        // 6 PM, 31 dec 1899.
        String dateTimeString = null;
        long remainingEntireDays = (long)dateTime;
        double fractionalDays = dateTime - remainingEntireDays;
        double hours = (24 * fractionalDays);
        String hourString = Long.toString((long)hours);
        if (hours < 10.0) {
            hourString = "0" + hourString;
        }
        double minutes = 60.0 * (hours - (long)hours);
        String minuteString = Long.toString((long)minutes);
        if (minutes < 10.0) {
            minuteString = "0" + minuteString;
        }
        double seconds = 60.0 * (minutes - (long)minutes);
        String secondString = Double.toString(seconds);
        if (seconds < 10.0) {
            secondString = "0" + secondString;
        }
        remainingEntireDays -= 2;
        long currentYear = 1900;
        long daysInYear = 365;
        while (remainingEntireDays >= daysInYear) {
            remainingEntireDays -= daysInYear;
            currentYear++;
            if ((currentYear % 4) != 0) {
                daysInYear = 365;
            }
            else if (((currentYear % 100) == 0) && ((currentYear % 400) != 0)) {
                daysInYear = 365;
            }
            else {
                daysInYear = 366;
            }
        } // while (remainingEntireDays >= daysInYear)
        String yearString = Long.toString(currentYear);
        String monthString;
        String dayString;
        int addDay;
        if (daysInYear == 366) {
            addDay = 1;
        }
        else {
            addDay = 0;
        }
        if (remainingEntireDays < 31) {
            monthString = "January";
            dayString = Long.toString(remainingEntireDays);
        }
        else if (remainingEntireDays < 59 + addDay) {
            monthString = "February";
            dayString = Long.toString(remainingEntireDays - 31);
        }
        else if (remainingEntireDays < 90 + addDay) {
            monthString = "March";
            dayString = Long.toString(remainingEntireDays - (59 + addDay));
        }
        else if (remainingEntireDays < 120 + addDay) {
            monthString = "April";
            dayString = Long.toString(remainingEntireDays - (90 + addDay));
        }
        else if (remainingEntireDays < 151 + addDay) {
            monthString = "May";
            dayString = Long.toString(remainingEntireDays - (120 + addDay));
        }
        else if (remainingEntireDays < 181 + addDay) {
            monthString = "June";
            dayString = Long.toString(remainingEntireDays - (151 + addDay));
        }
        else if (remainingEntireDays < 212 + addDay) {
            monthString = "July";
            dayString = Long.toString(remainingEntireDays - (181 + addDay));
        }
        else if (remainingEntireDays < 243 + addDay) {
            monthString = "August";
            dayString = Long.toString(remainingEntireDays - (212 + addDay));
        }
        else if (remainingEntireDays < 273 + addDay) {
            monthString = "September";
            dayString = Long.toString(remainingEntireDays - (243 + addDay));
        }
        else if (remainingEntireDays < 304 + addDay) {
            monthString = "October";
            dayString = Long.toString(remainingEntireDays - (273 + addDay));
        }
        else if (remainingEntireDays < 334 + addDay) {
            monthString = "November";
            dayString = Long.toString(remainingEntireDays - (304 + addDay));
        }
        else {
            monthString = "December";
            dayString = Long.toString(remainingEntireDays - (334 + addDay));
        }
        dateTimeString = monthString + " " + dayString + ", " + yearString + " " + 
                          hourString + ":" + minuteString + ":" + secondString;
        return dateTimeString;
    }

    private String calculateRelativeTime(double relativeTime) {
        String timeString = null;
        double hours = (24 * relativeTime);
        String hourString = Long.toString((long)hours);
        if (hours < 10.0) {
            hourString = "0" + hourString;
        }
        double minutes = 60.0 * (hours - (long)hours);
        String minuteString = Long.toString((long)minutes);
        if (minutes < 10.0) {
            minuteString = "0" + minuteString;
        }
        double seconds = 60.0 * (minutes - (long)minutes);
        String secondString = Double.toString(seconds);
        if (seconds < 10.0) {
            secondString = "0" + secondString;
        }
        timeString = hourString + ":" + minuteString + ":" + secondString;
        return timeString;
    }
}
