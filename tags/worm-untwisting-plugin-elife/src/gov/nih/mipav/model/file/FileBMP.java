package gov.nih.mipav.model.file;


import gov.nih.mipav.model.file.FileInfoBase.Unit;
import gov.nih.mipav.model.structures.*;

import java.io.*;

import gov.nih.mipav.view.*;

/**
 
 */

public class FileBMP extends FileBase {
   
    // Compression types
	private static final int BI_RGB = 0; // uncompressed
    
    private static final int BI_RLE8 = 1;
    
    private static final int BI_RLE4 = 2;
    
    private static final int BI_BITFIELDS = 3;
    
    // color space types
    private static final long CALIBRATED_RGB = 0;
    
    private static final long DEVICE_DEPENDENT_RGB = 1;
    
    private static final long DEVICE_DEPENDENT_CMYK = 2;
    

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** DOCUMENT ME! */
    private File file;

    /** DOCUMENT ME! */
    private String fileDir;


    /** DOCUMENT ME! */
    private FileInfoBMP fileInfo;

    /** DOCUMENT ME! */
    private String fileName;

    /** DOCUMENT ME! */
    private ModelImage image;

    /** DOCUMENT ME! */
    private int[] imageExtents = null;

    /** DOCUMENT ME! */
    private Number[] imgBuffer = null;

    /** DOCUMENT ME! */
    private float[] imgResols = new float[2];
    
    private int[] unitsOfMeasure = new int[2];

    /** DOCUMENT ME! */
    private ModelLUT LUT = null;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * LIFF reader/writer constructor.
     *
     * @param      fileName  file name
     * @param      fileDir   file directory
     *
     * @exception  IOException  if there is an error making the file
     */
    public FileBMP(String fileName, String fileDir) throws IOException {

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
    public Number[] getImageBuffer() {
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
        long fileLength;
        boolean endianess;
        int i, j, k;
       
        int imageSlices = 0;
        // File header fields
        short bfType; // Characters "BM" that identify .BMP files
        long bfSize; // file size
        int bfReserved1; // Unused must be 0
        int bfReserved2; // Unused must be 0
        long bfOffBits; // Offset from the beginning of the file to where the pixel data starts
        
        // Image header fields
        long biSize; // Header size
        int biWidth; // image width
        int biHeight; // image length
        int biPlanes; // Number of color planes, must be 1
        int biBitCount; // Bits per pixel - 1, 4, 8, 16, 24, or 32
        long biCompression = BI_RGB; // Compression type - BI_RGB = 0 (uncompressed), BI_RLE8 = 1,
                                    //                    BI_RLE4 = 2, or BI_BITFIELDS = 3
        long sizeOfBitmap = 0; // Size of the stored bitmap in bytes.  This value is typically
                           // zero when the bitmap data is uncompressed
        int biXPelsPerMeter; // Horizontal pixels per meter
        int biYPelsPerMeter; // Vertical pixels per meter
        long colorsUsed; // Number of colors present in the palette.  If this value is zero, and
                        // the value of biBitCount is less then 16, the number number of entries is
                        // equal to the maximum size possible for the bitmap.  BMP files with a 
                        // biBitCount value of 16 or greater will not have a color palette.
        long colorsImportant; // Number of significant colors in the palette, determined by their
                             // frequency of appearance in the bitmap data.  colorsImportant is 0
                             // if all of the colors in the palette are considered to be important.
        int redMask = 0; // Mask identifying bits of red component
        int greenMask = 0; // Mask identifying bits of green component
        int blueMask = 0; // Mask identifying bits of blue component
        int alphaMask = 0; // Mask identifying bits of alpha component
        long colorSpaceType; // colorSpaceType is used by the bitmap data.  DEVICE_DEPENDENT_RGB is
                             // the default color space.  CALIBRATED_RGB is defined by the 1931
                             // CIE XYZ standard.
        int redX; // X coordinate of red endpoint, used only when colorSpaceType = CALIBRATED_RGB
        int redY; // Y coordinate of red endpoint, used only when colorSpaceType = CALIBRATED_RGB
        int redZ; // Z coordinate of red endpoint, used only when colorSpaceType = CALIBRATED_RGB
        int greenX; // X coordinate of green endpoint, used only when colorSpaceType = CALIBRATED_RGB
        int greenY; // Y coordinate of green endpoint, used only when colorSpaceType = CALIBRATED_RGB
        int greenZ; // Z coordinate of green endpoint, used only when colorSpaceType = CALIBRATED_RGB
        int blueX; // X coordinate of blue endpoint, used only when colorSpaceType = CALIBRATED_RGB 
        int blueY; // Y coordinate of blue endpoint, used only when colorSpaceType = CALIBRATED_RGB
        int blueZ; // Z coordinate of blue endpoint, used only when colorSpaceType = CALIBRATED_RGB
        long gammaRed; // gamma red coordinate scale value
        long gammaGreen; // gamma green coordinate scale value
        long gammaBlue; // gamma blue coordinate scale value
        
        int sliceSize;
        boolean bottomUp;
        int bytesPerRow;
        int dimExtentsLUT[];
        int blueColor;
        int greenColor;
        int redColor;
        byte reserved;
        long presentLocation;
        int maximumColorsUsed;
        int imageType;
        int test;
        int redStartBit = -1;
        int redEndBit = -1;
        int numberRedBits = 0;
        int greenStartBit = -1;
        int greenEndBit = -1;
        int numberGreenBits = 0;
        int blueStartBit = -1;
        int blueEndBit = -1;
        int numberBlueBits = 0;
        int alphaStartBit = -1;
        int alphaEndBit = -1;
        int numberAlphaBits = 0;
        int bufferSize;
        byte byteBuffer[];
        boolean booleanBuffer[];
        int x;
        int y;
        int jstart;
        int index;
        byte byteBuffer2[];
        short shortColor;
        int intColor;
        int m;
        byte firstVal;
        byte secondVal;
        int usedBytes;
        int mod2;

        try {
            
            imgResols[0] = imgResols[1] = (float) 1.0;
            file = new File(fileDir + fileName);
            raFile = new RandomAccessFile(file, "r");
            
            fileLength = raFile.length();
            Preferences.debug("raFile.length = " + raFile.length() + "\n", Preferences.DEBUG_FILEIO);
            
            fileInfo = new FileInfoBMP(fileName, fileDir, FileUtility.BMP); // dummy fileInfo
            endianess = FileBase.LITTLE_ENDIAN;
            fileInfo.setEndianess(FileBase.LITTLE_ENDIAN);
            
            // Read the 14 byte BITMAPFILEHEADER structure
            bfType = readShort(endianess); // The first 2 bytes of the file must contain the characters
                                           // B followed by M.
            if (bfType != 0x4D42) {
            	raFile.close();
                throw new IOException("BMP Read Header: Error - first 2 bytes are an illegal " + bfType);
            }
            else {
            	Preferences.debug("BMP file has first 2 bytes = BM as expected\n", Preferences.DEBUG_FILEIO);
            }
            bfSize = getUInt(endianess); // Read the file size in bytes
            if (bfSize == fileLength) {
            	Preferences.debug("bfSize = raFile.length() = " + bfSize + " as expected\n",
            			Preferences.DEBUG_FILEIO);
            }
            else {
                Preferences.debug("bfSize unexpectedly =" + bfSize + " since raFile.length() = " 
                		+ raFile.length() + "\n", Preferences.DEBUG_FILEIO);	
            }
            bfReserved1 = getUnsignedShort(endianess);
            if (bfReserved1 == 0) {
            	Preferences.debug("bfReserved1 = 0 as required\n", Preferences.DEBUG_FILEIO);
            }
            else {
            	Preferences.debug("bfReserved1 = " + bfReserved1 + " instead of the required 0\n",
            			          Preferences.DEBUG_FILEIO); 
            }
            bfReserved2 = getUnsignedShort(endianess);
            if (bfReserved2 == 0) {
            	Preferences.debug("bfReserved2 = 0 as required\n", Preferences.DEBUG_FILEIO);
            }
            else {
            	Preferences.debug("bfReserved2 = " + bfReserved2 + " instead of the required 0\n",
            			          Preferences.DEBUG_FILEIO); 
            }
            bfOffBits = getUInt(endianess); // Offset from the beginning of the file to where the 
                                            // pixel data starts
            Preferences.debug("bfOffBits, the offset from the beginning of the file to where the pixel data starts = " +
            		bfOffBits + "\n", Preferences.DEBUG_FILEIO);
            
            // Read the image header, either a 12 byte OS2 BITMAPCOREHEADER structure or more commonly a
            // 40 or more byte Windows BITMAPINFOHEADER structure
            biSize = getUInt(endianess); // Header Size
            Preferences.debug("biSize, header byte size = " + biSize + "\n", Preferences.DEBUG_FILEIO);
            imageExtents = new int[2];
            if (biSize >= 40) {
            	// Windows image header
                biWidth = readInt(endianess); // image width
            }
            else {
            	// OS2 image header
            	biWidth = (int)readShort(endianess);
            }
            Preferences.debug("biWidth, image width = " + biWidth + "\n", Preferences.DEBUG_FILEIO);
            imageExtents[0] = biWidth;
            if (biSize >= 40) {
            	// Windows image header
                biHeight = readInt(endianess);  // If positive, read in the default bottom up manner
                                                // If negative, read from the top down
            }
            else {
            	// OS2 image header
            	biHeight = (int)readShort(endianess);
            }
            Preferences.debug("biHieght, image height " + biHeight + "\n", Preferences.DEBUG_FILEIO);
            if (biHeight > 0) {
            	bottomUp = true;
            	Preferences.debug("Since biHeight positive do normal bottom up read\n",
            			Preferences.DEBUG_FILEIO);
            }
            else {
            	bottomUp = false;
            	Preferences.debug("Since biHeight negative do top down read\n", Preferences.DEBUG_FILEIO);
            }
            imageExtents[1] = Math.abs(biHeight);
            fileInfo.setExtents(imageExtents);
            sliceSize = imageExtents[0]*imageExtents[1];
            biPlanes = getUnsignedShort(endianess); // Number of color planes, must be 1
            if (biPlanes == 1) {
            	Preferences.debug("biPlanes, the number of color planes = the required 1\n",
            			Preferences.DEBUG_FILEIO);
            }
            else {
            	Preferences.debug("biPlanes, the number of color planes = " + biPlanes +
            			" instead of the required 1\n", Preferences.DEBUG_FILEIO);
            }
            biBitCount = getUnsignedShort(endianess); // Bits per pixel - 1, 4, 8, 16, 24, or 32
            if ((biBitCount != 1) && (biBitCount != 4) && (biBitCount != 8) && (biBitCount != 16) &&
                (biBitCount != 24) && (biBitCount != 32)) {
            	Preferences.debug("biBitCount, bits per pixel = an illegal " + biBitCount + "\n",
            			Preferences.DEBUG_FILEIO);
            	raFile.close();
                throw new IOException("biBitCount, bits per pixel = an illegal " + biBitCount);
            }
            else {
            	Preferences.debug("biBitCount, bits per pixel = " + biBitCount + "\n",
            			Preferences.DEBUG_FILEIO);
            }
            
            // The number of bytes per row is rounded up to a multiple of 4
            bytesPerRow = (imageExtents[0] * biBitCount)/8;
            if (((imageExtents[0] * biBitCount) % 8) != 0) {
            	bytesPerRow++;
            }
            if ((bytesPerRow % 4) != 0) {
            	bytesPerRow = bytesPerRow + (4 - (bytesPerRow % 4));
            }
            bufferSize = bytesPerRow * imageExtents[1];
            if (biSize >= 40) {
                biCompression = getUInt(endianess);
                if ((biCompression != BI_RGB) && (biCompression != BI_RLE8) && (biCompression != BI_RLE4) &&
                    (biCompression != BI_BITFIELDS)) {
                	Preferences.debug("biCompression, compression type = an illegal " + biCompression + "\n",
                			Preferences.DEBUG_FILEIO);
                	raFile.close();
                	throw new IOException("biCompression, compression type = an illegal " + biCompression);
                }
                else if (biCompression == BI_RGB) {
                	Preferences.debug("Compression type = BI_RGB, uncompressed\n", Preferences.DEBUG_FILEIO);
                }
                else if (biCompression == BI_RLE8) {
                	Preferences.debug("Compression type = BI_RLE8\n", Preferences.DEBUG_FILEIO);
                }
                else if (biCompression == BI_RLE4) {
                	Preferences.debug("Compression type = BI_RLE4\n", Preferences.DEBUG_FILEIO);
                }
                else if (biCompression == BI_BITFIELDS) {
                	Preferences.debug("Compression type = BI_BITFIELDS\n", Preferences.DEBUG_FILEIO);
                }
                sizeOfBitmap = getUInt(endianess); // Size of the stored bitmap in bytes
                                                   // The value is typically zero when the bitmap data is uncompressed
                if (sizeOfBitmap != 0) {
                	Preferences.debug("Size of stord bitmap in bytes = " + sizeOfBitmap + "\n",
                			Preferences.DEBUG_FILEIO);
                }
                else {
                    Preferences.debug("Size of stored bitmap in bytes left zero for uncompressed data\n",
                    		Preferences.DEBUG_FILEIO);	
                }
                biXPelsPerMeter = readInt(endianess); // Horizontal pixels per meter
                Preferences.debug("Horizontal pixels per meter = " + biXPelsPerMeter + "\n",
                		Preferences.DEBUG_FILEIO);
                if (biXPelsPerMeter != 0) {
                	imgResols[0] = (float)(1.0/biXPelsPerMeter);
                }
                biYPelsPerMeter = readInt(endianess); // Vertical pixels per meter
                Preferences.debug("Vertical pixels per meter = " + biYPelsPerMeter + "\n",
                		Preferences.DEBUG_FILEIO);
                if (biYPelsPerMeter != 0) {
                	imgResols[1] = (float)(1.0/biYPelsPerMeter);
                }
                fileInfo.setResolutions(imgResols);
                unitsOfMeasure[0] = Unit.METERS.getLegacyNum();
                unitsOfMeasure[1] = Unit.METERS.getLegacyNum();
                fileInfo.setUnitsOfMeasure(unitsOfMeasure);
                colorsUsed = getUInt(endianess); // Number of colors present in the palette.  If this value is zero, and
                                                 // the value of biBitCount is less then 16, the number number of entries is
                                                 // equal to the maximum size possible for the bitmap. BMP files with a 
                                                 // biBitCount value of 16 or greater will not have a color palette.
                if (colorsUsed != 0) {
                	Preferences.debug("Colors present in the palette = " + colorsUsed + "\n", 
                			Preferences.DEBUG_FILEIO);
                }
                else if (biBitCount == 1) {
                	colorsUsed = 2;
                	Preferences.debug("Colors present in the palette = 2\n", Preferences.DEBUG_FILEIO);
                }
                else if (biBitCount == 4) {
                	colorsUsed = 16;
                	Preferences.debug("Colors present in the palette = 16\n", Preferences.DEBUG_FILEIO);
                }
                else if (biBitCount == 8) {
                	colorsUsed = 256;
                	Preferences.debug("Colors present in the palette = 256\n", Preferences.DEBUG_FILEIO);
                }
                else {
                	Preferences.debug("No color palette is present for bits per pixel >= 16\n",
                			Preferences.DEBUG_FILEIO);
                }
                colorsImportant = getUInt(endianess); // Number of significant colors in the palette, determined by their
                                                      // frequency of appearance in the bitmap data.  colorsImportant is 0
                                                      // if all of the colors in the palette are considered to be important.
                if (colorsUsed > 0) {
                	if (colorsImportant == 0) {
                		Preferences.debug("All of the colors in the palette are important\n",
                				Preferences.DEBUG_FILEIO);
                	}
                	else {
                		Preferences.debug(colorsImportant + " of the colors in the palette are important\n",
                				Preferences.DEBUG_FILEIO);
                	}
                }
                if (biSize >= 108) {
                    redMask = readInt(endianess); // Mask identifying bits of red component	
                    Preferences.debug("redMask = " + Integer.toHexString(redMask) + "\n", Preferences.DEBUG_FILEIO);
                    greenMask = readInt(endianess); // Mask identifying bits of green component
                    Preferences.debug("greenMask = " + Integer.toHexString(greenMask) + "\n", Preferences.DEBUG_FILEIO);
                    blueMask = readInt(endianess); // Mask identifying bits of blue component
                    Preferences.debug("blueMask = " + Integer.toHexString(blueMask) + "\n", Preferences.DEBUG_FILEIO);
                    alphaMask = readInt(endianess); // Mask identifying bits of alpha component
                    Preferences.debug("alphaMask = " + Integer.toHexString(alphaMask) + "\n", Preferences.DEBUG_FILEIO);
                    colorSpaceType = getUInt(endianess); 
                    if (colorSpaceType == CALIBRATED_RGB) {
                    	Preferences.debug("Color space type = CALIBRATED_RGB\n", Preferences.DEBUG_FILEIO);
                    }
                    else if (colorSpaceType == DEVICE_DEPENDENT_RGB) {
                    	Preferences.debug("Color space type = DEVICE_DEPENDENT_RGB\n", Preferences.DEBUG_FILEIO);
                    }
                    else if (colorSpaceType == DEVICE_DEPENDENT_CMYK) {
                    	Preferences.debug("Color space type = DEVICE_DEPENDENT_CMYK\n", Preferences.DEBUG_FILEIO);
                    }
                    else {
                    	Preferences.debug("Color space type = an illegal " + colorSpaceType + "\n",
                    			Preferences.DEBUG_FILEIO);
                    }
                    redX = readInt(endianess); // X coordinate of red endpoint
                                               // Only used when colorSpaceType = CALIBRATED_RGB
                    if (colorSpaceType == CALIBRATED_RGB) {
                    	Preferences.debug("X coordinate of red endpoint = " + redX + "\n", Preferences.DEBUG_FILEIO);
                    }
                    redY = readInt(endianess); // Y coordinate of red endpoint
                                               // Only used when colorSpaceType = CALIBRATED_RGB
                    if (colorSpaceType == CALIBRATED_RGB) {
                        Preferences.debug("Y coordinate of red endpoint = " + redY + "\n", Preferences.DEBUG_FILEIO);
                    }
                    redZ = readInt(endianess); // Z coordinate of red endpoint
                                               // Only used when colorSpaceType = CALIBRATED_RGB
                    if (colorSpaceType == CALIBRATED_RGB) {
                        Preferences.debug("Z coordinate of red endpoint = " + redZ + "\n", Preferences.DEBUG_FILEIO);
                    }
                    greenX = readInt(endianess); // X coordinate of green endpoint
                                                 // Only used when colorSpaceType = CALIBRATED_RGB
					if (colorSpaceType == CALIBRATED_RGB) {
					    Preferences.debug("X coordinate of green endpoint = " + greenX + "\n", Preferences.DEBUG_FILEIO);
					}
					greenY = readInt(endianess); // Y coordinate of green endpoint
					                             // Only used when colorSpaceType = CALIBRATED_RGB
					if (colorSpaceType == CALIBRATED_RGB) {
					    Preferences.debug("Y coordinate of green endpoint = " + greenY + "\n", Preferences.DEBUG_FILEIO);
					}
					greenZ = readInt(endianess); // Z coordinate of green endpoint
					                             // Only used when colorSpaceType = CALIBRATED_RGB
					if (colorSpaceType == CALIBRATED_RGB) {
					    Preferences.debug("Z coordinate of green endpoint = " + greenZ + "\n", Preferences.DEBUG_FILEIO);
					}
					blueX = readInt(endianess); // X coordinate of blue endpoint
                                                // Only used when colorSpaceType = CALIBRATED_RGB
					if (colorSpaceType == CALIBRATED_RGB) {
					    Preferences.debug("X coordinate of blue endpoint = " + blueX + "\n", Preferences.DEBUG_FILEIO);
					}
					blueY = readInt(endianess); // Y coordinate of blue endpoint
					                            // Only used when colorSpaceType = CALIBRATED_RGB
					if (colorSpaceType == CALIBRATED_RGB) {
					    Preferences.debug("Y coordinate of blue endpoint = " + blueY + "\n", Preferences.DEBUG_FILEIO);
					}
					blueZ = readInt(endianess); // Z coordinate of blue endpoint
					                            // Only used when colorSpaceType = CALIBRATED_RGB
					if (colorSpaceType == CALIBRATED_RGB) {
					    Preferences.debug("Z coordinate of blue endpoint = " + blueZ + "\n", Preferences.DEBUG_FILEIO);
					}
					gammaRed = getUInt(endianess); // Gamma red coordinate scale value
					Preferences.debug("Gamma red coordinate scale value = " + gammaRed + "\n", 
							Preferences.DEBUG_FILEIO);
					gammaGreen = getUInt(endianess); // Gamma green coordinate scale value
					Preferences.debug("Gamma green coordinate scale value = " + gammaGreen + "\n", 
							Preferences.DEBUG_FILEIO);
					gammaBlue = getUInt(endianess); // Gamma blue coordinate scale value
					Preferences.debug("Gamma blue coordinate scale value = " + gammaBlue + "\n", 
							Preferences.DEBUG_FILEIO);
                } // if (biSize >= 108)
                else if (((biBitCount == 16) || (biBitCount == 32)) && (biCompression == BI_BITFIELDS)) {
                	redMask = readInt(endianess); // Mask identifying bits of red component	
                    Preferences.debug("redMask = " + Integer.toHexString(redMask) + "\n", Preferences.DEBUG_FILEIO);
                    greenMask = readInt(endianess); // Mask identifying bits of green component
                    Preferences.debug("greenMask = " + Integer.toHexString(greenMask) + "\n", Preferences.DEBUG_FILEIO);
                    blueMask = readInt(endianess); // Mask identifying bits of blue component
                    Preferences.debug("blueMask = " + Integer.toHexString(blueMask) + "\n", Preferences.DEBUG_FILEIO);
                } // else if (((biBitCount == 16) || (biBitCount == 32)) && (biCompression == BI_BITFIELDS))
            } // if (biSize >= 40)
            else { // biSize == 12 OS2
            	presentLocation = raFile.getFilePointer();
            	maximumColorsUsed = (int)((bfOffBits - presentLocation)/3);
            	if (biBitCount == 1) {
                	colorsUsed = Math.min(2,maximumColorsUsed);
                	Preferences.debug("Colors present in the palette = 2\n", Preferences.DEBUG_FILEIO);
                }
                else if (biBitCount == 4) {
                	colorsUsed = Math.min(16,maximumColorsUsed);
                	Preferences.debug("Colors present in the palette = 16\n", Preferences.DEBUG_FILEIO);
                }
                else if (biBitCount == 8) {
                	colorsUsed = Math.min(256,maximumColorsUsed);
                	Preferences.debug("Colors present in the palette = 256\n", Preferences.DEBUG_FILEIO);
                }
                else {
                	colorsUsed = 0;
                	Preferences.debug("No color palette is present for bits per pixel >= 16\n",
                			Preferences.DEBUG_FILEIO);
                }	
            } // biSize == 12 OS2
            
            if ((redMask != 0) || (greenMask != 0) || (blueMask != 0) || (alphaMask != 0)) {
                if (redMask != 0) {
                	test = 1;
                	redStartBit = 0;
                	redEndBit = 0;
                	while (((test & redMask) == 0) && (test != 0)) {
                		test = test << 1;
                		redStartBit++;
                	}
                	redEndBit = redStartBit-1;
                	while (((test & redMask) != 0) && (test != 0)) {
                	   	test = test << 1;
                	   	redEndBit++;
                	}
                    numberRedBits = redEndBit - redStartBit + 1;
                } // if (redMask != 0)
                if (greenMask != 0) {
                	test = 1;
                	greenStartBit = 0;
                	greenEndBit = 0;
                	while (((test & greenMask) == 0) && (test != 0)) {
                		test = test << 1;
                		greenStartBit++;
                	}
                	greenEndBit = greenStartBit-1;
                	while (((test & greenMask) != 0) && (test != 0)) {
                	   	test = test << 1;
                	   	greenEndBit++;
                	}
                    numberGreenBits = greenEndBit - greenStartBit + 1;
                } // if (greenMask != 0)
                if (blueMask != 0) {
                	test = 1;
                	blueStartBit = 0;
                	blueEndBit = 0;
                	while (((test & blueMask) == 0) && (test != 0)) {
                		test = test << 1;
                		blueStartBit++;
                	}
                	blueEndBit = blueStartBit-1;
                	while (((test & blueMask) != 0) && (test != 0)) {
                	   	test = test << 1;
                	   	blueEndBit++;
                	}
                    numberBlueBits = blueEndBit - blueStartBit + 1;
                } // if (blueMask != 0)
                if (alphaMask != 0) {
                	test = 1;
                	alphaStartBit = 0;
                	alphaEndBit = 0;
                	while (((test & alphaMask) == 0) && (test != 0)) {
                		test = test << 1;
                		alphaStartBit++;
                	}
                	alphaEndBit = alphaStartBit-1;
                	while (((test & alphaMask) != 0) && (test != 0)) {
                	   	test = test << 1;
                	   	alphaEndBit++;
                	}
                    numberAlphaBits = alphaEndBit - alphaStartBit + 1;
                } // if (alphaMask != 0)
            } // if ((redMask != 0) || (greenMask != 0) || (blueMask != 0) || (alphaMask != 0))
            
            if (biBitCount == 1) {
            	imageType = ModelStorageBase.BOOLEAN;
            	Preferences.debug("imageType = BOOLEAN\n", Preferences.DEBUG_FILEIO);
            }
            else if ((biBitCount == 4) || (biBitCount == 8)) {
            	imageType = ModelStorageBase.UBYTE;
            	Preferences.debug("imageType = UBYTE\n", Preferences.DEBUG_FILEIO);
            }
            else if ((biBitCount == 16) || (biBitCount == 24)){
            	imageType = ModelStorageBase.ARGB;
            	Preferences.debug("imageType = ARGB\n", Preferences.DEBUG_FILEIO);
            }
            // biBitCount == 32
            else if ((numberRedBits <= 8) && (numberGreenBits <= 8) && (numberBlueBits <= 8) && (numberAlphaBits <= 8)) {
            	imageType = ModelStorageBase.ARGB;
            	Preferences.debug("imageType = ARGB\n", Preferences.DEBUG_FILEIO);
            }
            else {
            	imageType = ModelStorageBase.ARGB_USHORT;
            	Preferences.debug("imageType = ARGB_USHORT\n", Preferences.DEBUG_FILEIO);
            }
            fileInfo.setDataType(imageType);
            image = new ModelImage(imageType, imageExtents, fileName);
            image.setFileInfo(fileInfo, 0);
            //Read the color palette
            if (colorsUsed > 0) {
            	 // read the color table into a LUT
                dimExtentsLUT = new int[2];
                dimExtentsLUT[0] = 4;
                dimExtentsLUT[1] = 256;
                LUT = new ModelLUT(ModelLUT.GRAY, (int)colorsUsed, dimExtentsLUT);
                if (biSize == 12) {
                	for (j = 0; j < colorsUsed; j++) {
                		blueColor = raFile.readUnsignedByte();
                		greenColor = raFile.readUnsignedByte();
                		redColor = raFile.readUnsignedByte();
                        LUT.setColor(j, 1, redColor, greenColor, blueColor);
                    } // for (j = 0; j < colorsUsed; j++)      
                } // if (biSize == 12)
                else {
                	for (j = 0; j < colorsUsed; j++) {
	                	blueColor = raFile.readUnsignedByte();
	            		greenColor = raFile.readUnsignedByte();
	            		redColor = raFile.readUnsignedByte();
	            		reserved = raFile.readByte();
                        LUT.setColor(j, 1, redColor, greenColor, blueColor);
                	} // for (j = 0; j < colorsUsed; j++) 
                }
                for (j = (int)colorsUsed; j < 256; j++) {
                    LUT.setColor(j, 1, 0, 0, 0);    
                } // for (j = colorsUsed; j < 256; j++)
                LUT.makeIndexedLUT(null); 
            } // if (colorsUsed > 0)
            
            raFile.seek(bfOffBits);
            if (biBitCount == 1) {
            	// The most significant bitfield represents the leftmost pixel
            	booleanBuffer = new boolean[sliceSize];
            	byteBuffer = new byte[bufferSize];
            	raFile.read(byteBuffer);
            	if (bottomUp) {
	            	for (j = 8*bytesPerRow*(imageExtents[1]-1), jstart = 8*bytesPerRow*(imageExtents[1]-1),
	            			y = 0; y < imageExtents[1]; y++, j = jstart - 8*bytesPerRow) {
	                    for (x = 0, jstart = j, k = 0x80; x < imageExtents[0]; x++) {
	                        index = x + y*imageExtents[0];
	                        
	                        if ((byteBuffer[(j++) >>> 3] & k) != 0) {
	                            booleanBuffer[index] = true;
	                        }
	                        else {
	                            booleanBuffer[index] = false;
	                        }
	                        if (k == 0x01) {
	                            k = 0x80;
	                        }
	                        else {
	                            k = k >>> 1;
	                        }
	                    }
	                }
            	} // if (bottomUp)
            	else { // topDown
            		for (j = 0, jstart = 0, y = 0; y < imageExtents[1]; y++, j = jstart + 8*bytesPerRow) {
	                    for (x = 0, jstart = j, k = 0x80; x < imageExtents[0]; x++) {
	                        index = x + y*imageExtents[0];
	                        
	                        if ((byteBuffer[(j++) >>> 3] & k) != 0) {
	                            booleanBuffer[index] = true;
	                        }
	                        else {
	                            booleanBuffer[index] = false;
	                        }
	                        if (k == 0x01) {
	                            k = 0x80;
	                        }
	                        else {
	                            k = k >>> 1;
	                        }
	                    }
	                }	
            	} // topDown
                image.importData(0, booleanBuffer, true);
            } // if (biBitCount == 1)
            else if ((biBitCount == 4)  && (biCompression == BI_RGB)) {
            	// The most significant bitfield represents the leftmost pixel
            	byteBuffer2 = new byte[sliceSize];
                byteBuffer = new byte[bufferSize];
                raFile.read(byteBuffer);
                if (bottomUp) {
                	for (j = 2*bytesPerRow*(imageExtents[1]-1), jstart = 2*bytesPerRow*(imageExtents[1]-1),
                			y = 0; y < imageExtents[1]; y++, j = jstart - 2*bytesPerRow) {
                        for (x = 0, jstart = j, k = 0; x < imageExtents[0]; x++) {
                            index = x + y*imageExtents[0];
                            if (k == 0) {
                                byteBuffer2[index] = (byte)((byteBuffer[(j++) >> 1] & 0xf0) >>> 4);
                                k = 1;
                            }
                            else if (k == 1) {
                                byteBuffer2[index] = (byte)(byteBuffer[(j++) >> 1] & 0x0f);  
                                k = 0;
                            }
                        }
                    }	
                } // if (bottomUp)
                else { // topDown
                	for (j = 0, jstart = 0,y = 0; y < imageExtents[1]; y++, j = jstart + 2*bytesPerRow) {
                        for (x = 0, jstart = j, k = 0; x < imageExtents[0]; x++) {
                            index = x + y*imageExtents[0];
                            if (k == 0) {
                                byteBuffer2[index] = (byte)((byteBuffer[(j++) >> 1] & 0xf0) >>> 4);
                                k = 1;
                            }
                            else if (k == 1) {
                                byteBuffer2[index] = (byte)(byteBuffer[(j++) >> 1] & 0x0f);  
                                k = 0;
                            }
                        }
                    }
                } // else topDown
                image.importData(0, byteBuffer2, true);
            } // else if ((biBitCount == 4)  && (biCompression == BI_RGB))
            else if ((biBitCount == 4)&& (biCompression == BI_RLE4)) {
            	byteBuffer2 = new byte[sliceSize];
                byteBuffer = new byte[(int)sizeOfBitmap];	
                raFile.read(byteBuffer);
                if (bottomUp) {
                	x = 0;
                	y = imageExtents[1] - 1;
                    for (j = 0; j < sizeOfBitmap;) {

                        if (byteBuffer[j] != 0) {
                        	firstVal = (byte)((byteBuffer[j+1] & 0x000000f0) >>> 4);
                        	secondVal = (byte)(byteBuffer[j+1] & 0x0000000f);
                            for (k = 0, m = 0; k < (byteBuffer[j] & 0x000000ff); k++) {
                            	
                            	if (m == 0) {
                            		if (x < imageExtents[0]) {
                                        byteBuffer2[x + (imageExtents[0] * y)] = firstVal;
                            		}
                                    m = 1;
                            	}
                            	else {
                            		if (x < imageExtents[0]) {
                            		    byteBuffer2[x + (imageExtents[0] * y)] = secondVal;
                            		}
                            		m = 0;
                            	}
                                x++;

                                if ((x == (imageExtents[0] + (imageExtents[0] % 2))) && (y > 0)) {
                                    x = 0;
                                    y--;
                                } // if ((x == (imageExtents[0] + (imageExtents[0] % 2))) && (y > 0))
                                else if ((x == (imageExtents[0] + (imageExtents[0] % 2))) && (y == 0)) {
                                    x = 0;
                                    y = imageExtents[1] - 1;
                                } // else if ((x == (imageExtents[0] + (imageExtents[0] % 2))) && (y == 0))
                            } // for (k = 0; k < (fileBuffer[j] & 0x000000ff); k++)
                           
                            j = j + 2;
                        } // if (byteBuffer[j] != 0)
                        else if ((byteBuffer[j] == 0) && ((byteBuffer[j + 1] & 0x000000ff) > 2)) {

                            for (k = 0, m = 0; k < (byteBuffer[j + 1] & 0x000000ff); k++) {
                            	if (m == 0) {
                            		if (x < imageExtents[0]) {
                                        byteBuffer2[x + (imageExtents[0] * y)] = 
                                    	    (byte)((byteBuffer[j + k/2 + 2] & 0x000000f0) >>> 4);
                            		}
                                    m = 1;
                            	}
                            	else {
                            		if (x < imageExtents[0]) {
                            		    byteBuffer2[x + (imageExtents[0] * y)] = 
                                    	    (byte)(byteBuffer[j + k/2 + 2] & 0x0000000f);
                            		}
                                    m = 0;	
                            	}
                                x++;

                                if ((x == (imageExtents[0] + (imageExtents[0] % 2))) && (y > 0)) {
                                    x = 0;
                                    y--;
                                } // if ((x == (imageExtents[0] + (imageExtents[0] % 2))) && (y > 0))
                                else if ((x == (imageExtents[0] + (imageExtents[0] % 2))) && (y == 0)) {
                                    x = 0;
                                    y = imageExtents[1] - 1;
                                } // else if ((x == (imageExtents[0] + (imageExtents[0] % 2))) && (y == 0))
                            } // for (k = 0; k < (fileBuffer[j+1] & 0x000000ff); k++)
                            usedBytes = (byteBuffer[j+1] & 0x000000ff)/2 + ((byteBuffer[j+1] & 0x000000ff) % 2);
                            mod2 = usedBytes % 2;
                            
                            if (mod2 == 1) {
                               j = j + 3 + usedBytes;
                            }
                            else {
                            	j = j + 2 + usedBytes;
                            }
                            
	                    } // else if ((byteBuffer[j] == 0) && ((byteBuffer[j+1] & 0x000000ff) > 2))
	                    else if ((byteBuffer[j] == 0) && (byteBuffer[j + 1] == 2)) {
	                        x = x + (byteBuffer[j + 2] & 0x000000ff);
	                        y = y - (byteBuffer[j + 3] & 0x000000ff);
	                        j = j + 4;
	                    } // else if ((byteBuffer[j] == 0) && (byteBuffer[j+1] == 2))
	                    else if ((byteBuffer[j] == 0) && (byteBuffer[j + 1] == 0)) {
	
	                        // end of a line
	                        if (x != 0) {
	                            x = 0;
	
	                            if (y > 0) {
	                                y = y - 1;
	                            } else {
	                                y = imageExtents[1] - 1;
	                            }
	                        } // if (x != 0)
	
	                        j = j + 2;
	                    } // else if ((byteBuffer[j] == 0) && (byteBuffer[j+1] == 0))
	                    else if ((byteBuffer[j] == 0) && (byteBuffer[j + 1] == 1)) {
	
	                        // end of RLE bitmap
	                    	image.importData(0, byteBuffer2, true);
	                    	j = j + 2;
	                        
	                    } // else if ((fileBuffer[j] == 0) && (fileBuffer[j+1] == 1))
                    } // for (j = 0; j < sizeOfBitmap;)  
                } // if (bottomUp)
                else { // topDown
                	x = 0;
                	y = 0;
                    for (j = 0; j < sizeOfBitmap;) {
                    	if (byteBuffer[j] != 0) {
                        	firstVal = (byte)((byteBuffer[j+1] & 0x000000f0) >>> 4);
                        	secondVal = (byte)(byteBuffer[j+1] & 0x0000000f);
                            for (k = 0, m = 0; k < (byteBuffer[j] & 0x000000ff); k++) {
                            	
                            	if (m == 0) {
                            		if (x < imageExtents[0]) {
                                        byteBuffer2[x + (imageExtents[0] * y)] = firstVal;
                            		}
                                    m = 1;
                            	}
                            	else {
                            		if (x < imageExtents[0]) {
                            		    byteBuffer2[x + (imageExtents[0] * y)] = secondVal;
                            		}
                            		m = 0;
                            	}
                                x++;

                                if ((x == (imageExtents[0] + (imageExtents[0] % 2))) && (y < imageExtents[1]-1)) {
                                    x = 0;
                                    y++;
                                } // if ((x == (imageExtents[0] + (imageExtents[0] % 2))) && (y < imageExtents[1]-1))
                                else if ((x == (imageExtents[0] + (imageExtents[0] % 2))) && (y == imageExtents[1]-1)) {
                                    x = 0;
                                    y = 0;
                                } // else if ((x == (imageExtents[0] + (imageExtents[0] % 2))) && (y == imageExtents[1]-1))
                            } // for (k = 0; k < (fileBuffer[j] & 0x000000ff); k++)
                           
                            j = j + 2;
                        } // if (byteBuffer[j] != 0)
                        else if ((byteBuffer[j] == 0) && ((byteBuffer[j + 1] & 0x000000ff) > 2)) {

                            for (k = 0, m = 0; k < (byteBuffer[j + 1] & 0x000000ff); k++) {
                            	if (m == 0) {
                            		if (x < imageExtents[0]) {
                                        byteBuffer2[x + (imageExtents[0] * y)] = 
                                    	    (byte)((byteBuffer[j + k/2 + 2] & 0x000000f0) >>> 4);
                            		}
                                    m = 1;
                            	}
                            	else {
                            		if (x < imageExtents[0]) {
                            		    byteBuffer2[x + (imageExtents[0] * y)] = 
                                    	    (byte)(byteBuffer[j + k/2 + 2] & 0x0000000f);
                            		}
                                    m = 0;	
                            	}
                                x++;

                                if ((x == (imageExtents[0] + (imageExtents[0] % 2))) && (y < imageExtents[1]-1)) {
                                    x = 0;
                                    y++;
                                } // if ((x == (imageExtents[0] + (imageExtents[0] % 2))) && (y < imageExtents[1]-1))
                                else if ((x == (imageExtents[0] + (imageExtents[0] % 2))) && (y == imageExtents[1]-1)) {
                                    x = 0;
                                    y = 0;
                                } // else if ((x == (imageExtents[0] + (imageExtents[0] % 2))) && (y == imageExtents[1]-1))
                            } // for (k = 0; k < (fileBuffer[j+1] & 0x000000ff); k++)
                            usedBytes = (byteBuffer[j+1] & 0x000000ff)/2 + ((byteBuffer[j+1] & 0x000000ff) % 2);
                            mod2 = usedBytes % 2;
                            
                            if (mod2 == 1) {
                               j = j + 3 + usedBytes;
                            }
                            else {
                            	j = j + 2 + usedBytes;
                            }
                            
	                    } // else if ((byteBuffer[j] == 0) && ((byteBuffer[j+1] & 0x000000ff) > 2))
	                    else if ((byteBuffer[j] == 0) && (byteBuffer[j + 1] == 2)) {
	                        x = x + (byteBuffer[j + 2] & 0x000000ff);
	                        y = y - (byteBuffer[j + 3] & 0x000000ff);
	                        j = j + 4;
	                    } // else if ((byteBuffer[j] == 0) && (byteBuffer[j+1] == 2))
	                    else if ((byteBuffer[j] == 0) && (byteBuffer[j + 1] == 0)) {
	
	                        // end of a line
	                        if (x != 0) {
	                            x = 0;
	
	                            if (y < imageExtents[1]-1) {
	                                y = y + 1;
	                            } else {
	                                y = 0;
	                            }
	                        } // if (x != 0)
	
	                        j = j + 2;
	                    } // else if ((byteBuffer[j] == 0) && (byteBuffer[j+1] == 0))
	                    else if ((byteBuffer[j] == 0) && (byteBuffer[j + 1] == 1)) {
	
	                        // end of RLE bitmap
	                    	image.importData(0, byteBuffer2, true);
	                    	j = j + 2;
	                        
	                    } // else if ((fileBuffer[j] == 0) && (fileBuffer[j+1] == 1))
                    } // for (j = 0; j < sizeOfBitmap;)  	
                } // topDown	
            } // else if ((biBitCount == 4)&& (biCompression == BI_RLE4))
            else if ((biBitCount == 8) && (biCompression == BI_RGB)) {
            	byteBuffer2 = new byte[sliceSize];
                byteBuffer = new byte[bufferSize];
                raFile.read(byteBuffer);
                if (bottomUp) {
                	for (j = bytesPerRow*(imageExtents[1]-1), jstart = bytesPerRow*(imageExtents[1]-1),
                			y = 0; y < imageExtents[1]; y++, j = jstart - bytesPerRow) {
                        for (x = 0, jstart = j; x < imageExtents[0]; x++) {
                            index = x + y*imageExtents[0];
                            byteBuffer2[index] = byteBuffer[j++];
                        }
                    }		
                } // if (bottomUp)
                else { // topDown
                	for (j = 0, jstart = 0,y = 0; y < imageExtents[1]; y++, j = jstart + bytesPerRow) {
                        for (x = 0, jstart = j; x < imageExtents[0]; x++) {
                            index = x + y*imageExtents[0];
                            byteBuffer2[index] = byteBuffer[j++];
                        }
                    }	
                } // else topDown
                image.importData(0, byteBuffer2, true);
            } // else if ((biBitCount == 8) && (biCompression == BI_RGB))
            else if ((biBitCount == 8) && (biCompression == BI_RLE8)) {
            	byteBuffer2 = new byte[sliceSize];
                byteBuffer = new byte[(int)sizeOfBitmap];	
                raFile.read(byteBuffer);
                if (bottomUp) {
                	x = 0;
                	y = imageExtents[1] - 1;
                    for (j = 0; j < sizeOfBitmap;) {

                        if (byteBuffer[j] != 0) {

                            for (k = 0; k < (byteBuffer[j] & 0x000000ff); k++) {
                                byteBuffer2[x + (imageExtents[0] * y)] = byteBuffer[j + 1];
                                x++;

                                if ((x == imageExtents[0]) && (y > 0)) {
                                    x = 0;
                                    y--;
                                } // if ((x == imgExtents[0]) && (y > 0))
                                else if ((x == imageExtents[0]) && (y == 0)) {
                                    x = 0;
                                    y = imageExtents[1] - 1;
                                } // else if ((x == imgExtents[0]) && (y == 0))
                            } // for (k = 0; k < (fileBuffer[j] & 0x000000ff); k++)

                            j = j + 2;
                        } // if (byteBuffer[j] != 0)
                        else if ((byteBuffer[j] == 0) && ((byteBuffer[j + 1] & 0x000000ff) > 2)) {

                            for (k = 0; k < (byteBuffer[j + 1] & 0x000000ff); k++) {
                                byteBuffer2[x + (imageExtents[0] * y)] = byteBuffer[j + k + 2];
                                x++;

                                if ((x == imageExtents[0]) && (y > 0)) {
                                    x = 0;
                                    y--;
                                } // if ((x == imageExtents[0]) && (y > 0))
                                else if ((x == imageExtents[0]) && (y == 0)) {
                                    x = 0;
                                    y = imageExtents[1] - 1;
                                } // else if ((x == imageExtents[0]) && (y == 0))
                            } // for (k = 0; k < (fileBuffer[j+1] & 0x000000ff); k++)

                            j = j + 2 + (byteBuffer[j + 1] & 0x000000ff) +
                            ((byteBuffer[j + 1] & 0x000000ff) % 2);
	                    } // else if ((byteBuffer[j] == 0) && ((byteBuffer[j+1] & 0x000000ff) > 2))
	                    else if ((byteBuffer[j] == 0) && (byteBuffer[j + 1] == 2)) {
	                        x = x + (byteBuffer[j + 2] & 0x000000ff);
	                        y = y - (byteBuffer[j + 3] & 0x000000ff);
	                        j = j + 4;
	                    } // else if ((byteBuffer[j] == 0) && (byteBuffer[j+1] == 2))
	                    else if ((byteBuffer[j] == 0) && (byteBuffer[j + 1] == 0)) {
	
	                        // end of a line
	                        if (x != 0) {
	                            x = 0;
	
	                            if (y > 0) {
	                                y = y - 1;
	                            } else {
	                                y = imageExtents[1] - 1;
	                            }
	                        } // if (x != 0)
	
	                        j = j + 2;
	                    } // else if ((byteBuffer[j] == 0) && (byteBuffer[j+1] == 0))
	                    else if ((byteBuffer[j] == 0) && (byteBuffer[j + 1] == 1)) {
	
	                        // end of RLE bitmap
	                    	image.importData(0, byteBuffer2, true);
	                    	j = j + 2;
	                        
	                    } // else if ((fileBuffer[j] == 0) && (fileBuffer[j+1] == 1))
                    } // for (j = 0; j < sizeOfBitmap;)  
                } // if (bottomUp)
                else { // topDown
                	x = 0;
                	y = 0;
                    for (j = 0; j < sizeOfBitmap;) {

                        if (byteBuffer[j] != 0) {

                            for (k = 0; k < (byteBuffer[j] & 0x000000ff); k++) {
                                byteBuffer2[x + (imageExtents[0] * y)] = byteBuffer[j + 1];
                                x++;

                                if ((x == imageExtents[0]) && (y < imageExtents[1]-1)) {
                                    x = 0;
                                    y++;
                                } // if ((x == imgExtents[0]) && (y < imageExtents[1]-1))
                                else if ((x == imageExtents[0]) && (y == imageExtents[1]-1)) {
                                    x = 0;
                                    y = 0;
                                } // else if ((x == imgExtents[0]) && (y == imageExtents[1]-1))
                            } // for (k = 0; k < (fileBuffer[j] & 0x000000ff); k++)

                            j = j + 2;
                        } // if (byteBuffer[j] != 0)
                        else if ((byteBuffer[j] == 0) && ((byteBuffer[j + 1] & 0x000000ff) > 2)) {

                            for (k = 0; k < (byteBuffer[j + 1] & 0x000000ff); k++) {
                                byteBuffer2[x + (imageExtents[0] * y)] = byteBuffer[j + k + 2];
                                x++;

                                if ((x == imageExtents[0]) && (y < imageExtents[1]-1)) {
                                    x = 0;
                                    y++;
                                } // if ((x == imageExtents[0]) && (y < imageExtents[1]-1))
                                else if ((x == imageExtents[0]) && (y == imageExtents[1]-1)) {
                                    x = 0;
                                    y = 0;
                                } // else if ((x == imageExtents[0]) && (y == imageExtents[1]-1))
                            } // for (k = 0; k < (fileBuffer[j+1] & 0x000000ff); k++)

                            j = j + 2 + (byteBuffer[j + 1] & 0x000000ff) +
                            ((byteBuffer[j + 1] & 0x000000ff) % 2);
	                    } // else if ((byteBuffer[j] == 0) && ((byteBuffer[j+1] & 0x000000ff) > 2))
	                    else if ((byteBuffer[j] == 0) && (byteBuffer[j + 1] == 2)) {
	                        x = x + (byteBuffer[j + 2] & 0x000000ff);
	                        y = y + (byteBuffer[j + 3] & 0x000000ff);
	                        j = j + 4;
	                    } // else if ((byteBuffer[j] == 0) && (byteBuffer[j+1] == 2))
	                    else if ((byteBuffer[j] == 0) && (byteBuffer[j + 1] == 0)) {
	
	                        // end of a line
	                        if (x != 0) {
	                            x = 0;
	
	                            if (y < imageExtents[1]-1) {
	                                y = y + 1;
	                            } else {
	                                y = 0;
	                            }
	                        } // if (x != 0)
	
	                        j = j + 2;
	                    } // else if ((byteBuffer[j] == 0) && (byteBuffer[j+1] == 0))
	                    else if ((byteBuffer[j] == 0) && (byteBuffer[j + 1] == 1)) {
	
	                        // end of RLE bitmap
	                    	image.importData(0, byteBuffer2, true);
	                    	j = j + 2;
	                        
	                    } // else if ((fileBuffer[j] == 0) && (fileBuffer[j+1] == 1))
                    } // for (j = 0; j < sizeOfBitmap;)  	
                } // topDown
            } // else if ((biBitCount == 8) && (biCompression == BI_RLE8))
            else if ((biBitCount == 16) && (biCompression == BI_RGB)) {
                byteBuffer2 = new byte[4*sliceSize];
                byteBuffer = new byte[bufferSize];
                raFile.read(byteBuffer);
                if (bottomUp) {
                	for (j = bytesPerRow*(imageExtents[1]-1), jstart = bytesPerRow*(imageExtents[1]-1),
                			y = 0; y < imageExtents[1]; y++, j = jstart - bytesPerRow) {
                        for (x = 0, jstart = j; x < imageExtents[0]; x++, j += 2) {
                        	index = x + y*imageExtents[0];
                            byteBuffer2[4*index] = (byte)255;
                            shortColor = (short) ((byteBuffer[j] & 0xff) |
                                               ((byteBuffer[j + 1] & 0xff)  << 8));
                            byteBuffer2[4*index+1] = (byte)((shortColor & 0x7C00) >>> 7);
                            byteBuffer2[4*index+2] = (byte)((shortColor & 0x03E0) >>> 2);
                            byteBuffer2[4*index+3] = (byte)((shortColor & 0x001F) << 3);
                        }
                	}	
                } // if (bottomUp)
                else { // topDown
                	for (j = 0, jstart = 0,y = 0; y < imageExtents[1]; y++, j = jstart + bytesPerRow) {
                        for (x = 0, jstart = j; x < imageExtents[0]; x++, j += 2) {
                        	index = x + y*imageExtents[0];
                            byteBuffer2[4*index] = (byte)255;
                            shortColor = (short) ((byteBuffer[j] & 0xff) |
                                               ((byteBuffer[j + 1] & 0xff)  << 8));
                            byteBuffer2[4*index+1] = (byte)((shortColor & 0x7C00) >>> 7);
                            byteBuffer2[4*index+2] = (byte)((shortColor & 0x03E0) >>> 2);
                            byteBuffer2[4*index+3] = (byte)((shortColor & 0x001F) << 3);
                        }
                	}
                } // else topDown
                image.importData(0, byteBuffer2, true);
            } // else if ((biBitCount == 16) && (biCompression == BI_RGB))
            else if ((biBitCount == 16) && (biCompression == BI_BITFIELDS)) {
                byteBuffer2 = new byte[4*sliceSize];
                byteBuffer = new byte[bufferSize];
                raFile.read(byteBuffer);
                if (bottomUp) {
                	for (j = bytesPerRow*(imageExtents[1]-1), jstart = bytesPerRow*(imageExtents[1]-1),
                			y = 0; y < imageExtents[1]; y++, j = jstart - bytesPerRow) {
                        for (x = 0, jstart = j; x < imageExtents[0]; x++, j += 2) {
                        	index = x + y*imageExtents[0];
                            byteBuffer2[4*index] = (byte)255;
                            shortColor = (short) ((byteBuffer[j] & 0xff) |
                                               ((byteBuffer[j + 1] & 0xff)  << 8));
                            if ((redEndBit - 7) >= 0) {
                                byteBuffer2[4*index+1] = (byte)((shortColor & redMask) >>> (redEndBit - 7));
                            }
                            else if (redEndBit != -1) {
                            	byteBuffer2[4*index+1] = (byte)((shortColor & redMask) << (7 - redEndBit));	
                            }
                            if ((greenEndBit - 7) >= 0) {
                                byteBuffer2[4*index+2] = (byte)((shortColor & greenMask) >>> (greenEndBit - 7));
                            }
                            else if (greenEndBit != -1) {
                            	byteBuffer2[4*index+2] = (byte)((shortColor & greenMask) << (7 - greenEndBit));	
                            }
                            if ((blueEndBit - 7) >= 0) {
                            	byteBuffer2[4*index+3] = (byte)((shortColor & blueMask) >>> (blueEndBit - 7));    
                            }
                            else if (blueEndBit != -1) {
                            	byteBuffer2[4*index+3] = (byte)((shortColor & blueMask) << (7 - blueEndBit));	
                            }
                        }
                	}	
                } // if (bottomUp)
                else { // topDown
                	for (j = 0, jstart = 0,y = 0; y < imageExtents[1]; y++, j = jstart + bytesPerRow) {
                        for (x = 0, jstart = j; x < imageExtents[0]; x++, j += 2) {
                        	index = x + y*imageExtents[0];
                            byteBuffer2[4*index] = (byte)255;
                            shortColor = (short) ((byteBuffer[j] & 0xff) |
                                               ((byteBuffer[j + 1] & 0xff)  << 8));
                            if ((redEndBit - 7) >= 0) {
                                byteBuffer2[4*index+1] = (byte)((shortColor & redMask) >>> (redEndBit - 7));
                            }
                            else if (redEndBit != -1){
                            	byteBuffer2[4*index+1] = (byte)((shortColor & redMask) << (7 - redEndBit));	
                            }
                            if ((greenEndBit - 7) >= 0) {
                                byteBuffer2[4*index+2] = (byte)((shortColor & greenMask) >>> (greenEndBit - 7));
                            }
                            else if (greenEndBit != -1) {
                            	byteBuffer2[4*index+2] = (byte)((shortColor & greenMask) << (7 - greenEndBit));	
                            }
                            if ((blueEndBit - 7) >= 0) {
                            	byteBuffer2[4*index+3] = (byte)((shortColor & blueMask) >>> (blueEndBit - 7));    
                            }
                            else if (blueEndBit != -1) {
                            	byteBuffer2[4*index+3] = (byte)((shortColor & blueMask) << (7 - blueEndBit));	
                            }
                        }
                	}
                } // else topDown
                image.importData(0, byteBuffer2, true);
            } // else if ((biBitCount == 16) && (biCompression == BI_BIBITFIELDS))
            else if ((biBitCount == 24) && (biCompression == BI_RGB)) {
                byteBuffer2 = new byte[4*sliceSize];
                byteBuffer = new byte[bufferSize];
                raFile.read(byteBuffer);
                if (bottomUp) {
                	for (j = bytesPerRow*(imageExtents[1]-1), jstart = bytesPerRow*(imageExtents[1]-1),
                			y = 0; y < imageExtents[1]; y++, j = jstart - bytesPerRow) {
                        for (x = 0, jstart = j; x < imageExtents[0]; x++, j += 3) {
                        	index = x + y*imageExtents[0];
                            byteBuffer2[4*index] = (byte)255;
                            byteBuffer2[4*index+1] = byteBuffer[j+2];
                            byteBuffer2[4*index+2] = byteBuffer[j+1];
                            byteBuffer2[4*index+3] = byteBuffer[j];
                        }
                	}	
                } // if (bottomUp)
                else { // topDown
                	for (j = 0, jstart = 0,y = 0; y < imageExtents[1]; y++, j = jstart + bytesPerRow) {
                        for (x = 0, jstart = j; x < imageExtents[0]; x++, j += 3) {
                        	index = x + y*imageExtents[0];
                            byteBuffer2[4*index] = (byte)255;
                            byteBuffer2[4*index+1] = byteBuffer[j+2];
                            byteBuffer2[4*index+2] = byteBuffer[j+1];
                            byteBuffer2[4*index+3] = byteBuffer[j];
                        }
                	}
                } // else topDown
                image.importData(0, byteBuffer2, true);
            } // else if ((biBitCount == 24) && (biCompression == BI_RGB))
            else if ((biBitCount == 32) && (biCompression == BI_RGB)) {
                byteBuffer2 = new byte[4*sliceSize];
                byteBuffer = new byte[bufferSize];
                raFile.read(byteBuffer);
                if (bottomUp) {
                	for (j = bytesPerRow*(imageExtents[1]-1), jstart = bytesPerRow*(imageExtents[1]-1),
                			y = 0; y < imageExtents[1]; y++, j = jstart - bytesPerRow) {
                        for (x = 0, jstart = j; x < imageExtents[0]; x++, j += 4) {
                        	index = x + y*imageExtents[0];
                            byteBuffer2[4*index] = byteBuffer[j+3];
                            byteBuffer2[4*index+1] = byteBuffer[j+2];
                            byteBuffer2[4*index+2] = byteBuffer[j+1];
                            byteBuffer2[4*index+3] = byteBuffer[j];
                        }
                	}	
                } // if (bottomUp)
                else { // topDown
                	for (j = 0, jstart = 0,y = 0; y < imageExtents[1]; y++, j = jstart + bytesPerRow) {
                        for (x = 0, jstart = j; x < imageExtents[0]; x++, j += 4) {
                        	index = x + y*imageExtents[0];
                            byteBuffer2[4*index] = byteBuffer[j+3];
                            byteBuffer2[4*index+1] = byteBuffer[j+2];
                            byteBuffer2[4*index+2] = byteBuffer[j+1];
                            byteBuffer2[4*index+3] = byteBuffer[j];
                        }
                	}
                } // else topDown
                image.importData(0, byteBuffer2, true);
            } // else if ((biBitCount == 32) && (biCompression == BI_RGB))
            else if ((biBitCount == 32) && (biCompression == BI_BITFIELDS)) {
            	byteBuffer2 = new byte[4*sliceSize];
                byteBuffer = new byte[bufferSize];
                raFile.read(byteBuffer);
                if (bottomUp) {
                	for (j = bytesPerRow*(imageExtents[1]-1), jstart = bytesPerRow*(imageExtents[1]-1),
                			y = 0; y < imageExtents[1]; y++, j = jstart - bytesPerRow) {
                        for (x = 0, jstart = j; x < imageExtents[0]; x++, j += 4) {
                        	index = x + y*imageExtents[0];
                        	intColor = (int) ((byteBuffer[j] & 0xff) |
                                    ((byteBuffer[j + 1] & 0xff)  << 8) |
                                    ((byteBuffer[j + 2] & 0xff) << 16) |
                                    ((byteBuffer[j + 3] & 0xff) << 24));
               
                        	if ((alphaEndBit - 7) >= 0) {
                                byteBuffer2[4*index] = (byte)((intColor & alphaMask) >>> (alphaEndBit - 7));
                            }
                            else if (alphaEndBit != -1){
                            	byteBuffer2[4*index] = (byte)((intColor & alphaMask) << (7 - redEndBit));	
                            }
                            else {
                        	    byteBuffer2[4*index] = (byte)255;
                            }
                            
                            if ((redEndBit - 7) >= 0) {
                                byteBuffer2[4*index+1] = (byte)((intColor & redMask) >>> (redEndBit - 7));
                            }
                            else if (redEndBit != -1){
                            	byteBuffer2[4*index+1] = (byte)((intColor & redMask) << (7 - redEndBit));	
                            }
                            if ((greenEndBit - 7) >= 0) {
                                byteBuffer2[4*index+2] = (byte)((intColor & greenMask) >>> (greenEndBit - 7));
                            }
                            else if (greenEndBit != -1){
                            	byteBuffer2[4*index+2] = (byte)((intColor & greenMask) << (7 - greenEndBit));	
                            }
                            if ((blueEndBit - 7) >= 0) {
                            	byteBuffer2[4*index+3] = (byte)((intColor & blueMask) >>> (blueEndBit - 7));    
                            }
                            else if (blueEndBit != -1){
                            	byteBuffer2[4*index+3] = (byte)((intColor & blueMask) << (7 - blueEndBit));	
                            }
                        }
                	}	
                } // if (bottomUp)
                else { // topDown
                	for (j = 0, jstart = 0,y = 0; y < imageExtents[1]; y++, j = jstart + bytesPerRow) {
                        for (x = 0, jstart = j; x < imageExtents[0]; x++, j += 4) {
                        	index = x + y*imageExtents[0];
                        	intColor = (int) ((byteBuffer[j] & 0xff) |
                                    ((byteBuffer[j + 1] & 0xff)  << 8) |
                                    ((byteBuffer[j + 2] & 0xff) << 16) |
                                    ((byteBuffer[j + 3] & 0xff) << 24));
               
                        	if ((alphaEndBit - 7) >= 0) {
                                byteBuffer2[4*index] = (byte)((intColor & alphaMask) >>> (alphaEndBit - 7));
                            }
                            else if (alphaEndBit != -1){
                            	byteBuffer2[4*index] = (byte)((intColor & alphaMask) << (7 - redEndBit));	
                            }
                            else {
                        	    byteBuffer2[4*index] = (byte)255;
                            }
                            
                            if ((redEndBit - 7) >= 0) {
                                byteBuffer2[4*index+1] = (byte)((intColor & redMask) >>> (redEndBit - 7));
                            }
                            else if (redEndBit != -1){
                            	byteBuffer2[4*index+1] = (byte)((intColor & redMask) << (7 - redEndBit));	
                            }
                            if ((greenEndBit - 7) >= 0) {
                                byteBuffer2[4*index+2] = (byte)((intColor & greenMask) >>> (greenEndBit - 7));
                            }
                            else if (greenEndBit != -1){
                            	byteBuffer2[4*index+2] = (byte)((intColor & greenMask) << (7 - greenEndBit));	
                            }
                            if ((blueEndBit - 7) >= 0) {
                            	byteBuffer2[4*index+3] = (byte)((intColor & blueMask) >>> (blueEndBit - 7));    
                            }
                            else if (blueEndBit != -1){
                            	byteBuffer2[4*index+3] = (byte)((intColor & blueMask) << (7 - blueEndBit));	
                            }
                        }
                	}
                } // else topDown
                image.importData(0, byteBuffer2, true);	
            } // else if ((biBitCount == 32) && (biCompression == BI_BITFIELDS)) 
            
            if (multiFile) {
                Object obj = image.exportData(0, image.getSliceSize());
                System.out.println(obj.getClass());
                imgBuffer = new Number[image.getSliceSize()];
                if(obj instanceof byte[]) {
                    
                    byte[] bAr = new byte[image.getSliceSize()];
                    System.out.println(image.getSliceSize() +" vs "+bAr.length);
                    System.arraycopy(obj, 0, bAr, 0, bAr.length);
                    index = 0;
                    for(byte b : bAr) {
                        imgBuffer[index++] = Byte.valueOf(b);
                    }
                } else if(obj instanceof float[]) {
                    float[] fAr = new float[image.getSliceSize()];
                    System.arraycopy(image.exportData(0, image.getSliceSize()), image.getSliceSize(), fAr, 0, fAr.length);
                    index = 0;
                    for(float f : fAr) {
                        imgBuffer[index++] = Float.valueOf(f);
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
    
    private String readCString() throws IOException {
        String cString = "";
        boolean nullFound = false;
        byte oneByte[] = new byte[1];
        while (!nullFound) {
            raFile.read(oneByte);
            if (oneByte[0]  == 0) {
                nullFound = true;
            }
            else {
                cString += new String(oneByte);
            }
        } // while (!nullFound)
        return cString;
    }

    
    /**
     * Accessor to set the file name (used for reading COR multiFile).
     *
     * @param  fName  file name of image to read.
     */
    public void setFileName(String fName) {
        fileName = fName;
    }

    
}
