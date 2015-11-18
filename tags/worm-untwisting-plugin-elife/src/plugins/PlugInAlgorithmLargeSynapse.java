
import gov.nih.mipav.model.algorithms.AlgorithmBase;

import gov.nih.mipav.plugins.*; // needed to load PlugInAlgorithm / PlugInView / PlugInFile interface

import gov.nih.mipav.model.file.*;

import gov.nih.mipav.view.*;

import java.io.*;

import java.io.IOException;
import java.util.ArrayList;

import java.util.Vector;




/**
 * This is simple plugin that sweeps the images with sets of 16 parallel lines to find all red, blue, green synapses.
 * Note that Java can only index arrays with signed integers, so it can only go from 0 to 2**31 - 1 = 2,147,483,647 =
 * 2.147E9.
 * JC wishes to use files with 5000 X 5000 X 5000 pixels = 1.25E11 pixels.  Since ARGB has 4 bytes per pixel, this
 * requires 5E11 bytes.
 * This plugin is a modification of PlugInAlgorithmSynapseDetection that handles large size files. The user does not create a
 * source image for the plug in to operate on.  The plugin operates directly from the .tif file without a model image.
 * It does not produce a mask image or place VOIs on the source image as AlgorrithmSynapseDetection does.  Also, it does not
 * have the histInfo checkBox. In this modification the user must check a dialog box for large blue fraction whereas the
 * original plugin AlgorithmSynapseDetection activated the big blue fraction functionality if the blue fraction of the
 * source image was larger than 0.05. 
 * Use arrays of arrays rather than arrays for buffer and colorObjects to remove restriction on array size.
 * 
 *           The below comments are copied from the original PlugInAlgorithmSynapseDetection.
 *           Because these files are very large, buffers must be as few and small as possible.  Maintaining separate
 *           red, green, and blue buffers for the entire program is not feasible because of the large file sizes.
 *           Therefore, all pixels are classified as either BRIGHT_RED, RED, BRIGHT_GREEN, GREEN, BRIGHT_BLUE, BLUE,
 *           or NONE within a single byte buffer.  User specified redBrightIntensity, greenBrightIntensity, 
 *           and blueBrightIntensity are used to separate the BRIGHT_COLOR value from the COLOR value.
 *           
 *           If bigBlueFraction is true, all red, green, and blue objects are IDed.  Every pixel in the image is examined
 *           and if that pixel is not NONE and is not already part of a red, blue, or green object, then a new red, blue,
 *           or green object is declared and a 26 neighbor region grow of that object is performed.  If bigBlueFraction
 *           is false, then only blue objects are IDed.  Every pixel in the image is examined and if that pixel is
 *           BLUE or BRIGHT_BLUE and is not already part of a blue object, then a new blue object is declared and a
 *           26 neighbor grow of that blue object is performed.  The blue count and the x, y, and z indices of the
 *           blue center and the blue x, y, and z widths are calculated only for bigBlueFraction = false.  x width =
 *           high x - low x + 1, y width = high y - low y + 1, z width = high z - low z + 1.
 *           
 *           Note that when line profiles are taken thru a synapse the red and green have all sorts of
             different shapes but the blue is almost always a distinct almost rectangular pulse, so the 
             presence of blue >= blueBrightIntensity is the most significant factor.  A pixel is 
             counted as BRIGHT_BLUE as long as its blue value is >= blueBrightIntensity even if the red and/or
             green values at that pixel location are greater than or equal to the blue value.  This lets the well
             defined width of the blue band be determined.  If the blue value is < blueBrightIntensity, but the blue value
             is greater than the red and green values and >= blueIntensity, the pixel is classified as BLUE.  
             If blue value is less than blueBrightIntensity and the red value is greater than the green and blue values,
             then the pixel is classified as BRIGHT_RED if red >= redBrightIntensity and as RED if red >= redIntensity.
             If the blue value is less than blueIntensity and the green value is greater than the red and blue values, then the pixel is classified
             as BRIGHT_GREEN if green >= greenBrightIntensity and as GREEN if green >= greenIntensity.  If none of the above
             conditions is met, then the buffer value is set to NONE.
             
             The basic idea is to find a red, blue, green or a green, blue, red sequence while traversing a line from
             one side of the image to the opposite side.  13 sets of parallel lines are searched.  Lines parallel to:
             1.) The x axis.
             2.) The y axis.
             3.) The z axis.
             4.) x = -y
             5.) x = y
             6.) x = -z
             7.) x = z
             8.) y = -z
             9.) y = z
             delX = delY = delZ means the change in X = change in Y = change in Z.  Note that the ratio of 2 del quantities gives slopes,
             so in case 10, delY/delX = 1 and delZ/delX = 1.
             10.) delX = delY = delZ
             11.) delX = delY = -delZ
             12.) delX = -delY = delZ
             13.) -delX = delY = delZ
             The red band must have at least one BRIGHT_RED pixel, the green band must have at least one BRIGHT_GREEN pixel, 
             and the blue band must have at least one BRIGHT_BLUE pixel.  The widths of these bands is must fall within user
             specified minimum and maximum values.  For red and green, the user specifies redMin, redMax, greenMin, and 
             greenMax.  For the blue band lines within planes must fall within blueMinXY and blueMaxXY limits, and lines that
             cross planes must fall within blueMinZ and blueMaxZ limits.  Note that redBrightIntensity and redIntensity can be
             set at the same value, greenBrightIntensity and greenIntensity can be set at the same value, and
             blueBrightIntensity and blueIntensity can be set at the same value.  If the BrightIntensity and Intensity 
             values are the same, then the sofware treats all values as if they are BrightIntensity values.
             
             If a red, blue, green or green, blue, red sequence meets the intensity and width requirements, then a find is
             initially declared at the center of the blue band.  There are 2 possible cases.  The first is when the blueFraction 
             is small, blueFraction < 0.05.  bigBlueFraction is precalculated as true or false in the dialog.  if bigBlueFraction = false,
             the entire blue region can be assumed to be small and the entire blue region is assumed to be part of the synapse.  
             The center of the 26 connected BRIGHT_BLUE and BLUE neighbors within this blue region is used as the location
             of the point VOI for the synapse.  An array synapseBlueFound has an element for every blue object.  When a synapse is found,
             the synapseBlueFound index corresponding to that blue object is set to one, so that the same synapse will not
             be found again on another line search thru the same blue region in a different direction.  Note that in 3D a small
             blue synapse region is often surrounded by multiple red and green regions, so multiple finds will occur on this same
             small blue region if we only require a distinct combination of red, blue, and green objects.  
             
             The second case if when the blueFraction >= 0.05.  In this case it is not possible to have the entire blue region
             included in the synapse.  In this case the blue synapse center will always simply be taken
             as the initial find location and the blue pixel count of the synapse cannot be determined.  Here it would be
             possible to have multiple synapses using the same blue object, so in this case rather than requiring that the
             blue object has not been used, we require only that the particular red object, blue object, green object
             combination has not been used before.  The software allows 5 synapses to be found for every red object.
             Every red object has 5 entries in synapseBlueFound for blue object indices of a found synapse and 5 entries
             in synapseGreenFound for green object indices of a found synapse.
             
             For the redIntensity, greenIntensity, and blueIntensity I opened your file of contours around synapses Synapse100.xml
             on the image SynapseSpotted_d_RedGreenL4_L23_090105_1_to_59Crop3_enhanced.tif.  I drew one line segment thru each
             synapse and noted the maximum red, green, and blue pixel intensities and the blue width.  This was not super scientific as
             there are different ways the line segment could be drawn.
             Slice 0 red 98, green 130, blue 150, blue width 9
             Slice 1 left red 131, green 139, blue 175, blue width 10
             Slice 1 right red 255, green 162, blue 82, blue width 7
             Slice 2 red 214, green 101, blue 156, blue width 4
             Slice 4 red 108, green 87, blue 134, blue width 11
             Slice 7 left Not typical don't record
             Slice 7 right red 69, green 123, blue 146, blue width 18
             Slice 8 red 82, green 129, blue 163, blue width 24
             Slice 10 top red 56, green 110, blue 90, blue width 6
             Slice 10 bottom red 111, green 98, blue 107, blue width 10
             Slice 17 red 134, green 107, blue 218, blue width 10
             Slice 19 red 95, green 100, blue 90, blue width 14
             Slice 22 red 157, green 172, blue 99, blue width 9
             Slice 27 lower order along line is green, red, blue, red   Have red 116, green 239, blue 138, blue width 19
             Slice 27 upper order along line is red, green, blue Have red 214, green 171, blue 142, blue width 24
             So the measured range was:
             Red 56-255
             Green 87-239
             Blue 82-175
             Blue width 4-24
             To include most synapses I simply took the minimum of these maximum values and provided a further 1 or 2
             of margin by rounding down to the number ending in the nearest 0 or 5.  Of course, not all your images
             will have the same intensities, so you will have to vary these numbers with different runs.

 *
 * @see  PlugInGeneric
 */
public class PlugInAlgorithmLargeSynapse extends AlgorithmBase {
    
    private String inputFileName;
    
    private String directory;
    
    /** Minimum number of red pixels along line */
    private int redMin;
    
    /** Maximum number of pixels along line */
    private int redMax;
    
    /** Minimum number of green pixels along line */
    private int greenMin;
    
    /** Maximum number of pixels along line */
    private int greenMax;
    
    /** Minimum number of blue pixels along line within slice*/
    private int blueMinXY;
    
    /** Maximum number of blue pixels along line within slice*/
    private int blueMaxXY;
    
    /** Minimum number of blue pixels along line between slices*/
    private int blueMinZ;
    
    /** Maximum number of blue pixels along line between slices*/
    private int blueMaxZ;
    
    private boolean bigBlueFraction;
    
    /** Minimum intensity values */
    private int redIntensity;
    
    private int redBrightIntensity;
    
    private int greenIntensity;
    
    private int greenBrightIntensity;
    
    private int blueIntensity;
    
    private int blueBrightIntensity;
    
    private RandomAccessFile raFile;
    
    private boolean endianess;
    
    @SuppressWarnings("unchecked")
    private Vector<Index>[] dataOffsets = new Vector[4000];
    
    private int xDim;
    private int yDim;
    private int zDim;
    private long fileLength;
    
    private int[] IFDoffsets = new int[4096];
    
    private int[] bitsPerSample = null;
    
    private int imageSlice;
    
    private byte[] imageDescription = null;
    private int threePreviousColor;
    private int twoPreviousColor;
    private int onePreviousColor;
    private int presentColor;
    private int presentBrightColor;
    private int threePreviousWidth;
    private int twoPreviousWidth;
    private int onePreviousWidth;
    private int presentWidth;
    private boolean presentBrightness;
    private boolean onePreviousBrightness;
    private boolean twoPreviousBrightness;
    private boolean threePreviousBrightness;
    private int centerBlueX;
    private int centerBlueY;
    private int centerBlueZ;
    private int numSynapses = 0;
    private int previousNumSynapses = 0;
    private int threeBandMinXY;
    private int threeBandMinZ;
    private int blueCount;
    private String dataString = null;
    private int minimumBlueCount = Integer.MAX_VALUE;
    private int maximumBlueCount = Integer.MIN_VALUE;
    private long totalBlueCount = 0;
    private long totalBlueCountSquared = 0;
    private double averageBlueCount;
    private double blueStandardDeviation;
    private int colorObjects[][];
    private int redObjectIndex = 0;
    private int greenObjectIndex = 0;
    private int blueObjectIndex = 0;
    private ArrayList <Integer>blueCountList;
    private ArrayList <Short>blueCenterXList;
    private ArrayList <Short>blueCenterYList;
    private ArrayList <Short>blueCenterZList;
    private ArrayList <Short>blueXWidthList;
    private ArrayList <Short>blueYWidthList;
    private ArrayList <Short>blueZWidthList;
    //  If bigBlueFraction will allow storage of 5 synapses for each red object
    // If not bigBlueFraction allow only 1 synapse per blue object
    private int synapseBlueFound[];
    private int synapseGreenFound[];
    private int threePreviousObjectIndex;
    private int twoPreviousObjectIndex;
    private int onePreviousObjectIndex;
    private int presentObjectIndex;
    //  In each processed volume all bytes are classified as NONE, RED, GREEN, BLUE, BRIGHT_RED,
    // BRIGHT_GREEN, or BRIGHT_BLUE
    private byte buffer[][];
    // Set to xDim * yDim
    private int xySlice;
    private final byte NONE = 0;
    private final byte RED = 1;
    private final byte GREEN = 2;
    private final byte BLUE = 3;
    private final byte BRIGHT_RED = 4;
    private final byte BRIGHT_GREEN = 5;
    private final byte BRIGHT_BLUE = 6;
    private int blueX;
    private int blueY;
    private int blueZ;
    
    public static final int MAX_IFD_LENGTH = 10000;
    
    /** Tiff Types. */
    public static final int BYTE = 1; // 8 bit unsigned

    /** DOCUMENT ME! */
    public static final int ASCII = 2; // 7 bit ASCII

    /** DOCUMENT ME! */
    public static final int SHORT = 3; // 16 bit unsigned

    /** DOCUMENT ME! */
    public static final int LONG = 4; // 32 bit unsigned ****** 4 bytes !!!!

    /** DOCUMENT ME! */
    public static final int RATIONAL = 5; // 2 longs 1st numerator

    /** 2nd denom. */
    public static final int SBYTE = 6; // 8 bit signed

    /** DOCUMENT ME! */
    public static final int UNDEFINED = 7; // 8 bit undefined

    /** DOCUMENT ME! */
    public static final int SSHORT = 8; // 16 bit signed

    /** DOCUMENT ME! */
    public static final int SLONG = 9; // 32 bit signed

    /** DOCUMENT ME! */
    public static final int SRATIONAL = 10; //

    /** DOCUMENT ME! */
    public static final int FLOAT = 11; // single precision 4 byte IEEE format

    /** DOCUMENT ME! */
    public static final int DOUBLE = 12; // double precision 8 byte IEEE format
    
    /** The IFD type is identical to LONG, except that it is only used to point
     *  to other valid IFDs
     */
    public static final int IFD = 13;

    /** Tiff Tags. */
    public static final int NEW_SUBFILE_TYPE = 254;

    /** DOCUMENT ME! */
    public static final int IMAGE_WIDTH = 256;

    /** DOCUMENT ME! */
    public static final int IMAGE_LENGTH = 257;

    /** DOCUMENT ME! */
    public static final int BITS_PER_SAMPLE = 258;

    /** DOCUMENT ME! */
    public static final int PHOTO_INTERP = 262;

    /** DOCUMENT ME! */
    public static final int IMAGE_DESCRIPTION = 270;
    
    /** DOCUMENT ME! */
    public static final int STRIP_OFFSETS = 273;

    /** DOCUMENT ME! */
    public static final int SAMPLES_PER_PIXEL = 277;

    /** DOCUMENT ME! */
    public static final int ROWS_PER_STRIP = 278;

    /** DOCUMENT ME! */
    public static final int STRIP_BYTE_COUNTS = 279;

    
    /**
     * 
     * @param directory
     * @param inputFileName
     * @param redMin
     * @param redMax
     * @param greenMin
     * @param greenMax
     * @param blueMinXY
     * @param blueMaxXY
     * @param blueMinZ
     * @param blueMaxZ
     * @param bigBlueFraction
     * @param redIntensity
     * @param greenIntensity
     * @param blueIntensity
     * @param redBrightIntensity
     * @param greenBrightIntensity
     * @param blueBrightIntensity
     */
    public PlugInAlgorithmLargeSynapse(String directory, String inputFileName, int redMin, int redMax,
            int greenMin, int greenMax, int blueMinXY, int blueMaxXY, int blueMinZ, int blueMaxZ, boolean bigBlueFraction, 
            int redIntensity, int greenIntensity, int blueIntensity, int redBrightIntensity,
            int greenBrightIntensity, int blueBrightIntensity) {
        this.directory = directory;
        this.inputFileName = inputFileName;
        this.redMin = redMin;
        this.redMax = redMax;
        this.greenMin = greenMin;
        this.greenMax = greenMax;
        this.blueMinXY = blueMinXY;
        this.blueMaxXY = blueMaxXY;
        this.blueMinZ = blueMinZ;
        this.blueMaxZ = blueMaxZ;
        this.bigBlueFraction = bigBlueFraction;
        this.redIntensity = redIntensity;
        this.greenIntensity = greenIntensity;
        this.blueIntensity = blueIntensity;
        this.redBrightIntensity = redBrightIntensity;
        this.greenBrightIntensity = greenBrightIntensity;
        this.blueBrightIntensity = blueBrightIntensity;
    }

    //~ Methods --------------------------------------------------------------------------------------------------------

    
    
    /**
     * Runs the plugin.
     *
     */
    public void runAlgorithm() {
        long time;
        int i;
        int j;
        int x;
        int y;
        int z;
        int yPos;
        int red;
        int green;
        int blue;
        int pos;
        File file;
        int blueStartX = 0;
        int blueStartY = 0;
        int blueStartZ = 0;
        byte[] sliceBufferByte;
        int idx;
        int nIndex;
        int firstIndex;
        int lastIndex;
        int nLength;
        int totalLength;
        byte byteBuffer[] = null;
        int currentIndex;
        int a;
        int nBytes;
        int xStart;
        int yStart;
        int zStart;
        int index;
        
        fireProgressStateChanged("Synapse detection on image");

        time = System.currentTimeMillis();
        
        try {
            file = new File(directory + inputFileName);
            raFile = new RandomAccessFile(file, "r");
            
            fileLength = raFile.length();

            short byteOrder = raFile.readShort();

            if (byteOrder == 0x4949) {
                endianess = FileBase.LITTLE_ENDIAN;
            } else if (byteOrder == 0x4d4d) {
                endianess = FileBase.BIG_ENDIAN;
            } else {
                raFile.close();
                throw new IOException("TIFF Read Header: Error - first 2 bytes are an illegal " + byteOrder);
            }

            int magicTIFFNumber = getUnsignedShort(endianess);

            if (magicTIFFNumber != 42) {
                raFile.close();
                throw new IOException("Tiff Read Header: Error - Invalid Magic number = " + magicTIFFNumber);
            }

            imageSlice = 0;
            IFDoffsets[imageSlice] = getInt(endianess);

            boolean moreIFDs = true;
            

            while (moreIFDs) { // Find number of images!!
                raFile.seek(IFDoffsets[imageSlice]);
                moreIFDs = openIFD();
            }
            
            int sliceSize =  3* xDim * yDim;
            sliceBufferByte = new byte[sliceSize];

            Preferences.debug("xDim = " + xDim + "\n");
            Preferences.debug("yDim = " + yDim + "\n");
            xySlice = xDim * yDim;
            zDim = imageSlice;
            Preferences.debug("zDim = " + zDim + "\n");
            
            buffer = new byte[zDim][xySlice];
            
            
            for (z = 0; z < zDim; z++) {
                idx = 0;
                nIndex = dataOffsets[z].size();
                firstIndex = ((Index) (dataOffsets[z].elementAt(0))).index;
                lastIndex = ((Index) (dataOffsets[z].elementAt(nIndex - 1))).index;
                if (((Index) (dataOffsets[z].elementAt(nIndex - 1))).byteCount == 0) {

                    nLength = 3 * xDim * yDim;
                            

                    totalLength = nLength + lastIndex - firstIndex;
                } // if (((Index) (dataOffsets[slice].elementAt(nIndex - 1))).byteCount == 0)
                else {
                    totalLength = (lastIndex - firstIndex) + ((Index) (dataOffsets[z].elementAt(nIndex - 1))).byteCount;
                }

                byteBuffer = new byte[totalLength];

                currentIndex = 0;

                //System.err.println("first index: " + firstIndex + ", last index: " + lastIndex + ", totalLength: " +
                //totalLength);
                // System.err.println("packbit is: " + packBit);
                raFile.seek(firstIndex);
                raFile.read(byteBuffer, 0, totalLength);
                i = 0;

                for (a = 0; a < nIndex; a++, idx++) {

                        // System.err.println("Seeking to: " + ( (Index) (dataOffsets[slice].elementAt(idx))).index);
                        currentIndex = ((Index) (dataOffsets[z].elementAt(idx))).index - firstIndex;
                        //System.out.println("CurrentIndex = " + currentIndex);

                        // raFile.seek( ( (Index) (dataOffsets[slice].elementAt(idx))).index);
                        nBytes = ((Index) (dataOffsets[z].elementAt(idx))).byteCount;

                        //System.err.println("doing nBytes: " + nBytes);
                        if (nBytes == 0) {
                            nBytes = 3 * xDim * yDim;
                        }


                        for (j = 0; j < nBytes; i++, j++) {
                            sliceBufferByte[i] = byteBuffer[j + currentIndex];
                        } 
                   
                }
                    
                
                for (y = 0; y < yDim; y++) {
                    yPos = y * xDim;
                    for (x = 0; x < xDim; x++) {
                        pos = yPos + x;
                        red = sliceBufferByte[3*pos] & 0xff;
                        green = sliceBufferByte[3*pos + 1] & 0xff;
                        blue = sliceBufferByte[3*pos + 2] & 0xff;
                        if (blue >= blueBrightIntensity) {
                            buffer[z][pos] = BRIGHT_BLUE;
                        }
                        else if ((blue > red) && (blue > green) && (blue >= blueIntensity)) {
                            buffer[z][pos] = BLUE;
                        }
                        else if ((red > green) && (red > blue) && (red >= redIntensity)) {
                            if (red >= redBrightIntensity) {
                                buffer[z][pos] = BRIGHT_RED;
                            }
                            else {
                                buffer[z][pos] = RED;
                            }
                        }
                        else if ((green > red) && (green > blue) && (green >= greenIntensity)) {
                            if (green >= greenBrightIntensity) {
                                buffer[z][pos] = BRIGHT_GREEN;
                            }
                            else {
                                buffer[z][pos] = GREEN;
                            }
                        }
                        else {
                            buffer[z][pos] = NONE;
                        }
                    }
                }    
                
            } // for (z = 0 ; z < zDim; z++))
            raFile.close();
        }
        catch (IOException e) {
            MipavUtil.displayError("IOException in reading " + inputFileName);
            return;
        }
        
        identifyObjects();
        Preferences.debug("Number of red objects = " + redObjectIndex + "\n");
        Preferences.debug("Number of green objects = " + greenObjectIndex + "\n");
        Preferences.debug("Number of blue objects = " + blueObjectIndex + "\n");
        if (bigBlueFraction) {
            // Allow 5 synapses per red object
            synapseBlueFound = new int[5 * (redObjectIndex+1)];
            synapseGreenFound = new int[5 * (redObjectIndex+1)];
        }
        else {
            // Allow only 1 blue object in each synapse
            synapseBlueFound = new int[blueObjectIndex + 1];
        }
        
        threeBandMinXY = redMin + greenMin + blueMinXY;
        threeBandMinZ = redMin + greenMin + blueMinZ;
        if (bigBlueFraction) {
            dataString = "Index     x         y         z\n\n";    
        }
        else {
            dataString = "Index     x         y         z         count     x width   y width   z width\n\n";
        }
        
        // Search along all lines parallel to x axis
        dataString += ("Searching parallel to x axis\n");
        for (z = 0; z < zDim; z++) {
            for (y = 0; y < yDim; y++) {
                yPos = y * xDim;
                threePreviousColor = NONE;
                twoPreviousColor = NONE;
                onePreviousColor = NONE;
                presentColor = NONE;
                presentBrightColor = NONE;
                threePreviousWidth = 0;
                twoPreviousWidth = 0;
                onePreviousWidth = 0;
                presentWidth = 0;
                threePreviousBrightness = false;
                twoPreviousBrightness = false;
                onePreviousBrightness = false;
                presentBrightness = false;
                for (x = 0; x < xDim; x++) {
                   pos = yPos + x;  
                   if ((buffer[z][pos] == presentColor) || (buffer[z][pos] == presentBrightColor)) {
                       presentWidth++;
                       if ((presentColor == BRIGHT_BLUE) || (presentColor == BLUE)) {
                           blueX = (blueStartX + x) >> 1;
                           blueY = (blueStartY + y) >> 1;
                           blueZ = (blueStartZ + z) >> 1; 
                       }
                       if ((!presentBrightness) && (buffer[z][pos] >= BRIGHT_RED)) {
                           presentBrightness = true;
                       }
                   }
                   else {
                       threePreviousColor = twoPreviousColor;
                       threePreviousWidth = twoPreviousWidth;
                       twoPreviousColor = onePreviousColor;
                       twoPreviousWidth = onePreviousWidth;
                       onePreviousColor = presentColor;
                       onePreviousWidth = presentWidth;
                       threePreviousBrightness = twoPreviousBrightness;
                       twoPreviousBrightness = onePreviousBrightness;
                       onePreviousBrightness = presentBrightness;
                       threePreviousObjectIndex = twoPreviousObjectIndex;
                       twoPreviousObjectIndex = onePreviousObjectIndex;
                       onePreviousObjectIndex = presentObjectIndex;
                       if (buffer[z][pos] < BRIGHT_RED) {
                           presentColor = buffer[z][pos];
                           if (buffer[z][pos] == NONE) {
                               presentBrightColor = NONE;
                           }
                           else {
                               presentBrightColor = presentColor + 3;
                           }
                           presentBrightness = false;
                       }
                       else {
                           presentBrightColor = buffer[z][pos];
                           presentColor = presentBrightColor - 3;
                           presentBrightness = true;
                           
                       }
                       presentWidth = 1;
                       checkForSynapseXY(); 
                       if ((presentColor == BRIGHT_BLUE) || (presentColor == BLUE)) {
                           blueStartX = x;
                           blueStartY = y;
                           blueStartZ = z;
                           blueX = x;
                           blueY = y;
                           blueZ = z; 
                           presentObjectIndex = colorObjects[z][pos];
                       }
                       else if ((presentColor == BRIGHT_RED) || (presentColor == RED)) {
                           if (bigBlueFraction) {
                               presentObjectIndex = colorObjects[z][pos];
                           }
                           else {
                               presentObjectIndex = 0;
                           }
                       }
                       else if ((presentColor == BRIGHT_GREEN) || (presentColor == GREEN)) {
                           if (bigBlueFraction) {
                               presentObjectIndex = colorObjects[z][pos];
                           }
                           else {
                               presentObjectIndex = 0;
                           }
                       }
                   }
                } // for (x = 0; x < xDim; x++)
                threePreviousColor = twoPreviousColor;
                threePreviousWidth = twoPreviousWidth;
                twoPreviousColor = onePreviousColor;
                twoPreviousWidth = onePreviousWidth;
                onePreviousColor = presentColor;
                onePreviousWidth = presentWidth;
                threePreviousBrightness = twoPreviousBrightness;
                twoPreviousBrightness = onePreviousBrightness;
                onePreviousBrightness = presentBrightness;
                threePreviousObjectIndex = twoPreviousObjectIndex;
                twoPreviousObjectIndex = onePreviousObjectIndex;
                onePreviousObjectIndex = presentObjectIndex;
                checkForSynapseXY();
            } // for (y = 0; y < yDim; y++)
        } // for (z = 0; z < zDim; z++)
        System.out.println("Number of synapses found searching parallel to x axis = " + numSynapses);
        Preferences.debug("Number of synapses found searching parallel to x axis = " + numSynapses + "\n");
        dataString += "\nNumber of synapses found searching parallel to x axis = " + numSynapses + "\n";
        previousNumSynapses = numSynapses;
        
        // Search along all lines parallel to y axis
        dataString += ("\nSearching parallel to y axis\n");
        for (z = 0; z < zDim; z++) {
            for (x = 0; x < xDim; x++) {
                threePreviousColor = NONE;
                twoPreviousColor = NONE;
                onePreviousColor = NONE;
                presentColor = NONE;
                presentBrightColor = NONE;
                threePreviousWidth = 0;
                twoPreviousWidth = 0;
                onePreviousWidth = 0;
                presentWidth = 0;
                threePreviousBrightness = false;
                twoPreviousBrightness = false;
                onePreviousBrightness = false;
                presentBrightness = false;
                for (y = 0; y < yDim; y++) {
                    pos = x + y * xDim;
                    if ((buffer[z][pos] == presentColor) || (buffer[z][pos] == presentBrightColor)) {
                        presentWidth++;
                        if ((presentColor == BRIGHT_BLUE) || (presentColor == BLUE)) {
                            blueX = (blueStartX + x) >> 1;
                            blueY = (blueStartY + y) >> 1;
                            blueZ = (blueStartZ + z) >> 1;    
                        }
                        if ((!presentBrightness) && (buffer[z][pos] >= BRIGHT_RED)) {
                            presentBrightness = true;
                        }
                    }
                    else {
                        threePreviousColor = twoPreviousColor;
                        threePreviousWidth = twoPreviousWidth;
                        twoPreviousColor = onePreviousColor;
                        twoPreviousWidth = onePreviousWidth;
                        onePreviousColor = presentColor;
                        onePreviousWidth = presentWidth;
                        threePreviousBrightness = twoPreviousBrightness;
                        twoPreviousBrightness = onePreviousBrightness;
                        onePreviousBrightness = presentBrightness;
                        threePreviousObjectIndex = twoPreviousObjectIndex;
                        twoPreviousObjectIndex = onePreviousObjectIndex;
                        onePreviousObjectIndex = presentObjectIndex;
                        if (buffer[z][pos] < BRIGHT_RED) {
                            presentColor = buffer[z][pos];
                            if (buffer[z][pos] == NONE) {
                                presentBrightColor = NONE;
                            }
                            else {
                                presentBrightColor = presentColor + 3;
                            }
                            presentBrightness = false;
                        }
                        else {
                            presentBrightColor = buffer[z][pos];
                            presentColor = presentBrightColor - 3;
                            presentBrightness = true;
                            
                        }
                        presentWidth = 1;
                        checkForSynapseXY();
                        if ((presentColor == BRIGHT_BLUE) || (presentColor == BLUE)) {
                            blueStartX = x;
                            blueStartY = y;
                            blueStartZ = z;
                            blueX = x;
                            blueY = y;
                            blueZ = z; 
                            presentObjectIndex = colorObjects[z][pos];
                        }
                        else if ((presentColor == BRIGHT_RED) || (presentColor == RED)) {
                            if (bigBlueFraction) {
                                presentObjectIndex = colorObjects[z][pos];
                            }
                            else {
                                presentObjectIndex = 0;
                            }
                        }
                        else if ((presentColor == BRIGHT_GREEN) || (presentColor == GREEN)) {
                            if (bigBlueFraction) {
                                presentObjectIndex = colorObjects[z][pos];
                            }
                            else {
                                presentObjectIndex = 0;
                            }
                        }
                    }
                } // for (y = 0; y < yDim; y++)
                threePreviousColor = twoPreviousColor;
                threePreviousWidth = twoPreviousWidth;
                twoPreviousColor = onePreviousColor;
                twoPreviousWidth = onePreviousWidth;
                onePreviousColor = presentColor;
                onePreviousWidth = presentWidth;
                threePreviousBrightness = twoPreviousBrightness;
                twoPreviousBrightness = onePreviousBrightness;
                onePreviousBrightness = presentBrightness;
                threePreviousObjectIndex = twoPreviousObjectIndex;
                twoPreviousObjectIndex = onePreviousObjectIndex;
                onePreviousObjectIndex = presentObjectIndex;
                checkForSynapseXY();
            } // for (x = 0; x < xDim; x++)
        } // for (z = 0; z < zDim; z++)
        System.out.println("Number of synapses found searching parallel to y axis = " + (numSynapses - previousNumSynapses));
        Preferences.debug("Number of synapses found searching parallel to y axis = " + (numSynapses - previousNumSynapses) + "\n");
        dataString += "\nNumber of synapses found searching parallel to y axis = " + (numSynapses - previousNumSynapses) + "\n";
        previousNumSynapses = numSynapses;
        
        // Search all lines parallel to z axis
        dataString += ("\nSearching parallel to z axis\n");
        for (y = 0; y < yDim; y++) {
            yPos = y * xDim;
            for (x = 0; x < xDim; x++) {
                pos = yPos + x;
                threePreviousColor = NONE;
                twoPreviousColor = NONE;
                onePreviousColor = NONE;
                presentColor = NONE;
                presentBrightColor = NONE;
                threePreviousWidth = 0;
                twoPreviousWidth = 0;
                onePreviousWidth = 0;
                presentWidth = 0;
                threePreviousBrightness = false;
                twoPreviousBrightness = false;
                onePreviousBrightness = false;
                presentBrightness = false;
                for (z = 0; z < zDim; z++) {
                    if ((buffer[z][pos] == presentColor) || (buffer[z][pos] == presentBrightColor)) {
                        presentWidth++;
                        if ((presentColor == BRIGHT_BLUE) || (presentColor == BLUE)) {
                            blueX = (blueStartX + x) >> 1;
                            blueY = (blueStartY + y) >> 1;
                            blueZ = (blueStartZ + z) >> 1;    
                        }
                        if ((!presentBrightness) && (buffer[z][pos] >= BRIGHT_RED)) {
                            presentBrightness = true;
                        }
                    }
                    else {
                        threePreviousColor = twoPreviousColor;
                        threePreviousWidth = twoPreviousWidth;
                        twoPreviousColor = onePreviousColor;
                        twoPreviousWidth = onePreviousWidth;
                        onePreviousColor = presentColor;
                        onePreviousWidth = presentWidth;
                        threePreviousBrightness = twoPreviousBrightness;
                        twoPreviousBrightness = onePreviousBrightness;
                        onePreviousBrightness = presentBrightness;
                        threePreviousObjectIndex = twoPreviousObjectIndex;
                        twoPreviousObjectIndex = onePreviousObjectIndex;
                        onePreviousObjectIndex = presentObjectIndex;
                        if (buffer[z][pos] < BRIGHT_RED) {
                            presentColor = buffer[z][pos];
                            if (buffer[z][pos] == NONE) {
                                presentBrightColor = NONE;
                            }
                            else {
                                presentBrightColor = presentColor + 3;
                            }
                            presentBrightness = false;
                        }
                        else {
                            presentBrightColor = buffer[z][pos];
                            presentColor = presentBrightColor - 3;
                            presentBrightness = true;
                            
                        }
                        presentWidth = 1;
                        checkForSynapseZ();
                        if ((presentColor == BRIGHT_BLUE) || (presentColor == BLUE)) {
                            blueStartX = x;
                            blueStartY = y;
                            blueStartZ = z;
                            blueX = x;
                            blueY = y;
                            blueZ = z; 
                            presentObjectIndex = colorObjects[z][pos];
                        }
                        else if ((presentColor == BRIGHT_RED) || (presentColor == RED)) {
                            if (bigBlueFraction) {
                                presentObjectIndex = colorObjects[z][pos];
                            }
                            else {
                                presentObjectIndex = 0;
                            }
                        }
                        else if ((presentColor == BRIGHT_GREEN) || (presentColor == GREEN)) {
                            if (bigBlueFraction) {
                                presentObjectIndex = colorObjects[z][pos];
                            }
                            else {
                                presentObjectIndex = 0;
                            }
                        }
                    }
                } // for (z = 0; z < zDim; z++)
                threePreviousColor = twoPreviousColor;
                threePreviousWidth = twoPreviousWidth;
                twoPreviousColor = onePreviousColor;
                twoPreviousWidth = onePreviousWidth;
                onePreviousColor = presentColor;
                onePreviousWidth = presentWidth;
                threePreviousBrightness = twoPreviousBrightness;
                twoPreviousBrightness = onePreviousBrightness;
                onePreviousBrightness = presentBrightness;
                threePreviousObjectIndex = twoPreviousObjectIndex;
                twoPreviousObjectIndex = onePreviousObjectIndex;
                onePreviousObjectIndex = presentObjectIndex;
                checkForSynapseZ();
            } // for (x = 0; x < xDim; x++)
        } // for (y = 0; y < yDim; y++)
        System.out.println("Number of synapses found searching parallel to z axis = " + (numSynapses - previousNumSynapses));
        Preferences.debug("Number of synapses found searching parallel to z axis = " + (numSynapses - previousNumSynapses) + "\n");
        dataString += "\nNumber of synapses found searching parallel to z axis = " + (numSynapses - previousNumSynapses) + "\n";
        previousNumSynapses = numSynapses;
        
        //Search all lines parallel to x = -y.
        dataString += ("\nSearching parallel to x = -y\n");
        for (z = 0; z < zDim; z++) {
            for (xStart = 0; xStart <= xDim - threeBandMinXY; xStart++) {
                threePreviousColor = NONE;
                twoPreviousColor = NONE;
                onePreviousColor = NONE;
                presentColor = NONE;
                presentBrightColor = NONE;
                threePreviousWidth = 0;
                twoPreviousWidth = 0;
                onePreviousWidth = 0;
                presentWidth = 0;
                threePreviousBrightness = false;
                twoPreviousBrightness = false;
                onePreviousBrightness = false;
                presentBrightness = false;
                for (x = xStart, y = yDim-1; (x <= xDim - 1) && (y >= 0); x++, y--) {
                    pos = y * xDim + x; 
                    if ((buffer[z][pos] == presentColor) || (buffer[z][pos] == presentBrightColor)) {
                        presentWidth++;
                        if ((presentColor == BRIGHT_BLUE) || (presentColor == BLUE)) {
                            blueX = (blueStartX + x) >> 1;
                            blueY = (blueStartY + y) >> 1;
                            blueZ = (blueStartZ + z) >> 1;    
                        }
                        if ((!presentBrightness) && (buffer[z][pos] >= BRIGHT_RED)) {
                            presentBrightness = true;
                        }
                    }
                    else {
                        threePreviousColor = twoPreviousColor;
                        threePreviousWidth = twoPreviousWidth;
                        twoPreviousColor = onePreviousColor;
                        twoPreviousWidth = onePreviousWidth;
                        onePreviousColor = presentColor;
                        onePreviousWidth = presentWidth;
                        threePreviousBrightness = twoPreviousBrightness;
                        twoPreviousBrightness = onePreviousBrightness;
                        onePreviousBrightness = presentBrightness;
                        threePreviousObjectIndex = twoPreviousObjectIndex;
                        twoPreviousObjectIndex = onePreviousObjectIndex;
                        onePreviousObjectIndex = presentObjectIndex;
                        if (buffer[z][pos] < BRIGHT_RED) {
                            presentColor = buffer[z][pos];
                            if (buffer[z][pos] == NONE) {
                                presentBrightColor = NONE;
                            }
                            else {
                                presentBrightColor = presentColor + 3;
                            }
                            presentBrightness = false;
                        }
                        else {
                            presentBrightColor = buffer[z][pos];
                            presentColor = presentBrightColor - 3;
                            presentBrightness = true;
                            
                        }
                        presentWidth = 1;
                        checkForSynapseXY();
                        if ((presentColor == BRIGHT_BLUE) || (presentColor == BLUE)) {
                            blueStartX = x;
                            blueStartY = y;
                            blueStartZ = z;
                            blueX = x;
                            blueY = y;
                            blueZ = z; 
                            presentObjectIndex = colorObjects[z][pos];
                        }
                        else if ((presentColor == BRIGHT_RED) || (presentColor == RED)) {
                            if (bigBlueFraction) {
                                presentObjectIndex = colorObjects[z][pos];
                            }
                            else {
                                presentObjectIndex = 0;
                            }
                        }
                        else if ((presentColor == BRIGHT_GREEN) || (presentColor == GREEN)) {
                            if (bigBlueFraction) {
                                presentObjectIndex = colorObjects[z][pos];
                            }
                            else {
                                presentObjectIndex = 0;
                            }
                        }
                    }
                } // for (x = xStart, y = yDim-1; (x <= xDim - 1) && (y >= 0); x++, y--)
                threePreviousColor = twoPreviousColor;
                threePreviousWidth = twoPreviousWidth;
                twoPreviousColor = onePreviousColor;
                twoPreviousWidth = onePreviousWidth;
                onePreviousColor = presentColor;
                onePreviousWidth = presentWidth;
                threePreviousBrightness = twoPreviousBrightness;
                twoPreviousBrightness = onePreviousBrightness;
                onePreviousBrightness = presentBrightness;
                threePreviousObjectIndex = twoPreviousObjectIndex;
                twoPreviousObjectIndex = onePreviousObjectIndex;
                onePreviousObjectIndex = presentObjectIndex;
                checkForSynapseXY();
            } // for (xStart = 0; xStart <= xDim - threeBandMinXY; xStart++)
            for (yStart = yDim - 2; yStart >= threeBandMinXY-1; yStart--) {
                threePreviousColor = NONE;
                twoPreviousColor = NONE;
                onePreviousColor = NONE;
                presentColor = NONE;
                presentBrightColor = NONE;
                threePreviousWidth = 0;
                twoPreviousWidth = 0;
                onePreviousWidth = 0;
                presentWidth = 0;
                threePreviousBrightness = false;
                twoPreviousBrightness = false;
                onePreviousBrightness = false;
                presentBrightness = false;
                for (x = 0, y = yStart; (x <= xDim - 1) && (y >= 0); x++, y--) {
                    pos = y * xDim + x;
                    if ((buffer[z][pos] == presentColor) || (buffer[z][pos] == presentBrightColor)) {
                        presentWidth++;
                        if ((presentColor == BRIGHT_BLUE) || (presentColor == BLUE)) {
                            blueX = (blueStartX + x) >> 1;
                            blueY = (blueStartY + y) >> 1;
                            blueZ = (blueStartZ + z) >> 1;    
                        }
                        if ((!presentBrightness) && (buffer[z][pos] >= BRIGHT_RED)) {
                            presentBrightness = true;
                        }
                    }
                    else {
                        threePreviousColor = twoPreviousColor;
                        threePreviousWidth = twoPreviousWidth;
                        twoPreviousColor = onePreviousColor;
                        twoPreviousWidth = onePreviousWidth;
                        onePreviousColor = presentColor;
                        onePreviousWidth = presentWidth;
                        threePreviousBrightness = twoPreviousBrightness;
                        twoPreviousBrightness = onePreviousBrightness;
                        onePreviousBrightness = presentBrightness;
                        threePreviousObjectIndex = twoPreviousObjectIndex;
                        twoPreviousObjectIndex = onePreviousObjectIndex;
                        onePreviousObjectIndex = presentObjectIndex;
                        if (buffer[z][pos] < BRIGHT_RED) {
                            presentColor = buffer[z][pos];
                            if (buffer[z][pos] == NONE) {
                                presentBrightColor = NONE;
                            }
                            else {
                                presentBrightColor = presentColor + 3;
                            }
                            presentBrightness = false;
                        }
                        else {
                            presentBrightColor = buffer[z][pos];
                            presentColor = presentBrightColor - 3;
                            presentBrightness = true;
                            
                        }
                        presentWidth = 1;
                        checkForSynapseXY(); 
                        if ((presentColor == BRIGHT_BLUE) || (presentColor == BLUE)) {
                            blueStartX = x;
                            blueStartY = y;
                            blueStartZ = z;
                            blueX = x;
                            blueY = y;
                            blueZ = z; 
                            presentObjectIndex = colorObjects[z][pos];
                        }
                        else if ((presentColor == BRIGHT_RED) || (presentColor == RED)) {
                            if (bigBlueFraction) {
                                presentObjectIndex = colorObjects[z][pos];
                            }
                            else {
                                presentObjectIndex = 0;
                            }
                        }
                        else if ((presentColor == BRIGHT_GREEN) || (presentColor == GREEN)) {
                            if (bigBlueFraction) {
                                presentObjectIndex = colorObjects[z][pos];
                            }
                            else {
                                presentObjectIndex = 0;
                            }
                        }
                    }
                } // for (x = 0, y = yStart; (x <= xDim - 1) && (y >= 0); x++, y--)
                threePreviousColor = twoPreviousColor;
                threePreviousWidth = twoPreviousWidth;
                twoPreviousColor = onePreviousColor;
                twoPreviousWidth = onePreviousWidth;
                onePreviousColor = presentColor;
                onePreviousWidth = presentWidth;
                threePreviousBrightness = twoPreviousBrightness;
                twoPreviousBrightness = onePreviousBrightness;
                onePreviousBrightness = presentBrightness;
                threePreviousObjectIndex = twoPreviousObjectIndex;
                twoPreviousObjectIndex = onePreviousObjectIndex;
                onePreviousObjectIndex = presentObjectIndex;
                checkForSynapseXY();
            } // for (yStart = yDim - 2; yStart >= threeBandMinXY-1; yStart--)
        } // for (z = 0; z < zDim; z++)
        System.out.println("Number of synapses found searching parallel to (x = -y) = " + (numSynapses - previousNumSynapses));
        Preferences.debug("Number of synapses found searching parallel to (x = -y) = " + (numSynapses - previousNumSynapses) + "\n");
        dataString += "\nNumber of synapses found searching parallel to (x = -y) = " + (numSynapses - previousNumSynapses) + "\n";
        previousNumSynapses = numSynapses;
        
        // Search all lines parallel to x = y.
        dataString += ("\nSearching parallel to x = y\n");
        for (z = 0; z < zDim; z++) {
            for (xStart = xDim - 1; xStart >= threeBandMinXY - 1; xStart--) {
                threePreviousColor = NONE;
                twoPreviousColor = NONE;
                onePreviousColor = NONE;
                presentColor = NONE;
                presentBrightColor = NONE;
                threePreviousWidth = 0;
                twoPreviousWidth = 0;
                onePreviousWidth = 0;
                presentWidth = 0;
                threePreviousBrightness = false;
                twoPreviousBrightness = false;
                onePreviousBrightness = false;
                presentBrightness = false;
                for (x = xStart, y = yDim - 1; (x >= 0) && (y >= 0); x--, y--) {
                    pos = y * xDim + x;
                    if ((buffer[z][pos] == presentColor) || (buffer[z][pos] == presentBrightColor)) {
                        presentWidth++;
                        if ((presentColor == BRIGHT_BLUE) || (presentColor == BLUE)) {
                            blueX = (blueStartX + x) >> 1;
                            blueY = (blueStartY + y) >> 1;
                            blueZ = (blueStartZ + z) >> 1;    
                        }
                        if ((!presentBrightness) && (buffer[z][pos] >= BRIGHT_RED)) {
                            presentBrightness = true;
                        }
                    }
                    else {
                        threePreviousColor = twoPreviousColor;
                        threePreviousWidth = twoPreviousWidth;
                        twoPreviousColor = onePreviousColor;
                        twoPreviousWidth = onePreviousWidth;
                        onePreviousColor = presentColor;
                        onePreviousWidth = presentWidth;
                        threePreviousBrightness = twoPreviousBrightness;
                        twoPreviousBrightness = onePreviousBrightness;
                        onePreviousBrightness = presentBrightness;
                        threePreviousObjectIndex = twoPreviousObjectIndex;
                        twoPreviousObjectIndex = onePreviousObjectIndex;
                        onePreviousObjectIndex = presentObjectIndex;
                        if (buffer[z][pos] < BRIGHT_RED) {
                            presentColor = buffer[z][pos];
                            if (buffer[z][pos] == NONE) {
                                presentBrightColor = NONE;
                            }
                            else {
                                presentBrightColor = presentColor + 3;
                            }
                            presentBrightness = false;
                        }
                        else {
                            presentBrightColor = buffer[z][pos];
                            presentColor = presentBrightColor - 3;
                            presentBrightness = true;
                            
                        }
                        presentWidth = 1;
                        checkForSynapseXY();
                        if ((presentColor == BRIGHT_BLUE) || (presentColor == BLUE)) {
                            blueStartX = x;
                            blueStartY = y;
                            blueStartZ = z;
                            blueX = x;
                            blueY = y;
                            blueZ = z; 
                            presentObjectIndex = colorObjects[z][pos];
                        }
                        else if ((presentColor == BRIGHT_RED) || (presentColor == RED)) {
                            if (bigBlueFraction) {
                                presentObjectIndex = colorObjects[z][pos];
                            }
                            else {
                                presentObjectIndex = 0;
                            }
                        }
                        else if ((presentColor == BRIGHT_GREEN) || (presentColor == GREEN)) {
                            if (bigBlueFraction) {
                                presentObjectIndex = colorObjects[z][pos];
                            }
                            else {
                                presentObjectIndex = 0;
                            }
                        }
                    }
                } // for (x = xStart, y = yDim - 1; (x >= 0) && (y >= 0); x--, y--)
                threePreviousColor = twoPreviousColor;
                threePreviousWidth = twoPreviousWidth;
                twoPreviousColor = onePreviousColor;
                twoPreviousWidth = onePreviousWidth;
                onePreviousColor = presentColor;
                onePreviousWidth = presentWidth;
                threePreviousBrightness = twoPreviousBrightness;
                twoPreviousBrightness = onePreviousBrightness;
                onePreviousBrightness = presentBrightness;
                threePreviousObjectIndex = twoPreviousObjectIndex;
                twoPreviousObjectIndex = onePreviousObjectIndex;
                onePreviousObjectIndex = presentObjectIndex;
                checkForSynapseXY();
            } // for (xStart = xDim - 1; xStart >= threeBandMinXY - 1; xStart--)
            for (yStart = yDim - 2; yStart >= threeBandMinXY - 1; yStart--) {
                threePreviousColor = NONE;
                twoPreviousColor = NONE;
                onePreviousColor = NONE;
                presentColor = NONE;
                presentBrightColor = NONE;
                threePreviousWidth = 0;
                twoPreviousWidth = 0;
                onePreviousWidth = 0;
                presentWidth = 0;
                threePreviousBrightness = false;
                twoPreviousBrightness = false;
                onePreviousBrightness = false;
                presentBrightness = false;
                for (x = xDim - 1, y = yStart; (x >= 0) && (y >= 0); x--, y--) {
                    pos = y * xDim + x;
                    if ((buffer[z][pos] == presentColor) || (buffer[z][pos] == presentBrightColor)) {
                        presentWidth++;
                        if ((presentColor == BRIGHT_BLUE) || (presentColor == BLUE)) {
                            blueX = (blueStartX + x) >> 1;
                            blueY = (blueStartY + y) >> 1;
                            blueZ = (blueStartZ + z) >> 1;    
                        }
                        if ((!presentBrightness) && (buffer[z][pos] >= BRIGHT_RED)) {
                            presentBrightness = true;
                        }
                    }
                    else {
                        threePreviousColor = twoPreviousColor;
                        threePreviousWidth = twoPreviousWidth;
                        twoPreviousColor = onePreviousColor;
                        twoPreviousWidth = onePreviousWidth;
                        onePreviousColor = presentColor;
                        onePreviousWidth = presentWidth;
                        threePreviousBrightness = twoPreviousBrightness;
                        twoPreviousBrightness = onePreviousBrightness;
                        onePreviousBrightness = presentBrightness;
                        threePreviousObjectIndex = twoPreviousObjectIndex;
                        twoPreviousObjectIndex = onePreviousObjectIndex;
                        onePreviousObjectIndex = presentObjectIndex;
                        if (buffer[z][pos] < BRIGHT_RED) {
                            presentColor = buffer[z][pos];
                            if (buffer[z][pos] == NONE) {
                                presentBrightColor = NONE;
                            }
                            else {
                                presentBrightColor = presentColor + 3;
                            }
                            presentBrightness = false;
                        }
                        else {
                            presentBrightColor = buffer[z][pos];
                            presentColor = presentBrightColor - 3;
                            presentBrightness = true;
                            
                        }
                        presentWidth = 1;
                        checkForSynapseXY(); 
                        if ((presentColor == BRIGHT_BLUE) || (presentColor == BLUE)) {
                            blueStartX = x;
                            blueStartY = y;
                            blueStartZ = z;
                            blueX = x;
                            blueY = y;
                            blueZ = z; 
                            presentObjectIndex = colorObjects[z][pos];
                        }
                        else if ((presentColor == BRIGHT_RED) || (presentColor == RED)) {
                            if (bigBlueFraction) {
                                presentObjectIndex = colorObjects[z][pos];
                            }
                            else {
                                presentObjectIndex = 0;
                            }
                        }
                        else if ((presentColor == BRIGHT_GREEN) || (presentColor == GREEN)) {
                            if (bigBlueFraction) {
                                presentObjectIndex = colorObjects[z][pos];
                            }
                            else {
                                presentObjectIndex = 0;
                            }
                        }
                    }
                } // for (x = xDim - 1, y = yStart; (x >= 0) && (y >= 0); x--, y--)
                threePreviousColor = twoPreviousColor;
                threePreviousWidth = twoPreviousWidth;
                twoPreviousColor = onePreviousColor;
                twoPreviousWidth = onePreviousWidth;
                onePreviousColor = presentColor;
                onePreviousWidth = presentWidth;
                threePreviousBrightness = twoPreviousBrightness;
                twoPreviousBrightness = onePreviousBrightness;
                onePreviousBrightness = presentBrightness;
                threePreviousObjectIndex = twoPreviousObjectIndex;
                twoPreviousObjectIndex = onePreviousObjectIndex;
                onePreviousObjectIndex = presentObjectIndex;
                checkForSynapseXY();
            } // for (yStart = yDim - 2; yStart >= threeBandMinXY - 1; yStart--)
        } // for (z = 0; z < zDim; z++)
        System.out.println("Number of synapses found searching parallel to (x = y) = " + (numSynapses - previousNumSynapses));
        Preferences.debug("Number of synapses found searching parallel to (x = y) = " + (numSynapses - previousNumSynapses) + "\n");
        dataString += "\nNumber of synapses found searching parallel to (x = y) = " + (numSynapses - previousNumSynapses) + "\n";
        previousNumSynapses = numSynapses;
        
        // Search all lines parallel to x = -z.
        dataString += ("\nSearching parallel to x = -z\n");
        for (y = 0; y < yDim; y++) {
            yPos = y * xDim;
            for (xStart = 0; xStart <= xDim - threeBandMinZ; xStart++) {
                threePreviousColor = NONE;
                twoPreviousColor = NONE;
                onePreviousColor = NONE;
                presentColor = NONE;
                presentBrightColor = NONE;
                threePreviousWidth = 0;
                twoPreviousWidth = 0;
                onePreviousWidth = 0;
                presentWidth = 0;
                threePreviousBrightness = false;
                twoPreviousBrightness = false;
                onePreviousBrightness = false;
                presentBrightness = false;
                for (x = xStart, z = zDim-1; (x <= xDim - 1) && (z >= 0); x++, z--) {
                    pos = yPos + x; 
                    if ((buffer[z][pos] == presentColor) || (buffer[z][pos] == presentBrightColor)) {
                        presentWidth++;
                        if ((presentColor == BRIGHT_BLUE) || (presentColor == BLUE)) {
                            blueX = (blueStartX + x) >> 1;
                            blueY = (blueStartY + y) >> 1;
                            blueZ = (blueStartZ + z) >> 1;    
                        }
                        if ((!presentBrightness) && (buffer[z][pos] >= BRIGHT_RED)) {
                            presentBrightness = true;
                        }
                    }
                    else {
                        threePreviousColor = twoPreviousColor;
                        threePreviousWidth = twoPreviousWidth;
                        twoPreviousColor = onePreviousColor;
                        twoPreviousWidth = onePreviousWidth;
                        onePreviousColor = presentColor;
                        onePreviousWidth = presentWidth;
                        threePreviousBrightness = twoPreviousBrightness;
                        twoPreviousBrightness = onePreviousBrightness;
                        onePreviousBrightness = presentBrightness;
                        threePreviousObjectIndex = twoPreviousObjectIndex;
                        twoPreviousObjectIndex = onePreviousObjectIndex;
                        onePreviousObjectIndex = presentObjectIndex;
                        if (buffer[z][pos] < BRIGHT_RED) {
                            presentColor = buffer[z][pos];
                            if (buffer[z][pos] == NONE) {
                                presentBrightColor = NONE;
                            }
                            else {
                                presentBrightColor = presentColor + 3;
                            }
                            presentBrightness = false;
                        }
                        else {
                            presentBrightColor = buffer[z][pos];
                            presentColor = presentBrightColor - 3;
                            presentBrightness = true;
                            
                        }
                        presentWidth = 1;
                        checkForSynapseZ();
                        if ((presentColor == BRIGHT_BLUE) || (presentColor == BLUE)) {
                            blueStartX = x;
                            blueStartY = y;
                            blueStartZ = z;
                            blueX = x;
                            blueY = y;
                            blueZ = z; 
                            presentObjectIndex = colorObjects[z][pos];
                        }
                        else if ((presentColor == BRIGHT_RED) || (presentColor == RED)) {
                            if (bigBlueFraction) {
                                presentObjectIndex = colorObjects[z][pos];
                            }
                            else {
                                presentObjectIndex = 0;
                            }
                        }
                        else if ((presentColor == BRIGHT_GREEN) || (presentColor == GREEN)) {
                            if (bigBlueFraction) {
                                presentObjectIndex = colorObjects[z][pos];
                            }
                            else {
                                presentObjectIndex = 0;
                            }
                        }
                    }
                } // for (x = xStart, z = zDim-1; (x <= xDim - 1) && (z >= 0); x++, z--)
                threePreviousColor = twoPreviousColor;
                threePreviousWidth = twoPreviousWidth;
                twoPreviousColor = onePreviousColor;
                twoPreviousWidth = onePreviousWidth;
                onePreviousColor = presentColor;
                onePreviousWidth = presentWidth;
                threePreviousBrightness = twoPreviousBrightness;
                twoPreviousBrightness = onePreviousBrightness;
                onePreviousBrightness = presentBrightness;
                threePreviousObjectIndex = twoPreviousObjectIndex;
                twoPreviousObjectIndex = onePreviousObjectIndex;
                onePreviousObjectIndex = presentObjectIndex;
                checkForSynapseZ();
            } // for (xStart = 0; xStart <= xDim - threeBandMinZ; xStart++)
            for (zStart = zDim - 2; zStart >= threeBandMinZ-1; zStart--) {
                threePreviousColor = NONE;
                twoPreviousColor = NONE;
                onePreviousColor = NONE;
                presentColor = NONE;
                presentBrightColor = NONE;
                threePreviousWidth = 0;
                twoPreviousWidth = 0;
                onePreviousWidth = 0;
                presentWidth = 0;
                threePreviousBrightness = false;
                twoPreviousBrightness = false;
                onePreviousBrightness = false;
                presentBrightness = false;
                for (x = 0, z = zStart; (x <= xDim - 1) && (z >= 0); x++, z--) {
                    pos = yPos + x; 
                    if ((buffer[z][pos] == presentColor) || (buffer[z][pos] == presentBrightColor)) {
                        presentWidth++;
                        if ((presentColor == BRIGHT_BLUE) || (presentColor == BLUE)) {
                            blueX = (blueStartX + x) >> 1;
                            blueY = (blueStartY + y) >> 1;
                            blueZ = (blueStartZ + z) >> 1;    
                        }
                        if ((!presentBrightness) && (buffer[z][pos] >= BRIGHT_RED)) {
                            presentBrightness = true;
                        }
                    }
                    else {
                        threePreviousColor = twoPreviousColor;
                        threePreviousWidth = twoPreviousWidth;
                        twoPreviousColor = onePreviousColor;
                        twoPreviousWidth = onePreviousWidth;
                        onePreviousColor = presentColor;
                        onePreviousWidth = presentWidth;
                        threePreviousBrightness = twoPreviousBrightness;
                        twoPreviousBrightness = onePreviousBrightness;
                        onePreviousBrightness = presentBrightness;
                        threePreviousObjectIndex = twoPreviousObjectIndex;
                        twoPreviousObjectIndex = onePreviousObjectIndex;
                        onePreviousObjectIndex = presentObjectIndex;
                        if (buffer[z][pos] < BRIGHT_RED) {
                            presentColor = buffer[z][pos];
                            if (buffer[z][pos] == NONE) {
                                presentBrightColor = NONE;
                            }
                            else {
                                presentBrightColor = presentColor + 3;
                            }
                            presentBrightness = false;
                        }
                        else {
                            presentBrightColor = buffer[z][pos];
                            presentColor = presentBrightColor - 3;
                            presentBrightness = true;
                            
                        }
                        presentWidth = 1;
                        checkForSynapseZ();
                        if ((presentColor == BRIGHT_BLUE) || (presentColor == BLUE)) {
                            blueStartX = x;
                            blueStartY = y;
                            blueStartZ = z;
                            blueX = x;
                            blueY = y;
                            blueZ = z; 
                            presentObjectIndex = colorObjects[z][pos];
                        }
                        else if ((presentColor == BRIGHT_RED) || (presentColor == RED)) {
                            if (bigBlueFraction) {
                                presentObjectIndex = colorObjects[z][pos];
                            }
                            else {
                                presentObjectIndex = 0;
                            }
                        }
                        else if ((presentColor == BRIGHT_GREEN) || (presentColor == GREEN)) {
                            if (bigBlueFraction) {
                                presentObjectIndex = colorObjects[z][pos];
                            }
                            else {
                                presentObjectIndex = 0;
                            }
                        }
                    }
                } // for (x = 0, z = zStart; (x <= xDim - 1) && (z >= 0); x++, z--)
                threePreviousColor = twoPreviousColor;
                threePreviousWidth = twoPreviousWidth;
                twoPreviousColor = onePreviousColor;
                twoPreviousWidth = onePreviousWidth;
                onePreviousColor = presentColor;
                onePreviousWidth = presentWidth;
                threePreviousBrightness = twoPreviousBrightness;
                twoPreviousBrightness = onePreviousBrightness;
                onePreviousBrightness = presentBrightness;
                threePreviousObjectIndex = twoPreviousObjectIndex;
                twoPreviousObjectIndex = onePreviousObjectIndex;
                onePreviousObjectIndex = presentObjectIndex;
                checkForSynapseZ();
            } // for (zStart = zDim - 2; zStart >= threeBandMinZ-1; zStart--)
        } // for (y = 0; y < yDim; y++)
        System.out.println("Number of synapses found searching parallel to (x = -z) = " + (numSynapses - previousNumSynapses));
        Preferences.debug("Number of synapses found searching parallel to (x = -z) = " + (numSynapses - previousNumSynapses) + "\n");
        dataString += "\nNumber of synapses found searching parallel to (x = -z) = " + (numSynapses - previousNumSynapses) + "\n";
        previousNumSynapses = numSynapses;
        
        // Search all lines parallel to x = z.
        dataString += ("\nSearching parallel to x = z\n");
        for (y = 0; y < yDim; y++) {
            yPos = y * xDim;
            for (xStart = xDim - 1; xStart >= threeBandMinZ - 1; xStart--) {
                threePreviousColor = NONE;
                twoPreviousColor = NONE;
                onePreviousColor = NONE;
                presentColor = NONE;
                presentBrightColor = NONE;
                threePreviousWidth = 0;
                twoPreviousWidth = 0;
                onePreviousWidth = 0;
                presentWidth = 0;
                threePreviousBrightness = false;
                twoPreviousBrightness = false;
                onePreviousBrightness = false;
                presentBrightness = false;
                for (x = xStart, z = zDim - 1; (x >= 0) && (z >= 0); x--, z--) {
                    pos = yPos + x;
                    if ((buffer[z][pos] == presentColor) || (buffer[z][pos] == presentBrightColor)) {
                        presentWidth++;
                        if ((presentColor == BRIGHT_BLUE) || (presentColor == BLUE)) {
                            blueX = (blueStartX + x) >> 1;
                            blueY = (blueStartY + y) >> 1;
                            blueZ = (blueStartZ + z) >> 1;    
                        }
                        if ((!presentBrightness) && (buffer[z][pos] >= BRIGHT_RED)) {
                            presentBrightness = true;
                        }
                    }
                    else {
                        threePreviousColor = twoPreviousColor;
                        threePreviousWidth = twoPreviousWidth;
                        twoPreviousColor = onePreviousColor;
                        twoPreviousWidth = onePreviousWidth;
                        onePreviousColor = presentColor;
                        onePreviousWidth = presentWidth;
                        threePreviousBrightness = twoPreviousBrightness;
                        twoPreviousBrightness = onePreviousBrightness;
                        onePreviousBrightness = presentBrightness;
                        threePreviousObjectIndex = twoPreviousObjectIndex;
                        twoPreviousObjectIndex = onePreviousObjectIndex;
                        onePreviousObjectIndex = presentObjectIndex;
                        if (buffer[z][pos] < BRIGHT_RED) {
                            presentColor = buffer[z][pos];
                            if (buffer[z][pos] == NONE) {
                                presentBrightColor = NONE;
                            }
                            else {
                                presentBrightColor = presentColor + 3;
                            }
                            presentBrightness = false;
                        }
                        else {
                            presentBrightColor = buffer[z][pos];
                            presentColor = presentBrightColor - 3;
                            presentBrightness = true;
                            
                        }
                        presentWidth = 1;
                        checkForSynapseZ(); 
                        if ((presentColor == BRIGHT_BLUE) || (presentColor == BLUE)) {
                            blueStartX = x;
                            blueStartY = y;
                            blueStartZ = z;
                            blueX = x;
                            blueY = y;
                            blueZ = z; 
                            presentObjectIndex = colorObjects[z][pos];
                        }
                        else if ((presentColor == BRIGHT_RED) || (presentColor == RED)) {
                            if (bigBlueFraction) {
                                presentObjectIndex = colorObjects[z][pos];
                            }
                            else {
                                presentObjectIndex = 0;
                            }
                        }
                        else if ((presentColor == BRIGHT_GREEN) || (presentColor == GREEN)) {
                            if (bigBlueFraction) {
                                presentObjectIndex = colorObjects[z][pos];
                            }
                            else {
                                presentObjectIndex = 0;
                            }
                        }
                    }
                } // for (x = xStart, z = zDim - 1; (x >= 0) && (z >= 0); x--, z--)
                threePreviousColor = twoPreviousColor;
                threePreviousWidth = twoPreviousWidth;
                twoPreviousColor = onePreviousColor;
                twoPreviousWidth = onePreviousWidth;
                onePreviousColor = presentColor;
                onePreviousWidth = presentWidth;
                threePreviousBrightness = twoPreviousBrightness;
                twoPreviousBrightness = onePreviousBrightness;
                onePreviousBrightness = presentBrightness;
                threePreviousObjectIndex = twoPreviousObjectIndex;
                twoPreviousObjectIndex = onePreviousObjectIndex;
                onePreviousObjectIndex = presentObjectIndex;
                checkForSynapseZ();
            } // for (xStart = xDim - 1; xStart >= threeBandMinZ - 1; xStart--)
            for (zStart = zDim - 2; zStart >= threeBandMinZ - 1; zStart--) {
                threePreviousColor = NONE;
                twoPreviousColor = NONE;
                onePreviousColor = NONE;
                presentColor = NONE;
                presentBrightColor = NONE;
                threePreviousWidth = 0;
                twoPreviousWidth = 0;
                onePreviousWidth = 0;
                presentWidth = 0;
                threePreviousBrightness = false;
                twoPreviousBrightness = false;
                onePreviousBrightness = false;
                presentBrightness = false;
                for (x = xDim - 1, z = zStart; (x >= 0) && (z >= 0); x--, z--) {
                    pos = yPos + x;
                    if ((buffer[z][pos] == presentColor) || (buffer[z][pos] == presentBrightColor)) {
                        presentWidth++;
                        if ((presentColor == BRIGHT_BLUE) || (presentColor == BLUE)) {
                            blueX = (blueStartX + x) >> 1;
                            blueY = (blueStartY + y) >> 1;
                            blueZ = (blueStartZ + z) >> 1;    
                        }
                        if ((!presentBrightness) && (buffer[z][pos] >= BRIGHT_RED)) {
                            presentBrightness = true;
                        }
                    }
                    else {
                        threePreviousColor = twoPreviousColor;
                        threePreviousWidth = twoPreviousWidth;
                        twoPreviousColor = onePreviousColor;
                        twoPreviousWidth = onePreviousWidth;
                        onePreviousColor = presentColor;
                        onePreviousWidth = presentWidth;
                        threePreviousBrightness = twoPreviousBrightness;
                        twoPreviousBrightness = onePreviousBrightness;
                        onePreviousBrightness = presentBrightness;
                        threePreviousObjectIndex = twoPreviousObjectIndex;
                        twoPreviousObjectIndex = onePreviousObjectIndex;
                        onePreviousObjectIndex = presentObjectIndex;
                        if (buffer[z][pos] < BRIGHT_RED) {
                            presentColor = buffer[z][pos];
                            if (buffer[z][pos] == NONE) {
                                presentBrightColor = NONE;
                            }
                            else {
                                presentBrightColor = presentColor + 3;
                            }
                            presentBrightness = false;
                        }
                        else {
                            presentBrightColor = buffer[z][pos];
                            presentColor = presentBrightColor - 3;
                            presentBrightness = true;
                            
                        }
                        presentWidth = 1;
                        checkForSynapseZ(); 
                        if ((presentColor == BRIGHT_BLUE) || (presentColor == BLUE)) {
                            blueStartX = x;
                            blueStartY = y;
                            blueStartZ = z;
                            blueX = x;
                            blueY = y;
                            blueZ = z; 
                            presentObjectIndex = colorObjects[z][pos];
                        }
                        else if ((presentColor == BRIGHT_RED) || (presentColor == RED)) {
                            if (bigBlueFraction) {
                                presentObjectIndex = colorObjects[z][pos];
                            }
                            else {
                                presentObjectIndex = 0;
                            }
                        }
                        else if ((presentColor == BRIGHT_GREEN) || (presentColor == GREEN)) {
                            if (bigBlueFraction) {
                                presentObjectIndex = colorObjects[z][pos];
                            }
                            else {
                                presentObjectIndex = 0;
                            }
                        }
                    }
                } // for (x = xDim - 1, z = zStart; (x >= 0) && (z >= 0); x--, z--)
                threePreviousColor = twoPreviousColor;
                threePreviousWidth = twoPreviousWidth;
                twoPreviousColor = onePreviousColor;
                twoPreviousWidth = onePreviousWidth;
                onePreviousColor = presentColor;
                onePreviousWidth = presentWidth;
                threePreviousBrightness = twoPreviousBrightness;
                twoPreviousBrightness = onePreviousBrightness;
                onePreviousBrightness = presentBrightness;
                threePreviousObjectIndex = twoPreviousObjectIndex;
                twoPreviousObjectIndex = onePreviousObjectIndex;
                onePreviousObjectIndex = presentObjectIndex;
                checkForSynapseZ();
            } // for (zStart = zDim - 2; zStart >= threeBandMinZ - 1; zStart--)
        } // for (y = 0; y < yDim; y++)
        System.out.println("Number of synapses found searching parallel to (x = z) = " + (numSynapses - previousNumSynapses));
        Preferences.debug("Number of synapses found searching parallel to (x = z) = " + (numSynapses - previousNumSynapses) + "\n");
        dataString += "\nNumber of synapses found searching parallel to (x = z) = " + (numSynapses - previousNumSynapses) + "\n";
        previousNumSynapses = numSynapses;
        
        // Search all lines parallel to y = -z.
        dataString += ("\nSearching parallel to y = -z\n");
        for (x = 0; x < xDim; x++) {
            for (yStart = 0; yStart <= yDim - threeBandMinZ; yStart++) {
                threePreviousColor = NONE;
                twoPreviousColor = NONE;
                onePreviousColor = NONE;
                presentColor = NONE;
                presentBrightColor = NONE;
                threePreviousWidth = 0;
                twoPreviousWidth = 0;
                onePreviousWidth = 0;
                presentWidth = 0;
                threePreviousBrightness = false;
                twoPreviousBrightness = false;
                onePreviousBrightness = false;
                presentBrightness = false;
                for (y = yStart, z = zDim-1; (y <= yDim - 1) && (z >= 0); y++, z--) {
                    pos = x + y * xDim;
                    if ((buffer[z][pos] == presentColor) || (buffer[z][pos] == presentBrightColor)) {
                        presentWidth++;
                        if ((presentColor == BRIGHT_BLUE) || (presentColor == BLUE)) {
                            blueX = (blueStartX + x) >> 1;
                            blueY = (blueStartY + y) >> 1;
                            blueZ = (blueStartZ + z) >> 1;    
                        }
                        if ((!presentBrightness) && (buffer[z][pos] >= BRIGHT_RED)) {
                            presentBrightness = true;
                        }
                    }
                    else {
                        threePreviousColor = twoPreviousColor;
                        threePreviousWidth = twoPreviousWidth;
                        twoPreviousColor = onePreviousColor;
                        twoPreviousWidth = onePreviousWidth;
                        onePreviousColor = presentColor;
                        onePreviousWidth = presentWidth;
                        threePreviousBrightness = twoPreviousBrightness;
                        twoPreviousBrightness = onePreviousBrightness;
                        onePreviousBrightness = presentBrightness;
                        threePreviousObjectIndex = twoPreviousObjectIndex;
                        twoPreviousObjectIndex = onePreviousObjectIndex;
                        onePreviousObjectIndex = presentObjectIndex;
                        if (buffer[z][pos] < BRIGHT_RED) {
                            presentColor = buffer[z][pos];
                            if (buffer[z][pos] == NONE) {
                                presentBrightColor = NONE;
                            }
                            else {
                                presentBrightColor = presentColor + 3;
                            }
                            presentBrightness = false;
                        }
                        else {
                            presentBrightColor = buffer[z][pos];
                            presentColor = presentBrightColor - 3;
                            presentBrightness = true;
                            
                        }
                        presentWidth = 1;
                        checkForSynapseZ(); 
                        if ((presentColor == BRIGHT_BLUE) || (presentColor == BLUE)) {
                            blueStartX = x;
                            blueStartY = y;
                            blueStartZ = z;
                            blueX = x;
                            blueY = y;
                            blueZ = z; 
                            presentObjectIndex = colorObjects[z][pos];
                        }
                        else if ((presentColor == BRIGHT_RED) || (presentColor == RED)) {
                            if (bigBlueFraction) {
                                presentObjectIndex = colorObjects[z][pos];
                            }
                            else {
                                presentObjectIndex = 0;
                            }
                        }
                        else if ((presentColor == BRIGHT_GREEN) || (presentColor == GREEN)) {
                            if (bigBlueFraction) {
                                presentObjectIndex = colorObjects[z][pos];
                            }
                            else {
                                presentObjectIndex = 0;
                            }
                        }
                    }
                } // for (y = yStart, z = zDim-1; (y <= yDim - 1) && (z >= 0); y++, z--)
                threePreviousColor = twoPreviousColor;
                threePreviousWidth = twoPreviousWidth;
                twoPreviousColor = onePreviousColor;
                twoPreviousWidth = onePreviousWidth;
                onePreviousColor = presentColor;
                onePreviousWidth = presentWidth;
                threePreviousBrightness = twoPreviousBrightness;
                twoPreviousBrightness = onePreviousBrightness;
                onePreviousBrightness = presentBrightness;
                threePreviousObjectIndex = twoPreviousObjectIndex;
                twoPreviousObjectIndex = onePreviousObjectIndex;
                onePreviousObjectIndex = presentObjectIndex;
                checkForSynapseZ();
            } // for (yStart = 0; yStart <= yDim - threeBandMinZ; yStart++)
            for (zStart = zDim - 2; zStart >= threeBandMinZ-1; zStart--) {
                threePreviousColor = NONE;
                twoPreviousColor = NONE;
                onePreviousColor = NONE;
                presentColor = NONE;
                presentBrightColor = NONE;
                threePreviousWidth = 0;
                twoPreviousWidth = 0;
                onePreviousWidth = 0;
                presentWidth = 0;
                threePreviousBrightness = false;
                twoPreviousBrightness = false;
                onePreviousBrightness = false;
                presentBrightness = false;
                for (y = 0, z = zStart; (y <= yDim - 1) && (z >= 0); y++, z--) {
                    pos = x + y * xDim;
                    if ((buffer[z][pos] == presentColor) || (buffer[z][pos] == presentBrightColor)) {
                        presentWidth++;
                        if ((presentColor == BRIGHT_BLUE) || (presentColor == BLUE)) {
                            blueX = (blueStartX + x) >> 1;
                            blueY = (blueStartY + y) >> 1;
                            blueZ = (blueStartZ + z) >> 1;    
                        }
                        if ((!presentBrightness) && (buffer[z][pos] >= BRIGHT_RED)) {
                            presentBrightness = true;
                        }
                    }
                    else {
                        threePreviousColor = twoPreviousColor;
                        threePreviousWidth = twoPreviousWidth;
                        twoPreviousColor = onePreviousColor;
                        twoPreviousWidth = onePreviousWidth;
                        onePreviousColor = presentColor;
                        onePreviousWidth = presentWidth;
                        threePreviousBrightness = twoPreviousBrightness;
                        twoPreviousBrightness = onePreviousBrightness;
                        onePreviousBrightness = presentBrightness;
                        threePreviousObjectIndex = twoPreviousObjectIndex;
                        twoPreviousObjectIndex = onePreviousObjectIndex;
                        onePreviousObjectIndex = presentObjectIndex;
                        if (buffer[z][pos] < BRIGHT_RED) {
                            presentColor = buffer[z][pos];
                            if (buffer[z][pos] == NONE) {
                                presentBrightColor = NONE;
                            }
                            else {
                                presentBrightColor = presentColor + 3;
                            }
                            presentBrightness = false;
                        }
                        else {
                            presentBrightColor = buffer[z][pos];
                            presentColor = presentBrightColor - 3;
                            presentBrightness = true;
                            
                        }
                        presentWidth = 1;
                        checkForSynapseZ();
                        if ((presentColor == BRIGHT_BLUE) || (presentColor == BLUE)) {
                            blueStartX = x;
                            blueStartY = y;
                            blueStartZ = z;
                            blueX = x;
                            blueY = y;
                            blueZ = z; 
                            presentObjectIndex = colorObjects[z][pos];
                        }
                        else if ((presentColor == BRIGHT_RED) || (presentColor == RED)) {
                            if (bigBlueFraction) {
                                presentObjectIndex = colorObjects[z][pos];
                            }
                            else {
                                presentObjectIndex = 0;
                            }
                        }
                        else if ((presentColor == BRIGHT_GREEN) || (presentColor == GREEN)) {
                            if (bigBlueFraction) {
                                presentObjectIndex = colorObjects[z][pos];
                            }
                            else {
                                presentObjectIndex = 0;
                            }
                        }
                    }
                } // for (y = 0, z = zStart; (y <= yDim - 1) && (z >= 0); y++, z--)
                threePreviousColor = twoPreviousColor;
                threePreviousWidth = twoPreviousWidth;
                twoPreviousColor = onePreviousColor;
                twoPreviousWidth = onePreviousWidth;
                onePreviousColor = presentColor;
                onePreviousWidth = presentWidth;
                threePreviousBrightness = twoPreviousBrightness;
                twoPreviousBrightness = onePreviousBrightness;
                onePreviousBrightness = presentBrightness;
                threePreviousObjectIndex = twoPreviousObjectIndex;
                twoPreviousObjectIndex = onePreviousObjectIndex;
                onePreviousObjectIndex = presentObjectIndex;
                checkForSynapseZ();
            } // for (zStart = zDim - 2; zStart >= threeBandMinZ-1; zStart--)
        } // for (x = 0; x < xDim; x++)
        System.out.println("Number of synapses found searching parallel to (y = -z) = " + (numSynapses - previousNumSynapses));
        Preferences.debug("Number of synapses found searching parallel to (y = -z) = " + (numSynapses - previousNumSynapses) + "\n");
        dataString += "\nNumber of synapses found searching parallel to (y = -z) = " + (numSynapses - previousNumSynapses) + "\n";
        previousNumSynapses = numSynapses;
        
        // Search all lines parallel to y = z.
        dataString += ("\nSearching parallel to y = z\n");
        for (x = 0; x < xDim; x++) {
            for (yStart = yDim - 1; yStart >= threeBandMinZ - 1; yStart--) {
                threePreviousColor = NONE;
                twoPreviousColor = NONE;
                onePreviousColor = NONE;
                presentColor = NONE;
                presentBrightColor = NONE;
                threePreviousWidth = 0;
                twoPreviousWidth = 0;
                onePreviousWidth = 0;
                presentWidth = 0;
                threePreviousBrightness = false;
                twoPreviousBrightness = false;
                onePreviousBrightness = false;
                presentBrightness = false;
                for (y = yStart, z = zDim - 1; (y >= 0) && (z >= 0); y--, z--) {
                    pos = x + y * xDim;
                    if ((buffer[z][pos] == presentColor) || (buffer[z][pos] == presentBrightColor)) {
                        presentWidth++;
                        if ((presentColor == BRIGHT_BLUE) || (presentColor == BLUE)) {
                            blueX = (blueStartX + x) >> 1;
                            blueY = (blueStartY + y) >> 1;
                            blueZ = (blueStartZ + z) >> 1;    
                        }
                        if ((!presentBrightness) && (buffer[z][pos] >= BRIGHT_RED)) {
                            presentBrightness = true;
                        }
                    }
                    else {
                        threePreviousColor = twoPreviousColor;
                        threePreviousWidth = twoPreviousWidth;
                        twoPreviousColor = onePreviousColor;
                        twoPreviousWidth = onePreviousWidth;
                        onePreviousColor = presentColor;
                        onePreviousWidth = presentWidth;
                        threePreviousBrightness = twoPreviousBrightness;
                        twoPreviousBrightness = onePreviousBrightness;
                        onePreviousBrightness = presentBrightness;
                        threePreviousObjectIndex = twoPreviousObjectIndex;
                        twoPreviousObjectIndex = onePreviousObjectIndex;
                        onePreviousObjectIndex = presentObjectIndex;
                        if (buffer[z][pos] < BRIGHT_RED) {
                            presentColor = buffer[z][pos];
                            if (buffer[z][pos] == NONE) {
                                presentBrightColor = NONE;
                            }
                            else {
                                presentBrightColor = presentColor + 3;
                            }
                            presentBrightness = false;
                        }
                        else {
                            presentBrightColor = buffer[z][pos];
                            presentColor = presentBrightColor - 3;
                            presentBrightness = true;
                            
                        }
                        presentWidth = 1;
                        checkForSynapseZ(); 
                        if ((presentColor == BRIGHT_BLUE) || (presentColor == BLUE)) {
                            blueStartX = x;
                            blueStartY = y;
                            blueStartZ = z;
                            blueX = x;
                            blueY = y;
                            blueZ = z; 
                            presentObjectIndex = colorObjects[z][pos];
                        }
                        else if ((presentColor == BRIGHT_RED) || (presentColor == RED)) {
                            if (bigBlueFraction) {
                                presentObjectIndex = colorObjects[z][pos];
                            }
                            else {
                                presentObjectIndex = 0;
                            }
                        }
                        else if ((presentColor == BRIGHT_GREEN) || (presentColor == GREEN)) {
                            if (bigBlueFraction) {
                                presentObjectIndex = colorObjects[z][pos];
                            }
                            else {
                                presentObjectIndex = 0;
                            }
                        }
                    }
                } // for (y = yStart, z = zDim - 1; (y >= 0) && (z >= 0); y--, z--)
                threePreviousColor = twoPreviousColor;
                threePreviousWidth = twoPreviousWidth;
                twoPreviousColor = onePreviousColor;
                twoPreviousWidth = onePreviousWidth;
                onePreviousColor = presentColor;
                onePreviousWidth = presentWidth;
                threePreviousBrightness = twoPreviousBrightness;
                twoPreviousBrightness = onePreviousBrightness;
                onePreviousBrightness = presentBrightness;
                threePreviousObjectIndex = twoPreviousObjectIndex;
                twoPreviousObjectIndex = onePreviousObjectIndex;
                onePreviousObjectIndex = presentObjectIndex;
                checkForSynapseZ();
            } // for (yStart = yDim - 1; yStart >= threeBandMinZ - 1; yStart--)
            for (zStart = zDim - 2; zStart >= threeBandMinZ - 1; zStart--) {
                threePreviousColor = NONE;
                twoPreviousColor = NONE;
                onePreviousColor = NONE;
                presentColor = NONE;
                presentBrightColor = NONE;
                threePreviousWidth = 0;
                twoPreviousWidth = 0;
                onePreviousWidth = 0;
                presentWidth = 0;
                threePreviousBrightness = false;
                twoPreviousBrightness = false;
                onePreviousBrightness = false;
                presentBrightness = false;
                for (y = yDim - 1, z = zStart; (y >= 0) && (z >= 0); y--, z--) {
                    pos = x + y * xDim;
                    if ((buffer[z][pos] == presentColor) || (buffer[z][pos] == presentBrightColor)) {
                        presentWidth++;
                        if ((presentColor == BRIGHT_BLUE) || (presentColor == BLUE)) {
                            blueX = (blueStartX + x) >> 1;
                            blueY = (blueStartY + y) >> 1;
                            blueZ = (blueStartZ + z) >> 1;    
                        }
                        if ((!presentBrightness) && (buffer[z][pos] >= BRIGHT_RED)) {
                            presentBrightness = true;
                        }
                    }
                    else {
                        threePreviousColor = twoPreviousColor;
                        threePreviousWidth = twoPreviousWidth;
                        twoPreviousColor = onePreviousColor;
                        twoPreviousWidth = onePreviousWidth;
                        onePreviousColor = presentColor;
                        onePreviousWidth = presentWidth;
                        threePreviousBrightness = twoPreviousBrightness;
                        twoPreviousBrightness = onePreviousBrightness;
                        onePreviousBrightness = presentBrightness;
                        threePreviousObjectIndex = twoPreviousObjectIndex;
                        twoPreviousObjectIndex = onePreviousObjectIndex;
                        onePreviousObjectIndex = presentObjectIndex;
                        if (buffer[z][pos] < BRIGHT_RED) {
                            presentColor = buffer[z][pos];
                            if (buffer[z][pos] == NONE) {
                                presentBrightColor = NONE;
                            }
                            else {
                                presentBrightColor = presentColor + 3;
                            }
                            presentBrightness = false;
                        }
                        else {
                            presentBrightColor = buffer[z][pos];
                            presentColor = presentBrightColor - 3;
                            presentBrightness = true;
                            
                        }
                        presentWidth = 1;
                        checkForSynapseZ(); 
                        if ((presentColor == BRIGHT_BLUE) || (presentColor == BLUE)) {
                            blueStartX = x;
                            blueStartY = y;
                            blueStartZ = z;
                            blueX = x;
                            blueY = y;
                            blueZ = z; 
                            presentObjectIndex = colorObjects[z][pos];
                        }
                        else if ((presentColor == BRIGHT_RED) || (presentColor == RED)) {
                            if (bigBlueFraction) {
                                presentObjectIndex = colorObjects[z][pos];
                            }
                            else {
                                presentObjectIndex = 0;
                            }
                        }
                        else if ((presentColor == BRIGHT_GREEN) || (presentColor == GREEN)) {
                            if (bigBlueFraction) {
                                presentObjectIndex = colorObjects[z][pos];
                            }
                            else {
                                presentObjectIndex = 0;
                            }
                        }
                    }
                } // for (y = yDim - 1, z = zStart; (y >= 0) && (z >= 0); y--, z--)
                threePreviousColor = twoPreviousColor;
                threePreviousWidth = twoPreviousWidth;
                twoPreviousColor = onePreviousColor;
                twoPreviousWidth = onePreviousWidth;
                onePreviousColor = presentColor;
                onePreviousWidth = presentWidth;
                threePreviousBrightness = twoPreviousBrightness;
                twoPreviousBrightness = onePreviousBrightness;
                onePreviousBrightness = presentBrightness;
                threePreviousObjectIndex = twoPreviousObjectIndex;
                twoPreviousObjectIndex = onePreviousObjectIndex;
                onePreviousObjectIndex = presentObjectIndex;
                checkForSynapseZ();
            } // for (zStart = zDim - 2; zStart >= threeBandMinZ - 1; zStart--)
        } // for (x = 0; x < xDim; x++)
        System.out.println("Number of synapses found searching parallel to (y = z) = " + (numSynapses - previousNumSynapses));
        Preferences.debug("Number of synapses found searching parallel to (y = z) = " + (numSynapses - previousNumSynapses) + "\n");
        dataString += "\nNumber of synapses found searching parallel to (y = z) = " + (numSynapses - previousNumSynapses) + "\n";
        previousNumSynapses = numSynapses;
        
        // Search all lines with delX = delY = delZ
        dataString += ("\nSearching parallel to delX = delY = delZ\n");
        for (zStart = 0; zStart < zDim; zStart++) {
            for (yStart = 0; yStart < yDim; yStart++) {
                threePreviousColor = NONE;
                twoPreviousColor = NONE;
                onePreviousColor = NONE;
                presentColor = NONE;
                presentBrightColor = NONE;
                threePreviousWidth = 0;
                twoPreviousWidth = 0;
                onePreviousWidth = 0;
                presentWidth = 0;
                threePreviousBrightness = false;
                twoPreviousBrightness = false;
                onePreviousBrightness = false;
                presentBrightness = false;
                for (x = 0, y = yStart, z = zStart; (x <= xDim-1) && (y <= yDim - 1) && (z <= zDim - 1); x++, y++, z++) {
                    pos = y * xDim + x;
                    if ((buffer[z][pos] == presentColor) || (buffer[z][pos] == presentBrightColor)) {
                        presentWidth++;
                        if ((presentColor == BRIGHT_BLUE) || (presentColor == BLUE)) {
                            blueX = (blueStartX + x) >> 1;
                            blueY = (blueStartY + y) >> 1;
                            blueZ = (blueStartZ + z) >> 1;    
                        }
                        if ((!presentBrightness) && (buffer[z][pos] >= BRIGHT_RED)) {
                            presentBrightness = true;
                        }
                    }
                    else {
                        threePreviousColor = twoPreviousColor;
                        threePreviousWidth = twoPreviousWidth;
                        twoPreviousColor = onePreviousColor;
                        twoPreviousWidth = onePreviousWidth;
                        onePreviousColor = presentColor;
                        onePreviousWidth = presentWidth;
                        threePreviousBrightness = twoPreviousBrightness;
                        twoPreviousBrightness = onePreviousBrightness;
                        onePreviousBrightness = presentBrightness;
                        threePreviousObjectIndex = twoPreviousObjectIndex;
                        twoPreviousObjectIndex = onePreviousObjectIndex;
                        onePreviousObjectIndex = presentObjectIndex;
                        if (buffer[z][pos] < BRIGHT_RED) {
                            presentColor = buffer[z][pos];
                            if (buffer[z][pos] == NONE) {
                                presentBrightColor = NONE;
                            }
                            else {
                                presentBrightColor = presentColor + 3;
                            }
                            presentBrightness = false;
                        }
                        else {
                            presentBrightColor = buffer[z][pos];
                            presentColor = presentBrightColor - 3;
                            presentBrightness = true;
                            
                        }
                        presentWidth = 1;
                        checkForSynapseZ(); 
                        if ((presentColor == BRIGHT_BLUE) || (presentColor == BLUE)) {
                            blueStartX = x;
                            blueStartY = y;
                            blueStartZ = z;
                            blueX = x;
                            blueY = y;
                            blueZ = z; 
                            presentObjectIndex = colorObjects[z][pos];
                        }
                        else if ((presentColor == BRIGHT_RED) || (presentColor == RED)) {
                            if (bigBlueFraction) {
                                presentObjectIndex = colorObjects[z][pos];
                            }
                            else {
                                presentObjectIndex = 0;
                            }
                        }
                        else if ((presentColor == BRIGHT_GREEN) || (presentColor == GREEN)) {
                            if (bigBlueFraction) {
                                presentObjectIndex = colorObjects[z][pos];
                            }
                            else {
                                presentObjectIndex = 0;
                            }
                        }
                    }
                } // for (x = 0, y = yStart, z = zStart; (x <= xDim-1) && (y <= yDim - 1) && (z <= zDim - 1); x++, y++, z++)
                threePreviousColor = twoPreviousColor;
                threePreviousWidth = twoPreviousWidth;
                twoPreviousColor = onePreviousColor;
                twoPreviousWidth = onePreviousWidth;
                onePreviousColor = presentColor;
                onePreviousWidth = presentWidth;
                threePreviousBrightness = twoPreviousBrightness;
                twoPreviousBrightness = onePreviousBrightness;
                onePreviousBrightness = presentBrightness;
                threePreviousObjectIndex = twoPreviousObjectIndex;
                twoPreviousObjectIndex = onePreviousObjectIndex;
                onePreviousObjectIndex = presentObjectIndex;
                checkForSynapseZ();
            } // for (yStart = 0; yStart < yDim; yStart++)
        } // for (zStart = 0; zStart < zDim; zStart++)
        for (zStart = 0; zStart < zDim; zStart++) {
            for (xStart = 0; xStart < xDim; xStart++) {
                threePreviousColor = NONE;
                twoPreviousColor = NONE;
                onePreviousColor = NONE;
                presentColor = NONE;
                presentBrightColor = NONE;
                threePreviousWidth = 0;
                twoPreviousWidth = 0;
                onePreviousWidth = 0;
                presentWidth = 0;
                threePreviousBrightness = false;
                twoPreviousBrightness = false;
                onePreviousBrightness = false;
                presentBrightness = false;
                for (x = xStart, y = 0, z = zStart; (x <= xDim-1) && (y <= yDim - 1) && (z <= zDim - 1); x++, y++, z++) {
                    pos = y * xDim + x;
                    if ((buffer[z][pos] == presentColor) || (buffer[z][pos] == presentBrightColor)) {
                        presentWidth++;
                        if ((presentColor == BRIGHT_BLUE) || (presentColor == BLUE)) {
                            blueX = (blueStartX + x) >> 1;
                            blueY = (blueStartY + y) >> 1;
                            blueZ = (blueStartZ + z) >> 1;    
                        }
                        if ((!presentBrightness) && (buffer[z][pos] >= BRIGHT_RED)) {
                            presentBrightness = true;
                        }
                    }
                    else {
                        threePreviousColor = twoPreviousColor;
                        threePreviousWidth = twoPreviousWidth;
                        twoPreviousColor = onePreviousColor;
                        twoPreviousWidth = onePreviousWidth;
                        onePreviousColor = presentColor;
                        onePreviousWidth = presentWidth;
                        threePreviousBrightness = twoPreviousBrightness;
                        twoPreviousBrightness = onePreviousBrightness;
                        onePreviousBrightness = presentBrightness;
                        threePreviousObjectIndex = twoPreviousObjectIndex;
                        twoPreviousObjectIndex = onePreviousObjectIndex;
                        onePreviousObjectIndex = presentObjectIndex;
                        if (buffer[z][pos] < BRIGHT_RED) {
                            presentColor = buffer[z][pos];
                            if (buffer[z][pos] == NONE) {
                                presentBrightColor = NONE;
                            }
                            else {
                                presentBrightColor = presentColor + 3;
                            }
                            presentBrightness = false;
                        }
                        else {
                            presentBrightColor = buffer[z][pos];
                            presentColor = presentBrightColor - 3;
                            presentBrightness = true;
                            
                        }
                        presentWidth = 1;
                        checkForSynapseZ();
                        if ((presentColor == BRIGHT_BLUE) || (presentColor == BLUE)) {
                            blueStartX = x;
                            blueStartY = y;
                            blueStartZ = z;
                            blueX = x;
                            blueY = y;
                            blueZ = z; 
                            presentObjectIndex = colorObjects[z][pos];
                        }
                        else if ((presentColor == BRIGHT_RED) || (presentColor == RED)) {
                            if (bigBlueFraction) {
                                presentObjectIndex = colorObjects[z][pos];
                            }
                            else {
                                presentObjectIndex = 0;
                            }
                        }
                        else if ((presentColor == BRIGHT_GREEN) || (presentColor == GREEN)) {
                            if (bigBlueFraction) {
                                presentObjectIndex = colorObjects[z][pos];
                            }
                            else {
                                presentObjectIndex = 0;
                            }
                        }
                    }
                } // for (x = xStart, y = 0, z = zStart; (x <= xDim-1) && (y <= yDim - 1) && (z <= zDim - 1); x++, y++, z++)
                threePreviousColor = twoPreviousColor;
                threePreviousWidth = twoPreviousWidth;
                twoPreviousColor = onePreviousColor;
                twoPreviousWidth = onePreviousWidth;
                onePreviousColor = presentColor;
                onePreviousWidth = presentWidth;
                threePreviousBrightness = twoPreviousBrightness;
                twoPreviousBrightness = onePreviousBrightness;
                onePreviousBrightness = presentBrightness;
                threePreviousObjectIndex = twoPreviousObjectIndex;
                twoPreviousObjectIndex = onePreviousObjectIndex;
                onePreviousObjectIndex = presentObjectIndex;
                checkForSynapseZ();
            } // for (xStart = 0; xStart < xDim; xStart++)
        } // for (zStart = 0; zStart < zDim; zStart++)
        for (yStart = 0; yStart < yDim; yStart++) {
            for (xStart = 0; xStart < xDim; xStart++) {
                threePreviousColor = NONE;
                twoPreviousColor = NONE;
                onePreviousColor = NONE;
                presentColor = NONE;
                presentBrightColor = NONE;
                threePreviousWidth = 0;
                twoPreviousWidth = 0;
                onePreviousWidth = 0;
                presentWidth = 0;
                threePreviousBrightness = false;
                twoPreviousBrightness = false;
                onePreviousBrightness = false;
                presentBrightness = false;
                for (x = xStart, y = yStart, z = 0; (x <= xDim-1) && (y <= yDim - 1) && (z <= zDim - 1); x++, y++, z++) {
                    pos = y * xDim + x;
                    if ((buffer[z][pos] == presentColor) || (buffer[z][pos] == presentBrightColor)) {
                        presentWidth++;
                        if ((presentColor == BRIGHT_BLUE) || (presentColor == BLUE)) {
                            blueX = (blueStartX + x) >> 1;
                            blueY = (blueStartY + y) >> 1;
                            blueZ = (blueStartZ + z) >> 1;    
                        }
                        if ((!presentBrightness) && (buffer[z][pos] >= BRIGHT_RED)) {
                            presentBrightness = true;
                        }
                    }
                    else {
                        threePreviousColor = twoPreviousColor;
                        threePreviousWidth = twoPreviousWidth;
                        twoPreviousColor = onePreviousColor;
                        twoPreviousWidth = onePreviousWidth;
                        onePreviousColor = presentColor;
                        onePreviousWidth = presentWidth;
                        threePreviousBrightness = twoPreviousBrightness;
                        twoPreviousBrightness = onePreviousBrightness;
                        onePreviousBrightness = presentBrightness;
                        threePreviousObjectIndex = twoPreviousObjectIndex;
                        twoPreviousObjectIndex = onePreviousObjectIndex;
                        onePreviousObjectIndex = presentObjectIndex;
                        if (buffer[z][pos] < BRIGHT_RED) {
                            presentColor = buffer[z][pos];
                            if (buffer[z][pos] == NONE) {
                                presentBrightColor = NONE;
                            }
                            else {
                                presentBrightColor = presentColor + 3;
                            }
                            presentBrightness = false;
                        }
                        else {
                            presentBrightColor = buffer[z][pos];
                            presentColor = presentBrightColor - 3;
                            presentBrightness = true;
                            
                        }
                        presentWidth = 1;
                        checkForSynapseZ();
                        if ((presentColor == BRIGHT_BLUE) || (presentColor == BLUE)) {
                            blueStartX = x;
                            blueStartY = y;
                            blueStartZ = z;
                            blueX = x;
                            blueY = y;
                            blueZ = z; 
                            presentObjectIndex = colorObjects[z][pos];
                        }
                        else if ((presentColor == BRIGHT_RED) || (presentColor == RED)) {
                            if (bigBlueFraction) {
                                presentObjectIndex = colorObjects[z][pos];
                            }
                            else {
                                presentObjectIndex = 0;
                            }
                        }
                        else if ((presentColor == BRIGHT_GREEN) || (presentColor == GREEN)) {
                            if (bigBlueFraction) {
                                presentObjectIndex = colorObjects[z][pos];
                            }
                            else {
                                presentObjectIndex = 0;
                            }
                        }
                    }
                } // for (x = xStart, y = yStart, z = 0; (x <= xDim-1) && (y <= yDim - 1) && (z <= zDim - 1); x++, y++, z++)
                threePreviousColor = twoPreviousColor;
                threePreviousWidth = twoPreviousWidth;
                twoPreviousColor = onePreviousColor;
                twoPreviousWidth = onePreviousWidth;
                onePreviousColor = presentColor;
                onePreviousWidth = presentWidth;
                threePreviousBrightness = twoPreviousBrightness;
                twoPreviousBrightness = onePreviousBrightness;
                onePreviousBrightness = presentBrightness;
                threePreviousObjectIndex = twoPreviousObjectIndex;
                twoPreviousObjectIndex = onePreviousObjectIndex;
                onePreviousObjectIndex = presentObjectIndex;
                checkForSynapseZ();
            } // for (xStart = 0; xStart < xDim; xStart++)
        } // for (yStart = 0; yStart < yDim; yStart++)
        System.out.println("Number of synapses found searching parallel to (delX = delY = delZ) = " + (numSynapses - previousNumSynapses));
        Preferences.debug("Number of synapses found searching parallel to (delX = delY = delZ) = " + (numSynapses - previousNumSynapses) + "\n");
        dataString += "\nNumber of synapses found searching parallel to (delX = delY = delZ) = " + (numSynapses - previousNumSynapses) + "\n";
        previousNumSynapses = numSynapses;
        
        // Search all lines with delX = delY = -delZ
        dataString += ("\nSearching parallel to delX = delY = -delZ\n");
        for (zStart = 0; zStart < zDim; zStart++) {
            for (yStart = 0; yStart < yDim; yStart++) {
                threePreviousColor = NONE;
                twoPreviousColor = NONE;
                onePreviousColor = NONE;
                presentColor = NONE;
                presentBrightColor = NONE;
                threePreviousWidth = 0;
                twoPreviousWidth = 0;
                onePreviousWidth = 0;
                presentWidth = 0;
                threePreviousBrightness = false;
                twoPreviousBrightness = false;
                onePreviousBrightness = false;
                presentBrightness = false;
                for (x = 0, y = yStart, z = zStart; (x <= xDim-1) && (y <= yDim - 1) && (z >= 0); x++, y++, z--) {
                    pos = y * xDim + x;
                    if ((buffer[z][pos] == presentColor) || (buffer[z][pos] == presentBrightColor)) {
                        presentWidth++;
                        if ((presentColor == BRIGHT_BLUE) || (presentColor == BLUE)) {
                            blueX = (blueStartX + x) >> 1;
                            blueY = (blueStartY + y) >> 1;
                            blueZ = (blueStartZ + z) >> 1;    
                        }
                        if ((!presentBrightness) && (buffer[z][pos] >= BRIGHT_RED)) {
                            presentBrightness = true;
                        }
                    }
                    else {
                        threePreviousColor = twoPreviousColor;
                        threePreviousWidth = twoPreviousWidth;
                        twoPreviousColor = onePreviousColor;
                        twoPreviousWidth = onePreviousWidth;
                        onePreviousColor = presentColor;
                        onePreviousWidth = presentWidth;
                        threePreviousBrightness = twoPreviousBrightness;
                        twoPreviousBrightness = onePreviousBrightness;
                        onePreviousBrightness = presentBrightness;
                        threePreviousObjectIndex = twoPreviousObjectIndex;
                        twoPreviousObjectIndex = onePreviousObjectIndex;
                        onePreviousObjectIndex = presentObjectIndex;
                        if (buffer[z][pos] < BRIGHT_RED) {
                            presentColor = buffer[z][pos];
                            if (buffer[z][pos] == NONE) {
                                presentBrightColor = NONE;
                            }
                            else {
                                presentBrightColor = presentColor + 3;
                            }
                            presentBrightness = false;
                        }
                        else {
                            presentBrightColor = buffer[z][pos];
                            presentColor = presentBrightColor - 3;
                            presentBrightness = true;
                            
                        }
                        presentWidth = 1;
                        checkForSynapseZ(); 
                        if ((presentColor == BRIGHT_BLUE) || (presentColor == BLUE)) {
                            blueStartX = x;
                            blueStartY = y;
                            blueStartZ = z;
                            blueX = x;
                            blueY = y;
                            blueZ = z; 
                            presentObjectIndex = colorObjects[z][pos];
                        }
                        else if ((presentColor == BRIGHT_RED) || (presentColor == RED)) {
                            if (bigBlueFraction) {
                                presentObjectIndex = colorObjects[z][pos];
                            }
                            else {
                                presentObjectIndex = 0;
                            }
                        }
                        else if ((presentColor == BRIGHT_GREEN) || (presentColor == GREEN)) {
                            if (bigBlueFraction) {
                                presentObjectIndex = colorObjects[z][pos];
                            }
                            else {
                                presentObjectIndex = 0;
                            }
                        }
                    }
                } // for (x = 0, y = yStart, z = zStart; (x <= xDim-1) && (y <= yDim - 1) && (z >= 0); x++, y++, z--)
                threePreviousColor = twoPreviousColor;
                threePreviousWidth = twoPreviousWidth;
                twoPreviousColor = onePreviousColor;
                twoPreviousWidth = onePreviousWidth;
                onePreviousColor = presentColor;
                onePreviousWidth = presentWidth;
                threePreviousBrightness = twoPreviousBrightness;
                twoPreviousBrightness = onePreviousBrightness;
                onePreviousBrightness = presentBrightness;
                threePreviousObjectIndex = twoPreviousObjectIndex;
                twoPreviousObjectIndex = onePreviousObjectIndex;
                onePreviousObjectIndex = presentObjectIndex;
                checkForSynapseZ();
            } // for (yStart = 0; yStart < yDim; yStart++)
        } // for (zStart = 0; zStart < zDim; zStart++)
        for (zStart = 0; zStart < zDim; zStart++) {
            for (xStart = 0; xStart < xDim; xStart++) {
                threePreviousColor = NONE;
                twoPreviousColor = NONE;
                onePreviousColor = NONE;
                presentColor = NONE;
                presentBrightColor = NONE;
                threePreviousWidth = 0;
                twoPreviousWidth = 0;
                onePreviousWidth = 0;
                presentWidth = 0;
                threePreviousBrightness = false;
                twoPreviousBrightness = false;
                onePreviousBrightness = false;
                presentBrightness = false;
                for (x = xStart, y = 0, z = zStart; (x <= xDim-1) && (y <= yDim - 1) && (z >= 0); x++, y++, z--) {
                    pos = y * xDim + x;
                    if ((buffer[z][pos] == presentColor) || (buffer[z][pos] == presentBrightColor)) {
                        presentWidth++;
                        if ((presentColor == BRIGHT_BLUE) || (presentColor == BLUE)) {
                            blueX = (blueStartX + x) >> 1;
                            blueY = (blueStartY + y) >> 1;
                            blueZ = (blueStartZ + z) >> 1;    
                        }
                        if ((!presentBrightness) && (buffer[z][pos] >= BRIGHT_RED)) {
                            presentBrightness = true;
                        }
                    }
                    else {
                        threePreviousColor = twoPreviousColor;
                        threePreviousWidth = twoPreviousWidth;
                        twoPreviousColor = onePreviousColor;
                        twoPreviousWidth = onePreviousWidth;
                        onePreviousColor = presentColor;
                        onePreviousWidth = presentWidth;
                        threePreviousBrightness = twoPreviousBrightness;
                        twoPreviousBrightness = onePreviousBrightness;
                        onePreviousBrightness = presentBrightness;
                        threePreviousObjectIndex = twoPreviousObjectIndex;
                        twoPreviousObjectIndex = onePreviousObjectIndex;
                        onePreviousObjectIndex = presentObjectIndex;
                        if (buffer[z][pos] < BRIGHT_RED) {
                            presentColor = buffer[z][pos];
                            if (buffer[z][pos] == NONE) {
                                presentBrightColor = NONE;
                            }
                            else {
                                presentBrightColor = presentColor + 3;
                            }
                            presentBrightness = false;
                        }
                        else {
                            presentBrightColor = buffer[z][pos];
                            presentColor = presentBrightColor - 3;
                            presentBrightness = true;
                            
                        }
                        presentWidth = 1;
                        checkForSynapseZ();
                        if ((presentColor == BRIGHT_BLUE) || (presentColor == BLUE)) {
                            blueStartX = x;
                            blueStartY = y;
                            blueStartZ = z;
                            blueX = x;
                            blueY = y;
                            blueZ = z; 
                            presentObjectIndex = colorObjects[z][pos];
                        }
                        else if ((presentColor == BRIGHT_RED) || (presentColor == RED)) {
                            if (bigBlueFraction) {
                                presentObjectIndex = colorObjects[z][pos];
                            }
                            else {
                                presentObjectIndex = 0;
                            }
                        }
                        else if ((presentColor == BRIGHT_GREEN) || (presentColor == GREEN)) {
                            if (bigBlueFraction) {
                                presentObjectIndex = colorObjects[z][pos];
                            }
                            else {
                                presentObjectIndex = 0;
                            }
                        }
                    }
                } // for (x = xStart, y = 0, z = zStart; (x <= xDim-1) && (y <= yDim - 1) && (z >= 0); x++, y++, z--)
                threePreviousColor = twoPreviousColor;
                threePreviousWidth = twoPreviousWidth;
                twoPreviousColor = onePreviousColor;
                twoPreviousWidth = onePreviousWidth;
                onePreviousColor = presentColor;
                onePreviousWidth = presentWidth;
                threePreviousBrightness = twoPreviousBrightness;
                twoPreviousBrightness = onePreviousBrightness;
                onePreviousBrightness = presentBrightness;
                threePreviousObjectIndex = twoPreviousObjectIndex;
                twoPreviousObjectIndex = onePreviousObjectIndex;
                onePreviousObjectIndex = presentObjectIndex;
                checkForSynapseZ();
            } // for (xStart = 0; xStart < xDim; xStart++)
        } // for (zStart = 0; zStart < zDim; zStart++)
        for (yStart = 0; yStart < yDim; yStart++) {
            for (xStart = 0; xStart < xDim; xStart++) {
                threePreviousColor = NONE;
                twoPreviousColor = NONE;
                onePreviousColor = NONE;
                presentColor = NONE;
                presentBrightColor = NONE;
                threePreviousWidth = 0;
                twoPreviousWidth = 0;
                onePreviousWidth = 0;
                presentWidth = 0;
                threePreviousBrightness = false;
                twoPreviousBrightness = false;
                onePreviousBrightness = false;
                presentBrightness = false;
                for (x = xStart, y = yStart, z = zDim-1; (x <= xDim-1) && (y <= yDim - 1) && (z >= 0); x++, y++, z--) {
                    pos = y * xDim + x;
                    if ((buffer[z][pos] == presentColor) || (buffer[z][pos] == presentBrightColor)) {
                        presentWidth++;
                        if ((presentColor == BRIGHT_BLUE) || (presentColor == BLUE)) {
                            blueX = (blueStartX + x) >> 1;
                            blueY = (blueStartY + y) >> 1;
                            blueZ = (blueStartZ + z) >> 1;    
                        }
                        if ((!presentBrightness) && (buffer[z][pos] >= BRIGHT_RED)) {
                            presentBrightness = true;
                        }
                    }
                    else {
                        threePreviousColor = twoPreviousColor;
                        threePreviousWidth = twoPreviousWidth;
                        twoPreviousColor = onePreviousColor;
                        twoPreviousWidth = onePreviousWidth;
                        onePreviousColor = presentColor;
                        onePreviousWidth = presentWidth;
                        threePreviousBrightness = twoPreviousBrightness;
                        twoPreviousBrightness = onePreviousBrightness;
                        onePreviousBrightness = presentBrightness;
                        threePreviousObjectIndex = twoPreviousObjectIndex;
                        twoPreviousObjectIndex = onePreviousObjectIndex;
                        onePreviousObjectIndex = presentObjectIndex;
                        if (buffer[z][pos] < BRIGHT_RED) {
                            presentColor = buffer[z][pos];
                            if (buffer[z][pos] == NONE) {
                                presentBrightColor = NONE;
                            }
                            else {
                                presentBrightColor = presentColor + 3;
                            }
                            presentBrightness = false;
                        }
                        else {
                            presentBrightColor = buffer[z][pos];
                            presentColor = presentBrightColor - 3;
                            presentBrightness = true;
                            
                        }
                        presentWidth = 1;
                        checkForSynapseZ(); 
                        if ((presentColor == BRIGHT_BLUE) || (presentColor == BLUE)) {
                            blueStartX = x;
                            blueStartY = y;
                            blueStartZ = z;
                            blueX = x;
                            blueY = y;
                            blueZ = z; 
                            presentObjectIndex = colorObjects[z][pos];
                        }
                        else if ((presentColor == BRIGHT_RED) || (presentColor == RED)) {
                            if (bigBlueFraction) {
                                presentObjectIndex = colorObjects[z][pos];
                            }
                            else {
                                presentObjectIndex = 0;
                            }
                        }
                        else if ((presentColor == BRIGHT_GREEN) || (presentColor == GREEN)) {
                            if (bigBlueFraction) {
                                presentObjectIndex = colorObjects[z][pos];
                            }
                            else {
                                presentObjectIndex = 0;
                            }
                        }
                    }
                } // for (x = xStart, y = yStart, z = zDim-1; (x <= xDim-1) && (y <= yDim - 1) && (z >= 0); x++, y++, z--)
                threePreviousColor = twoPreviousColor;
                threePreviousWidth = twoPreviousWidth;
                twoPreviousColor = onePreviousColor;
                twoPreviousWidth = onePreviousWidth;
                onePreviousColor = presentColor;
                onePreviousWidth = presentWidth;
                threePreviousBrightness = twoPreviousBrightness;
                twoPreviousBrightness = onePreviousBrightness;
                onePreviousBrightness = presentBrightness;
                threePreviousObjectIndex = twoPreviousObjectIndex;
                twoPreviousObjectIndex = onePreviousObjectIndex;
                onePreviousObjectIndex = presentObjectIndex;
                checkForSynapseZ();
            } // for (xStart = 0; xStart < xDim; xStart++)
        } // for (yStart = 0; yStart < yDim; yStart++)
        System.out.println("Number of synapses found searching parallel to (delX = delY = -delZ) = " + (numSynapses - previousNumSynapses));
        Preferences.debug("Number of synapses found searching parallel to (delX = delY = -delZ) = " + (numSynapses - previousNumSynapses) + "\n");
        dataString += "\nNumber of synapses found searching parallel to (delX = delY = -delZ) = " + (numSynapses - previousNumSynapses) + "\n";
        previousNumSynapses = numSynapses;
        
        // Search all lines with delX = -delY = delZ
        dataString += ("\nSearching parallel to delX = -delY = delZ\n");
        for (zStart = 0; zStart < zDim; zStart++) {
            for (yStart = 0; yStart < yDim; yStart++) {
                threePreviousColor = NONE;
                twoPreviousColor = NONE;
                onePreviousColor = NONE;
                presentColor = NONE;
                presentBrightColor = NONE;
                threePreviousWidth = 0;
                twoPreviousWidth = 0;
                onePreviousWidth = 0;
                presentWidth = 0;
                threePreviousBrightness = false;
                twoPreviousBrightness = false;
                onePreviousBrightness = false;
                presentBrightness = false;
                for (x = 0, y = yStart, z = zStart; (x <= xDim-1) && (y >= 0) && (z <= zDim - 1); x++, y--, z++) {
                    pos = y * xDim + x;
                    if ((buffer[z][pos] == presentColor) || (buffer[z][pos] == presentBrightColor)) {
                        presentWidth++;
                        if ((presentColor == BRIGHT_BLUE) || (presentColor == BLUE)) {
                            blueX = (blueStartX + x) >> 1;
                            blueY = (blueStartY + y) >> 1;
                            blueZ = (blueStartZ + z) >> 1;    
                        }
                        if ((!presentBrightness) && (buffer[z][pos] >= BRIGHT_RED)) {
                            presentBrightness = true;
                        }
                    }
                    else {
                        threePreviousColor = twoPreviousColor;
                        threePreviousWidth = twoPreviousWidth;
                        twoPreviousColor = onePreviousColor;
                        twoPreviousWidth = onePreviousWidth;
                        onePreviousColor = presentColor;
                        onePreviousWidth = presentWidth;
                        threePreviousBrightness = twoPreviousBrightness;
                        twoPreviousBrightness = onePreviousBrightness;
                        onePreviousBrightness = presentBrightness;
                        threePreviousObjectIndex = twoPreviousObjectIndex;
                        twoPreviousObjectIndex = onePreviousObjectIndex;
                        onePreviousObjectIndex = presentObjectIndex;
                        if (buffer[z][pos] < BRIGHT_RED) {
                            presentColor = buffer[z][pos];
                            if (buffer[z][pos] == NONE) {
                                presentBrightColor = NONE;
                            }
                            else {
                                presentBrightColor = presentColor + 3;
                            }
                            presentBrightness = false;
                        }
                        else {
                            presentBrightColor = buffer[z][pos];
                            presentColor = presentBrightColor - 3;
                            presentBrightness = true;
                            
                        }
                        presentWidth = 1;
                        checkForSynapseZ();
                        if ((presentColor == BRIGHT_BLUE) || (presentColor == BLUE)) {
                            blueStartX = x;
                            blueStartY = y;
                            blueStartZ = z;
                            blueX = x;
                            blueY = y;
                            blueZ = z; 
                            presentObjectIndex = colorObjects[z][pos];
                        }
                        else if ((presentColor == BRIGHT_RED) || (presentColor == RED)) {
                            if (bigBlueFraction) {
                                presentObjectIndex = colorObjects[z][pos];
                            }
                            else {
                                presentObjectIndex = 0;
                            }
                        }
                        else if ((presentColor == BRIGHT_GREEN) || (presentColor == GREEN)) {
                            if (bigBlueFraction) {
                                presentObjectIndex = colorObjects[z][pos];
                            }
                            else {
                                presentObjectIndex = 0;
                            }
                        }
                    }
                } // for (x = 0, y = yStart, z = zStart; (x <= xDim-1) && (y >= 0) && (z <= zDim - 1); x++, y--, z++)
                threePreviousColor = twoPreviousColor;
                threePreviousWidth = twoPreviousWidth;
                twoPreviousColor = onePreviousColor;
                twoPreviousWidth = onePreviousWidth;
                onePreviousColor = presentColor;
                onePreviousWidth = presentWidth;
                threePreviousBrightness = twoPreviousBrightness;
                twoPreviousBrightness = onePreviousBrightness;
                onePreviousBrightness = presentBrightness;
                threePreviousObjectIndex = twoPreviousObjectIndex;
                twoPreviousObjectIndex = onePreviousObjectIndex;
                onePreviousObjectIndex = presentObjectIndex;
                checkForSynapseZ();
            } // for (yStart = 0; yStart < yDim; yStart++)
        } // for (zStart = 0; zStart < zDim; zStart++)
        for (zStart = 0; zStart < zDim; zStart++) {
            for (xStart = 0; xStart < xDim; xStart++) {
                threePreviousColor = NONE;
                twoPreviousColor = NONE;
                onePreviousColor = NONE;
                presentColor = NONE;
                presentBrightColor = NONE;
                threePreviousWidth = 0;
                twoPreviousWidth = 0;
                onePreviousWidth = 0;
                presentWidth = 0;
                threePreviousBrightness = false;
                twoPreviousBrightness = false;
                onePreviousBrightness = false;
                presentBrightness = false;
                for (x = xStart, y = yDim-1, z = zStart; (x <= xDim-1) && (y >= 0) && (z <= zDim - 1); x++, y--, z++) {
                    pos = y * xDim + x;
                    if ((buffer[z][pos] == presentColor) || (buffer[z][pos] == presentBrightColor)) {
                        presentWidth++;
                        if ((presentColor == BRIGHT_BLUE) || (presentColor == BLUE)) {
                            blueX = (blueStartX + x) >> 1;
                            blueY = (blueStartY + y) >> 1;
                            blueZ = (blueStartZ + z) >> 1;    
                        }
                        if ((!presentBrightness) && (buffer[z][pos] >= BRIGHT_RED)) {
                            presentBrightness = true;
                        }
                    }
                    else {
                        threePreviousColor = twoPreviousColor;
                        threePreviousWidth = twoPreviousWidth;
                        twoPreviousColor = onePreviousColor;
                        twoPreviousWidth = onePreviousWidth;
                        onePreviousColor = presentColor;
                        onePreviousWidth = presentWidth;
                        threePreviousBrightness = twoPreviousBrightness;
                        twoPreviousBrightness = onePreviousBrightness;
                        onePreviousBrightness = presentBrightness;
                        threePreviousObjectIndex = twoPreviousObjectIndex;
                        twoPreviousObjectIndex = onePreviousObjectIndex;
                        onePreviousObjectIndex = presentObjectIndex;
                        if (buffer[z][pos] < BRIGHT_RED) {
                            presentColor = buffer[z][pos];
                            if (buffer[z][pos] == NONE) {
                                presentBrightColor = NONE;
                            }
                            else {
                                presentBrightColor = presentColor + 3;
                            }
                            presentBrightness = false;
                        }
                        else {
                            presentBrightColor = buffer[z][pos];
                            presentColor = presentBrightColor - 3;
                            presentBrightness = true;
                            
                        }
                        presentWidth = 1;
                        checkForSynapseZ();
                        if ((presentColor == BRIGHT_BLUE) || (presentColor == BLUE)) {
                            blueStartX = x;
                            blueStartY = y;
                            blueStartZ = z;
                            blueX = x;
                            blueY = y;
                            blueZ = z; 
                            presentObjectIndex = colorObjects[z][pos];
                        }
                        else if ((presentColor == BRIGHT_RED) || (presentColor == RED)) {
                            if (bigBlueFraction) {
                                presentObjectIndex = colorObjects[z][pos];
                            }
                            else {
                                presentObjectIndex = 0;
                            }
                        }
                        else if ((presentColor == BRIGHT_GREEN) || (presentColor == GREEN)) {
                            if (bigBlueFraction) {
                                presentObjectIndex = colorObjects[z][pos];
                            }
                            else {
                                presentObjectIndex = 0;
                            }
                        }
                    }
                } // for (x = xStart, y = yDim-1, z = zStart; (x <= xDim-1) && (y >= 0) && (z <= zDim - 1); x++, y--, z++)
                threePreviousColor = twoPreviousColor;
                threePreviousWidth = twoPreviousWidth;
                twoPreviousColor = onePreviousColor;
                twoPreviousWidth = onePreviousWidth;
                onePreviousColor = presentColor;
                onePreviousWidth = presentWidth;
                threePreviousBrightness = twoPreviousBrightness;
                twoPreviousBrightness = onePreviousBrightness;
                onePreviousBrightness = presentBrightness;
                threePreviousObjectIndex = twoPreviousObjectIndex;
                twoPreviousObjectIndex = onePreviousObjectIndex;
                onePreviousObjectIndex = presentObjectIndex;
                checkForSynapseZ();
            } // for (xStart = 0; xStart < xDim; xStart++)
        } // for (zStart = 0; zStart < zDim; zStart++)
        for (yStart = 0; yStart < yDim; yStart++) {
            for (xStart = 0; xStart < xDim; xStart++) {
                threePreviousColor = NONE;
                twoPreviousColor = NONE;
                onePreviousColor = NONE;
                presentColor = NONE;
                presentBrightColor = NONE;
                threePreviousWidth = 0;
                twoPreviousWidth = 0;
                onePreviousWidth = 0;
                presentWidth = 0;
                threePreviousBrightness = false;
                twoPreviousBrightness = false;
                onePreviousBrightness = false;
                presentBrightness = false;
                for (x = xStart, y = yStart, z = 0; (x <= xDim-1) && (y >= 0) && (z <= zDim - 1); x++, y--, z++) {
                    pos = y * xDim + x;
                    if ((buffer[z][pos] == presentColor) || (buffer[z][pos] == presentBrightColor)) {
                        presentWidth++;
                        if ((presentColor == BRIGHT_BLUE) || (presentColor == BLUE)) {
                            blueX = (blueStartX + x) >> 1;
                            blueY = (blueStartY + y) >> 1;
                            blueZ = (blueStartZ + z) >> 1;    
                        }
                        if ((!presentBrightness) && (buffer[z][pos] >= BRIGHT_RED)) {
                            presentBrightness = true;
                        }
                    }
                    else {
                        threePreviousColor = twoPreviousColor;
                        threePreviousWidth = twoPreviousWidth;
                        twoPreviousColor = onePreviousColor;
                        twoPreviousWidth = onePreviousWidth;
                        onePreviousColor = presentColor;
                        onePreviousWidth = presentWidth;
                        threePreviousBrightness = twoPreviousBrightness;
                        twoPreviousBrightness = onePreviousBrightness;
                        onePreviousBrightness = presentBrightness;
                        threePreviousObjectIndex = twoPreviousObjectIndex;
                        twoPreviousObjectIndex = onePreviousObjectIndex;
                        onePreviousObjectIndex = presentObjectIndex;
                        if (buffer[z][pos] < BRIGHT_RED) {
                            presentColor = buffer[z][pos];
                            if (buffer[z][pos] == NONE) {
                                presentBrightColor = NONE;
                            }
                            else {
                                presentBrightColor = presentColor + 3;
                            }
                            presentBrightness = false;
                        }
                        else {
                            presentBrightColor = buffer[z][pos];
                            presentColor = presentBrightColor - 3;
                            presentBrightness = true;
                            
                        }
                        presentWidth = 1;
                        checkForSynapseZ();
                        if ((presentColor == BRIGHT_BLUE) || (presentColor == BLUE)) {
                            blueStartX = x;
                            blueStartY = y;
                            blueStartZ = z;
                            blueX = x;
                            blueY = y;
                            blueZ = z; 
                            presentObjectIndex = colorObjects[z][pos];
                        }
                        else if ((presentColor == BRIGHT_RED) || (presentColor == RED)) {
                            if (bigBlueFraction) {
                                presentObjectIndex = colorObjects[z][pos];
                            }
                            else {
                                presentObjectIndex = 0;
                            }
                        }
                        else if ((presentColor == BRIGHT_GREEN) || (presentColor == GREEN)) {
                            if (bigBlueFraction) {
                                presentObjectIndex = colorObjects[z][pos];
                            }
                            else {
                                presentObjectIndex = 0;
                            }
                        }
                    }
                } // for (x = xStart, y = yStart, z = 0; (x <= xDim-1) && (y >= 0) && (z <= zDim - 1); x++, y--, z++)
                threePreviousColor = twoPreviousColor;
                threePreviousWidth = twoPreviousWidth;
                twoPreviousColor = onePreviousColor;
                twoPreviousWidth = onePreviousWidth;
                onePreviousColor = presentColor;
                onePreviousWidth = presentWidth;
                threePreviousBrightness = twoPreviousBrightness;
                twoPreviousBrightness = onePreviousBrightness;
                onePreviousBrightness = presentBrightness;
                threePreviousObjectIndex = twoPreviousObjectIndex;
                twoPreviousObjectIndex = onePreviousObjectIndex;
                onePreviousObjectIndex = presentObjectIndex;
                checkForSynapseZ();
            } // for (xStart = 0; xStart < xDim; xStart++)
        } // for (yStart = 0; yStart < yDim; yStart++)
        System.out.println("Number of synapses found searching parallel to (delX = -delY = delZ) = " + (numSynapses - previousNumSynapses));
        Preferences.debug("Number of synapses found searching parallel to (delX = -delY = delZ) = " + (numSynapses - previousNumSynapses) + "\n");
        dataString += "\nNumber of synapses found searching parallel to (delX = -delY = delZ) = " + (numSynapses - previousNumSynapses) + "\n";
        previousNumSynapses = numSynapses;
        
        // Search all lines with -delX = delY = delZ
        dataString += ("\nSearching parallel to -delX = delY = delZ\n");
        for (zStart = 0; zStart < zDim; zStart++) {
            for (yStart = 0; yStart < yDim; yStart++) {
                threePreviousColor = NONE;
                twoPreviousColor = NONE;
                onePreviousColor = NONE;
                presentColor = NONE;
                presentBrightColor = NONE;
                threePreviousWidth = 0;
                twoPreviousWidth = 0;
                onePreviousWidth = 0;
                presentWidth = 0;
                threePreviousBrightness = false;
                twoPreviousBrightness = false;
                onePreviousBrightness = false;
                presentBrightness = false;
                for (x = xDim-1, y = yStart, z = zStart; (x >= 0) && (y <= yDim - 1) && (z <= zDim - 1); x--, y++, z++) {
                    pos = y * xDim + x;
                    if ((buffer[z][pos] == presentColor) || (buffer[z][pos] == presentBrightColor)) {
                        presentWidth++;
                        if ((presentColor == BRIGHT_BLUE) || (presentColor == BLUE)) {
                            blueX = (blueStartX + x) >> 1;
                            blueY = (blueStartY + y) >> 1;
                            blueZ = (blueStartZ + z) >> 1;    
                        }
                        if ((!presentBrightness) && (buffer[z][pos] >= BRIGHT_RED)) {
                            presentBrightness = true;
                        }
                    }
                    else {
                        threePreviousColor = twoPreviousColor;
                        threePreviousWidth = twoPreviousWidth;
                        twoPreviousColor = onePreviousColor;
                        twoPreviousWidth = onePreviousWidth;
                        onePreviousColor = presentColor;
                        onePreviousWidth = presentWidth;
                        threePreviousBrightness = twoPreviousBrightness;
                        twoPreviousBrightness = onePreviousBrightness;
                        onePreviousBrightness = presentBrightness;
                        threePreviousObjectIndex = twoPreviousObjectIndex;
                        twoPreviousObjectIndex = onePreviousObjectIndex;
                        onePreviousObjectIndex = presentObjectIndex;
                        if (buffer[z][pos] < BRIGHT_RED) {
                            presentColor = buffer[z][pos];
                            if (buffer[z][pos] == NONE) {
                                presentBrightColor = NONE;
                            }
                            else {
                                presentBrightColor = presentColor + 3;
                            }
                            presentBrightness = false;
                        }
                        else {
                            presentBrightColor = buffer[z][pos];
                            presentColor = presentBrightColor - 3;
                            presentBrightness = true;
                            
                        }
                        presentWidth = 1;
                        checkForSynapseZ();
                        if ((presentColor == BRIGHT_BLUE) || (presentColor == BLUE)) {
                            blueStartX = x;
                            blueStartY = y;
                            blueStartZ = z;
                            blueX = x;
                            blueY = y;
                            blueZ = z; 
                            presentObjectIndex = colorObjects[z][pos];
                        }
                        else if ((presentColor == BRIGHT_RED) || (presentColor == RED)) {
                            if (bigBlueFraction) {
                                presentObjectIndex = colorObjects[z][pos];
                            }
                            else {
                                presentObjectIndex = 0;
                            }
                        }
                        else if ((presentColor == BRIGHT_GREEN) || (presentColor == GREEN)) {
                            if (bigBlueFraction) {
                                presentObjectIndex = colorObjects[z][pos];
                            }
                            else {
                                presentObjectIndex = 0;
                            }
                        }
                    }
                } // for (x = xDim-1, y = yStart, z = zStart; (x >= 0) && (y <= yDim - 1) && (z <= zDim - 1); x--, y++, z++)
                threePreviousColor = twoPreviousColor;
                threePreviousWidth = twoPreviousWidth;
                twoPreviousColor = onePreviousColor;
                twoPreviousWidth = onePreviousWidth;
                onePreviousColor = presentColor;
                onePreviousWidth = presentWidth;
                threePreviousBrightness = twoPreviousBrightness;
                twoPreviousBrightness = onePreviousBrightness;
                onePreviousBrightness = presentBrightness;
                threePreviousObjectIndex = twoPreviousObjectIndex;
                twoPreviousObjectIndex = onePreviousObjectIndex;
                onePreviousObjectIndex = presentObjectIndex;
                checkForSynapseZ();
            } // for (yStart = 0; yStart < yDim; yStart++)
        } // for (zStart = 0; zStart < zDim; zStart++)
        for (zStart = 0; zStart < zDim; zStart++) {
            for (xStart = 0; xStart < xDim; xStart++) {
                threePreviousColor = NONE;
                twoPreviousColor = NONE;
                onePreviousColor = NONE;
                presentColor = NONE;
                presentBrightColor = NONE;
                threePreviousWidth = 0;
                twoPreviousWidth = 0;
                onePreviousWidth = 0;
                presentWidth = 0;
                threePreviousBrightness = false;
                twoPreviousBrightness = false;
                onePreviousBrightness = false;
                presentBrightness = false;
                for (x = xStart, y = 0, z = zStart; (x >= 0) && (y <= yDim - 1) && (z <= zDim - 1); x--, y++, z++) {
                    pos = y * xDim + x;
                    if ((buffer[z][pos] == presentColor) || (buffer[z][pos] == presentBrightColor)) {
                        presentWidth++;
                        if ((presentColor == BRIGHT_BLUE) || (presentColor == BLUE)) {
                            blueX = (blueStartX + x) >> 1;
                            blueY = (blueStartY + y) >> 1;
                            blueZ = (blueStartZ + z) >> 1;    
                        }
                        if ((!presentBrightness) && (buffer[z][pos] >= BRIGHT_RED)) {
                            presentBrightness = true;
                        }
                    }
                    else {
                        threePreviousColor = twoPreviousColor;
                        threePreviousWidth = twoPreviousWidth;
                        twoPreviousColor = onePreviousColor;
                        twoPreviousWidth = onePreviousWidth;
                        onePreviousColor = presentColor;
                        onePreviousWidth = presentWidth;
                        threePreviousBrightness = twoPreviousBrightness;
                        twoPreviousBrightness = onePreviousBrightness;
                        onePreviousBrightness = presentBrightness;
                        threePreviousObjectIndex = twoPreviousObjectIndex;
                        twoPreviousObjectIndex = onePreviousObjectIndex;
                        onePreviousObjectIndex = presentObjectIndex;
                        if (buffer[z][pos] < BRIGHT_RED) {
                            presentColor = buffer[z][pos];
                            if (buffer[z][pos] == NONE) {
                                presentBrightColor = NONE;
                            }
                            else {
                                presentBrightColor = presentColor + 3;
                            }
                            presentBrightness = false;
                        }
                        else {
                            presentBrightColor = buffer[z][pos];
                            presentColor = presentBrightColor - 3;
                            presentBrightness = true;
                            
                        }
                        presentWidth = 1;
                        checkForSynapseZ(); 
                        if ((presentColor == BRIGHT_BLUE) || (presentColor == BLUE)) {
                            blueStartX = x;
                            blueStartY = y;
                            blueStartZ = z;
                            blueX = x;
                            blueY = y;
                            blueZ = z; 
                            presentObjectIndex = colorObjects[z][pos];
                        }
                        else if ((presentColor == BRIGHT_RED) || (presentColor == RED)) {
                            if (bigBlueFraction) {
                                presentObjectIndex = colorObjects[z][pos];
                            }
                            else {
                                presentObjectIndex = 0;
                            }
                        }
                        else if ((presentColor == BRIGHT_GREEN) || (presentColor == GREEN)) {
                            if (bigBlueFraction) {
                                presentObjectIndex = colorObjects[z][pos];
                            }
                            else {
                                presentObjectIndex = 0;
                            }
                        }
                    }
                } // for (x = xStart, y = 0, z = zStart; (x >= 0) && (y <= yDim - 1) && (z <= zDim - 1); x--, y++, z++)
                threePreviousColor = twoPreviousColor;
                threePreviousWidth = twoPreviousWidth;
                twoPreviousColor = onePreviousColor;
                twoPreviousWidth = onePreviousWidth;
                onePreviousColor = presentColor;
                onePreviousWidth = presentWidth;
                threePreviousBrightness = twoPreviousBrightness;
                twoPreviousBrightness = onePreviousBrightness;
                onePreviousBrightness = presentBrightness;
                threePreviousObjectIndex = twoPreviousObjectIndex;
                twoPreviousObjectIndex = onePreviousObjectIndex;
                onePreviousObjectIndex = presentObjectIndex;
                checkForSynapseZ();
            } // for (xStart = 0; xStart < xDim; xStart++)
        } // for (zStart = 0; zStart < zDim; zStart++)
        for (yStart = 0; yStart < yDim; yStart++) {
            for (xStart = 0; xStart < xDim; xStart++) {
                threePreviousColor = NONE;
                twoPreviousColor = NONE;
                onePreviousColor = NONE;
                presentColor = NONE;
                presentBrightColor = NONE;
                threePreviousWidth = 0;
                twoPreviousWidth = 0;
                onePreviousWidth = 0;
                presentWidth = 0;
                threePreviousBrightness = false;
                twoPreviousBrightness = false;
                onePreviousBrightness = false;
                presentBrightness = false;
                for (x = xStart, y = yStart, z = 0; (x >= 0) && (y <= yDim - 1) && (z <= zDim - 1); x--, y++, z++) {
                    pos = y * xDim + x;
                    if ((buffer[z][pos] == presentColor) || (buffer[z][pos] == presentBrightColor)) {
                        presentWidth++;
                        if ((presentColor == BRIGHT_BLUE) || (presentColor == BLUE)) {
                            blueX = (blueStartX + x) >> 1;
                            blueY = (blueStartY + y) >> 1;
                            blueZ = (blueStartZ + z) >> 1;    
                        }
                        if ((!presentBrightness) && (buffer[z][pos] >= BRIGHT_RED)) {
                            presentBrightness = true;
                        }
                    }
                    else {
                        threePreviousColor = twoPreviousColor;
                        threePreviousWidth = twoPreviousWidth;
                        twoPreviousColor = onePreviousColor;
                        twoPreviousWidth = onePreviousWidth;
                        onePreviousColor = presentColor;
                        onePreviousWidth = presentWidth;
                        threePreviousBrightness = twoPreviousBrightness;
                        twoPreviousBrightness = onePreviousBrightness;
                        onePreviousBrightness = presentBrightness;
                        threePreviousObjectIndex = twoPreviousObjectIndex;
                        twoPreviousObjectIndex = onePreviousObjectIndex;
                        onePreviousObjectIndex = presentObjectIndex;
                        if (buffer[z][pos] < BRIGHT_RED) {
                            presentColor = buffer[z][pos];
                            if (buffer[z][pos] == NONE) {
                                presentBrightColor = NONE;
                            }
                            else {
                                presentBrightColor = presentColor + 3;
                            }
                            presentBrightness = false;
                        }
                        else {
                            presentBrightColor = buffer[z][pos];
                            presentColor = presentBrightColor - 3;
                            presentBrightness = true;
                            
                        }
                        presentWidth = 1;
                        checkForSynapseZ(); 
                        if ((presentColor == BRIGHT_BLUE) || (presentColor == BLUE)) {
                            blueStartX = x;
                            blueStartY = y;
                            blueStartZ = z;
                            blueX = x;
                            blueY = y;
                            blueZ = z; 
                            presentObjectIndex = colorObjects[z][pos];
                        }
                        else if ((presentColor == BRIGHT_RED) || (presentColor == RED)) {
                            if (bigBlueFraction) {
                                presentObjectIndex = colorObjects[z][pos];
                            }
                            else {
                                presentObjectIndex = 0;
                            }
                        }
                        else if ((presentColor == BRIGHT_GREEN) || (presentColor == GREEN)) {
                            if (bigBlueFraction) {
                                presentObjectIndex = colorObjects[z][pos];
                            }
                            else {
                                presentObjectIndex = 0;
                            }
                        }
                    }
                } // for (x = xStart, y = yStart, z = 0; (x >= 0) && (y <= yDim - 1) && (z <= zDim - 1); x--, y++, z++)
                threePreviousColor = twoPreviousColor;
                threePreviousWidth = twoPreviousWidth;
                twoPreviousColor = onePreviousColor;
                twoPreviousWidth = onePreviousWidth;
                onePreviousColor = presentColor;
                onePreviousWidth = presentWidth;
                threePreviousBrightness = twoPreviousBrightness;
                twoPreviousBrightness = onePreviousBrightness;
                onePreviousBrightness = presentBrightness;
                threePreviousObjectIndex = twoPreviousObjectIndex;
                twoPreviousObjectIndex = onePreviousObjectIndex;
                onePreviousObjectIndex = presentObjectIndex;
                checkForSynapseZ();
            } // for (xStart = 0; xStart < xDim; xStart++)
        } // for (yStart = 0; yStart < yDim; yStart++)
        System.out.println("Number of synapses found searching parallel to (-delX = delY = delZ) = " + (numSynapses - previousNumSynapses));
        Preferences.debug("Number of synapses found searching parallel to (-delX = delY = delZ) = " + (numSynapses - previousNumSynapses) + "\n");
        dataString += "\nNumber of synapses found searching parallel to (-delX = delY = delZ) = " + (numSynapses - previousNumSynapses) + "\n";
        previousNumSynapses = numSynapses;


        if (threadStopped) {
            finalize();

            return;
        }
        
        System.out.println("Total synapses found = " + numSynapses);
        Preferences.debug("Total synapses found = " + numSynapses + "\n");
        dataString += "\nTotal synapses found = " + numSynapses + "\n";
        if (!bigBlueFraction) {
            dataString += "Minimum blue count = " + minimumBlueCount + "\n";
            dataString += "Maximum blue count = " + maximumBlueCount + "\n";
            averageBlueCount = (double)totalBlueCount/numSynapses;
            dataString += "Average blue count = " + String.format("%.2f\n",averageBlueCount);
            blueStandardDeviation = (totalBlueCountSquared - (double)totalBlueCount * (double)totalBlueCount/numSynapses)/(numSynapses - 1);
            blueStandardDeviation = Math.sqrt(blueStandardDeviation);
            dataString += "Blue count standard deviation = " + String.format("%.2f\n",blueStandardDeviation);
        } // if (!bigBlueFraction)
       
        index = inputFileName.indexOf(".tif");
        if (index > 0) {
            inputFileName = inputFileName.substring(0, index);
        }
        file = new File(directory + inputFileName + ".txt");

        try {
            raFile = new RandomAccessFile(file, "rw");

            // Necessary so that if this is an overwritten file there isn't any
            // junk at the end
            raFile.setLength(0);
            raFile.write(dataString.getBytes());
            raFile.close();
        } catch (FileNotFoundException e) {
            MipavUtil.displayError("FileNotFoundException " + e);
        } catch (IOException e) {
            MipavUtil.displayError("IOException " + e);
        }
        
        
       
        fireProgressStateChanged(100);
        time = System.currentTimeMillis() - time;
        Preferences.debug("PlugInAlgorithmSynapseDetection elapsed time in seconds = " + (time/1000.0));
        setCompleted(true);
    }
    
    // Finding red, blue, green or green, blue, red with all 3 colors in the appropriate width range
    // means that a synapse has been found within a plane
    private void checkForSynapseXY() {
       int i;
       short blueXWidth;
       short blueYWidth;
       short blueZWidth;
       if ((threePreviousBrightness && twoPreviousBrightness && onePreviousBrightness &&
           (twoPreviousColor == BLUE) && (twoPreviousWidth >= blueMinXY) && (twoPreviousWidth <= blueMaxXY)) && 
           (((threePreviousColor == RED) && (threePreviousWidth >= redMin) && (threePreviousWidth <= redMax) &&
           (onePreviousColor == GREEN) && (onePreviousWidth >= greenMin) && (onePreviousWidth <= greenMax)) ||
           ((threePreviousColor == GREEN) && (threePreviousWidth >= greenMin) && (threePreviousWidth <= greenMax) &&
           (onePreviousColor == RED) && (onePreviousWidth >= redMin) && (onePreviousWidth <= redMax)))) {
           if (bigBlueFraction) {
               if (threePreviousColor == RED) {
                   for (i = 0; i < 5; i++) {
                       if ((synapseBlueFound[5*threePreviousObjectIndex+i] == twoPreviousObjectIndex) &&
                           (synapseGreenFound[5*threePreviousObjectIndex+i] == onePreviousObjectIndex)) {
                           if (i == 4) {
                               System.err.println("More than 5 synapses for redObject = " + threePreviousColor);
                           }
                           return;
                       }
                       else if (synapseBlueFound[5*threePreviousObjectIndex+i] == 0) {
                           synapseBlueFound[5*threePreviousObjectIndex+i] = twoPreviousObjectIndex;
                           synapseGreenFound[5*threePreviousObjectIndex+i] = onePreviousObjectIndex;
                           break;
                       }
                   }
               } // if (threePreviousColor == RED)
               else if (onePreviousColor == RED) {
                   for (i = 0; i < 5; i++) {
                       if ((synapseBlueFound[5*onePreviousObjectIndex+i] == twoPreviousObjectIndex) &&
                           (synapseGreenFound[5*onePreviousObjectIndex+i] == threePreviousObjectIndex)) {
                           if (i == 4) {
                               System.err.println("More than 5 synapses for redObject = " + onePreviousColor);
                           }
                           return;
                       }
                       else if (synapseBlueFound[5*onePreviousObjectIndex+i] == 0) {
                           synapseBlueFound[5*onePreviousObjectIndex+i] = twoPreviousObjectIndex;
                           synapseGreenFound[5*onePreviousObjectIndex+i] = threePreviousObjectIndex;
                           break;
                       }
                   }    
               } // else if (onePreviousColor == RED)
               centerBlueX = blueX;
               centerBlueY = blueY;
               centerBlueZ = blueZ;
               dataString += String.format("%-10d%-10d%-10d%-10d\n",numSynapses+1, centerBlueX, centerBlueY, centerBlueZ); 
           } // if (bigBlueFraction)
           else {
               if (synapseBlueFound[twoPreviousObjectIndex] > 0) {
                   return;
               }
               synapseBlueFound[twoPreviousObjectIndex] = 1;
               blueCount = blueCountList.get(twoPreviousObjectIndex-1).intValue();
               centerBlueX = blueCenterXList.get(twoPreviousObjectIndex-1).shortValue();
               centerBlueY = blueCenterYList.get(twoPreviousObjectIndex-1).shortValue();
               centerBlueZ = blueCenterZList.get(twoPreviousObjectIndex-1).shortValue();
               blueXWidth = blueXWidthList.get(twoPreviousObjectIndex-1).shortValue();
               blueYWidth = blueYWidthList.get(twoPreviousObjectIndex-1).shortValue();
               blueZWidth = blueZWidthList.get(twoPreviousObjectIndex-1).shortValue();
               dataString += String.format("%-10d%-10d%-10d%-10d%-10d%-10d%-10d%-10d\n",
                       numSynapses+1, centerBlueX, centerBlueY, centerBlueZ, blueCount,
                       blueXWidth, blueYWidth, blueZWidth);
               if (blueCount < minimumBlueCount) {
                   minimumBlueCount = blueCount;
               }
               if (blueCount > maximumBlueCount) {
                   maximumBlueCount = blueCount;
               }
               totalBlueCount += blueCount;
               totalBlueCountSquared += blueCount * blueCount;
           }
           
           numSynapses++;
       }
    }
    
    //Finding red, blue, green or green, blue, red with all 3 colors in the appropriate width range
    // means that a synapse has been found between planes
    private void checkForSynapseZ() {
       int i;
       short blueXWidth;
       short blueYWidth;
       short blueZWidth;
       if ((threePreviousBrightness && twoPreviousBrightness && onePreviousBrightness &&
           (twoPreviousColor == BLUE) && (twoPreviousWidth >= blueMinZ) && (twoPreviousWidth <= blueMaxZ)) && 
           (((threePreviousColor == RED) && (threePreviousWidth >= redMin) && (threePreviousWidth <= redMax) &&
           (onePreviousColor == GREEN) && (onePreviousWidth >= greenMin) && (onePreviousWidth <= greenMax)) ||
           ((threePreviousColor == GREEN) && (threePreviousWidth >= greenMin) && (threePreviousWidth <= greenMax) &&
           (onePreviousColor == RED) && (onePreviousWidth >= redMin) && (onePreviousWidth <= redMax)))) {
           if (bigBlueFraction) {
               if (threePreviousColor == RED) {
                   for (i = 0; i < 5; i++) {
                       if ((synapseBlueFound[5*threePreviousObjectIndex+i] == twoPreviousObjectIndex) &&
                           (synapseGreenFound[5*threePreviousObjectIndex+i] == onePreviousObjectIndex)) {
                           if (i == 4) {
                               System.err.println("More than 5 synapses for redObject = " + threePreviousColor);
                           }
                           return;
                       }
                       else if (synapseBlueFound[5*threePreviousObjectIndex+i] == 0) {
                           synapseBlueFound[5*threePreviousObjectIndex+i] = twoPreviousObjectIndex;
                           synapseGreenFound[5*threePreviousObjectIndex+i] = onePreviousObjectIndex;
                           break;
                       }
                   }
               } // if (threePreviousColor == RED)
               else if (onePreviousColor == RED) {
                   for (i = 0; i < 5; i++) {
                       if ((synapseBlueFound[5*onePreviousObjectIndex+i] == twoPreviousObjectIndex) &&
                           (synapseGreenFound[5*onePreviousObjectIndex+i] == threePreviousObjectIndex)) {
                           if (i == 4) {
                               System.err.println("More than 5 synapses for redObject = " + onePreviousColor);
                           }
                           return;
                       }
                       else if (synapseBlueFound[5*onePreviousObjectIndex+i] == 0) {
                           synapseBlueFound[5*onePreviousObjectIndex+i] = twoPreviousObjectIndex;
                           synapseGreenFound[5*onePreviousObjectIndex+i] = threePreviousObjectIndex;
                           break;
                       }
                   }    
               } // else if (onePreviousColor == RED)
               centerBlueX = blueX;
               centerBlueY = blueY;
               centerBlueZ = blueZ;
               dataString += String.format("%-10d%-10d%-10d%-10d\n",numSynapses+1, centerBlueX, centerBlueY, centerBlueZ); 
           } // if (bigBlueFraction)
           else {
               if (synapseBlueFound[twoPreviousObjectIndex] > 0) {
                   return;
               }
               synapseBlueFound[twoPreviousObjectIndex] = 1;
               blueCount = blueCountList.get(twoPreviousObjectIndex-1).intValue();
               centerBlueX = blueCenterXList.get(twoPreviousObjectIndex-1).shortValue();
               centerBlueY = blueCenterYList.get(twoPreviousObjectIndex-1).shortValue();
               centerBlueZ = blueCenterZList.get(twoPreviousObjectIndex-1).shortValue();
               blueXWidth = blueXWidthList.get(twoPreviousObjectIndex-1).shortValue();
               blueYWidth = blueYWidthList.get(twoPreviousObjectIndex-1).shortValue();
               blueZWidth = blueZWidthList.get(twoPreviousObjectIndex-1).shortValue();
               dataString += String.format("%-10d%-10d%-10d%-10d%-10d%-10d%-10d%-10d\n",
                       numSynapses+1, centerBlueX, centerBlueY, centerBlueZ, blueCount,
                       blueXWidth, blueYWidth, blueZWidth);
               if (blueCount < minimumBlueCount) {
                   minimumBlueCount = blueCount;
               }
               if (blueCount > maximumBlueCount) {
                   maximumBlueCount = blueCount;
               }
               totalBlueCount += blueCount;
               totalBlueCountSquared += blueCount * blueCount;
           }
           
           numSynapses++;
       }
    }
    
    private void identifyObjects() {
        int i;
        int x;
        int y;
        int z;
        int yPos;
        boolean change;
        change = true;
        int del;
        int maxZDel;
        int maxYDel;
        int maxYZDel;
        int maxXDel;
        int maxDel;
        int xLow;
        int xHigh;
        int yLow;
        int yHigh;
        int zLow;
        int zHigh;
        int x2;
        int y2;
        int z2;
        int y2Pos;
        int i2;
        int lowBlueX = 0;
        int highBlueX = 0;
        int lowBlueY = 0;
        int highBlueY = 0;
        int lowBlueZ = 0;
        int highBlueZ = 0;
        colorObjects = new int[zDim][xySlice];
        if (!bigBlueFraction) {
            blueCountList = new ArrayList<Integer>();
            blueCenterXList = new ArrayList<Short>();
            blueCenterYList = new ArrayList<Short>();
            blueCenterZList = new ArrayList<Short>();
            blueXWidthList = new ArrayList<Short>();
            blueYWidthList = new ArrayList<Short>();
            blueZWidthList = new ArrayList<Short>();
        }
        for (z = 0; z < zDim; z++) {
            fireProgressStateChanged(95 * z/zDim);
            maxZDel = Math.max(z, zDim - 1 - z);
            for (y = 0; y < yDim; y++) {
                yPos = y * xDim;
                maxYDel = Math.max(y, yDim - 1 - y);
                maxYZDel = Math.max(maxYDel, maxZDel);
                for (x = 0; x < xDim; x++) {
                    i = yPos + x; 
                    maxXDel = Math.max(x, xDim - 1 - x);
                    maxDel = Math.max(maxXDel, maxYZDel);
                    if (bigBlueFraction && (colorObjects[z][i] == 0) && ((buffer[z][i] == RED) || (buffer[z][i] == BRIGHT_RED))) {
                        change = true;
                        colorObjects[z][i] = ++redObjectIndex;
                        del = -1;
                        while (change) {
                            change = false;
                            if (del < maxDel) {
                                del++;   
                            }
                            xLow = Math.max(0, x - del);
                            xHigh = Math.min(xDim - 1, x + del);
                            yLow = Math.max(0, y - del);
                            yHigh = Math.min(yDim - 1, y + del);
                            zLow = Math.max(0, z - del);
                            zHigh = Math.min(zDim - 1, z + del);
                            for (z2 = zLow; z2 <= zHigh; z2++) {
                                for (y2 = yLow; y2 <= yHigh; y2++) {
                                    y2Pos = y2 * xDim;
                                    for (x2 = xLow; x2 <= xHigh; x2++) {
                                        i2 = y2Pos + x2;
                                        if ((colorObjects[z2][i2] > 0) && ((buffer[z2][i2] == BRIGHT_RED) || (buffer[z2][i2] == RED))) {
                                            if ((x2 > 0) && (colorObjects[z2][i2-1] == 0) && ((buffer[z2][i2-1] == BRIGHT_RED) || (buffer[z2][i2-1] == RED))) {
                                                colorObjects[z2][i2-1] = redObjectIndex;
                                                change = true;
                                            }
                                            if ((x2 < xDim - 1) && (colorObjects[z2][i2+1] == 0) && ((buffer[z2][i2+1] == BRIGHT_RED) || (buffer[z2][i2+1] == RED))) {
                                                colorObjects[z2][i2+1] = redObjectIndex;
                                                change = true;
                                            }
                                            if ((y2 > 0)&& (colorObjects[z2][i2-xDim] == 0) && ((buffer[z2][i2-xDim] == BRIGHT_RED) || (buffer[z2][i2-xDim] == RED))) {
                                                colorObjects[z2][i2-xDim] = redObjectIndex;
                                                change = true;
                                            }
                                            if ((y2 < yDim - 1) && (colorObjects[z2][i2+xDim] == 0) && ((buffer[z2][i2+xDim] == BRIGHT_RED) || (buffer[z2][i2+xDim] == RED))) {
                                                colorObjects[z2][i2+xDim] = redObjectIndex;
                                                change = true;
                                            }
                                            if ((z2 > 0) && (colorObjects[z2-1][i2] == 0) && ((buffer[z2-1][i2] == BRIGHT_RED) || (buffer[z2-1][i2] == RED))) {
                                                colorObjects[z2-1][i2] = redObjectIndex;
                                                change = true;
                                            }
                                            if ((z2 < zDim - 1) && (colorObjects[z2+1][i2] == 0) && ((buffer[z2+1][i2] == BRIGHT_RED) || (buffer[z2+1][i2] == RED))) {
                                                colorObjects[z2+1][i2] = redObjectIndex;
                                                change = true;
                                            }
                                            if ((x2 > 0) && (y2 > 0) && (colorObjects[z2][i2-xDim-1] == 0) && ((buffer[z2][i2-xDim-1] == BRIGHT_RED) || (buffer[z2][i2-xDim-1] == RED))) {
                                                colorObjects[z2][i2-xDim-1] = redObjectIndex;
                                                change = true;
                                            }
                                            if ((x2 > 0) && (y2 < yDim - 1) && (colorObjects[z2][i2+xDim-1] == 0) && ((buffer[z2][i2+xDim-1] == BRIGHT_RED) || (buffer[z2][i2+xDim-1] == RED))) {
                                                colorObjects[z2][i2+xDim-1] = redObjectIndex;
                                                change = true;
                                            }
                                            if ((x2 < xDim-1) && (y2 > 0) && (colorObjects[z2][i2-xDim+1] == 0) && ((buffer[z2][i2-xDim+1] == BRIGHT_RED) || (buffer[z2][i2-xDim+1] == RED))) {
                                                colorObjects[z2][i2-xDim+1] = redObjectIndex;
                                                change = true;
                                            }
                                            if ((x2 < xDim - 1) && (y2 < yDim - 1) && (colorObjects[z2][i2+xDim+1] == 0) && ((buffer[z2][i2+xDim+1] == BRIGHT_RED) || (buffer[z2][i2+xDim+1] == RED))) {
                                                colorObjects[z2][i2+xDim+1] = redObjectIndex;
                                                change = true;
                                            }
                                            if ((x2 > 0) && (z2 > 0) && (colorObjects[z2-1][i2-1] == 0) && ((buffer[z2-1][i2-1] == BRIGHT_RED) || (buffer[z2-1][i2-1] == RED))) {
                                                colorObjects[z2-1][i2-1] = redObjectIndex;
                                                change = true;
                                            }
                                            if ((x2 > 0) && (z2 < zDim - 1) && (colorObjects[z2+1][i2-1] == 0) && ((buffer[z2+1][i2-1] == BRIGHT_RED) || (buffer[z2+1][i2-1] == RED))) {
                                                colorObjects[z2+1][i2-1] = redObjectIndex;
                                                change = true;
                                            }
                                            if ((x2 < xDim-1) && (z2 > 0) && (colorObjects[z2-1][i2+1] == 0) && ((buffer[z2-1][i2+1] == BRIGHT_RED) || (buffer[z2-1][i2+1] == RED))) {
                                                colorObjects[z2-1][i2+1] = redObjectIndex;
                                                change = true;
                                            }
                                            if ((x2 < xDim - 1) && (z2 < zDim - 1) && (colorObjects[z2+1][i2+1] == 0) && ((buffer[z2+1][i2+1] == BRIGHT_RED) || (buffer[z2+1][i2+1] == RED))) {
                                                colorObjects[z2+1][i2+1] = redObjectIndex;
                                                change = true;
                                            }
                                            if ((y2 > 0) && (z2 > 0) && (colorObjects[z2-1][i2-xDim] == 0) && ((buffer[z2-1][i2-xDim] == BRIGHT_RED) || (buffer[z2-1][i2-xDim] == RED))) {
                                                colorObjects[z2-1][i2-xDim] = redObjectIndex;
                                                change = true;
                                            }
                                            if ((y2 > 0) && (z2 < zDim - 1) && (colorObjects[z2+1][i2-xDim] == 0) && ((buffer[z2+1][i2-xDim] == BRIGHT_RED) || (buffer[z2+1][i2-xDim] == RED))) {
                                                colorObjects[z2+1][i2-xDim] = redObjectIndex;
                                                change = true;
                                            }
                                            if ((y2 < yDim-1) && (z2 > 0) && (colorObjects[z2-1][i2+xDim] == 0) && ((buffer[z2-1][i2+xDim] == BRIGHT_RED) || (buffer[z2-1][i2+xDim] == RED))) {
                                                colorObjects[z2-1][i2+xDim] = redObjectIndex;
                                                change = true;
                                            }
                                            if ((y2 < yDim - 1) && (z2 < zDim - 1) && (colorObjects[z2+1][i2+xDim] == 0) && ((buffer[z2+1][i2+xDim] == BRIGHT_RED) || (buffer[z2+1][i2+xDim] == RED))) {
                                                colorObjects[z2+1][i2+xDim] = redObjectIndex;
                                                change = true;
                                            }
                                            if ((x2 > 0) && (y2 > 0) && (z2 > 0)&& (colorObjects[z2-1][i2-xDim-1] == 0) && ((buffer[z2-1][i2-xDim-1] == BRIGHT_RED) || (buffer[z2-1][i2-xDim-1] == RED))) {
                                                colorObjects[z2-1][i2-xDim-1] = redObjectIndex;
                                                change = true;
                                            }
                                            if ((x2 > 0) && (y2 > 0) && (z2 < zDim - 1) && (colorObjects[z2+1][i2-xDim-1] == 0) && ((buffer[z2+1][i2-xDim-1] == BRIGHT_RED) ||(buffer[z2+1][i2-xDim-1] == RED))) {
                                                colorObjects[z2+1][i2-xDim-1] = redObjectIndex;
                                                change = true;
                                            }
                                            if ((x2 > 0) && (y2 < yDim - 1) && (z2 > 0)&& (colorObjects[z2-1][i2+xDim-1] == 0) && ((buffer[z2-1][i2+xDim-1] == BRIGHT_RED) || (buffer[z2-1][i2+xDim-1] == RED))) {
                                                colorObjects[z2-1][i2+xDim-1] = redObjectIndex;
                                                change = true;
                                            }
                                            if ((x2 > 0) && (y2 < yDim - 1) && (z2 < zDim - 1) && (colorObjects[z2+1][i2+xDim-1] == 0) && ((buffer[z2+1][i2+xDim-1] == BRIGHT_RED) || (buffer[z2+1][i2+xDim-1] == RED))) {
                                                colorObjects[z2+1][i2+xDim-1] = redObjectIndex;
                                                change = true;
                                            }
                                            if ((x2 < xDim-1) && (y2 > 0) && (z2 > 0) && (colorObjects[z2-1][i2-xDim+1] == 0) && ((buffer[z2-1][i2-xDim+1] == BRIGHT_RED) || (buffer[z2-1][i2-xDim+1] == RED))) {
                                                colorObjects[z2-1][i2-xDim+1] = redObjectIndex;
                                                change = true;
                                            }
                                            if ((x2 < xDim-1) && (y2 > 0) && (z2 < zDim - 1) && (colorObjects[z2+1][i2-xDim+1] == 0) && ((buffer[z2+1][i2-xDim+1] == BRIGHT_RED) || (buffer[z2+1][i2-xDim+1] == RED))) {
                                                colorObjects[z2+1][i2-xDim+1] = redObjectIndex;
                                                change = true;
                                            }
                                            if ((x2 < xDim - 1) && (y2 < yDim - 1) && (z2 > 0) && (colorObjects[z2-1][i2+xDim+1] == 0) && ((buffer[z2-1][i2+xDim+1] == BRIGHT_RED) ||(buffer[z2-1][i2+xDim+1] == RED))) {
                                                colorObjects[z2-1][i2+xDim+1] = redObjectIndex;
                                                change = true;
                                            }
                                            if ((x2 < xDim - 1) && (y2 < yDim - 1) && (z2 < zDim - 1) && (colorObjects[z2+1][i2+xDim+1] == 0) && ((buffer[z2+1][i2+xDim+1] == BRIGHT_RED) || (buffer[z2+1][i2+xDim+1] == RED))) {
                                                colorObjects[z2+1][i2+xDim+1] = redObjectIndex;
                                                change = true;
                                            }
                                        } // if ((colorObjects[i2] > 0) && ((buffer[i2] == BRIGHT_RED) || (buffer[i2] == RED)))
                                    } // for (x2 = xLow; x2 <= xHigh; x2++)
                                } // for (y2 = yLow; y2 <= yHigh; y2++)
                            } // for (z2 = zLow; z2 <= zHigh; z2++)
                        } // while (change)
                    } // if (bigBlueFraction && (colorObjects[z][i] == 0) && ((buffer[z][i] == RED) || (buffer[z][i] == BRIGHT_RED)))
                    else if (bigBlueFraction && (colorObjects[z][i] == 0) && ((buffer[z][i] == GREEN) || (buffer[z][i] == BRIGHT_GREEN))) {
                        change = true;
                        colorObjects[z][i] = ++greenObjectIndex;
                        del = -1;
                        while (change) {
                            change = false;
                            if (del < maxDel) {
                                del++;   
                            }
                            xLow = Math.max(0, x - del);
                            xHigh = Math.min(xDim - 1, x + del);
                            yLow = Math.max(0, y - del);
                            yHigh = Math.min(yDim - 1, y + del);
                            zLow = Math.max(0, z - del);
                            zHigh = Math.min(zDim - 1, z + del);
                            for (z2 = zLow; z2 <= zHigh; z2++) {
                                for (y2 = yLow; y2 <= yHigh; y2++) {
                                    y2Pos = y2 * xDim;
                                    for (x2 = xLow; x2 <= xHigh; x2++) {
                                        i2 = y2Pos + x2;
                                        if ((colorObjects[z2][i2] > 0) && ((buffer[z2][i2] == BRIGHT_GREEN) || (buffer[z2][i2] == GREEN))) {
                                            if ((x2 > 0) && (colorObjects[z2][i2-1] == 0) && ((buffer[z2][i2-1] == BRIGHT_GREEN) || (buffer[z2][i2-1] == GREEN))) {
                                                colorObjects[z2][i2-1] = greenObjectIndex;
                                                change = true;
                                            }
                                            if ((x2 < xDim - 1) && (colorObjects[z2][i2+1] == 0) && ((buffer[z2][i2+1] == BRIGHT_GREEN) || (buffer[z2][i2+1] == GREEN))) {
                                                colorObjects[z2][i2+1] = greenObjectIndex;
                                                change = true;
                                            }
                                            if ((y2 > 0)&& (colorObjects[z2][i2-xDim] == 0) && ((buffer[z2][i2-xDim] == BRIGHT_GREEN) || (buffer[z2][i2-xDim] == GREEN))) {
                                                colorObjects[z2][i2-xDim] = greenObjectIndex;
                                                change = true;
                                            }
                                            if ((y2 < yDim - 1) && (colorObjects[z2][i2+xDim] == 0) && ((buffer[z2][i2+xDim] == BRIGHT_GREEN) || (buffer[z2][i2+xDim] == GREEN))) {
                                                colorObjects[z2][i2+xDim] = greenObjectIndex;
                                                change = true;
                                            }
                                            if ((z2 > 0) && (colorObjects[z2-1][i2] == 0) && ((buffer[z2-1][i2] == BRIGHT_GREEN) || (buffer[z2-1][i2] == GREEN))) {
                                                colorObjects[z2-1][i2] = greenObjectIndex;
                                                change = true;
                                            }
                                            if ((z2 < zDim - 1) && (colorObjects[z2+1][i2] == 0) && ((buffer[z2+1][i2] == BRIGHT_GREEN) || (buffer[z2+1][i2] == GREEN))) {
                                                colorObjects[z2+1][i2] = greenObjectIndex;
                                                change = true;
                                            }
                                            if ((x2 > 0) && (y2 > 0) && (colorObjects[z2][i2-xDim-1] == 0) && ((buffer[z2][i2-xDim-1] == BRIGHT_GREEN) || (buffer[z2][i2-xDim-1] == GREEN))) {
                                                colorObjects[z2][i2-xDim-1] = greenObjectIndex;
                                                change = true;
                                            }
                                            if ((x2 > 0) && (y2 < yDim - 1) && (colorObjects[z2][i2+xDim-1] == 0) && ((buffer[z2][i2+xDim-1] == BRIGHT_GREEN) || (buffer[z2][i2+xDim-1] == GREEN))) {
                                                colorObjects[z2][i2+xDim-1] = greenObjectIndex;
                                                change = true;
                                            }
                                            if ((x2 < xDim-1) && (y2 > 0) && (colorObjects[z2][i2-xDim+1] == 0) && ((buffer[z2][i2-xDim+1] == BRIGHT_GREEN) || (buffer[z2][i2-xDim+1] == GREEN))) {
                                                colorObjects[z2][i2-xDim+1] = greenObjectIndex;
                                                change = true;
                                            }
                                            if ((x2 < xDim - 1) && (y2 < yDim - 1) && (colorObjects[z2][i2+xDim+1] == 0) && ((buffer[z2][i2+xDim+1] == BRIGHT_GREEN) || (buffer[z2][i2+xDim+1] == GREEN))) {
                                                colorObjects[z2][i2+xDim+1] = greenObjectIndex;
                                                change = true;
                                            }
                                            if ((x2 > 0) && (z2 > 0) && (colorObjects[z2-1][i2-1] == 0) && ((buffer[z2-1][i2-1] == BRIGHT_GREEN) || (buffer[z2-1][i2-1] == GREEN))) {
                                                colorObjects[z2-1][i2-1] = greenObjectIndex;
                                                change = true;
                                            }
                                            if ((x2 > 0) && (z2 < zDim - 1) && (colorObjects[z2+1][i2-1] == 0) && ((buffer[z2+1][i2-1] == BRIGHT_GREEN) || (buffer[z2+1][i2-1] == GREEN))) {
                                                colorObjects[z2+1][i2-1] = greenObjectIndex;
                                                change = true;
                                            }
                                            if ((x2 < xDim-1) && (z2 > 0) && (colorObjects[z2-1][i2+1] == 0) && ((buffer[z2-1][i2+1] == BRIGHT_GREEN) || (buffer[z2-1][i2+1] == GREEN))) {
                                                colorObjects[z2-1][i2+1] = greenObjectIndex;
                                                change = true;
                                            }
                                            if ((x2 < xDim - 1) && (z2 < zDim - 1) && (colorObjects[z2+1][i2+1] == 0) && ((buffer[z2+1][i2+1] == BRIGHT_GREEN) || (buffer[z2+1][i2+1] == GREEN))) {
                                                colorObjects[z2+1][i2+1] = greenObjectIndex;
                                                change = true;
                                            }
                                            if ((y2 > 0) && (z2 > 0) && (colorObjects[z2-1][i2-xDim] == 0) && ((buffer[z2-1][i2-xDim] == BRIGHT_GREEN) || (buffer[z2-1][i2-xDim] == GREEN))) {
                                                colorObjects[z2-1][i2-xDim] = greenObjectIndex;
                                                change = true;
                                            }
                                            if ((y2 > 0) && (z2 < zDim - 1) && (colorObjects[z2+1][i2-xDim] == 0) && ((buffer[z2+1][i2-xDim] == BRIGHT_GREEN) || (buffer[z2+1][i2-xDim] == GREEN))) {
                                                colorObjects[z2+1][i2-xDim] = greenObjectIndex;
                                                change = true;
                                            }
                                            if ((y2 < yDim-1) && (z2 > 0) && (colorObjects[z2-1][i2+xDim] == 0) && ((buffer[z2-1][i2+xDim] == BRIGHT_GREEN) || (buffer[z2-1][i2+xDim] == GREEN))) {
                                                colorObjects[z2-1][i2+xDim] = greenObjectIndex;
                                                change = true;
                                            }
                                            if ((y2 < yDim - 1) && (z2 < zDim - 1) && (colorObjects[z2+1][i2+xDim] == 0) && ((buffer[z2+1][i2+xDim] == BRIGHT_GREEN) || (buffer[z2+1][i2+xDim] == GREEN))) {
                                                colorObjects[z2+1][i2+xDim] = greenObjectIndex;
                                                change = true;
                                            }
                                            if ((x2 > 0) && (y2 > 0) && (z2 > 0)&& (colorObjects[z2-1][i2-xDim-1] == 0) && ((buffer[z2-1][i2-xDim-1] == BRIGHT_GREEN) || (buffer[z2-1][i2-xDim-1] == GREEN))) {
                                                colorObjects[z2-1][i2-xDim-1] = greenObjectIndex;
                                                change = true;
                                            }
                                            if ((x2 > 0) && (y2 > 0) && (z2 < zDim - 1) && (colorObjects[z2+1][i2-xDim-1] == 0) && ((buffer[z2+1][i2-xDim-1] == BRIGHT_GREEN) ||(buffer[z2+1][i2-xDim-1] == GREEN))) {
                                                colorObjects[z2+1][i2-xDim-1] = greenObjectIndex;
                                                change = true;
                                            }
                                            if ((x2 > 0) && (y2 < yDim - 1) && (z2 > 0)&& (colorObjects[z2-1][i2+xDim-1] == 0) && ((buffer[z2-1][i2+xDim-1] == BRIGHT_GREEN) || (buffer[z2-1][i2+xDim-1] == GREEN))) {
                                                colorObjects[z2-1][i2+xDim-1] = greenObjectIndex;
                                                change = true;
                                            }
                                            if ((x2 > 0) && (y2 < yDim - 1) && (z2 < zDim - 1) && (colorObjects[z2+1][i2+xDim-1] == 0) && ((buffer[z2+1][i2+xDim-1] == BRIGHT_GREEN) || (buffer[z2+1][i2+xDim-1] == GREEN))) {
                                                colorObjects[z2+1][i2+xDim-1] = greenObjectIndex;
                                                change = true;
                                            }
                                            if ((x2 < xDim-1) && (y2 > 0) && (z2 > 0) && (colorObjects[z2-1][i2-xDim+1] == 0) && ((buffer[z2-1][i2-xDim+1] == BRIGHT_GREEN) || (buffer[z2-1][i2-xDim+1] == GREEN))) {
                                                colorObjects[z2-1][i2-xDim+1] = greenObjectIndex;
                                                change = true;
                                            }
                                            if ((x2 < xDim-1) && (y2 > 0) && (z2 < zDim - 1) && (colorObjects[z2+1][i2-xDim+1] == 0) && ((buffer[z2+1][i2-xDim+1] == BRIGHT_GREEN) || (buffer[z2+1][i2-xDim+1] == GREEN))) {
                                                colorObjects[z2+1][i2-xDim+1] = greenObjectIndex;
                                                change = true;
                                            }
                                            if ((x2 < xDim - 1) && (y2 < yDim - 1) && (z2 > 0) && (colorObjects[z2-1][i2+xDim+1] == 0) && ((buffer[z2-1][i2+xDim+1] == BRIGHT_GREEN) ||(buffer[z2-1][i2+xDim+1] == GREEN))) {
                                                colorObjects[z2-1][i2+xDim+1] = greenObjectIndex;
                                                change = true;
                                            }
                                            if ((x2 < xDim - 1) && (y2 < yDim - 1) && (z2 < zDim - 1) && (colorObjects[z2+1][i2+xDim+1] == 0) && ((buffer[z2+1][i2+xDim+1] == BRIGHT_GREEN) || (buffer[z2+1][i2+xDim+1] == GREEN))) {
                                                colorObjects[z2+1][i2+xDim+1] = greenObjectIndex;
                                                change = true;
                                            }
                                        } // if ((colorObjects[z2][i2] > 0) && ((buffer[z2][i2] == BRIGHT_GREEN) || (buffer[z2][i2] == GREEN))) 
                                    } // for (x2 = xLow; x2 <= xHigh; x2++)
                                } // for (y2 = yLow; y2 <= yHigh; y2++)
                            } // for (z2 = zLow; z2 <= zHigh; z2++)
                        } // while (change)
                    } // else if (bigBlueFraction && (colorObjects[z][i] == 0) && ((buffer[z][i] == GREEN) || (buffer[z][i] == BRIGHT_GREEN)))
                    else if ((colorObjects[z][i] == 0) && ((buffer[z][i] == BLUE) || (buffer[z][i] == BRIGHT_BLUE))) {
                        change = true;
                        colorObjects[z][i] = ++blueObjectIndex;
                        if (!bigBlueFraction) {
                            blueCount = 1;
                            centerBlueX = x;
                            centerBlueY = y;
                            centerBlueZ = z;
                            lowBlueX = x;
                            highBlueX = x;
                            lowBlueY = y;
                            highBlueY = y;
                            lowBlueZ = z;
                            highBlueZ = z;
                        }
                        del = -1;
                        while (change) {
                            change = false;
                            if (del < maxDel) {
                                del++;   
                            }
                            xLow = Math.max(0, x - del);
                            xHigh = Math.min(xDim - 1, x + del);
                            yLow = Math.max(0, y - del);
                            yHigh = Math.min(yDim - 1, y + del);
                            zLow = Math.max(0, z - del);
                            zHigh = Math.min(zDim - 1, z + del);
                            for (z2 = zLow; z2 <= zHigh; z2++) {
                                for (y2 = yLow; y2 <= yHigh; y2++) {
                                    y2Pos = y2 * xDim;
                                    for (x2 = xLow; x2 <= xHigh; x2++) {
                                        i2 = y2Pos + x2;
                                        if ((colorObjects[z2][i2] > 0) && ((buffer[z2][i2] == BRIGHT_BLUE) || (buffer[z2][i2] == BLUE))) {
                                            if ((x2 > 0) && (colorObjects[z2][i2-1] == 0) && ((buffer[z2][i2-1] == BRIGHT_BLUE) || (buffer[z2][i2-1] == BLUE))) {
                                                colorObjects[z2][i2-1] = blueObjectIndex;
                                                change = true;
                                                if (!bigBlueFraction) {
                                                    blueCount++;
                                                    centerBlueX += (x2 - 1);
                                                    centerBlueY += y2;
                                                    centerBlueZ += z2;
                                                    if ((x2 - 1) < lowBlueX) {
                                                        lowBlueX = x2 - 1;   
                                                    }
                                                    if ((x2 - 1) > highBlueX) {
                                                        highBlueX = x2 - 1;
                                                    }
                                                    if (y2 < lowBlueY) {
                                                        lowBlueY = y2;
                                                    }
                                                    if (y2 > highBlueY) {
                                                        highBlueY = y2;
                                                    }
                                                    if (z2 < lowBlueZ) {
                                                        lowBlueZ = z2;
                                                    }
                                                    if (z2 > highBlueZ) {
                                                        highBlueZ = z2;
                                                    }
                                                }
                                            }
                                            if ((x2 < xDim - 1) && (colorObjects[z2][i2+1] == 0) && ((buffer[z2][i2+1] == BRIGHT_BLUE) || (buffer[z2][i2+1] == BLUE))) {
                                                colorObjects[z2][i2+1] = blueObjectIndex;
                                                change = true;
                                                if (!bigBlueFraction) {
                                                    blueCount++;
                                                    centerBlueX += (x2 + 1);
                                                    centerBlueY += y2;
                                                    centerBlueZ += z2;
                                                    if ((x2 + 1) < lowBlueX) {
                                                        lowBlueX = x2 + 1;   
                                                    }
                                                    if ((x2 + 1) > highBlueX) {
                                                        highBlueX = x2 + 1;
                                                    }
                                                    if (y2 < lowBlueY) {
                                                        lowBlueY = y2;
                                                    }
                                                    if (y2 > highBlueY) {
                                                        highBlueY = y2;
                                                    }
                                                    if (z2 < lowBlueZ) {
                                                        lowBlueZ = z2;
                                                    }
                                                    if (z2 > highBlueZ) {
                                                        highBlueZ = z2;
                                                    }
                                                }
                                            }
                                            if ((y2 > 0)&& (colorObjects[z2][i2-xDim] == 0) && ((buffer[z2][i2-xDim] == BRIGHT_BLUE) || (buffer[z2][i2-xDim] == BLUE))) {
                                                colorObjects[z2][i2-xDim] = blueObjectIndex;
                                                change = true;
                                                if (!bigBlueFraction) {
                                                    blueCount++;
                                                    centerBlueX += x2;
                                                    centerBlueY += (y2 - 1);
                                                    centerBlueZ += z2;
                                                    if (x2 < lowBlueX) {
                                                        lowBlueX = x2;
                                                    }
                                                    if (x2 > highBlueX) {
                                                        highBlueX = x2;
                                                    }
                                                    if ((y2 - 1) < lowBlueY) {
                                                        lowBlueY = y2 - 1;   
                                                    }
                                                    if ((y2 - 1) > highBlueY) {
                                                        highBlueY = y2 - 1;
                                                    }
                                                    if (z2 < lowBlueZ) {
                                                        lowBlueZ = z2;
                                                    }
                                                    if (z2 > highBlueZ) {
                                                        highBlueZ = z2;
                                                    }
                                                }
                                            }
                                            if ((y2 < yDim - 1) && (colorObjects[z2][i2+xDim] == 0) && ((buffer[z2][i2+xDim] == BRIGHT_BLUE) || (buffer[z2][i2+xDim] == BLUE))) {
                                                colorObjects[z2][i2+xDim] = blueObjectIndex;
                                                change = true;
                                                if (!bigBlueFraction) {
                                                    blueCount++;
                                                    centerBlueX += x2;
                                                    centerBlueY += (y2 + 1);
                                                    centerBlueZ += z2;
                                                    if (x2 < lowBlueX) {
                                                        lowBlueX = x2;
                                                    }
                                                    if (x2 > highBlueX) {
                                                        highBlueX = x2;
                                                    }
                                                    if ((y2 + 1) < lowBlueY) {
                                                        lowBlueY = y2 + 1;   
                                                    }
                                                    if ((y2 + 1) > highBlueY) {
                                                        highBlueY = y2 + 1;
                                                    }
                                                    if (z2 < lowBlueZ) {
                                                        lowBlueZ = z2;
                                                    }
                                                    if (z2 > highBlueZ) {
                                                        highBlueZ = z2;
                                                    }
                                                }
                                            }
                                            if ((z2 > 0) && (colorObjects[z2-1][i2] == 0) && ((buffer[z2-1][i2] == BRIGHT_BLUE) || (buffer[z2-1][i2] == BLUE))) {
                                                colorObjects[z2-1][i2] = blueObjectIndex;
                                                change = true;
                                                if (!bigBlueFraction) {
                                                    blueCount++;
                                                    centerBlueX += x2;
                                                    centerBlueY += y2;
                                                    centerBlueZ += (z2 - 1);
                                                    if (x2 < lowBlueX) {
                                                        lowBlueX = x2;
                                                    }
                                                    if (x2 > highBlueX) {
                                                        highBlueX = x2;
                                                    }
                                                    if (y2 < lowBlueY) {
                                                        lowBlueY = y2;
                                                    }
                                                    if (y2 > highBlueY) {
                                                        highBlueY = y2;
                                                    }
                                                    if ((z2 - 1) < lowBlueZ) {
                                                        lowBlueZ = z2 - 1;   
                                                    }
                                                    if ((z2 - 1) > highBlueZ) {
                                                        highBlueZ = z2 - 1;
                                                    }
                                                }
                                            }
                                            if ((z2 < zDim - 1) && (colorObjects[z2+1][i2] == 0) && ((buffer[z2+1][i2] == BRIGHT_BLUE) || (buffer[z2+1][i2] == BLUE))) {
                                                colorObjects[z2+1][i2] = blueObjectIndex;
                                                change = true;
                                                if (!bigBlueFraction) {
                                                    blueCount++;
                                                    centerBlueX += x2;
                                                    centerBlueY += y2;
                                                    centerBlueZ += (z2 + 1);
                                                    if (x2 < lowBlueX) {
                                                        lowBlueX = x2;
                                                    }
                                                    if (x2 > highBlueX) {
                                                        highBlueX = x2;
                                                    }
                                                    if (y2 < lowBlueY) {
                                                        lowBlueY = y2;
                                                    }
                                                    if (y2 > highBlueY) {
                                                        highBlueY = y2;
                                                    }
                                                    if ((z2 + 1) < lowBlueZ) {
                                                        lowBlueZ = z2 + 1;   
                                                    }
                                                    if ((z2 + 1) > highBlueZ) {
                                                        highBlueZ = z2 + 1;
                                                    }
                                                }
                                            }
                                            if ((x2 > 0) && (y2 > 0) && (colorObjects[z2][i2-xDim-1] == 0) && ((buffer[z2][i2-xDim-1] == BRIGHT_BLUE) || (buffer[z2][i2-xDim-1] == BLUE))) {
                                                colorObjects[z2][i2-xDim-1] = blueObjectIndex;
                                                change = true;
                                                if (!bigBlueFraction) {
                                                    blueCount++;
                                                    centerBlueX += (x2 - 1);
                                                    centerBlueY += (y2 - 1);
                                                    centerBlueZ += z2;
                                                    if ((x2 - 1) < lowBlueX) {
                                                        lowBlueX = x2 - 1;   
                                                    }
                                                    if ((x2 - 1) > highBlueX) {
                                                        highBlueX = x2 - 1;
                                                    }
                                                    if ((y2 - 1) < lowBlueY) {
                                                        lowBlueY = y2 - 1;   
                                                    }
                                                    if ((y2 - 1) > highBlueY) {
                                                        highBlueY = y2 - 1;
                                                    }
                                                    if (z2 < lowBlueZ) {
                                                        lowBlueZ = z2;
                                                    }
                                                    if (z2 > highBlueZ) {
                                                        highBlueZ = z2;
                                                    }
                                                }
                                            }
                                            if ((x2 > 0) && (y2 < yDim - 1) && (colorObjects[z2][i2+xDim-1] == 0) && ((buffer[z2][i2+xDim-1] == BRIGHT_BLUE) || (buffer[z2][i2+xDim-1] == BLUE))) {
                                                colorObjects[z2][i2+xDim-1] = blueObjectIndex;
                                                change = true;
                                                if (!bigBlueFraction) {
                                                    blueCount++;
                                                    centerBlueX += (x2 - 1);
                                                    centerBlueY += (y2 + 1);
                                                    centerBlueZ += z2;
                                                    if ((x2 - 1) < lowBlueX) {
                                                        lowBlueX = x2 - 1;   
                                                    }
                                                    if ((x2 - 1) > highBlueX) {
                                                        highBlueX = x2 - 1;
                                                    }
                                                    if ((y2 + 1) < lowBlueY) {
                                                        lowBlueY = y2 + 1;   
                                                    }
                                                    if ((y2 + 1) > highBlueY) {
                                                        highBlueY = y2 + 1;
                                                    }
                                                    if (z2 < lowBlueZ) {
                                                        lowBlueZ = z2;
                                                    }
                                                    if (z2 > highBlueZ) {
                                                        highBlueZ = z2;
                                                    }
                                                }
                                            }
                                            if ((x2 < xDim-1) && (y2 > 0) && (colorObjects[z2][i2-xDim+1] == 0) && ((buffer[z2][i2-xDim+1] == BRIGHT_BLUE) || (buffer[z2][i2-xDim+1] == BLUE))) {
                                                colorObjects[z2][i2-xDim+1] = blueObjectIndex;
                                                change = true;
                                                if (!bigBlueFraction) {
                                                    blueCount++;
                                                    centerBlueX += (x2 + 1);
                                                    centerBlueY += (y2 - 1);
                                                    centerBlueZ += z2;
                                                    if ((x2 + 1) < lowBlueX) {
                                                        lowBlueX = x2 + 1;   
                                                    }
                                                    if ((x2 + 1) > highBlueX) {
                                                        highBlueX = x2 + 1;
                                                    }
                                                    if ((y2 - 1) < lowBlueY) {
                                                        lowBlueY = y2 - 1;   
                                                    }
                                                    if ((y2 - 1) > highBlueY) {
                                                        highBlueY = y2 - 1;
                                                    }
                                                    if (z2 < lowBlueZ) {
                                                        lowBlueZ = z2;
                                                    }
                                                    if (z2 > highBlueZ) {
                                                        highBlueZ = z2;
                                                    }
                                                }
                                            }
                                            if ((x2 < xDim - 1) && (y2 < yDim - 1) && (colorObjects[z2][i2+xDim+1] == 0) && ((buffer[z2][i2+xDim+1] == BRIGHT_BLUE) || (buffer[z2][i2+xDim+1] == BLUE))) {
                                                colorObjects[z2][i2+xDim+1] = blueObjectIndex;
                                                change = true;
                                                if (!bigBlueFraction) {
                                                    blueCount++;
                                                    centerBlueX += (x2 + 1);
                                                    centerBlueY += (y2 + 1);
                                                    centerBlueZ += z2;
                                                    if ((x2 + 1) < lowBlueX) {
                                                        lowBlueX = x2 + 1;   
                                                    }
                                                    if ((x2 + 1) > highBlueX) {
                                                        highBlueX = x2 + 1;
                                                    }
                                                    if ((y2 + 1) < lowBlueY) {
                                                        lowBlueY = y2 + 1;   
                                                    }
                                                    if ((y2 + 1) > highBlueY) {
                                                        highBlueY = y2 + 1;
                                                    }
                                                    if (z2 < lowBlueZ) {
                                                        lowBlueZ = z2;
                                                    }
                                                    if (z2 > highBlueZ) {
                                                        highBlueZ = z2;
                                                    }
                                                }
                                            }
                                            if ((x2 > 0) && (z2 > 0) && (colorObjects[z2-1][i2-1] == 0) && ((buffer[z2-1][i2-1] == BRIGHT_BLUE) || (buffer[z2-1][i2-1] == BLUE))) {
                                                colorObjects[z2-1][i2-1] = blueObjectIndex;
                                                change = true;
                                                if (!bigBlueFraction) {
                                                    blueCount++;
                                                    centerBlueX += (x2 - 1);
                                                    centerBlueY += y2;
                                                    centerBlueZ += (z2 - 1);
                                                    if ((x2 - 1) < lowBlueX) {
                                                        lowBlueX = x2 - 1;   
                                                    }
                                                    if ((x2 - 1) > highBlueX) {
                                                        highBlueX = x2 - 1;
                                                    }
                                                    if (y2 < lowBlueY) {
                                                        lowBlueY = y2;
                                                    }
                                                    if (y2 > highBlueY) {
                                                        highBlueY = y2;
                                                    }
                                                    if ((z2 - 1) < lowBlueZ) {
                                                        lowBlueZ = z2 - 1;   
                                                    }
                                                    if ((z2 - 1) > highBlueZ) {
                                                        highBlueZ = z2 - 1;
                                                    }
                                                }
                                            }
                                            if ((x2 > 0) && (z2 < zDim - 1) && (colorObjects[z2+1][i2-1] == 0) && ((buffer[z2+1][i2-1] == BRIGHT_BLUE) || (buffer[z2+1][i2-1] == BLUE))) {
                                                colorObjects[z2+1][i2-1] = blueObjectIndex;
                                                change = true;
                                                if (!bigBlueFraction) {
                                                    blueCount++;
                                                    centerBlueX += (x2 - 1);
                                                    centerBlueY += y2;
                                                    centerBlueZ += (z2 + 1);
                                                    if ((x2 - 1) < lowBlueX) {
                                                        lowBlueX = x2 - 1;   
                                                    }
                                                    if ((x2 - 1) > highBlueX) {
                                                        highBlueX = x2 - 1;
                                                    }
                                                    if (y2 < lowBlueY) {
                                                        lowBlueY = y2;
                                                    }
                                                    if (y2 > highBlueY) {
                                                        highBlueY = y2;
                                                    }
                                                    if ((z2 + 1) < lowBlueZ) {
                                                        lowBlueZ = z2 + 1;   
                                                    }
                                                    if ((z2 + 1) > highBlueZ) {
                                                        highBlueZ = z2 + 1;
                                                    }
                                                }
                                            }
                                            if ((x2 < xDim-1) && (z2 > 0) && (colorObjects[z2-1][i2+1] == 0) && ((buffer[z2-1][i2+1] == BRIGHT_BLUE) || (buffer[z2-1][i2+1] == BLUE))) {
                                                colorObjects[z2-1][i2+1] = blueObjectIndex;
                                                change = true;
                                                if (!bigBlueFraction) {
                                                    blueCount++;
                                                    centerBlueX += (x2 + 1);
                                                    centerBlueY += y2;
                                                    centerBlueZ += (z2 - 1);
                                                    if ((x2 + 1) < lowBlueX) {
                                                        lowBlueX = x2 + 1;   
                                                    }
                                                    if ((x2 + 1) > highBlueX) {
                                                        highBlueX = x2 + 1;
                                                    }
                                                    if (y2 < lowBlueY) {
                                                        lowBlueY = y2;
                                                    }
                                                    if (y2 > highBlueY) {
                                                        highBlueY = y2;
                                                    }
                                                    if ((z2 - 1) < lowBlueZ) {
                                                        lowBlueZ = z2 - 1;   
                                                    }
                                                    if ((z2 - 1) > highBlueZ) {
                                                        highBlueZ = z2 - 1;
                                                    }
                                                }
                                            }
                                            if ((x2 < xDim - 1) && (z2 < zDim - 1) && (colorObjects[z2+1][i2+1] == 0) && ((buffer[z2+1][i2+1] == BRIGHT_BLUE) || (buffer[z2+1][i2+1] == BLUE))) {
                                                colorObjects[z2+1][i2+1] = blueObjectIndex;
                                                change = true;
                                                if (!bigBlueFraction) {
                                                    blueCount++;
                                                    centerBlueX += (x2 + 1);
                                                    centerBlueY += y2;
                                                    centerBlueZ += (z2 + 1);
                                                    if ((x2 + 1) < lowBlueX) {
                                                        lowBlueX = x2 + 1;   
                                                    }
                                                    if ((x2 + 1) > highBlueX) {
                                                        highBlueX = x2 + 1;
                                                    }
                                                    if (y2 < lowBlueY) {
                                                        lowBlueY = y2;
                                                    }
                                                    if (y2 > highBlueY) {
                                                        highBlueY = y2;
                                                    }
                                                    if ((z2 + 1) < lowBlueZ) {
                                                        lowBlueZ = z2 + 1;   
                                                    }
                                                    if ((z2 + 1) > highBlueZ) {
                                                        highBlueZ = z2 + 1;
                                                    }
                                                }
                                            }
                                            if ((y2 > 0) && (z2 > 0) && (colorObjects[z2-1][i2-xDim] == 0) && ((buffer[z2-1][i2-xDim] == BRIGHT_BLUE) || (buffer[z2-1][i2-xDim] == BLUE))) {
                                                colorObjects[z2-1][i2-xDim] = blueObjectIndex;
                                                change = true;
                                                if (!bigBlueFraction) {
                                                    blueCount++;
                                                    centerBlueX += x2;
                                                    centerBlueY += (y2 - 1);
                                                    centerBlueZ += (z2 - 1);
                                                    if (x2 < lowBlueX) {
                                                        lowBlueX = x2;
                                                    }
                                                    if (x2 > highBlueX) {
                                                        highBlueX = x2;
                                                    }
                                                    if ((y2 - 1) < lowBlueY) {
                                                        lowBlueY = y2 - 1;   
                                                    }
                                                    if ((y2 - 1) > highBlueY) {
                                                        highBlueY = y2 - 1;
                                                    }
                                                    if ((z2 - 1) < lowBlueZ) {
                                                        lowBlueZ = z2 - 1;   
                                                    }
                                                    if ((z2 - 1) > highBlueZ) {
                                                        highBlueZ = z2 - 1;
                                                    }
                                                }
                                            }
                                            if ((y2 > 0) && (z2 < zDim - 1) && (colorObjects[z2+1][i2-xDim] == 0) && ((buffer[z2+1][i2-xDim] == BRIGHT_BLUE) || (buffer[z2+1][i2-xDim] == BLUE))) {
                                                colorObjects[z2+1][i2-xDim] = blueObjectIndex;
                                                change = true;
                                                if (!bigBlueFraction) {
                                                    blueCount++;
                                                    centerBlueX += x2;
                                                    centerBlueY += (y2 - 1);
                                                    centerBlueZ += (z2 + 1);
                                                    if (x2 < lowBlueX) {
                                                        lowBlueX = x2;
                                                    }
                                                    if (x2 > highBlueX) {
                                                        highBlueX = x2;
                                                    }
                                                    if ((y2 - 1) < lowBlueY) {
                                                        lowBlueY = y2 - 1;   
                                                    }
                                                    if ((y2 - 1) > highBlueY) {
                                                        highBlueY = y2 - 1;
                                                    }
                                                    if ((z2 + 1) < lowBlueZ) {
                                                        lowBlueZ = z2 + 1;   
                                                    }
                                                    if ((z2 + 1) > highBlueZ) {
                                                        highBlueZ = z2 + 1;
                                                    }
                                                }
                                            }
                                            if ((y2 < yDim-1) && (z2 > 0) && (colorObjects[z2-1][i2+xDim] == 0) && ((buffer[z2-1][i2+xDim] == BRIGHT_BLUE) || (buffer[z2-1][i2+xDim] == BLUE))) {
                                                colorObjects[z2-1][i2+xDim] = blueObjectIndex;
                                                change = true;
                                                if (!bigBlueFraction) {
                                                    blueCount++;
                                                    centerBlueX += x2;
                                                    centerBlueY += (y2 + 1);
                                                    centerBlueZ += (z2 - 1);
                                                    if (x2 < lowBlueX) {
                                                        lowBlueX = x2;
                                                    }
                                                    if (x2 > highBlueX) {
                                                        highBlueX = x2;
                                                    }
                                                    if ((y2 + 1) < lowBlueY) {
                                                        lowBlueY = y2 + 1;   
                                                    }
                                                    if ((y2 + 1) > highBlueY) {
                                                        highBlueY = y2 + 1;
                                                    }
                                                    if ((z2 - 1) < lowBlueZ) {
                                                        lowBlueZ = z2 - 1;   
                                                    }
                                                    if ((z2 - 1) > highBlueZ) {
                                                        highBlueZ = z2 - 1;
                                                    }
                                                }
                                            }
                                            if ((y2 < yDim - 1) && (z2 < zDim - 1) && (colorObjects[z2+1][i2+xDim] == 0) && ((buffer[z2+1][i2+xDim] == BRIGHT_BLUE) || (buffer[z2+1][i2+xDim] == BLUE))) {
                                                colorObjects[z2+1][i2+xDim] = blueObjectIndex;
                                                change = true;
                                                if (!bigBlueFraction) {
                                                    blueCount++;
                                                    centerBlueX += x2;
                                                    centerBlueY += (y2 + 1);
                                                    centerBlueZ += (z2 + 1);
                                                    if (x2 < lowBlueX) {
                                                        lowBlueX = x2;
                                                    }
                                                    if (x2 > highBlueX) {
                                                        highBlueX = x2;
                                                    }
                                                    if ((y2 + 1) < lowBlueY) {
                                                        lowBlueY = y2 + 1;   
                                                    }
                                                    if ((y2 + 1) > highBlueY) {
                                                        highBlueY = y2 + 1;
                                                    }
                                                    if ((z2 + 1) < lowBlueZ) {
                                                        lowBlueZ = z2 + 1;   
                                                    }
                                                    if ((z2 + 1) > highBlueZ) {
                                                        highBlueZ = z2 + 1;
                                                    }
                                                }
                                            }
                                            if ((x2 > 0) && (y2 > 0) && (z2 > 0)&& (colorObjects[z2-1][i2-xDim-1] == 0) && ((buffer[z2-1][i2-xDim-1] == BRIGHT_BLUE) || (buffer[z2-1][i2-xDim-1] == BLUE))) {
                                                colorObjects[z2-1][i2-xDim-1] = blueObjectIndex;
                                                change = true;
                                                if (!bigBlueFraction) {
                                                    blueCount++;
                                                    centerBlueX += (x2 - 1);
                                                    centerBlueY += (y2 - 1);
                                                    centerBlueZ += (z2 - 1);
                                                    if ((x2 - 1) < lowBlueX) {
                                                        lowBlueX = x2 - 1;   
                                                    }
                                                    if ((x2 - 1) > highBlueX) {
                                                        highBlueX = x2 - 1;
                                                    }
                                                    if ((y2 - 1) < lowBlueY) {
                                                        lowBlueY = y2 - 1;   
                                                    }
                                                    if ((y2 - 1) > highBlueY) {
                                                        highBlueY = y2 - 1;
                                                    }
                                                    if ((z2 - 1) < lowBlueZ) {
                                                        lowBlueZ = z2 - 1;   
                                                    }
                                                    if ((z2 - 1) > highBlueZ) {
                                                        highBlueZ = z2 - 1;
                                                    }
                                                }
                                            }
                                            if ((x2 > 0) && (y2 > 0) && (z2 < zDim - 1) && (colorObjects[z2+1][i2-xDim-1] == 0) && ((buffer[z2+1][i2-xDim-1] == BRIGHT_BLUE) ||(buffer[z2+1][i2-xDim-1] == BLUE))) {
                                                colorObjects[z2+1][i2-xDim-1] = blueObjectIndex;
                                                change = true;
                                                if (!bigBlueFraction) {
                                                    blueCount++;
                                                    centerBlueX += (x2 - 1);
                                                    centerBlueY += (y2 - 1);
                                                    centerBlueZ += (z2 + 1);
                                                    if ((x2 - 1) < lowBlueX) {
                                                        lowBlueX = x2 - 1;   
                                                    }
                                                    if ((x2 - 1) > highBlueX) {
                                                        highBlueX = x2 - 1;
                                                    }
                                                    if ((y2 - 1) < lowBlueY) {
                                                        lowBlueY = y2 - 1;   
                                                    }
                                                    if ((y2 - 1) > highBlueY) {
                                                        highBlueY = y2 - 1;
                                                    }
                                                    if ((z2 + 1) < lowBlueZ) {
                                                        lowBlueZ = z2 + 1;   
                                                    }
                                                    if ((z2 + 1) > highBlueZ) {
                                                        highBlueZ = z2 + 1;
                                                    }
                                                }
                                            }
                                            if ((x2 > 0) && (y2 < yDim - 1) && (z2 > 0)&& (colorObjects[z2-1][i2+xDim-1] == 0) && ((buffer[z2-1][i2+xDim-1] == BRIGHT_BLUE) || (buffer[z2-1][i2+xDim-1] == BLUE))) {
                                                colorObjects[z2-1][i2+xDim-1] = blueObjectIndex;
                                                change = true;
                                                if (!bigBlueFraction) {
                                                    blueCount++;
                                                    centerBlueX += (x2 - 1);
                                                    centerBlueY += (y2 + 1);
                                                    centerBlueZ += (z2 - 1);
                                                    if ((x2 - 1) < lowBlueX) {
                                                        lowBlueX = x2 - 1;   
                                                    }
                                                    if ((x2 - 1) > highBlueX) {
                                                        highBlueX = x2 - 1;
                                                    }
                                                    if ((y2 + 1) < lowBlueY) {
                                                        lowBlueY = y2 + 1;   
                                                    }
                                                    if ((y2 + 1) > highBlueY) {
                                                        highBlueY = y2 + 1;
                                                    }
                                                    if ((z2 - 1) < lowBlueZ) {
                                                        lowBlueZ = z2 - 1;   
                                                    }
                                                    if ((z2 - 1) > highBlueZ) {
                                                        highBlueZ = z2 - 1;
                                                    }
                                                }
                                            }
                                            if ((x2 > 0) && (y2 < yDim - 1) && (z2 < zDim - 1) && (colorObjects[z2+1][i2+xDim-1] == 0) && ((buffer[z2+1][i2+xDim-1] == BRIGHT_BLUE) || (buffer[z2+1][i2+xDim-1] == BLUE))) {
                                                colorObjects[z2+1][i2+xDim-1] = blueObjectIndex;
                                                change = true;
                                                if (!bigBlueFraction) {
                                                    blueCount++;
                                                    centerBlueX += (x2 - 1);
                                                    centerBlueY += (y2 + 1);
                                                    centerBlueZ += (z2 + 1);
                                                    if ((x2 - 1) < lowBlueX) {
                                                        lowBlueX = x2 - 1;   
                                                    }
                                                    if ((x2 - 1) > highBlueX) {
                                                        highBlueX = x2 - 1;
                                                    }
                                                    if ((y2 + 1) < lowBlueY) {
                                                        lowBlueY = y2 + 1;   
                                                    }
                                                    if ((y2 + 1) > highBlueY) {
                                                        highBlueY = y2 + 1;
                                                    }
                                                    if ((z2 + 1) < lowBlueZ) {
                                                        lowBlueZ = z2 + 1;   
                                                    }
                                                    if ((z2 + 1) > highBlueZ) {
                                                        highBlueZ = z2 + 1;
                                                    }
                                                }
                                            }
                                            if ((x2 < xDim-1) && (y2 > 0) && (z2 > 0) && (colorObjects[z2-1][i2-xDim+1] == 0) && ((buffer[z2-1][i2-xDim+1] == BRIGHT_BLUE) || (buffer[z2-1][i2-xDim+1] == BLUE))) {
                                                colorObjects[z2-1][i2-xDim+1] = blueObjectIndex;
                                                change = true;
                                                if (!bigBlueFraction) {
                                                    blueCount++;
                                                    centerBlueX += (x2 + 1);
                                                    centerBlueY += (y2 - 1);
                                                    centerBlueZ += (z2 - 1);
                                                    if ((x2 + 1) < lowBlueX) {
                                                        lowBlueX = x2 + 1;   
                                                    }
                                                    if ((x2 + 1) > highBlueX) {
                                                        highBlueX = x2 + 1;
                                                    }
                                                    if ((y2 - 1) < lowBlueY) {
                                                        lowBlueY = y2 - 1;   
                                                    }
                                                    if ((y2 - 1) > highBlueY) {
                                                        highBlueY = y2 - 1;
                                                    }
                                                    if ((z2 - 1) < lowBlueZ) {
                                                        lowBlueZ = z2 - 1;   
                                                    }
                                                    if ((z2 - 1) > highBlueZ) {
                                                        highBlueZ = z2 - 1;
                                                    }
                                                }
                                            }
                                            if ((x2 < xDim-1) && (y2 > 0) && (z2 < zDim - 1) && (colorObjects[z2+1][i2-xDim+1] == 0) && ((buffer[z2+1][i2-xDim+1] == BRIGHT_BLUE) || (buffer[z2+1][i2-xDim+1] == BLUE))) {
                                                colorObjects[z2+1][i2-xDim+1] = blueObjectIndex;
                                                change = true;
                                                if (!bigBlueFraction) {
                                                    blueCount++;
                                                    centerBlueX += (x2 + 1);
                                                    centerBlueY += (y2 - 1);
                                                    centerBlueZ += (z2 + 1);
                                                    if ((x2 + 1) < lowBlueX) {
                                                        lowBlueX = x2 + 1;   
                                                    }
                                                    if ((x2 + 1) > highBlueX) {
                                                        highBlueX = x2 + 1;
                                                    }
                                                    if ((y2 - 1) < lowBlueY) {
                                                        lowBlueY = y2 - 1;   
                                                    }
                                                    if ((y2 - 1) > highBlueY) {
                                                        highBlueY = y2 - 1;
                                                    }
                                                    if ((z2 + 1) < lowBlueZ) {
                                                        lowBlueZ = z2 + 1;   
                                                    }
                                                    if ((z2 + 1) > highBlueZ) {
                                                        highBlueZ = z2 + 1;
                                                    }
                                                }
                                            }
                                            if ((x2 < xDim - 1) && (y2 < yDim - 1) && (z2 > 0) && (colorObjects[z2-1][i2+xDim+1] == 0) && ((buffer[z2-1][i2+xDim+1] == BRIGHT_BLUE) ||(buffer[z2-1][i2+xDim+1] == BLUE))) {
                                                colorObjects[z2-1][i2+xDim+1] = blueObjectIndex;
                                                change = true;
                                                if (!bigBlueFraction) {
                                                    blueCount++;
                                                    centerBlueX += (x2 + 1);
                                                    centerBlueY += (y2 + 1);
                                                    centerBlueZ += (z2 - 1);
                                                    if ((x2 + 1) < lowBlueX) {
                                                        lowBlueX = x2 + 1;   
                                                    }
                                                    if ((x2 + 1) > highBlueX) {
                                                        highBlueX = x2 + 1;
                                                    }
                                                    if ((y2 + 1) < lowBlueY) {
                                                        lowBlueY = y2 + 1;   
                                                    }
                                                    if ((y2 + 1) > highBlueY) {
                                                        highBlueY = y2 + 1;
                                                    }
                                                    if ((z2 - 1) < lowBlueZ) {
                                                        lowBlueZ = z2 - 1;   
                                                    }
                                                    if ((z2 - 1) > highBlueZ) {
                                                        highBlueZ = z2 - 1;
                                                    }
                                                }
                                            }
                                            if ((x2 < xDim - 1) && (y2 < yDim - 1) && (z2 < zDim - 1) && (colorObjects[z2+1][i2+xDim+1] == 0) && ((buffer[z2+1][i2+xDim+1] == BRIGHT_BLUE) || (buffer[z2+1][i2+xDim+1] == BLUE))) {
                                                colorObjects[z2+1][i2+xDim+1] = blueObjectIndex;
                                                change = true;
                                                if (!bigBlueFraction) {
                                                    blueCount++;
                                                    centerBlueX += (x2 + 1);
                                                    centerBlueY += (y2 + 1);
                                                    centerBlueZ += (z2 + 1);
                                                    if ((x2 + 1) < lowBlueX) {
                                                        lowBlueX = x2 + 1;   
                                                    }
                                                    if ((x2 + 1) > highBlueX) {
                                                        highBlueX = x2 + 1;
                                                    }
                                                    if ((y2 + 1) < lowBlueY) {
                                                        lowBlueY = y2 + 1;   
                                                    }
                                                    if ((y2 + 1) > highBlueY) {
                                                        highBlueY = y2 + 1;
                                                    }
                                                    if ((z2 + 1) < lowBlueZ) {
                                                        lowBlueZ = z2 + 1;   
                                                    }
                                                    if ((z2 + 1) > highBlueZ) {
                                                        highBlueZ = z2 + 1;
                                                    }
                                                }
                                            }
                                        } // if ((colorObjects[i2] > 0) && ((buffer[i2] == BRIGHT_BLUE) || (buffer[i2] == BLUE)))
                                    } // for (x2 = xLow; x2 <= xHigh; x2++)
                                } // for (y2 = yLow; y2 <= yHigh; y2++)
                            } // for (z2 = zLow; z2 <= zHigh; z2++)
                        } // while (change)
                        if (! bigBlueFraction) {
                            centerBlueX = Math.round((float)centerBlueX/blueCount);
                            centerBlueY = Math.round((float)centerBlueY/blueCount);
                            centerBlueZ = Math.round((float)centerBlueZ/blueCount);
                            blueCountList.add(blueCount);
                            blueCenterXList.add((short)centerBlueX);
                            blueCenterYList.add((short)centerBlueY);
                            blueCenterZList.add((short)centerBlueZ);
                            blueXWidthList.add((short)(highBlueX - lowBlueX + 1));
                            blueYWidthList.add((short)(highBlueY - lowBlueY + 1));
                            blueZWidthList.add((short)(highBlueZ - lowBlueZ + 1));
                        }
                    } // else if ((colorObjects[i] == 0) && ((buffer[i] == BLUE) || (buffer[i] == BRIGHT_BLUE)))
                } // for (x = 0; x < xDim; x++)
            } // for (y = 0; y < yDim; y++) 
        } // for (z = 0; z < zDim; z++)
    }
                        
    
    /**
     * Reads two unsigned bytes from file.
     *
     * @param      bigEndian  <code>true</code> indicates big endian byte order, <code>false</code> indicates little
     *                        endian.
     *
     * @return     The value of unsigned short read from the file returned as an int.
     *
     * @exception  IOException  if there is an error reading the file
     */
    public final int getUnsignedShort(boolean bigEndian) throws IOException {
        byte[] buffer = new byte[2];
        raFile.readFully(buffer);

        if (bigEndian) {
            return (((buffer[0] & 0xff) << 8) | (buffer[1] & 0xff)); // Big Endian
        } else {
            return (((buffer[1] & 0xff) << 8) | (buffer[0] & 0xff)); // Little Endian
        }
    }
    
    /**
     * Reads four signed bytes from file.
     *
     * @param      bigEndian  <code>true</code> indicates big endian byte order, <code>false</code> indicates little
     *                        endian.
     *
     * @return     The value of the integer read from the file.
     *
     * @exception  IOException  if there is an error reading the file
     */
    public final int getInt(boolean bigEndian) throws IOException {
        byte[] buffer = new byte[4];
        raFile.readFully(buffer);

        if (bigEndian) {
            return (((buffer[0] & 0xff) << 24) | ((buffer[1] & 0xff) << 16) | ((buffer[2] & 0xff) << 8) |
                        (buffer[3] & 0xff)); // Big Endian
        } else {
            return (((buffer[3] & 0xff) << 24) | ((buffer[2] & 0xff) << 16) | ((buffer[1] & 0xff) << 8) |
                        (buffer[0] & 0xff));
        }
    }
    
    /**
     * Reads four unsigned bytes from file.
     *
     * @param      bigEndian  <code>true</code> indicates big endian byte order, <code>false</code> indicates little
     *                        endian.
     *
     * @return     The value of the integer read from the file.
     *
     * @exception  IOException  if there is an error reading the file
     */
    public final long getUInt(boolean bigEndian) throws IOException {
        byte[] buffer = new byte[4];
        raFile.readFully(buffer);

        if (bigEndian) {
            return (((buffer[0] & 0xffL) << 24) | ((buffer[1] & 0xffL) << 16) | ((buffer[2] & 0xffL) << 8) |
                        (buffer[3] & 0xffL)); // Big Endian
        } else {
            return (((buffer[3] & 0xffL) << 24) | ((buffer[2] & 0xffL) << 16) | ((buffer[1] & 0xffL) << 8) |
                        (buffer[0] & 0xffL));
        }
    }
    
    /**
     * Reads eight unsigned bytes from file.
     *
     * @param      bigEndian  <code>true</code> indicates big endian byte order, <code>false</code> indicates little
     *                        endian.
     *
     * @return     The value of the double read from the file.
     *
     * @exception  IOException  if there is an error reading the file
     */
    public final double getDouble(boolean bigEndian) throws IOException {
        byte[] buffer = new byte[8];
        raFile.readFully(buffer);

        long tmpLong;

        if (bigEndian) {
            tmpLong = (((buffer[0] & 0xffL) << 56) | ((buffer[1] & 0xffL) << 48) | ((buffer[2] & 0xffL) << 40) |
                           ((buffer[3] & 0xffL) << 32) | ((buffer[4] & 0xffL) << 24) | ((buffer[5] & 0xffL) << 16) |
                           ((buffer[6] & 0xffL) << 8) | (buffer[7] & 0xffL));

            return (Double.longBitsToDouble(tmpLong));
        } else {
            tmpLong = (((buffer[7] & 0xffL) << 56) | ((buffer[6] & 0xffL) << 48) | ((buffer[5] & 0xffL) << 40) |
                           ((buffer[4] & 0xffL) << 32) | ((buffer[3] & 0xffL) << 24) | ((buffer[2] & 0xffL) << 16) |
                           ((buffer[1] & 0xffL) << 8) | (buffer[0] & 0xffL));

            return (Double.longBitsToDouble(tmpLong));
        }
    }


    /**
     * Reads four unsigned bytes from file.
     *
     * @param      bigEndian  <code>true</code> indicates big endian byte order, <code>false</code> indicates little
     *                        endian.
     *
     * @return     The value of the float read from the file.
     *
     * @exception  IOException  if there is an error reading the file
     */
    public final float getFloat(boolean bigEndian) throws IOException {
        byte[] buffer = new byte[4];
        raFile.readFully(buffer);

        int tmpInt;

        if (bigEndian) {
            tmpInt = (((buffer[0] & 0xff) << 24) | ((buffer[1] & 0xff) << 16) | ((buffer[2] & 0xff) << 8) |
                          (buffer[3] & 0xff));

            return (Float.intBitsToFloat(tmpInt));
        } else {
            tmpInt = (((buffer[3] & 0xff) << 24) | ((buffer[2] & 0xff) << 16) | ((buffer[1] & 0xff) << 8) |
                          (buffer[0] & 0xff));

            return (Float.intBitsToFloat(tmpInt));
        }
    }
    
    /**
     * Reads two byte signed short from file.
     *
     * @param      bigEndian  <code>true</code> indicates big endian byte order, <code>false</code> indicates little
     *                        endian.
     *
     * @return     The value of signed short read from the file returned as an int.
     *
     * @exception  IOException  if there is an error reading the file
     */
    public final int getSignedShort(boolean bigEndian) throws IOException {
        int b3 = 0;
        byte[] buffer = new byte[2];

        raFile.readFully(buffer);

        if (bigEndian) {
            b3 = ((buffer[0] & 0xff) << 8) | (buffer[1] & 0xff);
        } else {
            b3 = ((buffer[1] & 0xff) << 8) | (buffer[0] & 0xff);
        }

        if ((b3 & 0x8000) != 0) {
            b3 = b3 | 0xffff0000;
        }

        return b3;
    }


    /**
     * Reads a string from a file of given <code>length</code>.
     *
     * @param      length  Number of bytes that form the string.
     *
     * @return     The string read from the file.
     *
     * @exception  IOException  if there is an error reading the file
     */
    public final String getString(int length) throws IOException {

        if (length <= 0) {
            return new String("");
        }

        byte[] b = new byte[length];
        raFile.readFully(b);

        return new String(b);
    }
    
    /**
     * Reads and decodes IFDs (Image File Directory).
     *
     * @param      fileInfo  DOCUMENT ME!
     *
     * @return     DOCUMENT ME!
     *
     * @exception  IOException  if there is an error reading the file
     */
    private boolean openIFD() throws IOException {
        int i;
        int iExifStart = 0;
        int i1;
        int tag;
        int type;
        int count;
        int ecount;
        long[] valueArray = new long[MAX_IFD_LENGTH];
        int value_offset;
        int nDirEntries;
        float valueFloat = 0.0f;
        double valueDouble[] = new double[30];
        long saveLocus;
        long preExifLocus = 0L;
        //boolean debuggingFileIO = Preferences.debugLevel(Preferences.DEBUG_FILEIO);
        boolean debuggingFileIO = false;
        int exifDirEntries = 0;
        nDirEntries = getUnsignedShort(endianess);

        if (nDirEntries <= 0) {
            throw new IOException("First 2 IFD bytes are an illegal " + nDirEntries);
        }

        if (debuggingFileIO) {
            Preferences.debug("\nOpenIFD: Entries = " + nDirEntries + "\n", Preferences.DEBUG_FILEIO);
        }

        for (i = 0; i < nDirEntries + exifDirEntries; i++) {
            tag = getUnsignedShort(endianess);

            if (tag == 0) {
                throw new IOException("Tiff Zero Tag Error");
            } 

            type = getUnsignedShort(endianess);
            count = getInt(endianess);

            if ((type == SHORT) && (count == 1)) {
                valueArray[0] = getUnsignedShort(endianess);
                getUnsignedShort(endianess);
            } else if ((type == SHORT) && (count == 2)) {
                valueArray[0] = getUnsignedShort(endianess);
                valueArray[1] = getUnsignedShort(endianess);
            } else if ((type == SHORT) && (count >= 3)) {
                value_offset = getInt(endianess);

                saveLocus = raFile.getFilePointer();
                raFile.seek(value_offset);

                for (i1 = 0; ((i1 < count) && (i1 < MAX_IFD_LENGTH)); i1++) {
                    valueArray[i1] = getUnsignedShort(endianess);
                }

                raFile.seek(saveLocus);
            } else if (((type == LONG) || (type == IFD)) && (count == 1)) {
                valueArray[0] = getUInt(endianess);
            } else if (((type == LONG) || (type == IFD)) && (count >= 2)) {
                value_offset = getInt(endianess);

                saveLocus = raFile.getFilePointer();
                raFile.seek(value_offset);

                for (i1 = 0; ((i1 < count) && (i1 < MAX_IFD_LENGTH)); i1++) {
                    valueArray[i1] = getUInt(endianess);
                }

                raFile.seek(saveLocus);
            } else if ((type == SLONG) && (count == 1)) {
                valueArray[0] = getInt(endianess);
            } else if ((type == SLONG) && (count >= 2)) {
                value_offset = getInt(endianess);

                saveLocus = raFile.getFilePointer();
                raFile.seek(value_offset);

                for (i1 = 0; ((i1 < count) && (i1 < MAX_IFD_LENGTH)); i1++) {
                    valueArray[i1] = getInt(endianess);
                }

                raFile.seek(saveLocus);
            } else if ((type == RATIONAL) || (type == SRATIONAL)) {
                value_offset = getInt(endianess);

                saveLocus = raFile.getFilePointer();
                raFile.seek(value_offset);

                for (i1 = 0; ((i1 < (2 * count)) && (i1 < MAX_IFD_LENGTH)); i1 = i1 + 2) {
                    valueArray[i1] = getInt(endianess);
                    valueArray[i1 + 1] = getInt(endianess);
                }

                raFile.seek(saveLocus);
            } else if (type == DOUBLE) {
                value_offset = getInt(endianess);
                saveLocus = raFile.getFilePointer();
                raFile.seek(value_offset);
                for (i1 = 0; ((i1 < count) && (i1 < valueDouble.length)); i1++) {
                    valueDouble[i1] = getDouble(endianess);
                }
                raFile.seek(saveLocus);
            } else if (((type == BYTE) || (type == UNDEFINED) || (type == ASCII)) && (count == 0)) {
                raFile.seek(raFile.getFilePointer() + 4);
                // raFile.readUnsignedByte();
                // raFile.readUnsignedByte();
                // raFile.readUnsignedByte();
                // raFile.readUnsignedByte();
            } else if (((type == BYTE) || (type == UNDEFINED) || (type == ASCII)) && (count == 1)) {
                valueArray[0] = raFile.readUnsignedByte();
                raFile.seek(raFile.getFilePointer() + 3);
                // raFile.readUnsignedByte();
                // raFile.readUnsignedByte();
                // raFile.readUnsignedByte();
            } else if (((type == BYTE) || (type == UNDEFINED) || (type == ASCII)) && (count == 2)) {
                valueArray[0] = raFile.readUnsignedByte();
                valueArray[1] = raFile.readUnsignedByte();
                raFile.seek(raFile.getFilePointer() + 2);
                // raFile.readUnsignedByte();
                // raFile.readUnsignedByte();
            } else if (((type == BYTE) || (type == UNDEFINED) || (type == ASCII)) && (count == 3)) {
                valueArray[0] = raFile.readUnsignedByte();
                valueArray[1] = raFile.readUnsignedByte();
                valueArray[2] = raFile.readUnsignedByte();
                raFile.seek(raFile.getFilePointer() + 1);
                // raFile.readUnsignedByte();
            } else if (((type == BYTE) || (type == UNDEFINED) || (type == ASCII)) && (count == 4)) {
                valueArray[0] = raFile.readUnsignedByte();
                valueArray[1] = raFile.readUnsignedByte();
                valueArray[2] = raFile.readUnsignedByte();
                valueArray[3] = raFile.readUnsignedByte();
            } else if (((type == BYTE) || (type == UNDEFINED) || (type == ASCII)) && (count > 4)) {
                value_offset = getInt(endianess);

                saveLocus = raFile.getFilePointer();
                raFile.seek(value_offset);

                for (i1 = 0; ((i1 < count) && (i1 < MAX_IFD_LENGTH)); i1++) {
                    valueArray[i1] = raFile.readUnsignedByte();
                }

                raFile.seek(saveLocus);
            } else if ((type == SBYTE) && (count == 1)) {
                valueArray[0] = raFile.readByte();
                raFile.seek(raFile.getFilePointer() + 3);
                // raFile.readByte();
                // raFile.readByte();
                // raFile.readByte();
            } else if ((type == SBYTE) && (count == 2)) {
                valueArray[0] = raFile.readByte();
                valueArray[1] = raFile.readByte();
                raFile.seek(raFile.getFilePointer() + 2);
                // raFile.readByte();
                // raFile.readByte();
            } else if ((type == SBYTE) && (count == 3)) {
                valueArray[0] = raFile.readByte();
                valueArray[1] = raFile.readByte();
                valueArray[2] = raFile.readByte();
                raFile.seek(raFile.getFilePointer() + 1);
                // raFile.readByte();
            } else if ((type == SBYTE) && (count == 4)) {
                valueArray[0] = raFile.readByte();
                valueArray[1] = raFile.readByte();
                valueArray[2] = raFile.readByte();
                valueArray[3] = raFile.readByte();
            } else if ((type == SBYTE) && (count > 4)) {
                value_offset = getInt(endianess);

                saveLocus = raFile.getFilePointer();
                raFile.seek(value_offset);

                for (i1 = 0; ((i1 < count) && (i1 < MAX_IFD_LENGTH)); i1++) {
                    valueArray[i1] = raFile.readByte();
                }

                raFile.seek(saveLocus);
            } else if ((type == SSHORT) && (count == 1)) {
                valueArray[0] = getSignedShort(endianess);
                getSignedShort(endianess);
            } else if ((type == SSHORT) && (count == 2)) {
                valueArray[0] = getSignedShort(endianess);
                valueArray[1] = getSignedShort(endianess);
            } else if ((type == SSHORT) && (count >= 3)) {
                value_offset = getInt(endianess);

                saveLocus = raFile.getFilePointer();
                raFile.seek(value_offset);

                for (i1 = 0; ((i1 < count) && (i1 < MAX_IFD_LENGTH)); i1++) {
                    valueArray[i1] = getSignedShort(endianess);
                }

                raFile.seek(saveLocus);
            } else if ((type == FLOAT) && (count == 1)) {
                valueFloat = getFloat(endianess);
            } else if ((type == FLOAT) && (count > 1)) {

                // Ignore these fields for now
                value_offset = getInt(endianess);
            } else {

                if (debuggingFileIO) {
                    Preferences.debug("\nOpenIFD: Unknown field type = " + type + " Tag = " + tag + " count = " +
                                      count + "\n", Preferences.DEBUG_FILEIO);
                }

                throw new IOException("FileTiff.openIFD: Unknown field type = " + type + " Tag = " + tag + " count = " + count);
            }
            
            if (debuggingFileIO) {
                Preferences.debug("\nFileTiff.openIFD: Tag = " + tag + "\n", Preferences.DEBUG_FILEIO);

                switch (type) {

                    case BYTE:
                        Preferences.debug("FileTiff.openIFD: Type = BYTE  Count = " + count + "\n",
                                          Preferences.DEBUG_FILEIO);
                        break;

                    case ASCII:
                        Preferences.debug("FileTiff.openIFD: Type = ASCII  Count = " + count + "\n",
                                          Preferences.DEBUG_FILEIO);
                        break;

                    case SHORT:
                        Preferences.debug("FileTiff.openIFD: Type = SHORT  Count = " + count + "\n",
                                          Preferences.DEBUG_FILEIO);
                        break;

                    case LONG:
                        Preferences.debug("FileTiff.openIFD: Type = LONG  Count = " + count + "\n",
                                          Preferences.DEBUG_FILEIO);
                        break;

                    case RATIONAL:
                        Preferences.debug("FileTiff.openIFD: Type = RATIONAL  Count = " + count + "\n",
                                          Preferences.DEBUG_FILEIO);
                        break;

                    case SBYTE:
                        Preferences.debug("FileTiff.openIFD: Type = SBYTE  Count = " + count + "\n",
                                          Preferences.DEBUG_FILEIO);
                        break;

                    case UNDEFINED:
                        Preferences.debug("FileTiff.openIFD: Type = UNDEFINED  Count = " + count + "\n",
                                          Preferences.DEBUG_FILEIO);
                        break;

                    case SSHORT:
                        Preferences.debug("FileTiff.openIFD: Type = SSHORT  Count = " + count + "\n",
                                          Preferences.DEBUG_FILEIO);
                        break;

                    case SLONG:
                        Preferences.debug("FileTiff.openIFD: Type = SLONG  Count = " + count + "\n",
                                          Preferences.DEBUG_FILEIO);
                        break;

                    case SRATIONAL:
                        Preferences.debug("FileTiff.openIFD: Type = SRATIONAL  Count = " + count + "\n",
                                          Preferences.DEBUG_FILEIO);
                        break;

                    case FLOAT:
                        Preferences.debug("FileTiff.openIFD: Type = FLOAT  Count = " + count + "\n",
                                          Preferences.DEBUG_FILEIO);
                        break;

                    case DOUBLE:
                        Preferences.debug("FileTiff.openIFD: Type = DOUBLE  Count = " + count + "\n",
                                          Preferences.DEBUG_FILEIO);
                        break;
                        
                    case IFD:
                        Preferences.debug("FileTiff.openIFD: Type = IFD  Count = " + count + "\n",
                                Preferences.DEBUG_FILEIO);
                        break;
                        
                }
            }

            
            if ((type == RATIONAL) || (type == SRATIONAL)) {
                ecount = 2 * count;
            } else {
                ecount = count;
            }

            if ((type != DOUBLE) && (type != FLOAT) && debuggingFileIO) {

                for (i1 = 0; ((i1 < ecount) && (i1 < MAX_IFD_LENGTH)); i1++) {
                    Preferences.debug("FileTiff.openIFD: value[" + (i1 + 1) + "] = " + valueArray[i1] + "\n",
                                      Preferences.DEBUG_FILEIO);
                }
            } else if ((type == DOUBLE) && (count == 1) && debuggingFileIO) {
                Preferences.debug("FileTiff.openIFD: value = " + valueDouble[0] + "\n", Preferences.DEBUG_FILEIO);
            } else if ((type == FLOAT) && (count == 1) && debuggingFileIO) {
                Preferences.debug("FileTiff.openIFD: value = " + valueFloat + "\n", Preferences.DEBUG_FILEIO);
            }

            switch (tag) {

                case NEW_SUBFILE_TYPE:
                    if (type != LONG) {
                        throw new IOException("NEW_SUBFILE_TYPE has illegal TYPE = " + type + "\n");
                    }

                    if (count != 1) {
                        throw new IOException("NEW_SUBFILE_TYPE has illegal count = " + count + "\n");
                    }

                    if (debuggingFileIO) {
                        Preferences.debug("FileTiff.openIFD: tag = NEW_SUBFILE_TYPE\n", Preferences.DEBUG_FILEIO);

                        if ((valueArray[0] & 0x01) == 0x01) {
                            Preferences.debug("Image is a reduced resolution version of another " +
                                              "image in this TIFF file\n", Preferences.DEBUG_FILEIO);
                        } else {
                            Preferences.debug("Image is not a reduced resolution version of another " +
                                              "image in this TIFF file\n", Preferences.DEBUG_FILEIO);
                        }

                        if ((valueArray[0] & 0x02) == 0x02) {
                            Preferences.debug("Image is a single page of a multi-page image\n", Preferences.DEBUG_FILEIO);
                        } else {
                            Preferences.debug("Image is not a single page of a multi-page image\n",
                                              Preferences.DEBUG_FILEIO);
                        }

                        if ((valueArray[0] & 0x04) == 0x04) {
                            Preferences.debug("Images defines a transparency mask for another image " +
                                              "in this TIFF file\n", Preferences.DEBUG_FILEIO);
                        } else {
                            Preferences.debug("Images does not define a transparency mask for another image " +
                                              "in this TIFF file\n", Preferences.DEBUG_FILEIO);
                        }
                    }

                    break;

                case IMAGE_WIDTH:
                    if ((type != SHORT) && (type != LONG)) {
                        throw new IOException("IMAGE_WIDTH has illegal TYPE = " + type + "\n");
                    }

                    if (count != 1) {
                        throw new IOException("IMAGE_WIDTH has illegal count = " + count + "\n");
                    }

                    xDim = (int) valueArray[0];
                    if (debuggingFileIO) {
                        Preferences.debug("FileTiff.openIFD: Image_Width = " + xDim + "\n", Preferences.DEBUG_FILEIO);
                    }

                    break;

                case IMAGE_LENGTH:
                    if ((type != SHORT) && (type != LONG)) {
                        throw new IOException("IMAGE_LENGTH has illegal TYPE = " + type + "\n");
                    }

                    if (count != 1) {
                        throw new IOException("IMAGE_LENGTH has illegal COUNT = " + count + "\n");
                    }

                    yDim = (int) valueArray[0];
                    if (debuggingFileIO) {
                        Preferences.debug("FileTiff.openIFD: Image_Length = " + yDim + "\n", Preferences.DEBUG_FILEIO);
                    }

                    break;

                case BITS_PER_SAMPLE:
                    if (type != SHORT) {
                        throw new IOException("BITS_PER_SAMPLE has illegal type = " + type + "\n");
                    }

                    bitsPerSample = new int[count];
                    for (i1 = 0; i1 < count; i1++) {
                        bitsPerSample[i1] = (int) valueArray[i1];
                    }

                    if ((count == 1) && debuggingFileIO) {
                        Preferences.debug("FileTiff.openIFD: BitsPerSample = " + valueArray[0] + "\n",
                                          Preferences.DEBUG_FILEIO);
                    } else if (count > 1) {

                        if (debuggingFileIO) {
                            Preferences.debug("FileTiff.openIFD: BitsPerSample are above\n", Preferences.DEBUG_FILEIO);
                        }

                        for (i1 = 1; i1 < count; i1++) {

                            if (valueArray[i1] != valueArray[0]) {

                                if (debuggingFileIO) {
                                    Preferences.debug("MIPAV cannot handle mixed data types", Preferences.DEBUG_FILEIO);
                                }

                                throw new IOException("MIPAV cannot handle mixed data types");
                            }
                        }
                    } // else if (count > 1)

                    break;

                case ROWS_PER_STRIP:
                    if ((type != SHORT) && (type != LONG)) {
                        throw new IOException("ROWS_PER-STRIP has illegal type = " + type + "\n");
                    }

                    if (count != 1) {
                        throw new IOException("ROWS_PER_STRIP has illegal count = " + count + "\n");
                    }

                    // Note that 2**32 -1 meaning to put all the rows in 1 strip shows up as -1
                    //rowsPerStrip = (int) valueArray[0];
                    //haveRowsPerStrip = true;
                    if (debuggingFileIO) {
                        Preferences.debug("ROWS_PER_STRIP = " + valueArray[0] + "\n", Preferences.DEBUG_FILEIO);
                    }

                    break;

                case STRIP_OFFSETS:
                    if ((type != SHORT) && (type != LONG)) {
                        throw new IOException("STRIP_OFFSETS has illegal type = " + type + "\n");
                    }

                    dataOffsets[imageSlice] = new Vector<Index>();
                    if (count == 1) {

                        if (debuggingFileIO) {
                            Preferences.debug("FileTiff.openIFD: Strip_offset = " + valueArray[0] + "\n",
                                              Preferences.DEBUG_FILEIO);
                        }

                        dataOffsets[imageSlice].addElement(new Index((int) valueArray[0]));
                    } else if (count > 1) {

                        if (debuggingFileIO) {
                            Preferences.debug("FileTiff.openIFD: Strip_offsets are above\n");
                        }

                        for (i1 = 0; i1 < count; i1++) {

                            dataOffsets[imageSlice].addElement(new Index((int) valueArray[i1]));
                        }
                    }

                    break;

                case STRIP_BYTE_COUNTS:
                    if ((type != SHORT) && (type != LONG)) {
                        throw new IOException("STRIP_BYTE_COUNTS has illegal type = " + type + "\n");
                    }

                    if (count == 1) {

                        if (debuggingFileIO) {
                            Preferences.debug("FileTiff.openIFD: Strip byte counts = " + valueArray[0] + "\n",
                                              Preferences.DEBUG_FILEIO);
                        }

                        ((Index) (dataOffsets[imageSlice].elementAt(0))).byteCount = (int) valueArray[0];
                    } else if (count > 1) {

                        if (debuggingFileIO) {
                            Preferences.debug("FileTiff.openIFD. Strip byte counts are above\n",
                                              Preferences.DEBUG_FILEIO);
                        }

                        for (i1 = 0; i1 < count; i1++) {

                            ((Index) (dataOffsets[imageSlice].elementAt(i1))).byteCount = (int) valueArray[i1];
                        }
                    }

                    break;

                case PHOTO_INTERP:
                    if (type != SHORT) {
                        throw new IOException("PHOTO_INTERP has illegal type = " + type + "\n");
                    }

                    if (count != 1) {
                        throw new IOException("PHOTO_INTERP has illegal count = " + count + "\n");
                    }

                    if (debuggingFileIO) {
                        Preferences.debug("FileTiff.openIFD: PhotoInterp= " + valueArray[0] + "\n",
                                          Preferences.DEBUG_FILEIO);
                    }

                    if (valueArray[0] == 1) { // Black is zero

                        if (debuggingFileIO) {
                            Preferences.debug("FileTiff.openIFD: PhotoInterp = Black is zero\n" +
                                              "For bilevel and grayscale images\n", Preferences.DEBUG_FILEIO);
                        }
                    } else if (valueArray[0] == 0) { // white is zero

                        if (debuggingFileIO) {
                            Preferences.debug("FileTiff.openIFD: PhotoInterp = White is zero\n" +
                                              "For bilevel and grayscale images\n", Preferences.DEBUG_FILEIO);
                        }
                    } else if (valueArray[0] == 2) { // Color RGB

                        if (debuggingFileIO) {
                            Preferences.debug("FileTiff.openIFD: PhotoInterp = RGB\n", Preferences.DEBUG_FILEIO);
                        }
                    } else if (valueArray[0] == 3) { // Color Indexed

                        if (debuggingFileIO) {
                            Preferences.debug("FileTiff.openIFD: PhotoInterp = Palette color\n", Preferences.DEBUG_FILEIO);
                        }
                    } else if (valueArray[0] == 4) { // Transparency Mask

                        if (debuggingFileIO) {
                            Preferences.debug("FileTiff.openIFD: PhotoInterp = Transparency Mask\n",
                                              Preferences.DEBUG_FILEIO);
                        }
                    } else if (valueArray[0] == 5) { // Separated - usually CMYK
                        // bits per pixel = 8,8,8,8 for CMYK
                         
                        if (debuggingFileIO) {
                            Preferences.debug("FileTiff.openIFD: PhotoInterp = Separated\n",
                                              Preferences.DEBUG_FILEIO);
                        }
                    } else {
                        throw new IOException("PHOTOINTERP has illegal value = " + valueArray[0]);
                    }

                    break;

                case SAMPLES_PER_PIXEL:
                    if (type != SHORT) {
                        throw new IOException("SAMPLES_PER_PIXEL has illegal type = " + type + "\n");
                    }

                    if (count != 1) {
                        throw new IOException("SAMPLES_PER_PIXEL has illegal count = " + count + "\n");
                    }

                    if (debuggingFileIO) {
                        Preferences.debug("FileTiff.openIFD: samplesPerPixel = " + valueArray[0] + "\n",
                                          Preferences.DEBUG_FILEIO);
                    }

                    //samplesPerPixel = (int) valueArray[0];
                    break;
                    
                
                case IMAGE_DESCRIPTION:
                    if (type != ASCII) {
                        throw new IOException("IMAGE_DESCRIPTION has illegal type = " + type + "\n");
                    }

                    imageDescription = new byte[count];
                    for (i1 = 0; i1 < count; i1++) {
                        imageDescription[i1] = (byte) valueArray[i1];
                    }

                    
                    if (debuggingFileIO) {
                        String str = new String(imageDescription);
                        Preferences.debug("FileTiff.openIFD: imageDescription = " + str.trim() + "\n",
                                          Preferences.DEBUG_FILEIO);
                    }

                    break;
                    
                
                
                default:
                    
                    break;
            }
            if ((i != 0) && (i == iExifStart + exifDirEntries)) {
                raFile.seek(preExifLocus);
            }
        }

        
        
        

        imageSlice++;
        IFDoffsets[imageSlice] = getInt(endianess);

        if (debuggingFileIO) {
            Preferences.debug("\nFileTiff.openIFD: Ref. to next imageSlice = " + IFDoffsets[imageSlice] + "\n",
                              Preferences.DEBUG_FILEIO);
        }

        if ((IFDoffsets[imageSlice] <= 0) || (IFDoffsets[imageSlice] >= fileLength)) {
            return false; // Done reading images
        }
        
        // Make sure the next IFD entry starts with a valid number of directory entries 
        // before considering it as valid.
        saveLocus = raFile.getFilePointer();
        raFile.seek(IFDoffsets[imageSlice]);
        
        nDirEntries = getUnsignedShort(endianess);

        if ((nDirEntries <= 0) || (nDirEntries >= 100)) {
            raFile.seek(saveLocus);
            return false;
        }

        raFile.seek(saveLocus);
        return true; // Read more IFDs (ie. images)
    }
    
    /**
     * Simple class to store image offsets and bytes located at the offset.
     */
    private class Index {

        /** DOCUMENT ME! */
        public int byteCount = 0;

        /** DOCUMENT ME! */
        public int index = 0;

        /**
         * Creates a new Index object.
         *
         * @param  _index  DOCUMENT ME!
         */
        public Index(int _index) {
            index = _index;
        }
    }
}
