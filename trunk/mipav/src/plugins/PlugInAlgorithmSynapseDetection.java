import gov.nih.mipav.model.algorithms.*;
import gov.nih.mipav.model.structures.*;

import gov.nih.mipav.view.*;

import java.awt.*;

import java.io.*;

import java.util.*;


/**
 *
 * @version  February 25, 2009
 * @author   William Gandler
 * @see      AlgorithmBase
 *
 *           PlugInAlgorithmSynapseDetection is used to place a point marker over the blue center of the synapse.
 *           Because these files are very large, buffers must be as few and small as possible.  Maintaining separate
 *           red, green, and blue buffers for the entire program is not feasible because of the large file sizes.
 *           Therefore, all pixels are classified as either BRIGHT_RED, RED, BRIGHT_GREEN, GREEN, BRIGHT_BLUE, BLUE,
 *           or NONE within a single byte buffer.  User specified redIntensity, greenIntensity, and blueIntensity 
 *           are used to separate the BRIGHT_COLOR value from the COLOR value.
 *           
 *           Note that when line profiles are taken thru a synapse the red and green have all sorts of
             different shapes but the blue is almost always a distinct 4-24 wide almost rectangular pulse, so the 
             presence of blue >= blueIntensity is the most significant factor.  A pixel is 
             counted as BRIGHT_BLUE as long as its blue value is >= blueIntensity even if the red and/or
             green values at that pixel location are greater than or equal to the blue value.  This lets the well
             defined width of the blue band be determined.  If the blue value is < blueIntensity, but the blue value
             is greater than the red and green values, the pixel is classified as BLUE.  If blue value is less than
             blueIntensity and the red value is greater than the green and blue values, then the pixel is classified
             as BRIGHT_RED if red >= redIntensity and as RED if red < redIntensity.  If the blue value is less than
             blueIntensity and the green value is greater than the red and blue values, then the pixel is classified
             as BRIGHT_GREEN if green >= greenIntensity and as GREEN if green < greenIntensity.  If none of the above
             conditions is met, then the buffer value is set to NONE.  After a BRIGHT_BLUE or BLUE value has been 
             processed, the buffer value is set to NONE, so that the same BRIGHT_BLUE or BLUE pixel will not be found
             multiple times on line searches in different directions.
             
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
             cross planes must fall within blueMinZ and blueMaxZ limits.
             
             If a red, blue, green or green, blue, red sequence meets the intensity and width requirements, then a find is
             initially declared at the center of the blue band.  However, this initial find could be at the edge rather than
             the center of the blue portion of the synapse.  So a search of 26 connected BRIGHT_BLUE and BLUE neighbors is
             conducted around the initial blue find within a block of width 2 * blueMaxXY - 1, length 2 * blueMaxXY - 1, and
             height 2 * blueMaxZ - 1.  The center of the 26 connected BRIGHT_BLUE and BLUE neighbors within this block is 
             calculated and the center is used as the location of the point VOI for the synapse.  The values of the initial
             BRIGHT_BLUE or BLUE pixel and of all 26 connected BRIGHT_BLUE and BLUE pixels is set to NONE so that the same
             synapse will not be found again on another line search thru the same pixel in a different direction.  
             
             For the redIntensity, greenIntensity, and blueIntensity I opened your file of contours around synapses Synapse100.xml
             on the image SynapseSpotted_d_RedGreenL4_L23_090105_1_to_59Crop3_enhanced.tif.  I drew one line segment thru each
             synapse and noted the maximum red, green, and blue pixel intensities and the blue width.  This was not super scientific as
             there are different ways the line segment could be drawn.
             Slice 0 red 98, green 130, blue 150, blue width 9
             Slice 1 left red 131, green 139, blue 175, blue width 10
             Slice 1 right red 255, green 162, blue 82, blue width 7
             Slice 2 red 214, green 101, blue 156, blue width 4
             Slice 4 red 108, green 87, blue 134, blue width 11
             Slice 7 left Not typical don’t record
             Slice 7 right red 69, green 123, blue 146, blue width 18
             Slice 8 red 82, green 129, blue 163, blue width 24
             Slice 10 top red 56, green 110, blue 90, blue width 6
             Slice 11 bottom red 111, green 98, blue 107, blue width 10
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
 */
public class PlugInAlgorithmSynapseDetection extends AlgorithmBase {

    //~ Static fields/initializers -------------------------------------------------------------------------------------

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** Minimum number of pixels along line */
    private int redMin = 1;
    
    private int redMax = 20;
    
    private int greenMin = 1;
    
    private int greenMax = 200;
    
    private int blueMinXY = 1;
    
    private int blueMaxXY = 20;
    
    private int blueMinZ = 1;
    
    private int blueMaxZ = 20;
    
    // Maximum of blueMaxXY and blueMaxZ;
    private int blueMax;
    
    /* Minimum intensity values */
    private int redIntensity = 55;
    
    private int greenIntensity = 85;
    
    private int blueIntensity = 80;
    
    /** If true, provide histograms of red, green, and blue values along 
     *  detected line segments.
     */
    private boolean histoInfo = false;
    
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
    private int centerBlueX;
    private int centerBlueY;
    private int centerBlueZ;
    private int numSynapses = 0;
    private int previousNumSynapses = 0;
    private int xArr[] = new int[1];
    private int yArr[] = new int[1];
    private int zArr[] = new int[1];
    private int xDim = srcImage.getExtents()[0];
    private int yDim = srcImage.getExtents()[1];
    private int zDim = srcImage.getExtents()[2];
    private int length = xDim * yDim * zDim;
    private int xySlice = xDim * yDim;
    private byte buffer[] = new byte[length];
    private byte greenBuffer[] = new byte[length];
    private byte blueBuffer[] = new byte[length];
    private BitSet blueMask = null;
    private BitSet blueMask2 = null;
    private int threeBandMinXY;
    private int threeBandMinZ;
    private ModelImage maskImage = null;
    // Number of pixels in a detected and grown blue region
    private int blueCount;
    private String fileName;
    private String fileDirectory;
    private File file;
    private RandomAccessFile raFile;
    private String dataString = null;
    private int minimumBlueCount = Integer.MAX_VALUE;
    private int maximumBlueCount = Integer.MIN_VALUE;
    private long totalBlueCount = 0;
    private long totalBlueCountSquared = 0;
    private double averageBlueCount;
    private double blueStandardDeviation;
    private BitSet threePreviousMask = null;
    private BitSet twoPreviousMask = null;
    private BitSet onePreviousMask = null;
    private BitSet presentMask = null;
    private BitSet colorMask = null;
    private float xInit[] = null;
    private float redExtents[] = null;
    private float greenExtents[] = null;
    private float blueExtents[] = null;
    private ViewJFrameGraph redGraph = null;
    private ViewJFrameGraph greenGraph = null;
    private ViewJFrameGraph blueGraph = null;
    private int threePreviousPos[] = null;
    private int twoPreviousPos[] = null;
    private int onePreviousPos[] = null;
    private int presentPos[] = null;
    private int presentPosIndex = 0;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Constructor.
     *
     * @param  srcImg         Source image model.
     * @param  redMin         Minimum number of red pixels along line
     * @param  redMax         Maximum number of red pixels along line
     * @param  greenMin       Minimum number of green pixels along line
     * @param  greenMax       Maximum number of green pixels along line
     * @param  blueMinXY      Minimum number of blue pixels along line within a slice
     * @param  blueMaxXY      Maximum number of blue pixels along line within a slice
     * @param  blueMinZ       Minimum number of blue pixels along line between slices
     * @param  blueMaxZ       Maximum number of blue pixels along line between slices
     * @param  redIntensity      Minimum red intensity
     * @param  greenIntensity    Minimum green intensity
     * @param  blueIntensity     Minimum blue intensity
     * @param  histoInfo         If true, provide histogram information of red, green, and
     *                           blue values along detected lines
     */
    public PlugInAlgorithmSynapseDetection(ModelImage srcImg, int redMin, int redMax, int greenMin,
                                         int greenMax, int blueMinXY, int blueMaxXY, 
                                         int blueMinZ, int blueMaxZ, int redIntensity,
                                         int greenIntensity, int blueIntensity,
                                         boolean histoInfo) {
        super(null, srcImg);
        this.redMin = redMin;
        this.redMax = redMax;
        this.greenMin = greenMin;
        this.greenMax = greenMax;
        this.blueMinXY = blueMinXY;
        this.blueMaxXY = blueMaxXY;
        this.blueMinZ = blueMinZ;
        this.blueMaxZ = blueMaxZ;
        this.redIntensity = redIntensity;
        this.greenIntensity = greenIntensity;
        this.blueIntensity = blueIntensity;
        this.histoInfo = histoInfo;
    }

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * Prepares this class for destruction.
     */
    public void finalize() {
        srcImage = null;
        super.finalize();
    }

    /**
     * Starts the algorithm.
     */
    public void runAlgorithm() {

        if (srcImage == null) {
            displayError("Source Image is null");

            return;
        }

        

       calc3D();
    } // end runAlgorithm()

    /**
     * DOCUMENT ME!
     */
    private void calc2D() {
        long time;

        time = System.currentTimeMillis();

        if (threadStopped) {
            finalize();

            return;
        }

        fireProgressStateChanged(100);
        time = System.currentTimeMillis() - time;
        Preferences.debug("PlugInAlgorithmSynapseDetection elapsed time in seconds = " + (time/1000.0));
        setCompleted(true);
    }
    
    /**
     * DOCUMENT ME!
     */
    private void calc3D() {
        long time;
        int zPos;
        int yPos;
        int xPos;
        int x;
        int y;
        int z;
        int pos;
        int red;
        int green;
        int blue;
        int blueStartX = 0;
        int blueStartY = 0;
        int blueStartZ = 0;
        int xStart;
        int yStart;
        int zStart;
        int i;
        
        fireProgressStateChanged("Synapse detection on image");

        time = System.currentTimeMillis();
        
        blueMax = Math.max(blueMaxXY, blueMaxZ);
        
        try {
            srcImage.exportRGBData(1, 0, length, buffer); // export red data
        } catch (IOException error) {
            buffer = null;
            greenBuffer = null;
            blueBuffer = null;
            errorCleanUp("Algorithm SynapseDetection reports: source image locked", true);

            return;
        }
        
        try {
            srcImage.exportRGBData(2, 0, length, greenBuffer); // export green data
        } catch (IOException error) {
            buffer = null;
            greenBuffer = null;
            blueBuffer = null;
            errorCleanUp("Algorithm SynapseDetection reports: source image locked", true);

            return;
        }
        
        try {
            srcImage.exportRGBData(3, 0, length, blueBuffer); // export blue data
        } catch (IOException error) {
            buffer = null;
            greenBuffer = null;
            blueBuffer = null;
            errorCleanUp("Algorithm SynapseDetection reports: source image locked", true);

            return;
        }
        
        // Classify all pixels as either BRIGHT_RED, RED, BRIGHT_GREEN, GREEN, BRIGHT_BLUE, BLUE, or NONE
        // Note that when line profiles are taken thru a synapse the red and green have all sorts of
        // different shapes but the blue is almost always a distinct short rectangular pulse, so the 
        // presence of blue above a threshold intensity is the most significant factor.  A pixel is 
        // counted as BRIGHT_BLUE as long as its blue value is >= blueIntensity even if the red and/or
        // green values at that pixel location are greater or equal than the blue value.
        for (pos = 0; pos < length; pos++) {
            red = buffer[pos] & 0xff;
            green = greenBuffer[pos] & 0xff;
            blue = blueBuffer[pos] & 0xff;
            if (blue >= blueIntensity) {
                buffer[pos] = BRIGHT_BLUE;
            }
            else if ((blue > red) && (blue > green)) {
                buffer[pos] = BLUE;
            }
            else if ((red > green) && (red > blue)) {
                if (red >= redIntensity) {
                    buffer[pos] = BRIGHT_RED;
                }
                else {
                    buffer[pos] = RED;
                }
            }
            else if ((green > red) && (green > blue)) {
                if (green >= greenIntensity) {
                    buffer[pos] = BRIGHT_GREEN;
                }
                else {
                    buffer[pos] = GREEN;
                }
            }
            else {
                buffer[pos] = NONE;
            }
        } // for (pos = 0; pos < length; pos++)
           
        greenBuffer = null;
        blueBuffer = null;
        System.gc();
        blueMask = new BitSet(length);
        blueMask2 = new BitSet(length);
        if (histoInfo) {
            threePreviousPos = new int[500];
            twoPreviousPos = new int[500];
            onePreviousPos = new int[500];
            presentPos = new int[500];
            colorMask = new BitSet(length);
        }
        threeBandMinXY = redMin + greenMin + blueMinXY;
        threeBandMinZ = redMin + greenMin + blueMinZ;
        dataString = "Index     x         y         z         count\n\n";
        
        // Search along all lines parallel to x axis
        dataString += ("Searching parallel to x axis\n");
        for (z = 0; z < zDim; z++) {
            zPos = z * xySlice;
            for (y = 0; y < yDim; y++) {
                yPos = zPos + y * xDim;
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
                   if ((buffer[pos] == presentColor) || (buffer[pos] == presentBrightColor)) {
                       presentWidth++;
                       if ((presentColor == BRIGHT_BLUE) || (presentColor == BLUE)) {
                           blueX = (blueStartX + x) >> 1;
                           blueY = (blueStartY + y) >> 1;
                           blueZ = (blueStartZ + z) >> 1; 
                       }
                       if ((!presentBrightness) && (buffer[pos] >= BRIGHT_RED)) {
                           presentBrightness = true;
                       }
                       if (histoInfo) {
                           presentPos[presentPosIndex++] = pos;
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
                       if (histoInfo) {
                           for (i = 0; i < threePreviousWidth; i++) {
                               threePreviousPos[i] = twoPreviousPos[i];
                           }
                           for (i = 0; i < twoPreviousWidth; i++) {
                               twoPreviousPos[i] = onePreviousPos[i];
                           }
                           for (i = 0; i < onePreviousWidth; i++) {
                               onePreviousPos[i] = presentPos[i];
                           }
                           presentPosIndex = 0;
                           presentPos[presentPosIndex++] = pos;
                       } // if (histoInfo)
                       if (buffer[pos] < BRIGHT_RED) {
                           presentColor = buffer[pos];
                           if (buffer[pos] == NONE) {
                               presentBrightColor = NONE;
                           }
                           else {
                               presentBrightColor = presentColor + 3;
                           }
                           presentBrightness = false;
                       }
                       else {
                           presentBrightColor = buffer[pos];
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
                if (histoInfo) {
                    for (i = 0; i < threePreviousWidth; i++) {
                        threePreviousPos[i] = twoPreviousPos[i];
                    }
                    for (i = 0; i < twoPreviousWidth; i++) {
                        twoPreviousPos[i] = onePreviousPos[i];
                    }
                    for (i = 0; i < onePreviousWidth; i++) {
                        onePreviousPos[i] = presentPos[i];
                    }
                } // if (histoInfo)
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
            zPos = z * xySlice;
            for (x = 0; x < xDim; x++) {
                xPos = zPos + x;
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
                    pos = xPos + y * xDim;
                    if ((buffer[pos] == presentColor) || (buffer[pos] == presentBrightColor)) {
                        presentWidth++;
                        if ((presentColor == BRIGHT_BLUE) || (presentColor == BLUE)) {
                            blueX = (blueStartX + x) >> 1;
                            blueY = (blueStartY + y) >> 1;
                            blueZ = (blueStartZ + z) >> 1;    
                        }
                        if ((!presentBrightness) && (buffer[pos] >= BRIGHT_RED)) {
                            presentBrightness = true;
                        }
                        if (histoInfo) {
                            presentPos[presentPosIndex++] = pos;
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
                        if (histoInfo) {
                            for (i = 0; i < threePreviousWidth; i++) {
                                threePreviousPos[i] = twoPreviousPos[i];
                            }
                            for (i = 0; i < twoPreviousWidth; i++) {
                                twoPreviousPos[i] = onePreviousPos[i];
                            }
                            for (i = 0; i < onePreviousWidth; i++) {
                                onePreviousPos[i] = presentPos[i];
                            }
                            presentPosIndex = 0;
                            presentPos[presentPosIndex++] = pos;
                        } // if (histoInfo)
                        if (buffer[pos] < BRIGHT_RED) {
                            presentColor = buffer[pos];
                            if (buffer[pos] == NONE) {
                                presentBrightColor = NONE;
                            }
                            else {
                                presentBrightColor = presentColor + 3;
                            }
                            presentBrightness = false;
                        }
                        else {
                            presentBrightColor = buffer[pos];
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
                if (histoInfo) {
                    for (i = 0; i < threePreviousWidth; i++) {
                        threePreviousPos[i] = twoPreviousPos[i];
                    }
                    for (i = 0; i < twoPreviousWidth; i++) {
                        twoPreviousPos[i] = onePreviousPos[i];
                    }
                    for (i = 0; i < onePreviousWidth; i++) {
                        onePreviousPos[i] = presentPos[i];
                    }
                } // if (histoInfo)
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
                xPos = yPos + x;
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
                    pos = xPos + z * xySlice;
                    if ((buffer[pos] == presentColor) || (buffer[pos] == presentBrightColor)) {
                        presentWidth++;
                        if ((presentColor == BRIGHT_BLUE) || (presentColor == BLUE)) {
                            blueX = (blueStartX + x) >> 1;
                            blueY = (blueStartY + y) >> 1;
                            blueZ = (blueStartZ + z) >> 1;    
                        }
                        if ((!presentBrightness) && (buffer[pos] >= BRIGHT_RED)) {
                            presentBrightness = true;
                        }
                        if (histoInfo) {
                            presentPos[presentPosIndex++] = pos;
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
                        if (histoInfo) {
                            for (i = 0; i < threePreviousWidth; i++) {
                                threePreviousPos[i] = twoPreviousPos[i];
                            }
                            for (i = 0; i < twoPreviousWidth; i++) {
                                twoPreviousPos[i] = onePreviousPos[i];
                            }
                            for (i = 0; i < onePreviousWidth; i++) {
                                onePreviousPos[i] = presentPos[i];
                            }
                            presentPosIndex = 0;
                            presentPos[presentPosIndex++] = pos;
                        } // if (histoInfo)
                        if (buffer[pos] < BRIGHT_RED) {
                            presentColor = buffer[pos];
                            if (buffer[pos] == NONE) {
                                presentBrightColor = NONE;
                            }
                            else {
                                presentBrightColor = presentColor + 3;
                            }
                            presentBrightness = false;
                        }
                        else {
                            presentBrightColor = buffer[pos];
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
                if (histoInfo) {
                    for (i = 0; i < threePreviousWidth; i++) {
                        threePreviousPos[i] = twoPreviousPos[i];
                    }
                    for (i = 0; i < twoPreviousWidth; i++) {
                        twoPreviousPos[i] = onePreviousPos[i];
                    }
                    for (i = 0; i < onePreviousWidth; i++) {
                        onePreviousPos[i] = presentPos[i];
                    }
                } // if (histoInfo)
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
            zPos = z * xySlice;
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
                    pos = zPos + y * xDim + x; 
                    if ((buffer[pos] == presentColor) || (buffer[pos] == presentBrightColor)) {
                        presentWidth++;
                        if ((presentColor == BRIGHT_BLUE) || (presentColor == BLUE)) {
                            blueX = (blueStartX + x) >> 1;
                            blueY = (blueStartY + y) >> 1;
                            blueZ = (blueStartZ + z) >> 1;    
                        }
                        if ((!presentBrightness) && (buffer[pos] >= BRIGHT_RED)) {
                            presentBrightness = true;
                        }
                        if (histoInfo) {
                            presentPos[presentPosIndex++] = pos;
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
                        if (histoInfo) {
                            for (i = 0; i < threePreviousWidth; i++) {
                                threePreviousPos[i] = twoPreviousPos[i];
                            }
                            for (i = 0; i < twoPreviousWidth; i++) {
                                twoPreviousPos[i] = onePreviousPos[i];
                            }
                            for (i = 0; i < onePreviousWidth; i++) {
                                onePreviousPos[i] = presentPos[i];
                            }
                            presentPosIndex = 0;
                            presentPos[presentPosIndex++] = pos;
                        } // if (histoInfo)
                        if (buffer[pos] < BRIGHT_RED) {
                            presentColor = buffer[pos];
                            if (buffer[pos] == NONE) {
                                presentBrightColor = NONE;
                            }
                            else {
                                presentBrightColor = presentColor + 3;
                            }
                            presentBrightness = false;
                        }
                        else {
                            presentBrightColor = buffer[pos];
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
                if (histoInfo) {
                    for (i = 0; i < threePreviousWidth; i++) {
                        threePreviousPos[i] = twoPreviousPos[i];
                    }
                    for (i = 0; i < twoPreviousWidth; i++) {
                        twoPreviousPos[i] = onePreviousPos[i];
                    }
                    for (i = 0; i < onePreviousWidth; i++) {
                        onePreviousPos[i] = presentPos[i];
                    }
                } // if (histoInfo)
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
                    pos = zPos + y * xDim + x;
                    if ((buffer[pos] == presentColor) || (buffer[pos] == presentBrightColor)) {
                        presentWidth++;
                        if ((presentColor == BRIGHT_BLUE) || (presentColor == BLUE)) {
                            blueX = (blueStartX + x) >> 1;
                            blueY = (blueStartY + y) >> 1;
                            blueZ = (blueStartZ + z) >> 1;    
                        }
                        if ((!presentBrightness) && (buffer[pos] >= BRIGHT_RED)) {
                            presentBrightness = true;
                        }
                        if (histoInfo) {
                            presentPos[presentPosIndex++] = pos;
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
                        if (histoInfo) {
                            for (i = 0; i < threePreviousWidth; i++) {
                                threePreviousPos[i] = twoPreviousPos[i];
                            }
                            for (i = 0; i < twoPreviousWidth; i++) {
                                twoPreviousPos[i] = onePreviousPos[i];
                            }
                            for (i = 0; i < onePreviousWidth; i++) {
                                onePreviousPos[i] = presentPos[i];
                            }
                            presentPosIndex = 0;
                            presentPos[presentPosIndex++] = pos;
                        } // if (histoInfo)
                        if (buffer[pos] < BRIGHT_RED) {
                            presentColor = buffer[pos];
                            if (buffer[pos] == NONE) {
                                presentBrightColor = NONE;
                            }
                            else {
                                presentBrightColor = presentColor + 3;
                            }
                            presentBrightness = false;
                        }
                        else {
                            presentBrightColor = buffer[pos];
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
                if (histoInfo) {
                    for (i = 0; i < threePreviousWidth; i++) {
                        threePreviousPos[i] = twoPreviousPos[i];
                    }
                    for (i = 0; i < twoPreviousWidth; i++) {
                        twoPreviousPos[i] = onePreviousPos[i];
                    }
                    for (i = 0; i < onePreviousWidth; i++) {
                        onePreviousPos[i] = presentPos[i];
                    }
                } // if (histoInfo)
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
            zPos = z * xySlice;
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
                    pos = zPos + y * xDim + x;
                    if ((buffer[pos] == presentColor) || (buffer[pos] == presentBrightColor)) {
                        presentWidth++;
                        if ((presentColor == BRIGHT_BLUE) || (presentColor == BLUE)) {
                            blueX = (blueStartX + x) >> 1;
                            blueY = (blueStartY + y) >> 1;
                            blueZ = (blueStartZ + z) >> 1;    
                        }
                        if ((!presentBrightness) && (buffer[pos] >= BRIGHT_RED)) {
                            presentBrightness = true;
                        }
                        if (histoInfo) {
                            presentPos[presentPosIndex++] = pos;
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
                        if (histoInfo) {
                            for (i = 0; i < threePreviousWidth; i++) {
                                threePreviousPos[i] = twoPreviousPos[i];
                            }
                            for (i = 0; i < twoPreviousWidth; i++) {
                                twoPreviousPos[i] = onePreviousPos[i];
                            }
                            for (i = 0; i < onePreviousWidth; i++) {
                                onePreviousPos[i] = presentPos[i];
                            }
                            presentPosIndex = 0;
                            presentPos[presentPosIndex++] = pos;
                        } // if (histoInfo)
                        if (buffer[pos] < BRIGHT_RED) {
                            presentColor = buffer[pos];
                            if (buffer[pos] == NONE) {
                                presentBrightColor = NONE;
                            }
                            else {
                                presentBrightColor = presentColor + 3;
                            }
                            presentBrightness = false;
                        }
                        else {
                            presentBrightColor = buffer[pos];
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
                if (histoInfo) {
                    for (i = 0; i < threePreviousWidth; i++) {
                        threePreviousPos[i] = twoPreviousPos[i];
                    }
                    for (i = 0; i < twoPreviousWidth; i++) {
                        twoPreviousPos[i] = onePreviousPos[i];
                    }
                    for (i = 0; i < onePreviousWidth; i++) {
                        onePreviousPos[i] = presentPos[i];
                    }
                } // if (histoInfo)
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
                    pos = zPos + y * xDim + x;
                    if ((buffer[pos] == presentColor) || (buffer[pos] == presentBrightColor)) {
                        presentWidth++;
                        if ((presentColor == BRIGHT_BLUE) || (presentColor == BLUE)) {
                            blueX = (blueStartX + x) >> 1;
                            blueY = (blueStartY + y) >> 1;
                            blueZ = (blueStartZ + z) >> 1;    
                        }
                        if ((!presentBrightness) && (buffer[pos] >= BRIGHT_RED)) {
                            presentBrightness = true;
                        }
                        if (histoInfo) {
                            presentPos[presentPosIndex++] = pos;
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
                        if (histoInfo) {
                            for (i = 0; i < threePreviousWidth; i++) {
                                threePreviousPos[i] = twoPreviousPos[i];
                            }
                            for (i = 0; i < twoPreviousWidth; i++) {
                                twoPreviousPos[i] = onePreviousPos[i];
                            }
                            for (i = 0; i < onePreviousWidth; i++) {
                                onePreviousPos[i] = presentPos[i];
                            }
                            presentPosIndex = 0;
                            presentPos[presentPosIndex++] = pos;
                        } // if (histoInfo)
                        if (buffer[pos] < BRIGHT_RED) {
                            presentColor = buffer[pos];
                            if (buffer[pos] == NONE) {
                                presentBrightColor = NONE;
                            }
                            else {
                                presentBrightColor = presentColor + 3;
                            }
                            presentBrightness = false;
                        }
                        else {
                            presentBrightColor = buffer[pos];
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
                if (histoInfo) {
                    for (i = 0; i < threePreviousWidth; i++) {
                        threePreviousPos[i] = twoPreviousPos[i];
                    }
                    for (i = 0; i < twoPreviousWidth; i++) {
                        twoPreviousPos[i] = onePreviousPos[i];
                    }
                    for (i = 0; i < onePreviousWidth; i++) {
                        onePreviousPos[i] = presentPos[i];
                    }
                } // if (histoInfo)
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
                    pos = yPos + z * xySlice + x; 
                    if ((buffer[pos] == presentColor) || (buffer[pos] == presentBrightColor)) {
                        presentWidth++;
                        if ((presentColor == BRIGHT_BLUE) || (presentColor == BLUE)) {
                            blueX = (blueStartX + x) >> 1;
                            blueY = (blueStartY + y) >> 1;
                            blueZ = (blueStartZ + z) >> 1;    
                        }
                        if ((!presentBrightness) && (buffer[pos] >= BRIGHT_RED)) {
                            presentBrightness = true;
                        }
                        if (histoInfo) {
                            presentPos[presentPosIndex++] = pos;
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
                        if (histoInfo) {
                            for (i = 0; i < threePreviousWidth; i++) {
                                threePreviousPos[i] = twoPreviousPos[i];
                            }
                            for (i = 0; i < twoPreviousWidth; i++) {
                                twoPreviousPos[i] = onePreviousPos[i];
                            }
                            for (i = 0; i < onePreviousWidth; i++) {
                                onePreviousPos[i] = presentPos[i];
                            }
                            presentPosIndex = 0;
                            presentPos[presentPosIndex++] = pos;
                        } // if (histoInfo)
                        if (buffer[pos] < BRIGHT_RED) {
                            presentColor = buffer[pos];
                            if (buffer[pos] == NONE) {
                                presentBrightColor = NONE;
                            }
                            else {
                                presentBrightColor = presentColor + 3;
                            }
                            presentBrightness = false;
                        }
                        else {
                            presentBrightColor = buffer[pos];
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
                if (histoInfo) {
                    for (i = 0; i < threePreviousWidth; i++) {
                        threePreviousPos[i] = twoPreviousPos[i];
                    }
                    for (i = 0; i < twoPreviousWidth; i++) {
                        twoPreviousPos[i] = onePreviousPos[i];
                    }
                    for (i = 0; i < onePreviousWidth; i++) {
                        onePreviousPos[i] = presentPos[i];
                    }
                } // if (histoInfo)
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
                    pos = yPos + z * xySlice + x; 
                    if ((buffer[pos] == presentColor) || (buffer[pos] == presentBrightColor)) {
                        presentWidth++;
                        if ((presentColor == BRIGHT_BLUE) || (presentColor == BLUE)) {
                            blueX = (blueStartX + x) >> 1;
                            blueY = (blueStartY + y) >> 1;
                            blueZ = (blueStartZ + z) >> 1;    
                        }
                        if ((!presentBrightness) && (buffer[pos] >= BRIGHT_RED)) {
                            presentBrightness = true;
                        }
                        if (histoInfo) {
                            presentPos[presentPosIndex++] = pos;
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
                        if (histoInfo) {
                            for (i = 0; i < threePreviousWidth; i++) {
                                threePreviousPos[i] = twoPreviousPos[i];
                            }
                            for (i = 0; i < twoPreviousWidth; i++) {
                                twoPreviousPos[i] = onePreviousPos[i];
                            }
                            for (i = 0; i < onePreviousWidth; i++) {
                                onePreviousPos[i] = presentPos[i];
                            }
                            presentPosIndex = 0;
                            presentPos[presentPosIndex++] = pos;
                        } // if (histoInfo)
                        if (buffer[pos] < BRIGHT_RED) {
                            presentColor = buffer[pos];
                            if (buffer[pos] == NONE) {
                                presentBrightColor = NONE;
                            }
                            else {
                                presentBrightColor = presentColor + 3;
                            }
                            presentBrightness = false;
                        }
                        else {
                            presentBrightColor = buffer[pos];
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
                if (histoInfo) {
                    for (i = 0; i < threePreviousWidth; i++) {
                        threePreviousPos[i] = twoPreviousPos[i];
                    }
                    for (i = 0; i < twoPreviousWidth; i++) {
                        twoPreviousPos[i] = onePreviousPos[i];
                    }
                    for (i = 0; i < onePreviousWidth; i++) {
                        onePreviousPos[i] = presentPos[i];
                    }
                } // if (histoInfo)
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
                    pos = yPos + z * xySlice + x;
                    if ((buffer[pos] == presentColor) || (buffer[pos] == presentBrightColor)) {
                        presentWidth++;
                        if ((presentColor == BRIGHT_BLUE) || (presentColor == BLUE)) {
                            blueX = (blueStartX + x) >> 1;
                            blueY = (blueStartY + y) >> 1;
                            blueZ = (blueStartZ + z) >> 1;    
                        }
                        if ((!presentBrightness) && (buffer[pos] >= BRIGHT_RED)) {
                            presentBrightness = true;
                        }
                        if (histoInfo) {
                            presentPos[presentPosIndex++] = pos;
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
                        if (histoInfo) {
                            for (i = 0; i < threePreviousWidth; i++) {
                                threePreviousPos[i] = twoPreviousPos[i];
                            }
                            for (i = 0; i < twoPreviousWidth; i++) {
                                twoPreviousPos[i] = onePreviousPos[i];
                            }
                            for (i = 0; i < onePreviousWidth; i++) {
                                onePreviousPos[i] = presentPos[i];
                            }
                            presentPosIndex = 0;
                            presentPos[presentPosIndex++] = pos;
                        } // if (histoInfo)
                        if (buffer[pos] < BRIGHT_RED) {
                            presentColor = buffer[pos];
                            if (buffer[pos] == NONE) {
                                presentBrightColor = NONE;
                            }
                            else {
                                presentBrightColor = presentColor + 3;
                            }
                            presentBrightness = false;
                        }
                        else {
                            presentBrightColor = buffer[pos];
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
                if (histoInfo) {
                    for (i = 0; i < threePreviousWidth; i++) {
                        threePreviousPos[i] = twoPreviousPos[i];
                    }
                    for (i = 0; i < twoPreviousWidth; i++) {
                        twoPreviousPos[i] = onePreviousPos[i];
                    }
                    for (i = 0; i < onePreviousWidth; i++) {
                        onePreviousPos[i] = presentPos[i];
                    }
                } // if (histoInfo)
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
                    pos = yPos + z * xySlice + x;
                    if ((buffer[pos] == presentColor) || (buffer[pos] == presentBrightColor)) {
                        presentWidth++;
                        if ((presentColor == BRIGHT_BLUE) || (presentColor == BLUE)) {
                            blueX = (blueStartX + x) >> 1;
                            blueY = (blueStartY + y) >> 1;
                            blueZ = (blueStartZ + z) >> 1;    
                        }
                        if ((!presentBrightness) && (buffer[pos] >= BRIGHT_RED)) {
                            presentBrightness = true;
                        }
                        if (histoInfo) {
                            presentPos[presentPosIndex++] = pos;
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
                        if (histoInfo) {
                            for (i = 0; i < threePreviousWidth; i++) {
                                threePreviousPos[i] = twoPreviousPos[i];
                            }
                            for (i = 0; i < twoPreviousWidth; i++) {
                                twoPreviousPos[i] = onePreviousPos[i];
                            }
                            for (i = 0; i < onePreviousWidth; i++) {
                                onePreviousPos[i] = presentPos[i];
                            }
                            presentPosIndex = 0;
                            presentPos[presentPosIndex++] = pos;
                        } // if (histoInfo)
                        if (buffer[pos] < BRIGHT_RED) {
                            presentColor = buffer[pos];
                            if (buffer[pos] == NONE) {
                                presentBrightColor = NONE;
                            }
                            else {
                                presentBrightColor = presentColor + 3;
                            }
                            presentBrightness = false;
                        }
                        else {
                            presentBrightColor = buffer[pos];
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
                if (histoInfo) {
                    for (i = 0; i < threePreviousWidth; i++) {
                        threePreviousPos[i] = twoPreviousPos[i];
                    }
                    for (i = 0; i < twoPreviousWidth; i++) {
                        twoPreviousPos[i] = onePreviousPos[i];
                    }
                    for (i = 0; i < onePreviousWidth; i++) {
                        onePreviousPos[i] = presentPos[i];
                    }
                } // if (histoInfo)
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
                    pos = x + z * xySlice + y * xDim;
                    if ((buffer[pos] == presentColor) || (buffer[pos] == presentBrightColor)) {
                        presentWidth++;
                        if ((presentColor == BRIGHT_BLUE) || (presentColor == BLUE)) {
                            blueX = (blueStartX + x) >> 1;
                            blueY = (blueStartY + y) >> 1;
                            blueZ = (blueStartZ + z) >> 1;    
                        }
                        if ((!presentBrightness) && (buffer[pos] >= BRIGHT_RED)) {
                            presentBrightness = true;
                        }
                        if (histoInfo) {
                            presentPos[presentPosIndex++] = pos;
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
                        if (histoInfo) {
                            for (i = 0; i < threePreviousWidth; i++) {
                                threePreviousPos[i] = twoPreviousPos[i];
                            }
                            for (i = 0; i < twoPreviousWidth; i++) {
                                twoPreviousPos[i] = onePreviousPos[i];
                            }
                            for (i = 0; i < onePreviousWidth; i++) {
                                onePreviousPos[i] = presentPos[i];
                            }
                            presentPosIndex = 0;
                            presentPos[presentPosIndex++] = pos;
                        } // if (histoInfo)
                        if (buffer[pos] < BRIGHT_RED) {
                            presentColor = buffer[pos];
                            if (buffer[pos] == NONE) {
                                presentBrightColor = NONE;
                            }
                            else {
                                presentBrightColor = presentColor + 3;
                            }
                            presentBrightness = false;
                        }
                        else {
                            presentBrightColor = buffer[pos];
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
                if (histoInfo) {
                    for (i = 0; i < threePreviousWidth; i++) {
                        threePreviousPos[i] = twoPreviousPos[i];
                    }
                    for (i = 0; i < twoPreviousWidth; i++) {
                        twoPreviousPos[i] = onePreviousPos[i];
                    }
                    for (i = 0; i < onePreviousWidth; i++) {
                        onePreviousPos[i] = presentPos[i];
                    }
                } // if (histoInfo)
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
                    pos = x + z * xySlice + y * xDim;
                    if ((buffer[pos] == presentColor) || (buffer[pos] == presentBrightColor)) {
                        presentWidth++;
                        if ((presentColor == BRIGHT_BLUE) || (presentColor == BLUE)) {
                            blueX = (blueStartX + x) >> 1;
                            blueY = (blueStartY + y) >> 1;
                            blueZ = (blueStartZ + z) >> 1;    
                        }
                        if ((!presentBrightness) && (buffer[pos] >= BRIGHT_RED)) {
                            presentBrightness = true;
                        }
                        if (histoInfo) {
                            presentPos[presentPosIndex++] = pos;
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
                        if (histoInfo) {
                            for (i = 0; i < threePreviousWidth; i++) {
                                threePreviousPos[i] = twoPreviousPos[i];
                            }
                            for (i = 0; i < twoPreviousWidth; i++) {
                                twoPreviousPos[i] = onePreviousPos[i];
                            }
                            for (i = 0; i < onePreviousWidth; i++) {
                                onePreviousPos[i] = presentPos[i];
                            }
                            presentPosIndex = 0;
                            presentPos[presentPosIndex++] = pos;
                        } // if (histoInfo)
                        if (buffer[pos] < BRIGHT_RED) {
                            presentColor = buffer[pos];
                            if (buffer[pos] == NONE) {
                                presentBrightColor = NONE;
                            }
                            else {
                                presentBrightColor = presentColor + 3;
                            }
                            presentBrightness = false;
                        }
                        else {
                            presentBrightColor = buffer[pos];
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
                if (histoInfo) {
                    for (i = 0; i < threePreviousWidth; i++) {
                        threePreviousPos[i] = twoPreviousPos[i];
                    }
                    for (i = 0; i < twoPreviousWidth; i++) {
                        twoPreviousPos[i] = onePreviousPos[i];
                    }
                    for (i = 0; i < onePreviousWidth; i++) {
                        onePreviousPos[i] = presentPos[i];
                    }
                } // if (histoInfo)
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
                    pos = x + z * xySlice + y * xDim;
                    if ((buffer[pos] == presentColor) || (buffer[pos] == presentBrightColor)) {
                        presentWidth++;
                        if ((presentColor == BRIGHT_BLUE) || (presentColor == BLUE)) {
                            blueX = (blueStartX + x) >> 1;
                            blueY = (blueStartY + y) >> 1;
                            blueZ = (blueStartZ + z) >> 1;    
                        }
                        if ((!presentBrightness) && (buffer[pos] >= BRIGHT_RED)) {
                            presentBrightness = true;
                        }
                        if (histoInfo) {
                            presentPos[presentPosIndex++] = pos;
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
                        if (histoInfo) {
                            for (i = 0; i < threePreviousWidth; i++) {
                                threePreviousPos[i] = twoPreviousPos[i];
                            }
                            for (i = 0; i < twoPreviousWidth; i++) {
                                twoPreviousPos[i] = onePreviousPos[i];
                            }
                            for (i = 0; i < onePreviousWidth; i++) {
                                onePreviousPos[i] = presentPos[i];
                            }
                            presentPosIndex = 0;
                            presentPos[presentPosIndex++] = pos;
                        } // if (histoInfo)
                        if (buffer[pos] < BRIGHT_RED) {
                            presentColor = buffer[pos];
                            if (buffer[pos] == NONE) {
                                presentBrightColor = NONE;
                            }
                            else {
                                presentBrightColor = presentColor + 3;
                            }
                            presentBrightness = false;
                        }
                        else {
                            presentBrightColor = buffer[pos];
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
                if (histoInfo) {
                    for (i = 0; i < threePreviousWidth; i++) {
                        threePreviousPos[i] = twoPreviousPos[i];
                    }
                    for (i = 0; i < twoPreviousWidth; i++) {
                        twoPreviousPos[i] = onePreviousPos[i];
                    }
                    for (i = 0; i < onePreviousWidth; i++) {
                        onePreviousPos[i] = presentPos[i];
                    }
                } // if (histoInfo)
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
                    pos = x + z * xySlice + y * xDim;
                    if ((buffer[pos] == presentColor) || (buffer[pos] == presentBrightColor)) {
                        presentWidth++;
                        if ((presentColor == BRIGHT_BLUE) || (presentColor == BLUE)) {
                            blueX = (blueStartX + x) >> 1;
                            blueY = (blueStartY + y) >> 1;
                            blueZ = (blueStartZ + z) >> 1;    
                        }
                        if ((!presentBrightness) && (buffer[pos] >= BRIGHT_RED)) {
                            presentBrightness = true;
                        }
                        if (histoInfo) {
                            presentPos[presentPosIndex++] = pos;
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
                        if (histoInfo) {
                            for (i = 0; i < threePreviousWidth; i++) {
                                threePreviousPos[i] = twoPreviousPos[i];
                            }
                            for (i = 0; i < twoPreviousWidth; i++) {
                                twoPreviousPos[i] = onePreviousPos[i];
                            }
                            for (i = 0; i < onePreviousWidth; i++) {
                                onePreviousPos[i] = presentPos[i];
                            }
                            presentPosIndex = 0;
                            presentPos[presentPosIndex++] = pos;
                        } // if (histoInfo)
                        if (buffer[pos] < BRIGHT_RED) {
                            presentColor = buffer[pos];
                            if (buffer[pos] == NONE) {
                                presentBrightColor = NONE;
                            }
                            else {
                                presentBrightColor = presentColor + 3;
                            }
                            presentBrightness = false;
                        }
                        else {
                            presentBrightColor = buffer[pos];
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
                if (histoInfo) {
                    for (i = 0; i < threePreviousWidth; i++) {
                        threePreviousPos[i] = twoPreviousPos[i];
                    }
                    for (i = 0; i < twoPreviousWidth; i++) {
                        twoPreviousPos[i] = onePreviousPos[i];
                    }
                    for (i = 0; i < onePreviousWidth; i++) {
                        onePreviousPos[i] = presentPos[i];
                    }
                } // if (histoInfo)
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
                    pos = z * xySlice + y * xDim + x;
                    if ((buffer[pos] == presentColor) || (buffer[pos] == presentBrightColor)) {
                        presentWidth++;
                        if ((presentColor == BRIGHT_BLUE) || (presentColor == BLUE)) {
                            blueX = (blueStartX + x) >> 1;
                            blueY = (blueStartY + y) >> 1;
                            blueZ = (blueStartZ + z) >> 1;    
                        }
                        if ((!presentBrightness) && (buffer[pos] >= BRIGHT_RED)) {
                            presentBrightness = true;
                        }
                        if (histoInfo) {
                            presentPos[presentPosIndex++] = pos;
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
                        if (histoInfo) {
                            for (i = 0; i < threePreviousWidth; i++) {
                                threePreviousPos[i] = twoPreviousPos[i];
                            }
                            for (i = 0; i < twoPreviousWidth; i++) {
                                twoPreviousPos[i] = onePreviousPos[i];
                            }
                            for (i = 0; i < onePreviousWidth; i++) {
                                onePreviousPos[i] = presentPos[i];
                            }
                            presentPosIndex = 0;
                            presentPos[presentPosIndex++] = pos;
                        } // if (histoInfo)
                        if (buffer[pos] < BRIGHT_RED) {
                            presentColor = buffer[pos];
                            if (buffer[pos] == NONE) {
                                presentBrightColor = NONE;
                            }
                            else {
                                presentBrightColor = presentColor + 3;
                            }
                            presentBrightness = false;
                        }
                        else {
                            presentBrightColor = buffer[pos];
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
                if (histoInfo) {
                    for (i = 0; i < threePreviousWidth; i++) {
                        threePreviousPos[i] = twoPreviousPos[i];
                    }
                    for (i = 0; i < twoPreviousWidth; i++) {
                        twoPreviousPos[i] = onePreviousPos[i];
                    }
                    for (i = 0; i < onePreviousWidth; i++) {
                        onePreviousPos[i] = presentPos[i];
                    }
                } // if (histoInfo)
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
                    pos = z * xySlice + y * xDim + x;
                    if ((buffer[pos] == presentColor) || (buffer[pos] == presentBrightColor)) {
                        presentWidth++;
                        if ((presentColor == BRIGHT_BLUE) || (presentColor == BLUE)) {
                            blueX = (blueStartX + x) >> 1;
                            blueY = (blueStartY + y) >> 1;
                            blueZ = (blueStartZ + z) >> 1;    
                        }
                        if ((!presentBrightness) && (buffer[pos] >= BRIGHT_RED)) {
                            presentBrightness = true;
                        }
                        if (histoInfo) {
                            presentPos[presentPosIndex++] = pos;
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
                        if (histoInfo) {
                            for (i = 0; i < threePreviousWidth; i++) {
                                threePreviousPos[i] = twoPreviousPos[i];
                            }
                            for (i = 0; i < twoPreviousWidth; i++) {
                                twoPreviousPos[i] = onePreviousPos[i];
                            }
                            for (i = 0; i < onePreviousWidth; i++) {
                                onePreviousPos[i] = presentPos[i];
                            }
                            presentPosIndex = 0;
                            presentPos[presentPosIndex++] = pos;
                        } // if (histoInfo)
                        if (buffer[pos] < BRIGHT_RED) {
                            presentColor = buffer[pos];
                            if (buffer[pos] == NONE) {
                                presentBrightColor = NONE;
                            }
                            else {
                                presentBrightColor = presentColor + 3;
                            }
                            presentBrightness = false;
                        }
                        else {
                            presentBrightColor = buffer[pos];
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
                if (histoInfo) {
                    for (i = 0; i < threePreviousWidth; i++) {
                        threePreviousPos[i] = twoPreviousPos[i];
                    }
                    for (i = 0; i < twoPreviousWidth; i++) {
                        twoPreviousPos[i] = onePreviousPos[i];
                    }
                    for (i = 0; i < onePreviousWidth; i++) {
                        onePreviousPos[i] = presentPos[i];
                    }
                } // if (histoInfo)
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
                    pos = z * xySlice + y * xDim + x;
                    if ((buffer[pos] == presentColor) || (buffer[pos] == presentBrightColor)) {
                        presentWidth++;
                        if ((presentColor == BRIGHT_BLUE) || (presentColor == BLUE)) {
                            blueX = (blueStartX + x) >> 1;
                            blueY = (blueStartY + y) >> 1;
                            blueZ = (blueStartZ + z) >> 1;    
                        }
                        if ((!presentBrightness) && (buffer[pos] >= BRIGHT_RED)) {
                            presentBrightness = true;
                        }
                        if (histoInfo) {
                            presentPos[presentPosIndex++] = pos;
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
                        if (histoInfo) {
                            for (i = 0; i < threePreviousWidth; i++) {
                                threePreviousPos[i] = twoPreviousPos[i];
                            }
                            for (i = 0; i < twoPreviousWidth; i++) {
                                twoPreviousPos[i] = onePreviousPos[i];
                            }
                            for (i = 0; i < onePreviousWidth; i++) {
                                onePreviousPos[i] = presentPos[i];
                            }
                            presentPosIndex = 0;
                            presentPos[presentPosIndex++] = pos;
                        } // if (histoInfo)
                        if (buffer[pos] < BRIGHT_RED) {
                            presentColor = buffer[pos];
                            if (buffer[pos] == NONE) {
                                presentBrightColor = NONE;
                            }
                            else {
                                presentBrightColor = presentColor + 3;
                            }
                            presentBrightness = false;
                        }
                        else {
                            presentBrightColor = buffer[pos];
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
                if (histoInfo) {
                    for (i = 0; i < threePreviousWidth; i++) {
                        threePreviousPos[i] = twoPreviousPos[i];
                    }
                    for (i = 0; i < twoPreviousWidth; i++) {
                        twoPreviousPos[i] = onePreviousPos[i];
                    }
                    for (i = 0; i < onePreviousWidth; i++) {
                        onePreviousPos[i] = presentPos[i];
                    }
                } // if (histoInfo)
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
                    pos = z * xySlice + y * xDim + x;
                    if ((buffer[pos] == presentColor) || (buffer[pos] == presentBrightColor)) {
                        presentWidth++;
                        if ((presentColor == BRIGHT_BLUE) || (presentColor == BLUE)) {
                            blueX = (blueStartX + x) >> 1;
                            blueY = (blueStartY + y) >> 1;
                            blueZ = (blueStartZ + z) >> 1;    
                        }
                        if ((!presentBrightness) && (buffer[pos] >= BRIGHT_RED)) {
                            presentBrightness = true;
                        }
                        if (histoInfo) {
                            presentPos[presentPosIndex++] = pos;
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
                        if (histoInfo) {
                            for (i = 0; i < threePreviousWidth; i++) {
                                threePreviousPos[i] = twoPreviousPos[i];
                            }
                            for (i = 0; i < twoPreviousWidth; i++) {
                                twoPreviousPos[i] = onePreviousPos[i];
                            }
                            for (i = 0; i < onePreviousWidth; i++) {
                                onePreviousPos[i] = presentPos[i];
                            }
                            presentPosIndex = 0;
                            presentPos[presentPosIndex++] = pos;
                        } // if (histoInfo)
                        if (buffer[pos] < BRIGHT_RED) {
                            presentColor = buffer[pos];
                            if (buffer[pos] == NONE) {
                                presentBrightColor = NONE;
                            }
                            else {
                                presentBrightColor = presentColor + 3;
                            }
                            presentBrightness = false;
                        }
                        else {
                            presentBrightColor = buffer[pos];
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
                if (histoInfo) {
                    for (i = 0; i < threePreviousWidth; i++) {
                        threePreviousPos[i] = twoPreviousPos[i];
                    }
                    for (i = 0; i < twoPreviousWidth; i++) {
                        twoPreviousPos[i] = onePreviousPos[i];
                    }
                    for (i = 0; i < onePreviousWidth; i++) {
                        onePreviousPos[i] = presentPos[i];
                    }
                } // if (histoInfo)
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
                    pos = z * xySlice + y * xDim + x;
                    if ((buffer[pos] == presentColor) || (buffer[pos] == presentBrightColor)) {
                        presentWidth++;
                        if ((presentColor == BRIGHT_BLUE) || (presentColor == BLUE)) {
                            blueX = (blueStartX + x) >> 1;
                            blueY = (blueStartY + y) >> 1;
                            blueZ = (blueStartZ + z) >> 1;    
                        }
                        if ((!presentBrightness) && (buffer[pos] >= BRIGHT_RED)) {
                            presentBrightness = true;
                        }
                        if (histoInfo) {
                            presentPos[presentPosIndex++] = pos;
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
                        if (histoInfo) {
                            for (i = 0; i < threePreviousWidth; i++) {
                                threePreviousPos[i] = twoPreviousPos[i];
                            }
                            for (i = 0; i < twoPreviousWidth; i++) {
                                twoPreviousPos[i] = onePreviousPos[i];
                            }
                            for (i = 0; i < onePreviousWidth; i++) {
                                onePreviousPos[i] = presentPos[i];
                            }
                            presentPosIndex = 0;
                            presentPos[presentPosIndex++] = pos;
                        } // if (histoInfo)
                        if (buffer[pos] < BRIGHT_RED) {
                            presentColor = buffer[pos];
                            if (buffer[pos] == NONE) {
                                presentBrightColor = NONE;
                            }
                            else {
                                presentBrightColor = presentColor + 3;
                            }
                            presentBrightness = false;
                        }
                        else {
                            presentBrightColor = buffer[pos];
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
                if (histoInfo) {
                    for (i = 0; i < threePreviousWidth; i++) {
                        threePreviousPos[i] = twoPreviousPos[i];
                    }
                    for (i = 0; i < twoPreviousWidth; i++) {
                        twoPreviousPos[i] = onePreviousPos[i];
                    }
                    for (i = 0; i < onePreviousWidth; i++) {
                        onePreviousPos[i] = presentPos[i];
                    }
                } // if (histoInfo)
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
                    pos = z * xySlice + y * xDim + x;
                    if ((buffer[pos] == presentColor) || (buffer[pos] == presentBrightColor)) {
                        presentWidth++;
                        if ((presentColor == BRIGHT_BLUE) || (presentColor == BLUE)) {
                            blueX = (blueStartX + x) >> 1;
                            blueY = (blueStartY + y) >> 1;
                            blueZ = (blueStartZ + z) >> 1;    
                        }
                        if ((!presentBrightness) && (buffer[pos] >= BRIGHT_RED)) {
                            presentBrightness = true;
                        }
                        if (histoInfo) {
                            presentPos[presentPosIndex++] = pos;
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
                        if (histoInfo) {
                            for (i = 0; i < threePreviousWidth; i++) {
                                threePreviousPos[i] = twoPreviousPos[i];
                            }
                            for (i = 0; i < twoPreviousWidth; i++) {
                                twoPreviousPos[i] = onePreviousPos[i];
                            }
                            for (i = 0; i < onePreviousWidth; i++) {
                                onePreviousPos[i] = presentPos[i];
                            }
                            presentPosIndex = 0;
                            presentPos[presentPosIndex++] = pos;
                        } // if (histoInfo)
                        if (buffer[pos] < BRIGHT_RED) {
                            presentColor = buffer[pos];
                            if (buffer[pos] == NONE) {
                                presentBrightColor = NONE;
                            }
                            else {
                                presentBrightColor = presentColor + 3;
                            }
                            presentBrightness = false;
                        }
                        else {
                            presentBrightColor = buffer[pos];
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
                if (histoInfo) {
                    for (i = 0; i < threePreviousWidth; i++) {
                        threePreviousPos[i] = twoPreviousPos[i];
                    }
                    for (i = 0; i < twoPreviousWidth; i++) {
                        twoPreviousPos[i] = onePreviousPos[i];
                    }
                    for (i = 0; i < onePreviousWidth; i++) {
                        onePreviousPos[i] = presentPos[i];
                    }
                } // if (histoInfo)
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
                    pos = z * xySlice + y * xDim + x;
                    if ((buffer[pos] == presentColor) || (buffer[pos] == presentBrightColor)) {
                        presentWidth++;
                        if ((presentColor == BRIGHT_BLUE) || (presentColor == BLUE)) {
                            blueX = (blueStartX + x) >> 1;
                            blueY = (blueStartY + y) >> 1;
                            blueZ = (blueStartZ + z) >> 1;    
                        }
                        if ((!presentBrightness) && (buffer[pos] >= BRIGHT_RED)) {
                            presentBrightness = true;
                        }
                        if (histoInfo) {
                            presentPos[presentPosIndex++] = pos;
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
                        if (histoInfo) {
                            for (i = 0; i < threePreviousWidth; i++) {
                                threePreviousPos[i] = twoPreviousPos[i];
                            }
                            for (i = 0; i < twoPreviousWidth; i++) {
                                twoPreviousPos[i] = onePreviousPos[i];
                            }
                            for (i = 0; i < onePreviousWidth; i++) {
                                onePreviousPos[i] = presentPos[i];
                            }
                            presentPosIndex = 0;
                            presentPos[presentPosIndex++] = pos;
                        } // if (histoInfo)
                        if (buffer[pos] < BRIGHT_RED) {
                            presentColor = buffer[pos];
                            if (buffer[pos] == NONE) {
                                presentBrightColor = NONE;
                            }
                            else {
                                presentBrightColor = presentColor + 3;
                            }
                            presentBrightness = false;
                        }
                        else {
                            presentBrightColor = buffer[pos];
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
                if (histoInfo) {
                    for (i = 0; i < threePreviousWidth; i++) {
                        threePreviousPos[i] = twoPreviousPos[i];
                    }
                    for (i = 0; i < twoPreviousWidth; i++) {
                        twoPreviousPos[i] = onePreviousPos[i];
                    }
                    for (i = 0; i < onePreviousWidth; i++) {
                        onePreviousPos[i] = presentPos[i];
                    }
                } // if (histoInfo)
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
                    pos = z * xySlice + y * xDim + x;
                    if ((buffer[pos] == presentColor) || (buffer[pos] == presentBrightColor)) {
                        presentWidth++;
                        if ((presentColor == BRIGHT_BLUE) || (presentColor == BLUE)) {
                            blueX = (blueStartX + x) >> 1;
                            blueY = (blueStartY + y) >> 1;
                            blueZ = (blueStartZ + z) >> 1;    
                        }
                        if ((!presentBrightness) && (buffer[pos] >= BRIGHT_RED)) {
                            presentBrightness = true;
                        }
                        if (histoInfo) {
                            presentPos[presentPosIndex++] = pos;
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
                        if (histoInfo) {
                            for (i = 0; i < threePreviousWidth; i++) {
                                threePreviousPos[i] = twoPreviousPos[i];
                            }
                            for (i = 0; i < twoPreviousWidth; i++) {
                                twoPreviousPos[i] = onePreviousPos[i];
                            }
                            for (i = 0; i < onePreviousWidth; i++) {
                                onePreviousPos[i] = presentPos[i];
                            }
                            presentPosIndex = 0;
                            presentPos[presentPosIndex++] = pos;
                        } // if (histoInfo)
                        if (buffer[pos] < BRIGHT_RED) {
                            presentColor = buffer[pos];
                            if (buffer[pos] == NONE) {
                                presentBrightColor = NONE;
                            }
                            else {
                                presentBrightColor = presentColor + 3;
                            }
                            presentBrightness = false;
                        }
                        else {
                            presentBrightColor = buffer[pos];
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
                if (histoInfo) {
                    for (i = 0; i < threePreviousWidth; i++) {
                        threePreviousPos[i] = twoPreviousPos[i];
                    }
                    for (i = 0; i < twoPreviousWidth; i++) {
                        twoPreviousPos[i] = onePreviousPos[i];
                    }
                    for (i = 0; i < onePreviousWidth; i++) {
                        onePreviousPos[i] = presentPos[i];
                    }
                } // if (histoInfo)
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
                    pos = z * xySlice + y * xDim + x;
                    if ((buffer[pos] == presentColor) || (buffer[pos] == presentBrightColor)) {
                        presentWidth++;
                        if ((presentColor == BRIGHT_BLUE) || (presentColor == BLUE)) {
                            blueX = (blueStartX + x) >> 1;
                            blueY = (blueStartY + y) >> 1;
                            blueZ = (blueStartZ + z) >> 1;    
                        }
                        if ((!presentBrightness) && (buffer[pos] >= BRIGHT_RED)) {
                            presentBrightness = true;
                        }
                        if (histoInfo) {
                            presentPos[presentPosIndex++] = pos;
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
                        if (histoInfo) {
                            for (i = 0; i < threePreviousWidth; i++) {
                                threePreviousPos[i] = twoPreviousPos[i];
                            }
                            for (i = 0; i < twoPreviousWidth; i++) {
                                twoPreviousPos[i] = onePreviousPos[i];
                            }
                            for (i = 0; i < onePreviousWidth; i++) {
                                onePreviousPos[i] = presentPos[i];
                            }
                            presentPosIndex = 0;
                            presentPos[presentPosIndex++] = pos;
                        } // if (histoInfo)
                        if (buffer[pos] < BRIGHT_RED) {
                            presentColor = buffer[pos];
                            if (buffer[pos] == NONE) {
                                presentBrightColor = NONE;
                            }
                            else {
                                presentBrightColor = presentColor + 3;
                            }
                            presentBrightness = false;
                        }
                        else {
                            presentBrightColor = buffer[pos];
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
                if (histoInfo) {
                    for (i = 0; i < threePreviousWidth; i++) {
                        threePreviousPos[i] = twoPreviousPos[i];
                    }
                    for (i = 0; i < twoPreviousWidth; i++) {
                        twoPreviousPos[i] = onePreviousPos[i];
                    }
                    for (i = 0; i < onePreviousWidth; i++) {
                        onePreviousPos[i] = presentPos[i];
                    }
                } // if (histoInfo)
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
                    pos = z * xySlice + y * xDim + x;
                    if ((buffer[pos] == presentColor) || (buffer[pos] == presentBrightColor)) {
                        presentWidth++;
                        if ((presentColor == BRIGHT_BLUE) || (presentColor == BLUE)) {
                            blueX = (blueStartX + x) >> 1;
                            blueY = (blueStartY + y) >> 1;
                            blueZ = (blueStartZ + z) >> 1;    
                        }
                        if ((!presentBrightness) && (buffer[pos] >= BRIGHT_RED)) {
                            presentBrightness = true;
                        }
                        if (histoInfo) {
                            presentPos[presentPosIndex++] = pos;
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
                        if (histoInfo) {
                            for (i = 0; i < threePreviousWidth; i++) {
                                threePreviousPos[i] = twoPreviousPos[i];
                            }
                            for (i = 0; i < twoPreviousWidth; i++) {
                                twoPreviousPos[i] = onePreviousPos[i];
                            }
                            for (i = 0; i < onePreviousWidth; i++) {
                                onePreviousPos[i] = presentPos[i];
                            }
                            presentPosIndex = 0;
                            presentPos[presentPosIndex++] = pos;
                        } // if (histoInfo)
                        if (buffer[pos] < BRIGHT_RED) {
                            presentColor = buffer[pos];
                            if (buffer[pos] == NONE) {
                                presentBrightColor = NONE;
                            }
                            else {
                                presentBrightColor = presentColor + 3;
                            }
                            presentBrightness = false;
                        }
                        else {
                            presentBrightColor = buffer[pos];
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
                if (histoInfo) {
                    for (i = 0; i < threePreviousWidth; i++) {
                        threePreviousPos[i] = twoPreviousPos[i];
                    }
                    for (i = 0; i < twoPreviousWidth; i++) {
                        twoPreviousPos[i] = onePreviousPos[i];
                    }
                    for (i = 0; i < onePreviousWidth; i++) {
                        onePreviousPos[i] = presentPos[i];
                    }
                } // if (histoInfo)
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
                    pos = z * xySlice + y * xDim + x;
                    if ((buffer[pos] == presentColor) || (buffer[pos] == presentBrightColor)) {
                        presentWidth++;
                        if ((presentColor == BRIGHT_BLUE) || (presentColor == BLUE)) {
                            blueX = (blueStartX + x) >> 1;
                            blueY = (blueStartY + y) >> 1;
                            blueZ = (blueStartZ + z) >> 1;    
                        }
                        if ((!presentBrightness) && (buffer[pos] >= BRIGHT_RED)) {
                            presentBrightness = true;
                        }
                        if (histoInfo) {
                            presentPos[presentPosIndex++] = pos;
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
                        if (histoInfo) {
                            for (i = 0; i < threePreviousWidth; i++) {
                                threePreviousPos[i] = twoPreviousPos[i];
                            }
                            for (i = 0; i < twoPreviousWidth; i++) {
                                twoPreviousPos[i] = onePreviousPos[i];
                            }
                            for (i = 0; i < onePreviousWidth; i++) {
                                onePreviousPos[i] = presentPos[i];
                            }
                            presentPosIndex = 0;
                            presentPos[presentPosIndex++] = pos;
                        } // if (histoInfo)
                        if (buffer[pos] < BRIGHT_RED) {
                            presentColor = buffer[pos];
                            if (buffer[pos] == NONE) {
                                presentBrightColor = NONE;
                            }
                            else {
                                presentBrightColor = presentColor + 3;
                            }
                            presentBrightness = false;
                        }
                        else {
                            presentBrightColor = buffer[pos];
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
                if (histoInfo) {
                    for (i = 0; i < threePreviousWidth; i++) {
                        threePreviousPos[i] = twoPreviousPos[i];
                    }
                    for (i = 0; i < twoPreviousWidth; i++) {
                        twoPreviousPos[i] = onePreviousPos[i];
                    }
                    for (i = 0; i < onePreviousWidth; i++) {
                        onePreviousPos[i] = presentPos[i];
                    }
                } // if (histoInfo)
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
                    pos = z * xySlice + y * xDim + x;
                    if ((buffer[pos] == presentColor) || (buffer[pos] == presentBrightColor)) {
                        presentWidth++;
                        if ((presentColor == BRIGHT_BLUE) || (presentColor == BLUE)) {
                            blueX = (blueStartX + x) >> 1;
                            blueY = (blueStartY + y) >> 1;
                            blueZ = (blueStartZ + z) >> 1;    
                        }
                        if ((!presentBrightness) && (buffer[pos] >= BRIGHT_RED)) {
                            presentBrightness = true;
                        }
                        if (histoInfo) {
                            presentPos[presentPosIndex++] = pos;
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
                        if (histoInfo) {
                            for (i = 0; i < threePreviousWidth; i++) {
                                threePreviousPos[i] = twoPreviousPos[i];
                            }
                            for (i = 0; i < twoPreviousWidth; i++) {
                                twoPreviousPos[i] = onePreviousPos[i];
                            }
                            for (i = 0; i < onePreviousWidth; i++) {
                                onePreviousPos[i] = presentPos[i];
                            }
                            presentPosIndex = 0;
                            presentPos[presentPosIndex++] = pos;
                        } // if (histoInfo)
                        if (buffer[pos] < BRIGHT_RED) {
                            presentColor = buffer[pos];
                            if (buffer[pos] == NONE) {
                                presentBrightColor = NONE;
                            }
                            else {
                                presentBrightColor = presentColor + 3;
                            }
                            presentBrightness = false;
                        }
                        else {
                            presentBrightColor = buffer[pos];
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
                if (histoInfo) {
                    for (i = 0; i < threePreviousWidth; i++) {
                        threePreviousPos[i] = twoPreviousPos[i];
                    }
                    for (i = 0; i < twoPreviousWidth; i++) {
                        twoPreviousPos[i] = onePreviousPos[i];
                    }
                    for (i = 0; i < onePreviousWidth; i++) {
                        onePreviousPos[i] = presentPos[i];
                    }
                } // if (histoInfo)
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
        
        if (histoInfo) {
            threePreviousPos = null;
            twoPreviousPos = null;
            onePreviousPos = null;
            presentPos = null;
        }
        
        srcImage.notifyImageDisplayListeners();
        System.out.println("Total synapses found = " + numSynapses);
        Preferences.debug("Total synapses found = " + numSynapses + "\n");
        dataString += "\nTotal synapses found = " + numSynapses + "\n";
        dataString += "Minimum blue count = " + minimumBlueCount + "\n";
        dataString += "Maximum blue count = " + maximumBlueCount + "\n";
        averageBlueCount = (double)totalBlueCount/numSynapses;
        dataString += "Average blue count = " + String.format("%.2f\n",averageBlueCount);
        blueStandardDeviation = (totalBlueCountSquared - (double)totalBlueCount * (double)totalBlueCount/numSynapses)/(numSynapses - 1);
        blueStandardDeviation = Math.sqrt(blueStandardDeviation);
        dataString += "Blue count standard deviation = " + String.format("%.2f\n",blueStandardDeviation);
        fileDirectory = srcImage.getImageDirectory();
        fileName = srcImage.getImageName() + ".txt";
        file = new File(fileDirectory + fileName);

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
        
        blueMask = null;
        for (i = 0; i < length; i++) {
            if (blueMask2.get(i))  {
                buffer[i] = (byte)255;
            }
            else {
                buffer[i] = 0;
            }
        }
        blueMask2 = null;
        maskImage = new ModelImage(ModelStorageBase.ARGB,srcImage.getExtents(),srcImage.getImageName() + "_mask");
        try {
            maskImage.importRGBData(1, 0, buffer, false);
            maskImage.importRGBData(2, 0, buffer, false);
            maskImage.importRGBData(3, 0, buffer, true);
        }
        catch (IOException e) {
            errorCleanUp("Algorithm SynapseDetection reports IOException on maskImage.importRGBData", true);

            return;    
        }      
        
        if (histoInfo) {
            greenBuffer = new byte[length];
            blueBuffer = new byte[length];
            
            try {
                srcImage.exportRGBData(1, 0, length, buffer); // export red data
            } catch (IOException error) {
                buffer = null;
                greenBuffer = null;
                blueBuffer = null;
                errorCleanUp("Algorithm SynapseDetection reports: source image locked", true);

                return;
            }
            
            try {
                srcImage.exportRGBData(2, 0, length, greenBuffer); // export green data
            } catch (IOException error) {
                buffer = null;
                greenBuffer = null;
                blueBuffer = null;
                errorCleanUp("Algorithm SynapseDetection reports: source image locked", true);

                return;
            }
            
            try {
                srcImage.exportRGBData(3, 0, length, blueBuffer); // export blue data
            } catch (IOException error) {
                buffer = null;
                greenBuffer = null;
                blueBuffer = null;
                errorCleanUp("Algorithm SynapseDetection reports: source image locked", true);

                return;
            }
            
            redExtents = new float[256];
            greenExtents = new float[256];
            blueExtents = new float[256];
            
            for (pos = 0; pos < length; pos++) {
                if (colorMask.get(pos)) {
                    red = buffer[pos] & 0xff;
                    green = greenBuffer[pos] & 0xff;
                    blue = blueBuffer[pos] & 0xff;
                    if (blue >= blueIntensity) {
                        blueExtents[blue]++;
                    }
                    else if ((blue > red) && (blue > green)) {
                        blueExtents[blue]++;
                    }
                    else if ((red > green) && (red > blue)) {
                        redExtents[red]++;
                    }
                    else if ((green > red) && (green > blue)) {
                        greenExtents[green]++;
                    } 
                } // if (colorMask.get(pos))
            } // for (pos = 0; pos < length; pos++)
            colorMask = null;
            buffer = null;
            greenBuffer = null;
            blueBuffer = null;
            
            xInit = new float[256];
            for (i = 0; i < 256; i++) {
                xInit[i] = (float)i;
            }
            redGraph = new ViewJFrameGraph(xInit, redExtents, "RED", "Intensity", "Count", Color.RED);
            greenGraph = new ViewJFrameGraph(xInit, greenExtents, "GREEN", "Intensity", "Count", Color.GREEN);
            blueGraph = new ViewJFrameGraph(xInit, blueExtents, "BLUE", "Intensity", "Count", Color.BLUE);
        } // if (histoInfo)
        buffer = null;
        new ViewJFrameImage(maskImage);
       
        fireProgressStateChanged(100);
        time = System.currentTimeMillis() - time;
        Preferences.debug("PlugInAlgorithmSynapseDetection elapsed time in seconds = " + (time/1000.0));
        setCompleted(true);
        
    }
    
    // Finding red, blue, green or green, blue, red with all 3 colors in the appropriate width range
    // means that a synapse has been found within a plane
    private void checkForSynapseXY() {
       VOI newPtVOI;
       int i;
       if ((threePreviousBrightness && twoPreviousBrightness && onePreviousBrightness &&
           (twoPreviousColor == BLUE) && (twoPreviousWidth >= blueMinXY) && (twoPreviousWidth <= blueMaxXY)) && 
           (((threePreviousColor == RED) && (threePreviousWidth >= redMin) && (threePreviousWidth <= redMax) &&
           (onePreviousColor == GREEN) && (onePreviousWidth >= greenMin) && (onePreviousWidth <= greenMax)) ||
           ((threePreviousColor == GREEN) && (threePreviousWidth >= greenMin) && (threePreviousWidth <= greenMax) &&
           (onePreviousColor == RED) && (onePreviousWidth >= redMin) && (onePreviousWidth <= redMax)))) {
           // If centerAndZeroBlueIteration is not used, before searches parallel to the x axis are completed,
           // we run out of java heap space with numSynapses = 126,738.
           centerAndZeroBlueIteration();
           newPtVOI = new VOI((short) (numSynapses), Integer.toString(numSynapses+1), zDim, VOI.POINT, -1.0f);
           newPtVOI.setColor(Color.white);
           xArr[0] = centerBlueX;
           yArr[0] = centerBlueY;
           zArr[0] = centerBlueZ;
           newPtVOI.importCurve(xArr, yArr, zArr, centerBlueZ);
           ((VOIPoint) (newPtVOI.getCurves()[centerBlueZ].elementAt(0))).setFixed(true);
          ((VOIPoint) (newPtVOI.getCurves()[centerBlueZ].elementAt(0))).setLabel(Integer.toString(numSynapses + 1));
          ((VOIPoint) (newPtVOI.getCurves()[centerBlueZ].elementAt(0))).setName(Integer.toString(numSynapses + 1));
           srcImage.registerVOI(newPtVOI);  
           numSynapses++;
           if (histoInfo) {
               for (i = 0; i < threePreviousWidth; i++) {
                   colorMask.set(threePreviousPos[i]);
               }
               for (i = 0; i < twoPreviousWidth; i++) {
                   colorMask.set(twoPreviousPos[i]);
               }
               for (i = 0; i < onePreviousWidth; i++) {
                   colorMask.set(onePreviousPos[i]);
               }
           } // if (histoInfo)
       }
    }
    
    //Finding red, blue, green or green, blue, red with all 3 colors in the appropriate width range
    // means that a synapse has been found between planes
    private void checkForSynapseZ() {
       VOI newPtVOI;
       int i;
       if ((threePreviousBrightness && twoPreviousBrightness && onePreviousBrightness &&
           (twoPreviousColor == BLUE) && (twoPreviousWidth >= blueMinZ) && (twoPreviousWidth <= blueMaxZ)) && 
           (((threePreviousColor == RED) && (threePreviousWidth >= redMin) && (threePreviousWidth <= redMax) &&
           (onePreviousColor == GREEN) && (onePreviousWidth >= greenMin) && (onePreviousWidth <= greenMax)) ||
           ((threePreviousColor == GREEN) && (threePreviousWidth >= greenMin) && (threePreviousWidth <= greenMax) &&
           (onePreviousColor == RED) && (onePreviousWidth >= redMin) && (onePreviousWidth <= redMax)))) {
           // If zero centerAndZeroBlueIteration is not used, before searches parallel to the x axis are completed,
           // we run out of java heap space with numSynapses = 126,738.
           centerAndZeroBlueIteration();
           newPtVOI = new VOI((short) (numSynapses), Integer.toString(numSynapses+1), zDim, VOI.POINT, -1.0f);
           newPtVOI.setColor(Color.white);
           xArr[0] = centerBlueX;
           yArr[0] = centerBlueY;
           zArr[0] = centerBlueZ;
           newPtVOI.importCurve(xArr, yArr, zArr, centerBlueZ);
           ((VOIPoint) (newPtVOI.getCurves()[centerBlueZ].elementAt(0))).setFixed(true);
          ((VOIPoint) (newPtVOI.getCurves()[centerBlueZ].elementAt(0))).setLabel(Integer.toString(numSynapses + 1));
          ((VOIPoint) (newPtVOI.getCurves()[centerBlueZ].elementAt(0))).setName(Integer.toString(numSynapses + 1));
           srcImage.registerVOI(newPtVOI);  
           numSynapses++;
           if (histoInfo) {
               for (i = 0; i < threePreviousWidth; i++) {
                   colorMask.set(threePreviousPos[i]);
               }
               for (i = 0; i < twoPreviousWidth; i++) {
                   colorMask.set(twoPreviousPos[i]);
               }
               for (i = 0; i < onePreviousWidth; i++) {
                   colorMask.set(onePreviousPos[i]);
               }
           } // if (histoInfo)
       }
    }
    
    // Change the BLUE or BRIGHT_BLUE of a detected synapse to NONE so it is not detected more than once.
    // Change all 26 neighbor connected BLUE or BRIGHT_BLUE to the original BLUE or BRIGHT_BLUE to NONE inside a block
    // of width 2 * blueMaxXY - 1, of length 2 * blueMaxXY - 1, and of height 2 * blueMaxZ - 1 around the originally 
    // detected BLUE or BRIGHT_BLUE pixel, so that only 1 BRIGHT_BLUE or BLUE pixel from each synapse can trigger a synapse
    // detection.  Find the center of the 26 connected BLUE and BRIGHT_BLUE pixels and use this as the synapse center.
    private void centerAndZeroBlueIteration() {
        boolean change = true;
        int x;
        int y;
        int z;
        int yPos;
        int zPos;
        int i = blueX + xDim * blueY + xySlice * blueZ;
        int del = -1;
        int zLow;
        int zHigh;
        int yLow;
        int yHigh;
        int xLow;
        int xHigh;
        buffer[i] = NONE;
        blueMask.set(i);
        blueMask2.set(i);
        int delXY;
        int delZ;
        centerBlueX = blueX;
        centerBlueY = blueY;
        centerBlueZ = blueZ;
        blueCount = 1;
        while (change && (del <= blueMax - 2)) {
            change = false;
            del++;
            delXY = Math.min(blueMaxXY-1, del);
            delZ = Math.min(blueMaxZ-1, del);
            xLow = Math.max(0, blueX-delXY);
            xHigh = Math.min(xDim-1, blueX + delXY);
            yLow = Math.max(0, blueY-delXY);
            yHigh = Math.min(yDim-1, blueY + delXY);
            zLow = Math.max(0, blueZ-delZ);
            zHigh = Math.min(zDim-1, blueZ + delZ);
            for (z = zLow; z <= zHigh; z++) {
                zPos = z * xySlice;
                for (y = yLow; y <= yHigh; y++) {
                    yPos = zPos + y * xDim;
                    for (x = xLow; x <= xHigh; x++) {
                        i = yPos + x; 
                        if (blueMask.get(i)) {
                            if ((x > 0) && (!blueMask.get(i-1)) && ((buffer[i-1] == BRIGHT_BLUE) || (buffer[i-1] == BLUE))) {  
                                buffer[i-1] = NONE;
                                blueMask.set(i-1);
                                blueMask2.set(i-1);
                                change = true;
                                centerBlueX += (x - 1);
                                centerBlueY += y;
                                centerBlueZ += z;
                                blueCount++;
                            }
                            if ((x < xDim - 1) && (!blueMask.get(i+1)) && ((buffer[i+1] == BRIGHT_BLUE) || (buffer[i+1] == BLUE))) {
                                buffer[i+1] = NONE;
                                blueMask.set(i+1);
                                blueMask2.set(i+1);
                                change = true;
                                centerBlueX += (x + 1);
                                centerBlueY += y;
                                centerBlueZ += z;
                                blueCount++;
                            }
                            if ((y > 0)&& (!blueMask.get(i-xDim)) && ((buffer[i-xDim] == BRIGHT_BLUE) || (buffer[i-xDim] == BLUE))) {
                                buffer[i-xDim] = NONE;
                                blueMask.set(i-xDim);
                                blueMask2.set(i-xDim);
                                change = true;
                                centerBlueX += x;
                                centerBlueY += (y - 1);
                                centerBlueZ += z;
                                blueCount++;
                            }
                            if ((y < yDim - 1) && (!blueMask.get(i+xDim)) && ((buffer[i+xDim] == BRIGHT_BLUE) || (buffer[i+xDim] == BLUE))) {
                                buffer[i+xDim] = NONE;
                                blueMask.set(i+xDim);
                                blueMask2.set(i+xDim);
                                change = true;
                                centerBlueX += x;
                                centerBlueY += (y + 1);
                                centerBlueZ += z;
                                blueCount++;
                            }
                            if ((z > 0) && (!blueMask.get(i-xySlice)) && ((buffer[i-xySlice] == BRIGHT_BLUE) || (buffer[i-xySlice] == BLUE))) {
                                buffer[i-xySlice] = NONE;
                                blueMask.set(i-xySlice);
                                blueMask2.set(i-xySlice);
                                change = true;
                                centerBlueX += x;
                                centerBlueY += y;
                                centerBlueZ += (z - 1);
                                blueCount++;
                            }
                            if ((z < zDim - 1) && (!blueMask.get(i+xySlice)) && ((buffer[i+xySlice] == BRIGHT_BLUE) || (buffer[i+xySlice] == BLUE))) {
                                buffer[i+xySlice] = NONE;
                                blueMask.set(i+xySlice);
                                blueMask2.set(i+xySlice);
                                change = true;
                                centerBlueX += x;
                                centerBlueY += y;
                                centerBlueZ += (z + 1);
                                blueCount++;
                            }
                            if ((x > 0) && (y > 0) && (!blueMask.get(i-xDim-1)) && ((buffer[i-xDim-1] == BRIGHT_BLUE) || (buffer[i-xDim-1] == BLUE))) {
                                buffer[i-xDim-1] = NONE;
                                blueMask.set(i-xDim-1);
                                blueMask2.set(i-xDim-1);
                                change = true;
                                centerBlueX += (x - 1);
                                centerBlueY += (y - 1);
                                centerBlueZ += z;
                                blueCount++;
                            }
                            if ((x > 0) && (y < yDim - 1) && (!blueMask.get(i+xDim-1)) && ((buffer[i+xDim-1] == BRIGHT_BLUE) || (buffer[i+xDim-1] == BLUE))) {
                                buffer[i+xDim-1] = NONE;
                                blueMask.set(i+xDim-1);
                                blueMask2.set(i+xDim-1);
                                change = true;
                                centerBlueX += (x - 1);
                                centerBlueY += (y + 1);
                                centerBlueZ += z;
                                blueCount++;
                            }
                            if ((x < xDim-1) && (y > 0) && (!blueMask.get(i-xDim+1)) && ((buffer[i-xDim+1] == BRIGHT_BLUE) || (buffer[i-xDim+1] == BLUE))) {
                                buffer[i-xDim+1] = NONE;
                                blueMask.set(i-xDim+1);
                                blueMask2.set(i-xDim+1);
                                change = true;
                                centerBlueX += (x + 1);
                                centerBlueY += (y - 1);
                                centerBlueZ += z;
                                blueCount++;
                            }
                            if ((x < xDim - 1) && (y < yDim - 1) && (!blueMask.get(i+xDim+1)) && ((buffer[i+xDim+1] == BRIGHT_BLUE) || (buffer[i+xDim+1] == BLUE))) {
                                buffer[i+xDim+1] = NONE;
                                blueMask.set(i+xDim+1);
                                blueMask2.set(i+xDim+1);
                                change = true;
                                centerBlueX += (x + 1);
                                centerBlueY += (y + 1);
                                centerBlueZ += z;
                                blueCount++;
                            }
                            if ((x > 0) && (z > 0) && (!blueMask.get(i-xySlice-1)) && ((buffer[i-xySlice-1] == BRIGHT_BLUE) || (buffer[i-xySlice-1] == BLUE))) {
                                buffer[i-xySlice-1] = NONE;
                                blueMask.set(i-xySlice-1);
                                blueMask2.set(i-xySlice-1);
                                change = true;
                                centerBlueX += (x - 1);
                                centerBlueY += y;
                                centerBlueZ += (z - 1);
                                blueCount++;
                            }
                            if ((x > 0) && (z < zDim - 1) && (!blueMask.get(i+xySlice-1)) && ((buffer[i+xySlice-1] == BRIGHT_BLUE) || (buffer[i+xySlice-1] == BLUE))) {
                                buffer[i+xySlice-1] = NONE;
                                blueMask.set(i+xySlice-1);
                                blueMask2.set(i+xySlice-1);
                                change = true;
                                centerBlueX += (x - 1);
                                centerBlueY += y;
                                centerBlueZ += (z + 1);
                                blueCount++;
                            }
                            if ((x < xDim-1) && (z > 0) && (!blueMask.get(i-xySlice+1)) && ((buffer[i-xySlice+1] == BRIGHT_BLUE) || (buffer[i-xySlice+1] == BLUE))) {
                                buffer[i-xySlice+1] = NONE;
                                blueMask.set(i-xySlice+1);
                                blueMask2.set(i-xySlice+1);
                                change = true;
                                centerBlueX += (x + 1);
                                centerBlueY += y;
                                centerBlueZ += (z - 1);
                                blueCount++;
                            }
                            if ((x < xDim - 1) && (z < zDim - 1) && (!blueMask.get(i+xySlice+1)) && ((buffer[i+xySlice+1] == BRIGHT_BLUE) || (buffer[i+xySlice+1] == BLUE))) {
                                buffer[i+xySlice+1] = NONE;
                                blueMask.set(i+xySlice+1);
                                blueMask2.set(i+xySlice+1);
                                change = true;
                                centerBlueX += (x + 1);
                                centerBlueY += y;
                                centerBlueZ += (z + 1);
                                blueCount++;
                            }
                            if ((y > 0) && (z > 0) && (!blueMask.get(i-xySlice-xDim)) && ((buffer[i-xySlice-xDim] == BRIGHT_BLUE) || (buffer[i-xySlice-xDim] == BLUE))) {
                                buffer[i-xySlice-xDim] = NONE;
                                blueMask.set(i-xySlice-xDim);
                                blueMask2.set(i-xySlice-xDim);
                                change = true;
                                centerBlueX += x;
                                centerBlueY += (y - 1);
                                centerBlueZ += (z - 1);
                                blueCount++;
                            }
                            if ((y > 0) && (z < zDim - 1) && (!blueMask.get(i+xySlice-xDim)) && ((buffer[i+xySlice-xDim] == BRIGHT_BLUE) || (buffer[i+xySlice-xDim] == BLUE))) {
                                buffer[i+xySlice-xDim] = NONE;
                                blueMask.set(i+xySlice-xDim);
                                blueMask2.set(i+xySlice-xDim);
                                change = true;
                                centerBlueX += x;
                                centerBlueY += (y - 1);
                                centerBlueZ += (z + 1);
                                blueCount++;
                            }
                            if ((y < yDim-1) && (z > 0) && (!blueMask.get(i-xySlice+xDim)) && ((buffer[i-xySlice+xDim] == BRIGHT_BLUE) || (buffer[i-xySlice+xDim] == BLUE))) {
                                buffer[i-xySlice+xDim] = NONE;
                                blueMask.set(i-xySlice+xDim);
                                blueMask2.set(i-xySlice+xDim);
                                change = true;
                                centerBlueX += x;
                                centerBlueY += (y + 1);
                                centerBlueZ += (z - 1);
                                blueCount++;
                            }
                            if ((y < yDim - 1) && (z < zDim - 1) && (!blueMask.get(i+xySlice+xDim)) && ((buffer[i+xySlice+xDim] == BRIGHT_BLUE) || (buffer[i+xySlice+xDim] == BLUE))) {
                                buffer[i+xySlice+xDim] = NONE;
                                blueMask.set(i+xySlice+xDim);
                                blueMask2.set(i+xySlice+xDim);
                                change = true;
                                centerBlueX += x;
                                centerBlueY += (y + 1);
                                centerBlueZ += (z + 1);
                                blueCount++;
                            }
                            if ((x > 0) && (y > 0) && (z > 0)&& (!blueMask.get(i-xySlice-xDim-1)) && ((buffer[i-xySlice-xDim-1] == BRIGHT_BLUE) || (buffer[i-xySlice-xDim-1] == BLUE))) {
                                buffer[i-xySlice-xDim-1] = NONE;
                                blueMask.set(i-xySlice-xDim-1);
                                blueMask2.set(i-xySlice-xDim-1);
                                change = true;
                                centerBlueX += (x - 1);
                                centerBlueY += (y - 1);
                                centerBlueZ += (z - 1);
                                blueCount++;
                            }
                            if ((x > 0) && (y > 0) && (z < zDim - 1) && (!blueMask.get(i+xySlice-xDim-1)) && ((buffer[i+xySlice-xDim-1] == BRIGHT_BLUE) ||(buffer[i+xySlice-xDim-1] == BLUE))) {
                                buffer[i+xySlice-xDim-1] = NONE;
                                blueMask.set(i+xySlice-xDim-1);
                                blueMask2.set(i+xySlice-xDim-1);
                                change = true;
                                centerBlueX += (x - 1);
                                centerBlueY += (y - 1);
                                centerBlueZ += (z + 1);
                                blueCount++;
                            }
                            if ((x > 0) && (y < yDim - 1) && (z > 0)&& (!blueMask.get(i-xySlice+xDim-1)) && ((buffer[i-xySlice+xDim-1] == BRIGHT_BLUE) || (buffer[i-xySlice+xDim-1] == BLUE))) {
                                buffer[i-xySlice+xDim-1] = NONE;
                                blueMask.set(i-xySlice+xDim-1);
                                blueMask2.set(i-xySlice+xDim-1);
                                change = true;
                                centerBlueX += (x - 1);
                                centerBlueY += (y + 1);
                                centerBlueZ += (z - 1);
                                blueCount++;
                            }
                            if ((x > 0) && (y < yDim - 1) && (z < zDim - 1) && (!blueMask.get(i+xySlice+xDim-1)) && ((buffer[i+xySlice+xDim-1] == BRIGHT_BLUE) || (buffer[i+xySlice+xDim-1] == BLUE))) {
                                buffer[i+xySlice+xDim-1] = NONE;
                                blueMask.set(i+xySlice+xDim-1);
                                blueMask2.set(i+xySlice+xDim-1);
                                change = true;
                                centerBlueX += (x - 1);
                                centerBlueY += (y + 1);
                                centerBlueZ += (z + 1);
                                blueCount++;
                            }
                            if ((x < xDim-1) && (y > 0) && (z > 0) && (!blueMask.get(i-xySlice-xDim+1)) && ((buffer[i-xySlice-xDim+1] == BRIGHT_BLUE) || (buffer[i-xySlice-xDim+1] == BLUE))) {
                                buffer[i-xySlice-xDim+1] = NONE;
                                blueMask.set(i-xySlice-xDim+1);
                                blueMask2.set(i-xySlice-xDim+1);
                                change = true;
                                centerBlueX += (x + 1);
                                centerBlueY += (y - 1);
                                centerBlueZ += (z - 1);
                                blueCount++;
                            }
                            if ((x < xDim-1) && (y > 0) && (z < zDim - 1) && (!blueMask.get(i+xySlice-xDim+1)) && ((buffer[i+xySlice-xDim+1] == BRIGHT_BLUE) || (buffer[i+xySlice-xDim+1] == BLUE))) {
                                buffer[i+xySlice-xDim+1] = NONE;
                                blueMask.set(i+xySlice-xDim+1);
                                blueMask2.set(i+xySlice-xDim+1);
                                change = true;
                                centerBlueX += (x + 1);
                                centerBlueY += (y - 1);
                                centerBlueZ += (z + 1);
                                blueCount++;
                            }
                            if ((x < xDim - 1) && (y < yDim - 1) && (z > 0) && (!blueMask.get(i-xySlice+xDim+1)) && ((buffer[i-xySlice+xDim+1] == BRIGHT_BLUE) ||(buffer[i-xySlice+xDim+1] == BLUE))) {
                                buffer[i-xySlice+xDim+1] = NONE;
                                blueMask.set(i-xySlice+xDim+1);
                                blueMask2.set(i-xySlice+xDim+1);
                                change = true;
                                centerBlueX += (x + 1);
                                centerBlueY += (y + 1);
                                centerBlueZ += (z - 1);
                                blueCount++;
                            }
                            if ((x < xDim - 1) && (y < yDim - 1) && (z < zDim - 1) && (!blueMask.get(i+xySlice+xDim+1)) && ((buffer[i+xySlice+xDim+1] == BRIGHT_BLUE) || (buffer[i+xySlice+xDim+1] == BLUE))) {
                                buffer[i+xySlice+xDim+1] = NONE;
                                blueMask.set(i+xySlice+xDim+1);
                                blueMask2.set(i+xySlice+xDim+1);
                                change = true;
                                centerBlueX += (x + 1);
                                centerBlueY += (y + 1);
                                centerBlueZ += (z + 1);
                                blueCount++;
                            }
                            blueMask.clear(i);
                        } // if (blueMask.get(i))
                    } // for (x = xLow; x <= xHigh; x++)
                } // for (y = yLow; y <= yHigh; y++)
            } // for (z = zLow; z <= zHigh; z++)
        } // while (change  && (del <= (blueMax - 2))
        centerBlueX = Math.round((float)centerBlueX/blueCount);
        centerBlueY = Math.round((float)centerBlueY/blueCount);
        centerBlueZ = Math.round((float)centerBlueZ/blueCount);
        dataString += String.format("%-10d%-10d%-10d%-10d%-10d\n",numSynapses+1, centerBlueX, centerBlueY, centerBlueZ, blueCount);
        if (blueCount < minimumBlueCount) {
            minimumBlueCount = blueCount;
        }
        if (blueCount > maximumBlueCount) {
            maximumBlueCount = blueCount;
        }
        totalBlueCount += blueCount;
        totalBlueCountSquared += blueCount * blueCount;
    }
    
    // Recursion triggers a stack overflow - don't use
    private void zeroBlueBufferRecursion(int xDel, int yDel, int zDel) {
      int i = xDel + xDim * yDel + xySlice * zDel;
      buffer[i] = NONE;
      if ((xDel > 0) && (buffer[i-1] == BLUE)) {
          zeroBlueBufferRecursion(xDel - 1, yDel, zDel);
      }
      if ((xDel < xDim - 1) && (buffer[i+1] == BLUE)) {
          zeroBlueBufferRecursion(xDel + 1, yDel, zDel);
      }
      if ((yDel > 0) && (buffer[i-xDim] == BLUE)) {
          zeroBlueBufferRecursion(xDel, yDel - 1, zDel);
      }
      if ((yDel < yDim - 1) && (buffer[i+xDim] == BLUE)) {
          zeroBlueBufferRecursion(xDel, yDel + 1, zDel);
      }
      if ((zDel > 0) && (buffer[i-xySlice] == BLUE)) {
          zeroBlueBufferRecursion(xDel, yDel, zDel - 1);
      }
      if ((zDel < zDim - 1) && (buffer[i+xySlice] == BLUE)) {
          zeroBlueBufferRecursion(xDel, yDel, zDel + 1);
      }
      if ((xDel > 0) && (yDel > 0) && (buffer[i-xDim-1] == BLUE)) {
          zeroBlueBufferRecursion(xDel - 1, yDel - 1, zDel);
      }
      if ((xDel > 0) && (yDel < yDim - 1) && (buffer[i+xDim-1] == BLUE)) {
          zeroBlueBufferRecursion(xDel - 1, yDel + 1, zDel);
      }
      if ((xDel < xDim-1) && (yDel > 0) && (buffer[i-xDim+1] == BLUE)) {
          zeroBlueBufferRecursion(xDel + 1, yDel - 1, zDel);
      }
      if ((xDel < xDim - 1) && (yDel < yDim - 1) && (buffer[i+xDim+1] == BLUE)) {
          zeroBlueBufferRecursion(xDel + 1, yDel + 1, zDel);
      }
      if ((xDel > 0) && (zDel > 0) && (buffer[i-xySlice-1] == BLUE)) {
          zeroBlueBufferRecursion(xDel - 1, yDel, zDel - 1);
      }
      if ((xDel > 0) && (zDel < zDim - 1) && (buffer[i+xySlice-1] == BLUE)) {
          zeroBlueBufferRecursion(xDel - 1, yDel, zDel + 1);
      }
      if ((xDel < xDim-1) && (zDel > 0) && (buffer[i-xySlice+1] == BLUE)) {
          zeroBlueBufferRecursion(xDel + 1, yDel, zDel - 1);
      }
      if ((xDel < xDim - 1) && (zDel < zDim - 1) && (buffer[i+xySlice+1] == BLUE)) {
          zeroBlueBufferRecursion(xDel + 1, yDel, zDel + 1);
      }
      if ((yDel > 0) && (zDel > 0) && (buffer[i-xySlice-xDim] == BLUE)) {
          zeroBlueBufferRecursion(xDel, yDel - 1, zDel - 1);
      }
      if ((yDel > 0) && (zDel < zDim - 1) && (buffer[i+xySlice-xDim] == BLUE)) {
          zeroBlueBufferRecursion(xDel, yDel - 1, zDel + 1);
      }
      if ((yDel < yDim-1) && (zDel > 0) && (buffer[i-xySlice+xDim] == BLUE)) {
          zeroBlueBufferRecursion(xDel, yDel + 1, zDel - 1);
      }
      if ((yDel < yDim - 1) && (zDel < zDim - 1) && (buffer[i+xySlice+xDim] == BLUE)) {
          zeroBlueBufferRecursion(xDel, yDel + 1, zDel + 1);
      }
      if ((xDel > 0) && (yDel > 0) && (zDel > 0)&& (buffer[i-xySlice-xDim-1] == BLUE)) {
          zeroBlueBufferRecursion(xDel - 1, yDel - 1, zDel - 1);
      }
      if ((xDel > 0) && (yDel > 0) && (zDel < zDim - 1) && (buffer[i+xySlice-xDim-1] == BLUE)) {
          zeroBlueBufferRecursion(xDel - 1, yDel - 1, zDel + 1);
      }
      if ((xDel > 0) && (yDel < yDim - 1) && (zDel > 0)&& (buffer[i-xySlice+xDim-1] == BLUE)) {
          zeroBlueBufferRecursion(xDel - 1, yDel + 1, zDel - 1);
      }
      if ((xDel > 0) && (yDel < yDim - 1) && (zDel < zDim - 1) && (buffer[i+xySlice+xDim-1] == BLUE)) {
          zeroBlueBufferRecursion(xDel - 1, yDel + 1, zDel + 1);
      }
      if ((xDel < xDim-1) && (yDel > 0) && (zDel > 0) && (buffer[i-xySlice-xDim+1] == BLUE)) {
          zeroBlueBufferRecursion(xDel + 1, yDel - 1, zDel - 1);
      }
      if ((xDel < xDim-1) && (yDel > 0) && (zDel < zDim - 1) && (buffer[i+xySlice-xDim+1] == BLUE)) {
          zeroBlueBufferRecursion(xDel + 1, yDel - 1, zDel + 1);
      }
      if ((xDel < xDim - 1) && (yDel < yDim - 1) && (zDel > 0) && (buffer[i-xySlice+xDim+1] == BLUE)) {
          zeroBlueBufferRecursion(xDel + 1, yDel + 1, zDel - 1);
      }
      if ((xDel < xDim - 1) && (yDel < yDim - 1) && (zDel < zDim - 1) && (buffer[i+xySlice+xDim+1] == BLUE)) {
          zeroBlueBufferRecursion(xDel + 1, yDel + 1, zDel + 1);
      }
    }

    
}
