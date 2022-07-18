import gov.nih.mipav.model.algorithms.*;
import gov.nih.mipav.model.structures.*;

import gov.nih.mipav.view.*;

import java.awt.*;

import java.io.*;

import java.util.*;


/**
 *
 * @version  March 19, 2009
 * @author   William Gandler
 * @see      AlgorithmBase
 *
 *           PlugInAlgorithmSynapseDetection is used to place a point marker over the blue part of the synapse.
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
    
    //  true if blueFraction >= 0.05
    private boolean bigBlueFraction = false;
    
    /* Minimum intensity values */
    private int redIntensity = 5;
    
    private int redBrightIntensity = 55;
    
    private int greenIntensity = 5;
    
    private int greenBrightIntensity = 85;
    
    private int blueIntensity = 15;
    
    private int blueBrightIntensity = 80;
    
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
    private byte greenBuffer[] = new byte[xySlice];
    private byte blueBuffer[] = new byte[xySlice];
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
    private BitSet colorMask = null;
    private float xInit[] = null;
    private float redExtents[] = null;
    private float greenExtents[] = null;
    private float blueExtents[] = null;
    private int threePreviousPos[] = null;
    private int twoPreviousPos[] = null;
    private int onePreviousPos[] = null;
    private int presentPos[] = null;
    private int presentPosIndex = 0;
    private int colorObjects[];
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
    // If bigBlueFraction will allow storage of 5 synapses for each red object
    // If not bigBlueFraction allow only 1 synapse per blue object
    private int synapseBlueFound[];
    private int synapseGreenFound[];
    private int threePreviousObjectIndex;
    private int twoPreviousObjectIndex;
    private int onePreviousObjectIndex;
    private int presentObjectIndex;

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
     * @param  bigBlueFraction true if blueFraction >= 0.05
     * @param  redIntensity      Minimum red intensity
     * @param  redBrightIntensity Minimum bright red intensity
     * @param  greenIntensity    Minimum green intensity
     * @param  greenBrightIntensity Minimum bright green intensity
     * @param  blueIntensity     Minimum blue intensity
     * @param  blueBrightIntensity Minimum bright blue intensity
     * @param  histoInfo         If true, provide histogram information of red, green, and
     *                           blue values along detected lines
     */
    public PlugInAlgorithmSynapseDetection(ModelImage srcImg, int redMin, int redMax, int greenMin,
                                         int greenMax, int blueMinXY, int blueMaxXY, 
                                         int blueMinZ, int blueMaxZ, boolean bigBlueFraction,
                                         int redIntensity, int redBrightIntensity,
                                         int greenIntensity, int greenBrightIntensity, int blueIntensity,
                                         int blueBrightIntensity, boolean histoInfo) {
        super(null, srcImg);
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
        this.redBrightIntensity = redBrightIntensity;
        this.greenIntensity = greenIntensity;
        this.greenBrightIntensity = greenBrightIntensity;
        this.blueIntensity = blueIntensity;
        this.blueBrightIntensity = blueBrightIntensity;
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
        int j;
        int k;
        
        fireProgressStateChanged("Synapse detection on image");

        time = System.currentTimeMillis();
        
        try {
            srcImage.exportRGBData(1, 0, length, buffer); // export red data
        } catch (IOException error) {
            buffer = null;
            greenBuffer = null;
            blueBuffer = null;
            errorCleanUp("Algorithm SynapseDetection reports: source image locked", true);

            return;
        }
        
        for (z = 0; z < zDim; z++) {
            try {
                srcImage.exportRGBData(2, 4*z*xySlice, xySlice, greenBuffer); // export green data
            } catch (IOException error) {
                buffer = null;
                greenBuffer = null;
                blueBuffer = null;
                errorCleanUp("Algorithm SynapseDetection reports: source image locked", true);
    
                return;
            }
            
            try {
                srcImage.exportRGBData(3, 4*z*xySlice, xySlice, blueBuffer); // export blue data
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
            for (pos = 0; pos < xySlice; pos++) {
                zPos = z*xySlice + pos;
                red = buffer[zPos] & 0xff;
                green = greenBuffer[pos] & 0xff;
                blue = blueBuffer[pos] & 0xff;
                if (blue >= blueBrightIntensity) {
                    buffer[zPos] = BRIGHT_BLUE;
                }
                else if ((blue > red) && (blue > green) && (blue >= blueIntensity)) {
                    buffer[zPos] = BLUE;
                }
                else if ((red > green) && (red > blue) && (red >= redIntensity)) {
                    if (red >= redBrightIntensity) {
                        buffer[zPos] = BRIGHT_RED;
                    }
                    else {
                        buffer[zPos] = RED;
                    }
                }
                else if ((green > red) && (green > blue) && (green >= greenIntensity)) {
                    if (green >= greenBrightIntensity) {
                        buffer[zPos] = BRIGHT_GREEN;
                    }
                    else {
                        buffer[zPos] = GREEN;
                    }
                }
                else {
                    buffer[zPos] = NONE;
                }
               
            } // for (pos = 0; pos < xySlice; pos++)
        } // for (z = 0; z < zDim; z++)
           
        greenBuffer = null;
        blueBuffer = null;
        System.gc();
        
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
        
        if (histoInfo) {
            threePreviousPos = new int[1000];
            twoPreviousPos = new int[1000];
            onePreviousPos = new int[1000];
            presentPos = new int[1000];
            colorMask = new BitSet(length);
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
                       threePreviousObjectIndex = twoPreviousObjectIndex;
                       twoPreviousObjectIndex = onePreviousObjectIndex;
                       onePreviousObjectIndex = presentObjectIndex;
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
                           presentObjectIndex = colorObjects[pos];
                       }
                       else if ((presentColor == BRIGHT_RED) || (presentColor == RED)) {
                           if (bigBlueFraction) {
                               presentObjectIndex = colorObjects[pos];
                           }
                           else {
                               presentObjectIndex = 0;
                           }
                       }
                       else if ((presentColor == BRIGHT_GREEN) || (presentColor == GREEN)) {
                           if (bigBlueFraction) {
                               presentObjectIndex = colorObjects[pos];
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
                        threePreviousObjectIndex = twoPreviousObjectIndex;
                        twoPreviousObjectIndex = onePreviousObjectIndex;
                        onePreviousObjectIndex = presentObjectIndex;
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
                            presentObjectIndex = colorObjects[pos];
                        }
                        else if ((presentColor == BRIGHT_RED) || (presentColor == RED)) {
                            if (bigBlueFraction) {
                                presentObjectIndex = colorObjects[pos];
                            }
                            else {
                                presentObjectIndex = 0;
                            }
                        }
                        else if ((presentColor == BRIGHT_GREEN) || (presentColor == GREEN)) {
                            if (bigBlueFraction) {
                                presentObjectIndex = colorObjects[pos];
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
                        threePreviousObjectIndex = twoPreviousObjectIndex;
                        twoPreviousObjectIndex = onePreviousObjectIndex;
                        onePreviousObjectIndex = presentObjectIndex;
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
                            presentObjectIndex = colorObjects[pos];
                        }
                        else if ((presentColor == BRIGHT_RED) || (presentColor == RED)) {
                            if (bigBlueFraction) {
                                presentObjectIndex = colorObjects[pos];
                            }
                            else {
                                presentObjectIndex = 0;
                            }
                        }
                        else if ((presentColor == BRIGHT_GREEN) || (presentColor == GREEN)) {
                            if (bigBlueFraction) {
                                presentObjectIndex = colorObjects[pos];
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
                        threePreviousObjectIndex = twoPreviousObjectIndex;
                        twoPreviousObjectIndex = onePreviousObjectIndex;
                        onePreviousObjectIndex = presentObjectIndex;
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
                            presentObjectIndex = colorObjects[pos];
                        }
                        else if ((presentColor == BRIGHT_RED) || (presentColor == RED)) {
                            if (bigBlueFraction) {
                                presentObjectIndex = colorObjects[pos];
                            }
                            else {
                                presentObjectIndex = 0;
                            }
                        }
                        else if ((presentColor == BRIGHT_GREEN) || (presentColor == GREEN)) {
                            if (bigBlueFraction) {
                                presentObjectIndex = colorObjects[pos];
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
                        threePreviousObjectIndex = twoPreviousObjectIndex;
                        twoPreviousObjectIndex = onePreviousObjectIndex;
                        onePreviousObjectIndex = presentObjectIndex;
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
                            presentObjectIndex = colorObjects[pos];
                        }
                        else if ((presentColor == BRIGHT_RED) || (presentColor == RED)) {
                            if (bigBlueFraction) {
                                presentObjectIndex = colorObjects[pos];
                            }
                            else {
                                presentObjectIndex = 0;
                            }
                        }
                        else if ((presentColor == BRIGHT_GREEN) || (presentColor == GREEN)) {
                            if (bigBlueFraction) {
                                presentObjectIndex = colorObjects[pos];
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
                        threePreviousObjectIndex = twoPreviousObjectIndex;
                        twoPreviousObjectIndex = onePreviousObjectIndex;
                        onePreviousObjectIndex = presentObjectIndex;
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
                            presentObjectIndex = colorObjects[pos];
                        }
                        else if ((presentColor == BRIGHT_RED) || (presentColor == RED)) {
                            if (bigBlueFraction) {
                                presentObjectIndex = colorObjects[pos];
                            }
                            else {
                                presentObjectIndex = 0;
                            }
                        }
                        else if ((presentColor == BRIGHT_GREEN) || (presentColor == GREEN)) {
                            if (bigBlueFraction) {
                                presentObjectIndex = colorObjects[pos];
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
                        threePreviousObjectIndex = twoPreviousObjectIndex;
                        twoPreviousObjectIndex = onePreviousObjectIndex;
                        onePreviousObjectIndex = presentObjectIndex;
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
                            presentObjectIndex = colorObjects[pos];
                        }
                        else if ((presentColor == BRIGHT_RED) || (presentColor == RED)) {
                            if (bigBlueFraction) {
                                presentObjectIndex = colorObjects[pos];
                            }
                            else {
                                presentObjectIndex = 0;
                            }
                        }
                        else if ((presentColor == BRIGHT_GREEN) || (presentColor == GREEN)) {
                            if (bigBlueFraction) {
                                presentObjectIndex = colorObjects[pos];
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
                        threePreviousObjectIndex = twoPreviousObjectIndex;
                        twoPreviousObjectIndex = onePreviousObjectIndex;
                        onePreviousObjectIndex = presentObjectIndex;
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
                            presentObjectIndex = colorObjects[pos];
                        }
                        else if ((presentColor == BRIGHT_RED) || (presentColor == RED)) {
                            if (bigBlueFraction) {
                                presentObjectIndex = colorObjects[pos];
                            }
                            else {
                                presentObjectIndex = 0;
                            }
                        }
                        else if ((presentColor == BRIGHT_GREEN) || (presentColor == GREEN)) {
                            if (bigBlueFraction) {
                                presentObjectIndex = colorObjects[pos];
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
                        threePreviousObjectIndex = twoPreviousObjectIndex;
                        twoPreviousObjectIndex = onePreviousObjectIndex;
                        onePreviousObjectIndex = presentObjectIndex;
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
                            presentObjectIndex = colorObjects[pos];
                        }
                        else if ((presentColor == BRIGHT_RED) || (presentColor == RED)) {
                            if (bigBlueFraction) {
                                presentObjectIndex = colorObjects[pos];
                            }
                            else {
                                presentObjectIndex = 0;
                            }
                        }
                        else if ((presentColor == BRIGHT_GREEN) || (presentColor == GREEN)) {
                            if (bigBlueFraction) {
                                presentObjectIndex = colorObjects[pos];
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
                        threePreviousObjectIndex = twoPreviousObjectIndex;
                        twoPreviousObjectIndex = onePreviousObjectIndex;
                        onePreviousObjectIndex = presentObjectIndex;
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
                            presentObjectIndex = colorObjects[pos];
                        }
                        else if ((presentColor == BRIGHT_RED) || (presentColor == RED)) {
                            if (bigBlueFraction) {
                                presentObjectIndex = colorObjects[pos];
                            }
                            else {
                                presentObjectIndex = 0;
                            }
                        }
                        else if ((presentColor == BRIGHT_GREEN) || (presentColor == GREEN)) {
                            if (bigBlueFraction) {
                                presentObjectIndex = colorObjects[pos];
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
                        threePreviousObjectIndex = twoPreviousObjectIndex;
                        twoPreviousObjectIndex = onePreviousObjectIndex;
                        onePreviousObjectIndex = presentObjectIndex;
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
                            presentObjectIndex = colorObjects[pos];
                        }
                        else if ((presentColor == BRIGHT_RED) || (presentColor == RED)) {
                            if (bigBlueFraction) {
                                presentObjectIndex = colorObjects[pos];
                            }
                            else {
                                presentObjectIndex = 0;
                            }
                        }
                        else if ((presentColor == BRIGHT_GREEN) || (presentColor == GREEN)) {
                            if (bigBlueFraction) {
                                presentObjectIndex = colorObjects[pos];
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
                        threePreviousObjectIndex = twoPreviousObjectIndex;
                        twoPreviousObjectIndex = onePreviousObjectIndex;
                        onePreviousObjectIndex = presentObjectIndex;
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
                            presentObjectIndex = colorObjects[pos];
                        }
                        else if ((presentColor == BRIGHT_RED) || (presentColor == RED)) {
                            if (bigBlueFraction) {
                                presentObjectIndex = colorObjects[pos];
                            }
                            else {
                                presentObjectIndex = 0;
                            }
                        }
                        else if ((presentColor == BRIGHT_GREEN) || (presentColor == GREEN)) {
                            if (bigBlueFraction) {
                                presentObjectIndex = colorObjects[pos];
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
                        threePreviousObjectIndex = twoPreviousObjectIndex;
                        twoPreviousObjectIndex = onePreviousObjectIndex;
                        onePreviousObjectIndex = presentObjectIndex;
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
                            presentObjectIndex = colorObjects[pos];
                        }
                        else if ((presentColor == BRIGHT_RED) || (presentColor == RED)) {
                            if (bigBlueFraction) {
                                presentObjectIndex = colorObjects[pos];
                            }
                            else {
                                presentObjectIndex = 0;
                            }
                        }
                        else if ((presentColor == BRIGHT_GREEN) || (presentColor == GREEN)) {
                            if (bigBlueFraction) {
                                presentObjectIndex = colorObjects[pos];
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
                        threePreviousObjectIndex = twoPreviousObjectIndex;
                        twoPreviousObjectIndex = onePreviousObjectIndex;
                        onePreviousObjectIndex = presentObjectIndex;
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
                            presentObjectIndex = colorObjects[pos];
                        }
                        else if ((presentColor == BRIGHT_RED) || (presentColor == RED)) {
                            if (bigBlueFraction) {
                                presentObjectIndex = colorObjects[pos];
                            }
                            else {
                                presentObjectIndex = 0;
                            }
                        }
                        else if ((presentColor == BRIGHT_GREEN) || (presentColor == GREEN)) {
                            if (bigBlueFraction) {
                                presentObjectIndex = colorObjects[pos];
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
                        threePreviousObjectIndex = twoPreviousObjectIndex;
                        twoPreviousObjectIndex = onePreviousObjectIndex;
                        onePreviousObjectIndex = presentObjectIndex;
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
                            presentObjectIndex = colorObjects[pos];
                        }
                        else if ((presentColor == BRIGHT_RED) || (presentColor == RED)) {
                            if (bigBlueFraction) {
                                presentObjectIndex = colorObjects[pos];
                            }
                            else {
                                presentObjectIndex = 0;
                            }
                        }
                        else if ((presentColor == BRIGHT_GREEN) || (presentColor == GREEN)) {
                            if (bigBlueFraction) {
                                presentObjectIndex = colorObjects[pos];
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
                        threePreviousObjectIndex = twoPreviousObjectIndex;
                        twoPreviousObjectIndex = onePreviousObjectIndex;
                        onePreviousObjectIndex = presentObjectIndex;
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
                            presentObjectIndex = colorObjects[pos];
                        }
                        else if ((presentColor == BRIGHT_RED) || (presentColor == RED)) {
                            if (bigBlueFraction) {
                                presentObjectIndex = colorObjects[pos];
                            }
                            else {
                                presentObjectIndex = 0;
                            }
                        }
                        else if ((presentColor == BRIGHT_GREEN) || (presentColor == GREEN)) {
                            if (bigBlueFraction) {
                                presentObjectIndex = colorObjects[pos];
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
                        threePreviousObjectIndex = twoPreviousObjectIndex;
                        twoPreviousObjectIndex = onePreviousObjectIndex;
                        onePreviousObjectIndex = presentObjectIndex;
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
                            presentObjectIndex = colorObjects[pos];
                        }
                        else if ((presentColor == BRIGHT_RED) || (presentColor == RED)) {
                            if (bigBlueFraction) {
                                presentObjectIndex = colorObjects[pos];
                            }
                            else {
                                presentObjectIndex = 0;
                            }
                        }
                        else if ((presentColor == BRIGHT_GREEN) || (presentColor == GREEN)) {
                            if (bigBlueFraction) {
                                presentObjectIndex = colorObjects[pos];
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
                        threePreviousObjectIndex = twoPreviousObjectIndex;
                        twoPreviousObjectIndex = onePreviousObjectIndex;
                        onePreviousObjectIndex = presentObjectIndex;
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
                            presentObjectIndex = colorObjects[pos];
                        }
                        else if ((presentColor == BRIGHT_RED) || (presentColor == RED)) {
                            if (bigBlueFraction) {
                                presentObjectIndex = colorObjects[pos];
                            }
                            else {
                                presentObjectIndex = 0;
                            }
                        }
                        else if ((presentColor == BRIGHT_GREEN) || (presentColor == GREEN)) {
                            if (bigBlueFraction) {
                                presentObjectIndex = colorObjects[pos];
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
                        threePreviousObjectIndex = twoPreviousObjectIndex;
                        twoPreviousObjectIndex = onePreviousObjectIndex;
                        onePreviousObjectIndex = presentObjectIndex;
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
                            presentObjectIndex = colorObjects[pos];
                        }
                        else if ((presentColor == BRIGHT_RED) || (presentColor == RED)) {
                            if (bigBlueFraction) {
                                presentObjectIndex = colorObjects[pos];
                            }
                            else {
                                presentObjectIndex = 0;
                            }
                        }
                        else if ((presentColor == BRIGHT_GREEN) || (presentColor == GREEN)) {
                            if (bigBlueFraction) {
                                presentObjectIndex = colorObjects[pos];
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
                        threePreviousObjectIndex = twoPreviousObjectIndex;
                        twoPreviousObjectIndex = onePreviousObjectIndex;
                        onePreviousObjectIndex = presentObjectIndex;
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
                            presentObjectIndex = colorObjects[pos];
                        }
                        else if ((presentColor == BRIGHT_RED) || (presentColor == RED)) {
                            if (bigBlueFraction) {
                                presentObjectIndex = colorObjects[pos];
                            }
                            else {
                                presentObjectIndex = 0;
                            }
                        }
                        else if ((presentColor == BRIGHT_GREEN) || (presentColor == GREEN)) {
                            if (bigBlueFraction) {
                                presentObjectIndex = colorObjects[pos];
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
                        threePreviousObjectIndex = twoPreviousObjectIndex;
                        twoPreviousObjectIndex = onePreviousObjectIndex;
                        onePreviousObjectIndex = presentObjectIndex;
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
                            presentObjectIndex = colorObjects[pos];
                        }
                        else if ((presentColor == BRIGHT_RED) || (presentColor == RED)) {
                            if (bigBlueFraction) {
                                presentObjectIndex = colorObjects[pos];
                            }
                            else {
                                presentObjectIndex = 0;
                            }
                        }
                        else if ((presentColor == BRIGHT_GREEN) || (presentColor == GREEN)) {
                            if (bigBlueFraction) {
                                presentObjectIndex = colorObjects[pos];
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
                        threePreviousObjectIndex = twoPreviousObjectIndex;
                        twoPreviousObjectIndex = onePreviousObjectIndex;
                        onePreviousObjectIndex = presentObjectIndex;
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
                            presentObjectIndex = colorObjects[pos];
                        }
                        else if ((presentColor == BRIGHT_RED) || (presentColor == RED)) {
                            if (bigBlueFraction) {
                                presentObjectIndex = colorObjects[pos];
                            }
                            else {
                                presentObjectIndex = 0;
                            }
                        }
                        else if ((presentColor == BRIGHT_GREEN) || (presentColor == GREEN)) {
                            if (bigBlueFraction) {
                                presentObjectIndex = colorObjects[pos];
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
                        threePreviousObjectIndex = twoPreviousObjectIndex;
                        twoPreviousObjectIndex = onePreviousObjectIndex;
                        onePreviousObjectIndex = presentObjectIndex;
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
                            presentObjectIndex = colorObjects[pos];
                        }
                        else if ((presentColor == BRIGHT_RED) || (presentColor == RED)) {
                            if (bigBlueFraction) {
                                presentObjectIndex = colorObjects[pos];
                            }
                            else {
                                presentObjectIndex = 0;
                            }
                        }
                        else if ((presentColor == BRIGHT_GREEN) || (presentColor == GREEN)) {
                            if (bigBlueFraction) {
                                presentObjectIndex = colorObjects[pos];
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
                        threePreviousObjectIndex = twoPreviousObjectIndex;
                        twoPreviousObjectIndex = onePreviousObjectIndex;
                        onePreviousObjectIndex = presentObjectIndex;
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
                            presentObjectIndex = colorObjects[pos];
                        }
                        else if ((presentColor == BRIGHT_RED) || (presentColor == RED)) {
                            if (bigBlueFraction) {
                                presentObjectIndex = colorObjects[pos];
                            }
                            else {
                                presentObjectIndex = 0;
                            }
                        }
                        else if ((presentColor == BRIGHT_GREEN) || (presentColor == GREEN)) {
                            if (bigBlueFraction) {
                                presentObjectIndex = colorObjects[pos];
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
                        threePreviousObjectIndex = twoPreviousObjectIndex;
                        twoPreviousObjectIndex = onePreviousObjectIndex;
                        onePreviousObjectIndex = presentObjectIndex;
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
                            presentObjectIndex = colorObjects[pos];
                        }
                        else if ((presentColor == BRIGHT_RED) || (presentColor == RED)) {
                            if (bigBlueFraction) {
                                presentObjectIndex = colorObjects[pos];
                            }
                            else {
                                presentObjectIndex = 0;
                            }
                        }
                        else if ((presentColor == BRIGHT_GREEN) || (presentColor == GREEN)) {
                            if (bigBlueFraction) {
                                presentObjectIndex = colorObjects[pos];
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
                        threePreviousObjectIndex = twoPreviousObjectIndex;
                        twoPreviousObjectIndex = onePreviousObjectIndex;
                        onePreviousObjectIndex = presentObjectIndex;
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
                            presentObjectIndex = colorObjects[pos];
                        }
                        else if ((presentColor == BRIGHT_RED) || (presentColor == RED)) {
                            if (bigBlueFraction) {
                                presentObjectIndex = colorObjects[pos];
                            }
                            else {
                                presentObjectIndex = 0;
                            }
                        }
                        else if ((presentColor == BRIGHT_GREEN) || (presentColor == GREEN)) {
                            if (bigBlueFraction) {
                                presentObjectIndex = colorObjects[pos];
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
                        threePreviousObjectIndex = twoPreviousObjectIndex;
                        twoPreviousObjectIndex = onePreviousObjectIndex;
                        onePreviousObjectIndex = presentObjectIndex;
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
                            presentObjectIndex = colorObjects[pos];
                        }
                        else if ((presentColor == BRIGHT_RED) || (presentColor == RED)) {
                            if (bigBlueFraction) {
                                presentObjectIndex = colorObjects[pos];
                            }
                            else {
                                presentObjectIndex = 0;
                            }
                        }
                        else if ((presentColor == BRIGHT_GREEN) || (presentColor == GREEN)) {
                            if (bigBlueFraction) {
                                presentObjectIndex = colorObjects[pos];
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
        if (!bigBlueFraction) {
            dataString += "Minimum blue count = " + minimumBlueCount + "\n";
            dataString += "Maximum blue count = " + maximumBlueCount + "\n";
            averageBlueCount = (double)totalBlueCount/numSynapses;
            dataString += "Average blue count = " + String.format("%.2f\n",averageBlueCount);
            blueStandardDeviation = (totalBlueCountSquared - (double)totalBlueCount * (double)totalBlueCount/numSynapses)/(numSynapses - 1);
            blueStandardDeviation = Math.sqrt(blueStandardDeviation);
            dataString += "Blue count standard deviation = " + String.format("%.2f\n",blueStandardDeviation);
        } // if (!bigBlueFraction)
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
        
        if (bigBlueFraction) {
            loopi:
            for (i = 0; i < colorObjects.length; i++) {
                for (j = 1; j < redObjectIndex; j++) {
                    for (k = 0; k < 5; k++) {
                        if (synapseBlueFound[5*j + k] == 0) {
                            // jump to the next j value
                            break;
                        }
                        else if (synapseBlueFound[5*j + k]  == colorObjects[i]) {
                            // Move on to the nxt blueObject without zeroing this one.
                            continue loopi;
                        }
                    }
                }
                colorObjects[i] = 0;
            }
        } // if (bigBluefraction)
        else {
            for (i = 0; i < colorObjects.length; i++) {
                if (synapseBlueFound[colorObjects[i]] == 0) {
                    colorObjects[i] = 0;    
                }
            }
        }
        
        for (i = 0; i < length; i++) {
            if (colorObjects[i] > 0) {
                buffer[i] = (byte)255;
            }
            else {
                buffer[i] = 0;
            }
        }
       
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
            greenBuffer = new byte[xySlice];
            blueBuffer = new byte[xySlice];
            
            redExtents = new float[256];
            greenExtents = new float[256];
            blueExtents = new float[256];
            
            try {
                srcImage.exportRGBData(1, 0, length, buffer); // export red data
            } catch (IOException error) {
                buffer = null;
                greenBuffer = null;
                blueBuffer = null;
                errorCleanUp("Algorithm SynapseDetection reports: source image locked", true);

                return;
            }
            
            for (z = 0; z < zDim; z++) {
                try {
                    srcImage.exportRGBData(2, 4*z*xySlice, xySlice, greenBuffer); // export green data
                } catch (IOException error) {
                    buffer = null;
                    greenBuffer = null;
                    blueBuffer = null;
                    errorCleanUp("Algorithm SynapseDetection reports: source image locked", true);
    
                    return;
                }
                
                try {
                    srcImage.exportRGBData(3, 4*z*xySlice, xySlice, blueBuffer); // export blue data
                } catch (IOException error) {
                    buffer = null;
                    greenBuffer = null;
                    blueBuffer = null;
                    errorCleanUp("Algorithm SynapseDetection reports: source image locked", true);
    
                    return;
                }
                
                
                
                for (pos = 0; pos < xySlice; pos++) {
                    zPos = z * xySlice + pos;
                    if (colorMask.get(zPos)) {
                        red = buffer[zPos] & 0xff;
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
            } // for (z = 0; z < zDim; z++)
            colorMask = null;
            buffer = null;
            greenBuffer = null;
            blueBuffer = null;
            
            xInit = new float[256];
            for (i = 0; i < 256; i++) {
                xInit[i] = (float)i;
            }
            new ViewJFrameGraph(xInit, redExtents, "RED", "Intensity", "Count", Color.RED);
            new ViewJFrameGraph(xInit, greenExtents, "GREEN", "Intensity", "Count", Color.GREEN);
            new ViewJFrameGraph(xInit, blueExtents, "BLUE", "Intensity", "Count", Color.BLUE);
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
           newPtVOI = new VOI((short) (numSynapses), Integer.toString(numSynapses+1), VOI.POINT, -1.0f);
           newPtVOI.setColor(Color.white);
           xArr[0] = centerBlueX;
           yArr[0] = centerBlueY;
           zArr[0] = centerBlueZ;
           newPtVOI.importCurve(xArr, yArr, zArr);
           ((VOIPoint) (newPtVOI.getCurves().elementAt(0))).setFixed(true);
          ((VOIPoint) (newPtVOI.getCurves().elementAt(0))).setLabel(Integer.toString(numSynapses + 1));
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
           newPtVOI = new VOI((short) (numSynapses), Integer.toString(numSynapses+1), VOI.POINT, -1.0f);
           newPtVOI.setColor(Color.white);
           xArr[0] = centerBlueX;
           yArr[0] = centerBlueY;
           zArr[0] = centerBlueZ;
           newPtVOI.importCurve(xArr, yArr, zArr);
           ((VOIPoint) (newPtVOI.getCurves().elementAt(0))).setFixed(true);
          ((VOIPoint) (newPtVOI.getCurves().elementAt(0))).setLabel(Integer.toString(numSynapses + 1));
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
    
    private void identifyObjects() {
        int i;
        int x;
        int y;
        int z;
        int zPos;
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
        int z2Pos;
        int y2Pos;
        int i2;
        int lowBlueX = 0;
        int highBlueX = 0;
        int lowBlueY = 0;
        int highBlueY = 0;
        int lowBlueZ = 0;
        int highBlueZ = 0;
        colorObjects = new int[length];
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
            zPos = z * xySlice;
            maxZDel = Math.max(z, zDim - 1 - z);
            for (y = 0; y < yDim; y++) {
                yPos = zPos + y * xDim;
                maxYDel = Math.max(y, yDim - 1 - y);
                maxYZDel = Math.max(maxYDel, maxZDel);
                for (x = 0; x < xDim; x++) {
                    i = yPos + x; 
                    maxXDel = Math.max(x, xDim - 1 - x);
                    maxDel = Math.max(maxXDel, maxYZDel);
                    if (bigBlueFraction && (colorObjects[i] == 0) && ((buffer[i] == RED) || (buffer[i] == BRIGHT_RED))) {
                        change = true;
                        colorObjects[i] = ++redObjectIndex;
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
                                z2Pos = z2 * xySlice;
                                for (y2 = yLow; y2 <= yHigh; y2++) {
                                    y2Pos = z2Pos + y2 * xDim;
                                    for (x2 = xLow; x2 <= xHigh; x2++) {
                                        i2 = y2Pos + x2;
                                        if ((colorObjects[i2] > 0) && ((buffer[i2] == BRIGHT_RED) || (buffer[i2] == RED))) {
                                            if ((x2 > 0) && (colorObjects[i2-1] == 0) && ((buffer[i2-1] == BRIGHT_RED) || (buffer[i2-1] == RED))) {
                                                colorObjects[i2-1] = redObjectIndex;
                                                change = true;
                                            }
                                            if ((x2 < xDim - 1) && (colorObjects[i2+1] == 0) && ((buffer[i2+1] == BRIGHT_RED) || (buffer[i2+1] == RED))) {
                                                colorObjects[i2+1] = redObjectIndex;
                                                change = true;
                                            }
                                            if ((y2 > 0)&& (colorObjects[i2-xDim] == 0) && ((buffer[i2-xDim] == BRIGHT_RED) || (buffer[i2-xDim] == RED))) {
                                                colorObjects[i2-xDim] = redObjectIndex;
                                                change = true;
                                            }
                                            if ((y2 < yDim - 1) && (colorObjects[i2+xDim] == 0) && ((buffer[i2+xDim] == BRIGHT_RED) || (buffer[i2+xDim] == RED))) {
                                                colorObjects[i2+xDim] = redObjectIndex;
                                                change = true;
                                            }
                                            if ((z2 > 0) && (colorObjects[i2-xySlice] == 0) && ((buffer[i2-xySlice] == BRIGHT_RED) || (buffer[i2-xySlice] == RED))) {
                                                colorObjects[i2-xySlice] = redObjectIndex;
                                                change = true;
                                            }
                                            if ((z2 < zDim - 1) && (colorObjects[i2+xySlice] == 0) && ((buffer[i2+xySlice] == BRIGHT_RED) || (buffer[i2+xySlice] == RED))) {
                                                colorObjects[i2+xySlice] = redObjectIndex;
                                                change = true;
                                            }
                                            if ((x2 > 0) && (y2 > 0) && (colorObjects[i2-xDim-1] == 0) && ((buffer[i2-xDim-1] == BRIGHT_RED) || (buffer[i2-xDim-1] == RED))) {
                                                colorObjects[i2-xDim-1] = redObjectIndex;
                                                change = true;
                                            }
                                            if ((x2 > 0) && (y2 < yDim - 1) && (colorObjects[i2+xDim-1] == 0) && ((buffer[i2+xDim-1] == BRIGHT_RED) || (buffer[i2+xDim-1] == RED))) {
                                                colorObjects[i2+xDim-1] = redObjectIndex;
                                                change = true;
                                            }
                                            if ((x2 < xDim-1) && (y2 > 0) && (colorObjects[i2-xDim+1] == 0) && ((buffer[i2-xDim+1] == BRIGHT_RED) || (buffer[i2-xDim+1] == RED))) {
                                                colorObjects[i2-xDim+1] = redObjectIndex;
                                                change = true;
                                            }
                                            if ((x2 < xDim - 1) && (y2 < yDim - 1) && (colorObjects[i2+xDim+1] == 0) && ((buffer[i2+xDim+1] == BRIGHT_RED) || (buffer[i2+xDim+1] == RED))) {
                                                colorObjects[i2+xDim+1] = redObjectIndex;
                                                change = true;
                                            }
                                            if ((x2 > 0) && (z2 > 0) && (colorObjects[i2-xySlice-1] == 0) && ((buffer[i2-xySlice-1] == BRIGHT_RED) || (buffer[i2-xySlice-1] == RED))) {
                                                colorObjects[i2-xySlice-1] = redObjectIndex;
                                                change = true;
                                            }
                                            if ((x2 > 0) && (z2 < zDim - 1) && (colorObjects[i2+xySlice-1] == 0) && ((buffer[i2+xySlice-1] == BRIGHT_RED) || (buffer[i2+xySlice-1] == RED))) {
                                                colorObjects[i2+xySlice-1] = redObjectIndex;
                                                change = true;
                                            }
                                            if ((x2 < xDim-1) && (z2 > 0) && (colorObjects[i2-xySlice+1] == 0) && ((buffer[i2-xySlice+1] == BRIGHT_RED) || (buffer[i2-xySlice+1] == RED))) {
                                                colorObjects[i2-xySlice+1] = redObjectIndex;
                                                change = true;
                                            }
                                            if ((x2 < xDim - 1) && (z2 < zDim - 1) && (colorObjects[i2+xySlice+1] == 0) && ((buffer[i2+xySlice+1] == BRIGHT_RED) || (buffer[i2+xySlice+1] == RED))) {
                                                colorObjects[i2+xySlice+1] = redObjectIndex;
                                                change = true;
                                            }
                                            if ((y2 > 0) && (z2 > 0) && (colorObjects[i2-xySlice-xDim] == 0) && ((buffer[i2-xySlice-xDim] == BRIGHT_RED) || (buffer[i2-xySlice-xDim] == RED))) {
                                                colorObjects[i2-xySlice-xDim] = redObjectIndex;
                                                change = true;
                                            }
                                            if ((y2 > 0) && (z2 < zDim - 1) && (colorObjects[i2+xySlice-xDim] == 0) && ((buffer[i2+xySlice-xDim] == BRIGHT_RED) || (buffer[i2+xySlice-xDim] == RED))) {
                                                colorObjects[i2+xySlice-xDim] = redObjectIndex;
                                                change = true;
                                            }
                                            if ((y2 < yDim-1) && (z2 > 0) && (colorObjects[i2-xySlice+xDim] == 0) && ((buffer[i2-xySlice+xDim] == BRIGHT_RED) || (buffer[i2-xySlice+xDim] == RED))) {
                                                colorObjects[i2-xySlice+xDim] = redObjectIndex;
                                                change = true;
                                            }
                                            if ((y2 < yDim - 1) && (z2 < zDim - 1) && (colorObjects[i2+xySlice+xDim] == 0) && ((buffer[i2+xySlice+xDim] == BRIGHT_RED) || (buffer[i2+xySlice+xDim] == RED))) {
                                                colorObjects[i2+xySlice+xDim] = redObjectIndex;
                                                change = true;
                                            }
                                            if ((x2 > 0) && (y2 > 0) && (z2 > 0)&& (colorObjects[i2-xySlice-xDim-1] == 0) && ((buffer[i2-xySlice-xDim-1] == BRIGHT_RED) || (buffer[i2-xySlice-xDim-1] == RED))) {
                                                colorObjects[i2-xySlice-xDim-1] = redObjectIndex;
                                                change = true;
                                            }
                                            if ((x2 > 0) && (y2 > 0) && (z2 < zDim - 1) && (colorObjects[i2+xySlice-xDim-1] == 0) && ((buffer[i2+xySlice-xDim-1] == BRIGHT_RED) ||(buffer[i2+xySlice-xDim-1] == RED))) {
                                                colorObjects[i2+xySlice-xDim-1] = redObjectIndex;
                                                change = true;
                                            }
                                            if ((x2 > 0) && (y2 < yDim - 1) && (z2 > 0)&& (colorObjects[i2-xySlice+xDim-1] == 0) && ((buffer[i2-xySlice+xDim-1] == BRIGHT_RED) || (buffer[i2-xySlice+xDim-1] == RED))) {
                                                colorObjects[i2-xySlice+xDim-1] = redObjectIndex;
                                                change = true;
                                            }
                                            if ((x2 > 0) && (y2 < yDim - 1) && (z2 < zDim - 1) && (colorObjects[i2+xySlice+xDim-1] == 0) && ((buffer[i2+xySlice+xDim-1] == BRIGHT_RED) || (buffer[i2+xySlice+xDim-1] == RED))) {
                                                colorObjects[i2+xySlice+xDim-1] = redObjectIndex;
                                                change = true;
                                            }
                                            if ((x2 < xDim-1) && (y2 > 0) && (z2 > 0) && (colorObjects[i2-xySlice-xDim+1] == 0) && ((buffer[i2-xySlice-xDim+1] == BRIGHT_RED) || (buffer[i2-xySlice-xDim+1] == RED))) {
                                                colorObjects[i2-xySlice-xDim+1] = redObjectIndex;
                                                change = true;
                                            }
                                            if ((x2 < xDim-1) && (y2 > 0) && (z2 < zDim - 1) && (colorObjects[i2+xySlice-xDim+1] == 0) && ((buffer[i2+xySlice-xDim+1] == BRIGHT_RED) || (buffer[i2+xySlice-xDim+1] == RED))) {
                                                colorObjects[i2+xySlice-xDim+1] = redObjectIndex;
                                                change = true;
                                            }
                                            if ((x2 < xDim - 1) && (y2 < yDim - 1) && (z2 > 0) && (colorObjects[i2-xySlice+xDim+1] == 0) && ((buffer[i2-xySlice+xDim+1] == BRIGHT_RED) ||(buffer[i2-xySlice+xDim+1] == RED))) {
                                                colorObjects[i2-xySlice+xDim+1] = redObjectIndex;
                                                change = true;
                                            }
                                            if ((x2 < xDim - 1) && (y2 < yDim - 1) && (z2 < zDim - 1) && (colorObjects[i2+xySlice+xDim+1] == 0) && ((buffer[i2+xySlice+xDim+1] == BRIGHT_RED) || (buffer[i2+xySlice+xDim+1] == RED))) {
                                                colorObjects[i2+xySlice+xDim+1] = redObjectIndex;
                                                change = true;
                                            }
                                        } // if ((colorObjects[i2] > 0) && ((buffer[i2] == BRIGHT_RED) || (buffer[i2] == RED)))
                                    } // for (x2 = xLow; x2 <= xHigh; x2++)
                                } // for (y2 = yLow; y2 <= yHigh; y2++)
                            } // for (z2 = zLow; z2 <= zHigh; z2++)
                        } // while (change)
                    } // if (bigBlueFraction && (colorObjects[i] == 0) && ((buffer[i] == RED) || (buffer[i] == BRIGHT_RED)))
                    else if (bigBlueFraction && (colorObjects[i] == 0) && ((buffer[i] == GREEN) || (buffer[i] == BRIGHT_GREEN))) {
                        change = true;
                        colorObjects[i] = ++greenObjectIndex;
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
                                z2Pos = z2 * xySlice;
                                for (y2 = yLow; y2 <= yHigh; y2++) {
                                    y2Pos = z2Pos + y2 * xDim;
                                    for (x2 = xLow; x2 <= xHigh; x2++) {
                                        i2 = y2Pos + x2;
                                        if ((colorObjects[i2] > 0) && ((buffer[i2] == BRIGHT_GREEN) || (buffer[i2] == GREEN))) {
                                            if ((x2 > 0) && (colorObjects[i2-1] == 0) && ((buffer[i2-1] == BRIGHT_GREEN) || (buffer[i2-1] == GREEN))) {
                                                colorObjects[i2-1] = greenObjectIndex;
                                                change = true;
                                            }
                                            if ((x2 < xDim - 1) && (colorObjects[i2+1] == 0) && ((buffer[i2+1] == BRIGHT_GREEN) || (buffer[i2+1] == GREEN))) {
                                                colorObjects[i2+1] = greenObjectIndex;
                                                change = true;
                                            }
                                            if ((y2 > 0)&& (colorObjects[i2-xDim] == 0) && ((buffer[i2-xDim] == BRIGHT_GREEN) || (buffer[i2-xDim] == GREEN))) {
                                                colorObjects[i2-xDim] = greenObjectIndex;
                                                change = true;
                                            }
                                            if ((y2 < yDim - 1) && (colorObjects[i2+xDim] == 0) && ((buffer[i2+xDim] == BRIGHT_GREEN) || (buffer[i2+xDim] == GREEN))) {
                                                colorObjects[i2+xDim] = greenObjectIndex;
                                                change = true;
                                            }
                                            if ((z2 > 0) && (colorObjects[i2-xySlice] == 0) && ((buffer[i2-xySlice] == BRIGHT_GREEN) || (buffer[i2-xySlice] == GREEN))) {
                                                colorObjects[i2-xySlice] = greenObjectIndex;
                                                change = true;
                                            }
                                            if ((z2 < zDim - 1) && (colorObjects[i2+xySlice] == 0) && ((buffer[i2+xySlice] == BRIGHT_GREEN) || (buffer[i2+xySlice] == GREEN))) {
                                                colorObjects[i2+xySlice] = greenObjectIndex;
                                                change = true;
                                            }
                                            if ((x2 > 0) && (y2 > 0) && (colorObjects[i2-xDim-1] == 0) && ((buffer[i2-xDim-1] == BRIGHT_GREEN) || (buffer[i2-xDim-1] == GREEN))) {
                                                colorObjects[i2-xDim-1] = greenObjectIndex;
                                                change = true;
                                            }
                                            if ((x2 > 0) && (y2 < yDim - 1) && (colorObjects[i2+xDim-1] == 0) && ((buffer[i2+xDim-1] == BRIGHT_GREEN) || (buffer[i2+xDim-1] == GREEN))) {
                                                colorObjects[i2+xDim-1] = greenObjectIndex;
                                                change = true;
                                            }
                                            if ((x2 < xDim-1) && (y2 > 0) && (colorObjects[i2-xDim+1] == 0) && ((buffer[i2-xDim+1] == BRIGHT_GREEN) || (buffer[i2-xDim+1] == GREEN))) {
                                                colorObjects[i2-xDim+1] = greenObjectIndex;
                                                change = true;
                                            }
                                            if ((x2 < xDim - 1) && (y2 < yDim - 1) && (colorObjects[i2+xDim+1] == 0) && ((buffer[i2+xDim+1] == BRIGHT_GREEN) || (buffer[i2+xDim+1] == GREEN))) {
                                                colorObjects[i2+xDim+1] = greenObjectIndex;
                                                change = true;
                                            }
                                            if ((x2 > 0) && (z2 > 0) && (colorObjects[i2-xySlice-1] == 0) && ((buffer[i2-xySlice-1] == BRIGHT_GREEN) || (buffer[i2-xySlice-1] == GREEN))) {
                                                colorObjects[i2-xySlice-1] = greenObjectIndex;
                                                change = true;
                                            }
                                            if ((x2 > 0) && (z2 < zDim - 1) && (colorObjects[i2+xySlice-1] == 0) && ((buffer[i2+xySlice-1] == BRIGHT_GREEN) || (buffer[i2+xySlice-1] == GREEN))) {
                                                colorObjects[i2+xySlice-1] = greenObjectIndex;
                                                change = true;
                                            }
                                            if ((x2 < xDim-1) && (z2 > 0) && (colorObjects[i2-xySlice+1] == 0) && ((buffer[i2-xySlice+1] == BRIGHT_GREEN) || (buffer[i2-xySlice+1] == GREEN))) {
                                                colorObjects[i2-xySlice+1] = greenObjectIndex;
                                                change = true;
                                            }
                                            if ((x2 < xDim - 1) && (z2 < zDim - 1) && (colorObjects[i2+xySlice+1] == 0) && ((buffer[i2+xySlice+1] == BRIGHT_GREEN) || (buffer[i2+xySlice+1] == GREEN))) {
                                                colorObjects[i2+xySlice+1] = greenObjectIndex;
                                                change = true;
                                            }
                                            if ((y2 > 0) && (z2 > 0) && (colorObjects[i2-xySlice-xDim] == 0) && ((buffer[i2-xySlice-xDim] == BRIGHT_GREEN) || (buffer[i2-xySlice-xDim] == GREEN))) {
                                                colorObjects[i2-xySlice-xDim] = greenObjectIndex;
                                                change = true;
                                            }
                                            if ((y2 > 0) && (z2 < zDim - 1) && (colorObjects[i2+xySlice-xDim] == 0) && ((buffer[i2+xySlice-xDim] == BRIGHT_GREEN) || (buffer[i2+xySlice-xDim] == GREEN))) {
                                                colorObjects[i2+xySlice-xDim] = greenObjectIndex;
                                                change = true;
                                            }
                                            if ((y2 < yDim-1) && (z2 > 0) && (colorObjects[i2-xySlice+xDim] == 0) && ((buffer[i2-xySlice+xDim] == BRIGHT_GREEN) || (buffer[i2-xySlice+xDim] == GREEN))) {
                                                colorObjects[i2-xySlice+xDim] = greenObjectIndex;
                                                change = true;
                                            }
                                            if ((y2 < yDim - 1) && (z2 < zDim - 1) && (colorObjects[i2+xySlice+xDim] == 0) && ((buffer[i2+xySlice+xDim] == BRIGHT_GREEN) || (buffer[i2+xySlice+xDim] == GREEN))) {
                                                colorObjects[i2+xySlice+xDim] = greenObjectIndex;
                                                change = true;
                                            }
                                            if ((x2 > 0) && (y2 > 0) && (z2 > 0)&& (colorObjects[i2-xySlice-xDim-1] == 0) && ((buffer[i2-xySlice-xDim-1] == BRIGHT_GREEN) || (buffer[i2-xySlice-xDim-1] == GREEN))) {
                                                colorObjects[i2-xySlice-xDim-1] = greenObjectIndex;
                                                change = true;
                                            }
                                            if ((x2 > 0) && (y2 > 0) && (z2 < zDim - 1) && (colorObjects[i2+xySlice-xDim-1] == 0) && ((buffer[i2+xySlice-xDim-1] == BRIGHT_GREEN) ||(buffer[i2+xySlice-xDim-1] == GREEN))) {
                                                colorObjects[i2+xySlice-xDim-1] = greenObjectIndex;
                                                change = true;
                                            }
                                            if ((x2 > 0) && (y2 < yDim - 1) && (z2 > 0)&& (colorObjects[i2-xySlice+xDim-1] == 0) && ((buffer[i2-xySlice+xDim-1] == BRIGHT_GREEN) || (buffer[i2-xySlice+xDim-1] == GREEN))) {
                                                colorObjects[i2-xySlice+xDim-1] = greenObjectIndex;
                                                change = true;
                                            }
                                            if ((x2 > 0) && (y2 < yDim - 1) && (z2 < zDim - 1) && (colorObjects[i2+xySlice+xDim-1] == 0) && ((buffer[i2+xySlice+xDim-1] == BRIGHT_GREEN) || (buffer[i2+xySlice+xDim-1] == GREEN))) {
                                                colorObjects[i2+xySlice+xDim-1] = greenObjectIndex;
                                                change = true;
                                            }
                                            if ((x2 < xDim-1) && (y2 > 0) && (z2 > 0) && (colorObjects[i2-xySlice-xDim+1] == 0) && ((buffer[i2-xySlice-xDim+1] == BRIGHT_GREEN) || (buffer[i2-xySlice-xDim+1] == GREEN))) {
                                                colorObjects[i2-xySlice-xDim+1] = greenObjectIndex;
                                                change = true;
                                            }
                                            if ((x2 < xDim-1) && (y2 > 0) && (z2 < zDim - 1) && (colorObjects[i2+xySlice-xDim+1] == 0) && ((buffer[i2+xySlice-xDim+1] == BRIGHT_GREEN) || (buffer[i2+xySlice-xDim+1] == GREEN))) {
                                                colorObjects[i2+xySlice-xDim+1] = greenObjectIndex;
                                                change = true;
                                            }
                                            if ((x2 < xDim - 1) && (y2 < yDim - 1) && (z2 > 0) && (colorObjects[i2-xySlice+xDim+1] == 0) && ((buffer[i2-xySlice+xDim+1] == BRIGHT_GREEN) ||(buffer[i2-xySlice+xDim+1] == GREEN))) {
                                                colorObjects[i2-xySlice+xDim+1] = greenObjectIndex;
                                                change = true;
                                            }
                                            if ((x2 < xDim - 1) && (y2 < yDim - 1) && (z2 < zDim - 1) && (colorObjects[i2+xySlice+xDim+1] == 0) && ((buffer[i2+xySlice+xDim+1] == BRIGHT_GREEN) || (buffer[i2+xySlice+xDim+1] == GREEN))) {
                                                colorObjects[i2+xySlice+xDim+1] = greenObjectIndex;
                                                change = true;
                                            }
                                        } // if ((colorObjects[i2] > 0) && ((buffer[i2] == BRIGHT_GREEN) || (buffer[i2] == GREEN))) 
                                    } // for (x2 = xLow; x2 <= xHigh; x2++)
                                } // for (y2 = yLow; y2 <= yHigh; y2++)
                            } // for (z2 = zLow; z2 <= zHigh; z2++)
                        } // while (change)
                    } // else if (bigBlueFraction && (colorObjects[i] == 0) && ((buffer[i] == GREEN) || (buffer[i] == BRIGHT_GREEN)))
                    else if ((colorObjects[i] == 0) && ((buffer[i] == BLUE) || (buffer[i] == BRIGHT_BLUE))) {
                        change = true;
                        colorObjects[i] = ++blueObjectIndex;
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
                                z2Pos = z2 * xySlice;
                                for (y2 = yLow; y2 <= yHigh; y2++) {
                                    y2Pos = z2Pos + y2 * xDim;
                                    for (x2 = xLow; x2 <= xHigh; x2++) {
                                        i2 = y2Pos + x2;
                                        if ((colorObjects[i2] > 0) && ((buffer[i2] == BRIGHT_BLUE) || (buffer[i2] == BLUE))) {
                                            if ((x2 > 0) && (colorObjects[i2-1] == 0) && ((buffer[i2-1] == BRIGHT_BLUE) || (buffer[i2-1] == BLUE))) {
                                                colorObjects[i2-1] = blueObjectIndex;
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
                                            if ((x2 < xDim - 1) && (colorObjects[i2+1] == 0) && ((buffer[i2+1] == BRIGHT_BLUE) || (buffer[i2+1] == BLUE))) {
                                                colorObjects[i2+1] = blueObjectIndex;
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
                                            if ((y2 > 0)&& (colorObjects[i2-xDim] == 0) && ((buffer[i2-xDim] == BRIGHT_BLUE) || (buffer[i2-xDim] == BLUE))) {
                                                colorObjects[i2-xDim] = blueObjectIndex;
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
                                            if ((y2 < yDim - 1) && (colorObjects[i2+xDim] == 0) && ((buffer[i2+xDim] == BRIGHT_BLUE) || (buffer[i2+xDim] == BLUE))) {
                                                colorObjects[i2+xDim] = blueObjectIndex;
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
                                            if ((z2 > 0) && (colorObjects[i2-xySlice] == 0) && ((buffer[i2-xySlice] == BRIGHT_BLUE) || (buffer[i2-xySlice] == BLUE))) {
                                                colorObjects[i2-xySlice] = blueObjectIndex;
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
                                            if ((z2 < zDim - 1) && (colorObjects[i2+xySlice] == 0) && ((buffer[i2+xySlice] == BRIGHT_BLUE) || (buffer[i2+xySlice] == BLUE))) {
                                                colorObjects[i2+xySlice] = blueObjectIndex;
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
                                            if ((x2 > 0) && (y2 > 0) && (colorObjects[i2-xDim-1] == 0) && ((buffer[i2-xDim-1] == BRIGHT_BLUE) || (buffer[i2-xDim-1] == BLUE))) {
                                                colorObjects[i2-xDim-1] = blueObjectIndex;
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
                                            if ((x2 > 0) && (y2 < yDim - 1) && (colorObjects[i2+xDim-1] == 0) && ((buffer[i2+xDim-1] == BRIGHT_BLUE) || (buffer[i2+xDim-1] == BLUE))) {
                                                colorObjects[i2+xDim-1] = blueObjectIndex;
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
                                            if ((x2 < xDim-1) && (y2 > 0) && (colorObjects[i2-xDim+1] == 0) && ((buffer[i2-xDim+1] == BRIGHT_BLUE) || (buffer[i2-xDim+1] == BLUE))) {
                                                colorObjects[i2-xDim+1] = blueObjectIndex;
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
                                            if ((x2 < xDim - 1) && (y2 < yDim - 1) && (colorObjects[i2+xDim+1] == 0) && ((buffer[i2+xDim+1] == BRIGHT_BLUE) || (buffer[i2+xDim+1] == BLUE))) {
                                                colorObjects[i2+xDim+1] = blueObjectIndex;
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
                                            if ((x2 > 0) && (z2 > 0) && (colorObjects[i2-xySlice-1] == 0) && ((buffer[i2-xySlice-1] == BRIGHT_BLUE) || (buffer[i2-xySlice-1] == BLUE))) {
                                                colorObjects[i2-xySlice-1] = blueObjectIndex;
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
                                            if ((x2 > 0) && (z2 < zDim - 1) && (colorObjects[i2+xySlice-1] == 0) && ((buffer[i2+xySlice-1] == BRIGHT_BLUE) || (buffer[i2+xySlice-1] == BLUE))) {
                                                colorObjects[i2+xySlice-1] = blueObjectIndex;
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
                                            if ((x2 < xDim-1) && (z2 > 0) && (colorObjects[i2-xySlice+1] == 0) && ((buffer[i2-xySlice+1] == BRIGHT_BLUE) || (buffer[i2-xySlice+1] == BLUE))) {
                                                colorObjects[i2-xySlice+1] = blueObjectIndex;
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
                                            if ((x2 < xDim - 1) && (z2 < zDim - 1) && (colorObjects[i2+xySlice+1] == 0) && ((buffer[i2+xySlice+1] == BRIGHT_BLUE) || (buffer[i2+xySlice+1] == BLUE))) {
                                                colorObjects[i2+xySlice+1] = blueObjectIndex;
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
                                            if ((y2 > 0) && (z2 > 0) && (colorObjects[i2-xySlice-xDim] == 0) && ((buffer[i2-xySlice-xDim] == BRIGHT_BLUE) || (buffer[i2-xySlice-xDim] == BLUE))) {
                                                colorObjects[i2-xySlice-xDim] = blueObjectIndex;
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
                                            if ((y2 > 0) && (z2 < zDim - 1) && (colorObjects[i2+xySlice-xDim] == 0) && ((buffer[i2+xySlice-xDim] == BRIGHT_BLUE) || (buffer[i2+xySlice-xDim] == BLUE))) {
                                                colorObjects[i2+xySlice-xDim] = blueObjectIndex;
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
                                            if ((y2 < yDim-1) && (z2 > 0) && (colorObjects[i2-xySlice+xDim] == 0) && ((buffer[i2-xySlice+xDim] == BRIGHT_BLUE) || (buffer[i2-xySlice+xDim] == BLUE))) {
                                                colorObjects[i2-xySlice+xDim] = blueObjectIndex;
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
                                            if ((y2 < yDim - 1) && (z2 < zDim - 1) && (colorObjects[i2+xySlice+xDim] == 0) && ((buffer[i2+xySlice+xDim] == BRIGHT_BLUE) || (buffer[i2+xySlice+xDim] == BLUE))) {
                                                colorObjects[i2+xySlice+xDim] = blueObjectIndex;
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
                                            if ((x2 > 0) && (y2 > 0) && (z2 > 0)&& (colorObjects[i2-xySlice-xDim-1] == 0) && ((buffer[i2-xySlice-xDim-1] == BRIGHT_BLUE) || (buffer[i2-xySlice-xDim-1] == BLUE))) {
                                                colorObjects[i2-xySlice-xDim-1] = blueObjectIndex;
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
                                            if ((x2 > 0) && (y2 > 0) && (z2 < zDim - 1) && (colorObjects[i2+xySlice-xDim-1] == 0) && ((buffer[i2+xySlice-xDim-1] == BRIGHT_BLUE) ||(buffer[i2+xySlice-xDim-1] == BLUE))) {
                                                colorObjects[i2+xySlice-xDim-1] = blueObjectIndex;
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
                                            if ((x2 > 0) && (y2 < yDim - 1) && (z2 > 0)&& (colorObjects[i2-xySlice+xDim-1] == 0) && ((buffer[i2-xySlice+xDim-1] == BRIGHT_BLUE) || (buffer[i2-xySlice+xDim-1] == BLUE))) {
                                                colorObjects[i2-xySlice+xDim-1] = blueObjectIndex;
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
                                            if ((x2 > 0) && (y2 < yDim - 1) && (z2 < zDim - 1) && (colorObjects[i2+xySlice+xDim-1] == 0) && ((buffer[i2+xySlice+xDim-1] == BRIGHT_BLUE) || (buffer[i2+xySlice+xDim-1] == BLUE))) {
                                                colorObjects[i2+xySlice+xDim-1] = blueObjectIndex;
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
                                            if ((x2 < xDim-1) && (y2 > 0) && (z2 > 0) && (colorObjects[i2-xySlice-xDim+1] == 0) && ((buffer[i2-xySlice-xDim+1] == BRIGHT_BLUE) || (buffer[i2-xySlice-xDim+1] == BLUE))) {
                                                colorObjects[i2-xySlice-xDim+1] = blueObjectIndex;
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
                                            if ((x2 < xDim-1) && (y2 > 0) && (z2 < zDim - 1) && (colorObjects[i2+xySlice-xDim+1] == 0) && ((buffer[i2+xySlice-xDim+1] == BRIGHT_BLUE) || (buffer[i2+xySlice-xDim+1] == BLUE))) {
                                                colorObjects[i2+xySlice-xDim+1] = blueObjectIndex;
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
                                            if ((x2 < xDim - 1) && (y2 < yDim - 1) && (z2 > 0) && (colorObjects[i2-xySlice+xDim+1] == 0) && ((buffer[i2-xySlice+xDim+1] == BRIGHT_BLUE) ||(buffer[i2-xySlice+xDim+1] == BLUE))) {
                                                colorObjects[i2-xySlice+xDim+1] = blueObjectIndex;
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
                                            if ((x2 < xDim - 1) && (y2 < yDim - 1) && (z2 < zDim - 1) && (colorObjects[i2+xySlice+xDim+1] == 0) && ((buffer[i2+xySlice+xDim+1] == BRIGHT_BLUE) || (buffer[i2+xySlice+xDim+1] == BLUE))) {
                                                colorObjects[i2+xySlice+xDim+1] = blueObjectIndex;
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
    
}
