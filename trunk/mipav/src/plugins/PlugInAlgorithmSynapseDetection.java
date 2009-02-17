import gov.nih.mipav.model.algorithms.*;
import gov.nih.mipav.model.file.*;
import gov.nih.mipav.model.structures.*;

import gov.nih.mipav.view.*;
import gov.nih.mipav.view.renderer.flythroughview.*;

import java.awt.*;

import java.io.*;

import java.text.*;

import java.util.*;


/**
 *
 * @version  February 17, 2009
 * @author   DOCUMENT ME!
 * @see      AlgorithmBase
 *
 *           <p>$Logfile: /mipav/src/plugins/PlugInAlgorithmFociAndStrands.java $ $Revision: 72 $ $Date: 2/06/06 5:50p $
 *           PlugInAlgorithmFociAndStrands is used to:
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
    private int threeBandMinXY;
    private int threeBandMinZ;

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
     */
    public PlugInAlgorithmSynapseDetection(ModelImage srcImg, int redMin, int redMax, int greenMin,
                                         int greenMax, int blueMinXY, int blueMaxXY, 
                                         int blueMinZ, int blueMaxZ, int redIntensity,
                                         int greenIntensity, int blueIntensity) {
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
        // presence of blue above a threshold intensity is the most significant factor.
        for (z = 0; z < zDim; z++) {
            zPos = z * xySlice;
            for (y = 0; y < yDim; y++) {
                yPos = zPos + y * xDim;
                for (x = 0; x < xDim; x++) {
                    pos = yPos + x;
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
                } // for (x = 0; x < xDim; x++)
            } // for (y = 0; y < yDim; y++)
        } // for (z = 0; z < zDim; z++)
        greenBuffer = null;
        blueBuffer = null;
        System.gc();
        blueMask = new BitSet(length);
        threeBandMinXY = redMin + greenMin + blueMinXY;
        threeBandMinZ = redMin + greenMin + blueMinZ;
        
        // Search along all lines parallel to x axis
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
                checkForSynapseXY();
            } // for (y = 0; y < yDim; y++)
        } // for (z = 0; z < zDim; z++)
        System.out.println("Number of synapses found searching parallel to x axis = " + numSynapses);
        Preferences.debug("Number of synapses found searching parallel to x axis = " + numSynapses + "\n");
        previousNumSynapses = numSynapses;
        
        // Search along all lines parallel to y axis
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
                checkForSynapseXY();
            } // for (x = 0; x < xDim; x++)
        } // for (z = 0; z < zDim; z++)
        System.out.println("Number of synapses found searching parallel to y axis = " + (numSynapses - previousNumSynapses));
        Preferences.debug("Number of synapses found searching parallel to y axis = " + (numSynapses - previousNumSynapses) + "\n");
        previousNumSynapses = numSynapses;
        
        // Search all lines parallel to z axis
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
                checkForSynapseZ();
            } // for (x = 0; x < xDim; x++)
        } // for (y = 0; y < yDim; y++)
        System.out.println("Number of synapses found searching parallel to z axis = " + (numSynapses - previousNumSynapses));
        Preferences.debug("Number of synapses found searching parallel to z axis = " + (numSynapses - previousNumSynapses) + "\n");
        previousNumSynapses = numSynapses;
        
        //Search all lines parallel to x = -y.
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
                checkForSynapseXY();
            } // for (yStart = yDim - 2; yStart >= threeBandMinXY-1; yStart--)
        } // for (z = 0; z < zDim; z++)
        System.out.println("Number of synapses found searching parallel to (x = -y) = " + (numSynapses - previousNumSynapses));
        Preferences.debug("Number of synapses found searching parallel to (x = -y) = " + (numSynapses - previousNumSynapses) + "\n");
        previousNumSynapses = numSynapses;
        
        // Search all lines parallel to x = y.
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
                checkForSynapseXY();
            } // for (yStart = yDim - 2; yStart >= threeBandMinXY - 1; yStart--)
        } // for (z = 0; z < zDim; z++)
        System.out.println("Number of synapses found searching parallel to (x = y) = " + (numSynapses - previousNumSynapses));
        Preferences.debug("Number of synapses found searching parallel to (x = y) = " + (numSynapses - previousNumSynapses) + "\n");
        previousNumSynapses = numSynapses;
        
        // Search all lines parallel to x = -z.
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
                checkForSynapseZ();
            } // for (zStart = zDim - 2; zStart >= threeBandMinZ-1; zStart--)
        } // for (y = 0; y < yDim; y++)
        System.out.println("Number of synapses found searching parallel to (x = -z) = " + (numSynapses - previousNumSynapses));
        Preferences.debug("Number of synapses found searching parallel to (x = -z) = " + (numSynapses - previousNumSynapses) + "\n");
        previousNumSynapses = numSynapses;
        
        // Search all lines parallel to x = z.
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
                checkForSynapseZ();
            } // for (zStart = zDim - 2; zStart >= threeBandMinZ - 1; zStart--)
        } // for (y = 0; y < yDim; y++)
        System.out.println("Number of synapses found searching parallel to (x = z) = " + (numSynapses - previousNumSynapses));
        Preferences.debug("Number of synapses found searching parallel to (x = z) = " + (numSynapses - previousNumSynapses) + "\n");
        previousNumSynapses = numSynapses;
        
        // Search all lines parallel to y = -z.
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
                checkForSynapseZ();
            } // for (zStart = zDim - 2; zStart >= threeBandMinZ-1; zStart--)
        } // for (x = 0; x < xDim; x++)
        System.out.println("Number of synapses found searching parallel to (y = -z) = " + (numSynapses - previousNumSynapses));
        Preferences.debug("Number of synapses found searching parallel to (y = -z) = " + (numSynapses - previousNumSynapses) + "\n");
        previousNumSynapses = numSynapses;
        
        // Search all lines parallel to y = z.
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
                checkForSynapseZ();
            } // for (zStart = zDim - 2; zStart >= threeBandMinZ - 1; zStart--)
        } // for (x = 0; x < xDim; x++)
        System.out.println("Number of synapses found searching parallel to (y = z) = " + (numSynapses - previousNumSynapses));
        Preferences.debug("Number of synapses found searching parallel to (y = z) = " + (numSynapses - previousNumSynapses) + "\n");
        previousNumSynapses = numSynapses;
        
        // Search all lines with delX = delY = delZ
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
                checkForSynapseZ();
            } // for (xStart = 0; xStart < xDim; xStart++)
        } // for (yStart = 0; yStart < yDim; yStart++)
        System.out.println("Number of synapses found searching parallel to (delX = delY = delZ) = " + (numSynapses - previousNumSynapses));
        Preferences.debug("Number of synapses found searching parallel to (delX = delY = delZ) = " + (numSynapses - previousNumSynapses) + "\n");
        previousNumSynapses = numSynapses;
        
        // Search all lines with delX = delY = -delZ
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
                checkForSynapseZ();
            } // for (xStart = 0; xStart < xDim; xStart++)
        } // for (yStart = 0; yStart < yDim; yStart++)
        System.out.println("Number of synapses found searching parallel to (delX = delY = -delZ) = " + (numSynapses - previousNumSynapses));
        Preferences.debug("Number of synapses found searching parallel to (delX = delY = -delZ) = " + (numSynapses - previousNumSynapses) + "\n");
        previousNumSynapses = numSynapses;
        
        // Search all lines with delX = -delY = delZ
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
                checkForSynapseZ();
            } // for (xStart = 0; xStart < xDim; xStart++)
        } // for (yStart = 0; yStart < yDim; yStart++)
        System.out.println("Number of synapses found searching parallel to (delX = -delY = delZ) = " + (numSynapses - previousNumSynapses));
        Preferences.debug("Number of synapses found searching parallel to (delX = -delY = delZ) = " + (numSynapses - previousNumSynapses) + "\n");
        previousNumSynapses = numSynapses;
        
        // Search all lines with -delX = delY = delZ
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
                checkForSynapseZ();
            } // for (xStart = 0; xStart < xDim; xStart++)
        } // for (yStart = 0; yStart < yDim; yStart++)
        System.out.println("Number of synapses found searching parallel to (-delX = delY = delZ) = " + (numSynapses - previousNumSynapses));
        Preferences.debug("Number of synapses found searching parallel to (-delX = delY = delZ) = " + (numSynapses - previousNumSynapses) + "\n");
        previousNumSynapses = numSynapses;


        if (threadStopped) {
            finalize();

            return;
        }
        
        srcImage.notifyImageDisplayListeners();
        System.out.println("Total synapses found = " + numSynapses);
        Preferences.debug("Total synapses found = " + numSynapses + "\n");

        fireProgressStateChanged(100);
        time = System.currentTimeMillis() - time;
        Preferences.debug("PlugInAlgorithmSynapseDetection elapsed time in seconds = " + (time/1000.0));
        setCompleted(true);
        
    }
    
    // Finding red, blue, green or green, blue, red with all 3 colors in the appropriate width range
    // means that a synapse has been found within a plane
    private void checkForSynapseXY() {
       VOI newPtVOI;
       if ((threePreviousBrightness && twoPreviousBrightness && onePreviousBrightness &&
           (twoPreviousColor == BLUE) && (twoPreviousWidth >= blueMinXY) && (twoPreviousWidth <= blueMaxXY)) && 
           (((threePreviousColor == RED) && (threePreviousWidth >= redMin) && (threePreviousWidth <= redMax) &&
           (onePreviousColor == GREEN) && (onePreviousWidth >= greenMin) && (onePreviousWidth <= greenMax)) ||
           ((threePreviousColor == GREEN) && (threePreviousWidth >= greenMin) && (threePreviousWidth <= greenMax) &&
           (onePreviousColor == RED) && (onePreviousWidth >= redMin) && (onePreviousWidth <= redMax)))) {
           newPtVOI = new VOI((short) (numSynapses), Integer.toString(numSynapses+1), zDim, VOI.POINT, -1.0f);
           newPtVOI.setColor(Color.white);
           xArr[0] = blueX;
           yArr[0] = blueY;
           zArr[0] = blueZ;
           newPtVOI.importCurve(xArr, yArr, zArr, blueZ);
           ((VOIPoint) (newPtVOI.getCurves()[blueZ].elementAt(0))).setFixed(true);
          ((VOIPoint) (newPtVOI.getCurves()[blueZ].elementAt(0))).setLabel(Integer.toString(numSynapses + 1));
          ((VOIPoint) (newPtVOI.getCurves()[blueZ].elementAt(0))).setName(Integer.toString(numSynapses + 1));
           srcImage.registerVOI(newPtVOI);  
           numSynapses++;
           //zeroBlueBufferRecursion(blueX, blueY, blueZ);
           // If zero BlueBufferIteration is not used, before searches parallel to the x axis are completed,
           // we run out of java heap space with numSynapses = 126,738.
           zeroBlueBufferIteration(blueX, blueY, blueZ);
       }
    }
    
//  Finding red, blue, green or green, blue, red with all 3 colors in the appropriate width range
    // means that a synapse has been found between planes
    private void checkForSynapseZ() {
       VOI newPtVOI;
       if ((threePreviousBrightness && twoPreviousBrightness && onePreviousBrightness &&
           (twoPreviousColor == BLUE) && (twoPreviousWidth >= blueMinZ) && (twoPreviousWidth <= blueMaxZ)) && 
           (((threePreviousColor == RED) && (threePreviousWidth >= redMin) && (threePreviousWidth <= redMax) &&
           (onePreviousColor == GREEN) && (onePreviousWidth >= greenMin) && (onePreviousWidth <= greenMax)) ||
           ((threePreviousColor == GREEN) && (threePreviousWidth >= greenMin) && (threePreviousWidth <= greenMax) &&
           (onePreviousColor == RED) && (onePreviousWidth >= redMin) && (onePreviousWidth <= redMax)))) {
           newPtVOI = new VOI((short) (numSynapses), Integer.toString(numSynapses+1), zDim, VOI.POINT, -1.0f);
           newPtVOI.setColor(Color.white);
           xArr[0] = blueX;
           yArr[0] = blueY;
           zArr[0] = blueZ;
           newPtVOI.importCurve(xArr, yArr, zArr, blueZ);
           ((VOIPoint) (newPtVOI.getCurves()[blueZ].elementAt(0))).setFixed(true);
          ((VOIPoint) (newPtVOI.getCurves()[blueZ].elementAt(0))).setLabel(Integer.toString(numSynapses + 1));
          ((VOIPoint) (newPtVOI.getCurves()[blueZ].elementAt(0))).setName(Integer.toString(numSynapses + 1));
           srcImage.registerVOI(newPtVOI);  
           numSynapses++;
           //zeroBlueBufferRecursion(blueX, blueY, blueZ);
           // If zero BlueBufferIteration is not used, before searches parallel to the x axis are completed,
           // we run out of java heap space with numSynapses = 126,738.
           zeroBlueBufferIteration(blueX, blueY, blueZ);
       }
    }
    
    // Change the BLUE of a detected synapse to NONE so it is not detected more than once
    // Change all 27 neighbor connected BLUE to the original BLUE to none inside a cube
    // of width 2 * blueMax around the originally specified BLUE pixel, so that only 1
    // BLUE pixel from each synapse can trigger a synapse detection.
    private void zeroBlueBufferIteration(int xStart, int yStart, int zStart) {
        boolean change = true;
        int x;
        int y;
        int z;
        int yPos;
        int zPos;
        int i = xStart + xDim * yStart + xySlice * zStart;
        int del = -1;
        int zLow;
        int zHigh;
        int yLow;
        int yHigh;
        int xLow;
        int xHigh;
        buffer[i] = NONE;
        blueMask.set(i);
        int delXY;
        int delZ;
        while (change && (del <= blueMax - 2)) {
            change = false;
            del++;
            delXY = Math.min(blueMaxXY-1, del);
            delZ = Math.min(blueMaxZ-1, del);
            xLow = Math.max(0, xStart-delXY);
            xHigh = Math.min(xDim-1, xStart + delXY);
            yLow = Math.max(0, yStart-delXY);
            yHigh = Math.min(yDim-1, yStart + delXY);
            zLow = Math.max(0, zStart-delZ);
            zHigh = Math.min(zDim-1, zStart + delZ);
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
                                change = true;
                            }
                            if ((x < xDim - 1) && (!blueMask.get(i+1)) && ((buffer[i+1] == BRIGHT_BLUE) || (buffer[i+1] == BLUE))) {
                                buffer[i+1] = NONE;
                                blueMask.set(i+1);
                                change = true;
                            }
                            if ((y > 0)&& (!blueMask.get(i-xDim)) && ((buffer[i-xDim] == BRIGHT_BLUE) || (buffer[i-xDim] == BLUE))) {
                                buffer[i-xDim] = NONE;
                                blueMask.set(i-xDim);
                                change = true;
                            }
                            if ((y < yDim - 1) && (!blueMask.get(i+xDim)) && ((buffer[i+xDim] == BRIGHT_BLUE) || (buffer[i+xDim] == BLUE))) {
                                buffer[i+xDim] = NONE;
                                blueMask.set(i+xDim);
                                change = true;
                            }
                            if ((z > 0) && (!blueMask.get(i-xySlice)) && ((buffer[i-xySlice] == BRIGHT_BLUE) || (buffer[i-xySlice] == BLUE))) {
                                buffer[i-xySlice] = NONE;
                                blueMask.set(i-xySlice);
                                change = true;
                            }
                            if ((z < zDim - 1) && (!blueMask.get(i+xySlice)) && ((buffer[i+xySlice] == BRIGHT_BLUE) || (buffer[i+xySlice] == BLUE))) {
                                buffer[i+xySlice] = NONE;
                                blueMask.set(i+xySlice);
                                change = true;
                            }
                            if ((x > 0) && (y > 0) && (!blueMask.get(i-xDim-1)) && ((buffer[i-xDim-1] == BRIGHT_BLUE) || (buffer[i-xDim-1] == BLUE))) {
                                buffer[i-xDim-1] = NONE;
                                blueMask.set(i-xDim-1);
                                change = true;
                            }
                            if ((x > 0) && (y < yDim - 1) && (!blueMask.get(i+xDim-1)) && ((buffer[i+xDim-1] == BRIGHT_BLUE) || (buffer[i+xDim-1] == BLUE))) {
                                buffer[i+xDim-1] = NONE;
                                blueMask.set(i+xDim-1);
                                change = true;
                            }
                            if ((x < xDim-1) && (y > 0) && (!blueMask.get(i-xDim+1)) && ((buffer[i-xDim+1] == BRIGHT_BLUE) || (buffer[i-xDim+1] == BLUE))) {
                                buffer[i-xDim+1] = NONE;
                                blueMask.set(i-xDim+1);
                                change = true;
                            }
                            if ((x < xDim - 1) && (y < yDim - 1) && (!blueMask.get(i+xDim+1)) && ((buffer[i+xDim+1] == BRIGHT_BLUE) || (buffer[i+xDim+1] == BLUE))) {
                                buffer[i+xDim+1] = NONE;
                                blueMask.set(i+xDim+1);
                                change = true;
                            }
                            if ((x > 0) && (z > 0) && (!blueMask.get(i-xySlice-1)) && ((buffer[i-xySlice-1] == BRIGHT_BLUE) || (buffer[i-xySlice-1] == BLUE))) {
                                buffer[i-xySlice-1] = NONE;
                                blueMask.set(i-xySlice-1);
                                change = true;
                            }
                            if ((x > 0) && (z < zDim - 1) && (!blueMask.get(i+xySlice-1)) && ((buffer[i+xySlice-1] == BRIGHT_BLUE) || (buffer[i+xySlice-1] == BLUE))) {
                                buffer[i+xySlice-1] = NONE;
                                blueMask.set(i+xySlice-1);
                                change = true;
                            }
                            if ((x < xDim-1) && (z > 0) && (!blueMask.get(i-xySlice+1)) && ((buffer[i-xySlice+1] == BRIGHT_BLUE) || (buffer[i-xySlice+1] == BLUE))) {
                                buffer[i-xySlice+1] = NONE;
                                blueMask.set(i-xySlice+1);
                                change = true;
                            }
                            if ((x < xDim - 1) && (z < zDim - 1) && (!blueMask.get(i+xySlice+1)) && ((buffer[i+xySlice+1] == BRIGHT_BLUE) || (buffer[i+xySlice+1] == BLUE))) {
                                buffer[i+xySlice+1] = NONE;
                                blueMask.set(i+xySlice+1);
                                change = true;
                            }
                            if ((y > 0) && (z > 0) && (!blueMask.get(i-xySlice-xDim)) && ((buffer[i-xySlice-xDim] == BRIGHT_BLUE) || (buffer[i-xySlice-xDim] == BLUE))) {
                                buffer[i-xySlice-xDim] = NONE;
                                blueMask.set(i-xySlice-xDim);
                                change = true;
                            }
                            if ((y > 0) && (z < zDim - 1) && (!blueMask.get(i+xySlice-xDim)) && ((buffer[i+xySlice-xDim] == BRIGHT_BLUE) || (buffer[i+xySlice-xDim] == BLUE))) {
                                buffer[i+xySlice-xDim] = NONE;
                                blueMask.set(i+xySlice-xDim);
                                change = true;
                            }
                            if ((y < yDim-1) && (z > 0) && (!blueMask.get(i-xySlice+xDim)) && ((buffer[i-xySlice+xDim] == BRIGHT_BLUE) || (buffer[i-xySlice+xDim] == BLUE))) {
                                buffer[i-xySlice+xDim] = NONE;
                                blueMask.set(i-xySlice+xDim);
                                change = true;
                            }
                            if ((y < yDim - 1) && (z < zDim - 1) && (!blueMask.get(i+xySlice+xDim)) && ((buffer[i+xySlice+xDim] == BRIGHT_BLUE) || (buffer[i+xySlice+xDim] == BLUE))) {
                                buffer[i+xySlice+xDim] = NONE;
                                blueMask.set(i+xySlice+xDim);
                                change = true;
                            }
                            if ((x > 0) && (y > 0) && (z > 0)&& (!blueMask.get(i-xySlice-xDim-1)) && ((buffer[i-xySlice-xDim-1] == BRIGHT_BLUE) || (buffer[i-xySlice-xDim-1] == BLUE))) {
                                buffer[i-xySlice-xDim-1] = NONE;
                                blueMask.set(i-xySlice-xDim-1);
                                change = true;
                            }
                            if ((x > 0) && (y > 0) && (z < zDim - 1) && (!blueMask.get(i+xySlice-xDim-1)) && ((buffer[i+xySlice-xDim-1] == BRIGHT_BLUE) ||(buffer[i+xySlice-xDim-1] == BLUE))) {
                                buffer[i+xySlice-xDim-1] = NONE;
                                blueMask.set(i+xySlice-xDim-1);
                                change = true;
                            }
                            if ((x > 0) && (y < yDim - 1) && (z > 0)&& (!blueMask.get(i-xySlice+xDim-1)) && ((buffer[i-xySlice+xDim-1] == BRIGHT_BLUE) || (buffer[i-xySlice+xDim-1] == BLUE))) {
                                buffer[i-xySlice+xDim-1] = NONE;
                                blueMask.set(i-xySlice+xDim-1);
                                change = true;
                            }
                            if ((x > 0) && (y < yDim - 1) && (z < zDim - 1) && (!blueMask.get(i+xySlice+xDim-1)) && ((buffer[i+xySlice+xDim-1] == BRIGHT_BLUE) || (buffer[i+xySlice+xDim-1] == BLUE))) {
                                buffer[i+xySlice+xDim-1] = NONE;
                                blueMask.set(i+xySlice+xDim-1);
                                change = true;
                            }
                            if ((x < xDim-1) && (y > 0) && (z > 0) && (!blueMask.get(i-xySlice-xDim+1)) && ((buffer[i-xySlice-xDim+1] == BRIGHT_BLUE) || (buffer[i-xySlice-xDim+1] == BLUE))) {
                                buffer[i-xySlice-xDim+1] = NONE;
                                blueMask.set(i-xySlice-xDim+1);
                                change = true;
                            }
                            if ((x < xDim-1) && (y > 0) && (z < zDim - 1) && (!blueMask.get(i+xySlice-xDim+1)) && ((buffer[i+xySlice-xDim+1] == BRIGHT_BLUE) || (buffer[i+xySlice-xDim+1] == BLUE))) {
                                buffer[i+xySlice-xDim+1] = NONE;
                                blueMask.set(i+xySlice-xDim+1);
                                change = true;
                            }
                            if ((x < xDim - 1) && (y < yDim - 1) && (z > 0) && (!blueMask.get(i-xySlice+xDim+1)) && ((buffer[i-xySlice+xDim+1] == BRIGHT_BLUE) ||(buffer[i-xySlice+xDim+1] == BLUE))) {
                                buffer[i-xySlice+xDim+1] = NONE;
                                blueMask.set(i-xySlice+xDim+1);
                                change = true;
                            }
                            if ((x < xDim - 1) && (y < yDim - 1) && (z < zDim - 1) && (!blueMask.get(i+xySlice+xDim+1)) && ((buffer[i+xySlice+xDim+1] == BRIGHT_BLUE) || (buffer[i+xySlice+xDim+1] == BLUE))) {
                                buffer[i+xySlice+xDim+1] = NONE;
                                blueMask.set(i+xySlice+xDim+1);
                                change = true;
                            }
                            blueMask.clear(i);
                        } // if (blueMask.get(i))
                    } // for (x = xLow; x <= xHigh; x++)
                } // for (y = yLow; y <= yHigh; y++)
            } // for (z = zLow; z <= zHigh; z++)
        } // while (change  && (del <= (blueMax - 2))
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
