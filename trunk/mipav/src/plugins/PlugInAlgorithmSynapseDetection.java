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
 * @version  February 10, 2009
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
    
    private int blueMin = 1;
    
    private int blueMax = 20;
    
    /* Minimum margin by which red exceeds green, blue */
    private int redMargin = 1;
    
    private int greenMargin = 1;
    
    private int blueMargin = 1;
    
    private int threePreviousColor;
    private int twoPreviousColor;
    private int onePreviousColor;
    private int presentColor;
    private int threePreviousWidth;
    private int twoPreviousWidth;
    private int onePreviousWidth;
    private int presentWidth;
    private final byte NONE = 0;
    private final byte RED = 1;
    private final byte GREEN = 2;
    private final byte BLUE = 3;
    private int blueX;
    private int blueY;
    private int blueZ;
    private int numSynapses = 0;
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

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Constructor.
     *
     * @param  srcImg         Source image model.
     * @param  redMin         Minimum number of red pixels along line
     * @param  redMax         Maximum number of red pixels along line
     * @param  greenMin       Minimum number of green pixels along line
     * @param  greenMax       Maximum number of green pixels along line
     * @param  blueMin        Minimum number of blue pixels along line
     * @param  blueMax        Maximum number of blue pixels along line
     * @param  redMargin      margin by which red exceeds green, blue
     * @param  greenMargin    margin by which green exceed red, blue
     * @param  blueMargin     margin by which blue exceeds red, green
     */
    public PlugInAlgorithmSynapseDetection(ModelImage srcImg, int redMin, int redMax, int greenMin,
                                         int greenMax, int blueMin, int blueMax, int redMargin,
                                         int greenMargin, int blueMargin) {
        super(null, srcImg);
        this.redMin = redMin;
        this.redMax = redMax;
        this.greenMin = greenMin;
        this.greenMax = greenMax;
        this.blueMin = blueMin;
        this.blueMax = blueMax;
        this.redMargin = redMargin;
        this.greenMargin = greenMargin;
        this.blueMargin = blueMargin;
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
        
        int x;
        int y;
        int z;
        int pos;
        int red;
        int green;
        int blue;
        int blueStartX;
        int blueStartY;
        int blueStartZ;
        
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
        
        for (z = 0; z < zDim; z++) {
            zPos = z * xySlice;
            for (y = 0; y < yDim; y++) {
                yPos = zPos + y * xDim;
                for (x = 0; x < xDim; x++) {
                    pos = yPos + x;
                    red = buffer[pos] & 0xff;
                    green = greenBuffer[pos] & 0xff;
                    blue = blueBuffer[pos] & 0xff;
                    if (((red - green) >= redMargin) && ((red - blue) >= redMargin)) {
                        buffer[pos] = RED;
                    }
                    else if (((green - red) >= greenMargin) && ((green - blue) >= greenMargin)) {
                        buffer[pos] = GREEN;
                    }
                    else if (((blue - red) >= blueMargin) && ((blue - green) >= blueMargin)) {
                        buffer[pos] = BLUE;
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
        
        // Line parallel to x axis
        for (z = 0; z < zDim; z++) {
            zPos = z * xySlice;
            for (y = 0; y < yDim; y++) {
                yPos = zPos + y * xDim;
                threePreviousColor = NONE;
                twoPreviousColor = NONE;
                onePreviousColor = NONE;
                presentColor = NONE;
                threePreviousWidth = 0;
                twoPreviousWidth = 0;
                onePreviousWidth = 0;
                presentWidth = 0;
                blueStartX = 0;
                blueStartY = y;
                blueStartZ = z;
                for (x = 0; x < xDim; x++) {
                   pos = yPos + x;  
                   if (buffer[pos] == RED) {
                       if (presentColor == RED) {
                           presentWidth++;
                       }
                       else {
                           threePreviousColor = twoPreviousColor;
                           threePreviousWidth = twoPreviousWidth;
                           twoPreviousColor = onePreviousColor;
                           twoPreviousWidth = onePreviousWidth;
                           onePreviousColor = presentColor;
                           onePreviousWidth = presentWidth;
                           presentColor = RED;
                           presentWidth = 1;
                           checkForSynapse();
                       }
                   } // if (buffer[pos] == RED)
                   else if (buffer[pos] == GREEN) {
                       if (presentColor == GREEN) {
                           presentWidth++;
                       }
                       else {
                           threePreviousColor = twoPreviousColor;
                           threePreviousWidth = twoPreviousWidth;
                           twoPreviousColor = onePreviousColor;
                           twoPreviousWidth = onePreviousWidth;
                           onePreviousColor = presentColor;
                           onePreviousWidth = presentWidth;
                           presentColor = GREEN;
                           presentWidth = 1;
                           checkForSynapse();
                       }    
                   } // else if (buffer[pos] == GREEN)
                   else if (buffer[pos] == BLUE) {
                       if (presentColor == BLUE) {
                           presentWidth++;
                           blueX = (blueStartX + x) >> 2;
                           blueY = (blueStartY + y) >> 2;
                           blueZ = (blueStartZ + z) >> 2;
                       }
                       else {
                           threePreviousColor = twoPreviousColor;
                           threePreviousWidth = twoPreviousWidth;
                           twoPreviousColor = onePreviousColor;
                           twoPreviousWidth = onePreviousWidth;
                           onePreviousColor = presentColor;
                           onePreviousWidth = presentWidth;
                           presentColor = BLUE;
                           presentWidth = 1;
                           checkForSynapse();
                           blueStartX = x;
                           blueStartY = y;
                           blueStartZ = z;
                           blueX = x;
                           blueY = y;
                           blueZ = z;
                       }        
                   } // else if (buffer[pos] == BLUE)
                   else { // not RED, GREEN, or BLUE
                       if (presentColor == NONE) {
                           presentWidth++;
                       }
                       else {
                           threePreviousColor = twoPreviousColor;
                           threePreviousWidth = twoPreviousWidth;
                           twoPreviousColor = onePreviousColor;
                           twoPreviousWidth = onePreviousWidth;
                           onePreviousColor = presentColor;
                           onePreviousWidth = presentWidth;
                           presentColor = NONE;
                           presentWidth = 1;
                           checkForSynapse();
                       }            
                   } // else not RED, GREEN, or BLUE
                } // for (x = 0; x < xDim; x++)
                threePreviousColor = twoPreviousColor;
                threePreviousWidth = twoPreviousWidth;
                twoPreviousColor = onePreviousColor;
                twoPreviousWidth = onePreviousWidth;
                onePreviousColor = presentColor;
                onePreviousWidth = presentWidth;
                checkForSynapse();
            } // for (y = 0; y < yDim; y++)
        } // for (z = 0; z < zDim; z++)

        if (threadStopped) {
            finalize();

            return;
        }

        fireProgressStateChanged(100);
        time = System.currentTimeMillis() - time;
        Preferences.debug("PlugInAlgorithmSynapseDetection elapsed time in seconds = " + (time/1000.0));
        setCompleted(true);
        
    }
    
    private void checkForSynapse() {
       VOI newPtVOI;
       if (((twoPreviousColor == BLUE) && (twoPreviousWidth >= blueMin) && (twoPreviousWidth <= blueMax)) && 
           (((threePreviousColor == RED) && (threePreviousWidth >= redMin) && (threePreviousWidth <= redMax) &&
           (onePreviousColor == GREEN) && (onePreviousWidth >= greenMin) && (onePreviousWidth <= greenMax)) ||
           ((threePreviousColor == GREEN) && (threePreviousWidth >= greenMin) && (threePreviousWidth <= greenMax)) &&
           (onePreviousColor == RED) && (onePreviousWidth >= redMin) && (onePreviousWidth <= redMax))) {
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
           System.out.println("numSynapses = " + numSynapses);
           //zeroBlueBufferRecursion(blueX, blueY, blueZ);
           zeroBlueBufferIteration(blueX, blueY, blueZ);
       }
    }
    
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
        while (change) {
            change = false;
            del++;
            xLow = Math.max(0, xStart-del);
            xHigh = Math.min(xDim-1, xStart + del);
            yLow = Math.max(0, yStart-del);
            yHigh = Math.min(yDim-1, yStart + del);
            zLow = Math.max(0, zStart-del);
            zHigh = Math.min(zDim-1, zStart + del);
            for (z = zLow; z <= zHigh; z++) {
                zPos = z * xySlice;
                for (y = yLow; y <= yHigh; y++) {
                    yPos = zPos + y * xDim;
                    for (x = xLow; x <= xHigh; x++) {
                        i = yPos + x; 
                        if (blueMask.get(i)) {
                            if ((x > 0) && (!blueMask.get(i-1)) && (buffer[i-1] == BLUE)) {  
                                buffer[i-1] = NONE;
                                blueMask.set(i-1);
                                change = true;
                            }
                            if ((x < xDim - 1) && (!blueMask.get(i+1)) && (buffer[i+1] == BLUE)) {
                                buffer[i+1] = NONE;
                                blueMask.set(i+1);
                                change = true;
                            }
                            if ((y > 0)&& (!blueMask.get(i-xDim)) && (buffer[i-xDim] == BLUE)) {
                                buffer[i-xDim] = NONE;
                                blueMask.set(i-xDim);
                                change = true;
                            }
                            if ((y < yDim - 1) && (!blueMask.get(i+xDim)) && (buffer[i+xDim] == BLUE)) {
                                buffer[i+xDim] = NONE;
                                blueMask.set(i+xDim);
                                change = true;
                            }
                            if ((z > 0) && (!blueMask.get(i-xySlice)) && (buffer[i-xySlice] == BLUE)) {
                                buffer[i-xySlice] = NONE;
                                blueMask.set(i-xySlice);
                                change = true;
                            }
                            if ((z < zDim - 1) && (!blueMask.get(i+xySlice)) && (buffer[i+xySlice] == BLUE)) {
                                buffer[i+xySlice] = NONE;
                                blueMask.set(i+xySlice);
                                change = true;
                            }
                            if ((x > 0) && (y > 0) && (!blueMask.get(i-xDim-1)) && (buffer[i-xDim-1] == BLUE)) {
                                buffer[i-xDim-1] = NONE;
                                blueMask.set(i-xDim-1);
                                change = true;
                            }
                            if ((x > 0) && (y < yDim - 1) && (!blueMask.get(i+xDim-1)) && (buffer[i+xDim-1] == BLUE)) {
                                buffer[i+xDim-1] = NONE;
                                blueMask.set(i+xDim-1);
                                change = true;
                            }
                            if ((x < xDim-1) && (y > 0) && (!blueMask.get(i-xDim+1)) && (buffer[i-xDim+1] == BLUE)) {
                                buffer[i-xDim+1] = NONE;
                                blueMask.set(i-xDim+1);
                                change = true;
                            }
                            if ((x < xDim - 1) && (y < yDim - 1) && (!blueMask.get(i+xDim+1)) && (buffer[i+xDim+1] == BLUE)) {
                                buffer[i+xDim+1] = NONE;
                                blueMask.set(i+xDim+1);
                                change = true;
                            }
                            if ((x > 0) && (z > 0) && (!blueMask.get(i-xySlice-1)) && (buffer[i-xySlice-1] == BLUE)) {
                                buffer[i-xySlice-1] = NONE;
                                blueMask.set(i-xySlice-1);
                                change = true;
                            }
                            if ((x > 0) && (z < zDim - 1) && (!blueMask.get(i+xySlice-1)) && (buffer[i+xySlice-1] == BLUE)) {
                                buffer[i+xySlice-1] = NONE;
                                blueMask.set(i+xySlice-1);
                                change = true;
                            }
                            if ((x < xDim-1) && (z > 0) && (!blueMask.get(i-xySlice+1)) && (buffer[i-xySlice+1] == BLUE)) {
                                buffer[i-xySlice+1] = NONE;
                                blueMask.set(i-xySlice+1);
                                change = true;
                            }
                            if ((x < xDim - 1) && (z < zDim - 1) && (!blueMask.get(i+xySlice+1)) && (buffer[i+xySlice+1] == BLUE)) {
                                buffer[i+xySlice+1] = NONE;
                                blueMask.set(i+xySlice+1);
                                change = true;
                            }
                            if ((y > 0) && (z > 0) && (!blueMask.get(i-xySlice-xDim)) && (buffer[i-xySlice-xDim] == BLUE)) {
                                buffer[i-xySlice-xDim] = NONE;
                                blueMask.set(i-xySlice-xDim);
                                change = true;
                            }
                            if ((y > 0) && (z < zDim - 1) && (!blueMask.get(i+xySlice-xDim)) && (buffer[i+xySlice-xDim] == BLUE)) {
                                buffer[i+xySlice-xDim] = NONE;
                                blueMask.set(i+xySlice-xDim);
                                change = true;
                            }
                            if ((y < yDim-1) && (z > 0) && (!blueMask.get(i-xySlice+xDim)) && (buffer[i-xySlice+xDim] == BLUE)) {
                                buffer[i-xySlice+xDim] = NONE;
                                blueMask.set(i-xySlice+xDim);
                                change = true;
                            }
                            if ((y < yDim - 1) && (z < zDim - 1) && (!blueMask.get(i+xySlice+xDim)) && (buffer[i+xySlice+xDim] == BLUE)) {
                                buffer[i+xySlice+xDim] = NONE;
                                blueMask.set(i+xySlice+xDim);
                                change = true;
                            }
                            if ((x > 0) && (y > 0) && (z > 0)&& (!blueMask.get(i-xySlice-xDim-1)) && (buffer[i-xySlice-xDim-1] == BLUE)) {
                                buffer[i-xySlice-xDim-1] = NONE;
                                blueMask.set(i-xySlice-xDim-1);
                                change = true;
                            }
                            if ((x > 0) && (y > 0) && (z < zDim - 1) && (!blueMask.get(i+xySlice-xDim-1)) && (buffer[i+xySlice-xDim-1] == BLUE)) {
                                buffer[i+xySlice-xDim-1] = NONE;
                                blueMask.set(i+xySlice-xDim-1);
                                change = true;
                            }
                            if ((x > 0) && (y < yDim - 1) && (z > 0)&& (!blueMask.get(i-xySlice+xDim-1)) && (buffer[i-xySlice+xDim-1] == BLUE)) {
                                buffer[i-xySlice+xDim-1] = NONE;
                                blueMask.set(i-xySlice+xDim-1);
                                change = true;
                            }
                            if ((x > 0) && (y < yDim - 1) && (z < zDim - 1) && (!blueMask.get(i+xySlice+xDim-1)) && (buffer[i+xySlice+xDim-1] == BLUE)) {
                                buffer[i+xySlice+xDim-1] = NONE;
                                blueMask.set(i+xySlice+xDim-1);
                                change = true;
                            }
                            if ((x < xDim-1) && (y > 0) && (z > 0) && (!blueMask.get(i-xySlice-xDim+1)) && (buffer[i-xySlice-xDim+1] == BLUE)) {
                                buffer[i-xySlice-xDim+1] = NONE;
                                blueMask.set(i-xySlice-xDim+1);
                                change = true;
                            }
                            if ((x < xDim-1) && (y > 0) && (z < zDim - 1) && (!blueMask.get(i+xySlice-xDim+1)) && (buffer[i+xySlice-xDim+1] == BLUE)) {
                                buffer[i+xySlice-xDim+1] = NONE;
                                blueMask.set(i+xySlice-xDim+1);
                                change = true;
                            }
                            if ((x < xDim - 1) && (y < yDim - 1) && (z > 0) && (!blueMask.get(i-xySlice+xDim+1)) && (buffer[i-xySlice+xDim+1] == BLUE)) {
                                buffer[i-xySlice+xDim+1] = NONE;
                                blueMask.set(i-xySlice+xDim+1);
                                change = true;
                            }
                            if ((x < xDim - 1) && (y < yDim - 1) && (z < zDim - 1) && (!blueMask.get(i+xySlice+xDim+1)) && (buffer[i+xySlice+xDim+1] == BLUE)) {
                                buffer[i+xySlice+xDim+1] = NONE;
                                blueMask.set(i+xySlice+xDim+1);
                                change = true;
                            }
                        } // if (blueMask.get(i))
                    } // for (x = xLow; x <= xHigh; x++)
                } // for (y = yLow; y <= yHigh; y++)
            } // for (z = zLow; z <= zHigh; z++)
        } // while (change)
        for (i = 0; i < length; i++) {
            blueMask.clear(i);
        }
    }
    
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
