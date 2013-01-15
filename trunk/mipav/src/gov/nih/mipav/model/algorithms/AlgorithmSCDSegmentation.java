package gov.nih.mipav.model.algorithms;

import gov.nih.mipav.model.structures.ModelImage;


import gov.nih.mipav.view.MipavUtil;
import gov.nih.mipav.view.Preferences;
import gov.nih.mipav.view.ViewJFrameImage;

import java.awt.Color;
import java.io.IOException;


/**
 * 
 * @author William Gandler
 * This program is an implementation of the algorithm described in "Color image segmentation of subset connectedness
 * and color homogeneity properties" by Ludovic Macaire, Nicolas Vandenbroucke, and Jack-Gerard Postaire, Computer 
 * Vision and Image Understanding, Vol. 102, 2006, pp. 105-116.
 * 
 * This algorithm segments 2D color images into a user specified number of classes.
 *
 */
public class AlgorithmSCDSegmentation extends AlgorithmBase  {
    
//~ Instance fields ------------------------------------------------------------------------------------------------
    
    
    // Must be an odd number, 1 or greater.
    private int initialSideLength;
    
    private int numClasses;
    
  //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     
     */
    public AlgorithmSCDSegmentation(ModelImage destImg, ModelImage srcImg, int initialSideLength, int numClasses) {
        super(destImg, srcImg);
        this.initialSideLength = initialSideLength;
        this.numClasses = numClasses;
    }
    
  //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * Prepares this class for destruction.
     */
    public void finalize() {
        
        super.finalize();
    }

    

    /**
     * Starts the algorithm.
     */
    public void runAlgorithm() {
        int i;
        int j;
        int xDim;
        int yDim;
        int x;
        int y;
        float red;
        float green;
        float blue;
        int index;
        int sliceSize;
        short redBuffer[];
        short greenBuffer[];
        short blueBuffer[];
        int initialSideHalf;
        int lowRed;
        int highRed;
        int lowGreen;
        int highGreen;
        int lowBlue;
        int highBlue;
        int numNeighbors;
        int ys;
        int xs;
        int neighborIndex;
        int neighborRed;
        int neighborGreen;
        int neighborBlue;
        int connectedNeighbors;
        int minRed = (int)Math.round(srcImage.getMinR());
        int maxRed = (int)Math.round(srcImage.getMaxR());
        int minGreen = (int)Math.round(srcImage.getMinG());
        int maxGreen = (int)Math.round(srcImage.getMaxG());
        int minBlue = (int)Math.round(srcImage.getMinB());
        int maxBlue = (int)Math.round(srcImage.getMaxB());
        int r;
        int g;
        int b;
        
        xDim = srcImage.getExtents()[0];
        yDim = srcImage.getExtents()[1];
        sliceSize = xDim * yDim;
        redBuffer = new short[sliceSize];
        greenBuffer = new short[sliceSize];
        blueBuffer = new short[sliceSize];
        initialSideHalf = (initialSideLength - 1)/2;
        int lowRedBound = minRed + initialSideHalf;
        int highRedBound = maxRed - initialSideHalf;
        int lowGreenBound = minGreen + initialSideHalf;
        int highGreenBound = maxGreen - initialSideHalf;
        int lowBlueBound = minBlue + initialSideHalf;
        int highBlueBound = maxBlue - initialSideHalf;
        int numSet;
        
        try {
            srcImage.exportRGBData(1, 0, sliceSize, redBuffer);
        }
        catch(IOException e) {
            MipavUtil.displayError("IOException " + e + " on srcImage.exportRGBData(1, 0, slice, redBuffer)");
            setCompleted(false);
            return;
        }
        
        try {
            srcImage.exportRGBData(2, 0, sliceSize, greenBuffer);
        }
        catch(IOException e) {
            MipavUtil.displayError("IOException " + e + " on srcImage.exportRGBData(2, 0, slice, greenBuffer)");
            setCompleted(false);
            return;
        }
        
        try {
            srcImage.exportRGBData(3, 0, sliceSize, blueBuffer);
        }
        catch(IOException e) {
            MipavUtil.displayError("IOException " + e + " on srcImage.exportRGBData(3, 0, slice, blueBuffer)");
            setCompleted(false);
            return;
        }
        
        for (r = lowRedBound; r <= highRedBound; r++) {
            lowRed = r - initialSideHalf;
            highRed = r + initialSideHalf;
            for (g = lowGreenBound; g <= highGreenBound; g++) {
                lowGreen = g - initialSideHalf;
                highGreen = g + initialSideHalf;
                for (b = lowBlueBound; b <= highBlueBound; b++) {
                    lowBlue = b - initialSideHalf;
                    highBlue = b + initialSideHalf;
                    for (y = 0; y < yDim; y++) {
                        for (x = 0; x < xDim; x++) {
                            index = x + y * xDim;
                            numSet = 0;
                            red = redBuffer[index];
                            if ((red >= lowRed) && (red <= highRed)) {
                                green = greenBuffer[index];
                                if ((green >= lowGreen) && (green <= highGreen)) {
                                    blue = blueBuffer[index];
                                    if ((blue >= lowBlue) && (blue <= highBlue)) {
                                        numSet++;
                                        connectedNeighbors = 0;
                                        for (ys = Math.max(y-1,0); ys <= Math.min(y+1, yDim-1); ys++) {
                                            for (xs = Math.max(x-1,0); xs <= Math.min(x+1, xDim-1); xs++) {
                                                if ((ys != y) || (xs != x)) {
                                                    neighborIndex = xs + ys * xDim;
                                                    neighborRed = redBuffer[neighborIndex];
                                                    if ((neighborRed >= lowRed) && (neighborRed <= highRed)) {
                                                        neighborGreen = greenBuffer[neighborIndex];
                                                        if ((neighborGreen >= lowGreen) && (neighborGreen <= highGreen)) {
                                                            neighborBlue = blueBuffer[neighborIndex];
                                                            if ((neighborBlue >= lowBlue) && (neighborBlue <= highBlue)) {
                                                                connectedNeighbors++;
                                                            } // if ((neighborBlue >= lowBlue) && (neighborBlue <= highBlue))
                                                        } // if ((neighborGreen >= lowGreen) && (neighborGreen <= highGreen))
                                                    } // if ((neighborRed >= lowRed) && (neighborRed <= highRed))
                                                } // if ((ys != y) || (xs != x))
                                            } // for (xs = Math.max(x-1,0); xs <= Math.min(x+1, xDim-1); xs++)
                                        } // for (ys = Math.max(y-1,0); ys <= Math.min(y+1, yDim-1); ys++)
                                    } // if ((blue >= lowBlue) && (blue <= highBlue))
                                } // if ((green >= lowGreen) && (green <= highGreen))
                            } // if ((red >= lowRed) && (red <= highRed))
                        } // for (x = 0; x < xDim; x++)
                    } // for (y = 0; y < yDim; y++)
                } // for (b = lowBlueBound; b <= highBlueBound; b++)
            } // for (g = lowGreenBound; g <= highGreenBound; g++)
        } // for (r = lowRedBound; r <= highRedBound; r++)
        
        
        
        setCompleted(true);
        return;
    }
    
    
    
}