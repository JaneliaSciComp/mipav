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
        short red;
        short green;
        short blue;
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
        short neighborRed;
        short neighborGreen;
        short neighborBlue;
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
        double pixelConnectedness;
        double connectednessSum;
        double connectednessDegree;
        short localRed[] = new short[8];
        short localGreen[] = new short[8];
        short localBlue[] = new short[8];
        double delta;
        int localRedSum;
        int localGreenSum;
        int localBlueSum;
        double localRedMean;
        double localGreenMean;
        double localBlueMean;
        double localSum;
        double pixelDispersionMeasure;
        double localDispersionMeasureSum;
        double localDispersionMeasure;
        short globalRed[] = new short[sliceSize];
        short globalGreen[] = new short[sliceSize];
        short globalBlue[] = new short[sliceSize];
        int globalRedSum;
        int globalGreenSum;
        int globalBlueSum;
        double globalRedMean;
        double globalGreenMean;
        double globalBlueMean;
        double globalSum;
        double globalDispersionMeasure;
        double homogeneityDegree;
        double SCD[][][] = new double[highRedBound-lowRedBound+1][highGreenBound-lowGreenBound+1][highBlueBound-lowBlueBound+1];
        
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
                    numSet = 0;
                    connectednessSum = 0.0;
                    localDispersionMeasureSum = 0.0;
                    globalRedSum = 0;
                    globalGreenSum = 0;
                    globalBlueSum = 0;
                    for (y = 0; y < yDim; y++) {
                        for (x = 0; x < xDim; x++) {
                            index = x + y * xDim;
                            red = redBuffer[index];
                            if ((red >= lowRed) && (red <= highRed)) {
                                green = greenBuffer[index];
                                if ((green >= lowGreen) && (green <= highGreen)) {
                                    blue = blueBuffer[index];
                                    if ((blue >= lowBlue) && (blue <= highBlue)) {
                                        globalRed[numSet] = red;
                                        globalGreen[numSet] = green;
                                        globalBlue[numSet] = blue;
                                        globalRedSum += red;
                                        globalGreenSum += green;
                                        globalBlueSum += blue;
                                        numSet++;
                                        numNeighbors = 0;
                                        connectedNeighbors = 0;
                                        localRedSum = 0;
                                        localGreenSum = 0;
                                        localBlueSum = 0;
                                        for (ys = Math.max(y-1,0); ys <= Math.min(y+1, yDim-1); ys++) {
                                            for (xs = Math.max(x-1,0); xs <= Math.min(x+1, xDim-1); xs++) {
                                                if ((ys != y) || (xs != x)) {
                                                    numNeighbors++;
                                                    neighborIndex = xs + ys * xDim;
                                                    neighborRed = redBuffer[neighborIndex];
                                                    if ((neighborRed >= lowRed) && (neighborRed <= highRed)) {
                                                        neighborGreen = greenBuffer[neighborIndex];
                                                        if ((neighborGreen >= lowGreen) && (neighborGreen <= highGreen)) {
                                                            neighborBlue = blueBuffer[neighborIndex];
                                                            if ((neighborBlue >= lowBlue) && (neighborBlue <= highBlue)) {
                                                                localRed[connectedNeighbors] = neighborRed;
                                                                localGreen[connectedNeighbors] = neighborGreen;
                                                                localBlue[connectedNeighbors] = neighborBlue;
                                                                localRedSum += neighborRed;
                                                                localGreenSum += neighborGreen;
                                                                localBlueSum += neighborBlue;
                                                                connectedNeighbors++;
                                                            } // if ((neighborBlue >= lowBlue) && (neighborBlue <= highBlue))
                                                        } // if ((neighborGreen >= lowGreen) && (neighborGreen <= highGreen))
                                                    } // if ((neighborRed >= lowRed) && (neighborRed <= highRed))
                                                } // if ((ys != y) || (xs != x))
                                            } // for (xs = Math.max(x-1,0); xs <= Math.min(x+1, xDim-1); xs++)
                                        } // for (ys = Math.max(y-1,0); ys <= Math.min(y+1, yDim-1); ys++)
                                        pixelConnectedness = ((double)connectedNeighbors)/((double)numNeighbors);
                                        connectednessSum += pixelConnectedness;
                                        localRedMean = ((double)localRedSum)/((double)connectedNeighbors);
                                        localGreenMean = ((double)localGreenSum)/((double)connectedNeighbors);
                                        localBlueMean = ((double)localBlueSum)/((double)connectedNeighbors);
                                        localSum = 0.0;
                                        for (i = 0; i < connectedNeighbors; i++) {
                                            delta = localRed[i] - localRedMean;
                                            localSum += delta * delta;
                                            delta = localGreen[i] - localGreenMean;
                                            localSum += delta * delta;
                                            delta = localBlue[i] - localBlueMean;
                                            localSum += delta * delta;
                                        }
                                        pixelDispersionMeasure = Math.sqrt(localSum)/connectedNeighbors;
                                        localDispersionMeasureSum += pixelDispersionMeasure;
                                    } // if ((blue >= lowBlue) && (blue <= highBlue))
                                } // if ((green >= lowGreen) && (green <= highGreen))
                            } // if ((red >= lowRed) && (red <= highRed))
                        } // for (x = 0; x < xDim; x++)
                    } // for (y = 0; y < yDim; y++)
                    globalRedMean = ((double)globalRedSum)/((double)numSet);
                    globalGreenMean = ((double)globalGreenSum)/((double)numSet);
                    globalBlueMean = ((double)globalBlueSum)/((double)numSet);
                    globalSum = 0.0;
                    for (i = 0; i < numSet; i++) {
                        delta = globalRed[i] - globalRedMean;
                        globalSum += delta * delta;
                        delta = globalGreen[i] - globalGreenMean;
                        globalSum += delta * delta;
                        delta = globalBlue[i] - globalBlueMean;
                        globalSum += delta * delta;
                    }
                    connectednessDegree = connectednessSum/numSet;
                    localDispersionMeasure = localDispersionMeasureSum/numSet;
                    globalDispersionMeasure = Math.sqrt(globalSum)/numSet;
                    if (globalDispersionMeasure != 0.0) {
                        homogeneityDegree = localDispersionMeasure/globalDispersionMeasure;
                    }
                    else {
                        homogeneityDegree = 1.0;
                    }
                    SCD[r - lowRedBound][g - lowGreenBound][b - lowBlueBound] = connectednessDegree * homogeneityDegree;
                } // for (b = lowBlueBound; b <= highBlueBound; b++)
            } // for (g = lowGreenBound; g <= highGreenBound; g++)
        } // for (r = lowRedBound; r <= highRedBound; r++)
        
        
        
        setCompleted(true);
        return;
    }
    
    
    
}