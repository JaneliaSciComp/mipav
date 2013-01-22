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
        int k;
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
        int rx;
        int gx;
        int bx;
        boolean localMaximum[][][] = new boolean[highRedBound-lowRedBound+1][highGreenBound-lowGreenBound+1][highBlueBound-lowBlueBound+1];
        int nCandidates;
        short candidateRed[];
        short candidateGreen[];
        short candidateBlue[];
        int candidateSideHalf[];
        boolean candidateMaxSize[];
        double candidateSCD[];
        int candidatesToGrow;
        int sideHalf;
        double newCandidateSCD[];
        int colorClass[] = new int[numClasses];
        double maxSCD;
        boolean candidateChosen[];
        double minEuclideanSquared;
        double euclideanSquared;
        double objectiveFunctionJ;
        double maxObjectiveFunctionJ;
        int candidateIndex = 0;
        ModelImage resultImage;
        int oldCandidates;
        int numObjects;
        int idObject[][][] = new int[highRedBound-lowRedBound+1][highGreenBound-lowGreenBound+1][highBlueBound-lowBlueBound+1];
        boolean objectFound;
        boolean objectGrow;
        int numBins = 0;
        double totr;
        double totg;
        double totb;
        int minr = 0;
        int ming = 0;
        int minb = 0;
        
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
        
        Preferences.debug("Section 1\n", Preferences.DEBUG_ALGORITHM);
        Preferences.debug("lowRedBound = " + lowRedBound + "\n", Preferences.DEBUG_ALGORITHM);
        Preferences.debug("highRedBound = " + highRedBound + "\n", Preferences.DEBUG_ALGORITHM);
        for (r = lowRedBound; r <= highRedBound; r++) {
            Preferences.debug("r = " + r + "\n", Preferences.DEBUG_ALGORITHM);
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
        
        
        nCandidates = 0;
        Preferences.debug("Section 2\n", Preferences.DEBUG_ALGORITHM);
        for (r = 0; r <= highRedBound - lowRedBound; r++) {
            Preferences.debug("r = " + r + "\n", Preferences.DEBUG_ALGORITHM);
            for (g = 0; g <= highGreenBound - lowGreenBound; g++) {
                for (b = 0; b <= highBlueBound - lowBlueBound; b++) {
                    localMaximum[r][g][b] = true;
                    for (rx = Math.max(0, r-1); rx <= Math.min(highRedBound - lowRedBound, r+1) && localMaximum[r][g][b]; rx++) {
                        for (gx = Math.max(0, g-1); gx <= Math.min(highGreenBound - lowGreenBound, g+1) && localMaximum[r][g][b]; gx++) {
                            for (bx = Math.max(0, b-1); bx <= Math.min(highBlueBound - lowBlueBound, b+1)  && localMaximum[r][g][b]; bx++) {
                                if (SCD[r][g][b] < SCD[rx][gx][bx]) {
                                    localMaximum[r][g][b] = false;
                                }
                            } // for (bx = Math.max(0, b-1); bx <= Math.min(highBlueBound - lowBlueBound, b+1)  && localMaximum[r][g][b]; bx++)
                        } // for (gx = Math.max(0, g-1); gx <= Math.min(highGreenBound - lowGreenBound, g+1) && localMaximum[r][g][b]; gx++)
                    } // for (rx = Math.max(0, r-1); rx <= Math.min(highRedBound - lowRedBound, r+1) && localMaximum[r][g][b]; rx++)
                    if (localMaximum[r][g][b]) {
                        nCandidates++;
                    }
                } // for (b = 0; b <= highBlueBound - lowBlueBound; b++) 
            } // for (g = 0; g <= highGreenBound - lowGreenBound; g++)
        } // for (r = 0; r <= highRedBound - lowRedBound; r++)
        
        oldCandidates = Integer.MAX_VALUE;
        Preferences.debug("Section 3\n", Preferences.DEBUG_ALGORITHM);
        while (nCandidates < oldCandidates) {
            Preferences.debug("nCandidates = " + nCandidates + "\n", Preferences.DEBUG_ALGORITHM);
            oldCandidates = nCandidates;
            for (r = 0; r <= highRedBound - lowRedBound; r++) {
                for (g = 0; g <= highGreenBound - lowGreenBound; g++) {
                    for (b = 0; b <= highBlueBound - lowBlueBound; b++) {
                        if (!localMaximum[r][g][b]) {
                            for (rx = Math.max(0, r-1); rx <= Math.min(highRedBound - lowRedBound, r+1); rx++) {
                                for (gx = Math.max(0, g-1); gx <= Math.min(highGreenBound - lowGreenBound, g+1); gx++) {
                                    for (bx = Math.max(0, b-1); bx <= Math.min(highBlueBound - lowBlueBound, b+1); bx++) {
                                        if (SCD[rx][gx][bx] == SCD[r][g][b]) {
                                            if (localMaximum[rx][gx][bx]) {
                                                localMaximum[rx][gx][bx] = false;
                                                nCandidates--;
                                            }
                                        }
                                    }
                                }
                            }
                        }
                    }
                }
            }
        } // while (nCandidates < oldCandidates)
        
        objectFound = true;
        numObjects = 0;
        Preferences.debug("Section 4\n", Preferences.DEBUG_ALGORITHM);
        while (objectFound) {
            Preferences.debug("numObjects = " + numObjects + "\n", Preferences.DEBUG_ALGORITHM);
            objectFound = false;
            for (r = 0; (r <= highRedBound - lowRedBound) && (!objectFound); r++) {
                for (g = 0; (g <= highGreenBound - lowGreenBound) && (!objectFound); g++) {
                    for (b = 0; (b <= highBlueBound - lowBlueBound) && (!objectFound); b++) {
                        if (localMaximum[r][g][b]  && idObject[r][g][b] == 0) {
                            numObjects++;
                            objectFound = true;
                            idObject[r][g][b] = numObjects;
                        }
                    }
                }
            }
            
            if (objectFound) {
                objectGrow = true;
                while (objectGrow) {
                    objectGrow = false;
                    for (r = 0; r <= highRedBound - lowRedBound; r++) {
                        for (g = 0; g <= highGreenBound - lowGreenBound; g++) {
                            for (b = 0; b <= highBlueBound - lowBlueBound; b++) {
                                if (idObject[r][g][b] == numObjects) {
                                    for (rx = Math.max(0, r-1); rx <= Math.min(highRedBound - lowRedBound, r+1); rx++) {
                                        for (gx = Math.max(0, g-1); gx <= Math.min(highGreenBound - lowGreenBound, g+1); gx++) {
                                            for (bx = Math.max(0, b-1); bx <= Math.min(highBlueBound - lowBlueBound, b+1); bx++) {
                                                if (localMaximum[rx][gx][bx] && idObject[rx][gx][bx] == 0) {
                                                    idObject[rx][gx][bx] = numObjects;
                                                    objectGrow = true;
                                                }
                                            }
                                        }
                                    }
                                }
                            }
                        }
                    } 
                } // while (objectGrow)
            } // if (objectFound)
        } // while (objectFound)
        
        nCandidates = numObjects;
        Preferences.debug("Final nCandidates = " + nCandidates + "\n", Preferences.DEBUG_ALGORITHM);
        candidatesToGrow = nCandidates;
        candidateRed = new short[nCandidates];
        candidateGreen = new short[nCandidates];
        candidateBlue = new short[nCandidates];
        candidateSideHalf = new int[nCandidates];
        candidateMaxSize = new boolean[nCandidates];
        candidateSCD = new double[nCandidates];
        newCandidateSCD = new double[nCandidates];
        
        Preferences.debug("Section 5\n", Preferences.DEBUG_ALGORITHM);
        for (i = 1; i <= nCandidates; i++) {
            Preferences.debug("i = "+ i + "\n", Preferences.DEBUG_ALGORITHM);
            numBins = 0;
            totr = 0.0;
            totg = 0.0;
            totb = 0.0;
            for (r = 0; r <= highRedBound - lowRedBound; r++) {
                for (g = 0; g <= highGreenBound - lowGreenBound; g++) {
                    for (b = 0; b <= highBlueBound - lowBlueBound; b++) {
                        if (idObject[r][g][b] == i) {
                            numBins++;
                            totr += r;
                            totg += g;
                            totb += b;
                        }
                    }
                }
            }
            totr = totr/numBins;
            totg = totg/numBins;
            totb = totb/numBins;
            
            minEuclideanSquared = Double.MAX_VALUE;
            for (r = 0; r <= highRedBound - lowRedBound; r++) {
                for (g = 0; g <= highGreenBound - lowGreenBound; g++) {
                    for (b = 0; b <= highBlueBound - lowBlueBound; b++) {
                        if (idObject[r][g][b] == i) {
                            euclideanSquared = 0.0;
                            delta = r - totr;
                            euclideanSquared += delta * delta;
                            delta = g - totg;
                            euclideanSquared += delta * delta;
                            delta = b - totb;
                            euclideanSquared += delta * delta;
                            if (euclideanSquared < minEuclideanSquared) {
                                minEuclideanSquared = euclideanSquared;
                                minr = r;
                                ming = g;
                                minb = b;
                            }
                        }
                    }
                }
            }
            
            candidateRed[i] = (short)(minr + lowRedBound);
            candidateGreen[i] = (short)(ming + lowGreenBound);
            candidateBlue[i] = (short)(minb + lowBlueBound);
            candidateSideHalf[i] = initialSideHalf;
            candidateSCD[i] = SCD[candidateRed[i] - lowRedBound][candidateGreen[i] - lowGreenBound][candidateBlue[i] - lowBlueBound];
        } // for (i = 1; i <= nCandidates; i++)
        
        Preferences.debug("Section 6\n", Preferences.DEBUG_ALGORITHM);
        
        for (i = 0; i < nCandidates; i++) {
            Preferences.debug("i = " + i + "\n", Preferences.DEBUG_ALGORITHM);
            for (j = i+1; j < nCandidates; j++) {
                if ((Math.abs(candidateRed[i] - candidateRed[j]) <= (candidateSideHalf[i] + candidateSideHalf[j])) &&
                    (Math.abs(candidateGreen[i] - candidateGreen[j]) <= (candidateSideHalf[i] + candidateSideHalf[j])) &&
                    (Math.abs(candidateBlue[i] - candidateBlue[j]) <= (candidateSideHalf[i] + candidateSideHalf[j]))) {
                    if (!candidateMaxSize[i]) {
                        candidateMaxSize[i] = true;
                        candidatesToGrow--;
                    }
                    if (!candidateMaxSize[j]) {
                        candidateMaxSize[j] = true;
                        candidatesToGrow--;
                    }
                }
            }
        }
        
        sideHalf = initialSideHalf;
        Preferences.debug("Section 7\n", Preferences.DEBUG_ALGORITHM);
        while (candidatesToGrow > 0) {
            Preferences.debug("candidatesToGrow = " + candidatesToGrow + "\n", Preferences.DEBUG_ALGORITHM);
            sideHalf++;
            for (i = 0; i < nCandidates; i++) {
                if (!candidateMaxSize[i]) {
                    if (((candidateRed[i] - sideHalf) < minRed) || ((candidateRed[i] + sideHalf) > maxRed) ||
                        ((candidateGreen[i] - sideHalf) < minGreen) || ((candidateGreen[i] + sideHalf) > maxGreen) ||
                        ((candidateBlue[i] - sideHalf) < minBlue) || ((candidateBlue[i] + sideHalf) > maxBlue)) {
                        candidateMaxSize[i] = true;
                        candidatesToGrow--;
                    }
                } // if (!candidateMaxSize[i])
            } // for (i = 0; i < nCandidates; i++)
            
            for (i = 0; i < nCandidates; i++) {
                if (!candidateMaxSize[i]) {
                    lowRed = candidateRed[i] - sideHalf;
                    highRed = candidateRed[i] + sideHalf;
                    lowGreen = candidateGreen[i] - sideHalf;
                    highGreen = candidateGreen[i] + sideHalf;
                    lowBlue = candidateBlue[i] - sideHalf;
                    highBlue = candidateBlue[i] + sideHalf;
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
                                        for (j = 0; j < connectedNeighbors; j++) {
                                            delta = localRed[j] - localRedMean;
                                            localSum += delta * delta;
                                            delta = localGreen[j] - localGreenMean;
                                            localSum += delta * delta;
                                            delta = localBlue[j] - localBlueMean;
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
                    for (j = 0; j < numSet; j++) {
                        delta = globalRed[j] - globalRedMean;
                        globalSum += delta * delta;
                        delta = globalGreen[j] - globalGreenMean;
                        globalSum += delta * delta;
                        delta = globalBlue[j] - globalBlueMean;
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
                    newCandidateSCD[i] = connectednessDegree * homogeneityDegree;
                    if (newCandidateSCD[i] <= candidateSCD[i]) {
                        candidateMaxSize[i] = true;
                        candidatesToGrow--;
                    }
                } // if (!candidateMaxSize[i])
            } // for (i = 0; i < nCandidates; i++)
            
            for (i = 0; i < nCandidates; i++) {
                if (!candidateMaxSize[i]) {
                    for (j = i+1; j < nCandidates; j++) {
                        if (candidateMaxSize[j]) {
                            if ((Math.abs(candidateRed[i] - candidateRed[j]) <= (sideHalf + candidateSideHalf[j])) &&
                                    (Math.abs(candidateGreen[i] - candidateGreen[j]) <= (sideHalf + candidateSideHalf[j])) &&
                                    (Math.abs(candidateBlue[i] - candidateBlue[j]) <= (sideHalf + candidateSideHalf[j]))) {
                                    if (!candidateMaxSize[i]) {
                                        candidateMaxSize[i] = true;
                                        candidatesToGrow--;
                                    }
                            }    
                        } // if (candidateMaxSize[j])
                        else { // !candidateMaxSize[j]
                            if ((Math.abs(candidateRed[i] - candidateRed[j]) <= (2*sideHalf)) &&
                                    (Math.abs(candidateGreen[i] - candidateGreen[j]) <= (2*sideHalf)) &&
                                    (Math.abs(candidateBlue[i] - candidateBlue[j]) <= (2*sideHalf))) {
                                    if (!candidateMaxSize[i]) {
                                        candidateMaxSize[i] = true;
                                        candidatesToGrow--;
                                    }
                                    if (!candidateMaxSize[j]) {
                                        candidateMaxSize[j] = true;
                                        candidatesToGrow--;
                                    }
                                }    
                        }
                    } // for (j = i+1; j < nCandidates; j++)
                } // if (!candidateMaxSize[i])
                
                if (!candidateMaxSize[i]) {
                    candidateSCD[i] = newCandidateSCD[i];
                    candidateSideHalf[i] = sideHalf;
                }
                
            } // for (i = 0; i < nCandidates; i++)
        } // while (candidatesToGrow > 0)
        
        // Select numClasses of the nCandidates
        
        // First select the nCandidate with the highest SCD
        colorClass[0] = 0;
        maxSCD = candidateSCD[0];
        for (i = 1; i < nCandidates; i++) {
            if (candidateSCD[i] > maxSCD) {
                colorClass[0] = i;
                maxSCD = candidateSCD[i];
            }
        }
        candidateChosen = new boolean[nCandidates];
        candidateChosen[colorClass[0]] = true;
        
        for (i = 1; i < numClasses; i++) {
            maxObjectiveFunctionJ = 0.0;
            for (j = 0; j < nCandidates; j++) {
                if (!candidateChosen[j]) {
                    minEuclideanSquared = Double.MAX_VALUE;
                    for (k = 0; k < i; k++) {
                        euclideanSquared = 0.0;
                        delta = candidateRed[j] - candidateRed[colorClass[k]];
                        euclideanSquared += delta * delta;
                        delta = candidateGreen[j] - candidateGreen[colorClass[k]];
                        euclideanSquared += delta * delta;
                        delta = candidateBlue[j] - candidateBlue[colorClass[k]];
                        euclideanSquared += delta * delta;
                        if (euclideanSquared < minEuclideanSquared) {
                            minEuclideanSquared = euclideanSquared;
                        }
                    } // for (k = 0; k < i; k++)
                    objectiveFunctionJ = candidateSCD[j] * minEuclideanSquared;
                    if (objectiveFunctionJ > maxObjectiveFunctionJ) {
                        maxObjectiveFunctionJ = objectiveFunctionJ;
                        colorClass[i] = j;
                    }
                } // if (!candidateChosen[j])
            } // for (j = 0; j < nCandidates; j++)
        } // for (i = 1; i < numClasses; i++)
        
        for (y = 0; y < yDim; y++) {
            for (x = 0; x < xDim; x++) {
                index = x + y * xDim;
                minEuclideanSquared = Double.MAX_VALUE;
                for (i = 0; i < numClasses; i++) {
                    euclideanSquared = 0.0;
                    delta = redBuffer[index] - candidateRed[colorClass[i]];
                    euclideanSquared += delta * delta;
                    delta = greenBuffer[index] - candidateGreen[colorClass[i]];
                    euclideanSquared += delta * delta;
                    delta = blueBuffer[index] - candidateBlue[colorClass[i]];
                    euclideanSquared += delta * delta;
                    if (euclideanSquared < minEuclideanSquared) {
                        minEuclideanSquared = euclideanSquared;
                        candidateIndex = colorClass[i];
                    }
                } // for (i = 0; i < numClasses; i++)
                redBuffer[index] = candidateRed[candidateIndex];
                greenBuffer[index] = candidateGreen[candidateIndex];
                blueBuffer[index] = candidateBlue[candidateIndex];
            } // for (x = 0; x < xDim; x++)
        } // for (y = 0; y < yDim; y++)
        
        if (destImage == null) {
            resultImage = srcImage;
        }
        else {
            resultImage = destImage;
        }
        
        try {
            resultImage.importRGBData(1, 0, redBuffer, true);
        }
        catch (IOException e) {
            MipavUtil.displayError("IOException " + e + " on resultImage.importRGBData(1, 0, redBuffer, true)");
            setCompleted(false);
            return;    
        }
        
        try {
            resultImage.importRGBData(2, 0, greenBuffer, true);
        }
        catch (IOException e) {
            MipavUtil.displayError("IOException " + e + " on resultImage.importRGBData(2, 0, greenBuffer, true)");
            setCompleted(false);
            return;    
        }
        
        try {
            resultImage.importRGBData(3, 0,  blueBuffer, true);
        }
        catch (IOException e) {
            MipavUtil.displayError("IOException " + e + " on resultImage.importRGBData(3, 0, blueBuffer, true)");
            setCompleted(false);
            return;    
        }
        
        setCompleted(true);
        return;
    }
    
    
    
}