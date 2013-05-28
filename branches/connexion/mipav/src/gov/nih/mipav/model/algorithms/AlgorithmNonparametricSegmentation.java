package gov.nih.mipav.model.algorithms;

import WildMagic.LibFoundation.Mathematics.Vector3f;

import gov.nih.mipav.model.algorithms.filters.*;
import gov.nih.mipav.model.structures.*;

import gov.nih.mipav.view.*;

import java.io.*;

import java.util.*;


/**
 * DOCUMENT ME!
 */
public class AlgorithmNonparametricSegmentation extends AlgorithmBase {

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** DOCUMENT ME! */
    private ModelImage featureImage = null;

    /** DOCUMENT ME! */
    private boolean useGreen = false;

    /** DOCUMENT ME! */
    private boolean useRed = false;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Creates a new AlgorithmNonparametricSegmentation object.
     *
     * @param  srcImage  original image
     */
    public AlgorithmNonparametricSegmentation(ModelImage srcImage) {

        super(null, srcImage);
    }

    // This program creates a 128 by 128 by 128 3D feature space for 3 color images.  For 3
    // color images the program is too slow to be practical.  For 2 color images the program
    // creates a 128 by 128 feature space.  The program worked for a 2 color image with solid
    // blobs of distinct color, but not not useful for a cell image with many fine granules.
    // For black and white images a 128 by 128 feture space of intensity versus the second
    // derivative of intensity was created.  The quality of the result was very poor.
    // Reference:
    // 1.) Non-parametric segmentation of multi-spectral MR images incorporating
    // spatial and intensity information by Joze Derganc, Bostjan Likar,
    // and Franjo Pernus, Medical Imaging 2002: Imaging Processing, Milan Sonka,
    // J. Michael Fitzpatrick, Editors, Proceedings of SPIE Vol. 4684, 2002,
    // pp. 391-400.

    /**
     * Creates a new AlgorithmNonparametricSegmentation object.
     *
     * @param  resultImage  segmented image
     * @param  srcImage     original image
     */
    public AlgorithmNonparametricSegmentation(ModelImage resultImage, ModelImage srcImage) {

        super(resultImage, srcImage);
    }

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * Prepares this class for destruction.
     */
    public void finalize() {
        srcImage = null;
        destImage = null;

        super.finalize();
    }

    /**
     * DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    public ModelImage getFeatureImage() {
        return featureImage;
    }

    /**
     * DOCUMENT ME!
     */
    public void runAlgorithm() {
        int numColors = 0;

        if (srcImage.isColorImage()) {
            srcImage.calcMinMax();

            if (srcImage.getMinR() != srcImage.getMaxR()) {
                useRed = true;
                numColors++;
            }

            if (srcImage.getMinG() != srcImage.getMaxG()) {
                useGreen = true;
                numColors++;
            }

            if (srcImage.getMinB() != srcImage.getMaxB()) {
                numColors++;
            }

            if (numColors == 3) {
                segment3Colors();
            } else if (numColors == 2) {
                segment2Colors();
            }
        } // if (srcImage.isColorImage())
        else {
            segmentBW();
        }
    }

    /**
     * DOCUMENT ME!
     */
    private void segment2Colors() {
        int c1Num;
        int c2Num;
        int c1Bins = 128;
        int c2Bins = 128;
        int c1Divide = 1;
        int c2Divide = 1;
        float[] featureSpace;
        int c1Index;
        int c2Index;
        int xDim;
        int yDim;
        int zDim;
        int sliceLength;
        int totLength;
        float[] c1Buffer;
        float[] c2Buffer;
        double c1Min;
        double c2Min;
        int i;
        short[] label;
        int x;
        int y;
        int z;
        int xmin;
        int xmax;
        int ymin;
        int ymax;
        ModelImage smoothImage;
        int[] smoothExtents;
        float[] smoothSigmas;
        AlgorithmGaussianBlurSep gaussAlgo;
        float oldDensity;
        int[] path;
        int pathIndex;
        float newDensity;
        boolean positiveLabel;
        int x1;
        int y1;
        int i1;
        int j1;
        float total;
        float density1;
        int nextIndex;
        int j;
        short labelNum = 0;
        int xp;
        int yp;
        ViewVOIVector VOIs = null;
        int nVOIs;
        int nPoints;
        Vector3f[] points;
        int index;
        short[] principalLabel;
        int k;
        int nBins;
        short[] segBuffer;
        float[] meanx;
        float[] meany;
        int[] labelCount;
        short[] label2 = null;
        float distance;
        float testDistance;
        short closestLabel;

        try {
            

            fireProgressStateChanged(srcImage.getImageName(), "Performing Nonparametric segmentation...");


            if (useRed) {
                c1Min = srcImage.getMinR();
                c1Num = (int) Math.round(srcImage.getMaxR() - c1Min + 1);

                if (useGreen) {
                    c2Min = srcImage.getMinG();
                    c2Num = (int) Math.round(srcImage.getMaxG() - c2Min + 1);
                } else {
                    c2Min = srcImage.getMinB();
                    c2Num = (int) Math.round(srcImage.getMaxB() - c2Min + 1);
                }
            } // if (useRed)
            else {
                c1Min = srcImage.getMinG();
                c1Num = (int) Math.round(srcImage.getMaxG() - c1Min + 1);
                c2Min = srcImage.getMinB();
                c2Num = (int) Math.round(srcImage.getMaxB() - c2Min + 1);
            } // else

            xDim = srcImage.getExtents()[0];
            yDim = srcImage.getExtents()[1];

            if (srcImage.getNDims() > 2) {
                zDim = srcImage.getExtents()[2];
            } else {
                zDim = 1;
            }

            sliceLength = xDim * yDim;
            totLength = sliceLength * zDim;

            // A maximum of 128 red, green, and blue bins
            if (c1Num <= 128) {
                c1Bins = c1Num;
            } else {
                c1Divide = c1Num / 128;

                if ((c1Num % 128) > 0) {
                    c1Divide++;
                }

                c1Bins = c1Num / c1Divide;

                if ((c1Num % c1Divide) > 0) {
                    c1Bins++;
                }
            }

            if (c2Num <= 128) {
                c2Bins = c2Num;
            } else {
                c2Divide = c2Num / 128;

                if ((c2Num % 128) > 0) {
                    c2Divide++;
                }

                c2Bins = c2Num / c2Divide;

                if ((c2Num % c2Divide) > 0) {
                    c2Bins++;
                }
            }


            // Create 2D feature space
            nBins = c1Bins * c2Bins;
            featureSpace = new float[nBins];
            c1Buffer = new float[totLength];
            c2Buffer = new float[totLength];

            if (useRed) {
                srcImage.exportRGBData(1, 0, totLength, c1Buffer);

                if (useGreen) {
                    srcImage.exportRGBData(2, 0, totLength, c2Buffer);
                } else {
                    srcImage.exportRGBData(3, 0, totLength, c2Buffer);
                }
            } else {
                srcImage.exportRGBData(2, 0, totLength, c1Buffer);
                srcImage.exportRGBData(3, 0, totLength, c2Buffer);
            }

            for (i = 0; i < totLength; i++) {
                c1Index = (int) ((c1Buffer[i] - c1Min) / c1Divide);
                c2Index = (int) ((c2Buffer[i] - c2Min) / c2Divide);
                featureSpace[(c2Index * c1Bins) + c1Index]++;
            } // for (i = 0; i < totLength; i++)

            // Smooth feature space
            smoothExtents = new int[2];
            smoothExtents[0] = c1Bins;
            smoothExtents[1] = c2Bins;
            smoothImage = new ModelImage(ModelImage.FLOAT, smoothExtents, "smoothedHistogram");
            smoothImage.importData(0, featureSpace, true);

            // Create a 3 by 3 smoothing kernel
            smoothSigmas = new float[2];
            smoothSigmas[0] = 0.6f;
            smoothSigmas[1] = 0.6f;
            gaussAlgo = new AlgorithmGaussianBlurSep(smoothImage, smoothSigmas, true, false);
            gaussAlgo.run();
            gaussAlgo.finalize();
            gaussAlgo = null;
            smoothImage.exportData(0, nBins, featureSpace);
            smoothImage.disposeLocal();
            smoothImage = null;

            // Normalize feature space
            total = featureSpace[0];

            for (i = 1; i < featureSpace.length; i++) {
                total += featureSpace[i];
            }

            for (i = 0; i < featureSpace.length; i++) {
                featureSpace[i] /= total;
            }
            /*featureImage = new ModelImage(ModelImage.FLOAT,smoothExtents,
             *                            "smoothedHistogram",
             * srcImage.getUserInterface());featureImage.importData(0, featureSpace, true);*/


            // Max-shift procedure;
            // Initialize label function to 0
            label = new short[nBins];
            path = new int[nBins];

            for (i = 0, x = 0, y = 0; i < nBins; i++) {
                fireProgressStateChanged(i * 100 / (2 * nBins));

                if (label[i] == 0) {
                    pathIndex = 0;
                    Arrays.fill(path, 0);
                    path[pathIndex++] = i;
                    nextIndex = i;
                    newDensity = featureSpace[i];
                    oldDensity = -1.0f;
                    positiveLabel = false;
                    xp = x;
                    yp = y;

                    while ((newDensity > oldDensity) && (!positiveLabel)) {
                        oldDensity = newDensity;
                        ymin = yp - 2;

                        if (ymin < 0) {
                            ymin = 0;
                        }

                        ymax = yp + 2;

                        if (ymax > (c2Bins - 1)) {
                            ymax = c2Bins - 1;
                        }

                        xmin = xp - 2;

                        if (xmin < 0) {
                            xmin = 0;
                        }

                        xmax = xp + 2;

                        if (xmax > (c1Bins - 1)) {
                            xmax = c1Bins - 1;
                        }

                        for (y1 = ymin, j1 = ymin * c1Bins; y1 <= ymax; y1++, j1 += c1Bins) {

                            for (x1 = xmin, i1 = j1 + xmin; x1 <= xmax; x1++, i1++) {
                                density1 = featureSpace[i1];

                                if (density1 > newDensity) {
                                    newDensity = density1;
                                    nextIndex = i1;
                                    xp = x1;
                                    yp = y1;
                                }
                            } // for (x1 = xmin, i1 = j1 + xmin; x1 <= xmax; x1++, i1++)
                        } // for (y1 = ymin, j1 = ymin*c1Bins; y1 <= ymax; y1++, j1 += c1Bins)

                        if (label[nextIndex] != 0) {
                            positiveLabel = true;
                        } else if (newDensity > oldDensity) {
                            path[pathIndex++] = nextIndex;
                        }
                    } // while ((newDensity > oldDensity) && (!positiveLabel))

                    if (positiveLabel) {

                        for (j = 0; j < pathIndex; j++) {
                            label[path[j]] = label[nextIndex];
                        }
                    } // if (positiveLabel)
                    else { // !positiveLabel
                        labelNum++;

                        for (j = 0; j < pathIndex; j++) {
                            label[path[j]] = labelNum;
                        }
                    } // else !positiveLabel

                    x++;

                    if (x == c1Bins) {
                        x = 0;
                        y++;
                    } // if (x == c1Bins)
                } // if (label[i] == 0)
            } // for (i = 0, x= 0, y = 0; i < nBins; i++)

            VOIs = srcImage.getVOIs();
            nVOIs = VOIs.size();
            nPoints = 0;

            for (i = 0; i < nVOIs; i++) {
                points = VOIs.VOIAt(i).exportAllPoints();

                if (points != null) {
                    nPoints += points.length;
                }
            } // for (i = 0; i < nVOIs; i++)

            if (nPoints != 0) {
                principalLabel = new short[nPoints];

                for (i = 0, k = 0; i < nVOIs; i++) {
                    points = VOIs.VOIAt(i).exportAllPoints();

                    if (points != null) {

                        for (j = 0; j < points.length; j++) {
                            x = (int) points[j].X;
                            y = (int) points[j].Y;
                            z = (int) points[j].Z;
                            index = (z * sliceLength) + (y * xDim) + x;
                            c1Index = (int) ((c1Buffer[index] - c1Min) / c1Divide);
                            c2Index = (int) ((c2Buffer[index] - c2Min) / c2Divide);
                            principalLabel[k++] = label[(c2Index * c1Bins) + c1Index];
                        }
                    }
                } // for (i = 0, k = 0; i < nVOIs; i++)

                // Find the mean point of each label from 1 to labelNum in feature space
                meanx = new float[labelNum];
                meany = new float[labelNum];
                labelCount = new int[labelNum];

                for (i = 0, x = 0, y = 0; i < nBins; i++) {
                    meanx[label[i] - 1] += x;
                    meany[label[i] - 1] += y;
                    labelCount[label[i] - 1]++;
                    x++;

                    if (x == c1Bins) {
                        x = 0;
                        y++;
                    }
                } // for (i = 0, x = 0, y = 0; i < nBins; i++)

                for (i = 0; i < labelNum; i++) {
                    meanx[i] /= labelCount[i];
                    meany[i] /= labelCount[i];
                }

                // Find the distance in feature space between the mean of the nonprincipal
                // cluster and the closest border point of a principal cluster
                label2 = new short[nBins];

                for (i = 0; i < nBins; i++) {

                    for (j = 0; j < nPoints; j++) {

                        if (label[i] == principalLabel[j]) {
                            label2[i] = (short) (j * 255 / nPoints);
                        }
                    }
                }

loop1:
                for (i = 0; i < labelNum; i++) {
                    fireProgressStateChanged(50 + (i * 100 / (2 * labelNum)));

                    for (j = 0; j < nPoints; j++) {

                        if (principalLabel[j] == (i + 1)) {
                            continue loop1;
                        } // if (principalLabel[j] == (i+1))
                    } // for (j = 0; j < nPoints; j++)

                    closestLabel = -1;
                    distance = Float.MAX_VALUE;

                    for (j = 0; j < nPoints; j++) {

                        for (k = 0, x = 0, y = 0; k < nBins; k++) {

                            if (label[k] == principalLabel[j]) {
                                testDistance = ((x - meanx[i]) * (x - meanx[i])) + ((y - meany[i]) * (y - meany[i]));

                                if (distance > testDistance) {
                                    distance = testDistance;
                                    closestLabel = (short) (j * 255 / nPoints);
                                }
                            } // if (label[k] == principalLabel[j])

                            x++;

                            if (x == c1Bins) {
                                x = 0;
                                y++;
                            } // if (x == c1Bins)
                        } // for (k = 0, x = 0, y = 0; k < nBins; k++)
                    } // for (j = 0; j < nPoints; j++)

                    for (j = 0; j < nBins; j++) {

                        if (label[j] == (i + 1)) {
                            label2[j] = closestLabel;
                        }
                    } // for (j = 0; j < nBins; j++)
                } // for (i = 0; i < labelNum; i++)

            } // if (nPoints != 0)

            segBuffer = new short[totLength];

            if (label2 != null) {

                for (i = 0; i < totLength; i++) {
                    c1Index = (int) ((c1Buffer[i] - c1Min) / c1Divide);
                    c2Index = (int) ((c2Buffer[i] - c2Min) / c2Divide);
                    segBuffer[i] = label2[(c2Index * c1Bins) + c1Index];
                }

            } // if (label2 != null)
            else {

                for (i = 0; i < totLength; i++) {
                    c1Index = (int) ((c1Buffer[i] - c1Min) / c1Divide);
                    c2Index = (int) ((c2Buffer[i] - c2Min) / c2Divide);
                    segBuffer[i] = label[(c2Index * c1Bins) + c1Index];
                }
            }

            if (destImage != null) {
                destImage.importData(0, segBuffer, true);
            } else {
                srcImage.reallocate(ModelImage.SHORT);
                srcImage.importData(0, segBuffer, true);
            }

            setCompleted(true);

            return;
        } // try
        catch (IOException ioe) {
            finalize();
            System.gc();
            MipavUtil.displayError("Algorithm Nonparametric segmentation reports:\n" + ioe.toString());

            setCompleted(false);

            return;
        } catch (OutOfMemoryError error) {
            finalize();
            System.gc();
            MipavUtil.displayError("Algorithm Nonparametricsegmentation reports:\n" + error.toString());


            setCompleted(false);

            return;
        }

    }

    /**
     * DOCUMENT ME!
     */
    private void segment3Colors() {
        int redNum;
        int greenNum;
        int blueNum;
        int redBins = 128;
        int greenBins = 128;
        int blueBins = 128;
        int redDivide = 1;
        int greenDivide = 1;
        int blueDivide = 1;
        float[] featureSpace;
        int redIndex;
        int greenIndex;
        int blueIndex;
        int xDim;
        int yDim;
        int zDim;
        int sliceLength;
        int totLength;
        float[] redBuffer;
        float[] greenBuffer;
        float[] blueBuffer;
        double redMin;
        double greenMin;
        double blueMin;
        int i;
        short[] label;
        int x;
        int y;
        int z;
        int xmin;
        int xmax;
        int ymin;
        int ymax;
        int zmin;
        int zmax;
        int redGreen;
        ModelImage smoothImage;
        int[] smoothExtents;
        float[] smoothSigmas;
        AlgorithmGaussianBlurSep gaussAlgo;
        float oldDensity;
        int[] path;
        int pathIndex;
        float newDensity;
        boolean positiveLabel;
        int x1;
        int y1;
        int z1;
        int i1;
        int j1;
        int k1;
        float total;
        float density1;
        int nextIndex;
        int j;
        short labelNum = 0;
        int xp;
        int yp;
        int zp;
        ViewVOIVector VOIs = null;
        int nVOIs;
        int nPoints;
        Vector3f[] points;
        int index;
        short[] principalLabel;
        int k;
        int nBins;
        short[] segBuffer;
        float[] meanx;
        float[] meany;
        float[] meanz;
        int[] labelCount;
        short[] label2 = null;
        float distance;
        float testDistance;
        short closestLabel;

        try {
            

            fireProgressStateChanged(srcImage.getImageName(), "Performing Nonparametric segmentation...");


            redMin = srcImage.getMinR();
            greenMin = srcImage.getMinG();
            blueMin = srcImage.getMinB();
            redNum = (int) Math.round(srcImage.getMaxR() - redMin + 1);
            greenNum = (int) Math.round(srcImage.getMaxG() - greenMin + 1);
            blueNum = (int) Math.round(srcImage.getMaxB() - blueMin + 1);
            xDim = srcImage.getExtents()[0];
            yDim = srcImage.getExtents()[1];

            if (srcImage.getNDims() > 2) {
                zDim = srcImage.getExtents()[2];
            } else {
                zDim = 1;
            }

            sliceLength = xDim * yDim;
            totLength = sliceLength * zDim;

            // A maximum 0f 128 red, green, and blue bins
            if (redNum <= 128) {
                redBins = redNum;
            } else {
                redDivide = redNum / 128;

                if ((redNum % 128) > 0) {
                    redDivide++;
                }

                redBins = redNum / redDivide;

                if ((redNum % redDivide) > 0) {
                    redBins++;
                }
            }

            if (greenNum <= 128) {
                greenBins = greenNum;
            } else {
                greenDivide = greenNum / 128;

                if ((greenNum % 128) > 0) {
                    greenDivide++;
                }

                greenBins = greenNum / greenDivide;

                if ((greenNum % greenDivide) > 0) {
                    greenBins++;
                }
            }

            if (blueNum <= 128) {
                blueBins = blueNum;
            } else {
                blueDivide = blueNum / 128;

                if ((blueNum % 128) > 0) {
                    blueDivide++;
                }

                blueBins = blueNum / blueDivide;

                if ((blueNum % blueDivide) > 0) {
                    blueBins++;
                }
            }

            // Create 3D feature space
            nBins = redBins * greenBins * blueBins;
            featureSpace = new float[nBins];
            redBuffer = new float[totLength];
            greenBuffer = new float[totLength];
            blueBuffer = new float[totLength];
            srcImage.exportRGBData(1, 0, totLength, redBuffer);
            srcImage.exportRGBData(2, 0, totLength, greenBuffer);
            srcImage.exportRGBData(3, 0, totLength, blueBuffer);
            redGreen = redBins * greenBins;

            for (i = 0; i < totLength; i++) {
                redIndex = (int) ((redBuffer[i] - redMin) / redDivide);
                greenIndex = (int) ((greenBuffer[i] - greenMin) / greenDivide);
                blueIndex = (int) ((blueBuffer[i] - blueMin) / blueDivide);
                featureSpace[(blueIndex * redGreen) + (greenIndex * redBins) + redIndex]++;
            } // for (i = 0; i < totLength; i++)

            // Smooth feature space
            smoothExtents = new int[3];
            smoothExtents[0] = redBins;
            smoothExtents[1] = greenBins;
            smoothExtents[2] = blueBins;
            smoothImage = new ModelImage(ModelImage.FLOAT, smoothExtents, "smoothedHistogram");
            smoothImage.importData(0, featureSpace, true);

            // Create a 3 by 3 by 3 smoothing kernel
            smoothSigmas = new float[3];
            smoothSigmas[0] = 0.6f;
            smoothSigmas[1] = 0.6f;
            smoothSigmas[2] = 0.6f;
            gaussAlgo = new AlgorithmGaussianBlurSep(smoothImage, smoothSigmas, true, false);
            gaussAlgo.run();
            gaussAlgo.finalize();
            gaussAlgo = null;
            smoothImage.exportData(0, nBins, featureSpace);
            smoothImage.disposeLocal();
            smoothImage = null;

            // Normalize feature space
            total = featureSpace[0];

            for (i = 1; i < featureSpace.length; i++) {
                total += featureSpace[i];
            }

            for (i = 0; i < featureSpace.length; i++) {
                featureSpace[i] /= total;
            }
            /*featureImage = new ModelImage(ModelImage.FLOAT,smoothExtents,
             *                            "smoothedHistogram",
             * srcImage.getUserInterface());featureImage.importData(0, featureSpace, true);*/


            // Max-shift procedure;
            // Initialize label function to 0
            label = new short[nBins];
            path = new int[nBins];

            for (i = 0, x = 0, y = 0, z = 0; i < nBins; i++) {
                fireProgressStateChanged(i * 100 / (2 * nBins));

                if (label[i] == 0) {
                    pathIndex = 0;
                    Arrays.fill(path, 0);
                    path[pathIndex++] = i;
                    nextIndex = i;
                    newDensity = featureSpace[i];
                    oldDensity = -1.0f;
                    positiveLabel = false;
                    xp = x;
                    yp = y;
                    zp = z;

                    while ((newDensity > oldDensity) && (!positiveLabel)) {
                        oldDensity = newDensity;
                        zmin = zp - 2;

                        if (zmin < 0) {
                            zmin = 0;
                        }

                        zmax = zp + 2;

                        if (zmax > (blueBins - 1)) {
                            zmax = blueBins - 1;
                        }

                        ymin = yp - 2;

                        if (ymin < 0) {
                            ymin = 0;
                        }

                        ymax = yp + 2;

                        if (ymax > (greenBins - 1)) {
                            ymax = greenBins - 1;
                        }

                        xmin = xp - 2;

                        if (xmin < 0) {
                            xmin = 0;
                        }

                        xmax = xp + 2;

                        if (xmax > (redBins - 1)) {
                            xmax = redBins - 1;
                        }

                        for (z1 = zmin, k1 = zmin * redGreen; z1 <= zmax; z1++, k1 += redGreen) {

                            for (y1 = ymin, j1 = k1 + (ymin * redBins); y1 <= ymax; y1++, j1 += redBins) {

                                for (x1 = xmin, i1 = j1 + xmin; x1 <= xmax; x1++, i1++) {
                                    density1 = featureSpace[i1];

                                    if (density1 > newDensity) {
                                        newDensity = density1;
                                        nextIndex = i1;
                                        xp = x1;
                                        yp = y1;
                                        zp = z1;
                                    }
                                } // for (x1 = xmin, i1 = j1 + xmin; x1 <= xmax; x1++, i1++)
                            } // for (y1 = ymin, j1 = k1 + ymin*redBins; y1 <= ymax; y1++, j1 += redBins)
                        } // for (z1 = zmin, k1 = zmin*redGreen; z1 <= zmax; z1++, k1 += redGreen)

                        if (label[nextIndex] != 0) {
                            positiveLabel = true;
                        } else if (newDensity > oldDensity) {
                            path[pathIndex++] = nextIndex;
                        }
                    } // while ((newDensity > oldDensity) && (!positiveLabel))

                    if (positiveLabel) {

                        for (j = 0; j < pathIndex; j++) {
                            label[path[j]] = label[nextIndex];
                        }
                    } // if (positiveLabel)
                    else { // !positiveLabel
                        labelNum++;

                        for (j = 0; j < pathIndex; j++) {
                            label[path[j]] = labelNum;
                        }
                    } // else !positiveLabel

                    x++;

                    if (x == redBins) {
                        x = 0;
                        y++;

                        if (y == greenBins) {
                            y = 0;
                            z++;
                        } // if (y == greenBins)
                    } // if (x == redBins)
                } // if (label[i] == 0)
            } // for (i = 0, x= 0, y = 0, z = 0; i < nBins; i++)

            VOIs = srcImage.getVOIs();
            nVOIs = VOIs.size();
            nPoints = 0;

            for (i = 0; i < nVOIs; i++) {
                points = VOIs.VOIAt(i).exportAllPoints();

                if (points != null) {
                    nPoints += points.length;
                }
            } // for (i = 0; i < nVOIs; i++)

            if (nPoints != 0) {
                principalLabel = new short[nPoints];

                for (i = 0, k = 0; i < nVOIs; i++) {
                    points = VOIs.VOIAt(i).exportAllPoints();

                    if (points != null) {

                        for (j = 0; j < points.length; j++) {
                            x = (int) points[j].X;
                            y = (int) points[j].Y;
                            z = (int) points[j].Z;
                            index = (z * sliceLength) + (y * xDim) + x;
                            redIndex = (int) ((redBuffer[index] - redMin) / redDivide);
                            greenIndex = (int) ((greenBuffer[index] - greenMin) / greenDivide);
                            blueIndex = (int) ((blueBuffer[index] - blueMin) / blueDivide);
                            principalLabel[k++] = label[(blueIndex * redGreen) + (greenIndex * redBins) + redIndex];
                        }
                    }
                } // for (i = 0, k = 0; i < nVOIs; i++)

                // Find the mean point of each label from 1 to labelNum in feature space
                meanx = new float[labelNum];
                meany = new float[labelNum];
                meanz = new float[labelNum];
                labelCount = new int[labelNum];

                for (i = 0, x = 0, y = 0, z = 0; i < nBins; i++) {
                    meanx[label[i] - 1] += x;
                    meany[label[i] - 1] += y;
                    meanz[label[i] - 1] += z;
                    labelCount[label[i] - 1]++;
                    x++;

                    if (x == redBins) {
                        x = 0;
                        y++;

                        if (y == greenBins) {
                            y = 0;
                            z++;
                        }
                    } // if (x == redBins)
                } // for (i = 0, x = 0, y = 0, z = 0; i < nBins; i++)

                for (i = 0; i < labelNum; i++) {
                    meanx[i] /= labelCount[i];
                    meany[i] /= labelCount[i];
                    meanz[i] /= labelCount[i];
                }

                // Find the distance in feature space between the mean of the nonprincipal
                // cluster and the closest border point of a principal cluster
                label2 = new short[nBins];

                for (i = 0; i < nBins; i++) {

                    for (j = 0; j < nPoints; j++) {

                        if (label[i] == principalLabel[j]) {
                            label2[i] = (short) (j * 255 / nPoints);
                        }
                    }
                }

loop1:
                for (i = 0; i < labelNum; i++) {
                    fireProgressStateChanged(50 + (i * 100 / (2 * labelNum)));

                    for (j = 0; j < nPoints; j++) {

                        if (principalLabel[j] == (i + 1)) {
                            continue loop1;
                        } // if (principalLabel[j] == (i+1))
                    } // for (j = 0; j < nPoints; j++)

                    closestLabel = -1;
                    distance = Float.MAX_VALUE;

                    for (j = 0; j < nPoints; j++) {

                        for (k = 0, x = 0, y = 0, z = 0; k < nBins; k++) {

                            if (label[k] == principalLabel[j]) {
                                testDistance = ((x - meanx[i]) * (x - meanx[i])) + ((y - meany[i]) * (y - meany[i])) +
                                               ((z - meanz[i]) * (z - meanz[i]));

                                if (distance > testDistance) {
                                    distance = testDistance;
                                    closestLabel = (short) (j * 255 / nPoints);
                                }
                            } // if (label[k] == principalLabel[j])

                            x++;

                            if (x == redBins) {
                                x = 0;
                                y++;

                                if (y == greenBins) {
                                    y = 0;
                                    z++;
                                } // if (y == greenBins)
                            } // if (x == redBins)
                        } // for (k = 0, x = 0, y = 0, z = 0; k < nBins; k++)
                    } // for (j = 0; j < nPoints; j++)

                    for (j = 0; j < nBins; j++) {

                        if (label[j] == (i + 1)) {
                            label2[j] = closestLabel;
                        }
                    } // for (j = 0; j < nBins; j++)
                } // for (i = 0; i < labelNum; i++)
            } // if (nPoints != 0)

            segBuffer = new short[totLength];

            if (label2 != null) {

                for (i = 0; i < totLength; i++) {
                    redIndex = (int) ((redBuffer[i] - redMin) / redDivide);
                    greenIndex = (int) ((greenBuffer[i] - greenMin) / greenDivide);
                    blueIndex = (int) ((blueBuffer[i] - blueMin) / blueDivide);
                    segBuffer[i] = label2[(blueIndex * redGreen) + (greenIndex * redBins) + redIndex];
                }

            } // if (label2 != null)
            else {

                for (i = 0; i < totLength; i++) {
                    redIndex = (int) ((redBuffer[i] - redMin) / redDivide);
                    greenIndex = (int) ((greenBuffer[i] - greenMin) / greenDivide);
                    blueIndex = (int) ((blueBuffer[i] - blueMin) / blueDivide);
                    segBuffer[i] = label[(blueIndex * redGreen) + (greenIndex * redBins) + redIndex];
                }
            }

            if (destImage != null) {
                destImage.importData(0, segBuffer, true);
            } else {
                srcImage.reallocate(ModelImage.SHORT);
                srcImage.importData(0, segBuffer, true);
            }

            setCompleted(true);

            return;
        } // try
        catch (IOException ioe) {
            finalize();
            System.gc();
            MipavUtil.displayError("Algorithm Nonparametric segmentation reports:\n" + ioe.toString());


            setCompleted(false);

            return;
        } catch (OutOfMemoryError error) {
            finalize();
            System.gc();
            MipavUtil.displayError("Algorithm Nonparametricsegmentation reports:\n" + error.toString());


            setCompleted(false);

            return;
        }

    }

    /**
     * DOCUMENT ME!
     */
    private void segmentBW() {
        int xDim;
        int yDim;
        int zDim;
        int sliceLength;
        int totLength;
        float imin;
        float imax;
        float[] lapSigmas;
        ModelImage image2;
        String lapName;
        float[] buffer;
        float[] buffer2;
        short[] segBuffer;
        int z;
        AlgorithmLaplacian lapAlgo = null;
        boolean image25D = true;
        float ampFactor = 1.0f;
        boolean wholeImage = true;
        float lapMin;
        float lapMax;
        int lapBins = 128;
        float lapScale;
        int i;
        int j;
        float lapNum;
        int lapLow;
        int lapHigh;
        int iNum;
        int iBins = 128;
        int iDivide = 1;
        int iIndex;
        int nBins;
        float[] featureSpace;
        int[] smoothExtents;
        ModelImage smoothImage;
        float[] smoothSigmas;
        AlgorithmGaussianBlurSep gaussAlgo;
        float total;
        short[] label;
        int[] path;
        int x;
        int y;
        int pathIndex;
        int nextIndex;
        float newDensity;
        float oldDensity;
        boolean positiveLabel;
        int xp;
        int yp;
        int ymin;
        int ymax;
        int xmin;
        int xmax;
        int i1;
        int j1;
        int x1;
        int y1;
        float density1;
        short labelNum = 0;
        ViewVOIVector VOIs = null;
        int nVOIs;
        int nPoints;
        Vector3f[] points;
        int index;
        short[] principalLabel;
        int k;
        float[] meanx;
        float[] meany;
        int[] labelCount;
        short[] label2 = null;
        float distance;
        float testDistance;
        short closestLabel;
        int lapIndex;


        try {
            

            fireProgressStateChanged(srcImage.getImageName(), "Performing Nonparametric segmentation...");


            imin = (float) srcImage.getMin();
            imax = (float) srcImage.getMax();
            iNum = (int) Math.round(imax - imin + 1);

            // A maximum of 128 intensity bins
            if (iNum <= 128) {
                iBins = iNum;
            } else {
                iDivide = iNum / 128;

                if ((iNum % 128) > 0) {
                    iDivide++;
                }

                iBins = iNum / iDivide;

                if ((iNum % iDivide) > 0) {
                    iBins++;
                }
            }

            xDim = srcImage.getExtents()[0];
            yDim = srcImage.getExtents()[1];

            if (srcImage.getNDims() > 2) {
                zDim = srcImage.getExtents()[2];
            } else {
                zDim = 1;
            }

            sliceLength = xDim * yDim;
            totLength = sliceLength * zDim;

            lapSigmas = new float[2];

            // This will make the Laplacian operator a 3 by 3 kernel
            lapSigmas[0] = 0.3f;
            lapSigmas[1] = 0.3f;
            lapName = srcImage.getImageName() + "_2";
            image2 = new ModelImage(ModelImage.FLOAT, srcImage.getExtents(), lapName);
            buffer = new float[totLength];
            buffer2 = new float[totLength];

            srcImage.exportData(0, totLength, buffer);

            // The Laplacian of the buffer will be placed in buffer2
            image2.importData(0, buffer, true);

            lapAlgo = new AlgorithmLaplacian(image2, lapSigmas, wholeImage, image25D, ampFactor);
            lapAlgo.run();
            lapAlgo.finalize();
            lapAlgo = null;
            image2.exportData(0, totLength, buffer2);
            lapMin = (float) image2.getMin();
            lapMax = (float) image2.getMax();

            lapScale = (lapBins - 1) / (lapMax - lapMin);

            // Create 2D feature space
            nBins = iBins * lapBins;
            featureSpace = new float[nBins];

            // Using partial intensity interpolation, split each
            // Laplacian count between 2 Laplacian histogram bins.
            for (i = 0; i < totLength; i++) {
                iIndex = (int) ((buffer[i] - imin) / iDivide);
                lapNum = (buffer2[i] - lapMin) * lapScale;

                if (lapNum >= (lapBins - 1)) {
                    lapNum = lapBins - 1;
                }

                lapLow = (int) lapNum;
                lapHigh = lapLow + 1;
                featureSpace[(lapLow * iBins) + iIndex] += (lapHigh - lapNum);

                if (lapHigh <= (lapBins - 1)) {
                    featureSpace[(lapHigh * iBins) + iIndex] += (lapNum - lapLow);
                }
            } // for (i = 0; i < totLength; i++)

            // Smooth feature space
            smoothExtents = new int[2];
            smoothExtents[0] = iBins;
            smoothExtents[1] = lapBins;
            smoothImage = new ModelImage(ModelImage.FLOAT, smoothExtents, "smoothedHistogram");
            smoothImage.importData(0, featureSpace, true);

            // Create a 3 by 3 smoothing kernel
            smoothSigmas = new float[2];
            smoothSigmas[0] = 0.6f;
            smoothSigmas[1] = 0.6f;
            gaussAlgo = new AlgorithmGaussianBlurSep(smoothImage, smoothSigmas, true, false);
            gaussAlgo.run();
            gaussAlgo.finalize();
            gaussAlgo = null;
            smoothImage.exportData(0, nBins, featureSpace);
            smoothImage.disposeLocal();
            smoothImage = null;

            // Normalize feature space
            total = featureSpace[0];

            for (i = 1; i < featureSpace.length; i++) {
                total += featureSpace[i];
            }

            for (i = 0; i < featureSpace.length; i++) {
                featureSpace[i] /= total;
            }
            /*featureImage = new ModelImage(ModelImage.FLOAT,smoothExtents,
             *                            "smoothedHistogram",
             * srcImage.getUserInterface());featureImage.importData(0, featureSpace, true);*/


            // Max-shift procedure;
            // Initialize label function to 0
            label = new short[nBins];
            path = new int[nBins];

            for (i = 0, x = 0, y = 0; i < nBins; i++) {
                fireProgressStateChanged(i * 100 / (2 * nBins));

                if (label[i] == 0) {
                    pathIndex = 0;
                    Arrays.fill(path, 0);
                    path[pathIndex++] = i;
                    nextIndex = i;
                    newDensity = featureSpace[i];
                    oldDensity = -1.0f;
                    positiveLabel = false;
                    xp = x;
                    yp = y;

                    while ((newDensity > oldDensity) && (!positiveLabel)) {
                        oldDensity = newDensity;
                        ymin = yp - 2;

                        if (ymin < 0) {
                            ymin = 0;
                        }

                        ymax = yp + 2;

                        if (ymax > (lapBins - 1)) {
                            ymax = lapBins - 1;
                        }

                        xmin = xp - 2;

                        if (xmin < 0) {
                            xmin = 0;
                        }

                        xmax = xp + 2;

                        if (xmax > (iBins - 1)) {
                            xmax = iBins - 1;
                        }

                        for (y1 = ymin, j1 = ymin * iBins; y1 <= ymax; y1++, j1 += iBins) {

                            for (x1 = xmin, i1 = j1 + xmin; x1 <= xmax; x1++, i1++) {
                                density1 = featureSpace[i1];

                                if (density1 > newDensity) {
                                    newDensity = density1;
                                    nextIndex = i1;
                                    xp = x1;
                                    yp = y1;
                                }
                            } // for (x1 = xmin, i1 = j1 + xmin; x1 <= xmax; x1++, i1++)
                        } // for (y1 = ymin, j1 = ymin*iBins; y1 <= ymax; y1++, j1 += iBins)

                        if (label[nextIndex] != 0) {
                            positiveLabel = true;
                        } else if (newDensity > oldDensity) {
                            path[pathIndex++] = nextIndex;
                        }
                    } // while ((newDensity > oldDensity) && (!positiveLabel))

                    if (positiveLabel) {

                        for (j = 0; j < pathIndex; j++) {
                            label[path[j]] = label[nextIndex];
                        }
                    } // if (positiveLabel)
                    else { // !positiveLabel
                        labelNum++;

                        for (j = 0; j < pathIndex; j++) {
                            label[path[j]] = labelNum;
                        }
                    } // else !positiveLabel

                    x++;

                    if (x == iBins) {
                        x = 0;
                        y++;
                    } // if (x == iBins)
                } // if (label[i] == 0)
            } // for (i = 0, x= 0, y = 0; i < nBins; i++)

            VOIs = srcImage.getVOIs();
            nVOIs = VOIs.size();
            nPoints = 0;

            for (i = 0; i < nVOIs; i++) {
                points = VOIs.VOIAt(i).exportAllPoints();

                if (points != null) {
                    nPoints += points.length;
                }
            } // for (i = 0; i < nVOIs; i++)

            if (nPoints != 0) {
                principalLabel = new short[nPoints];

                for (i = 0, k = 0; i < nVOIs; i++) {
                    points = VOIs.VOIAt(i).exportAllPoints();

                    if (points != null) {

                        for (j = 0; j < points.length; j++) {
                            x = (int) points[j].X;
                            y = (int) points[j].Y;
                            z = (int) points[j].Z;
                            index = (z * sliceLength) + (y * xDim) + x;
                            iIndex = (int) ((buffer[index] - imin) / iDivide);
                            lapIndex = (int) ((buffer2[index] - lapMin) * lapScale);

                            if (lapIndex >= (lapBins - 1)) {
                                lapIndex = lapBins - 1;
                            }

                            principalLabel[k++] = label[(lapIndex * iBins) + iIndex];
                        }
                    }
                } // for (i = 0, k = 0; i < nVOIs; i++)

                // Find the mean point of each label from 1 to labelNum in feature space
                meanx = new float[labelNum];
                meany = new float[labelNum];
                labelCount = new int[labelNum];

                for (i = 0, x = 0, y = 0; i < nBins; i++) {
                    meanx[label[i] - 1] += x;
                    meany[label[i] - 1] += y;
                    labelCount[label[i] - 1]++;
                    x++;

                    if (x == iBins) {
                        x = 0;
                        y++;
                    }
                } // for (i = 0, x = 0, y = 0; i < nBins; i++)

                for (i = 0; i < labelNum; i++) {
                    meanx[i] /= labelCount[i];
                    meany[i] /= labelCount[i];
                }

                // Find the distance in feature space between the mean of the nonprincipal
                // cluster and the closest border point of a principal cluster
                label2 = new short[nBins];

                for (i = 0; i < nBins; i++) {

                    for (j = 0; j < nPoints; j++) {

                        if (label[i] == principalLabel[j]) {
                            label2[i] = (short) (j * 255 / nPoints);
                        }
                    }
                }

loop1:
                for (i = 0; i < labelNum; i++) {
                    fireProgressStateChanged(50 + (i * 100 / (2 * labelNum)));

                    for (j = 0; j < nPoints; j++) {

                        if (principalLabel[j] == (i + 1)) {
                            continue loop1;
                        } // if (principalLabel[j] == (i+1))
                    } // for (j = 0; j < nPoints; j++)

                    closestLabel = -1;
                    distance = Float.MAX_VALUE;

                    for (j = 0; j < nPoints; j++) {

                        for (k = 0, x = 0, y = 0; k < nBins; k++) {

                            if (label[k] == principalLabel[j]) {
                                testDistance = ((x - meanx[i]) * (x - meanx[i])) + ((y - meany[i]) * (y - meany[i]));

                                if (distance > testDistance) {
                                    distance = testDistance;
                                    closestLabel = (short) (j * 255 / nPoints);
                                }
                            } // if (label[k] == principalLabel[j])

                            x++;

                            if (x == iBins) {
                                x = 0;
                                y++;
                            } // if (x == iBins)
                        } // for (k = 0, x = 0, y = 0; k < nBins; k++)
                    } // for (j = 0; j < nPoints; j++)

                    for (j = 0; j < nBins; j++) {

                        if (label[j] == (i + 1)) {
                            label2[j] = closestLabel;
                        }
                    } // for (j = 0; j < nBins; j++)
                } // for (i = 0; i < labelNum; i++)

            } // if (nPoints != 0)

            segBuffer = new short[totLength];

            if (label2 != null) {

                for (i = 0; i < totLength; i++) {
                    iIndex = (int) ((buffer[i] - imin) / iDivide);
                    lapIndex = (int) ((buffer2[i] - lapMin) * lapScale);

                    if (lapIndex >= (lapBins - 1)) {
                        lapIndex = lapBins - 1;
                    }

                    segBuffer[i] = label2[(lapIndex * iBins) + iIndex];
                }

            } // if (label2 != null)
            else {

                for (i = 0; i < totLength; i++) {
                    iIndex = (int) ((buffer[i] - imin) / iDivide);
                    lapIndex = (int) ((buffer2[i] - lapMin) * lapScale);

                    if (lapIndex >= (lapBins - 1)) {
                        lapIndex = lapBins - 1;
                    }

                    segBuffer[i] = label[(lapIndex * iBins) + iIndex];
                }
            }

            if (destImage != null) {
                destImage.importData(0, segBuffer, true);
            } else {
                srcImage.reallocate(ModelImage.SHORT);
                srcImage.importData(0, segBuffer, true);
            }

            setCompleted(true);

            return;
        } // try
        catch (IOException ioe) {
            finalize();
            System.gc();
            MipavUtil.displayError("Algorithm Nonparametric segmentation reports:\n" + ioe.toString());

            setCompleted(false);

            return;
        } catch (OutOfMemoryError error) {
            finalize();
            System.gc();
            MipavUtil.displayError("Algorithm Nonparametricsegmentation reports:\n" + error.toString());


            setCompleted(false);

            return;
        }

    }


}
