package gov.nih.mipav.model.algorithms;


import gov.nih.mipav.model.structures.*;

import gov.nih.mipav.view.*;
import gov.nih.mipav.view.dialogs.RegionGrowDialog;

import java.awt.Point;
import java.io.IOException;
import java.util.BitSet;


/**
 * Fills a region based on average intensity +or- standard deviation. A VOI is used to calc an intensity within the
 * region. The region grows if the intensity is bounded by average +/- the standard deviation. The a new VOI should be
 * generated but for now the image is filled with with a flood value.
 * 
 * <p>
 * If variableThresholds is true,
 * upperDelta = initialValue of upperBound - value of seed point lowerDelta = Value of seed point - initialValue of
 * lowerBound Each time a new pixel is added to the region recalculate the mean and standard deviation of the region
 * mean = (sum of the pixel values)/(pixel number) standard deviation = ((Sum of the squared values - (sum of
 * values)*(sum of values)/(pixel number))/(pixel number) Then recalculate upperBound and lowBound as: upperBound =
 * initialValue + (1.0 - Math.min(0.8, stdDev/mean)) * upperDelta lowBound = initialValue - (1.0 - Math.min(0.8,
 * stdDev/mean)) * lowerDelta upperBound cannot become higher than its initial value, but it can become lower than its
 * initial value. lowBound cannot become lower than its initial value, but it can become higher than its initial value.
 * That is, upperBound - lowBound cannot exceed its initial value, but it can become lower than its initial value.
 * Adaptive thresholding is useful in preventing bleeding across smooth image gradients. Reference: Automatic
 * Image-To-Map-Registration of Remote Sensing Data by Heiner Hild, Photogrammetric Week '01', D. Fritsch & R. Spiller,
 * Eds., Wichmann Verlag, Heidelberg, 2001.
 * </p>
 * 
 * <p>

 * 
 * @version 0.5 Jan, 2000
 * @author Matthew J. McAuliffe, Ph.D.
 */
public class AlgorithmRegionGrow extends AlgorithmBase {

    // ~ Instance fields
    // ------------------------------------------------------------------------------------------------

    /** DOCUMENT ME! */
    private float[] blueBuffer = null;

    /** DOCUMENT ME! */
    private double c;

    /** DOCUMENT ME! */
    private float[] greenBuffer = null;

    /** DOCUMENT ME! */
    private RegionGrowDialog growDialog = null;

    @SuppressWarnings("unused")
    private ViewJFrameImage imageFrame = null;

    /** DOCUMENT ME! */
    private double mMean;

    /** DOCUMENT ME! */
    private double mRMean, mGMean, mBMean;

    /** DOCUMENT ME! */
    private BitSet paintMask = null;

    /** DOCUMENT ME! */
    private float[] redBuffer = null;

    /** DOCUMENT ME! */
    private ModelImage srcImage;

    /** DOCUMENT ME! */
    private double[][] varInverse = null;

    // ~ Constructors
    // ---------------------------------------------------------------------------------------------------

    /**
     * Creates a new AlgorithmRegionGrow object.
     * 
     * @param srcImg source image model
     * @param _multiplier aaaaa
     * @param _floodValue aaaaa
     */
    public AlgorithmRegionGrow(final ModelImage srcImg, final float _multiplier, final float _floodValue) {

        destImage = null; // Calc in place
        srcImage = srcImg;

    }

    /**
     * Creates a new AlgorithmRegionGrow object.
     * 
     * @param destImg image model where result image is to stored
     * @param srcImg source image model
     * @param _multiplier placeholder
     * @param _floodValue placeholder
     */
    public AlgorithmRegionGrow(final ModelImage destImg, final ModelImage srcImg, final float _multiplier,
            final float _floodValue) {

        destImage = destImg; // Put results in destination image.
        srcImage = srcImg;

    }

    // ~ Methods
    // --------------------------------------------------------------------------------------------------------

    /**
     * Prepares this class for destruction.
     */
    public void finalize() {
        int i;

        destImage = null;
        srcImage = null;
        redBuffer = null;
        greenBuffer = null;
        blueBuffer = null;

        if (varInverse != null) {

            for (i = 0; i < varInverse.length; i++) {
                varInverse[i] = null;
            }

            varInverse = null;
        }

        super.finalize();
    }

    /**
     * 2D flood fill that forms a bitset(boolean) mask.
     * 
     * @param paintMask mask used to indicated where region has grown
     * @param seedPt seed point for flood fill
     * @param th
     * @param useVOI use selected VOI for initial variance
     * @param dp
     * @param growDialog the RegionGrowDialog
     * @param lowBound lower bound of values which are included in the region
     * @param upperBound upper bound of values which are included in the region
     * @param sizeLimit stop region grow when objects exceeds size limit in pixels
     * @param maxDistance max distance from the seed point (in pixels) that the region is allowed to grow.
     * @param variableThresholds If true vary thresholds as region grows
     * 
     * @return returns the area region
     */
    public int regionGrow2D(final BitSet paintMask, final Point seedPt, float th, boolean useVOI,
            final boolean dp, final RegionGrowDialog growDialog, float lowBound, float upperBound,
            final int sizeLimit, final float maxDistance, final boolean variableThresholds) {

        final int xDim = srcImage.getExtents()[0];
        final int yDim = srcImage.getExtents()[1];

        IntVector stack = null;
        float val;
        int i, idx, j;
        final int imageSize = xDim * yDim;
        int x, y;
        int count = 0;
        final int initIndex = (seedPt.y * xDim) + seedPt.x;
        final int xInit = seedPt.x;
        final int yInit = seedPt.y;
        double distance = -2;
        int current;
        int fmax;
        float activeThreshold;
        double imageMean;
        double diff;
        double totalVariance;
        double objectVariance;
        boolean[] havePushed = null;
        double imageRMean, imageGMean, imageBMean;
        double[][] totalVar = null;
        double[][] objectVar = null;
        double diffR;
        double diffG;
        double diffB;
        double varDet;
        ViewVOIVector VOIs = null;
        int nVOI;
        BitSet mask = null;
        int selectedContours;
        int iSel = 0;
        int voiSize = 0;
        float initialValue;
        float upperDelta;
        float lowerDelta;
        double sum = 0.0;
        double mean;
        double sumOfSquares = 0.0;
        double stdDev = 0.0;

        double resolX = srcImage.getFileInfo(0).getResolutions()[0];
        double resolY = srcImage.getFileInfo(0).getResolutions()[1];

        if (resolX <= 0) {
            resolX = 1;
        }

        if (resolY <= 0) {
            resolY = 1;
        }

        final double volRes = resolX * resolY;

        final double resX = resolX * resolX;
        final double resY = resolY * resolY;

        this.paintMask = paintMask;
        this.growDialog = growDialog;

        try {

            if (variableThresholds) {
                initialValue = srcImage.getFloat(initIndex);
                upperDelta = upperBound - initialValue;
                lowerDelta = initialValue - lowBound;
                stack = new IntVector(imageSize / 2, imageSize / 8);
                stack.push(initIndex);

                if (paintMask.get(initIndex) == false) {
                    count++;
                    paintMask.set(initIndex);
                    sum = initialValue;
                    sumOfSquares = initialValue * initialValue;
                }

                while ( !stack.isEmpty()) {

                    // i = stack.popLastIn();
                    i = stack.popFirstIn();
                    x = i % xDim;
                    y = (i % imageSize) / xDim;

                    if ( (x + 1) < xDim) {
                        idx = i + 1;
                        val = srcImage.getFloat(idx);

                        if (maxDistance > 0) {
                            distance = Math.sqrt( ( (x - xInit) * (x - xInit) * resX)
                                    + ( (y - yInit) * (y - yInit) * resY));
                        }

                        if ( (paintMask.get(idx) == false) && (val >= lowBound) && (val <= upperBound)
                                && (distance <= maxDistance)) {
                            stack.push(idx);
                            paintMask.set(idx);
                            count++;
                            sum += val;
                            sumOfSquares += val * val;
                            mean = sum / count;
                            stdDev = Math.sqrt( (sumOfSquares - (sum * sum / count)) / count);
                            upperBound = (float) (initialValue + ( (1.0 - Math.min(0.8, stdDev / mean)) * upperDelta));
                            lowBound = (float) (initialValue - ( (1.0 - Math.min(0.8, stdDev / mean)) * lowerDelta));

                            if ( (sizeLimit != -1) && ( ( (count + 1) * volRes) > sizeLimit)) {
                                break;
                            }
                        }
                    }

                    if ( (x - 1) >= 0) {
                        idx = i - 1;
                        val = srcImage.getFloat(idx);

                        if (maxDistance > 0) {
                            distance = Math.sqrt( ( (x - xInit) * (x - xInit) * resX)
                                    + ( (y - yInit) * (y - yInit) * resY));
                        }

                        if ( (paintMask.get(idx) == false) && (val >= lowBound) && (val <= upperBound)
                                && (distance <= maxDistance)) {
                            stack.push(idx);
                            paintMask.set(idx);
                            count++;
                            sum += val;
                            sumOfSquares += val * val;
                            mean = sum / count;
                            stdDev = Math.sqrt( (sumOfSquares - (sum * sum / count)) / count);
                            upperBound = (float) (initialValue + ( (1.0 - Math.min(0.8, stdDev / mean)) * upperDelta));
                            lowBound = (float) (initialValue - ( (1.0 - Math.min(0.8, stdDev / mean)) * lowerDelta));

                            if ( (sizeLimit != -1) && ( ( (count + 1) * volRes) > sizeLimit)) {
                                break;
                            }
                        }
                    }

                    if ( (y + 1) < yDim) {
                        idx = i + xDim;
                        val = srcImage.getFloat(idx);

                        if (maxDistance > 0) {
                            distance = Math.sqrt( ( (x - xInit) * (x - xInit) * resX)
                                    + ( (y - yInit) * (y - yInit) * resY));
                        }

                        if ( (paintMask.get(idx) == false) && (val >= lowBound) && (val <= upperBound)
                                && (distance <= maxDistance)) {
                            stack.push(idx);
                            paintMask.set(idx);
                            count++;
                            sum += val;
                            sumOfSquares += val * val;
                            mean = sum / count;
                            stdDev = Math.sqrt( (sumOfSquares - (sum * sum / count)) / count);
                            upperBound = (float) (initialValue + ( (1.0 - Math.min(0.8, stdDev / mean)) * upperDelta));
                            lowBound = (float) (initialValue - ( (1.0 - Math.min(0.8, stdDev / mean)) * lowerDelta));

                            if ( (sizeLimit != -1) && ( ( (count + 1) * volRes) > sizeLimit)) {
                                break;
                            }
                        }
                    }

                    if ( (y - 1) >= 0) {
                        idx = i - xDim;
                        val = srcImage.getFloat(idx);

                        if (maxDistance > 0) {
                            distance = Math.sqrt( ( (x - xInit) * (x - xInit) * resX)
                                    + ( (y - yInit) * (y - yInit) * resY));
                        }

                        if ( (paintMask.get(idx) == false) && (val >= lowBound) && (val <= upperBound)
                                && (distance <= maxDistance)) {
                            stack.push(idx);
                            paintMask.set(idx);
                            count++;
                            sum += val;
                            sumOfSquares += val * val;
                            mean = sum / count;
                            stdDev = Math.sqrt( (sumOfSquares - (sum * sum / count)) / count);
                            upperBound = (float) (initialValue + ( (1.0 - Math.min(0.8, stdDev / mean)) * upperDelta));
                            lowBound = (float) (initialValue - ( (1.0 - Math.min(0.8, stdDev / mean)) * lowerDelta));

                            if ( (sizeLimit != -1) && ( ( (count + 1) * volRes) > sizeLimit)) {
                                break;
                            }
                        }
                    }
                } // while ( !stack.isEmpty() )
            } // if (variableThresholds)
            else {
                stack = new IntVector(imageSize / 2, imageSize / 8);
                stack.push(initIndex);

                if (paintMask.get(initIndex) == false) {
                    count++;
                    paintMask.set(initIndex);
                }

                while ( !stack.isEmpty()) {

                    // i = stack.popLastIn();
                    i = stack.popFirstIn();
                    x = i % xDim;
                    y = (i % imageSize) / xDim;

                    if ( (x + 1) < xDim) {
                        idx = i + 1;
                        val = srcImage.getFloat(idx);

                        if (maxDistance > 0) {
                            distance = Math.sqrt( ( (x - xInit) * (x - xInit) * resX)
                                    + ( (y - yInit) * (y - yInit) * resY));
                        }

                        if ( (paintMask.get(idx) == false) && (val >= lowBound) && (val <= upperBound)
                                && (distance <= maxDistance)) {
                            stack.push(idx);
                            paintMask.set(idx);
                            count++;

                            if ( (sizeLimit != -1) && ( ( (count + 1) * volRes) > sizeLimit)) {
                                break;
                            }
                        }
                    }

                    if ( (x - 1) >= 0) {
                        idx = i - 1;
                        val = srcImage.getFloat(idx);

                        if (maxDistance > 0) {
                            distance = Math.sqrt( ( (x - xInit) * (x - xInit) * resX)
                                    + ( (y - yInit) * (y - yInit) * resY));
                        }

                        if ( (paintMask.get(idx) == false) && (val >= lowBound) && (val <= upperBound)
                                && (distance <= maxDistance)) {
                            stack.push(idx);
                            paintMask.set(idx);
                            count++;

                            if ( (sizeLimit != -1) && ( ( (count + 1) * volRes) > sizeLimit)) {
                                break;
                            }
                        }
                    }

                    if ( (y + 1) < yDim) {
                        idx = i + xDim;
                        val = srcImage.getFloat(idx);

                        if (maxDistance > 0) {
                            distance = Math.sqrt( ( (x - xInit) * (x - xInit) * resX)
                                    + ( (y - yInit) * (y - yInit) * resY));
                        }

                        if ( (paintMask.get(idx) == false) && (val >= lowBound) && (val <= upperBound)
                                && (distance <= maxDistance)) {
                            stack.push(idx);
                            paintMask.set(idx);
                            count++;

                            if ( (sizeLimit != -1) && ( ( (count + 1) * volRes) > sizeLimit)) {
                                break;
                            }
                        }
                    }

                    if ( (y - 1) >= 0) {
                        idx = i - xDim;
                        val = srcImage.getFloat(idx);

                        if (maxDistance > 0) {
                            distance = Math.sqrt( ( (x - xInit) * (x - xInit) * resX)
                                    + ( (y - yInit) * (y - yInit) * resY));
                        }

                        if ( (paintMask.get(idx) == false) && (val >= lowBound) && (val <= upperBound)
                                && (distance <= maxDistance)) {
                            stack.push(idx);
                            paintMask.set(idx);
                            count++;

                            if ( (sizeLimit != -1) && ( ( (count + 1) * volRes) > sizeLimit)) {
                                break;
                            }
                        }
                    }
                } // while ( !stack.isEmpty() )
            } 
           
        } catch (final OutOfMemoryError e) {
            stack = null;
            System.gc();
            displayError("Algorithm RegionGrow:  Out of memory");
            setCompleted(false);

            return 0;
        }

        setCompleted(true);
        stack = null;
        System.gc();

        return count;
    }

    /**
     * 2D flood fill for color images that forms a bitset(boolean) mask.
     * 
     * @param paintMask mask used to indicated where region has grown
     * @param seedPt seed point for flood fill
     * @param th
     * @param useVOI use selected VOI for initial variance
     * @param dp
     * @param growDialog the RegionGrowDialog
     * @param lowBoundR lower bound of red values which are included in the region
     * @param upperBoundR upper bound of red values which are included in the region
     * @param lowBoundG lower bound of green values which are included in the region
     * @param upperBoundG upper bound of green values which are included in the region
     * @param lowBoundB lower bound of blue values which are included in the region
     * @param upperBoundB upper bound of blue values which are included in the region
     * @param sizeLimit stop region grow when objects exceeds size limit in pixels
     * @param maxDistance max distance from the seed point (in pixels) that the region is allowed to grow.
     * 
     * @return returns the area region
     */
    public int regionGrow2D(final BitSet paintMask, final Point seedPt, float th, boolean useVOI,
            final boolean dp, final RegionGrowDialog growDialog, final float lowBoundR,
            final float upperBoundR, final float lowBoundG, final float upperBoundG, final float lowBoundB,
            final float upperBoundB, final int sizeLimit, final float maxDistance) {

        final int xDim = srcImage.getExtents()[0];
        final int yDim = srcImage.getExtents()[1];

        IntVector stack = null;
        float valR;
        float valG;
        float valB;
        int i, idx, j;
        final int imageSize = xDim * yDim;
        int x, y;
        int count = 0;
        final int initIndex = (seedPt.y * xDim) + seedPt.x;
        final int xInit = seedPt.x;
        final int yInit = seedPt.y;
        double distance = -2;
        int current;
        int fmax;
        float activeThreshold;
        boolean[] havePushed = null;
        double imageRMean, imageGMean, imageBMean;
        double[][] totalVar = null;
        double[][] objectVar = null;
        double diffR;
        double diffG;
        double diffB;
        double varDet;
        ViewVOIVector VOIs = null;
        int nVOI;
        BitSet mask = null;
        int selectedContours;
        int iSel = 0;
        int voiSize = 0;

        double resolX = srcImage.getFileInfo(0).getResolutions()[0];
        double resolY = srcImage.getFileInfo(0).getResolutions()[1];

        if (resolX <= 0) {
            resolX = 1;
        }

        if (resolY <= 0) {
            resolY = 1;
        }

        final double volRes = resolX * resolY;

        final double resX = resolX * resolX;
        final double resY = resolY * resolY;

        this.paintMask = paintMask;
        this.growDialog = growDialog;

        try {

            stack = new IntVector(imageSize / 2, imageSize / 8);
            stack.push(initIndex);

            if (paintMask.get(initIndex) == false) {
                count++;
                paintMask.set(initIndex);
            }

            while ( !stack.isEmpty()) {

                // i = stack.popLastIn();
                i = stack.popFirstIn();
                x = i % xDim;
                y = (i % imageSize) / xDim;

                if ( (x + 1) < xDim) {
                    idx = i + 1;
                    valR = srcImage.getFloat( (4 * idx) + 1);
                    valG = srcImage.getFloat( (4 * idx) + 2);
                    valB = srcImage.getFloat( (4 * idx) + 3);

                    if (maxDistance > 0) {
                        distance = Math.sqrt( ( (x - xInit) * (x - xInit) * resX)
                                + ( (y - yInit) * (y - yInit) * resY));
                    }

                    if ( (paintMask.get(idx) == false) && (valR >= lowBoundR) && (valR <= upperBoundR)
                            && (valG >= lowBoundG) && (valG <= upperBoundG) && (valB >= lowBoundB)
                            && (valB <= upperBoundB) && (distance <= maxDistance)) {
                        stack.push(idx);
                        paintMask.set(idx);
                        count++;

                        if ( (sizeLimit != -1) && ( ( (count + 1) * volRes) > sizeLimit)) {
                            break;
                        }
                    }
                }

                if ( (x - 1) >= 0) {
                    idx = i - 1;
                    valR = srcImage.getFloat( (4 * idx) + 1);
                    valG = srcImage.getFloat( (4 * idx) + 2);
                    valB = srcImage.getFloat( (4 * idx) + 3);

                    if (maxDistance > 0) {
                        distance = Math.sqrt( ( (x - xInit) * (x - xInit) * resX)
                                + ( (y - yInit) * (y - yInit) * resY));
                    }

                    if ( (paintMask.get(idx) == false) && (valR >= lowBoundR) && (valR <= upperBoundR)
                            && (valG >= lowBoundG) && (valG <= upperBoundG) && (valB >= lowBoundB)
                            && (valB <= upperBoundB) && (distance <= maxDistance)) {
                        stack.push(idx);
                        paintMask.set(idx);
                        count++;

                        if ( (sizeLimit != -1) && ( ( (count + 1) * volRes) > sizeLimit)) {
                            break;
                        }
                    }
                }

                if ( (y + 1) < yDim) {
                    idx = i + xDim;
                    valR = srcImage.getFloat( (4 * idx) + 1);
                    valG = srcImage.getFloat( (4 * idx) + 2);
                    valB = srcImage.getFloat( (4 * idx) + 3);

                    if (maxDistance > 0) {
                        distance = Math.sqrt( ( (x - xInit) * (x - xInit) * resX)
                                + ( (y - yInit) * (y - yInit) * resY));
                    }

                    if ( (paintMask.get(idx) == false) && (valR >= lowBoundR) && (valR <= upperBoundR)
                            && (valG >= lowBoundG) && (valG <= upperBoundG) && (valB >= lowBoundB)
                            && (valB <= upperBoundB) && (distance <= maxDistance)) {
                        stack.push(idx);
                        paintMask.set(idx);
                        count++;

                        if ( (sizeLimit != -1) && ( ( (count + 1) * volRes) > sizeLimit)) {
                            break;
                        }
                    }
                }

                if ( (y - 1) >= 0) {
                    idx = i - xDim;
                    valR = srcImage.getFloat( (4 * idx) + 1);
                    valG = srcImage.getFloat( (4 * idx) + 2);
                    valB = srcImage.getFloat( (4 * idx) + 3);

                    if (maxDistance > 0) {
                        distance = Math.sqrt( ( (x - xInit) * (x - xInit) * resX)
                                + ( (y - yInit) * (y - yInit) * resY));
                    }

                    if ( (paintMask.get(idx) == false) && (valR >= lowBoundR) && (valR <= upperBoundR)
                            && (valG >= lowBoundG) && (valG <= upperBoundG) && (valB >= lowBoundB)
                            && (valB <= upperBoundB) && (distance <= maxDistance)) {
                        stack.push(idx);
                        paintMask.set(idx);
                        count++;

                        if ( (sizeLimit != -1) && ( ( (count + 1) * volRes) > sizeLimit)) {
                            break;
                        }
                    }
                }
            } // while ( !stack.isEmpty() )
            
        } catch (final OutOfMemoryError e) {
            stack = null;
            System.gc();
            displayError("Algorithm RegionGrow:  Out of memory");
            setCompleted(false);

            return 0;
        }

        setCompleted(true);
        stack = null;
        System.gc();

        return count;
    }

    /**
     * 3D flood fill that forms a bitset(boolean) mask.
     * 
     * @param paintMask mask used to indicated where region has grown
     * @param seedPt seed point for flood fill
     * @param th
     * @param useVOI use selected VOI for initial variance
     * @param dp
     * @param growDialog the RegionGrowDialog
     * @param lowBound lower bound of values which are included in the region
     * @param upperBound upper bound of values which are included in the region
     * @param sizeLimit stop region grow when objects exceeds size limit in pixels
     * @param maxDistance max distance from the seed point (in pixels) that the region is allowed to grow.
     * @param variableThresholds If true vary thresholds as region grows
     * @param timeSlice timeSlice that will be used in a 4D image
     * @param regionBounds DOCUMENT ME!
     * 
     * @return returns the volume region
     */
    public int regionGrow3D(final BitSet paintMask, final Point3D seedPt, float th, boolean useVOI,
            final boolean dp, final RegionGrowDialog growDialog, float lowBound, float upperBound,
            final int sizeLimit, final float maxDistance, final boolean variableThresholds, final int timeSlice,
            CubeBounds regionBounds) {

        final int xDim = srcImage.getExtents()[0];
        final int yDim = srcImage.getExtents()[1];
        final int zDim = srcImage.getExtents()[2];

        IntVector stack = null;
        float val;
        int i, idx, j;
        final int imageSize = xDim * yDim;
        int x, y, z;
        final int length = xDim * yDim * zDim;
        int count = 0;
        final int initIndex = (seedPt.z * (xDim * yDim)) + (seedPt.y * xDim) + seedPt.x;
        final int offset4D = timeSlice * length;
        final int xInit = seedPt.x;
        final int yInit = seedPt.y;
        final int zInit = seedPt.z;
        double distance = -2;
        int current;
        int fmax;
        float activeThreshold;
        double imageMean;
        double diff;
        double totalVariance;
        double objectVariance;
        boolean[] havePushed = null;
        double imageRMean, imageGMean, imageBMean;
        double[][] totalVar = null;
        double[][] objectVar = null;
        double diffR;
        double diffG;
        double diffB;
        double varDet;
        ViewVOIVector VOIs = null;
        int nVOI;
        BitSet mask = null;
        int selectedContours;
        int iSel = 0;
        int voiSize = 0;
        int round = 1;
        float initialValue;
        float upperDelta;
        float lowerDelta;
        double sum = 0.0;
        double mean;
        double sumOfSquares = 0.0;
        double stdDev = 0.0;

        if (regionBounds == null) {

            // set region bounds to image extents, if no bounds were specified
            regionBounds = new CubeBounds(xDim, 0, yDim, 0, zDim, 0);
        }

        if ( !regionBounds.contains(seedPt)) {
            return 0;
        }

        double resolX = srcImage.getFileInfo(0).getResolutions()[0];
        double resolY = srcImage.getFileInfo(0).getResolutions()[1];
        final double resolZ = srcImage.getFileInfo(0).getResolutions()[2];

        if (resolX <= 0) {
            resolX = 1;
        }

        if (resolY <= 0) {
            resolY = 1;
        }

        if (resolZ <= 0) {
            resolY = 1;
        }

        final double volRes = resolX * resolY * resolZ;

        final double resX = resolX * resolX;
        final double resY = resolY * resolY;
        final double resZ = resolZ * resolZ;

        this.paintMask = paintMask;
        this.growDialog = growDialog;

        int pCtr = 0;

        try {

            if (variableThresholds) {
                initialValue = srcImage.getFloat(initIndex);
                upperDelta = upperBound - initialValue;
                lowerDelta = initialValue - lowBound;
                stack = new IntVector(length / 3, length / 8);
                stack.push(initIndex);

                if (paintMask.get(initIndex) == false) {
                    paintMask.set(initIndex);
                    count++;
                    sum = initialValue;
                    sumOfSquares = initialValue * initialValue;
                }

                while ( !stack.isEmpty()) {

                    i = stack.popFirstIn();
                    x = i % xDim;
                    y = (i % imageSize) / xDim;
                    z = i / imageSize;

                    if ( (x + 1) < regionBounds.highX()) {
                        idx = i + 1;
                        val = srcImage.getFloat(idx + offset4D);

                        if (maxDistance > 0) {
                            distance = Math.sqrt( ( (x - xInit) * (x - xInit) * resX)
                                    + ( (y - yInit) * (y - yInit) * resY) + ( (z - zInit) * (z - zInit) * resZ));
                        }

                        if ( (paintMask.get(idx) == false) && (val >= lowBound) && (val <= upperBound)
                                && (distance <= maxDistance)) {
                            stack.push(idx);
                            paintMask.set(idx);
                            count++;
                            sum += val;
                            sumOfSquares += val * val;
                            mean = sum / count;
                            stdDev = Math.sqrt( (sumOfSquares - (sum * sum / count)) / count);
                            upperBound = (float) (initialValue + ( (1.0 - Math.min(0.8, stdDev / mean)) * upperDelta));
                            lowBound = (float) (initialValue - ( (1.0 - Math.min(0.8, stdDev / mean)) * lowerDelta));

                            if ( (sizeLimit != -1) && ( ( (count + 1) * volRes) > sizeLimit)) {
                                break;
                            }
                        }
                    }

                    if ( (x - 1) >= regionBounds.lowX()) {
                        idx = i - 1;
                        val = srcImage.getFloat(idx + offset4D);

                        if (maxDistance > 0) {
                            distance = Math.sqrt( ( (x - xInit) * (x - xInit) * resX)
                                    + ( (y - yInit) * (y - yInit) * resY) + ( (z - zInit) * (z - zInit) * resZ));
                        }

                        if ( (paintMask.get(idx) == false) && (val >= lowBound) && (val <= upperBound)
                                && (distance <= maxDistance)) {
                            stack.push(idx);
                            paintMask.set(idx);
                            count++;
                            sum += val;
                            sumOfSquares += val * val;
                            mean = sum / count;
                            stdDev = Math.sqrt( (sumOfSquares - (sum * sum / count)) / count);
                            upperBound = (float) (initialValue + ( (1.0 - Math.min(0.8, stdDev / mean)) * upperDelta));
                            lowBound = (float) (initialValue - ( (1.0 - Math.min(0.8, stdDev / mean)) * lowerDelta));

                            if ( (sizeLimit != -1) && ( ( (count + 1) * volRes) > sizeLimit)) {
                                break;
                            }
                        }
                    }

                    if ( (y + 1) < regionBounds.highY()) {
                        idx = i + xDim;
                        val = srcImage.getFloat(idx + offset4D);

                        if (maxDistance > 0) {
                            distance = Math.sqrt( ( (x - xInit) * (x - xInit) * resX)
                                    + ( (y - yInit) * (y - yInit) * resY) + ( (z - zInit) * (z - zInit) * resZ));
                        }

                        if ( (paintMask.get(idx) == false) && (val >= lowBound) && (val <= upperBound)
                                && (distance <= maxDistance)) {
                            stack.push(idx);
                            paintMask.set(idx);
                            count++;
                            sum += val;
                            sumOfSquares += val * val;
                            mean = sum / count;
                            stdDev = Math.sqrt( (sumOfSquares - (sum * sum / count)) / count);
                            upperBound = (float) (initialValue + ( (1.0 - Math.min(0.8, stdDev / mean)) * upperDelta));
                            lowBound = (float) (initialValue - ( (1.0 - Math.min(0.8, stdDev / mean)) * lowerDelta));

                            if ( (sizeLimit != -1) && ( ( (count + 1) * volRes) > sizeLimit)) {
                                break;
                            }
                        }
                    }

                    if ( (y - 1) >= regionBounds.lowY()) {
                        idx = i - xDim;
                        val = srcImage.getFloat(idx + offset4D);

                        if (maxDistance > 0) {
                            distance = Math.sqrt( ( (x - xInit) * (x - xInit) * resX)
                                    + ( (y - yInit) * (y - yInit) * resY) + ( (z - zInit) * (z - zInit) * resZ));
                        }

                        if ( (paintMask.get(idx) == false) && (val >= lowBound) && (val <= upperBound)
                                && (distance <= maxDistance)) {
                            stack.push(idx);
                            paintMask.set(idx);
                            count++;
                            sum += val;
                            sumOfSquares += val * val;
                            mean = sum / count;
                            stdDev = Math.sqrt( (sumOfSquares - (sum * sum / count)) / count);
                            upperBound = (float) (initialValue + ( (1.0 - Math.min(0.8, stdDev / mean)) * upperDelta));
                            lowBound = (float) (initialValue - ( (1.0 - Math.min(0.8, stdDev / mean)) * lowerDelta));

                            if ( (sizeLimit != -1) && ( ( (count + 1) * volRes) > sizeLimit)) {
                                break;
                            }
                        }
                    }

                    if ( (z + 1) < regionBounds.highZ()) {
                        idx = i + imageSize;
                        val = srcImage.getFloat(idx + offset4D);

                        if (maxDistance > 0) {
                            distance = Math.sqrt( ( (x - xInit) * (x - xInit) * resX)
                                    + ( (y - yInit) * (y - yInit) * resY) + ( (z - zInit) * (z - zInit) * resZ));
                        }

                        if ( (paintMask.get(idx) == false) && (val >= lowBound) && (val <= upperBound)
                                && (distance <= maxDistance)) {
                            stack.push(idx);
                            paintMask.set(idx);
                            count++;
                            sum += val;
                            sumOfSquares += val * val;
                            mean = sum / count;
                            stdDev = Math.sqrt( (sumOfSquares - (sum * sum / count)) / count);
                            upperBound = (float) (initialValue + ( (1.0 - Math.min(0.8, stdDev / mean)) * upperDelta));
                            lowBound = (float) (initialValue - ( (1.0 - Math.min(0.8, stdDev / mean)) * lowerDelta));

                            if ( (sizeLimit != -1) && ( ( (count + 1) * volRes) > sizeLimit)) {
                                break;
                            }
                        }
                    }

                    if ( (z - 1) >= regionBounds.lowZ()) {
                        idx = i - imageSize;
                        val = srcImage.getFloat(idx + offset4D);

                        if (maxDistance > 0) {
                            distance = Math.sqrt( ( (x - xInit) * (x - xInit) * resX)
                                    + ( (y - yInit) * (y - yInit) * resY) + ( (z - zInit) * (z - zInit) * resZ));
                        }

                        if ( (paintMask.get(idx) == false) && (val >= lowBound) && (val <= upperBound)
                                && (distance <= maxDistance)) {
                            stack.push(idx);
                            paintMask.set(idx);
                            count++;
                            sum += val;
                            sumOfSquares += val * val;
                            mean = sum / count;
                            stdDev = Math.sqrt( (sumOfSquares - (sum * sum / count)) / count);
                            upperBound = (float) (initialValue + ( (1.0 - Math.min(0.8, stdDev / mean)) * upperDelta));
                            lowBound = (float) (initialValue - ( (1.0 - Math.min(0.8, stdDev / mean)) * lowerDelta));

                            if ( (sizeLimit != -1) && ( ( (count + 1) * volRes) > sizeLimit)) {
                                break;
                            }
                        }
                    }
                } // while ( !stack.isEmpty() )
            } // (variableThresholds)
            else { 
                stack = new IntVector(length / 3, length / 8);
                stack.push(initIndex);

                if (paintMask.get(initIndex) == false) {
                    paintMask.set(initIndex);
                    count++;
                }

                while ( !stack.isEmpty()) {

                    i = stack.popFirstIn();
                    x = i % xDim;
                    y = (i % imageSize) / xDim;
                    z = i / imageSize;

                    if ( (x + 1) < regionBounds.highX()) {
                        idx = i + 1;
                        val = srcImage.getFloat(idx + offset4D);

                        if (maxDistance > 0) {
                            distance = Math.sqrt( ( (x - xInit) * (x - xInit) * resX)
                                    + ( (y - yInit) * (y - yInit) * resY) + ( (z - zInit) * (z - zInit) * resZ));
                        }

                        if ( (paintMask.get(idx) == false) && (val >= lowBound) && (val <= upperBound)
                                && (distance <= maxDistance)) {
                            stack.push(idx);
                            paintMask.set(idx);
                            count++;

                            if ( (sizeLimit != -1) && ( ( (count + 1) * volRes) > sizeLimit)) {
                                break;
                            }
                        }
                    }

                    if ( (x - 1) >= regionBounds.lowX()) {
                        idx = i - 1;
                        val = srcImage.getFloat(idx + offset4D);

                        if (maxDistance > 0) {
                            distance = Math.sqrt( ( (x - xInit) * (x - xInit) * resX)
                                    + ( (y - yInit) * (y - yInit) * resY) + ( (z - zInit) * (z - zInit) * resZ));
                        }

                        if ( (paintMask.get(idx) == false) && (val >= lowBound) && (val <= upperBound)
                                && (distance <= maxDistance)) {
                            stack.push(idx);
                            paintMask.set(idx);
                            count++;

                            if ( (sizeLimit != -1) && ( ( (count + 1) * volRes) > sizeLimit)) {
                                break;
                            }
                        }
                    }

                    if ( (y + 1) < regionBounds.highY()) {
                        idx = i + xDim;
                        val = srcImage.getFloat(idx + offset4D);

                        if (maxDistance > 0) {
                            distance = Math.sqrt( ( (x - xInit) * (x - xInit) * resX)
                                    + ( (y - yInit) * (y - yInit) * resY) + ( (z - zInit) * (z - zInit) * resZ));
                        }

                        if ( (paintMask.get(idx) == false) && (val >= lowBound) && (val <= upperBound)
                                && (distance <= maxDistance)) {
                            stack.push(idx);
                            paintMask.set(idx);
                            count++;

                            if ( (sizeLimit != -1) && ( ( (count + 1) * volRes) > sizeLimit)) {
                                break;
                            }
                        }
                    }

                    if ( (y - 1) >= regionBounds.lowY()) {
                        idx = i - xDim;
                        val = srcImage.getFloat(idx + offset4D);

                        if (maxDistance > 0) {
                            distance = Math.sqrt( ( (x - xInit) * (x - xInit) * resX)
                                    + ( (y - yInit) * (y - yInit) * resY) + ( (z - zInit) * (z - zInit) * resZ));
                        }

                        if ( (paintMask.get(idx) == false) && (val >= lowBound) && (val <= upperBound)
                                && (distance <= maxDistance)) {
                            stack.push(idx);
                            paintMask.set(idx);
                            count++;

                            if ( (sizeLimit != -1) && ( ( (count + 1) * volRes) > sizeLimit)) {
                                break;
                            }
                        }
                    }

                    if ( (z + 1) < regionBounds.highZ()) {
                        idx = i + imageSize;
                        val = srcImage.getFloat(idx + offset4D);

                        if (maxDistance > 0) {
                            distance = Math.sqrt( ( (x - xInit) * (x - xInit) * resX)
                                    + ( (y - yInit) * (y - yInit) * resY) + ( (z - zInit) * (z - zInit) * resZ));
                        }

                        if ( (paintMask.get(idx) == false) && (val >= lowBound) && (val <= upperBound)
                                && (distance <= maxDistance)) {
                            stack.push(idx);
                            paintMask.set(idx);
                            count++;

                            if ( (sizeLimit != -1) && ( ( (count + 1) * volRes) > sizeLimit)) {
                                break;
                            }
                        }
                    }

                    if ( (z - 1) >= regionBounds.lowZ()) {
                        idx = i - imageSize;
                        val = srcImage.getFloat(idx + offset4D);

                        if (maxDistance > 0) {
                            distance = Math.sqrt( ( (x - xInit) * (x - xInit) * resX)
                                    + ( (y - yInit) * (y - yInit) * resY) + ( (z - zInit) * (z - zInit) * resZ));
                        }

                        if ( (paintMask.get(idx) == false) && (val >= lowBound) && (val <= upperBound)
                                && (distance <= maxDistance)) {
                            stack.push(idx);
                            paintMask.set(idx);
                            count++;

                            if ( (sizeLimit != -1) && ( ( (count + 1) * volRes) > sizeLimit)) {
                                break;
                            }
                        }
                    }
                } // while ( !stack.isEmpty() )
            } // else
            
        } catch (final OutOfMemoryError e) {
            stack = null;

            System.gc();
            displayError("Algorithm RegionGrow:  Out of memory");
            setCompleted(false);

            return 0;
        }

        setCompleted(true);
        stack = null;
        System.gc();

        return count;
    }

    /**
     * 3D flood fill for color images that forms a bitset(boolean) mask.
     * 
     * @param paintMask mask used to indicated where region has grown
     * @param seedPt seed point for flood fill
     * @param th
     * @param useVOI use selected VOI for initial variance
     * @param dp
     * @param growDialog the RegionGrowDialog
     * @param lowBoundR lower bound of red values which are included in the region
     * @param upperBoundR upper bound of red values which are included in the region
     * @param lowBoundG lower bound of green values which are included in the region
     * @param upperBoundG upper bound of green values which are included in the region
     * @param lowBoundB lower bound of blue values which are included in the region
     * @param upperBoundB upper bound of blue values which are included in the region
     * @param sizeLimit stop region grow when objects exceeds size limit in pixels
     * @param maxDistance max distance from the seed point (in pixels) that the region is allowed to grow.
     * @param timeSlice timeSlice that will be used in a 4D image
     * @param regionBounds DOCUMENT ME!
     * 
     * @return returns the volume region
     */
    public int regionGrow3D(final BitSet paintMask, final Point3D seedPt, float th, boolean useVOI,
            final boolean dp, final RegionGrowDialog growDialog, final float lowBoundR,
            final float upperBoundR, final float lowBoundG, final float upperBoundG, final float lowBoundB,
            final float upperBoundB, final int sizeLimit, final float maxDistance, final int timeSlice,
            CubeBounds regionBounds) {
        final int xDim = srcImage.getExtents()[0];
        final int yDim = srcImage.getExtents()[1];
        final int zDim = srcImage.getExtents()[2];

        IntVector stack = null;
        float valR;
        float valG;
        float valB;
        int i, idx, j;
        final int imageSize = xDim * yDim;
        int x, y, z;
        final int length = xDim * yDim * zDim;
        int count = 0;
        final int initIndex = (seedPt.z * (xDim * yDim)) + (seedPt.y * xDim) + seedPt.x;
        final int offset4D = timeSlice * length;
        final int xInit = seedPt.x;
        final int yInit = seedPt.y;
        final int zInit = seedPt.z;
        double distance = -2;
        int current;
        int fmax;
        float activeThreshold;
        boolean[] havePushed = null;
        double imageRMean, imageGMean, imageBMean;
        double[][] totalVar = null;
        double[][] objectVar = null;
        double diffR;
        double diffG;
        double diffB;
        double varDet;
        ViewVOIVector VOIs = null;
        int nVOI;
        BitSet mask = null;
        int selectedContours;
        int iSel = 0;
        int voiSize = 0;

        if (regionBounds == null) {

            // set region bounds to image extents, if no bounds were specified
            regionBounds = new CubeBounds(xDim, 0, yDim, 0, zDim, 0);
        }

        if ( !regionBounds.contains(seedPt)) {
            return 0;
        }

        double resolX = srcImage.getFileInfo(0).getResolutions()[0];
        double resolY = srcImage.getFileInfo(0).getResolutions()[1];
        final double resolZ = srcImage.getFileInfo(0).getResolutions()[2];

        if (resolX <= 0) {
            resolX = 1;
        }

        if (resolY <= 0) {
            resolY = 1;
        }

        if (resolZ <= 0) {
            resolY = 1;
        }

        final double volRes = resolX * resolY * resolZ;

        final double resX = resolX * resolX;
        final double resY = resolY * resolY;
        final double resZ = resolZ * resolZ;

        this.paintMask = paintMask;
        this.growDialog = growDialog;

        try {

            stack = new IntVector(length / 3, length / 8);
            stack.push(initIndex);

            if (paintMask.get(initIndex) == false) {
                paintMask.set(initIndex);
                count++;
            }

            while ( !stack.isEmpty()) {

                i = stack.popFirstIn();
                x = i % xDim;
                y = (i % imageSize) / xDim;
                z = i / imageSize;

                if ( (x + 1) < regionBounds.highX()) {
                    idx = i + 1;
                    valR = srcImage.getFloat( (4 * (idx + offset4D)) + 1);
                    valG = srcImage.getFloat( (4 * (idx + offset4D)) + 2);
                    valB = srcImage.getFloat( (4 * (idx + offset4D)) + 3);

                    if (maxDistance > 0) {
                        distance = Math.sqrt( ( (x - xInit) * (x - xInit) * resX)
                                + ( (y - yInit) * (y - yInit) * resY) + ( (z - zInit) * (z - zInit) * resZ));
                    }

                    if ( (paintMask.get(idx) == false) && (valR >= lowBoundR) && (valR <= upperBoundR)
                            && (valG >= lowBoundG) && (valG <= upperBoundG) && (valB >= lowBoundB)
                            && (valB <= upperBoundB) && (distance <= maxDistance)) {
                        stack.push(idx);
                        paintMask.set(idx);
                        count++;

                        if ( (sizeLimit != -1) && ( ( (count + 1) * volRes) > sizeLimit)) {
                            break;
                        }
                    }
                }

                if ( (x - 1) >= regionBounds.lowX()) {
                    idx = i - 1;
                    valR = srcImage.getFloat( (4 * (idx + offset4D)) + 1);
                    valG = srcImage.getFloat( (4 * (idx + offset4D)) + 2);
                    valB = srcImage.getFloat( (4 * (idx + offset4D)) + 3);

                    if (maxDistance > 0) {
                        distance = Math.sqrt( ( (x - xInit) * (x - xInit) * resX)
                                + ( (y - yInit) * (y - yInit) * resY) + ( (z - zInit) * (z - zInit) * resZ));
                    }

                    if ( (paintMask.get(idx) == false) && (valR >= lowBoundR) && (valR <= upperBoundR)
                            && (valG >= lowBoundG) && (valG <= upperBoundG) && (valB >= lowBoundB)
                            && (valB <= upperBoundB) && (distance <= maxDistance)) {
                        stack.push(idx);
                        paintMask.set(idx);
                        count++;

                        if ( (sizeLimit != -1) && ( ( (count + 1) * volRes) > sizeLimit)) {
                            break;
                        }
                    }
                }

                if ( (y + 1) < regionBounds.highY()) {
                    idx = i + xDim;
                    valR = srcImage.getFloat( (4 * (idx + offset4D)) + 1);
                    valG = srcImage.getFloat( (4 * (idx + offset4D)) + 2);
                    valB = srcImage.getFloat( (4 * (idx + offset4D)) + 3);

                    if (maxDistance > 0) {
                        distance = Math.sqrt( ( (x - xInit) * (x - xInit) * resX)
                                + ( (y - yInit) * (y - yInit) * resY) + ( (z - zInit) * (z - zInit) * resZ));
                    }

                    if ( (paintMask.get(idx) == false) && (valR >= lowBoundR) && (valR <= upperBoundR)
                            && (valG >= lowBoundG) && (valG <= upperBoundG) && (valB >= lowBoundB)
                            && (valB <= upperBoundB) && (distance <= maxDistance)) {
                        stack.push(idx);
                        paintMask.set(idx);
                        count++;

                        if ( (sizeLimit != -1) && ( ( (count + 1) * volRes) > sizeLimit)) {
                            break;
                        }
                    }
                }

                if ( (y - 1) >= regionBounds.lowY()) {
                    idx = i - xDim;
                    valR = srcImage.getFloat( (4 * (idx + offset4D)) + 1);
                    valG = srcImage.getFloat( (4 * (idx + offset4D)) + 2);
                    valB = srcImage.getFloat( (4 * (idx + offset4D)) + 3);

                    if (maxDistance > 0) {
                        distance = Math.sqrt( ( (x - xInit) * (x - xInit) * resX)
                                + ( (y - yInit) * (y - yInit) * resY) + ( (z - zInit) * (z - zInit) * resZ));
                    }

                    if ( (paintMask.get(idx) == false) && (valR >= lowBoundR) && (valR <= upperBoundR)
                            && (valG >= lowBoundG) && (valG <= upperBoundG) && (valB >= lowBoundB)
                            && (valB <= upperBoundB) && (distance <= maxDistance)) {
                        stack.push(idx);
                        paintMask.set(idx);
                        count++;

                        if ( (sizeLimit != -1) && ( ( (count + 1) * volRes) > sizeLimit)) {
                            break;
                        }
                    }
                }

                if ( (z + 1) < regionBounds.highZ()) {
                    idx = i + imageSize;
                    valR = srcImage.getFloat( (4 * (idx + offset4D)) + 1);
                    valG = srcImage.getFloat( (4 * (idx + offset4D)) + 2);
                    valB = srcImage.getFloat( (4 * (idx + offset4D)) + 3);

                    if (maxDistance > 0) {
                        distance = Math.sqrt( ( (x - xInit) * (x - xInit) * resX)
                                + ( (y - yInit) * (y - yInit) * resY) + ( (z - zInit) * (z - zInit) * resZ));
                    }

                    if ( (paintMask.get(idx) == false) && (valR >= lowBoundR) && (valR <= upperBoundR)
                            && (valG >= lowBoundG) && (valG <= upperBoundG) && (valB >= lowBoundB)
                            && (valB <= upperBoundB) && (distance <= maxDistance)) {
                        stack.push(idx);
                        paintMask.set(idx);
                        count++;

                        if ( (sizeLimit != -1) && ( ( (count + 1) * volRes) > sizeLimit)) {
                            break;
                        }
                    }
                }

                if ( (z - 1) >= regionBounds.lowZ()) {
                    idx = i - imageSize;
                    valR = srcImage.getFloat( (4 * (idx + offset4D)) + 1);
                    valG = srcImage.getFloat( (4 * (idx + offset4D)) + 2);
                    valB = srcImage.getFloat( (4 * (idx + offset4D)) + 3);

                    if (maxDistance > 0) {
                        distance = Math.sqrt( ( (x - xInit) * (x - xInit) * resX)
                                + ( (y - yInit) * (y - yInit) * resY) + ( (z - zInit) * (z - zInit) * resZ));
                    }

                    if ( (paintMask.get(idx) == false) && (valR >= lowBoundR) && (valR <= upperBoundR)
                            && (valG >= lowBoundG) && (valG <= upperBoundG) && (valB >= lowBoundB)
                            && (valB <= upperBoundB) && (distance <= maxDistance)) {
                        stack.push(idx);
                        paintMask.set(idx);
                        count++;

                        if ( (sizeLimit != -1) && ( ( (count + 1) * volRes) > sizeLimit)) {
                            break;
                        }
                    }
                }
            } // while ( !stack.isEmpty() )
            
        } catch (final OutOfMemoryError e) {
            stack = null;

            System.gc();
            displayError("Algorithm RegionGrow:  Out of memory");
            setCompleted(false);

            return 0;
        }

        setCompleted(true);
        stack = null;
        System.gc();

        return count;
    }

    /**
     * Starts the program.
     */
    public void runAlgorithm() {

        if (srcImage == null) {
            displayError("Source Image is null");

            return;
        }
        // if (srcImage.getDims() != 3) {displayError("Source Image is not 3D"); return;}

        /*
         * if (destImage != null){ if (srcImage.getNDims() == 2){ calcStoreInDest2D(); } else if (srcImage.getNDims() >
         * 2) { calcStoreInDest3D(); } } else { if (srcImage.getNDims() == 2){ calcInPlace2D(); } else if
         * (srcImage.getNDims() > 2) { calcInPlace3D(); } }
         */

    }

    /**
     * DOCUMENT ME!
     * 
     * @param threshold DOCUMENT ME!
     * @param compImage DOCUMENT ME!
     * @param dp
     */
    public void setNewThreshold(final float threshold, final PaintGrowListener compImage,
            final boolean dp) {
        int i;
        int count = 0;
        float activeThreshold;

        activeThreshold = threshold * 65535.0f;

        

        compImage.showRegionInfo(count, null);

        setCompleted(true);

        if (growDialog != null) {
            growDialog.notifyPaintListeners(true, false, paintMask);
        }

        srcImage.notifyImageDisplayListeners(null, false);
    }

    
    

}
