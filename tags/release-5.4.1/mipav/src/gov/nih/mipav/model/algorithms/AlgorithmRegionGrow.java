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
 * The variableThresholds option of region grow is not used with fuzzy connectedness. If variableThresholds is true,
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
 * The fuzzy connectedness option of the region growing code is ported with some modifications from NLM's Insight ITK
 * toolkit. Fuzzy connectedness requires a seed point and a fuzzy threshold value ranging from 0.0 to 1.0 as inputs. The
 * fuzzy scene that is generated depends only on the seed point and is independent of the fuzzy threshold specified.
 * Thus, once a fuzzy scene is created by clicking the mouse at a seed point, new paint bit masks corresponding to the
 * same seed point but different fuzzy thresholds can be created very quickly and easily by simply entering a new fuzzy
 * threshold with the slider and clicking the new threshold button. The user has the option of creating a fuzzy image.
 * </p>
 * 
 * <p>
 * 2 significant changes are made from the original code. In the original code the variance of the segment corresponding
 * to the seed point is arbitrarily taken as 2500. Here the total variance is calculated and the object variance is
 * roughly estimated as 1/5 of the total variance. In the original code a point could be pushed on the stack even if it
 * was already present on the stack. In this modified code a boolean array is used to prevent points currently on the
 * stack from being pushed onto the stack a second time.
 * </p>
 * 
 * <p>
 * The reference that Insight gives for their fuzzy connectedness code is: "Fuzzy Connectedness and Object Definition:
 * Theory, Algorithms, and Applications in Image Segmentation" by Jayaram K. Udupa and Supun Samarasekera, Graphicals
 * Models and Image Processing, Vol. 58, No. 3, pp. 246-261, 1996.
 * </p>
 * 
 * <p>
 * A summary of the algorithm used follows: 1.) Create an integer fuzzy scene of the same total length as the original
 * source image. Set the seed point to 65535 and set all the other elements of the fuzzy scene to 0. 2.) Push the
 * indices of the 4 or 6 nearest neighbors of the seed point onto the stack. Note that this is a LIFO stack - that is
 * indices will be popped from the front of the stack. While the stack is not empty do: 3.) Pop the first index from the
 * stack into current 4.) Find fmax Fuzzy affinity of 2 neighboring points measures how close the average of their 2
 * gray scale values is to the gray scale value of the seed point. Fuzzy affinity reaches a maximum of 65535 when the
 * average of the 2 gray scale values equals the gray scale value of the seed point and fuzzy affinity approaches a
 * minimum of 0 as the average of the 2 neighbors differs greatly from the seed value.
 * </p>
 * 
 * <p>
 * For each neighbor get the smaller of the neighbor's fuzzy scene value and the fuzzy afinity of current and the
 * neighbor. Thus, for a single path the strength of the path is determined by the weakest link in the path. For the
 * first neighbor this value is assigned to tmp and for all following neighbors this value if assigned to tmp2. Then,
 * for fmax choose the maximum of the 4 or 6 paths. Thus, the fuzzy connectedness between 2 points is determined by the
 * strongest of the multiple paths between the 2 points. 5.) if (fmax > fuzzyScene[current]) 6.) Set fuzzyScene[current] =
 * fmax 7.) Push the 4 or 6 indices of the nearest neighbors of current onto the stack if they are not already present
 * on the stack. endif endwhile 8.) Create an active threshold by multiplying the fuzzy threshold by 65535. Set the
 * paint bit map if the fuzzy scene element >= activeThreshold. Otherwise, clear the paint bit map.
 * </p>
 * 
 * <hr>
 * 
 * <p>
 * The (pre-version 3.6) ITK license for the code we ported the fuzzy connectedness from is below: <blockquote>
 * 
 * <pre>
 * Copyright (c) 1999-2008 Insight Software Consortium All rights reserved.
 * Redistribution and use in source and binary forms, with or without modification,
 * are permitted provided that the following conditions are met:
 * 
 * * Redistributions of source code must retain the above copyright notice,
 * this list of conditions and the following disclaimer.
 * 
 * * Redistributions in binary form must reproduce the above copyright notice, this list of conditions
 * and the following disclaimer in the documentation and/or other materials provided with the distribution.
 * 
 * * The name of the Insight Software Consortium, nor the names of any consortium members,
 * nor of any contributors, may be used to endorse or promote products derived from this
 * software without specific prior written permission.
 * 
 * * Modified source versions must be plainly marked as such, and must not be misrepresented
 * as being the original software.
 * 
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDER AND CONTRIBUTORS &quot;AS IS&quot; AND ANY EXPRESS
 * OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND
 * FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE AUTHORS OR CONTRIBUTORS BE LIABLE
 * FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
 * LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
 * INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
 * OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE,
 * EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 * </pre>
 * 
 * </blockquote>
 * </p>
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
    private ModelImage fuzzyImage = null;

    /** DOCUMENT ME! */
    private int[] fuzzyScene = null;

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
        fuzzyScene = null;

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
     * @param fuzzyThreshold if negative fuzzy connectedness is not used value ranges from 0 to 1 if fuzzy connectedness
     *            is used
     * @param useVOI use selected VOI for initial variance
     * @param displayFuzzy display a fuzzy image if true
     * @param growDialog the RegionGrowDialog
     * @param lowBound lower bound of values which are included in the region
     * @param upperBound upper bound of values which are included in the region
     * @param sizeLimit stop region grow when objects exceeds size limit in pixels
     * @param maxDistance max distance from the seed point (in pixels) that the region is allowed to grow.
     * @param variableThresholds If true vary thresholds as region grows
     * 
     * @return returns the area region
     */
    public int regionGrow2D(final BitSet paintMask, final Point seedPt, final float fuzzyThreshold, boolean useVOI,
            final boolean displayFuzzy, final RegionGrowDialog growDialog, float lowBound, float upperBound,
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

            if ( (fuzzyThreshold < 0.0f) && variableThresholds) { // don't use fuzzy connectedness
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
            } // if (fuzzyThreshold < 0.0f && variableThresholds)
            else if (fuzzyThreshold < 0.0f) { // don't use fuzzy connectedness
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
            } // else if (fuzzyThreshold < 0.0f)
            else if (srcImage.isColorImage() == false) { // use fuzzy connectedness on

                // black and white image
                fuzzyScene = new int[imageSize];

                for (i = 0; i < imageSize; i++) {
                    fuzzyScene[i] = 0;
                }

                fuzzyScene[initIndex] = 65535;
                havePushed = new boolean[imageSize];

                for (i = 0; i < imageSize; i++) {
                    havePushed[i] = false;
                }

                mMean = srcImage.getDouble(initIndex);
                imageMean = 0;

                if ( !useVOI) {

                    for (i = 0; i < imageSize; i++) {
                        imageMean += srcImage.getDouble(i);
                    }

                    imageMean /= imageSize;
                    totalVariance = 0;

                    for (i = 0; i < imageSize; i++) {
                        diff = srcImage.getDouble(i) - imageMean;
                        totalVariance += diff * diff;
                    }

                    totalVariance /= (imageSize - 1);
                    objectVariance = totalVariance / 5.0;
                } // if (!useVOI)
                else { // use selected VOI for initial standard deviation
                    VOIs = srcImage.getVOIs();
                    nVOI = VOIs.size();

                    if (nVOI == 0) {
                        System.gc();
                        displayError("Algorithm RegionGrow:  No VOI is present");
                        setCompleted(false);

                        return 0;
                    }

                    selectedContours = 0;

                    for (i = 0; i < nVOI; i++) {

                        if ( (VOIs.VOIAt(i).isActive()) && (VOIs.VOIAt(i).getCurveType() == VOI.CONTOUR)) {
                            selectedContours++;
                            iSel = i;
                        }
                    }

                    if (selectedContours == 0) {
                        System.gc();
                        displayError("AlgorithmRegionGrow: No selected contour is present");
                        setCompleted(false);

                        return 0;
                    }

                    if (selectedContours > 1) {
                        System.gc();
                        displayError("AlgorithmRegionGrow: More than 1 selected contour is present");
                        setCompleted(false);

                        return 0;
                    }

                    mask = new BitSet(imageSize);
                    VOIs.VOIAt(iSel).createBinaryMask3D(mask, xDim, yDim, false, false);

                    for (i = 0; i < imageSize; i++) {

                        if (mask.get(i)) {
                            imageMean += srcImage.getDouble(i);
                            voiSize++;
                        }
                    }

                    imageMean /= voiSize;
                    objectVariance = 0;

                    for (i = 0; i < imageSize; i++) {

                        if (mask.get(i)) {
                            diff = srcImage.getDouble(i) - imageMean;
                            objectVariance += diff * diff;
                        }
                    }

                    objectVariance /= (voiSize - 1);
                } // else use selected VOI for initial standard deviation

                c = -0.5 / objectVariance;

                stack = new IntVector(imageSize, imageSize / 4);

                // Push the 4 connected neighbors of the seed point onto the stack
                if (seedPt.x > 0) {
                    stack.push(initIndex - 1);
                    havePushed[initIndex - 1] = true;
                }

                if (seedPt.x < (xDim - 1)) {
                    stack.push(initIndex + 1);
                    havePushed[initIndex + 1] = true;
                }

                if (seedPt.y > 0) {
                    stack.push(initIndex - xDim);
                    havePushed[initIndex - xDim] = true;
                }

                if (seedPt.y < (yDim - 1)) {
                    stack.push(initIndex + xDim);
                    havePushed[initIndex + xDim] = true;
                }

                // buildGaussLUT();
                while ( !stack.isEmpty()) {

                    // System.out.println("stack length = " + stack.length());
                    current = stack.popFirstIn();
                    havePushed[current] = false;
                    fmax = (int) (findStrongPath(current, xDim, yDim));

                    if (fmax > fuzzyScene[current]) {
                        fuzzyScene[current] = fmax;

                        // Push the 4 connected neighbors of current
                        x = current % xDim;
                        y = current / xDim;

                        if ( (x > 0) && ( !havePushed[current - 1])) {
                            stack.push(current - 1);
                            havePushed[current - 1] = true;
                        }

                        if ( (x < (xDim - 1)) && ( !havePushed[current + 1])) {
                            stack.push(current + 1);
                            havePushed[current + 1] = true;
                        }

                        if ( (y > 0) && ( !havePushed[current - xDim])) {
                            stack.push(current - xDim);
                            havePushed[current - xDim] = true;
                        }

                        if ( (y < (yDim - 1)) && ( !havePushed[current + xDim])) {
                            stack.push(current + xDim);
                            havePushed[current + xDim] = true;
                        }
                    }
                } // while ( !stack.isEmpty())

                activeThreshold = fuzzyThreshold * 65535.0f;

                for (i = 0; i < imageSize; i++) {

                    if (fuzzyScene[i] >= activeThreshold) {
                        paintMask.set(i);
                        count++;
                    } else {
                        paintMask.clear(i);
                    }
                }

                if (growDialog != null) {
                    growDialog.setRegionGrowAlgo(this);
                }
            } // else use fuzzy connectedness on black and white image
            else { // use fuzzy connectedness on color image
                redBuffer = new float[imageSize];
                greenBuffer = new float[imageSize];
                blueBuffer = new float[imageSize];

                try {
                    srcImage.exportRGBData(1, 0, imageSize, redBuffer);
                    srcImage.exportRGBData(2, 0, imageSize, greenBuffer);
                    srcImage.exportRGBData(3, 0, imageSize, blueBuffer);
                } catch (final IOException error) {
                    displayError("AlgorithmRegionGrow reports: image locked");
                    setCompleted(false);

                    return 0;
                }

                fuzzyScene = new int[imageSize];

                for (i = 0; i < imageSize; i++) {
                    fuzzyScene[i] = 0;
                }

                fuzzyScene[initIndex] = 65535;
                havePushed = new boolean[imageSize];

                for (i = 0; i < imageSize; i++) {
                    havePushed[i] = false;
                }

                mRMean = redBuffer[initIndex];
                mGMean = greenBuffer[initIndex];
                mBMean = blueBuffer[initIndex];
                imageRMean = 0;
                imageGMean = 0;
                imageBMean = 0;

                if ( !useVOI) {

                    for (i = 0; i < imageSize; i++) {
                        imageRMean += redBuffer[i];
                        imageGMean += greenBuffer[i];
                        imageBMean += blueBuffer[i];
                    }

                    imageRMean /= imageSize;
                    imageGMean /= imageSize;
                    imageBMean /= imageSize;
                    totalVar = new double[3][3];

                    for (i = 0; i < 3; i++) {

                        for (j = 0; j < 3; j++) {
                            totalVar[i][j] = 0.0;
                        }
                    }

                    for (i = 0; i < imageSize; i++) {
                        diffR = redBuffer[i] - imageRMean;
                        diffG = greenBuffer[i] - imageGMean;
                        diffB = blueBuffer[i] - imageBMean;
                        totalVar[0][0] += diffR * diffR;
                        totalVar[0][1] += diffR * diffG;
                        totalVar[0][2] += diffR * diffB;
                        totalVar[1][1] += diffG * diffG;
                        totalVar[1][2] += diffG * diffB;
                        totalVar[2][2] += diffB * diffB;
                    }

                    totalVar[1][0] = totalVar[0][1];
                    totalVar[2][0] = totalVar[0][2];
                    totalVar[2][1] = totalVar[1][2];

                    for (i = 0; i < 3; i++) {

                        for (j = 0; j < 3; j++) {
                            totalVar[i][j] /= (imageSize - 1);
                        }
                    }

                    objectVar = new double[3][3];

                    for (i = 0; i < 3; i++) {

                        for (j = 0; j < 3; j++) {
                            objectVar[i][j] = totalVar[i][j] / 5.0;
                        }
                    }
                } // if (!useVOI)
                else { // use selected VOI for initial variances
                    VOIs = srcImage.getVOIs();
                    nVOI = VOIs.size();

                    if (nVOI == 0) {
                        System.gc();
                        displayError("Algorithm RegionGrow:  No VOI is present");
                        setCompleted(false);

                        return 0;
                    }

                    selectedContours = 0;

                    for (i = 0; i < nVOI; i++) {

                        if ( (VOIs.VOIAt(i).isActive()) && (VOIs.VOIAt(i).getCurveType() == VOI.CONTOUR)) {
                            selectedContours++;
                            iSel = i;
                        }
                    }

                    if (selectedContours == 0) {
                        System.gc();
                        displayError("AlgorithmRegionGrow: No selected contour is present");
                        setCompleted(false);

                        return 0;
                    }

                    if (selectedContours > 1) {
                        System.gc();
                        displayError("AlgorithmRegionGrow: More than 1 selected contour is present");
                        setCompleted(false);

                        return 0;
                    }

                    mask = new BitSet(imageSize);
                    VOIs.VOIAt(iSel).createBinaryMask3D(mask, xDim, yDim, false, false);

                    for (i = 0; i < imageSize; i++) {

                        if (mask.get(i)) {
                            imageRMean += redBuffer[i];
                            imageGMean += greenBuffer[i];
                            imageBMean += blueBuffer[i];
                            voiSize++;
                        }
                    }

                    imageRMean /= voiSize;
                    imageGMean /= voiSize;
                    imageBMean /= voiSize;
                    objectVar = new double[3][3];

                    for (i = 0; i < 3; i++) {

                        for (j = 0; j < 3; j++) {
                            objectVar[i][j] = 0.0;
                        }
                    }

                    for (i = 0; i < imageSize; i++) {

                        if (mask.get(i)) {
                            diffR = redBuffer[i] - imageRMean;
                            diffG = greenBuffer[i] - imageGMean;
                            diffB = blueBuffer[i] - imageBMean;
                            objectVar[0][0] += diffR * diffR;
                            objectVar[0][1] += diffR * diffG;
                            objectVar[0][2] += diffR * diffB;
                            objectVar[1][1] += diffG * diffG;
                            objectVar[1][2] += diffG * diffB;
                            objectVar[2][2] += diffB * diffB;
                        }
                    }

                    objectVar[1][0] = objectVar[0][1];
                    objectVar[2][0] = objectVar[0][2];
                    objectVar[2][1] = objectVar[1][2];

                    for (i = 0; i < 3; i++) {

                        for (j = 0; j < 3; j++) {
                            objectVar[i][j] /= (voiSize - 1);
                        }
                    }
                } // else use selected VOI for initial variances

                varDet = (objectVar[0][0] * objectVar[1][1] * objectVar[2][2])
                        + (objectVar[1][0] * objectVar[2][1] * objectVar[0][2])
                        + (objectVar[0][1] * objectVar[1][2] * objectVar[2][0])
                        - (objectVar[2][0] * objectVar[1][1] * objectVar[0][2])
                        - (objectVar[0][1] * objectVar[1][0] * objectVar[2][2])
                        - (objectVar[0][0] * objectVar[1][2] * objectVar[2][1]);

                varInverse = new double[3][3];
                varInverse[0][0] = ( (objectVar[1][1] * objectVar[2][2]) - (objectVar[2][1] * objectVar[1][2]))
                        / varDet;
                varInverse[0][1] = - ( (objectVar[1][0] * objectVar[2][2]) - (objectVar[2][0] * objectVar[1][2]))
                        / varDet;
                varInverse[0][2] = ( (objectVar[1][0] * objectVar[2][1]) - (objectVar[2][0] * objectVar[1][1]))
                        / varDet;
                varInverse[1][0] = - ( (objectVar[0][1] * objectVar[2][2]) - (objectVar[2][1] * objectVar[0][2]))
                        / varDet;
                varInverse[1][1] = ( (objectVar[0][0] * objectVar[2][2]) - (objectVar[2][0] * objectVar[0][2]))
                        / varDet;
                varInverse[1][2] = - ( (objectVar[0][0] * objectVar[2][1]) - (objectVar[2][0] * objectVar[0][1]))
                        / varDet;
                varInverse[2][0] = ( (objectVar[0][1] * objectVar[1][2]) - (objectVar[1][1] * objectVar[0][2]))
                        / varDet;
                varInverse[2][1] = - ( (objectVar[0][0] * objectVar[1][2]) - (objectVar[1][0] * objectVar[0][2]))
                        / varDet;
                varInverse[2][2] = ( (objectVar[0][0] * objectVar[1][1]) - (objectVar[1][0] * objectVar[0][1]))
                        / varDet;

                stack = new IntVector(imageSize, imageSize / 4);

                // Push the 4 connected neighbors of the seed point onto the stack
                if (seedPt.x > 0) {
                    stack.push(initIndex - 1);
                    havePushed[initIndex - 1] = true;
                }

                if (seedPt.x < (xDim - 1)) {
                    stack.push(initIndex + 1);
                    havePushed[initIndex + 1] = true;
                }

                if (seedPt.y > 0) {
                    stack.push(initIndex - xDim);
                    havePushed[initIndex - xDim] = true;
                }

                if (seedPt.y < (yDim - 1)) {
                    stack.push(initIndex + xDim);
                    havePushed[initIndex + xDim] = true;
                }

                while ( !stack.isEmpty()) {
                    current = stack.popFirstIn();
                    havePushed[current] = false;
                    fmax = (int) (findStrongPathColor(current, xDim, yDim));

                    if (fmax > fuzzyScene[current]) {
                        fuzzyScene[current] = fmax;

                        // Push the 4 connected neighbors of current
                        x = current % xDim;
                        y = current / xDim;

                        if ( (x > 0) && ( !havePushed[current - 1])) {
                            stack.push(current - 1);
                            havePushed[current - 1] = true;
                        }

                        if ( (x < (xDim - 1)) && ( !havePushed[current + 1])) {
                            stack.push(current + 1);
                            havePushed[current + 1] = true;
                        }

                        if ( (y > 0) && ( !havePushed[current - xDim])) {
                            stack.push(current - xDim);
                            havePushed[current - xDim] = true;
                        }

                        if ( (y < (yDim - 1)) && ( !havePushed[current + xDim])) {
                            stack.push(current + xDim);
                            havePushed[current + xDim] = true;
                        }
                    }
                } // while ( !stack.isEmpty())

                activeThreshold = fuzzyThreshold * 65535.0f;

                for (i = 0; i < imageSize; i++) {

                    if (fuzzyScene[i] >= activeThreshold) {
                        paintMask.set(i);
                        count++;
                    } else {
                        paintMask.clear(i);
                    }
                }

                if (growDialog != null) {
                    growDialog.setRegionGrowAlgo(this);
                }
            } // else use fuzzy connectedness on color image
        } catch (final OutOfMemoryError e) {
            stack = null;
            System.gc();
            displayError("Algorithm RegionGrow:  Out of memory");
            setCompleted(false);

            return 0;
        }

        if (displayFuzzy) {
            fuzzyImage = new ModelImage(ModelStorageBase.FLOAT, srcImage.getExtents(), srcImage.getImageName()
                    + "_fuzzy");

            try {
                fuzzyImage.importData(0, fuzzyScene, true);
            } catch (final IOException error) {
                displayError("AlgorithmRegionGrow: IOException on fuzzyImage.importData");
                setCompleted(false);

                return count;
            }

            try {
                imageFrame = new ViewJFrameImage(fuzzyImage);
            } catch (final OutOfMemoryError error) {
                System.gc();
                displayError("AlgorithmRegionGrow: Out of memory: unable to open fuzzy image frame");
                setCompleted(false);

                return count;
            }
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
     * @param fuzzyThreshold if negative fuzzy connectedness is not used value ranges from 0 to 1 if fuzzy connectedness
     *            is used
     * @param useVOI use selected VOI for initial variance
     * @param displayFuzzy display a fuzzy image if true
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
    public int regionGrow2D(final BitSet paintMask, final Point seedPt, final float fuzzyThreshold, boolean useVOI,
            final boolean displayFuzzy, final RegionGrowDialog growDialog, final float lowBoundR,
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

            if (fuzzyThreshold < 0.0f) { // don't use fuzzy connectedness
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
            } // else if (fuzzyThreshold < 0.0f)
            else { // use fuzzy connectedness on color image
                redBuffer = new float[imageSize];
                greenBuffer = new float[imageSize];
                blueBuffer = new float[imageSize];

                try {
                    srcImage.exportRGBData(1, 0, imageSize, redBuffer);
                    srcImage.exportRGBData(2, 0, imageSize, greenBuffer);
                    srcImage.exportRGBData(3, 0, imageSize, blueBuffer);
                } catch (final IOException error) {
                    displayError("AlgorithmRegionGrow reports: image locked");
                    setCompleted(false);

                    return 0;
                }

                fuzzyScene = new int[imageSize];

                for (i = 0; i < imageSize; i++) {
                    fuzzyScene[i] = 0;
                }

                fuzzyScene[initIndex] = 65535;
                havePushed = new boolean[imageSize];

                for (i = 0; i < imageSize; i++) {
                    havePushed[i] = false;
                }

                mRMean = redBuffer[initIndex];
                mGMean = greenBuffer[initIndex];
                mBMean = blueBuffer[initIndex];
                imageRMean = 0;
                imageGMean = 0;
                imageBMean = 0;

                if ( !useVOI) {

                    for (i = 0; i < imageSize; i++) {
                        imageRMean += redBuffer[i];
                        imageGMean += greenBuffer[i];
                        imageBMean += blueBuffer[i];
                    }

                    imageRMean /= imageSize;
                    imageGMean /= imageSize;
                    imageBMean /= imageSize;
                    totalVar = new double[3][3];

                    for (i = 0; i < 3; i++) {

                        for (j = 0; j < 3; j++) {
                            totalVar[i][j] = 0.0;
                        }
                    }

                    for (i = 0; i < imageSize; i++) {
                        diffR = redBuffer[i] - imageRMean;
                        diffG = greenBuffer[i] - imageGMean;
                        diffB = blueBuffer[i] - imageBMean;
                        totalVar[0][0] += diffR * diffR;
                        totalVar[0][1] += diffR * diffG;
                        totalVar[0][2] += diffR * diffB;
                        totalVar[1][1] += diffG * diffG;
                        totalVar[1][2] += diffG * diffB;
                        totalVar[2][2] += diffB * diffB;
                    }

                    totalVar[1][0] = totalVar[0][1];
                    totalVar[2][0] = totalVar[0][2];
                    totalVar[2][1] = totalVar[1][2];

                    for (i = 0; i < 3; i++) {

                        for (j = 0; j < 3; j++) {
                            totalVar[i][j] /= (imageSize - 1);
                        }
                    }

                    objectVar = new double[3][3];

                    for (i = 0; i < 3; i++) {

                        for (j = 0; j < 3; j++) {
                            objectVar[i][j] = totalVar[i][j] / 5.0;
                        }
                    }
                } // if (!useVOI)
                else { // use selected VOI for initial variances
                    VOIs = srcImage.getVOIs();
                    nVOI = VOIs.size();

                    if (nVOI == 0) {
                        System.gc();
                        displayError("Algorithm RegionGrow:  No VOI is present");
                        setCompleted(false);

                        return 0;
                    }

                    selectedContours = 0;

                    for (i = 0; i < nVOI; i++) {

                        if ( (VOIs.VOIAt(i).isActive()) && (VOIs.VOIAt(i).getCurveType() == VOI.CONTOUR)) {
                            selectedContours++;
                            iSel = i;
                        }
                    }

                    if (selectedContours == 0) {
                        System.gc();
                        displayError("AlgorithmRegionGrow: No selected contour is present");
                        setCompleted(false);

                        return 0;
                    }

                    if (selectedContours > 1) {
                        System.gc();
                        displayError("AlgorithmRegionGrow: More than 1 selected contour is present");
                        setCompleted(false);

                        return 0;
                    }

                    mask = new BitSet(imageSize);
                    VOIs.VOIAt(iSel).createBinaryMask3D(mask, xDim, yDim, false, false);

                    for (i = 0; i < imageSize; i++) {

                        if (mask.get(i)) {
                            imageRMean += redBuffer[i];
                            imageGMean += greenBuffer[i];
                            imageBMean += blueBuffer[i];
                            voiSize++;
                        }
                    }

                    imageRMean /= voiSize;
                    imageGMean /= voiSize;
                    imageBMean /= voiSize;
                    objectVar = new double[3][3];

                    for (i = 0; i < 3; i++) {

                        for (j = 0; j < 3; j++) {
                            objectVar[i][j] = 0.0;
                        }
                    }

                    for (i = 0; i < imageSize; i++) {

                        if (mask.get(i)) {
                            diffR = redBuffer[i] - imageRMean;
                            diffG = greenBuffer[i] - imageGMean;
                            diffB = blueBuffer[i] - imageBMean;
                            objectVar[0][0] += diffR * diffR;
                            objectVar[0][1] += diffR * diffG;
                            objectVar[0][2] += diffR * diffB;
                            objectVar[1][1] += diffG * diffG;
                            objectVar[1][2] += diffG * diffB;
                            objectVar[2][2] += diffB * diffB;
                        }
                    }

                    objectVar[1][0] = objectVar[0][1];
                    objectVar[2][0] = objectVar[0][2];
                    objectVar[2][1] = objectVar[1][2];

                    for (i = 0; i < 3; i++) {

                        for (j = 0; j < 3; j++) {
                            objectVar[i][j] /= (voiSize - 1);
                        }
                    }
                } // else use selected VOI for initial variances

                varDet = (objectVar[0][0] * objectVar[1][1] * objectVar[2][2])
                        + (objectVar[1][0] * objectVar[2][1] * objectVar[0][2])
                        + (objectVar[0][1] * objectVar[1][2] * objectVar[2][0])
                        - (objectVar[2][0] * objectVar[1][1] * objectVar[0][2])
                        - (objectVar[0][1] * objectVar[1][0] * objectVar[2][2])
                        - (objectVar[0][0] * objectVar[1][2] * objectVar[2][1]);

                varInverse = new double[3][3];
                varInverse[0][0] = ( (objectVar[1][1] * objectVar[2][2]) - (objectVar[2][1] * objectVar[1][2]))
                        / varDet;
                varInverse[0][1] = - ( (objectVar[1][0] * objectVar[2][2]) - (objectVar[2][0] * objectVar[1][2]))
                        / varDet;
                varInverse[0][2] = ( (objectVar[1][0] * objectVar[2][1]) - (objectVar[2][0] * objectVar[1][1]))
                        / varDet;
                varInverse[1][0] = - ( (objectVar[0][1] * objectVar[2][2]) - (objectVar[2][1] * objectVar[0][2]))
                        / varDet;
                varInverse[1][1] = ( (objectVar[0][0] * objectVar[2][2]) - (objectVar[2][0] * objectVar[0][2]))
                        / varDet;
                varInverse[1][2] = - ( (objectVar[0][0] * objectVar[2][1]) - (objectVar[2][0] * objectVar[0][1]))
                        / varDet;
                varInverse[2][0] = ( (objectVar[0][1] * objectVar[1][2]) - (objectVar[1][1] * objectVar[0][2]))
                        / varDet;
                varInverse[2][1] = - ( (objectVar[0][0] * objectVar[1][2]) - (objectVar[1][0] * objectVar[0][2]))
                        / varDet;
                varInverse[2][2] = ( (objectVar[0][0] * objectVar[1][1]) - (objectVar[1][0] * objectVar[0][1]))
                        / varDet;

                stack = new IntVector(imageSize, imageSize / 4);

                // Push the 4 connected neighbors of the seed point onto the stack
                if (seedPt.x > 0) {
                    stack.push(initIndex - 1);
                    havePushed[initIndex - 1] = true;
                }

                if (seedPt.x < (xDim - 1)) {
                    stack.push(initIndex + 1);
                    havePushed[initIndex + 1] = true;
                }

                if (seedPt.y > 0) {
                    stack.push(initIndex - xDim);
                    havePushed[initIndex - xDim] = true;
                }

                if (seedPt.y < (yDim - 1)) {
                    stack.push(initIndex + xDim);
                    havePushed[initIndex + xDim] = true;
                }

                while ( !stack.isEmpty()) {
                    current = stack.popFirstIn();
                    havePushed[current] = false;
                    fmax = (int) (findStrongPathColor(current, xDim, yDim));

                    if (fmax > fuzzyScene[current]) {
                        fuzzyScene[current] = fmax;

                        // Push the 4 connected neighbors of current
                        x = current % xDim;
                        y = current / xDim;

                        if ( (x > 0) && ( !havePushed[current - 1])) {
                            stack.push(current - 1);
                            havePushed[current - 1] = true;
                        }

                        if ( (x < (xDim - 1)) && ( !havePushed[current + 1])) {
                            stack.push(current + 1);
                            havePushed[current + 1] = true;
                        }

                        if ( (y > 0) && ( !havePushed[current - xDim])) {
                            stack.push(current - xDim);
                            havePushed[current - xDim] = true;
                        }

                        if ( (y < (yDim - 1)) && ( !havePushed[current + xDim])) {
                            stack.push(current + xDim);
                            havePushed[current + xDim] = true;
                        }
                    }
                } // while ( !stack.isEmpty())

                activeThreshold = fuzzyThreshold * 65535.0f;

                for (i = 0; i < imageSize; i++) {

                    if (fuzzyScene[i] >= activeThreshold) {
                        paintMask.set(i);
                        count++;
                    } else {
                        paintMask.clear(i);
                    }
                }

                if (growDialog != null) {
                    growDialog.setRegionGrowAlgo(this);
                }
            } // else use fuzzy connectedness on color image
        } catch (final OutOfMemoryError e) {
            stack = null;
            System.gc();
            displayError("Algorithm RegionGrow:  Out of memory");
            setCompleted(false);

            return 0;
        }

        if (displayFuzzy) {
            fuzzyImage = new ModelImage(ModelStorageBase.FLOAT, srcImage.getExtents(), srcImage.getImageName()
                    + "_fuzzy");

            try {
                fuzzyImage.importData(0, fuzzyScene, true);
            } catch (final IOException error) {
                displayError("AlgorithmRegionGrow: IOException on fuzzyImage.importData");
                setCompleted(false);

                return count;
            }

            try {
                imageFrame = new ViewJFrameImage(fuzzyImage);
            } catch (final OutOfMemoryError error) {
                System.gc();
                displayError("AlgorithmRegionGrow: Out of memory: unable to open fuzzy image frame");
                setCompleted(false);

                return count;
            }
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
     * @param fuzzyThreshold if negative fuzzy connectedness is not used value ranges from 0 to 1 if fuzzy connectedness
     *            is used
     * @param useVOI use selected VOI for initial variance
     * @param displayFuzzy display a fuzzy image if true
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
    public int regionGrow3D(final BitSet paintMask, final Point3D seedPt, final float fuzzyThreshold, boolean useVOI,
            final boolean displayFuzzy, final RegionGrowDialog growDialog, float lowBound, float upperBound,
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

            if ( (fuzzyThreshold < 0.0f) && variableThresholds) { // don't use fuzzy connectedness
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
            } // if (fuzzyThreshold < 0.0f && variableThresholds)
            else if (fuzzyThreshold < 0.0f) { // don't use fuzzy connectedness
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
            } // else if (fuzzyThreshold < 0.0f)
            else if (srcImage.isColorImage() == false) { // use fuzzy connectedness on
                // black and white image

                fireProgressStateChanged("Region Grow", "Growing round 1...");

                fuzzyScene = new int[length];

                for (i = 0; i < length; i++) {
                    fuzzyScene[i] = 0;
                }

                fuzzyScene[initIndex] = 65535;
                havePushed = new boolean[length];

                for (i = 0; i < length; i++) {
                    havePushed[i] = false;
                }

                mMean = srcImage.getDouble(initIndex);
                imageMean = 0;

                if ( !useVOI) {

                    for (i = 0; i < length; i++) {
                        imageMean += srcImage.getDouble(i);
                    }

                    imageMean /= length;
                    totalVariance = 0;

                    for (i = 0; i < length; i++) {
                        diff = srcImage.getDouble(i) - imageMean;
                        totalVariance += diff * diff;
                    }

                    totalVariance /= (length - 1);
                    objectVariance = totalVariance / 5.0;
                } // if (!useVOI)
                else { // use selected VOI for initial variance
                    VOIs = srcImage.getVOIs();
                    nVOI = VOIs.size();

                    if (nVOI == 0) {
                        System.gc();
                        displayError("Algorithm RegionGrow:  No VOI is present");
                        setCompleted(false);

                        return 0;
                    }

                    selectedContours = 0;

                    for (i = 0; i < nVOI; i++) {

                        if ( (VOIs.VOIAt(i).isActive()) && (VOIs.VOIAt(i).getCurveType() == VOI.CONTOUR)) {
                            selectedContours++;
                            iSel = i;
                        }
                    }

                    if (selectedContours == 0) {
                        System.gc();
                        displayError("AlgorithmRegionGrow: No selected contour is present");
                        setCompleted(false);

                        return 0;
                    }

                    if (selectedContours > 1) {
                        System.gc();
                        displayError("AlgorithmRegionGrow: More than 1 selected contour is present");
                        setCompleted(false);

                        return 0;
                    }

                    mask = new BitSet(length);
                    VOIs.VOIAt(iSel).createBinaryMask3D(mask, xDim, yDim, false, false);

                    for (i = 0; i < length; i++) {

                        if (mask.get(i)) {
                            imageMean += srcImage.getDouble(i);
                            voiSize++;
                        }
                    }

                    imageMean /= voiSize;
                    objectVariance = 0;

                    for (i = 0; i < length; i++) {

                        if (mask.get(i)) {
                            diff = srcImage.getDouble(i) - imageMean;
                            objectVariance += diff * diff;
                        }
                    }

                    objectVariance /= (voiSize - 1);
                }

                c = -0.5 / objectVariance;
                stack = new IntVector(length, length / 4);

                // Push the 6 connected neighbors of the seed point onto the stack
                if (seedPt.x > 0) {
                    stack.push(initIndex - 1);
                    havePushed[initIndex - 1] = true;
                }

                if (seedPt.x < (xDim - 1)) {
                    stack.push(initIndex + 1);
                    havePushed[initIndex + 1] = true;
                }

                if (seedPt.y > 0) {
                    stack.push(initIndex - xDim);
                    havePushed[initIndex - xDim] = true;
                }

                if (seedPt.y < (yDim - 1)) {
                    stack.push(initIndex + xDim);
                    havePushed[initIndex + xDim] = true;
                }

                if (seedPt.z > 0) {
                    stack.push(initIndex - imageSize);
                    havePushed[initIndex - imageSize] = true;
                }

                if (seedPt.z < (zDim - 1)) {
                    stack.push(initIndex + imageSize);
                    havePushed[initIndex + imageSize] = true;
                }

                activeThreshold = fuzzyThreshold * 65535.0f;

                // buildGaussLUT();

                while ( !stack.isEmpty()) {
                    pCtr++;

                    if (pCtr > 1000000) {
                        pCtr = 0;
                        round++;
                        fireProgressStateChanged("Growing round " + String.valueOf(round) + "...");
                    }

                    if ( (pCtr % 250000) == 0) {
                        fireProgressStateChanged(pCtr / 10000);
                    }

                    // System.out.println("stack length = " + stack.length());
                    current = stack.popFirstIn();
                    havePushed[current] = false;
                    fmax = (int) (findStrongPath(current, xDim, yDim, zDim));

                    if (fmax > fuzzyScene[current]) {
                        fuzzyScene[current] = fmax;

                        // Push the 6 connected neighbors of current
                        x = current % xDim;
                        y = (current % imageSize) / xDim;
                        z = current / imageSize;

                        if ( (x > 0) && ( !havePushed[current - 1])) {
                            stack.push(current - 1);
                            havePushed[current - 1] = true;
                        }

                        if ( (x < (xDim - 1)) && ( !havePushed[current + 1])) {
                            stack.push(current + 1);
                            havePushed[current + 1] = true;
                        }

                        if ( (y > 0) && ( !havePushed[current - xDim])) {
                            stack.push(current - xDim);
                            havePushed[current - xDim] = true;
                        }

                        if ( (y < (yDim - 1)) && ( !havePushed[current + xDim])) {
                            stack.push(current + xDim);
                            havePushed[current + xDim] = true;
                        }

                        if ( (z > 0) && ( !havePushed[current - imageSize])) {
                            stack.push(current - imageSize);
                            havePushed[current - imageSize] = true;
                        }

                        if ( (z < (zDim - 1)) && ( !havePushed[current + imageSize])) {
                            stack.push(current + imageSize);
                            havePushed[current + imageSize] = true;
                        }
                    }
                } // while ( !stack.isEmpty())

                activeThreshold = fuzzyThreshold * 65535.0f;

                for (i = 0; i < length; i++) {

                    if (fuzzyScene[i] >= activeThreshold) {
                        paintMask.set(i);
                        count++;
                    } else {
                        paintMask.clear(i);
                    }
                }

                if (growDialog != null) {
                    growDialog.setRegionGrowAlgo(this);
                }
            } // else use fuzzy connectedness on black and white image
            else { // use fuzzy connectedness on color image
                redBuffer = new float[length];
                greenBuffer = new float[length];
                blueBuffer = new float[length];

                try {
                    srcImage.exportRGBData(1, 0, length, redBuffer);
                    srcImage.exportRGBData(2, 0, length, greenBuffer);
                    srcImage.exportRGBData(3, 0, length, blueBuffer);
                } catch (final IOException error) {
                    displayError("AlgorithmRegionGrow reports: image locked");
                    setCompleted(false);

                    return 0;
                }

                fuzzyScene = new int[length];

                for (i = 0; i < length; i++) {
                    fuzzyScene[i] = 0;
                }

                fuzzyScene[initIndex] = 65535;
                havePushed = new boolean[length];

                for (i = 0; i < length; i++) {
                    havePushed[i] = false;
                }

                mRMean = redBuffer[initIndex];
                mGMean = greenBuffer[initIndex];
                mBMean = blueBuffer[initIndex];
                imageRMean = 0;
                imageGMean = 0;
                imageBMean = 0;

                if ( !useVOI) {

                    for (i = 0; i < length; i++) {
                        imageRMean += redBuffer[i];
                        imageGMean += greenBuffer[i];
                        imageBMean += blueBuffer[i];
                    }

                    imageRMean /= length;
                    imageGMean /= length;
                    imageBMean /= length;
                    totalVar = new double[3][3];

                    for (i = 0; i < 3; i++) {

                        for (j = 0; j < 3; j++) {
                            totalVar[i][j] = 0.0;
                        }
                    }

                    for (i = 0; i < length; i++) {
                        diffR = redBuffer[i] - imageRMean;
                        diffG = greenBuffer[i] - imageGMean;
                        diffB = blueBuffer[i] - imageBMean;
                        totalVar[0][0] += diffR * diffR;
                        totalVar[0][1] += diffR * diffG;
                        totalVar[0][2] += diffR * diffB;
                        totalVar[1][1] += diffG * diffG;
                        totalVar[1][2] += diffG * diffB;
                        totalVar[2][2] += diffB * diffB;
                    }

                    totalVar[1][0] = totalVar[0][1];
                    totalVar[2][0] = totalVar[0][2];
                    totalVar[2][1] = totalVar[1][2];

                    for (i = 0; i < 3; i++) {

                        for (j = 0; j < 3; j++) {
                            totalVar[i][j] /= (length - 1);
                        }
                    }

                    objectVar = new double[3][3];

                    for (i = 0; i < 3; i++) {

                        for (j = 0; j < 3; j++) {
                            objectVar[i][j] = totalVar[i][j] / 5.0;
                        }
                    }
                } // if (!useVOI)
                else { // use selected VOI for initial variances
                    VOIs = srcImage.getVOIs();
                    nVOI = VOIs.size();

                    if (nVOI == 0) {
                        System.gc();
                        displayError("Algorithm RegionGrow:  No VOI is present");
                        setCompleted(false);

                        return 0;
                    }

                    selectedContours = 0;

                    for (i = 0; i < nVOI; i++) {

                        if ( (VOIs.VOIAt(i).isActive()) && (VOIs.VOIAt(i).getCurveType() == VOI.CONTOUR)) {
                            selectedContours++;
                            iSel = i;
                        }
                    }

                    if (selectedContours == 0) {
                        System.gc();
                        displayError("AlgorithmRegionGrow: No selected contour is present");
                        setCompleted(false);

                        return 0;
                    }

                    if (selectedContours > 1) {
                        System.gc();
                        displayError("AlgorithmRegionGrow: More than 1 selected contour is present");
                        setCompleted(false);

                        return 0;
                    }

                    mask = new BitSet(length);
                    VOIs.VOIAt(iSel).createBinaryMask3D(mask, xDim, yDim, false, false);

                    for (i = 0; i < length; i++) {

                        if (mask.get(i)) {
                            imageRMean += redBuffer[i];
                            imageGMean += greenBuffer[i];
                            imageBMean += blueBuffer[i];
                            voiSize++;
                        }
                    }

                    imageRMean /= voiSize;
                    imageGMean /= voiSize;
                    imageBMean /= voiSize;
                    objectVar = new double[3][3];

                    for (i = 0; i < 3; i++) {

                        for (j = 0; j < 3; j++) {
                            objectVar[i][j] = 0.0;
                        }
                    }

                    for (i = 0; i < length; i++) {

                        if (mask.get(i)) {
                            diffR = redBuffer[i] - imageRMean;
                            diffG = greenBuffer[i] - imageGMean;
                            diffB = blueBuffer[i] - imageBMean;
                            objectVar[0][0] += diffR * diffR;
                            objectVar[0][1] += diffR * diffG;
                            objectVar[0][2] += diffR * diffB;
                            objectVar[1][1] += diffG * diffG;
                            objectVar[1][2] += diffG * diffB;
                            objectVar[2][2] += diffB * diffB;
                        }
                    }

                    objectVar[1][0] = objectVar[0][1];
                    objectVar[2][0] = objectVar[0][2];
                    objectVar[2][1] = objectVar[1][2];

                    for (i = 0; i < 3; i++) {

                        for (j = 0; j < 3; j++) {
                            objectVar[i][j] /= (voiSize - 1);
                        }
                    }
                } // else use selected VOI for initial variances

                varDet = (objectVar[0][0] * objectVar[1][1] * objectVar[2][2])
                        + (objectVar[1][0] * objectVar[2][1] * objectVar[0][2])
                        + (objectVar[0][1] * objectVar[1][2] * objectVar[2][0])
                        - (objectVar[2][0] * objectVar[1][1] * objectVar[0][2])
                        - (objectVar[0][1] * objectVar[1][0] * objectVar[2][2])
                        - (objectVar[0][0] * objectVar[1][2] * objectVar[2][1]);

                varInverse = new double[3][3];
                varInverse[0][0] = ( (objectVar[1][1] * objectVar[2][2]) - (objectVar[2][1] * objectVar[1][2]))
                        / varDet;
                varInverse[0][1] = - ( (objectVar[1][0] * objectVar[2][2]) - (objectVar[2][0] * objectVar[1][2]))
                        / varDet;
                varInverse[0][2] = ( (objectVar[1][0] * objectVar[2][1]) - (objectVar[2][0] * objectVar[1][1]))
                        / varDet;
                varInverse[1][0] = - ( (objectVar[0][1] * objectVar[2][2]) - (objectVar[2][1] * objectVar[0][2]))
                        / varDet;
                varInverse[1][1] = ( (objectVar[0][0] * objectVar[2][2]) - (objectVar[2][0] * objectVar[0][2]))
                        / varDet;
                varInverse[1][2] = - ( (objectVar[0][0] * objectVar[2][1]) - (objectVar[2][0] * objectVar[0][1]))
                        / varDet;
                varInverse[2][0] = ( (objectVar[0][1] * objectVar[1][2]) - (objectVar[1][1] * objectVar[0][2]))
                        / varDet;
                varInverse[2][1] = - ( (objectVar[0][0] * objectVar[1][2]) - (objectVar[1][0] * objectVar[0][2]))
                        / varDet;
                varInverse[2][2] = ( (objectVar[0][0] * objectVar[1][1]) - (objectVar[1][0] * objectVar[0][1]))
                        / varDet;

                stack = new IntVector(imageSize, imageSize / 4);

                // Push the 6 connected neighbors of the seed point onto the stack
                if (seedPt.x > 0) {
                    stack.push(initIndex - 1);
                    havePushed[initIndex - 1] = true;
                }

                if (seedPt.x < (xDim - 1)) {
                    stack.push(initIndex + 1);
                    havePushed[initIndex + 1] = true;
                }

                if (seedPt.y > 0) {
                    stack.push(initIndex - xDim);
                    havePushed[initIndex - xDim] = true;
                }

                if (seedPt.y < (yDim - 1)) {
                    stack.push(initIndex + xDim);
                    havePushed[initIndex + xDim] = true;
                }

                if (seedPt.z > 0) {
                    stack.push(initIndex - imageSize);
                    havePushed[initIndex - imageSize] = true;
                }

                if (seedPt.z < (zDim - 1)) {
                    stack.push(initIndex + imageSize);
                    havePushed[initIndex + imageSize] = true;
                }

                while ( !stack.isEmpty()) {

                    // System.out.println("stack length = " + stack.length());
                    current = stack.popFirstIn();
                    havePushed[current] = false;
                    fmax = (int) (findStrongPathColor(current, xDim, yDim, zDim));

                    if (fmax > fuzzyScene[current]) {
                        fuzzyScene[current] = fmax;

                        // Push the 6 connected neighbors of current
                        x = current % xDim;
                        y = (current % imageSize) / xDim;
                        z = current / imageSize;

                        if ( (x > 0) && ( !havePushed[current - 1])) {
                            stack.push(current - 1);
                            havePushed[current - 1] = true;
                        }

                        if ( (x < (xDim - 1)) && ( !havePushed[current + 1])) {
                            stack.push(current + 1);
                            havePushed[current + 1] = true;
                        }

                        if ( (y > 0) && ( !havePushed[current - xDim])) {
                            stack.push(current - xDim);
                            havePushed[current - xDim] = true;
                        }

                        if ( (y < (yDim - 1)) && ( !havePushed[current + xDim])) {
                            stack.push(current + xDim);
                            havePushed[current + xDim] = true;
                        }

                        if ( (z > 0) && ( !havePushed[current - imageSize])) {
                            stack.push(current - imageSize);
                            havePushed[current - imageSize] = true;
                        }

                        if ( (z < (zDim - 1)) && ( !havePushed[current + imageSize])) {
                            stack.push(current + imageSize);
                            havePushed[current + imageSize] = true;
                        }
                    }
                } // while ( !stack.isEmpty())

                activeThreshold = fuzzyThreshold * 65535.0f;

                for (i = 0; i < length; i++) {

                    if (fuzzyScene[i] >= activeThreshold) {
                        paintMask.set(i);
                        count++;
                    } else {
                        paintMask.clear(i);
                    }
                }

                if (growDialog != null) {
                    growDialog.setRegionGrowAlgo(this);
                }
            } // else use fuzzy connectedness on color image
        } catch (final OutOfMemoryError e) {
            stack = null;

            System.gc();
            displayError("Algorithm RegionGrow:  Out of memory");
            setCompleted(false);

            return 0;
        }

        if (displayFuzzy) {
            fuzzyImage = new ModelImage(ModelStorageBase.FLOAT, srcImage.getExtents(), srcImage.getImageName()
                    + "_fuzzy");

            try {
                fuzzyImage.importData(0, fuzzyScene, true);
            } catch (final IOException error) {
                displayError("AlgorithmRegionGrow: IOException on fuzzyImage.importData");
                setCompleted(false);

                return count;
            }

            try {
                imageFrame = new ViewJFrameImage(fuzzyImage);
            } catch (final OutOfMemoryError error) {
                System.gc();
                displayError("AlgorithmRegionGrow: Out of memory: unable to open fuzzy image frame");
                setCompleted(false);

                return count;
            }
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
     * @param fuzzyThreshold if negative fuzzy connectedness is not used value ranges from 0 to 1 if fuzzy connectedness
     *            is used
     * @param useVOI use selected VOI for initial variance
     * @param displayFuzzy display a fuzzy image if true
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
    public int regionGrow3D(final BitSet paintMask, final Point3D seedPt, final float fuzzyThreshold, boolean useVOI,
            final boolean displayFuzzy, final RegionGrowDialog growDialog, final float lowBoundR,
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

            if (fuzzyThreshold < 0.0f) { // don't use fuzzy connectedness
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
            } // else if (fuzzyThreshold < 0.0f)
            else { // use fuzzy connectedness on color image
                redBuffer = new float[length];
                greenBuffer = new float[length];
                blueBuffer = new float[length];

                try {
                    srcImage.exportRGBData(1, 0, length, redBuffer);
                    srcImage.exportRGBData(2, 0, length, greenBuffer);
                    srcImage.exportRGBData(3, 0, length, blueBuffer);
                } catch (final IOException error) {
                    displayError("AlgorithmRegionGrow reports: image locked");
                    setCompleted(false);

                    return 0;
                }

                fuzzyScene = new int[length];

                for (i = 0; i < length; i++) {
                    fuzzyScene[i] = 0;
                }

                fuzzyScene[initIndex] = 65535;
                havePushed = new boolean[length];

                for (i = 0; i < length; i++) {
                    havePushed[i] = false;
                }

                mRMean = redBuffer[initIndex];
                mGMean = greenBuffer[initIndex];
                mBMean = blueBuffer[initIndex];
                imageRMean = 0;
                imageGMean = 0;
                imageBMean = 0;

                if ( !useVOI) {

                    for (i = 0; i < length; i++) {
                        imageRMean += redBuffer[i];
                        imageGMean += greenBuffer[i];
                        imageBMean += blueBuffer[i];
                    }

                    imageRMean /= length;
                    imageGMean /= length;
                    imageBMean /= length;
                    totalVar = new double[3][3];

                    for (i = 0; i < 3; i++) {

                        for (j = 0; j < 3; j++) {
                            totalVar[i][j] = 0.0;
                        }
                    }

                    for (i = 0; i < length; i++) {
                        diffR = redBuffer[i] - imageRMean;
                        diffG = greenBuffer[i] - imageGMean;
                        diffB = blueBuffer[i] - imageBMean;
                        totalVar[0][0] += diffR * diffR;
                        totalVar[0][1] += diffR * diffG;
                        totalVar[0][2] += diffR * diffB;
                        totalVar[1][1] += diffG * diffG;
                        totalVar[1][2] += diffG * diffB;
                        totalVar[2][2] += diffB * diffB;
                    }

                    totalVar[1][0] = totalVar[0][1];
                    totalVar[2][0] = totalVar[0][2];
                    totalVar[2][1] = totalVar[1][2];

                    for (i = 0; i < 3; i++) {

                        for (j = 0; j < 3; j++) {
                            totalVar[i][j] /= (length - 1);
                        }
                    }

                    objectVar = new double[3][3];

                    for (i = 0; i < 3; i++) {

                        for (j = 0; j < 3; j++) {
                            objectVar[i][j] = totalVar[i][j] / 5.0;
                        }
                    }
                } // if (!useVOI)
                else { // use selected VOI for initial variances
                    VOIs = srcImage.getVOIs();
                    nVOI = VOIs.size();

                    if (nVOI == 0) {
                        System.gc();
                        displayError("Algorithm RegionGrow:  No VOI is present");
                        setCompleted(false);

                        return 0;
                    }

                    selectedContours = 0;

                    for (i = 0; i < nVOI; i++) {

                        if ( (VOIs.VOIAt(i).isActive()) && (VOIs.VOIAt(i).getCurveType() == VOI.CONTOUR)) {
                            selectedContours++;
                            iSel = i;
                        }
                    }

                    if (selectedContours == 0) {
                        System.gc();
                        displayError("AlgorithmRegionGrow: No selected contour is present");
                        setCompleted(false);

                        return 0;
                    }

                    if (selectedContours > 1) {
                        System.gc();
                        displayError("AlgorithmRegionGrow: More than 1 selected contour is present");
                        setCompleted(false);

                        return 0;
                    }

                    mask = new BitSet(length);
                    VOIs.VOIAt(iSel).createBinaryMask3D(mask, xDim, yDim, false, false);

                    for (i = 0; i < length; i++) {

                        if (mask.get(i)) {
                            imageRMean += redBuffer[i];
                            imageGMean += greenBuffer[i];
                            imageBMean += blueBuffer[i];
                            voiSize++;
                        }
                    }

                    imageRMean /= voiSize;
                    imageGMean /= voiSize;
                    imageBMean /= voiSize;
                    objectVar = new double[3][3];

                    for (i = 0; i < 3; i++) {

                        for (j = 0; j < 3; j++) {
                            objectVar[i][j] = 0.0;
                        }
                    }

                    for (i = 0; i < length; i++) {

                        if (mask.get(i)) {
                            diffR = redBuffer[i] - imageRMean;
                            diffG = greenBuffer[i] - imageGMean;
                            diffB = blueBuffer[i] - imageBMean;
                            objectVar[0][0] += diffR * diffR;
                            objectVar[0][1] += diffR * diffG;
                            objectVar[0][2] += diffR * diffB;
                            objectVar[1][1] += diffG * diffG;
                            objectVar[1][2] += diffG * diffB;
                            objectVar[2][2] += diffB * diffB;
                        }
                    }

                    objectVar[1][0] = objectVar[0][1];
                    objectVar[2][0] = objectVar[0][2];
                    objectVar[2][1] = objectVar[1][2];

                    for (i = 0; i < 3; i++) {

                        for (j = 0; j < 3; j++) {
                            objectVar[i][j] /= (voiSize - 1);
                        }
                    }
                } // else use selected VOI for initial variances

                varDet = (objectVar[0][0] * objectVar[1][1] * objectVar[2][2])
                        + (objectVar[1][0] * objectVar[2][1] * objectVar[0][2])
                        + (objectVar[0][1] * objectVar[1][2] * objectVar[2][0])
                        - (objectVar[2][0] * objectVar[1][1] * objectVar[0][2])
                        - (objectVar[0][1] * objectVar[1][0] * objectVar[2][2])
                        - (objectVar[0][0] * objectVar[1][2] * objectVar[2][1]);

                varInverse = new double[3][3];
                varInverse[0][0] = ( (objectVar[1][1] * objectVar[2][2]) - (objectVar[2][1] * objectVar[1][2]))
                        / varDet;
                varInverse[0][1] = - ( (objectVar[1][0] * objectVar[2][2]) - (objectVar[2][0] * objectVar[1][2]))
                        / varDet;
                varInverse[0][2] = ( (objectVar[1][0] * objectVar[2][1]) - (objectVar[2][0] * objectVar[1][1]))
                        / varDet;
                varInverse[1][0] = - ( (objectVar[0][1] * objectVar[2][2]) - (objectVar[2][1] * objectVar[0][2]))
                        / varDet;
                varInverse[1][1] = ( (objectVar[0][0] * objectVar[2][2]) - (objectVar[2][0] * objectVar[0][2]))
                        / varDet;
                varInverse[1][2] = - ( (objectVar[0][0] * objectVar[2][1]) - (objectVar[2][0] * objectVar[0][1]))
                        / varDet;
                varInverse[2][0] = ( (objectVar[0][1] * objectVar[1][2]) - (objectVar[1][1] * objectVar[0][2]))
                        / varDet;
                varInverse[2][1] = - ( (objectVar[0][0] * objectVar[1][2]) - (objectVar[1][0] * objectVar[0][2]))
                        / varDet;
                varInverse[2][2] = ( (objectVar[0][0] * objectVar[1][1]) - (objectVar[1][0] * objectVar[0][1]))
                        / varDet;

                stack = new IntVector(imageSize, imageSize / 4);

                // Push the 6 connected neighbors of the seed point onto the stack
                if (seedPt.x > 0) {
                    stack.push(initIndex - 1);
                    havePushed[initIndex - 1] = true;
                }

                if (seedPt.x < (xDim - 1)) {
                    stack.push(initIndex + 1);
                    havePushed[initIndex + 1] = true;
                }

                if (seedPt.y > 0) {
                    stack.push(initIndex - xDim);
                    havePushed[initIndex - xDim] = true;
                }

                if (seedPt.y < (yDim - 1)) {
                    stack.push(initIndex + xDim);
                    havePushed[initIndex + xDim] = true;
                }

                if (seedPt.z > 0) {
                    stack.push(initIndex - imageSize);
                    havePushed[initIndex - imageSize] = true;
                }

                if (seedPt.z < (zDim - 1)) {
                    stack.push(initIndex + imageSize);
                    havePushed[initIndex + imageSize] = true;
                }

                while ( !stack.isEmpty()) {

                    // System.out.println("stack length = " + stack.length());
                    current = stack.popFirstIn();
                    havePushed[current] = false;
                    fmax = (int) (findStrongPathColor(current, xDim, yDim, zDim));

                    if (fmax > fuzzyScene[current]) {
                        fuzzyScene[current] = fmax;

                        // Push the 6 connected neighbors of current
                        x = current % xDim;
                        y = (current % imageSize) / xDim;
                        z = current / imageSize;

                        if ( (x > 0) && ( !havePushed[current - 1])) {
                            stack.push(current - 1);
                            havePushed[current - 1] = true;
                        }

                        if ( (x < (xDim - 1)) && ( !havePushed[current + 1])) {
                            stack.push(current + 1);
                            havePushed[current + 1] = true;
                        }

                        if ( (y > 0) && ( !havePushed[current - xDim])) {
                            stack.push(current - xDim);
                            havePushed[current - xDim] = true;
                        }

                        if ( (y < (yDim - 1)) && ( !havePushed[current + xDim])) {
                            stack.push(current + xDim);
                            havePushed[current + xDim] = true;
                        }

                        if ( (z > 0) && ( !havePushed[current - imageSize])) {
                            stack.push(current - imageSize);
                            havePushed[current - imageSize] = true;
                        }

                        if ( (z < (zDim - 1)) && ( !havePushed[current + imageSize])) {
                            stack.push(current + imageSize);
                            havePushed[current + imageSize] = true;
                        }
                    }
                } // while ( !stack.isEmpty())

                activeThreshold = fuzzyThreshold * 65535.0f;

                for (i = 0; i < length; i++) {

                    if (fuzzyScene[i] >= activeThreshold) {
                        paintMask.set(i);
                        count++;
                    } else {
                        paintMask.clear(i);
                    }
                }

                if (growDialog != null) {
                    growDialog.setRegionGrowAlgo(this);
                }
            } // else use fuzzy connectedness on color image
        } catch (final OutOfMemoryError e) {
            stack = null;

            System.gc();
            displayError("Algorithm RegionGrow:  Out of memory");
            setCompleted(false);

            return 0;
        }

        if (displayFuzzy) {
            fuzzyImage = new ModelImage(ModelStorageBase.FLOAT, srcImage.getExtents(), srcImage.getImageName()
                    + "_fuzzy");

            try {
                fuzzyImage.importData(0, fuzzyScene, true);
            } catch (final IOException error) {
                displayError("AlgorithmRegionGrow: IOException on fuzzyImage.importData");
                setCompleted(false);

                return count;
            }

            try {
                imageFrame = new ViewJFrameImage(fuzzyImage);
            } catch (final OutOfMemoryError error) {
                System.gc();
                displayError("AlgorithmRegionGrow: Out of memory: unable to open fuzzy image frame");
                setCompleted(false);

                return count;
            }
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
     * @param fuzzyThreshold DOCUMENT ME!
     * @param compImage DOCUMENT ME!
     * @param displayFuzzy DOCUMENT ME!
     */
    public void setNewThreshold(final float fuzzyThreshold, final PaintGrowListener compImage,
            final boolean displayFuzzy) {
        int i;
        int count = 0;
        float activeThreshold;

        activeThreshold = fuzzyThreshold * 65535.0f;

        for (i = 0; i < fuzzyScene.length; i++) {

            if (fuzzyScene[i] >= activeThreshold) {
                paintMask.set(i);
                count++;
            } else {
                paintMask.clear(i);
            }
        }

        compImage.showRegionInfo(count, null);

        if ( (displayFuzzy) && (fuzzyImage == null)) {
            fuzzyImage = new ModelImage(ModelStorageBase.FLOAT, srcImage.getExtents(), srcImage.getImageName()
                    + "_fuzzy");

            try {
                fuzzyImage.importData(0, fuzzyScene, true);
            } catch (final IOException error) {
                displayError("AlgorithmRegionGrow: IOException on fuzzyImage.importData");
                setCompleted(false);

                return;
            }

            try {
                imageFrame = new ViewJFrameImage(fuzzyImage);
            } catch (final OutOfMemoryError error) {
                System.gc();
                displayError("AlgorithmRegionGrow: Out of memory: unable to open fuzzy image frame");
                setCompleted(false);

                return;
            }
        } // if ((displayFuzzy) && (fuzzyImage == null))

        setCompleted(true);

        if (growDialog != null) {
            growDialog.notifyPaintListeners(true, false, paintMask);
        }

        srcImage.notifyImageDisplayListeners(null, false);
    }

    /**
     * DOCUMENT ME!
     * 
     * @param current index of point
     * @param xDim DOCUMENT ME!
     * @param yDim DOCUMENT ME!
     * 
     * @return tmp
     */
    private double findStrongPath(final int current, final int xDim, final int yDim) {
        double tmp = 0;
        double tmp1;
        double tmp2;
        double centerPixel;
        int x, y;

        centerPixel = srcImage.getDouble(current);
        x = current % xDim;
        y = current / xDim;

        if (x > 0) {
            tmp = fuzzyScene[current - 1];
            tmp1 = fuzzyAffinity(srcImage.getDouble(current - 1), centerPixel);

            if (tmp > tmp1) {
                tmp = tmp1;
            }
        } // if (x > 0)

        if (x < (xDim - 1)) {
            tmp2 = fuzzyScene[current + 1];
            tmp1 = fuzzyAffinity(srcImage.getDouble(current + 1), centerPixel);

            if (tmp2 > tmp1) {
                tmp2 = tmp1;
            }

            if (tmp < tmp2) {
                tmp = tmp2;
            }
        } // if (x < (xDim - 1))

        if (y > 0) {
            tmp2 = fuzzyScene[current - xDim];
            tmp1 = fuzzyAffinity(srcImage.getDouble(current - xDim), centerPixel);

            if (tmp2 > tmp1) {
                tmp2 = tmp1;
            }

            if (tmp < tmp2) {
                tmp = tmp2;
            }
        } // if (y > 0)

        if (y < (yDim - 1)) {
            tmp2 = fuzzyScene[current + xDim];
            tmp1 = fuzzyAffinity(srcImage.getDouble(current + xDim), centerPixel);

            if (tmp2 > tmp1) {
                tmp2 = tmp1;
            }

            if (tmp < tmp2) {
                tmp = tmp2;
            }
        } // if (y < (yDim - 1))

        return tmp;
    }

    /**
     * DOCUMENT ME!
     * 
     * @param current index of point
     * @param xDim DOCUMENT ME!
     * @param yDim DOCUMENT ME!
     * @param zDim DOCUMENT ME!
     * 
     * @return tmp
     */
    private double findStrongPath(final int current, final int xDim, final int yDim, final int zDim) {
        double tmp = 0;
        double tmp1;
        double tmp2;
        float centerPixel;
        int x, y, z;
        int sliceSize;
        double affTmp;

        centerPixel = srcImage.getFloat(current);
        sliceSize = xDim * yDim;
        x = current % xDim;
        y = (current % sliceSize) / xDim;
        z = current / sliceSize;

        if (x > 0) {
            tmp = fuzzyScene[current - 1];
            affTmp = (0.5 * (srcImage.getDouble(current - 1) + centerPixel)) - mMean;
            tmp1 = 65535.0 * Math.exp(c * affTmp * affTmp);

            if (tmp > tmp1) {
                tmp = tmp1;
            }
        } // if (x > 0)

        if (x < (xDim - 1)) {
            tmp2 = fuzzyScene[current + 1];
            affTmp = (0.5 * (srcImage.getDouble(current + 1) + centerPixel)) - mMean;
            tmp1 = 65535.0 * Math.exp(c * affTmp * affTmp);

            if (tmp2 > tmp1) {
                tmp2 = tmp1;
            }

            if (tmp < tmp2) {
                tmp = tmp2;
            }
        } // if (x < (xDim - 1))

        if (y > 0) {
            tmp2 = fuzzyScene[current - xDim];
            affTmp = (0.5 * (srcImage.getDouble(current - xDim) + centerPixel)) - mMean;
            tmp1 = 65535.0 * Math.exp(c * affTmp * affTmp);

            if (tmp2 > tmp1) {
                tmp2 = tmp1;
            }

            if (tmp < tmp2) {
                tmp = tmp2;
            }
        } // if (y > 0)

        if (y < (yDim - 1)) {
            tmp2 = fuzzyScene[current + xDim];
            affTmp = (0.5 * (srcImage.getDouble(current + xDim) + centerPixel)) - mMean;
            tmp1 = 65535.0 * Math.exp(c * affTmp * affTmp);

            if (tmp2 > tmp1) {
                tmp2 = tmp1;
            }

            if (tmp < tmp2) {
                tmp = tmp2;
            }
        } // if (y < (yDim - 1))

        if (z > 0) {
            tmp2 = fuzzyScene[current - sliceSize];
            affTmp = (0.5 * (srcImage.getDouble(current - sliceSize) + centerPixel)) - mMean;
            tmp1 = 65535.0 * Math.exp(c * affTmp * affTmp);

            if (tmp2 > tmp1) {
                tmp2 = tmp1;
            }

            if (tmp < tmp2) {
                tmp = tmp2;
            }
        } // if (z > 0)

        if (z < (zDim - 1)) {
            tmp2 = fuzzyScene[current + sliceSize];
            affTmp = (0.5 * (srcImage.getDouble(current + sliceSize) + centerPixel)) - mMean;
            tmp1 = 65535.0 * Math.exp(c * affTmp * affTmp);

            if (tmp2 > tmp1) {
                tmp2 = tmp1;
            }

            if (tmp < tmp2) {
                tmp = tmp2;
            }
        } // if (z < (zDim - 1))

        return tmp;
    }

    /**
     * DOCUMENT ME!
     * 
     * @param current index of point
     * @param xDim DOCUMENT ME!
     * @param yDim DOCUMENT ME!
     * 
     * @return tmp
     */
    private double findStrongPathColor(final int current, final int xDim, final int yDim) {
        double tmp = 0;
        double tmp1;
        double tmp2;
        final double[] centerPixel = new double[3];
        final double[] neighbor = new double[3];
        int x, y;

        centerPixel[0] = redBuffer[current];
        centerPixel[1] = greenBuffer[current];
        centerPixel[2] = blueBuffer[current];
        x = current % xDim;
        y = current / xDim;

        if (x > 0) {
            tmp = fuzzyScene[current - 1];
            neighbor[0] = redBuffer[current - 1];
            neighbor[1] = greenBuffer[current - 1];
            neighbor[2] = blueBuffer[current - 1];
            tmp1 = fuzzyAffinityColor(neighbor, centerPixel);

            if (tmp > tmp1) {
                tmp = tmp1;
            }
        } // if (x > 0)

        if (x < (xDim - 1)) {
            tmp2 = fuzzyScene[current + 1];
            neighbor[0] = redBuffer[current + 1];
            neighbor[1] = greenBuffer[current + 1];
            neighbor[2] = blueBuffer[current + 1];
            tmp1 = fuzzyAffinityColor(neighbor, centerPixel);

            if (tmp2 > tmp1) {
                tmp2 = tmp1;
            }

            if (tmp < tmp2) {
                tmp = tmp2;
            }
        } // if (x < (xDim - 1))

        if (y > 0) {
            tmp2 = fuzzyScene[current - xDim];
            neighbor[0] = redBuffer[current - xDim];
            neighbor[1] = greenBuffer[current - xDim];
            neighbor[2] = blueBuffer[current - xDim];
            tmp1 = fuzzyAffinityColor(neighbor, centerPixel);

            if (tmp2 > tmp1) {
                tmp2 = tmp1;
            }

            if (tmp < tmp2) {
                tmp = tmp2;
            }
        } // if (y > 0)

        if (y < (yDim - 1)) {
            tmp2 = fuzzyScene[current + xDim];
            neighbor[0] = redBuffer[current + xDim];
            neighbor[1] = greenBuffer[current + xDim];
            neighbor[2] = blueBuffer[current + xDim];
            tmp1 = fuzzyAffinityColor(neighbor, centerPixel);

            if (tmp2 > tmp1) {
                tmp2 = tmp1;
            }

            if (tmp < tmp2) {
                tmp = tmp2;
            }
        } // if (y < (yDim - 1))

        return tmp;
    }

    /**
     * DOCUMENT ME!
     * 
     * @param current index of point
     * @param xDim DOCUMENT ME!
     * @param yDim DOCUMENT ME!
     * @param zDim DOCUMENT ME!
     * 
     * @return tmp
     */
    private double findStrongPathColor(final int current, final int xDim, final int yDim, final int zDim) {
        double tmp = 0;
        double tmp1;
        double tmp2;
        final double[] centerPixel = new double[3];
        final double[] neighbor = new double[3];
        int x, y, z;
        int sliceSize;

        centerPixel[0] = redBuffer[current];
        centerPixel[1] = greenBuffer[current];
        centerPixel[2] = blueBuffer[current];
        sliceSize = xDim * yDim;
        x = current % xDim;
        y = (current % sliceSize) / xDim;
        z = current / sliceSize;

        if (x > 0) {
            tmp = fuzzyScene[current - 1];
            neighbor[0] = redBuffer[current - 1];
            neighbor[1] = greenBuffer[current - 1];
            neighbor[2] = blueBuffer[current - 1];
            tmp1 = fuzzyAffinityColor(neighbor, centerPixel);

            if (tmp > tmp1) {
                tmp = tmp1;
            }
        } // if (x > 0)

        if (x < (xDim - 1)) {
            tmp2 = fuzzyScene[current + 1];
            neighbor[0] = redBuffer[current + 1];
            neighbor[1] = greenBuffer[current + 1];
            neighbor[2] = blueBuffer[current + 1];
            tmp1 = fuzzyAffinityColor(neighbor, centerPixel);

            if (tmp2 > tmp1) {
                tmp2 = tmp1;
            }

            if (tmp < tmp2) {
                tmp = tmp2;
            }
        } // if (x < (xDim - 1))

        if (y > 0) {
            tmp2 = fuzzyScene[current - xDim];
            neighbor[0] = redBuffer[current - xDim];
            neighbor[1] = greenBuffer[current - xDim];
            neighbor[2] = blueBuffer[current - xDim];
            tmp1 = fuzzyAffinityColor(neighbor, centerPixel);

            if (tmp2 > tmp1) {
                tmp2 = tmp1;
            }

            if (tmp < tmp2) {
                tmp = tmp2;
            }
        } // if (y > 0)

        if (y < (yDim - 1)) {
            tmp2 = fuzzyScene[current + xDim];
            neighbor[0] = redBuffer[current + xDim];
            neighbor[1] = greenBuffer[current + xDim];
            neighbor[2] = blueBuffer[current + xDim];
            tmp1 = fuzzyAffinityColor(neighbor, centerPixel);

            if (tmp2 > tmp1) {
                tmp2 = tmp1;
            }

            if (tmp < tmp2) {
                tmp = tmp2;
            }
        } // if (y < (yDim - 1))

        if (z > 0) {
            tmp2 = fuzzyScene[current - sliceSize];
            neighbor[0] = redBuffer[current - sliceSize];
            neighbor[1] = greenBuffer[current - sliceSize];
            neighbor[2] = blueBuffer[current - sliceSize];
            tmp1 = fuzzyAffinityColor(neighbor, centerPixel);

            if (tmp2 > tmp1) {
                tmp2 = tmp1;
            }

            if (tmp < tmp2) {
                tmp = tmp2;
            }
        } // if (z > 0)

        if (z < (zDim - 1)) {
            tmp2 = fuzzyScene[current + sliceSize];
            neighbor[0] = redBuffer[current + sliceSize];
            neighbor[1] = greenBuffer[current + sliceSize];
            neighbor[2] = blueBuffer[current + sliceSize];
            tmp1 = fuzzyAffinityColor(neighbor, centerPixel);

            if (tmp2 > tmp1) {
                tmp2 = tmp1;
            }

            if (tmp < tmp2) {
                tmp = tmp2;
            }
        } // if (z < (zDim - 1))

        return tmp;
    }

    /**
     * DOCUMENT ME!
     * 
     * @param f1 DOCUMENT ME!
     * @param f2 DOCUMENT ME!
     * 
     * @return result
     */
    private double fuzzyAffinity(final double f1, final double f2) {
        double tmp1;
        double result;

        tmp1 = (0.5 * (f1 + f2)) - mMean;
        result = 65535.0 * Math.exp(c * tmp1 * tmp1);

        return result;
    }

    /**
     * DOCUMENT ME!
     * 
     * @param f1 DOCUMENT ME!
     * @param f2 DOCUMENT ME!
     * 
     * @return result
     */
    private double fuzzyAffinityColor(final double[] f1, final double[] f2) {
        final double[] save = new double[3];
        double s00, s01, s02, s11, s12, s22;
        double tmp1;
        double result;

        save[0] = (0.5 * (f1[0] + f2[0])) - mRMean;
        save[1] = (0.5 * (f1[1] + f2[1])) - mGMean;
        save[2] = (0.5 * (f1[2] + f2[2])) - mBMean;

        s00 = save[0] * save[0];
        s01 = save[0] * save[1];
        s02 = save[0] * save[2];
        s11 = save[1] * save[1];
        s12 = save[1] * save[2];
        s22 = save[2] * save[2];

        tmp1 = (s00 * varInverse[0][0]) + (s11 * varInverse[1][1]) + (s22 * varInverse[2][2])
                + (s01 * (varInverse[0][1] + varInverse[1][0])) + (s02 * (varInverse[0][2] + varInverse[2][0]))
                + (s12 * (varInverse[1][2] + varInverse[2][1]));

        result = 65535.0 * Math.exp( -0.5 * tmp1);

        return result;
    }

}
