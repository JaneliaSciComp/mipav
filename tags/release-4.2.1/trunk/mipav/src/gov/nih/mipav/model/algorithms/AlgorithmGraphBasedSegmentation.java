package gov.nih.mipav.model.algorithms;


import gov.nih.mipav.model.algorithms.filters.*;
import gov.nih.mipav.model.file.*;
import gov.nih.mipav.model.structures.*;

import gov.nih.mipav.view.*;

import java.io.*;

import java.util.*;


/**
 * DOCUMENT ME!
 */
public class AlgorithmGraphBasedSegmentation extends AlgorithmBase {

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** DOCUMENT ME! */
    private ModelImage featureImage = null;

    /** Minimum component size enforced by post-processing. */
    private int minComponentSize = 20;

    /** Used to smooth the input image before segmenting it. */
    private float sigma = 0.5f;

    /** Value for the threshold function Larger values for threshold result in larger components in the result. */
    private float thresh = 500.0f;

    /** DOCUMENT ME! */
    private boolean useGreen = false;

    /**
     * This code is a port of the image segmentation C++ source code found at Pedro F. Felzenszwalb's website at
     * http://people.cs.uchicago.edu/~pff/. The implementation of this segmentation algorithm is described in: Efficient
     * Graph-Based Image Segmentation by Pedro F. Felzenszwalb and Danile P. Huttenlocher, Internation Journal of
     * Computer Vision, Vol. 59, No. 2, September, 2004.
     */
    private boolean useRed = false;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Creates a new AlgorithmGraphBasedSegmentation object.
     *
     * @param  srcImage          original image
     * @param  sigma             DOCUMENT ME!
     * @param  thresh            DOCUMENT ME!
     * @param  minComponentSize  DOCUMENT ME!
     */
    public AlgorithmGraphBasedSegmentation(ModelImage srcImage, float sigma, float thresh, int minComponentSize) {

        super(null, srcImage);
        this.sigma = sigma;
        this.thresh = thresh;
        this.minComponentSize = minComponentSize;
    }


    /**
     * Creates a new AlgorithmGraphBasedSegmentation object.
     *
     * @param  resultImage       segmented image
     * @param  srcImage          original image
     * @param  sigma             DOCUMENT ME!
     * @param  thresh            DOCUMENT ME!
     * @param  minComponentSize  DOCUMENT ME!
     */
    public AlgorithmGraphBasedSegmentation(ModelImage resultImage, ModelImage srcImage, float sigma, float thresh,
                                           int minComponentSize) {

        super(resultImage, srcImage);
        this.sigma = sigma;
        this.thresh = thresh;
        this.minComponentSize = minComponentSize;
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
     * Accessor to get srcImage.
     *
     * @return  DOCUMENT ME!
     */
    public ModelImage getImage() {
        return srcImage;
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
                segment3Colors2D();
            } else if (numColors == 2) {
                segment2Colors2D();
            }
        } // if (srcImage.isColorImage())
        else {
            segmentBW2D();
        }
    }

    /**
     * DOCUMENT ME!
     *
     * @param   a     DOCUMENT ME!
     * @param   key   DOCUMENT ME!
     * @param   low   DOCUMENT ME!
     * @param   high  DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    private static int binarySearch(float[] a, float key, int low, int high) {

        while (low <= high) {
            int mid = (low + high) >> 1;
            float midVal = a[mid];

            int cmp;

            if (midVal < key) {
                cmp = -1; // Neither val is NaN, thisVal is smaller
            } else if (midVal > key) {
                cmp = 1; // Neither val is NaN, thisVal is larger
            } else {
                int midBits = Float.floatToIntBits(midVal);
                int keyBits = Float.floatToIntBits(key);
                cmp = ((midBits == keyBits) ? 0 : // Values are equal
                    ((midBits < keyBits) ? -1 : // (-0.0, 0.0) or (!NaN, NaN)
                    1)); // (0.0, -0.0) or (NaN, !NaN)
            }

            if (cmp < 0) {
                low = mid + 1;
            } else if (cmp > 0) {
                high = mid - 1;
            } else {
                return mid; // key found
            }
        }

        return -(low + 1); // key not found.
    }

    /**
     * Returns the index of the median of the three indexed floats.
     *
     * @param   x  DOCUMENT ME!
     * @param   a  DOCUMENT ME!
     * @param   b  DOCUMENT ME!
     * @param   c  DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    private static int med3(float[] x, int a, int b, int c) {
        return ((x[a] < x[b]) ? ((x[b] < x[c]) ? b : ((x[a] < x[c]) ? c : a))
                              : ((x[b] > x[c]) ? b : ((x[a] > x[c]) ? c : a)));
    }

    /**
     * Sorts the specified sub-array of floats into ascending order.
     *
     * @param  x    DOCUMENT ME!
     * @param  off  DOCUMENT ME!
     * @param  len  DOCUMENT ME!
     * @param  x2   DOCUMENT ME!
     * @param  x3   DOCUMENT ME!
     */
    private static void sort1(float[] x, int off, int len, int[] x2, int[] x3) {

        // Insertion sort on smallest arrays
        if (len < 7) {

            for (int i = off; i < (len + off); i++) {

                for (int j = i; (j > off) && (x[j - 1] > x[j]); j--) {
                    swap(x, j, j - 1, x2, x3);
                }
            }

            return;
        }

        // Choose a partition element, v
        int m = off + (len >> 1); // Small arrays, middle element

        if (len > 7) {
            int l = off;
            int n = off + len - 1;

            if (len > 40) { // Big arrays, pseudomedian of 9

                int s = len / 8;
                l = med3(x, l, l + s, l + (2 * s));
                m = med3(x, m - s, m, m + s);
                n = med3(x, n - (2 * s), n - s, n);
            }

            m = med3(x, l, m, n); // Mid-size, med of 3
        }

        float v = x[m];

        // Establish Invariant: v* (<v)* (>v)* v*
        int a = off, b = a, c = off + len - 1, d = c;

        while (true) {

            while ((b <= c) && (x[b] <= v)) {

                if (x[b] == v) {
                    swap(x, a++, b, x2, x3);
                }

                b++;
            }

            while ((c >= b) && (x[c] >= v)) {

                if (x[c] == v) {
                    swap(x, c, d--, x2, x3);
                }

                c--;
            }

            if (b > c) {
                break;
            }

            swap(x, b++, c--, x2, x3);
        }

        // Swap partition elements back to middle
        int s, n = off + len;
        s = Math.min(a - off, b - a);
        vecswap(x, off, b - s, s, x2, x3);
        s = Math.min(d - c, n - d - 1);
        vecswap(x, b, n - s, s, x2, x3);

        // Recursively sort non-partition-elements
        if ((s = b - a) > 1) {
            sort1(x, off, s, x2, x3);
        }

        if ((s = d - c) > 1) {
            sort1(x, n - s, s, x2, x3);
        }
    }

    /**
     * DOCUMENT ME!
     *
     * @param  a          DOCUMENT ME!
     * @param  fromIndex  DOCUMENT ME!
     * @param  toIndex    DOCUMENT ME!
     * @param  a2         DOCUMENT ME!
     * @param  a3         DOCUMENT ME!
     */
    private static void sort2(float[] a, int fromIndex, int toIndex, int[] a2, int[] a3) {
        final int NEG_ZERO_BITS = Float.floatToIntBits(-0.0f);
        /*
         * The sort is done in three phases to avoid the expense of using NaN and -0.0 aware comparisons during the main
         * sort.
         */

        /*
         * Preprocessing phase:  Move any NaN's to end of array, count the number of -0.0's, and turn them into 0.0's.
         */
        int numNegZeros = 0;
        int i = fromIndex, n = toIndex;

        while (i < n) {

            if (a[i] != a[i]) {
                float swap = a[i];
                int swap2 = a2[i];
                int swap3 = a3[i];
                n--;
                a[i] = a[n];
                a2[i] = a2[n];
                a3[i] = a3[n];
                a[n] = swap;
                a2[n] = swap2;
                a3[n] = swap3;
            } else {

                if ((a[i] == 0) && (Float.floatToIntBits(a[i]) == NEG_ZERO_BITS)) {
                    a[i] = 0.0f;
                    numNegZeros++;
                }

                i++;
            }
        }

        // Main sort phase: quicksort everything but the NaN's
        sort1(a, fromIndex, n - fromIndex, a2, a3);

        // Postprocessing phase: change 0.0's to -0.0's as required
        if (numNegZeros != 0) {
            int j = binarySearch(a, 0.0f, fromIndex, n - 1); // posn of ANY zero

            do {
                j--;
            } while ((j >= 0) && (a[j] == 0.0f));

            // j is now one less than the index of the FIRST zero
            for (int k = 0; k < numNegZeros; k++) {
                a[++j] = -0.0f;
            }
        }
    }

    /**
     * Swaps x[a] with x[b].
     *
     * @param  x   DOCUMENT ME!
     * @param  a   DOCUMENT ME!
     * @param  b   DOCUMENT ME!
     * @param  x2  DOCUMENT ME!
     * @param  x3  DOCUMENT ME!
     */
    private static void swap(float[] x, int a, int b, int[] x2, int[] x3) {
        float t = x[a];
        int t2 = x2[a];
        int t3 = x3[a];
        x[a] = x[b];
        x2[a] = x2[b];
        x3[a] = x3[b];
        x[b] = t;
        x2[b] = t2;
        x3[b] = t3;
    }

    /**
     * Swaps x[a .. (a+n-1)] with x[b .. (b+n-1)].
     *
     * @param  x   DOCUMENT ME!
     * @param  a   DOCUMENT ME!
     * @param  b   DOCUMENT ME!
     * @param  n   DOCUMENT ME!
     * @param  x2  DOCUMENT ME!
     * @param  x3  DOCUMENT ME!
     */
    private static void vecswap(float[] x, int a, int b, int n, int[] x2, int[] x3) {

        for (int i = 0; i < n; i++, a++, b++) {
            swap(x, a, b, x2, x3);
        }
    }

    /**
     * DOCUMENT ME!
     */
    private void segment2Colors2D() {

        // The number of connected components in the segmented image
        int numSegComponents = 0;
        ModelImage smoothImage;
        boolean wholeImage;
        boolean image25D;
        int[] edgesA;
        int[] edgesB;
        float[] edgesW;
        int pos1;
        int pos2;
        double diff1;
        double diff2;
        int num;
        int xDim;
        int yDim;
        int sliceLength;
        float[] c1Buffer;
        float[] c2Buffer;
        int x;
        int y;
        float[] smoothSigmas;
        AlgorithmGaussianBlurSep gaussAlgo;
        int[] eltsRank;
        int[] eltsSize;
        int[] eltsP;
        int i;
        float[] threshold;
        int a;
        int b;
        RandomNumberGen randomGen;
        byte[] redByteBuffer;
        byte[] greenByteBuffer;
        byte[] blueByteBuffer;
        byte[] segBuffer;
        int comp;
        FileInfoBase[] fInfoBase;
        int[] extents;
        String srcName;

        try {
            

            fireProgressStateChanged(srcImage.getImageName(), "Performing graph based segmentation...");


            xDim = srcImage.getExtents()[0];
            yDim = srcImage.getExtents()[1];
            sliceLength = xDim * yDim;
            extents = srcImage.getExtents();
            srcName = srcImage.getImageName();

            c1Buffer = new float[sliceLength];
            c2Buffer = new float[sliceLength];

            if (useRed) {
                srcImage.exportRGBData(1, 0, sliceLength, c1Buffer);

                if (useGreen) {
                    srcImage.exportRGBData(2, 0, sliceLength, c2Buffer);
                } else {
                    srcImage.exportRGBData(3, 0, sliceLength, c2Buffer);
                }
            } else {
                srcImage.exportRGBData(2, 0, sliceLength, c1Buffer);
                srcImage.exportRGBData(3, 0, sliceLength, c2Buffer);
            }


            // Create a smoothing kernel
            smoothSigmas = new float[2];
            smoothSigmas[0] = sigma;
            smoothSigmas[1] = sigma;
            wholeImage = true;
            image25D = false;

            smoothImage = new ModelImage(ModelImage.FLOAT, srcImage.getExtents(), "smoothedImage");
            smoothImage.importData(0, c1Buffer, true);
            gaussAlgo = new AlgorithmGaussianBlurSep(smoothImage, smoothSigmas, wholeImage, image25D);
            gaussAlgo.run();
            gaussAlgo.finalize();
            gaussAlgo = null;
            smoothImage.exportData(0, sliceLength, c1Buffer);

            smoothImage.importData(0, c2Buffer, true);
            gaussAlgo = new AlgorithmGaussianBlurSep(smoothImage, smoothSigmas, wholeImage, image25D);
            gaussAlgo.run();
            gaussAlgo.finalize();
            gaussAlgo = null;
            smoothImage.exportData(0, sliceLength, c2Buffer);
            smoothImage.disposeLocal();
            smoothImage = null;

            // Build graph
            fireProgressStateChanged(20);
            edgesA = new int[4 * sliceLength];
            edgesB = new int[4 * sliceLength];
            edgesW = new float[4 * sliceLength];
            num = 0;

            for (y = 0; y < yDim; y++) {

                for (x = 0; x < xDim; x++) {

                    if (x < (xDim - 1)) {
                        pos1 = (y * xDim) + x;
                        pos2 = pos1 + 1;
                        edgesA[num] = pos1;
                        edgesB[num] = pos2;
                        diff1 = c1Buffer[pos1] - c1Buffer[pos2];
                        diff2 = c2Buffer[pos1] - c2Buffer[pos2];
                        edgesW[num++] = (float) Math.sqrt((diff1 * diff1) + (diff2 * diff2));
                    } // if (x < xDim - 1)

                    if (y < (yDim - 1)) {
                        pos1 = (y * xDim) + x;
                        pos2 = pos1 + xDim;
                        edgesA[num] = pos1;
                        edgesB[num] = pos2;
                        diff1 = c1Buffer[pos1] - c1Buffer[pos2];
                        diff2 = c2Buffer[pos1] - c2Buffer[pos2];
                        edgesW[num++] = (float) Math.sqrt((diff1 * diff1) + (diff2 * diff2));
                    } // if (y < yDim - 1)

                    if ((x < (xDim - 1)) && (y < (yDim - 1))) {
                        pos1 = (y * xDim) + x;
                        pos2 = pos1 + xDim + 1;
                        edgesA[num] = pos1;
                        edgesB[num] = pos2;
                        diff1 = c1Buffer[pos1] - c1Buffer[pos2];
                        diff2 = c2Buffer[pos1] - c2Buffer[pos2];
                        edgesW[num++] = (float) Math.sqrt((diff1 * diff1) + (diff2 * diff2));
                    } // if ((x < xDim - 1) && (y < yDim - 1))

                    if ((x < (xDim - 1)) && (y > 0)) {
                        pos1 = (y * xDim) + x;
                        pos2 = pos1 - xDim + 1;
                        edgesA[num] = pos1;
                        edgesB[num] = pos2;
                        diff1 = c1Buffer[pos1] - c1Buffer[pos2];
                        diff2 = c2Buffer[pos1] - c2Buffer[pos2];
                        edgesW[num++] = (float) Math.sqrt((diff1 * diff1) + (diff2 * diff2));
                    } // if ((x < xDim - 1) && (y > 0))
                } // for (x = 0; x < xDim; x++)
            } // for (y = 0; y < yDim; y++)

            // Sort edges by non-decreasing edge weight
            fireProgressStateChanged(40);
            sort2(edgesW, 0, num, edgesA, edgesB);

            // Make a disjoint-set forest
            fireProgressStateChanged(60);
            numSegComponents = sliceLength;
            eltsRank = new int[sliceLength];
            eltsSize = new int[sliceLength];
            Arrays.fill(eltsSize, 1);
            eltsP = new int[sliceLength];

            for (i = 0; i < sliceLength; i++) {
                eltsP[i] = i;
            }

            // Initialize thresholds
            threshold = new float[sliceLength];
            Arrays.fill(threshold, thresh);

            // For each edge, in non-decreasing weight order
            for (i = 0; i < num; i++) {
                y = edgesA[i];

                while (y != eltsP[y]) {
                    y = eltsP[y];
                }

                eltsP[edgesA[i]] = y;
                a = y;

                y = edgesB[i];

                while (y != eltsP[y]) {
                    y = eltsP[y];
                }

                eltsP[edgesB[i]] = y;
                b = y;

                if (a != b) {

                    if ((edgesW[i] <= threshold[a]) && (edgesW[i] <= threshold[b])) {

                        if (eltsRank[a] > eltsRank[b]) {
                            eltsP[b] = a;
                            eltsSize[a] += eltsSize[b];
                        } else {
                            eltsP[a] = b;
                            eltsSize[b] += eltsSize[a];

                            if (eltsRank[a] == eltsRank[b]) {
                                eltsRank[b]++;
                            }
                        }

                        numSegComponents--;

                        y = a;

                        while (y != eltsP[y]) {
                            y = eltsP[y];
                        }

                        eltsP[a] = y;
                        a = y;

                        threshold[a] = edgesW[i] + (thresh / eltsSize[a]);
                    } // if ((edgesW[i] <= threshold[a]) && (edgesW[i] <= threshold[b]))
                } // if (a != b)
            } // for (i = 0; i < num; i++)

            threshold = null;

            // post process small components
            fireProgressStateChanged(80);

            for (i = 0; i < num; i++) {
                y = edgesA[i];

                while (y != eltsP[y]) {
                    y = eltsP[y];
                }

                eltsP[edgesA[i]] = y;
                a = y;

                y = edgesB[i];

                while (y != eltsP[y]) {
                    y = eltsP[y];
                }

                eltsP[edgesB[i]] = y;
                b = y;

                if ((a != b) && (eltsSize[a] < minComponentSize) && (eltsSize[b] < minComponentSize)) {

                    if (eltsRank[a] > eltsRank[b]) {
                        eltsP[b] = a;
                        eltsSize[a] += eltsSize[b];
                    } else {
                        eltsP[a] = b;
                        eltsSize[b] += eltsSize[a];

                        if (eltsRank[a] == eltsRank[b]) {
                            eltsRank[b]++;
                        }
                    }

                    numSegComponents--;
                }
            } // for (i = 0; i < num; i++)

            edgesA = null;
            edgesB = null;
            edgesW = null;

            // Pick random colors for each component
            randomGen = new RandomNumberGen();
            redByteBuffer = new byte[sliceLength];
            greenByteBuffer = new byte[sliceLength];
            blueByteBuffer = new byte[sliceLength];

            for (i = 0; i < sliceLength; i++) {
                redByteBuffer[i] = (byte) randomGen.genUniformRandomNum(0, 255);
                greenByteBuffer[i] = (byte) randomGen.genUniformRandomNum(0, 255);
                blueByteBuffer[i] = (byte) randomGen.genUniformRandomNum(0, 255);
            }

            segBuffer = new byte[4 * sliceLength];

            for (y = 0; y < yDim; y++) {

                for (x = 0; x < xDim; x++) {
                    pos1 = (y * xDim) + x;
                    comp = pos1;

                    while (comp != eltsP[comp]) {
                        comp = eltsP[comp];
                    }

                    eltsP[pos1] = comp;
                    segBuffer[(4 * pos1) + 1] = redByteBuffer[comp];
                    segBuffer[(4 * pos1) + 2] = greenByteBuffer[comp];
                    segBuffer[(4 * pos1) + 3] = blueByteBuffer[comp];
                }
            }

            if (destImage != null) {
                destImage.importData(0, segBuffer, true);
            } else {
                fInfoBase = new FileInfoBase[1];
                fInfoBase[0] = (FileInfoBase) (srcImage.getFileInfo(0).clone());
                fInfoBase[0].setDataType(ModelStorageBase.ARGB);

                if (srcImage.getParentFrame() != null) {
                    srcImage.getParentFrame().close();
                }

                srcImage.disposeLocal();
                srcImage = null;

                srcImage = new ModelImage(ModelStorageBase.ARGB, extents, srcName);
                srcImage.setFileInfo(fInfoBase[0], 0);
                srcImage.importData(0, segBuffer, true);
            }

            setCompleted(true);

            return;
        } // try
        catch (IOException ioe) {
            finalize();
            System.gc();
            MipavUtil.displayError("AlgorithmGraphBasedSegmentation reports:\n" + ioe.toString());


            setCompleted(false);

            return;
        } catch (OutOfMemoryError error) {
            finalize();
            System.gc();
            MipavUtil.displayError("AlgorithmGraphBasedSegmentation reports:\n" + error.toString());


            setCompleted(false);

            return;
        }
    }

    /**
     * DOCUMENT ME!
     */
    private void segment3Colors2D() {

        // The number of connected components in the segmented image
        int numSegComponents = 0;
        ModelImage smoothImage;
        boolean wholeImage;
        boolean image25D;
        int[] edgesA;
        int[] edgesB;
        float[] edgesW;
        int pos1;
        int pos2;
        double diffR;
        double diffG;
        double diffB;
        int num;
        int xDim;
        int yDim;
        int sliceLength;
        float[] redBuffer;
        float[] greenBuffer;
        float[] blueBuffer;
        int x;
        int y;
        float[] smoothSigmas;
        AlgorithmGaussianBlurSep gaussAlgo;
        int[] eltsRank;
        int[] eltsSize;
        int[] eltsP;
        int i;
        float[] threshold;
        int a;
        int b;
        RandomNumberGen randomGen;
        byte[] redByteBuffer;
        byte[] greenByteBuffer;
        byte[] blueByteBuffer;
        byte[] segBuffer;
        int comp;
        FileInfoBase[] fInfoBase;
        int[] extents;
        String srcName;


        try {
            

            fireProgressStateChanged(srcImage.getImageName(), "Performing graph based segmentation...");


            xDim = srcImage.getExtents()[0];
            yDim = srcImage.getExtents()[1];
            sliceLength = xDim * yDim;
            extents = srcImage.getExtents();
            srcName = srcImage.getImageName();

            redBuffer = new float[sliceLength];
            greenBuffer = new float[sliceLength];
            blueBuffer = new float[sliceLength];
            srcImage.exportRGBData(1, 0, sliceLength, redBuffer);
            srcImage.exportRGBData(2, 0, sliceLength, greenBuffer);
            srcImage.exportRGBData(3, 0, sliceLength, blueBuffer);

            // Create a smoothing kernel
            smoothSigmas = new float[2];
            smoothSigmas[0] = sigma;
            smoothSigmas[1] = sigma;
            wholeImage = true;
            image25D = false;

            smoothImage = new ModelImage(ModelImage.FLOAT, srcImage.getExtents(), "smoothedImage");
            smoothImage.importData(0, redBuffer, true);
            gaussAlgo = new AlgorithmGaussianBlurSep(smoothImage, smoothSigmas, wholeImage, image25D);
            gaussAlgo.run();
            gaussAlgo.finalize();
            gaussAlgo = null;
            smoothImage.exportData(0, sliceLength, redBuffer);

            smoothImage.importData(0, greenBuffer, true);
            gaussAlgo = new AlgorithmGaussianBlurSep(smoothImage, smoothSigmas, wholeImage, image25D);
            gaussAlgo.run();
            gaussAlgo.finalize();
            gaussAlgo = null;
            smoothImage.exportData(0, sliceLength, greenBuffer);

            smoothImage.importData(0, blueBuffer, true);
            gaussAlgo = new AlgorithmGaussianBlurSep(smoothImage, smoothSigmas, wholeImage, image25D);
            gaussAlgo.run();
            gaussAlgo.finalize();
            gaussAlgo = null;
            smoothImage.exportData(0, sliceLength, blueBuffer);
            smoothImage.disposeLocal();
            smoothImage = null;

            // Build graph
            fireProgressStateChanged(20);
            edgesA = new int[4 * sliceLength];
            edgesB = new int[4 * sliceLength];
            edgesW = new float[4 * sliceLength];
            num = 0;

            for (y = 0; y < yDim; y++) {

                for (x = 0; x < xDim; x++) {

                    if (x < (xDim - 1)) {
                        pos1 = (y * xDim) + x;
                        pos2 = pos1 + 1;
                        edgesA[num] = pos1;
                        edgesB[num] = pos2;
                        diffR = redBuffer[pos1] - redBuffer[pos2];
                        diffG = greenBuffer[pos1] - greenBuffer[pos2];
                        diffB = blueBuffer[pos1] - blueBuffer[pos2];
                        edgesW[num++] = (float) Math.sqrt((diffR * diffR) + (diffG * diffG) + (diffB * diffB));
                    } // if (x < xDim - 1)

                    if (y < (yDim - 1)) {
                        pos1 = (y * xDim) + x;
                        pos2 = pos1 + xDim;
                        edgesA[num] = pos1;
                        edgesB[num] = pos2;
                        diffR = redBuffer[pos1] - redBuffer[pos2];
                        diffG = greenBuffer[pos1] - greenBuffer[pos2];
                        diffB = blueBuffer[pos1] - blueBuffer[pos2];
                        edgesW[num++] = (float) Math.sqrt((diffR * diffR) + (diffG * diffG) + (diffB * diffB));
                    } // if (y < yDim - 1)

                    if ((x < (xDim - 1)) && (y < (yDim - 1))) {
                        pos1 = (y * xDim) + x;
                        pos2 = pos1 + xDim + 1;
                        edgesA[num] = pos1;
                        edgesB[num] = pos2;
                        diffR = redBuffer[pos1] - redBuffer[pos2];
                        diffG = greenBuffer[pos1] - greenBuffer[pos2];
                        diffB = blueBuffer[pos1] - blueBuffer[pos2];
                        edgesW[num++] = (float) Math.sqrt((diffR * diffR) + (diffG * diffG) + (diffB * diffB));
                    } // if ((x < xDim - 1) && (y < yDim - 1))

                    if ((x < (xDim - 1)) && (y > 0)) {
                        pos1 = (y * xDim) + x;
                        pos2 = pos1 - xDim + 1;
                        edgesA[num] = pos1;
                        edgesB[num] = pos2;
                        diffR = redBuffer[pos1] - redBuffer[pos2];
                        diffG = greenBuffer[pos1] - greenBuffer[pos2];
                        diffB = blueBuffer[pos1] - blueBuffer[pos2];
                        edgesW[num++] = (float) Math.sqrt((diffR * diffR) + (diffG * diffG) + (diffB * diffB));
                    } // if ((x < xDim - 1) && (y > 0))
                } // for (x = 0; x < xDim; x++)
            } // for (y = 0; y < yDim; y++)

            // Sort edges by non-decreasing edge weight
            fireProgressStateChanged(40);
            sort2(edgesW, 0, num, edgesA, edgesB);

            // Make a disjoint-set forest
            fireProgressStateChanged(60);
            numSegComponents = sliceLength;
            eltsRank = new int[sliceLength];
            eltsSize = new int[sliceLength];
            Arrays.fill(eltsSize, 1);
            eltsP = new int[sliceLength];

            for (i = 0; i < sliceLength; i++) {
                eltsP[i] = i;
            }

            // Initialize thresholds
            threshold = new float[sliceLength];
            Arrays.fill(threshold, thresh);

            // For each edge, in non-decreasing weight order
            for (i = 0; i < num; i++) {
                y = edgesA[i];

                while (y != eltsP[y]) {
                    y = eltsP[y];
                }

                eltsP[edgesA[i]] = y;
                a = y;

                y = edgesB[i];

                while (y != eltsP[y]) {
                    y = eltsP[y];
                }

                eltsP[edgesB[i]] = y;
                b = y;

                if (a != b) {

                    if ((edgesW[i] <= threshold[a]) && (edgesW[i] <= threshold[b])) {

                        if (eltsRank[a] > eltsRank[b]) {
                            eltsP[b] = a;
                            eltsSize[a] += eltsSize[b];
                        } else {
                            eltsP[a] = b;
                            eltsSize[b] += eltsSize[a];

                            if (eltsRank[a] == eltsRank[b]) {
                                eltsRank[b]++;
                            }
                        }

                        numSegComponents--;

                        y = a;

                        while (y != eltsP[y]) {
                            y = eltsP[y];
                        }

                        eltsP[a] = y;
                        a = y;

                        threshold[a] = edgesW[i] + (thresh / eltsSize[a]);
                    } // if ((edgesW[i] <= threshold[a]) && (edgesW[i] <= threshold[b]))
                } // if (a != b)
            } // for (i = 0; i < num; i++)

            threshold = null;

            // post process small components
            fireProgressStateChanged(80);

            for (i = 0; i < num; i++) {
                y = edgesA[i];

                while (y != eltsP[y]) {
                    y = eltsP[y];
                }

                eltsP[edgesA[i]] = y;
                a = y;

                y = edgesB[i];

                while (y != eltsP[y]) {
                    y = eltsP[y];
                }

                eltsP[edgesB[i]] = y;
                b = y;

                if ((a != b) && (eltsSize[a] < minComponentSize) && (eltsSize[b] < minComponentSize)) {

                    if (eltsRank[a] > eltsRank[b]) {
                        eltsP[b] = a;
                        eltsSize[a] += eltsSize[b];
                    } else {
                        eltsP[a] = b;
                        eltsSize[b] += eltsSize[a];

                        if (eltsRank[a] == eltsRank[b]) {
                            eltsRank[b]++;
                        }
                    }

                    numSegComponents--;
                }
            } // for (i = 0; i < num; i++)

            edgesA = null;
            edgesB = null;
            edgesW = null;

            // Pick random colors for each component
            randomGen = new RandomNumberGen();
            redByteBuffer = new byte[sliceLength];
            greenByteBuffer = new byte[sliceLength];
            blueByteBuffer = new byte[sliceLength];

            for (i = 0; i < sliceLength; i++) {
                redByteBuffer[i] = (byte) randomGen.genUniformRandomNum(0, 255);
                greenByteBuffer[i] = (byte) randomGen.genUniformRandomNum(0, 255);
                blueByteBuffer[i] = (byte) randomGen.genUniformRandomNum(0, 255);
            }

            segBuffer = new byte[4 * sliceLength];

            for (y = 0; y < yDim; y++) {

                for (x = 0; x < xDim; x++) {
                    pos1 = (y * xDim) + x;
                    comp = pos1;

                    while (comp != eltsP[comp]) {
                        comp = eltsP[comp];
                    }

                    eltsP[pos1] = comp;
                    segBuffer[(4 * pos1) + 1] = redByteBuffer[comp];
                    segBuffer[(4 * pos1) + 2] = greenByteBuffer[comp];
                    segBuffer[(4 * pos1) + 3] = blueByteBuffer[comp];
                }
            }

            if (destImage != null) {
                destImage.importData(0, segBuffer, true);
            } else {
                fInfoBase = new FileInfoBase[1];
                fInfoBase[0] = (FileInfoBase) (srcImage.getFileInfo(0).clone());
                fInfoBase[0].setDataType(ModelStorageBase.ARGB);

                if (srcImage.getParentFrame() != null) {
                    srcImage.getParentFrame().close();
                }

                srcImage.disposeLocal();
                srcImage = null;

                srcImage = new ModelImage(ModelStorageBase.ARGB, extents, srcName);
                srcImage.setFileInfo(fInfoBase[0], 0);
                srcImage.importData(0, segBuffer, true);
            }

            setCompleted(true);

            return;
        } // try
        catch (IOException ioe) {
            finalize();
            System.gc();
            MipavUtil.displayError("AlgorithmGraphBasedSegmentation reports:\n" + ioe.toString());


            setCompleted(false);

            return;
        } catch (OutOfMemoryError error) {
            finalize();
            System.gc();
            MipavUtil.displayError("AlgorithmGraphBasedSegmentation reports:\n" + error.toString());

            setCompleted(false);

            return;
        }

    }

    /**
     * DOCUMENT ME!
     */
    private void segmentBW2D() {

        // The number of connected components in the segmented image
        int numSegComponents = 0;
        ModelImage smoothImage;
        boolean wholeImage;
        boolean image25D;
        int[] edgesA;
        int[] edgesB;
        float[] edgesW;
        int pos1;
        int pos2;
        int num;
        int xDim;
        int yDim;
        int sliceLength;
        float[] buffer;
        int x;
        int y;
        float[] smoothSigmas;
        AlgorithmGaussianBlurSep gaussAlgo;
        int[] eltsRank;
        int[] eltsSize;
        int[] eltsP;
        int i;
        float[] threshold;
        int a;
        int b;
        RandomNumberGen randomGen;
        byte[] redByteBuffer;
        byte[] greenByteBuffer;
        byte[] blueByteBuffer;
        byte[] segBuffer;
        int comp;
        FileInfoBase[] fInfoBase;
        int[] extents;
        String srcName;

        try {
            

            fireProgressStateChanged(srcImage.getImageName(), "Performing graph based segmentation...");


            xDim = srcImage.getExtents()[0];
            yDim = srcImage.getExtents()[1];
            sliceLength = xDim * yDim;
            extents = srcImage.getExtents();
            srcName = srcImage.getImageName();

            buffer = new float[sliceLength];
            srcImage.exportData(0, sliceLength, buffer);

            // Create a smoothing kernel
            smoothSigmas = new float[2];
            smoothSigmas[0] = sigma;
            smoothSigmas[1] = sigma;
            wholeImage = true;
            image25D = false;

            smoothImage = new ModelImage(ModelImage.FLOAT, srcImage.getExtents(), "smoothedImage");
            smoothImage.importData(0, buffer, true);
            gaussAlgo = new AlgorithmGaussianBlurSep(smoothImage, smoothSigmas, wholeImage, image25D);
            gaussAlgo.run();
            gaussAlgo.finalize();
            gaussAlgo = null;
            smoothImage.exportData(0, sliceLength, buffer);
            smoothImage.disposeLocal();
            smoothImage = null;

            // Build graph
            fireProgressStateChanged(20);
            edgesA = new int[4 * sliceLength];
            edgesB = new int[4 * sliceLength];
            edgesW = new float[4 * sliceLength];
            num = 0;

            for (y = 0; y < yDim; y++) {

                for (x = 0; x < xDim; x++) {

                    if (x < (xDim - 1)) {
                        pos1 = (y * xDim) + x;
                        pos2 = pos1 + 1;
                        edgesA[num] = pos1;
                        edgesB[num] = pos2;
                        edgesW[num++] = Math.abs(buffer[pos1] - buffer[pos2]);
                    } // if (x < xDim - 1)

                    if (y < (yDim - 1)) {
                        pos1 = (y * xDim) + x;
                        pos2 = pos1 + xDim;
                        edgesA[num] = pos1;
                        edgesB[num] = pos2;
                        edgesW[num++] = Math.abs(buffer[pos1] - buffer[pos2]);
                    } // if (y < yDim - 1)

                    if ((x < (xDim - 1)) && (y < (yDim - 1))) {
                        pos1 = (y * xDim) + x;
                        pos2 = pos1 + xDim + 1;
                        edgesA[num] = pos1;
                        edgesB[num] = pos2;
                        edgesW[num++] = Math.abs(buffer[pos1] - buffer[pos2]);
                    } // if ((x < xDim - 1) && (y < yDim - 1))

                    if ((x < (xDim - 1)) && (y > 0)) {
                        pos1 = (y * xDim) + x;
                        pos2 = pos1 - xDim + 1;
                        edgesA[num] = pos1;
                        edgesB[num] = pos2;
                        edgesW[num++] = Math.abs(buffer[pos1] - buffer[pos2]);
                    } // if ((x < xDim - 1) && (y > 0))
                } // for (x = 0; x < xDim; x++)
            } // for (y = 0; y < yDim; y++)

            // Sort edges by non-decreasing edge weight
            fireProgressStateChanged(40);
            sort2(edgesW, 0, num, edgesA, edgesB);

            // Make a disjoint-set forest
            fireProgressStateChanged(60);
            numSegComponents = sliceLength;
            eltsRank = new int[sliceLength];
            eltsSize = new int[sliceLength];
            Arrays.fill(eltsSize, 1);
            eltsP = new int[sliceLength];

            for (i = 0; i < sliceLength; i++) {
                eltsP[i] = i;
            }

            // Initialize thresholds
            threshold = new float[sliceLength];
            Arrays.fill(threshold, thresh);

            // For each edge, in non-decreasing weight order
            for (i = 0; i < num; i++) {
                y = edgesA[i];

                while (y != eltsP[y]) {
                    y = eltsP[y];
                }

                eltsP[edgesA[i]] = y;
                a = y;

                y = edgesB[i];

                while (y != eltsP[y]) {
                    y = eltsP[y];
                }

                eltsP[edgesB[i]] = y;
                b = y;

                if (a != b) {

                    if ((edgesW[i] <= threshold[a]) && (edgesW[i] <= threshold[b])) {

                        if (eltsRank[a] > eltsRank[b]) {
                            eltsP[b] = a;
                            eltsSize[a] += eltsSize[b];
                        } else {
                            eltsP[a] = b;
                            eltsSize[b] += eltsSize[a];

                            if (eltsRank[a] == eltsRank[b]) {
                                eltsRank[b]++;
                            }
                        }

                        numSegComponents--;

                        y = a;

                        while (y != eltsP[y]) {
                            y = eltsP[y];
                        }

                        eltsP[a] = y;
                        a = y;

                        threshold[a] = edgesW[i] + (thresh / eltsSize[a]);
                    } // if ((edgesW[i] <= threshold[a]) && (edgesW[i] <= threshold[b]))
                } // if (a != b)
            } // for (i = 0; i < num; i++)

            threshold = null;

            // post process small components
            fireProgressStateChanged(80);

            for (i = 0; i < num; i++) {
                y = edgesA[i];

                while (y != eltsP[y]) {
                    y = eltsP[y];
                }

                eltsP[edgesA[i]] = y;
                a = y;

                y = edgesB[i];

                while (y != eltsP[y]) {
                    y = eltsP[y];
                }

                eltsP[edgesB[i]] = y;
                b = y;

                if ((a != b) && (eltsSize[a] < minComponentSize) && (eltsSize[b] < minComponentSize)) {

                    if (eltsRank[a] > eltsRank[b]) {
                        eltsP[b] = a;
                        eltsSize[a] += eltsSize[b];
                    } else {
                        eltsP[a] = b;
                        eltsSize[b] += eltsSize[a];

                        if (eltsRank[a] == eltsRank[b]) {
                            eltsRank[b]++;
                        }
                    }

                    numSegComponents--;
                }
            } // for (i = 0; i < num; i++)

            edgesA = null;
            edgesB = null;
            edgesW = null;

            // Pick random colors for each component
            randomGen = new RandomNumberGen();
            redByteBuffer = new byte[sliceLength];
            greenByteBuffer = new byte[sliceLength];
            blueByteBuffer = new byte[sliceLength];

            for (i = 0; i < sliceLength; i++) {
                redByteBuffer[i] = (byte) randomGen.genUniformRandomNum(0, 255);
                greenByteBuffer[i] = (byte) randomGen.genUniformRandomNum(0, 255);
                blueByteBuffer[i] = (byte) randomGen.genUniformRandomNum(0, 255);
            }

            segBuffer = new byte[4 * sliceLength];

            for (y = 0; y < yDim; y++) {

                for (x = 0; x < xDim; x++) {
                    pos1 = (y * xDim) + x;
                    comp = pos1;

                    while (comp != eltsP[comp]) {
                        comp = eltsP[comp];
                    }

                    eltsP[pos1] = comp;
                    segBuffer[(4 * pos1) + 1] = redByteBuffer[comp];
                    segBuffer[(4 * pos1) + 2] = greenByteBuffer[comp];
                    segBuffer[(4 * pos1) + 3] = blueByteBuffer[comp];
                }
            }

            if (destImage != null) {
                destImage.importData(0, segBuffer, true);
            } else {
                fInfoBase = new FileInfoBase[1];
                fInfoBase[0] = (FileInfoBase) (srcImage.getFileInfo(0).clone());
                fInfoBase[0].setDataType(ModelStorageBase.ARGB);

                if (srcImage.getParentFrame() != null) {
                    srcImage.getParentFrame().close();
                }

                srcImage.disposeLocal();
                srcImage = null;

                srcImage = new ModelImage(ModelStorageBase.ARGB, extents, srcName);
                srcImage.setFileInfo(fInfoBase[0], 0);
                srcImage.importData(0, segBuffer, true);
            }

            setCompleted(true);

            return;
        } // try
        catch (IOException ioe) {
            finalize();
            System.gc();
            MipavUtil.displayError("AlgorithmGraphBasedSegmentation reports:\n" + ioe.toString());

            setCompleted(false);

            return;
        } catch (OutOfMemoryError error) {
            finalize();
            System.gc();
            MipavUtil.displayError("AlgorithmGraphBasedSegmentation reports:\n" + error.toString());

            setCompleted(false);

            return;
        }
    }


}
