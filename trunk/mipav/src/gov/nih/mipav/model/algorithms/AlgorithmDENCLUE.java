package gov.nih.mipav.model.algorithms;


import gov.nih.mipav.model.structures.*;

import gov.nih.mipav.view.*;

import java.awt.*;

import java.io.*;


/**
 * References: 1.) Data Mining Concepts and Techniques by Jiawei Han and Micheline Kamber Section 8.6.3 DENCLUE:
 * Clustering Based on Density Distribution Functions, pp. 366- 369, 2001. 2.) An Efficient Approach to Clustering in
 * Large Multimedia Databases with Noise by Alexander Hinneburg and Daniel A. Keim, Proc. 1998 Int. Conf. Knowledge
 * Discovery and Data Mining, pages 58-65, New York, August, 1998.
 */
public class AlgorithmDENCLUE extends AlgorithmBase {

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /**
     * If isGaussian is true, this is the standard deviation in the Gaussian influence function If a square wave
     * function, all pixels sum out to this distance, zero past this distance.
     */
    private float distance;

    /**
     * If isArbitrary is true, create arbitrary shaped clusters If isArbitrary is false, create center defined clusters.
     */
    private boolean isArbitrary;

    /** If true, use a Gaussian influence function If false, use a square wave influence function. */
    private boolean isGaussian;

    /** density function must equal or exceed this value to be part of a cluster. */
    private float threshold;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * This constructor initialises a Color Edge algorithm for a source and destination image, and ensures that the
     * destination image is <code>ModelStorageBase.UBYTE</code>.
     *
     * <p>Currently (25 May 2006), this algorithm does not support replacing the original data set with that of the
     * color edge image.</p>
     *
     * @param  dest         DOCUMENT ME!
     * @param  src          DOCUMENT ME!
     * @param  isGaussian   If true, Gaussian influence function If false, Square wave influence function
     * @param  distance     if isGaussian is true, the standard deviation in the Gaussian influence function. If a
     *                      square wave function, all pixels sum out to this distance, zero past this density
     * @param  threshold    Density function must equal or exceed this value to be part of a cluster
     * @param  isArbitrary  If isArbitrary is true, create arbitrary shaped clusters If isArbitrary is false, create
     *                      center defined clusters
     */
    public AlgorithmDENCLUE(ModelImage dest, ModelImage src, boolean isGaussian, float distance, float threshold,
                            boolean isArbitrary) {
        super(dest, src);

        this.isGaussian = isGaussian;
        this.distance = distance;
        this.threshold = threshold;
        this.isArbitrary = isArbitrary;
    }

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * Prepares this class for destruction.
     */
    public void finalize() {
        super.finalize();


    }

    /**
     * Standard algorithm run method. It will not run if the source Image is <code>null</code>. The calculation is done
     * and placed in a separate destination image if it is to be stored there.
     */
    public void runAlgorithm() {

        if (srcImage == null) {
            displayError("Source Image is null");

            return;
        }

        

        if (destImage != null) { // if there exists a destination image

            if (srcImage.getNDims() == 2) {
                calcStoreInDest2D();
            } else {
                calcStoreInDest3D();
            }
        } else { // there is no image but the original source.
            calcInPlace();
        }
    }


    /**
     * Filters the source image. Replaces the original image with the filtered image.
     *
     * <p><em>Does not currently work.</em></p>
     */
    private void calcInPlace() {
        errorCleanUp("AlgorithmDENCLUE: " + "Replace Image not yet implemented", false);
        finalize();

        return;
    }

    /**
     * This function produces a 2D density based clustering image that does not replace the original image-data.
     */
    private void calcStoreInDest2D() {

        int xDim = srcImage.getExtents()[0];
        int yDim = srcImage.getExtents()[1];
        int length = xDim * yDim;
        float[] buffer;
        float[] density;

        // float gradX[] = null;
        // float gradY[] = null;
        int maxDistance;
        int x, y;
        int j, k;
        int index;
        int indexS;
        int distInt;
        double localPart;
        ModelImage grayImage;
        AlgorithmVOIExtraction algoVOIExtraction;
        byte[] byteBuffer;
        Color[] colorTable;
        boolean found;
        int centerIter = 0;


        fireProgressStateChanged("Finding density cluster in " + srcImage.getImageName(), "DENCLUE..");


        buffer = new float[length];
        density = new float[length];

        if (!isArbitrary) {
            // gradX = new float[length];
            // gradY = new float[length];
        }

        try {
            srcImage.exportData(0, length, buffer);
        } catch (IOException e) {
            MipavUtil.displayError("Error on srcImage.exportData");

            setCompleted(false);
        }

        if (isGaussian) {
            maxDistance = Math.max(1, (int) ((5.0 * distance) + 0.5f));

            for (y = 0; y < yDim; y++) {

                for (x = 0; x < xDim; x++) {
                    index = (y * xDim) + x;

                    for (k = y - maxDistance; k <= (y + maxDistance); k++) {

                        if ((k >= 0) && (k < yDim)) {

                            for (j = x - maxDistance; j <= (x + maxDistance); j++) {

                                if ((j >= 0) && (j < xDim)) {
                                    indexS = (k * xDim) + j;
                                    localPart = buffer[indexS] *
                                                    Math.exp(-(((k - y) * (k - y)) + ((j - x) * (j - x))) /
                                                                 (2.0 * distance * distance));
                                    density[index] += (float) localPart;
                                    // if (!isArbitrary) { gradX[index] += (float)(localPart * (j - x)); gradY[index] +=
                                    // (float)(localPart * (k - y)); }
                                }
                            }
                        }
                    }
                }
            }
        } // if (isGaussian)
        else { // Square wave
            distInt = (int) distance;

            for (y = 0; y < yDim; y++) {

                for (x = 0; x < xDim; x++) {
                    index = (y * xDim) + x;

                    for (k = y - distInt; k <= (y + distInt); k++) {

                        if ((k >= 0) && (k < yDim)) {

                            for (j = x - distInt; j <= (x + distInt); j++) {

                                if ((j >= 0) && (j < xDim)) {

                                    if ((((k - y) * (k - y)) + ((j - x) * (j - x))) <= (distance * distance)) {
                                        indexS = (k * xDim) + j;
                                        density[index] += buffer[indexS];
                                        // if (!isArbitrary) {
                                        // gradX[index] += (buffer[indexS] * (j - x));
                                        // gradY[index] += (buffer[indexS] * (k - y));
                                        // }
                                    }
                                }
                            }
                        }
                    }
                }
            }
        } // else Square wave

        byteBuffer = new byte[length];

        for (index = 0; index < length; index++) {

            if (density[index] >= threshold) {
                byteBuffer[index] = 1;
            }
        }

        if (!isArbitrary) {
            found = true;

            while (found) {
                centerIter++;
                fireProgressStateChanged("Center based iteration " + centerIter);
                found = false;

                for (y = 0; y < yDim; y++) {

                    for (x = 0; x < xDim; x++) {
                        index = (y * xDim) + x;

                        if (byteBuffer[index] == 0) {

                            for (k = y - 1; k <= (y + 1); k++) {

                                if ((k >= 0) && (k < yDim)) {

                                    for (j = x - 1; j <= (x + 1); j++) {

                                        if ((j >= 0) && (j < xDim)) {
                                            indexS = (k * xDim) + j;

                                            if ((byteBuffer[indexS] == 1) && (density[indexS] >= density[index])) {
                                                byteBuffer[index] = 1;
                                                found = true;
                                            }
                                        }
                                    }
                                }
                            }
                        } // if (byteBuffer[index] == 0)
                    } // for (x = 0; x < xDim; x++)
                } // for (y = 0; y < yDim; y++)
            } // while (found)
        } // if (!isArbitrary)

        // gradX = null;
        // gradY = null;

        try {
            destImage.importData(0, density, true);
        } catch (IOException error) {
            byteBuffer = null;
            errorCleanUp("Error on destImage.importData", true);

            return;
        }

        density = null;

        grayImage = new ModelImage(ModelStorageBase.BYTE, srcImage.getExtents(), srcImage.getImageName() + "_gray");

        try {
            grayImage.importData(0, byteBuffer, true);
        } catch (IOException error) {
            byteBuffer = null;
            errorCleanUp("Error on grayImage.importData", true);

            return;
        }

        fireProgressStateChanged("Extracting VOIs");
        fireProgressStateChanged(80);
        colorTable = new Color[1];
        colorTable[0] = Color.RED;
        algoVOIExtraction = new AlgorithmVOIExtraction(grayImage);
        algoVOIExtraction.setColorTable(colorTable);
        algoVOIExtraction.run();
        algoVOIExtraction.finalize();
        algoVOIExtraction = null;
        System.gc();

        srcImage.setVOIs(grayImage.getVOIs());
        Preferences.debug(srcImage.getVOIs().size() + " VOIs put in source image\n");
        grayImage.disposeLocal();
        grayImage = null;


        setCompleted(true);

    } // calcStoreInDest2D()

    /**
     * This function produces a 3D density based clustering image that does not replace the original image-data.
     */
    private void calcStoreInDest3D() {

        int xDim = srcImage.getExtents()[0];
        int yDim = srcImage.getExtents()[1];
        int zDim = srcImage.getExtents()[2];
        int sliceSize = xDim * yDim;
        int length = sliceSize * zDim;
        float[] buffer;
        float[] density;
        int maxDistance;
        int x, y, z;
        int j, k, m;
        int index;
        int indexS;
        int distInt;
        double localPart;
        ModelImage grayImage;
        AlgorithmVOIExtraction algoVOIExtraction;
        byte[] byteBuffer;
        Color[] colorTable;
        boolean found;
        int centerIter = 0;


        fireProgressStateChanged("Finding density cluster in " + srcImage.getImageName(), "DENCLUE..");


        buffer = new float[length];
        density = new float[length];

        try {
            srcImage.exportData(0, length, buffer);
        } catch (IOException e) {
            MipavUtil.displayError("Error on srcImage.exportData");

            setCompleted(false);
        }

        if (isGaussian) {
            maxDistance = Math.max(1, (int) ((5.0 * distance) + 0.5f));

            for (z = 0; z < zDim; z++) {
                fireProgressStateChanged(z * 30 / zDim);

                for (y = 0; y < yDim; y++) {

                    for (x = 0; x < xDim; x++) {
                        index = (z * sliceSize) + (y * xDim) + x;

                        for (m = z - maxDistance; m <= (z + maxDistance); m++) {

                            if ((m >= 0) && (m < zDim)) {

                                for (k = y - maxDistance; k <= (y + maxDistance); k++) {

                                    if ((k >= 0) && (k < yDim)) {

                                        for (j = x - maxDistance; j <= (x + maxDistance); j++) {

                                            if ((j >= 0) && (j < xDim)) {
                                                indexS = (m * sliceSize) + (k * xDim) + j;
                                                localPart = buffer[indexS] *
                                                                Math.exp(-(((m - z) * (m - z)) + ((k - y) * (k - y)) +
                                                                               ((j - x) * (j - x))) /
                                                                             (2.0 * distance * distance));
                                                density[index] += (float) localPart;
                                            }
                                        }
                                    }
                                }
                            }
                        }
                    } // for (x = 0; x < xDim; x++)
                } // for (y = 0; y < yDim; y++)
            } // for (z = 0; z < zDim; z++)
        } // if (isGaussian)
        else { // Square wave
            distInt = (int) distance;

            for (z = 0; z < zDim; z++) {
                fireProgressStateChanged(z * 30 / zDim);

                for (y = 0; y < yDim; y++) {

                    for (x = 0; x < xDim; x++) {
                        index = (z * sliceSize) + (y * xDim) + x;

                        for (m = z - distInt; m <= (z + distInt); m++) {

                            if ((m >= 0) && (m < zDim)) {

                                for (k = y - distInt; k <= (y + distInt); k++) {

                                    if ((k >= 0) && (k < yDim)) {

                                        for (j = x - distInt; j <= (x + distInt); j++) {

                                            if ((j >= 0) && (j < xDim)) {

                                                if ((((m - z) * (m - z)) + ((k - y) * (k - y)) + ((j - x) * (j - x))) <=
                                                        (distance * distance)) {
                                                    indexS = (m * sliceSize) + (k * xDim) + j;
                                                    density[index] += buffer[indexS];
                                                }
                                            }
                                        }
                                    }
                                }
                            }
                        }
                    } // for (x = 0; x < xDim; x++)
                } // for (y = 0; y < yDim; y++)
            } // for (z = 0; z < zDim; z++)
        } // else Square wave

        byteBuffer = new byte[length];

        for (index = 0; index < length; index++) {

            if (density[index] >= threshold) {
                byteBuffer[index] = 1;
            }
        }

        if (!isArbitrary) {
            found = true;

            while (found) {
                found = false;
                centerIter++;
                fireProgressStateChanged("Center based iteration " + centerIter);

                for (z = 0; z < zDim; z++) {

                    for (y = 0; y < yDim; y++) {

                        for (x = 0; x < xDim; x++) {
                            index = (z * sliceSize) + (y * xDim) + x;

                            if (byteBuffer[index] == 0) {

                                for (m = z - 1; m <= (z + 1); m++) {

                                    if ((m >= 0) && (m < zDim)) {

                                        for (k = y - 1; k <= (y + 1); k++) {

                                            if ((k >= 0) && (k < yDim)) {

                                                for (j = x - 1; j <= (x + 1); j++) {

                                                    if ((j >= 0) && (j < xDim)) {
                                                        indexS = (m * sliceSize) + (k * xDim) + j;

                                                        if ((byteBuffer[indexS] == 1) &&
                                                                (density[indexS] >= density[index])) {
                                                            byteBuffer[index] = 1;
                                                            found = true;
                                                        }
                                                    }
                                                }
                                            }
                                        }
                                    }
                                }
                            } // if (byteBuffer[index] == 0)
                        } // for (x = 0; x < xDim; x++)
                    } // for (y = 0; y < yDim; y++)
                } // for (z = 0; z < zDim; z++)
            } // while (found)
        } // if (!isArbitrary)

        try {
            destImage.importData(0, density, true);
        } catch (IOException error) {
            byteBuffer = null;
            errorCleanUp("Error on destImage.importData", true);

            return;
        }

        density = null;

        grayImage = new ModelImage(ModelStorageBase.BYTE, srcImage.getExtents(), srcImage.getImageName() + "_gray");

        try {
            grayImage.importData(0, byteBuffer, true);
        } catch (IOException error) {
            byteBuffer = null;
            errorCleanUp("Error on grayImage.importData", true);

            return;
        }

        fireProgressStateChanged("Extracting VOIs");
        fireProgressStateChanged(80);
        colorTable = new Color[1];
        colorTable[0] = Color.RED;
        algoVOIExtraction = new AlgorithmVOIExtraction(grayImage);
        algoVOIExtraction.setColorTable(colorTable);
        algoVOIExtraction.run();
        algoVOIExtraction.finalize();
        algoVOIExtraction = null;
        System.gc();

        srcImage.setVOIs(grayImage.getVOIs());
        Preferences.debug(srcImage.getVOIs().size() + " VOIs put in source image\n");
        grayImage.disposeLocal();
        grayImage = null;


        setCompleted(true);

    } // calcStoreInDest3D()
}
