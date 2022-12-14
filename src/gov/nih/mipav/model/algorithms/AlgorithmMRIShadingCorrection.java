package gov.nih.mipav.model.algorithms;


import gov.nih.mipav.model.algorithms.filters.*;
import gov.nih.mipav.model.structures.*;

import gov.nih.mipav.view.*;

import java.io.*;

import java.util.*;


/**
 * DOCUMENT ME!
 */
public class AlgorithmMRIShadingCorrection extends AlgorithmBase {

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** Number of iterations. */
    private int iters;

    /** Normalization constant used to normalize the point force. */
    private float norm;

    /** x and y standard deviations of Gaussians used to convolve normalized forces. */
    private float scaleX;

    /** DOCUMENT ME! */
    private float scaleY;

    /** DOCUMENT ME! */
    private float thresholdLevel;

    /**
     * If true, don't use periphery pixels below threshold or pixels below threshold connected to periphery pixels below
     * threshold thru a 4 neighbor chain of below threshold pixels.
     */
    private boolean thresholdSelected;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Creates a new AlgorithmMRIShadingCorrection object.
     *
     * @param  srcImage           original image
     * @param  norm               DOCUMENT ME!
     * @param  scaleX             DOCUMENT ME!
     * @param  scaleY             DOCUMENT ME!
     * @param  iters              DOCUMENT ME!
     * @param  thresholdSelected  DOCUMENT ME!
     * @param  thresholdLevel     DOCUMENT ME!
     */
    public AlgorithmMRIShadingCorrection(ModelImage srcImage, float norm, float scaleX, float scaleY, int iters,
                                         boolean thresholdSelected, float thresholdLevel) {

        super(null, srcImage);
        this.norm = norm;
        this.scaleX = scaleX;
        this.scaleY = scaleY;
        this.iters = iters;
        this.thresholdSelected = thresholdSelected;
        this.thresholdLevel = thresholdLevel;
    }

    // The following 4 step algorithm is used to perform shading correction on
    // MRI images:
    // 1.) Calculate the probability distribution p(i,d) of image features, where
    // i is the image intensity and d is the second derivative of the image
    // intensity generated by a 3 by 3 Laplacian kernel.  The histogram (i,d)
    // is formed with partial intensity interpolation, splitting each (i,d)
    // value between 4 bins.  Then each histogram(i,d) value is divided
    // by the area to form the probability(i,d).
    //
    // 2.) Estimate the force that will condense p(i,d) along the intensity.
    // force = 1/area*derivative of ln(p(i(x,y),d(x,y))) with respect to intensity.
    // Take the log of each nonzero probability(i,d).  Use the sobel operator
    // on the log(probability(i,d)) array to obtain the derivative with respect
    // to intensity.  Use partial intensity interpolation to obtain the force(x,y)
    // as the sum of 4 sobel(i,d) bins divided by the area.
    //
    // 3.) Estimate the bias correction field at each (x,y) equal to:
    // 1 + [(normalization constant * force(x,y)/mean(abs(force(x,y))))
    // convolved with the Gaussian]
    //
    // 4.) Perform partial correction and stop if the user speicified number of
    // iterations has been reached.  Otherwise go back to step 1.
    // Multiply each point in the original image by the correction field(x,y).
    // Calculate the standard deviation of the corrected image.  Multiply
    // each point in the corrected image by the standard deviation of the
    // original image divided  by the standard deviation of the corrected
    // image to preserve the original contrast.  Calculate the mean of the
    // contrast preserved corrected image.  Add the mean of the original
    // image minus the mean of the contrast preserved corrected image to
    // each point in the contrast preserved corrected image to preserve
    // the mean intensity of the original image.
    //
    // 3D images are processed 1 2D slice at a time.

    // Reference:
    // 1.) MRI intensity inhomogeneity correction by combining intensity and
    // spatial information by Uros Vovk, Franjo Pernus, and Bostjan Likar,
    // Physics in Medicine and Biology, Vol. 49, 2004, pp. 4119-4133.

    /**
     * Creates a new AlgorithmMRIShadingCorrection object.
     *
     * @param  resultImage        shading corrected image
     * @param  srcImage           original image
     * @param  norm               DOCUMENT ME!
     * @param  scaleX             DOCUMENT ME!
     * @param  scaleY             DOCUMENT ME!
     * @param  iters              DOCUMENT ME!
     * @param  thresholdSelected  DOCUMENT ME!
     * @param  thresholdLevel     DOCUMENT ME!
     */
    public AlgorithmMRIShadingCorrection(ModelImage resultImage, ModelImage srcImage, float norm, float scaleX,
                                         float scaleY, int iters, boolean thresholdSelected, float thresholdLevel) {

        super(resultImage, srcImage);
        this.norm = norm;
        this.scaleX = scaleX;
        this.scaleY = scaleY;
        this.iters = iters;
        this.thresholdSelected = thresholdSelected;
        this.thresholdLevel = thresholdLevel;
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
     */
    public void runAlgorithm() {
        int xDim;
        int yDim;
        int zDim;
        int sliceSize;
        int area;
        float[] buffer;
        boolean[] objectBuffer = null;
        float[] buffer2;
        AlgorithmLaplacian lapAlgo = null;
        int i;
        int j;
        int x;
        int y;
        int z;
        boolean backgroundPresent = false;
        boolean found;
        String lapName;
        ModelImage image2;
        int[] extents;
        float[] lapSigmas;
        float[] sigmas;
        boolean image25D = false;
        float ampFactor = 1.0f;
        boolean wholeImage = true;
        int it;
        float imin;
        float imax;
        float lapMin;
        float lapMax;
        int numIBins = 256;
        int numLapBins = 256;
        float iScale;
        float lapScale;
        float iNum;
        int iLow;
        int iHigh;
        float lapNum;
        int lapLow;
        int lapHigh;
        float[][] histogram;
        float[][] sobel;
        float meanAbsForce;
        AlgorithmGaussianBlurSep gaussAlgo;
        double originalMean;
        double originalStdDev;
        double mean;
        double stdDev;
        float preserveContrast;
        float preserveMean;
        int imageType;

        

        fireProgressStateChanged(srcImage.getImageName(), "Performing MRI shading correction...");


        srcImage.calcMinMax();
        xDim = srcImage.getExtents()[0];
        yDim = srcImage.getExtents()[1];

        if (srcImage.getNDims() == 3) {
            zDim = srcImage.getExtents()[2];
        } else {
            zDim = 1;
        }

        sliceSize = xDim * yDim;
        area = sliceSize;

        try {
            buffer = new float[sliceSize];

            if (thresholdSelected) {
                objectBuffer = new boolean[sliceSize];
                mask = new BitSet(sliceSize);
            }

            extents = new int[2];
            extents[0] = xDim;
            extents[1] = yDim;
            lapSigmas = new float[2];

            // This will make the Laplacian operator a 3 by 3 kernel
            lapSigmas[0] = 0.3f;
            lapSigmas[1] = 0.3f;
            sigmas = new float[2];
            sigmas[0] = scaleX;
            sigmas[1] = scaleY;
            lapName = srcImage.getImageName() + "_2";
            image2 = new ModelImage(ModelImage.FLOAT, extents, lapName);
            buffer2 = new float[sliceSize];
            histogram = new float[numIBins][numLapBins];
            sobel = new float[numIBins][numLapBins];

            if (destImage != null) {
                imageType = destImage.getType();
            } else {
                imageType = srcImage.getType();
            }

            for (z = 0; z < zDim; z++) {
                srcImage.exportData(z * sliceSize, sliceSize, buffer);

                // If thresholdSelected is true, then object pixels will be
                // resresented by true in objectBuffer and background pixels
                // will be represented by false in objectBuffer.
                if (thresholdSelected) {
                    backgroundPresent = false;

                    for (i = 0; i < sliceSize; i++) {
                        objectBuffer[i] = true;
                    }

                    // Check x = 0;
                    for (i = 0; i < sliceSize; i += xDim) {

                        if (buffer[i] < thresholdLevel) {
                            objectBuffer[i] = false;
                            backgroundPresent = true;
                        }
                    } // for (i = 0; i < sliceSize; i += xDim)

                    // Check x = xDim - 1
                    for (i = xDim - 1; i < sliceSize; i += xDim) {

                        if (buffer[i] < thresholdLevel) {
                            objectBuffer[i] = false;
                            backgroundPresent = true;
                        }
                    } // for (i = xDim - 1; i < sliceSize; i += xDim)

                    // Check y = 0
                    for (i = 0; i < xDim; i++) {

                        if (buffer[i] < thresholdLevel) {
                            objectBuffer[i] = false;
                            backgroundPresent = true;
                        }
                    } // for (i = 0; i < xDim; i++)

                    // Check y = yDim - 1
                    for (i = xDim * (yDim - 1); i < sliceSize; i++) {

                        if (buffer[i] < thresholdLevel) {
                            objectBuffer[i] = false;
                            backgroundPresent = true;
                        }
                    } // for (i = xDim*(yDim-1); i < sliceSize; i++)

                    if (backgroundPresent) {
                        found = true;

                        while (found) {
                            found = false;

                            for (y = 1; y < (yDim - 1); y++) {
                                j = y * xDim;

                                for (x = 1; x < (xDim - 1); x++) {
                                    i = j + x;

                                    if ((objectBuffer[i]) && (buffer[i] < thresholdLevel) &&
                                            ((!objectBuffer[i - 1]) || (!objectBuffer[i + 1]) ||
                                                 (!objectBuffer[i - xDim]) || (!objectBuffer[i + xDim]))) {
                                        objectBuffer[i] = false;
                                        found = true;
                                    }
                                } // for (x = 1; x < xDim - 1; x++)
                            } // for (y = 1; y < yDim - 1; y++)
                        } // while (found)

                        area = 0;

                        for (i = 0; i < sliceSize; i++) {

                            if (objectBuffer[i]) {
                                area++;
                            }
                        }

                        for (i = 0; i < sliceSize; i++) {

                            if (objectBuffer[i]) {
                                mask.set(i);
                            } else {
                                mask.clear(i);
                            }
                        }

                    } // if (backgroundPresent)
                } // if (thresholdSelected)

                originalMean = 0.0;

                for (i = 0; i < sliceSize; i++) {

                    if ((!backgroundPresent) || (objectBuffer[i])) {
                        originalMean += buffer[i];
                    }
                }

                originalMean = originalMean / area;

                originalStdDev = 0.0;

                for (i = 0; i < sliceSize; i++) {

                    if ((!backgroundPresent) || (objectBuffer[i])) {
                        originalStdDev += (buffer[i] - originalMean) * (buffer[i] - originalMean);
                    }
                }

                originalStdDev = originalStdDev / (area - 1);
                originalStdDev = Math.sqrt(originalStdDev);

                for (it = 1; it <= iters; it++) {
                    fireProgressStateChanged(100 * ((iters * z) + it) / (zDim * iters));

                    // The Laplacian of the buffer will be placed in buffer2
                    image2.importData(0, buffer, true);

                    if (backgroundPresent) {
                        lapAlgo = new AlgorithmLaplacian(image2, lapSigmas, objectBuffer, image25D, ampFactor);
                    } else {
                        lapAlgo = new AlgorithmLaplacian(image2, lapSigmas, wholeImage, image25D, ampFactor);
                    }

                    lapAlgo.run();
                    lapAlgo.finalize();
                    lapAlgo = null;
                    image2.exportData(0, sliceSize, buffer2);

                    imin = Float.MAX_VALUE;
                    imax = -Float.MAX_VALUE;
                    lapMin = Float.MAX_VALUE;
                    lapMax = -Float.MAX_VALUE;

                    for (i = 0; i < sliceSize; i++) {

                        if ((!backgroundPresent) || (objectBuffer[i])) {

                            if (buffer[i] < imin) {
                                imin = buffer[i];
                            }

                            if (buffer[i] > imax) {
                                imax = buffer[i];
                            }

                            if (buffer2[i] < lapMin) {
                                lapMin = buffer2[i];
                            }

                            if (buffer2[i] > lapMax) {
                                lapMax = buffer2[i];
                            }
                        } // if ((!backgroundPresent) || (objectBuffer[i]))
                    } // for (i = 0; i < sliceSize; i++)

                    iScale = (numIBins - 1) / (imax - imin);
                    lapScale = (numLapBins - 1) / (lapMax - lapMin);

                    for (i = 0; i < numIBins; i++) {

                        for (j = 0; j < numLapBins; j++) {
                            histogram[i][j] = 0.0f;
                        }
                    }

                    // Using partial intensity interpolation, split each (intensity,
                    // Laplacian) count between 4 histogram[intensity][Laplacian] bins.
                    for (i = 0; i < sliceSize; i++) {

                        if ((!backgroundPresent) || (objectBuffer[i])) {
                            iNum = (buffer[i] - imin) * iScale;

                            if (iNum >= (numIBins - 1)) {
                                iNum = numIBins - 1;
                            }

                            iLow = (int) iNum;
                            iHigh = iLow + 1;
                            lapNum = (buffer2[i] - lapMin) * lapScale;

                            if (lapNum >= (numLapBins - 1)) {
                                lapNum = numLapBins - 1;
                            }

                            lapLow = (int) lapNum;
                            lapHigh = lapLow + 1;
                            histogram[iLow][lapLow] += (iHigh - iNum) * (lapHigh - lapNum);

                            if (lapHigh <= (numLapBins - 1)) {
                                histogram[iLow][lapHigh] += (iHigh - iNum) * (lapNum - lapLow);
                            }

                            if (iHigh <= (numIBins - 1)) {
                                histogram[iHigh][lapLow] += (iNum - iLow) * (lapHigh - lapNum);
                            }

                            if ((iHigh <= (numIBins - 1)) && (lapHigh <= (numLapBins - 1))) {
                                histogram[iHigh][lapHigh] += (iNum - iLow) * (lapNum - lapLow);
                            }
                        } // if ((!backgroundPresent) || (objectBuffer[i]))
                    } // for (i = 0; i < sliceSize; i++)

                    // Obtain probability by dividing by the total area count
                    // Then obtain log of probability;
                    for (i = 0; i < numIBins; i++) {

                        for (j = 0; j < numLapBins; j++) {
                            histogram[i][j] = histogram[i][j] / area;

                            if (histogram[i][j] > 0.0f) {
                                histogram[i][j] = (float) Math.log((double) histogram[i][j]);
                            }

                            sobel[i][j] = 0.0f;
                        }
                    }

                    // Use the 3 by 3 Sobel kernel to obtain the derivative of the
                    // log(probability(intensity,Laplacian)) with respect to the
                    // intensity.  This is equal to the force times the area.
                    for (i = 1; i < (numIBins - 1); i++) {
                        sobel[i][0] = (-2.0f * histogram[i - 1][0]) - histogram[i - 1][1] +
                                      (2.0f * histogram[i + 1][0]) + histogram[i + 1][1];
                        sobel[i][numLapBins - 1] = -histogram[i - 1][numLapBins - 2] -
                                                   (2.0f * histogram[i - 1][numLapBins - 1]) +
                                                   histogram[i + 1][numLapBins - 2] +
                                                   (2.0f * histogram[i + 1][numLapBins - 1]);

                        for (j = 1; j < (numLapBins - 1); j++) {
                            sobel[i][j] = -histogram[i - 1][j - 1] - (2.0f * histogram[i - 1][j]) -
                                          histogram[i - 1][j + 1] + histogram[i + 1][j - 1] +
                                          (2.0f * histogram[i + 1][j]) + histogram[i + 1][j + 1];
                        }
                    }

                    // On loop entry buffer2 has the Laplacian.  On loop exit buffer2
                    // has the force*area.  Use partial intensity interpolation to obtain
                    // the force*area from 4 sobel bins.
                    // Obtain the mean of the absolute value of the force*area
                    meanAbsForce = 0.0f;

                    for (i = 0; i < sliceSize; i++) {

                        if ((!backgroundPresent) || (objectBuffer[i])) {
                            iNum = (buffer[i] - imin) * iScale;

                            if (iNum >= (numIBins - 1)) {
                                iNum = numIBins - 1;
                            }

                            iLow = (int) iNum;
                            iHigh = iLow + 1;
                            lapNum = (buffer2[i] - lapMin) * lapScale;

                            if (lapNum >= (numLapBins - 1)) {
                                lapNum = numLapBins - 1;
                            }

                            lapLow = (int) lapNum;
                            lapHigh = lapLow + 1;
                            buffer2[i] = (iHigh - iNum) * (lapHigh - lapNum) * sobel[iLow][lapLow];

                            if (lapHigh <= (numLapBins - 1)) {
                                buffer2[i] += (iHigh - iNum) * (lapNum - lapLow) * sobel[iLow][lapHigh];
                            }

                            if (iHigh <= (numIBins - 1)) {
                                buffer2[i] += (iNum - iLow) * (lapHigh - lapNum) * sobel[iHigh][lapLow];
                            }

                            if ((iHigh <= (numIBins - 1)) && (lapHigh <= (numLapBins - 1))) {
                                buffer2[i] += (iNum - iLow) * (lapNum - lapLow) * sobel[iHigh][lapHigh];
                            }

                            meanAbsForce += Math.abs(buffer2[i]);
                        } // if ((!backgroundPresent) || (objectBuffer[i]))
                        else {
                            buffer2[i] = 0.0f;
                        }
                    } // for (i = 0; i < sliceSize; i++)

                    meanAbsForce = meanAbsForce / area;

                    // Place the norm * force*area / mean(abs(force*area)) in buffer2
                    for (i = 0; i < sliceSize; i++) {

                        if ((!backgroundPresent) || (objectBuffer[i])) {
                            buffer2[i] = norm * buffer2[i] / meanAbsForce;
                        }
                    }

                    // Convolve buffer2 with the user specified Gaussian kernel
                    image2.importData(0, buffer2, true);

                    if (backgroundPresent) {
                        gaussAlgo = new AlgorithmGaussianBlurSep(image2, sigmas, false, false);
                        gaussAlgo.setMask(mask);
                    } else {
                        gaussAlgo = new AlgorithmGaussianBlurSep(image2, sigmas, true, false);
                    }

                    gaussAlgo.run();
                    image2.importData(0, gaussAlgo.getResultBuffer(), true);
                    gaussAlgo.finalize();
                    gaussAlgo = null;

                    image2.exportData(0, sliceSize, buffer2);

                    // buffer2 will have the bias correction field
                    mean = 0.0;

                    for (i = 0; i < sliceSize; i++) {

                        if ((!backgroundPresent) || (objectBuffer[i])) {
                            buffer2[i] += 1.0f;
                            buffer[i] = buffer[i] * buffer2[i];
                            mean += buffer[i];
                        }
                    }

                    mean = mean / area;

                    stdDev = 0.0;

                    for (i = 0; i < sliceSize; i++) {

                        if ((!backgroundPresent) || (objectBuffer[i])) {
                            stdDev += (buffer[i] - mean) * (buffer[i] - mean);
                        }
                    }

                    stdDev = stdDev / (area - 1);
                    stdDev = Math.sqrt(stdDev);
                    preserveContrast = (float) (originalStdDev / stdDev);

                    mean = 0.0;

                    for (i = 0; i < sliceSize; i++) {

                        if ((!backgroundPresent) || (objectBuffer[i])) {
                            buffer[i] = buffer[i] * preserveContrast;
                            mean += buffer[i];
                        }
                    }

                    mean = mean / area;
                    preserveMean = (float) (originalMean - mean);

                    for (i = 0; i < sliceSize; i++) {

                        if ((!backgroundPresent) || (objectBuffer[i])) {
                            buffer[i] += preserveMean;

                            switch (imageType) {

                                case ModelStorageBase.BYTE:
                                    if (buffer[i] < -128.0f) {
                                        buffer[i] = -128.0f;
                                    } else if (buffer[i] > 127.0f) {
                                        buffer[i] = 127.0f;
                                    }

                                    break;

                                case ModelStorageBase.UBYTE:
                                    if (buffer[i] < 0.0f) {
                                        buffer[i] = 0.0f;
                                    } else if (buffer[i] > 255.0f) {
                                        buffer[i] = 255.0f;
                                    }

                                    break;

                                case ModelStorageBase.SHORT:
                                    if (buffer[i] < -32768.0f) {
                                        buffer[i] = -32768.0f;
                                    } else if (buffer[i] > 32767.0f) {
                                        buffer[i] = 32767.0f;
                                    }

                                    break;

                                case ModelStorageBase.USHORT:
                                    if (buffer[i] < 0.0f) {
                                        buffer[i] = 0.0f;
                                    } else if (buffer[i] > 65535.0f) {
                                        buffer[i] = 65535.0f;
                                    }

                                    break;

                                case ModelStorageBase.INTEGER:
                                    if (buffer[i] < Integer.MIN_VALUE) {
                                        buffer[i] = Integer.MIN_VALUE;
                                    } else if (buffer[i] > Integer.MAX_VALUE) {
                                        buffer[i] = Integer.MAX_VALUE;
                                    }

                                    break;

                                case ModelStorageBase.UINTEGER:
                                    if (buffer[i] < 0.0f) {
                                        buffer[i] = 0.0f;
                                    } else if (buffer[i] > 4294967295L) {
                                        buffer[i] = 4294967295L;
                                    }

                                    break;

                                case ModelStorageBase.ARGB:
                                    if (buffer[i] < 0.0f) {
                                        buffer[i] = 0.0f;
                                    } else if (buffer[i] > 255.0f) {
                                        buffer[i] = 255.0f;
                                    }

                                    break;

                                case ModelStorageBase.ARGB_USHORT:
                                    if (buffer[i] < 0.0f) {
                                        buffer[i] = 0.0f;
                                    } else if (buffer[i] > 65535.0f) {
                                        buffer[i] = 65535.0f;
                                    }

                                    break;
                            } // switch(imageType)
                        }
                    }

                } // for (it = 1; it <= iters; it++)

                if (destImage != null) {
                    destImage.importData(z * sliceSize, buffer, false);
                } else {
                    srcImage.importData(z * sliceSize, buffer, false);
                }
            } // for (z = 0; z < zDim; z++)

            if (destImage != null) {
                destImage.calcMinMax();
            } else {
                srcImage.calcMinMax();
            }

            setCompleted(true);

            return;

        } // try
        catch (IOException ioe) {
            finalize();
            System.gc();
            MipavUtil.displayError("Algorithm MRI shading correction reports:\n" + ioe.toString());


            setCompleted(false);

            return;
        } catch (OutOfMemoryError error) {
            finalize();
            System.gc();
            MipavUtil.displayError("Algorithm MRI shading correction reports:\n" + error.toString());


            setCompleted(false);

            return;
        }


    }
}
