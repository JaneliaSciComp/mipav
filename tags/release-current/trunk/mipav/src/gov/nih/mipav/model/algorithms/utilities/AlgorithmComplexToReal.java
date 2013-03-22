package gov.nih.mipav.model.algorithms.utilities;


import gov.nih.mipav.model.algorithms.*;
import gov.nih.mipav.model.file.*;
import gov.nih.mipav.model.structures.*;

import java.io.*;


/**
 * Simple algorithm that converts an RGB image to a single greyscale image.
 *
 * <p>User can specify red, green, and blue scaling factors Default is equal weighting new gray value = ( R + G + B) / 3
 * if no threshold averaging; if thresholdAverage is true, only average values above trheshold</p>
 *
 * <p>The standard for computer graphics is > Y = 0.299*R + 0.587*G + 0.114*B > > That assumes both Y and RGB are in the
 * range 0:255 > > BT.601 calls for Y to be limited to 16:235, if you want > that multiply the constants by 219/255 and
 * add 16 to Y.</p>
 *
 * <p>Yes, but Y is not pure luminance, the other components it might be PbPr, CbCr carry color information that affect
 * perceived brightness.</p>
 *
 * @author   Justin Senseney
 */
public class AlgorithmComplexToReal extends AlgorithmBase {

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** If true indicates that the result image is an average of the three channels. Default is false */
    private boolean intensityAverage = false;

    /** Weighting values for the red, green, and blue channels. Default all channels = 0.33333 */
    private float redValue = 1.0f / 3.0f, greenValue = 1.0f / 3.0f, blueValue = 1.0f / 3.0f;

    /** This value equal one third (0.3333333). */
    private float thirdValue = 1.0f / 3.0f;

    /** Only average values above threshold. */
    private float threshold = 0.0f;

    /** If true only average values above threshold. */
    private boolean thresholdAverage = false;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Creates a new AlgorithmComplexToReal object.
     *
     * @param  srcImg  source image model
     */
    public AlgorithmComplexToReal(ModelImage srcImg) {
        super(null, srcImg);
    }

    /**
     * Creates a new AlgorithmComplexToReal object.
     *
     * @param  destImg  image model where result image is to stored
     * @param  srcImg   source image model
     */
    public AlgorithmComplexToReal(ModelImage destImg, ModelImage srcImg) {
        super(destImg, srcImg);
    }

    /**
     * Creates a new AlgorithmComplexToReal object.
     *
     * @param  srcImg            source image model
     * @param  redValue          weighting of the red channel
     * @param  greenValue        weighting of the green channel
     * @param  blueValue         weighting of the blue channel
     * @param  thresholdAverage  if true only average values above threshold
     * @param  threshold         DOCUMENT ME!
     * @param  intensityAverage  DOCUMENT ME!
     */
    public AlgorithmComplexToReal(ModelImage srcImg, float redValue, float greenValue, float blueValue,
                              boolean thresholdAverage, float threshold, boolean intensityAverage) {
        super(null, srcImg);
        this.redValue = redValue;
        this.greenValue = greenValue;
        this.blueValue = blueValue;
        this.thresholdAverage = thresholdAverage;
        this.threshold = threshold;
        this.intensityAverage = intensityAverage;
    }


    /**
     * Creates a new AlgorithmComplexToReal object.
     *
     * @param  destImg           image model where result image is to stored
     * @param  srcImg            source image model
     * @param  redValue          weighting of the red channel
     * @param  greenValue        weighting of the green channel
     * @param  blueValue         weighting of the blue channel
     * @param  thresholdAverage  if true only average values above threshold
     * @param  threshold         DOCUMENT ME!
     * @param  intensityAverage  DOCUMENT ME!
     */
    public AlgorithmComplexToReal(ModelImage destImg, ModelImage srcImg, float redValue, float greenValue, float blueValue,
                              boolean thresholdAverage, float threshold, boolean intensityAverage) {
        super(destImg, srcImg);
        this.redValue = redValue;
        this.greenValue = greenValue;
        this.blueValue = blueValue;
        this.thresholdAverage = thresholdAverage;
        this.threshold = threshold;
        this.intensityAverage = intensityAverage;
    }

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * Prepares this class for destruction.
     */
    public void finalize() {
        destImage = null;
        srcImage = null;
        super.finalize();
    }

    /**
     * DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    public ModelImage getSrcImage() {
        return srcImage;
    }

    /**
     * Starts the program.
     */
    public void runAlgorithm() {

        if (srcImage == null) {
            displayError("RGBtoGray.run(): Source image is null");

            return;
        }

        if (srcImage.isColorImage() == false) {
            displayError("RGBtoGray.run(): Source Image is not a RGB type");

            return;
        }

        

        if (destImage == null) {
            calcStoreInPlace();
        } else {
            calcStoreInDest();
        }
    }

    /**
     * Calculates the gray scale image.
     */
    private void calcStoreInDest() {

        int i, j, k, m;
        int id;
        int z, t, f;
        int p;
        int totalLength;
        float sum;
        int offsetIn, offsetOut;
        int lengthIn, lengthOut; // total number of data-elements (pixels) in image
        float[] buffer; // data-buffer (for pixel data) which is the "heart" of the image\
        float[] bufferDest;
        float max;
        float remapFactor = 1.0f;
        boolean doFloat;
        int numVoxels = 0;
        float realSum = 0.0f;
        float imagSum = 0.0f;

        float averageReal = 0.0f;
        float averageImag = 0.0f;

        if (srcImage.getType() == ModelImage.ARGB_FLOAT) {
            doFloat = true;
        } else {
            doFloat = false;
        }

        try {
            lengthIn = 4 * srcImage.getSliceSize();
            lengthOut = srcImage.getSliceSize();
            buffer = new float[lengthIn];
            bufferDest = new float[lengthOut];
            fireProgressStateChanged(srcImage.getImageName(), "RGB to GRAY ...");
        } catch (OutOfMemoryError e) {
            buffer = null;
            bufferDest = null;
            System.gc();
            displayError("Algorithm RGBtoGray reports: Out of memory when creating image buffer");
            setCompleted(false);

            return;
        }

        

        int mod = lengthIn / 20;

        if (srcImage.getNDims() == 5) {
            f = srcImage.getExtents()[4];
        } else {
            f = 1;
        }

        if (srcImage.getNDims() >= 4) {
            t = srcImage.getExtents()[3];
        } else {
            t = 1;
        }

        if (srcImage.getNDims() >= 3) {
            z = srcImage.getExtents()[2];
        } else {
            z = 1;
        }

        totalLength = f * t * z * lengthIn;

        if (intensityAverage) {

            // get of max R, G, and B for remap
            max = (float) srcImage.getMaxR();

            if (srcImage.getMaxG() > max) {
                max = (float) srcImage.getMaxG();
            }

            if (srcImage.getMaxB() > max) {
                max = (float) srcImage.getMaxB();
            }

            // determine the remap factor to scale the max up to 255 (higher contrast)
            remapFactor = 255.0f / max;

            // MipavUtil.displayError("Max intensity is " + max + " and Remap Factor is " + remapFactor);

            // Run through the loop once to get Sums of Voxel Intensities for RGB
            for (m = 0; (m < f) && !threadStopped; m++) {

                for (k = 0; (k < t) && !threadStopped; k++) {

                    for (j = 0; (j < z) && !threadStopped; j++) {

                        try {
                            offsetIn = (m * t * z * lengthIn) + (k * z * lengthIn) + (j * lengthIn);
                            offsetOut = (m * t * z * lengthOut) + (k * z * lengthOut) + (j * lengthOut);
                            srcImage.exportData(offsetIn, lengthIn, buffer); // locks and releases lock
                        } catch (IOException error) {
                            buffer = null;
                            bufferDest = null;
                            displayError("Algorithm RGBtoGray : Input Image(s) locked");
                            setCompleted(false);

                            return;
                        }

                        for (i = 0, id = 0; (i < lengthIn) && !threadStopped; i += 3, id++) {

                            numVoxels++;
                            realSum += buffer[i + 1];
                            imagSum += buffer[i + 2];
                        } // end zyy
                    } // end j
                } // end t
            } // end f

            // Calculate Voxel Intensity Averages
            averageReal = realSum / numVoxels;
            averageImag = imagSum / numVoxels;

            // MipavUtil.displayError("AverageR = " + averageR + ", AverageG = " +
            // averageG + ", AverageB = " + averageB);

        }

        for (m = 0; (m < f) && !threadStopped; m++) {

            for (k = 0; (k < t) && !threadStopped; k++) {

                for (j = 0; (j < z) && !threadStopped; j++) {

                    try {
                        offsetIn = (m * t * z * lengthIn) + (k * z * lengthIn) + (j * lengthIn);
                        offsetOut = (m * t * z * lengthOut) + (k * z * lengthOut) + (j * lengthOut);
                        srcImage.exportData(offsetIn, lengthIn, buffer); // locks and releases lock
                    } catch (IOException error) {
                        buffer = null;
                        bufferDest = null;
                        displayError("Algorithm RGBtoGray : Input Image(s) locked");
                        setCompleted(false);

                        return;
                    }

                    for (i = 0, id = 0; (i < lengthIn) && !threadStopped; i += 3, id++) {

                        if (((i % mod) == 0)) {
                            fireProgressStateChanged(Math.round((float) (i + offsetIn) / (totalLength - 1) * 100));
                        }

                        if (thresholdAverage) {
                            p = 0;
                            sum = 0.0f;

                            if (buffer[i + 1] > threshold) {
                                p++;
                                sum = buffer[i + 1];
                            }

                            if (buffer[i + 2] > threshold) {
                                p++;
                                sum += buffer[i + 2];
                            }

                            if (buffer[i + 3] > threshold) {
                                p++;
                                sum += buffer[i + 3];
                            }

                            if (p == 0) {
                                bufferDest[id] = 0;
                            } else if (p == 1) {
                                bufferDest[id] = sum;
                            } else if (p == 2) {
                                bufferDest[id] = 0.5f * sum;
                            } else { // p == 3
                                bufferDest[id] = thirdValue * sum;
                            }

                            if (doFloat) {
                                bufferDest[id] = Math.round(bufferDest[id]);
                            }
                        } // if (thresholdAverage)
                        else if (intensityAverage) {
                            p = 0;
                            sum = 0.0f;

                            if (averageReal > 2.0f) {
                                p++;
                                sum = buffer[i + 1] * remapFactor;
                            }

                            if (averageImag > 2.0f) {
                                p++;
                                sum += buffer[i + 2] * remapFactor;
                            }

                            if (p == 0) {
                                bufferDest[id] = 0;
                            } else if (p == 1) {
                                bufferDest[id] = sum;
                            } else if (p == 2) {
                                bufferDest[id] = 0.5f * sum;
                            } else { // p == 3
                                bufferDest[id] = thirdValue * sum;
                            }

                            if (doFloat) {
                                bufferDest[id] = Math.round(bufferDest[id]);
                            }
                        } // if (averageIntensity)
                        else { // no thresholdAverage

                            if (doFloat) {
                                bufferDest[id] = (redValue * buffer[i + 1]) + (greenValue * buffer[i + 2]) +
                                                 (blueValue * buffer[i + 3]);
                            } else {
                                bufferDest[id] = Math.round((redValue * buffer[i + 1]) + (greenValue * buffer[i + 2]) +
                                                            (blueValue * buffer[i + 3]));
                            }
                        } // else no threshold average
                    }

                    if (threadStopped) {
                        buffer = null;
                        bufferDest = null;
                        finalize();

                        return;
                    }

                    try {
                        destImage.importData(offsetOut, bufferDest, false);
                    } catch (IOException error) {
                        displayError("Algorithm RGBtoGray: Output Image(s) locked");
                        setCompleted(false);
                        

                        return;
                    }
                }
            } // t loop
        } // f loop

        if (threadStopped) {
            buffer = null;
            bufferDest = null;
            finalize();

            return;
        }

        destImage.calcMinMax();
        
        setCompleted(true);
    }

    /**
     * Calculates the gray scale image. Must use getSrcImage() after running this routine
     */
    private void calcStoreInPlace() {

        int i, n;
        int id;
        int z, t, f;
        int p;
        int totalLength;
        float sum;
        int lengthIn, lengthOut; // total number of data-elements (pixels) in image
        float[] buffer; // data-buffer (for pixel data) which is the "heart" of the image\
        float[] bufferDest;
        float max;
        float remapFactor = 1.0f;
        boolean doFloat;
        int numVoxels = 0;
        float redSum = 0.0f;
        float greenSum = 0.0f;
        float blueSum = 0.0f;
        int bwType;
        int[] extents;
        int sliceSize;
        String imageName;
        FileInfoBase[] fInfoBase = null;

        float averageR = 0.0f;
        float averageG = 0.0f;
        float averageB = 0.0f;

        

        extents = srcImage.getExtents();
        sliceSize = srcImage.getSliceSize();
        imageName = srcImage.getImageName();

        if (srcImage.getNDims() == 5) {
            f = extents[4];
        } else {
            f = 1;
        }

        if (srcImage.getNDims() >= 4) {
            t = extents[3];
        } else {
            t = 1;
        }

        if (srcImage.getNDims() >= 3) {
            z = extents[2];
        } else {
            z = 1;
        }


        if (srcImage.getType() == ModelImage.ARGB_FLOAT) {
            doFloat = true;
        } else {
            doFloat = false;
        }

        try {
            lengthIn = 4 * f * t * z * sliceSize;
            buffer = new float[lengthIn];
            fireProgressStateChanged(srcImage.getImageName(), "RGB to GRAY ...");
        } catch (OutOfMemoryError e) {
            buffer = null;
            System.gc();
            displayError("Algorithm RGBtoGray reports: Out of memory when creating image buffer");
            setCompleted(false);

            return;
        }

        try {
            srcImage.exportData(0, lengthIn, buffer);
        } catch (IOException error) {
            buffer = null;
            displayError("Algorithm RGBtoGray : Input Image(s) locked");
            setCompleted(false);

            return;
        }

        if (srcImage.getType() == ModelStorageBase.ARGB) {
            bwType = ModelStorageBase.UBYTE;
        } else if (srcImage.getType() == ModelStorageBase.ARGB_USHORT) {
            bwType = ModelStorageBase.USHORT;
        } else {
            bwType = ModelStorageBase.FLOAT;
        }

        // get of max R, G, and B for remap
        max = (float) srcImage.getMaxR();

        if (srcImage.getMaxG() > max) {
            max = (float) srcImage.getMaxG();
        }

        if (srcImage.getMaxB() > max) {
            max = (float) srcImage.getMaxB();
        }

        fInfoBase = new FileInfoBase[f * t * z];

        for (n = 0; n < srcImage.getFileInfo().length; n++) {
            fInfoBase[n] = (FileInfoBase) (srcImage.getFileInfo(n).clone());
            fInfoBase[n].setDataType(bwType);
        }

        if (srcImage.getParentFrame() != null) {
            srcImage.getParentFrame().close();
        }

        srcImage.disposeLocal();
        srcImage = null;

        try {
            lengthOut = f * t * z * sliceSize;
            bufferDest = new float[lengthOut];
        } catch (OutOfMemoryError e) {
            buffer = null;
            bufferDest = null;
            System.gc();
            displayError("Algorithm RGBtoGray reports: Out of memory when creating image buffer");
            setCompleted(false);

            return;
        }


        int mod = lengthIn / 20;

        totalLength = f * t * z * lengthIn;

        if (intensityAverage) {


            // determine the remap factor to scale the max up to 255 (higher contrast)
            remapFactor = 255.0f / max;

            // MipavUtil.displayError("Max intensity is " + max + " and Remap Factor is " + remapFactor);

            // Run through the loop once to get Sums of Voxel Intensities for RGB


            for (i = 0; (i < lengthIn) && !threadStopped; i += 4) {

                numVoxels++;
                redSum += buffer[i + 1];
                greenSum += buffer[i + 2];
                blueSum += buffer[i + 3];
            } // for ( i = 0; i < lengthIn && !threadStopped; i += 4)


            // Calculate Voxel Intensity Averages
            averageR = redSum / numVoxels;
            averageG = greenSum / numVoxels;
            averageB = blueSum / numVoxels;

            // MipavUtil.displayError("AverageR = " + averageR + ", AverageG = " +
            // averageG + ", AverageB = " + averageB);

        } // if (intensityAverage)

        for (i = 0, id = 0; (i < lengthIn) && !threadStopped; i += 4, id++) {

            if (((i % mod) == 0)) {
                fireProgressStateChanged(Math.round((float) (i) / (totalLength - 1) * 100));
            }

            if (thresholdAverage) {
                p = 0;
                sum = 0.0f;

                if (buffer[i + 1] > threshold) {
                    p++;
                    sum = buffer[i + 1];
                }

                if (buffer[i + 2] > threshold) {
                    p++;
                    sum += buffer[i + 2];
                }

                if (buffer[i + 3] > threshold) {
                    p++;
                    sum += buffer[i + 3];
                }

                if (p == 0) {
                    bufferDest[id] = 0;
                } else if (p == 1) {
                    bufferDest[id] = sum;
                } else if (p == 2) {
                    bufferDest[id] = 0.5f * sum;
                } else { // p == 3
                    bufferDest[id] = thirdValue * sum;
                }

                if (doFloat) {
                    bufferDest[id] = Math.round(bufferDest[id]);
                }
            } // if (thresholdAverage)
            else if (intensityAverage) {
                p = 0;
                sum = 0.0f;

                if (averageR > 2.0f) {
                    p++;
                    sum = buffer[i + 1] * remapFactor;
                }

                if (averageG > 2.0f) {
                    p++;
                    sum += buffer[i + 2] * remapFactor;
                }

                if (averageB > 2.0f) {
                    p++;
                    sum += buffer[i + 3] * remapFactor;
                }

                if (p == 0) {
                    bufferDest[id] = 0;
                } else if (p == 1) {
                    bufferDest[id] = sum;
                } else if (p == 2) {
                    bufferDest[id] = 0.5f * sum;
                } else { // p == 3
                    bufferDest[id] = thirdValue * sum;
                }

                if (doFloat) {
                    bufferDest[id] = Math.round(bufferDest[id]);
                }
            } // if (averageIntensity)
            else { // no thresholdAverage

                if (doFloat) {
                    bufferDest[id] = (redValue * buffer[i + 1]) + (greenValue * buffer[i + 2]) +
                                     (blueValue * buffer[i + 3]);
                } else {
                    bufferDest[id] = Math.round((redValue * buffer[i + 1]) + (greenValue * buffer[i + 2]) +
                                                (blueValue * buffer[i + 3]));
                }
            } // else no threshold average
        }

        if (threadStopped) {
            buffer = null;
            bufferDest = null;
            finalize();

            return;
        }

        buffer = null;

        srcImage = new ModelImage(bwType, extents, imageName);

        for (n = 0; n < srcImage.getFileInfo().length; n++) {
            srcImage.setFileInfo(fInfoBase[n], n);
        }

        try {
            srcImage.importData(0, bufferDest, true);
        } catch (IOException error) {
            displayError("Algorithm RGBtoGray: Output Image(s) locked");
            setCompleted(false);
            

            return;
        }

        if (threadStopped) {
            buffer = null;
            bufferDest = null;
            finalize();

            return;
        }

        
        setCompleted(true);
    }
}
