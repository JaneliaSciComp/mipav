package gov.nih.mipav.model.algorithms.utilities;


import gov.nih.mipav.model.algorithms.*;
import gov.nih.mipav.model.structures.*;

import gov.nih.mipav.view.*;

import java.io.*;


/**
 * Algorithm used to add Gaussian or Uniform noise to an image. The additive noise is clamped to the lowest or highest
 * value is the source image type. For example a byte image where the source pixel = 120 + noise = 15 would be clamped
 * to 127 the maximum pixel value for a byte image.
 *
 * @version  1.0 June 15, 1999
 * @author   Matthew J. McAuliffe, Ph.D.
 * @see      RandomNumberGen
 */
public class AlgorithmNoise extends AlgorithmBase {

    //~ Static fields/initializers -------------------------------------------------------------------------------------

    /** Used to indicate Gaussian distribution of noise. */
    public static final int GAUSSIAN = 0;

    /** Used to indicate Uniform distribution of noise. */
    public static final int UNIFORM = 1;

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** Used to store string representation of the noise type. */
    private String[] algorithmName = { "GAUSSIAN", "UNIFORM" };

    /** Noise level to be added to the image. Default in NaN. */
    private double level = Double.NaN;

    /** Storage or min and max values. */
    private double min, max;

    /** Noise type. Defaults to uniform. */
    private int noiseType = UNIFORM;

    /** Reference to the random number generator. */
    private RandomNumberGen randomGen;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Creates a new AlgorithmNoise object.
     *
     * @param  srcImg      source image model
     * @param  _noiseType  gaussian noise or uniform noise
     * @param  _level      level of noise
     */
    public AlgorithmNoise(ModelImage srcImg, int _noiseType, double _level) {

        super(null, srcImg);
        noiseType = _noiseType;
        level = _level;
        randomGen = new RandomNumberGen();
        setRange();
    }

    /**
     * Creates a new AlgorithmNoise object.
     *
     * @param  destImg     image model where result image is to stored
     * @param  srcImg      source image model
     * @param  _noiseType  gaussian noise or uniform noise
     * @param  _level      level of noise
     */
    public AlgorithmNoise(ModelImage destImg, ModelImage srcImg, int _noiseType, double _level) {

        super(destImg, srcImg);
        noiseType = _noiseType;
        level = _level;
        randomGen = new RandomNumberGen();
        setRange();
    }

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * Prepares this class for destruction.
     */
    public void finalize() {
        randomGen = null;
        super.finalize();
    }

    /**
     * Starts algorithm.
     */
    public void runAlgorithm() {

        if (srcImage == null) {
            displayError("Change image type: Source Image is null");

            return;
        }

        constructLog();

        if (destImage != null) {

            if (srcImage.getNDims() == 2) {
                calcStoreInDest2D();
            } else if (srcImage.getNDims() > 2) {
                calcStoreInDest3D();
            }
        } else {

            if (srcImage.getNDims() == 2) {
                calcInPlace2D();
            } else if (srcImage.getNDims() > 2) {
                calcInPlace3D();
            }
        }

        if (threadStopped) {
            finalize();

            return;
        }
    }

    /**
     * This function replaces the 2D source image with an image that has noise added to it.
     */
    private void calcInPlace2D() {

        int i;
        int length;
        double[] buffer;
        double noise;
        double pixel;

        try {
            length = srcImage.getSliceSize();
            buffer = new double[length];
            srcImage.exportData(0, length, buffer); // locks and releases lock
            fireProgressStateChanged(srcImage.getImageName(), "Adding noise ... ");
        } catch (IOException error) {
            buffer = null;
            errorCleanUp("Algorithm Noise: Image(s) locked", true);

            return;
        } catch (OutOfMemoryError e) {
            buffer = null;
            errorCleanUp("Algorithm Noise:  Out of memory", true);

            return;
        }

        

        int mod = length / 100;

        for (i = 0; (i < length) && !threadStopped; i++) {

            if (((i % mod) == 0) && isProgressBarVisible()) {
                fireProgressStateChanged(Math.round((float) i / (length - 1) * 100));
            }

            if (noiseType == UNIFORM) {
                noise = randomGen.genUniformRandomNum(-level, level);
            } else {
                noise = randomGen.genGaussianRandomNum(-level, level);
            }

            pixel = buffer[i] + noise;

            // clamp noise to image type
            if (pixel > max) {
                pixel = max;
            } else if (pixel < min) {
                pixel = min;
            }

            buffer[i] = (float) pixel;
        }

        if (threadStopped) {
            finalize();

            return;
        }

        try {
            srcImage.importData(0, buffer, true);
        } catch (IOException error) {
            errorCleanUp("Algorithm Noise: Image(s) locked", false);

            return;
        }

        
        setCompleted(true);
    }

    /**
     * This function replaces the 3D source image with an image that has noise added to it.
     */
    private void calcInPlace3D() {
        int i;
        int length;
        double[] buffer;
        double noise;
        double pixel;

        try {
            length = srcImage.getSliceSize() * srcImage.getExtents()[2];
            buffer = new double[length];
            srcImage.exportData(0, length, buffer); // locks and releases lock
            fireProgressStateChanged(srcImage.getImageName(), "Adding noise ... ");
        } catch (IOException error) {
            buffer = null;
            errorCleanUp("Algorithm Noise: Image(s) locked", true);

            return;
        } catch (OutOfMemoryError e) {
            buffer = null;
            errorCleanUp("Algorithm Noise: Out of memory", true);

            return;
        }

        

        int mod = length / 100; // mod is 1 percent of length

        for (i = 0; (i < length) && !threadStopped; i++) {

            if (((i % mod) == 0) && isProgressBarVisible()) {
                fireProgressStateChanged(Math.round((float) i / (length - 1) * 100));
            }

            if (noiseType == UNIFORM) {
                noise = randomGen.genUniformRandomNum(-level, level);
            } else {
                noise = randomGen.genGaussianRandomNum(-level, level);
            }

            pixel = buffer[i] + noise;

            // clamp noise to image type
            if (pixel > max) {
                pixel = max;
            } else if (pixel < min) {
                pixel = min;
            }

            buffer[i] = (float) pixel;
        }

        if (threadStopped) {
            finalize();

            return;
        }

        try {
            srcImage.importData(0, buffer, true);
        } catch (IOException error) {
            errorCleanUp("Algorithm Noise: Image(s) locked", false);

            return;
        }

        setCompleted(true);
        
    }

    /**
     * This function produces a new 2D image that is the sum of the source image and noise.
     */
    private void calcStoreInDest2D() {

        int i;
        int length;
        double[] buffer;
        double noise;
        double pixel;

        try {
            destImage.setLock(ModelStorageBase.RW_LOCKED);
        } catch (IOException error) {
            displayError("Noise: Image(s) locked");
            setCompleted(false);

            return;
        }

        try {
            length = srcImage.getSliceSize();
            buffer = new double[length];
            srcImage.exportData(0, length, buffer); // locks and releases lock
            fireProgressStateChanged(srcImage.getImageName(), "Adding noise ... ");
        } catch (IOException error) {
            buffer = null;
            errorCleanUp("Algorithm Noise: Image(s) locked", true);

            return;
        } catch (OutOfMemoryError e) {
            buffer = null;
            errorCleanUp("Algorithm Noise:  Out of memory", true);

            return;
        }

        

        int mod = length / 100; // mod is 1 percent of length

        for (i = 0; (i < length) && !threadStopped; i++) {

            if (((i % mod) == 0) && isProgressBarVisible()) {
                fireProgressStateChanged(Math.round((float) i / (length - 1) * 100));
            }

            if (noiseType == UNIFORM) {
                noise = randomGen.genUniformRandomNum(-level, level);
            } else {
                noise = randomGen.genGaussianRandomNum(-level, level);
            }

            pixel = buffer[i] + noise;

            // clamp noise to image type
            if (pixel > max) {
                pixel = max;
            } else if (pixel < min) {
                pixel = min;
            }

            destImage.set(i, pixel);
        }

        if (threadStopped) {
            finalize();

            return;
        }

        destImage.calcMinMax();
        destImage.releaseLock();
        
        setCompleted(true);
    }

    /**
     * This function produces a new 3D image that is the sum of the source image and noise.
     */
    private void calcStoreInDest3D() {

        int i;
        int length;
        double[] buffer;
        double noise;
        double pixel;

        try {
            destImage.setLock(ModelStorageBase.RW_LOCKED);
        } catch (IOException error) {
            errorCleanUp("Change type: Image(s) locked", false);

            return;
        }

        try {
            length = srcImage.getSliceSize() * srcImage.getExtents()[2];
            buffer = new double[length];
            srcImage.exportData(0, length, buffer); // locks and releases lock
            fireProgressStateChanged(srcImage.getImageName(), "Adding noise ... ");
        } catch (IOException error) {
            buffer = null;
            errorCleanUp("Algorithm Noise: Image(s) locked", true);

            return;
        } catch (OutOfMemoryError e) {
            buffer = null;
            errorCleanUp("Algorithm Noise: Out of memory", true);

            return;
        }

        

        int mod = length / 100; // mod is 1 percent of length

        for (i = 0; (i < length) && !threadStopped; i++) {

            if (((i % mod) == 0) && isProgressBarVisible()) {
                fireProgressStateChanged(Math.round((float) i / (length - 1) * 100));
            }

            if (noiseType == UNIFORM) {
                noise = randomGen.genUniformRandomNum(-level, level);
            } else {
                noise = randomGen.genGaussianRandomNum(-level, level);
            }

            pixel = buffer[i] + noise;

            // clamp noise to image type
            if (pixel > max) {
                pixel = max;
            } else if (pixel < min) {
                pixel = min;
            }

            destImage.set(i, pixel);
        }

        if (threadStopped) {
            finalize();

            return;
        }

        destImage.calcMinMax();
        destImage.releaseLock();
        
        setCompleted(true);
    }

    /**
     * Constructs a string of the contruction parameters and outputs the string to the messsage frame if the logging
     * procedure is turned on.
     */
    private void constructLog() {
        historyString = new String("Noise(" + algorithmName[noiseType] + ", " + String.valueOf(level) + ")\n");
    }

    /**
     * Sets the range of the destination image.
     */
    private void setRange() {

        if (srcImage.getType() == ModelStorageBase.BYTE) {
            min = -128;
            max = 127;
        } else if (srcImage.getType() == ModelStorageBase.UBYTE) {
            min = 0;
            max = 255;
        } else if (srcImage.getType() == ModelStorageBase.SHORT) {
            min = -32768;
            max = 32767;
        } else if (srcImage.getType() == ModelStorageBase.USHORT) {
            min = 0;
            max = 65535;
        } else if (srcImage.getType() == ModelStorageBase.INTEGER) {
            min = Integer.MIN_VALUE;
            max = Integer.MAX_VALUE;
        } else if (srcImage.getType() == ModelStorageBase.UINTEGER) {
            min = 0;
            max = 4294967295L;
        } else if (srcImage.getType() == ModelStorageBase.LONG) {
            min = Long.MIN_VALUE;
            max = Long.MAX_VALUE;
        } else if (srcImage.getType() == ModelStorageBase.FLOAT) {
            min = -Float.MAX_VALUE;
            max = Float.MAX_VALUE;
        } else if (srcImage.getType() == ModelStorageBase.DOUBLE) {
            min = -Double.MAX_VALUE;
            max = Double.MAX_VALUE;
        }
    }
}
