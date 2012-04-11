package gov.nih.mipav.model.algorithms.utilities;


import gov.nih.mipav.model.algorithms.*;
import gov.nih.mipav.model.structures.*;

import java.io.*;


/**
 * Algorithm used to add Gaussian, Poisson, Uniform, Rayleigh, or Rician noise to an image. 
 * The additive noise is clamped to the lowest or highest value is the source image type. 
 * For example a byte image where the source pixel = 120 + noise = 15 would be clamped
 * to 127 the maximum pixel value for a byte image.
 * 
 * For Rayleigh noise the formula is simply sigma * sqrt(-2.0 * ln(U)), where U is a uniform 0 to 1 distribution 
 * not including 0.  The Rayleigh cumulative distribution function F(x) =  1 - exp(-(x**2)/(2*sigma**2)) is simply 
 * set equal to U and then the inverse function is obtained by solving for x in terms of U, 
 * giving x = sigma * sqrt(-2.0 * ln(1 - U)) and finally noting that 1 - U has the same distribution as U.
 * 
 * Rician noise is not additive, but is instead data dependent.  Let a be the original noiseless data value,
 * and x and y be gaussian random variables with zero mean and identical standard deviations sigma.  Then the
 * resulting value m = sqrt((a + x)**2 + y**2) is Rician distributed.  MR images have Rician noise.
 * 
 * Reference for Rician noise generation: "A Nonlocal Maximum Likelihood Estimation Method for Rician Noise
 * Reduction in MR Images" by Lili He and Ian R. Greenshields, IEEE Transactions on Medical Imaging, Vol. 28,
 * No. 2, February, 2009, pp. 165-172.
 *
 * @version  2.0 July 25, 2008
 * @author   Matthew J. McAuliffe, Ph.D.
 *           William Gandler
 * @see      RandomNumberGen
 */
public class AlgorithmNoise extends AlgorithmBase {

    //~ Static fields/initializers -------------------------------------------------------------------------------------

    /** Used to indicate Gaussian distribution of noise. */
    public static final int GAUSSIAN = 0;
    
    /** Used to indicate added Poisson noise */
    public static final int POISSON = 1;

    /** Used to indicate Uniform distribution of noise. */
    public static final int UNIFORM = 2;
    
    public static final int RAYLEIGH = 3;
    
    public static final int RICIAN = 4;

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** Noise level to be added to the image. Default in NaN. */
    private double level = Double.NaN;

    /** Storage or min and max values. */
    private double min, max;
    
    /** For Poisson out = gain * Poisson(mean) + offset */
    private double mean;
    
    private double gain;
    
    private double offset;
    
    private double sigma;

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
     * @param  mean  For poisson: out = gain * Poisson(mean) + offset
     * @param  gain
     * @param offset
     * @param sigma for Rayleigh
     */
    public AlgorithmNoise(ModelImage srcImg, int _noiseType, double _level, double mean, double gain, double offset,
    		double sigma) {

        super(null, srcImg);
        noiseType = _noiseType;
        level = _level;
        this.mean = mean;
        this.gain = gain;
        this.offset = offset;
        this.sigma = sigma;
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
     * @param  mean  For poisson: out = gain * Poisson(mean) + offset
     * @param  gain
     * @param  offset
     * @param  sigma for Rayleigh
     */
    public AlgorithmNoise(ModelImage destImg, ModelImage srcImg, int _noiseType, double _level, double mean, double gain,
                          double offset, double sigma) {

        super(destImg, srcImg);
        noiseType = _noiseType;
        level = _level;
        this.mean = mean;
        this.gain = gain;
        this.offset = offset;
        this.sigma = sigma;
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

        

        if (destImage != null) {

            if (srcImage.getNDims() == 2) {
                calcStoreInDest2D();
            } else if (srcImage.getNDims() > 2) {
                calcStoreInDest34D();
            }
        } else {

            if (srcImage.getNDims() == 2) {
                calcInPlace2D();
            } else if (srcImage.getNDims() > 2) {
                calcInPlace34D();
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
        double poissEvents[] = null;
        double sum;

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

        if (noiseType == POISSON) {
            poissEvents = randomGen.poissDecay(length, mean, gain, offset);    
        }

        int mod = length / 100;

        for (i = 0; (i < length) && !threadStopped; i++) {

            if (((i % mod) == 0)) {
                fireProgressStateChanged(Math.round((float) i / (length - 1) * 100));
            }

            if (noiseType == UNIFORM) {
                noise = randomGen.genUniformRandomNum(-level, level);
                pixel = buffer[i] + noise;
            } else if (noiseType == GAUSSIAN){
                noise = randomGen.genGaussianRandomNum(-level, level);
                pixel = buffer[i] + noise;
            }
            else if (noiseType == RAYLEIGH) {
            	noise = randomGen.genUniformRandomNum(Double.MIN_VALUE, 1.0);
            	pixel = buffer[i] + sigma * Math.sqrt(-2.0 * Math.log(noise));
            }
            else if (noiseType == RICIAN) {
                noise = randomGen.genGaussianRandomNum(-level, level);
                sum = noise + buffer[i];
                noise = randomGen.genGaussianRandomNum(-level, level);
                pixel = Math.sqrt(sum*sum + noise*noise);
            }
            else {
                pixel = buffer[i] + poissEvents[i];
            }


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
     * This function replaces the 3D or 4D source image with an image that has noise added to it.
     */
    private void calcInPlace34D() {
        int i;
        int length;
        double[] buffer;
        double noise;
        double pixel;
        double poissEvents[] = null;
        double sum;

        try {
            length = srcImage.getSliceSize() * srcImage.getExtents()[2];
            if (srcImage.getNDims() >= 4) {
                length *= srcImage.getExtents()[3];
            }
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

        if (noiseType == POISSON) {
            poissEvents = randomGen.poissDecay(length, mean, gain, offset);    
        }

        int mod = length / 100; // mod is 1 percent of length

        for (i = 0; (i < length) && !threadStopped; i++) {

            if (((i % mod) == 0)) {
                fireProgressStateChanged(Math.round((float) i / (length - 1) * 100));
            }

            if (noiseType == UNIFORM) {
                noise = randomGen.genUniformRandomNum(-level, level);
                pixel = buffer[i] + noise;
            } else if (noiseType == GAUSSIAN){
                noise = randomGen.genGaussianRandomNum(-level, level);
                pixel = buffer[i] + noise;
            }
            else if (noiseType == RAYLEIGH) {
            	noise = randomGen.genUniformRandomNum(Double.MIN_VALUE, 1.0);
            	pixel = buffer[i] + sigma * Math.sqrt(-2.0 * Math.log(noise));
            }
            else if (noiseType == RICIAN) {
                noise = randomGen.genGaussianRandomNum(-level, level);
                sum = noise + buffer[i];
                noise = randomGen.genGaussianRandomNum(-level, level);
                pixel = Math.sqrt(sum*sum + noise*noise);
            }
            else {
                pixel = buffer[i] + poissEvents[i];
            }

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
        double poissEvents[] = null;
        double sum;

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

        if (noiseType == POISSON) {
            poissEvents = randomGen.poissDecay(length, mean, gain, offset);    
        }

        int mod = length / 100; // mod is 1 percent of length

        for (i = 0; (i < length) && !threadStopped; i++) {

            if (((i % mod) == 0)) {
                fireProgressStateChanged(Math.round((float) i / (length - 1) * 100));
            }

            if (noiseType == UNIFORM) {
                noise = randomGen.genUniformRandomNum(-level, level);
                pixel = buffer[i] + noise;
            } else if (noiseType == GAUSSIAN){
                noise = randomGen.genGaussianRandomNum(-level, level);
                pixel = buffer[i] + noise;
            }
            else if (noiseType == RAYLEIGH) {
            	noise = randomGen.genUniformRandomNum(Double.MIN_VALUE, 1.0);
            	pixel = buffer[i] + sigma * Math.sqrt(-2.0 * Math.log(noise));
            }
            else if (noiseType == RICIAN) {
                noise = randomGen.genGaussianRandomNum(-level, level);
                sum = noise + buffer[i];
                noise = randomGen.genGaussianRandomNum(-level, level);
                pixel = Math.sqrt(sum*sum + noise*noise);
            }
            else {
                pixel = buffer[i] + poissEvents[i];
            }
            
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
     * This function produces a new 3D or 4D image that is the sum of the source image and noise.
     */
    private void calcStoreInDest34D() {

        int i;
        int length;
        double[] buffer;
        double noise;
        double pixel;
        double poissEvents[] = null;
        double sum;

        try {
            destImage.setLock(ModelStorageBase.RW_LOCKED);
        } catch (IOException error) {
            errorCleanUp("Change type: Image(s) locked", false);

            return;
        }

        try {
            length = srcImage.getSliceSize() * srcImage.getExtents()[2];
            if (srcImage.getNDims() >= 4) {
                length *= srcImage.getExtents()[3];
            }
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

        if (noiseType == POISSON) {
            poissEvents = randomGen.poissDecay(length, mean, gain, offset);    
        }

        int mod = length / 100; // mod is 1 percent of length

        for (i = 0; (i < length) && !threadStopped; i++) {

            if (((i % mod) == 0)) {
                fireProgressStateChanged(Math.round((float) i / (length - 1) * 100));
            }

            if (noiseType == UNIFORM) {
                noise = randomGen.genUniformRandomNum(-level, level);
                pixel = buffer[i] + noise;
            } else if (noiseType == GAUSSIAN){
                noise = randomGen.genGaussianRandomNum(-level, level);
                pixel = buffer[i] + noise;
            }
            else if (noiseType == RAYLEIGH) {
            	noise = randomGen.genUniformRandomNum(Double.MIN_VALUE, 1.0);
            	pixel = buffer[i] + sigma * Math.sqrt(-2.0 * Math.log(noise));
            }
            else if (noiseType == RICIAN) {
                noise = randomGen.genGaussianRandomNum(-level, level);
                sum = noise + buffer[i];
                noise = randomGen.genGaussianRandomNum(-level, level);
                pixel = Math.sqrt(sum*sum + noise*noise);
            }
            else {
                pixel = buffer[i] + poissEvents[i];
            }
            
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
