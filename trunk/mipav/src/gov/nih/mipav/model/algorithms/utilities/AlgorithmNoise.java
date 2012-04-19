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


-----Original Message-----
From: hakim.achterberg@gmail.com [mailto:hakim.achterberg@gmail.com] On Behalf Of Hakim Achterberg
Sent: Wednesday, April 18, 2012 9:30 AM
To: Gandler, William (NIH/CIT) [E]
Cc: McAuliffe, Matthew (NIH/CIT) [E]; Senseney, Justin (NIH/CIT) [E]
Subject: Re: Which Rician noise generator is the best?

Dear William,

All methods come down to the same equations, however there are some differences in the assumption of the signal. 
Adding Rician noise to the magnitude of a complex signal, is equivalent to adding Gaussian noise to both the real 
and imaginary part of the signal and then taking the magnitude. However, in our case the phase of this signal was
 unknown, so some assumption had to be made about the phase in the simulation.

The assumption I can deduct from the methods mentioned:
In equation (14) in "A Nonlocal Maximum Likelihood Estimation Method for Rician Noise Reduction in MR Images" the
 phase of the signal is assumed to be zero (or 0.5 pi, pi, 1.5 pi) so that the magnitude is completely in one of
  the two components.
In equation (4) in "Rician noise removal by non-Local Means filtering for low signal-to-noise ratio MRI:
 applications to DT-MRI" the phase of the signal is assumed to be 0.25 pi (or an equivalent rotation) so that
  the magnitude is split into two equal parts (a + bi, where a=b=M/sqrt(2)).
In equation (6) in "Development of computer-generated phantoms for FMRI software evaluation", I have the feeling
 they make an assumption which is incorrect and that is that adding the squared noises separately to the magnitude
  it is similar as to adding the noise to the real and imaginary parts of the signal in computation of the
   gradient magnitude. My feeling says this one is not correct, but I haven't extensively checked this.
In our work ( Algorithm 2.1 in "Optimal acquisition schemes in high angular resolution diffusion weighted
 imaging" ), we assume the phase of the signal to be random. So we create a random phase, change the signal from 
 a magnitude and phase into real and imaginary parts, add noise to these parts and calculate the gradient magnitude.

To be honest, from discussion with our MR physicist we thought our assumption to be (more or less) valid, but I do not
 recall that this assumption has been carefully checked. To really establish which algorithm is the best, 
 the assumptions made in each model should be checked against the data you are trying to simulate.

I hope that this helps and does not only add confusion.

Kind regards,
Hakim

 * Rician noise is not additive, but is instead data dependent.  Let a be the original noiseless data value,
 * and x and y be gaussian random variables with zero mean and identical standard deviations sigma.  Then with a 
 * uniformly distributed theta the resulting value m = sqrt((a*cos(theta) + x)**2 + (a*sin(theta) + y)**2) 
 * is Rician distributed.  MR images have Rician noise.
 * 
 * Reference for Rician noise generation: Optimal acquisition schemes in high angular resolution diffusion
 * weighted imaging" MSC Thesis by Hakim Achterberg.
 * Confirmation that the zero angle phase noise generator leads to a Rician noise distribution is found in
 *  "Analytically exact correct scheme for signal extraction from noisy magnitude MR signals" by Cheng Guan Koay
 *  and Peter J. Basser, Journal of Magnetic Resonance, 179, 2006, pp. 317-322, Equations 2 - 4.
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
        double sum2;
        double theta;

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
            	theta = randomGen.genUniformRandomNum(0.0, 2.0*Math.PI);
                noise = randomGen.genGaussianRandomNum(-level, level);
                sum = noise + buffer[i]*Math.cos(theta);
                noise = randomGen.genGaussianRandomNum(-level, level);
                sum2 = noise + buffer[i]*Math.sin(theta);
                pixel = Math.sqrt(sum*sum + sum2*sum2);
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
        double sum2;
        double theta;

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
            	theta = randomGen.genUniformRandomNum(0.0, 2.0*Math.PI);
                noise = randomGen.genGaussianRandomNum(-level, level);
                sum = noise + buffer[i]*Math.cos(theta);
                noise = randomGen.genGaussianRandomNum(-level, level);
                sum2 = noise + buffer[i]*Math.sin(theta);
                pixel = Math.sqrt(sum*sum + sum2*sum2);
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
        double sum2;
        double theta;

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
            	theta = randomGen.genUniformRandomNum(0.0, 2.0*Math.PI);
                noise = randomGen.genGaussianRandomNum(-level, level);
                sum = noise + buffer[i]*Math.cos(theta);
                noise = randomGen.genGaussianRandomNum(-level, level);
                sum2 = noise + buffer[i]*Math.sin(theta);
                pixel = Math.sqrt(sum*sum + sum2*sum2);
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
        double sum2;
        double theta;

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
            	theta = randomGen.genUniformRandomNum(0.0, 2.0*Math.PI);
                noise = randomGen.genGaussianRandomNum(-level, level);
                sum = noise + buffer[i]*Math.cos(theta);
                noise = randomGen.genGaussianRandomNum(-level, level);
                sum2 = noise + buffer[i]*Math.sin(theta);
                pixel = Math.sqrt(sum*sum + sum2*sum2);
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
