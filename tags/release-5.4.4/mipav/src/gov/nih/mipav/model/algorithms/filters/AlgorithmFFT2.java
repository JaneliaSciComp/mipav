package gov.nih.mipav.model.algorithms.filters;


import static java.lang.System.nanoTime;
import gov.nih.mipav.util.*;

import gov.nih.mipav.model.algorithms.AlgorithmBase;
import gov.nih.mipav.model.algorithms.filters.OpenCL.filters.OpenCLAlgorithmFFT;
//import gov.nih.mipav.model.algorithms.OpenCL.filters.OpenCLAlgorithmFFT;
import gov.nih.mipav.model.structures.*;

import java.io.IOException;
import java.util.Arrays;
import java.util.concurrent.CountDownLatch;


/**
 * Processing images by filtering in the frequency domain is a 3 step process: 1.) Performing a forward fast fourier
 * transform to convert a spatial image into its frequency image. 2.) Enhancing some frequency components of the image
 * and attenuating other frequency components of the image by mulitplying by a lowpass, highpass, bandpass, or bandstop
 * filter. Frequency filters may be constructed with 1 of 3 methods - finite impulse response filters constructed with
 * Hamming windows, Gaussian filters, and Butterworth filters. However, for the Gaussian filters only lowpass and
 * highpass filters are available. 3.) Performing an inverse fast fourier transform to convert from the frequency domain
 * back into the spatial domain. This software module performs all 3 steps of this process, but only performs one step
 * after a dialog OK. The AlgorithmFrequencyFilter module performs all 3 steps after a dialog OK.
 * 
 * <p>
 * The module also creates a Gabor filter, which is essentially a tilted Gaussian with 2 unequal axes at an offset
 * (freqU, freqV) from the origin. A Gabor filter only responds to a texture having both a particular frequency and a
 * particular orientation. Note that a filter and its mirror image reflected across the u and v frequency axes produce
 * identical frequency responses.
 * </p>
 * 
 * <p>
 * The core algorithm of this module, the fast fourier transform algorithm found in exec(), requires that all the
 * dimensions of an N-dimensional dataset be powers of 2. To be able to use this algorithm on datasets with arbitrary
 * dimensions, the data is zero padded to powers of 2 before applying the forward fast fourier transform and stripped
 * down to the original dimensions after applying the inverse fast fourier transform. If unequalDim is equal to true,
 * the fourier pictures are allowed to have unequal dimensions so as to save memory. If unequalDim is equal to false,
 * the pictures have equal dimensions so as to maximize symmetry.
 * </p>
 * 
 * <p>
 * The typical full sequence is as follows: 1.) Data from a real spatial image is exported into a float array realData.
 * 2.) An equally sized float array called imagData is created and filled with zeros. 3.) realData and imagData are
 * enlarged to the same length in every dimension. If finite impulse repsonse filters are constructed with Hamming
 * windows and no cropping, the new dimension size is equal to the minimum power of two number that equals or exceeds
 * the maximum original dimension size + kDim - 1, where kDim is the diameter of a circular or spherical convolution
 * kernel. If finite impulse response filters with Hamming windows and cropping or infinite impulse response Gaussian or
 * Butterworth filters are used, the new dimension size is equal to the minimum power of two number that equals or
 * exceeds the maximum original dimension size. The data is padded with zeros at the end of each dimension. 4.) exec()
 * is invoked to run the fast fourier transform algorithm. 5.) The center() algorithm is invoked to reorder the data for
 * display. The Fourier transform of real data is conjugate symmetric; the real parts are even and the imaginary parts
 * are odd. In other words, the magnitude is even and the phase is odd. Since images display the magnitude or log10(1 +
 * magnitude) a symmetry reflected across the center of the image is observed. 6.) The complex data is imported to the
 * display image with the image minimum and maximum being calculated. If logMagDisplay is set equal to false, the
 * minimum and maximum of sqrt(realData(i)*realData(i) + imagData(i)*imagData(i)) will be used as the image minimum and
 * maximum in other modules. If logMagDisplay is set equal to true, the log10(1 + sqrt(realData(i)*realData(i) +
 * imagData(i)*imagData(i))) will be used as the image minimum and maximum in other modules. Note that while this module
 * stores the complex data in the source or destination image, the display oriented modules in the MIPAV package will
 * use the magnitude or log magnitude as indicated above in the screen displays since complex numbers cannot be directly
 * displayed. 7.) The command setOriginalExtents is invoked so that the original dimensions of the source image are
 * available along with the display image. The original dimensions are needed so that the original source dimensions can
 * be restored after the inverse fast fourier transform. If finite impulse response filters with windows are used: 8a.)
 * An ideal filter kernel is constructed in the spatial domain. 9a.) A Hamming window kernel is constructed in the
 * spatial domain. 10a.) The ideal kernel and the Hamming window kernel are multiplied together. 11a.) The kernel is
 * zero padded up to the same dimensions that the image data was. 12a.) exec() is run to obtain the FFT of the kernel.
 * 13a.) The center() algorithm is run to reorder the data. If Gaussian or Butterworth filters are used: For Gaussian
 * and Butterworth filters the transfer functions affect the real and imaginary parts of the FFT of the image in exactly
 * the same manner. These filters are zero-phase- shift filters because they do not alter the phase of the transform.
 * Since these filters are conjugate symmetric, the inverse FFTs of these filters are purely real. Since this filtering
 * is equivalent to convolving to real 2D data sets, the result must be purely real. 8b.) The real part of the FFT is
 * set equal to the appropriate filter magnitude and the imaginary part is set equal to zero. This filter has the same
 * dimensions as the padded image data. 14) The data FFT is set equal to the product of the data FFT and the filter FFT.
 * 15.) The inverse FFT process is invoked. The complex data is exported into the 2 float arrays realData and imagData.
 * There should be no need for zero padding at this point since the dimensions should already be all powers of 2 from
 * before. 16.) The center() algorithm is invoked to restore the data to its original ordering. 17.) exec() is invoked
 * to run the inverse fast fourier transform algorithm. 18.) The realData now holds the correct response if Gaussian or
 * Butterworth filtering was used. If FIR filtering with windows was used then realData holds a version of the correct
 * response shifted by (kDim - 1)/2 toward the end of each dimension. The imagData should contain only roundoff error.
 * 19.) For FIR filters shift the data back by (kDim - 1)/2 toward the start of each dimension. 20.) Stripping is
 * performed to return the image to its original dimensions. 21.) The realData() is imported into the the new spatial
 * image.
 * </p>
 * 
 * <p>
 * Methods included calculate the magnitude and phase as shown below:
 * </p>
 * 
 * <p>
 * magnitude = ( (realData)^2 + (imagData)^2 )^(1/2); phase = arctan(imagData/realData);
 * </p>
 * 
 * @author William Gandler and Matthew J. McAuliffe
 */

public class AlgorithmFFT2 extends AlgorithmBase {
    // ~ Static fields/initializers
    // -------------------------------------------------------------------------------------

    /** Inverse FFT */
    public static final int INVERSE = -1;

    /** Forward FFT */
    public static final int FORWARD = 1;

    /**
     * Constants for xy plane, yz plane and zx plane.
     */
    public static final int SLICE_XY = 0;

    public static final int SLICE_YZ = 1;

    public static final int SLICE_ZX = 2;

    // ~ Instance fields
    // ------------------------------------------------------------------------------------------------

    /** Size of buffers (realData and imagData). */
    private int arrayLength;

    /** Dimension sizes. */
    private int[] dimLengths;

    /** Imaginary data. */
    private float[] imagData;

    /** If true process each slice one at a time in 3D image. */
    private boolean image25D;

    /** If true display log10 of 1 + magnitude. */
    private final boolean logMagDisplay;

    /** DOCUMENT ME! */
    private float minimum, maximum;

    /** Number of dimensions. */
    private int ndim; //

    /** Size of zero padded buffers. */
    private int newArrayLength;

    /** Zero padded dimension sizes. */
    private int[] newDimLengths;

    /** Original dimension sizes. */
    private int[] originalDimLengths;

    /** Real data. */
    private float[] realData;

    /** Transform direction. */
    private final int transformDir;

    /** If true allow unequal FFT dimensions. */
    private boolean unequalDim;
    
    private boolean complexInverse;

    /** True if zero padding actually performed. */
    private boolean zeroPad;

    private float[] finalData;
    
    private boolean useOCL = false;

    // ~ Constructors
    // ---------------------------------------------------------------------------------------------------

    public AlgorithmFFT2(final ModelImage srcImg, final int transformDir, final boolean logMagDisplay,
            final boolean unequalDim, final boolean image25D, final boolean complexInverse) {
        this(null, srcImg, transformDir, logMagDisplay, unequalDim, image25D, complexInverse);
    }

    public AlgorithmFFT2(final ModelImage destImg, final ModelImage srcImg, final int transformDir,
            final boolean logMagDisplay, final boolean unequalDim, final boolean image25D,
            final boolean complexInverse) {

        super(destImg, srcImg);
        this.transformDir = transformDir;
        this.logMagDisplay = logMagDisplay;

        if (transformDir == AlgorithmFFT2.FORWARD) {
            this.unequalDim = unequalDim;
            if (destImage == null) {
                srcImage.setUnequalDim(unequalDim);
                srcImage.setImage25D(image25D);
            } else {
                destImage.setUnequalDim(unequalDim);
                destImage.setImage25D(image25D);
            }
            this.image25D = image25D;
        } else if (transformDir == AlgorithmFFT2.INVERSE) {
            this.unequalDim = srcImage.getUnequalDim();
            this.image25D = srcImage.getImage25D();
            this.complexInverse = complexInverse;
        }
    }

    // ~ Methods
    // --------------------------------------------------------------------------------------------------------

    /**
     * Prepare this class for destruction.
     */
    public void finalize() {
        destImage = null;
        srcImage = null;
        super.finalize();
    }

    /**
     * Returns reference to imaginary data array.
     * 
     * @return the reference the the imaginary datat array
     */
    public float[] getImaginaryData() {
        return imagData;
    }

    /**
     * Returns reference to real data array.
     * 
     * @return the reference the the real datat array
     */
    public float[] getRealData() {
        return realData;
    }

    /**
     * Starts the program.
     */
    public void runAlgorithm() {

        if (srcImage == null) {
            displayError("Source Image is null");
            return;
        }

        final long startTime = System.currentTimeMillis();
        if ( useOCL )
        {
        	OpenCLAlgorithmFFT oclFFT = 
        		new OpenCLAlgorithmFFT( destImage, srcImage, transformDir, logMagDisplay, unequalDim, image25D, complexInverse );
        	oclFFT.run();
        	setCompleted(false);
        }
        else
        {
        	beforeExecute();
        	execute();
        	afterExecute();
        }
        System.out.println("Time Consumed : " + (System.currentTimeMillis() - startTime));
    }

    /**
     * The multi-threading version of FFT algorithm.
     * 
     * @param rData
     * @param iData
     * @param z
     */
    private void performMT() {
        int direction;
        int dimNumber;
        int newLength;
        int i, j, k, m;
        int originalSliceSize;
        int newSliceSize = 1;
        int originalVolumeSize;
        int newVolumeSize;
        if (transformDir == AlgorithmFFT2.FORWARD) {
            direction = 1;
        } else {
            direction = -1;
        }

        if (image25D) {
            dimNumber = 2;
        } else {
            dimNumber = ndim;
        }
        newLength = newArrayLength;
        final int xdim = newDimLengths[0];
        final int ydim = newDimLengths[1];
        final int zdim = (newDimLengths.length == 3) ? newDimLengths[2] : 1;
        if (transformDir == AlgorithmFFT2.FORWARD) {
            fireProgressStateChanged( -1, null, "Centering data before FFT algorithm ...");
            center(realData, imagData);
        }

        fireProgressStateChanged( -1, null, "Running FFT algorithm ...");

        //System.out.println("Number of Threads: " + nthreads);
        if (threadStopped) {
            return;
        }
        /**
         * Initialize the progress step.
         */
        if (image25D) {
            setProgressStep((float) (100.0 / (nthreads * Math.log(xdim * ydim) / Math.log(2.0))));
        } else {
            setProgressStep((float) (100.0 / (nthreads * Math.log(xdim * ydim * zdim) / Math.log(2.0))));
        }

        final CountDownLatch doneSignalX = new CountDownLatch(nthreads);
        AlgorithmFFT2.swapSlices(realData, imagData, xdim, ydim, zdim, AlgorithmFFT2.SLICE_YZ);
        for (i = 0; i < nthreads; i++) {
        	final int nslices;
        	if (zdim < nthreads) {
        	   if (i < zdim) {
        		   nslices = 1;
        	   }
        	   else {
        		   nslices = 0;
        	   }
        	}
        	else {
                nslices = zdim / nthreads;
        	}   
            final int sliceLen = xdim * ydim;
            final int start = i * nslices * sliceLen;
            final int end = start + 1;
            final int length = (i + 1) * nslices * sliceLen;
            final int dir = direction;
            final Runnable task = new Runnable() {
                public void run() {
                    doFFT(realData, imagData, start, end, 1, xdim, length, dir);
                    doneSignalX.countDown();
                }
            };
            ThreadUtil.mipavThreadPool.execute(task);
        }
        try {
            doneSignalX.await();
        } catch (final InterruptedException e) {
            e.printStackTrace();
        }
        if (threadStopped) {
            return;
        }

        final CountDownLatch doneSignalY = new CountDownLatch(nthreads);
        AlgorithmFFT2.swapSlices(realData, imagData, xdim, ydim, zdim, AlgorithmFFT2.SLICE_ZX);
        for (i = 0; i < nthreads; i++) {
            final int nslices = ydim / nthreads;
            final int start = i * nslices;
            final int end = (i + 1) * nslices;
            final int dir = direction;
            final Runnable task = new Runnable() {
                public void run() {
                    doFFT(realData, imagData, start, end, xdim, xdim * ydim, xdim * ydim * zdim, dir);
                    doneSignalY.countDown();
                }
            };
            ThreadUtil.mipavThreadPool.execute(task);
        }
        try {
            doneSignalY.await();
        } catch (final InterruptedException e) {
            e.printStackTrace();
        }
        if (threadStopped) {
            return;
        }

        if (dimNumber == 3) {
            final CountDownLatch doneSignalZ = new CountDownLatch(nthreads);
            AlgorithmFFT2.swapSlices(realData, imagData, xdim, ydim, zdim, AlgorithmFFT2.SLICE_XY);
            for (i = 0; i < nthreads; i++) {
                final int nslices = ydim / nthreads;
                final int start = i * nslices * xdim;
                final int end = (i + 1) * nslices * xdim;
                final int dir = direction;
                final Runnable task = new Runnable() {
                    public void run() {
                        doFFT(realData, imagData, start, end, xdim * ydim, xdim * ydim * zdim, xdim * ydim * zdim, dir);
                        doneSignalZ.countDown();
                    }
                };
                ThreadUtil.mipavThreadPool.execute(task);
            }
            try {
                doneSignalZ.await();
            } catch (final InterruptedException e) {
                e.printStackTrace();
            }
        }
        if (threadStopped) {
            return;
        }

        if (transformDir == AlgorithmFFT2.INVERSE) {
            fireProgressStateChanged( -1, null, "Centering data after inverse FFT ...");

            center(realData, imagData);
        }

        
        if (transformDir == AlgorithmFFT2.INVERSE) {
        	if (ndim >= 2) {
        		newSliceSize = newDimLengths[0]*newDimLengths[1];
        	}
        	if (complexInverse) {
        		for (i = 0; i < newLength; i++) {
        			if (image25D) {
        				realData[i] = realData[i] / newSliceSize;
        				imagData[i] = imagData[i] / newSliceSize;
        			}
        			else {
	                    realData[i] = realData[i] / newLength;
	                    imagData[i] = imagData[i] / newLength;
        			}
	            }
	
	            originalDimLengths = srcImage.getOriginalExtents();
	            finalData = new float[2 * AlgorithmBase.calculateImageSize(originalDimLengths)];
	            if ( !hasSameDimension(newDimLengths, originalDimLengths)) {
	                if (ndim == 1) {
	                    for (i = 0; i < originalDimLengths[0]; i++) {
	                    	finalData[2*i] = realData[i];
	                    	finalData[2*i+1] = imagData[i];
	                    }
	                } else if (ndim == 2) {
	                    for (i = 0; i < originalDimLengths[1]; i++) {
	                    	for (j = 0; j < originalDimLengths[0]; j++) {
	                    		finalData[2*(i*originalDimLengths[0] + j)] = realData[i*newDimLengths[0] + j];
	                    		finalData[2*(i*originalDimLengths[0] + j)+1] = imagData[i*newDimLengths[0] + j];
	                    	}
	                    }
	                } else if (ndim == 3) {
	                    originalSliceSize = originalDimLengths[0]*originalDimLengths[1];
	                    for (i = 0; i < originalDimLengths[2]; i++) {
	                        for (j = 0; j < originalDimLengths[1]; j++) {
	                            for (k = 0; k < originalDimLengths[0]; k++) {
	                            	finalData[2*(i*originalSliceSize + j*originalDimLengths[0] + k)] =
	                                realData[i*newSliceSize + j*newDimLengths[0] + k];
	                            	finalData[2*(i*originalSliceSize + j*originalDimLengths[0] + k)+1] =
		                            imagData[i*newSliceSize + j*newDimLengths[0] + k];
	                            }
	                        }
	                    }
	                } else if (ndim == 4) {
	                    originalSliceSize = originalDimLengths[0]*originalDimLengths[1];
	                    originalVolumeSize = originalSliceSize * originalDimLengths[2];
	                    newVolumeSize = newSliceSize * newDimLengths[2];
	                    for (i = 0; i < originalDimLengths[3]; i++) {
	                    	for (j = 0; j < originalDimLengths[2]; j++) {
	                    		for (k = 0; k < originalDimLengths[1]; k++) {
	                    			for (m = 0; m < originalDimLengths[0]; m++) {
	                    				finalData[2*(i*originalVolumeSize + j*originalSliceSize + 
	                    						  k*originalDimLengths[0] + m)] =
	                                    realData[i*newVolumeSize + j*newSliceSize + k*newDimLengths[0] + m];
	                    				finalData[2*(i*originalVolumeSize + j*originalSliceSize + 
	                    						  k*originalDimLengths[0] + m)+1] =
	                                    imagData[i*newVolumeSize + j*newSliceSize + k*newDimLengths[0] + m];
	                    			}
	                    		}
	                    	}
	                    }
	                }
	            } else {
	                System.arraycopy(realData, 0, finalData, 0, newLength);
	                for (i = 0; i < newLength; i++) {
	                	finalData[2*i] = realData[i];
	                	finalData[2*i+1] = imagData[i];
	                }
		        }	
        	} // if (complexInverse)
        	else { // !complexInverse
	            imagData = null;
	            for (i = 0; i < newLength; i++) {
	            	if (image25D) {
	            		realData[i] = realData[i] / newSliceSize;
	            	}
	            	else {
	                    realData[i] = realData[i] / newLength;
	                 // imagData[i] = imagData[i] / newLength;
	            	}
	            }
	
	            originalDimLengths = srcImage.getOriginalExtents();
	            finalData = new float[AlgorithmBase.calculateImageSize(originalDimLengths)];
	            if ( !hasSameDimension(newDimLengths, originalDimLengths)) {
	                if (ndim == 1) {
	                    System.arraycopy(realData, 0, finalData, 0, originalDimLengths[0]);
	                } else if (ndim == 2) {
	                    ArrayUtil.copy2D(realData, 0, newDimLengths[0], newDimLengths[1], finalData, 0,
	                            originalDimLengths[0], originalDimLengths[1], false);
	                } else if (ndim == 3) {
	                    ArrayUtil.copy3D(realData, 0, newDimLengths[0], newDimLengths[1], newDimLengths[2], finalData, 0,
	                            originalDimLengths[0], originalDimLengths[1], originalDimLengths[2], false);
	                } else if (ndim == 4) {
	                    ArrayUtil.copy4D(realData, newDimLengths[0], newDimLengths[1], newDimLengths[2], dimLengths[3],
	                            finalData, originalDimLengths[0], originalDimLengths[1], originalDimLengths[2],
	                            originalDimLengths[3], false);
	                }
	            } else {
	                System.arraycopy(realData, 0, finalData, 0, newLength);
		        }
        	} // else !complexInverse
        } // if (transformDir == AlgorithmFFT2.INVERSE)


    } // end of exec()

    private void center(final float[] rdata, final float[] idata) {
        final int xdim = newDimLengths[0];
        final int ydim = newDimLengths[1];
        if (ndim == 2) {
            for (int y = 0; y < ydim; y++) {
                for (int x = 0; x < xdim; x++) {
                    rdata[y * xdim + x] *= Math.pow( -1, x + y);
                    idata[y * xdim + x] *= Math.pow( -1, x + y);
                }
            }
            return;
        }
        final int zdim = newDimLengths[2];
        if (image25D) {
            for (int z = 0; z < zdim; z++) {
                for (int y = 0; y < ydim; y++) {
                    for (int x = 0; x < xdim; x++) {
                        rdata[z * xdim * ydim + y * xdim + x] *= Math.pow( -1, x + y);
                        idata[z * xdim * ydim + y * xdim + x] *= Math.pow( -1, x + y);
                    }
                }
            }
        } else {
            for (int z = 0; z < zdim; z++) {
                for (int y = 0; y < ydim; y++) {
                    for (int x = 0; x < xdim; x++) {
                        rdata[z * xdim * ydim + y * xdim + x] *= Math.pow( -1, x + y + z);
                        idata[z * xdim * ydim + y * xdim + x] *= Math.pow( -1, x + y + z);
                    }
                }
            }
        }
    }

    /**
     * Perform one dimension fast fourier transformation from start slice to end slice on given data, which includes x,
     * y or orientation.
     * 
     * For x direction: start = index of the start slice * xdim * ydim end = index of the end slice * xdim * ydim
     * startDist = 1 endDist = xdim length = end
     * 
     * For y direction: start = index of the start slice end = index of the end slice startDist = xdim endDist = xdim *
     * ydim length = xdim * ydim * zdim
     * 
     * For z direction: start = index of the start slice * xdim end = index of the end slice * xdim startDist = xdim *
     * ydim endDist = xdim * ydim * zdim length = xdim * ydim * zdim
     * 
     * 
     * @param rdata the real part of the data
     * @param idata the imaginary part of the data
     * @param start the location of the first pixel of the start slice
     * @param end the location of the first pixel of the end slice
     * @param startDist the start distance between two adjacent pixels from the FFT algorithm
     * @param endDist the end distance between two adjacent pixels from the FFT algorithm
     * @param length the length for each slice.
     */
    private void doFFT(final float[] rdata, final float[] idata, final int start, final int end, final int startDist,
            final int endDist, final int length, final int direction) {
        if (threadStopped) {
            return;
        }
        final float progressStep = getProgressStep();
        for (int l = startDist; l < endDist; l <<= 1) {
            final double delta = 2.0 * Math.PI / (l << 1) * direction * startDist;
            double angle = 0;
            for (int i = 0; i < l; i += startDist) {
                final double wtImag = Math.sin(angle);
                final double wtReal = Math.cos(angle);
                angle += delta;
                for (int j = start; j < end; j++) {
                    final int step = l << 1;
                    for (int p = j + i; p < length; p += step) {
                        if (threadStopped) {
                            return;
                        }
                        final int k = p + l;
                        final float tempReal = rdata[k];
                        final float tempImag = idata[k];
                        final float imag = (float) ( (tempImag * wtReal) - (tempReal * wtImag));
                        final float real = (float) ( (tempReal * wtReal) + (tempImag * wtImag));
                        rdata[k] = rdata[p] - real;
                        idata[k] = idata[p] - imag;
                        rdata[p] = rdata[p] + real;
                        idata[p] = idata[p] + imag;
                    }
                }
            }
            makeProgress(progressStep);
            fireProgressStateChanged((int) getProgress(), null, null);
        }
    }

    /**
     * Make the dimension be the power of two, and zero pad them.
     */
    public void beforeExecute() {
        float[] tempData;

        ndim = srcImage.getNDims();
        dimLengths = srcImage.getExtents();
        newDimLengths = new int[dimLengths.length];

        zeroPad = false;

        arrayLength = 1;

        for (int i = 0; i < ndim; i++) {
            arrayLength *= dimLengths[i];
            newDimLengths[i] = dimLengths[i];
        }

        for (int i = 0; i < ndim; i++) {
            if (i >= 2 && image25D) {
                break;
            }
            newDimLengths[i] = MipavMath.findMinimumPowerOfTwo(newDimLengths[i]);
        }

        /**
         * different dimensions are allowed
         */
        if ( !unequalDim) {
            int newLength = 0;
            for (int i = 0; i < ndim; i++) {
                if (i >= 2 && image25D) {
                    break;
                }
                if (newDimLengths[i] > newLength) {
                    newLength = newDimLengths[i];
                }
            }
            for (int i = 0; i < ndim; i++) {
                if (i >= 2 && image25D) {
                    break;
                }
                if (newDimLengths[i] < newLength) {
                    newDimLengths[i] = newLength;
                }
            }
        }
        newArrayLength = 1;

        for (int i = 0; i < ndim; i++) {
            newArrayLength *= newDimLengths[i];
        }

        if (newArrayLength > arrayLength) {
            zeroPad = true;
        }

        try {
            realData = new float[arrayLength];
            imagData = new float[arrayLength];
        } catch (final OutOfMemoryError e) {
            realData = null;
            displayError("AlgorithmFFT2: Out of memory creating realData");

            setCompleted(false);

            return;
        }

        try {

            if (!srcImage.isComplexImage()) {
                srcImage.exportData(0, arrayLength, realData); // locks and releases and lock

                // If the data is all real, then create an equal size imagData array and
                // fill it with zeros.
                Arrays.fill(imagData, 0.0f);
            } else {
                srcImage.exportComplexData(0, arrayLength, realData, imagData);
            }
        } catch (final IOException error) {
            displayError("AlgorithmFFT2: Source image is locked");

            setCompleted(false);

            return;
        } catch (final OutOfMemoryError e) {
            realData = null;
            imagData = null;
            displayError("AlgorithmFFT2: Out of memory");

            setCompleted(false);

            return;
        }

        if (transformDir == AlgorithmFFT2.FORWARD) {
            maximum = MipavMath.max(realData);
            minimum = MipavMath.min(realData);
        }

        if (zeroPad) {

            // zero pad the data so that all dimensions are powers of 2
            fireProgressStateChanged( -1, null, "Zero padding source data ...");

            try {
                tempData = new float[newArrayLength];
            } catch (final OutOfMemoryError e) {
                tempData = null;
                System.gc();
                displayError("AlgorithmFFT2: Out of memory creating tempData for zero padding");

                setCompleted(false);

                return;
            }

            if (ndim == 1) {
                System.arraycopy(realData, 0, tempData, 0, dimLengths[0]);
            } else if (ndim == 2) {
                ArrayUtil.copy2D(realData, 0, dimLengths[0], dimLengths[1], tempData, 0, newDimLengths[0],
                        newDimLengths[1], true);
            } else if (ndim == 3) {
                ArrayUtil.copy3D(realData, 0, dimLengths[0], dimLengths[1], dimLengths[2], tempData, 0,
                        newDimLengths[0], newDimLengths[1], newDimLengths[2], true);
            } else if (ndim == 4) {
                ArrayUtil.copy4D(realData, dimLengths[0], dimLengths[1], dimLengths[2], dimLengths[3], tempData,
                        newDimLengths[0], newDimLengths[1], newDimLengths[2], newDimLengths[3], true);
            }
            realData = tempData;

            try {
                tempData = new float[newArrayLength];
            } catch (final OutOfMemoryError e) {
                tempData = null;
                displayError("AlgorithmFFT2: Out of memory creating imagData in zero padding routine");

                setCompleted(false);

                return;
            }
            if (ndim == 1) {
                System.arraycopy(imagData, 0, tempData, 0, dimLengths[0]);
            } else if (ndim == 2) {
                ArrayUtil.copy2D(imagData, 0, dimLengths[0], dimLengths[1], tempData, 0, newDimLengths[0],
                        newDimLengths[1], true);
            } else if (ndim == 3) {
                ArrayUtil.copy3D(imagData, 0, dimLengths[0], dimLengths[1], dimLengths[2], tempData, 0,
                        newDimLengths[0], newDimLengths[1], newDimLengths[2], true);
            } else if (ndim == 4) {
                ArrayUtil.copy4D(imagData, dimLengths[0], dimLengths[1], dimLengths[2], dimLengths[3], tempData,
                        newDimLengths[0], newDimLengths[1], newDimLengths[2], newDimLengths[3], true);
            }
            imagData = tempData;
        }
    }

    public void execute() {
        if (multiThreadingEnabled && ndim > 2) {
            performMT();
        } else {
            perform();
        }
    }

    /**
     * This is the method that calculates the FFT Perform a data centering operation after the forward FFT Perform a
     * data centering operation before the inverse FFT Note that a frequency filter operation performs a forward FFT.
     * 
     * @param rData real data buffer
     * @param iData imaginary data buffer
     */
    private void perform() {

        final double TWO_PI = 2 * java.lang.Math.PI;
        double wt1Imag, wt1Real;
        double angle, delta;
        float imag, real, fTemp, fReal, fImag;
        int i, j, k, m, index1, index2, index3;
        int j1, j2, j3;
        int k1, k1Double;
        int iSwap, i1Swap, i2Swap, index, dim;
        int direction;
        int dimNumber;
        int newLength;
        int originalSliceSize;
        int newSliceSize = 1;
        int originalVolumeSize;
        int newVolumeSize;

        if (transformDir == AlgorithmFFT2.FORWARD) {
            direction = 1;
        } else {
            direction = -1;
        }

        if (image25D) {
            dimNumber = 2;
        } else {
            dimNumber = ndim;
        }
        newLength = newArrayLength;

        if (transformDir == AlgorithmFFT2.FORWARD) {

            if ( !image25D) {
                fireProgressStateChanged( -1, null, "Centering data after FFT algorithm ...");
            }
            center(realData, imagData);
        }

        if ( !image25D) {
            fireProgressStateChanged( -1, null, "Running FFT algorithm ...");
        }

        j1 = 1;
        dim = 1;
        for (i = 0; (i < dimNumber) && !threadStopped; i++) {
            j1 *= dim;
            dim = newDimLengths[i];
            j2 = j1 * dim;
            j3 = j2 * (newLength / (dim * j1));

            i1Swap = 0;

            for (index1 = 0; (index1 < j2) && !threadStopped; index1 += j1) {

                for (index2 = index1; (index2 < (index1 + j1)) && (index1 < i1Swap); index2++) {

                    for (index3 = index2; index3 < j3; index3 += j2) {
                        i2Swap = -index1 + index3 + i1Swap;

                        fTemp = imagData[index3];
                        imagData[index3] = imagData[i2Swap];
                        imagData[i2Swap] = fTemp;

                        fTemp = realData[index3];
                        realData[index3] = realData[i2Swap];
                        realData[i2Swap] = fTemp;
                    }
                }

                for (iSwap = j2 / 2; (iSwap >= j1) && (iSwap < (i1Swap + 1)); iSwap >>= 1) {
                    i1Swap = i1Swap - iSwap;
                }

                i1Swap = i1Swap + iSwap;
            }
            for (k1 = j1; (k1 < j2) && !threadStopped; k1 <<= 1) {
                delta = TWO_PI / (k1 << 1) * direction * j1;
                angle = 0;
                for (index1 = 0, angle = 0; index1 < k1; index1 += j1) {
                    wt1Imag = java.lang.Math.sin(angle);
                    wt1Real = java.lang.Math.cos(angle);
                    angle += delta;
                    for (index2 = index1; index2 < (index1 + j1); index2++) {
                        k1Double = k1 << 1;
                        for (index3 = index2; index3 < j3; index3 += k1Double) {
                            index = index3 + k1;
                            fReal = realData[index];
                            fImag = imagData[index];
                            imag = (float) ( (fImag * wt1Real) - (fReal * wt1Imag));
                            real = (float) ( (fReal * wt1Real) + (fImag * wt1Imag));
                            imagData[index] = imagData[index3] - imag;
                            realData[index] = realData[index3] - real;
                            imagData[index3] = imagData[index3] + imag;
                            realData[index3] = realData[index3] + real;
                        }
                    }
                }
            }

            if ( !image25D) {
                fireProgressStateChanged( (Math.round(10 + ((float) (i + 1) / ndim * 80))), null, null);
            }
        }

        if (threadStopped) {
            return;
        }

        if (transformDir == AlgorithmFFT2.INVERSE) {

            if ( !image25D) {
                fireProgressStateChanged( -1, null, "Centering data before inverse FFT ...");
            }

            center(realData, imagData);
        }

        if (transformDir == AlgorithmFFT2.INVERSE) {
        	if (ndim >= 2) {
        		newSliceSize = newDimLengths[0]*newDimLengths[1];
        	}
        	if (complexInverse) {
        		for (i = 0; i < newLength; i++) {
        			if (image25D) {
        				realData[i] = realData[i] / newSliceSize;
        				imagData[i] = imagData[i] / newSliceSize;
        			}
        			else {
	                    realData[i] = realData[i] / newLength;
	                    imagData[i] = imagData[i] / newLength;
        			}
	            }
	
	            originalDimLengths = srcImage.getOriginalExtents();
	            finalData = new float[2 * AlgorithmBase.calculateImageSize(originalDimLengths)];
	            if ( !hasSameDimension(newDimLengths, originalDimLengths)) {
	                if (ndim == 1) {
	                    for (i = 0; i < originalDimLengths[0]; i++) {
	                    	finalData[2*i] = realData[i];
	                    	finalData[2*i+1] = imagData[i];
	                    }
	                } else if (ndim == 2) {
	                    for (i = 0; i < originalDimLengths[1]; i++) {
	                    	for (j = 0; j < originalDimLengths[0]; j++) {
	                    		finalData[2*(i*originalDimLengths[0] + j)] = realData[i*newDimLengths[0] + j];
	                    		finalData[2*(i*originalDimLengths[0] + j)+1] = imagData[i*newDimLengths[0] + j];
	                    	}
	                    }
	                } else if (ndim == 3) {
	                    originalSliceSize = originalDimLengths[0]*originalDimLengths[1];
	                    for (i = 0; i < originalDimLengths[2]; i++) {
	                        for (j = 0; j < originalDimLengths[1]; j++) {
	                            for (k = 0; k < originalDimLengths[0]; k++) {
	                            	finalData[2*(i*originalSliceSize + j*originalDimLengths[0] + k)] =
	                                realData[i*newSliceSize + j*newDimLengths[0] + k];
	                            	finalData[2*(i*originalSliceSize + j*originalDimLengths[0] + k)+1] =
		                            imagData[i*newSliceSize + j*newDimLengths[0] + k];
	                            }
	                        }
	                    }
	                } else if (ndim == 4) {
	                    originalSliceSize = originalDimLengths[0]*originalDimLengths[1];
	                    originalVolumeSize = originalSliceSize * originalDimLengths[2];
	                    newVolumeSize = newSliceSize * newDimLengths[2];
	                    for (i = 0; i < originalDimLengths[3]; i++) {
	                    	for (j = 0; j < originalDimLengths[2]; j++) {
	                    		for (k = 0; k < originalDimLengths[1]; k++) {
	                    			for (m = 0; m < originalDimLengths[0]; m++) {
	                    				finalData[2*(i*originalVolumeSize + j*originalSliceSize + 
	                    						  k*originalDimLengths[0] + m)] =
	                                    realData[i*newVolumeSize + j*newSliceSize + k*newDimLengths[0] + m];
	                    				finalData[2*(i*originalVolumeSize + j*originalSliceSize + 
	                    						  k*originalDimLengths[0] + m)+1] =
	                                    imagData[i*newVolumeSize + j*newSliceSize + k*newDimLengths[0] + m];
	                    			}
	                    		}
	                    	}
	                    }
	                }
	            } else {
	                System.arraycopy(realData, 0, finalData, 0, newLength);
	                for (i = 0; i < newLength; i++) {
	                	finalData[2*i] = realData[i];
	                	finalData[2*i+1] = imagData[i];
	                }
		        }	
        	} // if (complexInverse)
        	else { // !complexInverse
	            imagData = null;
	            for (i = 0; i < newLength; i++) {
	            	if (image25D) {
	            	    realData[i] = realData[i] / newSliceSize;	
	            	}
	            	else {
	                    realData[i] = realData[i] / newLength;
	                    // imagData[i] = imagData[i] / newLength;
	            	}
	            }
	
	            originalDimLengths = srcImage.getOriginalExtents();
	            finalData = new float[AlgorithmBase.calculateImageSize(originalDimLengths)];
	            if ( !hasSameDimension(newDimLengths, originalDimLengths)) {
	                if (ndim == 1) {
	                    System.arraycopy(realData, 0, finalData, 0, originalDimLengths[0]);
	                } else if (ndim == 2) {
	                    ArrayUtil.copy2D(realData, 0, newDimLengths[0], newDimLengths[1], finalData, 0,
	                            originalDimLengths[0], originalDimLengths[1], false);
	                } else if (ndim == 3) {
	                    ArrayUtil.copy3D(realData, 0, newDimLengths[0], newDimLengths[1], newDimLengths[2], finalData, 0,
	                            originalDimLengths[0], originalDimLengths[1], originalDimLengths[2], false);
	                } else if (ndim == 4) {
	                    ArrayUtil.copy4D(realData, newDimLengths[0], newDimLengths[1], newDimLengths[2], dimLengths[3],
	                            finalData, originalDimLengths[0], originalDimLengths[1], originalDimLengths[2],
	                            originalDimLengths[3], false);
	                }
	            } else {
	                System.arraycopy(realData, 0, finalData, 0, newLength);
		        }
        	} // else !complexInverse
        } // if (transformDir == AlgorithmFFT2.INVERSE)

    }

    public void afterExecute() {
        fireProgressStateChanged( -1, null, "Storing FFT in source image ...");

        if (transformDir == AlgorithmFFT2.FORWARD) {

            // In the frequency domain so complex data is needed
            try {
                if (destImage == null) {
                    srcImage.reallocate(ModelStorageBase.COMPLEX, newDimLengths);
                } else {
                    destImage.reallocate(ModelStorageBase.COMPLEX, newDimLengths);
                }
            } catch (final IOException error) {
                displayError("AlgorithmFFT2: IOException on srcImage.reallocate");

                setCompleted(false);

                return;
            } catch (final OutOfMemoryError e) {
                displayError("AlgorithmFFT2: Out of memory on srcImage.reallocate");

                setCompleted(false);

                return;
            }

            try {

                // Calculate image minimum and maximum based on magnitude
                // if logMagDisplay is false or the log10(1 + magnitude) if logMagDisplay
                // is true
                if (destImage == null) {
                    srcImage.importComplexData(0, realData, imagData, true, logMagDisplay);
                } else {
                    destImage.importComplexData(0, realData, imagData, true, logMagDisplay);
                }
            } catch (final IOException error) {
                displayError("AlgorithmFFT2: IOException on source image import complex data");

                setCompleted(false);

                return;
            } catch (final OutOfMemoryError e) {
                System.gc();
                displayError("AlgorithmFFT2: Out of memory on source image import complex data");

                setCompleted(false);

                return;
            }
            if (destImage == null) {
                srcImage.setOriginalExtents(dimLengths);
                srcImage.setOriginalMinimum(minimum);
                srcImage.setOriginalMaximum(maximum);
            } else {
                destImage.setOriginalExtents(dimLengths);
                destImage.setOriginalMinimum(minimum);
                destImage.setOriginalMaximum(maximum);
            }
        } else if (transformDir == AlgorithmFFT2.INVERSE) {
            fireProgressStateChanged( -1, null, "Storing inverse FFT in source image ...");

            // back in the spatial domain so only realData is now present
            try {
                if (destImage == null) {
                	if (complexInverse) {
                		srcImage.reallocate(ModelStorageBase.COMPLEX, originalDimLengths);
                	}
                	else {
                        srcImage.reallocate(ModelStorageBase.FLOAT, originalDimLengths);
                	}
                } else {
                	if (complexInverse) {
                		destImage.reallocate(ModelStorageBase.COMPLEX, originalDimLengths);
                	}
                	else {
                        destImage.reallocate(ModelStorageBase.FLOAT, originalDimLengths);
                	}
                }
            } catch (final IOException error) {
                displayError("AlgorithmFFT2: IOException on srcImage.reallocate");

                setCompleted(false);

                return;
            } catch (final OutOfMemoryError e) {
                System.gc();
                displayError("AlgorithmFFT2: Out of memory on srcImage.reallocate");

                setCompleted(false);

                return;
            }

            try {
                if (destImage == null) {
                    srcImage.importData(0, finalData, true);
                } else {
                    destImage.importData(0, finalData, true);
                }
            } catch (final IOException error) {
                displayError("AlgorithmFFT2: IOException on source image import data");

                setCompleted(false);

                return;
            } catch (final OutOfMemoryError e) {
                displayError("AlgorithmFFT2: Out of memory on source image import data");

                setCompleted(false);

                return;
            }
        }

        setCompleted(true);
    }

    private boolean hasSameDimension(final int[] oldDims, final int[] newDims) {
        if (oldDims.length == newDims.length) {
            for (int i = 0; i < oldDims.length; i++) {
                if (oldDims[i] != newDims[i]) {
                    return false;
                }
            }
            return true;
        }
        return false;
    }

    /**
     * In order to use FFT, first thing is to rearrange the order of signals.
     * 
     * @param l the length of one dimension FFT
     * @return the indices used by FFT
     * @author Hailong Wang, Ph.D
     */
    public static final int[] generateFFTIndices(final int l) {
        final int n = (int) (Math.log(l) / Math.log(2));
        final int l2 = (int) Math.pow(2, n);
        if (l != l2) {
            System.out.println("The value of l must be the power of 2: " + l);
            return null;
        }

        final int[] indices = new int[l];
        for (int i = 0; i < n; i++) {
            final int max = (int) Math.pow(2, i);

            for (int j = 0; j < max; j++) {
                indices[max + j] += indices[j] + (int) Math.pow(2, n - i - 1);
            }
        }

        for (int i = 0; i < l; i++) {
            System.out.println(i + "\t" + indices[i]);
        }
        return indices;
    }
    
    /**
     * Turns on/off using OpenCL to compute the FFT.
     * @param on
     */
    public void useOCL( boolean on )
    {
    	useOCL = on;
    }

    /**
     * Swap slices in order to apply FFT algorithm.
     * 
     * @param rdata
     * @param idata
     * @param xdim
     * @param ydim
     * @param zdim
     * @param plane
     */
    public static void swapSlices(final float[] rdata, final float[] idata, final int xdim, final int ydim,
            final int zdim, final int plane) {
        int[] indices = null;
        final int sliceLength = xdim * ydim;

        if (plane == AlgorithmFFT2.SLICE_XY) {
            indices = AlgorithmFFT2.generateFFTIndices(zdim);
            float rtemp, itemp;
            for (int i = 0; i < indices.length; i++) {
                if (indices[i] < 0 || indices[indices[i]] == indices[i]) {
                    continue;
                }
                for (int j = 0; j < sliceLength; j++) {
                    rtemp = rdata[indices[i] * sliceLength + j];
                    itemp = idata[indices[i] * sliceLength + j];
                    rdata[indices[i] * sliceLength + j] = rdata[indices[indices[i]] * sliceLength + j];
                    idata[indices[i] * sliceLength + j] = idata[indices[indices[i]] * sliceLength + j];
                    rdata[indices[indices[i]] * sliceLength + j] = rtemp;
                    idata[indices[indices[i]] * sliceLength + j] = itemp;
                }
                indices[indices[i]] = -1;
                indices[i] = -1;
            }
        } else if (plane == AlgorithmFFT2.SLICE_YZ) {
            indices = AlgorithmFFT2.generateFFTIndices(xdim);
            float rtemp, itemp;
            final int step = xdim;
            for (int i = 0; i < indices.length; i++) {
                if (indices[i] < 0 || indices[indices[i]] == indices[i]) {
                    continue;
                }
                int index1 = indices[i];
                int index2 = indices[indices[i]];
                indices[indices[i]] = -1;
                indices[i] = -1;

                while (index1 < xdim * ydim * zdim) {
                    rtemp = rdata[index1];
                    itemp = idata[index1];
                    rdata[index1] = rdata[index2];
                    idata[index1] = idata[index2];
                    rdata[index2] = rtemp;
                    idata[index2] = itemp;
                    index1 += step;
                    index2 += step;
                }
            }
        } else if (plane == AlgorithmFFT2.SLICE_ZX) {
            indices = AlgorithmFFT2.generateFFTIndices(ydim);
            float rtemp, itemp;
            for (int i = 0; i < indices.length; i++) {
                if (indices[i] < 0 || indices[indices[i]] == indices[i]) {
                    continue;
                }
                for (int j = 0; j < zdim; j++) {
                    for (int k = 0; k < xdim; k++) {
                        rtemp = rdata[k + indices[i] * ydim + sliceLength * j];
                        itemp = idata[k + indices[i] * ydim + sliceLength * j];
                        rdata[k + indices[i] * ydim + sliceLength * j] = rdata[k + indices[indices[i]] * ydim
                                + sliceLength * j];
                        idata[k + indices[i] * ydim + sliceLength * j] = idata[k + indices[indices[i]] * ydim
                                + sliceLength * j];
                        rdata[k + indices[indices[i]] * ydim + sliceLength * j] = rtemp;
                        idata[k + indices[indices[i]] * ydim + sliceLength * j] = itemp;
                    }
                }
                indices[indices[i]] = -1;
                indices[i] = -1;
            }
        } else {
            throw new IllegalArgumentException("The value of variable plane is illeagal: " + plane);
        }
    }
}
