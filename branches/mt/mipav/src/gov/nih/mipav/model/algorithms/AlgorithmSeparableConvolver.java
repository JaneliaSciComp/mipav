package gov.nih.mipav.model.algorithms;


import gov.nih.mipav.model.structures.ModelImage;
import gov.nih.mipav.util.MipavUtil;

import java.io.IOException;
import java.util.BitSet;
import java.util.concurrent.CountDownLatch;


/**
 * Convolves an image with a separable (symmetric) kernel and returns the result. The Gaussian and its derivatives are
 * separable.
 *
 * <p>Faster than the regualar convolver -- reg 2D: n^d*m^d, sep 2D: d*n^d*m ; d = img dimension, n = img size, m = kern
 * size -- but requires more memory -- ~2 times more for 2D, ~3 times for 3D (plus significant extra memory required for
 * algorithms which use this class (2 or 3 times more, depending on the dimension)).</p>
 *
 * <p>Also note that this convolver has a different interface which must be used than the static point convolution
 * functions which most algorithms use from the AlgorithmConvolver.</p>
 *
 * <p>Color and voi mask code not tested (although the voi code is pretty straight-forward).</p>
 *
 * @see     AlgorithmConvolver
 * @author  Evan McCreedy
 */
public class AlgorithmSeparableConvolver extends AlgorithmBase {

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** Buffer size adjustment for color images. */
    private int cFactor = 1;

    /** Flag to indicate if the source image is color. */
    private boolean colorImage = false;

    int curPercent;
    
    int incIndex;
    /** Holds the result image data. */
    private float[] destBuffer;

    /** Whether to convolve the whole image or just pixels inside a mask. */
    private boolean entireImage = true;

    /** Holds the result image data (in float form) only used with the buffer constructor. */
    private float[] floatDestBuffer;

    /** Holds the original image data. */
    private float[] imgBuffer;

    /** The dimensions of the both source and destination images. */
    private int[] imgExtents;

    /** Holds the kernel image. */
    private ModelImage kernel;

    /** The dimensions of the kernel. */
    private int[] kernelExtents;

    /** The separated kernel in the X dimension. */
    private float[] kernelXBuffer;

    /** The separated kernel in the Y dimension. */
    private float[] kernelYBuffer;

    /** The separated kernel in the Z dimension. */
    private float[] kernelZBuffer;

    /** Flags to indicate which color channels to process. */
    private boolean red = true, green = true, blue = true;

    //~ Constructors ---------------------------------------------------------------------------------------------------

  
    /**
     * Sets destination, source, and kernel images. Call run() to convolve image.
     *
     * @param  destImg  destination image
     * @param  srcImg   source image
     * @param  kern     kernel image (kernel must be symmetric)
     * @param  minProgressValue the minimum progress value.
     * @param  maxProgressValue the maximum progress value.
     */
    public AlgorithmSeparableConvolver(ModelImage destImg, ModelImage srcImg, ModelImage kern) {
        super(destImg, srcImg);
        kernel = kern;

        imgExtents = srcImg.getExtents();
        kernelExtents = kern.getExtents();

        if (srcImage.isColorImage()) {
            cFactor = 4;
            colorImage = true;
        }

        // fill data arrays for image and kernels
        if (srcImg.getNDims() == 2) {
            convolverSetup2D();
        } else if (srcImg.getNDims() == 3) {
            convolverSetup3D();
        }
    }

    /**
     * Sets destination, source, and kernel buffers. Call run() to convolve image.
     *
     * @param  destBuffer  destination image data buffer
     * @param  srcBuffer   source image data buffer
     * @param  iExtents    source and destination image dimensions
     * @param  kernBuffer  kernel image data buffer (kernel must be symmetric)
     * @param  kExtents    kernel dimensions
     * @param  color       whether the image is color
     * @param  minProgressValue the minimum progress value.
     * @param  maxProgressValue the maximum progress value.
     */
    public AlgorithmSeparableConvolver(float[] destBuffer, float[] srcBuffer, int[] iExtents, float[] kernBuffer,
                                       int[] kExtents, boolean color) {
        super(null, null);

        imgBuffer = new float[srcBuffer.length];

        for (int i = 0; i < srcBuffer.length; i++) {
            imgBuffer[i] = srcBuffer[i];
        }

        floatDestBuffer = destBuffer;
        imgExtents = iExtents;
        kernelExtents = kExtents;
        colorImage = color;

        if (color) {
            cFactor = 4;
        }

        this.destBuffer = new float[destBuffer.length];

        //tempImgBuffer = new double[imgBuffer.length];

        if (imgExtents.length == 2) {
            int i;
            int length = kernelExtents[0];

            kernelXBuffer = new float[length];

            for (i = 0; i < length; i++) {
                kernelXBuffer[i] = kernBuffer[i];
            }

            length = kernelExtents[1];
            kernelYBuffer = new float[length];

            for (i = 0; i < length; i++) {
                kernelYBuffer[i] = kernBuffer[i * kExtents[0]];
            }
        } else if (imgExtents.length == 3) {
            int i;
            int length = kernelExtents[0];

            kernelXBuffer = new float[length];

            for (i = 0; i < length; i++) {
                kernelXBuffer[i] = kernBuffer[i];
            }

            length = kernelExtents[1];
            kernelYBuffer = new float[length];

            for (i = 0; i < length; i++) {
                kernelYBuffer[i] = kernBuffer[i * kExtents[0]];
            }

            length = kernelExtents[2];
            kernelZBuffer = new float[length];

            for (i = 0; i < length; i++) {
                kernelZBuffer[i] = kernBuffer[i * kExtents[0] * kExtents[1]];
            }
        }
    }

    /**
     * Sets destination, source, and kernel buffers. Call run() to convolve image.
     *
     * @param  destBuffer   destination image data buffer
     * @param  srcBuffer    source image data buffer
     * @param  iExtents     source and destination image dimensions
     * @param  kernXBuffer  kernel image data buffer in X dimension (kernel must be symmetric)
     * @param  kernYBuffer  kernel image data buffer in Y dimension (kernel must be symmetric)
     * @param  color        whether the image is color
     * @param  minProgressValue the minimum progress value.
     * @param  maxProgressValue the maximum progress value.
     */
    public AlgorithmSeparableConvolver(float[] destBuffer, float[] srcBuffer,
			int[] iExtents, float[] kernXBuffer, float[] kernYBuffer,
			boolean color) {
		super(null, null);

		imgBuffer = new float[srcBuffer.length];

		for (int i = 0; i < srcBuffer.length; i++) {
			imgBuffer[i] = srcBuffer[i];
		}

		floatDestBuffer = destBuffer;
		imgExtents = iExtents;
		colorImage = color;

		if (color) {
			cFactor = 4;
		}

		this.destBuffer = new float[destBuffer.length];

		// tempImgBuffer = new double[imgBuffer.length];

		this.kernelXBuffer = kernXBuffer;
		this.kernelYBuffer = kernYBuffer;

		kernelExtents = new int[] { kernelXBuffer.length, kernelYBuffer.length };
	}

	public AlgorithmSeparableConvolver(float[] srcBuffer, int[] iExtents,
			float[] kernXBuffer, float[] kernYBuffer, boolean color) {
		super(null, null);

		imgBuffer = srcBuffer;

		imgExtents = iExtents;
		colorImage = color;

		if (color) {
			cFactor = 4;
		}

		// tempImgBuffer = new double[imgBuffer.length];

		this.kernelXBuffer = kernXBuffer;
		this.kernelYBuffer = kernYBuffer;

		kernelExtents = new int[] { kernelXBuffer.length, kernelYBuffer.length };
	}

    /**
     * Sets destination, source, and kernel buffers. Call run() to convolve image.
     *
     * @param  destBuffer   destination image data buffer
     * @param  srcBuffer    source image data buffer
     * @param  iExtents     source and destination image dimensions
     * @param  kernXBuffer  kernel image data buffer in X dimension (kernel must be symmetric)
     * @param  kernYBuffer  kernel image data buffer in Y dimension (kernel must be symmetric)
     * @param  kernZBuffer  kernel image data buffer in Z dimension (kernel must be symmetric)
     * @param  color        whether the image is color
     * @param  minProgressValue the minimum progress value.
     * @param  maxProgressValue the maximum progress value.
     */
    public AlgorithmSeparableConvolver(float[] destBuffer, float[] srcBuffer,
			int[] iExtents, float[] kernXBuffer, float[] kernYBuffer,
			float[] kernZBuffer, boolean color) {
		super(null, null);

		imgBuffer = new float[srcBuffer.length];

		for (int i = 0; i < srcBuffer.length; i++) {
			imgBuffer[i] = srcBuffer[i];
		}

		floatDestBuffer = destBuffer;
		imgExtents = iExtents;
		colorImage = color;

		if (color) {
			cFactor = 4;
		}

		this.destBuffer = new float[destBuffer.length];

		this.kernelXBuffer = kernXBuffer;
		this.kernelYBuffer = kernYBuffer;
		this.kernelZBuffer = kernZBuffer;

		kernelExtents = new int[] { kernelXBuffer.length, kernelYBuffer.length,
				kernelZBuffer.length };
	}

	public AlgorithmSeparableConvolver(float[] srcBuffer, int[] iExtents,
			float[] kernXBuffer, float[] kernYBuffer, float[] kernZBuffer,
			boolean color) {
		super(null, null);

		imgBuffer = srcBuffer;

		imgExtents = iExtents;
		colorImage = color;

		if (color) {
			cFactor = 4;
		}

		this.kernelXBuffer = kernXBuffer;
		this.kernelYBuffer = kernYBuffer;
		this.kernelZBuffer = kernZBuffer;

		kernelExtents = new int[] { kernelXBuffer.length, kernelYBuffer.length,
				kernelZBuffer.length };
	}

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * Prepares this class for destruction.
     */
    public void finalize() {
        kernel = null;
        destImage = null;
        srcImage = null;
        imgBuffer = null;
        imgExtents = null;
        kernelExtents = null;
        kernelXBuffer = null;
        kernelYBuffer = null;
        kernelZBuffer = null;
        destBuffer = null;
        floatDestBuffer = null;

        super.finalize();
    }

    /**
     * Begins the excution of the 2D convolver.
     */
    public void run2D() {
        int xDim = imgExtents[0];
        int yDim = imgExtents[1];

        int xoffset = xDim * cFactor;
        int yoffset = yDim * cFactor;

        int stepProgressValue = (imgExtents[0] + imgExtents[1]) / 100;

        int progress = 0;
        float[] xrowBuffer = new float[xoffset];
        float[] xresultBuffer = new float[xoffset];
        // Convolve the image with the X dimension kernel
        for (int row = 0; (row < yDim) && !threadStopped; row++) {

            if (row % stepProgressValue == 0) {
                fireProgressStateChanged(progress++);
            }
            System.arraycopy(imgBuffer, row*xoffset, xrowBuffer, 0, xoffset);
            convolve(xrowBuffer, kernelXBuffer, xresultBuffer);
            System.arraycopy(xresultBuffer, 0, imgBuffer, row*xoffset, xoffset);
        }

        // Convolve the result image from above with the Y dimension kernel
        
        float[] yrowBuffer = new float[yoffset];
        float[] yresultBuffer = new float[yoffset];
        for (int col = 0; (col < xDim) && !threadStopped; col++) {
            if (col % stepProgressValue == 0) {
                fireProgressStateChanged( progress++);
            }
            rowCopy(imgBuffer, col, yrowBuffer, 0, yoffset, xoffset, cFactor);
            convolve(yrowBuffer, kernelYBuffer, yresultBuffer);
            rowCopy(yresultBuffer, 0, imgBuffer, col, yoffset, cFactor, xoffset);
        }
        
        fireProgressStateChanged(100);
        
        setCompleted(true);
    }

    /**
     * Copy a row in x, y or z direction to an array.
     * 
     * @param src		
     * @param srcPos
     * @param dest
     * @param destPos
     * @param length
     * @param srcDist	the distance between two pixels of source data in x, y or z direction.
     * @param destDist	the distance between two pixels of destination data in x, y or z direction.
     */
    public static void rowCopy(float[] src, int srcPos, float[] dest, int destPos, int length, int srcDist, int destDist){
    	for(int i = 0; i < length; i++){
    		dest[destPos + i * destDist] = src[srcPos + i * srcDist];
    	}
    }
    
    /**
     * Perform one-dimension convolution.
     * @param imageBuffer
     * @param kernelBuffer
     * @param resultBuffer
     */
    private void convolve(float[] imageBuffer, float[] kernelBuffer, float[] resultBuffer){
        boolean skipRed = false;
        boolean skipGreen = false;
        boolean skipBlue = false;

        if (colorImage && !red) {
            skipRed = true;
        }

        if (colorImage && !green) {
            skipGreen = true;
        }

        if (colorImage && !blue) {
            skipBlue = true;
        }
    	int kernelDim = kernelBuffer.length;
    	int halfKernelDimTimesCFactor = kernelDim/2 * cFactor;
    	for (int i = 0; i < imageBuffer.length; i++) {
			if (skipRed && ((i % 4) == 1)) {
				resultBuffer[i] = imageBuffer[i];
			} else if (skipGreen && ((i % 4) == 2)) {
				resultBuffer[i] = imageBuffer[i];
			} else if (skipBlue && ((i % 4) == 3)) {
				resultBuffer[i] = imageBuffer[i];
			} else if (entireImage || mask.get(i / cFactor)) {
				int count = 0;
				double sum = 0;
				double norm = 0;
				int start = i - halfKernelDimTimesCFactor;
				int end = start + (kernelDim - 1) * cFactor;
				if (start < 0) {
					count = count - ((start - (cFactor - 1)) / cFactor);
					if (cFactor > 1) {
						start = i % cFactor;
					} else {
						start = 0;
					}
				}
				if (end >= imageBuffer.length) {
					end = imageBuffer.length - cFactor;
				}
				for (int j = start; j <= end; j += cFactor) {
					sum += kernelBuffer[count] * imageBuffer[j];
					if (kernelBuffer[count] > 0) {
						norm += kernelBuffer[count];
					} else {
						norm -= kernelBuffer[count];
					}
					count++;
				}
				resultBuffer[i] = (float)(sum / norm);
			}else{
				resultBuffer[i] = imageBuffer[i];
			}
    	}
	}
    
    /**
	 * Begins the excution of the 3D convolver.
	 */
    public void run3D() {
    	int xDim = imgExtents[0];
    	int yDim = imgExtents[1];
    	int zDim = imgExtents[2];

        int xoffset = xDim * cFactor;
        int yoffset = yDim * cFactor;
        int zoffset = zDim * cFactor;
        int stepProgressValue = (yDim*zDim + xDim*zDim + xDim*yDim) / 100;

        int progress = 0;
        float[] xrowBuffer = new float[xoffset];
        float[] xresultBuffer = new float[xoffset];
        // Convolve the image with the X dimension kernel
        for (int row = 0; (row < yDim*zDim) && !threadStopped; row++) {

            if (row % stepProgressValue == 0) {
                fireProgressStateChanged(progress++);
            }
            System.arraycopy(imgBuffer, row*xoffset, xrowBuffer, 0, xoffset);
            convolve(xrowBuffer, kernelXBuffer, xresultBuffer);
            System.arraycopy(xresultBuffer, 0, imgBuffer, row*xoffset, xoffset);
        }

        // Convolve the result image from above with the Y dimension kernel
        float[] yrowBuffer = new float[yoffset];
        float[] yresultBuffer = new float[yoffset];
        for (int z = 0; (z < zDim) && !threadStopped; z++) {
        	for (int x = 0; (x < xDim) && !threadStopped; x++) {
				if ((z*xDim +x) % stepProgressValue == 0) {
					fireProgressStateChanged(progress++);
				}
				rowCopy(imgBuffer, x+z*xoffset*yoffset, yrowBuffer, 0, yoffset, xoffset, cFactor);
				convolve(yrowBuffer, kernelYBuffer, yresultBuffer);
				rowCopy(yresultBuffer, 0, imgBuffer, x+z*xoffset*yoffset, yoffset, cFactor, xoffset);
			}
        }

        // Convolve the result image from above with the Z dimension kernel
        float[] zrowBuffer = new float[zoffset];
        float[] zresultBuffer = new float[zoffset];
        for (int row = 0; (row < xDim*yDim) && !threadStopped; row++) {
            if (row % stepProgressValue == 0) {
                fireProgressStateChanged( progress++);
            }
            rowCopy(imgBuffer, row, zrowBuffer, 0, zoffset, xoffset*yoffset, cFactor);
            convolve(zrowBuffer, kernelZBuffer, zresultBuffer);
            rowCopy(zresultBuffer, 0, imgBuffer, row, zoffset, cFactor, xoffset*yoffset);
        }
        
        fireProgressStateChanged(100);
      
        setCompleted(true);

    }

    public void run3DMT() {
        setProgressStep((imgExtents[0] * imgExtents[1] + imgExtents[1] * imgExtents[2] + imgExtents[2]*imgExtents[0]) /100);

        // convolve the image with the X dimension kernel
        int nthreads = determineNumberOfThreads();
        final CountDownLatch doneSignalx = new CountDownLatch(nthreads);
        int nrows = imgExtents[1] * imgExtents[2];
        float step = ((float)nrows)/nthreads;
        for(int i = 0; i < nthreads; i++){
            final int start2 = (int)(step*i);
            final int end2 = (int)(step*(i+1));
            Runnable task = new Runnable(){
                public void run(){
              	  convolveX(start2, end2);
              	  doneSignalx.countDown();
                }
            };
            gov.nih.mipav.util.MipavUtil.threadPool.execute(task);
        }
        try{
      	  doneSignalx.await();
        }catch(InterruptedException e){
      	  
        }

        // y kernel dimensions
        final CountDownLatch doneSignaly = new CountDownLatch(nthreads);
        nrows = imgExtents[2];
        step = ((float)nrows)/nthreads;
        for(int i = 0; i < nthreads; i++){
            final int start2 = (int)(step*i);
            final int end2 = (int)(step*(i+1));
            Runnable task = new Runnable(){
                public void run(){
              	  convolveY(start2, end2);
              	  doneSignaly.countDown();
                }
            };
            gov.nih.mipav.util.MipavUtil.threadPool.execute(task);
        }
        try{
      	  doneSignaly.await();
        }catch(InterruptedException e){
      	  
        }

        // z kernel dimensions
        final CountDownLatch doneSignalz = new CountDownLatch(nthreads);
        nrows = imgExtents[0]*imgExtents[1];
        step = ((float)nrows)/nthreads;
        for(int i = 0; i < nthreads; i++){
            final int start2 = (int)(step*i);
            final int end2 = (int)(step*(i+1));
            Runnable task = new Runnable(){
                public void run(){
              	  convolveZ(start2, end2);
              	  doneSignalz.countDown();
                }
            };
            gov.nih.mipav.util.MipavUtil.threadPool.execute(task);
        }
        try{
      	  doneSignalz.await();
        }catch(InterruptedException e){
      	  
        }
        setCompleted(true);
    }

    private void convolveX(final int from, final int to){
    	int xDim = imgExtents[0];
        int xoffset = xDim * cFactor;
        float[] xrowBuffer = new float[xoffset];
        float[] xresultBuffer = new float[xoffset];
        int stepProgressValue = (int)this.getProgressStep();
        // Convolve the image with the X dimension kernel
        for (int row = from; (row < to) && !threadStopped; row++) {

            if (row % stepProgressValue == 0) {
            	makeProgress(1);
                fireProgressStateChanged((int)getProgress());
            }
            System.arraycopy(imgBuffer, row*xoffset, xrowBuffer, 0, xoffset);
            convolve(xrowBuffer, kernelXBuffer, xresultBuffer);
            System.arraycopy(xresultBuffer, 0, imgBuffer, row*xoffset, xoffset);
        }

	}
        
    private void convolveY(final int from, final int to){
    	int xDim = imgExtents[0];
    	int yDim = imgExtents[1];
    	int xoffset = xDim * cFactor;
    	int yoffset = yDim * cFactor;
        float[] yrowBuffer = new float[yoffset];
        float[] yresultBuffer = new float[yoffset];
        int stepProgressValue = (int)getProgressStep();
        for (int z = from; (z < to) && !threadStopped; z++) {
        	for (int x = 0; (x < xDim) && !threadStopped; x++) {
				if ((z*xDim +x) % stepProgressValue == 0) {
					makeProgress(1);
					fireProgressStateChanged((int)getProgress());
				}
				rowCopy(imgBuffer, x+z*xoffset*yoffset, yrowBuffer, 0, yoffset, xoffset, cFactor);
				convolve(yrowBuffer, kernelYBuffer, yresultBuffer);
				rowCopy(yresultBuffer, 0, imgBuffer, x+z*xoffset*yoffset, yoffset, cFactor, xoffset);
			}
        }
	}
        
    private void convolveZ(final int from, final int to){
    	int xDim = imgExtents[0];
    	int yDim = imgExtents[1];
    	int zDim = imgExtents[2];
    	
    	int xoffset = xDim * cFactor;
    	int yoffset = yDim * cFactor;
    	int zoffset = zDim * cFactor;
    	
        float[] zrowBuffer = new float[zoffset];
        float[] zresultBuffer = new float[zoffset];
        int stepProgressValue = (int)getProgressStep();
        for (int row = 0; (row < xDim*yDim) && !threadStopped; row++) {
            if (row % stepProgressValue == 0) {
            	makeProgress(1);
                fireProgressStateChanged((int)getProgress());
            }
            rowCopy(imgBuffer, row, zrowBuffer, 0, zoffset, xoffset*yoffset, cFactor);
            convolve(zrowBuffer, kernelZBuffer, zresultBuffer);
            rowCopy(zresultBuffer, 0, imgBuffer, row, zoffset, cFactor, xoffset*yoffset);
        }    	
    }
        
    /**
	 * Starts the convolution of the source image with the provided kernel.
	 */
    public void runAlgorithm() {
    	long start = System.currentTimeMillis();
        if (imgExtents.length == 2) {
            run2D();
        } else if (imgExtents.length > 2) {
            run3DMT();
//            run3D();
        }

        if (destImage != null) {

            try {
                destImage.importData(0, destBuffer, true);
            } catch (IOException ioe) {
            	gov.nih.mipav.view.MipavUtil.displayError("Unable to import data to destination image.");
            }

            destImage.notifyImageDisplayListeners(null, true);
        } else {

//            // put results into destination buffer as floats
//            for (int i = 0; i < destBuffer.length; i++) {
//                floatDestBuffer[i] = (float) destBuffer[i];
//            }
        }
        System.out.println("Time consumed: " + (System.currentTimeMillis()-start));
    }

    /**
     * Sets what color channels to convolve and tells the convolver that it is working on a color image.
     *
     * @param  _red    process the red channel
     * @param  _green  process the green channel
     * @param  _blue   process the blue channel
     */
    public void setColorChannels(boolean _red, boolean _green, boolean _blue) {
        red = _red;
        green = _green;
        blue = _blue;
        colorImage = true;
    }

    /**
     * Sets the mask to convolve within.
     *
     * @param  newMask  mask to convolve within
     */
    public void setMask(BitSet newMask) {
        mask = newMask;
        entireImage = false;
    }
 
    /**
     * Convolves kernel with a 2D image - The convolution occurs if the kernel is completely or partially contained in
     * the image.
     */
    private void convolverSetup2D() {

        if (srcImage == null) {
            displayError("Source Image is null");

            return;
        }

        if (srcImage.getNDims() != 2) {
            displayError("Source Image is not 2D");

            return;
        }

        if (kernel == null) {
            displayError("Kernel image is null");

            return;
        }

        if (kernel.getNDims() != 2) {
            displayError("Kernel Image is not 2D");

            return;
        }

        if (destImage == null) {
            displayError("Destination Image is null");

            return;
        }

        if (destImage.getNDims() != 2) {
            displayError("Destination Image is not 2D");

            return;
        }

        try {
            int length = imgExtents[0] * imgExtents[1] * cFactor;

            imgBuffer = new float[length];
            srcImage.exportData(0, length, imgBuffer); // locks and releases lock

            destBuffer = new float[length];

            kernel.setLock();

            int i;

            length = kernel.getExtents()[0];
            kernelXBuffer = new float[length];

            for (i = 0; i < length; i++) {
                kernelXBuffer[i] = kernel.getFloat(i);
            }

            length = kernel.getExtents()[1];
            kernelYBuffer = new float[length];

            for (i = 0; i < length; i++) {
                kernelYBuffer[i] = kernel.getFloat(i * kernel.getExtents()[0]);
            }

            kernel.releaseLock();
        } catch (IOException error) {
            displayError("Algorithm Convolver: Image(s) locked");
            setCompleted(false);

            return;
        } catch (OutOfMemoryError e) {
            displayError("Algorithm Convolver: Out of memory");
            setCompleted(false);

            return;
        }
    }

    /**
     * Convolving a kernel with a 3D image - The convolution occurs if the kernel is completely or partially contained
     * in the image.
     */
    private void convolverSetup3D() {

        if (srcImage == null) {
            displayError("Source Image is null");

            return;
        }

        if (srcImage.getNDims() != 3) {
            displayError("Source Image is not 3D");

            return;
        }

        if (kernel == null) {
            displayError("Kernel image is null");

            return;
        }

        if (kernel.getNDims() != 3) {
            displayError("Kernel Image is not 3D");

            return;
        }

        if (destImage == null) {
            displayError("Destination Image is null");

            return;
        }

        if (destImage.getNDims() != 3) {
            displayError("Destination Image is not 3D");

            return;
        }

        try {
            int length = imgExtents[0] * imgExtents[1] * imgExtents[2] * cFactor;

            imgBuffer = new float[length];
            srcImage.exportData(0, length, imgBuffer); // locks and releases lock

            destBuffer = new float[length];

            kernel.setLock();

            int i;

            length = kernel.getExtents()[0];
            kernelXBuffer = new float[length];

            for (i = 0; i < length; i++) {
                kernelXBuffer[i] = kernel.getFloat(i);
            }

            length = kernel.getExtents()[1];
            kernelYBuffer = new float[length];

            for (i = 0; i < length; i++) {
                kernelYBuffer[i] = kernel.getFloat(i * kernel.getExtents()[0]);
            }

            length = kernel.getExtents()[2];
            kernelZBuffer = new float[length];

            for (i = 0; i < length; i++) {
                kernelZBuffer[i] = kernel.getFloat(i * kernel.getExtents()[0] * kernel.getExtents()[1]);
            }

            kernel.releaseLock();
        } catch (IOException error) {
            displayError("Algorithm Convolver: Image(s) locked");
            setCompleted(false);

            return;
        } catch (OutOfMemoryError e) {
            displayError("Algorithm Convolver: Out of memory");
            setCompleted(false);

            return;
        }
    }

    private int determineNumberOfThreads(){
    	int ncores = MipavUtil.getAvailableCores();
    	if(imgExtents.length <= 2){
    		return 1;
    	}
    	int nthreads = imgExtents[0]*imgExtents[1]*imgExtents[2]/(256*256*16);
    	if(nthreads < 1){
    		nthreads = 1;
    	}else if(ncores < nthreads){
    		nthreads = ncores;
    	}
    	System.out.println("The number of threads: " + nthreads);
    	return nthreads;
    }

}
