package gov.nih.mipav.model.algorithms;


import gov.nih.mipav.model.structures.*;

import java.io.*;


/**
 *
 *      Convolves kernel with a 2D or 3D image - only pixels where
 *      the kernel is completely contained in the image are
 *      convolved, otherwise they are set to zero. This
 *      is reasonable since data at the edges of images is rarely
 *      used and large kernels should not be used since it is
 *      much faster to perform FFT, filter, and IFFT. The break
 *      even point is probably around a kernel size of 11 or so.
 *      To replace the image model with a convolved version simply
 *      construct this object with the source image as both the srcImg
 *      and the destImg.
 *      <p>
 *      Since this class extends the AlgorithmBase class that extends the
 *      Thread class it can be run in
 *      its own thread by invoking algoConvolver3DObj.start();
 *      It can also be invoked without a new thread by calling the
 *      the run() method directly (ie. algoConvolver3DObj.run()).
 *      <p><ol>
 *      <li>Source image is exported  (locked and unlocked by export)</li>
 *      <li>Kernel is exported</li>
 *      <li>Destination image is locked</li>
 *      <li>Image is convolved with kernel</li>
 *      <li>Destination image is unlocked</li>
 *      <li>Return</li>
 *      </ol>
 *		@version 0.1 Aug 1, 1997
 *		@author Matthew J. McAuliffe, Ph.D.
 */
public class AlgorithmConvolver extends AlgorithmBase {
    /**
     * The convolution kernel.
     */
    private ModelImage kernel;
    
    /**
     * The image data to convolve.
     */
    private float[] imgBuffer;

    /**
     *   Sets the source and destination images and calls the appropriate
     *   method based on image dimensionality.
     *   @param destImg  Destination image of result.
     *   @param srcImg   Source image to be convolved with kernel.
     *   @param kern     Kernel image.
     */
    public AlgorithmConvolver( ModelImage destImg, ModelImage srcImg, ModelImage kern ) {
        super( destImg, srcImg );
        kernel = kern;

        if ( srcImage.getNDims() == 2 ) {
            convolver2D();
        } else if ( srcImage.getNDims() > 2 ) {
            convolver3D();
        }
    }

    /**
     *   Convolves kernel with a 2D image - only pixels where
     *   the kernel is completely contained in the image are
     *   convolved, otherwise they are set to zero.
     */
    private void convolver2D() {

        if ( srcImage == null ) {
            displayError( "Source Image is null" );
            return;
        }
        if ( srcImage.getNDims() != 2 ) {
            displayError( "Source Image is not 2D" );
            return;
        }

        try {
            int length = srcImage.getSliceSize();

            imgBuffer = new float[length];
            srcImage.exportData( 0, length, imgBuffer ); // locks and releases lock
        } catch ( IOException error ) {
            displayError( "Algorithm Convolver: Image(s) locked" );
            setCompleted( false );
            return;
        } catch ( OutOfMemoryError e ) {
            displayError( "Algorithm Convolver: Out of memory" );
            setCompleted( false );
            return;
        }

    }

    /**
     *   Convolving a kernel with a 3D image -
     *   only pixels where the kernel is completely contained in
     *   the image are convolved, otherwise they are set to zero.
     */
    private void convolver3D() {
        if ( srcImage == null ) {
            displayError( "Source Image is null" );
            return;
        }
        if ( srcImage.getNDims() != 3 ) {
            displayError( "Source Image is not 3D" );
            return;
        }

        try {
            int length = srcImage.getSliceSize() * srcImage.getExtents()[2];

            imgBuffer = new float[length];
            srcImage.exportData( 0, length, imgBuffer ); // locks and releases lock
        } catch ( IOException error ) {
            displayError( "Algorithm Convolver: Image(s) locked" );
            setCompleted( false );
            return;
        } catch ( OutOfMemoryError e ) {
            displayError( "Algorithm Convolver: Out of memory" );
            setCompleted( false );
            return;
        }
    }

    /**
     *   Prepares this class for destruction.
     */
    public void finalize() {

        kernel = null;
        destImage = null;
        srcImage = null;
        imgBuffer = null;
        super.finalize();
    }

    /**
     *   Accessor that sets the destination and kernel images.
     *   @param destImg  Destination image.
     *   @param kern     Kernel image.
     */
    public void setImages( ModelImage destImg, ModelImage kern ) {
        destImage = destImg;
        kernel = kern;
    }

    /**
     *   Begins execution of the convolver.
     */
    public void runAlgorithm() {
        if ( srcImage.getNDims() == 2 ) {
            run2D();
        } else if ( srcImage.getNDims() > 2 ) {
            run3D();
        }
    }

    /**
     *   Begins the excution of the 2D convolver.
     */
    public void run2D() {
        float[] kernelBuffer;

        if ( destImage == null ) {
            displayError( "Destination Image is null" );
            return;
        }
        if ( srcImage == null ) {
            displayError( "Source Image is null" );
            return;
        }
        if ( kernel == null ) {
            displayError( "Kernel is null" );
            return;
        }

        if ( destImage.getNDims() != 2 ) {
            displayError( "Destination Image is not 2D" );
            return;
        }
        if ( srcImage.getNDims() != 2 ) {
            displayError( "Source Image is not 2D" );
            return;
        }
        if ( kernel.getNDims() != 2 ) {
            displayError( "Kernel is not 2D" );
            return;
        }

        try {
            int length = srcImage.getSliceSize();

            imgBuffer = new float[length];
            srcImage.exportData( 0, length, imgBuffer ); // locks and releases lock
        } catch ( IOException error ) {
            errorCleanUp( "Algorithm Convolver: Image (s) locked", false );
            return;
        } catch ( OutOfMemoryError e ) {
            errorCleanUp( "Algorithm Convolver: Out of memory", true );
            return;
        }

        try {
            int length = kernel.getSliceSize();

            kernelBuffer = new float[length];
            kernel.exportData( 0, length, kernelBuffer ); // locks and releases lock
        } catch ( IOException error ) {
            errorCleanUp( "Algorithm Convolver: Kernel locked", false );
            return;
        }

        try {
            destImage.setLock();
        } catch ( IOException error ) {
            errorCleanUp( "Algorithm Convolver: Destination Image locked", false );
            return;
        }

        float sum = 0;
        float norm = 0;
        int i, j, pix, count;

        int offsetX, offsetY;
        int startX, endX;
        int startY, endY;

        int xDim = srcImage.getExtents()[0];
        int yDim = srcImage.getExtents()[1];

        int xKDim = kernel.getExtents()[0];
        int yKDim = kernel.getExtents()[1];

        int halfxKDim = xKDim / 2;
        int halfyKDim = yKDim / 2;
        int imageLength = xDim * yDim;
        int stepY = yKDim * xDim;

        for ( pix = 0; pix < imageLength; pix++ ) {
            offsetX = ( pix % xDim ) - halfxKDim;
            offsetY = ( pix / xDim ) - halfyKDim;
            sum = 0;
            count = 0;
            norm = 0;
            startY = offsetY * xDim;
            endY = startY + stepY;
            for ( j = startY; j < endY; j += xDim ) {
                startX = j + offsetX;
                endX = startX + xKDim;
                for ( i = startX; i < endX; i++ ) {
                    if ( ( j >= 0 ) && ( j < imageLength ) && ( ( i - j ) >= 0 ) && ( ( i - j ) < xDim ) ) {
                        sum += kernelBuffer[count] * imgBuffer[i];
                        if ( kernelBuffer[count] >= 0 ) {
                            norm += kernelBuffer[count];
                        } else {
                            norm += -kernelBuffer[count];
                        }
                    }
                    count++;
                }
            }
            destImage.set( pix, sum / norm );
        }
        destImage.calcMinMax();
        destImage.releaseLock();
        destImage.notifyImageDisplayListeners( null, true );
        setCompleted( true );
    }

    /**
     *   Begins the excution of the 3D convolver.
     */
    public void run3D() {

        setStartTime();

        float[] kernelBuffer;

        if ( destImage == null ) {
            displayError( "Destination Image is null" );
            return;
        }
        if ( srcImage == null ) {
            displayError( "Source Image is null" );
            return;
        }
        if ( kernel == null ) {
            displayError( "Kernel is null" );
            return;
        }

        if ( destImage.getNDims() != 3 ) {
            displayError( "Destination Image is not 3D" );
            return;
        }
        if ( srcImage.getNDims() != 3 ) {
            displayError( "Source Image is not 3D" );
            return;
        }
        if ( kernel.getNDims() != 3 ) {
            displayError( "Kernel is not 3D" );
            return;
        }

        try {
            int length = srcImage.getSliceSize() * srcImage.getExtents()[2];

            imgBuffer = new float[length];
            srcImage.exportData( 0, length, imgBuffer ); // locks and releases lock
        } catch ( IOException error ) {
            errorCleanUp( "Algorithm Convolver: Image (s) locked", false );
            return;
        } catch ( OutOfMemoryError e ) {
            errorCleanUp( "Algorithm Convolver: Out of memory", true );
            return;
        }

        try {
            int length = kernel.getSliceSize() * kernel.getExtents()[2];

            kernelBuffer = new float[length];
            kernel.exportData( 0, length, kernelBuffer ); // locks and releases lock
        } catch ( IOException error ) {
            errorCleanUp( "Algorithm Convolver: Kernel locked", false );
            return;
        }

        try {
            destImage.setLock();
        } catch ( IOException error ) {
            errorCleanUp( "Algorithm Convolver: Destination Image locked", false );
            return;
        }

        int indexY;
        float sum = 0;
        float norm = 0;
        int i, j, k, pix, count;
        int offsetX, offsetY, offsetZ;
        int startX, startY, startZ;
        int endX, endY, endZ;

        int xDim = srcImage.getExtents()[0];
        int yDim = srcImage.getExtents()[1];
        int zDim = srcImage.getExtents()[2];
        int volSize = xDim * yDim * zDim;

        int xKDim = kernel.getExtents()[0];
        int yKDim = kernel.getExtents()[1];
        int zKDim = kernel.getExtents()[2];

        int halfxKDim = xKDim / 2;
        int halfyKDim = yKDim / 2;
        int halfzKDim = zKDim / 2;

        int size = srcImage.getSize();
        int sliceSize = xDim * yDim;
        int stepZ = zKDim * sliceSize;
        int stepY = yKDim * xDim;

        for ( pix = 0; pix < size; pix++ ) {

            offsetX = ( pix % xDim ) - halfxKDim;
            offsetY = ( ( pix / xDim ) % yDim ) - halfyKDim;
            offsetZ = ( pix / ( sliceSize ) ) - halfzKDim;

            count = 0;
            sum = 0;
            norm = 0;
            indexY = offsetY * xDim;
            startZ = offsetZ * sliceSize;
            endZ = startZ + stepZ;
            for ( k = startZ; k < endZ; k += sliceSize ) {
                startY = k + indexY;
                endY = startY + stepY;
                for ( j = startY; j < endY; j += xDim ) {
                    startX = j + offsetX;
                    endX = startX + xKDim;
                    for ( i = startX; i < endX; i++ ) {
                        if ( ( k >= 0 ) && ( k < volSize ) && ( ( j - k ) >= 0 ) && ( ( j - k ) < sliceSize )
                                && ( ( i - j ) >= 0 ) && ( ( i - j ) < xDim ) ) {
                            sum += kernelBuffer[count] * imgBuffer[i];
                            if ( kernelBuffer[count] >= 0 ) {
                                norm += kernelBuffer[count];
                            } else {
                                norm += -kernelBuffer[count];
                            }
                        }
                        count++;
                    }
                }
            }
            destImage.set( pix, sum / norm );
        }

        destImage.calcMinMax();
        destImage.releaseLock();
        destImage.notifyImageDisplayListeners( null, true );
        setCompleted( true );

        computeElapsedTime();
    }

    /**
     *   A static function that convolves a kernel with an image at a position.
     *   @param pix      index indicating location of convolution
     *   @param iExtents image dimensions
     *   @param image    image data
     *   @param kExtents kernel dimensions
     *   @param kernel   kernel data
     *   @return         the value of the pixel after convolution with the kernel
     */
    public synchronized static final float convolve2DPt( int pix, int[] iExtents,
            float[] image, int[] kExtents, float[] kernel ) {

        int i, j;
        int offsetX, offsetY;
        int xDim = iExtents[0];
        int yDim = iExtents[1];
        int xKDim = kExtents[0];
        int yKDim = kExtents[1];
        int yLimit = xDim * yDim;

        int startX, startY;
        int endX, endY;
        int count;
        float sum;
        float norm = 0;

        offsetX = ( pix % xDim ) - xKDim / 2;
        offsetY = ( pix / xDim ) - yKDim / 2;
        sum = 0;
        count = 0;
        startY = offsetY * xDim;
        endY = startY + yKDim * xDim;
        for ( j = startY; j < endY; j += xDim ) {
            startX = j + offsetX;
            endX = startX + xKDim;
            for ( i = startX; i < endX; i++ ) {

                if ( ( j >= 0 ) && ( j < yLimit ) && ( ( i - j ) >= 0 ) && ( ( i - j ) < xDim ) ) {
                    // Needed for compiler bug
                    // Run same image twice from AlgorithmLevelSetDiffusion and
                    // array index out of bounds exception shows up
                    if (count >= kernel.length) {
                        break;
                    }
                    sum += kernel[count] * image[i];
                    if ( kernel[count] >= 0 ) {
                        norm += kernel[count];
                    } else {
                        norm += -kernel[count];
                    }
                }
                count++;
            }
        }
        if ( norm > 0 ) {
            return ( sum / norm );
        } else {
            return 0;
        }
    }

    /**
     *   A static function that convolves a kernel with an image at a position.
     *   This version seems to just be used by AlgorithmLapMedianess.
     *   @see gov.nih.mipav.model.algorithms.AlgorithmLapMedianess
     *   @param pix      index indicating location of convolution
     *   @param iExtents image dimensions
     *   @param image    image data
     *   @param kExtents kernel dimensions
     *   @param kernel   kernel data
     *   @return         the value of the pixel after convolution with the kernel
     */
    public static final float convolve2DPtMed( int pix, int[] iExtents,
            float[] image, int[] kExtents, float[] kernel ) {

        int i, j;
        int offsetX, offsetY;
        int xDim = iExtents[0];
        int yDim = iExtents[1];
        int xKDim = kExtents[0];
        int yKDim = kExtents[1];
        int yLimit = xDim * yDim;

        int startX, startY;
        int endX, endY;
        int count;
        float sum;

        offsetX = ( pix % xDim ) - xKDim / 2;
        offsetY = ( pix / xDim ) - yKDim / 2;

        sum = 0;
        count = 0;
        startY = offsetY * xDim;
        endY = startY + yKDim * xDim;
        for ( j = startY; j < endY; j += xDim ) {
            startX = j + offsetX;
            endX = startX + xKDim;
            for ( i = startX; i < endX; i++ ) {
                if ( ( j >= 0 ) && ( j < yLimit ) && ( ( i - j ) >= 0 ) && ( ( i - j ) < xDim ) ) {
                    sum += kernel[count] * image[i];
                }
                count++;
            }
        }
        return sum;
    }

    /**
     *   A static function that convolves a kernel with an image at a position.
     *   This version seems to just be used by AlgorithmLapMedianess.
     *   @see gov.nih.mipav.model.algorithms.AlgorithmLapMedianess
     *   @param pix      index indicating location of convolution
     *   @param iExtents image dimensions
     *   @param image    image data
     *   @param kExtents kernel dimensions
     *   @param kernel   kernel data
     *   @return         the value of the pixel after convolution with the kernel
     */
    public static final float convolve3DPtMed( int pix, int[] iExtents,
            float[] image, int[] kExtents, float[] kernel ) {

        int i, j, k;
        int offsetX, offsetY, offsetZ;
        int xDim = iExtents[0];
        int yDim = iExtents[1];
        int zDim = iExtents[2];
        int xKDim = kExtents[0];
        int yKDim = kExtents[1];
        int zKDim = kExtents[2];
        int sliceSize = xDim * yDim;
        int volSize = sliceSize * zDim;

        int startX, startY, startZ;
        int endX, endY, endZ;
        int count;
        float sum;
        int remainder;

        remainder = pix % sliceSize;
        offsetX = ( remainder % xDim ) - xKDim / 2;
        offsetY = ( remainder / xDim ) - yKDim / 2;
        offsetZ = ( pix / sliceSize ) - zKDim / 2;

        sum = 0;
        count = 0;
        startZ = offsetZ * sliceSize;
        endZ = startZ + zKDim * sliceSize;
        for ( k = startZ; k < endZ; k += sliceSize ) {
            startY = k + offsetY * xDim;
            endY = startY + yKDim * xDim;
            for ( j = startY; j < endY; j += xDim ) {
                startX = j + offsetX;
                endX = startX + xKDim;
                for ( i = startX; i < endX; i++ ) {
                    if ( ( k >= 0 ) && ( k < volSize ) && ( ( j - k ) >= 0 ) && ( ( j - k ) < sliceSize )
                            && ( ( i - j ) >= 0 ) && ( ( i - j ) < xDim ) ) {
                        sum += kernel[count] * image[i];
                    }
                    count++;
                }
            }
        }
        return sum;
    }

    /**
     *   A static function that convolves a kernel with an image at a position.
     *   The convolution is performed only if the whole kernel fits inside the image.
     *   @param pix      index indicating location of convolution
     *   @param iExtents image dimensions
     *   @param image    image data
     *   @param kExtents kernel dimensions
     *   @param kernel   kernel data
     *   @return         the value of the pixel after convolution with the kernel or 0 if the kernel does not fit wholly within the image
     */
    public static final float convolveWhole2DPt( int pix, int[] iExtents,
            float[] image, int[] kExtents, float[] kernel ) {

        int i, j;
        int offsetX, offsetY;
        int xDim = iExtents[0];
        int yDim = iExtents[1];
        int xKDim = kExtents[0];
        int yKDim = kExtents[1];

        int startX, startY;
        int endX, endY;
        int count;
        float sum;
        float norm = 0;

        offsetX = ( pix % xDim ) - xKDim / 2;
        offsetY = ( pix / xDim ) - yKDim / 2;
        for ( int p = 0; p < kernel.length; p++ ) {
            norm += Math.abs( kernel[p] );
        }
        if ( ( offsetX >= 0 ) && ( offsetX + xKDim <= xDim ) && ( offsetY >= 0 ) && ( offsetY + yKDim <= yDim ) ) {
            sum = 0;
            count = 0;
            startY = offsetY * xDim;
            endY = startY + yKDim * xDim;
            for ( j = startY; j < endY; j += xDim ) {
                startX = j + offsetX;
                endX = startX + xKDim;
                for ( i = startX; i < endX; i++ ) {
                    sum += kernel[count] * image[i];
                    count++;
                }
            }
            if ( norm > 0 ) {
                return ( sum / norm );
            } else {
                return 0;
            }
        } else {
            return 0;
        }
    }

    /**
     *   A static function that convolves a kernel with an image at a position
     *   The convolution is performed only if the whole kernel fits inside the image
     *   @param pix      index indicating location of convolution
     *   @param iExtents image dimensions
     *   @param image    image data
     *   @param kExtents kernel dimensions
     *   @param kernel   kernel data
     *   @return         the value of the pixel after convolution with the kernel or 0 if the kernel does not fit wholly within the image
     */
    public static final float convolveWhole3DPt( int pix, int[] iExtents,
            float[] image, int[] kExtents, float[] kernel ) {

        int i, j, k;
        int offsetX, offsetY, offsetZ;
        int xDim = iExtents[0];
        int yDim = iExtents[1];
        int zDim = iExtents[2];
        int xKDim = kExtents[0];
        int yKDim = kExtents[1];
        int zKDim = kExtents[2];
        int sliceSize = xDim * yDim;

        int startX, startY, startZ;
        int endX, endY, endZ;
        int count;
        float sum;
        float norm = 0;
        int remainder;

        remainder = pix % sliceSize;
        offsetX = ( remainder % xDim ) - xKDim / 2;
        offsetY = ( remainder / xDim ) - yKDim / 2;
        offsetZ = ( pix / sliceSize ) - zKDim / 2;

        for ( int p = 0; p < kernel.length; p++ ) {
            norm += Math.abs( kernel[p] );
        }
        if ( ( offsetX >= 0 ) && ( offsetX + xKDim <= xDim ) && ( offsetY >= 0 ) && ( offsetY + yKDim <= yDim )
                && ( offsetZ >= 0 ) && ( offsetZ + zKDim <= zDim ) ) {
            sum = 0;
            count = 0;
            startZ = offsetZ * sliceSize;
            endZ = startZ + zKDim * sliceSize;
            for ( k = startZ; k < endZ; k += sliceSize ) {
                startY = k + offsetY * xDim;
                endY = startY + yKDim * xDim;
                for ( j = startY; j < endY; j += xDim ) {
                    startX = j + offsetX;
                    endX = startX + xKDim;
                    for ( i = startX; i < endX; i++ ) {
                        sum += kernel[count] * image[i];
                        count++;
                    }
                }
            }
            if ( norm > 0 ) {
                return ( sum / norm );
            } else {
                return 0;
            }
        } else {
            return 0;
        }
    }

    /**
     *   A static function that convolves a kernel with an RGB image at a position.
     *   @param pix      index indicating location of convolution
     *   @param iExtents image dimensions
     *   @param image    image data
     *   @param kExtents kernel dimensions
     *   @param kernel   kernel data
     *   @return         the value of the pixel after convolution with the kernel
     */
    public static final float convolve2DRGBPt( int pix, int[] iExtents,
            float[] image, int[] kExtents, float[] kernel ) {

        int i, j;
        int offsetX, offsetY;
        int xDim = iExtents[0];
        int yDim = iExtents[1];
        int yLimit = 4 * xDim * yDim;
        int xKDim = kExtents[0];
        int yKDim = kExtents[1];
        int offset = 4 * xDim;

        int startX, startY;
        int endX, endY;
        int count;
        float sum;
        float norm = 0;

        offsetX = ( pix % ( offset ) ) - ( xKDim ) / 2 * 4;
        offsetY = ( pix / ( offset ) ) - ( yKDim ) / 2;

        sum = 0;
        count = 0;
        startY = offsetY * offset;
        endY = startY + yKDim * offset;
        for ( j = startY; j < endY; j += offset ) {
            startX = j + offsetX;
            endX = startX + xKDim * 4;
            for ( i = startX; i < endX; i += 4 ) {
                if ( ( j >= 0 ) && ( j < yLimit ) && ( ( i - j ) >= 0 ) && ( ( i - j ) < offset ) ) {
                    sum += kernel[count] * image[i];
                    if ( kernel[count] >= 0 ) {
                        norm += kernel[count];
                    } else {
                        norm += -kernel[count];
                    }
                }
                count++;
            }
        }
        if ( norm > 0 ) {
            return ( sum / norm );
        } else {
            return 0;
        }
    }

    /**
     *   A static function that convolves a kernel with an image at a position.
     *   @param pt       floating point indicating location of convolution
     *   @param iExtents image dimensions
     *   @param image    image data
     *   @param kExtents kernel dimensions
     *   @param kernel   kernel data
     *   @return         the value of the pixel after convolution with the kernel
     */
    public static final float convolve2DPt( Point2Df pt, int[] iExtents,
            float[] image, int[] kExtents, float[] kernel ) {

        int i, j;
        float dx, dy;
        int offsetX, offsetY;
        int xDim = iExtents[0];
        int yDim = iExtents[1];
        int yLimit = xDim * yDim;
        int xKDim = kExtents[0];
        int yKDim = kExtents[1];

        int startX, startY;
        int endX, endY;
        int count;
        float sum;
        float norm = 0;

        dx = pt.x - (int) pt.x;
        dy = pt.y - (int) pt.y;
        offsetX = (int) pt.x - xKDim / 2;
        offsetY = (int) pt.y - yKDim / 2;
        sum = 0;
        count = 0;
        startY = offsetY * xDim;
        endY = startY + yKDim * xDim;
        for ( j = startY; j < endY; j += xDim ) {
            startX = j + offsetX;
            endX = startX + xKDim;
            for ( i = startX; i < endX; i++ ) {
                if ( ( j >= 0 ) && ( j < yLimit ) && ( ( i - j ) >= 0 ) && ( ( i - j ) < xDim ) ) {
                    sum += kernel[count] * getBilinear( i, dx, dy, iExtents, image );
                    if ( kernel[count] >= 0 ) {
                        norm += kernel[count];
                    } else {
                        norm += -kernel[count];
                    }
                }
                count++;
            }
        }
        if ( norm > 0 ) {
            return ( sum / norm );
        } else {
            return 0;
        }
    }

    /**
     *   Performs bilinear interpolation of image data.
     *   @param  i        index into image
     *   @param  dx       change in x from integer
     *   @param  dy       change in y from integer
     *   @param  iExtents dimensions of image
     *   @param  image    image data
     *   @return          the bilinearly interpolated value
     */
    private static final float getBilinear( int i, float dx, float dy,
            int[] iExtents, float[] image ) {

        int xDim = iExtents[0];
        float x1, x2;
        int ix, iy;

        // The below code prevents an out of bounds index being used for image
        // when the y coordinate is exactly equal to ydim - 1.
        if ( dx == 0.0f ) {
            ix = i;
        } else {
            ix = i + 1;
        }

        if ( dy == 0.0f ) {
            iy = 0;
        } else {
            iy = xDim;
        }

        x1 = ( 1 - dx ) * image[i] + dx * image[ix];
        x2 = ( 1 - dx ) * image[i + iy] + dx * image[ix + iy];

        return (float) ( ( 1 - dy ) * x1 + dy * x2 );
    }

    /**
     *   A static function that convolves a kernel with an image at a position.
     *   @param pix      index indicating location of convolution
     *   @param iExtents image dimensions
     *   @param image    image data
     *   @param kExtents kernel dimensions
     *   @param kernel   kernel data
     *   @return         the value of the pixel after convolution with the kernel
     */
    public static final float convolve3DPt( int pix, int[] iExtents,
            float[] image, int[] kExtents, float[] kernel ) {

        int i, j, k;
        int offsetX, offsetY, offsetZ;
        int sliceSize = iExtents[0] * iExtents[1];
        int volSize = sliceSize * iExtents[2];
        int xDim = iExtents[0];
        int xKDim = kExtents[0];
        int indexY;
        int stepY, stepZ;
        int startX, startY, startZ;
        int endX, endY, endZ;
        int count;
        float sum;
        float norm = 0;

        offsetX = ( pix % xDim ) - kExtents[0] / 2;
        offsetY = ( ( ( pix % sliceSize ) / xDim ) ) - kExtents[1] / 2;
        offsetZ = ( pix / ( sliceSize ) ) - kExtents[2] / 2;

        count = 0;
        sum = 0;
        indexY = offsetY * xDim;
        stepY = kExtents[1] * xDim;
        stepZ = kExtents[2] * sliceSize;
        startZ = offsetZ * sliceSize;
        endZ = startZ + stepZ;
        for ( k = startZ; k < endZ; k += sliceSize ) {
            if ( ( k >= 0 ) && ( k < volSize ) ) {
                startY = k + indexY;
                endY = startY + stepY;
                for ( j = startY; j < endY; j += xDim ) {
                    if ( ( ( j - k ) >= 0 ) && ( ( j - k ) < sliceSize ) ) {
                        startX = j + offsetX;
                        endX = startX + xKDim;
                        for ( i = startX; i < endX; i++ ) {
                            if ( ( ( i - j ) >= 0 ) && ( ( i - j ) < xDim ) ) {
                                sum += kernel[count] * image[i];
                                if ( kernel[count] >= 0 ) {
                                    norm += kernel[count];
                                } else {
                                    norm += -kernel[count];
                                }
                            }
                            count++;
                        }
                    }
                }
            }
        }
        if ( norm > 0 ) {
            return ( sum / norm );
        } else {
            return 0;
        }
    }

    /**
     *   A static function that convolves a kernel with an RGB image at a position.
     *   @param pix      index indicating location of convolution
     *   @param iExtents image dimensions
     *   @param image    image data
     *   @param kExtents kernel dimensions
     *   @param kernel   kernel data
     *   @return         the value of the pixel after convolution with the kernel
     */
    public static final float convolve3DRGBPt( int pix, int[] iExtents,
            float[] image, int[] kExtents, float[] kernel ) {

        int i, j, k;
        int offsetX, offsetY, offsetZ;

        int sliceSize = iExtents[0] * iExtents[1] * 4;
        int volSize = iExtents[2] * sliceSize;
        int xDim = iExtents[0];
        int xKDim = kExtents[0];
        int indexY;
        int stepY, stepZ;
        int startX, startY, startZ;
        int endX, endY, endZ;
        int count;
        float sum;
        float norm = 0;
        int offset = 4 * xDim;

        offsetX = ( pix % offset ) - kExtents[0] / 2 * 4;
        offsetY = ( ( ( pix % sliceSize ) / offset ) ) - kExtents[1] / 2;
        offsetZ = ( pix / ( sliceSize ) ) - kExtents[2] / 2;

        count = 0;
        sum = 0;
        indexY = offsetY * offset;
        stepY = kExtents[1] * offset;
        stepZ = kExtents[2] * sliceSize;
        startZ = offsetZ * sliceSize;
        endZ = startZ + stepZ;
        for ( k = startZ; k < endZ; k += sliceSize ) {
            startY = k + indexY;
            endY = startY + stepY;
            for ( j = startY; j < endY; j += offset ) {
                startX = j + offsetX;
                endX = startX + xKDim * 4;
                for ( i = startX; i < endX; i += 4 ) {
                    if ( ( k >= 0 ) && ( k < volSize ) && ( ( j - k ) >= 0 ) && ( ( j - k ) < sliceSize )
                            && ( ( i - j ) >= 0 ) && ( ( i - j ) < offset ) ) {
                        sum += kernel[count] * image[i];
                        if ( kernel[count] >= 0 ) {
                            norm += kernel[count];
                        } else {
                            norm += -kernel[count];
                        }
                    }
                    count++;
                }
            }
        }
        if ( norm > 0 ) {
            return ( sum / norm );
        } else {
            return 0;
        }
    }

    /**
     *   A static function that convolves a kernel with an image at a position.
     *   @param pt       floating point indicating location of convolution
     *   @param iExtents image dimensions
     *   @param image    image data
     *   @param kExtents kernel dimensions
     *   @param kernel   kernel data
     *   @return         the value of the pixel after convolution with the kernel
     */
    public static final float convolve3DPt( float[] pt, int[] iExtents,
            float[] image, int[] kExtents, float[] kernel ) {

        int i, j, k;
        int offsetX, offsetY, offsetZ;
        float dx, dy, dz;
        int sliceSize = iExtents[0] * iExtents[1];
        int volSize = iExtents[2] * sliceSize;
        int xDim = iExtents[0];
        int xKDim = kExtents[0];
        int indexY;
        int stepY, stepZ;
        int startX, startY, startZ;
        int endX, endY, endZ;
        int count;
        float sum;
        float norm = 0;

        dx = pt[0] - (int) pt[0];
        dy = pt[1] - (int) pt[1];
        dz = pt[2] - (int) pt[2];

        offsetX = (int) pt[0] - kExtents[0] / 2;
        offsetY = (int) pt[1] - kExtents[1] / 2;
        offsetZ = (int) pt[2] - kExtents[2] / 2;

        count = 0;
        sum = 0;
        indexY = offsetY * xDim;
        stepY = kExtents[1] * xDim;
        stepZ = kExtents[2] * sliceSize;
        startZ = offsetZ * sliceSize;
        endZ = startZ + stepZ;
        for ( k = startZ; k < endZ; k += sliceSize ) {
            startY = k + indexY;
            endY = startY + stepY;
            for ( j = startY; j < endY; j += xDim ) {
                startX = j + offsetX;
                endX = startX + xKDim;
                for ( i = startX; i < endX; i++ ) {
                    if ( ( k >= 0 ) && ( k < volSize ) && ( ( j - k ) >= 0 ) && ( ( j - k ) < sliceSize )
                            && ( ( i - j ) >= 0 ) && ( ( i - j ) < xDim ) ) {
                        sum += kernel[count] * getTrilinear( i, dx, dy, dz, iExtents, image );
                        if ( kernel[count] >= 0 ) {
                            norm += kernel[count];
                        } else {
                            norm += -kernel[count];
                        }
                    }
                    count++;
                }
            }
        }
        if ( norm > 0 ) {
            return ( sum / norm );
        } else {
            return 0;
        }
    }

    /**
     *   Performs trilinear interpolation of image data.
     *   @param  i1       index into image
     *   @param  dx       change in x from integer
     *   @param  dy       change in y from integer
     *   @param  dz       change in z from integer
     *   @param  iExtents dimensions of image
     *   @param  image    image data
     *   @return          the trilinearly interpolated data value
     */
    private static final float getTrilinear( int i1, float dx, float dy, float dz,
            int[] iExtents, float[] image ) {

        int xDim = iExtents[0];
        int yDim = iExtents[1];
        int imageSize = xDim * yDim;
        int i2, ix1, ix2, iy;

        float a1, a2;
        float b1, b2;

        // The following code prevents an out of bounds array index from occurring
        // in the case when the z coordinate exactly equals zdim - 1.
        if ( dz == 0.0f ) {
            i2 = i1;
        } else {
            i2 = i1 + imageSize;
        }

        if ( dx == 0.0f ) {
            ix1 = i1;
            ix2 = i2;
        } else {
            ix1 = i1 + 1;
            ix2 = i2 + 1;
        }

        if ( dy == 0.0f ) {
            iy = 0;
        } else {
            iy = xDim;
        }

        a1 = ( 1 - dx ) * image[i1] + dx * image[ix1];
        a2 = ( 1 - dx ) * image[i1 + iy] + dx * image[ix1 + iy];
        b1 = ( 1 - dy ) * a1 + dy * a2;

        a1 = ( 1 - dx ) * image[i2] + dx * image[ix2];
        a2 = ( 1 - dx ) * image[i2 + iy] + dx * image[ix2 + iy];
        b2 = ( 1 - dy ) * a1 + dy * a2;

        return (float) ( ( 1 - dz ) * b1 + dz * b2 );
    }
}
