package gov.nih.mipav.model.algorithms.utilities;


import gov.nih.mipav.model.algorithms.*;
import gov.nih.mipav.model.structures.*;
import gov.nih.mipav.model.structures.ModelStorageBase.DataType;

import java.io.*;


/**
 * This is a convenience function to convert from one image type to another and remap the data into a new range. It
 * assumes that that calling function has checked that the range values are in the range of the image type. For example,
 * an Unsigned Byte image type can represent data from 0 - 255 and therefore if converting from a Short type (-32768 to
 * 32767) the range parameters should fall between 0 and 255. This class can also be used to remap data to a different
 * range without changing the data type!
 * 
 * In going between COMPLEX and DCOMPLEX the real and imaginary signs should not be changed and the imaginary/real ratios 
 * should not be changed so just multiply both the real and imaginary parts by endRange2/endRange1.
 *
 * @version  0.1 June 15, 1998
 * @author   Matthew J. McAuliffe, Ph.D.
 * @see      ModelImage
 */
public class AlgorithmChangeType extends AlgorithmBase {

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** 1 for black and white, 4 for color. */
    private int colorFactor = 1;
    
    /** 1 for real images, 2 for complex. */
    private int complexFactor = 1;

    /** The ending range value of the input image. Typically the image maximum. */
    private double endRange1 = Double.NaN;

    /** The ending range value of the output image. */
    private double endRange2 = Double.NaN;

    /**
     * Indicates the new data type which the image should be converted to.
     *
     * @see  ModelStorageBase
     */
    private DataType newType = null;

    /** The starting range value of the input image. Typically the image minimum. */
    private double stRange1 = Double.NaN;

    /** The starting range value of the output image. */
    private double stRange2 = Double.NaN;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Creates a new AlgorithmChangeType object.
     *
     * @param  destImg  image model where result image is to stored
     * @param  srcImg   source image model
     * @param  stRge1   starting value of intensities of the source image
     * @param  endRge1  ending value of intensities of the source image
     * @param  stRge2   starting value of intensities of the destination image
     * @param  endRge2  ending value of intensities of the destination image
     * @param  img25D   DOCUMENT ME!
     */
    public AlgorithmChangeType(ModelImage destImg, ModelImage srcImg, double stRge1, double endRge1, double stRge2,
                               double endRge2, boolean img25D) {

        super(destImg, srcImg);
        stRange1 = stRge1;
        endRange1 = endRge1;
        stRange2 = stRge2;
        endRange2 = endRge2;
        image25D = img25D;
    }

    /**
     * Creates a new AlgorithmChangeType object.
     *
     * @param  srcImg   source image model
     * @param  newType  new type that the source image is to become.
     * @param  stRge1   starting value of intensities of the source image
     * @param  endRge1  ending value of intensities of the source image
     * @param  stRge2   starting value of intensities of the destination image
     * @param  endRge2  ending value of intensities of the destination image
     * @param  img25D   DOCUMENT ME!
     */
    public AlgorithmChangeType(ModelImage srcImg, int newType, double stRge1, double endRge1, double stRge2,
                               double endRge2, boolean img25D) {

        super(null, srcImg);
        this.newType = DataType.getDataType(newType);
        stRange1 = stRge1;
        endRange1 = endRge1;
        stRange2 = stRge2;
        endRange2 = endRge2;
        image25D = img25D;
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
     * Start algorithm.
     */
    public void runAlgorithm() {

        if (srcImage == null) {
            displayError("Change image type: Source Image is null");

            return;
        }

        if (srcImage.isColorImage()) {
            colorFactor = 4;
        }
        
        if (srcImage.isComplexImage()) {
            complexFactor = 2;
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
    }

    /**
     * This function replaces the source image with the new type image and new data range.
     */
    private void calcInPlace2D() {

        int i;
        int length;
        double[] buffer;
        double imMin, imMax;
        double imDiff, newDiff;

        try {
            length = srcImage.getSliceSize() * colorFactor * complexFactor;
            buffer = new double[length];
            srcImage.exportData(0, length, buffer); // locks and releases lock
            fireProgressStateChanged(srcImage.getImageName(), "Changing image to new type");
        } catch (IOException error) {
            buffer = null;
            errorCleanUp("Algorithm ChangeType: Image(s) locked", true);

            return;
        } catch (OutOfMemoryError e) {
            buffer = null;
            errorCleanUp("Algorithm ChangeType:  Out of memory", true);

            return;
        }

        imMax = endRange1; // srcImage.getMax();
        imMin = stRange1; // srcImage.getMin();
        imDiff = imMax - imMin;
        newDiff = endRange2 - stRange2;


        int mod = length / 100;


        for (i = 0; (i < length) && !threadStopped; i++) {

            if (((i % mod) == 0)) {
                fireProgressStateChanged(Math.round((float) i / (length - 1) * 100));
            }
            
            if ((srcImage.getType() == ModelStorageBase.COMPLEX) || (srcImage.getType() == ModelStorageBase.DCOMPLEX)) {
                buffer[i] = buffer[i] * endRange2/endRange1;	
            }
            else {

	            if ((buffer[i] >= imMin) && (buffer[i] <= imMax)) {
	                if (imDiff != 0.0) {
	                    buffer[i] = (((buffer[i] - imMin) / imDiff) * newDiff) + stRange2;
	                }
	                else {
	                    buffer[i] = (stRange2 + endRange2)/2.0;
	                }
	            } else if (buffer[i] < imMin) {
	                buffer[i] = stRange2;
	            } else if (buffer[i] > imMax) {
	                buffer[i] = endRange2;
	            }
            }
        }

        if (threadStopped) {
            buffer = null;
            finalize();

            return;
        }

        try {

            if (srcImage.getType() != newType.getLegacyNum()) {
                srcImage.reallocate(newType.getLegacyNum());
            }

            srcImage.importData(0, buffer, true);
        } catch (IOException error) {
            buffer = null;
            errorCleanUp("Algorithm ChangeType: Image(s) locked", true);

            return;
        } catch (OutOfMemoryError e) {
            buffer = null;
            errorCleanUp("Algorithm ChangeType: Out of memory", true);

            return;
        }

        if (threadStopped) {
            buffer = null;
            finalize();

            return;
        }

        setCompleted(true);

    }

    /**
     * This function replaces the source image with the new type image and new data range.
     */
    private void calcInPlace34D() {

        int i;
        int length;
        double[] buffer;
        double imMin, imMax;
        double imDiff, newDiff;

        imMax = endRange1; // srcImage.getMax();
        imMin = stRange1; // srcImage.getMin();
        imDiff = imMax - imMin;
        newDiff = endRange2 - stRange2;

        try {
            length = srcImage.getSliceSize() * srcImage.getExtents()[2] * colorFactor * complexFactor;

            if (srcImage.getNDims() == 4) {
                length = length * srcImage.getExtents()[3];
            }

            buffer = new double[length];
            srcImage.exportData(0, length, buffer); // locks and releases lock
            fireProgressStateChanged(srcImage.getImageName(), "Changing image to new type");
        } catch (IOException error) {
            buffer = null;
            errorCleanUp("Algorithm Change type: Image(s) locked", true);

            return;
        } catch (OutOfMemoryError e) {
            buffer = null;
            errorCleanUp("Algorithm Change type: Out of memory", true);

            return;
        }


        int mod = length / 100; // mod is 1 percent of length

        if (!image25D) {

            for (i = 0; (i < length) && !threadStopped; i++) {

                if (((i % mod) == 0)) {
                    fireProgressStateChanged(Math.round((float) i / (length - 1) * 100));
                }
                
                if ((srcImage.getType() == ModelStorageBase.COMPLEX) || (srcImage.getType() == ModelStorageBase.DCOMPLEX)) {
                    buffer[i] = buffer[i] * endRange2/endRange1;	
                }
                else {

	                if ((buffer[i] >= imMin) && (buffer[i] <= imMax)) {
	                    if (imDiff != 0.0) {
	                        buffer[i] = (((buffer[i] - imMin) / imDiff) * newDiff) + stRange2;
	                    }
	                    else {
	                        buffer[i] = (stRange2 + endRange2)/2.0;
	                    }
	                } else if (buffer[i] < imMin) {
	                    buffer[i] = stRange2;
	                } else if (buffer[i] > imMax) {
	                    buffer[i] = endRange2;
	                }
                }
            }
        } // image will be processed slice independently
        else {
            System.err.println("doing 25D");

            int sliceLength = srcImage.getSliceSize() * colorFactor * complexFactor;
            int numSlices = srcImage.getExtents()[2];
            int endIndex, startIndex;

            imMax = imMin = 0;

            for (int z = 0; z < numSlices; z++) {

                // first find min and max for slice range
                imMax = imMin = buffer[z * sliceLength];
                startIndex = z * sliceLength;
                endIndex = (z + 1) * sliceLength;

                // System.err.println("checking min and max btwn: " + startIndex + " and " + endIndex);
                for (i = startIndex; i < endIndex; i++) {

                    if (buffer[i] > imMax) {
                        imMax = buffer[i];
                    } else if (buffer[i] < imMin) {
                        imMin = buffer[i];
                    }
                }

                imDiff = imMax - imMin;

                // System.err.println("Slice min: " + imMin + " max: " + imMax + " diff: " + imDiff);

                for (i = (z * sliceLength); (i < endIndex) && !threadStopped; i++) {

                    if ((i % mod) == 0) {
                        fireProgressStateChanged(Math.round((float) i / (length - 1) * 100));
                    }
                    
                    if ((srcImage.getType() == ModelStorageBase.COMPLEX) || (srcImage.getType() == ModelStorageBase.DCOMPLEX)) {
                        buffer[i] = buffer[i] * endRange2/endRange1;	
                    }
                    else {

	                    if ((buffer[i] >= imMin) && (buffer[i] <= imMax)) {
	                        if (imDiff != 0.0) {
	                            buffer[i] = (((buffer[i] - imMin) / imDiff) * newDiff) + stRange2;
	                        }
	                        else {
	                            buffer[i] = (stRange2 + endRange2)/2.0;
	                        }
	                    } else if (buffer[i] < imMin) {
	                        buffer[i] = stRange2;
	                    } else if (buffer[i] > imMax) {
	                        buffer[i] = endRange2;
	                    }
                    }
                }
            }
        }

        if (threadStopped) {
            buffer = null;
            finalize();

            return;
        }

        try {
            if (srcImage.getType() != newType.getLegacyNum()) {
                srcImage.reallocate(newType.getLegacyNum());
            }
            srcImage.importData(0, buffer, true);
        } catch (IOException error) {
            buffer = null;
            errorCleanUp("Algorithm Change type: Image(s) locked", true);

            return;
        } catch (OutOfMemoryError e) {
            buffer = null;
            errorCleanUp("Algorithm Change type: Out of memory", true);

            return;
        }

        if (threadStopped) {
            buffer = null;
            finalize();

            return;
        }

        setCompleted(true);

    }

    /**
     * This function produces a new image with the new data type and new data range.
     */
    private void calcStoreInDest2D() {

        int i;
        int length;
        double[] buffer;
        double imMin, imMax;
        double imDiff, newDiff;

        try {
            destImage.setLock(ModelStorageBase.RW_LOCKED);
        } catch (IOException error) {
            errorCleanUp("Change type: Image(s) locked", false);

            return;
        }

        try {
            length = srcImage.getSliceSize() * colorFactor * complexFactor;
            buffer = new double[length];
            srcImage.exportData(0, length, buffer); // locks and releases lock
            fireProgressStateChanged(srcImage.getImageName(), "Creating new type image");
        } catch (IOException error) {
            buffer = null;
            errorCleanUp("Algorithm ChangeType: Image(s) locked", true);

            return;
        } catch (OutOfMemoryError e) {
            buffer = null;
            errorCleanUp("Algorithm ChangeType:  Out of memory", true);

            return;
        }


        int mod = length / 100; // mod is 1 percent of length

        imMax = endRange1; // srcImage.getMax();
        imMin = stRange1; // srcImage.getMin();
        imDiff = imMax - imMin;
        newDiff = endRange2 - stRange2;

        for (i = 0; (i < length) && !threadStopped; i++) {

            if (((i % mod) == 0)) {
                fireProgressStateChanged(Math.round((float) i / (length - 1) * 100));
            }
            
            if ((srcImage.getType() == ModelStorageBase.COMPLEX) || (srcImage.getType() == ModelStorageBase.DCOMPLEX)) {
                destImage.set(i, buffer[i] * endRange2/endRange1);	
            }
            else {

	            if ((buffer[i] >= imMin) && (buffer[i] <= imMax)) {
	                if (imDiff != 0.0) {
	                    destImage.set(i, (((buffer[i] - imMin) / imDiff) * newDiff) + stRange2);
	                }
	                else {
	                    destImage.set(i,(stRange2 + endRange2)/2.0);
	                }
	            } else if (buffer[i] < imMin) {
	                destImage.set(i, stRange2);
	            } else if (buffer[i] > imMax) {
	                destImage.set(i, endRange2);
	            }
            }
        }

        if (threadStopped) {
            buffer = null;
            finalize();

            return;
        }

        destImage.calcMinMax();
        destImage.releaseLock();

        // destImage.notifyImageDisplayListeners(null, true);

        setCompleted(true);
    }

    /**
     * This function produces a new image with the new data type and new data range.
     */
    private void calcStoreInDest34D() {

        int i, j;
        int length;
        double[] buffer;
        double imMin, imMax;
        double imDiff, newDiff;

        int numSlices; // slices will be counted as zDim for 3-dim and zDim * tDim for 4-dim

        numSlices = srcImage.getExtents()[2];

        if (srcImage.getNDims() == 4) {
            numSlices *= srcImage.getExtents()[3];
        }

        try {
            destImage.setLock(ModelStorageBase.RW_LOCKED);
        } catch (IOException error) {
            errorCleanUp("Change type: Image(s) locked", false);

            return;
        }

        imMax = endRange1; // srcImage.getMax();
        imMin = stRange1; // srcImage.getMin();
        imDiff = imMax - imMin;
        newDiff = endRange2 - stRange2;

        length = srcImage.getSliceSize() * colorFactor * complexFactor;

        buffer = new double[length];

        fireProgressStateChanged(srcImage.getImageName(), "Changing new type image");

        int mod = length * numSlices / 100;


        for (i = 0; i < numSlices; i++) {

            // export one slice at a time (so we don't have to allocate a huge buffer)
            try {
                srcImage.exportData((i * length), length, buffer);
            } catch (IOException ex) {
                buffer = null;
                errorCleanUp("Algorithm Change type: Image(s) locked", true);

                return;
            }

            if (!image25D) {

                for (j = 0; (j < length) && !threadStopped; j++) {

                    if (((((i * length) + j) % mod) == 0)) {
                        fireProgressStateChanged(Math.round((float) ((i * length) + j) / ((numSlices * length) - 1) *
                                                                100));
                    }
                    
                    if ((srcImage.getType() == ModelStorageBase.COMPLEX) || (srcImage.getType() == ModelStorageBase.DCOMPLEX)) {
                        destImage.set((i * length) + j, buffer[j] * endRange2/endRange1);	
                    }
                    else {

	                    if ((buffer[j] >= imMin) && (buffer[j] <= imMax)) {
	                        if (imDiff != 0.0) {
	                            destImage.set((i * length) + j, (((buffer[j] - imMin) / imDiff) * newDiff) + stRange2);
	                        }
	                        else if(imDiff == 0 && imMin == 0){
	                            //destImage.set(index + j,(stRange2 + endRange2)/2.0);
	                        	destImage.set((i * length) + j,0);
	                        }else if(imDiff == 0 && imMin != 0) {
	                        	destImage.set((i * length) + j,(stRange2 + endRange2)/2.0);
	                        }
	                    } else if (buffer[j] < imMin) {
	                        destImage.set((i * length) + j, stRange2);
	                    } else if (buffer[j] > imMax) {
	                        destImage.set((i * length) + j, endRange2);
	                    }
                    }
                }
            } // image will be processed slice independently
            else {
                int index = length * i;
                imMax = imMin = 0;

                // find the new min and max
                for (j = 0; j < length; j++) {

                    if (buffer[j] > imMax) {
                        imMax = buffer[j];
                    } else if (buffer[j] < imMin) {
                        imMin = buffer[j];
                    }
                }

                imDiff = imMax - imMin;

                for (j = 0; j < length; j++) {

                    if (((((i * length) + j) % mod) == 0)) {
                        fireProgressStateChanged(Math.round((float) ((i * length) + j) / ((numSlices * length) - 1) *
                                                                100));
                    }
                    
                    if ((srcImage.getType() == ModelStorageBase.COMPLEX) || (srcImage.getType() == ModelStorageBase.DCOMPLEX)) {
                        destImage.set(index + j, buffer[j] * endRange2/endRange1);	
                    }
                    else {

	                    if ((buffer[j] >= imMin) && (buffer[j] <= imMax)) {
	                        if (imDiff != 0.0) {
	                            destImage.set(index + j, (((buffer[j] - imMin) / imDiff) * newDiff) + stRange2);
	                        }
	                        else if(imDiff == 0 && imMin == 0){
	                            //destImage.set(index + j,(stRange2 + endRange2)/2.0);
	                        	destImage.set(index + j,0);
	                        }else if(imDiff == 0 && imMin != 0) {
	                        	destImage.set(index + j,(stRange2 + endRange2)/2.0);
	                        }
	                    } else if (buffer[j] < imMin) {
	                        destImage.set(index + j, stRange2);
	                    } else if (buffer[j] > imMax) {
	                        destImage.set(index + j, endRange2);
	                    } 
                    }
                }
            }

        }

        if (threadStopped) {
            buffer = null;
            finalize();

            return;
        }

        destImage.calcMinMax();
        destImage.releaseLock();

        // destImage.notifyImageDisplayListeners(null, true);

        setCompleted(true);
    }
  
}
