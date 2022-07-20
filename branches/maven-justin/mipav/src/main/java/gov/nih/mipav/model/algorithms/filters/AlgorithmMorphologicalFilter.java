package gov.nih.mipav.model.algorithms.filters;


import gov.nih.mipav.model.algorithms.*;
import gov.nih.mipav.model.structures.*;

import java.io.*;


/**
 * Performs morphological filtering on black and white images. This is a shading correction routine, that is, it
 * corrects for non-uniform illumination and non-uniform camera sensitivity. A morphological filtered pixel = the
 * original pixel - the morphological smoothed pixel + a constant to restore the original average image brightness. A
 * morphological smoothed pixel is given by the min(max(max(min(original pixel)))), where the sizes[] array contains the
 * dimensions of the region examined for the minimum and the maximum. Reference: 1.) "Shading Correction: Compensation
 * for Illumination and Sensor Inhomogeneities" by Ian T. Young, July 19, 2000.
 * www.ph.tn.tudelft.nl/People/albert/papers/YoungShading.pdf
 *
 * @version  0.1 Feb 17, 2005
 * @author   William Gandler
 */
public class AlgorithmMorphologicalFilter extends AlgorithmBase {

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /**
     * Flag, if true, indicates that the whole image should be processed. If false on process the image over the mask
     * areas.
     */
    private boolean entireImage;

    /** Kernel dimensionality. */
    private int[] sizes;

    /** DOCUMENT ME! */
    private int xDim;

    /** DOCUMENT ME! */
    private int yDim;

    /** DOCUMENT ME! */
    private int zDim;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Constructs a Morphological filter object.
     *
     * @param  srcImg    source image model
     * @param  sizes     Filter size in each dimension
     * @param  maskFlag  Flag that indicates that the Morphological filter will be calculated for the whole image if
     *                   equal to true
     * @param  img25D    Flag, if true, indicates that each slice of the 3D volume should be processed independently. 2D
     *                   images disregard this flag.
     */
    public AlgorithmMorphologicalFilter(ModelImage srcImg, int[] sizes, boolean maskFlag, boolean img25D) {
        super(null, srcImg);
        this.sizes = sizes;
        entireImage = maskFlag;
        image25D = img25D;

        if (entireImage == false) {
            mask = srcImage.generateVOIMask();
        }

    }

    /**
     * Constructs a Morphological filter object.
     *
     * @param  destImg   image model where result image is to stored
     * @param  srcImg    source image model
     * @param  sizes     Filter size in each dimension
     * @param  maskFlag  Flag that indicates that the Morphological filter will be calculated for the whole image if
     *                   equal to true
     * @param  img25D    Flag, if true, indicates that each slice of the 3D volume should be processed independently. 2D
     *                   images disregard this flag.
     */
    public AlgorithmMorphologicalFilter(ModelImage destImg, ModelImage srcImg, int[] sizes, boolean maskFlag,
                                        boolean img25D) {
        super(destImg, srcImg);
        this.sizes = sizes;
        entireImage = maskFlag;
        image25D = img25D;

        if (entireImage == false) {
            mask = srcImage.generateVOIMask();
        }

    }

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * Prepares this class for destruction.
     */
    public void finalize() {
        sizes = null;
        destImage = null;
        srcImage = null;
        super.finalize();
    }


    /**
     * Starts the program.
     */
    public void runAlgorithm() {

        if (srcImage == null) {
            displayError("Source Image is null");

            return;
        }

        

        if (destImage != null) {

            if (srcImage.getNDims() == 2) {
                calcStoreInDest2D(1);
            } else if ((srcImage.getNDims() == 3) && (image25D == false)) {
                calcStoreInDest3D();
            } else if ((srcImage.getNDims() == 3) && (image25D == true)) {
                calcStoreInDest2D(srcImage.getExtents()[2]);
            }
        } else {

            if (srcImage.getNDims() == 2) {
                calcInPlace2D(1);
            } else if ((srcImage.getNDims() == 3) && (image25D == false)) {
                calcInPlace3D();
            } else if ((srcImage.getNDims() == 3) && (image25D == true)) {
                calcInPlace2D(srcImage.getExtents()[2]);
            }
        }
    }

    /**
     * Calculates the Morphological Filter image and replaces the source image with the new image.
     *
     * @param  nImages  number of images to be blurred. If 2D image then nImage = 1, if 3D image where each image is to
     *                  processed independently then nImages equals the number of images in the volume.
     */
    private void calcInPlace2D(int nImages) {

        int i, s;
        int length;
        int start;
        float[] buffer;
        float[] resultBuffer;
        float[] resultBuffer2;
        double total = 0.0;
        long count = 0;
        float average;
        double newTotal = 0.0;
        float newAverage;
        float restoreAverage;


        xDim = srcImage.getExtents()[0];
        yDim = srcImage.getExtents()[1];

        try {
            length = srcImage.getSliceSize();
            buffer = new float[length];
            resultBuffer = new float[length];
            resultBuffer2 = new float[length];
            fireProgressStateChanged(srcImage.getImageName(), "Calculating the Morphological Filter ...");
        } catch (OutOfMemoryError e) {
            buffer = null;
            resultBuffer = null;
            errorCleanUp("Algorithm Morphological Filter exportData: Out of memory", true);

            return;
        }

        

        for (s = 0; (s < nImages) && !threadStopped; s++) {
            total = 0.0;
            count = 0;
            newTotal = 0.0;
            start = s * length;

            try {
                srcImage.exportData(start, length, buffer); // locks and releases lock
            } catch (IOException error) {
                buffer = null;
                resultBuffer = null;
                resultBuffer2 = null;
                System.gc();
                displayError("Algorithm Morphological: Image(s) locked");
                setCompleted(false);
                

                return;
            }

            for (i = 0; i < length; i++) {

                if (entireImage || mask.get(i)) {
                    resultBuffer[i] = buffer[i];
                    total += buffer[i];
                    count++;
                }
            }

            average = (float) (total / count);

            fireProgressStateChanged(Math.round((float) s / nImages * 100));

            for (i = 0; (i < length) && !threadStopped; i++) {

                if (entireImage || mask.get(i)) {
                    resultBuffer2[i] = min2DPt(i, resultBuffer);
                }
            } // for ( i = 0; i < length && !threadStopped; i++)

            fireProgressStateChanged(Math.round((float) ((4 * s) + 1) / (4 * nImages) * 100));

            for (i = 0; (i < length) && !threadStopped; i++) {

                if (entireImage || mask.get(i)) {
                    resultBuffer[i] = max2DPt(i, resultBuffer2);
                }
            } // for ( i = 0; i < length && !threadStopped; i++)

            fireProgressStateChanged(Math.round((float) ((4 * s) + 2) / (4 * nImages) * 100));

            for (i = 0; (i < length) && !threadStopped; i++) {

                if (entireImage || mask.get(i)) {
                    resultBuffer2[i] = max2DPt(i, resultBuffer);
                }
            } // for ( i = 0; i < length && !threadStopped; i++)

            fireProgressStateChanged(Math.round((float) ((4 * s) + 3) / (4 * nImages) * 100));
            
            for (i = 0; (i < length) && !threadStopped; i++) {

                if (entireImage || mask.get(i)) {
                    resultBuffer[i] = min2DPt(i, resultBuffer2);
                }
            } // for ( i = 0; i < length && !threadStopped; i++)

            for (i = 0; (i < length) && !threadStopped; i++) {

                if (entireImage || mask.get(i)) {
                    buffer[i] = buffer[i] - resultBuffer[i];
                    newTotal += buffer[i];
                }
            } // // for ( i = 0; i < length && !threadStopped; i++)

            newAverage = (float) (newTotal / count);
            restoreAverage = average - newAverage;


            for (i = 0; (i < length) && !threadStopped; i++) {

                if (entireImage || mask.get(i)) {
                    buffer[i] = buffer[i] + restoreAverage;

                    switch (srcImage.getType()) {

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
                    } // switch(srcImage.getType())
                } // if (entireImage || mask.get(i))
            } // for (i = 0; i < length && !threadStopped; i++)

            try {
                srcImage.importData(start, buffer, false);
            } catch (IOException error) {
                buffer = null;
                resultBuffer = null;
                resultBuffer2 = null;
                errorCleanUp("Algorithm Morphological Filter importData: Image(s) locked", true);

                return;
            }

        } // for (s = 0; s <  nImages && !threadStopped; s++)

        srcImage.calcMinMax();
        
        setCompleted(true);
    }

    /**
     * Calculates the Morphological Filter and replaces the source image with the new image.
     */
    private void calcInPlace3D() {

        int i;
        int length;
        float[] buffer;
        float[] resultBuffer;
        float[] resultBuffer2;
        double total = 0.0;
        long count = 0;
        float average;
        double newTotal = 0.0;
        float newAverage;
        float restoreAverage;


        xDim = srcImage.getExtents()[0];
        yDim = srcImage.getExtents()[1];
        zDim = srcImage.getExtents()[2];

        try {
            length = srcImage.getSliceSize() * srcImage.getExtents()[2];
            buffer = new float[length];
            resultBuffer = new float[length];
            resultBuffer2 = new float[length];
            srcImage.exportData(0, length, buffer); // locks and releases lock
            fireProgressStateChanged(srcImage.getImageName(), "Calculating the Morphological Filter ...");
        } catch (IOException error) {
            buffer = null;
            resultBuffer = null;
            resultBuffer2 = null;
            errorCleanUp("Algorithm Morphological Filter exportData: Image(s) locked", true);

            return;
        } catch (OutOfMemoryError e) {
            errorCleanUp("Algorithm Morphological Filter exportData: Out of memory", true);

            return;
        }

        
        total = 0.0;
        count = 0;
        newTotal = 0.0;

        try {
            srcImage.exportData(0, length, buffer); // locks and releases lock
        } catch (IOException error) {
            buffer = null;
            resultBuffer = null;
            resultBuffer2 = null;
            System.gc();
            displayError("Algorithm Morphological: Image(s) locked");
            setCompleted(false);
            

            return;
        }

        for (i = 0; i < length; i++) {

            if (entireImage || mask.get(i)) {
                resultBuffer[i] = buffer[i];
                total += buffer[i];
                count++;
            }
        }

        average = (float) (total / count);

        fireProgressStateChanged(20);

        for (i = 0; (i < length) && !threadStopped; i++) {

            if (entireImage || mask.get(i)) {
                resultBuffer2[i] = min3DPt(i, resultBuffer);
            }
        } // for ( i = 0; i < length && !threadStopped; i++)

        fireProgressStateChanged(40);

        for (i = 0; (i < length) && !threadStopped; i++) {

            if (entireImage || mask.get(i)) {
                resultBuffer[i] = max3DPt(i, resultBuffer2);
            }
        } // for ( i = 0; i < length && !threadStopped; i++)

        fireProgressStateChanged(60);

        for (i = 0; (i < length) && !threadStopped; i++) {

            if (entireImage || mask.get(i)) {
                resultBuffer2[i] = max3DPt(i, resultBuffer);
            }
        } // for ( i = 0; i < length && !threadStopped; i++)

        fireProgressStateChanged(80);

        for (i = 0; (i < length) && !threadStopped; i++) {

            if (entireImage || mask.get(i)) {
                resultBuffer[i] = min3DPt(i, resultBuffer2);
            }
        } // for ( i = 0; i < length && !threadStopped; i++)

        for (i = 0; (i < length) && !threadStopped; i++) {

            if (entireImage || mask.get(i)) {
                buffer[i] = buffer[i] - resultBuffer[i];
                newTotal += buffer[i];
            }
        } // // for ( i = 0; i < length && !threadStopped; i++)

        newAverage = (float) (newTotal / count);
        restoreAverage = average - newAverage;

        for (i = 0; (i < length) && !threadStopped; i++) {

            if (entireImage || mask.get(i)) {
                buffer[i] = buffer[i] + restoreAverage;

                switch (srcImage.getType()) {

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
                } // switch(srcImage.getType())
            } // if (entireImage || mask.get(i))
        } // for (i = 0; i < length && !threadStopped; i++)

        try {
            srcImage.importData(0, buffer, true);
        } catch (IOException error) {
            buffer = null;
            resultBuffer = null;
            resultBuffer2 = null;
            errorCleanUp("Algorithm Morphological Filter importData: Image(s) locked", true);

            return;
        }

        setCompleted(true);
        
    }

    /**
     * This function produces the Morphological Filter of input image.
     *
     * @param  nImages  number of images to be blurred. If 2D image then nImage = 1, if 3D image where each image is to
     *                  processed independently then nImages equals the number of images in the volume.
     */
    private void calcStoreInDest2D(int nImages) {
        int i, s;
        int length;
        int start;
        float[] buffer;
        float[] resultBuffer;
        float[] resultBuffer2;
        double total = 0.0;
        long count = 0;
        float average;
        double newTotal = 0.0;
        float newAverage;
        float restoreAverage;


        xDim = srcImage.getExtents()[0];
        yDim = srcImage.getExtents()[1];


        try {
            length = srcImage.getSliceSize();
            buffer = new float[length];
            resultBuffer = new float[length];
            resultBuffer2 = new float[length];
            fireProgressStateChanged(srcImage.getImageName(), "Calculating the Morphological Filter ...");
        } catch (OutOfMemoryError e) {
            buffer = null;
            resultBuffer = null;
            resultBuffer2 = null;
            errorCleanUp("Algorithm Morphological Filter exportData: Out of memory", true);

            return;
        }

        

        for (s = 0; (s < nImages) && !threadStopped; s++) {
            total = 0.0;
            count = 0;
            newTotal = 0.0;
            start = s * length;

            try {
                srcImage.exportData(start, length, buffer); // locks and releases lock
            } catch (IOException error) {
                buffer = null;
                resultBuffer = null;
                System.gc();
                displayError("Algorithm Morphological: Image(s) locked");
                setCompleted(false);
                

                return;
            }

            for (i = 0; i < length; i++) {

                if (entireImage || mask.get(i)) {
                    resultBuffer[i] = buffer[i];
                    total += buffer[i];
                    count++;
                }
            }

            average = (float) (total / count);

            fireProgressStateChanged(Math.round((float) s / nImages * 100));

            for (i = 0; (i < length) && !threadStopped; i++) {

                if (entireImage || mask.get(i)) {
                    resultBuffer2[i] = min2DPt(i, resultBuffer);
                }
            } // for ( i = 0; i < length && !threadStopped; i++)

            fireProgressStateChanged(Math.round((float) ((4 * s) + 1) / (4 * nImages) * 100));

            for (i = 0; (i < length) && !threadStopped; i++) {

                if (entireImage || mask.get(i)) {
                    resultBuffer[i] = max2DPt(i, resultBuffer2);
                }
            } // for ( i = 0; i < length && !threadStopped; i++)

            fireProgressStateChanged(Math.round((float) ((4 * s) + 2) / (4 * nImages) * 100));

            for (i = 0; (i < length) && !threadStopped; i++) {

                if (entireImage || mask.get(i)) {
                    resultBuffer2[i] = max2DPt(i, resultBuffer);
                }
            } // for ( i = 0; i < length && !threadStopped; i++)

            fireProgressStateChanged(Math.round((float) ((4 * s) + 3) / (4 * nImages) * 100));

            for (i = 0; (i < length) && !threadStopped; i++) {

                if (entireImage || mask.get(i)) {
                    resultBuffer[i] = min2DPt(i, resultBuffer2);
                }
            } // for ( i = 0; i < length && !threadStopped; i++)

            for (i = 0; (i < length) && !threadStopped; i++) {

                if (entireImage || mask.get(i)) {
                    buffer[i] = buffer[i] - resultBuffer[i];
                    newTotal += buffer[i];
                }
            } // // for ( i = 0; i < length && !threadStopped; i++)

            newAverage = (float) (newTotal / count);
            restoreAverage = average - newAverage;


            for (i = 0; (i < length) && !threadStopped; i++) {

                if (entireImage || mask.get(i)) {
                    buffer[i] = buffer[i] + restoreAverage;

                    switch (destImage.getType()) {

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
                    } // switch(destImage.getType())
                } // if (entireImage || mask.get(i))
            } // for (i = 0; i < length && !threadStopped; i++)

            try {
                destImage.importData(start, buffer, false);
            } catch (IOException error) {
                buffer = null;
                resultBuffer = null;
                resultBuffer2 = null;
                errorCleanUp("Algorithm Morphological Filter importData: Image(s) locked", true);

                return;
            }


        } // for (s = 0; s <  nImages && !threadStopped; s++)

        destImage.calcMinMax();
        
        setCompleted(true);
    }


    /**
     * This function produces the Morphological Filter of input image.
     */
    private void calcStoreInDest3D() {

        int i;
        int length;
        float[] buffer;
        float[] resultBuffer;
        float[] resultBuffer2;
        double total = 0.0;
        long count = 0;
        float average;
        double newTotal = 0.0;
        float newAverage;
        float restoreAverage;


        xDim = srcImage.getExtents()[0];
        yDim = srcImage.getExtents()[1];
        zDim = srcImage.getExtents()[2];

        try {
            destImage.setLock();
        } catch (IOException error) {
            displayError("Algorithm Morphological Filter: Image(s) locked");
            setCompleted(false);
            destImage.releaseLock();

            return;
        }

        try {
            length = srcImage.getSliceSize() * srcImage.getExtents()[2];
            buffer = new float[length];
            resultBuffer = new float[length];
            resultBuffer2 = new float[length];
            srcImage.exportData(0, length, buffer); // locks and releases lock
            fireProgressStateChanged(srcImage.getImageName(), "Calculating the Morphological Filter ...");
        } catch (IOException error) {
            buffer = null;
            resultBuffer = null;
            resultBuffer2 = null;
            errorCleanUp("Algorithm Morphological Filter exportData: Image(s) locked", true);

            return;
        } catch (OutOfMemoryError e) {
            buffer = null;
            errorCleanUp("Algorithm Morphological Filter exportData: Out of memory", true);

            return;
        }

        
        total = 0.0;
        count = 0;
        newTotal = 0.0;

        try {
            srcImage.exportData(0, length, buffer); // locks and releases lock
        } catch (IOException error) {
            buffer = null;
            resultBuffer = null;
            resultBuffer2 = null;
            System.gc();
            displayError("Algorithm Morphological: Image(s) locked");
            setCompleted(false);
            

            return;
        }

        for (i = 0; i < length; i++) {

            if (entireImage || mask.get(i)) {
                resultBuffer[i] = buffer[i];
                total += buffer[i];
                count++;
            }
        }

        average = (float) (total / count);

        fireProgressStateChanged(20);

        for (i = 0; (i < length) && !threadStopped; i++) {

            if (entireImage || mask.get(i)) {
                resultBuffer2[i] = min3DPt(i, resultBuffer);
            }
        } // for ( i = 0; i < length && !threadStopped; i++)

        fireProgressStateChanged(40);

        for (i = 0; (i < length) && !threadStopped; i++) {

            if (entireImage || mask.get(i)) {
                resultBuffer[i] = max3DPt(i, resultBuffer2);
            }
        } // for ( i = 0; i < length && !threadStopped; i++)

        fireProgressStateChanged(60);

        for (i = 0; (i < length) && !threadStopped; i++) {

            if (entireImage || mask.get(i)) {
                resultBuffer2[i] = max3DPt(i, resultBuffer);
            }
        } // for ( i = 0; i < length && !threadStopped; i++)

        fireProgressStateChanged(80);

        for (i = 0; (i < length) && !threadStopped; i++) {

            if (entireImage || mask.get(i)) {
                resultBuffer[i] = min3DPt(i, resultBuffer2);
            }
        } // for ( i = 0; i < length && !threadStopped; i++)

        for (i = 0; (i < length) && !threadStopped; i++) {

            if (entireImage || mask.get(i)) {
                buffer[i] = buffer[i] - resultBuffer[i];
                newTotal += buffer[i];
            }
        } // // for ( i = 0; i < length && !threadStopped; i++)

        newAverage = (float) (newTotal / count);
        restoreAverage = average - newAverage;

        for (i = 0; (i < length) && !threadStopped; i++) {

            if (entireImage || mask.get(i)) {
                buffer[i] = buffer[i] + restoreAverage;

                switch (destImage.getType()) {

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
                } // switch(destImage.getType())
            } // if (entireImage || mask.get(i))
        } // for (i = 0; i < length && !threadStopped; i++)

        destImage.releaseLock();

        try {
            destImage.importData(0, buffer, true);
        } catch (IOException error) {
            buffer = null;
            resultBuffer = null;
            resultBuffer2 = null;
            errorCleanUp("Algorithm Morphological Filter importData: Image(s) locked", true);

            return;
        }

        
        setCompleted(true);
    }

    /**
     * A function that finds the maximum value in a local image rectangle.
     *
     * @param   pix    index indicating location of convolution
     * @param   image  image data
     *
     * @return  DOCUMENT ME!
     */
    private float max2DPt(int pix, float[] image) {

        int i, j;
        int offsetX, offsetY;
        int xKDim = sizes[0];
        int yKDim = sizes[1];
        int yLimit = xDim * yDim;

        int startX, startY;
        int endX, endY;
        float max = -Float.MAX_VALUE;

        offsetX = (pix % xDim) - (xKDim / 2);
        offsetY = (pix / xDim) - (yKDim / 2);
        startY = offsetY * xDim;
        endY = startY + (yKDim * xDim);

        for (j = startY; j < endY; j += xDim) {
            startX = j + offsetX;
            endX = startX + xKDim;

            for (i = startX; i < endX; i++) {

                if ((j >= 0) && (j < yLimit) && ((i - j) >= 0) && ((i - j) < xDim)) {

                    if (image[i] > max) {
                        max = image[i];
                    }
                }
            }
        }

        return max;
    }

    /**
     * A function that finds the maximum value in a local image volume.
     *
     * @param   pix    index indicating location of convolution
     * @param   image  image data
     *
     * @return  DOCUMENT ME!
     */
    private float max3DPt(int pix, float[] image) {

        int i, j, k;
        int offsetX, offsetY, offsetZ;
        int sliceSize = xDim * yDim;
        int volSize = sliceSize * zDim;
        int xKDim = sizes[0];
        int indexY;
        int stepY, stepZ;
        int startX, startY, startZ;
        int endX, endY, endZ;
        float max = -Float.MAX_VALUE;

        offsetX = (pix % xDim) - (sizes[0] / 2);
        offsetY = ((pix % sliceSize) / xDim) - (sizes[1] / 2);
        offsetZ = (pix / (sliceSize)) - (sizes[2] / 2);

        indexY = offsetY * xDim;
        stepY = sizes[1] * xDim;
        stepZ = sizes[2] * sliceSize;
        startZ = offsetZ * sliceSize;
        endZ = startZ + stepZ;

        for (k = startZ; k < endZ; k += sliceSize) {

            if ((k >= 0) && (k < volSize)) {
                startY = k + indexY;
                endY = startY + stepY;

                for (j = startY; j < endY; j += xDim) {

                    if (((j - k) >= 0) && ((j - k) < sliceSize)) {
                        startX = j + offsetX;
                        endX = startX + xKDim;

                        for (i = startX; i < endX; i++) {

                            if (((i - j) >= 0) && ((i - j) < xDim)) {

                                if (image[i] > max) {
                                    max = image[i];
                                }
                            }
                        }
                    }
                }
            }
        }

        return max;
    }

    /**
     * A function that finds the minimum value in a local image rectangle.
     *
     * @param   pix    index indicating location of convolution
     * @param   image  image data
     *
     * @return  DOCUMENT ME!
     */
    private float min2DPt(int pix, float[] image) {

        int i, j;
        int offsetX, offsetY;
        int xKDim = sizes[0];
        int yKDim = sizes[1];
        int yLimit = xDim * yDim;

        int startX, startY;
        int endX, endY;
        float min = Float.MAX_VALUE;

        offsetX = (pix % xDim) - (xKDim / 2);
        offsetY = (pix / xDim) - (yKDim / 2);
        startY = offsetY * xDim;
        endY = startY + (yKDim * xDim);

        for (j = startY; j < endY; j += xDim) {
            startX = j + offsetX;
            endX = startX + xKDim;

            for (i = startX; i < endX; i++) {

                if ((j >= 0) && (j < yLimit) && ((i - j) >= 0) && ((i - j) < xDim)) {

                    if (image[i] < min) {
                        min = image[i];
                    }
                }
            }
        }

        return min;
    }

    /**
     * A function that finds the minimum value in a local image volume.
     *
     * @param   pix    index indicating location of convolution
     * @param   image  image data
     *
     * @return  DOCUMENT ME!
     */
    private float min3DPt(int pix, float[] image) {

        int i, j, k;
        int offsetX, offsetY, offsetZ;
        int sliceSize = xDim * yDim;
        int volSize = sliceSize * zDim;
        int xKDim = sizes[0];
        int indexY;
        int stepY, stepZ;
        int startX, startY, startZ;
        int endX, endY, endZ;
        float min = Float.MAX_VALUE;

        offsetX = (pix % xDim) - (sizes[0] / 2);
        offsetY = ((pix % sliceSize) / xDim) - (sizes[1] / 2);
        offsetZ = (pix / (sliceSize)) - (sizes[2] / 2);

        indexY = offsetY * xDim;
        stepY = sizes[1] * xDim;
        stepZ = sizes[2] * sliceSize;
        startZ = offsetZ * sliceSize;
        endZ = startZ + stepZ;

        for (k = startZ; k < endZ; k += sliceSize) {

            if ((k >= 0) && (k < volSize)) {
                startY = k + indexY;
                endY = startY + stepY;

                for (j = startY; j < endY; j += xDim) {

                    if (((j - k) >= 0) && ((j - k) < sliceSize)) {
                        startX = j + offsetX;
                        endX = startX + xKDim;

                        for (i = startX; i < endX; i++) {

                            if (((i - j) >= 0) && ((i - j) < xDim)) {

                                if (image[i] < min) {
                                    min = image[i];
                                }
                            }
                        }
                    }
                }
            }
        }

        return min;
    }


}
