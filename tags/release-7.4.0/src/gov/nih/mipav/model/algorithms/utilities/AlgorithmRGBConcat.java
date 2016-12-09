package gov.nih.mipav.model.algorithms.utilities;


import gov.nih.mipav.model.algorithms.*;
import gov.nih.mipav.model.file.*;
import gov.nih.mipav.model.structures.*;
import gov.nih.mipav.model.structures.ModelStorageBase.DataType;

import java.io.*;


/**
 * Simple algorithm that generates an RGB image from three gray images.
 *
 * @version  1.0 Dec 30, 1999
 * @author   Matthew J. McAuliffe, Ph.D.
 */
public class AlgorithmRGBConcat extends AlgorithmBase {

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** Destination image where results are to be stored. Must be of type RGB. */
    private ModelImage destImage;

    /**
     * Flag indicating whether of not to remap the data. If true and srcImage data max is < remapHighestValue
     *  data will be remapped [0-remapHighestValue] else if image max > remapHighestValue data will
     *  automatically be remapped [0-remapHighestValue].
     */
    private boolean reMap = false;
    
    /** If true, map all colors based on image min and max.  If false, map each color separately based on its own min
     *  and max.
     */
    public boolean commonMapping = true;
    
    private float remapHighestValue = 255.0f;

    /** Source gray scale image to be stored in the BLUE channel. */
    private ModelImage srcImageB;

    /** Source gray scale image to be stored in the GREEN channel. */
    private ModelImage srcImageG;

    /** Source gray scale image to be stored in the RED channel. */
    private ModelImage srcImageR;
    
    /** flag for performing bounds checking...normally should be set to true unless negative numbers are desired**/
    private boolean performBoundsChecking;
    
    /** ARGB, ARGB_USHORT, or ARGB_FLOAT */
    private DataType dataType;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Creates a new AlgorithmRGBConcat object.
     *
     * @param  srcImgR  image model where result image of the Red channel is to be stored
     * @param  srcImgG  image model where result image of the Green channel is to be stored
     * @param  srcImgB  image model where result image of the Blue channel is to be stored
     * @param  dataType Tells if srcImage will become ARGB, ARGB_USHORT, or ARGB_FLOAT
     * @param  remap    if true and srcImage data max is < remapHighestValue data will be remapped [0-remapHighestValue]
     *                  else if image max > remapHighestValue data will automatically be remapped [0-remapHighestValue].
     * @param  commonMapping
     * @param  remapHighestValue The highest value that will occur if remapping occurs
     * @param performBoundsChecking  flag for performing bounds checking...normally should be set to true unless negative numbers are desired             
     */
    public AlgorithmRGBConcat(ModelImage srcImgR, ModelImage srcImgG, ModelImage srcImgB, int dataType, boolean remap, 
                              boolean commonMapping, float remapHighestValue, boolean performBoundsChecking, boolean copyAllInfo) {

        srcImageR = srcImgR; // Put results in red   destination image.
        srcImageG = srcImgG; // Put results in green destination image.
        srcImageB = srcImgB; // Put results in blue  destination image.
        destImage = null;
        this.dataType = DataType.getDataType(dataType);
        reMap = remap;
        this.commonMapping = commonMapping;
        this.remapHighestValue = remapHighestValue;
        this.performBoundsChecking = performBoundsChecking;
    }

    /**
     * Creates a new AlgorithmRGBConcat object.
     *
     * @param  srcImgR  image model where result image of the Red channel is to be stored
     * @param  srcImgG  image model where result image of the Green channel is to be stored
     * @param  srcImgB  image model where result image of the Blue channel is to be stored
     * @param  destImg  destination image image model
     * @param  remap    if true and srcImage data max is < remapHighestValue data will be remapped [0-remapHighestValue]
     *                  else if image max > remapHighestValue data will automatically be remapped [0-remapHighestValue].
     * @param  commonMapping
     * @param  remapHighestValue The highest value that will occur if remapping occurs
     * * @param performBoundsChecking  flag for performing bounds checking...normally should be set to true unless negative numbers are desired 
     * @param copyAllInfo 
     */
    public AlgorithmRGBConcat(ModelImage srcImgR, ModelImage srcImgG, ModelImage srcImgB, ModelImage destImg,
                              boolean remap, boolean commonMapping, float remapHighestValue, boolean performBoundsChecking, boolean copyAllInfo) {

        srcImageR = srcImgR; // Put results in red   destination image.
        srcImageG = srcImgG; // Put results in green destination image.
        srcImageB = srcImgB; // Put results in blue  destination image.
        destImage = destImg;
        this.dataType = destImage.getDataType();
        reMap = remap;
        this.commonMapping = commonMapping;
        this.remapHighestValue = remapHighestValue;
        this.performBoundsChecking = performBoundsChecking;
    }
    
    /**
     * Creates a new AlgorithmRGBConcat object.
     *
     * @param  srcImgR  image model where result image of the Red channel is to be stored
     * @param  srcImgG  image model where result image of the Green channel is to be stored
     * @param  srcImgB  image model where result image of the Blue channel is to be stored
     * @param  destImg  destination image image model
     * @param  remap    if true and srcImage data max is < remapHighestValue data will be remapped [0-remapHighestValue]
     *                  else if image max > remapHighestValue data will automatically be remapped [0-remapHighestValue].
     * @param  commonMapping
     * @param  remapHighestValue The highest value that will occur if remapping occurs
     * * @param performBoundsChecking  flag for performing bounds checking...normally should be set to true unless negative numbers are desired 
     */
    public AlgorithmRGBConcat(ModelImage srcImgR, ModelImage srcImgG, ModelImage srcImgB, ModelImage destImg,
                              boolean remap, boolean commonMapping, float remapHighestValue, boolean performBoundsChecking) {
        this(srcImgR, srcImgG, srcImgB, destImg, remap, commonMapping, remapHighestValue, performBoundsChecking, true);
    }
    
    /**
     * Creates a new AlgorithmRGBConcat object.
     *
     * @param  srcImgR  image model where result image of the Red channel is to be stored
     * @param  srcImgG  image model where result image of the Green channel is to be stored
     * @param  srcImgB  image model where result image of the Blue channel is to be stored
     * @param  dataType Tells if srcImage will become ARGB, ARGB_USHORT, or ARGB_FLOAT
     * @param  remap    if true and srcImage data max is < remapHighestValue data will be remapped [0-remapHighestValue]
     *                  else if image max > remapHighestValue data will automatically be remapped [0-remapHighestValue].
     * @param  commonMapping
     * @param  remapHighestValue The highest value that will occur if remapping occurs
     * @param performBoundsChecking  flag for performing bounds checking...normally should be set to true unless negative numbers are desired             
     */
    public AlgorithmRGBConcat(ModelImage srcImgR, ModelImage srcImgG, ModelImage srcImgB, int dataType, boolean remap, 
            boolean commonMapping, float remapHighestValue, boolean performBoundsChecking) {
        this(srcImgR, srcImgG, srcImgB, dataType, remap, 
            commonMapping, remapHighestValue, performBoundsChecking, true);
    }

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * Prepares this class for destruction.
     */
    public void finalize() {
        srcImageR = null;
        srcImageG = null;
        srcImageB = null;
        destImage = null;
        super.finalize();
    }

    /**
     * Accessor to get imageR.
     *
     * @return  DOCUMENT ME!
     */
    public ModelImage getImageR() {
        return srcImageR;
    }

    /**
     * Starts the program.
     */
    public void runAlgorithm() {

        if ((srcImageR == null) || (srcImageG == null) || (srcImageB == null)) {
            displayError("RGBConcat.run(): Source image is null");

            return;
        }

        if (srcImageR.isColorImage() || srcImageG.isColorImage() || srcImageB.isColorImage()) {
            displayError("RGBConcat.run(): Source image(s) cannot be of RGB type");

            return;
        }

        if ((destImage != null) && (destImage.isColorImage() == false)) {
            displayError("RGBConcat.run(): Destination image must be of RGB type");

            return;
        }

        

        if (destImage != null) {
            calcStoreInDest();
        } else {
            calcStoreInPlace();
        }
    }

    /**
     * Concatenate the image and store the results in the destination image.
     */
    private void calcStoreInDest() {

        int i, j;
        int id;

        int length, srcLength; // total number of data-elements (pixels) in image
        float[] buffer; // data-buffer (for pixel data) which is the "heart" of the image\
        float[] bufferR;
        float[] bufferG;
        float[] bufferB;
        float minR = (float) srcImageR.getMin();
        float maxR = (float) srcImageR.getMax();
        float minG = (float) srcImageG.getMin();
        float maxG = (float) srcImageG.getMax();
        float minB = (float) srcImageB.getMin();
        float maxB = (float) srcImageB.getMax();
        float min = Math.min(minR, Math.min(minG, minB));
        float max = Math.min(maxR, Math.max(maxG, maxB));

        int nImages = 1;
        float upperLimit = dataType.getTypeMax().floatValue();

        try {
            length = 4 * destImage.getSliceSize();
            srcLength = destImage.getSliceSize();
            buffer = new float[length];
            bufferR = new float[srcLength];
            bufferG = new float[srcLength];
            bufferB = new float[srcLength];
            fireProgressStateChanged(destImage.getImageName(), "Concatinating gray images ...");
        } catch (OutOfMemoryError e) {
            buffer = null;
            bufferR = null;
            bufferG = null;
            bufferB = null;
            System.gc();
            displayError("Algorithm RGBConcat reports: Out of memory when creating image buffer");
            setCompleted(false);
            notifyListeners(this);

            return;
        }

        

        if (srcImageR.getNDims() == 3) {
            nImages = srcImageR.getExtents()[2];
        } else if (srcImageR.getNDims() == 4) {
            nImages = srcImageR.getExtents()[2] * srcImageR.getExtents()[3];
        } else {
            nImages = 1;
        }

        int mod = (nImages * length) / 20;

        float diff = max - min;
        float diffR = maxR - minR;
        float diffG = maxG - minG;
        float diffB = maxB - minB;

        if (diff == 0) {
            diff = 1;
        }
        
        if (diffR == 0) {
            diffR = 1;
        }
        
        if (diffG == 0) {
            diffG = 1;
        }
        
        if (diffB == 0) {
            diffB = 1;
        }

        for (j = 0; (j < nImages) && !threadStopped; j++) {

            try {
                srcImageR.exportData(j * srcLength, srcLength, bufferR);
                srcImageG.exportData(j * srcLength, srcLength, bufferG);
                srcImageB.exportData(j * srcLength, srcLength, bufferB);
            } catch (IOException error) {
                displayError("Algorithm RGBConcat reports: Export image(s) locked");
                setCompleted(false);
                notifyListeners(this);

                return;
            }

            if (threadStopped) {
                buffer = null;
                bufferR = null;
                bufferG = null;
                bufferB = null;
                notifyListeners(this);
                finalize();

                return;
            }

            if (reMap == true) {

                for (i = 0, id = 0; (i < length) && !threadStopped; i += 4, id++) {

                    if (((i % mod) == 0)) {
                        fireProgressStateChanged(Math.round((float) (i + (j * length)) / ((nImages * length) - 1) * 100));
                    }

                    buffer[i] = remapHighestValue;
                    if (commonMapping) {
                        buffer[i + 1] = ((bufferR[id] - min) / (diff)) * remapHighestValue;
                        buffer[i + 2] = ((bufferG[id] - min) / (diff)) * remapHighestValue;
                        buffer[i + 3] = ((bufferB[id] - min) / (diff)) * remapHighestValue;
                    }
                    else {
                        buffer[i + 1] = ((bufferR[id] - minR) / (diffR)) * remapHighestValue;
                        buffer[i + 2] = ((bufferG[id] - minG) / (diffG)) * remapHighestValue;
                        buffer[i + 3] = ((bufferB[id] - minB) / (diffB)) * remapHighestValue;    
                    }
                }

                if (threadStopped) {
                    buffer = null;
                    bufferR = null;
                    bufferG = null;
                    bufferB = null;
                    notifyListeners(this);
                    finalize();

                    return;
                }

            } else {

                for (i = 0, id = 0; (i < length) && !threadStopped; i += 4, id++) {

                    if (((i % mod) == 0)) {
                        fireProgressStateChanged(Math.round((float) (i + (j * length)) / ((nImages * length) - 1) * 100));
                    }

                    buffer[i] = upperLimit;
                    if(performBoundsChecking) {
	                    if (bufferR[id] < 0) {
	                        buffer[i + 1] = 0;
	                    } else if (bufferR[id] > upperLimit) {
	                        buffer[i + 1] = upperLimit;
	                    } else {
	                        buffer[i + 1] = bufferR[id];
	                    }
	
	                    if (bufferG[id] < 0) {
	                        buffer[i + 2] = 0;
	                    } else if (bufferG[id] > upperLimit) {
	                        buffer[i + 2] = upperLimit;
	                    } else {
	                        buffer[i + 2] = bufferG[id];
	                    }
	
	                    if (bufferB[id] < 0) {
	                        buffer[i + 3] = 0;
	                    } else if (bufferB[id] > upperLimit) {
	                        buffer[i + 3] = upperLimit;
	                    } else {
	                        buffer[i + 3] = bufferB[id];
	                    }
                    }
                    else {
                    	buffer[i + 1] = bufferR[id];
                    	buffer[i + 2] = bufferG[id];
                    	buffer[i + 3] = bufferB[id];
                    }
                }

                if (threadStopped) {
                    buffer = null;
                    bufferR = null;
                    bufferG = null;
                    bufferB = null;
                    notifyListeners(this);
                    finalize();

                    return;
                }
            }

            if (threadStopped) {
                buffer = null;
                bufferR = null;
                bufferG = null;
                bufferB = null;
                notifyListeners(this);
                finalize();

                return;
            }

            try {
                destImage.importData(j * length, buffer, false);
            } catch (IOException error) {
                displayError("Algorithm RGBConcat: Import image(s): " + error);
                setCompleted(false);
                
                notifyListeners(this);

                return;
            }
        }

        buffer = null;
        bufferR = null;
        bufferG = null;
        bufferB = null;

        if (threadStopped) {
            buffer = null;
            bufferR = null;
            bufferG = null;
            bufferB = null;
            notifyListeners(this);
            finalize();

            return;
        }

        destImage.calcMinMax();
        
        setCompleted(true);
    }

    /**
     * Concatenate the image Must run getImageR after running this routine.
     */
    private void calcStoreInPlace() {

        int i, n;
        int id;

        int length, srcLength, totSrcLength; // total number of data-elements (pixels) in image
        int totLength;
        float[] buffer; // data-buffer (for pixel data) which is the "heart" of the image\
        float[] bufferR;
        float[] bufferG;
        float[] bufferB;
        float minR = (float) srcImageR.getMin();
        float maxR = (float) srcImageR.getMax();
        float minG = (float) srcImageG.getMin();
        float maxG = (float) srcImageG.getMax();
        float minB = (float) srcImageB.getMin();
        float maxB = (float) srcImageB.getMax();
        float min = Math.min(minR, Math.min(minG, minB));
        float max = Math.min(maxR, Math.max(maxG, maxB));
        int[] extents;
        String imageName;
        FileInfoBase[] fInfoBase = null;
        float upperLimit = dataType.getTypeMax().floatValue();

        int nImages = 1;

        fireProgressStateChanged(srcImageR.getImageName(), "Concatinating gray images ...");

        extents = srcImageR.getExtents();
        imageName = srcImageR.getImageName();

        srcLength = srcImageR.getSliceSize();
        length = 4 * srcLength;

        if (srcImageR.getNDims() == 3) {
            nImages = extents[2];
        } else if (srcImageR.getNDims() == 4) {
            nImages = extents[2] * extents[3];
        } else {
            nImages = 1;
        }

        totSrcLength = nImages * srcLength;
        totLength = nImages * length;

        int mod = (nImages * length) / 20;

        try {
            bufferR = new float[totSrcLength];

            if (srcImageR.getImageName() != srcImageG.getImageName()) {
                bufferG = new float[totSrcLength];
            } else {
                bufferG = bufferR;
            }

            if ((srcImageB.getImageName() != srcImageR.getImageName()) &&
                    (srcImageB.getImageName() != srcImageG.getImageName())) {
                bufferB = new float[totSrcLength];
            } else if (srcImageB.getImageName() == srcImageR.getImageName()) {
                bufferB = bufferR;
            } else {
                bufferB = bufferG;
            }
        } catch (OutOfMemoryError e) {
            bufferR = null;
            bufferG = null;
            bufferB = null;
            System.gc();
            displayError("Algorithm RGBConcat reports: Out of memory when creating image buffer");
            setCompleted(false);
            notifyListeners(this);

            return;
        }

        

        float diff = max - min;
        float diffR = maxR - minR;
        float diffG = maxG - minG;
        float diffB = maxB - minB;

        if (diff == 0) {
            diff = 1;
        }
        
        if (diffR == 0) {
            diffR = 1;
        }
        
        if (diffG == 0) {
            diffG = 1;
        }
        
        if (diffB == 0) {
            diffB = 1;
        }

        try {
            srcImageR.exportData(0, totSrcLength, bufferR);
            srcImageG.exportData(0, totSrcLength, bufferG);
            srcImageB.exportData(0, totSrcLength, bufferB);
        } catch (IOException error) {
            displayError("Algorithm RGBConcat reports: Export image(s) locked");
            setCompleted(false);
            notifyListeners(this);

            return;
        }

        fInfoBase = new FileInfoBase[nImages];

        for (n = 0; n < srcImageR.getFileInfo().length; n++) {
            fInfoBase[n] = (FileInfoBase) (srcImageR.getFileInfo(n).clone());
            fInfoBase[n].setDataType(ModelStorageBase.ARGB);
        }

        if (srcImageR.getParentFrame() != null) {
            srcImageR.getParentFrame().close();
        }

        srcImageR.disposeLocal();
        srcImageR = null;

        try {
            buffer = new float[totLength];
        } catch (OutOfMemoryError e) {
            buffer = null;
            bufferR = null;
            bufferG = null;
            bufferB = null;
            System.gc();
            displayError("Algorithm RGBConcat reports: Out of memory when creating image buffer");
            setCompleted(false);
            notifyListeners(this);

            return;
        }


        if (threadStopped) {
            buffer = null;
            bufferR = null;
            bufferG = null;
            bufferB = null;
            notifyListeners(this);
            finalize();

            return;
        }

        if (reMap == true) {

            for (i = 0, id = 0; (i < totLength) && !threadStopped; i += 4, id++) {

                if (((i % mod) == 0)) {
                    fireProgressStateChanged(Math.round((float) (i) / (totLength - 1) * 100));
                }

                buffer[i] = remapHighestValue;
                if (commonMapping) {
                    buffer[i + 1] = ((bufferR[id] - min) / (diff)) * remapHighestValue;
                    buffer[i + 2] = ((bufferG[id] - min) / (diff)) * remapHighestValue;
                    buffer[i + 3] = ((bufferB[id] - min) / (diff)) * remapHighestValue;
                }
                else {
                    buffer[i + 1] = ((bufferR[id] - minR) / (diffR)) * remapHighestValue;
                    buffer[i + 2] = ((bufferG[id] - minG) / (diffG)) * remapHighestValue;
                    buffer[i + 3] = ((bufferB[id] - minB) / (diffB)) * remapHighestValue;    
                }
            }

            if (threadStopped) {
                buffer = null;
                bufferR = null;
                bufferG = null;
                bufferB = null;
                notifyListeners(this);
                finalize();

                return;
            }

        } else {

            for (i = 0, id = 0; (i < totLength) && !threadStopped; i += 4, id++) {

                if (((i % mod) == 0)) {
                    fireProgressStateChanged(Math.round((float) (i) / (totLength - 1) * 100));
                }

                buffer[i] = upperLimit;
                if(performBoundsChecking) {
	                if (bufferR[id] < 0) {
	                    buffer[i + 1] = 0;
	                } else if (bufferR[id] > upperLimit) {
	                    buffer[i + 1] = upperLimit;
	                } else {
	                    buffer[i + 1] = bufferR[id];
	                }
	
	                if (bufferG[id] < 0) {
	                    buffer[i + 2] = 0;
	                } else if (bufferG[id] > upperLimit) {
	                    buffer[i + 2] = upperLimit;
	                } else {
	                    buffer[i + 2] = bufferG[id];
	                }
	
	                if (bufferB[id] < 0) {
	                    buffer[i + 3] = 0;
	                } else if (bufferB[id] > upperLimit) {
	                    buffer[i + 3] = upperLimit;
	                } else {
	                    buffer[i + 3] = bufferB[id];
	                }
                }
                else {
                	buffer[i + 1] = bufferR[id];
                	buffer[i + 2] = bufferG[id];
                	buffer[i + 3] = bufferB[id];
                }
            }

            if (threadStopped) {
                buffer = null;
                bufferR = null;
                bufferG = null;
                bufferB = null;
                notifyListeners(this);
                finalize();

                return;
            }
        }

        if (threadStopped) {
            buffer = null;
            bufferR = null;
            bufferG = null;
            bufferB = null;
            notifyListeners(this);
            finalize();

            return;
        }

        bufferR = null;
        bufferG = null;
        bufferB = null;

        srcImageR = new ModelImage(dataType, extents, imageName);

        for (n = 0; n < srcImageR.getFileInfo().length; n++) {
            srcImageR.setFileInfo(fInfoBase[n], n);
        }


        try {
            srcImageR.importData(0, buffer, true);
        } catch (IOException error) {
            displayError("Algorithm RGBConcat: Import image(s): " + error);
            setCompleted(false);
            
            notifyListeners(this);

            return;
        }

        buffer = null;
        bufferR = null;
        bufferG = null;
        bufferB = null;

        if (threadStopped) {
            buffer = null;
            bufferR = null;
            bufferG = null;
            bufferB = null;
            notifyListeners(this);
            finalize();

            return;
        }

        
        setCompleted(true);
    }

}
