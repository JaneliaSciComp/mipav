import gov.nih.mipav.model.algorithms.AlgorithmBSmooth;
import gov.nih.mipav.model.algorithms.AlgorithmBase;
import gov.nih.mipav.model.algorithms.AlgorithmMorphology3D;
import gov.nih.mipav.model.algorithms.AlgorithmSnake;
import gov.nih.mipav.model.algorithms.AlgorithmThresholdDual;
import gov.nih.mipav.model.file.FileInfoBase;
import gov.nih.mipav.model.file.FileVOI;
import gov.nih.mipav.model.structures.ModelImage;
import gov.nih.mipav.model.structures.ModelStorageBase;
import gov.nih.mipav.model.structures.VOI;
import gov.nih.mipav.model.structures.VOIVector;

import gov.nih.mipav.view.MipavUtil;
import gov.nih.mipav.view.ViewJFrameImage;
import gov.nih.mipav.view.ViewVOIVector;

import java.io.IOException;



public class PlugInAlgorithmNewGeneric2 extends AlgorithmBase {
    
    /** X dimension of the CT image */
    private int xDim;

    /** Y dimension of the CT image */
    private int yDim;

    /** Z dimension of the CT image */
    private int zDim;

    /** Slice size for xDim*yDim */
    private int sliceSize;

    /** Volume size for xDim*yDim*zDim */
    private int volumeSize;
    
    /**
     * Constructor.
     *
     * @param  resultImage  Result image model
     * @param  srcImg       Source image model.
     */
    public PlugInAlgorithmNewGeneric2(ModelImage resultImage, ModelImage srcImg) {
        super(resultImage, srcImg);
    }
    
    /**
     * Starts the algorithm.
     */
    public void runAlgorithm() {
        // Algorithm to determine the outer thigh boundary and boundaries of the bone
        
        if (srcImage.getNDims() == 2) {
            calc2D();
        } else if (srcImage.getNDims() == 3) {
            calc3D();                
        }
 
    } // end runAlgorithm()
    
    

//  ~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * Prepares this class for destruction.
     */
    public void finalize() {
        destImage = null;
        srcImage = null;
        super.finalize();
    }
    
    private void calc2D() {
        MipavUtil.displayError("Error plugin only works for 3D images");
    }
    
    private void calc3D() {
        xDim = srcImage.getExtents()[0];
        yDim = srcImage.getExtents()[1];
        zDim = srcImage.getExtents()[2];

        sliceSize = xDim * yDim;
        volumeSize = sliceSize * zDim;

       ModelImage boneImage = extractBone(srcImage);
        ShowImage(boneImage, "CT Bone");
        
        ModelImage boneMarrowImage = extractBoneMarrow(srcImage, boneImage);
        ShowImage(boneMarrowImage, "CT Bone Marrow");
        
    } // end calc3D()
    
    
    // Bone marrow in CT images is "inside" the bone
    // determine an equation of a circle that describes the bone (some Hough transform)
    // check to see which pixels are inside the equation of the circle
    // Simpler Algorithm: find the center-of-mass of the two identified regions, we anticipate the bone
    // and marrow to be inside a 60X60 VOI about the center-of-mass, so use a scan-line algorithm to find
    // the inside bone marrow pixels
    ModelImage extractBoneMarrow(ModelImage srcImage, ModelImage boneImageMask) {
        ModelImage CTBoneMarrowImage = new ModelImage(srcImage.getType(), srcImage.getExtents(), "boneMarrowImg");

        short[] boneBuffer = new short[sliceSize];

        for (int sliceNum = 0; sliceNum < zDim; sliceNum++) {

            try {
                boneImageMask.exportData((sliceNum * boneBuffer.length), boneBuffer.length, boneBuffer);
            } catch (IOException ex) {
                System.err.println("Error exporting data");
            }
            
            
            
            try {
                CTBoneMarrowImage.importData((sliceNum * boneBuffer.length), boneBuffer, false);
            } catch (IOException ex) {
                System.err.println("Error importing data");
            }

        } // end for (int sliceNum = 0; ...)
        
        return CTBoneMarrowImage;
    } // end extractBoneMarrow(...)
    
    
    ModelImage extractBone(ModelImage srcImage) {
        // thresholds for bone in CT images
        float[] thresholds = { 1000.0f, 2000.0f };
        ModelImage CTBoneImage = threshold(srcImage, thresholds);
        
        // filter by cardinality.  Keep only connected objects that are about the size of the CT bones
        // bones should be 200 to 5000 pixels per slice
        IDObjects(CTBoneImage, 200 * zDim, 5000 * zDim);
        
        // make sure we only found 2 objects
        int numBones = (int)CTBoneImage.getMax();
        if (numBones != 2) {
            MipavUtil.displayError("extractBone() found moreor less than two leg bones!!!");
        }
        
        // may need to make sure the bone is a closed annulus, some kind of hough transform
        return CTBoneImage;
    } // end extractBone(...)
    
    
 
    public ModelImage threshold(ModelImage threshSourceImg, float[] thresh) {
        ModelImage resultImage = null;
        resultImage = new ModelImage(threshSourceImg.getType(), threshSourceImg.getExtents(), "threshResultImg");

        AlgorithmThresholdDual threshAlgo = null;
        threshAlgo = new AlgorithmThresholdDual(resultImage, threshSourceImg, thresh, 1, AlgorithmThresholdDual.BINARY_TYPE, true, false);
        threshAlgo.run();

        return resultImage;
    } // end threshold(...)

    /**
     * morphological ID_OBJECTS.
     *
     * @param  srcImage  --source image
     * @param  min       --smallest object to let through
     * @param  max       --largest object to let through
     */
    public void IDObjects(ModelImage srcImage, int min, int max) {
        AlgorithmMorphology3D MorphIDObj = null;
        MorphIDObj = new AlgorithmMorphology3D(srcImage, 4, 1, AlgorithmMorphology3D.ID_OBJECTS, 0, 0, 0, 0, true);
        MorphIDObj.setMinMax(min, max);
        MorphIDObj.run();
    }

    
    
    /**
     * DOCUMENT ME!
     *
     * @param  sourceImg  DOCUMENT ME!
     * @param  Name       DOCUMENT ME!
     */
    public void ShowImage(ModelImage sourceImg, String Name) {
        ModelImage cloneImg = (ModelImage) sourceImg.clone();
        cloneImg.calcMinMax();
        cloneImg.setImageName(Name);
        new ViewJFrameImage(cloneImg);
    }

    
    /**
     * Starts the algorithm.    OLD VERSION
     */
    public void runAlgorithm01() {

        // first version of an algorithm to find the outer thigh boundary
            
            // This seems to be where I need to put the propagation and snake code
            /*
             * From JDialogSnake
             * ModelImage image = getActiveImage();
             * float[] sigmas = { 1.0, 1.0, something that includes z resolution
             * int boundaryIterations = 50;
             * smoothness = 2;
             * VOI srcVOI = image.getVOIs();
             * int boundaryDir = AlgorithmSnake.IN_DIR; 
             * snakeAlgo = new AlgorithmSnake(image, sigmas, boundaryIterations, smoothness, srcVOI, boundaryDir);
             * 
             * int propagationType = AlgorithmSnake.PROP_ALL
             * snakeAlgo.setPropagation(propagationType);
             * 
             * snakeAlgo.addListener(this);
             * 
             * if (snakeAlgo.startMethod(Thread.MIN_PRIORITY) == false) {
             *    MipavUtil.displayError("A thread is already running on this object");
             * }
             * 
             */ 
 
/*        
            VOI goodVOI = srcImage.getVOIs().get(0);
            float xRes = srcImage.getFileInfo(0).getResolutions()[0];
            float zRes = srcImage.getFileInfo(0).getResolutions()[2];
            float normFactor = xRes / zRes; // Calculate correction factor
            float[] sigmas = new float[3];
            sigmas[0] = 1.0f;
            sigmas[1] = 1.0f;
            sigmas[2] = 1.0f * normFactor;
            int boundaryIterations = 50;
            float smoothness = 2.0f;
            int boundaryDir = AlgorithmSnake.ANY_DIR;
            AlgorithmSnake snakeAlgo = new AlgorithmSnake(srcImage, sigmas, boundaryIterations, smoothness, goodVOI, boundaryDir);
            int propagationType = AlgorithmSnake.PROP_ALL;
            snakeAlgo.setPropagation(propagationType);
            if (snakeAlgo.startMethod(Thread.MIN_PRIORITY) == false) {
                MipavUtil.displayError("A thread is already running on this object");
            }
*/            
        // this propagates the selected VOI to all slices
//        srcImage.getParentFrame().getComponentImage().getVOIHandler().propVOIAll();
        
        // propagate the selected VOI back one slice
//        srcImage.getParentFrame().getComponentImage().getVOIHandler().propVOI(1, false);
        
        // propagate the selected VOI forward one slice (Doesn't work)
//        srcImage.getParentFrame().getComponentImage().getVOIHandler().propVOI(-1, false);
        
        /*
         * This is close to what I want, but this puts the propagated contours in the same VOI.
         * I really want different VOI's for different slices.  Furthermore, I would like to propagate
         * the VOI to a VOI on the next slice, snake the propagated VOI to better fit the thigh on
         * that slice, smooth the VOI, and propagate this modified VOI to the next slice.  Do that
         * for each slice with an index greater than the current slice number and then do it backward
         * through the slice to the first slice in the volume. 
         */
        
        float xRes = srcImage.getFileInfo(0).getResolutions()[0];
        float zRes = srcImage.getFileInfo(0).getResolutions()[2];
        float normFactor = xRes / zRes; // Calculate correction factor
        float[] sigmas = new float[3];
        sigmas[0] = 1.0f;
        sigmas[1] = 1.0f;
        sigmas[2] = 1.0f * normFactor;
        int boundaryIterations = 50;
        float smoothness = 2.0f;
        int boundaryDir = AlgorithmSnake.ANY_DIR;
        
//        int propagationType = AlgorithmSnake.PROP_ALL;
//        int propagationType = AlgorithmSnake.PROP_SINGLE;
        int propagationType = AlgorithmSnake.PROP_NEXT;
        
        // Read the thigh VOI from a file 
        srcImage.getParentFrame().openVOI(false, false);
       
        // Set the read VOI as active
        VOIVector newVOI = srcImage.getVOIs();
        VOI theVOI = newVOI.get(0);
        theVOI.setActive(true);
        theVOI.getCurves()[srcImage.getParentFrame().getViewableSlice()].elementAt(0).setActive(true);
        
        // show the active VOI
        srcImage.getParentFrame().updateImages(true);

        AlgorithmSnake snakeAlgo = new AlgorithmSnake(srcImage, sigmas, boundaryIterations, smoothness, theVOI, boundaryDir);
        snakeAlgo.setPropagation(propagationType);
        if (snakeAlgo.startMethod(Thread.MIN_PRIORITY) == false) {
            MipavUtil.displayError("A thread is already running on this object");
        }
        snakeAlgo.runAlgorithm();
        VOI resultVOI = snakeAlgo.getResultVOI();
        resultVOI.setActive(true);
        
        // we also need to set the contour (curve) on the current slice to active
        resultVOI.getCurves()[srcImage.getParentFrame().getViewableSlice()].elementAt(0).setActive(true);
        
        // Smooth the resultVOI to 60 points
        AlgorithmBSmooth smoothAlgo = new AlgorithmBSmooth(srcImage, resultVOI, 60, true);
        smoothAlgo.runAlgorithm();
        VOI smoothResultVOI = smoothAlgo.getResultVOI();

        newVOI.set(0, smoothResultVOI);
               
        srcImage.getParentFrame().updateImages(true);

        
    } // end runAlgorithm01()

    
    private void calc3D_01() {
        xDim = srcImage.getExtents()[0];
        yDim = srcImage.getExtents()[1];
        zDim = srcImage.getExtents()[2];

        sliceSize = xDim * yDim;
        volumeSize = sliceSize * zDim;

        float [] buffer;
        int airLabelValue = 1;
        int fatLabelValue = 2;
        int partialVolumeLabelValue = 3;
        int muscleLabelValue = 4;
        int bloodLabelValue = 5;
        int boneLabelValue = 6;
        
        try {
            buffer = new float[volumeSize];
            srcImage.exportData(0, volumeSize, buffer); // locks and releases lock
        } catch (IOException error) {
            buffer = null;
            errorCleanUp("CT Thresholding Image: Image(s) locked", true);
            return;
        } catch (OutOfMemoryError e) {
            buffer = null;
            errorCleanUp("CT Thresholding Image: Out of Memory", true);
            return;
        }
        
        // threshold out some of the known tissues
        for (int i = 0; i < volumeSize; i++) {
            if (buffer[i] > 1000) buffer[i] = boneLabelValue;
            else if (buffer[i] >= 0 && buffer[i] <= 100) buffer[i] = muscleLabelValue;
            else if (buffer[i] >= -30 && buffer[i] <= 0) buffer[i] = partialVolumeLabelValue;
            else if (buffer[i] >= -190 && buffer[i] <= -30) buffer[i] =  fatLabelValue;
            else buffer[i] = 0;
        } // end for (int i = 0; ...)
        
        ModelImage thresholdImage = new ModelImage(srcImage.getType(), srcImage.getExtents(), "CT Threshold");

        try {
            thresholdImage.importData(0, buffer, false);
        } catch (IOException ex) {
            System.err.println("error exporting data fto thresholdImage");
        }
       
        ShowImage(thresholdImage, "CT Bone");
    } // end calc3D_01()
    



}
