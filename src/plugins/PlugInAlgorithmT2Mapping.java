import gov.nih.mipav.model.algorithms.AlgorithmBSmooth;
import gov.nih.mipav.model.algorithms.AlgorithmBase;
import gov.nih.mipav.model.algorithms.AlgorithmMorphology2D;
import gov.nih.mipav.model.algorithms.AlgorithmMorphology3D;
import gov.nih.mipav.model.algorithms.AlgorithmSnake;

import gov.nih.mipav.model.structures.ModelImage;
import gov.nih.mipav.model.structures.VOI;
import gov.nih.mipav.model.structures.VOIVector;
import gov.nih.mipav.view.MipavUtil;
import gov.nih.mipav.view.ViewJFrameImage;

import java.io.IOException;
import java.util.BitSet;


public class PlugInAlgorithmT2Mapping extends AlgorithmBase {

    /** X dimension of the CT image */
    private int xDim;

    /** Y dimension of the CT image */
    private int yDim;

    /** Z dimension of the CT image */
    private int zDim;

    /** Slice size for xDim*yDim in pixels*/
    private int sliceSize;

    /** Volume size for xDim*yDim*zDim */
    private int volumeSize;
    
    private ModelImage brainImage;
    private ModelImage segmentedImage;
    
//  private ModelImage muscleBundleImage;
//  center-of-mass array for region 1 and 2 (the thresholded bone)
    
    private int[] x1CMs;
    private int[] y1CMs;
    private int[] x2CMs;
    private int[] y2CMs;
    
    private ModelImage brainResultImage;
       
    // temp buffer to store slices.  Needed in many member functions.
    private short[] sliceBuffer;
    
    // unique label values for the regions of interest
    private short muscleBundleLabel = 10;
    private short thighTissueLabel = 10;
    private short boneMarrowLabel = 20;
    private short boneLabel = 3;

    private boolean initializedFlag = false;

    private BitSet volumeBitSet;
    
    private double[] TEValues;
    
    /**
     * Constructor.
     *
     * @param  resultImage  Result image model
     * @param  srcImg       Source image model.
     */
    public PlugInAlgorithmT2Mapping(ModelImage resultImage, ModelImage srcImg, double[] inTimeVals) 
    {
    	super(resultImage, srcImg);
    	TEValues= inTimeVals;
    }
    
    /**
     * Starts the algorithm.
     */
    public void runAlgorithm() 
    {
        // Algorithm to fit the T2

        if (!initializedFlag) 
        {
        	init();

        	//TODO:Fit each T2 map to multiexponential
        	//TODO: Make first channel major exponential component
        	//TODO: Make second channel total X^2
        	
        	ModelImage map = null;//t2mapMonoExp.map(srcImage);
        }

        if (!initializedFlag)
        {
            return;
        }

    } // end runAlgorithm()

//  ~ Methods --------------------------------------------------------------------------------------------------------
   
    /**
     * Prepares this class for destruction.
     */
    public void finalize()
    {
        destImage = null;
        srcImage = null;
        super.finalize();
    }
    
    /**
     * Create all the data structures that are needed by the various routines to automatically
     * segment the bone, bone marrow, and muscle bundle in 2D and 3D CT images of the thighs.
     */
     
    void init() 
    {
    //	   Entering TE values        

        // simple error check up front
        if (srcImage.getNDims() < 4) {
            MipavUtil.displayError("PlugInAlgorithmT2Mapping::init() Image must be a 4D image");
            return;
        } 

        // set and allocate know values
        xDim = srcImage.getExtents()[0];
        yDim = srcImage.getExtents()[1];

        sliceSize = xDim * yDim;

        try 
        {
            sliceBuffer = new short[sliceSize];
            x1CMs = new int [sliceSize];
            y1CMs = new int [sliceSize];
            x2CMs = new int [sliceSize];
            y2CMs = new int [sliceSize];
        } 
        catch (OutOfMemoryError error)
        {
            System.gc();
            MipavUtil.displayError("Out of memory: init()");
        }

        // set values that depend on the source image being a 2D or 3D image
        if (srcImage.getNDims() == 2) 
        {
            zDim = 1;
            volumeSize = sliceSize;
        } 
        else if (srcImage.getNDims() == 3) 
        {
            zDim = srcImage.getExtents()[2];
            volumeSize = sliceSize * zDim;
        }

        // make the label images and initialize their resolutions
//        muscleBundleImage = new ModelImage(ModelStorageBase.USHORT, srcImage.getExtents(), "muscleBundleImage");  
        
        volumeBitSet = new BitSet();
               
        // set initialized flag to true so the data structures are not reallocated
        initializedFlag = true;
    }   // end init()

   

    /**
     * morphological ID_OBJECTS.
     *
     * @param  srcImage  --source image
     * @param  min       --smallest object to let through
     * @param  max       --largest object to let through
     */
    public void IDObjects2D(ModelImage srcImage, int min, int max) 
    {
        AlgorithmMorphology2D MorphIDObj = null;
        MorphIDObj = new AlgorithmMorphology2D(srcImage, 4, 1, AlgorithmMorphology2D.ID_OBJECTS, 0, 0, 0, 0, true);
        MorphIDObj.setMinMax(min, max);
        MorphIDObj.run();
    }

    /**
     * morphological ID_OBJECTS.
     *
     * @param  srcImage  --source image
     * @param  min       --smallest object to let through
     * @param  max       --largest object to let through
     */
    public void IDObjects3D(ModelImage srcImage, int min, int max) 
    {
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
    public void ShowImage(ModelImage sourceImg, String Name) 
    {
        ModelImage cloneImg = (ModelImage) sourceImg.clone();
        cloneImg.calcMinMax();
        cloneImg.setImageName(Name);
        new ViewJFrameImage(cloneImg);
    }
    
    /**
     * Starts the algorithm.    OLD VERSION
     */
    public void runAlgorithm01() 
    {
    	
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
//        srcImage.getParentFra ().getComponentImage().getVOIHandler().propVOI(1, false);
        
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

//      int propagationType = AlgorithmSnake.PROP_ALL;
//      int propagationType = AlgorithmSnake.PROP_SINGLE;
        int propagationType = AlgorithmSnake.PROP_NEXT;

        // Read the thigh VOI from a file 
        srcImage.getParentFrame().openVOI(false, false);

        // Set the read VOI as active
        VOIVector newVOI = srcImage.getVOIs();
        VOI theVOI = newVOI.get(0);
        theVOI.setActive(true);
        theVOI.getSliceCurves(srcImage.getParentFrame().getViewableSlice()).elementAt(0).setActive(true);

        // show the active VOI
        srcImage.getParentFrame().updateImages(true);

        AlgorithmSnake snakeAlgo = new AlgorithmSnake(srcImage, sigmas, boundaryIterations, smoothness, theVOI, boundaryDir);
        snakeAlgo.setPropagation(propagationType);
        if (snakeAlgo.startMethod(Thread.MIN_PRIORITY) == false)
        {
            MipavUtil.displayError("A thread is already running on this object");
        }
        snakeAlgo.runAlgorithm();
        VOI resultVOI = snakeAlgo.getResultVOI();
        resultVOI.setActive(true);

        // we also need to set the contour (curve) on the current slice to active
        resultVOI.getSliceCurves(srcImage.getParentFrame().getViewableSlice()).elementAt(0).setActive(true);

        // Smooth the resultVOI to 60 points
        AlgorithmBSmooth smoothAlgo = new AlgorithmBSmooth(srcImage, resultVOI, 60, true);
        smoothAlgo.runAlgorithm();
        VOI smoothResultVOI = smoothAlgo.getResultVOI();

        newVOI.set(0, smoothResultVOI);
        
        srcImage.getParentFrame().updateImages(true);

    } // end runAlgorithm01()

    private void calc3D_01() 
    {
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

        try
        {
            buffer = new float[volumeSize];
            srcImage.exportData(0, volumeSize, buffer); // locks and releases lock
        } 
        catch (IOException error) 
        {
            buffer = null;
            errorCleanUp("CT Thresholding Image: Image(s) locked", true);
            return;
        } 
        catch (OutOfMemoryError e)
        {
            buffer = null;
            errorCleanUp("CT Thresholding Image: Out of Memory", true);
            return;
        }
        
        // threshold out some of the known tissues
        for (int i = 0; i < volumeSize; i++) {
            if (buffer[i] > 1000) 
            	buffer[i] = boneLabelValue;
            else if (buffer[i] >= 0 && buffer[i] <= 100) 
            	buffer[i] = muscleLabelValue;
            else if (buffer[i] >= -30 && buffer[i] <= 0)
            	buffer[i] = partialVolumeLabelValue;
            else if (buffer[i] >= -190 && buffer[i] <= -30)
            	buffer[i] =  fatLabelValue;
            else
            	buffer[i] = 0;
        } // end for (int i = 0; ...)
        
        ModelImage thresholdImage = new ModelImage(srcImage.getType(), srcImage.getExtents(), "CT Threshold");

        try
        {
            thresholdImage.importData(0, buffer, false);
        } 
        catch (IOException ex) 
        {
            System.err.println("error exporting data to thresholdImage");
        }

        ShowImage(thresholdImage, "CT Bone");
    } // end calc3D_01()

}