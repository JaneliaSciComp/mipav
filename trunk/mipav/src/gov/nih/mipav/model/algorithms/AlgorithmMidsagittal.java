package gov.nih.mipav.model.algorithms;


import java.io.IOException;

import gov.nih.mipav.model.algorithms.registration.*;
import gov.nih.mipav.model.algorithms.utilities.*;
import gov.nih.mipav.model.structures.*;
import gov.nih.mipav.view.MipavUtil;


/**
 * Finds the midsagittal line of a brain MRI by:
 *
 * <ul>
 *   <li>Flipping the image horizontally.</li>
 *   <li>Registering the flipped image against the original.</li>
 *   <li>Getting the angle that the registration rotated the image.</li>
 *   <li>Transforming the original image by half the registration rotation.</li>
 * </ul>
 *
 * @version  0.1 Dec 29, 2004
 * @author   Evan McCreedy
 */
public class AlgorithmMidsagittal extends AlgorithmBase {

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /**
     * Limits the number of iterations of Powell's algorithm when registering the original image against the flipped
     * image.
     */
    private int baseNumIter = 2;

    /** The coarse angle step to use in each dimension when registering the original image against the flipped image. */
    private int coarseAngle = 15;

    /** The cost function to use when registering the original image against the flipped image. */
    private int costFunc = AlgorithmCostFunctions.CORRELATION_RATIO_SMOOTHED;

    /** The number of degrees of freedom to use when registering the original image against the flipped image. */
    private int dof = 6;

    /** Whether to use subsampling when registering the original image against the flipped image. */
    private boolean doSubsample = true;
    
    private boolean doMultiThread = true;

    /**
     * Whether to skip searching of the parameter space when registering the original image against the flipped image.
     */
    private boolean fastMode = false;

    /** The fine angle step to use in each dimension when registering the original image against the flipped image. */
    private int fineAngle = 6;

    /** The interpolation method to use when registering the original image against the flipped image. */
    private int interp = AlgorithmTransform.TRILINEAR;

    /**
     * Whether to use the maximum of the minimum resolutions of the 2 images when respampling during registration
     * against the flipped image.
     */
    private boolean maxOfMin = true;

    /**
     * Number of minima from level 8 to test at level 4 when registering the original image against the flipped image.
     */
    private int numMinima = 3;

    /**
     * The search angles to use in each dimension (from <code>-searchAngle</code> to <code>searchAngle</code>) when
     * registering the original image against the flipped image.
     */
    private int searchAngle = 45;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Construct the midsagittal algorithm.
     *
     * @param  srcImg  source image model
     */
    public AlgorithmMidsagittal(ModelImage srcImg) {
        super(null, srcImg);
    }

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * Prepares this class for destruction.
     */
    public void finalize() {
        srcImage = null;
        super.finalize();
    }

    /**
     * Get the midsagittal aligned image.
     *
     * @return  the aligned image
     */
    public ModelImage getResultImage() {
        return destImage;
    }

    /**
     * Starts the algorithm.
     */
    public void runAlgorithm() {

        if (srcImage == null) {
            displayError("Source Image is null");
            finalize();

            return;
        }

        if (threadStopped) {
            finalize();

            return;
        }

        
        if (srcImage.getNDims() == 4) {
        	calc35D();
        }
        else {
            calc();
        }
    }

    /**
     * Find the midsagittal line and transform the source image to align it along that line vertically.
     */
    private void calc() {
        ModelImage flipImage = (ModelImage) srcImage.clone(srcImage.getImageName() + "_flip");

        fireProgressStateChanged("Flipping image ...");

        // flip
        AlgorithmFlip flipAlgo = new AlgorithmFlip(flipImage, AlgorithmFlip.Y_AXIS, AlgorithmFlip.IMAGE, true);
        flipAlgo.setRunningInSeparateThread(false);
        fireProgressStateChanged(10);


        flipAlgo.run();

        // register
        AlgorithmRegOAR3D regAlgo = new AlgorithmRegOAR3D(srcImage, flipImage, costFunc, dof, interp, -searchAngle,
                                                          searchAngle, coarseAngle, fineAngle, -searchAngle,
                                                          searchAngle, coarseAngle, fineAngle, -searchAngle,
                                                          searchAngle, coarseAngle, fineAngle, maxOfMin, doSubsample,
                                                          doMultiThread, fastMode, baseNumIter, numMinima);

        linkProgressToAlgorithm(regAlgo);
        regAlgo.setProgressValues(generateProgressValues(10, 90));

        regAlgo.setRunningInSeparateThread(false);
        regAlgo.run();

        flipImage.disposeLocal();

        // get xy translations and z rotation
        TransMatrix trans = regAlgo.getTransformMigsagittal();

        // rotate by half
        float[] res = srcImage.getFileInfo(0).getResolutions();
        int[] ext = srcImage.getExtents();
        AlgorithmTransform transformAlgo = new AlgorithmTransform(srcImage, trans, AlgorithmTransform.TRILINEAR, res[0],
                                                                  res[1], res[2], ext[0], ext[1], ext[2], 
                                                                  false, false,
                                                                  false);
        transformAlgo.setRunningInSeparateThread(false);
        linkProgressToAlgorithm(transformAlgo);
        transformAlgo.setProgressValues(generateProgressValues(90, 100));

        // transformAlgo.setUpdateOriginFlag(true);
        transformAlgo.run();

        fireProgressStateChanged(100);
        destImage = transformAlgo.getTransformedImage();
        destImage.calcMinMax();
        destImage.setImageName(srcImage.getImageName() + "_midsag");

        setCompleted(true);
    }
    
    private void calc35D() {
    	int t;
    	float[] res = srcImage.getFileInfo(0).getResolutions();
        int[] ext = srcImage.getExtents();
    	int xDim = srcImage.getExtents()[0];
    	int yDim = srcImage.getExtents()[1];
    	int zDim = srcImage.getExtents()[2];
    	float xResols = srcImage.getFileInfo()[0].getResolutions()[0];
    	float yResols = srcImage.getFileInfo()[0].getResolutions()[1];
    	float zResols = srcImage.getFileInfo()[0].getResolutions()[2];
    	float resols3D[] = new float[] {xResols, yResols, zResols};
    	int extents3D[] = new int[] {xDim, yDim, zDim};
    	int volume = xDim * yDim * zDim;
    	int tDim = srcImage.getExtents()[3];
    	int colorFactor = 1;
    	if (srcImage.isColorImage()) {
    		colorFactor = 4;
    	}
    	double volumeBuffer[] = new double[colorFactor * volume];
    	ModelImage volumeImage = new ModelImage(srcImage.getType(), extents3D, "volumeImage");
        volumeImage.getFileInfo()[0].setResolutions(resols3D);
        destImage = new ModelImage(srcImage.getType(), ext, srcImage.getImageName() + "_midsag");
    	for (t = 0; t < tDim; t++) {
    		fireProgressStateChanged((t * 100)/tDim);
    		try {
                srcImage.exportData(t * colorFactor * volume, colorFactor * volume, volumeBuffer);
            } catch (IOException ex) {
                System.gc();
                MipavUtil.displayError("IOException = " + ex + " on srcImage.exportData");

                return;
            } 

            try {
                volumeImage.importData(0, volumeBuffer, true);
            } catch (IOException ex) {
                System.gc();
                MipavUtil.displayError("IOException = " + ex + " on volumeImage.importData");

                return;
            }
            
            ModelImage flipImage = (ModelImage) volumeImage.clone(volumeImage.getImageName() + "_flip");
            // flip
            AlgorithmFlip flipAlgo = new AlgorithmFlip(flipImage, AlgorithmFlip.Y_AXIS, AlgorithmFlip.IMAGE, true);
            flipAlgo.setRunningInSeparateThread(false);

            flipAlgo.run();

            // register
            AlgorithmRegOAR3D regAlgo = new AlgorithmRegOAR3D(volumeImage, flipImage, costFunc, dof, interp, -searchAngle,
                                                              searchAngle, coarseAngle, fineAngle, -searchAngle,
                                                              searchAngle, coarseAngle, fineAngle, -searchAngle,
                                                              searchAngle, coarseAngle, fineAngle, maxOfMin, doSubsample,
                                                              doMultiThread, fastMode, baseNumIter, numMinima);
            
            regAlgo.setRunningInSeparateThread(false);
            regAlgo.run();

            flipImage.disposeLocal();

            // get xy translations and z rotation
            TransMatrix trans = regAlgo.getTransformMigsagittal();

            // rotate by half
            
            AlgorithmTransform transformAlgo = new AlgorithmTransform(volumeImage, trans, AlgorithmTransform.TRILINEAR, res[0],
                                                                      res[1], res[2], ext[0], ext[1], ext[2], 
                                                                      false, false,
                                                                      false);
            transformAlgo.setRunningInSeparateThread(false);
            transformAlgo.run();
            ModelImage transformedImage = transformAlgo.getTransformedImage();
            try {
                transformedImage.exportData(0, colorFactor * volume, volumeBuffer);
            } catch (IOException ex) {
                System.gc();
                MipavUtil.displayError("IOException = " + ex + " on transformedImage.exportData");

                return;
            }
            transformedImage.disposeLocal();
            transformedImage = null;
            
            try {
                destImage.importData(t * colorFactor * volume, volumeBuffer, false);
            } catch (IOException ex) {
                System.gc();
                MipavUtil.displayError("IOException = " + ex + " on destImage.importData");

                return;
            }
    	}
    	volumeImage.disposeLocal();
    	volumeImage = null;
    	destImage.calcMinMax();
    	fireProgressStateChanged(100);
    	setCompleted(true);
    }
}
