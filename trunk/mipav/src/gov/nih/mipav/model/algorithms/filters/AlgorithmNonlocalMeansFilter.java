package gov.nih.mipav.model.algorithms.filters;


import gov.nih.mipav.model.algorithms.*;
import gov.nih.mipav.model.structures.*;

import gov.nih.mipav.view.*;

import java.io.*;


/**
 This is a port of the 09/03/2006 NLmeansfilter.m by Jose Vicente Manjon Herrera & Antoni Buades.  The code is an
 implemntation of the algorithm in the article "A non-local algorithm for image denoising" by Antoni Buades and
 Jean-Michel Morel.
 */
public class AlgorithmNonlocalMeansFilter extends AlgorithmBase {

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** Side of the learning window of pixels which will be averaged.  Sizes generally range from about 
     *  15 X 15 to 21 X 21.
     */
    private int searchWindowSide;
    
    /** Comparsion window.  This should be smaller than the search window size.  In a non-noisy image it
     *  can be set to 3 X 3.  The value increases with the amount of noise.  In general, for noisy images
     *  7 X 7 or 9 X 9 is a good size.
     */
    private int similarityWindowSide;
    
    /** The degree of filtering of the obtained image.  When we know the standard deviation of the noise,
     *  sigma, the value of filterParameter.  A good value is between 10*sigma and 15*sigma.
     */
    private float filterParameter;

    /** In 3D if do25D == true, process each slice separately. */
    private boolean do25D = true;

   

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Creates a new AlgorithmAdaptivePathSmooth object.
     *
     * @param  destImage         denoised image
     * @param  srcImg            2D source image
     * @param  searchWindowSide  Side of the learning window of pixels which will be averaged
     * @param  similarityWindowSide Side of the comparsion window
     * @param  filterParameter   The degree of filtering of the obtained image
     * @param  do25D             If true, do slice by slice filtering
     */
    public AlgorithmNonlocalMeansFilter(ModelImage destImage, ModelImage srcImg, int searchWindowSide, 
                                       int similarityWindowSide, float filterParameter, boolean do25D) {
        super(destImage, srcImg);
        this.searchWindowSide = searchWindowSide;
        this.similarityWindowSide = similarityWindowSide;
        this.filterParameter = filterParameter;
        this.do25D = do25D;
    }

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * Prepares this class for destruction.
     */
    public void finalize() {
        srcImage = null;
        destImage = null;
       
        super.finalize();
    }


    /**
     * Starts the nonlocal means filter algorithm.
     */
    public void runAlgorithm() {

        if (srcImage == null) {
            displayError("Source Image is null");

            return;
        } 
        
        if ((srcImage.getNDims() == 2) || do25D){
            run2D();
        }
        else {
            run3D();
        }
    }
    
    private void run2D() {
        int z;
        int xDim;
        int yDim;
        int zDim;
        int length;
        float input[];
        xDim = srcImage.getExtents()[0];
        yDim = srcImage.getExtents()[1];
        length = xDim * yDim;
        input = new float[length];
        if (srcImage.getNDims() == 3) {
            zDim = srcImage.getExtents()[2];
        }
        else {
            zDim = 1;
        }
        for (z = 0; z < zDim; z++) {
            try {
                srcImage.exportData(z*length, length, input);
            }
            catch (IOException e) {
                MipavUtil.displayError("IOException on srcImage.exportData(z*length, length, input)");
                setCompleted(false);
                return;
            }
            
            try {
                if (destImage != null) {
                    destImage.importData(z*length, input, false);
                }
                else {
                    srcImage.importData(z*length, input, false);
                }
            }
            catch(IOException e) {
                MipavUtil.displayError("IOException on importData(z*length, input, false");
                setCompleted(false);
                return;
            }
        } // for (z = 0; z < zDim; z++)
        if (destImage != null) {
            destImage.calcMinMax();
        }
        else {
            srcImage.calcMinMax();
        }
        setCompleted(true);
        return;
    }
    
    private void run3D() {
        
    }

    


}
