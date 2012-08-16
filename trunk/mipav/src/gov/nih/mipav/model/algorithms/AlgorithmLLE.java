package gov.nih.mipav.model.algorithms;

import gov.nih.mipav.model.file.FileInfoBase.Unit;
import gov.nih.mipav.model.structures.*;

import gov.nih.mipav.view.*;

import java.io.*;

import java.util.*;



public class AlgorithmLLE extends AlgorithmBase {

    
    //~ Constructors ---------------------------------------------------------------------------------------------------

   

    /**
     * Creates a new AlgorithmLLE object.
     *
     * @param  destImg           list of image models where result image is to stored
     * @param  srcImg            source image model
     
     */
    public AlgorithmLLE(ModelImage destImg, ModelImage srcImg) {
        super(destImg, srcImg);
        
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
     * Starts the algorithm.
     */
    public void runAlgorithm() {
        int xDim;
        int yDim;
        int zDim;
        int sliceSize;
        int newXDim;
        int newYDim;
        int newSliceSize;
        double X[];
        int z;
        double sliceBuffer[];
        int i, j;
        double X2[];
        double repmat1[];

        if (srcImage == null) {
            displayError("LLE: Source Image is null");

            return;
        }
        
        
        xDim = srcImage.getExtents()[0];
        yDim = srcImage.getExtents()[1];
        zDim = srcImage.getExtents()[2];
        sliceSize = xDim * yDim;
        sliceBuffer = new double[sliceSize];
        
        newXDim = zDim;
        newYDim = xDim * yDim;
        newSliceSize = newXDim * newYDim;
        X = new double[newSliceSize];
        
        for (z = 0; z < zDim; z++) {
            try {
                srcImage.exportData(z * sliceSize, sliceSize, sliceBuffer);
            }
            catch (IOException e) {
                MipavUtil.displayError("IOException on srcImage.exportData" + e);
                setCompleted(false);
                return;
            }
            
            for (i = 0; i < sliceSize; i++) {
                X[z + i * newXDim] = sliceBuffer[i];
            }
        } // for (z = 0; z < zDim; z++)
        sliceBuffer = null;
        
        X2 = new double[newXDim];
        for (i = 0; i < newXDim; i++) {
            for (j = 0; j < newYDim; j++) {
                X2[i] += X[i + newXDim * j]*X[i + newXDim * j];    
            }
        }
        
        repmat1 = new double[newXDim * newXDim];
        for (i = 0; i < newXDim; i++) {
            for (j = 0; j < newXDim; j++) {
                // Each value is placed into newXDim = zDim rows
                repmat1[j + i * newXDim] = X2[j];
            }
        }
        
    } // runAlgorithm


    
}
