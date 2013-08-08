package gov.nih.mipav.model.algorithms;


import gov.nih.mipav.model.algorithms.filters.AlgorithmGradientMagnitudeSep;
import gov.nih.mipav.model.structures.*;

import gov.nih.mipav.view.*;

import java.io.*;

public class AlgorithmCannyEdgeDetection extends AlgorithmBase {
    
    private double highThreshold = 0.12;
    
    private double lowThreshold = 0.4 * highThreshold;
    
    private float sigma = (float)Math.sqrt(2.0);
    
    
  //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * AlgorithmCannyEdgeDetection - default constructor.
     */
    public AlgorithmCannyEdgeDetection() { }
    
    /**
     * AlgorithmCannyEdgeDetection.
     *
     * @param  destImg  DOCUMENT ME!
     * @param  srcImg   DOCUMENT ME!
     * @param  highThreshold
     * @param  lowThreshold
     * @param  sigma
     */
    public AlgorithmCannyEdgeDetection(ModelImage destImg, ModelImage srcImg, double highThreshold, double lowThreshold,
                                       float sigma) {
        super(destImg, srcImg);
        this.highThreshold = highThreshold;
        this.lowThreshold = lowThreshold;
        this.sigma = sigma;   
    }


    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * finalize -
     */
    public void finalize() {
        super.finalize();
    }

    /**
     * Starts the program.
     */
    public void runAlgorithm() {

        int xDim;
        int yDim;
        int sliceSize;
        double srcBuffer[];
        AlgorithmGradientMagnitudeSep gradMagAlgo = null;
        float mag[];
        float xMag[];
        float yMag[];
        float angle[];
        float sigmaArray[] = new float[2];
        boolean entireImage = true;
        boolean image25D = true;
        int i;
        double scale;
        double maxMag = 0.0;
        
        if (srcImage == null) {
            displayError("Source Image is null");
            finalize();

            return;
        }
        
        fireProgressStateChanged(srcImage.getImageName(), "Running Canny Edge Detection ...");

        xDim = srcImage.getExtents()[0];
        yDim = srcImage.getExtents()[1];
        sliceSize = xDim * yDim;
       
        sigmaArray[0] = sigma;
        sigmaArray[1] = sigma;
        gradMagAlgo = new AlgorithmGradientMagnitudeSep(srcImage, sigmaArray, entireImage, image25D);
        gradMagAlgo.setDirectionNeeded(true);
        gradMagAlgo.setRunningInSeparateThread(runningInSeparateThread);

        linkProgressToAlgorithm(gradMagAlgo);
        gradMagAlgo.setProgressValues(generateProgressValues(0, 50));

        gradMagAlgo.run();

        if (gradMagAlgo.isCompleted() == false) {
            setCompleted(false);
            gradMagAlgo.finalize();
            gradMagAlgo = null;
            System.gc();

            return;
        }
        
        mag = gradMagAlgo.getResultBuffer();
        xMag = gradMagAlgo.getXDerivativeDirections();
        yMag = gradMagAlgo.getYDerivativeDirections();
        gradMagAlgo.finalize();
        gradMagAlgo = null;
        angle = new float[sliceSize];
        scale = 180.0/Math.PI;
        maxMag = 0.0;
        for (i = 0; i < sliceSize; i++) {
            angle[i] = (float)(scale * Math.atan2(yMag[i], xMag[i]));
            if (mag[i] > maxMag) {
                maxMag = mag[i];
            }
        }
        xMag = null;
        yMag = null;
        for (i = 0; i < sliceSize; i++) {
            if (((angle[i] >= -22.5) && (angle[i] < 22.5)) || (angle[i] < -157.5) || (angle[i] >= 157.5)) {
                // Horizontal edge
            }
            else if (((angle[i] >= 67.5) && (angle[i] < 112.5)) || ((angle[i] >= -112.5) && (angle[i] < -67.5))) {
                // Vertical edge
            }
            else if (((angle[i] >= 22.5) && (angle[i] < 67.5)) || ((angle[i] >= -157.5) && (angle[i] < -112.5))) {
                // -45 degree edge
            }
            else {
                // 45 degree edge
            }
        }
    }
}