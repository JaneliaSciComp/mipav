package gov.nih.mipav.model.algorithms;


import gov.nih.mipav.model.algorithms.filters.AlgorithmGradientMagnitudeSep;
import gov.nih.mipav.model.structures.*;

import gov.nih.mipav.view.*;

import java.io.*;

public class AlgorithmCannyEdgeDetection extends AlgorithmBase {
    
    // Reference: Digital Image Processing Third Edition by Rafael C. Gonzalez and Richard E. Woods,
    // pp. 719-725.
    
    // Canny said that the ratio of high to low threshold should be two or three to one.
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
        int x;
        int y;
        int yoff;
        float gN[];
        byte gNH[];
        byte gNL[];
        byte valid[];
        boolean change;
        AlgorithmThinning2D thinAlgo = null;
        
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
        gN = new float[sliceSize];
        for (y = 0; y < yDim; y++)  {
            yoff = y * xDim;
            for (x = 0; x < xDim; x++) {
                i = yoff + x;
                if (((angle[i] >= -22.5) && (angle[i] < 22.5)) || (angle[i] < -157.5) || (angle[i] >= 157.5)) {
                    // Horizontal edge
                    if ((y > 0) && (mag[i - xDim] > mag[i])) {
                        gN[i] = 0.0f;
                    }
                    else if ((y < yDim-1) && (mag[i + xDim] > mag[i])) {
                        gN[i] = 0.0f;
                    }
                    else {
                        gN[i] = mag[i];
                    }
                }
                else if (((angle[i] >= 67.5) && (angle[i] < 112.5)) || ((angle[i] >= -112.5) && (angle[i] < -67.5))) {
                    // Vertical edge
                    if ((x > 0) && (mag[i - 1] > mag[i])) {
                        gN[i] = 0.0f;
                    }
                    else if ((x < xDim-1) && (mag[i+1] > mag[i])) {
                        gN[i] = 0.0f;
                    }
                    else {
                        gN[i] = mag[i];
                    }
                }
                else if (((angle[i] >= 22.5) && (angle[i] < 67.5)) || ((angle[i] >= -157.5) && (angle[i] < -112.5))) {
                    // -45 degree edge
                    if ((y > 0) && (x > 0) && (mag[i - xDim - 1] > mag[i])) {
                        gN[i] = 0.0f;
                    }
                    else if ((y < yDim-1) && (x < xDim-1) && (mag[i + xDim + 1] > mag[i])) {
                        gN[i] = 0.0f;
                    }
                    else {
                        gN[i] = mag[i];
                    }
                }
                else {
                    // 45 degree edge
                    if ((y > 0) && (x < xDim - 1) && (mag[i - xDim + 1] > mag[i])) {
                        gN[i] = 0.0f;
                    }
                    else if ((x > 0) && (y < yDim - 1) && (mag[i + xDim - 1] > mag[i])) {
                        gN[i] = 0.0f;
                    }
                    else {
                        gN[i] = mag[i];
                    }
                }
            } // for (x = 0; x < xDim; x++)
        } // for (y = 0; y < yDim; y++)
        mag = null;
        gNH = new byte[sliceSize];
        gNL = new byte[sliceSize];
        lowThreshold = lowThreshold * maxMag;
        highThreshold = highThreshold * maxMag;
        for (i = 0; i < sliceSize; i++) {
            if (gN[i] >= lowThreshold) {
                if (gN[i] >= highThreshold) {
                    gNH[i] = 1;
                }
                else {
                    gNL[i] = 1;
                }
            }
        } // for (i = 0; i < sliceSize; i++)
        valid = new byte[sliceSize];
        for (y = 0; y < yDim; y++) {
            yoff = y * xDim;
            for (x = 0; x < xDim; x++) {
                i = yoff + x;
                if (gNH[i] == 1) {
                    if ((x > 0) && (gNL[i-1] == 1)) {
                        valid[i-1] = 1;
                    }
                    else if ((x < xDim-1) && (gNL[i+1] == 1)) {
                        valid[i+1] = 1;
                    }
                    else if ((y > 0) && (gNL[i-xDim] == 1)) {
                        valid[i-xDim] = 1;
                    }
                    else if ((y < yDim-1) && (gNL[i+xDim] == 1)) {
                        valid[i+xDim] = 1;
                    }
                    else if ((x > 0) && (y > 0) && (gNL[i - xDim - 1] == 1)) {
                        valid[i-xDim-1] = 1;
                    }
                    else if ((x > 0) && (y < yDim - 1) && (gNL[i + xDim - 1] == 1)) {
                        valid[i + xDim - 1] = 1;
                    }
                    else if ((x < xDim-1) && (y > 0) && (gNL[i - xDim + 1] == 1)) {
                        valid[i - xDim + 1] = 1;
                    }
                    else if ((x < xDim -1) && (y < yDim - 1) && (gNL[i + xDim + 1] == 1)) {
                        valid[i + xDim + 1] = 1;
                    }
                } // if (gNH[i] == 1)
            } // for (x = 0; x < xDim; x++)
        } // for (y = 0; y < yDim; y++)
        change = true;
        while (change) {
            change = false;
            for (y = 0; y < yDim; y++) {
                yoff = y * xDim;
                for (x = 0; x < xDim; x++) {
                    i = yoff + x;
                    if ((valid[i] == 0) && (gNL[i] == 1)) {
                        if (((x > 0) && (valid[i-1] == 1)) ||
                            ((x < xDim - 1) && (valid[i+1] == 1)) ||
                            ((y > 0) && (valid[i-xDim] == 1)) ||
                            ((y < yDim-1) && (valid[i+xDim] == 1)) ||
                            ((x > 0) && (y > 0) && (valid[i - xDim - 1] == 1)) ||
                            ((x > 0) && (y < yDim -1) && (valid[i + xDim - 1] == 1)) ||
                            ((x < xDim - 1) && (y > 0) && (valid[i - xDim + 1] == 1)) ||
                            ((x < xDim -1) && (y < yDim - 1) && (valid[i + xDim + 1] == 1))) {
                            valid[i] = 1;
                            change = true;
                        }
                    }
                } // for (x = 0; x < xDim; x++)
            } // for (y = 0; y < yDim; y++)
        } // while (change)
        gNL = null;
        
        for (i = 0; i < sliceSize; i++) {
            if (valid[i] == 1) {
                gNH[i] = 1;
            }
        }
        valid = null;
        
        try {
            destImage.importData(0, gNH, true);
        }
        catch (IOException e) {
            MipavUtil.displayError("IOException " + e + " on destImage.importData(0, gNH, true)");
            setCompleted(false);
            return;
        }
        
        thinAlgo = new AlgorithmThinning2D(null, destImage);
        thinAlgo.run();
        thinAlgo.finalize();
        thinAlgo = null;
        
        setCompleted(true);
        return;
    }
}