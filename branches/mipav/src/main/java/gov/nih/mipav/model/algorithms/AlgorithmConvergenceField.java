package gov.nih.mipav.model.algorithms;


import gov.nih.mipav.model.structures.*;

import gov.nih.mipav.view.*;

import java.io.IOException;

public class AlgorithmConvergenceField extends AlgorithmBase {
    /**
     * References: 
     * 1.) "Force field energy functionals for image feature extraction", David J. Hurley, Mark S. Nixon,
     * and John N. Carter, Image and Vision Computing, Vol. 20, 2002, pp. 311-317.
     * 2.) "Force field feature extraction for ear biometrics", David J. Hurley, Mark S. Nixon, 
     * and John N. Carter, Computer Vision and Image Understanding, Vol. 98, 2005, pp. 491-512.
     * 3.) University of Southampton Ph.D. thesis, "Force Field Feature Extraction for Ear Biometrics",
     * David H. Hurley, September, 2001.
     */
    
    /**
     * 
     * @param destImg
     * @param srcImg
     */
    public AlgorithmConvergenceField(ModelImage destImg, ModelImage srcImg) {
        super(destImg, srcImg);
    }
    
    public void runAlgorithm() {
        double buffer[];
        int xDim;
        int yDim;
        int sliceSize;
        int x;
        int y;
        int i;
        int j;
        int pos;
        double fx[];
        double fy[];
        int diffx;
        int diffy;
        double mag;
        double denom;
        int index;
        double derivx;
        double derivy;
        double convergence[];
        double minValue;
        double maxValue;
        short scaledValue;
        byte byteBuffer[];
        double table[];
        
        if (srcImage == null) {
            displayError("Source Image is null");
            setCompleted(false);
    
            return;
        }
        
        fireProgressStateChanged(srcImage.getImageName(), "Convergence Field ...");
        
        xDim = srcImage.getExtents()[0];
        yDim = srcImage.getExtents()[1];
        sliceSize = xDim * yDim;
        buffer = new double[sliceSize];
        fx = new double[sliceSize];
        fy = new double[sliceSize];
        table = new double[sliceSize];
        
        try {
            srcImage.exportData(0, sliceSize, buffer);
        }
        catch (IOException e) {
            MipavUtil.displayError("IOException " + e + " on srcImage.exportData(0, sliceSize, buffer)");
            setCompleted(false);
            return;
        }
        
        // Set up the table giving the distance to the 1.5
        for (y = 0; y < yDim; y++) {
            for (x = 0; x < xDim; x++) {
                pos = x + y * xDim;
                table[pos] = Math.pow(x * x + y * y, 1.5);
            }
        }
        
        // Calculate the force direction
        for (y = 0; y < yDim; y++) {
            for (x = 0; x < xDim; x++) {
                pos = x + y * xDim;
                for (j = 0; j < yDim; j++) {
                    for (i = 0; i < xDim; i++) {
                        if ((i != x) || (j != y)) {
                            index = i + j * xDim;
                            diffx = i - x;
                            diffy = j - y;
                            denom = table[Math.abs(diffx) + xDim * Math.abs(diffy)];
                            fx[pos] = fx[pos] + (buffer[index] * diffx)/denom;
                            fy[pos] = fy[pos] + (buffer[index] * diffy)/denom;
                        } // if ((i != x) || (j != y))
                    } // for (i = 0; i < xDim; i++)
                } // for (j = 0; j < yDim; j++)
                mag = Math.sqrt(fx[pos] * fx[pos] + fy[pos] * fy[pos]);
                fx[pos] = fx[pos]/mag;
                fy[pos] = fy[pos]/mag;
            } // for (x = 0; x < xDim; x++)
        } // for (y = 0; y < yDim; y++)
        buffer = null;
        table = null;
        
        // Calculate the convergence field, which is equal to minus the divergence of the force direction
        minValue = Double.MAX_VALUE;
        maxValue = -Double.MAX_VALUE;
        convergence = new double[sliceSize];
        for (y = 0; y < yDim; y++) {
            for (x = 0; x < xDim; x++) {
                pos = x + y * xDim;
                if (x == 0) {
                    derivx = fx[pos+1] - fx[pos];
                }
                else if (x == xDim - 1) {
                    derivx = fx[pos] - fx[pos-1];
                }
                else {
                    derivx = (fx[pos+1] - fx[pos-1])/2.0;
                }
                if (y == 0) {
                    derivy = fy[pos + xDim] - fy[pos];
                }
                else if (y == yDim - 1) {
                    derivy = fy[pos] - fy[pos - xDim];
                }
                else {
                    derivy = (fy[pos + xDim] - fy[pos - xDim])/2.0;
                }
                convergence[pos] = -(derivx + derivy);
                if (convergence[pos] < minValue) {
                    minValue = convergence[pos];
                }
                if (convergence[pos] > maxValue) {
                    maxValue = convergence[pos];
                }
            } // for (x = 0; x < xDim; x++)
        } // for (y = 0; y < yDim; y++)
        fx = null;
        fy = null;
        Preferences.debug("The minimum convergence value found was = " + minValue + "\n", Preferences.DEBUG_ALGORITHM);
        Preferences.debug("The maximum convergence value found was = " + maxValue + "\n", Preferences.DEBUG_ALGORITHM);
        
        // Scale convergence values to a range going from 0 to 255.
        byteBuffer = new byte[sliceSize];
        for (i = 0; i < sliceSize; i++) {
           scaledValue = (short)(Math.round(255.0 * (convergence[i] - minValue)/(maxValue - minValue)));
           byteBuffer[i] = (byte)(scaledValue & 0xff);
        }
        convergence = null;
        
        try {
            destImage.importData(0, byteBuffer, true);
        }
        catch(IOException e) {
            MipavUtil.displayError("IOException " + e + " on destImage.importData(0, byteBuffer, true");
            setCompleted(false);
            return;
        }
        
        setCompleted(true);
        return;
    }
}