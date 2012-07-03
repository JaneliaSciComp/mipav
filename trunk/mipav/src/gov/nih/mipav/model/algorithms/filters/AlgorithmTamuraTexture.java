package gov.nih.mipav.model.algorithms.filters;


import gov.nih.mipav.model.algorithms.*;
import gov.nih.mipav.model.structures.*;

import gov.nih.mipav.view.*;

import java.io.*;
import java.text.DecimalFormat;

/**
 * DOCUMENT ME!
 *
 * @version  0.1 July 3, 2012
 * @author   William Gandler 

 
 * 
 */
public class AlgorithmTamuraTexture extends AlgorithmBase {
    
    /** Red channel. */
    private static final int RED_OFFSET = 1;
    
    /** coarsenessThreshold must be <= 1.0.
    If coarsenessThreshold < 1.0, select the largest k for which Ek >= coarsenessThreshold * Emax.
    If coarsenessThreshold = 1.0, select the largest k which gives Emax.
    */
    private double coarsenessThreshold = 1.0;
    
    private boolean doCoarseness;
    
    private boolean doContrast;
    
    private boolean doDirectionality;
    
    private int RGBOffset = RED_OFFSET; 

    //~ Constructors ---------------------------------------------------------------------------------------------------
    
   

    /**
     * Creates a new AlgorithmTamuraTexture object for black and white image.
     *
     * @param              srcImg             source image model
     * @param              doCoarseness
     * @param              coarsenessThreshold
     * @param              doContrast
     * @param              doDirectionality
     */
    public AlgorithmTamuraTexture(ModelImage srcImg, boolean doCoarseness, double coarsenessThreshold,
                                  boolean doContrast, boolean doDirectionality) {
        super(null, srcImg);
        this.doCoarseness = doCoarseness;
        this.coarsenessThreshold = coarsenessThreshold;
        this.doContrast = doContrast;
        this.doDirectionality = doDirectionality;
    }
    
    /**
     * Creates a new AlgorithmTamuraTexture object for color image.
     *
     * @param              srcImg             source image model
     * @param              RGBOffset          selects red, green, or blue channel
     * @param              doCoarseness
     * @param              coarsenessThreshold
     * @param              doContrast
     * @param              doDirectionality
     */
    public AlgorithmTamuraTexture(ModelImage srcImg, int RGBOffset, boolean doCoarseness, double coarsenessThreshold,
                                  boolean doContrast, boolean doDirectionality) {
        super(null, srcImg);
        this.RGBOffset = RGBOffset;
        this.doCoarseness = doCoarseness;
        this.coarsenessThreshold = coarsenessThreshold;
        this.doContrast = doContrast;
        this.doDirectionality = doDirectionality;
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
     * Starts the program.
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

        

        fireProgressStateChanged(0, null, "Running Tamura texture ...");
        
        calculateSliceTamuraTexture();
    }

    
    
    private void calculateSliceTamuraTexture() {
        int xDim = srcImage.getExtents()[0];
        int yDim = srcImage.getExtents()[1];
        int sliceSize = xDim * yDim;
        int zDim;
        double[] sourceBuffer = new double[sliceSize];
        int kSliceSize = (xDim-31)*(yDim-31);
        int khvSliceSize = (xDim-63)*(yDim-63);
        double[][] Ak = null;
        double[][] Ekh = null;
        double[][] Ekv = null;
        double[] Sbest = null;
        double Fcoarseness;
        int kMax;
        double Emax;
        float[] floatBuffer;
        int x, y;
        int i, j, k;
        int exponent;
        int kPos;
        int khvPos;
        int z;
        double sum;
        double mean;
        double diff;
        double squaredDiff;
        double fourthDiff;
        double squaredSum;
        double fourthSum;
        double variance;
        double fourthCentralMoment;
        double kurtosis;
        double standardDeviation;
        double Fcontrast;
        double gradH[] = null;
        double gradV[] = null;
        int gradSize = (xDim-2)*(yDim-2);
        int pos;
        int gradPos;
        ViewUserInterface UI = ViewUserInterface.getReference();
        DecimalFormat kDecimalFormat = new DecimalFormat();
        kDecimalFormat.setMinimumFractionDigits(3);
        kDecimalFormat.setMaximumFractionDigits(3);
        
        if (srcImage.getNDims() == 2) {
            zDim = 1;
        }
        else {
            zDim = srcImage.getExtents()[2];
        }

        if (doCoarseness) {
            Ak = new double[5][kSliceSize];
            Ekh = new double[5][khvSliceSize];
            Ekv = new double[5][khvSliceSize];
            Sbest = new double[khvSliceSize];
        } // if (doCoarseness)
        
        if (doDirectionality) {
            gradH = new double[gradSize];
            gradV = new double[gradSize];
        }
        
        for (z = 0; z < zDim; z++) {
            try {
                if (srcImage.isColorImage()) {
                    floatBuffer = new float[sliceSize];
                    srcImage.exportRGBData(RGBOffset, 4*z*sliceSize, sliceSize, floatBuffer);  
                    for (i = 0; i < sliceSize; i++) {
                        sourceBuffer[i] = (double)floatBuffer[i];
                    }
                    floatBuffer = null;
                }
                else {
                    srcImage.exportData(z*sliceSize, sliceSize, sourceBuffer);
                }
            } catch (IOException error) {
                if (srcImage.isColorImage()) {
                    MipavUtil.displayError(
                    "AlgorithmTamuraTexture: IOException on srcImage.exportRGBData(RGBOffset,4*z*sliceSize,sliceSize,floatBuffer)");
                }
                else {
                    MipavUtil.displayError(
                    "AlgorithmTamuraTexture: IOException on srcImage.exportData(z*sliceSize,sliceSize,sourceBuffer)");    
                }
                setCompleted(false);
    
                return;
            } 
            
            if (doCoarseness) {
                
                for (y = 16; (y <= yDim-16) && !threadStopped; y++) { 
                    for (x = 16; x <= xDim-16; x++) {
                        kPos = x-16 + ((y-16)*(xDim-31));
                        for (k = 1; k <= 5; k++) {
                             exponent = (int)Math.round(Math.pow(2,k-1)); 
                             for (j = y - exponent; j <= y + exponent - 1; j++) {
                                 for (i = x - exponent; i <= x + exponent - 1; i++) {
                                     Ak[k-1][kPos] += sourceBuffer[i + j*xDim];    
                                 } // for (i = x - exponent; i <= x + exponent - 1; i++)
                             } // for (j = y - exponent; j <= y + exponent - 1; j++)
                             Ak[k-1][kPos] = Ak[k-1][kPos]/(Math.pow(2.0, 2.0*k));
                        } // for (k = 1; k <= 5; k++)
                    } // for (x = 16; x <= xDim-16; x++)
                } // for (y = 16; (y <= yDim-16) && !threadStopped; y++)
                
                for (y = 32; y <= (yDim-32) && !threadStopped; y++) {
                    for (x = 32; x <= xDim - 32; x++) {
                        kPos = x-16 + ((y-16)*(xDim-31));
                        khvPos = x-32 +((y-32)*(xDim-63));
                        for (k = 1; k <= 5; k++) {   
                            exponent = (int)Math.round(Math.pow(2,k-1)); 
                            Ekh[k-1][khvPos] = Math.abs(Ak[k-1][kPos + exponent] - Ak[k-1][kPos - exponent]);
                            Ekv[k-1][khvPos] = Math.abs(Ak[k-1][kPos + exponent*(xDim-31)] - Ak[k-1][kPos - exponent*(xDim-31)]);
                        } // for (k = 1; k <= 5; k++)
                    } // for (x = 16; x <= xDim - 32; x++)
                } // for (y = 32; y <= (yDim-32) && !threadStopped; y++)
                
                if (z < zDim - 1) {
                    for (k = 0; k < 5; k++) {
                        for (i = 0; i < kSliceSize; i++) {
                            Ak[k][i] = 0.0;
                        } // for (i = 0; i < kSliceSize; i++)
                    } // for (k = 0; k < 5; k++)
                } // if (z < zDim - 1)
                
                Fcoarseness = 0.0;
                for (y = 32; y <= (yDim-32) && !threadStopped; y++) {
                     for (x = 32; x <= xDim-32; x++) {
                         kMax = 0;
                         Emax = 0.0;
                         khvPos = x-32 + ((y-32)*(xDim-63));
                         for (k = 1; k <= 5; k++) {
                             if (Ekh[k-1][khvPos] >= Emax) {
                                 kMax = k;
                                 Emax = Ekh[k-1][khvPos];
                             }
                             if (Ekv[k-1][khvPos] >= Emax) {
                                 kMax = k;
                                 Emax = Ekv[k-1][khvPos];
                             }
                         }
                         if (coarsenessThreshold < 1.0) {
                             for (k = kMax+1; k <= 5; k++) {
                                 if (Ekh[k-1][khvPos] >= coarsenessThreshold*Emax) {
                                     kMax = k;
                                 }
                                 if (Ekv[k-1][khvPos] >= coarsenessThreshold*Emax) {
                                     kMax = k;
                                 }    
                             } // for (k = kMax+1; k <= 5; k++)
                         } // if (coarsenessThreshold < 1.0)
                         Sbest[khvPos] = Math.pow(2.0,kMax);
                         Fcoarseness += Sbest[khvPos];
                     } // for (x = 32; x <= xDim-32; x++)
                } // for (y = 32; y <= (yDim-32) && !threadStopped; y++)
                
                Fcoarseness = Fcoarseness/khvSliceSize;
                UI.setDataText("Slice number = " + z + " Coarseness = " + 
                        kDecimalFormat.format(Fcoarseness) + "\n");
                
            } // if (doCoarseness)
            
            if (doContrast) {
                sum = 0.0;
                for (y = 0; y < yDim; y++){
                    for (x = 0; x < xDim; x++) {
                        sum += sourceBuffer[x + y * xDim];    
                    } // for (x = 0; x < xDim; x++)
                } // for (y = 0; y < yDim; y++)
                mean = sum/sliceSize;
                
                squaredSum = 0.0;
                fourthSum = 0.0;
                for (y = 0; y < yDim; y++) {
                    for (x = 0; x < xDim; x++) {
                        diff = sourceBuffer[x + y * xDim] - mean;
                        squaredDiff = diff * diff;
                        squaredSum += squaredDiff;
                        fourthDiff = squaredDiff * squaredDiff;
                        fourthSum += fourthDiff;
                    } // for (x = 0; x < xDim; x++)
                } // for (y = 0; y < yDim; y++)
                variance = squaredSum/sliceSize;
                fourthCentralMoment = fourthSum/sliceSize;
                kurtosis = fourthCentralMoment/(variance * variance);
                standardDeviation = Math.sqrt(variance);
                Fcontrast = standardDeviation/Math.pow(kurtosis, 0.25);
                UI.setDataText("Slice number = " + z + " Contrast = " + 
                        kDecimalFormat.format(Fcontrast) + "\n");
            } // if (doContrast)
            
            if (doDirectionality) {
                for (y = 1; y <= yDim - 2; y++) {
                    for (x = 1; x <= xDim - 2; x++) {
                        pos = x + y * xDim; 
                        gradPos = (x - 1) + (y - 1) * (xDim - 2);
                        gradH[gradPos] = gradH[gradPos] + sourceBuffer[pos - xDim + 1] - sourceBuffer[pos - xDim - 1]
                                                        + sourceBuffer[pos  + 1] - sourceBuffer[pos - 1]
                                                        + sourceBuffer[pos + xDim + 1] - sourceBuffer[pos + xDim - 1];
                        gradV[gradPos] = gradV[gradPos] + sourceBuffer[pos - xDim - 1] - sourceBuffer[pos + xDim - 1]
                                                        + sourceBuffer[pos - xDim] - sourceBuffer[pos + xDim]
                                                        + sourceBuffer[pos - xDim + 1] - sourceBuffer[pos + xDim + 1];
                    } // for (x = 1; x <= xDim - 2; x++)
                } // for (y = 1; y <= yDim - 2; y++)
                
                if (z < zDim) {
                    for (i = 0; i < gradSize; i++) {
                        gradH[i] = 0.0;
                        gradV[i] = 0.0;
                    }
                }
            } // if (doDirectionality)
    
            if (threadStopped) {
                finalize();
    
                return;
            }
            
            
        } // for (z = 0; z < zDim; z++)
               
       
        setCompleted(true);

        return;    
    }

}
