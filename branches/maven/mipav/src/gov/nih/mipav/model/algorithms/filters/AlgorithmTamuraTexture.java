package gov.nih.mipav.model.algorithms.filters;


import gov.nih.mipav.model.algorithms.*;
import gov.nih.mipav.model.structures.*;

import gov.nih.mipav.view.*;

import java.io.*;
import java.text.DecimalFormat;

/**
 * DOCUMENT ME!
 *
 * @version  0.1 July 6, 2012
 * @author   William Gandler 

 * In the coarseness measurement a 32 wide pixel rim around the edge is excluded from the calculation.
 * The contrast measurement uses all the pixels.  The directionality measurement excludes a 1 wide
 * pixel rim around the edge.  All measurements are calculated for whole slice values and individual
 * pixel values.  Directionality uses the gradient filter for the whole slice result and the Sobel
 * filter for the pixel result.
 * 
 * References: 1.) "Textural Features Corresponding to Visual Perception" by Hideyuki Tamura, Shunji Mori,
 * and Takashi Yamawaki, IEEE Transactions on Systems, Man, and Cybernetics, Vol. SMC-8, No. 6, June, 1978,
 * pp. 460 - 473.
 * 
 * 2.) "A Relevance Feedback Retrieval Method Based on Tamura Texture" by Ya-Li Qi, 2009 Second International
 * Symposium on Knowledge Acquisition and Modeling, 2009, pp. 174-177.
 * 
 */
public class AlgorithmTamuraTexture extends AlgorithmBase {
    
    private ModelImage destImage[] = null;
    
    /** Red channel. */
    private static final int RED_OFFSET = 1;
    
    /** coarsenessThreshold must be <= 1.0.
    If coarsenessThreshold < 1.0, select the largest k for which Ek >= coarsenessThreshold * Emax.
    If coarsenessThreshold = 1.0, select the largest k which gives Emax.
    */
    private double coarsenessThreshold = 1.0;
    
    private boolean doCoarseness;
    
    private boolean doContrast;
    
    // Contrast neighborhood size for individual pixel calculations, must be an odd integer
    private int cSize = 13;
    
    private boolean doDirectionality;
    
    private int histogramBins = 16;
    
    // Threshold for gradient magnitude for gradient angle to be included in histogram.
    private double histogramThreshold = 12.0;
    
    private int RGBOffset = RED_OFFSET; 

    //~ Constructors ---------------------------------------------------------------------------------------------------
    
   

    /**
     * Creates a new AlgorithmTamuraTexture object for black and white image.
     *
     * @param              destImage array
     * @param              srcImg             source image model
     * @param              doCoarseness
     * @param              coarsenessThreshold
     * @param              doContrast
     * @param              cSize
     * @param              doDirectionality
     * @param              histogramBins
     * @param              histogramThreshold
     */
    public AlgorithmTamuraTexture(ModelImage destImage[], ModelImage srcImg, boolean doCoarseness, double coarsenessThreshold,
                                  boolean doContrast, int cSize, boolean doDirectionality, int histogramBins,
                                  double histogramThreshold) {
        super(null, srcImg);
        this.destImage = destImage;
        this.doCoarseness = doCoarseness;
        this.coarsenessThreshold = coarsenessThreshold;
        this.doContrast = doContrast;
        this.cSize = cSize;
        this.doDirectionality = doDirectionality;
        this.histogramBins = histogramBins;
        this.histogramThreshold = histogramThreshold;
    }
    
    /**
     * Creates a new AlgorithmTamuraTexture object for color image.
     *
     * @param              destImage          array
     * @param              srcImg             source image model
     * @param              RGBOffset          selects red, green, or blue channel
     * @param              doCoarseness
     * @param              coarsenessThreshold
     * @param              doContrast
     * @param              cSize
     * @param              doDirectionality
     * @param              histogramBins
     * @param              histogramThreshold
     */
    public AlgorithmTamuraTexture(ModelImage destImage[], ModelImage srcImg, int RGBOffset, boolean doCoarseness,
            double coarsenessThreshold, boolean doContrast, int cSize, boolean doDirectionality, int histogramBins,
                                  double histogramThreshold) {
        super(null, srcImg);
        this.destImage = destImage;
        this.RGBOffset = RGBOffset;
        this.doCoarseness = doCoarseness;
        this.coarsenessThreshold = coarsenessThreshold;
        this.doContrast = doContrast;
        this.cSize = cSize;
        this.doDirectionality = doDirectionality;
        this.histogramBins = histogramBins;
        this.histogramThreshold = histogramThreshold;
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
        double[] buffer = new double[sliceSize];
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
        int cHalf = cSize/2;
        int count;
        double gradH;
        double gradV;
        double delG[] = null;
        double angle;
        double theta[] = null;
        int gradSize = (xDim-2)*(yDim-2);
        int pos;
        int gradPos;
        int totalHistogramCount;
        int bin[] = null;
        int binNumber;
        double HD[] = null;
        // -1 for valley, +1 for peak, 0 otherwise
        int pv[] = null;
        int lastValley;
        double Fdirectionality;
        int idiff;
        boolean found;
        int destImageIndex = 0;
        int coarsenessIndex = 0;
        int contrastIndex = 0;
        int directionalityIndex = 0;
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
            coarsenessIndex = destImageIndex++;
        } // if (doCoarseness)
        
        if (doContrast) {
            contrastIndex = destImageIndex++;
        }
        
        if (doDirectionality) {
            delG = new double[gradSize];
            theta = new double[gradSize];
            bin = new int[histogramBins];
            HD = new double[histogramBins];
            pv = new int[histogramBins];
            directionalityIndex = destImageIndex++;
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
                         pos = x + y*xDim;
                         buffer[pos] = Math.pow(2.0,kMax);
                         Fcoarseness += buffer[pos];
                     } // for (x = 32; x <= xDim-32; x++)
                } // for (y = 32; y <= (yDim-32) && !threadStopped; y++)
                
                
                if (z < zDim - 1) {
                    try {
                        destImage[coarsenessIndex].importData(z*sliceSize, buffer, false);
                    }
                    catch (IOException error) {
                        MipavUtil.displayError("AlgorithmTamuraTexture: IOException on destImage[" + coarsenessIndex +
                                               "].importData(z*sliceSize, buffer,false)");
                        setCompleted(false);
        
                        return;
                    }
                }
                else {
                    try {
                        destImage[coarsenessIndex].importData(z*sliceSize, buffer, true);
                    }
                    catch (IOException error) {
                        MipavUtil.displayError("AlgorithmTamuraTexture: IOException on destImage[" + coarsenessIndex +
                                               "].importData(z*sliceSize, buffer,true)");
                        setCompleted(false);
        
                        return;
                    }
                }
                
                for (i = 0; i < sliceSize; i++) {
                    buffer[i] = 0;
                }
                
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
                
                for (y = 0; y < yDim; y++) {
                    for (x = 0; x < xDim; x++) {
                        sum = 0.0;
                        count = 0;
                        for (j = Math.max(0,y-cHalf); j <= Math.min(yDim-1,y + cHalf); j++) {
                            for (i = Math.max(0,x-cHalf); i <= Math.min(xDim-1,x+cHalf); i++) {
                                sum += sourceBuffer[i + j * xDim];  
                                count++;
                            } // for (i = Math.max(0,x-cHalf); i <= Math.min(xDim-1,x+cHalf); i++)
                        } // for (j = Math.max(0,y-cHalf); j <= Math.min(yDim-1,y + cHalf); j++)
                        mean = sum/count;
                        
                        squaredSum = 0.0;
                        fourthSum = 0.0;
                        for (j = Math.max(0,y-cHalf); j <= Math.min(yDim-1,y + cHalf); j++) {
                            for (i = Math.max(0,x-cHalf); i <= Math.min(xDim-1,x+cHalf); i++) {
                                diff = sourceBuffer[i + j * xDim] - mean;
                                squaredDiff = diff * diff;
                                squaredSum += squaredDiff;
                                fourthDiff = squaredDiff * squaredDiff;
                                fourthSum += fourthDiff;
                            } // for (i = Math.max(0,x-cHalf); i <= Math.min(xDim-1,x+cHalf); i++)
                        } // for (j = Math.max(0,y-cHalf); j <= Math.min(yDim-1,y + cHalf); j++)
                        variance = squaredSum/count;
                        fourthCentralMoment = fourthSum/count;
                        kurtosis = fourthCentralMoment/(variance * variance);
                        standardDeviation = Math.sqrt(variance);
                        Fcontrast = standardDeviation/Math.pow(kurtosis, 0.25);
                        buffer[x + y * xDim] = Fcontrast;
                    } // for (x = 0; x < xDim; x++)
                } // for (y = 0; y < yDim; y++)
                
                if (z < zDim - 1) {
                    try {
                        destImage[contrastIndex].importData(z*sliceSize, buffer, false);
                    }
                    catch (IOException error) {
                        MipavUtil.displayError("AlgorithmTamuraTexture: IOException on destImage[" + contrastIndex +
                                               "].importData(z*sliceSize, buffer,false)");
                        setCompleted(false);
        
                        return;
                    }
                }
                else {
                    try {
                        destImage[contrastIndex].importData(z*sliceSize, buffer, true);
                    }
                    catch (IOException error) {
                        MipavUtil.displayError("AlgorithmTamuraTexture: IOException on destImage[" + contrastIndex +
                                               "].importData(z*sliceSize, buffer,true)");
                        setCompleted(false);
        
                        return;
                    }
                }
                
                for (i = 0; i < sliceSize; i++) {
                    buffer[i] = 0;
                }
            } // if (doContrast)
            
            if (doDirectionality) {
                for (y = 1; y <= yDim - 2; y++) {
                    for (x = 1; x <= xDim - 2; x++) {
                        pos = x + y * xDim; 
                        gradPos = (x - 1) + (y - 1) * (xDim - 2);
                        // Gradient filters for whole image results
                        gradH = sourceBuffer[pos - xDim + 1] - sourceBuffer[pos - xDim - 1]
                              + sourceBuffer[pos  + 1] - sourceBuffer[pos - 1]
                              + sourceBuffer[pos + xDim + 1] - sourceBuffer[pos + xDim - 1];
                        gradV = sourceBuffer[pos + xDim - 1] - sourceBuffer[pos - xDim - 1]
                              + sourceBuffer[pos + xDim] - sourceBuffer[pos - xDim]
                              + sourceBuffer[pos + xDim + 1] - sourceBuffer[pos - xDim + 1];
                        delG[gradPos] = (Math.abs(gradH) + Math.abs(gradV))/2.0;
                        // tan(x + PI) = tan(x)
                        // tan(x - PI) = tan(x)
                        // atan2 goes from -PI to PI
                        // Wish to put into atan -PI/2 to PI/2 range
                        angle = Math.atan2(gradV, gradH);
                        if (angle < -Math.PI/2.0) {
                            angle = angle + Math.PI;
                        }
                        else if (angle > Math.PI/2.0) {
                            angle = angle - Math.PI;
                        }
                        // Now put into theta 0 to PI range by adding PI/2.0
                        theta[gradPos] = angle + Math.PI/2.0;
                        
                        // Sobel filters for pixel results
                        gradH = sourceBuffer[pos - xDim + 1] - sourceBuffer[pos - xDim - 1]
                              + 2.0*sourceBuffer[pos  + 1] - 2.0*sourceBuffer[pos - 1]
                              + sourceBuffer[pos + xDim + 1] - sourceBuffer[pos + xDim - 1];
                        gradV = sourceBuffer[pos + xDim - 1] - sourceBuffer[pos - xDim - 1]
                              + 2.0*sourceBuffer[pos + xDim] - 2.0*sourceBuffer[pos - xDim]
                              + sourceBuffer[pos + xDim + 1] - sourceBuffer[pos - xDim + 1];
                        delG[gradPos] = (Math.abs(gradH) + Math.abs(gradV))/2.0;
                        // tan(x + PI) = tan(x)
                        // tan(x - PI) = tan(x)
                        // atan2 goes from -PI to PI
                        // Wish to put into atan -PI/2 to PI/2 range
                        angle = Math.atan2(gradV, gradH);
                        if (angle < -Math.PI/2.0) {
                            angle = angle + Math.PI;
                        }
                        else if (angle > Math.PI/2.0) {
                            angle = angle - Math.PI;
                        }
                        // Now put into theta 0 to PI range by adding PI/2.0
                        buffer[pos] = angle + Math.PI/2.0;
                    } // for (x = 1; x <= xDim - 2; x++)
                } // for (y = 1; y <= yDim - 2; y++)
                
                if (z < zDim - 1) {
                    try {
                        destImage[directionalityIndex].importData(z*sliceSize, buffer, false);
                    }
                    catch (IOException error) {
                        MipavUtil.displayError("AlgorithmTamuraTexture: IOException on destImage[" + directionalityIndex +
                                               "].importData(z*sliceSize, buffer,false)");
                        setCompleted(false);
        
                        return;
                    }
                }
                else {
                    try {
                        destImage[directionalityIndex].importData(z*sliceSize, buffer, true);
                    }
                    catch (IOException error) {
                        MipavUtil.displayError("AlgorithmTamuraTexture: IOException on destImage[" + directionalityIndex +
                                               "].importData(z*sliceSize, buffer,true)");
                        setCompleted(false);
        
                        return;
                    }
                }
                
                for (i = 0; i < sliceSize; i++) {
                    buffer[i] = 0;
                }
                
                totalHistogramCount = 0;
                for (i = 0; i < gradSize; i++) {
                    if (delG[i] >= histogramThreshold) {
                        totalHistogramCount++;
                        binNumber = (int)(theta[i]/(Math.PI/histogramBins));
                        if (binNumber == histogramBins) {
                            // Set PI angles to 0.
                            binNumber = 0;
                        }
                        bin[binNumber]++;
                    } // if (delG[i] >= histogramThreshold)
                } // for (i = 0; i < gradSize; i++)
                
                for (i = 0; i < histogramBins; i++) {
                    HD[i] = ((double)bin[i]/(double)totalHistogramCount);
                }
                
                if (z < zDim - 1) {
                    for (i = 0; i < histogramBins; i++) {
                        bin[i] = 0;
                    }
                } // if (z < zDim - 1)
                
                if (bin[0] > bin[1]) {
                    pv[0] = 1;
                }
                else if (bin[0] < bin[1]) {
                    pv[0] = -1;
                }
                if (bin[histogramBins-1] > bin[histogramBins-2]) {
                    pv[histogramBins-1] = 1;
                }
                else if (bin[histogramBins-1] < bin[histogramBins-2]) {
                    pv[histogramBins-1] = -1;
                }
                for (i = 1; i < histogramBins-1; i++) {
                    if ((bin[i] > bin[i-1]) && (bin[i] > bin[i+1])) {
                        pv[i] = 1;
                    }
                    else if ((bin[i] < bin[i-1]) && (bin[i] < bin[i+1]))  
                        pv[i] = -1;
                }
                
                lastValley = Integer.MAX_VALUE;
                Fdirectionality = 0;
                for (i = 0; i < histogramBins; i++) {
                    if (pv[i] == 1) {
                       for (j = lastValley; j < i; j++) {
                           idiff = i - j;
                           Fdirectionality += idiff*idiff*HD[j];     
                       }
                       found = false;
                       for (j = i+1; (j < histogramBins)  && (!found); j++) {
                           idiff = i - j;
                           Fdirectionality += idiff*idiff*HD[j];
                           if (pv[j] == -1) {
                               found = true;
                               lastValley = j;
                               i = j;
                           }
                       }
                    }
                    else if (pv[i] == -1) {
                        lastValley = i;
                    }
                } // for (i = 0; i < histogramBins; i++)
                
                if (z < zDim - 1) {
                    for (i = 0; i < histogramBins; i++) {
                        pv[i] = 0;
                    }
                } // if (z < zDim - 1)
                
                UI.setDataText("Slice number = " + z + " Directionality = " + 
                        kDecimalFormat.format(Fdirectionality) + "\n");
                
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
