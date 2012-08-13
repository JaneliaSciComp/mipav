package gov.nih.mipav.model.algorithms.filters;


import gov.nih.mipav.model.algorithms.AlgorithmBase;
import gov.nih.mipav.model.structures.*;

import gov.nih.mipav.view.MipavUtil;
import gov.nih.mipav.view.Preferences;

import java.io.IOException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.Comparator;
import java.util.List;

import de.jtem.numericalMethods.algebra.linear.decompose.Eigenvalue;

/** 
 * This code is ported from the MATLAB source code of the Rice Wavelet tools, version 2.4.
 * Below is the license for the original source code.
 */

/**This "rice-wlet-tools", version 2.4
Released - <Dec 1 2002>

CONDITIONS FOR USE:
Copyright (c) 2000 RICE UNIVERSITY. All rights reserved.

This software is distributed and licensed to you on a non-exclusive 
basis, free-of-charge. Redistribution and use in source and binary forms, 
with or without modification, are permitted provided that the following 
conditions are met:

1. Redistribution of source code must retain the above copyright notice, 
   this list of conditions and the following disclaimer.
2. Redistribution in binary form must reproduce the above copyright notice, 
   this list of conditions and the following disclaimer in the 
   documentation and/or other materials provided with the distribution.
3. All advertising materials mentioning features or use of this software 
   must display the following acknowledgment: This product includes 
   software developed by Rice University, Houston, Texas and its contributors.
4. Neither the name of the University nor the names of its contributors 
   may be used to endorse or promote products derived from this software 
   without specific prior written permission.

THIS SOFTWARE IS PROVIDED BY WILLIAM MARSH RICE UNIVERSITY, HOUSTON, TEXAS, 
AND CONTRIBUTORS AS IS AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, 
BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS 
FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL RICE UNIVERSITY 
OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, 
EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, 
PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; 
OR BUSINESS INTERRUPTIONS) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, 
WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR 
OTHERWISE), PRODUCT LIABILITY, OR OTHERWISE ARISING IN ANY WAY OUT OF THE 
USE OF THIS SOFTWARE,  EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

For information on commercial licenses, contact Rice University's Office of 
Technology Transfer at techtran@rice.edu or (713) 348-6173*/



public class AlgorithmRiceWaveletTools extends AlgorithmBase {
    
    public static final int MINIMUM_PHASE = 1;
    
    public static final int MID_PHASE = 2;
    
    public static final int MAXIMUM_PHASE = 3;
    
    private int filterLength;
    
    private int filterType = MINIMUM_PHASE;
    
    private double[] scalingFilter;
    
    private double[] waveletFilter;
    
    private int nDims;
    
    private int extents[];
    
    private int xDim;
    
    private int yDim;
    
    private int sliceSize;
    
    private int zDim;
    
    private int arrayLength;
    
    private double aArray[];
    
    private int error = 0;
    
    private int numberOfLevels;
    
    private boolean doWaveletImages;
    
    private ModelImage[] waveletImage = null;
    
    // Low pass wavelet component
    private double yl[] = null;
    // Save low pass wavelet component for each level
    private double llA[][] = null;
    // Create 3 high pass wavelet components for each level
    private double lhA[][] = null;
    private double hlA[][] = null;
    private double hhA[][] = null;
    private int minimumLevel = 1;
    private int maximumLevel;
    private boolean selfTest = false;
    // Given by mrdwt.m for Leopold, signal length 8, Daubechies' length 4, minimum phase, number of levels 1
    // Cannot reproduce but mrdwt and mirdwt return original picture and 1D signals.
    //private double ylmrdwtm[] = new double[]{0.8365,0.4830, 0.0, 0.0, 0.0, 0.0, -0.1294, 0.2241};
    //private double yhmrdwtm[] = new double[]{-0.2241, -0.1294, 0.0, 0.0, 0.0, 0.0, -0.4830, 0.8365};
    // I get:
    /*      Calculated yl[0] = 0.4182581518689039
            Calculated yh[0] = 0.4182581518689039
            Calculated yl[1] = 0.24148145657226708
            Calculated yh[1] = 0.24148145657226708
            Calculated yl[2] = 0.24148145657226708
            Calculated yh[2] = -0.24148145657226708
            Calculated yl[3] = -0.4182581518689039
            Calculated yh[3] = 0.4182581518689039
            Calculated yl[4] = 0.1120719340210067
            Calculated yh[4] = -0.1120719340210067
            Calculated yl[5] = 0.06470476127563018
            Calculated yh[5] = -0.06470476127563018
            Calculated yl[6] = -0.06470476127563018
            Calculated yh[6] = -0.06470476127563018
            Calculated yl[7] = 0.1120719340210067
            Calculated yh[7] = 0.1120719340210067 */
    
    public AlgorithmRiceWaveletTools(ModelImage destImg, ModelImage srcImg, int filterLength,
            int numberOfLevels, boolean doWaveletImages, int minimumLevel, int maximumLevel) {
        super(destImg, srcImg);
        this.filterLength = filterLength;
        this.numberOfLevels = numberOfLevels;
        this.doWaveletImages = doWaveletImages;
        this.minimumLevel = minimumLevel;
        this.maximumLevel = maximumLevel;
    }
    
    public AlgorithmRiceWaveletTools(ModelImage srcImg, int filterLength, int numberOfLevels,
            boolean doWaveletImages, int minimumLevel, int maximumLevel) {
        super(null, srcImg);
        this.filterLength = filterLength;
        this.numberOfLevels = numberOfLevels;
        this.doWaveletImages = doWaveletImages;
        this.minimumLevel = minimumLevel;
        this.maximumLevel = maximumLevel;
    }
    
    
    public void runAlgorithm() {
        int i, j, k;
        int divisor;
        
        if (srcImage == null) {
            displayError("Source Image is null");

            return;
        }
        
        if (selfTest) {
            nDims = 1;
            xDim = 8;
            yDim = 1;
            sliceSize = 8;
            filterLength = 4;
            numberOfLevels = 1;
            makeSig("Leopold",8);
        }
        else {
        
            nDims = srcImage.getNDims();
            extents = srcImage.getExtents();
            xDim = extents[0];
            if ((xDim % 2) == 1) {
                MipavUtil.displayError("1 level of decomposition requires an even xDim");
                setCompleted(false);
                return;
            }
            yDim = extents[1];
            if ((yDim % 2) == 1) {
                MipavUtil.displayError("1 level of decomposition requires an even yDim");
                setCompleted(false);
                return;
            }
            sliceSize = xDim * yDim;
            
            if ((filterLength % 2) == 1) {
                displayError("No Daubechies filter exists for odd length");
                setCompleted(false);
                return;
            }
            
            if (numberOfLevels <= 0) {
                MipavUtil.displayError("A wavelet decomposition requires number of levels >= 1");
                setCompleted(false);
            }
            
            if (numberOfLevels < Integer.MAX_VALUE) {
                divisor = 1;
                for (i = 1; i <= numberOfLevels; i++) {
                    divisor *= 2;
                }
                if ((xDim % divisor) != 0) {
                    MipavUtil.displayError("Error!  xDim mod " + divisor + " does not equal 0");
                    setCompleted(false);
                    return;
                }
                
                if ((yDim % divisor) != 0) {
                    MipavUtil.displayError("Error!  yDim mod " + divisor + " does not equal 0");
                    setCompleted(false);
                    return;
                }
            } // if (numberOfLevels < Integer.MAX_VALUE);
            else { // Calculate maximum possible number of levels
                i = xDim;
                j = 0;
                while ((i % 2) == 0) {
                    i = (i >> 1);
                    j++;
                }
                k = yDim;
                i = 0;
                while((k % 2) == 0) {
                    k = (k >> 1);
                    i++;
                }
                
                numberOfLevels = Math.min(i, j);
                Preferences.debug("The maximum possible number of levels = " + numberOfLevels + "\n", Preferences.DEBUG_FILEIO);
            } // else calculate maximum possible number of levels
    
            if (nDims > 2) {
                zDim = extents[2];
            }
    
            arrayLength = 1;
    
            for (i = 0; i < nDims; i++) {
                arrayLength *= extents[i];
            }
    
            try {
                aArray = new double[arrayLength];
            } catch (final OutOfMemoryError e) {
                aArray = null;
                System.gc();
                displayError("AlgorithmRiceWaveletTools: Out of memory creating a");
    
                setCompleted(false);
    
                return;
            }
    
            try {
                srcImage.exportData(0, arrayLength, aArray);
            } catch (final IOException error) {
                displayError("AlgorithmRiceWaveletTools: Source image is locked");
    
                setCompleted(false);
    
                return;
            }
        } // else
        
        scalingFilter = new double[filterLength];
        
        waveletFilter = new double[filterLength];
        
        daubcqf();
        Preferences.debug("Scaling filter:\n", Preferences.DEBUG_ALGORITHM);
        for (j = 0; j < filterLength; j++) {
            Preferences.debug(scalingFilter[j] + "\n", Preferences.DEBUG_ALGORITHM);
        }
        Preferences.debug("Wavelet filter:\n", Preferences.DEBUG_ALGORITHM);
        for (j = 0; j < filterLength; j++) {
            Preferences.debug(waveletFilter[j] + "\n", Preferences.DEBUG_ALGORITHM);
        }
        if (error == -1) {
            setCompleted(false);
            return;
        }
        
        // Create low pass wavelet component
        yl = new double[sliceSize];
        // Savethe low pass component for each level
        llA = new double[numberOfLevels-1][sliceSize];
        
        // Create 3 high pass components for each level
        lhA = new double[numberOfLevels][sliceSize];
        hlA = new double[numberOfLevels][sliceSize];
        hhA = new double[numberOfLevels][sliceSize];
        if (doWaveletImages) {
            waveletImage = new ModelImage[4*numberOfLevels+4];
        }
        else {
            waveletImage = new ModelImage[4];
        }
        
        mrdwt();
        
        if (selfTest) {
            for (i = 0; i < xDim; i++)  {
                Preferences.debug("aArray[" + i + "] = " + aArray[i] + "\n", Preferences.DEBUG_FILEIO);
                Preferences.debug("Calculated yl["+i+"] = " + yl[i] + "\n", Preferences.DEBUG_FILEIO);
                Preferences.debug("Calculated yh["+i+"] = " + lhA[0][i] + "\n", Preferences.DEBUG_FILEIO);
            }
        }
        
        mirdwt();
        if (selfTest) {
            for (i = 0; i < xDim; i++)  {
                Preferences.debug("aArray[" + i + "] = " + aArray[i] + "\n", Preferences.DEBUG_FILEIO);
            }
            setCompleted(false);
            return;
        }
        
        try {
            destImage.importData(0, aArray, true);
        }
        catch(IOException e) {
            MipavUtil.displayError("IOException on destImage.importData(0, aArray, true)");
            setCompleted(false);
            return;
        }
        
        setCompleted(true);
        return;
        
    }
    
    private void makeSig(String sigName, int signalLength) {
        int i;
        double t[] = new double[signalLength];
        aArray = new double[signalLength];
        for ( i = 1; i <= signalLength; i++) {
            t[i-1] = (double)i/(double)signalLength;    
        }
        if (sigName.equalsIgnoreCase("Leopold")) { // Kronecker
            for (i = 0; i < signalLength; i++) {
                if (t[i] == Math.floor(0.37 * signalLength)/signalLength) {
                    aArray[i] = 1.0;
                }
                else {
                    aArray[i] = 0.0;
                }
            }
        }
    }
    
    private void mrdwt() {
        int i;
        int lh;
        double h0[];
        double h1[];
        int actual_yDim;
        int actual_xDim;
        int sample_f;
        int actual_L;
        double xdummyl[];
        double xdummyh[];
        double ydummyll[];
        double ydummylh[];
        double ydummyhl[];
        double ydummyhh[];
        int maxmn;
        int n_cb;
        int ir;
        int n_c;
        int ic;
        int n_rb;
        int n_r;
        int waveletImageIndex = 0;
        
        lh = 2 * filterLength;
        h0 = new double[lh];
        h1 = new double[lh];
        for (i = 0; i < filterLength; i++) {
            h0[i] = waveletFilter[filterLength - i - 1]/2;
            h1[i] = scalingFilter[i]/2;
        }
        for (i = 0; i < filterLength; i++) {
            h0[filterLength + i] = scalingFilter[filterLength - i - 1]/2;
            h1[filterLength + i] = waveletFilter[i]/2;
        }
        
        for (i = 0; i < lh; i += 2) {
            h1[i] = -h1[i];
        }
        
        maxmn = Math.max(xDim, yDim);
        xdummyl = new double[maxmn + lh - 1];
        xdummyh = new double[maxmn + lh - 1];
        ydummyll = new double[maxmn];
        ydummylh = new double[maxmn];
        ydummyhl = new double[maxmn];
        ydummyhh = new double[maxmn];
        
        actual_yDim = 2*yDim;
        actual_xDim = 2*xDim;
        for (i = 0; i < sliceSize; i++) {
            yl[i] = aArray[i];
        }
        
        /* main loop */
        sample_f = 1;
        for (actual_L = 1; actual_L <= numberOfLevels; actual_L++) {
            actual_yDim = actual_yDim/2;
            actual_xDim = actual_xDim/2;
            
            /* go by rows */
            n_cb = xDim/actual_xDim;   /* # of column blocks per row */
            for (ir = 0; ir < yDim; ir++) {  /* loop over rows */
                for (n_c = 0; n_c < n_cb; n_c++) {  /* loop within one row */
                    /* store in dummy variable */ 
                    ic = -sample_f + n_c;
                    for (i = 0; i < actual_xDim; i++) {
                        ic = ic + sample_f;
                        xdummyl[i] = yl[ic + ir * xDim];
                    } // for (i = 0; i < actual_xDim; i++)
                    /* perform the filtering lowpass/highpass */
                    fpconv(xdummyl, actual_xDim, h0, h1, lh, ydummyll, ydummyhh);
                    /* restore dummy variables in matrices */
                    ic = -sample_f + n_c;
                    for (i = 0; i < actual_xDim; i++) {
                        ic = ic + sample_f;
                        yl[ic + ir * xDim] = ydummyll[i];
                        lhA[actual_L-1][ic + ir * xDim] = ydummyhh[i];
                    } // for (i = 0; i < actual_xDim; i++)
                } // for (n_c = 0; n_c < n_cb; n_c++)
            } // for (ir = 0; ir < yDim; ir++)
            
            /* go by columns in case of a 2D signal */
            if (yDim > 1) {
                n_rb = yDim/actual_yDim;  /* # of row blocks per column */
                for (ic = 0; ic < xDim; ic++) { /* loop over column */
                    for (n_r = 0; n_r < n_rb; n_r++) { /* loop within one column */
                        /* store in dummy variables */
                        ir = -sample_f + n_r;
                        for (i = 0; i < actual_yDim; i++) {
                            ir = ir + sample_f;
                            xdummyl[i] = yl[ic + ir * xDim];
                            xdummyh[i] = lhA[actual_L-1][ic + ir * xDim];
                        } // for (i = 0; i < actual_yDim; i++)
                        /* perform filtering, first LL/LH, then HL/HH */
                        fpconv(xdummyl, actual_yDim, h0, h1, lh, ydummyll, ydummylh);
                        fpconv(xdummyh, actual_yDim, h0, h1, lh, ydummyhl, ydummyhh);
                        /* restore dummy variables in arrays */
                        ir = -sample_f + n_r;
                        for (i = 0; i < actual_yDim; i++) {
                            ir = ir + sample_f; 
                            yl[ic + ir * xDim] = ydummyll[i];
                            lhA[actual_L-1][ic + ir * xDim] = ydummylh[i];
                            hlA[actual_L-1][ic + ir * xDim] = ydummyhl[i];
                            hhA[actual_L-1][ic + ir * xDim] = ydummyhh[i];
                        } // for (i = 0; i < actual_yDim; i++)
                    } // for (n_r = 0; n_r < n_rb; n_r++)
                } // for (ic = 0; ic < xDim; ic++)
                
                if (actual_L < numberOfLevels) {
                    for (i = 0; i < sliceSize; i++) {
                        llA[actual_L-1][i] = yl[i];
                    }
                }
                
                if ((actual_L > minimumLevel) && (actual_L <= maximumLevel) && (maximumLevel > minimumLevel)) {
                    for (i = 0; i < sliceSize; i++) {
                        llA[minimumLevel-1][i] *= yl[i];
                        lhA[minimumLevel-1][i] *= lhA[actual_L-1][i];
                        hlA[minimumLevel-1][i] *= hlA[actual_L-1][i];
                        hhA[minimumLevel-1][i] *= hhA[actual_L-1][i];
                    }
                }
                
                if ((actual_L == maximumLevel) && (maximumLevel > minimumLevel)) {
                    waveletImage[waveletImageIndex] = new ModelImage(ModelStorageBase.DOUBLE, extents,srcImage.getImageName() + "_Mll");  
                    try {
                        waveletImage[waveletImageIndex++].importData(0, llA[minimumLevel-1], true);  
                    }
                    catch(IOException  e) {
                        MipavUtil.displayError("IOException on waveletImage[waveletImageIndex++].importData(0, llA[minimumLevel-1], true)");
                        setCompleted(false);
                        return;
                    }    
                    waveletImage[waveletImageIndex] = new ModelImage(ModelStorageBase.DOUBLE, extents,srcImage.getImageName() + "_Mlh");  
                    try {
                        waveletImage[waveletImageIndex++].importData(0, lhA[minimumLevel-1], true);  
                    }
                    catch(IOException  e) {
                        MipavUtil.displayError("IOException on waveletImage[waveletImageIndex++].importData(0, lhA[minimumLevel-1], true)");
                        setCompleted(false);
                        return;
                    }
                    
                    waveletImage[waveletImageIndex] = new ModelImage(ModelStorageBase.DOUBLE, extents,srcImage.getImageName() + "_Mhl");  
                    try {
                        waveletImage[waveletImageIndex++].importData(0, hlA[minimumLevel-1], true);  
                    }
                    catch(IOException  e) {
                        MipavUtil.displayError("IOException on waveletImage[waveletImageIndex++].importData(0, hlA[minimumLevel-1], true)");
                        setCompleted(false);
                        return;
                    }
                    
                    waveletImage[waveletImageIndex] = new ModelImage(ModelStorageBase.DOUBLE, extents,srcImage.getImageName() + "_Mhh");  
                    try {
                        waveletImage[waveletImageIndex++].importData(0, hhA[minimumLevel-1], true);  
                    }
                    catch(IOException  e) {
                        MipavUtil.displayError("IOException on waveletImage[waveletImageIndex++].importData(0, hhA[minimumLevel-1], true)");
                        setCompleted(false);
                        return;
                    }    
                }
                
                if (doWaveletImages) {
                    
                    waveletImage[waveletImageIndex] = new ModelImage(ModelStorageBase.DOUBLE, extents,srcImage.getImageName() + "_L" +
                            actual_L + "_ll");  
                    try {
                        waveletImage[waveletImageIndex++].importData(0, yl, true);  
                    }
                    catch(IOException  e) {
                        MipavUtil.displayError("IOException on waveletImage[waveletImageIndex++].importData(0, yl, true)");
                        setCompleted(false);
                        return;
                    }    
                    waveletImage[waveletImageIndex] = new ModelImage(ModelStorageBase.DOUBLE, extents,srcImage.getImageName() + "_L" +
                                                                       actual_L + "_lh");  
                    try {
                        waveletImage[waveletImageIndex++].importData(0, lhA[actual_L-1], true);  
                    }
                    catch(IOException  e) {
                        MipavUtil.displayError("IOException on waveletImage[waveletImageIndex++].importData(0, lhA[actual_L-1], true)");
                        setCompleted(false);
                        return;
                    }
                    
                    waveletImage[waveletImageIndex] = new ModelImage(ModelStorageBase.DOUBLE, extents,srcImage.getImageName() + "_L" +
                            actual_L + "_hl");  
                    try {
                        waveletImage[waveletImageIndex++].importData(0, hlA[actual_L-1], true);  
                    }
                    catch(IOException  e) {
                        MipavUtil.displayError("IOException on waveletImage[waveletImageIndex++].importData(0, hlA[actual_L-1], true)");
                        setCompleted(false);
                        return;
                    }
                    
                    waveletImage[waveletImageIndex] = new ModelImage(ModelStorageBase.DOUBLE, extents,srcImage.getImageName() + "_L" +
                            actual_L + "_hh");  
                    try {
                        waveletImage[waveletImageIndex++].importData(0, hhA[actual_L-1], true);  
                    }
                    catch(IOException  e) {
                        MipavUtil.displayError("IOException on waveletImage[waveletImageIndex++].importData(0, hhA[actual_L-1], true)");
                        setCompleted(false);
                        return;
                    }
                    
                } // if (doWaveletImages)
            } // if (yDim > 1)
            sample_f = sample_f*2;
        } // for (actual_L = 1; actual_L <= numberOfLevels; actual_L++)
        
    } // mrdwt
    
    private void fpconv(double x_in[], int lx, double h0[], double h1[], int lh,
                        double x_outl[], double x_outh[]) {
        int i,j;
        double x0, x1;
        
        for (i = lx; i < lx + lh - 1; i++) {
            x_in[i] = x_in[i-lx];
        }
        
        for (i = 0; i < lx; i++) {
            x0 = 0;
            x1 = 0;
            for (j = 0; j < lh; j++) {
                x0 = x0 + x_in[j+i]*h0[lh-1-j];
                x1 = x1 + x_in[j+i]*h1[lh-1-j];
            } // for (j = 0; j < lh; j++)
            x_outl[i] = x0;
            x_outh[i] = x1;
        } // for (i = 0; i < lx; i++)
    } // fpconv
    
    public void mirdwt() {
        int i;
        int lh;
        double g0[];
        double g1[];
        double xh[];
        double xdummyl[];
        double xdummyh[];
        double ydummyll[];
        double ydummylh[];
        double ydummyhl[];
        double ydummyhh[];
        int maxmn;
        int lhm1;
        int sample_f;
        int actual_xDim;
        int actual_yDim;
        int actual_L;
        int n_rb;
        int ic;
        int n_r;
        int ir;
        int n_cb;
        int n_c;
        
        lh = 2 * filterLength;
        g0 = new double[lh];
        g1 = new double[lh];
        
        /* analysis lowpass and highpass */
        for (i = 0; i < filterLength; i++) {
            g0[i] = scalingFilter[i]/2;
            g1[i] = waveletFilter[filterLength - i - 1]/2;
        }
        for (i = 0; i < filterLength; i++) {
            g0[filterLength + i] = waveletFilter[i]/2;
            g1[filterLength + i] = scalingFilter[filterLength - i - 1]/2;
        }
        
        for (i = 1; i < lh; i += 2) {
            g1[i] = -g1[i];
        }
        
        maxmn = Math.max(xDim, yDim);
        xh = new double[sliceSize];
        xdummyl = new double[maxmn];
        xdummyh = new double[maxmn];
        ydummyll = new double[maxmn+lh-1];
        ydummylh = new double[maxmn+lh-1];
        ydummyhl = new double[maxmn+lh-1];
        ydummyhh = new double[maxmn+lh-1];
        
        lhm1 = lh - 1;
        /* 2^L */
        sample_f = 1;
        for (i = 1; i < numberOfLevels; i++) {
            sample_f = sample_f * 2;
        }
        actual_yDim = yDim/sample_f;
        actual_xDim = xDim/sample_f;
        /* Restore yl in aArray */
        for (i = 0; i < sliceSize; i++) {
            aArray[i] = yl[i];
        }
        
        /* main loop */
        for (actual_L = numberOfLevels; actual_L >= 1; actual_L--) {
            /* Go by columns in case of a 2D signal*/
            if (yDim > 1) {
                n_rb = yDim/actual_yDim;   /* # of row blocks per column */
                for (ic = 0; ic < xDim; ic++) {    /* loop over column */
                    for (n_r = 0; n_r < n_rb; n_r++) {    /* loop within one column */
                        /* store in dummy variables */
                        ir = -sample_f + n_r;
                        for (i = 0; i < actual_yDim; i++) {
                            ir = ir + sample_f; 
                            ydummyll[i+lhm1] = aArray[ic + ir * xDim];
                            ydummylh[i+lhm1] = lhA[actual_L-1][ic + ir * xDim];
                            ydummyhl[i+lhm1] = hlA[actual_L-1][ic + ir * xDim];
                            ydummyhh[i+lhm1] = hhA[actual_L-1][ic + ir * xDim];
                        } // for (i = 0; i < actual_yDim; i++)
                        /* perform filtering and adding: first LL/LH, then HL/HH */
                        bpconv(xdummyl, actual_yDim, g0, g1, lh, ydummyll, ydummylh);
                        bpconv(xdummyh, actual_yDim, g0, g1, lh, ydummyhl, ydummyhh);
                        /* Store dummy variables in matrices */
                        ir = -sample_f + n_r;
                        for (i = 0; i < actual_yDim; i++) {
                            ir = ir + sample_f;
                            aArray[ic + ir * xDim] = xdummyl[i];
                            xh[ic + ir * xDim] = xdummyh[i];
                        } // for (i = 0; i < actual_yDim; i++)
                    } // for (n_r = 0; n_r < n_rb; n_r++)
                } // for (ic = 0; ic < xDim; ic++)
            } // if (yDim > 1)
            
            /* go by rows */
            n_cb = xDim/actual_xDim;       /* number of column blocks per row */
            for (ir = 0; ir < yDim; ir++) {    /* loop over rows */
                for (n_c = 0; n_c < n_cb; n_c++) { /* loop within one row */
                    /* store in dummy variable */
                    ic = -sample_f + n_c;
                    for (i = 0; i < actual_xDim; i++) {
                        ic = ic + sample_f;
                        ydummyll[i+lhm1] = aArray[ic + ir * xDim];
                        if (yDim > 1) {
                            ydummyhh[i+lhm1] = xh[ic + ir * xDim];
                        }
                        else {
                            ydummyhh[i+lhm1] = lhA[actual_L-1][ic + ir * xDim];
                        }
                    } // for (i = 0; i < actual_xDim; i++)
                    /* perform filtering lowpass/highpass */
                    bpconv(xdummyl, actual_xDim, g0, g1, lh, ydummyll, ydummyhh);
                    /* restore dummy variables in matrices */
                    ic = -sample_f + n_c;
                    for (i = 0; i < actual_xDim; i++) {
                        ic = ic + sample_f;
                        aArray[ic + ir * xDim] = xdummyl[i];
                    } // for (i = 0; i < actual_xDim; i++)
                } // for (n_c = 0; n_c < n_cb; n_c++)
            } // for (ir = 0; ir < yDim; ir++)
            sample_f = sample_f/2;
            actual_yDim = actual_yDim * 2;
            actual_xDim = actual_xDim * 2;
        } // for (actual_L = numberOfLevels; actual_L >= 1; actual_L--)
    } // mirdwt()
    
    private void bpconv(double x_out[], int lx, double g0[], double g1[], int lh, double x_inl[], double x_inh[]) {
        int i, j;
        double x0;
        
        for (i = lh-2; i > -1; i--) {
            x_inl[i] = x_inl[lx+i];
            x_inh[i] = x_inh[lx+i];
        }
        for (i = 0; i < lx; i++) {
            x0 = 0;
            for (j = 0; j < lh; j++) {
                x0 = x0 + x_inl[j+i]*g0[lh-1-j] + x_inh[j+i]*g1[lh-1-j];
            }
            x_out[i] = x0;
        }
    } // bpconv
    
    /**
     * Function computes the Daubechies' scaling and wavelet filters (normalized to sqrt(2)).
     * The user specifies an even number filter length and a minimum phase, mid phase, or 
     * maximum phase solution.
     * Reference: "Orthonormal Bases of Compactly Supported Wavelets", CPAM, October, 1989.
     * 
     * From the original source file:
     * %File Name: daubcqf.m
       %Last Modification Date: 01/02/96   15:12:57
       %Current Version: daubcqf.m 2.4
       %File Creation Date: 10/10/88
       %Author: Ramesh Gopinath  <ramesh@dsp.rice.edu>
       %
       %Copyright (c) 2000 RICE UNIVERSITY. All rights reserved.
       %Created by Ramesh Gopinath, Department of ECE, Rice University. 
       
       Correctly gives scaling and wavelet filters for filter length = 4 minimum phase:
       Scaling filter:
        0.48296291314453416
        0.8365163037378078
        0.2241438680420134
        -0.12940952255126037
        Wavelet filter:
        0.12940952255126037
        0.2241438680420134
        -0.8365163037378078
        0.48296291314453416
        
       Correctly gives scaling and wavelet filters for filter length = 6 minimum phase
        Scaling filter:
        0.3326705529500828
        0.8068915093110931
        0.459877502118492
        -0.1350110200102549
        -0.08544127388202716
        0.03522629188570937
        Wavelet filter:
        -0.03522629188570937
        -0.08544127388202716
        0.1350110200102549
        0.459877502118492
        -0.8068915093110931
        0.3326705529500828
     */
    private void daubcqf() {
        int j, m;
        int k = filterLength/2;
        double a = 1.0;
        double p[] = new double[]{1.0};
        double q[] = new double[]{1.0};
        double h_0[] = new double[]{1.0, 1.0};
        double oldp[];
        double oldq[];
        double oldh_0[];
        double A[][];
        double[] eigenvalueR;
        double[][] V;
        double[] eI;
        double[] qtR;
        double[] qtI;
        double[] polyqtR;
        double[] polyqtI;
        double[] w;
        int minj;
        int maxj;
        double sumw;
        double scale;
        double temp[];
        
        if (filterLength == 2) {
            if (filterType == MINIMUM_PHASE) {
                scalingFilter[0] = 1.0/Math.sqrt(2.0);
                scalingFilter[1] = 1.0/Math.sqrt(2.0);
                waveletFilter[0] = 1.0/Math.sqrt(2.0);
                waveletFilter[1] = -1.0/Math.sqrt(2.0);
            }
            return;
        } // if (filterLength == 2)
        
        
        for (j = 1; j <= k-1; j++) {
            a = -a * 0.25 * (j + k - 1.0)/(double)j;
            oldh_0 = new double[h_0.length];
            for (m = 0; m < h_0.length; m++) {
                oldh_0[m] = h_0[m];
            }
            h_0 = new double[h_0.length + 1];
            for (m = 0; m < h_0.length - 1; m++) {
                h_0[m+1] += oldh_0[m];
                h_0[m] += oldh_0[m];
            }
            oldp = new double[p.length];
            for (m = 0; m < p.length; m++) {
                oldp[m] = p[m];
            }
            p = new double[p.length+1];
            for (m = 0; m < p.length-1; m++) {
                p[m+1] += -oldp[m];
                p[m] += oldp[m];
            }
            oldp = new double[p.length];
            for (m = 0; m < p.length; m++) {
                oldp[m] = p[m];
            }
            p = new double[p.length+1];
            for (m = 0; m < p.length-1; m++) {
                p[m+1] += -oldp[m];
                p[m] += oldp[m];
            }
            oldq = new double[q.length];
            for (m = 0; m < q.length; m++) {
                oldq[m] = q[m];
            }
            q = new double[q.length+2];
            for (m = 0; m < q.length-2; m++) {
                q[m+1] = oldq[m];    
            }
            for (m = 0; m < p.length; m++) {
                q[m] += a*p[m];
            }
        } // for (j = 1; j <= k-1; j++) 
        
        A = new double[q.length-1][q.length-1];
        for (m = 1; m < q.length-1; m++) {
            A[m][m-1] = 1.0;
        }
        for (m = 0; m < q.length -1; m++) {
            A[0][m] = -q[m+1]/q[0];
        }
        eigenvalueR = new double[q.length-1];
        eI = new double[q.length-1];
        V = new double[q.length-1][q.length-1];
        // A = V * (diagonal eigenvalues) * V'
        // In EigevalueDecomposition the columns of V represent the eigenvectors
        // Whitening matrix = v * 1/sqrt(diagonal eigenvalues) * V'
        Eigenvalue.decompose( A, V, eigenvalueR, eI );
        // Sort into ascending order
        List<EigenvalueComplex> list = new ArrayList<EigenvalueComplex>();  
        for (m = 0; m < q.length-1; m++) {
            list.add(new EigenvalueComplex(eigenvalueR[m], eI[m]));
        }
        Collections.sort(list, new EigenvalueComplexComparator());
        qtR = new double[k-1];
        qtI = new double[k-1];
        for (m = 0; m < k-1; m++) {
            qtR[m] = list.get(m).getReal();
            qtI[m] = list.get(m).getImaginary();
        }
        if (filterType == MID_PHASE) {
            if ((k % 2) == 1) {
                j = 0;
                for (m = 0; m <= filterLength-1; m += 4) {
                    qtR[j] = list.get(m).getReal();
                    qtI[j++] = list.get(m).getImaginary();
                }
                for (m = 1; m <= filterLength-1; m += 4) {
                    qtR[j] = list.get(m).getReal();
                    qtI[j++] = list.get(m).getImaginary();
                }
            } // if ((k % 2) == 1)
            else {
                qtR[0]= list.get(0).getReal();
                qtI[0] = list.get(0).getImaginary();
                j = 1;
                for (m = 3; m <= k-2; m += 4) {
                    qtR[j] = list.get(m).getReal();
                    qtI[j++] = list.get(m).getImaginary();
                }
                for (m = 4; m <= k-2; m += 4) {
                    qtR[j] = list.get(m).getReal();
                    qtI[j++] = list.get(m).getImaginary();
                }
                for (m = filterLength-4; m >= k-1; m -= 4) {
                    qtR[j] = list.get(m).getReal();
                    qtI[j++] = list.get(m).getImaginary();
                }
                for (m = filterLength-5; m >= k-1; m -= 4) {
                    qtR[j] = list.get(m).getReal();
                    qtI[j++] = list.get(m).getImaginary();
                }
            } // else
        } // if (filterType == MID_PHASE)
        polyqtR = new double[k];
        polyqtI = new double[k];
        polyqtR[0] = 1.0;
        polyqtI[0] = 0.0;
        for (j = 0; j <= k-2; j++) {
            for (m = j+1; m >= 1; m--) {
                polyqtR[m] = polyqtR[m] - qtR[j]*polyqtR[m-1] + qtI[j]*polyqtI[m-1];
                polyqtI[m] = polyqtI[m] - qtR[j]*polyqtI[m-1] - qtI[j]*polyqtR[m-1];
            }
        }
        // Convolve h_0 with polyqtR
        // Length of h_0 = 2 + k - 1 = k + 1
        // Length of polyqtR = k
        // Length of (h_0 * polyqtR) = k + 1 + k - 1 = 2*k = filterLength
        w = new double[filterLength];
        for (m = 0; m < filterLength; m++) {
            minj = Math.max(0, m + 1 - k);
            maxj = Math.min(m, k);
            for (j = minj; j <= maxj; j++) {
                w[m] += h_0[j]*polyqtR[m - j];
            }
        }
        sumw = 0.0;
        for (j = 0; j < filterLength; j++) {
            sumw += w[j];
        }
        // Normalize to sqrt(2)
        scale = Math.sqrt(2.0)/sumw;
        for (j = 0; j < filterLength; j++) {
            scalingFilter[j] = scale*w[j];
        }
        temp = new double[filterLength];
        if (filterType == MAXIMUM_PHASE) {
            for (j = 0; j < filterLength; j++) {
                temp[filterLength-1-j] = scalingFilter[j];
            }
            for (j = 0; j < filterLength; j++) {
                scalingFilter[j] = temp[j];
            }
        } // if (filterType == MAXIMUM_PHASE)
        sumw = 0.0;
        for (j = 0; j < filterLength ; j++) {
            sumw += scalingFilter[j]*scalingFilter[j];
        }
        if ((sumw - 1.0) > 1.0e-4) {
            displayError("Numerically unstable for this value of filterLength");
            error = -1;
        }
        for (j = 0; j < filterLength; j++) {
            waveletFilter[filterLength-1-j] = scalingFilter[j];
        }
        for (j = 0; j < filterLength; j+= 2) {
            waveletFilter[j] = -waveletFilter[j];
        }
        
        return;
    } // daubcqf()
    
    /**
     * Accessor that returns the image.
     * 
     * @return the wavelet image
     */
    public ModelImage[] getWaveletImages() {
        return waveletImage;
    }
    
    class EigenvalueComplex {
        private double er;
        private double ei;
        
        public EigenvalueComplex(double er, double ei) {
            this.er = er;
            this.ei = ei;
        }
        
        double getReal() {
            return er;
        }
        
        double getImaginary() {
            return ei;
        }
    }
    
    class EigenvalueComplexComparator implements Comparator<EigenvalueComplex> {
        public int compare(EigenvalueComplex e1, EigenvalueComplex e2) {
            if (e1.getReal() > e2.getReal()) {
                return 1;
            }
            else if (e1.getReal() < e2.getReal()) {
                return -1;
            }
            else if (e1.getImaginary() > e2.getImaginary()) {
                return 1;
            }
            else if (e1.getImaginary() < e2.getImaginary()) {
                return -1;
            }
            else {
                return 0;
            }
        }
    }
    
}