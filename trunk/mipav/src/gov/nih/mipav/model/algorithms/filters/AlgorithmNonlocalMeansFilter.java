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
    
    /**
     * Noise standard deviation
     */
    private float noiseStandardDeviation;

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
     * @param  noiseStandardDeviation   Noise standard deviation
     * @param  do25D             If true, do slice by slice filtering
     */
    public AlgorithmNonlocalMeansFilter(ModelImage destImage, ModelImage srcImg, int searchWindowSide, 
                                       int similarityWindowSide, float noiseStandardDeviation, boolean do25D) {
        super(destImage, srcImg);
        this.searchWindowSide = searchWindowSide;
        this.similarityWindowSide = similarityWindowSide;
        this.noiseStandardDeviation = noiseStandardDeviation;
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
        int x;
        int y;
        int z;
        int xDim;
        int yDim;
        int zDim;
        int length;
        float input[];
        double kernel[];
        int halfSimilarity;
        double input2[];
        int padXDim;
        int padYDim;
        int d;
        double value;
        int i;
        int j;
        int r;
        int s;
        int i1;
        int j1;
        double W1[];
        double W2[];
        double kernelSum;
        double wmax;
        double average;
        double sweight;
        int rmin;
        int rmax;
        int smin;
        int smax;
        int halfSearch;
        int k;
        double dsum;
        double w12;
        double w;
        long time;
        double filterParameter;
        
        time = System.currentTimeMillis();
        fireProgressStateChanged(0, srcImage.getImageName(), "Nonlocal means filter");
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
        halfSimilarity = (similarityWindowSide - 1)/2;
        halfSearch = (searchWindowSide - 1)/2;
        padXDim = xDim + 2 * halfSimilarity;
        padYDim = yDim + 2 * halfSimilarity;
        input2 = new double[padXDim * padYDim];
        kernel = new double[similarityWindowSide * similarityWindowSide];
        W1 = new double[similarityWindowSide * similarityWindowSide];
        W2 = new double[similarityWindowSide * similarityWindowSide];
        for (d = 1; d <= halfSimilarity; d++) {
            value = 2*d + 1;
            value = value * value;
            value = 1.0/value;
            for (y = -d; y <= d; y++) {
                for (x = -d; x <= d; x++) {
                    kernel[halfSimilarity - x + similarityWindowSide * (halfSimilarity - y)] += value;
                }
            }
        } // for (d = 1; d <= halfSimilarity; d++)
        kernelSum = 0.0;
        for (i = 0; i < kernel.length; i++) {
            kernelSum += kernel[i];
        }
        for (i = 0; i < kernel.length; i++) {
            kernel[i] /= kernelSum;
        }
        
        filterParameter = noiseStandardDeviation * noiseStandardDeviation;
        for (z = 0; z < zDim; z++) {
            try {
                srcImage.exportData(z*length, length, input);
            }
            catch (IOException e) {
                MipavUtil.displayError("IOException on srcImage.exportData(z*length, length, input)");
                setCompleted(false);
                return;
            }
            
            for (y = 0; y < yDim; y++) {
                for (x = 0; x < xDim; x++) {
                    input2[x + halfSimilarity + padXDim*(y + halfSimilarity)] = input[x + y * xDim];
                }
            }
            
            for (y = 0; y < yDim; y++) {
                for (x = 0; x < halfSimilarity; x++) {
                    // left side mirror reflection
                    input2[x + padXDim*(y + halfSimilarity)] = input[(halfSimilarity - 1 - x) + xDim * y];
                    // right side mirror reflection
                    input2[x + xDim + halfSimilarity + padXDim * (y + halfSimilarity)] =
                    input[xDim - 1 - x + xDim * y];
                }
            }
            for (y = 0; y < halfSimilarity; y++) {
                for (x = 0; x < xDim; x++) {
                    // top side mirror reflection
                    input2[x + halfSimilarity + padXDim * y] = input[x + xDim * (halfSimilarity - 1 - y)];
                    // bottom side mirror reflection
                    input2[x + halfSimilarity + padXDim * (y + yDim + halfSimilarity)] =
                    input[x + xDim * (yDim - 1 - y)];
                }
            }
            for (y = 0; y < halfSimilarity; y++) {
                for (x = 0; x < halfSimilarity; x++) {
                    // left top mirror reflection
                    input2[x + padXDim * y] = input[(halfSimilarity - 1 - x) + xDim * (halfSimilarity - 1 - y)];
                    // left bottom mirror reflection
                    input2[x + padXDim * (y + yDim + halfSimilarity)] = 
                    input[(halfSimilarity - 1 - x) + xDim * (yDim - 1 - y)];
                    // right top mirror reflection
                    input2[x + xDim + halfSimilarity + padXDim * y] =
                    input[xDim - 1 - x + xDim * (halfSimilarity - 1 - y)];
                    // right bottom mirror reflection
                    input2[x + xDim + halfSimilarity + padXDim * (y + yDim + halfSimilarity)] =
                    input[xDim - 1 - x + xDim * (yDim - 1 - y)];
                }
            }
            for (i = 1; i <= yDim; i++) {
                fireProgressStateChanged(100 * (z*yDim + (i - 1)) /(yDim * zDim));
                i1 = i + halfSimilarity;
                rmin = Math.max(i1 - halfSearch, halfSimilarity + 1);
                rmax = Math.min(i1 + halfSearch, yDim + halfSimilarity);
                for (j = 1; j <= xDim; j++) {  
                    j1 = j + halfSimilarity;
                    for (y = 0; y < similarityWindowSide; y++) {
                        for (x = 0; x < similarityWindowSide; x++) {
                            W1[x + y * similarityWindowSide] = 
                            input2[j1 - halfSimilarity - 1 + x + padXDim*(i1 - halfSimilarity - 1 + y)];
                        }
                    }
                    
                    wmax = 0.0;
                    average = 0.0;
                    sweight = 0.0;
                    
                    smin = Math.max(j1 - halfSearch, halfSimilarity + 1);
                    smax = Math.min(j1 + halfSearch, xDim + halfSimilarity);
                    for (r = rmin; r <= rmax; r++) {
                        for (s = smin; s <= smax; s++) {
                            if ((r == i1) && (s == j1)) {
                                continue;
                            }
                            for (y = 0; y < similarityWindowSide; y++) {
                                for (x = 0; x < similarityWindowSide; x++) {
                                    W2[x + y * similarityWindowSide] = 
                                    input2[s - halfSimilarity - 1 + x + padXDim*(r - halfSimilarity - 1 + y)];
                                }
                            }
                            
                            dsum = 0.0;
                            for (k = 0; k < kernel.length; k++) {
                                w12 = W1[k] - W2[k];
                                dsum += (kernel[k] * w12 * w12);
                            }
                            w = Math.exp(-dsum/filterParameter);
                            if (w > wmax) {
                                wmax = w;
                            }
                            sweight = sweight + w;
                            average = average + w * input2[s - 1 + padXDim * (r - 1)];
                        } // for (s = smin; s <= smax; s++)
                    } // for (r = rmin; r <= rmax; r++)
                    average = average + wmax * input2[j1 - 1 + padXDim * (i1 - 1)];
                    sweight = sweight + wmax;
                    
                    if (sweight > 0) {
                        input[j - 1 + xDim * (i - 1)] = (float)(average / sweight);
                    }
                } // for (j = 1; j <= xDim; j++)
            } // for (i = 1; i <= yDim; i++) 
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
        fireProgressStateChanged(100);
        time = System.currentTimeMillis() - time;
        Preferences.debug("Seconds elapsed in AlgorithmNonlocalMeansFilter = " + (time/1000.0) + "\n");
        setCompleted(true);
        return;
    }
    
    private void run3D() {
        int x;
        int y;
        int z;
        int xDim;
        int yDim;
        int zDim;
        int sliceSize;
        int length;
        float input[];
        double kernel[];
        int halfSimilarity;
        int similaritySquared;
        int similarityCubed;
        double input2[];
        int padXDim;
        int padYDim;
        int padZDim;
        int padSliceSize;
        int d;
        double value;
        int h;
        int i;
        int j;
        int q;
        int r;
        int s;
        int h1;
        int i1;
        int j1;
        double W1[];
        double W2[];
        double kernelSum;
        double wmax;
        double average;
        double sweight;
        int qmin;
        int qmax;
        int rmin;
        int rmax;
        int smin;
        int smax;
        int halfSearch;
        int k;
        double dsum;
        double w12;
        double w;
        long time;
        double filterParameter;
        
        time = System.currentTimeMillis();
        fireProgressStateChanged(0, srcImage.getImageName(), "Nonlocal means filter");
        xDim = srcImage.getExtents()[0];
        yDim = srcImage.getExtents()[1];
        zDim = srcImage.getExtents()[2];
        sliceSize = xDim * yDim;
        length = sliceSize * zDim;
        input = new float[length];
        halfSimilarity = (similarityWindowSide - 1)/2;
        similaritySquared = similarityWindowSide * similarityWindowSide;
        similarityCubed = similaritySquared * similarityWindowSide;
        halfSearch = (searchWindowSide - 1)/2;
        padXDim = xDim + 2 * halfSimilarity;
        padYDim = yDim + 2 * halfSimilarity;
        padZDim = zDim + 2 * halfSimilarity;
        padSliceSize = padXDim * padYDim;
        input2 = new double[padSliceSize * padZDim];
        kernel = new double[similarityCubed];
        W1 = new double[similarityCubed];
        W2 = new double[similarityCubed];
        for (d = 1; d <= halfSimilarity; d++) {
            value = 2*d + 1;
            value = value * value;
            value = 1.0/value;
            for (z = -d; z <= d; z++) {
                for (y = -d; y <= d; y++) {
                    for (x = -d; x <= d; x++) {
                        kernel[halfSimilarity - x + similarityWindowSide * (halfSimilarity - y) +
                               similaritySquared * (halfSimilarity - z)] += value;
                    }
                }
            }
        } // for (d = 1; d <= halfSimilarity; d++)
        kernelSum = 0.0;
        for (i = 0; i < kernel.length; i++) {
            kernelSum += kernel[i];
        }
        for (i = 0; i < kernel.length; i++) {
            kernel[i] /= kernelSum;
        }
        
        filterParameter = noiseStandardDeviation * noiseStandardDeviation;
        try {
            srcImage.exportData(0, length, input);
        }
        catch (IOException e) {
            MipavUtil.displayError("IOException on srcImage.exportData(0, length, input)");
            setCompleted(false);
            return;
        }
        
        for (z = 0; z < zDim; z++) {
            for (y = 0; y < yDim; y++) {
                for (x = 0; x < xDim; x++) {
                    input2[x + halfSimilarity + padXDim*(y + halfSimilarity) + padSliceSize*(z + halfSimilarity)] =
                    input[x + y * xDim + z * sliceSize];
                }
            }
        }
        
        for (z = 0; z < zDim; z++) {
            for (y = 0; y < yDim; y++) {
                for (x = 0; x < halfSimilarity; x++) {
                    // left side mirror reflection
                    input2[x + padXDim*(y + halfSimilarity) + padSliceSize*(z + halfSimilarity)] = 
                    input[(halfSimilarity - 1 - x) + xDim * y + sliceSize * z];
                    // right side mirror reflection
                    input2[x + xDim + halfSimilarity + padXDim * (y + halfSimilarity) + padSliceSize*(z + halfSimilarity)] =
                    input[xDim - 1 - x + xDim * y + sliceSize * z];
                }
            }
        }
        
        for (z = 0; z < zDim; z++) {
            for (y = 0; y < halfSimilarity; y++) {
                for (x = 0; x < xDim; x++) {
                    // top side mirror reflection
                    input2[x + halfSimilarity + padXDim * y + padSliceSize*(z + halfSimilarity)] =
                    input[x + xDim * (halfSimilarity - 1 - y) + sliceSize * z];
                    // bottom side mirror reflection
                    input2[x + halfSimilarity + padXDim * (y + yDim + halfSimilarity) + padSliceSize*(z + halfSimilarity)] =
                    input[x + xDim * (yDim - 1 - y) + sliceSize * z];
                }
            }
        }
        
        for (z = 0; z < halfSimilarity; z++) {
            for (y = 0; y < xDim; y++) {
                for (x = 0; x < xDim; x++) {
                    // front side mirror reflection
                    input2[x + halfSimilarity + padXDim*(y + halfSimilarity) + padSliceSize * z] =
                    input[x + xDim * y + sliceSize * (halfSimilarity - 1 - z)];
                    // back side mirror reflection
                    input2[x + halfSimilarity + padXDim*(y + halfSimilarity) + padSliceSize * (z + zDim + halfSimilarity)] =
                    input[x + xDim * y + sliceSize * (zDim - 1 - z)];
                }
            }
        }
        
        for (z = 0; z < zDim; z++) {
            for (y = 0; y < halfSimilarity; y++) {
                for (x = 0; x < halfSimilarity; x++) {
                    // left top mirror reflection
                    input2[x + padXDim * y + padSliceSize * (z + halfSimilarity)] = 
                    input[(halfSimilarity - 1 - x) + xDim * (halfSimilarity - 1 - y) + sliceSize * z ];
                    // left bottom mirror reflection
                    input2[x + padXDim * (y + yDim + halfSimilarity) + padSliceSize * (z + halfSimilarity)] = 
                    input[(halfSimilarity - 1 - x) + xDim * (yDim - 1 - y) + sliceSize * z];
                    // right top mirror reflection
                    input2[x + xDim + halfSimilarity + padXDim * y + padSliceSize * (z + halfSimilarity)] =
                    input[xDim - 1 - x + xDim * (halfSimilarity - 1 - y) + sliceSize * z];
                    // right bottom mirror reflection
                    input2[x + xDim + halfSimilarity + padXDim * (y + yDim + halfSimilarity)
                           + padSliceSize * (z + halfSimilarity)] =
                    input[xDim - 1 - x + xDim * (yDim - 1 - y) + sliceSize * z];
                }
            }
        }
        
        for (z = 0; z < halfSimilarity; z++) {
            for (y = 0; y < yDim; y++) {
                for (x = 0; x < halfSimilarity; x++) {
                    // left front mirror reflection
                    input2[x + padXDim * (y + halfSimilarity) + padSliceSize * z] =
                    input[(halfSimilarity - 1 - x) + xDim * y + sliceSize * (halfSimilarity - 1 - z)];
                    // left back mirror reflection
                    input2[x + padXDim *(y + halfSimilarity) + padSliceSize * (z + zDim + halfSimilarity)] =
                    input[(halfSimilarity - 1 - x) + xDim * y + sliceSize * (zDim - 1 - z)];
                    // right front mirror reflection
                    input2[x + xDim + halfSimilarity + padXDim * (y + halfSimilarity) + padSliceSize * z] =
                    input[xDim - 1 - x + xDim * y + sliceSize * (halfSimilarity - 1 - z)];
                    // right back mirror reflection
                    input2[x + xDim + halfSimilarity + padXDim *(y + halfSimilarity) + 
                           padSliceSize * (z + zDim + halfSimilarity)] =
                     input[xDim - 1 - x + xDim * y + sliceSize * (zDim - 1 - z)];
                }
            }
        }
        
        for (z = 0; z < halfSimilarity; z++) {
            for (y = 0; y < halfSimilarity; y++) {
                for (x = 0; x < xDim; x++) {
                    // top front mirror reflection
                    input2[x + halfSimilarity + padXDim * y + padSliceSize * z] =
                    input[x + xDim * (halfSimilarity - 1 - y) + sliceSize * (halfSimilarity - 1 - z)];
                    // top back mirror reflection
                    input2[x + halfSimilarity + padXDim * y + padSliceSize * (z + zDim + halfSimilarity)] =
                    input[x + xDim * (halfSimilarity - 1 - y) + sliceSize * (zDim - 1 - z)];
                    // bottom front mirror reflection
                    input2[x + halfSimilarity + padXDim * (y + yDim + halfSimilarity) + padSliceSize * z] =
                    input[x + xDim * (yDim - 1 - y) + sliceSize * (halfSimilarity - 1 - z)];
                    // bottom back mirror reflection
                    input2[x + halfSimilarity + padXDim * (y + yDim + halfSimilarity) +
                    padSliceSize * (z + zDim + halfSimilarity)] =
                    input[x + xDim * (yDim - 1 - y) + sliceSize * (zDim - 1 - z)];
                }
            }
        }
        
        
        for (z = 0; z < halfSimilarity; z++) {
            for (y = 0; y < halfSimilarity; y++) {
                for (x = 0; x < halfSimilarity; x++) {
                    // left top front reflection
                    input2[x + padXDim * y + padSliceSize * z] = 
                    input[halfSimilarity - 1 - x + xDim * (halfSimilarity - 1 - y) + sliceSize * (halfSimilarity - 1 - z)];
                    // left top back reflection
                    input2[x + padXDim * y + padSliceSize * (z + zDim + halfSimilarity)] = 
                    input[halfSimilarity - 1 - x + xDim * (halfSimilarity - 1 - y) + sliceSize * (zDim - 1 - z)];
                    // left bottom front reflection
                    input2[x + padXDim * (y + yDim + halfSimilarity) + padSliceSize * z] = 
                    input[halfSimilarity - 1 - x + xDim * (yDim - 1 - y) + sliceSize * (halfSimilarity - 1 - z)];
                    // left bottom back reflection
                    input2[x + padXDim * (y + yDim + halfSimilarity) + padSliceSize * (z + zDim + halfSimilarity)] = 
                    input[halfSimilarity - 1 - x + xDim * (yDim - 1 - y) + sliceSize * (zDim - 1 - z)];
                    // right top front reflection
                    input2[x + xDim + halfSimilarity + padXDim * y + padSliceSize * z] =
                    input[xDim - 1 - x + xDim * (halfSimilarity - 1 - y) + sliceSize * (halfSimilarity - 1 - z)];
                    // right top back reflection
                    input2[x + xDim + halfSimilarity + padXDim * y + padSliceSize * (z + zDim + halfSimilarity)] =
                    input[xDim - 1 - x + xDim * (halfSimilarity - 1 - y) + sliceSize * (zDim - 1 - z)];
                    // right bottom front reflection
                    input2[x + xDim + halfSimilarity + padXDim * (y + yDim + halfSimilarity) + padSliceSize * z] =
                    input[xDim - 1 - x + xDim * (yDim - 1 - y) + sliceSize * (halfSimilarity - 1 - z)];
                    // right bottom back reflection
                    input2[x + xDim + halfSimilarity + padXDim * (y + yDim + halfSimilarity) + 
                    padSliceSize * (z + zDim + halfSimilarity)] =
                    input[xDim - 1 - x + xDim * (yDim - 1 - y) + sliceSize * (zDim - 1 - z)];
                }
            }
        }
        
        for (h = 1; h <= zDim; h++) {
            fireProgressStateChanged(100 * (h - 1) /zDim);
            h1 = h + halfSimilarity;
            qmin = Math.max(h1 - halfSearch, halfSimilarity + 1);
            qmax = Math.min(h1 + halfSearch, zDim + halfSimilarity);
            for (i = 1; i <= yDim; i++) {
                i1 = i + halfSimilarity;
                rmin = Math.max(i1 - halfSearch, halfSimilarity + 1);
                rmax = Math.min(i1 + halfSearch, yDim + halfSimilarity);
                for (j = 1; j <= xDim; j++) {  
                    j1 = j + halfSimilarity;
                    for (z = 0; z < similarityWindowSide; z++) {
                        for (y = 0; y < similarityWindowSide; y++) {
                            for (x = 0; x < similarityWindowSide; x++) {
                                W1[x + y * similarityWindowSide + z * similaritySquared] = 
                                input2[j1 - halfSimilarity - 1 + x + padXDim*(i1 - halfSimilarity - 1 + y) +
                                       padSliceSize * (h1 - halfSimilarity - 1 + z)];
                            }
                        }
                    }
                    
                    wmax = 0.0;
                    average = 0.0;
                    sweight = 0.0;
                    
                    smin = Math.max(j1 - halfSearch, halfSimilarity + 1);
                    smax = Math.min(j1 + halfSearch, xDim + halfSimilarity);
                    for (q = qmin; q <= qmax; q++) {
                        for (r = rmin; r <= rmax; r++) {
                            for (s = smin; s <= smax; s++) {
                                if ((q == h1) && (r == i1) && (s == j1)) {
                                    continue;
                                }
                                for (z = 0; z < similarityWindowSide; z++) {
                                    for (y = 0; y < similarityWindowSide; y++) {
                                        for (x = 0; x < similarityWindowSide; x++) {
                                            W2[x + y * similarityWindowSide + z * similaritySquared] = 
                                            input2[s - halfSimilarity - 1 + x + padXDim*(r - halfSimilarity - 1 + y) +
                                                   padSliceSize * (q - halfSimilarity - 1 + z)];
                                        }
                                    }
                                }
                                
                                dsum = 0.0;
                                for (k = 0; k < kernel.length; k++) {
                                    w12 = W1[k] - W2[k];
                                    dsum += (kernel[k] * w12 * w12);
                                }
                                w = Math.exp(-dsum/filterParameter);
                                if (w > wmax) {
                                    wmax = w;
                                }
                                sweight = sweight + w;
                                average = average + w * input2[s - 1 + padXDim * (r - 1) + padSliceSize * (q - 1)];
                            } // for (s = smin; s <= smax; s++)
                        } // for (r = rmin; r <= rmax; r++)
                    } // for (q = qmin; q <= qmax; q++)
                    average = average + wmax * input2[j1 - 1 + padXDim * (i1 - 1) + padSliceSize * (h1 - 1)];
                    sweight = sweight + wmax;
                    
                    if (sweight > 0) {
                        input[j - 1 + xDim * (i - 1) + sliceSize * (h - 1)] = (float)(average / sweight);
                    }
                } // for (j = 1; j <= xDim; j++)
            } // for (i = 1; i <= yDim; i++) 
        } // for (h = 1; j <= zDim; h++)
        try {
            if (destImage != null) {
                destImage.importData(0, input, true);
            }
            else {
                srcImage.importData(0, input, true);
            }
        }
        catch(IOException e) {
            MipavUtil.displayError("IOException on importData(0, input, true");
            setCompleted(false);
            return;
        }
     
        
        fireProgressStateChanged(100);
        time = System.currentTimeMillis() - time;
        Preferences.debug("Seconds elapsed in AlgorithmNonlocalMeansFilter = " + (time/1000.0) + "\n");
        setCompleted(true);
        return;    
    }

    


}
