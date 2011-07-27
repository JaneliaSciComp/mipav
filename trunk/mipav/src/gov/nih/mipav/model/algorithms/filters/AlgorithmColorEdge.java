package gov.nih.mipav.model.algorithms.filters;


import gov.nih.mipav.model.algorithms.*;
import gov.nih.mipav.model.structures.*;

import java.io.*;


/**
     This algorithm uses a hypercomplex filter to find the edges between a region of two
 user specified colors.  This code is based on material in the article:
 Colour-Sensitive Edge Detection using Hypercomplex Filters by Carolyn J. Evans and
 Stephen J. Sangwine.  This filter operates in 2D.  Since the 2 colors are normalized to
 unit vectors, the filter only responds to the color or chromaticity information and
 ignores the luminance information.
 
     In this case hypercomplex filters is really just a fancy way of saying that dot 
 products and cross products of colors are used.
 
 FLAG_T24.tif is an excellent test file for this algorithm.
 */
public class AlgorithmColorEdge extends AlgorithmBase {

    //~ Instance fields ------------------------------------------------------------------------------------------------
    
    private double red1;
    private double green1;
    private double blue1;
    private double red2;
    private double green2;
    private double blue2;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * This constructor initialises a Color Edge algorithm for a source and destination image, and ensures that
     * the destination image is <code>ModelStorageBase.UBYTE</code>.
     *
     * <p>Currently (9 May 2006), this algorithm does not support replacing the original data set with that of the
     * color edge image.</p>
     *
     * @param  dest      DOCUMENT ME!
     * @param  src       DOCUMENT ME!
     * @param  red1
     * @param  green1
     * @param  blue1
     * @param  red2
     * @param  green2
     * @param  blue2
     */
    public AlgorithmColorEdge(ModelImage dest, ModelImage src, int red1int, int green1int, int blue1int,
                                   int red2int, int green2int, int blue2int) {
        super(dest, src);

        red1 = (double)red1int;
        green1 = (double)green1int;
        blue1 = (double)blue1int;
        red2 = (double)red2int;
        green2 = (double)green2int;
        blue2 = (double)blue2int;
    }

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * Prepares this class for destruction.
     */
    public void finalize() {
        super.finalize();

        
    }

    /**
     * Standard algorithm run method. It will not run if the source Image is <code>null</code>. The calculation is done
     * and placed in a separate destination image if it is to be stored there.
     */
    public void runAlgorithm() {

        if (srcImage == null) {
            displayError("Source Image is null");

            return;
        }

        

        if (destImage != null) { // if there exists a destination image
            calcStoreInDest();
        } else { // there is no image but the original source.
            calcInPlace();
        }
    }

    /**
     * Filters the source image. Replaces the original image with the filtered image.
     *
     * <p><em>Does not currently work.</em></p>
     */
    private void calcInPlace() {
        errorCleanUp("AlgorithmColorEdge: " + "Replace Image not yet implemented", false);
        finalize();

        return;
    }

    /**
     * This function produces a color edged image into a ModelImage that does not replace the original image-data.
     */
    private void calcStoreInDest() {
        int dataLen = srcImage.getSliceSize();
        int colorLen = 4*dataLen;
        int xDim = srcImage.getExtents()[0];
        int yDim = srcImage.getExtents()[1];
        double[] imageData;
        float[] realData;
        float[] vData;
        double iData;
        double jData;
        double kData;
        byte[] eData;
        byte[] edgeData;
        int sliceNum = 1;
        int z;
        int x, y;
        double absC1;
        double absC2;
        double C1DotC2;
        double absC1CrossC2;
        double iComp;
        double jComp;
        double kComp;
        double midGray;
        double cosC1C2;
        double sinC1C2;
        double tan12div2;
        double vDivSMax1;
        double vDivSMax2;
        double uC1i;
        double uC1j;
        double uC1k;
        double uC2i;
        double uC2j;
        double uC2k;
        int index;
        int i;
        
        fireProgressStateChanged(0, srcImage.getImageName(), "Color edge ...");
        
        
        if (srcImage.getType() == ModelStorageBase.ARGB) {
            midGray = 127.5;
        }
        else { // ModelStorageBase.ARGB_USHORT
            midGray = (srcImage.getMin() + srcImage.getMax())/2.0;
        }
      
        red1 -= midGray;
        green1 -= midGray;
        blue1 -= midGray;
        red2 -= midGray;
        green2 -= midGray;
        blue2 -= midGray;
        absC1 = Math.sqrt(red1*red1 + green1*green1 + blue1*blue1);
        absC2 = Math.sqrt(red2*red2 + green2*green2 + blue2*blue2);
        C1DotC2 = red1*red2 + green1*green2 + blue1*blue2;
        cosC1C2 = C1DotC2/(absC1 * absC2);
        iComp = green1*blue2 - blue1*green2;
        jComp = blue1*red2 - red1*blue2;
        kComp = red1*green2 - green1*red2;
        absC1CrossC2 = Math.sqrt(iComp*iComp + jComp*jComp + kComp*kComp);
        sinC1C2 = absC1CrossC2/(absC1 * absC2);
        if (sinC1C2 != 0.0) {
            tan12div2 = (1.0 - cosC1C2)/sinC1C2;
        }
        else {
            tan12div2 = 0.0;
        }
        vDivSMax1 = Math.min(0.204, tan12div2);
        vDivSMax2 = Math.min((1.0/Math.sqrt(3.0)), tan12div2);
        uC1i = red1/absC1;
        uC1j = green1/absC1;
        uC1k = blue1/absC1;     
        uC2i = red2/absC2;
        uC2j = green2/absC2;
        uC2k = blue2/absC2;

        if (srcImage.getNDims() == 3) {
            sliceNum *= srcImage.getExtents()[2];
        } else if (srcImage.getNDims() == 4) {
            sliceNum *= srcImage.getExtents()[2] * srcImage.getExtents()[3];
        }

        try {
            imageData = new double[colorLen];
        } catch (OutOfMemoryError oome) {
            errorCleanUp("AlgorithmColorEdge: " + "out of memory creating imageData", false);
            finalize();
            return;
        }
        
        try {
            eData = new byte[dataLen];
        } catch (OutOfMemoryError oome) {
            errorCleanUp("AlgorithmColorEdge: " + "out of memory creating eData", false);
            finalize();
            return;
        }
        
        try {
            edgeData = new byte[dataLen];
        } catch (OutOfMemoryError oome) {
            errorCleanUp("AlgorithmColorEdge: " + "out of memory creating edgeData", false);
            finalize();
            return;
        }
        
        try {
            realData = new float[dataLen];
        } catch (OutOfMemoryError oome) {
            errorCleanUp("AlgorithmColorEdge: " + "out of memory creating realData", false);
            finalize();
            return;
        }

        try {
            vData = new float[dataLen];
        } catch (OutOfMemoryError oome) {
            errorCleanUp("AlgorithmColorEdge: " + "out of memory creating vData", false);
            finalize();
            return;
        }
        
        for (z = 0; z < sliceNum; z++) {
            
            fireProgressStateChanged(((float) (z) / (sliceNum)), null, null);
           
            try {
                srcImage.exportData(z*colorLen, colorLen, imageData);
            } catch (IOException ioe) {
                errorCleanUp("AlgorithmColorEdge: " + "failure to export imageData", false);
                finalize();
                return;
            }
            
            for (i = 0; i < colorLen; i++) {
                imageData[i] -= midGray;
            }
            
            for (i = 0; i < dataLen; i++) {
                eData[i] = 0;
                edgeData[i] = 0;
            }
            
            for (y = 1; y <= yDim - 2; y++) {
                for (x = 1; x <= xDim - 2; x++) {
                    // Perform the hypercomplex filter designed to find all horizontal
                    // edges C1 -> C2 (from top to bottom)
                    index = x + y * xDim;
                    realData[index] = -(float)((uC2i*imageData[4*(index+xDim+1) + 1]
                                        + uC2j*imageData[4*(index+xDim+1) + 2]
                                        + uC2k*imageData[4*(index+xDim+1) + 3]
                                        + uC2i*imageData[4*(index+xDim) + 1]
                                        + uC2j*imageData[4*(index+xDim) + 2]
                                        + uC2k*imageData[4*(index+xDim) + 3]
                                        + uC2i*imageData[4*(index+xDim-1) + 1]
                                        + uC2j*imageData[4*(index+xDim-1) + 2]
                                        + uC2k*imageData[4*(index+xDim-1) + 3]
                                        + uC1i*imageData[4*(index-xDim+1) + 1]
                                        + uC1j*imageData[4*(index-xDim+1) + 2]
                                        + uC1k*imageData[4*(index-xDim+1) + 3]
                                        + uC1i*imageData[4*(index-xDim) + 1]
                                        + uC1j*imageData[4*(index-xDim) + 2]
                                        + uC1k*imageData[4*(index-xDim) + 3]
                                        + uC1i*imageData[4*(index-xDim-1) + 1]
                                        + uC1j*imageData[4*(index-xDim-1) + 2]
                                        + uC1k*imageData[4*(index-xDim-1) + 3])/6.0);
                    iData = (uC2j*imageData[4*(index+xDim+1) + 3] 
                             - uC2k*imageData[4*(index+xDim + 1) + 2]
                             + uC2j*imageData[4*(index+xDim) + 3]
                             - uC2k*imageData[4*(index+xDim) + 2] 
                             + uC2j*imageData[4*(index+xDim - 1) + 3] 
                             - uC2k*imageData[4*(index+xDim - 1) + 2] 
                             + uC1k*imageData[4*(index-xDim+1) + 2]
                             - uC1j*imageData[4*(index-xDim+1) + 3]
                             + uC1k*imageData[4*(index-xDim) + 2]
                             - uC1j*imageData[4*(index-xDim) + 3]
                             + uC1k*imageData[4*(index-xDim-1) + 2]
                             - uC1j*imageData[4*(index-xDim-1) + 3])/6.0;
                    jData = (uC2k*imageData[4*(index+xDim+1) + 1] 
                             - uC2i*imageData[4*(index+xDim + 1) + 3]
                             + uC2k*imageData[4*(index+xDim) + 1]
                             - uC2i*imageData[4*(index+xDim) + 3] 
                             + uC2k*imageData[4*(index+xDim - 1) + 1] 
                             - uC2i*imageData[4*(index+xDim - 1) + 3] 
                             + uC1i*imageData[4*(index-xDim+1) + 3]
                             - uC1k*imageData[4*(index-xDim+1) + 1]
                             + uC1i*imageData[4*(index-xDim) + 3]
                             - uC1k*imageData[4*(index-xDim) + 1]
                             + uC1i*imageData[4*(index-xDim-1) + 3]
                             - uC1k*imageData[4*(index-xDim-1) + 1])/6.0;
                    kData = (uC2i*imageData[4*(index+xDim+1) + 2] 
                             - uC2j*imageData[4*(index+xDim + 1) + 1]
                             + uC2i*imageData[4*(index+xDim) + 2]
                             - uC2j*imageData[4*(index+xDim) + 1] 
                             + uC2i*imageData[4*(index+xDim - 1) + 2] 
                             - uC2j*imageData[4*(index+xDim - 1) + 1] 
                             + uC1j*imageData[4*(index-xDim+1) + 1]
                             - uC1i*imageData[4*(index-xDim+1) + 2]
                             + uC1j*imageData[4*(index-xDim) + 1]
                             - uC1i*imageData[4*(index-xDim) + 2]
                             + uC1j*imageData[4*(index-xDim-1) + 1]
                             - uC1i*imageData[4*(index-xDim-1) + 2])/6.0;
                    vData[index] = (float)Math.sqrt(iData*iData + jData*jData
                                             + kData*kData);
                    vData[index] = vData[index]/Math.abs(realData[index]);
                    if ((realData[index] < 0)  && (vData[index] < vDivSMax1)) {
                        eData[index] = (byte)255;
                        edgeData[index] = (byte)255;
                    }
                } // for (x = 1; x <= xDim - 2; x++)
            } // for (y = 1; y <= yDim - 2; y++)
            
            for (y = 1; y <= yDim - 2; y++) {
                for (x = 1; x <= xDim - 2; x++) {
                    index = x + y*xDim;
                    if ((realData[index] < 0) && (vData[index] < vDivSMax2) &&
                       ((eData[index+1] != 0) || (eData[index-1] != 0))) {
                           edgeData[index] = (byte)255; 
                    }
                }
            }
    
            for (i = 0; i < dataLen; i++) {
                eData[i] = 0;
            }
            
            for (y = 1; y <= yDim - 2; y++) {
                for (x = 1; x <= xDim - 2; x++) {
                    // Perform the hypercomplex filter designed to find all horizontal
                    // edges C2 -> C1 (from top to bottom)
                    index = x + y * xDim;
                    realData[index] = (float)-((uC1i*imageData[4*(index+xDim+1) + 1]
                                        + uC1j*imageData[4*(index+xDim+1) + 2]
                                        + uC1k*imageData[4*(index+xDim+1) + 3]
                                        + uC1i*imageData[4*(index+xDim) + 1]
                                        + uC1j*imageData[4*(index+xDim) + 2]
                                        + uC1k*imageData[4*(index+xDim) + 3]
                                        + uC1i*imageData[4*(index+xDim-1) + 1]
                                        + uC1j*imageData[4*(index+xDim-1) + 2]
                                        + uC1k*imageData[4*(index+xDim-1) + 3]
                                        + uC2i*imageData[4*(index-xDim+1) + 1]
                                        + uC2j*imageData[4*(index-xDim+1) + 2]
                                        + uC2k*imageData[4*(index-xDim+1) + 3]
                                        + uC2i*imageData[4*(index-xDim) + 1]
                                        + uC2j*imageData[4*(index-xDim) + 2]
                                        + uC2k*imageData[4*(index-xDim) + 3]
                                        + uC2i*imageData[4*(index-xDim-1) + 1]
                                        + uC2j*imageData[4*(index-xDim-1) + 2]
                                        + uC2k*imageData[4*(index-xDim-1) + 3])/6.0);
                    iData = (uC1j*imageData[4*(index+xDim+1) + 3] 
                             - uC1k*imageData[4*(index+xDim + 1) + 2]
                             + uC1j*imageData[4*(index+xDim) + 3]
                             - uC1k*imageData[4*(index+xDim) + 2] 
                             + uC1j*imageData[4*(index+xDim - 1) + 3] 
                             - uC1k*imageData[4*(index+xDim - 1) + 2] 
                             + uC2k*imageData[4*(index-xDim+1) + 2]
                             - uC2j*imageData[4*(index-xDim+1) + 3]
                             + uC2k*imageData[4*(index-xDim) + 2]
                             - uC2j*imageData[4*(index-xDim) + 3]
                             + uC2k*imageData[4*(index-xDim-1) + 2]
                             - uC2j*imageData[4*(index-xDim-1) + 3])/6.0;
                    jData = (uC1k*imageData[4*(index+xDim+1) + 1] 
                             - uC1i*imageData[4*(index+xDim + 1) + 3]
                             + uC1k*imageData[4*(index+xDim) + 1]
                             - uC1i*imageData[4*(index+xDim) + 3] 
                             + uC1k*imageData[4*(index+xDim - 1) + 1] 
                             - uC1i*imageData[4*(index+xDim - 1) + 3] 
                             + uC2i*imageData[4*(index-xDim+1) + 3]
                             - uC2k*imageData[4*(index-xDim+1) + 1]
                             + uC2i*imageData[4*(index-xDim) + 3]
                             - uC2k*imageData[4*(index-xDim) + 1]
                             + uC2i*imageData[4*(index-xDim-1) + 3]
                             - uC2k*imageData[4*(index-xDim-1) + 1])/6.0;
                    kData = (uC1i*imageData[4*(index+xDim+1) + 2] 
                             - uC1j*imageData[4*(index+xDim + 1) + 1]
                             + uC1i*imageData[4*(index+xDim) + 2]
                             - uC1j*imageData[4*(index+xDim) + 1] 
                             + uC1i*imageData[4*(index+xDim - 1) + 2] 
                             - uC1j*imageData[4*(index+xDim - 1) + 1] 
                             + uC2j*imageData[4*(index-xDim+1) + 1]
                             - uC2i*imageData[4*(index-xDim+1) + 2]
                             + uC2j*imageData[4*(index-xDim) + 1]
                             - uC2i*imageData[4*(index-xDim) + 2]
                             + uC2j*imageData[4*(index-xDim-1) + 1]
                             - uC2i*imageData[4*(index-xDim-1) + 2])/6.0;
                    vData[index] = (float)Math.sqrt(iData*iData + jData*jData
                                             + kData*kData);
                    vData[index] = vData[index]/Math.abs(realData[index]);
                    if ((realData[index] < 0)  && (vData[index] < vDivSMax1)) {
                        eData[index] = (byte)255;
                        edgeData[index] = (byte)255;
                    }
                } // for (x = 1; x <= xDim - 2; x++)
            } // for (y = 1; y <= yDim - 2; y++)
            
            for (y = 1; y <= yDim - 2; y++) {
                for (x = 1; x <= xDim - 2; x++) {
                    index = x + y*xDim;
                    if ((realData[index] < 0) && (vData[index] < vDivSMax2) &&
                       ((eData[index+1] != 0) || (eData[index-1] != 0))) {
                           edgeData[index] = (byte)255; 
                    }
                }
            }
            
            for (i = 0; i < dataLen; i++) {
                eData[i] = 0;
            }
            
            for (y = 1; y <= yDim - 2; y++) {
                for (x = 1; x <= xDim - 2; x++) {
                    // Perform the hypercomplex filter designed to find all vertical
                    // edges C1 -> C2 (from left to right)
                    index = x + y * xDim;
                    realData[index] = (float)-((uC2i*imageData[4*(index+xDim+1) + 1]
                                        + uC2j*imageData[4*(index+xDim+1) + 2]
                                        + uC2k*imageData[4*(index+xDim+1) + 3]
                                        + uC2i*imageData[4*(index+1) + 1]
                                        + uC2j*imageData[4*(index+1) + 2]
                                        + uC2k*imageData[4*(index+1) + 3]
                                        + uC2i*imageData[4*(index-xDim+1) + 1]
                                        + uC2j*imageData[4*(index-xDim+1) + 2]
                                        + uC2k*imageData[4*(index-xDim+1) + 3]
                                        + uC1i*imageData[4*(index+xDim-1) + 1]
                                        + uC1j*imageData[4*(index+xDim-1) + 2]
                                        + uC1k*imageData[4*(index+xDim-1) + 3]
                                        + uC1i*imageData[4*(index-1) + 1]
                                        + uC1j*imageData[4*(index-1) + 2]
                                        + uC1k*imageData[4*(index-1) + 3]
                                        + uC1i*imageData[4*(index-xDim-1) + 1]
                                        + uC1j*imageData[4*(index-xDim-1) + 2]
                                        + uC1k*imageData[4*(index-xDim-1) + 3])/6.0);
                    iData = (uC2j*imageData[4*(index+xDim+1) + 3] 
                             - uC2k*imageData[4*(index+xDim + 1) + 2]
                             + uC2j*imageData[4*(index+1) + 3]
                             - uC2k*imageData[4*(index+1) + 2] 
                             + uC2j*imageData[4*(index-xDim + 1) + 3] 
                             - uC2k*imageData[4*(index-xDim + 1) + 2] 
                             + uC1k*imageData[4*(index+xDim-1) + 2]
                             - uC1j*imageData[4*(index+xDim-1) + 3]
                             + uC1k*imageData[4*(index-1) + 2]
                             - uC1j*imageData[4*(index-1) + 3]
                             + uC1k*imageData[4*(index-xDim-1) + 2]
                             - uC1j*imageData[4*(index-xDim-1) + 3])/6.0;
                    jData = (uC2k*imageData[4*(index+xDim+1) + 1] 
                             - uC2i*imageData[4*(index+xDim + 1) + 3]
                             + uC2k*imageData[4*(index+1) + 1]
                             - uC2i*imageData[4*(index+1) + 3] 
                             + uC2k*imageData[4*(index-xDim + 1) + 1] 
                             - uC2i*imageData[4*(index-xDim + 1) + 3] 
                             + uC1i*imageData[4*(index+xDim-1) + 3]
                             - uC1k*imageData[4*(index+xDim-1) + 1]
                             + uC1i*imageData[4*(index-1) + 3]
                             - uC1k*imageData[4*(index-1) + 1]
                             + uC1i*imageData[4*(index-xDim-1) + 3]
                             - uC1k*imageData[4*(index-xDim-1) + 1])/6.0;
                    kData = (uC2i*imageData[4*(index+xDim+1) + 2] 
                             - uC2j*imageData[4*(index+xDim + 1) + 1]
                             + uC2i*imageData[4*(index+1) + 2]
                             - uC2j*imageData[4*(index+1) + 1] 
                             + uC2i*imageData[4*(index-xDim + 1) + 2] 
                             - uC2j*imageData[4*(index-xDim + 1) + 1] 
                             + uC1j*imageData[4*(index+xDim-1) + 1]
                             - uC1i*imageData[4*(index+xDim-1) + 2]
                             + uC1j*imageData[4*(index-1) + 1]
                             - uC1i*imageData[4*(index-1) + 2]
                             + uC1j*imageData[4*(index-xDim-1) + 1]
                             - uC1i*imageData[4*(index-xDim-1) + 2])/6.0;
                    vData[index] = (float)Math.sqrt(iData*iData + jData*jData
                                             + kData*kData);
                    vData[index] = vData[index]/Math.abs(realData[index]);
                    if ((realData[index] < 0)  && (vData[index] < vDivSMax1)) {
                        eData[index] = (byte)255;
                        edgeData[index] = (byte)255;
                    }
                } // for (x = 1; x <= xDim - 2; x++)
            } // for (y = 1; y <= yDim - 2; y++)
            
            for (y = 1; y <= yDim - 2; y++) {
                for (x = 1; x <= xDim - 2; x++) {
                    index = x + y*xDim;
                    if ((realData[index] < 0) && (vData[index] < vDivSMax2) &&
                       ((eData[index-xDim] != 0) || (eData[index+xDim] != 0))) {
                           edgeData[index] = (byte)255; 
                    }
                }
            }
    
            for (i = 0; i < dataLen; i++) {
                eData[i] = 0;
            }
            
            for (y = 1; y <= yDim - 2; y++) {
                for (x = 1; x <= xDim - 2; x++) {
                    // Perform the hypercomplex filter designed to find all vertical
                    // edges C2 -> C1 (from left to right)
                    index = x + y * xDim;
                    realData[index] = (float)-((uC1i*imageData[4*(index+xDim+1) + 1]
                                        + uC1j*imageData[4*(index+xDim+1) + 2]
                                        + uC1k*imageData[4*(index+xDim+1) + 3]
                                        + uC1i*imageData[4*(index+1) + 1]
                                        + uC1j*imageData[4*(index+1) + 2]
                                        + uC1k*imageData[4*(index+1) + 3]
                                        + uC1i*imageData[4*(index-xDim+1) + 1]
                                        + uC1j*imageData[4*(index-xDim+1) + 2]
                                        + uC1k*imageData[4*(index-xDim+1) + 3]
                                        + uC2i*imageData[4*(index+xDim-1) + 1]
                                        + uC2j*imageData[4*(index+xDim-1) + 2]
                                        + uC2k*imageData[4*(index+xDim-1) + 3]
                                        + uC2i*imageData[4*(index-1) + 1]
                                        + uC2j*imageData[4*(index-1) + 2]
                                        + uC2k*imageData[4*(index-1) + 3]
                                        + uC2i*imageData[4*(index-xDim-1) + 1]
                                        + uC2j*imageData[4*(index-xDim-1) + 2]
                                        + uC2k*imageData[4*(index-xDim-1) + 3])/6.0);
                    iData = (uC1j*imageData[4*(index+xDim+1) + 3] 
                             - uC1k*imageData[4*(index+xDim + 1) + 2]
                             + uC1j*imageData[4*(index+1) + 3]
                             - uC1k*imageData[4*(index+1) + 2] 
                             + uC1j*imageData[4*(index-xDim + 1) + 3] 
                             - uC1k*imageData[4*(index-xDim + 1) + 2] 
                             + uC2k*imageData[4*(index+xDim-1) + 2]
                             - uC2j*imageData[4*(index+xDim-1) + 3]
                             + uC2k*imageData[4*(index-1) + 2]
                             - uC2j*imageData[4*(index-1) + 3]
                             + uC2k*imageData[4*(index-xDim-1) + 2]
                             - uC2j*imageData[4*(index-xDim-1) + 3])/6.0;
                    jData = (uC1k*imageData[4*(index+xDim+1) + 1] 
                             - uC1i*imageData[4*(index+xDim + 1) + 3]
                             + uC1k*imageData[4*(index+1) + 1]
                             - uC1i*imageData[4*(index+1) + 3] 
                             + uC1k*imageData[4*(index-xDim + 1) + 1] 
                             - uC1i*imageData[4*(index-xDim + 1) + 3] 
                             + uC2i*imageData[4*(index+xDim-1) + 3]
                             - uC2k*imageData[4*(index+xDim-1) + 1]
                             + uC2i*imageData[4*(index-1) + 3]
                             - uC2k*imageData[4*(index-1) + 1]
                             + uC2i*imageData[4*(index-xDim-1) + 3]
                             - uC2k*imageData[4*(index-xDim-1) + 1])/6.0;
                    kData = (uC1i*imageData[4*(index+xDim+1) + 2] 
                             - uC1j*imageData[4*(index+xDim + 1) + 1]
                             + uC1i*imageData[4*(index+1) + 2]
                             - uC1j*imageData[4*(index+1) + 1] 
                             + uC1i*imageData[4*(index-xDim + 1) + 2] 
                             - uC1j*imageData[4*(index-xDim + 1) + 1] 
                             + uC2j*imageData[4*(index+xDim-1) + 1]
                             - uC2i*imageData[4*(index+xDim-1) + 2]
                             + uC2j*imageData[4*(index-1) + 1]
                             - uC2i*imageData[4*(index-1) + 2]
                             + uC2j*imageData[4*(index-xDim-1) + 1]
                             - uC2i*imageData[4*(index-xDim-1) + 2])/6.0;
                    vData[index] = (float)Math.sqrt(iData*iData + jData*jData
                                             + kData*kData);
                    vData[index] = vData[index]/Math.abs(realData[index]);
                    if ((realData[index] < 0)  && (vData[index] < vDivSMax1)) {
                        eData[index] = (byte)255;
                        edgeData[index] = (byte)255;
                    }
                } // for (x = 1; x <= xDim - 2; x++)
            } // for (y = 1; y <= yDim - 2; y++)
            
            for (y = 1; y <= yDim - 2; y++) {
                for (x = 1; x <= xDim - 2; x++) {
                    index = x + y*xDim;
                    if ((realData[index] < 0) && (vData[index] < vDivSMax2) &&
                       ((eData[index-xDim] != 0) || (eData[index+xDim] != 0))) {
                           edgeData[index] = (byte)255; 
                    }
                }
            }
            
            if (threadStopped) {
                setCompleted(false);
                finalize();
    
                return;
            }
    
            try {
                destImage.importData(z*dataLen, edgeData, false);
            }
            catch (IOException ioe) {
                errorCleanUp("AlgorithmColorEdge: " + "failure to import edgeData", false);
                finalize();
    
                return;    
            }
        } // for (z = 0; z < sliceNum; z++)
        
        
        fireProgressStateChanged(100, null, null);
        destImage.calcMinMax();
        
        setCompleted(true);

    } // end calcStoreInDest()

   
}
