package gov.nih.mipav.model.algorithms;


import gov.nih.mipav.model.structures.*;

import gov.nih.mipav.view.*;

import java.io.IOException;
import java.util.*;

public class AlgorithmBoxCount extends AlgorithmBase {
    // Portions of boxcount.m revision 2.10, written by F. Moisy on 07/09/2008
    // were ported to Java in creating this routine.
    // n[] is the number n of 2D dimensional boxes of size r[] needed to cover the 
    // points on the boundary of the VOIContour.  The box sizes are powers of two, i.e.,
    // 1, 2, 4, ..., 2 ^p, where p is the smallest integer such that 
    // where xmin, xmax, ymin, and ymax are the smallest and largest positions with nonzero values
    // max(x max - x min + 1, y max - y min + 1) <= 2^p.
    // The box counting method is used in determining fractal properties of the 
    // contour boundary.  If the boundary is a fractal set, with fractal dimension
    // fd < d, then n scales as r^(-fd).  fd is know as the Minkowski-Bouligand dimension,
    // or Kolmogorov capacity, or Kolmogorov dimension, or simply box-counting dimension.
    // Complex outlines can be defined in 2D using a fractal dimension ranging from 1 to 2.
    
    // Note that the reference "High precision  boundary fractal analysis for shape characterization"
    // by Dominique Berube and Michel Jebrak found:
    // "Dilation and euclidean distance mapping methods(EDM) produce the more reliable results with a
    // low sensitivity to object size and resolution.  The precision of the EDM method (+-0.01) is
    // higher than that for the dilation method(+-0.03).  Box-counting behaves erratically when used
    // to analyze outlines."
    
    private boolean entireImage;
    
    public AlgorithmBoxCount(ModelImage srcImg, boolean entireImage) {
        super(null, srcImg);
        this.entireImage = entireImage;
    }
    
    /**
     * Starts the program.
     */
    public void runAlgorithm() {

        // do all source image verification before logging:
        if (srcImage == null) {
            displayError("Source Image is null");
            setCompleted(false);

            return;
        }
        
        fireProgressStateChanged(srcImage.getImageName(), "Box count ...");

        // Source Image must be Boolean, Byte, UByte, Short or UShort
        srcImage.calcMinMax();
        double minValue = srcImage.getMin();
        if (minValue < 0) {
            displayError("Source Image cannot have negative minimum");
            setCompleted(false);
            return;
        }
        if ((srcImage.getType() != ModelImage.BOOLEAN) && (srcImage.getType() != ModelImage.BYTE) &&
                (srcImage.getType() != ModelImage.UBYTE) && (srcImage.getType() != ModelImage.SHORT) &&
                (srcImage.getType() != ModelImage.USHORT)) {
            displayError("Source Image must be Boolean, Byte, UByte, Short or UShort");
            setCompleted(false);

            return;
        }

        if (srcImage.getNDims() == 2) {
            boxCount2D();    
        }
        else if (srcImage.getNDims() == 3) {
            boxCount3D();
        }
        else {
            displayError("Source Image is not 2D or 3D");
            setCompleted(false);

            return;
        }
    }
    
    private void boxCount2D() {
        int xDim = srcImage.getExtents()[0];
        int yDim = srcImage.getExtents()[1];
        int sliceSize = xDim * yDim;
        BitSet mask = null;
        short imgBuffer[] = null;
        int x;
        int y;
        int xLow = Integer.MAX_VALUE;
        int xHigh = Integer.MIN_VALUE;
        int yLow = Integer.MAX_VALUE;
        int yHigh = Integer.MIN_VALUE;
        int index;
        int xRange;
        int yRange;
        int width;
        double p;
        int pCeil;
        byte c[][];
        int i;
        int g;
        int siz;
        int siz2;
        int j;
        int nInit[];
        double ln[];
        double lr[];
        double gradn[];
        double gradr[];
        double sumxy;
        double sumx;
        double sumy;
        double sumxx;
        double bestFitFD;
        int numPoints = 0;
        int n[] = null;
        int r[] = null;
        double localFD[] = null;
        ViewUserInterface UI = ViewUserInterface.getReference();
        
        
        if (!entireImage) {
            mask = srcImage.generateVOIMask();
        }
        imgBuffer = new short[sliceSize];
        
        try {
            srcImage.exportData(0, sliceSize, imgBuffer);
        }
        catch(IOException error) {
            displayError("IOException " + error + " on srcImage.exportData(0, sliceSize, imgBuffer)");
            setCompleted(false);
            return;
        }
        
        index = -1;
        for (y = 0; y < yDim; y++) {
            for (x = 0; x < xDim; x++) {
                index++;
                if ((entireImage || mask.get(index)) && (imgBuffer[index] > 0)) {
                    if (x < xLow) {
                        xLow = x;
                    }
                    if (x > xHigh) {
                        xHigh = x;
                    }
                    if (y < yLow) {
                        yLow = y;
                    }
                    if (y > yHigh) {
                        yHigh = y;
                    }
                    numPoints++;
                } // if ((entireImage || mask.get(index)) && (imgBuffer[index] > 0))
            } // for (x = 0; x < xDim; x++)
        } // for (y = 0; y < yDim; y++)
        
        xRange = xHigh - xLow + 1;
        yRange = yHigh - yLow + 1;
        width = Math.max(xRange, yRange); // largest size of box
        p = Math.log(width)/Math.log(2.0); // number of generations
        
        pCeil = (int)Math.ceil(p);
        width = (int)Math.round(Math.pow(2.0, pCeil));
        c = new byte[width][width];
        for (y = yLow; y <= yHigh; y++) {
            for (x = xLow; x <= xHigh; x++) {
                index = x + y * xDim;
                if ((entireImage || mask.get(index)) && (imgBuffer[index] > 0)) {
                    c[x - xLow][y - yLow] = 1;    
                } // if ((entireImage || mask.get(index)) && (imgBuffer[index] > 0))
            } // for (x = xLow; x <= xHigh; x++)
        } // for (y = yLow; y <= yHigh; y++)
        imgBuffer = null;
        
        // Preallocate the number of boxes of size r
        n = new int[pCeil+1];
        r = new int[pCeil+1];
        nInit = new int[pCeil+1];
        nInit[pCeil] = numPoints;
        for (g = pCeil-1; g >= 0; g--) {
            siz = (int)Math.round(Math.pow(2.0, pCeil-g));
            siz2 = (int)Math.round(siz/2.0);
            for (i = 0; i < width-siz+1; i += siz) {
                for (j = 0; j < width-siz+1; j += siz) {
                    if ((c[i+siz2][j] == 1) || (c[i][j+siz2] == 1) || (c[i+siz2][j+siz2] == 1)) {
                        c[i][j] = 1;
                    }
                }
            }
            for (i = 0; i < width-siz+1; i+= siz) {
                for (j = 0; j < width-siz+1; j += siz) {
                    if (c[i][j] == 1) {
                        nInit[g]++;
                    }
                }
            }
        } // for (g = pCeil-1; g >= 0; g--)
        
        ln = new double[n.length];
        lr = new double[n.length];
        for (i = 0; i < n.length; i++) {
            n[i] = nInit[n.length-1-i];
            ln[i] = Math.log(n[i]);
            if (i == 0) {
                r[i] = 1;
            }
            else {
                r[i] = 2 * r[i-1]; // box size (1, 2, 4, 8, ...)
            }
            lr[i] = Math.log(r[i]);
        }
        
        gradn = new double[n.length];
        gradr = new double[n.length];
        localFD = new double[n.length];
        gradn[0] = ln[1] - ln[0];
        gradr[0] = lr[1] - lr[0];
        gradn[n.length-1] = ln[n.length-1] - ln[n.length-2];
        gradr[n.length-1] = lr[n.length-1] - lr[n.length-2];
        for (i = 1; i < n.length-1; i++) {
            gradn[i] = (ln[i+1] - ln[i-1])/2.0;
            gradr[i] = (lr[i+1] - lr[i-1])/2.0;
        }
        
        for (i = 0; i < n.length; i++) {
            localFD[i] = -gradn[i]/gradr[i];
        }
        
        // log(n) = slope * log(r) + intercept
        // fd = -slope
        // slope = (sumXiYi - sumXi*sumYi/n)/(sumXiSquared - sumXi*sumXi/n)
        sumxy = 0.0;
        sumx = 0.0;
        sumy = 0.0;
        sumxx = 0.0;
        for (i = 0; i < n.length; i++) {
            sumxy += lr[i]*ln[i];
            sumx += lr[i];
            sumy += ln[i];
            sumxx += lr[i]*lr[i];
        }
        bestFitFD = -(sumxy - sumx*sumy/n.length)/(sumxx - sumx*sumx/n.length);
        for (i = 0; i < n.length; i++) {
            UI.setDataText("Box size = " + r[i] + " Box number = " + n[i] + " Local fractal dimension = " + localFD[i] + "\n");
            Preferences.debug("Box size = " + r[i] + " Box number = " + n[i] + " Local fractal dimension = " + localFD[i] + "\n",
                               Preferences.DEBUG_ALGORITHM);
        }
        UI.setDataText("Best fit fractal dimension = " + bestFitFD + "\n");
        Preferences.debug("Best fit fractal dimension = " + bestFitFD + "\n", Preferences.DEBUG_ALGORITHM);
        setCompleted(true);
        return;
    }
    
    private void boxCount3D() {
        int xDim = srcImage.getExtents()[0];
        int yDim = srcImage.getExtents()[1];
        int zDim = srcImage.getExtents()[2];
        int sliceSize = xDim * yDim;
        int volSize = sliceSize * zDim;
        BitSet mask = null;
        short imgBuffer[] = null;
        int x;
        int y;
        int z;
        int xLow = Integer.MAX_VALUE;
        int xHigh = Integer.MIN_VALUE;
        int yLow = Integer.MAX_VALUE;
        int yHigh = Integer.MIN_VALUE;
        int zLow = Integer.MAX_VALUE;
        int zHigh = Integer.MIN_VALUE;
        int index;
        int xRange;
        int yRange;
        int zRange;
        int width;
        double p;
        int pCeil;
        byte c[][][];
        int i;
        int g;
        int siz;
        int siz2;
        int j;
        int k;
        int nInit[];
        double ln[];
        double lr[];
        double gradn[];
        double gradr[];
        double sumxy;
        double sumx;
        double sumy;
        double sumxx;
        double bestFitFD;
        int numPoints = 0;
        int n[] = null;
        int r[] = null;
        double localFD[] = null;
        ViewUserInterface UI = ViewUserInterface.getReference();
        
        
        if (!entireImage) {
            mask = srcImage.generateVOIMask();
        }
        imgBuffer = new short[volSize];
        
        try {
            srcImage.exportData(0, volSize, imgBuffer);
        }
        catch(IOException error) {
            displayError("IOException " + error + " on srcImage.exportData(0, volSize, imgBuffer)");
            setCompleted(false);
            return;
        }
        
        index = -1;
        for (z = 0; z < zDim; z++) {
            for (y = 0; y < yDim; y++) {
                for (x = 0; x < xDim; x++) {
                    index++;
                    if ((entireImage || mask.get(index)) && (imgBuffer[index] > 0)) {
                        if (x < xLow) {
                            xLow = x;
                        }
                        if (x > xHigh) {
                            xHigh = x;
                        }
                        if (y < yLow) {
                            yLow = y;
                        }
                        if (y > yHigh) {
                            yHigh = y;
                        }
                        if (z < zLow) {
                            zLow = z;
                        }
                        if (z > zHigh) {
                            zHigh = z;
                        }
                        numPoints++;
                    } // if ((entireImage || mask.get(index)) && (imgBuffer[index] > 0))
                } // for (x = 0; x < xDim; x++)
            } // for (y = 0; y < yDim; y++)
        } // for (z = 0; z < zDim; z++)
        
        xRange = xHigh - xLow + 1;
        yRange = yHigh - yLow + 1;
        zRange = zHigh - zLow + 1;
        width = Math.max(xRange, Math.max(yRange, zRange)); // largest size of box
        p = Math.log(width)/Math.log(2.0); // number of generations
        
        pCeil = (int)Math.ceil(p);
        width = (int)Math.round(Math.pow(2.0, pCeil));
        c = new byte[width][width][width];
        for (z = zLow; z <= zHigh; z++) {
            for (y = yLow; y <= yHigh; y++) {
                for (x = xLow; x <= xHigh; x++) {
                    index = x + y * xDim + z*sliceSize;
                    if ((entireImage || mask.get(index)) && (imgBuffer[index] > 0)) {
                        c[x - xLow][y - yLow][z - zLow] = 1;    
                    } // if ((entireImage || mask.get(index)) && (imgBuffer[index] > 0))
                } // for (x = xLow; x <= xHigh; x++)
            } // for (y = yLow; y <= yHigh; y++)
        } // for (z = zLow; z <= zHigh; z++)
        imgBuffer = null;
        
        // Preallocate the number of boxes of size r
        n = new int[pCeil+1];
        r = new int[pCeil+1];
        nInit = new int[pCeil+1];
        nInit[pCeil] = numPoints;
        for (g = pCeil-1; g >= 0; g--) {
            siz = (int)Math.round(Math.pow(2.0, pCeil-g));
            siz2 = (int)Math.round(siz/2.0);
            for (i = 0; i < width-siz+1; i += siz) {
                for (j = 0; j < width-siz+1; j += siz) {
                    for (k = 0; k < width-siz+1; k += siz) {
                        if ((c[i+siz2][j][k] == 1) || (c[i][j+siz2][k] == 1) || (c[i+siz2][j+siz2][k] == 1) ||
                            (c[i][j][k+siz2] == 1) || (c[i+siz2][j][k+siz2] == 1) || (c[i][j+siz2][k+siz2] == 1) ||
                            (c[i+siz2][j+siz2][k+siz2] == 1)) {
                            c[i][j][k] = 1;
                        }
                    }
                }
            }
            for (i = 0; i < width-siz+1; i+= siz) {
                for (j = 0; j < width-siz+1; j += siz) {
                    for (k = 0; k < width-siz+1; k += siz) {
                        if (c[i][j][k] == 1) {
                            nInit[g]++;
                        }
                    } 
                }
            }
        } // for (g = pCeil-1; g >= 0; g--)
        
        ln = new double[n.length];
        lr = new double[n.length];
        for (i = 0; i < n.length; i++) {
            n[i] = nInit[n.length-1-i];
            ln[i] = Math.log(n[i]);
            if (i == 0) {
                r[i] = 1;
            }
            else {
                r[i] = 2 * r[i-1]; // box size (1, 2, 4, 8, ...)
            }
            lr[i] = Math.log(r[i]);
        }
        
        gradn = new double[n.length];
        gradr = new double[n.length];
        localFD = new double[n.length];
        gradn[0] = ln[1] - ln[0];
        gradr[0] = lr[1] - lr[0];
        gradn[n.length-1] = ln[n.length-1] - ln[n.length-2];
        gradr[n.length-1] = lr[n.length-1] - lr[n.length-2];
        for (i = 1; i < n.length-1; i++) {
            gradn[i] = (ln[i+1] - ln[i-1])/2.0;
            gradr[i] = (lr[i+1] - lr[i-1])/2.0;
        }
        
        for (i = 0; i < n.length; i++) {
            localFD[i] = -gradn[i]/gradr[i];
        }
        
        // log(n) = slope * log(r) + intercept
        // fd = -slope
        // slope = (sumXiYi - sumXi*sumYi/n)/(sumXiSquared - sumXi*sumXi/n)
        sumxy = 0.0;
        sumx = 0.0;
        sumy = 0.0;
        sumxx = 0.0;
        for (i = 0; i < n.length; i++) {
            sumxy += lr[i]*ln[i];
            sumx += lr[i];
            sumy += ln[i];
            sumxx += lr[i]*lr[i];
        }
        bestFitFD = -(sumxy - sumx*sumy/n.length)/(sumxx - sumx*sumx/n.length);
        for (i = 0; i < n.length; i++) {
            UI.setDataText("Box size = " + r[i] + " Box number = " + n[i] + " Local fractal dimension = " + localFD[i] + "\n");
            Preferences.debug("Box size = " + r[i] + " Box number = " + n[i] + " Local fractal dimension = " + localFD[i] + "\n",
                               Preferences.DEBUG_ALGORITHM);
        }
        UI.setDataText("Best fit fractal dimension = " + bestFitFD + "\n");
        Preferences.debug("Best fit fractal dimension = " + bestFitFD + "\n", Preferences.DEBUG_ALGORITHM);
        setCompleted(true);
        return;
    }

        

}