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
    
    // Also ported is the file randcantor.m revision 2.00, written by F. Moisy on 11/222/2006
    // Generates generalized random Cantor set
    // randcantor generates a logical 2D or 3D array of size n^d, containing a set of fractally distributed
    // 1 where d is the dimension 2 or 2 and the size n must be a power of 2.  The resulting set c is
    // obtained by iteratively dividing an initial set filled with 1 ito 2^d subsets, multiplying each
    // by 0 with probability p (with 0 < p < 1).  The resulting array c has a fractal dimension
    // df = d + log(p)/log(2) < d.
    // p = 0.8 yields df = 1.68 for 2D image.
    
    private boolean entireImage;
    private boolean createTestImage = false;
    RandomNumberGen randomGen;
    
    public AlgorithmBoxCount(ModelImage srcImg, boolean entireImage) {
        super(null, srcImg);
        this.entireImage = entireImage;
    }
    
    /**
     * Starts the program.
     */
    public void runAlgorithm() {
        
        if (createTestImage) {
            create3DRandImage();
            return;
        }

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
    
    private void create2DRandImage() {
        double p = 0.8;
        int n = 1024;
        byte buffer[][] = null;
        randomGen = new RandomNumberGen();
        buffer = randcantor2D(p, n);
        String imageName = "randcantor2D";
        int extents[] = new int[2];
        extents[0] = n;
        extents[1] = n;
        ModelImage randImage = new ModelImage(ModelStorageBase.BYTE, extents, imageName);
        byte buffer2[] = new byte[n * n];
        int x;
        int y;
        for (y = 0; y < n; y++) {
            for (x = 0; x < n; x++) {
                buffer2[x + y * n] = buffer[y][x];
            }
        }
        try {
            randImage.importData(0, buffer2, true);
        }
        catch(IOException e) {
            displayError("IOException " + e + "on randImage.importData(0, buffer2, true");
            setCompleted(false);
            return;
        }
        new ViewJFrameImage(randImage);
        setCompleted(true);
        return;
    }
    
    /**
     * 
     * @param p (0 < p < 1)
     * @param n must be a power of 2
     * @return
     */
    private byte[][] randcantor2D(double p, int n) {
        int i;
        int j;
        byte c[][] = new byte[n][n];
        for (i = 0; i < n; i++) {
            for (j = 0; j < n; j++) {
                c[i][j] = 1;
            }
        }
        boxdiv2(c, p);
        return c;
    }
    
    private void boxdiv2(byte c[][], double p) {
        int siz = c.length;
        int siz2;
        double rand;
        int i;
        int j;
        byte cin[][];
        if (siz == 1) {
            c[0][0] = 1;
        }
        else {
            siz2 = (int)Math.round(siz/2.0);
            cin = new byte[siz2][siz2];
            // sub-square top-left
            rand = randomGen.genUniformRandomNum(0.0, 1.0);
            if (rand >= p) {
                for (i = 0; i < siz2; i++) {
                    for (j = 0; j < siz2; j++) {
                        c[i][j] = 0;
                    }
                }
            } // if (rand >= p)
            if (c[0][0] == 1) {
                for (i = 0; i < siz2; i++) {
                    for (j = 0; j < siz2; j++) {
                        cin[i][j] = c[i][j];
                    }
                }
                boxdiv2(cin, p);
                for (i = 0; i < siz2; i++) {
                    for (j = 0; j < siz2; j++) {
                        c[i][j] = cin[i][j];
                    }
                }
            } // if (c[0][0] == 1)
            
            // sub-square top right
            rand  = randomGen.genUniformRandomNum(0.0, 1.0);
            if (rand >= p) {
                for (i = siz2; i < siz; i++) {
                    for (j = 0; j < siz2; j++) {
                        c[i][j] = 0;
                    }
                }
            } // if (rand >= p)
            if (c[siz2][0] == 1) {
                for (i = siz2; i < siz; i++) {
                    for (j = 0; j < siz2; j++) {
                        cin[i-siz2][j] = c[i][j];
                    }
                }
                boxdiv2(cin, p);
                for (i = siz2; i < siz; i++) {
                    for (j = 0; j < siz2; j++) {
                        c[i][j] = cin[i-siz2][j];
                    }
                }
            } // if (c[siz2][0] == 1)
            
            // sub-square bottom-left
            rand = randomGen.genUniformRandomNum(0.0, 1.0);
            if (rand >= p) {
                for (i = 0; i < siz2; i++) {
                    for (j = siz2; j < siz; j++) {
                        c[i][j] = 0;
                    }
                }
            } // if (rand >= p)
            if (c[0][siz2] == 1) {
                for (i = 0; i < siz2; i++) {
                    for (j = siz2; j < siz; j++) {
                        cin[i][j-siz2] = c[i][j];
                    }
                }
                boxdiv2(cin, p);
                for (i = 0; i < siz2; i++) {
                    for (j = siz2; j < siz; j++) {
                        c[i][j] = cin[i][j-siz2];
                    }
                }
            } // if (c[0][siz2] == 1)
            
            // sub-square bottom-right
            rand = randomGen.genUniformRandomNum(0.0, 1.0);
            if (rand >= p) {
                for (i = siz2; i < siz; i++) {
                    for (j = siz2; j < siz; j++) {
                        c[i][j] = 0;
                    }
                }
            } // if (rand >= p)
            if (c[siz2][siz2] == 1) {
                for (i = siz2; i < siz; i++) {
                    for (j = siz2; j < siz; j++) {
                        cin[i-siz2][j-siz2] = c[i][j];
                    }
                }
                boxdiv2(cin, p);
                for (i = siz2; i < siz; i++) {
                    for (j = siz2; j < siz; j++) {
                        c[i][j] = cin[i-siz2][j-siz2];
                    }
                }
            } // if (c[siz2][siz2] == 1)
        } // else siz > 1
    }
    
    private void create3DRandImage() {
        double p = 0.8;
        int n = 256;
        byte buffer[][][] = null;
        randomGen = new RandomNumberGen();
        buffer = randcantor3D(p, n);
        String imageName = "randcantor3D";
        int extents[] = new int[3];
        extents[0] = n;
        extents[1] = n;
        extents[2] = n;
        ModelImage randImage = new ModelImage(ModelStorageBase.BYTE, extents, imageName);
        byte buffer2[] = new byte[n * n * n];
        int x;
        int y;
        int z;
        int nsq = n*n;
        for (z = 0; z < n; z++) {
            for (y = 0; y < n; y++) {
                for (x = 0; x < n; x++) {
                    buffer2[x + y * n + z * nsq] = buffer[z][y][x];
                }
            }
        }
        try {
            randImage.importData(0, buffer2, true);
        }
        catch(IOException e) {
            displayError("IOException " + e + "on randImage.importData(0, buffer2, true");
            setCompleted(false);
            return;
        }
        new ViewJFrameImage(randImage);
        setCompleted(true);
        return;
    }
    
    /**
     * 
     * @param p (0 < p < 1)
     * @param n must be a power of 2
     * @return
     */
    private byte[][][] randcantor3D(double p, int n) {
        int i;
        int j;
        int k;
        byte c[][][] = new byte[n][n][n];
        for (i = 0; i < n; i++) {
            for (j = 0; j < n; j++) {
                for (k = 0; k < n; k++) {
                   c[i][j][k] = 1;
                }
            }
        }
        boxdiv3(c, p);
        return c;
    }
    
    private void boxdiv3(byte c[][][], double p) {
        int siz = c.length;
        int siz2;
        double rand;
        int i;
        int j;
        int k;
        byte cin[][][];
        if (siz == 1) {
            c[0][0][0] = 1;
        }
        else {
            siz2 = (int)Math.round(siz/2.0);
            cin = new byte[siz2][siz2][siz2];
            // sub-cube top-left front
            rand = randomGen.genUniformRandomNum(0.0, 1.0);
            if (rand >= p) {
                for (i = 0; i < siz2; i++) {
                    for (j = 0; j < siz2; j++) {
                        for (k = 0; k < siz2; k++) {   
                            c[i][j][k] = 0;
                        }
                    }
                }
            } // if (rand >= p)
            if (c[0][0][0] == 1) {
                for (i = 0; i < siz2; i++) {
                    for (j = 0; j < siz2; j++) {
                        for (k = 0; k < siz2; k++) {
                            cin[i][j][k] = c[i][j][k];
                        }
                    }
                }
                boxdiv3(cin, p);
                for (i = 0; i < siz2; i++) {
                    for (j = 0; j < siz2; j++) {
                        for (k = 0; k < siz2; k++) {
                            c[i][j][k] = cin[i][j][k];
                        }
                    }
                }
            } // if (c[0][0][0] == 1)
            
            // sub-cube top right front
            rand  = randomGen.genUniformRandomNum(0.0, 1.0);
            if (rand >= p) {
                for (i = siz2; i < siz; i++) {
                    for (j = 0; j < siz2; j++) {
                        for (k = 0; k < siz2; k++) {
                            c[i][j][k] = 0;
                        }
                    }
                }
            } // if (rand >= p)
            if (c[siz2][0][0] == 1) {
                for (i = siz2; i < siz; i++) {
                    for (j = 0; j < siz2; j++) {
                        for (k = 0; k < siz2; k++) {
                            cin[i-siz2][j][k] = c[i][j][k];
                        }
                    }
                }
                boxdiv3(cin, p);
                for (i = siz2; i < siz; i++) {
                    for (j = 0; j < siz2; j++) {
                        for (k = 0; k < siz2; k++) {
                            c[i][j][k] = cin[i-siz2][j][k];
                        }
                    }
                }
            } // if (c[siz2][0][0] == 1)
            
            // sub-cube bottom-left front
            rand = randomGen.genUniformRandomNum(0.0, 1.0);
            if (rand >= p) {
                for (i = 0; i < siz2; i++) {
                    for (j = siz2; j < siz; j++) {
                        for (k = 0; k < siz2; k++) {   
                            c[i][j][k] = 0;
                        }
                    }
                }
            } // if (rand >= p)
            if (c[0][siz2][0] == 1) {
                for (i = 0; i < siz2; i++) {
                    for (j = siz2; j < siz; j++) {
                        for (k = 0; k < siz2; k++) {
                            cin[i][j-siz2][k] = c[i][j][k];
                        }
                    }
                }
                boxdiv3(cin, p);
                for (i = 0; i < siz2; i++) {
                    for (j = siz2; j < siz; j++) {
                        for (k = 0; k < siz2; k++) {
                            c[i][j][k] = cin[i][j-siz2][k];
                        }
                    }
                }
            } // if (c[0][siz2][0] == 1)
            
            // sub-cube bottom-right front
            rand = randomGen.genUniformRandomNum(0.0, 1.0);
            if (rand >= p) {
                for (i = siz2; i < siz; i++) {
                    for (j = siz2; j < siz; j++) {
                        for (k = 0; k < siz2; k++) {
                            c[i][j][k] = 0;
                        }
                    }
                }
            } // if (rand >= p)
            if (c[siz2][siz2][0] == 1) {
                for (i = siz2; i < siz; i++) {
                    for (j = siz2; j < siz; j++) {
                        for (k = 0; k < siz2; k++) {
                            cin[i-siz2][j-siz2][k] = c[i][j][k];
                        }
                    }
                }
                boxdiv3(cin, p);
                for (i = siz2; i < siz; i++) {
                    for (j = siz2; j < siz; j++) {
                        for (k = 0; k < siz2; k++) {
                            c[i][j][k] = cin[i-siz2][j-siz2][k];
                        }
                    }
                }
            } // if (c[siz2][siz2][0] == 1)
            
            // sub-cube top-left bottom
            rand = randomGen.genUniformRandomNum(0.0, 1.0);
            if (rand >= p) {
                for (i = 0; i < siz2; i++) {
                    for (j = 0; j < siz2; j++) {
                        for (k = siz2; k < siz; k++) {   
                            c[i][j][k] = 0;
                        }
                    }
                }
            } // if (rand >= p)
            if (c[0][0][siz2] == 1) {
                for (i = 0; i < siz2; i++) {
                    for (j = 0; j < siz2; j++) {
                        for (k = siz2; k < siz; k++) {
                            cin[i][j][k-siz2] = c[i][j][k];
                        }
                    }
                }
                boxdiv3(cin, p);
                for (i = 0; i < siz2; i++) {
                    for (j = 0; j < siz2; j++) {
                        for (k = siz2; k < siz; k++) {
                            c[i][j][k] = cin[i][j][k-siz2];
                        }
                    }
                }
            } // if (c[0][0][siz2] == 1)
            
            // sub-cube top right bottom
            rand  = randomGen.genUniformRandomNum(0.0, 1.0);
            if (rand >= p) {
                for (i = siz2; i < siz; i++) {
                    for (j = 0; j < siz2; j++) {
                        for (k = siz2; k < siz; k++) {
                            c[i][j][k] = 0;
                        }
                    }
                }
            } // if (rand >= p)
            if (c[siz2][0][siz2] == 1) {
                for (i = siz2; i < siz; i++) {
                    for (j = 0; j < siz2; j++) {
                        for (k = siz2; k < siz; k++) {
                            cin[i-siz2][j][k-siz2] = c[i][j][k];
                        }
                    }
                }
                boxdiv3(cin, p);
                for (i = siz2; i < siz; i++) {
                    for (j = 0; j < siz2; j++) {
                        for (k = siz2; k < siz; k++) {
                            c[i][j][k] = cin[i-siz2][j][k-siz2];
                        }
                    }
                }
            } // if (c[siz2][0][siz2] == 1)
            
            // sub-cube bottom-left bottom
            rand = randomGen.genUniformRandomNum(0.0, 1.0);
            if (rand >= p) {
                for (i = 0; i < siz2; i++) {
                    for (j = siz2; j < siz; j++) {
                        for (k = siz2; k < siz; k++) {   
                            c[i][j][k] = 0;
                        }
                    }
                }
            } // if (rand >= p)
            if (c[0][siz2][siz2] == 1) {
                for (i = 0; i < siz2; i++) {
                    for (j = siz2; j < siz; j++) {
                        for (k = siz2; k < siz; k++) {
                            cin[i][j-siz2][k-siz2] = c[i][j][k];
                        }
                    }
                }
                boxdiv3(cin, p);
                for (i = 0; i < siz2; i++) {
                    for (j = siz2; j < siz; j++) {
                        for (k = siz2; k < siz; k++) {
                            c[i][j][k] = cin[i][j-siz2][k-siz2];
                        }
                    }
                }
            } // if (c[0][siz2][siz2] == 1)
            
            // sub-cube bottom-right bottom
            rand = randomGen.genUniformRandomNum(0.0, 1.0);
            if (rand >= p) {
                for (i = siz2; i < siz; i++) {
                    for (j = siz2; j < siz; j++) {
                        for (k = siz2; k < siz; k++) {
                            c[i][j][k] = 0;
                        }
                    }
                }
            } // if (rand >= p)
            if (c[siz2][siz2][siz2] == 1) {
                for (i = siz2; i < siz; i++) {
                    for (j = siz2; j < siz; j++) {
                        for (k = siz2; k < siz; k++) {
                            cin[i-siz2][j-siz2][k-siz2] = c[i][j][k];
                        }
                    }
                }
                boxdiv3(cin, p);
                for (i = siz2; i < siz; i++) {
                    for (j = siz2; j < siz; j++) {
                        for (k = siz2; k < siz; k++) {
                            c[i][j][k] = cin[i-siz2][j-siz2][k-siz2];
                        }
                    }
                }
            } // if (c[siz2][siz2][siz2] == 1)
        } // else siz > 1
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
        sumxy = 0.0;
        sumx = 0.0;
        sumy = 0.0;
        sumxx = 0.0;
        for (i = 0; i < n.length-3; i++) {
            sumxy += lr[i]*ln[i];
            sumx += lr[i];
            sumy += ln[i];
            sumxx += lr[i]*lr[i];
        }
        bestFitFD = -(sumxy - sumx*sumy/(n.length-3))/(sumxx - sumx*sumx/(n.length-3));
        UI.setDataText("Best fit fractal dimension excluding 3 largest box sizes = " + bestFitFD + "\n");
        Preferences.debug("Best fit fractal dimension excluding 3 largest box sizes = " + bestFitFD + "\n", Preferences.DEBUG_ALGORITHM);
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
        sumxy = 0.0;
        sumx = 0.0;
        sumy = 0.0;
        sumxx = 0.0;
        for (i = 0; i < n.length-3; i++) {
            sumxy += lr[i]*ln[i];
            sumx += lr[i];
            sumy += ln[i];
            sumxx += lr[i]*lr[i];
        }
        bestFitFD = -(sumxy - sumx*sumy/(n.length-3))/(sumxx - sumx*sumx/(n.length-3));
        UI.setDataText("Best fit fractal dimension excluding 3 largest box sizes = " + bestFitFD + "\n");
        Preferences.debug("Best fit fractal dimension excluding 3 largest box sizes = " + bestFitFD + "\n", Preferences.DEBUG_ALGORITHM);
        setCompleted(true);
        return;
    }

        

}