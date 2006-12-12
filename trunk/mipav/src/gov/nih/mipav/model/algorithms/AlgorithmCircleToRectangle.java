package gov.nih.mipav.model.algorithms;


import gov.nih.mipav.model.structures.*;

import gov.nih.mipav.view.*;

import java.io.*;


/**
 
 *
 
 * <p>References: 1.) Advanced Calculus For Applications Second Edition by F. B. Hildebrand, Section 10.4 Analytic
 * Functions of a Complex Variable pages 550-554 and Section 11.4 Conformal Mapping pages 628-632, Prentice-Hall, Inc.,
 * 1976. 2.) Applied and Computational Complex Analysis Volume I by Peter Henrici, Chapter on Schwarz-Christoffel
 * Mapping Function, pp. 411-412. 3.) 2D-Shape Analysis using COnformal Mapping by E. Sharon and D. Mumford
 * 4.) http://mathworld.wolfram.com/LemniscateFunction.html
 */
public class AlgorithmCircleToRectangle extends AlgorithmBase {

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** DOCUMENT ME! */
    private double[] x;

    /** DOCUMENT ME! */
    private double[] y;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * AlgorithmCircularSectorToRectangle - default constructor.
     */
    public AlgorithmCircleToRectangle() { }

    /**
     * AlgorithmCircularSectorToRectangle.
     *
     * @param  destImg  DOCUMENT ME!
     * @param  srcImg   DOCUMENT ME!
     * @param  x        array with x coordinates of 4 sector boundary points
     * @param  y        array with y coordinates of 4 sector boundary points
     */
    public AlgorithmCircleToRectangle(ModelImage destImg, ModelImage srcImg, double[] x, double[] y) {
        super(destImg, srcImg);
        this.x = x;
        this.y = y;
    }

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * finalize -
     */
    public void finalize() {
        x = null;
        y = null;
        super.finalize();
    }

    /**
     * Starts the program.
     */
    public void runAlgorithm() {
        double x2, y2;
        
        double radius;
        
        Gamma gamm;
        double g1[] = new double[1];
        double g2[] = new double[2];
        double g3[] = new double[3];
        double d;

        double xc, yc;
        double sq2;
        double invSq2;
        JacobianElliptic je;
        double xr[] = new double[1];
        double yr[] = new double[1];
        double xmid;
        double ymid;
        double prod;

        int xDimSource;

        int yDimSource;

        int sourceSlice;

        int i, j;
        int index, index1;

        int xDimDest;
        int yDimDest;
        int destSlice;
        float[] srcBuffer;
        float[] destBuffer;
        double yp;
        double xp;
        double ySrc;
        double xSrc;
        float imageMin;
        int xBase;
        float delX;
        int yBase;
        float delY;
        int sIndex;
        int cf;

        if (srcImage == null) {
            displayError("Source Image is null");
            finalize();

            return;
        }

        constructLog();

        fireProgressStateChanged(srcImage.getImageName(), "Circle to rectangle ...");

        if (srcImage.isColorImage()) {
            cf = 4;
        } else {
            cf = 1;
        }

        xc = x[0];
        x2 = x[1];
        yc = y[0];
        y2 = y[1];
        
        Preferences.debug("x center = " + xc + " y center = " + yc + "\n");
        
        radius = Math.sqrt((x2-xc)*(x2-xc) + (y2-yc)*(y2-yc));
        Preferences.debug("radius = " + radius + "\n");
        
        // Let n = the number of sides in a polygon
        // Let wk be evenly spaced points on a unit circle and zk be the corner points of a polygon
        // zk = d*exp(2*PI*i*k/n)
        // z = c*integral from 0 to w of product from k = 1 to n of (w - wk)**(-2/n) dw maps from the inside of 
        // the unit circle to the inside of a polygon with wk corresponding to zk
        // c = gamma(1 - 1/n)*d/(gamma(1 + 1/n)*gamma(1 - 2/n))
        // For the case of a square :
        // z = c*integral from to w of dw/sqrt(1-w**4)
        // Letting d = Gamma(5/4)*Gamma(1/2)/Gamma(3/4) will make c equal to 1 for ease of computation.
        // The inverse lemniscate function phi(x) = sinlemn-1(x) = integral from 0 to w of dw/sqrt(1-w**4)
        // The inverse function maps from the circle to the square, but a reverse mapping from the square
        // to the circle is needed, so use sinelmn(phi) = (1/sqrt(2))*sd(phi*sqrt(2),1/sqrt(2))
        gamm = new Gamma(1.25, g1);
        gamm.run();
        gamm = new Gamma(0.5, g2);
        gamm.run();
        gamm = new Gamma(0.75, g3);
        gamm.run();
        d = g1[0]*g2[0]/g3[0];
        // Corners of the square are at distance d from center of square
        sq2 = Math.sqrt(2.0);
        invSq2 = 1.0/sq2;

        xDimSource = srcImage.getExtents()[0];
        yDimSource = srcImage.getExtents()[1];
        sourceSlice = xDimSource * yDimSource;

        xDimDest = destImage.getExtents()[0];
        yDimDest = destImage.getExtents()[1];
        destSlice = xDimDest * yDimDest;
        srcBuffer = new float[cf * sourceSlice];

        try {
            srcImage.exportData(0, cf * sourceSlice, srcBuffer);
        } catch (IOException e) {
            MipavUtil.displayError("IOException " + e + " on srcImage.exportData");

            setCompleted(false);

            return;
        }

        destBuffer = new float[cf * destSlice];

        if (!srcImage.isColorImage()) {
            imageMin = (float) srcImage.getMin();

            for (i = 0; i < destSlice; i++) {
                destBuffer[i] = imageMin;
            }
        } // if (!srcImage.isColorImage())

        // 2 mappings
        // Mapping 1 is simply a mapping from 1 rectangle to another in which the
        // y axis is inverted and the x and y axes are scaled.
        // Map from z0" = (xDimDest-1, (yDimDest-1)/2.0) to z0' = (d, 0);
        // Map from z1" = ((xDimDest-1)/2, 0) to z2' = (0, d)
        // Map from z2" = (0, (yDimDest-1)/2.0) to z3' = (-d, 0)
        // Map from z3" = ((xDimDest-1)/2.0, yDimDest-1) to z4' = (0, -d)

        // a*(xDimDest - 1) + b = d
        // a*0 + b = -d
        // a*(xDimDest - 1) = 2*d
        // a = 2*d/(xDimDest - 1)
        // b = -d
        // x' = 2*d*x"/(xDimDest - 1) - d
        
        // a*(yDimDest - 1) + b = -d
        // a*0 + b = d
        // a*(yDimDest - 1) = -2*d
        // a = -2*d/(yDimDest - 1)
        // b = d
        // y' = -2*d*y'/(yDimDest - 1) + d
        
        // Mapping 2 occurs from a square with corners at +-d to the circle
        // (1/sqrt(2)) * sd(z'*sqrt(2),1/sqrt(2)) goes to a unit disc
        // Then radius * sd(z'*sqrt(2), 1/sqrt(2)) goes to a disc with the original radius
        // The original x is at xc + real part of [radius * sd(z'*sqrt(2), 1/sqrt(2))
        // The original y is at yc - imag part of [radius * sd(z'*sqrt(2), 1/sqrt(2))
        xmid = (xDimDest - 1.0)/2.0;
        ymid = (yDimDest - 1.0)/2.0;
        prod = xmid*ymid;
        for (j = 0; j < yDimDest; j++) {
            fireProgressStateChanged(100 * j / yDimDest);
            index1 = j * xDimDest;
            yp = ((-2.0 * d * j) / (yDimDest - 1)) + d;

            for (i = 0; i < xDimDest; i++) {
                // The destination rectangle is tilted at 45 degrees so it fills a
                // diamond in the destination image.  Points outside the diamond
                // must be excluded.
                if ((i*ymid + j*xmid >= prod) &&
                    (i*ymid - j*xmid <= prod) &&
                    (i*ymid + j*xmid <= 3*prod) &&
                    (i*ymid - j*xmid >= -prod)) {
                    xp = ((2.0 * d * i)/ (xDimDest - 1)) - d;
    
                    je = new JacobianElliptic(xp*sq2,yp*sq2,invSq2,JacobianElliptic.SD,xr,yr);
                    je.run();
                    xSrc = xc + xr[0]*radius*invSq2;
                    ySrc = yc - yr[0]*radius*invSq2;
    
                    // Use bilinear interpolation to find the contributions from the
                    // 4 nearest neighbors in the original circle space
                    if ((xSrc >= 0.0) && ((xSrc) <= (xDimSource - 1)) && (ySrc >= 0.0) && (ySrc <= (yDimSource - 1))) {
                        xBase = (int) Math.floor(xSrc);
                        delX = (float) (xSrc - xBase);
                        yBase = (int) Math.floor(ySrc);
                        delY = (float) (ySrc - yBase);
                        index = index1 + i;
                        sIndex = (yBase * xDimSource) + xBase;
    
                        if (srcImage.isColorImage()) {
                            destBuffer[(4 * index) + 1] = (1 - delX) * (1 - delY) * srcBuffer[(4 * sIndex) + 1];
                            destBuffer[(4 * index) + 2] = (1 - delX) * (1 - delY) * srcBuffer[(4 * sIndex) + 2];
                            destBuffer[(4 * index) + 3] = (1 - delX) * (1 - delY) * srcBuffer[(4 * sIndex) + 3];
    
                            if (xSrc < (xDimSource - 1)) {
                                destBuffer[(4 * index) + 1] += delX * (1 - delY) * srcBuffer[(4 * sIndex) + 1];
                                destBuffer[(4 * index) + 2] += delX * (1 - delY) * srcBuffer[(4 * sIndex) + 2];
                                destBuffer[(4 * index) + 3] += delX * (1 - delY) * srcBuffer[(4 * sIndex) + 3];
                            }
    
                            if (ySrc < (yDimSource - 1)) {
                                destBuffer[(4 * index) + 1] += (1 - delX) * delY *
                                                                   srcBuffer[(4 * (sIndex + xDimSource)) + 1];
                                destBuffer[(4 * index) + 2] += (1 - delX) * delY *
                                                                   srcBuffer[(4 * (sIndex + xDimSource)) + 2];
                                destBuffer[(4 * index) + 3] += (1 - delX) * delY *
                                                                   srcBuffer[(4 * (sIndex + xDimSource)) + 3];
                            }
    
                            if ((xSrc < (xDimSource - 1)) && (ySrc < (yDimSource - 1))) {
                                destBuffer[(4 * index) + 1] += delX * delY * srcBuffer[(4 * (sIndex + xDimSource + 1)) + 1];
                                destBuffer[(4 * index) + 2] += delX * delY * srcBuffer[(4 * (sIndex + xDimSource + 1)) + 2];
                                destBuffer[(4 * index) + 3] += delX * delY * srcBuffer[(4 * (sIndex + xDimSource + 1)) + 3];
                            }
                        } // if (srcImage.isColorImage())
                        else { // black and white image
                            destBuffer[index] = (1 - delX) * (1 - delY) * srcBuffer[sIndex];
    
                            if (xSrc < (xDimSource - 1)) {
                                destBuffer[index] += delX * (1 - delY) * srcBuffer[sIndex + 1];
                            }
    
                            if (ySrc < (yDimSource - 1)) {
                                destBuffer[index] += (1 - delX) * delY * srcBuffer[sIndex + xDimSource];
                            }
    
                            if ((xSrc < (xDimSource - 1)) && (ySrc < (yDimSource - 1))) {
                                destBuffer[index] += delX * delY * srcBuffer[sIndex + xDimSource + 1];
                            }
                        } // else black and white image
                    }
                }
            }
        } // for (j = 0; j < yDimDest; j++)

        try {
            destImage.importData(0, destBuffer, true);
        } catch (IOException e) {
            MipavUtil.displayError("IOException " + e + " on destImage.importData");

            setCompleted(false);

            return;
        }

        setCompleted(true);

        return;
    }

    /**
     * Constructs a string of the contruction parameters and out puts the string to the messsage frame if the history
     * logging procedure is turned on.
     */
    private void constructLog() {


        historyString = new String("CircleToRectangle(" + String.valueOf(destImage.getExtents()[0]) + ", " +
                                   String.valueOf(destImage.getExtents()[1]) + ")\n");
    }
}
