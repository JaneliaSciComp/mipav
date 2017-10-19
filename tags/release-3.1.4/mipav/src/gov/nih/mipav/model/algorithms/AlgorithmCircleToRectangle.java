package gov.nih.mipav.model.algorithms;


import gov.nih.mipav.model.structures.*;

import gov.nih.mipav.view.*;

import java.io.*;


/**
 
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
    public void runAlgorithmNext() {
        double x2, y2;
        
        double radius;
        double xc, yc;
        JacobianElliptic je;
        double xr[] = new double[1];
        double yr[] = new double[1];

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
        double uk;
        double klow;
        double kmid;
        double khigh;
        double first[] = new double[1];
        double second[] = new double[1];
        EllipticIntegral eInt;
        double knum;
        double kdenom;
        double ukmid;
        double eps;
        double xoff;
        double xw[] = new double[1];
        double yw[] = new double[1];
        double kSqrt;

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

        // 4 mappings
        // Mapping 1 from destination rectangle to a rectangle centered around the y axis
        // Mapping 1 from rectangle at (0,0), (xDimDest-1, 0), (xDimDest-1,yDimDest-1),
        // (0, yDimDest-1) to rectangle at (-(xDimDest-1)/2,0), ((xDimDest-1)/2,0), ((xDimDest-1)/2, yDimDest-1),
        // (-(xDimDest-1)/2, yDimDest-1).
        
        // Mapping 2 occurs from the y axis centered rectangle to the upper half plane
        
        // In mapping 3 go from the y centered rectangle to the unit circle
        
        // In mapping 4 scale and translate the unit circle to arrive at the destination circle
        // (xDimDest-1)/2.0/(yDimDest-1) = 2.0 * K(k)/K(sqrt(1 - k*k)), where K is the complete
        // elliptic integral of the first kind.
        //      Now how to find k:
        // u(k) = K(k)/K(sqrt(1 - k*k)) = 0.25*(xDimDest - 1.0)/(yDimDest = 1.0),
        // where u(k) increases from 0 to infinity as k increases from 0 to 1
        uk = 0.25 * (xDimDest - 1.0)/(yDimDest - 1.0);
        klow = 0.001;
        khigh = 0.999;
        kmid = (klow + khigh)/2.0;
        while (true) {
            eInt = new EllipticIntegral(Math.sqrt(1.0 - kmid*kmid), first, second); 
            eInt.run();
            kdenom = first[0];
            eInt = new EllipticIntegral(kmid, first, second);
            eInt.run();
            knum = first[0];
            ukmid = knum/kdenom;
            eps = Math.abs((ukmid - uk)/uk);
            if (eps < 1.0E-8) {
                break;
            }
            if (ukmid < uk) {
                klow = kmid;
            }
            else {
                khigh = kmid;
            }
            kmid = (klow + khigh)/2.0;
        }
        
        
        Preferences.debug("kmid = " + kmid + "\n");
        kSqrt = Math.sqrt(kmid);
        xoff = (xDimDest - 1.0)/2.0;
        for (j = 0; j < yDimDest; j++) {
            fireProgressStateChanged(100 * j / yDimDest);
            index1 = j * xDimDest;

            for (i = 0; i < xDimDest; i++) {
                // Translate to a rectangle centered around the y axis
                xp = i - xoff;
                // Conformal map from the y centered rectangle to the upper half plane
                // w = sn(z, k)
                // The sn function must be a (xDimDest - 1) periodic function and a
                // 2*i*(yDimDest - 1) periodic function ???
                je = new JacobianElliptic(xp,(double)j,kmid,JacobianElliptic.SN,xw,yw);
                je.run();
                Preferences.debug("xw[0] = " + xw[0] + " yw[0] = " + yw[0] + "\n");
                // Map from upper half plane to inside of unit circle
                // xi = (i - sqrt(k)*w)/(1.0 + sqrt(k)*w)
                zdiv(-kSqrt*xw[0], -kSqrt*yw[0] + 1.0, kSqrt*xw[0] + 1.0, kSqrt*yw[0], xr, yr);
                // Scale and translate from unit circle to user selected circle
                xSrc = radius*xr[0] + xc;
                ySrc = radius*yr[0] + yc;
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
        double xmul;
        double ymul;
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

        // 3 mappings
        // Mapping 1 from rectangle at (0,0), (xDimDest-1, 0), (xDimDest-1,yDimDest-1),
        // (0, yDimDest-1) to square at (0,d), (d,0), (0,-d), (-d,0).
        
        // The mapping used here rotates the rectangle by 45 degrees
        // Map from z0" = (0,0) to z0' = 0,d
        // Map from z1" = (xDimDest-1, 0) to z1' = (d, 0)
        // Map form z2" = (xDimDest-1, yDimDest-1) to z2' = (0,-d)
        // Map from z3" = (0, yDimDest-1) to z3' = (-d,0)
        // a11x" + a12y" + a13 = x'
        // a21x" + a22y" + a23 = y'
        
        // d*x"/(xDimDest-1) - d*y"/(yDimDest-1) = x'
        // -d*x"/(xDimDest-1) -d*y"/(yDimDest-1) + d = y'
        
        // Mapping 2 occurs from a square with corners at +-d to the circle
        // (1/sqrt(2)) * sd(z'*sqrt(2),1/sqrt(2)) goes to a unit disc
        // Then radius * (1/sqrt(2))*  sd(z'*sqrt(2), 1/sqrt(2)) goes to a disc with the original radius
        // The original unrotated x is at xc + real part of [radius * (1/sqrt(2)) *  sd(z'*sqrt(2), 1/sqrt(2))
        // The original unrotated y is at yc - imag part of [radius * (1/sqrt(2)) * sd(z'*sqrt(2), 1/sqrt(2))
        // In the unrotated circle
        // xSrc = xc + xr[0]*radius*invSq2;
        // ySrc = yc - yr[0]*radius*invSq2;
        // In mapping 3 rotate around circle by -45 degrees, so cosine = 1/sqrt(2), sin = 1/sqrt(2).
        // xSrc = xc + xr[0]*radius*invSqr*cosine(-45) - yr[0]*radius*invSqr*(-sin(-45))
        // ySrc = yc + xr[0]*radius*invSqr*sin(-45) - yr[0]*radius*invSqr*cos(-45)
        xmul = ((double)d)/((double)(xDimDest-1));
        ymul = ((double)d)/((double)(yDimDest-1));
        for (j = 0; j < yDimDest; j++) {
            fireProgressStateChanged(100 * j / yDimDest);
            index1 = j * xDimDest;

            for (i = 0; i < xDimDest; i++) {
                
                xp = i*xmul - j*ymul;
                yp = -i*xmul - j*ymul + d;
                je = new JacobianElliptic(xp*sq2,yp*sq2,invSq2,JacobianElliptic.SD,xr,yr);
                je.run();
                xSrc = xc + 0.5 * xr[0] * radius - 0.5 * yr[0] * radius;
                ySrc = yc - 0.5 * xr[0] * radius - 0.5 * yr[0] * radius;

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
     * Starts the program.
     */
    public void runTraditionalAlgorithm() {
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
        // The traditional mapping in which the 4 corners of the rectangle are on the
        // x and y axis.
        // Mapping 1 is simply a mapping from 1 rectangle to another in which the
        // y axis is inverted and the x and y axes are scaled.
        // Map from z0" = (xDimDest-1, (yDimDest-1)/2.0) to z0' = (d, 0);
        // Map from z1" = ((xDimDest-1)/2, 0) to z1' = (0, d)
        // Map from z2" = (0, (yDimDest-1)/2.0) to z2' = (-d, 0)
        // Map from z3" = ((xDimDest-1)/2.0, yDimDest-1) to z3' = (0, -d)

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
        // Then radius * (1/sqrt(2)) * sd(z'*sqrt(2), 1/sqrt(2)) goes to a disc with the original radius
        // The original x is at xc + real part of [radius * (1/sqrt(2)) * sd(z'*sqrt(2), 1/sqrt(2))
        // The original y is at yc - imag part of [radius * (1/sqrt(2)) * sd(z'*sqrt(2), 1/sqrt(2))
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
     * complex divide c = a/b.
     *
     * @param  ar  double
     * @param  ai  double
     * @param  br  double
     * @param  bi  double
     * @param  cr  double[]
     * @param  ci  double[]
     */
    private void zdiv(double ar, double ai, double br, double bi, double[] cr, double[] ci) {
        double bm, cc, cd, ca, cb;

        bm = 1.0 / zabs(br, bi);
        cc = br * bm;
        cd = bi * bm;
        ca = ((ar * cc) + (ai * cd)) * bm;
        cb = ((ai * cc) - (ar * cd)) * bm;
        cr[0] = ca;
        ci[0] = cb;

        return;
    }
    
    /**
     * zabs computes the absolute value or magnitude of a double precision complex variable zr + j*zi.
     *
     * @param   zr  double
     * @param   zi  double
     *
     * @return  double
     */
    private double zabs(double zr, double zi) {
        double u, v, q, s;
        u = Math.abs(zr);
        v = Math.abs(zi);
        s = u + v;

        // s * 1.0 makes an unnormalized underflow on CDC machines into a true
        // floating zero
        s = s * 1.0;

        if (s == 0.0) {
            return 0.0;
        } else if (u > v) {
            q = v / u;

            return (u * Math.sqrt(1.0 + (q * q)));
        } else {
            q = u / v;

            return (v * Math.sqrt(1.0 + (q * q)));
        }
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