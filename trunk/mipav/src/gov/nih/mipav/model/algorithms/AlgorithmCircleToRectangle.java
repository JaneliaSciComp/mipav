package gov.nih.mipav.model.algorithms;


import gov.nih.mipav.model.structures.*;

import gov.nih.mipav.view.*;

import java.io.*;


/**
 
 *
 * <p>For conformal mapping we require that f(z) be analytic - the Cauchy-Riemann equations must be satisfied and f'(z)
 * must not equal zero. The Cauchy-Riemann equations in polar form are: du/dr = (1/r)(dv/dtheta); (1/r)du/dtheta =
 * -dv/dr The first equation gives zero on both sides and the second equation gives (alpha*PI)/r on both sides, so the
 * Cauchy Riemann equations hold. f'(z) = 1/(i*alpha*PI*z) does not equal zero.</p>
 *
 * <p>In the first mapping: rmin -> sqrt(rmin/rmax) -> log(sqrt(rmin/rmax))/(i*alpha*PI) =
 * i*log(sqrt(rmax/rmin))/(alpha*PI) rmin*exp(i*alpha*PI) -> sqrt(rmin/rmax)*exp(i*alpha*PI) -> 1 +
 * i*log(sqrt(rmax/rmin))/(alpha*PI) rmax -> sqrt(rmax/rmin) -> log(sqrt(rmax/rmin))/(i*alpha*PI) =
 * -i*log(sqrt(rmax/rmin))/(alpha*PI) rmax*exp(i*alpha*PI) -> sqrt(rmax/rmin)*exp(i*alpha*PI) -> 1 -
 * i*log(sqrt(rmax/rmin))/(alpha*PI)</p>
 *
 * <p>Now do a simple linear transformation where both the x axis is inverted and both axes are scaled to obtain: z1" =
 * xDim-1, 0 z2" = 0, 0 z3" = 0, yDim-1 z4" = xDim - 1, yDim-1</p>
 *
 * <p>x" = (1 - x')*(xDim - 1) Let var = log(sqrt(rmax/rmin))/(alpha*PI) y" = (y' + var)* (yDim - 1)/(2 * var)</p>
 *
 * <p>References: 1.) Advanced Calculus For Applications Second Edition by F. B. Hildebrand, Section 10.4 Analytic
 * Functions of a Complex Variable pages 550-554 and Section 11.4 Conformal Mapping pages 628-632, Prentice-Hall, Inc.,
 * 1976. 2.) "A Domain Decomposition Method for Conformal Mapping onto a Rectangle", N. Papamichael and N. S.
 * Stylianopoulos, Constructive Approximation, Vol. 7, 1991, pp. 349-379. Relevant result is given in Remark 4.7 on
 * pages 374-375.</p>
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
        double x1, x2, x3, x4, y1, y2, y3, y4;
        
        double radius;
        
        Gamma gamm;
        double g1[] = new double[1];
        double g2[] = new double[2];
        double g3[] = new double[3];
        double d;
        double sqd;

        double xc, yc;
        double sq2;
        double invSq2;
        JacobianElliptic je;
        double xr[] = new double[1];
        double yr[] = new double[1];

        double rmin, rmax;

        double theta1, theta2;

        // Angle of sector
        double theta;

        double alpha;

        int xDimSource;

        int yDimSource;

        int sourceSlice;

        int i, j;
        int index, index1;
        double r;
        double ang;

        int xDimDest;
        int yDimDest;
        int destSlice;
        float[] srcBuffer;
        float[] destBuffer;
        double var;
        double yp;
        double xp;
        double rscale;
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
        
        // z = c*integral from 0 to w of dw/sqrt(1-w**4) maps from the unit circle to
        // zk = dk*exp(2*PI*i*k/n), where n equal the number of sides of the polygon = 4
        // c = gamma(1 - 1/n)*d/(gamma(1 + 1/n)*gamma(1 - 2/n))
        // Letting d = Gamma(5/4)*Gamma(1/2)/Gamma(3/4) will make c equal to 1.
        gamm = new Gamma(1.25, g1);
        gamm.run();
        gamm = new Gamma(0.5, g2);
        gamm.run();
        gamm = new Gamma(0.75, g3);
        gamm.run();
        d = g1[0]*g2[0]/g3[0];
        // Corners of the square are at distance d from center of square
        // With center of unit square at origin, square segments are a
        // distance of sqd = d/sqrt(2) from the origin.
        sq2 = Math.sqrt(2.0);
        invSq2 = 1.0/sq2;
        sqd = d/sq2;

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
        // Map from z0" = (xDimDest-1, 0) to z0' = (sqd,sqd)
        // Map from z1" = (0, 0) to z2' = (-sqd,sqd)
        // Map from z2" = (0, yDimDest-1) to z3' = (-sqd,-sqd )
        // Map from z3" = (xDimDest-1, yDimDest-1) to z4' = (sqd, -sqd)

        // a*(xDimDest - 1) + b = sqd
        // a*0 + b = -sqd
        // a*(xDimDest - 1) = 2*sqd
        // a = 2*sqd/(xDimDest - 1)
        // b = -sqd
        // x' = 2*sqd*x"/(xDimDest - 1) - sqd
        
        // a*(yDimDest - 1) + b = -sqd
        // a*0 + b = sqd
        // a*(yDimDest - 1) = -2*sqd
        // a = -2*sqd/(yDimDest - 1)
        // b = sqd
        // y' = -2*sqd*y'/(yDimDest - 1) + sqd
        
        // Mapping 2 occurs from a square with sides at +-sqd to the circle
        // (1/sqrt(2)) * sd(z'*sqrt(2),1/sqrt(2)) goes to a unit disc with the angle theta 
        // starting at PI/4 radians.  
        // Then radius * exp(-i*PI/4) * sd(z'*sqrt(2), 1/sqrt(2)) goes to a disc with the original radius
        // with the angle theta starting at zero radians
        // The original x is at xc + real part of [radius * exp(-i*PI/4) * sd(z'*sqrt(2), 1/sqrt(2))
        // The original y is at yc - imag part of [radius * exp(-i*PI/4) * sd(z'*sqrt(2), 1/sqrt(2))
        
        for (j = 0; j < yDimDest; j++) {
            fireProgressStateChanged(100 * j / yDimDest);
            index1 = j * xDimDest;
            yp = ((-2.0 * sqd * j) / (yDimDest - 1)) + sqd;

            for (i = 0; i < xDimDest; i++) {
                xp = ((2.0 * sqd * i)/ (xDimDest - 1)) - sqd;

                je = new JacobianElliptic(xp*sq2,yp*sq2,invSq2,JacobianElliptic.SD,xr,yr);
                je.run();
                zmlt(radius*invSq2*xr[0], radius*invSq2*yr[0], invSq2, -invSq2, xr, yr);
                xSrc = xc + xr[0];
                ySrc = yc - yr[0];

                // Use bilinear interpolation to find the contributions from the
                // 4 nearest neighbors in the original circular sector space
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
     * complex multiply c = a * b.
     *
     * @param  ar  double
     * @param  ai  double
     * @param  br  double
     * @param  bi  double
     * @param  cr  double[]
     * @param  ci  double[]
     */
    private void zmlt(double ar, double ai, double br, double bi, double[] cr, double[] ci) {
        double ca, cb;

        ca = (ar * br) - (ai * bi);
        cb = (ar * bi) + (ai * br);
        cr[0] = ca;
        ci[0] = cb;

        return;
    }

    /**
     * Constructs a string of the contruction parameters and out puts the string to the messsage frame if the history
     * logging procedure is turned on.
     */
    private void constructLog() {


        historyString = new String("CircularSectorToRectangle(" + String.valueOf(destImage.getExtents()[0]) + ", " +
                                   String.valueOf(destImage.getExtents()[1]) + ")\n");
    }
}
