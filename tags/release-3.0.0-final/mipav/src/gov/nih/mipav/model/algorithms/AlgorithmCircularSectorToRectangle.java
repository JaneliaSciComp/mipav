package gov.nih.mipav.model.algorithms;


import gov.nih.mipav.model.structures.*;

import gov.nih.mipav.view.*;

import java.io.*;


/**
 * This software performs a 2D conformal mapping of a circular sector defined by 4 user points at the sector corners to
 * a rectangle of user specified size. The circular sector has an inner radius rmin, an outer radius rmax, and extends
 * over an angle theta = alpha * PI radians, with 0 < alpha <= 1. In a conformal mapping in any small neighborhood the
 * relative angle and shape are preserved. z1 is the upper right point on rmax z2 is the upper left point on rmax z3 is
 * the lower left point on rmin z4 is the lower right point on rmin The mapping to the user specified retangle is
 * performed in 2 steps. First, a conformal mapping to a rectangle of width 1 and height log(rmax/rmin)/theta occurs.
 * Then, this rectangle is linearly scaled to a rectangle of user specified xDim and yDim.
 *
 * <p>Let z4z1 be on the real axis, let the straight line z2z3 be inclined at an angle alpha*PI, 0 < alpha <= 1, z3 and
 * z4 are both on the radius rmin, and z1 and z2 are both on the radius rmax. For the conformal mapping we require that
 * rmax > 1 and 0 < rmin < 1. So divide all the distances by sqrt(rmax * rmin). Then the radii go from sqrt(rmin/rmax)
 * to sqrt(rmax/rmin).</p>
 *
 * <p>To go from a circular sector to a rectangle use the transformation w = f(z) = u + iv = log(z)/(i*alpha*PI) Let z =
 * r*exp(i*theta) Then, w = log(r*exp(i*theta))/(i*alpha*PI) = theta/(alpha*PI) - i*log(r)/(alpha*PI) so u =
 * theta/(alpha*PI), v = -log(r)/(alpha*PI)</p>
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
public class AlgorithmCircularSectorToRectangle extends AlgorithmBase {

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** DOCUMENT ME! */
    private double[] x;

    /** DOCUMENT ME! */
    private double[] y;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * AlgorithmCircularSectorToRectangle - default constructor.
     */
    public AlgorithmCircularSectorToRectangle() { }

    /**
     * AlgorithmCircularSectorToRectangle.
     *
     * @param  destImg  DOCUMENT ME!
     * @param  srcImg   DOCUMENT ME!
     * @param  x        array with x coordinates of 4 sector boundary points
     * @param  y        array with y coordinates of 4 sector boundary points
     */
    public AlgorithmCircularSectorToRectangle(ModelImage destImg, ModelImage srcImg, double[] x, double[] y) {
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

        double xc, yc;

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

        fireProgressStateChanged(srcImage.getImageName(), "Circular sector to rectangle ...");

        if (srcImage.isColorImage()) {
            cf = 4;
        } else {
            cf = 1;
        }

        x1 = x[0];
        x2 = x[1];
        x3 = x[2];
        x4 = x[3];
        y1 = y[0];
        y2 = y[1];
        y3 = y[2];
        y4 = y[3];
        // Calculate center point of circle
        // (yc - y4)/(xc - x4) = (y4 - y1)/(x4 - x1)
        // yc - y4 = (xc - x4) * (y4 - y1)/(x4 - x1)
        // yc = (y4 - y1)/(x4 - x1) * xc + (y4 - x4*(y4 - y1)/(x4 - x1))
        // yc = (y4 - y1)/(x4 - x1) * xc + (x4*y1 - x1*y4)/(x4 - x1)

        // (yc - y3)/(xc - x3) = (y3 - y2)/(x3 - x2)
        // yc - y3 = (xc - x3) * (y3 - y2)/(x3 - x2)
        // yc = (y3 - y2)/(x3 - x2) * xc + (y3 - x3*(y3 - y2)/(x3 - x2))
        // yc = (y3 - y2)/(x3 - x2) * xc + (x3*y2 - x2*y3)/(x3 - x2)

        // xc*((y4 - y1)/(x4 - x1) - (y3 - y2)/(x3 - x2)) +
        // ((x4*y1 - x1*y4)/(x4 - x1) - (x3*y2 - x2*y3)/(x3 - x2)) = 0

        xc = (((x3 * y2) - (x2 * y3)) / (x3 - x2)) - (((x4 * y1) - (x1 * y4)) / (x4 - x1));
        xc = xc / (((y4 - y1) / (x4 - x1)) - ((y3 - y2) / (x3 - x2)));

        yc = ((y4 - y1) / (x4 - x1) * xc) + (((x4 * y1) - (x1 * y4)) / (x4 - x1));
        Preferences.debug("x center = " + xc + " y center = " + yc + "\n");

        rmin = Math.sqrt(((x4 - xc) * (x4 - xc)) + ((y4 - yc) * (y4 - yc)));

        rmax = Math.sqrt(((x1 - xc) * (x1 - xc)) + ((y1 - yc) * (y1 - yc)));
        Preferences.debug("rmin = " + rmin + " rmax = " + rmax + "\n");

        // Calculate angle along line from center to z4 to z1 in -PI to PI radians
        theta1 = Math.atan2((y1 - y4), (x1 - x4));

        // Calculate angle along line from center to z3 to z2 in -PI to PI radians
        theta2 = Math.atan2((y2 - y3), (x2 - x3));

        // Angle of sector in radians
        // theta = alpha * PI
        theta = theta2 - theta1;

        alpha = theta / Math.PI;
        Preferences.debug("alpha = " + alpha + "\n");

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
        // x axis is inverted and the x and y axes are scaled.
        // Map from z1" = (xDimDest-1, 0) to z1' = (0,-log(sqrt(rmax/rmin))/theta)
        // Map from z2" = (0, 0) to z2' = (1,-log(sqrt(rmax/rmin))/theta)
        // Map from z3" = (0, yDimDest-1) to z3' = (1, log(sqrt(rmax/rmin))/theta)
        // Map from z4" = (xDimDest-1, yDimDest-1) to z4' = (0, log(sqrt(rmax/rmin))/theta)

        // x" = (1 - x')*(xDim - 1)
        // 1 - x' = x"/(xDim - 1)
        // x' - 1 = -x"/(xDim - 1)
        // x' = 1 - x"/(xDim - 1)
        // var = (log(sqrt(rmax/rmin))/theta
        // y" = (y' + var) * (yDim - 1)/(2 * var)
        // y' + var = (2 * var * y")/(yDim - 1)
        // y' = (2 * var * y")/(yDim - 1) - var

        // Mapping 2 occurs from a rectangle with width 1 and length log(rmax/rmin)/theta.
        // to a circular sector with angle theta, minimum radius rmin, and maximum
        // radius rmax
        // z1 = zc + rmax
        // z2 = zc + rmax*exp(i*theta)
        // z3 = zc + rmin*exp(i*theta)
        // z4 = zc + rmin
        var = 0.5 * Math.log(rmax / rmin) / theta;
        rscale = Math.sqrt(rmax * rmin);

        for (j = 0; j < yDimDest; j++) {
            fireProgressStateChanged(100 * j / yDimDest);
            index1 = j * xDimDest;
            yp = ((2.0 * var * j) / (yDimDest - 1)) - var;

            // v = -log(r)/(alpha *PI) -> r = exp(-theta*v), then scale by sqrt(rmax/rmin).
            r = rscale * Math.exp(-theta * yp);

            for (i = 0; i < xDimDest; i++) {
                xp = 1.0 - ((double) i / (xDimDest - 1));

                // u = theta/(alpha*PI) -> theta = alpha * PI * u, then add in theta1.
                ang = (theta * xp) + theta1;
                xSrc = xc + (r * Math.cos(ang));
                ySrc = yc + (r * Math.sin(ang));

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
     * Constructs a string of the contruction parameters and out puts the string to the messsage frame if the history
     * logging procedure is turned on.
     */
    private void constructLog() {


        historyString = new String("CircularSectorToRectangle(" + String.valueOf(destImage.getExtents()[0]) + ", " +
                                   String.valueOf(destImage.getExtents()[1]) + ")\n");
    }
}
