package gov.nih.mipav.model.algorithms;


import gov.nih.mipav.model.algorithms.utilities.AlgorithmFlip;
import gov.nih.mipav.model.algorithms.utilities.AlgorithmRotate;
import gov.nih.mipav.model.structures.*;

import gov.nih.mipav.view.*;

import java.io.*;


/**
 * This software uses 2D conformal mapping in converting a circular sector defined by 4 user points at the sector corners
 * to a rectangle of user specified size. The circular sector has an inner radius rmin, an outer radius rmax, and extends
 * over an angle theta = alpha * PI radians, with 0 < alpha <= 1. In a conformal mapping in any small neighborhood the
 * relative angle and shape are preserved. The points z1, z2, z3, and z4 are entered in a counterclockwise order with
 * z1 and z2 on rmax and z3 and z4 on rmin.  For the case of rmax pointing to the top of the image, z1 is the upper
 * right point on rmax, z2 is the upper left point on rmax, z3 is the lower left point on rmin, and z4 is the lower
 * right point on rmin. The mapping to the user specified rectangle is performed in 3 steps. First, all distances from
 * the circular center are divided by sqrt(rmax * rmin).  Second, a conformal mapping to a rectangle of width 1 and
 * height log(rmax/rmin)/theta occurs.  Then, this rectangle is linearly scaled to a rectangle of user specified xDim
 * and yDim.  Note that the linear scaling of one rectangle to another is not a conformal mapping unless the width and
 * height are scaled by the same factor.  From "Lectures on Quasiconformal Mappings" second edition by Lars V. Ahlfors:
 * "If Q is a square and R is a rectangle, not a square, there is no conformal mapping of Q on R which maps vertices
 * on vertices."
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
 * A similar transformation is performed in Conformal Mapping by Zeev Nehari Chapter VI Mapping Properties of Special 
 * Functions 2. Exponential and Trigonometric Functios pp. 273-280.  Nehari states that "the transformation w = exp(z)
 * maps the boundary of any rectangle in the z-plane whose sides are parallel to the axes onto a closed contour 
 * consisting of two circular arcs about the origin and two linear segments pointing at the origin."
 *
 * <p>References: 1.) Advanced Calculus For Applications Second Edition by F. B. Hildebrand, Section 10.4 Analytic
 * Functions of a Complex Variable pages 550-554 and Section 11.4 Conformal Mapping pages 628-632, Prentice-Hall, Inc.,
 * 1976. 2.) "A Domain Decomposition Method for Conformal Mapping onto a Rectangle", N. Papamichael and N. S.
 * Stylianopoulos, Constructive Approximation, Vol. 7, 1991, pp. 349-379. Relevant result is given in Remark 4.7 on
 * pages 374-375.</p>  3.) Conformal Mapping by Zeev Nehari Chapter VI Mapping Properties of Special Functions 2.
 * Exponential and Trigonometric Functions pp. 273-280.
 */
public class AlgorithmCircularSectorToRectangle extends AlgorithmBase {

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** DOCUMENT ME! */
    private double[] x;

    /** DOCUMENT ME! */
    private double[] y;
    
    private int testOrientation;
    private final int testTop = 1;
    private final int testBottom = 2;
    private final int testRight = 3;
    private final int testLeft = 4;

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
        double r = 0.0;
        double ang;

        int xDimDest;
        int yDimDest;
        int destSlice;
        float[] srcBuffer;
        float[] destBuffer;
        double var;
        double yp = 0.0;
        double xp = 0.0;
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
        boolean test = false;
        boolean maxRadiusTop = false;
        boolean maxRadiusBottom = false;
        boolean maxRadiusRight = false;
        boolean maxRadiusLeft = false;
        
        
        if (test) {
            testOrientation = testTop;
            selfTest3();
        }

        if (srcImage == null) {
            displayError("Source Image is null");
            finalize();

            return;
        }

        

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
        if ((x1 > x2) && (x4 > x3) && (y3 > y2) && (y4 > y1)) {
            maxRadiusTop = true;
        }
        else if ((x2 > x1) && (x3 > x4) && (y2 > y3) && (y1 > y4)) {
            maxRadiusBottom = true;
        }
        else if ((x1 > x4) && (x2 > x3) && (y1 > y2) && (y4 > y3)) {
            maxRadiusRight = true;
        }
        else {
            maxRadiusLeft = true;
        }
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
        Preferences.debug("x center = " + xc + " y center = " + yc + "\n", Preferences.DEBUG_ALGORITHM);

        rmin = Math.sqrt(((x4 - xc) * (x4 - xc)) + ((y4 - yc) * (y4 - yc)));

        rmax = Math.sqrt(((x1 - xc) * (x1 - xc)) + ((y1 - yc) * (y1 - yc)));
        Preferences.debug("rmin = " + rmin + " rmax = " + rmax + "\n", Preferences.DEBUG_ALGORITHM);

        // Calculate angle along line from center to z4 to z1 in -PI to PI radians
        theta1 = Math.atan2((y1 - y4), (x1 - x4));

        // Calculate angle along line from center to z3 to z2 in -PI to PI radians
        theta2 = Math.atan2((y2 - y3), (x2 - x3));

        // Angle of sector in radians
        // theta = alpha * PI
        if (maxRadiusLeft) {
            // Discontinuity of angle from -PI to PI at negative x axis
            theta = Math.abs(theta1) + Math.abs(theta2) - 2.0 * Math.PI;    
        }
        else {
            theta = theta2 - theta1;
        }

        alpha = theta / Math.PI;
        Preferences.debug("alpha = " + alpha + "\n", Preferences.DEBUG_ALGORITHM);

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
        // Consider case of maxRadiusTop
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
            if (maxRadiusTop) {
                yp = ((2.0 * var * j) / (yDimDest - 1)) - var;
                r = rscale * Math.exp(-theta * yp);
            }
            else if (maxRadiusBottom){
                yp = ((-2.0 * var * j) / (yDimDest - 1)) + var;
                r = rscale * Math.exp(-theta * yp);
            }
            else if (maxRadiusRight) {
                xp = 1.0 - ((double)j/(yDimDest - 1));
                ang = (theta * xp) + theta1;
            }
            else { // maxRadiusLeft
                xp = ((double)j/(yDimDest - 1));
                ang = (theta * xp) + theta1;
            }

            // v = -log(r)/(alpha *PI) -> r = exp(-theta*v), then scale by sqrt(rmax/rmin).
            

            for (i = 0; i < xDimDest; i++) {
                if (maxRadiusTop) {
                    xp = 1.0 - ((double) i / (xDimDest - 1));
                    ang = (theta * xp) + theta1;
                }
                else if (maxRadiusBottom) {
                    xp = ((double)i/ (xDimDest - 1));
                    ang = (theta * xp) + theta1;
                }
                else if (maxRadiusRight) {
                    yp = ((-2.0 * var * i)/ (xDimDest - 1)) + var;
                    r = rscale * Math.exp(-theta * yp);
                }
                else { // maxRadiusLeft
                    yp = ((2.0 * var * i)/ (xDimDest - 1)) - var;
                    r = rscale * Math.exp(-theta * yp);    
                }

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
    
    @SuppressWarnings("unused")
    private void selfTest() {
        // Maximum radius on top originally
        // Put maximum radius on bottom with testOrientation == testBottom
        // Put maximum radius on right with testOrientation == testRight
        // Put maximum radius on left with testOrienation == testLeft
        // Call up JDialogCircularSectorToRectangle with any image
        // In a 512 x 512 image create an image which is a circular sector going from -45 degrees to 45 degrees with
        // white -45 to -40
        // black -40 to -35
        // white -35 to -30
        // black -30 to -25
        // white -25 to -20
        // black -20 to -15
        // white -15 to -10
        // black -10 to -5
        // white -5 to +5
        // black +5 to +10
        // white +10 to +15
        // black +15 to +20
        // white +20 to +25
        // black +25 to +30
        // white +30 to +35
        // black +35 to +40
        // white +40 to +45
        // Center point of circle at 255.5, 255.5
        // z1, upper right point = 411, 100 at theta = 45 degrees on rmax = 155.5*sqrt(2) = 219.91020894
        // z2, upper left point = 100, 100 at theta = -45 degrees on rmax = 155.5*sqrt(2) = 219.91020894
        // z3, lower left point = 180, 180 at theta = -45 degrees on rmin = 75.5*sqrt(2) = 106.7731239
        // z4, lower right point = 331, 180 at theta = +45 degrees on rmin = 75.5*sqrt(2) = 106.7731239
        // Top point of circular sector at 255.5, 35.58979105 at theta = 0 degrees
        // White and black radial segements in input image transformed in to vertical white and black in output image
        int xDim = 512;
        int yDim = 512;
        int sliceSize = xDim * yDim;
        int extents[] = new int[2];
        extents[0] = xDim;
        extents[1] = yDim;
        byte buffer[] = new byte[sliceSize];
        int xs;
        int ys;
        int index;
        double xDist;
        double yDist;
        double r;
        double theta;
        double thetaAbs;
        AlgorithmFlip flipx;
        AlgorithmRotate rotateAlgo;
        double tempX;
        double tempY;
        double xCen;
        double yCen;
        int i;
        double tMax = Math.PI/4.0;
        for (ys = 35; ys <= 180; ys++) {
            for (xs = 100; xs <= 411; xs++) {
                index = xs + ys * xDim;
                xDist = xs - 255.5;
                yDist = 255.5 - ys;
                r = Math.sqrt(xDist*xDist + yDist*yDist);
                // Note here theta = 0 for xDist = 0
                theta = Math.atan2(xDist, yDist);
                thetaAbs = Math.abs(theta);
                if ((r >= 106.7731239) && (r <= 219.91020894)) {
                    if ((thetaAbs <= tMax) && (thetaAbs > (tMax * (40.0/45.0)))) {
                        buffer[index] = (byte)255;  
                    }
                    else if ((thetaAbs <= (tMax * (35.0/45.0))) && (thetaAbs > (tMax * (30.0/45.0)))) {
                        buffer[index] = (byte)255;
                    }
                    else if ((thetaAbs <= (tMax * (25.0/45.0))) && (thetaAbs > (tMax * (20.0/45.0)))) {
                        buffer[index] = (byte)255;
                    }
                    else if ((thetaAbs <= (tMax * (15.0/45.0))) && (thetaAbs > (tMax * (10.0/45.0)))) {
                        buffer[index] = (byte)255;
                    }
                    else if (thetaAbs <= (tMax * (5.0/45.0))) {
                        buffer[index] = (byte)255;
                    }
                } // if ((r >= 106.7731239) && (r <= 219.91020894))
            } // for (x = 100; x <= 411; x++)
        } // for (y = 36; y <= 180; y++)
        if ((srcImage.getExtents()[0] != extents[0]) || (srcImage.getExtents()[1] != extents[1])) {
            srcImage.changeExtents(extents);
            srcImage.recomputeDataSize();
        }
        srcImage.getParentFrame().dispose();
        srcImage.getVOIs().removeAllElements();
        try {
            srcImage.importData(0, buffer, true);
        }
        catch(IOException e) {
            MipavUtil.displayError("IOException on srcImage.importData");
        }
        
        x = new double[4];
        y = new double[4];
        x[0] = 411.0;
        x[1] = 100.0;
        x[2] = 180.0;
        x[3] = 331.0;
        y[0] = 100.0;
        y[1] = 100.0;
        y[2] = 180.0;
        y[3] = 180.0;
        
        if (testOrientation == testBottom) {
            flipx = new AlgorithmFlip(srcImage, AlgorithmFlip.X_AXIS, AlgorithmFlip.IMAGE, true);
            flipx.run();
            flipx.finalize();
            flipx = null;
            // 1 -> 0, 0 -> 1, 2 -> 3, 3 -> 2
            tempX = x[1];
            tempY = extents[1] - 1 - y[1];
            x[1] = x[0];
            y[1] = extents[1] - 1 - y[0];
            x[0] = tempX;
            y[0] = tempY;
            
            tempX = x[3];
            tempY = extents[1] - 1 - y[3];
            x[3] = x[2];
            y[3] = extents[1] - 1 - y[2];
            x[2] = tempX;
            y[2] = tempY;
        } // if (testOrientation == testBottom)
        else if (testOrientation == testRight) {
            xCen = (xDim - 1.0)/2.0;
            yCen = (yDim - 1.0)/2.0;
            for (i = 0; i < 4; i++) {
                x[i] = x[i] - xCen;
                y[i] = y[i] - yCen;
                tempX = -y[i];
                y[i] = x[i];
                x[i] = tempX;
                x[i] = x[i] + xCen;
                y[i] = y[i] + yCen;
            }
            rotateAlgo = new AlgorithmRotate(srcImage, AlgorithmRotate.Z_AXIS_PLUS);
            rotateAlgo.run();
            srcImage.disposeLocal();
            srcImage = null;
            srcImage = rotateAlgo.returnImage();
            rotateAlgo.finalize();
            rotateAlgo = null;  
        } // else if (testOrientation == testRight)
        else if (testOrientation == testLeft) {
            xCen = (xDim - 1.0)/2.0;
            yCen = (yDim - 1.0)/2.0;
            for (i = 0; i < 4; i++) {
                x[i] = x[i] - xCen;
                y[i] = y[i] - yCen;
                tempX = y[i];
                y[i] = -x[i];
                x[i] = tempX;
                x[i] = x[i] + xCen;
                y[i] = y[i] + yCen;
            }
            rotateAlgo = new AlgorithmRotate(srcImage, AlgorithmRotate.Z_AXIS_MINUS);
            rotateAlgo.run();
            srcImage.disposeLocal();
            srcImage = null;
            srcImage = rotateAlgo.returnImage();
            rotateAlgo.finalize();
            rotateAlgo = null;      
        } // else if (testOrientation == testLeft)
        
        new ViewJFrameImage(srcImage);
    }
    
    @SuppressWarnings("unused")
    private void selfTest2() {
        // Maximum radius on top originally
        // Put maximum radius on bottom with testOrientation == testBottom
        // Put maximum radius on left with testOrienation == testLeft
        // Call up JDialogCircularSectorToRectangle with any image
        // In a 512 x 512 image create an image which is a circular sector going from -45 degrees to 45 degrees with
        // rmin to rmin + .1*(rmax - rmin) on
        // rmin + .1*(rmax - rmin) to rmin + .2*(rmax - rmin) off
        // rmin + .2*(rmax - rmin) to rmin + .3*(rmax - rmin) on
        // rmin + .3*(rmax - rmin) to rmin + .4*(rmax - rmin) off
        // rmin + .4*(rmax - rmin) to rmin + .5*(rmax - rmin) on
        // rmin + .5*(rmax - rmin) to rmin + .6*(rmax - rmin) off
        // rmin + .6*(rmax - rmin) to rmin + .7*(rmax - rmin) on
        // rmin + .7*(rmax - rmin) to rmin + .8*(rmax - rmin) off
        // rmin + .8*(rmax - rmin) to rmin + .9*(rmax - rmin) on
        // rmin + .9*(rmax - rmin) to rmax off
        // Center point of circle at 255.5, 255.5
        // z1, upper right point = 411, 100 at theta = 45 degrees on rmax = 155.5*sqrt(2) = 219.91020894
        // z2, upper left point = 100, 100 at theta = -45 degrees on rmax = 155.5*sqrt(2) = 219.91020894
        // z3, lower left point = 180, 180 at theta = -45 degrees on rmin = 75.5*sqrt(2) = 106.7731239
        // z4, lower right point = 331, 180 at theta = +45 degrees on rmin = 75.5*sqrt(2) = 106.7731239
        // Top point of circular sector at 255.5, 35.58979105 at theta = 0 degrees
        // White and black circular segments transformed into white and black horizontal lines
        int xDim = 512;
        int yDim = 512;
        int sliceSize = xDim * yDim;
        int extents[] = new int[2];
        extents[0] = xDim;
        extents[1] = yDim;
        byte buffer[] = new byte[sliceSize];
        int xs;
        int ys;
        int index;
        double xDist;
        double yDist;
        double r;
        double theta;
        double thetaAbs;
        AlgorithmFlip flipx;
        AlgorithmRotate rotateAlgo;
        double tempX;
        double tempY;
        double xCen;
        double yCen;
        int i;
        double tMax = Math.PI/4.0;
        double rmax = 155.5*Math.sqrt(2.0);
        double rmin = 75.5*Math.sqrt(2.0);
        double delr = rmax - rmin;
        for (ys = 35; ys <= 180; ys++) {
            for (xs = 100; xs <= 411; xs++) {
                index = xs + ys * xDim;
                xDist = xs - 255.5;
                yDist = 255.5 - ys;
                r = Math.sqrt(xDist*xDist + yDist*yDist);
                // Note here theta = 0 for xDist = 0
                theta = Math.atan2(xDist, yDist);
                thetaAbs = Math.abs(theta);
                if (thetaAbs <= tMax) {
                    if ((r >= rmin) && (r < rmin + 0.1*delr)) {
                        buffer[index] = (byte)255;
                    }
                    else if ((r >= rmin + 0.2*delr) && (r < rmin + 0.3*delr)) {
                        buffer[index] = (byte)255;
                    }
                    else if ((r >= rmin + 0.4*delr) && (r < rmin + 0.5*delr)) {
                        buffer[index] = (byte)255;
                    }
                    else if ((r >= rmin + 0.6*delr) && (r < rmin + 0.7*delr)) {
                        buffer[index] = (byte)255;
                    }
                    else if ((r >= rmin + 0.8*delr) && (r < rmin + 0.9*delr)) {
                        buffer[index] = (byte)255;
                    }
                }
            } // for (x = 100; x <= 411; x++)
        } // for (y = 36; y <= 180; y++)
        if ((srcImage.getExtents()[0] != extents[0]) || (srcImage.getExtents()[1] != extents[1])) {
            srcImage.changeExtents(extents);
            srcImage.recomputeDataSize();
        }
        srcImage.getParentFrame().dispose();
        srcImage.getVOIs().removeAllElements();
        try {
            srcImage.importData(0, buffer, true);
        }
        catch(IOException e) {
            MipavUtil.displayError("IOException on srcImage.importData");
        }
        
        x = new double[4];
        y = new double[4];
        x[0] = 411.0;
        x[1] = 100.0;
        x[2] = 180.0;
        x[3] = 331.0;
        y[0] = 100.0;
        y[1] = 100.0;
        y[2] = 180.0;
        y[3] = 180.0;
        
        if (testOrientation == testBottom) {
            flipx = new AlgorithmFlip(srcImage, AlgorithmFlip.X_AXIS, AlgorithmFlip.IMAGE, true);
            flipx.run();
            flipx.finalize();
            flipx = null;
            // 1 -> 0, 0 -> 1, 2 -> 3, 3 -> 2
            tempX = x[1];
            tempY = extents[1] - 1 - y[1];
            x[1] = x[0];
            y[1] = extents[1] - 1 - y[0];
            x[0] = tempX;
            y[0] = tempY;
            
            tempX = x[3];
            tempY = extents[1] - 1 - y[3];
            x[3] = x[2];
            y[3] = extents[1] - 1 - y[2];
            x[2] = tempX;
            y[2] = tempY;
        } // if (testOrientation == testBottom)
        else if (testOrientation == testRight) {
            xCen = (xDim - 1.0)/2.0;
            yCen = (yDim - 1.0)/2.0;
            for (i = 0; i < 4; i++) {
                x[i] = x[i] - xCen;
                y[i] = y[i] - yCen;
                tempX = -y[i];
                y[i] = x[i];
                x[i] = tempX;
                x[i] = x[i] + xCen;
                y[i] = y[i] + yCen;
            }
            rotateAlgo = new AlgorithmRotate(srcImage, AlgorithmRotate.Z_AXIS_PLUS);
            rotateAlgo.run();
            srcImage.disposeLocal();
            srcImage = null;
            srcImage = rotateAlgo.returnImage();
            rotateAlgo.finalize();
            rotateAlgo = null;  
        } // else if (testOrientation == testRight)
        else if (testOrientation == testLeft) {
            xCen = (xDim - 1.0)/2.0;
            yCen = (yDim - 1.0)/2.0;
            for (i = 0; i < 4; i++) {
                x[i] = x[i] - xCen;
                y[i] = y[i] - yCen;
                tempX = y[i];
                y[i] = -x[i];
                x[i] = tempX;
                x[i] = x[i] + xCen;
                y[i] = y[i] + yCen;
            }
            rotateAlgo = new AlgorithmRotate(srcImage, AlgorithmRotate.Z_AXIS_MINUS);
            rotateAlgo.run();
            srcImage.disposeLocal();
            srcImage = null;
            srcImage = rotateAlgo.returnImage();
            rotateAlgo.finalize();
            rotateAlgo = null;      
        } // else if (testOrientation == testLeft)
        
        new ViewJFrameImage(srcImage);
    }
    
    private void selfTest3() {
        // Maximum radius on top originally
        // Put maximum radius on bottom with testOrientation == testBottom
        // Put maximum radius on right with testOrientation == testRight
        // Put maximum radius on left with testOrienation == testLeft
        // Call up JDialogCircularSectorToRectangle with any image
        // In a 512 x 512 image create an image which is a circular sector going from -45 degrees to 45 degrees with
        // intersecting perpindicular lines in source image.
        // Should see intersecting perpindicular lines in transformed image if the scaling between the 2 rectangles
        // is the same in x and y.
        // In testTop and testBottom a rectangle of width xDim and length yDim is transformed to a rectangle of width 1 and 
        // length log(rmax/rmin)/theta.  So for equal scaling in x and y must make yDim = log(rmax/rmin)/theta * xDim;
        // In testRight and testLeft a rectangle of width xDim and length yDim is transformed to a rectangle where the
        // length yDim is transformed to the width 1 and the width xDim is transformed to the length log(rmax/rmin)/theta.
        // So for equal scaling in x and y must make xDim = log(rmax/rmin)/theta * yDim;
        int xDim = 512;
        int yDim = 512;
        int sliceSize = xDim * yDim;
        int extents[] = new int[2];
        extents[0] = xDim;
        extents[1] = yDim;
        int destExtents[] = new int[2];
        byte buffer[] = new byte[sliceSize];
        int xs;
        int ys;
        int index;
        double xDist;
        double yDist;
        double r;
        double theta;
        double thetaAbs;
        AlgorithmFlip flipx;
        AlgorithmRotate rotateAlgo;
        double tempX;
        double tempY;
        double xCen;
        double yCen;
        int i;
        double tMax = Math.PI/4.0;
        for (ys = 35; ys <= 180; ys++) {
            for (xs = 100; xs <= 411; xs++) {
                index = xs + ys * xDim;
                xDist = xs - 255.5;
                yDist = 255.5 - ys;
                r = Math.sqrt(xDist*xDist + yDist*yDist);
                // Note here theta = 0 for xDist = 0
                theta = Math.atan2(xDist, yDist);
                thetaAbs = Math.abs(theta);
                if ((xs == 150) || (xs == 200) || (xs == 250) || (xs == 300) || (xs == 350) ||
                    (ys == 50) || (ys == 75) || (ys == 100) || (ys == 125) || (ys == 150)) {
                    buffer[index] = 0;
                }
                else if ((r >= 106.7731239) && (r <= 219.91020894)  && (thetaAbs <= tMax)) {
                    buffer[index] = (byte)255;
                } // else if ((r >= 106.7731239) && (r <= 219.91020894) && (thetaAbs <= tMax))
            } // for (x = 100; x <= 411; x++)
        } // for (y = 36; y <= 180; y++)
        if ((srcImage.getExtents()[0] != extents[0]) || (srcImage.getExtents()[1] != extents[1])) {
            srcImage.changeExtents(extents);
            srcImage.recomputeDataSize();
        }
        srcImage.getParentFrame().dispose();
        srcImage.getVOIs().removeAllElements();
        try {
            srcImage.importData(0, buffer, true);
        }
        catch(IOException e) {
            MipavUtil.displayError("IOException on srcImage.importData");
        }
        if ((testOrientation == testTop) || (testOrientation == testBottom)) {
            destExtents[0] = destImage.getExtents()[0];
            destExtents[1] = (int)Math.round(destExtents[0] * Math.log(219.91020894/106.7731239)/(2.0 * tMax));
            if (destImage.getExtents()[1] != destExtents[1]) {
                destImage.changeExtents(destExtents);
                destImage.recomputeDataSize();
            }
        } // if ((testOrientation == testTop) || (testOrientation == testBottom))
        else {
            destExtents[1] = destImage.getExtents()[1];
            destExtents[0] = (int)Math.round(destExtents[1] * Math.log(219.91020894/106.7731239)/(2.0 * tMax));
            if (destImage.getExtents()[0] != destExtents[0]) {
                destImage.changeExtents(destExtents);
                destImage.recomputeDataSize();
            }    
        }
        
        x = new double[4];
        y = new double[4];
        x[0] = 411.0;
        x[1] = 100.0;
        x[2] = 180.0;
        x[3] = 331.0;
        y[0] = 100.0;
        y[1] = 100.0;
        y[2] = 180.0;
        y[3] = 180.0;
        
        if (testOrientation == testBottom) {
            flipx = new AlgorithmFlip(srcImage, AlgorithmFlip.X_AXIS, AlgorithmFlip.IMAGE, true);
            flipx.run();
            flipx.finalize();
            flipx = null;
            // 1 -> 0, 0 -> 1, 2 -> 3, 3 -> 2
            tempX = x[1];
            tempY = extents[1] - 1 - y[1];
            x[1] = x[0];
            y[1] = extents[1] - 1 - y[0];
            x[0] = tempX;
            y[0] = tempY;
            
            tempX = x[3];
            tempY = extents[1] - 1 - y[3];
            x[3] = x[2];
            y[3] = extents[1] - 1 - y[2];
            x[2] = tempX;
            y[2] = tempY;
        } // if (testOrientation == testBottom)
        else if (testOrientation == testRight) {
            xCen = (xDim - 1.0)/2.0;
            yCen = (yDim - 1.0)/2.0;
            for (i = 0; i < 4; i++) {
                x[i] = x[i] - xCen;
                y[i] = y[i] - yCen;
                tempX = -y[i];
                y[i] = x[i];
                x[i] = tempX;
                x[i] = x[i] + xCen;
                y[i] = y[i] + yCen;
            }
            rotateAlgo = new AlgorithmRotate(srcImage, AlgorithmRotate.Z_AXIS_PLUS);
            rotateAlgo.run();
            srcImage.disposeLocal();
            srcImage = null;
            srcImage = rotateAlgo.returnImage();
            rotateAlgo.finalize();
            rotateAlgo = null;  
        } // else if (testOrientation == testRight)
        else if (testOrientation == testLeft) {
            xCen = (xDim - 1.0)/2.0;
            yCen = (yDim - 1.0)/2.0;
            for (i = 0; i < 4; i++) {
                x[i] = x[i] - xCen;
                y[i] = y[i] - yCen;
                tempX = y[i];
                y[i] = -x[i];
                x[i] = tempX;
                x[i] = x[i] + xCen;
                y[i] = y[i] + yCen;
            }
            rotateAlgo = new AlgorithmRotate(srcImage, AlgorithmRotate.Z_AXIS_MINUS);
            rotateAlgo.run();
            srcImage.disposeLocal();
            srcImage = null;
            srcImage = rotateAlgo.returnImage();
            rotateAlgo.finalize();
            rotateAlgo = null;      
        } // else if (testOrientation == testLeft)
        
        new ViewJFrameImage(srcImage);    
    }
}
