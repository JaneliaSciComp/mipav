package gov.nih.mipav.model.algorithms;


import gov.nih.mipav.model.structures.*;

import gov.nih.mipav.view.*;

import java.io.*;
import java.util.*;
import java.awt.*;

/**
 * This program performs the conformal mapping of a nearly circular region to a circle.
 
 * The conformal mapping is done in 3 steps:
 * 1.) Translate the circle from the center of the square image to the origin 
 * and convert to a unit disc by dividing the distance from the center by the radius.
 * 2.) Map from the unit circle to a unit nearly circular region at the origin.
 * 3.) Map from the unit nearly circlular region at the origin to the original nearly circular region by
 *     scaling and translating.
 * <p>References: 1.) Advanced Calculus For Applications Second Edition by F. B. Hildebrand, Section 10.4 Analytic
 * Functions of a Complex Variable pages 550-554 and Section 11.4 Conformal Mapping pages 628-632, Prentice-Hall, Inc.,
 * 1976. 
 * 2.) Conformal Mapping by Zeev Nehari, Dover Publications, Inc., 1952, Chapter V, Conformal Mapping of
 * Simply Connected Domains, Section 11, Conformal Mapping of Nearly Circular Domains, pp. 263 - 265.
 * 3.) "On the Conformal Mapping of Nearly-Circular Domains" by Zeev Nehari and Vikramaditya Singh,
 * Proceedings of the American Mathematical Society, 1956, pp. 370-378.
 * 
 * According to the Nehari book:
 * The function:
 * F(z) = z + ((eps*z)/(2*PI))*Integral from 0 to 2*PI of ((exp(i*theta) + z)/(exp(i*theta) - z))*p(theta)d(theta)
 * maps abs(z) < 1 onto the nearly circular domain whose boundary has the polar equation r = 1 + eps*p(theta),
 * where p(theta) is bounded and piecewise continuous and eps is a small positive parameter.
 * The Nehari article further assumes:
 * p(theta) > 0.
 * 
 * Multiply both the numerator and the denominator of the integral by (exp(i*theta) - z)* = (exp(-i*theta) - z*)
 * to create a real denominator
 * denom = 1 - 2xcos(theta) - 2ysin(theta) + x**2 + y**2
 * The real part of the integral equals:
 * Integral from 0 to 2*PI of ((1 - x**2 - y**2)/denom)p(theta)d(theta)
 * The imaginary part of the integral equals:
 * Integral from 0 to 2*PI of ((-2xsin(theta) + 2ycos(theta))/denom)p(theta)d(theta)
 * 0 to 2*PI direction would be counterclockwise.  Suppose curve is clockwise, then the theta increments are 
 * negative.  So both a counterclockwise integral and the negative of the clockwise integral are
 * realized by taking the absolute value of the theta angle increment.
 * 
 */
public class AlgorithmNearlyCircleToCircle extends AlgorithmBase {

    //~ Instance fields ------------------------------------------------------------------------------------------------

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * AlgorithmNearlyCircleToCircle - default constructor.
     */
    public AlgorithmNearlyCircleToCircle() { }

    /**
     * AlgorithmNearlyCircleToCircle.
     *
     * @param  destImg  DOCUMENT ME!
     * @param  srcImg   DOCUMENT ME!
     */
    public AlgorithmNearlyCircleToCircle(ModelImage destImg, ModelImage srcImg) {
        super(destImg, srcImg);
    }

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * finalize -
     */
    public void finalize() {
        super.finalize();
    }
    
    /**
     * Starts the program.
     */
    public void runAlgorithm() {
        
        double radius;
        
        

        double xc, yc;

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
        
        ViewVOIVector VOIs = null;
        int nVOIs;
        int contourVOIs;
        Vector[] contours = null;
        int nContours = 0;
        Point3Df geometricCenter;
        VOI selectedVOI = null;
        double radSq;
        double theta[];
        double p[];
        double costh[];
        double sinth[];
        double eps;
        Polygon gon;
        double xcen;
        double ycen;
        double xbound;
        double ybound;
        double xdist;
        double ydist;
        double minDist;
        double maxDist;
        double dist;
        double ratio;
        double stdDist;
        int n;
        double realInt;
        double imagInt;
        double delTheta;
        double startDenom;
        double endDenom;
        double startRealVal;
        double endRealVal;
        double startImagVal;
        double endImagVal;
        double distSqr;
        double xmult[] = new double[1];
        double ymult[] = new double[1];
        double con;

        if (srcImage == null) {
            displayError("Source Image is null");
            finalize();

            return;
        }

        

        fireProgressStateChanged(srcImage.getImageName(), "Nearly circle to circle ...");

        if (srcImage.isColorImage()) {
            cf = 4;
        } else {
            cf = 1;
        }

        xDimSource = srcImage.getExtents()[0];
        yDimSource = srcImage.getExtents()[1];
        sourceSlice = xDimSource * yDimSource;
        
        VOIs = srcImage.getVOIs();
        nVOIs = VOIs.size();
        contourVOIs = 0;
        for (i = 0; i < nVOIs; i++) {
            if (VOIs.VOIAt(i).getCurveType() == VOI.CONTOUR) {
                contourVOIs++;    
            }
        }
        
        if (contourVOIs == 0) {
            MipavUtil.displayError("No contour VOIs found");
            finalize();
            return;
        }
        
        if (contourVOIs > 1) {
            MipavUtil.displayError("More than 1 contour VOI found");
            finalize();
            return;
        }
        
        for (i = 0; i < nVOIs; i++) {
            if (VOIs.VOIAt(i).getCurveType() == VOI.CONTOUR) {
                selectedVOI = VOIs.VOIAt(i);
                contours = selectedVOI.getCurves();
                nContours = contours[0].size();
            }
        }
        
        if (nContours == 0) {
            MipavUtil.displayError("Contour VOI has no curves");
            finalize();
            return;
        }
        
        if (nContours > 1) {
            MipavUtil.displayError("Contour VOI has more than 1 curve");
            finalize();
            return;
        }
        
        geometricCenter = ((VOIContour)(contours[0].elementAt(0))).getGeometricCenter();
        xcen = geometricCenter.x;
        ycen = geometricCenter.y;
        Preferences.debug("X center = " + xcen + "\n");
        Preferences.debug("Y center = " + ycen + "\n");
        gon = ((VOIContour)(contours[0].elementAt(0))).exportPolygon();
        minDist = Double.MAX_VALUE;
        maxDist = -Double.MAX_VALUE;
        for (i = 0; i < gon.npoints; i++) {
            xbound = gon.xpoints[i];
            ybound = gon.ypoints[i];
            xdist = xbound - xcen;
            ydist = ybound - ycen;
            dist = Math.sqrt(xdist*xdist + ydist*ydist);
            if (dist < minDist) {
                minDist = dist;
            }
            if (dist > maxDist) {
                maxDist = dist;
            }
        } // for (i = 0; i < gon.npoints; i++)
        // Want p(theta) > 0 so make 
        // maxDist/stdDist = 1 + eps
        // minDist/stdDist = 1 + 0.1*eps
        // maxDist/minDist = (1 + eps)/(1 + 0.1eps)
        // ratio  = maxDist/minDist
        // ratio + 0.1*eps*ratio = 1 + eps
        // eps(1 - 0.1*ratio) = ratio - 1
        // eps = (ratio - 1)/(1 - 0.1*ratio)
        ratio = maxDist/minDist;
        eps = (ratio - 1.0)/(1 - 0.1*ratio);
        con = eps/(2.0 * Math.PI);
        stdDist = maxDist/(1.0 + eps);
        // dist/stdDist = 1 + eps*p(theta)
        // (dist/stdDist - 1) = eps*p(theta)
        // p(theta) = (dist/stdDist - 1)/eps
        theta = new double[gon.npoints+1];
        p = new double[gon.npoints+1];
        costh = new double[gon.npoints+1];
        sinth = new double[gon.npoints+1];
        for (i = 0; i < gon.npoints; i++) {
            xbound = gon.xpoints[i];
            ybound = gon.ypoints[i];
            xdist = xbound - xcen;
            ydist = ybound - ycen;
            theta[i] = Math.atan(Math.abs(ydist/xdist));
            if (xdist < 0.0) {
                if (ydist < 0.0) {
                    theta[i] = theta[i] + Math.PI;
                }
                else if (ydist == 0.0) {
                    theta[i] = Math.PI;
                }
                else {
                    theta[i] = Math.PI - theta[i];
                }
            } // if (xdist < 0.0)
            else if (ydist < 0.0) {
                theta[i] = 2.0*Math.PI - theta[i];
            }
            dist = Math.sqrt(xdist*xdist + ydist*ydist);
            costh[i] = xdist/dist;
            sinth[i] = ydist/dist;
            p[i] = ((dist/stdDist) - 1.0)/eps;
            if (i == 0) {
                theta[gon.npoints] = theta[0];
                costh[gon.npoints] = costh[0];
                sinth[gon.npoints] = sinth[0];
                p[gon.npoints] = p[0];
            } // if (i == 0)
        } // for (i = 0; i < gon.npoints; i++)

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
        
        // Put center of circle at center of destination image
        // Actually the image should be a square
        xc = (xDimDest - 1.0)/2.0;
        yc = (yDimDest - 1.0)/2.0;
        radius = Math.min(xc, yc);
        radSq = radius * radius;

        if (!srcImage.isColorImage()) {
            imageMin = (float) srcImage.getMin();

            for (i = 0; i < destSlice; i++) {
                destBuffer[i] = imageMin;
            }
        } // if (!srcImage.isColorImage())

        
        for (j = 0; j < yDimDest; j++) {
            fireProgressStateChanged(100 * j / yDimDest);
            index1 = j * xDimDest;
            for (i = 0; i < xDimDest; i++) {
                // Only consider points in the destination circle
                if (((j - yc)*(j - yc) + (i - xc)*(i - xc)) < radSq) {   
                    // Translate circle to origin
                    xp = i - xc;
                    yp = j - yc;
                    // Scale to a unit circle
                    xp = xp/radius;
                    yp = yp/radius; 
                    distSqr = xp*xp + yp*yp;
                    // Map from unit circle to a unit nearly circular region at the origin
                    realInt = 0.0;
                    imagInt = 0.0;
                    delTheta = Math.abs(theta[1] - theta[0]); 
                    if (delTheta > Math.PI) {
                        delTheta = 2.0*Math.PI - delTheta;
                    }
                    startDenom = 1.0 - 2.0*xp*costh[0] - 2.0*yp*sinth[0] + distSqr;
                    endDenom = 1.0 - 2.0*xp*costh[1] - 2.0*yp*sinth[1] + distSqr;
                    // Calculate realInt
                    startRealVal = (1.0 - distSqr)*p[0]/startDenom;
                    endRealVal = (1.0 - distSqr)*p[1]/endDenom;
                    realInt = delTheta * (startRealVal + endRealVal)/2.0;
                    // Calculate imagInt
                    startImagVal = (-2.0*xp*sinth[0] + 2.0*yp*costh[0])*p[0]/startDenom;
                    endImagVal = (-2.0*xp*sinth[1] + 2.0*yp*costh[1])*p[1]/endDenom;
                    imagInt = delTheta * (startImagVal + endImagVal)/2.0;
                    for (n = 1; n < gon.npoints; n++) {
                        delTheta = Math.abs(theta[n+1] - theta[n]);
                        if (delTheta > Math.PI) {
                            delTheta = 2.0*Math.PI - delTheta;
                        }
                        endDenom = 1.0 - 2.0*xp*costh[n+1] - 2.0*yp*sinth[n+1] + distSqr;
                        // Calculate realInt
                        startRealVal = endRealVal;
                        endRealVal = (1.0 - distSqr)*p[n+1]/endDenom;
                        realInt = realInt + delTheta * (startRealVal + endRealVal)/2.0;
                        // Calculate imagInt;
                        startImagVal = endImagVal;
                        endImagVal = (-2.0*xp*sinth[n+1] + 2.0*yp*costh[n+1])*p[n+1]/endDenom;
                        imagInt = imagInt + delTheta *(startImagVal + endImagVal)/2.0;
                    }
                    zmlt(xp*con, yp*con, realInt, imagInt, xmult, ymult);
                    xp = xp + xmult[0];
                    yp = yp + ymult[0];
                    // Scale to size of original nearly circular region
                    xp = xp * stdDist;
                    yp = yp * stdDist;
                    // Translate to center of original nearly circular region
                    xSrc = xcen + xp;
                    ySrc = ycen + yp;
                    // Use bilinear interpolation to find the contributions from the
                    // 4 nearest neighbors in the original nearly circle space
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
                } // if (((j - yc)*(j - yc) + (i - xc)*(i - xc)) <= radSq)
            } // for (i = 0; i < xDimDest; i++)
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
}
