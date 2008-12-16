package gov.nih.mipav.model.algorithms;

import WildMagic.LibFoundation.Mathematics.Vector3f;

import gov.nih.mipav.model.structures.*;

import gov.nih.mipav.view.*;

import java.awt.Color;
import java.io.*;
import java.util.*;

/**
 * This program performs the conformal mapping of an ellipse to a circle.
 * Consider the original ellipse as having a tilt theta with the x axis.
 * Then the conformal mapping is done in 3 steps.
 * 1.) Translate the circle from the center of the square image to the origin, 
 * rotate around the circle by -theta and convert to a unit disc by dividing the distance
 * from the center by the radius.
 * 2.) Map from the unit circle to a standard ellipse on the x axis with foci at +- 1 and
 * with the same major axis/minor axis ratio as the original ellipse.
 * 3.) Map from the standard ellipse to the original ellipse by rotating, scaling, and translating.
 * <p>References: 1.) Advanced Calculus For Applications Second Edition by F. B. Hildebrand, Section 10.4 Analytic
 * Functions of a Complex Variable pages 550-554 and Section 11.4 Conformal Mapping pages 628-632, Prentice-Hall, Inc.,
 * 1976. 
 * 2.) "On Conformal Representations of the Interior of an Ellipse" by Stanislawa Kanas and Toshiyuki Sugawa, 
 * Annales Academiae Scientiarum Fennicae Mathematica, Volume 31, 2006, pp. 329-348.
 * 3.) http://mathworld.com/InverseSine.html
 */
public class AlgorithmEllipseToCircle extends AlgorithmBase {

    //~ Instance fields ------------------------------------------------------------------------------------------------

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * AlgorithmEllipseToCircle - default constructor.
     */
    public AlgorithmEllipseToCircle() { }

    /**
     * AlgorithmEllipseToCircle.
     *
     * @param  destImg  DOCUMENT ME!
     * @param  srcImg   DOCUMENT ME!
     */
    public AlgorithmEllipseToCircle(ModelImage destImg, ModelImage srcImg) {
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
        
        EllipticIntegral eInt;
        ViewVOIVector VOIs = null;
        int nVOIs;
        int contourVOIs;
        Vector[] contours = null;
        int nContours = 0;
        Vector3f geometricCenter;
        // Use 1.0 for resolution regardless of actual units
        float xRes = 1.0f;
        float yRes = 1.0f;
        int xUnits = srcImage.getUnitsOfMeasure()[0];
        int yUnits = srcImage.getUnitsOfMeasure()[1];
        // Angle in degrees with the major axis
        float pAxis[] = new float[1];
        // shape (circle = 0, line = 1)
        float eccentricity[] = new float[1];
        // Diameter of major axis
        float majorAxis[] = new float[1];
        // Diameter of minor axis
        float minorAxis[] = new float[1];
        VOI selectedVOI = null;
        double radSq;
        double xrot, yrot;
        double theta;
        double costh, sinth;
        double axisRatio;
        double xi;
        double us;
        double usmid;
        double slow;
        double smid;
        double shigh;
        double eps;
        double knum;
        double kdenom;
        double first[] = new double[1];
        double second[] = new double[1];
        double pihf = 0.5 * Math.PI;
        double coef;
        double sqrtS;
        double prodr[] = new double[1];
        double prodi[] = new double[1];
        double rootr[] = new double[1];
        double rooti[] = new double[1];
        int error[] = new int[1];
        double logr[] = new double[1];
        double logi[] = new double[1];
        boolean complete = false;
        double modr;
        double modi = 0.0;
        boolean complementaryModulusUsed = false;
        boolean analyticContinuationUsed = false;
        double phir;
        double phii;
        double firstr[] = new double[1];
        double firsti[] = new double[1];
        double secondr[] = new double[1];
        double secondi[] = new double[1];
        boolean useStandardMethod = true;
        double ellipseRatio;
        ViewUserInterface UI = ViewUserInterface.getReference();
        int numErrors = 0;
        double sinr[] = new double[1];
        double sini[] = new double[1];
        boolean test = false;
        
        if (test) {
            selfTest();
        }

        if (srcImage == null) {
            displayError("Source Image is null");
            finalize();

            return;
        }

        fireProgressStateChanged(srcImage.getImageName(), "Ellipse to circle ...");

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
        Preferences.debug("X center = " + geometricCenter.X + "\n");
        Preferences.debug("Y center = " + geometricCenter.Y + "\n");
        
        ((VOIContour)(contours[0].elementAt(0))).secondOrderAttributes(xDimSource, yDimSource, xRes, yRes, xUnits, yUnits,
                                                                       pAxis, eccentricity, majorAxis, minorAxis);
        Preferences.debug("Major axis angle in degrees = " + pAxis[0] + "\n");
        Preferences.debug("Eccentricity = " + eccentricity[0] + "\n");
        Preferences.debug("Major axis length = " + majorAxis[0] + "\n");
        Preferences.debug("Minor axis length = " + minorAxis[0] + "\n");
        theta = (Math.PI/180.0)*pAxis[0];
        costh = Math.cos(theta);
        sinth = Math.sin(theta);
        
        // Let 2*a = major axis, 2*b = minor axis
        // For an ellipse with its center at the origin and foci on the x axis:
        //(x*x)/(a*a) + (y*y)/(b*b) = 1
        // The standard ellipse used here will have foci at +-1 and will be of the form:
        // (x*x)/((cosh(xi)**2) + (y*y)/((sinh(xi)**2) = 1
        // So to scale from the original ellipse to the standard ellipse use:
        // tanh(xi) = sinh(xi)/cosh(xi) = b/a = minorAxis/majorAxis = axisRatio
        // xi = arg tanh(axisRatio)
        // xi = 0.5 * log((1 + axisRatio)/(1 - axisRatio))
        axisRatio = minorAxis[0]/majorAxis[0];
        xi = 0.5 * Math.log((1.0 + axisRatio)/(1.0 - axisRatio));
        ellipseRatio = (majorAxis[0]/2.0)/((Math.exp(xi) + Math.exp(-xi))/2.0);
        // To conformal map from the unit disc to the standard ellipse use the transform
        // sin[(PI/(2*K(s)))*F(z/sqrt(s), s)]
        // where K(s) is the complete elliptic integral of the first kind and
        // F(z/sqrt(s), s) is the incomplete or normal elliptic integral of the first kind with:
        // F(z, s) = integral from 0 to z of dx/sqrt((1 - x*x)*(1 - s*s*x*x))
        // But the EllipticIntegral functions in MIPAV all use the form:
        // F(k, phi) = integral from 0 to phi of d(theta)/sqrt(1 - k*k*sin(theta)*sin(theta))
        // So x in the elliptical integral form used here equals sin(phi) in the elliptical integral
        // form used in MIPAV or phi = arcsin(x)
        // For complex z arc sin(z) = -i*log(i*z + sqrt(1 - z*z))
        // Now how to find s:
        // 0 <= s <= 1, where u(s) = 2*xi
        // u(s) =(PI/2) * K(sqrt(1 - s*s))/K(s)
        // where u(s) decreases from infinity to 0 as s moves from 0 to 1
        us = 2.0 * xi;
        slow = 0.001;
        shigh = 0.999;
        smid = (slow + shigh)/2.0;
        while (true) {
            eInt = new EllipticIntegral(Math.sqrt(1.0 - smid*smid), first, second); 
            eInt.run();
            knum = first[0];
            eInt = new EllipticIntegral(smid, first, second);
            eInt.run();
            kdenom = first[0];
            usmid = pihf * knum/kdenom;
            eps = Math.abs((usmid - us)/us);
            if (eps < 1.0E-8) {
                break;
            }
            if (usmid < us) {
                shigh = smid;
            }
            else {
                slow = smid;
            }
            smid = (slow + shigh)/2.0;
        }
        coef = pihf/kdenom;
        sqrtS= Math.sqrt(smid);
        Preferences.debug("smid = " + smid + "\n");

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
                if (((j - yc)*(j - yc) + (i - xc)*(i - xc)) <= radSq) {   
                    // Translate circle to origin
                    xp = i - xc;
                    yp = j - yc;
                    // Rotate around the circle by -theta
                    xrot = xp * costh + yp * sinth;
                    yrot = -xp * sinth + yp * costh;
                    // xrot = xrot/radius and yrot = yrot/radius scales the circle to a unit disc
                    // Use conformal map from unit circle to ellipse on the xaxis with foci
                    // at +- 1.
                    xrot = xrot/(radius*sqrtS);
                    yrot = yrot/(radius*sqrtS);
                    // Need arc sin (z) = -i*log(i*z + sqrt(1 - z*z)), where z = xrot + i * yrot
                    zmlt(xrot, yrot, xrot, yrot, prodr, prodi);
                    sqrtc(1.0 - prodr[0], -prodi[0], rootr, rooti);
                    zmlt(0.0, 1.0, xrot, yrot, prodr, prodi);
                    zlog(prodr[0] + rootr[0], prodi[0] + rooti[0], logr, logi, error);
                    zmlt(0.0, -1.0, logr[0], logi[0], prodr, prodi);
                    modr = smid;
                    phir = prodr[0];
                    phii = prodi[0];
                    error[0] = 0;
                    eInt = new EllipticIntegral(complete, modr, modi, complementaryModulusUsed, analyticContinuationUsed,
                               phir, phii, firstr, firsti, secondr, secondi, useStandardMethod, error);
                    eInt.run();
                    if (error[0] == 1) {
                        numErrors++;
                    }
                    // Multiply by PI/(2*K(s)) = coef;
                    firstr[0] = coef * firstr[0];
                    firsti[0] = coef * firsti[0];
                    zsin(firstr[0], firsti[0], sinr, sini);
                    // Now map from the standard ellipse on the xaxis with foci at +- 1
                    // to the original drawn ellipse
                    // Rotate to angle theta tilt of original ellipse
                    xp = sinr[0] * costh - sini[0] * sinth;
                    yp = sinr[0] * sinth + sini[0] * costh;
                    // Scale to size of original ellipse
                    xp = xp * ellipseRatio;
                    yp = yp * ellipseRatio;
                    // Translate to center of original ellipse
                    xSrc = geometricCenter.X + xp;
                    ySrc = geometricCenter.Y + yp;
                    // Use bilinear interpolation to find the contributions from the
                    // 4 nearest neighbors in the original ellipse space
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
        
        UI.setDataText(numErrors + " errors running ellipticIntegral\n");

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
    
    //  Performs square root of complex argument
    private void sqrtc(double zinr, double zini, double zsqr[], double zsqi[]) {
       double a;
       double th;
       
       a = zabs(zinr, zini);
       a = Math.sqrt(a);
       th = Math.atan2(zini, zinr);
       th *= 0.5;
       zsqr[0] = a * Math.cos(th);
       zsqi[0] = a * Math.sin(th);
       return;
    } // private void sqrtc
    
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
     * complex logarithm b = clog(a).
     *
     * @param  ar    double
     * @param  ai    double
     * @param  br    double[]
     * @param  bi    double[]
     * @param  ierr  int[] ierr = 0, normal return ierr = 1, z = cmplx(0.0, 0.0)
     */
    private void zlog(double ar, double ai, double[] br, double[] bi, int[] ierr) {
        double theta;
        double zm;
        ierr[0] = 0;

        if (ar == 0.0) {

            if (ai == 0.0) {
                ierr[0] = 1;

                return;
            } // if (ai == 0.0)
            else {

                if (ai > 0.0) {
                    bi[0] = Math.PI / 2.0;
                } else {
                    bi[0] = -Math.PI / 2.0;
                }

                br[0] = Math.log(Math.abs(ai));

                return;
            }
        } // if (ar == 0.0)
        else if (ai == 0.0) {

            if (ar > 0.0) {
                br[0] = Math.log(ar);
                bi[0] = 0.0;

                return;
            } else {
                br[0] = Math.log(Math.abs(ar));
                bi[0] = Math.PI;

                return;
            }
        } // else if (ai == 0.0)

        theta = Math.atan(ai / ar);

        if ((theta <= 0.0) && (ar < 0.0)) {
            theta = theta + Math.PI;
        } else if (ar < 0.0) {
            theta = theta - Math.PI;
        }

        zm = zabs(ar, ai);
        br[0] = Math.log(zm);
        bi[0] = theta;

        return;
    }
    
    private void zsin(double inr, double ini, double outr[], double outi[]) {
        outr[0] = Math.sin(inr)*cosh(ini);
        outi[0] = Math.cos(inr)*sinh(ini);
        return;
    } // private void zsin
    
    private double cosh(double x) {
        double var;
        var = (Math.exp(x) + Math.exp(-x))/2.0;
        return var;
    }
    
    private double sinh(double x) {
        double var;
        var = (Math.exp(x) - Math.exp(-x))/2.0;
        return var;
    }
    
    private void selfTest() {
        // Lines have perpindicular intersections in ellipse
        // Intersections should remain perpindicular in circle
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
        int xCen = 255;
        int yCen = 255;
        VOI newEllipseVOI;
        int maxEllipsePoints;
        // semimajor axis a
        double a = 200;
        // semiminor axis b
        double b = 100;
        // eccentricity
        double e = Math.sqrt(a*a - b*b)/a;
        // The circumference c of an ellipse is 4*a*E(e), where the function E is the complete elliptic integral of the second kind.
        EllipticIntegral ei;
        double first[] = new double[1];
        double second[] = new double[1];
        ei = new EllipticIntegral(e, first, second);
        ei.run();
        maxEllipsePoints = (int)Math.ceil(4.0 * a * second[0]);
        float xArr[] = new float[maxEllipsePoints];
        float yArr[] = new float[maxEllipsePoints];
        float zArr[] = new float[maxEllipsePoints];
        int j;
        double theta;
        // Angle ellipse is rotated by
        double phi = -30.0;
        AlgorithmTransform algoTrans;
        TransMatrix xfrm;
        int interp;
        float oXres;
        float oYres;
        int oXdim;
        int oYdim;
        int units[];
        boolean doVOI;
        boolean doClip;
        boolean doPad;
        boolean doRotateCenter;
        Vector3f center;
        boolean useSACenter = false;
        float fillValue = 0.0f;
        boolean doUpdateOrigin = false;
        ModelImage resultImage;
        for (ys = 155; ys <= 355; ys++) {
            for (xs = 55; xs <= 455; xs++) {
                index = xs + ys * xDim;
                xDist = xs - xCen;
                yDist = ys - yCen;
                r = Math.sqrt(xDist*xDist + 4.0*yDist*yDist);
                if ((xs == 155) || (xs == 205) || (xs == 255) || (xs == 305) || (xs == 355) ||
                    (ys == 205) || (ys == 230) || (ys == 255) || (ys == 280) || (ys == 305)) {
                    buffer[index] = 0;
                }
                else if (r <= 200.0) {
                    buffer[index] = (byte)255;
                }
            } // for (xs = 55; xs <= 455; xs++)
        } // for (ys = 155; ys <= 355; ys++)
        if ((srcImage.getExtents()[0] != extents[0]) || (srcImage.getExtents()[1] != extents[1])) {
            srcImage.changeExtents(extents);
            srcImage.recomputeDataSize();
        }
        srcImage.getParentFrame().dispose();
        srcImage.getVOIs().removeAllElements();
        try {
            srcImage.importData(0, buffer, true);
        }
        catch(IOException ex) {
            MipavUtil.displayError("IOException on srcImage.importData");
        }
        
        newEllipseVOI = new VOI((short) (1), Integer.toString(1),
                1, VOI.CONTOUR, -1.0f);
        newEllipseVOI.setColor(Color.blue);
        for (j = 0; j < maxEllipsePoints; j++) {
            theta = j * 2.0 * Math.PI/maxEllipsePoints; 
            xArr[j] = (float)(xCen + a * Math.cos(theta));
            yArr[j] = (float)(yCen + b * Math.sin(theta));
            zArr[j] = 0.0f;
        }
        newEllipseVOI.importCurve(xArr, yArr, zArr, 0);
        ((VOIContour)(newEllipseVOI.getCurves()[0].elementAt(0))).setFixed(true);
        newEllipseVOI.setActive(false);
        ((VOIContour)(newEllipseVOI.getCurves()[0].elementAt(0))).setActive(false);
        ((VOIContour)(newEllipseVOI.getCurves()[0].elementAt(0))).setClosed(true);
        ((VOIContour) (newEllipseVOI.getCurves()[0].elementAt(0))).setLabel(Integer.toString(1));
        ((VOIContour) (newEllipseVOI.getCurves()[0].elementAt(0))).setName(Integer.toString(1));
        srcImage.registerVOI(newEllipseVOI);
        if (phi != 0.0) {
            xfrm = new TransMatrix(3);
            xfrm.setRotate(phi);
            interp = AlgorithmTransform.BILINEAR;
            oXres = 1.0f;
            oYres = 1.0f;
            oXdim = srcImage.getExtents()[0];
            oYdim = srcImage.getExtents()[1];
            units = srcImage.getFileInfo()[0].getUnitsOfMeasure();
            doVOI = true;
            doClip = true;
            doPad = false;
            doRotateCenter = true;
            center = srcImage.getImageCentermm(useSACenter);
            algoTrans = new AlgorithmTransform(srcImage, xfrm, interp, oXres, oYres, oXdim, oYdim,
                                               units, doVOI, doClip, doPad, doRotateCenter, center);
            algoTrans.setFillValue(fillValue);
            algoTrans.setUpdateOriginFlag(doUpdateOrigin);
            algoTrans.run();
            resultImage = algoTrans.getTransformedImage();
            algoTrans.finalize();
            algoTrans = null;
            srcImage.disposeLocal();
            srcImage = null;
            srcImage = resultImage;
        } // if (phi != 0.0)
        
        new ViewJFrameImage(srcImage);
    } // private void selfTest()
}
