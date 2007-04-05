package gov.nih.mipav.model.algorithms;


import gov.nih.mipav.model.structures.*;

import gov.nih.mipav.view.*;

import java.io.*;
import java.util.*;

/**
 
 * <p>References: 1.) Advanced Calculus For Applications Second Edition by F. B. Hildebrand, Section 10.4 Analytic
 * Functions of a Complex Variable pages 550-554 and Section 11.4 Conformal Mapping pages 628-632, Prentice-Hall, Inc.,
 * 1976. 
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
        double x2, y2;
        
        double radius;
        
        

        double xc, yc;
        double sq2;
        double invSq2;
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
        
        EllipticIntegral eInt;
        ViewVOIVector VOIs = null;
        int nVOIs;
        int contourVOIs;
        Vector[] contours = null;
        int nContours = 0;
        Point3Df centerOfMass;
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
        BitSet mask;
        VOI selectedVOI = null;
        double radSq;
        double xrot, yrot;
        double theta;
        double costh, sinth;
        double axisRatio;
        double xi;
        double us;
        double uslow;
        double usmid;
        double ushigh;
        double slow;
        double smid;
        double shigh;
        double s;
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

        if (srcImage == null) {
            displayError("Source Image is null");
            finalize();

            return;
        }

        constructLog();

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
        
        centerOfMass = ((VOIContour)(contours[0].elementAt(0))).getCenterOfMass();
        
        ((VOIContour)(contours[0].elementAt(0))).secondOrderAttributes(xDimSource, yDimSource, xRes, yRes, xUnits, yUnits,
                                                                       pAxis, eccentricity, majorAxis, minorAxis);
        theta = (Math.PI/180.0)*pAxis[0];
        costh = Math.cos(theta);
        sinth = Math.sin(theta);
        mask = new BitSet(sourceSlice);
        selectedVOI.createActiveContourBinaryMask(mask, xDimSource, yDimSource);
        
        // Let 2*a = major axis, 2*b = minor axis
        // For an ellipse with its center at the origin and foci on the x axis:
        //(x*x)/(a*a) + (y*y)/(b*b) = 1
        // The standard ellipse used here will have foci at +-1 and will be of the form:
        // (x*x)/((cosh(xi)**2) + (y*y)/((sinh(xi)**2) = 1
        // So to scale from the original ellipse to the standard ellipse use:
        // tanh(xi) = sinh(xi)/cosh(xi) = b/a = majorAxis/minorAxis = axisRatio
        // xi = arg tanh(axisRatio)
        // xi = 0.5 * log((1 + axisRatio)/(1 - axisRatio))
        axisRatio = minorAxis[0]/majorAxis[0];
        xi = 0.5 * Math.log((1.0 + axisRatio)/(1.0 - axisRatio));
        ellipseRatio = (majorAxis[0]/2.0)/((Math.exp(xi) + Math.exp(-xi))/2.0);
        // To conformal map from the unit disc to the standard ellipse use the transform
        // sin[(PI/(2*K(s)))*F(z/sqrt(s), s)
        // where K(s) is the complete elliptic integral of the first kind and
        // F(z/sqrt(s), s) is the incomplete or normal elliptic integral of the first kind with:
        // F(z, s) = integral from 0 to z of dx/sqrt((1 - x*x)*(1 - s*s*x*x))
        // But the EllipticIntegral functions in MIPAV all use the form:
        // F(k, phi) = integral from 0 to phi of d(theta)/sqrt(1 - k*k*sin(theta)*sin(theta))
        // so x in the first form in sin(phi) or phi = arcsin(x)
        // For complex z arc sin(z) = -i*log(i*z + sqrt(1 - z*z))
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
        UI.setDataText("smid = " + smid + "\n");

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
        // Actually the image shoudl be a square
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
                    // Rotate around the circle by -theta
                    xp = i - xc;
                    yp = j - yc;
                    xrot = xp * costh + yp * sinth;
                    yrot = -xp * sinth + yp * costh;
                    // Use conformal map from unit circle to ellipse on the xaxis with foci
                    // at +- 1.
                    xrot = xrot/(radius*sqrtS);
                    yrot = yrot/(radius*sqrtS);
                    // Need arc sin (xrot, yrot) = -i*log(i*z + sqrt(1 - z*z))
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
                    // Rotate to tilt of original ellipse
                    xp = firstr[0] * costh - firsti[0] * sinth;
                    yp = firstr[0] * sinth + firsti[0] * costh;
                    // Scale to size of original ellipse
                    xp = xp * ellipseRatio;
                    yp = yp * ellipseRatio;
                    // Translate to center of original ellipse
                    xSrc = centerOfMass.x + xp;
                    ySrc = centerOfMass.y + yp;
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

    
    /**
     * Constructs a string of the contruction parameters and out puts the string to the messsage frame if the history
     * logging procedure is turned on.
     */
    private void constructLog() {


        historyString = new String("EllipseToCirlce(" + String.valueOf(destImage.getExtents()[0]) + ", " +
                                   String.valueOf(destImage.getExtents()[1]) + ")\n");
    }
}
