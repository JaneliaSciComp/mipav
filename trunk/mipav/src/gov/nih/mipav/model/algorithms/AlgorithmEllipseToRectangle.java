package gov.nih.mipav.model.algorithms;

import WildMagic.LibFoundation.Mathematics.Vector3f;

import gov.nih.mipav.model.structures.*;

import gov.nih.mipav.view.*;

import java.awt.Color;
import java.io.*;
import java.util.*;

/**
 * This program performs the conformal mapping of an ellipse to a rectangle.
 * Consider the original ellipse as having a tilt theta with the x axis.
 * From problem 9 in Nehari:
 * w = sin z maps the rectangle -pi/2 < Re |z| < pi/2, -c < Im |z| < c onto the interior
 * of the ellipse u**2/(cosh c)**2 + v**2/(sinh c)**2 = 1
 * which has been cut along the linear segments -cosh c <= w <= -1, 1 <= w <= cosh c
 * Then the conformal mapping is done in 3 steps.
 * 1.) Translate the center of the rectangular image to the origin, 
 * and divide down the rectangle size by ((xDim - 1)/3.1).
 * 2.) Map from the rectangle to a standard ellipse on the x axis with foci at +- 1 and
 * with the same major axis/minor axis ratio as the original ellipse.
 * 3.) Map from the standard ellipse to the original ellipse by rotating, scaling, and translating.
 * <p>References: 1.) Advanced Calculus For Applications Second Edition by F. B. Hildebrand, Section 10.4 Analytic
 * Functions of a Complex Variable pages 550-554 and Section 11.4 Conformal Mapping pages 628-632, Prentice-Hall, Inc.,
 * 1976. 
 * 2.)  Conformal Mapping from Zeev Nehari, Chapter VI, Mapping Properties of Special Functions, Section 2, 
 * Exponential and Trigonometric Functions, pp. 273-280.  See Porblem 9 on page 279.
 */
public class AlgorithmEllipseToRectangle extends AlgorithmBase {

    //~ Instance fields ------------------------------------------------------------------------------------------------
	private int xDimDest;
	
	private ModelImage resultImage;
	
    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * AlgorithmEllipseToRectangle - default constructor.
     */
    public AlgorithmEllipseToRectangle() { }

    /**
     * AlgorithmEllipseToRectangle.
     *
     * @param  srcImg   DOCUMENT ME!
     * @param  xDimDest
     */
    public AlgorithmEllipseToRectangle(ModelImage srcImg, int xDimDest) {
        super(null, srcImg);
        this.xDimDest = xDimDest;
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

        double xc, yc;

        int xDimSource;

        int yDimSource;

        int sourceSlice;

        int i, j;
        int index, index1;

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
        Vector contours = null;
        int nContours = 0;
        Vector3f geometricCenter;
        // Angle in degrees with the major axis
        float pAxis[] = new float[1];
        // shape (circle = 0, line = 1)
        float eccentricity[] = new float[1];
        // Diameter of major axis
        float majorAxis[] = new float[1];
        // Diameter of minor axis
        float minorAxis[] = new float[1];
        VOI selectedVOI = null;
        double theta;
        double costh, sinth;
        double axisRatio;
        double c;
        double ellipseRatio;
        double scaleFactor;
        boolean test = false;
        double coshy;
        double sinhy;
        double cosx;
        double sinx;
        double u;
        double v;
        String name;
        int extents[] = new int[2];
        double boundaryDistance;
        VOIContour ellipseContour;
        boolean snear[] = new boolean[1];
        int firstIndex[] = new int[1];
        int secondIndex[] = new int[1];
        int outerPolarity = -1;
        
        if (test) {
            selfTest();
        }

        if (srcImage == null) {
            displayError("Source Image is null");
            finalize();

            return;
        }

        fireProgressStateChanged(srcImage.getImageName(), "Ellipse to rectangle ...");

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
                nContours = contours.size();
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
        
        ellipseContour = (VOIContour)selectedVOI.getCurves().elementAt(0);
        geometricCenter = ((VOIContour)(contours.elementAt(0))).getGeometricCenter();
        boundaryDistance = ellipseContour.pinpol(geometricCenter.X, geometricCenter.Y, snear, firstIndex, secondIndex);
        if (boundaryDistance > 0.0) {
        	outerPolarity = -1;
        }
        Preferences.debug("X center = " + geometricCenter.X + "\n");
        Preferences.debug("Y center = " + geometricCenter.Y + "\n");
        
        //((VOIContour)(contours.elementAt(0))).secondOrderAttributes(xDimSource, yDimSource, xRes, yRes, xUnits, yUnits,
        //        pAxis, eccentricity, majorAxis, minorAxis);
        ((VOIContour)(contours.elementAt(0))).secondOrderAttributes(srcImage,
                pAxis, eccentricity, majorAxis, minorAxis);
        majorAxis[0] = majorAxis[0]/srcImage.getFileInfo(0).getResolutions()[0];
        minorAxis[0] = minorAxis[0]/srcImage.getFileInfo(0).getResolutions()[0];
        Preferences.debug("Major axis angle in degrees = " + pAxis[0] + "\n");
        Preferences.debug("Eccentricity = " + eccentricity[0] + "\n");
        Preferences.debug("Major axis length = " + majorAxis[0] + " pixels " + "\n");
        Preferences.debug("Minor axis length = " + minorAxis[0] + " pixels + " + "\n");
        theta = (Math.PI/180.0)*pAxis[0];
        costh = Math.cos(theta);
        sinth = Math.sin(theta);
        
        // Let 2*a = major axis, 2*b = minor axis
        // For an ellipse with its center at the origin and foci on the x axis:
        //(x*x)/(a*a) + (y*y)/(b*b) = 1
        // The standard ellipse used here will have foci at +-1 and will be of the form:
        // (x*x)/((cosh(c)**2) + (y*y)/((sinh(c)**2) = 1
        // So to scale from the original ellipse to the standard ellipse use:
        // tanh(c) = sinh(c)/cosh(c) = b/a = minorAxis/majorAxis = axisRatio
        // c = arg tanh(axisRatio)
        // c = 0.5 * log((1 + axisRatio)/(1 - axisRatio))
        axisRatio = minorAxis[0]/majorAxis[0];
        c = 0.5 * Math.log((1.0 + axisRatio)/(1.0 - axisRatio));
        ellipseRatio = (majorAxis[0]/2.0)/((Math.exp(c) + Math.exp(-c))/2.0);
        
        name = srcImage.getImageName() +  "_rectangle";
        yDimDest = (int)Math.ceil(axisRatio * xDimDest);
        extents[0] = xDimDest;
        extents[1] = yDimDest;
        resultImage = new ModelImage(srcImage.getType(), extents, name);
        resultImage.setResolutions(srcImage.getFileInfo(0).getResolutions());
        resultImage.setImageName(name);
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
        
        xc = (xDimDest - 1.0)/2.0;
        yc = (yDimDest - 1.0)/2.0;
        // Scale xc down to < PI/2.  Scale xc down to (3.1/2.0)
        scaleFactor = xc/(3.1/2.0);

        if (!srcImage.isColorImage()) {
            imageMin = (float) srcImage.getMin();

            for (i = 0; i < destSlice; i++) {
                destBuffer[i] = imageMin;
            }
        } // if (!srcImage.isColorImage())

        
        for (j = 0; j < yDimDest; j++) {
            fireProgressStateChanged(100 * j / yDimDest);
            yp = (j - yc)/scaleFactor;
            coshy = Math.cosh(yp);
            sinhy = Math.sinh(yp);
            index1 = j * xDimDest;
            for (i = 0; i < xDimDest; i++) {
                xp = (i - xc)/scaleFactor;
                cosx = Math.cos(xp);
                sinx = Math.sin(xp);
                // w = sin z
                // = sin(x + iy) = sin(x)*cos(iy) + cos(x)sin(iy)
                // = sin(x)cosh(y) + i*cos(x)sinh(y)
                // u = sin(x)cosh(y) v = cos(x)sinh(y)
                u = sinx * coshy;
                v = cosx * sinhy;
                // Now map from the standard ellipse on the xaxis with foci at +- 1
                // to the original drawn ellipse
                // Rotate to angle theta tilt of original ellipse
                xp = u * costh - v * sinth;
                yp = u * sinth + v * costh;
                // Scale to size of original ellipse
                xp = xp * ellipseRatio;
                yp = yp * ellipseRatio;
                // Translate to center of original ellipse
                xSrc = geometricCenter.X + xp;
                ySrc = geometricCenter.Y + yp;
                // Use bilinear interpolation to find the contributions from the
                // 4 nearest neighbors in the original ellipse space
                if ((xSrc >= 0.0) && ((xSrc) <= (xDimSource - 1)) && (ySrc >= 0.0) && (ySrc <= (yDimSource - 1))) {
                	boundaryDistance = ellipseContour.pinpol(xSrc, ySrc, snear, firstIndex, secondIndex) * outerPolarity;
                	if (boundaryDistance <= 0.0) {
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
                	} // if (boundaryDistance <= 0.0)
                }
            } // for (i = 0; i < xDimDest; i++)
        } // for (j = 0; j < yDimDest; j++)

        try {
            resultImage.importData(0, destBuffer, true);
        } catch (IOException e) {
            MipavUtil.displayError("IOException " + e + " on resultImage.importData");

            setCompleted(false);

            return;
        }

        setCompleted(true);

        return;
    }
    
    
    public ModelImage getResultImage() {
    	return resultImage;
    }
    
    
    private void selfTest() {
        // Lines have perpindicular intersections in ellipse
        // Intersections should remain perpindicular in rectangular
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
                VOI.CONTOUR, -1.0f);
        newEllipseVOI.setColor(Color.blue);
        for (j = 0; j < maxEllipsePoints; j++) {
            theta = j * 2.0 * Math.PI/maxEllipsePoints; 
            xArr[j] = (float)(xCen + a * Math.cos(theta));
            yArr[j] = (float)(yCen + b * Math.sin(theta));
            zArr[j] = 0.0f;
        }
        newEllipseVOI.importCurve(xArr, yArr, zArr);
        ((VOIContour)(newEllipseVOI.getCurves().elementAt(0))).setFixed(true);
        newEllipseVOI.setActive(false);
        ((VOIContour)(newEllipseVOI.getCurves().elementAt(0))).setActive(false);
        ((VOIContour)(newEllipseVOI.getCurves().elementAt(0))).setClosed(true);
        ((VOIContour) (newEllipseVOI.getCurves().elementAt(0))).setLabel(Integer.toString(1));
        srcImage.registerVOI(newEllipseVOI);
        if (phi != 0.0) {
            xfrm = new TransMatrix(3);
            xfrm.setRotate(phi);
            interp = AlgorithmTransform.BILINEAR;
            oXres = srcImage.getFileInfo()[0].getResolutions()[0];
            oYres = srcImage.getFileInfo()[0].getResolutions()[1];
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
        
         ViewJFrameImage newFrame = new ViewJFrameImage(srcImage);
         newFrame.updateImages(true);
    } // private void selfTest()
}
