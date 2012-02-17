package gov.nih.mipav.model.algorithms;


import gov.nih.mipav.model.structures.*;

import gov.nih.mipav.view.*;
import gov.nih.mipav.view.dialogs.*;

import java.io.*;
import java.text.DecimalFormat;
import java.awt.Color;

/**
 * [(y - vy)*cos(phi) - (x - vx)*sin(phi)]**2 =
 * 4*p*[(y - vy)*sin(phi) + (x - vx)*cos(phi)]
 * where vx, vy are the coordinates of the parabola vertex
 * p is the distance between the vertex and focus of the parabola
 * 
 *  This Hough transform uses (xi, yi) points in the original image space to generate vx, vy, phi, p points in the Hough
 *  transform.  This Hough transform module only works with binary images.   Before it is used the user must 
 *  compute the gradient of an image and threshold it to obtain a binary image.  Noise removal and thinning should also
 *  be performed, if necessary, before this program is run. 
 *  
 *  The user is asked for the number of vx bins, vy bins, phi bins, the phi constant value for when phi bins == 1,
 *  p bins, pMin value, pMax value, maxBufferSize, and number of parabolas.  The desired size for vxBins is 
 *  min(512, image.getExtents()[0]).  The desired size for vyBins is min(512, image.getExtents()[1]).
 *  The desired size for phi is 360.  The default value for phiConstant is 90 degrees.  The default value for pBins
 *  is Math.min(512, Math.max(image.getExtents()[0], image.getExtents()[1])).  The default value for pMin is 0.1.
 *  The default value for pMax is Math.max(image.getExtents()[0], image.getExtents()[1]).  The default value for
 *  maxBufferSize is 256 megabytes.  The default number of parabolas is 1. The program generates a Hough transform
 *  of the source image using the basic equation [(y - vy)*cos(phi) - (x - vx)*sin(phi)]**2 =
 *  4*p*[(y - vy)*sin(phi) + (x - vx)*cos(phi)]
 *  The program finds the parabolas containing the largest number of points.
 *  The program produces a dialog which allows the user to select which parabolas should be drawn.
 *  
 *  The Hough transform for the entire image is generated a separate time to find each parabola.
 *  For each (xi, yi) point in the original image not having a value of zero, calculate the first dimension value vxArray[j] = 
 *  j * (xDim - 1)/(vxBins - 1), with j = 0 to vxBins - 1.  Calculate the second dimension value vyArray[k] = k * (yDim - 1)/(vyBins - 1),
 *  with k = 0 to vyBins - 1.  calculate the third dimension phiArray[m] = m * 2.0 * PI/phiBins, with m going from 0 to phiBins - 1
 *  Calculate p = [(y - vy)*cos(phi) - (x - vx)*sin(phi)]**2/[4*[(y - vy)*sin(phi) + (x - vx)*cos(phi)]]
 *  p goes from pMin to pMax
 *  pMin + s4 * (pBins - 1) = pMax.
 *  s4 = (pMax - pMin)/(pBins - 1)
 *  n = (p - pMin)*(pBins - 1)/(pMax - pMin).
 *  Only calculate the Hough transform for pMax >= p >= pMin.
 *  
 *  Find the peak point in the vx, vy, phi, p Hough transform space.
 *  Put the values for this peak point in vxArray[c], vyArray[c], phiArray[c], pArray[c], and
 *  countArray[c].
 *  
 *  yf = y - vy
 *  xf = x - vx
 *  [yf*cos(phi)]^2 - 2*yf*sin[phi]*[xf*cos(phi)+ 2*p] + [xf*sin(phi)]^2 -4*p*xf*cos(phi) = 0
 *  
 *  ys = (y - vy)*cos(phi) - (x - vx)*sin(phi)
 *  xs = (y - vy)*sin(phi) + (x - vx)*cos(phi)
 *  ys^2 = 4*p*xs
 *  2*ys*(dys/dxs) = 4p
 *  (dys/dxs) = (2p)/ys = (2p)/sqrt(4pxs) = sqrt(p/xs)
 *  2*ys*[(dy/dx)*cos(phi) - sin(phi)] = 4*p*[(dy/dx)*sin(phi) + cos(phi)]
 *  (dy/dx) = (ys*sin(phi) + 2*p*cos(phi))/(ys*cos(phi) - 2*p*sin(phi))
 *  Using p = (ys^2)/(4*xs)
 *  (dy/dx) = (2*xs*sin(phi) + ys*cos(phi))/(2*xs*cos(phi) - ys*sin(phi))
 *  (dy/dx) = ((x-vx)*cos(phi)*sin(phi) + (y - vy) + (y - vy)*((sin(phi))^2)/((y - vy)*cos(phi)*sin(phi) + (x - vx) + (x - vx)*((cos(phi))^2)
 *  Using sin(2*phi) = 2*sin(phi)*cos(phi), ((cos(phi))^2) = (1/2)*(1 + cos(2*phi)), ((sin(phi)^2) = (1/2)*(1 - cos(2*phi))
 *  (dy/dx) = ((x - vx)*sin(2*phi) + 3*(y - vy)- (y - vy)*cos(2*phi))/((y - vy)*sin(2*phi) + 3*(x - vx)+ (x - vx)*cos(2*phi))
 *  Note that 4 possible values of phi, 2 sets of values 180 degrees apart can be found.
 *  Without slope you can only calculate p.  With slope you can calculate both phi and p.
 *  
 *  d*cos(2*phi) + e*sin(2*phi) + f = 0
 *  -d*cos(2*phi) = e*sin(2*phi) + f
 *  d^2*cos(2*phi)^2 = e^2*sin(2*phi)^2 + 2*e*f*sin(2*phi) + f^2 = 0
 *  d^2 - d^2*sin(2*phi)^2 = e^2*sin(2*phi)^2 + 2*e*f*sin(2*phi) + f^2 = 0
 *  (d^2 + e^2)*sin(2*phi)^2 + 2*e*f*sin(2*phi) + (f^2 - d^2) = 0
 *  sin(2*phi) = (-2*e*f +- sqrt(4*e^2*f^2 - 4*(d^2 + e^2)*(f^2 - d^2)))/(2*(d^2 + e^2))
 *  sin(2*phi) = (-e*f +- d*sqrt(d^2 + e^2 - f^2))/(d^2 + e^2)
 *  cos(2*phi) = -(e*sin(2*phi) + f)/d = (-d*f -+ e*sqrt(d^2 + e^2 - f^2))/(d^2 + e^2)
 *  Note that + for the sin(2*phi) root corresponds to - for the cos(2*phi) root
 *  Real solutions require d^2 + e^2 - f^2 >= 0.
 *  Each solution of 2*phi generates 2 solutions of phi, 180 degrees apart.
 *  Only use the phi within 90 degrees of arctan(y - vy, x - vx).
 *  So only 2 phi are generated.
 *  
 *  If more parabolas are to be found, then zero the houghBuffer and run through the
 *  same Hough transform a second time, but on this second run instead of incrementing
 *  the Hough buffer, zero the values in the source buffer that contributed to the peak
 *  point in the Hough buffer. So on the next run of the Hough transform the source points that
 *  contributed to the Hough peak value just detected will not be present.
 *  
 *  Create a dialog with numParabolasFound vxArray[i], vyArray[i], phiArray[i], pArray[i], and
 *  countArray[i] values, where the user will select a check box to have that parabola drawn.
 *  
 *  References: 1.) Digital Image Processing, Second Edition by Richard C. Gonzalez and Richard E. Woods, Section 10.2.2
 *  Global Processing via the Hough Transform, Prentice-Hall, Inc., 2002, pp. 587-591.
 *  
 *  2.) Form of parabola equation using phi taken from MATLAB routine houghparabola by Clara Isabel Sanchez.
 * 
 */
public class AlgorithmHoughParabola extends AlgorithmBase {

    //~ Instance fields ------------------------------------------------------------------------------------------------
    // Number of dimension vx bins in Hough transform space
    private int vxBins;
    
    // Number of dimension vy bins in Hough transform space
    private int vyBins;
    
    // number of dimension phi bins in Hough transform space
    private int phiBins;
    
    // Value of phi in radians if phiBins = 1
    private double phiConstant;
    
    // Number of dimension p bins in Hough transform space
    private int pBins;
    
    // Minimum p value
    private float pMin;
    
    // Maximum p value
    private float pMax;
    
    // Maximum number of points to take from each side of a point on a curve in determining a tangent
    // If only 1 point is available on each side, simply use avarage of slopes to each of the
    // neigboring points.  
    private int sidePointsForTangent = 3;
    
    /** The maximum Hough transform size in megabytes - default is currently 256 */
    private int maxBufferSize;
    
    // number of parabolas to be found
    private int numParabolas;
    
    private ModelImage testImage;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * AlgorithmHoughParabola - default constructor.
     */
    public AlgorithmHoughParabola() { }

    /**
     * AlgorithmHoughParabola.
     *
     * @param  destImg  Image with lines filled in
     * @param  srcImg   Binary source image that has lines with gaps
     * @param  vxBins   number of dimension vx bins in Hough transform space
     * @param  vyBins   number of dimension vy bins in Hough transform space
     * @param  phiBins  number of dimension phi bins in Hough transform space
     * @param  phiConstant phi value in radians if phiBins == 1
     * @param  pBins    number of dimension p bins in Hough transform space
     * @param  pMin     minimum p value
     * @param  pMax     maximum p value
     * @param  sidePointsForTangent Maximum number of points to take from each side
     *                              of a point on a curve in determining a tangent
     * @param  maxBufferSize maximum Hough transform size in megabytes
     * @param  numParabolas number of parabolas to be found
     */
    public AlgorithmHoughParabola(ModelImage destImg, ModelImage srcImg, int vxBins, int vyBins, int phiBins, 
                                double phiConstant, int pBins, float pMin, float pMax, int sidePointsForTangent,
                                int maxBufferSize, int numParabolas) {
        super(destImg, srcImg);
        this.vxBins = vxBins;
        this.vyBins = vyBins;
        this.phiBins = phiBins;
        this.phiConstant = phiConstant;
        this.pBins = pBins;
        this.pMin = pMin;
        this.pMax = pMax;
        this.sidePointsForTangent = sidePointsForTangent;
        this.maxBufferSize = maxBufferSize;
        this.numParabolas = numParabolas;
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
        int x, y;
        int offset;

        int xDim;

        int yDim;

        int sourceSlice;

        int i, j, k, k2, m, n, n2, c;
        int index, indexDest;
        
        byte[] srcBuffer;
        short[] countBuffer;
        float pBuffer[];
        float phiBuffer[] = null;
        boolean test = false;
        int largestValue;
        int largestIndex;
        int numParabolasFound;
        boolean selectedParabola[];
        JDialogHoughParabolaChoice choice;
        byte value = 0;
        long longNumBins;
        int bytesPerCell;
        int numBins;
        long desiredBytes;
        long actualBytesAvailable;
        double shrinkFactor = 1.0;
        double xVertex;
        double yVertex;
        double vxArray[];
        double vyArray[];
        //double phiArray[];
        double phiScale;
        float pScale;
        int maxParabolaPoints;
        float vxTable[];
        float vyTable[];
        double phiTable[];
        float pTable[];
        int countTable[];
        double p;
        double numerator;
        double denominator;
        double xdel;
        double ydel;
        //double sinArray[];
        //double cosArray[];
        int xy;
        int xyp;
        // 0 for x - xVertex negative, y - yVertex negative
        // 1 for x - xVertex negative, y - yVertex zero or positive
        // 2 for x - xVertex zero or positive, y - yVertex negative
        // 3 for x - xVertex zero or positive, y - yVertex zero or positive
        double  maxDistance[];
        int xMax[];
        int yMax[];
        // 2 endpoints for each parabola
        int xEndPoint[][];
        int yEndPoint[][];
        int typeEndPoint[][];
        double distanceEndPoint[][];
        double distance;
        int xStart;
        int xFinish;
        double xVal;
        double a;
        double b;
        double cv;
        double xf;
        double cosphi = 0.0;
        double sinphi = 0.0;
        double root;
        int y1;
        int y2;
        int yStart;
        int yFinish;
        double yVal;
        double yf;
        int x1;
        int x2;
        int pruningPix;
        boolean entireImage;
        AlgorithmMorphology2D algoMorph2D;
        int neighbors;
        int neigh0;
        int neigh1;
        int neigh2;
        int neigh3;
        int neigh4;
        int neigh5;
        int neigh6;
        int neigh7;
        int numPoints;
        int endPoints;
        int neighbor1[];
        int neighbor2[];
        int numOpenCurves;
        boolean foundArray[];
        int openStart[];
        int openLength[];
        int indexArray[];
        float slopeArray[];
        int indexPtr = 0;
        int startPtr;
        int nextPoint;
        float xArray[];
        float yArray[];
        int presentSidePoints;
        float tangentX;
        float tangentY;
        float xPoints[];
        float yPoints[];
        float xpc;
        float ypc;
        double xSqSum;
        double ySqSum;
        double xySum;
        double x1t;
        double x2t;
        double y1t;
        double y2t;
        double var;
        double d1;
        double d2;
        double slope;
        boolean foundPoint[];
        int numPointsFound;
        int newNumPoints;
        int newIndexArray[];
        float newSlopeArray[];
        double cd;
        double ce;
        double cf;
        double sin2phi1;
        double sin2phi2;
        double cos2phi1;
        double cos2phi2;
        double deSquares;
        double defSquares;
        double ef;
        double df;
        double vars;
        double varc;
        double twophi1;
        double twophi2;
        int numPhi = 0;
        double phi[] = new double[2];
        double theta = 0.0;
        int m1;
        DecimalFormat dfmt = new DecimalFormat("0.##");
        double cosphit;
        double sinphit;
        double diff;
        VOI parabolaVOI[] = null;
        VOI parabolaVOI2[] = null;
        int parabolasDrawn = 0;
        int parabolaPoints[];
        int parabolaPoints2[];
        float xArr[];
        float yArr[];
        float zArr[];
        float xArr2[];
        float yArr2[];
        float zArr2[];

        if (srcImage == null) {
            displayError("Source Image is null");
            finalize();

            return;
        }
        
        fireProgressStateChanged(srcImage.getImageName(), "Hough parabola ...");

        xDim = srcImage.getExtents()[0];
        yDim = srcImage.getExtents()[1];
        sourceSlice = xDim * yDim; 
        
        // Calculate the desired number of bins that would be used for each parameter if memory were not a
        // limitation.
        bytesPerCell = 4 + 2; // float for p parameter and short for count;
        longNumBins = (long)vxBins * (long)vyBins * (long)phiBins * (long)pBins;
        numBins = (int)longNumBins;
        desiredBytes = longNumBins * (long)bytesPerCell;
        actualBytesAvailable = (long)maxBufferSize * 1024L * 1024L;
        if (actualBytesAvailable < desiredBytes) {
            // Must shrink the number of each bins used for each parameter by the third or fourth root of
            // deisredBytes/actualBytesAvailable to keep the buffer size down to actualBytesAvailable
            if (phiBins > 1) {
                shrinkFactor = Math.pow((double)desiredBytes/(double)actualBytesAvailable, 0.25);
                phiBins = (int)Math.ceil(phiBins/shrinkFactor);
            }
            else {
                shrinkFactor = Math.pow((double)desiredBytes/(double)actualBytesAvailable,1.0/3.0);
            }
            vxBins = (int)Math.ceil(vxBins/shrinkFactor);
            vyBins = (int)Math.ceil(vyBins/shrinkFactor);
            pBins = (int)Math.ceil(pBins/shrinkFactor);
            numBins = vxBins * vyBins * phiBins * pBins;
        } // if (actualBytesAvailable < desiredBytes)
        xy = vxBins * vyBins;
        xyp = xy * phiBins;
        ViewUserInterface.getReference().setDataText("vxBins = " + vxBins + "\n");
        ViewUserInterface.getReference().setDataText("vyBins = " + vyBins + "\n");
        ViewUserInterface.getReference().setDataText("phiBins = " + phiBins + "\n");
        ViewUserInterface.getReference().setDataText("pBins = " + pBins + "\n");
        ViewUserInterface.getReference().setDataText("numBins = " + numBins + "\n");
        
        maxParabolaPoints = 2*Math.max(xDim, yDim) + Math.min(xDim, yDim);

        srcBuffer = new byte[sourceSlice];

        try {
            srcImage.exportData(0, sourceSlice, srcBuffer);
        } catch (IOException e) {
            MipavUtil.displayError("IOException " + e + " on srcImage.exportData");

            setCompleted(false);

            return;
        }
        
        for (i = 0; i < sourceSlice; i++) {
            if (srcBuffer[i] != 0) {
                value = srcBuffer[i];
                break;
            }
        }
        
        if (test) {
            for (y = 0; y < yDim; y++) {
                offset = y * xDim;
                for (x = 0; x < xDim; x++) {
                    index = offset + x; 
                    srcBuffer[index] = 0;
                } // for (x = 0; x < xDim; x++)
            } // for (y = 0; y < yDim; y++)
            
            xVertex = (xDim-1)/2.0;
            yVertex = (yDim-1)/2.0;
           
            // y = (int)Math.round(yVertex + 0.25*(x - xVertex)*(x - xVertex));
            // p = 1.0; 
            // 90 degrees
            /*xStart = (int)(xVertex - 20);
            xFinish = (int)(xVertex + 20);
            xdel = (double)(xFinish - xStart)/(double)maxParabolaPoints;
            for (j = 0; j <= maxParabolaPoints; j++) {
                xVal = xStart + j * xdel;
                xf = xVal - xVertex;
                y = (int)Math.round(yVertex + 0.25*xf*xf);
                if ((y >= 0) && (y < yDim)) {
                    index = (int)Math.round(xVal) + y * xDim;
                    srcBuffer[index] = value;
                }
            }*/
            
            // 270 degrees
            /*xStart = (int)(xVertex - 20);
            xFinish = (int)(xVertex + 20);
            xdel = (double)(xFinish - xStart)/(double)maxParabolaPoints;
            for (j = 0; j <= maxParabolaPoints; j++) {
                xVal = xStart + j * xdel;
                xf = xVal - xVertex;
                y = (int)Math.round(yVertex - 0.25*xf*xf);
                if ((y >= 0) && (y < yDim)) {
                    index = (int)Math.round(xVal) + y * xDim;
                    srcBuffer[index] = value;
                }
            }*/
            
            // 0 degrees
            /*xVertex = (xDim-1)/4.0;
            yVertex = (yDim-1)/4.0;
            yStart = (int)(yVertex - 20);
            yFinish = (int)(yVertex + 20);
            ydel = (double)(yFinish - yStart)/(double)maxParabolaPoints;
            for (j = 0; j <= maxParabolaPoints; j++) {
                yVal = yStart + j * ydel;
                yf = yVal - yVertex;
                x = (int)Math.round(xVertex + 0.25*yf*yf);
                if ((x >= 0) && (x < xDim)) {
                    index = x + + (int)Math.round(yVal) * xDim;
                    srcBuffer[index] = value;
                }
            }*/
            
            // 180 degrees
            xVertex = (xDim-1)/2.0;
            yVertex = (yDim-1)/2.0;
            yStart = (int)(yVertex - 20);
            yFinish = (int)(yVertex + 20);
            ydel = (double)(yFinish - yStart)/(double)maxParabolaPoints;
            for (j = 0; j <= maxParabolaPoints; j++) {
                yVal = yStart + j * ydel;
                yf = yVal - yVertex;
                x = (int)Math.round(xVertex - 0.25*yf*yf);
                if ((x >= 0) && (x < xDim)) {
                    index = x + + (int)Math.round(yVal) * xDim;
                    srcBuffer[index] = value;
                }
            }
            
            // 45 degrees
            //p = 1
            // If phi is set at 45 degrees, wrong answer may result only pMin is made large enough.
            xVertex = (xDim-1)/2.0;
            yVertex = (yDim-1)/2.0;
            xStart = (int)(xVertex - 20);
            xFinish = (int)(xVertex + 20);
            xdel = (double)(xFinish - xStart)/(double)maxParabolaPoints;
            cosphit = Math.cos(Math.PI/4.0);
            sinphit = Math.sin(Math.PI/4.0);
            a = cosphit * cosphit;
            for (j = 0; j <= maxParabolaPoints; j++) {
                xVal = xStart + j * xdel;
                xf = xVal - xVertex;
                b = -2.0 * sinphit*(xf*cosphit + 2.0);
                cv = xf*xf*sinphit*sinphit - 4.0*xf*cosphit;
                var = b*b - 4.0*a*cv;
                if (var >= 0.0) {
                    root = Math.sqrt(var);
                    y1 = (int)Math.round(yVertex + (-b - root)/(2.0 * a));
                    y2 = (int)Math.round(yVertex + (-b + root)/(2.0 * a));
                    if ((y1 >= 0) && (y1 < yDim)) {
                        index = (int)Math.round(xVal) + y1 * xDim;
                        srcBuffer[index] = value;
                    }
                    if ((y2 >= 0) && (y2 < yDim)) {
                        index = (int)Math.round(xVal) + y2 * xDim;
                        srcBuffer[index] = value;
                    }
                } // if (var >= 0.0)
            }
            testImage = new ModelImage(ModelImage.UBYTE, srcImage.getExtents(), "Hough Parabola Test Image");
            try {
                testImage.importData(0, srcBuffer, true);
            }catch (IOException e) {
                MipavUtil.displayError("IOException " + e + " on testImage.importData");

                setCompleted(false);

                return;
            }
            new ViewJFrameImage(testImage);
        } // if (test)
        
        try {
            destImage.importData(0, srcBuffer, true);
        } catch (IOException e) {
            MipavUtil.displayError("IOException " + e + " on destImage.importData");

            setCompleted(false);

            return;
        }
        
        // Skeletonize the binary image
        // Prune off branches with 2 or less pixels
        pruningPix = 2;
        entireImage = true;
        algoMorph2D = new AlgorithmMorphology2D(destImage, 0, 0.0f, AlgorithmMorphology2D.SKELETONIZE, 0, 0, pruningPix, 0, entireImage);
        algoMorph2D.run();
        algoMorph2D.finalize();
        
        try {
            destImage.exportData(0, sourceSlice, srcBuffer);
        } catch (IOException e) {
            MipavUtil.displayError("IOException " + e + " on destImage.exportData");

            setCompleted(false);

            return;
        }
        
        // When a diagonal neighbor is adjacent to a horizontal or vertical neighbor,
        // remove the horizontal or vertical neighbor
        for (y = 0; y < yDim; y++) {
            offset = y * xDim;
            for (x = 0; x < xDim; x++) {
                index = offset + x;
                if (srcBuffer[index] != 0) {
                    neighbors = 0;
                    neigh0 = -1;
                    neigh1 = -1;
                    neigh2 = -1;
                    neigh3 = -1;
                    neigh4 = -1;
                    neigh5 = -1;
                    neigh6 = -1;
                    neigh7 = -1;
                    if (y > 0) {
                        if (x > 0) {
                            if (srcBuffer[index - xDim - 1] != 0) {
                                neighbors++;
                                neigh0 = index - xDim - 1;
                            }
                        }
                        if (srcBuffer[index - xDim] != 0) {
                            neighbors++;
                            neigh1 = index - xDim;
                        }
                        if (x < xDim - 1) {
                            if (srcBuffer[index - xDim + 1] != 0) {
                                neighbors++;
                                neigh2 = index - xDim + 1;
                            }
                        }
                    } // if (y > 0)
                    if (x > 0) {
                        if (srcBuffer[index - 1] != 0) {
                            neighbors++;
                            neigh3 = index - 1;
                        }
                    } // if (x > 0)
                    if (x < xDim - 1) {
                        if (srcBuffer[index + 1] != 0) {
                            neighbors++;
                            neigh4 = index + 1;
                        }
                    } // if (x < xDim - 1)
                    if (y < yDim - 1) {
                        if (x > 0) {
                            if (srcBuffer[index + xDim - 1] != 0) {
                                neighbors++;
                                neigh5 = index + xDim - 1;
                            }
                        }
                        if (srcBuffer[index + xDim] != 0) {
                            neighbors++;
                            neigh6 = index + xDim;
                        }
                        if (x < xDim - 1) {
                            if (srcBuffer[index + xDim + 1] != 0) {
                                neighbors++;
                                neigh7 = index + xDim + 1;
                            }
                        }    
                    } // if (y < yDim - 1)
                    if (neighbors > 2) {
                        // Could be 3 or 4
                        if ((neigh0 >= 0) && (neigh1 >= 0)) {
                            srcBuffer[neigh1] = 0;
                            neigh1 = -1;
                        }
                        if ((neigh1 >= 0) && (neigh2 >= 0)) {
                            srcBuffer[neigh1] = 0;
                            neigh1 = -1;
                        }
                        if ((neigh0 >= 0) && (neigh3 >= 0)) {
                            srcBuffer[neigh3] = 0;
                            neigh3 = -1;
                        }
                        if ((neigh3 >= 0) && (neigh5 >= 0)) {
                            srcBuffer[neigh3] = 0;
                            neigh3 = -1;
                        }
                        if ((neigh2 >= 0) && (neigh4 >= 0)) {
                            srcBuffer[neigh4] = 0;
                            neigh4 = -1;
                        }
                        if ((neigh4 >= 0) && (neigh7 >= 0)) {
                            srcBuffer[neigh4] = 0;
                            neigh4 = -1;
                        }
                        if ((neigh5 >= 0) && (neigh6 >= 0)) {
                            srcBuffer[neigh6] = 0;
                            neigh6 = -1;
                        }
                        if ((neigh6 >= 0) && (neigh7 >= 0)) {
                            srcBuffer[neigh6] = 0;
                            neigh6 = -1;
                        }
                    }
                } // if (srcBuffer[index] != 0)
            } // for (x = 0; x < xDim; x++)
        } // for (y = 0; y < yDim; y++)
        
        // Remove points with more than 2 neighbors
        for (y = 0; y < yDim; y++) {
            offset = y * xDim;
            for (x = 0; x < xDim; x++) {
                index = offset + x;
                if (srcBuffer[index] != 0) {
                    neighbors = 0;
                    if (y > 0) {
                        if (x > 0) {
                            if (srcBuffer[index - xDim - 1] != 0) {
                                neighbors++;
                            }
                        }
                        if (srcBuffer[index - xDim] != 0) {
                            neighbors++;
                        }
                        if (x < xDim - 1) {
                            if (srcBuffer[index - xDim + 1] != 0) {
                                neighbors++;
                            }
                        }
                    } // if (y > 0)
                    if (x > 0) {
                        if (srcBuffer[index - 1] != 0) {
                            neighbors++;
                        }
                    } // if (x > 0)
                    if (x < xDim - 1) {
                        if (srcBuffer[index + 1] != 0) {
                            neighbors++;
                        }
                    } // if (x < xDim - 1)
                    if (y < yDim - 1) {
                        if (x > 0) {
                            if (srcBuffer[index + xDim - 1] != 0) {
                                neighbors++;
                            }
                        }
                        if (srcBuffer[index + xDim] != 0) {
                            neighbors++;
                        }
                        if (x < xDim - 1) {
                            if (srcBuffer[index + xDim + 1] != 0) {
                                neighbors++;
                            }
                        }    
                    } // if (y < yDim - 1)
                    if (neighbors > 2) {
                        srcBuffer[index] = 0;
                    }
                } // if (srcBuffer[index] != 0)
            } // for (x = 0; x < xDim; x++)
        } // for (y = 0; y < yDim; y++)
        
        
        // Find the 1 or 2 neighbors of every point
        // Find the number of end points, that is, points with only 1 neighbor
        // Delete isolated points with no neighbors
        endPoints = 0;
        neighbor1 = new int[sourceSlice];
        neighbor2 = new int[sourceSlice];
        for (i = 0; i < sourceSlice; i++) {
            neighbor1[i] = -1;
            neighbor2[i] = -1;
        }
        for (y = 0; y < yDim; y++) {
            offset = y * xDim;
            for (x = 0; x < xDim; x++) {
                index = offset + x;
                if (srcBuffer[index] != 0) {
                    neighbors = 0;
                    if (y > 0) {
                        if (x > 0) {
                            if (srcBuffer[index - xDim - 1] != 0) {
                                neighbors++;
                                neighbor1[index] = index - xDim - 1;
                            }
                        }
                        if (srcBuffer[index - xDim] != 0) {
                            neighbors++;
                            if (neighbor1[index] == -1) {
                                neighbor1[index] = index - xDim;
                            }
                            else {
                                neighbor2[index] = index - xDim;
                            }
                        }
                        if (x < xDim - 1) {
                            if (srcBuffer[index - xDim + 1] != 0) {
                                neighbors++;
                                if (neighbor1[index] == -1) {
                                    neighbor1[index] = index - xDim + 1;
                                }
                                else {
                                    neighbor2[index] = index - xDim + 1;
                                }
                            }
                        }
                    } // if (y > 0)
                    if (x > 0) {
                        if (srcBuffer[index - 1] != 0) {
                            neighbors++;
                            if (neighbor1[index] == -1) {
                                neighbor1[index] = index - 1;
                            }
                            else {
                                neighbor2[index] = index - 1;
                            }
                        }
                    } // if (x > 0)
                    if (x < xDim - 1) {
                        if (srcBuffer[index + 1] != 0) {
                            neighbors++;
                            if (neighbor1[index] == -1) {
                                neighbor1[index] = index + 1;
                            }
                            else {
                                neighbor2[index] = index + 1;
                            }
                        }
                    } // if (x < xDim - 1)
                    if (y < yDim - 1) {
                        if (x > 0) {
                            if (srcBuffer[index + xDim - 1] != 0) {
                                neighbors++;
                                if (neighbor1[index] == -1) {
                                    neighbor1[index] = index + xDim - 1;
                                }
                                else {
                                    neighbor2[index] = index + xDim - 1;
                                }
                            }
                        }
                        if (srcBuffer[index + xDim] != 0) {
                            neighbors++;
                            if (neighbor1[index] == -1) {
                                neighbor1[index] = index + xDim;
                            }
                            else {
                                neighbor2[index] = index + xDim;
                            }
                        }
                        if (x < xDim - 1) {
                            if (srcBuffer[index + xDim + 1] != 0) {
                                neighbors++;
                                if (neighbor1[index] == -1) {
                                    neighbor1[index] = index + xDim + 1;
                                }
                                else {
                                    neighbor2[index] = index + xDim + 1;
                                }
                            }
                        }    
                    } // if (y < yDim - 1)
                    if (neighbors == 0) {
                        srcBuffer[index] = 0;
                        neighbor1[index] = -1;
                        neighbor2[index] = -1;
                    }
                    else {
                        if (neighbors == 1) {
                            endPoints++;
                        }
                    }
                } // if (srcBuffer[index] != 0)
            } // for (x = 0; x < xDim; x++)
        } // for (y = 0; y < yDim; y++)
        
        numOpenCurves = endPoints/2;
        openStart = new int[numOpenCurves];
        openLength = new int[numOpenCurves];
        foundArray = new boolean[sourceSlice];
        // Set foundArray to true at every zero location
        for (y = 0; y < yDim; y++) {
            offset = y * xDim;
            for (x = 0; x < xDim; x++) {
                index = offset + x;
                if (srcBuffer[index] == 0) {
                    foundArray[index] = true;
                }
            }
        }
        
        // Find the starting positions and lengths of the open curves
        numPoints = 0;
        i = 0;
        for (y = 0; y < yDim; y++) {
            offset = y * xDim;
            for (x = 0; x < xDim; x++) {
                index = offset + x;
                if ((neighbor2[index] == -1) && (!foundArray[index])) {
                    foundArray[index] = true;
                    openStart[i] = index;
                    openLength[i]++;
                    numPoints++;
                    index = neighbor1[index];
                    foundArray[index] = true;
                    openLength[i]++;
                    numPoints++;
                    while(neighbor2[index] != -1) {
                        if (!foundArray[neighbor1[index]]) {
                            index = neighbor1[index];
                        }
                        else {
                            index = neighbor2[index];
                        }
                        foundArray[index] = true;
                        openLength[i]++;
                        numPoints++;
                    } // (while(neighbor2[index] != -1)
                    // Delete all open curves with only 2 points
                    // Also don't determine tangents of end points on longer curves,
                    // but use these 2 end points in determining tangents of more inner points
                    numPoints = numPoints - 2;
                    if (openLength[i] == 2) {
                        srcBuffer[openStart[i]] = 0;
                        srcBuffer[neighbor1[openStart[i]]] = 0;
                        numOpenCurves--;
                        openLength[i] = 0;
                    }
                    else {
                        i++;
                    }
                }
            }
        }
        
        ViewUserInterface.getReference().setDataText("Number of open curves = " + numOpenCurves + "\n");
        
        // For the open curves find the slope of the tangent line to the curve at a point
        // With a user specified sidePointsForTangent on each side of a point find the tangent line that
        // minimizes the sum of the squared distances from these side points to the tangent line 
        // Don't bother with the closed curves.  Closed curves cannot be part of a parabola.
        indexArray = new int[numPoints];
        slopeArray = new float[numPoints];
        for (i = 0; i < sourceSlice; i++) {
            if (srcBuffer[i] != 0) {
                foundArray[i] = false;
            }
        }
        indexPtr = 0;
        for (i = 0; i < numOpenCurves; i++) {
            startPtr = indexPtr;
            xArray = new float[openLength[i]];
            yArray = new float[openLength[i]];
            nextPoint = openStart[i];
            xArray[0] = nextPoint % xDim;
            yArray[0] = nextPoint / xDim;
            foundArray[nextPoint] = true;
            for (n = 1; n <= openLength[i] - 1; n++) {
              if (!foundArray[neighbor1[nextPoint]]) {
                  nextPoint = neighbor1[nextPoint];
              }
              else {
                  nextPoint = neighbor2[nextPoint];
              }
              if (n <= openLength[i] - 2) {
                  indexArray[indexPtr++] = nextPoint;
              }
              xArray[n] = nextPoint % xDim;
              yArray[n] = nextPoint / xDim;
              foundArray[nextPoint] = true;
            } // for (n = 0; n <= openLength[i] - 1; n++)
            indexPtr = startPtr;
            for (n = 1; n <= openLength[i] - 2; n++) {
                presentSidePoints = Math.min(sidePointsForTangent, n);
                presentSidePoints = Math.min(presentSidePoints, openLength[i] - 1 - n);
                if (presentSidePoints == 1) {
                    tangentX = (xArray[n+1] - xArray[n-1])/2.0f;
                    tangentY = (yArray[n+1] - yArray[n-1])/2.0f;
                    if (tangentX == 0.0f) {
                        slopeArray[indexPtr++] = Float.POSITIVE_INFINITY;
                    }
                    else {
                        slopeArray[indexPtr] = tangentY/tangentX;
                        indexPtr++;
                    }
                } // if (presentSidePoints == 1)
                else {
                    xPoints = new float[2*presentSidePoints+1];
                    yPoints = new float[2*presentSidePoints+1];
                    for (k = 0, j = n - presentSidePoints; j <= n + presentSidePoints; j++, k++) {
                        xPoints[k] = xArray[j];
                        yPoints[k] = yArray[j];
                    }
                    // Center all points for tangent point touching curve at (0, 0)
                    // That is, use an x axis and a y axis going thru the tangent point
                    xpc = xPoints[presentSidePoints];
                    ypc = yPoints[presentSidePoints];
                    for (k = 0; k < xPoints.length; k++) {
                        xPoints[k] = xPoints[k] - xpc;
                        yPoints[k] = yPoints[k] - ypc;
                    }
                    xSqSum = 0.0;
                    ySqSum = 0.0;
                    xySum = 0.0;
                    for (k = 0; k < xPoints.length; k++) {
                        xSqSum += xPoints[k]*xPoints[k];
                        ySqSum += yPoints[k]*yPoints[k];
                        xySum += xPoints[k]*yPoints[k];
                    }
                    if (xySum != 0.0) {
                        var = Math.sqrt(ySqSum*ySqSum - 2.0 * xSqSum * ySqSum + xSqSum * xSqSum + 4.0 * xySum * xySum);
                        x1t = 0.5 * ((-ySqSum + xSqSum + var)/xySum);
                        x2t = 0.5 * ((-ySqSum + xSqSum - var)/xySum);
                        y1t = 1.0;
                        y2t = 1.0;
                    }
                    else {
                        // If all points are symmetric to either this new x axis or this new y axis, then
                        // their product sum is 0 and the tangentX, tangentY must be 1,0 or 0,1
                        x1t = 1.0;
                        x2t = 0.0;
                        y1t = 0.0;
                        y2t = 1.0;
                    }
                    // x1t, y1t and x2t, y2t are perpindicular.  To find the solution, calculate the sum of
                    // distances from the curve points to the line for the 2 cases
                    // The shortest distance is the correct solution
                    // Distance from AX + BY + C = 0 to P1 is 
                    // abs((A*x1 + B*y1 + C))/sqrt(A**2 + B**2)
                    // Here A = slope, B = -1, and C = 0.
                    d1 = 0.0;
                    for (k = 0; k < xPoints.length; k++) {
                        if (x1t == 0.0) {
                            // Infinite slope thru (0,0)
                            d1 += Math.abs(yPoints[k]);
                        }
                        else if (y1t == 0.0) {
                            // Zero slope thru (0, 0)
                            d1 += Math.abs(xPoints[k]);
                        }
                        else {
                            slope = y1t/x1t;
                            d1 += Math.abs((slope * xPoints[k] - yPoints[k])/Math.sqrt(slope*slope + 1));
                        }
                    }
                    d2 = 0.0;
                    for (k = 0; k < xPoints.length; k++) {
                        if (x2t == 0.0) {
                            // Infinite slope thru (0,0)
                            d2 += Math.abs(yPoints[k]);
                        }
                        else if (y2t == 0.0) {
                            // Zero slope thru (0, 0)
                            d2 += Math.abs(xPoints[k]);
                        }
                        else {
                            slope = y2t/x2t;
                            d2 += Math.abs((slope * xPoints[k] - yPoints[k])/Math.sqrt(slope*slope + 1));
                        }
                    }
                    if (d1 < d2) {
                        tangentX = (float)x1t;
                        tangentY = (float)y1t;
                    }
                    else {
                        tangentX = (float)x2t;
                        tangentY = (float)y2t;
                    }
                    if (tangentX == 0.0f) {
                        slopeArray[indexPtr++] = Float.POSITIVE_INFINITY;
                        
                    }
                    else {
                        slopeArray[indexPtr] = tangentY/tangentX;
                        indexPtr++;
                    }    
                }
            } // for (n = 2; n <= openLength[i] - 2; n++)
        } // for (i = 0; i < numOpenCurves; i++)
        openStart = null;
        openLength = null;
        xArray = null;
        yArray = null;

        pBuffer = new float[numBins];
        if (phiBins > 1) {
            phiBuffer = new float[numBins];
        }
        countBuffer = new short[numBins];
        
        // Calculate vxArray, vyArray, phiArray, cosArray, sinArray values
        vxArray = new double[vxBins];
        vyArray = new double[vyBins];
        //phiArray = new double[phiBins];
        //cosArray = new double[phiBins];
        //sinArray = new double[phiBins];
        for (i = 0; i < vxBins; i++) {
            vxArray[i] = ((double)(i * (xDim - 1)))/((double)(vxBins - 1));
        }
        for (i = 0; i < vyBins; i++) {
            vyArray[i] = ((double)(i * (yDim - 1)))/((double)(vyBins - 1));
        }
        /*if (phiBins > 1) {
            for (i = 0; i < phiBins; i++) {
                phiArray[i] = i * 2.0 * Math.PI/phiBins;
                cosArray[i] = Math.cos(phiArray[i]);
                sinArray[i] = Math.sin(phiArray[i]);
            }
        }
        else {
            phiArray[0] = phiConstant;
            cosArray[0] = Math.cos(phiConstant);
            sinArray[0] = Math.sin(phiConstant);
        }*/
        if (phiBins == 1) {
            numPhi = 1;
            phi[0] = phiConstant;
            cosphi = Math.cos(phiConstant);
            sinphi = Math.sin(phiConstant);
        }
        pScale = (pBins - 1)/(pMax - pMin);
        phiScale = phiBins/(2.0 * Math.PI);
        maxParabolaPoints = 2*Math.max(xDim, yDim) + Math.min(xDim, yDim);
        
        vxTable = new float[numParabolas];
        vyTable = new float[numParabolas];
        phiTable = new double[numParabolas];
        pTable = new float[numParabolas];
        countTable = new int[numParabolas];
        numParabolasFound = 0;
        
        maxDistance = new double[4];
        xMax = new int[4];
        yMax = new int[4];
        xEndPoint = new int[numParabolas][2];
        yEndPoint = new int[numParabolas][2];
        typeEndPoint = new int[numParabolas][2];
        distanceEndPoint = new double[numParabolas][2];
        foundPoint = new boolean[numPoints];
        for (c = 0; c < numParabolas; c++) {
            // Calculate the Hough transform
            fireProgressStateChanged("Calculating Hough parabola " + String.valueOf(c+1));
            for (i = 0; i < numPoints; i++) {
                index = indexArray[i];
                x = index % xDim;
                y = index / xDim;
                for (j = 0; j < vxBins; j++) {
                    xdel = x - vxArray[j];
                    for (k = 0; k < vyBins; k++) {
                        ydel = y - vyArray[k];
                        if (phiBins > 1) {
                            theta = Math.atan2(ydel, xdel);
                            if (theta <= 0.0) {
                                theta = theta + 2.0 * Math.PI;
                            }
                            // theta and the desired angle phi cannot differ by more than PI/2.
                            //for (m = 0; m < phiBins; m++) {
                            // cd is cosine coefficient, ce is sine coefficient, cf is constant coefficient
                            if ((xdel == 0.0f) && (ydel == 0.0f)) {
                                continue;
                            }
                            else if (Float.isInfinite(slopeArray[i])) {
                                // (y - vy)*sin(2*phi) + 3*(x - vx) + (x - vx)*cos(2*phi) = 0
                                cd = xdel;
                                ce = ydel;
                                cf = 3*xdel;
                            }
                            else if (slopeArray[i] == 0.0f) {
                               // (x - vx)*sin(2*phi) + 3*(y - vy) - (y - vy)*cos(2*phi)= 0;
                                cd = -ydel;
                                ce = xdel;
                                cf = 3*ydel;
                            }
                            else {
                                // (dy/dx) = ((x - vx)*sin(2*phi) + 3*(y - vy)- (y - vy)*cos(2*phi))/((y - vy)*sin(2*phi) + 3*(x - vx)+ (x - vx)*cos(2*phi))
                                cd = xdel*slopeArray[i] + ydel;
                                ce = ydel*slopeArray[i] - xdel;
                                cf = 3*xdel*slopeArray[i] - 3*ydel;
                            }
                            deSquares = cd*cd + ce*ce;
                            defSquares = deSquares - cf*cf;
                            if (defSquares < 0.0) {
                                continue;
                            }
                            ef = ce * cf;
                            df = cd * cf;
                            numPhi = 0;
                            if (defSquares > 0.0) {
                                root = Math.sqrt(defSquares);
                                vars = cd * root;
                                varc = ce * root;
                                sin2phi1 = (-ef + vars)/deSquares;
                                if ((sin2phi1 >= -1.0) && (sin2phi1 <= 1.0)) {
                                    cos2phi1 = (-df - varc)/deSquares;
                                    if ((sin2phi1 >= 0.0) && (cos2phi1 >= 0.0)) {
                                        twophi1 = Math.asin(sin2phi1);   
                                    }
                                    else if ((sin2phi1 >= 0.0) && (cos2phi1 < 0.0)) {
                                        twophi1 = Math.PI - Math.asin(sin2phi1);
                                    }
                                    else if ((sin2phi1 < 0.0) && (cos2phi1 < 0.0)) {
                                        twophi1 = Math.PI - Math.asin(sin2phi1);
                                    }
                                    else { // (sin2phi1 < 0.0) && (cos2phi1 >= 0.0)
                                        twophi1 = 2.0*Math.PI + Math.asin(sin2phi1);
                                    }
                                    diff = Math.abs(theta - twophi1/2.0);
                                    if ((diff < Math.PI/2.0) || (diff > 3.0*Math.PI/2.0)) {
                                        phi[0] = twophi1/2.0;
                                    }
                                    else {
                                        phi[0] = twophi1/2.0 + Math.PI;
                                    }
                                    numPhi = 1;
                                } // if ((sin2phi1 >= -1.0) && (sin2phi1 <= 1.0))
                                sin2phi2 = (-ef - vars)/deSquares;
                                if ((sin2phi2 >= -1.0) && (sin2phi2 <= 1.0)) {
                                    cos2phi2 = (-df + varc)/deSquares;
                                    if ((sin2phi2 >= 0.0) && (cos2phi2 >= 0.0)) {
                                        twophi2 = Math.asin(sin2phi2);   
                                    }
                                    else if ((sin2phi2 >= 0.0) && (cos2phi2 < 0.0)) {
                                        twophi2 = Math.PI - Math.asin(sin2phi2);
                                    }
                                    else if ((sin2phi2 < 0.0) && (cos2phi2 < 0.0)) {
                                        twophi2 = Math.PI - Math.asin(sin2phi2);
                                    }
                                    else { // (sin2phi2 < 0.0) && (cos2phi2 >= 0.0)
                                        twophi2 = 2.0*Math.PI + Math.asin(sin2phi2);
                                    }
                                    diff = Math.abs(theta - twophi2/2.0);
                                    if ((diff < Math.PI/2.0) || (diff > 3.0*Math.PI/2.0)) {
                                        phi[numPhi] = twophi2/2.0;
                                    }
                                    else {
                                        phi[numPhi] = twophi2/2.0 + Math.PI;
                                    }
                                    numPhi++;
                                } // if ((sin2phi2 >= -2.0) && (sin2phi2 <= 1.0))
                            } // if (defSquares > 0.0)
                            else { // defSquares == 0.0
                                sin2phi1 = -ef/deSquares;   
                                if ((sin2phi1 >= -1.0) && (sin2phi1 <= 1.0)) {
                                    cos2phi1 = -df/deSquares;
                                    if ((sin2phi1 >= 0.0) && (cos2phi1 >= 0.0)) {
                                        twophi1 = Math.asin(sin2phi1);   
                                    }
                                    else if ((sin2phi1 >= 0.0) && (cos2phi1 < 0.0)) {
                                        twophi1 = Math.PI - Math.asin(sin2phi1);
                                    }
                                    else if ((sin2phi1 < 0.0) && (cos2phi1 < 0.0)) {
                                        twophi1 = Math.PI - Math.asin(sin2phi1);
                                    }
                                    else { // (sin2phi1 < 0.0) && (cos2phi1 >= 0.0)
                                        twophi1 = 2.0*Math.PI + Math.asin(sin2phi1);
                                    }
                                    diff = Math.abs(theta - twophi1/2.0);
                                    if ((diff < Math.PI/2.0) || (diff > 3.0 * Math.PI/2.0)) {
                                        phi[0] = twophi1/2.0;
                                    }
                                    else {
                                        phi[0] = twophi1/2.0 + Math.PI;
                                    }
                                    numPhi = 1;
                                } // if ((sin2phi1 >= -1.0) && (sin2phi1 <= 1.0))
                            } // else defSquares == 0.0
                        } // if (phiBins > 1)
                        /*numerator = ydel*cosArray[m] - xdel*sinArray[m];
                        numerator = numerator * numerator;
                        denominator = 4*(ydel*sinArray[m] + xdel*cosArray[m]); */
                        for (m = 0; m < numPhi; m++) {
                            if (phiBins > 1) {
                                cosphi = Math.cos(phi[m]);
                                sinphi = Math.sin(phi[m]);
                            }
                            numerator = ydel*cosphi - xdel*sinphi;
                            numerator = numerator * numerator;
                            denominator = 4*(ydel*sinphi + xdel*cosphi);
                            if (denominator != 0.0) {
                                p = numerator/denominator;
                                if ((p >= pMin) && (p <= pMax)) {
                                    n = (int)((p - pMin)*pScale);
                                    if (phiBins > 1) {
                                        m1 = (int)(phi[m]*phiScale);
                                        indexDest = j + k * vxBins + m1 * xy + n * xyp;
                                        phiBuffer[indexDest] += phi[m];
                                    }
                                    else {
                                        indexDest = j + k * vxBins + n * xyp;
                                    }
                                    countBuffer[indexDest]++;
                                    pBuffer[indexDest] += p;
                                } // if ((p >= pMin) && (p <= pMax)
                            } // if (denominator != 0.0)
                       // } // for (m = 0; m < phiBins; m++)
                        }  // for (m = 0; m < numPhi; m++)
                    } // for (k = 0; k < vyBins; k++)
                } // for (j = 0; j < vxBins; j++)
            } // for (i = 0; i < numPoints; i++)
            
            
           
            // Find cell with the highest counts
            // Obtain the vx, vy, phi, p, and count values of this parabola
            fireProgressStateChanged("Finding Hough peak parabola " + String.valueOf(c+1));
            
            largestValue = 0;
            largestIndex = -1;
            for (j = 0; j < numBins; j++) {
                if (countBuffer[j] > largestValue) {
                    largestValue = countBuffer[j];
                    largestIndex = j;
                }
            } // for (j = 0; j < houghSlice; j++)
            if (largestIndex == -1) {
                break;
            }
            
            numParabolasFound++;
            vxTable[c] = largestIndex % vxBins;
            vxTable[c] = vxTable[c] * ((float)(xDim - 1))/((float)(vxBins - 1));
            vyTable[c] = (largestIndex % xy)/vxBins;
            vyTable[c] = vyTable[c] * ((float)(yDim - 1))/((float)(vyBins - 1));
            if (phiBins > 1) {
                //phiTable[c] = (largestIndex % xyp)/xy;
                //phiTable[c] = (float)(phiTable[c] * 2.0 * Math.PI/phiBins);
                phiTable[c] = phiBuffer[largestIndex]/largestValue;
            }
            else {
                phiTable[c] = (float)phiConstant;
            }
            pTable[c] = pBuffer[largestIndex]/largestValue;
            countTable[c] = largestValue;
            ViewUserInterface.getReference().setDataText("Parabola # " + numParabolasFound + " found\n");
            ViewUserInterface.getReference().setDataText(" x vertex = " + dfmt.format(vxTable[c]) + "\n");
            ViewUserInterface.getReference().setDataText(" y vertex = " + dfmt.format(vyTable[c]) + "\n");
            ViewUserInterface.getReference().setDataText(" phi = " + dfmt.format(phiTable[c] * 180/Math.PI) + " degrees\n");
            ViewUserInterface.getReference().setDataText(" Vertex to focus distance = " + dfmt.format(pTable[c]) + "\n");
            ViewUserInterface.getReference().setDataText(" Points counted = " + countTable[c] + "\n");
            // Zero hough buffer for next run
            for (i = 0; i < numBins; i++) {
                countBuffer[i] = 0;
                pBuffer[i] = 0.0f;
                if (phiBins > 1) {
                    phiBuffer[i] = 0.0f;
                }
            }
            // zero all points in the source slice that contributed to this parabola
            // and find the endpoints of the parabola
            fireProgressStateChanged("Zeroing source parabola " + String.valueOf(c+1));
            for (i = 0; i < 4; i++) {
                maxDistance[i] = -1.0;
                xMax[i] = -1;
                yMax[i] = -1;
            }
            numPointsFound = 0;
            for (i = 0; i < numPoints; i++) {
                index = indexArray[i];
                x = index % xDim;
                y = index / xDim;
                for (j = 0; j < vxBins; j++) {
                    xdel = x - vxArray[j];
                    for (k = 0; k < vyBins; k++) {
                        ydel = y - vyArray[k];
                        if (phiBins > 1) {
                            theta = Math.atan2(ydel, xdel);
                            if (theta <= 0.0) {
                                theta = theta + 2.0 * Math.PI;
                            }
                            // theta and the desired angle phi cannot differ by more than PI/2.
                            //for (m = 0; m < phiBins; m++) {
                            // cd is cosine coefficient, ce is sine coefficient, cf is constant coefficient
                            if ((xdel == 0.0f) && (ydel == 0.0f)) {
                                continue;
                            }
                            else if (Float.isInfinite(slopeArray[i])) {
                                // (y - vy)*sin(2*phi) + 3*(x - vx) + (x - vx)*cos(2*phi) = 0
                                cd = xdel;
                                ce = ydel;
                                cf = 3*xdel;
                            }
                            else if (slopeArray[i] == 0.0f) {
                               // (x - vx)*sin(2*phi) + 3*(y - vy) - (y - vy)*cos(2*phi)= 0;
                                cd = -ydel;
                                ce = xdel;
                                cf = 3*ydel;
                            }
                            else {
                                // (dy/dx) = ((x - vx)*sin(2*phi) + 3*(y - vy)- (y - vy)*cos(2*phi))/((y - vy)*sin(2*phi) + 3*(x - vx)+ (x - vx)*cos(2*phi))
                                cd = xdel*slopeArray[i] + ydel;
                                ce = ydel*slopeArray[i] - xdel;
                                cf = 3*xdel*slopeArray[i] - 3*ydel;
                            }
                            deSquares = cd*cd + ce*ce;
                            defSquares = deSquares - cf*cf;
                            if (defSquares < 0.0) {
                                continue;
                            }
                            ef = ce * cf;
                            df = cd * cf;
                            numPhi = 0;
                            if (defSquares > 0.0) {
                                root = Math.sqrt(defSquares);
                                vars = cd * root;
                                varc = ce * root;
                                sin2phi1 = (-ef + vars)/deSquares;
                                if ((sin2phi1 >= -1.0) && (sin2phi1 <= 1.0)) {
                                    cos2phi1 = (-df - varc)/deSquares;
                                    if ((sin2phi1 >= 0.0) && (cos2phi1 >= 0.0)) {
                                        twophi1 = Math.asin(sin2phi1);   
                                    }
                                    else if ((sin2phi1 >= 0.0) && (cos2phi1 < 0.0)) {
                                        twophi1 = Math.PI - Math.asin(sin2phi1);
                                    }
                                    else if ((sin2phi1 < 0.0) && (cos2phi1 < 0.0)) {
                                        twophi1 = Math.PI - Math.asin(sin2phi1);
                                    }
                                    else { // (sin2phi1 < 0.0) && (cos2phi1 >= 0.0)
                                        twophi1 = 2.0*Math.PI + Math.asin(sin2phi1);
                                    }
                                    diff = Math.abs(theta - twophi1/2.0);
                                    if ((diff < Math.PI/2.0) || (diff > 3.0 * Math.PI/2.0)){
                                        phi[0] = twophi1/2.0;
                                    }
                                    else {
                                        phi[0] = twophi1/2.0 + Math.PI;
                                    }
                                    numPhi = 1;
                                } // if ((sin2phi1 >= -1.0) && (sin2phi1 <= 1.0))
                                sin2phi2 = (-ef - vars)/deSquares;
                                if ((sin2phi2 >= -1.0) && (sin2phi2 <= 1.0)) {
                                    cos2phi2 = (-df + varc)/deSquares;
                                    if ((sin2phi2 >= 0.0) && (cos2phi2 >= 0.0)) {
                                        twophi2 = Math.asin(sin2phi2);   
                                    }
                                    else if ((sin2phi2 >= 0.0) && (cos2phi2 < 0.0)) {
                                        twophi2 = Math.PI - Math.asin(sin2phi2);
                                    }
                                    else if ((sin2phi2 < 0.0) && (cos2phi2 < 0.0)) {
                                        twophi2 = Math.PI - Math.asin(sin2phi2);
                                    }
                                    else { // (sin2phi2 < 0.0) && (cos2phi2 >= 0.0)
                                        twophi2 = 2.0*Math.PI + Math.asin(sin2phi2);
                                    }
                                    diff = Math.abs(theta - twophi2/2.0);
                                    if ((diff < Math.PI/2.0) || (diff > 3.0*Math.PI/2.0)) {
                                        phi[numPhi] = twophi2/2.0;
                                    }
                                    else {
                                        phi[numPhi] = twophi2/2.0 + Math.PI;
                                    }
                                    numPhi++;
                                } // if ((sin2phi2 >= -2.0) && (sin2phi2 <= 1.0))
                            } // if (defSquares > 0.0)
                            else { // defSquares == 0.0
                                sin2phi1 = -ef/deSquares;   
                                if ((sin2phi1 >= -1.0) && (sin2phi1 <= 1.0)) {
                                    cos2phi1 = -df/deSquares;
                                    if ((sin2phi1 >= 0.0) && (cos2phi1 >= 0.0)) {
                                        twophi1 = Math.asin(sin2phi1);   
                                    }
                                    else if ((sin2phi1 >= 0.0) && (cos2phi1 < 0.0)) {
                                        twophi1 = Math.PI - Math.asin(sin2phi1);
                                    }
                                    else if ((sin2phi1 < 0.0) && (cos2phi1 < 0.0)) {
                                        twophi1 = Math.PI - Math.asin(sin2phi1);
                                    }
                                    else { // (sin2phi1 < 0.0) && (cos2phi1 >= 0.0)
                                        twophi1 = 2.0*Math.PI + Math.asin(sin2phi1);
                                    }
                                    diff = Math.abs(theta - twophi1/2.0);
                                    if ((diff < Math.PI/2.0) || (diff > 3.0*Math.PI/2.0)) {
                                        phi[0] = twophi1/2.0;
                                    }
                                    else {
                                        phi[0] = twophi1/2.0 + Math.PI;
                                    }
                                    numPhi = 1;
                                } // if ((sin2phi1 >= -1.0) && (sin2phi1 <= 1.0))
                            } // else defSquares == 0.0
                        } // if (phiBins > 1)
                        /* numerator = ydel*cosArray[m] - xdel*sinArray[m];
                        numerator = numerator * numerator;
                        denominator = 4*(ydel*sinArray[m] + xdel*cosArray[m]); */
                        for (m = 0; m < numPhi; m++) {
                            if (phiBins > 1) {
                                cosphi = Math.cos(phi[m]);
                                sinphi = Math.sin(phi[m]);
                            }
                            numerator = ydel*cosphi - xdel*sinphi;
                            numerator = numerator * numerator;
                            denominator = 4*(ydel*sinphi + xdel*cosphi);
                            if (denominator != 0.0) {
                                p = numerator/denominator;
                                if ((p >= pMin) && (p <= pMax)) {
                                    n = (int)((p - pMin)*pScale);
                                    if (phiBins > 1) {
                                        m1 = (int)(phi[m]*phiScale);
                                        indexDest = j + k * vxBins + m1 * xy + n * xyp;
                                    }
                                    else {
                                        indexDest = j + k * vxBins + n * xyp;
                                    }
                                    if (indexDest == largestIndex) {
                                        foundPoint[i] = true;
                                        numPointsFound++;
                                        distance = Math.sqrt(xdel*xdel + ydel*ydel);
                                        if ((xdel < 0.0) && (ydel < 0.0)) {
                                            if (distance > maxDistance[0]) {
                                                maxDistance[0] = distance;
                                                xMax[0] = x;
                                                yMax[0] = y;
                                            }
                                        } // if ((xdel < 0.0) && (ydel < 0.0))
                                        else if ((xdel < 0.0) && (ydel >= 0.0)) {
                                            if (distance > maxDistance[1]) {
                                                maxDistance[1] = distance;
                                                xMax[1] = x;
                                                yMax[1] = y;
                                            }
                                        } // else if ((xdel < 0.0) && (ydel >= 0.0))
                                        else if ((xdel >= 0.0) && (ydel < 0.0)) {
                                            if (distance > maxDistance[2]) {
                                                maxDistance[2] = distance;
                                                xMax[2] = x;
                                                yMax[2] = y;
                                            }
                                        } // else if ((xdel >= 0.0) && (ydel < 0.0))
                                        else {
                                            if (distance > maxDistance[3]) {
                                                maxDistance[3] = distance;
                                                xMax[3] = x;
                                                yMax[3] = y;
                                            }
                                        }
                                    } // if (indexDest == largestIndex)
                                } // if ((p >= pMin) && (p <= pMax)
                            } // if (denominator != 0.0)*/
                        //} // for (m = 0; m < phiBins; m++)
                        }  // for (m = 0; m < numPhi; m++)
                    } // for (k = 0; k < vyBins; k++)
                } // for (j = 0; j < vxBins; j++)
            } // for (i = 0; i < numPoints; i++)
            // Find one end point
            distanceEndPoint[c][0] = -1.0;
            for (i = 0; i < 4; i++) {
                if (maxDistance[i] > distanceEndPoint[c][0]) {
                    distanceEndPoint[c][0] = maxDistance[i];
                    xEndPoint[c][0] = xMax[i];
                    yEndPoint[c][0] = yMax[i];
                    typeEndPoint[c][0] = i;
                }
            }
            distanceEndPoint[c][1] = -1.0;
            for (i = 0; i < 4; i++) {
                if (i != typeEndPoint[c][0]) {
                    if (maxDistance[i] > distanceEndPoint[c][1]) {
                        distanceEndPoint[c][1] = maxDistance[i];
                        xEndPoint[c][1] = xMax[i];
                        yEndPoint[c][1] = yMax[i];
                        typeEndPoint[c][1] = i;
                    }
                }
            }
            // If a parabola was found, then delete the points found along its perimiter
            // from indexArray and slopeArray before running the
            // Hough transform again
            
            if (numPointsFound > 0) {
                newNumPoints = numPoints - numPointsFound;
                if (newNumPoints == 0) {
                    break;
                }
                newIndexArray = new int[newNumPoints];
                newSlopeArray = new float[newNumPoints];
                for (i = 0, j = 0; i < numPoints; i++) {
                    if (!foundPoint[i]) {
                        newIndexArray[j] = indexArray[i];
                        newSlopeArray[j] = slopeArray[i];
                        j++;
                    }
                } // for (i = 0, j = 0; i < numPoints; i++)
                numPoints = newNumPoints;
                foundPoint = null;
                foundPoint = new boolean[numPoints];
                indexArray = null;
                indexArray = new int[numPoints];
                slopeArray = null;
                slopeArray = new float[numPoints];
                for (i = 0; i < numPoints; i++) {
                    indexArray[i] = newIndexArray[i];
                    slopeArray[i] = newSlopeArray[i];
                }
                newIndexArray = null;
                newSlopeArray = null;
            } // if (numPointsFound > 0)
        } // for (c = 0; c < numParabolas; c++)
        
        // Restore original source values
        if (!test) {
            try {
                srcImage.exportData(0, sourceSlice, srcBuffer);
            } catch (IOException e) {
                MipavUtil.displayError("IOException " + e + " on srcImage.exportData");
    
                setCompleted(false);
    
                return;
            }
        } // if (!test)
        
        // Create a dialog with numParabolasFound vxTable[i], vyTable[i], phiTable[i], pTable[i], and
        // countTable[i] values, where the user will select a check box to have the selected parabola drawn.
        selectedParabola = new boolean[numParabolasFound];
        
        choice = new JDialogHoughParabolaChoice(ViewUserInterface.getReference().getMainFrame(), vxTable,
                 xDim, vyTable, yDim, phiTable, phiBins, pTable, pMin, pMax, countTable, selectedParabola);
        
        if (!choice.okayPressed() ) {
            setCompleted(false);
            return;
        }
        
        for (i = 0; i < numParabolasFound; i++) {
            if (selectedParabola[i]) {
                parabolasDrawn++;
            }
        }
        
        // Draw selected parabolas
        parabolaPoints = new int[parabolasDrawn];
        parabolaPoints2 = new int[parabolasDrawn];
        for (i = 0, k = 0, k2 = 0; i < numParabolasFound; i++) {
            if (selectedParabola[i]) {
                if (Math.abs(xEndPoint[i][1] - xEndPoint[i][0]) >= Math.abs(yEndPoint[i][1] - yEndPoint[i][0])) {
                    xStart = Math.min(xEndPoint[i][0], xEndPoint[i][1]);
                    xFinish = Math.max(xEndPoint[i][0], xEndPoint[i][1]);
                    xdel = (double)(xFinish - xStart)/(double)maxParabolaPoints;
                    if ((phiTable[i] != Math.PI/2.0) && (phiTable[i] != 3.0*Math.PI/2.0)) {
                        cosphi = Math.cos(phiTable[i]);
                        sinphi = Math.sin(phiTable[i]);
                        a = cosphi * cosphi;
                        for (j = 0; j <= maxParabolaPoints; j++) {
                            xVal = xStart + j * xdel;
                            xf = xVal - vxTable[i];
                            b = -2.0 * sinphi*(xf*cosphi + 2.0*pTable[i]);
                            cv = xf*xf*sinphi*sinphi - 4.0*pTable[i]*xf*cosphi;
                            var = b*b - 4.0*a*cv;
                            if (var >= 0.0) {
                                root = Math.sqrt(var);
                                y1 = (int)Math.round(vyTable[i] + (-b - root)/(2.0 * a));
                                y2 = (int)Math.round(vyTable[i] + (-b + root)/(2.0 * a));
                                if ((y1 >= 0) && (y1 < yDim)) {
                                    index = (int)Math.round(xVal) + y1 * xDim;
                                    srcBuffer[index] = value;
                                    parabolaPoints[k]++;
                                }
                                if ((y2 >= 0) && (y2 < yDim)) {
                                    index = (int)Math.round(xVal) + y2 * xDim;
                                    srcBuffer[index] = value;
                                    parabolaPoints2[k2]++;
                                }
                            } // if (var >= 0.0)
                        }
                    } // if ((phiTable[i] != Math.PI/2.0) && (phiTable[i] != 3.0*Math.PI/2.0))
                    else if (phiTable[i] == Math.PI/2.0) {
                        for (j = 0; j <= maxParabolaPoints; j++) {
                            xVal = xStart + j * xdel;
                            xf = xVal - vxTable[i];
                            y1 = (int)Math.round(vyTable[i] + xf*xf/(4.0 * pTable[i]));
                            if ((y1 >= 0) && (y1 < yDim)) {
                                index = (int)Math.round(xVal) + y1 * xDim;
                                srcBuffer[index] = value;
                                parabolaPoints[k]++;
                            }
                        }
                    } // else if (phiTable[i] == Math.PI/2.0)
                    else { // phiTable[i] == 3.0 * Math.PI/2.0
                        for (j = 0; j <= maxParabolaPoints; j++) {
                            xVal = xStart + j * xdel;
                            xf = xVal - vxTable[i];
                            y1 = (int)Math.round(vyTable[i] - xf*xf/(4.0 * pTable[i]));
                            if ((y1 >= 0) && (y1 < yDim)) {
                                index = (int)Math.round(xVal) + y1 * xDim;
                                srcBuffer[index] = value;
                                parabolaPoints[k]++;
                            }
                        }    
                    } // else phiTable[i] == 3.0 * Math.PI/2.0
                } // if (Math.abs(xEndPoint[i][1] - xEndPoint[i][0]) >= Math.abs(yEndPoint[i][1] - yEndPoint[i][0]))
                else {
                    yStart = Math.min(yEndPoint[i][0], yEndPoint[i][1]);
                    yFinish = Math.max(yEndPoint[i][0], yEndPoint[i][1]);
                    ydel = (double)(yFinish - yStart)/(double)maxParabolaPoints;
                    if ((phiTable[i] != 0.0) && (phiTable[i] != Math.PI)) {
                        cosphi = Math.cos(phiTable[i]);
                        sinphi = Math.sin(phiTable[i]); 
                        a = sinphi * sinphi;
                        for (j = 0; j <= maxParabolaPoints; j++) {
                            yVal = yStart + j * ydel;
                            yf = yVal - vyTable[i];
                            b = -2.0 * cosphi * (yf*sinphi + 2.0*pTable[i]);
                            cv = yf*yf*cosphi*cosphi - 4.0*pTable[i]*yf*sinphi;
                            var = b*b - 4.0*a*cv;
                            if (var >= 0.0) {
                                root = Math.sqrt(var);
                                x1 = (int)Math.round(vxTable[i] + (-b - root)/(2.0 * a));
                                x2 = (int)Math.round(vxTable[i] + (-b + root)/(2.0 * a));
                                if ((x1 >= 0) && (x1 < xDim)) {
                                    index = x1 + (int)Math.round(yVal) * xDim;
                                    srcBuffer[index] = value;
                                    parabolaPoints[k]++;
                                }
                                if ((x2 >= 0) && (x2 < xDim)) {
                                    index = x2 + (int)Math.round(yVal) * xDim;
                                    srcBuffer[index] = value;
                                    parabolaPoints2[k2]++;
                                }
                            } // if (var = 0.0)
                        }
                    } // if (phiTable[i] != 0.0)
                    else if (phiTable[i] == 0.0){ // phiTable = 0.0
                        for (j = 0; j < maxParabolaPoints; j++) {
                            yVal = yStart + j * ydel;
                            yf = yVal - vyTable[i];
                            x1 = (int)Math.round(vxTable[i] + yf*yf/(4.0 * pTable[i]));
                            if ((x1 >= 0) && (x1 < xDim)) {
                                index = x1 + (int)Math.round(yVal) * xDim;
                                srcBuffer[index] = value;
                                parabolaPoints[k]++;
                            }
                        }
                    } // else if (phiTable[i] == 0.0)
                    else { // phiTable[i] == Math.PI
                        for (j = 0; j < maxParabolaPoints; j++) {
                            yVal = yStart + j * ydel;
                            yf = yVal - vyTable[i];
                            x1 = (int)Math.round(vxTable[i] - yf*yf/(4.0 * pTable[i]));
                            if ((x1 >= 0) && (x1 < xDim)) {
                                index = x1 + (int)Math.round(yVal) * xDim;
                                srcBuffer[index] = value;
                                parabolaPoints[k]++;
                            }
                        }  
                    } // else phiTable[i] == Math.PI
                } // else (Math.abs(xEndPoint[i][1] - xEndPoint[i][0]) < Math.abs(yEndPoint[i][1] - yEndPoint[i][0]))
                k++;
                k2++;
            } // if (selectedParabola[i])
        } // for (i = 0, k = 0, k2 = 0; i < numParabolaFound; i++)
        
        parabolaVOI = new VOI[parabolasDrawn];
        parabolaVOI2 = new VOI[parabolasDrawn];
        for (i = 0, k = 0, k2 = 0; i < numParabolasFound; i++) {
            if (selectedParabola[i]) {
                n = 0;
                parabolaVOI[k] = new VOI((short)k, "parabolaVOI" + Integer.toString(k), VOI.POLYLINE, -1.0f);
                parabolaVOI[k].setColor(Color.red);
                xArr = new float[parabolaPoints[k]];
                yArr = new float[parabolaPoints[k]];
                zArr = new float[parabolaPoints[k]];
                n2 = 0;
                parabolaVOI2[k2] = new VOI((short)(parabolasDrawn+k2), "parabolaVOI" + Integer.toString(parabolasDrawn + k2),
                                          VOI.POLYLINE, -1.0f);
                parabolaVOI2[k2].setColor(Color.red);
                xArr2 = new float[parabolaPoints2[k2]];
                yArr2 = new float[parabolaPoints2[k2]];
                zArr2 = new float[parabolaPoints2[k2]];
                if (Math.abs(xEndPoint[i][1] - xEndPoint[i][0]) >= Math.abs(yEndPoint[i][1] - yEndPoint[i][0])) {
                    xStart = Math.min(xEndPoint[i][0], xEndPoint[i][1]);
                    xFinish = Math.max(xEndPoint[i][0], xEndPoint[i][1]);
                    xdel = (double)(xFinish - xStart)/(double)maxParabolaPoints;
                    if ((phiTable[i] != Math.PI/2.0) && (phiTable[i] != 3.0*Math.PI/2.0)) {
                        cosphi = Math.cos(phiTable[i]);
                        sinphi = Math.sin(phiTable[i]);
                        a = cosphi * cosphi;
                        for (j = 0; j <= maxParabolaPoints; j++) {
                            xVal = xStart + j * xdel;
                            xf = xVal - vxTable[i];
                            b = -2.0 * sinphi*(xf*cosphi + 2.0*pTable[i]);
                            cv = xf*xf*sinphi*sinphi - 4.0*pTable[i]*xf*cosphi;
                            var = b*b - 4.0*a*cv;
                            if (var >= 0.0) {
                                root = Math.sqrt(var);
                                y1 = (int)Math.round(vyTable[i] + (-b - root)/(2.0 * a));
                                y2 = (int)Math.round(vyTable[i] + (-b + root)/(2.0 * a));
                                if ((y1 >= 0) && (y1 < yDim)) {
                                    xArr[n] = Math.round(xVal);
                                    yArr[n++] = y1;
                                }
                                if ((y2 >= 0) && (y2 < yDim)) {
                                    xArr2[n2] = Math.round(xVal);
                                    yArr2[n2++] = y2;
                                }
                            } // if (var >= 0.0)
                        }
                    } // if ((phiTable[i] != Math.PI/2.0) && (phiTable[i] != 3.0*Math.PI/2.0))
                    else if (phiTable[i] == Math.PI/2.0) {
                        for (j = 0; j <= maxParabolaPoints; j++) {
                            xVal = xStart + j * xdel;
                            xf = xVal - vxTable[i];
                            y1 = (int)Math.round(vyTable[i] + xf*xf/(4.0 * pTable[i]));
                            if ((y1 >= 0) && (y1 < yDim)) {
                                xArr[n] = Math.round(xVal);
                                yArr[n++] = y1;
                            }
                        }
                    } // else if (phiTable[i] == Math.PI/2.0)
                    else { // phiTable[i] == 3.0 * Math.PI/2.0
                        for (j = 0; j <= maxParabolaPoints; j++) {
                            xVal = xStart + j * xdel;
                            xf = xVal - vxTable[i];
                            y1 = (int)Math.round(vyTable[i] - xf*xf/(4.0 * pTable[i]));
                            if ((y1 >= 0) && (y1 < yDim)) {
                                xArr[n] = Math.round(xVal);
                                yArr[n++] = y1;
                            }
                        }    
                    } // else phiTable[i] == 3.0 * Math.PI/2.0
                } // if (Math.abs(xEndPoint[i][1] - xEndPoint[i][0]) >= Math.abs(yEndPoint[i][1] - yEndPoint[i][0]))
                else {
                    yStart = Math.min(yEndPoint[i][0], yEndPoint[i][1]);
                    yFinish = Math.max(yEndPoint[i][0], yEndPoint[i][1]);
                    ydel = (double)(yFinish - yStart)/(double)maxParabolaPoints;
                    if ((phiTable[i] != 0.0) && (phiTable[i] != Math.PI)) {
                        cosphi = Math.cos(phiTable[i]);
                        sinphi = Math.sin(phiTable[i]); 
                        a = sinphi * sinphi;
                        for (j = 0; j <= maxParabolaPoints; j++) {
                            yVal = yStart + j * ydel;
                            yf = yVal - vyTable[i];
                            b = -2.0 * cosphi * (yf*sinphi + 2.0*pTable[i]);
                            cv = yf*yf*cosphi*cosphi - 4.0*pTable[i]*yf*sinphi;
                            var = b*b - 4.0*a*cv;
                            if (var >= 0.0) {
                                root = Math.sqrt(var);
                                x1 = (int)Math.round(vxTable[i] + (-b - root)/(2.0 * a));
                                x2 = (int)Math.round(vxTable[i] + (-b + root)/(2.0 * a));
                                if ((x1 >= 0) && (x1 < xDim)) {
                                    xArr[n] = x1;
                                    yArr[n++] = Math.round(yVal);
                                }
                                if ((x2 >= 0) && (x2 < xDim)) {
                                    xArr2[n2] = x2;
                                    yArr2[n2++] = Math.round(yVal);
                                }
                            } // if (var = 0.0)
                        }
                    } // if (phiTable[i] != 0.0)
                    else if (phiTable[i] == 0.0){ // phiTable = 0.0
                        for (j = 0; j < maxParabolaPoints; j++) {
                            yVal = yStart + j * ydel;
                            yf = yVal - vyTable[i];
                            x1 = (int)Math.round(vxTable[i] + yf*yf/(4.0 * pTable[i]));
                            if ((x1 >= 0) && (x1 < xDim)) {
                                xArr[n] = x1;
                                yArr[n++] = Math.round(yVal);
                            }
                        }
                    } // else if (phiTable[i] == 0.0)
                    else { // phiTable[i] == Math.PI
                        for (j = 0; j < maxParabolaPoints; j++) {
                            yVal = yStart + j * ydel;
                            yf = yVal - vyTable[i];
                            x1 = (int)Math.round(vxTable[i] - yf*yf/(4.0 * pTable[i]));
                            if ((x1 >= 0) && (x1 < xDim)) {
                                xArr[n] = x1;
                                yArr[n++] = Math.round(yVal);
                            }
                        }  
                    } // else phiTable[i] == Math.PI
                } // else (Math.abs(xEndPoint[i][1] - xEndPoint[i][0]) < Math.abs(yEndPoint[i][1] - yEndPoint[i][0]))
                if (n > 0) {
                    parabolaVOI[k].importCurve(xArr, yArr, zArr);
                    ((VOIContour)(parabolaVOI[k].getCurves().elementAt(0))).setFixed(true);
                    destImage.registerVOI(parabolaVOI[k]);
                }
                k++;
                if (n2 > 0) {
                    parabolaVOI2[k2].importCurve(xArr2, yArr2, zArr2);
                    ((VOIContour)(parabolaVOI2[k2].getCurves().elementAt(0))).setFixed(true);
                    destImage.registerVOI(parabolaVOI2[k2]);
                }
                k2++;
            } // if (selectedParabola[i])
        } // for (i = 0, k = 0, k2 = 0; i < numParabolaFound; i++)
        
        try {
            destImage.importData(0, srcBuffer, true);
        } catch (IOException e) {
            MipavUtil.displayError("IOException " + e + " on destImage.importData");

            setCompleted(false);

            return;
        }
        
        setCompleted(true);
        return;
    }
}
