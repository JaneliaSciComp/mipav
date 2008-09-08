package gov.nih.mipav.model.algorithms;


import gov.nih.mipav.model.file.*;
import gov.nih.mipav.model.structures.*;

import gov.nih.mipav.view.*;

import java.awt.*;

import java.io.*;

import java.util.*;


/**
 * This was the start of an unsuccessful port attempt of Power Crust software by Nina Amenta, Sunghee Choi, amnd Ravi Krishna Kolluri.
 */
public class AlgorithmSkelGeom3D extends AlgorithmBase {

    //~ Static fields/initializers -------------------------------------------------------------------------------------

    /** DOCUMENT ME! */
    private static final int MAXBLOCKS = 1000;

    /** DOCUMENT ME! */
    private static final int BLOCKSIZE = 100000;

    /** DOCUMENT ME! */
    private static final double DELIFT = 0.0;

    /** DOCUMENT ME! */
    private static final int MAXDIM = 8;

    /** DOCUMENT ME! */
    private static final short CNV = 0; /* status of simplex, if it's on convex hull */

    /** DOCUMENT ME! */
    private static final short VV = 1; /* Status if a regular simplex */

    /** DOCUMENT ME! */
    private static final short SLV = -1; /* if orient3d=0, sliver simplex */

    /**
     * DBL_MANT_DIG is I1MACH(14), the number of base-B digits for double precision. IEEE 754 double precision has 64
     * bits with 1 sign bit, 11 exponent bits, and 52 fraction, mantissa, or signficand bits. Floating-point numbers are
     * typically stored in normalized form, with the radix point after the first non-zero digit. In base two, the only
     * possible non-zero digit is 1. Thus, a leading digit of 1 can always be assumed and need not be explicitly
     * represented. Therefore, the mantissa effectively has 53 bits of resolution.
     */
    private static final int DBL_MANT_DIG = 53;

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** DOCUMENT ME! */
    private basisS b = null;

    /** DOCUMENT ME! */
    private int[] B = new int[251];

    /**
     * If true, throw away both poles for cells which are not long and skinny. Set bad to true on noise-free inputs from
     * surfaces with sharp corners, amd make sure that the estR is less than one ( The smaller estR, the more it will
     * interpolate data near the corners from the data on adjacient smooth surfaces).
     */
    private boolean bad = false;

    /** DOCUMENT ME! */
    private BasisSList basisSList;

    /** DOCUMENT ME! */
    private basisS bCheckPerps = null;

    /** DOCUMENT ME! */
    private double bErrMin;

    /** DOCUMENT ME! */
    private double bErrMinSq;

    /** DOCUMENT ME! */
    private int blockLoc = 0;

    /** DOCUMENT ME! */
    private double[][] bound = new double[8][3];

    /** DOCUMENT ME! */
    private double ccwErrBoundA;

    /** DOCUMENT ME! */
    private double ccwErrBoundB;

    /** DOCUMENT ME! */
    private double ccwErrBoundC;

    /** DOCUMENT ME! */
    private int cDim;

    /** DOCUMENT ME! */
    private double[] center = new double[3];

    /** DOCUMENT ME! */
    private simplex chRoot;

    /**
     * cosine (pi - alpha), where alpha is the angle between deep intersecting balls. Same as theta, but for trying to
     * label unlabeled poles, the second time around. (default = 0.3). Once you start fooling around with the estR, bad,
     * and/or defer options, some poles might fail to be labeled by the regular algorithm. This parameter is passed to a
     * second-pass clean-up function which should be a little more liberal in propagating labels. Make the deep value
     * smaller when you see lots of "unlabeled pole" messages.
     */
    private float deep = 0.3f;

    /**
     * If true, no propagation for 1st pole of Voronoi cells which are not long and skinny. Set to true when
     * reconstructing surfaces with boundaries. It allows a sample on the boundary to have two outside poles.
     */
    private boolean defer = false;

    /** DOCUMENT ME! */
    private int dim = 3;

    /**
     * epsilon = D1MACH(4) Machine epsilon is the smallest positive epsilon such that (1.0 + epsilon) != 1.0. epsilon =
     * 2**(1 - doubleDigits) = 2**(1 - 53) = 2**(-52) epsilon = 2.224460e-16 epsilon is called the largest relative
     * spacing
     */
    private double epsilon;

    /** DOCUMENT ME! */
    private int errorStatus = 0;

    /**
     * Estimate for sampling density constant. 1.0 or larger turns it off. (default = 0.6). Used to estimate whether
     * Voronoi cells are "well-shaped", for handling noise and sharp- corners hack. This is the r in r-sampling, that
     * is, the minimum distance to the nearest sample on the surface, as a fraction of distance to the medial axis. When
     * r is small the sampling is expected to be very dense, the Voronoi cells are expected to be really long and skinny
     * and the poles of fat Voronoi cells are thrown away. On noise-free inputs with no sharp corners, you can give a
     * value >= 1, and perhaps get a good reconstruction from a sparser input sample. Setting estR small might help get
     * good reconstructions from dense but noisy samples.
     */
    private float estR = 0.6f;

    /** DOCUMENT ME! */
    private int exactBits;

    /** DOCUMENT ME! */
    private int failCount;

    /** DOCUMENT ME! */
    private double iccErrBoundA;

    /** DOCUMENT ME! */
    private double iccErrBoundB;

    /** DOCUMENT ME! */
    private double iccErrBoundC;

    /** Point at infinity for vd; value not used. */
    private double[] infinity = new double[10];

    /** DOCUMENT ME! */
    private basisS infinityBasis;

    /** DOCUMENT ME! */
    private double ispErrBoundA;

    /** DOCUMENT ME! */
    private double ispErrBoundB;

    /** DOCUMENT ME! */
    private double ispErrBoundC;
    // The base representation of the exponent
    // FLT_RADIX = 2

    /** DOCUMENT ME! */
    private double lDetBound;

    /** array of lower bounds for lfs of each sample. */
    private double[] lfsLb;

    /** DOCUMENT ME! */
    private int lScale;

    /** DOCUMENT ME! */
    private int matSize = 0;

    /** DOCUMENT ME! */
    private double[] maxs = new double[] { -Double.MAX_VALUE, -Double.MAX_VALUE, Double.MAX_VALUE };

    /** DOCUMENT ME! */
    private double maxScale;

    /** Minimum value for outer pixel to be an object pixel. */
    private float minObjectValue;

    /** DOCUMENT ME! */
    private double[] mins = new double[] { Double.MAX_VALUE, Double.MAX_VALUE, Double.MAX_VALUE };

    /** Multiply all floating point surface positions by multUp and round them into integers. */
    private double multUp = 10.0;

    /** DOCUMENT ME! */
    private simplex ns;

    /** DOCUMENT ME! */
    private int numBlocks = 0;

    /** DOCUMENT ME! */
    private int numSites = 0;

    /** DOCUMENT ME! */
    private double o3dErrBoundA;

    /** DOCUMENT ME! */
    private double o3dErrBoundB;

    /** DOCUMENT ME! */
    private double o3dErrBoundC;

    /** omaxs, omins, and center are used for 8 vertices for bounding box. */
    private double[] omaxs = new double[3];

    /** DOCUMENT ME! */
    private double[] omins = new double[3];

    /** DOCUMENT ME! */
    private double[] p = new double[8];

    /** DOCUMENT ME! */
    private int pDim;

    /** DOCUMENT ME! */
    private neighbor pNeigh;

    /** DOCUMENT ME! */
    private int pNum;

    /** Arrays of poles per sample;. */
    private simplex[] pole1;

    /** DOCUMENT ME! */
    private simplex[] pole2;

    /** DOCUMENT ME! */
    private boolean powerDiagram = false;

    /** DOCUMENT ME! */
    private RandomNumberGen randomGen;

    /** DOCUMENT ME! */
    private int rDim;

    /** DOCUMENT ME! */
    private double resultErrBound;

    /** DOCUMENT ME! */
    private simplex root;

    /** DOCUMENT ME! */
    private double sb;

    /** DOCUMENT ME! */
    private int[] shufMat;

    /** DOCUMENT ME! */
    private int shufPointer;

    /** DOCUMENT ME! */
    private SimplexList simplexList;

    /** DOCUMENT ME! */
    private double[][][] siteBlocks = new double[MAXBLOCKS][][];

    /** If true, perform slice by slice hole filling on the thresholded image. */
    private boolean sliceHoleFilling = true;

    /** DOCUMENT ME! */
    private int sliceSize;

    /** DOCUMENT ME! */
    private boolean sNormalCall = false;

    /** Site number. */
    private int sNum = 0;

    /** splitter is used to split floating-point numbers into two half-length significands for exact multiplication. */
    private double splitter;

    /** DOCUMENT ME! */
    private int sqbtest = 0;

    /** DOCUMENT ME! */
    private int ss = MAXDIM;

    /** DOCUMENT ME! */
    private int ssTri = 2000;

    /** DOCUMENT ME! */
    private simplex[] st = null;

    /** DOCUMENT ME! */
    private simplex[] stTri = null;

    /**
     * cosine (pi - alpha), where alpha is the angle between deep intersecting balls. (default = 0.4). When a pole gets
     * labeled as inside or outside, it propagates the same label to any neighboring pole whose polar ball deeply
     * intersects its own. This value is never changed.
     */
    private float theta = 0.4f;

    /** DOCUMENT ME! */
    private int totLength;

    /** DOCUMENT ME! */
    private basisS ttBasis;

    /** If true return Delaunay triangulation from buildConvexHull() Found in hullmain.c. */
    private boolean vd = true;

    /** Found in ch.c. */
    private boolean vdCh;

    /** DOCUMENT ME! */
    private int vertIndex;

    /** DOCUMENT ME! */
    private int vNum = -1;

    /** DOCUMENT ME! */
    private byte[] vol;

    /** DOCUMENT ME! */
    private float[] volFloat;

    /** DOCUMENT ME! */
    private int xDim;

    /** DOCUMENT ME! */
    private int yDim;

    /** DOCUMENT ME! */
    private int zDim;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Constructor for putting a 3D skeleton into the source image.
     *
     * @param  srcImg            the source image.
     * @param  minObjectValue    minimum value for outer pixel to be an object pixel
     * @param  sliceHoleFilling  If true, perform slice by slice hole filling on the segmented
     * @param  estR              Estimate for sampling density constant
     * @param  bad               If true, throw away both poles from cells which are not long and skinny
     * @param  defer             If true, no propagation for 1st pole of Voronoi cells which are not long and skinny
     * @param  deep              cosine (pi - alpha) for trying to label unlabled poles, the second time around
     */
    public AlgorithmSkelGeom3D(ModelImage srcImg, float minObjectValue, boolean sliceHoleFilling, float estR,
                               boolean bad, boolean defer, float deep) {
        super(null, srcImg);
        this.minObjectValue = minObjectValue;
        this.sliceHoleFilling = sliceHoleFilling;
        this.estR = estR;
        this.bad = bad;
        this.defer = defer;
        this.deep = deep;
    }

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * DOCUMENT ME!
     */
    public void runAlgorithm() {
        int i, j, k;
        AlgorithmVOIExtraction algoVOIExtraction;
        ModelImage grayImage;
        ViewVOIVector VOIs = null;
        int nVOIs;
        Vector[] contours;
        int nContours;
        int nPts;
        int n;
        double xLoc, yLoc, zLoc;
        short[] shortMask;
        int x, y, z;
        int index;
        double width;
        boolean[] used;
        float[] xArr;
        float[] yArr;
        float[] zArr;
        VOI newPtVOI;
        short ptNum;
        short segNum;
        boolean haveSegColor;
        Color segColor = null;
        int[] extents2D;
        byte[] buffer2D;
        ModelImage grayImage2D;
        FileInfoBase fileInfo;
        boolean wholeImage = true;
        AlgorithmMorphology2D fillHolesAlgo2D;
        int kernel;
        float sphereDiameter;
        int method;
        int itersDilation;
        int itersErosion;
        int numPruningPixels;
        int edgingType;
        AlgorithmMorphology3D idObjectsAlgo3D;
        int numObjects;
        ModelImage idImage;
        int[] idArray;
        int[] idCount;
        int maxCount;
        int maxID;
        double half;
        double check;
        double lastCheck;
        boolean everyOther;
        long time;

        time = System.currentTimeMillis();

        /*int test = 1;
         * if (test == 1) { createCylinder(); setCompleted(true); return;}*/

        

        fireProgressStateChanged(srcImage.getImageName(), "Performing 3D skeletonization...");


        everyOther = true;
        half = 0.5;
        epsilon = 1.0;
        splitter = 1.0;
        check = 1.0;

        /* Repeatedly divide `epsilon' by two until it is too small to add to    */
        /*   one without causing roundoff.  (Also check if the sum is equal to   */
        /*   the previous sum, for machines that round up instead of using exact */
        /*   rounding.  Not that this library will work on such machines anyway. */
        do {
            lastCheck = check;
            epsilon *= half;

            if (everyOther) {
                splitter *= 2.0;
            }

            everyOther = !everyOther;
            check = 1.0 + epsilon;
        } while ((check != 1.0) && (check != lastCheck));

        splitter += 1.0;

        /* Error bounds for orientation and incircle tests. */
        resultErrBound = (3.0 + (8.0 * epsilon)) * epsilon;
        ccwErrBoundA = (3.0 + (16.0 * epsilon)) * epsilon;
        ccwErrBoundB = (2.0 + (12.0 * epsilon)) * epsilon;
        ccwErrBoundC = (9.0 + (64.0 * epsilon)) * epsilon * epsilon;
        o3dErrBoundA = (7.0 + (56.0 * epsilon)) * epsilon;
        o3dErrBoundB = (3.0 + (28.0 * epsilon)) * epsilon;
        o3dErrBoundC = (26.0 + (288.0 * epsilon)) * epsilon * epsilon;
        iccErrBoundA = (10.0 + (96.0 * epsilon)) * epsilon;
        iccErrBoundB = (4.0 + (48.0 * epsilon)) * epsilon;
        iccErrBoundC = (44.0 + (576.0 * epsilon)) * epsilon * epsilon;
        ispErrBoundA = (16.0 + (224.0 * epsilon)) * epsilon;
        ispErrBoundB = (5.0 + (72.0 * epsilon)) * epsilon;
        ispErrBoundC = (71.0 + (1408.0 * epsilon)) * epsilon * epsilon;

        exactBits = DBL_MANT_DIG;
        bErrMin = epsilon * MAXDIM * (1 << MAXDIM) * MAXDIM * 3.01;
        bErrMinSq = bErrMin * bErrMin;

        xDim = srcImage.getExtents()[0];
        yDim = srcImage.getExtents()[1];
        zDim = srcImage.getExtents()[2];
        sliceSize = xDim * yDim;
        totLength = sliceSize * zDim;

        File file;
        RandomAccessFile raFile;

        try {
            volFloat = new float[totLength];
            vol = new byte[totLength];
        } catch (OutOfMemoryError e) {
            MipavUtil.displayError("Out of memory error allocating volFloat and vol");

            setCompleted(false);

            return;
        }

        try {
            srcImage.exportData(0, totLength, volFloat);
        } catch (IOException e) {
            MipavUtil.displayError("IO error on srcImage.exportData");

            setCompleted(false);

            return;
        }

        for (i = 0; i < totLength; i++) {

            if (volFloat[i] >= minObjectValue) {
                vol[i] = 1;
            }
        }


        /*boolean test;
         * test = false; if (test) { solidImage = new ModelImage(ModelStorageBase.BYTE, srcImage.getExtents(),
         *        "solidImage", srcImage.getUserInterface()); try {     solidImage.importData(0, vol, true); } catch
         * (IOException e) {     MipavUtil.displayError("IOException on soldImage.importData");     setCompleted(false);
         *     return; }
         *
         * new ViewJFrameImage(solidImage);  setCompleted(true); return;} // if (test)*/

        if (sliceHoleFilling) {
            fireProgressStateChanged("Slice by slice hole filling");
            Preferences.debug("Slice by slice hole filling\n");
            extents2D = new int[2];
            extents2D[0] = srcImage.getExtents()[0];
            extents2D[1] = srcImage.getExtents()[1];
            buffer2D = new byte[sliceSize];
            grayImage2D = new ModelImage(ModelStorageBase.USHORT, extents2D, srcImage.getImageName() + "_gray2D");

            fileInfo = grayImage2D.getFileInfo()[0];
            fileInfo.setResolutions(srcImage.getFileInfo()[0].getResolutions());
            fileInfo.setUnitsOfMeasure(srcImage.getFileInfo()[0].getUnitsOfMeasure());
            grayImage2D.setFileInfo(fileInfo, 0);

            for (z = 0; z < zDim; z++) {

                for (i = 0; i < sliceSize; i++) {
                    buffer2D[i] = vol[(z * sliceSize) + i];
                }

                try {
                    grayImage2D.importData(0, buffer2D, true);
                } catch (IOException e) {
                    MipavUtil.displayError("Error on grayImage2D importData");

                    setCompleted(false);

                    return;
                }

                fillHolesAlgo2D = new AlgorithmMorphology2D(grayImage2D, 0, 0, AlgorithmMorphology2D.FILL_HOLES, 0, 0,
                                                            0, 0, wholeImage);
                fillHolesAlgo2D.run();
                fillHolesAlgo2D.finalize();
                fillHolesAlgo2D = null;

                try {
                    grayImage2D.exportData(0, sliceSize, buffer2D);
                } catch (IOException e) {
                    MipavUtil.displayError("Error on grayImage2D exportData");

                    setCompleted(false);

                    return;
                }

                for (i = 0; i < sliceSize; i++) {
                    vol[(z * sliceSize) + i] = buffer2D[i];
                }

            } // for (z = 0; z < zDim; z++)

            grayImage2D.disposeLocal();
            grayImage2D = null;
            buffer2D = null;
        } // if (sliceHoleFilling)

        fireProgressStateChanged("Delete all but the largest object\n");
        Preferences.debug("Delete all but the largest object\n");
        idImage = new ModelImage(ModelStorageBase.USHORT, srcImage.getExtents(), srcImage.getImageName() + "_id");

        try {
            idImage.importData(0, vol, true);
        } catch (IOException e) {
            MipavUtil.displayError("IOException on idImage.importData");

            setCompleted(false);

            return;
        }

        kernel = AlgorithmMorphology3D.SIZED_SPHERE;
        sphereDiameter = 0.0f;
        method = AlgorithmMorphology3D.ID_OBJECTS;
        itersDilation = 0;
        itersErosion = 0;
        numPruningPixels = 0;
        edgingType = 0;
        idObjectsAlgo3D = new AlgorithmMorphology3D(idImage, kernel, sphereDiameter, method, itersDilation,
                                                    itersErosion, numPruningPixels, edgingType, wholeImage);
        idObjectsAlgo3D.run();
        idObjectsAlgo3D.finalize();
        idObjectsAlgo3D = null;

        idImage.calcMinMax();

        // ViewJFrameImage testFrame = new ViewJFrameImage(grayImage, null,
        // new Dimension(600, 300), srcImage.getUserInterface());
        numObjects = (int) idImage.getMax();
        idArray = new int[totLength];

        try {
            idImage.exportData(0, totLength, idArray);
        } catch (IOException e) {
            MipavUtil.displayError("IOException on idImage.exportData");

            setCompleted(false);

            return;
        }

        idImage.disposeLocal();
        idImage = null;

        idCount = new int[numObjects + 1];

        for (i = 0; i < totLength; i++) {
            idCount[idArray[i]]++;
        }

        maxCount = 0;
        maxID = -1;

        for (i = 1; i <= numObjects; i++) {

            if (idCount[i] > maxCount) {
                maxCount = idCount[i];
                maxID = i;
            }
        }

        // Delete all but the largest object
        for (i = 0; i < totLength; i++) {

            if (idArray[i] != maxID) {
                vol[i] = 0;
            }
        }

        idCount = null;
        idArray = null;

        /*boolean test;
         * test = true; if (test) { solidImage = new ModelImage(ModelStorageBase.BYTE, srcImage.getExtents(),
         *      "solidImage", srcImage.getUserInterface()); try {     solidImage.importData(0, vol, true); } catch
         * (IOException e) {     MipavUtil.displayError("IOException on soldImage.importData");     setCompleted(false);
         *     return; }
         *
         * new ViewJFrameImage(solidImage);  setCompleted(true); return;} // if (test)*/

        grayImage = new ModelImage(ModelStorageBase.BYTE, srcImage.getExtents(), srcImage.getImageName() + "_gray");

        for (i = 0; i < srcImage.getExtents()[2]; i++) {
            fileInfo = grayImage.getFileInfo()[i];
            fileInfo.setResolutions(srcImage.getFileInfo()[0].getResolutions());
            fileInfo.setUnitsOfMeasure(srcImage.getFileInfo()[0].getUnitsOfMeasure());
            grayImage.setFileInfo(fileInfo, i);
        } // for (i = 0; i < srcImage.getExtents()[2]; i++)

        try {
            grayImage.importData(0, vol, true);
        } catch (IOException error) {
            MipavUtil.displayError("IOException on grayImage.importData");

            setCompleted(false);

            return;
        }

        algoVOIExtraction = new AlgorithmVOIExtraction(grayImage);
        algoVOIExtraction.run();
        algoVOIExtraction.finalize();
        algoVOIExtraction = null;

        srcImage.setVOIs(grayImage.getVOIs());
        VOIs = grayImage.getVOIs();
        nVOIs = VOIs.size();
        shortMask = new short[totLength];

        for (i = 0; i < nVOIs; i++) {
            fireProgressStateChanged("Processing VOI " + (i + 1) + " of " + nVOIs);

            // fireProgressStateChanged(75 + 10 * (i + 1) / nVOIs);
            if (VOIs.VOIAt(i).getCurveType() == VOI.CONTOUR) {
                contours = VOIs.VOIAt(i).getCurves();

                for (j = 0; j < contours.length; j++) {
                    nContours = contours[j].size();

                    for (k = 0; k < nContours; k++) {
                        nPts = ((Vector) (contours[j].elementAt(k))).size();
                        xArr = new float[nPts];
                        yArr = new float[nPts];
                        zArr = new float[nPts];
                        ((VOIBase) (contours[j].elementAt(k))).exportArrays(xArr, yArr, zArr);

                        for (n = 0; n < nPts; n++) {
                            xLoc = Math.floor((multUp * xArr[n]) + 0.5);
                            yLoc = Math.floor((multUp * yArr[n]) + 0.5);
                            zLoc = Math.floor((multUp * zArr[n]) + 0.5);
                            blockLoc = numSites % BLOCKSIZE;

                            if (blockLoc == 0) {
                                siteBlocks[numBlocks++] = new double[BLOCKSIZE][3];
                            }

                            siteBlocks[numBlocks - 1][blockLoc][0] = xLoc;

                            if (xLoc < mins[0]) {
                                mins[0] = xLoc;
                            }

                            if (xLoc > maxs[0]) {
                                maxs[0] = xLoc;
                            }

                            siteBlocks[numBlocks - 1][blockLoc][1] = yLoc;

                            if (yLoc < mins[1]) {
                                mins[1] = yLoc;
                            }

                            if (yLoc > maxs[1]) {
                                maxs[1] = yLoc;
                            }

                            siteBlocks[numBlocks - 1][blockLoc][2] = zLoc;

                            if (zLoc < mins[2]) {
                                mins[2] = zLoc;
                            }

                            if (zLoc > maxs[2]) {
                                maxs[2] = zLoc;
                            }

                            numSites++;
                        } // for (n = 0; n < nPts; n++)
                    } // for (k = 0; k < nContours; k++)
                } // for (j = 0; j < contours.length; j++)
            } // if (VOIs.VOIAt(i).getCurveType() == VOI.CONTOUR)
        } // for (i = 0; i < nVOIs; i++)

        Preferences.debug("numSites = " + numSites + "\n");

        // Read bounding box
        Preferences.debug("Reading bounding box\n");
        omaxs[0] = maxs[0];
        omins[0] = mins[0];
        omaxs[1] = maxs[1];
        omins[1] = mins[1];
        omaxs[2] = maxs[2];
        omins[2] = mins[2];

        center[0] = (maxs[0] - mins[0]) / 2.0;
        center[1] = (maxs[1] - mins[1]) / 2.0;
        center[2] = (maxs[2] - mins[2]) / 2.0;

        if ((maxs[0] - mins[0]) > (maxs[1] - mins[1])) {

            if ((maxs[2] - mins[2]) > (maxs[0] - mins[0])) {
                width = maxs[2] - mins[2];
            } else {
                width = maxs[0] - mins[0];
            }
        } else {

            if ((maxs[1] - mins[1]) > (maxs[2] - mins[2])) {
                width = maxs[1] - mins[1];
            } else {
                width = maxs[2] - mins[2];
            }
        }

        width = width * 4;

        bound[0][0] = center[0] + width;
        bound[1][0] = bound[0][0];
        bound[2][0] = bound[0][0];
        bound[3][0] = bound[0][0];
        bound[0][1] = center[1] + width;
        bound[1][1] = bound[0][1];
        bound[4][1] = bound[0][1];
        bound[5][1] = bound[0][1];
        bound[0][2] = center[2] + width;
        bound[2][2] = bound[0][2];
        bound[4][2] = bound[0][2];
        bound[6][2] = bound[0][2];
        bound[4][0] = center[0] - width;
        bound[5][0] = bound[4][0];
        bound[6][0] = bound[4][0];
        bound[7][0] = bound[4][0];
        bound[2][1] = center[1] - width;
        bound[3][1] = bound[2][1];
        bound[6][1] = bound[2][1];
        bound[7][1] = bound[2][1];
        bound[1][2] = center[2] - width;
        bound[3][2] = bound[1][2];
        bound[5][2] = bound[1][2];
        bound[7][2] = bound[1][2];

        for (i = 0; i < 8; i++) {
            blockLoc = numSites % BLOCKSIZE;

            if (blockLoc == 0) {
                siteBlocks[numBlocks++] = new double[BLOCKSIZE][3];
            }

            siteBlocks[numBlocks - 1][blockLoc][0] = bound[i][0];
            siteBlocks[numBlocks - 1][blockLoc][1] = bound[i][1];
            siteBlocks[numBlocks - 1][blockLoc][2] = bound[i][2];
            numSites++;
        } // for (i = 0; i < 8; i++)

        maxs[0] = bound[0][0];
        mins[0] = bound[4][0];
        maxs[1] = bound[0][1];
        mins[1] = bound[2][1];
        maxs[2] = bound[0][2];
        mins[2] = bound[1][2];

        Preferences.debug("Shuffling\n");
        randomGen = new RandomNumberGen();
        makeShuffle();
        infinity[0] = Double.POSITIVE_INFINITY;
        basisSList = new BasisSList();
        simplexList = new SimplexList();
        pNeigh = new neighbor();
        ttBasis = new basisS();

        // basisSList.insertAtBack(ttBasis);
        ttBasis.setRefCount(1);
        ttBasis.setLScale(-1);
        ttBasis.setSqa(0.0);
        ttBasis.setSqb(0.0);
        fireProgressStateChanged("Compute DT of input point set");
        Preferences.debug("Compute DT of input point set\n");
        root = buildConvexHull();

        if (errorStatus == -1) {

            setCompleted(false);

            return;
        }

        fireProgressStateChanged("Find poles");
        Preferences.debug("Find poles\n");
        pole1 = new simplex[numSites];
        pole2 = new simplex[numSites];
        lfsLb = new double[numSites];

        Preferences.debug("Computing Voronoi vertices and 1st poles\n");

        time = System.currentTimeMillis() - time;
        Preferences.debug(" Level 1 min = " + ((float) time / 60000.0f) + "\n");
        srcImage.notifyImageDisplayListeners();

        setCompleted(true);

        return;
    } // runAlgorithm

    /**
     * DOCUMENT ME!
     *
     * @param  a  DOCUMENT ME!
     * @param  x  DOCUMENT ME!
     * @param  y  DOCUMENT ME!
     */
    private void aXPlusY(double a, double[] x, double[] y) {
        int i;

        for (i = 0; i < rDim; i++) {
            y[i] += a * x[i];
        }
    } // private void aXPlusY(double a, double x[], double y[])

    /**
     * DOCUMENT ME!
     *
     * @param  a  DOCUMENT ME!
     * @param  x  DOCUMENT ME!
     * @param  y  DOCUMENT ME!
     */
    private void aXPlusYTest(double a, double[] x, double[] y) {
        int i;

        for (i = 0; i < rDim; i++) {
            y[i] += a * x[i];
        }
    } // private void aXPlusYTest(double a, double x[], double y[])

    /**
     * DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    private simplex buildConvexHull() {
        simplex root = null;
        simplex s = null;
        neighbor neigh;
        basisS bas;
        int i;
        int currentNumSites = 0;
        int currentNumBlocks;
        int currentBlockLoc;
        cDim = 0;
        pDim = dim;
        vdCh = vd;

        if (vdCh) {
            rDim = pDim + 1;
        } else {
            rDim = pDim;
        }

        if (rDim > MAXDIM) {
            MipavUtil.displayError("Dimension bound MAXDIM exceeded rDim = " + rDim + " pDim = " + pDim);
            errorStatus = -1;

            return null;
        }

        if (vdCh || powerDiagram) {
            p[0] = Double.POSITIVE_INFINITY;
            vertIndex = Integer.MAX_VALUE;
            infinityBasis = new basisS();
            basisSList.insertAtBack(infinityBasis);
            infinityBasis.setRefCount(1);
            infinityBasis.setVecs((2 * rDim) - 1, 1);
            infinityBasis.setVecs(rDim - 1, 1);
            infinityBasis.setSqa(1.0);
            infinityBasis.setSqb(1.0);
        } // if (vdCh || powerDiagram)
        else if (shufMat[sNum] >= numSites) {
            sNum++;

            return null;
        } else {
            currentNumSites = shufMat[sNum++];
            currentNumBlocks = currentNumSites / BLOCKSIZE;
            currentBlockLoc = currentNumSites % BLOCKSIZE;
            p[0] = siteBlocks[currentNumBlocks][currentBlockLoc][0];
            p[1] = siteBlocks[currentNumBlocks][currentBlockLoc][1];
            p[2] = siteBlocks[currentNumBlocks][currentBlockLoc][2];
            vertIndex = currentNumSites;
        }

        root = new simplex();
        simplexList.insertAtBack(root);
        chRoot = root;

        s = new simplex();
        simplexList.insertAtBack(s);
        neigh = root.getPeak();

        if (neigh != null) {
            bas = neigh.getBasisS();

            if (bas != null) {
                bas.incRefCount();
            }
        } // if (neigh != null)

        for (i = 0; i < cDim; i++) {
            neigh = root.getNeigh(i);
            bas = neigh.getBasisS();

            if (bas != null) {
                bas.incRefCount();
            }
        }

        neigh = root.getPeak();

        if (neigh != null) {
            neigh.setVert(p);
            neigh.setVertIndex(vertIndex);
            neigh.setSimp(s);
        } // if (neigh != null)

        neigh = s.getPeak();

        if (neigh != null) {
            neigh.setSimp(root);
        } // if (neigh != null)

        buildHull(root);

        return root;
    } // private simplex buildConvexHull

    /**
     * DOCUMENT ME!
     *
     * @param  root  DOCUMENT ME!
     */
    private void buildHull(simplex root) {
        int loop1 = 1;
        int loop2 = 1;
        simplex s;
        simplex t;
        int[] cn = new int[1];

        while (cDim < rDim) {
            Preferences.debug("Number = " + loop1 + " on first while loop in buildHull\n");
            loop1++;
            p = getAnotherSite(cn);

            if (p == null) {
                return;
            }

            vertIndex = cn[0];
            Preferences.debug("Doing outOfFlat in buildHull\n");

            if (outOfFlat(root, p, vertIndex)) {
                Preferences.debug("Doing extendSimplices in buildHull\n");
                extendSimplices(root);
            } else {
                Preferences.debug("Doing first search in buildHull\n");
                s = search(root);

                if (errorStatus == -1) {
                    return;
                }

                Preferences.debug("Doing first makeFacets in buildHull\n");
                t = makeFacets(s);

                if (errorStatus == -1) {
                    return;
                }

                Preferences.debug("Doing first connect in buildHull\n");
                connect(t);
            }
        } // while (cDim < rDim)

        while ((p = getAnotherSite(cn)) != null) {
            Preferences.debug("Number = " + loop2 + " on second while loop in buildHull\n");
            loop2++;
            vertIndex = cn[0];
            Preferences.debug("Doing second search in buildHull\n");
            s = search(root);

            if (errorStatus == -1) {
                return;
            }

            Preferences.debug("Doing second makeFacets in buildHull\n");
            t = makeFacets(s);

            if (errorStatus == -1) {
                return;
            }

            Preferences.debug("Doing second connect in buildHull\n");
            connect(t);

            if (errorStatus == -1) {
                return;
            }
        }

        return;
    } // private void buildHull(simplex root)

    /**
     * DOCUMENT ME!
     *
     * @param   s  DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    private boolean checkPerps(simplex s) {
        double[] z;
        double[] y;
        double[] tt;
        int i;
        int j;
        int k;

        for (i = 1; i < cDim; i++) {

            if (Math.abs(s.getNeigh(i).getBasisS().getSqb()) < epsilon) {
                Preferences.debug("Math.abs(s.getNeigh(" + i + ").getBasisS().getSqb()) < epsilon in checkPerps\n");
                Preferences.debug("s.getNeigh(" + i + ").getBasisS().getSqb() = " + s.getNeigh(i).getBasisS().getSqb() +
                                  "\n");

                return false;
            }
        } // for (i = 1; i < cDim; i++)

        if (bCheckPerps == null) {
            bCheckPerps = new basisS();
            // basisSList.insertAtBack(bCheckPerps);
        } else {
            bCheckPerps.setLScale(0);
        }

        tt = s.getNeigh(0).getVert();

        for (i = 1; i < cDim; i++) {
            y = s.getNeigh(i).getVert();

            if ((vdCh || powerDiagram) && Double.isInfinite(y[0])) {
                bCheckPerps.setRefCount(infinityBasis.getRefCount());
                bCheckPerps.setLScale(infinityBasis.getLScale());
                bCheckPerps.setSqa(infinityBasis.getSqa());
                bCheckPerps.setSqb(infinityBasis.getSqb());
                bCheckPerps.setVecsCopy(infinityBasis.getVecs());
            } else {

                for (k = 0; k < pDim; k++) {
                    bCheckPerps.setVecs(k, y[k] - tt[k]);
                    bCheckPerps.setVecs(k + rDim, y[k] - tt[k]);
                }

                if (vdCh) {
                    z = bCheckPerps.getVecs();
                    z[(2 * rDim) - 1] = z[rDim - 1] = vecDotPDim(z, z) * Math.pow(2.0, -DELIFT);
                    bCheckPerps.setVecs(rDim - 1, z[rDim - 1]);
                    bCheckPerps.setVecs((2 * rDim) - 1, z[(2 * rDim) - 1]);
                }
            } // else

            if ((s.getNormal() != null) && (cosAngleSq(bCheckPerps, s.getNormal()) > bErrMinSq)) {
                Preferences.debug("Bad normal\n");
                Preferences.debug("i = " + i + "\n");
                printSimplexF(s);

                return false;
            } // if ((s.getNormal() != null) && (cosAngleSq(bCheckPerps, s.getNormal()) > bErrMinSq))

            for (j = i + 1; j < cDim; j++) {

                if (cosAngleSq(bCheckPerps, s.getNeigh(j).getBasisS()) > bErrMinSq) {
                    Preferences.debug("Bad basis\n");
                    Preferences.debug("i = " + i + " j = " + j + "\n");
                    printBasis(bCheckPerps);
                    printSimplexF(s);

                    return false;
                } // if (cosAngleSq(bCheckPerps, s.getNeigh(j).getBasisS()) > bErrMinSq)
            } // for (j = i+1; j < cDim; j++)
        } // for (i = 1; i < cDim; i++)

        return true;
    } // private boolean checkPerps(simplex s)

    /**
     * DOCUMENT ME!
     *
     * @param  s  DOCUMENT ME!
     */
    private void computeVV(simplex s) {

        /* Computes Voronoi vertices */
        double[][] v = new double[MAXDIM][3];
        int i;
        int j;
        int k;
        boolean inf = false;
        double[] cc = new double[3];
        double[] cond = new double[1];
        double[][] ta = new double[4][3];
        double slvNum;
        double sqRad;
        int vIndex;

        if (s == null) {
            return;
        }

        for (j = 0; j < cDim; j++) {
            v[j] = s.getNeigh(j).getVert();

            /* v[j] stores coordinates of j'th vertex of simplex s; j=0...3 */
            vIndex = s.getNeigh(j).getVertIndex();

            if (Double.isInfinite(v[j][0])) { /* Means simplex s is on the convex hull */
                inf = true;

                break; /* skip the rest of the for loop,
                        *ignore convex hull faces (= bounding box) */
            } // if (Double.isInfinite(v[j][0])

            i = siteNumm(v[j], vIndex); /* i is the index of the vertex v[j] */

            for (k = 0; k < (cDim - 1); k++) {
                ta[j][k] = v[j][k] / multUp; /* restore original coords */

                /* for inf = 0, ta[0], ta[1], ta[2], ta[3] are 4 vertices of s */
            } // for (k = 0; k < cDim-1; k++)
        } // for (j = 0; j < cDim; j++)

        if (!inf) { /* if not faces on convex hull, compute circumcenter */
            tetCircumcenter(ta[0], ta[1], ta[2], ta[3], cc, cond);

            /* cc is the displacement of circumcenter from ta[0] */
            /* cond is the denominator (orient3d) value */
            sqRad = (cc[0] * cc[0]) + (cc[1] * cc[1]) + (cc[2] * cc[2]);
            slvNum = (cond[0] * cond[0]) / (sqRad * sqRad * sqRad);

            if (cond[0] != 0) { /* ignore them if cond[0] = 0 */
                s.setVV(new double[3]);

                for (k = 0; k < (cDim - 1); k++) {
                    s.setVV(k, (ta[0][k] + cc[k]));
                }

                s.setStatus(VV);
            } // if (cond[0] != 0)
            else { /* if cond == 0, s is SLIVER */
                s.setVV(null);
                s.setStatus(SLV);
            } // else cond == 0
        } // if (!inf)
        else { /* if on convex hull */
            s.setStatus(CNV);
        } // else on convex hull

        /* Computing poles */
        for (j = 0; j < cDim; j++) { /* Compute 1st pole for vertex */
            i = siteNumm(s.getNeigh(j).getVert(), s.getNeigh(j).getVertIndex());

            if (i == -1) {
                continue;
            } // if (i == -1)

            /* Ignore poles that are too far away to matter - a relic of the
             *original California-style crust.  Probably no longer needed. */
            if ((s.getNeigh(j).getVert(0) > omaxs[0]) || (s.getNeigh(j).getVert(0) < omins[0]) ||
                    (s.getNeigh(j).getVert(1) > omaxs[1]) || (s.getNeigh(j).getVert(1) < omins[1]) ||
                    (s.getNeigh(j).getVert(2) > omaxs[2]) || (s.getNeigh(j).getVert(2) < omins[2])) {
                pole1[i] = null;

                continue;
            } else {

                if (pole1[i] == null) {

                    /* The vertex i is encountered for the 1st time */
                    if (s.getStatus() == VV) { /* We don't store infinite poles */
                        pole1[i] = s;

                        continue;
                    }
                } // if (pole1[i] == null)

                if ((s.getStatus() == VV) && (pole1[i].getStatus() == VV)) {

                    if (sqdist(pole1[i].getVV(), ta[j]) < sqdist(s.getVV(), ta[j])) {
                        pole1[i] = s; /* Upfdate 1st pole */
                    }
                }
            } // else
        } // for (j = 0; j < cDim; j++)

        return;
    } // private void computeVV(simplex s)

    /**
     * DOCUMENT ME!
     *
     * @param  s  DOCUMENT ME!
     */
    private void connect(simplex s) {

        /* make neighbor connections between newly created simplices incident to p */
        double[] xf = new double[3];
        double[] xb;
        double[] xfi;
        simplex sb;
        simplex sf;
        simplex seen;
        int i;
        neighbor sn;
        int bIndex;
        int fiIndex;
        int fIndex;
        neighbor neigh;

        if (s == null) {
            return;
        }

        if (s.getPeak().getVert() != null) {
            MipavUtil.displayError("s.getPeak().getVert() != null in connect");
            errorStatus = -1;

            return;
        }

        if (s.getPeak().getSimp().getPeak().getVertIndex() != vertIndex) {
            MipavUtil.displayError("s.getPeak().getSimp().getPeak().getVertIndex != vertIndex in connect");
            errorStatus = -1;

            return;
        }

        if (opVert(s, p, vertIndex).getSimp().getPeak().getVert() != null) {
            MipavUtil.displayError("opVert(s, p, vertIndex).getSimp().getPeak().getVert() != null) in connect");
            errorStatus = -1;

            return;
        }

        if (s.getVisit() == pNum) {
            return;
        }

        s.setVisit(pNum);
        seen = s.getPeak().getSimp();
        xfi = opSimp(seen, s).getVert();

        if (errorStatus == -1) {
            return;
        }

        fiIndex = opSimp(seen, s).getVertIndex();

        for (i = 0, sn = s.getNeigh(0); i < cDim; i++, sn = s.getNeigh(i)) {
            xb = sn.getVert();
            bIndex = sn.getVertIndex();

            if (vertIndex == bIndex) {
                continue;
            }

            sb = seen;
            sf = sn.getSimp();
            xf = xfi;
            fIndex = fiIndex;

            if (sf.getPeak().getVert() == null) { /* are we done already? */
                neigh = opVert(seen, xb, bIndex);

                if (errorStatus == -1) {
                    return;
                }

                if (neigh != null) {
                    sf = neigh.getSimp();
                } else {
                    sf = null;
                }

                if ((sf != null) && (sf.getPeak() != null) && (sf.getPeak().getVert() != null)) {
                    continue;
                }
            } else {

                do {
                    xb = xf;
                    bIndex = fIndex;
                    neigh = opSimp(sf, sb);

                    if (errorStatus == -1) {
                        return;
                    }

                    if (neigh != null) {
                        xf = neigh.getVert();
                        fIndex = neigh.getVertIndex();
                    } else {
                        xf = null;
                    }

                    sb = sf;
                    neigh = opVert(sb, xb, bIndex);

                    if (errorStatus == -1) {
                        return;
                    }

                    if (neigh != null) {
                        sf = neigh.getSimp();
                    } else {
                        sf = null;
                    }

                } while (sf.getPeak().getVert() != null);
            }

            sn.setSimp(sf);
            Preferences.debug("Doing neigh = opVert(sf, xf, fIndex) in connect\n");
            neigh = opVert(sf, xf, fIndex);

            if (errorStatus == -1) {
                return;
            }

            if (neigh != null) {
                neigh.setSimp(s);
            }

            Preferences.debug("Calling connect(sf) in connect\n");
            connect(sf);

            if (errorStatus == -1) {
                return;
            }
        } // for (i = 0, sn = s.getNeigh(0); i < cDim; i++, sn = s.getNeigh(i))

        return;
    } // private void connect(simplex s)

    /**
     * DOCUMENT ME!
     *
     * @param   v  DOCUMENT ME!
     * @param   w  DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    private double cosAngleSq(basisS v, basisS w) {
        double dd;
        double[] vv;
        double[] wv;

        vv = v.getVecs();
        wv = w.getVecs();
        dd = vecDot(vv, wv);

        return dd * dd / norm2(vv) / norm2(wv);
    } // private double cosAngleSq(basisS v, basisS w)

    /**
     * DOCUMENT ME!
     */
    private void createCylinder() {
        byte[] vol = new byte[250 * 250 * 250];
        int x, y, z;
        int sliceSize = 250 * 250;
        int[] extents = new int[3];
        extents[0] = 250;
        extents[1] = 250;
        extents[2] = 250;

        ModelImage cylImage;

        // Must leave first and last slices blank
        for (z = 1; z < 249; z++) {

            for (y = 0; y < 250; y++) {

                for (x = 0; x < 250; x++) {

                    if ((((x - 125) * (x - 125)) + ((y - 125) * (y - 125))) <= (50 * 50)) {
                        vol[(z * sliceSize) + (y * 250) + x] = 1;
                    }
                }
            }
        }

        cylImage = new ModelImage(ModelImage.BYTE, extents, "cylinder");

        try {
            cylImage.importData(0, vol, true);
        } catch (IOException error) {

            if (cylImage != null) {
                cylImage.disposeLocal();
                cylImage = null;
            }

            MipavUtil.displayError("Error on cylImage.importData");

            setCompleted(false);

            return;
        }

        try {
            cylImage.saveImage(srcImage.getFileInfo(0).getFileDirectory(), "cylinder", FileUtility.XML, true);
        } catch (OutOfMemoryError error) {

            if (cylImage != null) {
                cylImage.disposeLocal();
                cylImage = null;
            }

            MipavUtil.displayError("Error on cylImage.saveImage");

            setCompleted(false);

            return;
        }

        return;
    } // createCylinder

    /*****************************************************************************/
    /*                                                                           */
    /*  estimate()   Produce a one-word estimate of an expansion's value.        */
    /*                                                                           */
    /*  See either version of my paper for details.                              */
    /*                                                                           */
    /*****************************************************************************/

    private double estimate(int elen, double[][] e) {
        double Q;
        int eindex;

        Q = e[0][0];

        for (eindex = 1; eindex < elen; eindex++) {
            Q += e[eindex][0];
        }

        return Q;
    } // private double estimate(int elen, double e[])

    /**
     * DOCUMENT ME!
     *
     * @param   s  DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    private simplex extendSimplices(simplex s) {

        /* p lies outside flat containing previous sites;
         *make p a vertex of every current simplex, and create some new simplices */
        int i;
        int ocDim = cDim - 1;
        simplex ns = null;
        neighbor nsn;
        basisS bas;
        neighbor neigh;

        if (s.getVisit() == pNum) {

            if (s.getPeak().getVert() != null) {

                if (ocDim >= 0) {
                    return s.getNeigh(ocDim).getSimp();
                } else {
                    return s.getPeak().getSimp();
                }
            } else {
                return s;
            }
        } // if (s.getVisit() == pNum)

        s.setVisit(pNum);

        if (ocDim >= 0) {
            s.getNeigh(ocDim).setVert(p);
            s.getNeigh(ocDim).setVertIndex(vertIndex);
        } else {
            s.getPeak().setVert(p);
            s.getPeak().setVertIndex(vertIndex);
        }

        bas = s.getNormal();

        if (bas != null) {
            bas.decRefCount();

            if (bas.getRefCount() == 0) {
                basisSList.deleteNode(bas);
            }
            /*else {
             *  bas = null;}*/
        } // if (bas != null)

        bas = s.getNeigh(0).getBasisS();

        if (bas != null) {
            bas.decRefCount();

            if (bas.getRefCount() == 0) {
                basisSList.deleteNode(bas);
            }
            /*else {
             *  bas = null;}*/
        } // if (bas != null)

        if (s.getPeak().getVert() == null) {

            if (ocDim >= 0) {
                s.getNeigh(ocDim).setSimp(extendSimplices(s.getPeak().getSimp()));
            } else {
                s.getPeak().setSimp(extendSimplices(s.getPeak().getSimp()));
            }

            return s;
        } // if (s.getPeak().getVert() == null)
        else {
            ns = new simplex();
            simplexList.insertAtBack(ns);
            ns.setSimplexCopy(s);
            neigh = s.getPeak();
            bas = neigh.getBasisS();

            if (bas != null) {
                bas.incRefCount();
            }

            for (i = 0; i < cDim; i++) {
                neigh = s.getNeigh(i);
                bas = neigh.getBasisS();

                if (bas != null) {
                    bas.incRefCount();
                }
            } // for (i = 0; i < cDim; i++)

            if (ocDim >= 0) {
                s.getNeigh(ocDim).setSimp(ns);
            } else {
                s.getPeak().setSimp(ns);
            }

            ns.getPeak().setVert(null);
            ns.getPeak().setVertIndex(Integer.MIN_VALUE);
            ns.getPeak().setSimp(s);

            if (ocDim >= 0) {
                ns.setNeigh(ocDim, s.getPeak());
            } else {
                ns.setPeak(s.getPeak());
            }

            if (s.getPeak().getBasisS() != null) {
                s.getPeak().getBasisS().incRefCount();
            }

            for (i = 0; i < cDim; i++) {
                nsn = ns.getNeigh(i);
                nsn.setSimp(extendSimplices(nsn.getSimp()));
            }
        } // else

        return ns;
    }

    /*****************************************************************************/
    /*                                                                           */
    /*  fastExpansionSumZeroElim()   Sum two expansions, eliminating zero     */
    /*                                  components from the output expansion.    */
    /*                                                                           */
    /*  Sets h = e + f.  See the long version of my paper for details.           */
    /*                                                                           */
    /*  If round-to-even is used (as with IEEE 754), maintains the strongly      */
    /*  nonoverlapping property.  (That is, if e is strongly nonoverlapping, h   */
    /*  will be also.)  Does NOT maintain the nonoverlapping or nonadjacent      */
    /*  properties.                                                              */
    /*                                                                           */
    /*****************************************************************************/

    private int fastExpansionSumZeroElim(int elen, double[][] e, int flen, double[][] f, double[][] h) { /* h cannot be e or f. */

        double Q;
        double[] Qnew = new double[1];
        double[] hh = new double[1];
        int eindex, findex, hindex;
        double enow, fnow;

        enow = e[0][0];
        fnow = f[0][0];
        eindex = findex = 0;

        if ((fnow > enow) == (fnow > -enow)) {
            Q = enow;
            enow = e[++eindex][0];
        } else {
            Q = fnow;
            fnow = f[++findex][0];
        }

        hindex = 0;

        if ((eindex < elen) && (findex < flen)) {

            if ((fnow > enow) == (fnow > -enow)) {
                fastTwoSum(enow, Q, Qnew, hh);
                enow = e[++eindex][0];
            } else {
                fastTwoSum(fnow, Q, Qnew, hh);
                fnow = f[++findex][0];
            }

            Q = Qnew[0];

            if (hh[0] != 0.0) {
                h[hindex++][0] = hh[0];
            }

            while ((eindex < elen) && (findex < flen)) {

                if ((fnow > enow) == (fnow > -enow)) {
                    twoSum(Q, enow, Qnew, hh);
                    enow = e[++eindex][0];
                } else {
                    twoSum(Q, fnow, Qnew, hh);
                    fnow = f[++findex][0];
                }

                Q = Qnew[0];

                if (hh[0] != 0.0) {
                    h[hindex++][0] = hh[0];
                }
            }
        }

        while (eindex < elen) {
            twoSum(Q, enow, Qnew, hh);
            enow = e[++eindex][0];
            Q = Qnew[0];

            if (hh[0] != 0.0) {
                h[hindex++][0] = hh[0];
            }
        }

        while (findex < flen) {
            twoSum(Q, fnow, Qnew, hh);
            fnow = f[++findex][0];
            Q = Qnew[0];

            if (hh[0] != 0.0) {
                h[hindex++][0] = hh[0];
            }
        }

        if ((Q != 0.0) || (hindex == 0)) {
            h[hindex++][0] = Q;
        }

        return hindex;
    } // private int fastExpansionSumZeroElim

    /**
     * DOCUMENT ME!
     *
     * @param  a  DOCUMENT ME!
     * @param  b  DOCUMENT ME!
     * @param  x  DOCUMENT ME!
     * @param  y  DOCUMENT ME!
     */
    private void fastTwoSum(double a, double b, double[] x, double[] y) {
        x[0] = a + b;
        fastTwoSumTail(a, b, x[0], y);

        return;
    } // private void fastTwoSum(double a, double b, double x[], double y[])

    /**
     * DOCUMENT ME!
     *
     * @param  a  DOCUMENT ME!
     * @param  b  DOCUMENT ME!
     * @param  x  DOCUMENT ME!
     * @param  y  DOCUMENT ME!
     */
    private void fastTwoSumTail(double a, double b, double x, double[] y) {
        double bvirt;
        bvirt = x - a;
        y[0] = b - bvirt;

        return;
    } // private void fastTwoSumTail(double a, double b, double x, double y[])

    /**
     * DOCUMENT ME!
     *
     * @param   cn  DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    private double[] getAnotherSite(int[] cn) {
        double[] pNext = new double[3];
        int currentNumSites;
        int currentNumBlocks;
        int currentBlockLoc;

        if (shufMat[sNum] >= numSites) {
            sNum++;

            return null;
        } else {
            currentNumSites = shufMat[sNum++];
            currentNumBlocks = currentNumSites / BLOCKSIZE;
            currentBlockLoc = currentNumSites % BLOCKSIZE;
            pNext[0] = siteBlocks[currentNumBlocks][currentBlockLoc][0];
            pNext[1] = siteBlocks[currentNumBlocks][currentBlockLoc][1];
            pNext[2] = siteBlocks[currentNumBlocks][currentBlockLoc][2];
            cn[0] = currentNumSites;
        }

        pNum = siteNumm(pNext, currentNumSites) + 2;

        return pNext;
    } // private double[] getAnotherSite()

    /**
     * DOCUMENT ME!
     *
     * @param  s  DOCUMENT ME!
     */
    private void getBasisSede(simplex s) {
        int k = 1;
        neighbor sn = s.getNeigh(1);
        neighbor sn0 = s.getNeigh(0);
        neighbor t;
        basisS bas;
        Preferences.debug("s.getNeigh(0).getVert(0) = " + s.getNeigh(0).getVert(0) + " at getBasisSede entry\n");

        if (s.getNeigh(1).getVert() != null) {
            Preferences.debug("s.getNeigh(1).getVert(0) = " + s.getNeigh(1).getVert(0) + " at getBasisSede entry\n");
        }

        if ((vdCh || powerDiagram) && (sn0 != null) && (sn0.getVert() != null) && (Double.isInfinite(sn0.getVert(0))) &&
                (cDim > 1)) {
            t = sn0;
            sn0 = sn;
            s.setNeigh(0, sn0);
            sn = t;
            s.setNeigh(1, sn);
            bas = sn0.getBasisS();

            if (bas != null) {
                bas.decRefCount();

                if (bas.getRefCount() == 0) {
                    basisSList.deleteNode(bas);
                }
                /*else {
                 *  bas = null;}*/
            } // if (bas != null)

            sn0.setBasisS(ttBasis);
            ttBasis.incRefCount();
        } // if ((vdCh || powerDiagram) && (sn0 != null) && (sn0.getVert() != null) &&
        else {

            if (sn0 != null) {

                if (sn0.getBasisS() == null) {
                    sn0.setBasisS(ttBasis);
                    ttBasis.incRefCount();
                }
            } // if (sn0 != null)
            else {

                while ((k < cDim) && (sn.getBasisS() != null)) {
                    k++;
                    sn = s.getNeigh(k);
                } // else while (k < cDim && (sn.getBasisS() != null))
            }
        } // else

        while (k < cDim) {

            if (sn != null) {
                bas = sn.getBasisS();

                if (bas != null) {
                    bas.decRefCount();

                    if (bas.getRefCount() == 0) {
                        basisSList.deleteNode(bas);
                    }
                    /*else {
                     *  bas = null;}*/
                } // if (bas != null)

                Preferences.debug("Doing reduce in getBasisSede\n");
                reduce(sn.getBasisS(), 3, sn.getVert(), s, k, sn);
            } // if (sn != null)

            k++;
            sn = s.getNeigh(k);
        } // while (k < cDim)

        return;
    } // private void getBasisSede(simplex s)

    /**
     * DOCUMENT ME!
     *
     * @param  s  DOCUMENT ME!
     */
    private void getNormalSede(simplex s) {
        neighbor rn;
        int i;
        int j;
        double[] a;
        double[] b;
        double[] c;

        getBasisSede(s);

        if ((rDim == 3) && (cDim == 3)) {
            a = s.getNeigh(1).getBasisS().getVecs();
            b = s.getNeigh(2).getBasisS().getVecs();
            s.setNormal(new basisS());
            basisSList.insertAtBack(s.getNormal());
            s.getNormal().setRefCount(1);

            s.getNormal().setVecs(0, (a[1] * b[2]) - (a[2] * b[1]));
            s.getNormal().setVecs(1, (a[2] * b[0]) - (a[0] * b[2]));
            s.getNormal().setVecs(2, (a[0] * b[1]) - (a[1] * b[0]));
            c = s.getNormal().getVecs();
            s.getNormal().setSqb(norm2(c));

            for (i = cDim + 1; i > 0; i--) {

                if (i > 1) {
                    rn = chRoot.getNeigh(i - 2);
                } else {
                    rn = chRoot.getPeak();
                }

                for (j = 0; (j < cDim) && (rn.getVertIndex() != s.getNeigh(j).getVertIndex()); j++) {
                    ;
                }

                if (j < cDim) {
                    continue;
                }

                if (Double.isInfinite(rn.getVert()[0])) {

                    if (s.getNormal().getVecs()[2] > -bErrMin) {
                        continue;
                    }
                } // if (Double.isInfinite(rn.getVert()[0]))
                else if (!sees(rn.getVert(), rn.getVertIndex(), s)) {

                    if (errorStatus == -1) {
                        return;
                    }

                    continue;
                }

                c = s.getNormal().getVecs();
                c[0] = -c[0];
                c[1] = -c[1];
                c[2] = -c[2];
                s.getNormal().setVecs(0, c[0]);
                s.getNormal().setVecs(1, c[1]);
                s.getNormal().setVecs(2, c[2]);

                break;
            } // for (i = cDim+1; i > 0; i--)

            if (!checkPerps(s)) {
                MipavUtil.displayError("First checkPerps failed in getNormalSede");
                errorStatus = -1;

                return;
            }
        } // if (rDim == 3 && cDim == 3)

        for (i = cDim + 1; i > 0; i--) {

            if (i > 1) {
                rn = chRoot.getNeigh(i - 2);
            } else {
                rn = chRoot.getPeak();
            }

            for (j = 0; (j < cDim) && (rn.getVertIndex() != s.getNeigh(j).getVertIndex()); j++) {
                ;
            }

            if (j < cDim) {
                continue;
            }

            Preferences.debug("Doing reduce in getNormalSede\n");
            reduce(s.getNormal(), 1, rn.getVert(), s, cDim, null);

            if (s.getNormal().getSqb() != 0.0) {
                break;
            }
        } // for (i = cDim+1; i > 0; i--)

        if (!checkPerps(s)) {
            MipavUtil.displayError("Second checkPerps failed in getNormalSede");
            errorStatus = -1;

            return;
        }
    } // private void getNormalSede(simplex s)

    /**
     * DOCUMENT ME!
     *
     * @param   x  DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    private double logb(double x) {

        if (x <= 0) {
            return -Double.MAX_VALUE;
        }

        return Math.log(x) / Math.log(2);
    } // private double logb(double x)

    /**
     * DOCUMENT ME!
     *
     * @param   seen  DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    private simplex makeFacets(simplex seen) {

        /* Visit simplices with sees(p,s), and make a facet for every neighbor
         *of s not seen by p */
        simplex n;
        neighbor bn;
        int i;
        int j;
        neighbor neigh;
        basisS bas;

        if (seen == null) {
            return null;
        }

        if (!sees(p, vertIndex, seen)) {
            MipavUtil.displayError("sees returned false in makeFacets");
            errorStatus = -1;

            return null;
        }

        if (seen.getPeak().getVert() != null) {
            MipavUtil.displayError("seen.getPeak().getVert() != null in makeFacets");
            errorStatus = -1;

            return null;
        }

        seen.getPeak().setVert(p);
        seen.getPeak().setVertIndex(vertIndex);

        for (i = 0; i < cDim; i++) {
            bn = seen.getNeigh(i);
            n = bn.getSimp();

            if (pNum != n.getVisit()) {
                n.setVisit(pNum);

                if (sees(p, vertIndex, n)) {
                    Preferences.debug("Calling makeFacets(n) in makeFacets\n");
                    makeFacets(n);
                }

                if (errorStatus == -1) {
                    return null;
                }
            } // if (pNum != n.getVisit())

            if (n.getPeak().getVert() != null) {
                continue;
            }

            ns = new simplex();
            simplexList.insertAtBack(ns);
            ns.setSimplexCopy(seen);
            neigh = seen.getPeak();
            bas = neigh.getBasisS();

            if (bas != null) {
                bas.incRefCount();
            }

            for (j = 0; j < cDim; j++) {
                neigh = seen.getNeigh(j);
                bas = neigh.getBasisS();

                if (bas != null) {
                    bas.incRefCount();
                }
            }

            ns.setVisit(0);
            ns.getPeak().setVert(null);
            ns.getPeak().setVertIndex(Integer.MIN_VALUE);
            ns.setNormal(null);
            ns.getPeak().setSimp(seen);
            bas = ns.getNeigh(i).getBasisS();

            if (bas != null) {
                bas.decRefCount();

                if (bas.getRefCount() == 0) {
                    basisSList.deleteNode(bas);
                }
                /*else {
                 *  bas = null;}*/
            } // if (bas != null)

            ns.getNeigh(i).setVert(p);
            ns.getNeigh(i).setVertIndex(vertIndex);
            Preferences.debug("Doing neigh = opSimp(n,seen) in makeFacets\n");
            neigh = opSimp(n, seen);

            if (errorStatus == -1) {
                return null;
            }

            if (neigh != null) {
                neigh.setSimp(ns);
            }

            bn.setSimp(ns);
        } // for (i = 0; i < cDim; i++)

        return ns;
    } // private simplex makeFacets(simplex seen)

    /**
     * DOCUMENT ME!
     */
    private void makeShuffle() {
        int i, t, j;

        if (matSize <= numSites) {
            matSize = numSites + 1;
            shufMat = new int[matSize];
        }

        for (i = 0; i <= numSites; i++) {
            shufMat[i] = i;
        }

        for (i = 0; i < numSites; i++) {
            t = shufMat[i];
            j = (int) (i + ((numSites - i) * randomGen.genUniformRandomNum(0.0, 1.0)));
            shufMat[i] = shufMat[j];
            shufMat[j] = t;
        }
    } // makeShuffle

    /**
     * DOCUMENT ME!
     *
     * @param   x  DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    private double norm2(double[] x) {
        int i;
        double sum = 0.0;

        for (i = 0; i < rDim; i++) {
            sum += x[i] * x[i];
        }

        return sum;
    }

    /**
     * DOCUMENT ME!
     *
     * @param   a  DOCUMENT ME!
     * @param   b  DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    private neighbor opSimp(simplex a, simplex b) {

        /* The neighbor entry of a containing b */
        int i;
        neighbor x;

        for (i = 0, x = a.getNeigh(0); (((x == null) || (x.getSimp() != b)) && (i < cDim)); i++, x = a.getNeigh(i)) {
            ;
        }

        if (i < cDim) {
            return x;
        } else {
            Preferences.debug("Adjacency failure in opSimp\n");
            MipavUtil.displayError("Adjacency failure in opSimp");

            if (b == null) {
                Preferences.debug("b is null in opSimp\n");
            }

            for (i = 0; i < cDim; i++) {

                if (a.getNeigh(i) == null) {
                    Preferences.debug("a.getNeigh(" + i + ") == null in opSimp\n");
                } else if (a.getNeigh(i).getSimp() == null) {
                    Preferences.debug("a.getNeigh(" + i + ").getSimp() == null in opSimp\n");
                }
            } // for (i = 0; i < cDim; i++)

            printSimplexF(a);
            printSimplexF(b);
            visitTriangGen(a);
            errorStatus = -1;

            return null;
        }
    } // private neighbor opSimp(simplex a, simplex b)

    /**
     * DOCUMENT ME!
     *
     * @param   a          DOCUMENT ME!
     * @param   b          DOCUMENT ME!
     * @param   vertIndex  DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    private neighbor opVert(simplex a, double[] b, int vertIndex) {

        /* The neighbor entry of a containing b */
        int i;
        int j;
        neighbor x;

        for (i = 0, x = a.getNeigh(0); (((x == null) || (x.getVertIndex() != vertIndex)) && (i < cDim));
                 i++, x = a.getNeigh(i)) {
            ;
        }

        if (i < cDim) {
            return x;
        } else {
            Preferences.debug("Adjacency failure in opVert\n");
            MipavUtil.displayError("Adjacency failure in opVert");

            for (i = 0; i < cDim; i++) {

                if (a.getNeigh(i) == null) {
                    Preferences.debug("a.getNeigh(" + i + ") == null in opVert\n");
                } else if (a.getNeigh(i).getVert() == null) {
                    Preferences.debug("a.getNeigh(" + i + ").getVert() == null in opVert\n");
                }
            } // for (i = 0; i < cDim; i++)

            if (b == null) {
                Preferences.debug("b is null in opVert\n");
            } else {

                for (j = 0; j < pDim; j++) {
                    Preferences.debug("b[" + j + "] = " + b[j] + "\n");
                }
            }

            printSimplexF(a);
            visitTriangGen(a);
            errorStatus = -1;

            return null;
        }
    } // private neighbor opVert(simplex a, double b[], int vertIndex)

    /**
     * DOCUMENT ME!
     *
     * @param   pa  DOCUMENT ME!
     * @param   pb  DOCUMENT ME!
     * @param   pc  DOCUMENT ME!
     * @param   pd  DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    private double orient3d(double[] pa, double[] pb, double[] pc, double[] pd) {
        double adx, bdx, cdx, ady, bdy, cdy, adz, bdz, cdz;
        double bdxcdy, cdxbdy, cdxady, adxcdy, adxbdy, bdxady;
        double det;
        double permanent, errBound;

        adx = pa[0] - pd[0];
        bdx = pb[0] - pd[0];
        cdx = pc[0] - pd[0];
        ady = pa[1] - pd[1];
        bdy = pb[1] - pd[1];
        cdy = pc[1] - pd[1];
        adz = pa[2] - pd[2];
        bdz = pb[2] - pd[2];
        cdz = pc[2] - pd[2];

        bdxcdy = bdx * cdy;
        cdxbdy = cdx * bdy;

        cdxady = cdx * ady;
        adxcdy = adx * cdy;

        adxbdy = adx * bdy;
        bdxady = bdx * ady;

        det = (adz * (bdxcdy - cdxbdy)) + (bdz * (cdxady - adxcdy)) + (cdz * (adxbdy - bdxady));

        permanent = ((Math.abs(bdxcdy) + Math.abs(cdxbdy)) * Math.abs(adz)) +
                    ((Math.abs(cdxady) + Math.abs(adxcdy)) * Math.abs(bdz)) +
                    ((Math.abs(adxbdy) + Math.abs(bdxady)) * Math.abs(cdz));
        errBound = o3dErrBoundA * permanent;

        if ((det > errBound) || (-det > errBound)) {
            return det;
        }

        return orient3dAdapt(pa, pb, pc, pd, permanent);
    } // private double orient3d

    /**
     * DOCUMENT ME!
     *
     * @param   pa         DOCUMENT ME!
     * @param   pb         DOCUMENT ME!
     * @param   pc         DOCUMENT ME!
     * @param   pd         DOCUMENT ME!
     * @param   permanent  DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    private double orient3dAdapt(double[] pa, double[] pb, double[] pc, double[] pd, double permanent) {
        double adx, bdx, cdx, ady, bdy, cdy;
        double[] adz = new double[1];
        double[] bdz = new double[1];
        double[] cdz = new double[1];
        double det, errBound;

        double[] bdxcdy1 = new double[1];
        double[] cdxbdy1 = new double[1];
        double[] cdxady1 = new double[1];
        double[] adxcdy1 = new double[1];
        double[] adxbdy1 = new double[1];
        double[] bdxady1 = new double[1];
        double[] bdxcdy0 = new double[1];
        double[] cdxbdy0 = new double[1];
        double[] cdxady0 = new double[1];
        double[] adxcdy0 = new double[1];
        double[] adxbdy0 = new double[1];
        double[] bdxady0 = new double[1];
        double[][] bc = new double[4][1];
        double[][] ca = new double[4][1];
        double[][] ab = new double[4][1];
        double[] bc3 = new double[1];
        double[] ca3 = new double[1];
        double[] ab3 = new double[1];
        double[][] adet = new double[8][1];
        double[][] bdet = new double[8][1];
        double[][] cdet = new double[8][1];
        int alen, blen, clen;
        double[][] abdet = new double[16][1];
        int ablen;
        double[][] finnow;
        double[][] finother;
        double[][] finswap;
        double[][] fin1 = new double[192][1];
        double[][] fin2 = new double[192][1];
        int finlength;

        double[] adxtail = new double[1];
        double[] bdxtail = new double[1];
        double[] cdxtail = new double[1];
        double[] adytail = new double[1];
        double[] bdytail = new double[1];
        double[] cdytail = new double[1];
        double[] adztail = new double[1];
        double[] bdztail = new double[1];
        double[] cdztail = new double[1];
        double[] at_blarge = new double[1];
        double[] at_clarge = new double[1];
        double[] bt_clarge = new double[1];
        double[] bt_alarge = new double[1];
        double[] ct_alarge = new double[1];
        double[] ct_blarge = new double[1];
        double[][] at_b = new double[4][1];
        double[][] at_c = new double[4][1];
        double[][] bt_c = new double[4][1];
        double[][] bt_a = new double[4][1];
        double[][] ct_a = new double[4][1];
        double[][] ct_b = new double[4][1];
        int at_blen, at_clen, bt_clen, bt_alen, ct_alen, ct_blen;
        double[] bdxt_cdy1 = new double[1];
        double[] cdxt_bdy1 = new double[1];
        double[] cdxt_ady1 = new double[1];
        double[] adxt_cdy1 = new double[1];
        double[] adxt_bdy1 = new double[1];
        double[] bdxt_ady1 = new double[1];
        double[] bdxt_cdy0 = new double[1];
        double[] cdxt_bdy0 = new double[1];
        double[] cdxt_ady0 = new double[1];
        double[] adxt_cdy0 = new double[1];
        double[] adxt_bdy0 = new double[1];
        double[] bdxt_ady0 = new double[1];
        double[] bdyt_cdx1 = new double[1];
        double[] cdyt_bdx1 = new double[1];
        double[] cdyt_adx1 = new double[1];
        double[] adyt_cdx1 = new double[1];
        double[] adyt_bdx1 = new double[1];
        double[] bdyt_adx1 = new double[1];
        double[] bdyt_cdx0 = new double[1];
        double[] cdyt_bdx0 = new double[1];
        double[] cdyt_adx0 = new double[1];
        double[] adyt_cdx0 = new double[1];
        double[] adyt_bdx0 = new double[1];
        double[] bdyt_adx0 = new double[1];
        double[][] bct = new double[8][1];
        double[][] cat = new double[8][1];
        double[][] abt = new double[8][1];
        int bctlen, catlen, abtlen;
        double[] bdxt_cdyt1 = new double[1];
        double[] cdxt_bdyt1 = new double[1];
        double[] cdxt_adyt1 = new double[1];
        double[] adxt_cdyt1 = new double[1];
        double[] adxt_bdyt1 = new double[1];
        double[] bdxt_adyt1 = new double[1];
        double[] bdxt_cdyt0 = new double[1];
        double[] cdxt_bdyt0 = new double[1];
        double[] cdxt_adyt0 = new double[1];
        double[] adxt_cdyt0 = new double[1];
        double[] adxt_bdyt0 = new double[1];
        double[] bdxt_adyt0 = new double[1];
        double[][] u = new double[4][1];
        double[][] v = new double[12][1];
        double[][] w = new double[16][1];
        double[] u3 = new double[1];
        int vlength, wlength;
        double negate;

        adx = pa[0] - pd[0];
        bdx = pb[0] - pd[0];
        cdx = pc[0] - pd[0];
        ady = pa[1] - pd[1];
        bdy = pb[1] - pd[1];
        cdy = pc[1] - pd[1];
        adz[0] = pa[2] - pd[2];
        bdz[0] = pb[2] - pd[2];
        cdz[0] = pc[2] - pd[2];

        twoProduct(bdx, cdy, bdxcdy1, bdxcdy0);
        twoProduct(cdx, bdy, cdxbdy1, cdxbdy0);
        twoTwoDiff(bdxcdy1[0], bdxcdy0[0], cdxbdy1[0], cdxbdy0[0], bc3, bc[2], bc[1], bc[0]);
        bc[3][0] = bc3[0];
        alen = scaleExpansionZeroElim(4, bc, adz, adet);

        twoProduct(cdx, ady, cdxady1, cdxady0);
        twoProduct(adx, cdy, adxcdy1, adxcdy0);
        twoTwoDiff(cdxady1[0], cdxady0[0], adxcdy1[0], adxcdy0[0], ca3, ca[2], ca[1], ca[0]);
        ca[3][0] = ca3[0];
        blen = scaleExpansionZeroElim(4, ca, bdz, bdet);

        twoProduct(adx, bdy, adxbdy1, adxbdy0);
        twoProduct(bdx, ady, bdxady1, bdxady0);
        twoTwoDiff(adxbdy1[0], adxbdy0[0], bdxady1[0], bdxady0[0], ab3, ab[2], ab[1], ab[0]);
        ab[3][0] = ab3[0];
        clen = scaleExpansionZeroElim(4, ab, cdz, cdet);

        ablen = fastExpansionSumZeroElim(alen, adet, blen, bdet, abdet);
        finlength = fastExpansionSumZeroElim(ablen, abdet, clen, cdet, fin1);

        det = estimate(finlength, fin1);
        errBound = o3dErrBoundB * permanent;

        if ((det >= errBound) || (-det >= errBound)) {
            return det;
        }

        twoDiffTail(pa[0], pd[0], adx, adxtail);
        twoDiffTail(pb[0], pd[0], bdx, bdxtail);
        twoDiffTail(pc[0], pd[0], cdx, cdxtail);
        twoDiffTail(pa[1], pd[1], ady, adytail);
        twoDiffTail(pb[1], pd[1], bdy, bdytail);
        twoDiffTail(pc[1], pd[1], cdy, cdytail);
        twoDiffTail(pa[2], pd[2], adz[0], adztail);
        twoDiffTail(pb[2], pd[2], bdz[0], bdztail);
        twoDiffTail(pc[2], pd[2], cdz[0], cdztail);

        if ((adxtail[0] == 0.0) && (bdxtail[0] == 0.0) && (cdxtail[0] == 0.0) && (adytail[0] == 0.0) &&
                (bdytail[0] == 0.0) && (cdytail[0] == 0.0) && (adztail[0] == 0.0) && (bdztail[0] == 0.0) &&
                (cdztail[0] == 0.0)) {
            return det;
        }

        errBound = (o3dErrBoundC * permanent) + (resultErrBound * Math.abs(det));
        det += ((adz[0] * (((bdx * cdytail[0]) + (cdy * bdxtail[0])) - ((bdy * cdxtail[0]) + (cdx * bdytail[0])))) +
                (adztail[0] * ((bdx * cdy) - (bdy * cdx)))) +
               ((bdz[0] * (((cdx * adytail[0]) + (ady * cdxtail[0])) - ((cdy * adxtail[0]) + (adx * cdytail[0])))) +
                (bdztail[0] * ((cdx * ady) - (cdy * adx)))) +
               ((cdz[0] * (((adx * bdytail[0]) + (bdy * adxtail[0])) - ((ady * bdxtail[0]) + (bdx * adytail[0])))) +
                (cdztail[0] * ((adx * bdy) - (ady * bdx))));

        if ((det >= errBound) || (-det >= errBound)) {
            return det;
        }

        finnow = fin1;
        finother = fin2;

        if (adxtail[0] == 0.0) {

            if (adytail[0] == 0.0) {
                at_b[0][0] = 0.0;
                at_blen = 1;
                at_c[0][0] = 0.0;
                at_clen = 1;
            } else {
                negate = -adytail[0];
                twoProduct(negate, bdx, at_blarge, at_b[0]);
                at_b[1][0] = at_blarge[0];
                at_blen = 2;
                twoProduct(adytail[0], cdx, at_clarge, at_c[0]);
                at_c[1][0] = at_clarge[0];
                at_clen = 2;
            }
        } else {

            if (adytail[0] == 0.0) {
                twoProduct(adxtail[0], bdy, at_blarge, at_b[0]);
                at_b[1][0] = at_blarge[0];
                at_blen = 2;
                negate = -adxtail[0];
                twoProduct(negate, cdy, at_clarge, at_c[0]);
                at_c[1][0] = at_clarge[0];
                at_clen = 2;
            } else {
                twoProduct(adxtail[0], bdy, adxt_bdy1, adxt_bdy0);
                twoProduct(adytail[0], bdx, adyt_bdx1, adyt_bdx0);
                twoTwoDiff(adxt_bdy1[0], adxt_bdy0[0], adyt_bdx1[0], adyt_bdx0[0], at_blarge, at_b[2], at_b[1],
                           at_b[0]);
                at_b[3][0] = at_blarge[0];
                at_blen = 4;
                twoProduct(adytail[0], cdx, adyt_cdx1, adyt_cdx0);
                twoProduct(adxtail[0], cdy, adxt_cdy1, adxt_cdy0);
                twoTwoDiff(adyt_cdx1[0], adyt_cdx0[0], adxt_cdy1[0], adxt_cdy0[0], at_clarge, at_c[2], at_c[1],
                           at_c[0]);
                at_c[3][0] = at_clarge[0];
                at_clen = 4;
            }
        }

        if (bdxtail[0] == 0.0) {

            if (bdytail[0] == 0.0) {
                bt_c[0][0] = 0.0;
                bt_clen = 1;
                bt_a[0][0] = 0.0;
                bt_alen = 1;
            } else {
                negate = -bdytail[0];
                twoProduct(negate, cdx, bt_clarge, bt_c[0]);
                bt_c[1][0] = bt_clarge[0];
                bt_clen = 2;
                twoProduct(bdytail[0], adx, bt_alarge, bt_a[0]);
                bt_a[1][0] = bt_alarge[0];
                bt_alen = 2;
            }
        } else {

            if (bdytail[0] == 0.0) {
                twoProduct(bdxtail[0], cdy, bt_clarge, bt_c[0]);
                bt_c[1][0] = bt_clarge[0];
                bt_clen = 2;
                negate = -bdxtail[0];
                twoProduct(negate, ady, bt_alarge, bt_a[0]);
                bt_a[1][0] = bt_alarge[0];
                bt_alen = 2;
            } else {
                twoProduct(bdxtail[0], cdy, bdxt_cdy1, bdxt_cdy0);
                twoProduct(bdytail[0], cdx, bdyt_cdx1, bdyt_cdx0);
                twoTwoDiff(bdxt_cdy1[0], bdxt_cdy0[0], bdyt_cdx1[0], bdyt_cdx0[0], bt_clarge, bt_c[2], bt_c[1],
                           bt_c[0]);
                bt_c[3][0] = bt_clarge[0];
                bt_clen = 4;
                twoProduct(bdytail[0], adx, bdyt_adx1, bdyt_adx0);
                twoProduct(bdxtail[0], ady, bdxt_ady1, bdxt_ady0);
                twoTwoDiff(bdyt_adx1[0], bdyt_adx0[0], bdxt_ady1[0], bdxt_ady0[0], bt_alarge, bt_a[2], bt_a[1],
                           bt_a[0]);
                bt_a[3][0] = bt_alarge[0];
                bt_alen = 4;
            }
        }

        if (cdxtail[0] == 0.0) {

            if (cdytail[0] == 0.0) {
                ct_a[0][0] = 0.0;
                ct_alen = 1;
                ct_b[0][0] = 0.0;
                ct_blen = 1;
            } else {
                negate = -cdytail[0];
                twoProduct(negate, adx, ct_alarge, ct_a[0]);
                ct_a[1][0] = ct_alarge[0];
                ct_alen = 2;
                twoProduct(cdytail[0], bdx, ct_blarge, ct_b[0]);
                ct_b[1][0] = ct_blarge[0];
                ct_blen = 2;
            }
        } else {

            if (cdytail[0] == 0.0) {
                twoProduct(cdxtail[0], ady, ct_alarge, ct_a[0]);
                ct_a[1][0] = ct_alarge[0];
                ct_alen = 2;
                negate = -cdxtail[0];
                twoProduct(negate, bdy, ct_blarge, ct_b[0]);
                ct_b[1][0] = ct_blarge[0];
                ct_blen = 2;
            } else {
                twoProduct(cdxtail[0], ady, cdxt_ady1, cdxt_ady0);
                twoProduct(cdytail[0], adx, cdyt_adx1, cdyt_adx0);
                twoTwoDiff(cdxt_ady1[0], cdxt_ady0[0], cdyt_adx1[0], cdyt_adx0[0], ct_alarge, ct_a[2], ct_a[1],
                           ct_a[0]);
                ct_a[3][0] = ct_alarge[0];
                ct_alen = 4;
                twoProduct(cdytail[0], bdx, cdyt_bdx1, cdyt_bdx0);
                twoProduct(cdxtail[0], bdy, cdxt_bdy1, cdxt_bdy0);
                twoTwoDiff(cdyt_bdx1[0], cdyt_bdx0[0], cdxt_bdy1[0], cdxt_bdy0[0], ct_blarge, ct_b[2], ct_b[1],
                           ct_b[0]);
                ct_b[3][0] = ct_blarge[0];
                ct_blen = 4;
            }
        }

        bctlen = fastExpansionSumZeroElim(bt_clen, bt_c, ct_blen, ct_b, bct);
        wlength = scaleExpansionZeroElim(bctlen, bct, adz, w);
        finlength = fastExpansionSumZeroElim(finlength, finnow, wlength, w, finother);
        finswap = finnow;
        finnow = finother;
        finother = finswap;

        catlen = fastExpansionSumZeroElim(ct_alen, ct_a, at_clen, at_c, cat);
        wlength = scaleExpansionZeroElim(catlen, cat, bdz, w);
        finlength = fastExpansionSumZeroElim(finlength, finnow, wlength, w, finother);
        finswap = finnow;
        finnow = finother;
        finother = finswap;

        abtlen = fastExpansionSumZeroElim(at_blen, at_b, bt_alen, bt_a, abt);
        wlength = scaleExpansionZeroElim(abtlen, abt, cdz, w);
        finlength = fastExpansionSumZeroElim(finlength, finnow, wlength, w, finother);
        finswap = finnow;
        finnow = finother;
        finother = finswap;

        if (adztail[0] != 0.0) {
            vlength = scaleExpansionZeroElim(4, bc, adztail, v);
            finlength = fastExpansionSumZeroElim(finlength, finnow, vlength, v, finother);
            finswap = finnow;
            finnow = finother;
            finother = finswap;
        }

        if (bdztail[0] != 0.0) {
            vlength = scaleExpansionZeroElim(4, ca, bdztail, v);
            finlength = fastExpansionSumZeroElim(finlength, finnow, vlength, v, finother);
            finswap = finnow;
            finnow = finother;
            finother = finswap;
        }

        if (cdztail[0] != 0.0) {
            vlength = scaleExpansionZeroElim(4, ab, cdztail, v);
            finlength = fastExpansionSumZeroElim(finlength, finnow, vlength, v, finother);
            finswap = finnow;
            finnow = finother;
            finother = finswap;
        }

        if (adxtail[0] != 0.0) {

            if (bdytail[0] != 0.0) {
                twoProduct(adxtail[0], bdytail[0], adxt_bdyt1, adxt_bdyt0);
                twoOneProduct(adxt_bdyt1[0], adxt_bdyt0[0], cdz[0], u3, u[2], u[1], u[0]);
                u[3][0] = u3[0];
                finlength = fastExpansionSumZeroElim(finlength, finnow, 4, u, finother);
                finswap = finnow;
                finnow = finother;
                finother = finswap;

                if (cdztail[0] != 0.0) {
                    twoOneProduct(adxt_bdyt1[0], adxt_bdyt0[0], cdztail[0], u3, u[2], u[1], u[0]);
                    u[3][0] = u3[0];
                    finlength = fastExpansionSumZeroElim(finlength, finnow, 4, u, finother);
                    finswap = finnow;
                    finnow = finother;
                    finother = finswap;
                }
            }

            if (cdytail[0] != 0.0) {
                negate = -adxtail[0];
                twoProduct(negate, cdytail[0], adxt_cdyt1, adxt_cdyt0);
                twoOneProduct(adxt_cdyt1[0], adxt_cdyt0[0], bdz[0], u3, u[2], u[1], u[0]);
                u[3][0] = u3[0];
                finlength = fastExpansionSumZeroElim(finlength, finnow, 4, u, finother);
                finswap = finnow;
                finnow = finother;
                finother = finswap;

                if (bdztail[0] != 0.0) {
                    twoOneProduct(adxt_cdyt1[0], adxt_cdyt0[0], bdztail[0], u3, u[2], u[1], u[0]);
                    u[3][0] = u3[0];
                    finlength = fastExpansionSumZeroElim(finlength, finnow, 4, u, finother);
                    finswap = finnow;
                    finnow = finother;
                    finother = finswap;
                }
            }
        }

        if (bdxtail[0] != 0.0) {

            if (cdytail[0] != 0.0) {
                twoProduct(bdxtail[0], cdytail[0], bdxt_cdyt1, bdxt_cdyt0);
                twoOneProduct(bdxt_cdyt1[0], bdxt_cdyt0[0], adz[0], u3, u[2], u[1], u[0]);
                u[3][0] = u3[0];
                finlength = fastExpansionSumZeroElim(finlength, finnow, 4, u, finother);
                finswap = finnow;
                finnow = finother;
                finother = finswap;

                if (adztail[0] != 0.0) {
                    twoOneProduct(bdxt_cdyt1[0], bdxt_cdyt0[0], adztail[0], u3, u[2], u[1], u[0]);
                    u[3][0] = u3[0];
                    finlength = fastExpansionSumZeroElim(finlength, finnow, 4, u, finother);
                    finswap = finnow;
                    finnow = finother;
                    finother = finswap;
                }
            }

            if (adytail[0] != 0.0) {
                negate = -bdxtail[0];
                twoProduct(negate, adytail[0], bdxt_adyt1, bdxt_adyt0);
                twoOneProduct(bdxt_adyt1[0], bdxt_adyt0[0], cdz[0], u3, u[2], u[1], u[0]);
                u[3][0] = u3[0];
                finlength = fastExpansionSumZeroElim(finlength, finnow, 4, u, finother);
                finswap = finnow;
                finnow = finother;
                finother = finswap;

                if (cdztail[0] != 0.0) {
                    twoOneProduct(bdxt_adyt1[0], bdxt_adyt0[0], cdztail[0], u3, u[2], u[1], u[0]);
                    u[3][0] = u3[0];
                    finlength = fastExpansionSumZeroElim(finlength, finnow, 4, u, finother);
                    finswap = finnow;
                    finnow = finother;
                    finother = finswap;
                }
            }
        }

        if (cdxtail[0] != 0.0) {

            if (adytail[0] != 0.0) {
                twoProduct(cdxtail[0], adytail[0], cdxt_adyt1, cdxt_adyt0);
                twoOneProduct(cdxt_adyt1[0], cdxt_adyt0[0], bdz[0], u3, u[2], u[1], u[0]);
                u[3][0] = u3[0];
                finlength = fastExpansionSumZeroElim(finlength, finnow, 4, u, finother);
                finswap = finnow;
                finnow = finother;
                finother = finswap;

                if (bdztail[0] != 0.0) {
                    twoOneProduct(cdxt_adyt1[0], cdxt_adyt0[0], bdztail[0], u3, u[2], u[1], u[0]);
                    u[3][0] = u3[0];
                    finlength = fastExpansionSumZeroElim(finlength, finnow, 4, u, finother);
                    finswap = finnow;
                    finnow = finother;
                    finother = finswap;
                }
            }

            if (bdytail[0] != 0.0) {
                negate = -cdxtail[0];
                twoProduct(negate, bdytail[0], cdxt_bdyt1, cdxt_bdyt0);
                twoOneProduct(cdxt_bdyt1[0], cdxt_bdyt0[0], adz[0], u3, u[2], u[1], u[0]);
                u[3][0] = u3[0];
                finlength = fastExpansionSumZeroElim(finlength, finnow, 4, u, finother);
                finswap = finnow;
                finnow = finother;
                finother = finswap;

                if (adztail[0] != 0.0) {
                    twoOneProduct(cdxt_bdyt1[0], cdxt_bdyt0[0], adztail[0], u3, u[2], u[1], u[0]);
                    u[3] = u3;
                    finlength = fastExpansionSumZeroElim(finlength, finnow, 4, u, finother);
                    finswap = finnow;
                    finnow = finother;
                    finother = finswap;
                }
            }
        }

        if (adztail[0] != 0.0) {
            wlength = scaleExpansionZeroElim(bctlen, bct, adztail, w);
            finlength = fastExpansionSumZeroElim(finlength, finnow, wlength, w, finother);
            finswap = finnow;
            finnow = finother;
            finother = finswap;
        }

        if (bdztail[0] != 0.0) {
            wlength = scaleExpansionZeroElim(catlen, cat, bdztail, w);
            finlength = fastExpansionSumZeroElim(finlength, finnow, wlength, w, finother);
            finswap = finnow;
            finnow = finother;
            finother = finswap;
        }

        if (cdztail[0] != 0.0) {
            wlength = scaleExpansionZeroElim(abtlen, abt, cdztail, w);
            finlength = fastExpansionSumZeroElim(finlength, finnow, wlength, w, finother);
            finswap = finnow;
            finnow = finother;
            finother = finswap;
        }

        return finnow[finlength - 1][0];
    } // private double orient3dAdapt

    /**
     * DOCUMENT ME!
     *
     * @param   root       DOCUMENT ME!
     * @param   p          DOCUMENT ME!
     * @param   vertIndex  DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    private boolean outOfFlat(simplex root, double[] p, int vertIndex) {
        basisS bas;
        neighbor neigh, neigh2;
        double[] vert = null;
        int vertIndex2 = -1;

        bas = pNeigh.getBasisS();

        if (bas == null) {
            bas = new basisS();

            // basisSList.insertAtBack(bas);
            pNeigh.setBasisS(bas);
        }

        pNeigh.setVert(p);
        pNeigh.setVertIndex(vertIndex);
        cDim++;
        neigh = root.getPeak();

        if (neigh != null) {
            vert = neigh.getVert();
            vertIndex2 = neigh.getVertIndex();
        } // if (neigh != null)

        if (cDim >= 1) {
            neigh2 = root.getNeigh(cDim - 1);
        } else {
            neigh2 = root.getPeak();
        }

        if (neigh2 != null) {
            neigh2.setVert(vert);
            neigh2.setVertIndex(vertIndex2);
        } // if (neigh2 != null)

        if (neigh2 != null) {
            bas = neigh2.getBasisS();

            if (bas != null) {
                bas.decRefCount();

                if (bas.getRefCount() == 0) {
                    basisSList.deleteNode(bas);
                } /*else {
                   * bas = null;}*/
            } // if (bas != null)
        } // if (neigh2 != null)

        Preferences.debug("Doing getBasisSede in outOfFlat\n");
        getBasisSede(root);

        if ((vdCh || powerDiagram) && (root.getNeigh(0) != null) && (root.getNeigh(0).getVert() != null) &&
                (Double.isInfinite(root.getNeigh(0).getVert()[0]))) {
            return true;
        }

        Preferences.debug("Doing reduce in outOfFlat\n");
        reduce(pNeigh.getBasisS(), 2, p, root, cDim, null);

        if (pNeigh.getBasisS().getSqa() != 0.0) {
            return true;
        }

        cDim--;

        return false;
    } // private boolean outOfFlat(simplex root, double p[])

    /**
     * DOCUMENT ME!
     *
     * @param  b  DOCUMENT ME!
     */
    private void printBasis(basisS b) {
        double[] vecs;
        int i;

        if (b == null) {
            Preferences.debug("Null basis\n");

            return;
        }

        if (b.getLScale() < 0) {
            Preferences.debug("Basis computed\n");

            return;
        }

        Preferences.debug("b.lScale = " + b.getLScale() + "\n");
        vecs = b.getVecs();

        if (vecs == null) {
            Preferences.debug("vecs == null\n");

            return;
        }

        Preferences.debug("rDim = " + rDim + " vecs.length = " + vecs.length + "\n");
        Preferences.debug("Expected length of vecs is 2*rDim\n");

        for (i = 0; i < vecs.length; i++) {
            Preferences.debug("vecs[" + i + "] = " + vecs[i] + "\n");
        }

        return;
    } // private void printBasis(basisS b)

    /**
     * DOCUMENT ME!
     *
     * @param  n  DOCUMENT ME!
     */
    private void printNeighborFull(neighbor n) {
        int i;

        if (n == null) {
            Preferences.debug("Null neighbor\n");

            return;
        }

        printSimplexNum(n.getSimp());

        if (n.getVert() == null) {
            Preferences.debug("Null vert\n");
        } else {
            Preferences.debug("Site num = " + siteNumm(n.getVert(), n.getVertIndex()) + "\n");
        }

        if (n.getVert() != null) {
            Preferences.debug("n.getVert().length = " + n.getVert().length);
            Preferences.debug("vert length should = pDim = " + pDim + "\n");

            for (i = 0; i < pDim; i++) {
                Preferences.debug("n.getVert(" + i + ") = " + n.getVert(i) + "\n");
            }
        } // if (n.getVert() != null)

        printBasis(n.getBasisS());

        return;
    } // private void printNeighborFull(neighbor n)

    /**
     * DOCUMENT ME!
     *
     * @param  s  DOCUMENT ME!
     */
    private void printSimplexF(simplex s) {
        int i;

        if (s == null) {
            Preferences.debug("Null simplex\n");

            return;
        }

        Preferences.debug("Normal:\n");
        printBasis(s.getNormal());
        Preferences.debug("Peak:\n");
        printNeighborFull(s.getPeak());
        Preferences.debug("Facet:\n");

        for (i = 0; i < cDim; i++) {
            printNeighborFull(s.getNeigh(i));
        }

        return;
    } // private void printSimplexF(simplex s)

    /**
     * DOCUMENT ME!
     *
     * @param  s  DOCUMENT ME!
     */
    private void printSimplexNum(simplex s) {
        Preferences.debug("simplex:");

        if (s == null) {
            Preferences.debug("  null\n");
        } else {
            Preferences.debug(" not null\n");
        }

        return;
    } // private void printSimplexNum(simplex s)

    /**
     * DOCUMENT ME!
     *
     * @param   v        DOCUMENT ME!
     * @param   vStatus  DOCUMENT ME!
     * @param   p        DOCUMENT ME!
     * @param   s        DOCUMENT ME!
     * @param   k        DOCUMENT ME!
     * @param   sn       DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    private boolean reduce(basisS v, int vStatus, double[] p, simplex s, int k, neighbor sn) {

        // vStatus == 1 if call from reduce(s.getNormal(), 1, rn.getVert(), s, cDim, null);
        // vStatus == 2 if call from reduce(pNeigh.getBasisS(), 2, p, root, cDim, null);
        // vStatus == 3 if call from reduce(sn.getBasisS(), 3, sn.getVert(), s, k, sn);
        double[] z;
        neighbor neigh;
        double[] tt;
        int i;
        neigh = s.getNeigh(0);
        tt = neigh.getVert();

        for (i = 0; i < pDim; i++) {
            Preferences.debug("reduce entry p[" + i + "] = " + p[i] + "\n");
        }

        for (i = 0; i < pDim; i++) {
            Preferences.debug("reduce entry tt[" + i + "] = " + tt[i] + "\n");
        }

        if (v == null) {
            Preferences.debug("v is null on entry to reduce\n");
            v = new basisS();
            basisSList.insertAtBack(v);
            v.setRefCount(1);

            if (vStatus == 1) {
                s.setNormal(v);
            } else if (vStatus == 2) {
                pNeigh.setBasisS(v);
            } else if (vStatus == 3) {
                sn.setBasisS(v);
            }
        } else {
            v.setLScale(0);

            for (i = 0; i < (2 * rDim); i++) {
                Preferences.debug("reduce entry vec[" + i + "] = " + v.getVecs()[i] + "\n");
            }
        }

        if (vdCh || powerDiagram) {

            if (Double.isInfinite(p[0])) {
                v.setRefCount(infinityBasis.getRefCount());
                v.setLScale(infinityBasis.getLScale());
                v.setVecsCopy(infinityBasis.getVecs());
                v.setSqa(infinityBasis.getSqa());
                v.setSqb(infinityBasis.getSqb());
            } // if (Double.isInfinite(p[0]))
            else {

                for (i = 0; i < pDim; i++) {
                    v.setVecs(i, p[i] - tt[i]);
                    v.setVecs(i + rDim, p[i] - tt[i]);
                }

                if (vdCh) {
                    z = v.getVecs();
                    z[(2 * rDim) - 1] = z[rDim - 1] = vecDotPDim(z, z) * Math.pow(2.0, -DELIFT);
                    v.setVecs(rDim - 1, z[rDim - 1]);
                    v.setVecs((2 * rDim) - 1, z[(2 * rDim) - 1]);
                }
            } // else
        } // if (vdCh || powerDiagram)
        else {

            for (i = 0; i < pDim; i++) {
                v.setVecs(i, p[i] - tt[i]);
                v.setVecs(i + rDim, p[i] - tt[i]);
            }
        } // else

        Preferences.debug("Doing reduceInner in reduce\n");

        return reduceInner(v, s, k);
    } // private boolean reduce(basisS v, double point[], simplex s, int k)

    /**
     * DOCUMENT ME!
     *
     * @param   v  DOCUMENT ME!
     * @param   s  DOCUMENT ME!
     * @param   k  DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    private boolean reduceInner(basisS v, simplex s, int k) {
        double[] va = new double[rDim];
        double[] vb;
        double[] va2 = new double[rDim];
        double[] vb2;
        int i;
        int j;
        int m;
        double dd;
        double scale;
        basisS snibv;
        neighbor sni;
        double nrm2;

        for (i = 0; i < (2 * rDim); i++) {
            Preferences.debug("reduceInner entry vec[" + i + "] = " + v.getVecs()[i] + "\n");
        }

        for (i = rDim; i < (2 * rDim); i++) {
            va[i - rDim] = v.getVecs()[i];
        }

        /* lower_terms(v) */
        nrm2 = norm2(v.getVecs());
        v.setSqa(nrm2);
        v.setSqb(nrm2);

        if (k <= 1) {

            for (i = 0; i < rDim; i++) {
                v.setVecs(i, v.getVecs()[i + rDim]);
            }

            return true;
        } // if (k <= 1)

        for (j = 0; j < 250; j++) {

            for (i = 0; i < rDim; i++) {
                v.setVecs(i, v.getVecs()[i + rDim]);
            }

            for (i = k - 1; i > 0; i--) {
                sni = s.getNeigh(i);
                snibv = sni.getBasisS();
                vb2 = snibv.getVecs();
                dd = -vecDot(vb2, v.getVecs()) / snibv.getSqb();

                for (m = rDim; m < (2 * rDim); m++) {
                    va2[m - rDim] = vb2[m];
                }

                vb = v.getVecs();
                aXPlusY(dd, va2, vb);

                for (m = 0; m < rDim; m++) {
                    v.setVecs(m, vb[m]);
                }
            } // for (i = k-1; i > 0; i--)

            v.setSqb(norm2(v.getVecs()));
            v.setSqa(norm2(va));

            if ((2 * v.getSqb()) >= v.getSqa()) {
                B[j]++;

                return true;
            }

            Preferences.debug("Doing sc # " + j + " in reduceInner\n");
            scale = sc(v, s, k, j);
            Preferences.debug("Doing vecScaleTest # " + j + " in reduceInner\n");
            vecScaleTest(rDim, scale, va);

            for (i = 0; i < rDim; i++) {
                v.setVecs(i + rDim, va[i]);
            }

            for (i = k - 1; i > 0; i--) {
                sni = s.getNeigh(i);
                snibv = sni.getBasisS();
                vb2 = snibv.getVecs();
                dd = -vecDot(vb2, va) / snibv.getSqb();
                dd = Math.floor(dd + 0.5);

                for (m = rDim; m < (2 * rDim); m++) {
                    va2[m - rDim] = vb2[m];
                }

                aXPlusYTest(dd, va2, va);

                for (m = 0; m < rDim; m++) {
                    v.setVecs(m + rDim, va[m]);
                }
            } // for (i = k-1; i > 0; i--)
        } // for (j = 0; j < 250; j++)

        if (failCount++ < 10) {
            Preferences.debug("reduceInner failed\n");
            printBasis(v);
            printSimplexF(s);
        }

        return false;
    } // private boolean reduceInner(basisS v, simplex s, int k)

    /**
     * DOCUMENT ME!
     *
     * @param   v  DOCUMENT ME!
     * @param   s  DOCUMENT ME!
     * @param   k  DOCUMENT ME!
     * @param   j  DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    private double sc(basisS v, simplex s, int k, int j) {

        /* amount by which to scale up vector, for reduceInner */
        double laBound;

        if (j < 10) {
            laBound = logb(v.getSqa()) / 2.0;
            maxScale = exactBits - laBound - (0.66 * (k - 2)) - 1 - DELIFT;

            if (maxScale < 1) {
                Preferences.debug("Warning, overshot exact arithmetic\n");
                maxScale = 1;
            } // if (maxScale < 1)

            if (j == 0) {
                int i;
                neighbor sni;
                basisS snib;

                lDetBound = DELIFT;

                sb = 0;

                for (i = k - 1; i > 0; i--) {
                    sni = s.getNeigh(i);
                    snib = sni.getBasisS();
                    sb += snib.getSqb();
                    lDetBound += (logb(snib.getSqb()) / 2.0) + 1;
                    lDetBound -= snib.getLScale();
                } // for (i = k-1; i > 0; i--)
            } // if (j == 0)
        } // if (j < 10)

        if ((lDetBound - v.getLScale() + (logb(v.getSqb()) / 2.0) + 1) < 0.0) {
            Preferences.debug("lDetBound = " + lDetBound + " in routine sc\n");
            Preferences.debug("v.getLScale() = " + v.getLScale() + " in routine sc\n");
            Preferences.debug("v.getSqb() = " + v.getSqb() + " in routine sc\n");
            printSimplexF(s);
            printBasis(v);

            return 0;
        } // if (lDetBound - v.getLScale() + logb(v.getSqb())/2.0 + 1 < 0.0)
        else {
            lScale = (int) (logb(2.0 * sb / (v.getSqb() + (v.getSqa() * bErrMin))) / 2.0);

            if (lScale > maxScale) {
                lScale = (int) maxScale;
            } else if (lScale < 0) {
                lScale = 0;
            }

            v.setLScale(v.getLScale() + lScale);

            if (lScale < 20) {
                return (1 << lScale);
            } else {
                return Math.pow(2.0, lScale);
            }
        } // else
    } // private double sc(basisS v, simplex s, int k, int j)


    /*****************************************************************************/
    /*                                                                           */
    /*  scaleExpansionZeroElim()   Multiply an expansion by a scalar,          */
    /*                               eliminating zero components from the        */
    /*                               output expansion.                           */
    /*                                                                           */
    /*  Sets h = be.  See either version of my paper for details.                */
    /*                                                                           */
    /*  Maintains the nonoverlapping property.  If round-to-even is used (as     */
    /*  with IEEE 754), maintains the strongly nonoverlapping and nonadjacent    */
    /*  properties as well.  (That is, if e has one of these properties, so      */
    /*  will h.)                                                                 */
    /*                                                                           */
    /*****************************************************************************/

    private int scaleExpansionZeroElim(int elen, double[][] e, double[] b, double[][] h) { /* e and h cannot be the
                                                                                            * same. */

        double[] Q = new double[1];
        double[] sum = new double[1];
        double[] hh = new double[1];
        double[] product1 = new double[1];
        double[] product0 = new double[1];
        int eindex, hindex;
        double enow;
        double[] bhi = new double[1];
        double[] blo = new double[1];

        split(b[0], bhi, blo);
        twoProductPresplit(e[0][0], b[0], bhi[0], blo[0], Q, hh);
        hindex = 0;

        if (hh[0] != 0) {
            h[hindex++][0] = hh[0];
        }

        for (eindex = 1; eindex < elen; eindex++) {
            enow = e[eindex][0];
            twoProductPresplit(enow, b[0], bhi[0], blo[0], product1, product0);
            twoSum(Q[0], product0[0], sum, hh);

            if (hh[0] != 0) {
                h[hindex++][0] = hh[0];
            }

            fastTwoSum(product1[0], sum[0], Q, hh);

            if (hh[0] != 0) {
                h[hindex++][0] = hh[0];
            }
        }

        if ((Q[0] != 0.0) || (hindex == 0)) {
            h[hindex++][0] = Q[0];
        }

        return hindex;
    } // private int scaleExpansionZeroElim

    /**
     * DOCUMENT ME!
     *
     * @param   root  DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    private simplex search(simplex root) {

        /* return a simplex s that corresponds to a facet of the
         *current hull, and sees(p, s) */
        simplex s;
        simplex[] stTemp;
        neighbor sn;
        int i;
        int tms = 0;

        if (st == null) {
            st = new simplex[ss + MAXDIM + 1];
        } // if (st == null)

        st[tms++] = root.getPeak().getSimp();
        root.setVisit(pNum);

        if (!sees(p, vertIndex, root)) {

            if (errorStatus == -1) {
                return null;
            }

            for (i = 0; i < cDim; i++) {
                sn = root.getNeigh(i);
                st[tms++] = sn.getSimp();
            }
        } // if (!sees(p, vertIndex, root))

        while (tms > 0) {

            if (tms > ss) {
                stTemp = new simplex[st.length];

                for (i = 0; i < st.length; i++) {
                    stTemp[i] = st[i];
                }

                st = new simplex[(ss += ss) + MAXDIM + 1];

                for (i = 0; i < stTemp.length; i++) {
                    st[i] = stTemp[i];
                }
            } // if (tms > ss)

            s = st[--tms];

            if (s.getVisit() == pNum) {
                continue;
            }

            s.setVisit(pNum);

            if (!sees(p, vertIndex, s)) {

                if (errorStatus == -1) {
                    return null;
                }

                continue;
            }

            if (s.getPeak().getVert() == null) {
                return s;
            }

            for (i = 0; i < cDim; i++) {
                sn = s.getNeigh(i);
                st[tms++] = sn.getSimp();
            } // for (i = 0; i < cDim; i++)
        } // while (tms > 0)

        return null;
    } // private simplex search(simplex root)

    /**
     * DOCUMENT ME!
     *
     * @param   p            DOCUMENT ME!
     * @param   vertexIndex  DOCUMENT ME!
     * @param   s            DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    private boolean sees(double[] p, int vertexIndex, simplex s) {
        double[] tt;
        double[] zz;
        double dd;
        double dds = 0.0;
        int i;
        int j;
        basisS bas;

        if (b == null) {
            b = new basisS();
            // basisSList.insertAtBack(b);
        } else {
            b.setLScale(0);
        }

        if (cDim == 0) {
            return false;
        }

        if (s.getNormal() == null) {
            getNormalSede(s);

            if (errorStatus == -1) {
                return false;
            }

            for (i = 0; i < cDim; i++) {
                bas = s.getNeigh(i).getBasisS();

                if (bas != null) {
                    bas.decRefCount();

                    if (bas.getRefCount() == 0) {
                        basisSList.deleteNode(bas);
                    }
                    /*else {
                     *  bas = null;}*/
                } // if (bas != null)
            } // for (i = 0; i < cDim; i++)
        } // if (s.getNormal() == null)

        tt = s.getNeigh(0).getVert();

        if (vdCh || powerDiagram) {

            if (Double.isInfinite(p[0])) {
                b.setRefCount(infinityBasis.getRefCount());
                b.setLScale(infinityBasis.getLScale());
                b.setVecsCopy(infinityBasis.getVecs());
                b.setSqa(infinityBasis.getSqa());
                b.setSqb(infinityBasis.getSqb());
            } // if (Double.isInfinite(p[0]))
            else {

                for (i = 0; i < pDim; i++) {
                    b.setVecs(i, p[i] - tt[i]);
                    b.setVecs(i + rDim, p[i] - tt[i]);
                }

                if (vdCh) {
                    zz = b.getVecs();
                    zz[(2 * rDim) - 1] = zz[rDim - 1] = vecDotPDim(zz, zz) * Math.pow(2.0, -DELIFT);
                    b.setVecs(rDim - 1, zz[rDim - 1]);
                    b.setVecs((2 * rDim) - 1, zz[(2 * rDim) - 1]);
                }
            }
        } // if (vdCh || powerDiagram)
        else {

            for (i = 0; i < pDim; i++) {
                b.setVecs(i, p[i] - tt[i]);
                b.setVecs(i + rDim, p[i] - tt[i]);
            }
        } // else

        for (i = 0; i < 3; i++) {
            dd = vecDot(b.getVecs(), s.getNormal().getVecs());

            if (dd == 0.0) {
                Preferences.debug("Degeneracy in sees\n");
                Preferences.debug("siteNumm(p, vertIndex) = " + siteNumm(p, vertIndex) + "\n");

                for (j = 0; j < pDim; j++) {
                    Preferences.debug("p[" + j + "] = " + p[j] + "\n");
                }

                printSimplexF(s);

                return false;
            } // if (dd == 0.0)

            dds = dd * dd / s.getNormal().getSqb() / norm2(b.getVecs());

            if (dds > bErrMinSq) {
                return (dd < 0.0);
            }

            Preferences.debug("Doing getBasisSede in sees\n");
            getBasisSede(s);
            Preferences.debug("Doing reduceInner in sees\n");
            reduceInner(b, s, cDim);
        } // for (i = 0; i < 3; i++)

        if (i == 3) {
            MipavUtil.displayError("Looped too much in sees");
            Preferences.debug("Looped too much in sees\n");
            Preferences.debug("dds = " + dds + "\n");
            Preferences.debug("siteNumm(p, vertIndex) = " + siteNumm(p, vertIndex) + "\n");
            printSimplexF(s);
            Preferences.debug("Finished diagnostics for looped too much in sees\n");
            errorStatus = -1;
        } // if (i == 3)

        return false;
    } // private boolean sees (double p[], int vertIndex, simplex s)

    /**
     * DOCUMENT ME!
     *
     * @param   p                DOCUMENT ME!
     * @param   currentNumSites  DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    private int siteNumm(double[] p, int currentNumSites) {
        int i;
        int currentNumBlocks = currentNumSites / BLOCKSIZE;

        if (p == null) {
            return -2;
        }

        if ((vd || powerDiagram) && Double.isInfinite(p[0])) {
            return -1;
        }

        for (i = 0; i < numBlocks; i++) {

            if (currentNumBlocks == i) {
                return currentNumSites;
            }
        } // for (i = 0; i < numBlocks; i++)

        return -3;
    } // int siteNumm(double p[], int currentNumSites)


    /**
     * DOCUMENT ME!
     *
     * @param  a    DOCUMENT ME!
     * @param  ahi  DOCUMENT ME!
     * @param  alo  DOCUMENT ME!
     */
    private void split(double a, double[] ahi, double[] alo) {
        double c;
        double abig;

        c = splitter * a;
        abig = c - a;
        ahi[0] = c - abig;
        alo[0] = a - ahi[0];

        return;
    } // private void split(double a, double ahi[], double alo[])

    /**
     * DOCUMENT ME!
     *
     * @param   a  DOCUMENT ME!
     * @param   b  DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    private double sqdist(double[] a, double[] b) {

        /* returns the squared distance between a and b */
        return ((a[0] - b[0]) * (a[0] - b[0])) + ((a[1] - b[1]) * (a[1] - b[1])) + ((a[2] - b[2]) * (a[2] - b[2]));
    }


    /*****************************************************************************/
    /*                                                                           */
    /*  tetCircumcenter()   Find the circumcenter of a tetrahedron.              */
    /*                                                                           */
    /*  The result is returned both in terms of xyz coordinates and xi-eta-zeta  */
    /*  coordinates, relative to the tetrahedron's point `a' (that is, `a' is    */
    /*  the origin of both coordinate systems).  Hence, the xyz coordinates      */
    /*  returned are NOT absolute; one must add the coordinates of `a' to        */
    /*  find the absolute coordinates of the circumcircle.  However, this means  */
    /*  that the result is frequently more accurate than would be possible if    */
    /*  absolute coordinates were returned, due to limited floating-point        */
    /*  precision.  In general, the circumradius can be computed much more       */
    /*  accurately.                                                              */
    /*                                                                           */
    /*  The xi-eta-zeta coordinate system is defined in terms of the             */
    /*  tetrahedron.  Point `a' is the origin of the coordinate system.          */
    /*  The edge `ab' extends one unit along the xi axis.  The edge `ac'         */
    /*  extends one unit along the eta axis.  The edge `ad' extends one unit     */
    /*  along the zeta axis.  These coordinate values are useful for linear      */
    /*  interpolation.                                                           */
    /*                                                                           */
    /*  If `xi' is NULL on input, the xi-eta-zeta coordinates will not be        */
    /*  computed.                                                                */
    /*                                                                           */
    /*****************************************************************************/

    private void tetCircumcenter(double[] a, double[] b, double[] c, double[] d, double[] circumcenter, double[] cond) {
        double xba, yba, zba, xca, yca, zca, xda, yda, zda;
        double baLength, caLength, daLength;
        double xcrosscd, ycrosscd, zcrosscd;
        double xcrossdb, ycrossdb, zcrossdb;
        double xcrossbc, ycrossbc, zcrossbc;
        double denominator;
        double xcirca, ycirca, zcirca;

        /* Use coordinates relative to point `a' of the tetrahedron. */
        xba = b[0] - a[0];
        yba = b[1] - a[1];
        zba = b[2] - a[2];
        xca = c[0] - a[0];
        yca = c[1] - a[1];
        zca = c[2] - a[2];
        xda = d[0] - a[0];
        yda = d[1] - a[1];
        zda = d[2] - a[2];

        /* Squares of lengths of the edges incident to `a'. */
        baLength = (xba * xba) + (yba * yba) + (zba * zba);
        caLength = (xca * xca) + (yca * yca) + (zca * zca);
        daLength = (xda * xda) + (yda * yda) + (zda * zda);

        /* Cross products of these edges. */
        xcrosscd = (yca * zda) - (yda * zca);
        ycrosscd = (zca * xda) - (zda * xca);
        zcrosscd = (xca * yda) - (xda * yca);
        xcrossdb = (yda * zba) - (yba * zda);
        ycrossdb = (zda * xba) - (zba * xda);
        zcrossdb = (xda * yba) - (xba * yda);
        xcrossbc = (yba * zca) - (yca * zba);
        ycrossbc = (zba * xca) - (zca * xba);
        zcrossbc = (xba * yca) - (xca * yba);

        /* Calculate the denominator of the formulae. */
        /* Use orient3d() from http://www.cs.cmu.edu/~quake/robust.html     */
        /*   to ensure a correctly signed (and reasonably accurate) result, */
        /*   avoiding any possibility of division by zero.                  */
        cond[0] = orient3d(b, c, d, a);
        denominator = 0.5 / (cond[0]);


        /* Calculate offset (from `a') of circumcenter. */
        xcirca = ((baLength * xcrosscd) + (caLength * xcrossdb) + (daLength * xcrossbc)) * denominator;
        ycirca = ((baLength * ycrosscd) + (caLength * ycrossdb) + (daLength * ycrossbc)) * denominator;
        zcirca = ((baLength * zcrosscd) + (caLength * zcrossdb) + (daLength * zcrossbc)) * denominator;
        circumcenter[0] = xcirca;
        circumcenter[1] = ycirca;
        circumcenter[2] = zcirca;

        return;
    } // private void tetCircumcenter

    /**
     * DOCUMENT ME!
     *
     * @param  a  DOCUMENT ME!
     * @param  b  DOCUMENT ME!
     * @param  x  DOCUMENT ME!
     * @param  y  DOCUMENT ME!
     */
    private void twoDiff(double a, double b, double[] x, double[] y) {
        x[0] = a - b;
        twoDiffTail(a, b, x[0], y);

        return;
    } // private void twoDiff(double a, double b, double x[], double y[])

    /**
     * DOCUMENT ME!
     *
     * @param  a  DOCUMENT ME!
     * @param  b  DOCUMENT ME!
     * @param  x  DOCUMENT ME!
     * @param  y  DOCUMENT ME!
     */
    private void twoDiffTail(double a, double b, double x, double[] y) {
        double bvirt;
        double avirt;
        double bround;
        double around;
        bvirt = a - x;
        avirt = x + bvirt;
        bround = bvirt - b;
        around = a - avirt;
        y[0] = around + bround;

        return;
    } // private void twoDiffTail(double a, double b, double x, double y[])

    /**
     * DOCUMENT ME!
     *
     * @param  a1  DOCUMENT ME!
     * @param  a0  DOCUMENT ME!
     * @param  b   DOCUMENT ME!
     * @param  x2  DOCUMENT ME!
     * @param  x1  DOCUMENT ME!
     * @param  x0  DOCUMENT ME!
     */
    private void twoOneDiff(double a1, double a0, double b, double[] x2, double[] x1, double[] x0) {
        double[] _i = new double[1];
        twoDiff(a0, b, _i, x0);
        twoSum(a1, _i[0], x2, x1);

        return;
    } // private void twoOneDiff

    /**
     * DOCUMENT ME!
     *
     * @param  a1  DOCUMENT ME!
     * @param  a0  DOCUMENT ME!
     * @param  b   DOCUMENT ME!
     * @param  x3  DOCUMENT ME!
     * @param  x2  DOCUMENT ME!
     * @param  x1  DOCUMENT ME!
     * @param  x0  DOCUMENT ME!
     */
    private void twoOneProduct(double a1, double a0, double b, double[] x3, double[] x2, double[] x1, double[] x0) {
        double[] bhi = new double[1];
        double[] blo = new double[1];
        double[] _i = new double[1];
        double[] _j = new double[1];
        double[] _k = new double[1];
        double[] _0 = new double[1];
        split(b, bhi, blo);
        twoProductPresplit(a0, b, bhi[0], blo[0], _i, x0);
        twoProductPresplit(a1, b, bhi[0], blo[0], _j, _0);
        twoSum(_i[0], _0[0], _k, x1);
        fastTwoSum(_j[0], _k[0], x3, x2);

        return;
    } // private void twoOneProduct

    /**
     * DOCUMENT ME!
     *
     * @param  a  DOCUMENT ME!
     * @param  b  DOCUMENT ME!
     * @param  x  DOCUMENT ME!
     * @param  y  DOCUMENT ME!
     */
    private void twoProduct(double a, double b, double[] x, double[] y) {
        x[0] = a * b;
        twoProductTail(a, b, x[0], y);

        return;
    } // private void twoProduct(double a, double b, double x[], double y[])

    /**
     * Two_Product_Presplit() is Two_Product() where one of the inputs has already been split. Avoids redundant
     * splitting.
     *
     * @param  a    DOCUMENT ME!
     * @param  b    DOCUMENT ME!
     * @param  bhi  DOCUMENT ME!
     * @param  blo  DOCUMENT ME!
     * @param  x    DOCUMENT ME!
     * @param  y    DOCUMENT ME!
     */
    private void twoProductPresplit(double a, double b, double bhi, double blo, double[] x, double[] y) {
        double[] ahi = new double[1];
        double[] alo = new double[1];
        double err1;
        double err2;
        double err3;

        x[0] = a * b;
        split(a, ahi, alo);
        err1 = x[0] - (ahi[0] * bhi);
        err2 = err1 - (alo[0] * bhi);
        err3 = err2 - (ahi[0] * blo);
        y[0] = (alo[0] * blo) - err3;

        return;
    } // private void twoProductPresplit

    /**
     * DOCUMENT ME!
     *
     * @param  a  DOCUMENT ME!
     * @param  b  DOCUMENT ME!
     * @param  x  DOCUMENT ME!
     * @param  y  DOCUMENT ME!
     */
    private void twoProductTail(double a, double b, double x, double[] y) {
        double[] ahi = new double[1];
        double[] alo = new double[1];
        double[] bhi = new double[1];
        double[] blo = new double[1];
        double err1;
        double err2;
        double err3;
        split(a, ahi, alo);
        split(b, bhi, blo);
        err1 = x - (ahi[0] * bhi[0]);
        err2 = err1 - (alo[0] * bhi[0]);
        err3 = err2 - (ahi[0] * blo[0]);
        y[0] = (alo[0] * blo[0]) - err3;

        return;
    } // private void twoProductTail(double a, double b, double x, double y[])

    /**
     * DOCUMENT ME!
     *
     * @param  a  DOCUMENT ME!
     * @param  b  DOCUMENT ME!
     * @param  x  DOCUMENT ME!
     * @param  y  DOCUMENT ME!
     */
    private void twoSum(double a, double b, double[] x, double[] y) {
        x[0] = a + b;
        twoSumTail(a, b, x[0], y);

        return;
    } // private void twoSum(double a, double b, double x[], double y[])

    /**
     * DOCUMENT ME!
     *
     * @param  a  DOCUMENT ME!
     * @param  b  DOCUMENT ME!
     * @param  x  DOCUMENT ME!
     * @param  y  DOCUMENT ME!
     */
    private void twoSumTail(double a, double b, double x, double[] y) {
        double bvirt;
        double avirt;
        double bround;
        double around;
        bvirt = x - a;
        avirt = x - bvirt;
        bround = b - bvirt;
        around = a - avirt;
        y[0] = around + bround;

        return;
    } // private void twoSumTail(double a, double b, double x, double y[])

    /**
     * DOCUMENT ME!
     *
     * @param  a1  DOCUMENT ME!
     * @param  a0  DOCUMENT ME!
     * @param  b1  DOCUMENT ME!
     * @param  b0  DOCUMENT ME!
     * @param  x3  DOCUMENT ME!
     * @param  x2  DOCUMENT ME!
     * @param  x1  DOCUMENT ME!
     * @param  x0  DOCUMENT ME!
     */
    private void twoTwoDiff(double a1, double a0, double b1, double b0, double[] x3, double[] x2, double[] x1,
                            double[] x0) {
        double[] _j = new double[1];
        double[] _0 = new double[1];
        twoOneDiff(a1, a0, b0, _j, _0, x0);
        twoOneDiff(_j[0], _0[0], b1, x3, x2, x1);

        return;
    } // private void twoTwoDiff

    /**
     * DOCUMENT ME!
     *
     * @param   x  DOCUMENT ME!
     * @param   y  DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    private double vecDot(double[] x, double[] y) {
        int i;
        double sum = 0.0;

        for (i = 0; i < rDim; i++) {
            sum += x[i] * y[i];
        }

        return sum;
    } // private double vecDot(double x[], double y[])


    /**
     * DOCUMENT ME!
     *
     * @param   x  DOCUMENT ME!
     * @param   y  DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    private double vecDotPDim(double[] x, double[] y) {
        int i;
        double sum = 0.0;

        for (i = 0; i < pDim; i++) {
            sum += x[i] * y[i];
        }

        return sum;
    } // private double vecDotPDim(double x[], double y[])

    /**
     * DOCUMENT ME!
     *
     * @param  n  DOCUMENT ME!
     * @param  a  DOCUMENT ME!
     * @param  x  DOCUMENT ME!
     */
    private void vecScaleTest(int n, double a, double[] x) {
        int i;

        for (i = 0; i < n; i++) {
            x[i] *= a;
        }
    } // private void vecScaleTest(int n, double a, double x[])


    /**
     * DOCUMENT ME!
     *
     * @param  s  DOCUMENT ME!
     */
    private void visitTriangGen(simplex s) {
        int i;
        int tms = 0;
        simplex[] stTemp;
        simplex t;
        neighbor sn;
        vNum--;

        if (stTri == null) {
            stTri = new simplex[ssTri + MAXDIM + 1];
        }

        if (s != null) {
            stTri[tms++] = s;
        }

        while (tms > 0) {

            if (tms > ssTri) {
                Preferences.debug("In visitTriangGen tms = " + tms + "\n");
                stTemp = new simplex[stTri.length];

                for (i = 0; i < stTri.length; i++) {
                    stTemp[i] = stTri[i];
                }

                stTri = new simplex[(ssTri += ssTri) + MAXDIM + 1];

                for (i = 0; i < stTemp.length; i++) {
                    stTri[i] = stTemp[i];
                }
            } // if (tms > ssTri)

            if (stTri[--tms] == null) {
                t = null;

                continue;
            }

            t = stTri[tms];

            if (t.getVisit() == vNum) {
                continue;
            }

            t.setVisit(vNum);
            printSimplexF(t);
            sn = t.getPeak();

            if ((sn.getSimp() != null) && (sn.getSimp().getVisit() != vNum)) {
                stTri[tms++] = sn.getSimp();
            }

            for (i = 0; i < cDim; i++) {
                sn = t.getNeigh(i);

                if ((sn.getSimp() != null) && (sn.getSimp().getVisit() != vNum)) {
                    stTri[tms++] = sn.getSimp();
                }
            } // for (i = 0; i < cDim; i++)
        } // while (tms > 0)

        return;
    } // private void visitTriangGen(simplex s)

    //~ Inner Classes --------------------------------------------------------------------------------------------------

    /**
     * DOCUMENT ME!
     */
    private class basisS {

        /** DOCUMENT ME! */
        int lScale; /* the log base 2 of total scaling of vector */

        /** DOCUMENT ME! */
        basisS nextNode; /* free list */

        /** DOCUMENT ME! */
        int refCount; /* storage management */

        /** DOCUMENT ME! */
        double sqa; /* sum of squared norms of a part */

        /** DOCUMENT ME! */
        double sqb; /* sum of squared norms of b part */

        /** DOCUMENT ME! */
        double[] vecs = new double[8]; /* The actual vectors */

        /**
         * Creates a new basisS object.
         */
        public basisS() { }

        /**
         * DOCUMENT ME!
         */
        public void decRefCount() {
            refCount--;
        }

        /**
         * DOCUMENT ME!
         *
         * @return  DOCUMENT ME!
         */
        public int getLScale() {
            return lScale;
        }

        /**
         * public basisS(basisS next) { this.nextNode = next; } public void setNext(basisS next) { this.nextNode = next;
         * }.
         *
         * @return  DOCUMENT ME!
         */
        public basisS getNext() {
            return nextNode;
        }

        /**
         * DOCUMENT ME!
         *
         * @return  DOCUMENT ME!
         */
        public int getRefCount() {
            return refCount;
        }

        /**
         * DOCUMENT ME!
         *
         * @return  DOCUMENT ME!
         */
        public double getSqa() {
            return sqa;
        }

        /**
         * DOCUMENT ME!
         *
         * @return  DOCUMENT ME!
         */
        public double getSqb() {
            return sqb;
        }

        /**
         * DOCUMENT ME!
         *
         * @return  DOCUMENT ME!
         */
        public double[] getVecs() {
            return vecs;
        }

        /**
         * DOCUMENT ME!
         */
        public void incRefCount() {
            refCount++;
        }

        /**
         * DOCUMENT ME!
         *
         * @param  num  DOCUMENT ME!
         */
        public void newVecs(int num) {
            vecs = new double[num];
        }

        /**
         * DOCUMENT ME!
         *
         * @param  lScale  DOCUMENT ME!
         */
        public void setLScale(int lScale) {
            this.lScale = lScale;
        }

        /**
         * DOCUMENT ME!
         *
         * @param  refCount  DOCUMENT ME!
         */
        public void setRefCount(int refCount) {
            this.refCount = refCount;
        }

        /**
         * DOCUMENT ME!
         *
         * @param  sqa  DOCUMENT ME!
         */
        public void setSqa(double sqa) {
            this.sqa = sqa;
        }

        /**
         * DOCUMENT ME!
         *
         * @param  sqb  DOCUMENT ME!
         */
        public void setSqb(double sqb) {
            this.sqb = sqb;
            /*if (sqb == 0.0) {
             *  sqbtest++; if (sqbtest == 2) {     double test[] = new double[1];     test[1] = 0.0; }}*/
        }

        /**
         * DOCUMENT ME!
         *
         * @param  vecs  DOCUMENT ME!
         */
        public void setVecs(double[] vecs) {
            this.vecs = vecs;
        }

        /**
         * DOCUMENT ME!
         *
         * @param  index  DOCUMENT ME!
         * @param  num    DOCUMENT ME!
         */
        public void setVecs(int index, double num) {
            vecs[index] = num;
        }

        /**
         * DOCUMENT ME!
         *
         * @param  vecs  DOCUMENT ME!
         */
        public void setVecsCopy(double[] vecs) {
            this.vecs = new double[vecs.length];

            for (int i = 0; i < vecs.length; i++) {
                this.vecs[i] = vecs[i];
            }
        }
    } // private class basisS

    /**
     * DOCUMENT ME!
     */
    private class BasisSList {

        /** DOCUMENT ME! */
        private basisS firstNode;

        /** DOCUMENT ME! */
        private basisS lastNode;

        /**
         * Creates a new BasisSList object.
         */
        public BasisSList() {
            firstNode = lastNode = null;
        }

        /**
         * DOCUMENT ME!
         *
         * @param  node  DOCUMENT ME!
         */
        public void deleteNode(basisS node) {
            basisS previousNode = null;
            basisS current = firstNode;

            while ((current != node) && (current != null)) {
                previousNode = current;
                current = current.nextNode;
            }

            if (current != null) {

                if (firstNode == lastNode) {
                    firstNode = lastNode = null;
                } else if (current == firstNode) {
                    firstNode = current.nextNode;
                    current = null;
                } else if (current == lastNode) {
                    lastNode = previousNode;
                    current = null;
                } else {
                    previousNode.nextNode = current.nextNode;
                    current = null;
                }
            } // if (current != null)
        } // public void deleteNode(basisS node)

        /**
         * DOCUMENT ME!
         *
         * @param  insertNode  DOCUMENT ME!
         */
        public void insertAtBack(basisS insertNode) {

            if (isEmpty()) {
                firstNode = lastNode = insertNode;
            } else {
                lastNode = lastNode.nextNode = insertNode;
            }
        }

        /**
         * DOCUMENT ME!
         *
         * @return  DOCUMENT ME!
         */
        public boolean isEmpty() {
            return firstNode == null;
        }
    } // private class BasisSList

    /**
     * DOCUMENT ME!
     */
    private class neighbor {

        /** DOCUMENT ME! */
        basisS basis; /* derived vectors */

        /** DOCUMENT ME! */
        simplex simp; /* neighbor sharing all vertices but vert */

        /** DOCUMENT ME! */
        double[] vert; /* vertex of simplex */

        /** DOCUMENT ME! */
        int vertIndex;

        /**
         * Creates a new neighbor object.
         */
        public neighbor() {
            // basis = new basisS();
            // basisSList.insertAtBack(basis);
        }


        /**
         * DOCUMENT ME!
         *
         * @return  DOCUMENT ME!
         */
        public basisS getBasisS() {
            return basis;
        }

        /**
         * DOCUMENT ME!
         *
         * @return  DOCUMENT ME!
         */
        public simplex getSimp() {
            return simp;
        }

        /**
         * DOCUMENT ME!
         *
         * @return  DOCUMENT ME!
         */
        public double[] getVert() {
            return vert;
        }

        /**
         * DOCUMENT ME!
         *
         * @param   index  DOCUMENT ME!
         *
         * @return  DOCUMENT ME!
         */
        public double getVert(int index) {
            return vert[index];
        }

        /**
         * DOCUMENT ME!
         *
         * @return  DOCUMENT ME!
         */
        public int getVertIndex() {
            return vertIndex;
        }

        /**
         * DOCUMENT ME!
         *
         * @param  basis  DOCUMENT ME!
         */
        public void setBasisS(basisS basis) {
            this.basis = basis;
        }

        /**
         * DOCUMENT ME!
         *
         * @param  simp  DOCUMENT ME!
         */
        public void setSimp(simplex simp) {
            this.simp = simp;
        }

        /**
         * DOCUMENT ME!
         *
         * @param  vert  DOCUMENT ME!
         */
        public void setVert(double[] vert) {
            this.vert = vert;
        }

        /**
         * DOCUMENT ME!
         *
         * @param  index  DOCUMENT ME!
         * @param  num    DOCUMENT ME!
         */
        public void setVert(int index, double num) {
            vert[index] = num;
        }

        /**
         * DOCUMENT ME!
         *
         * @param  vert  DOCUMENT ME!
         */
        public void setVertCopy(double[] vert) {

            if (vert != null) {
                this.vert = new double[vert.length];

                for (int i = 0; i < vert.length; i++) {
                    this.vert[i] = vert[i];
                }
            } else {
                this.vert = null;
            }
        }

        /**
         * DOCUMENT ME!
         *
         * @param  vertIndex  DOCUMENT ME!
         */
        public void setVertIndex(int vertIndex) {
            this.vertIndex = vertIndex;
        }
    } // private class neighbor

    /**
     * DOCUMENT ME!
     */
    private class simplex {

        /** DOCUMENT ME! */
        short[] edgeStatus = new short[6];

        /** DOCUMENT ME! */
        short mark;

        /** DOCUMENT ME! */
        neighbor[] neigh = new neighbor[4]; /* neighbors of simplex */

        /** DOCUMENT ME! */
        simplex nextNode; /* Used in free list */

        /** DOCUMENT ME! */
        basisS normal = null; /* Normal vector pointing inward */

        /** DOCUMENT ME! */
        neighbor peak; /* if null, remaining vertices give facet */

        /** DOCUMENT ME! */
        int poleIndex; /* for 1st DT, if STATUS == POLE_OUTPUT, contains poleIndex; for
                        *2nd, contains vertex index for powercurst output for OFF file format */

        /** DOCUMENT ME! */
        double sqRadius; /* squared radius of Voronoi ball */

        /** DOCUMENT ME! */
        short status; /* 0(CNV) if on conv hull so vv contains normal vector
                       * 1(VV) if vv points to circumcenter of simplex -1(SLV) if cond = 0 so vv points to hull2(AV) if
                       * av contains averaged pole */

        /**
         * NOTE!!! neighbor has to be the LAST field in the simplex structure, since its length gets altered by some
         * tricky Clarkson-move. Also peak has to be the one before it. Don't try to move these babies!!
         */
        int visit; /* number of last site visiting this simplex */

        /** DOCUMENT ME! */
        double[] vv = new double[3]; /* Voronoi vertex of simplex */

        /**
         * Creates a new simplex object.
         */
        public simplex() {
            peak = new neighbor();

            for (int i = 0; i < 4; i++) {
                neigh[i] = new neighbor();
            }
            // normal = new basisS();
            // basisSList.insertAtBack(normal);
        }

        /**
         * DOCUMENT ME!
         *
         * @return  DOCUMENT ME!
         */
        public short[] getEdgeStatus() {
            return edgeStatus;
        }

        /**
         * DOCUMENT ME!
         *
         * @return  DOCUMENT ME!
         */
        public short getMark() {
            return mark;
        }

        /**
         * DOCUMENT ME!
         *
         * @return  DOCUMENT ME!
         */
        public neighbor[] getNeigh() {
            return neigh;
        }

        /**
         * DOCUMENT ME!
         *
         * @param   index  DOCUMENT ME!
         *
         * @return  DOCUMENT ME!
         */
        public neighbor getNeigh(int index) {

            if ((neigh != null) && (neigh.length >= (index + 1))) {
                return neigh[index];
            } else {
                return null;
            }
        }

        /**
         * public simplex(simplex next) { this.nextNode = next; peak = new neighbor(); for (int i = 0; i < 4; i++) {
         * neigh[i] = new neighbor(); } normal = new basisS(); basisSList.insertAtBack(normal); } public void
         * setNextNode(simplex next) { this.nextNode = next; }.
         *
         * @return  DOCUMENT ME!
         */
        public simplex getNextNode() {
            return nextNode;
        }

        /**
         * DOCUMENT ME!
         *
         * @return  DOCUMENT ME!
         */
        public basisS getNormal() {
            return normal;
        }

        /**
         * DOCUMENT ME!
         *
         * @return  DOCUMENT ME!
         */
        public neighbor getPeak() {
            return peak;
        }

        /**
         * DOCUMENT ME!
         *
         * @return  DOCUMENT ME!
         */
        public int getPoleIndex() {
            return poleIndex;
        }

        /**
         * DOCUMENT ME!
         *
         * @return  DOCUMENT ME!
         */
        public double getSqRadius() {
            return sqRadius;
        }

        /**
         * DOCUMENT ME!
         *
         * @return  DOCUMENT ME!
         */
        public short getStatus() {
            return status;
        }

        /**
         * DOCUMENT ME!
         *
         * @return  DOCUMENT ME!
         */
        public int getVisit() {
            return visit;
        }

        /**
         * DOCUMENT ME!
         *
         * @return  DOCUMENT ME!
         */
        public double[] getVV() {
            return vv;
        }

        /**
         * DOCUMENT ME!
         *
         * @param  edgeStatus  DOCUMENT ME!
         */
        public void setEdgeStatus(short[] edgeStatus) {
            this.edgeStatus = edgeStatus;
        }

        /**
         * DOCUMENT ME!
         *
         * @param  edgeStatus  DOCUMENT ME!
         */
        public void setEdgeStatusCopy(short[] edgeStatus) {
            this.edgeStatus = new short[edgeStatus.length];

            for (int i = 0; i < edgeStatus.length; i++) {
                this.edgeStatus[i] = edgeStatus[i];
            }
        }

        /**
         * DOCUMENT ME!
         *
         * @param  mark  DOCUMENT ME!
         */
        public void setMark(short mark) {
            this.mark = mark;
        }

        /**
         * DOCUMENT ME!
         *
         * @param  neigh  DOCUMENT ME!
         */
        public void setNeigh(neighbor[] neigh) {
            this.neigh = neigh;
        }

        /**
         * DOCUMENT ME!
         *
         * @param  index  DOCUMENT ME!
         * @param  neigh  DOCUMENT ME!
         */
        public void setNeigh(int index, neighbor neigh) {
            this.neigh[index] = neigh;
        }

        /**
         * DOCUMENT ME!
         *
         * @param  normal  DOCUMENT ME!
         */
        public void setNormal(basisS normal) {
            this.normal = normal;
        }

        /**
         * DOCUMENT ME!
         *
         * @param  peak  DOCUMENT ME!
         */
        public void setPeak(neighbor peak) {
            this.peak = peak;
        }

        /**
         * DOCUMENT ME!
         *
         * @param  poleIndex  DOCUMENT ME!
         */
        public void setPoleIndex(int poleIndex) {
            this.poleIndex = poleIndex;
        }

        /**
         * DOCUMENT ME!
         *
         * @param  sim  DOCUMENT ME!
         */
        public void setSimplexCopy(simplex sim) {
            int i;
            mark = sim.getMark();
            vv = new double[sim.getVV().length];

            for (i = 0; i < vv.length; i++) {
                vv[i] = sim.getVV()[i];
            }

            sqRadius = sim.getSqRadius();
            status = sim.getStatus();
            poleIndex = sim.getPoleIndex();
            edgeStatus = new short[sim.getEdgeStatus().length];

            for (i = 0; i < edgeStatus.length; i++) {
                edgeStatus[i] = sim.getEdgeStatus()[i];
            }

            visit = sim.getVisit();
            normal = sim.getNormal();

            // normal.setRefCount(sim.getNormal().getRefCount());
            // normal.setLScale(sim.getNormal().getLScale());
            // normal.setSqa(sim.getNormal().getSqa());
            // normal.setSqb(sim.getNormal().getSqb());
            // normal.setVecsCopy(sim.getNormal().getVecs());
            // peak = new neighbor();
            if (sim.getPeak() != null) {

                if (peak == null) {
                    peak = new neighbor();
                }

                peak.setVertCopy(sim.getPeak().getVert());
                peak.setVertIndex(sim.getPeak().getVertIndex());
                peak.setBasisS(sim.getPeak().getBasisS());

                /*if (sim.getPeak().getBasisS() != null) {
                 *  peak.getBasisS().setRefCount(sim.getPeak().getBasisS().getRefCount());
                 * peak.getBasisS().setLScale(sim.getPeak().getBasisS().getLScale());
                 * peak.getBasisS().setSqa(sim.getPeak().getBasisS().getSqa());
                 * peak.getBasisS().setSqb(sim.getPeak().getBasisS().getSqb());
                 * peak.getBasisS().setVecsCopy(sim.getPeak().getBasisS().getVecs());}*/
                peak.setSimp(sim.getPeak().getSimp());
            } // if (sim.getPeak() != null)
            else {
                peak = null;
            }

            for (i = 0; i < 4; i++) {

                if (sim.getNeigh()[i] != null) {

                    if (neigh[i] == null) {
                        neigh[i] = new neighbor();
                    }

                    neigh[i].setVertCopy(sim.getNeigh()[i].getVert());
                    neigh[i].setVertIndex(sim.getNeigh()[i].getVertIndex());
                    neigh[i].setBasisS(sim.getNeigh()[i].getBasisS());

                    /*if (sim.getNeigh()[i].getBasisS() != null) {
                     *  neigh[i].getBasisS().setRefCount(sim.getNeigh()[i].getBasisS().getRefCount());
                     * neigh[i].getBasisS().setLScale(sim.getNeigh()[i].getBasisS().getLScale());
                     * neigh[i].getBasisS().setSqa(sim.getNeigh()[i].getBasisS().getSqa());
                     * neigh[i].getBasisS().setSqb(sim.getNeigh()[i].getBasisS().getSqb());
                     * neigh[i].getBasisS().setVecsCopy(sim.getNeigh()[i].getBasisS().getVecs());}*/
                    neigh[i].setSimp(sim.getNeigh()[i].getSimp());
                } // if (sim.getNeigh[i] != null)
                else {
                    neigh[i] = null;
                }
            } // for (i = 0; i < 4; i++)
        }

        /**
         * DOCUMENT ME!
         *
         * @param  sqRadius  DOCUMENT ME!
         */
        public void setSqRadius(double sqRadius) {
            this.sqRadius = sqRadius;
        }

        /**
         * DOCUMENT ME!
         *
         * @param  status  DOCUMENT ME!
         */
        public void setStatus(short status) {
            this.status = status;
        }

        /**
         * DOCUMENT ME!
         *
         * @param  visit  DOCUMENT ME!
         */
        public void setVisit(int visit) {
            this.visit = visit;
        }

        /**
         * DOCUMENT ME!
         *
         * @param  vv  DOCUMENT ME!
         */
        public void setVV(double[] vv) {
            this.vv = vv;
        }

        /**
         * DOCUMENT ME!
         *
         * @param  index  DOCUMENT ME!
         * @param  value  DOCUMENT ME!
         */
        public void setVV(int index, double value) {
            this.vv[index] = value;
        }

        /**
         * DOCUMENT ME!
         *
         * @param  vv  DOCUMENT ME!
         */
        public void setVVCopy(double[] vv) {
            this.vv = new double[vv.length];

            for (int i = 0; i < vv.length; i++) {
                this.vv[i] = vv[i];
            }
        }

    } // private class simplex

    /**
     * DOCUMENT ME!
     */
    private class SimplexList {

        /** DOCUMENT ME! */
        private simplex firstNode;

        /** DOCUMENT ME! */
        private simplex lastNode;

        /**
         * Creates a new SimplexList object.
         */
        public SimplexList() {
            firstNode = lastNode = null;
        }

        /**
         * DOCUMENT ME!
         *
         * @param  insertNode  DOCUMENT ME!
         */
        public void insertAtBack(simplex insertNode) {

            if (isEmpty()) {
                firstNode = lastNode = insertNode;
            } else {
                lastNode = lastNode.nextNode = insertNode;
            }
        }

        /**
         * DOCUMENT ME!
         *
         * @return  DOCUMENT ME!
         */
        public boolean isEmpty() {
            return firstNode == null;
        }

        /*public void setChRoot(simplex node) {
         *  simplex current = firstNode; while (current != null) {     if (current != node) { current.setChRoot(false);
         *    }     else {         current.setChRoot(true);     }     current = current.nextNode; } }
         *
         * public simplex getChRoot() { simplex current = firstNode; while (current != null) {     if
         * (current.getChRoot()) {         return current;     }     current = current.nextNode; } return null;}*/
    } // private class SimplexList

}
